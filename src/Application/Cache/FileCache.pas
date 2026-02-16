unit FileCache;

{Disk-based TTL file content cache with lazy size eviction.
	Avoids re-downloading unchanged files by caching content keyed by hash.
	Thread-safe via TCriticalSection. Each entry is a pair of sidecar files:
	<hash>.dat (file content) + <hash>.meta (expiry, size).
	Hash-based keys are content-addressable: changed files get new hashes,
	orphaned entries are eventually evicted by size limit.}

interface

uses
	SysUtils, SyncObjs;

type
	IFileCache = interface
		['{B8C4E9F2-6D53-4CA0-9F28-3E7A1B4C6D9F}']
		{Copies cached file to LocalPath. Returns True on cache hit (not expired).}
		function TryGet(const Hash: WideString; const LocalPath: WideString): Boolean;
		{Copies SourcePath into cache keyed by Hash. Evicts oldest entries if over size limit.}
		procedure Put(const Hash: WideString; const SourcePath: WideString);
		{Removes all cached files.}
		procedure InvalidateAll;
	end;

	TDiskFileCache = class(TInterfacedObject, IFileCache)
	private
		FLock: TCriticalSection;
		FCacheDir: WideString;
		FTTLSeconds: Integer;
		FMaxSizeBytes: Int64;

		function CachedFilePath(const Hash: WideString): WideString;
		function MetaFilePath(const Hash: WideString): WideString;
		procedure WriteMeta(const Hash: WideString; DataSize: Int64);
		function ReadMeta(const Hash: WideString; out ExpiresAt: TDateTime; out DataSize: Int64): Boolean;
		procedure EvictIfOverSize;
		procedure DeleteEntry(const Hash: WideString);
		function GetTotalCacheSize: Int64;
	public
		constructor Create(const CacheDir: WideString; TTLSeconds: Integer; MaxSizeMB: Integer);
		destructor Destroy; override;

		function TryGet(const Hash: WideString; const LocalPath: WideString): Boolean;
		procedure Put(const Hash: WideString; const SourcePath: WideString);
		procedure InvalidateAll;
	end;

	{No-op implementation used when FileCacheEnabled=False}
	TNullFileCache = class(TInterfacedObject, IFileCache)
	public
		function TryGet(const Hash: WideString; const LocalPath: WideString): Boolean;
		procedure Put(const Hash: WideString; const SourcePath: WideString);
		procedure InvalidateAll;
	end;

implementation

uses
	Classes, IOUtils, DateUtils, Generics.Collections, Generics.Defaults,
	Windows;

type
	{Internal record for eviction sorting}
	TMetaEntry = record
		Hash: WideString;
		ExpiresAt: TDateTime;
		DataSize: Int64;
	end;

{TDiskFileCache}

constructor TDiskFileCache.Create(const CacheDir: WideString; TTLSeconds: Integer; MaxSizeMB: Integer);
begin
	inherited Create;
	FLock := TCriticalSection.Create;
	FCacheDir := IncludeTrailingPathDelimiter(CacheDir);
	FTTLSeconds := TTLSeconds;
	FMaxSizeBytes := Int64(MaxSizeMB) * 1024 * 1024;
	ForceDirectories(FCacheDir);
end;

destructor TDiskFileCache.Destroy;
begin
	FreeAndNil(FLock);
	inherited;
end;

function TDiskFileCache.CachedFilePath(const Hash: WideString): WideString;
begin
	Result := FCacheDir + Hash + '.dat';
end;

function TDiskFileCache.MetaFilePath(const Hash: WideString): WideString;
begin
	Result := FCacheDir + Hash + '.meta';
end;

procedure TDiskFileCache.WriteMeta(const Hash: WideString; DataSize: Int64);
var
	Lines: TStringList;
begin
	Lines := TStringList.Create;
	try
		Lines.Add('ExpiresAt=' + FloatToStr(Now + FTTLSeconds / SecsPerDay));
		Lines.Add('DataSize=' + IntToStr(DataSize));
		Lines.SaveToFile(MetaFilePath(Hash), TEncoding.UTF8);
	finally
		Lines.Free;
	end;
end;

function TDiskFileCache.ReadMeta(const Hash: WideString; out ExpiresAt: TDateTime; out DataSize: Int64): Boolean;
var
	Lines: TStringList;
	MetaPath: WideString;
	Line: string;
begin
	Result := False;
	ExpiresAt := 0;
	DataSize := 0;
	MetaPath := MetaFilePath(Hash);
	if not FileExists(MetaPath) then
		Exit;

	Lines := TStringList.Create;
	try
		Lines.LoadFromFile(MetaPath, TEncoding.UTF8);
		for Line in Lines do
		begin
			if Line.StartsWith('ExpiresAt=') then
				ExpiresAt := StrToFloatDef(Copy(Line, 11, Length(Line)), 0)
			else if Line.StartsWith('DataSize=') then
				DataSize := StrToInt64Def(Copy(Line, 10, Length(Line)), 0);
		end;
		Result := ExpiresAt > 0;
	finally
		Lines.Free;
	end;
end;

procedure TDiskFileCache.DeleteEntry(const Hash: WideString);
begin
	SysUtils.DeleteFile(CachedFilePath(Hash));
	SysUtils.DeleteFile(MetaFilePath(Hash));
end;

function TDiskFileCache.GetTotalCacheSize: Int64;
var
	SR: TSearchRec;
begin
	Result := 0;
	if FindFirst(FCacheDir + '*.dat', faAnyFile, SR) = 0 then
	begin
		repeat
			Result := Result + SR.Size;
		until FindNext(SR) <> 0;
		SysUtils.FindClose(SR);
	end;
end;

procedure TDiskFileCache.EvictIfOverSize;
var
	SR: TSearchRec;
	Entries: TList<TMetaEntry>;
	Entry: TMetaEntry;
	TotalSize: Int64;
	Hash: WideString;
	ExpiresAt: TDateTime;
	DataSize: Int64;
	I: Integer;
begin
	// Caller must hold FLock
	TotalSize := GetTotalCacheSize;
	if TotalSize <= FMaxSizeBytes then
		Exit;

	Entries := TList<TMetaEntry>.Create;
	try
		// Collect all meta entries
		if FindFirst(FCacheDir + '*.meta', faAnyFile, SR) = 0 then
		begin
			repeat
				Hash := ChangeFileExt(SR.Name, '');
				if ReadMeta(Hash, ExpiresAt, DataSize) then
				begin
					Entry.Hash := Hash;
					Entry.ExpiresAt := ExpiresAt;
					Entry.DataSize := DataSize;
					Entries.Add(Entry);
				end;
			until FindNext(SR) <> 0;
			SysUtils.FindClose(SR);
		end;

		// Sort by ExpiresAt ascending (oldest/soonest-to-expire first)
		Entries.Sort(TComparer<TMetaEntry>.Construct(
			function(const Left, Right: TMetaEntry): Integer
			begin
				if Left.ExpiresAt < Right.ExpiresAt then
					Result := -1
				else if Left.ExpiresAt > Right.ExpiresAt then
					Result := 1
				else
					Result := 0;
			end));

		// Delete oldest entries until under limit
		for I := 0 to Entries.Count - 1 do
		begin
			if TotalSize <= FMaxSizeBytes then
				Break;
			Entry := Entries[I];
			TotalSize := TotalSize - Entry.DataSize;
			DeleteEntry(Entry.Hash);
		end;
	finally
		Entries.Free;
	end;
end;

function TDiskFileCache.TryGet(const Hash: WideString; const LocalPath: WideString): Boolean;
var
	ExpiresAt: TDateTime;
	DataSize: Int64;
	SourcePath: WideString;
begin
	Result := False;
	if Hash = '' then
		Exit;

	FLock.Enter;
	try
		if not ReadMeta(Hash, ExpiresAt, DataSize) then
			Exit;

		if ExpiresAt <= Now then
		begin
			DeleteEntry(Hash);
			Exit;
		end;

		SourcePath := CachedFilePath(Hash);
		if not FileExists(SourcePath) then
		begin
			DeleteEntry(Hash);
			Exit;
		end;

		Result := CopyFileW(PWideChar(SourcePath), PWideChar(LocalPath), False);
	finally
		FLock.Leave;
	end;
end;

procedure TDiskFileCache.Put(const Hash: WideString; const SourcePath: WideString);
var
	DestPath: WideString;
	FileSize: Int64;
	SR: TSearchRec;
begin
	if Hash = '' then
		Exit;
	if not FileExists(SourcePath) then
		Exit;

	// Get source file size before locking
	FileSize := 0;
	if FindFirst(SourcePath, faAnyFile, SR) = 0 then
	begin
		FileSize := SR.Size;
		SysUtils.FindClose(SR);
	end;

	DestPath := CachedFilePath(Hash);
	FLock.Enter;
	try
		if not CopyFileW(PWideChar(SourcePath), PWideChar(DestPath), False) then
			Exit;
		WriteMeta(Hash, FileSize);
		EvictIfOverSize;
	finally
		FLock.Leave;
	end;
end;

procedure TDiskFileCache.InvalidateAll;
var
	SR: TSearchRec;
begin
	FLock.Enter;
	try
		if FindFirst(FCacheDir + '*.dat', faAnyFile, SR) = 0 then
		begin
			repeat
				SysUtils.DeleteFile(FCacheDir + SR.Name);
			until FindNext(SR) <> 0;
			SysUtils.FindClose(SR);
		end;
		if FindFirst(FCacheDir + '*.meta', faAnyFile, SR) = 0 then
		begin
			repeat
				SysUtils.DeleteFile(FCacheDir + SR.Name);
			until FindNext(SR) <> 0;
			SysUtils.FindClose(SR);
		end;
	finally
		FLock.Leave;
	end;
end;

{TNullFileCache}

function TNullFileCache.TryGet(const Hash: WideString; const LocalPath: WideString): Boolean;
begin
	Result := False;
end;

procedure TNullFileCache.Put(const Hash: WideString; const SourcePath: WideString);
begin
	// No-op
end;

procedure TNullFileCache.InvalidateAll;
begin
	// No-op
end;

end.
