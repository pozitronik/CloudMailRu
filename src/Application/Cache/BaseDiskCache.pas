unit BaseDiskCache;

{Base class for disk-based TTL caches with lazy LRU size eviction.
	Encapsulates shared infrastructure: directory management, meta file I/O,
	TTL expiry checking, size-based eviction, and bulk invalidation.
	Subclasses provide data file extension and domain-specific serialization.}

interface

uses
	SysUtils, SyncObjs;

type
	{Internal record for eviction sorting}
	TMetaEntry = record
		Hash: WideString;
		ExpiresAt: TDateTime;
		DataSize: Int64;
	end;

	TBaseDiskCache = class(TInterfacedObject)
	protected
		FLock: TCriticalSection;
		FCacheDir: WideString;
		FTTLSeconds: Integer;
		FMaxSizeBytes: Int64;

		{Subclass provides the data file extension (e.g. '.json', '.dat')}
		function GetDataFileExtension: WideString; virtual; abstract;

		function DataFilePath(const Hash: WideString): WideString;
		function MetaFilePath(const Hash: WideString): WideString;
		procedure WriteBaseMeta(const Hash: WideString; DataSize: Int64);
		function ReadBaseMeta(const Hash: WideString; out ExpiresAt: TDateTime; out DataSize: Int64): Boolean;
		procedure DeleteEntry(const Hash: WideString);
		function GetTotalCacheSize: Int64;
		procedure EvictIfOverSize;
		procedure DoInvalidateAll;
	public
		constructor Create(const CacheDir: WideString; TTLSeconds: Integer; MaxSizeMB: Integer);
		destructor Destroy; override;
	end;

implementation

uses
	Classes, IOUtils, DateUtils, Generics.Collections, Generics.Defaults;

constructor TBaseDiskCache.Create(const CacheDir: WideString; TTLSeconds: Integer; MaxSizeMB: Integer);
begin
	inherited Create;
	FLock := TCriticalSection.Create;
	FCacheDir := IncludeTrailingPathDelimiter(CacheDir);
	FTTLSeconds := TTLSeconds;
	FMaxSizeBytes := Int64(MaxSizeMB) * 1024 * 1024;
	ForceDirectories(FCacheDir);
end;

destructor TBaseDiskCache.Destroy;
begin
	FreeAndNil(FLock);
	inherited;
end;

function TBaseDiskCache.DataFilePath(const Hash: WideString): WideString;
begin
	Result := FCacheDir + Hash + GetDataFileExtension;
end;

function TBaseDiskCache.MetaFilePath(const Hash: WideString): WideString;
begin
	Result := FCacheDir + Hash + '.meta';
end;

procedure TBaseDiskCache.WriteBaseMeta(const Hash: WideString; DataSize: Int64);
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

function TBaseDiskCache.ReadBaseMeta(const Hash: WideString; out ExpiresAt: TDateTime; out DataSize: Int64): Boolean;
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

procedure TBaseDiskCache.DeleteEntry(const Hash: WideString);
begin
	SysUtils.DeleteFile(DataFilePath(Hash));
	SysUtils.DeleteFile(MetaFilePath(Hash));
end;

function TBaseDiskCache.GetTotalCacheSize: Int64;
var
	SR: TSearchRec;
begin
	Result := 0;
	if FindFirst(FCacheDir + '*' + GetDataFileExtension, faAnyFile, SR) = 0 then
	begin
		repeat
			Result := Result + SR.Size;
		until FindNext(SR) <> 0;
		SysUtils.FindClose(SR);
	end;
end;

procedure TBaseDiskCache.EvictIfOverSize;
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
	{Caller must hold FLock}
	TotalSize := GetTotalCacheSize;
	if TotalSize <= FMaxSizeBytes then
		Exit;

	Entries := TList<TMetaEntry>.Create;
	try
		{Collect all meta entries}
		if FindFirst(FCacheDir + '*.meta', faAnyFile, SR) = 0 then
		begin
			repeat
				Hash := ChangeFileExt(SR.Name, '');
				if ReadBaseMeta(Hash, ExpiresAt, DataSize) then
				begin
					Entry.Hash := Hash;
					Entry.ExpiresAt := ExpiresAt;
					Entry.DataSize := DataSize;
					Entries.Add(Entry);
				end;
			until FindNext(SR) <> 0;
			SysUtils.FindClose(SR);
		end;

		{Sort by ExpiresAt ascending (oldest/soonest-to-expire first)}
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

		{Delete oldest entries until under limit}
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

procedure TBaseDiskCache.DoInvalidateAll;
var
	SR: TSearchRec;
	Ext: WideString;
begin
	Ext := GetDataFileExtension;
	if FindFirst(FCacheDir + '*' + Ext, faAnyFile, SR) = 0 then
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
end;

end.
