unit FileCache;

{Disk-based TTL file content cache with lazy size eviction.
	Avoids re-downloading unchanged files by caching content keyed by hash.
	Thread-safe via TCriticalSection. Each entry is a pair of sidecar files:
	<hash>.dat (file content) + <hash>.meta (expiry, size).
	Hash-based keys are content-addressable: changed files get new hashes,
	orphaned entries are eventually evicted by size limit.}

interface

uses
	SysUtils,
	BaseDiskCache;

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

	TDiskFileCache = class(TBaseDiskCache, IFileCache)
	protected
		function GetDataFileExtension: WideString; override;
	public
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
	Windows, SyncObjs;

{TDiskFileCache}

function TDiskFileCache.GetDataFileExtension: WideString;
begin
	Result := '.dat';
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
		if not ReadBaseMeta(Hash, ExpiresAt, DataSize) then
			Exit;

		if ExpiresAt <= Now then
		begin
			DeleteEntry(Hash);
			Exit;
		end;

		SourcePath := DataFilePath(Hash);
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

	DestPath := DataFilePath(Hash);
	FLock.Enter;
	try
		if not CopyFileW(PWideChar(SourcePath), PWideChar(DestPath), False) then
			Exit;
		WriteBaseMeta(Hash, FileSize);
		EvictIfOverSize;
	finally
		FLock.Leave;
	end;
end;

procedure TDiskFileCache.InvalidateAll;
begin
	FLock.Enter;
	try
		DoInvalidateAll;
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
