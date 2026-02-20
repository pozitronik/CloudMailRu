unit DirectoryCache;

{Disk-based TTL directory listing cache with lazy size eviction.
	Reduces API calls for repeated directory navigations. Thread-safe via TCriticalSection.
	Stores raw listings BEFORE FilterHiddenMetadataFiles so settings changes take effect
	without cache invalidation. Each entry is a pair of sidecar files:
	<hash>.json (serialized listing) + <hash>.meta (path, expiry, size).}

interface

uses
	SysUtils,
	BaseDiskCache,
	CloudDirItemList;

type
	IDirectoryCache = interface
		['{A7F3D8E1-5C42-4B9A-8E17-2D6F0A3B5C8E}']
		{Attempts to retrieve a cached listing. Returns True if found and not expired.
			The returned Listing is a copy safe for in-place modification.}
		function TryGet(const Path: WideString; out Listing: TCloudDirItemList): Boolean;
		{Stores a copy of the listing with TTL-based expiration. Evicts oldest entries if over size limit.}
		procedure Put(const Path: WideString; const Listing: TCloudDirItemList);
		{Removes the cached entry for a specific path.}
		procedure InvalidatePath(const Path: WideString);
		{Removes all cached entries.}
		procedure InvalidateAll;
	end;

	TDiskDirectoryCache = class(TBaseDiskCache, IDirectoryCache)
	private
		function NormalizePath(const Path: WideString): WideString;
		function PathToHash(const Path: WideString): WideString;
		procedure WriteMeta(const Hash, Path: WideString; DataSize: Int64);
		function ReadMeta(const Hash: WideString; out Path: WideString; out ExpiresAt: TDateTime; out DataSize: Int64): Boolean;
	protected
		function GetDataFileExtension: WideString; override;
	public
		function TryGet(const Path: WideString; out Listing: TCloudDirItemList): Boolean;
		procedure Put(const Path: WideString; const Listing: TCloudDirItemList);
		procedure InvalidatePath(const Path: WideString);
		procedure InvalidateAll;
	end;

	{No-op implementation used when CacheListings=False}
	TNullDirectoryCache = class(TInterfacedObject, IDirectoryCache)
	public
		function TryGet(const Path: WideString; out Listing: TCloudDirItemList): Boolean;
		procedure Put(const Path: WideString; const Listing: TCloudDirItemList);
		procedure InvalidatePath(const Path: WideString);
		procedure InvalidateAll;
	end;

implementation

uses
	Classes, IOUtils, DateUtils, System.Hash, SyncObjs,
	CloudDirItemListJsonAdapter;

{TDiskDirectoryCache}

function TDiskDirectoryCache.GetDataFileExtension: WideString;
begin
	Result := '.json';
end;

function TDiskDirectoryCache.NormalizePath(const Path: WideString): WideString;
begin
	Result := WideLowerCase(Path);
end;

function TDiskDirectoryCache.PathToHash(const Path: WideString): WideString;
begin
	Result := THashMD5.GetHashString(string(NormalizePath(Path)));
end;

procedure TDiskDirectoryCache.WriteMeta(const Hash, Path: WideString; DataSize: Int64);
var
	Lines: TStringList;
begin
	Lines := TStringList.Create;
	try
		Lines.Add('Path=' + Path);
		Lines.Add('ExpiresAt=' + FloatToStr(Now + FTTLSeconds / SecsPerDay));
		Lines.Add('DataSize=' + IntToStr(DataSize));
		Lines.SaveToFile(MetaFilePath(Hash), TEncoding.UTF8);
	finally
		Lines.Free;
	end;
end;

function TDiskDirectoryCache.ReadMeta(const Hash: WideString; out Path: WideString; out ExpiresAt: TDateTime; out DataSize: Int64): Boolean;
var
	Lines: TStringList;
	MetaPath: WideString;
	Line: string;
begin
	Result := False;
	Path := '';
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
			if Line.StartsWith('Path=') then
				Path := Copy(Line, 6, Length(Line))
			else if Line.StartsWith('ExpiresAt=') then
				ExpiresAt := StrToFloatDef(Copy(Line, 11, Length(Line)), 0)
			else if Line.StartsWith('DataSize=') then
				DataSize := StrToInt64Def(Copy(Line, 10, Length(Line)), 0);
		end;
		Result := Path <> '';
	finally
		Lines.Free;
	end;
end;

function TDiskDirectoryCache.TryGet(const Path: WideString; out Listing: TCloudDirItemList): Boolean;
var
	Hash: WideString;
	EntryPath: WideString;
	ExpiresAt: TDateTime;
	DataSize: Int64;
	JSON: WideString;
	Bytes: TBytes;
begin
	Listing := nil;
	Hash := PathToHash(Path);
	FLock.Enter;
	try
		if not ReadMeta(Hash, EntryPath, ExpiresAt, DataSize) then
			Exit(False);

		if ExpiresAt <= Now then
		begin
			DeleteEntry(Hash);
			Exit(False);
		end;

		if not FileExists(DataFilePath(Hash)) then
		begin
			DeleteEntry(Hash);
			Exit(False);
		end;

		Bytes := TFile.ReadAllBytes(DataFilePath(Hash));
		JSON := TEncoding.UTF8.GetString(Bytes);
		Result := TCloudDirItemListJsonAdapter.Parse(JSON, Listing);
		if not Result then
			DeleteEntry(Hash);
	finally
		FLock.Leave;
	end;
end;

procedure TDiskDirectoryCache.Put(const Path: WideString; const Listing: TCloudDirItemList);
var
	Hash: WideString;
	JSON: WideString;
	Bytes: TBytes;
begin
	Hash := PathToHash(Path);
	// ToJSON produces the body content: {"list":[...]}
	// Wrap in {"body":...} envelope for Parse compatibility
	JSON := '{"body":' + TCloudDirItemListJsonAdapter.ToJSON(Listing) + '}';
	Bytes := TEncoding.UTF8.GetBytes(JSON);
	FLock.Enter;
	try
		TFile.WriteAllBytes(DataFilePath(Hash), Bytes);
		WriteMeta(Hash, NormalizePath(Path), Length(Bytes));
		EvictIfOverSize;
	finally
		FLock.Leave;
	end;
end;

procedure TDiskDirectoryCache.InvalidatePath(const Path: WideString);
begin
	FLock.Enter;
	try
		DeleteEntry(PathToHash(Path));
	finally
		FLock.Leave;
	end;
end;

procedure TDiskDirectoryCache.InvalidateAll;
begin
	FLock.Enter;
	try
		DoInvalidateAll;
	finally
		FLock.Leave;
	end;
end;

{TNullDirectoryCache}

function TNullDirectoryCache.TryGet(const Path: WideString; out Listing: TCloudDirItemList): Boolean;
begin
	Listing := nil;
	Result := False;
end;

procedure TNullDirectoryCache.Put(const Path: WideString; const Listing: TCloudDirItemList);
begin
	{No-op}
end;

procedure TNullDirectoryCache.InvalidatePath(const Path: WideString);
begin
	{No-op}
end;

procedure TNullDirectoryCache.InvalidateAll;
begin
	{No-op}
end;

end.
