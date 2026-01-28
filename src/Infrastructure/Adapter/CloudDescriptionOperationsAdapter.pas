unit CloudDescriptionOperationsAdapter;

{Adapter that wraps cloud operations to implement ICloudDescriptionOps.
	Enables DescriptionSyncManager to use cloud operations via interface,
	abstracting dependencies to enable unit testing.}

interface

uses
	CloudMailRu,
	WindowsFileSystem,
	WFXTypes,
	System.SysUtils;

type
	{Interface for basic cloud file operations.
		Narrow interface used by CloudDescriptionOperationsAdapter.}
	ICloudFileOperations = interface
		['{B9C95D48-FCF9-4375-8D15-0412756191C0}']

		{Download file from cloud.
			@param RemotePath Path on cloud
			@param LocalPath Local destination path
			@param ResultHash Output hash of downloaded file
			@param LogErrors Whether to log errors
			@return FS_FILE_OK on success, error code otherwise}
		function GetFile(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = True): Integer;

		{Upload file to cloud.
			@param LocalPath Local source path
			@param RemotePath Path on cloud
			@return FS_FILE_OK on success, error code otherwise}
		function PutFile(LocalPath, RemotePath: WideString): Integer;

		{Delete file from cloud.
			@param Path Path on cloud
			@return True on success}
		function DeleteFile(Path: WideString): Boolean;
	end;

	{Wraps TCloudMailRu to implement ICloudFileOperations interface}
	TCloudFileOperationsAdapter = class(TInterfacedObject, ICloudFileOperations)
	private
		FCloud: TCloudMailRu;
	public
		constructor Create(Cloud: TCloudMailRu);

		function GetFile(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = True): Integer;
		function PutFile(LocalPath, RemotePath: WideString): Integer;
		function DeleteFile(Path: WideString): Boolean;
	end;

	{Interface for cloud operations needed by description synchronization.
		Abstracts TCloudMailRu dependency to enable unit testing.}
	ICloudDescriptionOps = interface
		['{B1E745B6-3964-4FFF-ABA6-0DE5A3B35332}']

		{Download remote description file to local path.
			@param RemotePath Full path to description file on cloud
			@param LocalCopy Local path to save downloaded content
			@return True if file downloaded, False if not exists or error}
		function GetDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;

		{Upload local description file to remote path.
			If local file doesn't exist, deletes remote file.
			@param RemotePath Full path for description file on cloud
			@param LocalCopy Local path to upload from
			@return True on success}
		function PutDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;

		{Delete file from cloud.
			@param Path Full path to file on cloud
			@return True on success}
		function DeleteFile(const Path: WideString): Boolean;
	end;

	{Wraps cloud operations to ICloudDescriptionOps interface}
	TCloudDescriptionOperationsAdapter = class(TInterfacedObject, ICloudDescriptionOps)
	private
		FCloudOps: ICloudFileOperations;
		FFileSystem: IFileSystem;
	public
		{Create adapter with injected dependencies}
		constructor Create(CloudOps: ICloudFileOperations; FileSystem: IFileSystem); overload;

		{Convenience constructor wrapping TCloudMailRu directly}
		constructor Create(Cloud: TCloudMailRu); overload;

		function GetDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
		function PutDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
		function DeleteFile(const Path: WideString): Boolean;
	end;

implementation

{TCloudMailRuFileOpsAdapter}

constructor TCloudFileOperationsAdapter.Create(Cloud: TCloudMailRu);
begin
	inherited Create;
	FCloud := Cloud;
end;

function TCloudFileOperationsAdapter.GetFile(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
begin
	Result := FCloud.Downloader.Download(RemotePath, LocalPath, ResultHash, LogErrors);
end;

function TCloudFileOperationsAdapter.PutFile(LocalPath, RemotePath: WideString): Integer;
begin
	Result := FCloud.Uploader.Upload(LocalPath, RemotePath);
end;

function TCloudFileOperationsAdapter.DeleteFile(Path: WideString): Boolean;
begin
	Result := FCloud.FileOps.Delete(Path);
end;

{TCloudDescriptionOperationsAdapter}

constructor TCloudDescriptionOperationsAdapter.Create(CloudOps: ICloudFileOperations; FileSystem: IFileSystem);
begin
	inherited Create;
	FCloudOps := CloudOps;
	FFileSystem := FileSystem;
end;

constructor TCloudDescriptionOperationsAdapter.Create(Cloud: TCloudMailRu);
begin
	Create(TCloudFileOperationsAdapter.Create(Cloud), TWindowsFileSystem.Create);
end;

function TCloudDescriptionOperationsAdapter.GetDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
var
	ResultHash: WideString;
begin
	{Download description file without logging errors (file may not exist)}
	Result := FCloudOps.GetFile(RemotePath, LocalCopy, ResultHash, False) = FS_FILE_OK;
end;

function TCloudDescriptionOperationsAdapter.PutDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
begin
	{Upload description file or delete remote if local doesn't exist}
	if FFileSystem.FileExists(LocalCopy) then
		Result := FCloudOps.PutFile(LocalCopy, RemotePath) = FS_FILE_OK
	else
		Result := FCloudOps.DeleteFile(RemotePath);
end;

function TCloudDescriptionOperationsAdapter.DeleteFile(const Path: WideString): Boolean;
begin
	Result := FCloudOps.DeleteFile(Path);
end;

end.
