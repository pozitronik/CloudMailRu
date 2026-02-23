unit CloudDescriptionOperationsAdapter;

{Adapter that wraps cloud operations to implement ICloudDescriptionOps.
	Enables DescriptionSyncManager to use cloud operations via interface,
	abstracting dependencies to enable unit testing.}

interface

uses
	CloudFileOperationsAdapter,
	CloudMailRu,
	FileSystem,
	WFXTypes,
	System.SysUtils;

type
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
		FCloudOps: ICloudFileOperationsAdapter;
		FFileSystem: IFileSystem;
	public
		{Create adapter with injected dependencies}
		constructor Create(CloudOps: ICloudFileOperationsAdapter; FileSystem: IFileSystem); overload;

		{Convenience constructor wrapping TCloudMailRu directly}
		constructor Create(Cloud: TCloudMailRu); overload;

		function GetDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
		function PutDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
		function DeleteFile(const Path: WideString): Boolean;
	end;

implementation

{TCloudDescriptionOperationsAdapter}

constructor TCloudDescriptionOperationsAdapter.Create(CloudOps: ICloudFileOperationsAdapter; FileSystem: IFileSystem);
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
