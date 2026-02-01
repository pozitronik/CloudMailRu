unit CloudFileOperationsAdapter;

{Adapter that wraps TCloudMailRu to implement ICloudFileOperations.
	Narrow interface used by CloudDescriptionOperationsAdapter for basic
	cloud file operations (download, upload, delete).}

interface

uses
	CloudMailRu,
	WFXTypes;

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

implementation

{TCloudFileOperationsAdapter}

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
	Result := FCloud.FileOperations.Delete(Path);
end;

end.
