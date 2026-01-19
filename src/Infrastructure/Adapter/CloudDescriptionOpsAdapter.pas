unit CloudDescriptionOpsAdapter;

{Adapter that wraps TCloudMailRu to implement ICloudDescriptionOps.
 Enables DescriptionSyncManager to use cloud operations via interface,
 abstracting TCloudMailRu dependency to enable unit testing.}

interface

uses
	CloudMailRu;

type
	{Interface for cloud operations needed by description synchronization.
	 Abstracts TCloudMailRu dependency to enable unit testing.}
	ICloudDescriptionOps = interface
		['{DBE14B0E-4CEA-44E1-9D3E-21F2CDF65068}']

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

	{Wraps TCloudMailRu methods to ICloudDescriptionOps interface}
	TCloudDescriptionOpsAdapter = class(TInterfacedObject, ICloudDescriptionOps)
	private
		FCloud: TCloudMailRu;
	public
		constructor Create(Cloud: TCloudMailRu);

		function GetDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
		function PutDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
		function DeleteFile(const Path: WideString): Boolean;
	end;

implementation

{TCloudDescriptionOpsAdapter}

constructor TCloudDescriptionOpsAdapter.Create(Cloud: TCloudMailRu);
begin
	inherited Create;
	FCloud := Cloud;
end;

function TCloudDescriptionOpsAdapter.GetDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
begin
	Result := FCloud.GetDescriptionFile(RemotePath, LocalCopy);
end;

function TCloudDescriptionOpsAdapter.PutDescriptionFile(const RemotePath, LocalCopy: WideString): Boolean;
begin
	Result := FCloud.PutDescriptionFile(RemotePath, LocalCopy);
end;

function TCloudDescriptionOpsAdapter.DeleteFile(const Path: WideString): Boolean;
begin
	Result := FCloud.DeleteFile(Path);
end;

end.
