unit CloudDescriptionOpsAdapter;

{Adapter that wraps TCloudMailRu to implement ICloudDescriptionOps.
 Enables DescriptionSyncManager to use cloud operations via interface.}

interface

uses
	CloudMailRu,
	ICloudDescriptionOpsInterface;

type
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
