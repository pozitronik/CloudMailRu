unit OverwritePreparationHandler;

{Handles file overwrite preparation by deleting existing remote file.
	Cloud API doesn't support overwrite directly, so we delete first.}

interface

uses
	ConnectionManager,
	CloudMailRu,
	RealPath,
	WFXTypes;

type
	TOverwritePreparationResult = record
		Success: Boolean;
		ResultCode: Integer;
	end;

	IOverwritePreparationHandler = interface
		['{4F6DB3F8-9A3C-4AA2-8C03-ABF2189FF5B0}']

		{Prepares for overwrite by deleting existing file if required.
			@param Path Remote path to prepare
			@param RequiresOverwrite Whether overwrite was requested
			@return Result with Success=True if ready to proceed, or ResultCode on failure}
		function Prepare(const Path: TRealPath; RequiresOverwrite: Boolean): TOverwritePreparationResult;
	end;

	TOverwritePreparationHandler = class(TInterfacedObject, IOverwritePreparationHandler)
	private
		FConnectionManager: IConnectionManager;
	public
		constructor Create(ConnectionManager: IConnectionManager);
		function Prepare(const Path: TRealPath; RequiresOverwrite: Boolean): TOverwritePreparationResult;
	end;

implementation

constructor TOverwritePreparationHandler.Create(ConnectionManager: IConnectionManager);
begin
	inherited Create;
	FConnectionManager := ConnectionManager;
end;

function TOverwritePreparationHandler.Prepare(const Path: TRealPath; RequiresOverwrite: Boolean): TOverwritePreparationResult;
var
	getResult: Integer;
	Cloud: TCloudMailRu;
begin
	Result.Success := True;
	Result.ResultCode := FS_FILE_OK;

	if not RequiresOverwrite then
		Exit;

	{Cloud API doesn't support overwrite, delete existing file first}
	Cloud := FConnectionManager.Get(Path.account, getResult);
	if Cloud = nil then
	begin
		Result.Success := False;
		Result.ResultCode := FS_FILE_NOTSUPPORTED;
		Exit;
	end;

	if not Cloud.FileOps.Delete(Path.Path) then
	begin
		Result.Success := False;
		Result.ResultCode := FS_FILE_NOTSUPPORTED;
	end;
end;

end.
