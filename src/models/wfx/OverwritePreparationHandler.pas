unit OverwritePreparationHandler;

{Handles file overwrite preparation by deleting existing remote file.
 Cloud API doesn't support overwrite directly, so we delete first.}

interface

uses
	IOverwritePreparationHandlerInterface,
	IConnectionManagerInterface,
	RealPath,
	PLUGIN_TYPES;

type
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

function TOverwritePreparationHandler.Prepare(const Path: TRealPath;
	RequiresOverwrite: Boolean): TOverwritePreparationResult;
var
	getResult: Integer;
begin
	Result.Success := True;
	Result.ResultCode := FS_FILE_OK;

	if not RequiresOverwrite then
		Exit;

	{Cloud API doesn't support overwrite, delete existing file first}
	if not FConnectionManager.Get(Path.account, getResult).deleteFile(Path.Path) then
	begin
		Result.Success := False;
		Result.ResultCode := FS_FILE_NOTSUPPORTED;
	end;
end;

end.
