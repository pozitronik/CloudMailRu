unit DownloadPreparationValidator;

{Pre-download validation for FsGetFile.

 Centralizes initial validation logic:
 - Resume flag check (not supported by cloud API)
 - Virtual path rejection (can't download from trash/shared/invites)}

interface

uses
	RealPath,
	IDownloadPreparationValidatorInterface;

type
	TDownloadPreparationValidator = class(TInterfacedObject, IDownloadPreparationValidator)
	public
		function Validate(const RemotePath: TRealPath; CopyFlags: Integer): TDownloadValidationResult;
	end;

implementation

uses
	PLUGIN_TYPES,
	SystemHelper;

function TDownloadPreparationValidator.Validate(const RemotePath: TRealPath; CopyFlags: Integer): TDownloadValidationResult;
begin
	Result.ShouldProceed := False;
	Result.ResultCode := FS_FILE_NOTSUPPORTED;

	{Resume is not supported - NEVER CALLED HERE}
	if CheckFlag(FS_COPYFLAGS_RESUME, CopyFlags) then
		Exit;

	{Can't download from virtual directories}
	if RemotePath.isVirtual then
		Exit;

	Result.ShouldProceed := True;
	Result.ResultCode := FS_FILE_OK;
end;

end.
