unit DownloadPreparationValidator;

{Pre-download validation for FsGetFile.

	Centralizes initial validation logic:
	- Resume flag check (not supported by cloud API)
	- Virtual path rejection (can't download from trash/shared/invites)

	Note: Local file conflict resolution is handled separately by
	ILocalFileConflictResolver which is already extracted.}

interface

uses
	RealPath;

type
	TDownloadValidationResult = record
		ShouldProceed: Boolean;
		ResultCode: Integer; {Error code if not proceeding (FS_FILE_*)}
	end;

	IDownloadPreparationValidator = interface
		['{DC01E5E1-D5D5-45F2-8870-15016011CF71}']

		{Validates whether download can proceed.
			@param RemotePath The parsed remote path
			@param CopyFlags TC copy operation flags
			@returns Validation result with proceed decision and error code}
		function Validate(const RemotePath: TRealPath; CopyFlags: Integer): TDownloadValidationResult;
	end;

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
