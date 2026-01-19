unit UploadPreparationValidator;

{Pre-upload validation for FsPutFile.

 Centralizes validation logic that was previously inline:
 - Local file existence check
 - Path validation (reject empty account or virtual paths)
 - Resume flag check (not supported by cloud API)
 - Conflict detection (file exists without overwrite permission)}

interface

uses
	RealPath;

type
	TUploadValidationResult = record
		ShouldProceed: Boolean;
		ResultCode: Integer;       {Error code if not proceeding (FS_FILE_*)}
		RequiresOverwrite: Boolean; {True if existing file should be deleted first}
	end;

	IUploadPreparationValidator = interface
		['{B2C3D4E5-F6A7-8901-BCDE-F23456789ABC}']

		{Validates whether upload can proceed.
		 @param LocalName The local file path to upload
		 @param RemotePath The parsed remote path
		 @param CopyFlags TC copy operation flags
		 @returns Validation result with proceed decision and error code}
		function Validate(const LocalName: WideString; const RemotePath: TRealPath; CopyFlags: Integer): TUploadValidationResult;
	end;

	{Function type for file existence check - allows injection for testing}
	TFileExistsFunc = reference to function(const Path: WideString): Boolean;

	TUploadPreparationValidator = class(TInterfacedObject, IUploadPreparationValidator)
	private
		FFileExists: TFileExistsFunc;
	public
		constructor Create(FileExistsFunc: TFileExistsFunc);

		function Validate(const LocalName: WideString; const RemotePath: TRealPath;
			CopyFlags: Integer): TUploadValidationResult;
	end;

implementation

uses
	PLUGIN_TYPES,
	SystemHelper,
	PathHelper;

constructor TUploadPreparationValidator.Create(FileExistsFunc: TFileExistsFunc);
begin
	inherited Create;
	FFileExists := FileExistsFunc;
end;

function TUploadPreparationValidator.Validate(const LocalName: WideString;
	const RemotePath: TRealPath; CopyFlags: Integer): TUploadValidationResult;
begin
	Result.ShouldProceed := False;
	Result.ResultCode := FS_FILE_OK;
	Result.RequiresOverwrite := False;

	{Check if local file exists}
	if not FFileExists(GetUNCFilePath(LocalName)) then
	begin
		Result.ResultCode := FS_FILE_NOTFOUND;
		Exit;
	end;

	{Check for invalid paths}
	if RemotePath.isAccountEmpty or RemotePath.isVirtual then
	begin
		Result.ResultCode := FS_FILE_NOTSUPPORTED;
		Exit;
	end;

	{Resume is not supported by cloud API}
	if CheckFlag(FS_COPYFLAGS_RESUME, CopyFlags) then
	begin
		Result.ResultCode := FS_FILE_NOTSUPPORTED;
		Exit;
	end;

	{Облако не поддерживает разные регистры - cloud doesn't support case-different names}
	if (CheckFlag(FS_COPYFLAGS_EXISTS_SAMECASE, CopyFlags) or
		CheckFlag(FS_COPYFLAGS_EXISTS_DIFFERENTCASE, CopyFlags)) and
		not CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags) then
	begin
		Result.ResultCode := FS_FILE_EXISTS;
		Exit;
	end;

	{Check if overwrite is required}
	Result.RequiresOverwrite := CheckFlag(FS_COPYFLAGS_OVERWRITE, CopyFlags);

	Result.ShouldProceed := True;
end;

end.
