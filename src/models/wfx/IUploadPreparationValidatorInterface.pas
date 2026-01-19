unit IUploadPreparationValidatorInterface;

{Interface for pre-upload validation in FsPutFile.

 Encapsulates validation logic that determines whether upload should proceed:
 - Local file existence check
 - Path validation (empty account, virtual paths)
 - Resume flag handling (not supported)
 - Conflict detection (file exists without overwrite flag)

 Note: Overwrite handling (deleting existing file) is not part of this
 validator because it requires ConnectionManager access. The validator
 indicates when overwrite is needed via RequiresOverwrite flag.}

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

implementation

end.
