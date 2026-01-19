unit IDownloadPreparationValidatorInterface;

{Interface for pre-download validation in FsGetFile.

 Encapsulates initial validation before download attempt:
 - Resume flag check (not supported)
 - Virtual path rejection (trash, shared, invites)

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
		['{C3D4E5F6-A7B8-9012-CDEF-345678901234}']

		{Validates whether download can proceed.
		 @param RemotePath The parsed remote path
		 @param CopyFlags TC copy operation flags
		 @returns Validation result with proceed decision and error code}
		function Validate(const RemotePath: TRealPath; CopyFlags: Integer): TDownloadValidationResult;
	end;

implementation

end.
