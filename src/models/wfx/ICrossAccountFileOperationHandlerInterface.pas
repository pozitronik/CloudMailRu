unit ICrossAccountFileOperationHandlerInterface;

{Interface for handling cross-account file operations (copy/move between different accounts).
 Supports two transfer modes: via hash (file identity) or via public link (weblink cloning).}

interface

uses
	CloudMailRu,
	RealPath;

type
	{Callback for checking if operation was aborted by user}
	TAbortCheckFunc = reference to function: Boolean;

	ICrossAccountFileOperationHandler = interface
		['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

		{Executes cross-account file operation (copy/move between different accounts).
		 @param OldCloud Source cloud connection
		 @param NewCloud Target cloud connection
		 @param OldRealPath Source path
		 @param NewRealPath Target path
		 @param Move True to move (delete source after copy), False to copy only
		 @param OverWrite True to overwrite existing target
		 @param Mode Copy mode (CopyBetweenAccountsModeDisabled/ViaHash/ViaPublicLink)
		 @param IsSourcePublicAccount True if source is a public account
		 @param AbortCheck Callback for checking user abort
		 @return FS_FILE_OK on success, error code otherwise}
		function Execute(OldCloud, NewCloud: TCloudMailRu;
			const OldRealPath, NewRealPath: TRealPath;
			Move, OverWrite: Boolean;
			Mode: Integer;
			IsSourcePublicAccount: Boolean;
			AbortCheck: TAbortCheckFunc): Integer;
	end;

implementation

end.
