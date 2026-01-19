unit ISameAccountMoveHandlerInterface;

{Interface for handling move/copy operations within a single account.
 Encapsulates overwrite handling, skip-path management, and description sync.}

interface

uses
	RealPath,
	CloudMailRu;

type
	ISameAccountMoveHandler = interface
		['{A9D4E7C2-3F8B-4A6E-9C1D-5E2F8B7A3C4D}']

		{Executes move or copy operation within the same account.
		 @param Cloud The cloud connection for the account
		 @param OldPath Source path
		 @param NewPath Destination path
		 @param Move True for move, False for copy
		 @param OverWrite True to overwrite existing files
		 @return FS_FILE_* result code}
		function Execute(Cloud: TCloudMailRu; const OldPath, NewPath: TRealPath;
			Move, OverWrite: Boolean): Integer;
	end;

implementation

end.
