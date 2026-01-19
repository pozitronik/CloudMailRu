unit ILocalFileConflictResolverInterface;

{Interface for resolving local file conflicts during download.
 Determines action when target file already exists based on settings.}

interface

uses
	PLUGIN_TYPES;

type
	{Result of conflict resolution}
	TConflictResolution = record
		ShouldProceed: Boolean;  {True to continue with download}
		ResultCode: Integer;     {FS_FILE_* code to return if not proceeding}
	end;

	ILocalFileConflictResolver = interface
		['{C7F4E2A9-8D1B-4C5E-B3A6-2F9E1D8C7B5A}']

		{Resolves conflict when local file exists.
		 @param LocalPath Path to local file
		 @param CopyFlags TC copy flags (check for FS_COPYFLAGS_OVERWRITE)
		 @param OverwriteMode Setting: Ask/Ignore/Overwrite
		 @return Resolution with ShouldProceed and ResultCode}
		function Resolve(const LocalPath: WideString; CopyFlags: Integer;
			OverwriteMode: Integer): TConflictResolution;
	end;

implementation

end.
