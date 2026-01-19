unit IPathListingHandlerInterface;

{Interface for path directory listing in FsFindFirst.
 Handles the case when path is not '\' - lists cloud directory contents.}

interface

uses
	Windows,
	PLUGIN_TYPES,
	RealPath,
	CMRDirItemList,
	CMRIncomingInviteList;

type
	{Result of path listing operation}
	TPathListingResult = record
		Handle: THandle;
		FindData: tWIN32FINDDATAW;
		Listing: TCMRDirItemList;                    {Directory items for FsFindNext}
		IncomingInvites: TCMRIncomingInviteList;     {Incoming invites if invites dir}
		FileCounter: Integer;                        {Counter for iteration}
		ErrorCode: DWORD;                            {Error code to set, 0 if none}
		RealPath: TRealPath;                         {Parsed path for sharedDir check}
	end;

	IPathListingHandler = interface
		['{B2C3D4E5-F6A7-8901-BCDE-F23456789012}']

		{Executes path listing - returns cloud directory contents.
		 @param Path Full path to list (e.g. '\account\folder')
		 @return Result with listing data and first FindData entry}
		function Execute(const Path: WideString): TPathListingResult;
	end;

implementation

end.
