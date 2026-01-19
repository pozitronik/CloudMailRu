unit IRootListingHandlerInterface;

{Interface for root directory listing in FsFindFirst.
 Handles the case when path is '\' - lists all configured accounts.}

interface

uses
	Windows,
	PLUGIN_TYPES,
	WSList;

type
	{Result of root listing operation}
	TRootListingResult = record
		Handle: THandle;
		FindData: tWIN32FINDDATAW;
		Accounts: TWSList;       {List of accounts to iterate in FsFindNext}
		FileCounter: Integer;    {Counter for iteration, 0 if no accounts}
		ErrorCode: DWORD;        {Error code to set, 0 if none}
	end;

	IRootListingHandler = interface
		['{224EE416-B03E-41F8-8EB4-8EB556417FCA}']

		{Executes root listing - returns list of all configured accounts.
		 @return Result with accounts list and first FindData entry}
		function Execute: TRootListingResult;

		{Executes root listing with provided accounts list.
		 @param Accounts List of account names to display
		 @return Result with accounts list and first FindData entry}
		function ExecuteWithAccounts(Accounts: TWSList): TRootListingResult;
	end;

implementation

end.
