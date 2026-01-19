unit RootListingHandler;

{Interface and implementation for root directory listing in FsFindFirst.
 Handles the case when path is '\' - lists all configured accounts.}

interface

uses
	Windows,
	PLUGIN_TYPES,
	WSList,
	CMRConstants,
	WindowsHelper;

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

	TRootListingHandler = class(TInterfacedObject, IRootListingHandler)
	public
		{Executes root listing with provided accounts list.
		 @param Accounts List of account names to display (ownership transferred to result)
		 @return Result with accounts list and first FindData entry}
		function ExecuteWithAccounts(Accounts: TWSList): TRootListingResult;

		{IRootListingHandler - not used directly, call ExecuteWithAccounts instead}
		function Execute: TRootListingResult;
	end;

implementation

function TRootListingHandler.Execute: TRootListingResult;
begin
	{This method should not be called directly - use ExecuteWithAccounts}
	Result.Handle := INVALID_HANDLE_VALUE;
	Result.ErrorCode := ERROR_NO_MORE_FILES;
	Result.FileCounter := 0;
	Result.Accounts := nil;
end;

function TRootListingHandler.ExecuteWithAccounts(Accounts: TWSList): TRootListingResult;
begin
	Result.ErrorCode := 0;
	Result.FileCounter := 0;
	Result.Accounts := Accounts;

	if Result.Accounts.Count > 0 then
	begin
		Result.FindData := GetFindDataEmptyDir(Result.Accounts[0]);
		Result.FileCounter := 1;
		Result.Handle := FIND_ROOT_DIRECTORY;
	end
	else
	begin
		Result.Handle := INVALID_HANDLE_VALUE;
		Result.ErrorCode := ERROR_NO_MORE_FILES;
	end;
end;

end.
