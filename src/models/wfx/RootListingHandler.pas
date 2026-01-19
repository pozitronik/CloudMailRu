unit RootListingHandler;

{Root directory listing handler.
 Lists all configured accounts when path is '\'.}

interface

uses
	Windows,
	IRootListingHandlerInterface,
	PLUGIN_TYPES,
	WSList,
	CMRConstants,
	WindowsHelper;

type
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
