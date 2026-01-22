unit RootListingHandlerTest;

{Unit tests for TRootListingHandler.
 Tests root directory listing (path = '\') that returns account list.}

interface

uses
	DUnitX.TestFramework,
	Windows,
	RootListingHandler,
	WSList,
	CMRConstants;

type
	[TestFixture]
	TRootListingHandlerTest = class
	private
		FHandler: TRootListingHandler;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Success tests}
		[Test]
		procedure TestExecuteWithAccounts_WithAccounts_ReturnsRootDirectoryHandle;
		[Test]
		procedure TestExecuteWithAccounts_WithAccounts_SetsFileCounterTo1;
		[Test]
		procedure TestExecuteWithAccounts_WithAccounts_ReturnsFindDataForFirstAccount;
		[Test]
		procedure TestExecuteWithAccounts_WithAccounts_ReturnsAccountsList;

		{Empty accounts tests}
		[Test]
		procedure TestExecuteWithAccounts_WithNoAccounts_ReturnsInvalidHandle;
		[Test]
		procedure TestExecuteWithAccounts_WithNoAccounts_SetsErrorCode;
		[Test]
		procedure TestExecuteWithAccounts_WithNoAccounts_FileCounterIsZero;

		{Execute method tests (placeholder implementation)}
		[Test]
		procedure TestExecute_ReturnsInvalidHandle;
		[Test]
		procedure TestExecute_SetsErrorCodeToNoMoreFiles;
		[Test]
		procedure TestExecute_FileCounterIsZero;
		[Test]
		procedure TestExecute_AccountsIsNil;
	end;

implementation

uses
	SysUtils;

{TRootListingHandlerTest}

procedure TRootListingHandlerTest.Setup;
begin
	FHandler := TRootListingHandler.Create;
end;

procedure TRootListingHandlerTest.TearDown;
begin
	FreeAndNil(FHandler);
end;

{Success tests}

procedure TRootListingHandlerTest.TestExecuteWithAccounts_WithAccounts_ReturnsRootDirectoryHandle;
var
	ListingResult: TRootListingResult;
	TestAccounts: TWSList;
begin
	TestAccounts := [];
	TestAccounts.Add('account1');
	TestAccounts.Add('account2');

	ListingResult := FHandler.ExecuteWithAccounts(TestAccounts);

	Assert.AreEqual(THandle(FIND_ROOT_DIRECTORY), ListingResult.Handle);
end;

procedure TRootListingHandlerTest.TestExecuteWithAccounts_WithAccounts_SetsFileCounterTo1;
var
	ListingResult: TRootListingResult;
	TestAccounts: TWSList;
begin
	TestAccounts := [];
	TestAccounts.Add('account1');

	ListingResult := FHandler.ExecuteWithAccounts(TestAccounts);

	Assert.AreEqual(1, ListingResult.FileCounter);
end;

procedure TRootListingHandlerTest.TestExecuteWithAccounts_WithAccounts_ReturnsFindDataForFirstAccount;
var
	ListingResult: TRootListingResult;
	TestAccounts: TWSList;
	FileName: WideString;
begin
	TestAccounts := [];
	TestAccounts.Add('myaccount');
	TestAccounts.Add('other');

	ListingResult := FHandler.ExecuteWithAccounts(TestAccounts);

	FileName := ListingResult.FindData.cFileName;
	Assert.AreEqual('myaccount', FileName);
end;

procedure TRootListingHandlerTest.TestExecuteWithAccounts_WithAccounts_ReturnsAccountsList;
var
	ListingResult: TRootListingResult;
	TestAccounts: TWSList;
begin
	TestAccounts := [];
	TestAccounts.Add('acc1');
	TestAccounts.Add('acc2');
	TestAccounts.Add('acc3');

	ListingResult := FHandler.ExecuteWithAccounts(TestAccounts);

	Assert.AreEqual(3, ListingResult.Accounts.Count);
end;

{Empty accounts tests}

procedure TRootListingHandlerTest.TestExecuteWithAccounts_WithNoAccounts_ReturnsInvalidHandle;
var
	ListingResult: TRootListingResult;
begin
	ListingResult := FHandler.ExecuteWithAccounts([]);

	Assert.AreEqual(THandle(INVALID_HANDLE_VALUE), ListingResult.Handle);
end;

procedure TRootListingHandlerTest.TestExecuteWithAccounts_WithNoAccounts_SetsErrorCode;
var
	ListingResult: TRootListingResult;
begin
	ListingResult := FHandler.ExecuteWithAccounts([]);

	Assert.AreEqual(DWORD(ERROR_NO_MORE_FILES), ListingResult.ErrorCode);
end;

procedure TRootListingHandlerTest.TestExecuteWithAccounts_WithNoAccounts_FileCounterIsZero;
var
	ListingResult: TRootListingResult;
begin
	ListingResult := FHandler.ExecuteWithAccounts([]);

	Assert.AreEqual(0, ListingResult.FileCounter);
end;

{Execute method tests - placeholder implementation that returns empty result}

procedure TRootListingHandlerTest.TestExecute_ReturnsInvalidHandle;
var
	ListingResult: TRootListingResult;
begin
	{Execute is a placeholder - should return invalid handle}
	ListingResult := FHandler.Execute;

	Assert.AreEqual(THandle(INVALID_HANDLE_VALUE), ListingResult.Handle);
end;

procedure TRootListingHandlerTest.TestExecute_SetsErrorCodeToNoMoreFiles;
var
	ListingResult: TRootListingResult;
begin
	{Execute returns ERROR_NO_MORE_FILES as error code}
	ListingResult := FHandler.Execute;

	Assert.AreEqual(DWORD(ERROR_NO_MORE_FILES), ListingResult.ErrorCode);
end;

procedure TRootListingHandlerTest.TestExecute_FileCounterIsZero;
var
	ListingResult: TRootListingResult;
begin
	{Execute returns zero file counter}
	ListingResult := FHandler.Execute;

	Assert.AreEqual(0, ListingResult.FileCounter);
end;

procedure TRootListingHandlerTest.TestExecute_AccountsIsNil;
var
	ListingResult: TRootListingResult;
begin
	{Execute returns nil accounts list}
	ListingResult := FHandler.Execute;

	Assert.IsNull(ListingResult.Accounts, 'Accounts should be nil for Execute');
end;

initialization
	TDUnitX.RegisterTestFixture(TRootListingHandlerTest);

end.
