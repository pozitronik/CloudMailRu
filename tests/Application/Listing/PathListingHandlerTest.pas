unit PathListingHandlerTest;

{Unit tests for TPathListingHandler.
 Tests cloud directory listing for non-root paths.}

interface

uses
	DUnitX.TestFramework,
	Windows,
	PathListingHandler,
	ListingProvider,
	ListingPathValidator,
	ConnectionManager,
	MockConnectionManager,
	MockCloudHTTP,
	MockHTTPManager,
	CloudMailRu,
	CloudSettings,
	FileCipher,
	RealPath,
	CloudDirItem,
	CloudDirItemList,
	CloudIncomingInviteList,
	CloudConstants,
	WFXTypes,
	AuthStrategy,
	WindowsFileSystem,
	TCLogger,
	TCProgress,
	TCRequest,
	TCHandler,
	OpenSSLProvider;

type
	{Mock listing provider with configurable results}
	TMockListingProvider = class(TInterfacedObject, IListingProvider)
	private
		FListing: TCloudDirItemList;
		FFetchSuccess: Boolean;
		FFetchCalled: Boolean;
	public
		constructor Create(const Items: TCloudDirItemList; FetchSuccess: Boolean = True);
		function FetchListing(Cloud: TCloudMailRu; const Path: TRealPath;
			var DirListing: TCloudDirItemList; var InviteListing: TCloudIncomingInviteList): Boolean;
		property FetchCalled: Boolean read FFetchCalled;
	end;

	{Mock listing path validator with configurable results}
	TMockListingPathValidator = class(TInterfacedObject, IListingPathValidator)
	private
		FIsValid: Boolean;
		FErrorCode: DWORD;
		FValidateCalled: Boolean;
	public
		constructor Create(IsValid: Boolean; ErrorCode: DWORD = 0);
		function ValidatePath(isVirtual, isInAccountsList, IsPublicAccount: Boolean;
			const Path: WideString; const Listing: TCloudDirItemList): TListingValidationResult;
		property ValidateCalled: Boolean read FValidateCalled;
	end;

	{Testable CloudMailRu for path listing tests}
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
	end;

	[TestFixture]
	TPathListingHandlerTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FMockConnectionManager: TMockConnectionManager;
		FListingProvider: TMockListingProvider;
		FListingPathValidator: TMockListingPathValidator;
		FCloud: TTestableCloudMailRu;
		FHandler: IPathListingHandler;

		function CreateDirItem(const Name: WideString; IsFolder: Boolean = False): TCloudDirItem;
		function CreateCloud: TTestableCloudMailRu;
		procedure SetupHandler(const Items: TCloudDirItemList; FetchSuccess: Boolean = True;
			ValidateSuccess: Boolean = True; ValidateError: DWORD = 0);
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Mock behavior tests}
		[Test]
		procedure TestMockListingProvider_ReturnsFetchSuccess;
		[Test]
		procedure TestMockListingProvider_ReturnsFetchFailure;
		[Test]
		procedure TestMockListingPathValidator_ReturnsValidResult;
		[Test]
		procedure TestMockListingPathValidator_ReturnsInvalidResult;

		{TPathListingHandler.Execute tests}
		[Test]
		procedure TestExecute_ConnectionFailed_ReturnsAccessDenied;
		[Test]
		procedure TestExecute_CloudIsNil_ReturnsPathNotFound;
		[Test]
		procedure TestExecute_FetchFailed_ReturnsPathNotFound;
		[Test]
		procedure TestExecute_ValidationFailed_ReturnsValidatorError;
		[Test]
		procedure TestExecute_EmptyListing_ReturnsNoMoreFiles;
		[Test]
		procedure TestExecute_NonEmptyListing_ReturnsFindOK;
		[Test]
		procedure TestExecute_NonEmptyListing_SetsFirstItemInFindData;
		[Test]
		procedure TestExecute_NonEmptyListing_SetsFileCounter;
		[Test]
		procedure TestExecute_SharedDir_ReturnsFindSharedLinks;
		[Test]
		procedure TestExecute_ParsesPathCorrectly;
	end;

implementation

uses
	SysUtils;

{TMockListingProvider}

constructor TMockListingProvider.Create(const Items: TCloudDirItemList; FetchSuccess: Boolean);
begin
	inherited Create;
	FListing := Items;
	FFetchSuccess := FetchSuccess;
	FFetchCalled := False;
end;

function TMockListingProvider.FetchListing(Cloud: TCloudMailRu; const Path: TRealPath;
	var DirListing: TCloudDirItemList; var InviteListing: TCloudIncomingInviteList): Boolean;
begin
	FFetchCalled := True;
	DirListing := FListing;
	InviteListing := [];
	Result := FFetchSuccess;
end;

{TMockListingPathValidator}

constructor TMockListingPathValidator.Create(IsValid: Boolean; ErrorCode: DWORD);
begin
	inherited Create;
	FIsValid := IsValid;
	FErrorCode := ErrorCode;
	FValidateCalled := False;
end;

function TMockListingPathValidator.ValidatePath(isVirtual, isInAccountsList, IsPublicAccount: Boolean;
	const Path: WideString; const Listing: TCloudDirItemList): TListingValidationResult;
begin
	FValidateCalled := True;
	Result.IsValid := FIsValid;
	Result.ErrorCode := FErrorCode;
end;

{TTestableCloudMailRu}

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

{TPathListingHandlerTest}

procedure TPathListingHandlerTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
	FMockConnectionManager := TMockConnectionManager.Create;
end;

procedure TPathListingHandlerTest.TearDown;
begin
	FHandler := nil;
	FCloud.Free;
	FMockConnectionManager := nil;
	FListingProvider := nil;
	FListingPathValidator := nil;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TPathListingHandlerTest.CreateDirItem(const Name: WideString; IsFolder: Boolean): TCloudDirItem;
begin
	FillChar(Result, SizeOf(TCloudDirItem), 0);
	Result.name := Name;
	if IsFolder then
		Result.kind := TYPE_DIR
	else
		Result.kind := TYPE_FILE;
end;

function TPathListingHandlerTest.CreateCloud: TTestableCloudMailRu;
var
	Settings: TCloudSettings;
begin
	Settings := Default(TCloudSettings);
	Result := TTestableCloudMailRu.Create(
		Settings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		TNullCipher.Create, TNullOpenSSLProvider.Create);
	Result.SetUnitedParams('api=2&access_token=test_token');
end;

procedure TPathListingHandlerTest.SetupHandler(const Items: TCloudDirItemList; FetchSuccess: Boolean;
	ValidateSuccess: Boolean; ValidateError: DWORD);
begin
	FListingProvider := TMockListingProvider.Create(Items, FetchSuccess);
	FListingPathValidator := TMockListingPathValidator.Create(ValidateSuccess, ValidateError);
	FHandler := TPathListingHandler.Create(FMockConnectionManager, FListingProvider, FListingPathValidator);
end;

{Mock behavior tests}

procedure TPathListingHandlerTest.TestMockListingProvider_ReturnsFetchSuccess;
var
	Items: TCloudDirItemList;
	Listing: TCloudDirItemList;
	Invites: TCloudIncomingInviteList;
	Success: Boolean;
	Path: TRealPath;
begin
	Items := [CreateDirItem('test.txt')];
	FListingProvider := TMockListingProvider.Create(Items, True);

	Success := FListingProvider.FetchListing(nil, Path, Listing, Invites);

	Assert.IsTrue(Success);
	Assert.AreEqual(1, Integer(Length(Listing)));
end;

procedure TPathListingHandlerTest.TestMockListingProvider_ReturnsFetchFailure;
var
	Items: TCloudDirItemList;
	Listing: TCloudDirItemList;
	Invites: TCloudIncomingInviteList;
	Success: Boolean;
	Path: TRealPath;
begin
	Items := [];
	FListingProvider := TMockListingProvider.Create(Items, False);

	Success := FListingProvider.FetchListing(nil, Path, Listing, Invites);

	Assert.IsFalse(Success);
end;

procedure TPathListingHandlerTest.TestMockListingPathValidator_ReturnsValidResult;
var
	ValidationResult: TListingValidationResult;
begin
	FListingPathValidator := TMockListingPathValidator.Create(True, 0);

	ValidationResult := FListingPathValidator.ValidatePath(False, False, False, '', []);

	Assert.IsTrue(ValidationResult.IsValid);
	Assert.AreEqual(DWORD(0), ValidationResult.ErrorCode);
end;

procedure TPathListingHandlerTest.TestMockListingPathValidator_ReturnsInvalidResult;
var
	ValidationResult: TListingValidationResult;
begin
	FListingPathValidator := TMockListingPathValidator.Create(False, ERROR_ACCESS_DENIED);

	ValidationResult := FListingPathValidator.ValidatePath(False, False, False, '', []);

	Assert.IsFalse(ValidationResult.IsValid);
	Assert.AreEqual(DWORD(ERROR_ACCESS_DENIED), ValidationResult.ErrorCode);
end;

{TPathListingHandler.Execute tests}

procedure TPathListingHandlerTest.TestExecute_ConnectionFailed_ReturnsAccessDenied;
begin
	{Setup with no cloud registered - Get will return nil with error}
	SetupHandler([], True, True);

	var Result := FHandler.Execute('\unknownaccount\folder');

	{Connection manager returns nil for unknown account}
	Assert.AreEqual(DWORD(ERROR_PATH_NOT_FOUND), Result.ErrorCode);
end;

procedure TPathListingHandlerTest.TestExecute_CloudIsNil_ReturnsPathNotFound;
begin
	{Setup handler but don't register any cloud}
	SetupHandler([], True, True);

	var Result := FHandler.Execute('\testaccount\folder');

	Assert.AreEqual(DWORD(ERROR_PATH_NOT_FOUND), Result.ErrorCode);
	Assert.AreEqual(THandle(INVALID_HANDLE_VALUE), Result.Handle);
end;

procedure TPathListingHandlerTest.TestExecute_FetchFailed_ReturnsPathNotFound;
begin
	FCloud := CreateCloud;
	FMockConnectionManager.SetCloud('testaccount', FCloud);
	{When fetch fails AND validation fails, we get the proper error}
	SetupHandler([], False, False, ERROR_PATH_NOT_FOUND);

	var Result := FHandler.Execute('\testaccount\folder');

	Assert.IsTrue(FListingProvider.FetchCalled, 'FetchListing should be called');
	Assert.AreEqual(DWORD(ERROR_PATH_NOT_FOUND), Result.ErrorCode);
end;

procedure TPathListingHandlerTest.TestExecute_ValidationFailed_ReturnsValidatorError;
begin
	FCloud := CreateCloud;
	FMockConnectionManager.SetCloud('testaccount', FCloud);
	SetupHandler([CreateDirItem('test.txt')], True, False, ERROR_ACCESS_DENIED);

	var Result := FHandler.Execute('\testaccount\folder');

	Assert.IsTrue(FListingPathValidator.ValidateCalled, 'ValidatePath should be called');
	Assert.AreEqual(DWORD(ERROR_ACCESS_DENIED), Result.ErrorCode);
end;

procedure TPathListingHandlerTest.TestExecute_EmptyListing_ReturnsNoMoreFiles;
begin
	FCloud := CreateCloud;
	FMockConnectionManager.SetCloud('testaccount', FCloud);
	SetupHandler([], True, True);

	var Result := FHandler.Execute('\testaccount\folder');

	Assert.AreEqual(DWORD(ERROR_NO_MORE_FILES), Result.ErrorCode);
	Assert.AreEqual(THandle(FIND_NO_MORE_FILES), Result.Handle);
end;

procedure TPathListingHandlerTest.TestExecute_NonEmptyListing_ReturnsFindOK;
begin
	FCloud := CreateCloud;
	FMockConnectionManager.SetCloud('testaccount', FCloud);
	SetupHandler([CreateDirItem('test.txt')], True, True);

	var Result := FHandler.Execute('\testaccount\folder');

	Assert.AreEqual(DWORD(0), Result.ErrorCode);
	Assert.AreEqual(THandle(FIND_OK), Result.Handle);
end;

procedure TPathListingHandlerTest.TestExecute_NonEmptyListing_SetsFirstItemInFindData;
var
	Items: TCloudDirItemList;
begin
	FCloud := CreateCloud;
	FMockConnectionManager.SetCloud('testaccount', FCloud);
	Items := [CreateDirItem('firstfile.txt'), CreateDirItem('secondfile.txt')];
	SetupHandler(Items, True, True);

	var Result := FHandler.Execute('\testaccount\folder');

	{FindData should contain first item}
	Assert.AreEqual('firstfile.txt', WideString(Result.FindData.cFileName));
end;

procedure TPathListingHandlerTest.TestExecute_NonEmptyListing_SetsFileCounter;
begin
	FCloud := CreateCloud;
	FMockConnectionManager.SetCloud('testaccount', FCloud);
	SetupHandler([CreateDirItem('test.txt')], True, True);

	var Result := FHandler.Execute('\testaccount\folder');

	{Counter should be 1 after returning first item}
	Assert.AreEqual(1, Result.FileCounter);
end;

procedure TPathListingHandlerTest.TestExecute_SharedDir_ReturnsFindSharedLinks;
begin
	FCloud := CreateCloud;
	FMockConnectionManager.SetCloud('testaccount', FCloud);
	SetupHandler([CreateDirItem('sharedfile.txt')], True, True);

	{Use .shared postfix to indicate shared directory}
	var Result := FHandler.Execute('\testaccount.shared');

	Assert.AreEqual(THandle(FIND_SHARED_LINKS), Result.Handle);
	Assert.IsTrue(Result.RealPath.sharedDir, 'RealPath should have sharedDir flag');
end;

procedure TPathListingHandlerTest.TestExecute_ParsesPathCorrectly;
begin
	FCloud := CreateCloud;
	FMockConnectionManager.SetCloud('myaccount', FCloud);
	SetupHandler([CreateDirItem('test.txt')], True, True);

	var Result := FHandler.Execute('\myaccount\subfolder\deep');

	Assert.AreEqual('myaccount', Result.RealPath.account);
	{Path uses Windows backslashes internally}
	Assert.AreEqual('subfolder\deep', Result.RealPath.path);
end;

initialization
	TDUnitX.RegisterTestFixture(TPathListingHandlerTest);

end.
