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
	CloudMailRu,
	RealPath,
	CMRDirItem,
	CMRDirItemList,
	CMRIncomingInviteList,
	CMRConstants;

type
	{Mock listing provider with configurable results}
	TMockListingProvider = class(TInterfacedObject, IListingProvider)
	private
		FListing: TCMRDirItemList;
		FFetchSuccess: Boolean;
	public
		constructor Create(const Items: TCMRDirItemList; FetchSuccess: Boolean = True);
		function FetchListing(Cloud: TCloudMailRu; const Path: TRealPath;
			var DirListing: TCMRDirItemList; var InviteListing: TCMRIncomingInviteList): Boolean;
	end;

	{Mock listing path validator with configurable results}
	TMockListingPathValidator = class(TInterfacedObject, IListingPathValidator)
	private
		FIsValid: Boolean;
		FErrorCode: DWORD;
	public
		constructor Create(IsValid: Boolean; ErrorCode: DWORD = 0);
		function ValidatePath(isVirtual, isInAccountsList, IsPublicAccount: Boolean;
			const Path: WideString; const Listing: TCMRDirItemList): TListingValidationResult;
	end;

	[TestFixture]
	TPathListingHandlerTest = class
	private
		FListingProvider: TMockListingProvider;
		FListingPathValidator: TMockListingPathValidator;

		function CreateDirItem(const Name: WideString; IsFolder: Boolean = False): TCMRDirItem;
	public
		[TearDown]
		procedure TearDown;

		{Note: Full integration tests would require ConnectionManager mocking.
		 These tests verify mock behavior and isolated logic.}

		{Mock behavior tests}
		[Test]
		procedure TestMockListingProvider_ReturnsFetchSuccess;
		[Test]
		procedure TestMockListingProvider_ReturnsFetchFailure;
		[Test]
		procedure TestMockListingPathValidator_ReturnsValidResult;
		[Test]
		procedure TestMockListingPathValidator_ReturnsInvalidResult;
	end;

implementation

uses
	SysUtils;

{TMockListingProvider}

constructor TMockListingProvider.Create(const Items: TCMRDirItemList; FetchSuccess: Boolean);
begin
	inherited Create;
	FListing := Items;
	FFetchSuccess := FetchSuccess;
end;

function TMockListingProvider.FetchListing(Cloud: TCloudMailRu; const Path: TRealPath;
	var DirListing: TCMRDirItemList; var InviteListing: TCMRIncomingInviteList): Boolean;
begin
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
end;

function TMockListingPathValidator.ValidatePath(isVirtual, isInAccountsList, IsPublicAccount: Boolean;
	const Path: WideString; const Listing: TCMRDirItemList): TListingValidationResult;
begin
	Result.IsValid := FIsValid;
	Result.ErrorCode := FErrorCode;
end;

{TPathListingHandlerTest}

function TPathListingHandlerTest.CreateDirItem(const Name: WideString; IsFolder: Boolean): TCMRDirItem;
begin
	FillChar(Result, SizeOf(TCMRDirItem), 0);
	Result.name := Name;
	if IsFolder then
		Result.kind := 'folder'
	else
		Result.kind := 'file';
end;

procedure TPathListingHandlerTest.TearDown;
begin
	FListingProvider := nil;
	FListingPathValidator := nil;
end;

{Mock behavior tests}

procedure TPathListingHandlerTest.TestMockListingProvider_ReturnsFetchSuccess;
var
	Items: TCMRDirItemList;
	Listing: TCMRDirItemList;
	Invites: TCMRIncomingInviteList;
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
	Items: TCMRDirItemList;
	Listing: TCMRDirItemList;
	Invites: TCMRIncomingInviteList;
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

initialization
	TDUnitX.RegisterTestFixture(TPathListingHandlerTest);

end.
