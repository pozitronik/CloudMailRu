unit ListingPathValidatorTest;

{Unit tests for TListingPathValidator - path validation before directory listing.
 Tests virtual path constraints and directory item validation.}

interface

uses
	Windows,
	DUnitX.TestFramework,
	CloudDirItem,
	CloudDirItemList,
	CloudConstants,
	ListingPathValidator;

type
	[TestFixture]
	TListingPathValidatorTest = class
	private
		FValidator: IListingPathValidator;

		function CreateDirItem(const Name, HomePath: WideString): TCloudDirItem;
		function CreateFileItem(const Name, HomePath: WideString): TCloudDirItem;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Virtual path constraint tests}
		[Test]
		procedure TestValidatePath_VirtualNotInAccountsList_ReturnsAccessDenied;
		[Test]
		procedure TestValidatePath_VirtualInAccountsList_ReturnsValid;
		[Test]
		procedure TestValidatePath_NotVirtual_PassesVirtualCheck;

		{Directory validation tests - private account}
		[Test]
		procedure TestValidatePath_PrivateAccount_DirectoryFound_ReturnsValid;
		[Test]
		procedure TestValidatePath_PrivateAccount_FileFound_ReturnsPathNotFound;
		[Test]
		procedure TestValidatePath_PrivateAccount_NotFound_ReturnsValid;

		{Directory validation tests - public account}
		[Test]
		procedure TestValidatePath_PublicAccount_DirectoryFound_ReturnsValid;
		[Test]
		procedure TestValidatePath_PublicAccount_FileFound_ReturnsPathNotFound;

		{Empty listing tests}
		[Test]
		procedure TestValidatePath_EmptyListing_ReturnsValid;

		{Combined scenarios}
		[Test]
		procedure TestValidatePath_VirtualButFile_ReturnsAccessDeniedFirst;
	end;

implementation

{Helper methods}

function TListingPathValidatorTest.CreateDirItem(const Name, HomePath: WideString): TCloudDirItem;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.name := Name;
	Result.home := HomePath;
	Result.type_ := TYPE_DIR;
end;

function TListingPathValidatorTest.CreateFileItem(const Name, HomePath: WideString): TCloudDirItem;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.name := Name;
	Result.home := HomePath;
	Result.type_ := TYPE_FILE;
end;

{Setup/TearDown}

procedure TListingPathValidatorTest.Setup;
begin
	FValidator := TListingPathValidator.Create;
end;

procedure TListingPathValidatorTest.TearDown;
begin
	FValidator := nil;
end;

{Virtual path constraint tests}

procedure TListingPathValidatorTest.TestValidatePath_VirtualNotInAccountsList_ReturnsAccessDenied;
var
	Listing: TCloudDirItemList;
	Result: TListingValidationResult;
begin
	{Virtual path but not at account level - should deny access}
	SetLength(Listing, 0);

	Result := FValidator.ValidatePath(True, False, False, '/somepath', Listing);

	Assert.IsFalse(Result.IsValid, 'Should not be valid');
	Assert.AreEqual(Cardinal(ERROR_ACCESS_DENIED), Result.ErrorCode, 'Should return ACCESS_DENIED');
end;

procedure TListingPathValidatorTest.TestValidatePath_VirtualInAccountsList_ReturnsValid;
var
	Listing: TCloudDirItemList;
	Result: TListingValidationResult;
begin
	{Virtual path at account level - should allow}
	SetLength(Listing, 0);

	Result := FValidator.ValidatePath(True, True, False, '/somepath', Listing);

	Assert.IsTrue(Result.IsValid, 'Should be valid when at account level');
	Assert.AreEqual(Cardinal(0), Result.ErrorCode, 'Should have no error');
end;

procedure TListingPathValidatorTest.TestValidatePath_NotVirtual_PassesVirtualCheck;
var
	Listing: TCloudDirItemList;
	Result: TListingValidationResult;
begin
	{Non-virtual path should pass virtual check regardless of IsInAccountsList}
	SetLength(Listing, 0);

	Result := FValidator.ValidatePath(False, False, False, '/somepath', Listing);

	Assert.IsTrue(Result.IsValid, 'Non-virtual path should pass');
end;

{Directory validation tests - private account}

procedure TListingPathValidatorTest.TestValidatePath_PrivateAccount_DirectoryFound_ReturnsValid;
var
	Listing: TCloudDirItemList;
	Result: TListingValidationResult;
begin
	{Private account: lookup by home path, found directory}
	{Note: FindByHomePath prepends '/' to path, so we use path without leading slash}
	SetLength(Listing, 1);
	Listing[0] := CreateDirItem('folder', '/folder');

	Result := FValidator.ValidatePath(False, False, False, 'folder', Listing);

	Assert.IsTrue(Result.IsValid, 'Directory should be valid');
	Assert.AreEqual(Cardinal(0), Result.ErrorCode);
end;

procedure TListingPathValidatorTest.TestValidatePath_PrivateAccount_FileFound_ReturnsPathNotFound;
var
	Listing: TCloudDirItemList;
	Result: TListingValidationResult;
begin
	{Private account: lookup by home path, found file - can't list a file}
	{Note: FindByHomePath prepends '/' to path, so we use path without leading slash}
	SetLength(Listing, 1);
	Listing[0] := CreateFileItem('document.txt', '/document.txt');

	Result := FValidator.ValidatePath(False, False, False, 'document.txt', Listing);

	Assert.IsFalse(Result.IsValid, 'File should not be valid for listing');
	Assert.AreEqual(Cardinal(ERROR_PATH_NOT_FOUND), Result.ErrorCode);
end;

procedure TListingPathValidatorTest.TestValidatePath_PrivateAccount_NotFound_ReturnsValid;
var
	Listing: TCloudDirItemList;
	Result: TListingValidationResult;
begin
	{Private account: path not in listing - valid (empty directory)}
	{Note: FindByHomePath prepends '/' to path, so we use path without leading slash}
	SetLength(Listing, 1);
	Listing[0] := CreateDirItem('other', '/other');

	Result := FValidator.ValidatePath(False, False, False, 'notfound', Listing);

	Assert.IsTrue(Result.IsValid, 'Not found (isNone) should be valid');
end;

{Directory validation tests - public account}

procedure TListingPathValidatorTest.TestValidatePath_PublicAccount_DirectoryFound_ReturnsValid;
var
	Listing: TCloudDirItemList;
	Result: TListingValidationResult;
begin
	{Public account: lookup by name, found directory}
	SetLength(Listing, 1);
	Listing[0] := CreateDirItem('myfolder', '/some/path/myfolder');

	Result := FValidator.ValidatePath(False, False, True, '/any/path/myfolder', Listing);

	Assert.IsTrue(Result.IsValid, 'Directory should be valid');
end;

procedure TListingPathValidatorTest.TestValidatePath_PublicAccount_FileFound_ReturnsPathNotFound;
var
	Listing: TCloudDirItemList;
	Result: TListingValidationResult;
begin
	{Public account: lookup by name, found file - can't list a file}
	SetLength(Listing, 1);
	Listing[0] := CreateFileItem('readme.txt', '/public/readme.txt');

	Result := FValidator.ValidatePath(False, False, True, '/any/readme.txt', Listing);

	Assert.IsFalse(Result.IsValid, 'File should not be valid for listing');
	Assert.AreEqual(Cardinal(ERROR_PATH_NOT_FOUND), Result.ErrorCode);
end;

{Empty listing tests}

procedure TListingPathValidatorTest.TestValidatePath_EmptyListing_ReturnsValid;
var
	Listing: TCloudDirItemList;
	Result: TListingValidationResult;
begin
	{Empty listing - valid (empty directory)}
	SetLength(Listing, 0);

	Result := FValidator.ValidatePath(False, False, False, '/emptydir', Listing);

	Assert.IsTrue(Result.IsValid, 'Empty listing should be valid');
end;

{Combined scenarios}

procedure TListingPathValidatorTest.TestValidatePath_VirtualButFile_ReturnsAccessDeniedFirst;
var
	Listing: TCloudDirItemList;
	Result: TListingValidationResult;
begin
	{Virtual constraint is checked before item type - should fail on virtual first}
	SetLength(Listing, 1);
	Listing[0] := CreateFileItem('file.txt', '/file.txt');

	Result := FValidator.ValidatePath(True, False, False, '/file.txt', Listing);

	Assert.IsFalse(Result.IsValid, 'Should not be valid');
	Assert.AreEqual(Cardinal(ERROR_ACCESS_DENIED), Result.ErrorCode,
		'Should return ACCESS_DENIED (virtual check first), not PATH_NOT_FOUND');
end;

initialization
	TDUnitX.RegisterTestFixture(TListingPathValidatorTest);

end.
