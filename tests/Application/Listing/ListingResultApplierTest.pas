unit ListingResultApplierTest;

{Tests for TListingResultApplier.
 Verifies common result fields are correctly applied to output variables.}

interface

uses
	DUnitX.TestFramework,
	Windows,
	ListingResultApplier;

type
	[TestFixture]
	TListingResultApplierTest = class
	private
		FApplier: IListingResultApplier;
	public
		[Setup]
		procedure Setup;

		[TearDown]
		procedure TearDown;

		[Test]
		procedure Apply_SetsFileCounterFromBase;

		[Test]
		procedure Apply_SetsFindDataFromBase;

		[Test]
		procedure Apply_ReturnsHandleFromBase;

		[Test]
		procedure Apply_WhenErrorCodeIsZero_DoesNotCallSetLastError;

		[Test]
		procedure Apply_WhenErrorCodeIsNonZero_CallsSetLastError;

		[Test]
		procedure Apply_PreservesAllFindDataFields;

		[Test]
		procedure Apply_HandlesZeroFileCounter;

		[Test]
		procedure Apply_HandlesMaxFileCounter;

		[Test]
		procedure Apply_HandlesINVALID_HANDLE_VALUE;
	end;

implementation

uses
	SysUtils;

procedure TListingResultApplierTest.Setup;
begin
	FApplier := TListingResultApplier.Create;
end;

procedure TListingResultApplierTest.TearDown;
begin
	FApplier := nil;
end;

procedure TListingResultApplierTest.Apply_SetsFileCounterFromBase;
var
	Base: TListingResultBase;
	FindData: tWIN32FINDDATAW;
	FileCounter: Integer;
begin
	Base.FileCounter := 42;
	Base.ErrorCode := 0;
	Base.Handle := 0;
	FillChar(Base.FindData, SizeOf(Base.FindData), 0);
	FillChar(FindData, SizeOf(FindData), 0);
	FileCounter := 0;

	FApplier.Apply(Base, FindData, FileCounter);

	Assert.AreEqual(42, FileCounter, 'FileCounter should be set from Base');
end;

procedure TListingResultApplierTest.Apply_SetsFindDataFromBase;
var
	Base: TListingResultBase;
	FindData: tWIN32FINDDATAW;
	FileCounter: Integer;
begin
	FillChar(Base, SizeOf(Base), 0);
	Base.FindData.dwFileAttributes := FILE_ATTRIBUTE_DIRECTORY;
	Base.FindData.nFileSizeLow := 12345;
	FillChar(FindData, SizeOf(FindData), 0);
	FileCounter := 0;

	FApplier.Apply(Base, FindData, FileCounter);

	Assert.AreEqual(Cardinal(FILE_ATTRIBUTE_DIRECTORY), FindData.dwFileAttributes, 'dwFileAttributes should be copied');
	Assert.AreEqual(DWORD(12345), FindData.nFileSizeLow, 'nFileSizeLow should be copied');
end;

procedure TListingResultApplierTest.Apply_ReturnsHandleFromBase;
var
	Base: TListingResultBase;
	FindData: tWIN32FINDDATAW;
	FileCounter: Integer;
	ResultHandle: THandle;
begin
	FillChar(Base, SizeOf(Base), 0);
	Base.Handle := 999;
	FillChar(FindData, SizeOf(FindData), 0);
	FileCounter := 0;

	ResultHandle := FApplier.Apply(Base, FindData, FileCounter);

	Assert.AreEqual(THandle(999), ResultHandle, 'Handle should be returned from Base');
end;

procedure TListingResultApplierTest.Apply_WhenErrorCodeIsZero_DoesNotCallSetLastError;
var
	Base: TListingResultBase;
	FindData: tWIN32FINDDATAW;
	FileCounter: Integer;
	PreviousError: DWORD;
begin
	{Set a known error code}
	SetLastError(12345);
	PreviousError := GetLastError;

	FillChar(Base, SizeOf(Base), 0);
	Base.ErrorCode := 0;
	FillChar(FindData, SizeOf(FindData), 0);
	FileCounter := 0;

	FApplier.Apply(Base, FindData, FileCounter);

	{Last error should remain unchanged when ErrorCode is 0}
	Assert.AreEqual(PreviousError, GetLastError, 'SetLastError should not be called when ErrorCode is 0');
end;

procedure TListingResultApplierTest.Apply_WhenErrorCodeIsNonZero_CallsSetLastError;
var
	Base: TListingResultBase;
	FindData: tWIN32FINDDATAW;
	FileCounter: Integer;
begin
	FillChar(Base, SizeOf(Base), 0);
	Base.ErrorCode := ERROR_NO_MORE_FILES;
	FillChar(FindData, SizeOf(FindData), 0);
	FileCounter := 0;

	FApplier.Apply(Base, FindData, FileCounter);

	Assert.AreEqual(DWORD(ERROR_NO_MORE_FILES), GetLastError, 'SetLastError should be called with ErrorCode');
end;

procedure TListingResultApplierTest.Apply_PreservesAllFindDataFields;
var
	Base: TListingResultBase;
	FindData: tWIN32FINDDATAW;
	FileCounter: Integer;
	ExpectedTime: TFileTime;
begin
	FillChar(Base, SizeOf(Base), 0);
	ExpectedTime.dwLowDateTime := 100;
	ExpectedTime.dwHighDateTime := 200;
	Base.FindData.dwFileAttributes := FILE_ATTRIBUTE_NORMAL;
	Base.FindData.ftCreationTime := ExpectedTime;
	Base.FindData.ftLastAccessTime := ExpectedTime;
	Base.FindData.ftLastWriteTime := ExpectedTime;
	Base.FindData.nFileSizeHigh := 1;
	Base.FindData.nFileSizeLow := 2;
	FillChar(FindData, SizeOf(FindData), 0);
	FileCounter := 0;

	FApplier.Apply(Base, FindData, FileCounter);

	Assert.AreEqual(Cardinal(FILE_ATTRIBUTE_NORMAL), FindData.dwFileAttributes, 'dwFileAttributes mismatch');
	Assert.AreEqual(ExpectedTime.dwLowDateTime, FindData.ftCreationTime.dwLowDateTime, 'ftCreationTime mismatch');
	Assert.AreEqual(DWORD(1), FindData.nFileSizeHigh, 'nFileSizeHigh mismatch');
	Assert.AreEqual(DWORD(2), FindData.nFileSizeLow, 'nFileSizeLow mismatch');
end;

procedure TListingResultApplierTest.Apply_HandlesZeroFileCounter;
var
	Base: TListingResultBase;
	FindData: tWIN32FINDDATAW;
	FileCounter: Integer;
begin
	FillChar(Base, SizeOf(Base), 0);
	Base.FileCounter := 0;
	FillChar(FindData, SizeOf(FindData), 0);
	FileCounter := 999;

	FApplier.Apply(Base, FindData, FileCounter);

	Assert.AreEqual(0, FileCounter, 'FileCounter should be set to 0');
end;

procedure TListingResultApplierTest.Apply_HandlesMaxFileCounter;
var
	Base: TListingResultBase;
	FindData: tWIN32FINDDATAW;
	FileCounter: Integer;
begin
	FillChar(Base, SizeOf(Base), 0);
	Base.FileCounter := MaxInt;
	FillChar(FindData, SizeOf(FindData), 0);
	FileCounter := 0;

	FApplier.Apply(Base, FindData, FileCounter);

	Assert.AreEqual(MaxInt, FileCounter, 'FileCounter should handle MaxInt');
end;

procedure TListingResultApplierTest.Apply_HandlesINVALID_HANDLE_VALUE;
var
	Base: TListingResultBase;
	FindData: tWIN32FINDDATAW;
	FileCounter: Integer;
	ResultHandle: THandle;
begin
	FillChar(Base, SizeOf(Base), 0);
	Base.Handle := INVALID_HANDLE_VALUE;
	FillChar(FindData, SizeOf(FindData), 0);
	FileCounter := 0;

	ResultHandle := FApplier.Apply(Base, FindData, FileCounter);

	Assert.AreEqual(THandle(INVALID_HANDLE_VALUE), ResultHandle, 'Should handle INVALID_HANDLE_VALUE');
end;

initialization
	TDUnitX.RegisterTestFixture(TListingResultApplierTest);

end.
