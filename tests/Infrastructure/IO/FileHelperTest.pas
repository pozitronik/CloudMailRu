unit FileHelperTest;

interface

uses
	DUnitX.TestFramework,
	TestHelper,
	SysUtils,
	Windows,
	FileHelper;

type

	[TestFixture]
	TFileHelperTest = class(TObject)
	private const
		TEST_TEMP_FILE = 'tempfile.txt';
		TEST_FILE_NON_EXISTING = 'nonexistingfile.txt';
	public
		[Test]
		procedure TestSetAllFileTime;
		[Test]
		procedure TestSetAllFileTimeNonExistentFile;
	end;

implementation

procedure TFileHelperTest.TestSetAllFileTime;
var
	FileName: string;
	OriginalTime, NewTime, RetrievedTime: TFileTime;
	SystemTime: TSystemTime;
	Handle: THandle;
begin
	// Create a temporary file for testing
	FileName := DataPath(TEST_TEMP_FILE);
	Handle := FileCreate(FileName);
	try
		Assert.IsTrue(Handle <> THandle(-1), 'Failed to create test file');

		// Get the original file time
		Assert.IsTrue(GetFileTime(Handle, @OriginalTime, nil, nil), 'Failed to get original file time');
		FileClose(Handle);

		// Set a new file time
		DateTimeToSystemTime(Now, SystemTime); // Convert current time to system time
		SystemTimeToFileTime(SystemTime, NewTime); // Convert system time to file time
		SetAllFileTime(FileName, NewTime); // Call the method to test

		// Retrieve the updated file time
		Handle := FileOpen(FileName, FmOpenRead);

		Assert.IsTrue(Handle <> THandle(-1), 'Failed to open test file');
		Assert.IsTrue(GetFileTime(Handle, @RetrievedTime, nil, nil), 'Failed to get updated file time');
		FileClose(Handle);

		// Compare the new time with the retrieved time
		Assert.IsTrue(CompareFileTime(NewTime, RetrievedTime) = 0, 'The file time was not updated correctly');
	finally
		// Clean up: Delete the temporary file
		SysUtils.DeleteFile(FileName);
	end;
end;

{ TDD: Test that SetAllFileTime handles non-existent files gracefully without crashing }
procedure TFileHelperTest.TestSetAllFileTimeNonExistentFile;
var
	FileName: string;
	NewTime: TFileTime;
	SystemTime: TSystemTime;
begin
	FileName := DataPath(TEST_FILE_NON_EXISTING);
	DateTimeToSystemTime(Now, SystemTime);
	SystemTimeToFileTime(SystemTime, NewTime);
	{ Should not crash or raise exception when file doesn't exist }
	SetAllFileTime(FileName, NewTime);
	Assert.Pass('SetAllFileTime should handle non-existent files gracefully');
end;

initialization

TDUnitX.RegisterTestFixture(TFileHelperTest);

end.
