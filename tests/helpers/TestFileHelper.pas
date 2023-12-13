unit TestFileHelper;

interface

uses
	DUnitX.TestFramework,
	TestHelper,
	SysUtils,
	Windows,
	FileHelper;

type

	[TestFixture]
	TTestFileHelper = class(TObject)
	private const
		TEST_WRITEABLE_DIR = 'writeable_dir';
		TEST_NONWRITEABLE_DIR = 'nonwriteable_dir';
		TEST_TEMP_FILE = 'tempfile.txt';
		TEST_SIZE_FILE = 'Settings.ini';
		TEST_FILE_NON_EXISTING = 'nonexistingfile.txt';
	public
		[Test]
		procedure TestIsWriteable;
		[Test]
		procedure TestSetAllFileTime;
		[Test]
		procedure TestSizeOfFileWithExistingFile;
		[Test]
		procedure TestSizeOfFileWithNonExistingFile;
	end;

implementation

procedure TTestFileHelper.TestIsWriteable;
var
	WriteableDir: string;
	NonWriteableDir: string;
begin
	// Define a directory that is known to be writeable
	WriteableDir := DataPath(TEST_WRITEABLE_DIR); // Use DataPath to get the relative path

	// Test if the function returns true for the writeable directory
	Assert.IsTrue(IsWriteable(WriteableDir), 'Writeable directory should return true');

	// Define a directory that is known to be non-writeable (if possible)
	NonWriteableDir := DataPath(TEST_NONWRITEABLE_DIR); // Use DataPath to get the relative path

	// Test if the function returns false for the non-writeable directory
	// This might require running the test with specific permissions to ensure the directory is indeed non-writeable
	Assert.IsFalse(IsWriteable(NonWriteableDir), 'Non-writeable directory should return false');
end;

procedure TTestFileHelper.TestSetAllFileTime;
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

procedure TTestFileHelper.TestSizeOfFileWithExistingFile;
var
	FileName: String;
	FileSize: Int64;
begin
	FileName := DataPath(TEST_SIZE_FILE);
	FileSize := 817;

	Assert.AreEqual(FileSize, SizeOfFile(FileName), 'Size of existing file should be correctly returned');
end;

procedure TTestFileHelper.TestSizeOfFileWithNonExistingFile;
var
	FileName: String;
begin
	FileName := DataPath(TEST_FILE_NON_EXISTING);
	Assert.IsTrue(-1 = SizeOfFile(FileName), 'Size of non-existing file should return -1');
end;

initialization

TDUnitX.RegisterTestFixture(TTestFileHelper);

end.
