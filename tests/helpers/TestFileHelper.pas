unit TestFileHelper;

interface

uses
	DUnitX.TestFramework,
	TestHelper,
	FileHelper;

type

	[TestFixture]
	TTestFileHelper = class(TObject)
	private const
		TEST_WRITEABLE_DIR = 'writeable_dir';
		TEST_NONWRITEABLE_DIR = 'nonwriteable_dir';
	public
		[Test]
		procedure TestIsWriteable;
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

initialization

TDUnitX.RegisterTestFixture(TTestFileHelper);

end.
