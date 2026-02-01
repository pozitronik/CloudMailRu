unit PathHelperTest;

interface

uses
	PathHelper,
	DUnitX.TestFramework;

type

	[TestFixture]
	TPathHelperTest = class
	public
		[Test]
		procedure TestIncludeSlash;
		[Test]
		procedure TestGetUNCFilePath;
		[Test]
		procedure TestPathToUrl;
		[Test]
		procedure TestExtractUniversalFilePath;
		[Test]
		procedure TestExtractUniversalFileName;
	end;

implementation

{TPathHelperTest}

procedure TPathHelperTest.TestExtractUniversalFileName;
var
	FileName, ExpectedResult: string;
begin
	// Test with a Windows-style path
	FileName := 'C:\Folder\Subfolder\File.txt';
	ExpectedResult := 'File.txt';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileName(FileName), 'Windows-style path should return the correct file name');

	// Test with a Unix-style path
	FileName := '/usr/local/bin/script.sh';
	ExpectedResult := 'script.sh';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileName(FileName), 'Unix-style path should return the correct file name');

	// Test with mixed delimiters
	FileName := 'C:/Folder/Subfolder/File.txt';
	ExpectedResult := 'File.txt';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileName(FileName), 'Path with mixed delimiters should return the correct file name');

	// Test with a UNC path
	FileName := '\\Server\Share\Folder\File.txt';
	ExpectedResult := 'File.txt';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileName(FileName), 'UNC path should return the correct file name');

	// Test with only the file name (no path)
	FileName := 'File.txt';
	ExpectedResult := 'File.txt';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileName(FileName), 'File name without path should return the file name');

	// Test with an empty string
	FileName := '';
	ExpectedResult := '';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileName(FileName), 'Empty string should return an empty string');

end;

procedure TPathHelperTest.TestExtractUniversalFilePath;
var
	FileName, ExpectedResult: string;
begin
	// Test with a Windows-style path
	FileName := 'C:\Folder\Subfolder\File.txt';
	ExpectedResult := 'C:\Folder\Subfolder\';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFilePath(FileName), 'Windows-style path should be extracted correctly');

	// Test with a Unix-style path
	FileName := '/usr/local/bin/script.sh';
	ExpectedResult := '/usr/local/bin/';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFilePath(FileName), 'Unix-style path should be extracted correctly');

	// Test with mixed delimiters
	FileName := 'C:/Folder/Subfolder/File.txt';
	ExpectedResult := 'C:/Folder/Subfolder/';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFilePath(FileName), 'Path with mixed delimiters should be extracted correctly');

	// Test with a UNC path
	FileName := '\\Server\Share\Folder\File.txt';
	ExpectedResult := '\\Server\Share\Folder\';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFilePath(FileName), 'UNC path should be extracted correctly');

	// Test with only the file name (no path)
	FileName := 'File.txt';
	ExpectedResult := '';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFilePath(FileName), 'File name without path should return an empty string');

	// Test with an empty string
	FileName := '';
	ExpectedResult := '';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFilePath(FileName), 'Empty string should return an empty string');

end;

procedure TPathHelperTest.TestGetUNCFilePath;
var
	FilePath, ExpectedResult: WideString;
begin
	// Test with a standard UNC path
	FilePath := '\\Server\Share';
	ExpectedResult := '\\Server\Share'; // Assuming no change for valid UNC paths
	Assert.AreEqual(ExpectedResult, GetUNCFilePath(FilePath), 'UNC path should remain unchanged');

	// Test with a local absolute path
	FilePath := 'C:\Folder\File.txt';
	ExpectedResult := '\\?\C:\Folder\File.txt'; // Assuming conversion to extended-length path
	Assert.AreEqual(ExpectedResult, GetUNCFilePath(FilePath), 'Local absolute path should be converted');

	// Test with a simple relative path
	FilePath := 'Folder\File.txt';
	ExpectedResult := '\\?\Folder\File.txt'; // Assuming conversion to extended-length path
	Assert.AreEqual(ExpectedResult, GetUNCFilePath(FilePath), 'Simple relative path should be converted');

	// Test with a dot (current directory) relative path
	FilePath := '.\File.txt';
	ExpectedResult := '\\?\.\File.txt'; // Assuming conversion
	Assert.AreEqual(ExpectedResult, GetUNCFilePath(FilePath), 'Dot relative path should be converted');

	// Test with a dot-dot (parent directory) relative path
	FilePath := '..\File.txt';
	ExpectedResult := '\\?\..\File.txt'; // Assuming conversion
	Assert.AreEqual(ExpectedResult, GetUNCFilePath(FilePath), 'Dot-dot relative path should be converted');

	// Test with an empty string
	FilePath := '';
	ExpectedResult := '\\?\'; // Assuming conversion even for empty string
	Assert.AreEqual(ExpectedResult, GetUNCFilePath(FilePath), 'Empty string should be handled');
end;

procedure TPathHelperTest.TestIncludeSlash;
var
	TestURL: string;
begin
	// Test with a URL without a trailing slash
	TestURL := 'http://example.com/test';
	Assert.AreEqual('http://example.com/test/', IncludeSlash(TestURL), 'IncludeSlash should add a trailing slash to URL');

	// Test with a URL that already has a trailing slash
	TestURL := 'http://example.com/test/';
	Assert.AreEqual('http://example.com/test/', IncludeSlash(TestURL), 'IncludeSlash should not add an extra slash if one already exists');

	//	 Optionally, test with an empty string if you decide to keep the initial condition
	TestURL := '';
	Assert.AreEqual('/', IncludeSlash(TestURL), 'IncludeSlash should return a single slash for an empty string');
end;

procedure TPathHelperTest.TestPathToUrl;
var
	Path, ExpectedResult: WideString;
begin
	// Test converting a standard path
	Path := 'C:\Folder\File.txt';
	ExpectedResult := 'C%3A/Folder/File.txt'; // URL encoding the colon
	Assert.AreEqual(ExpectedResult, PathToUrl(Path), 'Standard path should be converted to URL format');

	// Test URL encoding
	Path := 'C:\Folder Name\File Name.txt';
	ExpectedResult := 'C%3A/Folder%20Name/File%20Name.txt'; // URL encoding spaces and the colon
	Assert.AreEqual(ExpectedResult, PathToUrl(Path), 'Path with spaces should be URL encoded');

	// Test URL encoding with special characters
	Path := 'C:\Folder&Name\File(Name).txt';
	ExpectedResult := 'C%3A/Folder%26Name/File%28Name%29.txt'; // URL encoding &, (, ), and the colon
	Assert.AreEqual(ExpectedResult, PathToUrl(Path), 'Path with special characters should be URL encoded');

	// Test with URL encoding turned off
	Path := 'C:\Folder Name\File Name.txt';
	ExpectedResult := 'C:/Folder Name/File Name.txt'; // No URL encoding
	Assert.AreEqual(ExpectedResult, PathToUrl(Path, True, False), 'Path should not be URL encoded when encoding is off');

	// Test handling of empty string with RestrictEmptyUrl true
	Path := '';
	ExpectedResult := '/'; // RestrictEmptyUrl is true
	Assert.AreEqual(ExpectedResult, PathToUrl(Path), 'Empty path should return "/" when RestrictEmptyUrl is true');

	// Test handling of empty string with RestrictEmptyUrl false
	Path := '';
	ExpectedResult := ''; // No restriction on empty URL
	Assert.AreEqual(ExpectedResult, PathToUrl(Path, False), 'Empty path should remain empty when RestrictEmptyUrl is false');
end;

initialization

TDUnitX.RegisterTestFixture(TPathHelperTest);

end.
