unit TestPathHelper;

interface

uses
	PathHelper,
	DUnitX.TestFramework;

type

	[TestFixture]
	TTestPathHelper = class
	public
		[Test]
		procedure TestIncludeSlash;
		[Test]
		procedure TestChangePathFileName;
		[Test]
		procedure TestCopyExt;
		[Test]
		procedure TestGetUNCFilePath;
		[Test]
		procedure TestGetLFCFilePath;
		[Test]
		procedure TestPathToUrl;
		[Test]
		procedure TestUrlToPath;
		[Test]
		procedure TestExtractUniversalFilePath;
		[Test]
		procedure TestExtractUniversalFileName;
		[Test]
		procedure TestExtractUniversalFileExt;
	end;

implementation

{TTestPathHelper}

procedure TTestPathHelper.TestCopyExt;
var
	FromFilename, ToFilename, ExpectedFilename: string;
begin
	// Case 1: Copying an extension to a filename without an extension
	FromFilename := 'source.txt';
	ToFilename := 'target';
	ExpectedFilename := 'target.txt';
	Assert.AreEqual(ExpectedFilename, CopyExt(FromFilename, ToFilename), 'CopyExt should copy the extension from source to target');

	// Case 2: Overwriting an existing extension in the target filename
	FromFilename := 'document.pdf';
	ToFilename := 'report.doc';
	ExpectedFilename := 'report.pdf';
	Assert.AreEqual(ExpectedFilename, CopyExt(FromFilename, ToFilename), 'CopyExt should overwrite the existing extension in the target filename');

end;

procedure TTestPathHelper.TestExtractUniversalFileExt;
var
	FileName, ExpectedResult: string;
begin
	// Test with a standard file name with extension
	FileName := 'File.txt';
	ExpectedResult := '.txt';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileExt(FileName), 'Standard file name should return the correct extension with dot');

	// Test with TrimDot = True
	FileName := 'File.txt';
	ExpectedResult := 'txt';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileExt(FileName, True), 'Standard file name should return the correct extension without dot');

	// Test with a file name without extension
	FileName := 'File';
	ExpectedResult := '';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileExt(FileName), 'File name without extension should return an empty string');

	// Test with a file name having multiple dots
	FileName := 'File.backup.txt';
	ExpectedResult := '.txt';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileExt(FileName), 'File name with multiple dots should return the last extension');

	// Test with a file path
	FileName := 'C:\Folder\Subfolder\File.txt';
	ExpectedResult := '.txt';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileExt(FileName), 'File path should return the correct extension');

	// Test with a UNC path
	FileName := '\\Server\Share\Folder\File.txt';
	ExpectedResult := '.txt';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileExt(FileName), 'UNC path should return the correct extension');

	// Test with an empty string
	FileName := '';
	ExpectedResult := '';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileExt(FileName), 'Empty string should return an empty string');

	// Test with a file name having only a dot
	FileName := 'File.';
	ExpectedResult := '.';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileExt(FileName), 'File name with only a dot should return a dot');

	// Test with a file path ending with a dot
	FileName := 'C:\Folder\Subfolder\File.';
	ExpectedResult := '.';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileExt(FileName), 'File path ending with a dot should return a dot');

	// Test with a file name starting with a dot
	FileName := '.hiddenfile';
	ExpectedResult := '';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileExt(FileName, False), 'File name starting with a dot and no extension should return an empty string');

	// Test with a file name with invalid characters
	FileName := 'File<>.txt';
	ExpectedResult := '.txt';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileExt(FileName), 'File name with invalid characters should still return the correct extension');

	// Test with a path containing only delimiters
	FileName := '\\\///';
	ExpectedResult := '';
	Assert.AreEqual(ExpectedResult, ExtractUniversalFileExt(FileName), 'Path with only delimiters should return an empty string');
end;

procedure TTestPathHelper.TestExtractUniversalFileName;
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

procedure TTestPathHelper.TestExtractUniversalFilePath;
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

procedure TTestPathHelper.TestGetLFCFilePath;
var
	FilePath, ExpectedResult: WideString;
begin
	// Test with an extended-length UNC path
	FilePath := '\\?\C:\Folder\File.txt';
	ExpectedResult := 'C:\Folder\File.txt';
	Assert.AreEqual(ExpectedResult, GetLFCFilePath(FilePath), 'Extended-length UNC path should be converted to LFS path');

	// Test with a standard local path
	FilePath := 'C:\Folder\File.txt';
	ExpectedResult := 'C:\Folder\File.txt'; // Assuming no change for standard paths
	Assert.AreEqual(ExpectedResult, GetLFCFilePath(FilePath), 'Standard local path should remain unchanged');

	// Test with an empty string
	FilePath := '';
	ExpectedResult := '';
	Assert.AreEqual(ExpectedResult, GetLFCFilePath(FilePath), 'Empty string should remain unchanged');

	// Test with a path that is just the UNC prefix
	FilePath := '\\?\';
	ExpectedResult := ''; // Assuming the prefix is removed, resulting in an empty string
	Assert.AreEqual(ExpectedResult, GetLFCFilePath(FilePath), 'Path with only UNC prefix should be handled');

	// Test with a short path (less than 4 characters)
	FilePath := 'C:\';
	ExpectedResult := 'C:\'; // Assuming no change for short paths
	Assert.AreEqual(ExpectedResult, GetLFCFilePath(FilePath), 'Short path should remain unchanged');

	// Test with a path slightly longer than the UNC prefix
	FilePath := '\\?\C';
	ExpectedResult := 'C'; // Assuming the prefix is removed
	Assert.AreEqual(ExpectedResult, GetLFCFilePath(FilePath), 'Path slightly longer than UNC prefix should be handled');

end;

procedure TTestPathHelper.TestGetUNCFilePath;
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

procedure TTestPathHelper.TestIncludeSlash;
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

procedure TTestPathHelper.TestPathToUrl;
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

procedure TTestPathHelper.TestUrlToPath;
var
	URL, ExpectedResult: WideString;
begin
	// Test with a standard URL
	URL := 'http://example.com/folder/file.txt';
	ExpectedResult := 'http:\\example.com\folder\file.txt';
	Assert.AreEqual(ExpectedResult, UrlToPath(URL), 'Standard URL should be converted to a file path');

	// Test with a URL that contains encoded characters
	URL := 'http://example.com/folder%20name/file%20name.txt';
	ExpectedResult := 'http:\\example.com\folder%20name\file%20name.txt'; // Encoded characters are not decoded
	Assert.AreEqual(ExpectedResult, UrlToPath(URL), 'URL with encoded characters should keep the encoding but convert slashes');

	// Test with a URL that only contains forward slashes
	URL := '/folder/name/';
	ExpectedResult := '\folder\name\';
	Assert.AreEqual(ExpectedResult, UrlToPath(URL), 'URL with only forward slashes should be converted correctly');

	// Test with a URL with mixed slashes
	URL := 'http:/example.com\folder/name/';
	ExpectedResult := 'http:\example.com\folder\name\';
	Assert.AreEqual(ExpectedResult, UrlToPath(URL), 'URL with mixed slashes should be converted to use backslashes only');

	// Test with an empty string
	URL := '';
	ExpectedResult := '';
	Assert.AreEqual(ExpectedResult, UrlToPath(URL), 'Empty URL should return an empty string');
end;

procedure TTestPathHelper.TestChangePathFileName;
var
	FilePath, NewFileName, ExpectedResult: string;
begin
	FilePath := 'C:\TestDirectory\OldFileName.txt';
	NewFileName := 'NewFileName.txt';
	ExpectedResult := 'C:\TestDirectory\NewFileName.txt';

	Assert.AreEqual(ExpectedResult, ChangePathFileName(FilePath, NewFileName), 'ChangePathFileName should correctly change the file name');
end;

initialization

TDUnitX.RegisterTestFixture(TTestPathHelper);

end.
