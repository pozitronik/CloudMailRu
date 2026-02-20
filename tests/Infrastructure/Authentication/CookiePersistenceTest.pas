unit CookiePersistenceTest;

interface

uses
	IdCookieManager, IdCookie, IdURI,
	FileSystem,
	CookiePersistence,
	DUnitX.TestFramework;

type

	[TestFixture]
	TCookiePersistenceTest = class
	private
		FFileSystem: TMemoryFileSystem;
		FFileSystemIntf: IFileSystem; // Prevents premature free via ref counting
		FCookieManager: TIdCookieManager;

		procedure AddTestCookie(const Name, Value, Domain, Path: WideString; Secure, HttpOnly: Boolean);
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure TestSave_CreatesCookieFile;
		[Test]
		procedure TestSave_WritesCSRFTokenAsFirstLine;
		[Test]
		procedure TestSave_WritesCookieData;
		[Test]
		procedure TestSave_EmptyFilePath_ReturnsFalse;
		[Test]
		procedure TestSave_NilCookieManager_ReturnsFalse;

		[Test]
		procedure TestLoad_RestoresCookies;
		[Test]
		procedure TestLoad_RestoresCSRFToken;
		[Test]
		procedure TestLoad_FileNotFound_ReturnsFalse;
		[Test]
		procedure TestLoad_EmptyFile_ReturnsFalse;
		[Test]
		procedure TestLoad_EmptyFilePath_ReturnsFalse;
		[Test]
		procedure TestLoad_NilCookieManager_ReturnsFalse;
		[Test]
		procedure TestLoad_SkipsMalformedLines;
		[Test]
		procedure TestLoad_NoCSRFToken_ReturnsFalse;

		[Test]
		procedure TestSaveLoad_Roundtrip;
		[Test]
		procedure TestSaveLoad_MultipleCookies;
		[Test]
		procedure TestSaveLoad_SecureAndHttpOnlyFlags;

		[Test]
		procedure TestDelete_RemovesFile;
		[Test]
		procedure TestDelete_NoFile_NoError;

		[Test]
		procedure TestExists_FilePresent_ReturnsTrue;
		[Test]
		procedure TestExists_FileAbsent_ReturnsFalse;
		[Test]
		procedure TestExists_EmptyFilePath_ReturnsFalse;
	end;

	[TestFixture]
	TCookiePersistenceBuildPathTest = class
	public
		[Test]
		procedure TestBuildFilePath_BasicFormat;
		[Test]
		procedure TestBuildFilePath_TrailingSlashInConfigDir;

		[Test]
		procedure TestSanitizeAccountName_ReplacesAt;
		[Test]
		procedure TestSanitizeAccountName_ReplacesUnsafeChars;
		[Test]
		procedure TestSanitizeAccountName_PreservesSafeChars;
		[Test]
		procedure TestSanitizeAccountName_MultipleDots;
	end;

implementation

uses
	SysUtils, Classes;

{TCookiePersistenceTest}

procedure TCookiePersistenceTest.Setup;
begin
	FFileSystem := TMemoryFileSystem.Create;
	FFileSystemIntf := FFileSystem;
	FCookieManager := TIdCookieManager.Create(nil);
end;

procedure TCookiePersistenceTest.TearDown;
begin
	FCookieManager.Free;
	FFileSystemIntf := nil;
end;

procedure TCookiePersistenceTest.AddTestCookie(const Name, Value, Domain, Path: WideString; Secure, HttpOnly: Boolean);
var
	CookieStr: WideString;
	URL: TIdURI;
begin
	CookieStr := Name + '=' + Value
		+ '; Domain=' + Domain
		+ '; Path=' + Path;

	if Secure then
		CookieStr := CookieStr + '; Secure';
	if HttpOnly then
		CookieStr := CookieStr + '; HttpOnly';

	URL := TIdURI.Create('https://cloud.mail.ru/');
	try
		FCookieManager.AddServerCookie(CookieStr, URL);
	finally
		URL.Free;
	end;
end;

procedure TCookiePersistenceTest.TestSave_CreatesCookieFile;
var
	Persistence: TCookiePersistence;
begin
	AddTestCookie('sid', 'abc123', '.mail.ru', '/', True, False);

	Persistence := TCookiePersistence.Create('C:\cookies\test.cookies', FFileSystem);
	try
		Persistence.Save(FCookieManager, 'csrf_token');
	finally
		Persistence.Free;
	end;

	Assert.IsTrue(FFileSystem.FileExists('C:\cookies\test.cookies'));
end;

procedure TCookiePersistenceTest.TestSave_WritesCSRFTokenAsFirstLine;
var
	Persistence: TCookiePersistence;
	Lines: TStringList;
begin
	AddTestCookie('sid', 'value', '.mail.ru', '/', True, False);

	Persistence := TCookiePersistence.Create('C:\cookies\test.cookies', FFileSystem);
	try
		Persistence.Save(FCookieManager, 'my_csrf_token');
	finally
		Persistence.Free;
	end;

	Lines := FFileSystem.ReadAllLines('C:\cookies\test.cookies', TEncoding.UTF8);
	try
		Assert.IsTrue(Lines.Count >= 1);
		Assert.AreEqual('__csrf__'#9'my_csrf_token', Lines[0]);
	finally
		Lines.Free;
	end;
end;

procedure TCookiePersistenceTest.TestSave_WritesCookieData;
var
	Persistence: TCookiePersistence;
	Content: WideString;
begin
	AddTestCookie('sid', 'abc123', '.mail.ru', '/', True, False);

	Persistence := TCookiePersistence.Create('C:\cookies\test.cookies', FFileSystem);
	try
		Persistence.Save(FCookieManager, 'csrf');
	finally
		Persistence.Free;
	end;

	Content := FFileSystem.ReadAllText('C:\cookies\test.cookies', TEncoding.UTF8);
	Assert.Contains(Content, 'sid');
	Assert.Contains(Content, 'abc123');
	// Indy strips the leading dot from cookie domains
	Assert.Contains(Content, 'mail.ru');
end;

procedure TCookiePersistenceTest.TestSave_EmptyFilePath_ReturnsFalse;
var
	Persistence: TCookiePersistence;
	SaveResult: Boolean;
begin
	Persistence := TCookiePersistence.Create('', FFileSystem);
	try
		SaveResult := Persistence.Save(FCookieManager, 'csrf');
	finally
		Persistence.Free;
	end;

	Assert.IsFalse(SaveResult);
end;

procedure TCookiePersistenceTest.TestSave_NilCookieManager_ReturnsFalse;
var
	Persistence: TCookiePersistence;
	SaveResult: Boolean;
begin
	Persistence := TCookiePersistence.Create('C:\test.cookies', FFileSystem);
	try
		SaveResult := Persistence.Save(nil, 'csrf');
	finally
		Persistence.Free;
	end;

	Assert.IsFalse(SaveResult);
end;

procedure TCookiePersistenceTest.TestLoad_RestoresCookies;
var
	Persistence: TCookiePersistence;
	LoadManager: TIdCookieManager;
	CSRFToken: WideString;
begin
	// Prepare cookie file
	FFileSystem.SetFileContent('C:\test.cookies',
		'__csrf__'#9'token123' + sLineBreak +
		'sid'#9'value123'#9'.mail.ru'#9'/'#9'1'#9'0' + sLineBreak);

	LoadManager := TIdCookieManager.Create(nil);
	try
		Persistence := TCookiePersistence.Create('C:\test.cookies', FFileSystem);
		try
			Persistence.Load(LoadManager, CSRFToken);
		finally
			Persistence.Free;
		end;

		Assert.IsTrue(LoadManager.CookieCollection.Count > 0, 'Should load at least one cookie');
	finally
		LoadManager.Free;
	end;
end;

procedure TCookiePersistenceTest.TestLoad_RestoresCSRFToken;
var
	Persistence: TCookiePersistence;
	CSRFToken: WideString;
begin
	FFileSystem.SetFileContent('C:\test.cookies',
		'__csrf__'#9'my_csrf_value' + sLineBreak +
		'sid'#9'val'#9'.mail.ru'#9'/'#9'0'#9'0' + sLineBreak);

	Persistence := TCookiePersistence.Create('C:\test.cookies', FFileSystem);
	try
		Persistence.Load(FCookieManager, CSRFToken);
	finally
		Persistence.Free;
	end;

	Assert.AreEqual('my_csrf_value', CSRFToken);
end;

procedure TCookiePersistenceTest.TestLoad_FileNotFound_ReturnsFalse;
var
	Persistence: TCookiePersistence;
	CSRFToken: WideString;
	LoadResult: Boolean;
begin
	Persistence := TCookiePersistence.Create('C:\nonexistent.cookies', FFileSystem);
	try
		LoadResult := Persistence.Load(FCookieManager, CSRFToken);
	finally
		Persistence.Free;
	end;

	Assert.IsFalse(LoadResult);
end;

procedure TCookiePersistenceTest.TestLoad_EmptyFile_ReturnsFalse;
var
	Persistence: TCookiePersistence;
	CSRFToken: WideString;
	LoadResult: Boolean;
begin
	FFileSystem.SetFileContent('C:\test.cookies', '');

	Persistence := TCookiePersistence.Create('C:\test.cookies', FFileSystem);
	try
		LoadResult := Persistence.Load(FCookieManager, CSRFToken);
	finally
		Persistence.Free;
	end;

	Assert.IsFalse(LoadResult);
end;

procedure TCookiePersistenceTest.TestLoad_EmptyFilePath_ReturnsFalse;
var
	Persistence: TCookiePersistence;
	CSRFToken: WideString;
	LoadResult: Boolean;
begin
	Persistence := TCookiePersistence.Create('', FFileSystem);
	try
		LoadResult := Persistence.Load(FCookieManager, CSRFToken);
	finally
		Persistence.Free;
	end;

	Assert.IsFalse(LoadResult);
end;

procedure TCookiePersistenceTest.TestLoad_NilCookieManager_ReturnsFalse;
var
	Persistence: TCookiePersistence;
	CSRFToken: WideString;
	LoadResult: Boolean;
begin
	FFileSystem.SetFileContent('C:\test.cookies', '__csrf__'#9'token');

	Persistence := TCookiePersistence.Create('C:\test.cookies', FFileSystem);
	try
		LoadResult := Persistence.Load(nil, CSRFToken);
	finally
		Persistence.Free;
	end;

	Assert.IsFalse(LoadResult);
end;

procedure TCookiePersistenceTest.TestLoad_SkipsMalformedLines;
var
	Persistence: TCookiePersistence;
	CSRFToken: WideString;
	LoadResult: Boolean;
begin
	// Malformed lines (too few fields) should be skipped, not cause errors
	FFileSystem.SetFileContent('C:\test.cookies',
		'__csrf__'#9'token' + sLineBreak +
		'only_name' + sLineBreak +         // Too few fields - skipped
		'two'#9'fields' + sLineBreak +      // Still too few - skipped
		'sid'#9'val'#9'.mail.ru'#9'/'#9'0'#9'0' + sLineBreak);

	Persistence := TCookiePersistence.Create('C:\test.cookies', FFileSystem);
	try
		LoadResult := Persistence.Load(FCookieManager, CSRFToken);
	finally
		Persistence.Free;
	end;

	Assert.IsTrue(LoadResult, 'Should succeed despite malformed lines');
	Assert.AreEqual('token', CSRFToken);
end;

procedure TCookiePersistenceTest.TestLoad_NoCSRFToken_ReturnsFalse;
var
	Persistence: TCookiePersistence;
	CSRFToken: WideString;
	LoadResult: Boolean;
begin
	// File with cookies but no CSRF token line
	FFileSystem.SetFileContent('C:\test.cookies',
		'sid'#9'val'#9'.mail.ru'#9'/'#9'0'#9'0' + sLineBreak);

	Persistence := TCookiePersistence.Create('C:\test.cookies', FFileSystem);
	try
		LoadResult := Persistence.Load(FCookieManager, CSRFToken);
	finally
		Persistence.Free;
	end;

	Assert.IsFalse(LoadResult, 'Should fail without CSRF token');
end;

procedure TCookiePersistenceTest.TestSaveLoad_Roundtrip;
var
	SavePersistence, LoadPersistence: TCookiePersistence;
	LoadManager: TIdCookieManager;
	SavedCSRF, LoadedCSRF: WideString;
	SaveResult, LoadResult: Boolean;
begin
	SavedCSRF := 'roundtrip_csrf_token';
	AddTestCookie('session_id', 'sess_value', '.mail.ru', '/', True, False);

	SavePersistence := TCookiePersistence.Create('C:\roundtrip.cookies', FFileSystem);
	try
		SaveResult := SavePersistence.Save(FCookieManager, SavedCSRF);
	finally
		SavePersistence.Free;
	end;
	Assert.IsTrue(SaveResult, 'Save should succeed');

	LoadManager := TIdCookieManager.Create(nil);
	try
		LoadPersistence := TCookiePersistence.Create('C:\roundtrip.cookies', FFileSystem);
		try
			LoadResult := LoadPersistence.Load(LoadManager, LoadedCSRF);
		finally
			LoadPersistence.Free;
		end;

		Assert.IsTrue(LoadResult, 'Load should succeed');
		Assert.AreEqual(SavedCSRF, LoadedCSRF);
		Assert.IsTrue(LoadManager.CookieCollection.Count > 0, 'Should have loaded cookies');
	finally
		LoadManager.Free;
	end;
end;

procedure TCookiePersistenceTest.TestSaveLoad_MultipleCookies;
var
	Persistence: TCookiePersistence;
	LoadManager: TIdCookieManager;
	CSRFToken: WideString;
begin
	AddTestCookie('sid', 'val1', '.mail.ru', '/', True, False);
	AddTestCookie('t', 'val2', '.cloud.mail.ru', '/', False, False);
	AddTestCookie('p', 'val3', '.mail.ru', '/api', True, True);

	Persistence := TCookiePersistence.Create('C:\multi.cookies', FFileSystem);
	try
		Persistence.Save(FCookieManager, 'csrf');
	finally
		Persistence.Free;
	end;

	LoadManager := TIdCookieManager.Create(nil);
	try
		Persistence := TCookiePersistence.Create('C:\multi.cookies', FFileSystem);
		try
			Persistence.Load(LoadManager, CSRFToken);
		finally
			Persistence.Free;
		end;

		// Should load all three cookies
		Assert.IsTrue(LoadManager.CookieCollection.Count >= 3, 'Should have at least 3 cookies');
	finally
		LoadManager.Free;
	end;
end;

procedure TCookiePersistenceTest.TestSaveLoad_SecureAndHttpOnlyFlags;
var
	Persistence: TCookiePersistence;
	Content: WideString;
begin
	AddTestCookie('secure_cookie', 'val', '.mail.ru', '/', True, False);
	AddTestCookie('httponly_cookie', 'val', '.mail.ru', '/', False, True);
	AddTestCookie('both_cookie', 'val', '.mail.ru', '/', True, True);
	AddTestCookie('neither_cookie', 'val', '.mail.ru', '/', False, False);

	Persistence := TCookiePersistence.Create('C:\flags.cookies', FFileSystem);
	try
		Persistence.Save(FCookieManager, 'csrf');
	finally
		Persistence.Free;
	end;

	Content := FFileSystem.ReadAllText('C:\flags.cookies', TEncoding.UTF8);

	// Verify Secure and HttpOnly flags are preserved in file
	Assert.Contains(Content, 'secure_cookie');
	Assert.Contains(Content, 'httponly_cookie');
	Assert.Contains(Content, 'both_cookie');
	Assert.Contains(Content, 'neither_cookie');
end;

procedure TCookiePersistenceTest.TestDelete_RemovesFile;
var
	Persistence: TCookiePersistence;
begin
	FFileSystem.SetFileContent('C:\test.cookies', 'data');

	Persistence := TCookiePersistence.Create('C:\test.cookies', FFileSystem);
	try
		Persistence.Delete;
	finally
		Persistence.Free;
	end;

	Assert.IsFalse(FFileSystem.FileExists('C:\test.cookies'));
end;

procedure TCookiePersistenceTest.TestDelete_NoFile_NoError;
var
	Persistence: TCookiePersistence;
begin
	Persistence := TCookiePersistence.Create('C:\nonexistent.cookies', FFileSystem);
	try
		Persistence.Delete; // Should not raise
	finally
		Persistence.Free;
	end;

	Assert.Pass;
end;

procedure TCookiePersistenceTest.TestExists_FilePresent_ReturnsTrue;
var
	Persistence: TCookiePersistence;
begin
	FFileSystem.SetFileContent('C:\test.cookies', 'data');

	Persistence := TCookiePersistence.Create('C:\test.cookies', FFileSystem);
	try
		Assert.IsTrue(Persistence.Exists);
	finally
		Persistence.Free;
	end;
end;

procedure TCookiePersistenceTest.TestExists_FileAbsent_ReturnsFalse;
var
	Persistence: TCookiePersistence;
begin
	Persistence := TCookiePersistence.Create('C:\nonexistent.cookies', FFileSystem);
	try
		Assert.IsFalse(Persistence.Exists);
	finally
		Persistence.Free;
	end;
end;

procedure TCookiePersistenceTest.TestExists_EmptyFilePath_ReturnsFalse;
var
	Persistence: TCookiePersistence;
begin
	Persistence := TCookiePersistence.Create('', FFileSystem);
	try
		Assert.IsFalse(Persistence.Exists);
	finally
		Persistence.Free;
	end;
end;

{TCookiePersistenceBuildPathTest}

procedure TCookiePersistenceBuildPathTest.TestBuildFilePath_BasicFormat;
var
	Path: WideString;
begin
	Path := TCookiePersistence.BuildFilePath('C:\Config', 'user@mail.ru');

	Assert.AreEqual('C:\Config\cookies\user_at_mail.ru.cookies', Path);
end;

procedure TCookiePersistenceBuildPathTest.TestBuildFilePath_TrailingSlashInConfigDir;
var
	Path: WideString;
begin
	Path := TCookiePersistence.BuildFilePath('C:\Config\', 'user@mail.ru');

	Assert.AreEqual('C:\Config\cookies\user_at_mail.ru.cookies', Path);
end;

procedure TCookiePersistenceBuildPathTest.TestSanitizeAccountName_ReplacesAt;
begin
	Assert.AreEqual('user_at_mail.ru', TCookiePersistence.SanitizeAccountName('user@mail.ru'));
end;

procedure TCookiePersistenceBuildPathTest.TestSanitizeAccountName_ReplacesUnsafeChars;
begin
	// Characters like : * ? " < > | should be replaced with _
	Assert.AreEqual('a_b_c_d', TCookiePersistence.SanitizeAccountName('a:b*c?d'));
end;

procedure TCookiePersistenceBuildPathTest.TestSanitizeAccountName_PreservesSafeChars;
begin
	Assert.AreEqual('user-name_123.test', TCookiePersistence.SanitizeAccountName('user-name_123.test'));
end;

procedure TCookiePersistenceBuildPathTest.TestSanitizeAccountName_MultipleDots;
begin
	Assert.AreEqual('user_at_sub.domain.mail.ru', TCookiePersistence.SanitizeAccountName('user@sub.domain.mail.ru'));
end;

initialization

TDUnitX.RegisterTestFixture(TCookiePersistenceTest);
TDUnitX.RegisterTestFixture(TCookiePersistenceBuildPathTest);

end.
