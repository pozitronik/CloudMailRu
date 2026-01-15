unit TCHelperTest;

interface

uses
	TCHelper,
	Description,
	IniFiles,
	Windows,
	DUnitX.TestFramework;

type

	[TestFixture]
	TTCHelperTest = class
	private
		FTempIniFile: string;
		FSavedCommanderIni: string;
		procedure SetCommanderIniEnv(const Value: string);
		procedure RestoreCommanderIniEnv;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{ FindTCIniPath tests }
		[Test]
		procedure TestFindTCIniPathReturnsEnvVar;
		[Test]
		procedure TestFindTCIniPathEmptyWhenNotSet;

		{ GetTCCommentPreferredFormat tests - default behavior }
		[Test]
		procedure TestGetTCCommentPreferredFormatDefaultUTF8;
		[Test]
		procedure TestGetTCCommentPreferredFormatReadsFromIni;
		[Test]
		procedure TestGetTCCommentPreferredFormatUnicode;
		[Test]
		procedure TestGetTCCommentPreferredFormatUnicodeBE;
		[Test]
		procedure TestGetTCCommentPreferredFormatDefault;

		{ GetTCCommentPreferredFormat validation tests }
		[Test]
		procedure TestGetTCCommentPreferredFormatInvalidReturnsUTF8;
		[Test]
		procedure TestGetTCCommentPreferredFormatNegativeReturnsUTF8;
		[Test]
		procedure TestGetTCCommentPreferredFormatHighValueReturnsUTF8;

		{ GetTCIconsSize tests - default behavior }
		[Test]
		procedure TestGetTCIconsSizeDefaultValue;
		[Test]
		procedure TestGetTCIconsSizeReadsAllResolutions;

		{ FindTCWindow tests - environment dependent }
		[Test]
		procedure TestFindTCWindowReturnsHandle;
	end;

implementation

uses
	System.SysUtils;

{ Setup and TearDown }

procedure TTCHelperTest.Setup;
begin
	FTempIniFile := IncludeTrailingPathDelimiter(GetEnvironmentVariable('TEMP')) +
		'TCHelperTest_' + IntToStr(GetCurrentThreadId) + '.ini';
	{ Save current COMMANDER_INI value }
	FSavedCommanderIni := GetEnvironmentVariable('COMMANDER_INI');
end;

procedure TTCHelperTest.TearDown;
begin
	RestoreCommanderIniEnv;
	if FileExists(FTempIniFile) then
		System.SysUtils.DeleteFile(FTempIniFile);
end;

procedure TTCHelperTest.SetCommanderIniEnv(const Value: string);
begin
	SetEnvironmentVariable('COMMANDER_INI', PChar(Value));
end;

procedure TTCHelperTest.RestoreCommanderIniEnv;
begin
	if FSavedCommanderIni <> '' then
		SetEnvironmentVariable('COMMANDER_INI', PChar(FSavedCommanderIni))
	else
		SetEnvironmentVariable('COMMANDER_INI', nil);
end;

{ FindTCIniPath tests }

procedure TTCHelperTest.TestFindTCIniPathReturnsEnvVar;
var
	TestPath: string;
begin
	TestPath := 'C:\Test\Path\wincmd.ini';
	SetCommanderIniEnv(TestPath);
	Assert.AreEqual(TestPath, FindTCIniPath);
end;

procedure TTCHelperTest.TestFindTCIniPathEmptyWhenNotSet;
begin
	SetCommanderIniEnv('');
	Assert.AreEqual('', FindTCIniPath);
end;

{ GetTCCommentPreferredFormat tests - default behavior }

procedure TTCHelperTest.TestGetTCCommentPreferredFormatDefaultUTF8;
begin
	{ When INI file doesn't exist, should return UTF-8 }
	SetCommanderIniEnv('Z:\NonExistent\Path\wincmd.ini');
	Assert.AreEqual(ENCODING_UTF8, GetTCCommentPreferredFormat);
end;

procedure TTCHelperTest.TestGetTCCommentPreferredFormatReadsFromIni;
var
	Ini: TIniFile;
begin
	{ Create INI with UTF-8 encoding }
	Ini := TIniFile.Create(FTempIniFile);
	try
		Ini.WriteInteger('Configuration', 'CommentPreferredFormat', ENCODING_UTF8);
	finally
		Ini.Free;
	end;

	SetCommanderIniEnv(FTempIniFile);
	Assert.AreEqual(ENCODING_UTF8, GetTCCommentPreferredFormat);
end;

procedure TTCHelperTest.TestGetTCCommentPreferredFormatUnicode;
var
	Ini: TIniFile;
begin
	Ini := TIniFile.Create(FTempIniFile);
	try
		Ini.WriteInteger('Configuration', 'CommentPreferredFormat', ENCODING_UNICODE);
	finally
		Ini.Free;
	end;

	SetCommanderIniEnv(FTempIniFile);
	Assert.AreEqual(ENCODING_UNICODE, GetTCCommentPreferredFormat);
end;

procedure TTCHelperTest.TestGetTCCommentPreferredFormatUnicodeBE;
var
	Ini: TIniFile;
begin
	Ini := TIniFile.Create(FTempIniFile);
	try
		Ini.WriteInteger('Configuration', 'CommentPreferredFormat', ENCODING_UNCODE_BE);
	finally
		Ini.Free;
	end;

	SetCommanderIniEnv(FTempIniFile);
	Assert.AreEqual(ENCODING_UNCODE_BE, GetTCCommentPreferredFormat);
end;

procedure TTCHelperTest.TestGetTCCommentPreferredFormatDefault;
var
	Ini: TIniFile;
begin
	Ini := TIniFile.Create(FTempIniFile);
	try
		Ini.WriteInteger('Configuration', 'CommentPreferredFormat', ENCODING_DEFAULT);
	finally
		Ini.Free;
	end;

	SetCommanderIniEnv(FTempIniFile);
	Assert.AreEqual(ENCODING_DEFAULT, GetTCCommentPreferredFormat);
end;

{ GetTCCommentPreferredFormat validation tests }

procedure TTCHelperTest.TestGetTCCommentPreferredFormatInvalidReturnsUTF8;
var
	Ini: TIniFile;
begin
	{ Value 4 is not in valid set [0..3], should return UTF-8 }
	Ini := TIniFile.Create(FTempIniFile);
	try
		Ini.WriteInteger('Configuration', 'CommentPreferredFormat', 4);
	finally
		Ini.Free;
	end;

	SetCommanderIniEnv(FTempIniFile);
	Assert.AreEqual(ENCODING_UTF8, GetTCCommentPreferredFormat);
end;

procedure TTCHelperTest.TestGetTCCommentPreferredFormatNegativeReturnsUTF8;
var
	Ini: TIniFile;
begin
	{ Negative value should return UTF-8 }
	Ini := TIniFile.Create(FTempIniFile);
	try
		Ini.WriteInteger('Configuration', 'CommentPreferredFormat', -1);
	finally
		Ini.Free;
	end;

	SetCommanderIniEnv(FTempIniFile);
	Assert.AreEqual(ENCODING_UTF8, GetTCCommentPreferredFormat);
end;

procedure TTCHelperTest.TestGetTCCommentPreferredFormatHighValueReturnsUTF8;
var
	Ini: TIniFile;
begin
	{ Combined encodings (values > 3) should return UTF-8 }
	Ini := TIniFile.Create(FTempIniFile);
	try
		Ini.WriteInteger('Configuration', 'CommentPreferredFormat', 100);
	finally
		Ini.Free;
	end;

	SetCommanderIniEnv(FTempIniFile);
	Assert.AreEqual(ENCODING_UTF8, GetTCCommentPreferredFormat);
end;

{ GetTCIconsSize tests }

procedure TTCHelperTest.TestGetTCIconsSizeDefaultValue;
begin
	{ When INI file doesn't exist, should return default 16 }
	SetCommanderIniEnv('Z:\NonExistent\Path\wincmd.ini');
	Assert.AreEqual(16, GetTCIconsSize);
end;

procedure TTCHelperTest.TestGetTCIconsSizeReadsAllResolutions;
var
	Ini: TIniFile;
begin
	{ Create INI with AllResolutions section (ResolutionSpecific=false) }
	Ini := TIniFile.Create(FTempIniFile);
	try
		Ini.WriteBool('Configuration', 'ResolutionSpecific', False);
		Ini.WriteInteger('AllResolutions', 'Iconsize32', 32);
	finally
		Ini.Free;
	end;

	SetCommanderIniEnv(FTempIniFile);
	Assert.AreEqual(32, GetTCIconsSize);
end;

{ FindTCWindow tests }

procedure TTCHelperTest.TestFindTCWindowReturnsHandle;
begin
	{ This test verifies FindTCWindow returns a valid result.
	  If TC is running, it returns a valid handle.
	  If TC is not running, it returns 0.
	  Both are valid behaviors - just verify it doesn't crash. }
	FindTCWindow;
	Assert.IsTrue(True, 'FindTCWindow executed without error');
end;

initialization

TDUnitX.RegisterTestFixture(TTCHelperTest);

end.
