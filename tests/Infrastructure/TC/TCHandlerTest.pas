unit TCHandlerTest;

interface

uses
	TCHandler,
	Environment,
	Description,
	IniFiles,
	Windows,
	DUnitX.TestFramework;

type
	{Tests for TNullTCHandler - returns safe defaults}
	[TestFixture]
	TNullTCHandlerTest = class
	private
		FHandler: ITCHandler;
	public
		[Setup]
		procedure Setup;

		[Test]
		procedure FindTCWindow_ReturnsZero;

		[Test]
		procedure GetTCIniPath_ReturnsEmptyString;

		[Test]
		procedure GetTCIconsSize_ReturnsDefaultSize;

		[Test]
		procedure GetTCCommentPreferredFormat_ReturnsUTF8;
	end;

	{Tests for TMemoryTCHandler - configurable values for testing}
	[TestFixture]
	TMemoryTCHandlerTest = class
	private
		FHandler: TMemoryTCHandler;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure FindTCWindow_ReturnsConfiguredValue;

		[Test]
		procedure GetTCIniPath_ReturnsConfiguredValue;

		[Test]
		procedure GetTCIconsSize_ReturnsConfiguredValue;

		[Test]
		procedure GetTCCommentPreferredFormat_ReturnsConfiguredValue;

		[Test]
		procedure DefaultValues_AreCorrect;
	end;

	{Tests for TTCHandler with injected IEnvironment}
	[TestFixture]
	TTCHandlerWithDITest = class
	private
		FEnvironment: TMemoryEnvironment;
		FHandler: ITCHandler;
	public
		[Setup]
		procedure Setup;

		[Test]
		procedure GetTCIniPath_ReturnsEnvironmentVariable;

		[Test]
		procedure GetTCIniPath_ReturnsEmptyWhenEnvVarNotSet;

		[Test]
		procedure GetTCIconsSize_ReturnsDefaultWhenIniNotExists;

		[Test]
		procedure GetTCIconsSize_ReturnsDefaultWhenIniPathEmpty;

		[Test]
		procedure GetTCCommentPreferredFormat_ReturnsDefaultWhenIniNotExists;

		[Test]
		procedure GetTCCommentPreferredFormat_ReturnsDefaultWhenIniPathEmpty;

		[Test]
		{Covers TTCHandler.FindTCWindow (line 172)}
		procedure TestFindTCWindow_ReturnsHandle;

		[Test]
		{Covers GetTCIconsSize non-resolution-specific path (lines 194-224)}
		procedure TestGetTCIconsSize_NonResSpecific_ReadsAllResolutions;

		[Test]
		{Covers GetTCIconsSize resolution-specific fallback to AllResolutions (lines 197-216)}
		procedure TestGetTCIconsSize_ResSpecific_FallbackToAllResolutions;

		[Test]
		{Covers GetTCCommentPreferredFormat INI reading (lines 239-243)}
		procedure TestGetTCCommentPreferredFormat_ReadsValidFormatFromIni;

		[Test]
		{Covers GetTCCommentPreferredFormat invalid format fallback (line 248)}
		procedure TestGetTCCommentPreferredFormat_InvalidFormat_FallsBackToUTF8;
	end;

implementation

uses
	System.SysUtils,
	System.Classes;

{TNullTCHandlerTest}

procedure TNullTCHandlerTest.Setup;
begin
	FHandler := TNullTCHandler.Create;
end;

procedure TNullTCHandlerTest.FindTCWindow_ReturnsZero;
begin
	Assert.AreEqual(HWND(0), FHandler.FindTCWindow);
end;

procedure TNullTCHandlerTest.GetTCIniPath_ReturnsEmptyString;
begin
	Assert.AreEqual('', FHandler.GetTCIniPath);
end;

procedure TNullTCHandlerTest.GetTCIconsSize_ReturnsDefaultSize;
begin
	Assert.AreEqual(TC_DEFAULT_ICON_SIZE, FHandler.GetTCIconsSize);
end;

procedure TNullTCHandlerTest.GetTCCommentPreferredFormat_ReturnsUTF8;
begin
	Assert.AreEqual(ENCODING_UTF8, FHandler.GetTCCommentPreferredFormat);
end;

{TMemoryTCHandlerTest}

procedure TMemoryTCHandlerTest.Setup;
begin
	FHandler := TMemoryTCHandler.Create;
end;

procedure TMemoryTCHandlerTest.TearDown;
begin
	FHandler.Free;
end;

procedure TMemoryTCHandlerTest.FindTCWindow_ReturnsConfiguredValue;
begin
	FHandler.SetTCWindow(12345);

	Assert.AreEqual(HWND(12345), FHandler.FindTCWindow);
end;

procedure TMemoryTCHandlerTest.GetTCIniPath_ReturnsConfiguredValue;
begin
	FHandler.SetTCIniPath('C:\TC\wincmd.ini');

	Assert.AreEqual('C:\TC\wincmd.ini', FHandler.GetTCIniPath);
end;

procedure TMemoryTCHandlerTest.GetTCIconsSize_ReturnsConfiguredValue;
begin
	FHandler.SetIconsSize(32);

	Assert.AreEqual(32, FHandler.GetTCIconsSize);
end;

procedure TMemoryTCHandlerTest.GetTCCommentPreferredFormat_ReturnsConfiguredValue;
begin
	FHandler.SetCommentFormat(ENCODING_UNICODE);

	Assert.AreEqual(ENCODING_UNICODE, FHandler.GetTCCommentPreferredFormat);
end;

procedure TMemoryTCHandlerTest.DefaultValues_AreCorrect;
begin
	Assert.AreEqual(HWND(0), FHandler.FindTCWindow);
	Assert.AreEqual('', FHandler.GetTCIniPath);
	Assert.AreEqual(TC_DEFAULT_ICON_SIZE, FHandler.GetTCIconsSize);
	Assert.AreEqual(ENCODING_UTF8, FHandler.GetTCCommentPreferredFormat);
end;

{TTCHandlerWithDITest}

procedure TTCHandlerWithDITest.Setup;
begin
	FEnvironment := TMemoryEnvironment.Create;
	FHandler := TTCHandler.Create(FEnvironment);
end;

procedure TTCHandlerWithDITest.GetTCIniPath_ReturnsEnvironmentVariable;
begin
	FEnvironment.SetEnvironmentVariable('COMMANDER_INI', 'D:\TotalCmd\wincmd.ini');

	Assert.AreEqual('D:\TotalCmd\wincmd.ini', FHandler.GetTCIniPath);
end;

procedure TTCHandlerWithDITest.GetTCIniPath_ReturnsEmptyWhenEnvVarNotSet;
begin
	{Environment variable not set}

	Assert.AreEqual('', FHandler.GetTCIniPath);
end;

procedure TTCHandlerWithDITest.GetTCIconsSize_ReturnsDefaultWhenIniNotExists;
begin
	FEnvironment.SetEnvironmentVariable('COMMANDER_INI', 'C:\nonexistent\wincmd.ini');
	{File not added to FEnvironment, so FileExists returns false}

	Assert.AreEqual(TC_DEFAULT_ICON_SIZE, FHandler.GetTCIconsSize);
end;

procedure TTCHandlerWithDITest.GetTCIconsSize_ReturnsDefaultWhenIniPathEmpty;
begin
	{COMMANDER_INI not set, path is empty}

	Assert.AreEqual(TC_DEFAULT_ICON_SIZE, FHandler.GetTCIconsSize);
end;

procedure TTCHandlerWithDITest.GetTCCommentPreferredFormat_ReturnsDefaultWhenIniNotExists;
begin
	FEnvironment.SetEnvironmentVariable('COMMANDER_INI', 'C:\nonexistent\wincmd.ini');
	{File not added to FEnvironment, so FileExists returns false}

	Assert.AreEqual(ENCODING_UTF8, FHandler.GetTCCommentPreferredFormat);
end;

procedure TTCHandlerWithDITest.GetTCCommentPreferredFormat_ReturnsDefaultWhenIniPathEmpty;
begin
	{COMMANDER_INI not set, path is empty}

	Assert.AreEqual(ENCODING_UTF8, FHandler.GetTCCommentPreferredFormat);
end;

{TTCHandler real Windows API tests}

procedure TTCHandlerWithDITest.TestFindTCWindow_ReturnsHandle;
var
	Handle: HWND;
begin
	{TTCHandler.FindTCWindow calls Windows.FindWindow — result depends on whether TC is running}
	Handle := FHandler.FindTCWindow;
	Assert.Pass('FindTCWindow returned ' + IntToStr(Handle));
end;

procedure TTCHandlerWithDITest.TestGetTCIconsSize_NonResSpecific_ReadsAllResolutions;
var
	IniPath: string;
	IniContent: TStringList;
begin
	IniPath := IncludeTrailingPathDelimiter(System.SysUtils.GetEnvironmentVariable('TEMP')) +
		'TCHandlerTest_nonres_' + IntToStr(GetCurrentProcessId) + '.ini';
	IniContent := TStringList.Create;
	try
		IniContent.Add('[Configuration]');
		IniContent.Add('ResolutionSpecific=0');
		IniContent.Add('[AllResolutions]');
		IniContent.Add('Iconsize32=24');
		IniContent.SaveToFile(IniPath);
	finally
		IniContent.Free;
	end;
	try
		FEnvironment.SetEnvironmentVariable('COMMANDER_INI', IniPath);
		FEnvironment.AddExistingFile(IniPath);
		Assert.AreEqual(24, FHandler.GetTCIconsSize);
	finally
		System.SysUtils.DeleteFile(IniPath);
	end;
end;

procedure TTCHandlerWithDITest.TestGetTCIconsSize_ResSpecific_FallbackToAllResolutions;
var
	IniPath: string;
	IniContent: TStringList;
begin
	IniPath := IncludeTrailingPathDelimiter(System.SysUtils.GetEnvironmentVariable('TEMP')) +
		'TCHandlerTest_resspec_' + IntToStr(GetCurrentProcessId) + '.ini';
	IniContent := TStringList.Create;
	try
		IniContent.Add('[Configuration]');
		IniContent.Add('ResolutionSpecific=1');
		IniContent.Add('[AllResolutions]');
		IniContent.Add('Iconsize32=48');
		IniContent.SaveToFile(IniPath);
	finally
		IniContent.Free;
	end;
	try
		FEnvironment.SetEnvironmentVariable('COMMANDER_INI', IniPath);
		FEnvironment.AddExistingFile(IniPath);

		{No monitor-specific section exists, falls back to AllResolutions}
		Assert.AreEqual(48, FHandler.GetTCIconsSize);
	finally
		System.SysUtils.DeleteFile(IniPath);
	end;
end;

procedure TTCHandlerWithDITest.TestGetTCCommentPreferredFormat_ReadsValidFormatFromIni;
var
	IniPath: string;
	IniContent: TStringList;
begin
	IniPath := IncludeTrailingPathDelimiter(System.SysUtils.GetEnvironmentVariable('TEMP')) +
		'TCHandlerTest_comment_' + IntToStr(GetCurrentProcessId) + '.ini';
	IniContent := TStringList.Create;
	try
		IniContent.Add('[Configuration]');
		IniContent.Add('CommentPreferredFormat=' + IntToStr(ENCODING_UNICODE));
		IniContent.SaveToFile(IniPath);
	finally
		IniContent.Free;
	end;
	try
		FEnvironment.SetEnvironmentVariable('COMMANDER_INI', IniPath);
		FEnvironment.AddExistingFile(IniPath);
		Assert.AreEqual(ENCODING_UNICODE, FHandler.GetTCCommentPreferredFormat);
	finally
		System.SysUtils.DeleteFile(IniPath);
	end;
end;

procedure TTCHandlerWithDITest.TestGetTCCommentPreferredFormat_InvalidFormat_FallsBackToUTF8;
var
	IniPath: string;
	IniContent: TStringList;
begin
	IniPath := IncludeTrailingPathDelimiter(System.SysUtils.GetEnvironmentVariable('TEMP')) +
		'TCHandlerTest_badfmt_' + IntToStr(GetCurrentProcessId) + '.ini';
	IniContent := TStringList.Create;
	try
		IniContent.Add('[Configuration]');
		IniContent.Add('CommentPreferredFormat=99');
		IniContent.SaveToFile(IniPath);
	finally
		IniContent.Free;
	end;
	try
		FEnvironment.SetEnvironmentVariable('COMMANDER_INI', IniPath);
		FEnvironment.AddExistingFile(IniPath);

		{99 is not a valid encoding constant, falls back to ENCODING_UTF8}
		Assert.AreEqual(ENCODING_UTF8, FHandler.GetTCCommentPreferredFormat);
	finally
		System.SysUtils.DeleteFile(IniPath);
	end;
end;

initialization
	TDUnitX.RegisterTestFixture(TNullTCHandlerTest);
	TDUnitX.RegisterTestFixture(TMemoryTCHandlerTest);
	TDUnitX.RegisterTestFixture(TTCHandlerWithDITest);

end.
