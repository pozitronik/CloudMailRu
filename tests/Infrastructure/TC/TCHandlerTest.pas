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
	end;

implementation

uses
	System.SysUtils;

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

initialization
	TDUnitX.RegisterTestFixture(TNullTCHandlerTest);
	TDUnitX.RegisterTestFixture(TMemoryTCHandlerTest);
	TDUnitX.RegisterTestFixture(TTCHandlerWithDITest);

end.
