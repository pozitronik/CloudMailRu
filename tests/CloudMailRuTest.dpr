program CloudMailRuTest;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}

{ Enable FastMM5 memory leak reporting for tests }
{$DEFINE FastMM_EnableMemoryLeakReporting}

uses
	FastMM5 in 'FastMM5\FastMM5.pas',
	System.SysUtils,
{$IFDEF TESTINSIGHT}
	TestInsight.DUnitX,
{$ELSE}
	DUnitX.Loggers.Console,
	DUnitX.Loggers.Xml.NUnit,
{$ENDIF }
	DUnitX.TestFramework,
	TestHelper in 'TestHelper.pas',
	TCLoggerTest in 'models\TCLoggerTest.pas',
	TCLogger in '..\src\models\tc\TCLogger.pas',
	PLUGIN_TYPES in '..\src\types\PLUGIN_TYPES.pas',
	SystemHelper in '..\src\helpers\SystemHelper.pas',
	TCProgress in '..\src\models\tc\TCProgress.pas',
	TCProgressTest in 'models\TCProgressTest.pas',
	TCRequest in '..\src\models\tc\TCRequest.pas',
	TCRequestTest in 'models\TCRequestTest.pas',
	DebugHelper in '..\src\helpers\DebugHelper.pas',
	FileHelper in '..\src\helpers\FileHelper.pas',
	PathHelper in '..\src\helpers\PathHelper.pas',
	WindowsHelper in '..\src\helpers\WindowsHelper.pas',
	ConnectionSettings in '..\src\models\settings\ConnectionSettings.pas',
	SETTINGS_CONSTANTS in '..\src\models\settings\SETTINGS_CONSTANTS.pas',
	CMRConstants in '..\src\types\CMRConstants.pas',
	ProxySettings in '..\src\models\settings\ProxySettings.pas',
	StringHelper in '..\src\helpers\StringHelper.pas',
	TCHelper in '..\src\helpers\TCHelper.pas',
	Description in '..\src\models\Description.pas',
	LANGUAGE_STRINGS in '..\src\types\LANGUAGE_STRINGS.pas',
	PluginSettingsManagerTest in 'models\settings\PluginSettingsManagerTest.pas',
	AccountsManagerTest in 'models\settings\AccountsManagerTest.pas',
	ParsingHelper in '..\src\helpers\ParsingHelper.pas',
	WSList in '..\src\models\WSList.pas',
	AccountSettings in '..\src\models\settings\AccountSettings.pas',
	AccountsManager in '..\src\models\settings\AccountsManager.pas',
	FileCipher in '..\src\models\cipher\FileCipher.pas',
	CMRDirItemList in '..\src\models\dto\CMRDirItemList.pas',
	CMRDirItem in '..\src\models\dto\CMRDirItem.pas',
	JSONHelper in '..\src\helpers\JSONHelper.pas',
	CipherInterface in '..\src\models\cipher\CipherInterface.pas',
	DCPblockciphers in '..\src\libs\DCPCrypt\DCPblockciphers.pas',
	DCPcrypt2 in '..\src\libs\DCPCrypt\DCPcrypt2.pas',
	DCPtypes in '..\src\libs\DCPCrypt\DCPtypes.pas',
	DCPrijndael in '..\src\libs\DCPCrypt\Ciphers\DCPrijndael.pas',
	DCPconst in '..\src\libs\DCPCrypt\DCPconst.pas',
	DCPbase64 in '..\src\libs\DCPCrypt\DCPbase64.pas',
	DCPsha1 in '..\src\libs\DCPCrypt\Hashes\DCPsha1.pas',
	PluginSettingsManager in '..\src\models\settings\PluginSettingsManager.pas',
	PluginSettings in '..\src\models\settings\PluginSettings.pas',
	IniFilesHelper in '..\src\helpers\IniFilesHelper.pas',
	StreamingSettings in '..\src\models\settings\StreamingSettings.pas',
	TestFileHelper in 'helpers\TestFileHelper.pas',
	TestPathHelper in 'helpers\TestPathHelper.pas',
	StringHelperTest in 'helpers\StringHelperTest.pas',
	ParsingHelperTest in 'helpers\ParsingHelperTest.pas',
	RealPathTest in 'models\dto\RealPathTest.pas',
	HashInfo in '..\src\models\HashInfo.pas',
	HashInfoTest in 'models\HashInfoTest.pas',
	CMRDirItemTest in 'models\dto\CMRDirItemTest.pas',
	CMROperationResultTest in 'models\dto\CMROperationResultTest.pas',
	CMRSpaceTest in 'models\dto\CMRSpaceTest.pas',
	WSListTest in 'models\WSListTest.pas',
	JSONHelperTest in 'helpers\JSONHelperTest.pas',
	IniFilesHelperTest in 'helpers\IniFilesHelperTest.pas',
	PluginHelperTest in 'helpers\PluginHelperTest.pas',
	WindowsHelperTest in 'helpers\WindowsHelperTest.pas',
	CMRDirItemListTest in 'models\dto\CMRDirItemListTest.pas',
	CMROAuthTest in 'models\dto\CMROAuthTest.pas',
	CMRIncomingInviteTest in 'models\dto\CMRIncomingInviteTest.pas',
	CMRInviteListTest in 'models\dto\CMRInviteListTest.pas',
	CMRIncomingInviteListTest in 'models\dto\CMRIncomingInviteListTest.pas',
	CMRTwostepTest in 'models\dto\CMRTwostepTest.pas',
	CMRFileIdentityTest in 'models\dto\CMRFileIdentityTest.pas',
	AccountSettingsTest in 'models\settings\AccountSettingsTest.pas',
	PluginSettingsTest in 'models\settings\PluginSettingsTest.pas',
	FileCipherTest in 'models\cipher\FileCipherTest.pas',
	CloudMailRuResourceTest in 'models\CloudMailRuResourceTest.pas',
	CloudMailRuStaticTest in 'models\CloudMailRuStaticTest.pas',
	CloudMailRuInstanceTest in 'models\CloudMailRuInstanceTest.pas',
	ChunkedFileStreamTest in 'models\ChunkedFileStreamTest.pas',
	DescriptionTest in 'models\DescriptionTest.pas',
	FileSplitInfoTest in 'models\FileSplitInfoTest.pas',
	TCHelperTest in 'helpers\TCHelperTest.pas',
	IconHelper in '..\src\helpers\IconHelper.pas',
	IconHelperTest in 'helpers\IconHelperTest.pas',
	CloudMailRu in '..\src\models\CloudMailRu.pas',
	CMRFileIdentity in '..\src\models\dto\CMRFileIdentity.pas',
	CMRIncomingInviteList in '..\src\models\dto\CMRIncomingInviteList.pas',
	CMRInviteList in '..\src\models\dto\CMRInviteList.pas',
	CMROAuth in '..\src\models\dto\CMROAuth.pas',
	CMROperationResult in '..\src\models\dto\CMROperationResult.pas',
	CMRSpace in '..\src\models\dto\CMRSpace.pas',
	CMRTwostep in '..\src\models\dto\CMRTwostep.pas',
	CloudMailRuHTTP in '..\src\models\http\CloudMailRuHTTP.pas',
	CMRInvite in '..\src\models\dto\CMRInvite.pas',
	CMRIncomingInvite in '..\src\models\dto\CMRIncomingInvite.pas',
	CMROwner in '..\src\models\dto\CMROwner.pas',
	ChunkedFileStream in '..\src\models\ChunkedFileStream.pas',
	FileSplitInfo in '..\src\models\FileSplitInfo.pas',
	PluginHelper in '..\src\helpers\PluginHelper.pas',
	HTTPManager in '..\src\models\http\HTTPManager.pas',
	RealPath in '..\src\models\dto\RealPath.pas',
	CloudSettings in '..\src\models\settings\CloudSettings.pas',
	AskPassword in '..\src\forms\AskPassword.pas'{AskPasswordForm};

{keep comment here to protect the following conditional from being removed by the IDE when adding a unit}
{$IFNDEF TESTINSIGHT}

var
	Runner: ITestRunner;
	Results: IRunResults;
	Logger: ITestLogger;
	NunitLogger: ITestLogger;
{$ENDIF}

begin
{$IFDEF TESTINSIGHT}
	TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
	try
		//Check command line options, will exit if invalid
		TDUnitX.CheckCommandLine;
		//Create the test runner
		Runner := TDUnitX.CreateRunner;
		//Tell the runner to use RTTI to find Fixtures
		Runner.UseRTTI := True;
		//When true, Assertions must be made during tests;
		Runner.FailsOnNoAsserts := False;

		//tell the runner how we will log things
		//Log to the console window if desired
		if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
		begin
			Logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
			Runner.AddLogger(Logger);
		end;
		//Generate an NUnit compatible XML File
		NunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
		Runner.AddLogger(NunitLogger);

		//Run tests
		Results := Runner.Execute;
		if not Results.AllPassed then
			System.ExitCode := EXIT_ERRORS;

{$IFNDEF CI}
		//We don't want this happening when running under CI.
		if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
		begin
			System.Write('Done.. press <Enter> key to quit.');
			System.Readln;
		end;
{$ENDIF}
	except
		on E: Exception do
			System.Writeln(E.ClassName, ': ', E.Message);
	end;
{$ENDIF}

end.
