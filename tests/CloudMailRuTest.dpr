program CloudMailRuTest;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}

uses
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
	TCLogger in '..\models\TCLogger.pas',
	PLUGIN_TYPES in '..\types\PLUGIN_TYPES.pas',
	SystemHelper in '..\helpers\SystemHelper.pas',
	TCProgress in '..\models\TCProgress.pas',
	TCProgressTest in 'models\TCProgressTest.pas',
	TCRequest in '..\models\TCRequest.pas',
	TCRequestTest in 'models\TCRequestTest.pas',
	FileHelper in '..\helpers\FileHelper.pas',
	PathHelper in '..\helpers\PathHelper.pas',
	WindowsHelper in '..\helpers\WindowsHelper.pas',
	ConnectionSettings in '..\models\settings\ConnectionSettings.pas',
	SETTINGS_CONSTANTS in '..\models\settings\SETTINGS_CONSTANTS.pas',
	CMRConstants in '..\types\CMRConstants.pas',
	ProxySettings in '..\models\settings\ProxySettings.pas',
	StringHelper in '..\helpers\StringHelper.pas',
	TCHelper in '..\helpers\TCHelper.pas',
	Description in '..\models\Description.pas',
	LANGUAGE_STRINGS in '..\types\LANGUAGE_STRINGS.pas',
	PluginSettingsManagerTest in 'models\settings\PluginSettingsManagerTest.pas',
	AccountsManagerTest in 'models\settings\AccountsManagerTest.pas',
	ParsingHelper in '..\helpers\ParsingHelper.pas',
	WSList in '..\models\WSList.pas',
	AccountSettings in '..\models\settings\AccountSettings.pas',
	AccountsManager in '..\models\settings\AccountsManager.pas',
	FileCipher in '..\models\cipher\FileCipher.pas',
	CMRDirItemList in '..\models\dto\CMRDirItemList.pas',
	CMRDirItem in '..\models\dto\CMRDirItem.pas',
	JSONHelper in '..\helpers\JSONHelper.pas',
	DCPblockciphers in '..\DCPCrypt\DCPblockciphers.pas',
	DCPcrypt2 in '..\DCPCrypt\DCPcrypt2.pas',
	DCPtypes in '..\DCPCrypt\DCPtypes.pas',
	DCPrijndael in '..\DCPCrypt\Ciphers\DCPrijndael.pas',
	DCPconst in '..\DCPCrypt\DCPconst.pas',
	DCPbase64 in '..\DCPCrypt\DCPbase64.pas',
	DCPsha1 in '..\DCPCrypt\Hashes\DCPsha1.pas',
	PluginSettingsManager in '..\models\settings\PluginSettingsManager.pas',
	PluginSettings in '..\models\settings\PluginSettings.pas',
	IniFilesHelper in '..\helpers\IniFilesHelper.pas',
	StreamingSettings in '..\models\settings\StreamingSettings.pas',
	TestFileHelper in 'helpers\TestFileHelper.pas';

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
