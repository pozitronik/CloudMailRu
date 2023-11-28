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
	PLUGIN_TYPES in '..\PLUGIN_TYPES.pas',
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
	CMRConstants in '..\CMRConstants.pas',
	ProxySettings in '..\models\settings\ProxySettings.pas',
	StringHelper in '..\helpers\StringHelper.pas',
	TCHelper in '..\helpers\TCHelper.pas',
	Description in '..\models\Description.pas',
	CMRStrings in '..\CMRStrings.pas',
	PluginSettingsTest in 'models\settings\PluginSettingsTest.pas',
	AbstractPluginSettings in '..\models\settings\AbstractPluginSettings.pas',
	PluginSettings in '..\models\settings\PluginSettings.pas';

{keep comment here to protect the following conditional from being removed by the IDE when adding a unit}
{$IFNDEF TESTINSIGHT}

var
	runner: ITestRunner;
	results: IRunResults;
	logger: ITestLogger;
	nunitLogger: ITestLogger;
{$ENDIF}

begin
{$IFDEF TESTINSIGHT}
	TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
	try
		//Check command line options, will exit if invalid
		TDUnitX.CheckCommandLine;
		//Create the test runner
		runner := TDUnitX.CreateRunner;
		//Tell the runner to use RTTI to find Fixtures
		runner.UseRTTI := True;
		//When true, Assertions must be made during tests;
		runner.FailsOnNoAsserts := False;

		//tell the runner how we will log things
		//Log to the console window if desired
		if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
		begin
			logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
			runner.AddLogger(logger);
		end;
		//Generate an NUnit compatible XML File
		nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
		runner.AddLogger(nunitLogger);

		//Run tests
		results := runner.Execute;
		if not results.AllPassed then
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
