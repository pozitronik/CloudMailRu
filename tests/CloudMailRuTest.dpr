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
	ILoggerInterface in '..\src\models\logger\ILoggerInterface.pas',
	TCLogger in '..\src\models\logger\TCLogger.pas',
	TCLoggerTest in 'models\logger\TCLoggerTest.pas',
	NullLoggerTest in 'models\logger\NullLoggerTest.pas',
	PLUGIN_TYPES in '..\src\types\PLUGIN_TYPES.pas',
	SystemHelper in '..\src\helpers\SystemHelper.pas',
	IProgressInterface in '..\src\models\progress\IProgressInterface.pas',
	TCProgress in '..\src\models\progress\TCProgress.pas',
	TCProgressTest in 'models\progress\TCProgressTest.pas',
	NullProgressTest in 'models\progress\NullProgressTest.pas',
	IRequestInterface in '..\src\models\request\IRequestInterface.pas',
	TCRequest in '..\src\models\request\TCRequest.pas',
	TCRequestTest in 'models\request\TCRequestTest.pas',
	NullRequestTest in 'models\request\NullRequestTest.pas',
	IPasswordManagerInterface in '..\src\models\password\IPasswordManagerInterface.pas',
	TCPasswordManager in '..\src\models\password\TCPasswordManager.pas',
	TCPasswordManagerTest in 'models\password\TCPasswordManagerTest.pas',
	NullPasswordManagerTest in 'models\password\NullPasswordManagerTest.pas',
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
	TokenRetryHelper in '..\src\helpers\TokenRetryHelper.pas',
	TokenRetryHelperTest in 'helpers\TokenRetryHelperTest.pas',
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
	NullCipherTest in 'models\cipher\NullCipherTest.pas',
	CloudMailRuResourceTest in 'models\CloudMailRuResourceTest.pas',
	CloudMailRuStaticTest in 'models\CloudMailRuStaticTest.pas',
	CloudMailRuInstanceTest in 'models\CloudMailRuInstanceTest.pas',
	CloudMailRuHashTest in 'models\CloudMailRuHashTest.pas',
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
	ICloudHTTPInterface in '..\src\models\http\ICloudHTTPInterface.pas',
	CMRInvite in '..\src\models\dto\CMRInvite.pas',
	CMRIncomingInvite in '..\src\models\dto\CMRIncomingInvite.pas',
	CMROwner in '..\src\models\dto\CMROwner.pas',
	ChunkedFileStream in '..\src\models\ChunkedFileStream.pas',
	FileSplitInfo in '..\src\models\FileSplitInfo.pas',
	PluginHelper in '..\src\helpers\PluginHelper.pas',
	HTTPManager in '..\src\models\http\HTTPManager.pas',
	RealPath in '..\src\models\dto\RealPath.pas',
	CloudSettings in '..\src\models\settings\CloudSettings.pas',
	IAccountsManagerInterface in '..\src\models\settings\IAccountsManagerInterface.pas',
	IPluginSettingsManagerInterface in '..\src\models\settings\IPluginSettingsManagerInterface.pas',
	NullAccountsManagerTest in 'models\settings\NullAccountsManagerTest.pas',
	NullPluginSettingsManagerTest in 'models\settings\NullPluginSettingsManagerTest.pas',
	CloudSettingsTest in 'models\settings\CloudSettingsTest.pas',
	IPasswordUIProviderInterface in '..\src\models\ui\IPasswordUIProviderInterface.pas',
	PasswordUIProvider in '..\src\models\ui\PasswordUIProvider.pas',
	NullPasswordUIProviderTest in 'models\ui\NullPasswordUIProviderTest.pas',
	IHTTPManagerInterface in '..\src\models\http\IHTTPManagerInterface.pas',
	NullHTTPManagerTest in 'models\http\NullHTTPManagerTest.pas',
	ICipherValidatorInterface in '..\src\models\cipher\ICipherValidatorInterface.pas',
	CipherValidator in '..\src\models\cipher\CipherValidator.pas',
	CipherValidatorTest in 'models\cipher\CipherValidatorTest.pas',
	ConnectionManagerTest in 'models\ConnectionManagerTest.pas',
	ConnectionManager in '..\src\models\ConnectionManager.pas',
	IFileSystemInterface in '..\src\models\filesystem\IFileSystemInterface.pas',
	WindowsFileSystem in '..\src\models\filesystem\WindowsFileSystem.pas',
	IConfigFileInterface in '..\src\models\config\IConfigFileInterface.pas',
	IniConfigFile in '..\src\models\config\IniConfigFile.pas',
	IEnvironmentInterface in '..\src\models\environment\IEnvironmentInterface.pas',
	WindowsEnvironment in '..\src\models\environment\WindowsEnvironment.pas',
	MemoryEnvironmentTest in 'models\environment\MemoryEnvironmentTest.pas',
	AskPassword in '..\src\forms\AskPassword.pas'{AskPasswordForm},
	IAuthStrategyInterface in '..\src\models\auth\IAuthStrategyInterface.pas',
	NullAuthStrategyTest in 'models\auth\NullAuthStrategyTest.pas',
	OAuthAppAuthStrategy in '..\src\models\auth\OAuthAppAuthStrategy.pas',
	OAuthAppAuthStrategyTest in 'models\auth\OAuthAppAuthStrategyTest.pas',
	SharedAccountAuthStrategy in '..\src\models\auth\SharedAccountAuthStrategy.pas',
	SharedAccountAuthStrategyTest in 'models\auth\SharedAccountAuthStrategyTest.pas',
	WebAuthStrategy in '..\src\models\auth\WebAuthStrategy.pas',
	TwoStepAuthStrategy in '..\src\models\auth\TwoStepAuthStrategy.pas',
	OldOAuthStrategy in '..\src\models\auth\OldOAuthStrategy.pas',
	DeprecatedStrategiesTest in 'models\auth\DeprecatedStrategiesTest.pas',
	MockCloudHTTP in 'mocks\MockCloudHTTP.pas',
	MockHTTPManager in 'mocks\MockHTTPManager.pas',
	MockShardHelper in 'mocks\MockShardHelper.pas',
	MockAuthStrategy in 'mocks\MockAuthStrategy.pas',
	MockCloudHTTPStreamTest in 'mocks\MockCloudHTTPStreamTest.pas',
	CloudMailRuDirListingTest in 'models\CloudMailRuDirListingTest.pas',
	CloudMailRuFileOperationsTest in 'models\CloudMailRuFileOperationsTest.pas',
	CloudMailRuPublicAccountTest in 'models\CloudMailRuPublicAccountTest.pas',
	CloudMailRuErrorHandlingTest in 'models\CloudMailRuErrorHandlingTest.pas',
	CloudMailRuUploadDownloadTest in 'models\CloudMailRuUploadDownloadTest.pas',
	CloudMailRuAdditionalOpsTest in 'models\CloudMailRuAdditionalOpsTest.pas',
	CloudMailRuMiscOpsTest in 'models\CloudMailRuMiscOpsTest.pas',
	CloudMailRuCombinedOpsTest in 'models\CloudMailRuCombinedOpsTest.pas',
	CloudMailRuShardTest in 'models\CloudMailRuShardTest.pas',
	CloudMailRuLoginTest in 'models\CloudMailRuLoginTest.pas',
	CloudMailRuDownloadTest in 'models\CloudMailRuDownloadTest.pas',
	CloudMailRuUploadTest in 'models\CloudMailRuUploadTest.pas',
	CloudMailRuIntegrationTest in 'models\CloudMailRuIntegrationTest.pas',
	CloudMailRuLoginFlowTest in 'models\CloudMailRuLoginFlowTest.pas',
	CloudMailRuPutFileSplitTest in 'models\CloudMailRuPutFileSplitTest.pas',
	IThreadStateManagerInterface in '..\src\models\wfx\IThreadStateManagerInterface.pas',
	ThreadStateManager in '..\src\models\wfx\ThreadStateManager.pas',
	ThreadStateManagerTest in 'models\wfx\ThreadStateManagerTest.pas',
	IContentFieldProviderInterface in '..\src\models\wfx\IContentFieldProviderInterface.pas',
	ContentFieldProvider in '..\src\models\wfx\ContentFieldProvider.pas',
	ContentFieldProviderTest in 'models\wfx\ContentFieldProviderTest.pas',
	IIconProviderInterface in '..\src\models\wfx\IIconProviderInterface.pas',
	IconProvider in '..\src\models\wfx\IconProvider.pas',
	IconProviderTest in 'models\wfx\IconProviderTest.pas',
	IOperationLifecycleInterface in '..\src\models\wfx\IOperationLifecycleInterface.pas',
	OperationLifecycleHandler in '..\src\models\wfx\OperationLifecycleHandler.pas',
	OperationLifecycleHandlerTest in 'models\wfx\OperationLifecycleHandlerTest.pas',
	ICloudDescriptionOpsInterface in '..\src\models\wfx\ICloudDescriptionOpsInterface.pas',
	IDescriptionSyncManagerInterface in '..\src\models\wfx\IDescriptionSyncManagerInterface.pas',
	DescriptionSyncManager in '..\src\models\wfx\DescriptionSyncManager.pas',
	CloudDescriptionOpsAdapter in '..\src\models\wfx\CloudDescriptionOpsAdapter.pas',
	MockCloudDescriptionOps in 'mocks\MockCloudDescriptionOps.pas',
	DescriptionSyncManagerTest in 'models\wfx\DescriptionSyncManagerTest.pas',
	IRetryHandlerInterface in '..\src\models\wfx\IRetryHandlerInterface.pas',
	RetryHandler in '..\src\models\wfx\RetryHandler.pas',
	RetryHandlerTest in 'models\wfx\RetryHandlerTest.pas',
	ICommandDispatcherInterface in '..\src\models\wfx\ICommandDispatcherInterface.pas',
	CommandDispatcher in '..\src\models\wfx\CommandDispatcher.pas',
	CommandDispatcherTest in 'models\wfx\CommandDispatcherTest.pas',
	IListingProviderInterface in '..\src\models\wfx\IListingProviderInterface.pas',
	ListingProvider in '..\src\models\wfx\ListingProvider.pas',
	ListingProviderTest in 'models\wfx\ListingProviderTest.pas',
	IDescriptionSyncGuardInterface in '..\src\models\wfx\IDescriptionSyncGuardInterface.pas',
	DescriptionSyncGuard in '..\src\models\wfx\DescriptionSyncGuard.pas',
	DescriptionSyncGuardTest in 'models\wfx\DescriptionSyncGuardTest.pas',
	ILocalFileDeletionHandlerInterface in '..\src\models\wfx\ILocalFileDeletionHandlerInterface.pas',
	LocalFileDeletionHandler in '..\src\models\wfx\LocalFileDeletionHandler.pas',
	LocalFileDeletionHandlerTest in 'models\wfx\LocalFileDeletionHandlerTest.pas';

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
			Logger := TDUnitXConsoleLogger.Create(True); { Quiet mode: dots instead of verbose output }
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
