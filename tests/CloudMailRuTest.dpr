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
	TCLogger in '..\src\Infrastructure\Logger\TCLogger.pas',
	TCLoggerTest in 'Infrastructure\Logger\TCLoggerTest.pas',
	PLUGIN_TYPES in '..\src\types\PLUGIN_TYPES.pas',
	SystemHelper in '..\src\helpers\SystemHelper.pas',
	TCProgress in '..\src\Infrastructure\Progress\TCProgress.pas',
	TCProgressTest in 'Infrastructure\Progress\TCProgressTest.pas',
	TCRequest in '..\src\Infrastructure\Request\TCRequest.pas',
	TCRequestTest in 'Infrastructure\Request\TCRequestTest.pas',
	TCPasswordManager in '..\src\Infrastructure\Password\TCPasswordManager.pas',
	TCPasswordManagerTest in 'Infrastructure\Password\TCPasswordManagerTest.pas',
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
	PasswordUIProvider in '..\src\Infrastructure\UI\PasswordUIProvider.pas',
	PasswordUIProviderTest in 'Infrastructure\UI\PasswordUIProviderTest.pas',
	IHTTPManagerInterface in '..\src\models\http\IHTTPManagerInterface.pas',
	NullHTTPManagerTest in 'models\http\NullHTTPManagerTest.pas',
	CipherValidator in '..\src\Infrastructure\Cipher\CipherValidator.pas',
	CipherValidatorTest in 'Infrastructure\Cipher\CipherValidatorTest.pas',
	ConnectionManagerTest in 'models\ConnectionManagerTest.pas',
	IConnectionManagerInterface in '..\src\models\IConnectionManagerInterface.pas',
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
	MockConnectionManager in 'mocks\MockConnectionManager.pas',
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
	ThreadStateManager in '..\src\Infrastructure\State\ThreadStateManager.pas',
	ThreadStateManagerTest in 'Infrastructure\State\ThreadStateManagerTest.pas',
	ContentFieldProvider in '..\src\Presentation\ContentField\ContentFieldProvider.pas',
	ContentFieldProviderTest in 'Presentation\ContentField\ContentFieldProviderTest.pas',
	IconProvider in '..\src\Presentation\Icon\IconProvider.pas',
	IconProviderTest in 'Presentation\Icon\IconProviderTest.pas',
	OperationLifecycleHandler in '..\src\Application\Operation\OperationLifecycleHandler.pas',
	OperationLifecycleHandlerTest in 'Application\Operation\OperationLifecycleHandlerTest.pas',
	DescriptionSyncManager in '..\src\Application\Description\DescriptionSyncManager.pas',
	CloudDescriptionOpsAdapter in '..\src\Infrastructure\Adapter\CloudDescriptionOpsAdapter.pas',
	MockCloudDescriptionOps in 'mocks\MockCloudDescriptionOps.pas',
	DescriptionSyncManagerTest in 'Application\Description\DescriptionSyncManagerTest.pas',
	RetryHandler in '..\src\Application\Retry\RetryHandler.pas',
	RetryHandlerTest in 'Application\Retry\RetryHandlerTest.pas',
	CommandDispatcher in '..\src\Application\Command\CommandDispatcher.pas',
	CommandDispatcherTest in 'Application\Command\CommandDispatcherTest.pas',
	ListingProvider in '..\src\Application\Listing\ListingProvider.pas',
	ListingProviderTest in 'Application\Listing\ListingProviderTest.pas',
	DescriptionSyncGuard in '..\src\Application\Description\DescriptionSyncGuard.pas',
	DescriptionSyncGuardTest in 'Application\Description\DescriptionSyncGuardTest.pas',
	LocalFileDeletionHandler in '..\src\Application\FileOps\LocalFileDeletionHandler.pas',
	LocalFileDeletionHandlerTest in 'Application\FileOps\LocalFileDeletionHandlerTest.pas',
	DownloadSuccessHandler in '..\src\Application\Download\DownloadSuccessHandler.pas',
	DownloadSuccessHandlerTest in 'Application\Download\DownloadSuccessHandlerTest.pas',
	OperationActionExecutor in '..\src\Application\Operation\OperationActionExecutor.pas',
	OperationActionExecutorTest in 'Application\Operation\OperationActionExecutorTest.pas',
	ListingSkipDecider in '..\src\Application\Listing\ListingSkipDecider.pas',
	ListingSkipDeciderTest in 'Application\Listing\ListingSkipDeciderTest.pas',
	ListingPathValidator in '..\src\Application\Listing\ListingPathValidator.pas',
	ListingPathValidatorTest in 'Application\Listing\ListingPathValidatorTest.pas',
	SameAccountMoveHandler in '..\src\Application\FileOps\SameAccountMoveHandler.pas',
	SameAccountMoveHandlerTest in 'Application\FileOps\SameAccountMoveHandlerTest.pas',
	FileStreamExecutor in '..\src\Application\FileOps\FileStreamExecutor.pas',
	FileStreamExecutorTest in 'Application\FileOps\FileStreamExecutorTest.pas',
	LocalFileConflictResolver in '..\src\Application\Download\LocalFileConflictResolver.pas',
	LocalFileConflictResolverTest in 'Application\Download\LocalFileConflictResolverTest.pas',
	ListingItemFetcher in '..\src\Application\Listing\ListingItemFetcher.pas',
	ListingItemFetcherTest in 'Application\Listing\ListingItemFetcherTest.pas',
	SharedItemDeletionHandler in '..\src\Application\FileOps\SharedItemDeletionHandler.pas',
	SharedItemDeletionHandlerTest in 'Application\FileOps\SharedItemDeletionHandlerTest.pas',
	AccountRegistrationHandler in '..\src\Application\Operations\AccountRegistrationHandler.pas',
	AccountRegistrationHandlerTest in 'Application\Operations\AccountRegistrationHandlerTest.pas',
	TrashBinOperationHandler in '..\src\Application\Operations\TrashBinOperationHandler.pas',
	TrashBinOperationHandlerTest in 'Application\Operations\TrashBinOperationHandlerTest.pas',
	InviteOperationHandler in '..\src\Application\Operations\InviteOperationHandler.pas',
	InviteOperationHandlerTest in 'Application\Operations\InviteOperationHandlerTest.pas',
	CrossAccountFileOperationHandler in '..\src\Application\FileOps\CrossAccountFileOperationHandler.pas',
	CrossAccountFileOperationHandlerTest in 'Application\FileOps\CrossAccountFileOperationHandlerTest.pas',
	IconRenderingEngine in '..\src\Presentation\Icon\IconRenderingEngine.pas',
	IconRenderingEngineTest in 'Presentation\Icon\IconRenderingEngineTest.pas',
	FileExecutionDispatcher in '..\src\Application\Operations\FileExecutionDispatcher.pas',
	FileExecutionDispatcherTest in 'Application\Operations\FileExecutionDispatcherTest.pas',
	SharedItemActionHandler in '..\src\Application\Operations\SharedItemActionHandler.pas',
	SharedItemActionHandlerTest in 'Application\Operations\SharedItemActionHandlerTest.pas',
	MoveOperationContextTracker in '..\src\Application\FileOps\MoveOperationContextTracker.pas',
	MoveOperationContextTrackerTest in 'Application\FileOps\MoveOperationContextTrackerTest.pas',
	DirectoryDeletionPreCheck in '..\src\Application\FileOps\DirectoryDeletionPreCheck.pas',
	DirectoryDeletionPreCheckTest in 'Application\FileOps\DirectoryDeletionPreCheckTest.pas',
	UploadPreparationValidator in '..\src\Application\Upload\UploadPreparationValidator.pas',
	UploadPreparationValidatorTest in 'Application\Upload\UploadPreparationValidatorTest.pas',
	DownloadPreparationValidator in '..\src\Application\Download\DownloadPreparationValidator.pas',
	DownloadPreparationValidatorTest in 'Application\Download\DownloadPreparationValidatorTest.pas',
	UploadCompletionHandler in '..\src\Application\Upload\UploadCompletionHandler.pas',
	UploadCompletionHandlerTest in 'Application\Upload\UploadCompletionHandlerTest.pas',
	RootListingHandler in '..\src\Application\Listing\RootListingHandler.pas',
	RootListingHandlerTest in 'Application\Listing\RootListingHandlerTest.pas',
	PathListingHandler in '..\src\Application\Listing\PathListingHandler.pas',
	PathListingHandlerTest in 'Application\Listing\PathListingHandlerTest.pas',
	IconContextBuilder in '..\src\Presentation\Icon\IconContextBuilder.pas',
	IconContextBuilderTest in 'Presentation\Icon\IconContextBuilderTest.pas',
	OverwritePreparationHandler in '..\src\Application\Upload\OverwritePreparationHandler.pas',
	OverwritePreparationHandlerTest in 'Application\Upload\OverwritePreparationHandlerTest.pas',
	OperationStatusContextBuilder in '..\src\Application\Operation\OperationStatusContextBuilder.pas',
	OperationStatusContextBuilderTest in 'Application\Operation\OperationStatusContextBuilderTest.pas',
	ListingResultApplier in '..\src\Application\Listing\ListingResultApplier.pas',
	ListingResultApplierTest in 'Application\Listing\ListingResultApplierTest.pas',
	DownloadOrchestrator in '..\src\Application\Download\DownloadOrchestrator.pas',
	DownloadOrchestratorTest in 'Application\Download\DownloadOrchestratorTest.pas';

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
