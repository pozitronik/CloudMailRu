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
	WFXTypes in '..\src\Infrastructure\Protocol\WFXTypes.pas',
	SystemHelper in '..\src\Infrastructure\OS\SystemHelper.pas',
	TCProgress in '..\src\Infrastructure\Progress\TCProgress.pas',
	TCProgressTest in 'Infrastructure\Progress\TCProgressTest.pas',
	TCRequest in '..\src\Infrastructure\Request\TCRequest.pas',
	TCRequestTest in 'Infrastructure\Request\TCRequestTest.pas',
	TCPasswordManager in '..\src\Infrastructure\Password\TCPasswordManager.pas',
	TCPasswordManagerTest in 'Infrastructure\Password\TCPasswordManagerTest.pas',
	DebugHelper in '..\src\Infrastructure\Logger\DebugHelper.pas',
	FileHelper in '..\src\Infrastructure\IO\FileHelper.pas',
	PathHelper in '..\src\Infrastructure\IO\PathHelper.pas',
	WindowsHelper in '..\src\Infrastructure\OS\WindowsHelper.pas',
	ConnectionSettings in '..\src\Infrastructure\Settings\ConnectionSettings.pas',
	SETTINGS_CONSTANTS in '..\src\Infrastructure\Settings\SETTINGS_CONSTANTS.pas',
	CMRConstants in '..\src\Domain\Constants\CMRConstants.pas',
	ProxySettings in '..\src\Infrastructure\Settings\ProxySettings.pas',
	StringHelper in '..\src\Infrastructure\IO\StringHelper.pas',
	TCHandler in '..\src\Infrastructure\TC\TCHandler.pas',
	Description in '..\src\Domain\Services\Description.pas',
	LANGUAGE_STRINGS in '..\src\Presentation\Localization\LANGUAGE_STRINGS.pas',
	PluginSettingsManagerTest in 'Infrastructure\Settings\PluginSettingsManagerTest.pas',
	AccountsManagerTest in 'Infrastructure\Settings\AccountsManagerTest.pas',
	ParsingHelper in '..\src\Infrastructure\HTTP\ParsingHelper.pas',
	WSList in '..\src\Domain\ValueObjects\WSList.pas',
	AccountSettings in '..\src\Infrastructure\Settings\AccountSettings.pas',
	AccountsManager in '..\src\Infrastructure\Settings\AccountsManager.pas',
	FileCipher in '..\src\Infrastructure\Cipher\FileCipher.pas',
	CMRDirItemList in '..\src\Domain\ValueObjects\CMRDirItemList.pas',
	CMRDirItem in '..\src\Domain\ValueObjects\CMRDirItem.pas',
	JSONHelper in '..\src\Infrastructure\HTTP\JSONHelper.pas',
	DCPblockciphers in '..\src\libs\DCPCrypt\DCPblockciphers.pas',
	DCPcrypt2 in '..\src\libs\DCPCrypt\DCPcrypt2.pas',
	DCPtypes in '..\src\libs\DCPCrypt\DCPtypes.pas',
	DCPrijndael in '..\src\libs\DCPCrypt\Ciphers\DCPrijndael.pas',
	DCPconst in '..\src\libs\DCPCrypt\DCPconst.pas',
	DCPbase64 in '..\src\libs\DCPCrypt\DCPbase64.pas',
	DCPsha1 in '..\src\libs\DCPCrypt\Hashes\DCPsha1.pas',
	PluginSettingsManager in '..\src\Infrastructure\Settings\PluginSettingsManager.pas',
	PluginSettings in '..\src\Infrastructure\Settings\PluginSettings.pas',
	IniFilesHelper in '..\src\Infrastructure\Config\IniFilesHelper.pas',
	StreamingSettings in '..\src\Infrastructure\Settings\StreamingSettings.pas',
	FileHelperTest in 'Infrastructure\IO\FileHelperTest.pas',
	PathHelperTest in 'Infrastructure\IO\PathHelperTest.pas',
	StringHelperTest in 'Infrastructure\IO\StringHelperTest.pas',
	TokenRetryHelper in '..\src\Application\Retry\TokenRetryHelper.pas',
	TokenRetryHelperTest in 'Application\Retry\TokenRetryHelperTest.pas',
	ParsingHelperTest in 'Infrastructure\HTTP\ParsingHelperTest.pas',
	RealPathTest in 'Domain\ValueObjects\RealPathTest.pas',
	HashInfo in '..\src\Domain\ValueObjects\HashInfo.pas',
	HashInfoTest in 'Domain\ValueObjects\HashInfoTest.pas',
	CMRDirItemTest in 'Domain\ValueObjects\CMRDirItemTest.pas',
	CMROperationResultTest in 'Domain\ValueObjects\CMROperationResultTest.pas',
	CMRSpaceTest in 'Domain\ValueObjects\CMRSpaceTest.pas',
	WSListTest in 'Domain\ValueObjects\WSListTest.pas',
	JSONHelperTest in 'Infrastructure\HTTP\JSONHelperTest.pas',
	IniFilesHelperTest in 'Infrastructure\Config\IniFilesHelperTest.pas',
	IniConfigFileTest in 'Infrastructure\Config\IniConfigFileTest.pas',
	WindowsHelperTest in 'Infrastructure\OS\WindowsHelperTest.pas',
	CMRDirItemListTest in 'Domain\ValueObjects\CMRDirItemListTest.pas',
	CMROAuthTest in 'Domain\ValueObjects\CMROAuthTest.pas',
	CMRIncomingInviteTest in 'Domain\ValueObjects\CMRIncomingInviteTest.pas',
	CMRInviteListTest in 'Domain\ValueObjects\CMRInviteListTest.pas',
	CMRIncomingInviteListTest in 'Domain\ValueObjects\CMRIncomingInviteListTest.pas',
	CMRTwostepTest in 'Domain\ValueObjects\CMRTwostepTest.pas',
	CMRFileIdentityTest in 'Domain\ValueObjects\CMRFileIdentityTest.pas',
	AccountSettingsTest in 'Infrastructure\Settings\AccountSettingsTest.pas',
	PluginSettingsTest in 'Infrastructure\Settings\PluginSettingsTest.pas',
	FileCipherTest in 'Infrastructure\Cipher\FileCipherTest.pas',
	NullCipherTest in 'Infrastructure\Cipher\NullCipherTest.pas',
	CloudMailRuResourceTest in 'Domain\Services\CloudMailRuResourceTest.pas',
	CloudMailRuStaticTest in 'Domain\Services\CloudMailRuStaticTest.pas',
	CloudAccessUtils in '..\src\Domain\Services\CloudAccessUtils.pas',
	CloudAccessUtilsTest in 'Domain\Services\CloudAccessUtilsTest.pas',
	CloudMailRuInstanceTest in 'Domain\Services\CloudMailRuInstanceTest.pas',
	CloudMailRuFactoryTest in 'Domain\Services\CloudMailRuFactoryTest.pas',
	CloudMailRuHashTest in 'Domain\Services\CloudMailRuHashTest.pas',
	ChunkedFileStreamTest in 'Infrastructure\IO\ChunkedFileStreamTest.pas',
	DescriptionTest in 'Domain\Services\DescriptionTest.pas',
	FileSplitInfoTest in 'Infrastructure\IO\FileSplitInfoTest.pas',
	TCHandlerTest in 'Infrastructure\TC\TCHandlerTest.pas',
	IconHelper in '..\src\Presentation\Icon\IconHelper.pas',
	IconHelperTest in 'Presentation\Icon\IconHelperTest.pas',
	CloudMailRu in '..\src\Domain\Services\CloudMailRu.pas',
	CloudMailRuFactory in '..\src\Domain\Services\CloudMailRuFactory.pas',
	CMRFileIdentity in '..\src\Domain\ValueObjects\CMRFileIdentity.pas',
	CMRIncomingInviteList in '..\src\Domain\ValueObjects\CMRIncomingInviteList.pas',
	CMRInviteList in '..\src\Domain\ValueObjects\CMRInviteList.pas',
	CMROAuth in '..\src\Domain\ValueObjects\CMROAuth.pas',
	CMROperationResult in '..\src\Domain\ValueObjects\CMROperationResult.pas',
	CMRSpace in '..\src\Domain\ValueObjects\CMRSpace.pas',
	CMRTwostep in '..\src\Domain\ValueObjects\CMRTwostep.pas',
	CloudHTTP in '..\src\Infrastructure\HTTP\CloudHTTP.pas',
	CMRInvite in '..\src\Domain\ValueObjects\CMRInvite.pas',
	CMRIncomingInvite in '..\src\Domain\ValueObjects\CMRIncomingInvite.pas',
	CMROwner in '..\src\Domain\ValueObjects\CMROwner.pas',
	ChunkedFileStream in '..\src\Infrastructure\IO\ChunkedFileStream.pas',
	FileSplitInfo in '..\src\Infrastructure\IO\FileSplitInfo.pas',
	HTTPManager in '..\src\Infrastructure\HTTP\HTTPManager.pas',
	RealPath in '..\src\Domain\ValueObjects\RealPath.pas',
	CloudSettings in '..\src\Infrastructure\Settings\CloudSettings.pas',
	CloudSettingsTest in 'Infrastructure\Settings\CloudSettingsTest.pas',
	PasswordUIProvider in '..\src\Infrastructure\UI\PasswordUIProvider.pas',
	PasswordUIProviderTest in 'Infrastructure\UI\PasswordUIProviderTest.pas',
	HTTPManagerTest in 'Infrastructure\HTTP\HTTPManagerTest.pas',
	CloudHTTPTest in 'Infrastructure\HTTP\CloudHTTPTest.pas',
	CipherValidator in '..\src\Infrastructure\Cipher\CipherValidator.pas',
	CipherValidatorTest in 'Infrastructure\Cipher\CipherValidatorTest.pas',
	ConnectionManagerTest in 'Infrastructure\Services\ConnectionManagerTest.pas',
	
	ConnectionManager in '..\src\Infrastructure\Services\ConnectionManager.pas',
	WindowsFileSystem in '..\src\Infrastructure\FileSystem\WindowsFileSystem.pas',
	WindowsFileSystemTest in 'Infrastructure\FileSystem\WindowsFileSystemTest.pas',
	IniConfigFile in '..\src\Infrastructure\Config\IniConfigFile.pas',
	WindowsEnvironment in '..\src\Infrastructure\Environment\WindowsEnvironment.pas',
	WindowsEnvironmentTest in 'Infrastructure\Environment\WindowsEnvironmentTest.pas',
	AskPassword in '..\src\Presentation\UI\Forms\AskPassword.pas'{AskPasswordForm},
	IAuthStrategyInterface in '..\src\Infrastructure\Authentication\IAuthStrategyInterface.pas',
	NullAuthStrategyTest in 'Infrastructure\Authentication\NullAuthStrategyTest.pas',
	OAuthAppAuthStrategy in '..\src\Infrastructure\Authentication\OAuthAppAuthStrategy.pas',
	OAuthAppAuthStrategyTest in 'Infrastructure\Authentication\OAuthAppAuthStrategyTest.pas',
	SharedAccountAuthStrategy in '..\src\Infrastructure\Authentication\SharedAccountAuthStrategy.pas',
	SharedAccountAuthStrategyTest in 'Infrastructure\Authentication\SharedAccountAuthStrategyTest.pas',
	WebAuthStrategy in '..\src\Infrastructure\Authentication\WebAuthStrategy.pas',
	TwoStepAuthStrategy in '..\src\Infrastructure\Authentication\TwoStepAuthStrategy.pas',
	OldOAuthStrategy in '..\src\Infrastructure\Authentication\OldOAuthStrategy.pas',
	DeprecatedStrategiesTest in 'Infrastructure\Authentication\DeprecatedStrategiesTest.pas',
	MockCloudHTTP in 'mocks\MockCloudHTTP.pas',
	MockHTTPManager in 'mocks\MockHTTPManager.pas',
	MockConnectionManager in 'mocks\MockConnectionManager.pas',
	MockShardHelper in 'mocks\MockShardHelper.pas',
	MockAuthStrategy in 'mocks\MockAuthStrategy.pas',
	MockCloudHTTPStreamTest in 'mocks\MockCloudHTTPStreamTest.pas',
	CloudMailRuDirListingTest in 'Domain\Services\CloudMailRuDirListingTest.pas',
	CloudMailRuFileOperationsTest in 'Domain\Services\CloudMailRuFileOperationsTest.pas',
	CloudMailRuPublicAccountTest in 'Domain\Services\CloudMailRuPublicAccountTest.pas',
	CloudMailRuErrorHandlingTest in 'Domain\Services\CloudMailRuErrorHandlingTest.pas',
	CloudMailRuUploadDownloadTest in 'Domain\Services\CloudMailRuUploadDownloadTest.pas',
	CloudMailRuAdditionalOpsTest in 'Domain\Services\CloudMailRuAdditionalOpsTest.pas',
	CloudMailRuMiscOpsTest in 'Domain\Services\CloudMailRuMiscOpsTest.pas',
	CloudMailRuCombinedOpsTest in 'Domain\Services\CloudMailRuCombinedOpsTest.pas',
	CloudMailRuShardTest in 'Domain\Services\CloudMailRuShardTest.pas',
	CloudMailRuLoginTest in 'Domain\Services\CloudMailRuLoginTest.pas',
	CloudMailRuDownloadTest in 'Domain\Services\CloudMailRuDownloadTest.pas',
	CloudMailRuUploadTest in 'Domain\Services\CloudMailRuUploadTest.pas',
	CloudMailRuIntegrationTest in 'Domain\Services\CloudMailRuIntegrationTest.pas',
	CloudMailRuLoginFlowTest in 'Domain\Services\CloudMailRuLoginFlowTest.pas',
	CloudFileUploaderSplitTest in 'Application\Upload\CloudFileUploaderSplitTest.pas',
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
	CloudDescriptionOpsAdapterTest in 'Infrastructure\Adapter\CloudDescriptionOpsAdapterTest.pas',
	CMRDirItemJsonAdapter in '..\src\Infrastructure\Adapter\CMRDirItemJsonAdapter.pas',
	CMRDirItemJsonAdapterTest in 'Infrastructure\Adapter\CMRDirItemJsonAdapterTest.pas',
	CMRDirItemListJsonAdapter in '..\src\Infrastructure\Adapter\CMRDirItemListJsonAdapter.pas',
	CMRDirItemListJsonAdapterTest in 'Infrastructure\Adapter\CMRDirItemListJsonAdapterTest.pas',
	CMROAuthJsonAdapter in '..\src\Infrastructure\Adapter\CMROAuthJsonAdapter.pas',
	CMROAuthJsonAdapterTest in 'Infrastructure\Adapter\CMROAuthJsonAdapterTest.pas',
	CMROperationResultJsonAdapter in '..\src\Infrastructure\Adapter\CMROperationResultJsonAdapter.pas',
	CMROperationResultJsonAdapterTest in 'Infrastructure\Adapter\CMROperationResultJsonAdapterTest.pas',
	CMRSpaceJsonAdapter in '..\src\Infrastructure\Adapter\CMRSpaceJsonAdapter.pas',
	CMRSpaceJsonAdapterTest in 'Infrastructure\Adapter\CMRSpaceJsonAdapterTest.pas',
	CMRTwostepJsonAdapter in '..\src\Infrastructure\Adapter\CMRTwostepJsonAdapter.pas',
	CMRTwostepJsonAdapterTest in 'Infrastructure\Adapter\CMRTwostepJsonAdapterTest.pas',
	CMRInviteListJsonAdapter in '..\src\Infrastructure\Adapter\CMRInviteListJsonAdapter.pas',
	CMRInviteListJsonAdapterTest in 'Infrastructure\Adapter\CMRInviteListJsonAdapterTest.pas',
	CMRIncomingInviteListJsonAdapter in '..\src\Infrastructure\Adapter\CMRIncomingInviteListJsonAdapter.pas',
	CMRIncomingInviteListJsonAdapterTest in 'Infrastructure\Adapter\CMRIncomingInviteListJsonAdapterTest.pas',
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
	DownloadOrchestratorTest in 'Application\Download\DownloadOrchestratorTest.pas',
	CloudHashCalculator in '..\src\Infrastructure\Hash\CloudHashCalculator.pas',
	CloudHashCalculatorTest in 'Infrastructure\Hash\CloudHashCalculatorTest.pas',
	CloudShardManager in '..\src\Infrastructure\Shard\CloudShardManager.pas',
	CloudShardManagerTest in 'Infrastructure\Shard\CloudShardManagerTest.pas',
	CloudErrorMapper in '..\src\Domain\Services\CloudErrorMapper.pas',
	CloudErrorMapperTest in 'Domain\Services\CloudErrorMapperTest.pas',
	CloudCallbackTypes in '..\src\Application\CloudCallbackTypes.pas',
	CloudFileDownloader in '..\src\Application\Download\CloudFileDownloader.pas',
	CloudFileDownloaderTest in 'Application\Download\CloudFileDownloaderTest.pas',
	CloudFileUploader in '..\src\Application\Upload\CloudFileUploader.pas',
	CloudFileUploaderTest in 'Application\Upload\CloudFileUploaderTest.pas',
	CloudShareService in '..\src\Application\Share\CloudShareService.pas',
	CloudShareServiceTest in 'Application\Share\CloudShareServiceTest.pas',
	CloudListingService in '..\src\Application\Listing\CloudListingService.pas',
	CloudListingServiceTest in 'Application\Listing\CloudListingServiceTest.pas',
	CloudFileOperations in '..\src\Application\FileOps\CloudFileOperations.pas',
	CloudFileOperationsTest in 'Application\FileOps\CloudFileOperationsTest.pas',
	RemotePropertyPresenter in '..\src\Presentation\Presenter\RemotePropertyPresenter.pas',
	RemotePropertyPresenterTest in 'Presentation\Presenter\RemotePropertyPresenterTest.pas',
	AccountsPresenter in '..\src\Presentation\Presenter\AccountsPresenter.pas',
	AccountsPresenterTest in 'Presentation\Presenter\AccountsPresenterTest.pas',
	InvitePropertyPresenter in '..\src\Presentation\Presenter\InvitePropertyPresenter.pas',
	InvitePropertyPresenterTest in 'Presentation\Presenter\InvitePropertyPresenterTest.pas',
	DeletedPropertyPresenter in '..\src\Presentation\Presenter\DeletedPropertyPresenter.pas',
	DeletedPropertyPresenterTest in 'Presentation\Presenter\DeletedPropertyPresenterTest.pas',
	AskPasswordPresenter in '..\src\Presentation\Presenter\AskPasswordPresenter.pas',
	AskPasswordPresenterTest in 'Presentation\Presenter\AskPasswordPresenterTest.pas',
	AskPasswordFormTest in 'Presentation\UI\Forms\AskPasswordFormTest.pas',
	{Integration Tests}
	IntegrationTestConfig in 'Integration\IntegrationTestConfig.pas',
	TestDataGenerator in 'Integration\TestDataGenerator.pas',
	IntegrationTestBase in 'Integration\IntegrationTestBase.pas',
	AuthenticationIntegrationTest in 'Integration\AuthenticationIntegrationTest.pas',
	QuotaIntegrationTest in 'Integration\QuotaIntegrationTest.pas',
	DirectoryIntegrationTest in 'Integration\DirectoryIntegrationTest.pas',
	UploadIntegrationTest in 'Integration\UploadIntegrationTest.pas',
	DownloadIntegrationTest in 'Integration\DownloadIntegrationTest.pas',
	FileOpsIntegrationTest in 'Integration\FileOpsIntegrationTest.pas',
	TrashIntegrationTest in 'Integration\TrashIntegrationTest.pas',
	SharingIntegrationTest in 'Integration\SharingIntegrationTest.pas',
	InviteIntegrationTest in 'Integration\InviteIntegrationTest.pas';

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
