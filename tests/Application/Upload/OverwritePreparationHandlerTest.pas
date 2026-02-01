unit OverwritePreparationHandlerTest;

{Tests for TOverwritePreparationHandler.
 Note: Full integration tests require cloud connection mocking.
 These tests cover the logic paths that don't require cloud operations.}

interface

uses
	DUnitX.TestFramework,
	OverwritePreparationHandler,
	MockConnectionManager,
	MockCloudHTTP,
	MockHTTPManager,
	CloudMailRu,
	CloudSettings,
	Cipher,
	AuthStrategy,
	FileSystem,
	Logger,
	Progress,
	Request,
	TCHandler,
	OpenSSLProvider,
	AccountCredentialsProvider,
	WFXTypes,
	RealPath,
	TestHelper;

type
	[TestFixture]
	TOverwritePreparationHandlerTest = class
	private
		FHandler: IOverwritePreparationHandler;
		FMockConnectionManager: TMockConnectionManager;
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TCloudMailRu;

		function CreateCloud: TCloudMailRu;
	public
		[Setup]
		procedure Setup;

		[TearDown]
		procedure TearDown;

		[Test]
		procedure Prepare_WhenOverwriteNotRequired_ReturnsSuccess;

		[Test]
		procedure Prepare_WhenOverwriteNotRequired_DoesNotCallConnectionManager;

		[Test]
		procedure Prepare_WhenOverwriteNotRequired_ResultCodeIsFS_FILE_OK;

		[Test]
		procedure Prepare_WhenOverwriteNotRequired_SuccessIsTrue;

		[Test]
		procedure Prepare_WhenOverwriteRequired_CallsConnectionManager;
	end;

implementation

uses
	SysUtils;

function TOverwritePreparationHandlerTest.CreateCloud: TCloudMailRu;
var
	Settings: TCloudSettings;
begin
	Settings := Default(TCloudSettings);
	Result := TCloudMailRu.Create(
		Settings,
		FMockHTTPManager,
		TestThreadID(),
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		TNullCipher.Create,
		TNullOpenSSLProvider.Create,
		TNullAccountCredentialsProvider.Create);
end;

procedure TOverwritePreparationHandlerTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
	FCloud := CreateCloud;
	FMockConnectionManager := TMockConnectionManager.Create;
	FMockConnectionManager.SetCloud('account', FCloud);
	FHandler := TOverwritePreparationHandler.Create(FMockConnectionManager);
end;

procedure TOverwritePreparationHandlerTest.TearDown;
begin
	FHandler := nil;
	FMockConnectionManager := nil;
	FCloud.Free;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

procedure TOverwritePreparationHandlerTest.Prepare_WhenOverwriteNotRequired_ReturnsSuccess;
var
	Path: TRealPath;
	Result: TOverwritePreparationResult;
begin
	Path.FromPath('\account\test.txt');

	Result := FHandler.Prepare(Path, False);

	Assert.IsTrue(Result.Success, 'Should succeed when overwrite not required');
end;

procedure TOverwritePreparationHandlerTest.Prepare_WhenOverwriteNotRequired_DoesNotCallConnectionManager;
var
	Path: TRealPath;
begin
	Path.FromPath('\account\test.txt');

	FHandler.Prepare(Path, False);

	Assert.AreEqual(0, FMockConnectionManager.GetCallCount, 'Should not call connection manager');
end;

procedure TOverwritePreparationHandlerTest.Prepare_WhenOverwriteNotRequired_ResultCodeIsFS_FILE_OK;
var
	Path: TRealPath;
	Result: TOverwritePreparationResult;
begin
	Path.FromPath('\account\test.txt');

	Result := FHandler.Prepare(Path, False);

	Assert.AreEqual(FS_FILE_OK, Result.ResultCode, 'ResultCode should be FS_FILE_OK');
end;

procedure TOverwritePreparationHandlerTest.Prepare_WhenOverwriteNotRequired_SuccessIsTrue;
var
	Path: TRealPath;
	Result: TOverwritePreparationResult;
begin
	Path.FromPath('\account\test.txt');

	Result := FHandler.Prepare(Path, False);

	Assert.IsTrue(Result.Success, 'Success should be True');
end;

procedure TOverwritePreparationHandlerTest.Prepare_WhenOverwriteRequired_CallsConnectionManager;
var
	Path: TRealPath;
	Result: TOverwritePreparationResult;
begin
	Path.FromPath('\account\test.txt');
	{Cloud is configured but Delete will fail (mock HTTP returns failure)}
	FMockHTTP.SetResponse('', False, '');

	Result := FHandler.Prepare(Path, True);

	Assert.IsFalse(Result.Success, 'Should fail when delete fails');
	Assert.AreEqual(1, FMockConnectionManager.GetCallCount, 'Should call connection manager when overwrite required');
end;

initialization
	TDUnitX.RegisterTestFixture(TOverwritePreparationHandlerTest);

end.
