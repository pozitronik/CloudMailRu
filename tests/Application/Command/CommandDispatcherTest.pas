unit CommandDispatcherTest;

{Unit tests for TCommandDispatcher - plugin command routing.
 Tests Execute method for all supported commands: rmdir, share, hash, clone, trash, shared, invites.}

interface

uses
	System.SysUtils,
	DUnitX.TestFramework,
	CommandDispatcher,
	ConnectionManager,
	CloudMailRu,
	CloudSettings,
	PluginSettings,
	PluginSettingsManager,
	MockConnectionManager,
	MockCloudHTTP,
	MockHTTPManager,
	IAuthStrategyInterface,
	WindowsFileSystem,
	TCLogger,
	TCProgress,
	TCRequest,
	CMRConstants,
	PLUGIN_TYPES;

type
	{Mock settings manager for testing}
	TMockSettingsManager = class(TInterfacedObject, IPluginSettingsManager)
	private
		FSettings: TPluginSettings;
	public
		constructor Create;
		function GetSettings: TPluginSettings;
		procedure SwitchProxyPasswordStorage;
		procedure SetLogUserSpace(Value: Boolean);
	end;

	{Testable subclass that exposes protected members}
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
		procedure SetPublicLink(const Value: WideString);
	end;

	[TestFixture]
	TCommandResultTest = class
	public
		{TCommandResult.OK tests}
		[Test]
		procedure TestOK_ReturnsExecOK;
		[Test]
		procedure TestOK_HasEmptySymlinkPath;

		{TCommandResult.Error tests}
		[Test]
		procedure TestError_ReturnsExecError;
		[Test]
		procedure TestError_HasEmptySymlinkPath;

		{TCommandResult.Symlink tests}
		[Test]
		procedure TestSymlink_ReturnsExecSymlink;
		[Test]
		procedure TestSymlink_StoresPath;
		[Test]
		procedure TestSymlink_PreservesPathWithBackslash;
		[Test]
		procedure TestSymlink_PreservesPathWithPostfix;
	end;

	[TestFixture]
	TCommandDispatcherTest = class
	private
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FMockConnectionManager: TMockConnectionManager;
		FMockSettingsManager: TMockSettingsManager;
		FCloud: TTestableCloudMailRu;
		FDispatcher: ICommandDispatcher;

		function CreateCloud(PublicAccount: Boolean = False): TTestableCloudMailRu;
		procedure SetupDispatcher(Cloud: TTestableCloudMailRu = nil);
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{rmdir command tests}
		[Test]
		procedure TestRmdir_Success_ReturnsOK;
		[Test]
		procedure TestRmdir_Failure_ReturnsError;
		[Test]
		procedure TestRmdir_CombinesRemoteNameAndParameter;

		{share command tests}
		[Test]
		procedure TestShare_Success_ReturnsOK;
		[Test]
		procedure TestShare_Failure_ReturnsError;
		[Test]
		procedure TestShare_ExtractsLinkFromUrl;

		{hash command tests}
		[Test]
		procedure TestHash_ValidHash_ReturnsOK;
		[Test]
		procedure TestHash_InvalidHash_ReturnsError;
		[Test]
		procedure TestHash_CallsAddFileByIdentity;

		{clone command tests}
		[Test]
		procedure TestClone_Success_ReturnsOK;
		[Test]
		procedure TestClone_Failure_ReturnsError;
		[Test]
		procedure TestClone_LogsUserSpaceWhenEnabled;

		{trash command tests}
		[Test]
		procedure TestTrash_DeepPath_ReturnsSymlink;
		[Test]
		procedure TestTrash_AccountRoot_ReturnsSymlink;
		[Test]
		procedure TestTrash_PublicAccount_ReturnsError;

		{shared command tests}
		[Test]
		procedure TestShared_DeepPath_ReturnsSymlink;
		[Test]
		procedure TestShared_AccountRoot_ReturnsSymlink;
		[Test]
		procedure TestShared_PublicAccount_ReturnsError;

		{invites command tests}
		[Test]
		procedure TestInvites_DeepPath_ReturnsSymlink;
		[Test]
		procedure TestInvites_AccountRoot_ReturnsSymlink;
		[Test]
		procedure TestInvites_PublicAccount_ReturnsError;

		{Unknown command tests}
		[Test]
		procedure TestUnknownCommand_ReturnsOK;
	end;

implementation

const
	JSON_SUCCESS = '{"email":"test@mail.ru","body":{},"status":200}';
	JSON_FAILURE = '{"email":"test@mail.ru","body":{"home":{"error":"not_exists"}},"status":400}';
	JSON_CLONE_SUCCESS = '{"email":"test@mail.ru","body":{"home":"/cloned"},"status":200}';

{TMockSettingsManager}

constructor TMockSettingsManager.Create;
begin
	inherited Create;
	FSettings := Default(TPluginSettings);
	FSettings.LogUserSpace := False;
end;

function TMockSettingsManager.GetSettings: TPluginSettings;
begin
	Result := FSettings;
end;

procedure TMockSettingsManager.SwitchProxyPasswordStorage;
begin
	{Not used in these tests}
end;

procedure TMockSettingsManager.SetLogUserSpace(Value: Boolean);
begin
	FSettings.LogUserSpace := Value;
end;

{TTestableCloudMailRu}

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

procedure TTestableCloudMailRu.SetPublicLink(const Value: WideString);
begin
	FPublicLink := Value;
end;

{TCommandResultTest}

procedure TCommandResultTest.TestOK_ReturnsExecOK;
var
	R: TCommandResult;
begin
	R := TCommandResult.OK;
	Assert.AreEqual(FS_EXEC_OK, R.ResultCode);
end;

procedure TCommandResultTest.TestOK_HasEmptySymlinkPath;
var
	R: TCommandResult;
begin
	R := TCommandResult.OK;
	Assert.AreEqual('', R.SymlinkPath);
end;

procedure TCommandResultTest.TestError_ReturnsExecError;
var
	R: TCommandResult;
begin
	R := TCommandResult.Error;
	Assert.AreEqual(FS_EXEC_ERROR, R.ResultCode);
end;

procedure TCommandResultTest.TestError_HasEmptySymlinkPath;
var
	R: TCommandResult;
begin
	R := TCommandResult.Error;
	Assert.AreEqual('', R.SymlinkPath);
end;

procedure TCommandResultTest.TestSymlink_ReturnsExecSymlink;
var
	R: TCommandResult;
begin
	R := TCommandResult.Symlink('\test');
	Assert.AreEqual(FS_EXEC_SYMLINK, R.ResultCode);
end;

procedure TCommandResultTest.TestSymlink_StoresPath;
var
	R: TCommandResult;
begin
	R := TCommandResult.Symlink('\account\path');
	Assert.AreEqual('\account\path', R.SymlinkPath);
end;

procedure TCommandResultTest.TestSymlink_PreservesPathWithBackslash;
var
	R: TCommandResult;
begin
	R := TCommandResult.Symlink('\myaccount');
	Assert.AreEqual('\myaccount', R.SymlinkPath);
end;

procedure TCommandResultTest.TestSymlink_PreservesPathWithPostfix;
var
	R: TCommandResult;
begin
	R := TCommandResult.Symlink('\account.trash');
	Assert.AreEqual('\account.trash', R.SymlinkPath);
end;

{TCommandDispatcherTest}

procedure TCommandDispatcherTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
	FMockConnectionManager := TMockConnectionManager.Create;
	FMockSettingsManager := TMockSettingsManager.Create;
end;

procedure TCommandDispatcherTest.TearDown;
begin
	FDispatcher := nil;
	FCloud.Free;
	FMockConnectionManager := nil;
	FMockSettingsManager := nil;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCommandDispatcherTest.CreateCloud(PublicAccount: Boolean): TTestableCloudMailRu;
var
	Settings: TCloudSettings;
begin
	Settings := Default(TCloudSettings);
	Settings.AccountSettings.PublicAccount := PublicAccount;
	if PublicAccount then
		Settings.AccountSettings.PublicUrl := 'https://cloud.mail.ru/public/abc123';

	Result := TTestableCloudMailRu.Create(
		Settings,
		FMockHTTPManager,
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create);

	Result.SetUnitedParams('api=2&access_token=test_token');
end;

procedure TCommandDispatcherTest.SetupDispatcher(Cloud: TTestableCloudMailRu);
begin
	if Assigned(Cloud) then
	begin
		FCloud := Cloud;
		FMockConnectionManager.SetCloud('testaccount', FCloud);
	end;

	FDispatcher := TCommandDispatcher.Create(
		FMockConnectionManager,
		TNullLogger.Create,
		FMockSettingsManager);
end;

{rmdir tests}

procedure TCommandDispatcherTest.TestRmdir_Success_ReturnsOK;
begin
	FCloud := CreateCloud;
	FMockConnectionManager.SetCloud('testaccount', FCloud);
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_SUCCESS);
	SetupDispatcher;

	var Result := FDispatcher.Execute('\testaccount', 'rmdir', '\folder');

	Assert.AreEqual(FS_EXEC_OK, Result.ResultCode);
end;

procedure TCommandDispatcherTest.TestRmdir_Failure_ReturnsError;
begin
	FCloud := CreateCloud;
	FMockConnectionManager.SetCloud('testaccount', FCloud);
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_FAILURE);
	SetupDispatcher;

	var Result := FDispatcher.Execute('\testaccount', 'rmdir', '\folder');

	Assert.AreEqual(FS_EXEC_ERROR, Result.ResultCode);
end;

procedure TCommandDispatcherTest.TestRmdir_CombinesRemoteNameAndParameter;
begin
	FCloud := CreateCloud;
	FMockConnectionManager.SetCloud('testaccount', FCloud);
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_SUCCESS);
	SetupDispatcher;

	{RemoteName + Parameter should be combined to form full path}
	var Result := FDispatcher.Execute('\testaccount\parent', 'rmdir', '\subdir');

	Assert.AreEqual(FS_EXEC_OK, Result.ResultCode);
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_REMOVE), 'Should call file remove API');
end;

{share tests}

procedure TCommandDispatcherTest.TestShare_Success_ReturnsOK;
begin
	SetupDispatcher(CreateCloud);
	FMockHTTP.SetResponse(API_FOLDER_SHARE, True, JSON_SUCCESS);

	var Result := FDispatcher.Execute('\testaccount\folder', 'share', 'user@mail.ru');

	Assert.AreEqual(FS_EXEC_OK, Result.ResultCode);
end;

procedure TCommandDispatcherTest.TestShare_Failure_ReturnsError;
begin
	SetupDispatcher(CreateCloud);
	FMockHTTP.SetResponse(API_FOLDER_SHARE, True, JSON_FAILURE);

	var Result := FDispatcher.Execute('\testaccount\folder', 'share', 'user@mail.ru');

	Assert.AreEqual(FS_EXEC_ERROR, Result.ResultCode);
end;

procedure TCommandDispatcherTest.TestShare_ExtractsLinkFromUrl;
begin
	SetupDispatcher(CreateCloud);
	FMockHTTP.SetResponse(API_FOLDER_SHARE, True, JSON_SUCCESS);

	{If parameter is URL, should extract email/link from it}
	var Result := FDispatcher.Execute('\testaccount\folder', 'share', 'mailto:user@mail.ru');

	Assert.AreEqual(FS_EXEC_OK, Result.ResultCode);
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FOLDER_SHARE), 'Should call share API');
end;

{hash tests}

procedure TCommandDispatcherTest.TestHash_ValidHash_ReturnsOK;
begin
	SetupDispatcher(CreateCloud);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_SUCCESS);

	{Valid hash format: hash:size:filename - hash must be 40 chars (SHA1 hex)}
	var Result := FDispatcher.Execute('\testaccount\folder', 'hash', 'ABCDEF1234567890ABCDEF1234567890ABCDEF12:1024:testfile.txt');

	Assert.AreEqual(FS_EXEC_OK, Result.ResultCode);
end;

procedure TCommandDispatcherTest.TestHash_InvalidHash_ReturnsError;
begin
	SetupDispatcher(CreateCloud);

	{Invalid hash format should return error - hash too short}
	var Result := FDispatcher.Execute('\testaccount\folder', 'hash', 'ABC123:1024:testfile.txt');

	Assert.AreEqual(FS_EXEC_ERROR, Result.ResultCode);
end;

procedure TCommandDispatcherTest.TestHash_CallsAddFileByIdentity;
begin
	SetupDispatcher(CreateCloud);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_SUCCESS);

	{40-char SHA1 hash}
	FDispatcher.Execute('\testaccount\folder', 'hash', '1234567890ABCDEF1234567890ABCDEF12345678:2048:myfile.dat');

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_ADD), 'Should call file add API');
end;

{clone tests}

procedure TCommandDispatcherTest.TestClone_Success_ReturnsOK;
begin
	SetupDispatcher(CreateCloud);
	FMockHTTP.SetResponse(API_CLONE, True, JSON_CLONE_SUCCESS);

	var Result := FDispatcher.Execute('\testaccount\folder', 'clone', 'https://cloud.mail.ru/public/abc123');

	Assert.AreEqual(FS_EXEC_OK, Result.ResultCode);
end;

procedure TCommandDispatcherTest.TestClone_Failure_ReturnsError;
begin
	SetupDispatcher(CreateCloud);
	FMockHTTP.SetResponse(API_CLONE, True, JSON_FAILURE);

	var Result := FDispatcher.Execute('\testaccount\folder', 'clone', 'https://cloud.mail.ru/public/invalid');

	Assert.AreEqual(FS_EXEC_ERROR, Result.ResultCode);
end;

procedure TCommandDispatcherTest.TestClone_LogsUserSpaceWhenEnabled;
begin
	FMockSettingsManager := TMockSettingsManager.Create;
	FMockSettingsManager.SetLogUserSpace(True);
	SetupDispatcher(CreateCloud);
	FMockHTTP.SetResponse(API_CLONE, True, JSON_CLONE_SUCCESS);
	FMockHTTP.SetResponse(API_USER_SPACE, True, '{"email":"test@mail.ru","body":{"bytes_total":1000,"bytes_used":500,"overquota":false},"status":200}');

	var Result := FDispatcher.Execute('\testaccount\folder', 'clone', 'https://cloud.mail.ru/public/abc123');

	Assert.AreEqual(FS_EXEC_OK, Result.ResultCode);
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_USER_SPACE), 'Should log user space when enabled');
end;

{trash tests}

procedure TCommandDispatcherTest.TestTrash_DeepPath_ReturnsSymlink;
begin
	SetupDispatcher(CreateCloud);

	var Result := FDispatcher.Execute('\testaccount\somefolder', 'trash', '');

	Assert.AreEqual(FS_EXEC_SYMLINK, Result.ResultCode);
	Assert.AreEqual('\testaccount.trash', Result.SymlinkPath);
end;

procedure TCommandDispatcherTest.TestTrash_AccountRoot_ReturnsSymlink;
begin
	SetupDispatcher(CreateCloud);

	{At account root level - still considered "in account", returns symlink}
	var Result := FDispatcher.Execute('\testaccount', 'trash', '');

	Assert.AreEqual(FS_EXEC_SYMLINK, Result.ResultCode);
	Assert.AreEqual('\testaccount.trash', Result.SymlinkPath);
end;

procedure TCommandDispatcherTest.TestTrash_PublicAccount_ReturnsError;
begin
	SetupDispatcher(CreateCloud(True)); {Public account}

	var Result := FDispatcher.Execute('\testaccount\folder', 'trash', '');

	Assert.AreEqual(FS_EXEC_ERROR, Result.ResultCode);
end;

{shared tests}

procedure TCommandDispatcherTest.TestShared_DeepPath_ReturnsSymlink;
begin
	SetupDispatcher(CreateCloud);

	var Result := FDispatcher.Execute('\testaccount\somefolder', 'shared', '');

	Assert.AreEqual(FS_EXEC_SYMLINK, Result.ResultCode);
	Assert.AreEqual('\testaccount.shared', Result.SymlinkPath);
end;

procedure TCommandDispatcherTest.TestShared_AccountRoot_ReturnsSymlink;
begin
	SetupDispatcher(CreateCloud);

	{At account root level - still considered "in account", returns symlink}
	var Result := FDispatcher.Execute('\testaccount', 'shared', '');

	Assert.AreEqual(FS_EXEC_SYMLINK, Result.ResultCode);
	Assert.AreEqual('\testaccount.shared', Result.SymlinkPath);
end;

procedure TCommandDispatcherTest.TestShared_PublicAccount_ReturnsError;
begin
	SetupDispatcher(CreateCloud(True));

	var Result := FDispatcher.Execute('\testaccount\folder', 'shared', '');

	Assert.AreEqual(FS_EXEC_ERROR, Result.ResultCode);
end;

{invites tests}

procedure TCommandDispatcherTest.TestInvites_DeepPath_ReturnsSymlink;
begin
	SetupDispatcher(CreateCloud);

	var Result := FDispatcher.Execute('\testaccount\somefolder', 'invites', '');

	Assert.AreEqual(FS_EXEC_SYMLINK, Result.ResultCode);
	Assert.AreEqual('\testaccount.invites', Result.SymlinkPath);
end;

procedure TCommandDispatcherTest.TestInvites_AccountRoot_ReturnsSymlink;
begin
	SetupDispatcher(CreateCloud);

	{At account root level - still considered "in account", returns symlink}
	var Result := FDispatcher.Execute('\testaccount', 'invites', '');

	Assert.AreEqual(FS_EXEC_SYMLINK, Result.ResultCode);
	Assert.AreEqual('\testaccount.invites', Result.SymlinkPath);
end;

procedure TCommandDispatcherTest.TestInvites_PublicAccount_ReturnsError;
begin
	SetupDispatcher(CreateCloud(True));

	var Result := FDispatcher.Execute('\testaccount\folder', 'invites', '');

	Assert.AreEqual(FS_EXEC_ERROR, Result.ResultCode);
end;

{Unknown command tests}

procedure TCommandDispatcherTest.TestUnknownCommand_ReturnsOK;
begin
	SetupDispatcher(CreateCloud);

	var Result := FDispatcher.Execute('\testaccount\folder', 'unknowncommand', 'param');

	{Unknown commands should return OK (no-op behavior)}
	Assert.AreEqual(FS_EXEC_OK, Result.ResultCode);
end;

initialization
	TDUnitX.RegisterTestFixture(TCommandResultTest);
	TDUnitX.RegisterTestFixture(TCommandDispatcherTest);

end.
