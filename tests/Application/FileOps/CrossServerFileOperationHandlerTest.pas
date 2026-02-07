unit CrossServerFileOperationHandlerTest;

{Unit tests for TCrossServerFileOperationHandler.
	Tests cross-server file transfers via memory stream.}

interface

uses
	DUnitX.TestFramework,
	SysUtils,
	CloudCallbackTypes,
	CrossServerFileOperationHandler,
	RetryHandler,
	Cipher,
	Logger,
	Progress,
	Request,
	TCHandler,
	RealPath,
	CloudMailRu,
	CloudSettings,
	CloudOAuth,
	MockCloudHTTP,
	MockHTTPManager,
	AuthStrategy,
	FileSystem,
	OpenSSLProvider,
	AccountCredentialsProvider,
	TestHelper;

type
	{Mock retry handler that tracks calls; optionally executes the retry and/or abort callback}
	TMockCrossServerRetryHandler = class(TInterfacedObject, IRetryHandler)
	private
		FHandleErrorCalled: Boolean;
		FAbortCheckCalled: Boolean;
		FReturnValue: Integer;
		FExecuteCallback: Boolean;
		FCallAbortCheck: Boolean;
	public
		constructor Create(ReturnValue: Integer = 0; ExecuteCallback: Boolean = False; CallAbortCheck: Boolean = False);
		function HandleOperationError(
			CurrentResult: Integer;
			OperationType: TRetryOperationType;
			const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
			RetryOperation: TRetryOperation;
			AbortCheck: TAbortCheckFunc
		): Integer;
		property HandleErrorCalled: Boolean read FHandleErrorCalled;
		property AbortCheckCalled: Boolean read FAbortCheckCalled;
	end;

	{Mock logger that tracks log calls}
	TMockCrossServerLogger = class(TInterfacedObject, ILogger)
	private
		FLogCalled: Boolean;
		FLastLogLevel: Integer;
		FLastMessage: WideString;
	public
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
		property LogCalled: Boolean read FLogCalled;
		property LastLogLevel: Integer read FLastLogLevel;
		property LastMessage: WideString read FLastMessage;
	end;

	{Testable CloudMailRu for cross-server tests -- allows setting server profile and OAuth token}
	TCrossServerTestableCloud = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
		procedure SetOAuthToken(const Token: TCloudOAuth);
	end;

	[TestFixture]
	TCrossServerFileOperationHandlerTest = class
	private
		FHandler: ICrossServerFileOperationHandler;
		FMockRetryHandler: TMockCrossServerRetryHandler;
		FMockLogger: TMockCrossServerLogger;
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FOldCloud: TCrossServerTestableCloud;
		FNewCloud: TCrossServerTestableCloud;

		function CreateCloud(const ServerName: WideString = ''): TCrossServerTestableCloud;
		{Configure mock HTTP for full download+upload pipeline through TCloudMailRu}
		procedure SetupTransferMocks(const FileContent: string);
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Overwrite deletes target first}
		[Test]
		procedure TestExecute_Overwrite_DeletesTargetFirst;

		{StatusFile fails}
		[Test]
		procedure TestExecute_StatusFileFails_ReturnsNotSupported;

		{Hash dedup succeeds -- instant copy, no download}
		[Test]
		procedure TestExecute_HashDedupSucceeds_NoDownload;

		{Hash dedup succeeds + move -- deletes source}
		[Test]
		procedure TestExecute_HashDedupSucceeds_Move_DeletesSource;

		{Hash dedup fails -- download + upload via memory stream}
		[Test]
		procedure TestExecute_HashDedupFails_DownloadsAndUploads;

		{Download fails -- returns error}
		[Test]
		procedure TestExecute_DownloadFails_ReturnsError;

		{Upload fails -- calls retry handler}
		[Test]
		procedure TestExecute_UploadFails_CallsRetryHandler;

		{Move succeeds -- deletes source}
		[Test]
		procedure TestExecute_MoveSucceeds_DeletesSource;

		{Move delete source fails -- logs error}
		[Test]
		procedure TestExecute_MoveDeleteSourceFails_LogsError;

		{Full transfer: dedup fails, download+upload succeeds}
		[Test]
		procedure TestExecute_FullTransfer_Success;

		{Full transfer with move: deletes source after download+upload}
		[Test]
		procedure TestExecute_FullTransfer_Move_DeletesSource;

		{Upload fails, retry callback executes and succeeds}
		[Test]
		procedure TestExecute_UploadFails_RetryCallbackExecutesAndSucceeds;

		{Move after successful retry: deletes source}
		[Test]
		procedure TestExecute_MoveAfterRetry_DeletesSource;

		{Move after successful retry: delete source fails, logs error}
		[Test]
		procedure TestExecute_MoveAfterRetry_DeleteFails_LogsError;

		{Upload fails, retry handler invokes AbortCheck callback}
		[Test]
		procedure TestExecute_RetryHandler_CallsAbortCheck;
	end;

implementation

uses
	WFXTypes,
	CloudConstants,
	SettingsConstants,
	LanguageStrings;

const
	{Sample JSON responses}
	JSON_STATUS_FILE =
		'{"email":"test@mail.ru","body":{' +
		'"name":"file.txt","size":1024,"kind":"file","mtime":1234567890,' +
		'"hash":"1234567890123456789012345678901234567890"' +
		'},"status":200}';

	JSON_ADD_FILE_OK =
		'{"email":"test@mail.ru","body":{"hash":"1234567890123456789012345678901234567890"},"status":200}';

	JSON_DELETE_OK =
		'{"email":"test@mail.ru","body":"ok","status":200}';

	{Simulated download -- GetFile writes to stream via MockCloudHTTP.
		For cross-server tests, we only need the shard request to succeed.}
	JSON_SHARD_GET =
		'{"email":"test@mail.ru","body":{"url":"https://cloclo1.cloud.mail.ru/get/"},"status":200}';

{TMockCrossServerRetryHandler}

constructor TMockCrossServerRetryHandler.Create(ReturnValue: Integer; ExecuteCallback: Boolean; CallAbortCheck: Boolean);
begin
	inherited Create;
	FHandleErrorCalled := False;
	FAbortCheckCalled := False;
	FReturnValue := ReturnValue;
	FExecuteCallback := ExecuteCallback;
	FCallAbortCheck := CallAbortCheck;
end;

function TMockCrossServerRetryHandler.HandleOperationError(
	CurrentResult: Integer;
	OperationType: TRetryOperationType;
	const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
	RetryOperation: TRetryOperation;
	AbortCheck: TAbortCheckFunc
): Integer;
begin
	FHandleErrorCalled := True;
	if FCallAbortCheck then
	begin
		AbortCheck();
		FAbortCheckCalled := True;
	end;
	if FExecuteCallback then
		Result := RetryOperation()
	else
		Result := FReturnValue;
end;

{TMockCrossServerLogger}

procedure TMockCrossServerLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	FLogCalled := True;
	FLastLogLevel := LogLevel;
	FLastMessage := LogString;
end;

procedure TMockCrossServerLogger.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
begin
	FLogCalled := True;
	FLastLogLevel := LogLevel;
	FLastMessage := LogString;
end;

{TCrossServerTestableCloud}

procedure TCrossServerTestableCloud.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

procedure TCrossServerTestableCloud.SetOAuthToken(const Token: TCloudOAuth);
begin
	FOAuthToken := Token;
end;

{TCrossServerFileOperationHandlerTest}

procedure TCrossServerFileOperationHandlerTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
	FMockRetryHandler := TMockCrossServerRetryHandler.Create;
	FMockLogger := TMockCrossServerLogger.Create;
	FHandler := TCrossServerFileOperationHandler.Create(FMockRetryHandler, FMockLogger);
	FOldCloud := nil;
	FNewCloud := nil;
end;

procedure TCrossServerFileOperationHandlerTest.TearDown;
begin
	FHandler := nil;
	FOldCloud.Free;
	FNewCloud.Free;
	FMockRetryHandler := nil;
	FMockLogger := nil;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCrossServerFileOperationHandlerTest.CreateCloud(const ServerName: WideString): TCrossServerTestableCloud;
var
	Settings: TCloudSettings;
	OAuthToken: TCloudOAuth;
begin
	Settings := Default(TCloudSettings);
	Settings.AccountSettings.Server := ServerName;
	Result := TCrossServerTestableCloud.Create(
		Settings,
		FMockHTTPManager,
		TestThreadID(),
		TNullAuthStrategy.Create,
		TNullFileSystem.Create,
		TNullLogger.Create,
		TNullProgress.Create,
		TNullRequest.Create,
		TNullTCHandler.Create,
		TNullCipher.Create, TNullOpenSSLProvider.Create, TNullAccountCredentialsProvider.Create);
	Result.SetUnitedParams('api=2&access_token=test_token');
	OAuthToken.access_token := 'test_token';
	OAuthToken.refresh_token := 'test_refresh';
	Result.SetOAuthToken(OAuthToken);
end;

procedure TCrossServerFileOperationHandlerTest.SetupTransferMocks(const FileContent: string);
var
	ContentBytes: TBytes;
begin
	ContentBytes := TEncoding.UTF8.GetBytes(FileContent);
	{OAuth dispatcher returns shard URLs for download and upload}
	FMockHTTP.SetResponse('dispatcher.cloud.mail.ru/d', True, 'https://dl.shard/ 127.0.0.1 1');
	FMockHTTP.SetResponse('dispatcher.cloud.mail.ru/u', True, 'https://ul.shard/ 127.0.0.1 1');
	{Download from shard writes file content to stream}
	FMockHTTP.SetStreamResponse('dl.shard', ContentBytes, FS_FILE_OK);
	{Upload to shard returns 40-char SHA1 hash}
	FMockHTTP.SetPutFileResponse('ul.shard', 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', FS_FILE_OK);
end;

{Tests}

procedure TCrossServerFileOperationHandlerTest.TestExecute_Overwrite_DeletesTargetFirst;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{Delete succeeds, StatusFile succeeds, AddFileByIdentity succeeds}
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_DELETE_OK);
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_FILE_OK);

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, True,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_REMOVE), 'Should delete target when overwrite is true');
end;

procedure TCrossServerFileOperationHandlerTest.TestExecute_StatusFileFails_ReturnsNotSupported;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	FMockHTTP.SetResponse(API_FILE, False, '');

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'Should return not supported when StatusFile fails');
end;

procedure TCrossServerFileOperationHandlerTest.TestExecute_HashDedupSucceeds_NoDownload;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{StatusFile succeeds, AddFileByIdentity succeeds (hash found on destination)}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_FILE_OK);

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(CLOUD_OPERATION_OK, Result, 'Should return OK when hash dedup succeeds');
end;

procedure TCrossServerFileOperationHandlerTest.TestExecute_HashDedupSucceeds_Move_DeletesSource;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{StatusFile succeeds, AddFileByIdentity succeeds, Delete source succeeds}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_FILE_OK);
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_DELETE_OK);

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, True, False, {Move=True}
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_REMOVE), 'Should delete source after successful dedup move');
end;

procedure TCrossServerFileOperationHandlerTest.TestExecute_HashDedupFails_DownloadsAndUploads;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{StatusFile succeeds, dedup fails, download+upload needs full pipeline}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	SetupTransferMocks('cross-server content');
	{Queue /file/add responses: first for dedup (fail), second for post-upload registration (success)}
	FMockHTTP.QueueResponse(API_FILE_ADD, False, '');
	FMockHTTP.QueueResponse(API_FILE_ADD, True, JSON_ADD_FILE_OK);

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(CLOUD_OPERATION_OK, Result, 'Full download+upload transfer should succeed');
end;

procedure TCrossServerFileOperationHandlerTest.TestExecute_DownloadFails_ReturnsError;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{StatusFile succeeds, dedup fails, download shard resolves but GetFile fails}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	FMockHTTP.SetResponse(API_FILE_ADD, False, '');
	FMockHTTP.SetResponse('dispatcher.cloud.mail.ru/d', True, 'https://dl.shard/ 127.0.0.1 1');
	FMockHTTP.SetStreamResponse('dl.shard', nil, FS_FILE_READERROR);

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		function: Boolean begin Result := False; end);

	{Download fails at line 107, Exit skips retry handler (line 117) due to try/finally}
	Assert.AreNotEqual(FS_FILE_OK, Result, 'Should return error when download fails');
	Assert.IsFalse(FMockRetryHandler.HandleErrorCalled, 'Retry handler should not be called for download failures');
end;

procedure TCrossServerFileOperationHandlerTest.TestExecute_UploadFails_CallsRetryHandler;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{StatusFile succeeds, dedup fails, download succeeds, upload PutFile fails}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	FMockHTTP.SetResponse(API_FILE_ADD, False, '');
	FMockHTTP.SetResponse('dispatcher.cloud.mail.ru/d', True, 'https://dl.shard/ 127.0.0.1 1');
	FMockHTTP.SetResponse('dispatcher.cloud.mail.ru/u', True, 'https://ul.shard/ 127.0.0.1 1');
	FMockHTTP.SetStreamResponse('dl.shard', TEncoding.UTF8.GetBytes('content'), FS_FILE_OK);
	FMockHTTP.SetPutFileResponse('ul.shard', '', FS_FILE_WRITEERROR);

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockRetryHandler.HandleErrorCalled,
		'Should call retry handler when upload fails');
end;

procedure TCrossServerFileOperationHandlerTest.TestExecute_MoveSucceeds_DeletesSource;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{Happy path: hash dedup succeeds for move}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_FILE_OK);
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_DELETE_OK);

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, True, False, {Move=True}
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_REMOVE), 'Should delete source on successful move');
end;

procedure TCrossServerFileOperationHandlerTest.TestExecute_MoveDeleteSourceFails_LogsError;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{Hash dedup succeeds but delete source fails}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_FILE_OK);
	FMockHTTP.SetResponse(API_FILE_REMOVE, False, '');

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, True, False, {Move=True}
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockLogger.LogCalled, 'Should log error when delete source fails');
	Assert.AreEqual(LOG_LEVEL_ERROR, FMockLogger.LastLogLevel, 'Should log at error level');
end;

{Full transfer: dedup fails, download+upload pipeline succeeds end-to-end}

procedure TCrossServerFileOperationHandlerTest.TestExecute_FullTransfer_Success;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	SetupTransferMocks('full transfer content');
	{Dedup fails, post-upload registration succeeds}
	FMockHTTP.QueueResponse(API_FILE_ADD, False, '');
	FMockHTTP.QueueResponse(API_FILE_ADD, True, JSON_ADD_FILE_OK);

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(CLOUD_OPERATION_OK, Result, 'Full transfer should succeed');
	Assert.IsFalse(FMockRetryHandler.HandleErrorCalled, 'Retry handler should not be called on success');
end;

{Full transfer with move: download+upload succeeds, then deletes source}

procedure TCrossServerFileOperationHandlerTest.TestExecute_FullTransfer_Move_DeletesSource;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	SetupTransferMocks('move content');
	FMockHTTP.QueueResponse(API_FILE_ADD, False, '');
	FMockHTTP.QueueResponse(API_FILE_ADD, True, JSON_ADD_FILE_OK);
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_DELETE_OK);

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, True, False, {Move=True}
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_REMOVE), 'Should delete source after successful transfer move');
end;

{Upload fails, retry handler executes callback which re-downloads and re-uploads successfully}

procedure TCrossServerFileOperationHandlerTest.TestExecute_UploadFails_RetryCallbackExecutesAndSucceeds;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
	RetryHandler: TMockCrossServerRetryHandler;
begin
	{Create handler with retry callback execution enabled}
	RetryHandler := TMockCrossServerRetryHandler.Create(0, True);
	FHandler := TCrossServerFileOperationHandler.Create(RetryHandler, FMockLogger);
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	{Download shard for both first attempt and retry}
	FMockHTTP.SetResponse('dispatcher.cloud.mail.ru/d', True, 'https://dl.shard/ 127.0.0.1 1');
	FMockHTTP.SetResponse('dispatcher.cloud.mail.ru/u', True, 'https://ul.shard/ 127.0.0.1 1');
	{Download always succeeds (used by both initial attempt and retry callback)}
	FMockHTTP.SetStreamResponse('dl.shard', TEncoding.UTF8.GetBytes('retry content'), FS_FILE_OK);
	{First upload PutFile fails, retry upload PutFile succeeds}
	FMockHTTP.QueuePutFileResponse('ul.shard', '', FS_FILE_WRITEERROR);
	FMockHTTP.QueuePutFileResponse('ul.shard', 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', FS_FILE_OK);
	{Dedup fails, retry AddFileByIdentity succeeds}
	FMockHTTP.QueueResponse(API_FILE_ADD, False, '');
	FMockHTTP.QueueResponse(API_FILE_ADD, True, JSON_ADD_FILE_OK);

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(RetryHandler.HandleErrorCalled, 'Retry handler should be invoked');
	Assert.AreEqual(CLOUD_OPERATION_OK, Result, 'Retry callback should succeed');
end;

{Move after successful retry: deletes source}

procedure TCrossServerFileOperationHandlerTest.TestExecute_MoveAfterRetry_DeletesSource;
var
	OldPath, NewPath: TRealPath;
	RetryHandler: TMockCrossServerRetryHandler;
begin
	RetryHandler := TMockCrossServerRetryHandler.Create(0, True);
	FHandler := TCrossServerFileOperationHandler.Create(RetryHandler, FMockLogger);
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	FMockHTTP.SetResponse('dispatcher.cloud.mail.ru/d', True, 'https://dl.shard/ 127.0.0.1 1');
	FMockHTTP.SetResponse('dispatcher.cloud.mail.ru/u', True, 'https://ul.shard/ 127.0.0.1 1');
	FMockHTTP.SetStreamResponse('dl.shard', TEncoding.UTF8.GetBytes('move retry'), FS_FILE_OK);
	FMockHTTP.QueuePutFileResponse('ul.shard', '', FS_FILE_WRITEERROR);
	FMockHTTP.QueuePutFileResponse('ul.shard', 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', FS_FILE_OK);
	FMockHTTP.QueueResponse(API_FILE_ADD, False, '');
	FMockHTTP.QueueResponse(API_FILE_ADD, True, JSON_ADD_FILE_OK);
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_DELETE_OK);

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, True, False, {Move=True}
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_REMOVE), 'Should delete source after successful retry move');
end;

{Move after successful retry: delete source fails, logs error}

procedure TCrossServerFileOperationHandlerTest.TestExecute_MoveAfterRetry_DeleteFails_LogsError;
var
	OldPath, NewPath: TRealPath;
	RetryHandler: TMockCrossServerRetryHandler;
begin
	RetryHandler := TMockCrossServerRetryHandler.Create(0, True);
	FHandler := TCrossServerFileOperationHandler.Create(RetryHandler, FMockLogger);
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	FMockHTTP.SetResponse('dispatcher.cloud.mail.ru/d', True, 'https://dl.shard/ 127.0.0.1 1');
	FMockHTTP.SetResponse('dispatcher.cloud.mail.ru/u', True, 'https://ul.shard/ 127.0.0.1 1');
	FMockHTTP.SetStreamResponse('dl.shard', TEncoding.UTF8.GetBytes('move retry fail'), FS_FILE_OK);
	FMockHTTP.QueuePutFileResponse('ul.shard', '', FS_FILE_WRITEERROR);
	FMockHTTP.QueuePutFileResponse('ul.shard', 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', FS_FILE_OK);
	FMockHTTP.QueueResponse(API_FILE_ADD, False, '');
	FMockHTTP.QueueResponse(API_FILE_ADD, True, JSON_ADD_FILE_OK);
	FMockHTTP.SetResponse(API_FILE_REMOVE, False, ''); {delete source fails}

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, True, False, {Move=True}
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockLogger.LogCalled, 'Should log error when delete source fails after retry');
	Assert.AreEqual(LOG_LEVEL_ERROR, FMockLogger.LastLogLevel, 'Should log at error level');
end;

{Upload fails, retry handler invokes AbortCheck callback -- covers lines 136-138}

procedure TCrossServerFileOperationHandlerTest.TestExecute_RetryHandler_CallsAbortCheck;
var
	OldPath, NewPath: TRealPath;
	RetryHandler: TMockCrossServerRetryHandler;
begin
	{Create handler with AbortCheck invocation enabled}
	RetryHandler := TMockCrossServerRetryHandler.Create(FS_FILE_WRITEERROR, False, True);
	FHandler := TCrossServerFileOperationHandler.Create(RetryHandler, FMockLogger);
	FOldCloud := CreateCloud('server1');
	FNewCloud := CreateCloud('server2');
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{StatusFile succeeds, dedup fails, download+upload fails to trigger retry}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE);
	FMockHTTP.SetResponse(API_FILE_ADD, False, '');
	SetupTransferMocks('abort check content');
	FMockHTTP.SetPutFileResponse('ul.shard', '', FS_FILE_WRITEERROR);

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(RetryHandler.HandleErrorCalled, 'Retry handler should be called');
	Assert.IsTrue(RetryHandler.AbortCheckCalled, 'AbortCheck callback should be invoked');
end;

initialization
	TDUnitX.RegisterTestFixture(TCrossServerFileOperationHandlerTest);

end.
