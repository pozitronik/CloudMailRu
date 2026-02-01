unit CrossAccountFileOperationHandlerTest;

{Unit tests for TCrossAccountFileOperationHandler.
 Tests cross-account file operations including ViaHash and ViaPublicLink modes.}

interface

uses
	DUnitX.TestFramework,
	SysUtils,
	CloudCallbackTypes,
	CrossAccountFileOperationHandler,
	RetryHandler,
	Cipher,
	Logger,
	Progress,
	Request,
	TCHandler,
	RealPath,
	CloudMailRu,
	CloudSettings,
	MockCloudHTTP,
	MockHTTPManager,
	AuthStrategy,
	FileSystem,
	OpenSSLProvider,
	AccountCredentialsProvider,
	TestHelper;

type
	{Mock retry handler that tracks calls}
	TMockRetryHandler = class(TInterfacedObject, IRetryHandler)
	private
		FHandleErrorCalled: Boolean;
		FReturnValue: Integer;
	public
		constructor Create(ReturnValue: Integer = 0);
		function HandleOperationError(
			CurrentResult: Integer;
			OperationType: TRetryOperationType;
			const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
			RetryOperation: TRetryOperation;
			AbortCheck: TAbortCheckFunc
		): Integer;
		property HandleErrorCalled: Boolean read FHandleErrorCalled;
	end;

	{Mock logger that tracks log calls}
	TMockLogger = class(TInterfacedObject, ILogger)
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

	{Testable CloudMailRu for cross-account operation tests}
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
	end;

	[TestFixture]
	TCrossAccountFileOperationHandlerTest = class
	private
		FHandler: ICrossAccountFileOperationHandler;
		FMockRetryHandler: TMockRetryHandler;
		FMockLogger: TMockLogger;
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FOldCloud: TTestableCloudMailRu;
		FNewCloud: TTestableCloudMailRu;

		function CreateCloud: TTestableCloudMailRu;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Public account validation tests}
		[Test]
		procedure TestExecute_PublicAccount_ReturnsUserAbort;
		[Test]
		procedure TestExecute_PublicAccount_LogsWarning;

		{Disabled mode tests}
		[Test]
		procedure TestExecute_DisabledMode_ReturnsUserAbort;
		[Test]
		procedure TestExecute_DisabledMode_LogsWarning;

		{Invalid mode tests}
		[Test]
		procedure TestExecute_InvalidMode_ReturnsWriteError;

		{ViaHash mode tests}
		[Test]
		procedure TestExecute_ViaHash_StatusFileFails_ReturnsNotSupported;
		[Test]
		procedure TestExecute_ViaHash_AddByIdentitySucceeds_ReturnsOK;
		[Test]
		procedure TestExecute_ViaHash_WithOverwrite_DeletesTargetFirst;
		[Test]
		procedure TestExecute_ViaHash_OverwriteDeleteFails_ReturnsNotSupported;
		[Test]
		procedure TestExecute_ViaHash_MoveSucceeds_DeletesSource;
		[Test]
		procedure TestExecute_ViaHash_MoveDeleteSourceFails_LogsError;
		[Test]
		procedure TestExecute_ViaHash_AddByIdentityFails_CallsRetryHandler;

		{ViaPublicLink mode tests}
		[Test]
		procedure TestExecute_ViaPublicLink_StatusFileFails_ReturnsNotSupported;
		[Test]
		procedure TestExecute_ViaPublicLink_AlreadyPublished_ClonesDirectly;
		[Test]
		procedure TestExecute_ViaPublicLink_NotPublished_PublishesFirst;
		[Test]
		procedure TestExecute_ViaPublicLink_PublishFails_ReturnsReadError;
		[Test]
		procedure TestExecute_ViaPublicLink_OverwriteDeleteFails_ReturnsNotSupported;
		[Test]
		procedure TestExecute_ViaPublicLink_CloneSucceeds_ReturnsOK;
		[Test]
		procedure TestExecute_ViaPublicLink_CloneFails_CallsRetryHandler;
		[Test]
		procedure TestExecute_ViaPublicLink_TempPublished_UnpublishesAfter;
		[Test]
		procedure TestExecute_ViaPublicLink_UnpublishFails_LogsError;
		[Test]
		procedure TestExecute_ViaPublicLink_MoveDeleteSourceFails_LogsError;
	end;

implementation

uses
	WFXTypes,
	CloudConstants,
	SettingsConstants,
	LanguageStrings;

const
	{Sample JSON responses for cross-account operations}
	JSON_STATUS_FILE_SUCCESS =
		'{"email":"test@mail.ru","body":{' +
		'"name":"file.txt","size":1024,"kind":"file","mtime":1234567890,' +
		'"hash":"1234567890123456789012345678901234567890"' +
		'},"status":200}';

	JSON_STATUS_FILE_PUBLISHED =
		'{"email":"test@mail.ru","body":{' +
		'"name":"file.txt","size":1024,"kind":"file","mtime":1234567890,' +
		'"hash":"1234567890123456789012345678901234567890",' +
		'"weblink":"abc123public"' +
		'},"status":200}';

	JSON_STATUS_FILE_NOT_EXISTS =
		'{"email":"test@mail.ru","body":{"home":{"error":"not_exists"}},"status":400}';

	JSON_ADD_FILE_SUCCESS =
		'{"email":"test@mail.ru","body":{"hash":"1234567890123456789012345678901234567890"},"status":200}';

	JSON_ADD_FILE_EXISTS =
		'{"email":"test@mail.ru","body":{"home":{"error":"exists"}},"status":400}';

	JSON_DELETE_SUCCESS =
		'{"email":"test@mail.ru","body":"ok","status":200}';

	JSON_PUBLISH_SUCCESS =
		'{"email":"test@mail.ru","body":"abc123public","status":200}';

	JSON_CLONE_SUCCESS =
		'{"email":"test@mail.ru","body":"/cloned_file.txt","status":200}';

	JSON_CLONE_FAIL =
		'{"email":"test@mail.ru","body":{"home":{"error":"not_exists"}},"status":400}';

{TMockRetryHandler}

constructor TMockRetryHandler.Create(ReturnValue: Integer);
begin
	inherited Create;
	FHandleErrorCalled := False;
	FReturnValue := ReturnValue;
end;

function TMockRetryHandler.HandleOperationError(
	CurrentResult: Integer;
	OperationType: TRetryOperationType;
	const AskMessage, AskTitle, RetryLogMessage, FormatParam: WideString;
	RetryOperation: TRetryOperation;
	AbortCheck: TAbortCheckFunc
): Integer;
begin
	FHandleErrorCalled := True;
	Result := FReturnValue;
end;

{TMockLogger}

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	FLogCalled := True;
	FLastLogLevel := LogLevel;
	FLastMessage := LogString;
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
begin
	FLogCalled := True;
	FLastLogLevel := LogLevel;
	FLastMessage := LogString;
end;

{TTestableCloudMailRu}

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

{TCrossAccountFileOperationHandlerTest}

procedure TCrossAccountFileOperationHandlerTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
	FMockRetryHandler := TMockRetryHandler.Create;
	FMockLogger := TMockLogger.Create;
	FHandler := TCrossAccountFileOperationHandler.Create(FMockRetryHandler, FMockLogger);
	FOldCloud := nil;
	FNewCloud := nil;
end;

procedure TCrossAccountFileOperationHandlerTest.TearDown;
begin
	FHandler := nil;
	FOldCloud.Free;
	FNewCloud.Free;
	FMockRetryHandler := nil;
	FMockLogger := nil;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TCrossAccountFileOperationHandlerTest.CreateCloud: TTestableCloudMailRu;
var
	Settings: TCloudSettings;
begin
	Settings := Default(TCloudSettings);
	Result := TTestableCloudMailRu.Create(
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
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_PublicAccount_ReturnsUserAbort;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	Result := FHandler.Execute(nil, nil, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaHash, True, {IsSourcePublicAccount}
		function: Boolean begin Result := False; end);

	Assert.AreEqual(FS_FILE_USERABORT, Result, 'Should return user abort for public account');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_PublicAccount_LogsWarning;
var
	OldPath, NewPath: TRealPath;
begin
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	FHandler.Execute(nil, nil, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaHash, True, {IsSourcePublicAccount}
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockLogger.LogCalled, 'Should log warning');
	Assert.AreEqual(LOG_LEVEL_WARNING, FMockLogger.LastLogLevel, 'Should log at warning level');
	Assert.AreEqual(ERR_DIRECT_OPERATIONS_NOT_SUPPORTED, FMockLogger.LastMessage, 'Should log correct message');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_DisabledMode_ReturnsUserAbort;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	Result := FHandler.Execute(nil, nil, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeDisabled, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(FS_FILE_USERABORT, Result, 'Should return user abort for disabled mode');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_DisabledMode_LogsWarning;
var
	OldPath, NewPath: TRealPath;
begin
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	FHandler.Execute(nil, nil, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeDisabled, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockLogger.LogCalled, 'Should log warning');
	Assert.AreEqual(LOG_LEVEL_WARNING, FMockLogger.LastLogLevel, 'Should log at warning level');
	Assert.AreEqual(ERR_DIRECT_OPERATIONS_DISABLED, FMockLogger.LastMessage, 'Should log correct message');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_InvalidMode_ReturnsWriteError;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	Result := FHandler.Execute(nil, nil, OldPath, NewPath, False, False,
		999, {invalid mode}
		False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(FS_FILE_WRITEERROR, Result, 'Should return write error for invalid mode');
end;

{ViaHash mode tests}

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaHash_StatusFileFails_ReturnsNotSupported;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{StatusFile fails - file not found}
	FMockHTTP.SetResponse(API_FILE, False, '');

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaHash, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'Should return not supported when statusFile fails');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaHash_AddByIdentitySucceeds_ReturnsOK;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{StatusFile succeeds, AddFileByIdentity succeeds}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_FILE_SUCCESS);

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaHash, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(CLOUD_OPERATION_OK, Result, 'Should return OK when addByIdentity succeeds');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaHash_WithOverwrite_DeletesTargetFirst;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{Delete succeeds, StatusFile succeeds, AddFileByIdentity succeeds}
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_DELETE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_FILE_SUCCESS);

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, True, {OverWrite=True}
		CopyBetweenAccountsModeViaHash, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_REMOVE), 'Should call delete API when overwrite is true');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaHash_MoveSucceeds_DeletesSource;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{StatusFile succeeds, AddFileByIdentity succeeds, Delete source succeeds}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_FILE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_DELETE_SUCCESS);

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, True, False, {Move=True}
		CopyBetweenAccountsModeViaHash, False,
		function: Boolean begin Result := False; end);

	{Delete should be called to remove source after successful copy}
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_REMOVE), 'Should delete source after successful move');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaHash_OverwriteDeleteFails_ReturnsNotSupported;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{Delete target fails during overwrite}
	FMockHTTP.SetResponse(API_FILE_REMOVE, False, '');

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, True, {OverWrite=True}
		CopyBetweenAccountsModeViaHash, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'Should return not supported when overwrite delete fails');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaHash_MoveDeleteSourceFails_LogsError;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{StatusFile succeeds, AddFileByIdentity succeeds, but Delete source fails}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_ADD, True, JSON_ADD_FILE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_REMOVE, False, '');

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, True, False, {Move=True}
		CopyBetweenAccountsModeViaHash, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockLogger.LogCalled, 'Should log error when delete source fails');
	Assert.AreEqual(LOG_LEVEL_ERROR, FMockLogger.LastLogLevel, 'Should log at error level');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaHash_AddByIdentityFails_CallsRetryHandler;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{StatusFile succeeds, but AddFileByIdentity fails with error}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_ADD, False, '');

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaHash, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockRetryHandler.HandleErrorCalled, 'Should call retry handler when addByIdentity fails');
end;

{ViaPublicLink mode tests}

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaPublicLink_StatusFileFails_ReturnsNotSupported;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{StatusFile fails}
	FMockHTTP.SetResponse(API_FILE, False, '');

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaPublicLink, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'Should return not supported when statusFile fails');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaPublicLink_AlreadyPublished_ClonesDirectly;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{File is already published - should not call publish API}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_PUBLISHED);
	FMockHTTP.SetResponse(API_CLONE, True, JSON_CLONE_SUCCESS);

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaPublicLink, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_CLONE), 'Should call clone API');
	Assert.IsFalse(FMockHTTP.WasURLCalled(API_FILE_PUBLISH), 'Should not call publish API for already published file');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaPublicLink_NotPublished_PublishesFirst;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{File is not published - should call publish API first}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_PUBLISH, True, JSON_PUBLISH_SUCCESS);
	FMockHTTP.SetResponse(API_CLONE, True, JSON_CLONE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_UNPUBLISH, True, JSON_DELETE_SUCCESS);

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaPublicLink, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_PUBLISH), 'Should call publish API for unpublished file');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_CLONE), 'Should call clone API');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaPublicLink_PublishFails_ReturnsReadError;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{File is not published and publish fails}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_PUBLISH, False, '');

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaPublicLink, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(FS_FILE_READERROR, Result, 'Should return read error when publish fails');
	Assert.IsTrue(FMockLogger.LogCalled, 'Should log error when publish fails');
	Assert.AreEqual(LOG_LEVEL_ERROR, FMockLogger.LastLogLevel, 'Should log at error level');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaPublicLink_OverwriteDeleteFails_ReturnsNotSupported;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{Delete target fails during overwrite}
	FMockHTTP.SetResponse(API_FILE_REMOVE, False, '');

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, True, {OverWrite=True}
		CopyBetweenAccountsModeViaPublicLink, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result, 'Should return not supported when overwrite delete fails');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaPublicLink_CloneSucceeds_ReturnsOK;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{Clone operation succeeds}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_PUBLISHED);
	FMockHTTP.SetResponse(API_CLONE, True, JSON_CLONE_SUCCESS);

	Result := FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaPublicLink, False,
		function: Boolean begin Result := False; end);

	Assert.AreEqual(CLOUD_OPERATION_OK, Result, 'Should return OK when clone succeeds');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaPublicLink_CloneFails_CallsRetryHandler;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{Clone operation fails}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_PUBLISHED);
	FMockHTTP.SetResponse(API_CLONE, False, '');

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaPublicLink, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockRetryHandler.HandleErrorCalled, 'Should call retry handler when clone fails');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaPublicLink_TempPublished_UnpublishesAfter;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{File was not published, so temp link should be removed after clone}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_PUBLISH, True, JSON_PUBLISH_SUCCESS);
	FMockHTTP.SetResponse(API_CLONE, True, JSON_CLONE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_UNPUBLISH, True, JSON_DELETE_SUCCESS);

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaPublicLink, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_UNPUBLISH), 'Should call unpublish API to remove temp link');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaPublicLink_UnpublishFails_LogsError;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{File was not published, clone succeeds, but unpublish fails}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_PUBLISH, True, JSON_PUBLISH_SUCCESS);
	FMockHTTP.SetResponse(API_CLONE, True, JSON_CLONE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_UNPUBLISH, False, '');

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, False, False,
		CopyBetweenAccountsModeViaPublicLink, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockLogger.LogCalled, 'Should log error when unpublish fails');
	Assert.AreEqual(LOG_LEVEL_ERROR, FMockLogger.LastLogLevel, 'Should log at error level');
end;

procedure TCrossAccountFileOperationHandlerTest.TestExecute_ViaPublicLink_MoveDeleteSourceFails_LogsError;
var
	OldPath, NewPath: TRealPath;
begin
	FOldCloud := CreateCloud;
	FNewCloud := CreateCloud;
	OldPath.FromPath('\account1\file.txt');
	NewPath.FromPath('\account2\file.txt');

	{Clone succeeds but delete source fails during move}
	FMockHTTP.SetResponse(API_FILE, True, JSON_STATUS_FILE_PUBLISHED);
	FMockHTTP.SetResponse(API_CLONE, True, JSON_CLONE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_REMOVE, False, '');

	FHandler.Execute(FOldCloud, FNewCloud, OldPath, NewPath, True, False, {Move=True}
		CopyBetweenAccountsModeViaPublicLink, False,
		function: Boolean begin Result := False; end);

	Assert.IsTrue(FMockLogger.LogCalled, 'Should log error when delete source fails');
	Assert.AreEqual(LOG_LEVEL_ERROR, FMockLogger.LastLogLevel, 'Should log at error level');
end;

initialization
	TDUnitX.RegisterTestFixture(TCrossAccountFileOperationHandlerTest);

end.
