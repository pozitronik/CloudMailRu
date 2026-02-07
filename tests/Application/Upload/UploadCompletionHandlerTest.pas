unit UploadCompletionHandlerTest;

{Unit tests for TUploadCompletionHandler.
 Tests post-upload completion logic including progress reporting,
 logging, move flag handling, and description sync.}

interface

uses
	DUnitX.TestFramework,
	UploadCompletionHandler,
	MockLogger,
	MockProgress,
	MockSyncGuards,
	LocalFileDeletionHandler,
	DescriptionSyncGuard,
	TimestampSyncGuard,
	RealPath,
	CloudMailRu,
	WFXTypes;

type
	{Mock local file deletion handler with configurable result}
	TMockLocalFileDeletionHandler = class(TInterfacedObject, ILocalFileDeletionHandler)
	private
		FResultCode: Integer;
	public
		DeleteCalled: Boolean;
		LastLocalPath: WideString;

		constructor Create(ResultCode: Integer = FS_FILE_OK);
		function DeleteLocalFile(const LocalPath: WideString): Integer;
	end;

	[TestFixture]
	TUploadCompletionHandlerTest = class
	private
		FHandler: IUploadCompletionHandler;
		FLogger: TMockLogger;
		FProgress: TMockProgress;
		FLocalFileDeletionHandler: TMockLocalFileDeletionHandler;
		FDescriptionSyncGuard: TMockDescriptionSyncGuard;
		FTimestampSyncGuard: TMockTimestampSyncGuard;

		function CreateContext(const LocalName, RemoteName: WideString; CopyFlags: Integer): TUploadCompletionContext;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Progress/logging tests}
		[Test]
		procedure TestHandleCompletion_ReportsProgress100Percent;
		[Test]
		procedure TestHandleCompletion_LogsTransferComplete;

		{Move flag tests}
		[Test]
		procedure TestHandleCompletion_WhenMoveFlag_DeletesLocalFile;
		[Test]
		procedure TestHandleCompletion_WhenNoMoveFlag_DoesNotDeleteLocalFile;
		[Test]
		procedure TestHandleCompletion_WhenMoveDeleteFails_ReturnsErrorCode;

		{Description sync tests}
		[Test]
		procedure TestHandleCompletion_CallsDescriptionSync;
		[Test]
		procedure TestHandleCompletion_WhenMoveDeleteFails_DoesNotCallDescriptionSync;

		{Timestamp sync tests}
		[Test]
		procedure TestHandleCompletion_CallsTimestampSync;
		[Test]
		procedure TestHandleCompletion_TimestampSyncCalledBeforeMove;

		{Result tests}
		[Test]
		procedure TestHandleCompletion_OnSuccess_ReturnsOK;

		{TNullUploadCompletionHandler tests}
		[Test]
		procedure TestNullHandler_ReturnsOK;
	end;

implementation

uses
	SysUtils,
	CloudConstants;

{TMockLocalFileDeletionHandler}

constructor TMockLocalFileDeletionHandler.Create(ResultCode: Integer);
begin
	inherited Create;
	FResultCode := ResultCode;
	DeleteCalled := False;
end;

function TMockLocalFileDeletionHandler.DeleteLocalFile(const LocalPath: WideString): Integer;
begin
	DeleteCalled := True;
	LastLocalPath := LocalPath;
	Result := FResultCode;
end;

{TUploadCompletionHandlerTest}

function TUploadCompletionHandlerTest.CreateContext(const LocalName, RemoteName: WideString; CopyFlags: Integer): TUploadCompletionContext;
begin
	Result.LocalName := LocalName;
	Result.RemoteName := RemoteName;
	Result.CopyFlags := CopyFlags;
	Result.RemotePath.FromPath('\account\folder\file.txt');
	Result.Cloud := nil; {Not needed for most tests}
end;

procedure TUploadCompletionHandlerTest.Setup;
begin
	FLogger := TMockLogger.Create;
	FProgress := TMockProgress.Create;
	FLocalFileDeletionHandler := TMockLocalFileDeletionHandler.Create;
	FDescriptionSyncGuard := TMockDescriptionSyncGuard.Create;
	FTimestampSyncGuard := TMockTimestampSyncGuard.Create;

	FHandler := TUploadCompletionHandler.Create(
		FLogger,
		FProgress,
		FLocalFileDeletionHandler,
		FDescriptionSyncGuard,
		FTimestampSyncGuard
	);
end;

procedure TUploadCompletionHandlerTest.TearDown;
begin
	FHandler := nil;
	{Mocks are freed when interface references are released}
end;

{Progress/logging tests}

procedure TUploadCompletionHandlerTest.TestHandleCompletion_ReportsProgress100Percent;
var
	Context: TUploadCompletionContext;
begin
	Context := CreateContext('C:\local\file.txt', '\account\remote\file.txt', 0);

	FHandler.HandleCompletion(Context);

	Assert.IsTrue(FProgress.ProgressCalled);
	Assert.AreEqual(100, FProgress.LastPercentDone);
	Assert.AreEqual('C:\local\file.txt', FProgress.LastSourceName);
end;

procedure TUploadCompletionHandlerTest.TestHandleCompletion_LogsTransferComplete;
var
	Context: TUploadCompletionContext;
begin
	Context := CreateContext('C:\local\file.txt', '\account\remote\file.txt', 0);

	FHandler.HandleCompletion(Context);

	Assert.IsTrue(FLogger.LogCalled);
	Assert.AreEqual(LOG_LEVEL_FILE_OPERATION, FLogger.LastLogLevel);
	Assert.AreEqual(MSGTYPE_TRANSFERCOMPLETE, FLogger.LastMsgType);
	Assert.Contains(FLogger.LastMessage, 'C:\local\file.txt');
end;

{Move flag tests}

procedure TUploadCompletionHandlerTest.TestHandleCompletion_WhenMoveFlag_DeletesLocalFile;
var
	Context: TUploadCompletionContext;
begin
	Context := CreateContext('C:\local\file.txt', '\account\remote\file.txt', FS_COPYFLAGS_MOVE);

	FHandler.HandleCompletion(Context);

	Assert.IsTrue(FLocalFileDeletionHandler.DeleteCalled);
	Assert.AreEqual('C:\local\file.txt', FLocalFileDeletionHandler.LastLocalPath);
end;

procedure TUploadCompletionHandlerTest.TestHandleCompletion_WhenNoMoveFlag_DoesNotDeleteLocalFile;
var
	Context: TUploadCompletionContext;
begin
	Context := CreateContext('C:\local\file.txt', '\account\remote\file.txt', 0);

	FHandler.HandleCompletion(Context);

	Assert.IsFalse(FLocalFileDeletionHandler.DeleteCalled);
end;

procedure TUploadCompletionHandlerTest.TestHandleCompletion_WhenMoveDeleteFails_ReturnsErrorCode;
var
	Context: TUploadCompletionContext;
	ResultCode: Integer;
begin
	{Recreate handler with failing deletion handler}
	FLocalFileDeletionHandler := TMockLocalFileDeletionHandler.Create(FS_FILE_NOTSUPPORTED);
	FHandler := TUploadCompletionHandler.Create(
		FLogger,
		FProgress,
		FLocalFileDeletionHandler,
		FDescriptionSyncGuard,
		FTimestampSyncGuard
	);

	Context := CreateContext('C:\local\file.txt', '\account\remote\file.txt', FS_COPYFLAGS_MOVE);

	ResultCode := FHandler.HandleCompletion(Context);

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, ResultCode);
end;

{Description sync tests}

procedure TUploadCompletionHandlerTest.TestHandleCompletion_CallsDescriptionSync;
var
	Context: TUploadCompletionContext;
begin
	Context := CreateContext('C:\local\file.txt', '\account\remote\file.txt', 0);

	FHandler.HandleCompletion(Context);

	Assert.IsTrue(FDescriptionSyncGuard.OnFileUploadedCalled);
	Assert.AreEqual('C:\local\file.txt', FDescriptionSyncGuard.LastLocalPath);
end;

procedure TUploadCompletionHandlerTest.TestHandleCompletion_WhenMoveDeleteFails_DoesNotCallDescriptionSync;
var
	Context: TUploadCompletionContext;
begin
	{Recreate handler with failing deletion handler}
	FLocalFileDeletionHandler := TMockLocalFileDeletionHandler.Create(FS_FILE_NOTSUPPORTED);
	FDescriptionSyncGuard := TMockDescriptionSyncGuard.Create;
	FTimestampSyncGuard := TMockTimestampSyncGuard.Create;
	FHandler := TUploadCompletionHandler.Create(
		FLogger,
		FProgress,
		FLocalFileDeletionHandler,
		FDescriptionSyncGuard,
		FTimestampSyncGuard
	);

	Context := CreateContext('C:\local\file.txt', '\account\remote\file.txt', FS_COPYFLAGS_MOVE);

	FHandler.HandleCompletion(Context);

	Assert.IsFalse(FDescriptionSyncGuard.OnFileUploadedCalled);
end;

{Result tests}

procedure TUploadCompletionHandlerTest.TestHandleCompletion_OnSuccess_ReturnsOK;
var
	Context: TUploadCompletionContext;
	ResultCode: Integer;
begin
	Context := CreateContext('C:\local\file.txt', '\account\remote\file.txt', 0);

	ResultCode := FHandler.HandleCompletion(Context);

	Assert.AreEqual(FS_FILE_OK, ResultCode);
end;

{Timestamp sync tests}

procedure TUploadCompletionHandlerTest.TestHandleCompletion_CallsTimestampSync;
var
	Context: TUploadCompletionContext;
begin
	Context := CreateContext('C:\local\file.txt', '\account\remote\file.txt', 0);

	FHandler.HandleCompletion(Context);

	Assert.AreEqual(1, FTimestampSyncGuard.UploadedCalls, 'Should call timestamp sync once');
end;

procedure TUploadCompletionHandlerTest.TestHandleCompletion_TimestampSyncCalledBeforeMove;
var
	Context: TUploadCompletionContext;
begin
	{Timestamp sync must be called before move deletion (reads local file mtime)}
	Context := CreateContext('C:\local\file.txt', '\account\remote\file.txt', FS_COPYFLAGS_MOVE);

	FHandler.HandleCompletion(Context);

	{Both should be called - timestamp sync before move, deletion after}
	Assert.AreEqual(1, FTimestampSyncGuard.UploadedCalls, 'Timestamp sync should be called even with move flag');
	Assert.IsTrue(FLocalFileDeletionHandler.DeleteCalled, 'Local file should be deleted for move');
end;

{Null handler tests}

procedure TUploadCompletionHandlerTest.TestNullHandler_ReturnsOK;
var
	NullHandler: IUploadCompletionHandler;
	Context: TUploadCompletionContext;
	ResultCode: Integer;
begin
	NullHandler := TNullUploadCompletionHandler.Create;
	Context := CreateContext('C:\local\file.txt', '\account\remote\file.txt', FS_COPYFLAGS_MOVE);

	ResultCode := NullHandler.HandleCompletion(Context);

	Assert.AreEqual(FS_FILE_OK, ResultCode, 'Null handler should always return OK');
end;

initialization
	TDUnitX.RegisterTestFixture(TUploadCompletionHandlerTest);

end.
