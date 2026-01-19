unit UploadCompletionHandlerTest;

{Unit tests for TUploadCompletionHandler.
 Tests post-upload completion logic including progress reporting,
 logging, move flag handling, and description sync.}

interface

uses
	DUnitX.TestFramework,
	UploadCompletionHandler,
	ILoggerInterface,
	IProgressInterface,
	ILocalFileDeletionHandlerInterface,
	IDescriptionSyncGuardInterface,
	RealPath,
	CloudMailRu,
	PLUGIN_TYPES;

type
	{Mock logger that tracks Log calls}
	TMockLogger = class(TInterfacedObject, ILogger)
	public
		LogCalled: Boolean;
		LastLogLevel: Integer;
		LastMsgType: Integer;
		LastMessage: WideString;

		procedure Log(LogLevel, MsgType: Integer; LogString: WideString); overload;
		procedure Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const); overload;
	end;

	{Mock progress that tracks Progress calls}
	TMockProgress = class(TInterfacedObject, IProgress)
	public
		ProgressCalled: Boolean;
		LastSourceName: WideString;
		LastTargetName: WideString;
		LastPercentDone: Integer;

		function Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(SourceName: WideString; PercentDone: Integer): Boolean; overload;
		function Progress(PercentDone: Integer): Boolean; overload;
		function Aborted(): Boolean;
	end;

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

	{Mock description sync guard that tracks OnFileUploaded calls}
	TMockDescriptionSyncGuard = class(TInterfacedObject, IDescriptionSyncGuard)
	public
		OnFileUploadedCalled: Boolean;
		LastRemotePath: TRealPath;
		LastLocalPath: WideString;

		procedure OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
		procedure OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
	end;

	[TestFixture]
	TUploadCompletionHandlerTest = class
	private
		FHandler: IUploadCompletionHandler;
		FLogger: TMockLogger;
		FProgress: TMockProgress;
		FLocalFileDeletionHandler: TMockLocalFileDeletionHandler;
		FDescriptionSyncGuard: TMockDescriptionSyncGuard;

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

		{Result tests}
		[Test]
		procedure TestHandleCompletion_OnSuccess_ReturnsOK;
	end;

implementation

uses
	SysUtils,
	CMRConstants;

{TMockLogger}

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString);
begin
	LogCalled := True;
	LastLogLevel := LogLevel;
	LastMsgType := MsgType;
	LastMessage := LogString;
end;

procedure TMockLogger.Log(LogLevel, MsgType: Integer; LogString: WideString; const Args: array of const);
begin
	LogCalled := True;
	LastLogLevel := LogLevel;
	LastMsgType := MsgType;
	LastMessage := Format(LogString, Args);
end;

{TMockProgress}

function TMockProgress.Progress(SourceName, TargetName: WideString; PercentDone: Integer): Boolean;
begin
	ProgressCalled := True;
	LastSourceName := SourceName;
	LastTargetName := TargetName;
	LastPercentDone := PercentDone;
	Result := False; {Not aborted}
end;

function TMockProgress.Progress(SourceName: WideString; PercentDone: Integer): Boolean;
begin
	Result := Progress(SourceName, '', PercentDone);
end;

function TMockProgress.Progress(PercentDone: Integer): Boolean;
begin
	Result := Progress('', '', PercentDone);
end;

function TMockProgress.Aborted: Boolean;
begin
	Result := False;
end;

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

{TMockDescriptionSyncGuard}

procedure TMockDescriptionSyncGuard.OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
begin
	{Not used in upload tests}
end;

procedure TMockDescriptionSyncGuard.OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
begin
	{Not used in upload tests}
end;

procedure TMockDescriptionSyncGuard.OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
	{Not used in upload tests}
end;

procedure TMockDescriptionSyncGuard.OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
	OnFileUploadedCalled := True;
	LastRemotePath := RealPath;
	LastLocalPath := LocalPath;
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

	FHandler := TUploadCompletionHandler.Create(
		FLogger,
		FProgress,
		FLocalFileDeletionHandler,
		FDescriptionSyncGuard
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
		FDescriptionSyncGuard
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
	FHandler := TUploadCompletionHandler.Create(
		FLogger,
		FProgress,
		FLocalFileDeletionHandler,
		FDescriptionSyncGuard
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

initialization
	TDUnitX.RegisterTestFixture(TUploadCompletionHandlerTest);

end.
