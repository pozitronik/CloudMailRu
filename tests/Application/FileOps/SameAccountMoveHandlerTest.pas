unit SameAccountMoveHandlerTest;

{Unit tests for TSameAccountMoveHandler - same-account move/copy operations.
 Tests overwrite handling, skip-path management, and description sync.}

interface

uses
	Classes,
	DUnitX.TestFramework,
	RealPath,
	CloudMailRu,
	CloudSettings,
	Cipher,
	WFXTypes,
	CloudConstants,
	ThreadStateManager,
	DescriptionSyncGuard,
	TimestampSyncGuard,
	SameAccountMoveHandler,
	MockCloudHTTP,
	MockHTTPManager,
	AuthStrategy,
	FileSystem,
	Logger,
	Progress,
	Request,
	TCHandler,
	OpenSSLProvider,
	AccountCredentialsProvider,
	TestHelper;

type
	{Mock thread state manager for skip-path testing}
	TMockMoveThreadState = class(TInterfacedObject, IThreadStateManager)
	private
		FHasSkippedPath: Boolean;
		FSkippedPaths: TStringList;
	public
		constructor Create;
		destructor Destroy; override;

		procedure SetHasSkippedPath(Value: Boolean);
		function GetSkippedPathCount: Integer;
		function HasSkippedPathFor(const Path: WideString): Boolean;

		{IThreadStateManager - skip path methods}
		function HasRemoveDirSkippedPath: Boolean;
		function IsPathSkipped(const Path: WideString): Boolean;
		procedure AddSkippedPath(const Path: WideString);
		procedure RemoveSkippedPath(const Path: WideString);
		procedure CreateRemoveDirSkippedPath;
		procedure ClearRemoveDirSkippedPath;
		function GetRemoveDirSkippedPath: TStringList;

		{Unused methods - minimal implementation}
		function GetSkipListDelete: Boolean;
		procedure SetSkipListDelete(Value: Boolean);
		function GetSkipListRenMov: Boolean;
		procedure SetSkipListRenMov(Value: Boolean);
		function GetCanAbortRenMov: Boolean;
		procedure SetCanAbortRenMov(Value: Boolean);
		function GetListingAborted: Boolean;
		procedure SetListingAborted(Value: Boolean);
		function GetRetryCountDownload: Integer;
		procedure IncrementRetryCountDownload;
		procedure ResetRetryCountDownload;
		function GetRetryCountUpload: Integer;
		procedure IncrementRetryCountUpload;
		procedure ResetRetryCountUpload;
		function GetRetryCountRenMov: Integer;
		procedure IncrementRetryCountRenMov;
		procedure ResetRetryCountRenMov;
		function GetFsStatusInfo: Integer;
		procedure SetFsStatusInfo(Value: Integer);
		procedure RemoveFsStatusInfo;
		procedure SetBackgroundThreadStatus(Value: Integer);
		procedure RemoveBackgroundThread;
		procedure IncrementBackgroundJobs(const Account: WideString);
		procedure DecrementBackgroundJobs(const Account: WideString);
		function HasActiveBackgroundJobs(const Account: WideString): Boolean;
		function HasAnyActiveOperations: Boolean;
	end;

	{Mock description sync guard}
	TMockDescriptionSyncGuard = class(TInterfacedObject, IDescriptionSyncGuard)
	public
		OnFileRenamedCalled: Boolean;
		LastOldPath: TRealPath;
		LastNewPath: TRealPath;

		constructor Create;

		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileDeleted(const Path: TRealPath; Cloud: TCloudMailRu);
		procedure OnDirectoryDeleted(const Path: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
		procedure OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
	end;

	{Mock timestamp sync guard}
	TMockTimestampSyncGuard = class(TInterfacedObject, ITimestampSyncGuard)
	public
		OnFileRenamedCalled: Boolean;
		constructor Create;
		procedure OnFileUploaded(const RemotePath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
		function OnFileDownloaded(const RemotePath: TRealPath; const LocalPath: WideString; CloudMTime: Int64; Cloud: TCloudMailRu): Int64;
		procedure OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
	end;

	{Testable CloudMailRu for same-account operation tests}
	TTestableCloudMailRu = class(TCloudMailRu)
	public
		procedure SetUnitedParams(const Value: WideString);
	end;

	[TestFixture]
	TSameAccountMoveHandlerTest = class
	private
		FHandler: ISameAccountMoveHandler;
		FThreadState: TMockMoveThreadState;
		FDescriptionSyncGuard: TMockDescriptionSyncGuard;
		FTimestampSyncGuard: TMockTimestampSyncGuard;
		FMockHTTP: TMockCloudHTTP;
		FMockHTTPManager: TMockHTTPManager;
		FCloud: TTestableCloudMailRu;

		function CreateCloud: TTestableCloudMailRu;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Move operation tests}
		[Test]
		procedure TestExecute_MoveSuccess_ReturnsOK;
		[Test]
		procedure TestExecute_MoveSuccess_NotifiesDescriptionSync;
		[Test]
		procedure TestExecute_MoveSuccess_NotifiesTimestampSync;
		[Test]
		procedure TestExecute_MoveSuccess_CallsMoveAPI;
		[Test]
		procedure TestExecute_MoveFails_ReturnsError;

		{Copy operation tests}
		[Test]
		procedure TestExecute_CopySuccess_ReturnsOK;
		[Test]
		procedure TestExecute_CopySuccess_DoesNotNotifyDescriptionSync;
		[Test]
		procedure TestExecute_CopySuccess_CallsCopyAPI;

		{Overwrite tests}
		[Test]
		procedure TestExecute_OverwriteTrue_DeletesTargetFirst;
		[Test]
		procedure TestExecute_OverwriteDeleteFails_ReturnsNotSupported;

		{Skip path management tests}
		[Test]
		procedure TestExecute_MoveExists_AddsToSkipPath;
		[Test]
		procedure TestExecute_MoveOK_RemovesFromSkipPath;
		[Test]
		procedure TestExecute_NoSkipPathList_DoesNotAddPath;

		{Issue #219 - Copy with rename target existence check}
		[Test]
		procedure TestExecute_CopyWithRename_TargetExists_ReturnsExists;
		[Test]
		procedure TestExecute_CopyWithRename_TargetExists_DoesNotCallCopyAPI;
		[Test]
		procedure TestExecute_CopySameName_TargetExists_ProceedsWithCopy;
		[Test]
		procedure TestExecute_CopyWithRename_OverwriteTrue_SkipsExistenceCheck;
	end;

implementation

uses
	SysUtils;

const
	{Sample JSON responses for same-account operations}
	JSON_MOVE_SUCCESS = '{"email":"test@mail.ru","body":"/newfolder/file.txt","status":200}';
	JSON_COPY_SUCCESS = '{"email":"test@mail.ru","body":"/newfolder/file.txt","status":200}';
	JSON_RENAME_SUCCESS = '{"email":"test@mail.ru","body":"/folder/newname.txt","status":200}';
	JSON_DELETE_SUCCESS = '{"email":"test@mail.ru","body":"ok","status":200}';
	JSON_OPERATION_EXISTS = '{"email":"test@mail.ru","body":{"home":{"error":"exists"}},"status":400}';
	JSON_OPERATION_FAIL = '{"email":"test@mail.ru","body":{"home":{"error":"not_exists"}},"status":400}';
	{StatusFile response for existing file - used in Issue #219 tests}
	JSON_FILE_EXISTS = '{"email":"test@mail.ru","body":{"name":"existing.txt","kind":"file","type":"file","home":"/folder/existing.txt","size":1024},"status":200}';

{TMockMoveThreadState}

constructor TMockMoveThreadState.Create;
begin
	inherited Create;
	FHasSkippedPath := False;
	FSkippedPaths := TStringList.Create;
end;

destructor TMockMoveThreadState.Destroy;
begin
	FSkippedPaths.Free;
	inherited;
end;

procedure TMockMoveThreadState.SetHasSkippedPath(Value: Boolean);
begin
	FHasSkippedPath := Value;
end;

function TMockMoveThreadState.GetSkippedPathCount: Integer;
begin
	Result := FSkippedPaths.Count;
end;

function TMockMoveThreadState.HasSkippedPathFor(const Path: WideString): Boolean;
begin
	Result := FSkippedPaths.IndexOf(Path) >= 0;
end;

function TMockMoveThreadState.HasRemoveDirSkippedPath: Boolean;
begin
	Result := FHasSkippedPath;
end;

function TMockMoveThreadState.IsPathSkipped(const Path: WideString): Boolean;
begin
	Result := FSkippedPaths.IndexOf(Path) >= 0;
end;

procedure TMockMoveThreadState.AddSkippedPath(const Path: WideString);
begin
	if FSkippedPaths.IndexOf(Path) < 0 then
		FSkippedPaths.Add(Path);
end;

procedure TMockMoveThreadState.RemoveSkippedPath(const Path: WideString);
var
	Idx: Integer;
begin
	Idx := FSkippedPaths.IndexOf(Path);
	if Idx >= 0 then
		FSkippedPaths.Delete(Idx);
end;

procedure TMockMoveThreadState.CreateRemoveDirSkippedPath; begin end;
procedure TMockMoveThreadState.ClearRemoveDirSkippedPath; begin FSkippedPaths.Clear; end;
function TMockMoveThreadState.GetRemoveDirSkippedPath: TStringList; begin Result := FSkippedPaths; end;

{Unused methods}
function TMockMoveThreadState.GetSkipListDelete: Boolean; begin Result := False; end;
procedure TMockMoveThreadState.SetSkipListDelete(Value: Boolean); begin end;
function TMockMoveThreadState.GetSkipListRenMov: Boolean; begin Result := False; end;
procedure TMockMoveThreadState.SetSkipListRenMov(Value: Boolean); begin end;
function TMockMoveThreadState.GetCanAbortRenMov: Boolean; begin Result := False; end;
procedure TMockMoveThreadState.SetCanAbortRenMov(Value: Boolean); begin end;
function TMockMoveThreadState.GetListingAborted: Boolean; begin Result := False; end;
procedure TMockMoveThreadState.SetListingAborted(Value: Boolean); begin end;
function TMockMoveThreadState.GetRetryCountDownload: Integer; begin Result := 0; end;
procedure TMockMoveThreadState.IncrementRetryCountDownload; begin end;
procedure TMockMoveThreadState.ResetRetryCountDownload; begin end;
function TMockMoveThreadState.GetRetryCountUpload: Integer; begin Result := 0; end;
procedure TMockMoveThreadState.IncrementRetryCountUpload; begin end;
procedure TMockMoveThreadState.ResetRetryCountUpload; begin end;
function TMockMoveThreadState.GetRetryCountRenMov: Integer; begin Result := 0; end;
procedure TMockMoveThreadState.IncrementRetryCountRenMov; begin end;
procedure TMockMoveThreadState.ResetRetryCountRenMov; begin end;
function TMockMoveThreadState.GetFsStatusInfo: Integer; begin Result := 0; end;
procedure TMockMoveThreadState.SetFsStatusInfo(Value: Integer); begin end;
procedure TMockMoveThreadState.RemoveFsStatusInfo; begin end;
procedure TMockMoveThreadState.SetBackgroundThreadStatus(Value: Integer); begin end;
procedure TMockMoveThreadState.RemoveBackgroundThread; begin end;
procedure TMockMoveThreadState.IncrementBackgroundJobs(const Account: WideString); begin end;
procedure TMockMoveThreadState.DecrementBackgroundJobs(const Account: WideString); begin end;
function TMockMoveThreadState.HasActiveBackgroundJobs(const Account: WideString): Boolean; begin Result := False; end;
function TMockMoveThreadState.HasAnyActiveOperations: Boolean; begin Result := False; end;

{TMockDescriptionSyncGuard}

constructor TMockDescriptionSyncGuard.Create;
begin
	inherited Create;
	OnFileRenamedCalled := False;
end;

procedure TMockDescriptionSyncGuard.OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
begin
	OnFileRenamedCalled := True;
	LastOldPath := OldPath;
	LastNewPath := NewPath;
end;

procedure TMockDescriptionSyncGuard.OnFileDeleted(const Path: TRealPath; Cloud: TCloudMailRu);
begin
end;

procedure TMockDescriptionSyncGuard.OnDirectoryDeleted(const Path: TRealPath; Cloud: TCloudMailRu);
begin
end;

procedure TMockDescriptionSyncGuard.OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
end;

procedure TMockDescriptionSyncGuard.OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
end;

{TMockTimestampSyncGuard}

constructor TMockTimestampSyncGuard.Create;
begin
	inherited Create;
	OnFileRenamedCalled := False;
end;

procedure TMockTimestampSyncGuard.OnFileUploaded(const RemotePath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
end;

function TMockTimestampSyncGuard.OnFileDownloaded(const RemotePath: TRealPath; const LocalPath: WideString; CloudMTime: Int64; Cloud: TCloudMailRu): Int64;
begin
	Result := 0;
end;

procedure TMockTimestampSyncGuard.OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
begin
end;

procedure TMockTimestampSyncGuard.OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
begin
	OnFileRenamedCalled := True;
end;

{TTestableCloudMailRu}

procedure TTestableCloudMailRu.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

{TSameAccountMoveHandlerTest}

procedure TSameAccountMoveHandlerTest.Setup;
begin
	FMockHTTP := TMockCloudHTTP.Create;
	FMockHTTPManager := TMockHTTPManager.Create(FMockHTTP);
	FThreadState := TMockMoveThreadState.Create;
	FDescriptionSyncGuard := TMockDescriptionSyncGuard.Create;
	FTimestampSyncGuard := TMockTimestampSyncGuard.Create;
	FHandler := TSameAccountMoveHandler.Create(FThreadState, FDescriptionSyncGuard, FTimestampSyncGuard);
	FCloud := nil;
end;

procedure TSameAccountMoveHandlerTest.TearDown;
begin
	FHandler := nil;
	FCloud.Free;
	FTimestampSyncGuard := nil;
	FDescriptionSyncGuard := nil;
	FThreadState := nil;
	FMockHTTPManager := nil;
	FMockHTTP := nil;
end;

function TSameAccountMoveHandlerTest.CreateCloud: TTestableCloudMailRu;
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

{Move operation tests}

procedure TSameAccountMoveHandlerTest.TestExecute_MoveSuccess_ReturnsOK;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');

	{Move uses API_FILE_MOVE when moving to different folder}
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_MOVE_SUCCESS);

	Result := FHandler.Execute(FCloud, OldPath, NewPath, True, False);

	Assert.AreEqual(FS_FILE_OK, Result, 'Should return OK on successful move');
end;

procedure TSameAccountMoveHandlerTest.TestExecute_MoveSuccess_NotifiesTimestampSync;
var
	OldPath, NewPath: TRealPath;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');

	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_MOVE_SUCCESS);

	FHandler.Execute(FCloud, OldPath, NewPath, True, False);

	Assert.IsTrue(FTimestampSyncGuard.OnFileRenamedCalled,
		'Should notify timestamp sync on successful move');
end;

procedure TSameAccountMoveHandlerTest.TestExecute_MoveSuccess_NotifiesDescriptionSync;
var
	OldPath, NewPath: TRealPath;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');

	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_MOVE_SUCCESS);

	FHandler.Execute(FCloud, OldPath, NewPath, True, False);

	Assert.IsTrue(FDescriptionSyncGuard.OnFileRenamedCalled,
		'Should notify description sync on successful move');
end;

procedure TSameAccountMoveHandlerTest.TestExecute_MoveSuccess_CallsMoveAPI;
var
	OldPath, NewPath: TRealPath;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');

	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_MOVE_SUCCESS);

	FHandler.Execute(FCloud, OldPath, NewPath, True, False);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_MOVE), 'Should call move API');
end;

procedure TSameAccountMoveHandlerTest.TestExecute_MoveFails_ReturnsError;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');

	{Move API fails}
	FMockHTTP.SetResponse(API_FILE_MOVE, False, '');

	Result := FHandler.Execute(FCloud, OldPath, NewPath, True, False);

	Assert.AreNotEqual(FS_FILE_OK, Result, 'Should return error when move fails');
end;

{Copy operation tests}

procedure TSameAccountMoveHandlerTest.TestExecute_CopySuccess_ReturnsOK;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');

	{Copy uses API_FILE_COPY}
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_COPY_SUCCESS);

	Result := FHandler.Execute(FCloud, OldPath, NewPath, False, False);

	Assert.AreEqual(FS_FILE_OK, Result, 'Should return OK on successful copy');
end;

procedure TSameAccountMoveHandlerTest.TestExecute_CopySuccess_DoesNotNotifyDescriptionSync;
var
	OldPath, NewPath: TRealPath;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');

	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_COPY_SUCCESS);

	FHandler.Execute(FCloud, OldPath, NewPath, False, False);

	Assert.IsFalse(FDescriptionSyncGuard.OnFileRenamedCalled,
		'Should NOT notify description sync on copy (only moves)');
end;

procedure TSameAccountMoveHandlerTest.TestExecute_CopySuccess_CallsCopyAPI;
var
	OldPath, NewPath: TRealPath;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');

	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_COPY_SUCCESS);

	FHandler.Execute(FCloud, OldPath, NewPath, False, False);

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_COPY), 'Should call copy API');
end;

{Overwrite tests}

procedure TSameAccountMoveHandlerTest.TestExecute_OverwriteTrue_DeletesTargetFirst;
var
	OldPath, NewPath: TRealPath;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');

	{Delete succeeds, then move succeeds}
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_DELETE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_MOVE_SUCCESS);

	FHandler.Execute(FCloud, OldPath, NewPath, True, True); {OverWrite=True}

	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_REMOVE),
		'Should delete target first when overwrite is true');
end;

procedure TSameAccountMoveHandlerTest.TestExecute_OverwriteDeleteFails_ReturnsNotSupported;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');

	{Delete fails}
	FMockHTTP.SetResponse(API_FILE_REMOVE, False, '');

	Result := FHandler.Execute(FCloud, OldPath, NewPath, True, True); {OverWrite=True}

	Assert.AreEqual(FS_FILE_NOTSUPPORTED, Result,
		'Should return not supported when delete fails during overwrite');
end;

{Skip path management tests}

procedure TSameAccountMoveHandlerTest.TestExecute_MoveExists_AddsToSkipPath;
var
	OldPath, NewPath: TRealPath;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');
	FThreadState.SetHasSkippedPath(True);

	{Move returns EXISTS error}
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_OPERATION_EXISTS);

	FHandler.Execute(FCloud, OldPath, NewPath, True, False);

	Assert.IsTrue(FThreadState.HasSkippedPathFor(OldPath.ToPath),
		'Should add path to skip list when move returns EXISTS');
end;

procedure TSameAccountMoveHandlerTest.TestExecute_MoveOK_RemovesFromSkipPath;
var
	OldPath, NewPath: TRealPath;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');
	FThreadState.SetHasSkippedPath(True);
	FThreadState.AddSkippedPath(OldPath.ToPath);

	{Move succeeds}
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_MOVE_SUCCESS);

	FHandler.Execute(FCloud, OldPath, NewPath, True, False);

	Assert.IsFalse(FThreadState.HasSkippedPathFor(OldPath.ToPath),
		'Should remove path from skip list when move succeeds');
end;

procedure TSameAccountMoveHandlerTest.TestExecute_NoSkipPathList_DoesNotAddPath;
var
	OldPath, NewPath: TRealPath;
begin
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\newfolder\file.txt');
	FThreadState.SetHasSkippedPath(False); {No skip path list}

	{Move returns EXISTS error but should not add to skip path}
	FMockHTTP.SetResponse(API_FILE_MOVE, True, JSON_OPERATION_EXISTS);

	FHandler.Execute(FCloud, OldPath, NewPath, True, False);

	Assert.AreEqual(0, FThreadState.GetSkippedPathCount,
		'Should not add to skip list when feature is disabled');
end;

{Issue #219 - Copy with rename target existence check}

procedure TSameAccountMoveHandlerTest.TestExecute_CopyWithRename_TargetExists_ReturnsExists;
var
	OldPath, NewPath: TRealPath;
	Result: Integer;
begin
	{Issue #219: Copy source.txt to target.txt when target.txt exists should return EXISTS,
		not proceed with copy+rename which leaves orphaned intermediate file.
		Note: Paths must be in different directories since same-dir copy is unsupported.}
	FCloud := CreateCloud;
	OldPath.FromPath('\account\srcfolder\source.txt');
	NewPath.FromPath('\account\dstfolder\target.txt'); {Different dir + different filename = rename needed}

	{StatusFile returns true - target file exists}
	FMockHTTP.SetResponse(API_FILE, True, JSON_FILE_EXISTS);

	Result := FHandler.Execute(FCloud, OldPath, NewPath, False, False); {Copy, no overwrite}

	Assert.AreEqual(FS_FILE_EXISTS, Result,
		'Should return EXISTS when copy target exists and overwrite is false');
end;

procedure TSameAccountMoveHandlerTest.TestExecute_CopyWithRename_TargetExists_DoesNotCallCopyAPI;
var
	OldPath, NewPath: TRealPath;
begin
	{Issue #219: When target exists and overwrite is false, should not call copy API.
		This prevents creating orphaned intermediate files.
		Note: Paths must be in different directories since same-dir copy is unsupported.}
	FCloud := CreateCloud;
	OldPath.FromPath('\account\srcfolder\source.txt');
	NewPath.FromPath('\account\dstfolder\target.txt'); {Different dir + different filename = rename needed}

	{StatusFile returns true - target file exists}
	FMockHTTP.SetResponse(API_FILE, True, JSON_FILE_EXISTS);

	FHandler.Execute(FCloud, OldPath, NewPath, False, False); {Copy, no overwrite}

	Assert.IsFalse(FMockHTTP.WasURLCalled(API_FILE_COPY),
		'Should NOT call copy API when target exists - prevents orphaned files');
end;

procedure TSameAccountMoveHandlerTest.TestExecute_CopySameName_TargetExists_ProceedsWithCopy;
var
	OldPath, NewPath: TRealPath;
begin
	{When copying to different folder with SAME filename, no rename is needed.
		StatusFile check is skipped because cloud API handles this case correctly.}
	FCloud := CreateCloud;
	OldPath.FromPath('\account\folder\file.txt');
	NewPath.FromPath('\account\otherfolder\file.txt'); {Same filename, different folder}

	{Copy succeeds - no StatusFile check needed for same-name copies}
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_COPY_SUCCESS);

	FHandler.Execute(FCloud, OldPath, NewPath, False, False); {Copy, no overwrite}

	{StatusFile uses 'api/v2/file?' pattern - check this specific endpoint was NOT called.
		Note: Using 'file?' suffix to avoid matching file/copy, file/move, etc.}
	Assert.IsFalse(FMockHTTP.WasURLCalled('api/v2/file?'),
		'Should not check target existence when filenames match');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_COPY),
		'Should proceed with copy when filenames match');
end;

procedure TSameAccountMoveHandlerTest.TestExecute_CopyWithRename_OverwriteTrue_SkipsExistenceCheck;
var
	OldPath, NewPath: TRealPath;
begin
	{When overwrite is true, we want to overwrite anyway, so existence check is skipped.
		Note: Paths must be in different directories since same-dir copy is unsupported.}
	FCloud := CreateCloud;
	OldPath.FromPath('\account\srcfolder\source.txt');
	NewPath.FromPath('\account\dstfolder\target.txt'); {Different dir + different filename = rename needed}

	{Delete succeeds, then copy succeeds, then rename succeeds}
	FMockHTTP.SetResponse(API_FILE_REMOVE, True, JSON_DELETE_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_COPY, True, JSON_COPY_SUCCESS);
	FMockHTTP.SetResponse(API_FILE_RENAME, True, JSON_RENAME_SUCCESS);

	FHandler.Execute(FCloud, OldPath, NewPath, False, True); {Copy, with overwrite}

	{StatusFile uses 'api/v2/file?' pattern - check this specific endpoint was NOT called.
		Note: Using 'file?' suffix to avoid matching file/copy, file/move, etc.}
	Assert.IsFalse(FMockHTTP.WasURLCalled('api/v2/file?'),
		'Should not check target existence when overwrite is true');
	Assert.IsTrue(FMockHTTP.WasURLCalled(API_FILE_COPY),
		'Should proceed with copy when overwrite is true');
end;

initialization
	TDUnitX.RegisterTestFixture(TSameAccountMoveHandlerTest);

end.
