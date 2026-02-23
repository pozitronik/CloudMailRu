unit TrashBinOperationHandlerTest;

{Unit tests for TTrashBinOperationHandler using mock ICloudListingService.}

interface

uses
	DUnitX.TestFramework,
	Windows,
	CloudDirItem,
	CloudDirItemList,
	CloudIncomingInviteList,
	CloudSpace,
	CloudConstants,
	CloudFileVersion,
	CloudListingService,
	TrashBinOperationHandler;

type
	{Mock ICloudListingService for testing TrashBinOperationHandler}
	TMockListingService = class(TInterfacedObject, ICloudListingService)
	private
		FTrashbinEmptyResult: Boolean;
		FTrashbinRestoreResult: Boolean;
		FTrashbinEmptyCalled: Boolean;
		FTrashbinRestoreCalls: Integer;
		FLastRestorePath: WideString;
		FLastRestoreRev: Integer;
	public
		constructor Create;

		{Mock configuration}
		procedure SetTrashbinEmptyResult(Value: Boolean);
		procedure SetTrashbinRestoreResult(Value: Boolean);

		{Mock verification}
		function WasTrashbinEmptyCalled: Boolean;
		function GetTrashbinRestoreCalls: Integer;
		function GetLastRestorePath: WideString;
		function GetLastRestoreRev: Integer;

		{ICloudListingService implementation}
		function GetDirectory(Path: WideString; var Listing: TCloudDirItemList; ShowProgress: Boolean = False): Boolean;
		function GetSharedLinks(var Listing: TCloudDirItemList; ShowProgress: Boolean = False): Boolean;
		function GetIncomingInvites(var Listing: TCloudIncomingInviteList; ShowProgress: Boolean = False): Boolean;
		function GetIncomingInvitesAsDirItems(var DirListing: TCloudDirItemList; var InvitesListing: TCloudIncomingInviteList; ShowProgress: Boolean = False): Boolean;
		function GetTrashbin(var Listing: TCloudDirItemList; ShowProgress: Boolean = False): Boolean;
		function StatusFile(Path: WideString; var FileInfo: TCloudDirItem): Boolean;
		function TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function TrashbinEmpty(): Boolean;
		function GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
		procedure LogUserSpaceInfo(Email: WideString);
		function GetFileHistory(Path: WideString; var Versions: TCloudFileVersionList): Boolean;
	end;

	[TestFixture]
	TTrashBinOperationHandlerTest = class
	private
		FHandler: ITrashBinOperationHandler;
		FMockService: TMockListingService;
		FMockServiceIntf: ICloudListingService;

		function CreateDeletedItem(const Name, DeletedFrom: WideString; Rev: Integer): TCloudDirItem;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Dialog cancel test}
		[Test]
		procedure TestExecute_DialogCancel_ReturnsOK;

		{Empty trashbin tests}
		[Test]
		procedure TestExecute_EmptyTrashbin_CallsService;
		[Test]
		procedure TestExecute_EmptyTrashbin_Success_ReturnsOK;
		[Test]
		procedure TestExecute_EmptyTrashbin_Failure_ReturnsError;

		{Restore single tests}
		[Test]
		procedure TestExecute_RestoreSingle_CallsServiceWithCorrectPath;
		[Test]
		procedure TestExecute_RestoreSingle_Success_ReturnsOK;
		[Test]
		procedure TestExecute_RestoreSingle_Failure_ReturnsError;

		{Restore all tests}
		[Test]
		procedure TestExecute_RestoreAll_CallsServiceForEachItem;
		[Test]
		procedure TestExecute_RestoreAll_StopsOnFirstFailure;
	end;

implementation

uses
	SysUtils,
	Controls,
	WFXTypes;

{TMockListingService}

constructor TMockListingService.Create;
begin
	inherited Create;
	FTrashbinEmptyResult := True;
	FTrashbinRestoreResult := True;
	FTrashbinEmptyCalled := False;
	FTrashbinRestoreCalls := 0;
end;

procedure TMockListingService.SetTrashbinEmptyResult(Value: Boolean);
begin
	FTrashbinEmptyResult := Value;
end;

procedure TMockListingService.SetTrashbinRestoreResult(Value: Boolean);
begin
	FTrashbinRestoreResult := Value;
end;

function TMockListingService.WasTrashbinEmptyCalled: Boolean;
begin
	Result := FTrashbinEmptyCalled;
end;

function TMockListingService.GetTrashbinRestoreCalls: Integer;
begin
	Result := FTrashbinRestoreCalls;
end;

function TMockListingService.GetLastRestorePath: WideString;
begin
	Result := FLastRestorePath;
end;

function TMockListingService.GetLastRestoreRev: Integer;
begin
	Result := FLastRestoreRev;
end;

function TMockListingService.GetDirectory(Path: WideString; var Listing: TCloudDirItemList; ShowProgress: Boolean): Boolean;
begin
	Result := False;
end;

function TMockListingService.GetSharedLinks(var Listing: TCloudDirItemList; ShowProgress: Boolean): Boolean;
begin
	Result := False;
end;

function TMockListingService.GetIncomingInvites(var Listing: TCloudIncomingInviteList; ShowProgress: Boolean): Boolean;
begin
	Result := False;
end;

function TMockListingService.GetIncomingInvitesAsDirItems(var DirListing: TCloudDirItemList; var InvitesListing: TCloudIncomingInviteList; ShowProgress: Boolean): Boolean;
begin
	Result := False;
end;

function TMockListingService.GetTrashbin(var Listing: TCloudDirItemList; ShowProgress: Boolean): Boolean;
begin
	Result := False;
end;

function TMockListingService.StatusFile(Path: WideString; var FileInfo: TCloudDirItem): Boolean;
begin
	Result := False;
end;

function TMockListingService.TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString): Boolean;
begin
	Inc(FTrashbinRestoreCalls);
	FLastRestorePath := Path;
	FLastRestoreRev := RestoreRevision;
	Result := FTrashbinRestoreResult;
end;

function TMockListingService.TrashbinEmpty: Boolean;
begin
	FTrashbinEmptyCalled := True;
	Result := FTrashbinEmptyResult;
end;

function TMockListingService.GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
begin
	Result := False;
end;

procedure TMockListingService.LogUserSpaceInfo(Email: WideString);
begin
	{No-op}
end;

function TMockListingService.GetFileHistory(Path: WideString; var Versions: TCloudFileVersionList): Boolean;
begin
	Result := False;
end;

{TTrashBinOperationHandlerTest}

function TTrashBinOperationHandlerTest.CreateDeletedItem(const Name, DeletedFrom: WideString; Rev: Integer): TCloudDirItem;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.name := Name;
	Result.deleted_from := DeletedFrom;
	Result.rev := Rev;
	Result.type_ := TYPE_FILE;
end;

procedure TTrashBinOperationHandlerTest.Setup;
begin
	FHandler := TTrashBinOperationHandler.Create;
	FMockService := TMockListingService.Create;
	FMockServiceIntf := FMockService;
end;

procedure TTrashBinOperationHandlerTest.TearDown;
begin
	FHandler := nil;
	FMockServiceIntf := nil;
	FMockService := nil;
end;

{Dialog cancel test}

procedure TTrashBinOperationHandlerTest.TestExecute_DialogCancel_ReturnsOK;
var
	Listing: TCloudDirItemList;
	Item: TCloudDirItem;
	ExecResult: Integer;
begin
	SetLength(Listing, 1);
	Listing[0] := CreateDeletedItem('test.txt', '/backup/', 1);
	Item := Listing[0];

	ExecResult := FHandler.Execute(0, FMockServiceIntf, Listing, Item, False, 'account',
		function(ParentWindow: HWND; Items: TCloudDirItemList;
			IsTrashDir: Boolean; const AccountName: WideString): Integer
		begin
			Result := mrCancel;
		end);

	Assert.AreEqual(FS_EXEC_OK, ExecResult, 'Cancel should return OK (not an error)');
	Assert.IsFalse(FMockService.WasTrashbinEmptyCalled, 'Should not call TrashbinEmpty on cancel');
	Assert.AreEqual(0, FMockService.GetTrashbinRestoreCalls, 'Should not call TrashbinRestore on cancel');
end;

{Empty trashbin tests}

procedure TTrashBinOperationHandlerTest.TestExecute_EmptyTrashbin_CallsService;
var
	Listing: TCloudDirItemList;
	Item: TCloudDirItem;
begin
	SetLength(Listing, 1);
	Listing[0] := CreateDeletedItem('test.txt', '/backup/', 1);
	Item := Listing[0];

	FHandler.Execute(0, FMockServiceIntf, Listing, Item, True, 'account',
		function(ParentWindow: HWND; Items: TCloudDirItemList;
			IsTrashDir: Boolean; const AccountName: WideString): Integer
		begin
			Result := mrNo; {mrNo = Empty trashbin}
		end);

	Assert.IsTrue(FMockService.WasTrashbinEmptyCalled, 'Should call TrashbinEmpty when dialog returns mrNo');
end;

procedure TTrashBinOperationHandlerTest.TestExecute_EmptyTrashbin_Success_ReturnsOK;
var
	Listing: TCloudDirItemList;
	Item: TCloudDirItem;
	ExecResult: Integer;
begin
	SetLength(Listing, 1);
	Listing[0] := CreateDeletedItem('test.txt', '/backup/', 1);
	Item := Listing[0];
	FMockService.SetTrashbinEmptyResult(True);

	ExecResult := FHandler.Execute(0, FMockServiceIntf, Listing, Item, True, 'account',
		function(ParentWindow: HWND; Items: TCloudDirItemList;
			IsTrashDir: Boolean; const AccountName: WideString): Integer
		begin
			Result := mrNo;
		end);

	Assert.AreEqual(FS_EXEC_OK, ExecResult);
end;

procedure TTrashBinOperationHandlerTest.TestExecute_EmptyTrashbin_Failure_ReturnsError;
var
	Listing: TCloudDirItemList;
	Item: TCloudDirItem;
	ExecResult: Integer;
begin
	SetLength(Listing, 1);
	Listing[0] := CreateDeletedItem('test.txt', '/backup/', 1);
	Item := Listing[0];
	FMockService.SetTrashbinEmptyResult(False);

	ExecResult := FHandler.Execute(0, FMockServiceIntf, Listing, Item, True, 'account',
		function(ParentWindow: HWND; Items: TCloudDirItemList;
			IsTrashDir: Boolean; const AccountName: WideString): Integer
		begin
			Result := mrNo;
		end);

	Assert.AreEqual(FS_EXEC_ERROR, ExecResult);
end;

{Restore single tests}

procedure TTrashBinOperationHandlerTest.TestExecute_RestoreSingle_CallsServiceWithCorrectPath;
var
	Listing: TCloudDirItemList;
	Item: TCloudDirItem;
begin
	SetLength(Listing, 1);
	Item := CreateDeletedItem('document.pdf', '/work/docs/', 42);
	Listing[0] := Item;

	FHandler.Execute(0, FMockServiceIntf, Listing, Item, False, 'account',
		function(ParentWindow: HWND; Items: TCloudDirItemList;
			IsTrashDir: Boolean; const AccountName: WideString): Integer
		begin
			Result := mrYes; {mrYes = Restore single}
		end);

	Assert.AreEqual(1, FMockService.GetTrashbinRestoreCalls, 'Should call TrashbinRestore once');
	Assert.AreEqual('/work/docs/document.pdf', FMockService.GetLastRestorePath, 'Path should be deleted_from + name');
	Assert.AreEqual(42, FMockService.GetLastRestoreRev, 'Should pass correct revision');
end;

procedure TTrashBinOperationHandlerTest.TestExecute_RestoreSingle_Success_ReturnsOK;
var
	Listing: TCloudDirItemList;
	Item: TCloudDirItem;
	ExecResult: Integer;
begin
	Item := CreateDeletedItem('test.txt', '/backup/', 1);
	SetLength(Listing, 1);
	Listing[0] := Item;
	FMockService.SetTrashbinRestoreResult(True);

	ExecResult := FHandler.Execute(0, FMockServiceIntf, Listing, Item, False, 'account',
		function(ParentWindow: HWND; Items: TCloudDirItemList;
			IsTrashDir: Boolean; const AccountName: WideString): Integer
		begin
			Result := mrYes;
		end);

	Assert.AreEqual(FS_EXEC_OK, ExecResult);
end;

procedure TTrashBinOperationHandlerTest.TestExecute_RestoreSingle_Failure_ReturnsError;
var
	Listing: TCloudDirItemList;
	Item: TCloudDirItem;
	ExecResult: Integer;
begin
	Item := CreateDeletedItem('test.txt', '/backup/', 1);
	SetLength(Listing, 1);
	Listing[0] := Item;
	FMockService.SetTrashbinRestoreResult(False);

	ExecResult := FHandler.Execute(0, FMockServiceIntf, Listing, Item, False, 'account',
		function(ParentWindow: HWND; Items: TCloudDirItemList;
			IsTrashDir: Boolean; const AccountName: WideString): Integer
		begin
			Result := mrYes;
		end);

	Assert.AreEqual(FS_EXEC_ERROR, ExecResult);
end;

{Restore all tests}

procedure TTrashBinOperationHandlerTest.TestExecute_RestoreAll_CallsServiceForEachItem;
var
	Listing: TCloudDirItemList;
	Item: TCloudDirItem;
begin
	SetLength(Listing, 3);
	Listing[0] := CreateDeletedItem('file1.txt', '/docs/', 1);
	Listing[1] := CreateDeletedItem('file2.txt', '/docs/', 2);
	Listing[2] := CreateDeletedItem('file3.txt', '/docs/', 3);
	Item := TCloudDirItem.None;

	FHandler.Execute(0, FMockServiceIntf, Listing, Item, True, 'account',
		function(ParentWindow: HWND; Items: TCloudDirItemList;
			IsTrashDir: Boolean; const AccountName: WideString): Integer
		begin
			Result := mrYesToAll; {mrYesToAll = Restore all}
		end);

	Assert.AreEqual(3, FMockService.GetTrashbinRestoreCalls, 'Should call TrashbinRestore for each item');
end;

procedure TTrashBinOperationHandlerTest.TestExecute_RestoreAll_StopsOnFirstFailure;
var
	Listing: TCloudDirItemList;
	Item: TCloudDirItem;
	ExecResult: Integer;
begin
	SetLength(Listing, 3);
	Listing[0] := CreateDeletedItem('file1.txt', '/docs/', 1);
	Listing[1] := CreateDeletedItem('file2.txt', '/docs/', 2);
	Listing[2] := CreateDeletedItem('file3.txt', '/docs/', 3);
	Item := TCloudDirItem.None;

	FMockService.SetTrashbinRestoreResult(False); {All will fail, but we want to check it stops}

	ExecResult := FHandler.Execute(0, FMockServiceIntf, Listing, Item, True, 'account',
		function(ParentWindow: HWND; Items: TCloudDirItemList;
			IsTrashDir: Boolean; const AccountName: WideString): Integer
		begin
			Result := mrYesToAll;
		end);

	Assert.AreEqual(FS_EXEC_ERROR, ExecResult, 'Should return error on failure');
	Assert.AreEqual(1, FMockService.GetTrashbinRestoreCalls, 'Should stop after first failure');
end;

initialization
	TDUnitX.RegisterTestFixture(TTrashBinOperationHandlerTest);

end.
