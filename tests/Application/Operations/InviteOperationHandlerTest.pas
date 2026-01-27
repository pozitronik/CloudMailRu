unit InviteOperationHandlerTest;

{Unit tests for TInviteOperationHandler using mock ICloudShareService.}

interface

uses
	DUnitX.TestFramework,
	Windows,
	CloudDirItem,
	CloudInviteList,
	CloudIncomingInvite,
	CloudConstants,
	CloudShareService,
	InviteOperationHandler;

type
	{Mock ICloudShareService for testing InviteOperationHandler}
	TMockShareService = class(TInterfacedObject, ICloudShareService)
	private
		FMountResult: Boolean;
		FUnmountResult: Boolean;
		FRejectResult: Boolean;
		FMountCalls: Integer;
		FUnmountCalls: Integer;
		FRejectCalls: Integer;
		FLastMountHome: WideString;
		FLastMountToken: WideString;
		FLastUnmountHome: WideString;
		FLastUnmountCloneCopy: Boolean;
		FLastRejectToken: WideString;
	public
		constructor Create;

		{Mock configuration}
		procedure SetMountResult(Value: Boolean);
		procedure SetUnmountResult(Value: Boolean);
		procedure SetRejectResult(Value: Boolean);

		{Mock verification}
		function GetMountCalls: Integer;
		function GetUnmountCalls: Integer;
		function GetRejectCalls: Integer;
		function GetLastMountHome: WideString;
		function GetLastMountToken: WideString;
		function GetLastUnmountHome: WideString;
		function GetLastUnmountCloneCopy: Boolean;
		function GetLastRejectToken: WideString;

		{ICloudShareService implementation}
		function Publish(Path: WideString; var PublicLink: WideString): Boolean;
		function Unpublish(Path: WideString; PublicLink: WideString): Boolean;
		function GetShareInfo(Path: WideString; var InviteListing: TCloudInviteList): Boolean;
		function Share(Path, Email: WideString; Access: Integer): Boolean;
		function Unshare(Path, Email: WideString): Boolean;
		function Mount(Home, InviteToken: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function Unmount(Home: WideString; CloneCopy: Boolean): Boolean;
		function RejectInvite(InviteToken: WideString): Boolean;
		function GetPublishedFileStreamUrl(FileIdentity: TCloudDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; Publish: Boolean = CLOUD_PUBLISH): Boolean;
		function CloneWeblink(Path, Link: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Integer;
	end;

	[TestFixture]
	TInviteOperationHandlerTest = class
	private
		FHandler: IInviteOperationHandler;
		FMockService: TMockShareService;
		FMockServiceIntf: ICloudShareService;

		function CreateInvite(const Name, InviteToken: WideString): TCloudIncomingInvite;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{Dialog cancel test}
		[Test]
		procedure TestExecute_DialogCancel_ReturnsOK;
		[Test]
		procedure TestExecute_DialogCancel_NoOperationCalled;

		{Mount folder tests (mrYes)}
		[Test]
		procedure TestExecute_MountFolder_CallsServiceWithCorrectParams;
		[Test]
		procedure TestExecute_MountFolder_Success_ReturnsOK;

		{Unmount with keep tests (mrAbort)}
		[Test]
		procedure TestExecute_UnmountKeep_CallsServiceWithCloneCopyTrue;
		[Test]
		procedure TestExecute_UnmountKeep_Success_ReturnsOK;

		{Unmount without keep tests (mrClose)}
		[Test]
		procedure TestExecute_UnmountNoKeep_CallsServiceWithCloneCopyFalse;
		[Test]
		procedure TestExecute_UnmountNoKeep_Success_ReturnsOK;

		{Reject invite tests (mrNo)}
		[Test]
		procedure TestExecute_RejectInvite_CallsServiceWithCorrectToken;
		[Test]
		procedure TestExecute_RejectInvite_Success_ReturnsOK;
	end;

implementation

uses
	SysUtils,
	Controls,
	WFXTypes;

{TMockShareService}

constructor TMockShareService.Create;
begin
	inherited Create;
	FMountResult := True;
	FUnmountResult := True;
	FRejectResult := True;
	FMountCalls := 0;
	FUnmountCalls := 0;
	FRejectCalls := 0;
end;

procedure TMockShareService.SetMountResult(Value: Boolean);
begin
	FMountResult := Value;
end;

procedure TMockShareService.SetUnmountResult(Value: Boolean);
begin
	FUnmountResult := Value;
end;

procedure TMockShareService.SetRejectResult(Value: Boolean);
begin
	FRejectResult := Value;
end;

function TMockShareService.GetMountCalls: Integer;
begin
	Result := FMountCalls;
end;

function TMockShareService.GetUnmountCalls: Integer;
begin
	Result := FUnmountCalls;
end;

function TMockShareService.GetRejectCalls: Integer;
begin
	Result := FRejectCalls;
end;

function TMockShareService.GetLastMountHome: WideString;
begin
	Result := FLastMountHome;
end;

function TMockShareService.GetLastMountToken: WideString;
begin
	Result := FLastMountToken;
end;

function TMockShareService.GetLastUnmountHome: WideString;
begin
	Result := FLastUnmountHome;
end;

function TMockShareService.GetLastUnmountCloneCopy: Boolean;
begin
	Result := FLastUnmountCloneCopy;
end;

function TMockShareService.GetLastRejectToken: WideString;
begin
	Result := FLastRejectToken;
end;

function TMockShareService.Publish(Path: WideString; var PublicLink: WideString): Boolean;
begin
	Result := False;
end;

function TMockShareService.Unpublish(Path: WideString; PublicLink: WideString): Boolean;
begin
	Result := False;
end;

function TMockShareService.GetShareInfo(Path: WideString; var InviteListing: TCloudInviteList): Boolean;
begin
	Result := False;
end;

function TMockShareService.Share(Path, Email: WideString; Access: Integer): Boolean;
begin
	Result := False;
end;

function TMockShareService.Unshare(Path, Email: WideString): Boolean;
begin
	Result := False;
end;

function TMockShareService.Mount(Home, InviteToken: WideString; ConflictMode: WideString): Boolean;
begin
	Inc(FMountCalls);
	FLastMountHome := Home;
	FLastMountToken := InviteToken;
	Result := FMountResult;
end;

function TMockShareService.Unmount(Home: WideString; CloneCopy: Boolean): Boolean;
begin
	Inc(FUnmountCalls);
	FLastUnmountHome := Home;
	FLastUnmountCloneCopy := CloneCopy;
	Result := FUnmountResult;
end;

function TMockShareService.RejectInvite(InviteToken: WideString): Boolean;
begin
	Inc(FRejectCalls);
	FLastRejectToken := InviteToken;
	Result := FRejectResult;
end;

function TMockShareService.GetPublishedFileStreamUrl(FileIdentity: TCloudDirItem; var StreamUrl: WideString; ShardType: WideString; Publish: Boolean): Boolean;
begin
	Result := False;
end;

function TMockShareService.CloneWeblink(Path, Link, ConflictMode: WideString): Integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
end;

{TInviteOperationHandlerTest}

function TInviteOperationHandlerTest.CreateInvite(const Name, InviteToken: WideString): TCloudIncomingInvite;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.name := Name;
	Result.invite_token := InviteToken;
end;

procedure TInviteOperationHandlerTest.Setup;
begin
	FHandler := TInviteOperationHandler.Create;
	FMockService := TMockShareService.Create;
	FMockServiceIntf := FMockService;
end;

procedure TInviteOperationHandlerTest.TearDown;
begin
	FHandler := nil;
	FMockServiceIntf := nil;
	FMockService := nil;
end;

{Dialog cancel tests}

procedure TInviteOperationHandlerTest.TestExecute_DialogCancel_ReturnsOK;
var
	Invite: TCloudIncomingInvite;
	ExecResult: Integer;
begin
	Invite := CreateInvite('SharedFolder', 'token123');

	ExecResult := FHandler.Execute(0, FMockServiceIntf, Invite,
		function(ParentWindow: HWND; const Inv: TCloudIncomingInvite): Integer
		begin
			Result := mrCancel;
		end);

	Assert.AreEqual(FS_EXEC_OK, ExecResult, 'Cancel should return OK (not an error)');
end;

procedure TInviteOperationHandlerTest.TestExecute_DialogCancel_NoOperationCalled;
var
	Invite: TCloudIncomingInvite;
begin
	Invite := CreateInvite('SharedFolder', 'token123');

	FHandler.Execute(0, FMockServiceIntf, Invite,
		function(ParentWindow: HWND; const Inv: TCloudIncomingInvite): Integer
		begin
			Result := mrCancel;
		end);

	Assert.AreEqual(0, FMockService.GetMountCalls, 'Should not call Mount on cancel');
	Assert.AreEqual(0, FMockService.GetUnmountCalls, 'Should not call Unmount on cancel');
	Assert.AreEqual(0, FMockService.GetRejectCalls, 'Should not call RejectInvite on cancel');
end;

{Mount folder tests (mrYes)}

procedure TInviteOperationHandlerTest.TestExecute_MountFolder_CallsServiceWithCorrectParams;
var
	Invite: TCloudIncomingInvite;
begin
	Invite := CreateInvite('SharedDocs', 'invite-token-abc');

	FHandler.Execute(0, FMockServiceIntf, Invite,
		function(ParentWindow: HWND; const Inv: TCloudIncomingInvite): Integer
		begin
			Result := mrYes; {mrYes = Mount folder}
		end);

	Assert.AreEqual(1, FMockService.GetMountCalls, 'Should call Mount once');
	Assert.AreEqual('SharedDocs', FMockService.GetLastMountHome, 'Should pass invite name as home');
	Assert.AreEqual('invite-token-abc', FMockService.GetLastMountToken, 'Should pass invite token');
end;

procedure TInviteOperationHandlerTest.TestExecute_MountFolder_Success_ReturnsOK;
var
	Invite: TCloudIncomingInvite;
	ExecResult: Integer;
begin
	Invite := CreateInvite('SharedDocs', 'token123');
	FMockService.SetMountResult(True);

	ExecResult := FHandler.Execute(0, FMockServiceIntf, Invite,
		function(ParentWindow: HWND; const Inv: TCloudIncomingInvite): Integer
		begin
			Result := mrYes;
		end);

	Assert.AreEqual(FS_EXEC_OK, ExecResult);
end;

{Unmount with keep tests (mrAbort)}

procedure TInviteOperationHandlerTest.TestExecute_UnmountKeep_CallsServiceWithCloneCopyTrue;
var
	Invite: TCloudIncomingInvite;
begin
	Invite := CreateInvite('SharedFolder', 'token123');

	FHandler.Execute(0, FMockServiceIntf, Invite,
		function(ParentWindow: HWND; const Inv: TCloudIncomingInvite): Integer
		begin
			Result := mrAbort; {mrAbort = Unmount folder, keep data}
		end);

	Assert.AreEqual(1, FMockService.GetUnmountCalls, 'Should call Unmount once');
	Assert.AreEqual('SharedFolder', FMockService.GetLastUnmountHome, 'Should pass invite name');
	Assert.IsTrue(FMockService.GetLastUnmountCloneCopy, 'CloneCopy should be true for keep data');
end;

procedure TInviteOperationHandlerTest.TestExecute_UnmountKeep_Success_ReturnsOK;
var
	Invite: TCloudIncomingInvite;
	ExecResult: Integer;
begin
	Invite := CreateInvite('SharedFolder', 'token123');
	FMockService.SetUnmountResult(True);

	ExecResult := FHandler.Execute(0, FMockServiceIntf, Invite,
		function(ParentWindow: HWND; const Inv: TCloudIncomingInvite): Integer
		begin
			Result := mrAbort;
		end);

	Assert.AreEqual(FS_EXEC_OK, ExecResult);
end;

{Unmount without keep tests (mrClose)}

procedure TInviteOperationHandlerTest.TestExecute_UnmountNoKeep_CallsServiceWithCloneCopyFalse;
var
	Invite: TCloudIncomingInvite;
begin
	Invite := CreateInvite('SharedProject', 'token456');

	FHandler.Execute(0, FMockServiceIntf, Invite,
		function(ParentWindow: HWND; const Inv: TCloudIncomingInvite): Integer
		begin
			Result := mrClose; {mrClose = Unmount folder, don't keep data}
		end);

	Assert.AreEqual(1, FMockService.GetUnmountCalls, 'Should call Unmount once');
	Assert.AreEqual('SharedProject', FMockService.GetLastUnmountHome, 'Should pass invite name');
	Assert.IsFalse(FMockService.GetLastUnmountCloneCopy, 'CloneCopy should be false for no keep');
end;

procedure TInviteOperationHandlerTest.TestExecute_UnmountNoKeep_Success_ReturnsOK;
var
	Invite: TCloudIncomingInvite;
	ExecResult: Integer;
begin
	Invite := CreateInvite('SharedFolder', 'token123');
	FMockService.SetUnmountResult(True);

	ExecResult := FHandler.Execute(0, FMockServiceIntf, Invite,
		function(ParentWindow: HWND; const Inv: TCloudIncomingInvite): Integer
		begin
			Result := mrClose;
		end);

	Assert.AreEqual(FS_EXEC_OK, ExecResult);
end;

{Reject invite tests (mrNo)}

procedure TInviteOperationHandlerTest.TestExecute_RejectInvite_CallsServiceWithCorrectToken;
var
	Invite: TCloudIncomingInvite;
begin
	Invite := CreateInvite('SharedFiles', 'reject-token-xyz');

	FHandler.Execute(0, FMockServiceIntf, Invite,
		function(ParentWindow: HWND; const Inv: TCloudIncomingInvite): Integer
		begin
			Result := mrNo; {mrNo = Reject invite}
		end);

	Assert.AreEqual(1, FMockService.GetRejectCalls, 'Should call RejectInvite once');
	Assert.AreEqual('reject-token-xyz', FMockService.GetLastRejectToken, 'Should pass invite token');
end;

procedure TInviteOperationHandlerTest.TestExecute_RejectInvite_Success_ReturnsOK;
var
	Invite: TCloudIncomingInvite;
	ExecResult: Integer;
begin
	Invite := CreateInvite('SharedFolder', 'token123');
	FMockService.SetRejectResult(True);

	ExecResult := FHandler.Execute(0, FMockServiceIntf, Invite,
		function(ParentWindow: HWND; const Inv: TCloudIncomingInvite): Integer
		begin
			Result := mrNo;
		end);

	Assert.AreEqual(FS_EXEC_OK, ExecResult);
end;

initialization
	TDUnitX.RegisterTestFixture(TInviteOperationHandlerTest);

end.
