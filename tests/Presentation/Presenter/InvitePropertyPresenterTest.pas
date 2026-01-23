unit InvitePropertyPresenterTest;

interface

uses
	DUnitX.TestFramework,
	InvitePropertyPresenter,
	CMRIncomingInvite,
	CMROwner,
	CMRConstants,
	CloudAccessUtils,
	PluginHelper,
	LANGUAGE_STRINGS,
	System.SysUtils;

type
	{Mock view for testing InvitePropertyPresenter}
	TMockInvitePropertyView = class(TInterfacedObject, IInvitePropertyView)
	public
		Caption: WideString;
		Name: WideString;
		OwnerEmail: WideString;
		OwnerName: WideString;
		Access: WideString;
		Size: WideString;
		TokenLabel: WideString;
		TokenValue: WideString;
		MountEnabled: Boolean;
		RejectEnabled: Boolean;
		UnmountCopyEnabled: Boolean;
		UnmountDeleteEnabled: Boolean;

		{IInvitePropertyView implementation}
		procedure SetCaption(ACaption: WideString);
		procedure SetName(AName: WideString);
		procedure SetOwnerEmail(AEmail: WideString);
		procedure SetOwnerName(AName: WideString);
		procedure SetAccess(AAccess: WideString);
		procedure SetSize(ASize: WideString);
		procedure SetTokenLabel(ALabelText: WideString);
		procedure SetTokenValue(AValue: WideString);
		procedure SetMountEnabled(AEnabled: Boolean);
		procedure SetRejectEnabled(AEnabled: Boolean);
		procedure SetUnmountCopyEnabled(AEnabled: Boolean);
		procedure SetUnmountDeleteEnabled(AEnabled: Boolean);
	end;

	[TestFixture]
	TInvitePropertyPresenterTest = class
	private
		FMockView: TMockInvitePropertyView;
		FPresenter: TInvitePropertyPresenter;

		function CreateTestInvite(IsMounted: Boolean): TCMRIncomingInvite;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		[Test]
		procedure Initialize_SetsCaption_WithAccountNameAndItemName;

		[Test]
		procedure Initialize_SetsOwnerEmail_FromItem;

		[Test]
		procedure Initialize_SetsOwnerName_FromItem;

		[Test]
		procedure Initialize_SetsName_FromItem;

		[Test]
		procedure Initialize_SetsAccess_ConvertedToString;

		[Test]
		procedure Initialize_SetsSize_Formatted;

		[Test]
		procedure Initialize_WhenNotMounted_ShowsInviteToken;

		[Test]
		procedure Initialize_WhenNotMounted_SetsMountEnabled;

		[Test]
		procedure Initialize_WhenNotMounted_SetsRejectEnabled;

		[Test]
		procedure Initialize_WhenNotMounted_DisablesUnmountButtons;

		[Test]
		procedure Initialize_WhenMounted_ShowsMountPath;

		[Test]
		procedure Initialize_WhenMounted_ChangesTokenLabel;

		[Test]
		procedure Initialize_WhenMounted_DisablesReject;

		[Test]
		procedure Initialize_WhenMounted_EnablesUnmountButtons;

		[Test]
		procedure Item_Property_ReturnsInitializedItem;

		[Test]
		procedure AccountName_Property_ReturnsInitializedAccountName;
	end;

implementation

{TMockInvitePropertyView}

procedure TMockInvitePropertyView.SetCaption(ACaption: WideString);
begin
	Caption := ACaption;
end;

procedure TMockInvitePropertyView.SetName(AName: WideString);
begin
	Name := AName;
end;

procedure TMockInvitePropertyView.SetOwnerEmail(AEmail: WideString);
begin
	OwnerEmail := AEmail;
end;

procedure TMockInvitePropertyView.SetOwnerName(AName: WideString);
begin
	OwnerName := AName;
end;

procedure TMockInvitePropertyView.SetAccess(AAccess: WideString);
begin
	Access := AAccess;
end;

procedure TMockInvitePropertyView.SetSize(ASize: WideString);
begin
	Size := ASize;
end;

procedure TMockInvitePropertyView.SetTokenLabel(ALabelText: WideString);
begin
	TokenLabel := ALabelText;
end;

procedure TMockInvitePropertyView.SetTokenValue(AValue: WideString);
begin
	TokenValue := AValue;
end;

procedure TMockInvitePropertyView.SetMountEnabled(AEnabled: Boolean);
begin
	MountEnabled := AEnabled;
end;

procedure TMockInvitePropertyView.SetRejectEnabled(AEnabled: Boolean);
begin
	RejectEnabled := AEnabled;
end;

procedure TMockInvitePropertyView.SetUnmountCopyEnabled(AEnabled: Boolean);
begin
	UnmountCopyEnabled := AEnabled;
end;

procedure TMockInvitePropertyView.SetUnmountDeleteEnabled(AEnabled: Boolean);
begin
	UnmountDeleteEnabled := AEnabled;
end;

{TInvitePropertyPresenterTest}

function TInvitePropertyPresenterTest.CreateTestInvite(IsMounted: Boolean): TCMRIncomingInvite;
begin
	Result.name := 'SharedFolder';
	Result.owner.email := 'owner@example.com';
	Result.owner.name := 'Owner Name';
	Result.access := CLOUD_SHARE_ACCESS_READ_ONLY;
	Result.size := 1024 * 1024; // 1 MB
	Result.tree := '/shared/folder';
	Result.invite_token := 'token123';

	if IsMounted then
		Result.home := '/MyMountedFolder'
	else
		Result.home := '';
end;

procedure TInvitePropertyPresenterTest.Setup;
begin
	FMockView := TMockInvitePropertyView.Create;
	FPresenter := TInvitePropertyPresenter.Create(FMockView);
end;

procedure TInvitePropertyPresenterTest.TearDown;
begin
	FPresenter.Free;
	{FMockView is reference counted, will be freed automatically}
end;

procedure TInvitePropertyPresenterTest.Initialize_SetsCaption_WithAccountNameAndItemName;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(False);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.AreEqual(Format(INVITE_FORM_TITLE, ['TestAccount', 'SharedFolder']), FMockView.Caption);
end;

procedure TInvitePropertyPresenterTest.Initialize_SetsOwnerEmail_FromItem;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(False);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.AreEqual('owner@example.com', FMockView.OwnerEmail);
end;

procedure TInvitePropertyPresenterTest.Initialize_SetsOwnerName_FromItem;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(False);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.AreEqual('Owner Name', FMockView.OwnerName);
end;

procedure TInvitePropertyPresenterTest.Initialize_SetsName_FromItem;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(False);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.AreEqual('SharedFolder', FMockView.Name);
end;

procedure TInvitePropertyPresenterTest.Initialize_SetsAccess_ConvertedToString;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(False);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.AreEqual(TCloudAccessUtils.AccessToString(CLOUD_SHARE_ACCESS_READ_ONLY), FMockView.Access);
end;

procedure TInvitePropertyPresenterTest.Initialize_SetsSize_Formatted;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(False);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.AreEqual(FormatSize(1024 * 1024, TYPE_BYTES), FMockView.Size);
end;

procedure TInvitePropertyPresenterTest.Initialize_WhenNotMounted_ShowsInviteToken;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(False);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.AreEqual('token123', FMockView.TokenValue);
end;

procedure TInvitePropertyPresenterTest.Initialize_WhenNotMounted_SetsMountEnabled;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(False);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.IsTrue(FMockView.MountEnabled);
end;

procedure TInvitePropertyPresenterTest.Initialize_WhenNotMounted_SetsRejectEnabled;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(False);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.IsTrue(FMockView.RejectEnabled);
end;

procedure TInvitePropertyPresenterTest.Initialize_WhenNotMounted_DisablesUnmountButtons;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(False);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.IsFalse(FMockView.UnmountCopyEnabled);
	Assert.IsFalse(FMockView.UnmountDeleteEnabled);
end;

procedure TInvitePropertyPresenterTest.Initialize_WhenMounted_ShowsMountPath;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(True);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.AreEqual('/MyMountedFolder', FMockView.TokenValue);
end;

procedure TInvitePropertyPresenterTest.Initialize_WhenMounted_ChangesTokenLabel;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(True);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.AreEqual(MOUNTED_AS, FMockView.TokenLabel);
end;

procedure TInvitePropertyPresenterTest.Initialize_WhenMounted_DisablesReject;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(True);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.IsFalse(FMockView.RejectEnabled);
end;

procedure TInvitePropertyPresenterTest.Initialize_WhenMounted_EnablesUnmountButtons;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(True);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.IsTrue(FMockView.UnmountCopyEnabled);
	Assert.IsTrue(FMockView.UnmountDeleteEnabled);
end;

procedure TInvitePropertyPresenterTest.Item_Property_ReturnsInitializedItem;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(False);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.AreEqual('SharedFolder', FPresenter.Item.name);
	Assert.AreEqual('token123', FPresenter.Item.invite_token);
end;

procedure TInvitePropertyPresenterTest.AccountName_Property_ReturnsInitializedAccountName;
var
	Item: TCMRIncomingInvite;
begin
	Item := CreateTestInvite(False);

	FPresenter.Initialize(Item, 'TestAccount');

	Assert.AreEqual('TestAccount', FPresenter.AccountName);
end;

initialization
	TDUnitX.RegisterTestFixture(TInvitePropertyPresenterTest);

end.
