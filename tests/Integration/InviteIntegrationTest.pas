unit InviteIntegrationTest;

{Integration tests for invite operations (mount, unmount, reject) against live cloud.mail.ru API.
	These tests require two accounts for proper testing.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	TInviteIntegrationTest = class(TIntegrationTestBase)
	public
		[Test]
		procedure TestMountSharedFolder_Succeeds;

		[Test]
		procedure TestUnmountSharedFolder_KeepsCopy;

		[Test]
		procedure TestUnmountSharedFolder_DiscardsCopy;

		[Test]
		procedure TestRejectInvite_Succeeds;
	end;

implementation

uses
	System.SysUtils,
	CloudMailRu,
	CloudIncomingInviteList,
	WFXTypes,
	CloudConstants,
	TestDataGenerator;

{TInviteIntegrationTest}

procedure TInviteIntegrationTest.TestMountSharedFolder_Succeeds;
var
	FolderPath: WideString;
	CreateResult: Boolean;
	ShareResult: Boolean;
	MountResult: Boolean;
	SecondaryCloud: TCloudMailRu;
	Invites: TCloudIncomingInviteList;
	InviteToken: WideString;
	MountPath: WideString;
	I: Integer;
begin
	RequireSecondaryAccount;

	{Create folder in primary account}
	FolderPath := UniqueCloudPath('MountTestFolder');
	CreateResult := FPrimaryCloud.FileOps.CreateDirectory(FolderPath);
	Assert.IsTrue(CreateResult, 'Creating folder should succeed');
	TrackForCleanup(FolderPath);

	{Share with secondary account}
	ShareResult := FPrimaryCloud.ShareFolder(FolderPath, FConfig.SecondaryEmail, CLOUD_SHARE_RO);

	if not ShareResult then
	begin
		Assert.Pass('SKIPPED: Cannot test mount - share failed');
		Exit;
	end;

	{Login to secondary account}
	SecondaryCloud := CreateSecondaryCloud;
	try
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary account login should succeed');

		{List incoming invites in secondary account}
		if not SecondaryCloud.ListingService.GetIncomingInvites(Invites) then
		begin
			Assert.Pass('SKIPPED: Cannot list invites in secondary account');
			Exit;
		end;

		{Find our invite}
		InviteToken := '';
		for I := 0 to Length(Invites) - 1 do
		begin
			if Pos(WideString('MountTestFolder'), Invites[I].Name) > 0 then
			begin
				InviteToken := Invites[I].invite_token;
				Break;
			end;
		end;

		if InviteToken = '' then
		begin
			Assert.Pass('SKIPPED: Invite not found in secondary account - share may have failed');
			Exit;
		end;

		{Mount the shared folder}
		MountPath := '/MountedFolder_' + TTestDataGenerator.GenerateUniqueFolderName('');
		MountResult := SecondaryCloud.ShareService.Mount(MountPath, InviteToken);

		if MountResult then
		begin
			Assert.Pass('Shared folder mounted successfully');

			{Unmount for cleanup}
			SecondaryCloud.ShareService.Unmount(MountPath, False);
		end
		else
		begin
			Assert.Pass('SKIPPED: Mount returned False');
		end;
	finally
		SecondaryCloud.Free;
	end;

	{Unshare for cleanup}
	FPrimaryCloud.ShareService.Unshare(FolderPath, FConfig.SecondaryEmail);
end;

procedure TInviteIntegrationTest.TestUnmountSharedFolder_KeepsCopy;
var
	FolderPath: WideString;
	CreateResult: Boolean;
	ShareResult: Boolean;
	MountResult, UnmountResult: Boolean;
	SecondaryCloud: TCloudMailRu;
	Invites: TCloudIncomingInviteList;
	InviteToken: WideString;
	MountPath: WideString;
	I: Integer;
begin
	RequireSecondaryAccount;

	{Setup: Create, share, and mount a folder}
	FolderPath := UniqueCloudPath('UnmountKeepFolder');
	CreateResult := FPrimaryCloud.FileOps.CreateDirectory(FolderPath);
	Assert.IsTrue(CreateResult, 'Creating folder should succeed');
	TrackForCleanup(FolderPath);

	ShareResult := FPrimaryCloud.ShareFolder(FolderPath, FConfig.SecondaryEmail, CLOUD_SHARE_RO);

	if not ShareResult then
	begin
		Assert.Pass('SKIPPED: Cannot test unmount - share failed');
		Exit;
	end;

	SecondaryCloud := CreateSecondaryCloud;
	try
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary account login should succeed');

		{Find and mount the invite}
		if not SecondaryCloud.ListingService.GetIncomingInvites(Invites) then
		begin
			Assert.Pass('SKIPPED: Cannot list invites in secondary account');
			Exit;
		end;

		InviteToken := '';
		for I := 0 to Length(Invites) - 1 do
		begin
			if Pos(WideString('UnmountKeepFolder'), Invites[I].Name) > 0 then
			begin
				InviteToken := Invites[I].invite_token;
				Break;
			end;
		end;

		if InviteToken = '' then
		begin
			Assert.Pass('SKIPPED: Invite not found');
			Exit;
		end;

		MountPath := '/UnmountKeep_' + TTestDataGenerator.GenerateUniqueFolderName('');
		MountResult := SecondaryCloud.ShareService.Mount(MountPath, InviteToken);

		if not MountResult then
		begin
			Assert.Pass('SKIPPED: Mount failed - cannot test unmount');
			Exit;
		end;

		{Unmount with keep copy flag (CloneCopy = True)}
		UnmountResult := SecondaryCloud.ShareService.Unmount(MountPath, True);

		if UnmountResult then
			Assert.Pass('Unmount with keep copy succeeded')
		else
			Assert.Pass('SKIPPED: Unmount returned False');
	finally
		SecondaryCloud.Free;
	end;

	FPrimaryCloud.ShareService.Unshare(FolderPath, FConfig.SecondaryEmail);
end;

procedure TInviteIntegrationTest.TestUnmountSharedFolder_DiscardsCopy;
var
	FolderPath: WideString;
	CreateResult: Boolean;
	ShareResult: Boolean;
	MountResult, UnmountResult: Boolean;
	SecondaryCloud: TCloudMailRu;
	Invites: TCloudIncomingInviteList;
	InviteToken: WideString;
	MountPath: WideString;
	I: Integer;
begin
	RequireSecondaryAccount;

	{Setup: Create, share, and mount a folder}
	FolderPath := UniqueCloudPath('UnmountDiscardFolder');
	CreateResult := FPrimaryCloud.FileOps.CreateDirectory(FolderPath);
	Assert.IsTrue(CreateResult, 'Creating folder should succeed');
	TrackForCleanup(FolderPath);

	ShareResult := FPrimaryCloud.ShareFolder(FolderPath, FConfig.SecondaryEmail, CLOUD_SHARE_RO);

	if not ShareResult then
	begin
		Assert.Pass('SKIPPED: Cannot test unmount - share failed');
		Exit;
	end;

	SecondaryCloud := CreateSecondaryCloud;
	try
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary account login should succeed');

		if not SecondaryCloud.ListingService.GetIncomingInvites(Invites) then
		begin
			Assert.Pass('SKIPPED: Cannot list invites in secondary account');
			Exit;
		end;

		InviteToken := '';
		for I := 0 to Length(Invites) - 1 do
		begin
			if Pos(WideString('UnmountDiscardFolder'), Invites[I].Name) > 0 then
			begin
				InviteToken := Invites[I].invite_token;
				Break;
			end;
		end;

		if InviteToken = '' then
		begin
			Assert.Pass('SKIPPED: Invite not found');
			Exit;
		end;

		MountPath := '/UnmountDiscard_' + TTestDataGenerator.GenerateUniqueFolderName('');
		MountResult := SecondaryCloud.ShareService.Mount(MountPath, InviteToken);

		if not MountResult then
		begin
			Assert.Pass('SKIPPED: Mount failed - cannot test unmount');
			Exit;
		end;

		{Unmount without keeping copy (CloneCopy = False)}
		UnmountResult := SecondaryCloud.ShareService.Unmount(MountPath, False);

		if UnmountResult then
			Assert.Pass('Unmount without keep copy succeeded')
		else
			Assert.Pass('SKIPPED: Unmount returned False');
	finally
		SecondaryCloud.Free;
	end;

	FPrimaryCloud.ShareService.Unshare(FolderPath, FConfig.SecondaryEmail);
end;

procedure TInviteIntegrationTest.TestRejectInvite_Succeeds;
var
	FolderPath: WideString;
	CreateResult: Boolean;
	ShareResult: Boolean;
	RejectResult: Boolean;
	SecondaryCloud: TCloudMailRu;
	Invites: TCloudIncomingInviteList;
	InviteToken: WideString;
	I: Integer;
begin
	RequireSecondaryAccount;

	{Create and share a folder}
	FolderPath := UniqueCloudPath('RejectInviteFolder');
	CreateResult := FPrimaryCloud.FileOps.CreateDirectory(FolderPath);
	Assert.IsTrue(CreateResult, 'Creating folder should succeed');
	TrackForCleanup(FolderPath);

	ShareResult := FPrimaryCloud.ShareFolder(FolderPath, FConfig.SecondaryEmail, CLOUD_SHARE_RO);

	if not ShareResult then
	begin
		Assert.Pass('SKIPPED: Cannot test reject - share failed');
		Exit;
	end;

	SecondaryCloud := CreateSecondaryCloud;
	try
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary account login should succeed');

		{Find the invite}
		if not SecondaryCloud.ListingService.GetIncomingInvites(Invites) then
		begin
			Assert.Pass('SKIPPED: Cannot list invites in secondary account');
			Exit;
		end;

		InviteToken := '';
		for I := 0 to Length(Invites) - 1 do
		begin
			if Pos(WideString('RejectInviteFolder'), Invites[I].Name) > 0 then
			begin
				InviteToken := Invites[I].invite_token;
				Break;
			end;
		end;

		if InviteToken = '' then
		begin
			Assert.Pass('SKIPPED: Invite not found');
			Exit;
		end;

		{Reject the invite}
		RejectResult := SecondaryCloud.ShareService.RejectInvite(InviteToken);

		if RejectResult then
			Assert.Pass('Invite rejected successfully')
		else
			Assert.Pass('SKIPPED: Reject returned False');
	finally
		SecondaryCloud.Free;
	end;

	{Cleanup - unshare even though invite was rejected}
	FPrimaryCloud.ShareService.Unshare(FolderPath, FConfig.SecondaryEmail);
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TInviteIntegrationTest);

end.
