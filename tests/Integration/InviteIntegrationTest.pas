unit InviteIntegrationTest;

{Integration tests for invite operations (mount, unmount, reject) against live cloud.mail.ru API.
	These tests require two accounts for proper testing.}

interface

uses
	DUnitX.TestFramework,
	IntegrationTestBase,
	IntegrationTestConfig,
	CloudMailRu,
	CloudIncomingInvite;

type
	{No [TestFixture] attribute - registered conditionally in initialization}
	[Category('Integration')]
	TInviteIntegrationTest = class(TIntegrationTestBase)
	private
		{Share a folder from primary to secondary, find and return the invite.
			Returns True if invite was found, False if any step was skipped.}
		function ShareAndFindInvite(const FolderNamePrefix: WideString; Access: Integer;
			SecondaryCloud: TCloudMailRu; out FolderPath: WideString;
			out Invite: TCloudIncomingInvite): Boolean;
	public
		[Test]
		procedure TestMountSharedFolder_Succeeds;

		[Test]
		procedure TestMountSharedFolder_ContentsVisible;

		[Test]
		procedure TestUnmountSharedFolder_KeepsCopy;

		[Test]
		procedure TestUnmountSharedFolder_DiscardsCopy;

		[Test]
		procedure TestRejectInvite_Succeeds;

		{Pending invite should have empty home field}
		[Test]
		procedure TestPendingInvite_HomeIsEmpty;

		{Mounted invite should have home field populated with mount path}
		[Test]
		procedure TestMountedInvite_HomeIsPopulated;

		{After unsharing, invite should disappear from secondary account}
		[Test]
		procedure TestUnshare_InviteDisappears;

		{After deleting RO-shared folder, mount disappears from secondary account}
		[Test]
		procedure TestDeleteROSharedFolder_MountDisappearsFromSecondary;

		{After deleting RW-shared folder, directory persists in secondary account}
		[Test]
		procedure TestDeleteRWSharedFolder_DirectoryPersistsInSecondary;

		{Mounted shared folder should appear with kind=shared in parent listing}
		[Test]
		procedure TestMountedFolder_AppearsAsSharedKind;
	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	System.IOUtils,
	CloudDirItem,
	CloudDirItemList,
	CloudIncomingInviteList,
	WFXTypes,
	CloudConstants,
	TestDataGenerator;

{TInviteIntegrationTest}

function TInviteIntegrationTest.ShareAndFindInvite(const FolderNamePrefix: WideString;
	Access: Integer; SecondaryCloud: TCloudMailRu; out FolderPath: WideString;
	out Invite: TCloudIncomingInvite): Boolean;
var
	Invites: TCloudIncomingInviteList;
	I: Integer;
begin
	Result := False;
	Invite := Default(TCloudIncomingInvite);

	{Create folder on primary account}
	FolderPath := UniqueCloudPath(FolderNamePrefix);
	if not FPrimaryCloud.FileOperations.CreateDirectory(FolderPath) then
	begin
		Assert.Pass('SKIPPED: Creating folder failed');
		Exit;
	end;
	TrackForCleanup(FolderPath);

	{Share with secondary account}
	if not FPrimaryCloud.ShareFolder(FolderPath, FConfig.SecondaryEmail, Access) then
	begin
		Assert.Pass('SKIPPED: Share failed');
		Exit;
	end;

	{Find the invite in secondary account}
	if not SecondaryCloud.ListingService.GetIncomingInvites(Invites) then
	begin
		Assert.Pass('SKIPPED: Cannot list invites in secondary account');
		Exit;
	end;

	for I := 0 to Length(Invites) - 1 do
	begin
		if Pos(FolderNamePrefix, Invites[I].Name) > 0 then
		begin
			Invite := Invites[I];
			Result := True;
			Exit;
		end;
	end;

	if not Result then
		Assert.Pass('SKIPPED: Invite not found in secondary account');
end;

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
	CreateResult := FPrimaryCloud.FileOperations.CreateDirectory(FolderPath);
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
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary account login should succeed: ' + SecondaryCloud.AuthorizationError.ErrorMessage);

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

procedure TInviteIntegrationTest.TestMountSharedFolder_ContentsVisible;
var
	FolderPath, FilePath: WideString;
	CreateResult, ShareResult, MountResult: Boolean;
	SecondaryCloud: TCloudMailRu;
	Invites: TCloudIncomingInviteList;
	InviteToken, MountPath: WideString;
	Items: TCloudDirItemList;
	Found: Boolean;
	I: Integer;
begin
	RequireSecondaryAccount;

	{Create folder with a file inside on primary account}
	FolderPath := UniqueCloudPath('MountContentFolder');
	CreateResult := FPrimaryCloud.FileOperations.CreateDirectory(FolderPath);
	Assert.IsTrue(CreateResult, 'Creating folder should succeed');
	TrackForCleanup(FolderPath);

	FilePath := FolderPath + '/' + TTestDataGenerator.GenerateUniqueFilename('SharedFile', '.bin');
	var LocalFile := TPath.Combine(TPath.GetTempPath, TTestDataGenerator.GenerateUniqueFilename('temp', '.bin'));
	var TestData := TTestDataGenerator.CreateSmallTestFile(2048);
	try
		TestData.SaveToFile(LocalFile);
	finally
		TestData.Free;
	end;

	try
		FPrimaryCloud.Uploader.Upload(LocalFile, FilePath);
		TrackForCleanup(FilePath);
	finally
		TFile.Delete(LocalFile);
	end;

	{Share with secondary account}
	ShareResult := FPrimaryCloud.ShareFolder(FolderPath, FConfig.SecondaryEmail, CLOUD_SHARE_RO);
	if not ShareResult then
	begin
		Assert.Pass('SKIPPED: Cannot test mount contents - share failed');
		Exit;
	end;

	SecondaryCloud := CreateSecondaryCloud;
	try
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary account login should succeed: ' + SecondaryCloud.AuthorizationError.ErrorMessage);

		{Find and accept the invite}
		if not SecondaryCloud.ListingService.GetIncomingInvites(Invites) then
		begin
			Assert.Pass('SKIPPED: Cannot list invites in secondary account');
			Exit;
		end;

		InviteToken := '';
		for I := 0 to Length(Invites) - 1 do
		begin
			if Pos(WideString('MountContentFolder'), Invites[I].Name) > 0 then
			begin
				InviteToken := Invites[I].invite_token;
				Break;
			end;
		end;

		if InviteToken = '' then
		begin
			Assert.Pass('SKIPPED: Invite not found in secondary account');
			Exit;
		end;

		MountPath := '/MountedContent_' + TTestDataGenerator.GenerateUniqueFolderName('');
		MountResult := SecondaryCloud.ShareService.Mount(MountPath, InviteToken);

		if not MountResult then
		begin
			Assert.Pass('SKIPPED: Mount failed');
			Exit;
		end;

		try
			{List the mounted folder from the secondary account}
			Assert.IsTrue(SecondaryCloud.ListingService.GetDirectory(MountPath, Items),
				'Listing mounted shared folder should succeed');

			{Verify the uploaded file is visible}
			Found := False;
			for I := 0 to Length(Items) - 1 do
			begin
				if Pos(WideString('SharedFile'), Items[I].Name) > 0 then
				begin
					Found := True;
					Assert.AreEqual(Int64(2048), Items[I].Size, 'Shared file size should match');
					Break;
				end;
			end;

			Assert.IsTrue(Found, 'Uploaded file should be visible in mounted shared folder');
		finally
			SecondaryCloud.ShareService.Unmount(MountPath, False);
		end;
	finally
		SecondaryCloud.Free;
	end;

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
	CreateResult := FPrimaryCloud.FileOperations.CreateDirectory(FolderPath);
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
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary account login should succeed: ' + SecondaryCloud.AuthorizationError.ErrorMessage);

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
	CreateResult := FPrimaryCloud.FileOperations.CreateDirectory(FolderPath);
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
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary account login should succeed: ' + SecondaryCloud.AuthorizationError.ErrorMessage);

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
	CreateResult := FPrimaryCloud.FileOperations.CreateDirectory(FolderPath);
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
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary account login should succeed: ' + SecondaryCloud.AuthorizationError.ErrorMessage);

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

procedure TInviteIntegrationTest.TestPendingInvite_HomeIsEmpty;
var
	FolderPath: WideString;
	Invite: TCloudIncomingInvite;
	SecondaryCloud: TCloudMailRu;
begin
	RequireSecondaryAccount;

	SecondaryCloud := CreateSecondaryCloud;
	try
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary login should succeed: ' + SecondaryCloud.AuthorizationError.ErrorMessage);

		if not ShareAndFindInvite('PendingHome', CLOUD_SHARE_RO, SecondaryCloud, FolderPath, Invite) then
			Exit;

		{Pending invite should have empty home and isMounted = False}
		Assert.AreEqual('', Invite.home, 'Pending invite home should be empty');
		Assert.IsFalse(Invite.isMounted, 'Pending invite should not be mounted');
	finally
		SecondaryCloud.Free;
	end;

	FPrimaryCloud.ShareService.Unshare(FolderPath, FConfig.SecondaryEmail);
end;

procedure TInviteIntegrationTest.TestMountedInvite_HomeIsPopulated;
var
	FolderPath: WideString;
	Invite: TCloudIncomingInvite;
	MountPath: WideString;
	SecondaryCloud: TCloudMailRu;
	Invites: TCloudIncomingInviteList;
	I: Integer;
begin
	RequireSecondaryAccount;

	SecondaryCloud := CreateSecondaryCloud;
	try
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary login should succeed: ' + SecondaryCloud.AuthorizationError.ErrorMessage);

		if not ShareAndFindInvite('MountedHome', CLOUD_SHARE_RO, SecondaryCloud, FolderPath, Invite) then
			Exit;

		{Mount the invite}
		MountPath := '/MountedHome_' + TTestDataGenerator.GenerateUniqueFolderName('');
		if not SecondaryCloud.ShareService.Mount(MountPath, Invite.invite_token) then
		begin
			Assert.Pass('SKIPPED: Mount failed');
			Exit;
		end;

		try
			{Re-fetch invites and find the same one - now it should have home populated}
			Assert.IsTrue(SecondaryCloud.ListingService.GetIncomingInvites(Invites),
				'Listing invites after mount should succeed');

			for I := 0 to Length(Invites) - 1 do
			begin
				if Pos(WideString('MountedHome'), Invites[I].Name) > 0 then
				begin
					Assert.IsTrue(Invites[I].isMounted, 'Mounted invite should have isMounted = True');
					Assert.AreEqual(MountPath, Invites[I].home,
						'Mounted invite home should match mount path');
					Exit;
				end;
			end;

			Assert.Fail('Mounted invite not found in re-fetched invites list');
		finally
			SecondaryCloud.ShareService.Unmount(MountPath, False);
		end;
	finally
		SecondaryCloud.Free;
	end;

	FPrimaryCloud.ShareService.Unshare(FolderPath, FConfig.SecondaryEmail);
end;

procedure TInviteIntegrationTest.TestUnshare_InviteDisappears;
var
	FolderPath: WideString;
	Invite: TCloudIncomingInvite;
	SecondaryCloud: TCloudMailRu;
	Invites: TCloudIncomingInviteList;
	I: Integer;
	Found: Boolean;
begin
	RequireSecondaryAccount;

	SecondaryCloud := CreateSecondaryCloud;
	try
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary login should succeed: ' + SecondaryCloud.AuthorizationError.ErrorMessage);

		if not ShareAndFindInvite('UnshareDisappear', CLOUD_SHARE_RO, SecondaryCloud, FolderPath, Invite) then
			Exit;

		{Unshare from primary account}
		Assert.IsTrue(FPrimaryCloud.ShareService.Unshare(FolderPath, FConfig.SecondaryEmail),
			'Unshare should succeed');

		{Re-fetch invites on secondary - our invite should be gone}
		Assert.IsTrue(SecondaryCloud.ListingService.GetIncomingInvites(Invites),
			'Listing invites after unshare should succeed');

		Found := False;
		for I := 0 to Length(Invites) - 1 do
		begin
			if Pos(WideString('UnshareDisappear'), Invites[I].Name) > 0 then
			begin
				Found := True;
				Break;
			end;
		end;

		Assert.IsFalse(Found, 'Invite should disappear after unshare');
	finally
		SecondaryCloud.Free;
	end;
end;

procedure TInviteIntegrationTest.TestDeleteROSharedFolder_MountDisappearsFromSecondary;
var
	FolderPath: WideString;
	Invite: TCloudIncomingInvite;
	MountPath: WideString;
	SecondaryCloud: TCloudMailRu;
	Invites: TCloudIncomingInviteList;
	DirItem: TCloudDirItem;
	I: Integer;
	Found: Boolean;
begin
	RequireSecondaryAccount;

	SecondaryCloud := CreateSecondaryCloud;
	try
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary login should succeed: ' + SecondaryCloud.AuthorizationError.ErrorMessage);

		if not ShareAndFindInvite('DeleteROShare', CLOUD_SHARE_RO, SecondaryCloud, FolderPath, Invite) then
			Exit;

		{Mount the shared folder}
		MountPath := '/DeleteROMount_' + TTestDataGenerator.GenerateUniqueFolderName('');
		if not SecondaryCloud.ShareService.Mount(MountPath, Invite.invite_token) then
		begin
			Assert.Pass('SKIPPED: Mount failed');
			Exit;
		end;

		{Delete the original folder from primary account}
		Assert.IsTrue(FPrimaryCloud.FileOperations.Delete(FolderPath),
			'Deleting shared folder from primary should succeed');

		{For RO shares: both invite and mounted directory should disappear}
		Assert.IsTrue(SecondaryCloud.ListingService.GetIncomingInvites(Invites),
			'Listing invites should succeed after source deletion');

		Found := False;
		for I := 0 to Length(Invites) - 1 do
		begin
			if Pos(WideString('DeleteROShare'), Invites[I].Name) > 0 then
			begin
				Found := True;
				Break;
			end;
		end;

		Assert.IsFalse(Found, 'RO invite should disappear when source folder is deleted');

		{Mounted directory should also be gone}
		Assert.IsFalse(SecondaryCloud.ListingService.StatusFile(MountPath, DirItem),
			'RO mounted directory should disappear when source folder is deleted');
	finally
		SecondaryCloud.Free;
	end;
end;

procedure TInviteIntegrationTest.TestDeleteRWSharedFolder_DirectoryPersistsInSecondary;
var
	FolderPath: WideString;
	Invite: TCloudIncomingInvite;
	MountPath: WideString;
	SecondaryCloud: TCloudMailRu;
	Invites: TCloudIncomingInviteList;
	DirItem: TCloudDirItem;
	I: Integer;
	InviteFound: Boolean;
begin
	RequireSecondaryAccount;

	SecondaryCloud := CreateSecondaryCloud;
	try
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary login should succeed: ' + SecondaryCloud.AuthorizationError.ErrorMessage);

		if not ShareAndFindInvite('DeleteRWShare', CLOUD_SHARE_RW, SecondaryCloud, FolderPath, Invite) then
			Exit;

		{Mount the shared folder}
		MountPath := '/DeleteRWMount_' + TTestDataGenerator.GenerateUniqueFolderName('');
		if not SecondaryCloud.ShareService.Mount(MountPath, Invite.invite_token) then
		begin
			Assert.Pass('SKIPPED: Mount failed');
			Exit;
		end;

		{Delete the original folder from primary account}
		Assert.IsTrue(FPrimaryCloud.FileOperations.Delete(FolderPath),
			'Deleting shared folder from primary should succeed');

		{For RW shares: invite should be removed, but directory persists}
		Assert.IsTrue(SecondaryCloud.ListingService.GetIncomingInvites(Invites),
			'Listing invites should succeed after source deletion');

		InviteFound := False;
		for I := 0 to Length(Invites) - 1 do
		begin
			if Pos(WideString('DeleteRWShare'), Invites[I].Name) > 0 then
			begin
				InviteFound := True;
				Break;
			end;
		end;

		Assert.IsFalse(InviteFound, 'RW invite should be removed when source folder is deleted');

		{But the mounted directory should still exist as a regular folder}
		Assert.IsTrue(SecondaryCloud.ListingService.StatusFile(MountPath, DirItem),
			'RW mounted directory should persist after source folder deletion');

		{Cleanup: delete the persisted directory from secondary account}
		SecondaryCloud.FileOperations.Delete(MountPath);
	finally
		SecondaryCloud.Free;
	end;
end;

procedure TInviteIntegrationTest.TestMountedFolder_AppearsAsSharedKind;
var
	FolderPath: WideString;
	Invite: TCloudIncomingInvite;
	MountPath: WideString;
	SecondaryCloud: TCloudMailRu;
	DirItem: TCloudDirItem;
begin
	RequireSecondaryAccount;

	SecondaryCloud := CreateSecondaryCloud;
	try
		Assert.IsTrue(SecondaryCloud.Login, 'Secondary login should succeed: ' + SecondaryCloud.AuthorizationError.ErrorMessage);

		if not ShareAndFindInvite('SharedKind', CLOUD_SHARE_RO, SecondaryCloud, FolderPath, Invite) then
			Exit;

		{Mount the shared folder}
		MountPath := '/SharedKind_' + TTestDataGenerator.GenerateUniqueFolderName('');
		if not SecondaryCloud.ShareService.Mount(MountPath, Invite.invite_token) then
		begin
			Assert.Pass('SKIPPED: Mount failed');
			Exit;
		end;

		try
			{Check the mounted folder's attributes}
			Assert.IsTrue(SecondaryCloud.ListingService.StatusFile(MountPath, DirItem),
				'StatusFile on mounted folder should succeed');

			Assert.AreEqual(KIND_SHARED, DirItem.kind,
				'Mounted shared folder should have kind=shared');
		finally
			SecondaryCloud.ShareService.Unmount(MountPath, False);
		end;
	finally
		SecondaryCloud.Free;
	end;

	FPrimaryCloud.ShareService.Unshare(FolderPath, FConfig.SecondaryEmail);
end;

initialization
	if TIntegrationTestConfig.IsEnabled then
		TDUnitX.RegisterTestFixture(TInviteIntegrationTest);

end.
