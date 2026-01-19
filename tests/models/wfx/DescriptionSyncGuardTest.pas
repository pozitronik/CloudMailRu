unit DescriptionSyncGuardTest;

{Unit tests for TDescriptionSyncGuard - guarded description sync operations.
 Tests verify that sync operations are only called when conditions are met.}

interface

uses
	System.SysUtils,
	DUnitX.TestFramework,
	IDescriptionSyncGuardInterface,
	IDescriptionSyncManagerInterface,
	IPluginSettingsManagerInterface,
	IAccountsManagerInterface,
	ICloudDescriptionOpsInterface,
	DescriptionSyncGuard,
	PluginSettings,
	AccountSettings,
	RealPath;

type
	{Mock sync manager that tracks calls}
	TMockDescriptionSyncManager = class(TInterfacedObject, IDescriptionSyncManager)
	public
		DeletedCalls: Integer;
		RenamedCalls: Integer;
		DownloadedCalls: Integer;
		UploadedCalls: Integer;
		LastDeletedPath: WideString;
		LastRenamedOldPath: WideString;
		LastRenamedNewPath: WideString;

		constructor Create;
		procedure Reset;

		procedure OnFileDeleted(const RealPath: TRealPath; CloudOps: ICloudDescriptionOps);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; CloudOps: ICloudDescriptionOps);
		procedure OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; CloudOps: ICloudDescriptionOps);
		procedure OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; CloudOps: ICloudDescriptionOps);
	end;

	{Mock settings manager}
	TMockSettingsManager = class(TInterfacedObject, IPluginSettingsManager)
	private
		FSettings: TPluginSettings;
	public
		constructor Create;
		function GetSettings: TPluginSettings;
		procedure SwitchProxyPasswordStorage;

		procedure SetDescriptionTrackCloudFS(Value: Boolean);
		procedure SetDescriptionCopyFromCloud(Value: Boolean);
		procedure SetDescriptionCopyToCloud(Value: Boolean);
	end;

	{Mock accounts manager}
	TMockAccountsManager = class(TInterfacedObject, IAccountsManager)
	private
		FIsRemoteDescriptionsSupported: Boolean;
	public
		constructor Create;
		function GetAccountSettings(Account: WideString): TAccountSettings;
		procedure SetAccountSettings(Account: WideString; Settings: TAccountSettings);
		procedure SwitchPasswordStorage(Account: WideString);
		procedure SetCryptedGUID(Account: WideString; GUID: WideString);

		procedure SetRemoteDescriptionsSupported(Value: Boolean);
	end;

	[TestFixture]
	TDescriptionSyncGuardTest = class
	private
		FGuard: IDescriptionSyncGuard;
		FSyncManager: TMockDescriptionSyncManager;
		FSettingsManager: TMockSettingsManager;
		FAccountsManager: TMockAccountsManager;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{OnFileDeleted tests}
		[Test]
		procedure TestOnFileDeleted_WhenEnabled_CallsSyncManager;
		[Test]
		procedure TestOnFileDeleted_WhenTrackingDisabled_DoesNotCall;
		[Test]
		procedure TestOnFileDeleted_WhenAccountNotSupported_DoesNotCall;

		{OnFileRenamed tests}
		[Test]
		procedure TestOnFileRenamed_WhenEnabled_CallsSyncManager;
		[Test]
		procedure TestOnFileRenamed_WhenDisabled_DoesNotCall;

		{OnFileDownloaded tests}
		[Test]
		procedure TestOnFileDownloaded_WhenEnabled_CallsSyncManager;
		[Test]
		procedure TestOnFileDownloaded_WhenDisabled_DoesNotCall;
		[Test]
		procedure TestOnFileDownloaded_IgnoresAccountSupport;

		{OnFileUploaded tests}
		[Test]
		procedure TestOnFileUploaded_WhenEnabled_CallsSyncManager;
		[Test]
		procedure TestOnFileUploaded_WhenDisabled_DoesNotCall;
		[Test]
		procedure TestOnFileUploaded_WhenAccountNotSupported_DoesNotCall;
	end;

implementation

{TMockDescriptionSyncManager}

constructor TMockDescriptionSyncManager.Create;
begin
	inherited Create;
	Reset;
end;

procedure TMockDescriptionSyncManager.Reset;
begin
	DeletedCalls := 0;
	RenamedCalls := 0;
	DownloadedCalls := 0;
	UploadedCalls := 0;
	LastDeletedPath := '';
	LastRenamedOldPath := '';
	LastRenamedNewPath := '';
end;

procedure TMockDescriptionSyncManager.OnFileDeleted(const RealPath: TRealPath; CloudOps: ICloudDescriptionOps);
begin
	Inc(DeletedCalls);
	LastDeletedPath := RealPath.Path;
end;

procedure TMockDescriptionSyncManager.OnFileRenamed(const OldPath, NewPath: TRealPath; CloudOps: ICloudDescriptionOps);
begin
	Inc(RenamedCalls);
	LastRenamedOldPath := OldPath.Path;
	LastRenamedNewPath := NewPath.Path;
end;

procedure TMockDescriptionSyncManager.OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; CloudOps: ICloudDescriptionOps);
begin
	Inc(DownloadedCalls);
end;

procedure TMockDescriptionSyncManager.OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; CloudOps: ICloudDescriptionOps);
begin
	Inc(UploadedCalls);
end;

{TMockSettingsManager}

constructor TMockSettingsManager.Create;
begin
	inherited Create;
	FSettings.DescriptionTrackCloudFS := False;
	FSettings.DescriptionCopyFromCloud := False;
	FSettings.DescriptionCopyToCloud := False;
end;

function TMockSettingsManager.GetSettings: TPluginSettings;
begin
	Result := FSettings;
end;

procedure TMockSettingsManager.SwitchProxyPasswordStorage;
begin
end;

procedure TMockSettingsManager.SetDescriptionTrackCloudFS(Value: Boolean);
begin
	FSettings.DescriptionTrackCloudFS := Value;
end;

procedure TMockSettingsManager.SetDescriptionCopyFromCloud(Value: Boolean);
begin
	FSettings.DescriptionCopyFromCloud := Value;
end;

procedure TMockSettingsManager.SetDescriptionCopyToCloud(Value: Boolean);
begin
	FSettings.DescriptionCopyToCloud := Value;
end;

{TMockAccountsManager}

constructor TMockAccountsManager.Create;
begin
	inherited Create;
	FIsRemoteDescriptionsSupported := False;
end;

function TMockAccountsManager.GetAccountSettings(Account: WideString): TAccountSettings;
begin
	Result := Default(TAccountSettings);
	{IsRemoteDescriptionsSupported is computed: not((EncryptFilesMode <> 0) and EncryptFileNames)}
	if FIsRemoteDescriptionsSupported then
		Result.EncryptFilesMode := 0 {EncryptModeNone - makes IsRemoteDescriptionsSupported = True}
	else begin
		Result.EncryptFilesMode := 1; {Any non-zero mode}
		Result.EncryptFileNames := True; {Combined with non-zero mode makes IsRemoteDescriptionsSupported = False}
	end;
end;

procedure TMockAccountsManager.SetAccountSettings(Account: WideString; Settings: TAccountSettings);
begin
end;

procedure TMockAccountsManager.SwitchPasswordStorage(Account: WideString);
begin
end;

procedure TMockAccountsManager.SetCryptedGUID(Account: WideString; GUID: WideString);
begin
end;

procedure TMockAccountsManager.SetRemoteDescriptionsSupported(Value: Boolean);
begin
	FIsRemoteDescriptionsSupported := Value;
end;

{TDescriptionSyncGuardTest}

procedure TDescriptionSyncGuardTest.Setup;
begin
	FSyncManager := TMockDescriptionSyncManager.Create;
	FSettingsManager := TMockSettingsManager.Create;
	FAccountsManager := TMockAccountsManager.Create;
	FGuard := TDescriptionSyncGuard.Create(FSyncManager, FSettingsManager, FAccountsManager);
end;

procedure TDescriptionSyncGuardTest.TearDown;
begin
	FGuard := nil;
	FSyncManager := nil;
	FSettingsManager := nil;
	FAccountsManager := nil;
end;

{OnFileDeleted tests}

procedure TDescriptionSyncGuardTest.TestOnFileDeleted_WhenEnabled_CallsSyncManager;
var
	Path: TRealPath;
begin
	FSettingsManager.SetDescriptionTrackCloudFS(True);
	FAccountsManager.SetRemoteDescriptionsSupported(True);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileDeleted(Path, nil);

	Assert.AreEqual(1, FSyncManager.DeletedCalls, 'Should call sync manager once');
end;

procedure TDescriptionSyncGuardTest.TestOnFileDeleted_WhenTrackingDisabled_DoesNotCall;
var
	Path: TRealPath;
begin
	FSettingsManager.SetDescriptionTrackCloudFS(False);
	FAccountsManager.SetRemoteDescriptionsSupported(True);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileDeleted(Path, nil);

	Assert.AreEqual(0, FSyncManager.DeletedCalls, 'Should not call sync manager');
end;

procedure TDescriptionSyncGuardTest.TestOnFileDeleted_WhenAccountNotSupported_DoesNotCall;
var
	Path: TRealPath;
begin
	FSettingsManager.SetDescriptionTrackCloudFS(True);
	FAccountsManager.SetRemoteDescriptionsSupported(False);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileDeleted(Path, nil);

	Assert.AreEqual(0, FSyncManager.DeletedCalls, 'Should not call sync manager');
end;

{OnFileRenamed tests}

procedure TDescriptionSyncGuardTest.TestOnFileRenamed_WhenEnabled_CallsSyncManager;
var
	OldPath, NewPath: TRealPath;
begin
	FSettingsManager.SetDescriptionTrackCloudFS(True);
	FAccountsManager.SetRemoteDescriptionsSupported(True);
	OldPath.FromPath('\account\old.txt');
	NewPath.FromPath('\account\new.txt');

	FGuard.OnFileRenamed(OldPath, NewPath, nil);

	Assert.AreEqual(1, FSyncManager.RenamedCalls, 'Should call sync manager once');
end;

procedure TDescriptionSyncGuardTest.TestOnFileRenamed_WhenDisabled_DoesNotCall;
var
	OldPath, NewPath: TRealPath;
begin
	FSettingsManager.SetDescriptionTrackCloudFS(False);
	FAccountsManager.SetRemoteDescriptionsSupported(True);
	OldPath.FromPath('\account\old.txt');
	NewPath.FromPath('\account\new.txt');

	FGuard.OnFileRenamed(OldPath, NewPath, nil);

	Assert.AreEqual(0, FSyncManager.RenamedCalls, 'Should not call sync manager');
end;

{OnFileDownloaded tests}

procedure TDescriptionSyncGuardTest.TestOnFileDownloaded_WhenEnabled_CallsSyncManager;
var
	Path: TRealPath;
begin
	FSettingsManager.SetDescriptionCopyFromCloud(True);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileDownloaded(Path, 'C:\local\file.txt', nil);

	Assert.AreEqual(1, FSyncManager.DownloadedCalls, 'Should call sync manager once');
end;

procedure TDescriptionSyncGuardTest.TestOnFileDownloaded_WhenDisabled_DoesNotCall;
var
	Path: TRealPath;
begin
	FSettingsManager.SetDescriptionCopyFromCloud(False);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileDownloaded(Path, 'C:\local\file.txt', nil);

	Assert.AreEqual(0, FSyncManager.DownloadedCalls, 'Should not call sync manager');
end;

procedure TDescriptionSyncGuardTest.TestOnFileDownloaded_IgnoresAccountSupport;
var
	Path: TRealPath;
begin
	FSettingsManager.SetDescriptionCopyFromCloud(True);
	FAccountsManager.SetRemoteDescriptionsSupported(False); {Account doesn't support, but should still work}
	Path.FromPath('\account\file.txt');

	FGuard.OnFileDownloaded(Path, 'C:\local\file.txt', nil);

	Assert.AreEqual(1, FSyncManager.DownloadedCalls, 'Should call sync manager (ignores account support)');
end;

{OnFileUploaded tests}

procedure TDescriptionSyncGuardTest.TestOnFileUploaded_WhenEnabled_CallsSyncManager;
var
	Path: TRealPath;
begin
	FSettingsManager.SetDescriptionCopyToCloud(True);
	FAccountsManager.SetRemoteDescriptionsSupported(True);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileUploaded(Path, 'C:\local\file.txt', nil);

	Assert.AreEqual(1, FSyncManager.UploadedCalls, 'Should call sync manager once');
end;

procedure TDescriptionSyncGuardTest.TestOnFileUploaded_WhenDisabled_DoesNotCall;
var
	Path: TRealPath;
begin
	FSettingsManager.SetDescriptionCopyToCloud(False);
	FAccountsManager.SetRemoteDescriptionsSupported(True);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileUploaded(Path, 'C:\local\file.txt', nil);

	Assert.AreEqual(0, FSyncManager.UploadedCalls, 'Should not call sync manager');
end;

procedure TDescriptionSyncGuardTest.TestOnFileUploaded_WhenAccountNotSupported_DoesNotCall;
var
	Path: TRealPath;
begin
	FSettingsManager.SetDescriptionCopyToCloud(True);
	FAccountsManager.SetRemoteDescriptionsSupported(False);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileUploaded(Path, 'C:\local\file.txt', nil);

	Assert.AreEqual(0, FSyncManager.UploadedCalls, 'Should not call sync manager');
end;

initialization
	TDUnitX.RegisterTestFixture(TDescriptionSyncGuardTest);

end.
