unit TimestampSyncGuardTest;

{Unit tests for TTimestampSyncGuard - guarded timestamp sync operations.
	Tests verify that sync operations are only called when corresponding settings are enabled,
	and that OnFileDownloaded returns 0 when disabled.}

interface

uses
	System.SysUtils,
	System.Classes,
	DUnitX.TestFramework,
	TimestampSyncGuard,
	TimestampSyncManager,
	PluginSettingsManager,
	CloudDescriptionOperationsAdapter,
	PluginSettings,
	RealPath,
	StreamingSettings;

type
	{Mock sync manager that tracks calls and returns configured values}
	TMockTimestampSyncManager = class(TInterfacedObject, ITimestampSyncManager)
	public
		UploadedCalls: Integer;
		DownloadedCalls: Integer;
		DeletedCalls: Integer;
		RenamedCalls: Integer;
		DownloadReturnValue: Int64;

		constructor Create;

		procedure OnFileUploaded(const RemotePath: TRealPath; const LocalFilePath: WideString;
			Cloud: ICloudDescriptionOps);
		function OnFileDownloaded(const RemotePath: TRealPath; const LocalFilePath: WideString;
			CloudMTime: Int64; Cloud: ICloudDescriptionOps): Int64;
		procedure OnFileDeleted(const RemotePath: TRealPath; Cloud: ICloudDescriptionOps);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: ICloudDescriptionOps);
	end;

	{Mock settings manager for controlling timestamp settings}
	TMockTimestampSettingsManager = class(TInterfacedObject, IPluginSettingsManager)
	private
		FSettings: TPluginSettings;
	public
		constructor Create;
		function GetSettings: TPluginSettings;
		procedure SetSettings(Value: TPluginSettings);
		procedure Save;
		procedure SwitchProxyPasswordStorage;
		function GetStreamingSettings(const FileName: WideString): TStreamingSettings;
		procedure SetStreamingSettings(const FileName: WideString; StreamSettings: TStreamingSettings);
		procedure GetStreamingExtensionsList(ExtensionsList: TStrings);
		procedure RemoveStreamingExtension(const Extension: WideString);
		function GetAccountsIniFilePath: WideString;
		procedure Refresh;

		procedure SetTimestampCopyToCloud(Value: Boolean);
		procedure SetTimestampCopyFromCloud(Value: Boolean);
		procedure SetTimestampTrackCloudFS(Value: Boolean);
	end;

	[TestFixture]
	TTimestampSyncGuardTest = class
	private
		FGuard: ITimestampSyncGuard;
		FSyncManager: TMockTimestampSyncManager;
		FSettingsManager: TMockTimestampSettingsManager;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{OnFileUploaded tests}
		[Test]
		procedure TestOnFileUploaded_WhenEnabled_CallsSyncManager;
		[Test]
		procedure TestOnFileUploaded_WhenDisabled_DoesNotCall;

		{OnFileDownloaded tests}
		[Test]
		procedure TestOnFileDownloaded_WhenEnabled_CallsSyncManager;
		[Test]
		procedure TestOnFileDownloaded_WhenEnabled_ReturnsManagerResult;
		[Test]
		procedure TestOnFileDownloaded_WhenDisabled_ReturnsZero;

		{OnFileDeleted tests}
		[Test]
		procedure TestOnFileDeleted_WhenEnabled_CallsSyncManager;
		[Test]
		procedure TestOnFileDeleted_WhenDisabled_DoesNotCall;

		{OnFileRenamed tests}
		[Test]
		procedure TestOnFileRenamed_WhenEnabled_CallsSyncManager;
		[Test]
		procedure TestOnFileRenamed_WhenDisabled_DoesNotCall;
	end;

implementation

{TMockTimestampSyncManager}

constructor TMockTimestampSyncManager.Create;
begin
	inherited Create;
	UploadedCalls := 0;
	DownloadedCalls := 0;
	DeletedCalls := 0;
	RenamedCalls := 0;
	DownloadReturnValue := 0;
end;

procedure TMockTimestampSyncManager.OnFileUploaded(const RemotePath: TRealPath;
	const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);
begin
	Inc(UploadedCalls);
end;

function TMockTimestampSyncManager.OnFileDownloaded(const RemotePath: TRealPath;
	const LocalFilePath: WideString; CloudMTime: Int64;
	Cloud: ICloudDescriptionOps): Int64;
begin
	Inc(DownloadedCalls);
	Result := DownloadReturnValue;
end;

procedure TMockTimestampSyncManager.OnFileDeleted(const RemotePath: TRealPath;
	Cloud: ICloudDescriptionOps);
begin
	Inc(DeletedCalls);
end;

procedure TMockTimestampSyncManager.OnFileRenamed(const OldPath, NewPath: TRealPath;
	Cloud: ICloudDescriptionOps);
begin
	Inc(RenamedCalls);
end;

{TMockTimestampSettingsManager}

constructor TMockTimestampSettingsManager.Create;
begin
	inherited Create;
	FSettings.TimestampCopyToCloud := False;
	FSettings.TimestampCopyFromCloud := False;
	FSettings.TimestampTrackCloudFS := False;
end;

function TMockTimestampSettingsManager.GetSettings: TPluginSettings;
begin
	Result := FSettings;
end;

procedure TMockTimestampSettingsManager.SetSettings(Value: TPluginSettings);
begin
	FSettings := Value;
end;

procedure TMockTimestampSettingsManager.Save;
begin
end;

procedure TMockTimestampSettingsManager.SwitchProxyPasswordStorage;
begin
end;

function TMockTimestampSettingsManager.GetStreamingSettings(const FileName: WideString): TStreamingSettings;
begin
	Result := Default(TStreamingSettings);
end;

procedure TMockTimestampSettingsManager.SetStreamingSettings(const FileName: WideString; StreamSettings: TStreamingSettings);
begin
end;

procedure TMockTimestampSettingsManager.GetStreamingExtensionsList(ExtensionsList: TStrings);
begin
	ExtensionsList.Clear;
end;

procedure TMockTimestampSettingsManager.RemoveStreamingExtension(const Extension: WideString);
begin
end;

function TMockTimestampSettingsManager.GetAccountsIniFilePath: WideString;
begin
	Result := EmptyWideStr;
end;

procedure TMockTimestampSettingsManager.Refresh;
begin
end;

procedure TMockTimestampSettingsManager.SetTimestampCopyToCloud(Value: Boolean);
begin
	FSettings.TimestampCopyToCloud := Value;
end;

procedure TMockTimestampSettingsManager.SetTimestampCopyFromCloud(Value: Boolean);
begin
	FSettings.TimestampCopyFromCloud := Value;
end;

procedure TMockTimestampSettingsManager.SetTimestampTrackCloudFS(Value: Boolean);
begin
	FSettings.TimestampTrackCloudFS := Value;
end;

{TTimestampSyncGuardTest}

procedure TTimestampSyncGuardTest.Setup;
begin
	FSyncManager := TMockTimestampSyncManager.Create;
	FSettingsManager := TMockTimestampSettingsManager.Create;
	FGuard := TTimestampSyncGuard.Create(FSyncManager, FSettingsManager);
end;

procedure TTimestampSyncGuardTest.TearDown;
begin
	FGuard := nil;
	FSyncManager := nil;
	FSettingsManager := nil;
end;

{OnFileUploaded tests}

procedure TTimestampSyncGuardTest.TestOnFileUploaded_WhenEnabled_CallsSyncManager;
var
	Path: TRealPath;
begin
	FSettingsManager.SetTimestampCopyToCloud(True);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileUploaded(Path, 'C:\local\file.txt', nil);

	Assert.AreEqual(1, FSyncManager.UploadedCalls, 'Should call sync manager once');
end;

procedure TTimestampSyncGuardTest.TestOnFileUploaded_WhenDisabled_DoesNotCall;
var
	Path: TRealPath;
begin
	FSettingsManager.SetTimestampCopyToCloud(False);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileUploaded(Path, 'C:\local\file.txt', nil);

	Assert.AreEqual(0, FSyncManager.UploadedCalls, 'Should not call sync manager');
end;

{OnFileDownloaded tests}

procedure TTimestampSyncGuardTest.TestOnFileDownloaded_WhenEnabled_CallsSyncManager;
var
	Path: TRealPath;
begin
	FSettingsManager.SetTimestampCopyFromCloud(True);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileDownloaded(Path, 'C:\local\file.txt', 0, nil);

	Assert.AreEqual(1, FSyncManager.DownloadedCalls, 'Should call sync manager once');
end;

procedure TTimestampSyncGuardTest.TestOnFileDownloaded_WhenEnabled_ReturnsManagerResult;
var
	Path: TRealPath;
	StoredMTime: Int64;
begin
	FSettingsManager.SetTimestampCopyFromCloud(True);
	FSyncManager.DownloadReturnValue := 1704067200;
	Path.FromPath('\account\file.txt');

	StoredMTime := FGuard.OnFileDownloaded(Path, 'C:\local\file.txt', 0, nil);

	Assert.AreEqual(Int64(1704067200), StoredMTime);
end;

procedure TTimestampSyncGuardTest.TestOnFileDownloaded_WhenDisabled_ReturnsZero;
var
	Path: TRealPath;
	StoredMTime: Int64;
begin
	FSettingsManager.SetTimestampCopyFromCloud(False);
	FSyncManager.DownloadReturnValue := 1704067200; {Would return this if enabled}
	Path.FromPath('\account\file.txt');

	StoredMTime := FGuard.OnFileDownloaded(Path, 'C:\local\file.txt', 0, nil);

	Assert.AreEqual(Int64(0), StoredMTime, 'Should return 0 when disabled');
	Assert.AreEqual(0, FSyncManager.DownloadedCalls, 'Should not call sync manager');
end;

{OnFileDeleted tests}

procedure TTimestampSyncGuardTest.TestOnFileDeleted_WhenEnabled_CallsSyncManager;
var
	Path: TRealPath;
begin
	FSettingsManager.SetTimestampTrackCloudFS(True);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileDeleted(Path, nil);

	Assert.AreEqual(1, FSyncManager.DeletedCalls, 'Should call sync manager once');
end;

procedure TTimestampSyncGuardTest.TestOnFileDeleted_WhenDisabled_DoesNotCall;
var
	Path: TRealPath;
begin
	FSettingsManager.SetTimestampTrackCloudFS(False);
	Path.FromPath('\account\file.txt');

	FGuard.OnFileDeleted(Path, nil);

	Assert.AreEqual(0, FSyncManager.DeletedCalls, 'Should not call sync manager');
end;

{OnFileRenamed tests}

procedure TTimestampSyncGuardTest.TestOnFileRenamed_WhenEnabled_CallsSyncManager;
var
	OldPath, NewPath: TRealPath;
begin
	FSettingsManager.SetTimestampTrackCloudFS(True);
	OldPath.FromPath('\account\old.txt');
	NewPath.FromPath('\account\new.txt');

	FGuard.OnFileRenamed(OldPath, NewPath, nil);

	Assert.AreEqual(1, FSyncManager.RenamedCalls, 'Should call sync manager once');
end;

procedure TTimestampSyncGuardTest.TestOnFileRenamed_WhenDisabled_DoesNotCall;
var
	OldPath, NewPath: TRealPath;
begin
	FSettingsManager.SetTimestampTrackCloudFS(False);
	OldPath.FromPath('\account\old.txt');
	NewPath.FromPath('\account\new.txt');

	FGuard.OnFileRenamed(OldPath, NewPath, nil);

	Assert.AreEqual(0, FSyncManager.RenamedCalls, 'Should not call sync manager');
end;

initialization
	TDUnitX.RegisterTestFixture(TTimestampSyncGuardTest);

end.
