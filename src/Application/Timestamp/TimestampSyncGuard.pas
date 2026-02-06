unit TimestampSyncGuard;

{Guarded timestamp sync operations.
	Encapsulates settings checks before delegating to the underlying sync manager.
	Mirrors TDescriptionSyncGuard pattern.}

interface

uses
	TimestampSyncManager,
	PluginSettingsManager,
	CloudDescriptionOperationsAdapter,
	RealPath,
	CloudMailRu;

type
	ITimestampSyncGuard = interface
		['{A3D7E5B1-2F84-4C96-B8E3-1D6F9A4C7E2B}']

		{Sync timestamp on file upload. Checks: TimestampCopyToCloud}
		procedure OnFileUploaded(const RemotePath: TRealPath; const LocalPath: WideString;
			Cloud: TCloudMailRu);

		{Sync timestamp on file download. Checks: TimestampCopyFromCloud.
			Returns stored local mtime (0 = none/disabled).}
		function OnFileDownloaded(const RemotePath: TRealPath; const LocalPath: WideString;
			CloudMTime: Int64; Cloud: TCloudMailRu): Int64;

		{Sync timestamp on file deletion. Checks: TimestampTrackCloudFS}
		procedure OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);

		{Sync timestamp on file rename/move. Checks: TimestampTrackCloudFS}
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
	end;

	TTimestampSyncGuard = class(TInterfacedObject, ITimestampSyncGuard)
	private
		FSyncManager: ITimestampSyncManager;
		FSettingsManager: IPluginSettingsManager;
	public
		constructor Create(SyncManager: ITimestampSyncManager; SettingsManager: IPluginSettingsManager);

		procedure OnFileUploaded(const RemotePath: TRealPath; const LocalPath: WideString;
			Cloud: TCloudMailRu);
		function OnFileDownloaded(const RemotePath: TRealPath; const LocalPath: WideString;
			CloudMTime: Int64; Cloud: TCloudMailRu): Int64;
		procedure OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
	end;

implementation

constructor TTimestampSyncGuard.Create(SyncManager: ITimestampSyncManager;
	SettingsManager: IPluginSettingsManager);
begin
	inherited Create;
	FSyncManager := SyncManager;
	FSettingsManager := SettingsManager;
end;

procedure TTimestampSyncGuard.OnFileUploaded(const RemotePath: TRealPath;
	const LocalPath: WideString; Cloud: TCloudMailRu);
begin
	if FSettingsManager.GetSettings.TimestampCopyToCloud then
		FSyncManager.OnFileUploaded(RemotePath, LocalPath,
			TCloudDescriptionOperationsAdapter.Create(Cloud));
end;

function TTimestampSyncGuard.OnFileDownloaded(const RemotePath: TRealPath;
	const LocalPath: WideString; CloudMTime: Int64; Cloud: TCloudMailRu): Int64;
begin
	if FSettingsManager.GetSettings.TimestampCopyFromCloud then
		Result := FSyncManager.OnFileDownloaded(RemotePath, LocalPath, CloudMTime,
			TCloudDescriptionOperationsAdapter.Create(Cloud))
	else
		Result := 0;
end;

procedure TTimestampSyncGuard.OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
begin
	if FSettingsManager.GetSettings.TimestampTrackCloudFS then
		FSyncManager.OnFileDeleted(RealPath, TCloudDescriptionOperationsAdapter.Create(Cloud));
end;

procedure TTimestampSyncGuard.OnFileRenamed(const OldPath, NewPath: TRealPath;
	Cloud: TCloudMailRu);
begin
	if FSettingsManager.GetSettings.TimestampTrackCloudFS then
		FSyncManager.OnFileRenamed(OldPath, NewPath,
			TCloudDescriptionOperationsAdapter.Create(Cloud));
end;

end.
