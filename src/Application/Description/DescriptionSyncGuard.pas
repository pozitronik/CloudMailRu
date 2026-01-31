unit DescriptionSyncGuard;

{Guarded description sync operations.
	Encapsulates condition checks (settings + account support) before delegating
	to the underlying sync manager.}

interface

uses
	DescriptionSyncManager,
	PluginSettingsManager,
	CloudDescriptionOperationsAdapter,
	RealPath,
	CloudMailRu;

type
	{Interface for guarded description sync operations.
		Encapsulates the condition checks (settings + account support) that determine
		whether description sync operations should be performed.}
	IDescriptionSyncGuard = interface
		['{C4E8A2D6-7F3B-4E1A-9D5C-8B2F6A4E1C9D}']

		{Sync description on file/directory deletion.
			Checks: DescriptionTrackCloudFS
			@param RealPath Path of deleted item
			@param Cloud Cloud connection for description operations}
		procedure OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);

		{Sync description on file/directory rename or move.
			Checks: DescriptionTrackCloudFS
			@param OldPath Original path
			@param NewPath New path after rename/move
			@param Cloud Cloud connection for description operations}
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);

		{Sync description on file download (copy description from cloud to local).
			Checks: DescriptionCopyFromCloud
			@param RealPath Remote path of downloaded file
			@param LocalPath Local destination path
			@param Cloud Cloud connection for description operations}
		procedure OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);

		{Sync description on file upload (copy description from local to cloud).
			Checks: DescriptionCopyToCloud
			@param RealPath Remote destination path
			@param LocalPath Local source path
			@param Cloud Cloud connection for description operations}
		procedure OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
	end;

	TDescriptionSyncGuard = class(TInterfacedObject, IDescriptionSyncGuard)
	private
		FSyncManager: IDescriptionSyncManager;
		FSettingsManager: IPluginSettingsManager;
	public
		{Create guard with required dependencies.
			@param SyncManager Underlying sync manager to delegate to
			@param SettingsManager Plugin settings for checking sync flags}
		constructor Create(SyncManager: IDescriptionSyncManager; SettingsManager: IPluginSettingsManager);

		procedure OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
		procedure OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
	end;

implementation

constructor TDescriptionSyncGuard.Create(SyncManager: IDescriptionSyncManager; SettingsManager: IPluginSettingsManager);
begin
	inherited Create;
	FSyncManager := SyncManager;
	FSettingsManager := SettingsManager;
end;

procedure TDescriptionSyncGuard.OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
begin
	if FSettingsManager.GetSettings.DescriptionTrackCloudFS then
		FSyncManager.OnFileDeleted(RealPath, TCloudDescriptionOperationsAdapter.Create(Cloud));
end;

procedure TDescriptionSyncGuard.OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
begin
	if FSettingsManager.GetSettings.DescriptionTrackCloudFS then
		FSyncManager.OnFileRenamed(OldPath, NewPath, TCloudDescriptionOperationsAdapter.Create(Cloud));
end;

procedure TDescriptionSyncGuard.OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
	if FSettingsManager.GetSettings.DescriptionCopyFromCloud then
		FSyncManager.OnFileDownloaded(RealPath, LocalPath, TCloudDescriptionOperationsAdapter.Create(Cloud));
end;

procedure TDescriptionSyncGuard.OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
	if FSettingsManager.GetSettings.DescriptionCopyToCloud then
		FSyncManager.OnFileUploaded(RealPath, LocalPath, TCloudDescriptionOperationsAdapter.Create(Cloud));
end;

end.
