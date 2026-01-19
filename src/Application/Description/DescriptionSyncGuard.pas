unit DescriptionSyncGuard;

{Guarded description sync operations.
 Encapsulates condition checks (settings + account support) before delegating
 to the underlying sync manager.}

interface

uses
	DescriptionSyncManager,
	IPluginSettingsManagerInterface,
	IAccountsManagerInterface,
	CloudDescriptionOpsAdapter,
	RealPath,
	CloudMailRu;

type
	{Interface for guarded description sync operations.
	 Encapsulates the condition checks (settings + account support) that determine
	 whether description sync operations should be performed.}
	IDescriptionSyncGuard = interface
		['{C4E8A2D6-7F3B-4E1A-9D5C-8B2F6A4E1C9D}']

		{Sync description on file/directory deletion.
		 Checks: DescriptionTrackCloudFS AND account.IsRemoteDescriptionsSupported
		 @param RealPath Path of deleted item
		 @param Cloud Cloud connection for description operations}
		procedure OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);

		{Sync description on file/directory rename or move.
		 Checks: DescriptionTrackCloudFS AND account.IsRemoteDescriptionsSupported
		 @param OldPath Original path
		 @param NewPath New path after rename/move
		 @param Cloud Cloud connection for description operations}
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);

		{Sync description on file download (copy description from cloud to local).
		 Checks: DescriptionCopyFromCloud only (no account check needed)
		 @param RealPath Remote path of downloaded file
		 @param LocalPath Local destination path
		 @param Cloud Cloud connection for description operations}
		procedure OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);

		{Sync description on file upload (copy description from local to cloud).
		 Checks: DescriptionCopyToCloud AND account.IsRemoteDescriptionsSupported
		 @param RealPath Remote destination path
		 @param LocalPath Local source path
		 @param Cloud Cloud connection for description operations}
		procedure OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
	end;

	TDescriptionSyncGuard = class(TInterfacedObject, IDescriptionSyncGuard)
	private
		FSyncManager: IDescriptionSyncManager;
		FSettingsManager: IPluginSettingsManager;
		FAccountsManager: IAccountsManager;

		function IsTrackingEnabled(const Account: WideString): Boolean;
		function IsUploadEnabled(const Account: WideString): Boolean;
	public
		{Create guard with required dependencies.
		 @param SyncManager Underlying sync manager to delegate to
		 @param SettingsManager Plugin settings for checking sync flags
		 @param AccountsManager Account settings for checking remote description support}
		constructor Create(
			SyncManager: IDescriptionSyncManager;
			SettingsManager: IPluginSettingsManager;
			AccountsManager: IAccountsManager
		);

		procedure OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
		procedure OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
		procedure OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
	end;

implementation

constructor TDescriptionSyncGuard.Create(
	SyncManager: IDescriptionSyncManager;
	SettingsManager: IPluginSettingsManager;
	AccountsManager: IAccountsManager
);
begin
	inherited Create;
	FSyncManager := SyncManager;
	FSettingsManager := SettingsManager;
	FAccountsManager := AccountsManager;
end;

function TDescriptionSyncGuard.IsTrackingEnabled(const Account: WideString): Boolean;
begin
	Result := FSettingsManager.GetSettings.DescriptionTrackCloudFS
		and FAccountsManager.GetAccountSettings(Account).IsRemoteDescriptionsSupported;
end;

function TDescriptionSyncGuard.IsUploadEnabled(const Account: WideString): Boolean;
begin
	Result := FSettingsManager.GetSettings.DescriptionCopyToCloud
		and FAccountsManager.GetAccountSettings(Account).IsRemoteDescriptionsSupported;
end;

procedure TDescriptionSyncGuard.OnFileDeleted(const RealPath: TRealPath; Cloud: TCloudMailRu);
begin
	if IsTrackingEnabled(RealPath.account) then
		FSyncManager.OnFileDeleted(RealPath, TCloudDescriptionOpsAdapter.Create(Cloud));
end;

procedure TDescriptionSyncGuard.OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: TCloudMailRu);
begin
	if IsTrackingEnabled(NewPath.account) then
		FSyncManager.OnFileRenamed(OldPath, NewPath, TCloudDescriptionOpsAdapter.Create(Cloud));
end;

procedure TDescriptionSyncGuard.OnFileDownloaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
	{Download only checks DescriptionCopyFromCloud - no account support check needed}
	if FSettingsManager.GetSettings.DescriptionCopyFromCloud then
		FSyncManager.OnFileDownloaded(RealPath, LocalPath, TCloudDescriptionOpsAdapter.Create(Cloud));
end;

procedure TDescriptionSyncGuard.OnFileUploaded(const RealPath: TRealPath; const LocalPath: WideString; Cloud: TCloudMailRu);
begin
	if IsUploadEnabled(RealPath.account) then
		FSyncManager.OnFileUploaded(RealPath, LocalPath, TCloudDescriptionOpsAdapter.Create(Cloud));
end;

end.
