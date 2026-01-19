unit IDescriptionSyncGuardInterface;

{Interface for guarded description sync operations.
 Encapsulates the condition checks (settings + account support) that determine
 whether description sync operations should be performed.}

interface

uses
	RealPath,
	CloudMailRu;

type
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

implementation

end.
