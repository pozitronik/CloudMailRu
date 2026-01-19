unit IDescriptionSyncManagerInterface;

interface

uses
	RealPath,
	ICloudDescriptionOpsInterface;

type
	{Manages file description synchronization between local and remote storage.
	 Handles description file (.ion) operations when files are created, deleted,
	 renamed, or transferred.}
	IDescriptionSyncManager = interface
		['{A1B2C3D4-E5F6-7890-ABCD-123456789ABC}']

		{Called when a remote file is deleted - removes entry from remote description file}
		procedure OnFileDeleted(const RemotePath: TRealPath; Cloud: ICloudDescriptionOps);

		{Called when a remote file is renamed or moved - transfers entry between description files}
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: ICloudDescriptionOps);

		{Called after downloading a file - copies description from remote to local}
		procedure OnFileDownloaded(const RemotePath: TRealPath; const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);

		{Called after uploading a file - copies description from local to remote}
		procedure OnFileUploaded(const RemotePath: TRealPath; const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);
	end;

implementation

end.
