unit TimestampSyncManager;

{Manages timestamp metadata synchronization between local and remote storage.
	Stores file modification times to enable mtime preservation across upload/download cycles.
	Inherits shared delete/rename lifecycle from TBaseRemoteMetadataSyncManager.}

interface

uses
	SysUtils,
	RealPath,
	TimestampMetadata,
	TimestampEntry,
	FileSystem,
	RemoteMetadataStore,
	BaseRemoteMetadataSyncManager,
	CloudDescriptionOperationsAdapter;

type
	{Manages file timestamp metadata synchronization between local and remote storage}
	ITimestampSyncManager = interface
		['{D7F1A8E3-5C42-4B90-9A1D-8E3F7C2B6D54}']

		{Called after uploading a file - stores local mtime in remote metadata}
		procedure OnFileUploaded(const RemotePath: TRealPath; const LocalFilePath: WideString;
			Cloud: ICloudDescriptionOps);

		{Called after downloading a file - returns stored local mtime (0 = none available)}
		function OnFileDownloaded(const RemotePath: TRealPath; const LocalFilePath: WideString;
			CloudMTime: Int64; Cloud: ICloudDescriptionOps): Int64;

		{Called when a remote file is deleted - removes entry from remote metadata}
		procedure OnFileDeleted(const RemotePath: TRealPath; Cloud: ICloudDescriptionOps);

		{Called when a remote file is renamed or moved - transfers entry between metadata files}
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: ICloudDescriptionOps);
	end;

	TTimestampSyncManager = class(TBaseRemoteMetadataSyncManager, ITimestampSyncManager)
	private
		FConflictMode: Integer;
	protected
		function GetTempExt: WideString; override;
		function CreateStore(const FilePath: WideString): IRemoteMetadataStore; override;
	public
		constructor Create(const TimestampFileName: WideString; FileSystem: IFileSystem;
			ConflictMode: Integer);

		procedure OnFileUploaded(const RemotePath: TRealPath; const LocalFilePath: WideString;
			Cloud: ICloudDescriptionOps);
		function OnFileDownloaded(const RemotePath: TRealPath; const LocalFilePath: WideString;
			CloudMTime: Int64; Cloud: ICloudDescriptionOps): Int64;
		procedure OnFileDeleted(const RemotePath: TRealPath; Cloud: ICloudDescriptionOps);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: ICloudDescriptionOps);
	end;

implementation

uses
	PathHelper,
	SettingsConstants,
	TimestampStoreAdapter;

constructor TTimestampSyncManager.Create(const TimestampFileName: WideString;
	FileSystem: IFileSystem; ConflictMode: Integer);
begin
	inherited Create(TimestampFileName, FileSystem);
	FConflictMode := ConflictMode;
end;

function TTimestampSyncManager.GetTempExt: WideString;
begin
	Result := TIMESTAMP_TEMP_EXT;
end;

function TTimestampSyncManager.CreateStore(const FilePath: WideString): IRemoteMetadataStore;
begin
	Result := TTimestampStoreAdapter.Create(TTimestampMetadata.Create(FilePath, FFileSystem));
end;

procedure TTimestampSyncManager.OnFileDeleted(const RemotePath: TRealPath;
	Cloud: ICloudDescriptionOps);
begin
	HandleFileDeleted(RemotePath, Cloud);
end;

procedure TTimestampSyncManager.OnFileRenamed(const OldPath, NewPath: TRealPath;
	Cloud: ICloudDescriptionOps);
begin
	HandleFileRenamed(OldPath, NewPath, Cloud);
end;

procedure TTimestampSyncManager.OnFileUploaded(const RemotePath: TRealPath;
	const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);
var
	Metadata: TTimestampMetadata;
	RemoteMetaPath, LocalTempPath: WideString;
	RemoteMetaExists: Boolean;
	Entry: TTimestampEntry;
	LocalMTime: Int64;
begin
	LocalMTime := FFileSystem.GetFileModTime(LocalFilePath);
	if LocalMTime = 0 then
		Exit; {Cannot read local file mtime, nothing to store}

	RemoteMetaPath := GetRemoteMetadataPath(ExtractFileDir(RemotePath.Path));
	LocalTempPath := FFileSystem.GetTmpFileName(GetTempExt);
	try
		RemoteMetaExists := Cloud.GetDescriptionFile(RemoteMetaPath, LocalTempPath);

		Metadata := TTimestampMetadata.Create(LocalTempPath, FFileSystem);
		try
			if RemoteMetaExists then
				Metadata.Read;

			Entry.LocalMTime := LocalMTime;
			Entry.CloudMTime := 0; {Unknown until next download}
			Metadata.SetEntry(ExtractFileName(RemotePath.Path), Entry);
			Metadata.Write();

			if RemoteMetaExists then
				Cloud.DeleteFile(RemoteMetaPath);
			Cloud.PutDescriptionFile(RemoteMetaPath, Metadata.MetadataFileName);
		finally
			Metadata.Free;
		end;
	finally
		FFileSystem.DeleteFile(LocalTempPath);
	end;
end;

function TTimestampSyncManager.OnFileDownloaded(const RemotePath: TRealPath;
	const LocalFilePath: WideString; CloudMTime: Int64;
	Cloud: ICloudDescriptionOps): Int64;
var
	Metadata: TTimestampMetadata;
	RemoteMetaPath, LocalTempPath: WideString;
	Entry: TTimestampEntry;
begin
	Result := 0;

	RemoteMetaPath := GetRemoteMetadataPath(ExtractFileDir(RemotePath.Path));

	if not DownloadRemoteMetadata(RemoteMetaPath, Cloud, LocalTempPath) then
		Exit; {No metadata file exists}

	try
		Metadata := TTimestampMetadata.Create(LocalTempPath, FFileSystem);
		try
			Metadata.Read;
			Entry := Metadata.GetEntry(ExtractFileName(RemotePath.Path));

			if Entry.IsEmpty then
				Exit; {No stored timestamp for this file}

			{Conflict detection: if stored cloud_mtime differs from actual,
			 the file was modified on cloud since we last uploaded}
			if (Entry.CloudMTime <> 0) and (CloudMTime <> 0) and
				(Entry.CloudMTime <> CloudMTime) then
			begin
				if FConflictMode = TimestampConflictUseServer then
					Exit; {Return 0 = use server time}
			end;

			Result := Entry.LocalMTime;
		finally
			Metadata.Free;
		end;
	finally
		FFileSystem.DeleteFile(LocalTempPath);
	end;
end;

end.
