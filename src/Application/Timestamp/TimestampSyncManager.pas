unit TimestampSyncManager;

{Manages timestamp metadata synchronization between local and remote storage.
	Mirrors TDescriptionSyncManager pattern but stores file modification times
	instead of text descriptions, enabling mtime preservation across upload/download cycles.}

interface

uses
	SysUtils,
	RealPath,
	TimestampMetadata,
	TimestampEntry,
	FileSystem,
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

	TTimestampSyncManager = class(TInterfacedObject, ITimestampSyncManager)
	private
		FTimestampFileName: WideString;
		FFileSystem: IFileSystem;
		FConflictMode: Integer;

		function GetRemoteMetadataPath(const DirPath: WideString): WideString;
		function DownloadRemoteMetadata(const RemoteMetaPath: WideString; Cloud: ICloudDescriptionOps;
			out LocalTempPath: WideString): Boolean;
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
	SettingsConstants;

constructor TTimestampSyncManager.Create(const TimestampFileName: WideString;
	FileSystem: IFileSystem; ConflictMode: Integer);
begin
	inherited Create;
	FTimestampFileName := TimestampFileName;
	FFileSystem := FileSystem;
	FConflictMode := ConflictMode;
end;

function TTimestampSyncManager.GetRemoteMetadataPath(const DirPath: WideString): WideString;
begin
	Result := IncludeTrailingBackslash(DirPath) + FTimestampFileName;
end;

function TTimestampSyncManager.DownloadRemoteMetadata(const RemoteMetaPath: WideString;
	Cloud: ICloudDescriptionOps; out LocalTempPath: WideString): Boolean;
begin
	LocalTempPath := FFileSystem.GetTmpFileName(TIMESTAMP_TEMP_EXT);
	Result := Cloud.GetDescriptionFile(RemoteMetaPath, LocalTempPath);
	if not Result then
		FFileSystem.DeleteFile(LocalTempPath);
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
	LocalTempPath := FFileSystem.GetTmpFileName(TIMESTAMP_TEMP_EXT);
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

procedure TTimestampSyncManager.OnFileDeleted(const RemotePath: TRealPath;
	Cloud: ICloudDescriptionOps);
var
	Metadata: TTimestampMetadata;
	RemoteMetaPath, LocalTempPath: WideString;
begin
	RemoteMetaPath := GetRemoteMetadataPath(ExtractFileDir(RemotePath.Path));

	if not DownloadRemoteMetadata(RemoteMetaPath, Cloud, LocalTempPath) then
		Exit; {No metadata file exists}

	try
		Metadata := TTimestampMetadata.Create(LocalTempPath, FFileSystem);
		try
			Metadata.Read;
			Metadata.DeleteEntry(ExtractFileName(RemotePath.Path));
			Metadata.Write();
			Cloud.DeleteFile(RemoteMetaPath);
			Cloud.PutDescriptionFile(RemoteMetaPath, Metadata.MetadataFileName);
		finally
			Metadata.Free;
		end;
	finally
		FFileSystem.DeleteFile(LocalTempPath);
	end;
end;

procedure TTimestampSyncManager.OnFileRenamed(const OldPath, NewPath: TRealPath;
	Cloud: ICloudDescriptionOps);
var
	OldMetadata, NewMetadata: TTimestampMetadata;
	OldRemoteMetaPath, NewRemoteMetaPath, OldLocalTempPath, NewLocalTempPath: WideString;
	NewRemoteMetaExists: Boolean;
	OldItem, NewItem: WideString;
	Entry: TTimestampEntry;
begin
	OldItem := ExtractFileName(OldPath.Path);
	NewItem := ExtractFileName(NewPath.Path);
	OldRemoteMetaPath := GetRemoteMetadataPath(ExtractFileDir(OldPath.Path));

	if ExtractFileDir(OldPath.Path) = ExtractFileDir(NewPath.Path) then
	begin
		{Rename within same directory - modify single metadata file}
		if not DownloadRemoteMetadata(OldRemoteMetaPath, Cloud, OldLocalTempPath) then
			Exit;

		try
			OldMetadata := TTimestampMetadata.Create(OldLocalTempPath, FFileSystem);
			try
				OldMetadata.Read;
				if OldMetadata.RenameEntry(OldItem, NewItem) then
				begin
					OldMetadata.Write();
					Cloud.DeleteFile(OldRemoteMetaPath);
					Cloud.PutDescriptionFile(OldRemoteMetaPath, OldMetadata.MetadataFileName);
				end;
			finally
				OldMetadata.Free;
			end;
		finally
			FFileSystem.DeleteFile(OldLocalTempPath);
		end;
	end else begin
		{Move between directories - transfer entry between two metadata files}
		NewRemoteMetaPath := GetRemoteMetadataPath(ExtractFileDir(NewPath.Path));
		if not DownloadRemoteMetadata(OldRemoteMetaPath, Cloud, OldLocalTempPath) then
			Exit;

		try
			OldMetadata := TTimestampMetadata.Create(OldLocalTempPath, FFileSystem);
			try
				OldMetadata.Read;
				Entry := OldMetadata.GetEntry(OldItem);
				if Entry.IsEmpty then
					Exit; {No entry to transfer}

				NewLocalTempPath := FFileSystem.GetTmpFileName(TIMESTAMP_TEMP_EXT);
				try
					NewRemoteMetaExists := Cloud.GetDescriptionFile(NewRemoteMetaPath, NewLocalTempPath);
					NewMetadata := TTimestampMetadata.Create(NewLocalTempPath, FFileSystem);
					try
						if NewRemoteMetaExists then
							NewMetadata.Read;

						NewMetadata.SetEntry(NewItem, Entry);
						OldMetadata.DeleteEntry(OldItem);
						OldMetadata.Write();
						NewMetadata.Write();

						Cloud.DeleteFile(OldRemoteMetaPath);
						Cloud.PutDescriptionFile(OldRemoteMetaPath, OldMetadata.MetadataFileName);

						if NewRemoteMetaExists then
							Cloud.DeleteFile(NewRemoteMetaPath);
						Cloud.PutDescriptionFile(NewRemoteMetaPath, NewMetadata.MetadataFileName);
					finally
						NewMetadata.Free;
					end;
				finally
					FFileSystem.DeleteFile(NewLocalTempPath);
				end;
			finally
				OldMetadata.Free;
			end;
		finally
			FFileSystem.DeleteFile(OldLocalTempPath);
		end;
	end;
end;

end.
