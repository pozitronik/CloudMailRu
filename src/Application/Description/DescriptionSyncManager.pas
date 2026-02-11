unit DescriptionSyncManager;

{Manages file description synchronization between local and remote storage.
	Handles description file (.ion) operations when files are created, deleted,
	renamed, or transferred.
	Inherits shared delete/rename lifecycle from TBaseRemoteMetadataSyncManager.}

interface

uses
	SysUtils,
	RealPath,
	Description,
	FileSystem,
	RemoteMetadataStore,
	BaseRemoteMetadataSyncManager,
	CloudDescriptionOperationsAdapter,
	TCHandler;

type
	{Manages file description synchronization between local and remote storage.
		Handles description file (.ion) operations when files are created, deleted,
		renamed, or transferred.}
	IDescriptionSyncManager = interface
		['{BB56B2CC-B5E6-4454-8D7E-B14AA8A52345}']

		{Called when a remote file is deleted - removes entry from remote description file}
		procedure OnFileDeleted(const RemotePath: TRealPath; Cloud: ICloudDescriptionOps);

		{Called when a remote file is renamed or moved - transfers entry between description files}
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: ICloudDescriptionOps);

		{Called after downloading a file - copies description from remote to local}
		procedure OnFileDownloaded(const RemotePath: TRealPath; const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);

		{Called after uploading a file - copies description from local to remote}
		procedure OnFileUploaded(const RemotePath: TRealPath; const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);
	end;

	TDescriptionSyncManager = class(TBaseRemoteMetadataSyncManager, IDescriptionSyncManager)
	private
		FTCHandler: ITCHandler;

		function GetLocalDescriptionPath(const LocalFilePath: WideString): WideString;
		function CreateDescription(const FilePath: WideString): TDescription;
	protected
		function GetTempExt: WideString; override;
		function CreateStore(const FilePath: WideString): IRemoteMetadataStore; override;
	public
		constructor Create(const DescriptionFileName: WideString; FileSystem: IFileSystem; TCHandler: ITCHandler);

		procedure OnFileDeleted(const RemotePath: TRealPath; Cloud: ICloudDescriptionOps);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: ICloudDescriptionOps);
		procedure OnFileDownloaded(const RemotePath: TRealPath; const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);
		procedure OnFileUploaded(const RemotePath: TRealPath; const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);
	end;

implementation

uses
	PathHelper,
	DescriptionStoreAdapter;

constructor TDescriptionSyncManager.Create(const DescriptionFileName: WideString;
	FileSystem: IFileSystem; TCHandler: ITCHandler);
begin
	inherited Create(DescriptionFileName, FileSystem);
	FTCHandler := TCHandler;
end;

function TDescriptionSyncManager.GetTempExt: WideString;
begin
	Result := DESCRIPTION_TEMP_EXT;
end;

function TDescriptionSyncManager.CreateStore(const FilePath: WideString): IRemoteMetadataStore;
begin
	Result := TDescriptionStoreAdapter.Create(CreateDescription(FilePath));
end;

function TDescriptionSyncManager.GetLocalDescriptionPath(const LocalFilePath: WideString): WideString;
begin
	Result := IncludeTrailingPathDelimiter(ExtractFileDir(LocalFilePath)) + FMetadataFileName;
end;

function TDescriptionSyncManager.CreateDescription(const FilePath: WideString): TDescription;
begin
	Result := TDescription.Create(FilePath, FFileSystem, FTCHandler.GetTCCommentPreferredFormat);
end;

procedure TDescriptionSyncManager.OnFileDeleted(const RemotePath: TRealPath; Cloud: ICloudDescriptionOps);
begin
	HandleFileDeleted(RemotePath, Cloud);
end;

procedure TDescriptionSyncManager.OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: ICloudDescriptionOps);
begin
	HandleFileRenamed(OldPath, NewPath, Cloud);
end;

procedure TDescriptionSyncManager.OnFileDownloaded(const RemotePath: TRealPath; const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);
var
	RemoteDescriptions, LocalDescriptions: TDescription;
	RemoteIonPath, LocalTempPath: WideString;
begin
	RemoteIonPath := GetRemoteMetadataPath(ExtractFileDir(RemotePath.Path));

	if not DownloadRemoteMetadata(RemoteIonPath, Cloud, LocalTempPath) then
		exit; {No remote description file exists}

	try
		RemoteDescriptions := CreateDescription(LocalTempPath);
		try
			RemoteDescriptions.Read;
			LocalDescriptions := CreateDescription(GetLocalDescriptionPath(LocalFilePath));
			try
				LocalDescriptions.Read;
				LocalDescriptions.CopyFrom(RemoteDescriptions, ExtractFileName(LocalFilePath));
				LocalDescriptions.Write();
			finally
				LocalDescriptions.Free;
			end;
		finally
			RemoteDescriptions.Free;
		end;
	finally
		FFileSystem.DeleteFile(LocalTempPath);
	end;
end;

procedure TDescriptionSyncManager.OnFileUploaded(const RemotePath: TRealPath; const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);
var
	RemoteDescriptions, LocalDescriptions: TDescription;
	RemoteIonPath, LocalIonPath, LocalTempPath: WideString;
	RemoteIonExists: Boolean;
begin
	LocalIonPath := GetLocalDescriptionPath(LocalFilePath);

	if not FFileSystem.FileExists(LocalIonPath) then
		exit; {No local description file exists}

	LocalDescriptions := CreateDescription(LocalIonPath);
	try
		LocalDescriptions.Read;

		RemoteIonPath := GetRemoteMetadataPath(ExtractFileDir(RemotePath.Path));
		LocalTempPath := FFileSystem.GetTmpFileName(GetTempExt);
		try
			RemoteIonExists := Cloud.GetDescriptionFile(RemoteIonPath, LocalTempPath);

			RemoteDescriptions := CreateDescription(LocalTempPath);
			try
				if RemoteIonExists then
					RemoteDescriptions.Read;

				RemoteDescriptions.CopyFrom(LocalDescriptions, ExtractFileName(RemotePath.Path));
				RemoteDescriptions.Write();

				if RemoteIonExists then
					Cloud.DeleteFile(RemoteIonPath);
				Cloud.PutDescriptionFile(RemoteIonPath, RemoteDescriptions.ionFilename);
			finally
				RemoteDescriptions.Free;
			end;
		finally
			FFileSystem.DeleteFile(LocalTempPath);
		end;
	finally
		LocalDescriptions.Free;
	end;
end;

end.
