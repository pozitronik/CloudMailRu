unit BaseRemoteMetadataSyncManager;

{Base class for remote metadata sync managers.
	Implements the shared download-modify-reupload lifecycle for OnFileDeleted
	and OnFileRenamed. Subclasses provide a metadata store factory via CreateStore
	and implement type-specific OnFileDownloaded/OnFileUploaded handlers.}

interface

uses
	SysUtils,
	RealPath,
	RemoteMetadataStore,
	FileSystem,
	CloudDescriptionOperationsAdapter;

type
	TBaseRemoteMetadataSyncManager = class(TInterfacedObject)
	protected
		FMetadataFileName: WideString;
		FFileSystem: IFileSystem;

		{Temp file extension for downloaded metadata (e.g. 'ion', 'tsm')}
		function GetTempExt: WideString; virtual; abstract;

		{Factory: create an IRemoteMetadataStore backed by the given file.
			The returned interface owns the underlying object.}
		function CreateStore(const FilePath: WideString): IRemoteMetadataStore; virtual; abstract;

		function GetRemoteMetadataPath(const DirPath: WideString): WideString;
		function DownloadRemoteMetadata(const RemoteMetaPath: WideString;
			Cloud: ICloudDescriptionOps; out LocalTempPath: WideString): Boolean;

		{Shared lifecycle: delete entry from remote metadata file}
		procedure HandleFileDeleted(const RemotePath: TRealPath; Cloud: ICloudDescriptionOps);

		{Shared lifecycle: rename or move entry between remote metadata files}
		procedure HandleFileRenamed(const OldPath, NewPath: TRealPath; Cloud: ICloudDescriptionOps);
	public
		constructor Create(const MetadataFileName: WideString; FileSystem: IFileSystem);
	end;

implementation

uses
	PathHelper;

constructor TBaseRemoteMetadataSyncManager.Create(const MetadataFileName: WideString;
	FileSystem: IFileSystem);
begin
	inherited Create;
	FMetadataFileName := MetadataFileName;
	FFileSystem := FileSystem;
end;

function TBaseRemoteMetadataSyncManager.GetRemoteMetadataPath(const DirPath: WideString): WideString;
begin
	Result := IncludeTrailingBackslash(DirPath) + FMetadataFileName;
end;

function TBaseRemoteMetadataSyncManager.DownloadRemoteMetadata(const RemoteMetaPath: WideString;
	Cloud: ICloudDescriptionOps; out LocalTempPath: WideString): Boolean;
begin
	LocalTempPath := FFileSystem.GetTmpFileName(GetTempExt);
	Result := Cloud.GetDescriptionFile(RemoteMetaPath, LocalTempPath);
	if not Result then
		FFileSystem.DeleteFile(LocalTempPath);
end;

procedure TBaseRemoteMetadataSyncManager.HandleFileDeleted(const RemotePath: TRealPath;
	Cloud: ICloudDescriptionOps);
var
	Store: IRemoteMetadataStore;
	RemoteMetaPath, LocalTempPath: WideString;
begin
	RemoteMetaPath := GetRemoteMetadataPath(ExtractFileDir(RemotePath.Path));

	if not DownloadRemoteMetadata(RemoteMetaPath, Cloud, LocalTempPath) then
		Exit; {No metadata file exists}

	try
		Store := CreateStore(LocalTempPath);
		Store.Read;
		Store.DeleteEntry(ExtractFileName(RemotePath.Path));
		Store.Write;
		Cloud.DeleteFile(RemoteMetaPath);
		Cloud.PutDescriptionFile(RemoteMetaPath, Store.GetFileName);
	finally
		FFileSystem.DeleteFile(LocalTempPath);
	end;
end;

procedure TBaseRemoteMetadataSyncManager.HandleFileRenamed(const OldPath, NewPath: TRealPath;
	Cloud: ICloudDescriptionOps);
var
	OldStore, NewStore: IRemoteMetadataStore;
	OldRemoteMetaPath, NewRemoteMetaPath, OldLocalTempPath, NewLocalTempPath: WideString;
	NewRemoteMetaExists: Boolean;
	OldItem, NewItem, EntryValue: WideString;
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
			OldStore := CreateStore(OldLocalTempPath);
			OldStore.Read;
			if OldStore.RenameEntry(OldItem, NewItem) then
			begin
				OldStore.Write;
				Cloud.DeleteFile(OldRemoteMetaPath);
				Cloud.PutDescriptionFile(OldRemoteMetaPath, OldStore.GetFileName);
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
			OldStore := CreateStore(OldLocalTempPath);
			OldStore.Read;

			EntryValue := OldStore.GetEntryAsString(OldItem);
			if EntryValue = '' then
				Exit; {No entry to transfer}

			NewLocalTempPath := FFileSystem.GetTmpFileName(GetTempExt);
			try
				NewRemoteMetaExists := Cloud.GetDescriptionFile(NewRemoteMetaPath, NewLocalTempPath);
				NewStore := CreateStore(NewLocalTempPath);
				if NewRemoteMetaExists then
					NewStore.Read;

				NewStore.SetEntryFromString(NewItem, EntryValue);
				OldStore.DeleteEntry(OldItem);
				OldStore.Write;
				NewStore.Write;

				Cloud.DeleteFile(OldRemoteMetaPath);
				Cloud.PutDescriptionFile(OldRemoteMetaPath, OldStore.GetFileName);

				if NewRemoteMetaExists then
					Cloud.DeleteFile(NewRemoteMetaPath);
				Cloud.PutDescriptionFile(NewRemoteMetaPath, NewStore.GetFileName);
			finally
				FFileSystem.DeleteFile(NewLocalTempPath);
			end;
		finally
			FFileSystem.DeleteFile(OldLocalTempPath);
		end;
	end;
end;

end.
