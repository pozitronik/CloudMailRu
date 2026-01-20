unit DescriptionSyncManager;

{Manages file description synchronization between local and remote storage.
 Handles description file (.ion) operations when files are created, deleted,
 renamed, or transferred.}

interface

uses
	SysUtils,
	RealPath,
	Description,
	WindowsFileSystem,
	CloudDescriptionOpsAdapter;

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

	{Manages synchronization of file descriptions (.ion files) between local and remote storage.
	 Extracts description file manipulation logic from WFX to centralize and make testable.}
	TDescriptionSyncManager = class(TInterfacedObject, IDescriptionSyncManager)
	private
		FDescriptionFileName: WideString;
		FFileSystem: IFileSystem;

		function GetRemoteDescriptionPath(const DirPath: WideString): WideString;
		function GetLocalDescriptionPath(const LocalFilePath: WideString): WideString;
		function CreateDescription(const FilePath: WideString): TDescription;
		function DownloadRemoteDescription(const RemoteIonPath: WideString; Cloud: ICloudDescriptionOps; out LocalTempPath: WideString): Boolean;
	public
		constructor Create(const DescriptionFileName: WideString; FileSystem: IFileSystem);

		procedure OnFileDeleted(const RemotePath: TRealPath; Cloud: ICloudDescriptionOps);
		procedure OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: ICloudDescriptionOps);
		procedure OnFileDownloaded(const RemotePath: TRealPath; const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);
		procedure OnFileUploaded(const RemotePath: TRealPath; const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);
	end;

implementation

uses
	WindowsHelper,
	PathHelper,
	TCHelper,
	CMRConstants;

constructor TDescriptionSyncManager.Create(const DescriptionFileName: WideString; FileSystem: IFileSystem);
begin
	inherited Create;
	FDescriptionFileName := DescriptionFileName;
	FFileSystem := FileSystem;
end;

function TDescriptionSyncManager.GetRemoteDescriptionPath(const DirPath: WideString): WideString;
begin
	Result := IncludeTrailingBackslash(DirPath) + FDescriptionFileName;
end;

function TDescriptionSyncManager.GetLocalDescriptionPath(const LocalFilePath: WideString): WideString;
begin
	Result := IncludeTrailingPathDelimiter(ExtractFileDir(LocalFilePath)) + FDescriptionFileName;
end;

function TDescriptionSyncManager.CreateDescription(const FilePath: WideString): TDescription;
begin
	Result := TDescription.Create(FilePath, FFileSystem, GetTCCommentPreferredFormat);
end;

function TDescriptionSyncManager.DownloadRemoteDescription(const RemoteIonPath: WideString; Cloud: ICloudDescriptionOps; out LocalTempPath: WideString): Boolean;
begin
	LocalTempPath := GetTmpFileName(DESCRIPTION_TEMP_EXT);
	Result := Cloud.GetDescriptionFile(RemoteIonPath, LocalTempPath);
end;

procedure TDescriptionSyncManager.OnFileDeleted(const RemotePath: TRealPath; Cloud: ICloudDescriptionOps);
var
	RemoteDescriptions: TDescription;
	RemoteIonPath, LocalTempPath: WideString;
begin
	RemoteIonPath := GetRemoteDescriptionPath(ExtractFileDir(RemotePath.Path));

	if not DownloadRemoteDescription(RemoteIonPath, Cloud, LocalTempPath) then
		exit; { No description file exists }

	RemoteDescriptions := CreateDescription(LocalTempPath);
	try
		RemoteDescriptions.Read;
		RemoteDescriptions.DeleteValue(ExtractFileName(RemotePath.Path));
		RemoteDescriptions.Write();
		Cloud.DeleteFile(RemoteIonPath);
		Cloud.PutDescriptionFile(RemoteIonPath, RemoteDescriptions.ionFilename);
	finally
		RemoteDescriptions.Free;
	end;
end;

procedure TDescriptionSyncManager.OnFileRenamed(const OldPath, NewPath: TRealPath; Cloud: ICloudDescriptionOps);
var
	OldDescriptions, NewDescriptions: TDescription;
	OldRemoteIonPath, NewRemoteIonPath, OldLocalTempPath, NewLocalTempPath: WideString;
	NewRemoteIonExists: Boolean;
	OldItem, NewItem: WideString;
begin
	OldItem := ExtractFileName(OldPath.Path);
	NewItem := ExtractFileName(NewPath.Path);
	OldRemoteIonPath := GetRemoteDescriptionPath(ExtractFileDir(OldPath.Path));
	NewRemoteIonPath := GetRemoteDescriptionPath(ExtractFileDir(NewPath.Path));

	if ExtractFileDir(OldPath.Path) = ExtractFileDir(NewPath.Path) then
	begin
		{ Rename within same directory - modify single description file }
		if not DownloadRemoteDescription(OldRemoteIonPath, Cloud, OldLocalTempPath) then
			exit; { No description file exists }

		OldDescriptions := CreateDescription(OldLocalTempPath);
		try
			OldDescriptions.Read;
			if OldDescriptions.RenameItem(OldItem, NewItem) then
			begin
				OldDescriptions.Write();
				Cloud.DeleteFile(OldRemoteIonPath);
				Cloud.PutDescriptionFile(OldRemoteIonPath, OldDescriptions.ionFilename);
			end;
		finally
			OldDescriptions.Free;
		end;
	end
	else
	begin
		{ Move between directories - transfer entry between two description files }
		if not DownloadRemoteDescription(OldRemoteIonPath, Cloud, OldLocalTempPath) then
			exit; { No source description file exists }

		OldDescriptions := CreateDescription(OldLocalTempPath);
		try
			OldDescriptions.Read;

			NewLocalTempPath := GetTmpFileName(DESCRIPTION_TEMP_EXT);
			NewRemoteIonExists := Cloud.GetDescriptionFile(NewRemoteIonPath, NewLocalTempPath);
			NewDescriptions := CreateDescription(NewLocalTempPath);
			try
				if NewRemoteIonExists then
					NewDescriptions.Read;

				NewDescriptions.SetValue(ExtractFileName(NewPath.Path), OldDescriptions.GetValue(ExtractFileName(OldPath.Path)));
				OldDescriptions.DeleteValue(ExtractFileName(OldPath.Path));
				OldDescriptions.Write();
				NewDescriptions.Write();

				Cloud.DeleteFile(OldRemoteIonPath);
				Cloud.PutDescriptionFile(OldRemoteIonPath, OldDescriptions.ionFilename);

				if NewRemoteIonExists then
					Cloud.DeleteFile(NewRemoteIonPath);
				Cloud.PutDescriptionFile(NewRemoteIonPath, NewDescriptions.ionFilename);
			finally
				NewDescriptions.Free;
			end;
		finally
			OldDescriptions.Free;
		end;
	end;
end;

procedure TDescriptionSyncManager.OnFileDownloaded(const RemotePath: TRealPath; const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);
var
	RemoteDescriptions, LocalDescriptions: TDescription;
	RemoteIonPath, LocalTempPath: WideString;
begin
	RemoteIonPath := GetRemoteDescriptionPath(ExtractFileDir(RemotePath.Path));

	if not DownloadRemoteDescription(RemoteIonPath, Cloud, LocalTempPath) then
		exit; { No remote description file exists }

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
end;

procedure TDescriptionSyncManager.OnFileUploaded(const RemotePath: TRealPath; const LocalFilePath: WideString; Cloud: ICloudDescriptionOps);
var
	RemoteDescriptions, LocalDescriptions: TDescription;
	RemoteIonPath, LocalIonPath, LocalTempPath: WideString;
	RemoteIonExists: Boolean;
begin
	LocalIonPath := GetLocalDescriptionPath(LocalFilePath);

	if not FileExists(GetUNCFilePath(LocalIonPath)) then
		exit; { No local description file exists }

	LocalDescriptions := CreateDescription(LocalIonPath);
	try
		LocalDescriptions.Read;

		RemoteIonPath := GetRemoteDescriptionPath(ExtractFileDir(RemotePath.Path));
		LocalTempPath := GetTmpFileName(DESCRIPTION_TEMP_EXT);
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
		LocalDescriptions.Free;
	end;
end;

end.
