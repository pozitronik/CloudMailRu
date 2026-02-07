unit CrossServerFileOperationHandler;

{Handles cross-server file operations: transfers files between cloud accounts
	on different servers via memory stream when server-side operations (hash dedup,
	weblink cloning) are unavailable across server boundaries.}

interface

uses
	System.Classes,
	CloudMailRu,
	CloudCallbackTypes,
	CloudErrorMapper,
	RealPath,
	RetryHandler,
	Logger;

type
	ICrossServerFileOperationHandler = interface
		['{A3F7D2E1-8B4C-4A6F-9E1D-3C5B7F0A2D8E}']

		{Executes cross-server file operation (copy/move between accounts on different servers).
			Downloads from source into memory, uploads from memory to destination.
			Attempts hash dedup first for instant transfer when content exists on destination.
			@param OldCloud Source cloud connection
			@param NewCloud Target cloud connection
			@param OldRealPath Source path
			@param NewRealPath Target path
			@param Move True to move (delete source after copy), False to copy only
			@param OverWrite True to overwrite existing target
			@param AbortCheck Callback for checking user abort
			@return FS_FILE_OK on success, error code otherwise}
		function Execute(OldCloud, NewCloud: TCloudMailRu;
			const OldRealPath, NewRealPath: TRealPath;
			Move, OverWrite: Boolean;
			AbortCheck: TAbortCheckFunc): Integer;
	end;

	TCrossServerFileOperationHandler = class(TInterfacedObject, ICrossServerFileOperationHandler)
	private
		FRetryHandler: IRetryHandler;
		FLogger: ILogger;
	public
		constructor Create(RetryHandler: IRetryHandler; Logger: ILogger);

		function Execute(OldCloud, NewCloud: TCloudMailRu;
			const OldRealPath, NewRealPath: TRealPath;
			Move, OverWrite: Boolean;
			AbortCheck: TAbortCheckFunc): Integer;
	end;

implementation

uses
	SysUtils,
	WFXTypes,
	CloudConstants,
	CloudDirItem,
	CloudFileIdentity,
	LanguageStrings,
	PathHelper;

constructor TCrossServerFileOperationHandler.Create(RetryHandler: IRetryHandler; Logger: ILogger);
begin
	inherited Create;
	FRetryHandler := RetryHandler;
	FLogger := Logger;
end;

function TCrossServerFileOperationHandler.Execute(OldCloud, NewCloud: TCloudMailRu;
	const OldRealPath, NewRealPath: TRealPath;
	Move, OverWrite: Boolean;
	AbortCheck: TAbortCheckFunc): Integer;
var
	CurrentItem: TCloudDirItem;
	FileIdentity: TCloudFileIdentity;
	TargetPath: WideString;
	MemoryStream: TMemoryStream;
begin
	Result := FS_FILE_NOTSUPPORTED;

	if OverWrite and not NewCloud.FileOperations.Delete(NewRealPath.Path) then
		Exit;

	{Get source file metadata}
	if not OldCloud.ListingService.StatusFile(OldRealPath.Path, CurrentItem) then
		Exit;

	FileIdentity.Hash := CurrentItem.hash;
	FileIdentity.Size := CurrentItem.size;
	TargetPath := IncludeTrailingPathDelimiter(ExtractFileDir(NewRealPath.Path)) + ExtractFileName(NewRealPath.Path);

	{Try hash dedup first -- free instant transfer if content exists on destination}
	Result := NewCloud.Uploader.AddFileByIdentity(FileIdentity, TargetPath, CLOUD_CONFLICT_STRICT, False);
	if Result in [FS_FILE_OK, FS_FILE_EXISTS] then
	begin
		{Dedup succeeded, handle move if needed}
		if (Result = FS_FILE_OK) and Move and not OldCloud.FileOperations.Delete(OldRealPath.Path) then
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_DELETE, [CurrentItem.Home]);
		Exit;
	end;

	{Hash dedup failed -- download to memory, upload from memory}
	MemoryStream := TMemoryStream.Create;
	try
		Result := OldCloud.Downloader.DownloadToStream(OldRealPath.Path, MemoryStream, OldRealPath.ToPath, NewRealPath.ToPath);
		if not (Result in [FS_FILE_OK]) then
			Exit;

		MemoryStream.Position := 0;
		Result := NewCloud.Uploader.UploadStream(ExtractFileName(NewRealPath.Path), TargetPath, MemoryStream, CLOUD_CONFLICT_STRICT, FileIdentity.Hash, OldRealPath.ToPath, NewRealPath.ToPath);
	finally
		MemoryStream.Free;
	end;

	{Retry on upload failure}
	if not (Result in [FS_FILE_OK, FS_FILE_EXISTS]) then
		Result := FRetryHandler.HandleOperationError(Result, rotRenMov, ERR_CLONE_FILE_ASK, ERR_OPERATION, CLONE_FILE_RETRY, TCloudErrorMapper.ErrorCodeText(Result),
			function: Integer
			var
				RetryStream: TMemoryStream;
			begin
				RetryStream := TMemoryStream.Create;
				try
					Result := OldCloud.Downloader.DownloadToStream(OldRealPath.Path, RetryStream, OldRealPath.ToPath, NewRealPath.ToPath);
					if Result in [FS_FILE_OK] then
					begin
						RetryStream.Position := 0;
						Result := NewCloud.Uploader.UploadStream(ExtractFileName(NewRealPath.Path), TargetPath, RetryStream, CLOUD_CONFLICT_STRICT, FileIdentity.Hash, OldRealPath.ToPath, NewRealPath.ToPath);
					end;
				finally
					RetryStream.Free;
				end;
			end,
			function: Boolean
			begin
				Result := AbortCheck();
			end);

	{Delete source if move operation succeeded}
	if (Result = CLOUD_OPERATION_OK) and Move and not OldCloud.FileOperations.Delete(OldRealPath.Path) then
		FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_DELETE, [CurrentItem.Home]);
end;

end.
