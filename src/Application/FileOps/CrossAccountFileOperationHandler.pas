unit CrossAccountFileOperationHandler;

{Handles cross-account file operations: copy/move files between different cloud accounts.
	Supports transfer via hash (file identity) or via public link (weblink cloning).}

interface

uses
	CloudMailRu,
	CloudCallbackTypes,
	CloudErrorMapper,
	RealPath,
	RetryHandler,
	TCLogger;

type
	ICrossAccountFileOperationHandler = interface
		['{2C8C695B-1FCA-418D-9650-B1764F40EB30}']

		{Executes cross-account file operation (copy/move between different accounts).
			@param OldCloud Source cloud connection
			@param NewCloud Target cloud connection
			@param OldRealPath Source path
			@param NewRealPath Target path
			@param Move True to move (delete source after copy), False to copy only
			@param OverWrite True to overwrite existing target
			@param Mode Copy mode (CopyBetweenAccountsModeDisabled/ViaHash/ViaPublicLink)
			@param IsSourcePublicAccount True if source is a public account
			@param AbortCheck Callback for checking user abort
			@return FS_FILE_OK on success, error code otherwise}
		function Execute(OldCloud, NewCloud: TCloudMailRu; const OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean; Mode: Integer; IsSourcePublicAccount: Boolean; AbortCheck: TAbortCheckFunc): Integer;
	end;

	TCrossAccountFileOperationHandler = class(TInterfacedObject, ICrossAccountFileOperationHandler)
	private
		FRetryHandler: IRetryHandler;
		FLogger: ILogger;

		{Transfers file by adding it via hash identity to target account}
		function ExecuteViaHash(OldCloud, NewCloud: TCloudMailRu; const OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean; AbortCheck: TAbortCheckFunc): Integer;

		{Transfers file by creating/cloning public weblink}
		function ExecuteViaPublicLink(OldCloud, NewCloud: TCloudMailRu; const OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean; AbortCheck: TAbortCheckFunc): Integer;

		{Clones weblink from source to target, unpublishes if needed}
		function CloneWeblink(NewCloud, OldCloud: TCloudMailRu; const CloudPath, Weblink, Home: WideString; NeedUnpublish: Boolean): Integer;
	public
		constructor Create(RetryHandler: IRetryHandler; Logger: ILogger);

		function Execute(OldCloud, NewCloud: TCloudMailRu; const OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean; Mode: Integer; IsSourcePublicAccount: Boolean; AbortCheck: TAbortCheckFunc): Integer;
	end;

implementation

uses
	SysUtils,
	WFXTypes,
	CloudConstants,
	CloudDirItem,
	CloudFileIdentity,
	SettingsConstants,
	LanguageStrings;

constructor TCrossAccountFileOperationHandler.Create(RetryHandler: IRetryHandler; Logger: ILogger);
begin
	inherited Create;
	FRetryHandler := RetryHandler;
	FLogger := Logger;
end;

function TCrossAccountFileOperationHandler.Execute(OldCloud, NewCloud: TCloudMailRu; const OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean; Mode: Integer; IsSourcePublicAccount: Boolean; AbortCheck: TAbortCheckFunc): Integer;
begin
	{Public accounts cannot be source for cross-account operations}
	if IsSourcePublicAccount then
	begin
		FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, ERR_DIRECT_OPERATIONS_NOT_SUPPORTED);
		Exit(FS_FILE_USERABORT);
	end;

	case Mode of
		CopyBetweenAccountsModeDisabled:
			begin
				FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, ERR_DIRECT_OPERATIONS_DISABLED);
				Exit(FS_FILE_USERABORT);
			end;
		CopyBetweenAccountsModeViaHash:
			Result := ExecuteViaHash(OldCloud, NewCloud, OldRealPath, NewRealPath, Move, OverWrite, AbortCheck);
		CopyBetweenAccountsModeViaPublicLink:
			Result := ExecuteViaPublicLink(OldCloud, NewCloud, OldRealPath, NewRealPath, Move, OverWrite, AbortCheck);
		else
			Result := FS_FILE_WRITEERROR;
	end;
end;

function TCrossAccountFileOperationHandler.ExecuteViaHash(OldCloud, NewCloud: TCloudMailRu; const OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean; AbortCheck: TAbortCheckFunc): Integer;
var
	CurrentItem: TCloudDirItem;
	FileIdentity: TCloudFileIdentity;
	TargetPath: WideString;
begin
	Result := FS_FILE_NOTSUPPORTED;

	if OverWrite and not NewCloud.FileOperations.Delete(NewRealPath.Path) then
		Exit;

	if not OldCloud.ListingService.StatusFile(OldRealPath.Path, CurrentItem) then
		Exit;

	{Convert TCloudDirItem to TCloudFileIdentity for AddFileByIdentity}
	FileIdentity.Hash := CurrentItem.hash;
	FileIdentity.Size := CurrentItem.size;

	TargetPath := IncludeTrailingPathDelimiter(ExtractFileDir(NewRealPath.Path)) + ExtractFileName(NewRealPath.Path);
	Result := NewCloud.Uploader.AddFileByIdentity(FileIdentity, TargetPath);

	if not(Result in [FS_FILE_OK, FS_FILE_EXISTS]) then
		Result := FRetryHandler.HandleOperationError(Result, rotRenMov, ERR_CLONE_FILE_ASK, ERR_OPERATION, CLONE_FILE_RETRY, TCloudErrorMapper.ErrorCodeText(Result),
			function: Integer
			begin
				Result := NewCloud.Uploader.AddFileByIdentity(FileIdentity, TargetPath);
			end,
			function: Boolean
			begin
				Result := AbortCheck();
			end);

	{Delete source if move operation succeeded}
	if (Result = CLOUD_OPERATION_OK) and Move and not OldCloud.FileOperations.Delete(OldRealPath.Path) then
		FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_DELETE, [CurrentItem.Home]);
end;

function TCrossAccountFileOperationHandler.ExecuteViaPublicLink(OldCloud, NewCloud: TCloudMailRu; const OldRealPath, NewRealPath: TRealPath; Move, OverWrite: Boolean; AbortCheck: TAbortCheckFunc): Integer;
var
	NeedUnpublish: Boolean;
	CurrentItem: TCloudDirItem;
	Weblink: WideString;
begin
	Result := FS_FILE_NOTSUPPORTED;
	NeedUnpublish := False;

	if OverWrite and not NewCloud.FileOperations.Delete(NewRealPath.Path) then
		Exit;

	if not OldCloud.ListingService.StatusFile(OldRealPath.Path, CurrentItem) then
		Exit;

	{Create temporary weblink if file is not already published}
	if not CurrentItem.isPublished then
	begin
		NeedUnpublish := True;
		Weblink := CurrentItem.Weblink;
		if not OldCloud.PublishFile(CurrentItem.Home, Weblink) then
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_GET_TEMP_PUBLIC_LINK, [CurrentItem.Home]);
			Exit(FS_FILE_READERROR);
		end;
		CurrentItem.Weblink := Weblink;
	end;

	Result := CloneWeblink(NewCloud, OldCloud, NewRealPath.Path, CurrentItem.Weblink, CurrentItem.Home, NeedUnpublish);

	if not(Result in [FS_FILE_OK, FS_FILE_EXISTS]) then
		Result := FRetryHandler.HandleOperationError(Result, rotRenMov, ERR_PUBLISH_FILE_ASK, ERR_PUBLISH_FILE, PUBLISH_FILE_RETRY, TCloudErrorMapper.ErrorCodeText(Result),
			function: Integer
			begin
				Result := CloneWeblink(NewCloud, OldCloud, NewRealPath.Path, CurrentItem.Weblink, CurrentItem.Home, NeedUnpublish);
			end,
			function: Boolean
			begin
				Result := AbortCheck();
			end);

	{Delete source if move operation succeeded}
	if (Result = CLOUD_OPERATION_OK) and Move and not OldCloud.FileOperations.Delete(OldRealPath.Path) then
		FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_DELETE, [CurrentItem.Home]);
end;

function TCrossAccountFileOperationHandler.CloneWeblink(NewCloud, OldCloud: TCloudMailRu; const CloudPath, Weblink, Home: WideString; NeedUnpublish: Boolean): Integer;
var
	TempWeblink: WideString;
begin
	Result := NewCloud.ShareService.CloneWeblink(ExtractFileDir(CloudPath), Weblink, CLOUD_CONFLICT_STRICT);

	{Remove temporary public link if needed}
	if NeedUnpublish and (FS_FILE_USERABORT <> Result) then
	begin
		TempWeblink := Weblink;
		if not OldCloud.PublishFile(Home, TempWeblink, CLOUD_UNPUBLISH) then
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, PREFIX_ERR_REMOVE_TEMP_PUBLIC_LINK + Home);
	end;
end;

end.
