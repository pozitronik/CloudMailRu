unit RemotePropertyPresenter;

{Presenter for RemoteProperty dialog - handles all business logic for file/folder properties.
	Follows MVP pattern: View (TPropertyForm) implements IRemotePropertyView,
	Presenter orchestrates operations, Model consists of extracted cloud services.}

interface

uses
	CloudDirItem,
	CloudDirItemList,
	CloudInviteList,
	CloudFileIdentity,
	CloudConstants,
	CloudMailRu,
	CloudMailRuFactory,
	CloudFileDownloader,
	CloudFileUploader,
	CloudFileOperations,
	CloudListingService,
	CloudShareService,
	CloudAccessMapper,
	Description,
	HashInfo,
	FileSystem,
	TCHandler,
	LanguageStrings,
	WFXTypes,
	System.Classes,
	System.SysUtils;

type
	{Tabs available in RemoteProperty dialog}
	TRemotePropertyTab = (rptFolderAccess, rptDownloadLinks, rptDescription, rptHashesList);

	{Callback for logging status messages}
	TLogMessageProc = reference to procedure(Message: WideString);

	{View interface for RemoteProperty dialog}
	IRemotePropertyView = interface
		['{08CEFDE0-5411-455E-8537-CC53EBB279DF}']
		{Item display}
		procedure SetCaption(Caption: WideString);
		procedure SetWebLink(Link: WideString);
		procedure SetWebLinkEnabled(Enabled: Boolean);
		procedure SetPublishChecked(Checked: Boolean);
		procedure SetPublishEnabled(Enabled: Boolean);

		{Tab visibility}
		procedure ShowTab(Tab: TRemotePropertyTab);
		procedure HideTab(Tab: TRemotePropertyTab);
		procedure SetExtPropertiesVisible(Visible: Boolean);

		{Invites list}
		procedure ClearInvitesList;
		procedure AddInvite(Email, Access: WideString);
		function GetSelectedInviteEmail: WideString;
		function GetSelectedInviteAccess: WideString;

		{Download links}
		procedure ClearDownloadLinks;
		procedure AddDownloadLink(Link: WideString);
		procedure SetDownloadLinksLogMessage(Message: WideString);
		procedure SetDownloadLinksCancelEnabled(Enabled: Boolean);
		procedure SetDownloadLinksRefreshEnabled(Enabled: Boolean);

		{Hashes}
		procedure ClearHashes;
		procedure AddHash(HashCommand: WideString);
		procedure SetHashesLogMessage(Message: WideString);
		procedure SetHashesCancelEnabled(Enabled: Boolean);
		procedure SetHashesRefreshEnabled(Enabled: Boolean);
		procedure SetHashesMemoReadOnly(ReadOnly: Boolean);
		procedure SetApplyHashesEnabled(Enabled: Boolean);
		procedure SetLoadHashesEnabled(Enabled: Boolean);
		function GetHashCommands: TStrings;

		{Description}
		procedure SetDescription(Text: WideString);
		function GetDescription: WideString;
		procedure SetDescriptionReadOnly(ReadOnly: Boolean);
		procedure SetDescriptionSaveEnabled(Enabled: Boolean);
		procedure SetDescriptionTabCaption(Caption: WideString);

		{User interaction}
		procedure ShowError(Title, Message: WideString);
		procedure ProcessMessages;
		function IsDownloadLinksCancelled: Boolean;
		function IsHashesCancelled: Boolean;
		procedure ResetDownloadLinksCancelled;
		procedure ResetHashesCancelled;

		{Invite input}
		function GetInviteEmailInput: WideString;
		function GetInviteAccessInput: Integer;
	end;

	{Configuration for RemoteProperty presenter}
	TRemotePropertyConfig = record
		DoUrlEncode: Boolean;
		AutoUpdateDownloadListing: Boolean;
		ShowDescription: Boolean;
		EditDescription: Boolean;
		PluginIonFileName: WideString;
	end;

	{Presenter for RemoteProperty dialog}
	TRemotePropertyPresenter = class
	private
		FView: IRemotePropertyView;
		FDownloader: ICloudFileDownloader;
		FUploader: ICloudFileUploader;
		FFileOps: ICloudFileOperations;
		FListingService: ICloudListingService;
		FShareService: ICloudShareService;
		FFileSystem: IFileSystem;
		FPublicCloudFactory: IPublicCloudFactory;
		FTCHandler: ITCHandler;
		FIsPublicAccount: Boolean;
		FProps: TCloudDirItem;
		FRemotePath: WideString;
		FConfig: TRemotePropertyConfig;
		FInvitesListing: TCloudInviteList;

		{Internal recursive listing helpers -- structurally similar by design.
			Merging into a generic method with callbacks was attempted but rejected:
			the call sites become harder to read than the duplication they eliminate.}
		function FillRecursiveDownloadListing(const Path: WideString; Downloader: ICloudFileDownloader; ListingService: ICloudListingService): Boolean;
		function FillRecursiveHashListing(const Path: WideString; ListingService: ICloudListingService; const BaseDir: WideString): Boolean;

		{Description file operations}
		function GetDescriptionFilePath: WideString;
		function DownloadDescriptionFile(var LocalPath: WideString): Boolean;
	public
		constructor Create(View: IRemotePropertyView; Downloader: ICloudFileDownloader; Uploader: ICloudFileUploader; FileOps: ICloudFileOperations; ListingService: ICloudListingService; ShareService: ICloudShareService; FileSystem: IFileSystem; PublicCloudFactory: IPublicCloudFactory; TCHandler: ITCHandler; IsPublicAccount: Boolean);

		{Initialize view state based on item properties}
		procedure Initialize(Props: TCloudDirItem; RemotePath: WideString; Config: TRemotePropertyConfig);

		{Publishing operations}
		procedure OnPublishChanged(Publish: Boolean);

		{Invite operations}
		procedure RefreshInvites;
		procedure OnInviteClick;
		procedure OnInviteDeleteClick;
		procedure OnInviteChangeAccessClick;

		{Download links operations}
		procedure RefreshDownloadLinks;

		{Hashes operations}
		procedure RefreshHashes;
		procedure ApplyHashCommands;
		function CanApplyHashes: Boolean;

		{Description operations}
		procedure LoadDescription;
		procedure SaveDescription;

		{Hash command generation - stateless, suitable for unit testing}
		class function GenerateHashCommand(Item: TCloudDirItem; BaseDir: WideString = ''; Path: WideString = ''): WideString; static;

		{Properties}
		property Props: TCloudDirItem read FProps;
		property InvitesListing: TCloudInviteList read FInvitesListing;
	end;

implementation

uses
	Winapi.Windows,
	Winapi.Messages,
	PathHelper;

const
	{Default filename for descriptions - matches TC convention}
	DESCRIPTION_DEFAULT_FILENAME = 'descript.ion';

	{TRemotePropertyPresenter}

constructor TRemotePropertyPresenter.Create(View: IRemotePropertyView; Downloader: ICloudFileDownloader; Uploader: ICloudFileUploader; FileOps: ICloudFileOperations; ListingService: ICloudListingService; ShareService: ICloudShareService; FileSystem: IFileSystem; PublicCloudFactory: IPublicCloudFactory; TCHandler: ITCHandler; IsPublicAccount: Boolean);
begin
	inherited Create;
	FView := View;
	FDownloader := Downloader;
	FUploader := Uploader;
	FFileOps := FileOps;
	FListingService := ListingService;
	FShareService := ShareService;
	FFileSystem := FileSystem;
	FPublicCloudFactory := PublicCloudFactory;
	FTCHandler := TCHandler;
	FIsPublicAccount := IsPublicAccount;
end;

procedure TRemotePropertyPresenter.Initialize(Props: TCloudDirItem; RemotePath: WideString; Config: TRemotePropertyConfig);
begin
	FProps := Props;
	FRemotePath := RemotePath;
	FConfig := Config;

	FView.SetCaption(Props.name);

	{Initialize tabs visibility}
	FView.SetExtPropertiesVisible(False);
	FView.HideTab(rptFolderAccess);
	FView.HideTab(rptDownloadLinks);
	FView.HideTab(rptDescription);
	FView.HideTab(rptHashesList);

	{Hash buttons depend on account type}
	FView.SetApplyHashesEnabled(CanApplyHashes);
	FView.SetLoadHashesEnabled(CanApplyHashes);

	if FIsPublicAccount then
	begin
		{Public account: always show download links, cannot change publish state}
		FView.SetPublishEnabled(False);
		FView.SetPublishChecked(True);
		FView.SetExtPropertiesVisible(True);
		FView.ShowTab(rptDownloadLinks);
		FView.SetHashesMemoReadOnly(True);

		if FConfig.AutoUpdateDownloadListing then
			RefreshDownloadLinks;
	end else begin
		{Private account: show weblink if published}
		FView.SetPublishChecked(Props.WebLink <> EmptyWideStr);
		FView.SetWebLinkEnabled(Props.WebLink <> EmptyWideStr);

		if Props.WebLink <> EmptyWideStr then
			FView.SetWebLink(PUBLIC_ACCESS_URL + Props.WebLink);

		{Show folder access tab for directories or shared items}
		if (Props.type_ = TYPE_DIR) or (Props.kind = KIND_SHARED) then
		begin
			FView.SetExtPropertiesVisible(True);
			FView.ShowTab(rptFolderAccess);
			RefreshInvites;
		end;
	end;

	{Description tab}
	if FConfig.ShowDescription then
	begin
		FView.SetExtPropertiesVisible(True);
		FView.ShowTab(rptDescription);
		LoadDescription;
		FView.SetDescriptionReadOnly(not FConfig.EditDescription);
		FView.SetDescriptionSaveEnabled(FConfig.EditDescription);

		if FConfig.PluginIonFileName <> DESCRIPTION_DEFAULT_FILENAME then
			FView.SetDescriptionTabCaption(Format(DESCRIPTION_FROM, [FConfig.PluginIonFileName]));
	end;

	{Hashes tab is always visible}
	FView.ShowTab(rptHashesList);
end;

procedure TRemotePropertyPresenter.OnPublishChanged(Publish: Boolean);
var
	PublicLink: WideString;
begin
	if FIsPublicAccount then
		Exit;

	FView.SetPublishEnabled(False);
	FView.SetWebLink(WAIT);

	if Publish then
	begin
		if FShareService.Publish(FProps.home, PublicLink) then
		begin
			FView.SetWebLink(PUBLIC_ACCESS_URL + PublicLink);
			FProps.WebLink := PublicLink;
			FView.SetWebLinkEnabled(True);
			FView.SetExtPropertiesVisible(True);
			FView.ShowTab(rptDownloadLinks);
		end else begin
			FView.ShowError(ERR_PUBLISH_FILE, Format(ERR_PUBLISH_MSG, [FProps.home]));
			FView.SetPublishChecked(False);
		end;
	end else begin
		if FShareService.Unpublish(FProps.home, FProps.WebLink) then
		begin
			FView.SetWebLink(EmptyWideStr);
			FProps.WebLink := EmptyWideStr;
			FView.SetWebLinkEnabled(False);
			FView.HideTab(rptDownloadLinks);
		end else begin
			FView.ShowError(ERR_UNPUBLISH_FILE, Format(ERR_PUBLISH_MSG, [FProps.home]));
			FView.SetPublishChecked(True);
		end;
	end;

	FView.SetPublishEnabled(True);

	if Publish and FConfig.AutoUpdateDownloadListing then
		RefreshDownloadLinks;
end;

procedure TRemotePropertyPresenter.RefreshInvites;
var
	i: Integer;
begin
	FView.ClearInvitesList;

	if FShareService.GetShareInfo(FProps.home, FInvitesListing) then
	begin
		for i := 0 to Length(FInvitesListing) - 1 do
			FView.AddInvite(FInvitesListing[i].Email, TCloudAccessMapper.AccessToString(FInvitesListing[i].Access));
	end
	else
		FView.ShowError(PREFIX_ERR_INVITES_LISTING, Format(ERR_LIST_INVITES_MSG, [FProps.home]));
end;

procedure TRemotePropertyPresenter.OnInviteClick;
var
	Email: WideString;
	Access: Integer;
begin
	Email := FView.GetInviteEmailInput;
	Access := FView.GetInviteAccessInput;

	if FShareService.Share(FProps.home, Email, Access) then
		RefreshInvites
	else
		FView.ShowError(PREFIX_ERR_INVITE, Format(ERR_INVITE_MSG, [Email, FProps.home]));
end;

procedure TRemotePropertyPresenter.OnInviteDeleteClick;
var
	Email: WideString;
begin
	Email := FView.GetSelectedInviteEmail;
	if Email = EmptyWideStr then
		Exit;

	if FShareService.Unshare(FProps.home, Email) then
		RefreshInvites
	else
		FView.ShowError(PREFIX_ERR_UNSHARE_FOLDER, Format(ERR_UNSHARE_FOLDER_MSG, [Email, FProps.home]));
end;

procedure TRemotePropertyPresenter.OnInviteChangeAccessClick;
var
	Email, CurrentAccessStr: WideString;
	NewAccess: Integer;
begin
	Email := FView.GetSelectedInviteEmail;
	if Email = EmptyWideStr then
		Exit;

	CurrentAccessStr := FView.GetSelectedInviteAccess;
	{Toggle access: get opposite access level}
	NewAccess := TCloudAccessMapper.StringToAccess(CurrentAccessStr, True);

	if FShareService.Share(FProps.home, Email, NewAccess) then
		RefreshInvites
	else
		FView.ShowError(PREFIX_ERR_SHARE_FOLDER, Format(ERR_SHARE_FOLDER_MSG, [Email, FProps.home]));
end;

procedure TRemotePropertyPresenter.RefreshDownloadLinks;
var
	TempPublicCloud: TCloudMailRu;
begin
	FView.ClearDownloadLinks;
	FView.SetDownloadLinksCancelEnabled(True);
	FView.SetDownloadLinksRefreshEnabled(False);
	FView.ResetDownloadLinksCancelled;

	try
		if FIsPublicAccount then
		begin
			{Public account: use main cloud services directly}
			if FProps.type_ = TYPE_DIR then
				FillRecursiveDownloadListing(IncludeTrailingPathDelimiter(FRemotePath), FDownloader, FListingService)
			else
				FView.AddDownloadLink(FDownloader.GetSharedFileUrl(FRemotePath));
		end else begin
			{Private account with published item: create temp public cloud}
			if FPublicCloudFactory.CreatePublicCloud(TempPublicCloud, PUBLIC_ACCESS_URL + FProps.WebLink) then
				try
					if FProps.type_ = TYPE_DIR then
						FillRecursiveDownloadListing(EmptyWideStr, TempPublicCloud.Downloader, TempPublicCloud.ListingService)
					else
						FView.AddDownloadLink(TempPublicCloud.Downloader.GetSharedFileUrl(FRemotePath));
				finally
					TempPublicCloud.Free;
				end;
		end;

		FView.SetDownloadLinksLogMessage(DONE);
	finally
		FView.SetDownloadLinksRefreshEnabled(True);
		FView.SetDownloadLinksCancelEnabled(False);
	end;
end;

function TRemotePropertyPresenter.FillRecursiveDownloadListing(const Path: WideString; Downloader: ICloudFileDownloader; ListingService: ICloudListingService): Boolean;
var
	CurrentDirListing: TCloudDirItemList;
	i: Integer;
begin
	Result := True;

	FView.SetDownloadLinksLogMessage(Format(PREFIX_SCAN, [IncludeTrailingPathDelimiter(Path)]));
	FView.ProcessMessages;

	if FView.IsDownloadLinksCancelled then
		Exit(False);

	if not ListingService.GetDirectory(Path, CurrentDirListing) then
		Exit;

	for i := 0 to Length(CurrentDirListing) - 1 do
	begin
		if CurrentDirListing[i].type_ = TYPE_DIR then
		begin
			Result := FillRecursiveDownloadListing(IncludeTrailingPathDelimiter(Path) + CurrentDirListing[i].name, Downloader, ListingService);
			if not Result then
				Break;
		end
		else
			FView.AddDownloadLink(Downloader.GetSharedFileUrl(IncludeTrailingPathDelimiter(Path) + CurrentDirListing[i].name));
	end;
end;

procedure TRemotePropertyPresenter.RefreshHashes;
begin
	FView.ClearHashes;
	FView.SetHashesCancelEnabled(True);
	FView.SetHashesRefreshEnabled(False);
	FView.SetApplyHashesEnabled(False);
	FView.SetLoadHashesEnabled(False);
	FView.ResetHashesCancelled;

	try
		if FProps.type_ = TYPE_DIR then
			FillRecursiveHashListing(IncludeTrailingPathDelimiter(FRemotePath), FListingService, IncludeTrailingPathDelimiter(FRemotePath))
		else
			FView.AddHash(GenerateHashCommand(FProps));

		FView.SetHashesLogMessage(DONE);
	finally
		FView.SetHashesRefreshEnabled(True);
		FView.SetHashesCancelEnabled(False);
		FView.SetApplyHashesEnabled(CanApplyHashes);
		FView.SetLoadHashesEnabled(CanApplyHashes);
	end;
end;

function TRemotePropertyPresenter.FillRecursiveHashListing(const Path: WideString; ListingService: ICloudListingService; const BaseDir: WideString): Boolean;
var
	CurrentDirListing: TCloudDirItemList;
	i: Integer;
	CurrentItem: TCloudDirItem;
begin
	Result := True;

	FView.SetHashesLogMessage(Format(PREFIX_SCAN, [IncludeTrailingPathDelimiter(Path)]));
	FView.ProcessMessages;

	if FView.IsHashesCancelled then
		Exit(False);

	if not ListingService.GetDirectory(Path, CurrentDirListing) then
		Exit;

	for i := 0 to Length(CurrentDirListing) - 1 do
	begin
		CurrentItem := CurrentDirListing[i];
		if CurrentItem.type_ = TYPE_DIR then
		begin
			Result := FillRecursiveHashListing(IncludeTrailingPathDelimiter(Path) + CurrentItem.name, ListingService, BaseDir);
			if not Result then
				Break;
		end
		else
			FView.AddHash(GenerateHashCommand(CurrentItem, BaseDir, Path));
	end;
end;

function TRemotePropertyPresenter.CanApplyHashes: Boolean;
begin
	Result := not FIsPublicAccount;
end;

procedure TRemotePropertyPresenter.ApplyHashCommands;
var
	Commands: TStrings;
	i: Integer;
	CurrentCommand: THashInfo;
	TargetPath: WideString;
	TrimmedLine: string;
begin
	FView.SetApplyHashesEnabled(False);
	Commands := FView.GetHashCommands;

	try
		for i := 0 to Commands.Count - 1 do
		begin
			FView.ProcessMessages;

			{Skip empty lines and comment lines (# at the start)}
			TrimmedLine := Trim(Commands[i]);
			if (TrimmedLine = '') or (TrimmedLine[1] = '#') then
				Continue;

			CurrentCommand := THashInfo.Create(Commands[i]);

			if CurrentCommand.valid then
			begin
				{Determine target path based on item type}
				if FProps.kind = TYPE_DIR then
					TargetPath := IncludeTrailingPathDelimiter(FRemotePath) + CurrentCommand.name
				else
					TargetPath := ExtractFilePath(FRemotePath) + CurrentCommand.name;

				FUploader.AddFileByIdentity(CurrentCommand.CloudFileIdentity, TargetPath, CLOUD_CONFLICT_RENAME);
			end
			else
				FView.SetHashesLogMessage(Format(ERR_LINE_HASH, [i, Commands[i], CurrentCommand.errorString]));
		end;

		{Refresh TC panel}
		PostMessage(FTCHandler.FindTCWindow, WM_USER + TC_REFRESH_MESSAGE, TC_REFRESH_PARAM, 0);
	finally
		FView.SetApplyHashesEnabled(True);
	end;
end;

class function TRemotePropertyPresenter.GenerateHashCommand(Item: TCloudDirItem; BaseDir: WideString; Path: WideString): WideString;
var
	AppliedName: WideString;
begin
	{If base directory is set, calculate relative name from it}
	if BaseDir = EmptyWideStr then
	begin
		AppliedName := Item.name;
	end else begin
		if (Path <> EmptyWideStr) then
		begin
			if (Pos(BaseDir, Path) = 1) and (BaseDir <> Path) then
				AppliedName := IncludeTrailingPathDelimiter(StringReplace(Path, BaseDir, '', [])) + Item.name
			else
				AppliedName := Item.name;
		end;
	end;

	Result := Format('hash "%s:%d:%s"', [Item.hash, Item.size, AppliedName]);
end;

function TRemotePropertyPresenter.GetDescriptionFilePath: WideString;
begin
	Result := IncludeTrailingBackslash(ExtractFileDir(FRemotePath)) + FConfig.PluginIonFileName;
end;

function TRemotePropertyPresenter.DownloadDescriptionFile(var LocalPath: WideString): Boolean;
var
	ResultHash: WideString;
begin
	LocalPath := FFileSystem.GetTmpFileName(DESCRIPTION_TEMP_EXT);
	{Download without logging errors - file may not exist}
	Result := FDownloader.Download(GetDescriptionFilePath, LocalPath, ResultHash, False) = FS_FILE_OK;
end;

procedure TRemotePropertyPresenter.LoadDescription;
var
	LocalPath: WideString;
	CurrentDescriptions: TDescription;
begin
	FView.SetDescription(EmptyWideStr);

	if not DownloadDescriptionFile(LocalPath) then
		Exit;

	CurrentDescriptions := TDescription.Create(LocalPath, FFileSystem, FTCHandler.GetTCCommentPreferredFormat);
	try
		CurrentDescriptions.Read;
		FView.SetDescription(CurrentDescriptions.GetValue(ExtractFileName(FRemotePath), FORMAT_CLEAR));
	finally
		CurrentDescriptions.Free;
	end;

	FFileSystem.DeleteFile(LocalPath);
end;

procedure TRemotePropertyPresenter.SaveDescription;
var
	RemotePath, LocalPath: WideString;
	RemoteFileExists: Boolean;
	CurrentDescriptions: TDescription;
	ResultHash: WideString;
begin
	RemotePath := GetDescriptionFilePath;
	LocalPath := FFileSystem.GetTmpFileName(DESCRIPTION_TEMP_EXT);

	{Download existing description file (if any) without logging errors}
	RemoteFileExists := FDownloader.Download(RemotePath, LocalPath, ResultHash, False) = FS_FILE_OK;

	CurrentDescriptions := TDescription.Create(LocalPath, FFileSystem, FTCHandler.GetTCCommentPreferredFormat);
	try
		if RemoteFileExists then
		begin
			{Read existing descriptions and delete remote file}
			CurrentDescriptions.Read;
			FFileOps.Delete(RemotePath);
		end;

		CurrentDescriptions.SetValue(ExtractFileName(FRemotePath), FView.GetDescription);
		CurrentDescriptions.Write();

		{Upload new description file or delete remote if local is empty}
		if FFileSystem.FileExists(CurrentDescriptions.ionFilename) then
			FUploader.Upload(CurrentDescriptions.ionFilename, RemotePath)
		else
			FFileOps.Delete(RemotePath);
	finally
		CurrentDescriptions.Free;
	end;
end;

end.
