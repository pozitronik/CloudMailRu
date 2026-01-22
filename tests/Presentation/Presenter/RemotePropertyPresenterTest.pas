unit RemotePropertyPresenterTest;

interface

uses
	DUnitX.TestFramework,
	RemotePropertyPresenter,
	CMRDirItem,
	CMRDirItemList,
	CMRInviteList,
	CMRInvite,
	CMRFileIdentity,
	CMRConstants,
	CMRSpace,
	CMRIncomingInviteList,
	CloudFileDownloader,
	CloudFileUploader,
	CloudFileOperations,
	CloudListingService,
	CloudShareService,
	CloudMailRuFactory,
	CloudMailRu,
	WindowsFileSystem,
	PLUGIN_TYPES,
	System.Classes,
	System.SysUtils,
	System.Generics.Collections;

type
	TRemotePropertyTabSet = set of TRemotePropertyTab;

type
	{Mock implementation of IRemotePropertyView for testing}
	TMockRemotePropertyView = class(TInterfacedObject, IRemotePropertyView)
	private
		FCaption: WideString;
		FWebLink: WideString;
		FWebLinkEnabled: Boolean;
		FPublishChecked: Boolean;
		FPublishEnabled: Boolean;
		FExtPropertiesVisible: Boolean;
		FVisibleTabs: TRemotePropertyTabSet;
		FInvites: TDictionary<WideString, WideString>;
		FDownloadLinks: TStringList;
		FHashes: TStringList;
		FDescription: WideString;
		FDescriptionReadOnly: Boolean;
		FDescriptionSaveEnabled: Boolean;
		FDescriptionTabCaption: WideString;
		FDownloadLinksLogMessage: WideString;
		FHashesLogMessage: WideString;
		FDownloadLinksCancelEnabled: Boolean;
		FDownloadLinksRefreshEnabled: Boolean;
		FHashesCancelEnabled: Boolean;
		FHashesRefreshEnabled: Boolean;
		FHashesMemoReadOnly: Boolean;
		FApplyHashesEnabled: Boolean;
		FLoadHashesEnabled: Boolean;
		FInviteEmailInput: WideString;
		FInviteAccessInput: Integer;
		FDownloadLinksCancelled: Boolean;
		FHashesCancelled: Boolean;
		FErrorTitle: WideString;
		FErrorMessage: WideString;
		FProcessMessagesCalled: Boolean;
	public
		constructor Create;
		destructor Destroy; override;

		{IRemotePropertyView}
		procedure SetCaption(ACaption: WideString);
		procedure SetWebLink(Link: WideString);
		procedure SetWebLinkEnabled(Enabled: Boolean);
		procedure SetPublishChecked(Checked: Boolean);
		procedure SetPublishEnabled(Enabled: Boolean);
		procedure ShowTab(Tab: TRemotePropertyTab);
		procedure HideTab(Tab: TRemotePropertyTab);
		procedure SetExtPropertiesVisible(Visible: Boolean);
		procedure ClearInvitesList;
		procedure AddInvite(Email, Access: WideString);
		function GetSelectedInviteEmail: WideString;
		function GetSelectedInviteAccess: WideString;
		procedure ClearDownloadLinks;
		procedure AddDownloadLink(Link: WideString);
		procedure SetDownloadLinksLogMessage(Message: WideString);
		procedure SetDownloadLinksCancelEnabled(Enabled: Boolean);
		procedure SetDownloadLinksRefreshEnabled(Enabled: Boolean);
		procedure ClearHashes;
		procedure AddHash(HashCommand: WideString);
		procedure SetHashesLogMessage(Message: WideString);
		procedure SetHashesCancelEnabled(Enabled: Boolean);
		procedure SetHashesRefreshEnabled(Enabled: Boolean);
		procedure SetHashesMemoReadOnly(ReadOnly: Boolean);
		procedure SetApplyHashesEnabled(Enabled: Boolean);
		procedure SetLoadHashesEnabled(Enabled: Boolean);
		function GetHashCommands: TStrings;
		procedure SetDescription(Text: WideString);
		function GetDescription: WideString;
		procedure SetDescriptionReadOnly(ReadOnly: Boolean);
		procedure SetDescriptionSaveEnabled(Enabled: Boolean);
		procedure SetDescriptionTabCaption(ACaption: WideString);
		procedure ShowError(Title, Message: WideString);
		procedure ProcessMessages;
		function IsDownloadLinksCancelled: Boolean;
		function IsHashesCancelled: Boolean;
		procedure ResetDownloadLinksCancelled;
		procedure ResetHashesCancelled;
		function GetInviteEmailInput: WideString;
		function GetInviteAccessInput: Integer;

		{Test helpers - state access}
		property Caption: WideString read FCaption;
		property WebLink: WideString read FWebLink;
		property WebLinkEnabled: Boolean read FWebLinkEnabled;
		property PublishChecked: Boolean read FPublishChecked;
		property PublishEnabled: Boolean read FPublishEnabled;
		property ExtPropertiesVisible: Boolean read FExtPropertiesVisible;
		property VisibleTabs: TRemotePropertyTabSet read FVisibleTabs;
		property Invites: TDictionary<WideString, WideString> read FInvites;
		property DownloadLinks: TStringList read FDownloadLinks;
		property Hashes: TStringList read FHashes;
		property Description: WideString read FDescription write FDescription;
		property DescriptionReadOnly: Boolean read FDescriptionReadOnly;
		property DescriptionSaveEnabled: Boolean read FDescriptionSaveEnabled;
		property DescriptionTabCaption: WideString read FDescriptionTabCaption;
		property DownloadLinksLogMessage: WideString read FDownloadLinksLogMessage;
		property HashesLogMessage: WideString read FHashesLogMessage;
		property ErrorTitle: WideString read FErrorTitle;
		property ErrorMessage: WideString read FErrorMessage;
		property ProcessMessagesCalled: Boolean read FProcessMessagesCalled;

		{Test helpers - input simulation}
		property InviteEmailInput: WideString read FInviteEmailInput write FInviteEmailInput;
		property InviteAccessInput: Integer read FInviteAccessInput write FInviteAccessInput;
		property DownloadLinksCancelled: Boolean read FDownloadLinksCancelled write FDownloadLinksCancelled;
		property HashesCancelled: Boolean read FHashesCancelled write FHashesCancelled;

		function IsTabVisible(Tab: TRemotePropertyTab): Boolean;
		procedure Reset;
	end;

	{Mock share service for testing}
	TMockShareService = class(TInterfacedObject, ICloudShareService)
	private
		FPublishResult: Boolean;
		FPublishLink: WideString;
		FUnpublishResult: Boolean;
		FShareInfoResult: Boolean;
		FShareInfoInvites: TCMRInviteList;
		FShareResult: Boolean;
		FUnshareResult: Boolean;
		FLastSharePath: WideString;
		FLastShareEmail: WideString;
		FLastShareAccess: Integer;
	public
		function Publish(Path: WideString; var PublicLink: WideString): Boolean;
		function Unpublish(Path: WideString; PublicLink: WideString): Boolean;
		function GetShareInfo(Path: WideString; var InviteListing: TCMRInviteList): Boolean;
		function Share(Path, Email: WideString; Access: Integer): Boolean;
		function Unshare(Path, Email: WideString): Boolean;
		function Mount(Home, InviteToken: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function Unmount(Home: WideString; CloneCopy: Boolean): Boolean;
		function RejectInvite(InviteToken: WideString): Boolean;
		function GetPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; Publish: Boolean = CLOUD_PUBLISH): Boolean;
		function CloneWeblink(Path, Link: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Integer;

		{Test configuration}
		property PublishResult: Boolean read FPublishResult write FPublishResult;
		property PublishLink: WideString read FPublishLink write FPublishLink;
		property UnpublishResult: Boolean read FUnpublishResult write FUnpublishResult;
		property ShareInfoResult: Boolean read FShareInfoResult write FShareInfoResult;
		property ShareInfoInvites: TCMRInviteList read FShareInfoInvites write FShareInfoInvites;
		property ShareResult: Boolean read FShareResult write FShareResult;
		property UnshareResult: Boolean read FUnshareResult write FUnshareResult;
		property LastSharePath: WideString read FLastSharePath;
		property LastShareEmail: WideString read FLastShareEmail;
		property LastShareAccess: Integer read FLastShareAccess;
	end;

	{Mock listing service for testing}
	TMockListingService = class(TInterfacedObject, ICloudListingService)
	private
		FGetDirectoryResult: Boolean;
		FDirectoryListing: TCMRDirItemList;
	public
		function GetDirectory(Path: WideString; var Listing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function GetSharedLinks(var Listing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function GetIncomingInvites(var Listing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean;
		function GetIncomingInvitesAsDirItems(var DirListing: TCMRDirItemList; var InvitesListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean;
		function GetTrashbin(var Listing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function StatusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
		function TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function TrashbinEmpty(): Boolean;
		function GetUserSpace(var SpaceInfo: TCMRSpace): Boolean;
		procedure LogUserSpaceInfo(Email: WideString);

		{Test configuration}
		property GetDirectoryResult: Boolean read FGetDirectoryResult write FGetDirectoryResult;
		property DirectoryListing: TCMRDirItemList read FDirectoryListing write FDirectoryListing;
	end;

	{Mock downloader for testing}
	TMockDownloader = class(TInterfacedObject, ICloudFileDownloader)
	private
		FDownloadResult: Integer;
		FDownloadedHash: WideString;
		FSharedFileUrl: WideString;
	public
		function Download(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = True): Integer;
		function GetSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;

		property DownloadResult: Integer read FDownloadResult write FDownloadResult;
		property DownloadedHash: WideString read FDownloadedHash write FDownloadedHash;
		property SharedFileUrl: WideString read FSharedFileUrl write FSharedFileUrl;
	end;

	{Mock uploader for testing}
	TMockUploader = class(TInterfacedObject, ICloudFileUploader)
	private
		FUploadResult: Integer;
		FAddByIdentityResult: Integer;
		FLastAddedIdentity: TCMRFileIdentity;
		FLastAddedPath: WideString;
	public
		function Upload(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer;
		function AddFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = True; LogSuccess: Boolean = False): Integer;

		property UploadResult: Integer read FUploadResult write FUploadResult;
		property AddByIdentityResult: Integer read FAddByIdentityResult write FAddByIdentityResult;
		property LastAddedIdentity: TCMRFileIdentity read FLastAddedIdentity;
		property LastAddedPath: WideString read FLastAddedPath;
	end;

	{Mock file operations for testing}
	TMockFileOps = class(TInterfacedObject, ICloudFileOperations)
	private
		FDeleteResult: Boolean;
	public
		function CreateDirectory(Path: WideString): Boolean;
		function RemoveDirectory(Path: WideString): Boolean;
		function Delete(Path: WideString): Boolean;
		function Rename(OldName, NewName: WideString): Integer;
		function MoveToPath(OldName, ToPath: WideString): Integer;
		function CopyToPath(OldName, ToPath: WideString): Integer;
		function Move(OldName, NewName: WideString): Integer;
		function Copy(OldName, NewName: WideString): Integer;

		property DeleteResult: Boolean read FDeleteResult write FDeleteResult;
	end;

	{Mock public cloud factory}
	TMockPublicCloudFactory = class(TInterfacedObject, IPublicCloudFactory)
	private
		FCreateResult: Boolean;
	public
		function CreatePublicCloud(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean;
		property CreateResult: Boolean read FCreateResult write FCreateResult;
	end;

	[TestFixture]
	TRemotePropertyPresenterTest = class
	private
		FView: TMockRemotePropertyView;
		FViewRef: IRemotePropertyView;
		FShareService: TMockShareService;
		FShareServiceRef: ICloudShareService;
		FListingService: TMockListingService;
		FListingServiceRef: ICloudListingService;
		FDownloader: TMockDownloader;
		FDownloaderRef: ICloudFileDownloader;
		FUploader: TMockUploader;
		FUploaderRef: ICloudFileUploader;
		FFileOps: TMockFileOps;
		FFileOpsRef: ICloudFileOperations;
		FPublicCloudFactory: TMockPublicCloudFactory;
		FPublicCloudFactoryRef: IPublicCloudFactory;
		FPresenter: TRemotePropertyPresenter;

		function CreateTestItem(const Name: WideString; ItemType: WideString = TYPE_FILE; ItemKind: WideString = ''; WebLink: WideString = ''): TCMRDirItem;
		function CreateConfig(ShowDescription: Boolean = True; EditDescription: Boolean = True): TRemotePropertyConfig;
	public
		[Setup]
		procedure Setup;
		[TearDown]
		procedure TearDown;

		{GenerateHashCommand tests - static method, no mocks needed}
		[Test]
		procedure TestGenerateHashCommandSimple;
		[Test]
		procedure TestGenerateHashCommandWithBaseDirSamePath;
		[Test]
		procedure TestGenerateHashCommandWithBaseDirSubPath;

		{Initialize tests}
		[Test]
		procedure TestInitializePrivateAccountNoWebLink;
		[Test]
		procedure TestInitializePrivateAccountWithWebLink;
		[Test]
		procedure TestInitializePrivateAccountDirectory;
		[Test]
		procedure TestInitializePublicAccount;
		[Test]
		procedure TestInitializeShowsHashesTab;

		{Publish/Unpublish tests}
		[Test]
		procedure TestOnPublishChangedPublishSuccess;
		[Test]
		procedure TestOnPublishChangedPublishFailed;
		[Test]
		procedure TestOnPublishChangedUnpublishSuccess;
		[Test]
		procedure TestOnPublishChangedUnpublishFailed;
		[Test]
		procedure TestOnPublishChangedIgnoredForPublicAccount;

		{Invite tests}
		[Test]
		procedure TestRefreshInvitesSuccess;
		[Test]
		procedure TestRefreshInvitesFailed;
		[Test]
		procedure TestOnInviteClickSuccess;
		[Test]
		procedure TestOnInviteClickFailed;
		[Test]
		procedure TestOnInviteDeleteClickSuccess;

		{CanApplyHashes tests}
		[Test]
		procedure TestCanApplyHashesPrivateAccount;
		[Test]
		procedure TestCanApplyHashesPublicAccount;
	end;

implementation

{TMockRemotePropertyView}

constructor TMockRemotePropertyView.Create;
begin
	inherited;
	FInvites := TDictionary<WideString, WideString>.Create;
	FDownloadLinks := TStringList.Create;
	FHashes := TStringList.Create;
	FVisibleTabs := [];
end;

destructor TMockRemotePropertyView.Destroy;
begin
	FInvites.Free;
	FDownloadLinks.Free;
	FHashes.Free;
	inherited;
end;

procedure TMockRemotePropertyView.SetCaption(ACaption: WideString);
begin
	FCaption := ACaption;
end;

procedure TMockRemotePropertyView.SetWebLink(Link: WideString);
begin
	FWebLink := Link;
end;

procedure TMockRemotePropertyView.SetWebLinkEnabled(Enabled: Boolean);
begin
	FWebLinkEnabled := Enabled;
end;

procedure TMockRemotePropertyView.SetPublishChecked(Checked: Boolean);
begin
	FPublishChecked := Checked;
end;

procedure TMockRemotePropertyView.SetPublishEnabled(Enabled: Boolean);
begin
	FPublishEnabled := Enabled;
end;

procedure TMockRemotePropertyView.ShowTab(Tab: TRemotePropertyTab);
begin
	Include(FVisibleTabs, Tab);
end;

procedure TMockRemotePropertyView.HideTab(Tab: TRemotePropertyTab);
begin
	Exclude(FVisibleTabs, Tab);
end;

procedure TMockRemotePropertyView.SetExtPropertiesVisible(Visible: Boolean);
begin
	FExtPropertiesVisible := Visible;
end;

procedure TMockRemotePropertyView.ClearInvitesList;
begin
	FInvites.Clear;
end;

procedure TMockRemotePropertyView.AddInvite(Email, Access: WideString);
begin
	FInvites.AddOrSetValue(Email, Access);
end;

function TMockRemotePropertyView.GetSelectedInviteEmail: WideString;
var
	Key: WideString;
begin
	{For testing, return first key}
	Result := '';
	if FInvites.Count > 0 then
		for Key in FInvites.Keys do
		begin
			Result := Key;
			Exit;
		end;
end;

function TMockRemotePropertyView.GetSelectedInviteAccess: WideString;
var
	Email: WideString;
begin
	Email := GetSelectedInviteEmail;
	if FInvites.ContainsKey(Email) then
		Result := FInvites[Email]
	else
		Result := '';
end;

procedure TMockRemotePropertyView.ClearDownloadLinks;
begin
	FDownloadLinks.Clear;
end;

procedure TMockRemotePropertyView.AddDownloadLink(Link: WideString);
begin
	FDownloadLinks.Add(Link);
end;

procedure TMockRemotePropertyView.SetDownloadLinksLogMessage(Message: WideString);
begin
	FDownloadLinksLogMessage := Message;
end;

procedure TMockRemotePropertyView.SetDownloadLinksCancelEnabled(Enabled: Boolean);
begin
	FDownloadLinksCancelEnabled := Enabled;
end;

procedure TMockRemotePropertyView.SetDownloadLinksRefreshEnabled(Enabled: Boolean);
begin
	FDownloadLinksRefreshEnabled := Enabled;
end;

procedure TMockRemotePropertyView.ClearHashes;
begin
	FHashes.Clear;
end;

procedure TMockRemotePropertyView.AddHash(HashCommand: WideString);
begin
	FHashes.Add(HashCommand);
end;

procedure TMockRemotePropertyView.SetHashesLogMessage(Message: WideString);
begin
	FHashesLogMessage := Message;
end;

procedure TMockRemotePropertyView.SetHashesCancelEnabled(Enabled: Boolean);
begin
	FHashesCancelEnabled := Enabled;
end;

procedure TMockRemotePropertyView.SetHashesRefreshEnabled(Enabled: Boolean);
begin
	FHashesRefreshEnabled := Enabled;
end;

procedure TMockRemotePropertyView.SetHashesMemoReadOnly(ReadOnly: Boolean);
begin
	FHashesMemoReadOnly := ReadOnly;
end;

procedure TMockRemotePropertyView.SetApplyHashesEnabled(Enabled: Boolean);
begin
	FApplyHashesEnabled := Enabled;
end;

procedure TMockRemotePropertyView.SetLoadHashesEnabled(Enabled: Boolean);
begin
	FLoadHashesEnabled := Enabled;
end;

function TMockRemotePropertyView.GetHashCommands: TStrings;
begin
	Result := FHashes;
end;

procedure TMockRemotePropertyView.SetDescription(Text: WideString);
begin
	FDescription := Text;
end;

function TMockRemotePropertyView.GetDescription: WideString;
begin
	Result := FDescription;
end;

procedure TMockRemotePropertyView.SetDescriptionReadOnly(ReadOnly: Boolean);
begin
	FDescriptionReadOnly := ReadOnly;
end;

procedure TMockRemotePropertyView.SetDescriptionSaveEnabled(Enabled: Boolean);
begin
	FDescriptionSaveEnabled := Enabled;
end;

procedure TMockRemotePropertyView.SetDescriptionTabCaption(ACaption: WideString);
begin
	FDescriptionTabCaption := ACaption;
end;

procedure TMockRemotePropertyView.ShowError(Title, Message: WideString);
begin
	FErrorTitle := Title;
	FErrorMessage := Message;
end;

procedure TMockRemotePropertyView.ProcessMessages;
begin
	FProcessMessagesCalled := True;
end;

function TMockRemotePropertyView.IsDownloadLinksCancelled: Boolean;
begin
	Result := FDownloadLinksCancelled;
end;

function TMockRemotePropertyView.IsHashesCancelled: Boolean;
begin
	Result := FHashesCancelled;
end;

procedure TMockRemotePropertyView.ResetDownloadLinksCancelled;
begin
	FDownloadLinksCancelled := False;
end;

procedure TMockRemotePropertyView.ResetHashesCancelled;
begin
	FHashesCancelled := False;
end;

function TMockRemotePropertyView.GetInviteEmailInput: WideString;
begin
	Result := FInviteEmailInput;
end;

function TMockRemotePropertyView.GetInviteAccessInput: Integer;
begin
	Result := FInviteAccessInput;
end;

function TMockRemotePropertyView.IsTabVisible(Tab: TRemotePropertyTab): Boolean;
begin
	Result := Tab in FVisibleTabs;
end;

procedure TMockRemotePropertyView.Reset;
begin
	FCaption := '';
	FWebLink := '';
	FWebLinkEnabled := False;
	FPublishChecked := False;
	FPublishEnabled := False;
	FExtPropertiesVisible := False;
	FVisibleTabs := [];
	FInvites.Clear;
	FDownloadLinks.Clear;
	FHashes.Clear;
	FDescription := '';
	FDescriptionReadOnly := False;
	FDescriptionSaveEnabled := False;
	FDescriptionTabCaption := '';
	FDownloadLinksLogMessage := '';
	FHashesLogMessage := '';
	FErrorTitle := '';
	FErrorMessage := '';
	FProcessMessagesCalled := False;
	FDownloadLinksCancelled := False;
	FHashesCancelled := False;
end;

{TMockShareService}

function TMockShareService.Publish(Path: WideString; var PublicLink: WideString): Boolean;
begin
	PublicLink := FPublishLink;
	Result := FPublishResult;
end;

function TMockShareService.Unpublish(Path: WideString; PublicLink: WideString): Boolean;
begin
	Result := FUnpublishResult;
end;

function TMockShareService.GetShareInfo(Path: WideString; var InviteListing: TCMRInviteList): Boolean;
begin
	InviteListing := FShareInfoInvites;
	Result := FShareInfoResult;
end;

function TMockShareService.Share(Path, Email: WideString; Access: Integer): Boolean;
begin
	FLastSharePath := Path;
	FLastShareEmail := Email;
	FLastShareAccess := Access;
	Result := FShareResult;
end;

function TMockShareService.Unshare(Path, Email: WideString): Boolean;
begin
	FLastSharePath := Path;
	FLastShareEmail := Email;
	Result := FUnshareResult;
end;

function TMockShareService.Mount(Home, InviteToken: WideString; ConflictMode: WideString): Boolean;
begin
	Result := True;
end;

function TMockShareService.Unmount(Home: WideString; CloneCopy: Boolean): Boolean;
begin
	Result := True;
end;

function TMockShareService.RejectInvite(InviteToken: WideString): Boolean;
begin
	Result := True;
end;

function TMockShareService.GetPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString; Publish: Boolean): Boolean;
begin
	Result := True;
end;

function TMockShareService.CloneWeblink(Path, Link: WideString; ConflictMode: WideString): Integer;
begin
	Result := FS_FILE_OK;
end;

{TMockListingService}

function TMockListingService.GetDirectory(Path: WideString; var Listing: TCMRDirItemList; ShowProgress: Boolean): Boolean;
begin
	Listing := FDirectoryListing;
	Result := FGetDirectoryResult;
end;

function TMockListingService.GetSharedLinks(var Listing: TCMRDirItemList; ShowProgress: Boolean): Boolean;
begin
	Result := False;
end;

function TMockListingService.GetIncomingInvites(var Listing: TCMRIncomingInviteList; ShowProgress: Boolean): Boolean;
begin
	Result := False;
end;

function TMockListingService.GetIncomingInvitesAsDirItems(var DirListing: TCMRDirItemList; var InvitesListing: TCMRIncomingInviteList; ShowProgress: Boolean): Boolean;
begin
	Result := False;
end;

function TMockListingService.GetTrashbin(var Listing: TCMRDirItemList; ShowProgress: Boolean): Boolean;
begin
	Result := False;
end;

function TMockListingService.StatusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
begin
	Result := False;
end;

function TMockListingService.TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString): Boolean;
begin
	Result := False;
end;

function TMockListingService.TrashbinEmpty(): Boolean;
begin
	Result := False;
end;

function TMockListingService.GetUserSpace(var SpaceInfo: TCMRSpace): Boolean;
begin
	Result := False;
end;

procedure TMockListingService.LogUserSpaceInfo(Email: WideString);
begin
end;

{TMockDownloader}

function TMockDownloader.Download(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
begin
	ResultHash := FDownloadedHash;
	Result := FDownloadResult;
end;

function TMockDownloader.GetSharedFileUrl(RemotePath: WideString; ShardType: WideString): WideString;
begin
	Result := FSharedFileUrl + RemotePath;
end;

{TMockUploader}

function TMockUploader.Upload(LocalPath, RemotePath: WideString; ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
begin
	Result := FUploadResult;
end;

function TMockUploader.AddFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath: WideString; ConflictMode: WideString; LogErrors, LogSuccess: Boolean): Integer;
begin
	FLastAddedIdentity := FileIdentity;
	FLastAddedPath := RemotePath;
	Result := FAddByIdentityResult;
end;

{TMockFileOps}

function TMockFileOps.CreateDirectory(Path: WideString): Boolean;
begin
	Result := True;
end;

function TMockFileOps.RemoveDirectory(Path: WideString): Boolean;
begin
	Result := True;
end;

function TMockFileOps.Delete(Path: WideString): Boolean;
begin
	Result := FDeleteResult;
end;

function TMockFileOps.Rename(OldName, NewName: WideString): Integer;
begin
	Result := FS_FILE_OK;
end;

function TMockFileOps.MoveToPath(OldName, ToPath: WideString): Integer;
begin
	Result := FS_FILE_OK;
end;

function TMockFileOps.CopyToPath(OldName, ToPath: WideString): Integer;
begin
	Result := FS_FILE_OK;
end;

function TMockFileOps.Move(OldName, NewName: WideString): Integer;
begin
	Result := FS_FILE_OK;
end;

function TMockFileOps.Copy(OldName, NewName: WideString): Integer;
begin
	Result := FS_FILE_OK;
end;

{TMockPublicCloudFactory}

function TMockPublicCloudFactory.CreatePublicCloud(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean;
begin
	TempCloud := nil;
	Result := FCreateResult;
end;

{TRemotePropertyPresenterTest}

procedure TRemotePropertyPresenterTest.Setup;
begin
	FView := TMockRemotePropertyView.Create;
	FViewRef := FView;

	FShareService := TMockShareService.Create;
	FShareServiceRef := FShareService;
	FShareService.ShareInfoResult := True;
	FShareService.ShareResult := True;
	FShareService.UnshareResult := True;

	FListingService := TMockListingService.Create;
	FListingServiceRef := FListingService;
	FListingService.GetDirectoryResult := True;

	FDownloader := TMockDownloader.Create;
	FDownloaderRef := FDownloader;
	FDownloader.DownloadResult := FS_FILE_OK;
	FDownloader.SharedFileUrl := 'https://cloud.mail.ru/dl/';

	FUploader := TMockUploader.Create;
	FUploaderRef := FUploader;
	FUploader.UploadResult := FS_FILE_OK;
	FUploader.AddByIdentityResult := FS_FILE_OK;

	FFileOps := TMockFileOps.Create;
	FFileOpsRef := FFileOps;
	FFileOps.DeleteResult := True;

	FPublicCloudFactory := TMockPublicCloudFactory.Create;
	FPublicCloudFactoryRef := FPublicCloudFactory;
	FPublicCloudFactory.CreateResult := False; {Default to false - no temp cloud}
end;

procedure TRemotePropertyPresenterTest.TearDown;
begin
	FPresenter.Free;
	FPresenter := nil;
	FViewRef := nil;
	FShareServiceRef := nil;
	FListingServiceRef := nil;
	FDownloaderRef := nil;
	FUploaderRef := nil;
	FFileOpsRef := nil;
	FPublicCloudFactoryRef := nil;
end;

function TRemotePropertyPresenterTest.CreateTestItem(const Name: WideString; ItemType, ItemKind, WebLink: WideString): TCMRDirItem;
begin
	Result := Default(TCMRDirItem);
	Result.name := Name;
	Result.type_ := ItemType;
	Result.kind := ItemKind;
	Result.WebLink := WebLink;
	Result.home := '/' + Name;
	Result.hash := 'ABC123';
	Result.size := 1024;
end;

function TRemotePropertyPresenterTest.CreateConfig(ShowDescription, EditDescription: Boolean): TRemotePropertyConfig;
begin
	Result.DoUrlEncode := True;
	Result.AutoUpdateDownloadListing := False;
	Result.ShowDescription := ShowDescription;
	Result.EditDescription := EditDescription;
	Result.PluginIonFileName := 'descript.ion';
end;

{GenerateHashCommand tests}

procedure TRemotePropertyPresenterTest.TestGenerateHashCommandSimple;
var
	Item: TCMRDirItem;
	Result: WideString;
begin
	Item := CreateTestItem('test.txt');
	Item.hash := 'DEADBEEF';
	Item.size := 12345;

	Result := TRemotePropertyPresenter.GenerateHashCommand(Item);

	Assert.AreEqual('hash "DEADBEEF:12345:test.txt"', String(Result));
end;

procedure TRemotePropertyPresenterTest.TestGenerateHashCommandWithBaseDirSamePath;
var
	Item: TCMRDirItem;
	Result: WideString;
begin
	Item := CreateTestItem('test.txt');
	Item.hash := 'DEADBEEF';
	Item.size := 12345;

	{When path equals basedir, use just the filename}
	Result := TRemotePropertyPresenter.GenerateHashCommand(Item, '/folder/', '/folder/');

	Assert.AreEqual('hash "DEADBEEF:12345:test.txt"', String(Result));
end;

procedure TRemotePropertyPresenterTest.TestGenerateHashCommandWithBaseDirSubPath;
var
	Item: TCMRDirItem;
	Result: WideString;
begin
	Item := CreateTestItem('test.txt');
	Item.hash := 'DEADBEEF';
	Item.size := 12345;

	{When path is subdir of basedir, use relative path}
	Result := TRemotePropertyPresenter.GenerateHashCommand(Item, '/folder/', '/folder/sub');

	Assert.AreEqual('hash "DEADBEEF:12345:sub\test.txt"', String(Result));
end;

{Initialize tests}

procedure TRemotePropertyPresenterTest.TestInitializePrivateAccountNoWebLink;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('test.txt');

	FPresenter.Initialize(Item, '/test.txt', CreateConfig(False, False));

	Assert.AreEqual('test.txt', String(FView.Caption));
	Assert.IsFalse(FView.PublishChecked, 'Should not be published');
	Assert.IsFalse(FView.WebLinkEnabled, 'WebLink should be disabled');
	Assert.IsFalse(FView.IsTabVisible(rptFolderAccess), 'FolderAccess tab should be hidden');
	Assert.IsTrue(FView.IsTabVisible(rptHashesList), 'Hashes tab should be visible');
end;

procedure TRemotePropertyPresenterTest.TestInitializePrivateAccountWithWebLink;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('test.txt', TYPE_FILE, '', 'ABC123XYZ');

	FPresenter.Initialize(Item, '/test.txt', CreateConfig(False, False));

	Assert.IsTrue(FView.PublishChecked, 'Should be published');
	Assert.IsTrue(FView.WebLinkEnabled, 'WebLink should be enabled');
	Assert.Contains(FView.WebLink, 'ABC123XYZ', 'WebLink should contain the link');
end;

procedure TRemotePropertyPresenterTest.TestInitializePrivateAccountDirectory;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('folder', TYPE_DIR);

	FPresenter.Initialize(Item, '/folder', CreateConfig(False, False));

	Assert.IsTrue(FView.ExtPropertiesVisible, 'ExtProperties should be visible');
	Assert.IsTrue(FView.IsTabVisible(rptFolderAccess), 'FolderAccess tab should be visible for directories');
end;

procedure TRemotePropertyPresenterTest.TestInitializePublicAccount;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, True);

	Item := CreateTestItem('test.txt');

	FPresenter.Initialize(Item, '/test.txt', CreateConfig(False, False));

	Assert.IsFalse(FView.PublishEnabled, 'Publish checkbox should be disabled for public account');
	Assert.IsTrue(FView.PublishChecked, 'Should show as published');
	Assert.IsTrue(FView.IsTabVisible(rptDownloadLinks), 'DownloadLinks tab should be visible');
	Assert.IsFalse(FPresenter.CanApplyHashes, 'Should not be able to apply hashes');
end;

procedure TRemotePropertyPresenterTest.TestInitializeShowsHashesTab;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('test.txt');

	FPresenter.Initialize(Item, '/test.txt', CreateConfig(False, False));

	Assert.IsTrue(FView.IsTabVisible(rptHashesList), 'Hashes tab should always be visible');
end;

{Publish/Unpublish tests}

procedure TRemotePropertyPresenterTest.TestOnPublishChangedPublishSuccess;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('test.txt');
	FPresenter.Initialize(Item, '/test.txt', CreateConfig(False, False));

	FShareService.PublishResult := True;
	FShareService.PublishLink := 'NEWLINK123';

	FPresenter.OnPublishChanged(True);

	Assert.Contains(FView.WebLink, 'NEWLINK123', 'WebLink should contain new link');
	Assert.IsTrue(FView.WebLinkEnabled, 'WebLink should be enabled');
	Assert.IsTrue(FView.IsTabVisible(rptDownloadLinks), 'DownloadLinks tab should become visible');
end;

procedure TRemotePropertyPresenterTest.TestOnPublishChangedPublishFailed;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('test.txt');
	FPresenter.Initialize(Item, '/test.txt', CreateConfig(False, False));

	FShareService.PublishResult := False;

	FPresenter.OnPublishChanged(True);

	Assert.IsFalse(FView.PublishChecked, 'Checkbox should be unchecked on failure');
	Assert.IsNotEmpty(FView.ErrorTitle, 'Error should be shown');
end;

procedure TRemotePropertyPresenterTest.TestOnPublishChangedUnpublishSuccess;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('test.txt', TYPE_FILE, '', 'EXISTINGLINK');
	FPresenter.Initialize(Item, '/test.txt', CreateConfig(False, False));

	FShareService.UnpublishResult := True;

	FPresenter.OnPublishChanged(False);

	Assert.IsEmpty(FView.WebLink, 'WebLink should be cleared');
	Assert.IsFalse(FView.WebLinkEnabled, 'WebLink should be disabled');
	Assert.IsFalse(FView.IsTabVisible(rptDownloadLinks), 'DownloadLinks tab should be hidden');
end;

procedure TRemotePropertyPresenterTest.TestOnPublishChangedUnpublishFailed;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('test.txt', TYPE_FILE, '', 'EXISTINGLINK');
	FPresenter.Initialize(Item, '/test.txt', CreateConfig(False, False));

	FShareService.UnpublishResult := False;

	FPresenter.OnPublishChanged(False);

	Assert.IsTrue(FView.PublishChecked, 'Checkbox should remain checked on failure');
	Assert.IsNotEmpty(FView.ErrorTitle, 'Error should be shown');
end;

procedure TRemotePropertyPresenterTest.TestOnPublishChangedIgnoredForPublicAccount;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, True);

	Item := CreateTestItem('test.txt');
	FPresenter.Initialize(Item, '/test.txt', CreateConfig(False, False));
	FView.Reset;

	FShareService.PublishResult := True;
	FShareService.PublishLink := 'NEWLINK';

	FPresenter.OnPublishChanged(True);

	{For public accounts, nothing should change}
	Assert.IsEmpty(FView.WebLink, 'WebLink should not change for public account');
end;

{Invite tests}

procedure TRemotePropertyPresenterTest.TestRefreshInvitesSuccess;
var
	Item: TCMRDirItem;
	Invites: TCMRInviteList;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('folder', TYPE_DIR);

	SetLength(Invites, 2);
	Invites[0].email := 'user1@mail.ru';
	Invites[0].access := CLOUD_SHARE_ACCESS_READ_WRITE;
	Invites[1].email := 'user2@mail.ru';
	Invites[1].access := CLOUD_SHARE_ACCESS_READ_ONLY;
	FShareService.ShareInfoInvites := Invites;
	FShareService.ShareInfoResult := True;

	FPresenter.Initialize(Item, '/folder', CreateConfig(False, False));

	Assert.AreEqual<Integer>(2, FView.Invites.Count, 'Should have 2 invites');
	Assert.IsTrue(FView.Invites.ContainsKey('user1@mail.ru'), 'Should contain user1');
	Assert.IsTrue(FView.Invites.ContainsKey('user2@mail.ru'), 'Should contain user2');
end;

procedure TRemotePropertyPresenterTest.TestRefreshInvitesFailed;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('folder', TYPE_DIR);
	FShareService.ShareInfoResult := False;

	FPresenter.Initialize(Item, '/folder', CreateConfig(False, False));

	Assert.AreEqual<Integer>(0, FView.Invites.Count, 'Should have no invites');
	Assert.IsNotEmpty(FView.ErrorTitle, 'Error should be shown');
end;

procedure TRemotePropertyPresenterTest.TestOnInviteClickSuccess;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('folder', TYPE_DIR);
	FShareService.ShareInfoResult := True;
	FPresenter.Initialize(Item, '/folder', CreateConfig(False, False));

	FView.InviteEmailInput := 'newuser@mail.ru';
	FView.InviteAccessInput := CLOUD_SHARE_RW;
	FShareService.ShareResult := True;

	FPresenter.OnInviteClick;

	Assert.AreEqual('/folder', String(FShareService.LastSharePath), 'Should share correct path');
	Assert.AreEqual('newuser@mail.ru', String(FShareService.LastShareEmail), 'Should share with correct email');
	Assert.AreEqual(CLOUD_SHARE_RW, FShareService.LastShareAccess, 'Should share with correct access');
end;

procedure TRemotePropertyPresenterTest.TestOnInviteClickFailed;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('folder', TYPE_DIR);
	FShareService.ShareInfoResult := True;
	FPresenter.Initialize(Item, '/folder', CreateConfig(False, False));
	FView.Reset;

	FView.InviteEmailInput := 'newuser@mail.ru';
	FView.InviteAccessInput := CLOUD_SHARE_RW;
	FShareService.ShareResult := False;

	FPresenter.OnInviteClick;

	Assert.IsNotEmpty(FView.ErrorTitle, 'Error should be shown');
end;

procedure TRemotePropertyPresenterTest.TestOnInviteDeleteClickSuccess;
var
	Item: TCMRDirItem;
	Invites: TCMRInviteList;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('folder', TYPE_DIR);

	SetLength(Invites, 1);
	Invites[0].email := 'user@mail.ru';
	Invites[0].access := CLOUD_SHARE_ACCESS_READ_WRITE;
	FShareService.ShareInfoInvites := Invites;
	FShareService.ShareInfoResult := True;
	FShareService.UnshareResult := True;

	FPresenter.Initialize(Item, '/folder', CreateConfig(False, False));

	FPresenter.OnInviteDeleteClick;

	Assert.AreEqual('/folder', String(FShareService.LastSharePath), 'Should unshare correct path');
	Assert.AreEqual('user@mail.ru', String(FShareService.LastShareEmail), 'Should unshare correct email');
end;

{CanApplyHashes tests}

procedure TRemotePropertyPresenterTest.TestCanApplyHashesPrivateAccount;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, False);

	Item := CreateTestItem('test.txt');
	FPresenter.Initialize(Item, '/test.txt', CreateConfig(False, False));

	Assert.IsTrue(FPresenter.CanApplyHashes, 'Private accounts should be able to apply hashes');
end;

procedure TRemotePropertyPresenterTest.TestCanApplyHashesPublicAccount;
var
	Item: TCMRDirItem;
begin
	FPresenter := TRemotePropertyPresenter.Create(FViewRef, FDownloaderRef, FUploaderRef, FFileOpsRef, FListingServiceRef, FShareServiceRef, TMemoryFileSystem.Create, FPublicCloudFactoryRef, True);

	Item := CreateTestItem('test.txt');
	FPresenter.Initialize(Item, '/test.txt', CreateConfig(False, False));

	Assert.IsFalse(FPresenter.CanApplyHashes, 'Public accounts should not be able to apply hashes');
end;

initialization

TDUnitX.RegisterTestFixture(TRemotePropertyPresenterTest);

end.
