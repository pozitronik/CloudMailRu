unit RemoteProperty;

{Properties dialog for cloud files/folders.
	Implements IRemotePropertyView for MVP pattern.
	Business logic delegated to TRemotePropertyPresenter.}

interface

uses
	CloudDirItemList,
	CloudDirItem,
	CloudInviteList,
	WFXTypes,
	Description,
	CloudConstants,
	LanguageStrings,
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.StdCtrls,
	CloudMailRu,
	CloudMailRuFactory,
	CloudAccessMapper,
	WindowsHelper,
	TCHandler,
	SystemHelper,
	Vcl.Grids,
	Vcl.ValEdit,
	Vcl.Menus,
	Vcl.ComCtrls,
	Vcl.ToolWin,
	System.ImageList,
	Vcl.ImgList,
	Clipbrd,
	HashInfo,
	WindowsFileSystem,
	RemotePropertyPresenter;

const
	WM_AFTER_SHOW = WM_USER + 300; {Custom message for post-show initialization}

type
	{Properties dialog form implementing IRemotePropertyView}
	TPropertyForm = class(TForm, IRemotePropertyView)
		PublicLinkLabel: TLabel;
		WebLink: TEdit;
		AccessCB: TCheckBox;
		OkButton: TButton;
		InvitesPopup: TPopupMenu;
		ItemChangeAccess: TMenuItem;
		ItemDelete: TMenuItem;
		ItemRefresh: TMenuItem;
		N1: TMenuItem;
		ExtPropertiesPC: TPageControl;
		FolderAccessTS: TTabSheet;
		DownloadLinksTS: TTabSheet;
		InviteEmailLabel: TLabel;
		AccessLabel: TLabel;
		InviteEmailEdit: TEdit;
		InviteAcessCB: TComboBox;
		InviteBtn: TButton;
		InvitesLE: TValueListEditor;
		DownloadLinksMemo: TMemo;
		DownloadLinksTB: TToolBar;
		SaveLinksTb: TToolButton;
		DownloadLinksIL: TImageList;
		WrapLinksTb: TToolButton;
		SaveDialogSD: TSaveDialog;
		LinksLogLabel: TLabel;
		CancelLinksScanTb: TToolButton;
		RefreshLinksScanTb: TToolButton;
		DescriptionTS: TTabSheet;
		DescriptionEditMemo: TMemo;
		DescriptionSaveButton: TButton;
		HashesListTS: TTabSheet;
		HashesListTB: TToolBar;
		SaveHashesTb: TToolButton;
		WrapHashesTb: TToolButton;
		HashesLogLabel: TLabel;
		CancelHashesScanTb: TToolButton;
		RefreshHashesScanTb: TToolButton;
		HashesMemo: TMemo;
		LoadHashesTb: TToolButton;
		OpenDialogOD: TOpenDialog;
		ApplyHashesTB: TToolButton;
		procedure AccessCBClick(Sender: TObject);
		class function ShowProperty(parentWindow: HWND; RemoteName: WideString; RemoteProperty: TCloudDirItem; Cloud: TCloudMailRu; FileSystem: IFileSystem; TCHandler: ITCHandler; DoUrlEncode: Boolean = true; AutoUpdateDownloadListing: Boolean = true; ShowDescription: Boolean = true; EditDescription: Boolean = true; PluginIonFileName: WideString = 'descript.ion'): Integer;
		procedure FormActivate(Sender: TObject);
		procedure InviteBtnClick(Sender: TObject);
		procedure ItemDeleteClick(Sender: TObject);
		procedure ItemRefreshClick(Sender: TObject);
		procedure InvitesPopupPopup(Sender: TObject);
		procedure ItemChangeAccessClick(Sender: TObject);
		procedure WrapLinksTbClick(Sender: TObject);
		procedure SaveLinksTbClick(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure CancelLinksScanTbClick(Sender: TObject);
		procedure RefreshLinksScanTbClick(Sender: TObject);
		procedure DescriptionSaveButtonClick(Sender: TObject);
		procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure PublicLinkLabelClick(Sender: TObject);
		procedure RefreshHashesScanTbClick(Sender: TObject);
		procedure CancelHashesScanTbClick(Sender: TObject);
		procedure WrapHashesTbClick(Sender: TObject);
		procedure SaveHashesTbClick(Sender: TObject);
		procedure LoadHashesTbClick(Sender: TObject);
		procedure ApplyHashesTBClick(Sender: TObject);
		procedure HashesMemoExit(Sender: TObject);
	private
		{Presenter handles business logic}
		FPresenter: TRemotePropertyPresenter;
		{Cancellation flags}
		FDownloadLinksCancelled: Boolean;
		FHashesCancelled: Boolean;
		{Post-show initialization}
		procedure WMAfterShow(var Message: TMessage); message WM_AFTER_SHOW;
	protected
		{IRemotePropertyView implementation}
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
	public
		destructor Destroy; override;
	end;

var
	PropertyForm: TPropertyForm;

implementation

{$R *.dfm}

uses
	PathHelper;

{TPropertyForm - IRemotePropertyView implementation}

procedure TPropertyForm.SetCaption(ACaption: WideString);
begin
	Caption := ACaption;
end;

procedure TPropertyForm.SetWebLink(Link: WideString);
begin
	WebLink.Text := Link;
	{Only focus when form is active - SetFocus fails if form not yet shown}
	if (Link <> EmptyWideStr) and Active then
	begin
		WebLink.SetFocus;
		WebLink.SelectAll;
	end;
end;

procedure TPropertyForm.SetWebLinkEnabled(Enabled: Boolean);
begin
	WebLink.Enabled := Enabled;
end;

procedure TPropertyForm.SetPublishChecked(Checked: Boolean);
begin
	AccessCB.Checked := Checked;
end;

procedure TPropertyForm.SetPublishEnabled(Enabled: Boolean);
begin
	AccessCB.Enabled := Enabled;
end;

procedure TPropertyForm.ShowTab(Tab: TRemotePropertyTab);
begin
	case Tab of
		rptFolderAccess:
			FolderAccessTS.TabVisible := true;
		rptDownloadLinks:
			DownloadLinksTS.TabVisible := true;
		rptDescription:
			DescriptionTS.TabVisible := true;
		rptHashesList:
			HashesListTS.TabVisible := true;
	end;
end;

procedure TPropertyForm.HideTab(Tab: TRemotePropertyTab);
begin
	case Tab of
		rptFolderAccess:
			FolderAccessTS.TabVisible := False;
		rptDownloadLinks:
			DownloadLinksTS.TabVisible := False;
		rptDescription:
			DescriptionTS.TabVisible := False;
		rptHashesList:
			HashesListTS.TabVisible := False;
	end;
end;

procedure TPropertyForm.SetExtPropertiesVisible(Visible: Boolean);
begin
	ExtPropertiesPC.Visible := Visible;
end;

procedure TPropertyForm.ClearInvitesList;
begin
	while InvitesLE.Strings.Count > 0 do
		InvitesLE.DeleteRow(1);
end;

procedure TPropertyForm.AddInvite(Email, Access: WideString);
begin
	InvitesLE.InsertRow(Email, Access, true);
end;

function TPropertyForm.GetSelectedInviteEmail: WideString;
begin
	Result := InvitesLE.Keys[InvitesLE.Row];
end;

function TPropertyForm.GetSelectedInviteAccess: WideString;
begin
	Result := InvitesLE.Values[GetSelectedInviteEmail];
end;

procedure TPropertyForm.ClearDownloadLinks;
begin
	DownloadLinksMemo.Lines.Clear;
end;

procedure TPropertyForm.AddDownloadLink(Link: WideString);
begin
	DownloadLinksMemo.Lines.Add(Link);
end;

procedure TPropertyForm.SetDownloadLinksLogMessage(Message: WideString);
begin
	LinksLogLabel.Caption := Message;
end;

procedure TPropertyForm.SetDownloadLinksCancelEnabled(Enabled: Boolean);
begin
	CancelLinksScanTb.Enabled := Enabled;
end;

procedure TPropertyForm.SetDownloadLinksRefreshEnabled(Enabled: Boolean);
begin
	RefreshLinksScanTb.Enabled := Enabled;
end;

procedure TPropertyForm.ClearHashes;
begin
	HashesMemo.Lines.Clear;
end;

procedure TPropertyForm.AddHash(HashCommand: WideString);
begin
	HashesMemo.Lines.Add(HashCommand);
end;

procedure TPropertyForm.SetHashesLogMessage(Message: WideString);
begin
	HashesLogLabel.Caption := Message;
end;

procedure TPropertyForm.SetHashesCancelEnabled(Enabled: Boolean);
begin
	CancelHashesScanTb.Enabled := Enabled;
end;

procedure TPropertyForm.SetHashesRefreshEnabled(Enabled: Boolean);
begin
	RefreshHashesScanTb.Enabled := Enabled;
end;

procedure TPropertyForm.SetHashesMemoReadOnly(ReadOnly: Boolean);
begin
	HashesMemo.ReadOnly := ReadOnly;
end;

procedure TPropertyForm.SetApplyHashesEnabled(Enabled: Boolean);
begin
	ApplyHashesTB.Enabled := Enabled;
end;

procedure TPropertyForm.SetLoadHashesEnabled(Enabled: Boolean);
begin
	LoadHashesTb.Enabled := Enabled;
end;

function TPropertyForm.GetHashCommands: TStrings;
begin
	Result := HashesMemo.Lines;
end;

procedure TPropertyForm.SetDescription(Text: WideString);
begin
	DescriptionEditMemo.Lines.Text := Text;
end;

function TPropertyForm.GetDescription: WideString;
begin
	Result := DescriptionEditMemo.Lines.Text;
end;

procedure TPropertyForm.SetDescriptionReadOnly(ReadOnly: Boolean);
begin
	DescriptionEditMemo.ReadOnly := ReadOnly;
end;

procedure TPropertyForm.SetDescriptionSaveEnabled(Enabled: Boolean);
begin
	DescriptionSaveButton.Enabled := Enabled;
end;

procedure TPropertyForm.SetDescriptionTabCaption(ACaption: WideString);
begin
	DescriptionTS.Caption := ACaption;
end;

procedure TPropertyForm.ShowError(Title, Message: WideString);
begin
	MessageBoxW(Handle, PWideChar(Message), PWideChar(Title), MB_OK + MB_ICONERROR);
end;

procedure TPropertyForm.ProcessMessages;
begin
	SystemHelper.ProcessMessages;
end;

function TPropertyForm.IsDownloadLinksCancelled: Boolean;
begin
	Result := FDownloadLinksCancelled;
end;

function TPropertyForm.IsHashesCancelled: Boolean;
begin
	Result := FHashesCancelled;
end;

procedure TPropertyForm.ResetDownloadLinksCancelled;
begin
	FDownloadLinksCancelled := False;
end;

procedure TPropertyForm.ResetHashesCancelled;
begin
	FHashesCancelled := False;
end;

function TPropertyForm.GetInviteEmailInput: WideString;
begin
	Result := InviteEmailEdit.Text;
end;

function TPropertyForm.GetInviteAccessInput: Integer;
begin
	Result := InviteAcessCB.ItemIndex;
end;

destructor TPropertyForm.Destroy;
begin
	FPresenter.Free;
	inherited;
end;

{TPropertyForm - Event handlers}

procedure TPropertyForm.AccessCBClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnPublishChanged(AccessCB.Checked);
end;

procedure TPropertyForm.FormActivate(Sender: TObject);
begin
	CenterWindow(parentWindow, Handle);
end;

procedure TPropertyForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	case Key of
		VK_ESCAPE:
			Close;
		VK_F2:
			begin
				if Assigned(FPresenter) then
				begin
					FPresenter.SaveDescription;
					ModalResult := IDCONTINUE;
					CloseModal;
				end;
			end;
	end;
end;

procedure TPropertyForm.FormShow(Sender: TObject);
begin
	PostMessage(Handle, WM_AFTER_SHOW, 0, 0);
end;

procedure TPropertyForm.WMAfterShow(var Message: TMessage);
begin
	{Presenter initialization happens after form is shown}
	{View state already set by presenter during Initialize call}
end;

procedure TPropertyForm.InviteBtnClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnInviteClick;
end;

procedure TPropertyForm.ItemDeleteClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnInviteDeleteClick;
end;

procedure TPropertyForm.ItemRefreshClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.RefreshInvites;
end;

procedure TPropertyForm.InvitesPopupPopup(Sender: TObject);
var
	Email, Access: WideString;
begin
	Email := GetSelectedInviteEmail;
	if Email = EmptyWideStr then
	begin
		ItemChangeAccess.Visible := False;
		ItemDelete.Visible := False;
		Exit;
	end else begin
		ItemChangeAccess.Visible := true;
		ItemDelete.Visible := true;
	end;

	Access := GetSelectedInviteAccess;
	Access := TCloudAccessMapper.AccessToString(Access, true);
	ItemChangeAccess.Caption := Format(PREFIX_ACCESS_CHANGE, [Access]);
end;

procedure TPropertyForm.ItemChangeAccessClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.OnInviteChangeAccessClick;
end;

procedure TPropertyForm.RefreshLinksScanTbClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.RefreshDownloadLinks;
end;

procedure TPropertyForm.CancelLinksScanTbClick(Sender: TObject);
begin
	FDownloadLinksCancelled := true;
end;

procedure TPropertyForm.RefreshHashesScanTbClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.RefreshHashes;
end;

procedure TPropertyForm.CancelHashesScanTbClick(Sender: TObject);
begin
	FHashesCancelled := true;
end;

procedure TPropertyForm.ApplyHashesTBClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.ApplyHashCommands;
end;

procedure TPropertyForm.HashesMemoExit(Sender: TObject);
begin
	if Assigned(FPresenter) and FPresenter.CanApplyHashes then
		ApplyHashesTB.Enabled := HashesMemo.Text <> EmptyWideStr;
end;

procedure TPropertyForm.DescriptionSaveButtonClick(Sender: TObject);
begin
	if Assigned(FPresenter) then
		FPresenter.SaveDescription;
end;

procedure TPropertyForm.PublicLinkLabelClick(Sender: TObject);
begin
	if AccessCB.Checked then
		Clipboard.AsText := WebLink.Text;
end;

procedure TPropertyForm.WrapLinksTbClick(Sender: TObject);
begin
	if WrapLinksTb.Down then
		DownloadLinksMemo.ScrollBars := ssVertical
	else
		DownloadLinksMemo.ScrollBars := ssBoth;
	DownloadLinksMemo.WordWrap := WrapLinksTb.Down;
end;

procedure TPropertyForm.SaveLinksTbClick(Sender: TObject);
begin
	if SaveDialogSD.Execute(Handle) then
		DownloadLinksMemo.Lines.SaveToFile(SaveDialogSD.FileName);
end;

procedure TPropertyForm.WrapHashesTbClick(Sender: TObject);
begin
	if WrapHashesTb.Down then
		HashesMemo.ScrollBars := ssVertical
	else
		HashesMemo.ScrollBars := ssBoth;
	HashesMemo.WordWrap := WrapHashesTb.Down;
end;

procedure TPropertyForm.SaveHashesTbClick(Sender: TObject);
begin
	if SaveDialogSD.Execute(Handle) then
		HashesMemo.Lines.SaveToFile(SaveDialogSD.FileName);
end;

procedure TPropertyForm.LoadHashesTbClick(Sender: TObject);
begin
	if OpenDialogOD.Execute(Handle) then
		HashesMemo.Lines.LoadFromFile(OpenDialogOD.FileName);
end;

{TPropertyForm - Static factory method}

class function TPropertyForm.ShowProperty(parentWindow: HWND; RemoteName: WideString; RemoteProperty: TCloudDirItem; Cloud: TCloudMailRu; FileSystem: IFileSystem; TCHandler: ITCHandler; DoUrlEncode: Boolean; AutoUpdateDownloadListing: Boolean; ShowDescription: Boolean; EditDescription: Boolean; PluginIonFileName: WideString): Integer;
var
	Form: TPropertyForm;
	Config: TRemotePropertyConfig;
begin
	Form := TPropertyForm.Create(nil);
	try
		Form.parentWindow := parentWindow;

		{Create presenter with cloud services}
		Form.FPresenter := TRemotePropertyPresenter.Create(Form, Cloud.Downloader, Cloud.Uploader, Cloud.FileOps, Cloud.ListingService, Cloud.ShareService, FileSystem, TPublicCloudFactory.Create, TCHandler, Cloud.IsPublicAccount);

		{Configure and initialize presenter}
		Config.DoUrlEncode := DoUrlEncode;
		Config.AutoUpdateDownloadListing := AutoUpdateDownloadListing;
		Config.ShowDescription := ShowDescription;
		Config.EditDescription := EditDescription;
		Config.PluginIonFileName := PluginIonFileName;

		Form.FPresenter.Initialize(RemoteProperty, RemoteName, Config);

		Result := Form.ShowModal;
	finally
		Form.Free;
	end;
end;

end.
