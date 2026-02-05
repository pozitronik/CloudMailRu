unit DeletedProperty;

interface

uses
	CloudDirItemList,
	CloudDirItem,
	DeletedPropertyPresenter,
	LanguageStrings,
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.StdCtrls,
	PluginForm;

type
	TDeletedPropertyForm = class(TPluginForm, IDeletedPropertyView)
		DelNameLB: TLabel;
		DelFromLB: TLabel;
		DelAtLB: TLabel;
		DelByLB: TLabel;
		RestoreBTN: TButton;
		CancelBTN: TButton;
		NameLB: TLabel;
		FromLB: TLabel;
		AtLB: TLabel;
		ByLB: TLabel;
		SizeLB: TLabel;
		RestoreAllBTN: TButton;
		EmptyBTN: TButton;
		DelSizeLB: TLabel;
	private
		FPresenter: TDeletedPropertyPresenter;
		procedure UpdateFormCaptions;

		{IDeletedPropertyView implementation}
		procedure SetCaption(Caption: WideString);
		procedure SetItemName(Name: WideString);
		procedure SetDeletedFrom(Path: WideString);
		procedure SetDeletedAt(DateTime: WideString);
		procedure SetDeletedBy(UserId: WideString);
		procedure SetSize(Size: WideString);
		procedure SetRestoreEnabled(Enabled: Boolean);
		procedure SetRestoreAllEnabled(Enabled: Boolean);
		procedure SetEmptyEnabled(Enabled: Boolean);
	public
		destructor Destroy; override;
		class function ShowProperties(parentWindow: HWND; Items: TCloudDirItemList; TrashDir: boolean = false; AccountName: WideString = ''): integer;
	end;

implementation

{$R *.dfm}

{TDeletedPropertyForm - IDeletedPropertyView implementation}

procedure TDeletedPropertyForm.SetCaption(Caption: WideString);
begin
	self.Caption := Caption;
end;

procedure TDeletedPropertyForm.SetItemName(Name: WideString);
begin
	DelNameLB.Caption := Name;
end;

procedure TDeletedPropertyForm.SetDeletedFrom(Path: WideString);
begin
	DelFromLB.Caption := Path;
end;

procedure TDeletedPropertyForm.SetDeletedAt(DateTime: WideString);
begin
	DelAtLB.Caption := DateTime;
end;

procedure TDeletedPropertyForm.SetDeletedBy(UserId: WideString);
begin
	DelByLB.Caption := UserId;
end;

procedure TDeletedPropertyForm.SetSize(Size: WideString);
begin
	DelSizeLB.Caption := Size;
end;

procedure TDeletedPropertyForm.SetRestoreEnabled(Enabled: Boolean);
begin
	RestoreBTN.Enabled := Enabled;
end;

procedure TDeletedPropertyForm.SetRestoreAllEnabled(Enabled: Boolean);
begin
	RestoreAllBTN.Enabled := Enabled;
end;

procedure TDeletedPropertyForm.SetEmptyEnabled(Enabled: Boolean);
begin
	EmptyBTN.Enabled := Enabled;
end;

procedure TDeletedPropertyForm.UpdateFormCaptions;
begin
	NameLB.Caption := DFM_DEL_LBL_NAME;
	FromLB.Caption := DFM_DEL_LBL_FROM;
	AtLB.Caption := DFM_DEL_LBL_AT;
	ByLB.Caption := DFM_DEL_LBL_BY;
	SizeLB.Caption := DFM_DEL_LBL_SIZE;
	RestoreBTN.Caption := DFM_DEL_BTN_RESTORE;
	CancelBTN.Caption := DFM_DEL_BTN_CANCEL;
	RestoreAllBTN.Caption := DFM_DEL_BTN_RESTORE_ALL;
	EmptyBTN.Caption := DFM_DEL_BTN_CLEAR_TRASH;
end;

{TDeletedPropertyForm}

destructor TDeletedPropertyForm.Destroy;
begin
	FreeAndNil(FPresenter);
	inherited;
end;

class function TDeletedPropertyForm.ShowProperties(parentWindow: HWND; Items: TCloudDirItemList; TrashDir: boolean; AccountName: WideString): integer;
var
	DeletedPropertyForm: TDeletedPropertyForm;
begin
	DeletedPropertyForm := TDeletedPropertyForm.Create(nil);
	try
		DeletedPropertyForm.parentWindow := parentWindow;
		DeletedPropertyForm.UpdateFormCaptions;
		DeletedPropertyForm.FPresenter := TDeletedPropertyPresenter.Create(DeletedPropertyForm);
		DeletedPropertyForm.FPresenter.Initialize(Items, TrashDir, AccountName);
		result := DeletedPropertyForm.ShowModal;
	finally
		FreeAndNil(DeletedPropertyForm);
	end;
end;

end.
