unit DeletedProperty;

interface

uses
	CMRDirItemList,
	CMRDirItem,
	CloudMailRu,
	CMRConstants,
	CMRStrings,
	PluginHelper,
	DateUtils,
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.StdCtrls;

type
	TDeletedPropertyForm = class(TForm)
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
		{Private declarations}
	public
		{Public declarations}
		class function ShowProperties(parentWindow: HWND; Items: TCMRDirItemList; TrashDir: boolean = false; AccountName: WideString = ''): integer;
	end;

implementation

{$R *.dfm}
{TDeletedPropertyForm}

class function TDeletedPropertyForm.ShowProperties(parentWindow: HWND; Items: TCMRDirItemList; TrashDir: boolean = false; AccountName: WideString = ''): integer;
var
	DeletedPropertyForm: TDeletedPropertyForm;
	FormCaption, NameCaption, FromCaption, AtCaption, ByCaption, SizeCaption: WideString;
	function summary_size(Items: TCMRDirItemList): integer;
	var
		Item: TCMRDirItem;
	begin
		result := 0;
		for Item in Items do
			result := result + Item.size;
	end;

begin
	try
		DeletedPropertyForm := TDeletedPropertyForm.Create(nil);
		DeletedPropertyForm.parentWindow := parentWindow;

		if Length(Items) = 0 then
		begin
			NameCaption := EMPTY;
			FormCaption := Format(ACCOUNT_TRASH, [AccountName]);
			DeletedPropertyForm.RestoreBTN.Enabled := false;
			DeletedPropertyForm.RestoreAllBTN.Enabled := false;
			DeletedPropertyForm.EmptyBTN.Enabled := false;
		end else if Length(Items) = 1 then
		begin
			NameCaption := Items[0].name;
			FromCaption := Items[0].deleted_from;

			AtCaption := DateTimeToStr(UnixToDateTime(Items[0].deleted_at));
			ByCaption := Items[0].deleted_by.ToString; //display user id as is, because no conversation api method performed
			SizeCaption := FormatSize(Items[0].size, TYPE_BYTES);
			FormCaption := Format(DELETED_ITEM, [NameCaption]);
			DeletedPropertyForm.RestoreAllBTN.Enabled := false;
		end else begin
			NameCaption := MULTIPLE_ITEMS;
			FromCaption := UNSET_ITEM;
			AtCaption := UNSET_ITEM;
			ByCaption := UNSET_ITEM;
			SizeCaption := FormatSize(summary_size(Items), TYPE_BYTES);
			FormCaption := MULTIPLE_ITEMS_DELETED;
		end;

		if TrashDir then //свойства для самой корзины, даём выбор Очистить/Восстановить все/Отмена
		begin
			FormCaption := AccountName + TrashPostfix;
			NameCaption := FormCaption;
			FromCaption := UNSET_ITEM;
			AtCaption := UNSET_ITEM;
			ByCaption := UNSET_ITEM;
			DeletedPropertyForm.RestoreBTN.Enabled := false;
			DeletedPropertyForm.RestoreAllBTN.Enabled := true;
		end else begin //свойства для пачки файлов, даём выбор Восстановить/Отмена

		end;

		DeletedPropertyForm.Caption := FormCaption;
		DeletedPropertyForm.DelNameLB.Caption := NameCaption;
		DeletedPropertyForm.DelFromLB.Caption := FromCaption;
		DeletedPropertyForm.DelAtLB.Caption := AtCaption;
		DeletedPropertyForm.DelByLB.Caption := ByCaption;
		DeletedPropertyForm.DelSizeLB.Caption := SizeCaption;
		result := DeletedPropertyForm.ShowModal;
	finally
		FreeAndNil(DeletedPropertyForm);
	end;
end;

end.
