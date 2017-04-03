unit DeletedProperty;

interface

uses
	CloudMailRu, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
	TDeletedPropertyForm = class(TForm)
		DelNameLB: TLabel;
		DelFromLB: TLabel;
		DelAtLB: TLabel;
		DelByLB: TLabel;
		DelSizeLB: TLabel;
		RestoreBTN: TButton;
		CancelBTN: TButton;
		NameLB: TLabel;
		FromLB: TLabel;
		AtLB: TLabel;
		ByLB: TLabel;
		SizeLB: TLabel;
	private
		{Private declarations}
	public
		{Public declarations}
		class function ShowProperties(parentWindow: HWND; Item: TCloudMailRuDirListingItem): integer;
	end;

implementation

{$R *.dfm}
{TDeletedPropertyForm}

class function TDeletedPropertyForm.ShowProperties(parentWindow: HWND; Item: TCloudMailRuDirListingItem): integer;
var
	DeletedPropertyForm: TDeletedPropertyForm;
begin
	try
		DeletedPropertyForm:=TDeletedPropertyForm.Create(nil);
		DeletedPropertyForm.parentWindow := parentWindow;
		DeletedPropertyForm.Caption := 'Deleted item property: ' + Item.name;
		DeletedPropertyForm.DelNameLB.Caption := Item.name;
		DeletedPropertyForm.DelFromLB.Caption := Item.deleted_from;
		DeletedPropertyForm.DelAtLB.Caption := Item.deleted_at.ToString; //todo from unixtimestamp ?
		DeletedPropertyForm.DelByLB.Caption := Item.deleted_by.ToString; //todo check api user to name
		DeletedPropertyForm.DelSizeLB.Caption := Item.size.ToString;
		result:=DeletedPropertyForm.ShowModal;
	finally
		FreeAndNil(DeletedPropertyForm);
	end;
end;

end.
