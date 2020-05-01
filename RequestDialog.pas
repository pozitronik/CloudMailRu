unit RequestDialog;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs, PLUGIN_Types, Vcl.StdCtrls, WideStrUtils;

type
	TRequestDialogForm = class(TForm)
		TextEdit: TEdit;
		TitleLabel: TLabel;
		CancelBtn: TButton;
		OkBtn: TButton;
	private

		{Private declarations}
	public
		{Public declarations}
		class function RequestHandle(RequestType: Integer; CustomTitle, CustomText, ReturnedText: pwidechar; maxlen: Integer; AOwner: TComponent = nil): Bool; stdcall; static;
	end;

var
	RequestDialogForm: TRequestDialogForm;

implementation

{$R *.dfm}
{TRequestDialogForm}

class function TRequestDialogForm.RequestHandle(RequestType: Integer; CustomTitle, CustomText, ReturnedText: pwidechar; maxlen: Integer; AOwner: TComponent = nil): Bool; stdcall;
var
	RequestDialog: TRequestDialogForm;
	OwnerForm: TCustomForm;
begin
	result := false;

	case RequestType of
		RT_Other:
			begin

			end;
		RT_UserName:
			begin

			end;
		RT_Password:
			begin
				try
					RequestDialog := TRequestDialogForm.Create(AOwner);
					if (nil <> AOwner) then
					begin
						OwnerForm := (AOwner as TCustomForm);
						RequestDialog.ParentWindow := OwnerForm.Handle;
					end;
					if nil = CustomTitle then
						RequestDialog.Caption := 'Total Commander'
					else
						RequestDialog.Caption := CustomTitle;
					RequestDialog.TitleLabel.Caption := CustomText;
					RequestDialog.TextEdit.Text := ReturnedText;
					result := RequestDialog.ShowModal in [MrOK, MrYES];
					WStrCopy(ReturnedText, pwidechar(RequestDialog.TextEdit.Text));
				finally
					FreeAndNil(RequestDialog);
				end;
			end;
		RT_Account:
			begin

			end;
		RT_UserNameFirewall:
			begin;
			end;
		RT_PasswordFirewall:
			begin

			end;
		RT_TargetDir:
			begin

			end;
		RT_URL:
			begin

			end;
		RT_MsgOK:
			begin;
			end;
		RT_MsgYesNo:
			begin

			end;
		RT_MsgOKCancel:
			begin

			end;
	end;
end;

end.
