unit AskPassword;

interface

uses
	Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
	Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
	TAskPasswordForm = class(TForm)
    PasswordEditLabel: TLabel;
    PasswordEdit: TEdit;
    OkButton: TButton;
    UseTCPwdMngrCB: TCheckBox;
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

var
	AskPasswordForm: TAskPasswordForm;

implementation

{$R *.dfm}

end.
