unit RemoteProperty;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TPropertyForm = class(TForm)
    PublicLinkLabel: TLabel;
    PublicLinkEdit: TEdit;
    AccessLabel: TCheckBox;
    OkButton: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PropertyForm: TPropertyForm;

implementation

{$R *.dfm}

end.
