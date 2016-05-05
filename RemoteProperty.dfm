object PropertyForm: TPropertyForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  ClientHeight = 87
  ClientWidth = 438
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    438
    87)
  PixelsPerInch = 96
  TextHeight = 13
  object PublicLinkLabel: TLabel
    Left = 8
    Top = 8
    Width = 131
    Height = 13
    Caption = 'Public link (Ctrl+C to copy):'
  end
  object WebLink: TEdit
    Left = 8
    Top = 27
    Width = 422
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
  end
  object AccessCB: TCheckBox
    Left = 8
    Top = 54
    Width = 131
    Height = 17
    Caption = 'Public access enabled'
    TabOrder = 1
    OnClick = AccessCBClick
  end
  object OkButton: TButton
    Left = 342
    Top = 54
    Width = 88
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
end
