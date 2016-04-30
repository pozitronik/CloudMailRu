object AskPasswordForm: TAskPasswordForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Password'
  ClientHeight = 111
  ClientWidth = 431
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object PasswordEditLabel: TLabel
    Left = 8
    Top = 8
    Width = 120
    Height = 13
    Caption = 'Enter account password:'
  end
  object PasswordEdit: TEdit
    Left = 8
    Top = 27
    Width = 415
    Height = 21
    TabOrder = 0
    Text = 'PasswordEdit'
  end
  object OkButton: TButton
    Left = 348
    Top = 77
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
  end
  object UseTCPwdMngrCB: TCheckBox
    Left = 8
    Top = 54
    Width = 415
    Height = 17
    Caption = 'Use TC passwords storage'
    TabOrder = 2
  end
end
