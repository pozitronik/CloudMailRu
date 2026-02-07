object AskEncryptionPasswordForm: TAskEncryptionPasswordForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  BorderIcons = []
  Caption = 'Encryption password'
  ClientHeight = 85
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  TextHeight = 13
  object PasswordEditLabel: TLabel
    Left = 7
    Top = 8
    Width = 420
    Height = 40
    AutoSize = False
    Caption = 'Enter encryption password for current session:'
    WordWrap = True
  end
  object PasswordEdit: TEdit
    Left = 7
    Top = 27
    Width = 420
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
    OnChange = PasswordEditChange
  end
  object OkButton: TButton
    Left = 352
    Top = 54
    Width = 75
    Height = 25
    Caption = 'OK'
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object SkipButton: TButton
    Left = 7
    Top = 54
    Width = 168
    Height = 25
    Caption = 'No encryption this time'
    ModalResult = 7
    TabOrder = 2
  end
end
