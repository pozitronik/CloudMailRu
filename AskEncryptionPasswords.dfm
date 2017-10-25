object AskEncryptionPasswordsForm: TAskEncryptionPasswordsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'File encryption'
  ClientHeight = 151
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object EncryptFilesLabel: TLabel
    Left = 7
    Top = 8
    Width = 176
    Height = 13
    Caption = 'Encrypt\decrypt files with password:'
  end
  object OkButton: TButton
    Left = 352
    Top = 120
    Width = 75
    Height = 25
    Caption = 'OK'
    Enabled = False
    ModalResult = 1
    TabOrder = 0
  end
  object SkipEncryprionButton: TButton
    Left = 7
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Skip'
    ModalResult = 5
    TabOrder = 1
  end
  object EncryptFilesPasswordEdit: TEdit
    Left = 7
    Top = 27
    Width = 420
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
    OnChange = EncryptFilesPasswordEditChange
  end
  object EncryptFilenamesPasswordEdit: TEdit
    Left = 7
    Top = 74
    Width = 420
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object EncryptFilenamesCB: TCheckBox
    Left = 7
    Top = 54
    Width = 226
    Height = 17
    Caption = 'Encrypt\decrypt filenames with password:'
    TabOrder = 4
  end
  object UseTCPwdMngrCB: TCheckBox
    Left = 7
    Top = 97
    Width = 218
    Height = 17
    Caption = 'Store passwords in TC password manager'
    TabOrder = 5
  end
end
