object RequestDialogForm: TRequestDialogForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'RequestDialogForm'
  ClientHeight = 103
  ClientWidth = 461
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object TitleLabel: TLabel
    Left = 8
    Top = 8
    Width = 45
    Height = 13
    Caption = 'TitleLabel'
  end
  object TextEdit: TEdit
    Left = 8
    Top = 40
    Width = 445
    Height = 21
    PasswordChar = '*'
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 378
    Top = 67
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object OkBtn: TButton
    Left = 297
    Top = 67
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
end
