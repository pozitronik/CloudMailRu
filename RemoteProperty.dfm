object PropertyForm: TPropertyForm
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  ClientHeight = 101
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Visible = True
  DesignSize = (
    635
    101)
  PixelsPerInch = 96
  TextHeight = 13
  object PublicLinkLabel: TLabel
    Left = 8
    Top = 8
    Width = 131
    Height = 13
    Caption = 'Public link (Ctrl+C to copy):'
  end
  object PublicLinkEdit: TEdit
    Left = 8
    Top = 27
    Width = 619
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
  end
  object AccessLabel: TCheckBox
    Left = 8
    Top = 54
    Width = 131
    Height = 17
    Caption = 'Public access enabled'
    TabOrder = 1
  end
  object OkButton: TButton
    Left = 552
    Top = 64
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 2
  end
end
