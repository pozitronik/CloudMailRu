object DeletedPropertyForm: TDeletedPropertyForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'DeletedPropertyForm'
  ClientHeight = 144
  ClientWidth = 645
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
  object DelNameLB: TLabel
    Left = 80
    Top = 8
    Width = 53
    Height = 13
    Caption = 'DelNameLB'
  end
  object DelFromLB: TLabel
    Left = 80
    Top = 27
    Width = 50
    Height = 13
    Caption = 'DelFromLB'
  end
  object DelAtLB: TLabel
    Left = 80
    Top = 46
    Width = 37
    Height = 13
    Caption = 'DelAtLB'
  end
  object DelByLB: TLabel
    Left = 80
    Top = 65
    Width = 38
    Height = 13
    Caption = 'DelByLB'
  end
  object DelSizeLB: TLabel
    Left = 80
    Top = 84
    Width = 45
    Height = 13
    Caption = 'DelSizeLB'
  end
  object NameLB: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object FromLB: TLabel
    Left = 8
    Top = 27
    Width = 66
    Height = 13
    Caption = 'Deleted from:'
  end
  object AtLB: TLabel
    Left = 8
    Top = 46
    Width = 54
    Height = 13
    Caption = 'Deleted at:'
  end
  object ByLB: TLabel
    Left = 8
    Top = 65
    Width = 52
    Height = 13
    Caption = 'Deleted by'
  end
  object SizeLB: TLabel
    Left = 8
    Top = 84
    Width = 61
    Height = 13
    Caption = 'Original size:'
  end
  object DeleteBTN: TButton
    Left = 8
    Top = 111
    Width = 75
    Height = 25
    Caption = 'Delete'
    ModalResult = 6
    TabOrder = 0
  end
  object RestoreBTN: TButton
    Left = 97
    Top = 111
    Width = 75
    Height = 25
    Caption = 'Restore'
    ModalResult = 7
    TabOrder = 1
  end
  object CancelBTN: TButton
    Left = 562
    Top = 111
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
