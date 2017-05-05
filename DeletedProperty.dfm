object DeletedPropertyForm: TDeletedPropertyForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'DeletedPropertyForm'
  ClientHeight = 144
  ClientWidth = 522
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
    Left = 83
    Top = 8
    Width = 431
    Height = 13
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object DelFromLB: TLabel
    Left = 83
    Top = 27
    Width = 431
    Height = 13
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object DelAtLB: TLabel
    Left = 83
    Top = 46
    Width = 431
    Height = 13
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object DelByLB: TLabel
    Left = 83
    Top = 65
    Width = 431
    Height = 13
    AutoSize = False
    EllipsisPosition = epPathEllipsis
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
    Width = 69
    Height = 13
    Caption = 'Summary size:'
  end
  object DelSizeLB: TLabel
    Left = 83
    Top = 84
    Width = 431
    Height = 13
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object RestoreBTN: TButton
    Left = 8
    Top = 111
    Width = 75
    Height = 25
    Caption = 'Restore'
    ModalResult = 6
    TabOrder = 0
  end
  object CancelBTN: TButton
    Left = 439
    Top = 111
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object RestoreAllBTN: TButton
    Left = 89
    Top = 111
    Width = 75
    Height = 25
    Caption = 'Restore all'
    ModalResult = 14
    TabOrder = 2
  end
  object EmptyBTN: TButton
    Left = 170
    Top = 111
    Width = 75
    Height = 25
    Caption = 'Clear trash'
    ModalResult = 7
    TabOrder = 3
  end
end
