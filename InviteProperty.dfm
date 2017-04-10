object InvitePropertyForm: TInvitePropertyForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'InvitePropertyForm'
  ClientHeight = 164
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object InviteNameLB: TLabel
    Left = 83
    Top = 8
    Width = 431
    Height = 13
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object InviteOwnerEmailLB: TLabel
    Left = 83
    Top = 27
    Width = 431
    Height = 13
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object InviteOwnerNameLB: TLabel
    Left = 83
    Top = 46
    Width = 431
    Height = 13
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object InviteAccessLB: TLabel
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
  object OwnerEmailLB: TLabel
    Left = 8
    Top = 27
    Width = 63
    Height = 13
    Caption = 'Owner email:'
  end
  object OwnerNameLB: TLabel
    Left = 8
    Top = 46
    Width = 65
    Height = 13
    Caption = 'Owner name:'
  end
  object AccessLB: TLabel
    Left = 8
    Top = 65
    Width = 37
    Height = 13
    Caption = 'Access:'
  end
  object SizeLB: TLabel
    Left = 8
    Top = 84
    Width = 69
    Height = 13
    Caption = 'Summary size:'
  end
  object InviteSizeLB: TLabel
    Left = 83
    Top = 84
    Width = 431
    Height = 13
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object TokenLB: TLabel
    Left = 8
    Top = 104
    Width = 62
    Height = 13
    Caption = 'Invite token:'
  end
  object InviteTokenLB: TLabel
    Left = 83
    Top = 104
    Width = 431
    Height = 13
    AutoSize = False
    EllipsisPosition = epPathEllipsis
  end
  object MountBTN: TButton
    Left = 8
    Top = 131
    Width = 75
    Height = 25
    Caption = 'Mount'
    ModalResult = 6
    TabOrder = 0
  end
  object CancelBTN: TButton
    Left = 439
    Top = 131
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object RejectBTN: TButton
    Left = 341
    Top = 131
    Width = 75
    Height = 25
    Caption = 'Reject'
    ModalResult = 7
    TabOrder = 2
  end
  object UnmountCopyBTN: TButton
    Left = 89
    Top = 131
    Width = 120
    Height = 25
    Caption = 'Unmount (save copy)'
    ModalResult = 3
    TabOrder = 3
  end
  object UnmountDeleteBTN: TButton
    Left = 215
    Top = 131
    Width = 120
    Height = 25
    Caption = 'Unmount and delete'
    ModalResult = 8
    TabOrder = 4
  end
end
