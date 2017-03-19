object PropertyForm: TPropertyForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 5
  ClientHeight = 287
  ClientWidth = 485
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    485
    287)
  PixelsPerInch = 96
  TextHeight = 13
  object PublicLinkLabel: TLabel
    Left = 0
    Top = 0
    Width = 131
    Height = 13
    Caption = 'Public link (Ctrl+C to copy):'
  end
  object WebLink: TEdit
    Left = 0
    Top = 19
    Width = 485
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
    ExplicitWidth = 500
  end
  object AccessCB: TCheckBox
    Left = 0
    Top = 46
    Width = 131
    Height = 17
    Caption = 'Public access enabled'
    TabOrder = 1
    OnClick = AccessCBClick
  end
  object OkButton: TButton
    Left = 391
    Top = 46
    Width = 94
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
    ExplicitLeft = 406
  end
  object InvitesGB: TGroupBox
    Left = 0
    Top = 77
    Width = 485
    Height = 210
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Folder access'
    TabOrder = 3
    ExplicitWidth = 500
    DesignSize = (
      485
      210)
    object InviteEmailLabel: TLabel
      Left = 5
      Top = 16
      Width = 93
      Height = 13
      Caption = 'New member email:'
    end
    object AccessLabel: TLabel
      Left = 281
      Top = 16
      Width = 37
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Access:'
      ExplicitLeft = 218
    end
    object InvitesLE: TValueListEditor
      Left = 2
      Top = 62
      Width = 481
      Height = 146
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      BiDiMode = bdLeftToRight
      BorderStyle = bsNone
      Color = clBtnFace
      Ctl3D = True
      DrawingStyle = gdsClassic
      Options = [goRowSelect, goThumbTracking]
      ParentBiDiMode = False
      ParentCtl3D = False
      PopupMenu = InvitesPopup
      ScrollBars = ssVertical
      TabOrder = 0
      TitleCaptions.Strings = (
        'Member'
        'Access')
      ExplicitWidth = 418
      ColWidths = (
        269
        210)
      RowHeights = (
        18
        18)
    end
    object InviteEmailEdit: TEdit
      Left = 5
      Top = 35
      Width = 275
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      ExplicitWidth = 212
    end
    object InviteAcessCB: TComboBox
      Left = 281
      Top = 35
      Width = 115
      Height = 21
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemIndex = 0
      TabOrder = 2
      Text = 'Read and write'
      Items.Strings = (
        'Read and write'
        'Read only')
      ExplicitLeft = 218
    end
    object InviteBtn: TButton
      Left = 402
      Top = 35
      Width = 75
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Add'
      TabOrder = 3
      OnClick = InviteBtnClick
      ExplicitLeft = 339
    end
  end
  object InvitesPopup: TPopupMenu
    OnPopup = InvitesPopupPopup
    Left = 392
    Top = 221
    object ItemChangeAccess: TMenuItem
      Caption = 'Change access'
      OnClick = ItemChangeAccessClick
    end
    object ItemDelete: TMenuItem
      Caption = 'Drop access'
      OnClick = ItemDeleteClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object ItemRefresh: TMenuItem
      Caption = 'Refresh'
      OnClick = ItemRefreshClick
    end
  end
end
