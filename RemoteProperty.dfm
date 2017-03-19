object PropertyForm: TPropertyForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 5
  ClientHeight = 358
  ClientWidth = 836
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
    836
    358)
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
    Left = 2
    Top = 19
    Width = 834
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
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
    Left = 745
    Top = 46
    Width = 91
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object ExtPropertiesPC: TPageControl
    Left = 0
    Top = 75
    Width = 836
    Height = 283
    ActivePage = FolderAccessTS
    TabOrder = 3
    object FolderAccessTS: TTabSheet
      Caption = 'Folder Access'
      DesignSize = (
        828
        255)
      object InviteEmailLabel: TLabel
        Left = 5
        Top = 16
        Width = 93
        Height = 13
        Caption = 'New member email:'
      end
      object AccessLabel: TLabel
        Left = 632
        Top = 16
        Width = 37
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Access:'
        ExplicitLeft = 218
      end
      object InviteEmailEdit: TEdit
        Left = 5
        Top = 35
        Width = 626
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object InviteAcessCB: TComboBox
        Left = 632
        Top = 35
        Width = 115
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemIndex = 0
        TabOrder = 1
        Text = 'Read and write'
        Items.Strings = (
          'Read and write'
          'Read only')
      end
      object InviteBtn: TButton
        Left = 753
        Top = 35
        Width = 75
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'Add'
        TabOrder = 2
        OnClick = InviteBtnClick
      end
      object InvitesLE: TValueListEditor
        Left = 0
        Top = 62
        Width = 828
        Height = 193
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
        TabOrder = 3
        TitleCaptions.Strings = (
          'Member'
          'Access')
        ColWidths = (
          269
          557)
        RowHeights = (
          18
          18)
      end
    end
    object DownloadLinksTS: TTabSheet
      Caption = 'Download Links'
      ImageIndex = 1
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
