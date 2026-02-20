object AccountsForm: TAccountsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Cloud Accounts'
  ClientHeight = 549
  ClientWidth = 709
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poMainFormCenter
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  TextHeight = 13
  object OptionPages: TPageControl
    Left = 0
    Top = 0
    Width = 709
    Height = 549
    ActivePage = AccountsTab
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 703
    ExplicitHeight = 479
    object AccountsTab: TTabSheet
      Caption = 'Accounts'
      ImageIndex = 5
      DesignSize = (
        701
        521)
      object AccountNameLabel: TLabel
        Left = 272
        Top = 4
        Width = 68
        Height = 13
        AutoSize = False
        Caption = 'Account name'
      end
      object ServerLabel: TLabel
        Left = 272
        Top = 47
        Width = 32
        Height = 13
        Caption = 'Server'
      end
      object SharesPanel: TPanel
        Left = 272
        Top = 140
        Width = 425
        Height = 115
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 9
        ExplicitWidth = 419
        ExplicitHeight = 45
        DesignSize = (
          425
          115)
        object PublicUrlLabel: TLabel
          Left = 0
          Top = 0
          Width = 425
          Height = 13
          Align = alTop
          AutoSize = False
          Caption = 'Public url:'
          ExplicitLeft = 137
          ExplicitTop = 8
          ExplicitWidth = 474
        end
        object PublicUrlEdit: TEdit
          Left = 0
          Top = 17
          Width = 339
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = FieldChanged
          ExplicitWidth = 333
        end
        object TestShareButton: TButton
          Left = 345
          Top = 17
          Width = 80
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Test'
          Enabled = False
          TabOrder = 1
          OnClick = TestShareButtonClick
          ExplicitLeft = 339
        end
      end
      object AccountsPanel: TPanel
        Left = 272
        Top = 141
        Width = 425
        Height = 346
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        ShowCaption = False
        TabOrder = 7
        DesignSize = (
          425
          346)
        object EmailLabel: TLabel
          Left = 0
          Top = 39
          Width = 57
          Height = 13
          Caption = 'Email/Login:'
        end
        object PasswordLabel: TLabel
          Left = 0
          Top = 78
          Width = 72
          Height = 13
          Caption = 'App password:'
        end
        object AuthMethodLabel: TLabel
          Left = 0
          Top = 0
          Width = 113
          Height = 13
          Caption = 'Authentication method:'
        end
        object EmailEdit: TEdit
          Left = 0
          Top = 53
          Width = 425
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = FieldChanged
          ExplicitWidth = 419
        end
        object PasswordEdit: TEdit
          Left = 0
          Top = 95
          Width = 339
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          PasswordChar = '*'
          TabOrder = 1
          OnChange = FieldChanged
          ExplicitWidth = 333
        end
        object AuthMethodCombo: TComboBox
          Left = 0
          Top = 14
          Width = 425
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 6
          OnChange = AuthMethodComboChange
          Items.Strings = (
            'OAuth App Password'
            'VK ID Browser Login')
          ExplicitWidth = 419
        end
        object UseTCPwdMngrCB: TCheckBox
          Left = 0
          Top = 121
          Width = 339
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Store password in TC password manager'
          TabOrder = 2
          OnClick = FieldChanged
          ExplicitWidth = 333
        end
        object FileSizeGB: TGroupBox
          Left = 0
          Top = 151
          Width = 425
          Height = 68
          Align = alBottom
          Caption = 'File size'
          TabOrder = 4
          ExplicitTop = 144
          DesignSize = (
            425
            68)
          object CloudMaxFileSizeLabelBytes: TLabel
            Left = 393
            Top = 46
            Width = 27
            Height = 13
            Alignment = taRightJustify
            Anchors = [akLeft, akRight]
            Caption = 'bytes'
          end
          object UnlimitedFileSizeCB: TCheckBox
            Left = 8
            Top = 17
            Width = 412
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Ignore 2Gb limit (paid account)'
            TabOrder = 0
            OnClick = UnlimitedFileSizeCBClick
            ExplicitWidth = 406
          end
          object SplitLargeFilesCB: TCheckBox
            Left = 8
            Top = 42
            Width = 186
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Split files when '#8805' than'
            TabOrder = 1
            OnClick = SplitLargeFilesCBClick
            ExplicitWidth = 180
          end
          object CloudMaxFileSizeValue: TEdit
            Left = 183
            Top = 41
            Width = 201
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Enabled = False
            NumbersOnly = True
            TabOrder = 2
            OnChange = FieldChanged
            OnExit = CloudMaxFileSizeValueExit
            ExplicitWidth = 195
          end
        end
        object EncryptGB: TGroupBox
          Left = 0
          Top = 219
          Width = 425
          Height = 127
          Align = alBottom
          Caption = 'Encryption'
          TabOrder = 5
          ExplicitTop = 212
          DesignSize = (
            425
            127)
          object CryptPasswordStorageLabel: TLabel
            Left = 8
            Top = 40
            Width = 86
            Height = 13
            Caption = 'Password storage'
          end
          object CipherProfileLabel: TLabel
            Left = 8
            Top = 80
            Width = 94
            Height = 13
            Caption = 'Encryption backend'
          end
          object EncryptFilesCB: TCheckBox
            Left = 8
            Top = 17
            Width = 325
            Height = 17
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Enable file encryption'
            TabOrder = 0
            OnClick = EncryptFilesCBClick
            ExplicitWidth = 319
          end
          object EncryptFilesPwdButton: TButton
            Left = 340
            Top = 14
            Width = 80
            Height = 21
            Anchors = [akTop, akRight]
            Caption = 'Set password'
            Enabled = False
            TabOrder = 1
            OnClick = EncryptFilesPwdButtonClick
            ExplicitLeft = 334
          end
          object CryptPasswordStorageCombo: TComboBox
            Left = 8
            Top = 56
            Width = 412
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemIndex = 0
            TabOrder = 2
            Text = 'Don'#39't save (ask each session)'
            OnChange = CryptPasswordStorageComboChange
            Items.Strings = (
              'Don'#39't save (ask each session)'
              'TC Password Manager'
              'INI file (plaintext!)')
            ExplicitWidth = 406
          end
          object CipherProfileCombo: TComboBox
            Left = 8
            Top = 99
            Width = 412
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 3
            OnChange = CipherProfileComboChange
            ExplicitWidth = 406
          end
        end
        object TestAccountButton: TButton
          Left = 345
          Top = 95
          Width = 80
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Test'
          Enabled = False
          TabOrder = 3
          OnClick = TestAccountButtonClick
          ExplicitLeft = 339
        end
      end
      object AccountsListView: TListView
        Left = 4
        Top = 4
        Width = 262
        Height = 483
        Anchors = [akLeft, akTop, akBottom]
        Columns = <
          item
            Caption = 'Account'
            Width = 84
          end
          item
            Caption = 'Type'
            Width = 58
          end
          item
            Caption = 'Server'
            Width = 80
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnKeyUp = AccountsListViewKeyUp
        OnSelectItem = AccountsListViewSelectItem
        ExplicitHeight = 430
      end
      object AddButton: TButton
        Left = 4
        Top = 492
        Width = 125
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'New'
        TabOrder = 1
        OnClick = AddButtonClick
        ExplicitTop = 422
      end
      object DeleteButton: TButton
        Left = 141
        Top = 492
        Width = 125
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Delete'
        TabOrder = 2
        OnClick = DeleteButtonClick
        ExplicitTop = 422
      end
      object AccountNameEdit: TEdit
        Left = 272
        Top = 20
        Width = 425
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnChange = FieldChanged
        ExplicitWidth = 419
      end
      object AccountTypeGB: TGroupBox
        Left = 272
        Top = 93
        Width = 425
        Height = 40
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Account type'
        TabOrder = 6
        ExplicitWidth = 419
        object PrivateRB: TRadioButton
          Left = 8
          Top = 14
          Width = 115
          Height = 23
          Caption = 'Private'
          Checked = True
          TabOrder = 0
          TabStop = True
          OnClick = PrivateRBClick
        end
        object PublicRB: TRadioButton
          Left = 140
          Top = 17
          Width = 115
          Height = 17
          Caption = 'Public'
          TabOrder = 1
          OnClick = PublicRBClick
        end
      end
      object ServerCombo: TComboBox
        Left = 272
        Top = 66
        Width = 339
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnChange = ServerComboChange
        Items.Strings = (
          '(Default)')
        ExplicitWidth = 333
      end
      object ServersButton: TButton
        Left = 617
        Top = 66
        Width = 80
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'Configure'
        TabOrder = 5
        OnClick = ServersButtonClick
        ExplicitLeft = 611
      end
      object ApplyButton: TButton
        Left = 573
        Top = 492
        Width = 124
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Apply'
        Enabled = False
        TabOrder = 8
        OnClick = ApplyButtonClick
        ExplicitLeft = 567
        ExplicitTop = 422
      end
    end
    object GlobalTab: TTabSheet
      Caption = 'Global settings'
      ImageIndex = 1
      DesignSize = (
        701
        521)
      object ChunkOverwriteModeLabel: TLabel
        Left = 4
        Top = 87
        Width = 150
        Height = 13
        Caption = 'Overwrite splitted chunk mode:'
      end
      object DeleteFailOnUploadModeLabel: TLabel
        Left = 4
        Top = 113
        Width = 143
        Height = 13
        Caption = 'Delete file after upload mode:'
      end
      object OverwriteLocalModeLabel: TLabel
        Left = 4
        Top = 139
        Width = 127
        Height = 13
        Caption = 'Overwrite local files mode:'
      end
      object IconsModeLabel: TLabel
        Left = 4
        Top = 165
        Width = 88
        Height = 13
        Caption = 'Plugin icons mode:'
      end
      object OperationErrorModeLabel: TLabel
        Left = 4
        Top = 194
        Width = 140
        Height = 13
        Caption = 'On downloads/uploads error:'
      end
      object RetryAttemptsLabel: TLabel
        Left = 406
        Top = 194
        Width = 77
        Height = 13
        Hint = 
          'Number of retry attempts on error (-1 for infinite, 0 to disable' +
          ')'
        Alignment = taRightJustify
        Caption = 'Retry attempts:'
        ParentShowHint = False
        ShowHint = True
      end
      object RetryWaitLabel: TLabel
        Left = 564
        Top = 194
        Width = 20
        Height = 13
        Hint = 'Delay between retry attempts in milliseconds'
        Alignment = taRightJustify
        Caption = 'wait'
        ParentShowHint = False
        ShowHint = True
      end
      object msLabel: TLabel
        Left = 676
        Top = 194
        Width = 13
        Height = 13
        Hint = 'Delay between retry attempts in milliseconds'
        Caption = 'ms'
        ParentShowHint = False
        ShowHint = True
      end
      object CopyBetweenAccountsModeLabel: TLabel
        Left = 4
        Top = 62
        Width = 172
        Height = 13
        Hint = 'How to transfer files between different cloud accounts'
        Caption = 'Copying/moving between accounts:'
        ParentShowHint = False
        ShowHint = True
      end
      object FileHistoryCB: TCheckBox
        Left = 4
        Top = 317
        Width = 697
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Show file version history'
        TabOrder = 14
        OnClick = GlobalSettingsFieldChanged
      end
      object GlobalSettingsApplyBtn: TButton
        Left = 573
        Top = 492
        Width = 124
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Apply'
        Enabled = False
        TabOrder = 13
        OnClick = GlobalSettingsApplyBtnClick
        ExplicitTop = 476
      end
      object ChunkOverwriteModeCombo: TComboBox
        Left = 237
        Top = 84
        Width = 464
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 2
        Text = 'Silently overwrite'
        OnChange = GlobalSettingsFieldChanged
        Items.Strings = (
          'Silently overwrite'
          'Ignore'
          'Abort operation')
      end
      object DeleteFailOnUploadModeCombo: TComboBox
        Left = 237
        Top = 110
        Width = 464
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 3
        Text = 'Ask user'
        OnChange = GlobalSettingsFieldChanged
        Items.Strings = (
          'Ask user'
          'Ignore file'
          'Abort operation'
          'Try to unset read only flag and delete, ignore file on error'
          'Try to unset read only flag and delete, abort operation on error')
      end
      object OverwriteLocalModeCombo: TComboBox
        Left = 237
        Top = 136
        Width = 464
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 4
        Text = 'Ask user'
        OnChange = GlobalSettingsFieldChanged
        Items.Strings = (
          'Ask user'
          'Ignore file'
          'Silently overwrite')
      end
      object DisableMultiThreadingCB: TCheckBox
        Left = 4
        Top = 270
        Width = 697
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Disable background operations support'
        TabOrder = 12
        OnClick = GlobalSettingsFieldChanged
      end
      object IconsModeCombo: TComboBox
        Left = 237
        Top = 162
        Width = 464
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 5
        Text = 'System default'
        OnChange = GlobalSettingsFieldChanged
        Items.Strings = (
          'System default'
          'Internal icons'
          'Internal icons overlay'
          'External icons'
          'External icons overlay')
      end
      object SpaceInfoLoggingCB: TCheckBox
        Left = 4
        Top = 246
        Width = 697
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Enable space info logging'
        TabOrder = 11
        OnClick = GlobalSettingsFieldChanged
      end
      object OperationErrorModeCombo: TComboBox
        Left = 237
        Top = 191
        Width = 162
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 6
        Text = 'Ask user'
        OnChange = GlobalSettingsFieldChanged
        Items.Strings = (
          'Ask user'
          'Ignore file'
          'Abort operation'
          'Retry with this file')
      end
      object AttemptWaitValue: TSpinEdit
        Left = 590
        Top = 191
        Width = 80
        Height = 22
        MaxValue = 2147483647
        MinValue = 0
        TabOrder = 8
        Value = 0
        OnChange = GlobalSettingsFieldChanged
      end
      object RetryAttemptsValue: TSpinEdit
        Left = 489
        Top = 191
        Width = 40
        Height = 22
        MaxValue = 2147483647
        MinValue = -1
        TabOrder = 7
        Value = 0
        OnChange = GlobalSettingsFieldChanged
      end
      object PrecalculateHashCB: TCheckBox
        Left = 4
        Top = 222
        Width = 226
        Height = 17
        Caption = 'Try to find files by hash before uploading'
        TabOrder = 9
        OnClick = GlobalSettingsFieldChanged
      end
      object CheckCRCCB: TCheckBox
        Left = 4
        Top = 294
        Width = 697
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Check uploads/downloads CRC'
        TabOrder = 15
        OnClick = GlobalSettingsFieldChanged
      end
      object CopyBetweenAccountsModeCombo: TComboBox
        Left = 237
        Top = 57
        Width = 464
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 1
        Text = 'Disabled'
        OnChange = GlobalSettingsFieldChanged
        Items.Strings = (
          'Disabled'
          'Via hash (recommended)'
          'Via public link')
      end
      object PrecalculateHashStrategyCombo: TComboBox
        Left = 238
        Top = 220
        Width = 463
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 10
        OnChange = GlobalSettingsFieldChanged
        Items.Strings = (
          'Autoselect (BCrypt > OpenSSL > Delphi)'
          'Delphi implementation'
          'BCrypt (Windows BCrypt/CNG API)'
          'OpenSSL (OpenSSL EVP)')
      end
      object ShowAccountsGB: TGroupBox
        Left = 0
        Top = 0
        Width = 701
        Height = 51
        Align = alTop
        Caption = 'Show accounts:'
        Padding.Left = 3
        Padding.Bottom = 3
        TabOrder = 0
        object ShowInvitesFoldersCB: TCheckBox
          Left = 5
          Top = 15
          Width = 180
          Height = 31
          Align = alLeft
          Caption = 'Invites folders'
          TabOrder = 0
          OnClick = GlobalSettingsFieldChanged
        end
        object ShowSharedFoldersCB: TCheckBox
          Left = 185
          Top = 15
          Width = 180
          Height = 31
          Align = alLeft
          Caption = 'Shared links folders'
          TabOrder = 1
          OnClick = GlobalSettingsFieldChanged
        end
        object ShowTrashFoldersCB: TCheckBox
          Left = 365
          Top = 15
          Width = 180
          Height = 31
          Align = alLeft
          Caption = 'Trash folders'
          TabOrder = 2
          OnClick = GlobalSettingsFieldChanged
        end
      end
    end
    object NetworkTab: TTabSheet
      Caption = 'Network settings'
      ImageIndex = 2
      DesignSize = (
        701
        521)
      object ProxyGB: TGroupBox
        Left = 0
        Top = 67
        Width = 701
        Height = 156
        Align = alTop
        Caption = 'Proxy settings'
        TabOrder = 1
        DesignSize = (
          701
          156)
        object ProxyTypeLabel: TLabel
          Left = 4
          Top = 18
          Width = 53
          Height = 13
          Caption = 'Proxy type'
        end
        object ProxyPortLabel: TLabel
          Left = 598
          Top = 47
          Width = 20
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = 'Port'
          ExplicitLeft = 584
        end
        object ProxyUserLabel: TLabel
          Left = 4
          Top = 76
          Width = 78
          Height = 13
          Caption = 'Proxy username'
        end
        object ProxyPWDLabel: TLabel
          Left = 4
          Top = 105
          Width = 46
          Height = 13
          Caption = 'Password'
        end
        object ProxyServerLabel: TLabel
          Left = 4
          Top = 47
          Width = 62
          Height = 13
          Caption = 'Proxy server'
        end
        object ProxyCB: TComboBox
          Left = 132
          Top = 14
          Width = 564
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = ProxyCBChange
          Items.Strings = (
            'No proxy'
            'Socks5'
            'Socks4'
            'HTTP(S)')
        end
        object ProxyServerEdit: TEdit
          Left = 132
          Top = 43
          Width = 456
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = GlobalSettingsFieldChanged
        end
        object ProxyPortEdit: TEdit
          Left = 624
          Top = 43
          Width = 72
          Height = 21
          Anchors = [akTop, akRight]
          NumbersOnly = True
          TabOrder = 2
          OnChange = GlobalSettingsFieldChanged
        end
        object ProxyUserEdit: TEdit
          Left = 132
          Top = 72
          Width = 564
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
          OnChange = ProxyUserEditChange
        end
        object ProxyPwd: TMaskEdit
          Left = 132
          Top = 101
          Width = 564
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          PasswordChar = '*'
          TabOrder = 4
          Text = ''
          OnChange = GlobalSettingsFieldChanged
        end
        object ProxyTCPwdMngrCB: TCheckBox
          Left = 4
          Top = 129
          Width = 299
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Store proxy password in TC password manager'
          TabOrder = 5
          OnClick = GlobalSettingsFieldChanged
        end
      end
      object NetworkSettingsApplyBtn: TButton
        Left = 573
        Top = 492
        Width = 124
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Apply'
        Enabled = False
        TabOrder = 6
        OnClick = GlobalSettingsApplyBtnClick
        ExplicitTop = 476
      end
      object SpeedLimitGB: TGroupBox
        Left = 0
        Top = 223
        Width = 701
        Height = 106
        Align = alTop
        Caption = 'Limits and timeouts'
        TabOrder = 2
        DesignSize = (
          701
          106)
        object UploadsBPSLabel: TLabel
          Left = 4
          Top = 22
          Width = 147
          Height = 13
          Hint = 'Use 0 for unlimited upload speed'
          Caption = 'Limit uploads BPS (0 for none):'
          ParentShowHint = False
          ShowHint = True
        end
        object DownloadsBPSLabel: TLabel
          Left = 4
          Top = 51
          Width = 161
          Height = 13
          Hint = 'Use 0 for unlimited download speed'
          Caption = 'Limit downloads BPS (0 for none):'
          ParentShowHint = False
          ShowHint = True
        end
        object SocketTimeoutLabel: TLabel
          Left = 4
          Top = 79
          Width = 230
          Height = 13
          Hint = 'Default: 30000ms (30 seconds). Set 0 for unlimited'
          Caption = 'Network operations timeout, ms (0 = unlimited):'
          ParentShowHint = False
          ShowHint = True
        end
        object UploadBPSEdit: TSpinEdit
          Left = 292
          Top = 18
          Width = 404
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = GlobalSettingsFieldChanged
        end
        object DownloadBPSEdit: TSpinEdit
          Left = 292
          Top = 47
          Width = 404
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = GlobalSettingsFieldChanged
        end
        object SocketTimeoutEdit: TSpinEdit
          Left = 292
          Top = 75
          Width = 404
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 30000
          OnChange = GlobalSettingsFieldChanged
        end
      end
      object UserAgentEdit: TEdit
        Left = 0
        Top = 368
        Width = 701
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
        OnChange = GlobalSettingsFieldChanged
      end
      object ChangeUserAgentCB: TCheckBox
        Left = 0
        Top = 345
        Width = 289
        Height = 17
        Caption = 'Change plugin UserAgent'
        TabOrder = 3
        OnClick = ChangeUserAgentCBClick
      end
      object ResetUserAgentButton: TButton
        Left = 621
        Top = 343
        Width = 80
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'Reset'
        TabOrder = 4
        OnClick = ResetUserAgentButtonClick
      end
      object SSLParametersGroupbox: TGroupBox
        Left = 0
        Top = 0
        Width = 701
        Height = 67
        Align = alTop
        Caption = 'SSL/TLS'
        TabOrder = 0
        DesignSize = (
          701
          67)
        object SSLBackendLabel: TLabel
          Left = 4
          Top = 17
          Width = 60
          Height = 13
          Caption = 'SSL Backend'
        end
        object SSLBackendCB: TComboBox
          Left = 87
          Top = 14
          Width = 609
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = GlobalSettingsFieldChanged
        end
        object UseDLLFromPluginDir: TCheckBox
          Left = 4
          Top = 40
          Width = 692
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Load SSL libraries only from plugin directory'
          TabOrder = 1
          OnClick = GlobalSettingsFieldChanged
        end
      end
    end
    object CommentsTab: TTabSheet
      Caption = 'Metadata'
      ImageIndex = 3
      DesignSize = (
        701
        521)
      object CommentsSettingsApplyBtn: TButton
        Left = 573
        Top = 492
        Width = 124
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Apply'
        Enabled = False
        TabOrder = 0
        OnClick = GlobalSettingsApplyBtnClick
        ExplicitTop = 476
      end
      object FileCommentsCB: TGroupBox
        Left = 0
        Top = 0
        Width = 701
        Height = 214
        Align = alTop
        Caption = 'File comments'
        TabOrder = 1
        DesignSize = (
          701
          214)
        object DescriptionFileNameLabel: TLabel
          Left = 4
          Top = 183
          Width = 151
          Height = 13
          Hint = 'Leave empty to use default descript.ion'
          Caption = 'Override descript.ion file name:'
          ParentShowHint = False
          ShowHint = True
        end
        object DescriptionEnabledCB: TCheckBox
          Left = 4
          Top = 17
          Width = 693
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Read comments from cloud filesystem'
          TabOrder = 0
          OnClick = GlobalSettingsFieldChanged
        end
        object DescriptionEditorEnabledCB: TCheckBox
          Left = 4
          Top = 40
          Width = 693
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable comments editor'
          TabOrder = 1
          OnClick = GlobalSettingsFieldChanged
        end
        object DescriptionCopyToCloudCB: TCheckBox
          Left = 4
          Top = 63
          Width = 693
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Copy comments to cloud'
          TabOrder = 2
          OnClick = GlobalSettingsFieldChanged
        end
        object DescriptionCopyFromCloudCB: TCheckBox
          Left = 4
          Top = 86
          Width = 693
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Copy comments from cloud'
          TabOrder = 3
          OnClick = GlobalSettingsFieldChanged
        end
        object DescriptionTrackCloudFSCB: TCheckBox
          Left = 4
          Top = 109
          Width = 693
          Height = 17
          Hint = 'Updates comments when files are renamed or deleted in cloud'
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Track cloud filesystem changes'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          OnClick = GlobalSettingsFieldChanged
        end
        object HideDescriptionFileCB: TCheckBox
          Left = 4
          Top = 132
          Width = 693
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Hide comments file in directory listing'
          TabOrder = 5
          OnClick = GlobalSettingsFieldChanged
        end
        object SkipDescriptionDownloadCB: TCheckBox
          Left = 4
          Top = 155
          Width = 693
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Skip comments file when downloading'
          TabOrder = 6
          OnClick = GlobalSettingsFieldChanged
        end
        object DescriptionFileNameEdit: TEdit
          Left = 251
          Top = 180
          Width = 446
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 7
          OnChange = GlobalSettingsFieldChanged
        end
      end
      object FileTimestampsCB: TGroupBox
        Left = 0
        Top = 214
        Width = 701
        Height = 156
        Align = alTop
        Caption = 'File timestamps'
        TabOrder = 2
        DesignSize = (
          701
          156)
        object TimestampModeLabel: TLabel
          Left = 4
          Top = 20
          Width = 84
          Height = 13
          Caption = 'Timestamp mode:'
        end
        object TimestampConflictModeLabel: TLabel
          Left = 4
          Top = 52
          Width = 121
          Height = 13
          Hint = 'How to handle conflicting timestamps on download'
          Caption = 'Timestamp conflict mode:'
          ParentShowHint = False
          ShowHint = True
        end
        object TimestampFileNameLabel: TLabel
          Left = 4
          Top = 128
          Width = 193
          Height = 13
          Hint = 'Leave empty to use default .cloud_timestamps'
          Caption = 'Override timestamp metadata file name:'
          ParentShowHint = False
          ShowHint = True
        end
        object TimestampModeCB: TComboBox
          Left = 251
          Top = 17
          Width = 446
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = TimestampModeCBChange
          Items.Strings = (
            'Disabled'
            'Cloud time'
            'Full sync')
        end
        object TimestampConflictModeCB: TComboBox
          Left = 251
          Top = 49
          Width = 446
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = GlobalSettingsFieldChanged
          Items.Strings = (
            'Use stored local time'
            'Use server time on conflict')
        end
        object HideTimestampFileCB: TCheckBox
          Left = 4
          Top = 79
          Width = 693
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Hide timestamp file in directory listing'
          TabOrder = 2
          OnClick = GlobalSettingsFieldChanged
        end
        object SkipTimestampDownloadCB: TCheckBox
          Left = 4
          Top = 102
          Width = 693
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Skip timestamp file when downloading'
          TabOrder = 3
          OnClick = GlobalSettingsFieldChanged
        end
        object TimestampFileNameEdit: TEdit
          Left = 251
          Top = 125
          Width = 446
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
          OnChange = GlobalSettingsFieldChanged
        end
      end
    end
    object StreamingTab: TTabSheet
      Caption = 'Streaming settings'
      ImageIndex = 4
      DesignSize = (
        701
        521)
      object ExtLabel: TLabel
        Left = 272
        Top = 4
        Width = 411
        Height = 13
        AutoSize = False
        Caption = 'File extension'
      end
      object CommandLabel: TLabel
        Left = 272
        Top = 46
        Width = 411
        Height = 13
        AutoSize = False
        Caption = 'Command'
      end
      object ParametersLabel: TLabel
        Left = 272
        Top = 88
        Width = 411
        Height = 13
        AutoSize = False
        Caption = 'Parameters (%url% for stream url substitution)'
      end
      object StartPathLabel: TLabel
        Left = 272
        Top = 129
        Width = 411
        Height = 13
        AutoSize = False
        Caption = 'Start path'
      end
      object StreamingTypeLabel: TLabel
        Left = 272
        Top = 172
        Width = 411
        Height = 13
        AutoSize = False
        Caption = 'Streaming type'
      end
      object StreamingExtensionEdit: TEdit
        Left = 272
        Top = 20
        Width = 425
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnChange = StreamingFieldChanged
      end
      object CommandPathEdit: TEdit
        Left = 272
        Top = 61
        Width = 397
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnChange = StreamingFieldChanged
      end
      object CommandPathButton: TButton
        Left = 675
        Top = 61
        Width = 22
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 5
        OnClick = CommandPathButtonClick
      end
      object ParametersEdit: TEdit
        Left = 272
        Top = 105
        Width = 425
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 6
        OnChange = StreamingFieldChanged
      end
      object StartPathEdit: TEdit
        Left = 272
        Top = 146
        Width = 425
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 7
        OnChange = StreamingFieldChanged
      end
      object StreamingTypeCombo: TComboBox
        Left = 272
        Top = 190
        Width = 425
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 8
        Text = 'None (download and open file)'
        OnChange = StreamingFieldChanged
        Items.Strings = (
          'None (download and open file)'
          'Disabled (no action)'
          'M3U8 playlist (useful for media files)'
          'Default (publish file & get download link)'
          'Weblink (publish file & get web link)')
      end
      object ApplyExtButton: TButton
        Left = 573
        Top = 492
        Width = 124
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Apply'
        Enabled = False
        TabOrder = 9
        OnClick = ApplyExtButtonClick
        ExplicitTop = 476
      end
      object NewExtButton: TButton
        Left = 4
        Top = 492
        Width = 125
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'New'
        TabOrder = 1
        OnClick = NewExtButtonClick
        ExplicitTop = 476
      end
      object DeleteExtButton: TButton
        Left = 141
        Top = 492
        Width = 125
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Delete'
        TabOrder = 2
        OnClick = DeleteExtButtonClick
        ExplicitTop = 476
      end
      object StreamingExtensionsListView: TListView
        Left = 4
        Top = 4
        Width = 262
        Height = 483
        Anchors = [akLeft, akTop, akBottom]
        Columns = <
          item
            Caption = 'Extension'
            Width = 120
          end
          item
            Caption = 'Type'
            Width = 58
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnKeyUp = StreamingExtensionsListViewKeyUp
        OnSelectItem = StreamingExtensionsListViewSelectItem
        ExplicitHeight = 467
      end
    end
    object CacheTab: TTabSheet
      Caption = 'Cache'
      ImageIndex = 5
      DesignSize = (
        701
        521)
      object CacheSettingsApplyBtn: TButton
        Left = 573
        Top = 492
        Width = 124
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Apply'
        Enabled = False
        TabOrder = 0
        OnClick = GlobalSettingsApplyBtnClick
        ExplicitTop = 476
      end
      object CacheDirGB: TGroupBox
        Left = 0
        Top = 0
        Width = 701
        Height = 85
        Align = alTop
        Caption = 'Cache directory'
        TabOrder = 1
        DesignSize = (
          701
          85)
        object CacheDirLabel: TLabel
          Left = 4
          Top = 20
          Width = 48
          Height = 13
          Caption = 'Directory:'
        end
        object ListingCacheSizeLabel: TLabel
          Left = 4
          Top = 42
          Width = 692
          Height = 13
          AutoSize = False
          Caption = 'Listing cache: -'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object FileCacheSizeLabel: TLabel
          Left = 4
          Top = 64
          Width = 692
          Height = 13
          AutoSize = False
          Caption = 'File cache: -'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object CacheDirEdit: TEdit
          Left = 116
          Top = 17
          Width = 494
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = CacheDirEditChange
        end
        object BrowseCacheDirButton: TButton
          Left = 616
          Top = 17
          Width = 80
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Browse...'
          TabOrder = 1
          OnClick = BrowseCacheDirButtonClick
        end
      end
      object CacheSettingsGB: TGroupBox
        Left = 0
        Top = 85
        Width = 701
        Height = 105
        Align = alTop
        Caption = 'Directory listing cache'
        TabOrder = 2
        DesignSize = (
          701
          105)
        object CacheTTLLabel: TLabel
          Left = 4
          Top = 45
          Width = 133
          Height = 13
          Caption = 'Cache entry TTL (seconds):'
        end
        object CacheMaxSizeLabel: TLabel
          Left = 4
          Top = 75
          Width = 125
          Height = 13
          Caption = 'Maximum cache size (MB):'
        end
        object CacheStatusLabel: TLabel
          Left = 160
          Top = 107
          Width = 3
          Height = 13
          Visible = False
        end
        object CacheEnabledCB: TCheckBox
          Left = 4
          Top = 17
          Width = 692
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable directory listing cache'
          TabOrder = 0
          OnClick = GlobalSettingsFieldChanged
        end
        object CacheTTLEdit: TSpinEdit
          Left = 290
          Top = 42
          Width = 90
          Height = 22
          MaxValue = 86400
          MinValue = 1
          TabOrder = 1
          Value = 60
          OnChange = GlobalSettingsFieldChanged
        end
        object CacheMaxSizeEdit: TSpinEdit
          Left = 290
          Top = 72
          Width = 90
          Height = 22
          MaxValue = 10240
          MinValue = 1
          TabOrder = 2
          Value = 50
          OnChange = GlobalSettingsFieldChanged
        end
        object ClearCacheButton: TButton
          Left = 546
          Top = 72
          Width = 150
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Clear cache now'
          TabOrder = 3
          OnClick = ClearCacheButtonClick
        end
      end
      object FileCacheSettingsGB: TGroupBox
        Left = 0
        Top = 190
        Width = 701
        Height = 105
        Align = alTop
        Caption = 'File content cache'
        TabOrder = 3
        DesignSize = (
          701
          105)
        object FileCacheTTLLabel: TLabel
          Left = 4
          Top = 45
          Width = 133
          Height = 13
          Caption = 'Cache entry TTL (seconds):'
        end
        object FileCacheMaxSizeLabel: TLabel
          Left = 4
          Top = 75
          Width = 142
          Height = 13
          Caption = 'Maximum file cache size (MB):'
        end
        object FileCacheStatusLabel: TLabel
          Left = 160
          Top = 107
          Width = 3
          Height = 13
          Visible = False
        end
        object FileCacheEnabledCB: TCheckBox
          Left = 4
          Top = 17
          Width = 693
          Height = 17
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Enable file content cache'
          TabOrder = 0
          OnClick = GlobalSettingsFieldChanged
        end
        object FileCacheTTLEdit: TSpinEdit
          Left = 290
          Top = 42
          Width = 90
          Height = 22
          MaxValue = 86400
          MinValue = 1
          TabOrder = 1
          Value = 3600
          OnChange = GlobalSettingsFieldChanged
        end
        object FileCacheMaxSizeEdit: TSpinEdit
          Left = 290
          Top = 72
          Width = 90
          Height = 22
          MaxValue = 102400
          MinValue = 1
          TabOrder = 2
          Value = 500
          OnChange = GlobalSettingsFieldChanged
        end
        object ClearFileCacheButton: TButton
          Left = 546
          Top = 72
          Width = 150
          Height = 21
          Anchors = [akTop, akRight]
          Caption = 'Clear file cache'
          TabOrder = 3
          OnClick = ClearFileCacheButtonClick
        end
      end
    end
    object ServersTab: TTabSheet
      Caption = 'Servers'
      ImageIndex = 6
      DesignSize = (
        701
        521)
      object ServerNameLabel: TLabel
        Left = 272
        Top = 4
        Width = 61
        Height = 13
        Caption = 'Server name'
      end
      object ServerUrlLabel: TLabel
        Left = 272
        Top = 42
        Width = 165
        Height = 13
        Caption = 'Server URL (http[s]://server:port)'
      end
      object ServerStatusLabel: TLabel
        Left = 462
        Top = 117
        Width = 221
        Height = 13
        Alignment = taRightJustify
        Anchors = [akLeft, akBottom]
        AutoSize = False
        ExplicitTop = 42
      end
      object ServersListView: TListView
        Left = 4
        Top = 4
        Width = 262
        Height = 483
        Anchors = [akLeft, akTop, akBottom]
        Columns = <
          item
            Caption = 'Name'
            Width = 120
          end
          item
            Caption = 'URL'
            Width = 120
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnKeyUp = ServersListViewKeyUp
        OnSelectItem = ServersListViewSelectItem
        ExplicitHeight = 467
      end
      object ServerNameEdit: TEdit
        Left = 272
        Top = 20
        Width = 425
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        OnChange = ServerFieldChanged
      end
      object ServerUrlEdit: TEdit
        Left = 272
        Top = 58
        Width = 339
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        OnChange = ServerFieldChanged
      end
      object TestServerButton: TButton
        Left = 617
        Top = 58
        Width = 80
        Height = 21
        Anchors = [akTop, akRight]
        Caption = 'Test'
        TabOrder = 5
        OnClick = TestServerButtonClick
      end
      object AddServerButton: TButton
        Left = 4
        Top = 492
        Width = 125
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'New'
        TabOrder = 1
        OnClick = AddServerButtonClick
        ExplicitTop = 476
      end
      object DeleteServerButton: TButton
        Left = 141
        Top = 492
        Width = 125
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Delete'
        TabOrder = 2
        OnClick = DeleteServerButtonClick
        ExplicitTop = 476
      end
      object ApplyServerButton: TButton
        Left = 573
        Top = 492
        Width = 124
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Apply'
        Enabled = False
        TabOrder = 6
        OnClick = ApplyServerButtonClick
        ExplicitTop = 476
      end
      object ServerParametersGB: TGroupBox
        Left = 272
        Top = 85
        Width = 425
        Height = 327
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Server parameters'
        TabOrder = 7
        DesignSize = (
          425
          327)
        object ApiUrlLabel: TLabel
          Left = 8
          Top = 17
          Width = 395
          Height = 13
          AutoSize = False
          Caption = 'API URL'
        end
        object OAuthUrlLabel: TLabel
          Left = 8
          Top = 58
          Width = 395
          Height = 13
          AutoSize = False
          Caption = 'OAuth URL'
        end
        object DispatcherUrlLabel: TLabel
          Left = 8
          Top = 103
          Width = 395
          Height = 13
          AutoSize = False
          Caption = 'Dispatcher URL'
        end
        object ThumbnailUrlLabel: TLabel
          Left = 8
          Top = 143
          Width = 395
          Height = 13
          AutoSize = False
          Caption = 'Thumbnail URL'
        end
        object ServerPublicUrlLabel: TLabel
          Left = 8
          Top = 183
          Width = 395
          Height = 13
          AutoSize = False
          Caption = 'Public URL'
        end
        object DownloadUrlLabel: TLabel
          Left = 8
          Top = 226
          Width = 395
          Height = 13
          AutoSize = False
          Caption = 'Download URL'
        end
        object UploadUrlLabel: TLabel
          Left = 8
          Top = 271
          Width = 395
          Height = 13
          AutoSize = False
          Caption = 'Upload URL'
        end
        object ApiUrlEdit: TEdit
          Left = 8
          Top = 33
          Width = 409
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = ServerFieldChanged
        end
        object OAuthUrlEdit: TEdit
          Left = 8
          Top = 77
          Width = 409
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          OnChange = ServerFieldChanged
        end
        object DispatcherUrlEdit: TEdit
          Left = 8
          Top = 119
          Width = 409
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          OnChange = ServerFieldChanged
        end
        object ThumbnailUrlEdit: TEdit
          Left = 8
          Top = 159
          Width = 409
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
          OnChange = ServerFieldChanged
        end
        object ServerPublicUrlEdit: TEdit
          Left = 8
          Top = 199
          Width = 409
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 4
          OnChange = ServerFieldChanged
        end
        object DownloadUrlEdit: TEdit
          Left = 8
          Top = 244
          Width = 409
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 5
          OnChange = ServerFieldChanged
        end
        object UploadUrlEdit: TEdit
          Left = 8
          Top = 289
          Width = 409
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 6
          OnChange = ServerFieldChanged
        end
      end
    end
    object TranslationTab: TTabSheet
      Caption = 'Translation'
      ImageIndex = 5
      DesignSize = (
        701
        521)
      object LanguageLabel: TLabel
        Left = 4
        Top = 3
        Width = 52
        Height = 13
        Caption = 'Languages'
      end
      object TranslationStatusLabel: TLabel
        Left = 16
        Top = 413
        Width = 3
        Height = 13
        Anchors = [akLeft, akBottom]
        ExplicitTop = 340
      end
      object LanguageList: TListBox
        Left = 4
        Top = 26
        Width = 693
        Height = 460
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 0
        ExplicitHeight = 444
      end
      object ApplyTranslationBtn: TButton
        Left = 573
        Top = 492
        Width = 124
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Apply'
        TabOrder = 1
        OnClick = ApplyTranslationBtnClick
        ExplicitTop = 476
      end
    end
  end
  object CommandPathOpenDialog: TOpenDialog
    DefaultExt = 'exe'
    Filter = 'Executables|*.exe|Batch scripts|*.bat;*.cmd|Any files|*.*'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Browse...'
    Left = 380
    Top = 440
  end
end
