object RegistrationForm: TRegistrationForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 5
  Caption = 'Mail.ru account registration'
  ClientHeight = 337
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  TextHeight = 13
  object FirstNameLabel: TLabel
    Left = 0
    Top = 0
    Width = 54
    Height = 13
    Caption = 'First name:'
  end
  object LastNameLabel: TLabel
    Left = 244
    Top = 0
    Width = 53
    Height = 13
    Caption = 'Last name:'
  end
  object LoginLabel: TLabel
    Left = 25
    Top = 27
    Width = 29
    Height = 13
    Caption = 'Login:'
  end
  object AtLabel: TLabel
    Left = 287
    Top = 27
    Width = 10
    Height = 13
    Caption = '@'
  end
  object PasswordLabel: TLabel
    Left = 4
    Top = 56
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object CaptchaImg: TImage
    Left = 0
    Top = 110
    Width = 500
    Height = 200
    AutoSize = True
    Center = True
    Proportional = True
    Stretch = True
  end
  object CaptchaLabel: TLabel
    Left = 0
    Top = 316
    Width = 44
    Height = 13
    Caption = 'Captcha:'
  end
  object FirstNameEdit: TEdit
    Left = 60
    Top = 0
    Width = 176
    Height = 21
    TabOrder = 0
    OnChange = FirstNameEditChange
  end
  object LastNameEdit: TEdit
    Left = 303
    Top = 0
    Width = 197
    Height = 21
    TabOrder = 1
    OnChange = FirstNameEditChange
  end
  object LoginEdit: TEdit
    Left = 60
    Top = 27
    Width = 221
    Height = 21
    TabOrder = 2
    OnChange = FirstNameEditChange
  end
  object DomainCombo: TComboBox
    Left = 303
    Top = 27
    Width = 197
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 3
    Text = 'mail.ru'
    Items.Strings = (
      'mail.ru'
      'inbox.ru'
      'list.ru'
      'bk.ru')
  end
  object SignupBTN: TButton
    Left = 442
    Top = 79
    Width = 58
    Height = 25
    Caption = 'Sign up'
    Enabled = False
    TabOrder = 4
    OnClick = SignupBTNClick
  end
  object UserAgreementLink: TLinkLabel
    Left = 0
    Top = 87
    Width = 436
    Height = 17
    Caption = 
      'By pressing the "Sign up" button, you accept the terms and condi' +
      'tions of  <a href="https://help.mail.ru/mail-help/UA">User agree' +
      'ment</a>'
    TabOrder = 5
  end
  object PasswordEdit: TEdit
    Left = 60
    Top = 56
    Width = 221
    Height = 21
    TabOrder = 6
    OnChange = FirstNameEditChange
  end
  object UseTCPwdMngrCB: TCheckBox
    Left = 287
    Top = 56
    Width = 213
    Height = 17
    Caption = 'Store password in TC password manager'
    TabOrder = 7
  end
  object CaptchaEdit: TEdit
    Left = 50
    Top = 316
    Width = 369
    Height = 21
    Enabled = False
    TabOrder = 8
    OnKeyUp = CaptchaEditKeyUp
  end
  object SendBtn: TButton
    Left = 425
    Top = 316
    Width = 75
    Height = 21
    Caption = 'Confirm'
    Enabled = False
    ModalResult = 1
    TabOrder = 9
    OnClick = SendBtnClick
  end
end
