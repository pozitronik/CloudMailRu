object RegistrationForm: TRegistrationForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Registration'
  ClientHeight = 352
  ClientWidth = 513
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
  object FirstNameLabel: TLabel
    Left = 8
    Top = 8
    Width = 54
    Height = 13
    Caption = 'First name:'
  end
  object LastNameLabel: TLabel
    Left = 263
    Top = 8
    Width = 53
    Height = 13
    Caption = 'Last name:'
  end
  object LoginLabel: TLabel
    Left = 33
    Top = 35
    Width = 29
    Height = 13
    Caption = 'Login:'
  end
  object AtLabel: TLabel
    Left = 306
    Top = 35
    Width = 10
    Height = 13
    Caption = '@'
  end
  object PasswordLabel: TLabel
    Left = 12
    Top = 64
    Width = 50
    Height = 13
    Caption = 'Password:'
  end
  object CaptchaImg: TImage
    Left = 8
    Top = 118
    Width = 500
    Height = 200
    AutoSize = True
    Center = True
    Proportional = True
    Stretch = True
  end
  object CaptchaLabel: TLabel
    Left = 8
    Top = 324
    Width = 44
    Height = 13
    Caption = 'Captcha:'
  end
  object FirstNameEdit: TEdit
    Left = 68
    Top = 8
    Width = 176
    Height = 21
    TabOrder = 0
    Text = 'Nathan'
  end
  object LastNameEdit: TEdit
    Left = 322
    Top = 8
    Width = 186
    Height = 21
    TabOrder = 1
    Text = 'Drake'
  end
  object LoginEdit: TEdit
    Left = 68
    Top = 35
    Width = 232
    Height = 21
    TabOrder = 2
    Text = 'LoginEdit'
  end
  object DomainCombo: TComboBox
    Left = 322
    Top = 35
    Width = 186
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
    Left = 450
    Top = 87
    Width = 58
    Height = 25
    Caption = 'Sign up'
    TabOrder = 4
    OnClick = SignupBTNClick
  end
  object UserAgreementLink: TLinkLabel
    Left = 8
    Top = 95
    Width = 436
    Height = 17
    Caption = 
      'By pressing the "Sign up" button, you accept the terms and condi' +
      'tions of  <a href="https://help.mail.ru/mail-help/UA">User agree' +
      'ment</a>'
    TabOrder = 5
  end
  object PasswordEdit: TEdit
    Left = 68
    Top = 64
    Width = 221
    Height = 21
    TabOrder = 6
    Text = 'PasswordEdit'
  end
  object UseTCPwdMngrCB: TCheckBox
    Left = 295
    Top = 64
    Width = 213
    Height = 17
    Caption = 'Store password in TC password manager'
    TabOrder = 7
  end
  object CaptchaEdit: TEdit
    Left = 58
    Top = 324
    Width = 369
    Height = 21
    TabOrder = 8
    Text = 'CaptchaEdit'
  end
  object SendBtn: TButton
    Left = 433
    Top = 324
    Width = 75
    Height = 21
    Caption = 'SendBtn'
    TabOrder = 9
  end
end
