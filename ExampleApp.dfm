object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Example'
  ClientHeight = 381
  ClientWidth = 733
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    733
    381)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = -3
    Top = 0
    Width = 728
    Height = 344
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 653
    Top = 350
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'test'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 348
    Width = 75
    Height = 25
    Caption = 'Test Class'
    TabOrder = 2
    OnClick = Button2Click
  end
  object IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL
    MaxLineAction = maException
    Port = 0
    DefaultPort = 0
    SSLOptions.Mode = sslmUnassigned
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 608
    Top = 48
  end
  object IdCookieManager1: TIdCookieManager
    Left = 608
    Top = 96
  end
  object HTTP: TIdHTTP
    IOHandler = IdSSLIOHandlerSocketOpenSSL1
    AllowCookies = True
    HandleRedirects = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.ContentRangeEnd = -1
    Request.ContentRangeStart = -1
    Request.ContentRangeInstanceLength = -1
    Request.Accept = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
    Request.BasicAuthentication = False
    Request.UserAgent = 
      'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, l' +
      'ike Gecko) Chrome/24.0.1312.57 Safari/537.17'
    Request.Ranges.Units = 'bytes'
    Request.Ranges = <>
    HTTPOptions = [hoForceEncodeParams]
    CookieManager = IdCookieManager1
    Left = 608
    Top = 16
  end
end
