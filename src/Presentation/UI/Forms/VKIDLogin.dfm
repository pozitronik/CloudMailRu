object VKIDLoginForm: TVKIDLoginForm
  Left = 0
  Top = 0
  BorderStyle = bsSizeable
  Caption = 'Mail.ru / VK ID Login'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  TextHeight = 13
  object EdgeBrowser: TEdgeBrowser
    Left = 0
    Top = 0
    Width = 800
    Height = 600
    Align = alClient
    TabOrder = 0
    OnNavigationCompleted = EdgeBrowserNavigationCompleted
  end
end
