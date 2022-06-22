object SplashVclFm: TSplashVclFm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
  ClientHeight = 300
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    400
    300)
  PixelsPerInch = 96
  TextHeight = 16
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 400
    Height = 300
    Align = alClient
    ExplicitHeight = 281
  end
  object lblName: TLabel
    Left = 166
    Top = 26
    Width = 92
    Height = 34
    Anchors = [akLeft, akTop, akRight]
    Caption = 'lblName'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -27
    Font.Name = 'Impact'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object lblVersion: TLabel
    Left = 166
    Top = 57
    Width = 121
    Height = 18
    Caption = 'Version: 0.0.0.0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object lblCopyright: TLabel
    Left = 16
    Top = 269
    Width = 157
    Height = 16
    Caption = 'Copyright '#169' 2022 Sensoft. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object lblInfo: TLabel
    Left = 16
    Top = 233
    Width = 368
    Height = 16
    AutoSize = False
    Caption = 'Copyright '#169' 2022 Sensoft. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object lblSite: TLabel
    Left = 273
    Top = 267
    Width = 107
    Height = 16
    Cursor = crHandPoint
    Caption = 'https://sensoft.pro'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5531897
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lblSiteClick
  end
  object prbProgress: TProgressBar
    Left = 24
    Top = 251
    Width = 368
    Height = 12
    TabOrder = 0
    TabStop = True
  end
end
