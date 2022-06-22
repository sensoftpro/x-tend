object AboutVclFm: TAboutVclFm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
  ClientHeight = 249
  ClientWidth = 403
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    403
    249)
  PixelsPerInch = 96
  TextHeight = 16
  object lblVersion: TLabel
    Left = 144
    Top = 45
    Width = 92
    Height = 16
    Caption = 'Version: 0.0.0.0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4795653
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = False
  end
  object lblCopyright: TLabel
    Left = 144
    Top = 158
    Width = 157
    Height = 16
    Caption = 'Copyright '#169' 2022 Sensoft. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4795653
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = False
  end
  object Image1: TImage
    Left = 16
    Top = 16
    Width = 100
    Height = 120
  end
  object lblName: TLabel
    Left = 144
    Top = 16
    Width = 242
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblName'
    EllipsisPosition = epWordEllipsis
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4795653
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 272
  end
  object lblEmail: TLabel
    Left = 144
    Top = 110
    Width = 40
    Height = 16
    Caption = 'E-Mail:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4795653
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = False
  end
  object lblWeb: TLabel
    Left = 144
    Top = 86
    Width = 31
    Height = 16
    Caption = 'Web:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4795653
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = False
  end
  object lblAllRightsReserved: TLabel
    Left = 144
    Top = 180
    Width = 114
    Height = 16
    Caption = 'All Rights Reserved.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4795653
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = False
  end
  object lblSite: TLabel
    Left = 190
    Top = 82
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
  object lblMail: TLabel
    Left = 190
    Top = 110
    Width = 97
    Height = 16
    Cursor = crHandPoint
    Caption = 'info@sensoft.pro'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5531897
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lblMailClick
  end
  object btnOk: TButton
    Left = 306
    Top = 206
    Width = 81
    Height = 27
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object btnFeedback: TButton
    Left = 144
    Top = 206
    Width = 156
    Height = 27
    Anchors = [akRight, akBottom]
    Caption = 'Feedback'
    TabOrder = 1
  end
end
