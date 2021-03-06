object TranslationVclEditFm: TTranslationVclEditFm
  Left = 0
  Top = 0
  Caption = #1055#1077#1088#1077#1074#1086#1076
  ClientHeight = 445
  ClientWidth = 598
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnDestroy = FormDestroy
  DesignSize = (
    598
    445)
  PixelsPerInch = 96
  TextHeight = 13
  object lblLanguage: TLabel
    Left = 16
    Top = 176
    Width = 82
    Height = 13
    Caption = #1071#1079#1099#1082' '#1087#1077#1088#1077#1074#1086#1076#1072':'
  end
  object lblKey: TLabel
    Left = 16
    Top = 15
    Width = 106
    Height = 13
    Caption = #1050#1083#1102#1095' '#1076#1083#1103' '#1087#1077#1088#1077#1074#1086#1076#1072':'
  end
  object lblKeyValue: TLabel
    Left = 144
    Top = 13
    Width = 394
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = #1085#1077' '#1079#1072#1076#1072#1085
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object cbxLanguage: TComboBox
    Left = 144
    Top = 173
    Width = 217
    Height = 21
    TabOrder = 0
    OnChange = cbxLanguageChange
  end
  object memTranslation: TMemo
    Left = 16
    Top = 208
    Width = 568
    Height = 180
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 1
    WantTabs = True
  end
  object memSourceText: TMemo
    Left = 16
    Top = 40
    Width = 568
    Height = 118
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
    WantTabs = True
  end
  object btnEdit: TButton
    Left = 544
    Top = 8
    Width = 40
    Height = 26
    Hint = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100' '#1082#1083#1102#1095
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = btnEditClick
  end
  object OkCancel: TPanel
    Left = 0
    Top = 402
    Width = 598
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 520
      Top = 5
      Width = 73
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alRight
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 1
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 437
      Top = 5
      Width = 73
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alRight
      Caption = #1054#1082
      ModalResult = 1
      TabOrder = 0
    end
  end
end
