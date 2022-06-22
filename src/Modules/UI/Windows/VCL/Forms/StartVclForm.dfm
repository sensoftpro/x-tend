object StartVclFm: TStartVclFm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1057#1090#1072#1088#1090#1086#1074#1072#1103' '#1092#1086#1088#1084#1072
  ClientHeight = 287
  ClientWidth = 448
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object lblLanguage: TLabel
    Left = 25
    Top = 217
    Width = 101
    Height = 16
    Caption = #1071#1079#1099#1082' '#1087#1083#1072#1090#1092#1086#1088#1084#1099
  end
  object lblAvailableConfigurations: TLabel
    Left = 25
    Top = 17
    Width = 151
    Height = 16
    Caption = #1044#1086#1089#1090#1091#1087#1085#1099#1077' '#1082#1086#1085#1092#1080#1075#1091#1088#1072#1094#1080#1080
  end
  object btnRun: TButton
    Left = 328
    Top = 40
    Width = 105
    Height = 33
    Caption = #1047#1072#1087#1091#1089#1082
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = btnRunClick
  end
  object lbxActiveDomains: TListBox
    Left = 25
    Top = 40
    Width = 289
    Height = 157
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemHeight = 19
    Items.Strings = (
      'brbt'
      'btrsb')
    ParentFont = False
    TabOrder = 1
    OnDblClick = lbxActiveDomainsDblClick
  end
  object cbxLanguage: TComboBox
    Left = 24
    Top = 239
    Width = 289
    Height = 24
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnChange = cbxLanguageChange
  end
end
