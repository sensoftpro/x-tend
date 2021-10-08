object OptionsFm: TOptionsFm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1054#1087#1094#1080#1080
  ClientHeight = 379
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 16
  object lblLanguage: TLabel
    Left = 25
    Top = 21
    Width = 113
    Height = 16
    Caption = #1071#1079#1099#1082#1080' '#1087#1088#1080#1083#1086#1078#1077#1085#1080#1103
  end
  object chbShowStartPage: TCheckBox
    Left = 25
    Top = 176
    Width = 369
    Height = 17
    Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1089#1090#1072#1088#1090#1086#1074#1091#1102' '#1089#1090#1088#1072#1085#1080#1094#1091' '#1087#1088#1080' '#1079#1072#1087#1091#1089#1082#1077' '#1087#1088#1080#1083#1086#1078#1077#1085#1080#1103
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  inline OkCancelFrm1: TOkCancelFrm
    Left = 0
    Top = 336
    Width = 446
    Height = 43
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    ExplicitTop = 336
    ExplicitWidth = 446
    inherited btnOk: TcxButton
      Left = 285
      ExplicitLeft = 285
    end
    inherited btnCancel: TcxButton
      Left = 368
      ExplicitLeft = 368
    end
  end
  object btnEditDictionaries: TButton
    Left = 259
    Top = 82
    Width = 161
    Height = 25
    Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100' '#1089#1083#1086#1074#1072#1088#1080
    Enabled = False
    TabOrder = 2
    OnClick = btnEditDictionariesClick
  end
  object lbxLanguages: TListBox
    Left = 25
    Top = 43
    Width = 217
    Height = 106
    TabOrder = 0
  end
  object btnAddLanguage: TButton
    Left = 259
    Top = 43
    Width = 161
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1103#1079#1099#1082
    Enabled = False
    TabOrder = 1
    OnClick = btnEditDictionariesClick
  end
  object chbShowHorzLinesInGrids: TCheckBox
    Left = 25
    Top = 204
    Width = 369
    Height = 17
    Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1075#1086#1088#1080#1079#1086#1085#1090#1072#1083#1100#1085#1099#1077' '#1083#1080#1085#1080#1080' '#1074' '#1090#1072#1073#1083#1080#1094#1072#1093
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object chbShowBordersForDisabled: TCheckBox
    Left = 25
    Top = 232
    Width = 369
    Height = 17
    Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1086#1073#1074#1086#1076#1082#1091' '#1075#1088#1072#1085#1080#1094' '#1091' '#1085#1077#1072#1082#1090#1080#1074#1085#1099#1093' '#1087#1086#1083#1077#1081
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object chbMouseMultiSelectInGrids: TCheckBox
    Left = 25
    Top = 260
    Width = 369
    Height = 17
    Caption = #1052#1085#1086#1078#1077#1089#1090#1074#1077#1085#1085#1099#1081' '#1074#1099#1073#1086#1088' '#1089#1090#1088#1086#1082' '#1074' '#1090#1072#1073#1083#1080#1094#1072#1093' '#1084#1099#1096#1082#1086#1081
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object chbMarkRequiredFields: TCheckBox
    Left = 25
    Top = 288
    Width = 369
    Height = 17
    Caption = #1055#1086#1084#1077#1095#1072#1090#1100' '#1086#1073#1103#1079#1072#1090#1077#1083#1100#1085#1099#1077' '#1087#1086#1083#1103' '#1089#1080#1084#1074#1086#1083#1072#1084#1080' **'
    Checked = True
    State = cbChecked
    TabOrder = 8
  end
end
