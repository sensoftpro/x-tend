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
    Height = 134
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
    TabOrder = 3
  end
  object chbShowBordersForDisabled: TCheckBox
    Left = 25
    Top = 232
    Width = 369
    Height = 17
    Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1086#1073#1074#1086#1076#1082#1091' '#1075#1088#1072#1085#1080#1094' '#1091' '#1085#1077#1072#1082#1090#1080#1074#1085#1099#1093' '#1087#1086#1083#1077#1081
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object chbMouseMultiSelectInGrids: TCheckBox
    Left = 25
    Top = 260
    Width = 369
    Height = 17
    Caption = #1052#1085#1086#1078#1077#1089#1090#1074#1077#1085#1085#1099#1081' '#1074#1099#1073#1086#1088' '#1089#1090#1088#1086#1082' '#1074' '#1090#1072#1073#1083#1080#1094#1072#1093' '#1084#1099#1096#1082#1086#1081
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object chbMarkRequiredFields: TCheckBox
    Left = 25
    Top = 288
    Width = 369
    Height = 17
    Caption = #1055#1086#1084#1077#1095#1072#1090#1100' '#1086#1073#1103#1079#1072#1090#1077#1083#1100#1085#1099#1077' '#1087#1086#1083#1103' '#1089#1080#1084#1074#1086#1083#1072#1084#1080' **'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object OkCancel: TPanel
    Left = 0
    Top = 336
    Width = 446
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 7
    object btnOk: TcxButton
      AlignWithMargins = True
      Left = 285
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
      OptionsImage.Glyph.SourceDPI = 96
      OptionsImage.Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000000467414D410000B18F0BFC6105000000206348524D00007A26000080
        840000FA00000080E8000075300000EA6000003A98000017709CBA513C000000
        06624B4744000000000000F943BB7F0000000970485973000000600000006000
        F06B42CF000000784944415438CBE5D0310A83401005D007896D20B708586891
        2687CB65D4C6BB78062F9010C809368DC212D075AD02F9F5BCF9CCF07739E08E
        D35EDC2260C0391777130E78A2CCC14D845FB82E0D5F26B0D65C2FE11BDEE871
        CC6D2E3046C36DF4B064F39C0A8F086D6A4E2DC9C2DF4B76E139F5969B7F231F
        ABEB2869761A44B90000002574455874646174653A6372656174650032303231
        2D31322D31315430383A31383A34322B30303A3030B82636DD00000025744558
        74646174653A6D6F6469667900323032312D31322D31315430383A31383A3432
        2B30303A3030C97B8E610000000049454E44AE426082}
      TabOrder = 0
    end
    object btnCancel: TcxButton
      AlignWithMargins = True
      Left = 368
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
  end
end
