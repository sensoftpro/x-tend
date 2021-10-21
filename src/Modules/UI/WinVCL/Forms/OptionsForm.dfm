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
    TabOrder = 4
  end
  object chbShowBordersForDisabled: TCheckBox
    Left = 25
    Top = 232
    Width = 369
    Height = 17
    Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1086#1073#1074#1086#1076#1082#1091' '#1075#1088#1072#1085#1080#1094' '#1091' '#1085#1077#1072#1082#1090#1080#1074#1085#1099#1093' '#1087#1086#1083#1077#1081
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object chbMouseMultiSelectInGrids: TCheckBox
    Left = 25
    Top = 260
    Width = 369
    Height = 17
    Caption = #1052#1085#1086#1078#1077#1089#1090#1074#1077#1085#1085#1099#1081' '#1074#1099#1073#1086#1088' '#1089#1090#1088#1086#1082' '#1074' '#1090#1072#1073#1083#1080#1094#1072#1093' '#1084#1099#1096#1082#1086#1081
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object chbMarkRequiredFields: TCheckBox
    Left = 25
    Top = 288
    Width = 369
    Height = 17
    Caption = #1055#1086#1084#1077#1095#1072#1090#1100' '#1086#1073#1103#1079#1072#1090#1077#1083#1100#1085#1099#1077' '#1087#1086#1083#1103' '#1089#1080#1084#1074#1086#1083#1072#1084#1080' **'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object OkCancel: TPanel
    Left = 0
    Top = 336
    Width = 446
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 8
    ExplicitLeft = -5
    ExplicitTop = 0
    ExplicitWidth = 451
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
        6100000041744558745469746C6500436F6E646974696F6E616C466F726D6174
        74696E7349636F6E53657453796D626F6C73333B436F6E646974696F6E616C46
        6F726D617474696E673B9DC5ED720000025449444154785EAD935D6C4B6118C7
        DF2A1BFD38DA4E6DB122D5B5DBAC92CA1CC4B0656CA9CE6ABE3A22662C9DB656
        5F4985359D91655DCC47EDA327CA120417222415DA1925F4424AB6650BDD8A0B
        71C1848CC415B3C7FB9EB4F45CB8907993DFB938CFF3FFE579DEE445003021D8
        CF3F1C1E66322605C3C7FC11D8996C54C768D01EAF06D93A311D1A646D57234B
        7B16DADD96C586153902E10EB7F2AA95A9800D0E8581C81202B6F92BC4D097F1
        281A1D7F813EFF1C409FC6FAD0C71FCFD8B09A164BAA9A95FE5BCF1BA07F8401
        739B9104A7FD16D49E51E1F01037FC9D0D4FCA2DA0E455CDAAF0EDBE2608BF3D
        06CECB4B416F4B77E15A6AF21DF0E3BB4D21A14478CE02816CEBF1798FBB074F
        42E84D3D345D2F84129BFC12AE89493D21E097EFCF5C53E32907936BB69F5E27
        53113B46B4FE50A6FF66A411824307E0ECDD52D0D765448452FE0C124EBEC4A9
        D5A7CBA0F7C33908F47B60674BDE68F1AE9916833DC3D7756F2F04861DC0840C
        603CA878AFA40579E4F25659B123499052B05DEA719C5F043DAF9C303872031A
        2E1AC1D76D879E980BBAC26B61E391B9633945227D7C32DE4A731A47C0C38868
        93A4639F570757222678FAAE13825127F89EE8C17C6A3EE82AA813A4878443AF
        5BD0B26A294780E23B51B891B178B4C03C2A01EFC3D5E0BAB618E8CAE9BDB826
        273D0F626E148C36227A9B842B58827FC425126D99D857D39A0DAD779643716D
        FAB759DAD41564CDFBC36E14787914F9070EA3FC4A8A2BC8DF42111212596EA9
        F04291350DD48582FAC4E80B378B114147D824E60AB8203E868A8F2D62A57F39
        FFE5354E885F4F2C8B1CABF609A90000000049454E44AE426082}
      TabOrder = 0
      ExplicitLeft = 259
      ExplicitTop = 9
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
      ExplicitLeft = 290
    end
  end
end
