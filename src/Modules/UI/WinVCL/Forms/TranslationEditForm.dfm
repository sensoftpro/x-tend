object TranslationEditFm: TTranslationEditFm
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
    ExplicitTop = 2
    ExplicitWidth = 451
    object btnOk: TcxButton
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
      ExplicitLeft = 416
    end
    object btnCancel: TcxButton
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
      ExplicitLeft = 290
    end
  end
end
