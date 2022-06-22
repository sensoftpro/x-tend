object LangVclEditFm: TLangVclEditFm
  Left = 0
  Top = 0
  Caption = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1085#1080#1077' '#1089#1083#1086#1074#1072#1088#1077#1081
  ClientHeight = 452
  ClientWidth = 802
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object gridLanguages: TStringGrid
    Left = 0
    Top = 0
    Width = 802
    Height = 404
    Align = alClient
    BorderStyle = bsNone
    ColCount = 3
    DefaultColWidth = 250
    DefaultRowHeight = 22
    FixedCols = 2
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goColMoving, goEditing, goAlwaysShowEditor]
    TabOrder = 0
    OnDblClick = gridLanguagesDblClick
    ColWidths = (
      250
      250
      250)
    RowHeights = (
      22
      22)
  end
  object OkCancel: TPanel
    Left = 0
    Top = 404
    Width = 802
    Height = 48
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 724
      Top = 12
      Width = 73
      Height = 31
      Margins.Left = 5
      Margins.Top = 12
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alRight
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 1
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 641
      Top = 12
      Width = 73
      Height = 31
      Margins.Left = 5
      Margins.Top = 12
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alRight
      Caption = #1054#1082
      ModalResult = 1
      TabOrder = 0
    end
  end
end
