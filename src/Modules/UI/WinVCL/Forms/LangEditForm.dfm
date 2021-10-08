object LangEditFm: TLangEditFm
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
  inline OkCancelFrm1: TOkCancelFrm
    Left = 0
    Top = 401
    Width = 802
    Height = 51
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitTop = 401
    ExplicitWidth = 802
    ExplicitHeight = 51
    inherited btnOk: TcxButton
      Left = 641
      Height = 41
      ExplicitLeft = 641
      ExplicitHeight = 41
    end
    inherited btnCancel: TcxButton
      Left = 724
      Height = 41
      ExplicitLeft = 724
      ExplicitHeight = 41
    end
  end
  object gridLanguages: TStringGrid
    Left = 0
    Top = 0
    Width = 802
    Height = 401
    Align = alClient
    BorderStyle = bsNone
    ColCount = 3
    DefaultColWidth = 250
    DefaultRowHeight = 22
    FixedCols = 2
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goColMoving, goEditing, goAlwaysShowEditor]
    TabOrder = 1
    OnDblClick = gridLanguagesDblClick
    ColWidths = (
      250
      250
      250)
    RowHeights = (
      22
      22)
  end
end
