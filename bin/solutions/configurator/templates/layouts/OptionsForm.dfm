object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 378
  Height = 232
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentBackground = False
  ParentColor = False
  ParentFont = False
  TabOrder = 0
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 215
    Height = 19
    Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100#1089#1082#1080#1077' '#1085#1072#1089#1090#1088#1086#1081#1082#1080
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnOk: TPanel
    Left = 213
    Top = 187
    Width = 73
    Height = 33
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Ok?Composition=ImageLeft'
    TabOrder = 0
  end
  object btnCancel: TPanel
    Left = 296
    Top = 187
    Width = 73
    Height = 33
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel?Composition=TextOnly'
    TabOrder = 1
  end
  object chbShowHorzLinesInGrids: TPanel
    Left = 27
    Top = 52
    Width = 342
    Height = 20
    Caption = 'SysServices/Current/ShowHorzLinesInGrids'
    TabOrder = 2
  end
  object chbShowBordersForDisabled: TPanel
    Left = 27
    Top = 80
    Width = 342
    Height = 20
    Caption = 'SysServices/Current/ShowBordersForDisabled'
    TabOrder = 3
  end
  object chbMouseMultiSelectInGrids: TPanel
    Left = 27
    Top = 108
    Width = 342
    Height = 20
    Caption = 'SysServices/Current/MouseMultiselectInGrids'
    TabOrder = 4
  end
  object chbMarkRequiredFields: TPanel
    Left = 27
    Top = 136
    Width = 342
    Height = 20
    Caption = 'SysServices/Current/MarkRequiredFields'
    TabOrder = 5
  end
end
