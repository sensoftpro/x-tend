object Form: TForm
  Left = 0
  Top = 0
  Width = 596
  Height = 308
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object Name: TPanel
    Left = 24
    Top = 36
    Width = 273
    Height = 27
    Caption = 'Name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object Parameters: TPanel
    Left = 24
    Top = 90
    Width = 273
    Height = 191
    Caption = 'Parameters?fields=Name'
    TabOrder = 1
  end
  object Type_: TPanel
    Left = 324
    Top = 118
    Width = 245
    Height = 24
    Caption = 'Parameters/Selected/Type'
    TabOrder = 3
  end
  object DefaultValue: TPanel
    Left = 324
    Top = 162
    Width = 245
    Height = 24
    Caption = 'Parameters/Selected/DefaultValue'
    TabOrder = 4
  end
  object Description: TPanel
    Left = 324
    Top = 212
    Width = 245
    Height = 69
    Caption = 'Parameters/Selected/Description'
    TabOrder = 5
  end
  object ValuesOnly: TPanel
    Left = 412
    Top = 69
    Width = 157
    Height = 24
    Caption = 'Parameters/Selected/ValuesOnly'
    TabOrder = 2
  end
end
