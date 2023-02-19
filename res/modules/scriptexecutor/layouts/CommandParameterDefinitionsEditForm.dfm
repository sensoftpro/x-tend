object Form: TForm
  Left = 0
  Top = 0
  Width = 462
  Height = 239
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object Name: TPanel
    Left = 20
    Top = 36
    Width = 233
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
  object Type_: TPanel
    Left = 20
    Top = 90
    Width = 233
    Height = 24
    Caption = 'Type'
    TabOrder = 2
  end
  object Description: TPanel
    Left = 20
    Top = 141
    Width = 421
    Height = 68
    Caption = 'Description'
    TabOrder = 4
  end
  object DefaultValue: TPanel
    Left = 280
    Top = 90
    Width = 157
    Height = 24
    Caption = 'DefaultValue'
    TabOrder = 3
  end
  object ValuesOnly: TPanel
    Left = 280
    Top = 37
    Width = 157
    Height = 24
    Caption = 'ValuesOnly'
    TabOrder = 1
  end
end
