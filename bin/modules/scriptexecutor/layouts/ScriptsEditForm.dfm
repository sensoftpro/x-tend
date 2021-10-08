object Form: TForm
  Left = 0
  Top = 0
  Width = 1010
  Height = 709
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    1010
    709)
  object PageControl1: TPageControl
    Left = 375
    Top = 92
    Width = 614
    Height = 305
    ActivePage = TabSheet2
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    MultiLine = True
    ParentFont = False
    TabOrder = 5
    TabPosition = tpRight
    object TabSheet1: TTabSheet
      Caption = #1050#1086#1084#1072#1085#1076#1099
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 584
        Height = 297
        Align = alClient
        Caption = 'Commands?fields=Text&childlayout=ScriptCommands'
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = #1050#1086#1076
      ImageIndex = 1
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 584
        Height = 297
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Code'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Meslo LG L DZ'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
  end
  object Name: TPanel
    Left = 20
    Top = 28
    Width = 537
    Height = 29
    Caption = 'Name'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 20
    Top = 92
    Width = 325
    Height = 305
    Caption = 'Variables'
    TabOrder = 2
  end
  object Panel3: TPanel
    Left = 20
    Top = 436
    Width = 325
    Height = 253
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Methods'
    TabOrder = 3
  end
  object Panel4: TPanel
    Left = 375
    Top = 436
    Width = 614
    Height = 253
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Methods/Selected/Commands?fields=Text&childlayout=ScriptCommands'
    TabOrder = 4
  end
  object Panel5: TPanel
    Left = 933
    Top = 20
    Width = 56
    Height = 49
    Anchors = [akTop, akRight]
    Caption = 'RunScript?ImageSize=32'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'RunScript'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object Panel7: TPanel
    Left = 871
    Top = 19
    Width = 56
    Height = 49
    Anchors = [akTop, akRight]
    Caption = 'ConvertToCode?ImageSize=32'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'RunScript'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object Panel8: TPanel
    Left = 568
    Top = 28
    Width = 273
    Height = 29
    Caption = 'Ident'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
  end
end
