object Form: TForm
  Tag = 1
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
  object Splitter1: TSplitter
    Left = 297
    Top = 73
    Width = 10
    Height = 636
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 1010
    Height = 73
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      1010
      73)
    object Name: TPanel
      Left = 20
      Top = 20
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
    object Panel5: TPanel
      Left = 930
      Top = 6
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
      Left = 868
      Top = 6
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
      TabOrder = 2
    end
    object ident: TPanel
      Left = 568
      Top = 20
      Width = 273
      Height = 29
      Caption = 'Ident'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 73
    Width = 297
    Height = 636
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 57
    ExplicitHeight = 652
    object Splitter3: TSplitter
      Left = 0
      Top = 336
      Width = 297
      Height = 8
      Cursor = crVSplit
      Align = alTop
    end
    object Methods: TPanel
      AlignWithMargins = True
      Left = 20
      Top = 364
      Width = 274
      Height = 269
      Margins.Left = 20
      Margins.Top = 20
      Align = alClient
      Caption = 'Methods'
      TabOrder = 0
      ExplicitLeft = -140
      ExplicitTop = 399
      ExplicitWidth = 325
      ExplicitHeight = 253
    end
    object Variables: TPanel
      AlignWithMargins = True
      Left = 20
      Top = 20
      Width = 274
      Height = 313
      Margins.Left = 20
      Margins.Top = 20
      Align = alTop
      Caption = 'Variables'
      TabOrder = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 297
    end
  end
  object pnlClient: TPanel
    Left = 307
    Top = 73
    Width = 703
    Height = 636
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 312
    ExplicitTop = 112
    ExplicitWidth = 481
    ExplicitHeight = 277
    object Splitter2: TSplitter
      Left = 0
      Top = 353
      Width = 703
      Height = 8
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 358
    end
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 703
      Height = 353
      ActivePage = TabSheet2
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      MultiLine = True
      ParentFont = False
      TabOrder = 0
      TabPosition = tpRight
      ExplicitLeft = -133
      ExplicitTop = -28
      ExplicitWidth = 614
      ExplicitHeight = 305
      object TabSheet1: TTabSheet
        Caption = #1050#1086#1084#1072#1085#1076#1099
        ExplicitWidth = 584
        ExplicitHeight = 297
        object Panel1: TPanel
          Left = 0
          Top = 0
          Width = 673
          Height = 345
          Align = alClient
          Caption = 'Commands?fields=Text&childlayout=ScriptCommands'
          TabOrder = 0
          ExplicitWidth = 584
          ExplicitHeight = 297
        end
      end
      object TabSheet2: TTabSheet
        Caption = #1050#1086#1076
        ImageIndex = 1
        ExplicitWidth = 584
        ExplicitHeight = 297
        object Panel6: TPanel
          Left = 0
          Top = 0
          Width = 673
          Height = 345
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
          ExplicitWidth = 584
          ExplicitHeight = 297
        end
      end
    end
    object SelectedMethod: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 381
      Width = 680
      Height = 252
      Margins.Top = 20
      Margins.Right = 20
      Align = alBottom
      Caption = 'Methods/Selected/Commands?fields=Text&childlayout=ScriptCommands'
      TabOrder = 1
      ExplicitLeft = -133
      ExplicitTop = 384
      ExplicitWidth = 846
    end
  end
end
