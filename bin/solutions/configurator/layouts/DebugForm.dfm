object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 997
  Height = 667
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
  object pcTabs: TPageControl
    Left = 0
    Top = 0
    Width = 997
    Height = 667
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Areas and Views'
      object spltCommon: TSplitter
        Left = 397
        Top = 0
        Width = 8
        Height = 636
        ExplicitLeft = 406
        ExplicitTop = -8
        ExplicitHeight = 604
      end
      object memArea: TPanel
        Left = 0
        Top = 0
        Width = 397
        Height = 636
        Align = alLeft
        Caption = 'SysServices/Current/AreasInfo'
        TabOrder = 0
      end
      object pnlView: TPanel
        Left = 405
        Top = 0
        Width = 584
        Height = 636
        Align = alClient
        Caption = 'SysServices/Current/ViewsInfo'
        TabOrder = 1
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Holders and Log'
      ImageIndex = 1
      object memHolders: TPanel
        AlignWithMargins = True
        Left = 6
        Top = 6
        Width = 325
        Height = 624
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alLeft
        Caption = 'SysServices/Current/HoldersInfo'
        TabOrder = 0
      end
      object memLog: TPanel
        AlignWithMargins = True
        Left = 340
        Top = 6
        Width = 643
        Height = 624
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alClient
        Caption = 'SysServices/Current/Log'
        TabOrder = 1
      end
    end
  end
end
