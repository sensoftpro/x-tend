object DebugVclFm: TDebugVclFm
  Left = 0
  Top = 0
  Caption = #1054#1090#1083#1072#1076#1086#1095#1085#1072#1103' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1103
  ClientHeight = 644
  ClientWidth = 993
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = UpdateDebugInfo
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pcTabs: TPageControl
    Left = 0
    Top = 0
    Width = 993
    Height = 644
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 785
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      ExplicitHeight = 757
      object spltCommon: TSplitter
        Left = 537
        Top = 0
        Width = 8
        Height = 616
        ExplicitLeft = 406
        ExplicitTop = -8
        ExplicitHeight = 604
      end
      object memArea: TMemo
        Left = 0
        Top = 0
        Width = 537
        Height = 616
        Align = alLeft
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object pnlView: TPanel
        Left = 545
        Top = 0
        Width = 440
        Height = 616
        Align = alClient
        Caption = 'pnlView'
        TabOrder = 1
        ExplicitLeft = 405
        ExplicitWidth = 580
        ExplicitHeight = 757
        object memView: TMemo
          Left = 1
          Top = 1
          Width = 438
          Height = 614
          Align = alClient
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 0
          ExplicitWidth = 578
          ExplicitHeight = 755
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      ExplicitHeight = 757
      object memHolders: TMemo
        AlignWithMargins = True
        Left = 6
        Top = 6
        Width = 325
        Height = 604
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alLeft
        Lines.Strings = (
          'memHolders')
        ReadOnly = True
        TabOrder = 0
        ExplicitHeight = 745
      end
      object memLog: TMemo
        AlignWithMargins = True
        Left = 340
        Top = 6
        Width = 639
        Height = 604
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alClient
        Lines.Strings = (
          'memLog')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
        ExplicitHeight = 745
      end
    end
  end
end
