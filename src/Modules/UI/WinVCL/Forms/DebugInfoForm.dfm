object DebugFm: TDebugFm
  Left = 0
  Top = 0
  Caption = #1054#1090#1083#1072#1076#1086#1095#1085#1072#1103' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1103
  ClientHeight = 785
  ClientWidth = 993
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pcTabs: TPageControl
    Left = 0
    Top = 0
    Width = 993
    Height = 785
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object spltCommon: TSplitter
        Left = 397
        Top = 0
        Width = 8
        Height = 757
        ExplicitLeft = 406
        ExplicitTop = -8
        ExplicitHeight = 604
      end
      object memArea: TMemo
        Left = 0
        Top = 0
        Width = 397
        Height = 757
        Align = alLeft
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object pnlView: TPanel
        Left = 405
        Top = 0
        Width = 580
        Height = 757
        Align = alClient
        Caption = 'pnlView'
        TabOrder = 1
        object memView: TMemo
          Left = 1
          Top = 1
          Width = 578
          Height = 755
          Align = alClient
          ScrollBars = ssBoth
          TabOrder = 0
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object memHolders: TMemo
        AlignWithMargins = True
        Left = 6
        Top = 6
        Width = 325
        Height = 745
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alLeft
        Lines.Strings = (
          'memHolders')
        TabOrder = 0
      end
      object memLog: TMemo
        AlignWithMargins = True
        Left = 340
        Top = 6
        Width = 639
        Height = 745
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alClient
        Lines.Strings = (
          'memLog')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
  end
end
