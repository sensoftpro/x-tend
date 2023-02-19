object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 945
  Height = 579
  TabOrder = 0
  DesignSize = (
    945
    579)
  object WorkArea: TPanel
    Left = 274
    Top = 66
    Width = 671
    Height = 513
    Align = alClient
    Caption = '@WorkArea?ViewType=Paged&View=Configurations&Layout=Collection'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitLeft = 280
    ExplicitTop = 72
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 945
    Height = 17
    Align = alTop
    Caption = '@NavigationArea?ViewType=MainMenu'
    PopupMenu = MainMenu
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 0
    Top = 17
    Width = 945
    Height = 49
    Align = alTop
    Caption = '@NavigationArea?ViewType=ToolBar'
    PopupMenu = ToolBar
    TabOrder = 2
  end
  object Panel3: TPanel
    Left = 0
    Top = 66
    Width = 137
    Height = 513
    Align = alLeft
    Caption = '@NavigationArea?ViewType=TreeView'
    PopupMenu = MainMenu
    TabOrder = 3
  end
  object Panel4: TPanel
    Left = 137
    Top = 66
    Width = 137
    Height = 513
    Align = alLeft
    Caption = '@NavigationArea?ViewType=NavBar'
    Color = clTeal
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    PopupMenu = MainMenu
    TabOrder = 4
  end
  object Panel5: TPanel
    Left = 18
    Top = 528
    Width = 103
    Height = 33
    Anchors = [akLeft, akTop, akBottom]
    Caption = '#Refill#TreeView'
    TabOrder = 5
  end
  object MainMenu: TPopupMenu
    Left = 340
    Top = 171
    object System1: TMenuItem
      Caption = '@System?Id=System&Caption='#1057#1080#1089#1090#1077#1084#1072
      object ShowSysLog1: TMenuItem
        Caption = 'ShowSysLog'
      end
      object ShowSettings1: TMenuItem
        Caption = 'ShowSettings'
      end
      object ShowOptions1: TMenuItem
        Caption = 'ShowOptions'
      end
      object LoadChanges1: TMenuItem
        Caption = 'LoadChanges'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Quit1: TMenuItem
        Caption = 'Quit'
      end
    end
    object Libraries1: TMenuItem
      Caption = '@Libraries?Id=Libraries&Caption='#1057#1087#1088#1072#1074#1086#1095#1085#1080#1082#1080
    end
    object ConfigurationsCaption1: TMenuItem
      Caption = '@Configurations?Caption='#1050#1086#1085#1092#1080#1075#1091#1088#1072#1094#1080#1080
      object Configurations1: TMenuItem
        Caption = 'Configurations/@'
      end
    end
    object Windows1: TMenuItem
      Caption = '@Windows?Id=Windows&Caption='#1054#1082#1085#1072
      object CloseAllPages1: TMenuItem
        Caption = 'CloseAllWindows'
      end
    end
    object ShowAbout1: TMenuItem
      Caption = 'ShowAbout'
    end
    object SystemIdSystemCaption1: TMenuItem
      Caption = '@Parent1?Caption='#1056#1086#1076#1080#1090#1077#1083#1100' 1'
      object Quit2: TMenuItem
        Caption = '@Parent2?Caption='#1056#1086#1076#1080#1090#1077#1083#1100' 2'
        object Parent3Caption31: TMenuItem
          Caption = '@Parent3?Caption='#1056#1086#1076#1080#1090#1077#1083#1100' 3'
          object ShowAbout2: TMenuItem
            Caption = 'ShowAbout'
          end
        end
      end
    end
  end
  object ToolBar: TPopupMenu
    Left = 784
    Top = 16
    object SysUsersWorkAreaWorkAreaLayoutCollection1: TMenuItem
      Caption = 'SysUsers?WorkArea=ContentWorkArea&Layout=Collection'
    end
    object ConfigurationsWorkAreaWorkAreaLayoutCollection1: TMenuItem
      Caption = 'Configurations?WorkArea=WorkArea&Layout=Collection'
    end
    object StreetsWorkAreaWorkAreaLayoutCollection1: TMenuItem
      Caption = 'Streets?WorkArea=WorkArea&Layout=Collection'
    end
    object DistrictsWorkAreaWorkAreaLayoutCollection1: TMenuItem
      Caption = 'Districts?WorkArea=WorkArea&Layout=Collection'
    end
    object DistrictsWorkAreaWorkAreaLayoutCollection2: TMenuItem
      Caption = '_Owners?WorkArea=WorkArea&Layout=Collection'
    end
  end
end
