object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 945
  Height = 579
  TabOrder = 0
  object WorkArea: TPanel
    Left = 0
    Top = 49
    Width = 945
    Height = 530
    Align = alClient
    Caption = '@WorkArea?ViewType=Paged&View=Configurations&Layout=Collection'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitTop = 24
    ExplicitHeight = 555
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 945
    Height = 49
    Align = alTop
    Caption = '@NavigationArea?ViewType=ToolBar'
    PopupMenu = ToolBar
    TabOrder = 1
  end
  object MainMenu1: TMainMenu
    Left = 340
    Top = 171
    object System1: TMenuItem
      Caption = 'Id=System@Caption='#1057#1080#1089#1090#1077#1084#1072
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
      Caption = 'Id=Libraries@Caption='#1057#1087#1088#1072#1074#1086#1095#1085#1080#1082#1080
    end
    object Windows1: TMenuItem
      Caption = 'Id=Windows@Caption='#1054#1082#1085#1072
      object CloseAllPages1: TMenuItem
        Caption = 'CloseAllWindows'
      end
    end
    object ShowAbout1: TMenuItem
      Caption = 'ShowAbout'
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
  end
end
