object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 945
  Height = 579
  TabOrder = 0
  object WorkArea: TPanel
    Left = 0
    Top = 24
    Width = 945
    Height = 555
    Align = alClient
    Caption = '@WorkArea?ViewType=Paged&View=Configurations&Layout=Collection'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ExplicitLeft = 305
    ExplicitWidth = 640
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 945
    Height = 24
    AutoSize = True
    ButtonHeight = 24
    ButtonWidth = 374
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ShowCaptions = True
    TabOrder = 1
    object ToolButton2: TToolButton
      Left = 0
      Top = 0
      Caption = 'View=SysUsers@WorkArea=WorkArea@Layout=Collection'
      ImageIndex = 0
    end
    object ToolButton1: TToolButton
      Left = 374
      Top = 0
      Caption = 'View=Configurations@WorkArea=WorkArea@Layout=Collection'
      ImageIndex = 1
    end
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
end
