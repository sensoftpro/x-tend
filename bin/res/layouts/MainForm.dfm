object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 1080
  Height = 710
  TabOrder = 0
  object WorkArea: TPanel
    Left = 0
    Top = 24
    Width = 1080
    Height = 686
    Align = alClient
    Caption = '@WorkArea?ViewType=Paged'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 1080
    Height = 24
    AutoSize = True
    ButtonHeight = 24
    ButtonWidth = 163
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Caption = 'ShowSettings'
      ImageIndex = 2
    end
  end
  object MainMenu1: TMainMenu
    Left = 340
    Top = 171
    object System1: TMenuItem
      Caption = 'Id=System@Caption='#1057#1080#1089#1090#1077#1084#1072
      object ShowOptions1: TMenuItem
        Caption = 'ShowOptions'
      end
      object ShowSettings1: TMenuItem
        Caption = 'ShowSettings'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Quit1: TMenuItem
        Caption = 'Quit'
      end
    end
    object IdLibrariesCaption1: TMenuItem
      Caption = 'Id=Libraries@Caption='#1057#1087#1088#1072#1074#1086#1095#1085#1080#1082#1080
    end
    object IdWindowsCaption1: TMenuItem
      Caption = 'Id=Windows@Caption='#1054#1082#1085#1072
      object PlaceCascade1: TMenuItem
        Caption = 'ArrangeCascade'
      end
      object PlaceVert1: TMenuItem
        Caption = 'ArrangeVert'
      end
      object PlaceHorz1: TMenuItem
        Caption = 'ArrangeHorz'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object CloseAllWindows1: TMenuItem
        Caption = 'CloseAllWindows'
      end
    end
    object ShowAbout1: TMenuItem
      Caption = 'ShowAbout'
    end
  end
end
