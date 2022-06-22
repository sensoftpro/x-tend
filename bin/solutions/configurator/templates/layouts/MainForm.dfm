object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 945
  Height = 579
  TabOrder = 0
  object WorkArea: TPanel
    Left = 0
    Top = 73
    Width = 945
    Height = 506
    Align = alClient
    Caption = '@WorkArea?ViewType=Paged&View=SysUsers&Layout=Collection'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Panel6: TPanel
    Left = 0
    Top = 0
    Width = 945
    Height = 25
    Align = alTop
    Caption = '@NavigationArea?ViewType=MainMenu'
    PopupMenu = MainMenu
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 25
    Width = 945
    Height = 48
    Align = alTop
    Caption = '@NavigationArea?ViewType=ToolBar&ImageSize=32'
    PopupMenu = ToolBar
    TabOrder = 2
  end
  object MainMenu: TPopupMenu
    Left = 768
    Top = 160
    object SystemCaption1: TMenuItem
      Caption = '@System?Caption='#1057#1080#1089#1090#1077#1084#1072
      object ShowSysLog2: TMenuItem
        Caption = 'ShowSysLog'
      end
      object MenuItem2: TMenuItem
        Caption = 'ShowSettings'
      end
      object MenuItem1: TMenuItem
        Caption = 'ShowOptions'
      end
      object DetectorsLayoutDetectorsCollectionContentWorkAreamodal1: TMenuItem
        Caption = 'LoadChanges'
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MenuItem3: TMenuItem
        Caption = 'Quit'
      end
    end
    object MenuItem4: TMenuItem
      Caption = '@Libs?Id=Libraries&Caption='#1057#1087#1088#1072#1074#1086#1095#1085#1080#1082#1080
    end
    object WindowsCaption1: TMenuItem
      Caption = '@Windows?Caption='#1054#1082#1085#1072
      object ArrangeCascade1: TMenuItem
        Caption = 'ArrangeCascade'
      end
      object ArrangeVert1: TMenuItem
        Caption = 'ArrangeVert'
      end
      object ArrangeHorz1: TMenuItem
        Caption = 'ArrangeHorz'
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object CloseAllWindows2: TMenuItem
        Caption = 'CloseAllWindows'
      end
    end
    object ShowAbout2: TMenuItem
      Caption = 'ShowAbout'
    end
  end
  object ToolBar: TPopupMenu
    Left = 448
    Top = 184
    object SysUsers1: TMenuItem
      Caption = 'SysUsers'
    end
  end
end
