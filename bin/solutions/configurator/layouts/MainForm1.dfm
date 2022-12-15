object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 945
  Height = 579
  TabOrder = 0
  object Label1: TLabel
    Left = 192
    Top = 268
    Width = 113
    Height = 33
    Caption = 'Label1'
  end
  object Bevel1: TBevel
    Left = 628
    Top = 88
    Width = 137
    Height = 137
  end
  object Panel1: TPanel
    Left = 380
    Top = 184
    Width = 185
    Height = 41
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 396
    Top = 328
    Width = 185
    Height = 41
    Caption = 'Quit'
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 396
    Top = 404
    Width = 185
    Height = 41
    Caption = 'Quit?ViewStyle=link'
    TabOrder = 2
  end
  object Panel4: TPanel
    Left = 404
    Top = 252
    Width = 185
    Height = 41
    Caption = 'Quit?ViewStyle=panel'
    TabOrder = 3
  end
  object MainMenu: TPopupMenu
    Left = 300
    Top = 107
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
  end
end
