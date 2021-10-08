object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 811
  Height = 361
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 811
    Height = 361
    Align = alClient
    Caption = '$?CollectionViewType=Pivot'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    PopupMenu = GridPopup
    TabOrder = 0
  end
  object GridPopup: TPopupMenu
    Left = 452
    Top = 200
    object ExportToCsv2: TMenuItem
      Caption = '#ExportToCsv#Pivot'
    end
    object ApplyBestFitPivot1: TMenuItem
      Caption = '#ApplyBestFit#Pivot'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object RecordCount1: TMenuItem
      Caption = '#RecordCount'
    end
  end
end
