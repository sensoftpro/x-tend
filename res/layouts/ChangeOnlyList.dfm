object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 495
  Height = 307
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 29
    Width = 495
    Height = 278
    Align = alClient
    BevelOuter = bvNone
    Caption = '$'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    PopupMenu = GridPopup
    ShowCaption = False
    TabOrder = 0
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 495
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 27
      Height = 29
      Align = alLeft
      Caption = '$/Selected/Edit'
      TabOrder = 0
      ExplicitLeft = 45
    end
    object Panel6: TPanel
      Left = 27
      Top = 0
      Width = 27
      Height = 29
      Align = alLeft
      Caption = '$/Selected/Delete'
      TabOrder = 1
      ExplicitLeft = 72
    end
    object Panel2: TPanel
      Left = 54
      Top = 0
      Width = 27
      Height = 29
      Align = alLeft
      Caption = '$/Selected/View'
      TabOrder = 2
      ExplicitLeft = 99
    end
  end
  object GridPopup: TPopupMenu
    Left = 252
    Top = 120
    object Edit1: TMenuItem
      Caption = '$/Selected/Edit'
    end
    object Delete1: TMenuItem
      Caption = '$/Selected/Delete'
    end
    object View1: TMenuItem
      Caption = '$/Selected/View'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object RecordCount1: TMenuItem
      Caption = '#RecordCount'
    end
  end
end
