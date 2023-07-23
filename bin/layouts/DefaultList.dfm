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
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 45
      Height = 29
      Align = alLeft
      Caption = '$/Add'
      TabOrder = 0
    end
    object Panel5: TPanel
      Left = 45
      Top = 0
      Width = 27
      Height = 29
      Align = alLeft
      Caption = '$/Selected/Edit'
      TabOrder = 1
      ExplicitLeft = 27
    end
    object Panel6: TPanel
      Left = 72
      Top = 0
      Width = 27
      Height = 29
      Align = alLeft
      Caption = '$/Selected/Delete'
      TabOrder = 2
      ExplicitLeft = 54
    end
    object Panel2: TPanel
      Left = 99
      Top = 0
      Width = 27
      Height = 29
      Align = alLeft
      Caption = '$/Selected/View'
      TabOrder = 3
      ExplicitLeft = 81
    end
  end
end
