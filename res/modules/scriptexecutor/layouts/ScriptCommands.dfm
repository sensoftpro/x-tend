object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 495
  Height = 307
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 29
    Width = 495
    Height = 278
    Align = alClient
    BevelOuter = bvNone
    Caption = '$'
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
      Width = 27
      Height = 29
      Align = alLeft
      Caption = '$/Add'
      TabOrder = 0
    end
    object Panel5: TPanel
      Left = 27
      Top = 0
      Width = 27
      Height = 29
      Align = alLeft
      Caption = '$/Selected/Edit'
      TabOrder = 1
    end
    object Panel6: TPanel
      Left = 54
      Top = 0
      Width = 27
      Height = 29
      Align = alLeft
      Caption = '$/Selected/Delete'
      TabOrder = 2
    end
    object Panel2: TPanel
      Left = 81
      Top = 0
      Width = 27
      Height = 29
      Align = alLeft
      Caption = '$/Selected/View'
      TabOrder = 3
    end
    object Panel7: TPanel
      Left = 108
      Top = 0
      Width = 9
      Height = 29
      Align = alLeft
      TabOrder = 4
    end
    object Panel8: TPanel
      Left = 117
      Top = 0
      Width = 27
      Height = 29
      Align = alLeft
      Caption = '$/Selected/MoveUp'
      TabOrder = 5
      ExplicitLeft = 150
    end
    object Panel9: TPanel
      Left = 144
      Top = 0
      Width = 27
      Height = 29
      Align = alLeft
      Caption = '$/Selected/MoveDown'
      TabOrder = 6
      ExplicitLeft = 177
    end
  end
end
