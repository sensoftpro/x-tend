object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 451
  Height = 43
  TabOrder = 0
  object Panel1: TPanel
    Left = 280
    Top = 0
    Width = 171
    Height = 43
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object btnCancel: TPanel
      AlignWithMargins = True
      Left = 88
      Top = 5
      Width = 73
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Cancel'
      TabOrder = 0
      ExplicitLeft = 373
    end
    object btnOk: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 73
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      Caption = 'Ok'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      ExplicitLeft = 1
      ExplicitTop = 6
      ExplicitHeight = 31
    end
  end
end
