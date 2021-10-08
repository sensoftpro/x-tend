object EditForm: TEditForm
  Left = 0
  Top = 0
  Width = 811
  Height = 361
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  object Panel1: TPanel
    Left = 0
    Top = 81
    Width = 811
    Height = 280
    Align = alClient
    Caption = '$'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    PopupMenu = GridPopup
    TabOrder = 0
  end
  object Panel3: TPanel
    Left = 0
    Top = 53
    Width = 811
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Panel4: TPanel
      Left = 73
      Top = 0
      Width = 42
      Height = 28
      Align = alLeft
      Caption = '$/Add'
      TabOrder = 0
    end
    object Panel5: TPanel
      Left = 115
      Top = 0
      Width = 29
      Height = 28
      Align = alLeft
      Caption = '$/Selected/Edit'
      TabOrder = 1
    end
    object Panel6: TPanel
      Left = 144
      Top = 0
      Width = 29
      Height = 28
      Align = alLeft
      Caption = '$/Selected/Delete'
      TabOrder = 2
    end
    object Panel7: TPanel
      Left = 0
      Top = 0
      Width = 5
      Height = 28
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
    end
    object Panel8: TPanel
      Left = 173
      Top = 0
      Width = 29
      Height = 28
      Align = alLeft
      Caption = '$/Selected/View'
      TabOrder = 4
    end
    object Panel2: TPanel
      Left = 202
      Top = 0
      Width = 29
      Height = 28
      Align = alLeft
      Caption = '$/Refresh'
      TabOrder = 5
    end
    object Panel9: TPanel
      Left = 231
      Top = 0
      Width = 5
      Height = 28
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 6
    end
    object Panel10: TPanel
      Left = 236
      Top = 0
      Width = 29
      Height = 28
      Align = alLeft
      Caption = '#Placeholder'
      TabOrder = 7
    end
    object Panel11: TPanel
      Left = 265
      Top = 0
      Width = 5
      Height = 28
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 8
    end
    object Panel17: TPanel
      Left = 5
      Top = 0
      Width = 68
      Height = 28
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 9
      object Panel19: TPanel
        Left = -1
        Top = 0
        Width = 1
        Height = 29
        Align = alCustom
        Caption = '#FilterByText#List'
        TabOrder = 1
      end
      object Panel18: TPanel
        Left = 0
        Top = 2
        Width = 56
        Height = 24
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = '#FilterByText#List/Text'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clPurple
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
    end
    object Panel20: TPanel
      Left = 270
      Top = 0
      Width = 301
      Height = 28
      Align = alLeft
      BevelOuter = bvNone
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 10
      object Panel21: TPanel
        Left = -1
        Top = 0
        Width = 1
        Height = 29
        Align = alCustom
        Caption = '#FilterByPeriod#List'
        TabOrder = 1
      end
      object Panel22: TPanel
        Left = 6
        Top = 2
        Width = 65
        Height = 21
        Align = alCustom
        Anchors = [akLeft, akTop, akBottom]
        Caption = '#FilterByPeriod#List/IsActive'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
      end
      object Panel23: TPanel
        Left = 97
        Top = 3
        Width = 86
        Height = 23
        Align = alCustom
        Anchors = [akLeft, akTop, akBottom]
        Caption = '#FilterByPeriod#List/FromDate'
        DoubleBuffered = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clPurple
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentDoubleBuffered = False
        ParentFont = False
        TabOrder = 2
      end
      object Panel24: TPanel
        Left = 214
        Top = 3
        Width = 86
        Height = 23
        Align = alCustom
        Anchors = [akLeft, akTop, akBottom]
        Caption = '#FilterByPeriod#List/ToDate'
        DoubleBuffered = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clPurple
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentDoubleBuffered = False
        ParentFont = False
        TabOrder = 3
      end
    end
  end
  object Panel12: TPanel
    Left = 0
    Top = 0
    Width = 811
    Height = 53
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 6
      Width = 130
      Height = 33
      Caption = '$@Caption'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6771712
      Font.Height = -27
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Panel27: TPanel
      Left = 549
      Top = 0
      Width = 262
      Height = 53
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object GridPopup: TPopupMenu
    Left = 396
    Top = 172
    object Add1: TMenuItem
      Caption = '$/Add'
    end
    object SelectedOpenInPage1: TMenuItem
      Caption = '$/Selected/OpenInPage'
    end
    object Edit1: TMenuItem
      Caption = '$/Selected/Edit'
    end
    object Delete1: TMenuItem
      Caption = '$/Selected/Delete'
    end
    object View1: TMenuItem
      Caption = '$/Selected/View'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Placeholder1: TMenuItem
      Caption = '#Placeholder'
    end
    object ExportToCsv1: TMenuItem
      Caption = '#GroupByColumn#List'
    end
    object ApplyBestFit1: TMenuItem
      Caption = '#ApplyBestFit#List'
    end
    object ExportToCsv2: TMenuItem
      Caption = '#ExportToCsv#List'
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object RecordCount1: TMenuItem
      Caption = '#RecordCount'
    end
  end
end
