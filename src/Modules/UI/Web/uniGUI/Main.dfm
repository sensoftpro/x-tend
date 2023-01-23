object UniMainForm: TUniMainForm
  Left = 0
  Top = 0
  ClientHeight = 300
  ClientWidth = 635
  Caption = 'Main Form'
  OldCreateOrder = False
  MonitoredKeys.Keys = <>
  OnBeforeShow = UniFormBeforeShow
  PixelsPerInch = 96
  TextHeight = 13
  object UniLabel1: TUniLabel
    Left = 132
    Top = 148
    Width = 46
    Height = 13
    Hint = ''
    Caption = 'UniLabel1'
    TabOrder = 0
  end
  object UniEdit1: TUniEdit
    Left = 276
    Top = 132
    Width = 121
    Hint = ''
    Text = 'UniEdit1'
    TabOrder = 1
  end
  object UniButton1: TUniButton
    Left = 272
    Top = 208
    Width = 75
    Height = 25
    Hint = ''
    Caption = 'UniButton1'
    TabOrder = 2
    OnClick = UniButton1Click
  end
end
