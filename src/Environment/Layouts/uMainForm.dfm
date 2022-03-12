object frMain: TfrMain
  Left = 0
  Top = 0
  Caption = #1051#1077#1081#1072#1091#1090#1099' '#1076#1083#1103' '#1092#1086#1088#1084
  ClientHeight = 555
  ClientWidth = 895
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    895
    555)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 748
    Top = 216
    Width = 49
    Height = 13
    Anchors = [akTop, akRight]
    Caption = #1050#1086#1083#1086#1085#1082#1072
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 748
    Top = 280
    Width = 48
    Height = 13
    Anchors = [akTop, akRight]
    Caption = #1047#1085#1072#1095#1077#1085#1080#1077
  end
  object Label2: TLabel
    Left = 748
    Top = 328
    Width = 42
    Height = 13
    Anchors = [akTop, akRight]
    Caption = #1057#1090#1088#1086#1082#1072
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 748
    Top = 392
    Width = 48
    Height = 13
    Anchors = [akTop, akRight]
    Caption = #1047#1085#1072#1095#1077#1085#1080#1077
  end
  object btnNewLayout: TButton
    Left = 748
    Top = 21
    Width = 133
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'btnNewLayout'
    TabOrder = 0
    OnClick = btnNewLayoutClick
  end
  object btnAddGrid: TButton
    Left = 748
    Top = 52
    Width = 133
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'btnAddGrid'
    TabOrder = 1
    OnClick = btnAddGridClick
  end
  object btnAddRow: TButton
    Left = 748
    Top = 101
    Width = 133
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'btnAddRow'
    TabOrder = 2
    OnClick = btnAddRowClick
  end
  object btnAddColumn: TButton
    Left = 748
    Top = 132
    Width = 133
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'btnAddColumn'
    TabOrder = 3
    OnClick = btnAddColumnClick
  end
  object ScrollBox1: TScrollBox
    Left = -1
    Top = 0
    Width = 730
    Height = 555
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvSpace
    BorderStyle = bsNone
    Color = clSilver
    ParentColor = False
    TabOrder = 4
  end
  object cbxColumn: TComboBox
    Left = 748
    Top = 240
    Width = 133
    Height = 21
    Anchors = [akTop, akRight]
    ItemIndex = 0
    TabOrder = 5
    Text = '- - -'
    Items.Strings = (
      '- - -'
      #1040#1073#1089#1086#1083#1102#1090#1085#1086#1077
      #1054#1090#1085#1086#1089#1080#1090#1077#1083#1100#1085#1086#1077)
  end
  object edtColumn: TEdit
    Left = 806
    Top = 277
    Width = 59
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 6
    Text = '10'
  end
  object udnColumn: TUpDown
    Left = 865
    Top = 277
    Width = 16
    Height = 21
    Anchors = [akTop, akRight]
    Associate = edtColumn
    Min = 1
    Max = 10000
    Position = 10
    TabOrder = 7
  end
  object cbxRow: TComboBox
    Left = 748
    Top = 352
    Width = 133
    Height = 21
    Anchors = [akTop, akRight]
    ItemIndex = 0
    TabOrder = 8
    Text = '- - -'
    Items.Strings = (
      '- - -'
      #1040#1073#1089#1086#1083#1102#1090#1085#1086#1077
      #1054#1090#1085#1086#1089#1080#1090#1077#1083#1100#1085#1086#1077)
  end
  object edtRow: TEdit
    Left = 806
    Top = 389
    Width = 59
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 9
    Text = '10'
  end
  object udnRow: TUpDown
    Left = 865
    Top = 389
    Width = 16
    Height = 21
    Anchors = [akTop, akRight]
    Associate = edtRow
    Min = 1
    Max = 10000
    Position = 10
    TabOrder = 10
  end
  object btnApply: TButton
    Left = 748
    Top = 445
    Width = 133
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'btnApply'
    TabOrder = 11
    OnClick = btnApplyClick
  end
end
