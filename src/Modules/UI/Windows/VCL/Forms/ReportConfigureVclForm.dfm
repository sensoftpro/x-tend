object ReportConfigureVclFm: TReportConfigureVclFm
  Left = 0
  Top = 0
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1096#1072#1073#1083#1086#1085#1086#1074' '#1086#1090#1095#1077#1090#1086#1074
  ClientHeight = 496
  ClientWidth = 843
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    843
    496)
  PixelsPerInch = 96
  TextHeight = 13
  object btnLoadReport: TButton
    Left = 733
    Top = 20
    Width = 49
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    TabOrder = 0
    OnClick = btnLoadReportClick
  end
  object lbxReportParameters: TListBox
    Left = 496
    Top = 51
    Width = 335
    Height = 174
    Anchors = [akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    OnClick = lbxReportParametersClick
  end
  object redtTemplate: TRichEdit
    Left = 16
    Top = 22
    Width = 465
    Height = 455
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    Zoom = 100
  end
  object cbxTemplates: TComboBox
    Left = 496
    Top = 22
    Width = 237
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 3
  end
  object btnSaveReport: TButton
    Left = 784
    Top = 20
    Width = 47
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save'
    TabOrder = 4
    OnClick = btnSaveReportClick
  end
  object gbxReportParamSettings: TGroupBox
    Left = 496
    Top = 240
    Width = 335
    Height = 237
    Anchors = [akRight, akBottom]
    Caption = ' '#1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1072#1088#1072#1084#1077#1090#1088#1072' '#1086#1090#1095#1077#1090#1072' '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clPurple
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    object lblParameter: TLabel
      Left = 16
      Top = 24
      Width = 77
      Height = 13
      Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblPosition: TLabel
      Left = 16
      Top = 164
      Width = 113
      Height = 13
      Caption = #1055#1086#1079#1080#1094#1080#1103' '#1074' '#1076#1086#1082#1091#1084#1077#1085#1090#1077':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object lblLength: TLabel
      Left = 229
      Top = 164
      Width = 36
      Height = 13
      Caption = #1044#1083#1080#1085#1072':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object mmParameter: TMemo
      Left = 16
      Top = 39
      Width = 301
      Height = 106
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
      OnChange = mmParameterChange
    end
    object edtPosition: TEdit
      Left = 139
      Top = 160
      Width = 62
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
    object edtLength: TEdit
      Left = 271
      Top = 160
      Width = 46
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
    object btnApply: TButton
      Left = 159
      Top = 200
      Width = 75
      Height = 25
      Action = actApply
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
    object btnCancel: TButton
      Left = 240
      Top = 200
      Width = 75
      Height = 25
      Action = actCancel
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
    end
  end
  object alFormActions: TActionList
    OnUpdate = alFormActionsUpdate
    Left = 332
    Top = 236
    object actApply: TAction
      Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
      OnExecute = actApplyExecute
    end
    object actCancel: TAction
      Caption = #1054#1090#1084#1077#1085#1072
      OnExecute = actCancelExecute
    end
  end
end
