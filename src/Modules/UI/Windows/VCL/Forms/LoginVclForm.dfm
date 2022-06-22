object LoginVclFm: TLoginVclFm
  Left = 492
  Top = 360
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'LoginFm'
  ClientHeight = 201
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 16
  object Image1: TImage
    Left = 20
    Top = 21
    Width = 116
    Height = 124
    Transparent = True
  end
  object lblLogin: TLabel
    Left = 151
    Top = 41
    Width = 83
    Height = 16
    Alignment = taRightJustify
    Caption = #1055#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100
  end
  object lblPassword: TLabel
    Left = 191
    Top = 89
    Width = 43
    Height = 16
    Alignment = taRightJustify
    Caption = #1055#1072#1088#1086#1083#1100
  end
  object edLogin: TEdit
    Left = 241
    Top = 38
    Width = 121
    Height = 24
    TabOrder = 0
  end
  object edPass: TMaskEdit
    Left = 241
    Top = 86
    Width = 121
    Height = 24
    PasswordChar = '*'
    TabOrder = 1
    Text = ''
  end
  object edRFID: TEdit
    Left = 241
    Top = 123
    Width = 121
    Height = 24
    TabOrder = 2
    Visible = False
  end
  object OkCancel: TPanel
    Left = 0
    Top = 158
    Width = 386
    Height = 43
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object btnOk: TButton
      AlignWithMargins = True
      Left = 225
      Top = 5
      Width = 73
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alRight
      Caption = #1054#1082
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 308
      Top = 5
      Width = 73
      Height = 33
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alRight
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 1
    end
  end
end
