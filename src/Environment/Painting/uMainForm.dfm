object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = #1056#1077#1085#1076#1077#1088#1080#1085#1075' '#1089#1094#1077#1085#1099
  ClientHeight = 580
  ClientWidth = 953
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 953
    Height = 65
    Align = alTop
    TabOrder = 0
    object lblFPS: TLabel
      Left = 692
      Top = 16
      Width = 129
      Height = 23
      Caption = '<'#1085#1077#1090' '#1076#1072#1085#1085#1099#1093'>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHotLight
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object rgrPainter: TRadioGroup
      Left = 12
      Top = 14
      Width = 397
      Height = 45
      Caption = #1054#1090#1088#1080#1089#1086#1074#1082#1072
      Columns = 5
      Items.Strings = (
        'GDI'
        'GDI+'
        'DirectX'
        'Skia'
        'OpenGL')
      TabOrder = 0
      WordWrap = True
    end
    object btnCreate: TButton
      Left = 464
      Top = 19
      Width = 93
      Height = 25
      Caption = 'Create'
      TabOrder = 1
      OnClick = btnCreateClick
    end
    object btnMeasure: TButton
      Left = 576
      Top = 21
      Width = 101
      Height = 25
      Caption = 'Measure'
      TabOrder = 2
      OnClick = btnMeasureClick
    end
    object btnSave: TButton
      Left = 848
      Top = 19
      Width = 93
      Height = 25
      Caption = 'Save'
      TabOrder = 3
      OnClick = btnSaveClick
    end
  end
  object pnlChart: TPanel
    Left = 0
    Top = 65
    Width = 953
    Height = 515
    Align = alClient
    TabOrder = 1
  end
  object sdgSave: TSavePictureDialog
    Filter = 
      'All (*.bmp;*.jpg;*.jpeg;*.gif;*.png;*.ico;*.wmf;*.tif;*.tiff)|*.' +
      'bmp;*.jpg;*.jpeg;*.gif;*.png;*.ico;*.wmf;*.tif;*.tiff|Bitmaps (*' +
      '.bmp)|*.bmp|JPEG Images (*.jpg)|*.jpg|JPEG Images (*.jpeg)|*.jpe' +
      'g|GIF Images (*.gif)|*.gif|PNG Images (*.png)|*.png|Icons (*.ico' +
      ')|*.ico|Metafiles (*.wmf)|*.wmf|TIFF Images (*.tif)|*.tif|TIFF I' +
      'mages (*.tiff)|*.tiff'
    Left = 836
    Top = 149
  end
end
