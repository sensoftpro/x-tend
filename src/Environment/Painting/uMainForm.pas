unit uMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uWinScene, uSimpleChart, Vcl.StdCtrls, uDrawStyles, Vcl.ExtCtrls, uScene, Vcl.ExtDlgs;

type
  TfmMain = class(TForm)
    pnlTop: TPanel;
    pnlChart: TPanel;
    rgrPainter: TRadioGroup;
    btnCreate: TButton;
    btnMeasure: TButton;
    lblFPS: TLabel;
    btnSave: TButton;
    sdgSave: TSavePictureDialog;
    procedure FormDestroy(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure btnMeasureClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FScene: TScene;
    FChart: TSimpleChart;
    procedure Measure;
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

uses
  UITypes, Types, uVCLPainter, uGDIPlusPainter, uDirect2DPainter, uSkiaPainter;

{$R *.dfm}

//  Создать битмап с размерами ARect
function CreateBmpRect(const ARect : TRect;
  const APixelFormat: TPixelFormat = pfCustom) : TBitmap;
begin
  result := TBitmap.Create;
  result.Width  := abs(ARect.Width);
  result.Height := abs(ARect.Height);
  if APixelFormat <> pfCustom then
    Result.PixelFormat := APixelFormat;
end;

procedure TfmMain.btnCreateClick(Sender: TObject);
begin
  if Assigned(FScene) then
    FreeAndNil(FScene);

  case rgrPainter.ItemIndex of
    0: FScene := TVCLScene.Create(pnlChart);
    1: FScene := TGDIPlusScene.Create(pnlChart);
    2: FScene := TDirect2DScene.Create(pnlChart);
    3: FScene := TSkiaScene.Create(pnlChart);
  else
    FScene := nil;
  end;

  if Assigned(FScene) then
    FChart := TDataChart.Create(FScene, nil);
    //FAliveRect := TAliveRect.Create(FScene, nil, RectF(100, 100, 500, 300), TAlphaColorRec.Blueviolet, TAlphaColorRec.Blue, 1.2);
end;

procedure TfmMain.btnMeasureClick(Sender: TObject);
begin
  Measure;
end;

procedure TfmMain.btnSaveClick(Sender: TObject);
begin
  if sdgSave.Execute then
    FScene.SaveToFile(sdgSave.FileName);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FScene) then
    FreeAndNil(FScene);
end;

procedure TfmMain.Measure;
var
  vStart, vStop, vFreq: Int64;
  i: Integer;
const
  cRefreshCount = 200;
begin
  if Assigned(FScene) then
  begin
    QueryPerformanceFrequency(vFreq);
    QueryPerformanceCounter(vStart);

    for i := 1 to cRefreshCount do
      FScene.FullRefresh;

    QueryPerformanceCounter(vStop);
    lblFPS.Caption := FormatFloat('###,##0.###', 1000 * (vStop - vStart) / vFreq / cRefreshCount) + ' мcек';
  end
  else
    lblFPS.Caption := '< нет данных >';
end;

end.
