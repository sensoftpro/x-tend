unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, uScene, uSimpleChart, Diagnostics,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, uFMXScene,
  FMX.Objects;

type
  TfmMain = class(TForm)
    pnlTop: TPanel;
    btnCreate: TButton;
    rbSkia: TRadioButton;
    rbOpenGL: TRadioButton;
    rbGDI: TRadioButton;
    grpPainters: TGroupBox;
    btnMeasure: TButton;
    lblFps: TLabel;
    pbxCanvas: TPaintBox;
    procedure btnCreateClick(Sender: TObject);
    procedure btnMeasureClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FScene: TScene;
    FChart: TSimpleChart;
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.fmx}

procedure TfmMain.btnCreateClick(Sender: TObject);
begin
  if Assigned(FScene) then
    FreeAndNil(FScene);

  FScene := TFMXScene.Create(pbxCanvas);

  if Assigned(FScene) then
    FChart := TDataChart.Create(FScene, nil);
end;

procedure TfmMain.btnMeasureClick(Sender: TObject);
var
  vStartWatch, vStopWatch: TStopwatch;
  i: Integer;
const
  cRefreshCount = 200;
begin
  if Assigned(FScene) then
  begin
    vStartWatch.StartNew;

    for i := 1 to cRefreshCount do
      FScene.FullRefresh;

    vStopWatch.StartNew;
    lblFPS.Text := FormatFloat('###,##0.###', (vStartWatch.ElapsedMilliseconds - vStopWatch.ElapsedMilliseconds) / cRefreshCount) + ' мcек';
    vStartWatch.Reset;
    vStopWatch.Reset;
  end
  else
    lblFPS.Text := '< нет данных >';
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FScene);
end;

end.
