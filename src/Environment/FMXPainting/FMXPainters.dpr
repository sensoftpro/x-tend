program FMXPainters;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  uConsts in '..\..\Common\uConsts.pas',
  uJSON in '..\..\Common\uJSON.pas',
  uTensor in '..\..\Common\uTensor.pas',
  uChartUtils in '..\..\Common\Scene\uChartUtils.pas',
  uSimpleChart in '..\..\Common\Scene\uSimpleChart.pas',
  uModule in '..\..\Modules\uModule.pas',
  uDrawStyles in '..\..\Modules\Drawing\uDrawStyles.pas',
  uScene in '..\..\Modules\Drawing\uScene.pas',
  uMainForm in 'uMainForm.pas' {fmMain},
  uFMXScene in '..\..\Modules\UI\FMX\uFMXScene.pas',
  uFMXPainter in '..\..\Modules\Drawing\FMX\uFMXPainter.pas';

{$R *.res}

begin
  GlobalUseDirect2D := True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
