program Painters;

uses
  Vcl.Forms,
  uConsts in '..\..\Common\uConsts.pas',
  uJSON in '..\..\Common\uJSON.pas',
  uTensor in '..\..\Common\uTensor.pas',
  uChartUtils in '..\..\Common\Scene\uChartUtils.pas',
  uSimpleChart in '..\..\Common\Scene\uSimpleChart.pas',
  uModule in '..\..\Modules\uModule.pas',
  uDrawStyles in '..\..\Modules\Drawing\uDrawStyles.pas',
  uScene in '..\..\Modules\Drawing\uScene.pas',
  uWinScene in '..\..\Modules\UI\WinVCL\uWinScene.pas',

{$I ..\..\Modules\Drawing\VCL\files.inc}
{$I ..\..\Modules\Drawing\GDIPlus\files.inc}
{$I ..\..\Modules\Drawing\Direct2D\files.inc}
{$I ..\..\Modules\Drawing\OpenGL\files.inc}
{$I ..\..\Modules\Drawing\Skia\files.inc}

  uMainForm in 'uMainForm.pas' {fmMain};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
