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
  uOpenGLPainter in '..\..\Modules\Drawing\OpenGL\uOpenGLPainter.pas',
  dglOpenGL in '..\..\Modules\Drawing\OpenGL\dglOpenGL.pas',
  Skia.Api in '..\..\Modules\Drawing\Skia\Skia.Api.pas',
  Skia in '..\..\Modules\Drawing\Skia\Skia.pas',
  uSkiaPainter in '..\..\Modules\Drawing\Skia\uSkiaPainter.pas',
  uMainForm in 'uMainForm.pas' {fmMain},
  uFreeType in '..\..\Modules\Drawing\OpenGL\uFreeType.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
