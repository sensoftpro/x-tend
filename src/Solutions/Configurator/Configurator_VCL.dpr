﻿program Configurator_VCL;

{$IFOPT D-}{$WEAKLINKRTTI ON}{$ENDIF}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
  Vcl.Dialogs,
  SysUtils,

  // COMMON
  uConsts in '..\..\Common\uConsts.pas',
  uUtils in '..\..\Common\uUtils.pas',
  uJSON in '..\..\Common\uJSON.pas',
  uFastClasses in '..\..\Common\uFastClasses.pas',
  uTensor in '..\..\Common\uTensor.pas',
  uTask in '..\..\Common\uTask.pas',
  uNet in '..\..\Common\uNet.pas',
  uComplexObject in '..\..\Platform\uComplexObject.pas',
  uSettings in '..\..\Platform\uSettings.pas',
  uChartData in '..\..\Common\Scene\uChartData.pas',
  uChartUtils in '..\..\Common\Scene\uChartUtils.pas',
  uSimpleChart in '..\..\Common\Scene\uSimpleChart.pas',
  uPlatform in '..\..\Platform\uPlatform.pas',

  // CONFIGURATION
  uLocalizator in '..\..\Platform\Configuration\uLocalizator.pas',
  uQueryDef in '..\..\Platform\Configuration\uQueryDef.pas',
  uIcon in '..\..\Platform\Configuration\uIcon.pas',
  uReaction in '..\..\Platform\Configuration\uReaction.pas',
  uEnumeration in '..\..\Platform\Configuration\uEnumeration.pas',
  uDefinition in '..\..\Platform\Configuration\uDefinition.pas',
  uConfiguration in '..\..\Platform\Configuration\uConfiguration.pas',
  uCodeConfiguration in '..\..\Platform\Configuration\uCodeConfiguration.pas',

  // RUNTIME
  uScheduler in '..\..\Platform\Domain\uScheduler.pas',
  uTranslator in '..\..\Platform\Domain\uTranslator.pas',
  uQuery in '..\..\Platform\Domain\uQuery.pas',
  uDomainUtils in '..\..\Platform\Domain\uDomainUtils.pas',
  uSimpleField in '..\..\Platform\Domain\uSimpleField.pas',
  uObjectField in '..\..\Platform\Domain\uObjectField.pas',
  uSession in '..\..\Platform\Domain\uSession.pas',
  uChangeManager in '..\..\Platform\Domain\uChangeManager.pas',
  uEntityList in '..\..\Platform\Domain\uEntityList.pas',
  uEntity in '..\..\Platform\Domain\uEntity.pas',
  uCollection in '..\..\Platform\Domain\uCollection.pas',
  uDomain in '..\..\Platform\Domain\uDomain.pas',

  // MODULE INTERFACES
  uModule in '..\..\Modules\uModule.pas',
  uLogger in '..\..\Platform\uLogger.pas',
  uCodeParser in '..\..\Common\uCodeParser.pas',
  uScriptExecutor in '..\..\Platform\uScriptExecutor.pas',
  uScript in '..\..\Platform\uScript.pas',
  uPresenter in '..\..\Modules\UI\uPresenter.pas',
  uView in '..\..\Modules\UI\uView.pas',
  uUIBuilder in '..\..\Modules\UI\uUIBuilder.pas',
  uInteractor in '..\..\Modules\UI\uInteractor.pas',
  uLayout in '..\..\Modules\UI\uLayout.pas',
  // Storage common
  uStorage in '..\..\Modules\Storage\uStorage.pas',
  uParameters in '..\..\Modules\Storage\uParameters.pas',
  // Painting common
  uDrawStyles in '..\..\Modules\Drawing\uDrawStyles.pas',
  uScene in '..\..\Modules\Drawing\uScene.pas',
  // Reporting common
  uReport in '..\..\Modules\Reporting\uReport.pas',

  // UI: WinVCL
  uWinVCLPresenter in '..\..\Modules\UI\WinVCL\uWinVCLPresenter.pas',
  vclScene in '..\..\Modules\UI\WinVCL\vclScene.pas',
  vclArea in '..\..\Modules\UI\WinVCL\vclArea.pas',
  vclSimpleEditors in '..\..\Modules\UI\WinVCL\vclSimpleEditors.pas',
  vclEntityEditors in '..\..\Modules\UI\WinVCL\vclEntityEditors.pas',
  vclBlobEditors in '..\..\Modules\UI\WinVCL\vclBlobEditors.pas',
  vclListEditors in '..\..\Modules\UI\WinVCL\vclListEditors.pas',
  DebugInfoForm in '..\..\Modules\UI\WinVCL\DebugInfoForm.pas' {DebugFm},
  // UI: DevExpress
  uDevExpressPresenter in '..\..\Modules\UI\WinVCL\DevExpress\uDevExpressPresenter.pas',
  dexArea in '..\..\Modules\UI\WinVCL\DevExpress\dexArea.pas',
  dexSimpleEditors in '..\..\Modules\UI\WinVCL\DevExpress\dexSimpleEditors.pas',
  dexEntityEditors in '..\..\Modules\UI\WinVCL\DevExpress\dexEntityEditors.pas',
  dexListEditors in '..\..\Modules\UI\WinVCL\DevExpress\dexListEditors.pas',

  // Painters: VCL
  uCommonVCLPainter in '..\..\Modules\Drawing\VCL\uCommonVCLPainter.pas',
  uVCLPainter in '..\..\Modules\Drawing\VCL\uVCLPainter.pas',

  // Reports: FastReports
  //uFRReport in '..\..\Modules\Reporting\FastReport\uFRReport.pas',

  // Storage: SQLite
  uSQLiteStorage in '..\..\Modules\Storage\SQLite\uSQLiteStorage.pas',

  uConfiguratorScript in 'uConfiguratorScript.pas';

{$R *.res}

begin
  try
    TPlatform.Run;
  except
    on E: Exception do
      ShowMessage('Ошибка старта приложения: ' + E.Message);
  end;
end.
