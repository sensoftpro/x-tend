﻿program Configurator;

uses
  Dialogs,
  SysUtils,

  //1. Сделать настраиваемые подключаемые стили для Actions
  //3. Сделать редактор лейаутов, привести все названия настроек к единому виду
  //4. Убрать TButtonDesc

  // COMMON
  uConsts in '..\..\Common\uConsts.pas',
  uUtils in '..\..\Common\uUtils.pas',
  uJSON in '..\..\Common\uJSON.pas',
  uFastClasses in '..\..\Common\uFastClasses.pas',
  uTensor in '..\..\Common\uTensor.pas',
  uTask in '..\..\Common\uTask.pas',
  uWebTask in '..\..\Common\uWebTask.pas',
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
  uMigration in '..\..\Platform\Configuration\uMigration.pas',
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
  // UI common
  uPresenter in '..\..\Modules\UI\uPresenter.pas',
  uView in '..\..\Modules\UI\uView.pas',
  uLayout in '..\..\Modules\UI\uLayout.pas',
  uUIBuilder in '..\..\Modules\UI\uUIBuilder.pas',
  uInteractor in '..\..\Modules\UI\uInteractor.pas',
  // Storage common
  uStorage in '..\..\Modules\Storage\uStorage.pas',
  uParameters in '..\..\Modules\Storage\uParameters.pas',
  // Painting common
  uDrawStyles in '..\..\Modules\Drawing\uDrawStyles.pas',
  uScene in '..\..\Modules\Drawing\uScene.pas',
  // Reporting common
  uReport in '..\..\Modules\Reporting\uReport.pas',

  // MODULE IMPLEMENTATIONS

  // UI: Win.DevExpress classes
  uWinVCLPresenter in '..\..\Modules\UI\WinVCL\uWinVCLPresenter.pas',
  uWinScene in '..\..\Modules\UI\WinVCL\uWinScene.pas',
  vclArea in '..\..\Modules\UI\WinVCL\Editors\vclArea.pas',
  vclSimpleEditors in '..\..\Modules\UI\WinVCL\Editors\vclSimpleEditors.pas',
  vclEntityEditors in '..\..\Modules\UI\WinVCL\Editors\vclEntityEditors.pas',
  vclListEditors in '..\..\Modules\UI\WinVCL\Editors\vclListEditors.pas',
  vclBlobEditors in '..\..\Modules\UI\WinVCL\Editors\vclBlobEditors.pas',
  vclPopupForm in '..\..\Modules\UI\WinVCL\Editors\vclPopupForm.pas',
  // ... and forms
  uManagedForm in '..\..\Modules\UI\WinVCL\Forms\uManagedForm.pas',
  StartForm in '..\..\Modules\UI\WinVCL\Forms\StartForm.pas' {StartFm},
  LoginForm in '..\..\Modules\UI\WinVCL\Forms\LoginForm.pas' {LoginFm},
  OptionsForm in '..\..\Modules\UI\WinVCL\Forms\OptionsForm.pas' {OptionsFm},
  TranslationEditForm in '..\..\Modules\UI\WinVCL\Forms\TranslationEditForm.pas' {TranslationEditFm},
  LangEditForm in '..\..\Modules\UI\WinVCL\Forms\LangEditForm.pas' {LangEditFm},
  AboutForm in '..\..\Modules\UI\WinVCL\Forms\AboutForm.pas' {AboutFm},
  ReportConfigureForm in '..\..\Modules\UI\WinVCL\Forms\ReportConfigureForm.pas' {ReportConfigureFm},
  uOAuthForm in '..\..\Modules\UI\WinVCL\Forms\uOAuthForm.pas' {frmOAuth},
  DebugInfoForm in '..\..\Modules\UI\WinVCL\Forms\DebugInfoForm.pas' {DebugFm},
  SplashForm in '..\..\Modules\UI\WinVCL\Forms\SplashForm.pas' {SplashFm},

  //uSampleArea in '..\..\Modules\UI\uSampleArea.pas',
  //uSamplePresenter in '..\..\Modules\UI\uSamplePresenter.pas',

  // Storage: OLEDB
  ADOX_TLB in '..\..\Modules\Storage\OLEDB\ADOX_TLB.pas',
  uDBConnector in '..\..\Modules\Storage\OLEDB\uDBConnector.pas',
  uOLEDBStorage in '..\..\Modules\Storage\OLEDB\uOLEDBStorage.pas',
  // Storage: SQLite
  uSQLite3 in '..\..\Modules\Storage\SQLite\uSQLite3.pas',
  uSQLiteStorage in '..\..\Modules\Storage\SQLite\uSQLiteStorage.pas',

  // Painting: VCL
  uVCLPainter in '..\..\Modules\Drawing\VCL\uVCLPainter.pas',

  // Reporting: FastReport
  uFRReport in '..\..\Modules\Reporting\FastReport\uFRReport.pas',

  // Script modules
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
