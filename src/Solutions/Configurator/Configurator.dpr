﻿program Configurator;

uses
  Dialogs,
  SysUtils,
  uConsts in '..\..\Common\uConsts.pas',
  uUtils in '..\..\Common\uUtils.pas',
  uJSON in '..\..\Common\uJSON.pas',
  uFastClasses in '..\..\Common\uFastClasses.pas',
  uTensor in '..\..\Common\uTensor.pas',
  uTask in '..\..\Common\uTask.pas',
  uWebTask in '..\..\Common\uWebTask.pas',
  uComplexObject in '..\..\Platform\uComplexObject.pas',
  uProcessMediator in '..\..\Platform\Service\uProcessMediator.pas',
  uSettings in '..\..\Platform\uSettings.pas',
  uPlatform in '..\..\Platform\uPlatform.pas',
  uLocalizator in '..\..\Platform\Configuration\uLocalizator.pas',
  uQueryDef in '..\..\Platform\Configuration\uQueryDef.pas',
  uIcon in '..\..\Platform\Configuration\uIcon.pas',
  uReaction in '..\..\Platform\Configuration\uReaction.pas',
  uEnumeration in '..\..\Platform\Configuration\uEnumeration.pas',
  uMigration in '..\..\Platform\Configuration\uMigration.pas',
  uDefinition in '..\..\Platform\Configuration\uDefinition.pas',
  uConfiguration in '..\..\Platform\Configuration\uConfiguration.pas',
  uCodeConfiguration in '..\..\Platform\Configuration\uCodeConfiguration.pas',
  uScheduler in '..\..\Platform\Domain\uScheduler.pas',
  uTranslator in '..\..\Platform\Domain\uTranslator.pas',
  uQuery in '..\..\Platform\Domain\uQuery.pas',
  uReport in '..\..\Platform\Domain\uReport.pas',
  uDomainUtils in '..\..\Platform\Domain\uDomainUtils.pas',
  uSimpleField in '..\..\Platform\Domain\uSimpleField.pas',
  uObjectField in '..\..\Platform\Domain\uObjectField.pas',
  uSession in '..\..\Platform\Domain\uSession.pas',
  uChangeManager in '..\..\Platform\Domain\uChangeManager.pas',
  uEntityList in '..\..\Platform\Domain\uEntityList.pas',
  uEntity in '..\..\Platform\Domain\uEntity.pas',
  uCollection in '..\..\Platform\Domain\uCollection.pas',
  uDomain in '..\..\Platform\Domain\uDomain.pas',
  uModule in '..\..\Platform\uModule.pas',
  uLogger in '..\..\Platform\Service\uLogger.pas',
  uStorage in '..\..\Platform\Service\uStorage.pas',
  uDrawStyles in '..\..\Common\uDrawStyles.pas',
  uView in '..\..\Platform\uView.pas',
  uUIBuilder in '..\..\Platform\uUIBuilder.pas',
  uInteractor in '..\..\Platform\uInteractor.pas',
  uPresenter in '..\..\Platform\uPresenter.pas',
  uScript in '..\..\Platform\uScript.pas',
  uCodeParser in '..\..\common\uCodeParser.pas',
  uScriptExecutor in '..\..\Platform\Service\uScriptExecutor.pas',
  uChartData in '..\..\Common\Scene\uChartData.pas',
  uChartUtils in '..\..\Common\Scene\uChartUtils.pas',
  uScene in '..\..\Common\Scene\uScene.pas',
  uSimpleChart in '..\..\Common\Scene\uSimpleChart.pas',
  ADOX_TLB in '..\..\Modules\Storage\OLEDB\ADOX_TLB.pas',
  uParameters in '..\..\Modules\Storage\OLEDB\uParameters.pas',
  uDBConnector in '..\..\Modules\Storage\OLEDB\uDBConnector.pas',
  uOLEDBStorage in '..\..\Modules\Storage\OLEDB\uOLEDBStorage.pas',
  uFRReport in '..\..\Modules\Reporting\FastReport\uFRReport.pas',
  uGDIPlusPainter in '..\..\Modules\Drawing\GDIPlus\uGDIPlusPainter.pas',
  uWinVCLPresenter in '..\..\Modules\UI\WinVCL\uWinVCLPresenter.pas',
  uWinScene in '..\..\Modules\UI\WinVCL\uWinScene.pas',
  vclArea in '..\..\Modules\UI\WinVCL\Editors\vclArea.pas',
  vclSimpleEditors in '..\..\Modules\UI\WinVCL\Editors\vclSimpleEditors.pas',
  vclEntityEditors in '..\..\Modules\UI\WinVCL\Editors\vclEntityEditors.pas',
  vclListEditors in '..\..\Modules\UI\WinVCL\Editors\vclListEditors.pas',
  vclBlobEditors in '..\..\Modules\UI\WinVCL\Editors\vclBlobEditors.pas',
  vclPopupForm in '..\..\Modules\UI\WinVCL\Editors\vclPopupForm.pas',
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
  OkCancelFrame in '..\..\Modules\UI\WinVCL\Forms\OkCancelFrame.pas' {OkCancelFrm: TFrame},
  SplashForm in '..\..\Modules\UI\WinVCL\Forms\SplashForm.pas' {SplashFm},
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
