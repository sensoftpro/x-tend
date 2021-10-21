program Configurator;

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

{$I ..\..\Modules\UI\WinVCL\files.inc}
{$I ..\..\Modules\UI\Web\uniGUI\files.inc}

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
