﻿program Configurator;

{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
  uConsts in '..\..\..\Common\uConsts.pas',
  uUtils in '..\..\..\Common\uUtils.pas',
  uJSON in '..\..\..\Common\uJSON.pas',
  uFastClasses in '..\..\..\Common\uFastClasses.pas',
  uTensor in '..\..\..\Common\uTensor.pas',
  uTask in '..\..\..\Common\uTask.pas',
  uWebTask in '..\..\..\Common\uWebTask.pas',
  uComplexObject in '..\..\..\Platform\uComplexObject.pas',
  uSettings in '..\..\..\Platform\uSettings.pas',
  uChartData in '..\..\..\Common\Scene\uChartData.pas',
  uChartUtils in '..\..\..\Common\Scene\uChartUtils.pas',
  uSimpleChart in '..\..\..\Common\Scene\uSimpleChart.pas',
  uPlatform in '..\..\..\Platform\uPlatform.pas',
  uLocalizator in '..\..\..\Platform\Configuration\uLocalizator.pas',
  uQueryDef in '..\..\..\Platform\Configuration\uQueryDef.pas',
  uIcon in '..\..\..\Platform\Configuration\uIcon.pas',
  uReaction in '..\..\..\Platform\Configuration\uReaction.pas',
  uEnumeration in '..\..\..\Platform\Configuration\uEnumeration.pas',
  uDefinition in '..\..\..\Platform\Configuration\uDefinition.pas',
  uConfiguration in '..\..\..\Platform\Configuration\uConfiguration.pas',
  uCodeConfiguration in '..\..\..\Platform\Configuration\uCodeConfiguration.pas',
  uScheduler in '..\..\..\Platform\Domain\uScheduler.pas',
  uTranslator in '..\..\..\Platform\Domain\uTranslator.pas',
  uQuery in '..\..\..\Platform\Domain\uQuery.pas',
  uDomainUtils in '..\..\..\Platform\Domain\uDomainUtils.pas',
  uSimpleField in '..\..\..\Platform\Domain\uSimpleField.pas',
  uObjectField in '..\..\..\Platform\Domain\uObjectField.pas',
  uSession in '..\..\..\Platform\Domain\uSession.pas',
  uChangeManager in '..\..\..\Platform\Domain\uChangeManager.pas',
  uEntityList in '..\..\..\Platform\Domain\uEntityList.pas',
  uEntity in '..\..\..\Platform\Domain\uEntity.pas',
  uCollection in '..\..\..\Platform\Domain\uCollection.pas',
  uDomain in '..\..\..\Platform\Domain\uDomain.pas',
  uModule in '..\..\..\Modules\uModule.pas',
  uLogger in '..\..\..\Platform\uLogger.pas',
  uCodeParser in '..\..\..\Common\uCodeParser.pas',
  uScriptExecutor in '..\..\..\Platform\uScriptExecutor.pas',
  uScript in '..\..\..\Platform\uScript.pas',
  uPresenter in '..\..\..\Modules\UI\uPresenter.pas',
  uView in '..\..\..\Modules\UI\uView.pas',
  uUIBuilder in '..\..\..\Modules\UI\uUIBuilder.pas',
  uInteractor in '..\..\..\Modules\UI\uInteractor.pas',
  uLayout in '..\..\..\Modules\UI\uLayout.pas',
  uStorage in '..\..\..\Modules\Storage\uStorage.pas',
  uParameters in '..\..\..\Modules\Storage\uParameters.pas',
  uDrawStyles in '..\..\..\Modules\Drawing\uDrawStyles.pas',
  uScene in '..\..\..\Modules\Drawing\uScene.pas',
  uReport in '..\..\..\Modules\Reporting\uReport.pas',
  uFGXPresenter in '..\..\..\Modules\UI\FGXNative\uFGXPresenter.pas',
  fgxArea in '..\..\..\Modules\UI\FGXNative\fgxArea.pas',
  uSQLiteStorage in '..\..\..\Modules\Storage\SQLite\uSQLiteStorage.pas',
  uConfiguratorScript in '..\uConfiguratorScript.pas',
  Assets.Consts in 'Assets.Consts.pas';

{$R *.res}

begin
  try
    TPlatform.Run;
  except
  end;
end.
