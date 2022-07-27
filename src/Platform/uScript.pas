{---------------------------------------------------------------------------------
  X-Tend runtime

  Contributors:
    Vladimir Kustikov (kustikov@sensoft.pro)
    Sergey Arlamenkov (arlamenkov@sensoft.pro)

  You may retrieve the latest version of this file at the GitHub,
  located at https://github.com/sensoftpro/x-tend.git
 ---------------------------------------------------------------------------------
  MIT License

  Copyright © 2021 Sensoft

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 ---------------------------------------------------------------------------------}

unit uScript;

interface

uses
  Classes, SysUtils, UITypes, Generics.Collections, uFastClasses, uCollection, uEntity, uReport, uSession, uDefinition,
  uScheduler, uConsts, uEnumeration, uMigration, uChangeManager, uComplexObject, uView, uInteractor, uTask;

type
  TInclusion = class;
  TScriptClass = class of TScript;
  TInclusionClass = class of TInclusion;

  TScriptInfo = record
    ScriptClass: TScriptClass;
    Includes: string;
  end;

  TBaseScript = class abstract
  private
    function GetVersionName: string;
  protected
    [Weak] FConfiguration: TObject;
    FAppTitle: string;
    FVersion: string;
    FVersionName: string;

    constructor Create(const AConfiguration: TObject); virtual;

    function ActionByName(const AName: string): TActionDef;
    function DefinitionByName(const AName: string): TDefinition;
    function AddDefinition(const AID: Integer; const AName, AAncestors, ACaption, AEmptyValue: string;
      const AFlags: Integer = 0; const AKind: TCollectionKind = clkLibrary): TDefinition; overload;
    function AddDefinition(const AName, AAncestors, ACaption, AEmptyValue: string;
      const AFlags: Integer = 0; const AKind: TCollectionKind = clkLibrary): TDefinition; overload;
    function AddAction(const AName, ACaption: string; const AImageID: Integer; const AFlags: Integer = 0): TActionDef;
    function AddEnumeration<T>(const AName: string): TEnumeration;
    function AddStateMachine<T>(const AName: string): TStateMachine;
    function AddMigration(const AVersion: string): TMigration;
    function RegisterComplexClass(const AName: string; const AComplexClass: TComplexClass): TComplexClassDef;
    procedure RegisterReaction(const ADefinitionNames, AReactiveFields, AFieldChain: string; const AReactionProc: TReactionProc);
    procedure AddPlannedJob(const AName: string; const APeriod: Integer; const AHandler: TSchedulerProc;
      const ARefFieldName: string = ''; const ASnapToPeriod: Boolean = False);
    procedure AddSecuritySettings(const AHolder: TChangeHolder; const ACollection: TCollection;
      const ASubject: string; const AObject: string; const AStateName: string; const AFlags: Integer);

    function GetParentListView(const AView: TView): TView;
  protected
    procedure DoInit; virtual; abstract;
    procedure DoDeinit; virtual;

    procedure DoCreateDefinitions; virtual;
    procedure DoCreateMigrations; virtual;
    procedure DoCreateDefaultEntities(const ADomain: TObject; const AHolder: TChangeHolder); virtual;
    function GetFullText(const AEntity: TEntity; var AHandled: Boolean): string; virtual;
    function DoCheckField(const AEntity: TEntity; const AFieldName: string; var AHandled: Boolean): Boolean; virtual;
    procedure DoAfterEntityCreation(const AHolder: TChangeHolder; const AOwnerContext: TObject; const AEntity: TEntity); virtual;
    procedure DoBeforeEntitySaving(const AHolder: TChangeHolder; const AEntity: TEntity); virtual;
    procedure DoAfterEntitySaved(const AHolder: TChangeHolder; const AEntity: TEntity); virtual;
    procedure DoBeforeEntityRemoving(const AHolder: TChangeHolder; const AEntity: TEntity); virtual;
    function DoGetReportValue(const AReportData: TReportData;
      const AParamName: string; const AIndex1, AIndex2: Integer; var AHandled: Boolean): TReportValue; virtual;
    function DoGetParamValue(const AEntity: TEntity; const AParamName: string; var AHandled: Boolean): string; virtual;
    procedure DoOnDomainReady(const ADomain: TObject); virtual;
    procedure DoOnDomainStopped; virtual;
    procedure DoOnLogined(const AInteractor: TInteractor); virtual;

    function CheckCanChangeField(const AView: TView; const AEntity: TEntity; const AFieldName: string;
      const ANewValue: Variant; var AHandled: Boolean): Boolean; virtual;
    procedure DoActualizeData(const ADomain: TObject); virtual;
    function DoCalculateStyle(const AViewName: string; const AEntity: TEntity;
      var AHandled: Boolean): TColor; virtual;
    function DoCheckActionFlags(const AView: TView; const AActionName: string;
      const AContext: TObject; const AParams: TEntity): TViewState; virtual;
    procedure DoFillActionParams(const AView: TView; const AActionName: string; const AContext: TObject;
      const AParams: TEntity); virtual;
    function DoExecuteAction(const AView: TView; const AActionName: string;
      const AContext: TObject; const AParams: TEntity; const AParentHolder: TChangeHolder): Boolean; virtual;
    function DoExecuteCommand(const AExecutor: TObject; const ATask: TTaskHandle;
      const AFiber, ACommand: TObject): Boolean; virtual;
  public
    destructor Destroy; override;

    property AppTitle: string read FAppTitle;
    // Version текстовый номер версии с разделением точкой, используется для сравнения версии по порядку (чтобы старый ехе не смог стартовать на новой версии хранилища)
    property Version: string read FVersion;
    // VersionName полное текстовое представление версии продукта, может быть использован любой текст, по умолчанию будет равно Version, имеет приоритет над FVersion
    property VersionName: string read GetVersionName;
  end;

  TScript = class(TBaseScript)
  private
    class var RegisteredScripts: TStringDictionary<TScriptInfo>;
  private
    function InternalExecuteAction(const AView: TView; const AActionName: string;
      const AContext: TObject; const AParams: TEntity; const AParentHolder: TChangeHolder): Boolean;
    function InternalCheckActionFlags(const AView: TView; const AActionName: string;
      const AContext: TObject; const AParams: TEntity): TViewState;
    procedure FillActionParams(const AView: TView; const AActionName: string; const AContext: TObject;
      const AParams: TEntity);
  protected
    FInclusions: TObjectList<TInclusion>;
    function DoExecuteDefaultAction(const ASession: TUserSession; const AParams: string): Boolean; virtual;
    function DoBeforeUIClosing(const AInteractor: TInteractor): Boolean; virtual;
  protected
    constructor Create(const AConfiguration: TObject); override;
  public
    class function GetRegisteredScripts: TStrings;
    class function ScriptExists(const AName: string): Boolean;
    class function AddScript(const AConfiguration: TObject): TScript;
    destructor Destroy; override;

    // Рабочая процедура
    function CreateConfiguration: string;
    procedure CreateDefaultEntities(const ADomain: TObject);
    procedure DomainReady(const ADomain: TObject);
    procedure DomainStopped;
    procedure Logined(const AInteractor: TInteractor);

    // Калькуляции (анонимные функции у коллекций)
    function FullText(const AEntity: TEntity): string;

    // Обновления флагов видимости
    function CheckField(const AEntity: TEntity; const AFieldName: string): Boolean;

    procedure ActualizeData(const ADomain: TObject);
    function CalculateStyle(const AViewName: string; const AEntity: TEntity): TColor;
    function CanChangeField(const AView: TView; const AEntity: TEntity;
      const AFieldName: string; const ANewValue: Variant): Boolean;
    function BeforeUIClosing(const AInteractor: TInteractor): Boolean;
    function CheckActionFlags(const AView: TView): TViewState;
    function ExecuteAction(const AView: TView; const AParentHolder: TChangeHolder): Boolean;
    function ExecuteCommand(const AExecutor: TObject; const ATask: TTaskHandle; const AFiber, ACommand: TObject): Boolean;
    procedure CreateContentTypeChangeHandler(const ADefinition: TDefinition; const ATargetFieldName, AContentTypePath: string);

    // Триггеры-обработчики начала/окончания действий
    procedure AfterEntityCreation(const AHolder: TChangeHolder; const AOwnerContext: TObject; const AEntity: TEntity);
    procedure BeforeEntitySaving(const AHolder: TChangeHolder; const AEntity: TEntity);
    procedure AfterEntitySaved(const AHolder: TChangeHolder; const AEntity: TEntity);
    procedure BeforeEntityRemoving(const AHolder: TChangeHolder; const AEntity: TEntity);

    // Работа с отчетами и действиями
    function GetReportValue(const AReportData: TReportData;
      const AParamName: string; const AIndex1, AIndex2: Integer): TReportValue;
    function GetParamValue(const AEntity: TEntity; const AParamName: string): string;
    function ExecuteDefaultAction(const ASession: TUserSession; const AParams: string): Boolean;
  end;

  TInclusion = class(TBaseScript)
  private
    class var RegisteredInclusions: TStringDictionary<TInclusionClass>;
  protected
    procedure DoInit; override;
  end;

procedure RegisterScript(const AConfigurationName: string; const AClass: TScriptClass; const AIncludes: string = '');
procedure RegisterInclusion(const AInclusionName: string; const AClass: TInclusionClass);

implementation

uses
  Variants, IOUtils, Math, uConfiguration, uDomain, uObjectField, uEntityList, uJSON, uUtils,
  uDomainUtils, uReaction, uPresenter, idUri;

procedure RegisterScript(const AConfigurationName: string; const AClass: TScriptClass; const AIncludes: string = '');
var
  vScriptInfo: TScriptInfo;
begin
  vScriptInfo.ScriptClass := AClass;
  vScriptInfo.Includes := AIncludes;
  TScript.RegisteredScripts.AddObject(AConfigurationName, vScriptInfo);
end;

procedure RegisterInclusion(const AInclusionName: string; const AClass: TInclusionClass);
begin
  TInclusion.RegisteredInclusions.AddObject(AInclusionName, AClass);
end;

{ TScript }

procedure TScript.ActualizeData(const ADomain: TObject);
var
  vInclusion: TInclusion;
begin
  for vInclusion in FInclusions do
    vInclusion.DoActualizeData(ADomain);
  DoActualizeData(ADomain);
end;

class function TScript.AddScript(const AConfiguration: TObject): TScript;
var
  vName: string;
  vScriptInfo: TScriptInfo;
  vIncludes: TStrings;
  vIncludeName: string;
  vInclusionClass: TInclusionClass;
begin
  vName := TConfiguration(AConfiguration).Name;
  if ScriptExists(vName) then
  begin
    vScriptInfo := RegisteredScripts.ObjectByName(vName);
    Result := vScriptInfo.ScriptClass.Create(AConfiguration);
    if vScriptInfo.Includes <> '' then
    begin
      vIncludes := CreateDelimitedList(vScriptInfo.Includes);
      try
        for vIncludeName in vIncludes do
        begin
          vInclusionClass := TInclusion.RegisteredInclusions.ObjectByName(Trim(vIncludeName));
          if Assigned(vInclusionClass) then
          begin
            Result.FInclusions.Add(vInclusionClass.Create(AConfiguration));
            TConfiguration(AConfiguration).AddInclusion(Trim(vIncludeName));
          end
          else
            Assert(False, Format('There is no module [%s] for configuration [%s]', [vIncludeName, vName]));
        end;
      finally
        FreeAndNil(vIncludes);
      end;
    end;
  end
  else begin
    Result := nil;
    Assert(False, Format('There is no script for configuration [%s]', [vName]));
  end;
end;

procedure TScript.AfterEntityCreation(const AHolder: TChangeHolder; const AOwnerContext: TObject; const AEntity: TEntity);
var
  vInclusion: TInclusion;
begin
  if not Assigned(AEntity) then
    Exit;
  if not Assigned(AEntity.Collection) then
    Exit;

  for vInclusion in FInclusions do
    vInclusion.DoAfterEntityCreation(AHolder, AOwnerContext, AEntity);
  DoAfterEntityCreation(AHolder, AOwnerContext, AEntity);
end;

procedure TScript.AfterEntitySaved(const AHolder: TChangeHolder; const AEntity: TEntity);
var
  vInclusion: TInclusion;
begin
  if not Assigned(AEntity) then
    Exit;
  if AEntity.CollectionName = '' then
    Exit;

  for vInclusion in FInclusions do
    vInclusion.DoAfterEntitySaved(AHolder, AEntity);
  DoAfterEntitySaved(AHolder, AEntity);
end;

procedure TScript.BeforeEntityRemoving(const AHolder: TChangeHolder; const AEntity: TEntity);
var
  vInclusion: TInclusion;
begin
  if not Assigned(AEntity) then
    Exit;
  if AEntity.CollectionName = '' then
    Exit;

  for vInclusion in FInclusions do
    vInclusion.DoBeforeEntityRemoving(AHolder, AEntity);
  DoBeforeEntityRemoving(AHolder, AEntity);
end;

procedure TScript.BeforeEntitySaving(const AHolder: TChangeHolder; const AEntity: TEntity);
var
  vInclusion: TInclusion;
begin
  if not Assigned(AEntity) then
    Exit;
  if AEntity.CollectionName = '' then
    Exit;

  for vInclusion in FInclusions do
    vInclusion.DoBeforeEntitySaving(AHolder, AEntity);
  DoBeforeEntitySaving(AHolder, AEntity);
end;

function TScript.BeforeUIClosing(const AInteractor: TInteractor): Boolean;
begin
  Result := DoBeforeUIClosing(AInteractor);
end;

function TScript.CalculateStyle(const AViewName: string; const AEntity: TEntity): TColor;
var
  vHandled: Boolean;
  vInclusion: TInclusion;
begin
  for vInclusion in FInclusions do
  begin
    vHandled := True;
    Result := vInclusion.DoCalculateStyle(AViewName, AEntity, vHandled);
    if vHandled then
      Exit;
  end;

  vHandled := True;
  Result := DoCalculateStyle(AViewName, AEntity, vHandled);
  if not vHandled then
    Result := TColorRec.Black;
end;

function TScript.CanChangeField(const AView: TView; const AEntity: TEntity;
  const AFieldName: string; const ANewValue: Variant): Boolean;
var
  vHandled: Boolean;
  vInclusion: TInclusion;
begin
  Result := True;
  if not Assigned(AEntity) then
    Exit;
  if AEntity.CollectionName = '' then
    Exit;

  for vInclusion in FInclusions do
  begin
    vHandled := True;
    Result := vInclusion.CheckCanChangeField(AView, AEntity, AFieldName, ANewValue, vHandled);
    if vHandled then
      Exit;
  end;

  vHandled := True;
  Result := CheckCanChangeField(AView, AEntity, AFieldName, ANewValue, vHandled);
  if not vHandled then
    Result := True;
end;

function TScript.CheckActionFlags(const AView: TView): TViewState;
var
  vAction: TActionDef;
  vActionName: string;
  vContextView: TView;
  vContext: TObject;
  vListObject: TEntityList;
  vSelectedItem: TObject;
  vParams: TEntity;
begin
  Result := vsHidden;
  if not Assigned(AView) or (AView.DefinitionKind <> dkAction) or not Assigned(AView.Definition) then
    Exit;

  vAction := TActionDef(AView.Definition);
  vActionName := vAction.Name;
  vContext := AView.ParentDomainObject;
  vParams := TEntity(AView.DomainObject);

  vContextView := AView.Parent;
  if Assigned(vContextView) and vAction.HasFlag(ccMultiTarget)
    and (vContextView.DefinitionKind = dkEntity) and (vContextView.Name = 'Selected') then
  begin
    vListObject := TEntityList(vContextView.ParentDomainObject);
    Assert(Assigned(vListObject), 'Список записей отсутствует для действия [' + vActionName +']');
    if (vActionName = 'Delete') and (vContextView.Parent.State > vsSelectOnly) then
      Result := vsDisabled
    else
      Result := vsHidden;
    for vSelectedItem in vListObject.Selection do
      Result := Result or InternalCheckActionFlags(AView, vActionName, TEntity(vSelectedItem), vParams);
  end
  else
    Result := InternalCheckActionFlags(AView, vActionName, vContext, vParams);
end;

function TScript.CheckField(const AEntity: TEntity; const AFieldName: string): Boolean;
var
  vHandled: Boolean;
  vInclusion: TInclusion;
begin
  Result := True;
  if not Assigned(AEntity) then
    Exit;
  if AEntity.CollectionName = '' then
    Exit;

  for vInclusion in FInclusions do
  begin
    vHandled := True;
    Result := vInclusion.DoCheckField(AEntity, AFieldName, vHandled);
    if vHandled then
      Exit;
  end;

  vHandled := True;
  Result := DoCheckField(AEntity, AFieldName, vHandled);
  if not vHandled then
    Result := True;
end;

constructor TScript.Create(const AConfiguration: TObject);
begin
  inherited Create(AConfiguration);
  FInclusions := TObjectList<TInclusion>.Create;
end;

function TScript.CreateConfiguration: string;
var
  vDefinition: TDefinition;
  vMigration: TMigration;
  vAction: TActionDef;
  vInclusion: TInclusion;
begin
  RegisterComplexClass('Tensor', TTensorData);

  AddAction('Quit', 'Выход', -1);
  AddAction('Logout', 'Сменить пользователя', -1);

  vAction := AddAction('Add', 'Добавить', 1);
  vAction.AddSimpleFieldDef('SelectedIndex', '', '', 0, Null, Null, fkInteger, '', '', vsFullAccess, cNotSave);

  vAction := AddAction('Create', 'Создать', 1);
  vAction.AddSimpleFieldDef('SelectedIndex', '', '', 0, Null, Null, fkInteger, '', '', vsFullAccess, cNotSave);

  vAction := AddAction('Link', 'Привязать', 2);
  vAction.AddEntityFieldDef('SelectedEntity', '', 'Выбранная запись', '', '', 0, vsSelectOnly, cRequired or cNotSave);

  AddAction('Edit', 'Редактировать', 3, ccContextAction);
  AddAction('OpenInPage', 'Открыть на отдельной вкладке', 8, ccMultiTarget or ccContextAction);
  AddAction('Delete', 'Удалить', 4, ccMultiTarget or ccContextAction);
  AddAction('Open', 'Открыть', 30, ccContextAction);
  AddAction('Refresh', 'Обновить список', 5);
  AddAction('Save', 'Сохранить', 6);
  AddAction('SaveAs', 'Сохранить как', 6);

  AddAction('View', 'Просмотр', 8, ccContextAction);
  AddAction('Show', 'Показать', 8, ccContextAction);
  AddAction('Close', 'Закрыть', 4);
  AddAction('Ok', 'Ок', 12);
  AddAction('Cancel', 'Отмена', -1);

  AddAction('#HandleDblClick', 'Обработать двойное нажатие', -1);
  AddAction('#ExportToCsv', 'Экспорт в Excel', 7);
  AddAction('#ExportToCsv2', 'Экспорт в *.csv', 7);
  AddAction('#ApplyBestFit', 'Оптимальная ширина колонок', 47);

  vAction := AddAction('#FilterByText', 'Фильтровать по тексту', 13, ccInstantExecution);
  vAction.AddSimpleFieldDef('Text', '', '', Null, Null, 50, fkString, '', '', vsFullAccess, cNotSave);

  vAction := AddAction('#FilterByPeriod', 'Фильтровать по периоду', 13, ccInstantExecution);
  vAction.AddSimpleFieldDef('IsActive', '', 'период', True, Null, Null, fkBoolean);
  vAction.AddSimpleFieldDef('FromDate', '', 'с', cDateNow, Null, Null, fkDateTime);
  vAction.AddSimpleFieldDef('ToDate', '', 'по', cDateNow, Null, Null, fkDateTime);
  vAction.RegisterReaction('@FromDate;@ToDate', 'IsActive', TProc(procedure(const AHolder: TChangeHolder;
      const AFieldChain: string; const AEntity, AParam: TEntity)
    begin
      if AEntity['IsActive'] then
      begin
        AEntity.FieldByName('FromDate').SetUIState(vsFullAccess);
        AEntity.FieldByName('ToDate').SetUIState(vsFullAccess);
      end
      else begin
        AEntity.FieldByName('FromDate').SetUIState(vsHidden);
        AEntity.FieldByName('ToDate').SetUIState(vsHidden);
      end;
    end));

  vAction := AddAction('#GroupByColumn', 'Группировка данных', 9);
  vAction.AddSimpleFieldDef('IsChecked', '', '', False, Null, Null, fkBoolean);

  // Системное меню
  AddAction('ChangePassword', 'Сменить пароль', 24);
  AddAction('ShowSysLog', 'Системный журнал', -1);
  AddAction('SetupRTFReports', 'Настройки RTF отчётов', -1);
  AddAction('ShowSettings', 'Настройки', 29);
  AddAction('ShowOptions', 'Опции', -1);
  AddAction('LoadChanges', 'Загрузить изменения', -1);
  AddAction('ArrangeMozaic', 'Мозаикой', 33);
  AddAction('ArrangeCascade', 'Каскадом', 15);
  AddAction('ArrangeVert', 'Вертикально', 17);
  AddAction('ArrangeHorz', 'Горизонтально', 16);
  AddAction('CloseAllWindows', 'Закрыть все', -1);
  AddAction('ShowAbout', 'О программе', -1);
  AddAction('ShowStartPage', 'Показать стартовую страницу', -1);
  AddAction('GetAllUpdates', 'Выгрузить изменения в файл', -1);
  AddAction('ApplyAllUpdates', 'Применить обновления', -1);
  // При необходимости дописывать сюда действия для актуализации состояния модели
  AddAction('ActualizeData', 'Актуализировать данные', -1);

  // Любая сущность, которая будет определена позже
  AddDefinition('~', '', 'Неизвестная', cNullItemName, ccSystem or ccHideInMenu or ccNotSave);

  // Управление идентификацией
  vDefinition := AddDefinition('Numerators', '', 'Нумератор', cNullItemName, ccSystem or ccHideInMenu or ccLazyLoad);
  vDefinition.AddSimpleFieldDef('TableName', 'table_name', 'table_name', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('LastId', 'last_id', 'last_id', Null, Null, Null, fkInteger, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('LastCode', 'last_code', 'last_code', Null, Null, Null, fkInteger, '', '', vsFullAccess, cRequired);

  vDefinition := AddDefinition('SysDefinitions', '', 'Коллекции', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Caption', 'caption', 'Заголовок', Null, Null, 80);
  vDefinition.AddSimpleFieldDef('StorageName', 'storage_name', 'Наименование хранилища', Null, Null, 50, fkString, '', '', cRequired);
  vDefinition.AddSimpleFieldDef('Kind', 'kind', 'Тип', 1, Null, Null, fkInteger, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('LastId', 'last_id', 'last_id', Null, Null, Null, fkInteger, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('LastCode', 'last_code', 'last_code', Null, Null, Null, fkInteger, '', '', vsFullAccess, cRequired);

  // Пользователи и безопасность
  vDefinition := AddDefinition('SysSubjects', '', 'Действующие субъекты', '< не указана >', 0, clkMixin);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Имя', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired or cLocalizable);
  vDefinition.AddSimpleFieldDef('AccessFlags', 'access_flags', 'Доступ по умолчанию', vsFullAccess, 0, 15, fkInteger, '', '', vsFullAccess, cRequired or cHideInGrid);
  vDefinition.AddListFieldDef('SecuredObjects', 'Subject', 'Объекты с уникальными правами доступа', 'SysSecuredObjects', '', '', vsHidden, cAnalytic, estSortByID, '', rpStrong);

  vDefinition := AddDefinition('CollisionActions', '', 'Действия при коллизиях', '< не выбрано >', ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Имя', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired or cLocalizable);
  vDefinition.AddUniqueIndex('Name');

  vDefinition := AddDefinition('SysUsers', 'SysSubjects', 'Пользователи', '[Система]', ccHideInMenu or ccStopEvents).SetImageID(32);
  vDefinition.AddSimpleFieldDef('Login', 'login', 'Логин', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('PasswordText', '', 'Пароль', Null, Null, 50, fkString, 'mask', '', vsFullAccess, cNotSave or cHideInGrid);
  vDefinition.AddSimpleFieldDef('Password', 'user_password', 'Пароль (хэш)', Md5Hash(''), Null, 32, fkString, '', '', vsHidden);
  vDefinition.AddSimpleFieldDef('RFID', 'rf_id', 'Радиометка', Null, Null, 50, fkString, '', '', vsHidden{ cReadOnly});
  vDefinition.AddSimpleFieldDef('Online', 'online', 'В системе', False, Null, Null, fkBoolean, '', '', vsReadOnly);
  vDefinition.AddEntityFieldDef('CollisionAction', 'collision_action', 'Способ разрешения коллизий при сохранении', '', 'CollisionActions', 1, vsSelectOnly);
  vDefinition.AddListFieldDef('Roles', 'User', 'Роли пользователя', 'SysUsersRoles', '', 'mtm?transit=Role', vsFullAccess, 0, estUserSort, '', rpStrong);
  //vDefinition.AddListFieldDef('Notifications', 'User', 'Уведомления пользователя', 'SysUsersNotifications', '', '', vsReadOnly, cAnalytic, estUserSort, '', rpStrong);
  vDefinition.AddUniqueIndex('Login');
  vDefinition.AddAction('GeneratePassword', 'Сгенерировать пароль', 24);
  vDefinition.RegisterReaction('Password', 'PasswordText', TProc(procedure(const AHolder: TChangeHolder;
      const AFieldChain: string; const AEntity, AParam: TEntity)
    begin
      AEntity._SetFieldValue(AHolder, 'Password', MD5Hash(AEntity['PasswordText']));
    end));

  vDefinition := AddDefinition('SysRoles', 'SysSubjects', 'Роли', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('ParentRole', 'parent', 'Родительская роль', '', 'SysRoles');
  vDefinition.AddSimpleFieldDef('Code', 'code', 'Код', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddListFieldDef('Users', 'Role', 'Пользователи', 'SysUsersRoles', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddUniqueIndex('Code');

  vDefinition := AddDefinition('SysUsersRoles', '', 'Роли пользователей', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('User', 'user', 'Пользователь', '', 'SysUsers', 0, vsHidden);
  vDefinition.AddEntityFieldDef('Role', 'role', 'Роль', '', 'SysRoles', 0, vsSelectOnly, cRequired);
  vDefinition.AddUniqueIndex('User,Role');
  vDefinition.RegisterReaction('Name', 'Name.User;User;Name.Role;Role', TProc(procedure(const AHolder: TChangeHolder;
      const AFieldChain: string; const AEntity, AParam: TEntity)
    begin
      AEntity._SetFieldValue(AHolder, 'Name', SafeDisplayName(AEntity.ExtractEntity('User')) + ' : ' + SafeDisplayName(AEntity.ExtractEntity('Role')));
    end));

  vDefinition := AddDefinition('SysSecuredObjects', '', 'Настройки безопасности', cNullItemName, ccHideInMenu or ccNotSave);
  vDefinition.AddEntityFieldDef('Subject', 'subject', 'Субъект', '', 'SysSubjects', 0, vsFullAccess, cNotSave);
  vDefinition.AddSimpleFieldDef('ObjectName', 'object_name', 'Объект', Null, Null, 100, fkString, '', '', vsFullAccess, cNotSave);
  vDefinition.AddSimpleFieldDef('StateName', 'state_name', 'Состояние', Null, Null, 100, fkString, '', '', vsFullAccess, cNotSave);
  vDefinition.AddSimpleFieldDef('AccessFlags', 'access_flags', 'Доступ', 0, 15, 0, fkFlag, '', '', vsFullAccess, cNotSave);
  vDefinition.AddUniqueIndex('Subject,ObjectName');

  // Системный лог
  vDefinition := AddDefinition('SysLogActionKinds', '', 'Виды действий', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', '', 1, 50, fkString, '', '', vsFullAccess, cLocalizable);
  vDefinition.AddSimpleFieldDef('Code', 'code', 'Код', '', 1, 10);

  vDefinition := AddDefinition('SysLog', '', 'Системный журнал', cNullItemName, ccSystem or ccHideInMenu or ccLazyLoad);
  vDefinition.AddSimpleFieldDef('LogTime', 'log_time', 'Время', cDateTimeNow, Null, Null, fkDateTime, 'datetime');
  vDefinition.AddSimpleFieldDef('UserName', 'user_name', 'Пользователь', Null, Null, 50);
  vDefinition.AddSimpleFieldDef('Description', 'description', 'Описание', Null, Null, -1, fkString, 'memo', '', vsReadOnly);
  vDefinition.AddListFieldDef('SysLogActions', 'SysLog', 'Действия', 'SysLogActions', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);

  vDefinition := AddDefinition('SysLogActions', '', 'Действия пользователей', cNullItemName, ccSystem or ccHideInMenu or ccLazyLoad);
  vDefinition.AddEntityFieldDef('SysLog', 'sys_log', 'Номер в журнале', '', 'SysLog', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('CollectionName', 'collection', 'Коллекция', Null, Null, 50, fkString, '', '', vsReadOnly);
  vDefinition.AddSimpleFieldDef('EntityID', 'entity_id', 'ID', Null, Null, Null, fkInteger, '', '', vsReadOnly);
  vDefinition.AddEntityFieldDef('ActionKind', 'action_kind', 'Вид действия', '', 'SysLogActionKinds', 0, vsReadOnly);
  vDefinition.AddSimpleFieldDef('Description', 'description', 'Описание', Null, Null, -1, fkString, 'memo', '', vsReadOnly);
  vDefinition.AddSimpleFieldDef('JSON', 'json', 'Актуальные значения', Null, Null, -1, fkString, 'memo', '', vsReadOnly);

  // Коммуникации
  (*vDefinition := AddDefinition('SysNotificationSeverities', '', 'Важность уведомлений', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', '', 1, 50, fkString, '', '', vsFullAccess, cLocalizable);
  vDefinition.AddSimpleFieldDef('Code', 'code', 'Код', '', 1, 10);

  vDefinition := AddDefinition('SysNotificationStates', '', 'Статусы уведомлений', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', '', 1, 50, fkString, '', '', vsFullAccess, cLocalizable);
  //vDefinition.AddSimpleFieldDef('', 'Code', 'code', 'Код', '', 1, 10);

  vDefinition := AddDefinition('SysNotifications', '', 'Уведомления', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddEntityFieldDef('Sender', 'sender', 'Отправитель', '', 'SysUsers', 0);
  vDefinition.AddEntityFieldDef('Severity', 'severity', 'Важность', '', 'SysNotificationSeverities', 1);
  vDefinition.AddSimpleFieldDef('Message', 'message', 'Сообщение', Null, Null, 255, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('DueDate', 'due_date', 'Сделать до', Null, Null, Null, fkDateTime, 'datetime');
  vDefinition.AddSimpleFieldDef('ActionLink', 'action_link', 'Ссылка на действие', Null, Null, 100, fkString, '', '', vsHidden);

  vDefinition := AddDefinition('SysUsersNotifications', '', 'Уведомления пользователя', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddEntityFieldDef('User', 'user', 'Пользователь', '', 'SysUsers', 0, vsHidden);
  vDefinition.AddEntityFieldDef('Notification', 'notification', 'Уведомление', '', 'SysNotifications', 0, vsHidden);
  vDefinition.AddEntityFieldDef('State', 'state', 'Статус', '', 'SysNotificationStates', 1); *)

  vDefinition := AddDefinition('SysDocuments', '', 'Документы', ' <не загружен> ', ccHideInMenu); // Lazy Load
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Название документа', Null, Null, 100);
  vDefinition.AddSimpleFieldDef('FileName', 'file_name', 'Имя файла', Null, Null, 100, fkString, '', '', vsReadOnly, cRequired);
  vDefinition.AddSimpleFieldDef('FullPathName', 'full_path_name', 'Полный путь к файлу', Null, Null, 255, fkString, '', '', vsHidden);
  vDefinition.AddBlobFieldDef('Content', 'content', 'Содержимое', bffRaw, '', vsReadOnly).SetStorageKind(skSharedFolder);
  vDefinition.AddSimpleFieldDef('ContentUrl', 'content_url', 'Место хранения файла', Null, Null, 255, fkString, '', '', vsHidden);
  vDefinition.AddAction('ViewDocument', 'Открыть документ', 30);
  vDefinition.AddAction('LoadDocument', 'Загрузить документ', 23);
  vAction := vDefinition.AddAction('Load', 'Загрузить документ', 23);
  vAction.AddSimpleFieldDef('FileName', 'file_name', 'Путь к файлу', Null, Null, 255, fkString, 'file?filter=Все файлы (*.*)|*.*', '', vsSelectOnly, cRequired);
  vDefinition.AddAction('ClearDocument', 'Очистить документ', 4);
  vDefinition.RegisterReaction('FileName;Name;Content', 'FullPathName', TProc(procedure(const AHolder: TChangeHolder;
      const AFieldChain: string; const AEntity, AParam: TEntity)
    var
      vFilePath: string;
      vFileName: string;
      vStream: TMemoryStream;
    begin
      vFilePath := Trim(AEntity['FullPathName']);
      if FileExists(vFilePath) then
      begin
        vFileName := ExtractFileName(vFilePath);
        AEntity._SetFieldValue(AHolder, 'FileName', vFileName);
        if AEntity['Name'] = '' then
          AEntity._SetFieldValue(AHolder, 'Name', ChangeFileExt(vFileName, ''));

        vStream := TMemoryStream.Create;
        vStream.LoadFromFile(vFilePath);
        AEntity._SetFieldStream(AHolder, 'Content', vStream);
      end
      else begin
        AEntity._SetFieldValue(AHolder, 'FileName', '');
        AEntity._SetFieldStream(AHolder, 'Content', nil);
        AEntity._SetFieldValue(AHolder, 'Name', '');
      end;
    end));

  // Константы
  vDefinition := AddDefinition('MailServices', '', 'Сервисы эл.почты', '< не выбран >', ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование сервиса', '', Null, 20, fkString, '', '', vsFullAccess, cLocalizable);
  vDefinition.AddSimpleFieldDef('SmtpHost', 'smtp_host', 'SMTP сервер', Null, Null, 100);
  vDefinition.AddSimpleFieldDef('SmtpPort', 'smtp_port', 'Порт', 25, 1, 65536, fkInteger);
  vDefinition.AddSimpleFieldDef('SmtpTimeout', 'smtp_timeout', 'Таймаут подключения к SMTP', 5000, 0, Null, fkInteger);

  vDefinition := AddDefinition('SysConstants', '', 'Системные настройки', cNullItemName, ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Code', 'code', 'Код окружения', Null, Null, 10);
  vDefinition.AddSimpleFieldDef('Description', 'description', 'Описание', Null, Null, 255, fkString, '', '', vsFullAccess, cLocalizable);
  vDefinition.AddSimpleFieldDef('Version', 'version', 'Версия', Null, Null, 50, fkString, '', '', vsReadOnly);
  vDefinition.AddEntityFieldDef('MailService', 'mail_service', 'Почтовый сервис', '', 'MailServices');
  vDefinition.AddSimpleFieldDef('MailLogin', 'mail_login', 'Адрес служебной почты', Null, Null, 120);
  vDefinition.AddSimpleFieldDef('MailPassword', 'mail_password', 'Пароль служебной почты', Null, Null, 30, fkString, 'mask');
  vDefinition.AddSimpleFieldDef('MailFromName', 'mail_from_name', 'Подпись отправителя', 'Application notifications', Null, 100, fkString, '', '', vsFullAccess, cLocalizable);
  vDefinition.AddSimpleFieldDef('IsMailVerified', 'is_mail_verified', 'Соединение проверено', False, Null, Null, fkBoolean, '', '', vsReadOnly);
  vDefinition.AddSimpleFieldDef('NewRecordColor', 'new_record_color', 'Цвет новой записи', TColorRec.Silver, Null, Null, fkColor, 'simple', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('ServiceRecordColor', 'service_record_color', 'Цвет служебной записи', TColorRec.Navy, Null, Null, fkColor, 'simple', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('SharedFolder', 'shared_folder', 'Папка для хранения общих файлов', Null, Null, 255, fkString, 'dir');
  vDefinition.AddUniqueIndex('Code');
  vDefinition.AddAction('CheckEmailConnection', 'Проверить SMTP соединение', 25);
  vDefinition.RegisterReaction('IsMailVerified', 'MailService;MailLogin;MailPassword',
    TProc(procedure(const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParam: TEntity)
    begin
      if AEntity['IsMailVerified'] then
        AEntity._SetFieldValue(AHolder, 'IsMailVerified', False);
    end));

  for vInclusion in FInclusions do
    vInclusion.DoCreateDefinitions;
  DoCreateDefinitions;

  for vInclusion in FInclusions do
    vInclusion.DoCreateMigrations;
  DoCreateMigrations;

  if TConfiguration(FConfiguration).Migrations.Count > 0 then
  begin
    vMigration := TConfiguration(FConfiguration).Migrations[TConfiguration(FConfiguration).Migrations.Count - 1];
    if vMigration.VersionGreaterThan(FVersion) then
      FVersion := vMigration.Version;
  end;

  Result := FVersion;
end;

procedure TScript.CreateContentTypeChangeHandler(const ADefinition: TDefinition; const ATargetFieldName,
  AContentTypePath: string);
var
  vFieldChain: string;

  function BuildFieldChain(const AFieldPath: string): string;
  var
    vFieldPath: TStrings;
    vChain: string;
    i, j: Integer;
  begin
    Result := '';
    vFieldPath := CreateDelimitedList(AFieldPath, '.');
    try
      for i := vFieldPath.Count - 1 downto 0 do
      begin
        vChain := '';
        for j := i downto 0 do
          if j > 0 then
            vChain := vChain + vFieldPath[j] + '.'
          else
            vChain := vChain + vFieldPath[j];
        if i > 0 then
          Result := Result + vChain + ';'
        else
          Result := Result + vChain;
      end;
    finally
      FreeAndNil(vFieldPath);
    end;
  end;
begin
  vFieldChain := BuildFieldChain(AContentTypePath);

  ADefinition.RegisterReaction(ATargetFieldName, vFieldChain, TProc(procedure(
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    var
      vContentTypeName: string;
    begin
      vContentTypeName := SafeDisplayName(AEntity.ExtractEntity(AContentTypePath));
      if vContentTypeName <> '' then
        TEntityField(AEntity.FieldByName(ATargetFieldName)).SetContentDefinition(AHolder,
          TDomain(AEntity.Domain).Configuration[vContentTypeName])
      else
        TEntityField(AEntity.FieldByName(ATargetFieldName)).SetContentDefinition(AHolder, nil);
    end));
end;

procedure TScript.CreateDefaultEntities(const ADomain: TObject);
var
  vDomain: TDomain absolute ADomain;
const
  cAdminID = 1;
  cAdminsID = 1;
begin
  vDomain.DomainSession.AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
    var
      vCollection: TCollection;
      vInclusion: TInclusion;
    begin
      vCollection := vDomain['MailServices'];
      vCollection.CreateDefaultEntity(AHolder, 1, 'Name;SmtpHost;SmtpPort', ['@mail.ru', 'smtp.mail.ru', 2525]);
      vCollection.CreateDefaultEntity(AHolder, 2, 'Name;SmtpHost;SmtpPort', ['Яндекс', 'smtp.yandex.ru', 587]);
      vCollection.CreateDefaultEntity(AHolder, 3, 'Name;SmtpHost;SmtpPort', ['GMail', 'smtp.gmail.com', 587]);

      vCollection := vDomain['CollisionActions'];
      vCollection.CreateDefaultEntity(AHolder, 1, 'Name', ['Вручную']);
      vCollection.CreateDefaultEntity(AHolder, 2, 'Name', ['Автоматически, в пользу своих изменений']);
      vCollection.CreateDefaultEntity(AHolder, 3, 'Name', ['Автоматически, в пользу пришедших изменений']);

      vCollection := vDomain['SysRoles'];
      vCollection.CreateDefaultEntity(AHolder, cAdminsID, 'Code;Name;AccessFlags',
        ['Administrators', 'Администраторы', vsFullAccess], True);

      (*vCollection := vDomain['SysNotificationSeverities'];
      vCollection.CreateDefaultEntity(AHolder, 1, 'Name;Code', ['Информация', 'INFO']);
      vCollection.CreateDefaultEntity(AHolder, 2, 'Name;Code', ['Внимание', 'WARN']);
      vCollection.CreateDefaultEntity(AHolder, 3, 'Name;Code', ['Важно!', 'ALARM']);

      vCollection := vDomain['SysNotificationStates'];
      vCollection.CreateDefaultEntity(AHolder, 1, 'Name', ['Новое']);
      vCollection.CreateDefaultEntity(AHolder, 2, 'Name', ['Просмотрено']);
      vCollection.CreateDefaultEntity(AHolder, 3, 'Name', ['Обработано']);
      vCollection.CreateDefaultEntity(AHolder, 4, 'Name', ['Удалено']);*)

      vCollection := vDomain['SysLogActionKinds'];
      vCollection.CreateDefaultEntity(AHolder, 1, 'Name;Code', ['Добавление', 'add']);
      vCollection.CreateDefaultEntity(AHolder, 2, 'Name;Code', ['Изменение', 'update']);
      vCollection.CreateDefaultEntity(AHolder, 3, 'Name;Code', ['Удаление', 'delete']);

      try
        for vInclusion in FInclusions do
          vInclusion.DoCreateDefaultEntities(vDomain, AHolder);
        DoCreateDefaultEntities(vDomain, AHolder);
      except
        on E: Exception do
          vDomain.Logger.AddMessage('Ошибка при создании объектов по умолчанию. (' + E.Message + ')')
      end;

      Result := True;
    end, nil, True);
end;

destructor TScript.Destroy;
begin
  FreeAndNil(FInclusions);
  inherited Destroy;
end;

function TScript.DoBeforeUIClosing(const AInteractor: TInteractor): Boolean;
begin
  Result := AInteractor.ShowYesNoDialog('Подтвердите', 'Вы действительно хотите выйти?') = drYes;
end;

function TScript.DoExecuteDefaultAction(const ASession: TUserSession; const AParams: string): Boolean;
begin
  Result := False;
end;

procedure TScript.DomainReady(const ADomain: TObject);
var
  vInclusion: TInclusion;
begin
  for vInclusion in FInclusions do
    vInclusion.DoOnDomainReady(ADomain);
  DoOnDomainReady(ADomain);
end;

procedure TScript.DomainStopped;
var
  vInclusion: TInclusion;
begin
  for vInclusion in FInclusions do
    vInclusion.DoOnDomainStopped;
  DoOnDomainStopped;
end;

function TScript.ExecuteAction(const AView: TView; const AParentHolder: TChangeHolder): Boolean;
var
  vInteractor: TInteractor;
  vSession: TUserSession;
  vAction: TActionDef;
  vActionName: string;
  vContextView: TView;
  vContext: TObject;
  vListObject: TEntityList;
  i: Integer;
  vParams: TEntity;
  vNeedClearParams: Boolean;
  vSelectionCopy: TList<TObject>;
  vSelectionCount: Integer;
  vRecordsText: string;
  vNeedShowParams: Boolean;
  vVisibleFieldCount: Integer;
  vField: TBaseField;
  vFileName: string;
  vDone: Boolean;
  vUrl: TIdURI;
  vUrlCommand, vFilter: string;
  vUrlParams: TStrings;
begin
  Result := False;

  vInteractor := TInteractor(AView.Interactor);
  vSession := TUserSession(AView.Session);

  vAction := TActionDef(AView.Definition);
  vActionName := vAction.Name;
  vContext := AView.ParentDomainObject;
  vParams := TEntity(AView.DomainObject);

  vNeedClearParams := False;
  vDone := False;
  try
    if Assigned(vParams) then
    begin
      FillActionParams(AView, vActionName, vContext, vParams);
      vVisibleFieldCount := vParams.VisibleFieldCount(vSession);
      vNeedShowParams := (vVisibleFieldCount > 0) and vAction.HasFlag(ccAlwaysShowParameters);
      if vVisibleFieldCount = 1 then
      begin
        vField := nil;
        for i := 0 to vParams.FieldCount - 1 do
          if (vParams.Fields[i].GetUIState(vSession) > vsHidden)
            and not vParams.Fields[i].FieldDef.HasFlag(cHideInEdit) then
          begin
            vField := vParams.Fields[i];
            Break;
          end;
        if Assigned(vField) then
        begin
          vUrl := TIdURI.Create(vField.FieldDef.StyleName);
          vUrlCommand := vUrl.Document;
          vUrlParams := CreateDelimitedList(vUrl.Params, '&');
          vFilter := vUrlParams.Values['filter'];
          vUrlParams.Free;
          vUrl.Free;
          if vUrlCommand = 'file' then
          begin
            vFileName := vParams[vField.FieldName];
            if TPresenter(vInteractor.Presenter).ShowOpenDialog(vFileName, 'Выберите файл', vFilter, '', '') then
            begin
              vDone := True;
              vParams._SetFieldValue(vSession.NullHolder, vField.FieldName, vFileName);
            end
            else
              Exit;
          end;
        end;
      end;

      if (not vParams.IsValid or vNeedShowParams) and not vDone then
      begin
        vNeedClearParams := not vParams.IsValid and not vNeedShowParams;
        if vActionName = 'Link' then
          TObjectFieldDef(vAction.FieldByName('SelectedEntity')).SetContentDefinition(
            TListFieldDef(AView.Parent.Definition)._ContentDefinition);

        if not vInteractor.AtomicEditParams(AView) then
          Exit;
      end;
    end;

    vContextView := AView.Parent;
    if Assigned(vContextView) and vAction.HasFlag(ccMultiTarget)
      and (vContextView.DefinitionKind = dkEntity) and (vContextView.Name = 'Selected') then
    begin
      if not Assigned(vContext) then
        Exit;

      vListObject := TEntityList(vContextView.ParentDomainObject);
      Assert(Assigned(vListObject), 'Список записей отсутствует для действия [' + vActionName +']');

      vSelectionCount := vListObject.Selection.Count;
      if vSelectionCount = 0 then
        Exit;

      if vActionName = 'Delete' then
      begin
        if vSelectionCount > 1 then
        begin
          case vSelectionCount mod 10 of
            1: vRecordsText := 'запись';
            2..4: vRecordsText := 'записи';
          else
            vRecordsText := 'записей';
          end;

          if TPresenter(vInteractor.Presenter).ShowYesNoDialog(vInteractor.Translate('cptPrompt', 'Подтвердите'),
            Format(vInteractor.Translate('msgWantDeleteNRecords', 'Вы действительно хотите удалить %d %s?'),
            [vSelectionCount, vRecordsText])) = drNo
          then
            Exit;
        end
        else if (vSelectionCount = 1) and (TPresenter(vInteractor.Presenter).ShowYesNoDialog(
          vInteractor.Translate('cptPrompt', 'Подтвердите'), vInteractor.Translate('msgWantDeleteRecord',
          'Вы действительно хотите удалить запись') + ' [' + SafeDisplayName(vListObject.Selection[0]) +']?') = drNo)
        then
          Exit;
      end;

      Result := True;

      vSelectionCopy := TList<TObject>.Create;
      for i := 0 to vSelectionCount - 1 do
        if InternalCheckActionFlags(AView, vActionName, TEntity(vListObject.Selection[i]), vParams) = vsFullAccess then
          vSelectionCopy.Add(vListObject.Selection[i]);

      try
        for i := 0 to vSelectionCount - 1 do
          Result := Result and InternalExecuteAction(AView, vActionName, TEntity(vSelectionCopy[i]), vParams, AParentHolder);
      finally
        FreeAndNil(vSelectionCopy);
      end;
    end
    else
      if InternalCheckActionFlags(AView, vActionName, vContext, vParams) = vsFullAccess then
        Result := InternalExecuteAction(AView, vActionName, vContext, vParams, AParentHolder)
      else
        Result := False;
  finally
    if vNeedClearParams then
      vParams.ResetToDefault(vSession.NullHolder);
  end;
end;

function TScript.ExecuteCommand(const AExecutor: TObject; const ATask: TTaskHandle; const AFiber, ACommand: TObject): Boolean;
var
  vInclusion: TInclusion;
begin
  Result := True;

  if DoExecuteCommand(AExecutor, ATask, AFiber, ACommand) then
    Exit;

  for vInclusion in FInclusions do
    if vInclusion.DoExecuteCommand(AExecutor, ATask, AFiber, ACommand) then
      Exit;

  Result := False;
end;

function TScript.ExecuteDefaultAction(const ASession: TUserSession; const AParams: string): Boolean;
begin
  Result := DoExecuteDefaultAction(ASession, AParams);
end;

procedure TScript.FillActionParams(const AView: TView; const AActionName: string; const AContext: TObject;
  const AParams: TEntity);
var
  vInclusion: TInclusion;
begin
  for vInclusion in FInclusions do
    vInclusion.DoFillActionParams(AView, AActionName, AContext, AParams);

  DoFillActionParams(AView, AActionName, AContext, AParams);
end;

function TScript.FullText(const AEntity: TEntity): string;
var
  vHandled: Boolean;
  vInclusion: TInclusion;
begin
  for vInclusion in FInclusions do
  begin
    vHandled := True;
    Result := vInclusion.GetFullText(AEntity, vHandled);
    if vHandled then
      Exit;
  end;

  vHandled := True;
  Result := GetFullText(AEntity, vHandled);
  if not vHandled then
    Result := SafeDisplayName(AEntity);
end;

function TScript.GetParamValue(const AEntity: TEntity; const AParamName: string): string;
var
  vHandled: Boolean;
  vInclusion: TInclusion;
begin
  Result := '';
  if not Assigned(AEntity) then
    Exit;
  if AEntity.CollectionName = '' then
    Exit;

  for vInclusion in FInclusions do
  begin
    vHandled := True;
    Result := vInclusion.DoGetParamValue(AEntity, AParamName, vHandled);
    if vHandled then
      Break;
  end;

  vHandled := True;
  Result := DoGetParamValue(AEntity, AParamName, vHandled);
  if not vHandled then
    Result := '';
end;

class function TScript.GetRegisteredScripts: TStrings;
begin
  Result := RegisteredScripts.ToStrings;
end;

function TScript.GetReportValue(const AReportData: TReportData; const AParamName: string; const AIndex1,
  AIndex2: Integer): TReportValue;
var
  vReportName: string;
  vHandled: Boolean;
  vInclusion: TInclusion;
begin
  Result.Value := '';
  Result.FieldKind := fkString;
  Result.Extra := '';

  vReportName := AReportData.ReportName;
  if vReportName = '' then
    Exit;

  Result := AReportData.ValueByName(AParamName);
  if not VarIsNull(Result.Value) then
    Exit;

  for vInclusion in FInclusions do
  begin
    vHandled := True;
    Result := vInclusion.DoGetReportValue(AReportData, AParamName, AIndex1, AIndex2, vHandled);
    if vHandled then
      Exit;
  end;

  vHandled := True;
  Result := DoGetReportValue(AReportData, AParamName, AIndex1, AIndex2, vHandled);
  if not vHandled then
    Result.Value := Null;
end;

function TScript.InternalCheckActionFlags(const AView: TView; const AActionName: string; const AContext: TObject;
  const AParams: TEntity): TViewState;
var
  vInclusion: TInclusion;
  vInteractor: TInteractor;
  vParentView: TView;
  vDefinition: TDefinition;
  vEntity: TEntity;
begin
  Result := DoCheckActionFlags(AView, AActionName, AContext, AParams);
  if Result <> vsUndefined then
    Exit;

  for vInclusion in FInclusions do
  begin
    Result := vInclusion.DoCheckActionFlags(AView, AActionName, AContext, AParams);
    if Result <> vsUndefined then
      Exit;
  end;

  Result := vsFullAccess;
  vInteractor := TInteractor(AView.Interactor);

  if AActionName = 'Save' then
  begin
    if not Assigned(vInteractor.Session) then
      Result := vsDisabled;
  end
  else if AActionName = '#FilterByPeriod' then
  begin
    if Assigned(AView.Parent) and (AView.Parent.DefinitionKind = dkCollection) and Assigned(AView.Parent.Definition) then
    begin
      vDefinition := TDefinition(AView.Parent.Definition);
      if vDefinition.FieldExists('DocDate') then
        Result := vsFullAccess
      else
        Result := vsHidden;
    end
    else
      Result := vsHidden;
  end
  else if AActionName = 'Add' then
  begin
    vParentView := GetParentListView(AView);
    if Assigned(vParentView) then
    begin
      if vParentView.State < vsFullAccess then
        Result := vsHidden;
    end
    else
      if AView.Parent.State < vsFullAccess then
        Result := vsHidden;
  end
  else if AActionName = 'Create' then
  begin
    vParentView := AView.Parent;
    if vParentView.State < vsFullAccess then
      Result := vsHidden;
  end
  else if AActionName = 'Link' then
  begin
    vParentView := GetParentListView(AView);
    if (vParentView.DefinitionKind <> dkListField) or (vParentView.State < vsSelectOnly) then
      Result := vsHidden
    else if TListFieldDef(vParentView.Definition).RelationPower <> rpWeak then
      Result := vsHidden;
  end
  else if AActionName = 'Edit' then
  begin
    vParentView := GetParentListView(AView);
    if Assigned(vParentView) then
    begin
      if vParentView.State > vsSelectOnly then
        if AView.Parent.State = vsHidden then
          Result := vsDisabled
        else if AView.Parent.State <= vsSelectOnly then
          Result := vsHidden
        else if Assigned(AContext) then
          Result := AView.Parent.State
        else
          Result := vsDisabled
      else
        Result := vsHidden;
    end
    else begin
      if AView.Parent.State < vsSelectOnly then
        Result := vsDisabled
      else
        Result := vsFullAccess;
    end;
  end
  else if AActionName = 'Delete' then
  begin
    vParentView := GetParentListView(AView);
    if vParentView.State < vsFullAccess then
      Result := vsHidden
    else if AView.Parent.State = vsHidden then
      Result := vsDisabled
    else if AView.Parent.State <= vsSelectOnly then
      Result := vsHidden
    else
      Result := AView.Parent.State;
  end
  else if AActionName = 'Unlink' then
  begin
    {vParentListView := GetParentListView(AView);
    if vParentListView.State >= vsSelectOnly then
      if AView.Parent.State = vsHidden then
        Result := cDisabled
      else if AView.Parent.State <= vsReadOnly then
        Result := vsHidden
      else
        Result := AView.Parent.State
    else
      Result := vsHidden;}
    Result := vsHidden;
  end
  else if AActionName = 'View' then
  begin
    vParentView := GetParentListView(AView);
    if Assigned(vParentView) then
    begin
      if vParentView.State in [vsReadOnly, vsSelectOnly] then
      begin
        if AView.Parent.State = vsHidden then
          Result := vsDisabled
        else if AView.Parent.State > vsSelectOnly then
          Result := vsHidden;
      end
      else if (vParentView.State < vsFullAccess) or not (AView.Parent.State in [vsReadOnly, vsSelectOnly]) then
        Result := vsHidden;
    end
    else begin
      if AView.Parent.State = vsFullAccess then
        Result := vsHidden;
    end;
  end
  else if AActionName = 'OpenInPage' then
    Result := vsFullAccess
  else if AActionName = 'Load' then
    Result := vsFullAccess
  else if AActionName = 'Refresh' then
  begin
    Result := vsFullAccess;
  end
  else if AActionName = 'Show' then
  begin
    if not Assigned(AView.ParentDomainObject) then
      Result := vsDisabled
    else
      Result := vsFullAccess;
  end
  else if AActionName = 'ChangePassword' then
    Result := vsDisabled
  else if (AActionName = 'ShowSysUsers') or (AActionName = 'ShowSysLog') or (AActionName = 'SetupRTFReports')
    or (AActionName = 'ShowStartPage') then
  begin
    if not TUserSession(vInteractor.Session).IsAdmin then
      Result := vsHidden
  end
  else if (AActionName = 'ShowSettings') or (AActionName = 'ShowAbout') or (AActionName = 'Quit') then
    //
  else if Assigned(AContext) and (AContext is TEntity) then
  begin
    vEntity := TEntity(AContext);
    if vEntity.InstanceOf('SysDocuments') then
    begin
      if AActionName = 'ViewDocument' then
      begin
        if not Assigned(vEntity.GetFieldBlob('Content')) or (Pos('.', vEntity['FileName']) = 0) then
          Result := vsDisabled;
      end
      else if AActionName = 'ClearDocument' then
      begin
        if not Assigned(vEntity.GetFieldBlob('Content')) or (Pos('.', vEntity['FileName']) = 0) then
          Result := vsDisabled;
      end;
    end
    else if (AActionName = 'GeneratePassword') and TEntity(AContext).IsService then
      Result := vsDisabled
    else if AActionName = 'CheckEmailConnection' then
    begin
      //if not Assigned(vEntity.ExtractEntity('MailService')) then
      //  Result := vsDisabled;
    end;
  end
  else if not Assigned(AContext) and (AView.Parent.DefinitionKind <> dkDomain) then
    Result := vsDisabled;
end;

function TScript.InternalExecuteAction(const AView: TView; const AActionName: string; const AContext: TObject;
  const AParams: TEntity; const AParentHolder: TChangeHolder): Boolean;
var
  i: Integer;
  vStartPageName: string;
  vNewPassword: string;
  vMessage: string;
  AInteractor: TInteractor;
  vModalResult: Integer;
  vConfig: TEntity;
  jChanges: TJSONObject;
  vSelectedView: TView;
  vEntity: TEntity;
  vFileName: string;
  vContentStream: TStream;
  vStream: TMemoryStream;
  vInclusion: TInclusion;
const
  cLegalSymbols = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_#';

  procedure InternalDelete(const AEntity: TEntity);
  var
    vParentObject: TObject;
  begin
    vParentObject := AView.Parent.ParentDomainObject;
    if not Assigned(vParentObject) or not (vParentObject is TEntityList) then
      Exit;

    TEntityList(vParentObject).RemoveEntity(AParentHolder, AEntity);
  end;

  procedure InternalEdit(const AEntity: TEntity);
  var
    vLayout: string;
  begin
    if not Assigned(AEntity) then Exit;

    vLayout := '';
    if AEntity.FieldExists('_Layout') then
      vLayout := AEntity['_Layout'];
    AInteractor.AtomicEditEntity(AView.Parent, AParentHolder, vLayout);
  end;

  procedure InternalOpen(const AEntity: TEntity);
  var
    vParentObject: TObject;
  begin
    vParentObject := AView.Parent.ParentDomainObject;
    if not Assigned(vParentObject) or not (vParentObject is TEntityList) then
      Exit;

    if not Assigned(AEntity) then
      Exit;

    AInteractor.UIBuilder.Navigate(AInteractor.GetViewOfEntity(AEntity),
      'WorkArea', '', '', AParentHolder, nil, AEntity.DisplayName);
  end;

  procedure InternalAdd(const AEntityList: TEntityList);
  var
    vDefinitionName: string;
    vDefinitionIndex: Integer;
  begin
    if Assigned(AParams) then
      vDefinitionIndex := Max(0, AParams['SelectedIndex'])
    else
      vDefinitionIndex := 0;

    if AEntityList.ContentDefinitions.Count > vDefinitionIndex then
      vDefinitionName := TDefinition(AEntityList.ContentDefinitions[vDefinitionIndex]).Name
    else
      Exit;

    AInteractor.AtomicEditEntity(function(const AHolder: TObject): TView
      var
        vNewEntity: TEntity;
      begin
        vNewEntity := AEntityList.AddEntity(AHolder, vDefinitionName, '', []);
        AEntityList.SelectEntity(vNewEntity);
        Result := AView.Parent.ViewByName('Selected');
        if not Assigned(Result) then
          Result := AInteractor.GetViewOfEntity(vNewEntity);
      end, AParentHolder);
  end;

  procedure InternalCreate(const AView: TView);
  var
    vDefinitionName: string;
    vDefinitionIndex: Integer;
    vFieldDef: TObjectFieldDef;
    vNewEntity: TEntity;
    vMasterEntity: TEntity;
    vSession: TUserSession;
    vHolder: TChangeHolder;
    vResult: Boolean;
  begin
    if Assigned(AParams) then
      vDefinitionIndex := Max(0, AParams['SelectedIndex'])
    else
      vDefinitionIndex := 0;

    vFieldDef := TObjectFieldDef(AView.Parent.Definition);

    if vFieldDef.ContentDefinitions.Count > vDefinitionIndex then
      vDefinitionName := TDefinition(vFieldDef.ContentDefinitions[vDefinitionIndex]).Name
    else
      Exit;

    vSession := TUserSession(AView.Session);
    vHolder := nil;
    vNewEntity := nil;
    vSession.DomainWrite(procedure
      begin
        vHolder := vSession.RetainChangeHolder(TChangeHolder(AParentHolder));
        if not Assigned(AView.Parent.ParentDomainObject) then
          vNewEntity := TDomain(AView.Domain).CreateNewEntity(vHolder, vDefinitionName, cNewID)
        else begin
          vMasterEntity := TEntity(AView.Parent.ParentDomainObject);
          vNewEntity := TDomain(AView.Domain)[vDefinitionName]._CreateNewEntity(vHolder, cNewID,
            '', [], vMasterEntity.FieldByName(vFieldDef.Name));
        end;
      end);

    try
      vResult := AInteractor.ShowEntityEditor(AView.Parent, vHolder, '');
      if vResult then
        AView.Parent.SetDomainObject(vNewEntity);
    finally
      vSession.DomainWrite(procedure
        begin
          vSession.ReleaseChangeHolder(vHolder, vResult);
        end);
    end;
  end;

  procedure InternalView(const AEntity: TEntity);
  begin
    if Assigned(AEntity) then
      AInteractor.ViewEntity(AView.Parent);
  end;

begin
  AInteractor := TInteractor(AView.Interactor);

  if AView.Definition is TRTFReport then
  begin
    if Assigned(AContext) then
      ShowRTFReport(AInteractor, TRTFReport(AView.Definition), TEntity(AContext));
    Exit(True);
  end
  else if AView.Definition is TReportDef then
  begin
    if Assigned(AContext) then
      ShowReport(AInteractor, TReportDef(AView.Definition), TEntity(AContext));
    Exit(True);
  end;

  Result := DoExecuteAction(AView, AActionName, AContext, AParams, AParentHolder);
  if Result then
    Exit;

  for vInclusion in FInclusions do
  begin
    Result := vInclusion.DoExecuteAction(AView, AActionName, AContext, AParams, AParentHolder);
    if Result then
      Exit;
  end;

  Result := True;
  // Обработка бесконтекстных действий
  if (AActionName = 'Exit') or (AActionName = 'Close') or (AActionName = 'Cancel') or (AActionName = 'Quit')
    or (AActionName = 'Ok') then
  begin
    if AActionName = 'Ok' then
      vModalResult := mrOk
    else if AActionName = 'Cancel' then
      vModalResult := mrCancel
    else
      vModalResult := mrNone;

    AInteractor.UIBuilder.CloseCurrentArea(vModalResult);
    Exit;
  end
  else if AActionName = 'ShowAbout' then
  begin
    TPresenter(AInteractor.Presenter).ShowPage(AInteractor, 'about');
    Exit;
  end
  else if AActionName = 'ChangePassword' then
  begin
    TPresenter(AInteractor.Presenter).ShowPage(AInteractor, 'about');
    Exit;
  end
  else if AActionName = 'ShowSysLog' then
  begin
    TDomain(AInteractor.Domain).LoadCollection('SysLog');
    TDomain(AInteractor.Domain).LoadCollection('SysLogActions');
    AInteractor.UIBuilder.Navigate(AInteractor.UIBuilder.RootView.BuildView('SysLog'), 'WorkArea', 'Collection');
    Exit;
  end
  else if AActionName = 'SetupRTFReports' then
  begin
    TPresenter(AInteractor.Presenter).ShowPage(AInteractor, 'rtf_reports');
    Exit;
  end
  else if AActionName = 'ShowSettings' then
  begin
    vConfig := TDomain(AInteractor.Domain).FirstEntity('SysConstants');
    if Assigned(vConfig) then
      Result := AInteractor.AtomicEditEntity(AInteractor.UIBuilder.RootView.BuildView('SysConstants/Current'), AParentHolder);
    Exit;
  end
  else if AActionName = 'ShowOptions' then
  begin
    TPresenter(AInteractor.Presenter).ShowPage(AInteractor, 'options');
    Exit;
  end
  else if AActionName = 'LoadChanges' then
  begin
    TUserSession(AInteractor.Session).ReloadDomainChanges(TUserSession(AInteractor.Session).NullHolder);
    Exit;
  end
  else if AActionName = 'ShowStartPage' then
  begin
    vStartPageName := TDomain(AInteractor.Domain).Settings.GetValue('Core', 'StartPage', '');
    if (vStartPageName <> '')
      and FileExists(TDomain(AInteractor.Domain).Configuration.FindLayoutFile(vStartPageName, LAYOUT_DFM_EXT))
    then
      AInteractor.UIBuilder.Navigate(nil, 'WorkArea', vStartPageName, '', TUserSession(AInteractor.Session).NullHolder);

    Exit;
  end
  else if AActionName = 'GetAllUpdates' then
  begin
    jChanges := TDomain(AInteractor.Domain).Storage.GetChanges(0);
    if Assigned(jChanges) then
      try
        jChanges.SaveToUTF8File('all_changes.txt');
      finally
        FreeAndNil(jChanges);
      end;
    Exit;
  end
  else if AActionName = 'ApplyAllUpdates' then
  begin
    if FileExists('all_changes.txt') then
    begin
      jChanges := TJSONObject.LoadFromUTF8File('all_changes.txt');
      try
        TDomain(AInteractor.Domain).ImportChanges(jChanges);
      finally
        FreeAndNil(jChanges);
      end;
    end;
    Exit;
  end
  else if AActionName = 'ActualizeData' then
  begin
    ActualizeData(AInteractor.Domain);
    Exit;
  end
  else if AActionName = 'ArrangeMozaic' then
  begin
    TPresenter(AInteractor.Presenter).ArrangePages(AInteractor, waMozaic);
    Exit;
  end
  else if AActionName = 'ArrangeCascade' then
  begin
    TPresenter(AInteractor.Presenter).ArrangePages(AInteractor, waCascade);
    Exit;
  end
  else if AActionName = 'ArrangeHorz' then
  begin
    TPresenter(AInteractor.Presenter).ArrangePages(AInteractor, waTileHorz);
    Exit;
  end
  else if AActionName = 'ArrangeVert' then
  begin
    TPresenter(AInteractor.Presenter).ArrangePages(AInteractor, waTileVert);
    Exit;
  end
  else if AActionName = 'CloseAllWindows' then
  begin
    TPresenter(AInteractor.Presenter).CloseAllPages(AInteractor);
    AInteractor.UIBuilder.PrintHierarchy;
    Exit;
  end
  else if AActionName = 'Logout' then
  begin
    AInteractor.ShowMessage('Смена пользователя');
    //TPresenter(AInteractor.Presenter).Logout(AInteractor);
    Exit;
  end
  else if (AActionName = 'Save') then
  begin
    if Assigned(AParentHolder) and AParentHolder.IsModified then
      TUserSession(AInteractor.Session).Save(AParentHolder);
    Exit;
  end
  else if (AActionName = 'Create') then
  begin
    if Assigned(AView.Parent) and (AView.Parent.DefinitionKind = dkObjectField) then
      InternalCreate(AView);

    Exit;
  end
  else if not Assigned(AContext) then
  begin
    //AInteractor.ShowMessage('Нет контекста для вызова действия!');
    Exit(False);
  end;

  if AActionName = '#HandleDblClick' then
  begin
    // Редактирование проверяем через систему, просмотр доступен всегда
    vSelectedView := AView.Parent;
    if not Assigned(vSelectedView) then
      Exit;

    // Кейс EmptyCollection лейаутов (без кнопок) не проходит
    if vSelectedView.State = vsFullAccess then
    //vActionView := vSelectedView.ViewByName('Edit');
    //if Assigned(vActionView) and (vActionView.State = vsFullAccess) then
    //  vActionView.ExecuteAction(AParentHolder)
      InternalEdit(TEntity(AContext))
    else
      InternalView(TEntity(AContext));
  end
  else if AActionName = 'Add' then
  begin
    if AContext is TEntityList then
      InternalAdd(TEntityList(AContext));
  end
  else if (AActionName = 'Link') then
  begin
    if AContext is TEntityList then
    begin
      vEntity := AParams.ExtractEntity('SelectedEntity');
      if Assigned(vEntity) then
        TUserSession(AInteractor.Session).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
          begin
            TEntityList(AContext).LinkEntity(AHolder, vEntity);
            Result := True;
          end, AParentHolder);
    end;
  end
  else if (AActionName = 'Edit') then
  begin
    InternalEdit(TEntity(AContext));
  end
  else if (AActionName = 'OpenInPage') then
  begin
    InternalOpen(TEntity(AContext));
  end
  else if (AActionName = 'Delete') then
  begin
    InternalDelete(TEntity(AContext));
  end
  else if (AActionName = 'Refresh') then
  begin
    // Должно самостоятельно обновить UI
    if (AContext is TEntityList) and (TEntityList(AContext).FillerKind = lfkDefinition) then
      TUserSession(AInteractor.Session).ReloadDomainChanges(TUserSession(AInteractor.Session).NullHolder);
  end
  else if (AActionName = 'View') then
  begin
    InternalView(TEntity(AContext));
  end
  else if (AActionName = 'Show') then
  begin
    if Assigned(TEntity(AContext)) then
      AInteractor.ViewEntity(AView.Parent);
  end
  else if AContext is TEntity then
  begin
    vEntity := TEntity(AContext);
    if (AActionName = 'GeneratePassword') and vEntity.InstanceOf('SysUsers') then
    begin
      vNewPassword := '';
      for i := 1 to 8 do
        if Random(10) < 9 then
          vNewPassword := vNewPassword + Copy(cLegalSymbols, Random(64)+1, 1);

      TUserSession(AInteractor.Session).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
        begin
          vEntity._SetFieldValue(AHolder, 'PasswordText', vNewPassword);
          Result := True;
        end, AParentHolder);

      AInteractor.ShowMessage(AInteractor.Translate('txtNewPassword', 'Новый пароль') + ': ' + vNewPassword);
    end
    else if (AActionName = 'CheckEmailConnection') and vEntity.InstanceOf('SysConstants') then
    begin
      vMessage := CheckEmailConnection(AInteractor.Domain);

      TUserSession(AInteractor.Session).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
        begin
          vEntity._SetFieldValue(AHolder, 'IsMailVerified', vMessage = '');
          Result := True;
        end, AParentHolder);

      if vMessage = '' then
        vMessage := AInteractor.Translate('msgSMTPVerified', 'SMTP сервер настроен правильно');
      AInteractor.ShowMessage(vMessage);
    end
    else if vEntity.InstanceOf('SysDocuments') then
    begin
      if AActionName = 'ViewDocument' then
      begin
        vFileName := TPath.Combine(TPath.GetTempPath, TPath.GetFileName(vEntity.GetFieldValue('FileName')));
        vContentStream := vEntity.GetFieldBlob('Content');
        vContentStream.Position := 0;
        if vContentStream is TMemoryStream then
          TMemoryStream(vContentStream).SaveToFile(vFileName)
        else begin
          vStream := TMemoryStream.Create;
          try
            vStream.LoadFromStream(vContentStream);
            vStream.SaveToFile(vFileName);
          finally
            FreeAndNil(vStream);
          end;
        end;
        TPresenter(AInteractor.Presenter).OpenFile(vFileName);
      end
      else if AActionName = 'LoadDocument' then
      begin
        vFileName := vEntity['FullPathName'];
        if TPresenter(AInteractor.Presenter).ShowOpenDialog(vFileName, '', '', '', '') then
          TUserSession(AInteractor.Session).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
            begin
              vEntity._SetFieldValue(AHolder, 'FullPathName', vFileName);
              Result := True;
            end, AParentHolder);
      end
      else if AActionName = 'ClearDocument' then
      begin
        TUserSession(AInteractor.Session).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
          begin
            vEntity._SetFieldValue(AHolder, 'FullPathName', '');
            Result := True;
          end, AParentHolder);
      end
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

procedure TScript.Logined(const AInteractor: TInteractor);
var
  vInclusion: TInclusion;
begin
  for vInclusion in FInclusions do
    vInclusion.DoOnLogined(AInteractor);
  DoOnLogined(AInteractor);
end;

class function TScript.ScriptExists(const AName: string): Boolean;
begin
  Result := RegisteredScripts.Exists(AName);
end;

{ TInclusion }

procedure TInclusion.DoInit;
begin
  FVersion := '0.0';
  FAppTitle := '';
end;

{ TBaseScript }

function TBaseScript.ActionByName(const AName: string): TActionDef;
begin
  Result := TConfiguration(FConfiguration).Actions.ObjectByName(AName);
end;

function TBaseScript.AddAction(const AName, ACaption: string; const AImageID: Integer; const AFlags: Integer = 0): TActionDef;
begin
  Result := TConfiguration(FConfiguration).Actions.Add(AName, ACaption, AImageID, AFlags);
end;

function TBaseScript.AddDefinition(const AName, AAncestors, ACaption, AEmptyValue: string; const AFlags: Integer;
  const AKind: TCollectionKind): TDefinition;
begin
  Result := TConfiguration(FConfiguration).Definitions.Add(AName, AAncestors, AKind);
  Result.SetCaption(ACaption).SetEmptyValue(AEmptyValue).SetFlags(AFlags);
end;

function TBaseScript.AddDefinition(const AID: Integer; const AName, AAncestors, ACaption, AEmptyValue: string;
  const AFlags: Integer = 0; const AKind: TCollectionKind = clkLibrary): TDefinition;
begin
  Result := AddDefinition(AName, AAncestors, ACaption, AEmptyValue, AFlags, AKind).SetID(AID);
end;

function TBaseScript.AddEnumeration<T>(const AName: string): TEnumeration;
begin
  Result := TConfiguration(FConfiguration).Enumerations.AddEnum<T>(AName);
end;

function TBaseScript.AddMigration(const AVersion: string): TMigration;
begin
  Result := TConfiguration(FConfiguration).Migrations.AddMigration(AVersion);
end;

procedure TBaseScript.AddPlannedJob(const AName: string; const APeriod: Integer; const AHandler: TSchedulerProc;
  const ARefFieldName: string = ''; const ASnapToPeriod: Boolean = False);
var
  vPlannedJob: TPlannedJob;
begin
  vPlannedJob := TPlannedJob.Create(AName, APeriod, @AHandler, ARefFieldName, ASnapToPeriod);
  TConfiguration(FConfiguration).PlannedJobs.Add(vPlannedJob);
end;

procedure TBaseScript.AddSecuritySettings(const AHolder: TChangeHolder; const ACollection: TCollection;
  const ASubject: string; const AObject: string; const AStateName: string; const AFlags: Integer);
var
  vDomain: TDomain;
  vSubjects: TStrings;
  vSubject: TEntity;
  vObjects: TStrings;
  vStates: TStrings;
  vObjectPrefix: string;
  vStatePrefix: string;
  vPos: Integer;
  i, j, k: Integer;
begin
  vDomain := TDomain(ACollection.Domain);

  vSubjects := CreateDelimitedList(ASubject);

  vPos := Pos(':', AObject);
  if vPos > 0 then
  begin
    vObjectPrefix := Copy(AObject, 1, vPos - 1) + '.';
    vObjects := CreateDelimitedList(Copy(AObject, vPos + 1, Length(AObject) - vPos));
  end
  else begin
    vObjectPrefix := '';
    vObjects := CreateDelimitedList(AObject);
  end;

  vPos := Pos(':', AStateName);
  if vPos > 0 then
  begin
    vStatePrefix := Copy(AStateName, 1, vPos - 1) + '.';
    vStates := CreateDelimitedList(Copy(AStateName, vPos + 1, Length(AStateName) - vPos));
  end
  else begin
    vStatePrefix := '';
    vStates := CreateDelimitedList(AStateName);
  end;

  if vStates.Count = 0 then
    vStates.Add('');

  for i := 0 to vSubjects.Count - 1 do
  begin
    vSubject := vDomain['SysRoles'].FindOne(vDomain.DomainSession, 'Code=:Code', [vSubjects[i]]);
    Assert(Assigned(vSubject), 'Система не нашла роли [' + vSubjects[i] +  '] при задании прав пользователей');

    for j := 0 to vObjects.Count - 1 do
      for k := 0 to vStates.Count - 1 do
        ACollection.CreateDefaultEntity(AHolder, 0, '!Subject;!ObjectName;!StateName;!AccessFlags',
          [NativeInt(vSubject), vObjectPrefix + vObjects[j], vStatePrefix + vStates[k], AFlags]);
  end;

  FreeAndNil(vSubjects);
  FreeAndNil(vObjects);
  FreeAndNil(vStates);
end;

function TBaseScript.AddStateMachine<T>(const AName: string): TStateMachine;
begin
  Result := TConfiguration(FConfiguration).StateMachines.AddStateMachine<T>(AName);
end;

function TBaseScript.CheckCanChangeField(const AView: TView; const AEntity: TEntity; const AFieldName: string;
  const ANewValue: Variant; var AHandled: Boolean): Boolean;
begin
  Result := True;
  AHandled := False;
end;

constructor TBaseScript.Create(const AConfiguration: TObject);
begin
  inherited Create;
  FConfiguration := AConfiguration;
  DoInit;
end;

function TBaseScript.DefinitionByName(const AName: string): TDefinition;
begin
  Result := TConfiguration(FConfiguration)[AName];
end;

destructor TBaseScript.Destroy;
begin
  DoDeinit;
  FConfiguration := nil;
  inherited Destroy;
end;

procedure TBaseScript.DoActualizeData(const ADomain: TObject);
begin
end;

procedure TBaseScript.DoAfterEntityCreation(const AHolder: TChangeHolder; const AOwnerContext: TObject; const AEntity: TEntity);
begin
end;

procedure TBaseScript.DoAfterEntitySaved(const AHolder: TChangeHolder; const AEntity: TEntity);
begin
end;

procedure TBaseScript.DoBeforeEntityRemoving(const AHolder: TChangeHolder; const AEntity: TEntity);
begin
end;

procedure TBaseScript.DoBeforeEntitySaving(const AHolder: TChangeHolder; const AEntity: TEntity);
begin
end;

function TBaseScript.DoCalculateStyle(const AViewName: string; const AEntity: TEntity;
  var AHandled: Boolean): TColor;
begin
  Result := cNullColor;
  AHandled := False;
end;

function TBaseScript.DoCheckActionFlags(const AView: TView; const AActionName: string; const AContext: TObject;
  const AParams: TEntity): TViewState;
begin
  Result := vsUndefined;
end;

function TBaseScript.DoCheckField(const AEntity: TEntity; const AFieldName: string; var AHandled: Boolean): Boolean;
begin
  Result := True;
  AHandled := False;
end;

procedure TBaseScript.DoCreateDefaultEntities(const ADomain: TObject; const AHolder: TChangeHolder);
begin
end;

procedure TBaseScript.DoCreateDefinitions;
begin
end;

procedure TBaseScript.DoCreateMigrations;
begin
end;

procedure TBaseScript.DoDeinit;
begin
end;

function TBaseScript.DoExecuteAction(const AView: TView; const AActionName: string; const AContext: TObject;
  const AParams: TEntity; const AParentHolder: TChangeHolder): Boolean;
begin
  Result := False;
end;

function TBaseScript.DoExecuteCommand(const AExecutor: TObject; const ATask: TTaskHandle; const AFiber, ACommand: TObject): Boolean;
begin
  Result := False;
end;

procedure TBaseScript.DoFillActionParams(const AView: TView; const AActionName: string;
  const AContext: TObject; const AParams: TEntity);
begin
end;

function TBaseScript.DoGetParamValue(const AEntity: TEntity; const AParamName: string; var AHandled: Boolean): string;
begin
  Result := '';
  AHandled := False;
end;

function TBaseScript.DoGetReportValue(const AReportData: TReportData; const AParamName: string; const AIndex1,
  AIndex2: Integer; var AHandled: Boolean): TReportValue;
begin
  Result.Value := '';
  Result.FieldKind := fkString;
  Result.Extra := '';
  AHandled := False;
end;

procedure TBaseScript.DoOnDomainReady(const ADomain: TObject);
begin
end;

procedure TBaseScript.DoOnDomainStopped;
begin
end;

procedure TBaseScript.DoOnLogined(const AInteractor: TInteractor);
begin
end;

function TBaseScript.GetFullText(const AEntity: TEntity; var AHandled: Boolean): string;
begin
  Result := '';
  AHandled := False;
end;

function TBaseScript.GetParentListView(const AView: TView): TView;
begin
  // Для контекстных действий их состояние мы определяем по состоянию списка
  Result := AView.Parent;
  if Assigned(Result) then
  begin
    if not (Result.DefinitionKind in [dkCollection, dkListField]) then
    begin
      Assert(Result.DefinitionKind in [dkEntity, dkObjectField], 'Wrong context for action ' + AView.Name);
      if Assigned(Result.Parent) and (Result.Parent.DefinitionKind in [dkCollection, dkListField]) then
        Result := Result.Parent
      else
        Result := nil;
    end;
  end;
end;

function TBaseScript.GetVersionName: string;
begin
  Result := FVersionName;
  if Length(Result) = 0 then
    Result := FVersion;
end;

function TBaseScript.RegisterComplexClass(const AName: string; const AComplexClass: TComplexClass): TComplexClassDef;
begin
  Result := TConfiguration(FConfiguration).ComplexClasses.Add(AName, TObject(AComplexClass));
end;

procedure TBaseScript.RegisterReaction(const ADefinitionNames, AReactiveFields, AFieldChain: string;
  const AReactionProc: TReactionProc);
begin
  TConfiguration(FConfiguration).RegisterReaction(ADefinitionNames, AReactiveFields, AFieldChain, TProc(
    procedure(const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    begin
      AReactionProc(AHolder, AFieldChain, AEntity, AParameter);
    end));
end;

initialization

TScript.RegisteredScripts := TStringDictionary<TScriptInfo>.Create;
TInclusion.RegisteredInclusions := TStringDictionary<TInclusionClass>.Create;

finalization

FreeAndNil(TInclusion.RegisteredInclusions);
FreeAndNil(TScript.RegisteredScripts);

end.
