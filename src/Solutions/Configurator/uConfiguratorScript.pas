{---------------------------------------------------------------------------------
  X-Tend configuration example

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

unit uConfiguratorScript;

interface

uses
  Classes, uScript, uCollection, uEntity, uReport, uSession, uView, uConsts, uChangeManager, uInteractor;

type
  TConfiguratorScript = class(TScript)
  protected
    procedure DoInit; override;

    procedure DoCreateDefinitions; override;
    procedure DoCreateDefaultEntities(const ADomain: TObject; const AHolder: TChangeHolder); override;
    function GetFullText(const AEntity: TEntity; var AHandled: Boolean): string; override;
    function DoCheckActionFlags(const AView: TView; const AActionName: string;
      const AContext: TObject; const AParams: TEntity): TViewState; override;
    function CheckCanChangeField(const AView: TView; const AEntity: TEntity; const AFieldName: string;
      const ANewValue: Variant; var AHandled: Boolean): Boolean; override;
    procedure DoAfterEntityCreation(const AHolder: TChangeHolder; const AOwnerContext: TObject; const AEntity: TEntity); override;
    function DoExecuteAction(const AView: TView; const AActionName: string;
      const AContext: TObject; const AParams: TEntity; const AParentHolder: TChangeHolder): Boolean; override;
  end;

implementation

uses
  Windows, Dialogs, ClipBrd, Math, SysUtils, DateUtils, Variants, XMLIntf, XMLDoc, ZIP, RegularExpressions,
  IniFiles, IOUtils, StrUtils, Generics.Collections,
  uJSON, uConfiguration, uDefinition, uDomain, uTask, uWebTask, uPresenter, uSimpleField, uObjectField,
  uUtils, uDBConnector, uEnumeration, uEntityList, uComplexObject;

const
  cAdminID = 1;
  cAdminsID = 1;

procedure TConfiguratorScript.DoInit;
begin
  FAppTitle := 'Конфигуратор решений';
  FVersion := '0.1';
end;

// Вспомогательные функции

function GenerateModules(const ADomain: TDomain; const AProjectName, ATitle, AVersion: string): string;
var
  vPlatformPath: string;
  vResPath: string;
  vSrcPath: string;
  vBinPath: string;
  vProcessingFileName: string;
  vTargetFileName: string;
  vHandledText: string;
  vDBName: string;
  vGUID: TGUID;
  vFileContent: TStringStream;
  vIniFile: TIniFile;
begin
  Result := '';
  vPlatformPath := ADomain.Constant['PlatformFolder'];
  if vPlatformPath = '' then
    vPlatformPath := ExtractFilePath(ParamStr(0)) + '..\'
  else
    vPlatformPath := IncludeTrailingPathDelimiter(vPlatformPath);

  vResPath := TPath.Combine(ADomain.Configuration.ConfigurationDir, 'templates');
  if not TDirectory.Exists(vResPath) then
    Exit;

  // Подготовка директории для исходников
  vSrcPath := vPlatformPath + 'src\Solutions\' + AProjectName;
  ForceDirectories(vSrcPath);

  // Копирование иконки проекта
  vProcessingFileName := TPath.Combine(vResPath, 'favicon.ico');
  vTargetFileName := TPath.Combine(vSrcPath, 'favicon.ico');
  if TFile.Exists(vProcessingFileName) and not TFile.Exists(vTargetFileName) then
    TFile.Copy(vProcessingFileName, vTargetFileName);

  vFileContent := TStringStream.Create('', TEncoding.UTF8);
  try
    // Подготовка *.dproj файла
    vProcessingFileName := TPath.Combine(vResPath, 'NewApplication.dproj');
    if TFile.Exists(vProcessingFileName) then
    begin
      vFileContent.LoadFromFile(vProcessingFileName);
      CreateGUID(vGUID);
      vHandledText := ReplaceStr(vFileContent.DataString, '$ProjectGuid$', UpperCase(GUIDToString(vGUID)));
      vHandledText := ReplaceStr(vHandledText, '$ProjectName$', AProjectName);
      vFileContent.Size := 0;
      vFileContent.WriteString(vHandledText);
      Result := TPath.Combine(vSrcPath, AProjectName + '.dproj');
      vFileContent.SaveToFile(Result);
    end;

    // Подготовка *.dpr файла
    vProcessingFileName := TPath.Combine(vResPath, 'NewApplication.dpr');
    if TFile.Exists(vProcessingFileName) then
    begin
      vFileContent.LoadFromFile(vProcessingFileName);
      vHandledText := ReplaceStr(vFileContent.DataString, 'NewApplication', AProjectName);
      vFileContent.Size := 0;
      vFileContent.WriteString(vHandledText);
      vFileContent.SaveToFile(TPath.Combine(vSrcPath, AProjectName + '.dpr'));
    end;

    // Подготовка скрипта (*.pas файла)
    vProcessingFileName := TPath.Combine(vResPath, 'NewScript.pas');
    if TFile.Exists(vProcessingFileName) then
    begin
      vFileContent.LoadFromFile(vProcessingFileName);
      vHandledText := ReplaceStr(vFileContent.DataString, 'NewApplication', AProjectName);
      vHandledText := ReplaceStr(vHandledText, '$ProjectTitle$', ATitle);
      vHandledText := ReplaceStr(vHandledText, '$ProjectVersion$', AVersion);
      vFileContent.Size := 0;
      vFileContent.WriteString(vHandledText);
      vFileContent.SaveToFile(TPath.Combine(vSrcPath, 'u' + AProjectName + 'Script.pas'));
    end;
  finally
    FreeAndNil(vFileContent);
  end;

  // Подготовка директории для бинарников
  vBinPath := vPlatformPath + 'bin\solutions\' + LowerCase(AProjectName);
  ForceDirectories(vBinPath);

  // Копирование пустой базы данных
  vDBName := LowerCase(AProjectName) + '.mdb';
  vProcessingFileName := TPath.Combine(vResPath, 'storage.mdb');
  vTargetFileName := TPath.Combine(vBinPath, vDBName);
  if TFile.Exists(vProcessingFileName) and not TFile.Exists(vTargetFileName) then
    TFile.Copy(vProcessingFileName, vTargetFileName);

  if TDirectory.Exists(TPath.Combine(vResPath, 'layouts')) then
    TDirectory.Copy(TPath.Combine(vResPath, 'layouts'), TPath.Combine(vBinPath, 'layouts'));

  vIniFile := TIniFile.Create(TPath.Combine(vBinPath, 'settings.ini'));
  try
    if not vIniFile.ValueExists('Core', 'Deployment') then
      vIniFile.WriteString('Core', 'Deployment', 'prod');
    if not vIniFile.ValueExists('Core', 'EnvironmentID') then
      vIniFile.WriteString('Core', 'EnvironmentID', UpperCase(GUIDToString(vGUID)));
    if not vIniFile.ValueExists('Core', 'AutoLogin') then
      vIniFile.WriteString('Core', 'AutoLogin', '1');
    if not vIniFile.ValueExists('MSAccess', 'Database') then
      vIniFile.WriteString('MSAccess', 'Database', vDBName);
    if not vIniFile.ValueExists('Modules', 'DataStorage') then
      vIniFile.WriteString('Modules', 'DataStorage', 'SQLite');
    if not vIniFile.ValueExists('Modules', 'ReportEngine') then
      vIniFile.WriteString('Modules', 'ReportEngine', 'FastReport');
    if not vIniFile.ValueExists('Modules', 'ChartPainter') then
      vIniFile.WriteString('Modules', 'ChartPainter', 'VCL');
    if not vIniFile.ValueExists('Modules', 'UI') then
      vIniFile.WriteString('Modules', 'UI', 'Windows.DevExpress');
    if not vIniFile.ValueExists('Modules', 'TaskEngine') then
      vIniFile.WriteString('Modules', 'TaskEngine', 'SimpleEngine');
  finally
    FreeAndNil(vIniFile);
  end;
end;

// Scheduler

function DoScheduledAction(const ADomain: TObject): Integer;
begin
  Result := 0;
end;

// Handlers

procedure OnSomethingChanged(const ASession: TUserSession; const AHolder: TChangeHolder;
  const AFieldChain: string; const AEntity, AParam: TEntity);
begin
end;

{ TConfiguratorScript }

function TConfiguratorScript.CheckCanChangeField(const AView: TView; const AEntity: TEntity;
  const AFieldName: string; const ANewValue: Variant; var AHandled: Boolean): Boolean;
begin
  Result := True;
end;

procedure TConfiguratorScript.DoAfterEntityCreation(const AHolder: TChangeHolder; const AOwnerContext: TObject; const AEntity: TEntity);
begin
  // Здесь мы в контексте редактирования
  if AEntity.InstanceOf('') then
  begin
    //
  end
  else
    inherited DoAfterEntityCreation(AHolder, AOwnerContext, AEntity);
end;

function TConfiguratorScript.DoCheckActionFlags(const AView: TView; const AActionName: string;
  const AContext: TObject; const AParams: TEntity): TViewState;
begin
  Result := inherited DoCheckActionFlags(AView, AActionName, AContext, AParams);
  if Result <> vsUndefined then
    Exit;
end;

procedure TConfiguratorScript.DoCreateDefaultEntities(const ADomain: TObject; const AHolder: TChangeHolder);
var
  vDomain: TDomain absolute ADomain;
  vCollection: TCollection;
begin
  inherited DoCreateDefaultEntities(ADomain, AHolder);

  vCollection := vDomain['SysUsers'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'Login;Name;!Password',
    ['admin', 'Администратор', Md5Hash('')], True);

  vCollection := vDomain['SysUsersRoles'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'User;Role;Flags', [
    Integer(vDomain.EntityByID('SysUsers', 1)), Integer(vDomain.EntityByID('SysRoles', 1))], True);

  vCollection := vDomain['SysConstants'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'Code;MailLogin;MailPassword', ['debug', 'noname@mail.ru', '']);
end;

procedure TConfiguratorScript.DoCreateDefinitions;
var
  vDefinition: TDefinition;
  //vListFieldDef: TListFieldDef;
  vActionDef: TActionDef;
begin
  AddEnumeration<TCollectionKind>('CollectionKinds').AddDisplayNames(cCollectionKindNames);
  AddEnumeration<TFieldKind>('FieldKinds').AddDisplayNames(cFieldKindNames);
  AddEnumeration<TLayoutKind>('LayoutKinds').AddDisplayNames(cLayoutKindNames);
  AddEnumeration<TStorageKind>('StorageKinds').AddDisplayNames(cStorageKindNames);
  AddEnumeration<TSearchType>('SearchTypes').AddDisplayNames(cSearchTypeNames);
  AddEnumeration<TEntitySortType>('SortTypes').AddDisplayNames(cSortTypeNames);
  AddEnumeration<TEnumType>('EnumTypes').AddDisplayNames(cEnumTypeNames);
  AddEnumeration<TRelationPower>('RelationPowers').AddDisplayNames(cRelationPowerNames);
  AddEnumeration<TBlobFormat>('BlobFormats').AddDisplayNames(cBlobFormatNames);
  AddEnumeration<TIntegrationKind>('IntegrationKinds').AddDisplayNames(cIntegrationKindNames);
  AddEnumeration<THTTPHeaderMethod>('HTTPMethods').AddDisplayNames(cHTTPMethodNames);
  AddEnumeration<TAuthorizationType>('AuthorizationTypes').AddDisplayNames(cAuthorizationTypeNames);
  AddEnumeration<TColorTarget>('ColorTargets').AddDisplayNames(cColorTargetNames);
  AddEnumeration<TCommitKind>('CommitKinds').AddDisplayNames(cCommitKindNames);
  AddEnumeration<TViewStates>('ViewStates').AddIDs(cViewStateIDs).AddDisplayNames(cViewStateCaptions);
  AddEnumeration<TDefinitionFlags>('DefinitionFlags').AddIDs(cDefinitionFlagIDs).AddDisplayNames(cDefinitionFlagCaptions);
  AddEnumeration<TFieldFlags>('FieldFlags').AddIDs(cFieldFlagIDs).AddDisplayNames(cFieldFlagCaptions);
  AddEnumeration<TChangeKinds>('ChangeKinds').AddIDs(cChangeKindIDs).AddDisplayNames(cChangeKindCaptions);

  vDefinition := DefinitionByName('SysConstants');
  vDefinition.AddSimpleFieldDef('PlatformFolder', 'platform_folder', 'Корневая папка платформы', Null, Null, 255, fkString, 'dir');

  vDefinition := AddDefinition('WebServices', '', 'Web службы', cNullItemName);
  vDefinition.AddSimpleFieldDef('Kind', 'kind', 'Тип', ikEmail, Null, Null, fkEnum, '', '', vsFullAccess, 0, 'IntegrationKinds');
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Название', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Description', 'description', 'Описание', Null, Null, 250, fkString, '', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('BaseURL', 'base_url', 'Основной адрес', Null, Null, 250, fkString, '', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('AuthorizationType', 'authorization_type', 'Тип авторизации', atBasic, Null, Null, fkEnum, '', '', vsFullAccess, 0, 'AuthorizationTypes');
  vDefinition.AddSimpleFieldDef('AuthTemplate', 'auth_template', 'Шаблон строки входа', Null, Null, 150, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Headers', 'headers', 'Общие заголовки', Null, Null, -1, fkString, 'memo', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('Parameters', 'parameters', 'Общие параметры', Null, Null, -1, fkString, 'memo', '', vsFullAccess);
  vDefinition.AddListFieldDef('Requests', 'WebService', 'Доступные запросы', 'WebRequests', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddListFieldDef('Credentials', 'WebService', 'Учетные данные', 'WebServiceCredentials', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);

  vDefinition := AddDefinition('WebRequests', '', 'Запросы Web', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('WebService', 'web_service', 'Web служба', '', 'WebServices', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Название', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Method', 'method', 'Метод', hhmGet, Null, Null, fkEnum, '', '', vsFullAccess, 0, 'HTTPMethods');
  vDefinition.AddSimpleFieldDef('URL', 'url', 'Ссылка', Null, Null, 250, fkString, '', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('Headers', 'headers', 'Заголовки', Null, Null, -1, fkString, 'memo', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('Body', 'body', 'Тело', Null, Null, -1, fkString, 'memo', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('Parameters', 'parameters', 'Параметры', Null, Null, -1, fkString, 'memo', '', vsFullAccess);
  vActionDef := vDefinition.AddAction('Execute', 'Выполнить', -1);

  vDefinition := AddDefinition('WebServiceCredentials', '', 'Учетные данные', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('WebService', 'web_service', 'Web служба', '', 'WebServices', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('APIKey', 'api_key', 'Ключ API', Null, Null, 100, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('ValidTill', 'valid_till', 'Действителен до', Null, Null, Null, fkDateTime, 'datetime', '', vsFullAccess);


//  vDefinition := AddDefinition('Integrations', '', 'Интеграции', cNullItemName{, ccHideInMenu});
//  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
//  vDefinition.AddSimpleFieldDef('Kind', 'kind', 'Тип', ikEmail, Null, Null, fkEnum, '', '', vsFullAccess, cRequired, 'IntegrationKinds');

  vDefinition := AddDefinition('_Configurations', '', 'Конфигурации', cNullItemName, ccHideInMenu, clkMixin);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Caption', 'caption', 'Заголовок', Null, Null, 80, fkString, '', '', vsFullAccess, cRequired or cLocalizable);
  vDefinition.AddSimpleFieldDef('Version', 'version', 'Версия', Null, Null, 15, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddListFieldDef('Icons', 'Configuration', 'Иконки', 'Icons', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddListFieldDef('Enumerations', 'Configuration', 'Перечисления', 'Enumerations', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddListFieldDef('StateMachines', 'Configuration', 'Машины состояний', 'StateMachines', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddListFieldDef('Definitions', 'Configuration', 'Коллекции', 'Definitions', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  //vDefinition.AddListFieldDef('Layouts', 'Configuration', 'Разметки отображения', 'Layouts', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);

  vDefinition := AddDefinition('Inclusions', '_Configurations', 'Включения', cNullItemName, 0);

  vDefinition := AddDefinition('Configurations', '_Configurations', 'Конфигурации', cNullItemName, 0);
  vDefinition.AddAction('MakeSolution', 'Создать приложение', 0);

  vDefinition := AddDefinition('Icons', '', 'Иконки', cNullItemName{, ccHideInMenu});
  vDefinition.AddEntityFieldDef('Configuration', 'configuration', 'Конфигурация', '', '_Configurations', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('IconID', 'icon_id', 'Идентификатор', 0, 0, Null, fkInteger, '', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Caption', 'caption', 'Заголовок', Null, Null, 80, fkString, '', '', vsFullAccess, cRequired or cLocalizable);
  vDefinition.AddSimpleFieldDef('FileName', 'file_name', 'Наименование файла', Null, Null, 250, fkString);
  //vListFieldDef :=
  vDefinition.AddListFieldDef('IconData', 'Icon', 'Данные иконок', 'IconData', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddUniqueIndex('Name');

  vDefinition := AddDefinition('IconResolutions', '', 'Размеры иконок', cNullItemName{, ccHideInMenu});
  vDefinition.AddSimpleFieldDef('Size', 'size', 'Размер', 16, 1, Null, fkInteger, '', '', vsFullAccess);
  vDefinition.AddUniqueIndex('Size');
  vDefinition.RegisterReaction('Name', 'Size', TProc(procedure(const ASession: TUserSession;
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    begin
      AEntity._SetFieldValue(AHolder, 'Name', Format('%dx%d', [AEntity['Size'], AEntity['Size']]));
    end));

  vDefinition := AddDefinition('IconData', '', 'Данные иконок', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('Icon', 'icon', 'Иконка', '', 'Icons', 0, vsHidden);
  vDefinition.AddEntityFieldDef('Resolution', 'resolution', 'Размер', '', 'IconResolutions', 1, vsSelectOnly, cRequired);
  vDefinition.AddBlobFieldDef('Content', 'content', 'Содержимое', bffImage, '', vsReadOnly);
  vDefinition.AddUniqueIndex('Icon,Resolution');
  vDefinition.RegisterReaction('Name', 'Name.Icon;Name.Resolution;Resolution', TProc(procedure(const ASession: TUserSession;
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    begin
      AEntity._SetFieldValue(AHolder, 'Name', Format('%dx%d', [AEntity['Size'], AEntity['Size']]));
    end));

  vDefinition := AddDefinition('_Enumerations', '', 'Перечисления', cNullItemName, 0, clkMixin);
  vDefinition.AddEntityFieldDef('Configuration', 'configuration', 'Конфигурация', '', '_Configurations', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Caption', 'caption', 'Заголовок', Null, Null, 80, fkString, '', '', vsFullAccess, cRequired or cLocalizable);
  vDefinition.AddSimpleFieldDef('Kind', 'kind', 'Тип нумерации', etOrdinal, Null, Null, fkEnum, '', '', vsFullAccess, 0, 'EnumTypes');
  vDefinition.AddSimpleFieldDef('Prefix', 'prefix', 'Префикс для элементов', Null, Null, 10, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddListFieldDef('Items', 'Enumeration', 'Значения', 'EnumItems', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddUniqueIndex('Configuration,Prefix;Configuration,Name');

  vDefinition := AddDefinition('EnumItems', '', 'Значения перечисления', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('Enumeration', 'enumeration', 'Перечисление', '', '_Enumerations', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Caption', 'caption', 'Заголовок', Null, Null, 80, fkString, '', '', vsFullAccess, cRequired or cLocalizable);
  vDefinition.AddSimpleFieldDef('Value', 'value', 'Значение', 0, Null, Null, fkInteger, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('Color', 'color', 'Цвет', cNullColor, Null, Null, fkColor, '', '', vsFullAccess, 0);
  vDefinition.AddEntityFieldDef('Icon', 'icon', 'Иконка', '', 'Icons', 0, vsSelectOnly);
  vDefinition.AddUniqueIndex('Enumeration,Name;Enumeration,Value');
  vDefinition.RegisterReaction('Value', 'Enumeration', TProc(procedure(const ASession: TUserSession;
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    var
      vEnumType: TEnumType;
      vEnumItems: TListField;
      vItem: TEntity;
      vMaxValue: Integer;
    begin
      if not Assigned(AParameter) then
        Exit;

      vEnumType := TEnumType(Integer(AParameter['Kind']));
      vEnumItems := TListField(AParameter.FieldByName('Items'));
      vMaxValue := -1;
      for vItem in vEnumItems do
        if vItem <> AEntity then
          vMaxValue := Max(vItem['Value'], vMaxValue);

      if vMaxValue < 0 then
        vMaxValue := 0
      else if vEnumType = etBitwise then
      begin
        if vMaxValue < 2 then
          vMaxValue := vMaxValue + 1
        else
          vMaxValue := Round(Power(2, Log2(vMaxValue) + 1))
      end
      else
        vMaxValue := vMaxValue + 1;

      AEntity._SetFieldValue(AHolder, 'Value', vMaxValue);
    end));
  vDefinition.RegisterReaction('Caption', 'Name', TProc(procedure(const ASession: TUserSession;
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    begin
      AEntity._SetFieldValue(AHolder, 'Caption', AEntity['Name']);
    end));

  vDefinition := AddDefinition('Enumerations', '_Enumerations', 'Перечисления', cNullItemName, 0);

  vDefinition := AddDefinition('StateMachines', '_Enumerations', 'Машины состояний', cNullItemName, 0);
  vDefinition.AddListFieldDef('Transitions', 'Enumeration', 'Значения', 'StateTransitions', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);

  vDefinition := AddDefinition('StateTransitions', '', 'Доступные переходы', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('StateMachine', 'state_machine', 'Машина состояний', '', 'StateMachines', 0, vsHidden);
  vDefinition.AddEntityFieldDef('StateFrom', 'state_from', 'Из состояния', '', 'EnumItems', 0, vsSelectOnly, cRequired).SetDependenciesAndFilter('Enumeration=StateMachine', '');
  vDefinition.AddEntityFieldDef('StateTo', 'state_to', 'В состояние', '', 'EnumItems', 0, vsSelectOnly, cRequired).SetDependenciesAndFilter('Enumeration=StateMachine', '');
  vDefinition.AddUniqueIndex('StateFrom,StateTo');
  vDefinition.RegisterReaction('Name', 'Name.StateFrom;StateFrom;Name.StateTo;StateTo', TProc(procedure(const ASession: TUserSession;
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    begin
      AEntity._SetFieldValue(AHolder, 'Name', SafeDisplayName(AEntity.ExtractEntity('StateFrom')) +
        ' >> ' + SafeDisplayName(AEntity.ExtractEntity('StateTo')));
    end));

  vDefinition := AddDefinition('_Definitions', '', 'Определения', cNullItemName, ccHideInMenu, clkMixin);
  vDefinition.AddEntityFieldDef('Configuration', 'configuration', 'Конфигурация', '', '_Configurations', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('FullName', 'full_name', 'Полное наименование', Null, Null, 100, fkString, '', '', vsHidden, cCalculated);
  vDefinition.AddSimpleFieldDef('Caption', 'caption', 'Заголовок', Null, Null, 80, fkString, '', '', vsFullAccess, cRequired or cLocalizable);
  vDefinition.AddSimpleFieldDef('EmptyValue', 'empty_value', 'Вывести при отсутствии значения', cNullItemName, Null, 150, fkString, '', '', vsFullAccess, cLocalizable);
  vDefinition.AddSimpleFieldDef('Description', 'description', 'Описание', Null, Null, 250, fkString, '', '', vsFullAccess);
  vDefinition.AddEntityFieldDef('Icon', 'icon', 'Иконка', '', 'Icons', 0, vsSelectOnly);
  vDefinition.AddSimpleFieldDef('StorageName', 'storage_name', 'Наименование хранилища', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('StorageKind', 'storage_kind', 'Тип хранилища', skDatabase, Null, Null, fkEnum, '', '', vsFullAccess, 0, 'StorageKinds');
  vDefinition.AddSimpleFieldDef('Kind', 'kind', 'Тип', clkLibrary, Null, Null, fkEnum, '', '', vsFullAccess, cRequired, 'CollectionKinds');
  vDefinition.AddSimpleFieldDef('UIState', 'ui_state', 'Доступность', _vsFullAccess, _vsHidden, _vsUndefined, fkEnum, '', '', vsFullAccess, 0, 'ViewStates');
  vDefinition.AddSimpleFieldDef('Flags', 'flags', 'Флаги', ccHideInMenu, 0, Null, fkFlag, '', '', vsFullAccess, 0, 'DefinitionFlags');
  vDefinition.AddListFieldDef('Ancestors', 'Child', 'Предки', 'DefinitionMixins', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddListFieldDef('Descendants', 'Parent', 'Потомки', 'DefinitionMixins', '', '', vsReadOnly, 0, estUserSort, '', rpStrong);
  vDefinition.AddListFieldDef('Fields', 'Definition', 'Поля', '_Fields', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddSimpleFieldDef('GroupFieldName', 'group_field_name', 'Поле группировки', Null, Null, 250, fkString, 'fieldpath', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('ColorFieldName', 'color_field_name', 'Поле цвета', Null, Null, 250, fkString, 'fieldpath', '', vsFullAccess);
  vDefinition.AddEntityFieldDef('StateField', 'state_field', 'Поле состояния', '', 'StateFields', 0, vsFullAccess); //field_path
  vDefinition.AddSimpleFieldDef('ColorTarget', 'color_target', 'Зона раскраски', ctNone, ctNone, ctText, fkEnum, '', '', vsFullAccess, 0, 'ColorTargets');
  vDefinition.RegisterReaction('FullName', 'Name', TProc(procedure(const ASession: TUserSession;
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    begin
      AEntity._SetFieldValue(AHolder, 'FullName', AEntity['Name']);
    end));
  vDefinition.RegisterReaction('StorageName', 'Name', TProc(procedure(const ASession: TUserSession;
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    begin
      if AEntity.IsNew then
        AEntity._SetFieldValue(AHolder, 'StorageName', BuildStorageName(AEntity['Name']));
    end));

  vDefinition := AddDefinition('Definitions', '_Definitions', 'Коллекции', cNullItemName, 0);
  vDefinition.AddListFieldDef('DefaultEntities', 'Definition', 'Записи по умолчанию', 'DefaultEntities', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);

  vDefinition := AddDefinition('DefaultEntities', '', 'Записи по умолчанию', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('Definition', 'definition', 'Определение', '', 'Definitions', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('Identifier', 'identifier', 'Идентификатор', 1, 1, Null, fkInteger, '', '', vsFullAccess, cRequired);
  vDefinition.AddListFieldDef('Fields', 'Entity', 'Поля', '_FieldValues', '', '', vsSelectOnly, 0, estUserSort, '', rpStrong);
  vDefinition.AddUniqueIndex('Definition,Identifier');
  // Заполнить поля при создании

  vDefinition := AddDefinition('_FieldValues', '', 'Значения полей', cNullItemName, ccHideInMenu, clkMixin);
  vDefinition.AddEntityFieldDef('Entity', 'entity', 'Запись', '', 'DefaultEntities', 0, vsHidden);
  vDefinition.AddEntityFieldDef('Field', 'field', 'Описание поля', '', '_Fields', 0, vsReadOnly);
  vDefinition.AddSimpleFieldDef('TextValue', 'text_value', 'Значение', Null, Null, 50, fkString, '', '', vsReadOnly);

  vDefinition := AddDefinition('StringValues', '_FieldValues', 'Строковые поля', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Value', 'value', 'Значение', Null, Null, 250, fkString, '', '', vsFullAccess, 0);

  vDefinition := AddDefinition('IntegerValues', '_FieldValues', 'Целочисленные поля', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Value', 'value', 'Значение', 0, Null, Null, fkInteger, '', '', vsFullAccess, 0);

  vDefinition := AddDefinition('ColorValues', '_FieldValues', 'Цветовые поля', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Value', 'value', 'Значение', 0, Null, Null, fkColor, '', '', vsFullAccess, 0);

  vDefinition := AddDefinition('EnumValues', '_FieldValues', 'Перечисления', cNullItemName, ccSystem or ccHideInMenu);
  //vDefinition.AddEntityFieldDef('Value', 'value', 'Значение', '', 'EnumItems', 0, vsSelectOnly, 0, estSortByID).SetDependenciesAndFilter('Enumeration=Enumeration', '');

  vDefinition := AddDefinition('StateValues', '_FieldValues', 'Состояния', cNullItemName, ccSystem or ccHideInMenu);
  //vDefinition.AddEntityFieldDef('Value', 'value', 'Значение', '', 'EnumItems', 0, vsSelectOnly, 0).SetDependenciesAndFilter('Enumeration=Enumeration', '');

  // enum и flag поля очень похожи
  vDefinition := AddDefinition('FlagValues', '_FieldValues', 'Мультивыбор', cNullItemName, ccSystem or ccHideInMenu);
  //vDefinition.AddEntityFieldDef('Value', 'value', 'Значение', '', 'EnumItems', 0, vsSelectOnly, 0).SetDependenciesAndFilter('Enumeration=Enumeration', '');

  vDefinition := AddDefinition('FloatValues', '_FieldValues', 'Вещественные поля', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Value', 'value', 'Значение', 0.0, Null, Null, fkFloat, '', '', vsFullAccess, 0);

  vDefinition := AddDefinition('CurrencyValues', '_FieldValues', 'Денежные поля', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Value', 'value', 'Значение', 0.0, Null, Null, fkCurrency, '', '', vsFullAccess, 0);

  vDefinition := AddDefinition('DateTimeValues', '_FieldValues', 'Поля даты/времени', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Value', 'value', 'Значение', Null, Null, Null, fkDateTime, '', '', vsFullAccess, 0);

  vDefinition := AddDefinition('BooleanValues', '_FieldValues', 'Булевы поля', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Value', 'value', 'Значение', False, Null, Null, fkBoolean, '', '', vsFullAccess, 0);

  vDefinition := AddDefinition('ObjectValues', '_FieldValues', 'Поля связей', cNullItemName, ccSystem or ccHideInMenu);
  //vDefinition.AddEntityFieldDef('Value', 'value', 'Значение', '', 'EnumItems', 0, vsSelectOnly, 0, estSortByID).SetDependenciesAndFilter('Enumeration=Enumeration', '');

  vDefinition := AddDefinition('ListValues', '_FieldValues', 'Поля списков', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddListFieldDef('Values', 'List', 'Значения', 'ListEntities', '', '', vsSelectOnly, 0, estUserSort, '', rpStrong);

  vDefinition := AddDefinition('BlobValues', '_FieldValues', 'Поля данных', cNullItemName, ccSystem or ccHideInMenu);

  vDefinition := AddDefinition('ComplexValues', '_FieldValues', 'Поля нативных данных', cNullItemName, ccSystem or ccHideInMenu);

  vDefinition := AddDefinition('ListEntities', '', 'Записи в списке', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddEntityFieldDef('List', 'list', 'Список', '', 'ListValues', 0, vsHidden, cRequired);
  vDefinition.AddEntityFieldDef('Entity', 'entity', 'Запись', '', 'DefaultEntities', 0, vsSelectOnly, cRequired);


  vDefinition := AddDefinition('DefinitionMixins', '', 'Отношения', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('Configuration', 'configuration', 'Конфигурация', '', 'Configurations', 0, vsHidden);
  vDefinition.AddEntityFieldDef('Parent', 'parent', 'Предок', '', 'Definitions', 0, vsSelectOnly, cRequired).SetDependenciesAndFilter('', '&(*!=*,Configuration=Configuration)');
  vDefinition.AddEntityFieldDef('Child', 'child', 'Потомок', '', 'Definitions', 0, vsSelectOnly, cRequired).SetDependenciesAndFilter('', '&(*!=*,Configuration=Configuration)');
  vDefinition.AddUniqueIndex('Parent,Child');
  vDefinition.RegisterReaction('Name', 'Name.Parent;Parent;Name.Child;Child', TProc(procedure(const ASession: TUserSession;
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    begin
      AEntity._SetFieldValue(AHolder, 'Name', SafeDisplayName(AEntity.ExtractEntity('Parent')) +
        ' <- ' + SafeDisplayName(AEntity.ExtractEntity('Child')));
    end));
  vDefinition.RegisterReaction('Configuration', 'Parent;Child', TProc(procedure(const ASession: TUserSession;
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    begin
      if not Assigned(AEntity.ExtractEntity('Configuration')) and Assigned(AParameter) then
        AEntity._SetFieldEntity(AHolder, 'Configuration', AParameter.ExtractEntity('Configuration'));
    end));

  vDefinition := AddDefinition('_Fields', '', 'Поля сущностей', cNullItemName, ccHideInMenu, clkMixin);
  vDefinition.AddEntityFieldDef('Definition', 'definition', 'Определение', '', '_Definitions', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('FullName', 'full_name', 'Полное наименование', Null, Null, 100, fkString, '', '', vsHidden, cCalculated);
  vDefinition.AddSimpleFieldDef('Caption', 'caption', 'Заголовок', Null, Null, 80, fkString, '', '', vsFullAccess, cLocalizable);
  vDefinition.AddSimpleFieldDef('Hint', 'hint', 'Подсказка', Null, Null, 150, fkString, '', '', vsFullAccess, cLocalizable);
  vDefinition.AddSimpleFieldDef('Description', 'description', 'Описание', Null, Null, 250, fkString, '', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('StyleName', 'style_name', 'Стиль отображения', Null, Null, 250, fkString, '', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('DisplayFormat', 'display_format', 'Формат вывода', Null, Null, 50, fkString, '', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('StorageName', 'storage_name', 'Наименование хранилища', Null, Null, 50, fkString, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('StorageKind', 'storage_kind', 'Тип хранилища', skDatabase, Null, Null, fkEnum, '', '', vsFullAccess, 0, 'StorageKinds');
  vDefinition.AddSimpleFieldDef('UIState', 'ui_state', 'Доступность', _vsFullAccess, Null, _vsUndefined, fkEnum, '', '', vsFullAccess, 0, 'ViewStates');
  vDefinition.AddSimpleFieldDef('Flags', 'flags', 'Флаги', 0, 0, Null, fkFlag, '', '', vsFullAccess, 0, 'FieldFlags');
  // overridable
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkNotDefined, Null, Null, fkEnum, '', '', vsFullAccess, cRequired, 'FieldKinds');
  vDefinition.AddSimpleFieldDef('CommitKind', 'commit_kind', 'Событие инициации изменения', ckOnChange, Null, Null, fkEnum, '', '', vsFullAccess, 0, 'CommitKinds');
  vDefinition.AddSimpleFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', Null, Null, 250, fkString, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('SearchType', 'search_type', 'Тип поиска', stSearchFromBegin, Null, Null, fkEnum, '', '', vsFullAccess, cRequired, 'SearchTypes');
  //FNotificationChains: TNotificationChains;
  //FDependentFields: TStrings
  //CalcStorageName;
  vDefinition.RegisterReaction('FullName', 'Name;Name.Definition;Definition', TProc(procedure(const ASession: TUserSession;
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    var
      vFieldName: string;
    begin
      vFieldName := SafeDisplayName(AEntity.ExtractEntity('Definition')) + '.' + AEntity['Name'];
      AEntity._SetFieldValue(AHolder, 'FullName', vFieldName);
    end));
  vDefinition.RegisterReaction('StorageName', 'Name', TProc(procedure(const ASession: TUserSession;
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    begin
      if AEntity.IsNew then
        AEntity._SetFieldValue(AHolder, 'StorageName', BuildStorageName(AEntity['Name']));
    end));

  vDefinition := AddDefinition('StringFields', '_Fields', 'Строковые поля', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkString, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddSimpleFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', Null, Null, 250, fkString, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MaxSize', 'max_size', 'Максимальный размер', 50, -1, Null, fkInteger, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('Dictionary', 'dictionary', 'Коллекция для автоподстановки', Null, Null, 50, fkString, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('SearchType', 'search_type', 'Тип поиска', stSearchEverywhere, Null, Null, fkEnum, '', '', vsFullAccess, cRequired, 'SearchTypes');

  vDefinition := AddDefinition('IntegerFields', '_Fields', 'Целочисленные поля', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkInteger, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddSimpleFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', 0, Null, Null, fkInteger, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MinValue', 'min_value', 'Минимальное значение', Null, Null, Null, fkInteger, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MaxValue', 'max_value', 'Максимальное значение', Null, Null, Null, fkInteger, '', '', vsFullAccess, 0);

  vDefinition := AddDefinition('ColorFields', '_Fields', 'Цветовые поля', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkColor, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddSimpleFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', 0, Null, Null, fkColor, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MinValue', 'min_value', 'Минимальное значение', Null, Null, Null, fkColor, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MaxValue', 'max_value', 'Максимальное значение', Null, Null, Null, fkColor, '', '', vsFullAccess, 0);

  vDefinition := AddDefinition('EnumFields', '_Fields', 'Перечисления', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkEnum, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddEntityFieldDef('Enumeration', 'enumeration', 'Перечисление', '', 'Enumerations', 0, vsFullAccess, cRequired, estSortByID);
  vDefinition.AddEntityFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', '', 'EnumItems', 0, vsSelectOnly, 0, estSortByID).SetDependenciesAndFilter('Enumeration=Enumeration', '');
  vDefinition.AddEntityFieldDef('MinValue', 'min_value', 'Минимальное значение', '', 'EnumItems', 0, vsSelectOnly, 0, estSortByID).SetDependenciesAndFilter('Enumeration=Enumeration', '');
  vDefinition.AddEntityFieldDef('MaxValue', 'max_value', 'Максимальное значение', '', 'EnumItems', 0, vsSelectOnly, 0, estSortByID).SetDependenciesAndFilter('Enumeration=Enumeration', '');

  vDefinition := AddDefinition('StateFields', '_Fields', 'Состояния', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkEnum, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddEntityFieldDef('Enumeration', 'enumeration', 'Перечисление', '', 'Enumerations', 0, vsFullAccess, cRequired);
  vDefinition.AddEntityFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', '', 'EnumItems', 0, vsSelectOnly, 0).SetDependenciesAndFilter('Enumeration=Enumeration', '');
  vDefinition.AddEntityFieldDef('MinValue', 'min_value', 'Минимальное значение', '', 'EnumItems', 0, vsSelectOnly, 0).SetDependenciesAndFilter('Enumeration=Enumeration', '');
  vDefinition.AddEntityFieldDef('MaxValue', 'max_value', 'Максимальное значение', '', 'EnumItems', 0, vsSelectOnly, 0).SetDependenciesAndFilter('Enumeration=Enumeration', '');

  // enum и flag поля очень похожи
  vDefinition := AddDefinition('FlagFields', '_Fields', 'Мультивыбор', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkFlag, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddEntityFieldDef('Enumeration', 'enumeration', 'Перечисление', '', 'Enumerations', 0, vsFullAccess, cRequired);
  vDefinition.AddEntityFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', '', 'EnumItems', 0, vsSelectOnly, 0).SetDependenciesAndFilter('Enumeration=Enumeration', '');

  vDefinition := AddDefinition('FloatFields', '_Fields', 'Вещественные поля', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkFloat, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddSimpleFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', 0.0, Null, Null, fkFloat, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MinValue', 'min_value', 'Минимальное значение', Null, Null, Null, fkFloat, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MaxValue', 'max_value', 'Максимальное значение', Null, Null, Null, fkFloat, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MaxFloatDigits', 'max_float_digit', 'Максимальное число десятичных знаков', Null, Null, Null, fkInteger, '', '', vsFullAccess, 0);

  vDefinition := AddDefinition('CurrencyFields', '_Fields', 'Денежные поля', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkCurrency, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddSimpleFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', 0.0, Null, Null, fkCurrency, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MinValue', 'min_value', 'Минимальное значение', Null, Null, Null, fkCurrency, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MaxValue', 'max_value', 'Максимальное значение', Null, Null, Null, fkCurrency, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MaxFloatDigits', 'max_float_digit', 'Максимальное число десятичных знаков', 2, Null, Null, fkInteger, '', '', vsFullAccess, 0);

  vDefinition := AddDefinition('DateTimeFields', '_Fields', 'Поля даты/времени', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkDateTime, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddSimpleFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', Null, Null, Null, fkDateTime, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MinValue', 'min_value', 'Минимальное значение', Null, Null, Null, fkDateTime, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('MaxValue', 'max_value', 'Максимальное значение', Null, Null, Null, fkDateTime, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('DisplayFormat', 'display_format', 'Формат вывода', 'dd.mm.yyyy hh:nn:ss', Null, 50, fkString, '', '', vsFullAccess);

  vDefinition := AddDefinition('BooleanFields', '_Fields', 'Булевы поля', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkBoolean, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddSimpleFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', False, Null, Null, fkBoolean, '', '', vsFullAccess, 0);

  vDefinition := AddDefinition('ObjectFields', '_Fields', 'Поля связей', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkObject, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddEntityFieldDef('ContentDefinition', 'content_definition', 'Тип значений', '', 'Definitions', 0, vsFullAccess, cRequired);
  vDefinition.AddEntityFieldDef('MasterField', 'master_field', 'Связанное поле', '', 'ObjectFields', 0, vsSelectOnly, cRequired).SetDependenciesAndFilter('ContentDefinition=Definition', '');   //все входящие ContentDefinition{Definition
//??  vDefinition.AddEntityFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', '', '', 0, vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('SearchType', 'search_type', 'Тип поиска', stSearchEverywhere, Null, Null, fkEnum, '', '', vsFullAccess, cRequired, 'SearchTypes');
  vDefinition.AddSimpleFieldDef('SortType', 'sort_type', 'Тип сортировки', estSortByID, Null, Null, fkEnum, '', '', vsFullAccess, 0, 'SortTypes');
  vDefinition.AddSimpleFieldDef('RelationPower', 'relation_power', 'Сила связи', rpWeak, Null, Null, fkEnum, '', '', vsFullAccess, 0, 'RelationPowers');
  vDefinition.AddSimpleFieldDef('FilterCode', 'filter_code', 'Фильтр', '', Null, -1, fkString, 'code', '', vsFullAccess);

  vDefinition := AddDefinition('ListFields', '_Fields', 'Поля списков', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkList, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddEntityFieldDef('ContentDefinition', 'content_definition', 'Тип значений', '', 'Definitions', 0, vsFullAccess, cRequired);
  vDefinition.AddEntityFieldDef('MasterField', 'master_field', 'Связанное поле', '', 'ObjectFields', 0, vsSelectOnly, cRequired).SetDependenciesAndFilter('ContentDefinition=Definition', '');   //все входящие ContentDefinition{Definition
  vDefinition.AddSimpleFieldDef('SearchType', 'search_type', 'Тип поиска', stSearchEverywhere, Null, Null, fkEnum, '', '', vsFullAccess, cRequired, 'SearchTypes');
  vDefinition.AddSimpleFieldDef('SortType', 'sort_type', 'Тип сортировки', estSortByID, Null, Null, fkEnum, '', '', vsFullAccess, 0, 'SortTypes');
  vDefinition.AddSimpleFieldDef('RelationPower', 'relation_power', 'Сила связи', rpWeak, Null, Null, fkEnum, '', '', vsFullAccess, 0, 'RelationPowers');
  vDefinition.AddSimpleFieldDef('FilterCode', 'filter_code', 'Фильтр', '', Null, -1, fkString, 'code', '', vsFullAccess);

  vDefinition := AddDefinition('BlobFields', '_Fields', 'Поля данных', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkBlob, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddSimpleFieldDef('BlobFormat', 'blob_format', 'Тип данных', bffRaw, Null, Null, fkEnum, '', '', vsFullAccess, 0, 'BlobFormats');
  vDefinition.AddSimpleFieldDef('MIMEType', 'mime_type', 'MIME тип', 'text/plain', Null, 100, fkString, '', '', vsFullAccess);

  vDefinition := AddDefinition('ComplexFields', '_Fields', 'Поля нативных данных', cNullItemName, ccSystem or ccHideInMenu);
  vDefinition.AddSimpleFieldDef('FieldKind', 'field_kind', 'Тип поля', fkComplex, Null, Null, fkEnum, '', '', vsHidden, 0, 'FieldKinds');
  vDefinition.AddSimpleFieldDef('ObjectKindName', 'object_kind_name', 'Наименование типа', '', Null, -1, fkString, '', '', vsFullAccess, cRequired);

  //fkComplex

  vDefinition := AddDefinition('Indents', '', 'Отступы', 'Не заданы', 0); //clkStruct
  vDefinition.AddSimpleFieldDef('Left', 'left', 'Слева', 0, 0, Null, fkInteger, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('Top', 'top', 'Сверху', 0, 0, Null, fkInteger, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('Right', 'right', 'Справа', 0, 0, Null, fkInteger, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('Bottom', 'bottom', 'Снизу', 0, 0, Null, fkInteger, '', '', vsFullAccess, 0);
  // Всякие проверки и реакции

  vDefinition := AddDefinition('_Layouts', '', 'Разметка интерфейса', cNullItemName, 0, clkMixin);
  vDefinition.AddEntityFieldDef('Parent', 'parent', 'Родитель', '', '_Layouts', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('LayoutKind', 'layout_kind', 'Тип разметки', lkAbsolute, Null, Null, fkEnum, '', '', vsSelectOnly, 0, 'LayoutKinds');
  vDefinition.AddSimpleFieldDef('Width', 'width', 'Ширина', 0, 0, Null, fkInteger, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('Height', 'height', 'Высота', 0, 0, Null, fkInteger, '', '', vsFullAccess, 0);
  vDefinition.AddEntityFieldDef('Margin', 'margin', 'Внешний отступ', '', 'Indents', 0, vsFullAccess);
  vDefinition.AddEntityFieldDef('Padding', 'padding', 'Внутренний отступ', '', 'Indents', 0, vsFullAccess);
  vDefinition.AddListFieldDef('Children', 'Parent', 'Дочерние разметки', '_Layouts', '', '', vsSelectOnly, 0, estUserSort, '', rpStrong);

  AddDefinition('Layouts', '_Layouts', 'Разметка интерфейса', cNullItemName, 0);
end;

function InterpolateString(const ADomain: TDomain; const ATemplate: string; const ADataProviders: array of TEntity): string;
var
  vRestText: string;
  vFieldName: string;
  vPos: Integer;
  vEndText: string;

  function FindEntityValue(const AFieldName: string): string;
  var
    vCurEntity: TEntity;
  begin
    for vCurEntity in ADataProviders do
      if vCurEntity.FieldExists(AFieldName) then
        Exit(vCurEntity.FieldToString(AFieldName));
    Result := ADomain.Constant[AFieldName];
  end;
begin
  Result := '';
  vRestText := ATemplate;

  vPos := Pos('{:', vRestText);

  while vPos > 0 do
  begin
    Result := Result + Copy(vRestText, 1, vPos - 1);
    vEndText := Copy(vRestText, vPos + 2, Length(vRestText) - vPos);

    vPos := Pos('}', vEndText);
    if vPos > 0 then
    begin
      vFieldName := Copy(vEndText, 1, vPos - 1);
      vRestText := Copy(vEndText, vPos + 1, Length(vEndText) - vPos);
    end
    else begin
      vFieldName := vEndText;
      vRestText := '';
    end;

    Result := Result + FindEntityValue(vFieldName);

    vPos := Pos('{:', vRestText);
  end;

  Result := Result + vRestText;
end;

procedure AppendLines(const ALines: TStrings; const AText: string; const APreventDuplicates: Boolean = False);
var
  vCandidates: TStrings;
  i: Integer;
begin
  if Trim(AText) <> '' then
  begin
    if ALines.Count = 0 then
      ALines.Text := AText
    else if not APreventDuplicates then
      ALines.Text := ALines.Text + #13#10 + AText
    else begin
      vCandidates := TStringList.Create;
      try
        vCandidates.NameValueSeparator := ALines.NameValueSeparator;
        vCandidates.Text := AText;
        for i := 0 to vCandidates.Count - 1 do
          ALines.Values[vCandidates.Names[i]] := Trim(vCandidates.ValueFromIndex[i]);
      finally
        FreeAndNil(vCandidates);
      end;
    end;
  end;
end;

function ExtractHeader(const ALines: TStrings; const AName: string): string;
var
  vIndex: Integer;
begin
  vIndex := ALines.IndexOfName(AName);
  if vIndex >= 0 then
  begin
    Result := Trim(ALines.ValueFromIndex[vIndex]);
    ALines.Delete(vIndex);
  end
  else
    Result := '';
end;

procedure PerformWebRequest(const AName: string; const ATask: TTaskHandle; const ASession: TUserSession; const AEntity, AParams: TEntity);
var
  vDomain: TDomain;
  vRequestUrl: string;
  vRequest: THTTPTask;
  vWebService: TEntity;
  vCredentials: TEntity;
  vAuthString: string;
  vQuery: TStrings;
  vParameters: TStrings;
  vLine: string;
  i: Integer;
begin
  if not Assigned(AEntity) then
    Exit;
  vWebService := AEntity.ExtractEntity('WebService');
  if not Assigned(vWebService) then
    Exit;

  vDomain := TDomain(AEntity.Domain);
  vRequestUrl := vWebService['BaseURL'] + AEntity['URL'];
  vParameters := TStringList.Create;
  vParameters.Text := Trim(vWebService['Parameters']);
  AppendLines(vParameters, AEntity['Parameters']);
  if vParameters.Count > 0 then
  begin
    vQuery := CreateDelimitedList('', '&');
    try
      for vLine in vParameters do
        vQuery.Add(vLine);
      vRequestUrl := vRequestUrl + '?' + InterpolateString(vDomain, vQuery.DelimitedText, [AParams, AEntity, vWebService]);
    finally
      FreeAndNil(vQuery);
    end;
  end;

  vRequest := THTTPTask.Create(AName, vRequestUrl, THTTPHeaderMethod(Integer(AEntity['Method'])));
  case TAuthorizationType(Integer(vWebService['AuthorizationType'])) of
    atNone: ;
    atBasic:
      begin
        vCredentials := TListField(vWebService.FieldByName('Credentials')).FindOne(nil, '|(ValidTill=@,ValidTill>:Date)', [Now]);
        if not Assigned(vCredentials) then
        begin
          FreeAndNil(vRequest);
          Exit;
        end;
        vAuthString := InterpolateString(vDomain, vWebService['AuthTemplate'], [vCredentials]);
        vAuthString := EncodeBase64(vAuthString);
        vRequest.AddHeader('Authorization', 'Basic ' + vAuthString);
      end;
  else
    begin
      FreeAndNil(vRequest);
      Assert(False, 'This Auth method is not supported yet');
      Exit;
    end;
  end;

  vRequest.Request.UserAgent := TDomain(AEntity.Domain).Configuration.Name + ', v' + TDomain(AEntity.Domain).Configuration.Version;

  vParameters.NameValueSeparator := ':';
  vParameters.Text := Trim(vWebService['Headers']);
  AppendLines(vParameters, AEntity['Headers'], True);

  if vParameters.IndexOfName('Accept') >= 0 then
    vRequest.Request.Accept := ExtractHeader(vParameters, 'Accept');
  if vParameters.IndexOfName('Content-Type') >= 0 then
    vRequest.ContentType := ExtractHeader(vParameters, 'Content-Type');

  for i := 0 to vParameters.Count - 1 do
    vRequest.AddHeader(vParameters.Names[i], InterpolateString(vDomain, Trim(vParameters.ValueFromIndex[i]), [AParams, AEntity, vWebService]));

  if Trim(AEntity['Body']) <> '' then
    vRequest.SetPostParams(Trim(AEntity['Body']), TEncoding.UTF8)
  else
    vRequest.AddHeader('Content-Length', '0');

  FreeAndNil(vParameters);

  vRequest.Start(nil);
  try
    if vRequest.ResponseCode >= 400 then
      ASession.AtomicModification(ATask, function(const AHolder: TChangeHolder): Boolean
        begin
          Result := True;
        end)
    else if vRequest.ResponseCode = 200 then
    begin

    end
    else
      raise Exception.Create(Format('Не удалось отправить сообщение. Ответ сервера: %d - %s',
        [vRequest.ResponseCode, vRequest.ResponseText]));
  finally
    vRequest.Free;
  end;
end;

function TConfiguratorScript.DoExecuteAction(const AView: TView; const AActionName: string;
  const AContext: TObject; const AParams: TEntity; const AParentHolder: TChangeHolder): Boolean;
var
  vEntity: TEntity absolute AContext;
  vEntityList: TEntityList absolute AContext;
  vInteractor: TInteractor;
  //vSession: TUserSession;
  vDomain: TDomain;
  vProjFileName: string;
begin
  Result := inherited DoExecuteAction(AView, AActionName, AContext, AParams, AParentHolder);
  if Result then
    Exit;

  vInteractor := TInteractor(AView.Interactor);
  vDomain := TDomain(vInteractor.Domain);
  //vSession := TUserSession(vInteractor.Session);

  // Обработка общих действий (конфигурация)
  if AActionName = cUnknownName then
  begin
    // Handle it
    Exit(True);
  end
  else if not Assigned(AContext) then
    Exit(False);

  // Обработка действий над списками
  if AContext is TEntityList then
  begin
    if (AActionName = cUnknownName) and vEntityList.MainDefinition.IsDescendantOf(cUnknownName) then
    begin
      // Handle it
      Exit(True);
    end;
    Exit(False);
  end;

  // Обработка действий над сущностями
  if (AActionName = 'Execute') and vEntity.InstanceOf('WebRequests') then
  begin
    //PerformWebRequest(vEntity['Name'], vTask, vSession, vEntity, AParams);

    // Handle it
    Exit(True);
  end
  else if vEntity.InstanceOf('Configurations') then
  begin
    if AActionName = 'MakeSolution' then
    begin
      vProjFileName := GenerateModules(vDomain, vEntity['Name'], vEntity['Caption'], vEntity['Version']);

      if Assigned(vInteractor) and Assigned(vInteractor.Presenter) then
      begin
        vInteractor.ShowMessage('Генерация выполнена успешно');
        TPresenter(vInteractor.Presenter).OpenFile(vProjFileName);
      end;
      Exit(True);
    end;
  end;

  Result := False;
end;

function TConfiguratorScript.GetFullText(const AEntity: TEntity; var AHandled: Boolean): string;
var
  vCollectionName: string;
begin
  vCollectionName := AEntity.CollectionName;
  if vCollectionName = cUnknownName then
    Result := ''
  else
    Result := inherited GetFullText(AEntity, AHandled);
end;

initialization

RegisterScript('Configurator', TConfiguratorScript);

end.
