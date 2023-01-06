{---------------------------------------------------------------------------------
  X-Tend runtime

  Contributors:
    Vladimir Kustikov (kustikov@sensoft.pro)
    Sergey Arlamenkov (arlamenkov@sensoft.pro)

  You may retrieve the latest version of this file at the GitHub,
  located at https://github.com/sensoftpro/x-tend.git
 ---------------------------------------------------------------------------------
  MIT License

  Copyright © 2023 Sensoft

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

unit uDomain;

interface

uses
  Generics.Collections, uFastClasses, Classes, SyncObjs, uModule, uConsts, uConfiguration, uDefinition,
  uCollection, uEntity, uEntityList, uStorage, uSession, uChangeManager, uSettings, uLogger, uScheduler,
  uTranslator, uJSON, uUtils, uView;

type
  TNotifyProgressEvent = procedure (const AProgress: Integer; const AInfo: string) of object;
  TNotifyErrorEvent = procedure (const ACaption, AText: string) of object;

  TDomainLock = class
  private
    [Weak] FDomain: TObject;
    [Weak] FCurrentSession: TUserSession;
    FLock: TCriticalSection;
    FLockCount: Integer;
  public
    constructor Create(const ADomain: TObject);
    destructor Destroy; override;

    procedure Enter(const ASession: TUserSession);
    procedure Leave;
    function Locked: Boolean;
  end;

  // в домене должна быть возможность прямого указания на соединение
  // например, домен заказа еды м.б. связан с телеграм-ботами @feed, @eda и т.п.

  {TLayouts = class end;

  TUIBuilder_ = class
  private
    FLayouts: TLayouts;
    [Weak] FPresenter: TObject;

    function GetTranslation(const ADefinition: TDefinition; const ATranslationPart: TTranslationPart = tpCaption): string;
    function GetFieldTranslation(const AFieldDef: TFieldDef; const ATranslationPart: TTranslationPart = tpCaption): string;
    function GetImageID(const AImageID: Integer): Integer;
    procedure GetLayoutName(const AEntity: TEntity; const AParams: string; var ALayoutName: string);
  public
    constructor Create(const AInteractor: TObject);
    destructor Destroy; override;

    procedure ApplyLayout(const AArea: TUIArea; const AView: TView; const ALayoutName: string; const AParams: string);

    function Navigate(const AView: TView; const AAreaName, ALayoutName: string;
      const AOptions: string = ''; const AChangeHolder: TObject = nil; const ACaption: string = '';
      const AOnClose: TProc = nil): TDialogResult;

    procedure CreateChildAreas(const AArea: TUIArea; const AView: TView; const ALayout: TLayout; const AParams: string);
    procedure CloseCurrentArea(const AModalResult: Integer);

    property Presenter: TObject read FPresenter;
    property Layouts: TLayouts read FLayouts;
  end;  }

  TDomain = class
  private
    [Weak] FConfiguration: TConfiguration;
    FStoredVersion: TVersion;
    FUId: string;
    FIsAlive: Boolean; // Домен загружен и готов к работе

    FSettings: TSettings;
    FUserSettings: TSettings;
    FScheduler: TScheduler;
    FLogger: TLogger;
    FDomainLogger: TLogger;
    FTranslator: TTranslator;
    FModules: TDictionary<string, TBaseModule>;
    FModuleInstances: TObjectList<TBaseModule>;
    FWereErrors: Boolean;
    FSharedFolderAvailable: Boolean;

    // Модули
    FStorage: TStorage;

    FActualLogID: Integer;
    FSessions: TUserSessions;
    FDomainSession: TUserSession;
    FDomainHolder: TChangeHolder;

    FDataLock: TDomainLock;

    FOnError: TNotifyErrorEvent;
    FOnProgress: TNotifyProgressEvent;

    procedure ApplyChanges(const AHolder: TObject; const AChanges: TJSONObject);
  private
    FCollections: TStringDictionary<TCollection>;
    FRootCollection: TCollection;
    FAppName: string;
    FAppTitle: string;
    FLoadingChanges: Boolean;

    {$IFDEF DEBUG}
    procedure SyncFieldsWithStorage(const ADefinition: TDefinition; const AStorage: TStorage);
    {$ENDIF}
    function VerifyStorageStructure: Boolean;
    //procedure UpdateDomainStructures;
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    procedure Preload(const AStorage: TStorage);
    procedure Load(const AStorage: TStorage);
    function InternalLogin(const ALogin, APassword: string): TEntity;
    function GetConstant(const AName: string): Variant;
    function GetModuleByName(const AName: string; const AType: string): TBaseModule; overload;
    function GetModuleByName(const AName: string): TBaseModule; overload;
  public
    constructor Create(const APlatform: TObject; const AConfiguration: TConfiguration;
      const AUId: string; const ASettings: TSettings);
    destructor Destroy; override;

    procedure Init;
    procedure Run;
    procedure Stop;

    procedure NotifyLoadingProgress(const AProgress: Integer; const AInfo: string = '');
    procedure NotifyError(const ACaption, AText: string);

    function Login(const AName, APassword: string): TUserSession;
    //function LoginByRFID(const ARFID: string): TUserSession;
    procedure Logout(const ASession: TUserSession);
    //procedure SetRFIDHandler(const ARFIDHandler: TRFIDReadEvent);

    procedure ExecuteDefaultAction(const ASession: TUserSession; const AParameter: string);

    // storage
    function Now: TDateTime;
    procedure ReloadChanges(const AHolder: TObject);
    procedure ImportChanges(const AChanges: TJSONObject);

    // object extensions
    function TryGetModule(const AName, AType: string; out AModule: TBaseModule): Boolean;

    function SyncWithStorage(const ADefinition: TDefinition; const AStorage: TStorage): Boolean;

    function CollectionByName(const AName: string): TCollection;
    function CollectionExists(const AName: string): Boolean;
    function CollectionByStorageName(const AStorageName: string): TCollection;
    function CollectionByID(const AID: Integer): TCollection;
    procedure GetCollections(const AList: TList<TCollection>; const ADefinitionKind: TCollectionKind);

    // Handling inheritance
    procedure GetEntityList(const ASession: TObject; const ADefinition: TDefinition;
      const AEntityList: TEntityList; const AFilterName: string);
    function CollectionsByDefinition(const ADefinition: TDefinition): TList<TCollection>;

    function LoadCollection(const ACollectionName: string): TCollection;

    function CreateNewEntity(const AHolder: TObject; const ACollectionName: string;
      const AID: Integer = cNewID; const AOwnerContext: TObject = nil): TEntity;
    function EntityByID(const ACollectionName: string; const AID: Integer): TEntity;
    function FirstEntity(const ACollectionName: string): TEntity;
    function FindOneEntity(const ADefinitionName: string; const ASession: TUserSession;
      const AQuery: string; const AParams: array of Variant): TEntity;
    function FindEntities(const ADefinitionName: string; const ASession: TUserSession;
      const AQuery: string; const AParams: array of Variant): TList<TEntity>;

    function Translate(const AKey, ADefault: string): string;
    function TranslateDefinition(const ADefinition: TDefinition;
      const ATranslationPart: TTranslationPart = tpCaption): string;
    function TranslateFieldDef(const AFieldDef: TFieldDef;
      const ATranslationPart: TTranslationPart = tpCaption): string;

    procedure UpdateLogID(const ANewLogID: Integer);
    procedure CheckLocking(const AExpectedResult: Boolean = True);

    function Log(const AMessage: string; const AMessageKind: TMessageKind = mkAny): string;
    function LogEnter(const AMessage: string): string;
    function LogExit(const AMessage: string): string;

    property AppName: string read FAppName;
    property AppTitle: string read FAppTitle;
    property CollectionNameIndexed[const AName: string]: TCollection read CollectionByName; default;
    property Collections: TStringDictionary<TCollection> read FCollections;
    property RootCollection: TCollection read FRootCollection;
    property UId: string read FUId;
    property IsAlive: Boolean read FIsAlive write FIsAlive;

    property WereErrors: Boolean read FWereErrors;
    property SharedFolderAvailable: Boolean read FSharedFolderAvailable;
    property Settings: TSettings read FSettings;
    property UserSettings: TSettings read FUserSettings;
    property Logger: TLogger read FLogger;
    property Language: string read GetLanguage write SetLanguage;
    property Constant[const AName: string]: Variant read GetConstant;

    property Sessions: TUserSessions read FSessions;
    property DomainSession: TUserSession read FDomainSession;
    property DomainHolder: TChangeHolder read FDomainHolder;
    property Storage: TStorage read FStorage;
    property StoredVersion: TVersion read FStoredVersion;
    property Configuration: TConfiguration read FConfiguration;
    property Module[const AName: string]: TBaseModule read GetModuleByName;
    function NewModule(const AType, AModuleName: string): TBaseModule;
    function IsModuleLoaded(const AName: string): Boolean;
    property DataLock: TDomainLock read FDataLock;
    property LoadingChanges: Boolean read FLoadingChanges;
    property OnError: TNotifyErrorEvent read FOnError write FOnError;
    property OnProgress: TNotifyProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
  Types, SysUtils, StrUtils, IOUtils, Math, Variants, IniFiles,
  uPlatform, uQueryDef, uQuery;

type
  TCreateDefaultEntitiesProc = procedure (const ADomain: TObject) of object;
  TExecuteDefaultActionFunc = function(const ASession: TUserSession; const AParams: string): Boolean of object;
  TDomainReadyProc = procedure(const ADomain: TObject) of object;
  TDomainStoppedProc = procedure(const ADomain: TObject) of object;

{ TDomain }

procedure TDomain.ApplyChanges(const AHolder: TObject; const AChanges: TJSONObject);
var
  vInteractor: TObject;
  vPair: TJSONPair;
  vUpdates: TJSONArray;
  vUpdate: TJSONObject;
  vActions: TJSONArray;
  vAction: TJSONObject;
  vActionID: Integer;
  vFields: TJSONObject;
  i, j: Integer;
  vActionKind: TEntitySaveAction;
  vCollection: string;
  vEntityID: Integer;
  vEntity: TEntity;
  vJustCreated: TList<TEntity>;
  vLogID: Integer;
  vContext: string;
begin
  Assert(Assigned(AHolder), 'Пустой холдер в методе');
  vInteractor := TChangeHolder(AHolder).Interactor;
  vJustCreated := TList<TEntity>.Create;
  vLogID := -1;
  try
    try
      // Раскладываем обновления по своим местам
      vUpdates := TJSONArray(AChanges.Get('updates').JsonValue);
      for i := 0 to vUpdates.Size - 1 do
      begin
        vUpdate := TJSONObject(vUpdates.Get(i));
        vLogID := TJSONNumber(vUpdate.Get('log_id').JsonValue).AsInt;
        vActions := TJSONArray(vUpdate.Get('actions').JsonValue);

        if Assigned(vInteractor) then
        begin
          vContext := vUpdate.Get('log_time').JsonValue.Value + ' ' + Format(Translate('fmtUserChangedField',
            'пользователь %s изменил значение поля %%s записи %%s'), [AnsiUpperCase(vUpdate.Get('user_name').JsonValue.Value)]) +
            #13#10#13#10 + Translate('txtChangedValue', 'Пришедшее значение') + ': '#9'%s'#13#10 +
            Translate('txtActualValue', 'Текущее значение') + ': '#9'%s'#13#10#13#10 +
            Translate('txtPromptIgnoreChanges', 'Игнорировать пришедшие изменения, оставив текущую версию?');
        end
        else
          vContext := '';

        for j := 0 to vActions.Size - 1 do
        begin
          vAction := TJSONObject(vActions.Get(j));
          vActionID := TJSONNumber(vAction.Get('change_id').JsonValue).AsInt;
          vActionKind := TEntitySaveAction(TJSONNumber(vAction.Get('action_kind_id').JsonValue).AsInt);
          vCollection := vAction.Get('collection').JsonValue.Value;
          vEntityID := TJSONNumber(vAction.Get('entity_id').JsonValue).AsInt;
          vPair := vAction.Get('fields');
          if Assigned(vPair) then
            vFields := TJSONObject(vPair.JsonValue)
          else
            vFields := nil;
          // Наиболее простой случай - идентификаторы выдаются одним хранилищем
          case vActionKind of
            esaInsert: begin
                vEntity := CollectionByName(vCollection).
                  CreateEntityFromJSON(vEntityID, vFields);
                vEntity.LogID := vActionID;
                vJustCreated.Add(vEntity);
              end;
            esaUpdate: begin
                vEntity := CollectionByName(vCollection).EntityByID(vEntityID);
                // Пришедшая сущность могла уже быть удалена
                if Assigned(vEntity) then
                begin
                  // Убрать поля, которые конфликтуют, записать их в холдер
                  //   инициировать пересчеты калькулируемых полей
                  TChangeHolder(AHolder).ProcessUpdatingEntity(vInteractor, vEntity, vFields, vContext);
                  CollectionByName(vCollection).FillEntityFromJSON(AHolder, vEntity, vFields, False);
                  vEntity.LogID := vActionID;
                end;
              end;
            esaDelete: begin
                vEntity := CollectionByName(vCollection).EntityByID(vEntityID);
                if Assigned(vEntity) then
                begin
                  vJustCreated.Remove(vEntity);

                  // Убрать все ссылки на изменения в холдерах
                  TChangeHolder(AHolder).ProcessDeletingEntity(vEntity);

                  FLoadingChanges := True;
                  try
                    CollectionByName(vCollection).MarkEntityAsDeleted(AHolder, vEntity);
                  finally
                    FLoadingChanges := False;
                  end;
                  CollectionByName(vCollection).RemoveEntity(vEntity);
                end;
              end;
          else
            // Do nothing
          end;
        end;

        // Теперь добавляем сущности в списки, если это нужно
        for vEntity in vJustCreated do
        begin
          FLoadingChanges := True;
          try
            vEntity.SubscribeFields(AHolder);
            TCollection(vEntity.Collection).NotifyListeners(AHolder, dckListAdded, vEntity);
          finally
            FLoadingChanges := False;
          end;
        end;

        vJustCreated.Clear;

        FActualLogID := vLogID;
      end;
    except
      on E: Exception do
      begin
        FLogger.AddMessage('Something gone wrong: ' + E.Message + ', log id = ' + IntToStr(vLogID));
        FLogger.Flush;
      end;
    end;
  finally
    FreeAndNil(vJustCreated);
  end;
end;

procedure TDomain.CheckLocking(const AExpectedResult: Boolean);
begin
  if FDataLock.Locked <> AExpectedResult then
  begin
    FLogger.AddMessage('@@@ Ожидание в блокировке @@@');
    if AExpectedResult then
      Assert(False, 'Неожиданное состояние блокировки: ' + BoolToStr(not AExpectedResult));
  end;
end;

function TDomain.CollectionByID(const AID: Integer): TCollection;
begin
  if AID = 0 then
  // ID = 0 у коллекции SysDefinitions
    Result := FRootCollection
  else
    Result := TCollection(FRootCollection.EntityByID(AID));
end;

function TDomain.CollectionByStorageName(const AStorageName: string): TCollection;
var
  vCollection: TCollection;
begin
  for vCollection in FCollections do
    if SameText(AStorageName, vCollection.ContentDefinition.StorageName) then
      Exit(vCollection);
  Result := nil;
end;

function TDomain.CollectionExists(const AName: string): Boolean;
begin
  Result := FCollections.Exists(AName);
end;

function TDomain.CollectionsByDefinition(const ADefinition: TDefinition): TList<TCollection>;
  procedure GetCollectionsByDefinition(const ADefinition: TDefinition; const AList: TList<TCollection>);
  var
    vCollection: TCollection;
    vDescDefinition: TDefinition;
  begin
    if FCollections.TryGetObject(ADefinition.Name, vCollection) then
      AList.Add(vCollection);

    for vDescDefinition in ADefinition.Descendants do
      GetCollectionsByDefinition(vDescDefinition, AList);
  end;
begin
  Result := TList<TCollection>.Create;
  GetCollectionsByDefinition(ADefinition, Result);
end;

constructor TDomain.Create(const APlatform: TObject; const AConfiguration: TConfiguration;
  const AUId: string; const ASettings: TSettings);
var
  vAppDataDirectory: string;
  vLogsDir: string;
begin
  inherited Create;

  FConfiguration := AConfiguration;
  FUId := AUId;
  FIsAlive := False;
  FDataLock := TDomainLock.Create(Self);
  FLoadingChanges := False;
  FWereErrors := True;
  FSharedFolderAvailable := False;
  FStoredVersion := TVersion.Create('');

  // Использовать ASettings для нахождения путей к рабочим папкам
  FSettings := TIniSettings.Create(TPath.Combine(FConfiguration.ConfigurationDir, 'settings.ini'));

  FTranslator := TTranslator.Create(AConfiguration.Localizator, TPlatform(APlatform).Language);
  FModules := TDictionary<string, TBaseModule>.Create;
  FModuleInstances := TObjectList<TBaseModule>.Create;

  NotifyLoadingProgress(0, 'Создание домена');

  vAppDataDirectory := FConfiguration.CacheDir;
  FUserSettings := TIniSettings.Create(TPath.Combine(vAppDataDirectory, 'settings.ini'));
  vLogsDir := TPath.Combine(vAppDataDirectory, 'logs');
  FLogger := TLogger.Create(Self, 'System logger');
  if not TDirectory.Exists(vLogsDir) then
    ForceDirectories(vLogsDir);
  FLogger.SetTarget(TPath.Combine(vLogsDir, 'log_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.txt'));
  FDomainLogger := TLogger.Create(Self, 'Domain logger');
  FDomainLogger.SetTarget(TPath.Combine(vAppDataDirectory, 'domain_log.txt'));
  FScheduler := TScheduler.Create(Self, 60);

  FAppName := Translate('AppTitle', FConfiguration._Caption);
  FAppTitle := FAppName + ' - ' + FConfiguration.Version.ToString;

  FSessions := TUserSessions.Create(Self);
  FDomainSession := FSessions.AddSession(nil);
  FDomainHolder := FDomainSession.NullHolder;

  FCollections := TStringDictionary<TCollection>.Create;
  FRootCollection := TCollection.Create(Self, FConfiguration.RootDefinition);
  FRootCollection.SetCollectionAttributes(TCollection, FConfiguration.RootDefinition);
end;

function TDomain.CreateNewEntity(const AHolder: TObject; const ACollectionName: string; const AID: Integer;
  const AOwnerContext: TObject): TEntity;
var
  vCollection: TCollection;
begin
  vCollection := CollectionByName(ACollectionName);
  if Assigned(vCollection) then
    Result := vCollection._CreateNewEntity(AHolder, AID, '', [], AOwnerContext, True)
  else
    Result := nil;
end;

destructor TDomain.Destroy;
var
  vCollection: TCollection;
  i: Integer;
begin
  FScheduler.CancelAll;
  FreeAndNil(FScheduler);

  // Модули должны очищаться в обратном порядке
  FreeAndNil(FModules);
  for i := FModuleInstances.Count - 1 downto 0 do
    FModuleInstances.Delete(i);
  FreeAndNil(FModuleInstances);

  // Удаляем всех слушателей, чтобы уведомления не приходили
  for vCollection in FCollections do
    vCollection.Unsubscribe;
  // При удалении корневой коллекции она почистит все подчиненные коллекции
  FreeAndNil(FRootCollection);
  FreeAndNil(FCollections);

  FreeAndNil(FLogger);
  FreeAndNil(FDomainLogger);
  FreeAndNil(FSettings);
  FreeAndNil(FUserSettings);
  FreeAndNil(FTranslator);

  FreeAndNil(FSessions);

  FreeAndNil(FDataLock);

  FConfiguration := nil;

  inherited Destroy;
end;

function TDomain.EntityByID(const ACollectionName: string; const AID: Integer): TEntity;
var
  vCollection: TCollection;
begin
  vCollection := CollectionByName(ACollectionName);
  if Assigned(vCollection) then
    Result := vCollection.EntityByID(AID)
  else
    Result := nil;
end;

procedure TDomain.ExecuteDefaultAction(const ASession: TUserSession; const AParameter: string);
var
  i: Integer;
  vSession: TUserSession;
begin
  if Assigned(ASession) then
    vSession := ASession
  else begin
    vSession := FDomainSession;
    for i := 0 to FSessions.Count - 1 do
    begin
      vSession := FSessions[i];
      if vSession <> FDomainSession then
        Break;
    end;
  end;

  TExecuteDefaultActionFunc(FConfiguration.ExecuteDefaultActionFunc)(vSession, AParameter);
end;

function TDomain.FindEntities(const ADefinitionName: string; const ASession: TUserSession; const AQuery: string;
  const AParams: array of Variant): TList<TEntity>;
  procedure InternalFindEntities(const ADefinitionName: string; const ASession: TUserSession; const AQuery: string;
    const AParams: array of Variant; const AList: TList<TEntity>);
  var
    vCollection: TCollection;
    vDescendant: TDefinition;
    vDefinition: TDefinition;
  begin
    vDefinition := FConfiguration[ADefinitionName];
    if not Assigned(vDefinition) then
      Exit;

    vCollection := CollectionByName(vDefinition.Name);
    if Assigned(vCollection) then
      vCollection.FindInto(ASession, AQuery, AParams, Result);

    if vDefinition.Descendants.Count > 0 then
      for vDescendant in vDefinition.Descendants do
        InternalFindEntities(vDescendant.Name, ASession, AQuery, AParams, AList);
  end;
begin
  Result := TList<TEntity>.Create;
  InternalFindEntities(ADefinitionName, ASession, AQuery, AParams, Result);
end;

function TDomain.FindOneEntity(const ADefinitionName: string; const ASession: TUserSession; const AQuery: string;
  const AParams: array of Variant): TEntity;
var
  vCollection: TCollection;
  vDescendant: TDefinition;
  vDefinition: TDefinition;
begin
  vDefinition := FConfiguration[ADefinitionName];
  if not Assigned(vDefinition) then
    Exit(nil);

  vCollection := CollectionByName(vDefinition.Name);
  if Assigned(vCollection) then
  begin
    Result := vCollection.FindOne(ASession, AQuery, AParams);
    if Assigned(Result) then
      Exit;
  end;

  if vDefinition.Descendants.Count > 0 then
    for vDescendant in vDefinition.Descendants do
    begin
      Result := FindOneEntity(vDescendant.Name, ASession, AQuery, AParams);
      if Assigned(Result) then
        Exit;
    end;

  Result := nil;
end;

function TDomain.FirstEntity(const ACollectionName: string): TEntity;
var
  vCollection: TCollection;
begin
  vCollection := CollectionByName(ACollectionName);
  if Assigned(vCollection) then
    Result := vCollection.First
  else
    Result := nil;
end;

function TDomain.CollectionByName(const AName: string): TCollection;
begin
  Result := FCollections.ObjectByName(AName);
end;

procedure TDomain.GetCollections(const AList: TList<TCollection>; const ADefinitionKind: TCollectionKind);
var
  vCollection: TCollection;
begin
  AList.Clear;
  for vCollection in FCollections do
    if vCollection.ContentDefinition.Kind = ADefinitionKind then
      AList.Add(vCollection);
end;

function TDomain.GetConstant(const AName: string): Variant;
var
  vConsts: TEntity;
begin
  vConsts := CollectionByName('SysConstants').First;
  if Assigned(vConsts) and vConsts.FieldExists(AName) then
    Result := vConsts[AName]
  else
    Result := '';
end;

procedure TDomain.GetEntityList(const ASession: TObject; const ADefinition: TDefinition;
  const AEntityList: TEntityList; const AFilterName: string);
var
  vQueryDef: TQueryDef;
  vSession: TUserSession;
  vFilter: TFilter;

  procedure ApplyFilter(const ADefinition: TDefinition; const AFilterName: string);
  var
    vFilter: TFilter;
    vQueryObject: TQueryExecutor;
    i: Integer;
  begin
    if not ADefinition.Filters.TryGetValue(AFilterName, vFilter) then
      Exit;

    vQueryObject := TQueryExecutor.Create(vFilter.QueryDef);
    try
      vQueryObject.SetParameters(vSession, nil);
      vQueryObject.Select(vSession);
      for i := 0 to vQueryObject.Results.Count - 1 do
        AEntityList.Add(vQueryObject.Results[i]);
    finally
      FreeAndNil(vQueryObject);
    end;
  end;

  procedure InternalGetEntityList(const ADefinition: TDefinition; const AFilterName: string);
  var
    vCollection: TCollection;
    vDescDefinition: TDefinition;
    i: Integer;
  begin
    if not Assigned(ADefinition) then
      Exit;

    vCollection := CollectionByName(ADefinition.Name);
    if Assigned(vCollection) then
    begin
      // Добавить значения
      if ADefinition.Filters.ContainsKey(AFilterName) then
        ApplyFilter(ADefinition, AFilterName)
      else
        for i := 0 to vCollection.Count - 1 do
          AEntityList.Add(vCollection[i]);
    end;

    for vDescDefinition in ADefinition.Descendants do
      InternalGetEntityList(vDescDefinition, AFilterName);
  end;
begin
  AEntityList.Clear;
  if not Assigned(ADefinition) then
    Exit;

  if Assigned(ASession) then
    vSession := TUserSession(ASession)
  else
    vSession := FDomainSession;

  InternalGetEntityList(ADefinition, AFilterName);

  if ADefinition.Filters.TryGetValue(AFilterName, vFilter) then
    vQueryDef := vFilter.QueryDef
  else
    vQueryDef := nil;

  AEntityList.SetFiller(ADefinition, False, vQueryDef);
end;

function TDomain.GetLanguage: string;
begin
  Result := FTranslator.Language;
end;

function TDomain.GetModuleByName(const AName: string; const AType: string): TBaseModule;
var
  vModuleClass: TModuleClass;
  vModuleName: string;
begin
  if FModules.TryGetValue(AName, Result) then
    Exit;

  vModuleClass := _Platform.ResolveModuleClass(FSettings, AName, AType, vModuleName);
  if Assigned(vModuleClass) then
  begin
    Result := TDomainModuleClass(vModuleClass).Create(Self, vModuleName);
    FModules.AddOrSetValue(AName, Result);
    FModuleInstances.Add(Result);
  end
  else
    Result := nil;
end;

function TDomain.GetModuleByName(const AName: string): TBaseModule;
begin
  if not FModules.TryGetValue(AName, Result) then
    Result := GetModuleByName(AName, AName);
end;

procedure TDomain.ImportChanges(const AChanges: TJSONObject);
var
  vUpdates: TJSONArray;
  vUpdate: TJSONObject;
  vActions: TJSONArray;
  i: Integer;
  vJustCreated: TList<TEntity>;
  vLogID: Integer;
begin
  vJustCreated := TList<TEntity>.Create;
  try
    try
      // Раскладываем обновления по своим местам
      vUpdates := TJSONArray(AChanges.Get('updates').JsonValue);
      for i := 0 to vUpdates.Size - 1 do
      begin
        vUpdate := TJSONObject(vUpdates.Get(i));
        vLogID := TJSONNumber(vUpdate.Get('log_id').JsonValue).AsInt;
        vActions := TJSONArray(vUpdate.Get('actions').JsonValue);

        FDomainSession.AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
          var
            vPair: TJSONPair;
            j: Integer;
            vAction: TJSONObject;
            vActionID: Integer;
            vActionKind: TEntitySaveAction;
            vFields: TJSONObject;
            vCollectionName: string;
            vCollection: TCollection;
            vEntityID: Integer;
            vEntity: TEntity;
          begin
            for j := 0 to vActions.Size - 1 do
            begin
              vAction := TJSONObject(vActions.Get(j));
              vActionID := TJSONNumber(vAction.Get('change_id').JsonValue).AsInt;
              vActionKind := TEntitySaveAction(TJSONNumber(vAction.Get('action_kind_id').JsonValue).AsInt);
              vCollectionName := vAction.Get('collection').JsonValue.Value;
              vCollection := CollectionByName(vCollectionName);
              if not Assigned(vCollection) then
                Continue;
              vEntityID := TJSONNumber(vAction.Get('entity_id').JsonValue).AsInt;
              vPair := vAction.Get('fields');
              if Assigned(vPair) then
                vFields := TJSONObject(vPair.JsonValue)
              else
                vFields := nil;
              // Наиболее простой случай - идентификаторы выдаются одним хранилищем
              case vActionKind of
                esaInsert: begin
                    vEntity := vCollection.CreateEntityFromJSON(vEntityID, vFields);
                    vEntity.IsNew := True;
                    vEntity.LogID := vActionID;
                    AHolder.RegisterEntityCreating(vEntity);
                    vJustCreated.Add(vEntity);
                  end;
                esaUpdate: begin
                    vEntity := vCollection.EntityByID(vEntityID);
                    // Пришедшая сущность могла уже быть удалена
                    if Assigned(vEntity) then
                    begin
                      vCollection.FillEntityFromJSON(AHolder, vEntity, vFields, False);
                      vEntity.LogID := vActionID;
                    end;
                  end;
                esaDelete: begin
                    vEntity := vCollection.EntityByID(vEntityID);
                    if Assigned(vEntity) then
                    begin
                      vJustCreated.Remove(vEntity);

                      FLoadingChanges := True;
                      try
                        vCollection.MarkEntityAsDeleted(AHolder, vEntity);
                      finally
                        FLoadingChanges := False;
                      end;
                      vCollection.RemoveEntity(vEntity);
                    end;
                  end;
              else
                // Do nothing
              end;
            end;

            // Теперь добавляем сущности в списки, если это нужно
            for vEntity in vJustCreated do
            begin
              FLoadingChanges := True;
              try
                vEntity.SubscribeFields(AHolder);
                TCollection(vEntity.Collection).NotifyListeners(AHolder, dckListAdded, vEntity);
              finally
                FLoadingChanges := False;
              end;
            end;

            vJustCreated.Clear;

            FActualLogID := vLogID;

            Result := True;
          end, nil, True);
      end;
    except
    end;
  finally
    FreeAndNil(vJustCreated);
  end;
end;

procedure TDomain.Init;
begin
  NotifyLoadingProgress(5, 'Соединение с базой данных');

  FStorage := TStorage(GetModuleByName('DataStorage', 'Storage'));
  Assert(Assigned(FStorage), 'Для конфигурации не задано основное хранилище');
  try
    FStorage.Connect;
  except
    on E: Exception do
    begin
      NotifyError('Ошибка подключения', 'Ошибка подключения к хранилищу данных. ' + E.Message);
      Exit;
      //raise Exception.Create('Ошибка подключения');
    end;
  end;

  NotifyLoadingProgress(15, 'Обновление структур данных');

  FStoredVersion := FStorage.Version;
  if not FStoredVersion.IsValid then
    FStoredVersion := FConfiguration.Version;

  // Хранилище обновлено извне
  if (FConfiguration.Version < FStoredVersion) then
  begin
    NotifyError('Ошибка запуска', 'Используется устаревшая версия программы [' + FConfiguration.Version.ToString + '].'#13#10 +
      'Необходимо установить новую версию [' + FStoredVersion.ToString + '] самостоятельно или обратиться за помощью к администратору');
    Exit;
    //raise Exception.Create('Устаревшая версия программы');
  end
  // Новая версия конфигурации, требуется обновление структуры
  else if (FConfiguration.Version > FStoredVersion) or (FStorage.Version = '') or (_Platform.DeploymentType = 'dev') then
  begin
    if VerifyStorageStructure then
      FStorage.Version := FConfiguration.Version.ToString;
    //AConfiguration.Localizator.EnumerateConfigurationElements(Self);
  end;

  FWereErrors := False;
end;

function TDomain.InternalLogin(const ALogin, APassword: string): TEntity;
var
  i, j: Integer;
  vCollections: TList<TCollection>;
  vCollection: TCollection;
begin
  vCollections := CollectionsByDefinition(FConfiguration['SysUsers']);
  try
    for i := 0 to vCollections.Count - 1 do
    begin
      vCollection := TCollection(vCollections[i]);
      for j := 0 to vCollection.Count - 1 do
      begin
        Result := vCollection[j];
        if SameText(Result['Login'], ALogin) and SameText(Result['Password'], MD5Hash(APassword)) then
          Exit;
      end;
    end;
    Result := nil;
  finally
    FreeAndNil(vCollections);
  end;
end;

function TDomain.IsModuleLoaded(const AName: string): Boolean;
begin
  Result := FModules.ContainsKey(AName);
end;

procedure TDomain.Load(const AStorage: TStorage);
var
  i: Integer;
  vCollection: TCollection;
  vSharedFolder: string;
begin
  CollectionByName('SysConstants').LoadAll(FStorage);

  vSharedFolder := Trim(GetConstant('SharedFolder'));
  FSharedFolderAvailable := (vSharedFolder <> '') and TDirectory.Exists(vSharedFolder);

  for i := 0 to FCollections.Count - 1 do
  begin
    vCollection := FCollections[i];
    if vCollection.ContentDefinition.Name = 'SysConstants' then
      Continue;
    NotifyLoadingProgress(35 + Round((85 - 35) * i / FCollections.Count),
      'Загрузка коллекции "' + vCollection.ContentDefinition._Caption + '"');
    if not vCollection.ContentDefinition.HasFlag(ccNotSave or ccLazyLoad) then
      vCollection.LoadAll(FStorage);
  end;

  NotifyLoadingProgress(85, 'Заполнение системных справочников данными');
  TCreateDefaultEntitiesProc(FConfiguration.CreateDefaultEntitiesProc)(Self);

  NotifyLoadingProgress(90, 'Создание связей внутри модели');

  // Создаем все связи в модели
  for vCollection in FCollections do
    if not vCollection.ContentDefinition.HasFlag(ccLazyLoad) then
    begin
      FStorage.UpdateNumerator(vCollection.ContentDefinition.StorageName, vCollection.MaxID);
      vCollection.Subscribe;
    end;

  FScheduler.ScheduleAll;
end;

function TDomain.LoadCollection(const ACollectionName: string): TCollection;
begin
  Result := CollectionByName(ACollectionName);
  if Assigned(Result) and not Result.Loaded then
  begin
    Result.LoadAll(FStorage);
    Result.Subscribe;
  end;
end;

function TDomain.Log(const AMessage: string; const AMessageKind: TMessageKind): string;
begin
  Result := FDomainLogger.AddMessage(AMessage, AMessageKind);
end;

function TDomain.LogEnter(const AMessage: string): string;
begin
  Result := FDomainLogger.AddEnterMessage(AMessage);
end;

function TDomain.LogExit(const AMessage: string): string;
begin
  Result := FDomainLogger.AddExitMessage(AMessage);
end;

function TDomain.Login(const AName, APassword: string): TUserSession;
var
  vUser: TEntity;
begin
  try
    vUser := InternalLogin(AName, APassword);
    if Assigned(vUser) then
      Result := FSessions.AddSession(vUser)
    else
      Result := nil;
  except
    on E: Exception do
    begin
      Result := nil;
      FLogger.AddMessage(E.Message);
      FLogger.Flush;
    end;
  end;
end;

{function TDomain.LoginByRFID(const ARFID: string): TUserSession;
var
  i, j: Integer;
  vCollections: TList<TCollection>;
  vCollection: TCollection;
  vUser: TEntity;
begin
  vCollections := CollectionsByDefinition(FConfiguration['SysUsers']);
  try
    for i := 0 to vCollections.Count - 1 do
    begin
      vCollection := TCollection(vCollections[i]);
      for j := 0 to vCollection.Count - 1 do
      begin
        vUser := vCollection[j];
        if SameText(vUser['RFID'], ARFID) then
        begin
          Result := FSessions.AddSession(vUser);
          Exit;
        end;
      end;
    end;
    Result := nil;
  finally
    FreeAndNil(vCollections);
  end;
end; }

procedure TDomain.Logout(const ASession: TUserSession);
begin
  FSessions.RemoveSession(ASession);
end;

function TDomain.NewModule(const AType, AModuleName: string): TBaseModule;
var
  vModuleClass: TModuleClass;
begin
  vModuleClass := TBaseModule.GetModuleClass(AType, AModuleName);
  if not Assigned(vModuleClass) then
  begin
    NotifyError('Ошибка загрузки модуля',
      Format('Модуль [%s] типа [%s] не зарегистрирован в системе', [AModuleName, AType]) + #13#10#13#10
      + 'Проверьте, что файл с модулем подключен к проекту и регистрация класса модуля в нём выполнена правильно:'
      + #13#10#13#10'initialization'#13#10'  TBaseModule.RegisterModule(''' + AType + ''', '''
      + AModuleName + ''', {Класс модуля});');
    Result := nil;
  end
  else
    Result := TDomainModuleClass(vModuleClass).Create(Self, AModuleName);
end;

procedure TDomain.NotifyError(const ACaption, AText: string);
begin
  FLogger.AddMessage(AText, mkError);
  if Assigned(FOnError) then
    FOnError(ACaption, AText);
end;

procedure TDomain.NotifyLoadingProgress(const AProgress: Integer; const AInfo: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(AProgress, AInfo);
end;

function TDomain.Now: TDateTime;
begin
  if FStorage = nil then
    Result := SysUtils.Now
  else
    Result := FStorage.GetTime;
end;

procedure TDomain.Preload(const AStorage: TStorage);
var
  vEntity: TEntity;
begin
  NotifyLoadingProgress(25, 'Создание системных структур');

  // Приоритетным источником загрузки является хранилище
  FRootCollection.LoadAll(FStorage, False);

{  FDomainSession.AtomicModification(function(const AHolder: TChangeHolder): Boolean
    var
      vCollection: TCollection;
      vCollectionName: string;
      vContentDefinition: TCfgDefinition;
      i: Integer;
    begin
      for i := 0 to FRootCollection.Count - 1 do
      begin
        vCollection := TCollection(FRootCollection[i]);
        vCollectionName := vCollection.FieldValues['Name'];
        vContentDefinition := FConfiguration.DefinitionByName(vCollectionName);
        if not Assigned(vContentDefinition) then
          FRootCollection.MarkEntityAsDeleted(FDomainSession, vCollection)
        else
          vCollection.SetCollectionAttributes(TEntity, vContentDefinition);
      end;

      Result := True;
    end, chkMaster, True); }

  FDomainSession.AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
    var
      vSyncDefinition: TDefinition;
      vDefEntity: TCollection;
      vHolder: TChangeHolder;
    begin
      for vSyncDefinition in FConfiguration.Definitions do
      begin
        if (vSyncDefinition.Kind = clkMixin) or (vSyncDefinition.Name = 'Numerators')
          //or (vSyncDefinition.Name = '~')
        then
          Continue;

        if vSyncDefinition = FConfiguration.RootDefinition then
        begin
          vDefEntity := FRootCollection;
          vHolder := FDomainHolder;
        end
        else begin
          // Search by Name
          vDefEntity := TCollection(FRootCollection.FindOne(FDomainSession, 'Name=:Name', [vSyncDefinition.Name]));
          if not Assigned(vDefEntity) then
          begin
            if vSyncDefinition.ID > 0 then
              vDefEntity := TCollection(FRootCollection.EntityByID(vSyncDefinition.ID));
            if not Assigned(vDefEntity) then
              vDefEntity := TCollection(FRootCollection._CreateNewEntity(AHolder, cNewID, '', []));
          end;
          vDefEntity.SetCollectionAttributes(TEntity, vSyncDefinition);
          vHolder := AHolder;
        end;

        FCollections.AddObject(vSyncDefinition.Name, vDefEntity);
        vDefEntity._SetFieldValue(vHolder, 'Name', vSyncDefinition.Name);
        vDefEntity._SetFieldValue(vHolder, 'Caption', TranslateDefinition(vSyncDefinition));
        vDefEntity._SetFieldValue(vHolder, 'StorageName', vSyncDefinition.StorageName);
        vDefEntity._SetFieldValue(vHolder, 'Kind', Integer(vSyncDefinition.Kind));
      end;
      Result := True;
    end, nil, True);

  for vEntity in FRootCollection do
    if Assigned(TCollection(vEntity).ContentDefinition) then
      if TCollection(vEntity).ContentDefinition.ID = 0 then
        TCollection(vEntity).ContentDefinition.SetID(vEntity.ID);

  FStorage.UpdateNumerator(FConfiguration.RootDefinition.StorageName, FRootCollection.MaxID);
end;

procedure TDomain.ReloadChanges(const AHolder: TObject);
var
  vJSONObject: TJSONObject;
begin
  vJSONObject := FStorage.GetChanges(FActualLogID);
  if not Assigned(vJSONObject) then
    Exit;

  try
    ApplyChanges(AHolder, vJSONObject);
  finally
    vJSONObject.Free;
  end;
end;

procedure TDomain.Run;
var
  vCollection: TCollection;
  vDefinition: TDefinition;
  vEntity: TEntity;
  vFieldDef: TFieldDef;
begin
  Preload(FStorage);
  Load(FStorage);
  FActualLogID := FStorage.GetLastLogID;

  NotifyLoadingProgress(95, 'Локализация интерфейса');

  // Проверка локализаций
  FConfiguration.Localizator.EnumerateConfigurationElements(FConfiguration);
  for vCollection in FCollections do
  begin
    vDefinition := vCollection.ContentDefinition;
    for vEntity in vCollection do
    begin
      if not vEntity.IsService and not vDefinition.HasFlag(ccSystem) then
        Continue;

      for vFieldDef in vDefinition.Fields do
      begin
        if vFieldDef.HasFlag(cLocalizable) then
          FConfiguration.Localizator.AddTranslation(vDefinition.Name + '#' + IntToStr(vEntity.ID) +
            '.' + vFieldDef.Name, vEntity._FieldText(vFieldDef.Name));
      end;
    end;
  end;

  FIsAlive := True;

  TDomainReadyProc(FConfiguration.DomainReadyProc)(Self);
end;

procedure TDomain.SetLanguage(const Value: string);
begin
  FTranslator.Language := Value;
end;

{procedure TDomain.SetRFIDHandler(const ARFIDHandler: TRFIDReadEvent);
var
  vExtension: TBaseExtension;
begin
  vExtension := ResolveExtension('ProbePrepares');
  if Assigned(vExtension) then
    vExtension.SetRFIDHandler(ARFIDHandler);
end;}

procedure TDomain.Stop;
begin
  FIsAlive := False;
  TDomainStoppedProc(FConfiguration.DomainStoppedProc)(Self);
end;

{$IFDEF DEBUG}
procedure TDomain.SyncFieldsWithStorage(const ADefinition: TDefinition; const AStorage: TStorage);
var
  vField: TFieldDef;

  procedure InternalSyncronize(const AFieldDef: TFieldDef);
  var
    vEntityFieldDef: TEntityFieldDef;
    vSize: Integer;
  begin
    if AFieldDef.HasFlag(cNotSave) then
      Exit;

    if AFieldDef.Kind = fkString then
    begin
      if TSimpleFieldDef(AFieldDef).Size > 0 then
        vSize := TSimpleFieldDef(AFieldDef).Size
      else if not VarIsNull(TSimpleFieldDef(AFieldDef).MaxValue) then
        vSize := TSimpleFieldDef(AFieldDef).MaxValue
      else
        vSize := 50;
    end
    else
      vSize := 0;

    if (AFieldDef.Kind = fkObject) then
    begin
      vEntityFieldDef := TEntityFieldDef(AFieldDef);
      AStorage.SyncItemDef(AFieldDef.StorageName, fkObject, 0);
      if TEntityFieldDef(AFieldDef).IsSelector then
        AStorage.SyncItemDef(vEntityFieldDef.SelectorStorageName, fkInteger, 0);
    end
    else if AFieldDef.Kind = fkComplex then
      AStorage.SyncItemDef(AFieldDef.StorageName, fkString, -1)
    else
      AStorage.SyncItemDef(AFieldDef.StorageName, AFieldDef.Kind, vSize);
  end;

begin
  for vField in ADefinition.Fields do
    InternalSyncronize(vField);
  for vField in ADefinition.ServiceFields.Objects do
    InternalSyncronize(vField);
end;
{$ENDIF}

function TDomain.SyncWithStorage(const ADefinition: TDefinition; const AStorage: TStorage): Boolean;
begin
{$IFDEF DEBUG}
  if (ADefinition.Kind = clkMixin) or ADefinition.HasFlag(ccNotSave) then
    Result := True
  else
    Result := AStorage.SyncGroupDef(ADefinition.StorageName, procedure(const AStorage: TStorage)
      begin
        SyncFieldsWithStorage(ADefinition, AStorage);
      end);
{$ELSE}
  Result := True;
{$ENDIF}
end;

function TDomain.Translate(const AKey, ADefault: string): string;
begin
  Result := FTranslator.Translate(AKey, ADefault);
end;

function TDomain.TranslateDefinition(const ADefinition: TDefinition;
  const ATranslationPart: TTranslationPart = tpCaption): string;
var
  vDefaultValue: string;
begin
  case ATranslationPart of
    tpCaption: vDefaultValue := ADefinition._Caption;
    tpEmptyValue: vDefaultValue := ADefinition._EmptyValue;
    tpPrefix: vDefaultValue := ADefinition.Prefix;
  else
    vDefaultValue := '';
  end;

  Result := FTranslator.Translate(ADefinition.FullName + '@' + cTranslationPartCaptions[ATranslationPart], vDefaultValue);
end;

function TDomain.TranslateFieldDef(const AFieldDef: TFieldDef;
  const ATranslationPart: TTranslationPart = tpCaption): string;
var
  vDefaultValue: string;
begin
  case ATranslationPart of
    tpCaption: vDefaultValue := AFieldDef._Caption;
    tpHint: vDefaultValue := AFieldDef._Hint;
  else
    vDefaultValue := '';
  end;

  Result := FTranslator.Translate(AFieldDef.FullName + '@' + cTranslationPartCaptions[ATranslationPart], vDefaultValue);
end;

function TDomain.TryGetModule(const AName, AType: string; out AModule: TBaseModule): Boolean;
begin
  Result := FModules.TryGetValue(AName + ':' + AType, AModule);
end;

function TDomain.VerifyStorageStructure: Boolean;
var
  vDefinition: TDefinition;
  vNeedRestart: Boolean;
  vSyncronized: Boolean;
  vAttempts: Integer;
begin
  Result := True;
  try
    vNeedRestart := False;
    for vDefinition in FConfiguration.Definitions do
    begin
      // Синхронизация коллекций с текущим состоянием базы данных
      vAttempts := 0;
      vSyncronized := False;
      while not vSyncronized and (vAttempts <= 1) do
      begin
        try
          if not SyncWithStorage(vDefinition, FStorage) then
          begin
            FLogger.AddMessage('Creating storage structures for collection ' + vDefinition.Name);
            vNeedRestart := True;
          end;
          vSyncronized := True;
        except
          on E: Exception do
          begin
            vAttempts := vAttempts + 1;
            FLogger.AddMessage('Error: ' + E.Message + '. Trying to fix, attempt #' + IntToStr(vAttempts));
            vNeedRestart := False;
            FStorage.Rebuild;
          end;
        end;
      end;
    end;

    if vNeedRestart then
      FStorage.Rebuild;
  except
    on E: Exception do
    begin
      FLogger.AddMessage('Critical error while synchronizing: ' + E.Message);
      Result := False;
    end;
  end;

  if not Result then
  begin
    FLogger.AddMessage('Error occurs while synchronization has been processed');
    FLogger.Flush;
    Abort;
  end;
end;

{procedure TDomain.UpdateDomainStructures;
var
  vCollections: TList<TCollection>;
  vDefinition: TDefinition;
  vCollection: TCollection;
  vEntity: TEntity;
begin
  vCollections := TList<TCollection>.Create(FCollections);
  try
    for vDefinition in FConfiguration.Definitions do
    begin
      vCollection := FCollections.ObjectByName(vDefinition.Name);
      if not Assigned(vCollection) then
      begin
        Assert(False, 'Redo this!');
        //vCollection := AddCollection(vDefinition);
        //FStorage.UpdateNumerator(vDefinition.StorageName, vCollection.MaxID);
      end
      else begin
        // Обновляем поля всех загруженных записей
        for vEntity in vCollection do
          vEntity.UpdateFields;
        vCollections.Remove(vCollection);
      end;
    end;

    for vCollection in vCollections do
      FCollections.RemoveObject(vCollection.Name);
  finally
    FreeAndNil(vCollections);
  end;
end;}

procedure TDomain.UpdateLogID(const ANewLogID: Integer);
begin
  if ANewLogID > FActualLogID then
    FActualLogID := ANewLogID;
end;

{ TDomainLock }

constructor TDomainLock.Create(const ADomain: TObject);
begin
  inherited Create;
  FDomain := ADomain;
  FLock := TCriticalSection.Create;
  FCurrentSession := nil;
  FLockCount := 0;
end;

destructor TDomainLock.Destroy;
begin
  FDomain := nil;
  FCurrentSession := nil;
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TDomainLock.Enter(const ASession: TUserSession);
begin
  repeat until FLock.TryEnter;
  FLockCount := FLockCount + 1;
  FCurrentSession := ASession;
end;

procedure TDomainLock.Leave;
begin
  FCurrentSession := nil;
  FLockCount := FLockCount - 1;
  FLock.Leave;
end;

function TDomainLock.Locked: Boolean;
begin
  Result := FLockCount > 0;
end;

end.
