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

unit uChangeManager;

interface

uses
  Classes, Generics.Collections, uStorage, uEntity, uComplexObject, uReaction, uJSON, uConsts;

type
  PCalcWaiter = ^TCalcWaiter;
  TCalcWaiter = record
    FieldChain: string;
    Instance: TEntity;
    Parameter: TEntity;
    Handlers: THandlers;
  end;

  TChangeHolder = class;

  TChangedEntity = class
  private
    [Weak] FDomain: TObject;
    [Weak] FDefinition: TObject;
    [Weak] FEntity: TEntity;
    [Weak] FHolder: TChangeHolder;
    FIsNew: Boolean;
    FDeleted: Boolean;
    FChangedFields: TObjectDictionary<string, TJSONValue>;
    procedure FillFrom(const ASource: TChangedEntity);
    procedure TransferAllFields(const AStorage: TStorage);
    procedure TransferChangedFields(const AStorage: TStorage);
    function GetDefinitionName: string;
    function GetFieldCount: Integer;
  public
    constructor Create(const AHolder: TChangeHolder; const AEntity: TEntity; const AIsNew, ADeleted: Boolean);
    destructor Destroy; override;

    function KeepOldData(const AFieldName: string): Boolean;
    // Добавляет запись об изменении поля
    //   True - добавление успешно
    //   False - изменения инициировали переход к начальному состоянию,
    //           т.е. поле стало неизмененным
    function RegisterFieldChanges(const AFieldName: string): Boolean;

    procedure RevertField(const AFieldName: string);
    function OldFieldValue(const AFieldName: string): string;

    procedure Revert;
    function Apply(const ALogRecord: TEntity;
      const ALogActionID: Integer): string;

    function FieldByName(const AFieldName: string): TBaseField;

    property DefinitionName: string read GetDefinitionName;
    property Deleted: Boolean read FDeleted;
    property IsNew: Boolean read FIsNew;
    property Entity: TEntity read FEntity;
    property Fields: TObjectDictionary<string, TJSONValue> read FChangedFields;
    property FieldCount: Integer read GetFieldCount;
  end;

  // Должен содержать в себе очередь калькуляций
  TChangeHolder = class
  private
    // Каждая сущность может сохраняться только один раз
    [Weak] FDomain: TObject;
    [Weak] FSession: TObject;
    [Weak] FInteractor: TObject;
    [Weak] FParentHolder: TChangeHolder;
    FChangedEntities: TObjectList<TChangedEntity>;
    // Очередь ожидания
    FCalcQueue: TList<TCalcWaiter>;
    // статус
    FIsCalculating: Boolean;
    FIsCancelling: Boolean;
    FIsAnemic: Boolean;
    FViewLockCount: Integer;
    FViewList: TBulkUpdateViewList;

    function NeedSkipProcessing(const AEntity: TEntity): Boolean; inline;
    procedure DeleteChangedEntity(const AIndex: Integer);
    function IndexOfChangedEntity(const AEntity: TEntity): Integer;

    function IsEntityChanged(const AEntity: TEntity): Boolean;

    procedure ExecuteReactions(const AHandlers: THandlers; const AFieldChain: string;
      const AInstance, AParameter: TEntity);
    procedure DoScheduleCalculation(const AHandlers: THandlers; const AFieldChain: string;
      const AInstance, AParameter: TEntity);

    function AddTransferred(const ATransferedEntity: TChangedEntity): Boolean;
  public
    constructor Create(const ASession: TObject; const AParentHolder: TChangeHolder; const AIsAnemic: Boolean);
    destructor Destroy; override;

    procedure RegisterEntityCreating(const AEntity: TEntity);
    procedure RegisterEntityDeleting(const AEntity: TEntity);
    function KeepOldData(const AEntity: TEntity; const AFieldName: string): Boolean;
    procedure RegisterFieldChanges(const AEntity: TEntity; const AFieldName: string);

    procedure ProcessDeletingEntity(const AEntity: TEntity);
    procedure ProcessUpdatingEntity(const AInteractor: TObject; const AEntity: TEntity;
      const AFields: TJSONObject; const AContext: string);

    procedure RevertField(const AEntity: TEntity; const AFieldName: string);
	// Используется для предотвращения калькуляций
    function OldEntityValue(const AEntity: TEntity; const AFieldName: string): string;

    function ApplyChanges(const ASkipLogging: Boolean = False): Integer;
    procedure TransferChanges;
    procedure RevertChanges;

    procedure ProcessCalculations;
    procedure ScheduleCalculation(const AFieldChain: string;
      const AInstance, AParameter: TEntity);
    function IsModified: Boolean;

    procedure BeginUpdate;
    procedure EndUpdate;

    function IsVisibleModified: Boolean;
    function GetDebugInfo: string;

    property ParentHolder: TChangeHolder read FParentHolder;
    property Session: TObject read FSession;
    property Interactor: TObject read FInteractor;
    property IsAnemic: Boolean read FIsAnemic;
    property ViewList: TBulkUpdateViewList read FViewList;
  end;

implementation

uses
  SysUtils, Variants, Math, uDomain, uDefinition, uCollection, uSession,
  uDomainUtils, uInteractor, uSimpleField;

{ TChangeHolder }

// Список может хранить значения nil для того, чтобы не нарушалась индексация
//  но в этом случае isModified будет отрабатывать медленнее

function TChangeHolder.AddTransferred(const ATransferedEntity: TChangedEntity): Boolean;
var
  vIndex: Integer;
  vEntity: TEntity;
begin
  Result := False;
  vEntity := ATransferedEntity.Entity;
  if not Assigned(vEntity) then
    TDomain(FDomain).Logger.AddMessage('Entity was removed before')
  //else if vEntity.Definition is TActionDef then
    // Ничего не делаем, холдер сам его удалит
  else begin
    vIndex := IndexOfChangedEntity(vEntity);
    if vIndex < 0 then
    begin
      ATransferedEntity.FHolder := Self;
      FChangedEntities.Add(ATransferedEntity);
      TDomain(FDomain).Logger.AddMessage('fully transfered ' + vEntity.ToString);
      // Изменения забраны в самом объекте, удалять его не нужно
      Result := True;
    end
    else
      FChangedEntities[vIndex].FillFrom(ATransferedEntity);
  end;
end;

function TChangeHolder.ApplyChanges(const ASkipLogging: Boolean = False): Integer;
// Нельзя в рамках одной транзакции делать больше 32000 операций
//   убранный код позволял обойти это ограничение, но таких ситуаций практически не встречалось
var
  vChangedEntity: TChangedEntity;
  vLogRecord: TEntity;
  vTransactionID: Integer;
  vCurrentLogActionID: Integer;
  vNextLogActionID: Integer;
  vInitialCount: Integer;
  vDescription: string;
  vFullDescription: string;
  vStorage: TStorage;
  vAppliedEntities: TList<TChangedEntity>;
  vNeedRollback: Boolean;
  i: Integer;
begin
  Result := -1;

  for i := FChangedEntities.Count - 1 downto 0 do
  begin
    vChangedEntity := FChangedEntities[i];
    if (vChangedEntity.DefinitionName = 'SysLog') or (vChangedEntity.DefinitionName = 'SysLogActions') then Continue;

    TEntityChangingProc(TDomain(FDomain).Configuration.BeforeEntitySavingProc)(Self, vChangedEntity.Entity);
  end;

  vInitialCount := FChangedEntities.Count;
  if vInitialCount <= 0 then
    Exit;

  { TODO -owa :
    Вначале нужно очистить список от "грязных" значений
    (свежедобавленных) и сразу удаленных и т.п. }

  // Пересчитать все калькуляции сущности, возможно из предыдущего метода
  //ScheduleAndExecuteAllCalculations;

  // Формируем строку описания транзакции
  vFullDescription := '';


  vAppliedEntities := TList<TChangedEntity>.Create;
  try
    vStorage := TDomain(FDomain).Storage;

    vTransactionID := vStorage.BeginTransaction;
    vNeedRollback := True;

    try
      // Создаем новую запись в журнале
      if ASkipLogging then
      begin
        vLogRecord := nil;
        vCurrentLogActionID := -1;
        vNextLogActionID := -1;
      end
      else begin
        vLogRecord := TDomain(FDomain).CreateNewEntity(Self, 'SysLog');
        vLogRecord._SetFieldValue(Self, 'UserName', TUserSession(FSession).CurrentUserName);
        vNextLogActionID := vStorage.CreateIDs('sys_log_actions', vInitialCount);
        vCurrentLogActionID := vNextLogActionID - vInitialCount + 1;
      end;

      // Применение изменений
      while (FChangedEntities.Count > 0) do
      begin
        vChangedEntity := FChangedEntities.Extract(FChangedEntities[0]);
        vAppliedEntities.Add(vChangedEntity);

        if vChangedEntity.DefinitionName = 'SysLog' then
        begin
          vLogRecord := vChangedEntity.Entity;
          vLogRecord._SetFieldValue(Self, 'Description', vFullDescription);
          vChangedEntity.Apply(vLogRecord, -1);

          Result := SafeID(vLogRecord);
        end
        else if vChangedEntity.DefinitionName = 'SysLogActions' then
        begin
          vChangedEntity.Apply(vLogRecord, -1);
        end
        else if vChangedEntity.DefinitionName <> '' then
        begin
          if not ((vCurrentLogActionID > 0) or ASkipLogging) then
            Assert(False, 'Wrong LogID for the entity');
          vDescription := vChangedEntity.Apply(vLogRecord, vCurrentLogActionID);
          if not SameText(vDescription, '') then
          begin
            if vCurrentLogActionID >= 0 then
            begin
              vCurrentLogActionID := vCurrentLogActionID + 1;
              if vCurrentLogActionID > vNextLogActionID then
                vCurrentLogActionID := -1;
            end;

            if vFullDescription <> '' then
              vFullDescription := vFullDescription + #13#10;
            vFullDescription := vFullDescription + vDescription;
          end;
        end;
      end;

      vStorage.CommitTransaction(vTransactionID);
      for i := vAppliedEntities.Count - 1 downto 0 do
        vAppliedEntities[i].Free;
    except
      for i := vAppliedEntities.Count - 1 downto 0 do
        FChangedEntities.Insert(0, vAppliedEntities[i]);
      if vNeedRollback then
        vStorage.RollbackTransaction(vTransactionID);
      raise;
    end;
  finally
    FreeAndNil(vAppliedEntities);
  end;
end;

procedure TChangeHolder.BeginUpdate;
begin
  if FViewLockCount = 0 then
    FViewList := TBulkUpdateViewList.Create;
  Inc(FViewLockCount);
end;

constructor TChangeHolder.Create(const ASession: TObject; const AParentHolder: TChangeHolder; const AIsAnemic: Boolean);
begin
  inherited Create;

  FSession := ASession;
  FInteractor := TUserSession(FSession).Interactor;
  FDomain := TUserSession(FSession).Domain;
  FParentHolder := AParentHolder;
  FIsAnemic := AIsAnemic;
  
  FChangedEntities := TObjectList<TChangedEntity>.Create;
  FIsCancelling := False;
  FCalcQueue := TList<TCalcWaiter>.Create;
  FIsCalculating := False;

  FViewList := nil;
  FViewLockCount := 0;
end;

procedure TChangeHolder.DeleteChangedEntity(const AIndex: Integer);
begin
  //Core.AddMessage('%%% ENTITY DELETED: ' + IntToStr(Integer(FChangedEntityLinks[AIndex])));
  FChangedEntities.Delete(AIndex);
end;

destructor TChangeHolder.Destroy;
begin
  FreeAndNil(FCalcQueue);
  FreeAndNil(FChangedEntities);
  FreeAndNil(FViewList);

  FParentHolder := nil;
  FDomain := nil;
  FSession := nil;
  inherited Destroy;
end;

procedure TChangeHolder.DoScheduleCalculation(const AHandlers: THandlers; const AFieldChain: string;
  const AInstance, AParameter: TEntity);
var
  vWaiter: TCalcWaiter;
begin
  for vWaiter in FCalcQueue do
    if (vWaiter.Handlers = AHandlers) and (vWaiter.Instance = AInstance) and (vWaiter.Parameter = AParameter) then
      Exit;

  vWaiter.FieldChain := AFieldChain;
  vWaiter.Instance := AInstance;
  vWaiter.Parameter := AParameter;
  vWaiter.Handlers := AHandlers;
  FCalcQueue.Add(vWaiter);
end;

procedure TChangeHolder.EndUpdate;
var
  i: Integer;
  vMessage: TDomainChangedMessage;
begin
  Dec(FViewLockCount);
  if FViewLockCount > 0 then
    Exit;

  Assert(FViewLockCount = 0, 'Слишком много вызовов EndUpdate()');

  vMessage.Msg := DM_DOMAIN_CHANGED;
  vMessage.Kind := dckListEndUpdate;
  vMessage.Sender := Self;
  vMessage.Parameter := nil;
  vMessage.Holder := nil;

  try
    for i := 0 to FViewList.Count - 1 do
      FViewList[i].Dispatch(vMessage);
  finally
    FreeAndNil(FViewList);
  end;
end;

procedure TChangeHolder.ExecuteReactions(const AHandlers: THandlers; const AFieldChain: string;
  const AInstance, AParameter: TEntity);
var
  vHandler: TProc;
begin
  for vHandler in AHandlers do
    TReactionProcRef(vHandler)(Self, AFieldChain, AInstance, AParameter);
end;

function TChangeHolder.GetDebugInfo: string;
var
  vChangedEntity: TChangedEntity;
  vFieldName: string;
  vPrefix: string;
begin
  Result := '';

  for vChangedEntity in FChangedEntities do
  begin
    vPrefix := Format('[%d]', [SafeID(vChangedEntity.Entity)]);
    if vChangedEntity.IsNew then
      vPrefix := vPrefix + '[new]';
    if vChangedEntity.FDeleted then
      vPrefix := vPrefix + '[del]';

    Result := Result + #13#10'Entity' + vPrefix + ': ' + SafeDisplayName(vChangedEntity.Entity) +
      ' - ' + IntToStr(vChangedEntity.FieldCount);
    for vFieldName in vChangedEntity.Fields.Keys do
      Result := Result + #13#10' >>>' + vFieldName;
  end;
end;

function TChangeHolder.IndexOfChangedEntity(const AEntity: TEntity): Integer;
begin
  for Result := 0 to FChangedEntities.Count - 1 do
    if FChangedEntities[Result].Entity = AEntity then
      Exit;
  Result := -1;
end;

function TChangeHolder.IsEntityChanged(const AEntity: TEntity): Boolean;
var
  i: Integer;
begin
  for i := 0 to FChangedEntities.Count - 1 do
    if FChangedEntities[i].Entity = AEntity then
      Exit(True);
  if Assigned(FParentHolder) then
    Result := FParentHolder.IsEntityChanged(AEntity)
  else
    Result := False;
end;

function TChangeHolder.IsModified: Boolean;
begin
  Result := (FChangedEntities.Count > 0);
end;

function TChangeHolder.IsVisibleModified: Boolean;
var
  vChangedEntity: TChangedEntity;
  vFieldName: string;
  vField: TBaseField;
begin
  Result := True;
  if IsModified then
  begin
    for vChangedEntity in FChangedEntities do
    begin
      if vChangedEntity.Deleted then
        Exit;
      for vFieldName in vChangedEntity.Fields.Keys do
      begin
        vField := vChangedEntity.FieldByName(vFieldName);
        if vField.GetUIState(FSession) > vsHidden then
          Exit;
      end;
    end;
  end;
  Result := False;
end;

function TChangeHolder.KeepOldData(const AEntity: TEntity; const AFieldName: string): Boolean;
var
  vIndex: Integer;
  vChangedEntity: TChangedEntity;
begin
  Result := False;
  if FIsAnemic or FIsCancelling then
    Exit;

  vIndex := IndexOfChangedEntity(AEntity);
  if vIndex < 0 then
  begin
    vChangedEntity := TChangedEntity.Create(Self, AEntity, False, False);
    FChangedEntities.Add(vChangedEntity);
  end
  else
    vChangedEntity := TChangedEntity(FChangedEntities[vIndex]);

  Result := not vChangedEntity.KeepOldData(AFieldName);
  if not Result then
    TDomain(FDomain).Logger.AddMessage(Format('$$$ [%d] KEPT OLD DATA %s - %s',
      [vChangedEntity.FieldCount, AEntity.ToString, AFieldName]));
end;

function TChangeHolder.NeedSkipProcessing(const AEntity: TEntity): Boolean;
begin
  Result := FIsCancelling or not Assigned(AEntity);
  if not Result then
    Result := AEntity.Definition.HasFlag(ccNotSave);
end;

function TChangeHolder.OldEntityValue(const AEntity: TEntity; const AFieldName: string): string;
var
  vIndex: Integer;
  vChangedEntity: TChangedEntity;
begin
  Result := '';
  if FIsCancelling then
    Exit;

  vIndex := IndexOfChangedEntity(AEntity);
  if vIndex < 0 then
    Exit;

  vChangedEntity := TChangedEntity(FChangedEntities[vIndex]);
  Result := vChangedEntity.OldFieldValue(AFieldName);
end;

procedure TChangeHolder.RegisterFieldChanges(const AEntity: TEntity; const AFieldName: string);
var
  vIndex: Integer;
  vChangedEntity: TChangedEntity;
begin
  if FIsAnemic or FIsCancelling then
    Exit;

  vIndex := IndexOfChangedEntity(AEntity);
  if vIndex < 0 then
    Exit;

  vChangedEntity := TChangedEntity(FChangedEntities[vIndex]);
  TDomain(FDomain).Logger.AddMessage('$$$ TRY NEW DATA ' + AFieldName);
  // Мы можем удалить сущность из списка измененных только в случае,
  //   если она не новая и если в ней нет измененных полей
  if not vChangedEntity.RegisterFieldChanges(AFieldName) then
  begin
    if not vChangedEntity.IsNew and not vChangedEntity.Deleted then
      DeleteChangedEntity(vIndex);
  end;
end;

procedure TChangeHolder.ProcessCalculations;
var
  vCurrentWaiter: TCalcWaiter;
begin
  if FIsCalculating then
    Exit;

  FIsCalculating := True;
  try
    while FCalcQueue.Count > 0 do
    begin
      vCurrentWaiter := FCalcQueue.Extract(FCalcQueue[0]);
      ExecuteReactions(vCurrentWaiter.Handlers, vCurrentWaiter.FieldChain,
        vCurrentWaiter.Instance, vCurrentWaiter.Parameter);
    end;
  finally
    FIsCalculating := False;
  end;
end;

procedure TChangeHolder.ProcessDeletingEntity(const AEntity: TEntity);
var
  vIndex: Integer;
begin
  if FIsAnemic then
    Exit;

  vIndex := IndexOfChangedEntity(AEntity);
  if vIndex >= 0 then
    DeleteChangedEntity(vIndex);
end;

procedure TChangeHolder.ProcessUpdatingEntity(const AInteractor: TObject; const AEntity: TEntity;
  const AFields: TJSONObject; const AContext: string);
var
  vIndex: Integer;
  vChangedEntity: TChangedEntity;
  vFieldName: string;
  vField: TBaseField;
  jPair: TJSONPair;
  jThisValue: TJSONValue;
  vEntityName: string;
  vIncomingValue: string;
  vThisValue: string;
  vCurrentUser: TEntity;
  vCollisionActionID: Integer;
begin
  if FIsAnemic then
    Exit;

  vIndex := IndexOfChangedEntity(AEntity);
  if vIndex < 0 then
    Exit;

  vCurrentUser := TUserSession(FSession).CurrentUser;
  if Assigned(vCurrentUser) then
    vCollisionActionID := Max(1, SafeID(vCurrentUser.ExtractEntity('CollisionAction')))
  else
    vCollisionActionID := 3; // Автоматически, в пользу пришедших изменений

  vChangedEntity := TChangedEntity(FChangedEntities[vIndex]);
  for vFieldName in vChangedEntity.Fields.Keys do
  begin
    vField := vChangedEntity.FieldByName(vFieldName);
    if not Assigned(vField) then
      Continue;

    jPair := AFields.Get(vFieldName);
    if not Assigned(jPair) then
      Continue;

    vIncomingValue := JSONValueToString(FDomain, jPair.JsonValue, vField.FieldKind);
    jThisValue := vField.JSON;
    try
      vThisValue := JSONValueToString(FDomain, jThisValue, vField.FieldKind);
    finally
      FreeAndNil(jThisValue);
    end;

    // неверно, т.к. при добавлении двух сумм а=10 и б=10, общая сумма у каждого будет 10
    //   и эта проверка для поля пройдет, хотя надо бы его пересчитать как 10+10=20
    if CompareStr(vIncomingValue, vThisValue) = 0 then
      Continue;

    vEntityName := Format('%s(%d):%s)', [AEntity.Definition.Name, AEntity.ID, AEntity.DisplayName]);

    // True - оставить своё
    // False - принять пришедшее
    case vCollisionActionID of
      2: begin
        //В пользу своих изменений
        jPair := AFields.RemovePair(vFieldName);
        FreeAndNil(jPair);
      end;
      3: begin
        //В пользу пришедших изменений
        vChangedEntity.Fields.Remove(vFieldName);
      end;
    else
      begin
        if Assigned(AInteractor) and (TInteractor(AInteractor).ShowYesNoDialog(
          TDomain(FDomain).Translate('txtVersionsConflict', 'Конфликт версий'),
          Format(AContext, [vFieldName, vEntityName, vIncomingValue, vThisValue]), False) = drYes)
        then begin
          //В пользу своих изменений
          jPair := AFields.RemovePair(vFieldName);
          FreeAndNil(jPair);
        end
        else
          //В пользу пришедших изменений
          vChangedEntity.Fields.Remove(vFieldName);
      end;
    end;
  end;

  if vChangedEntity.FieldCount <= 0 then
    DeleteChangedEntity(vIndex);
end;

procedure TChangeHolder.RegisterEntityCreating(const AEntity: TEntity);
begin
  if FIsAnemic or NeedSkipProcessing(AEntity) then
    Exit;
  FChangedEntities.Add(TChangedEntity.Create(Self, AEntity, True, False));
  TDomain(FDomain).Logger.AddMessage('$$$ NEW ENTITY, type: ' + AEntity.Definition.Name);
end;

procedure TChangeHolder.RegisterEntityDeleting(const AEntity: TEntity);
var
  vIndex: Integer;
  vChangedEntity: TChangedEntity;
begin
  if FIsAnemic or NeedSkipProcessing(AEntity) then
    Exit;

  vIndex := IndexOfChangedEntity(AEntity);
  if vIndex >= 0 then
  begin
    vChangedEntity := TChangedEntity(FChangedEntities[vIndex]);
    vChangedEntity.FDeleted := True;
    if vChangedEntity.IsNew and vChangedEntity.Deleted then
    begin
      DeleteChangedEntity(vIndex);
      TDomain(FDomain).Logger.AddMessage('$$$ REMOVE CHANGED ENTITY');
    end;
  end
  else
    FChangedEntities.Add(TChangedEntity.Create(Self, AEntity, False, True));
end;

procedure TChangeHolder.RevertChanges;
begin
  FCalcQueue.Clear;
  FIsCancelling := True;
  try
    while FChangedEntities.Count > 0 do
    begin
      FChangedEntities[0].Revert;
      FChangedEntities.Delete(0);
    end;
  finally
    FIsCancelling := False;
  end;
end;

procedure TChangeHolder.RevertField(const AEntity: TEntity; const AFieldName: string);
var
  vIndex: Integer;
  vChangedEntity: TChangedEntity;
begin
  if FIsAnemic then
    Exit;

  vIndex := IndexOfChangedEntity(AEntity);
  if vIndex >= 0 then
  begin
    vChangedEntity := TChangedEntity(FChangedEntities[vIndex]);
    vChangedEntity.RevertField(AFieldName);
  end;
end;

procedure TChangeHolder.ScheduleCalculation(const AFieldChain: string;
  const AInstance, AParameter: TEntity);
var
  vHandlers: THandlers;
begin
  vHandlers := TReactions(AInstance.Definition.Reactions).FindReactions(AFieldChain);
  if not Assigned(vHandlers) then
    Exit;

  // Проверить, нужен ли пересчет в реальном времени
  if (FChangedEntities.Count = 0) or IsEntityChanged(AInstance) then
    ExecuteReactions(vHandlers, AFieldChain, AInstance, AParameter)
  else
    DoScheduleCalculation(vHandlers, AFieldChain, AInstance, AParameter);
end;

procedure TChangeHolder.TransferChanges;
var
  i: Integer;
  vChangedEntity: TChangedEntity;
begin
  //ProcessCalculations;

  for i := FChangedEntities.Count - 1 downto 0 do
  begin
    vChangedEntity := FChangedEntities.Extract(FChangedEntities[i]);
    if not FParentHolder.AddTransferred(vChangedEntity) then
      FreeAndNil(vChangedEntity);
  end;
end;

{ TChangedEntity }

function TChangedEntity.Apply(const ALogRecord: TEntity; const ALogActionID: Integer): string;
var
  vDomain: TDomain;
  vStorage: TStorage;
  vSkipLogging: Boolean;

  function GenerateLogAction(const AEntity: TEntity;
    const AActionKind: TEntitySaveAction): string;
  var
    vLogAction: TEntity;
    vDescription: string;
    vCaption: string;
    i: Integer;
    vPair: TPair<string, TJSONValue>;
    vField: TBaseField;
    vJSONObject: TJSONObject;
    vValue: TJSONValue;
  begin
    Result := '';
    if not Assigned(ALogRecord) then
      Exit;

    if (AEntity.CollectionName = 'SysLogActions') or (AEntity.CollectionName = 'SysLog') then
      Exit;

    vCaption := AEntity.GetCaption(nil);
    vLogAction := vDomain.CreateNewEntity(FHolder, 'SysLogActions');
    vLogAction.SetID(ALogActionID);
    vLogAction._SetFieldEntity(FHolder, 'SysLog', ALogRecord);
    vLogAction._SetFieldValue(FHolder, 'CollectionName', AEntity.CollectionName);
    vLogAction._SetFieldValue(FHolder, 'EntityID', AEntity.ID);
    vLogAction._SetFieldEntity(FHolder, 'ActionKind', vDomain.EntityByID('SysLogActionKinds', Integer(AActionKind)));

    vDescription := '';
    vJSONObject := TJSONObject.Create;
    try
      Result := vCaption + ': ' + cSaveActionNames[AActionKind] + #13#10;
      case AActionKind of
        esaInsert:
          for i := 0 to AEntity.FieldCount - 1 do
          begin
            vField := AEntity.Fields[i];
            if not vField.FieldDef.HasFlag(cSystem) then
              vDescription := vDescription + vField.LogInfo;
            if vField.FieldDef.HasFlag(cNotSave) then
              Continue;
            vValue := vField.JSON;
            if Assigned(vValue) then
              vJSONObject.AddPair(TJSONPair.Create(vField.FieldName, vValue));
          end;
        esaDelete:
          for i := 0 to AEntity.FieldCount - 1 do
          begin
            vField := AEntity.Fields[i];
            if not vField.FieldDef.HasFlag(cSystem) then
              vDescription := vDescription + vField.LogInfo;
          end;
        esaUpdate:
          for vPair in FChangedFields do
          begin
            vField := FieldByName(vPair.Key);
            if not vField.FieldDef.HasFlag(cSystem) then
              vDescription := vDescription + vField.LogInfo(vPair.Value);
            if vField.FieldDef.HasFlag(cNotSave) then
              Continue;
            vValue := vField.JSON;
            if Assigned(vValue) then
              vJSONObject.AddPair(TJSONPair.Create(vPair.Key, vValue));
          end;
      end;

      vLogAction._SetFieldValue(FHolder, 'Description', vDescription);
      vLogAction._SetFieldValue(FHolder, 'JSON', vJSONObject.ToString);
      Result := Result + vDescription;
    finally
      FreeAndNil(vJSONObject);
    end;
  end;

begin
  Result := '';

  if not Assigned(FEntity) then
    Exit;
  if FEntity.IsParam then
    Exit;
  if FEntity.Definition.HasFlag(ccNotSave) then
    Exit;

  vSkipLogging := (FEntity.Definition.Name = 'SysLog') or (FEntity.Definition.Name = 'SysLogActions');

  vDomain := TDomain(FDomain);
  vStorage := vDomain.Storage;
  vStorage.Activate(FEntity.Definition.StorageName);
  if not vSkipLogging then
    vDomain.Logger.AddEnterMessage('SAVING ' + FEntity.ToString)
  else begin
    vDomain.Logger.AddMessage('SAVING ' + FEntity.ToString);
    vDomain.Logger.Enabled := False;;
  end;
  try
    try
      // Сформировать запись лога
      if FDeleted then
      begin
        vDomain.Logger.AddMessage('Deletion...');
        if FEntity.ID > 0 then
        begin
          Result := GenerateLogAction(FEntity, esaDelete);
          vDomain.Storage.DeleteItem(FEntity.ID);
        end;
        TCollection(FEntity.Collection).RemoveEntity(FEntity);
      end
      else begin
        FEntity.LogID := ALogActionID;
        if FEntity.IsNew then
        begin
          vDomain.Logger.AddMessage('Addition...');
          if FEntity.ID < 0 then
            FEntity.GenerateID;
          vStorage.AddItem(TransferAllFields, FEntity.ID, ALogActionID);
          FEntity.IsNew := False;
          if Assigned(FEntity.Collection) then
            TCollection(FEntity.Collection).ProcessEntitySaving(FHolder, FEntity);
          Result := GenerateLogAction(FEntity, esaInsert);
        end
        else begin
          vDomain.Logger.AddMessage('Updating...');
          vStorage.UpdateItem(TransferChangedFields, FEntity.ID, ALogActionID);
          if Assigned(FEntity.Collection) then
            TCollection(FEntity.Collection).ProcessEntitySaving(FHolder, FEntity);
          Result := GenerateLogAction(FEntity, esaUpdate);
        end;
      end;

      if not vSkipLogging then
        vDomain.Logger.AddExitMessage('SAVED')
      else begin
        vDomain.Logger.Enabled := True;
        vDomain.Logger.AddMessage('SAVED');
      end;
    except
      on E: Exception do
      begin
        //Revert;
        if not vSkipLogging then
          vDomain.Logger.AddExitMessage('Error while storing data: ' + E.Message)
        else begin
          vDomain.Logger.Enabled := True;
          vDomain.Logger.AddMessage('Error while storing data: ' + E.Message)
        end;
        Result := '';
        raise;
      end;
    end;
  finally
    vStorage.Deactivate;
  end;
end;

constructor TChangedEntity.Create(const AHolder: TChangeHolder; const AEntity: TEntity; const AIsNew, ADeleted: Boolean);
begin
  inherited Create;
  FHolder := AHolder;
  FDomain := FHolder.FDomain;
  Assert(Assigned(AEntity), 'Wrong data for keeping changes!');
  FEntity := AEntity;

  FDefinition := AEntity.Definition;

  FIsNew := AIsNew;
  FDeleted := ADeleted;
  FChangedFields := TObjectDictionary<string, TJSONValue>.Create([doOwnsValues]);
end;

destructor TChangedEntity.Destroy;
begin
  FreeAndNil(FChangedFields);

  FEntity := nil;
  FDomain := nil;
  FDefinition := nil;
  FHolder := nil;

  inherited Destroy;
end;

function TChangedEntity.FieldByName(const AFieldName: string): TBaseField;
begin
  if Assigned(FEntity) then
    Result := FEntity.FieldByName(AFieldName)
  else
    Result := nil;
end;

procedure TChangedEntity.FillFrom(const ASource: TChangedEntity);
var
  vFieldName: string;
  vSrcPair: TPair<string, TJSONValue>;
begin
  if not Assigned(FEntity) then
    Exit;

  TDomain(FDomain).Logger.AddEnterMessage('PASSING ' + FEntity.ToString);
  FIsNew := ASource.FIsNew;
  FDeleted := ASource.FDeleted;

  for vFieldName in ASource.Fields.Keys do
  begin
    TDomain(FDomain).Logger.AddMessage('> ' + vFieldName + ': transferred');
    vSrcPair := ASource.Fields.ExtractPair(vFieldName);
    FChangedFields.AddOrSetValue(vFieldName, vSrcPair.Value);
  end;
  TDomain(FDomain).Logger.AddExitMessage('PASSED');
end;

function TChangedEntity.GetDefinitionName: string;
begin
  Result := TDefinition(FDefinition).Name;
end;

function TChangedEntity.GetFieldCount: Integer;
begin
  Result := FChangedFields.Count;
end;

function TChangedEntity.KeepOldData(const AFieldName: string): Boolean;
var
  vField: TBaseField;
begin
  Result := not FChangedFields.ContainsKey(AFieldName);
  if Result then
  begin
    vField := FieldByName(AFieldName);
    if vField.FieldKind = fkComplex then
      FChangedFields.Add(AFieldName, nil)
    else
      FChangedFields.Add(AFieldName, vField.JSON);
  end;
end;

function TChangedEntity.OldFieldValue(const AFieldName: string): string;
var
  vField: TBaseField;
  vSavedValue: TJSONValue;
begin
  Result := '';
  if FChangedFields.TryGetValue(AFieldName, vSavedValue) then
  begin
    vField := FieldByName(AFieldName);
    Result := JSONValueToString(FDomain, vSavedValue, vField.FieldKind);
  end;
end;

function TChangedEntity.RegisterFieldChanges(const AFieldName: string): Boolean;
var
  vField: TBaseField;
  vSavedValue: TJSONValue;
  vNewValue: TJSONValue;
begin
  Result := True;
  if FChangedFields.TryGetValue(AFieldName, vSavedValue) then
  begin
    vField := FieldByName(AFieldName);
    vNewValue := vField.JSON;
    try
      if ValuesAreEqual(FDomain, vSavedValue, vNewValue, vField.FieldKind) then
      begin
        FChangedFields.Remove(AFieldName);
        Result := FChangedFields.Count > 0;
      end
      else
        TDomain(FDomain).Logger.AddMessage('$$$ SKIPPED');
    finally
      FreeAndNil(vNewValue);
    end;
  end;
end;

procedure TChangedEntity.Revert;
var
  vPair: TPair<string, TJSONValue>;
  vField: TBaseField;
begin
  if not Assigned(FEntity) then
    Exit;

  TDomain(FDomain).Logger.AddEnterMessage('CANCELING ' + FEntity.ToString);

  if FIsNew then
  begin
    FEntity.Delete(FHolder);
    TCollection(FEntity.Collection).RemoveEntity(FEntity);
  end
  else begin
    if FDeleted then
    begin
      TDomain(FDomain).Logger.AddMessage('UNDELETED');
      FEntity.Deleted := False;
    end;

    for vPair in FChangedFields do
    begin
      vField := FieldByName(vPair.Key);
      if not Assigned(vField) then
        Continue;

      if vField.FieldKind = fkComplex then
      begin
        if Assigned(TComplexField(vField).InitialJSON) then
          vField.JSON := TComplexField(vField).InitialJSON;
      end
      else
        vField.JSON := vPair.Value;
      TDomain(FDomain).Logger.AddMessage('<< ' + vField.FieldName + ': ' + cFieldKindNames[vField.FieldKind]);
    end;
  end;

  TDomain(FDomain).Logger.AddExitMessage('CANCELED');
end;

procedure TChangedEntity.RevertField(const AFieldName: string);
var
  vField: TBaseField;
  vSavedValue: TJSONValue;
begin
  if not FChangedFields.TryGetValue(AFieldName, vSavedValue) then
    Exit;

  vField := FieldByName(AFieldName);
  if not Assigned(vField) then
    Exit;

  if vField.FieldKind = fkComplex then
  begin
    if Assigned(TComplexField(vField).InitialJSON) then
      vField.JSON := TComplexField(vField).InitialJSON;
  end
  else
    vField.JSON := vSavedValue;

  FChangedFields.Remove(AFieldName);

  TDomain(FDomain).Logger.AddMessage('<< ' + vField.FieldName + ': ' + cFieldKindNames[vField.FieldKind]);
end;

procedure TChangedEntity.TransferAllFields(const AStorage: TStorage);
var
  i: Integer;
begin
  if not Assigned(FEntity) then
    Exit;

  { BOOKMARK : Обработка сохранения новых сервисных полей }
  if FEntity.IsEnvSpecific then
    AStorage.WriteValue('guid', fkString, FEntity.EnvironmentID);
  for i := 0 to FEntity.FieldCount - 1 do
    FEntity.Fields[i].Transfer(AStorage);
end;

procedure TChangedEntity.TransferChangedFields(const AStorage: TStorage);
var
  vPair: TPair<string, TJSONValue>;
  vField: TBaseField;
begin
  { BOOKMARK : Обработка сохранения измененных сервисных полей }
  for vPair in FChangedFields do
  begin
    vField := FieldByName(vPair.Key);
    if Assigned(vField) then
      vField.Transfer(AStorage);
  end;
end;

end.

