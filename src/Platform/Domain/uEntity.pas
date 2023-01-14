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

unit uEntity;

interface

uses
  Classes, Generics.Collections, SysUtils, IniFiles, uStorage, uComplexObject,
  uConsts, uJSON, uDefinition, uEnumeration;

//const
  // Possible actions for TEntityList
  //lpaAdd    = 1;
  //lpaLink   = 2;
  //lpaRemove = 4;

type
  TEntity = class;

  TBaseField = class
  protected
    [Weak] FInstance: TEntity;
    [Weak] FFieldDef: TFieldDef;
    FValidationStatus: TValidateStatus;
    FUIState: TViewState;
    FCalcUIState: TViewState;
    FEnabled: Boolean;
    // Основные свойства поля
    function GetFieldName: string; inline;
    function GetFieldKind: TFieldKind; inline;
    function GetStorageName: string; inline;
    function GetStorageKind: TStorageKind; inline;
    // Работа с JSON-сериализацией
    function GetJSON: TJSONValue;
    procedure SetJSON(const AJSONValue: TJSONValue);
  private
    function GetMaxUIState: TViewState;
    function InternalGetUIState: TViewState;
  protected
    [Weak] FDomain: TObject;
    procedure BeforeChanges(const AHolder: TObject); virtual;
    procedure AfterChanges(const AHolder: TObject); virtual;
    // Установка и получение значения
    function DoGetValue: Variant; virtual;
    procedure DoSetValue(const AValue: Variant); virtual;
    function GetJSONValue: TJSONValue; virtual;
    procedure SetJSONValue(const AJSONValue: TJSONValue); virtual;
    // Сериализация
    //   Загрузка
    procedure DoLoad(const AStorage: TStorage); virtual; abstract;
    //   Передача значений
    procedure DoTransfer(const AStorage: TStorage); virtual; abstract;
    // Валидация поля (скриптовой метод)
    function CheckIsValid: Boolean; virtual;

    // Используется для запросов
    function DoExtractFieldValue(const AFieldName: string): Variant; virtual;

    function DoCompare(const AValue: Variant): Integer; virtual; abstract;
    function DoCheckValuedCondition(const AValue: Variant; const ACondition: TConditionKind;
      const AModifier: TConditionModifier): Boolean; virtual; abstract;
  public
    constructor Create(const AInstance: TEntity; const AFieldDef: TFieldDef); virtual;
    destructor Destroy; override;

    function LogInfo(const AOldValue: TJSONValue = nil): string;

    // Заполнение объекта из JSON
    procedure SetFromJSON(const AJSONObject: TJSONObject; const AIsNew: Boolean);

    procedure Transfer(const AStorage: TStorage);
    procedure Load(const AStorage: TStorage);

    function ExtractFieldValue(const AFieldName: string = ''): Variant;
    function Compare(const AValue: Variant): Integer;
    function CheckValuedCondition(const AValue: Variant; const ACondition: TConditionKind;
      const AModifier: TConditionModifier): Boolean;

    function GetUIState(const ASession: TObject): TViewState;
    procedure SetUIState(const Value: TViewState);

    procedure SetValue(const AHolder: TObject; const AValue: Variant);
    function IsValid: Boolean;

    property Domain: TObject read FDomain;
    property FieldName: string read GetFieldName;
    property StorageName: string read GetStorageName;
    property FieldKind: TFieldKind read GetFieldKind;
    property OwnerInstance: TEntity read FInstance;
    property FieldDef: TFieldDef read FFieldDef;
    property Value: Variant read DoGetValue write DoSetValue;
    property JSON: TJSONValue read GetJSON write SetJSON;
    property Enabled: Boolean read FEnabled;
    property ValidationStatus: TValidateStatus read FValidationStatus write FValidationStatus;
  end;

  TEntity = class
  private
    // Служебная информация о состоянии
    FID: Integer;
    FEnvironmentID: string;
    FLogID: Integer;
    FIsNew: Boolean;
    FIsEnvSpecific: Boolean;
    FIsRemote: Boolean;
    FDeleted: Boolean;
    FIsService: Boolean;
    FPassObject: TObject; // Объект для передачи дополнительной информации
  private
    procedure LoadFields(const AStorage: TStorage);
    procedure ActivateField(const AHolder: TObject; const AFieldDef: TFieldDef);
  protected
    procedure InternalLoad(const AStorage: TStorage);
  private
    function GetField(const AIndex: Integer): TBaseField;
    function GetFieldCount: Integer;
    function GetAnyFieldValue(const AFieldName: string): Variant;
    function GetCollectionName: string;
    function InternalGetField(const AFieldName: string): TBaseField; inline;

    function CreateField(const AFieldDef: TFieldDef): TBaseField;
    function GetUIListenerCount: string;
    procedure SetEnvironmentID(const Value: string);
  public
    procedure EnableField(const AField: TBaseField);
  protected
    [Weak] FDomain: TObject;
    [Weak] FDefinition: TDefinition;
    [Weak] FCollection: TObject;
    FListeners: TObjectDictionary<TEntity, TStrings>;
    FUIListeners: TObjectDictionary<string, TList<TObject>>;
    FFieldList: TObjectList<TBaseField>;
    FViewState: TViewState;
    procedure CreateFields;
    procedure FieldInternalSetValue(const AField: TBaseField; const AValue: Variant);
    procedure SetViewState(const Value: TViewState); virtual;
  public
    constructor Create(const ADomain: TObject; const ADefinition: TDefinition); virtual;
    destructor Destroy; override;

    // Работа с внутренними хранилищами полей и групп
    procedure UpdateFields;

    // Процедура нужна, чтобы уведомить, что сущность создана и загружена
    //   и дать вложенным полям настроить свои подписки
    procedure SubscribeFields(const AHolder: TObject);
    procedure UnsubscribeFields(const AHolder: TObject);

    procedure AddListener(const AFieldName: string; const AInstance: TEntity);
    procedure RemoveListener(const AFieldName: string; const AInstance: TEntity);
    procedure RemoveAllListeners;

    procedure AddUIListener(const AFieldName: string; const AView: TObject);
    procedure RemoveUIListener(const AFieldName: string; const AView: TObject);

    // ИЗВЛЕЧЕНИЕ ИНФОРМАЦИИ
    function FieldExists(const AFieldName: string): Boolean; inline;
    // Извлечение значения поля, которое может иметь произвольную вложенность
    //   Name, Person:LegalPersons, OSAGO.Vehicle.VIN,
    //   Contract.Vendor:LegalPersons.OPForm.Name, Customer.IsNull
    function FieldByName(const AFieldName: string): TBaseField;
    // Используется для формирования запросов
    function ExtractFieldValue(const AFieldName: string; const AUseNameForObjects: Boolean = True): Variant;
    // Ускоренное извлечение сущности из поля (в том числе и составного)
    //   Name, VendorLastName, Person, Person:LegalPersons, Drivers[1]
    function ExtractEntity(const AFieldName: string): TEntity;

    function IsParam: Boolean;
    function IsValid: Boolean;
    procedure ResetToDefault(const AHolder: TObject);

    property Fields[const AIndex: Integer]: TBaseField read GetField;
    property FieldCount: Integer read GetFieldCount;
    // Получение значений поля для отображения в гридах и для расчетов
    property FieldValues[const AFieldName: string]: Variant read GetAnyFieldValue; default;
    function VisibleFieldCount(const ASession: TObject): Integer;

    function InstanceOf(const ATypeName: string): Boolean;
    property Domain: TObject read FDomain;
    property Definition: TDefinition read FDefinition;
    property Collection: TObject read FCollection;
    property CollectionName: string read GetCollectionName;
    property UIListenerCount: string read GetUIListenerCount;
    property IsService: Boolean read FIsService write FIsService;

    function GetCaption(const AInteractor: TObject): string;
  public
    procedure ProcessMasterFieldChanged(const AHolder: TObject; const ADependentFieldDef: TFieldDef;
      const AMasterFieldName, ALinkFieldName: string);
    procedure ProcessFieldChanged(const AHolder: TObject; const AChangeKind: Word; const AFieldName: string;
      const AEntity: TEntity);
    procedure ProcessLinkedEntityChanged(const AHolder: TObject; const AFieldName: string; const APrevChain: string;
      const AEntity: TEntity);
    procedure ProcessLinkedEntityDeleted(const AHolder: TObject; const AFieldName: string; const AEntity: TEntity);
    procedure NotifyView(const AHolder: TObject; const AChangeKind: Word; const AParameter: TObject; const AFieldName: string = '');

    // Автоматическое управление листовыми полями
    procedure TryRemoveFromForeignList(const AHolder: TObject; const AField: TBaseField);
    procedure TryAddToForeignList(const AHolder: TObject; const AField: TBaseField);
    procedure TryUpdateForeignList(const AHolder: TObject; const AFilterField: TBaseField);

    procedure FieldInitialize(const AField: TBaseField);
    function FieldToString(const AFieldName: string): string;
    procedure _SetFieldValue(const AHolder: TObject; const AFieldName: string; const AValue: Variant);
    procedure _SetFieldEntity(const AHolder: TObject; const AFieldName: string; const AEntity: TEntity);
    procedure _SetFieldStream(const AHolder: TObject; const AFieldName: string; const AStream: TStream);
    procedure _SetFieldObject(const AHolder: TObject; const AFieldName: string; const AObject: TComplexObject);
    procedure _RestoreFieldValue(const AHolder: TObject; const AFieldName: string);
    function _SetState(const AHolder: TObject; const ANewState: Variant): Boolean;
    function GetFieldValue(const AFieldName: string): Variant;
    function GetFieldEntity(const AFieldName: string): TEntity;
    function GetFieldBlob(const AFieldName: string): TStream;
    function GetFieldList(const AFieldName: string): TList<TEntity>;
    function GetFieldObject(const AFieldName: string): TComplexObject;
    function EntityState(const AFieldName: string = ''): TState;
    function GetStateCaption(const AFieldName: string): string;
  public
    // Управление содержимым сущности
    // Обращение к коллекции
    procedure FillEntity(const AHolder: TObject; const ATargetEntity: TEntity; const AIgnoreFields: string);
    function Clone(const AHolder: TObject; const AIgnoreFields: string): TEntity;

    procedure GenerateID;
    function FindSimilar(const ASession: TObject): TEntity;

    procedure FillFromJSON(const AJSONObject: TJSONObject; const AIsNew: Boolean);
    procedure ExportToJSON(const AJSONObject: TJSONObject);

    // Состояние и основные свойства сущности
    procedure SetID(const AID: Integer);
    property ID: Integer read FID;
    property EnvironmentID: string read FEnvironmentID write SetEnvironmentID;
    property LogID: Integer read FLogID write FLogID;
    procedure Populate(const AFieldNames: string; const AValues: array of Variant; const APopulateAll: Boolean = True);
    procedure Delete(const AHolder: TObject);
    procedure SetDeleted(const AHolder: TObject);

    function _FieldText(const AFieldName: string): string;
    function FullText: string;

    property IsNew: Boolean read FIsNew write FIsNew;
    property IsEnvSpecific: Boolean read FIsEnvSpecific;
    property IsRemote: Boolean read FIsRemote;
    property Deleted: Boolean read FDeleted write FDeleted;
    //TODO Костыль для состояний
    property ViewState: TViewState read FViewState write SetViewState;
    property PassObject: TObject read FPassObject write FPassObject;

    property Listeners: TObjectDictionary<TEntity, TStrings> read FListeners;

    // Сбор и вывод информации о сущности
    function DisplayName: string;
    function ToString: string; override;
  end;

  TEntityClass = class of TEntity;

type
  TDisplayNameRef = reference to function(const AEntity: TEntity): string;
  TDisplayNameFunc = function(const AEntity: TEntity): string of object;
  TGetParamValueFunc = function(const AEntity: TEntity; const AParamName: string): string of object;

function SafeID(const AEntity: TEntity): Integer;
function SafeDisplayName(const AEntity: TEntity; const ADefault: string = cNullItemName): string;
function SafeCode(const AEntity: TEntity; const ADefault: string = ''): string;
function SafeToString(const AEntity: TEntity): string;

implementation

uses
  Math, StrUtils, Variants, Types, DateUtils,

  uPlatform, uDomain, uCollection, uConfiguration, uUtils, uDomainUtils,
  uSession, uChangeManager, uReaction, uSimpleField, uObjectField;

type
  TCheckFieldFunc = function(const AEntity: TEntity; const AFieldName: string): Boolean of object;
  TFullTextFunc = function(const AEntity: TEntity): string of object;

function SafeDisplayName(const AEntity: TEntity; const ADefault: string = cNullItemName): string;
begin
  if Assigned(AEntity) then
  begin
    Result := AEntity.DisplayName;
    if AEntity.IsRemote then
      Result := 'REMOTE: ' + Result;
  end
  else
    Result := ADefault;
end;

function SafeCode(const AEntity: TEntity; const ADefault: string = ''): string;
begin
  if Assigned(AEntity) and AEntity.FieldExists('Code') then
    Result := AEntity['Code']
  else
    Result := ADefault;
end;

function SafeID(const AEntity: TEntity): Integer;
begin
  if not Assigned(AEntity) then
    Result := 0
  else
    Result := AEntity.ID;
end;

function SafeToString(const AEntity: TEntity): string;
begin
  if not Assigned(AEntity) then
    Result := '{{EMPTY}}'
  else
    Result := AEntity.ToString;
end;

function BoolToStr(const AValue: Boolean): string;
begin
  if AValue then
    Result := 'Yes'
  else
    Result := 'No';
end;

{ TBaseField }

procedure TBaseField.AfterChanges(const AHolder: TObject);
begin
  if FFieldDef.UsedInFilters then
    FInstance.TryUpdateForeignList(AHolder, Self);
end;

procedure TBaseField.BeforeChanges(const AHolder: TObject);
begin
end;

function TBaseField.CheckIsValid: Boolean;
begin
  Result := TCheckFieldFunc(TDomain(FDomain).Configuration.CheckFieldFunc)(TEntity(FInstance), GetFieldName);
end;

function TBaseField.CheckValuedCondition(const AValue: Variant; const ACondition: TConditionKind;
  const AModifier: TConditionModifier): Boolean;
begin
  Result := DoCheckValuedCondition(AValue, ACondition, AModifier);
end;

function TBaseField.Compare(const AValue: Variant): Integer;
begin
  Result := DoCompare(AValue);
end;

constructor TBaseField.Create(const AInstance: TEntity; const AFieldDef: TFieldDef);
begin
  inherited Create;
  FDomain := TEntity(AInstance).Domain;

  FInstance := AInstance;
  FFieldDef := AFieldDef;
  FUIState := vsUndefined; //AFieldDef.UIState;
  FCalcUIState := vsFullAccess;
  FValidationStatus := vsValid;
  FEnabled := not AFieldDef.HasFlag(cLazy);
end;

destructor TBaseField.Destroy;
begin
  FInstance := nil;
  FFieldDef := nil;

  inherited Destroy;
end;

function TBaseField.DoExtractFieldValue(const AFieldName: string): Variant;
begin
  Assert(False, 'There is no field value "' + AFieldName + '"');
end;

procedure TBaseField.SetFromJSON(const AJSONObject: TJSONObject; const AIsNew: Boolean);
var
  vJSONPair: TJSONPair;
begin
  vJSONPair := AJSONObject.Get(FieldName);
  if Assigned(vJSONPair) then
  begin
    if AIsNew then
      SetJSONValue(vJSONPair.JsonValue)
    else
      SetJSON(vJSONPair.JsonValue);
  end;
end;

procedure TBaseField.SetJSON(const AJSONValue: TJSONValue);
var
  vHolder: TChangeHolder;
  vParameter: TEntity;
begin
  vHolder := TDomain(FDomain).DomainHolder;

  BeforeChanges(vHolder);

  SetJSONValue(AJSONValue);

  AfterChanges(vHolder);

  if FFieldDef.Kind = fkObject then
    vParameter := TEntity(NativeInt(DoGetValue))
  else
    vParameter := nil;
  FInstance.ProcessFieldChanged(vHolder, dckFieldChanged, GetFieldName, vParameter);
end;

procedure TBaseField.SetJSONValue(const AJSONValue: TJSONValue);
begin
end;

procedure TBaseField.SetUIState(const Value: TViewState);
var
  vNewState: TViewState;
begin
  if FCalcUIState = Value then
    Exit;

  FCalcUIState := Value;

  if FUIState = vsUndefined then
    Exit;

  vNewState := GetMaxUIState;
  if FUIState <> vNewState then
  begin
    FUIState := vNewState;
    TEntity(FInstance).NotifyView(nil, dckViewStateChanged, nil, FieldName);
  end;
end;

function TBaseField.ExtractFieldValue(const AFieldName: string = ''): Variant;
begin
  Result := DoExtractFieldValue(AFieldName);
end;

function TBaseField.GetFieldKind: TFieldKind;
begin
  Result := FFieldDef.Kind;
end;

function TBaseField.GetFieldName: string;
begin
  Result := FFieldDef.Name;
end;

function TBaseField.GetStorageKind: TStorageKind;
begin
  Result := FFieldDef.StorageKind;
end;

function TBaseField.GetStorageName: string;
begin
  Result := FFieldDef.StorageName;
end;

function TBaseField.DoGetValue: Variant;
begin
  Result := Null;
end;

function TBaseField.GetJSON: TJSONValue;
begin
  Result := GetJSONValue;
end;

function TBaseField.GetJSONValue: TJSONValue;
begin
  Result := nil;
end;

function TBaseField.GetMaxUIState: TViewState;
begin
  if FFieldDef.HasFlag(cSystem) {or FInstance.Definition.HasFlag(ccSystem)} then
    Result := vsReadOnly
  else
    Result := vsFullAccess;

  Result := Result and FFieldDef.UIState and FCalcUIState;
end;

procedure TBaseField.DoSetValue(const AValue: Variant);
begin
end;

function TBaseField.InternalGetUIState: TViewState;
var
  vHandler: TProc;
begin
  if FUIState = vsUndefined then
  begin
   // FCalcUIState := vsFullAccess; // иначе запрошенное ранее состояние сбрасывается
    if Assigned(FFieldDef.FNotificationChains.UICalculations) then
    begin
      // Обновить FCalcUIState
      for vHandler in FFieldDef.FNotificationChains.UICalculations do
      begin
        TDomain(FDomain).LogEnter('>> UI reaction, ' + FFieldDef.FullName);
        try
          TReactionProcRef(vHandler)(TDomain(FDomain).DomainHolder, FieldName, FInstance, nil);
        finally
          TDomain(FDomain).LogExit('<< UI reaction, ' + FFieldDef.FullName);
        end;
      end;
    end;
    FUIState := GetMaxUIState;
  end;

  Result := FUIState;
end;

function TBaseField.IsValid: Boolean;
begin
  Result := not FFieldDef.HasFlag(cRequired);
  if Result then
    Exit;

  case FFieldDef.Kind of
    fkString..fkCurrency: Result := TSimpleFieldDef(FFieldDef).NullValue <> DoGetValue;
    fkObject: Result := Assigned(TEntityField(Self).Entity);
    fkList: Result := TListField(Self).Count > 0;
    fkBlob: Result := Assigned(TBlobField(Self).Stream);
    fkComplex: Result := Assigned(TComplexField(Self).ComplexObject);
  else
    Result := False;
  end;
end;

procedure TBaseField.Load(const AStorage: TStorage);
begin
  if FFieldDef.Hasflag(cNotSave) then
    Exit;
  DoLoad(AStorage);
end;

function TBaseField.LogInfo(const AOldValue: TJSONValue = nil): string;
var
  vLogInfo: string;
  vJSONValue: TJSONValue;
begin
  if FFieldDef.HasFlag(cNotSave) or (FFieldDef.Kind = fkList) then
    Result := ''
  else begin
    vJSONValue := GetJSON;
    try
      vLogInfo := JSONValueToString(FDomain, vJSONValue, FFieldDef.Kind);
    finally
      vJSONValue.Free;
    end;

    if Assigned(AOldValue) then
      vLogInfo := JSONValueToString(FDomain, AOldValue, FFieldDef.Kind) + ' => ' + vLogInfo;

    Result := '[' + FFieldDef._Caption + ']: ' + vLogInfo + #13#10;
  end;
end;

procedure TBaseField.SetValue(const AHolder: TObject; const AValue: Variant);
var
  vHolder: TChangeHolder absolute AHolder;
  vNeedRegisterChanges: Boolean;
  i: Integer;
  vDependentFieldDef: TObjectFieldDef;
  vParameter: TEntity;
begin
  if DoCheckValuedCondition(AValue, ckEqualTo, cmNone) then
    Exit;

  Assert(Assigned(AHolder), 'Пустой холдер в методе');

  BeforeChanges(AHolder);

  if FInstance.Definition is TActionDef then
  begin
    // Поля действий не должны регистрироваться в сессии
    DoSetValue(AValue);
  end
  else begin
    vNeedRegisterChanges := not FInstance.IsNew;
    if vNeedRegisterChanges then
      vNeedRegisterChanges := vHolder.KeepOldData(FInstance, GetFieldName);
    DoSetValue(AValue);
    if vNeedRegisterChanges then
      vHolder.RegisterFieldChanges(FInstance, GetFieldName);
  end;

  AfterChanges(AHolder);

  for i := 0 to FFieldDef.DependentFields.Count - 1 do
  begin
    // От мастер-поля поля могут зависеть, а могут и нет, зависит от коллекции (наследование)
    vDependentFieldDef := TObjectFieldDef(FFieldDef.DependentFields.Objects[i]);
    if FInstance.InstanceOf(vDependentFieldDef.Definition.Name) then
      FInstance.ProcessMasterFieldChanged(vHolder, vDependentFieldDef,
        FFieldDef.DependentFields.ValueFromIndex[i], FFieldDef.DependentFields.Names[i]);
  end;

  if not IsValid then
    FValidationStatus := vsRequiredIsNull
  else if TCheckFieldFunc(TDomain(FDomain).Configuration.CheckFieldFunc)(TEntity(FInstance), GetFieldName) then
    FValidationStatus := vsValid
  else
    FValidationStatus := vsInvalid;

  if FFieldDef.Kind = fkObject then
    vParameter := TEntity(NativeInt(Value))
  else
    vParameter := nil;

  FInstance.ProcessFieldChanged(AHolder, dckFieldChanged, GetFieldName, vParameter);
end;

procedure TBaseField.Transfer(const AStorage: TStorage);
begin
  if FFieldDef.HasFlag(cNotSave) then
    Exit;

  TDomain(FDomain).Logger.AddMessage('>> ' + FieldName + ': ' + cFieldKindNames[GetFieldKind]);
  DoTransfer(AStorage);
end;

function TBaseField.GetUIState(const ASession: TObject): TViewState;
var
  vSession: TUserSession absolute ASession;
begin
  Result := InternalGetUIState;
  if Assigned(vSession) then
    Result := Result and vSession.GetUIState(FFieldDef.FullName, FInstance.EntityState);
end;

{ TEntity }

procedure TEntity.ActivateField(const AHolder: TObject; const AFieldDef: TFieldDef);
var
  vHandler: TProc;
begin
  //TDomain(FDomain).LogEnter('>> Activate field [' + AFieldDef.Name + '], entity: ' + FDefinition.Name + ' / ' + IntToStr(FID));
  try
    if AFieldDef.HasFlag(cCalculated) and AFieldDef.HasFlag(cNotSave) and Assigned(AFieldDef.FNotificationChains.Calculations) then
      for vHandler in AFieldDef.FNotificationChains.Calculations do
        TReactionProcRef(vHandler)(TChangeHolder(AHolder), '', Self, nil);

    if Assigned(AFieldDef.FNotificationChains.UICalculations) then
      for vHandler in AFieldDef.FNotificationChains.UICalculations do
        TReactionProcRef(vHandler)(TChangeHolder(AHolder), '', Self, nil);
  finally
    //TDomain(FDomain).LogExit('<< Activate field, entity: ' + FDefinition.Name + ' / ' + IntToStr(FID));
  end;
end;

procedure TEntity.AddListener(const AFieldName: string; const AInstance: TEntity);
var
  vFieldNames: TStrings;
begin
  if not FListeners.TryGetValue(AInstance, vFieldNames) then
  begin
    vFieldNames := TStringList.Create;
    vFieldNames.Add(AFieldName);
    FListeners.Add(AInstance, vFieldNames);
  end
  else if vFieldNames.IndexOf(AFieldName) < 0 then
    vFieldNames.Add(AFieldName);
end;

procedure TEntity.AddUIListener(const AFieldName: string; const AView: TObject);
var
  vListeners: TList<TObject>;
begin
  if not FUIListeners.TryGetValue(AFieldName, vListeners) then
  begin
    vListeners := TList<TObject>.Create;
    FUIListeners.Add(AFieldName, vListeners);
  end;

  if not vListeners.Contains(AView) then
    vListeners.Add(AView);
end;

constructor TEntity.Create(const ADomain: TObject; const ADefinition: TDefinition);
begin
  inherited Create;
  FDomain := ADomain;
  FDefinition := ADefinition;
  FIsEnvSpecific := False;

  FListeners := TObjectDictionary<TEntity, TStrings>.Create([doOwnsValues]);
  FUIListeners := TObjectDictionary<string, TList<TObject>>.Create([doOwnsValues]);

  FFieldList := TObjectList<TBaseField>.Create;

  FIsEnvSpecific := FDefinition.HasFlag(ccLocalOnly);
  FCollection := TDomain(FDomain).CollectionByName(FDefinition.Name);

  // Заполнение полей и настройка подписок между ними
  CreateFields;

  FID := 0;
  FEnvironmentID := '';
  FIsNew := False;
  FIsRemote := False;
  FDeleted := False;
  FIsService := False;
  FLogID := 0;
  FViewState := vsFullAccess;
end;

function TEntity.CreateField(const AFieldDef: TFieldDef): TBaseField;
begin
  case AFieldDef.Kind of
    fkObject:
      Result := TEntityField.Create(Self, AFieldDef);
    fkBlob:
      Result := TBlobField.Create(Self, AFieldDef);
    fkComplex:
      Result := TComplexField.Create(Self, AFieldDef);
    fkList:
      Result := TListField.Create(Self, AFieldDef);
    fkString:
      Result := TStringField.Create(Self, AFieldDef);
    fkInteger, fkEnum, fkFlag:
      Result := TIntegerField.Create(Self, AFieldDef);
    fkFloat:
      Result := TFloatField.Create(Self, AFieldDef);
    fkDateTime:
      Result := TDateTimeField.Create(Self, AFieldDef);
    fkBoolean:
      Result := TBooleanField.Create(Self, AFieldDef);
    fkColor:
      Result := TColorField.Create(Self, AFieldDef);
    fkCurrency:
      Result := TCurrencyField.Create(Self, AFieldDef);
  else
    begin
      Result := nil;
      Assert(False, 'Field type is not supported!');
    end;
  end;

  FFieldList.Add(Result);
  FieldInitialize(Result);
end;

procedure TEntity.CreateFields;
var
  i: Integer;
begin
  for i := 0 to FDefinition.Fields.Count - 1do
    CreateField(FDefinition.Fields[i]);
end;

destructor TEntity.Destroy;
begin
  FreeAndNil(FUIListeners);
  FreeAndNil(FListeners);
  FreeAndNil(FFieldList);

  FCollection := nil;
  FDefinition := nil;
  FDomain := nil;

  inherited Destroy;
end;

procedure TEntity.EnableField(const AField: TBaseField);
begin
  AField.FEnabled := True;
  ActivateField(TDomain(FDomain).DomainHolder, AField.FieldDef);
end;

function TEntity.EntityState(const AFieldName: string = ''): TState;
var
  vFieldDef: TFieldDef;
  vStateMachine: TStateMachine;
  vStateMachineName: string;
begin
  Result := nil;

  if AFieldName <> '' then
    vFieldDef := FDefinition.FieldByName(AFieldName)
  else
    vFieldDef := FDefinition.StateFieldDef;

  if not Assigned(vFieldDef) or not (vFieldDef.Kind in [fkEnum]) then
    Exit;

  vStateMachineName := TSimpleFieldDef(vFieldDef).Dictionary;
  try
    Assert(vStateMachineName <> '', 'There is no state machine type name!');
    vStateMachine := TDomain(FDomain).Configuration.StateMachines.ObjectByName(vStateMachineName);
    Assert(Assigned(vStateMachine), 'There is no state machine [' + vStateMachineName + ']');
    Result := vStateMachine.StateByID(TIntegerField(FieldByName(vFieldDef.Name)).Value);
  except
    on E: Exception do
    begin
      TDomain(FDomain).Logger.AddMessage('Ошибка получения состояния: ' + E.Message);
      Result := nil;
    end;
  end;
end;

function TEntity.ExtractEntity(const AFieldName: string): TEntity;
var
  vField: TBaseField;
  vFieldPath: string;
  vFieldName: string;
  vEntityQuery: string;
begin
  vFieldPath := '';
  vField := InternalGetField(AFieldName);
  if not Assigned(vField) then
  begin
    vFieldPath := AFieldName;
    vFieldName := CutDataTillMarker(vFieldPath, '.');
    if vFieldPath = '' then
      raise Exception.Create(Format('Запрашиваемое поле [%s] отсутствует в объекте из коллекции [%s]',
        [AFieldName, FDefinition.Name]));

    vField := InternalGetField(vFieldName);
    if not Assigned(vField) then
      Exit(nil);
  end;

  Result := nil;
  if vField.FieldKind = fkObject then
    Result := TEntityField(vField).Entity
  else if vField.FieldKind = fkList then
  begin
    vEntityQuery := CutDataTillMarker(vFieldPath, '.');
    if vFieldPath <> '' then
      Result := TListField(vField).ExtractEntity(vEntityQuery);
  end;

  if Assigned(Result) and (vFieldPath <> '') then
    Result := Result.ExtractEntity(vFieldPath);
end;

function TEntity.ExtractFieldValue(const AFieldName: string; const AUseNameForObjects: Boolean = True): Variant;
var
  vPos: Integer;
  vField: TBaseField;
begin
  if {FIsNull or} FDeleted then
    Exit(Null);

  vPos := Pos('.', AFieldName);
  if vPos > 0 then
  begin
    vField := FieldByName(Copy(AFieldName, 1, vPos - 1));
    Result := vField.ExtractFieldValue(Copy(AFieldName, vPos + 1, Length(AFieldName) - vPos));
  end
  else begin
    //if AFieldName = 'IsNull' then
    //  Result := IsNull
    //else if AFieldName = 'ID' then
    //  Result := FID
    //else if AFieldName = 'IsNew' then
    //  Result := FIsNew
    //else if AFieldName = 'Deleted' then
    //  Result := FDeleted
    //else if AFieldName = 'FullText' then
    //  Result := AnsiUpperCase(FullText)
    //else if AFieldName = 'DisplayName' then
    //  Result := FDisplayName
    //else
    if AUseNameForObjects then
      Result := GetAnyFieldValue(AFieldName)
    else
      Result := GetFieldValue(AFieldName);
  end;
end;

function TEntity.FieldByName(const AFieldName: string): TBaseField;
var
  vFieldPath: string;
  vFieldName: string;
  vEntityQuery: string;
  vEntity: TEntity;
begin
  Result := InternalGetField(AFieldName);
  if Assigned(Result) then
    Exit;

  vFieldPath := AFieldName;
  vFieldName := CutDataTillMarker(vFieldPath, '.');
  if vFieldPath = '' then
    raise Exception.Create(Format('Запрашиваемое поле [%s] отсутствует в объекте из коллекции [%s]',
      [AFieldName, FDefinition.Name]));

  Result := InternalGetField(vFieldName);
  if Assigned(Result) then
  begin
    vEntity := nil;
    if Result.FieldKind = fkObject then
      vEntity := TEntityField(Result).Entity
    else if Result.FieldKind = fkList then
    begin
      vEntityQuery := CutDataTillMarker(vFieldPath, '.');
      if vFieldPath <> '' then
        vEntity := TListField(Result).ExtractEntity(vEntityQuery);
    end;

    if Assigned(vEntity) then
      Result := vEntity.FieldByName(vFieldPath)
    else
      Result := nil;
  end;
end;

function TEntity.FieldExists(const AFieldName: string): Boolean;
begin
  Result := FDefinition.FieldExists(AFieldName);
end;

procedure TEntity.FieldInitialize(const AField: TBaseField);
var
  vFieldDef: TFieldDef;
  vDefaultValue: Variant;
  vNow: TDateTime;
  vValue: TDateTime;
begin
  vFieldDef := AField.FieldDef;

  case vFieldDef.Kind of
    fkString..fkFloat, fkBoolean..fkCurrency:
      FieldInternalSetValue(AField, TSimpleFieldDef(vFieldDef).DefaultValue);
    fkDateTime:
    begin
      vDefaultValue := TSimpleFieldDef(vFieldDef).DefaultValue;
      if VarIsStr(vDefaultValue) then
      begin
        vNow := TDomain(FDomain).Now;
        // TODO : Возможно, следует сделать парсер для разбора строковых значений
        if vDefaultValue = cDateNow then
          vValue := Trunc(vNow)
          //FValue := Core.Now //чтобы сортировка по дате была правильней, todo: по хорошему надо чётко отделять дату и время
        else if vDefaultValue = cDateTimeNow then
          vValue := vNow
        else if vDefaultValue = cDateTomorrow then
          vValue := IncDay(Trunc(vNow), 1)
        else if vDefaultValue = cDatePlus1YearMinus1Day then
          vValue := IncDay(IncMonth(Trunc(vNow), 12), -1)
        else if vDefaultValue = cDatePlus1Week then
          vValue := IncDay(Trunc(vNow), 7)
        else if vDefaultValue = cDateMinus1Week then
          vValue := IncDay(Trunc(vNow), -7)
        else if vDefaultValue = cStartOfYear then
          vValue := StartOfTheYear(vNow)
        else
          vValue := cNullDateTime;

        FieldInternalSetValue(AField, vValue);
      end
      else
        FieldInternalSetValue(AField, vDefaultValue);
    end;
  end;
end;

procedure TEntity.FieldInternalSetValue(const AField: TBaseField; const AValue: Variant);
begin
  case AField.FieldKind of
    fkString: TStringField(AField).Value := StringFromVariant(AValue);
    fkInteger, fkEnum, fkFlag: TIntegerField(AField).Value := IntegerFromVariant(AValue);
    fkFloat: TFloatField(AField).Value := FloatFromVariant(AValue);
    fkDateTime: TDateTimeField(AField).Value := DateTimeFromVariant(AValue);
    fkBoolean: TBooleanField(AField).Value := BooleanFromVariant(AValue);
    fkColor: TColorField(AField).Value := ColorFromVariant(AValue);
    fkCurrency: TCurrencyField(AField).Value := CurrencyFromVariant(AValue);
    fkObject: TEntityField(AField).Value := AValue;
    fkBlob: TBlobField(AField).Value := AValue;
    fkComplex: TComplexField(AField).Value := AValue;
  end;
end;

function TEntity.FieldToString(const AFieldName: string): string;
var
  vIntValue: Integer;
  vEnumName: string;
  vEnum: TEnumeration;
  vItem: TEnumItem;
  vFieldDef: TFieldDef;
  vSize: Integer;
  vChildEntity: TEntity;
begin
  vFieldDef := FDefinition.FieldByName(AFieldName);

  case vFieldDef.Kind of
    fkString: Result := TStringField(FieldByName(AFieldName)).Value;
    fkInteger: Result := IntToStr(TIntegerField(FieldByName(AFieldName)).Value);
    fkEnum: begin
      vEnumName := TSimpleFieldDef(vFieldDef).Dictionary;
      Assert(vEnumName <> '', 'There is no enumeration type name!');
      vIntValue := TIntegerField(FieldByName(AFieldName)).Value;
      vEnum := _Platform.Enumerations.ObjectByName(vEnumName);
      if not Assigned(vEnum) then
        vEnum := TDomain(FDomain).Configuration.Enumerations.ObjectByName(vEnumName);
      if not Assigned(vEnum) then
        vEnum := TDomain(FDomain).Configuration.StateMachines.ObjectByName(vEnumName);
      Assert(Assigned(vEnum), 'There is no enumeration [' + vEnumName + ']');
      vItem := vEnum.Items[vIntValue];
      Result := TDomain(Domain).Translate(vItem.FullName, vItem.DisplayText);
    end;
    fkFlag: begin
      //vEnumName := TSimpleFieldDef(vFieldDef).Dictionary;
      //Assert(vEnumName <> '', 'There is no flag type name!');
      vIntValue := TIntegerField(FieldByName(AFieldName)).Value;
      Result := IntToStr(vIntValue);
    end;
    fkFloat: Result := FormatFloat('', TFloatField(FieldByName(AFieldName)).Value);
    fkDateTime: Result := FormatDateTime('dd.mm.yyyy', TDateTimeField(FieldByName(AFieldName)).Value);
    fkBoolean: Result := BoolToStr(TBooleanField(FieldByName(AFieldName)).Value);
    fkColor: Result := IntToHex(TColorField(FieldByName(AFieldName)).Value, 8);
    fkCurrency: Result := CurrToStr(TCurrencyField(FieldByName(AFieldName)).Value);
    fkBlob: begin
      if Assigned(TBlobField(FieldByName(AFieldName)).Stream) then
        vSize := TBlobField(FieldByName(AFieldName)).Stream.Size
      else
        vSize := 0;
      Result := 'size=' + IntToStr(vSize);
    end;
    fkComplex: Result := TComplexFieldDef(FDefinition.FieldByName(AFieldName)).ObjectKindName;
    fkObject: begin
      vChildEntity := ExtractEntity(AFieldName);
      if Assigned(vChildEntity) then
        Result := vChildEntity['Name']
      else
        Result := '';
    end
  else
    Result := '';
  end;
end;

function TEntity.GetField(const AIndex: Integer): TBaseField;
begin
  Result := FFieldList[AIndex];
end;

function TEntity.GetFieldBlob(const AFieldName: string): TStream;
begin
  Result := TBlobField(FieldByName(AFieldName)).Stream;
end;

function TEntity.GetFieldCount: Integer;
begin
  Result := FFieldList.Count;
end;

function TEntity.GetFieldEntity(const AFieldName: string): TEntity;
begin
  Result := TEntityField(FieldByName(AFieldName)).Entity;
end;

function TEntity.GetFieldList(const AFieldName: string): TList<TEntity>;
var
  vEntity: TEntity;
begin
  Result := TList<TEntity>.Create;
  for vEntity in TListField(FieldByName(AFieldName)) do
    if Assigned(vEntity) then
      Result.Add(vEntity);
end;

function TEntity.GetFieldObject(const AFieldName: string): TComplexObject;
begin
  Result := TComplexField(FieldByName(AFieldName)).ComplexObject;
end;

function TEntity.GetFieldValue(const AFieldName: string): Variant;
begin
  Result := FieldByName(AFieldName).Value;
end;

function TEntity.GetAnyFieldValue(const AFieldName: string): Variant;
var
  vField: TBaseField;
  vEntity: TEntity;
begin
  vField := FieldByName(AFieldName);
  if not vField.Enabled then
    vField.OwnerInstance.EnableField(vField);

  case vField.FieldKind of
    fkList: Result := '';
    fkObject: begin
        vEntity := TEntityField(vField).Entity;
        if Assigned(vEntity) then
          Result := vEntity['Name']
        else if Assigned(TEntityField(vField).ContentDefinition) then
          Result := TEntityField(vField).ContentDefinition._EmptyValue
        else
          Result := cNullItemName;
      end;
  else
    Result := TSimpleField(vField).Value;
  end;
end;

function TEntity.GetStateCaption(const AFieldName: string): string;
var
  vState: TState;
begin
  vState := EntityState(AFieldName);
  if Assigned(vState) then
    Result := vState.DisplayText
  else
    Result := 'не определено';
end;

function TEntity.GetUIListenerCount: string;
var
  vKey: string;
  vList: TList<TObject>;
begin
  Result := '';
  for vKey in FUIListeners.Keys do
  begin
    vList := FUIListeners[vKey];
    if vList.Count = 0 then
      Continue;

    if Result <> '' then
      Result := Result + ', ';
    Result := Result + '"' + vKey + '": ' + IntToStr(vList.Count);
  end;
end;

function TEntity.GetCaption(const AInteractor: TObject): string;
begin
  Result := TDomain(FDomain).TranslateDefinition(FDefinition);
end;

function TEntity.GetCollectionName: string;
begin
  Result := FDefinition.Name;
end;

procedure TEntity.ProcessFieldChanged(const AHolder: TObject; const AChangeKind: Word; const AFieldName: string;
  const AEntity: TEntity);
var
  i: Integer;
  vListener: TPair<TEntity, TStrings>;

  procedure UpdateViews(const ASubscribedEntity, AParameter: TEntity; const ASubscribedFieldName: string;
    const ANotificationKind: Word);
  begin
    if ASubscribedEntity.Deleted then
      Exit;

    // Уведомления от коллекций
    if Assigned(ASubscribedEntity.Collection) then
      TCollection(ASubscribedEntity.Collection).NotifyListeners(AHolder, dckEntityChanged, ASubscribedEntity);

    if ASubscribedEntity.Definition.FieldByName(ASubscribedFieldName).UIState = vsHidden then
      Exit;

    ASubscribedEntity.NotifyView(AHolder, ANotificationKind, AParameter, ASubscribedFieldName);
  end;
begin
  if (not TDomain(FDomain).IsAlive and (FDefinition.Name <> 'SysServices')) or FDeleted then
    Exit;

  //TDomain(FDomain).Log('%% Field changed: ' + AFieldName);

  UpdateViews(Self, AEntity, AFieldName, AChangeKind);

  if FListeners.Count > 0 then
  begin
    if AFieldName = 'Name' then
      for vListener in FListeners do
      begin
        for i := 0 to vListener.Value.Count - 1 do
          UpdateViews(vListener.Key, Self, vListener.Value[i], dckNameChanged)
      end;
    {else if Assigned(FDefinition.StateFieldDef) then
    begin
      if AFieldName = FDefinition.StateFieldDef.Name then
        for vListener in FListeners do
        begin
          for i := 0 to vListener.Value.Count - 1 do
            UpdateViews(vListener.Key, Self, vListener.Value[i], dckEntityChanged)
        end;
    end;}
    {else if AFieldName = 'Data' then
      for i := 0 to FListeners.Count - 1 do
        UpdateViews(FListeners[i].Entity, Self, FListeners[i].Name, dckEntityChanged);}
  end;

  ProcessLinkedEntityChanged(AHolder, AFieldName, '', AEntity);
end;

procedure TEntity.SubscribeFields(const AHolder: TObject);
var
  vFieldDef: TFieldDef;
  vField: TBaseField;
begin
  for vField in FFieldList do
  begin
    vFieldDef := vField.FieldDef;
    if vFieldDef.Kind = fkObject then
      vField.AfterChanges(AHolder);

    // TODO Performance: Сделать так, чтобы поля регистрировались для активации
    if not vFieldDef.HasFlag(cLazy) then
      ActivateField(AHolder, vFieldDef);
  end;
end;

procedure TEntity.ProcessLinkedEntityChanged(const AHolder: TObject;
  const AFieldName: string; const APrevChain: string; const AEntity: TEntity);
var
  vFieldDef: TFieldDef;
  vHandlers: THandlers;
  vHandler: TProc;
  i, j: Integer;
  vListener: TEntity;
  vTransitFields: TList<TFieldDef>;
  vTempListeners: TList<TEntity>;
  vFieldNames: TStrings;
  vPrevChain: string;
  vFieldName: string;
begin
  // Обработка рекуррентных действий над цепочками изменений
  //TDomain(FDomain).LogEnter('>> ProcessLinkedEntityChanged, entity: ' + FDefinition.Name + ' / ' + IntToStr(FID));
  //TDomain(FDomain).Log('%% Field: ' + AFieldName);
  try
    vFieldDef := FDefinition.FieldByName(AFieldName);
    vPrevChain := IfThen(APrevChain = '', AFieldName, APrevChain + '.' + AFieldName);

    // 1. Калькуляции
    if vFieldDef.FNotificationChains.TryGetReactions(APrevChain, FDefinition.Name, vHandlers) then
    begin
      for vHandler in vHandlers do
      begin
        //TDomain(FDomain).LogEnter('>> Base calculation, ' + FDefinition.Name + ':' + AFieldName + ':' + APrevChain);
        try
          TReactionProcRef(vHandler)(TChangeHolder(AHolder), vPrevChain, Self, AEntity);
        finally
          //TDomain(FDomain).LogExit('<< Base calculation, ' + FDefinition.Name + ':' + AFieldName + ':' + APrevChain);
        end;
      end;
    end;

    // 2. Передача по цепочке вычислений, без действий
    if (FListeners.Count > 0) and vFieldDef.FNotificationChains.TryGetTransitFields(APrevChain, vTransitFields)
      and Assigned(vTransitFields) then
    begin
      //TDomain(FDomain).Log('%% Transition');
      vTempListeners := TList<TEntity>.Create;
      try
        for vListener in FListeners.Keys do
          vTempListeners.Add(vListener);

        for i := vTempListeners.Count - 1 downto 0 do
        begin
          vListener := vTempListeners[i];
          if not FListeners.TryGetValue(vListener, vFieldNames) then
            Continue;

          if not vListener.Deleted then
          begin
            for j := 0 to vFieldNames.Count - 1 do
            begin
              vFieldName := vFieldNames[j];
              if vTransitFields.Contains(vListener.Definition.FieldByName(vFieldName)) then
                vListener.ProcessLinkedEntityChanged(AHolder, vFieldName, vPrevChain, Self);

              // Required to check, since this action can remove vListener from FListeners
              if not FListeners.ContainsKey(vListener) then
                Break;
            end;
          end;
        end;
      finally
        FreeAndNil(vTempListeners);
      end;
    end;
  finally
    //TDomain(FDomain).LogExit('<< ProcessLinkedEntityChanged, entity: ' + FDefinition.Name + ' / ' + IntToStr(FID));
  end;
end;

procedure TEntity.ProcessLinkedEntityDeleted(const AHolder: TObject; const AFieldName: string; const AEntity: TEntity);
var
  vField: TBaseField;
begin
  vField := FieldByName(AFieldName);
  case vField.FieldKind of
    fkObject:
      begin
        TDomain(FDomain).Logger.AddMessage('DELETE notification ' + AFieldName);
        _SetFieldEntity(AHolder, AFieldName, nil);
      end;
    fkList: TListField(vField).UnlinkListEntity(AHolder, AEntity);
  end;
end;

procedure TEntity.ProcessMasterFieldChanged(const AHolder: TObject; const ADependentFieldDef: TFieldDef;
  const AMasterFieldName, ALinkFieldName: string);
var
  vDependentEntity: TEntity;
  vDependentField: TBaseField;
  vMasterField: TBaseField;
  vMasterEntity: TEntity;
  vPos: Integer;
  vLinkFieldName: string;
  vFieldName: string;
  vField: TBaseField;
  i: Integer;
  vEntity: TEntity;
begin
  case ADependentFieldDef.Kind of
    fkObject: begin
      vDependentField := FieldByName(ADependentFieldDef.Name);
      vDependentEntity := TEntityField(vDependentField).Entity;
      if not Assigned(vDependentEntity) then
      begin
        NotifyView(AHolder, dckViewStateChanged, Self, ADependentFieldDef.Name);
        Exit;
      end;

      vMasterField := TEntityField(FieldByName(AMasterFieldName));
      if Assigned(vMasterField) and (vMasterField.FieldKind = fkObject) then
        vMasterEntity := TEntityField(vMasterField).Entity
      else
        vMasterEntity := nil;

      vLinkFieldName := ALinkFieldName;
      vPos := Pos('.', vLinkFieldName);
      while (vPos > 0) and Assigned(vDependentEntity) do
      begin
        vFieldName := Copy(vLinkFieldName, 1, vPos - 1);
        System.Delete(vLinkFieldName, 1, vPos);
        vField := vDependentEntity.FieldByName(vFieldName);
        if vField.FieldKind = fkObject then
        begin
          vDependentEntity := vDependentEntity.ExtractEntity(vFieldName);
          vPos := Pos('.', vLinkFieldName)
        end
        else if vField.FieldKind = fkList then
        begin
          for i := 0 to TListField(vField).Count - 1 do
          begin
            vEntity := TEntity(TListField(vField)[i]);
            vDependentField := vEntity.FieldByName(vLinkFieldName);
            if Assigned(vDependentField) and (vDependentField.FieldKind = fkObject) then
            begin
              vDependentEntity := TEntity(TEntityField(vDependentField).Entity);
              if Assigned(vDependentEntity) and (vDependentEntity = vMasterEntity) then
                Exit;
            end;
          end;
          _SetFieldEntity(AHolder, ADependentFieldDef.Name, nil);
          Exit;
        end;
      end;

      //TODO сделать возможными проверки Users=User?
      if not Assigned(vDependentEntity) then
        _SetFieldEntity(AHolder, ADependentFieldDef.Name, nil)
      else if vDependentEntity.ExtractEntity(vLinkFieldName) <> vMasterEntity then
        _SetFieldEntity(AHolder, ADependentFieldDef.Name, nil);
    end;
    fkList: begin
      // for i := 0 to FList.Count - 1 do
      //   TDomain(FDomain).Unsubscribe(TEntity(FList[i]).Link, Self.Link, cListSubscriptions);
      // FList.Clear;
      // Assert(False, 'List population does not implemented yet');
    end;
  end;
end;

procedure TEntity.RemoveAllListeners;
begin
  FListeners.Clear;
end;

procedure TEntity.RemoveListener(const AFieldName: string; const AInstance: TEntity);
var
  vFieldNames: TStrings;
  vIndex: Integer;
begin
  if FDeleted or (not FListeners.TryGetValue(AInstance, vFieldNames)) then
    Exit;

  vIndex := vFieldNames.IndexOf(AFieldName);
  if vIndex < 0 then
    Exit;

  vFieldNames.Delete(vIndex);
  if vFieldNames.Count = 0 then
    FListeners.Remove(AInstance);
end;

procedure TEntity.RemoveUIListener(const AFieldName: string; const AView: TObject);
var
  vList: TList<TObject>;
begin
  if Assigned(FUIListeners) and FUIListeners.TryGetValue(AFieldName, vList) then
    if vList.Remove(AView) >= 0 then
      if (vList.Count = 0) and (AFieldName <> '') then
        FieldByName(AFieldName).FUIState := vsUndefined;
end;

procedure TEntity.ResetToDefault(const AHolder: TObject);
var
  i: Integer;
begin
  Assert(Assigned(AHolder), 'Пустой холдер в методе');
  for i := 0 to FFieldList.Count - 1 do
    if GetField(i).FieldKind = fkObject then
      TEntityField(GetField(i)).ResetToDefault(AHolder)
    else
      FieldInitialize(GetField(i));
end;

procedure TEntity._SetFieldEntity(const AHolder: TObject; const AFieldName: string; const AEntity: TEntity);
var
  vFieldDef: TFieldDef;
begin
  Assert(Assigned(AHolder), 'Пустой холдер в методе');
  vFieldDef := FDefinition.FieldByName(AFieldName);
  if Assigned(vFieldDef) and (vFieldDef.Kind = fkObject) then
    TEntityField(FieldByName(AFieldName)).SetEntity(AHolder, AEntity);
end;

procedure TEntity._SetFieldObject(const AHolder: TObject; const AFieldName: string; const AObject: TComplexObject);
var
  vFieldDef: TFieldDef;
begin
  Assert(Assigned(AHolder), 'Пустой холдер в методе');
  vFieldDef := FDefinition.FieldByName(AFieldName);
  if Assigned(vFieldDef) and (vFieldDef.Kind = fkComplex) then
    TComplexField(FieldByName(AFieldName)).SetObject(AHolder, AObject);
end;

procedure TEntity._SetFieldStream(const AHolder: TObject; const AFieldName: string; const AStream: TStream);
var
  vFieldDef: TFieldDef;
begin
  Assert(Assigned(AHolder), 'Пустой холдер в методе');
  vFieldDef := FDefinition.FieldByName(AFieldName);
  if Assigned(vFieldDef) and (vFieldDef.Kind = fkBlob) then
    TBlobField(FieldByName(AFieldName)).SetStream(AHolder, AStream);
end;

procedure TEntity._SetFieldValue(const AHolder: TObject; const AFieldName: string; const AValue: Variant);
var
  vFieldDef: TFieldDef;
begin
  Assert(Assigned(AHolder), 'Пустой холдер в методе');
  vFieldDef := FDefinition.FieldByName(AFieldName);
  Assert(Assigned(vFieldDef), Format('Поле [%s] отсутствует в коллекции [%s]', [AFieldName, FDefinition.Name]));
  if Assigned(vFieldDef) and (vFieldDef.Kind in [fkString..fkCurrency]) then
    FieldByName(AFieldName).SetValue(AHolder, AValue);
end;

function TEntity._SetState(const AHolder: TObject; const ANewState: Variant): Boolean;
var
  vState: TState;
begin
  Assert(Assigned(AHolder), 'Пустой холдер в методе');

  Result := False;
  vState := EntityState;
  if not Assigned(vState) then
    Exit;

  Result := vState.CanTransitToState(ANewState);
  if not Result then
  begin
    TDomain(FDomain).Logger.AddMessage(FDefinition.Name + ': попытка перейти из состояния ' + vState.FullName + ' в ' + IntToStr(ANewState));
    Exit;
  end;

  // Уточнить, есть ли доп. ограничения в конфигурации. Если есть, выйти

  // Составить разницу между состояниями
  // Уведомить измененные объекты

  FieldByName(FDefinition.StateFieldDef.Name).SetValue(AHolder, Integer(ANewState));
end;

procedure TEntity.TryAddToForeignList(const AHolder: TObject; const AField: TBaseField);
var
  vListFieldDefs: TList<TListFieldDef>;
  vListFieldDef: TListFieldDef;
  vOwnerInstance: TEntity;
  vRefListField: TListField;
begin
  vListFieldDefs := FDefinition.GetLinkedOwnerLists(AField.FieldDef);
  if not Assigned(vListFieldDefs) then
    Exit;

  for vListFieldDef in vListFieldDefs do
  begin
    // Ищем инстанс листового поля через мастер-поле, определенное в этой сущности
    vOwnerInstance := ExtractEntity(vListFieldDef.MasterFieldName);
    if not Assigned(vOwnerInstance) then
      Continue;

    vRefListField := TListField(vOwnerInstance.FieldByName(vListFieldDef.Name));
    if vListFieldDef.MasterFieldName = AField.FieldName then
    begin
      if vRefListField.MatchToFilter(AHolder, Self) then
        vRefListField.AddToList(AHolder, Self);
    end
    else
      vRefListField.UpdateListOnFilterChanged(AHolder, Self);
  end;
end;

procedure TEntity.TryRemoveFromForeignList(const AHolder: TObject; const AField: TBaseField);
var
  vListFieldDefs: TList<TListFieldDef>;
  vListFieldDef: TListFieldDef;
  vOwnerInstance: TEntity;
  vRefListField: TListField;
begin
  vListFieldDefs := FDefinition.GetLinkedOwnerLists(AField.FieldDef);
  if not Assigned(vListFieldDefs) then
    Exit;

  for vListFieldDef in vListFieldDefs do
  begin
    if vListFieldDef.MasterFieldName <> AField.FieldName then
      Continue;

    vOwnerInstance := ExtractEntity(vListFieldDef.MasterFieldName);
    if Assigned(vOwnerInstance) then
    begin
      vRefListField := TListField(vOwnerInstance.FieldByName(vListFieldDef.Name));
      vRefListField.DeleteFromList(AHolder, Self);
    end;
  end;
end;

procedure TEntity.TryUpdateForeignList(const AHolder: TObject; const AFilterField: TBaseField);
var
  vListFieldDefs: TList<TListFieldDef>;
  vListFieldDef: TListFieldDef;
  vOwnerInstance: TEntity;
  vRefListField: TListField;
begin
  vListFieldDefs := FDefinition.GetLinkedOwnerLists(AFilterField.FieldDef);
  if not Assigned(vListFieldDefs) then
    Exit;

  for vListFieldDef in vListFieldDefs do
  begin
    // Ищем инстанс листового поля через мастер-поле, определенное в этой сущности
    vOwnerInstance := ExtractEntity(vListFieldDef.MasterFieldName);
    if Assigned(vOwnerInstance) then
    begin
      vRefListField := TListField(vOwnerInstance.FieldByName(vListFieldDef.Name));
      // в найденном листовом поле актуализируем привязку, применив фильтр
      vRefListField.UpdateListOnFilterChanged(AHolder, Self);
    end;
  end;
end;

procedure TEntity.UnsubscribeFields(const AHolder: TObject);
var
  vField: TBaseField;
begin
  for vField in FFieldList do
    if vField.FieldDef.Kind = fkObject then
      vField.BeforeChanges(AHolder);
end;

procedure TEntity.UpdateFields;
var
  vFields: TList<TBaseField>;
  vFieldDef: TFieldDef;
  vField: TBaseField;
begin
  vFields := TList<TBaseField>.Create;
  for vField in FFieldList do
    vFields.Add(vField);

  for vFieldDef in FDefinition.Fields do
  begin
    vField := FieldByName(vFieldDef.Name);
    if not Assigned(vField) then
      CreateField(vFieldDef)
    else
      vFields.Remove(vField);
  end;

  for vField in vFields do
    FFieldList.Remove(vField);

  FreeAndNil(vFields);
end;

function TEntity.VisibleFieldCount(const ASession: TObject): Integer;
var
  i: Integer;
  vField: TBaseField;
begin
  Result := 0;
  for i := 0 to FFieldList.Count - 1 do
  begin
    vField := GetField(i);
    if (vField.GetUIState(ASession) > vsHidden) and not vField.FieldDef.HasFlag(cHideInEdit) then
      Inc(Result);
  end;
end;

function TEntity.InstanceOf(const ATypeName: string): Boolean;
begin
  Result := FDefinition.IsDescendantOf(ATypeName);
end;

function TEntity.IsParam: Boolean;
begin
  Result := FDefinition.IsItParameter;
end;

function TEntity.IsValid: Boolean;
var
  i: Integer;
  vField: TBaseField;
begin
  Result := False;
  for i := 0 to FFieldList.Count - 1 do
  begin
    vField := GetField(i);
    if not vField.IsValid then
      Exit;
  end;
  Result := True;
end;

procedure TEntity.NotifyView(const AHolder: TObject; const AChangeKind: Word; const AParameter: TObject; const AFieldName: string = '');
var
  i: Integer;
  vListeners: TList<TObject>;
  vMessage: TDomainChangedMessage;
begin
  if not FUIListeners.TryGetValue(AFieldName, vListeners) then
    Exit;

  vMessage.Msg := DM_DOMAIN_CHANGED;
  vMessage.Kind := AChangeKind;
  vMessage.Sender := Self;
  vMessage.Parameter := AParameter;
  vMessage.Holder := AHolder;

  for i := vListeners.Count - 1 downto 0 do
    vListeners[i].Dispatch(vMessage);
end;

function TEntity.Clone(const AHolder: TObject; const AIgnoreFields: string): TEntity;
begin
  Result := TCollection(FCollection)._CreateNewEntity(AHolder, cNewID, '', [], nil, False);

  // Возможны разные побочные эффекты..
  // Нужно переходить на отложенную калькуляцию
  FillEntity(AHolder, Result, AIgnoreFields);
  // Core.ApplyCalculations(Result);

  TEntityCreationProc(TDomain(FDomain).Configuration.AfterEntityCreationProc)(TChangeHolder(AHolder), nil, Result);
end;

procedure TEntity.Delete(const AHolder: TObject);
begin
  TCollection(FCollection).MarkEntityAsDeleted(AHolder, Self);
end;

function TEntity.DisplayName: string;
begin
  Result := GetAnyFieldValue('Name');
end;

procedure TEntity.LoadFields(const AStorage: TStorage);
var
  i: Integer;
begin
  FIsNew := False;

  { BOOKMARK : Обработка загрузки сервисных полей }
  if FIsEnvSpecific then
  begin
    FEnvironmentID := VarToStrDef(AStorage.ReadValue('guid', fkString), '');
    FIsRemote := not SameText(FEnvironmentID, _Platform.EnvironmentID);
  end;

  for i := 0 to FFieldList.Count - 1 do
    GetField(i).Load(AStorage);
end;

procedure TEntity.GenerateID;
begin
  TCollection(FCollection).GenerateEntityID(Self);
  if FIsEnvSpecific then
    FEnvironmentID := _Platform.EnvironmentID;
end;

function TEntity._FieldText(const AFieldName: string): string;
var
  vFieldDef: TFieldDef;
  vFieldKind: TFieldKind;
  vDate: TDateTime;
  vChildEnt: TEntity;
begin
  Result := '';

  if not FDefinition.FieldExists(AFieldName) then
  begin
    Result := GetAnyFieldValue('Name');
    Exit;
  end;

  vFieldDef := FDefinition.FieldByName(AFieldName);
  vFieldKind := vFieldDef.Kind;

  if vFieldKind = fkCurrency then
    Result := FormatFloat('#,##0.00;;0', GetAnyFieldValue(AFieldName))
  else if vFieldKind = fkDateTime then
  begin
    vDate := GetAnyFieldValue(AFieldName);
    if vDate < 2 then  // типа определил, что дата не задана :)
      Result := ' -'
    else if Pos(vFieldDef.StyleName, 'datetime') > 0 then
      Result := FormatDateAndTime(vDate)
    else
      Result := FormatDate(vDate);
  end
  else if vFieldKind = fkBoolean then
  begin
  end
  else if vFieldKind = fkObject then
  begin
    vChildEnt := ExtractEntity(AFieldName);
    if Assigned(vChildEnt) then
      Result := vChildEnt['Name']
    else
      Result := '';
  end
  else
    Result := GetAnyFieldValue(AFieldName);
end;

procedure TEntity._RestoreFieldValue(const AHolder: TObject; const AFieldName: string);
begin
  Assert(Assigned(AHolder), 'Пустой холдер в методе');
  TChangeHolder(AHolder).RevertField(Self, AFieldName);
end;

procedure TEntity.FillEntity(const AHolder: TObject; const ATargetEntity: TEntity;
  const AIgnoreFields: string);
var
  j: Integer;
  vIgnored: TStrings;
  vFieldDef: TFieldDef;
  vFieldName: string;
  vStream: TMemoryStream;
  vOldStream: TStream;
  vObject: TComplexObject;
  vList: TList<TEntity>;
  vTargetListEntity: TEntity;
begin
  vIgnored := CreateDelimitedList(AIgnoreFields);
  try
    for vFieldDef in FDefinition.Fields do
    begin
      vFieldName := vFieldDef.Name;
      if vIgnored.IndexOf(vFieldName) >= 0 then
        Continue;
      if vFieldDef.Kind in [fkString..fkCurrency] then
        ATargetEntity._SetFieldValue(AHolder, vFieldName, GetFieldValue(vFieldName))
      else if vFieldDef.Kind = fkObject then
        ATargetEntity._SetFieldEntity(AHolder, vFieldName, GetFieldEntity(vFieldName))
      else if vFieldDef.Kind = fkBlob then
      begin
        vOldStream := GetFieldBlob(vFieldName);
        if Assigned(vOldStream) then
        begin
          vStream := TMemoryStream.Create;
          vStream.LoadFromStream(vOldStream);
        end
        else
          vStream := nil;
        ATargetEntity._SetFieldStream(AHolder, vFieldName, vStream);
      end
      else if vFieldDef.Kind = fkComplex then
      begin
        vObject := TComplexField(FieldByName(vFieldName)).CloneComplexObject;
        ATargetEntity._SetFieldObject(AHolder, vFieldName, vObject);
      end
      else if vFieldDef.Kind = fkList then
      begin
        if TListFieldDef(vFieldDef).RelationPower = rpStrong then
        begin
          vList := GetFieldList(vFieldName);
          try
            for j := 0 to vList.Count - 1 do
            begin
              vTargetListEntity := TEntity(vList[j]).Clone(AHolder, TListFieldDef(vFieldDef).MasterFieldName);
              TListField(ATargetEntity.FieldByName(vFieldName)).LinkListEntity(AHolder, vTargetListEntity);
            end;
          finally
            FreeAndNil(vList);
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(vIgnored);
  end;
end;

procedure TEntity.FillFromJSON(const AJSONObject: TJSONObject; const AIsNew: Boolean);
var
  i: Integer;
begin
  Assert(Assigned(AJSONObject), 'Attempt to load from empty JSON object');

  if FIsEnvSpecific then
  begin
    FEnvironmentID := AJSONObject.ExtractString('guid');
    FIsRemote := not SameText(FEnvironmentID, _Platform.EnvironmentID);
  end;

  for i := 0 to FFieldList.Count - 1 do
    GetField(i).SetFromJSON(AJSONObject, AIsNew);
end;

procedure TEntity.SetDeleted(const AHolder: TObject);
var
  i, j: Integer;
  vField: TBaseField;
  vEntity: TEntity;
  vTempListeners: TList<TEntity>;
  vFieldNames: TStrings;
  vListener: TEntity;
begin
  if FDeleted then
    Exit;

  Assert(Assigned(AHolder), 'Пустой холдер в методе');

  FDeleted := True;
  TChangeHolder(AHolder).RegisterEntityDeleting(Self);

  vTempListeners := TList<TEntity>.Create;
  try
    for vListener in FListeners.Keys do
      vTempListeners.Add(vListener);

    for i := vTempListeners.Count - 1 downto 0 do
    begin
      vListener := vTempListeners[i];
      if not FListeners.TryGetValue(vListener, vFieldNames) then
        Continue;

      for j := 0 to vFieldNames.Count - 1 do
      begin
        vListener.ProcessLinkedEntityDeleted(AHolder, vFieldNames[j], Self);
        // Required to check, since this action can remove vListener from FListeners
        if not FListeners.ContainsKey(vListener) then
          Break;
      end;
    end;
  finally
    FreeAndNil(vTempListeners);
  end;

  NotifyView(AHolder, dckEntityDeleted, Self);

  for i := 0 to FFieldList.Count - 1 do
  begin
    vField := GetField(i);
    if vField.FieldKind <> fkObject then
      Continue;

    vEntity := TEntity(TEntityField(vField).Entity);
    if Assigned(vEntity) then
      vEntity.RemoveListener(vField.FieldName, Self);
  end;

  //TDomain(Domain).Logger.AddMessage(Format('Entity is marked for deletion: %s[%d]', [FDefinition.Name, FID]));
end;

procedure TEntity.SetEnvironmentID(const Value: string);
begin
  if FEnvironmentID = Value then
    Exit;

  FEnvironmentID := Value;
  FIsRemote := not SameText(FEnvironmentID, _Platform.EnvironmentID);
end;

procedure TEntity.SetID(const AID: Integer);
begin
  FID := AID;
end;

procedure TEntity.SetViewState(const Value: TViewState);
begin
  if FViewState = Value then
    Exit;
  FViewState := Value;
  NotifyView(nil, dckViewStateChanged, Self);
end;

function TEntity.FullText: string;
begin
  Result := TFullTextFunc(TConfiguration(FDefinition.Configuration).FullTextFunc)(Self);
end;

procedure TEntity.ExportToJSON(const AJSONObject: TJSONObject);
var
  i: Integer;
  vField: TBaseField;
  vValue: TJSONValue;
begin
  AJSONObject.AddPair('id', TJSONNumber.Create(FID));
  if FIsEnvSpecific then
    AJSONObject.AddPair('guid', TJSONString.Create(FEnvironmentID));
  for i := 0 to FieldCount - 1 do
  begin
    vField := GetField(i);
    vValue := vField.JSON;
    if Assigned(vValue) then
      AJSONObject.AddPair(TJSONPair.Create(vField.FieldName, vValue));
  end;
end;

function TEntity.InternalGetField(const AFieldName: string): TBaseField;
var
  vFieldIndex: Integer;
begin
  vFieldIndex := FDefinition.IndexOfField(AFieldName);
  if vFieldIndex >= 0 then
    Result := GetField(vFieldIndex)
  else
    Result := nil;
end;

procedure TEntity.InternalLoad(const AStorage: TStorage);
begin
  AStorage.ReadItem(LoadFields);
end;

function TEntity.FindSimilar(const ASession: TObject): TEntity;
begin
  if Assigned(FCollection) then
    Result := TCollection(FCollection).FindSimilarEntity(ASession, Self)
  else
    Result := nil;
end;

type
  TCrackedField = class(TBaseField);

procedure TEntity.Populate(const AFieldNames: string; const AValues: array of Variant;
  const APopulateAll: Boolean = True);
var
  i: Integer;
  vFieldNames: TStrings;
  vFieldName: string;
  vIsImportant: Boolean;
// Процедура заполняет поля предустановленными значениями
//   Важно, что поля принимают значение сразу, без регистрации изменений
begin
  if Length(AValues) = 0 then
    Exit;

  vFieldNames := CreateDelimitedList(AFieldNames);
  try
    for i := 0 to Min(vFieldNames.Count - 1, High(AValues)) do
    begin
      vFieldName := vFieldNames[i];
      vIsImportant := (Length(vFieldName) > 0) and (vFieldName.Chars[0] = '!');
      if vIsImportant then
        vFieldName := vFieldName.Remove(0, 1);

      if not FDefinition.FieldExists(vFieldName) then
        raise Exception.Create(Format('Поле [%s] не существует в коллекции [%s]', [vFieldName, FDefinition.Name]));

      if APopulateAll or vIsImportant or FDefinition.FieldByName(vFieldName).HasFlag(cNotSave) then
        FieldInternalSetValue(FieldByName(vFieldName), AValues[i]);
    end;
  finally
    FreeAndNil(vFieldNames);
  end;
end;

function TEntity.ToString: string;
begin
  Result := '(' + FDefinition.Name + ') ' + IntToStr(FID) + ': ' + GetAnyFieldValue('Name') + ' [' +
    IntToHex(NativeInt(Self), 8) + ']';
end;

end.


