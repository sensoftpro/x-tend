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

unit uObjectField;

interface

uses
  Classes, Generics.Collections, UITypes, uEntity, uConsts, uDefinition, uJSON, uStorage;

type
  TObjectField = class(TBaseField)
  private
    // Корневое определение для иерархий или обычное для неиерархических коллекций
    FContentDefinition: TDefinition;
    // Получение метаинформации о поле
    function GetViewName: string;
    function GetSortType: TEntitySortType;
    function GetIsSelector: Boolean;
    function GetSearchType: TSearchType;
    function GetContentDefinition: TDefinition;
    procedure SetContentDefinition(const Value: TDefinition);
    function GetContentDefinitions: TList<TDefinition>;
    function GetDefaultDefinitionName: string;
  public
    constructor Create(const AInstance: TEntity; const AFieldDef: TFieldDef); override;
    destructor Destroy; override;

    property IsSelector: Boolean read GetIsSelector;
    property ContentDefinition: TDefinition read GetContentDefinition write SetContentDefinition;
    property DefaultDefinitionName: string read GetDefaultDefinitionName;
    property ContentDefinitions: TList<TDefinition> read GetContentDefinitions;
    property SortType: TEntitySortType read GetSortType;
    property SearchType: TSearchType read GetSearchType;
    property ViewName: string read GetViewName;
  end;

  TEntityField = class(TObjectField)
  private
    FCollectionID: Integer;
    FEntityID: Integer; // temporary, only for loading
    FEntity: TEntity;
    FLoaded: Boolean;

    function GetContentCollectionName: string;
    function GetEntityID: Integer;
    function GetEntity: TEntity;
    function RestoreEntity(const ACollectionID, AEntityID: Integer): TEntity;

    function GetSelectorStorageName: string;
  protected
    procedure BeforeChanges(const AHolder: TObject); override;
    procedure AfterChanges(const AHolder: TObject); override;
    function DoGetValue: Variant; override;
    procedure DoSetValue(const AValue: Variant); override;
    function GetJSONValue: TJSONValue; override;
    procedure SetJSONValue(const AJSONValue: TJSONValue); override;
    procedure DoTransfer(const AStorage: TStorage); override;
    procedure DoLoad(const AStorage: TStorage); override;
    function DoExtractFieldValue(const AFieldName: string): Variant; override;
    function DoCompare(const AValue: Variant): Integer; override;
    function DoCheckValuedCondition(const AValue: Variant; const ACondition:
      TConditionKind; const AModifier: TConditionModifier): Boolean; override;
  public
    constructor Create(const AInstance: TEntity; const AFieldDef: TFieldDef); override;

    // Восстанавливает сущность до ее первоначального состояния
    procedure ResetToDefault(const AHolder: TObject);
    // Отдает UI список всех имеющихся сущностей, возможных для этого поля
    procedure GetAllEntitiesForSelect(const ASession: TObject; const AList: TObject);
    // Отдает UI список сущностей, которые можно выбрать в это поле
    procedure GetEntitiesForSelect(const ASession: TObject; const AList: TObject);

    // Рабочая информация о хранимой в поле сущности
    //property CollectionID: Integer read FCollectionID;
    property EntityID: Integer read GetEntityID;

    procedure SetIdentifiers(const AEntityID: Integer; const ACollectionID: Integer = 0);

    property ContentCollectionName: string read GetContentCollectionName;
    procedure SetEntity(const AHolder: TObject; const AEntity: TEntity);
    property Entity: TEntity read GetEntity;
  end;

  TListField = class(TObjectField)
  private
    // Список хранит живые сущности
    FList: TList<TEntity>;
    FDict: TDictionary<TEntity, string>;

    function GetMasterFieldName: string;

    procedure InternalRemove(const AHolder: TObject; const AEntity: TEntity);
    function GetEntity(const AIndex: Integer): TEntity;
    function GetCount: Integer;
    function GetRelationPower: TRelationPower;
  protected
    function DoGetValue: Variant; override;
    procedure DoSetValue(const AValue: Variant); override;
    procedure DoLoad(const AStorage: TStorage); override;
    procedure DoTransfer(const AStorage: TStorage); override;
    function DoExtractFieldValue(const AFieldName: string): Variant; override;
    function DoCompare(const AValue: Variant): Integer; override;
    function DoCheckValuedCondition(const AValue: Variant; const ACondition:
      TConditionKind; const AModifier: TConditionModifier): Boolean; override;
  public
    constructor Create(const AInstance: TEntity; const AFieldDef: TFieldDef); override;
    destructor Destroy; override;

    // Управление списком сущностей на уровне GUI
    function AddListEntity(const AHolder: TObject; const ACollectionName: string;
      const AFieldNames: string; const AValues: array of Variant): TEntity;
    procedure LinkListEntity(const AHolder: TObject; const AEntity: TEntity);
    procedure UnlinkListEntity(const AHolder: TObject; const AEntity: TEntity);
    procedure RemoveListEntity(const AHolder: TObject; const AEntity: TEntity);

    // Управление списком сущностей на уровне ядра (TEntity, TEntityField)
    procedure AddToList(const AHolder: TObject; const AEntity: TEntity);
    procedure DeleteFromList(const AHolder: TObject; const AEntity: TEntity);
    procedure ClearList(const AHolder: TObject);

    procedure GetAllEntitiesForSelect(const ASession: TObject; const AList: TObject);
    procedure GetEntitiesForSelect(const ASession: TObject; const AList: TObject);
    procedure GetEntityList(const ASession: TObject; const AEntityList: TObject);
    procedure GetList(const ASession: TObject; const AList: TList<TEntity>);
    function GetEntityColor(const AEntity: TEntity): TColor;
    function Contains(const AEntity: TEntity): Boolean;
    function ExtractEntity(const AEntityQuery: string): TEntity;
    function ExtractField(const AFieldName: string): TBaseField;

    function FindOne(const ASession: TObject; const AQuery: string; const AParams: array of Variant): TEntity;

    function GetEnumerator: TEnumerator<TEntity>;

    property Entities[const AIndex: Integer]: TEntity read GetEntity; default;
    property Count: Integer read GetCount;
    property MasterFieldName: string read GetMasterFieldName;
    property RelationPower: TRelationPower read GetRelationPower;
  end;

implementation

uses
  SysUtils, Variants, Math, uDomain, uCollection, uDomainUtils, uQueryDef, uQuery, uEntityList, uChangeManager, uUtils;

{ TObjectField }

function TObjectField.GetContentDefinition: TDefinition;
begin
  if Assigned(FContentDefinition) then
    Result := FContentDefinition
  else
    Result := TObjectFieldDef(FFieldDef)._ContentDefinition;
end;

function TObjectField.GetContentDefinitions: TList<TDefinition>;
begin
  Result := GetContentDefinition.InnerDefinitions;
end;

function TObjectField.GetDefaultDefinitionName: string;
begin
  Result := TObjectFieldDef(FFieldDef).DefaultDefinitionName;
end;

function TObjectField.GetIsSelector: Boolean;
begin
  Result := TObjectFieldDef(FFieldDef).IsSelector;
end;

constructor TObjectField.Create(const AInstance: TEntity; const AFieldDef: TFieldDef);
begin
  inherited Create(AInstance, AFieldDef);
  FContentDefinition := nil;
end;

destructor TObjectField.Destroy;
begin
  FContentDefinition := nil;
  inherited Destroy;
end;

function TObjectField.GetSearchType: TSearchType;
begin
  Result := TObjectFieldDef(FFieldDef).SearchType;
end;

function TObjectField.GetSortType: TEntitySortType;
begin
  Result := TObjectFieldDef(FFieldDef).SortType;
end;

function TObjectField.GetViewName: string;
begin
  Result := TObjectFieldDef(FFieldDef).StyleName;
end;

procedure TObjectField.SetContentDefinition(const Value: TDefinition);
begin
  Assert(Assigned(Value), 'New content definition should not be empty!');
  if FContentDefinition <> Value then
    FContentDefinition := Value;
end;

{ TEntityField }

constructor TEntityField.Create(const AInstance: TEntity; const AFieldDef: TFieldDef);
begin
  inherited Create(AInstance, AFieldDef);

  FEntity := nil;
  FCollectionID := TEntityFieldDef(AFieldDef).DefaultTypeID;
  FEntityID := TEntityFieldDef(AFieldDef).DefaultEntityID;
  FLoaded := False;
end;

procedure TEntityField.ResetToDefault(const AHolder: TObject);
begin
  if TEntityFieldDef(FFieldDef).DefaultEntityID = 0 then
    SetEntity(AHolder, nil)
  else begin
    Assert(not IsSelector, 'Selective field have no default value');
    SetEntity(AHolder, TDomain(FDomain)[DefaultDefinitionName].EntityByID(
      TEntityFieldDef(FFieldDef).DefaultEntityID));
  end;
end;

function TEntityField.RestoreEntity(const ACollectionID, AEntityID: Integer): TEntity;
var
  vCollection: TCollection;
begin
  vCollection := TDomain(FDomain).CollectionByID(ACollectionID);
  if not Assigned(vCollection) then
    Result := nil
  else
    Result := vCollection.EntityByID(AEntityID);
end;

procedure TEntityField.DoLoad(const AStorage: TStorage);
var
  vCollectionID: Integer;
begin
  if IsSelector then
  begin
    vCollectionID := VarToInt(AStorage.ReadValue(GetSelectorStorageName, fkInteger), 0);
    if vCollectionID > 0 then
      FCollectionID := vCollectionID;
  end;
  FEntityID := VarToInt(AStorage.ReadValue(StorageName, fkInteger), 0);
  FLoaded := False;
end;

procedure TEntityField.DoTransfer(const AStorage: TStorage);
begin
  if IsSelector then
    AStorage.WriteValue(GetSelectorStorageName, fkInteger, FCollectionID);

  if GetEntityID = 0 then
    AStorage.WriteValue(StorageName, fkInteger, Null)
  else begin
    if GetEntityID < 0 then
      TEntity(FEntity).GenerateID;
    AStorage.WriteValue(StorageName, fkInteger, GetEntityID);
  end;
end;

procedure TEntityField.AfterChanges(const AHolder: TObject);
var
  vEntity: TEntity;
begin
  vEntity := GetEntity;
  if Assigned(vEntity) then
  begin
    vEntity.AddListener(FieldName, FInstance);
    FInstance.TryAddToForeignList(AHolder, Self);
  end;
end;

procedure TEntityField.BeforeChanges(const AHolder: TObject);
begin
  if Assigned(FEntity) then
  begin
    FInstance.TryRemoveFromForeignList(AHolder, Self);
    FEntity.RemoveListener(FieldName, FInstance);
  end;
end;

function TEntityField.DoCheckValuedCondition(const AValue: Variant;
  const ACondition: TConditionKind; const AModifier: TConditionModifier): Boolean;
begin
  Result := CheckCondition(DoGetValue, AValue, FieldKind, ACondition, AModifier)
end;

function TEntityField.DoCompare(const AValue: Variant): Integer;
var
  vMyValue: TEntity;
  vValue: TEntity;
begin
  vMyValue := GetEntity;
  vValue := EntityFromVariant(AValue);
  if vMyValue = vValue then
    Result := 0
  else
    Result := AnsiCompareText(SafeDisplayName(vMyValue), SafeDisplayName(vValue));
end;

function TEntityField.DoExtractFieldValue(const AFieldName: string): Variant;
var
  vEntity: TEntity;
begin
  if AFieldName = 'ID' then
    Result := GetEntityID
  else begin
    vEntity := GetEntity;
    if Assigned(vEntity) then
      Result := vEntity.ExtractFieldValue(AFieldName)
    else
      Result := Null;
  end;
end;

function TEntityField.DoGetValue: Variant;
begin
  Result := Integer(GetEntity);
end;

procedure TEntityField.DoSetValue(const AValue: Variant);
begin
  FEntity := EntityFromVariant(AValue);
  if Assigned(FEntity) then
    FCollectionID := FEntity.Definition.ID;
end;

procedure TEntityField.GetAllEntitiesForSelect(const ASession, AList: TObject);
begin
  GetEntitiesForSelect(ASession, AList);
end;

function TEntityField.GetContentCollectionName: string;
begin
  Result := TDomain(FDomain).CollectionByID(FCollectionID).Name;
end;

procedure TEntityField.GetEntitiesForSelect(const ASession: TObject; const AList: TObject);
var
  vList: TEntityList absolute AList;
  i: Integer;
  vQuery: TQueryExecutor;
begin
  vList.Clear;
  vQuery := TQueryExecutor.Create(TEntityFieldDef(FFieldDef).QueryDef);
  try
    vQuery.SetParameters(ASession, FInstance);
    vQuery.Select(ASession);
    for i := 0 to vQuery.Results.Count - 1 do
      vList.Add(vQuery.Results[i]);
    vList.Sort(SortType);
    if not FFieldDef.HasFlag(cRequired) then
      vList.AddFirst(nil);
    vList.SetFiller(Self, False, TEntityFieldDef(FFieldDef).QueryDef);
  finally
    FreeAndNil(vQuery);
  end;
end;

function TEntityField.GetEntity: TEntity;
begin
  if not FLoaded then
  begin
    if not Assigned(FEntity) then
      FEntity := RestoreEntity(FCollectionID, FEntityID);
    FLoaded := True;
  end;
  Result := FEntity;
end;

function TEntityField.GetEntityID: Integer;
begin
  Result := SafeID(GetEntity);
end;

function TEntityField.GetSelectorStorageName: string;
begin
  Result := TEntityFieldDef(FFieldDef).SelectorStorageName;
end;

function TEntityField.GetJSONValue: TJSONValue;
var
  vEntityID: Integer;
begin
  vEntityID := GetEntityID;
  if vEntityID < 0 then
    vEntityID := 0;

  if IsSelector then
  begin
    Result := TJSONObject.Create;
    TJSONObject(Result).AddPair('id', TJSONNumber.Create(vEntityID));
    TJSONObject(Result).AddPair('type_id', TJSONNumber.Create(FCollectionID));
  end
  else
    Result := TJSONNumber.Create(vEntityID);
end;

procedure TEntityField.SetEntity(const AHolder: TObject; const AEntity: TEntity);
begin
  if Assigned(AEntity) and IsSelector then
    FCollectionID := AEntity.Definition.ID;

  SetValue(AHolder, Integer(AEntity));
end;

procedure TEntityField.SetIdentifiers(const AEntityID, ACollectionID: Integer);
begin
  if IsSelector then
  begin
    if ACollectionID > 0 then
      FCollectionID := ACollectionID;
  end;
  FEntityID := AEntityID;
  FLoaded := False;
end;

procedure TEntityField.SetJSONValue(const AJSONValue: TJSONValue);
var
  vJSONValue: TJSONValue;
  vCollectionID: Integer;
begin
  { TODO -owa : Проверить, правильно ли отрабатывает }
  FEntity := nil;
  if IsSelector then
  begin
    if not (AJSONValue is TJSONObject) then
    begin
      FEntityID := TJSONNumber(AJSONValue).AsInt;
      FCollectionID := FCollectionID;
    end
    else begin
      Assert(AJSONValue is TJSONObject, '[' + AJSONValue.ToString + ']');
      vJSONValue := TJSONObject(AJSONValue).Get('id').JsonValue;
      FEntityID := TJSONNumber(vJSONValue).AsInt;
      vJSONValue := TJSONObject(AJSONValue).Get('type_id').JsonValue;
      vCollectionID := TJSONNumber(vJSONValue).AsInt;
      if vCollectionID > 0 then
        FCollectionID := vCollectionID;
    end
  end
  else begin
    Assert(AJSONValue is TJSONNumber);
    FEntityID := TJSONNumber(AJSONValue).AsInt;
    FCollectionID := FCollectionID;
  end;
  FLoaded := False;
end;

{ TListField }

function TListField.AddListEntity(const AHolder: TObject; const ACollectionName: string;
  const AFieldNames: string; const AValues: array of Variant): TEntity;
var
  vCollectionName: string;
  vItem: TEntity;
  vMaxOrder: Integer;
begin
  if ACollectionName = '' then
    vCollectionName := TListFieldDef(FFieldDef).ContentDefinitionName
  else
    vCollectionName := ACollectionName;

  Result := TDomain(FDomain)[vCollectionName]._CreateNewEntity(TChangeHolder(AHolder), cNewID, AFieldNames, AValues, Self, False);

  // Установка правильного порядка вставки
  if (SortType = estSortByOrder) and Result.FieldExists('Order') then
  begin
    vMaxOrder := -1;
    for vItem in FList do
      if Assigned(vItem) then
        vMaxOrder := Max(vMaxOrder, vItem['Order']);
    Result._SetFieldValue(AHolder, 'Order', vMaxOrder + 1);
  end;

  LinkListEntity(AHolder, Result);
  TEntityCreationProc(TDomain(FDomain).Configuration.AfterEntityCreationProc)(TChangeHolder(AHolder), nil, Result);
end;

constructor TListField.Create(const AInstance: TEntity; const AFieldDef: TFieldDef);
begin
  inherited Create(AInstance, AFieldDef);
  FList := TList<TEntity>.Create;
  FDict := TDictionary<TEntity, string>.Create;
end;

destructor TListField.Destroy;
begin
  FreeAndNil(FDict);
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TListField.DoLoad(const AStorage: TStorage);
begin
end;

procedure TListField.DoSetValue(const AValue: Variant);
begin
end;

procedure TListField.DoTransfer(const AStorage: TStorage);
begin
end;

function TListField.ExtractEntity(const AEntityQuery: string): TEntity;
begin
  Result := nil;

end;

function TListField.ExtractField(const AFieldName: string): TBaseField;
var
  vFieldPath: string;
  vEntityQuery: string;
  vEntity: TEntity;
begin
  vFieldPath := AFieldName;
  vEntityQuery := CutDataTillMarker(vFieldPath, '.');
  if vFieldPath = '' then
    Exit(nil);

  vEntity := ExtractEntity(vEntityQuery);
  if Assigned(vEntity) then
    Result := vEntity.FieldByName(vFieldPath)
  else
    Result := nil;
end;

function TListField.FindOne(const ASession: TObject; const AQuery: string; const AParams: array of Variant): TEntity;
var
  vSession: TObject;
  vQueryDef: TQueryDef;
  vQuery: TQueryExecutor;
  vParamIndex: Integer;
  vParamName: string;
  i: Integer;
  vEntity: TEntity;
begin
  if not Assigned(ASession) then
    vSession := TDomain(FDomain).DomainSession
  else
    vSession := ASession;

  Result := nil;
  vQueryDef := TQueryDef.Create('', AQuery);
  vQuery := TQueryExecutor.Create(vQueryDef);
  try
    vQuery.SetParameters(vSession, nil);
    if Length(AParams) > 0 then
    begin
      vParamIndex := 0;
      for i := 0 to vQueryDef.Parameters.Count - 1 do
      begin
        vParamName := vQueryDef.Parameters[i];
        if Pos(':', vParamName) = 1 then
        begin
          vQuery.SetParameter(vParamName, AParams[vParamIndex]);
          vParamIndex := vParamIndex + 1;
          if vParamIndex >= Length(AParams) then
            Break;
        end;
      end;
    end;

    for i := 0 to FList.Count - 1 do
    begin
      vEntity := GetEntity(i);
      if not vEntity.IsRemote and vQuery.IsMatch(vSession, vEntity) then
        Exit(vEntity);
    end;
  finally
    FreeAndNil(vQuery);
    FreeAndNil(vQueryDef);
  end;
end;

function TListField.DoCheckValuedCondition(const AValue: Variant;
  const ACondition: TConditionKind;
  const AModifier: TConditionModifier): Boolean;
begin
  Result := CheckCondition(DoGetValue, AValue, FieldKind, ACondition, AModifier);
end;

function TListField.DoCompare(const AValue: Variant): Integer;
begin
  Result := 1;
  Assert(False, 'Lists comparision is not supported');
end;

function TListField.DoExtractFieldValue(const AFieldName: string): Variant;
var
  vFieldPath: string;
  vEntityPath: string;
  vEntity: TEntity;
begin
  if AFieldName = 'Count' then
    Result := GetCount
  else begin
    vFieldPath := AFieldName;
    vEntityPath := CutDataTillMarker(vFieldPath, '.');
    if vFieldPath = '' then
      Exit(Null);

    vEntity := ExtractEntity(vEntityPath);
    if Assigned(vEntity) then
      Result := vEntity.ExtractFieldValue(vFieldPath)
    else
      Result := Null;
  end;
end;

function TListField.DoGetValue: Variant;
begin
  Result := Integer(Self);
end;

procedure TListField.GetAllEntitiesForSelect(const ASession, AList: TObject);
var
  vList: TEntityList absolute AList;
  i: Integer;
  vQuery: TQueryExecutor;
begin
  vList.Clear;
  vQuery := TQueryExecutor.Create(TListFieldDef(FFieldDef).QueryDef);
  try
    vQuery.SetParameters(ASession, FInstance);
    vQuery.Select(ASession);
    for i := 0 to vQuery.Results.Count - 1 do
      vList.Add(vQuery.Results[i]);
    vList.Sort(SortType);
    vList.SetFiller(Self, True, TListFieldDef(FFieldDef).QueryDef);
  finally
    FreeAndNil(vQuery);
  end;
end;

function TListField.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TListField.GetEntitiesForSelect(const ASession: TObject; const AList: TObject);
var
  vList: TEntityList absolute AList;
  i: Integer;
  vQuery: TQueryExecutor;
begin
  vList.Clear;
  vQuery := TQueryExecutor.Create(TListFieldDef(FFieldDef).ExclusiveQueryDef);
  try
    vQuery.SetParameters(ASession, FInstance);
    vQuery.Select(ASession);
    for i := 0 to vQuery.Results.Count - 1 do
      vList.Add(vQuery.Results[i]);
    vList.Sort(SortType);
    vList.SetFiller(Self, True, TListFieldDef(FFieldDef).ExclusiveQueryDef);
  finally
    FreeAndNil(vQuery);
  end;
end;

function TListField.GetEntity(const AIndex: Integer): TEntity;
begin
  Result := FList[AIndex];
end;

function TListField.GetEntityColor(const AEntity: TEntity): TColor;
var
  vColorFieldName: string;
  vColorField: TBaseField;
begin
  Result := cNullColor;
  vColorFieldName := TListFieldDef(FFieldDef).ColorFieldName;
  if vColorFieldName = '' then
    Exit;

  vColorField := AEntity.FieldByName(vColorFieldName);
  if Assigned(vColorField) then
    Result := vColorField.Value;
end;

procedure TListField.GetEntityList(const ASession: TObject; const AEntityList: TObject);
var
  i: Integer;
  vList: TEntityList absolute AEntityList;
  vCount: Integer;
  vQuery: TQueryExecutor;
begin
  vList.Clear;
  vQuery := TQueryExecutor.Create(TListFieldDef(FFieldDef).ContentQueryDef);
  try
    vQuery.SetParameters(ASession, FInstance);
    vCount := GetCount;
    for i := 0 to vCount - 1 do
      if vQuery.IsMatch(ASession, FList[i]) then
        vList.Add(FList[i]);
    vList.Sort(SortType, TListFieldDef(FFieldDef).ColorFieldName);
    vList.SetFiller(Self, False, TListFieldDef(FFieldDef).ContentQueryDef);
  finally
    FreeAndNil(vQuery);
  end;
end;

function TListField.GetEnumerator: TEnumerator<TEntity>;
begin
  Result := FList.GetEnumerator;
end;

function TListField.GetRelationPower: TRelationPower;
begin
  Result := TListFieldDef(FFieldDef).RelationPower;
end;

function TListField.GetMasterFieldName: string;
begin
  Result := TListFieldDef(FFieldDef).MasterFieldName;
end;

procedure TListField.InternalRemove(const AHolder: TObject; const AEntity: TEntity);
var
  vCollection: TCollection;
begin
  Assert(Assigned(AEntity), 'Somebody already delete this entity');

  TDomain(FDomain).Logger.AddMessage('Marked for deletion: ' + AEntity.ToString);
  vCollection := TCollection(AEntity.Collection);
  vCollection.MarkEntityAsDeleted(AHolder, AEntity);
end;

procedure TListField.LinkListEntity(const AHolder: TObject; const AEntity: TEntity);
begin
  if Contains(AEntity) then
    TDomain(FDomain).Logger.AddMessage('Exit from LinkListEntity')
  else
    // Привязываем листовую сущность
    //TChangeHolder(AHolder).SetFieldEntity(AEntity, GetMasterFieldName, FInstance);
    AEntity._SetFieldEntity(AHolder, GetMasterFieldName, FInstance);
end;

procedure TListField.GetList(const ASession: TObject; const AList: TList<TEntity>);
var
  i: Integer;
  vQuery: TQueryExecutor;
begin
  AList.Clear;
  vQuery := TQueryExecutor.Create(TListFieldDef(FFieldDef).ContentQueryDef);
  try
    vQuery.SetParameters(ASession, FInstance);
    for i := 0 to FList.Count - 1 do
      if vQuery.IsMatch(ASession, FList[i]) then
        AList.Add(FList[i]);
  finally
    FreeAndNil(vQuery);
  end;
end;

procedure TListField.AddToList(const AHolder: TObject; const AEntity: TEntity);
var
  vQuery: TQueryExecutor;
  vSession: TObject;
begin
  if Contains(AEntity) then
    Exit;

  if TListFieldDef(FFieldDef).Filter <> '' then
  begin
    vQuery := TQueryExecutor.Create(TListFieldDef(FFieldDef).QueryDef);
    try
      if Assigned(AHolder) then
        vSession := TChangeHolder(AHolder).Session
      else
        vSession := nil;

      if not vQuery.IsMatch(vSession, AEntity) then
        Exit;
    finally
      FreeAndNil(vQuery);
    end;
  end;

  FList.Add(AEntity);
  FDict.Add(AEntity, '');
  AEntity.AddListener(FieldName, FInstance);
  FInstance.ProcessFieldChanged(AHolder, dckListAdded, GetFieldName, AEntity);
end;

procedure TListField.ClearList(const AHolder: TObject);
var
  i: Integer;
begin
  if GetRelationPower = rpStrong then
  begin
    for i := FList.Count - 1 downto 0 do
      InternalRemove(AHolder, FList[i])
  end
  else
    for i := FList.Count - 1 downto 0 do
      UnlinkListEntity(AHolder, FList[i]);
end;

function TListField.Contains(const AEntity: TEntity): Boolean;
begin
  Result := FDict.ContainsKey(AEntity);
end;

procedure TListField.RemoveListEntity(const AHolder: TObject; const AEntity: TEntity);
begin
  if GetRelationPower = rpStrong then
    InternalRemove(AHolder, AEntity)
  else
    UnlinkListEntity(AHolder, AEntity)
end;

procedure TListField.UnlinkListEntity(const AHolder: TObject; const AEntity: TEntity);
var
  vMasterField: TEntityField;
begin
  vMasterField := TEntityField(AEntity.FieldByName(GetMasterFieldName));
  // При отвязке сущности от списка нужно устанавливать ее мастер поле
  //   в значение поля по умолчанию ??? или в специальное значение
  if Assigned(AHolder) then
  begin
    if AEntity.Deleted then
      vMasterField.SetEntity(AHolder, nil)
    else
      vMasterField.ResetToDefault(AHolder);
  end
  else begin
    //?? DeleteFromList ??
    FList.Remove(AEntity);
    FDict.Remove(AEntity);
  end;
end;

procedure TListField.DeleteFromList(const AHolder: TObject; const AEntity: TEntity);
begin
  if FList.Remove(AEntity) < 0 then
    Exit;

  FDict.Remove(AEntity);
  AEntity.RemoveListener(FieldName, FInstance);
  if not FInstance.Deleted then
    FInstance.ProcessFieldChanged(AHolder, dckListRemoved, GetFieldName, AEntity);
end;

end.
