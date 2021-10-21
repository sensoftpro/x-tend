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

unit uCollection;

interface

uses
  Classes, Generics.Collections, uConsts, uDefinition, uEntity, uSession,
  uStorage, uJSON, uChangeManager;

type
  PIndexNode = ^TIndexNode;
  TIndexNode = array[0..0] of Pointer;
  TCollection = class;

  // Класс используется для хранения сопоставлений между идентификаторами,
  // сущностями и новыми сущностями
  TIndexMap = class
  private
    FNodePower: Integer;
    FCutMask: Integer;
    FIndexNodeSize: Integer;
    FNodeSize: Integer;
    FLevelCount: Integer;
    FMaxID: Integer;
    FRoot: PIndexNode;
    FJustCreatedList: TList<TEntity>;
    procedure GrowTo(const ANewMaxID: Integer);
  public
    constructor Create(const ACollection: TCollection; const ANodePower: Integer = 6);
    destructor Destroy; override;

    // Добавление "живой" сущности
    procedure AddEntity(const AID: Integer; const AEntity: TEntity);
    procedure DeleteEntity(const AID: Integer);
    // Получение "живой" сущности или nil
    function GetEntity(const AID: Integer): TEntity;
  end;

  TCollection = class(TEntity)
  private
    [Weak] FContentDefinition: TDefinition;
    FEntityClass: TEntityClass;
    // Список инстанцированных сущностей
    FList: TObjectList<TEntity>;
    // Быстрый доступ к сущностям по ID
    FIndexMap: TIndexMap;
    FLoaded: Boolean;
    FMaxID: Integer;
    FListeners: TList<TObject>;
    function GetCount: Integer;

    function GetEntity(const AIndex: Integer): TEntity;
    function GetName: string;
  private
    procedure DoLoadAll(const AStorage: TStorage);
    procedure DoFetchAll(const AFieldDict: TDictionary<string, Integer>; const AData: Variant);

    function InternalCreateEntity(const AID: Integer = cNewID;
      const AIsNew: Boolean = False): TEntity;
  protected
    procedure SetViewState(const Value: TViewState); override;
  public // Special for TEntity
    procedure InternalAdd(const AEntity: TEntity);

    procedure AddViewListener(const AListener: TObject);
    procedure RemoveViewListener(const AListener: TObject);
    procedure NotifyListeners(const AChangeKind: Word; const AParameter: TObject);
  public
    constructor Create(const ADomain: TObject; const ADefinition: TDefinition); override;
    destructor Destroy; override;

    function EntityByNameAndGrouping(const AFieldName, AValue: string;
      const AGroupFieldName: string = ''; const AGroupID: Integer = 0): TEntity;

    function IndexOfField(const AFieldName: string): Integer;
  public
    procedure LoadAll(const AStorage: TStorage; const APreferAsync: Boolean = False);
    procedure ExportAll(const AJSONArray: TJSONArray);
    procedure Subscribe;
    procedure Unsubscribe;

    function CreateEntityFromJSON(const AEntityID: Integer;
      const AJSONObject: TJSONObject): TEntity;
    procedure FillEntityFromJSON(const AEntity: TEntity;
      const AJSONObject: TJSONObject; const AIsNew: Boolean);

    function Find(const ASession: TUserSession; const AQuery: string; const AParams: array of Variant;
      const AFindAll: Boolean = True): TList<TEntity>;
    procedure FindInto(const ASession: TUserSession; const AQuery: string; const AParams: array of Variant;
      const AList: TList<TEntity>; const AFindAll: Boolean = True);
    function FindOne(const ASession: TUserSession; const AQuery: string; const AParams: array of Variant): TEntity;

    // This is a just way to get or release entity
    // Get existed entity
    function EntityByID(const AID: Integer): TEntity;
    function EntityByName(const AName: string): TEntity;
    function First: TEntity;
    // Add, Create New or Load entity
    // Wrappers for overridable methods
    function CreateNewEntity(const AHolder: TChangeHolder; const AID: Integer = cNewID;
      const AFieldContext: TBaseField = nil): TEntity;
    function CreateDefaultEntity(const AHolder: TChangeHolder; const AID: Integer; const AFieldNames: string;
      const AValues: array of Variant; const AIsService: Boolean = False): TEntity;
    procedure RemoveEntity(const AEntity: TEntity);
    procedure MarkEntityAsDeleted(const AHolder: TObject; const AEntity: TEntity);
    procedure ProcessEntitySaving(const AHolder: TObject; const AEntity: TEntity);
    procedure GenerateEntityID(const AEntity: TEntity);
    function CloneEntity(const AHolder: TObject; const ASourceEntity: TEntity): TEntity;

    procedure SetCollectionAttributes(const AEntityClass: TEntityClass; const AContentDefinition: TDefinition);

    function FindSimilarEntity(const ASession: TObject; const AEntity: TEntity): TEntity;

    function GetCaption: string;

    function GetEnumerator: TEnumerator<TEntity>;

    property Count: Integer read GetCount;
    property Entities[const AIndex: Integer]: TEntity read GetEntity; default;
    property ContentDefinition: TDefinition read FContentDefinition;
    property Name: string read GetName;
    property MaxID: Integer read FMaxID;
    property IndexMap: TIndexMap read FIndexMap;
    property Loaded: Boolean read FLoaded;
  end;

type
  TEntityChangingProc = procedure(const AHolder: TChangeHolder; const AEntity: TEntity) of object;

implementation

uses
  Types, SysUtils, Variants, uDomain, uSimpleField, uObjectField,
  uQueryDef, uQuery, uDomainUtils;

{ TIndexMap }

{$R-}

procedure TIndexMap.AddEntity(const AID: Integer; const AEntity: TEntity);
var
  i: Integer;
  vCurrentNode: PIndexNode;
  vNewNode: PIndexNode;
  vCurrentIndex: Integer;
begin
  // Есть два варианта, сериализованная сущность и только что добавленная
  // 1. Только что добавленная
  if AID < 0 then
  begin
    vCurrentIndex := -FJustCreatedList.Add(AEntity) - 1;
    TEntity(AEntity).SetID(vCurrentIndex);
    Exit;
  end;

  // 2. Сериализованная сущность
  if AID > FMaxID then
    GrowTo(AID);

  vCurrentNode := FRoot;
  for i := FLevelCount - 1 downto 1 do
  begin
    vCurrentIndex := (AID shr (FNodePower * i)) and FCutMask;
    if Assigned(vCurrentNode^[vCurrentIndex]) then
      vCurrentNode := vCurrentNode^[vCurrentIndex]
    else begin
      GetMem(vNewNode, FIndexNodeSize);
      FillChar(vNewNode^, FIndexNodeSize, 0);
      vCurrentNode^[vCurrentIndex] := vNewNode;
      vCurrentNode := vNewNode;
    end;
  end;

  vCurrentNode^[AID and FCutMask] := AEntity;
end;

constructor TIndexMap.Create(const ACollection: TCollection; const ANodePower: Integer);
begin
  inherited Create;

  FJustCreatedList := TList<TEntity>.Create;
  FNodePower := ANodePower;
  FNodeSize := Round(Exp(FNodePower*Ln(2)));
  FIndexNodeSize := SizeOf(Pointer) * FNodeSize;
  FCutMask := FNodeSize - 1;
  FLevelCount := 1;
  FMaxID := FNodeSize * FLevelCount - 1;
  GetMem(FRoot, FIndexNodeSize);
  FillChar(FRoot^, FIndexNodeSize, 0);
end;

procedure TIndexMap.DeleteEntity(const AID: Integer);
var
  i: Integer;
  vCurrentNode: PIndexNode;
  vCurrentIndex: Integer;
begin
  // Есть два варианта, сериализованная сущность и только что добавленная
  // 1. Только что добавленная
  if AID < 0 then
  begin
    // Удалять сущности нельзя, так как изменится индексация
    FJustCreatedList[-AID - 1] := nil;
    Exit;
  end;

  // 2. Сериализованная сущность
  if AID > FMaxID then
    // Удалять нечего
    Exit;

  vCurrentNode := FRoot;
  for i := FLevelCount - 1 downto 1 do
  begin
    vCurrentIndex := (AID shr (FNodePower * i)) and FCutMask;
    vCurrentNode := vCurrentNode^[vCurrentIndex];
    if vCurrentNode = nil then
      Exit;
  end;

  vCurrentNode^[AID and FCutMask] := nil;
end;

destructor TIndexMap.Destroy;
  procedure DestroyChildren(const ALevel: Integer; const ANode: PIndexNode);
  var
    i: Integer;
  begin
    if ALevel > 0 then
      for i := 0 to FNodeSize-1 do
        if Assigned(ANode^[i]) then
          DestroyChildren(ALevel - 1, ANode^[i]);

    FreeMem(ANode, FIndexNodeSize);
  end;
begin
  DestroyChildren(FLevelCount-1, FRoot);
  FreeAndNil(FJustCreatedList);
  inherited Destroy;
end;

function TIndexMap.GetEntity(const AID: Integer): TEntity;
var
  i: Integer;
  vCurrentNode: PIndexNode;
  vCurrentIndex: Integer;
begin
  Result := nil;
  if AID = 0 then
    Exit;

  // Отработка двух возможных вариантов
  // 1. Новая сущность
  if AID < 0 then
  begin
    vCurrentIndex := -AID - 1;
    if vCurrentIndex < FJustCreatedList.Count then
      Result := FJustCreatedList[vCurrentIndex];
    Exit;
  end;

  // 2. Сериализованная сущность
  //   мы можем выйти досрочно на любом этапе, при этом значение будет
  //   невалидным указателем - нужно предусмотреть этот случай
  if AID > FMaxID then
    Exit;

  vCurrentNode := FRoot;
  for i := FLevelCount - 1 downto 1 do
  begin
    vCurrentIndex := (AID shr (FNodePower * i)) and FCutMask;
    vCurrentNode := vCurrentNode^[vCurrentIndex];
    if vCurrentNode = nil then
      Exit;
  end;

  Result := vCurrentNode^[AID and FCutMask];
end;

procedure TIndexMap.GrowTo(const ANewMaxID: Integer);
var
  vOldRoot: PIndexNode;
begin
  FLevelCount := FLevelCount + 1;

  if (MaxInt / FMaxID) < FNodeSize then
    FMaxID := MaxInt
  else
    FMaxID := FNodeSize * (FMaxID + 1) - 1;
  vOldRoot := FRoot;
  GetMem(FRoot, FIndexNodeSize);
  FillChar(FRoot^, FIndexNodeSize, 0);
  FRoot^[0] := vOldRoot;

  if FMaxID < ANewMaxID then
    GrowTo(ANewMaxID);
end;

{$R+}

type
  TCrackedEntity = class(TEntity);

{ TCollection }

procedure TCollection.AddViewListener(const AListener: TObject);
begin
  if FListeners.IndexOf(AListener) < 0 then
    FListeners.Add(AListener);
end;

function TCollection.CloneEntity(const AHolder: TObject; const ASourceEntity: TEntity): TEntity;
begin
  if Assigned(ASourceEntity) then
    Result := ASourceEntity.Clone(AHolder, '')
  else
    Result := nil;
end;

constructor TCollection.Create(const ADomain: TObject; const ADefinition: TDefinition);
begin
  inherited Create(ADomain, ADefinition);

  FCollection := TDomain(ADomain).RootCollection;
  FEntityClass := TEntity;

  FIndexMap := TIndexMap.Create(Self, 4);
  FList := TObjectList<TEntity>.Create;
  FListeners := TList<TObject>.Create;
  FLoaded := False;
  FMaxID := -1;
end;

type
  TCrackedField = class(TBaseField);

function TCollection.CreateNewEntity(const AHolder: TChangeHolder; const AID: Integer = cNewID;
  const AFieldContext: TBaseField = nil): TEntity;
var
  vField: TEntityField absolute AFieldContext;
  vDocCode: string;
  vDependencies: TStrings;
  i: Integer;
  vMyFieldName: string;
  vMasterFieldName: string;
  vMasterField: TEntityField;
begin
  Result := InternalCreateEntity(AID, True);

  if Assigned(AHolder) then
    AHolder.RegisterEntityCreating(Result);

  // Создание нового кода документа
  if (FContentDefinition.Kind = clkDocument) and FContentDefinition.FieldExists('DocCode') then
  begin
    vDocCode := TDomain(FDomain).Translate(FContentDefinition.Name + '@Prefix', FContentDefinition.Prefix) +
      IntToStr(TDomain(FDomain).Storage.CreateCodes(FContentDefinition.StorageName));
    Result.FieldByName('DocCode').Value := vDocCode;
  end;

  // Нужно, так как entity поля могут иметь значения по умолчанию
  Result.SubscribeFields;

  if Assigned(AFieldContext) and (AFieldContext.FieldKind = fkObject) then
  begin
    // Установка поля, от которого зависит эта сущность
    vDependencies := TObjectFieldDef(vField.FieldDef).SelectiveFields;
    for i := 0 to vDependencies.Count - 1 do
    begin
      vMyFieldName := vDependencies.Names[i];
      vMasterFieldName := vDependencies.ValueFromIndex[i];
      if vMasterFieldName <> '' then
      begin
        vMasterField := TEntityField(vField.OwnerInstance.FieldByName(vMasterFieldName));
        AHolder.SetFieldEntity(TEntity(Result), vMyFieldName, TEntity(vMasterField.Entity));
      end;
    end;
  end;

  TEntityChangingProc(TDomain(FDomain).Configuration.AfterEntityCreationProc)(AHolder, Result);

  NotifyListeners(dckListAdded, Result);
end;

function TCollection.CreateEntityFromJSON(const AEntityID: Integer; const AJSONObject: TJSONObject): TEntity;
begin
  Result := EntityByID(AEntityID);
  if Assigned(Result) then
  begin
    Exit;
    //Assert(not Assigned(Result), 'This object is already created');
  end;

  Result := InternalCreateEntity(AEntityID, False);
  Result.FillFromJSON(AJSONObject, True);
end;

function TCollection.CreateDefaultEntity(const AHolder: TChangeHolder; const AID: Integer; const AFieldNames: string;
  const AValues: array of Variant; const AIsService: Boolean = False): TEntity;
var
  i: Integer;
  vPopulateAll: Boolean;
begin
  vPopulateAll := True;

  // Для локальных коллекций в параметре AID можно передать доп. информацию
  if FContentDefinition.HasFlag(ccLocalOnly) then
  begin
    for i := 0 to FList.Count - 1 do
    begin
      Result := GetEntity(i);
      if not Result.IsRemote then
      begin
        Result.IsService := AIsService;
        Exit;
      end;
    end;

    Result := CreateNewEntity(AHolder);
  end
  else begin
    if FMaxID < AID  then
      FMaxID := AID;

    Result := EntityByID(AID);
    if (AID > 0) and Assigned(Result) then
      vPopulateAll := False
    else
      Result := CreateNewEntity(AHolder, AID);
  end;

  // Заполнение значений свежесозданной сущности
  if Length(AFieldNames) > 0 then
    TCrackedEntity(Result).Populate(AFieldNames, AValues, vPopulateAll);
  Result.IsService := AIsService;

  if not vPopulateAll then
    Exit;

  if FContentDefinition.HasFlag(ccNotSave) then
    Result.IsNew := False;
end;

destructor TCollection.Destroy;
begin
  FreeAndNil(FListeners);
  FreeAndNil(FList);
  FreeAndNil(FIndexMap);

  FContentDefinition := nil;

  inherited Destroy;
end;

procedure TCollection.DoFetchAll(const AFieldDict: TDictionary<string, Integer>; const AData: Variant);
var
  i, j: Integer;
  vId: Integer;
  vEntity: TEntity;
  vField: TBaseField;

  function ValueByFieldName(const ARowNo: Integer; const AFieldName: string): Variant;
  var
    vIndex: Integer;
  begin
    if AFieldDict.TryGetValue(AFieldName, vIndex) then
      Result := AData[vIndex, ARowNo]
    else
      Result := Null;
  end;
begin
  for i := VarArrayLowBound(AData, 2) to VarArrayHighBound(AData, 2) do
  begin
    vId := ValueByFieldName(i, 'id');
    if Assigned(EntityByID(vId)) then
      Continue;

    if (FContentDefinition = TDomain(FDomain).Configuration.RootDefinition) and (vId = 2) then
      vEntity := Self
    else
      vEntity := InternalCreateEntity(vId, False);

    vEntity.IsNew := False;
    if vEntity.IsEnvSpecific then
      vEntity.EnvironmentID := VarToStrDef(ValueByFieldName(i, 'guid'), '');

    for j := 0 to vEntity.FieldCount - 1 do
    begin
      vField := vEntity.Fields[j];
      if vField.FieldKind in [fkString..fkCurrency] then
        vField.Value := ValueByFieldName(i, vField.StorageName)
      else if vField.FieldKind = fkObject then
      begin
        if TEntityField(vField).IsSelector then
          TEntityField(vField).SetIdentifiers(VarToInt(ValueByFieldName(i, vField.StorageName), 0),
            VarToInt(ValueByFieldName(i, TEntityFieldDef(vField.FieldDef).SelectorStorageName), 0))
        else
          TEntityField(vField).SetIdentifiers(VarToInt(ValueByFieldName(i, vField.StorageName), 0));
      end
      else if vField.FieldKind = fkBlob then
      begin
      end
      else if vField.FieldKind = fkComplex then
      begin
      end;
    end;
  end;
end;

procedure TCollection.DoLoadAll(const AStorage: TStorage);
var
  vId: Integer;
  vName: string;
  vEntity: TEntity;
begin
  vId := AStorage.ReadValue('id', fkInteger);
  if Assigned(EntityByID(vId)) then
    Exit;

  { TODO -owa : Убрать магическую константу 2 - это SysDefinition }
  if (FContentDefinition = TDomain(FDomain).Configuration.RootDefinition) then
  begin
    vName := AStorage.ReadValue('name', fkString);
    if SameText('SysDefinitions', vName) then
      vEntity := Self
    else
      vEntity := InternalCreateEntity(vId, False);
  end
  else
    vEntity := InternalCreateEntity(vId, False);
  TCrackedEntity(vEntity).InternalLoad(AStorage);
end;

function TCollection.EntityByID(const AID: Integer): TEntity;
begin
  Result := FIndexMap.GetEntity(AID);
  if not Assigned(Result) then;
    { TODO -owa -clazy load : Отсутствие сущности указывает на необходимость ее загрузки }
end;

function TCollection.EntityByName(const AName: string): TEntity;
var
  vEntity: TEntity;
begin
  for vEntity in FList do
    if SameText(AName, vEntity['Name']) then
      Exit(vEntity);
  Result := nil;
end;

function TCollection.EntityByNameAndGrouping(const AFieldName, AValue, AGroupFieldName: string;
  const AGroupID: Integer): TEntity;
var
  vIndex: Integer;
  vGroupingIndex: Integer;
  i: Integer;
begin
  vGroupingIndex := FContentDefinition.IndexOfField(AGroupFieldName);
  vIndex := FContentDefinition.IndexOfField(AFieldName);
  if vIndex < 0 then
    Exit(nil);

  for i := 0 to FList.Count - 1 do
  begin
    Result := TEntity(FList[i]);
    if TStringField(Result.Fields[vIndex]).Value = AValue then
    begin
      if vGroupingIndex < 0 then
        Exit
      else if TEntityField(Result.Fields[vGroupingIndex]).EntityID = AGroupID then
        Exit;
    end;
  end;

  Result := nil;
end;

procedure TCollection.ExportAll(const AJSONArray: TJSONArray);
var
  i: Integer;
  vEntity: TEntity;
  vEntityData: TJSONObject;
begin
  for i := 0 to FList.Count - 1 do
  begin
    vEntity := GetEntity(i);
    if Assigned(vEntity) and (vEntity.ID > 0) then
    begin
      vEntityData := TJSONObject.Create;
      vEntity.ExportToJSON(vEntityData);
      AJSONArray.AddElement(vEntityData);
    end;
  end;
end;

procedure TCollection.FillEntityFromJSON(const AEntity: TEntity; const AJSONObject: TJSONObject; const AIsNew: Boolean);
begin
  if not Assigned(AEntity) then
    Exit;

  AEntity.FillFromJSON(AJSONObject, AIsNew);
  NotifyListeners(dckEntityChanged, AEntity);
end;

function TCollection.Find(const ASession: TUserSession; const AQuery: string; const AParams: array of Variant;
  const AFindAll: Boolean = True): TList<TEntity>;
begin
  Result := TList<TEntity>.Create;
  FindInto(ASession, AQuery, AParams, Result, AFindAll);
end;

procedure TCollection.FindInto(const ASession: TUserSession; const AQuery: string; const AParams: array of Variant;
  const AList: TList<TEntity>; const AFindAll: Boolean);
var
  vQueryDef: TQueryDef;
  vQuery: TQueryExecutor;
  vParamIndex: Integer;
  vParamName: string;
  i: Integer;
  vEntity: TEntity;
begin
  vQueryDef := TQueryDef.Create('', AQuery);
  vQuery := TQueryExecutor.Create(vQueryDef);
  try
    vQuery.SetParameters(ASession, nil);
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
      if not vEntity.IsRemote and vQuery.IsMatch(ASession, vEntity) then
      begin
        AList.Add(vEntity);
        if not AFindAll then
          Exit;
      end;
    end;
  finally
    FreeAndNil(vQuery);
    FreeAndNil(vQueryDef);
  end;
end;

function TCollection.FindOne(const ASession: TUserSession; const AQuery: string;
  const AParams: array of Variant): TEntity;
var
  vList: TList<TEntity>;
begin
  vList := Find(ASession, AQuery, AParams, True);
  try
    if vList.Count > 0 then
      Result := vList[0]
    else
      Result := nil;
  finally
    FreeAndNil(vList);
  end;
end;

function TCollection.FindSimilarEntity(const ASession: TObject; const AEntity: TEntity): TEntity;
var
  i: Integer;
  vEntity: TEntity;
  vCheckUniqueQuery: TQueryExecutor;
begin
  Result := nil;
  if not Assigned(FContentDefinition.CheckUniqueQuery) then
    Exit;

  vCheckUniqueQuery := TQueryExecutor.Create(FContentDefinition.CheckUniqueQuery);
  try
    vCheckUniqueQuery.SetParameters(ASession, AEntity);
    for i := 0 to FList.Count - 1 do
    begin
      vEntity := FList[i];
      Assert(Assigned(vEntity), 'Wrong behaviour');

      if not vEntity.IsRemote and vCheckUniqueQuery.IsMatch(ASession, vEntity) then
      begin
        Result := vEntity;
        Exit;
      end;
    end;
  finally
    FreeAndNil(vCheckUniqueQuery);
  end;
end;

function TCollection.First: TEntity;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Result := FList[i];
    if not Result.IsRemote then
      Exit;
  end;

  Result := nil;
end;

procedure TCollection.GenerateEntityID(const AEntity: TEntity);
var
  vId: Integer;
begin
  Assert(Assigned(AEntity) and (AEntity.ID < 0), 'Generation of new ID for already existed entity');

  FIndexMap.DeleteEntity(AEntity.ID);
  vId := TDomain(FDomain).Storage.CreateIDs(FContentDefinition.StorageName);
  AEntity.SetID(vId);
  FIndexMap.AddEntity(vId, AEntity);

  TDomain(FDomain).Logger.AddMessage('> Generated new ID for entity: ' + AEntity.ToString);
end;

function TCollection.GetCaption: string;
begin
  Result := TDomain(FDomain).TranslateDefinition(FContentDefinition);
end;

function TCollection.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCollection.GetEntity(const AIndex: Integer): TEntity;
begin
  Result := FList[AIndex];
end;

function TCollection.GetEnumerator: TEnumerator<TEntity>;
begin
  Result := FList.GetEnumerator;
end;

function TCollection.GetName: string;
begin
  Result := FContentDefinition.Name;
end;

function TCollection.IndexOfField(const AFieldName: string): Integer;
begin
  Result := FContentDefinition.IndexOfField(AFieldName);
end;

procedure TCollection.InternalAdd(const AEntity: TEntity);
begin
  FList.Add(AEntity);
  FIndexMap.AddEntity(AEntity.ID, AEntity);
end;

function TCollection.InternalCreateEntity(const AID: Integer; const AIsNew: Boolean): TEntity;
//var
//  vNewStr: string;
begin
  Result := FEntityClass.Create(FDomain, FContentDefinition);
  Result.SetID(AID);
  Result.IsNew := AIsNew;
  //if AIsNew then
  //  vNewStr := 'NEW '
  //else
  //  vNewStr := '';
  //TDomain(FDomain).Logger.AddMessage('# Creating ' + vNewStr + FContentDefinition.Name + ': ' + IntToStr(AID));
  InternalAdd(Result);
end;

procedure TCollection.LoadAll(const AStorage: TStorage; const APreferAsync: Boolean = False);
begin
  if not FContentDefinition.HasFlag(ccNotSave) then
  begin
    AStorage.Activate(FContentDefinition.StorageName);
    try
      try
        if APreferAsync and False // FContentDefinition.IsDescendantOf('SysDefinitions')
        //  or FContentDefinition.IsDescendantOf('Invoices')
        //  or FContentDefinition.IsDescendantOf('InvoiceServices'))
        then
          AStorage.ReadGroupAsync(FContentDefinition.StorageName, DoFetchAll)
        else
          AStorage.ReadGroup(FContentDefinition.StorageName, DoLoadAll);
      except
        if not TDomain(FDomain).SyncWithStorage(FContentDefinition, AStorage) then
          AStorage.Rebuild
        else
          raise;
      end;
    finally
      AStorage.Deactivate;
    end;
  end;

  FLoaded := True;

  // TODO: search indexes
  //if FSearchIndexes.Count > 0 then
  //  ShowSearchIndexInfo;
end;

procedure TCollection.MarkEntityAsDeleted(const AHolder: TObject; const AEntity: TEntity);
var
  vFieldDef: TFieldDef;
begin
  if not Assigned(AEntity) then
    Exit;
  if AEntity.Deleted then
    Exit;

  // TODO: search indexes
  //if ADeleted then
  //  RemoveFromSearchIndexes(AEntity)
  //else
  //  AddToSearchIndexes(AEntity);

  // Транзитные листовые поля тоже необходимо пометить
  for vFieldDef in FContentDefinition.Fields do
    if vFieldDef.Kind = fkList then
      TListField(AEntity.FieldByName(vFieldDef.Name)).ClearList(AHolder);

  TEntityChangingProc(TDomain(FDomain).Configuration.BeforeEntityRemovingProc)(TChangeHolder(AHolder), AEntity);

  NotifyListeners(dckListRemoved, AEntity);

  AEntity.SetDeleted(AHolder);
end;

procedure TCollection.NotifyListeners(const AChangeKind: Word; const AParameter: TObject);
var
  vMessage: TDomainChangedMessage;
  i: Integer;
  vListener: TObject;
begin
  vMessage.Msg := DM_DOMAIN_CHANGED;
  vMessage.Kind := AChangeKind;
  vMessage.Sender := Self;
  vMessage.Parameter := AParameter;

  for i := FListeners.Count - 1 downto 0 do
  begin
    vListener := TObject(FListeners[i]);
    vListener.Dispatch(vMessage);
  end;
end;

procedure TCollection.ProcessEntitySaving(const AHolder: TObject; const AEntity: TEntity);
begin
  NotifyListeners(dckEntitySaved, AEntity);
  TEntityChangingProc(TDomain(FDomain).Configuration.AfterEntitySavedProc)(TChangeHolder(AHolder), AEntity);
end;

procedure TCollection.RemoveEntity(const AEntity: TEntity);
begin
  if not Assigned(AEntity) then
    Exit;

  if not AEntity.Deleted then
    Exit;

  FIndexMap.DeleteEntity(AEntity.ID);
  if FList.Remove(AEntity) < 0 then
    AEntity.DisposeOf;

  TDomain(Domain).Logger.AddMessage('Real deletion of entity');
end;

procedure TCollection.RemoveViewListener(const AListener: TObject);
begin
  FListeners.Remove(AListener);
end;

procedure TCollection.SetCollectionAttributes(const AEntityClass: TEntityClass; const AContentDefinition: TDefinition);
begin
  FEntityClass := AEntityClass;
  FContentDefinition := AContentDefinition;
  if ID > 0 then
    AContentDefinition.SetID(ID)
  else if AContentDefinition.ID > 0 then
    SetID(AContentDefinition.ID);
end;

procedure TCollection.SetViewState(const Value: TViewState);
begin
  if FViewState = Value then
    Exit;
  FViewState := Value;
  NotifyListeners(dckViewStateChanged, Self);
end;

procedure TCollection.Subscribe;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FList[i].SubscribeFields;
end;

procedure TCollection.Unsubscribe;
var
  vEntity: TEntity;
begin
  for vEntity in FList do
    vEntity.RemoveAllListeners;
end;

end.
