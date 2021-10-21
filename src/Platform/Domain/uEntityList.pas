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

unit uEntityList;

interface

uses
  Classes, Generics.Collections, Generics.Defaults, uConsts, uEntity, uDefinition, uChangeManager, uQueryDef, uQuery;

type
  TEntityListFillerKind = (lfkUndefined, lfkSelector, lfkList, lfkDefinition);

  TEntityList = class
  private
    [Weak] FDomain: TObject;
    [Weak] FSession: TObject;
    [Weak] FFiller: TObject;
    [Weak] FMainDefinition: TDefinition;
    [Weak] FContentDefinitions: TList<TDefinition>;
    FList: TList<TEntity>;
    FSelection: TList<TEntity>;
    FUIListeners: TList<TObject>;
    FQuery: TQueryExecutor;
    FFillerKind: TEntityListFillerKind;
    FSortType: TEntitySortType;
    FSortFieldName: string;
    FViewState: TViewState;
    function GetCount: Integer;
    function GetEntity(const AIndex: Integer): TEntity;
    function CompareNames(const Left, Right: TEntity): Integer;
    function CompareTypesAndNames(const Left, Right: TEntity): Integer;
    function CompareField(const Left, Right: TEntity): Integer;
    function CompareIDs(const Left, Right: TEntity): Integer;
    function CompareOrders(const Left, Right: TEntity): Integer;
    procedure SubscribeList;
    procedure UnsubscribeList;
    procedure NotifyView(const AChangeKind: Word; const AParameter: TEntity);
    function GetDefinitionName: string;
  protected
    procedure DM_DomainChanged(var AMessage: TDomainChangedMessage); message DM_DOMAIN_CHANGED;
  public
    // New methods for UI handling
    function AddEntity(const AHolder: TChangeHolder; const ACollectionName: string): TEntity;
    ///todo Заглушка, нужно будет удалить, когда список начнет обрабатывать уведомления от модели
    procedure CheckAndAdd(const AEntity: TEntity);
    procedure _Remove(const AEntity: TEntity);
    // Subscription
    procedure AddUIListener(const AView: TObject);
    procedure RemoveUIListener(const AView: TObject);
    // Selection
    function SelectEntity(const AEntity: TEntity): TEntity;
    procedure SelectEntities(const AList: TList<TEntity>);
    function Selected: TEntity;
    property Selection: TList<TEntity> read FSelection;
  public
    constructor Create(const ADomain, ASession: TObject);
    destructor Destroy; override;

    procedure Add(const AEntity: TEntity);
    procedure AddFirst(const AEntity: TEntity);
    procedure Sort(const ASortType: TEntitySortType = estUserSort;
      const ASortFieldName: string = '');
    procedure Resort;
    procedure Clear;
    procedure SetFiller(const Value: TObject; const AForSelect: Boolean; const AQueryDef: TQueryDef);
    procedure SetDefinition(const Value: TObject);

    procedure LinkEntity(const AHolder: TObject; const AEntity: TEntity);
    procedure RemoveEntity(const AHolder: TObject; const AEntity: TEntity);
    procedure GetEntitiesForSelect(const ASession: TObject; const AList: TEntityList);

    function GetEnumerator: TEnumerator<TEntity>;

    property Filler: TObject read FFiller;
    property FillerKind: TEntityListFillerKind read FFillerKind;
    property MainDefinition: TDefinition read FMainDefinition;
    property DefinitionName: string read GetDefinitionName;
    property SortType: TEntitySortType read FSortType;
    property ContentDefinitions: TList<TDefinition> read FContentDefinitions;
    property Entity[const AIndex: Integer]: TEntity read GetEntity; default;
    property Count: Integer read GetCount;
    property ViewState: TViewState read FViewState;
  end;

implementation

uses
  Types, SysUtils, StrUtils, uSession, uDomain, uCollection, uObjectField;

{ TEntityList }

procedure TEntityList.CheckAndAdd(const AEntity: TEntity);
begin
  if not AEntity.IsRemote and not FList.Contains(AEntity) then
  begin
    FList.Add(AEntity);
    NotifyView(dckListAdded, AEntity);
  end;
end;

procedure TEntityList._Remove(const AEntity: TEntity);
begin
  if FList.Remove(AEntity) >= 0 then
    NotifyView(dckListRemoved, AEntity);
  FSelection.Remove(AEntity);
end;

procedure TEntityList.Add(const AEntity: TEntity);
begin
  if not AEntity.IsRemote then
    FList.Add(AEntity);
end;

function TEntityList.AddEntity(const AHolder: TChangeHolder; const ACollectionName: string): TEntity;
begin
  Result := nil;
  case FFillerKind of
    lfkSelector: Result := TDomain(FDomain)[ACollectionName].CreateNewEntity(AHolder, cNewID, TBaseField(FFiller));
    lfkList: Result := TEntity(TListField(FFiller).AddListEntity(AHolder, ACollectionName));
    lfkDefinition: Result := TDomain(FDomain)[ACollectionName].CreateNewEntity(AHolder);
  else
    Assert(Assigned(Result), 'Unknown component filled this list');
  end;
end;

procedure TEntityList.AddFirst(const AEntity: TEntity);
begin
  FList.Insert(0, AEntity);
end;

procedure TEntityList.AddUIListener(const AView: TObject);
begin
  if FUIListeners.IndexOf(AView) < 0 then
    FUIListeners.Add(AView);
end;

procedure TEntityList.Clear;
begin
  FList.Clear;
  FSelection.Clear;
end;

function TEntityList.CompareIDs(const Left, Right: TEntity): Integer;
begin
  Result := SafeID(Left) - SafeID(Right);
end;

function TEntityList.CompareNames(const Left, Right: TEntity): Integer;
begin
  Result := AnsiCompareText(SafeDisplayName(Left), SafeDisplayName(Right));
end;

function TEntityList.CompareOrders(const Left, Right: TEntity): Integer;
begin
  Result := 0;
  if Assigned(Left) <> Assigned(Right) then
  begin
    if Assigned(Left) then
      Result := 1
    else
      Result := -1;
  end
  else if Assigned(Left) then
  begin
    if not Left.FieldExists('Order') then
      Result := Left.ID - Right.ID
    else
      Result := Left['Order'] - Right['Order'];
  end;
end;

function TEntityList.CompareTypesAndNames(const Left, Right: TEntity): Integer;
begin
  Result := 0;
  if Left <> Right then
  begin
    if Assigned(Left) then
      Result := 1
    else
      Result := -1;
  end
  else if Assigned(Left) then
  begin
    Result := AnsiCompareText(Left.CollectionName, Right.CollectionName);
    if Result = 0 then
      Result := CompareNames(Left, Right);
  end;
end;

function TEntityList.CompareField(const Left, Right: TEntity): Integer;
var
  vValue1: Variant;
  vValue2: Variant;
begin
  vValue1 := Left.ExtractFieldValue(FSortFieldName);
  vValue2 := Right.ExtractFieldValue(FSortFieldName);
  Result := vValue1 - vValue2;
end;

constructor TEntityList.Create(const ADomain, ASession: TObject);
begin
  inherited Create;
  FDomain := ADomain;
  FSession := ASession;
  FList := TList<TEntity>.Create;
  FSelection := TList<TEntity>.Create;
  FUIListeners := TList<TObject>.Create;
  FQuery := nil;

  FFiller := nil;
  FFillerKind := lfkUndefined;
  FMainDefinition := nil;
  FContentDefinitions := nil;
  FSortType := estUserSort;
  FSortFieldName := '';
  FViewState := vsFullAccess;
end;

destructor TEntityList.Destroy;
begin
  UnsubscribeList;

  FreeAndNil(FQuery);
  FreeAndNil(FSelection);
  FreeAndNil(FUIListeners);
  FFiller := nil;
  FreeAndNil(FList);
  FDomain := nil;
  inherited Destroy;
end;

procedure TEntityList.DM_DomainChanged(var AMessage: TDomainChangedMessage);
var
  vEntity: TEntity;
begin
  vEntity := TEntity(AMessage.Parameter);
  if not Assigned(vEntity) then
  begin
    if AMessage.Kind = dckFilterChanged then
      NotifyView(AMessage.Kind, vEntity);
    Exit;
  end;

  if AMessage.Kind = dckEntitySaved then
  begin
    if FList.IndexOf(vEntity) >= 0 then
      NotifyView(dckEntitySaved, vEntity)
  end
  else if AMessage.Kind = dckEntityChanged then
  begin
    if FList.IndexOf(vEntity) >= 0 then
    begin
      if Assigned(FQuery) and not FQuery.IsMatch(FSession, vEntity) then
        _Remove(vEntity)
      else
        NotifyView(dckEntityChanged, vEntity);
    end
    else if Assigned(FQuery) then
    begin
      if FQuery.IsMatch(FSession, vEntity) then
        CheckAndAdd(vEntity);
    end
    else
      CheckAndAdd(vEntity);
  end
  else if AMessage.Kind = dckListRemoved then
    _Remove(vEntity)
  else if AMessage.Kind = dckListAdded then
  begin
    if FList.IndexOf(vEntity) >= 0 then
      Exit;

    if Assigned(FQuery) then
    begin
      if FQuery.IsMatch(FSession, vEntity) then
        CheckAndAdd(vEntity);
    end
    else
      CheckAndAdd(vEntity);
  end
  else if AMessage.Kind = dckViewStateChanged then
  begin
    FViewState := TEntity(AMessage.Sender).ViewState;
    NotifyView(AMessage.Kind, vEntity);
  end
  else
    NotifyView(AMessage.Kind, vEntity);
end;

function TEntityList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TEntityList.GetDefinitionName: string;
begin
  // TODO Иметь возможность задать это свойство из UI
  //   Оно может содержать дополнение к основному имени через знак ~
  Result := FMainDefinition.Name;
end;

procedure TEntityList.GetEntitiesForSelect(const ASession: TObject; const AList: TEntityList);
begin
  if FFillerKind = lfkList then
    TListField(FFiller).GetEntitiesForSelect(ASession, AList);
end;

function TEntityList.GetEntity(const AIndex: Integer): TEntity;
begin
  Result := FList[AIndex];
end;

function TEntityList.GetEnumerator: TEnumerator<TEntity>;
begin
  Result := FList.GetEnumerator;
end;

procedure TEntityList.LinkEntity(const AHolder: TObject; const AEntity: TEntity);
begin
  if FFillerKind in [lfkList, lfkSelector] then
    TListField(FFiller).LinkListEntity(AHolder, AEntity);
end;

procedure TEntityList.NotifyView(const AChangeKind: Word; const AParameter: TEntity);
var
  vMessage: TDomainChangedMessage;
  i: Integer;
  vListener: TObject;
begin
  if FUIListeners.Count = 0 then
    Exit;

  vMessage.Msg := DM_DOMAIN_CHANGED;
  vMessage.Kind := AChangeKind;
  vMessage.Sender := Self;
  vMessage.Parameter := AParameter;

  for i := FUIListeners.Count - 1 downto 0 do
  begin
    vListener := TObject(FUIListeners[i]);
    vListener.Dispatch(vMessage);
  end;
end;

procedure TEntityList.RemoveEntity(const AHolder: TObject; const AEntity: TEntity);
//HOLDER - записываем изменения в ParentHolder
begin
  TUserSession(FSession).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
    begin
      case FFillerKind of
        lfkList: TListField(FFiller).RemoveListEntity(AHolder, AEntity);
        lfkDefinition: AEntity.Delete(AHolder);
        lfkSelector: AEntity.Delete(AHolder);
      end;
      Result := True;
    end, TChangeHolder(AHolder));
end;

procedure TEntityList.RemoveUIListener(const AView: TObject);
var
  vIndex: Integer;
begin
  vIndex := FUIListeners.IndexOf(AView);
  if vIndex >= 0 then
    FUIListeners.Delete(vIndex);
end;

procedure TEntityList.Resort;
begin
  if FList.Count > 1 then
    Sort(FSortType, FSortFieldName);
end;

function TEntityList.Selected: TEntity;
begin
  if FSelection.Count > 0 then
    Result := TEntity(FSelection[0])
  else
    Result := nil;
end;

procedure TEntityList.SelectEntities(const AList: TList<TEntity>);
var
  vEntity: TEntity;
  vSameList: Boolean;
begin
  vSameList := FSelection.Count = AList.Count;
  if vSameList then
    for vEntity in FSelection do
    begin
      vSameList := AList.Contains(vEntity);
      if not vSameList then
        Break;
    end;

  if vSameList then
    Exit;

  FSelection.Clear;
  FSelection.AddRange(AList);
  NotifyView(dckSelectionChanged, Selected);
end;

function TEntityList.SelectEntity(const AEntity: TEntity): TEntity;
begin
  Result := Selected;

  if AEntity = Result then
    Exit;

  FSelection.Clear;
  if Assigned(AEntity) then
    FSelection.Add(AEntity);
  NotifyView(dckSelectionChanged, AEntity);
end;

procedure TEntityList.SetDefinition(const Value: TObject);
begin
  Assert(Assigned(Value), 'Invalid definition for entity list');

  if Value is TDefinition then
  begin
    FMainDefinition := TDefinition(Value);
    FContentDefinitions := FMainDefinition.InnerDefinitions;
  end
  else if Value is TObjectFieldDef then
  begin
    FMainDefinition := TDefinition(TObjectFieldDef(Value)._ContentDefinition);
    FContentDefinitions := TObjectFieldDef(Value).ContentDefinitions;
    FSortType := TObjectFieldDef(Value).SortType;
  end;
end;

procedure TEntityList.SetFiller(const Value: TObject; const AForSelect: Boolean;
  const AQueryDef: TQueryDef);
begin
  Assert(Assigned(Value), 'Invalid Filler for entity list');
  if FFiller = Value then
    Exit;

  // Отписать предыдущего
  UnsubscribeList;

  FFiller := Value;

  if FFiller is TDefinition then
  begin
    FFillerKind := lfkDefinition;
    FMainDefinition := TDefinition(FFiller);
    FContentDefinitions := FMainDefinition.InnerDefinitions;
  end
  else if Value is TListField then
  begin
    if AForSelect then
      FFillerKind := lfkSelector
    else
      FFillerKind := lfkList;
    FMainDefinition := TDefinition(TListField(FFiller).ContentDefinition);
    FContentDefinitions := TListField(FFiller).ContentDefinitions;
  end
  else begin
    FFillerKind := lfkSelector;
    FMainDefinition := TDefinition(TEntityField(FFiller).ContentDefinition);
    FContentDefinitions := FMainDefinition.InnerDefinitions;
  end;

  SubscribeList;

  // Заполнение фильтра для списка
  if Assigned(AQueryDef) then
  begin
    FQuery := TQueryExecutor.Create(AQueryDef);
    if FFillerKind in [lfkSelector, lfkList] then
      FQuery.SetParameters(FSession, TBaseField(FFiller).OwnerInstance)
    else
      FQuery.SetParameters(FSession, nil);
  end
  else
    FQuery := nil;
end;

procedure TEntityList.Sort(const ASortType: TEntitySortType = estUserSort;
  const ASortFieldName: string = '');
var
  vHasNullForSelect: Boolean;

  function ExtractFieldName(const AFieldName: string): string;
  var
    vPos: Integer;
  begin
    Result := AFieldName;
    vPos := AFieldName.LastIndexOf('.') + 1;
    if vPos > 0 then
      Delete(Result, vPos, Length(AFieldName) - vPos + 1);
    Result := Result + '.ID';
  end;

begin
  if FList.Count <= 1 then
    Exit;

  vHasNullForSelect := (FFillerKind = lfkSelector) and not Assigned(GetEntity(0));
  if vHasNullForSelect then
    FList.Delete(0);

  FSortType := ASortType;
  case FSortType of
    estUserSort: {do nothing};
    estSortByID: FList.Sort(TComparer<TEntity>.Construct(CompareIDs));
    estSortByName: FList.Sort(TComparer<TEntity>.Construct(CompareNames));
    estSortByTypeAndName: FList.Sort(TComparer<TEntity>.Construct(CompareTypesAndNames));
    estSortByColorFieldID:
      if ASortFieldName <> '' then
      begin
        FSortFieldName := ExtractFieldName(ASortFieldName);
        FList.Sort(TComparer<TEntity>.Construct(CompareField));
      end;
    estSortByOrder: FList.Sort(TComparer<TEntity>.Construct(CompareOrders));
  else
    Assert(False, 'The sorting isn''t supported');
  end;

  if vHasNullForSelect then
    AddFirst(nil);
end;

procedure TEntityList.SubscribeList;
var
  i: Integer;
  vCollection: TCollection;
begin
  if not Assigned(FFiller) then
    Exit;

  for i := 0 to FContentDefinitions.Count - 1 do
  begin
    vCollection := TDomain(FDomain)[TDefinition(FContentDefinitions[i]).Name];
    vCollection.AddViewListener(Self);
  end;
end;

procedure TEntityList.UnsubscribeList;
var
  i: Integer;
  vCollection: TCollection;
begin
  if not Assigned(FFiller) then
    Exit;

  for i := 0 to FContentDefinitions.Count - 1 do
  begin
    vCollection := TDomain(FDomain)[TDefinition(FContentDefinitions[i]).Name];
    vCollection.RemoveViewListener(Self);
  end;

  FreeAndNil(FQuery);
end;

end.
