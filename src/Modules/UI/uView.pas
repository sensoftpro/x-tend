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

unit uView;

interface

uses
  Classes, Generics.Collections, uFastClasses, uConsts, uEntity, uObjectField, uComplexObject;

type
  TDefinitionKind = (dkUndefined, dkDomain, dkNavigation, dkCollection, dkEntity, dkAction,
    dkListField, dkObjectField, dkSimpleField, dkComplexField);

type
  TView = class
  private
    [Weak] FInteractor: TObject;
    [Weak] FSession: TObject;
    [Weak] FDomain: TObject;
    [Weak] FParent: TView;
    FInitialName: string;
    FName: string;
    FFullName: string;
    FFilter: string;
    FQuery: TStrings;
    FDomainObject: TObject;
    [Weak] FDefinition: TObject;
    FDefinitionKind: TDefinitionKind;
    FItems: TObjectStringList<TView>;
    FListeners: TList<TObject>;
    FState: TViewState;
    FUIContext: string;
    FFreezeCount: Integer;
    procedure ExtractViewParams(const AName: string);
    function ExtractAction(const ADefinition: TObject; const AItemName: string): Boolean;
    function ExtractField(const ADefinition: TObject; const AName: string): Boolean;
    function IsContextDependent: Boolean;
    procedure DoParentChanged(const AParentDomainObject: TObject);
    procedure UpdateUIState;
    procedure UpdateChildViews(const ADefinitionKind: TDefinitionKind = dkUndefined);
    function GetParentDomainObject: TObject;
    function GetFieldValue: Variant;
    function GetFieldEntity: TEntity;
    function GetFieldStream: TStream;
    function GetStateCaption: string;
    function GetFieldObject: TComplexObject;
    procedure RemoveView(const AView: TView);
    function SetDomainEntity(const AEntity: TEntity): Boolean;
    procedure SubscribeField(const AParentEntity: TEntity);
    procedure UnsubscribeField(const AParentEntity: TEntity);
    procedure SubscribeDomainObject;
    procedure UnsubscribeDomainObject;
    procedure DM_DomainChanged(var AMessage: TDomainChangedMessage); message DM_DOMAIN_CHANGED;
  public
    constructor Create(const AInteractor: TObject; const AParent: TView; const AName: string);
    destructor Destroy; override;

    function GetEnumerator: TEnumerator<TView>;
    function ViewByName(const AName: string): TView;

    function BuildView(const AViewPath: string): TView;
    procedure CleanView;

    procedure AddListener(const AListener: TObject);
    procedure RemoveListener(const AListener: TObject);
    procedure RefreshView(const AParameter: TObject);
    procedure NotifyUI(const AKind: Word; const AParameter: TObject);
    procedure ElevateAccess;

    procedure ExecuteAction(const AParentHolder: TObject);
    function ExtractListField: TListField;
    function ExtractEntityField: TEntityField;
    function GetField: TBaseField; inline;

    procedure LoadActionState(const AEntity: TEntity);
    procedure StoreActionState(const AField: TBaseField);

    procedure SetDefinition(const ADefinition: TObject);
    function SetDomainObject(const Value: TObject): Boolean;
    procedure SetFieldValue(const AHolder: TObject; const AValue: Variant);
    procedure SetFieldEntity(const AHolder: TObject; const AEntity: TEntity);
    procedure SetFieldStream(const AHolder: TObject; const AStream: TStream);
    property FieldValue: Variant read GetFieldValue;
    property FieldStream: TStream read GetFieldStream;
    property FieldEntity: TEntity read GetFieldEntity;
    property FieldObject: TComplexObject read GetFieldObject;
    property StateCaption: string read GetStateCaption;

    function TextHierarchy(const AIndent: string = ''; const ACurrentView: TView = nil): string;
    function Description: string;

    procedure AddParameter(const AKey, AValue: string);
    function QueryParameter(const AName: string; const ADefaultValue: string = ''): string;
    function QueryText: string;

    property FullName: string read FFullName;
    property Name: string read FName;
    property InitialName: string read FInitialName;
    property UIContext: string read FUIContext;
    property Definition: TObject read FDefinition;
    property DefinitionKind: TDefinitionKind read FDefinitionKind;
    property ParentDomainObject: TObject read GetParentDomainObject;
    property DomainObject: TObject read FDomainObject;
    property Interactor: TObject read FInteractor;
    property Session: TObject read FSession;
    property Domain: TObject read FDomain;
    property Parent: TView read FParent;
    property State: TViewState read FState write FState;
  end;

type
  TCheckActionFlagsFunc = function(const AView: TView): TViewState of object;

implementation

uses
  Types, Variants, SysUtils, uUtils, uInteractor, uDomain, uConfiguration, uDefinition,
  uSession, uEntityList, uCollection, uSimpleField, uChangeManager, uEnumeration, uSettings, uJSON;

type
  TExecuteActionFunc = function(const AView: TView; const AParentHolder: TChangeHolder): Boolean of object;

const
  cDefinitionKindNames: array[TDefinitionKind] of string = (
    '? ? ?', 'Domain', 'Navigation', 'Collection', 'Entity', 'Action', 'List field',
    'Object field', 'Simple field', 'Complex field');

{ TView }

procedure TView.AddListener(const AListener: TObject);
begin
  if FListeners.IndexOf(AListener) < 0 then
    FListeners.Add(AListener);
end;

procedure TView.AddParameter(const AKey, AValue: string);
begin
  FQuery.Values[AKey] := AValue;
end;

function TView.BuildView(const AViewPath: string): TView;
var
  vViewPath: TStrings;
  i: Integer;
  vViewName: string;
  vView: TView;
  vRootView: TView;
begin
  // Удаление дублирующих и окружающих путь знаков '/'
  vViewPath := CreateDelimitedList(AViewPath, '/');
  for i := vViewPath.Count - 1 downto 0 do
    if Trim(vViewPath[i]) = '' then
      vViewPath.Delete(i);
  try
    Result := Self;
    for i := 0 to vViewPath.Count - 1 do
    begin
      vViewName := Trim(vViewPath[i]);
      if vViewName = '$' then
        Continue
      else if vViewName = '~' then
      begin
        Result := TInteractor(FInteractor).RootView;
        Continue;
      end;

      vView := Result.ViewByName(vViewName);
      if not Assigned(vView) then
      begin
        vView := TView.Create(FInteractor, Result, vViewName);
        { TODO -owa : REDO: Если View не найдено в контексте, оно ищется в корне }
        // !!! Нужен облегченный метод проверки на возможность создания в контексте
        if (vView.DefinitionKind = dkUndefined) and (i = 0) then
        begin
          FreeAndNil(vView);
          vRootView := TInteractor(FInteractor).RootView;
          vView := vRootView.ViewByName(vViewName);
          if not Assigned(vView) then
          begin
            vView := TView.Create(FInteractor, vRootView, vViewName);
            vRootView.FItems.AddObject(vViewName, vView);
          end;
        end
        else
          Result.FItems.AddObject(vViewName, vView);
      end;
      Result := vView;
    end;
  finally
    FreeAndNil(vViewPath);
  end;
end;

procedure TView.CleanView;
begin
  // "Живое" view
  if FListeners.Count > 0 then
    Exit;

  // Промежуточное view
  if FItems.Count > 0 then
    Exit;

  // Корневое view
  if not Assigned(FParent) then
    Exit;

  FParent.RemoveView(Self);
end;

constructor TView.Create(const AInteractor: TObject; const AParent: TView; const AName: string);
begin
  inherited Create;

  FInteractor := AInteractor;
  FSession := TInteractor(AInteractor).Session;
  FDomain := TInteractor(AInteractor).Domain;
  FParent := AParent;
  FDefinition := nil;
  FDomainObject := nil;
  FQuery := CreateDelimitedList('', '&');
  FItems := TObjectStringList<TView>.Create;
  FListeners := TList<TObject>.Create;
  FFullName := '';
  FFilter := '';
  FFreezeCount := 0;

  if not Assigned(FParent) then
  begin
    FName := '';
    FUIContext := '';
    FDefinitionKind := dkDomain;
    FState := vsFullAccess;
  end
  else
    ExtractViewParams(AName);
end;

function TView.Description: string;
var
  vDescription: string;
  vParentObject: TObject;
  vField: TBaseField;
  vValue: Variant;

  function GetListDescription(const AList: TEntityList): string;
  begin
    if not Assigned(AList) then
      Result := '???'
    else begin
      if not Assigned(AList.MainDefinition) then
        Result := '[???]'
      else
        Result := '[' + AList.MainDefinition.Name + ']';
    end;
  end;

  function GetEntityDescription(const AEntity: TEntity): string;
  begin
    if not Assigned(AEntity) then
      Result := '???'
    else if not Assigned(TEntity(AEntity).Collection) then
      Result := TEntity(AEntity).CollectionName
    else
      Result := AEntity.ToString;
  end;

begin
  case FDefinitionKind of
    dkUndefined: vDescription := ' - ???';
    dkDomain: vDescription := '';
    dkCollection, dkListField:
      Result := ' - ' + GetListDescription(TEntityList(FDomainObject));
    dkEntity, dkObjectField: vDescription := ' - ' + GetEntityDescription(TEntity(FDomainObject));
    dkAction:
      begin
        vParentObject := GetParentDomainObject;
        if not Assigned(vParentObject) then
          vDescription := ' - CTX: domain'
        else if vParentObject is TEntityList then
          vDescription := ' - CTX: ' + GetListDescription(TEntityList(vParentObject))
        else
          vDescription := ' - CTX: ' + GetEntityDescription(TEntity(vParentObject));

        if not Assigned(FDomainObject) then
          vDescription := vDescription + ', < no params >'
        else
          vDescription := vDescription + ', PARAMETERS';
      end;
    dkSimpleField:
      try
        vValue := GetFieldValue;
        if VarIsStr(vValue) then
        begin
          vDescription := VarToStr(vValue);
          if Length(vDescription) > 1000 then
            vDescription := Copy(vDescription, 1, 1000) + '...';
          vDescription := ' - ' + vDescription;
        end
        else
          vDescription := ' - ' + VarToStr(vValue);
      except
        vDescription := ' - can''t convert';
      end;
    dkComplexField:
      begin
        vField := GetField;
        if Assigned(vField) then
          vDescription := TComplexFieldDef(vField.FieldDef).ObjectKindName
        else
          vDescription := '[empty]'
      end;
  end;

  Result := cViewStateNames[FState] + '(' + IntToStr(FListeners.Count) + ') ' + FName + ': ' + cDefinitionKindNames[FDefinitionKind] + vDescription;
end;

destructor TView.Destroy;
var
  vParentObject: TEntity;
begin
  // Нужно отписывать здесь, так как если делать отписку у родительского объекта, View для поля уже будет удалено
  if FDefinitionKind in [dkListField, dkObjectField, dkSimpleField, dkComplexField] then
  begin
    vParentObject := TEntity(GetParentDomainObject);
    UnSubscribeField(vParentObject);
  end;

  UnsubscribeDomainObject;
  FreeAndNil(FListeners);

  if Assigned(FDomainObject) then
  begin
    if FDefinitionKind = dkAction then
    begin
      if Assigned(TEntity(FDomainObject)) then
        TEntity(FDomainObject).UnsubscribeFields(TDomain(FDomain).DomainHolder);
      FreeAndNil(TEntity(FDomainObject));
    end
    else if FDefinitionKind in [dkCollection, dkListField] then
      FreeAndNil(TEntityList(FDomainObject))
    else
      FDomainObject := nil;
  end;

  FreeAndNil(FItems);
  FreeAndNil(FQuery);

  inherited Destroy;
end;

procedure TView.DM_DomainChanged(var AMessage: TDomainChangedMessage);
var
  vSelectedView: TView;
  vActiveView: TView;
begin
  if AMessage.Kind = dckSelectionChanged then
  begin
    Assert(FDefinitionKind in [dkListField, dkCollection]);
    vSelectedView := ViewByName('Selected');
    if Assigned(vSelectedView) then
      vSelectedView.DoParentChanged(FDomainObject);
    Exit;
  end
  else if Assigned(AMessage.Holder) and Assigned(TChangeHolder(AMessage.Holder).ViewList) then
  begin
    if TChangeHolder(AMessage.Holder).ViewList.IndexOf(Self) < 0 then
    begin
      TChangeHolder(AMessage.Holder).ViewList.Add(Self);
      Inc(FFreezeCount);
    end;
    Exit;
  end
  else if AMessage.Kind = dckListEndUpdate then
  begin
    Dec(FFreezeCount);
    Assert(FFreezeCount >= 0, 'Неправильное количество обновлений');

    if FFreezeCount = 0 then
      NotifyUI(dckListEndUpdate, nil);
    Exit;
  end
  else if FFreezeCount > 0 then
    Exit;

  // Обработка изменений модели
  if AMessage.Kind = dckFieldChanged then
  begin
    Assert(FDefinitionKind in [dkObjectField, dkSimpleField, dkComplexField]);
    // Рекурсивное обновление иерархии View
    if FDefinitionKind = dkObjectField then
      SetDomainObject(AMessage.Parameter);

    UpdateUIState;
    TInteractor(FInteractor).PrintHierarchy;

    NotifyUI(dckFieldChanged, nil);
  end
  else if AMessage.Kind = dckEntityChanged then
  begin
    NotifyUI(AMessage.Kind, AMessage.Parameter);
    if FDefinitionKind = dkObjectField then
      UpdateChildViews(dkAction);

    if not (FDefinitionKind in [dkListField, dkCollection]) then
      Exit;

    for vActiveView in FItems.Objects do
    begin
      if vActiveView.DefinitionKind <> dkEntity then
        Continue;
      if vActiveView.DomainObject = AMessage.Parameter then
      begin
        vActiveView.UpdateChildViews(dkAction);
        TInteractor(FInteractor).PrintHierarchy;
      end;
    end;
  end
  else if AMessage.Kind = dckEntitySaved then
  begin
    if FDefinitionKind in [dkListField, dkCollection] then
      NotifyUI(AMessage.Kind, AMessage.Parameter);
  end
  else if AMessage.Kind in [dckListAdded, dckListRemoved] then
  begin
    ///todo сейчас эти уведомления приходят еще и от коллекций
    if FDefinitionKind = dkObjectField then
      Exit;

    Assert(FDefinitionKind in [dkListField, dkCollection]);
    if (FDefinitionKind = dkListField) and (AMessage.Kind = dckListAdded) then
    begin
      for vActiveView in FItems.Objects do
      begin
        if (vActiveView.DefinitionKind = dkEntity) {and (vActiveView.DomainObject = AMessage.Parameter)} then
          vActiveView.DoParentChanged(FDomainObject);
      end;
      TInteractor(FInteractor).PrintHierarchy;
    end;

    NotifyUI(AMessage.Kind, AMessage.Parameter);
  end
  else if AMessage.Kind = dckEntityDeleted then
  begin
    Assert(FDefinitionKind in [dkObjectField, dkEntity]);
    SetDomainObject(nil);
  end
  else if AMessage.Kind = dckViewStateChanged then
    RefreshView(AMessage.Parameter)
  else if AMessage.Kind = dckNameChanged then
  begin
    if FDefinitionKind = dkObjectField then
      UpdateChildViews(dkAction);
    NotifyUI(AMessage.Kind, AMessage.Parameter);
  end
  else
    NotifyUI(AMessage.Kind, AMessage.Parameter);
end;

procedure TView.DoParentChanged(const AParentDomainObject: TObject);
var
  vListField: TListField;
  vTextID, vSearchField: string;
  vId: Integer;
  vChanged: Boolean;
  vChangeKind: Word;
  vInteractor: TInteractor;
  vCollection: TCollection;
  i: Integer;
  vEntityList: TEntityList;
  vEntity: TEntity;
begin
  vInteractor := TInteractor(FInteractor);

  vChanged := False;
  vChangeKind := dckEntityChanged;
  if FDefinitionKind = dkCollection then
  begin
    // При очистке Selected устанавливается в nil
    TDomain(vInteractor.Domain).GetEntityList(vInteractor.Session,
      TDefinition(FDefinition), TEntityList(FDomainObject), FFilter);
    vChanged := True;
  end
  else if FDefinitionKind = dkEntity then
  begin
    vChanged := True;
    if SameText(FName, 'Current') then
    begin
      if TDefinition(FDefinition).IsDescendantOf('SysUsers') then
        SetDomainEntity(TUserSession(vInteractor.Session).CurrentUser)
      else if (TDefinition(FDefinition).Name = 'SysConstants') or (TDefinition(FDefinition).Name = 'SysTemporaries')
          or (TDefinition(FDefinition).Name = 'Settings') or TDefinition(FDefinition).HasFlag(ccLocalOnly)
          or TDefinition(FDefinition).HasFlag(ccNotSave)
      then
        SetDomainEntity(TDomain(vInteractor.Domain).FirstEntity(TDefinition(FDefinition).Name))
      else
        SetDomainEntity(TDomain(vInteractor.Domain).FirstEntity(TDefinition(FDefinition).Name));
        //Assert(False, Format('Property "Current" doesn''t supported for collection [%s]', [TDefinition(FDefinition).Name]));
    end
    else if Pos('New', FName) = 1 then
    begin
      if FName <> 'New' then
      begin
        vTextID := Copy(FName, 4, Length(FName) - 3);
        vId := StrToIntDef(vTextID, 0);
        if vId > 0 then
          SetDomainEntity(TDomain(vInteractor.Domain).EntityByID(TDefinition(FDefinition).Name, -vId))
        else
          SetDomainEntity(nil);
      end;
    end
    else if Pos('Index', FName) = 1 then
    begin
      if FName <> 'Index' then
      begin
        vTextID := Copy(FName, 6, Length(FName) - 5);
        vId := StrToIntDef(vTextID, 0);
        vCollection := TDomain(vInteractor.Domain).CollectionByName(TDefinition(FDefinition).Name);
        if (vId > -1) and (vId < vCollection.Count) then
          SetDomainEntity(TDomain(vInteractor.Domain).CollectionByName(TDefinition(FDefinition).Name)[vId])
        else
          SetDomainEntity(nil);
      end;
    end
    else if (Pos('$', FName) = 1) and (Length(FName) > 1) then
    begin
      vTextID := Copy(FName, 2, Length(FName) - 1);
      vID := StrToIntDef(vTextID, -1);
      vCollection := TDomain(vInteractor.Domain).CollectionByName(TDefinition(FDefinition).Name);
      if (vID >= 0) and (vCollection.Count > vID) then
        SetDomainEntity(vCollection.Entities[vID])
      else
        SetDomainEntity(nil);
    end
    else if Pos('@', FName) = 1 then
    begin
      vTextID := Copy(FName, 2, Length(FName) - 1);
      vEntityList := TEntityList(AParentDomainObject);

      if vEntityList.MainDefinition.FieldExists('Code') then
        vSearchField := 'Code'
      else
        vSearchField := 'Name';

      vEntity := nil;
      for i := 0 to vEntityList.Count - 1 do
        if SameText(vEntityList[i][vSearchField], vTextID) then
        begin
          vEntity := vEntityList[i];
          Break;
        end;

      SetDomainEntity(vEntity);
    end
    else if SameText(FName, 'Selected') then
      SetDomainEntity(TEntity(TEntityList(AParentDomainObject).Selected))
    else begin
      if FParent.DefinitionKind = dkListField then
      begin
        vId := StrToIntDef(FName, -1);
        if (vId >= 0) and (TEntityList(AParentDomainObject).Count > vId) then
          vChanged := SetDomainEntity(TEntityList(AParentDomainObject).Entity[vId])
        else
          SetDomainEntity(nil);
      end
      else begin
        vId := StrToIntDef(FName, 0);
        if vId > 0 then
          SetDomainEntity(TDomain(vInteractor.Domain).EntityByID(TDefinition(FDefinition).Name, vId))
        else
          SetDomainEntity(nil);
      end;
    end;
  end
  else if FDefinitionKind = dkAction then
    vChanged := True
  else if FDefinitionKind = dkObjectField then
  begin
    if Assigned(AParentDomainObject) then
      SetDomainObject(TEntity(AParentDomainObject).ExtractEntity(TFieldDef(FDefinition).Name))
    else
      SetDomainObject(nil);
    vChanged := True;
  end
  else if FDefinitionKind = dkListField then
  begin
    if not Assigned(AParentDomainObject) then
      TEntityList(FDomainObject).Clear
    else begin
      vListField := TListField(TEntity(AParentDomainObject).FieldByName(TFieldDef(FDefinition).Name));
      vListField.GetEntityList(vInteractor.Session, FDomainObject);
    end;
    vChangeKind := dckFieldChanged;
    vChanged := True;
  end
  else if FDefinitionKind = dkSimpleField then
    vChanged := True
  else if FDefinitionKind = dkComplexField then
    vChanged := True;

  if not vChanged then
    Exit;

  UpdateUIState;

  NotifyUI(vChangeKind, nil);
  UpdateChildViews;
end;

procedure TView.ExecuteAction(const AParentHolder: TObject);
var
  vInteractor: TInteractor;
begin
  Assert(FDefinitionKind = dkAction, 'Попытка вызова на исполнение не предназначенного для этого объекта');

  vInteractor := TInteractor(FInteractor);
  try
    TExecuteActionFunc(TConfiguration(vInteractor.Configuration).ExecuteActionFunc)(Self, TChangeHolder(AParentHolder));
  except
    on E: Exception do
      raise Exception.Create(Format('Ошибка при выполнении [%s]. Причина: %s', [FName, E.Message]));
  end;
end;

function TView.ExtractAction(const ADefinition: TObject; const AItemName: string): Boolean;
begin
  FDefinition := nil;
  if Assigned(ADefinition) then
  begin
    FDefinition := TDefinition(ADefinition).ActionByName(AItemName);
    if not Assigned(FDefinition) then
      FDefinition := TDefinition(ADefinition).ReportByName(AItemName);
  end;

  if not Assigned(FDefinition) then
    FDefinition := TConfiguration(TInteractor(FInteractor).Configuration).Actions.ObjectByName(AItemName);

  //if not Assigned(FDefinition) then
  //  FDefinition := TConfiguration(TInteractor(FInteractor).Configuration).Applications.ObjectByName(AItemName);

  Result := Assigned(FDefinition);
  if Result then
    FDefinitionKind := dkAction
  else
    FDefinitionKind := dkUndefined;
end;

function TView.ExtractField(const ADefinition: TObject; const AName: string): Boolean;
begin
  FDefinition := TDefinition(ADefinition).FieldByName(AName);
  if not Assigned(FDefinition) then
    FDefinition := TDefinition(ADefinition).ServiceFields.ObjectByName(AName);

  Result := Assigned(FDefinition);
  if Result then
  begin
    case TFieldDef(FDefinition).Kind of
      fkObject: FDefinitionKind := dkObjectField;
      fkList: FDefinitionKind := dkListField;
      fkComplex: FDefinitionKind := dkComplexField;
    else
      FDefinitionKind := dkSimpleField;
    end;
  end
  else
    FDefinitionKind := dkUndefined;
end;

function TView.ExtractListField: TListField;
begin
  Result := TListField(GetField);
end;

function TView.ExtractEntityField: TEntityField;
begin
  Result := TEntityField(GetField);
end;

procedure TView.ExtractViewParams(const AName: string);
var
  vPos: Integer;
  vParentDefinition: TObject;
  vParentObject: TEntity;
  vInteractor: TInteractor;
  vPostfix: string;

  function GetParentFieldDefinition(const ABaseDefinition: TObject; const AEntity: TEntity): TDefinition;
  begin
    Result := TDefinition(ABaseDefinition);
    if not Assigned(Result) then
      Exit;

    // Когда у нас несколько наследников, только по определению нам не определить точный тип => Уточняем тип
    if Result.Descendants.Count > 0 then
    begin
      if Assigned(AEntity) then
        Result := AEntity.Definition;
    end;
  end;
begin
  vInteractor := TInteractor(FInteractor);

  // Получаем имя, по которому идентифицируем определение
  vPos := Pos('?', AName);
  if vPos > 0 then
  begin
    FName := Copy(AName, 1, vPos - 1);
    if Length(AName) - vPos >= 2 then
      FQuery.DelimitedText := Copy(AName, vPos + 1, Length(AName) - vPos);
  end
  else
    FName := AName;

  // Убираем из имени числовой идентификатор
  FInitialName := FName;
  vPos := Pos('~', FName);
  if vPos > 0 then
  begin
    vPostfix := FName;
    FName := Copy(FName, 1, vPos - 1);
    Delete(vPostfix, 1, vPos);
    if StrToIntDef(vPostfix, -999) = -999 then
      FFilter := Trim(vPostfix);
  end;

  // Получаем параметры действия
  vPos := Pos('#', FName, 2);
  if vPos > 0 then
  begin
    FUIContext := FName;
    Delete(FUIContext, 1, vPos);
    FName := Copy(FName, 1, vPos - 1);
  end
  else
    FUIContext := '';

  // 1. Получаем определение для этого View
  vParentDefinition := FParent.Definition;
  vParentObject := TEntity(GetParentDomainObject);
  FDefinition := nil;
  FDomainObject := nil;

  case FParent.DefinitionKind of
    dkDomain:
      if not ExtractAction(nil, FName) then
      begin
        FDefinition := TConfiguration(vInteractor.Configuration).DefinitionByName[FName];
        if Assigned(FDefinition) then
          FDefinitionKind := dkCollection
        else
          FDefinitionKind := dkUndefined;
      end;
    dkCollection:
      begin
        vParentDefinition := TDefinition(vParentDefinition);
        if not ExtractAction(vParentDefinition, FName) then
        begin
          if (FName = 'Selected') or (FName = 'Current') or (Pos('New', FName) = 1) or (Pos('Index', FName) = 1) or (Pos('$', FName) = 1) or (StrToIntDef(FName, 0) > 0) then
          begin
            FDefinition := vParentDefinition;
            FDefinitionKind := dkEntity;
          end
          else
            FDefinitionKind := dkUndefined;
        end;
      end;
    dkEntity:
      begin
        // Мы не можем доверять типу сущности, так как он может быть общим
        vParentDefinition := GetParentFieldDefinition(vParentDefinition, vParentObject);
        if FName = '[]' then
        begin
          if vParentDefinition = TConfiguration(vInteractor.Configuration).RootDefinition then
          begin
            FDefinitionKind := dkCollection;
            if Assigned(vParentObject) and (vParentObject is TCollection) then
              FDefinition := TCollection(vParentObject).ContentDefinition;
          end
          else
            FDefinitionKind := dkUndefined;
        end
        else if not ExtractAction(vParentDefinition, FName) then
          ExtractField(vParentDefinition, FName);
      end;
    dkAction:
    // всегда простая свежеинстанцированная сущность
      if not ExtractAction(vParentDefinition, FName) then
        ExtractField(vParentDefinition, FName);
    dkObjectField:
      begin
        vParentDefinition := GetParentFieldDefinition(TObjectFieldDef(vParentDefinition)._ContentDefinition, vParentObject);
        if FName = '[]' then
        begin
          if vParentDefinition = TConfiguration(vInteractor.Configuration).RootDefinition then
          begin
            FDefinitionKind := dkCollection;
            if Assigned(vParentObject) and (vParentObject is TCollection) then
              FDefinition := TCollection(vParentObject).ContentDefinition;
          end
          else
            FDefinitionKind := dkUndefined;
        end
        else if not ExtractAction(vParentDefinition, FName) then
          ExtractField(vParentDefinition, FName);
      end;
    dkListField:
      begin
        { TODO -owa : Возможно, здесь понадобится точное определение типа }
        vParentDefinition := TObjectFieldDef(vParentDefinition)._ContentDefinition;
        if (FName = 'Selected') or (StrToIntDef(FName, -1) >= 0 {поиск по индексу}) or (Pos('@', FName) = 1 {поиск по Name}) then
        begin
          FDefinitionKind := dkEntity;
          FDefinition := vParentDefinition;
        end
        else
          ExtractAction(vParentDefinition, FName);
      end;
    dkComplexField:
      ExtractAction(vParentDefinition, FName);
  else
    Assert(False, 'Unsupported type, control [' + FName + ']');
  end;

  if FParent.FullName <> '' then
    FFullName := FParent.FullName + '/' + FInitialName
  else
    FFullName := FInitialName;

  if FDefinitionKind = dkUndefined then
    Exit;

  // 2. Создание объектов для поддержки UI
  if FDefinitionKind in [dkCollection, dkListField] then
  begin
    FDomainObject := TEntityList.Create(vInteractor.Domain, vInteractor.Session);
    TEntityList(FDomainObject).SetDefinition(FDefinition);
    SubscribeDomainObject;
  end
  else if FDefinitionKind = dkAction then
  begin
    if TActionDef(FDefinition).Fields.Count > 0 then
    begin
      FDomainObject := TEntity.Create(vInteractor.Domain, TActionDef(FDefinition));

      LoadActionState(TEntity(FDomainObject));

      TEntity(FDomainObject).SubscribeFields(TDomain(FDomain).DomainHolder);

      TEntityCreationProc(TDomain(FDomain).Configuration.AfterEntityCreationProc)(TDomain(FDomain).DomainHolder,
        vParentObject, TEntity(FDomainObject));
    end;
    SubscribeDomainObject;
  end
  else if (FDefinitionKind in [dkEntity, dkObjectField]) then
  begin
    if (FDefinitionKind = dkEntity) then
    begin
      if FName = 'New' then
        FDomainObject := TDomain(vInteractor.Domain).CreateNewEntity(TDomain(FDomain).DomainHolder, TDefinition(FDefinition).Name);
    end;

    SubscribeDomainObject;
  end;

  if FDefinitionKind in [dkListField, dkObjectField, dkSimpleField, dkComplexField] then
    SubscribeField(vParentObject);

  // 3. Обновляем состояние доменных объектов
  DoParentChanged(vParentObject);
end;

procedure TView.ElevateAccess;
begin
  FState := vsFullAccess;

  NotifyUI(dckViewStateChanged, nil);
  UpdateChildViews;
  TInteractor(FInteractor).PrintHierarchy;
end;

function TView.IsContextDependent: Boolean;
var
  vOwnerView: TView;
  vListFieldDef: TListFieldDef;
  vFieldDef: TEntityFieldDef;
  vEntity: TEntity;
  vDependencies: TStrings;
  i: Integer;
  vMyFieldName: string;
  vMasterFieldName: string;
  vMasterField: TEntityField;
begin
  Result := False;
  if FDefinitionKind <> dkObjectField then
    Exit;

  if not Assigned(FParent) then
    Exit;

  if FParent.DefinitionKind = dkObjectField then
  begin
    vFieldDef := TEntityFieldDef(FParent.Definition);
    Result := vFieldDef.SelectiveFields.IndexOfName(TEntityFieldDef(FDefinition).Name) >= 0;
    Exit;
  end;

  if not (FParent.DefinitionKind in [dkAction, dkEntity]) then
    Exit;

  vFieldDef := TEntityFieldDef(FDefinition);
  vDependencies := vFieldDef.SelectiveFields;
  if (vDependencies.Count > 0) and Assigned(ParentDomainObject) then
  begin
    vEntity := TEntity(ParentDomainObject);
    for i := 0 to vDependencies.Count - 1 do
    begin
      vMyFieldName := vDependencies.Names[i];
      vMasterFieldName := vDependencies.ValueFromIndex[i];
      if vMasterFieldName <> '' then
      begin
        vMasterField := TEntityField(vEntity.FieldByName(vMasterFieldName));
        if Assigned(vMasterField) then
        begin
          Result := not Assigned(vMasterField.Entity);
          if Result then
            Exit;
        end;
      end;
    end;
  end;

  vOwnerView := FParent.Parent;
  if not Assigned(vOwnerView) then
    Exit;
  if vOwnerView.DefinitionKind <> dkListField then
    Exit;

  vListFieldDef := TListFieldDef(vOwnerView.Definition);
  if not Assigned(vListFieldDef) then
    Exit;

  Result := TEntityFieldDef(FDefinition).Name = vListFieldDef.MasterFieldName;
end;

procedure TView.LoadActionState(const AEntity: TEntity);
var
  vSettings: TSettings;
  vSectionName: string;
  vValues: TStrings;
  vField: TBaseField;
  jFieldValue: TJSONObject;
  i: Integer;
begin
  vSettings := TDomain(FDomain).UserSettings;
  vSectionName := 'Defaults$' + FFullName;
  if vSettings.SectionExists(vSectionName) then
  begin
    vValues := TStringList.Create;
    try
      vSettings.ReadSection(vSectionName, vValues);
      for i := 0 to vValues.Count - 1 do
      begin
        if not TEntity(FDomainObject).FieldExists(vValues.Names[i]) then
        begin
          vSettings.DeleteKey(vSectionName, vValues.Names[i]);
          Continue;
        end;

        vField := TEntity(FDomainObject).FieldByName(vValues.Names[i]);
        if vField.FieldDef.HasFlag(cNotSave) then
          Continue;

        jFieldValue := TJSONObject.LoadFromText(vValues.ValueFromIndex[i]);
        try
          vField.SetFromJSON(jFieldValue, True);
        finally
          FreeAndNil(jFieldValue);
        end;
      end;
    finally
      FreeAndNil(vValues);
    end;
  end;
end;

procedure TView.NotifyUI(const AKind: Word; const AParameter: TObject);
var
  vMessage: TViewChangedMessage;
  vListener: TObject;
begin
  //Assert(FFreezeCount = 0, 'Почему-то FFreezeCount не равен нулю');
  if FFreezeCount > 0 then
    Exit;

  vMessage.Msg := DM_VIEW_CHANGED;
  vMessage.Kind := AKind;
  vMessage.View := Self;
  vMessage.Parameter := AParameter;

  for vListener in FListeners do
    vListener.Dispatch(vMessage);
end;

function TView.QueryParameter(const AName: string; const ADefaultValue: string = ''): string;
var
  vName: string;
begin
  vName := Trim(AName);
  if FQuery.IndexOfName(vName) >= 0 then
    Result := FQuery.Values[vName]
  else
    Result := ADefaultValue;
end;

function TView.QueryText: string;
begin
  if Assigned(FQuery) then
    Result := FQuery.DelimitedText
  else
    Result := '';
end;

procedure TView.RefreshView(const AParameter: TObject);
begin
  UpdateUIState;
  NotifyUI(dckViewStateChanged, AParameter);
  TInteractor(FInteractor).PrintHierarchy;
end;

procedure TView.RemoveListener(const AListener: TObject);
begin
  if Assigned(FListeners) then
  begin
    FListeners.Remove(AListener);
    CleanView;
  end;
end;

procedure TView.RemoveView(const AView: TView);
begin
//  if Assigned(FDomainObject) and (FDomainObject is TEntityList) and (AView.Name = 'Selected') then
//    TEntityList(FDomainObject).SelectEntity(nil);
  FItems.RemoveByObject(AView);
  CleanView;
end;

procedure TView.SetDefinition(const ADefinition: TObject);
begin
  if ADefinition is TDefinition then
  begin
    ExtractViewParams(TDefinition(ADefinition).Name);
    NotifyUI(dckContentTypeChanged, nil);
  end
  else
    Assert(False, 'Операция не поддерживается');
end;

function TView.SetDomainEntity(const AEntity: TEntity): Boolean;
begin
  //if Assigned(FDomainObject) and Assigned(AEntity)
  //  and (TEntity(AEntity).Definition <> TEntity(FDomainObject).Definition)
  //then ; // Потенциальная проблема, если среди отображений полей есть переопределенные поля

  Result := SetDomainObject(AEntity);
  if Assigned(AEntity) then
    FDefinition := AEntity.Definition;
end;

function TView.SetDomainObject(const Value: TObject): Boolean;
begin
  if FDomainObject = Value then
    Exit(False);

  Result := True;
  UnsubscribeDomainObject;

  FDomainObject := Value;
  UpdateChildViews;

  SubscribeDomainObject;
end;

procedure TView.SetFieldEntity(const AHolder: TObject; const AEntity: TEntity);
var
  vEntity: TEntity;
  vFieldDef: TFieldDef;
begin
  vEntity := TEntity(GetParentDomainObject);
  if not Assigned(vEntity) then
    Exit;

  vFieldDef := TFieldDef(FDefinition);
  Assert((FDefinitionKind = dkObjectField) and Assigned(vFieldDef), 'Несоответствие типа поля и данных');

  TUserSession(TInteractor(FInteractor).Session).DomainWrite(procedure
    begin
      vEntity._SetFieldEntity(AHolder, vFieldDef.Name, AEntity);
    end);

  if Assigned(FParent) and (FParent.DefinitionKind = dkAction) and not vFieldDef.HasFlag(cNotSave) then
    StoreActionState(vEntity.FieldByName(vFieldDef.Name));
end;

procedure TView.SetFieldStream(const AHolder: TObject; const AStream: TStream);
var
  vEntity: TEntity;
  vFieldDef: TFieldDef;
begin
  vEntity := TEntity(GetParentDomainObject);
  if not Assigned(vEntity) then
    Exit;

  vFieldDef := TFieldDef(FDefinition);
  Assert((FDefinitionKind = dkSimpleField) and Assigned(vFieldDef) and (vFieldDef.Kind = fkBlob),
    'Несоответствие типа поля и данных');

  TUserSession(TInteractor(FInteractor).Session).DomainWrite(procedure
    begin
      vEntity._SetFieldStream(AHolder, vFieldDef.Name, AStream);
    end);
end;

procedure TView.SetFieldValue(const AHolder: TObject; const AValue: Variant);
var
  vEntity: TEntity;
  vFieldDef: TFieldDef;
  vValue: Variant;
begin
  vEntity := TEntity(GetParentDomainObject);
  if not Assigned(vEntity) then
    Exit;

  vFieldDef := TFieldDef(FDefinition);
  Assert((FDefinitionKind = dkSimpleField) and Assigned(vFieldDef), 'Несоответствие типа поля и данных');

  vValue := AValue;
  TUserSession(TInteractor(FInteractor).Session).DomainWrite(procedure
    begin
      vEntity._SetFieldValue(AHolder, vFieldDef.Name, vValue);
    end);

  if Assigned(FParent) and (FParent.DefinitionKind = dkAction) and not vFieldDef.HasFlag(cNotSave) then
    StoreActionState(vEntity.FieldByName(vFieldDef.Name));
end;

procedure TView.StoreActionState(const AField: TBaseField);
var
  jFieldValue: TJSONValue;
begin
  jFieldValue := AField.JSON;
  try
    TDomain(FDomain).UserSettings.SetValue('Defaults$' + FParent.FullName, AField.FieldName,
      '{"' + AField.FieldName + '":' + jFieldValue.ToString + '}');
  finally
    FreeAndNil(jFieldValue);
  end;
end;

procedure TView.SubscribeDomainObject;
var
  vChildView: TView;
begin
  // Нужно здесь проверить по FullName Definition-а, что
  //  1) он в списке полей безопасности этого пользователя
  //  2) если родительский Definition совпадает с Definition состояния,
  //       то подписаться на поле "-State-" родительской сущности
  //       иначе найти коллекцию указанного Definition и подписаться на поле "-State-" ее сущности First
  if not Assigned(FDomainObject) then
    Exit;

  // обработка подписок на сущности и списки
  if FDomainObject is TEntityList then
    TEntityList(FDomainObject).AddUIListener(Self)
  else if FDomainObject is TEntity then
  begin
    for vChildView in FItems.Objects do
      vChildView.SubscribeField(TEntity(FDomainObject));
    TEntity(FDomainObject).AddUIListener('', Self);
  end
  else
    Assert(False, 'Тип доменного объекта не поддерживается для отписки');
end;

procedure TView.SubscribeField(const AParentEntity: TEntity);
begin
  if not (FDefinitionKind in [dkListField, dkObjectField, dkSimpleField, dkComplexField]) then
    Exit;
  if SameText(TFieldDef(FDefinition).Name, 'ID') then
    Exit;

  if Assigned(AParentEntity) then
    AParentEntity.AddUIListener(TFieldDef(FDefinition).Name, Self);
end;

function TView.TextHierarchy(const AIndent: string = ''; const ACurrentView: TView = nil): string;
var
  vChildView: TView;
begin
  Result := AIndent + Description + #13#10;
  for vChildView in FItems.Objects do
    Result := Result + vChildView.TextHierarchy(AIndent + '    ', ACurrentView);
end;

procedure TView.UnsubscribeDomainObject;
var
  vChildView: TView;
  vClassName: string;
begin
  if not Assigned(FDomainObject) then
    Exit;

  // обработка подписок на сущности и списки
  if FDomainObject is TEntityList then
    TEntityList(FDomainObject).RemoveUIListener(Self)
  else if FDomainObject is TEntity then
  begin
    for vChildView in FItems.Objects do
      vChildView.UnsubscribeField(TEntity(FDomainObject));
    TEntity(FDomainObject).RemoveUIListener('', Self);
  end
  else begin
    vClassName := FDomainObject.ClassName;
    Assert(False, 'Тип доменного объекта не поддерживается для отписки^ ' + vClassName);
  end;
end;

procedure TView.UnsubscribeField(const AParentEntity: TEntity);
begin
  if not (FDefinitionKind in [dkListField, dkObjectField, dkSimpleField, dkComplexField]) then
    Exit;
  if SameText(TFieldDef(FDefinition).Name, 'ID') then
    Exit;

  if Assigned(AParentEntity) then
    AParentEntity.RemoveUIListener(TFieldDef(FDefinition).Name, Self);
end;

procedure TView.UpdateChildViews(const ADefinitionKind: TDefinitionKind = dkUndefined);
var
  vChildView: TView;
begin
  for vChildView in FItems.Objects do
    if (ADefinitionKind = dkUndefined) or (vChildView.DefinitionKind = ADefinitionKind) then
      vChildView.DoParentChanged(FDomainObject);
end;

procedure TView.UpdateUIState;
var
  vParentState: TViewState;
  vContextEntity: TEntity;
  vSession: TUserSession;
  vDefinition: TDefinition;
  vSysAction: TActionDef;
  vActionName: string;
  vIsContextDependent: Boolean;
  vFieldDef: TFieldDef;
  vEntityState: TState;
begin
  // Здесь у нас есть полностью заполненный объект DomainObject
  if Assigned(FParent) then
    vParentState := FParent.State
  else
    vParentState := vsFullAccess;

  vSession := TUserSession(TInteractor(FInteractor).Session);
  case FDefinitionKind of
    dkDomain: FState := vsFullAccess;
    dkCollection:
      begin
        if Assigned(FDomainObject) then
        begin
          FState := vParentState and TEntityList(FDomainObject).ViewState;
          vDefinition := TDefinition(FDefinition);
          if vDefinition.HasFlag(ccSystem) then
            FState := FState and vsReadOnly;
          FState := FState and vDefinition.UIState and vSession.GetUIState(vDefinition.FullName, nil);
        end
        else
          FState := vsHidden;
      end;
    dkEntity:
      begin
        if Assigned(FDomainObject) then
        begin
          FState := vParentState and TEntity(FDomainObject).ViewState;
          if TEntity(FDomainObject).IsService then
            FState := FState and vsReadOnly
          else
          begin
            FState := FState and vsFullAccess;
            UpdateChildViews(dkAction);
          end;
        end
        else
          FState := vsHidden;
      end;
    dkAction:
      begin
        if FName = 'Close' then
        begin
          FState := vsFullAccess;
          Exit;
        end;

        vEntityState := nil;
        vSysAction := TActionDef(FDefinition);
        vActionName := vSysAction.FullName;
        if Assigned(FParent) and (FParent.DefinitionKind in [dkEntity, dkObjectField]) then
        begin
          vContextEntity := TEntity(GetParentDomainObject);
          if Assigned(vContextEntity) then
          begin
            vEntityState := vContextEntity.EntityState;
            if Pos('.', vActionName) = 0 then
              vActionName := vContextEntity.Definition.Name + '.' + vActionName;
          end;
        end;

        FState := TCheckActionFlagsFunc(TConfiguration(TInteractor(FInteractor).Configuration).CheckActionFlagsFunc)(Self);
        FState := FState and vSession.GetUIState(vActionName, vEntityState);
        if (FName = 'Add') or (FName = 'Edit') or (FName = 'Delete') or (FName = 'Link') or (FName = 'OpenInPage')
          or (FName = 'Unlink') or (FName = 'View') or (FName = 'Refresh') or (FName = 'ViewDocument') or (FName = 'ClearDocument')
          or (FName = 'Load') or (FName = 'Create') or (FName = 'CreateEmbedded') or (FName = 'Open') or (FName = 'Show') or (FName.StartsWith('#')) then
          // Родительское состояние уже учтено
        else begin
          if (vParentState in [vsReadOnly{, vsSelectOnly}]) or ((vParentState = vsHidden) and (FParent.Name = 'Selected')) then
            vParentState := vsDisabled;
          FState := FState and vParentState;
          //if FState in [vsHidden, vsReadOnly] then
          //  FState := vsDisabled;
        end;
      end;
    dkListField, dkObjectField, dkSimpleField, dkComplexField:
      begin
        //?? Сделать промежуточное состояние между vsReadOnly и vsSelectOnly?
        if vParentState = vsSelectOnly then
          //vParentState := vsReadOnly;
          vParentState := vsFullAccess;

        vIsContextDependent := IsContextDependent;
        if vIsContextDependent then
          vParentState := vParentState and vsDisabled;

        vContextEntity := TEntity(GetParentDomainObject);

        if Assigned(vContextEntity) then
        begin
          vFieldDef := TFieldDef(FDefinition);
          if vContextEntity.FieldExists(vFieldDef.Name) then
            FState := vContextEntity.FieldByName(vFieldDef.Name).GetUIState(vSession)
          else
            FState := vsReadOnly;

          if Assigned(FParent) then
          begin
            if FParent.DefinitionKind = dkAction then
            begin
              if vParentState = vsHidden then
                FState := vsHidden
              else if (vParentState = vsDisabled) {and vIsContextDependent} then
                FState := vsDisabled
              else
                FParent.RefreshView(nil);
            end
            else
              FState := FState and vParentState;
          end;
        end
        else
          FState := vsHidden;
      end;
  else
    FState := vsHidden;
  end;
end;

function TView.GetEnumerator: TEnumerator<TView>;
begin
  Result := FItems.Objects.GetEnumerator;
end;

function TView.GetField: TBaseField;
var
  vEntity: TEntity;
  vFieldDef: TFieldDef;
begin
  Result := nil;
  vEntity := TEntity(GetParentDomainObject);
  if not Assigned(vEntity) then
    Exit;

  vFieldDef := TFieldDef(FDefinition);
  if Assigned(vFieldDef) then
  begin
    if vFieldDef.Name = 'ID' then
      Result := nil
    else
      Result := vEntity.FieldByName(vFieldDef.Name);
  end;
end;

function TView.GetFieldEntity: TEntity;
var
  vField: TEntityField;
begin
  vField := TEntityField(GetField);
  if Assigned(vField) then
    Result := vField.Entity
  else
    Result := nil;
end;

function TView.GetFieldObject: TComplexObject;
var
  vField: TComplexField;
begin
  vField := TComplexField(GetField);
  if Assigned(vField) then
    Result := vField.ComplexObject
  else
    Result := nil;
end;

function TView.GetFieldStream: TStream;
var
  vField: TBlobField;
begin
  vField := TBlobField(GetField);
  if Assigned(vField) then
    Result := vField.Stream
  else
    Result := nil;
end;

function TView.GetFieldValue: Variant;
var
  vField: TBaseField;
begin
  if True then

  vField := GetField;
  if Assigned(vField) then
  begin
    if not vField.Enabled then
      TEntity(vField.OwnerInstance).EnableField(vField);
    Result := vField.Value;
  end
  else if TFieldDef(FDefinition).Name = 'ID' then
    Result := SafeID(TEntity(GetParentDomainObject))
  else
    Result := Null;
end;

function TView.GetParentDomainObject: TObject;
begin
  if Assigned(FParent) then
    Result := FParent.DomainObject
  else
    Result := nil;
end;

function TView.GetStateCaption: string;
var
  vEntity: TEntity;
  vFieldDef: TFieldDef;
begin
  Result := '';
  vEntity := TEntity(GetParentDomainObject);
  if not Assigned(vEntity) then
    Exit;

  vFieldDef := TFieldDef(FDefinition);
  Assert((FDefinitionKind = dkSimpleField) and Assigned(vFieldDef) and (vFieldDef.Kind = fkEnum), 'Несоответствие типа поля и данных');

  Result := vEntity.GetStateCaption(vFieldDef.Name);
end;

function TView.ViewByName(const AName: string): TView;
begin
  Result := FItems.ObjectByName(AName);
end;

end.
