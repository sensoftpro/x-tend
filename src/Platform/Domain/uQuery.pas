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

unit uQuery;

interface

uses
  Generics.Collections, Classes, uQueryDef, uEntity, uConsts;

type
  TQueryExecutor = class
  private
    FQueryDef: TQueryDef;
    FResults: TList<TEntity>;
    FParameters: TDictionary<string, Variant>;
    FCollectionName: string;
    procedure AddSorted(const AEntity: TEntity; const AStartPos: Integer;
      const ASortFields: TStrings = nil);
    function LessThanSecond(const AEntity1, AEntity2: TEntity;
      const ASortFields: TStrings): Boolean;

    function CheckValueIsMatch(const ACriteria: TSimpleCriteria; const ASession: TObject;
      const AEntity: TEntity; const AFieldKind: TFieldKind; const AValue: Variant): Boolean;
    function CheckFieldIsMatch(const ACriteria: TSimpleCriteria; const ASession: TObject;
      const AEntity: TEntity; const AField: TBaseField): Boolean;
    function CheckEntityIsMatch(const ACriteria: TSimpleCriteria; const AEntity: TEntity): Boolean;

    function CheckSimpleCriteria(const ACriteria: TSimpleCriteria;
      const ASession: TObject; const AEntity: TEntity): Boolean;
    function CheckComplexCriteria(const ACriteria: TComplexCriteria;
      const ASession: TObject; const AEntity: TEntity): Boolean;

    function CheckCriteriaIsMatch(const ACriteria: TBaseCriteria;
      const ASession: TObject; const AEntity: TEntity): Boolean;
  public
    constructor Create(const AQueryDef: TQueryDef; const ACollectionName: string = '');
    destructor Destroy; override;

    function Select(const ASession: TObject; const AGetParameterFunc: TGetParameterFunc = nil): TList<TEntity>;
    function IsMatch(const ASession: TObject; const AEntity: TEntity): Boolean;
    procedure SetParameter(const AKey: string; const AValue: Variant);
    procedure SetParameters(const ASession: TObject; const AEntity: TEntity);

    procedure SetSorting(const ASortFields: string);

    property Results: TList<TEntity> read FResults;
    property Parameters: TDictionary<string, Variant> read FParameters;
  end;

implementation

uses
  SysUtils, Variants, uObjectField, uDomain, uCollection, uSession, uDomainUtils, uDefinition;

{ TQueryExecutor }

procedure TQueryExecutor.AddSorted(const AEntity: TEntity; const AStartPos: Integer; const ASortFields: TStrings);
var
  i: Integer;
  vIndex: Integer;
begin
  if Assigned(ASortFields) then
  begin
    vIndex := -1;
    for i := AStartPos to FResults.Count - 1 do
      if LessThanSecond(AEntity, FResults[i], ASortFields) then
      begin
        vIndex := i;
        Break;
      end;
    if vIndex < 0 then
      FResults.Add(AEntity)
    else
      FResults.Insert(vIndex, AEntity);
  end
  else
    FResults.Add(AEntity);
end;

function TQueryExecutor.CheckComplexCriteria(const ACriteria: TComplexCriteria; const ASession: TObject;
  const AEntity: TEntity): Boolean;
var
  vCriteria: TBaseCriteria;
begin
  Result := True;
  if ACriteria.Criterias.Count = 0 then
    Exit;

  if ACriteria.IsAnd then
  begin
    for vCriteria in ACriteria.Criterias do
    begin
      Result := CheckCriteriaIsMatch(vCriteria, ASession, AEntity);
      if not Result then
        Exit;
    end;
  end
  else begin
    for vCriteria in ACriteria.Criterias do
    begin
      Result := CheckCriteriaIsMatch(vCriteria, ASession, AEntity);
      if Result then
        Exit;
    end;
  end;
end;

function TQueryExecutor.CheckCriteriaIsMatch(const ACriteria: TBaseCriteria; const ASession: TObject;
  const AEntity: TEntity): Boolean;
begin
  if ACriteria is TSimpleCriteria then
    Result := CheckSimpleCriteria(TSimpleCriteria(ACriteria), ASession, AEntity)
  else if ACriteria is TComplexCriteria then
    Result := CheckComplexCriteria(TComplexCriteria(ACriteria), ASession, AEntity)
  else
    Result := False;
end;

function TQueryExecutor.CheckEntityIsMatch(const ACriteria: TSimpleCriteria; const AEntity: TEntity): Boolean;
var
  vParamCriteria: TParamCriteria absolute ACriteria;
  vField: TBaseField;
  vValue: Variant;
begin
  if ACriteria is TParamCriteria then
  begin
    Result := False;
    vValue := FParameters[vParamCriteria.ParamName];

    if vParamCriteria.ParamName = '*' then
    begin
      if vParamCriteria.Condition = ckEqualTo then
        Result := (AEntity = EntityFromVariant(vValue))
      else
        Assert(False, 'Invalid comparision');
      Exit;
    end;

    Assert(VarIsNull(vValue), 'Entity can be compared to fields only!');
    vField := TBaseField(NativeInt(vValue));
    if (vField.FieldKind = fkObject) and (vParamCriteria.Condition = ckEqualTo) then
      Result := vField.CheckValuedCondition(NativeInt(AEntity), ckEqualTo, cmNone)
    else if (vField.FieldKind = fkList) and (vParamCriteria.Condition = ckPartOf) then
      Result := vField.CheckValuedCondition(NativeInt(AEntity), ckContains, cmNone)
    else
      Assert(False, 'Wrong behaviour..');
  end
  else begin
    Result := False;
    Assert(False, 'Wrong field type for comparision');
  end;
end;

function TQueryExecutor.CheckFieldIsMatch(const ACriteria: TSimpleCriteria; const ASession: TObject;
  const AEntity: TEntity; const AField: TBaseField): Boolean;
begin
  if Assigned(AField) then
    Result := CheckValueIsMatch(ACriteria, ASession, AEntity, AField.FieldKind, AField.Value)
  else
    Result := CheckValueIsMatch(ACriteria, ASession, AEntity, fkNotDefined, Null);
end;

function TQueryExecutor.CheckSimpleCriteria(const ACriteria: TSimpleCriteria;
  const ASession: TObject; const AEntity: TEntity): Boolean;
var
  vField: TBaseField;
  vPath: string;
  vVariable: string;
  vFieldName: string;
  vPos: Integer;
  vEntity: TEntity;
  vResultField: TBaseField;
  vHandled: Boolean;
  i: Integer;
begin
  Result := False;
  if ACriteria.Variable = '*' then
    Result := CheckEntityIsMatch(ACriteria, AEntity)
  else if ACriteria.Variable = '$' then
  begin
    // Ветка не работает
    vPath := ACriteria.Variable;
    if Length(vPath) > 2 then
    begin
      if not Assigned(TUserSession(ASession).CurrentUser) then
        Exit;
      Delete(vPath, 1, 2);
      vField := TUserSession(ASession).CurrentUser.FieldByName(vPath);
      if not Assigned(vField) then
        Exit;
      Result := CheckFieldIsMatch(ACriteria, ASession, AEntity, vField);
    end
    else
      Assert(False, 'Such a query can''t be handled');
  end
  else if ACriteria.Variable.Chars[0] = '$' then
    Result := CheckFieldIsMatch(ACriteria, ASession, AEntity, nil)
  else begin
    vVariable := ACriteria.Variable;
    vEntity := AEntity;
    vPos := Pos('.', vVariable);
    vHandled := False;
    while (vPos > 0) and Assigned(vEntity) and not vHandled do
    begin
      vFieldName := Copy(vVariable, 1, vPos - 1);
      System.Delete(vVariable, 1, vPos);
      vField := vEntity.FieldByName(vFieldName);
      if vField.FieldKind = fkObject then
      begin
        vEntity := vEntity.ExtractEntity(vFieldName);
        vPos := Pos('.', vVariable);
      end
      else if vField.FieldKind = fkList then
      begin
        vHandled := True;
        for i := 0 to TListField(vField).Count - 1 do
        begin
          vEntity := TEntity(TListField(vField)[i]);
          vResultField := vEntity.FieldByName(vVariable);
          if Assigned(vResultField) then
            Result := CheckFieldIsMatch(ACriteria, ASession, AEntity, vResultField);
          if Result then
            Break;
        end;
      end;
    end;

    if not vHandled then
    begin
      if Assigned(vEntity) then
      begin
        if SameText(vVariable, 'ID') then
          Result := CheckValueIsMatch(ACriteria, ASession, AEntity, fkInteger, vEntity.ID)
        else
          Result := CheckFieldIsMatch(ACriteria, ASession, AEntity, vEntity.FieldByName(vVariable));
      end;
    end;
  end;
  if ACriteria.IsNot then
    Result := not Result;
end;

function TQueryExecutor.CheckValueIsMatch(const ACriteria: TSimpleCriteria; const ASession: TObject;
  const AEntity: TEntity; const AFieldKind: TFieldKind; const AValue: Variant): Boolean;
var
  vParamCriteria: TParamCriteria absolute ACriteria;
  vStrCriteria: TStringCriteria absolute ACriteria;
  vIntCriteria: TIntegerCriteria absolute ACriteria;
  vFloatCriteria: TFloatCriteria absolute ACriteria;
  vField: TBaseField;
  vValue: Variant;
begin
  Result := False;

  if ACriteria is TParamCriteria then
  begin
    if (vParamCriteria.ParamName = '@') and (AFieldKind = fkObject) then
      Result := NativeInt(AValue) = 0
    else if not FParameters.TryGetValue(vParamCriteria.ParamName, vValue) then
    begin
      vField := AEntity.FieldByName(vParamCriteria.ParamName);
      if Assigned(vField) then
        if vField.FieldKind in [fkString..fkObject] then
          Result := CheckCondition(AValue, vField.Value, AFieldKind, vParamCriteria.Condition, vParamCriteria.Modifier);
    end
    else begin
      if vParamCriteria.FieldPath = '' then
        Result := CheckCondition(AValue, vValue, AFieldKind, vParamCriteria.Condition, vParamCriteria.Modifier)
      else begin
        if vValue > 0 then
        begin
          vField := TEntity(NativeInt(vValue)).FieldByName(vParamCriteria.FieldPath);
          if Assigned(vField) then
            if vField.FieldKind in [fkString..fkObject] then
              Result := CheckCondition(AValue, vField.Value, AFieldKind, vParamCriteria.Condition, vParamCriteria.Modifier);
        end;
      end;
    end;
  end
  else if ACriteria is TBoolCriteria then
  begin
    if TBoolCriteria(ACriteria).Variable = '$IsAdmin$' then
      Result := TUserSession(ASession).IsAdmin
    else if TBoolCriteria(ACriteria).Variable = '$IsService$' then
      Result := AEntity.IsService
    else if TBoolCriteria(ACriteria).Variable = '$IsNew$' then
      Result := AEntity.IsNew
    else
      Result := CheckCondition(AValue, True, AFieldKind, ckEqualTo, cmNone);
  end
  else if ACriteria is TStringCriteria then
  begin
    Result := CheckCondition(AValue, vStrCriteria.Value, AFieldKind, vStrCriteria.Condition, vStrCriteria.Modifier);
  end
  else if ACriteria is TIntegerCriteria then
  begin
    Result := CheckCondition(AValue, vIntCriteria.Value, AFieldKind, vIntCriteria.Condition, cmNone);
  end
  else if ACriteria is TFloatCriteria then
  begin
    Result := CheckCondition(AValue, vFloatCriteria.Value, AFieldKind, vFloatCriteria.Condition, cmNone);
  end
  else
    Assert(False, 'Wrong field type for comparision');
end;

constructor TQueryExecutor.Create(const AQueryDef: TQueryDef; const ACollectionName: string = '');
begin
  inherited Create;
  FQueryDef := AQueryDef;
  FParameters := TDictionary<string, Variant>.Create;
  FResults := TList<TEntity>.Create;
  FCollectionName := ACollectionName;
end;

destructor TQueryExecutor.Destroy;
begin
  FreeAndNil(FParameters);
  FreeAndNil(FResults);
  FQueryDef := nil;
  inherited Destroy;
end;

function TQueryExecutor.IsMatch(const ASession: TObject; const AEntity: TEntity): Boolean;
var
  vCriteria: TBaseCriteria;
begin
  vCriteria := FQueryDef.RootCriteria;
  if Assigned(vCriteria) then
    Result := CheckCriteriaIsMatch(vCriteria, ASession, AEntity)
  else
    Result := True;
end;

function TQueryExecutor.LessThanSecond(const AEntity1, AEntity2: TEntity; const ASortFields: TStrings): Boolean;
var
  i: Integer;
  vField: TBaseField;
  vValue: Variant;
  vCompareResult: Integer;
begin
  i := 0;
  Result := False;
  repeat
    vField := AEntity2.FieldByName(ASortFields[i]);
    Assert(Assigned(vField));

    vValue := vField.Value;
    vCompareResult := AEntity1.FieldByName(ASortFields[i]).Compare(vValue);

    if vCompareResult > 0 then
      Exit
    else if vCompareResult = 0 then
      i := i + 1
    else
      Result := True;
  until (i = ASortFields.Count) or Result;
end;

function TQueryExecutor.Select(const ASession: TObject; const AGetParameterFunc: TGetParameterFunc): TList<TEntity>;
var
  vCollection: TCollection;
  vCollectionName: string;
  vStartPos: Integer;
  vEntity: TEntity;
  vAllParamsAssigned: Boolean;
  vKey: string;
begin
  if Assigned(AGetParameterFunc) then
    for vKey in FParameters.Keys do
      SetParameter(vKey, AGetParameterFunc(vKey));

  // Проверить, что все параметры установлены
  vAllParamsAssigned := True;
  for vKey in FParameters.Keys do
    // Сюда может прийти имя поля, которое нужно разыменовать в результате запроса
    if VarIsNull(FParameters[vKey]) and (vKey = '') then
    begin
      vAllParamsAssigned := False;
      TDomain(TUserSession(ASession).Domain).Logger.AddMessage(
        Format('Error: Parameter [%s] is not assigned. Query: [%s]', [vKey, FQueryDef.QueryString]));
      Break;
    end;

  FResults.Clear;
  if vAllParamsAssigned then
  begin
    if FCollectionName <> '' then
    begin
      vCollection := TDomain(TUserSession(ASession).Domain).CollectionByName(FCollectionName);
      for vEntity in vCollection do
        if IsMatch(ASession, vEntity) then
          AddSorted(vEntity, 0, FQueryDef.SortFields);
    end
    else
      for vCollectionName in FQueryDef.CollectionNames do
      begin
        vStartPos := FResults.Count;
        vCollection := TDomain(TUserSession(ASession).Domain).CollectionByName(vCollectionName);
        for vEntity in vCollection do
          if IsMatch(ASession, vEntity) then
            AddSorted(vEntity, vStartPos, FQueryDef.SortFields);
      end;
  end;
  Result := FResults;
end;

procedure TQueryExecutor.SetParameter(const AKey: string; const AValue: Variant);
begin
  FParameters.AddOrSetValue(AKey, AValue);
end;

procedure TQueryExecutor.SetParameters(const ASession: TObject; const AEntity: TEntity);
var
  vField: TBaseField;
  vKey: string;
  vValue: Variant;
begin
  for vKey in FQueryDef.Parameters do
  begin
    vValue := Null;
    if vKey = '*' then
      vValue := NativeInt(AEntity)
    else if vKey = '@' then
      vValue := 0
    else if vKey = '$' then
      vValue := NativeInt(TUserSession(ASession).CurrentUser)
    else if Assigned(AEntity) then
    begin
      vField := AEntity.FieldByName(vKey);
      if Assigned(vField) then
        vValue := vField.Value;
    end;

    SetParameter(vKey, vValue);
  end;
end;

procedure TQueryExecutor.SetSorting(const ASortFields: string);
begin
  FQueryDef.SetSorting(ASortFields);
end;

end.
