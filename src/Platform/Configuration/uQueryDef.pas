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

unit uQueryDef;

interface

uses
  Generics.Collections, Classes, uConsts;

////////////////////////////////////////////////////////////////////////////////
///  Формат запроса:
///
///  QUERY := <CRITERIA>
///  CRITERIA := [NOT](<FIELDPATH>[<CONDITION>(<FIELDPATH>|<VALUE>)])|<COMPLEXCRITERIA>
///  COMPLEXCRITERIA := (OR|AND)(<CRITERIA>[,<CRITERIA>]+);
///  VALUE := "text"|number
///  FIELDPATH := <FIELD>[.<FIELD>]*[<MODIFIER>]
///  FIELD := FieldName[:CollectionName]
///  NOT := !
///  AND := &
///  OR := |
///  CRITERIA := (=|^|$|>|<|}|{)   //равно, начинается с, заканчивается на, больше, меньше, является частью, содержит
///  MODIFIER := (@|#|%)           //игнорировать регистр, окружающие пробелы, язык
///  *!=*                          //равенство самому себе
///  Специальные символы
///    * - текущая сущность
///    @ - пустая сущность
///    $ - залогиненный пользователь

type
  TGetParameterFunc = function(const AParamName: string): Variant of object;

  TQueryDef = class;

  TBaseCriteria = class
  private
    // For TQueryObject invokes only
    procedure CollapseNegative(const AIsNot: Boolean);
  protected
    FIsNot: Boolean;
    function BuildText: string; virtual; abstract;
    procedure DoCollapseNegative(const AIsNot: Boolean); virtual;
  public
    constructor Create(const AIsNot: Boolean);
    function Text: string;

    property IsNot: Boolean read FIsNot;
  end;

  TSimpleCriteria = class(TBaseCriteria)
  protected
    FVariable: string;
    function BuildText: string; override;
    function StrToModifier(const AModifier: string): TConditionModifier;
  public
    constructor Create(const AIsNot: Boolean; const AVariable: string);

    property Variable: string read FVariable;
  end;

  TBoolCriteria = class(TSimpleCriteria)
  protected
    function BuildText: string; override;
  end;

  TStringCriteria = class(TSimpleCriteria)
  private
    FModifier: TConditionModifier;
    FCondition: TConditionKind;
    FValue: string;
  protected
    function BuildText: string; override;
  public
    constructor Create(const AIsNot: Boolean; const AVariable,
      AModifier, ACondition, AValue: string);

    property Modifier: TConditionModifier read FModifier;
    property Condition: TConditionKind read FCondition;
    property Value: string read FValue;
  end;

  TIntegerCriteria = class(TSimpleCriteria)
  private
    FCondition: TConditionKind;
    FValue: Integer;
  protected
    function BuildText: string; override;
  public
    constructor Create(const AIsNot: Boolean; const AVariable,
      ACondition: string; const AValue: Integer);

    property Condition: TConditionKind read FCondition;
    property Value: Integer read FValue;
  end;

  TFloatCriteria = class(TSimpleCriteria)
  private
    FCondition: TConditionKind;
    FValue: Double;
  protected
    function BuildText: string; override;
  public
    constructor Create(const AIsNot: Boolean; const AVariable,
      ACondition: string; const AValue: Double);

    property Condition: TConditionKind read FCondition;
    property Value: Double read FValue;
  end;

  TParamCriteria = class(TSimpleCriteria)
  private
    FModifier: TConditionModifier;
    FCondition: TConditionKind;
    FParamName: string;
    FFieldPath: string;
  protected
    function BuildText: string; override;
  public
    constructor Create(const AQueryObject: TQueryDef; const AIsNot: Boolean;
      const AVariable, AModifier, ACondition: string; const AParamName: string);

    property Modifier: TConditionModifier read FModifier;
    property Condition: TConditionKind read FCondition;
    property ParamName: string read FParamName;
    property FieldPath: string read FFieldPath;
  end;

  TComplexCriteria = class(TBaseCriteria)
  private
    FIsAnd: Boolean;
    FCriterias: TObjectList<TBaseCriteria>;
  protected
    function BuildText: string; override;
    procedure DoCollapseNegative(const AIsNot: Boolean); override;
  public
    constructor Create(const AIsNot, AIsAnd: Boolean);
    destructor Destroy; override;

    procedure AddCriteria(const ACriteria: TBaseCriteria);

    property IsAnd: Boolean read FIsAnd;
    property Criterias: TObjectList<TBaseCriteria> read FCriterias;
  end;

  TQueryDef = class
  private
    FQueryString: string;
    FCollectionNames: TStrings;
    FSortFields: TStrings;
    FFilterFields: TStrings;
    FParameters: TStrings;
    FRootCriteria: TBaseCriteria;
    procedure ParseComplexCriteria(const vCriteria: TComplexCriteria;
      var AStartPos: Integer);
    function ParseSimpleCriteria(var AStartPos: Integer;
      const ANotModifier: Boolean = False): TBaseCriteria;
    function ParseCriteria(var AStartPos: Integer): TBaseCriteria;
  public
    constructor Create(const AFromClause, AWhereClause: string;
      const AOrderClause: string = '');
    destructor Destroy; override;

    procedure SetSorting(const ASortFields: string);
    function PrintText: string;

    property CollectionNames: TStrings read FCollectionNames;
    property SortFields: TStrings read FSortFields;
    property FilterFields: TStrings read FFilterFields;
    property QueryString: string read FQueryString;
    property RootCriteria: TBaseCriteria read FRootCriteria;
    property Parameters: TStrings read FParameters;
  end;

implementation

uses
  SysUtils, Character;

{ TQueryObject }

constructor TQueryDef.Create(const AFromClause, AWhereClause: string;
  const AOrderClause: string = '');
var
  vStartIndex: Integer;
begin
  inherited Create;

  FParameters := TStringList.Create;
  FFilterFields := TStringList.Create;

  FQueryString := AWhereClause;
  if AWhereClause <> '' then
  begin
    vStartIndex := 0;
    FRootCriteria := ParseCriteria(vStartIndex);
    FRootCriteria.CollapseNegative(False);
  end;

  FCollectionNames := TStringList.Create;
  FCollectionNames.Delimiter := ';';
  FCollectionNames.DelimitedText := AFromClause;

  if AOrderClause <> '' then
  begin
    FSortFields := TStringList.Create;
    FSortFields.Delimiter := ';';
    FSortFields.DelimitedText := AOrderClause;
  end
  else
    FSortFields := nil;
end;

destructor TQueryDef.Destroy;
begin
  FreeAndNil(FParameters);
  FreeAndNil(FRootCriteria);
  FreeAndNil(FFilterFields);
  FreeAndNil(FSortFields);
  FreeAndNil(FCollectionNames);
  inherited Destroy;
end;

procedure TQueryDef.ParseComplexCriteria(const vCriteria: TComplexCriteria;
  var AStartPos: Integer);
begin
  repeat
    if FQueryString.Chars[AStartPos] = ',' then
      AStartPos := AStartPos + 1;
    vCriteria.AddCriteria(ParseCriteria(AStartPos));
    if AStartPos >= Length(FQueryString) then
      Exit;
  until FQueryString.Chars[AStartPos] = ')';
  AStartPos := AStartPos + 1;
end;

function TQueryDef.ParseCriteria(var AStartPos: Integer): TBaseCriteria;
var
  vNotModifier: Boolean;
  vAndModifier: Boolean;
begin
  vNotModifier := FQueryString.Chars[AStartPos] = '!';
  if vNotModifier then
    AStartPos := AStartPos + 1;
  if FQueryString.Chars[AStartPos].IsInArray(['&', '|']) then
  begin
    vAndModifier := FQueryString.Chars[AStartPos] = '&';
    AStartPos := AStartPos + 2;
    Result := TComplexCriteria.Create(vNotModifier, vAndModifier);
    ParseComplexCriteria(TComplexCriteria(Result), AStartPos);
  end
  else
    Result := ParseSimpleCriteria(AStartPos, vNotModifier);
end;

function TQueryDef.ParseSimpleCriteria(var AStartPos: Integer;
  const ANotModifier: Boolean): TBaseCriteria;
var
  vEnd: Integer;
  vValue: string;
  vStart: Integer;
  vVariable: string;
  vModifier: string;
  vCondition: string;
  vIsNot: Boolean;
  vHasDot: Boolean;
  i: Integer;
  vConvertableValue: string;
begin
  vEnd := AStartPos;
  while vEnd < Length(FQueryString) do
  begin
    if FQueryString.Chars[vEnd].IsInArray([',', ')']) then
      Break;
    vEnd := vEnd + 1;
  end;

  vValue := Trim(FQueryString.Substring(AStartPos, vEnd - AStartPos));
  vStart := 0;
  while (vStart < Length(vValue)) and (vValue.Chars[vStart].IsLetterOrDigit or vValue.Chars[vStart].IsInArray([':','.','*','$'])) do
    vStart := vStart + 1;
  vVariable := vValue.Substring(0, vStart);
  if (vStart < Length(vValue)) and vValue.Chars[vStart].IsInArray(['@','%','#']) then
  begin
    vModifier := vValue.Chars[vStart];
    vStart := vStart + 1;
  end;
  if (vStart < Length(vValue)) and (vValue.Chars[vStart] = '!') then
  begin
    vIsNot := True;
    vStart := vStart + 1;
  end
  else
    vIsNot := False;
  if (vStart < Length(vValue)) and vValue.Chars[vStart].IsInArray(['=','>','<','^','}','{','+']) then
  begin
    vCondition := vValue.Chars[vStart];
    vStart := vStart + 1;
  end;
  vValue := vValue.Remove(0, vStart);
  AStartPos := vEnd;

  if Length(vValue) = 0 then
    Result := TBoolCriteria.Create(ANotModifier xor vIsNot, vVariable)
  else if vValue.Chars[0] = '"' then
    Result := TStringCriteria.Create(ANotModifier xor vIsNot, vVariable, vModifier, vCondition, vValue)
  else if vValue.Chars[0].IsDigit then
  begin
    vHasDot := False;
    vConvertableValue := vValue.Chars[0];
    i := 1;
    while i < Length(vValue) do
    begin
      if not (vValue.Chars[i].IsDigit or (vValue.Chars[i] = '.')) then
        Break;
      if vValue.Chars[i] = '.' then
      begin
        if vHasDot then
          Break
        else begin
          vHasDot := True;
          vConvertableValue := vConvertableValue + FormatSettings.DecimalSeparator;
        end;
      end
      else
        vConvertableValue := vConvertableValue + vValue.Chars[i];
      i := i + 1;
    end;
    if vHasDot then
      Result := TFloatCriteria.Create(ANotModifier xor vIsNot, vVariable, vCondition, StrToFloat(vConvertableValue))
    else
      Result := TIntegerCriteria.Create(ANotModifier xor vIsNot, vVariable, vCondition, StrToInt(vConvertableValue));
  end
  else begin
    Result := TParamCriteria.Create(Self, ANotModifier xor vIsNot, vVariable, vModifier, vCondition, vValue);
    FParameters.Add(TParamCriteria(Result).ParamName);
  end;

  if Result is TSimpleCriteria then
    FFilterFields.Add(TSimpleCriteria(Result).Variable);
end;

function TQueryDef.PrintText: string;
begin
  Result := FRootCriteria.Text;
end;

procedure TQueryDef.SetSorting(const ASortFields: string);
var
  vSortFields: TStrings;
  vFieldName: string;
begin
  if ASortFields = '' then
    Exit;

  if not Assigned(FSortFields) then
  begin
    FSortFields := TStringList.Create;
    FSortFields.Delimiter := ';';
    FSortFields.DelimitedText := ASortFields;
  end
  else begin
    vSortFields := TStringList.Create;
    vSortFields.Assign(FSortFields);
    FSortFields.DelimitedText := ASortFields;
    try
      for vFieldName in vSortFields do
        if FSortFields.IndexOf(vFieldName) < 0 then
          FSortFields.Add(vFieldName);
    finally
      vSortFields.Free;
    end;
  end;
end;

{ TBaseCriteria }

procedure TBaseCriteria.CollapseNegative(const AIsNot: Boolean);
begin
  DoCollapseNegative(AIsNot);
end;

constructor TBaseCriteria.Create(const AIsNot: Boolean);
begin
  inherited Create;
  FIsNot := AIsNot;
end;

procedure TBaseCriteria.DoCollapseNegative(const AIsNot: Boolean);
begin
  FIsNot := FIsNot xor AIsNot;
end;

function TBaseCriteria.Text: string;
begin
  if FIsNot then
    Result := 'not ' + BuildText
  else
    Result := BuildText;
end;

{ TSimpleCriteria }

constructor TSimpleCriteria.Create(const AIsNot: Boolean; const AVariable: string);
begin
  inherited Create(AIsNot);
  if (Length(AVariable) > 0) and (AVariable.Chars[0] = '*') then
    FVariable := '*'
  else
    FVariable := AVariable;
end;

function TSimpleCriteria.StrToModifier(
  const AModifier: string): TConditionModifier;
begin
  if AModifier = '@' then
    Result := cmIgnoreCase
  else if AModifier = '#' then
    Result := cmIgnoreSpace
  else if AModifier = '%' then
    Result := cmIgnoreLanguage
  else
    Result := cmNone;
end;

function TSimpleCriteria.BuildText: string;
begin
  Result := FVariable;
end;

{ TComplexCriteria }

procedure TComplexCriteria.AddCriteria(const ACriteria: TBaseCriteria);
begin
  FCriterias.Add(ACriteria);
end;

constructor TComplexCriteria.Create(const AIsNot, AIsAnd: Boolean);
begin
  inherited Create(AIsNot);
  FIsAnd := AIsAnd;
  FCriterias := TObjectList<TBaseCriteria>.Create;
end;

destructor TComplexCriteria.Destroy;
begin
  FreeAndNil(FCriterias);
  inherited Destroy;
end;

procedure TComplexCriteria.DoCollapseNegative(const AIsNot: Boolean);
var
  vCriteria: TBaseCriteria;
begin
  inherited DoCollapseNegative(AIsNot);

  if FIsNot then
  begin
    FIsNot := False;
    FIsAnd := not FIsAnd;
    for vCriteria in FCriterias do
      vCriteria.CollapseNegative(True);
  end;
end;

function TComplexCriteria.BuildText: string;
var
  vCriteria: TBaseCriteria;
begin
  Result := '';
  for vCriteria in FCriterias do
  begin
    if Result <> '' then
    begin
      if FIsAnd then
        Result := Result + ' and '
      else
        Result := Result + ' or ';
    end;
    Result := Result + vCriteria.Text;
  end;

  Result := '(' + Result + ')';
end;

{ TBoolCriteria }

function TBoolCriteria.BuildText: string;
begin
  Result := FVariable;
end;

{ TStringCriteria }

function TStringCriteria.BuildText: string;
begin
  Result := FVariable + cModifierNames[FModifier] + ' ' +
    cConditionNames[FCondition] + ' "' + FValue + '"';
end;

constructor TStringCriteria.Create(const AIsNot: Boolean; const AVariable,
  AModifier, ACondition, AValue: string);
var
  vPos: Integer;
begin
  inherited Create(AIsNot, AVariable);

  FModifier := StrToModifier(AModifier);
  FCondition := StrToCondition(ACondition);

  FValue := Copy(AValue, 2, Length(AValue) - 1);
  vPos := Pos('"', FValue);
  if vPos > 0 then
    Delete(FValue, vPos, Length(FValue) - vPos + 1);
  FValue := ApplyModifier(FValue, FModifier);
end;

{ TIntegerCriteria }

function TIntegerCriteria.BuildText: string;
begin
  Result := FVariable + ' ' + cConditionNames[FCondition] + ' ' + IntToStr(FValue);
end;

constructor TIntegerCriteria.Create(const AIsNot: Boolean; const AVariable,
  ACondition: string; const AValue: Integer);
begin
  inherited Create(AIsNot, AVariable);
  FCondition := StrToCondition(ACondition);
  FValue := AValue;
end;

{ TFloatCriteria }

function TFloatCriteria.BuildText: string;
begin
  Result := FVariable + ' ' + cConditionNames[FCondition] + ' ' + FloatToStr(FValue);
end;

constructor TFloatCriteria.Create(const AIsNot: Boolean; const AVariable,
  ACondition: string; const AValue: Double);
begin
  inherited Create(AIsNot, AVariable);
  FCondition := StrToCondition(ACondition);
  FValue := AValue;
end;

{ TFieldCriteria }

function TParamCriteria.BuildText: string;
begin
  Result := FVariable + cModifierNames[FModifier] + ' ' +
    cConditionNames[FCondition] + ' ' + FParamName + cModifierNames[FModifier];
end;

constructor TParamCriteria.Create(const AQueryObject: TQueryDef;
  const AIsNot: Boolean; const AVariable, AModifier, ACondition, AParamName: string);
var
  i: Integer;
begin
  inherited Create(AIsNot, AVariable);

  FModifier := StrToModifier(AModifier);
  FCondition := StrToCondition(ACondition);
  FFieldPath := '';
  if AParamName.Chars[0].IsInArray(['*', '@', '$']) then
  begin
    FParamName := AParamName.Chars[0];
    if Length(AParamName) > 2 then
      FFieldPath := Copy(AParamName, 3, Length(AParamName) - 2);
  end
  else begin
    i := 0;
    while (i < Length(AParamName)) and (AParamName.Chars[i].IsLetterOrDigit or AParamName.Chars[i].IsInArray(['.',':'{,'.'}])) do
      i := i + 1;
    FParamName := AParamName.Substring(0, i);
  end;
end;

end.

