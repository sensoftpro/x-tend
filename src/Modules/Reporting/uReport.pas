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

unit uReport;

interface

uses
  Classes, Generics.Collections, uFastClasses, uModule, uEntity, uDefinition, uConsts, Character, uInteractor;

type
  TReportData = class;

  TAggregation = class
  private
    FAggregationDef: TAggregationDef;
    FCollector: Currency;
    FCount: Integer;
    function GetObservedFieldName: string;
    function GetValue: TReportValue;
  public
    constructor Create(const AAggregationDef: TAggregationDef);
    destructor Destroy; override;

    property Value: TReportValue read GetValue;
    procedure ProcessValue(const AValue: TReportValue);
    function MatchesToCondition(const AReportData: TReportData): Boolean;

    property ObservedFieldName: string read GetObservedFieldName;
  end;

  TPrintLayer = class
  private
    [Weak] FLayer: TLayer;
    FAggregations: TObjectStringList<TAggregation>;
  public
    constructor Create(const ALayer: TLayer);
    destructor Destroy; override;

    function AggregationByName(const AAggName: string): TAggregation;
    procedure ProcessAggregations(const AReportData: TReportData);
    property Layer: TLayer read FLayer;
  end;

  TReportData = class(TBaseModule)
  private
    [Weak] FReportDef: TDefinition;
    [Weak] FSession: TObject;
    [Weak] FDomain: TObject;
    FContext: TList<TEntity>;
    FPrintLayers: TObjectList<TPrintLayer>;
    FRowIndex: Integer;
    function FindValue(const AInstance: TEntity; const AParamName: string): TReportValue;
    function GetReportName: string;
    procedure PrintDataLayer(const ALayer: TDataLayer);
  protected
    [Weak] FInputParams: TEntity;
    function GetLayers: TList<TLayer>;
    procedure DoShowBand(const ABandName: string); virtual;
    procedure DoShowReport(const AReportContent: TStream; const AFileName: string); virtual;
    procedure DoGeneratePDF(const AReportContent: TStream; const AFileName: string); virtual;
  public
    constructor Create(const ASession: TObject; const AReportDef: TDefinition;
      const AContextEntity, AInputParams: TEntity); virtual;
    destructor Destroy; override;

    procedure ShowBand(const ABandName: string; const ARowIndex: Integer = -1);
    function ValueByName(const AParamName: string): TReportValue;

    procedure Push(const AEntity: TEntity);
    function Pop: TEntity;
    function Peek: TEntity;

    procedure PrintData(const AData: TList<TLayer>);
    procedure PrintHeader(const ALayer: TLayer);
    procedure PrintFooter(const ALayer: TLayer);

    procedure ProcessAggregations;

    function FillReportData(const AInitialText: string): string;
    function SetupQueryParameters(const AParamName: string): Variant;
    function ReportValue(const AParamName: string): TReportValue;
    function ReportTextValue(const AParamName: string): string;

    property Session: TObject read FSession;
    property ReportName: string read GetReportName;
  end;

procedure GeneratePDF(const AInteractor: TInteractor; const AReport: TReportDef;
  const AContextEntity, AInputParams: TEntity; const AFileName: string);
procedure ShowReport(const AInteractor: TInteractor; const AReport: TReportDef;
  const AContextEntity: TEntity);
procedure ShowRTFReport(const AInteractor: TInteractor; const AReport: TRTFReport;
  const AEntity: TEntity);

implementation

uses
  SysUtils, IOUtils, Variants, uPlatform, uEnumeration, uConfiguration, uDomain, uSession,
  uObjectField, uQuery, uUtils, uDomainUtils, uPresenter;

type
  TReportDataClass = class of TReportData;
  TGetReportValueFunc = function(const AReportData: TReportData; const AParamName: string;
    const AIndex1, AIndex2: Integer): TReportValue of object;

procedure GeneratePDF(const AInteractor: TInteractor; const AReport: TReportDef;
  const AContextEntity, AInputParams: TEntity; const AFileName: string);
var
  vDomain: TDomain;
  vModuleInfo: TModuleInfo;
  vReportData: TReportData;
begin
  vDomain := TDomain(AInteractor.Domain);
  if not Assigned(AReport.Content) then
  begin
    vDomain.Logger.AddMessage('Report [' + AReport.FileName + '] does not found in the filesystem');
    Exit;
  end;

  vModuleInfo := _Platform.ResolveModuleInfo(vDomain.Settings, 'ReportEngine', 'Reporting');
  if not Assigned(vModuleInfo) then
    Exit;

  vReportData := TReportDataClass(vModuleInfo.ModuleClass).Create(AInteractor.Session, AReport, AContextEntity, AInputParams);
  try
    AReport.Content.Position := 0;
    vReportData.DoGeneratePDF(AReport.Content, AFileName);
  finally
    vReportData.Free;
  end;
end;

procedure ShowReport(const AInteractor: TInteractor; const AReport: TReportDef;
  const AContextEntity: TEntity);
var
  vDomain: TDomain;
  vInputParams: TEntity;
  vModuleInfo: TModuleInfo;
  vReportData: TReportData;
  vFileName: string;
begin
  if not Assigned(AContextEntity) then
    Exit;

  vDomain := TDomain(AInteractor.Domain);
  if not Assigned(AReport.Content) then
  begin
    vDomain.Logger.AddMessage('Report [' + AReport.FileName + '] does not found in the filesystem');
    Exit;
  end;

  vModuleInfo := _Platform.ResolveModuleInfo(vDomain.Settings, 'ReportEngine', 'Reporting');
  if not Assigned(vModuleInfo) then
    Exit;

  vInputParams := TEntity.Create(AInteractor.Domain, AReport);
  try
    vInputParams.SubscribeFields(vDomain.DomainHolder);
    if not ((vInputParams.VisibleFieldCount(AInteractor.Session) > 0)
      and not AInteractor.EditParams(vInputParams)) then
    begin
      vReportData := TReportDataClass(vModuleInfo.ModuleClass).Create(
        AInteractor.Session, AReport, AContextEntity, vInputParams);
      try
        if AReport.OutputFileMask = '' then
          vFileName := 'Report' + FormatDateTime('ddmmyyyy_hhnnss', Now)
        else
          vFileName := EscapeFileName(Trim(vReportData.FillReportData(AReport.OutputFileMask))) +
            '_' + FormatDateTime('ddmmyyyy', Now);

        AReport.Content.Position := 0;
        vReportData.DoShowReport(AReport.Content, vFileName);
      finally
        FreeAndNil(vReportData);
      end;
    end;
  finally
    FreeAndNil(vInputParams);
  end;
end;

procedure ShowRTFReport(const AInteractor: TInteractor; const AReport: TRTFReport;
  const AEntity: TEntity);
var
  vNewReport: TStringList;
  vTempFileName: string;
  vReportContent: string;
  vNewReportContent: string;
  vParamName: string;
  vParamValue: string;
  vLen: Integer;
  vPos: Integer;
  vReportData: TReportData;
const
  cMarker = '$$';
begin
  AReport.Content.Position := 0;

  vReportData := TReportData.Create(AInteractor, AReport, AEntity, nil);
  try
    vNewReportContent := '';
    vReportContent := AReport.Content.DataString;
    vLen := Length(cMarker);
    vPos := Pos(cMarker, vReportContent);
    while vPos > 0 do
    begin
      vNewReportContent := vNewReportContent + Copy(vReportContent, 1, vPos - 1);
      Delete(vReportContent, 1, vPos - 1 + vLen);

      vPos := Pos(cMarker, vReportContent);
      Assert(vPos > 0, 'Invalid report template!');

      vParamName := Copy(vReportContent, 1, vPos - 1);
      vParamValue := vReportData.ReportTextValue(vParamName);

      vNewReportContent := vNewReportContent + vParamValue;
      Delete(vReportContent, 1, vPos - 1 + vLen);

      vPos := Pos(cMarker, vReportContent);
    end;
  finally
    FreeAndNil(vReportData);
  end;

  vNewReportContent := vNewReportContent + vReportContent;

  vTempFileName := TPath.Combine(TPath.GetTempPath, FormatDateTime('ddhhnnsszzz', Now) + AReport.Name + '.rtf');
  vNewReport := TStringList.Create;
  try
    vNewReport.Text := vNewReportContent;
    vNewReport.SaveToFile(vTempFileName);
  finally
    FreeAndNil(vNewReport);
  end;

  TPresenter(AInteractor.Presenter).OpenFile(vTempFileName);
end;

{ TReportData }

constructor TReportData.Create(const ASession: TObject; const AReportDef: TDefinition;
  const AContextEntity, AInputParams: TEntity);
begin
  inherited Create;

  FSession := ASession;
  FDomain := TUserSession(FSession).Domain;
  FReportDef := AReportDef;
  FInputParams := AInputParams;

  FContext := TList<TEntity>.Create;
  if Assigned(AContextEntity) then
    FContext.Add(AContextEntity);

  FPrintLayers := TObjectList<TPrintLayer>.Create;
end;

destructor TReportData.Destroy;
begin
  FreeAndNil(FPrintLayers);
  FreeAndNil(FContext);

  FReportDef := nil;
  FDomain := nil;
  FSession := nil;

  inherited Destroy;
end;

procedure TReportData.DoGeneratePDF(const AReportContent: TStream; const AFileName: string);
begin
end;

procedure TReportData.DoShowBand(const ABandName: string);
begin
end;

procedure TReportData.DoShowReport(const AReportContent: TStream; const AFileName: string);
begin
end;

function TReportData.FillReportData(const AInitialText: string): string;
  function ProcessParam(const ATextRest: string): string;
  var
    vFieldName: string;
    vPos: Integer;
    vStartText: string;
    vEndText: string;
  begin
    Result := ATextRest;

    vPos := Pos('{:', ATextRest);
    if vPos = 0 then
      Exit;

    vStartText := Copy(ATextRest, 1, vPos - 1);
    vEndText := Copy(ATextRest, vPos + 2, Length(ATextRest) - vPos);
    vPos := Pos('}', vEndText);
    if vPos > 0 then
    begin
      vFieldName := Copy(vEndText, 1, vPos - 1);
      vEndText := Copy(vEndText, vPos + 1, Length(vEndText) - vPos);
    end
    else begin
      vFieldName := vEndText;
      vEndText := '';
    end;

    if vFieldName = 'COUNT' then
      Result := '$$$'
    else if (vFieldName = 'LINENO') or (vFieldName = '#') or SameText(vFieldName, 'Line#') then
      Result := IntToStr(FRowIndex)
    else
      Result := ReportTextValue(vFieldName);

    Result := vStartText + Result + ProcessParam(vEndText);
  end;
begin
  Result := ProcessParam(AInitialText);
end;

function TReportData.FindValue(const AInstance: TEntity; const AParamName: string): TReportValue;
var
  vField: TBaseField;
begin
  Assert(Assigned(AInstance), '1');

  if AInstance.FieldExists(AParamName) then
  begin
    vField := AInstance.FieldByName(AParamName);

    Result.Value := vField.Value;
    Result.FieldKind := vField.FieldKind;
    if not (vField.FieldKind in [fkNotDefined, fkObject..fkComplex]) then
      Result.Extra := TSimpleFieldDef(vField.FieldDef).Dictionary
    else
      Result.Extra := '';
  end
  else begin
    Result.Value := Null;
    Result.FieldKind := fkNotDefined;
    Result.Extra := '';
  end;
end;

function TReportData.GetLayers: TList<TLayer>;
begin
  Result := TReportDef(FReportDef).Layers;
end;

function TReportData.GetReportName: string;
begin
  if Assigned(FReportDef) then
    Result := FReportDef.Name
  else
    Result := 'Templated';
end;

procedure TReportData.ProcessAggregations;
var
  i: Integer;
begin
  for i := FPrintLayers.Count - 1 downto 0 do
  begin
    FPrintLayers[i].ProcessAggregations(Self);
    // Аггрегации нужны до ближайшего уровня данных
    if FPrintLayers[i].Layer is TDataLayer then
      Break;
  end;
end;

procedure TReportData.Push(const AEntity: TEntity);
begin
  FContext.Add(AEntity);
end;

function TReportData.ReportTextValue(const AParamName: string): string;
var
  vIntValue: Integer;
  vEnum: TEnumeration;
  vItem: TEnumItem;
  vValue: TReportValue;
  vListField: TListField;
  i: Integer;
begin
  Result := '';
  vValue := ReportValue(AParamName);
  if VarIsNull(vValue.Value) then
    Exit;

  case vValue.FieldKind of
    fkString: Result := StringFromVariant(vValue.Value);
    fkInteger: Result := IntToStr(IntegerFromVariant(vValue.Value));
    fkEnum: begin
      Assert(vValue.Extra <> '', 'There is no enumeration type name!');
      vIntValue := IntegerFromVariant(vValue.Value);
      vEnum := _Platform.Enumerations.ObjectByName(vValue.Extra);
      Assert(Assigned(vEnum), 'There is no enumeration [' + vValue.Extra + ']');
      vItem := vEnum.Items[vIntValue];
      Result := TDomain(FDomain).Translate(vItem.FullName, vItem.DisplayText);
    end;
    fkFlag: begin { TODO -owa : correct }
      Assert(vValue.Extra <> '', 'There is no flag type name!');
      vIntValue := IntegerFromVariant(vValue.Value);
      Result := IntToStr(vIntValue);
    end;
    fkFloat: Result := FormatFloat('#.000', FloatFromVariant(vValue.Value));
    fkDateTime:
      if DateTimeFromVariant(vValue.Value) > 0 then
        Result := FormatDate(DateTimeFromVariant(vValue.Value));
    fkBoolean:
      if BooleanFromVariant(vValue.Value) then
        Result := 'YES';
    fkColor: Result := IntToHex(ColorFromVariant(vValue.Value), 8);
    fkCurrency:
  //      if vValue <> 0 then  //будем показывать 0
        Result := FormatFloat('#,##0.00;;0', CurrencyFromVariant(vValue.Value));
    fkObject: Result := SafeDisplayName(EntityFromVariant(vValue.Value));
    fkList: begin
      Result := '';
      vListField := TListField(NativeInt(vValue.Value));
      if not Assigned(vListField) then
        Exit;
      for i := 0 to vListField.Count - 1 do
      begin
        if Result <> '' then
          Result := Result + ';';
        Result := Result + SafeDisplayName(vListField[i]);
      end;
    end;
  end;
end;

function TReportData.ReportValue(const AParamName: string): TReportValue;
var
  vPos: Integer;
  vParamName: string;
  vIndexFull: string;
  vIndex1: Integer;
  vIndex2: Integer;
const
  cBracketStart = '[';
  cBracketEnd = ']';
  cIndexDelimiter = ',';
begin
  // Разбор значения
  vIndex1 := 0;
  vIndex2 := 0;
  vParamName := AParamName;
  vPos := Pos(cBracketStart, vParamName);
  if vPos > 0 then
  begin
    vIndexFull := Copy(vParamName, vPos + 1, Length(vParamName) - vPos);
    Delete(vParamName, vPos, Length(vParamName) - vPos + 1);

    vPos := Pos(cIndexDelimiter, vIndexFull);
    if vPos > 0 then
    begin
      vIndex1 := StrToIntDef(Trim(Copy(vIndexFull, 1, vPos - 1)), 0);
      Delete(vIndexFull, 1, vPos);

      vPos := Pos(cBracketEnd, vIndexFull);
      if vPos > 0 then
        vIndex2 := StrToIntDef(Trim(Copy(vIndexFull, 1, vPos - 1)), 0);
    end
    else begin
      vPos := Pos(cBracketEnd, vIndexFull);
      if vPos > 0 then
        vIndex1 := StrToIntDef(Trim(Copy(vIndexFull, 1, vPos - 1)), 0);
    end;
  end;

  Result := TGetReportValueFunc(TConfiguration(FReportDef.Configuration).GetReportValueFunc)(Self, vParamName, vIndex1, vIndex2);
end;

function TReportData.SetupQueryParameters(const AParamName: string): Variant;
var
  vVarValue: TReportValue;
begin
  vVarValue := ValueByName(AParamName);
  if VarIsNull(vVarValue.Value) then
    vVarValue := ReportValue(AParamName);
  Result := vVarValue.Value;
end;

procedure TReportData.ShowBand(const ABandName: string; const ARowIndex: Integer = -1);
begin
  if ABandName = '' then
    Exit;
  FRowIndex := ARowIndex;
  DoShowBand(ABandName);
end;

function TReportData.Peek: TEntity;
begin
  if FContext.Count > 0 then
    Result := FContext.Last
  else
    Result := nil;
end;

function TReportData.Pop: TEntity;
begin
  if FContext.Count > 0 then
    Result := FContext.Extract(FContext.Last)
  else
    Result := nil;
end;

procedure TReportData.PrintData(const AData: TList<TLayer>);
var
  vLayer: TLayer;
begin
  if not Assigned(AData) then
    Exit;

  for vLayer in AData do
    if vLayer is TDataLayer then
      PrintDataLayer(TDataLayer(vLayer));
end;

procedure TReportData.PrintDataLayer(const ALayer: TDataLayer);
var
  vGroup: TLayer;
  vList: TList<TEntity>;
  i, j: Integer;
  vBreakValues: TStrings;
  vNextValue: string;
  vBreakIndex: Integer;
  vDataQuery: TQueryExecutor;

  function GetDataList: TList<TEntity>;
  var
    vEntity: TEntity;
    vFieldDef: TFieldDef;
  begin
    if FContext.Count > 0 then
    begin
      vEntity := FContext[FContext.Count - 1];
      vFieldDef := vEntity.Definition.FieldByName(ALayer.Source);
      if Assigned(vFieldDef) and (vFieldDef.Kind = fkList) then
      begin
        TListField(vEntity.FieldByName(ALayer.Source)).GetList(FSession, vDataQuery.Results);
        Result := vDataQuery.Results;
        Exit;
      end;
    end;

    Result := vDataQuery.Select(FSession, SetupQueryParameters);
  end;
begin
  vDataQuery := TQueryExecutor.Create(ALayer.DataQuery);

  PrintHeader(ALayer);
  try
    if ALayer.DataName <> '' then
    begin
      // Определяем, есть ли группировка
      // ВАЖНО: сейчас сгруппировать можно только по одному полю
      if ALayer.Groups.Count > 0 then
        vGroup := ALayer.Groups[0]
      else
        vGroup := nil;

      if ALayer.Count > 0 then
      begin
        for i := 0 to ALayer.Count - 1 do
        begin
          PrintHeader(vGroup);
          try
            // Вывод банда с данными
            ShowBand(ALayer.DataName, i + 1);
            // Обработка аггрегаций
            ProcessAggregations;
            // Обработка вложенных данных
            PrintData(ALayer.Data);
          finally
            PrintFooter(vGroup);
          end;
        end;
      end
      else begin
        if ALayer.Groups.Count > 0 then
        begin
          vDataQuery.SetSorting(ALayer.BreakingFields.DelimitedText);

          // Получаем список всех записей для вывода
          vList := GetDataList;
          if vList.Count > 0 then
          begin
            vBreakValues := TStringList.Create;
            for j := 0 to ALayer.BreakingFields.Count - 1 do
              vBreakValues.Add('');

            for i := 0 to vList.Count - 1 do
            begin
              // Получаем данные о следующей записи
              Push(vList[i]);
              vBreakIndex := -1;
              for j := 0 to ALayer.BreakingFields.Count - 1 do
              begin
                vNextValue := FillReportData('{:' + ALayer.BreakingFields[j] + '}');
                if (vBreakIndex < 0) and (AnsiCompareStr(vBreakValues[j], vNextValue) <> 0) then
                  vBreakIndex := j;
                vBreakValues[j] := vNextValue;
              end;
              Pop;

              if vBreakIndex >= 0 then
              begin
                if i > 0 then
                  for j := ALayer.BreakingFields.Count - 1 downto vBreakIndex do
                    PrintFooter(ALayer.Groups[j]);

                Pop;
                Push(vList[i]);

                for j := vBreakIndex to ALayer.BreakingFields.Count - 1 do
                  PrintHeader(ALayer.Groups[j]);
              end
              else begin
                Pop;
                Push(vList[i]);
              end;

              // Обработка аггрегаций
              ProcessAggregations;
              // Вывод банда с данными
              ShowBand(ALayer.DataName);
              // Обработка вложенных данных
              PrintData(ALayer.Data);
            end;

            for j := ALayer.BreakingFields.Count - 1 downto 0 do
              PrintFooter(ALayer.Groups[j]);
            Pop;

            vBreakValues.Free;
          end;
        end
        else begin
          vList := GetDataList;
          for i := 0 to vList.Count - 1 do
          begin
            Push(vList[i]);
            try
              // Обработка аггрегаций
              ProcessAggregations;
              // Вывод банда с данными
              ShowBand(ALayer.DataName, i + 1);
              // Обработка вложенных данных
              PrintData(ALayer.Data);
            finally
              Pop;
            end;
          end;
        end;
      end;
    end;
  finally
    PrintFooter(ALayer);
    FreeAndNil(vDataQuery);
  end;
end;

procedure TReportData.PrintFooter(const ALayer: TLayer);
begin
  if Assigned(ALayer) then
  begin
    ShowBand(ALayer.FooterName);
    if FPrintLayers.Count > 0 then
      FPrintLayers.Remove(FPrintLayers.Last);
  end;
end;

procedure TReportData.PrintHeader(const ALayer: TLayer);
begin
  if Assigned(ALayer) then
  begin
    FPrintLayers.Add(TPrintLayer.Create(ALayer));
    ShowBand(ALayer.HeaderName);
  end;
end;

function TReportData.ValueByName(const AParamName: string): TReportValue;
var
  vAggregation: TAggregation;
  i: Integer;
begin
  Result.Value := Null;
  if AParamName = '*' then
  begin
    if FContext.Count > 0 then
      SetObjectValue(Result, FContext.Last)
    else if Assigned(FInputParams) then
      SetObjectValue(Result, FInputParams);
    Exit;
  end
  else if AParamName = '@' then
  begin
    SetObjectValue(Result, nil);
    Exit;
  end;

  // Ищем среди полей агрегации
  if FPrintLayers.Count > 0 then
  begin
    vAggregation := FPrintLayers.Last.AggregationByName(AParamName);
    if Assigned(vAggregation) then
    begin
      Result := vAggregation.Value;
      Exit;
    end;
  end;

  for i := FContext.Count - 1 downto 0 do
  begin
    Result := FindValue(FContext[i], AParamName);
    if not VarIsNull(Result.Value) then
      Exit;
  end;

  if Assigned(FInputParams) then
    Result := FindValue(FInputParams, AParamName);
end;

{ TPrintLayer }

function TPrintLayer.AggregationByName(const AAggName: string): TAggregation;
begin
  Result := FAggregations.ObjectByName(AAggName);
end;

constructor TPrintLayer.Create(const ALayer: TLayer);
var
  vAggDef: TAggregationDef;
begin
  inherited Create;
  FLayer := ALayer;
  FAggregations := TObjectStringList<TAggregation>.Create;
  for vAggDef in FLayer.AggregationDefs.Objects do
    FAggregations.AddObject(vAggDef.Name, TAggregation.Create(vAggDef));
end;

destructor TPrintLayer.Destroy;
begin
  FreeAndNil(FAggregations);
  inherited Destroy;
end;

procedure TPrintLayer.ProcessAggregations(const AReportData: TReportData);
var
  vAggregation: TAggregation;
  vValue: TReportValue;
begin
  for vAggregation in FAggregations.Objects do
  begin
    if vAggregation.MatchesToCondition(AReportData) then
    begin
      vValue := AReportData.ReportValue(vAggregation.ObservedFieldName);
      vAggregation.ProcessValue(vValue);
    end;
  end;
end;

{ TAggregation }

constructor TAggregation.Create(const AAggregationDef: TAggregationDef);
begin
  inherited Create;
  FAggregationDef := AAggregationDef;
  FCollector := 0;
  FCount := 0;
end;

destructor TAggregation.Destroy;
begin
  FAggregationDef := nil;
  inherited Destroy;
end;

function TAggregation.GetObservedFieldName: string;
begin
  Result := FAggregationDef.ObservedFieldName;
end;

function TAggregation.GetValue: TReportValue;
begin
  { TODO : Аггрегация сделана только для Currency }
  Result.Value := Null;
  Result.FieldKind := fkNotDefined;
  case FAggregationDef.Kind of
    akCount: SetIntegerValue(Result, FCount);
    akSum: SetCurrencyValue(Result, FCollector);
    akMin: SetCurrencyValue(Result, FCollector);
    akMax: SetCurrencyValue(Result, FCollector);
    akAverage:
      if FCount <= 0 then
        SetCurrencyValue(Result, 0);
      else
        SetCurrencyValue(Result, FCollector / FCount);
  end;
end;

function TAggregation.MatchesToCondition(const AReportData: TReportData): Boolean;
var
  vValue1, vValue2: TReportValue;
begin
  Result := True;
  if FAggregationDef.ConditionField1 = '' then
    Exit;

  if FAggregationDef.ConditionField2 = '' then
    Result := Boolean(AReportData.ReportValue(FAggregationDef.ConditionField1).Value)
  else begin
    vValue1 := AReportData.ReportValue(FAggregationDef.ConditionField1);
    vValue2 := AReportData.ReportValue(FAggregationDef.ConditionField2);
    Result := CheckCondition(vValue1.Value, vValue2.Value, vValue1.FieldKind, FAggregationDef.ConditionKind, cmNone);
  end;

  if FAggregationDef.IsNot then
    Result := not Result;
end;

procedure TAggregation.ProcessValue(const AValue: TReportValue);
var
  vValue: Currency;
begin
  { TODO : Аггрегация сделана только для Currency }
  if VarIsNull(AValue.Value) or not (AValue.FieldKind in [fkInteger, fkFloat, fkCurrency]) then
    vValue := 0
  else
    vValue := Currency(AValue.Value);

  case FAggregationDef.Kind of
    akCount: FCount := FCount + 1;
    akSum: FCollector := FCollector + vValue;
    akMin:
      if FCount = 0 then
      begin
        FCount := 1;
        FCollector := vValue;
      end
      else if FCollector > vValue then
        FCollector := vValue;
    akMax:
      if FCount = 0 then
      begin
        FCount := 1;
        FCollector := vValue;
      end
      else if FCollector < vValue then
        FCollector := vValue;
    akAverage:
      begin
        FCount := FCount + 1;
        FCollector := FCollector + vValue;
      end;
  end;
end;

initialization

TBaseModule.RegisterModule('Reporting', '', '<null>', TReportData);

end.
