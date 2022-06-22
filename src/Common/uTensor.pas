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

unit uTensor;

interface

uses
  Generics.Collections;

type
  TNumValue = Double;
  TTensorDataKind = (tdkDouble);
  TDataFillKind = (dfkNotFill, dfkZero, dfkRandom, dfkRange);
  TDoubleVector = array of Double;
  TDoubleSquare = array of TDoubleVector;
  TDoubleCube = array of TDoubleVector;

  TTensor = class;
  TTensorDataFunc = reference to function(const ATensor: TTensor): Double;

  TIntegerList = class(TList<Integer>)
  public
    procedure AddSortedUniqueRange(const AStart, AEnd: Integer);
    function AsTextRanges(const AIncrement: Integer): string;
  end;

  TTensor = class
  private
    FData: Pointer;
    FDataKind: TTensorDataKind;
    FDimensions: TIntegerList;
    FValueSize: Integer;
    FFilledMapSize: Integer;
    FCoefs: TIntegerList;
    FTensorSize: Integer;
    function SameShape(const AShape: TIntegerList): Boolean;
    procedure CorrectTensor;
    function InternalGetDouble(const AIndex: Integer): Double;
    procedure InternalSetDouble(const AIndex: Integer; const AValue: Double);
    function GetDoubleValue(const AIndexer: TIntegerList): Double;
    procedure SetDoubleValue(const AIndexer: TIntegerList; const AValue: Double);
    function ArrayToIntegerList(const ADimensions: array of Integer): TIntegerList;
    function CalcFlatIndex(const AIndexer: TIntegerList): Integer; overload;
    function CalcFlatIndex(const AIndexes: array of Integer): Integer; overload;
    function CreateSlicedTensor(const ADimensions: array of Integer): TTensor;
    function MeanAndTotalVariance(var ATotalVariance: Extended): Extended;
    function MeanAndStdDev(var AStdDev: Double): Double;
    function GetDataSize: Integer;
    function GetValue(const AIndexes: array of Integer): Double;
    procedure SetValue(const AIndexes: array of Integer; const AValue: Double);
  protected
    property DoubleValue[const AIndexer: TIntegerList]: Double read GetDoubleValue write SetDoubleValue;
  public
    constructor Create(const ADimensions: array of Integer; const ADataKind: TTensorDataKind = tdkDouble); overload;
    constructor Create(const AShape: TIntegerList; const ADataKind: TTensorDataKind = tdkDouble); overload;
    destructor Destroy; override;

    function PrintValues: string;

    procedure SetDimension(const AIndex, Value: Integer);
    procedure Reshape(const ANewShape: array of Integer); overload;
    procedure Reshape(const ANewShape: TIntegerList); overload;
    procedure Reshape; overload;
    procedure Fill(const AFillKind: TDataFillKind = dfkNotFill;
      const ARangeStart: Integer = 0; const ARangeEnd: Integer = 0);
    function Slice(const ADimensions: array of Integer): TTensor; overload;
    procedure Slice(const ADimensions: TIntegerList; const ATensor: TTensor); overload;
    function ApplyReducer(const ADimIndex: Integer; const AReducer: TTensorDataFunc): TTensor;
    procedure Sort;

    procedure Append(const AValue: Double); overload;
    procedure Append(const AValues: TDoubleVector); overload;
    procedure Append(const ATensor: TTensor); overload;
    procedure Append(const ATensors: array of TTensor); overload;
    procedure Remove(const ADimensions: array of Integer);
    procedure Clear;

    procedure CopyTo(const ATensor: TTensor);

    function Sum: Extended;
    function Mean: Extended;
    function StdDev: Double;
    function Median: Double;
    procedure MinMax(var AMin, AMax: Double);
    function Min: Double;
    function Max: Double;
    function Range: Double;
    function AvgMovingRange: Double;

    function TensorMean(const ADimIndex: Integer): TTensor;
    function TensorRange(const ADimIndex: Integer): TTensor;
    function TensorStdDev(const ADimIndex: Integer): TTensor;
    function TensorMedian(const ADimIndex: Integer): TTensor;

    property DataKind: TTensorDataKind read FDataKind;
    property DataSize: Integer read GetDataSize;
    property Dimensions: TIntegerList read FDimensions;
    property Values[const AIndexes: array of Integer]: Double read GetValue write SetValue;
    property FlatValues[const AIndex: Integer]: Double read InternalGetDouble write InternalSetDouble;
    property Size: Integer read FTensorSize;
  end;

implementation

uses
  SysUtils, Math;

type
  PDoubleArray = ^TDoubleArray;
  TDoubleArray = array[0..0] of Double;

{ TTensor }

procedure TTensor.Append(const AValues: TDoubleVector);
var
  vSize: Integer;
  vOldDataSize: Integer;
  vRequiredTensorSize: Integer;
  i: Integer;
begin
  vSize := Length(AValues);
  if vSize = 0 then
    Exit;

  if FTensorSize = 0 then
  begin
    vRequiredTensorSize := 1;
    for i := 1 to FDimensions.Count - 1 do
      if FDimensions[i] > 0 then
        vRequiredTensorSize := vRequiredTensorSize * FDimensions[i];
  end
  else
    vRequiredTensorSize := FTensorSize div FDimensions[0];

  Assert(vSize mod vRequiredTensorSize = 0, 'Неверная размерность массива для добавления');
  vOldDataSize := GetDataSize;
  FDimensions[0] := FDimensions[0] + vSize div vRequiredTensorSize;
  Reshape;

  Move(AValues[0], Pointer(NativeInt(FData) + vOldDataSize)^, vSize * FValueSize);
end;

procedure TTensor.Append(const AValue: Double);
begin
  Append([AValue]);
end;

procedure TTensor.Append(const ATensors: array of TTensor);
var
  vSize: Integer;
  vRequiredTensorSize: Integer;
  vOldDataSize: Integer;
  i: Integer;
  vTensor: TTensor;
begin
  vSize := Length(ATensors);
  if vSize = 0 then
    Exit;

  vRequiredTensorSize := FTensorSize div FDimensions[0];
  for i := 0 to vSize - 1 do
    Assert(ATensors[i].FTensorSize = vRequiredTensorSize, 'Неверный размер тензора для добавления');

  vOldDataSize := GetDataSize;
  FDimensions[0] := FDimensions[0] + vSize;
  Reshape;
  for i := 0 to vSize - 1 do
  begin
    vTensor := ATensors[i];
    Move(vTensor.FData, Pointer(NativeInt(FData) + vOldDataSize + i * vTensor.DataSize)^, vTensor.DataSize);
  end;
end;

procedure TTensor.Append(const ATensor: TTensor);
begin
  Append([ATensor]);
end;

function TTensor.ApplyReducer(const ADimIndex: Integer; const AReducer: TTensorDataFunc): TTensor;
var
  vMovingTensor: TTensor;
  vResultShape: TIntegerList;
  vMovingShape: TIntegerList;
  vSliceShape: TIntegerList;
  i: Integer;
begin
  vResultShape := TIntegerList.Create;
  vMovingShape := TIntegerList.Create;
  vSliceShape := TIntegerList.Create;
  try
    for i := 0 to FDimensions.Count - 1 do
    begin
      if i = ADimIndex then
        vResultShape.Add(FDimensions[i])
      else
        vMovingShape.Add(FDimensions[i]);
      vSliceShape.Add(-1);
    end;
  finally
    // Создать тензор результатов
    Result := TTensor.Create(vResultShape);
    // Создать скользящий тензор, в который будем загонять срезы
    vMovingTensor := TTensor.Create(vMovingShape);

    FreeAndNil(vResultShape);
    FreeAndNil(vMovingShape);
  end;

  try
    // Организовать скользящий цикл (потом можно организовать рекурсию)
    for i := 0 to FDimensions[ADimIndex] - 1 do
    begin
      vSliceShape[ADimIndex] := i;
      Slice(vSliceShape, vMovingTensor);
      Result.FlatValues[i] := AReducer(vMovingTensor);
    end;
  finally
    FreeAndNil(vMovingTensor);
  end;
end;

function TTensor.ArrayToIntegerList(const ADimensions: array of Integer): TIntegerList;
var
  i: Integer;
begin
  Result := TIntegerList.Create;
  for i := Low(ADimensions) to High(ADimensions) do
    Result.Add(ADimensions[i]);
end;

function TTensor.AvgMovingRange: Double;
var
  i: Integer;
  vSum: Double;
begin
  vSum := 0;
  if FTensorSize < 2 then
  begin
    Result := 0;
    Exit;
  end;

  for i := 1 to FTensorSize - 1 do
    vSum := vSum + Abs(InternalGetDouble(i) - InternalGetDouble(i - 1));
  Result := vSum / (FTensorSize - 1);
end;

function TTensor.CalcFlatIndex(const AIndexer: TIntegerList): Integer;
var
  vCoef: Integer;
  i: Integer;
begin
  Assert(FDimensions.Count = AIndexer.Count, 'Неверный размер индексатора');
  vCoef := 1;
  Result := 0;
  for i := FDimensions.Count - 1 downto 0 do
  begin
    Result := Result + AIndexer[i] * vCoef;
    vCoef := vCoef * FDimensions[i];
  end;
end;

function TTensor.CalcFlatIndex(const AIndexes: array of Integer): Integer;
var
  vCoef: Integer;
  i: Integer;
begin
  Assert(FDimensions.Count = Length(AIndexes), 'Неверный размер индексатора');
  vCoef := 1;
  Result := 0;
  for i := High(AIndexes) downto 0 do
  begin
    Result := Result + AIndexes[i] * vCoef;
    vCoef := vCoef * FDimensions[i];
  end;
end;

procedure TTensor.Clear;
begin
  FDimensions[0] := 0;
  Reshape;
end;

procedure TTensor.CopyTo(const ATensor: TTensor);
begin
  if FTensorSize <> ATensor.Size then
    ATensor.Reshape([FTensorSize]);
  Move(FData^, ATensor.FData^, FValueSize * FTensorSize);
end;

procedure TTensor.CorrectTensor;
var
  vNewMapSize: Integer;
begin
  vNewMapSize := FTensorSize * FValueSize;
  if Assigned(FData) then
  begin
    if vNewMapSize > FFilledMapSize then
    begin
      FData := ReallocMemory(FData, vNewMapSize);
      FillChar(Pointer(NativeInt(FData) + FFilledMapSize)^, vNewMapSize - FFilledMapSize, 0);
      FFilledMapSize := vNewMapSize;
    end;
  end
  else begin
    FData := GetMemory(vNewMapSize);
    FFilledMapSize := vNewMapSize;
  end;
end;

constructor TTensor.Create(const AShape: TIntegerList; const ADataKind: TTensorDataKind);
begin
  inherited Create;

  FData := nil;
  FFilledMapSize := 0;
  FDataKind := ADataKind;
  FCoefs := TIntegerList.Create;
  case ADataKind of
    tdkDouble: FValueSize := SizeOf(Double);
  else
    FValueSize := 0;
  end;
  FDimensions := TIntegerList.Create;
  Reshape(AShape);
end;

function TTensor.CreateSlicedTensor(const ADimensions: array of Integer): TTensor;
var
  i: Integer;
  vNewShape: TIntegerList;
  vLen: Integer;
begin
  vLen := Length(ADimensions);
  vNewShape := TIntegerList.Create;
  try
    for i := 0 to FDimensions.Count - 1 do
      if (i >= vLen) or (ADimensions[i] < 0) then
        vNewShape.Add(FDimensions[i]);
    Result := TTensor.Create(vNewShape, FDataKind);
  finally
    vNewShape.Free;
  end;
end;

constructor TTensor.Create(const ADimensions: array of Integer; const ADataKind: TTensorDataKind);
var
  vTemp: TIntegerList;
begin
  vTemp := ArrayToIntegerList(ADimensions);
  try
    Create(vTemp, ADataKind);
  finally
    vTemp.Free;
  end;
end;

destructor TTensor.Destroy;
begin
  FCoefs.Free;
  FDimensions.Free;
  if Assigned(FData) then
    FreeMemory(FData);
  inherited Destroy;
end;

procedure TTensor.Fill(const AFillKind: TDataFillKind; const ARangeStart, ARangeEnd: Integer);
var
  i: Integer;
begin
  if AFillKind = dfkNotFill then
    Exit;

  if AFillKind = dfkZero then
  begin
    FillChar(FData^, DataSize, 0);
    Exit;
  end;

  if ARangeEnd < ARangeStart then
  begin
    raise Exception.Create('Неверный диапазон генерации значений');
  end
  else if ARangeEnd = ARangeStart then
  begin
    for i := 0 to FTensorSize - 1 do
      InternalSetDouble(i, ARangeStart);
    Exit;
  end;

  if AFillKind = dfkRandom then
  begin
    for i := 0 to FTensorSize - 1 do
      InternalSetDouble(i, ARangeStart + (ARangeEnd - ARangeStart) * Random);
  end
  else if AFillKind = dfkRange then
  begin
    for i := 0 to FTensorSize - 1 do
      InternalSetDouble(i, ARangeStart + i);
  end
  else
    raise Exception.Create('Такой тип заполнения ещё не реализован');
end;

function TTensor.GetDataSize: Integer;
begin
  Result := FTensorSize * FValueSize;
end;

function TTensor.GetDoubleValue(const AIndexer: TIntegerList): Double;
begin
  Result := InternalGetDouble(CalcFlatIndex(AIndexer));
end;

function TTensor.GetValue(const AIndexes: array of Integer): Double;
begin
  Result := InternalGetDouble(CalcFlatIndex(AIndexes));
end;

{$R-}
function TTensor.InternalGetDouble(const AIndex: Integer): Double;
begin
  Result := PDoubleArray(FData)^[AIndex];
end;

procedure TTensor.InternalSetDouble(const AIndex: Integer; const AValue: Double);
begin
  PDoubleArray(FData)^[AIndex] := AValue;
end;

function TTensor.Max: Double;
var
  vMin, vMax: Double;
begin
  MinMax(vMin, vMax);
  Result := vMax;
end;

function TTensor.Mean: Extended;
var
  vVariance: Extended;
begin
  Result := MeanAndTotalVariance(vVariance);
end;

procedure KahanSumExtended(var s, r: Extended; const d: Extended);
var
  t, u: Extended;
begin
  t := d - r;
  u := s + t;
  r := (u - s) - t;
  s := u;
end;

function TTensor.MeanAndStdDev(var AStdDev: Double): Double;
var
  vTotalVariance: Extended;
begin
  if FTensorSize = 1 then
  begin
    Result := InternalGetDouble(0);
    AStdDev := Result;
    Exit;
  end;
  Result := MeanAndTotalVariance(vTotalVariance);
  AStdDev := Sqrt(vTotalVariance / (FTensorSize - 1));
end;

function TTensor.MeanAndTotalVariance(var ATotalVariance: Extended): Extended;
var
  s, r: Extended;
  i: Integer;
begin
  if FTensorSize = 0 then
  begin
    Result := 0;
    ATotalVariance := 0;
    Exit;
  end
  else if FTensorSize = 1 then
  begin
    Result := InternalGetDouble(0);
    ATotalVariance := Result;
    Exit;
  end;

  Result := Sum / FTensorSize;
  s := Sqr(Result - InternalGetDouble(0));
  r := 0;
  for i := 1 to FTensorSize - 1 do
    KahanSumExtended(s, r, Sqr(Result - InternalGetDouble(i)));
  ATotalVariance := s;
end;

function TTensor.Median: Double;
var
  vSortedTensor: TTensor;
begin
  vSortedTensor := TTensor.Create(FDimensions, FDataKind);
  try
    Move(FData^, vSortedTensor.FData^, FValueSize * FTensorSize);
    //CopyMemory(vSortedTensor.FData, FData, FValueSize * FTensorSize);
    vSortedTensor.Sort;
    if FTensorSize mod 2 = 1 then
      Result := vSortedTensor.InternalGetDouble(FTensorSize div 2)
    else
      Result := (vSortedTensor.InternalGetDouble(FTensorSize div 2) +
        vSortedTensor.InternalGetDouble(FTensorSize div 2 - 1)) / 2;
  finally
    FreeAndNil(vSortedTensor);
  end;
end;

function TTensor.Min: Double;
var
  vMin, vMax: Double;
begin
  MinMax(vMin, vMax);
  Result := vMin;
end;

procedure TTensor.MinMax(var AMin, AMax: Double);
var
  i: Integer;
  vCurValue: Double;
begin
  if FTensorSize = 0 then
  begin
    AMin := 0;
    AMax := 0;
    Exit;
  end;

  AMin := InternalGetDouble(0);
  AMax := AMin;

  for i := 1 to FTensorSize - 1 do
  begin
    vCurValue := InternalGetDouble(i);
    if vCurValue < AMin then
      AMin := vCurValue
    else if vCurValue > AMax then
      AMax := vCurValue;
  end;
end;

{$R+}

function TTensor.PrintValues: string;
var
  i, j: Integer;
  vIndex: UInt64;
  vDiv: UInt64;
  vResult: string;
begin
  Result := '';
  if FFilledMapSize <= 0 then
  begin
    //raise Exception.Create('Нет данных для отображения');
    Exit;
  end;

  if FDimensions.Count >= 2 then
  begin
    for i := 0 to FTensorSize - 1 do
    begin
      vIndex := i;
      vResult := '';
      for j := 0 to FDimensions.Count - 2 do
      begin
        DivMod(vIndex, FCoefs[j], vDiv, vIndex);
        if j > 0 then
          vResult := vResult + ', ';
        vResult := vResult + IntToStr(vDiv);
      end;
      vResult := vResult + ', ' + IntToStr(vIndex);
      Result := Result + #13#10'[' + vResult + '] = ' + FormatFloat('#.00', InternalGetDouble(i));
    end;
  end
  else begin
    for i := 0 to FTensorSize - 1 do
      Result := Result + #13#10'[' + IntToStr(i) + '] = ' + FormatFloat('#.00', InternalGetDouble(i));
  end;
end;

function TTensor.Range: Double;
var
  vMin, vMax: Double;
begin
  MinMax(vMin, vMax);
  Result := vMax - vMin;
end;

procedure TTensor.Remove(const ADimensions: array of Integer);
// Указать номера удаляемых строк в измерении или -1
var
  vDimensions: TIntegerList;
  vIndexes: TIntegerList;
  i: Integer;

  // Для последнего измерения возможна оптимизация, т.к. удалять надо с шагом последнего измерения и стартом на удаляемой строке
  procedure EnumRemovingIndexes(const ARemain, AStart, AHandledCount: Integer; const AResult: TIntegerList);
  var
    vValue: Integer;
    vStart: Integer;
    vDimension: Integer;
    vRemain: Integer;
  begin
    vDimension := FDimensions[AHandledCount];
    if vDimension = 0 then
      Exit;
    vRemain := ARemain div vDimension;
    for vValue := 0 to vDimension - 1 do
    begin
      vStart := AStart + vRemain * vValue;
      if vValue = vDimensions[AHandledCount] then
        AResult.AddSortedUniqueRange(vStart, vStart + vRemain - 1);
      if AHandledCount < FDimensions.Count - 1 then
        EnumRemovingIndexes(vRemain, vStart, AHandledCount + 1, AResult);
    end;
  end;

  procedure CompressTensor(const ARemovingIndexes: TIntegerList);
  var
    vStartIndex: Integer;
    vRemovingCount: Integer;
    vInsertIndex: Integer;
    vPrevRemovingIndex: Integer;
    vNextRemovingIndex: Integer;
    vInsertingCount: Integer;
    i: Integer;
  begin
    if ARemovingIndexes.Count = 0 then
      Exit;

    vStartIndex := ARemovingIndexes[0];
    vInsertIndex := vStartIndex;
    vRemovingCount := 1;
    for i := 1 to ARemovingIndexes.Count - 1 do
    begin
      vNextRemovingIndex := ARemovingIndexes[i];
      if vStartIndex + vRemovingCount = vNextRemovingIndex then
        vRemovingCount := vRemovingCount + 1
      else begin
        vPrevRemovingIndex := vStartIndex + vRemovingCount - 1;
        vInsertingCount := vNextRemovingIndex - vPrevRemovingIndex - 1;
        Move(Pointer(NativeInt(FData) + (vPrevRemovingIndex + 1) * FValueSize)^,
          Pointer(NativeInt(FData) + vInsertIndex * FValueSize)^, vInsertingCount * FValueSize);
        //MoveMemory(Pointer(Integer(FData) + vInsertIndex * FValueSize),
        //  Pointer(Integer(FData) + (vPrevRemovingIndex + 1) * FValueSize), vInsertingCount * FValueSize);

        vInsertIndex := vInsertIndex + vInsertingCount;
        vStartIndex := vNextRemovingIndex;
        vRemovingCount := 1;
      end;
    end;

    if vRemovingCount > 0 then
    begin
      vPrevRemovingIndex := ARemovingIndexes.Last;
      vInsertingCount := FTensorSize - vPrevRemovingIndex - 1;
      if vInsertingCount > 0 then
        Move(Pointer(NativeInt(FData) + (vPrevRemovingIndex + 1) * FValueSize)^,
          Pointer(NativeInt(FData) + vInsertIndex * FValueSize)^, vInsertingCount * FValueSize);
        //MoveMemory(Pointer(Integer(FData) + vInsertIndex * FValueSize),
        //  Pointer(Integer(FData) + (vPrevRemovingIndex + 1) * FValueSize), vInsertingCount * FValueSize);
    end;
  end;

begin
  vDimensions := ArrayToIntegerList(ADimensions);
  vIndexes := TIntegerList.Create;
  try
    EnumRemovingIndexes(FTensorSize, 0, 0, vIndexes);
    CompressTensor(vIndexes);
    for i := 0 to vDimensions.Count - 1 do
      if (vDimensions[i] >= 0) and (vDimensions[i] < FDimensions[i]) then
        FDimensions[i] := FDimensions[i] - 1;
    Reshape;
  finally
    vDimensions.Free;
  end;
end;

procedure TTensor.Reshape;
var
  i: Integer;
  vCoef: Integer;
begin
  FTensorSize := 1;
  for i := 0 to FDimensions.Count - 1 do
    FTensorSize := FTensorSize * FDimensions[i];

  FCoefs.Clear;
  if FDimensions.Count >= 2 then
  begin
    vCoef := 1;
    for i := FDimensions.Count - 1 downto 1 do
    begin
      vCoef := vCoef * FDimensions[i];
      FCoefs.Insert(0, vCoef);
    end;
  end;

  CorrectTensor;
end;

procedure TTensor.Reshape(const ANewShape: TIntegerList);
var
  i: Integer;
begin
  if SameShape(ANewShape) then
    Exit;

  FDimensions.Clear;
  for i := 0 to ANewShape.Count - 1 do
    FDimensions.Add(ANewShape[i]);
  Reshape;
end;

procedure TTensor.Reshape(const ANewShape: array of Integer);
var
  vTemp: TIntegerList;
begin
  vTemp := ArrayToIntegerList(ANewShape);
  try
    Reshape(vTemp);
  finally
    vTemp.Free;
  end;
end;

function TTensor.SameShape(const AShape: TIntegerList): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FDimensions.Count <> AShape.Count then
    Exit;

  for i := 0 to FDimensions.Count - 1 do
    if FDimensions[i] <> AShape[i] then
      Exit;

  Result := True;
end;

procedure TTensor.SetDimension(const AIndex, Value: Integer);
begin
  if FDimensions[AIndex] = Value then
    Exit;

  FDimensions[AIndex] := Value;
  Reshape;
end;

procedure TTensor.SetDoubleValue(const AIndexer: TIntegerList; const AValue: Double);
begin
  InternalSetDouble(CalcFlatIndex(AIndexer), AValue);
end;

procedure TTensor.SetValue(const AIndexes: array of Integer; const AValue: Double);
begin
  InternalSetDouble(CalcFlatIndex(AIndexes), AValue);
end;

procedure TTensor.Slice(const ADimensions: TIntegerList; const ATensor: TTensor);
var
  vSrcIndexer: TIntegerList;
  vResIndexer: TIntegerList;

  procedure CopyTensorValues(const ASrcIndexer, AResIndexer: TIntegerList; const AResult: TTensor);
  var
    vCurDimension: Integer;
    vValue: Integer;
  begin
    vCurDimension := IfThen(ASrcIndexer.Count < ADimensions.Count, ADimensions[ASrcIndexer.Count], -1);
    if vCurDimension < 0 then
      for vValue := 0 to FDimensions[ASrcIndexer.Count] - 1 do
      begin
        ASrcIndexer.Add(vValue);
        AResIndexer.Add(vValue);
        try
          if ASrcIndexer.Count = ADimensions.Count then
            AResult.DoubleValue[AResIndexer] := Self.DoubleValue[ASrcIndexer]
          else
            CopyTensorValues(ASrcIndexer, AResIndexer, AResult);
        finally
          ASrcIndexer.Delete(ASrcIndexer.Count - 1);
          AResIndexer.Delete(AResIndexer.Count - 1);
        end;
      end
    else begin
      ASrcIndexer.Add(vCurDimension);
      try
        if ASrcIndexer.Count = ADimensions.Count then
          AResult.DoubleValue[AResIndexer] := Self.DoubleValue[ASrcIndexer]
        else
          CopyTensorValues(ASrcIndexer, AResIndexer, AResult);
      finally
        ASrcIndexer.Delete(ASrcIndexer.Count - 1);
      end;
    end;
  end;

begin
  vSrcIndexer := TIntegerList.Create;
  vResIndexer := TIntegerList.Create;
  try
    CopyTensorValues(vSrcIndexer, vResIndexer, ATensor);
  finally
    vSrcIndexer.Free;
    vResIndexer.Free;
  end;
end;

procedure QuickSort(const ATensor: TTensor; l, r: Integer);
var
  i, j: Integer;
  p, t: Double;
begin
  repeat
    i := l;
    j := r;
    p := ATensor.FlatValues[(l + r) shr 1];
    repeat
      while ATensor.FlatValues[i] < p do
        Inc(i);
      while ATensor.FlatValues[j] > p do
        Dec(j);
      if i <= j then
      begin
        if i <> j then
        begin
          t := ATensor.FlatValues[i];
          ATensor.FlatValues[i] := ATensor.FlatValues[j];
          ATensor.FlatValues[j] := t;
        end;
        Inc(i);
        Dec(j);
      end;
    until i > j;
    if L < j then
      QuickSort(ATensor, l, j);
    l := i;
  until i >= r;
end;

procedure TTensor.Sort;
begin
  if FTensorSize > 1 then
    QuickSort(Self, 0, FTensorSize - 1);
end;

function TTensor.StdDev: Double;
begin
  MeanAndStdDev(Result);
end;

function TTensor.Slice(const ADimensions: array of Integer): TTensor;
var
  vDimensions: TIntegerList;
begin
  Result := CreateSlicedTensor(ADimensions);
  vDimensions := ArrayToIntegerList(ADimensions);
  try
    Slice(vDimensions, Result);
  finally
    vDimensions.Free;
  end;
end;

function TTensor.Sum: Extended;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FTensorSize - 1 do
    Result := Result + InternalGetDouble(i);
end;

function TTensor.TensorMean(const ADimIndex: Integer): TTensor;
begin
  Result := ApplyReducer(ADimIndex, function(const ATensor: TTensor): Double
    begin
      Result := ATensor.Mean;
    end);
end;

function TTensor.TensorMedian(const ADimIndex: Integer): TTensor;
begin
  Result := ApplyReducer(ADimIndex, function(const ATensor: TTensor): Double
    begin
      Result := ATensor.Median;
    end);
end;

function TTensor.TensorRange(const ADimIndex: Integer): TTensor;
begin
  Result := ApplyReducer(ADimIndex, function(const ATensor: TTensor): Double
    begin
      Result := ATensor.Range;
    end);
end;

function TTensor.TensorStdDev(const ADimIndex: Integer): TTensor;
begin
  Result := ApplyReducer(ADimIndex, function(const ATensor: TTensor): Double
    begin
      Result := ATensor.StdDev;
    end);
end;

{ TIntegerList }

procedure TIntegerList.AddSortedUniqueRange(const AStart, AEnd: Integer);
var
  i: Integer;
  vIndex: Integer;
begin
  vIndex := 0;
  while (vIndex < Count) and (Items[vIndex] < AStart) do
    vIndex := vIndex + 1;

  for i := AStart to AEnd do
  begin
    if vIndex = Count then
      Add(i)
    else if Items[vIndex] > i then
      Insert(vIndex, i);
    vIndex := vIndex + 1;
  end;
end;

function TIntegerList.AsTextRanges(const AIncrement: Integer): string;
var
  i: Integer;
  vValue: Integer;
  vLastValue: Integer;
  vIsRange: Boolean;
begin
  if Count = 0 then
    Exit('[]');

  vLastValue := Items[0];
  Result := IntToStr(vLastValue + AIncrement);
  vIsRange := False;
  for i := 1 to Count - 1 do
  begin
    vValue := Items[i];
    if vValue - 1 = vLastValue then
      vIsRange := True
    else begin
      if vIsRange then
        Result := Result + ':' + IntToStr(vLastValue + AIncrement);
      Result := Result + ', ' + IntToStr(vValue + AIncrement);
      vIsRange := False;
    end;
    vLastValue := vValue;
  end;

  if vIsRange then
    Result := Result + ':' + IntToStr(vLastValue + AIncrement);

  Result := '[' + Result + ']';
end;

end.
