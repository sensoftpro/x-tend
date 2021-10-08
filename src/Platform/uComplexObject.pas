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

unit uComplexObject;

interface

uses
  Classes, uFastClasses, uJSON, uStorage, uTensor;

type
  TComplexObject = class
  private
    [Weak] FField: TObject;
    FOnChanged: TNotifyEvent;
  protected
    FStorageName: string;
    procedure DoFill(const AObject: TComplexObject); virtual;

    function GetJSONValue: TJSONValue; virtual;
    procedure SetJSONValue(const Value: TJSONValue); virtual;
    procedure DoTransfer(const AStorage: TStorage); virtual;
    procedure DoLoad(const AStorage: TStorage); virtual;
    procedure DoUpdateContent; virtual;
  public
    constructor Create(const AField: TObject; const AOnChanged: TNotifyEvent); virtual;

    procedure _Fill(const AObject: TComplexObject);
    procedure Transfer(const AStorage: TStorage);
    procedure Load(const AStorage: TStorage);

    procedure NotifyChanged(const AHolder: TObject);

    property JSONValue: TJSONValue read GetJSONValue write SetJSONValue;
    property Field: TObject read FField;
  end;

  TTensorData = class(TComplexObject)
  private
    FTensor: TTensor;
  protected
    function GetJSONValue: TJSONValue; override;
    procedure SetJSONValue(const Value: TJSONValue); override;
    procedure DoFill(const AObject: TComplexObject); override;
  public
    constructor Create(const AField: TObject; const AOnChanged: TNotifyEvent); override;
    destructor Destroy; override;

    property _Tensor: TTensor read FTensor;
  end;

  TComplexClass = class of TComplexObject;

implementation

uses
  Variants, SysUtils, uDefinition, uEntity, uConsts;

{ TComplexObject }

constructor TComplexObject.Create(const AField: TObject; const AOnChanged: TNotifyEvent);
begin
  inherited Create;
  FField := AField;
  if Assigned(FField) then
    FStorageName := TBaseField(AField).StorageName
  else
    FStorageName := '';
  FOnChanged := AOnChanged;
end;

procedure TComplexObject.DoFill(const AObject: TComplexObject);
begin
end;

procedure TComplexObject.DoLoad(const AStorage: TStorage);
var
  vText: string;
  jData: TJSONObject;
begin
  vText := VarToStr(AStorage.ReadValue(FStorageName, fkString));
  jData := TJSONObject.LoadFromText(vText);
  try
    SetJSONValue(jData);
  finally
    FreeAndNil(jData);
  end;
end;

procedure TComplexObject.DoTransfer(const AStorage: TStorage);
var
  jData: TJSONObject;
begin
  jData := TJSONObject(GetJSONValue);
  try
    AStorage.WriteValue(FStorageName, fkString, jData.ToString);
  finally
    FreeAndNil(jData);
  end;
end;

procedure TComplexObject.DoUpdateContent;
begin
end;

procedure TComplexObject._Fill(const AObject: TComplexObject);
begin
  DoFill(AObject);
end;

function TComplexObject.GetJSONValue: TJSONValue;
begin
  Result := nil;
end;

procedure TComplexObject.Load(const AStorage: TStorage);
begin
  DoLoad(AStorage);
end;

procedure TComplexObject.NotifyChanged(const AHolder: TObject);
begin
  DoUpdateContent;
  if Assigned(FField) then
    FOnChanged(AHolder);
end;

procedure TComplexObject.SetJSONValue(const Value: TJSONValue);
begin
end;

procedure TComplexObject.Transfer(const AStorage: TStorage);
begin
  DoTransfer(AStorage);
end;

{ TTensorData }

constructor TTensorData.Create(const AField: TObject; const AOnChanged: TNotifyEvent);
begin
  inherited Create(AField, AOnChanged);
  FTensor := TTensor.Create([1024]);
  FTensor.Fill(dfkZero);
end;

destructor TTensorData.Destroy;
begin
  FreeAndNil(FTensor);
  inherited Destroy;
end;

procedure TTensorData.DoFill(const AObject: TComplexObject);
begin
  FTensor.CopyTo(TTensorData(AObject).FTensor);
  TTensorData(AObject).NotifyChanged(nil); { TODO -owa : Chek in real usage }
end;

function TTensorData.GetJSONValue: TJSONValue;
var
  jData: TJSONObject;
  vArray: TJSONArray;
  i: Integer;
begin
  jData := TJSONObject.Create;
  jData.StoreInteger('kind', Integer(FTensor.DataKind));
  vArray := TJSONArray.Create;
  for i := 0 to FTensor.Dimensions.Count - 1 do
    vArray.AddElement(TJSONNumber.Create(FTensor.Dimensions[i]));
  jData.AddPair('dimensions', vArray);
  vArray := TJSONArray.Create;
  for i := 0 to FTensor.Size - 1 do
    vArray.AddElement(TJSONNumber.Create(FTensor.FlatValues[i]));
  jData.AddPair('values', vArray);
  Result := jData;
end;

procedure TTensorData.SetJSONValue(const Value: TJSONValue);
var
  jData: TJSONObject absolute Value;
  vDataKind: TTensorDataKind;
  vDimensions: TIntegerList;
  vArray: TJSONArray;
  vValue: TJSONValue;
  i: Integer;
begin
  if not Assigned(Value) then
  begin
    FTensor.Clear;
    Exit;
  end;

  FreeAndNil(FTensor);

  vDataKind := TTensorDataKind(jData.ExtractInteger('kind'));
  vDimensions := TIntegerList.Create;
  try
    vArray := jData.ExtractArray('dimensions');
    for vValue in vArray do
      vDimensions.Add(TJSONNumber(vValue).AsInt);
    FTensor := TTensor.Create(vDimensions, vDataKind);
  finally
    FreeAndNil(vDimensions);
  end;

  vArray := jData.ExtractArray('values');
  Assert(FTensor.Size = vArray.Size, 'Размеры массивов данных не совпадают');
  for i := 0 to FTensor.Size - 1 do
    FTensor.FlatValues[i] := TJSONNumber(vArray.Get(i)).AsDouble;
end;

end.
