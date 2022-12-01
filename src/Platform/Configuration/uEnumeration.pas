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

unit uEnumeration;

interface

uses
  Generics.Collections, uFastClasses, UITypes, Variants, TypInfo;

type
  TEnumItem = class
  private
    FID: Integer;
    FCode: string;
    FFullName: string;
    FDisplayText: string;
  public
    constructor Create(const AID: Integer; const AValueName, AFullName: string); virtual;

    property ID: Integer read FID;
    property FullName: string read FFullName;
    property Code: string read FCode;
    property DisplayText: string read FDisplayText;
  end;

  TEnumData = class(TEnumItem)
  end;

  TState = class(TEnumItem)
  private
    FColor: TColor;
    FAvailableTransitions: TList<Integer>;
  public
    constructor Create(const AID: Integer; const AValueName, AFullName: string); override;
    destructor Destroy; override;

    procedure AddAvailableTransition(const AAvailableState: Integer);
    function CanTransitToState(const ANewState: Variant): Boolean;

    property Color: TColor read FColor;
  end;

  TEnumItemClass = class of TEnumItem;

  TEnumeration = class
  private
    FName: string;
    FItems: TObjectList<TEnumItem>;
    FTypeInfo: PTypeInfo;
    FIndexType: TOrdType;
    function GetItem(const AID: Integer): TEnumItem;
    function GetCount: Integer;
  protected
    constructor Create(const AName: string; const ATypeInfo: PTypeInfo; const AEnumItemClass: TEnumItemClass);
  public
    class function CreateEnum<T>(const AName: string; const AEnumItemClass: TEnumItemClass): TEnumeration;
    destructor Destroy; override;

    function AddDisplayNames(const ADefaultValues: array of string): TEnumeration;
    function AddIDs(const AIDs: array of Integer): TEnumeration;
    property Items[const AID: Integer]: TEnumItem read GetItem; default;
    property Count: Integer read GetCount;

    function GetEnumerator: TEnumerator<TEnumItem>;
    function GetItemByDisplayText(const AName: string): TEnumItem;

    property Name: string read FName;
  end;

  TStateMachine = class(TEnumeration)
  private
    function ConvertToInt<T>(const AValue: T): Integer;
  public
    function AddTransitions<T>(const AState: T; const AAvailableStates: array of T): TStateMachine;
    function AddColors(const AColors: array of TColor): TEnumeration;
    function StateByID(const AID: Integer): TState;

    function Get(const AValue: Variant): Integer;
  end;

  TEnumerations = class(TObjectStringDictionary<TEnumeration>)
  public
    function AddEnum<T>(const AName: string): TEnumeration;
  end;

  TStateMachines = class(TObjectStringDictionary<TStateMachine>)
  public
    function AddStateMachine<T>(const AName: string): TStateMachine;
  end;

implementation

uses
  RTTI, SysUtils, IniFiles;

{ TEnumeration }

function TEnumeration.AddDisplayNames(const ADefaultValues: array of string): TEnumeration;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    FItems[i].FDisplayText := ADefaultValues[i];
  Result := Self;
end;

function TEnumeration.AddIDs(const AIDs: array of Integer): TEnumeration;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    FItems[i].FID := AIDs[i];
  Result := Self;
end;

constructor TEnumeration.Create(const AName: string; const ATypeInfo: PTypeInfo; const AEnumItemClass: TEnumItemClass);
var
  vTypeName: string;
  vTypeData: PTypeData;
  vValueName: string;
  vFullName: string;
  i: Integer;
  vItem: TEnumItem;
begin
  inherited Create;
  FName := AName;
  FTypeInfo := ATypeInfo;
  FItems := TObjectList<TEnumItem>.Create;

  vTypeName := FTypeInfo^.NameFld.ToString;
  vTypeData := GetTypeData(FTypeInfo);
  FIndexType := vTypeData^.OrdType;
  for i := vTypeData^.MinValue to vTypeData^.MaxValue do
  begin
    vValueName := GetEnumName(FTypeInfo, i);
    vFullName := vTypeName + '.' + vValueName;
    vItem := AEnumItemClass.Create(i, vValueName, vFullName);
    FItems.Add(vItem);
  end;
end;

class function TEnumeration.CreateEnum<T>(const AName: string; const AEnumItemClass: TEnumItemClass): TEnumeration;
begin
  Result := TEnumeration.Create(AName, TypeInfo(T), AEnumItemClass);
end;

destructor TEnumeration.Destroy;
begin
  FTypeInfo := nil;
  FreeAndNil(FItems);

  inherited Destroy;
end;

function TEnumeration.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TEnumeration.GetEnumerator: TEnumerator<TEnumItem>;
begin
  Result := FItems.GetEnumerator;
end;

function TEnumeration.GetItem(const AID: Integer): TEnumItem;
begin
  Result := FItems[AID];
end;

function TEnumeration.GetItemByDisplayText(const AName: string): TEnumItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FItems.Count - 1 do
    if SameText(FItems[i].DisplayText, AName) then
    begin
      Result := FItems[i];
      Break;
    end;
end;

{ TEnumItem }

constructor TEnumItem.Create(const AID: Integer; const AValueName, AFullName: string);
var
  i: Integer;
const
  cCodeStart = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_';
begin
  inherited Create;
  FID := AID;
  FFullName := AFullName;
  FCode := AValueName;
  for i := 1 to Length(FCode) do
    if Pos(Copy(FCode, i, 1), cCodeStart) > 0 then
    begin
      if i > 1 then
        Delete(FCode, 1, i-1);
      Exit;
    end;
end;

{ TStateMachine }

function TStateMachine.AddColors(const AColors: array of TColor): TEnumeration;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    StateByID(i).FColor := AColors[i];
  Result := Self;
end;

function TStateMachine.AddTransitions<T>(const AState: T; const AAvailableStates: array of T): TStateMachine;
var
  vStateID: Integer;
  vState: TState;
  i: Integer;
  vAvailableState: Integer;
begin
  vState := StateByID(ConvertToInt<T>(AState));
  if vState.ID >= 0 then
    for i := Low(AAvailableStates) to High(AAvailableStates) do
      vState.AddAvailableTransition(ConvertToInt<T>(AAvailableStates[i]));
  Result := Self;
end;

function TStateMachine.ConvertToInt<T>(const AValue: T): Integer;
var
  pValue: Pointer;
begin
  pValue := @AValue;
  case FIndexType of
    otSByte: Result := PShortInt(pValue)^;
    otUByte: Result := PByte(pValue)^;
    otSWord: Result := PSmallInt(pValue)^;
    otUWord: Result := PWord(pValue)^;
    otSLong: Result := PInteger(pValue)^;
    otULong: Result := PLongWord(pValue)^;
  end;
end;

function TStateMachine.Get(const AValue: Variant): Integer;
begin
  case FIndexType of
    otSByte: Result := VarAsType(AValue, varShortInt);
    otUByte: Result := VarAsType(AValue, varByte);
    otSWord: Result := VarAsType(AValue, varSmallint);
    otUWord: Result := VarAsType(AValue, varWord);
    otSLong: Result := VarAsType(AValue, varInteger);
    otULong: Result := VarAsType(AValue, varLongWord);
  else
    Result := -1;
  end;
end;

function TStateMachine.StateByID(const AID: Integer): TState;
begin
  Result := TState(FItems[AID]);
end;

{ TState }

procedure TState.AddAvailableTransition(const AAvailableState: Integer);
begin
  if not FAvailableTransitions.Contains(AAvailableState) then
    FAvailableTransitions.Add(AAvailableState);
end;

function TState.CanTransitToState(const ANewState: Variant): Boolean;
begin
  if FID = 0 then
    Result := True
  else
    Result := True;
    //Result := FAvailableTransitions.IndexOf(Pointer(Integer(ANewState))) >= 0;
end;

constructor TState.Create(const AID: Integer; const AValueName, AFullName: string);
begin
  inherited Create(AID, AValueName, AFullName);
  FAvailableTransitions := TList<Integer>.Create;
end;

destructor TState.Destroy;
begin
  FreeAndNil(FAvailableTransitions);
  inherited Destroy;
end;

{ TEnumerations<T> }

function TEnumerations.AddEnum<T>(const AName: string): TEnumeration;
begin
  Result := TEnumeration.CreateEnum<T>(AName, TEnumData);
  AddObject(AName, Result);
end;

{ TStateMachines }

function TStateMachines.AddStateMachine<T>(const AName: string): TStateMachine;
begin
  Result := TStateMachine(TEnumeration.CreateEnum<T>(AName, TState));
  AddObject(AName, Result);
end;

end.
