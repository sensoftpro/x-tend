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

unit uFastClasses;

interface

uses
  Classes, Generics.Collections;

type
  TStringDictionary<T> = class(TEnumerable<T>)
  private
    // Данные
    FDict: TDictionary<string, Integer>;
    FObjects: TList<T>;
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): T;
    function GetKeys: TDictionary<string, Integer>.TKeyCollection;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
    function CreateObjectList: TList<T>; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddObject(const AKey: string; const AObject: T);
    procedure AddOrSetObject(const AKey: string; const AObject: T);
    procedure RemoveObject(const AKey: string);
    procedure Clear; virtual;
    function IndexOfName(const AKey: string): Integer; inline;
    function ObjectByName(const AKey: string): T;
    function TryGetObject(const AKey: string; var AValue: T): Boolean;
    function Exists(const AKey: string): Boolean;
    function ToStrings: TStrings;

    property Keys: TDictionary<string, Integer>.TKeyCollection read GetKeys;
    property Items[const AIndex: Integer]: T read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TObjectStringDictionary<T: class> = class(TStringDictionary<T>)
  protected
    function CreateObjectList: TList<T>; override;
  end;

  TStringList<T> = class
  public
    type
      TStringListItem = class
      public
        Name: string;
        Value: string;
        &Object: T;
        constructor Create(const AName: string; const AObject: T);
      end;

      TObjectEnumerator = class(TEnumerator<T>)
      private
        FList: TStringList<T>;
        FIndex: Integer;
        function GetCurrent: T;
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AList: TStringList<T>);
        property Current: T read GetCurrent;
        function MoveNext: Boolean;
      end;

      TObjectCollection = class(TEnumerable<T>)
      private
        [Weak] FList: TStringList<T>;
        function GetCount: Integer;
      protected
        function DoGetEnumerator: TEnumerator<T>; override;
      public
        constructor Create(const AList: TStringList<T>);
        function GetEnumerator: TObjectEnumerator; reintroduce;
        property Count: Integer read GetCount;
      end;
  private
    FItems: TObjectList<TStringListItem>;
    FNames: TDictionary<string, TStringListItem>;
    FObjects: TDictionary<T, TStringListItem>;
    FObjectCollection: TObjectCollection;

    function GetCount: Integer;
    function GetItem(const AIndex: Integer): TStringListItem;
    function GetObjects: TObjectCollection;
  protected
    procedure DisposeObject(const AObject: T); virtual;
    procedure ClearObjects; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddObject(const AKey: string; const AObject: T);

    procedure RemoveByName(const AName: string);
    procedure RemoveByObject(const AObject: T);
    function ExtractByName(const AName: string): T;

    function Exists(const AName: string): Boolean;
    function ObjectByName(const AName: string): T;
    function GetObject(const AIndex: Integer): T;
    function GetEnumerator: TEnumerator<TStringListItem>;

    property Items[const AIndex: Integer]: TStringListItem read GetItem; default;
    property Objects: TObjectCollection read GetObjects;
    property Count: Integer read GetCount;
  end;

  TObjectStringList<T: class> = class(TStringList<T>)
  protected
    procedure DisposeObject(const AObject: T); override;
    procedure ClearObjects; override;
  end;

implementation

uses
  SysUtils;

{ TStringDictionary<T> }

procedure TStringDictionary<T>.AddObject(const AKey: string; const AObject: T);
var
  vIndex: Integer;
begin
  vIndex := FObjects.Add(AObject);
  FDict.Add(AKey, vIndex);
end;

procedure TStringDictionary<T>.AddOrSetObject(const AKey: string; const AObject: T);
var
  vIndex: Integer;
begin
  if FDict.TryGetValue(AKey, vIndex) then
    FObjects[vIndex] := AObject
  else
    AddObject(AKey, AObject);
end;

procedure TStringDictionary<T>.Clear;
begin
  FDict.Clear;
  FObjects.Clear;
end;

constructor TStringDictionary<T>.Create;
begin
  inherited Create;
  FDict := TDictionary<string, Integer>.Create;
  FObjects := CreateObjectList;
end;

function TStringDictionary<T>.CreateObjectList: TList<T>;
begin
  Result := TList<T>.Create;
end;

destructor TStringDictionary<T>.Destroy;
begin
  FreeAndNil(FDict);
  FreeAndNil(FObjects);
  inherited Destroy;
end;

function TStringDictionary<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := FObjects.GetEnumerator;
end;

function TStringDictionary<T>.Exists(const AKey: string): Boolean;
begin
  Result := FDict.ContainsKey(AKey);
end;

function TStringDictionary<T>.GetCount: Integer;
begin
  Result := FObjects.Count;
end;

function TStringDictionary<T>.GetItem(const AIndex: Integer): T;
begin
  Result := FObjects[AIndex];
end;

function TStringDictionary<T>.GetKeys: TDictionary<string, Integer>.TKeyCollection;
begin
  Result := FDict.Keys;
end;

function TStringDictionary<T>.IndexOfName(const AKey: string): Integer;
begin
  if not FDict.TryGetValue(AKey, Result) then
    Result := -1;
end;

function TStringDictionary<T>.ObjectByName(const AKey: string): T;
var
  vIndex: Integer;
begin
  vIndex := IndexOfName(AKey);
  if vIndex >= 0 then
    Result := FObjects[vIndex]
  else
    Result := Default(T);
end;

procedure TStringDictionary<T>.RemoveObject(const AKey: string);
var
  vRemovingIndex: Integer;
  vIndex: Integer;
  vKey: string;
begin
  if not FDict.TryGetValue(AKey, vRemovingIndex) then
    Exit;

  FDict.Remove(AKey);
  FObjects.Delete(vRemovingIndex);

  // Обновление индексации
  for vKey in FDict.Keys do
  begin
    vIndex := FDict[vKey];
    if vIndex > vRemovingIndex then
      FDict[vKey] := vIndex - 1;
  end;
end;

function TStringDictionary<T>.ToStrings: TStrings;
var
  vKey: string;
begin
  Result := TStringList.Create;
  for vKey in FDict.Keys do
    Result.Add(vKey);
end;

function TStringDictionary<T>.TryGetObject(const AKey: string; var AValue: T): Boolean;
var
  vIndex: Integer;
begin
  Result := FDict.TryGetValue(AKey, vIndex);
  if Result then
    AValue := FObjects[vIndex];
end;

{ TObjectStringDictionary<T> }

function TObjectStringDictionary<T>.CreateObjectList: TList<T>;
begin
  Result := TObjectList<T>.Create;
end;

{ TStringList<T> }

procedure TStringList<T>.AddObject(const AKey: string; const AObject: T);
var
  vItem: TStringListItem;
begin
  vItem := TStringListItem.Create(AKey, AObject);

  FItems.Add(vItem);
  FNames.Add(AKey, vItem);
  FObjects.Add(AObject, vItem);
end;

procedure TStringList<T>.ClearObjects;
begin
end;

constructor TStringList<T>.Create;
begin
  inherited Create;
  FItems := TObjectList<TStringListItem>.Create;
  FNames := TDictionary<string, TStringListItem>.Create;
  FObjects := TDictionary<T, TStringListItem>.Create;
end;

destructor TStringList<T>.Destroy;
begin
  ClearObjects;
  FreeAndNil(FObjectCollection);
  FreeAndNil(FNames);
  FreeAndNil(FObjects);
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TStringList<T>.DisposeObject(const AObject: T);
begin
end;

function TStringList<T>.Exists(const AName: string): Boolean;
begin
  Result := FNames.ContainsKey(AName);
end;

function TStringList<T>.ExtractByName(const AName: string): T;
var
  vItem: TStringListItem;
begin
  if FNames.TryGetValue(AName, vItem) then
  begin
    Result := vItem.&Object;
    FNames.Remove(AName);
    FObjects.Remove(Result);
    FItems.Remove(vItem);
  end;
end;

function TStringList<T>.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TStringList<T>.GetEnumerator: TEnumerator<TStringListItem>;
begin
  Result := FItems.GetEnumerator;
end;

function TStringList<T>.GetItem(const AIndex: Integer): TStringListItem;
begin
  Result := FItems[AIndex];
end;

function TStringList<T>.GetObject(const AIndex: Integer): T;
var
  vItem: TStringListItem;
begin
  vItem := FItems[AIndex];
  if Assigned(vItem) then
    Result := vItem.&Object
  else
    Result := Default(T);
end;

function TStringList<T>.GetObjects: TObjectCollection;
begin
  if FObjectCollection = nil then
    FObjectCollection := TObjectCollection.Create(Self);
  Result := FObjectCollection;
end;

function TStringList<T>.ObjectByName(const AName: string): T;
var
  vItem: TStringListItem;
begin
  if FNames.TryGetValue(AName, vItem) then
    Result := vItem.&Object
  else
    Result := Default(T);
end;

procedure TStringList<T>.RemoveByName(const AName: string);
var
  vItem: TStringListItem;
begin
  if FNames.TryGetValue(AName, vItem) then
  begin
    FNames.Remove(AName);
    FObjects.Remove(vItem.&Object);
    DisposeObject(vItem.&Object);
    FItems.Remove(vItem);
  end;
end;

procedure TStringList<T>.RemoveByObject(const AObject: T);
var
  vItem: TStringListItem;
begin
  if FObjects.TryGetValue(AObject, vItem) then
  begin
    FObjects.Remove(AObject);
    FNames.Remove(vItem.Name);
    DisposeObject(vItem.&Object);
    FItems.Remove(vItem);
  end;
end;

{ TObjectStringList<T> }

procedure TObjectStringList<T>.ClearObjects;
var
  vItem: TStringListItem;
begin
  for vItem in FItems do
    DisposeObject(vItem.&Object);
end;

procedure TObjectStringList<T>.DisposeObject(const AObject: T);
begin
  AObject.DisposeOf;
end;

{ TStringList<T>.TStringListItem }

constructor TStringList<T>.TStringListItem.Create(const AName: string; const AObject: T);
begin
  inherited Create;
  Name := AName;
  &Object := AObject;
end;

{ TStringList<T>.TObjectEnumerator }

constructor TStringList<T>.TObjectEnumerator.Create(const AList: TStringList<T>);
begin
  FIndex := -1;
  FList := AList;
end;

function TStringList<T>.TObjectEnumerator.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TStringList<T>.TObjectEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TStringList<T>.TObjectEnumerator.GetCurrent: T;
begin
  Result := FList.FItems[FIndex].&Object;
end;

function TStringList<T>.TObjectEnumerator.MoveNext: Boolean;
begin
  if FIndex >= FList.Count then
    Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TStringList<T>.TObjectCollection }

constructor TStringList<T>.TObjectCollection.Create(const AList: TStringList<T>);
begin
  inherited Create;
  FList := AList;
end;

function TStringList<T>.TObjectCollection.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TStringList<T>.TObjectCollection.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TStringList<T>.TObjectCollection.GetEnumerator: TObjectEnumerator;
begin
  Result := TObjectEnumerator.Create(FList);
end;

end.

