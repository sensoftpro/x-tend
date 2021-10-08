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

unit uJSON;

{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Classes, SysUtils, Generics.Collections, UITypes, UIConsts;

const
  NullString = '';
  {$EXTERNALSYM NullString}

type
  TInt15 = 0..15;

  TInt32Object = class
  public
    constructor Create(const Value: Integer);
    destructor Destroy; override;
    function IntValue: Integer;
  private
    FValue: Integer;
  end;

  TStringBuffer = class
  private
    FBuffer: String;
    FCount: Integer;
  private
    function CharAt( const Idx: Integer): WideChar;
  public
    constructor Create; overload;
    constructor Create(InitialSize: Integer); overload;
    constructor Create(const Value: String); overload;
    procedure Append(const Value: String); overload;
    procedure Append(const Value: Integer); overload;
    procedure Append(const Value: TStringBuffer); overload;
    property Length: Integer read FCount write FCount;
    procedure Replace(const Original, Replacement: String; const StartIndex: Integer; const Count: Integer);
    function ToString: String; override;
    function Substring(const Ordinal: Integer): String;

    property Chars[ const Index: Integer]: WideChar read CharAt;
  end;

  TJSONAncestor = class;

  TArrayList = class
  private
    FList: TList<TJSONAncestor>;
  private
    function GetValue(Index: Integer): TJSONAncestor;
    procedure SetValue(Index: Integer; Value: TJSONAncestor);
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Element: TJSONAncestor);
    procedure RemoveAt(Index: Integer);
    procedure Remove(Index: Integer); overload;
    procedure Remove(Element: TJSONAncestor); overload;
  public
    property Count: Integer read GetCount;
    property Values[Index: Integer]: TJSONAncestor read GetValue write SetValue; default;
  end;

  TJSONValue = class;
  TJSONString = class;

  TJSONAncestor = class abstract
  public
    constructor Create;
    function Value: UnicodeString; virtual;
    function EstimatedByteSize: Integer; virtual; abstract;
    function ToBytes(const Data: TBytes; const Offset: Integer): Integer; virtual; abstract;
    function ToRawString: string; virtual;
    function Clone: TJSONAncestor; virtual; abstract;
    function GetOwned: Boolean; virtual;
  protected
    function IsNull: Boolean; virtual;
    procedure AddDescendant(const Descendent: TJSONAncestor); virtual; abstract;
    procedure SetOwned(const Own: Boolean); virtual;
  private
    FOwned: Boolean;
  public
    property Null: Boolean read IsNull;
    property Owned: Boolean write SetOwned;
  end;

  TJSONByteReader = class
  public
    constructor Create(const Data: TBytes; const Offset: Integer; const Range: Integer); overload;
    constructor Create(const Data: TBytes; const Offset: Integer; const Range: Integer; const IsUTF8: Boolean); overload;
    function ConsumeByte: Byte; virtual;
    function PeekByte: Byte; virtual;
    function Empty: Boolean; virtual;
    function HasMore(const Size: Integer): Boolean; virtual;
  protected
    function GetOffset: Integer; virtual;
  private
    procedure ConsumeBOM;
    procedure MoveOffset;
  private
    FData: TBytes;
    FOffset: Integer;
    FRange: Integer;
    FIsUTF8: Boolean;
    FUtf8data: TBytes;
    FUtf8offset: Integer;
    FUtf8length: Integer;
  public
    property Offset: Integer read GetOffset;
  end;

  TJSONException = class(Exception)
  public
    constructor Create(const ErrorMessage: UnicodeString);
  private
    const FSerialVersionUID = 1964987864664789863;
  end;

  TJSONPair = class sealed(TJSONAncestor)
  public
    constructor Create; overload;
    constructor Create(const Str: TJSONString; const Value: TJSONValue); overload;
    constructor Create(const Str: UnicodeString; const Value: TJSONValue); overload;
    constructor Create(const Str: UnicodeString; const Value: UnicodeString); overload;
    destructor Destroy; override;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Offset: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function ToRawString: string; override;
    function Clone: TJSONAncestor; override;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
    procedure SetJsonString(const Descendant: TJSONString);
    procedure SetJsonValue(const Val: TJSONValue);
    function GetJsonString: TJSONString;
    function GetJsonValue: TJSONValue;
  private
    FJsonString: TJSONString;
    FJsonValue: TJSONValue;
  public
    property JsonString: TJSONString read GetJsonString write SetJsonString;
    property JsonValue: TJSONValue read GetJsonValue write SetJsonValue;
  end;

  TJSONValue = class abstract(TJSONAncestor)
  end;

  TJSONTrue = class sealed(TJSONValue)
  public
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Offset: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
  end;

  TJSONString = class(TJSONValue)
  public
    class function Hex(const Digit: TInt15): Byte; static;
    constructor Create; overload;
    constructor Create(const Value: UnicodeString); overload;
    destructor Destroy; override;
    procedure AddChar(const Ch: WideChar); virtual;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Idx: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function ToRawString: string; override;
    function Value: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
    function IsNull: Boolean; override;
  protected
    FStrBuffer: TStringBuffer;
  end;

  TJSONNumber = class sealed(TJSONString)
  public
    constructor Create; overload;
    constructor Create(const Value: Double); overload;
    constructor Create(const Value: Integer); overload;
    constructor Create(const Value: Int64); overload;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Idx: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function ToRawString: string; override;
    function Value: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  protected
    constructor Create(const Value: UnicodeString); overload;
    function GetAsDouble: Double;
    function GetAsInt: Integer;
    function GetAsInt64: Int64;
  public
    property AsDouble: Double read GetAsDouble;
    property AsInt: Integer read GetAsInt;
    property AsInt64: Int64 read GetAsInt64;
  end;

  TJSONPairEnumerator = class
  private
    FIndex: Integer;
    FDBXArrayList: TArrayList;
  public
    constructor Create(ADBXArrayList: TArrayList);
    function GetCurrent: TJSONPair; inline;
    function MoveNext: Boolean;
    property Current: TJSONPair read GetCurrent;
  end;

  TJSONArray = class;

  TJSONObject = class sealed(TJSONValue)
  public
    class function HexToDecimal(const Value: Byte): Integer; static;
    class function ParseJSONValue(const Data: TBytes; const Offset: Integer; IsUTF8: Boolean = True): TJSONValue; overload; static;
    class function ParseJSONValue(const Data: TBytes; const Offset: Integer; const Count: Integer; IsUTF8: Boolean = True): TJSONValue; overload; static;
    class function ParseJSONValue(const Data: String): TJSONValue; overload; static;
    class function ParseJSONValue(const Data: UTF8String): TJSONValue; overload; static;
    constructor Create; overload;
    constructor Create(const Pair: TJSONPair); overload;
    function Size: Integer;
    function Get(const I: Integer): TJSONPair; overload;
    function GetEnumerator: TJSONPairEnumerator;
    function Get(const PairName: UnicodeString): TJSONPair; overload;
    destructor Destroy; override;
    function AddPair(const Pair: TJSONPair): TJSONObject; overload;
    function AddPair(const Str: TJSONString; const Val: TJSONValue): TJSONObject; overload;
    function AddPair(const Str: UnicodeString; const Val: TJSONValue): TJSONObject; overload;
    function AddPair(const Str: UnicodeString; const Val: UnicodeString): TJSONObject; overload;
    function RemovePair(const PairName: String): TJSONPair;
    function Contains(const APairName: string): Boolean;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Idx: Integer): Integer; override;
    function Clone: TJSONAncestor; override;
    function Parse(const Data: TBytes; const Pos: Integer): Integer; overload;
    function Parse(const Data: TBytes; const Pos: Integer; const Count: Integer): Integer; overload;
    procedure SetMemberList(AList: TArrayList);

    function ToString: UnicodeString; override;
    function ToRawString: string; override;
    function ToUTF8String: UTF8String;

    function ExtractValue(const AKey: string): TJSONValue;
    function ExtractString(const AKey: string; const APath: string = ''): string;
    function ExtractInteger(const AKey: string; const ADefault: Integer = 0): Integer;
    function ExtractInt64(const AKey: string; const ADefault: Int64 = 0): Int64;
    function ExtractFloat(const AKey: string; const ADefault: Double = 0): Double;
    function ExtractBoolean(const AKey: string): Boolean;
    function ExtractDateTime(const AKey: string): TDateTime;
    function ExtractColor(const AKey: string; const ADefault: TAlphaColor = TAlphaColorRec.Null): Cardinal;
    function ExtractObject(const AKey: string): TJSONObject;
    function ExtractArray(const AKey: string): TJSONArray;
    function ExtractStrings(const AKey: string): TStrings;
    function DelveIntoPath(const APath: string): TJSONValue;

    procedure StoreString(const AKey: string; const AValue: string);
    procedure StoreInteger(const AKey: string; const AValue: Int64);
    procedure StoreFloat(const AKey: string; const AValue: Double);
    procedure StoreBoolean(const AKey: string; const AValue: Boolean);
    procedure StoreDateTime(const AKey: string; const AValue: TDateTime);
    procedure StoreColor(const AKey: string; const AValue: TAlphaColor);
    procedure StoreStrings(const AKey: string; const AValue: TStrings);
    procedure RestoreString(const AKey: string; var AValue: string);
    procedure RestoreInteger(const AKey: string; var AValue: Integer);
    procedure RestoreInt64(const AKey: string; var AValue: Int64);
    procedure RestoreFloat(const AKey: string; var AValue: Double);
    procedure RestoreBoolean(const AKey: string; var AValue: Boolean);
    procedure RestoreDateTime(const AKey: string; var AValue: TDateTime);

    procedure SaveToFile(const AFileName: string);
    procedure SaveToUTF8File(const AFileName: string);
    class function LoadFromFile(const AFileName: string): TJSONObject; static;
    class function LoadFromUTF8File(const AFileName: string): TJSONObject; static;
    class function LoadFromText(const AText: string; const APath: string = ''): TJSONObject; static;
    class function LoadFromUTF8Text(const AText: UTF8String): TJSONObject; static;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
  private
    function Parse(const Br: TJSONByteReader): Integer; overload;
    class procedure ConsumeWhitespaces(const Br: TJSONByteReader); static;
    class function ParseObject(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer; static;
    class function ParsePair(const Br: TJSONByteReader; const Parent: TJSONObject): Integer; static;
    class function ParseArray(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer; static;
    class function ParseValue(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer; static;
    class function ParseNumber(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer; static;
    class function ParseString(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer; static;
  private
    FMembers: TArrayList;
  end;

  TJSONNull = class sealed(TJSONValue)
  public
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Offset: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
    function IsNull: Boolean; override;
  end;

  TJSONFalse = class sealed(TJSONValue)
  public
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Offset: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
  end;

  TJSONArrayEnumerator = class
  private
    FIndex: Integer;
    FArray: TJSONArray;
  public
    constructor Create(AArray: TJSONArray);
    function GetCurrent: TJSONValue; inline;
    function MoveNext: Boolean;
    property Current: TJSONValue read GetCurrent;
  end;

  TJSONArrayObjectEnumerator = class
  private
    FIndex: Integer;
    FArray: TJSONArray;
  public
    constructor Create(AArray: TJSONArray);
    function GetCurrent: TJSONObject; inline;
    function MoveNext: Boolean;
    property Current: TJSONObject read GetCurrent;
  end;

  TJSONArray = class sealed(TJSONValue)
  public
    constructor Create; overload;
    constructor Create(const FirstElem: TJSONValue); overload;
    constructor Create(const FirstElem: TJSONValue; const SecondElem: TJSONValue); overload;
    constructor Create(const FirstElem: String; const SecondElem: String); overload;
    destructor Destroy; override;
    function Size: Integer;
    function Get(const Index: Integer): TJSONValue;
    function Remove(Index: Integer): TJSONValue;
    procedure AddElement(const Element: TJSONValue);
    function Add(const Element: UnicodeString): TJSONArray; overload;
    function Add(const Element: Integer): TJSONArray; overload;
    function Add(const Element: Double): TJSONArray; overload;
    function Add(const Element: Boolean): TJSONArray; overload;
    function Add(const Element: TJSONObject): TJSONArray; overload;
    function Add(const Element: TJSONArray): TJSONArray; overload;
    function EstimatedByteSize: Integer; override;
    procedure SetElements(AList: TArrayList);
    function ToBytes(const Data: TBytes; const Pos: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function ToRawString: string; override;
    function Clone: TJSONAncestor; override;
    function GetEnumerator: TJSONArrayObjectEnumerator;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
    function Pop: TJSONValue;
  private
    FElements: TArrayList;
  end;

procedure SaveTextToFile(const AText: string; const AFileName: string);

implementation

uses
  Math, Types, StrUtils, RegularExpressions;

resourcestring
  SUTF8InvalidHeaderByte = 'UTF8: Type cannot be determined out of header byte at position %s';
  SUTF8UnexpectedByte = 'UTF8: An unexpected continuation byte in %s-byte UTF8 in position %s';
  SUTF8Start = 'UTF8: A start byte not followed by enough continuation bytes in position %s';

const
  HexChars = '0123456789ABCDEF';

procedure SaveTextToFile(const AText: string; const AFileName: string);
begin
  with TStringList.Create do
  begin
    Text := AText;
    SaveToFile(AFileName);
    Free;
  end;
end;

function CreateStrings(const AArray: TJSONArray): TStrings;
var
  i: Integer;
begin
  Result := nil;
  if not Assigned(AArray) then
    Exit;
  if AArray.Size = 0 then
    Exit;

  Result := TStringList.Create;
  for i := 0 to AArray.Size - 1 do
    Result.Add(TJSONString(AArray.Get(i)).Value);
end;

procedure SaveStrings(const AParentObject: TJSONObject; const AKey: string;
  const AValues: TStrings);
var
  i: Integer;
  vArray: TJSONArray;
begin
  if not Assigned(AValues) then
    Exit;
  if AValues.Count = 0 then
    Exit;

  vArray := TJSONArray.Create;
  try
    for i := 0 to AValues.Count - 1 do
      vArray.Add(AValues[i]);
  finally
    AParentObject.AddPair(AKey, vArray);
  end;
end;

function JSONDateToDateTime(const AValue: string): TDateTime;
var
  vMatches: TMatchCollection;
  vGroups: TGroupCollection;
  i: Integer;
  vDateTimeParts: array[0..6] of Word;
begin
  Result := 0;
  try
    vMatches := TRegEx.Matches(AValue, '(\d+)-(\d+)-(\d+)T(\d+):(\d+):(\d+)\.(\d+)Z', [roIgnoreCase]);
    if vMatches.Count = 0 then
      Exit;

    vGroups := vMatches[0].Groups;
    for i := 1 to vGroups.Count - 1 do
      vDateTimeParts[i-1] := StrToIntDef(vGroups[i].Value, 0);
    Result := EncodeDate(vDateTimeParts[0], vDateTimeParts[1], vDateTimeParts[2]) +
      EncodeTime(vDateTimeParts[3], vDateTimeParts[4], vDateTimeParts[5], vDateTimeParts[6]);
  except
    Result := 0;
  end;
end;

function IncrAfter(var Arg: Integer): Integer; inline;
begin
  Result := Arg;
  Inc(Arg);
end;

function StringIsNil(const Str: UnicodeString): Boolean;
begin
  Result := Str = NullString;
end;

function GetUSFormat: TFormatSettings;
begin
  Result := TFormatSettings.Create('en-US');
end;

constructor TInt32Object.Create(const Value: Integer);
begin
  inherited Create;
  FValue := Value;
end;

destructor TInt32Object.Destroy;
begin
  inherited Destroy;
end;

function TInt32Object.IntValue: Integer;
begin
  Result := FValue;
end;

constructor TStringBuffer.Create;
begin
  inherited Create;
end;

constructor TStringBuffer.Create(InitialSize: Integer);
begin
  inherited Create;
  SetLength(FBuffer,InitialSize);
end;

constructor TStringBuffer.Create(const Value: String);
begin
  inherited Create;
  Append( Value );
end;

function TStringBuffer.ToString: String;
begin
  if FCount > System.Length(FBuffer) then
    SetLength(FBuffer,FCount);
  Result := Copy(FBuffer,0,FCount);
end;

function TStringBuffer.Substring(const Ordinal: Integer): String;
begin
  if Ordinal >= FCount then
    Result := ''
  else
    Result := Copy(FBuffer, Ordinal, FCount - Ordinal);
end;

procedure TStringBuffer.Append(const Value: String);
var
  Pos: Integer;
begin
  if FCount+System.Length(Value) > System.Length(FBuffer) then
    SetLength(FBuffer, Math.Max(2*System.Length(FBuffer),System.Length(FBuffer)+System.Length(Value)));
  for Pos := 1 to System.Length(Value) do
    FBuffer[FCount+Pos] := Value[Pos];
  FCount := FCount + System.Length(Value);
end;

procedure TStringBuffer.Append(const Value: Integer);
begin
  Append(IntToStr(Value));
end;

procedure TStringBuffer.Append(const Value: TStringBuffer);
begin
  Append(Value.ToString);
end;

procedure Zap(Buffer: TStringBuffer);
var
  I: Integer;
begin
  for I := Buffer.FCount+1 to Length(Buffer.FBuffer) do
    Buffer.FBuffer[I] := ' ';
end;

procedure TStringBuffer.Replace(const Original, Replacement: string; const StartIndex: Integer; const Count: Integer);
var
  Part: UnicodeString;
begin
  Part := Copy(FBuffer, StartIndex+1, Count);
  Part := ReplaceStr(Part, Original, Replacement);
  Self.FCount := StartIndex;
  Zap(Self);
  Append(Part);
  Zap(Self);
end;

function TStringBuffer.CharAt( const Idx: Integer ): WideChar;
begin
  Result := FBuffer[ Idx + 1 ];
end;

constructor TArrayList.Create;
begin
  inherited;
  FList := TList<TJSONAncestor>.Create;
//  SetLength(FList,30);
end;

destructor TArrayList.Destroy;
begin
  FreeAndNil(FList);
//  SetLength(FList,0);
  inherited;
end;

function TArrayList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TArrayList.GetValue(Index: Integer): TJSONAncestor;
begin
  Result := FList[Index];
end;

procedure TArrayList.SetValue(Index: Integer; Value: TJSONAncestor);
begin
  FList[Index] := Value;
end;

procedure TArrayList.Remove(Element: TJSONAncestor);
begin
  FList.Remove(Element);
end;

procedure TArrayList.Remove(Index: Integer);
begin
  FList.Remove(FList[Index]);
end;

procedure TArrayList.RemoveAt(Index: Integer);
begin
  Remove(Index);
end;

procedure TArrayList.Add(Element: TJSONAncestor);
begin
  FList.Add(Element);
end;

procedure TArrayList.Clear;
begin
  FList.Clear;
end;

constructor TJSONAncestor.Create;
begin
  inherited Create;
  FOwned := True;
end;

function TJSONAncestor.IsNull: Boolean;
begin
  Result := False;
end;

function TJSONAncestor.Value: UnicodeString;
begin
  Result := NullString;
end;

procedure TJSONAncestor.SetOwned(const Own: Boolean);
begin
  FOwned := Own;
end;

function TJSONAncestor.ToRawString: string;
begin
  Result := ToString;
end;

function TJSONAncestor.GetOwned: Boolean;
begin
  Result := FOwned;
end;

constructor TJSONByteReader.Create(const Data: TBytes; const Offset: Integer; const Range: Integer);
begin
  inherited Create;
  self.FData := Data;
  self.FOffset := Offset;
  self.FRange := Range;
  ConsumeBOM;
end;

constructor TJSONByteReader.Create(const Data: TBytes; const Offset: Integer; const Range: Integer; const IsUTF8: Boolean);
begin
  inherited Create;
  self.FData := Data;
  self.FOffset := Offset;
  self.FRange := Range;
  self.FIsUTF8 := IsUTF8;
  if IsUTF8 then
    ConsumeBOM;
end;

procedure TJSONByteReader.ConsumeBOM;
begin
  if FOffset + 3 < FRange then
  begin
    if (FData[FOffset] = Byte(239)) and (FData[FOffset + 1] = Byte(187)) and (FData[FOffset + 2] = Byte(191)) then
    begin
      FIsUTF8 := True;
      FOffset := FOffset + 3;
    end;
  end;
end;

procedure TJSONByteReader.MoveOffset;
begin
  if FUtf8offset < FUtf8length then
    IncrAfter(FUtf8offset)
  else
    IncrAfter(FOffset);
end;

function TJSONByteReader.ConsumeByte: Byte;
var
  Data: Byte;
begin
  Data := PeekByte;
  MoveOffset;
  Result := Data;
end;

function TJSONByteReader.PeekByte: Byte;
var
  Bmp: Int64;
  W1: Integer;
  W2: Integer;
begin
  if not FIsUTF8 then
    Exit(FData[FOffset]);
  if FUtf8offset < FUtf8length then
    Exit(FUtf8data[FUtf8offset]);
  if (FData[FOffset] and (Byte(128))) <> 0 then
  begin
    FUtf8offset := 0;
    if (FData[FOffset] and (Byte(224))) = Byte(192) then
    begin
      if FOffset + 1 >= FRange then
        raise TJSONException.Create(Format(SUTF8Start, [TInt32Object.Create(FOffset)]));
      if (FData[FOffset + 1] and (Byte(192))) <> Byte(128) then
        raise TJSONException.Create(Format(SUTF8UnexpectedByte, [TInt32Object.Create(2),TInt32Object.Create(FOffset + 1)]));
      SetLength(FUtf8data,6);
      FUtf8length := 6;
      FUtf8data[0] := Ord('\');
      FUtf8data[1] := Ord('u');
      FUtf8data[2] := TJSONString.Hex(0);
      FUtf8data[3] := TJSONString.Hex((Byte((FData[FOffset] and Byte(28)))) shr 2);
      FUtf8data[4] := TJSONString.Hex((Byte((Byte(FData[FOffset]) and Byte(3))) shl 2) or (Byte((Byte((FData[FOffset + 1] and Byte(48))) shr 4))));
      FUtf8data[5] := TJSONString.Hex(FData[FOffset + 1] and Byte(15));
      FOffset := FOffset + 2;
    end
    else if (FData[FOffset] and (Byte(240))) = Byte(224) then
    begin
      if FOffset + 2 >= FRange then
        raise TJSONException.Create(Format(SUTF8Start, [TInt32Object.Create(FOffset)]));
      if (FData[FOffset + 1] and (Byte(192))) <> Byte(128) then
        raise TJSONException.Create(Format(SUTF8UnexpectedByte, [TInt32Object.Create(3),TInt32Object.Create(FOffset + 1)]));
      if (FData[FOffset + 2] and (Byte(192))) <> Byte(128) then
        raise TJSONException.Create(Format(SUTF8UnexpectedByte, [TInt32Object.Create(3),TInt32Object.Create(FOffset + 2)]));
      SetLength(FUtf8data,6);
      FUtf8length := 6;
      FUtf8data[0] := Ord('\');
      FUtf8data[1] := Ord('u');
      FUtf8data[2] := TJSONString.Hex(FData[FOffset] and Byte(15));
      FUtf8data[3] := TJSONString.Hex((Byte((FData[FOffset + 1] and Byte(60)))) shr 2);
      FUtf8data[4] := TJSONString.Hex((Byte((Byte(FData[FOffset + 1]) and Byte(3))) shl 2) or (Byte((Byte((FData[FOffset + 2] and Byte(48))) shr 4))));
      FUtf8data[5] := TJSONString.Hex(FData[FOffset + 2] and Byte(15));
      FOffset := FOffset + 3;
    end
    else if (FData[FOffset] and (Byte(248))) = Byte(240) then
    begin
      if FOffset + 3 >= FRange then
        raise TJSONException.Create(Format(SUTF8Start, [TInt32Object.Create(FOffset)]));
      if (FData[FOffset + 1] and (Byte(192))) <> Byte(128) then
        raise TJSONException.Create(Format(SUTF8UnexpectedByte, [TInt32Object.Create(4),TInt32Object.Create(FOffset + 1)]));
      if (FData[FOffset + 2] and (Byte(192))) <> Byte(128) then
        raise TJSONException.Create(Format(SUTF8UnexpectedByte, [TInt32Object.Create(4),TInt32Object.Create(FOffset + 2)]));
      if (FData[FOffset + 3] and (Byte(192))) <> Byte(128) then
        raise TJSONException.Create(Format(SUTF8UnexpectedByte, [TInt32Object.Create(4),TInt32Object.Create(FOffset + 3)]));
      Bmp := FData[FOffset] and Byte(7);
      Bmp := (Bmp shl 6) or (FData[FOffset + 1] and Byte(63));
      Bmp := (Bmp shl 6) or (FData[FOffset + 2] and Byte(63));
      Bmp := (Bmp shl 6) or (FData[FOffset + 3] and Byte(63));
      Bmp := Bmp - 65536;
      W1 := 55296;
      W1 := W1 or ((Integer((Bmp shr 10))) and 2047);
      W2 := 56320;
      W2 := W2 or Integer((Bmp and 2047));
      SetLength(FUtf8data,12);
      FUtf8length := 12;
      FUtf8data[0] := Ord('\');
      FUtf8data[1] := Ord('u');
      FUtf8data[2] := TJSONString.Hex((W1 and 61440) shr 12);
      FUtf8data[3] := TJSONString.Hex((W1 and 3840) shr 8);
      FUtf8data[4] := TJSONString.Hex((W1 and 240) shr 4);
      FUtf8data[5] := TJSONString.Hex(W1 and 15);
      FUtf8data[6] := Ord('\');
      FUtf8data[7] := Ord('u');
      FUtf8data[8] := TJSONString.Hex((W2 and 61440) shr 12);
      FUtf8data[9] := TJSONString.Hex((W2 and 3840) shr 8);
      FUtf8data[10] := TJSONString.Hex((W2 and 240) shr 4);
      FUtf8data[11] := TJSONString.Hex(W2 and 15);
      FOffset := FOffset + 4;
    end
    else
      raise TJSONException.Create(Format(SUTF8InvalidHeaderByte, [TInt32Object.Create(FOffset)]));
    Result := FUtf8data[FUtf8offset];
  end
  else
    Result := FData[FOffset];
end;

function TJSONByteReader.Empty: Boolean;
begin
  Result := (FOffset >= FRange) and (FUtf8offset >= FUtf8length);
end;

function TJSONByteReader.GetOffset: Integer;
begin
  Result := FOffset;
end;

function TJSONByteReader.HasMore(const Size: Integer): Boolean;
begin
  if FOffset + Size < FRange then
    Result := True
  else if FUtf8offset + Size < FUtf8length then
    Result := True
  else
    Result := False;
end;

constructor TJSONException.Create(const ErrorMessage: UnicodeString);
begin
  inherited Create(ErrorMessage);
end;

constructor TJSONPair.Create;
begin
  inherited Create;
end;

constructor TJSONPair.Create(const Str: TJSONString; const Value: TJSONValue);
begin
  inherited Create;
  FJsonString := Str;
  FJsonValue := Value;
end;

constructor TJSONPair.Create(const Str: UnicodeString; const Value: TJSONValue);
begin
  Create(TJSONString.Create(Str), Value);
end;

constructor TJSONPair.Create(const Str: UnicodeString; const Value: UnicodeString);
begin
  Create(TJSONString.Create(Str), TJSONString.Create(Value));
end;

destructor TJSONPair.Destroy;
begin
  if FJsonString <> nil then
    FreeAndNil(FJsonString);
  if (FJsonValue <> nil) and FJsonValue.GetOwned then
    FreeAndNil(FJsonValue);
  inherited Destroy;
end;

procedure TJSONPair.AddDescendant(const Descendant: TJSONAncestor);
begin
  if FJsonString = nil then
    FJsonString := TJSONString(Descendant)
  else
    FJsonValue := TJSONValue(Descendant);
end;

procedure TJSONPair.SetJsonString(const Descendant: TJSONString);
begin
  if Descendant <> nil then
    FJsonString := Descendant;
end;

procedure TJSONPair.SetJsonValue(const Val: TJSONValue);
begin
  if Val <> nil then
    FJsonValue := Val;
end;

function TJSONPair.EstimatedByteSize: Integer;
begin
  Result := 1 + FJsonString.EstimatedByteSize + FJsonValue.EstimatedByteSize;
end;

function TJSONPair.ToBytes(const Data: TBytes; const Offset: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := FJsonString.ToBytes(Data, Offset);
  Data[IncrAfter(Idx)] := Ord(':');
  Result := FJsonValue.ToBytes(Data, Idx);
end;

function TJSONPair.ToRawString: string;
begin
  if (FJsonString <> nil) and (FJsonValue <> nil) then
    Result := FJsonString.ToRawString + ':' + FJsonValue.ToRawString
  else
    Result := NullString;
end;

function TJSONPair.GetJsonString: TJSONString;
begin
  Result := FJsonString;
end;

function TJSONPair.GetJsonValue: TJSONValue;
begin
  Result := FJsonValue;
end;

function TJSONPair.ToString: UnicodeString;
begin
  if (FJsonString <> nil) and (FJsonValue <> nil) then
    Result := FJsonString.ToString + ':' + FJsonValue.ToString
  else
    Result := NullString;
end;

function TJSONPair.Clone: TJSONAncestor;
begin
  Result := TJSONPair.Create(TJSONString(FJsonString.Clone), TJSONValue(FJsonValue.Clone));
end;

procedure TJSONTrue.AddDescendant(const Descendant: TJSONAncestor);
begin
end;

function TJSONTrue.EstimatedByteSize: Integer;
begin
  Result := 4;
end;

function TJSONTrue.ToBytes(const Data: TBytes; const Offset: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := Offset;
  Data[IncrAfter(Idx)] := Ord('t');
  Data[IncrAfter(Idx)] := Ord('r');
  Data[IncrAfter(Idx)] := Ord('u');
  Data[IncrAfter(Idx)] := Ord('e');
  Result := Idx;
end;

function TJSONTrue.ToString: UnicodeString;
begin
  Result := 'true';
end;

function TJSONTrue.Clone: TJSONAncestor;
begin
  Result := TJSONTrue.Create;
end;

class function TJSONString.Hex(const Digit: TInt15): Byte;
begin
  Result := byte(HexChars[1 + Digit]);
end;

constructor TJSONString.Create;
begin
  inherited Create;
end;

constructor TJSONString.Create(const Value: UnicodeString);
begin
  inherited Create;
  FStrBuffer := TStringBuffer.Create(Value);
end;

destructor TJSONString.Destroy;
begin
  FreeAndNil(FStrBuffer);
  inherited Destroy;
end;

procedure TJSONString.AddChar(const Ch: WideChar);
begin
  FStrBuffer.Append(Ch);
end;

procedure TJSONString.AddDescendant(const Descendant: TJSONAncestor);
begin
end;

function TJSONString.IsNull: Boolean;
begin
  if FStrBuffer = nil then
    Exit(True);
  Result := False;
end;

function TJSONString.EstimatedByteSize: Integer;
begin
  if Null then
    Exit(4);
  Result := 2 + 6 * FStrBuffer.Length;
end;

function TJSONString.ToBytes(const Data: TBytes; const Idx: Integer): Integer;
var
  Offset: Integer;
  Index: Integer;
  Count: Integer;
  CurrentChar: WideChar;
  UnicodeValue: Integer;
begin
  Offset := Idx;
  if Null then
  begin
    Data[IncrAfter(Offset)] := Ord('n');
    Data[IncrAfter(Offset)] := Ord('u');
    Data[IncrAfter(Offset)] := Ord('l');
    Data[IncrAfter(Offset)] := Ord('l');
  end
  else
  begin
    Data[IncrAfter(Offset)] := Ord('"');
    Index := 0;
    Count := FStrBuffer.Length;
    while Index < Count do
    begin
      CurrentChar := FStrBuffer.Chars[IncrAfter(Index)];
      case CurrentChar of
        '"':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('"');
          end;
        '\':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('\');
          end;
        '/':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('/');
          end;
        #$8:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('b');
          end;
        #$c:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('f');
          end;
        #$a:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('n');
          end;
        #$d:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('r');
          end;
        #$9:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('t');
          end;
        else
          if (CurrentChar < WideChar(32)) or (CurrentChar > WideChar(127)) then
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('u');
            UnicodeValue := Ord(CurrentChar);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 61440) shr 12);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 3840) shr 8);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 240) shr 4);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 15));
          end
          else
            Data[IncrAfter(Offset)] := Ord(CurrentChar);
      end;
    end;
    Data[IncrAfter(Offset)] := Ord('"');
  end;
  Result := Offset;
end;

function TJSONString.ToRawString: string;
var
  vIndex: Integer;
  vCount: Integer;
  vCurrentChar: WideChar;
begin
  if not Assigned(FStrBuffer) then
    Result := NullString
  else begin
    Result := '';
    vIndex := 0;
    vCount := FStrBuffer.Length;
    while vIndex < vCount do
    begin
      vCurrentChar := FStrBuffer.Chars[IncrAfter(vIndex)];
      case vCurrentChar of
        '"': Result := Result + '\"';
        '\': Result := Result + '\\';
        '/': Result := Result + '\/';
        #$8: Result := Result + '\b';
        #$9: Result := Result + '\t';
        #$A: Result := Result + '\n';
        #$C: Result := Result + '\f';
        #$D: Result := Result + '\r';
      else
        Result := Result + vCurrentChar;
      end;
    end;

    Result := '"' + Result + '"';
  end;
end;

function TJSONString.ToString: UnicodeString;
var
  vIndex: Integer;
  vCount: Integer;
  vCurrentChar: WideChar;
  vUnicodeValue: Integer;
begin
  if not Assigned(FStrBuffer) then
    Result := NullString
  else begin
    Result := '';
    vIndex := 0;
    vCount := FStrBuffer.Length;
    while vIndex < vCount do
    begin
      vCurrentChar := FStrBuffer.Chars[IncrAfter(vIndex)];
      case vCurrentChar of
        '"': Result := Result + '\"';
        '\': Result := Result + '\\';
        '/': Result := Result + '\/';
        #$8: Result := Result + '\b';
        #$9: Result := Result + '\t';
        #$A: Result := Result + '\n';
        #$C: Result := Result + '\f';
        #$D: Result := Result + '\r';
      else
        if (vCurrentChar < WideChar(32)) or (vCurrentChar > WideChar(127)) then
        begin
          vUnicodeValue := Ord(vCurrentChar);
          Result := Result + '\u' + HexChars[1 + (vUnicodeValue and 61440) shr 12] +
            HexChars[1 + (vUnicodeValue and 3840) shr 8] + HexChars[1 + (vUnicodeValue and 240) shr 4] +
            HexChars[1 + (vUnicodeValue and 15)];
        end
        else
          Result := Result + vCurrentChar;
      end;
    end;

    Result := '"' + Result + '"';
  end;
end;

function TJSONString.Value: UnicodeString;
begin
  if FStrBuffer = nil then
    Result := NullString
  else
    Result := FStrBuffer.ToString;
end;

function TJSONString.Clone: TJSONAncestor;
begin
  if FStrBuffer = nil then
    Result := TJSONString.Create
  else
    Result := TJSONString.Create(Value);
end;

constructor TJSONNumber.Create;
begin
  inherited Create('');
end;

function JsonFloat(Value: Double): string;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings.DecimalSeparator := '.';
  Result := FloatToStr(Value, FormatSettings);
end;

function JsonToFloat(const DotValue: String): double;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings.DecimalSeparator := '.';
  Result := StrToFloat(DotValue, FormatSettings);
end;

constructor TJSONNumber.Create(const Value: Double);
begin
  inherited Create(JsonFloat(Value));
end;

constructor TJSONNumber.Create(const Value: UnicodeString);
begin
  inherited Create(Value);
end;

constructor TJSONNumber.Create(const Value: Int64);
begin
  inherited Create(IntToStr(Value));
end;

constructor TJSONNumber.Create(const Value: Integer);
begin
  inherited Create(IntToStr(Value));
end;

function TJSONNumber.EstimatedByteSize: Integer;
begin
  Result := FStrBuffer.Length;
end;

function TJSONNumber.ToBytes(const Data: TBytes; const Idx: Integer): Integer;
var
  Offset: Integer;
  Index: Integer;
  Count: Integer;
  CurrentChar: WideChar;
begin
  Offset := Idx;
  Index := 0;
  Count := FStrBuffer.Length;
  while Index < Count do
  begin
    CurrentChar := FStrBuffer.Chars[IncrAfter(Index)];
    Data[IncrAfter(Offset)] := Ord(CurrentChar);
  end;
  Result := Offset;
end;

function TJSONNumber.ToRawString: string;
begin
  Result := FStrBuffer.ToString;
end;

function TJSONNumber.ToString: UnicodeString;
begin
  Result := FStrBuffer.ToString;
end;

function TJSONNumber.Value: UnicodeString;
var
  BuffStr: String;
begin
  BuffStr := FStrBuffer.ToString;
  if (FStrBuffer.Length > 11) and (AnsiPos('.', BuffStr) = 0) then
    Result := IntToStr(GetAsInt64)
  else
    Result := FloatToStr(JsonToFloat(BuffStr));
end;

function TJSONNumber.Clone: TJSONAncestor;
begin
  Result := TJSONNumber.Create(ToString);
end;

function TJSONNumber.GetAsDouble: Double;
begin
  Result := JsonToFloat(FStrBuffer.ToString);
end;

function TJSONNumber.GetAsInt: Integer;
begin
  Result := StrToInt(FStrBuffer.ToString);
end;

function TJSONNumber.GetAsInt64: Int64;
begin
  Result := StrToInt64(FStrBuffer.ToString);
end;

class function TJSONObject.HexToDecimal(const Value: Byte): Integer;
begin
  if Value > Ord('9') then
  begin
    if Value > Ord('F') then
      Exit(Value - Ord('a') + 10)
    else
      Exit(Value - Ord('A') + 10);
  end;

  Result := Value - Ord('0');
end;

class function TJSONObject.LoadFromFile(const AFileName: string): TJSONObject;
var
  vStream: TStringStream;
begin
  vStream := TStringStream.Create;
  try
    vStream.LoadFromFile(AFileName);
    vStream.Position := 0;
    Result := LoadFromText(vStream.DataString);
  finally
    vStream.Free;
  end;
end;

// Используем разделитель "/"
class function TJSONObject.LoadFromText(const AText: string; const APath: string = ''): TJSONObject;
begin
  try
    Result := TJSONObject(ParseJSONValue(AText));
    if Trim(APath) <> '' then
      Result := TJSONObject(Result.DelveIntoPath(APath));
  except
    Result := nil;
  end;
end;

class function TJSONObject.LoadFromUTF8File(
  const AFileName: string): TJSONObject;
var
  vStream: TStringStream;
begin
  vStream := TStringStream.Create('', TEncoding.UTF8);
  try
    vStream.LoadFromFile(AFileName);
    vStream.Position := 0;
    Result := LoadFromText(vStream.DataString);
  finally
    vStream.Free;
  end;
end;

class function TJSONObject.LoadFromUTF8Text(const AText: UTF8String): TJSONObject;
begin
  try
    Result := TJSONObject(ParseJSONValue(AText));
  except
    Result := nil;
  end;
end;

class function TJSONObject.ParseJSONValue(const Data: TBytes; const Offset: Integer; IsUTF8: Boolean): TJSONValue;
begin
  Result := ParseJSONValue(Data, Offset, Length(Data), IsUTF8);
end;

class function TJSONObject.ParseJSONValue(const Data: TBytes; const Offset: Integer;
                                          const Count: Integer; IsUTF8: Boolean): TJSONValue;
var
  Parent: TJSONArray;
  Answer: TJSONValue;
  Br: TJSONByteReader;
begin
  Parent := TJSONArray.Create;
  Answer := nil;
  Br := TJSONByteReader.Create(Data, Offset, Count, IsUTF8);
  try
    ConsumeWhitespaces(Br);
    if (ParseValue(Br, Parent) = Count) and (Parent.Size = 1) then
      Answer := Parent.Pop;
    Result := Answer;
  finally
    Parent.Free;
    Br.Free;
  end;
end;

class function TJSONObject.ParseJSONValue(const Data: String): TJSONValue;
begin
  Result := ParseJSONValue(TEncoding.UTF8.GetBytes(Data), 0, True);
end;

class function TJSONObject.ParseJSONValue(const Data: UTF8String): TJSONValue;
begin
  Result := ParseJSONValue(BytesOf(Data), 0, True);
end;

constructor TJSONObject.Create;
begin
  inherited Create;
  FMembers := TArrayList.Create;
end;

constructor TJSONObject.Create(const Pair: TJSONPair);
begin
  Create;
  if Pair <> nil then
    FMembers.Add(Pair);
end;

procedure TJSONObject.SaveToFile(const AFileName: string);
var
  vStream: TStringStream;
begin
  vStream := TStringStream.Create(ToString);
  try
    vStream.SaveToFile(AFileName);
  finally
    vStream.Free;
  end;
end;

procedure TJSONObject.SaveToUTF8File(const AFileName: string);
var
  vStream: TStringStream;
begin
  vStream := TStringStream.Create(ToString, TEncoding.UTF8, False);
  try
    vStream.SaveToFile(AFileName);
  finally
    vStream.Free;
  end;
end;

procedure TJSONObject.SetMemberList(AList: TArrayList);
begin
  FMembers.Free;
  FMembers := AList;
end;

function TJSONObject.Size: Integer;
begin
  Result := FMembers.Count;
end;

procedure TJSONObject.StoreBoolean(const AKey: string; const AValue: Boolean);
begin
  if AValue then
    AddPair(AKey, TJSONTrue.Create)
  else
    AddPair(AKey, TJSONFalse.Create);
end;

procedure TJSONObject.StoreColor(const AKey: string; const AValue: TAlphaColor);
begin
  AddPair(AKey, AlphaColorToString(AValue));
end;

procedure TJSONObject.StoreDateTime(const AKey: string;
  const AValue: TDateTime);
var
  vDatePart: array[0..6] of Word;
begin
  DecodeDate(AValue, vDatePart[0], vDatePart[1], vDatePart[2]);
  DecodeTime(AValue, vDatePart[3], vDatePart[4], vDatePart[5], vDatePart[6]);
  AddPair(AKey, Format('%d-%d-%dT%d:%d:%d.%dZ',
    [vDatePart[0], vDatePart[1], vDatePart[2], vDatePart[3],
     vDatePart[4], vDatePart[5], vDatePart[6]]));
end;

procedure TJSONObject.StoreFloat(const AKey: string; const AValue: Double);
begin
  AddPair(AKey, TJSONNumber.Create(AValue));
end;

procedure TJSONObject.StoreInteger(const AKey: string; const AValue: Int64);
begin
  AddPair(AKey, TJSONNumber.Create(AValue));
end;

procedure TJSONObject.StoreString(const AKey, AValue: string);
begin
  AddPair(AKey, AValue);
end;

procedure TJSONObject.StoreStrings(const AKey: string; const AValue: TStrings);
begin
  if Assigned(AValue) then
    SaveStrings(Self, AKey, AValue);
end;

function TJSONObject.Get(const I: Integer): TJSONPair;
begin
//{$IFDEF DEVELOPERS}
//  // JSONObjects are unordered pairs, so do not depend on the index
//  // of a pair.  Here we allow index to be used only in a special case.
//  Assert((I = 0) and (Size <= 1));
//{$ENDIF}
  if (I >= 0) and (I < Size) then
    Result := TJSONPair(FMembers[I])
  else
    Result := nil;
end;

function TJSONObject.Get(const PairName: UnicodeString): TJSONPair;
var
  Candidate: TJSONPair;
  I: Integer;
begin
  for i := 0 to Size - 1 do
  begin
    Candidate := TJSONPair(FMembers[I]);
    if (Candidate.JsonString.Value = PairName) then
      Exit(Candidate);
  end;
  Result := nil;
end;

function TJSONObject.GetEnumerator: TJSONPairEnumerator;
begin
  Result := TJSONPairEnumerator.Create(FMembers);
end;

destructor TJSONObject.Destroy;
var
  Member: TJSONAncestor;
  I: Integer;
begin
  if FMembers <> nil then
  begin
    for i := 0 to FMembers.Count - 1 do
    begin
      Member := TJSONAncestor(FMembers[I]);
      if Member.GetOwned then
        Member.Free;
    end;
    FreeAndNil(FMembers);
  end;
  inherited Destroy;
end;

function TJSONObject.AddPair(const Pair: TJSONPair): TJSONObject;
begin
  if Pair <> nil then
    AddDescendant(Pair);
  Result := self;
end;

function TJSONObject.AddPair(const Str: TJSONString; const Val: TJSONValue): TJSONObject;
begin
  if (Str <> nil) and (Val <> nil) then
    AddPair(TJSONPair.Create(Str, Val));
  Result := self;
end;

function TJSONObject.AddPair(const Str: UnicodeString; const Val: TJSONValue): TJSONObject;
begin
  if (not StringIsNil(Str)) and (Val <> nil) then
    AddPair(TJSONPair.Create(Str, Val));
  Result := self;
end;

function TJSONObject.AddPair(const Str: UnicodeString; const Val: UnicodeString): TJSONObject;
begin
  if (not StringIsNil(Str)) and (not StringIsNil(Val)) then
    AddPair(TJSONPair.Create(Str, Val));
  Result := self;
end;

procedure TJSONObject.AddDescendant(const Descendant: TJSONAncestor);
begin
  FMembers.Add(Descendant);
end;

function TJSONObject.EstimatedByteSize: Integer;
var
  Size: Integer;
  I: Integer;
begin
  Size := 1;
  for i := 0 to FMembers.Count - 1 do
    Size := Size + (TJSONAncestor(FMembers[I])).EstimatedByteSize + 1;
  if Size = 1 then
    Exit(2);
  Result := Size;
end;

function TJSONObject.ExtractArray(const AKey: string): TJSONArray;
begin
  Result := TJSONArray(ExtractValue(AKey));
end;

function TJSONObject.ExtractBoolean(const AKey: string): Boolean;
var
  vValue: TJSONValue;
  vStrBool: string;
begin
  vValue := ExtractValue(AKey);
  if vValue is TJSONTrue then
    Result := True
  else if vValue is TJSONFalse then
    Result := False
  else begin
    vStrBool := ExtractString(AKey);
    if vStrBool = 'true' then
      Result := True
    else if vStrBool = 'false'  then
      Result := False
    else
      Result := Boolean(StrToIntDef(vStrBool, 0));
  end;
end;

function TJSONObject.ExtractColor(const AKey: string; const ADefault: TAlphaColor): Cardinal;
begin
  Result := StringToAlphaColor(Trim(ExtractString(AKey)));
  if (Result = TAlphaColorRec.Null) and (ADefault <> TAlphaColorRec.Null) then
    Result := ADefault;
end;

function TJSONObject.ExtractDateTime(const AKey: string): TDateTime;
var
  vStrDate: string;
begin
  vStrDate := ExtractString(AKey);
  if vStrDate <> '' then
    Result := JSONDateToDateTime(vStrDate)
  else
    Result := 0;
end;

function TJSONObject.ExtractFloat(const AKey: string; const ADefault: Double = 0): Double;
var
  vValue: TJSONValue;
begin
  vValue := ExtractValue(AKey);
  if Assigned(vValue) then
    Result := TJSONNumber(vValue).AsDouble
  else
    Result := ADefault;
end;

function TJSONObject.ExtractInt64(const AKey: string; const ADefault: Int64 = 0): Int64;
var
  vValue: TJSONValue;
begin
  vValue := ExtractValue(AKey);
  if Assigned(vValue) then
    Result := TJSONNumber(vValue).AsInt64
  else
    Result := ADefault;
end;

function TJSONObject.ExtractInteger(const AKey: string; const ADefault: Integer = 0): Integer;
var
  vValue: TJSONValue;
begin
  vValue := ExtractValue(AKey);
  if Assigned(vValue) then
    Result := TJSONNumber(vValue).AsInt
  else
    Result := ADefault;
end;

function TJSONObject.ExtractObject(const AKey: string): TJSONObject;
begin
  Result := TJSONObject(ExtractValue(AKey))
end;

function TJSONObject.ExtractString(const AKey: string; const APath: string = ''): string;
var
  vParent: TJSONObject;
  vValue: TJSONValue;
begin
  Result := '';

  if Trim(APath) <> '' then
    vParent := TJSONObject(DelveIntoPath(APath))
  else
    vParent := Self;

  if not Assigned(vParent) then
    Exit;

  vValue := vParent.ExtractValue(AKey);
  if Assigned(vValue) then
    Result := TJSONString(vValue).Value;
end;

function TJSONObject.ExtractStrings(const AKey: string): TStrings;
var
  vArray: TJSONArray;
  i: Integer;
  vValue: string;
begin
  vArray := TJSONArray(ExtractValue(AKey));
  if Assigned(vArray) then
  begin
    Result := TStringList.Create;
    for i := 0 to vArray.Size - 1 do
    begin
      vValue := TJSONString(vArray.Get(i)).Value;
      if Result.IndexOf(vValue) < 0 then
        Result.Add(vValue);
    end;
  end
  else
    Result := nil;
end;

function TJSONObject.ExtractValue(const AKey: string): TJSONValue;
var
  vPair: TJSONPair;
begin
  vPair := Get(AKey);
  if Assigned(vPair) then
    Result := vPair.JsonValue
  else
    Result := nil;
end;

function TJSONObject.ToBytes(const Data: TBytes; const Idx: Integer): Integer;
var
  Offset: Integer;
  Size: Integer;
  I: Integer;
begin
  Offset := Idx;
  Size := FMembers.Count;
  Data[IncrAfter(Offset)] := Ord('{');
  if Size > 0 then
    Offset := (TJSONAncestor(FMembers[0])).ToBytes(Data, Offset);
  for i := 1 to FMembers.Count - 1 do
  begin
    Data[IncrAfter(Offset)] := Ord(',');
    Offset := (TJSONAncestor(FMembers[I])).ToBytes(Data, Offset);
  end;
  Data[IncrAfter(Offset)] := Ord('}');
  Result := Offset;
end;

function TJSONObject.ToRawString: string;
var
  Buf: TStringBuffer;
  Size: Integer;
  I: Integer;
begin
  Size := FMembers.Count;
  Buf := TStringBuffer.Create;
  try
    Buf.Append('{');
    if Size > 0 then
      Buf.Append(FMembers[0].ToRawString);
    for I := 1 to Size - 1 do
    begin
      Buf.Append(',');
      Buf.Append(FMembers[I].ToRawString);
    end;
    Buf.Append('}');
    Result := Buf.ToString;
  finally
    Buf.Free;
  end;
end;

function TJSONObject.Clone: TJSONAncestor;
var
  Data: TJSONObject;
  I: Integer;
begin
  Data := TJSONObject.Create;
  for I := 0 to FMembers.Count - 1 do
    Data.AddPair(TJSONPair(Get(I).Clone));
  Result := Data;
end;

function TJSONObject.Parse(const Data: TBytes; const Pos: Integer): Integer;
var
  Offset: Integer;
  Count: Integer;
begin
  Count := Length(Data);
  Offset := Parse(Data, Pos, Count);
  if Offset = Count then
    Result := Count
  else if Offset < 0 then
    Result := Offset
  else
    Result := -Offset;
end;

function TJSONObject.Parse(const Data: TBytes; const Pos: Integer; const Count: Integer): Integer;
var
  Br: TJSONByteReader;
begin
  if (Data = nil) or (Pos < 0) or (Pos >= Count) then
    Exit(-1);
  Br := TJSONByteReader.Create(Data, Pos, Count);
  try
    Result := Parse(Br);
  finally
    Br.Free;
  end;
end;

function TJSONObject.Parse(const Br: TJSONByteReader): Integer;
var
  SepPos: Integer;
  PairExpected: Boolean;
begin
  ConsumeWhitespaces(Br);
  if Br.Empty then
    Exit(-Br.Offset);
  if Br.PeekByte <> Ord('{') then
    Exit(-Br.Offset);
  Br.ConsumeByte;
  ConsumeWhitespaces(Br);
  if Br.Empty then
    Exit(-Br.Offset);
  PairExpected := False;
  while PairExpected or (Br.PeekByte <> Ord('}')) do
  begin
    SepPos := ParsePair(Br, self);
    if SepPos <= 0 then
      Exit(SepPos);
    ConsumeWhitespaces(Br);
    if Br.Empty then
      Exit(-Br.Offset);
    PairExpected := False;
    if Br.PeekByte = Ord(',') then
    begin
      Br.ConsumeByte;
      ConsumeWhitespaces(Br);
      PairExpected := True;
      if Br.PeekByte = Ord('}') then
        Exit(-Br.Offset);
    end;
  end;
  Br.ConsumeByte;
  ConsumeWhitespaces(Br);
  Result := Br.Offset;
end;

class procedure TJSONObject.ConsumeWhitespaces(const Br: TJSONByteReader);
var
  Current: Byte;
begin
  while not Br.Empty do
  begin
    Current := Br.PeekByte;
    case Current of
      32,
      9,
      10,
      13:
        Br.ConsumeByte;
      else
        Exit;
    end;
  end;
end;

function TJSONObject.Contains(const APairName: string): Boolean;
var
  i: Integer;
begin
  for i := 0 to Size - 1 do
    if (TJSONPair(FMembers[i]).JsonString.Value = APairName) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

class function TJSONObject.ParseObject(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer;
var
  JsonObj: TJSONObject;
begin
  JsonObj := TJSONObject.Create;
  Parent.AddDescendant(JsonObj);
  Result := JsonObj.Parse(Br);
end;

class function TJSONObject.ParsePair(const Br: TJSONByteReader; const Parent: TJSONObject): Integer;
var
  Pair: TJSONPair;
  CommaPos: Integer;
begin
  Pair := TJSONPair.Create;
  Parent.AddDescendant(Pair);
  CommaPos := ParseString(Br, Pair);
  if CommaPos > 0 then
  begin
    ConsumeWhitespaces(Br);
    if Br.Empty then
      Exit(-Br.Offset);
    if Br.PeekByte <> Ord(':') then
      Exit(-Br.Offset);
    Br.ConsumeByte;
    ConsumeWhitespaces(Br);
    CommaPos := ParseValue(Br, Pair);
  end;
  Result := CommaPos;
end;

class function TJSONObject.ParseArray(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer;
var
  ValueExpected: Boolean;
  JsonArray: TJSONArray;
  Pos: Integer;
begin
  ConsumeWhitespaces(Br);
  if Br.Empty then
    Exit(-Br.Offset);
  if Br.PeekByte <> Ord('[') then
    Exit(-Br.Offset);
  Br.ConsumeByte;
  JsonArray := TJSONArray.Create;
  Parent.AddDescendant(JsonArray);
  ValueExpected := False;
  while ValueExpected or (Br.PeekByte <> Ord(']')) do
  begin
    ConsumeWhitespaces(Br);
    Pos := ParseValue(Br, JsonArray);
    if Pos <= 0 then
      Exit(Pos);
    ConsumeWhitespaces(Br);
    if Br.Empty then
      Exit(-Br.Offset);
    ValueExpected := False;
    if Br.PeekByte = Ord(',') then
    begin
      Br.ConsumeByte;
      ValueExpected := True;
    end
    else if Br.PeekByte <> Ord(']') then
      Exit(-Br.Offset);
  end;
  Br.ConsumeByte;
  ConsumeWhitespaces(Br);
  Result := Br.Offset;
end;

class function TJSONObject.ParseValue(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer;
var
  Pos: Integer;
begin
  Pos := Br.Offset;
  if Br.Empty then
    Exit(-Pos);
  case Br.PeekByte of
    Ord('"'):
      Exit(ParseString(Br, Parent));
    Ord('-'),
    Ord('0'),
    Ord('1'),
    Ord('2'),
    Ord('3'),
    Ord('4'),
    Ord('5'),
    Ord('6'),
    Ord('7'),
    Ord('8'),
    Ord('9'):
      Exit(ParseNumber(Br, Parent));
    Ord('{'):
      Exit(ParseObject(Br, Parent));
    Ord('['):
      Exit(ParseArray(Br, Parent));
    Ord('t'):
      begin
        if not Br.HasMore(3) then
          Exit(-Pos);
        Br.ConsumeByte;
        if (Br.ConsumeByte <> Ord('r')) or (Br.ConsumeByte <> Ord('u')) or (Br.ConsumeByte <> Ord('e')) then
          Exit(-Pos);
        Parent.AddDescendant(TJSONTrue.Create);
        Exit(Br.Offset);
      end;
    Ord('f'):
      begin
        if not Br.HasMore(4) then
          Exit(-Pos);
        Br.ConsumeByte;
        if (Br.ConsumeByte <> Ord('a')) or (Br.ConsumeByte <> Ord('l')) or (Br.ConsumeByte <> Ord('s')) or (Br.ConsumeByte <> Ord('e')) then
          Exit(-Pos);
        Parent.AddDescendant(TJSONFalse.Create);
        Exit(Br.Offset);
      end;
    Ord('n'):
      begin
        if not Br.HasMore(3) then
          Exit(-Pos);
        Br.ConsumeByte;
        if (Br.ConsumeByte <> Ord('u')) or (Br.ConsumeByte <> Ord('l')) or (Br.ConsumeByte <> Ord('l')) then
          Exit(-Pos);
        Parent.AddDescendant(TJSONNull.Create);
        Exit(Br.Offset);
      end;
  end;
  Result := -Pos;
end;

function TJSONObject.RemovePair(const PairName: String): TJSONPair;
var
  Candidate: TJSONPair;
  I: Integer;
begin
  for I := 0 to Size - 1 do
  begin
    Candidate := TJSONPair(FMembers[I]);
    if (Candidate.JsonString.Value = PairName) then
    begin
      FMembers.RemoveAt(i);
      Exit(Candidate);
    end;
  end;
  Result := nil;
end;

procedure TJSONObject.RestoreBoolean(const AKey: string; var AValue: Boolean);
var
  vStrBool: string;
begin
  vStrBool := ExtractString(AKey);
  if vStrBool = 'true' then
    AValue := True
  else if vStrBool = 'false' then
    AValue := False
  else if vStrBool <> '' then
    AValue := Boolean(StrToIntDef(vStrBool, 0));
end;

procedure TJSONObject.RestoreDateTime(const AKey: string;
  var AValue: TDateTime);
var
  vStrDate: string;
begin
  vStrDate := ExtractString(AKey);
  if vStrDate <> '' then
    AValue := JSONDateToDateTime(vStrDate);
end;

procedure TJSONObject.RestoreFloat(const AKey: string; var AValue: Double);
var
  vValue: TJSONValue;
begin
  vValue := ExtractValue(AKey);
  if Assigned(vValue) then
    AValue := TJSONNumber(vValue).AsDouble;
end;

procedure TJSONObject.RestoreInt64(const AKey: string; var AValue: Int64);
var
  vValue: TJSONValue;
begin
  vValue := ExtractValue(AKey);
  if Assigned(vValue) then
    AValue := TJSONNumber(vValue).AsInt64;
end;

procedure TJSONObject.RestoreInteger(const AKey: string; var AValue: Integer);
var
  vValue: TJSONValue;
begin
  vValue := ExtractValue(AKey);
  if Assigned(vValue) then
    AValue := TJSONNumber(vValue).AsInt;
end;

procedure TJSONObject.RestoreString(const AKey: string; var AValue: string);
var
  vValue: TJSONValue;
begin
  vValue := ExtractValue(AKey);
  if Assigned(vValue) then
    AValue := TJSONString(vValue).Value;
end;

class function TJSONObject.ParseNumber(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer;
var
  Nb: TJSONNumber;
  Consume: Boolean;
  Exponent: Boolean;
  OneAdded: Boolean;
begin
  Nb := TJSONNumber.Create;
  Parent.AddDescendant(Nb);
  if Br.PeekByte = Ord('-') then
  begin
    Nb.AddChar('-');
    Br.ConsumeByte;
    if Br.Empty then
      Exit(-1);
  end;
  if Br.PeekByte = Ord('0') then
  begin
    Nb.AddChar('0');
    Br.ConsumeByte;
    if Br.Empty then
      Exit(Br.Offset);
    case Br.PeekByte of
      Ord('0'),
      Ord('1'),
      Ord('2'),
      Ord('3'),
      Ord('4'),
      Ord('5'),
      Ord('6'),
      Ord('7'),
      Ord('8'),
      Ord('9'):
        Exit(-Br.Offset);
    end;
  end;
  Consume := True;
  while Consume do
    case Br.PeekByte of
      Ord('0'),
      Ord('1'),
      Ord('2'),
      Ord('3'),
      Ord('4'),
      Ord('5'),
      Ord('6'),
      Ord('7'),
      Ord('8'),
      Ord('9'):
        begin
          Nb.AddChar(WideChar(Br.ConsumeByte));
          if Br.Empty then
            Exit(Br.Offset);
        end;
      else
        Consume := False;
    end;
  Exponent := False;
  if Br.PeekByte = Ord('.') then
  begin
    Nb.AddChar('.');
    Br.ConsumeByte;
    if Br.Empty then
      Exit(-Br.Offset);
  end
  else if (Br.PeekByte = Ord('e')) or (Br.PeekByte = Ord('E')) then
  begin
    Nb.AddChar(WideChar(Br.ConsumeByte));
    Exponent := True;
    if Br.Empty then
      Exit(-Br.Offset);
    if (Br.PeekByte = Ord('-')) or (Br.PeekByte = Ord('+')) then
    begin
      Nb.AddChar(WideChar(Br.ConsumeByte));
      if Br.Empty then
        Exit(-Br.Offset);
    end;
  end
  else
    Exit(Br.Offset);
  OneAdded := False;
  Consume := True;
  while Consume do
    case Br.PeekByte of
      Ord('0'),
      Ord('1'),
      Ord('2'),
      Ord('3'),
      Ord('4'),
      Ord('5'),
      Ord('6'),
      Ord('7'),
      Ord('8'),
      Ord('9'):
        begin
          Nb.AddChar(WideChar(Br.ConsumeByte));
          OneAdded := True;
          if Br.Empty then
            Exit(Br.Offset);
        end;
      else
        Consume := False;
    end;
  if not OneAdded then
    Exit(-Br.Offset);
  if not Exponent and ((Br.PeekByte = Ord('e')) or (Br.PeekByte = Ord('E'))) then
  begin
    Nb.AddChar(WideChar(Br.ConsumeByte));
    if Br.Empty then
      Exit(-Br.Offset);
    if (Br.PeekByte = Ord('-')) or (Br.PeekByte = Ord('+')) then
    begin
      Nb.AddChar(WideChar(Br.ConsumeByte));
      if Br.Empty then
        Exit(-Br.Offset);
    end;
    OneAdded := False;
    Consume := True;
    while Consume do
      case Br.PeekByte of
        Ord('0'),
        Ord('1'),
        Ord('2'),
        Ord('3'),
        Ord('4'),
        Ord('5'),
        Ord('6'),
        Ord('7'),
        Ord('8'),
        Ord('9'):
          begin
            Nb.AddChar(WideChar(Br.ConsumeByte));
            OneAdded := True;
            if Br.Empty then
              Exit(Br.Offset);
          end;
        else
          Consume := False;
      end;
    if not OneAdded then
      Exit(-Br.Offset);
  end;
  Result := Br.Offset;
end;

class function TJSONObject.ParseString(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer;
var
  UnicodeCh: Integer;
  Ch: WideChar;
  Str: TJSONString;
begin
  if Br.PeekByte <> Ord('"') then
    Exit(-Br.Offset);
  Br.ConsumeByte;
  if Br.Empty then
    Exit(-Br.Offset);
  Str := TJSONString.Create('');
  Parent.AddDescendant(Str);
  while Br.PeekByte <> Ord('"') do
  begin
    case Br.PeekByte of
      Ord('\'):
        begin
          Br.ConsumeByte;
          if Br.Empty then
            Exit(-Br.Offset);
          case Br.PeekByte of
            Ord('"'):
              Ch := '"';
            Ord('\'):
              Ch := '\';
            Ord('/'):
              Ch := '/';
            Ord('b'):
              Ch := #$8;
            Ord('f'):
              Ch := #$c;
            Ord('n'):
              Ch := #$a;
            Ord('r'):
              Ch := #$d;
            Ord('t'):
              Ch := #$9;
            Ord('u'):
              begin
                Br.ConsumeByte;
                if not Br.HasMore(3) then
                  Exit(-Br.Offset);
                UnicodeCh := HexToDecimal(Br.ConsumeByte) shl 12;
                UnicodeCh := UnicodeCh or HexToDecimal(Br.ConsumeByte) shl 8;
                UnicodeCh := UnicodeCh or HexToDecimal(Br.ConsumeByte) shl 4;
                UnicodeCh := UnicodeCh or HexToDecimal(Br.PeekByte);
                Ch := WideChar(UnicodeCh);
              end;
            else
              Exit(-Br.Offset);
          end;
        end;
      else
        Ch := WideChar(Br.PeekByte);
    end;
    Str.AddChar(Ch);
    Br.ConsumeByte;
    if Br.Empty then
      Exit(-Br.Offset);
  end;
  Br.ConsumeByte;
  Result := Br.Offset;
end;

function TJSONObject.DelveIntoPath(const APath: string): TJSONValue;
var
  vPath: TStrings;
  i: Integer;
begin
  vPath := TStringList.Create;
  vPath.Delimiter := '/';
  vPath.DelimitedText := Trim(APath);
  Result := Self;
  try
    for i := 0 to vPath.Count - 1 do
    begin
      if Result is TJSONObject then
        Result := TJSONObject(Result).ExtractValue(Trim(vPath[i]))
      else
        Result := nil;

      if not Assigned(Result) then
        Break;
    end;
  finally
    vPath.Free;
  end;
end;

function TJSONObject.ToString: UnicodeString;
var
  Buf: TStringBuffer;
  Size: Integer;
  I: Integer;
begin
  Size := FMembers.Count;
  Buf := TStringBuffer.Create;
  try
    Buf.Append('{');
    if Size > 0 then
      Buf.Append(FMembers[0].ToString);
    for I := 1 to Size - 1 do
    begin
      Buf.Append(',');
      Buf.Append(FMembers[I].ToString);
    end;
    Buf.Append('}');
    Result := Buf.ToString;
  finally
    Buf.Free;
  end;
end;

function TJSONObject.ToUTF8String: UTF8String;
begin
  Result := UTF8Encode(ToRawString);
end;

procedure TJSONNull.AddDescendant(const Descendant: TJSONAncestor);
begin
end;

function TJSONNull.IsNull: Boolean;
begin
  Result := True;
end;

function TJSONNull.EstimatedByteSize: Integer;
begin
  Result := 4;
end;

function TJSONNull.ToBytes(const Data: TBytes; const Offset: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := Offset;
  Data[IncrAfter(Idx)] := Ord('n');
  Data[IncrAfter(Idx)] := Ord('u');
  Data[IncrAfter(Idx)] := Ord('l');
  Data[IncrAfter(Idx)] := Ord('l');
  Result := Idx;
end;

function TJSONNull.ToString: UnicodeString;
begin
  Result := 'null';
end;

function TJSONNull.Clone: TJSONAncestor;
begin
  Result := TJSONNull.Create;
end;

procedure TJSONFalse.AddDescendant(const Descendant: TJSONAncestor);
begin
end;

function TJSONFalse.EstimatedByteSize: Integer;
begin
  Result := 5;
end;

function TJSONFalse.ToBytes(const Data: TBytes; const Offset: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := Offset;
  Data[IncrAfter(Idx)] := Ord('f');
  Data[IncrAfter(Idx)] := Ord('a');
  Data[IncrAfter(Idx)] := Ord('l');
  Data[IncrAfter(Idx)] := Ord('s');
  Data[IncrAfter(Idx)] := Ord('e');
  Result := Idx;
end;

function TJSONFalse.ToString: UnicodeString;
begin
  Result := 'false';
end;

function TJSONFalse.Clone: TJSONAncestor;
begin
  Result := TJSONFalse.Create;
end;

constructor TJSONArray.Create;
begin
  inherited Create;
  FElements := TArrayList.Create;
end;

constructor TJSONArray.Create(const FirstElem: TJSONValue);
begin
  Create;
  AddElement(FirstElem);
end;

constructor TJSONArray.Create(const FirstElem: TJSONValue; const SecondElem: TJSONValue);
begin
  Create;
  AddElement(FirstElem);
  AddElement(SecondElem);
end;

constructor TJSONArray.Create(const FirstElem: String; const SecondElem: String);
begin
  Create;
  AddElement(TJSONString.Create(FirstElem));
  AddElement(TJSONString.Create(SecondElem));
end;

destructor TJSONArray.Destroy;
var
  Element: TJSONAncestor;
  I: Integer;
begin
  if FElements <> nil then
  begin
    for I := 0 to FElements.Count - 1 do
    begin
      Element := TJSONAncestor(FElements[I]);
      if Element.GetOwned then
        Element.Free;
    end;
    FreeAndNil(FElements);
  end;
  inherited Destroy;
end;

procedure TJSONArray.SetElements(AList: TArrayList);
begin
  FElements.Free;
  FElements := AList;
end;

function TJSONArray.Size: Integer;
begin
  if (FElements = nil) or (FElements.Count = 0) then
    Exit(0);
  Result := FElements.Count;
end;

function TJSONArray.Get(const Index: Integer): TJSONValue;
begin
  if (Index < 0) or (Index >= Size) then
    Exit(nil);
  Result := TJSONValue(FElements[Index]);
end;

procedure TJSONArray.AddDescendant(const Descendant: TJSONAncestor);
begin
  FElements.Add(Descendant);
end;

function TJSONArray.Pop: TJSONValue;
var
  Value: TJSONValue;
begin
  Value := TJSONValue(FElements[0]);
  FElements.RemoveAt(0);
  Result := Value;
end;

function TJSONArray.Remove(Index: Integer): TJSONValue;
begin
  Result := Get(Index);
  if (Index >= 0) and (Index < Size) then
    FElements.RemoveAt(Index);
end;

procedure TJSONArray.AddElement(const Element: TJSONValue);
begin
  if Element <> nil then
    AddDescendant(Element);
end;

function TJSONArray.Add(const Element: UnicodeString): TJSONArray;
begin
  AddElement(TJSONString.Create(Element));
  Result := self;
end;

function TJSONArray.Add(const Element: Integer): TJSONArray;
begin
  AddElement(TJSONNumber.Create(Element));
  Result := self;
end;

function TJSONArray.Add(const Element: Double): TJSONArray;
begin
  AddElement(TJSONNumber.Create(Element));
  Result := self;
end;

function TJSONArray.Add(const Element: Boolean): TJSONArray;
begin
  if Element then
    AddElement(TJSONTrue.Create)
  else
    AddElement(TJSONFalse.Create);
  Result := self;
end;

function TJSONArray.Add(const Element: TJSONObject): TJSONArray;
begin
  if Element <> nil then
    AddElement(Element)
  else
    AddElement(TJSONNull.Create);
  Result := self;
end;

function TJSONArray.Add(const Element: TJSONArray): TJSONArray;
begin
  AddElement(Element);
  Result := self;
end;

function TJSONArray.EstimatedByteSize: Integer;
var
  Size: Integer;
  I: Integer;
begin
  Size := 1;
  for I := 0 to FElements.Count - 1 do
    Size := Size + (TJSONAncestor(FElements[I])).EstimatedByteSize + 1;
  if Size = 1 then
    Exit(2);
  Result := Size;
end;

function TJSONArray.ToBytes(const Data: TBytes; const Pos: Integer): Integer;
var
  Offset: Integer;
  Size: Integer;
  I: Integer;
begin
  Offset := Pos;
  Size := FElements.Count;
  Data[IncrAfter(Offset)] := Ord('[');
  if Size > 0 then
    Offset := (TJSONAncestor(FElements[0])).ToBytes(Data, Offset);
  for I := 1 to Size - 1 do
  begin
    Data[IncrAfter(Offset)] := Ord(',');
    Offset := (TJSONAncestor(FElements[I])).ToBytes(Data, Offset);
  end;
  Data[IncrAfter(Offset)] := Ord(']');
  Result := Offset;
end;

function TJSONArray.ToRawString: string;
var
  Buf: TStringBuffer;
  Size: Integer;
  I: Integer;
begin
  Size := FElements.Count;
  Buf := TStringBuffer.Create;
  try
    Buf.Append('[');
    if Size > 0 then
      Buf.Append(FElements[0].ToRawString);
    for I := 1 to Size - 1 do
    begin
      Buf.Append(',');
      Buf.Append(FElements[I].ToRawString);
    end;
    Buf.Append(']');
    Result := Buf.ToString;
  finally
    Buf.Free;
  end;
end;

function TJSONArray.ToString: UnicodeString;
var
  Buf: TStringBuffer;
  Size: Integer;
  I: Integer;
begin
  Size := FElements.Count;
  Buf := TStringBuffer.Create;
  try
    Buf.Append('[');
    if Size > 0 then
      Buf.Append(FElements[0].ToString);
    for I := 1 to Size - 1 do
    begin
      Buf.Append(',');
      Buf.Append(FElements[I].ToString);
    end;
    Buf.Append(']');
    Result := Buf.ToString;
  finally
    Buf.Free;
  end;
end;

function TJSONArray.Clone: TJSONAncestor;
var
  Data: TJSONArray;
  I: Integer;
begin
  Data := TJSONArray.Create;
  for I := 0 to Size - 1 do
    Data.AddDescendant(Get(I).Clone);
  Result := Data;
end;

function TJSONArray.GetEnumerator: TJSONArrayObjectEnumerator;
begin
  Result := TJSONArrayObjectEnumerator.Create(Self);
end;

{ TJSONPairEnumerator }

constructor TJSONPairEnumerator.Create(ADBXArrayList: TArrayList);
begin
  inherited Create;
  FIndex := -1;
  FDBXArrayList := ADBXArrayList;
end;

function TJSONPairEnumerator.GetCurrent: TJSONPair;
begin
  Result := TJSONPair(FDBXArrayList[FIndex]);
end;

function TJSONPairEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FDBXArrayList.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TJSONArrayEnumerator }

constructor TJSONArrayEnumerator.Create(AArray: TJSONArray);
begin
  inherited Create;
  FIndex := -1;
  FArray := AArray;
end;

function TJSONArrayEnumerator.GetCurrent: TJSONValue;
begin
  Result := FArray.Get(FIndex);
end;

function TJSONArrayEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FArray.Size - 1;
  if Result then
    Inc(FIndex);
end;

{ TJSONArrayObjectEnumerator }

constructor TJSONArrayObjectEnumerator.Create(AArray: TJSONArray);
begin
  inherited Create;
  FIndex := -1;
  FArray := AArray;
end;

function TJSONArrayObjectEnumerator.GetCurrent: TJSONObject;
begin
  Result := TJSONObject(FArray.Get(FIndex));
end;

function TJSONArrayObjectEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FArray.Size - 1;
  if Result then
    Inc(FIndex);
end;

{$WARN SYMBOL_DEPRECATED ON}

end.

