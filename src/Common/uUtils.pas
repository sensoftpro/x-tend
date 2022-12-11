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

unit uUtils;

interface

uses
  Classes, Types, UITypes, XMLIntf;

type
  TWordCase = (wcRod, wcDat, wcVin);
  TXMLExtractionFunc = reference to function(const ANode: IXMLNode): Boolean;

function CheckINN(const INN: string): Boolean;
function CheckPFCertificate(const PF: string): Boolean;
function CheckOGRN(const OGRN: string): Boolean;

// Функции даты/времени
function FormatDate(const ADate: TDateTime): string;
function FormatDateAndTime(const ADate: TDateTime): string;
function FormatMoney(const AValue: Double): string;

function DefineNameCase(const ASurName, AFirstName, APatronymicName: string;
  const AWordCase: TWordCase = wcRod): string;
//function DeleteFileToRecycleBin(const FileName: string): Integer;

// Работа с Base64
function EncodeBase64(const AInput: string): string;
function DecodeBase64(const AInput: string): string;

function MoneyToStringEng(const AMoney: Currency; const ACurrName, ACentName: string): string;

type
  TCompLevel = (cmlNone, cmlFastest, cmlDefault, cmlMax);

function CompressStream(InStream, OutStream: TStream; const Level : TCompLevel = cmlDefault):boolean;
procedure ExpandStream(const AInStream, AOutStream: TStream);
function CryptStream(InStream, OutStream: TStream; const Password: string; isCrypt: boolean): Boolean;

function IsTextMultiEqual(const APattern, AText: string): Boolean;

function EscapeFileName(const AInput: string): string;

function BuildStorageName(const AName: string): string;
function SanitizeFileName(const AFileName: string): string;

function NewGUID: TGUID;
function GenerateUID: string;

function FindClass(const AClassName: string): TClass;
function FindAnyClass(const AClassName: string): TClass;

function CreateDelimitedList(const ADelimitedText: string; const ADelimiter: Char = ';'): TStrings;
function GetFileSize(const AFileName: string): Int64;
function GetFileCreateTime(const AFileName: string): TDateTime;
function MD5Hash(const s: string): string;
function MD5HashFromFile(const AFileName: string): string;
function UrlEncode(const s: string; const InQueryString: Boolean): string;

function Transliterate(const s: string): string;

function CalcInnerColor(const AStartColor, AEndColor: Cardinal; const APercentage: Double): Cardinal;
function ColorToAlphaColor(const AColor: TColor; const ATransparency: Double = 0): Cardinal;
function AlphaColorToColor(const AColor: Cardinal): TColor;
// -1 - к черному, +1 - к белому
function DimColor(const AColor: Cardinal; const APercent: Double = 0): Cardinal;

// XML utils
function FindNode(const AParentNode: IXMLNode; const ANodeName: string): IXMLNode;
function ExtractNode(const AParentNode: IXMLNode; const APath: string): IXMLNode;
function ExtractDataFromXML(const AFileName: string; const AXMLExtractionFunc: TXMLExtractionFunc): Boolean;

// RegExp
function ExtractByRE(const AText, ARegExpression: string): string;
function CutDataTillMarker(var s: string; const AMarker: string = ' '): string;
function GetNSymbols(const s: string; const ACount: Integer = 1): string;
function JoinIfFilled(const s: string; const APrefix, APostfix: string): string;
function TimeIntervalToStr(const ASeconds: Int64): string;

// RTTI
function MakeMethod(const ACode, AData: Pointer): TMethod;

function GetUrlParam(const AUrl, AParamName: string; const ADefaultValue: string = ''): string;
function GetUrlCommand(const AUrl: string; const ADefaultValue: string = ''): string;
function RemoveUrlParam(const AUrl, AParamName: string): string;
function ExtractUrlParams(const AUrl: string; const ADefaultValue: string = ''): string;

function EncodeUrl(const AUrl: string): string;
function DecodeUrl(const AUrl: string): string;

function IpToStr(const AIp): string;
procedure StrToIp(const AIpStr: string; const AIp);
function BinToHex(const ABin: array of Byte): string;
procedure HexToBin(const AHex: string; const ABuf);

function CeilTo(const AValue: Double; const ADigit: Integer = -2): Double;
function FloorTo(const AValue: Double; const ADigit: Integer = -2): Double;
function FormatTo(const AValue: Double; const ADigit: Integer = -2; const AUseStrictFormat: Boolean = False): string;
function FormatAsBitString(const AValue: Cardinal; const ASize: Byte = 32): string;

type
  TPrintProcedure = reference to procedure(const AText: string);
var
  PrintProc: TPrintProcedure = nil;
  PrintBuffer: TStrings = nil;

type
  TVersion = packed record
    class operator Implicit(Val: string): TVersion;
    class operator Explicit(Val: string): TVersion;
    class operator Equal(Val1, Val2: TVersion): Boolean;
    class operator Equal(Val1: TVersion; Val2: string): Boolean;
    class operator NotEqual(Val1, Val2: TVersion): Boolean;
    class operator NotEqual(Val1: TVersion; Val2: string): Boolean;
    class operator GreaterThan(Val1, Val2: TVersion): Boolean;
    class operator GreaterThan(Val1: TVersion; Val2: string): Boolean;
    class operator GreaterThanOrEqual(Val1, Val2: TVersion): Boolean;
    class operator GreaterThanOrEqual(Val1: TVersion; Val2: string): Boolean;
    class operator LessThan(Val1, Val2: TVersion): Boolean;
    class operator LessThan(Val1: TVersion; Val2: string): Boolean;
    class operator LessThanOrEqual(Val1, Val2: TVersion): Boolean;
    class operator LessThanOrEqual(Val1: TVersion; Val2: string): Boolean;
  private
    FMajor: Byte;
    FMinor: Byte;
    FRelease: Word;
    class function Compare(Val1, Val2: TVersion): Integer; static;
    class function CompareWithStr(Val1: TVersion; Val2: string): Integer; static;
    procedure StrToVersion(const s: string);
    function VersionToStr: string;
  public
    constructor Create(const s: string);
    function ToString: string; overload;
    function IsValid: Boolean;
  end;

procedure Print(const AText: string; const AParams: array of const);

implementation

uses
  Math, Variants, SysUtils, RTTI, RegularExpressions, ZLib, NetEncoding, Hash, XMLDoc, StrUtils, IdURI, Windows, IOUtils;

function MakeMethod(const ACode, AData: Pointer): TMethod;
begin
  Result.Code := ACode;
  Result.Data := AData;
end;

function GetNSymbols(const s: string; const ACount: Integer = 1): string;
begin
  if ACount < 1 then
    Exit('');
  if Length(s) <= ACount then
    Result := s
  else
    Result := Copy(s, 1, ACount);
end;

function JoinIfFilled(const s: string; const APrefix, APostfix: string): string;
begin
  Result := IfThen(s <> '', APrefix + s + APostfix, '');
end;

function CutDataTillMarker(var s: string; const AMarker: string = ' '): string;
var
  vPos: Integer;
begin
  vPos := Pos(AMarker, s);
  if vPos > 0 then
  begin
    Result := Copy(s, 1, vPos - 1);
    Delete(s, 1, vPos + Length(AMarker) - 1);
    s := Trim(s);
  end
  else begin
    Result := s;
    s := '';
  end;
end;

function TimeIntervalToStr(const ASeconds: Int64): string;
var
  vSeconds: Int64;
  function CheckPeriod(var ARemainSeconds: Int64; const ADivider: Integer): string;
  var
    vPeriod: Int64;
  begin
    vPeriod := ARemainSeconds div ADivider;
    if vPeriod > 0 then
      Result := IntToStr(vPeriod)
    else
      Result := '';
    ARemainSeconds := ARemainSeconds mod ADivider;
  end;
begin
  vSeconds := ASeconds;
  Result := Result + JoinIfFilled(CheckPeriod(vSeconds, 86400), ' ', 'дн');
  Result := Result + JoinIfFilled(CheckPeriod(vSeconds, 3600), ' ', 'ч');
  Result := Result + JoinIfFilled(CheckPeriod(vSeconds, 60), ' ', 'м');
  Result := Trim(Result + JoinIfFilled(IntToStr(vSeconds), ' ', 'с'));
end;

function ExtractByRE(const AText, ARegExpression: string): string;
var
  vMatch: TMatch;
begin
  vMatch := TRegEx.Match(AText, ARegExpression, [roIgnoreCase]);
  if vMatch.Success and (vMatch.Groups.Count > 1) then
    Result := vMatch.Groups[1].Value
  else
    Result := '';
end;

function FindNode(const AParentNode: IXMLNode; const ANodeName: string): IXMLNode;
var
  i: Integer;
begin
  for i := 0 to AParentNode.ChildNodes.Count - 1 do
  begin
    Result := AParentNode.ChildNodes[i];
    if SameText(Result.NodeName, ANodeName) then
      Exit;
  end;
  Result := nil;
end;

// Пример APath  av:Лист_Отчетность/av:Раздел1РеквизитыПИФ/av:РегистрНомерПИФ
function ExtractNode(const AParentNode: IXMLNode; const APath: string): IXMLNode;
var
  vPath: string;
  vName: string;
  vPos: Integer;
begin
  Result := AParentNode;
  vPath := APath;

  repeat
    vPos := Pos('/', vPath);
    if vPos > 0 then
    begin
      vName := Copy(vPath, 1, vPos - 1);
      Delete(vPath, 1, vPos);
    end
    else
      vName := vPath;

    Result := FindNode(Result, vName);
  until (vPos = 0) or not Assigned(Result);
end;

function ExtractDataFromXML(const AFileName: string; const AXMLExtractionFunc: TXMLExtractionFunc): Boolean;
var
  vXML: IXMLDocument;
begin
  if not FileExists(AFileName) then
    Exit(False);

  try
    vXML := TXMLDocument.Create(AFileName);
    vXML.Active := True;
    try
      try
        if Assigned(AXMLExtractionFunc) then
          Result := AXMLExtractionFunc(vXML.DocumentElement)
        else
          Result := True;
      except
        Result := False;
      end;
    finally
      vXML.Active := False;
    end;
  except
    Result := False;
  end;
end;

function CalcInnerColor(const AStartColor, AEndColor: Cardinal; const APercentage: Double): Cardinal;
var
  vStartColor, vEndColor, vResColor: TAlphaColorRec;
begin
  if AStartColor = AEndColor then
    Exit(AStartColor);
  if APercentage <= 0 then
    Exit(AStartColor);
  if APercentage >= 1 then
    Exit(AEndColor);

  vStartColor := TAlphaColorRec.Create(AStartColor);
  vEndColor := TAlphaColorRec.Create(AEndColor);
  vResColor.A := vStartColor.A + Round((Integer(vEndColor.A) - vStartColor.A) * APercentage);
  vResColor.R := vStartColor.R + Round((Integer(vEndColor.R) - vStartColor.R) * APercentage);
  vResColor.G := vStartColor.G + Round((Integer(vEndColor.G) - vStartColor.G) * APercentage);
  vResColor.B := vStartColor.B + Round((Integer(vEndColor.B) - vStartColor.B) * APercentage);
  Result := vResColor.Color;
end;

function ColorToAlphaColor(const AColor: TColor; const ATransparency: Double): Cardinal;
var
  vColor: Integer;
  vResColor: TAlphaColorRec;
begin
  vColor := TColorRec.ColorToRGB(AColor);
  vResColor.A := Round(255 * (1 - ATransparency));
  vResColor.B := vColor shr 16 and $FF;
  vResColor.G := vColor shr 8 and $FF;
  vResColor.R := vColor and $FF;
  Result := vResColor.Color;
end;

function AlphaColorToColor(const AColor: Cardinal): TColor;
begin
  Result := (AColor and $FF shl 16) or (AColor and $FF00) or (AColor shr 16 and $FF);
end;

function DimColor(const AColor: Cardinal; const APercent: Double = 0): Cardinal;
var
  vResColor: TAlphaColorRec;
begin
  vResColor.Color := AColor;
  if APercent > 0 then
  begin
    vResColor.B := Round(Min(255, vResColor.B + (255 - vResColor.B) * APercent));
    vResColor.G := Round(Min(255, vResColor.G + (255 - vResColor.G) * APercent));
    vResColor.R := Round(Min(255, vResColor.R + (255 - vResColor.R) * APercent));
  end
  else begin
    vResColor.B := Round(Max(0, vResColor.B + vResColor.B * APercent));
    vResColor.G := Round(Max(0, vResColor.G + vResColor.G * APercent));
    vResColor.R := Round(Max(0, vResColor.R + vResColor.R * APercent));
  end;
  Result := vResColor.Color;
end;

function UrlEncode(const S: string; const InQueryString: Boolean): string;
var
  vSymbol: Char;
begin
  Result := EmptyStr;
  for vSymbol in S do
    case vSymbol of
    // The NoConversion set contains characters as specificed in RFC 1738 and
    // should not be modified unless the standard changes.
    'A'..'Z', 'a'..'z', '*', '@', '.', '_', '-', '0'..'9',
    '$', '!', '''', '(', ')':
       Result := Result + vSymbol;
    '—': Result := Result + '%E2%80%94';
    ' ':
      if InQueryString then
        Result := Result + '+'
      else
        Result := Result + '%20';
   else
     Result := Result + '%' + IntToHex(Ord(vSymbol), 2);
   end;
end;

function MD5Hash(const s: string): string;
begin
  Result := THashMD5.GetHashString(s);
end;

function MD5HashFromFile(const AFileName: string): string;
begin
  Result := THashMD5.GetHashStringFromFile(AFileName);
end;

function Transliterate(const s: string): string;
var
  vChar: Char;
  vPos: Integer;
const
  cRussianLetters = 'АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдеёжзийклмнопрстуфхцчшщъыьэюя';
  cLatinLetters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_ ';
  cReplacements: array[1..66] of string = (
    'A', 'B', 'V', 'G', 'D', 'E', 'YO', 'ZH', 'Z', 'I', 'Y',
    'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U', 'F',
    'H', 'TS', 'CH', 'SH', 'SCH', '''', 'Y', '''', 'E', 'YU', 'YA',
    'a', 'b', 'v', 'g', 'd', 'e', 'yo', 'zh', 'z', 'i', 'y',
    'k', 'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'f',
    'h', 'ts', 'ch', 'sh', 'sch', '''', 'y', '''', 'e', 'yu', 'ya');
begin
  Result := '';
  for vChar in s do
  begin
    // строчные буквы
    vPos := Pos(vChar, cRussianLetters);
    if vPos > 0 then
      Result := Result + cReplacements[vPos]
    else if Pos(vChar, cRussianLetters) > 0 then
      Result := Result + vChar;
  end;
end;

function GetFileSize(const AFileName: string): Int64;
var
  vSearchRec: TSearchRec;
begin
  if FindFirst(ExpandFileName(AFileName), faAnyFile, vSearchRec) = 0 then
    Result := vSearchRec.Size
  else
    Result := -1;
  SysUtils.FindClose(vSearchRec);
end;

function GetFileCreateTime(const AFileName: string): TDateTime;
begin
  Result := TFile.GetLastWriteTime(AFileName);
end;

function CreateDelimitedList(const ADelimitedText: string; const ADelimiter: Char = ';'): TStrings;
begin
  Result := TStringList.Create;
  Result.StrictDelimiter := True; // need before DelimitedText := ADelimitedText
  Result.Delimiter := ADelimiter;
  Result.DelimitedText := ADelimitedText;
  Result.QuoteChar := #0;
end;

function FindClass(const AClassName: string): TClass;
var
  vRttiContext: TRttiContext;
  vRttiType: TRttiType;
begin
  vRttiContext := TRttiContext.Create;
  try
    vRttiType := vRttiContext.FindType(AClassName);
    if Assigned(vRttiType) and vRttiType.IsInstance then
      Result := vRttiType.AsInstance.MetaClassType
    else
      Result := nil;
  finally
    vRttiContext.Free;
  end;
end;

function FindAnyClass(const AClassName: string): TClass;
var
  vRttiContext: TRttiContext;
  vRttiTypes: TArray<TRttiType>;
  vRttiType: TRttiType;
begin
  Result := nil;

  vRttiContext := TRttiContext.Create;
  vRttiTypes := vRttiContext.GetTypes;
  for vRttiType in vRttiTypes do
  begin
    if vRttiType.IsInstance and (SameText(AClassName, vRttiType.Name)) then
    begin
      Result := vRttiType.AsInstance.MetaClassType;
      Break;
    end;
  end;
  vRttiContext.Free;
end;

function NewGUID: TGUID;
var
  vResult: HResult;
begin
  vResult := CreateGuid(Result);
  if vResult <> S_OK then
    Result := TGUID.Empty;
end;

function GenerateUID: string;
var
  vUID: TGUID;
  vResult: HResult;
begin
  vResult := CreateGuid(vUID);
  if vResult = S_OK then
    Result := Copy(StringReplace(LowerCase(GuidToString(vUID)), '-', '', [rfReplaceAll]), 2, 32)
  else
    Result := '';
end;

function BuildStorageName(const AName: string): string;
var
  vBuffer: string;
  vChar: Char;
const
  cUpperLetters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  cDigits = '0123456789';
begin
  Result := '';
  vBuffer := '';
  for vChar in AName do
  begin
    // строчные буквы
    if Pos(vChar, cUpperLetters) = 0 then
    begin
      if Length(vBuffer) > 1 then
      begin
        if (Result <> '') and (Result[Length(Result)] <> '_') then
          Result := Result + '_';
        if Pos(vChar, cDigits) = 0 then
        begin
          Result := Result + Copy(vBuffer, 1, Length(vBuffer) - 1);
          Delete(vBuffer, 1, Length(vBuffer) - 1);
        end;
      end;

      if Length(vBuffer) > 0 then
      begin
        if (Result <> '') and (Result[Length(Result)] <> '_') then
          Result := Result + '_';
        Result := Result + vBuffer;
        vBuffer := '';
      end;

      Result := Result + vChar;
    end
    else
      vBuffer := vBuffer + LowerCase(vChar);
  end;

  if vBuffer = '' then
    Exit;

  if (Result <> '') and (Result[Length(Result)] <> '_') then
    Result := Result + '_';
  Result := Result + vBuffer;
end;

function SanitizeFileName(const AFileName: string): string;
begin
  Result := AFileName;
end;

function EscapeFileName(const AInput: string): string;
var
  i: Integer;
  vChar: Char;
  vPrevChar: Char;
const
  cAllowedChars =
    'abcdefghijklmnopqrstuvwxyz' +
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
    'абвгдеёжзийклмнопрстуфхцчшщъыьэюя' +
    'АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ' +
    '0123456789_-~(),.;';
begin
  Result := '';
  vPrevChar := #0;
  for i := 0 to Length(AInput) - 1 do
  begin
    vChar := AInput.Chars[i];
    if Pos(vChar, cAllowedChars) = 0 then
      vChar := '_';
    if (vPrevChar <> '_') or (vChar <> '_') then
    begin
      Result := Result + vChar;
      vPrevChar := vChar;
    end;
  end;

  Result := Copy(Result, 1, 100);
end;

function FormatDate(const ADate: TDateTime): string;
begin
  Result := FormatDateTime('dd.mm.yyyy', ADate);
end;

function FormatDateAndTime(const ADate: TDateTime): string;
begin
  Result := FormatDateTime('dd.mm.yyyy  h:nn:ss', ADate);
end;

function FormatMoney(const AValue: Double): string;
begin
  Result := FormatFloat('0.00', AValue);
end;

// ==================================================================================================
// Функция вычисляет контрольное число ИНН и возвращает True если ИНН
// введен правильно или False в противном случае
// В качестве параметра передается проверяемый ИНН
// Для справки: структура ИНН
//              10-ти разрядный ИНН - NNNNXXXXXC
//              12-ти разрядный ИНН - NNNNXXXXXXCC
//              где: NNNN - номер налоговой инспекции
//                   XXXXX, XXXXXX - порядковый номер налогоплательщика (номер записи в госреестре)
//                   C - контрольное число в 10-ти разрядном ИНН
//                   CC - контрольное число в 12-ти разрядном ИНН
//                        (фактически, идущие подряд две контрольные цифры)
// ==================================================================================================
function CheckINN(const INN: string): Boolean;
const
  factor1: array[0..8]  of byte = (2, 4, 10, 3, 5, 9, 4, 6, 8);
  factor2: array[0..9]  of byte = (7, 2, 4, 10, 3, 5, 9, 4, 6, 8);
  factor3: array[0..10] of byte = (3, 7, 2, 4, 10, 3, 5, 9, 4, 6, 8);
var
 i: byte;
 Sum: word;
 Sum2: word;
begin
  if Length(INN) = 0 then
  begin
    Result := True;
    Exit;
  end;

  Result := False;

  try
    if Length(INN) = 10 then begin
      Sum := 0;
      for i:=0 to 8 do
        Sum := Sum + StrToInt(INN[i+1])*factor1[i];
      Sum := Sum mod 11;
      Sum := Sum mod 10;
      Result := StrToInt(INN[10]) = Sum;
    end
    else if Length(INN) = 12 then begin
      Sum := 0;
      for i:=0 to 9 do
        Sum := Sum + StrToInt(INN[i+1])*factor2[i];
      Sum := Sum mod 11;
      Sum := Sum mod 10;
      Sum2 := 0;
      for i:=0 to 10 do
        Sum2 := Sum2 + StrToInt(INN[i+1])*factor3[i];
      Sum2 := Sum2 mod 11;
      Sum2 := Sum2 mod 10;
      Result := (StrToInt(INN[11]) = Sum) and
                (StrToInt(INN[12]) = Sum2);
    end; //
  except
    Result := False;
  end; // try
end;

// ===========================================
// Функция вычисляет контрольное число страхового номера ПФ и возвращает True если
// оно введено правильно или False в противном случае
// В качестве параметра передается страховой номер ПФ без разделителй
//
// Проверка контрольного числа Страхового номера проводится только для
// номеров больше  номера 001-001-998.
// Контрольное число Страхового номера рассчитывается следующим образом:
// каждая цифра Страхового номера умножается на номер своей позиции (позиции
// отсчитываются с конца), полученные произведения суммируются, сумма делится
// на 101, последние две цифры остатка от деления являются Контрольным числом.
// ===========================================
function CheckPFCertificate(const PF: string): Boolean;
var
  Sum: Word;
  i: Byte;
begin
  if Length(PF) = 0 then
  begin
    Result := True;
    Exit;
  end;

  Sum := 0;

  if Length(PF) <> 11 then
    Result := False
  else
    try
      for i:=1 to 9 do
        Sum := Sum + StrToInt(PF[i])*(9-i+1);
      Sum := Sum mod 101;
      Result := StrToInt(Copy(PF, 10, 2)) = Sum;
    except
      Result := False;
    end; // try
end;

function CheckOGRN(const OGRN: string): Boolean;
begin
  if Length(OGRN) = 0 then
  begin
    Result := True;
    Exit;
  end;

  if Length(OGRN) <> 13 then
    Result := False
  else
    try
      Result := ((StrToInt(Copy(OGRN, 1, 9)) mod 11) * 1000 +
        StrToInt(Copy(OGRN, 10, 3))) mod 11 mod 10
        = StrToInt(Copy(OGRN, 13, 1));
    except
      Result := False;
    end;
end;

function DefineNameCase(const ASurName, AFirstName, APatronymicName: string;
  const AWordCase: TWordCase = wcRod): string;
var
  vRootStr, vEndStr: String;
  vLength: Integer;
  vGender: Integer;
  vEndChar: Char;
begin
  Result := '';
  vGender := 0;

  // Отчество
  vRootStr := APatronymicName;
  vLength := Length(vRootStr);
  if vLength > 1 then
  begin
    vEndStr := Copy(vRootStr, vLength - 1, 2);
    Delete(vRootStr, vLength - 1, 2);
    if vEndStr = 'ич' then
    begin
      vGender := 1;

      case AWordCase of
        wcRod: Result := vRootStr + 'ича';
        wcDat: Result := vRootStr + 'ичу';
        wcVin: Result := vRootStr + 'ича';
      else
        Result := APatronymicName;
      end;
    end
    else if vEndStr = 'на' then
    begin
      vGender := 2;

      case AWordCase of
        wcRod: Result := vRootStr + 'ны';
        wcDat: Result := vRootStr + 'не';
        wcVin: Result := vRootStr + 'ну';
      else
        Result := APatronymicName;
      end;
    end
    else
      Result := APatronymicName;
  end
  else
    Result := APatronymicName;

  // Имя
  vRootStr := AFirstName;
  vLength := Length(vRootStr);
  if vLength > 0 then
  begin
    if AFirstName = 'Павел' then
    begin
      case AWordCase of
        wcRod: Result := 'Павла ' + Result;
        wcDat: Result := 'Павлу ' + Result;
        wcVin: Result := 'Павла ' + Result;
      else
        Result := AFirstName + ' ' + Result;
      end;
    end
    else begin
      vEndChar := vRootStr.Chars[vLength - 1];
      Delete(vRootStr, vLength, 1);
      case vEndChar of
        'б', 'в', 'г', 'д', 'ж', 'з', 'к', 'л', 'м', 'н', 'п', 'р', 'с',
        'т', 'ф', 'х', 'ц', 'ч', 'ш', 'щ': begin
          if vGender = 2 then
            Result := AFirstName + ' ' + Result
          else begin
            if vGender = 0 then
              vGender := 1;
            case AWordCase of
              wcRod: Result := AFirstName + 'а ' + Result;
              wcDat: Result := AFirstName + 'у ' + Result;
              wcVin: Result := AFirstName + 'а ' + Result;
            else
              Result := AFirstName + ' ' + Result;
            end;
          end;
        end;
        'а': begin
          if vGender = 0 then
            vGender := 2;
          case AWordCase of
            wcRod: Result := vRootStr + 'ы ' + Result;
            wcDat: Result := vRootStr + 'е ' + Result;
            wcVin: Result := vRootStr + 'у ' + Result;
          else
            Result := AFirstName + ' ' + Result;
          end;
        end;
        'я': begin
          if vGender = 0 then
            vGender := 2;
          case AWordCase of
            wcRod: Result := vRootStr + 'и ' + Result;
            wcDat:
              if AFirstName.Chars[vLength - 2] = 'и' then
                Result := vRootStr + 'и ' + Result
              else
                Result := vRootStr + 'е ' + Result;
            wcVin: Result := vRootStr + 'ю ' + Result;
          else
            Result := AFirstName + ' ' + Result;
          end;
        end;
        'ь': begin
          if vGender = 1 then
          begin
            case AWordCase of
              wcRod: Result := vRootStr + 'я ' + Result;
              wcDat: Result := vRootStr + 'ю ' + Result;
              wcVin: Result := vRootStr + 'я ' + Result;
            else
              Result := AFirstName + ' ' + Result;
            end;
          end
          else
            Result := AFirstName + ' ' + Result;
        end;
        'й': begin
          if vGender = 2 then
            Result := AFirstName + ' ' + Result
          else begin
            if vGender = 0 then
              vGender := 1;
            case AWordCase of
              wcRod: Result := vRootStr + 'я ' + Result;
              wcDat: Result := vRootStr + 'ю ' + Result;
              wcVin: Result := vRootStr + 'я ' + Result;
            else
              Result := AFirstName + ' ' + Result;
            end;
          end;
        end
      else
        Result := AFirstName + ' ' + Result;
      end; // case
    end
  end
  else
    Result := AFirstName + ' ' + Result;

  // Фамилия
  vRootStr := ASurName;
  vLength := Length(vRootStr);
  if vLength > 1 then
  begin
    vEndStr := Copy(vRootStr, vLength - 1, 2);
    Delete(vRootStr, vLength - 1, 2);
    if vEndStr = 'ва' then
    begin
      if vGender = 1 then
      begin
        case AWordCase of
          wcRod: Result := vRootStr + 'вы ' + Result;
          wcDat: Result := vRootStr + 'ве ' + Result;
          wcVin: Result := vRootStr + 'ву ' + Result;
        else
          Result := ASurName + ' ' + Result;
        end;
      end
      else begin
        case AWordCase of
          wcRod: Result := vRootStr + 'вой ' + Result;
          wcDat: Result := vRootStr + 'вой ' + Result;
          wcVin: Result := vRootStr + 'ву ' + Result;
        else
          Result := ASurName + ' ' + Result;
        end;
      end;
    end
    else if vEndStr = 'на' then
    begin
      if vGender = 1 then
      begin
        case AWordCase of
          wcRod: Result := vRootStr + 'ны ' + Result;
          wcDat: Result := vRootStr + 'не ' + Result;
          wcVin: Result := vRootStr + 'ну ' + Result;
        else
          Result := ASurName + ' ' + Result;
        end;
      end
      else begin
        case AWordCase of
          wcRod: Result := vRootStr + 'ной ' + Result;
          wcDat: Result := vRootStr + 'ной ' + Result;
          wcVin: Result := vRootStr + 'ну ' + Result;
        else
          Result := ASurName + ' ' + Result;
        end;
      end;
    end
    else if vEndStr = 'ая' then
    begin
      case AWordCase of
        wcRod: Result := vRootStr + 'ой ' + Result;
        wcDat: Result := vRootStr + 'ой ' + Result;
        wcVin: Result := vRootStr + 'ую ' + Result;
      else
        Result := ASurName + ' ' + Result;
      end;
    end
    else if (vEndStr = 'ых') or (vEndStr = 'их') then
      Result := Trim(ASurName + ' ' + Result)
    else if (vEndStr = 'ой') or (vEndStr = 'ый') or (vEndStr = 'ий') then
    begin
      if vGender = 2 then
        Result := Trim(ASurName + ' ' + Result)
      else begin
        case AWordCase of
          wcRod: Result := vRootStr + 'ого ' + Result;
          wcDat: Result := vRootStr + 'ому ' + Result;
          wcVin: Result := vRootStr + 'ого ' + Result;
        else
          Result := ASurName + ' ' + Result;
        end;
      end;
    end
    else if Pos(ASurName[vLength], 'бвгджзклмнпрстфхцчшщ') > 0 then
    begin
      if vGender = 1 then
      begin
        case AWordCase of
          wcRod: Result := ASurName + 'а ' + Result;
          wcDat: Result := ASurName + 'у ' + Result;
          wcVin: Result := ASurName + 'а ' + Result;
        else
          Result := ASurName + ' ' + Result;
        end;
      end
      else
        Result := Trim(ASurName + ' ' + Result);
    end
    else if ASurName.Chars[vLength - 1] = 'а' then
    begin
      case AWordCase of
        wcRod: Result := vRootStr + vEndStr.Chars[0] + 'ы ' + Result;
        wcDat: Result := vRootStr + vEndStr.Chars[0] + 'е ' + Result;
        wcVin: Result := vRootStr + vEndStr.Chars[0] + 'у ' + Result;
      else
        Result := ASurName + ' ' + Result;
      end;
    end
    else if ASurName.Chars[vLength - 1] = 'я' then
    begin
      case AWordCase of
        wcRod: Result := vRootStr + vEndStr.Chars[0] + 'и ' + Result;
        wcDat: Result := vRootStr + vEndStr.Chars[0] + 'е ' + Result;
        wcVin: Result := vRootStr + vEndStr.Chars[0] + 'ю ' + Result;
      else
        Result := ASurName + ' ' + Result;
      end;
    end
    else
      Result := Trim(ASurName + ' ' + Result);
  end
  else
    Result := Trim(ASurName + ' ' + Result);
end;

{function DeleteFileToRecycleBin(const FileName: string): Integer;
var
  SHF: TSHFileOpStruct;
begin
  with SHF do
  begin
    Wnd := 0;
    wFunc := FO_DELETE;
    pFrom := PChar(FileName + #0);
    pTo := nil;
    fFlags := FOF_SILENT or FOF_NOCONFIRMATION or FOF_ALLOWUNDO;
    fAnyOperationsAborted := False;
    hNameMappings := nil;
    lpszProgressTitle := nil;
  end;
  Result := SHFileOperation(SHF);
end; }

// Base64 encoding
function EncodeBase64(const AInput: string): string;
var
  vEncoder: TBase64Encoding;
begin
  vEncoder := TBase64Encoding.Create(0);
  try
    Result := vEncoder.Encode(AInput);
  finally
    vEncoder.Free;
  end;
end;

// Base64 decoding
function DecodeBase64(const AInput: string): string;
var
  vEncoder: TBase64Encoding;
begin
  vEncoder := TBase64Encoding.Create(0);
  try
    Result := vEncoder.Decode(AInput);
  finally
    vEncoder.Free;
  end;
end;

function MoneyToStringEng(const AMoney: Currency; const ACurrName, ACentName: string): string;
const
  M_Ed: array [1..9] of string =
    ('one ','two ','three ','four ','five ','six ','seven ','eight ','nine ');
  W_Ed: array [1..9] of string =
    ('one ','two ','three ','four ','five ','six ','seven ','eight ','nine ');
  E_Ds: array [0..9] of string =
    ('ten ','eleven ','twelve ','thirteen ','fourteen ',
     'fifteen ','sixteen ','seventeen ','eighteen ','nineteen ');
  D_Ds: array [2..9] of string =
    ('twenty ','thirty ','forty ','fifty ','sixty ','seventy ',
     'eighty ','ninety ');
  U_Hd: array [1..9] of string =
    ('one hundred ','two hundred ','three hundred ','four hundred ','five hundred ',
    'six hundred ','seven hundred ','eight hundred ','nine hundred ');
  M_Tr: array[1..6,1..3] of string =
    (('thousand ','thousand ','thousand '),
     ('million ','million ','million '),
     ('billion ','billion ','billion '),
     ('trillion ','trillion ','trillion '),
     ('quadrillion ','quadrillion ','quadrillion '),
     ('quintillion ','quintillion ','quintillion '));
var
  Breaking: Int64;
  TriadaArr: array[0..6] of Integer;
  Ones, Decimals, Hundreds, Cnt, i: Integer;
  BufRes: string;
  DelimStr: string;
  SetStr: string;
  Flag: Byte;
  vCents: Integer;
begin
  vCents := Round((AMoney - Trunc(AMoney)) * 100);

  Result := '';
  DelimStr := '';
  Breaking := Trunc(AMoney);
  Cnt := 0;
  while Breaking >= 1000 do begin
    TriadaArr[Cnt] := Breaking mod 1000;
    Breaking := Breaking div 1000;
    Cnt := Cnt + 1;
    end;
  if (Cnt = 0) and (AMoney = 0)
    then begin
      Result := 'empty sum';
      Exit;
      end
    else TriadaArr[Cnt] := Breaking;

  for i := 0 to Cnt do begin
    BufRes := '';
    Flag := 3;
    Ones := TriadaArr[i] mod 10;
    Decimals := (TriadaArr[i] div 10) mod 10;
    Hundreds := TriadaArr[i] div 100;
    if Hundreds <> 0 then BufRes := U_Hd[Hundreds];
    if (Decimals <> 0) and (Decimals <> 1)
      then begin
        BufRes := BufRes + D_Ds[Decimals];
        if Ones <> 0 then begin
          if i = 1
            then BufRes := BufRes + W_Ed[Ones]
            else BufRes := BufRes + M_Ed[Ones];
          if Ones = 1
            then Flag := 1
            else if Ones < 5
              then Flag := 2;
          end;
        end
      else if Decimals = 1
        then BufRes := BufRes + E_Ds[Ones]
        else if Ones <> 0
          then begin
            if i = 1
              then BufRes := BufRes + W_Ed[Ones]
              else BufRes := BufRes + M_Ed[Ones];
            if Ones = 1
              then Flag := 1
              else if Ones < 5
                then Flag := 2;
            end;
    if (i > 0) and (TriadaArr[i] <> 0)
      then BufRes := BufRes + M_Tr[i, Flag];
    Result := BufRes + Result;
    SetStr := IntToStr(TriadaArr[i]);
    if Cnt = i
      then DelimStr := SetStr + DelimStr
      else begin
        while Length(SetStr) < 3
          do SetStr := '0' + SetStr;
        DelimStr := ' ' + SetStr + DelimStr;
        end;
    end;
  Result := Trim(Result) + ' ' + ACurrName + ' ' + FormatFloat('00', vCents) + ' ' + ACentName;
end;

function CryptStream(InStream, OutStream: TStream; const Password:string; isCrypt:boolean):Boolean;
var Index:LongInt;
    PassLength:Byte;
    PassIndex:Byte;
    Data:Byte;
begin
  InStream.Position := 0;
  OutStream.Position := 0;
  index:=0;
  passindex:=1;
  PassLength:=Length(Password);
  if (PassLength=0)then
    begin
      OutStream.CopyFrom(InStream,InStream.Size);
      OutStream.Position := 0;
      Result:=false;
      Exit;
    end;
  if not isCrypt then
    begin
      OutStream.CopyFrom(InStream,InStream.Size);
      OutStream.Position := 0;
      Result:=true;
      Exit;
    end;
  While index<>instream.Size do
    begin
      InStream.Seek(Index,soFromBeginning);
      InStream.Read(Data,1);
      Data:=Data xor (Byte(Password[Passindex])xor
                     (255-(Passlength-Passindex)))shl
                     ((InStream.Size-Index) mod 255);
      Inc(PassIndex);
      if PassIndex>PassLength then PassIndex:=1;
      OutStream.Write(Data,1);
      Inc(Index);
    end;
  Result:=True;
  OutStream.Position := 0;
end;

function CompressStream(InStream, OutStream: TStream; const Level: TCompLevel = cmlDefault):boolean;
begin
  InStream.Position := 0;
  OutStream.Position := 0;
  with TCompressionStream.Create(TCompressionLevel(Level), OutStream) do
    try
      CopyFrom(InStream, InStream.Size);
      Free;
      result := True;
    except
      result := False;
    end;
end;

procedure ExpandStream(const AInStream, AOutStream: TStream);
var
  vCount: integer;
  vZStream: TDecompressionStream;
  vBuffer: Byte;
begin
  AInStream.Position := 0;
  AOutStream.Position := 0;
  vZStream := TDecompressionStream.Create(AInStream);
  try
    while True do
    begin
      vCount := vZStream.Read(vBuffer, SizeOf(vBuffer));
      if vCount <> 0 then
        AOutStream.WriteBuffer(vBuffer, vCount)
      else
        Break;
    end;
  finally
    vZStream.Free;
  end;
end;

function IsTextMultiEqual(const APattern, AText: string): Boolean;
var
  vParts: TStringList;
  c: Integer;
  vPattern: string;
begin
  Result := True;
  vPattern := Trim(APattern);
  if Length(vPattern) = 0 then Exit;

  vParts := TStringList.Create;
  try
    vParts.Delimiter := ' ';
    vParts.DelimitedText := APattern;
    for c := 0 to vParts.Count - 1 do
      if Pos(vParts[c], AText) = 0 then
      begin
        Result := False;
        Break;
      end;
  finally
    vParts.Free;
  end;
end;

function GetUrlParam(const AUrl, AParamName: string; const ADefaultValue: string = ''): string;
var
  vFrom, vTo: Integer;
  vSearchUrl, vSrcUrl, vParamName: string;
begin
  if Length(AUrl) = 0 then
  begin
    Result := ADefaultValue;
    Exit;
  end;

  vSrcUrl := AUrl;
  if Pos('?', vSrcUrl) = 0 then
    vSrcUrl := '?' + vSrcUrl;
  vSearchUrl := AnsiUpperCase(vSrcUrl);

  vParamName := '&' + AnsiUpperCase(AParamName) + '=';

  vFrom := Pos(vParamName, vSearchUrl);

  if vFrom = 0 then
  begin
    vParamName := '?' + AnsiUpperCase(AParamName) + '=';
    vFrom := Pos(vParamName, vSearchUrl);
  end;

  if vFrom = 0 then
  begin
    Result := ADefaultValue;
    Exit;
  end;

	vFrom := vFrom + Length(vParamName);
	vTo := PosEx('&', vSearchUrl, vFrom);
  if vTo = 0 then
    vTo := PosEx('?', vSearchUrl, vFrom);

	if vTo = 0 then
		vTo := Length(vSearchUrl) + 1;

	Result := Copy(vSrcUrl, vFrom, vTo - vFrom);
end;

function RemoveUrlParam(const AUrl, AParamName: string): string;
var
  vFrom, vTo: Integer;
  vSearchUrl, vSrcUrl, vParamName: string;
begin
  Result := AUrl;
  if Length(Result) = 0 then Exit;

  vSrcUrl := AUrl;
  if Pos('?', vSrcUrl) = 0 then
    vSrcUrl := '?' + vSrcUrl;
  vSearchUrl := AnsiUpperCase(vSrcUrl);

  vParamName := '&' + AnsiUpperCase(AParamName) + '=';

  vFrom := Pos(vParamName, vSearchUrl);

  if vFrom = 0 then
  begin
    vParamName := '?' + AnsiUpperCase(AParamName) + '=';
    vFrom := Pos(vParamName, vSearchUrl);
  end;

  if vFrom = 0 then Exit;

  vTo := PosEx('&', vSearchUrl, vFrom + 1);
  if vTo = 0 then
		vTo := Length(vSearchUrl) + 1;

  if vFrom > 1 then
  begin
    if (vSrcUrl[vFrom] = '&') or ((vTo < Length(vSearchUrl) + 1) and (vSrcUrl[vFrom] = '?') and (vSrcUrl[vTo] <> '&')) then
      vFrom := vFrom - 1
  end;

  if (vTo < Length(vSearchUrl) + 1) and ((vSrcUrl[vFrom] = '?') and (vSrcUrl[vTo] = '&')) then
    vTo := vTo + 1;

  Result := Copy(vSrcUrl, 1, vFrom) + Copy(vSrcUrl, vTo, Length(vSearchUrl) + 1);

  if Length(Result) = 0 then Exit;

  if (Result[Length(Result)] = '&') or (Result[Length(Result)] = '?') then
    Result := Copy(Result, 0, Length(Result) - 1)
  else if (Result[1] = '&') or (Result[1] = '?') then
    Result := Copy(Result, 2, Length(Result));
end;

function GetUrlCommand(const AUrl: string; const ADefaultValue: string = ''): string;
var
  vUrl: string;
  vFrom, vTo: Integer;
begin
  if Length(AUrl) = 0 then
  begin
    Result := ADefaultValue;
    Exit;
  end;

  vUrl := AnsiUpperCase(AUrl);
  if Pos('//', vUrl) = 0 then
    vUrl := '//' + vUrl;

	vFrom := Pos('//', vUrl);
	if vFrom = 0 then
  begin
		Result := ADefaultValue;
    Exit;
  end;

	vTo := PosEx('?', AUrl, vFrom);
	if vTo = 0 then
		vTo := Length(AUrl) + 1;

	Result := Copy(AUrl, vFrom, vTo - vFrom);
end;

function ExtractUrlParams(const AUrl: string; const ADefaultValue: string = ''): string;
var
  vFrom: Integer;
begin
  vFrom := Pos('?', AUrl);
  if vFrom = 0 then
    Result := ''
  else
  	Result := Copy(AUrl, vFrom + 1, Length(AUrl) + 1);
end;

function EncodeUrl(const AUrl: string): string;
begin
  Result := TIdURI.URLEncode(AUrl);
end;

function DecodeUrl(const AUrl: string): string;
begin
  Result := TIdURI.URLDecode(AUrl);
end;

function IpToStr(const AIp): string;
var
  i: Integer;
begin
  Result := IntToStr(PByteArray(@AIp)[0]);
  for i := 1 to 3 do
    Result := Result + '.' + IntToStr(PByteArray(@AIp)[i]);
end;

procedure StrToIp(const AIpStr: string; const AIp);
var
  vList: TStringList;
begin
  vList := TStringList.Create;
  try
    vList.Delimiter := '.';
    vList.DelimitedText := AIpStr;
    PByteArray(@AIp)[0] := StrToIntDef(vList[0], 0);
    PByteArray(@AIp)[1] := StrToIntDef(vList[1], 0);
    PByteArray(@AIp)[2] := StrToIntDef(vList[2], 0);
    PByteArray(@AIp)[3] := StrToIntDef(vList[3], 0);
  finally
    FreeAndNil(vList);
  end;
end;

function BinToHex(const ABin: array of Byte): string;
const
  cHexSymbols = '0123456789ABCDEF';
var
  i: Integer;
begin
  SetLength(Result, 2*Length(ABin));
  for i :=  0 to Length(ABin)-1 do
  begin
    Result[1 + 2*i + 0] := cHexSymbols[1 + ABin[i] shr 4];
    Result[1 + 2*i + 1] := cHexSymbols[1 + ABin[i] and $0F];
  end;
end;

procedure HexToBin(const AHex: string; const ABuf);
var
  i: Integer;
begin
  for i := 0 to (Length(AHex) - 1) div 2 do
    PByteArray(@ABuf)[i] := StrToIntDef('$' + AHex.Substring(i * 2, 2), 0);
end;

function CeilTo(const AValue: Double; const ADigit: Integer = -2): Double;
var
  vFactor: Double;
begin
  vFactor := IntPower(10.0, ADigit);
  if AValue < 0 then
    Result := Ceil(AValue / vFactor) * vFactor
  else
    Result := Ceil(AValue / vFactor) * vFactor;
end;

function FloorTo(const AValue: Double; const ADigit: Integer = -2): Double;
var
  vFactor: Double;
begin
  vFactor := IntPower(10.0, ADigit);
  if AValue < 0 then
    Result := Floor(AValue / vFactor) * vFactor
  else
    Result := Floor(AValue / vFactor) * vFactor;
end;

function FormatTo(const AValue: Double; const ADigit: Integer; const AUseStrictFormat: Boolean): string;
const
  cWeakFormats: array[1..10] of string = ('0.#', '0.##', '0.###', '0.####', '0.#####', '0.######',
    '0.#######', '0.########', '0.#########', '0.##########');
  cStrictFormats: array[1..10] of string = ('0.0', '0.00', '0.000', '0.0000', '0.00000', '0.000000',
    '0.0000000', '0.00000000', '0.000000000', '0.0000000000');
begin
  if ADigit >= 0 then
    Result := IntToStr(Round(AValue))
  else if AUseStrictFormat then
    Result := FormatFloat('0,' + cStrictFormats[-ADigit], AValue)
  else
    Result := FormatFloat('0,' + cWeakFormats[-ADigit], AValue);
end;

function FormatAsBitString(const AValue: Cardinal; const ASize: Byte = 32): string;
var
  vMask: Cardinal;
  i: Integer;
begin
  Result := '';
  for i := 0 to ASize - 1 do
  begin
    vMask := 1 shl i;
    if AValue and vMask = vMask then
      Result := '1' + Result
    else
      Result := '0' + Result;
  end;
end;

procedure Print(const AText: string; const AParams: array of const);
var
  vText: string;
begin
  if Length(AParams) > 0 then
    vText := Format(AText, AParams)
  else
    vText := AText;

  if Assigned(PrintBuffer) then
    PrintBuffer.Add(vText)
  else if Assigned(PrintProc) then
    PrintProc(vText);
end;

{ TVersion }

class function TVersion.Compare(Val1, Val2: TVersion): Integer;
begin
  Result := Val1.FMajor - Val2.FMajor;
  if Result <> 0 then
    Exit;
  Result := Val1.FMinor - Val2.FMinor;
  if Result <> 0 then
    Exit;
  Result := Val1.FRelease - Val2.FRelease;
end;

class function TVersion.CompareWithStr(Val1: TVersion; Val2: string): Integer;
var
  vVersion: TVersion;
begin
  vVersion := TVersion.Create(Val2);
  Result := Compare(Val1, Val2);
end;

constructor TVersion.Create(const s: string);
begin
  StrToVersion(s);
end;

class operator TVersion.Equal(Val1, Val2: TVersion): Boolean;
begin
  Result := Compare(Val1, Val2) = 0;
end;

class operator TVersion.Equal(Val1: TVersion; Val2: string): Boolean;
begin
  Result := CompareWithStr(Val1, Val2) = 0;
end;

class operator TVersion.Explicit(Val: string): TVersion;
begin
  Result.StrToVersion(Val);
end;

class operator TVersion.GreaterThan(Val1, Val2: TVersion): Boolean;
begin
  Result := Compare(Val1, Val2) > 0;
end;

class operator TVersion.GreaterThan(Val1: TVersion; Val2: string): Boolean;
begin
  Result := CompareWithStr(Val1, Val2) > 0;
end;

class operator TVersion.GreaterThanOrEqual(Val1, Val2: TVersion): Boolean;
begin
  Result := Compare(Val1, Val2) >= 0;
end;

class operator TVersion.GreaterThanOrEqual(Val1: TVersion; Val2: string): Boolean;
begin
  Result := CompareWithStr(Val1, Val2) >= 0;
end;

class operator TVersion.Implicit(Val: string): TVersion;
begin
  Result.StrToVersion(Val);
end;

function TVersion.IsValid: Boolean;
begin
  Result := (FMajor > 0) or (FMinor > 0) or (FRelease > 0);
end;

class operator TVersion.LessThan(Val1, Val2: TVersion): Boolean;
begin
  Result := Compare(Val1, Val2) < 0;
end;

class operator TVersion.LessThan(Val1: TVersion; Val2: string): Boolean;
begin
  Result := CompareWithStr(Val1, Val2) < 0;
end;

class operator TVersion.LessThanOrEqual(Val1, Val2: TVersion): Boolean;
begin
  Result := Compare(Val1, Val2) <= 0;
end;

class operator TVersion.LessThanOrEqual(Val1: TVersion; Val2: string): Boolean;
begin
  Result := CompareWithStr(Val1, Val2) <= 0;
end;

class operator TVersion.NotEqual(Val1, Val2: TVersion): Boolean;
begin
  Result := Compare(Val1, Val2) <> 0;
end;

class operator TVersion.NotEqual(Val1: TVersion; Val2: string): Boolean;
begin
  Result := CompareWithStr(Val1, Val2) <> 0;
end;

procedure TVersion.StrToVersion(const s: string);
var
  vList: TStrings;
begin
  FMajor := 0; FMinor := 0; FRelease := 0;
  vList := CreateDelimitedList(s, '.');
  try
    if vList.Count > 0 then
      FMajor := StrToIntDef(vList[0], 0);
    if vList.Count > 1 then
      FMinor := StrToIntDef(vList[1], 0);
    if vList.Count > 2 then
      FRelease := StrToIntDef(vList[2], 0);
  finally
    FreeAndNil(vList);
  end;
end;

function TVersion.ToString: string;
begin
  Result := VersionToStr;
end;

function TVersion.VersionToStr: string;
begin
  Result := Format('%d.%d.%d', [FMajor, FMinor, FRelease]);
end;

end.
