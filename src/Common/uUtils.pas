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
function ColorToAlphaColor(const AColor: TColor; const ATransparency: Double): Cardinal;

// XML utils
function FindNode(const AParentNode: IXMLNode; const ANodeName: string): IXMLNode;
function ExtractNode(const AParentNode: IXMLNode; const APath: string): IXMLNode;
function ExtractDataFromXML(const AFileName: string; const AXMLExtractionFunc: TXMLExtractionFunc): Boolean;

// RegExp
function ExtractByRE(const AText, ARegExpression: string): string;
function CutDataTillMarker(var s: string; const AMarker: string = ' '): string;
function GetNSymbols(const s: string; const ACount: Integer = 1): string;
function JoinIfFilled(const s: string; const APrefix, APostfix: string): string;

// RTTI
function MakeMethod(const ACode, AData: Pointer): TMethod;

function GetUrlParam(const AUrl, AParamName: string; const ADefaultValue: string = ''): string;
function GetUrlCommand(const AUrl: string; const ADefaultValue: string = ''): string;
function ExtractUrlParams(const AUrl: string; const ADefaultValue: string = ''): string;

function EncodeUrl(const AUrl: string): string;
function DecodeUrl(const AUrl: string): string;

function IpToStr(const AIp): string;
procedure StrToIp(const AIpStr: string; const AIp);
function BinToHex(const ABin: array of Byte): string;
procedure HexToBin(const AHex: string; const ABuf);

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
  vParts.Delimiter := ' ';
  vParts.DelimitedText := APattern;
  for c := 0 to vParts.Count - 1 do
    if Pos(vParts[c], AText) = 0 then
    begin
      Result := False;
      Break;
    end;
  vParts.Free;
end;

function GetUrlParamDefTerm(const AUrl, AParamName, AValueIfNone, ATerm: string): string;
var
  vFrom, vTo: Integer;
begin
	vFrom := Pos(AnsiUpperCase(AParamName), AnsiUpperCase(AUrl));
	if vFrom = 0 then
  begin
		Result := AValueIfNone;
    Exit;
  end;

	vFrom := vFrom + Length(AParamName);
	vTo := PosEx(ATerm, AUrl, vFrom);
	if vTo = 0 then
		vTo := Length(AUrl) + 1;

	Result := Copy(AUrl, vFrom, vTo - vFrom);
end;

function GetUrlParam(const AUrl, AParamName: string; const ADefaultValue: string = ''): string;
begin
	Result := GetUrlParamDefTerm(AUrl, AParamName + '=', ADefaultValue, '&');
end;

function GetUrlCommand(const AUrl: string; const ADefaultValue: string = ''): string;
var
  vUrl: string;
begin
  vUrl := AUrl;
  if Pos('//', vUrl) = 0 then
    vUrl := '//' + vUrl;
	Result := GetUrlParamDefTerm(vUrl, '//', ADefaultValue, '?');
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
	vList.Delimiter := '.';
	vList.DelimitedText := AIpStr;
	PByteArray(@AIp)[0] := StrToIntDef(vList[0], 0);
	PByteArray(@AIp)[1] := StrToIntDef(vList[1], 0);
	PByteArray(@AIp)[2] := StrToIntDef(vList[2], 0);
	PByteArray(@AIp)[3] := StrToIntDef(vList[3], 0);
	FreeAndNil(vList);
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

end.
