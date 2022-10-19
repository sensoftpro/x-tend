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

unit uDomainUtils;

interface

uses
  Classes, uConsts, uJSON, Types, uEntity, uObjectField;

// Comparisions
function IsBlobEqualTo(const AMyValue, AValue: TStream): Boolean;
function CheckCondition(const AValue1, AValue2: Variant; const AFieldKind: TFieldKind;
  const ACondition: TConditionKind; const AModifier: TConditionModifier): Boolean;

// Field values
function StringFromVariant(const AValue: Variant): string;
function IntegerFromVariant(const AValue: Variant): Integer;
function FloatFromVariant(const AValue: Variant): Double;
function DateTimeFromVariant(const AValue: Variant): TDateTime;
function BooleanFromVariant(const AValue: Variant): Boolean;
function ColorFromVariant(const AValue: Variant): Integer;
function CurrencyFromVariant(const AValue: Variant): Currency;
function EntityFromVariant(const AValue: Variant): TEntity;
function ObjectFromVariant(const AValue: Variant): TObject;
function BlobFromVariant(const AValue: Variant): TStream;

function VarToInt(const AValue: Variant; const ANullValue: Integer): Integer;
function VarToString(const AValue: Variant; const ANullValue: string): string;

// Pointer values
function ValuesAreEqual(const ADomain: TObject; const AValue1, AValue2: TJSONValue; const AFieldKind: TFieldKind): Boolean;
function JSONValueToString(const ADomain: TObject; const AValue: TJSONValue; const AFieldKind: TFieldKind): string;

// Reports
procedure SetIntegerValue(var vOut: TReportValue; const AValue: Integer);
procedure SetFloatValue(var vOut: TReportValue; const AValue: Double);
procedure SetDateTimeValue(var vOut: TReportValue; const AValue: TDateTime);
procedure SetBooleanValue(var vOut: TReportValue; const AValue: Boolean);
procedure SetCurrencyValue(var vOut: TReportValue; const AValue: Currency);
procedure SetObjectValue(var vOut: TReportValue; const AValue: TObject);
procedure SetBlobValue(var vOut: TReportValue; const AValue: TObject);

// Blobs
function StreamToString(const AStream: TStream): string;
function StringToStream(const s: string): TStream;

function EntityFilled(const AEntity: TObject): Boolean;

function SendEmail(const ADomain: TObject; const ARecipients, ASubject, AMessage: string;
  const AAttachments: TStrings): Boolean;
function CheckEmailConnection(const ADomain: TObject): string;

function FindLastEntity(const AListField: TListField; const ATimeFieldName: string): TEntity;

implementation

uses
  Variants, SysUtils, Math, uDomain, uComplexObject, uUtils, uTask, uWebTask;

function FindLastEntity(const AListField: TListField; const ATimeFieldName: string): TEntity;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to AListField.Count - 1 do
  begin
    if not Assigned(Result) then
      Result := AListField[i]
    else if Result[ATimeFieldName] < AListField[i][ATimeFieldName] then
      Result := AListField[i];
  end;
end;

function CheckEmailConnection(const ADomain: TObject): string;
var
  vConstants: TEntity;
  vMailService: TEntity;
  vTask: TEMailTask;
  vDomain: TDomain absolute ADomain;
begin
  Result := vDomain.Translate('msgWrongSMTP', 'Ошибка. Приложение не настроено для работы с электронной почтой');
  vConstants := vDomain.FirstEntity('SysConstants');
  if not Assigned(vConstants) then
    Exit;

  vMailService := vConstants.ExtractEntity('MailService');
  if not Assigned(vMailService) then
    Exit;

  vTask := TEMailTask.Create('EMail', vMailService['SmtpHost'], vMailService['SmtpPort'], vConstants['MailLogin'],
    vConstants['MailPassword'], vConstants['MailFromName'], '', vMailService['SmtpTimeout']);
  try
    Result := vTask.CheckConnection;
  finally
    FreeAndNil(vTask);
  end;
end;

function SendEmail(const ADomain: TObject; const ARecipients, ASubject, AMessage: string;
  const AAttachments: TStrings): Boolean;
var
  vConstants: TEntity;
  vMailService: TEntity;
  vTask: TEMailTask;
  vDomain: TDomain absolute ADomain;
begin
  Result := False;
  vConstants := vDomain.FirstEntity('SysConstants');
  if not Assigned(vConstants) then
    Exit;

  vMailService := vConstants.ExtractEntity('MailService');
  if not Assigned(vMailService) then
    Exit;

  vTask := TEMailTask.Create('EMail', vMailService['SmtpHost'], vMailService['SmtpPort'], vConstants['MailLogin'],
    vConstants['MailPassword'], vConstants['MailFromName'], '', vMailService['SmtpTimeout']);
  try
    vTask.InitTask(ARecipients, ASubject, AMessage, AAttachments);
    vTask.ContentType := 'text/html';
    vTask.Start(nil);
    Result := True;
  finally
    FreeAndNil(vTask);
  end;
end;

function EntityFilled(const AEntity: TObject): Boolean;
begin
  Result := Assigned(AEntity);
end;

// Comparisions
function CheckStringCondition(const AValue1, AValue2: Variant;
  const ACondition: TConditionKind; const AModifier: TConditionModifier): Boolean;
var
  vMyValue: string;
  vValue: string;
begin
  Result := False;
  vMyValue := ApplyModifier(StringFromVariant(AValue1), AModifier);
  vValue := ApplyModifier(StringFromVariant(AValue2), AModifier);

  case ACondition of
    ckEqualTo: Result := AnsiCompareStr(vMyValue, vValue) = 0;
    ckMatchesTo: Result := (vValue = '') or (Pos(vValue, vMyValue) = 1);
    //ckEndsWith: Result := (vValue = '') or ((Pos(vValue, vMyValue) > 0) and
    //  (vValue = Copy(vMyValue, Length(vMyValue) - Length(vValue) + 1, Length(vValue))));
    ckGreaterThan: Result := AnsiCompareStr(vMyValue, vValue) > 0;
    ckLessThan: Result := AnsiCompareStr(vMyValue, vValue) < 0;
    ckPartOf: Result := (vValue <> '') and (Pos(vMyValue, vValue) > 0);
    ckContains: Result := (vValue = '') or (Pos(vValue, vMyValue) > 0);
  else
    Assert(False, 'Compare criteria doesn''t supported')
  end;
end;

function CheckIntegerCondition(const AValue1, AValue2: Variant;
  const ACondition: TConditionKind): Boolean;
var
  vMyValue: Integer;
  vValue: Integer;
begin
  Result := False;
  vMyValue := IntegerFromVariant(AValue1);
  vValue := IntegerFromVariant(AValue2);
  case ACondition of
    ckEqualTo: Result := vMyValue = vValue;
    ckGreaterThan: Result := vMyValue > vValue;
    ckLessThan: Result := vMyValue < vValue;
  else
    Assert(False, 'Compare criteria doesn''t supported')
  end;
end;

function CheckFloatCondition(const AValue1, AValue2: Variant;
  const ACondition: TConditionKind): Boolean;
var
  vMyValue: Double;
  vValue: Double;
begin
  Result := False;
  vMyValue := FloatFromVariant(AValue1);
  vValue := FloatFromVariant(AValue2);

  case ACondition of
    ckEqualTo: Result := SameValue(vMyValue, vValue);
    ckGreaterThan: Result := vMyValue > vValue;
    ckLessThan: Result := vMyValue < vValue;
  else
    Assert(False, 'Compare criteria doesn''t supportedя')
  end;
end;

function CheckDateTimeCondition(const AValue1, AValue2: Variant;
  const ACondition: TConditionKind): Boolean;
var
  vMyValue: TDateTime;
  vValue: TDateTime;
begin
  Result := False;
  vMyValue := DateTimeFromVariant(AValue1);
  vValue := DateTimeFromVariant(AValue2);

  case ACondition of
    ckEqualTo: Result := SameValue(vMyValue, vValue);
    ckGreaterThan: Result := vMyValue > vValue;
    ckLessThan: Result := vMyValue < vValue;
  else
    Assert(False, 'Compare criteria doesn''t supported')
  end;
end;

function CheckBooleanCondition(const AValue1, AValue2: Variant;
  const ACondition: TConditionKind): Boolean;
var
  vMyValue: Boolean;
  vValue: Boolean;
begin
  Result := False;
  vMyValue := BooleanFromVariant(AValue1);
  vValue := BooleanFromVariant(AValue2);

  if ACondition = ckEqualTo then
    Result := vMyValue = vValue
  else
    Assert(False, 'Compare criteria doesn''t supported');
end;

function CheckColorCondition(const AValue1, AValue2: Variant;
  const ACondition: TConditionKind): Boolean;
var
  vMyValue: Integer;
  vValue: Integer;
begin
  Result := False;
  vMyValue := ColorFromVariant(AValue1);
  vValue := ColorFromVariant(AValue2);

  if ACondition = ckEqualTo then
    Result := vMyValue = vValue
  else
    Assert(False, 'Compare criteria doesn''t supported');
end;

function CheckCurrencyCondition(const AValue1, AValue2: Variant;
  const ACondition: TConditionKind): Boolean;
var
  vMyValue: Currency;
  vValue: Currency;
begin
  Result := False;
  vMyValue := CurrencyFromVariant(AValue1);
  vValue := CurrencyFromVariant(AValue2);

  case ACondition of
    ckEqualTo: Result := SameValue(vMyValue, vValue);
    ckGreaterThan: Result := vMyValue > vValue;
    ckLessThan: Result := vMyValue < vValue;
  else
    Assert(False, 'Compare criteria doesn''t supported')
  end;
end;

function CheckObjectCondition(const AValue1, AValue2: Variant;
  const ACondition: TConditionKind): Boolean;
var
  vMyValue: TEntity;
  vValue: TObject;
begin
  Result := False;
  vMyValue := EntityFromVariant(AValue1);
  vValue := ObjectFromVariant(AValue2);

  case ACondition of
    ckEqualTo: Result := vMyValue = vValue;
    ckPartOf:
      if Assigned(vValue) then
        Result := TListField(vValue).Contains(vMyValue);
  else
    Assert(False, 'Compare criteria doesn''t supported')
  end;
end;

function CheckListCondition(const AValue1, AValue2: Variant;
  const ACondition: TConditionKind): Boolean;
var
  vMyValue: TEntity;
  vValue: TEntity;
  i: Integer;
begin
  Result := False;

  vMyValue := EntityFromVariant(AValue1);
  vValue := EntityFromVariant(AValue2);
  if not Assigned(vMyValue) or not Assigned(vValue) then
    Exit;

  case ACondition of
    ckContains: Result := TListField(vMyValue).Contains(vValue);
    ckCrosses:
      for i := 0 to TListField(vValue).Count - 1 do
        if TListField(vMyValue).Contains(TListField(vValue)[i]) then
        begin
          Result := True;
          Exit;
        end;
  else
    Assert(False, 'Compare criteria doesn''t supported')
  end;
end;

function IsBlobEqualTo(const AMyValue, AValue: TStream): Boolean;
begin
  Result := AMyValue = AValue;
//  if Result then
//    Exit;

//  Result := Assigned(AMyValue) and Assigned(AValue);
//  if not Result then
//    Exit;

//  Result := AMyValue.Size = AValue.Size;
//  if not Result then
//    Exit;

//  Result := CompareMem(TMemoryStream(AMyValue).Memory,
//    TMemoryStream(AValue).Memory, AMyValue.Size);
end;

function CheckBlobCondition(const AValue1, AValue2: Variant;
  const ACondition: TConditionKind): Boolean;
var
  vMyValue: TStream;
  vValue: TStream;
begin
  Result := False;
  vMyValue := BlobFromVariant(AValue1);
  vValue := BlobFromVariant(AValue2);

  case ACondition of
    ckEqualTo: Result := IsBlobEqualTo(vMyValue, vValue);
  else
    Assert(False, 'Compare criteria doesn''t supported')
  end;
end;

function CheckComplexCondition(const AValue1, AValue2: Variant;
  const ACondition: TConditionKind): Boolean;
var
  vMyValue: TComplexObject;
  vValue: TComplexObject;
begin
  Result := False;
  vMyValue := TComplexObject(ObjectFromVariant(AValue1));
  vValue := TComplexObject(ObjectFromVariant(AValue2));

  case ACondition of
    ckEqualTo: Result := vMyValue = vValue;
  else
    Assert(False, 'Compare criteria doesn''t supported')
  end;
end;

function CheckCondition(const AValue1, AValue2: Variant; const AFieldKind: TFieldKind;
  const ACondition: TConditionKind; const AModifier: TConditionModifier): Boolean;
begin
  case AFieldKind of
    fkString: Result := CheckStringCondition(AValue1, AValue2, ACondition, AModifier);
    fkInteger, fkEnum, fkFlag, fkColor: Result := CheckIntegerCondition(AValue1, AValue2, ACondition);
    fkFloat: Result := CheckFloatCondition(AValue1, AValue2, ACondition);
    fkDateTime: Result := CheckDateTimeCondition(AValue1, AValue2, ACondition);
    fkBoolean: Result := CheckBooleanCondition(AValue1, AValue2, ACondition);
    fkCurrency: Result := CheckCurrencyCondition(AValue1, AValue2, ACondition);
    fkObject: Result := CheckObjectCondition(AValue1, AValue2, ACondition);
    fkList: Result := CheckListCondition(AValue1, AValue2, ACondition);
    fkBlob: Result := CheckBlobCondition(AValue1, AValue2, ACondition);
    fkComplex: Result := CheckComplexCondition(AValue1, AValue2, ACondition);
  else
    Result := False;
  end;
end;

// Field values
function StringFromVariant(const AValue: Variant): string;
begin
  Result := VarToStr(AValue);
end;

function IntegerFromVariant(const AValue: Variant): Integer;
begin
  Result := VarToInt(AValue, cNullInteger);
end;

function FloatFromVariant(const AValue: Variant): Double;
begin
  if VarIsNull(AValue) or not VarIsNumeric(AValue) then
    Result := cNullFloat
  else
    Result := Double(AValue);
end;

function DateTimeFromVariant(const AValue: Variant): TDateTime;
begin
  if VarIsNull(AValue) then
    Result := cNullDateTime
  else
    Result := VarToDateTime(AValue);
end;

function BooleanFromVariant(const AValue: Variant): Boolean;
begin
  if VarIsNull(AValue) or not VarIsOrdinal(AValue) then
    Result := cNullBoolean
  else
    Result := Boolean(AValue);
end;

function ColorFromVariant(const AValue: Variant): Integer;
begin
  Result := VarToInt(AValue, cNullColor);
end;

function CurrencyFromVariant(const AValue: Variant): Currency;
begin
  if VarIsNull(AValue) or not VarIsNumeric(AValue) then
    Result := cNullCurrency
  else
    Result := Currency(AValue);
end;

function EntityFromVariant(const AValue: Variant): TEntity;
begin
  if VarIsNull(AValue) or not VarIsOrdinal(AValue) then
    Result := nil
  else
    Result := TEntity(NativeInt(AValue));
end;

function ObjectFromVariant(const AValue: Variant): TObject;
begin
  if VarIsNull(AValue) or not VarIsOrdinal(AValue) then
    Result := nil
  else
    Result := TObject(NativeInt(AValue));
end;

function BlobFromVariant(const AValue: Variant): TStream;
begin
  if VarIsNull(AValue) or not VarIsOrdinal(AValue) then
    Result := nil
  else
    Result := TStream(Integer(AValue));
end;

function VarToInt(const AValue: Variant; const ANullValue: Integer): Integer;
var
  vValue: Int64;
begin
  if VarIsNull(AValue) or not VarIsOrdinal(AValue) then
    Result := ANullValue
  else
  begin
    vValue := AValue;
    if vValue > MaxInt then // todo: пока обрезаем по максимуму, потом нужно сделать полноценную поддержку Int64
      vValue := MaxInt;
    Result := Integer(vValue);
  end;
end;

function VarToString(const AValue: Variant; const ANullValue: string): string;
begin
  if VarIsNull(AValue) then
    Result := ANullValue
  else
    Result := VarToStr(AValue);
end;

// Pointer values
function ValuesAreEqual(const ADomain: TObject; const AValue1, AValue2: TJSONValue; const AFieldKind: TFieldKind): Boolean;
begin
  Result := AValue1 = AValue2;
  if Result then
    Exit;

  Result := (AValue1 <> nil) and (AValue2 <> nil);
  if Result then
    Result := SameStr(JSONValueToString(ADomain, AValue1, AFieldKind), JSONValueToString(ADomain, AValue2, AFieldKind));
end;

function JSONValueToString(const ADomain: TObject; const AValue: TJSONValue; const AFieldKind: TFieldKind): string;
var
  vTypeID: Integer;
  vEntityID: Integer;
begin
  case AFieldKind of
    fkString: Result := TJSONString(AValue).Value;
    fkInteger, fkEnum, fkFlag:
      Result := AValue.ToString;
    fkFloat: Result := FormatFloat('0.######', TJSONNumber(AValue).AsDouble);
    fkDateTime: Result := FormatDateTime('dd.mm.yyyy hh:nn:ss', TJSONNumber(AValue).AsDouble);
    fkBoolean: begin
      if Boolean(TJSONNumber(AValue).AsInt) then
        Result := 'Yes'
      else
        Result := 'No';
    end;
    fkColor: Result := '#' + IntToHex(TJSONNumber(AValue).AsInt, 8);
    fkCurrency: Result := CurrToStr(TJSONNumber(AValue).AsDouble);
    fkObject: begin
      if AValue is TJSONObject then
      begin
        vTypeID := TJSONObject(AValue).ExtractInteger('type_id');
        vEntityID := TJSONObject(AValue).ExtractInteger('id');
        if vEntityID <= 0 then
          Result := '[EMPTY]'
        else
          Result :=  Format('[%s: %d]', [TDomain(ADomain).CollectionByID(vTypeID).Name, vEntityID]);
      end
      else if AValue is TJSONNumber then
        Result := Format('[ID: %d]', [TJSONNumber(AValue).AsInt])
      else
        Result := '';
    end;
    fkBlob, fkComplex: begin
      if Assigned(AValue) then
        Result := 'md5(' + MD5Hash(AValue.ToString) + ')'
      else
        Result := '[empty]';
    end
  else
    Result := '';
  end
end;

// Reports
procedure SetIntegerValue(var vOut: TReportValue; const AValue: Integer);
begin
  vOut.Value := AValue;
  vOut.FieldKind := fkInteger;
end;

procedure SetFloatValue(var vOut: TReportValue; const AValue: Double);
begin
  vOut.Value := AValue;
  vOut.FieldKind := fkFloat;
end;

procedure SetDateTimeValue(var vOut: TReportValue; const AValue: TDateTime);
begin
  vOut.Value := AValue;
  vOut.FieldKind := fkDateTime;
end;

procedure SetBooleanValue(var vOut: TReportValue; const AValue: Boolean);
begin
  vOut.Value := AValue;
  vOut.FieldKind := fkBoolean;
end;

procedure SetCurrencyValue(var vOut: TReportValue; const AValue: Currency);
begin
  vOut.Value := AValue;
  vOut.FieldKind := fkCurrency;
end;

procedure SetObjectValue(var vOut: TReportValue; const AValue: TObject);
begin
  vOut.Value := Integer(AValue);
  vOut.FieldKind := fkObject;
end;

procedure SetBlobValue(var vOut: TReportValue; const AValue: TObject);
begin
  vOut.Value := Integer(AValue);
  vOut.FieldKind := fkBlob;
end;

// Blobs
function StreamToString(const AStream: TStream): string;
var
  vStream: TStringStream;
begin
  Result := '';
  vStream := TStringStream.Create('');
  try
//    vStream.LoadFromStream(AStream);
    vStream.CopyFrom(AStream, 0); // Delphi 7 support
    Result := EncodeBase64(vStream.DataString);
  finally
    vStream.Free;
  end;
end;

function StringToStream(const s: string): TStream;
begin
  Result := TStringStream.Create(DecodeBase64(s));
end;

end.
