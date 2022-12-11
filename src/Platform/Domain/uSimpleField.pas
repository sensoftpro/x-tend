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

unit uSimpleField;

interface

uses
  Classes, uDefinition, uEntity, uStorage, uComplexObject, uConsts, uJSON;

type
  TSimpleField = class(TBaseField)
  protected
    procedure DoTransfer(const AStorage: TStorage); override;
    procedure DoLoad(const AStorage: TStorage); override;
  end;

  TStringField = class(TSimpleField)
  private
    FValue: string;
  protected
    procedure DoTransfer(const AStorage: TStorage); override;
    procedure DoSetValue(const AValue: Variant); override;
    function DoGetValue: Variant; override;
    function DoCompare(const AValue: Variant): Integer; override;
    function DoCheckValuedCondition(const AValue: Variant; const ACondition:
      TConditionKind; const AModifier: TConditionModifier): Boolean; override;

    function GetJSONValue: TJSONValue; override;
    procedure SetJSONValue(const AJSONValue: TJSONValue); override;
    function DoExtractFieldValue(const AFieldName: string): Variant; override;
  public
    property Value: string read FValue write FValue;
  end;

  TIntegerField = class(TSimpleField)
  private
    FValue: Integer;
  protected
    procedure DoSetValue(const AValue: Variant); override;
    function DoGetValue: Variant; override;
    function DoCompare(const AValue: Variant): Integer; override;
    function DoCheckValuedCondition(const AValue: Variant; const ACondition:
      TConditionKind; const AModifier: TConditionModifier): Boolean; override;

    function GetJSONValue: TJSONValue; override;
    procedure SetJSONValue(const AJSONValue: TJSONValue); override;
    function DoExtractFieldValue(const AFieldName: string): Variant; override;
  public
    property Value: Integer read FValue write FValue;
  end;

  TFloatField = class(TSimpleField)
  private
    FValue: Double;
  protected
    procedure DoSetValue(const AValue: Variant); override;
    function DoGetValue: Variant; override;
    function DoCompare(const AValue: Variant): Integer; override;
    function DoCheckValuedCondition(const AValue: Variant; const ACondition:
      TConditionKind; const AModifier: TConditionModifier): Boolean; override;

    function GetJSONValue: TJSONValue; override;
    procedure SetJSONValue(const AJSONValue: TJSONValue); override;
    function DoExtractFieldValue(const AFieldName: string): Variant; override;
  public
    property Value: Double read FValue write FValue;
  end;

  TDateTimeField = class(TSimpleField)
  private
    FValue: TDateTime;
  protected
    procedure DoSetValue(const AValue: Variant); override;
    function DoGetValue: Variant; override;
    function DoCompare(const AValue: Variant): Integer; override;
    function DoCheckValuedCondition(const AValue: Variant; const ACondition:
      TConditionKind; const AModifier: TConditionModifier): Boolean; override;

    function GetJSONValue: TJSONValue; override;
    procedure SetJSONValue(const AJSONValue: TJSONValue); override;
    function DoExtractFieldValue(const AFieldName: string): Variant; override;
  public
    property Value: TDateTime read FValue write FValue;
  end;

  TBooleanField = class(TSimpleField)
  private
    FValue: Boolean;
  protected
    procedure DoSetValue(const AValue: Variant); override;
    function DoGetValue: Variant; override;
    function DoCompare(const AValue: Variant): Integer; override;
    function DoCheckValuedCondition(const AValue: Variant; const ACondition:
      TConditionKind; const AModifier: TConditionModifier): Boolean; override;

    function GetJSONValue: TJSONValue; override;
    procedure SetJSONValue(const AJSONValue: TJSONValue); override;
    function DoExtractFieldValue(const AFieldName: string): Variant; override;
  public
    property Value: Boolean read FValue write FValue;
  end;

  TColorField = class(TSimpleField)
  private
    FValue: Integer;
  protected
    procedure DoSetValue(const AValue: Variant); override;
    function DoGetValue: Variant; override;
    function DoCompare(const AValue: Variant): Integer; override;
    function DoCheckValuedCondition(const AValue: Variant; const ACondition:
      TConditionKind; const AModifier: TConditionModifier): Boolean; override;

    function GetJSONValue: TJSONValue; override;
    procedure SetJSONValue(const AJSONValue: TJSONValue); override;
    function DoExtractFieldValue(const AFieldName: string): Variant; override;
  public
    property Value: Integer read FValue write FValue;
  end;

  TCurrencyField = class(TSimpleField)
  private
    FValue: Currency;
  protected
    procedure DoSetValue(const AValue: Variant); override;
    function DoGetValue: Variant; override;
    function DoCompare(const AValue: Variant): Integer; override;
    function DoCheckValuedCondition(const AValue: Variant; const ACondition:
      TConditionKind; const AModifier: TConditionModifier): Boolean; override;

    function GetJSONValue: TJSONValue; override;
    procedure SetJSONValue(const AJSONValue: TJSONValue); override;
    function DoExtractFieldValue(const AFieldName: string): Variant; override;
  public
    property Value: Currency read FValue write FValue;
  end;

  TBlobField = class(TBaseField)
  private
    function GetStream: TStream;
    function LoadFromFile(const AFilePath: string): TStream;
  protected
    FStream: TStream;
    procedure DoSetValue(const AValue: Variant); override;
    function DoGetValue: Variant; override;

    function GetJSONValue: TJSONValue; override;
    procedure SetJSONValue(const AJSONValue: TJSONValue); override;
    procedure DoTransfer(const AStorage: TStorage); override;
    procedure DoLoad(const AStorage: TStorage); override;
    function DoExtractFieldValue(const AFieldName: string): Variant; override;
    function DoCompare(const AValue: Variant): Integer; override;
    function DoCheckValuedCondition(const AValue: Variant; const ACondition:
      TConditionKind; const AModifier: TConditionModifier): Boolean; override;
  public
    destructor Destroy; override;

    procedure SetStream(const AHolder: TObject; const AValue: TStream);
    property Stream: TStream read GetStream;
  end;

  TComplexField = class(TBaseField)
  private
    FComplexObject: TComplexObject;
    FLoaded: Boolean;
    FInitialJSON: TJSONValue;
    procedure ComplexObjectChanged(Sender: TObject);
    function GetObjectClassName: string; inline;
    procedure UpdateInitialJSON;
  protected
    function CreateComplexObject: TComplexObject;

    procedure DoSetValue(const AValue: Variant); override;
    function DoGetValue: Variant; override;

    function GetJSONValue: TJSONValue; override;
    procedure SetJSONValue(const AJSONValue: TJSONValue); override;
    procedure DoTransfer(const AStorage: TStorage); override;
    procedure DoLoad(const AStorage: TStorage); override;
    function DoExtractFieldValue(const AFieldName: string): Variant; override;
    function DoCompare(const AValue: Variant): Integer; override;
    function DoCheckValuedCondition(const AValue: Variant; const ACondition:
      TConditionKind; const AModifier: TConditionModifier): Boolean; override;
  public
    constructor Create(const AInstance: TEntity; const AFieldDef: TFieldDef); override;
    destructor Destroy; override;

    function CloneComplexObject: TComplexObject;

    procedure SetObject(const AHolder: TObject; const AValue: TComplexObject);
    property ComplexObject: TComplexObject read FComplexObject;
    property ObjectClassName: string read GetObjectClassName;
    property Loaded: Boolean read FLoaded write FLoaded;
    property InitialJSON: TJSONValue read FInitialJSON;
  end;

implementation

uses
  Math, SysUtils, DateUtils, IOUtils, Variants, uDomain, uDomainUtils, uChangeManager;

function CalcFileName(const ADomain: TObject): string;
var
  vSharedFolder: string;
begin
  vSharedFolder := Trim(TDomain(ADomain).Constant['SharedFolder']);
  Assert(vSharedFolder <> '', 'Shared folder doesn''t defined in the domain');
  if not TDirectory.Exists(vSharedFolder) then
    TDirectory.CreateDirectory(vSharedFolder);

  repeat
    Result := TPath.Combine(vSharedFolder, TPath.GetRandomFileName);
  until not TFile.Exists(Result);
end;

{ TSimpleField }

procedure TSimpleField.DoLoad(const AStorage: TStorage);
begin
  DoSetValue(AStorage.ReadValue(StorageName, FieldKind));
end;

procedure TSimpleField.DoTransfer(const AStorage: TStorage);
var
  vNullableValue: Variant;
begin
  vNullableValue := DoGetValue;
  if not FFieldDef.HasFlag(cRequired) and (vNullableValue = TSimpleFieldDef(FFieldDef).NullValue) then
    vNullableValue := Null;

  AStorage.WriteValue(StorageName, FieldKind, vNullableValue);
end;

{ TStringField }

function TStringField.DoCheckValuedCondition(const AValue: Variant;
  const ACondition: TConditionKind;
  const AModifier: TConditionModifier): Boolean;
begin
  Result := CheckCondition(DoGetValue, AValue, FieldKind, ACondition, AModifier);
end;

function TStringField.DoCompare(const AValue: Variant): Integer;
begin
  Result := CompareStr(StringFromVariant(DoGetValue), StringFromVariant(AValue));
end;

function TStringField.DoExtractFieldValue(const AFieldName: string): Variant;
begin
  Assert(AFieldName = EmptyStr, 'String field can''t get value [' +
    FieldName + '] by parameter [' + AFieldName + ']');

  Result := DoGetValue;
end;

function TStringField.DoGetValue: Variant;
var
  vInstance: TEntity;
begin
  vInstance := OwnerInstance;
  if (vInstance.IsService or vInstance.Definition.HasFlag(ccSystem))
    and FFieldDef.HasFlag(cLocalizable) then
  begin
    Result := TDomain(vInstance.Domain).Translate(
      vInstance.Definition.Name + '#' + IntToStr(vInstance.ID) + '.' + FFieldDef.Name, FValue)
  end
  else
    Result := FValue;
end;

procedure TStringField.DoSetValue(const AValue: Variant);
begin
  FValue := StringFromVariant(AValue);
end;

procedure TStringField.DoTransfer(const AStorage: TStorage);
var
  vMaxSize: Integer;
begin
  if VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    vMaxSize := -1
  else
    vMaxSize := TSimpleFieldDef(FFieldDef).MaxValue;

  if (vMaxSize > 0) and (vMaxSize < Length(FValue)) then
    FValue := Copy(FValue, 1, vMaxSize);

  inherited DoTransfer(AStorage);
end;

function TStringField.GetJSONValue: TJSONValue;
begin
  Result := TJSONString.Create(FValue);
end;

procedure TStringField.SetJSONValue(const AJSONValue: TJSONValue);
begin
  Assert(AJSONValue is TJSONString);
  FValue := TJSONString(AJSONValue).Value;
end;

{ TIntegerField }

function TIntegerField.DoCheckValuedCondition(const AValue: Variant;
  const ACondition: TConditionKind;
  const AModifier: TConditionModifier): Boolean;
begin
  Result := CheckCondition(DoGetValue, AValue, FieldKind, ACondition, AModifier);
end;

function TIntegerField.DoCompare(const AValue: Variant): Integer;
begin
  Result := CompareValue(IntegerFromVariant(DoGetValue), IntegerFromVariant(AValue));
end;

function TIntegerField.DoExtractFieldValue(const AFieldName: string): Variant;
begin
  Assert(AFieldName = EmptyStr, 'Integer field can''t get value [' +
    FieldName + '] by parameter [' + AFieldName + ']');

  Result := DoGetValue;
end;

function TIntegerField.DoGetValue: Variant;
begin
  Result := FValue;
end;

procedure TIntegerField.DoSetValue(const AValue: Variant);
begin
  FValue := IntegerFromVariant(AValue);
end;

function TIntegerField.GetJSONValue: TJSONValue;
begin
  Result := TJSONNumber.Create(FValue);
end;

procedure TIntegerField.SetJSONValue(const AJSONValue: TJSONValue);
begin
  Assert(AJSONValue is TJSONNumber);
  FValue := TJSONNumber(AJSONValue).AsInt;
end;

{ TFloatField }

function TFloatField.DoCheckValuedCondition(const AValue: Variant;
  const ACondition: TConditionKind;
  const AModifier: TConditionModifier): Boolean;
begin
  Result := CheckCondition(DoGetValue, AValue, FieldKind, ACondition, AModifier);
end;

function TFloatField.DoCompare(const AValue: Variant): Integer;
begin
  Result := CompareValue(FloatFromVariant(DoGetValue), FloatFromVariant(AValue));
end;

function TFloatField.DoExtractFieldValue(const AFieldName: string): Variant;
begin
  Assert(AFieldName = EmptyStr, 'Float field can''t get value [' +
    FieldName + '] by parameter [' + AFieldName + ']');

  Result := DoGetValue;
end;

function TFloatField.DoGetValue: Variant;
begin
  Result := FValue;
end;

procedure TFloatField.DoSetValue(const AValue: Variant);
begin
  FValue := FloatFromVariant(AValue);
end;

function TFloatField.GetJSONValue: TJSONValue;
begin
  Result := TJSONNumber.Create(FValue);
end;

procedure TFloatField.SetJSONValue(const AJSONValue: TJSONValue);
begin
  Assert(AJSONValue is TJSONNumber);
  FValue := TJSONNumber(AJSONValue).AsDouble;
end;

{ TDateTimeField }

function TDateTimeField.DoCheckValuedCondition(const AValue: Variant;
  const ACondition: TConditionKind;
  const AModifier: TConditionModifier): Boolean;
begin
  Result := CheckCondition(DoGetValue, AValue, FieldKind, ACondition, AModifier);
end;

function TDateTimeField.DoCompare(const AValue: Variant): Integer;
begin
  Result := CompareValue(DateTimeFromVariant(DoGetValue), DateTimeFromVariant(AValue));
end;

function TDateTimeField.DoExtractFieldValue(const AFieldName: string): Variant;
begin
  Assert(AFieldName = EmptyStr, 'Datetime field can''t get value [' +
    FieldName + '] by parameter [' + AFieldName + ']');

  Result := DoGetValue;
end;

function TDateTimeField.DoGetValue: Variant;
begin
  Result := FValue;
end;

procedure TDateTimeField.DoSetValue(const AValue: Variant);
begin
  FValue := DateTimeFromVariant(AValue);
end;

function TDateTimeField.GetJSONValue: TJSONValue;
begin
  if FValue < 0 then
    Result := TJSONNumber.Create(cNullDateTime)
  else
    Result := TJSONNumber.Create(FValue);
end;

procedure TDateTimeField.SetJSONValue(const AJSONValue: TJSONValue);
begin
  Assert(AJSONValue is TJSONNumber);
  FValue := TJSONNumber(AJSONValue).AsDouble;
end;

{ TBooleanField }

function TBooleanField.DoCheckValuedCondition(const AValue: Variant;
  const ACondition: TConditionKind;
  const AModifier: TConditionModifier): Boolean;
begin
  Result := CheckCondition(DoGetValue, AValue, FieldKind, ACondition, AModifier);
end;

function TBooleanField.DoCompare(const AValue: Variant): Integer;
begin
  Result := CompareValue(NativeInt(BooleanFromVariant(DoGetValue)),
    NativeInt(BooleanFromVariant(AValue)));
end;

function TBooleanField.DoExtractFieldValue(const AFieldName: string): Variant;
begin
  Assert(AFieldName = EmptyStr, 'Boolean field can''t get value [' +
    FieldName + '] by parameter [' + AFieldName + ']');

  Result := DoGetValue;
end;

function TBooleanField.DoGetValue: Variant;
begin
  Result := FValue;
end;

procedure TBooleanField.DoSetValue(const AValue: Variant);
begin
  FValue := BooleanFromVariant(AValue);
end;

function TBooleanField.GetJSONValue: TJSONValue;
begin
  Result := TJSONNumber.Create(NativeInt(FValue));
end;

procedure TBooleanField.SetJSONValue(const AJSONValue: TJSONValue);
begin
  if AJSONValue is TJSONNumber then
    FValue := Boolean(TJSONNumber(AJSONValue).AsInt)
  else
    FValue := Boolean(StrToIntDef(TJSONString(AJSONValue).Value, 0));
end;

{ TColorField }

function TColorField.DoCheckValuedCondition(const AValue: Variant;
  const ACondition: TConditionKind;
  const AModifier: TConditionModifier): Boolean;
begin
  Result := CheckCondition(DoGetValue, AValue, FieldKind, ACondition, AModifier);
end;

function TColorField.DoCompare(const AValue: Variant): Integer;
begin
  Result := CompareValue(ColorFromVariant(DoGetValue), ColorFromVariant(AValue));
end;

function TColorField.DoExtractFieldValue(const AFieldName: string): Variant;
begin
  Assert(AFieldName = EmptyStr, 'Color field can''t get value [' +
    FieldName + '] by parameter [' + AFieldName + ']');

  Result := DoGetValue;
end;

function TColorField.DoGetValue: Variant;
begin
  Result := FValue;
end;

procedure TColorField.DoSetValue(const AValue: Variant);
begin
  FValue := ColorFromVariant(AValue);
end;

function TColorField.GetJSONValue: TJSONValue;
begin
  Result := TJSONNumber.Create(FValue);
end;

procedure TColorField.SetJSONValue(const AJSONValue: TJSONValue);
begin
  Assert(AJSONValue is TJSONNumber);
  FValue := TJSONNumber(AJSONValue).AsInt;
end;

{ TCurrencyField }

function TCurrencyField.DoCheckValuedCondition(const AValue: Variant;
  const ACondition: TConditionKind;
  const AModifier: TConditionModifier): Boolean;
begin
  Result := CheckCondition(DoGetValue, AValue, FieldKind, ACondition, AModifier);
end;

function TCurrencyField.DoCompare(const AValue: Variant): Integer;
begin
  Result := CompareValue(CurrencyFromVariant(DoGetValue), CurrencyFromVariant(AValue));
end;

function TCurrencyField.DoExtractFieldValue(const AFieldName: string): Variant;
begin
  Assert(AFieldName = EmptyStr, 'Currency field can''t get value [' +
    FieldName + '] by parameter [' + AFieldName + ']');

  Result := DoGetValue;
end;

function TCurrencyField.DoGetValue: Variant;
begin
  Result := FValue;
end;

procedure TCurrencyField.DoSetValue(const AValue: Variant);
begin
  FValue := CurrencyFromVariant(AValue);
end;

function TCurrencyField.GetJSONValue: TJSONValue;
begin
  Result := TJSONNumber.Create(FValue);
end;

procedure TCurrencyField.SetJSONValue(const AJSONValue: TJSONValue);
begin
  Assert(AJSONValue is TJSONNumber);
  FValue := TJSONNumber(AJSONValue).AsDouble;
end;

{ TBlobField }

destructor TBlobField.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TBlobField.DoCheckValuedCondition(const AValue: Variant;
  const ACondition: TConditionKind;
  const AModifier: TConditionModifier): Boolean;
begin
  Result := CheckCondition(DoGetValue, AValue, FieldKind, ACondition, AModifier);
end;

function TBlobField.DoCompare(const AValue: Variant): Integer;
var
  vMyValue: TStream;
  vValue: TStream;
begin
  vMyValue := BlobFromVariant(DoGetValue);
  vValue := BlobFromVariant(AValue);
  if IsBlobEqualTo(vMyValue, vValue) then
    Result := 0
  else
    Result := 1;
end;

function TBlobField.DoExtractFieldValue(const AFieldName: string): Variant;
begin
  Assert(False, 'Retrieving value from BLOB');
end;

function TBlobField.DoGetValue: Variant;
begin
  Result := NativeInt(FStream);
end;

procedure TBlobField.DoLoad(const AStorage: TStorage);
var
  vStream: TStream;
begin
  vStream := nil;
  if GetStorageKind = skSharedFolder then
  begin
    if FInstance.FieldExists('ContentUrl') then
    begin
      FInstance.FieldByName('ContentUrl').Load(AStorage);
      if TDomain(FDomain).SharedFolderAvailable { TFile.Exists(FInstance['ContentUrl'])} then
        //vStream := LoadFromFile(FInstance['ContentUrl'])
      else
        vStream := AStorage.ReadStream(StorageName);
    end
    else
      vStream := AStorage.ReadStream(StorageName);
  end
  else
    { TODO -owa -cspeed : Долгая операция }
    vStream := AStorage.ReadStream(StorageName);

  DoSetValue(NativeInt(vStream));
end;

procedure TBlobField.DoSetValue(const AValue: Variant);
begin
  if Assigned(FStream) then
    FStream.Free;
  FStream := BlobFromVariant(AValue);
end;

procedure TBlobField.DoTransfer(const AStorage: TStorage);
var
  vFilePath: string;
  vContentUrlField: TBaseField;
begin
  if GetStorageKind = skSharedFolder then
  begin
    AStorage.WriteStream(StorageName, nil);

    Assert(FInstance.FieldExists('ContentUrl'), 'There is no place to store content url');
    vContentUrlField := FInstance.FieldByName('ContentUrl');
    vFilePath := vContentUrlField.Value;

    if Assigned(FStream) then
    begin
      if not TFile.Exists(vFilePath) then
      begin
        if vFilePath = '' then
          vFilePath := CalcFileName(FDomain);
        vContentUrlField.Value := vFilePath;
        vContentUrlField.Transfer(AStorage);
      end;
      TMemoryStream(FStream).SaveToFile(vFilePath);
    end
    else if TFile.Exists(vFilePath) then
    begin
      try
        TFile.Delete(vFilePath);
      except
      end;

      vContentUrlField.Value := '';
      vContentUrlField.Transfer(AStorage);
    end;
  end
  else
    AStorage.WriteStream(StorageName, FStream);
end;

function TBlobField.GetJSONValue: TJSONValue;
begin
  if Assigned(FStream) then
    Result := TJSONString.Create(StreamToString(FStream))
  else
    Result := nil;
end;

function TBlobField.GetStream: TStream;
begin
  if not Assigned(FStream) and (GetStorageKind = skSharedFolder) and FInstance.FieldExists('ContentUrl') then
    FStream := LoadFromFile(FInstance['ContentUrl']);
  Result := FStream;
end;

function TBlobField.LoadFromFile(const AFilePath: string): TStream;
var
  vStream: TMemoryStream;
begin
  Result := nil;
  if TFile.Exists(AFilePath) then
  begin
    vStream := TMemoryStream.Create;
    try
      vStream.LoadFromFile(AFilePath);
      vStream.Position := 0;
      Result := vStream;
    except
      FreeAndNil(vStream);
    end;
  end;
end;

procedure TBlobField.SetJSONValue(const AJSONValue: TJSONValue);
begin
  if (not Assigned(AJSONValue)) or AJSONValue.Null then
    DoSetValue(0)
  else
    DoSetValue(NativeInt(StringToStream(TJSONString(AJSONValue).Value)));
end;

procedure TBlobField.SetStream(const AHolder: TObject; const AValue: TStream);
begin
  SetValue(AHolder, NativeInt(AValue));
end;

{ TComplexField }

function TComplexField.CloneComplexObject: TComplexObject;
begin
  if Assigned(FComplexObject) then
  begin
    Result := CreateComplexObject;
    FComplexObject._Fill(Result);
  end
  else
    Result := nil;
end;

constructor TComplexField.Create(const AInstance: TEntity; const AFieldDef: TFieldDef);
begin
  inherited Create(AInstance, AFieldDef);
  FComplexObject := CreateComplexObject;
  FLoaded := False;
  FInitialJSON := nil;
end;

function TComplexField.CreateComplexObject: TComplexObject;
var
  vObjectKindName: string;
  vComplexClassDef: TComplexClassDef;
  vComplexClass: TComplexClass;
begin
  vObjectKindName := TComplexFieldDef(FFieldDef).ObjectKindName;
  vComplexClassDef := TDomain(FInstance.Domain).Configuration.ComplexClasses.ObjectByName(vObjectKindName);
  Assert(Assigned(vComplexClassDef), Format('Для типа [%s] не найден класс объекта', [vObjectKindName]));
  vComplexClass := TComplexClass(vComplexClassDef.ComplexClass);
  Result := vComplexClass.Create(Self, ComplexObjectChanged);
end;

destructor TComplexField.Destroy;
begin
  FreeAndNil(FInitialJSON);
  FreeAndNil(FComplexObject);
  inherited Destroy;
end;

function TComplexField.DoCheckValuedCondition(const AValue: Variant; const ACondition: TConditionKind;
  const AModifier: TConditionModifier): Boolean;
begin
  Result := CheckCondition(DoGetValue, AValue, FieldKind, ACondition, AModifier);
end;

function TComplexField.DoCompare(const AValue: Variant): Integer;
begin
  if ObjectFromVariant(AValue) = FComplexObject then
    Result := 0
  else
    Result := 1;
end;

function TComplexField.DoExtractFieldValue(const AFieldName: string): Variant;
begin
  Assert(False, 'Retrieving simple value from Complex Object');
end;

function TComplexField.DoGetValue: Variant;
begin
  Result := NativeInt(FComplexObject);
end;

procedure TComplexField.DoLoad(const AStorage: TStorage);
begin
  FComplexObject.Load(AStorage);
  UpdateInitialJSON;
end;

procedure TComplexField.DoSetValue(const AValue: Variant);
begin
  if Assigned(FComplexObject) then
    FComplexObject.Free;
  FComplexObject := TComplexObject(ObjectFromVariant(AValue));
end;

procedure TComplexField.DoTransfer(const AStorage: TStorage);
begin
  FComplexObject.Transfer(AStorage);
  UpdateInitialJSON;
end;

function TComplexField.GetJSONValue: TJSONValue;
begin
  Result := FComplexObject.JSONValue;
end;

function TComplexField.GetObjectClassName: string;
begin
  Result := TComplexFieldDef(FFieldDef).ObjectKindName;
end;

procedure TComplexField.ComplexObjectChanged(Sender: TObject);
var
  vHolder: TChangeHolder absolute Sender;
begin
  if not FInstance.IsNew then
    vHolder.KeepOldData(TEntity(FInstance), GetFieldName);

  FInstance.ProcessFieldChanged(vHolder, dckFieldChanged, GetFieldName, nil);
end;

procedure TComplexField.SetJSONValue(const AJSONValue: TJSONValue);
begin
  FComplexObject.JSONValue := AJSONValue;
end;

procedure TComplexField.SetObject(const AHolder: TObject; const AValue: TComplexObject);
begin
  SetValue(AHolder, NativeInt(AValue));
end;

procedure TComplexField.UpdateInitialJSON;
begin
  if Assigned(FInitialJSON) then
    FreeAndNil(FInitialJSON);
  FInitialJSON := FComplexObject.JSONValue;
end;

end.
