unit uDomainDataSet;

interface

uses
  Classes, DB, uConsts, uEntityList, uDefinition, uView;

type
  TDomainDataSet = class (TDataSet)
  private
    FIsOpen: Boolean;
    FCursor: Integer;
    FEntityList: TEntityList;
    FView: TView;
    procedure CreateColumnsFromModel;
    procedure CreateColumn(const AFieldDef: TFieldDef);
  protected
    procedure InternalHandleException; override;
    // Инициализация/деинициализация курсора
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: Boolean; override;
    procedure InternalClose; override;
    function GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    // Навигация
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalSetToRecord(Buffer: TRecBuf); override;
    // Разрешение доступа
    function GetCanModify: Boolean; override;
    // Необязательные методы
    function GetRecordCount: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecNo: Integer; override; //Здесь номер записи считается от 1
  public
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function GetFieldData(Field: TField; var ABuffer: TValueBuffer): Boolean; override;

    constructor Create(AOwner:TComponent); override;

    procedure AssignEntityList(const AList: TEntityList; const AView: TView);
  end;

implementation

uses
  Forms, uInteractor, uDomain, uEntity, uObjectField;

type
  TEntityRecordBuffer = record
//    RecordData:TRecordData;
    RecordIndex: Integer; // Номер записи, считая от 1
  end;

  PEntityRecordBuffer = ^TEntityRecordBuffer;

function GetDBFieldType(const ADomainFieldKind: TFieldKind): TFieldType;
begin
  Result := ftUnknown;
  case ADomainFieldKind of
    fkNotDefined: Result := ftUnknown;
    fkString: Result := ftString;
    fkInteger: Result := ftInteger;
    fkEnum: Result := ftInteger;
    fkFlag: Result := ftInteger;
    fkFloat: Result := ftFloat;
    fkDateTime: Result := ftDateTime;
    fkBoolean: Result := ftBoolean;
    fkColor: Result := ftInteger;
    fkCurrency: Result := ftCurrency;
    fkObject: Result := ftString;
    fkList: Result := ftString;
    fkBlob: Result := ftBlob;
    fkComplex: Result := ftBlob;
  end;
end;

function GetDomainFieldKind(const AFieldType: TFieldType): TFieldKind;
begin
  Result := fkNotDefined;
  case AFieldType of
    ftUnknown: Result := fkNotDefined;
    ftString: Result := fkString;
    ftSmallint: Result := fkInteger;
    ftInteger: Result := fkInteger;
    ftWord: Result := fkInteger;
    ftBoolean: Result := fkBoolean;
    ftFloat: Result := fkFloat;
    ftCurrency: Result := fkCurrency;
    ftBCD: Result := fkBlob;
    ftDate: Result := fkDateTime;
    ftTime: Result := fkDateTime;
    ftDateTime: Result := fkDateTime;
    ftBytes: Result := fkBlob;
    ftVarBytes: Result := fkBlob;
    ftAutoInc: Result := fkInteger;
    ftBlob: Result := fkBlob;
    ftMemo: Result := fkString;
    ftGraphic: Result := fkBlob;
    ftFmtMemo: Result := fkString;
    ftParadoxOle: Result := fkBlob;
    ftDBaseOle: Result := fkBlob;
    ftTypedBinary: Result := fkBlob;
    ftCursor: Result := fkInteger;
    ftFixedChar: Result := fkString;
    ftWideString: Result := fkString;
    ftLargeint: Result := fkInteger;
    ftADT: Result := fkBlob;
    ftArray: Result := fkBlob;
    ftReference: Result := fkInteger;
    ftDataSet: Result := fkBlob;
    ftOraBlob: Result := fkBlob;
    ftOraClob: Result := fkBlob;
    ftVariant: Result := fkBlob;
    ftInterface: Result := fkInteger;
    ftIDispatch: Result := fkInteger;
    ftGuid: Result := fkString;
    ftTimeStamp: Result := fkString;
    ftFMTBcd: Result := fkBlob;
    ftFixedWideChar: Result := fkString;
    ftWideMemo: Result := fkString;
    ftOraTimeStamp: Result := fkString;
    ftOraInterval: Result := fkString;
    ftLongWord: Result := fkInteger;
    ftShortint: Result := fkInteger;
    ftByte: Result := fkInteger;
    ftExtended: Result := fkFloat;
    ftConnection: Result := fkBlob;
    ftParams: Result := fkBlob;
    ftStream: Result := fkBlob;
    ftTimeStampOffset: Result := fkString;
    ftObject: Result := fkInteger;
    ftSingle: Result := fkFloat;
  end;
end;

{ TDomainDataSet }

function TDomainDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result, Sizeof(TRecordBuffer));
end;

procedure TDomainDataSet.AssignEntityList(const AList: TEntityList; const AView: TView);
begin
  FEntityList := AList;
  FView := AView;
end;

constructor TDomainDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FIsOpen := False;
end;

procedure TDomainDataSet.CreateColumn(const AFieldDef: TFieldDef);
var
  vDBFieldDef: DB.TFieldDef;
begin
  vDBFieldDef := FieldDefs.AddFieldDef;
  vDBFieldDef.DataType := GetDBFieldType(AFieldDef.Kind);
  vDBFieldDef.FieldNo := FieldDefs.Count + 1;
  vDBFieldDef.Name := AFieldDef.Name;
end;

procedure TDomainDataSet.CreateColumnsFromModel;
var
  vFieldDef: TFieldDef;
  vMainDefinition: TDefinition;
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(FView.Interactor);
  vMainDefinition := FEntityList.MainDefinition;

  for vFieldDef in vMainDefinition.Fields do
  begin
    if vInteractor.NeedSkipColumn(FEntityList, vFieldDef) then
      Continue;

    CreateColumn(vFieldDef);
  end;
end;

procedure TDomainDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer, Sizeof(TRecordBuffer));
end;

function TDomainDataSet.GetCanModify: Boolean;
begin
  Result := False;
end;

function TDomainDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  Result := False;
end;

function TDomainDataSet.GetFieldData(Field: TField; var ABuffer: TValueBuffer): Boolean;
var
  vInt: Integer;
  vEntity: TEntity;
  vEntField: TBaseField;
  vStr: string;
begin
  Result := True;
  vEntity := FEntityList[FCursor - 1];
  vEntField := vEntity.FieldByName(Field.FieldName);

  case vEntField.FieldKind of
    fkNotDefined: ;
    fkString:
      begin
        vStr := vEntField.Value;
        Move(AnsiString(vStr)[1], ABuffer[0], 4);
      end;
    fkInteger:
      begin
        vInt := vEntField.Value;
        Move(PInteger(vInt), ABuffer[0], 4);
      end;
    fkEnum: ;
    fkFlag: ;
    fkFloat: ;
    fkDateTime: ;
    fkBoolean: ;
    fkColor: ;
    fkCurrency: ;
    fkObject:
      begin
        vStr := vEntity.ExtractEntity(Field.FieldName).DisplayName;
        Move(AnsiString(vStr)[1], ABuffer[0], Length(vStr));
      end;
    fkList: ;
    fkBlob: ;
    fkComplex: ;
  end;
end;

function TDomainDataSet.GetRecNo: Integer;
begin
  Result := PEntityRecordBuffer(ActiveBuffer)^.RecordIndex;
end;

function TDomainDataSet.GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grOK;

  case GetMode of
    gmPrior: if FCursor <= 1 then Result := grBOF else Dec(FCursor);
    gmNext: if FCursor >= RecordCount then Result := grEOF else Inc(FCursor);
    gmCurrent: if (FCursor < 1) or (FCursor > RecordCount) then Result := grError;
  end;

  if Result = grOK then
    with PEntityRecordBuffer(Buffer)^ do
      begin
        RecordIndex := FCursor;
      end;

  if (Result = grError) and DoCheck then
    DatabaseError('Error in GetRecord()');
end;

function TDomainDataSet.GetRecordCount: Integer;
begin
  Result := FEntityList.Count;
end;

procedure TDomainDataSet.InternalClose;
begin
  BindFields(False); //Отвязываем поля

  if not (lcPersistent in Fields.LifeCycles) then
    DestroyFields;

  FIsOpen := False;
end;

procedure TDomainDataSet.InternalFirst;
begin
  FCursor := 0;
end;

procedure TDomainDataSet.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TDomainDataSet.InternalInitFieldDefs;
begin
  FieldDefs.Clear;

  CreateColumnsFromModel;
end;

procedure TDomainDataSet.InternalLast;
begin
  FCursor := RecordCount + 1;
end;

procedure TDomainDataSet.InternalOpen;
begin
  InternalInitFieldDefs;
  if not (lcPersistent in Fields.LifeCycles) then
    CreateFields;

  BindFields(True); //Привязываем поля к БД

  FIsOpen := True;
  FCursor := 0;
end;

procedure TDomainDataSet.InternalSetToRecord(Buffer: TRecBuf);
begin
  FCursor := PEntityRecordBuffer(Buffer)^.RecordIndex;
end;

function TDomainDataSet.IsCursorOpen: Boolean;
begin
  Result := FIsOpen;
end;

procedure TDomainDataSet.SetRecNo(Value: Integer);
begin
  if (Value < 1) or (Value >= RecordCount + 1) then Exit;

  FCursor := Value;

  Resync([]);
  //Мне не пришло в голову более умного средства синхронизации логического
  // курсора с физическим, чем выполнение Resync (ресинхронизация – сброс
  // логического курсора и последующее чтение записей, окружающих
  // физический курсор)

end;

end.
