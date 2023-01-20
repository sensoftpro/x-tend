unit uFireDACStorage;

interface

uses
  uStorage, uParameters, uJSON, Classes, DB, uConsts, Generics.Collections, FireDAC.Comp.Client,
  FireDAC.Phys.SQLite, FireDAC.Stan.Def, FireDAC.DApt, FireDAC.Stan.Async, //FireDAC.Phys.SQLiteWrapper.Stat,
  //{$IFDEF FMX}
  FireDAC.FMXUI.Wait,
  //{$ELSE}
  //FireDAC.VCLUI.Wait,
  //{$ENDIF}
  FireDAC.UI.Intf, FireDAC.Stan.Intf, FireDAC.Stan.Param, FireDAC.Comp.UI;

type
  TFireDACContext = class
  private
    function BuildInsertQuery: string;
    function BuildUpdateQuery: string;
    function BuildDeleteQuery: string;
    function GetParamValues: TFDParams;
    function GetKeyParamValues: TFDParams;
    function DataTypeFromFieldKind(const AKind: TFieldKind): TFieldType;
  protected
    FStorage: TStorage;
    FTableName: string;
    FParameters: TList<TBaseParameter>;
    FKeyParameters: TList<TBaseParameter>;
  public
    constructor Create(const AStorage: TStorage; const ATableName: string);
    destructor Destroy; override;

    procedure AddParameter(const AName: string; const AValue: Variant; const AFieldKind: TFieldKind;
      const AIsKey: Boolean = False); virtual;
//    procedure AddBlobParameter(const AName: string; const AValue: TStream); virtual;
//
    procedure Commit(const AConnection: TFDConnection; const ASaveAction: TEntitySaveAction);
  end;

  TFireDACStorage = class(TStorage)
  protected
    FGroupList: TList;
    FConnection: TFDCOnnection;
    FCurTableName: string;
    FCurTableColumns: TStrings;
    FCurTableTypes: TStrings;
    FTransCount: Integer;
    FDatabase: String;

    function GetActiveContext: TFireDACContext;
    function GetActiveDataset: TDataset;

    procedure DoConnect; override;
    procedure DoDisconnect; override;
    procedure DoRebuild; override;
    procedure DoActivate(const ATag: string); override;
    procedure DoDeactivate; override;

    procedure Push(const AObject: TObject);
    procedure Pop;
    procedure PopContext;

    function GetMaxTableID(const ATableName: string): Integer;
    function DoGetVersion: string; override;
    function DoGetLastLogID: Integer; override;
//    function DoGetChanges(const ALogID: Integer): TJSONObject; override;

    procedure DoSetVersion(const Value: string); override;
    function DoCreateIDs(const ATag: string; const ACount: Integer): Integer; override;
    function DoCreateCodes(const ATag: string; const ACount: Integer = 1): Integer; override;

    function DoSyncGroupDef(const ATag: string; const ASyncFunc: TStorageFunc): Boolean; override;
    procedure DoSyncItemDef(const ATag: string; const AKind: TFieldKind; const ASize: Integer); override;

    procedure DoReadGroup(const ATag: string; const AReadFunc: TStorageFunc); override;
    function DoReadValue(const ATag: string; const AFieldKind: TFieldKind): Variant; override;

    procedure DoWriteItem(const AWriteFunc: TStorageFunc;
      const AID, ALogID: Integer; const ASaveAction: TEntitySaveAction); override;
    procedure DoWriteValue(const ATag: string; const AFieldKind: TFieldKind; const AValue: Variant;
      const AIsKey: Boolean = False); override;

    function SearchTable(const AName: string): Boolean;
    function SQLTypeFromFieldKind(const AKind: TFieldKind; const ASize: Integer): string;
    // Работа с транзакциями
    function DoBeginTransaction: Integer; override;
    procedure DoCommitTransaction(const ATransactionID: Integer = -1); override;
    procedure DoRollbackTransaction(const ATransactionID: Integer = -1); override;
  public
    constructor Create(const ADomain: TObject; const AName: string); override;
    destructor Destroy; override;

    procedure UpdateNumerator(const ATag: string; const ALastID: Integer); override;
  end;

implementation

uses
  uModule, uDomain, uSettings, SysUtils, IOUtils, Variants, uDomainUtils, Math;

{ TFireDACContext }

constructor TFireDACContext.Create(const AStorage: TStorage; const ATableName: string);
begin
  inherited Create;

  FStorage := AStorage;
  FTableName := ATableName;
  FParameters := TList<TBaseParameter>.Create;
  FKeyParameters := TList<TBaseParameter>.Create;
end;

destructor TFireDACContext.Destroy;
var
  i: Integer;
begin
  for i := 0 to FParameters.Count - 1 do
    TSimpleParameter(FParameters[i]).Free;
  for i := 0 to FKeyParameters.Count - 1 do
    TSimpleParameter(FKeyParameters[i]).Free;

  FParameters.Free;
  FKeyParameters.Free;
  FStorage := nil;

  inherited Destroy;
end;

function TFireDACContext.BuildDeleteQuery: string;
var
  i: Integer;
  vWhereStr: string;
  vWasFirst: Boolean;
  vParamName: string;
begin
  vWhereStr := '';

  vWasFirst := False;
  for i := 0 to FKeyParameters.Count - 1 do
  begin
    vParamName := TSimpleParameter(FKeyParameters[i]).Name;
    if not vWasFirst then
    begin
      vWhereStr := '[' + vParamName + '] = :'+ vParamName;
      vWasFirst := True;
    end
    else
      vWhereStr := vWhereStr + ' and [' + vParamName + '] = :'+ vParamName;
  end;

  if Length(vWhereStr) = 0 then
    Result := ''
  else
    Result := Format('delete from [%s] where %s', [FTableName, vWhereStr]);
end;

function TFireDACContext.BuildInsertQuery: string;
var
  i: Integer;
  vVarStr: string;
  vParamStr: string;
  vParamName: string;
begin
  if FParameters.Count + FKeyParameters.Count <= 0 then
  begin
    Result := '';
    Exit;
  end;

  vVarStr := '';
  vParamStr := '';

  for i := 0 to FParameters.Count - 1 do
  begin
    vParamName := TSimpleParameter(FParameters[i]).Name;
    if vVarStr <> '' then
    begin
      vVarStr := vVarStr + ', ';
      vParamStr := vParamStr + ', ';
    end;
    vVarStr := vVarStr + '[' + vParamName + ']';
    vParamStr := vParamStr + ':' + vParamName;
  end;

  for i := 0 to FKeyParameters.Count - 1 do
  begin
    vParamName := TSimpleParameter(FKeyParameters[i]).Name;
    if vVarStr <> '' then
    begin
      vVarStr := vVarStr + ', ';
      vParamStr := vParamStr + ', ';
    end;
    vVarStr := vVarStr + '[' + vParamName + ']';
    vParamStr := vParamStr + ':' + vParamName;
  end;

  Result := Format('insert into [%s] (%s) values (%s)', [
    FTableName, vVarStr, vParamStr]);
end;

function TFireDACContext.BuildUpdateQuery: string;
var
  i: Integer;
  vSetStr: string;
  vWhereStr: string;
  vWasFirst: Boolean;
  vParamName: string;
begin
  vSetStr := '';
  vWhereStr := '';
  vWasFirst := False;

  for i := 0 to FParameters.Count - 1 do
  begin
    vParamName := TSimpleParameter(FParameters[i]).Name;
    if not vWasFirst then
    begin
      vSetStr := '[' + vParamName + '] = :' + vParamName;
      vWasFirst := True;
    end
    else
      vSetStr := vSetStr + ', [' + vParamName + '] = :' + vParamName;
  end;

  vWasFirst := False;
  for i := 0 to FKeyParameters.Count - 1 do
  begin
    vParamName := TSimpleParameter(FKeyParameters[i]).Name;
    if not vWasFirst then
    begin
      vWhereStr := '[' + vParamName + '] = :' + vParamName;
      vWasFirst := True;
    end
    else
      vWhereStr := vWhereStr + ' and [' + vParamName + '] = :' + vParamName;
  end;

  if (Length(vSetStr) = 0) or (Length(vWhereStr) = 0) then
    Result := ''
  else
    Result := Format('update [%s] set %s where %s', [FTableName, vSetStr, vWhereStr]);
end;

procedure TFireDACContext.Commit(const AConnection: TFDConnection; const ASaveAction: TEntitySaveAction);
var
  vSQLText: string;
begin
  case ASaveAction of
    esaInsert: vSQLText := BuildInsertQuery;
    esaUpdate: vSQLText := BuildUpdateQuery;
    esaDelete: vSQLText := BuildDeleteQuery;
  else
    vSQLText := '';
  end;

  if Trim(vSQLText) = '' then
    Exit;

  Assert(Length(vSQLText) > 0, 'Invalid query for data storing');

  if ASaveAction = esaDelete then
    AConnection.ExecSQL(vSQLText, GetKeyParamValues)
  else
    AConnection.ExecSQL(vSQLText, GetParamValues);
end;

function TFireDACContext.GetKeyParamValues: TFDParams;
var
  i: Integer;
  vSimpleParameter: TSimpleParameter;
begin
  Result := TFDParams.Create;
  for i := 0 to FKeyParameters.Count - 1 do
  begin
    vSimpleParameter := TSimpleParameter(FKeyParameters[i]);
    Result.Add(vSimpleParameter.Name, vSimpleParameter.Value);
  end;
end;

function TFireDACContext.DataTypeFromFieldKind(const AKind: TFieldKind): TFieldType;
begin         
  case AKind of
    fkString: Result := ftString;
    fkFloat, fkDateTime, fkCurrency: Result := ftFloat;
  else
    Result := ftInteger;
  end;
end;

function TFireDACContext.GetParamValues: TFDParams;
var
  i: Integer;
  vParam: TBaseParameter;
  vSimpleParameter: TSimpleParameter;
  vBlobParameter: TBlobParameter;
  vFDParam: TFDParam;
begin
  Result := TFDParams.Create;
  for i := 0 to FParameters.Count - 1 do
  begin
    vParam := FParameters[i];
    vFDParam := Result.Add;
    vFDParam.Name := vParam.Name;
    if vParam.ParamKind = pkSimple then
    begin
      vSimpleParameter := TSimpleParameter(FParameters[i]);
      vFDParam.DataType := DataTypeFromFieldKind(vSimpleParameter.FieldKind);
      vFDParam.Value := vSimpleParameter.Value;
    end else
    if vParam.ParamKind = pkBlob then
    begin
      vBlobParameter := TBlobParameter(FParameters[i]);
      vFDParam.LoadFromStream(vBlobParameter.Value, ftBlob);
    end;
  end;
  for i := 0 to FKeyParameters.Count - 1 do
  begin
    vSimpleParameter := TSimpleParameter(FKeyParameters[i]);
    Result.Add(vSimpleParameter.Name, vSimpleParameter.Value);
  end;
end;

procedure TFireDACContext.AddParameter(const AName: string; const AValue: Variant; const AFieldKind: TFieldKind;
  const AIsKey: Boolean = False);
var
  vParameter: TBaseParameter;
begin
  for vParameter in FParameters do
    if SameText(vParameter.Name, AName) then
      Exit;

  vParameter := TSimpleParameter.Create(AName, AValue, AFieldKind, AIsKey);
  if AIsKey then
    FKeyParameters.Add(vParameter)
  else
    FParameters.Add(vParameter);
end;

{ TFireDACStorage }

constructor TFireDACStorage.Create(const ADomain: TObject; const AName: string);
var
  vSettings: TSettings;
  vSectionName: String;
begin
  inherited Create(ADomain, AName);

  FGroupList := TList.Create;
  FCurTableColumns := TStringList.Create;
  FCurTableTypes := TStringList.Create;


  vSettings := TDomain(ADomain).Settings;
  if vSettings.SectionExists(AName) then
    vSectionName := AName
  else
    vSectionName := 'FireDAC';

  FDatabase := vSettings.GetValue(vSectionName, 'Database', 'empty.db');
  if TPath.IsRelativePath(ExtractFilePath(FDatabase)) then
    FDatabase := TPath.Combine(TDomain(ADomain).Configuration.ConfigurationDir, FDatabase);

  FConnection := TFDConnection.Create(nil);
  FConnection.DriverName := vSettings.GetValue(vSectionName, 'DBType', 'SQLite');
  FConnection.Params.Database := FDatabase;

  if not vSettings.SectionExists(vSectionName) then
    vSettings.SetValue(AName, 'Database', FDatabase);

  FLogger.AddMessage(FDatabase);
end;

destructor TFireDACStorage.Destroy;
begin
  Disconnect;
  FreeAndNil(FConnection);
  FreeAndNil(FGroupList);
  FreeAndNil(FCurTableColumns);
  FreeAndNil(FCurTableTypes);
  inherited Destroy;
end;

procedure TFireDACStorage.DoConnect;
begin
  try
    FConnection.Connected := True;
  except
    on E: Exception do
    begin
      FLogger.AddMessage('Error: ' + E.Message);
      raise;
    end;
  end;
end;

procedure TFireDACStorage.DoDisconnect;
begin
  FConnection.Connected := False;
end;

function TFireDACStorage.DoGetVersion: string;
var
  vDataset: TDataset;
begin
  try
    FConnection.ExecSQL('select [version] from [sys_constants]', vDataset);
    try
      if not vDataset.Eof then
        Result := vDataset.FieldByName('version').AsString
      else
        Result := '';
    finally
      FreeAndNil(vDataset);
    end;
  except
    Result := '';
  end;
end;

procedure TFireDACStorage.DoSetVersion(const Value: string);
begin
  try
    FConnection.ExecSQL(Format('update [sys_constants] set [version] = ''%s''', [Value]));
  except
  end;
end;


function TFireDACStorage.DoSyncGroupDef(const ATag: string;
  const ASyncFunc: TStorageFunc): Boolean;
var
  i: Integer;
  vAlterText: string;
  vColName: string;
begin
  Result := SearchTable(ATag);
  if not Result then
  begin
    try
      if ATag = 'numerators' then
        vAlterText := 'CREATE TABLE [' + ATag + '] ([id] INTEGER NULL)'
      else
        vAlterText := 'CREATE TABLE [' + ATag + '] ([id] INTEGER NOT NULL PRIMARY KEY)';
      FConnection.ExecSQL(vAlterText);

      FCurTableName := ATag;
      FCurTableColumns.Add('id');
      FCurTableTypes.Add('INTEGER');
    except
      on E: Exception do
      begin
        FLogger.AddMessage('Error: ' + E.Message);
      end;
    end;
  end;

  if Assigned(ASyncFunc) then
    ASyncFunc(Self);

  for i := FCurTableColumns.Count - 1 downto 0 do
  begin
    vColName := FCurTableColumns[i];
    if FItemDefsList.IndexOf(vColName) < 0 then
    begin
      FConnection.ExecSQL(Format('ALTER TABLE [%s] DROP [%s]', [FCurTableName, vColName]));
      FCurTableColumns.Delete(i);
      FCurTableTypes.Delete(i);
    end;
  end;
end;

function TFireDACStorage.SearchTable(const AName: string): Boolean;
var
  vDataset: TDataset;
begin
  Result := False;
  FCurTableName := '';
  FCurTableColumns.Clear;
  FCurTableTypes.Clear;

  if AName = '' then Exit;

  try
    FConnection.ExecSQL(Format('PRAGMA table_info(%s)', [AName]), vDataset);
  except
    Exit;
  end;
  Result :=  vDataset.RecordCount > 0;
  if not Result then Exit;

  try
    FCurTableName := AName;

    vDataset.First;
    while not vDataset.Eof do
    begin
      FCurTableColumns.Add(vDataset.FieldByName('name').AsString);
      FCurTableTypes.Add(vDataset.FieldByName('type').AsString);
      vDataset.Next;
    end;

  finally
    FreeAndNil(vDataset);
  end;
end;

procedure TFireDACStorage.DoSyncItemDef(const ATag: string; const AKind: uConsts.TFieldKind; const ASize: Integer);
var
  vAlterText, vType, vColumn: string;
  vColIdx: Integer;
begin
  if ATag = '' then Exit;
  vColumn := ATag;
  vType := SQLTypeFromFieldKind(AKind, ASize);
  if vType = '' then
    Exit;

  vColIdx := FCurTableColumns.IndexOf(vColumn);
  if vColIdx < 0 then
  begin
    vAlterText := Format('ALTER TABLE [%s] ADD [%s] %s', [FCurTableName, vColumn, vType]);
    FConnection.ExecSQL(vAlterText);
    FCurTableColumns.Add(vColumn);
    FCurTableTypes.Add(vType);
  end
  else begin
    if (FCurTableTypes[vColIdx] <> vType) then
    begin
      // no alter column - replace column
      vAlterText := Format('ALTER TABLE [%s] DROP [%s]', [ FCurTableName, vColumn ]);
      FConnection.ExecSQL(vAlterText);
      vAlterText := Format('ALTER TABLE [%s] ADD [%s] %s', [ FCurTableName, vColumn, vType ]);
      FConnection.ExecSQL(vAlterText);
      FCurTableTypes[vColIdx] := vType;
    end;
  end;
end;

function TFireDACStorage.SQLTypeFromFieldKind(const AKind: uConsts.TFieldKind; const ASize: Integer): string;
begin
  case AKind of
    fkInteger: Result := 'INTEGER';
    fkString: Result := 'TEXT';
    fkFloat: Result := 'REAL';
    fkDateTime: Result := 'REAL';
    fkBoolean: Result := 'INTEGER';
    fkCurrency: Result := 'REAL';
    fkList: Result := 'NULL';
    fkBlob, fkComplex: Result := 'BLOB';
  else
    Result := 'INTEGER';
  end;
end;

procedure TFireDACStorage.DoActivate(const ATag: string);
var
  vContext: TFireDACContext;
begin
  // Здесь нужно запомнить имя таблицы и создать контекст
  vContext := TFireDACContext.Create(Self, ATag);
  Push(vContext);
end;

procedure TFireDACStorage.DoDeactivate;
begin
  PopContext;
end;

procedure TFireDACStorage.Pop;
begin
  FGroupList.Delete(FGroupList.Count - 1);
end;

procedure TFireDACStorage.PopContext;
var
  vIndex: Integer;
begin
  vIndex := FGroupList.Count - 1;
  TFireDACContext(FGroupList[vIndex]).Free;
  FGroupList.Delete(FGroupList.Count - 1);
end;

procedure TFireDACStorage.Push(const AObject: TObject);
begin
  FGroupList.Add(Pointer(AObject));
end;

procedure TFireDACStorage.DoReadGroup(const ATag: string; const AReadFunc: TStorageFunc);
var
  vDataset: TDataset;
begin
  try
    FConnection.ExecSQL(Format('select * from [%s]', [ATag]), vDataset);
  except
    Exit;
  end;
  if not Assigned(AReadFunc) then
  begin
    FreeAndNil(vDataset);
    Exit;
  end;
  Push(vDataset);
  vDataset.First;
  while not vDataset.Eof do
  begin
    AReadFunc(Self);
    vDataset.Next;
  end;
  Pop;
end;

procedure TFireDACStorage.DoWriteItem(const AWriteFunc: TStorageFunc;
  const AID, ALogID: Integer; const ASaveAction: TEntitySaveAction);
begin
  DoWriteValue('id', fkInteger, AID, True);
  DoWriteValue('log_id', fkInteger, ALogID, False);
  if Assigned(AWriteFunc) then
    AWriteFunc(Self);
  GetActiveContext.Commit(FConnection, ASaveAction);
end;

procedure TFireDACStorage.DoWriteValue(const ATag: string; const AFieldKind: TFieldKind;
  const AValue: Variant; const AIsKey: Boolean = False);
begin
  GetActiveContext.AddParameter(ATag, AValue, AFieldKind, AIsKey);
end;

function TFireDACStorage.GetActiveContext: TFireDACContext;
begin
  Result := TFireDACContext(FGroupList[FGroupList.Count - 1]);
end;

function TFireDACStorage.DoReadValue(const ATag: string; const AFieldKind: uConsts.TFieldKind): Variant;
var
  vDataset: TDataSet;
begin
  vDataset := GetActiveDataset;
  Result := vDataset.FieldByName(ATag).AsVariant;
end;

function TFireDACStorage.GetActiveDataset: TDataset;
begin
  Result := TDataset(FGroupList[FGroupList.Count - 1]);
end;

function TFireDACStorage.DoCreateIDs(const ATag: string; const ACount: Integer): Integer;
var
  vMaxID: Integer;
  vTableName: string;
  vDataset: TDataset;
begin
  vTableName := ATag;
  vMaxID := GetMaxTableID(vTableName);

  try
    FConnection.ExecSQL(Format('select [last_id] from [numerators] where [table_name] = ''%s''', [vTableName]), vDataset);
  except
    if vMaxID > 0 then
      Result := vMaxID + ACount
    else
      Result := ACount;
    FConnection.ExecSQL(Format('insert into [numerators] ([id], [table_name], [last_id], [last_code]) values (%d, ''%s'', %d, %d)',
      [GetMaxTableID('numerators') + 1, vTableName, Result, 0]));
    Exit;
  end;

  try
    vDataset.First;
    Result := vDataset.FieldByName('last_id').AsInteger;
    if vMaxID > Result then
      Result := vMaxID + ACount
    else
      Result := Result + ACount;

    FConnection.ExecSQL(Format('update [numerators] set [last_id] = %d where [table_name] = ''%s''', [Result, vTableName]));
  finally
    FreeAndNil(vDataset);
  end;
end;

function TFireDACStorage.GetMaxTableID(const ATableName: string): Integer;
var
  v: Variant;
  vDataset: TDataset;
begin
  Result := 0;
  try
    FConnection.ExecSQL(Format('select max([id]) as [m] from [%s]', [ATableName]), vDataset);
  except
    Exit;
  end;

  try
    vDataset.First;
    v := vDataset.FieldByName('m').AsVariant;
    if VarIsNull(v) then Exit;
    Result := v;
  finally
    FreeAndNil(vDataset);
  end;
end;

function TFireDACStorage.DoCreateCodes(const ATag: string; const ACount: Integer): Integer;
var
  vTableName: string;
  vDataset: TDataset;
begin
  vTableName := ATag;
  try
    FConnection.ExecSQL(Format('select [last_code] from [numerators] where [table_name] = ''%s''', [vTableName]), vDataset);
  except
    Result := 1;
    FConnection.ExecSQL('insert into [numerators] ([id], [table_name], [last_code], [last_id]) values (%d, ''%s'', %d, %d)',
      [GetMaxTableID('numerators') + 1, vTableName, Result, 0]);
    exit;
  end;
  try
    vDataset.First;
    Result := vDataset.FieldByName('last_code').AsInteger + 1;
    FConnection.ExecSQL(Format('update [numerators] set [last_code] = %d where [table_name] = ''%s''', [Result, vTableName]));
  finally
    FreeAndNil(vDataset);
  end;
end;

procedure TFireDACStorage.DoRebuild;
begin
  try
    Disconnect;
    Sleep(1000);
  finally
    Connect;
  end;
end;

function TFireDACStorage.DoGetLastLogID: Integer;
begin
  Result := 0;
  try
    Result := VarToInt(FConnection.ExecSQLScalar('select max([id]) as [id] from [sys_log]'), -1);
  except
  end;
end;

procedure TFireDACStorage.UpdateNumerator(const ATag: string; const ALastID: Integer);
var
  vLastStoredID: Integer;
  vDataset: TDataset;
begin
  if ALastID < 0 then
    Exit;
  try
    FConnection.ExecSQL(Format('select [last_id] from [numerators] where [table_name] = ''%s''', [ATag]), vDataset);
  except
    FConnection.ExecSQL(Format('insert into [numerators] ([id], [table_name], [last_id], [last_code]) values (%d, ''%s'', %d, %d)',
      [0, ATag, ALastID, 0]));
    exit;
  end;
  try
    vDataset.First;
    vLastStoredID := vDataset.FieldByName('last_id').AsInteger;
    if ALastID > vLastStoredID then
      FConnection.ExecSQL(Format('update [numerators] set [last_id] = %d where [table_name] = ''%s''', [ALastID, ATag]));
  finally
    FreeAndNil(vDataset);
  end;
end;

procedure TFireDACStorage.DoRollbackTransaction(const ATransactionID: Integer);
begin
  FConnection.ExecSQL('rollback  transaction');
  FTransCount := Max(0, FTransCount - 1);
end;

function TFireDACStorage.DoBeginTransaction: Integer;
begin
  FConnection.ExecSQL('begin transaction');
  Inc(FTransCount);
  Result := FTransCount;
end;

procedure TFireDACStorage.DoCommitTransaction(const ATransactionID: Integer);
begin
  FConnection.ExecSQL('commit transaction');
  FTransCount := Max(0, FTransCount - 1);
end;

initialization

TBaseModule.RegisterModule('Storage', '', 'FireDAC', TFireDACStorage);

end.
