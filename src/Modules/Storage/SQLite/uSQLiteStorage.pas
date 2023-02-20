{---------------------------------------------------------------------------------
  X-Tend runtime

  Contributors:
    Vladimir Kustikov (kustikov@sensoft.pro)
    Sergey Arlamenkov (arlamenkov@sensoft.pro)
    Yurii Vyrovschikov (uri.3000@ya.ru)

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

unit uSQLiteStorage;

interface

uses
  Classes, Generics.Collections, System.Sqlite, uStorage, uJSON, uParameters, uConsts;

type
  TSQLiteRecord = class
  private
    FItems: array of Variant;
    function GetCount: Integer;
    function GetValue(Idx: Integer): Variant;
    procedure SetCount(const Value: Integer);
    procedure SetValue(Idx: Integer; const Value: Variant);
  public
    property Count: Integer read GetCount write SetCount;
    property Value[Idx: Integer]: Variant read GetValue write SetValue; default;
  end;

  TSQLiteRecords = TObjectList<TSQLiteRecord>;

  TSQLiteResult = class
  private
    FRecords: TSQLiteRecords;
    FColumns: TStrings;
    FCurrentRow: Integer;
    function GetValue(Row: Integer; Column: string): Variant;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Empty: Boolean;

    property Columns: TStrings read FColumns;
    property Records: TSQLiteRecords read FRecords;
    property Values[ Row: Integer; Column: string ]: Variant read GetValue; default;
    property CurrentRow: Integer read FCurrentRow write FCurrentRow;
  end;

  TSQLite = class
  private
    FDBFileName: string;
    FDB: sqlite3;
    FSQLiteResult: TSQLiteResult;
  public
    constructor Create(const ADBFileName: string);
    destructor Destroy; override;

    function Open: Boolean;
    procedure Close;
    function DBQuery(Sql: string; Params: array of const): Boolean;
    function ErrorMessage: string;

    property SQLiteResult: TSQLiteResult read FSQLiteResult;
  end;

  TVarRecArray = array of TVarRec;

  TSQLiteDBContext = class
  private
    function BuildInsertQuery: string;
    function BuildUpdateQuery: string;
    function BuildDeleteQuery: string;
    function GetParamValues: TVarRecArray;
    function GetKeyParamValues: TVarRecArray;
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
    procedure AddBlobParameter(const AName: string; const AValue: TStream); virtual;

    procedure Commit(const ASQLite: TSQLite; const ASaveAction: TEntitySaveAction);
  end;

  TSQLiteStorage = class(TStorage)
  protected
    FGroupList: TList;
    FSQLite: TSQLite;
    FTransCount: Integer;
    FCurTableName: string;
    FCurTableColumns: TStrings;
    FCurTableTypes: TStrings;
    FDBFileName: string;

    function GetActiveContext: TSQLiteDBContext;
    function GetActiveDataset: TSQLiteResult;
    procedure Push(const AObject: TObject);
    procedure Pop;
    procedure PopContext; virtual;
    function GetMaxTableID(const ATableName: string): Integer;
    function SearchTable(const AName: string): Boolean;
    function ExecuteDBQuery(Sql: string; Params: array of const): Boolean;
    function SQLTypeFromFieldKind(const AKind: TFieldKind; const ASize: Integer): string;

    procedure DoWriteValue(const ATag: string; const AFieldKind: TFieldKind; const AValue: Variant;
      const AIsKey: Boolean = False); override;
    function DoReadValue(const ATag: string; const AFieldKind: TFieldKind): Variant; override;
    procedure DoWriteStream(const ATag: string; const AStream: TStream); override;
    function DoReadStream(const ATag: string): TStream; override;
    procedure DoReadGroup(const ATag: string; const AReadFunc: TStorageFunc); override;

    function DoSyncGroupDef(const ATag: string; const ASyncFunc:
      TStorageFunc): Boolean; override;

    procedure DoWriteItem(const AWriteFunc: TStorageFunc;
      const AID, ALogID: Integer; const ASaveAction: TEntitySaveAction); override;
    procedure DoSyncItemDef(const ATag: string; const AKind: TFieldKind;
      const ASize: Integer); override;
    function DoGetVersion: string; override;
    procedure DoSetVersion(const Value: string); override;

    function DoCreateIDs(const ATag: string; const ACount: Integer): Integer; override;
    function DoCreateCodes(const ATag: string; const ACount: Integer = 1): Integer; override;
    function DoGetLastLogID: Integer; override;
    function DoGetChanges(const ALogID: Integer): TJSONObject; override;

    procedure DoActivate(const ATag: string); override;
    procedure DoDeactivate; override;

    procedure DoConnect; override;
    procedure DoDisconnect; override;
    procedure DoRebuild; override;

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
  Variants, SysUtils, StrUtils, Math, IOUtils, uModule, uDomain, uDomainUtils, uSettings;

procedure EmptyDestructor(user: pointer); cdecl;
begin
end;

{ TSQLiteResult }

procedure TSQLiteResult.Clear;
begin
  FColumns.Clear;
  FRecords.Clear;
  FCurrentRow := 0;
end;

constructor TSQLiteResult.Create;
begin
  FColumns := TStringList.Create;
  FRecords := TSQLiteRecords.Create;
end;

destructor TSQLiteResult.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FRecords);
  inherited;
end;

function TSQLiteResult.Empty: Boolean;
begin
  Result := FColumns.Count = 0;
end;

function TSQLiteResult.GetValue(Row: Integer; Column: string): Variant;
var
  ColIdx: Integer;
  Rec: TSQLiteRecord;
begin
  Result := Null;
  if Empty then Exit;
  if (Row < 0) or (Row >= FRecords.Count) then Exit;
  if Column = '' then Exit;
  ColIdx := FColumns.IndexOf(Column);
  if ColIdx < 0 then Exit;
  Rec := FRecords[ Row ];
  Result := Rec[ ColIdx ];
end;

{ TSQLite }

procedure TSQLite.Close;
begin
  sqlite3_close(FDB);
end;

constructor TSQLite.Create(const ADBFileName: string);
begin
  FSQLiteResult := TSQLiteResult.Create;
  FDBFileName := ADBFileName;
end;

function TSQLite.DBQuery(Sql: string; Params: array of const): Boolean;
var
  res, I, BindIdx, IntVal, Col, ColType, ColCount: Integer;
  Int64Val: Int64;
  FloatVal: Double;
  stm: sqlite3_stmt;
  tail, ColName: PAnsiChar;
  asql: AnsiString;
  s: WideString;
  v: Variant;
  vt: TVarType;
  Rec: TSQLiteRecord;
  ArgType, ArrLen: Integer;
  ResData, ArrData: PByte;
  Obj: TObject;
  Stream: TStream;
  Len: Integer;
  BlobData: array of Byte;
begin
  Result := False;
  FSQLiteResult.Clear;

  if (FDB = nil) or (Sql = '') then
    Exit;

  asql := AnsiString(sql);
  res := sqlite3_prepare(FDB, PAnsiChar(asql), Length(asql), stm, tail);
  if res <> SQLITE_OK then
  begin
    if tail <> '' then
      sqlite3_free(tail);
    Exit;
  end;
  try
    // связываем значения параметров
    for I := 0 to High(Params) do
    begin
      BindIdx := I + 1;
      s := '';
      with TVarRec(Params[I]) do
      begin
        ArgType := VType;
        case ArgType of
          vtBoolean,
          vtInteger:        sqlite3_bind_int(stm, BindIdx, VInteger);
          vtExtended,
          vtCurrency:       sqlite3_bind_double(stm, BindIdx, VExtended^);
          vtInt64:          sqlite3_bind_int64(stm, BindIdx, vtInt64);

          vtString:         s := WideString(VString);
          vtPChar:          s := WideString(VPChar);
          vtPWideChar:      s := WideString(VPWideChar);
          vtAnsiString:     s := WideString(PAnsiChar(VAnsiString));
          vtWideString:     s := WideString(PWideChar(VWideString));
          vtUnicodeString:  s := WideString(PWideChar(VUnicodeString));

          vtObject:
            begin
              Obj := VObject;
              if (Obj <> nil) and (Obj.InheritsFrom(TStream)) then
              begin
                Stream := TStream(Obj);
                Len := Stream.Size;
                SetLength(BlobData, Len);
                Stream.Position := 0;
                Stream.Read(BlobData[0], Len);
                sqlite3_bind_blob(stm, BindIdx, @BlobData[0], Len, nil);
              end
              else
                sqlite3_bind_null(stm, BindIdx);
            end;

          vtVariant:
            begin
              v := VVariant^;
              vt := VarType(v);
              case vt of
                varEmpty,
                varNull:
                  begin
                    sqlite3_bind_null(stm, BindIdx);
                  end;

                varSingle,
                varDouble,
                varCurrency,
                varDate:
                  begin
                    FloatVal := v;
                    sqlite3_bind_double(stm, BindIdx, FloatVal);
                  end;

                varSmallInt,
                varInteger,
                varBoolean,
                varShortInt,
                varByte,
                varWord,
                varUInt32:
                  begin
                    IntVal := v;
                    sqlite3_bind_int(stm, BindIdx, IntVal);
                  end;

                varInt64:
                  begin
                    Int64Val := v;
                    sqlite3_bind_int64(stm, BindIdx, Int64Val);
                  end;

                varOleStr,
                varUString,
                varString:   s := VarToStr(v);
              else
              end;
            end;
        else
          // vtChar, vtPointer, vtObject, vtClass, vtCurrency, vtInterface, vtWideChar
        end;
      end;
      // строковые данные
      if s <> '' then
      begin
        sqlite3_bind_text16(stm, BindIdx, PWideChar(s), ByteLength(s), EmptyDestructor);
      end;
    end;

    // выполняем
    res := sqlite3_step(stm);

    // insert, update, delete or empty select Result
    if res = SQLITE_DONE then
    begin
      Result := True;
      Exit;
    end;

    // select Result

    // columns
    if res = SQLITE_ROW then
    begin
      ColCount := sqlite3_column_count(stm);
      for Col := 0 to ColCount - 1 do
      begin
        ColName := sqlite3_column_name(stm, Col);
        FSQLiteResult.Columns.Add(string(AnsiString(ColName)));
      end;

      // data
      while res = SQLITE_ROW do
      begin
        Rec := TSQLiteRecord.Create;
        Rec.Count := ColCount;
        for Col := 0 to ColCount - 1 do
        begin
          ColType := sqlite3_column_type(stm, Col);
          case ColType of
            SQLITE_INTEGER: Rec[ Col ] := sqlite3_column_int(stm, col);
            SQLITE_FLOAT:   Rec[ Col ] := sqlite3_column_double(stm, col);
            SQLITE_TEXT:    Rec[ Col ] := WideString(sqlite3_column_text16(stm, col));
            SQLITE_NULL:    Rec[ Col ] := Null;
            SQLITE_BLOB:    // Rec[ Col ] := Null;
              begin
                ArrLen := sqlite3_column_bytes(stm, Col);
                v := VarArrayCreate([0, ArrLen - 1], varByte);
                ArrData := VarArrayLock(v);
                ResData := sqlite3_column_blob(stm, col);
                Move(ResData^, ArrData^, ArrLen);
                VarArrayUnlock(v);
                Rec[ Col ] := v;
              end;
          else
          end;
        end;
        FSQLiteResult.Records.Add(Rec);
        res := sqlite3_step(stm);
      end;
    end;
  finally
    sqlite3_finalize(stm);
  end;
  Result := True;
end;

destructor TSQLite.Destroy;
begin
  FSQLiteResult.Free;
  inherited;
end;

function TSQLite.ErrorMessage: string;
begin
  Result := IntToStr(sqlite3_errcode(FDB));
end;

function TSQLite.Open: Boolean;
var
  s: AnsiString;
begin
  s := AnsiString(FDBFileName);
  Result := sqlite3_open(PAnsiChar(s), FDB) = SQLITE_OK;
end;

{ TSQLiteRecord }

function TSQLiteRecord.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TSQLiteRecord.GetValue(Idx: Integer): Variant;
begin
  Result := Null;
  if (Idx < 0) or (Idx >= Count) then Exit;
  Result := FItems[ Idx ];
end;

procedure TSQLiteRecord.SetCount(const Value: Integer);
begin
  if Value < 0 then Exit;
  SetLength(FItems, Value);
end;

procedure TSQLiteRecord.SetValue(Idx: Integer; const Value: Variant);
begin
  if (Idx < 0) or (Idx >= Count) then Exit;
  FItems[ Idx ] := Value;
end;

{ TSQLiteDBContext }

procedure TSQLiteDBContext.AddBlobParameter(const AName: string; const AValue: TStream);
var
  vParameter: TBlobParameter;
begin
  vParameter := TBlobParameter.Create(AName, AValue);
  FParameters.Add(vParameter);
end;

procedure TSQLiteDBContext.AddParameter(const AName: string; const AValue: Variant; const AFieldKind: TFieldKind;
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

function TSQLiteDBContext.BuildDeleteQuery: string;
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
      vWhereStr := '[' + vParamName + '] = ?';
      vWasFirst := True;
    end
    else
      vWhereStr := vWhereStr + ' and [' + vParamName + '] = ?';
  end;

  if Length(vWhereStr) = 0 then
    Result := ''
  else
    Result := Format('delete from [%s] where %s', [FTableName, vWhereStr]);
end;

function TSQLiteDBContext.BuildInsertQuery: string;
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
    vParamStr := vParamStr + '?';
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
    vParamStr := vParamStr + '?';
  end;

  Result := Format('insert into [%s] (%s) values (%s)', [
    FTableName, vVarStr, vParamStr]);
end;

function TSQLiteDBContext.BuildUpdateQuery: string;
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
      vSetStr := '[' + vParamName + '] = ?';
      vWasFirst := True;
    end
    else
      vSetStr := vSetStr + ', [' + vParamName + '] = ?';
  end;

  vWasFirst := False;
  for i := 0 to FKeyParameters.Count - 1 do
  begin
    vParamName := TSimpleParameter(FKeyParameters[i]).Name;
    if not vWasFirst then
    begin
      vWhereStr := '[' + vParamName + '] = ?';
      vWasFirst := True;
    end
    else
      vWhereStr := vWhereStr + ' and [' + vParamName + '] = ?';
  end;

  if (Length(vSetStr) = 0) or (Length(vWhereStr) = 0) then
    Result := ''
  else
    Result := Format('update [%s] set %s where %s', [FTableName, vSetStr, vWhereStr]);
end;

procedure TSQLiteDBContext.Commit(const ASQLite: TSQLite; const ASaveAction: TEntitySaveAction);
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
    ASQLite.DBQuery(vSQLText, GetKeyParamValues)
  else
    ASQLite.DBQuery(vSQLText, GetParamValues);
end;

constructor TSQLiteDBContext.Create(const AStorage: TStorage; const ATableName: string);
begin
  inherited Create;

  FStorage := AStorage;
  FTableName := ATableName;
  FParameters := TList<TBaseParameter>.Create;
  FKeyParameters := TList<TBaseParameter>.Create;
end;

destructor TSQLiteDBContext.Destroy;
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

function TSQLiteDBContext.GetKeyParamValues: TVarRecArray;
var
  i: Integer;
  vSimpleParameter: TSimpleParameter;
begin
  SetLength(Result, FKeyParameters.Count);
  for i := 0 to FKeyParameters.Count - 1 do
  begin
    vSimpleParameter := TSimpleParameter(FKeyParameters[i]);
    Result[i].VType := vtVariant;
    Result[i].VVariant := @vSimpleParameter.Value;
  end;
end;

function TSQLiteDBContext.GetParamValues: TVarRecArray;
var
  vLen, vIdx, i: Integer;
  vParam: TBaseParameter;
  vSimpleParameter: TSimpleParameter;
  vBlobParameter: TBlobParameter;
begin
  vLen := FKeyParameters.Count + FParameters.Count;
  SetLength(Result, vLen);
  vIdx := 0;
  for i := 0 to FParameters.Count - 1 do
  begin
    vParam := FParameters[i];
    if vParam.ParamKind = pkSimple then
    begin
      vSimpleParameter := TSimpleParameter(FParameters[i]);
      Result[vIdx].VType := vtVariant;
      Result[vIdx].VVariant := @vSimpleParameter.Value;
    end else
    if vParam.ParamKind = pkBlob then
    begin
      vBlobParameter := TBlobParameter(FParameters[i]);
      Result[vIdx].VType := vtObject;
      Result[vIdx].VObject := vBlobParameter.Value;
    end;
    Inc(vIdx);
  end;
  for i := 0 to FKeyParameters.Count - 1 do
  begin
    vSimpleParameter := TSimpleParameter(FKeyParameters[i]);
    Result[vIdx].VType := vtVariant;
    Result[vIdx].VVariant := @vSimpleParameter.Value;
    Inc(vIdx);
  end;
end;

{ TSQLiteStorage }

constructor TSQLiteStorage.Create(const ADomain: TObject; const AName: string);
var
  vSettings: TSettings;
  vSectionName: string;
  vDatabase: string;
  vDirectory: string;
begin
  inherited Create(ADomain, AName);
  FGroupList := TList.Create;
  FCurTableColumns := TStringList.Create;
  FCurTableTypes := TStringList.Create;

  vSettings := TDomain(ADomain).Settings;
  vSectionName := AName;
  vDatabase := vSettings.GetValue(vSectionName, 'Database', 'empty.db');
  if TPath.IsRelativePath(ExtractFilePath(vDatabase)) then
    vDatabase := TPath.Combine(TDomain(ADomain).Configuration.ConfigurationDir, vDatabase);

  vDirectory := ExtractFileDir(vDatabase);
  if not TDirectory.Exists(vDirectory) then
    ForceDirectories(vDirectory);

  FDBFileName := vDatabase;
  FSQLite := TSQLite.Create(FDBFileName);

  if not vSettings.SectionExists(AName) then
  begin
    vSettings.SetValue(AName, 'Database', vDatabase);
  end;

  FLogger.AddMessage(FDBFileName);
end;

destructor TSQLiteStorage.Destroy;
begin
  Disconnect;
  FGroupList.Free;
  FSQLite.Free;
  FCurTableColumns.Free;
  FCurTableTypes.Free;
  inherited Destroy;
end;

procedure TSQLiteStorage.DoActivate(const ATag: string);
var
  vContext: TSQLiteDBContext;
begin
  // Здесь нужно запомнить имя таблицы и создать контекст
  vContext := TSQLiteDBContext.Create(Self, ATag);
  Push(vContext);
end;

procedure TSQLiteStorage.DoCommitTransaction(const ATransactionID: Integer);
begin
  ExecuteDBQuery('commit transaction', []);
  FTransCount := Max(0, FTransCount - 1);
end;

procedure TSQLiteStorage.DoConnect;
begin
  try
    FSQLite.Open;
  except
    on E: Exception do
    begin
      FLogger.AddMessage('Error: ' + E.Message);
      raise;
    end;
  end;
end;

function TSQLiteStorage.DoCreateCodes(const ATag: string; const ACount: Integer): Integer;
var
  vTableName: string;
begin
  vTableName := ATag;
  ExecuteDBQuery('select [last_code] from [numerators] where [table_name] = ?', [vTableName]);

  if FSQLite.SQLiteResult.Empty then
  begin
    Result := 1;
    ExecuteDBQuery('insert into [numerators] ([id], [table_name], [last_code], [last_id]) values (?, ?, ?, ?)',
      [GetMaxTableID('numerators') + 1, vTableName, Result, 0]);
  end
  else begin
    Result := FSQLite.SQLiteResult[0, 'last_code'] + 1;
    ExecuteDBQuery('update [numerators] set [last_code] = ? where [table_name] = ?', [Result, vTableName]);
  end;
end;

function TSQLiteStorage.DoCreateIDs(const ATag: string; const ACount: Integer): Integer;
var
  vMaxID: Integer;
  vTableName: string;
begin
  vTableName := ATag;
  vMaxID := GetMaxTableID(vTableName);

  ExecuteDBQuery('select [last_id] from [numerators] where [table_name] = ?', [vTableName]);
  if FSQLite.SQLiteResult.Empty then
  begin
    if vMaxID > 0 then
      Result := vMaxID + ACount
    else
      Result := ACount;
    ExecuteDBQuery('insert into [numerators] ([id], [table_name], [last_id], [last_code]) values (?, ?, ?, ?)',
      [GetMaxTableID('numerators') + 1, vTableName, Result, 0]);
  end
  else begin
    Result := FSQLite.SQLiteResult[0, 'last_id'];
    if vMaxID > Result then
      Result := vMaxID + ACount
    else
      Result := Result + ACount;

    ExecuteDBQuery('update [numerators] set [last_id] = ? where [table_name] = ?', [Result, vTableName]);
  end;
end;

procedure TSQLiteStorage.DoDeactivate;
begin
  PopContext;
end;

procedure TSQLiteStorage.DoDisconnect;
begin
  FSQLite.Close;
end;

function TSQLiteStorage.DoGetChanges(const ALogID: Integer): TJSONObject;
var
  vCollectionName: string;
  vCurrentLogID: Integer;
  vUpdates: TJSONArray;
  vUpdate: TJSONObject;
  vActions: TJSONArray;
  vAction: TJSONObject;
  vLastLogID: Integer;
  vRow: Integer;
  vRes: TSQLiteResult;

// Формат файла JSON - обновление
// {
//   "last_log_id": 152,
//++   "id_name": "Point 1",
//++   "cfg_version": "1.0.0.0",
//++   "packet_type": "full",
//   "updates": [
//     { "log_id": 151,
//       "user_name": "admin",
//     "actions": [
//         { "action_kind_id": 2,   // 1-add, 2-update, 3-delete
//           "collection": "Persons",
//           "entity_id": 42,
//           "fields":
//             {
//               "id": 1,
//               "name": "Вася",
//               ...
//             }
//         },
//         // другие действия в этом обновлении
//         {
//           ...
//         }
//       ]
//     }
//     {
//       // другие обновления
//     }
//   ]
// }
begin
  vCurrentLogID := GetLastLogID;
  if vCurrentLogID = ALogID then
  begin
    Result := nil;
    Exit;
  end;

  Result := TJSONObject.Create;

  ExecuteDBQuery('select la.sys_log_id, l.user_name, l.log_time, ' +
    'la.id, la.action_kind_id, la.collection, la.entity_id, la.json ' +
    'from sys_log l inner join sys_log_actions la on la.sys_log_id = l.id ' +
    'where l.id > ? order by l.id, la.action_kind_id, la.collection', [ALogID]);

  try
    Result.AddPair('last_log_id', TJSONNumber.Create(vCurrentLogID));
    vUpdates := TJSONArray.Create;
    Result.AddPair('updates', vUpdates);
    vLastLogID := -1;
    vActions := nil;

    vRes := FSQLite.SQLiteResult;
    for vRow := 0 to vRes.Records.Count - 1 do
    begin
      vCollectionName := vRes[ vRow, 'collection'];
      vCurrentLogID := vRes[ vRow, 'sys_log_id'];
      if vLastLogID < vCurrentLogID then
      begin
        vLastLogID := vCurrentLogID;
        vUpdate := TJSONObject.Create;
        vUpdate.AddPair('log_id', TJSONNumber.Create(vCurrentLogID));
        vUpdate.AddPair('user_name', vRes[ vRow, 'user_name']);
        vUpdate.AddPair('log_time', FormatDateTime('dd.mm.yy hh:nn:ss', vRes[ vRow, 'log_time']));
        vActions := TJSONArray.Create;
        vUpdate.AddPair('actions', vActions);
        vUpdates.AddElement(vUpdate);
      end;

      vAction := TJSONObject.Create;
      vAction.AddPair('change_id', TJSONNumber.Create(vRes[vRow, 'id']));
      vAction.AddPair('action_kind_id', TJSONNumber.Create(vRes[vRow, 'action_kind_id']));
      vAction.AddPair('collection', vCollectionName);
      vAction.AddPair('entity_id', TJSONNumber.Create(vRes[vRow, 'entity_id']));
      vAction.AddPair('fields', TJSONObject.ParseJSONValue(vRes[vRow, 'json']));
      vActions.AddElement(vAction);
    end;
  finally
  end;
end;

function TSQLiteStorage.DoGetLastLogID: Integer;
begin
  Result := 0;
  if ExecuteDBQuery('select max([id]) as [id] from [sys_log]', []) then
    Result := VarToInt(FSQLIte.SQLiteResult[0, 'id'], -1);
end;

function TSQLiteStorage.DoGetVersion: string;
begin
  try
    if ExecuteDBQuery('select [version] from [sys_constants]', []) then
      Result := VarToStr(FSQLIte.SQLiteResult[0, 'version']);
  except
    Result := '';
  end;
end;

// Добавить сюда QueryObject
procedure TSQLiteStorage.DoReadGroup(const ATag: string; const AReadFunc: TStorageFunc);
var
  i: Integer;
  vRes: TSQLiteResult;
begin
  ExecuteDBQuery(Format('select * from [%s]', [ATag]), []);
  if FSQLite.SQLiteResult.Empty then Exit;
  if not Assigned(AReadFunc) then Exit;

  vRes := FSQLite.SQLiteResult;
  Push(vRes);
  try
    for i := 0 to vRes.Records.Count - 1 do
    begin
      vRes.CurrentRow := i;
      AReadFunc(Self);
    end;
  finally
    Pop;
  end;
end;

function TSQLiteStorage.DoReadStream(const ATag: string): TStream;
var
  v: Variant;
  vData: Pointer;
  vSize: Integer;
  vRes: TSQLiteResult;
begin
  Result := nil;
  vRes := GetActiveDataset;
  if vRes = nil then Exit;

  v := vRes[vRes.CurrentRow, ATag];
  if VarIsNull(v) then
    Exit;

  vData := VarArrayLock(v);
  vSize := VarArrayHighBound(v, 1) - VarArrayLowBound(v, 1) + 1;
  Result := TMemoryStream.Create;
  if vSize > 0 then
    Result.Write(vData^, vSize);
  VarArrayUnlock(v);
end;

function TSQLiteStorage.DoReadValue(const ATag: string; const AFieldKind: TFieldKind): Variant;
var
  vRes: TSQLiteResult;
begin
  vRes := GetActiveDataset;
  Result := vRes[vRes.CurrentRow, ATag];
end;

procedure TSQLiteStorage.DoRebuild;
begin
  try
    Disconnect;
    Sleep(1000);
  finally
    Connect;
  end;
end;

procedure TSQLiteStorage.DoRollbackTransaction(const ATransactionID: Integer);
begin
  ExecuteDBQuery('rollback  transaction', []);
  FTransCount := Max(0, FTransCount - 1);
end;

function TSQLiteStorage.DoBeginTransaction: Integer;
begin
  ExecuteDBQuery('begin transaction', []);
  Inc(FTransCount);
  Result := FTransCount;
end;

procedure TSQLiteStorage.DoSetVersion(const Value: string);
begin
  try
    ExecuteDBQuery('update [sys_constants] set [version] = ?', [Value]);  // todo: set version
  except
  end;
end;

function TSQLiteStorage.DoSyncGroupDef(const ATag: string;
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
      ExecuteDBQuery(vAlterText, []);

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
      ExecuteDBQuery(Format('ALTER TABLE [%s] DROP [%s]', [FCurTableName, vColName]), []);
      FCurTableColumns.Delete(i);
      FCurTableTypes.Delete(i);
    end;
  end;
end;

procedure TSQLiteStorage.DoSyncItemDef(const ATag: string;
  const AKind: TFieldKind; const ASize: Integer);
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
    ExecuteDBQuery(vAlterText, []);
    FCurTableColumns.Add(vColumn);
    FCurTableTypes.Add(vType);
  end
  else begin
    if (FCurTableTypes[vColIdx] <> vType) then
    begin
      // no alter column - replace column
      vAlterText := Format('ALTER TABLE [%s] DROP [%s]', [ FCurTableName, vColumn ]);
      ExecuteDBQuery(vAlterText, []);
      vAlterText := Format('ALTER TABLE [%s] ADD [%s] %s', [ FCurTableName, vColumn, vType ]);
      ExecuteDBQuery(vAlterText, []);
      FCurTableTypes[vColIdx] := vType;
    end;
  end;
end;

procedure TSQLiteStorage.DoWriteItem(const AWriteFunc: TStorageFunc;
  const AID, ALogID: Integer; const ASaveAction: TEntitySaveAction);
begin
  DoWriteValue('id', fkInteger, AID, True);
  DoWriteValue('log_id', fkInteger, ALogID, False);
  if Assigned(AWriteFunc) then
    AWriteFunc(Self);
  GetActiveContext.Commit(FSQLite, ASaveAction);
end;

procedure TSQLiteStorage.DoWriteStream(const ATag: string; const AStream: TStream);
begin
  GetActiveContext.AddBlobParameter(ATag, AStream);
end;

procedure TSQLiteStorage.DoWriteValue(const ATag: string; const AFieldKind: TFieldKind;
  const AValue: Variant; const AIsKey: Boolean = False);
begin
  GetActiveContext.AddParameter(ATag, AValue, AFieldKind, AIsKey);
end;

function TSQLiteStorage.ExecuteDBQuery(Sql: string; Params: array of const): Boolean;
begin
  Result := FSQLite.DBQuery(Sql, Params);
  if not Result then
    TDomain(FDomain).Logger.AddMessage('DB ERROR: ' + Sql + ', Code: ' + FSQLite.ErrorMessage);
end;

function TSQLiteStorage.GetActiveContext: TSQLiteDBContext;
begin
  Result := TSQLiteDBContext(FGroupList[FGroupList.Count - 1]);
end;

function TSQLiteStorage.GetActiveDataset: TSQLiteResult;
begin
  Result := TSQLiteResult(FGroupList[FGroupList.Count - 1]);
end;

function TSQLiteStorage.GetMaxTableID(const ATableName: string): Integer;
var
  v: Variant;
begin
  Result := 0;
  if not ExecuteDBQuery(Format('select max([id]) as [m] from [%s]', [ATableName]), []) then Exit;
  v := FSQLite.SQLiteResult[0, 'm'];
  if VarIsNull(v) then Exit;
  Result := v;
end;

procedure TSQLiteStorage.Pop;
begin
  FGroupList.Delete(FGroupList.Count - 1);
end;

procedure TSQLiteStorage.PopContext;
var
  vIndex: Integer;
begin
  vIndex := FGroupList.Count - 1;
  TSQLiteDBContext(FGroupList[vIndex]).Free;
  FGroupList.Delete(FGroupList.Count - 1);
end;

procedure TSQLiteStorage.Push(const AObject: TObject);
begin
  FGroupList.Add(Pointer(AObject));
end;

function TSQLiteStorage.SearchTable(const AName: string): Boolean;
var
  I: Integer;
  vRes: TSQLiteResult;
begin
  Result := False;
  FCurTableName := '';
  FCurTableColumns.Clear;
  FCurTableTypes.Clear;

  if AName = '' then Exit;

  if not ExecuteDBQuery(Format('PRAGMA table_info(%s)', [AName]), []) then Exit;
  Result := FSQLite.SQLiteResult.Records.Count > 0;
  if not Result then Exit;

  FCurTableName := AName;
  vRes := FSQLite.SQLiteResult;

  for I := 0 to FSQLite.SQLiteResult.Records.Count - 1 do
  begin
    FCurTableColumns.Add(vRes[I, 'name']);
    FCurTableTypes.Add(vRes[I, 'type']);
  end;
end;

function TSQLiteStorage.SQLTypeFromFieldKind(const AKind: TFieldKind; const ASize: Integer): string;
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

procedure TSQLiteStorage.UpdateNumerator(const ATag: string; const ALastID: Integer);
var
  vLastStoredID: Integer;
begin
  if ALastID < 0 then
    Exit;

  if not ExecuteDBQuery('select [last_id] from [numerators] where [table_name] = ?', [ATag]) then
  begin
    vLastStoredID := -1;
  end else
  begin
    if FSQLite.SQLiteResult.Empty then
      vLastStoredID := -1
    else
      vLastStoredID := FSQLite.SQLiteResult[0, 'last_id'];
  end;

  if vLastStoredID < 0 then
    ExecuteDBQuery('insert into [numerators] ([id], [table_name], [last_id], [last_code]) values (?, ?, ?, ?)',
      [0, ATag, ALastID, 0])
  else if ALastID > vLastStoredID then
    ExecuteDBQuery('update [numerators] set [last_id] = ? where [table_name] = ?', [ALastID, ATag]);
end;

initialization

TBaseModule.RegisterModule('Storage', '', 'SQLite', TSQLiteStorage);

end.

