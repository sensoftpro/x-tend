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

unit uSQLiteStorage;

interface

uses
  Classes, Generics.Collections, uStorage, uJSON, uParameters, uConsts, uSQLite3;

type
  TVarRecArray = array of TVarRec;

  TSQLiteDBContext = class
  private
    FStorage: TStorage;
    FTableName: string;
    FParameters: TList<TBaseParameter>;
    FKeyParameters: TList<TBaseParameter>;
    function BuildInsertQuery: string;
    function BuildUpdateQuery: string;
    function BuildDeleteQuery: string;
    function GetParamValues: TVarRecArray;
  public
    constructor Create(const AStorage: TStorage; const ATableName: string);
    destructor Destroy; override;

    procedure AddParameter(const AName: string; const AValue: Variant;
      const AIsKey: Boolean = False);
    procedure AddBlobParameter(const AName: string; const AValue: TStream);
    procedure Commit(const ASQLite: TSQLite; const ASaveAction: TEntitySaveAction);
    procedure OpenQueryAsync(const ASQLite: TSQLite; const AFetchFunc: TFetchDataFunc);
  end;

  TSQLiteStorage = class(TStorage)
  private
    FGroupList: TList;
    FSQLite: TSQLite;
    FTransCount: Integer;
    FCurTableName: string;
    FCurTableColumns: TStrings;
    FCurTableTypes: TStrings;
    function GetActiveContext: TSQLiteDBContext;
    function GetActiveDataset: TSQLiteResult;
    procedure Push(const AObject: TObject);
    procedure Pop;
    procedure PopContext;
    function GetMaxTableID(const ATableName: string): Integer;
    function SearchTable(const AName: string): Boolean;
  protected
    FDBFileName: string;
    function SQLTypeFromFieldKind(const AKind: TFieldKind; const ASize: Integer): string;

    procedure DoWriteValue(const ATag: string; const AFieldKind: TFieldKind; const AValue: Variant;
      const AIsKey: Boolean = False); override;
    function DoReadValue(const ATag: string; const AFieldKind: TFieldKind): Variant; override;
    procedure DoWriteStream(const ATag: string; const AStream: TStream); override;
    function DoReadStream(const ATag: string): TStream; override;
    procedure DoReadGroup(const ATag: string; const AReadFunc: TStorageFunc); override;
    procedure DoReadGroupAsync(const ATag: string; const AFetchFunc: TFetchDataFunc); override;

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

{ TSQLiteDBContext }

procedure TSQLiteDBContext.AddBlobParameter(const AName: string; const AValue: TStream);
var
  vParameter: TBlobParameter;
begin
  vParameter := TBlobParameter.Create(AName, AValue);
  FParameters.Add(vParameter);
end;

procedure TSQLiteDBContext.AddParameter(const AName: string; const AValue: Variant;
  const AIsKey: Boolean = False);
var
  vParameter: TBaseParameter;
begin
  for vParameter in FParameters do
    if SameText(vParameter.Name, AName) then
      Exit;

  vParameter := TSimpleParameter.Create(AName, AValue, AIsKey);
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

function TSQLiteDBContext.GetParamValues: TVarRecArray;
var
  vLen, vIdx, i: Integer;
  vSimpleParameter: TSimpleParameter;
begin
  vLen := FKeyParameters.Count + FParameters.Count;
  SetLength(Result, vLen);
  vIdx := 0;
  for i := 0 to FParameters.Count - 1 do
  begin
    vSimpleParameter := TSimpleParameter(FParameters[i]);
    Result[vIdx].VType := vtVariant;
    Result[vIdx].VVariant := @vSimpleParameter.Value;
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

procedure TSQLiteDBContext.OpenQueryAsync(const ASQLite: TSQLite; const AFetchFunc: TFetchDataFunc);
var
  vSQLText: string;
  i, vCol, vRow, vColCount, vRowCount: Integer;
  vWhereStr: string;
  vParamName: string;
  vFields, vRows: Variant;
  vFieldDict: TDictionary<string, Integer>;
  vRec: TSQLiteRecord;
begin
  if ASQLite = nil then
    Exit;
  if not Assigned(AFetchFunc) then
    Exit;

  vWhereStr := '';
  for i := 0 to FKeyParameters.Count - 1 do
  begin
    vParamName := TSimpleParameter(FKeyParameters[i]).Name;
    if vParamName = 'id' then
      Continue;
    if i > 0 then
      vWhereStr := vWhereStr + ' and ';
    vWhereStr := vWhereStr + vParamName + ' = ?';
  end;

  if vWhereStr <> '' then
    vWhereStr := ' where ' + vWhereStr;

  vSQLText := Format('select * from [%s]' + vWhereStr, [FTableName]);

  ASQLite.DBQuery(vSQLText, []);
  if ASQLite.SQLiteResult.Empty then
    Exit;

  vFieldDict := TDictionary<string, Integer>.Create;
  try
    vColCount := ASQLite.SQLiteResult.Columns.Count;
    vFields := VarArrayCreate([0, vColCount - 1], varVariant);
    for i := 0 to vColCount - 1 do
    begin
      vFields[i] := i;
      vFieldDict.Add(ASQLite.SQLiteResult.Columns[i], i);
    end;

    vRowCount := ASQLite.SQLiteResult.Records.Count;
    vRows := VarArrayCreate([0, vColCount - 1, 0, vRowCount - 1], varVariant);

    // Выгружаем значения всех записей в массив
    for vRow := 0 to vRowCount - 1 do
    begin
      vRec := ASQLite.SQLiteResult.Records[vRow];
      for vCol := 0 to vColCount - 1 do
        vRows[vCol, vRow] := vRec[vCol];
    end;

    AFetchFunc(vFieldDict, vRows);
  finally
    vFieldDict.Free;
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
  FSQLite.DBQuery('commit transaction', []);
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
  FSQLite.DBQuery('select [last_code] from [numerators] where [table_name] = ?', [vTableName]);

  if FSQLite.SQLiteResult.Empty then
  begin
    Result := 1;
    FSQLite.DBQuery('insert into [numerators] ([id], [table_name], [last_code], [last_id]) values (?, ?, ?, ?)',
      [GetMaxTableID('numerators') + 1, vTableName, Result, 0]);
  end
  else begin
    Result := FSQLite.SQLiteResult[0, 'last_code'] + 1;
    FSQLite.DBQuery('update [numerators] set [last_code] = ? where [table_name] = ?', [Result, vTableName]);
  end;
end;

function TSQLiteStorage.DoCreateIDs(const ATag: string; const ACount: Integer): Integer;
var
  vMaxID: Integer;
  vTableName: string;
begin
  vTableName := ATag;
  vMaxID := GetMaxTableID(vTableName);

  FSQLite.DBQuery('select [last_id] from [numerators] where [table_name] = ?', [vTableName]);
  if FSQLite.SQLiteResult.Empty then
  begin
    if vMaxID > 0 then
      Result := vMaxID + ACount
    else
      Result := ACount;
    FSQLite.DBQuery('insert into [numerators] ([id], [table_name], [last_id], [last_code]) values (?, ?, ?, ?)',
      [GetMaxTableID('numerators') + 1, vTableName, Result, 0]);
  end
  else begin
    Result := FSQLite.SQLiteResult[0, 'last_id'];
    if vMaxID > Result then
      Result := vMaxID + ACount
    else
      Result := Result + ACount;

    FSQLite.DBQuery('update [numerators] set [last_id] = ? where [table_name] = ?', [Result, vTableName]);
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

  FSQLite.DBQuery('select la.sys_log_id, l.user_name, l.log_time, ' +
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
  if FSQLIte.DBQuery('select max([id]) as [id] from [sys_log]', []) then
    Result := VarToInt(FSQLIte.SQLiteResult[0, 'id'], -1);
end;

function TSQLiteStorage.DoGetVersion: string;
begin
  try
    if FSQLIte.DBQuery('select [version] from [sys_constants]', []) then
      Result := VarToStr(FSQLIte.SQLiteResult[0, 'version']);
  except
    Result := '';
  end;
end;

// Добавить сюда QueryObject
procedure TSQLiteStorage.DoReadGroup(const ATag: string; const AReadFunc: TStorageFunc);
var
  I: Integer;
  vRes: TSQLiteResult;
begin
  FSQLite.DBQuery(Format('select * from [%s]', [ATag]), []);
  if FSQLite.SQLiteResult.Empty then Exit;
  if not Assigned(AReadFunc) then Exit;

  vRes := FSQLite.SQLiteResult.Clone;
  Push(vRes);
  try
    for I := 0 to vRes.Records.Count - 1 do
    begin
      vRes.CurrentRow := I;
      AReadFunc(Self);
    end;
  finally
    Pop;
  end;
end;

procedure TSQLiteStorage.DoReadGroupAsync(const ATag: string; const AFetchFunc: TFetchDataFunc);
begin
  GetActiveContext.OpenQueryAsync(FSQLite, AFetchFunc);
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

  v := vRes[0, ATag];
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
  FSQLite.DBQuery('rollback transaction', []);
  FTransCount := Max(0, FTransCount - 1);
end;

function TSQLiteStorage.DoBeginTransaction: Integer;
begin
  FSQLite.DBQuery('begin transaction', []);
  Inc(FTransCount);
  Result := FTransCount;
end;

procedure TSQLiteStorage.DoSetVersion(const Value: string);
begin
  try
    FSQLite.DBQuery('update [sys_constants] set [version] = ?', [Value]);  // todo: set version
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
      FSQLite.DBQuery(vAlterText, []);

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
      FSQLite.DBQuery(Format('ALTER TABLE [%s] DROP [%s]', [FCurTableName, vColName]), []);
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
    FSQLite.DBQuery(vAlterText, []);
    FCurTableColumns.Add(vColumn);
    FCurTableTypes.Add(vType);
  end
  else begin
    if (FCurTableTypes[vColIdx] <> vType) then
    begin
      // no alter column - replace column
      vAlterText := Format('ALTER TABLE [%s] DROP [%s]', [ FCurTableName, vColumn ]);
      FSQLite.DBQuery(vAlterText, []);
      vAlterText := Format('ALTER TABLE [%s] ADD [%s] %s', [ FCurTableName, vColumn, vType ]);
      FSQLite.DBQuery(vAlterText, []);
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
  GetActiveContext.AddParameter(ATag, AValue, AIsKey);
end;

function TSQLiteStorage.GetActiveContext: TSQLiteDBContext;
begin
  Result := TSQLiteDBContext(FGroupList[FGroupList.Count - 1]);
end;

function TSQLiteStorage.GetActiveDataset: TSQLiteResult;
begin
  Result := TSQLiteResult(FGroupList[ FGroupList.Count - 1 ]);
end;

function TSQLiteStorage.GetMaxTableID(const ATableName: string): Integer;
var
  v: Variant;
begin
  Result := 0;
  if not FSQLite.DBQuery(Format('select max([id]) as [m] from [%s]', [ATableName]), []) then Exit;
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

  if not FSQLite.DBQuery(Format('PRAGMA table_info(%s)', [AName]), []) then Exit;
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

  if not FSQLite.DBQuery('select [last_id] from [numerators] where [table_name] = ?', [ATag]) then
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
    FSQLite.DBQuery('insert into [numerators] ([id], [table_name], [last_id], [last_code]) values (?, ?, ?, ?)',
      [0, ATag, ALastID, 0])
  else if ALastID > vLastStoredID then
    FSQLite.DBQuery('update [numerators] set [last_id] = ? where [table_name] = ?', [ALastID, ATag]);
end;

initialization

TBaseModule.RegisterModule('Storage', 'SQLite', TSQLiteStorage);

end.

