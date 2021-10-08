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

unit uOLEDBStorage;

interface

uses
  Classes, ADOX_TLB, DB, Generics.Collections, uDBConnector, uStorage, uLogger, uJSON, uParameters, uConsts;

type
  TDBContext = class
  private
    FStorage: TStorage;
    FTableName: string;
    FParameters: TList<TBaseParameter>;
    FKeyParameters: TList<TBaseParameter>;
    function BuildInsertQuery: string;
    function BuildUpdateQuery: string;
    function BuildDeleteQuery: string;
    function GetDBParameter(const AParamName: string): TBaseParameter;
    function PrepareQuery: string;
    function OpenQuery(const ADBConnector: TDBConnector): TDataset;
    procedure OpenQueryAsync(const ADBConnector: TDBConnector; const AFetchFunc: TFetchDataFunc);
  public
    constructor Create(const AStorage: TStorage; const ATableName: string);
    destructor Destroy; override;

    procedure AddParameter(const AName: string; const AValue: Variant;
      const AIsKey: Boolean = False);
    procedure AddBlobParameter(const AName: string; const AValue: TStream);
    procedure Commit(const ADBConnector: TDBConnector;
      const ASaveAction: TEntitySaveAction);
  end;

  TOLEDBStorage = class(TStorage)
  private
    FCatalog: _Catalog;
    FCurTableDef: _Table;
    FDBConnector: TDBConnector;
    FGroupList: TList;
    function GetActiveDataset: TDataset;
    function GetActiveContext: TDBContext;
    procedure Push(const AObject: TObject);
    procedure Pop;
    procedure PopContext;
    function SearchTable(const ATag: string): _Table;
    function SearchColumn(const ATable: _Table; const ATag: string): _Column;
    function SearchPrimaryKey(const ATable: _Table): _Key;
    function SearchClusteredIndex(const ATable: _Table): _Index;
    function DataTypeFromFieldKind(const AKind: TFieldKind;
      const ASize: Integer): DataTypeEnum;
    procedure RefreshCatalog;
  protected
    FConnectionString: string;
    function SQLTypeFromFieldKind(const AKind: TFieldKind;
      const ASize: Integer): string; virtual; abstract;
    procedure CheckTableIndexes(const ATable: _Table); virtual; abstract;

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
    // –абота с транзакци€ми
    function DoBeginTransaction: Integer; override;
    procedure DoCommitTransaction(const ATransactionID: Integer = -1); override;
    procedure DoRollbackTransaction(const ATransactionID: Integer = -1); override;
  public
    constructor Create(const ALogger: TLogger; const AParams: string = ''); override;
    destructor Destroy; override;

    procedure UpdateNumerator(const ATag: string; const ALastID: Integer); override;
  end;

  TMSAccessStorage = class(TOLEDBStorage)
  protected
    function SQLTypeFromFieldKind(const AKind: TFieldKind;
      const ASize: Integer): string; override;
    procedure CheckTableIndexes(const ATable: _Table); override;
  public
    constructor Create(const ALogger: TLogger; const AParams: string = ''); override;
  end;

  TSQLServerStorage = class(TOLEDBStorage)
  protected
    function SQLTypeFromFieldKind(const AKind: TFieldKind;
      const ASize: Integer): string; override;
    procedure CheckTableIndexes(const ATable: _Table); override;
  public
    constructor Create(const ALogger: TLogger; const AParams: string = ''); override;
  end;

implementation

uses
  Variants, SysUtils, StrUtils, uModule;

{ TDBContext }

procedure TDBContext.AddBlobParameter(const AName: string;
  const AValue: TStream);
var
  vParameter: TBlobParameter;
begin
  vParameter := TBlobParameter.Create(AName, AValue);
  FParameters.Add(vParameter);
end;

procedure TDBContext.AddParameter(const AName: string; const AValue: Variant;
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

function TDBContext.BuildDeleteQuery: string;
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
      vWhereStr := '[' + vParamName + '] = :' + vParamName;
      vWasFirst := True;
    end
    else
      vWhereStr := vWhereStr + ' and [' + vParamName + '] = :' + vParamName;
  end;

  if Length(vWhereStr) = 0 then
    Result := ''
  else
    Result := Format('delete from [%s] where %s', [FTableName, vWhereStr]);
end;

function TDBContext.BuildInsertQuery: string;
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

function TDBContext.BuildUpdateQuery: string;
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
    Result := Format('update [%s] set %s where %s', [
      FTableName, vSetStr, vWhereStr]);
end;

procedure TDBContext.Commit(const ADBConnector: TDBConnector;
  const ASaveAction: TEntitySaveAction);
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

  ADBConnector.SQLExec(vSQLText, GetDBParameter);
end;

constructor TDBContext.Create(const AStorage: TStorage;const ATableName: string);
begin
  inherited Create;

  FStorage := AStorage;
  FTableName := ATableName;
  FParameters := TList<TBaseParameter>.Create;
  FKeyParameters := TList<TBaseParameter>.Create;
end;

destructor TDBContext.Destroy;
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

function TDBContext.GetDBParameter(const AParamName: string): TBaseParameter;
var
  i: Integer;
begin
  for i := 0 to FParameters.Count - 1 do
  begin
    Result := TBaseParameter(FParameters[i]);
    if Result.Name = AParamName then
      Exit;
  end;

  for i := 0 to FKeyParameters.Count - 1 do
  begin
    Result := TSimpleParameter(FKeyParameters[i]);
    if Result.Name = AParamName then
      Exit;
  end;

  Result := nil;
  Assert(False, 'Unknown parameter "' + AParamName + '"');
end;

function TDBContext.OpenQuery(const ADBConnector: TDBConnector): TDataset;
var
  vSQLText: string;
begin
  vSQLText := PrepareQuery;
  Result := ADBConnector.SQLOpen(vSQLText, GetDBParameter);
end;

procedure TDBContext.OpenQueryAsync(const ADBConnector: TDBConnector; const AFetchFunc: TFetchDataFunc);
var
  vSQLText: string;
begin
  vSQLText := PrepareQuery;
  ADBConnector.LoadData(vSQLText, GetDBParameter, AFetchFunc);
end;

function TDBContext.PrepareQuery: string;
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
    if vParamName = 'id' then
      Continue;

    if not vWasFirst then
    begin
      vWhereStr := vParamName + ' = :' + vParamName;
      vWasFirst := True;
    end
    else
      vWhereStr := vWhereStr + ' and ' + vParamName + ' = :' + vParamName;
  end;

  if vWhereStr <> '' then
    vWhereStr := ' where ' + vWhereStr;
  Result := Format('select * from [%s]' + vWhereStr, [FTableName]);
end;

{ TOLEDBStorage }

//http://hiprog.com/index.php?option=com_content&task=view&id=251661555&Itemid=35

constructor TOLEDBStorage.Create(const ALogger: TLogger; const AParams: string = '');
  {function TypeToStr(const AType: DataTypeEnum): string;
  begin
    case AType of
      adTinyInt: Result := 'TinyInt';
      adSmallInt: Result := 'SmallInt';
      adInteger: Result := 'Integer';
      adBigInt: Result := 'BigInt';
      adUnsignedTinyInt: Result := 'UnsignedTinyInt';
      adUnsignedSmallInt: Result := 'UnsignedSmallInt';
      adUnsignedInt: Result := 'UnsignedInt';
      adUnsignedBigInt: Result := 'UnsignedBigInt';
      adSingle: Result := 'Single';
      adDouble: Result := 'Double';
      adCurrency: Result := 'Currency';
      adDecimal: Result := 'Decimal';
      adNumeric: Result := 'Numeric';
      adBoolean: Result := 'Boolean';
      adError: Result := 'Error';
      adUserDefined: Result := 'UserDefined';
      adVariant: Result := 'Variant';
      adIDispatch: Result := 'IDispatch';
      adIUnknown: Result := 'IUnknown';
      adGUID: Result := 'GUID';
      adDate: Result := 'Date';
      adDBDate: Result := 'DBDate';
      adDBTime: Result := 'DBTime';
      adDBTimeStamp: Result := 'DBTimeStamp';
      adBSTR: Result := 'BSTR';
      adChar: Result := 'Char';
      adVarChar: Result := 'VarChar';
      adLongVarChar: Result := 'LongVarChar';
      adWChar: Result := 'WChar';
      adVarWChar: Result := 'VarWChar';
      adLongVarWChar: Result := 'LongVarWChar';
      adBinary: Result := 'Binary';
      adVarBinary: Result := 'VarBinary';
      adLongVarBinary: Result := 'LongVarBinary';
      adChapter: Result := 'Chapter';
      adFileTime: Result := 'FileTime';
      adPropVariant: Result := 'PropVariant';
      adVarNumeric: Result := 'VarNumeric';
    else
      Result := 'Empty';
    end;
  end;

  {function DebugData: string;
  var
    vTable: _Table;
    vColumn: _Column;
    vProperty: Property_;
    vType: string;
    vAttrib: string;

    i, j, k: Integer;
    vStr: TStringList;
  begin
    for i := 0 to FCatalog.Tables.Count - 1 do
    begin
      vTable := FCatalog.Tables[i];
      vType := AnsiUpperCase(vTable.type_);
      Result := Result + #13#10 + vTable.Name + ' : ' + vType;
      for j := 0 to vTable.Columns.Count - 1 do
      begin
        vColumn := vTable.Columns[j];

        if vColumn.Attributes = adColFixed then
          vAttrib := 'FIXED'
        else
          vAttrib := 'NULL';

        Result := Result + #13#10 + '  ' + vColumn.Name + ' : ' + TypeToStr(vColumn.type_);
        Result := Result + #13#10 + '    size = ' + IntToStr(vColumn.DefinedSize);
        Result := Result + #13#10 + '    scale = ' + IntToStr(vColumn.NumericScale);
        Result := Result + #13#10 + '    precision = ' + IntToStr(vColumn.Precision);
        Result := Result + #13#10 + '    attrib = ' + vAttrib;

        for k := 0 to vColumn.Properties.Count - 1 do
        begin
          vProperty := vColumn.Properties[k];
          Result := Result + #13#10 + '      ' + vProperty.Name + '=' + VarToStr(vProperty.Value) + ' : ' + TypeToStr(vProperty.type_);
        end;
      end;
    end;

    vStr := TStringList.Create;
    vStr.Text := Result;
    vStr.SaveToFile('structure.txt');
    vStr.Free;
  end; }
begin
  inherited Create(ALogger, AParams);
  FGroupList := TList.Create;
  FConnectionString := AParams;
end;

function TOLEDBStorage.DataTypeFromFieldKind(
  const AKind: TFieldKind; const ASize: Integer): DataTypeEnum;
begin
  case AKind of
    fkString:
      if (ASize > 255) or (ASize <= 0) then
        Result := adLongVarWChar
      else
        Result := adVarWChar;
    fkFloat: Result := adSingle;
    fkDateTime: Result := adDate;
    fkBoolean: Result := adBoolean;
    fkCurrency: Result := adCurrency;
    fkList: Result := adEmpty;
    fkBlob, fkComplex: Result := adLongVarBinary;
  else
    Result := adInteger;
  end;
end;

destructor TOLEDBStorage.Destroy;
begin
  Disconnect;
  FDBConnector.Free;
  FGroupList.Free;
  inherited Destroy;
end;

procedure TOLEDBStorage.DoActivate(const ATag: string);
var
  vContext: TDBContext;
begin
  // «десь нужно запомнить им€ таблицы и создать контекст
  vContext := TDBContext.Create(Self, ATag);

  Push(vContext);
end;

procedure TOLEDBStorage.DoCommitTransaction(const ATransactionID: Integer);
begin
  FDBConnector.CommitTrans;
end;

procedure TOLEDBStorage.DoConnect;
begin
  try
    FDBConnector.Connect;
    FCatalog := CoCatalog.Create;
    FCatalog.Set_ActiveConnection(FConnectionString);
  except
    on E: Exception do
    begin
      FLogger.AddMessage('Error: ' + E.Message);
      raise;
    end;
  end;
end;

function TOLEDBStorage.DoCreateCodes(const ATag: string; const ACount: Integer): Integer;
begin
  Result := FDBConnector.NewUniqueCodes(ATag, ACount);
end;

function TOLEDBStorage.DoCreateIDs(const ATag: string; const ACount: Integer): Integer;
begin
  Result := FDBConnector.NewUniqueIDs(ATag, ACount);
end;

procedure TOLEDBStorage.DoDeactivate;
begin
  PopContext;
end;

procedure TOLEDBStorage.DoDisconnect;
begin
  FDBConnector.Disconnect;
  FCatalog := nil;
end;

function TOLEDBStorage.DoGetChanges(const ALogID: Integer): TJSONObject;
var
  vCollectionName: string;
//  vCollection: TEntities;
  vCurrentLogID: Integer;
  vDataset: TDataset;
  vUpdates: TJSONArray;
  vUpdate: TJSONObject;
  vActions: TJSONArray;
  vAction: TJSONObject;
  vLastLogID: Integer;
// ‘ормат файла JSON - обновление
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
//               "name": "¬ас€",
//               ...
//             }
//         },
//         // другие действи€ в этом обновлении
//         {
//           ...
//         }
//       ]
//     }
//     {
//       // другие обновлени€
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

  vDataset := FDBConnector.SQLOpen('select la.sys_log_id, l.user_name, l.log_time, ' +
    'la.id, la.action_kind_id, la.collection, la.entity_id, la.json ' +
    'from sys_log l inner join sys_log_actions la on la.sys_log_id = l.id ' +
    'where l.id > :log_id order by l.id, la.action_kind_id, la.collection', [ALogID]);

  try
    Result.AddPair('last_log_id', TJSONNumber.Create(vCurrentLogID));
    vUpdates := TJSONArray.Create;
    Result.AddPair('updates', vUpdates);
    vLastLogID := -1;
    vActions := nil;

    while not vDataset.Eof do
    begin
      vCollectionName := vDataset.FieldByName('collection').AsString;
      vCurrentLogID := vDataset.FieldByName('sys_log_id').AsInteger;
      if vLastLogID < vCurrentLogID then
      begin
        vLastLogID := vCurrentLogID;
        vUpdate := TJSONObject.Create;
        vUpdate.AddPair('log_id', TJSONNumber.Create(vCurrentLogID));
        vUpdate.AddPair('user_name', vDataset.FieldByName('user_name').AsString);
        vUpdate.AddPair('log_time', FormatDateTime('dd.mm.yy hh:nn:ss', vDataset.FieldByName('log_time').AsDateTime));
        vActions := TJSONArray.Create;
        vUpdate.AddPair('actions', vActions);
        vUpdates.AddElement(vUpdate);
      end;

      vAction := TJSONObject.Create;
      vAction.AddPair('change_id', TJSONNumber.Create(vDataset.FieldByName('id').AsInteger));
      vAction.AddPair('action_kind_id', TJSONNumber.Create(vDataset.FieldByName('action_kind_id').AsInteger));
      vAction.AddPair('collection', vCollectionName);
      vAction.AddPair('entity_id', TJSONNumber.Create(vDataset.FieldByName('entity_id').AsInteger));
      vAction.AddPair('fields', TJSONObject.ParseJSONValue(vDataset.FieldByName('json').AsString));
      vActions.AddElement(vAction);

      vDataset.Next;
    end;
  finally
    FDBConnector.ReleaseDataset(vDataset);
  end;
end;

function TOLEDBStorage.DoGetLastLogID: Integer;
begin
  Result := FDBConnector.GetLastLogID;
end;

function TOLEDBStorage.DoGetVersion: string;
var
  vDataset: TDataset;
begin
  try
    vDataset := FDBConnector.SQLOpen('select [version] from [sys_constants]', []);
    try
      if not vDataset.Eof then
        Result := vDataset.FieldByName('version').AsString
      else
        Result := '';
    finally
      FDBConnector.ReleaseDataset(vDataset);
    end;
  except
    Result := '';
  end;
end;

// ƒобавить сюда QueryObject
procedure TOLEDBStorage.DoReadGroup(const ATag: string; const AReadFunc: TStorageFunc);
var
  vDataset: TDataset;
begin
  vDataset := GetActiveContext.OpenQuery(FDBConnector);
  Push(vDataset);
  try
    while not vDataset.Eof do
    begin
      if Assigned(AReadFunc) then
        AReadFunc(Self);

      vDataset.Next;
    end;
  finally
    Pop;
    FDBConnector.ReleaseDataset(vDataset);
  end;
end;

procedure TOLEDBStorage.DoReadGroupAsync(const ATag: string; const AFetchFunc: TFetchDataFunc);
begin
  GetActiveContext.OpenQueryAsync(FDBConnector, AFetchFunc);
end;

function TOLEDBStorage.DoReadStream(const ATag: string): TStream;
var
  vDataset: TDataset;
  vField: TField;
begin
  vDataset := GetActiveDataset;
  vField := vDataset.FindField(ATag);

  if Assigned(vField) and not (vField.IsNull) then
  begin
    Result := TMemoryStream.Create;
    TBlobField(vField).SaveToStream(Result);
  end
  else
    Result := nil;
end;

function TOLEDBStorage.DoReadValue(const ATag: string; const AFieldKind: TFieldKind): Variant;
var
  vDataset: TDataset;
  vField: TField;
begin
  vDataset := GetActiveDataset;
  vField := vDataset.FindField(ATag);

  if Assigned(vField) then
    Result := vField.AsVariant
  else
    Result := Null;
end;

procedure TOLEDBStorage.DoRebuild;

{  procedure CompactDatabase_JRO(const AConnString: string);
  var
    vPos: Integer;
    vPrefix: string;
    vPostFix: string;
    vOldFileName: string;
    vTempFileName: string;
    vTempConnStr: string;
    vJetEngine: IJetEngine;
  begin
    vOldFileName := AConnString;
    vPos := Pos('Data Source=', AConnString);
    if vPos > 0 then
    begin
      vPrefix := Copy(vOldFileName, 1, vPos - 1) + 'Data Source=';
      Delete(vOldFileName, 1, vPos + Length('Data Source=') - 1);
      vPos := Pos(';', vOldFileName);
      if vPos > 0 then
      begin
        vPostfix := vOldFileName;
        Delete(vPostFix, 1, vPos - 1);
        vOldFileName := Copy(vOldFileName, 1, vPos - 1);
      end
      else
        vPostFix := '';

      vTempFileName := ChangeFileExt(vOldFileName, '.tmp');
      vTempConnStr := vPrefix + vTempFileName + vPostFix;

      DeleteFile(PChar(vTempFileName));
      vJetEngine := CoJetEngine.Create as IJetEngine;
      try
        vJetEngine.CompactDatabase(AConnString, vTempConnStr);
      finally
        vJetEngine := nil;
      end;

      if DeleteFile(vOldFileName) then
        RenameFile(vTempFileName, vOldFileName);
    end;
  end;  }
begin
//  CompactDatabase_JRO(FConnectionString);
  try
    Disconnect;
    Sleep(1000);
  finally
    Connect;
  end;
end;

procedure TOLEDBStorage.DoRollbackTransaction(const ATransactionID: Integer);
begin
  FDBConnector.RollbackTrans;
end;

function TOLEDBStorage.DoBeginTransaction: Integer;
begin
  Result := FDBConnector.BeginTrans;
end;

procedure TOLEDBStorage.DoSetVersion(const Value: string);
begin
  try
    FDBConnector.SQLExec('update sys_constants set version=:version', [Value]);
  except
  end;
end;

function TOLEDBStorage.DoSyncGroupDef(const ATag: string;
  const ASyncFunc: TStorageFunc): Boolean;
var
  vColumn: _Column;
  i: Integer;
  vAlterText: string;
begin
  FCurTableDef := SearchTable(ATag);
  Result := Assigned(FCurTableDef);
  if not Result then
  begin
    try
      if ATag = 'numerators' then
        vAlterText := 'CREATE TABLE [' + ATag + '] ([id] int NULL)'
      else
        vAlterText := 'CREATE TABLE [' + ATag + '] ([id] int NOT NULL)';
      FDBConnector.SQLExec(vAlterText, []);
      repeat
        RefreshCatalog;
        FCurTableDef := SearchTable(ATag);
      until Assigned(FCurTableDef);
    except
      on E: Exception do
      begin
        FLogger.AddMessage('Error: ' + E.Message);
      end;
    end;
  end;

  if Assigned(ASyncFunc) then
    ASyncFunc(Self);

  if ATag <> 'numerators' then
    CheckTableIndexes(FCurTableDef);

  for i := FCurTableDef.Columns.Count - 1 downto 0 do
  begin
    vColumn := FCurTableDef.Columns[i];
    if FItemDefsList.IndexOf(vColumn.Name) < 0 then
      FCurTableDef.Columns.Delete(i);
  end;

  RefreshCatalog;
end;

procedure TOLEDBStorage.DoSyncItemDef(const ATag: string;
  const AKind: TFieldKind; const ASize: Integer);
var
  vColumn: _Column;
  vType: DataTypeEnum;
  vAlterText: string;
  vSize: Integer;
begin
  vColumn := SearchColumn(FCurTableDef, ATag);

  vType := DataTypeFromFieldKind(AKind, ASize);
  if vType = adEmpty then
    Exit;

  if vColumn = nil then
  begin
    // ≈сть непри€тный эффект в Access - поле отсутствует в списке
    // полей таблицы, но одновременно таблица считает, что такое поле
    // уже было, и его добавл€ть нельз€. Ёто происходит, когда поле
    // было добавлено и после этого удалено, а сейчас снова требуетс€
    // его добавить
    try
      vAlterText := 'ALTER TABLE [' + FCurTableDef.Name + '] DROP COLUMN [' + ATag + ']';
      FDBConnector.SQLExec(vAlterText, []);
    except
    end;

    vAlterText := 'ALTER TABLE [' + FCurTableDef.Name + '] ADD [' +
      ATag + '] ' + SQLTypeFromFieldKind(AKind, ASize);

    FDBConnector.SQLExec(vAlterText, []);
  end
  else begin
    if AKind = fkBoolean then
      vSize := 2
    else
      vSize := ASize;

    if (vColumn.type_ <> vType) or
      ((vColumn.DefinedSize <> vSize) and (vType <> adLongVarWChar) and (vType <> adLongVarBinary){Access Error}) then
    begin
      // ќбрезка существующих данных под новый размер пол€
      if (AKind = fkString) and (vColumn.DefinedSize > vSize) and (vSize > 0) then
      begin
        vAlterText := 'UPDATE [' + FCurTableDef.Name + '] SET [' + vColumn.Name +
          '] = LEFT([' + vColumn.Name + '], ' + IntToStr(vSize) + ')';
        FDBConnector.SQLExec(vAlterText, []);
      end;

      vAlterText := 'ALTER TABLE [' + FCurTableDef.Name + '] ALTER COLUMN [' +
        vColumn.Name + '] ' + SQLTypeFromFieldKind(AKind, ASize);

      FDBConnector.SQLExec(vAlterText, []);
    end;
  end;
end;

procedure TOLEDBStorage.DoWriteItem(const AWriteFunc: TStorageFunc;
  const AID, ALogID: Integer; const ASaveAction: TEntitySaveAction);
begin
  DoWriteValue('id', fkInteger, AID, True);
  DoWriteValue('log_id', fkInteger, ALogID, False);
  if Assigned(AWriteFunc) then
    AWriteFunc(Self);
  GetActiveContext.Commit(FDBConnector, ASaveAction);
end;

procedure TOLEDBStorage.DoWriteStream(const ATag: string;
  const AStream: TStream);
begin
  GetActiveContext.AddBlobParameter(ATag, AStream);
end;

procedure TOLEDBStorage.DoWriteValue(const ATag: string; const AFieldKind: TFieldKind;
  const AValue: Variant; const AIsKey: Boolean = False);
begin
  GetActiveContext.AddParameter(ATag, AValue, AIsKey);
end;

function TOLEDBStorage.GetActiveContext: TDBContext;
begin
  Result := TDBContext(FGroupList[FGroupList.Count - 1]);
end;

function TOLEDBStorage.GetActiveDataset: TDataset;
begin
  Result := TDataset(FGroupList[FGroupList.Count - 1]);
end;

procedure TOLEDBStorage.Pop;
begin
  FGroupList.Delete(FGroupList.Count - 1);
end;

procedure TOLEDBStorage.PopContext;
var
  vIndex: Integer;
begin
  vIndex := FGroupList.Count - 1;
  TDBContext(FGroupList[vIndex]).Free;
  FGroupList.Delete(FGroupList.Count - 1);
end;

procedure TOLEDBStorage.Push(const AObject: TObject);
begin
  FGroupList.Add(Pointer(AObject));
end;

procedure TOLEDBStorage.RefreshCatalog;
begin
  FCatalog := nil;
  if not Assigned(FCatalog) then
  begin
    FCatalog := CoCatalog.Create;
    FCatalog.Set_ActiveConnection(FConnectionString);
  end;
end;

function TOLEDBStorage.SearchColumn(const ATable: _Table; const ATag: string): _Column;
var
  i: Integer;
begin
  if not Assigned(ATable) then
  begin
    Result := nil;
    Exit;
  end;

  for i := 0 to ATable.Columns.Count - 1 do
  begin
    Result := ATable.Columns[i];
    if SameText(Result.Name, ATag) then
      Exit;
  end;

  Result := nil;
end;

function TOLEDBStorage.SearchPrimaryKey(const ATable: _Table): _Key;
var
  i: Integer;
begin
  if not Assigned(ATable) then
  begin
    Result := nil;
    Exit;
  end;

  for i := 0 to ATable.Keys.Count - 1 do
  begin
    Result := ATable.Keys[i];
    if Result.type_ = adKeyPrimary then
      Exit;
  end;

  Result := nil;
end;

function TOLEDBStorage.SearchClusteredIndex(const ATable: _Table): _Index;
var
  i: Integer;
begin
  if not Assigned(ATable) then
  begin
    Result := nil;
    Exit;
  end;

  for i := 0 to ATable.Indexes.Count - 1 do
  begin
    Result := ATable.Indexes[i];
    if Result.Clustered then
      Exit;
  end;

  Result := nil;
end;

function TOLEDBStorage.SearchTable(const ATag: string): _Table;
var
  i: Integer;
begin
  for i := 0 to FCatalog.Tables.Count - 1 do
  begin
    Result := FCatalog.Tables[i];
    if SameText(Result.Name, ATag) then
    begin
      Assert(SameText(Result.type_, 'TABLE'), '»спользуемое название таблицы [' + ATag +
        '] используетс€ в качестве системного самим хранилищем');
      Exit;
    end;
  end;

  Result := nil;
end;

procedure TOLEDBStorage.UpdateNumerator(const ATag: string; const ALastID: Integer);
var
  vIDDataset: TDataset;
  vLastStoredID: Integer;
begin
  if ALastID < 0 then
    Exit;

  vIDDataset := FDBConnector.SQLOpen('select last_id from numerators where table_name = :table_name', [ATag]);
  try
    if vIDDataset.Eof then
      vLastStoredID := -1
    else
      vLastStoredID := vIDDataset.FieldByName('last_id').AsInteger;
  finally
    FDBConnector.ReleaseDataset(vIDDataset);
  end;

  if vLastStoredID < 0 then
    FDBConnector.SQLExec('insert into numerators (id, table_name, last_id, last_code) ' +
      'values (:id, :table_name, :last_id, :last_code)', [0, ATag, ALastID, 0])
  else if ALastID > vLastStoredID then
    FDBConnector.SQLExec('update numerators set last_id=:last_id where table_name=:table_name', [ALastID, ATag]);
end;

{ TSQLServerStorage }

procedure TSQLServerStorage.CheckTableIndexes(const ATable: _Table);
var
  vIndex: _Index;
  vKey: _Key;
begin
{ TODO : ≈сли индекс с указанным именем уже существует, программа упадет с ошибкой }

  vIndex := SearchClusteredIndex(FCurTableDef);
  if vIndex = nil then
  begin
    vKey := SearchPrimaryKey(FCurTableDef);
    if vKey <> nil then
      FCurTableDef.Keys.Delete(vKey.Name);

    try
      if FCurTableDef.Name <> 'numerators' then
      begin
        vIndex := CoIndex.Create;
        vIndex.Name := FCurTableDef.Name + '_id';
        vIndex.PrimaryKey := True;
        vIndex.Unique := True;
        vIndex.Clustered := True;
        vIndex.Columns.Append('id', adInteger, 32);
        FCurTableDef.Indexes.Append(vIndex, vIndex.Columns);
      end;
    except
    end;
  end;
end;

constructor TSQLServerStorage.Create(const ALogger: TLogger; const AParams: string = '');
begin
  inherited Create(ALogger, AParams);

  FName := 'MSSQLServer';

  try
    FDBConnector := TSQLServerConnector.Create(FConnectionString);
  except
    on E: Exception do
    begin
      FLogger.AddMessage('Error: ' + E.Message);
      raise;
    end;
  end;
end;

function TSQLServerStorage.SQLTypeFromFieldKind(const AKind: TFieldKind;
  const ASize: Integer): string;
begin
  case AKind of
    fkString:
      if ASize > 0 then
        Result := 'NVARCHAR(' + IntToStr(ASize) + ')'
      else
        Result := 'NVARCHAR(MAX)';
    fkFloat: Result := 'REAL';
    fkDateTime: Result := 'DATETIME';
    fkBoolean: Result := 'BIT';
    fkCurrency: Result := 'MONEY';
    fkList: Result := '';
    fkBlob, fkComplex: Result := 'IMAGE';
  else
    Result := 'INT';
  end;
end;

{ TMSAccessStorage }

procedure TMSAccessStorage.CheckTableIndexes(const ATable: _Table);
var
  vKey: _Key;
begin
  vKey := SearchPrimaryKey(FCurTableDef);
  if vKey = nil then
    FCurTableDef.Keys.Append(FCurTableDef.Name + '_id', adKeyPrimary, 'id', '', '');
end;

constructor TMSAccessStorage.Create(const ALogger: TLogger; const AParams: string = '');
begin
  inherited Create(ALogger, AParams);

  FName := 'MSAccess';

  FDBConnector := TAccessConnector.Create(FConnectionString);
end;

function TMSAccessStorage.SQLTypeFromFieldKind(const AKind: TFieldKind;
  const ASize: Integer): string;
begin
  case AKind of
    fkString:
      if (ASize > 255) or (ASize <= 0) then
        Result := 'LONGTEXT'
      else
        Result := 'TEXT(' + IntToStr(ASize) + ')';
    fkFloat: Result := 'REAL';
    fkDateTime: Result := 'DATETIME';
    fkBoolean: Result := 'BIT';
    fkCurrency: Result := 'MONEY';
    fkList: Result := '';
    fkBlob, fkComplex: Result := 'OLEOBJECT';
  else
    Result := 'INTEGER';
  end;
end;

initialization

TBaseModule.RegisterModule('Storage', 'MSAccess.OLEDB', TMSAccessStorage);
TBaseModule.RegisterModule('Storage', 'SQLServer.OLEDB', TSQLServerStorage);

end.

