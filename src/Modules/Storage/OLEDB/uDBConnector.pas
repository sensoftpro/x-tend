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

unit uDBConnector;

interface

uses
  ActiveX, DB, ADODB, Generics.Collections, Classes, IniFiles, uParameters, uConsts;

type
  TOnLoadDataEvent = procedure();
  TDBConnector = class;

  TConnectionData = class
  private
    FDBConnector: TDBConnector;
    FConnection: TADOConnection;
    FQuery: TADOQuery;
    FError: string;
    FRecordsetBookmark: Variant;
    FFields: Variant;
    FFieldDict: TDictionary<string, Integer>;
    FRows: Variant;
    FCount: Integer;
    FOnLoadData: TFetchDataFunc;
    procedure ExecuteComplete(Connection: TADOConnection; RecordsAffected: Integer; const Error: Error;
      var EventStatus: TEventStatus; const Command: _Command; const Recordset: _Recordset);
    procedure FetchComplete(DataSet: TCustomADODataSet; const Error: Error; var EventStatus: TEventStatus);
    procedure FetchProgress(DataSet: TCustomADODataSet; Progress, MaxProgress: Integer; var EventStatus: TEventStatus);
    procedure HandleFetchProgress(DataSet: TCustomADODataSet; Progress, MaxProgress: Integer);
    procedure HandleFetchComplete(DataSet: TCustomADODataSet);
  public
    constructor Create(const ADBConnector: TDBConnector);
    destructor Destroy; override;

    procedure Load(const ASQL: string; const AOnLoadData: TFetchDataFunc);
  end;

  TDBConnector = class
  private
    FConnection: TADOConnection;
    FConnectionString: string;
    FIsOuterConnection: Boolean;
    FWasConnected: Boolean;
    FTransCount: Integer;
    FQueryCash: THashedStringList;

    procedure SetupParameters(const AQuery: TADOQuery;
      const AParams: array of Variant; const ABLOB: TStream = nil);
    procedure SetupCustomParams(const AQuery: TADOQuery;
      const AGetDBParameterFunc: TGetDBParameterFunc);
    function RetrieveQuery(const ASQL: string): TADOQuery;
    function InternalExecQuery(const AQuery: TADOQuery; const ACount: Integer = 0): Integer;
    procedure InternalOpenQuery(var AQuery: TADOQuery; const ACount: Integer = 0);
  protected
    function GetServerTime: TDateTime; virtual;
    function GetUniqueIDs(const ATableName: string;
      const ACount: Integer): Integer; virtual;
    function GetUniqueCodes(const ATableName: string;
      const ACount: Integer): Integer; virtual;
    procedure TuneQuery(const AQuery: TADOQuery); virtual;
  public
    class function VerifyConnectionString(const AConnectionString: string): Boolean;
  public
    constructor Create(const AConnection: TADOConnection); overload;
    constructor Create(const AConnectionString: string); overload;
    destructor Destroy; override;

    function SQLExec(const ASQL: string;
      const AParams: array of Variant;
      const ABLOB: TStream = nil): Integer; overload;
    function SQLExec(const ASQL: string;
      const AGetDBParameterFunc: TGetDBParameterFunc): Integer; overload;

    function SQLOpen(const ASQL: string;
      const AParams: array of Variant;
      const ABLOB: TStream = nil): TDataset; overload;
    function SQLOpen(const ASQL: string;
      const AGetDBParameterFunc: TGetDBParameterFunc): TDataset; overload;

    procedure LoadData(const ASQL: string; const AGetDBParameterFunc: TGetDBParameterFunc;
      const AOnDataLoad: TFetchDataFunc);

    procedure Connect;
    procedure Disconnect;

    function GetMaxTableID(const ATableName: string): Integer;
    function NewUniqueIDs(const ATableName: string;
      const ACount: Integer): Integer;
    function NewUniqueCodes(const ATableName: string;
      const ACount: Integer): Integer;
    function GetLastLogID: Integer;
    function Now: TDateTime;

    function BeginTrans: Integer;
    procedure CommitTrans;
    function RollbackTrans: Integer;
    function InTransaction: Boolean;

    procedure ReleaseDataset(var ADataset: TDataset);

    property Connection: TADOConnection read FConnection;
  end;

  TSQLServerConnector = class(TDBConnector)
  protected
    function GetServerTime: TDateTime; override;
    function GetUniqueIDs(const ATableName: string;
      const ACount: Integer): Integer; override;
    function GetUniqueCodes(const ATableName: string;
      const ACount: Integer): Integer; override;
    procedure TuneQuery(const AQuery: TADOQuery); override;
  end;

  TAccessConnector = class(TDBConnector)
  protected
    function GetUniqueIDs(const ATableName: string;
      const ACount: Integer): Integer; override;
    function GetUniqueCodes(const ATableName: string;
      const ACount: Integer): Integer; override;
    procedure TuneQuery(const AQuery: TADOQuery); override;
  end;

implementation

uses
  SysUtils, ComObj, ADOInt, {ADOConEd,} Variants;

{ TDBConnector }

function TDBConnector.BeginTrans: Integer;
begin
  if FTransCount = 0 then
    FConnection.BeginTrans;

  FTransCount := FTransCount + 1;

  Result := FTransCount;
end;

procedure TDBConnector.CommitTrans;
begin
  FTransCount := FTransCount - 1;

  if FTransCount = 0 then
    FConnection.CommitTrans;
end;

procedure TDBConnector.Connect;
begin
  if not FConnection.Connected then
  begin
    FConnection.ConnectionString := FConnectionString;
    //EditConnectionString(FConnection);
    FConnection.Connected := True;
  end;
end;

constructor TDBConnector.Create(const AConnection: TADOConnection);
begin
  inherited Create;

  FConnection := AConnection;
  FConnectionString := AConnection.ConnectionString;

  FIsOuterConnection := True;
  FWasConnected := FConnection.Connected;

  FQueryCash := THashedStringList.Create;

  FTransCount := 0;
end;

constructor TDBConnector.Create(const AConnectionString: string);
begin
  inherited Create;

  FConnectionString := AConnectionString;

  FConnection := TADOConnection.Create(nil);
  FConnection.LoginPrompt := False;
  FConnection.KeepConnection := True;
  { TODO : wa -check }
  //FConnection.IsolationLevel := ilReadCommitted;

  FIsOuterConnection := False;
  FWasConnected := False;

  FQueryCash := THashedStringList.Create;

  FTransCount := 0;
end;

destructor TDBConnector.Destroy;
var
  i: Integer;
begin
  if Assigned(FQueryCash) then
  begin
    for i := 0 to FQueryCash.Count - 1 do
      TADOQuery(FQueryCash.Objects[i]).Free;
    FreeAndNil(FQueryCash);
  end;

  if not FWasConnected then
    FConnection.Connected := False;

  if not FIsOuterConnection then
    FreeAndNil(FConnection);

  inherited Destroy;
end;

procedure TDBConnector.Disconnect;
begin
  if FConnection.Connected then
    FConnection.Connected := False;
end;

function TDBConnector.GetLastLogID: Integer;
begin
  Result := GetMaxTableID('sys_log');
end;

function TDBConnector.GetMaxTableID(const ATableName: string): Integer;
var
  vDataset: TDataset;
begin
  //'select max(id) as id from %s'; - жуткие тормоза на MSSQL
  vDataset := SQLOpen(Format('select id from %s order by id desc', [ATableName]), []);
  try
    if not vDataset.Eof then
      Result := vDataset.FieldByName('id').AsInteger
    else
      Result := 0;
  finally
    ReleaseDataset(vDataset);
  end;
end;

function TDBConnector.GetServerTime: TDateTime;
begin
  Result := SysUtils.Now;
end;

function TDBConnector.GetUniqueCodes(const ATableName: string; const ACount: Integer): Integer;
begin
  raise Exception.Create('Вызов абстрактного метода');
end;

function TDBConnector.GetUniqueIDs(const ATableName: string; const ACount: Integer): Integer;
begin
  raise Exception.Create('Вызов абстрактного метода');
end;

function TDBConnector.InternalExecQuery(const AQuery: TADOQuery; const ACount: Integer = 0): Integer;
begin
  try
    Result := AQuery.ExecSQL;
  except
    on E: EOleException do
    begin
      if (E.ErrorCode = -2147467259) and (ACount < 1) then
      begin
        Disconnect;
        Connect;
        Result := InternalExecQuery(AQuery, ACount + 1);
      end
      else
        raise;
    end
    else
      raise;
  end;
end;

procedure TDBConnector.InternalOpenQuery(var AQuery: TADOQuery; const ACount: Integer = 0);
begin
  try
    AQuery.Open;
  except
    on E: EOleException do
    begin
      if (E.ErrorCode = -2147467259) and (ACount < 1) then
      begin
        Disconnect;
        Connect;
        InternalOpenQuery(AQuery, ACount + 1);
      end
      else begin
        FreeAndNil(AQuery);
        raise;
      end;
    end
    else begin
      FreeAndNil(AQuery);
      raise;
    end;
  end;
end;

function TDBConnector.InTransaction: Boolean;
begin
  Result := FConnection.InTransaction;
end;

procedure TDBConnector.LoadData(const ASQL: string; const AGetDBParameterFunc: TGetDBParameterFunc;
  const AOnDataLoad: TFetchDataFunc);
var
  vConnectionData: TConnectionData;
begin
  vConnectionData := TConnectionData.Create(Self);
  vConnectionData.Load(ASQL, AOnDataLoad);
end;

function TDBConnector.NewUniqueCodes(const ATableName: string;
  const ACount: Integer): Integer;
begin
  Result := GetUniqueCodes(ATableName, ACount);
end;

function TDBConnector.NewUniqueIDs(const ATableName: string;
  const ACount: Integer): Integer;
begin
  Result := GetUniqueIDs(ATableName, ACount);
end;

function TDBConnector.Now: TDateTime;
begin
  Result := GetServerTime;
end;

procedure TDBConnector.ReleaseDataset(var ADataset: TDataset);
begin
  ADataset.Free;
end;

function TDBConnector.RetrieveQuery(const ASQL: string): TADOQuery;
var
  vQueryIndex: Integer;
begin
  vQueryIndex := FQueryCash.IndexOf(ASQL);
  if vQueryIndex < 0 then
  begin
    Result := TADOQuery.Create(nil);
    Connect;
    Result.Connection := FConnection;
    Result.SQL.Text := ASQL;
    Result.Prepared := True;
    FQueryCash.AddObject(ASQL, Result);
  end
  else
    Result := TADOQuery(FQueryCash.Objects[vQueryIndex]);
end;

function TDBConnector.RollbackTrans: Integer;
begin
  FTransCount := FTransCount - 1;

  if FTransCount = 0 then
    FConnection.RollbackTrans;

  Result := FTransCount;
end;

procedure TDBConnector.SetupCustomParams(const AQuery: TADOQuery;
  const AGetDBParameterFunc: TGetDBParameterFunc);
var
  i: Integer;
  vParameter: TBaseParameter;
  vStream: TStream;
begin
  for i := 0 to AQuery.Parameters.Count - 1 do
  begin
    vParameter := AGetDBParameterFunc(AQuery.Parameters[i].Name);
    if vParameter.ParamKind = pkSimple then
      AQuery.Parameters[i].Value := TSimpleParameter(vParameter).Value
    else if vParameter.ParamKind = pkBlob then
    begin
      vStream := TBlobParameter(vParameter).Value;
      if Assigned(vStream) then
        AQuery.Parameters[i].LoadFromStream(vStream, ftBlob)
      else
        AQuery.Parameters[i].Value := Null;
    end
    else
      Assert(False, 'Unhandled parameter''s type');
  end;
end;

procedure TDBConnector.SetupParameters(const AQuery: TADOQuery;
  const AParams: array of Variant; const ABLOB: TStream = nil);
var
  i: Integer;
  vStartParameterIndex: Integer;
begin
  if Assigned(ABLOB) then
  begin
    AQuery.Parameters[0].LoadFromStream(ABLOB, ftBlob);
    vStartParameterIndex := 1;
  end
  else
    vStartParameterIndex := 0;

  for i := vStartParameterIndex to AQuery.Parameters.Count - 1 do
    if Length(AParams) > i - vStartParameterIndex then
      AQuery.Parameters[i].Value := AParams[i - vStartParameterIndex];
end;

function TDBConnector.SQLExec(const ASQL: String;
  const AParams: array of Variant; const ABLOB: TStream = nil): Integer;
var
  vQuery: TADOQuery;
begin
  vQuery := RetrieveQuery(ASQL);
  SetupParameters(vQuery, AParams, ABLOB);
  Result := InternalExecQuery(vQuery);
end;

function TDBConnector.SQLOpen(const ASQL: string;
  const AParams: array of Variant; const ABLOB: TStream = nil): TDataSet;
var
  vQuery: TADOQuery;
begin
  vQuery := TADOQuery.Create(nil);
  Connect;
  vQuery.Connection := FConnection;
  TuneQuery(vQuery);
  vQuery.SQL.Text := ASQL;

  SetupParameters(vQuery, AParams, ABLOB);

  InternalOpenQuery(vQuery);
  Result := vQuery;
end;

function TDBConnector.SQLExec(const ASQL: string;
  const AGetDBParameterFunc: TGetDBParameterFunc): Integer;
var
  vQuery: TADOQuery;
begin
  vQuery := RetrieveQuery(ASQL);
  SetupCustomParams(vQuery, AGetDBParameterFunc);
  Result := InternalExecQuery(vQuery);
end;

function TDBConnector.SQLOpen(const ASQL: string;
  const AGetDBParameterFunc: TGetDBParameterFunc): TDataset;
var
  vQuery: TADOQuery;
begin
  vQuery := TADOQuery.Create(nil);
  Connect;
  vQuery.Connection := FConnection;
  TuneQuery(vQuery);
  vQuery.SQL.Text := ASQL;

  SetupCustomParams(vQuery, AGetDBParameterFunc);

  InternalOpenQuery(vQuery);
  Result := vQuery;
end;

procedure TDBConnector.TuneQuery(const AQuery: TADOQuery);
begin
end;

class function TDBConnector.VerifyConnectionString(const AConnectionString: string): Boolean;
var
  vDBConnector: TDBConnector;
begin
  Result := AConnectionString <> '';
  if not Result then
    Exit;

  vDBConnector := TDBConnector.Create(AConnectionString);
  try
    try
      vDBConnector.Connect;
      Result := True;
    except
      Result := False;
    end;
  finally
    vDBConnector.Free;
  end;
end;

{ TSQLServerConnector }

function TSQLServerConnector.GetServerTime: TDateTime;
var
  vDataset: TDataset;
begin
  vDataset := SQLOpen('select GetDate() as date', []);
  try
    Assert(not vDataset.IsEmpty, 'Server didn''t return the timestamp');

    Result := vDataset.FieldByName('date').AsDateTime;
  finally
    ReleaseDataset(vDataset);
  end;
end;

function TSQLServerConnector.GetUniqueCodes(const ATableName: string;
  const ACount: Integer): Integer;
var
  vDataset: TDataset;
begin
  vDataset := SQLOpen('select last_code from numerators ' +
    'where table_name = :table_name', [ATableName]);
  try
    if vDataset.Eof then
    begin
      Result := 1;
      SQLExec('insert into numerators (id, table_name, last_code, last_id) ' +
        'values (:id, :table_name, :last_code, :last_id)',
        [GetMaxTableID('numerators') + 1, ATableName, Result, 0]);
    end
    else begin
      Result := vDataset.FieldByName('last_code').AsInteger + 1;

      SQLExec('update numerators set last_code = :last_code ' +
        'where table_name = :table_name', [Result, ATableName]);
    end;
  finally
    ReleaseDataset(vDataset);
  end;
end;

function TSQLServerConnector.GetUniqueIDs(const ATableName: string;
  const ACount: Integer): Integer;
var
  vDataset: TDataset;
  vMaxID: Integer;
begin
  vMaxID := GetMaxTableID(ATableName);

  vDataset := SQLOpen('select last_id from numerators ' +
    'where table_name = :table_name', [ATableName]);
  try
    if vDataset.Eof then
    begin
      if vMaxID > 0 then
        Result := vMaxID + ACount
      else
        Result := ACount;
      SQLExec('insert into numerators (id, table_name, last_id, last_code) ' +
        'values (:id, :table_name, :last_id, :last_code)',
        [GetMaxTableID('numerators') + 1, ATableName, Result, 0]);
    end
    else begin
      Result := vDataset.FieldByName('last_id').AsInteger;
      if vMaxID > Result then
        Result := vMaxID + ACount
      else
        Result := Result + ACount;

      SQLExec('update numerators set last_id = :last_id ' +
        'where table_name = :table_name', [Result, ATableName]);
    end;
  finally
    ReleaseDataset(vDataset);
  end;
end;

procedure TSQLServerConnector.TuneQuery(const AQuery: TADOQuery);
begin
  AQuery.CursorType := ctOpenForwardOnly;
  AQuery.CursorLocation := clUseClient;
end;

{ TAccessConnector }

function TAccessConnector.GetUniqueIDs(const ATableName: string;
  const ACount: Integer): Integer;
var
  vDataset: TDataset;
  vMaxID: Integer;
begin
  vMaxID := GetMaxTableID(ATableName);

  vDataset := SQLOpen('select last_id from numerators ' +
    'where table_name = :table_name', [ATableName]);
  try
    if vDataset.Eof then
    begin
      if vMaxID > 0 then
        Result := vMaxID + ACount
      else
        Result := ACount;
      SQLExec('insert into numerators (table_name, last_id) values (:table_name, :last_id)',
        [ATableName, Result]);
    end
    else begin
      Result := vDataset.FieldByName('last_id').AsInteger;
      if vMaxID > Result then
        Result := vMaxID + ACount
      else
        Result := Result + ACount;

      SQLExec('update numerators set last_id = :last_id ' +
        'where table_name = :table_name', [Result, ATableName]);
    end;
  finally
    ReleaseDataset(vDataset);
  end;
end;

procedure TAccessConnector.TuneQuery(const AQuery: TADOQuery);
begin
  AQuery.CursorType := ctOpenForwardOnly;
  AQuery.CursorLocation := clUseServer;
end;

function TAccessConnector.GetUniqueCodes(const ATableName: string;
  const ACount: Integer): Integer;
var
  vDataset: TDataset;
begin
  vDataset := SQLOpen('select last_code from numerators ' +
    'where table_name = :table_name', [ATableName]);
  try
    if vDataset.Eof then
    begin
      Result := ACount;
      SQLExec('insert into numerators (table_name, last_code) values (:table_name, :last_code)',
        [ATableName, Result]);
    end
    else begin
      Result := vDataset.FieldByName('last_code').AsInteger + ACount;

      SQLExec('update numerators set last_code = :last_code ' +
        'where table_name = :table_name', [Result, ATableName]);
    end;
  finally
    ReleaseDataset(vDataset);
  end;
end;

{ TConnectionData }

constructor TConnectionData.Create(const ADBConnector: TDBConnector);
begin
  inherited Create;

  Assert(False, 'Убрать утечки памяти');
  FDBConnector := ADBConnector;

  FConnection := TADOConnection.Create(nil);
  FConnection.LoginPrompt := False;
  FConnection.KeepConnection := True;
  FConnection.ConnectionString := ADBConnector.FConnectionString;
  FConnection.ConnectOptions := coAsyncConnect;
  FConnection.OnExecuteComplete := ExecuteComplete;

  FQuery := TADOQuery.Create(nil);
  FQuery.Connection := FConnection;
  FQuery.ExecuteOptions := [eoAsyncExecute, eoAsyncFetchNonBlocking];
  FQuery.OnFetchProgress := FetchProgress;
  FQuery.OnFetchComplete := FetchComplete;
  FQuery.CursorType := ctOpenForwardOnly;
  //FQuery.CursorLocation := clUseServer;

  FFieldDict := TDictionary<string, Integer>.Create;
end;

destructor TConnectionData.Destroy;
begin
  FreeAndNil(FFieldDict);

  FOnLoadData := nil;

  if Assigned(FQuery) then
  begin
    if Assigned(FQuery.Recordset) and ((FQuery.Recordset.State and adStateExecuting) <> 0) then
      //Отменяем выполнение запроса.
      FQuery.Recordset.Cancel;
    FreeAndNil(FQuery);
  end;

  //Удаление соединения.
  if Assigned(FConnection) then
    FreeAndNil(FConnection);

  inherited Destroy;
end;

procedure TConnectionData.ExecuteComplete(Connection: TADOConnection;
  RecordsAffected: Integer; const Error: Error; var EventStatus: TEventStatus;
  const Command: _Command; const Recordset: _Recordset);
begin
  //Если произошла ошибка, то нужно её показать.
  if EventStatus = TEventStatus.esErrorsOccured then
  begin
    //Показываем, что процесс загрузки завершён.
    //ShowProgress(false);
    //Запрашиваем здесь состояние, но ошибку не показываем, если она происходит.
    //Если здесь не запросить состояние, то будут ошибки при закрытии формы.
    try
     FQuery.RecordsetState;
    except
    end;
    //Если ошибка - это не отмена, то показываем ошибку.
    //if Error.Number <> adErrOperationCancelled then
    //    Application.MessageBox(PWideChar(Error.Description),
    //        PWideChar(Application.Title), MB_ICONERROR);
  end;
end;

procedure TConnectionData.FetchComplete(DataSet: TCustomADODataSet; const Error: Error; var EventStatus: TEventStatus);
begin
  HandleFetchProgress(DataSet, 100, 100);

  HandleFetchComplete(Dataset);
end;

procedure TConnectionData.FetchProgress(DataSet: TCustomADODataSet; Progress, MaxProgress: Integer;
  var EventStatus: TEventStatus);
begin
  HandleFetchProgress(DataSet, Progress, MaxProgress);
end;

procedure TConnectionData.HandleFetchComplete(DataSet: TCustomADODataSet);
begin
  //Если была ошибка, то показываем её.
  if not FError.IsEmpty then
  begin
    //Если выборка данных продолжает происходить, то удаляем объекты TADOQuery и TADOConnection.
    if Assigned(FQuery) and ((FQuery.Recordset.State and adStateFetching) <> 0) then
      try
        FreeAndNil(FQuery);
      finally
        FreeAndNil(FConnection);
      end;
  end
end;

procedure TConnectionData.HandleFetchProgress(DataSet: TCustomADODataSet; Progress, MaxProgress: Integer);
var
  i: integer;
begin
  try
    if (FError = '') and Assigned(FQuery) then
    begin
      if (FQuery.Recordset.RecordCount > FCount) and (FRecordsetBookmark > 1) then
      begin
        //Сдвигаемся на одну запись, после текущей.
        FQuery.Recordset.Move(1, FRecordsetBookmark);
        //Сохраняем закладку для текущей записи.
        FRecordsetBookmark := FQuery.Recordset.Bookmark;
      end;

      if FQuery.Recordset.RecordCount > FCount then
      begin
        //Создаём массив со списком индексов полей.
        if VarIsNull(FFields) then
        begin
          FFields := VarArrayCreate([0, FQuery.Recordset.Fields.Count - 1], varVariant);
          for i := 0 to FQuery.Recordset.Fields.Count - 1 do
          begin
            FFields[i] := i;
            FFieldDict.Add(FQuery.Recordset.Fields[i].Name, i);
          end;
        end;
        //Выгружаем значения полей для всех появившихся записей в массив.
        FRows := FQuery.Recordset.GetRows(FQuery.Recordset.RecordCount - FCount,
          FRecordsetBookmark, FFields);
        //Идём на последнюю запись и сохраняем для неё закладку.
        FQuery.Recordset.MoveLast;
        FRecordsetBookmark := FQuery.Recordset.Bookmark;
        FCount := FQuery.Recordset.RecordCount;

        FOnLoadData(FFieldDict, FRows);
      end;
    end;
  except
    on E: Exception do
    begin
      //Если во время копирования происходит ошибка, то сохраняем текст ошибки...
      FError := E.Message;
      //... и отправляем самим себе сообщение, что загрузка завершена.
      HandleFetchComplete(Dataset);
    end;
  end;
end;

procedure TConnectionData.Load(const ASQL: string; const AOnLoadData: TFetchDataFunc);
begin
  FQuery.SQL.Text := ASQL;
  //Устанавливаем закладку в начало.
  FError := '';
  FRecordsetBookmark := 1;
  FFields := Null;
  FFieldDict.Clear;
  FCount := 0;
  FOnLoadData := AOnLoadData;
  //Выполняем запрос.
  FQuery.Open;
end;

initialization

  CoInitialize(nil);

finalization

  CoUninitialize;

end.
