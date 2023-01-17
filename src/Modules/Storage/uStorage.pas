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

unit uStorage;

interface

uses
  Classes, Variants, IniFiles, uModule, uLogger, {XMLIntf, }uConsts, uJSON;

type
  TStorage = class;

  TStorageFunc = reference to procedure(const AStorage: TStorage);

  TStorage = class(TDomainModule)
  protected
    FLogger: TLogger;
    FVersion: string;
    FItemDefsList: TStrings;
  private
    function GetVersion: string;
  protected
    procedure DoWriteValue(const ATag: string; const AFieldKind: TFieldKind; const AValue: Variant;
      const AIsKey: Boolean = False); virtual; abstract;
    function DoReadValue(const ATag: string; const AFieldKind: TFieldKind): Variant; virtual; abstract;
    procedure DoWriteStream(const ATag: string; const AStream: TStream); virtual; abstract;
    function DoReadStream(const ATag: string): TStream; virtual; abstract;
    procedure DoReadGroup(const ATag: string; const AReadFunc: TStorageFunc); virtual; abstract;
    function DoSyncGroupDef(const ATag: string; const ASyncFunc:
      TStorageFunc): Boolean; virtual; abstract;
    procedure DoWriteItem(const AWriteFunc: TStorageFunc;
      const AID, ALogID: Integer; const ASaveAction: TEntitySaveAction); virtual; abstract;
    procedure DoSyncItemDef(const ATag: string; const AKind: TFieldKind;
      const ASize: Integer); virtual; abstract;
    function DoGetVersion: string; virtual; abstract;
    procedure DoSetVersion(const Value: string); virtual; abstract;

    function DoCreateIDs(const ATag: string; const ACount: Integer): Integer; virtual;
    function DoCreateCodes(const ATag: string; const ACount: Integer): Integer; virtual;
    function DoGetLastLogID: Integer; virtual;
    function DoGetChanges(const ALogID: Integer): TJSONObject; virtual;

    procedure DoActivate(const ATag: string); virtual; abstract;
    procedure DoDeactivate; virtual; abstract;
    procedure DoConnect; virtual; abstract;
    procedure DoDisconnect; virtual; abstract;
    procedure DoRebuild; virtual;
    // Работа с транзакциями
    function DoBeginTransaction: Integer; virtual;
    procedure DoCommitTransaction(const ATransactionID: Integer = -1); virtual;
    procedure DoRollbackTransaction(const ATransactionID: Integer = -1); virtual;
  public
    constructor Create(const ADomain: TObject; const AName: string); override;
    destructor Destroy; override;

    procedure WriteValue(const ATag: string; const AFieldKind: TFieldKind; const AValue: Variant;
      const AIsKey: Boolean = False);
    function ReadValue(const ATag: string; const AFieldKind: TFieldKind): Variant;
    procedure WriteStream(const ATag: string; const AStream: TStream);
    function ReadStream(const ATag: string): TStream;
    procedure ReadGroup(const ATag: string; const AReadFunc: TStorageFunc);
    function SyncGroupDef(const ATag: string; const ASyncFunc:
      TStorageFunc): Boolean;

    procedure AddItem(const AWriteFunc: TStorageFunc; const AID, ALogID: Integer);
    procedure UpdateItem(const AWriteFunc: TStorageFunc; const AID, ALogID: Integer);
    procedure DeleteItem(const AID: Integer);
    procedure ReadItem(const AReadFunc: TStorageFunc);
    procedure SyncItemDef(const ATag: string; const AKind: TFieldKind;
      const ASize: Integer);

    procedure Connect;
    procedure Disconnect;
    procedure Rebuild;

    function GetTime: TDateTime; virtual;
    function CreateIDs(const ATag: string; const ACount: Integer = 1): Integer;
    function CreateCodes(const ATag: string; const ACount: Integer = 1): Integer;
    procedure UpdateNumerator(const ATag: string; const ALastID: Integer); virtual;

    // Запоминаем таблицу, в которую будем писать изменения
    procedure Activate(const ATag: string);
    procedure Deactivate;

    function BeginTransaction: Integer;
    procedure CommitTransaction(const ATransactionID: Integer = -1);
    procedure RollbackTransaction(const ATransactionID: Integer = -1);

    // Wide API
    function GetLastLogID: Integer;
    function GetChanges(const ALogID: Integer): TJSONObject;
    property Version: string read GetVersion write DoSetVersion;
  end;

  TIniStorage = class(TStorage)
  private
    FFileName: string;
    FTransactionFileName: string;
    FIniFile: TIniFile;
    FInTransaction: Boolean;
    FCurrentSection: string;
    FCurrentID: Integer;
    function BuildKeyName(const ATag: string): string;
  protected
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

    // Работа с транзакциями
    function DoBeginTransaction: Integer; override;
    procedure DoCommitTransaction(const ATransactionID: Integer = -1); override;
    procedure DoRollbackTransaction(const ATransactionID: Integer = -1); override;
  public
    constructor Create(const ADomain: TObject; const AName: string); override;
    destructor Destroy; override;

    procedure UpdateNumerator(const ATag: string; const ALastID: Integer); override;
  end;

 { TXMLStorage = class(TStorage)
  private
    FFileName: string;

    FXMLDocument: IXMLDocument;
    FXMLRootNode: IXMLNode;

    FGroupList: TList;
    FNumerators: TStringList;
    FCodeNumerators: TStringList;

    function GetActiveNode: IXMLNode;
    procedure Push(const ANode: IXMLNode);
    procedure Pop;
    procedure UpdateNumerator(const ATag: string; const AID: Integer);
    function GetNodeByID(const AID: Integer): IXMLNode;
  protected
    procedure DoWriteValue(const ATag: string; const AValue: Variant;
      const AIsKey: Boolean = False); override;
    function DoReadValue(const ATag: string): Variant; override;
    procedure DoWriteStream(const ATag: string; const AStream: TStream); override;
    function DoReadStream(const ATag: string): TStream; override;
    procedure DoReadGroup(const ATag: string; const AReadFunc: TStorageFunc); override;
    function DoSyncGroupDef(const ATag: string; const ASyncFunc:
      TStorageFunc): Boolean; override;

    procedure DoWriteItem(const AWriteFunc: TStorageFunc;
      const AID: Integer; const ASaveAction: TEntitySaveAction); override;
    procedure DoSyncItemDef(const ATag: string; const AKind: TFieldKind;
      const ASize: Integer); override;

    function DoCreateID(const ATag: string): Integer; override;
    function DoCreateCode(const ATag: string): Integer; override;
    //function DoGetLastLogID: Integer; override;
    //function DoGetChanges(const ALogID: Integer): string; override;

    procedure DoActivate(const ATag: string); override;
    procedure DoDeactivate; override;
  public
    constructor Create(const ACore: TObject; const AIDName: string;
      const AFileName: string; const AVersion: string = '');
    destructor Destroy; override;
  end;
      }

implementation

uses
  RegularExpressions, Math, SysUtils, IOUtils, uDomain, uSettings, uUtils;

{ TStorage }

procedure TStorage.Activate(const ATag: string);
begin
  DoActivate(ATag);
end;

procedure TStorage.AddItem(const AWriteFunc: TStorageFunc; const AID, ALogID: Integer);
begin
  DoWriteItem(AWriteFunc, AID, ALogID, esaInsert);
end;

procedure TStorage.CommitTransaction(const ATransactionID: Integer);
begin
  DoCommitTransaction(ATransactionID);
end;

procedure TStorage.Connect;
begin
  DoConnect;
end;

constructor TStorage.Create(const ADomain: TObject; const AName: string);
begin
  inherited Create(ADomain, AName);

  FLogger := TDomain(ADomain).Logger;
  FVersion := '~';
  FItemDefsList := TStringList.Create;
end;

function TStorage.CreateCodes(const ATag: string; const ACount: Integer): Integer;
begin
  Result := DoCreateCodes(ATag, ACount);
end;

function TStorage.CreateIDs(const ATag: string; const ACount: Integer): Integer;
begin
  Result := DoCreateIDs(ATag, ACount);
end;

procedure TStorage.Deactivate;
begin
  DoDeactivate;
end;

procedure TStorage.DeleteItem(const AID: Integer);
begin
  DoWriteItem(nil, AID, 0, esaDelete);
end;

destructor TStorage.Destroy;
begin
  FreeAndNil(FItemDefsList);
  inherited Destroy;
end;

procedure TStorage.Disconnect;
begin
  DoDisconnect;
end;

procedure TStorage.DoCommitTransaction(const ATransactionID: Integer);
begin
end;

function TStorage.DoCreateCodes(const ATag: string; const ACount: Integer): Integer;
begin
  Result := -1;
end;

function TStorage.DoCreateIDs(const ATag: string; const ACount: Integer): Integer;
begin
  Result := -1;
end;

function TStorage.DoGetChanges(const ALogID: Integer): TJSONObject;
begin
  Result := nil;
end;

function TStorage.DoGetLastLogID: Integer;
begin
  Result := -1;
end;

procedure TStorage.DoRebuild;
begin
end;

procedure TStorage.DoRollbackTransaction(const ATransactionID: Integer);
begin
end;

function TStorage.DoBeginTransaction: Integer;
begin
  Result := -1;
end;

function TStorage.GetChanges(const ALogID: Integer): TJSONObject;
begin
  Result := DoGetChanges(ALogID);
end;

function TStorage.GetLastLogID: Integer;
begin
  Result := DoGetLastLogID;
end;

function TStorage.GetTime: TDateTime;
begin
  Result := Now;
end;

function TStorage.GetVersion: string;
begin
  if FVersion = '~' then
    FVersion := DoGetVersion;
  Result := FVersion;
end;

procedure TStorage.WriteStream(const ATag: string; const AStream: TStream);
begin
  DoWriteStream(ATag, AStream);
end;

procedure TStorage.ReadItem(const AReadFunc: TStorageFunc);
begin
  if Assigned(AReadFunc) then
    AReadFunc(Self);
end;

function TStorage.ReadStream(const ATag: string): TStream;
begin
  Result := DoReadStream(ATag);
end;

procedure TStorage.ReadGroup(const ATag: string; const AReadFunc: TStorageFunc);
begin
  DoReadGroup(ATag, AReadFunc);
end;

function TStorage.BeginTransaction: Integer;
begin
  Result := DoBeginTransaction;
end;

function TStorage.SyncGroupDef(const ATag: string;
  const ASyncFunc: TStorageFunc): Boolean;
begin
  FItemDefsList.Clear;
  Result := DoSyncGroupDef(ATag, ASyncFunc);
end;

procedure TStorage.SyncItemDef(const ATag: string; const AKind: TFieldKind;
  const ASize: Integer);
begin
  FItemDefsList.Add(ATag);
  DoSyncItemDef(ATag, AKind, ASize);
end;

procedure TStorage.UpdateItem(const AWriteFunc: TStorageFunc;
  const AID, ALogID: Integer);
begin
  DoWriteItem(AWriteFunc, AID, ALogID, esaUpdate);
end;

procedure TStorage.UpdateNumerator(const ATag: string; const ALastID: Integer);
begin
end;

procedure TStorage.WriteValue(const ATag: string; const AFieldKind: TFieldKind; const AValue: Variant;
  const AIsKey: Boolean = False);
begin
  DoWriteValue(ATag, AFieldKind, AValue, AIsKey);
end;

function TStorage.ReadValue(const ATag: string; const AFieldKind: TFieldKind): Variant;
begin
  Result := DoReadValue(ATag, AFieldKind);
end;

procedure TStorage.Rebuild;
begin
  DoRebuild;
end;

procedure TStorage.RollbackTransaction(const ATransactionID: Integer);
begin
  DoRollbackTransaction(ATransactionID);
end;

{ TXMLStorage }
  {
constructor TXMLStorage.Create(const ACore: TObject; const AIDName: string;
  const AFileName: string; const AVersion: string = '');
begin
  inherited Create(ACore, AIDName);

    if not FSettings.SectionExists('XML') then
      FSettings.SetValue('XML', 'FileName', 'database.xml');
    vDatabase := FSettings.GetValue('XML', 'FileName', 'database.xml');
    //Result := vStorageClass.Create(Self, vStorageName, vVersion, vDatabase)

  FFileName := AFileName;
  FNumerators := TStringList.Create;
  FCodeNumerators := TStringList.Create;

  if FileExists(FFileName) then
    FXMLDocument := TXMLDocument.Create(FFileName)
  else
    FXMLDocument := NewXMLDocument;

  FXMLDocument.Active := True;
  FXMLRootNode := FXMLDocument.ChildNodes['Root'];
  FXMLRootNode.Attributes['IDName'] := AIDName;
  FXMLRootNode.Attributes['Version'] := AVersion;

  FGroupList := TList.Create;

  Push(FXMLRootNode);
end;

destructor TXMLStorage.Destroy;
begin
  Pop;
  FGroupList.Free;

  FXMLDocument.SaveToFile(FFileName);

  FXMLDocument.Active := False;

  inherited Destroy;
end;

procedure TXMLStorage.DoActivate(const ATag: string);
var
  vGroupNode: IXMLNode;
begin
  vGroupNode := FXMLRootNode.ChildNodes.FindNode(ATag);
  if not Assigned(vGroupNode) then
    vGroupNode := GetActiveNode.ChildNodes[ATag];

  Push(vGroupNode);
end;

function TXMLStorage.DoCreateCode(const ATag: string): Integer;
var
  vIndex: Integer;
begin
  vIndex := FCodeNumerators.IndexOf(ATag);
  if vIndex < 0 then
  begin
    Result := 1;
    FCodeNumerators.AddObject(ATag, TObject(1));
  end
  else begin
    Result := Integer(FCodeNumerators.Objects[vIndex]) + 1;
    FCodeNumerators.Objects[vIndex] := TObject(Result);
  end;
end;

function TXMLStorage.DoCreateID(const ATag: string): Integer;
var
  vIndex: Integer;
begin
  vIndex := FNumerators.IndexOf(ATag);
  if vIndex < 0 then
  begin
    Result := 1;
    FNumerators.AddObject(ATag, TObject(1));
  end
  else begin
    Result := Integer(FNumerators.Objects[vIndex]) + 1;
    FNumerators.Objects[vIndex] := TObject(Result);
  end;
end;

procedure TXMLStorage.DoDeactivate;
begin
  Pop;
end;

procedure TXMLStorage.DoReadGroup(const ATag: string;
  const AReadFunc: TStorageFunc);
var
  vGroupNode: IXMLNode;
  vNode: IXMLNode;
  vId: Integer;
  i: Integer;
begin
  vGroupNode := FXMLRootNode.ChildNodes.FindNode(ATag);

  if not Assigned(vGroupNode) then
    Exit;

  Push(vGroupNode);
  try
    for i := 0 to vGroupNode.ChildNodes.Count - 1 do
    begin
      // Обновление нумератора
      vNode := vGroupNode.ChildNodes[i];
      if vNode.AttributeNodes.IndexOf('id') >= 0 then
      begin
        vId := vNode.Attributes['id'];
        UpdateNumerator(ATag, vId);
      end;

      Push(vNode);
      try
        if Assigned(AReadFunc) then
          AReadFunc(Self);
      finally
        Pop;
      end;
    end;
  finally
    Pop;
  end;
end;

function TXMLStorage.DoReadStream(const ATag: string): TStream;
var
  vInputStr: string;
begin
  Result := nil;

  if not GetActiveNode.HasAttribute(ATag) then
    Exit;

  vInputStr := Trim(GetActiveNode.Attributes[ATag]);
  if vInputStr = '' then
    Exit;

  Result := TStringStream.Create(DecodeBase64(vInputStr));
end;

function TXMLStorage.DoReadValue(const ATag: string): Variant;
begin
  if GetActiveNode.HasAttribute(ATag) then
    Result := GetActiveNode.Attributes[ATag]
  else
    Result := Null;
end;

procedure TXMLStorage.DoSyncGroupDef(const ATag: string;
  const ASyncFunc: TStorageFunc);
begin
end;

procedure TXMLStorage.DoSyncItemDef(const ATag: string; const AKind: TFieldKind;
  const ASize: Integer);
begin
end;

procedure TXMLStorage.DoWriteItem(const AWriteFunc: TStorageFunc;
  const AID: Integer; const ASaveAction: TEntitySaveAction);
var
  vNode: IXMLNode;
begin
  vNode := GetNodeByID(AID);
  if ASaveAction = esaInsert then
    vNode := GetActiveNode.AddChild('Item')
  else if ASaveAction = esaUpdate then
  begin
    if Assigned(vNode) then
      vNode.AttributeNodes.Clear
    else
      vNode := GetActiveNode.AddChild('Item');
  end
  else if ASaveAction = esaDelete then
  begin
    if Assigned(vNode) then
      GetActiveNode.ChildNodes.Remove(vNode);
    Exit;
  end;

  vNode.Attributes['id'] := AID;
  Push(vNode);
  try
    if Assigned(AWriteFunc) then
      AWriteFunc(Self);
  finally
    Pop;
  end;
end;

procedure TXMLStorage.DoWriteStream(const ATag: string; const AStream: TStream);
var
  vOutput: TStringStream;
begin
  if Assigned(AStream) then
  begin
    vOutput := TStringStream.Create('');
    try
      vOutput.CopyFrom(AStream, AStream.Size);
      vOutput.Position := 0;
      GetActiveNode.Attributes[ATag] := EncodeBase64(vOutput.DataString);
    finally
      vOutput.Free;
    end;
  end;
end;

procedure TXMLStorage.DoWriteValue(const ATag: string;
  const AValue: Variant; const AIsKey: Boolean = False);
begin
  GetActiveNode.Attributes[ATag] := AValue;
end;

function TXMLStorage.GetActiveNode: IXMLNode;
begin
  Result := IXMLNode(FGroupList[FGroupList.Count - 1]);
end;

function TXMLStorage.GetNodeByID(const AID: Integer): IXMLNode;
var
  vNode: IXMLNode;
  i: Integer;
begin
  vNode := GetActiveNode;

  for i := 0 to vNode.ChildNodes.Count - 1 do
  begin
    Result := vNode.ChildNodes[i];
    if Result.Attributes['id'] = AID then
      Exit;
  end;

  Result := nil
end;

procedure TXMLStorage.Pop;
begin
  FGroupList.Delete(FGroupList.Count - 1);
end;

procedure TXMLStorage.Push(const ANode: IXMLNode);
begin
  FGroupList.Add(Pointer(ANode));
end;

procedure TXMLStorage.UpdateNumerator(const ATag: string; const AID: Integer);
var
  vIndex: Integer;
  vMaxID: Integer;
begin
  vIndex := FNumerators.IndexOf(ATag);
  if vIndex < 0 then
    FNumerators.AddObject(ATag, TObject(1))
  else begin
    vMaxID := Integer(FNumerators.Objects[vIndex]);
    if vMaxID < AID then
      FNumerators.Objects[vIndex] := TObject(AID);
  end;
end;
     }

{ TIniStorage }

function TIniStorage.BuildKeyName(const ATag: string): string;
begin
  Result := Format('Item$%d$%s', [FCurrentID, ATag]);
end;

constructor TIniStorage.Create(const ADomain: TObject; const AName: string);
var
  vSettings: TSettings;
  vDBName: string;
begin
  inherited Create(ADomain, AName);

  vSettings := TDomain(ADomain).Settings;
  if vSettings.SectionExists(AName) then
    vDBName := vSettings.GetValue(AName, 'FileName', 'database.ini')
  else
    vDBName := vSettings.GetValue('INI', 'FileName', 'database.ini');
  if not vSettings.KeyExists(AName, 'FileName') then
    vSettings.SetValue(AName, 'FileName', vDBName);

  FFileName := TPath.Combine(TDomain(ADomain).Configuration.ConfigurationDir, vDBName);

  FInTransaction := False;
  FTransactionFileName := ChangeFileExt(FFileName, '.~trn');
  FCurrentSection := '';
  FCurrentID := 0;
end;

destructor TIniStorage.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TIniStorage.DoActivate(const ATag: string);
begin
  FCurrentSection := ATag;
end;

function TIniStorage.DoBeginTransaction: Integer;
begin
  Result := -1;
  try
    TFile.Copy(FFileName, FTransactionFileName, True);
    FInTransaction := True;
  except
    FInTransaction := False;
  end;
end;

procedure TIniStorage.DoCommitTransaction(const ATransactionID: Integer);
begin
  FInTransaction := False;
end;

procedure TIniStorage.DoConnect;
begin
  if not FileExists(FFileName) then
  begin
    with TStringList.Create do
    begin
      SaveToFile(FFileName);
      Free;
    end;
  end;

  try
    FIniFile := TIniFile.Create(FFileName);
  except
    on E: Exception do
    begin
      FLogger.AddMessage('Error: ' + E.Message);
      raise;
    end;
  end;
end;

function TIniStorage.DoCreateCodes(const ATag: string; const ACount: Integer): Integer;
var
  vLastCode: Integer;
begin
  if FIniFile.SectionExists(ATag) then
    vLastCode := FIniFile.ReadInteger(ATag, 'LastCode', 0)
  else
    vLastCode := 0;

  Result := vLastCode + ACount;

  FIniFile.WriteInteger(ATag, 'LastCode', Result);
end;

function TIniStorage.DoCreateIDs(const ATag: string; const ACount: Integer): Integer;
var
  vLastID: Integer;
begin
  if FIniFile.SectionExists(ATag) then
    vLastID := FIniFile.ReadInteger(ATag, 'LastID', 0)
  else
    vLastID := 0;

  Result := vLastID + ACount;

  FIniFile.WriteInteger(ATag, 'LastID', Result);
end;

procedure TIniStorage.DoDeactivate;
begin
  FCurrentSection := '';
end;

procedure TIniStorage.DoDisconnect;
begin
  if FInTransaction then
    DoRollbackTransaction;

  FreeAndNil(FIniFile);
end;

function TIniStorage.DoGetChanges(const ALogID: Integer): TJSONObject;
begin
  Result := nil;
end;

function TIniStorage.DoGetLastLogID: Integer;
begin
  if FIniFile.ValueExists('sys_log', 'LastID') then
    Result := FIniFile.ReadInteger('sys_log', 'LastID', 0)
  else
    Result := 0;
end;

function TIniStorage.DoGetVersion: string;
begin
  Result := FIniFile.ReadString('Core', 'Version', '');
end;

procedure TIniStorage.DoReadGroup(const ATag: string; const AReadFunc: TStorageFunc);
var
  vAllItems: TStrings;
  i: Integer;
  vId: Integer;
  vKeyStart: string;

  function ExtractID(const s: string): Integer;
  var
    vMatch: TMatch;
  begin
    vMatch := TRegEx.Match(s, '^Item\$(\d+)\$');
    if vMatch.Success then
      Result := StrToIntDef(vMatch.Groups[1].Value, 0)
    else
      Result := 0;
  end;
begin
  if not FIniFile.SectionExists(ATag) then
    Exit;

  vAllItems := TStringList.Create;
  FIniFile.ReadSection(ATag, vAllItems);
  Activate(ATag);
  try
    while vAllItems.Count > 0 do
    begin
      vId := ExtractID(vAllItems[0]);
      if vId <= 0 then
      begin
        vAllItems.Delete(0);
        Continue;
      end;

      FCurrentID := vId;
      vKeyStart := Format('Item$%d$', [vId]);
      try
        if Assigned(AReadFunc) then
          AReadFunc(Self);
        for i := vAllItems.Count - 1 downto 0 do
          if Pos(vKeyStart, vAllItems[i]) = 1 then
            vAllItems.Delete(i);
      finally
        FCurrentID := 0;
      end;
    end;
  finally
    Deactivate;
    FreeAndNil(vAllItems);
  end;
end;

function TIniStorage.DoReadStream(const ATag: string): TStream;
var
  vKey: string;
begin
  vKey := BuildKeyName(ATag);
  if FIniFile.ValueExists(FCurrentSection, vKey) then
  begin
    Result := TMemoryStream.Create;
    FIniFile.ReadBinaryStream(FCurrentSection, vKey, Result);
  end
  else
    Result := nil;
end;

function TIniStorage.DoReadValue(const ATag: string; const AFieldKind: TFieldKind): Variant;
var
  vKey: string;
begin
  if ATag = 'id' then
  begin
    Result := FCurrentID;
    Exit;
  end;

  vKey := BuildKeyName(ATag);
  if FIniFile.ValueExists(FCurrentSection, vKey) then
  begin
    case AFieldKind of
      fkString:
      begin
        Result := FIniFile.ReadString(FCurrentSection, vKey, '');
        Result := DecodeBase64(Result);
      end;
      fkInteger, fkEnum, fkFlag: Result := FIniFile.ReadInteger(FCurrentSection, vKey, 0);
      fkColor: Result := FIniFile.ReadInteger(FCurrentSection, vKey, cNullColor);
      fkBoolean: Result := FIniFile.ReadBool(FCurrentSection, vKey, False);
      fkCurrency, fkFloat: Result := FIniFile.ReadFloat(FCurrentSection, vKey, 0);
      fkDateTime: Result := FIniFile.ReadDateTime(FCurrentSection, vKey, TDateTime(0));
    else
      Result := Null;
    end;
  end
  else
    Result := Null;
end;

procedure TIniStorage.DoRollbackTransaction(const ATransactionID: Integer);
begin
  FInTransaction := False;
  FreeAndNil(FIniFile);
  TFile.Copy(FTransactionFileName, FFileName, True);
  FIniFile := TIniFile.Create(FFileName);
end;

procedure TIniStorage.DoSetVersion(const Value: string);
begin
  FIniFile.WriteString('Core', 'Version', Value);
end;

function TIniStorage.DoSyncGroupDef(const ATag: string; const ASyncFunc: TStorageFunc): Boolean;
begin
  Result := True;
  if Assigned(ASyncFunc) then
    ASyncFunc(Self);
  // Удалить неиспользуемые поля
end;

procedure TIniStorage.DoSyncItemDef(const ATag: string; const AKind: TFieldKind; const ASize: Integer);
begin
end;

procedure TIniStorage.DoWriteItem(const AWriteFunc: TStorageFunc; const AID, ALogID: Integer;
  const ASaveAction: TEntitySaveAction);
var
  vItemValues: TStrings;
  vKeyStart: string;
  i: Integer;
begin
  FCurrentID := AID;
  vKeyStart := Format('Item$%d$', [AID]);
  try
    if ASaveAction = esaDelete then
    begin
      vItemValues := TStringList.Create;
      FIniFile.ReadSection(FCurrentSection, vItemValues);
      try
        for i := 0 to vItemValues.Count - 1 do
          if Pos(vKeyStart, vItemValues[i]) = 1 then
            FIniFile.DeleteKey(FCurrentSection, vItemValues[i]);
      finally
        FreeAndNil(vItemValues);
      end;
    end
    else begin
      DoWriteValue('log_id', fkInteger, ALogID, False);
      if Assigned(AWriteFunc) then
        AWriteFunc(Self);
    end;
  finally
    FCurrentID := 0;
  end;
end;

procedure TIniStorage.DoWriteStream(const ATag: string; const AStream: TStream);
var
  vKey: string;
begin
  vKey := BuildKeyName(ATag);
  FIniFile.WriteBinaryStream(FCurrentSection, vKey, AStream);
end;

procedure TIniStorage.DoWriteValue(const ATag: string; const AFieldKind: TFieldKind; const AValue: Variant;
  const AIsKey: Boolean);
var
  vKey: string;
  vValue: string;
begin
  vKey := BuildKeyName(ATag);

  if VarIsNull(AValue) then
  begin
    FIniFile.DeleteKey(FCurrentSection, vKey);
    Exit;
  end;

  case AFieldKind of
    fkString:
    begin
      vValue := EncodeBase64(VarToStr(AValue));
      FIniFile.WriteString(FCurrentSection, vKey, vValue);
    end;
    fkInteger, fkEnum, fkFlag, fkColor:
      FIniFile.WriteInteger(FCurrentSection, vKey, AValue);
    fkBoolean:
      FIniFile.WriteBool(FCurrentSection, vKey, AValue);
    fkCurrency, fkFloat:
      FIniFile.WriteFloat(FCurrentSection, vKey, AValue);
    fkDateTime:
      FIniFile.WriteDateTime(FCurrentSection, vKey, VarToDateTime(AValue));
  else
    // Do nothing
  end;
end;

procedure TIniStorage.UpdateNumerator(const ATag: string; const ALastID: Integer);
begin
  FIniFile.WriteInteger(ATag, 'LastID', Max(ALastID, 0));
end;

initialization

TBaseModule.RegisterModule('Storage', '', 'IniFile', TIniStorage);

end.
