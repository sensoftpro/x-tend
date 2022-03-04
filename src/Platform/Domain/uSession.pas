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

unit uSession;

interface

uses
  Classes, Generics.Collections, uEntity, uChangeManager, uEnumeration, uTask, uConsts;

type
  TUserSession = class;

  TReactionProc = procedure(const ASession: TUserSession; const AHolder: TChangeHolder;
    const AFieldChain: string; const AEntity, AParameter: TEntity);
  TReactionProcRef = reference to procedure(const ASession: TUserSession; const AHolder: TChangeHolder;
    const AFieldChain: string; const AEntity, AParameter: TEntity);

  TAtomicModificationFunc = reference to function(const AHolder: TChangeHolder): Boolean;
  TAtomicWriteProc = reference to procedure;

  TUserSession = class
  private
    [Weak] FDomain: TObject;
    { TODO -owa : Подумать: Для одной сессии может быть несколько интеракторов? }
    [Weak] FInteractor: TObject;
    [Weak] FUser: TEntity;
    FIsAdmin: Boolean;

    FHolders: TObjectList<TChangeHolder>;

    function GetCurrentUserName: string;
    procedure SetInteractor(const Value: TObject);

    procedure InternalReleaseChangeHolder(const AHolder: TChangeHolder; const AResult: Boolean;
      const ASkipLogging: Boolean = False);
  public
    constructor Create(const ADomain: TObject; const AUser: TEntity);
    destructor Destroy; override;

    // Вызывается ТОЛЬКО для явных быстрых модификаций (без EditEntity), блокирующий
    function AtomicModification(const ATask: TTaskHandle; const AModificationFunc: TAtomicModificationFunc;
      const AParentHolder: TChangeHolder = nil; const ASkipLogging: Boolean = False): Boolean;
    // Вызывается для изменений из UI редакторов, блокирующий. Холдер находится в UI
    procedure AtomicWrite(const ATask: TTaskHandle; const AWriteProc: TAtomicWriteProc);

    function Edit(const AParentHolder: TChangeHolder): TChangeHolder;
    procedure Cancel(const AHolder: TChangeHolder);
    procedure Save(const AHolder: TChangeHolder);
    procedure DomainWrite(const AWriteProc: TAtomicWriteProc);

    function RetainChangeHolder(const AParentHolder: TChangeHolder): TChangeHolder;
    procedure ReleaseChangeHolder(const AHolder: TChangeHolder; const AResult: Boolean;
      const ASkipLogging: Boolean = False);
    procedure ReloadDomainChanges(const AHolder: TChangeHolder);

    function GetUIState(const AObjectName: string; const AContextState: TState): TViewState;

    procedure CheckLocking(const AExpectedResult: Boolean = True);

    property Holders: TObjectList<TChangeHolder> read FHolders;
    property CurrentUser: TEntity read FUser;
    property CurrentUserName: string read GetCurrentUserName;
    property IsAdmin: Boolean read FIsAdmin;
    property Interactor: TObject read FInteractor write SetInteractor;
    property Domain: TObject read FDomain;
  end;

  TUserSessions = class
  private
    [Weak] FDomain: TObject;
    FItems: TObjectList<TUserSession>;
    function GetCount: Integer;
    function GetSession(const AIndex: Integer): TUserSession;
  public
    constructor Create(const ADomain: TObject);
    destructor Destroy; override;

    function AddSession(const AUser: TEntity): TUserSession;
    procedure RemoveSession(const ASession: TUserSession);
    function SessionByLogin(const ALogin: string): TUserSession;

    function GetEnumerator: TEnumerator<TUserSession>;

    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]: TUserSession read GetSession; default;
  end;

function IsUserGrantedForRole(const AUser: TEntity; const ARoleID: Integer): Boolean;
procedure AddUserRole(const AHolder: TChangeHolder; const AUser: TEntity; const ARoleID: Integer);
procedure RemoveUserRole(const AHolder: TChangeHolder; const AUser: TEntity; const ARoleID: Integer);

//procedure RegisterUserStatus(const ASession: TUserSession; const AUser: TEntity; const AOnline: Boolean);
//function GetUIState(const AUser: TEntity; const AObjectName: string; const AContextState: TState): TViewState;

implementation

uses
  SysUtils, uDomain, uObjectField, uInteractor;

procedure AddUserRole(const AHolder: TChangeHolder; const AUser: TEntity; const ARoleID: Integer);
var
  vRoles: TListField;
  vRole: TEntity;
  //vUserRole: TEntity;
begin
  if Assigned(AUser) then
  begin
    if IsUserGrantedForRole(AUser, ARoleID) then
      Exit;

    vRole := TDomain(AUser.Domain).EntityByID('SysRoles', ARoleID);
    if Assigned(vRole) then
    begin
      vRoles := TListField(AUser.FieldByName('Roles'));
      {vUserRole := }vRoles.AddListEntity(AHolder, 'SysUsersRoles', 'Role', [Integer(vRole)]);
      //vUserRole._SetFieldEntity(AHolder, 'Role', vRole);
    end;
  end;
end;

function IsUserGrantedForRole(const AUser: TEntity; const ARoleID: Integer): Boolean;
var
  vRoles: TListField;
  vRole: TEntity;
begin
  if Assigned(AUser) then
  begin
    vRoles := TListField(AUser.FieldByName('Roles'));
    for vRole in vRoles do
    begin
      Result := SafeID(vRole.ExtractEntity('Role')) = ARoleID;
      if Result then
        Exit;
    end;
  end;

  Result := False;
end;

procedure RemoveUserRole(const AHolder: TChangeHolder; const AUser: TEntity; const ARoleID: Integer);
var
  vRoles: TListField;
  vUserRole: TEntity;
  i: Integer;
begin
  if Assigned(AUser) then
  begin
    vRoles := TListField(AUser.FieldByName('Roles'));
    for i := vRoles.Count - 1 downto 0 do
    begin
      vUserRole := TEntity(vRoles[i]);
      if SafeID(vUserRole.ExtractEntity('Role')) = ARoleID then
        vRoles.RemoveListEntity(AHolder, vUserRole);
    end;
  end;
end;

procedure RegisterUserStatus(const ASession: TUserSession; const AUser: TEntity; const AOnline: Boolean);
begin
  ASession.AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
    begin
      AHolder.SetFieldValue(AUser, 'Online', AOnline);
      Result := True;
    end, nil, True);
end;

function GetSubjectAccessFlags(const ASubject: TEntity; const AObjectName: string;
  const AStateName: string): TViewState;
var
  vList: TListField;
  vSecuredObject: TEntity;
  i: Integer;
begin
  Result := vsHidden;
  if not Assigned(ASubject) then
    Exit;

  vList := TListField(ASubject.FieldByName('SecuredObjects'));
  for i := 0 to vList.Count - 1 do
  begin
    vSecuredObject := TEntity(vList[i]);
    if SameText(vSecuredObject['ObjectName'], AObjectName) then
    begin
      if (vSecuredObject['StateName'] = '') or SameText(vSecuredObject['StateName'], AStateName) then
      begin
        Result := vSecuredObject['AccessFlags'];
        if Result = 14 then
          Result := vsFullAccess;
        Exit;
      end;
    end;
  end;
  Result := vsUndefined;
end;

function GetAccessFlags(const AUser: TEntity; const AObjectName: string; const AContextState: TState): TViewState;
var
  vStateName: string;
  vRoles: TListField;
  vRole: TEntity;
  vState: TViewState;
  vPos: Integer;
  i: Integer;
begin
  if Assigned(AContextState) then
    vStateName := AContextState.Code
  else
    vStateName := '';
  Result := GetSubjectAccessFlags(AUser, AObjectName, vStateName);
  if Result <> vsUndefined then
    Exit;

  Result := vsHidden;
  vRoles := TListField(AUser.FieldByName('Roles'));
  for i := 0 to vRoles.Count - 1 do
  begin
    vRole := TEntity(vRoles[i]).ExtractEntity('Role');
    vState := vsUndefined;
    while (vState = vsUndefined) and Assigned(vRole) do
    begin
      vState := GetSubjectAccessFlags(vRole, AObjectName, vStateName);
      vRole := vRole.ExtractEntity('ParentRole');
    end;

    Result := Result or vState;
  end;

  if Result and vsUndefined <> vsUndefined then
    Exit;

  // Проверяем унаследованные права самого объекта (без поля)
  // NB: в объекте может быть только один уровень вложенности
  vPos := Pos('.', AObjectName);
  if vPos > 0 then
    Result := GetAccessFlags(AUser, Copy(AObjectName, 1, vPos - 1), AContextState)
  else begin
    // Берем флаг, установленный без привязки к защите у самого субъекта
    Result := AUser['AccessFlags'];
    if Result = 14 then
      Result := vsFullAccess;
  end;
end;

{ TUserSession }

function TUserSession.AtomicModification(const ATask: TTaskHandle; const AModificationFunc: TAtomicModificationFunc;
  const AParentHolder: TChangeHolder = nil; const ASkipLogging: Boolean = False): Boolean;
var
  vResult: Boolean;
begin
  try
    AtomicWrite(ATask, procedure
      var
        vHolder: TChangeHolder;
      begin
        vHolder := RetainChangeHolder(AParentHolder);
        try
          vResult := AModificationFunc(vHolder);
        finally
          ReleaseChangeHolder(vHolder, vResult, ASkipLogging);
        end;
      end);
    Result := vResult;
  except
    on E: Exception do
    begin
      if Assigned(FInteractor) then
        TInteractor(FInteractor).ShowMessage('Произошла ошибка!'#13#10 + E.Message);
      Result := False;
      if Assigned(FDomain) then TDomain(FDomain).Log('Произошла ошибка!'#13#10 + E.Message, mkError);
    end;
  end;
end;

procedure TUserSession.AtomicWrite(const ATask: TTaskHandle; const AWriteProc: TAtomicWriteProc);
begin
  TTaskHandle.SafeInvoke(ATask, procedure
    begin
      if Assigned(FDomain) then
        DomainWrite(AWriteProc);
    end);
end;

procedure TUserSession.Cancel(const AHolder: TChangeHolder);
begin
  DomainWrite(procedure
    begin
      ReleaseChangeHolder(AHolder, False, True);
    end);
end;

procedure TUserSession.CheckLocking(const AExpectedResult: Boolean = True);
begin
  if TDomain(FDomain).DataLock.Locked <> AExpectedResult then
  begin
    TDomain(FDomain).Logger.AddMessage('@@@ Ожидание в блокировке @@@');
    if AExpectedResult then
      Assert(False, 'Неожиданное состояние блокировки: ' + BoolToStr(not AExpectedResult));
  end;
end;

constructor TUserSession.Create(const ADomain: TObject; const AUser: TEntity);
begin
  inherited Create;
  FDomain := ADomain;
  FUser := AUser;

  FHolders := TObjectList<TChangeHolder>.Create;

  if Assigned(FUser) then
  begin
    FIsAdmin := IsUserGrantedForRole(FUser, 1);
    RegisterUserStatus(Self, FUser, True);
  end
  else
    FIsAdmin := False;
end;

destructor TUserSession.Destroy;
var
  i: Integer;
begin
  if Assigned(FUser) then
    RegisterUserStatus(Self, FUser, False);

  FDomain := nil;
  FUser := nil;
  for i := FHolders.Count - 1 downto 0 do
    FHolders[i].RevertChanges;
  FreeAndNil(FHolders);

  inherited Destroy;
end;

procedure TUserSession.DomainWrite(const AWriteProc: TAtomicWriteProc);
begin
  CheckLocking(False);
  TDomain(FDomain).DataLock.Enter(Self);
  try
    try
      AWriteProc;
    except
      on E: Exception do
      begin
        TDomain(FDomain).Logger.AddMessage(E.Message);
        raise;
      end;
    end;
  finally
    TDomain(FDomain).DataLock.Leave;
  end;
end;

function TUserSession.Edit(const AParentHolder: TChangeHolder): TChangeHolder;
var
  vNewHolder: TChangeHolder;
begin
  DomainWrite(procedure
    begin
      vNewHolder := RetainChangeHolder(AParentHolder);
    end);
  Result := vNewHolder;
end;

function TUserSession.GetCurrentUserName: string;
begin
  if Assigned(FUser) then
    Result := FUser['Login']
  else
    Result := 'System';
end;

function TUserSession.GetUIState(const AObjectName: string; const AContextState: TState): TViewState;
begin
  Result := GetAccessFlags(FUser, AObjectName, AContextState);
end;

procedure TUserSession.InternalReleaseChangeHolder(const AHolder: TChangeHolder; const AResult, ASkipLogging: Boolean);
var
  vNewLogID: Integer;
begin
  try
    CheckLocking;
  except
    AHolder.RevertChanges;
    raise Exception.Create('Ошибка сохранения записи. Неверное состояние блокировки');
  end;

  vNewLogID := -1;

  if AResult then
  begin
    if not Assigned(AHolder.ParentHolder) then
    begin
      try
        if not ASkipLogging then
          ReloadDomainChanges(AHolder);

        if AHolder.IsModified then
          vNewLogID := AHolder.ApplyChanges(ASkipLogging);
      except
        on E: Exception do
        begin
          AHolder.RevertChanges;
          raise Exception.Create('Ошибка при сохранении данных.'#13#10'Ваши изменения будут отменены.'#13#10'Причина: ' + E.Message);
        end;
      end;
    end
    else
      AHolder.TransferChanges;
  end
  else
    AHolder.RevertChanges;

  TDomain(FDomain).UpdateLogID(vNewLogID);
  TDomain(FDomain).Logger.AddExitMessage('$$$ RELEASE HOLDER');
end;

procedure TUserSession.ReleaseChangeHolder(const AHolder: TChangeHolder; const AResult, ASkipLogging: Boolean);
begin
  try
    InternalReleaseChangeHolder(AHolder, AResult, ASkipLogging);
  finally
    if FHolders.Remove(AHolder) < 0 then
      AHolder.DisposeOf;

    if Assigned(FInteractor) then
      TInteractor(FInteractor).PrintHierarchy;
  end;
end;

procedure TUserSession.ReloadDomainChanges(const AHolder: TChangeHolder);
begin
  TDomain(FDomain).ReloadChanges(FInteractor, AHolder);
end;

function TUserSession.RetainChangeHolder(const AParentHolder: TChangeHolder): TChangeHolder;
begin
  CheckLocking;
  TDomain(FDomain).Logger.AddEnterMessage('$$$ RETAIN HOLDER');
  Result := TChangeHolder.Create(Self, AParentHolder);

  FHolders.Add(Result);
end;

procedure TUserSession.Save(const AHolder: TChangeHolder);
begin
  if Assigned(AHolder) then
  begin
    DomainWrite(procedure
      begin
        InternalReleaseChangeHolder(AHolder, True, False);
      end);
  end;
end;

procedure TUserSession.SetInteractor(const Value: TObject);
begin
  // Это нужно, если к одной сессии может подключиться несколько интеракторов
  FInteractor := Value;
  if FInteractor = nil then
    TDomain(FDomain).Logout(Self);
end;

{ TUserSessions }

function TUserSessions.AddSession(const AUser: TEntity): TUserSession;
begin
  Result := TUserSession.Create(FDomain, AUser);
  FItems.Add(Result);
end;

constructor TUserSessions.Create(const ADomain: TObject);
begin
  inherited Create;
  FDomain := ADomain;
  FItems := TObjectList<TUserSession>.Create;
end;

destructor TUserSessions.Destroy;
begin
  FreeAndNil(FItems);
  FDomain := nil;
  inherited Destroy;
end;

function TUserSessions.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TUserSessions.GetEnumerator: TEnumerator<TUserSession>;
begin
  Result := FItems.GetEnumerator;
end;

function TUserSessions.GetSession(const AIndex: Integer): TUserSession;
begin
  Result := FItems[AIndex];
end;

procedure TUserSessions.RemoveSession(const ASession: TUserSession);
begin
  FItems.Remove(ASession);
end;

function TUserSessions.SessionByLogin(const ALogin: string): TUserSession;
var
  vSession: TUserSession;
begin
  for vSession in FItems do
    if Assigned(vSession.CurrentUser) and SameText(vSession.CurrentUser['Login'], ALogin) then
      Exit(vSession);
  Result := nil;
end;

end.
