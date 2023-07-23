unit uProjectNameScript;

interface

uses
  Classes, uScript, uCollection, uEntity, uReport, uSession, uView, uConsts, uChangeManager, uInteractor;

type
  TProjectNameScript = class(TScript)
  protected
    procedure DoInit; override;

    procedure DoCreateDefinitions; override;
    procedure DoCreateDefaultEntities(const ADomain: TObject; const AHolder: TChangeHolder); override;
    function GetFullText(const AEntity: TEntity; var AHandled: Boolean): string; override;
    function DoCheckActionFlags(const AView: TView; const AActionName: string;
      const AContext: TObject; const AParams: TEntity): TViewState; override;
    function CheckCanChangeField(const AView: TView; const AEntity: TEntity; const AFieldName: string;
      const ANewValue: Variant; var AHandled: Boolean): Boolean; override;
    procedure DoAfterEntityCreation(const AHolder: TChangeHolder; const AOwnerContext: TObject; const AEntity: TEntity); override;
    function DoExecuteAction(const AView: TView; const AActionName: string;
      const AContext: TObject; const AParams: TEntity; const AParentHolder: TChangeHolder): Boolean; override;
  end;

implementation

uses
  SysUtils, DateUtils, StrUtils, Variants, uDomain, uUtils;

const
  cAdminID = 1;
  cAdminsID = 1;

procedure TProjectNameScript.DoInit;
begin
  FAppTitle := '$ProjectTitle$';
  FVersion := '$ProjectVersion$';
end;

// Scheduler

function DoScheduledAction(const ADomain: TObject): Integer;
begin
  Result := 0;
end;

// Handlers

procedure OnSomethingChanged(const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParam: TEntity);
begin
end;

{ TProjectNameScript }

function TProjectNameScript.CheckCanChangeField(const AView: TView; const AEntity: TEntity;
  const AFieldName: string; const ANewValue: Variant; var AHandled: Boolean): Boolean;
begin
  Result := True;
end;

procedure TProjectNameScript.DoAfterEntityCreation(const AHolder: TChangeHolder; const AOwnerContext: TObject; const AEntity: TEntity);
var
  vCollectionName: string;
begin
  // ����� �� � ��������� ��������������
  vCollectionName := AEntity.CollectionName;
  if vCollectionName = cUnknownName then
  else
    inherited DoAfterEntityCreation(AHolder, AOwnerContext, AEntity);
end;

function TProjectNameScript.DoCheckActionFlags(const AView: TView; const AActionName: string;
  const AContext: TObject; const AParams: TEntity): TViewState;
begin
  Result := inherited DoCheckActionFlags(AView, AActionName, AContext, AParams);
  if Result <> vsUndefined then
    Exit;
end;

procedure TProjectNameScript.DoCreateDefaultEntities(const ADomain: TObject; const AHolder: TChangeHolder);
var
  vDomain: TDomain absolute ADomain;
  vCollection: TCollection;
begin
  inherited DoCreateDefaultEntities(ADomain, AHolder);

  vCollection := vDomain['SysUsers'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'Login;Name;!Password',
    ['admin', '�������������', Md5Hash('')], True);

  vCollection := vDomain['SysUsersRoles'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'User;Role;Flags', [
    NativeInt(vDomain.EntityByID('SysUsers', 1)), NativeInt(vDomain.EntityByID('SysRoles', 1))], True);

  vCollection := vDomain['SysConstants'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'Code;MailLogin;MailPassword', ['debug', 'noname@mail.ru', '']);
end;

procedure TProjectNameScript.DoCreateDefinitions;
begin
  inherited DoCreateDefinitions;
end;

function TProjectNameScript.DoExecuteAction(const AView: TView; const AActionName: string;
  const AContext: TObject; const AParams: TEntity; const AParentHolder: TChangeHolder): Boolean;
begin
  Result := inherited DoExecuteAction(AView, AActionName, AContext, AParams, AParentHolder);
  if Result then
    Exit;

  if AActionName = cUnknownName then
    Result := True
  else
    Result := False;
end;

function TProjectNameScript.GetFullText(const AEntity: TEntity; var AHandled: Boolean): string;
var
  vCollectionName: string;
begin
  vCollectionName := AEntity.CollectionName;
  if vCollectionName = cUnknownName then
    Result := ''
  else
    Result := inherited GetFullText(AEntity, AHandled);
end;

initialization

RegisterScript('ProjectName', TProjectNameScript);

end.
