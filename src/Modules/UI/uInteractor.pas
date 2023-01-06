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

unit uInteractor;

interface

uses
  Generics.Collections, uConsts, uUIBuilder, uView;

type
  TGetViewFunc = reference to function(const AHolder: TObject): TView;

  TInteractor = class
  private
    [Weak] FSession: TObject;          // Пользовательская сессия
    [Weak] FDomain: TObject;           // Ссылка на исполняемый домен
    [Weak] FConfiguration: TObject;    // Ссылка на конфигурацию
    [Weak] FPresenter: TObject;        // Ссылка на UI

    FUIBuilder: TUIBuilder;
    FUIInstance: TUIInstance;
  public
    constructor Create(const APresenter, ASession: TObject);
    destructor Destroy; override;

    function GetViewOfEntity(const AEntity: TObject): TView;
    function ShowEntityEditor(const AView: TView; const AHolder: TObject; const ALayoutName: string = ''; const ACaption: string = ''): Boolean;
    function AtomicEditEntity(const AGetViewFunc: TGetViewFunc; const AParentHolder: TObject; const ALayoutName: string = ''; const ACaption: string = ''): Boolean; overload;
    function AtomicEditEntity(const AView: TView; const AParentHolder: TObject; const ALayoutName: string = ''; const ACaption: string = ''): Boolean; overload;
    function AtomicEditEntity(const AEntity: TObject; const AParentHolder: TObject; const ALayoutName: string = ''; const ACaption: string = ''): Boolean; overload;
    function AtomicEditParams(const AView: TView; const ALayoutName: string = ''; const ACaption: string = ''): Boolean;
    function EditParams(const AEntity: TObject; const ALayoutName: string = ''; const ACaption: string = ''): Boolean;
    procedure ViewEntity(const AView: TView; const ALayoutName: string = '');

    function Translate(const AKey: string; const ADefault: string = ''): string;
    function NeedSkipField(const ASource: TObject; const AFieldDef: TObject; const AAutoCreation: Boolean = True): Boolean;
    function NeedSkipColumn(const ASource: TObject; const AFieldDef: TObject): Boolean;

    procedure ShowMessage(const AText: string; const AMessageType: TMessageType = msNone);

    property Presenter: TObject read FPresenter;
    property UIBuilder: TUIBuilder read FUIBuilder;
    property UIInstance: TUIInstance read FUIInstance;
    property Session: TObject read FSession;
    property Domain: TObject read FDomain;
    property Configuration: TObject read FConfiguration;
  end;

implementation

uses
  SysUtils,
  uDomain, uEntity, uObjectField, uSession, uPresenter, uEntityList,
  uConfiguration, uDefinition, uChangeManager;

{ TInteractor }

function TInteractor.AtomicEditEntity(const AGetViewFunc: TGetViewFunc; const AParentHolder: TObject;
  const ALayoutName: string = ''; const ACaption: string = ''): Boolean;
var
  vView: TView;
  vResult: Boolean;
  vHolder: TChangeHolder;
begin
  TUserSession(FSession).DomainWrite(procedure
    begin
      vHolder := TUserSession(FSession).RetainChangeHolder(TChangeHolder(AParentHolder));
      vView := AGetViewFunc(vHolder);
    end);

  try
    vResult := ShowEntityEditor(vView, vHolder, ALayoutName, ACaption);
  finally
    TUserSession(FSession).DomainWrite(procedure
      begin
        TUserSession(FSession).ReleaseChangeHolder(vHolder, vResult);
      end);
  end;

  Result := vResult;
end;

function TInteractor.AtomicEditEntity(const AView: TView; const AParentHolder: TObject; const ALayoutName: string = ''; const ACaption: string = ''): Boolean;
begin
  Result := AtomicEditEntity(function(const AHolder: TObject): TView
    begin
      Result := AView;
    end, AParentHolder, ALayoutName, ACaption);
end;

function TInteractor.AtomicEditEntity(const AEntity, AParentHolder: TObject; const ALayoutName: string = ''; const ACaption: string = ''): Boolean;
begin
  if not Assigned(AEntity) then
    Exit(False);

  Result := AtomicEditEntity(function(const AHolder: TObject): TView
    begin
      Result := GetViewOfEntity(TEntity(AEntity));
    end, AParentHolder, ALayoutName, ACaption);
end;

function TInteractor.AtomicEditParams(const AView: TView; const ALayoutName: string = ''; const ACaption: string = ''): Boolean;
begin
  Result := ShowEntityEditor(AView, nil, ALayoutName, ACaption);
end;

constructor TInteractor.Create(const APresenter, ASession: TObject);
begin
  inherited Create;

  FPresenter := APresenter;

  FSession := ASession;
  TUserSession(FSession).Interactor := Self;
  FDomain := TUserSession(ASession).Domain;
  FConfiguration := TDomain(FDomain).Configuration;

  FUIBuilder := TUIBuilder.Create(Self);
  FUIInstance := TUIInstance.Create(FUIBuilder, Self);
end;

destructor TInteractor.Destroy;
begin
  FreeAndNil(FUIInstance);
  FreeAndNil(FUIBuilder);

  FPresenter := nil;

  TUserSession(FSession).Interactor := nil;
  FSession := nil;
  FDomain := nil;
  FConfiguration := nil;

  inherited Destroy;
end;

function TInteractor.EditParams(const AEntity: TObject; const ALayoutName: string = ''; const ACaption: string = ''): Boolean;
begin
  Result := ShowEntityEditor(GetViewOfEntity(AEntity), nil, ALayoutName, ACaption);
end;

function TInteractor.GetViewOfEntity(const AEntity: TObject): TView;
var
  vEntity: TEntity absolute AEntity;
begin
  if not Assigned(vEntity) then
    Exit(nil);

  if vEntity.ID > 0 then
    Result := FUIBuilder.RootView.BuildView(vEntity.Definition.Name + '/' + IntToStr(vEntity.ID))
  else if vEntity.ID < 0 then
    Result := FUIBuilder.RootView.BuildView(vEntity.Definition.Name + '/New' + IntToStr(-vEntity.ID))
  else if (vEntity.ID = 0) and not vEntity.IsNew then
    Result := FUIBuilder.RootView.BuildView(vEntity.Definition.Name + '/Auto')
  else
    Result := nil;
end;

function TInteractor.NeedSkipColumn(const ASource: TObject; const AFieldDef: TObject): Boolean;
var
  vFieldKind: TFieldKind;
  vFieldDef: TFieldDef absolute AFieldDef;
begin
  if vFieldDef = nil then
  begin
    Result := True;
    Exit;
  end;

  vFieldKind := vFieldDef.Kind;

  Result := (NeedSkipField(ASource, AFieldDef) and not vFieldDef.HasFlag(cHideInEdit))
    or vFieldDef.HasFlag(cHideInGrid) or (vFieldKind in [fkList, fkBlob, fkComplex])
    or ((vFieldKind = fkObject) and (TEntityList(ASource).FillerKind = lfkList) and
       (vFieldDef.Name = TListField(TEntityList(ASource).Filler).MasterFieldName));
end;

function TInteractor.NeedSkipField(const ASource: TObject; const AFieldDef: TObject; const AAutoCreation: Boolean = True): Boolean;
var
  vFieldDef: TFieldDef absolute AFieldDef;

  function FieldIsHidden(const AFieldDef: TFieldDef): Boolean;
  begin
    Result := AFieldDef.UIState and TUserSession(FSession).GetUIState(AFieldDef.FullName, nil) = vsHidden;
  end;

begin
  Result := (AFieldDef is TServiceFieldDef)
    or FieldIsHidden(vFieldDef) or (AAutoCreation and vFieldDef.HasFlag(cHideInEdit))
    or (Assigned(ASource) and (TEntityList(ASource).FillerKind = lfkList) and
      (TListFieldDef(TListField(TEntityList(ASource).Filler).FieldDef).IsFieldHidden(vFieldDef.Name)));
end;

function TInteractor.ShowEntityEditor(const AView: TView; const AHolder: TObject; const ALayoutName: string = ''; const ACaption: string = ''): Boolean;
var
  vEntity: TEntity;
begin
  TDomain(FDomain).CheckLocking(False);
  Assert(AView.DefinitionKind in [dkEntity, dkAction, dkObjectField], 'Показываем непонятно что');

  vEntity := TEntity(AView.DomainObject);
  if not Assigned(vEntity) then
  begin
    Result := False;
    Exit;
  end;

  Result := FUIBuilder.Navigate(AView, 'child', ALayoutName, 'operation=edit', AHolder, ACaption) = drOk;
end;

procedure TInteractor.ShowMessage(const AText: string; const AMessageType: TMessageType = msNone);
begin
  TDomain(FDomain).CheckLocking(False);
  TPresenter(FPresenter).ShowMessage(TDomain(FDomain).AppTitle, AText, AMessageType);
end;

function TInteractor.Translate(const AKey, ADefault: string): string;
begin
  Result := TDomain(FDomain).Translate(AKey, ADefault);
end;

procedure TInteractor.ViewEntity(const AView: TView; const ALayoutName: string = '');
begin
  ShowEntityEditor(AView, nil, ALayoutName);
end;

end.
