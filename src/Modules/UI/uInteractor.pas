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
  Generics.Collections, SysUtils, uConsts, uUIBuilder, uView, uLayout;

type
  TGetViewFunc = reference to function(const AHolder: TObject): TView;

  TInteractor = class
  private
    [Weak] FSession: TObject;          // Пользовательская сессия
    [Weak] FDomain: TObject;           // Ссылка на исполняемый домен
    [Weak] FConfiguration: TObject;    // Ссылка на конфигурацию
    [Weak] FPresenter: TObject;        // Ссылка на UI
    FUIBuilder: TUIBuilder;
    FRootView: TView;
    FRootArea: TUIArea;
    FUniqueIds: TDictionary<string, Integer>;
    FAreaStack: TStack<TUIArea>;
    FDefaultParams: string;
    [Weak] FCurrentArea: TUIArea;
    [Weak] FLastArea: TUIArea;
    [Weak] FActiveArea: TUIArea;
    [Weak] FPagedArea: TUIArea;

    procedure SetLastArea(const Value: TUIArea);
    procedure SetPagedArea(const Value: TUIArea);
    function GetUniqueViewName(const AViewName: string): string;
  public
    constructor Create(const APresenter, ASession: TObject);
    destructor Destroy; override;

    function GetViewOfEntity(const AEntity: TObject; const APostfix: string = ''; const AUnique: Boolean = False): TView;
    procedure ShowEntityEditor(const AView: TView; const AHolder: TObject; const ALayoutName: string = '';
      const ACaption: string = ''; const AOnClose: TCloseProc = nil);
    procedure AtomicEditEntity(const AGetViewFunc: TGetViewFunc; const AParentHolder: TObject;
      const ALayoutName: string = ''; const ACaption: string = ''; const AOnClose: TCloseProc = nil); overload;
    procedure AtomicEditEntity(const AView: TView; const AParentHolder: TObject;
      const ALayoutName: string = ''; const ACaption: string = '';
      const AOnClose: TCloseProc = nil); overload;
    procedure AtomicEditEntity(const AEntity: TObject; const AParentHolder: TObject;
      const ALayoutName: string = ''; const ACaption: string = '';
      const AOnClose: TCloseProc = nil); overload;
    procedure AtomicEditParams(const AView: TView; const ALayoutName: string = '';
      const ACaption: string = ''; const AOnClose: TCloseProc = nil);
    procedure EditParams(const AEntity: TObject; const ALayoutName: string = '';
      const ACaption: string = ''; const AOnClose: TCloseProc = nil);
    procedure ViewEntity(const AView: TView; const ALayoutName: string = '');

    function Translate(const AKey: string; const ADefault: string = ''): string;
    function NeedSkipField(const ASource: TObject; const AFieldDef: TObject; const AAutoCreation: Boolean = True): Boolean;
    function NeedSkipColumn(const ASource: TObject; const AFieldDef: TObject): Boolean;

    procedure ShowMessage(const AText: string; const AMessageType: TMessageType = msNone);
    procedure ShowYesNoDialog(const ACaption, AText: string;
      const AWithCancel: Boolean = False; const AOnClose: TCloseProc = nil);

    procedure CloseCurrentArea(const AModalResult: Integer);
    procedure PrintHierarchy;
    procedure ProcessAreaDeleting(const AArea: TUIArea);

    property Presenter: TObject read FPresenter;
    property UIBuilder: TUIBuilder read FUIBuilder;
    property Session: TObject read FSession;
    property Domain: TObject read FDomain;
    property Configuration: TObject read FConfiguration;

    property RootView: TView read FRootView;
    property RootArea: TUIArea read FRootArea write FRootArea;
    property CurrentArea: TUIArea read FCurrentArea write FCurrentArea;
    property AreaStack: TStack<TUIArea> read FAreaStack;
    property DefaultParams: string read FDefaultParams write FDefaultParams;
    property PagedArea: TUIArea read FPagedArea write SetPagedArea;
    property LastArea: TUIArea read FLastArea write SetLastArea;
    property ActiveArea: TUIArea read FActiveArea write FActiveArea;
  end;

implementation

uses
  uDomain, uEntity, uObjectField, uSession, uPresenter, uEntityList,
  uConfiguration, uDefinition, uChangeManager, uUtils;

{ TInteractor }

procedure TInteractor.AtomicEditEntity(const AGetViewFunc: TGetViewFunc; const AParentHolder: TObject;
  const ALayoutName: string = ''; const ACaption: string = ''; const AOnClose: TCloseProc = nil);
var
  vView: TView;
  vHolder: TChangeHolder;
begin
  TUserSession(FSession).DomainWrite(procedure
    begin
      vHolder := TUserSession(FSession).RetainChangeHolder(TChangeHolder(AParentHolder));
      vView := AGetViewFunc(vHolder);
    end);

  ShowEntityEditor(vView, vHolder, ALayoutName, ACaption, procedure(const AResult: TDialogResult)
    begin
      TUserSession(FSession).DomainWrite(procedure
      begin
        TUserSession(FSession).ReleaseChangeHolder(vHolder, AResult = drOk);
      end);

      if Assigned(AOnClose) then
        AOnClose(AResult);
    end);
end;

procedure TInteractor.AtomicEditEntity(const AView: TView; const AParentHolder: TObject;
  const ALayoutName: string = ''; const ACaption: string = ''; const AOnClose: TCloseProc = nil);
begin
  AtomicEditEntity(function(const AHolder: TObject): TView
    begin
      Result := AView;
    end, AParentHolder, ALayoutName, ACaption, AOnClose);
end;

procedure TInteractor.AtomicEditEntity(const AEntity, AParentHolder: TObject;
  const ALayoutName: string = ''; const ACaption: string = ''; const AOnClose: TCloseProc = nil);
begin
  if Assigned(AEntity) then
    AtomicEditEntity(function(const AHolder: TObject): TView
      begin
        Result := GetViewOfEntity(TEntity(AEntity));
      end, AParentHolder, ALayoutName, ACaption, AOnClose);
end;

procedure TInteractor.AtomicEditParams(const AView: TView; const ALayoutName: string = '';
  const ACaption: string = ''; const AOnClose: TCloseProc = nil);
begin
  ShowEntityEditor(AView, nil, ALayoutName, ACaption, AOnClose);
end;

procedure TInteractor.CloseCurrentArea(const AModalResult: Integer);
begin
  FCurrentArea.Close(AModalResult);
end;

constructor TInteractor.Create(const APresenter, ASession: TObject);
begin
  inherited Create;

  FPresenter := APresenter;

  FSession := ASession;
  TUserSession(FSession).AddInteractor(Self);
  FDomain := TUserSession(ASession).Domain;
  FConfiguration := TDomain(FDomain).Configuration;
  FUIBuilder := TDomain(FDomain).UIBuilder;
  FUIBuilder.Presenter := APresenter;
  FAreaStack := TStack<TUIArea>.Create;
  FUniqueIds := TDictionary<string, Integer>.Create;

  FRootArea := nil;
  FCurrentArea := nil;
  FLastArea := nil;
  FActiveArea := nil;
  FPagedArea := nil;
  FDefaultParams := '';
  FRootView := TView.Create(Self, nil, '');
end;

destructor TInteractor.Destroy;
begin
  FreeAndNil(FAreaStack);
  FreeAndNil(FUniqueIds);
  SetLastArea(nil);
  FCurrentArea := nil;
  FPagedArea := nil;
  if Assigned(FRootArea) then
    FRootArea.Release;
  FreeAndNil(FRootView);

  FUIBuilder := nil;
  FPresenter := nil;
  FSession := nil;
  FDomain := nil;
  FConfiguration := nil;

  inherited Destroy;
end;

procedure TInteractor.EditParams(const AEntity: TObject; const ALayoutName: string = '';
  const ACaption: string = ''; const AOnClose: TCloseProc = nil);
begin
  ShowEntityEditor(GetViewOfEntity(AEntity), nil, ALayoutName, ACaption, AOnClose);
end;

function TInteractor.GetUniqueViewName(const AViewName: string): string;
var
  vLastID: Integer;
begin
  if FUniqueIds.TryGetValue(AViewName, vLastID) then
    vLastID := vLastID + 1
  else
    vLastID := 1;
  FUniqueIds.AddOrSetValue(AViewName, vLastID);
  Result := AViewName + '~' + IntToStr(vLastID);
end;

function TInteractor.GetViewOfEntity(const AEntity: TObject; const APostfix: string = ''; const AUnique: Boolean = False): TView;
var
  vEntity: TEntity absolute AEntity;
  vDefinitionName: string;
  vViewName: string;
begin
  if not Assigned(vEntity) then
    Exit(nil);

  vDefinitionName := vEntity.Definition.Name;
  if vEntity.ID > 0 then
    vViewName := vDefinitionName + '/' + IntToStr(vEntity.ID)
  else if vEntity.ID < 0 then
    vViewName := vDefinitionName + '/New' + IntToStr(-vEntity.ID)
  else if (vEntity.ID = 0) and not vEntity.IsNew then
    vViewName := vDefinitionName + '/Auto'
  else
    Exit(nil);

  if APostfix <> '' then
    vViewName := vViewName + '~' + APostfix;
  if AUnique then
    vViewName := GetUniqueViewName(vViewName);

  Result := FRootView.BuildView(vViewName);
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

procedure TInteractor.PrintHierarchy;
begin
  if Assigned(TUserSession(FSession).CurrentUser) then
    TPresenter(FPresenter).ShowPage(Self, 'debug');
end;

procedure TInteractor.ProcessAreaDeleting(const AArea: TUIArea);
begin
  if FRootArea = AArea then
    FRootArea := nil;
  if FCurrentArea = AArea then
    FCurrentArea := nil;
  if FActiveArea = AArea then
    FActiveArea := nil;
  if FPagedArea = AArea then
    FPagedArea := nil;
end;

procedure TInteractor.SetLastArea(const Value: TUIArea);
begin
  if FLastArea = Value then
    Exit;

  if Assigned(FLastArea) and not Assigned(FLastArea.Interactor) then
  begin
    FLastArea.UnbindContent;
    FLastArea.Free;
  end;
  FLastArea := Value;
end;

procedure TInteractor.SetPagedArea(const Value: TUIArea);
begin
  Assert(not Assigned(FPagedArea), 'Область страниц инициализирована дважды!');
  FPagedArea := Value;
end;

procedure TInteractor.ShowEntityEditor(const AView: TView; const AHolder: TObject;
  const ALayoutName: string = ''; const ACaption: string = ''; const AOnClose: TCloseProc = nil);
var
  vEntity: TEntity;
begin
  TDomain(FDomain).CheckLocking(False);
  Assert(AView.DefinitionKind in [dkEntity, dkAction, dkObjectField], 'Показываем непонятно что');

  vEntity := TEntity(AView.DomainObject);
  if Assigned(vEntity) then
    FUIBuilder.Navigate(AView, 'child', ALayoutName, 'operation=edit', AHolder, ACaption, AOnClose);
end;

procedure TInteractor.ShowMessage(const AText: string; const AMessageType: TMessageType = msNone);
begin
  TDomain(FDomain).CheckLocking(False);
  TPresenter(FPresenter).ShowMessage(TDomain(FDomain).AppTitle, AText, AMessageType);
end;

procedure TInteractor.ShowYesNoDialog(const ACaption, AText: string;
  const AWithCancel: Boolean; const AOnClose: TCloseProc);
begin
  TDomain(FDomain).CheckLocking(False);
  TPresenter(FPresenter).ShowYesNoDialog(ACaption, AText, AWithCancel, AOnClose);
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
