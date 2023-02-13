unit uniListEditors;

interface

uses
  Generics.Collections, Classes, SysUtils,
  uEntity, uDefinition, uEntityList, uUIBuilder, uLayout, uDomain, uInteractor, uUtils, uSession, uChangeManager,
  uPresenter, uConsts,

  uniArea, uniListBox, uniPanel, uniGUITypes;

type
  TEntityListSelectorMTM = class(TUniGUIControl) // many to many link
  private
    FListBox: TUniListBox;
    FEntityList: TEntityList;
    FCheckList: TList<Boolean>;
    FTransitField: TObjectFieldDef;
    procedure FillList;
    procedure OnClickCheck(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    procedure DoOnChange; override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
  end;

//  TGridEditor = class(TUniGUIControl)
//  private
//    FGrid: TListView;
//    FAllData: TEntityList;
//    FEntities: TList<TEntity>;
//    FFilterText: string;
//    FFilterDateFrom, FFilterDateTo: TDateTime;
//    FFilterDateActive: Boolean;
//    FLayoutExists: Boolean;
//    FColumns: TObjectList<TColumnBinding>;
//    procedure DoOnTableViewDblClick(Sender: TObject);
//    procedure DoOnSelectItem(Sender: TObject; AItem: TListItem; ASelected: Boolean);
//    procedure DoOnItemChecked(Sender: TObject; AItem: TListItem);
//    procedure OnColumnClick(Sender: TObject; Column: TListColumn);
//    procedure OnColumnResize(Sender: TCustomListview; columnindex: Integer; columnwidth: Integer);
//    procedure CreateColumnsFromModel(const AFields: string = '');
//    procedure EnableColumnParamsChangeHandlers(const AEnable: Boolean);
//    procedure CreateColumn(const AFieldDef: TFieldDef; const AOverriddenCaption: string; const AWidth: Integer);
//    procedure BeforeContextMenuShow(Sender: TObject);
//    procedure SaveColumnWidths(Sender: TObject);
//    procedure LoadColumnWidths;
//    {function FindColumnByFieldName(const AFieldName: string): TListColumn;}
//
//    procedure FillRow(AEntity: TEntity);
//    function CreateRow(AEntity: TEntity): Boolean;
//    function GetValue(AEntity: TEntity; AColumn: TListColumn): Variant;
//
//    function IsMatchToFilter(const AEntity: TEntity): Boolean;
//    function GetSearchType: uConsts.TSearchType;
//    procedure ExportToExcel;
//  protected
//    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
//    procedure DoBeforeFreeControl; override;
//    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
//    procedure DoExecuteUIAction(const AView: TView); override;
//    procedure SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl); override;
//    procedure SetParent(const AParent: TUIArea); override;
//
//  end;

implementation


{ TEntityListSelectorMTM }

procedure TEntityListSelectorMTM.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEntityList);
  FreeAndNil(FCheckList);
  FreeAndNil(FListBox);
end;

function TEntityListSelectorMTM.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vListFieldDef: TListFieldDef;
  vTransitFieldName: string;
  vTransitField: TFieldDef;
  vInteractor: TInteractor;
begin
  inherited;
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FListBox := TUniListBox.Create(TUniPanel(Result));
  FListBox.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FListBox.LayoutConfig.Width := '100%';
  FListBox.LayoutConfig.Height := '100%';
  FListBox.MultiSelect := True;
  //FListBox.CheckBoxOnly := True;

  FCheckList := TList<Boolean>.Create;

  Assert(FView.Definition is TListFieldDef, 'FView.Definition не является TListFieldDef');
  vListFieldDef := TListFieldDef(FView.Definition);
  vTransitFieldName := GetUrlParam(vListFieldDef.StyleName, 'transit');
  Assert(Length(vTransitFieldName) > 0, 'Не задано транзитное поле. Параметр transit.');
  Assert(vListFieldDef._ContentDefinition.FieldExists(vTransitFieldName),
    'Указанное имя транзитного поля не существует: ' + vTransitFieldName);

  vTransitField := vListFieldDef._ContentDefinition.FieldByName(vTransitFieldName);
  Assert(vTransitField is TObjectFieldDef, 'Указанное транзитное поле не является TObjectFieldDef');

  FTransitField := TObjectFieldDef(vTransitField);

  vInteractor := TInteractor(FView.Interactor);

  FEntityList := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  TDomain(FDomain).GetEntityList(FView.Session, FTransitField._ContentDefinition, FEntityList, '');

  FListBox.OnClick := OnClickCheck;
end;

procedure TEntityListSelectorMTM.DoOnChange;
begin
  inherited;

end;

procedure TEntityListSelectorMTM.FillEditor;
begin
  inherited;

end;

procedure TEntityListSelectorMTM.FillList;
var
  i: Integer;
  vItem: Integer;
  vSelectedList: TEntityList;
  vList: TList<TEntity>;
  vSelectedIndex: Integer;
begin
  vSelectedList := TEntityList(FView.DomainObject);
  vList := TList<TEntity>.Create;
  try
    for i := 0 to vSelectedList.Count - 1 do
      vList.Add(vSelectedList[i].ExtractEntity(FTransitField.Name));

    FListBox.Items.BeginUpdate;
    try
      FListBox.Items.Clear;
      FCheckList.Clear;

      for i := 0 to FEntityList.Count - 1 do
      begin
        vItem := FListBox.Items.Add(FEntityList[i].DisplayName);
        vSelectedIndex := vList.IndexOf(FEntityList[i]);
        FListBox.Selected[vItem] := vSelectedIndex >= 0;
        FCheckList.Add(vSelectedIndex >= 0);
        if FCheckList[vItem] then
          FListBox.Items.Objects[vItem] := vSelectedList[vSelectedIndex]
        else
          FListBox.Items.Objects[vItem] := FEntityList[i];
      end;
    finally
      FListBox.Items.EndUpdate;
    end;
  finally
    FreeAndNil(vList);
  end;
end;

procedure TEntityListSelectorMTM.OnClickCheck(Sender: TObject);
var
  vEntity: TEntity;
  AIndex: Integer;
begin
  for AIndex := 0 to FCheckList.Count do
    if FListBox.Selected[AIndex] <> FCheckList[AIndex] then
      Break;
  FCheckList[AIndex] := FListBox.Selected[AIndex];
  vEntity := TEntity(FListBox.Items.Objects[AIndex]);
  if FCheckList[AIndex] then
  begin
    Assert(vEntity.Definition.IsDescendantOf(FEntityList.MainDefinition.Name),
      'К записи привязан объект неправильного типа');
    TUserSession(FView.Session).AtomicModification(nil,
      function(const AHolder: TChangeHolder): Boolean
      var
        vTransitEntity: TEntity;
      begin
        vTransitEntity := TEntityList(FView.DomainObject).AddEntity(AHolder, '', FTransitField.Name, [NativeInt(vEntity)]);
//        vTransitEntity._SetFieldEntity(AHolder, FTransitField.Name, vEntity);
        FListBox.Items.Objects[AIndex] := vTransitEntity;
        Result := True;
      end, TChangeHolder(FOwner.Holder));
  end
  else
  begin
    Assert(vEntity.Definition.IsDescendantOf(TEntityList(FView.DomainObject).MainDefinition.Name),
      'К записи привязан объект неправильного типа');
    FListBox.Items.Objects[AIndex] := vEntity.ExtractEntity(FTransitField.Name);
    TUserSession(FView.Session).AtomicModification(nil,
      function(const AHolder: TChangeHolder): Boolean
      begin
        TEntityList(FView.DomainObject).RemoveEntity(AHolder, vEntity);
        Result := True;
      end, TChangeHolder(FOwner.Holder));
  end;
end;

procedure TEntityListSelectorMTM.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(AHandler) then
    FListBox.OnClick := OnClickCheck
  else
    FListBox.OnClick := nil;
end;

procedure TEntityListSelectorMTM.UpdateArea(const AKind: Word; const AParameter: TEntity);
begin
  FillList;
end;

initialization

  TPresenter.RegisterControlClass('Web.UniGUI', uiListEdit, 'mtm', TEntityListSelectorMTM);

end.
