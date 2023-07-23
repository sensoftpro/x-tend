unit uniEntityEditors;
interface
uses Classes, uniArea, uEntityList, uUIBuilder, uLayout, uEntity, uView, uniPanel, uniComboBox, uniEdit, uniButton, uniMainMenu, uniLabel, uniBitBtn;
type
  TCrackedBitBtn = class(TUniCustomBitBtn) end;
  
  TUniGUIEntitySelector = class(TUniGUIControl)
  private
    FFlat: Boolean;
    FEntities: TEntityList;
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;
  
  TCloseQueryEvent = procedure(Sender: TObject; var ACanClose: Boolean) of object;
  TOnSelectEntityEvent = procedure (const ANewEntity: TEntity) of object;
  
  TUniGUIEntityFieldEditor = class(TUniGUIControl)
  private
    FBasePanel: TUniPanel;
    FSelectButton: TUniComboBox;
    FTextEdit: TUniEdit;
    FbtnAdd: TCrackedBitBtn;
    FEntities: TEntityList;
    FTypeSelectionMenu: TUniPopupMenu;
    procedure FillList;
  protected
    procedure UpdateVisibility; virtual;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure SetParent(const AParent: TUIArea); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SetFocused(const Value: Boolean); override;
    procedure DoDeinit; override;
  end;
  
  TUniGUIRadioEntitySelector = class(TUniGUIControl)
  private
    FEntities: TEntityList;
    procedure FillList;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;
  
  TUniGUILinkedEntityFieldEditor = class(TUniGUIEntityFieldEditor)
  private
    FLabel: TUniLabel;
    procedure OnLabelClick(Sender: TObject);
  protected
    procedure DoBeforeFreeControl; override;
    procedure UpdateVisibility; override;
  end;
  
implementation

uses SysUtils, uObjectField, uPresenter, uInteractor, uConsts, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, uDefinition, uniRadioGroup,
  uniImageList, uniGUITypes, Variants, uUtils;
  
{ TUniGUIEntitySelector }
procedure TUniGUIEntitySelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TUniGUIEntitySelector.DoBeforeFreeControl;
begin
  FreeAndNil(FEntities);
end;

function TUniGUIEntitySelector.DoCreateControl(const AParent: TUIArea;
  const ALayout: TLayout): TObject;
var
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(FView.Interactor);
  FEntities := TEntityList.Create(vInteractor.Domain, vInteractor.Session);
  Result := TUniComboBox.Create(ExtractOwner(AParent));
  TUniComboBox(Result).Style := csDropDown;
  TUniComboBox(Result).OnDropDown := CBOnInitPopup;
  FFlat := (ALayout.BevelOuter = lbkNone) and (ALayout.BevelInner = lbkNone);
end;

procedure TUniGUIEntitySelector.DoOnChange;
var
  vEntity: TEntity;
begin
  if TUniComboBox(FControl).ItemIndex >= 0 then
    vEntity := TEntity(TUniComboBox(FControl).Items.Objects[TUniComboBox(FControl).ItemIndex])
  else
    vEntity := nil;
  SetFieldEntity(vEntity);
end;

procedure TUniGUIEntitySelector.FillEditor;
var
  vEdit: TUniComboBox;
  vEntity: TEntity;
begin
  FillList;
  vEdit := TUniComboBox(FControl);
  vEntity := FView.FieldEntity;
  if Assigned(vEntity) then
  begin
    vEdit.Text := vEntity['Name'];
    vEdit.Enabled := FView.State >= vsSelectOnly;
  end
  else begin
    vEdit.Text := '';
    vEdit.Enabled := False;
  end;
end;

procedure TUniGUIEntitySelector.FillList;
var
  vField: TEntityField;
  vEntity: TEntity;
begin
  TUniComboBox(FControl).Items.BeginUpdate;
  try
    TUniComboBox(FControl).Items.Clear;
    vField := FView.ExtractEntityField;
    vField.GetEntitiesForSelect(TInteractor(FView.Interactor).Session, FEntities);
    for vEntity in FEntities do
      TUniComboBox(FControl).Items.AddObject(SafeDisplayName(vEntity), vEntity);
  finally
    TUniComboBox(FControl).Items.EndUpdate;
  end;
end;

procedure TUniGUIEntitySelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TUniComboBox(FControl).OnChange := AHandler;
end;

procedure TUniGUIEntityFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FTypeSelectionMenu);
  FreeAndNil(FEntities);
  FreeAndNil(FTextEdit);
end;

function TUniGUIEntityFieldEditor.DoCreateControl(const AParent: TUIArea;
  const ALayout: TLayout): TObject;
begin
  FEntities := nil;
  FBasePanel := TUniPanel.Create(ExtractOwner(AParent));
  FBasePanel.ShowCaption := False;
  
  FSelectButton := TUniComboBox.Create(ExtractOwner(AParent));
  with FSelectButton do
  begin
    Parent := FBasePanel;
    Align := alClient;
    Width := FBasePanel.Width;
    Style := csDropDownList;
  end;
  FSelectButton.Enabled := True;

  FTextEdit := TUniEdit.Create(ExtractOwner(AParent));
  FTextEdit.Parent := FBasePanel;
  FTextEdit.Align := alClient;
  FTextEdit.Visible := False;
  FTextEdit.ReadOnly := True;
  FTextEdit.Text := '';
  FTextEdit.TabStop := False;
  FTextEdit.Color := clBtnFace;
  Result := FBasePanel;
end;

procedure TUniGUIEntityFieldEditor.DoDeinit;
begin
  inherited;
  FSelectButton.Text := '';
  FSelectButton.Enabled := False;
  FbtnAdd.Visible := True; // todo: create option
end;

procedure TUniGUIEntityFieldEditor.FillEditor;
var
  vEntity: TEntity;
  vDefinition: TDefinition;
begin
  inherited;

  FillList;

  vEntity := TEntity(FView.FieldEntity);
  if TObjectFieldDef(FView.Definition).ContentDefinitionName = '~' then
  begin
    if FView.ParentDomainObject is TEntity then
      vDefinition := TEntityField(FView.ExtractEntityField).ContentDefinition
    else
      vDefinition := TDefinition(TObjectFieldDef(FView.Definition).ContentDefinitions[0]);
  end
  else
    vDefinition := TDefinition(TObjectFieldDef(FView.Definition).ContentDefinitions[0]);

  with FSelectButton do
  begin
    if not Assigned(vEntity) then
      Text := FOwner.GetTranslation(vDefinition, tpEmptyValue)
    else
      Text := vEntity['Name'];
    Hint := Text;
  end;

  UpdateVisibility;

  FbtnAdd.Visible := (FView.State = vsFullAccess) and not Assigned(vEntity) and (not vDefinition.HasFlag(ccSystem));
  FbtnAdd.Enabled := FbtnAdd.Visible;
  // todo: create option
end;

procedure TUniGUIEntityFieldEditor.FillList;
var
  vInteractor: TInteractor;
  vField: TEntityField;
  vEntity: TEntity;
  vPrevCursor: TCursorType;
begin
  vInteractor := TInteractor(FView.Interactor);

  if FEntities = nil then
    FEntities := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  vPrevCursor := TPresenter(vInteractor.Presenter).SetCursor(crtHourGlass);
  try
    vField := FView.ExtractEntityField;
    vField.GetEntitiesForSelect(TInteractor(FView.Interactor).Session, FEntities);

    FSelectButton.Items.Clear;
    for vEntity in FEntities do
      if Assigned(vEntity) then
        FSelectButton.Items.AddObject(vEntity['Name'], vEntity);
  finally
    TPresenter(vInteractor.Presenter).SetCursor(vPrevCursor);
  end;
end;

procedure TUniGUIEntityFieldEditor.SetFocused(const Value: Boolean);
begin
  if Value and FSelectButton.CanFocus then
    FSelectButton.SetFocus;
end;

procedure TUniGUIEntityFieldEditor.SetParent(const AParent: TUIArea);
var
  vButtonLayout: TLayout;
  vAddArea: TUIArea;
begin
  if Assigned(FbtnAdd) then Exit;

  inherited SetParent(AParent);

  vButtonLayout := FUIBuilder.Layouts.CreateSimpleLayout(lkPanel);
  vButtonLayout.Caption := 'Create?place=embedded';
  vButtonLayout.Hint := TInteractor(FView.Interactor).Translate('cptAddEntity', 'Добавить234');
  vButtonLayout.ImageID := 'add';
  vButtonLayout.Button_Flat := True;
  vButtonLayout.Align := lalRight;
  vButtonLayout.TabOrder := -1;
  vButtonLayout.Height := FLayout.Height;
  vButtonLayout.Width := FLayout.Height;
  vAddArea := FOwner.CreateChildArea(FView, vButtonLayout, '');
  FbtnAdd := TCrackedBitBtn(GetRealControl(vAddArea));
end;

procedure TUniGUIEntityFieldEditor.UpdateVisibility;
begin
  if (FView.State < vsSelectOnly) then
  begin
    FSelectButton.Visible := False;
    FTextEdit.Visible := True;
    FTextEdit.Text := FSelectButton.Text;
  end
  else begin
    FTextEdit.Visible := False;
    FSelectButton.Visible := True;
  end;
end;

{ TUniGUIRadioEntitySelector }
procedure TUniGUIRadioEntitySelector.DoBeforeFreeControl;
begin
  FreeAndNil(FEntities);
end;

function TUniGUIRadioEntitySelector.DoCreateControl(const AParent: TUIArea;
  const ALayout: TLayout): TObject;
begin
  Result := TUniRadioGroup.Create(ExtractOwner(AParent));
  TUniRadioGroup(Result).BorderStyle := ubsNone;
  TUniRadioGroup(Result).Name := 'radio';
  TUniRadioGroup(Result).Caption := '';
  FNeedCreateCaption := False;
  FEntities := TEntityList.Create(TInteractor(FView.Interactor).Domain, TInteractor(FView.Interactor).Session);
end;

procedure TUniGUIRadioEntitySelector.DoOnChange;
var
  vIndex: Integer;
begin
  vIndex := TUniRadioGroup(FControl).ItemIndex;
  SetFieldEntity(FEntities[vIndex]);
end;

procedure TUniGUIRadioEntitySelector.FillEditor;
var
  vRadioEdit: TUniRadioGroup;
begin
  inherited;
  FillList;
  vRadioEdit := TUniRadioGroup(FControl);
  if VarIsNull(FView.FieldValue) or (FView.FieldValue = 0) then
  begin
    vRadioEdit.Enabled := False;
  end
  else begin
    vRadioEdit.Enabled := FView.State >= vsSelectOnly
  end;
end;

procedure TUniGUIRadioEntitySelector.FillList;
var
  vRadioItems: TStrings;
  vField: TEntityField;
  i: Integer;
  vEnt: TEntity;
begin
  vField := FView.ExtractEntityField;
  vField.GetEntitiesForSelect(TInteractor(FView.Interactor).Session, FEntities);

  vRadioItems := TUniRadioGroup(FControl).Items;
  vRadioItems.BeginUpdate;
  try
    vRadioItems.Clear;
    for i := 0 to FEntities.Count - 1 do
    begin
      vEnt := FEntities[i];
      vRadioItems.AddObject(SafeDisplayName(vEnt), vEnt);
      if vEnt = TEntity(NativeInt(FView.FieldValue)) then
        TUniRadioGroup(FControl).ItemIndex := TUniRadioGroup(FControl).Items.Count - 1;
    end;
  finally
    vRadioItems.EndUpdate;
  end;
end;

procedure TUniGUIRadioEntitySelector.SwitchChangeHandlers(
  const AHandler: TNotifyEvent);
begin
  TUniRadioGroup(FControl).OnClick := AHandler;
end;

{ TUniGUILinkedEntityFieldEditor }

procedure TUniGUILinkedEntityFieldEditor.DoBeforeFreeControl;
begin
  inherited DoBeforeFreeControl;
  FreeAndNil(FLabel);
end;

procedure TUniGUILinkedEntityFieldEditor.OnLabelClick(Sender: TObject);
var
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(FView.Interactor);
  vInteractor.ViewEntity(FView);
end;

procedure TUniGUILinkedEntityFieldEditor.UpdateVisibility;
var
  vEntity: TEntity;
begin
  vEntity := TEntity(FView.FieldEntity);
  if (FView.State = vsSelectOnly) and Assigned(vEntity) and not vEntity.IsNew then
  begin
    FSelectButton.Visible := False;
    FTextEdit.Visible := False;
    if not Assigned(FLabel) then
    begin
      FLabel := TUniLabel.Create(nil);
      FLabel.Align := alClient;
      FLabel.Parent := FBasePanel;
      FLabel.Font.Color := clBlue;
      FLabel.Font.Style := [fsUnderline];
      FLabel.Cursor := crHandPoint;
      FLabel.Transparent := True;
      FLabel.OnClick := OnLabelClick;
    end
    else
      FLabel.Visible := True;
    FLabel.Caption := FSelectButton.Text;
  end
  else if (FView.State < vsSelectOnly) then
  begin
    FSelectButton.Visible := False;
    if Assigned(FLabel) then
      FLabel.Visible := False;
    FTextEdit.Visible := True;
    FTextEdit.Text := FSelectButton.Text;
  end
  else
  begin
    FTextEdit.Visible := False;
    if Assigned(FLabel) then
      FLabel.Visible := False;
    FSelectButton.Visible := True;
  end;
end;

initialization

TPresenter.RegisterControlClass('Web.UniGUI', uiEntityEdit, '', TUniGUIEntityFieldEditor);
TPresenter.RegisterControlClass('Web.UniGUI', uiEntityEdit, 'simple', TUniGUIEntityFieldEditor);
TPresenter.RegisterControlClass('Web.UniGUI', uiEntityEdit, 'link', TUniGUILinkedEntityFieldEditor);
TPresenter.RegisterControlClass('Web.UniGUI', uiEntityEdit, 'select', TUniGUIEntitySelector);
TPresenter.RegisterControlClass('Web.UniGUI', uiEntityEdit, 'radio', TUniGUIRadioEntitySelector);

end.
