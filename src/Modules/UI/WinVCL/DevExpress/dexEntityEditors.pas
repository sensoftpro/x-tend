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

unit dexEntityEditors;

interface

uses
  Classes, Generics.Collections, StdCtrls, ExtCtrls, ActnList, Types, Menus, Forms, uInteractor, Graphics, uConsts,
  cxTL, cxLabel, cxTextEdit, cxEdit, cxMaskEdit, cxDropDownEdit, cxButtons, cxVGrid,

  Buttons, dexArea, vclEntityEditors, uUIBuilder, uLayout, uView, uEntity, uEntityList, uDefinition, Controls;

type
  TDEEntitySelector = class(TDEControl)
  private
    FFlat: Boolean;
    FEntities: TEntityList;
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
    function GetDisabledBorderStyle: TcxEditBorderStyle;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDESelectEntityPopup = class(TUIPopupWindow)
  private
    FList: TcxTreeList;
    FTopLabel, FBottomLabel: TLabel;
    FOnEntitySelected: TOnSelectEntityEvent;
    FAllData: TEntityList;
    FSelectedEntity: TEntity;
    FTopPanel: TPanel;
    FFilterEdit: TEdit;
    FSearchType: TSearchType;
    FFilterText: string;
    FDisplayName: string;
    procedure UpdateCount;
    procedure OnDblClick(Sender: TObject);
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpdateSizes;
    procedure Reload;
    function IsMatchToFilter(const AEntity: TEntity): Boolean;
    procedure OnFilterChange(Sender: TObject);
  protected
    procedure DoOnBeforeShow; override;
  public
    constructor Create(const AInteractor: TInteractor); override;
    destructor Destroy; override;

    procedure Init(const AItems: TEntityList; const AIsSelectMode: Boolean;
      const ASelectedEntity: TEntity; const AFilter, ADisplayName: string);

    property OnEntitySelected: TOnSelectEntityEvent read FOnEntitySelected write FOnEntitySelected;
  end;

  TDEEntityFieldEditor = class(TDEControl)
  private
    FBasePanel: TPanel;
    FSelectButton: TcxComboBox;
    FTextEdit: TcxTextEdit;
    FbtnAdd: TcxButton;
    FEntities: TEntityList;
    FSelectPopup: TDESelectEntityPopup;
    FTypeSelectionMenu: TPopupMenu;
    FDisplayName: string;
    procedure OnSelectClick(Sender: TObject);
    procedure ShowPopup(const AFilter: string);
    procedure DoOnClose(Sender: TObject);
    procedure ClosePopup;
    function PopupShowing: Boolean;
    procedure OnEntitySelected(const ANewEntity: TEntity);
    procedure OnWinControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnComboMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function GetDisabledBorderStyle: TcxEditBorderStyle;
  protected
    procedure UpdateVisibility; virtual;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SetFocused(const Value: Boolean); override;
    procedure FocusedChanged(const AFocused: Boolean); override;
    procedure DoDeinit; override;
  end;

  TDERadioEntitySelector = class(TDEControl)  // based on TRadioGroup. At the top there is a large indent for the title. If the title is empty, then the elements look crooked (a lot of empty space on top).
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

  TDERadioEntitySelector2 = class(TDEControl) // based on TPanel with TRadioButton controls to get rid of empty space on top
  private
    FEntities: TEntityList;
    FItemHeight: Integer;
    procedure OnChange(Sender: TObject);
    procedure FillList;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDELinkedEntityFieldEditor = class(TDEEntityFieldEditor)
  private
    FLabel: TcxLabel;
    procedure OnLabelClick(Sender: TObject);
  protected
    procedure DoBeforeFreeControl; override;
    procedure UpdateVisibility; override;
  end;

  TDEListEntityFieldEditor = class(TDEControl)
  private
    FGrid: TcxTreeList;
    FEntities: TEntityList;
    procedure OnDblClick(Sender: TObject);
    procedure DoOnFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    procedure Reload;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;

  TDEEntityFieldListEditor = class(TDEControl) // property editor
  private
    FEditor: TcxVerticalGrid;
    FDomainObject: TEntity;
    FChildViews: TList<TView>;
    FDisplayFields: TStringList;
    FReadOnly: Boolean;
    procedure CleanChildViews;
    function CreateEditRow(const ARootEntity: TEntity; const AFieldDef: TFieldDef;
      const AViewPath: string; const AOverriddenCaption: string = ''): TcxEditorRow;
    function CreateCategoryRow(const ARootEntity: TEntity; const AFieldDef: TFieldDef): TcxCategoryRow;
    procedure CreateRows(const ARootEntity: TEntity; const ARootRow: TcxCustomRow;
      const AViewPath: string; const ARootEntityIndex: Integer = -1);
    procedure OnFieldChange(Sender: TObject);
    procedure FreeEditors;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure DoDisableContent; override;
    procedure FillEditor; override;
    procedure RefillArea(const AKind: Word); override;
  end;

implementation

uses
  SysUtils, Windows, Messages, cxInplaceContainer, cxCalendar, cxSpinEdit, cxControls, cxRadioGroup,
  uDomain, uChangeManager, uObjectField, uSession, Variants,
  uPresenter, uWinVCLPresenter, StrUtils, uUtils, uEnumeration;

type
  TRowData = class
    Entity: TEntity;
    FieldDef: TFieldDef;
    constructor Create(const AEntity: TEntity; const AFieldDef: TFieldDef);
  end;

function IsAlpha(c: Char): Boolean;
begin
  Result := ((c >= 'а') and (c <= 'я')) or ((c >= 'А') and (c <= 'Я')) or
    ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z'));
end;

function VKeytoWideString(const AKey: Word): WideString;
var
  WBuff: array [0..255] of WideChar;
  KeyboardState: TKeyboardState;
  UResult: Integer;
begin
  Result := '';
  GetKeyBoardState (KeyboardState);
  ZeroMemory(@WBuff[0], SizeOf(WBuff));
  UResult := ToUnicode(AKey, MapVirtualKey(AKey, 0), KeyboardState, WBuff, Length(WBuff), 0);
  if UResult > 0 then
    SetString(Result, WBuff, UResult)
  else if UResult = -1 then
    Result := WBuff;
end;

{ TDEEntityFieldEditor }

procedure TDEEntityFieldEditor.DoBeforeFreeControl;
begin
  inherited;

  FreeAndNil(FTypeSelectionMenu);
  FreeAndNil(FSelectPopup);
  FreeAndNil(FEntities);
  FreeAndNil(FTextEdit);
end;

function TDEEntityFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vButtonLayout: TLayout;
  vAddArea: TUIArea;
begin
  if Assigned(FSelectPopup) then
  begin
    FSelectPopup.Free;
    FSelectPopup := nil;
  end;
  FEntities := nil;

  FBasePanel := TPanel.Create(nil);
  FBasePanel.BevelOuter := bvNone;
  FBasePanel.ShowCaption := False;

  FSelectButton := TcxComboBox.Create(nil);
  with FSelectButton do
  begin
    Parent := FBasePanel;
    Align := alClient;
    Width := FBasePanel.Width;
    OnKeyDown := OnWinControlKeyDown;
    OnMouseDown := OnComboMouseDown;
    Properties.ReadOnly := True;
    Properties.AutoSelect := False;
    Properties.IncrementalSearch := False;
    Properties.DropDownListStyle := lsEditList;
    Properties.ValidateOnEnter := False;
  end;

  FSelectButton.Enabled := True;

  FTextEdit := TcxTextEdit.Create(nil);
  FTextEdit.Parent := FBasePanel;
  FTextEdit.Align := alClient;
  FTextEdit.Visible := False;
  FTextEdit.Properties.ReadOnly := True;
  FTextEdit.EditValue := '';
  FTextEdit.TabStop := False;
  FTextEdit.Style.Color := clBtnFace;

  Result := FBasePanel;

  vButtonLayout := FUIBuilder.Layouts.CreateSimpleLayout(lkPanel);
  vButtonLayout.Caption := 'Create?place=embedded';
  vButtonLayout.Hint := TInteractor(FView.Interactor).Translate('cptAddEntity', 'Добавить');
  vButtonLayout.ImageID := 'add';
  vButtonLayout.Button_Flat := True;
  vButtonLayout.Align := lalRight;
  vButtonLayout.TabOrder := -1;
  vButtonLayout.Height := ALayout.Height;
  vButtonLayout.Width := ALayout.Height;

  vAddArea := FOwner.CreateChildArea(FView, vButtonLayout, '');

  FbtnAdd := TcxButton(GetRealControl(vAddArea));
  FbtnAdd.Parent := FBasePanel;

  FDisplayName := 'Name';
  if Assigned(FCreateParams) and (FCreateParams.IndexOfName('DisplayFieldName') > -1) then
    FDisplayName := FCreateParams.Values['DisplayFieldName'];
end;

procedure TDEEntityFieldEditor.DoDeinit;
begin
  inherited;
  FSelectButton.Text := '';
  FSelectButton.Enabled := False;

  FbtnAdd.Visible := True; // todo: create option
end;

type
  TWinControlAccess = class (TWinControl);

procedure TDEEntityFieldEditor.ClosePopup;
begin
  if PopupShowing and (not Focused) and (FSelectPopup.InvokedBy = FControl) then
    FSelectPopup.Close;
end;

procedure TDEEntityFieldEditor.FillEditor;
var
  vEntity: TEntity;
  vDefinition: TDefinition;
begin
  inherited;

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
      Text := vEntity[FDisplayName];
    Hint := Text;
  end;

  UpdateVisibility;

  FbtnAdd.Visible := (FView.State = vsFullAccess) and not Assigned(vEntity) and (not vDefinition.HasFlag(ccSystem));
  FbtnAdd.Enabled := FbtnAdd.Visible;
  // todo: create option
end;

procedure TDEEntityFieldEditor.FocusedChanged(const AFocused: Boolean);
begin
  inherited;
  ClosePopup;
end;

function TDEEntityFieldEditor.GetDisabledBorderStyle: TcxEditBorderStyle;
begin
  if StrToBoolDef(TDomain(TInteractor(FView.Interactor).Domain).UserSettings.GetValue('Core', 'ShowBordersForDisabled'), True) then
    Result := ebsUltraFlat
  else
    Result := ebsNone;
end;

procedure TDEEntityFieldEditor.OnComboMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OnSelectClick(Sender);
end;

procedure TDEEntityFieldEditor.OnEntitySelected(const ANewEntity: TEntity);
begin
  if PopupShowing then
    SetFieldEntity(ANewEntity);

  ClosePopup;
  PostMessage(TWinControl(TWinControl(FControl).Parent).Handle, WM_NEXTDLGCTL, 0, 0);
end;

procedure TDEEntityFieldEditor.DoOnClose(Sender: TObject);
begin
  if FSelectButton.CanFocus then
    FSelectButton.SetFocus;
end;

procedure TDEEntityFieldEditor.OnSelectClick(Sender: TObject);
begin
  if PopupShowing and (FSelectPopup.InvokedBy = FControl) then
    FSelectPopup.Close
  else
  begin
    SetFocused(True);
    ShowPopup('');
  end;
end;

procedure TDEEntityFieldEditor.OnWinControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then
  begin
    if PopupShowing then
   //   ClosePopup // иначе при поиске нельзя использовать пробел
    else
      ShowPopup('');
  end
  else if Key = VK_ESCAPE then
  begin
    if PopupShowing then
      ClosePopup
  end
  else if Key = VK_F2 then
  begin
//    if FbtnAdd.Enabled then
//      DoAdd(0);
  end
  else if Key = VK_F3 then
  begin
//    if FbtnEdit.Enabled then
//      DoEdit;
  end
  else if Key = VK_DOWN then
    ShowPopup('')
  else if IsAlpha(Char(Key)) then
    ShowPopup(VKeytoWideString(Key))
  else if Key = VK_RETURN then

  else if Key in [VK_PRIOR, VK_NEXT] then
    ClosePopup;
end;

function TDEEntityFieldEditor.PopupShowing: Boolean;
begin
  Result := Assigned(FSelectPopup) and FSelectPopup.Form.Visible;
end;

procedure TDEEntityFieldEditor.SetFocused(const Value: Boolean);
begin
  inherited;
  if Value and FSelectButton.CanFocus then
    FSelectButton.SetFocus;
end;

procedure TDEEntityFieldEditor.ShowPopup(const AFilter: string);
var
  vInteractor: TInteractor;
  vField: TEntityField;
  vEntity: TEntity;
  vPrevCursor: TCursorType;
begin
  vEntity := TEntity(FView.FieldEntity);
  vInteractor := TInteractor(FView.Interactor);

  if FEntities = nil then
    FEntities := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  vPrevCursor := TPresenter(vInteractor.Presenter).SetCursor(crtHourGlass);
  try
    vField := FView.ExtractEntityField;
    vField.GetEntitiesForSelect(TInteractor(FView.Interactor).Session, FEntities);

    if FSelectPopup = nil then
    begin
      FSelectPopup := TDESelectEntityPopup.Create(TInteractor(FView.Interactor));
      FSelectPopup.OnClose := DoOnClose;
      FSelectPopup.OnEntitySelected := OnEntitySelected;
    end;

    FSelectPopup.Init(FEntities, True, vEntity, AFilter, FDisplayName);
    FSelectPopup.ShowFor(TWinControl(FControl), '');
  finally
    TPresenter(vInteractor.Presenter).SetCursor(vPrevCursor);
  end;
end;

procedure TDEEntityFieldEditor.UpdateVisibility;
begin
  if (FView.State < vsSelectOnly) then
  begin
    FSelectButton.Visible := False;

    FTextEdit.Visible := True;
    FTextEdit.EditingText := FSelectButton.Text;
    FTextEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else begin
    FTextEdit.Visible := False;

    FSelectButton.OnClick := OnSelectClick;
    FSelectButton.Visible := True;
  end;
end;

{ TDEListEntityFieldEditor }

procedure TDEListEntityFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEntities);
end;

function TDEListEntityFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FGrid := TcxTreeList.Create(nil);
  FGrid.OnDblClick := OnDblClick;
  FGrid.OnFocusedNodeChanged := DoOnFocusedNodeChanged;
  FGrid.CreateColumn;
  FGrid.OptionsView.Headers := False;
  FGrid.OptionsView.ShowRoot := False;
  FGrid.OptionsView.ColumnAutoWidth := True;
  FGrid.OptionsSelection.HideSelection := False;
  FGrid.OptionsSelection.HideFocusRect := False;
  FGrid.OptionsSelection.MultiSelect := False;
  FGrid.OptionsData.Editing := False;
  FGrid.OptionsSelection.CellSelect := False;

  Result := FGrid;
end;

procedure TDEListEntityFieldEditor.DoOnFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode,
  AFocusedNode: TcxTreeListNode);
begin
  if Assigned(FGrid.FocusedNode) then
    SetFieldEntity(TEntity(FGrid.FocusedNode.Data));
end;

procedure TDEListEntityFieldEditor.FillEditor;
var
  vField: TEntityField;
  vPrevCursor: TCursor;
  vInteractor: TInteractor;
begin
  if not Assigned(FView.FieldEntity) then
    Exit;

  if Assigned(FEntities) then
    Exit;

  vInteractor := TInteractor(FView.Interactor);
  FEntities := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  vPrevCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    vField := FView.ExtractEntityField;
    vField.GetEntitiesForSelect(vInteractor.Session, FEntities);
    Reload;
  finally
    Screen.Cursor := vPrevCursor;
  end;
end;

procedure TDEListEntityFieldEditor.OnDblClick(Sender: TObject);
begin

end;

procedure TDEListEntityFieldEditor.Reload;
var
  i: Integer;
  vNode, vSelectedNode: TcxTreeListNode;
  vCurrEntity: TEntity;
begin
  vCurrEntity := nil; vSelectedNode := nil;
  if Assigned(FGrid.FocusedNode) then
    vCurrEntity := TEntity(FGrid.FocusedNode.Data);

  FGrid.BeginUpdate;
  try
    FGrid.Clear;
    for i := 0 to FEntities.Count - 1 do
    begin
      vNode := FGrid.Add;
      vNode.Data := FEntities[i];
      vNode.Values[0] := SafeDisplayName(FEntities[i]);
      if vNode.Data = vCurrEntity then
      begin
        vNode.Selected := True;
        vNode.Focused := True;
        vSelectedNode := vNode;
      end;
    end;
  finally
    FGrid.EndUpdate;
    if Assigned(vSelectedNode) then
      vSelectedNode.MakeVisible;
  end;
end;

{ TDELinkedEntityFieldEditor }

procedure TDELinkedEntityFieldEditor.DoBeforeFreeControl;
begin
  inherited DoBeforeFreeControl;
  FreeAndNil(FLabel);
end;

procedure TDELinkedEntityFieldEditor.OnLabelClick(Sender: TObject);
var
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(FView.Interactor);
  vInteractor.ViewEntity(FView);
end;

procedure TDELinkedEntityFieldEditor.UpdateVisibility;
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
      FLabel := TcxLabel.Create(nil);
      FLabel.Align := alClient;
      FLabel.Parent := FBasePanel;
      FLabel.Style.Font.Color := clBlue;
      FLabel.Style.Font.Style := [fsUnderline];
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
    FTextEdit.EditingText := FSelectButton.Text;
  end
  else
  begin
    FTextEdit.Visible := False;
    if Assigned(FLabel) then
      FLabel.Visible := False;

    FSelectButton.Visible := True;
    FSelectButton.OnClick := OnSelectClick;
  end;
end;

{ TDEEntitySelector }

procedure TDEEntitySelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TDEEntitySelector.DoBeforeFreeControl;
begin
  FreeAndNil(FEntities);
end;

function TDEEntitySelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(FView.Interactor);
  FEntities := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  Result := TcxComboBox.Create(nil);
  TcxComboBox(Result).Properties.DropDownListStyle := lsFixedList;
  TcxComboBox(Result).Properties.OnInitPopup := CBOnInitPopup;

  FFlat := (ALayout.BevelOuter = lbkNone) and (ALayout.BevelInner = lbkNone);
end;

procedure TDEEntitySelector.DoOnChange;
var
  vEntity: TEntity;
begin
  vEntity := TEntity(TcxComboBox(FControl).ItemObject);
  SetFieldEntity(vEntity);
end;

procedure TDEEntitySelector.FillEditor;
var
  vEdit: TcxComboBox;
  vEntity: TEntity;
begin
  FillList;

  vEdit := TcxComboBox(FControl);
  vEntity := FView.FieldEntity;
  if Assigned(vEntity) then
  begin
    vEdit.EditValue := vEntity['Name'];
    vEdit.Enabled := True;
    vEdit.Properties.ReadOnly := FView.State < vsSelectOnly;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.TabStop := False;
    end
    else begin
      if FFlat then begin
        vEdit.Style.BorderStyle := ebsNone;
        vEdit.ParentColor := True;
      end
      else
        vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.TabStop := FOwner.TabStop;
    end;
  end
  else begin
    vEdit.EditValue := '';
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end;
end;

procedure TDEEntitySelector.FillList;
var
  vField: TEntityField;
  vEntity: TEntity;
begin
  TcxComboBox(FControl).Properties.Items.BeginUpdate;
  try
    TcxComboBox(FControl).Properties.Items.Clear;
    vField := FView.ExtractEntityField;
    vField.GetEntitiesForSelect(TInteractor(FView.Interactor).Session, FEntities);
    for vEntity in FEntities do
      TcxComboBox(FControl).Properties.Items.AddObject(SafeDisplayName(vEntity), vEntity);
  finally
    TcxComboBox(FControl).Properties.Items.EndUpdate;
  end;
end;

function TDEEntitySelector.GetDisabledBorderStyle: TcxEditBorderStyle;
begin
  if StrToBoolDef(TDomain(FView.Domain).UserSettings.GetValue('Core', 'ShowBordersForDisabled'), True) and not FFlat then
    Result := ebsUltraFlat
  else
    Result := ebsNone;
end;

procedure TDEEntitySelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TcxComboBox(FControl).Properties) then
    TcxComboBox(FControl).Properties.OnChange := AHandler;
end;

{ TDEEntityFieldListEditor }

procedure TDEEntityFieldListEditor.CleanChildViews;
var
  vChildView: TView;
begin
  if Assigned(FView) then
  begin
    for vChildView in FChildViews do
      vChildView.RemoveListener(Self);
    FChildViews.Clear;
  end;
end;

function TDEEntityFieldListEditor.CreateCategoryRow(const ARootEntity: TEntity; const AFieldDef: TFieldDef): TcxCategoryRow;
begin
  Result := TcxCategoryRow(FEditor.Add(TcxCategoryRow));
  Result.Properties.Caption := FOwner.GetFieldTranslation(AFieldDef);
  Result.Properties.Hint := FOwner.GetFieldTranslation(AFieldDef, tpHint);
  Result.Tag := NativeInt(TRowData.Create(ARootEntity, AFieldDef));
end;

function TDEEntityFieldListEditor.CreateEditRow(const ARootEntity: TEntity; const AFieldDef: TFieldDef;
  const AViewPath: string; const AOverriddenCaption: string = ''): TcxEditorRow;
var
  vFieldView: TView;
  vStyleName: string;
begin
  Result := TcxEditorRow(FEditor.Add(TcxEditorRow));
  vStyleName := GetUrlCommand(AFieldDef.StyleName);
  if AOverriddenCaption = '' then
    Result.Properties.Caption := FOwner.GetFieldTranslation(AFieldDef)
  else
    Result.Properties.Caption := AOverriddenCaption;

  Result.Tag := NativeInt(TRowData.Create(ARootEntity, AFieldDef));

  Result.Properties.EditPropertiesClassName := 'TcxTextEditProperties';
  case AFieldDef.Kind of
    fkNotDefined: ;
    fkString: ;
    fkInteger: Result.Properties.EditPropertiesClassName := 'TcxSpinEditProperties';
    fkEnum: Result.Properties.EditPropertiesClassName := 'TcxComboBoxProperties';
    fkFlag: ;
    fkFloat:
    begin
      Result.Properties.EditPropertiesClassName := 'TcxSpinEditProperties';
      TcxSpinEditProperties(Result.Properties.EditProperties).ValueType := vtFloat;
      if Length(AFieldDef.Format) > 0 then
        TcxSpinEditProperties(Result.Properties.EditProperties).DisplayFormat := AFieldDef.Format;
    end;
    fkDateTime:
    begin
      Result.Properties.EditPropertiesClassName := 'TcxDateEditProperties';
      if AFieldDef.StyleName = 'datetime' then
        TcxDateEditProperties(Result.Properties.EditProperties).Kind := ckDateTime;
    end;
    fkBoolean: Result.Properties.EditPropertiesClassName := 'TcxCheckBoxProperties';
    fkColor: Result.Properties.EditPropertiesClassName := 'TdxColorEditProperties';
    fkCurrency: ;
    fkObject: Result.Properties.EditPropertiesClassName := 'TcxComboBoxProperties';
    fkList: ;
    fkBlob: ;
    fkComplex: ;
  end;

  vFieldView := FView.BuildView(AViewPath + AFieldDef.Name);
  vFieldView.AddListener(Self);
  FChildViews.Add(vFieldView);
  Result.Properties.EditProperties.ReadOnly := (vFieldView.State < vsSelectOnly);
  Result.Properties.EditProperties.ValidationOptions := [evoShowErrorIcon];

  if Result.Properties.EditProperties.ReadOnly then
    Result.Properties.Options.ShowEditButtons := eisbNever
  else
    Result.Properties.EditProperties.OnChange := OnFieldChange;
end;

procedure TDEEntityFieldListEditor.CreateRows(const ARootEntity: TEntity; const ARootRow: TcxCustomRow;
  const AViewPath: string; const ARootEntityIndex: Integer = -1);
var
  vFieldDef: TFieldDef;
  vListField: TListField;
  vCategory: TcxCategoryRow;
  i: Integer;
begin
  if ARootEntity = nil then Exit;

  for vFieldDef in ARootEntity.Definition.Fields do
  begin
    if Assigned(FDisplayFields) then
    begin
      if FDisplayFields.IndexOf(vFieldDef.Name) < 0 then Continue;
    end;

    if vFieldDef.Kind = fkList then
    begin
      vListField := TListField(ARootEntity.FieldByName(vFieldDef.Name));
      if vListField.Count > 0 then
      begin
        vCategory := CreateCategoryRow(ARootEntity, vFieldDef);
        vCategory.Parent := ARootRow;
        for i := 0 to vListField.Count - 1 do
          CreateRows(vListField[i], vCategory, vFieldDef.Name + '/' + IntToStr(i) + '/', i);
      end;
    end
    else
    begin
      if vFieldDef.HasFlag(cHideInEdit) or (vFieldDef.UIState < vsDisabled) then
        Continue;
      CreateEditRow(ARootEntity, vFieldDef, AViewPath, FOwner.GetFieldTranslation(vFieldDef) + ' ' +
        IfThen(ARootEntityIndex > -1, IntToStr(ARootEntityIndex + 1))).Parent := ARootRow;
    end;
  end;
end;

procedure TDEEntityFieldListEditor.DoBeforeFreeControl;
var
  vChildView: TView;
begin
  inherited;

  for vChildView in FChildViews do
    vChildView.RemoveListener(Self);

  FreeAndNil(FDisplayFields);
  FreeEditors;
  FreeAndNil(FChildViews);
end;

function TDEEntityFieldListEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FEditor := TcxVerticalGrid.Create(nil);
  FEditor.OptionsView.ShowEditButtons := ecsbFocused;
  FEditor.OptionsView.PaintStyle := psDelphi;
  FEditor.BorderStyle := cxcbsNone;

  Result := FEditor;

  FChildViews := TList<TView>.Create;

  if Assigned(FCreateParams) then
    FEditor.OptionsView.RowHeaderWidth := StrToIntDef(FCreateParams.Values['headerWidth'], 150)
  else
    FEditor.OptionsView.RowHeaderWidth := 150;

  if FCreateParams.IndexOfName('Fields') > 0 then
  begin
    FDisplayFields := TStringList.Create;
    FDisplayFields.StrictDelimiter := True;
    FDisplayFields.Delimiter := ',';
    FDisplayFields.DelimitedText := FCreateParams.Values['Fields'];
  end;

  FReadOnly := FCreateParams.Values['ViewState'] = 'ReadOnly';
  FEditor.OptionsData.Editing := not FReadOnly;

  FDomainObject := TEntity(FView.DomainObject);
  CreateRows(FDomainObject, nil, '');
end;

procedure TDEEntityFieldListEditor.DoDisableContent;
begin
  CleanChildViews;
end;

procedure TDEEntityFieldListEditor.FillEditor;
var
  i, j: Integer;
  vRow: TcxEditorRow;
  vFieldDef: TFieldDef;
  vEntityField: TEntityField;
  vEntities: TEntityList;
  vInteractor: TInteractor;
  vLevelEntity: TEntity;
  vEnum: TEnumeration;
  vEnumItem: TEnumItem;
//  vList: TList<TEntity>;
begin
  FEditor.BeginUpdate;

  try
    vInteractor := TInteractor(FView.Interactor);
    for i := 0 to FEditor.Rows.Count - 1 do
    begin
      if FEditor.Rows[i] is TcxCategoryRow then Continue;

      vRow := TcxEditorRow(FEditor.Rows[i]);
      vFieldDef := TRowData(vRow.Tag).FieldDef;
      vLevelEntity := TRowData(vRow.Tag).Entity;

      case vFieldDef.Kind of
        {fkNotDefined: ;
        fkString: ;
        fkInteger: vRow.Properties.EditPropertiesClassName := 'TcxSpinEditProperties';
        fkFlag: ;
        fkFloat: ;
        fkDateTime: vRow.Properties.EditPropertiesClassName := 'TcxDateEditProperties';
        fkBoolean: vRow.Properties.EditPropertiesClassName := 'TcxCheckBoxProperties';
        fkColor: vRow.Properties.EditPropertiesClassName := 'TdxColorEditProperties';
        fkCurrency: ; }
        fkEnum:
          begin
            vEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(vFieldDef).Dictionary);
            if not Assigned(vEnum) then
              vEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(vFieldDef).Dictionary);
            vRow.Properties.EditProperties.BeginUpdate;
            try
              TcxComboBoxProperties(vRow.Properties.EditProperties).Items.Clear;
              for j := 0 to vEnum.Count - 1 do
              begin
                vEnumItem := vEnum[j];
                if not vFieldDef.HasFlag(cRequired) or (vEnumItem.ID > 0) then
                begin
                  TcxComboBoxProperties(vRow.Properties.EditProperties).Items.AddObject(vEnumItem.DisplayText, TObject(vEnumItem.ID));
                  if vEnumItem.ID = vLevelEntity.FieldByName(vFieldDef.Name).Value then
                    vRow.Properties.Value := vEnumItem.DisplayText;
                end;
              end;
            finally
              vRow.Properties.EditProperties.EndUpdate;
            end;
          end;
        fkObject:
          begin
            if vRow.Properties.DataBinding.Data = nil then
            begin
              vEntities := TEntityList.Create(vInteractor.Domain, vInteractor.Session);
              vRow.Properties.DataBinding.Data := vEntities;
            end
            else
              vEntities := TEntityList(vRow.Properties.DataBinding.Data);

            vEntityField := TEntityField(vLevelEntity.FieldByName(vFieldDef.Name));

            vEntityField.GetEntitiesForSelect(vInteractor.Session, vEntities);
            vRow.Properties.EditProperties.BeginUpdate;
            try
              TcxComboBoxProperties(vRow.Properties.EditProperties).Items.Clear;
              for j := 0 to vEntities.Count - 1 do
              begin
                TcxComboBoxProperties(vRow.Properties.EditProperties).Items.AddObject(SafeDisplayName(vEntities[j]), vEntities[j]);
                if vEntities[j] = vEntityField.Entity then
                  vRow.Properties.Value := vEntityField.Entity.DisplayName;
              end;
            finally
              vRow.Properties.EditProperties.EndUpdate;
            end;
          end;
        fkList:
          begin
  //          vList := vLevelEntity.GetFieldList(vFieldDef.Name);
          end;
        {fkBlob: ;
        fkComplex: ;  }
        else
          vRow.Properties.Value := vLevelEntity.FieldByName(vFieldDef.Name).Value;
      end;
    end;
  finally
    FEditor.EndUpdate;
  end;
end;

procedure TDEEntityFieldListEditor.FreeEditors;
var
  i: Integer;
  vRow: TcxEditorRow;
begin
  for i := 0 to FEditor.Rows.Count - 1 do
  begin
    if FEditor.Rows[i] is TcxEditorRow then
    begin
      vRow := TcxEditorRow(FEditor.Rows[i]);
      if TRowData(vRow.Tag).FieldDef.Kind = fkObject then
      begin
        TEntityList(vRow.Properties.DataBinding.Data).Free;
        vRow.Properties.DataBinding.Data := nil;
      end;
      TcxEditorRow(FEditor.Rows[i]).Properties.EditProperties.OnChange := nil;
    end;
    TRowData(TcxCustomRow(FEditor.Rows[i]).Tag).Free;
  end;
  FEditor.ClearRows;
end;

procedure TDEEntityFieldListEditor.RefillArea(const AKind: Word);
var
  vEntity: TEntity;
begin
  vEntity := TEntity(FView.ParentDomainObject);
  if not Assigned(vEntity) then
    Exit;

  try
    SwitchChangeHandlers(nil);
    if not Assigned(FView.ParentDomainObject) then
      DoDeinit
    else if (AKind <> dckViewStateChanged) and (AKind <> dckNameChanged) then
      FillEditor;

    SwitchChangeHandlers(FOwner.OnChange);
  except
    TInteractor(FView.Interactor).ShowMessage('Error in FillEditorFromModel, Field: ' + FFieldDef.Name);
  end;
end;

procedure TDEEntityFieldListEditor.OnFieldChange(Sender: TObject);
var
  vEditor: TcxCustomEdit;
  vRow: TcxCustomRow;
  vFieldDef: TFieldDef;
  vValue: Variant;
  vEntity, vLevelEntity: TEntity;
  vIndex, vId: Integer;
begin
  vEditor := TcxCustomEdit(Sender);
  vRow := TcxCustomVerticalGrid(vEditor.Parent).FocusedRow;
  if vRow is TcxEditorRow then
  begin
    vFieldDef := TRowData(TcxEditorRow(vRow).Tag).FieldDef;
    vLevelEntity := TRowData(TcxEditorRow(vRow).Tag).Entity;

    vValue := vEditor.EditingValue;
    case vFieldDef.Kind of
      fkNotDefined: ;
      fkString: ;
      fkInteger: ;
      fkEnum:
        begin
          vIndex := TcxComboBoxProperties(TcxEditorRow(vRow).Properties.EditProperties).Items.IndexOf(vValue);
          if vIndex > -1 then
          begin
            vId := Integer(TcxComboBoxProperties(TcxEditorRow(vRow).Properties.EditProperties).Items.Objects[vIndex]);

            TUserSession(FView.Session).DomainWrite(procedure
              begin
                vLevelEntity._SetFieldValue(TChangeHolder(FOwner.Holder), vFieldDef.Name, vId);
              end);
          end;

          Exit;
        end;
      fkFlag: ;
      fkFloat: vValue := Double(vValue);
      fkDateTime: ;
      fkBoolean: ;
      fkColor: vValue := Integer(vValue);
      fkCurrency: ;
      fkObject:
        begin
          vIndex := TcxComboBoxProperties(TcxEditorRow(vRow).Properties.EditProperties).Items.IndexOf(vValue);
          if vIndex > -1 then
          begin
            vEntity := TEntity(TcxComboBoxProperties(TcxEditorRow(vRow).Properties.EditProperties).Items.Objects[vIndex]);

            TUserSession(FView.Session).DomainWrite(procedure
              begin
                vLevelEntity._SetFieldEntity(TChangeHolder(FOwner.Holder), vFieldDef.Name, vEntity);
              end);
          end;

          Exit;
        end;
      fkList: ;
      fkBlob: ;
      fkComplex: ;
    end;

    TUserSession(FView.Session).DomainWrite(procedure
      begin
        vLevelEntity._SetFieldValue(TChangeHolder(FOwner.Holder), vFieldDef.Name, vValue);
      end);
  end;
end;

{ TRowData }

constructor TRowData.Create(const AEntity: TEntity; const AFieldDef: TFieldDef);
begin
  Entity := AEntity;
  FieldDef := AFieldDef;
end;

{ TDERadioEntitySelector }

procedure TDERadioEntitySelector.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEntities);
end;

function TDERadioEntitySelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TcxRadioGroup.Create(nil);
  TcxRadioGroup(Result).Transparent := True;
  TcxRadioGroup(Result).Style.BorderStyle := ebsNone;
  TcxRadioGroup(Result).Name := 'radio';
  TcxRadioGroup(Result).Caption := '';
  FNeedCreateCaption := False;

  FEntities := TEntityList.Create(TInteractor(FView.Interactor).Domain, TInteractor(FView.Interactor).Session);
end;

procedure TDERadioEntitySelector.DoOnChange;
var
  vIndex: Integer;
begin
//  if FFieldDef.HasFlag(cRequired) then
//    vIndex := TcxRadioGroup(FControl.Control).ItemIndex + 1
//  else
  vIndex := TcxRadioGroup(FControl).ItemIndex;
  SetFieldEntity(TEntity(TcxRadioGroup(FControl).Properties.Items[vIndex].Tag));
end;

procedure TDERadioEntitySelector.FillEditor;
var
  vRadioEdit: TcxRadioGroup;
begin
  FillList;

  vRadioEdit := TcxRadioGroup(FControl);
  if VarIsNull(FView.FieldValue) or (FView.FieldValue = 0) then
  begin
    // do nothing
  end
  else
  begin
    vRadioEdit.Properties.ReadOnly := FView.State < vsSelectOnly;
    vRadioEdit.Enabled := not vRadioEdit.Properties.ReadOnly;

    if vRadioEdit.Properties.ReadOnly then
      vRadioEdit.TabStop := False
    else
      vRadioEdit.TabStop := FOwner.TabStop;
  end;
end;

procedure TDERadioEntitySelector.FillList;
var
  vRadioItems: TcxRadioGroupItems;
  vRadioItem: TcxRadioGroupItem;
  vField: TEntityField;
  i: Integer;
  vEnt: TEntity;
begin
  vField := FView.ExtractEntityField;
  vField.GetEntitiesForSelect(TInteractor(FView.Interactor).Session, FEntities);

  vRadioItems := TcxRadioGroup(FControl).Properties.Items;
  vRadioItems.BeginUpdate;
  try
    vRadioItems.Clear;
    for i := 0 to FEntities.Count - 1 do
    begin
      vEnt := FEntities[i];

      vRadioItem := vRadioItems.Add;
      vRadioItem.Caption := SafeDisplayName(vEnt);
      vRadioItem.Tag := NativeInt(vEnt);
      if vEnt = TEntity(NativeInt(FView.FieldValue)) then
        TcxRadioGroup(FControl).ItemIndex := TcxRadioGroup(FControl).Properties.Items.Count - 1;
    end;
  finally
    vRadioItems.EndUpdate;
  end;
end;

procedure TDERadioEntitySelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TcxRadioGroup(FControl).Properties.OnChange := AHandler;
end;

{ TDESelectEntityPopup }

constructor TDESelectEntityPopup.Create(const AInteractor: TInteractor);
begin
  inherited;

  FTopPanel := TPanel.Create(nil);
  FTopPanel.Height := 46;
  FTopPanel.Align := alTop;
  FTopPanel.Parent := FInnerControl;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.TabOrder := 0;

  FTopLabel := TLabel.Create(nil);
  FTopLabel.Parent := FTopPanel;
  FTopLabel.Align := alTop;
  FTopLabel.AlignWithMargins := True;
  FTopLabel.AutoSize := False;
  FTopLabel.Height := 17;

  FFilterEdit := TEdit.Create(nil);
  with FFilterEdit do
  begin
    Parent := FTopPanel;
    Left := 0;
    Top := FTopLabel.Top + FTopLabel.Height + 4;
    TabOrder := 0;
//    OnKeyDown := OnWinControlKeyDown;
    Width := 60;
    MaxLength := 12;
    AutoSelect := False;
  end;

  FList := TcxTreeList.Create(nil);
  FList.Parent := FInnerControl;
  FList.Align := alClient;
  FList.OnKeyDown := OnKeyDown;
  FList.OnDblClick := OnDblClick;
  FList.CreateColumn;
  FList.OptionsView.Headers := False;
  FList.OptionsView.ShowRoot := False;
  FList.OptionsView.ColumnAutoWidth := True;
  FList.OptionsSelection.HideSelection := False;
  FList.OptionsSelection.HideFocusRect := False;
  FList.OptionsSelection.MultiSelect := False;
  FList.OptionsData.Editing := False;
  FList.OptionsSelection.CellSelect := False;
  FList.Font.Size := 11;

  FBottomLabel := TLabel.Create(nil);
  FBottomLabel.Parent := FInnerControl;
  FBottomLabel.Align := alBottom;
  FBottomLabel.AlignWithMargins := True;
  FBottomLabel.Height := 17;
end;

destructor TDESelectEntityPopup.Destroy;
begin
  FreeAndNil(FTopLabel);
  FreeAndNil(FTopPanel);
  FreeAndNil(FList);
  FreeAndNil(FBottomLabel);
  inherited;
end;

procedure TDESelectEntityPopup.DoOnBeforeShow;
begin
  inherited;
  Reload;
end;

procedure TDESelectEntityPopup.Init(const AItems: TEntityList; const AIsSelectMode: Boolean;
  const ASelectedEntity: TEntity; const AFilter, ADisplayName: string);
var
  i: Integer;
  vCaption: string;
  vDomain: TDomain;
begin
  FAllData := AItems;
  FDisplayName := ADisplayName;

  if FAllData.Filler is TEntityField then
    FSearchType := TEntityField(FAllData.Filler).SearchType
  else
    FSearchType := stSearchFromBegin;

  vDomain := TDomain(FInteractor.Domain);

  if AItems.Count = 0 then
  begin
    FList.Visible := False;
    FFilterEdit.Visible := False;
    FTopLabel.Caption := vDomain.TranslateDefinition(AItems.MainDefinition) + ' ' +
      FInteractor.Translate('txtNotSelected', 'отсутствуют') + ' ';
    Sizeable := False;
    Color := clInfoBk;
    FBottomLabel.Visible := False;
  end
  else
  begin
    FList.Visible := True;
    FSelectedEntity := ASelectedEntity;

    if AItems.Count > 10 then
    begin
      FFilterText := AnsiUpperCase(Trim(AFilter));
      if Length(FFilterText) > 0 then
      begin
        FFilterEdit.OnChange := nil;
        FFilterEdit.Text := FFilterText;
        FFilterEdit.SelStart := 1000;
      end;
      FFilterEdit.Visible := True;
      FFilterEdit.OnChange := OnFilterChange;

      vCaption := '';
      for i := 0 to AItems.ContentDefinitions.Count - 1 do
      begin
        if Length(vCaption) > 0 then
          vCaption := vCaption + ', ';
        vCaption := vCaption + vDomain.TranslateDefinition(AItems.ContentDefinitions[i]);
      end;
      FTopPanel.Visible := True;
      FTopLabel.Caption := FInteractor.Translate('cptSelection', 'Выбор') + ': ' + vCaption;
      FBottomLabel.Visible := True;
    end
    else
    begin
      FTopPanel.Visible := False;
      FBottomLabel.Visible := False;
    end;
  end;
end;

procedure TDESelectEntityPopup.OnFilterChange(Sender: TObject);
begin
  FFilterText := AnsiUpperCase(Trim(FFilterEdit.Text));
  Reload;
end;

procedure TDESelectEntityPopup.OnDblClick(Sender: TObject);
begin
  if Assigned(FOnEntitySelected) and Assigned(FList.FocusedNode) then
    FOnEntitySelected(TEntity(FList.FocusedNode.Data));
  Close;
end;

procedure TDESelectEntityPopup.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    OnDblClick(Self);
end;

function TDESelectEntityPopup.IsMatchToFilter(const AEntity: TEntity): Boolean;
var
  vText: string;
  vST: TSearchType;
begin
  Result := Assigned(AEntity);
  if not Result then
    Exit(True);

  if AEntity.Deleted then
    Exit(False);

  if Length(FFilterText) > 0 then
  begin
    vText := AnsiUpperCase(AEntity.DisplayName);
    vST := FSearchType;
    Result :=
      ((vST = stSearchFromBegin) and (Pos(FFilterText, vText) = 1)) or
      ((vST = stSearchEverywhere) and (Pos(FFilterText, vText) > 0)) or
      ((vST = stSearchMultiEntrance) and IsTextMultiEqual(FFilterText, vText));
  end;
end;

procedure TDESelectEntityPopup.Reload;
var
  i: Integer;
  vEntity: TEntity;
  vNode: TcxTreeListNode;
begin
  FList.BeginUpdate;
  try
    FList.Clear;

    FAllData.Resort;
    for i := 0 to FAllData.Count - 1 do
    begin
      vEntity := FAllData[i];

      if IsMatchToFilter(vEntity) then
      begin
        vNode := FList.Add;
        vNode.Data := vEntity;
        if not Assigned(vEntity) then
          vNode.Values[0] := FAllData.MainDefinition._EmptyValue
        else
          vNode.Values[0] := vEntity[FDisplayName];
        if vEntity = FSelectedEntity then
          vNode.Focused := True;
      end;
    end;

  finally
    FList.EndUpdate;
    UpdateCount;
    UpdateSizes;
  end;
end;

procedure TDESelectEntityPopup.UpdateCount;
begin
  FBottomLabel.Caption := 'Количество записей: ' + IntToStr(FList.Count);
end;

type
  TcxTreeListAccess = class (TcxTreeList);
  TcxTreeListNodeAccess = class (TcxTreeListNode);

procedure TDESelectEntityPopup.UpdateSizes;
var
  vHeight, vRowCount, vRowHeight: Integer;
begin
  vHeight := 0;

  if FTopPanel.Visible then
  begin
    if FFilterEdit.Visible then
    begin
      if FTopPanel.AlignWithMargins then
        vHeight := vHeight + FTopPanel.Margins.ExplicitHeight
      else
        vHeight := vHeight + FTopPanel.Height;
    end
    else
    begin
      if FTopLabel.AlignWithMargins then
        vHeight := vHeight + FTopLabel.Margins.ExplicitHeight
      else
        vHeight := vHeight + FTopLabel.Height;
    end;
  end;

  if FBottomLabel.Visible then
  begin
    if FBottomLabel.AlignWithMargins then
      vHeight := vHeight + FBottomLabel.Margins.ExplicitHeight
    else
      vHeight := vHeight + FBottomLabel.Height;
  end;

  if FList.Visible then
  begin
    TcxTreeListAccess(FList).ViewInfo.Calculate;
    vRowCount := FList.Count;
    vRowHeight := 0;
    if (vRowCount > 0) then
    begin
      if Assigned(TcxTreeListNodeAccess(FList.Items[0]).ViewData) then
        vRowHeight := TcxTreeListNodeAccess(FList.Items[0]).ViewData.Height
      else
        vRowHeight := 22;// default row height
    end;

    if vRowCount > 10 then vRowCount := 10;
    vHeight := vHeight + vRowCount * vRowHeight;
  end;

  if vHeight > 0 then
  begin
    FInnerControl.Width := InvokedBy.Width;
    FInnerControl.ClientHeight := vHeight + 2;
    if FList.Visible and Assigned(FList.FocusedNode) then
      FList.FocusedNode.MakeVisible;
  end
  else
  begin
    FInnerControl.ClientHeight := 0;
    FInnerControl.Width := 0;
  end;
end;

{ TDERadioEntitySelector2 }

procedure TDERadioEntitySelector2.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEntities);
end;

function TDERadioEntitySelector2.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vRadioItem: TcxRadioButton;
begin
  Result := TPanel.Create(nil);
  TPanel(Result).Name := 'radio';
  TPanel(Result).Caption := '';
  TPanel(Result).Align := alClient;
  TPanel(Result).BevelOuter := bvNone;
  FNeedCreateCaption := False;

  FEntities := TEntityList.Create(TInteractor(FView.Interactor).Domain, TInteractor(FView.Interactor).Session);

  vRadioItem := TcxRadioButton.Create(nil);
  vRadioItem.Parent := TPanel(Result);
  FItemHeight := vRadioItem.Height;
  vRadioItem.Free;
end;

procedure TDERadioEntitySelector2.DoOnChange;
begin
end;

procedure TDERadioEntitySelector2.FillEditor;
var
  vRadioEdit: TPanel;
begin
  FillList;

  vRadioEdit := TPanel(FControl);
  if VarIsNull(FView.FieldValue) or (FView.FieldValue = 0) then
  begin
    // do nothing
  end
  else
  begin
    vRadioEdit.Enabled := FView.State > vsSelectOnly;

    if vRadioEdit.Enabled then
      vRadioEdit.TabStop := FOwner.TabStop
    else
      vRadioEdit.TabStop := False;
  end;
end;

procedure TDERadioEntitySelector2.FillList;
var
  vRadioEdit: TPanel;
  vRadioItem: TcxRadioButton;
  vField: TEntityField;
  i, c: Integer;
  vEnt: TEntity;
  vStepSize, vVisibleItemCount: Integer;
begin
  vField := FView.ExtractEntityField;
  vField.GetEntitiesForSelect(TInteractor(FView.Interactor).Session, FEntities);

  vRadioEdit := TPanel(FControl);

  while vRadioEdit.ControlCount > 0 do
    vRadioEdit.Controls[0].Free;

  vVisibleItemCount := 0;
  for i := 0 to FEntities.Count - 1 do
  begin
    vEnt := FEntities[i];
    if not Assigned(vEnt) then Continue; // todo: надо обработать cRequired
    Inc(vVisibleItemCount);
  end;

  vStepSize := (vRadioEdit.Height - FItemHeight) div (vVisibleItemCount - 1);
  c := 0;
  for i := 0 to FEntities.Count - 1 do
  begin
    vEnt := FEntities[i];
    if not Assigned(vEnt) then Continue; // todo: надо обработать cRequired

    vRadioItem := TcxRadioButton.Create(nil);
    vRadioItem.Width := vRadioEdit.Width;
    //vRadioItem.AutoSize := True;
    vRadioItem.Parent := vRadioEdit;
    vRadioItem.Caption := SafeDisplayName(vEnt);
    vRadioItem.Tag := NativeInt(vEnt);
    vRadioItem.Top := vStepSize * c;
    vRadioItem.OnClick := OnChange;
    if vEnt = TEntity(NativeInt(FView.FieldValue)) then
      vRadioItem.Checked := True;
    Inc(c);
  end;
end;

procedure TDERadioEntitySelector2.OnChange(Sender: TObject);
begin
  SetFieldEntity(TEntity(TcxRadioButton(Sender).Tag));
end;

procedure TDERadioEntitySelector2.SwitchChangeHandlers(const AHandler: TNotifyEvent);
var
  i: Integer;
  vRadioEdit: TPanel;
begin
  vRadioEdit := TPanel(FControl);
  for i := 0 to vRadioEdit.ControlCount - 1 do
    if Assigned(AHandler) then
      TcxRadioButton(vRadioEdit.Controls[i]).OnClick := OnChange
    else
      TcxRadioButton(vRadioEdit.Controls[i]).OnClick := nil;
end;

initialization

TPresenter.RegisterControlClass('Windows.DevExpress', uiEntityEdit, '', TDEEntityFieldEditor);
TPresenter.RegisterControlClass('Windows.DevExpress', uiEntityEdit, 'simple', TDEEntityFieldEditor);
TPresenter.RegisterControlClass('Windows.DevExpress', uiEntityEdit, 'list', TDEListEntityFieldEditor);
TPresenter.RegisterControlClass('Windows.DevExpress', uiEntityEdit, 'link', TDELinkedEntityFieldEditor);
TPresenter.RegisterControlClass('Windows.DevExpress', uiEntityEdit, 'select', TDEEntitySelector);
TPresenter.RegisterControlClass('Windows.DevExpress', uiEntityEdit, 'fieldlist', TDEEntityFieldListEditor);
TPresenter.RegisterControlClass('Windows.DevExpress', uiEntityEdit, 'radio', TDERadioEntitySelector2);

end.
