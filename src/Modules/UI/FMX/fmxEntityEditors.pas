﻿{---------------------------------------------------------------------------------
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

unit fmxEntityEditors;

interface

uses
  Classes, Generics.Collections, System.UITypes,
  FMX.StdCtrls, FMX.ExtCtrls, FMX.Controls, FMX.Types, FMX.ComboEdit, FMX.Edit, FMX.Menus,
  FMXPopupForm, uUIBuilder, uView, uEntity, uEntityList, uDefinition, uFMXArea, uBaseLayout;

type
  {TFMXEntitySelector = class(TFMXFieldArea)
  private
    FFlat: Boolean;
    FEntities: TEntityList;
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
    function GetDisabledBorderStyle: TcxEditBorderStyle;
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFMXEntityFieldEditor = class(TFMXFieldArea)
  private
    FBasePanel: TPanel;
    FSelectButton: TComboEdit;
    FTextEdit: TEdit;
    FbtnAdd: TButton;
    FEntities: TEntityList;
//    FSelectPopup: TSelectEntityPopup;
    FButtonView: TView;
    procedure OnSelectClick(Sender: TObject);
    procedure OnAddClick(Sender: TObject);
    procedure ShowPopup(const AFilter: string);
    procedure DoOnClose(Sender: TObject);
    procedure ClosePopup;
    function PopupShowing: Boolean;
    procedure OnEntitySelected(const ANewEntity: TEntity);
    procedure OnWinControlKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure OnComboMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnActionMenuSelected(Sender: TObject);
  protected
    procedure UpdateVisibility; virtual;
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SetFocused(const Value: Boolean); override;
    procedure FocusedChanged(const AFocused: Boolean); override;
    procedure DoDeinit; override;
  end;

  {TRadioEntitySelector = class(TFMXFieldArea)
  private
    FEntities: TEntityList;
    procedure FillList;
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFMXLinkedEntityFieldEditor = class(TFMXEntityFieldEditor)
  private
    FLabel: TcxLabel;
    procedure OnLabelClick(Sender: TObject);
  protected
    procedure DoBeforeFreeControl; override;
    procedure UpdateVisibility; override;
  end;

  TListEntityFieldEditor = class (TFMXFieldArea)
  private
    FGrid: TcxTreeList;
    FEntities: TEntityList;
    procedure OnDblClick(Sender: TObject);
    procedure DoOnFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    procedure Reload;
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;

  TEntityFieldListEditor = class(TFMXFieldArea) // property editor
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
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure DoBeforeFreeControl; override;
    procedure DoDisableContent; override;
    procedure FillEditor; override;
    procedure RefillArea(const AKind: Word); override;
  end;   }

implementation

uses
  FMX.Graphics, SysUtils, FMX.Forms, FMX.ImgList,
  uInteractor, uDomain, uChangeManager, uObjectField, uConsts, uSession,  Variants,
  uPresenter, StrUtils, uUtils, uEnumeration;

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

{function VKeytoWideString(const AKey: Word): WideString;
var
  WBuff         : array [0..255] of WideChar;
  KeyboardState : TKeyboardState;
  UResult       : Integer;
begin
  Result := '';
  GetKeyBoardState (KeyboardState);
  ZeroMemory(@WBuff[0], SizeOf(WBuff));
  UResult := ToUnicode(AKey, MapVirtualKey(AKey, 0), KeyboardState, WBuff, Length(WBuff), 0);
  if UResult > 0 then
    SetString(Result, WBuff, UResult)
  else if UResult = -1 then
    Result := WBuff;
end;       }

{ TFMXEntityFieldEditor

procedure TFMXEntityFieldEditor.DoBeforeFreeControl;
begin
  inherited;

  if Assigned(FButtonView) then
    FButtonView.RemoveListener(Self);

  if Assigned(FSelectPopup) then
  begin
    FSelectPopup.Free;
    FSelectPopup := nil;
  end;
  FreeAndNil(FEntities);
  FreeAndNil(FTextEdit);
end;

procedure TFMXEntityFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
var
  vDefinitions: TList<TDefinition>;
  i: Integer;
  vMenuItem: TMenuItem;
  vDefinition: TDefinition;
begin
  if Assigned(FSelectPopup) then
  begin
    FSelectPopup.Free;
    FSelectPopup := nil;
  end;
  FEntities := nil;
  FButtonView := nil;
  FBasePanel := TPanel.Create(nil);
//  FBasePanel.ShowCaption := False;

  FSelectButton := TComboEdit.Create(nil);
  with FSelectButton do
  begin
    Parent := FBasePanel;
    Align := TAlignLayout.alClient;
    Width := FBasePanel.Width;
    OnKeyDown := OnWinControlKeyDown;
    OnMouseDown := OnComboMouseDown;
//    ReadOnly := True;
//    AutoSelect := False;
  end;

  FSelectButton.Enabled := True;

  FTextEdit := TEdit.Create(nil);
  FTextEdit.Parent := FBasePanel;
  FTextEdit.Align := TAlignLayout.Client;
  FTextEdit.Visible := False;
  FTextEdit.ReadOnly := True;
  FTextEdit.Text := '';
  FTextEdit.TabStop := False;

  FControl := FBasePanel;

  FbtnAdd := TButton.Create(nil);
  TControl(FbtnAdd).Parent := FBasePanel;
  FbtnAdd.Text := TInteractor(FView.Interactor).Translate('cptAddEntity', 'Добавить');
  FbtnAdd.Hint := FbtnAdd.Text;
  FbtnAdd.Images := TImageList(TInteractor(Interactor).Images[16]);
  FbtnAdd.ImageIndex := 1;
  FbtnAdd.CanFocus := False;
  FbtnAdd.Align := TAlignLayout.Right;
  FbtnAdd.TabStop := False;
  FButtonView := FView.BuildView('Create?place=embedded');
  FButtonView.AddListener(Self);

  {vDefinitions := TEntityFieldDef(FFieldDef).ContentDefinitions;
  if vDefinitions.Count > 1 then
  begin
    FPopupMenu := TPopupMenu.Create(nil);
    FPopupMenu.Images := TImageList(TInteractor(Interactor).Images[16]);
    for i := 0 to vDefinitions.Count - 1 do
    begin
      vDefinition := TDefinition(vDefinitions[i]);
      vMenuItem := TMenuItem.Create(nil);
      vMenuItem.Text := GetTranslation(vDefinition);
      vMenuItem.ImageIndex := GetImageID(vDefinition._ImageID);
      vMenuItem.Tag := Integer(FbtnAdd);
      vMenuItem.OnClick := OnActionMenuSelected;
      FPopupMenu.AddObject(vMenuItem);
    end;
    FbtnAdd.PopupMenu := FPopupMenu;
    FbtnAdd.Kind := cxbkOfficeDropDown;
    FbtnAdd.Width := FbtnAdd.Height + 16;
  end
  else
  begin
    FbtnAdd.OnClick := OnAddClick;
    FbtnAdd.Width := FbtnAdd.Height;
//  end;
end;

procedure TFMXEntityFieldEditor.DoDeinit;
begin
  inherited;
  FSelectButton.Enabled := False;
  FbtnAdd.Visible := True; // todo: create option
end;

procedure TFMXEntityFieldEditor.OnActionMenuSelected(Sender: TObject);
var
  vMenuItem: TMenuItem;
  vSelectedIndex: Integer;
begin
  vMenuItem := TMenuItem(Sender);

  vSelectedIndex := vMenuItem.Index;

  Assert(vSelectedIndex >= 0, 'Выбран неизвестный пункт меню');
  if Assigned(FButtonView) then
  begin
    TEntity(FButtonView.DomainObject)._SetFieldValue(FSession.NullHolder, 'SelectedIndex', vSelectedIndex);
    FUIBuilder.LastArea := Self;
    ExecuteUIAction(FButtonView);
  end;
end;

procedure TFMXEntityFieldEditor.ClosePopup;
begin
  if PopupShowing and (not Focused) and (FSelectPopup.InvokedBy = FControl) then
    FSelectPopup.Close;
end;

procedure TFMXEntityFieldEditor.FillEditor;
var
  vEntity: TEntity;
  vDefinition: TDefinition;
begin
  inherited;

  vEntity := TEntity(FView.FieldEntity);
  if TObjectFieldDef(FView.Definition).ContentDefinitionName = '-' then
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
      Text := GetTranslation(vDefinition, tpEmptyValue)
    else
      Text := vEntity['Name'];
    Hint := Text;
  end;

  UpdateVisibility;

  FbtnAdd.Visible := (FView.State = vsFullAccess) and not Assigned(vEntity) and (not vDefinition.HasFlag(ccSystem));
  FbtnAdd.Enabled := FbtnAdd.Visible;
  // todo: create option
end;

procedure TFMXEntityFieldEditor.FocusedChanged(const AFocused: Boolean);
begin
  inherited;
  ClosePopup;
end;

procedure TFMXEntityFieldEditor.OnAddClick(Sender: TObject);
begin
  SetFocused(True);
  if Assigned(FButtonView) then
  begin
    TEntity(FButtonView.DomainObject)._SetFieldValue(FSession.NullHolder, 'SelectedIndex', 0);
    FUIBuilder.LastArea := Self;
    ExecuteUIAction(FButtonView);
    //if vSaved then
    //  PostMessage(TWinControl(FControl).Handle, WM_NEXTDLGCTL, 0, 0)
  end;
end;

procedure TFMXEntityFieldEditor.OnComboMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  OnSelectClick(Sender);
end;

procedure TFMXEntityFieldEditor.OnEntitySelected(const ANewEntity: TEntity);
begin
  if PopupShowing then
    SetFieldEntity(ANewEntity);

  ClosePopup;
  //PostMessage(TControl(TControl(FControl).Parent).Handle, WM_NEXTDLGCTL, 0, 0);
end;

procedure TFMXEntityFieldEditor.DoOnClose(Sender: TObject);
begin
  if FSelectButton.CanFocus then
    FSelectButton.SetFocus;
end;

procedure TFMXEntityFieldEditor.OnSelectClick(Sender: TObject);
begin
  if PopupShowing and (FSelectPopup.InvokedBy = FControl) then
    FSelectPopup.Close
  else
  begin
    SetFocused(True);
    ShowPopup('');
  end;
end;

procedure TFMXEntityFieldEditor.OnWinControlKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = VKSPACE then
  begin
    if PopupShowing then
   //   ClosePopup // иначе при поиске нельзя использовать пробел
    else
      ShowPopup('');
  end
  else if Key = VKESCAPE then
  begin
    if PopupShowing then
      ClosePopup
  end
  else if Key = VKF2 then
  begin
//    if FbtnAdd.Enabled then
//      DoAdd(0);
  end
  else if Key = VKF3 then
  begin
//    if FbtnEdit.Enabled then
//      DoEdit;
  end
  else if Key = VKDOWN then
    ShowPopup('')
  else if IsAlpha(Char(Key)) then
    ShowPopup(KeyChar)
  else if Key = VKRETURN then

  else if Key in [VKPRIOR, VKNEXT] then
    ClosePopup;
end;

function TFMXEntityFieldEditor.PopupShowing: Boolean;
begin
  Result := Assigned(FSelectPopup) and FSelectPopup.Form.Visible;
end;

procedure TFMXEntityFieldEditor.SetFocused(const Value: Boolean);
begin
  inherited;
  if Value and FSelectButton.CanFocus then
    FSelectButton.SetFocus;
end;

procedure TFMXEntityFieldEditor.ShowPopup(const AFilter: string);
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
      FSelectPopup := TSelectEntityPopup.Create(TInteractor(FView.Interactor));
      FSelectPopup.OnClose := DoOnClose;
      FSelectPopup.OnEntitySelected := OnEntitySelected;
    end;

    FSelectPopup.Init(FEntities, True, vEntity, AFilter);
    FSelectPopup.ShowFor(TControl(FControl), '');
  finally
    TPresenter(vInteractor.Presenter).SetCursor(vPrevCursor);
  end;
end;

procedure TFMXEntityFieldEditor.UpdateVisibility;
begin
  if (FView.State < vsSelectOnly) then
  begin
    FSelectButton.Visible := False;

    FTextEdit.Visible := True;
    FTextEdit.Text := FSelectButton.Items[FSelectButton.ItemIndex];
//    FTextEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else begin
    FTextEdit.Visible := False;

    FSelectButton.OnClick := OnSelectClick;
    FSelectButton.Visible := True;
  end;
end;

{ TListEntityFieldEditor }

{procedure TListEntityFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEntities);
end;

procedure TListEntityFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
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

  FControl := FGrid;
end;

procedure TListEntityFieldEditor.DoOnFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode,
  AFocusedNode: TcxTreeListNode);
begin
  if Assigned(FGrid.FocusedNode) then
    SetFieldEntity(TEntity(FGrid.FocusedNode.Data));
end;

procedure TListEntityFieldEditor.FillEditor;
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

procedure TListEntityFieldEditor.OnDblClick(Sender: TObject);
begin

end;

procedure TListEntityFieldEditor.Reload;
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

{ TFMXLinkedEntityFieldEditor }

{procedure TFMXLinkedEntityFieldEditor.DoBeforeFreeControl;
begin
  inherited DoBeforeFreeControl;
  FreeAndNil(FLabel);
end;

procedure TFMXLinkedEntityFieldEditor.OnLabelClick(Sender: TObject);
var
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(FView.Interactor);
  vInteractor.ViewEntity(FView);
end;

procedure TFMXLinkedEntityFieldEditor.UpdateVisibility;
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

{ TFMXEntitySelector }

{procedure TFMXEntitySelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TFMXEntitySelector.DoBeforeFreeControl;
begin
  FreeAndNil(FEntities);
end;

procedure TFMXEntitySelector.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
var
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(FView.Interactor);
  FEntities := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  FControl := TcxComboBox.Create(nil);
  TcxComboBox(FControl).Properties.DropDownListStyle := lsFixedList;
  TcxComboBox(FControl).Properties.OnInitPopup := CBOnInitPopup;

  if Assigned(ALayout) then
    FFlat := (ALayout.ExtractInteger('BevelOuter') = Ord(bvNone)) and (ALayout.ExtractInteger('BevelInner') = Ord(bvNone))
      and (ALayout.ExtractInteger('BevelKind') = Ord(TBevelKind.bkNone));
end;

procedure TFMXEntitySelector.DoOnChange;
var
  vEntity: TEntity;
begin
  vEntity := TEntity(TcxComboBox(FControl).ItemObject);
  SetFieldEntity(vEntity);
end;

procedure TFMXEntitySelector.FillEditor;
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
      vEdit.TabStop := True;
    end;
  end
  else begin
    vEdit.EditValue := '';
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end;
end;

procedure TFMXEntitySelector.FillList;
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

function TFMXEntitySelector.GetDisabledBorderStyle: TcxEditBorderStyle;
begin
  if StrToBoolDef(TDomain(FView.Domain).UserSettings.GetValue('Core', 'ShowBordersForDisabled'), True) and not FFlat then
    Result := ebsUltraFlat
  else
    Result := ebsNone;
end;

procedure TFMXEntitySelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TcxComboBox(FControl).Properties) then
    TcxComboBox(FControl).Properties.OnChange := AHandler;
end;

{ TEntityFieldListEditor }

{procedure TEntityFieldListEditor.CleanChildViews;
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

function TEntityFieldListEditor.CreateCategoryRow(const ARootEntity: TEntity; const AFieldDef: TFieldDef): TcxCategoryRow;
begin
  Result := TcxCategoryRow(FEditor.Add(TcxCategoryRow));
  Result.Properties.Caption := GetFieldTranslation(AFieldDef);
  Result.Properties.Hint := GetFieldTranslation(AFieldDef, tpHint);
  Result.Tag := Integer(TRowData.Create(ARootEntity, AFieldDef));
end;

function TEntityFieldListEditor.CreateEditRow(const ARootEntity: TEntity; const AFieldDef: TFieldDef;
  const AViewPath: string; const AOverriddenCaption: string = ''): TcxEditorRow;
var
  vFieldView: TView;
  vStyleName: string;
begin
  Result := TcxEditorRow(FEditor.Add(TcxEditorRow));
  vStyleName := GetUrlCommand(AFieldDef.StyleName);
  if AOverriddenCaption = '' then
    Result.Properties.Caption := GetFieldTranslation(AFieldDef)
  else
    Result.Properties.Caption := AOverriddenCaption;

  Result.Tag := Integer(TRowData.Create(ARootEntity, AFieldDef));

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

procedure TEntityFieldListEditor.CreateRows(const ARootEntity: TEntity; const ARootRow: TcxCustomRow;
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
      CreateEditRow(ARootEntity, vFieldDef, AViewPath, GetFieldTranslation(vFieldDef) + ' ' + IfThen(ARootEntityIndex > -1, IntToStr(ARootEntityIndex + 1))).Parent := ARootRow;
    end;
  end;
end;

procedure TEntityFieldListEditor.DoBeforeFreeControl;
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

procedure TEntityFieldListEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FEditor := TcxVerticalGrid.Create(nil);
  FEditor.OptionsView.ShowEditButtons := ecsbFocused;
  FEditor.OptionsView.PaintStyle := psDelphi;
  FEditor.BorderStyle := cxcbsNone;

  FControl := FEditor;

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

procedure TEntityFieldListEditor.DoDisableContent;
begin
  CleanChildViews;
end;

procedure TEntityFieldListEditor.FillEditor;
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
        {fkEnum:
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
        {else
          vRow.Properties.Value := vLevelEntity.FieldByName(vFieldDef.Name).Value;
      end;
    end;
  finally
    FEditor.EndUpdate;
  end;
end;

procedure TEntityFieldListEditor.FreeEditors;
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

procedure TEntityFieldListEditor.RefillArea(const AKind: Word);
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

    SwitchChangeHandlers(OnChange);
  except
    TInteractor(FView.Interactor).ShowMessage('Error in FillEditorFromModel, Field: ' + FFieldDef.Name);
  end;
end;

procedure TEntityFieldListEditor.OnFieldChange(Sender: TObject);
var
  vEditor: TcxCustomEdit;
  vRow: TcxCustomRow;
  vFieldDef: TFieldDef;
  vValue: Variant;
//  vInteractor: TInteractor;
  vEntity, vLevelEntity: TEntity;
  vIndex, vId: Integer;
begin
  vEditor := TcxCustomEdit(Sender);
//  vInteractor := TInteractor(FView.Interactor);
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
                vLevelEntity._SetFieldValue(TChangeHolder(Holder), vFieldDef.Name, vId);
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
                vLevelEntity._SetFieldEntity(TChangeHolder(Holder), vFieldDef.Name, vEntity);
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
        vLevelEntity._SetFieldValue(TChangeHolder(Holder), vFieldDef.Name, vValue);
      end);
  end;
end;

{ TRowData }

constructor TRowData.Create(const AEntity: TEntity; const AFieldDef: TFieldDef);
begin
  Entity := AEntity;
  FieldDef := AFieldDef;
end;

{ TRadioEntitySelector }

{procedure TRadioEntitySelector.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEntities);
end;

procedure TRadioEntitySelector.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TcxRadioGroup.Create(nil);
  TcxRadioGroup(FControl).Transparent := True;
  TcxRadioGroup(FControl).Style.BorderStyle := ebsNone;
  TcxRadioGroup(FControl).Name := 'radio';
  TcxRadioGroup(FControl).Caption := '';
  FNeedCreateCaption := False;

  FEntities := TEntityList.Create(TInteractor(FView.Interactor).Domain, TInteractor(FView.Interactor).Session);
end;

procedure TRadioEntitySelector.DoOnChange;
var
  vIndex: Integer;
begin
//  if FFieldDef.HasFlag(cRequired) then
//    vIndex := TcxRadioGroup(FControl).ItemIndex + 1
//  else
  vIndex := TcxRadioGroup(FControl).ItemIndex;
  SetFieldEntity(FEntities[vIndex]);
end;

procedure TRadioEntitySelector.FillEditor;
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
      vRadioEdit.TabStop := True;
  end;
end;

procedure TRadioEntitySelector.FillList;
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
      if Assigned(vEnt) then
      begin
        vRadioItem := vRadioItems.Add;
        vRadioItem.Caption := vEnt.DisplayName;
        vRadioItem.Tag := Integer(vEnt);
        if vEnt = TEntity(Integer(FView.FieldValue)) then
          TcxRadioGroup(FControl).ItemIndex := TcxRadioGroup(FControl).Properties.Items.Count - 1;
      end;
      //todo: обработать <не задано> если нужно
    end;
  finally
    vRadioItems.EndUpdate;
  end;
end;

procedure TRadioEntitySelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TcxRadioGroup(FControl).Properties.OnChange := AHandler;
end;     }

initialization

TPresenter.RegisterUIClass('Windows.DevExpress', uiEntityEdit, '', TFMXEntityFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEntityEdit, 'simple', TFMXEntityFieldEditor);
{TPresenter.RegisterUIClass('Windows.DevExpress', uiEntityEdit, 'list', TListEntityFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEntityEdit, 'link', TFMXLinkedEntityFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEntityEdit, 'select', TFMXEntitySelector);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEntityEdit, 'fieldlist', TEntityFieldListEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEntityEdit, 'radio', TRadioEntitySelector); }

end.
