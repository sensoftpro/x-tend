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
  FMX.StdCtrls, FMX.Edit, FMX.ComboEdit, FMX.ListBox,
  uUIBuilder, uView, uEntity, uEntityList, uDefinition, fmxArea, uLayout;

type
  TFMXEntityFieldEditor = class(TFMXControl)
  private
    FBasePanel: TPanel;
    FSelectButton: TComboEdit;
    FTextEdit: TLabel;
    FbtnAdd: TButton;
    FEntities: TEntityList;
    procedure FillList;
    procedure CBOnChange(Sender: TObject);
  protected
    procedure UpdateVisibility;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoDeinit; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFMXEntitySelector = class(TFMXControl)
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

  TFMXEntityFieldListEditor = class(TFMXControl)
  private
    FListBox: TListBox;
    FDomainObject: TEntity;
    procedure CreateRows(AEntity: TEntity; ACaptionPrefix: string);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TFMXRadioEntitySelector = class(TFMXControl)
  private
    FEntities: TEntityList;
    FFontSize: Single;
    procedure OnChange(Sender: TObject);
    procedure FillList;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

implementation

uses
  FMX.Graphics, SysUtils, FMX.Controls, FMX.Types,
  uInteractor, uDomain, uChangeManager, uObjectField, uConsts, uSession,  Variants,
  uPresenter, StrUtils, uUtils, uEnumeration;

function IsAlpha(c: Char): Boolean;
begin
  Result := ((c >= 'а') and (c <= 'я')) or ((c >= 'А') and (c <= 'Я')) or
    ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z'));
end;

{ TFMXEntityFieldEditor }

procedure TFMXEntityFieldEditor.DoBeforeFreeControl;
begin
  inherited;

  FreeAndNil(FEntities);
  FreeAndNil(FTextEdit);
  FreeAndNil(FSelectButton);
end;

function TFMXEntityFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vButtonLayout: TLayout;
  vAddArea: TUIArea;
begin
  FEntities := nil;

  FBasePanel := TPanel.Create(nil);
  FBasePanel.StyleLookup := 'pushpanel';

  FSelectButton := TComboEdit.Create(nil);
  with FSelectButton do
  begin
    Parent := FBasePanel;
    Align := TAlignLayout.Client;
    Width := FBasePanel.Width;
    OnChange := CBOnChange;
  end;

  FSelectButton.Enabled := True;

  FTextEdit := TLabel.Create(nil);
  FTextEdit.Parent := FBasePanel;
  FTextEdit.Align := TAlignLayout.Client;
  FTextEdit.Visible := False;
  FTextEdit.Text := '';
  FTextEdit.TabStop := False;

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

  FbtnAdd := TButton(GetRealControl(vAddArea));
  FbtnAdd.Parent := FBasePanel;
end;

procedure TFMXEntityFieldEditor.DoDeinit;
begin
  inherited;
  FSelectButton.Text := '';
  FSelectButton.Enabled := False;

  FbtnAdd.Visible := True; // todo: create option
end;

procedure TFMXEntityFieldEditor.FillEditor;
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

procedure TFMXEntityFieldEditor.CBOnChange(Sender: TObject);
var
  vIndex: Integer;
begin
  vIndex := FSelectButton.ItemIndex;

   if vIndex < 0 then Exit;

  SetFieldEntity(TEntity(FSelectButton.Items.Objects[vIndex]));
end;

procedure TFMXEntityFieldEditor.FillList;
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

procedure TFMXEntityFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  if Assigned(AHandler) then
    FSelectButton.OnChange := CBOnChange
  else
    FSelectButton.OnChange := nil;
end;

procedure TFMXEntityFieldEditor.UpdateVisibility;
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

{ TFMXEntitySelector }

procedure TFMXEntitySelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TFMXEntitySelector.DoBeforeFreeControl;
begin
  FreeAndNil(FEntities);
end;

function TFMXEntitySelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(FView.Interactor);
  FEntities := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  Result := TComboBox.Create(nil);
  TComboBox(Result).OnPopup := CBOnInitPopup;

  FFlat := (ALayout.BevelOuter = lbkNone) and (ALayout.BevelInner = lbkNone);
end;

procedure TFMXEntitySelector.DoOnChange;
var
  vEntity: TEntity;
begin
  if TComboBox(FControl).ItemIndex = -1 then
    Exit;

  vEntity := TEntity(TComboBox(FControl).Items.Objects[TComboBox(FControl).ItemIndex]);
  SetFieldEntity(vEntity);
end;

procedure TFMXEntitySelector.FillEditor;
var
  vEdit: TComboBox;
  vEntity: TEntity;
begin
  FillList;

  vEdit := TComboBox(FControl);
  vEntity := FView.FieldEntity;
  if Assigned(vEntity) then
  begin
    vEdit.Enabled := True;
    vEdit.ItemIndex := vEdit.Items.IndexOfObject(vEntity);
  end
  else
    vEdit.Enabled := False;
end;

procedure TFMXEntitySelector.FillList;
var
  vField: TEntityField;
  vEntity: TEntity;
begin
  TComboBox(FControl).Items.BeginUpdate;
  try
    TComboBox(FControl).Items.Clear;
    vField := FView.ExtractEntityField;
    vField.GetEntitiesForSelect(TInteractor(FView.Interactor).Session, FEntities);
    for vEntity in FEntities do
      TComboBox(FControl).Items.AddObject(SafeDisplayName(vEntity), vEntity);
  finally
    TComboBox(FControl).Items.EndUpdate;
  end;
end;

procedure TFMXEntitySelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TComboBox(FControl)) then
    TComboBox(FControl).OnChange := AHandler;
end;

{ TFMXEntityFieldListEditor }

procedure TFMXEntityFieldListEditor.CreateRows(AEntity: TEntity; ACaptionPrefix: string);
var
  vFieldDef: TFieldDef;
  vListField: TListField;
  vData: string;
  i: Integer;
begin
  for vFieldDef in AEntity.Definition.Fields do
  begin
    if vFieldDef.Kind = fkList then
    begin
      vListField := TListField(AEntity.FieldByName(vFieldDef.Name));
      if vListField.Count > 0 then
      begin
        for i := 0 to vListField.Count - 1 do
        begin
          if FListBox.Items.IndexOf(FOwner.GetFieldTranslation(vListField.FieldDef)) < 0 then
          begin
            FListBox.Items.Add(ACaptionPrefix + FOwner.GetFieldTranslation(vListField.FieldDef));
            FListBox.Items.Add('');
          end;
          CreateRows(vListField[i],ACaptionPrefix + '    ');
        end;
      end;
    end
    else
    begin
      vData := AEntity.FieldByName(vFieldDef.Name).Value;
      if vData = '' then
        Continue;
      if vFieldDef.HasFlag(cHideInEdit) or (vFieldDef.UIState < vsDisabled) then
        Continue;
      FListBox.Items.Add(ACaptionPrefix + FOwner.GetFieldTranslation(vFieldDef));
      FListBox.Items.Add(vData);
    end;
  end;
end;

function TFMXEntityFieldListEditor.DoCreateControl(const AParent: TUIArea;
  const ALayout: TLayout): TObject;
begin
  FListBox := TListBox.Create(nil);
  FListBox.Columns := 2;

  Result := FListBox;
  FListBox.AlternatingRowBackground := True;
//  if Assigned(FCreateParams) then
//    FListBox.ItemWidth := StrToIntDef(FCreateParams.Values['headerWidth'], 150)
//  else
//    FListBox.ItemWidth := 150;

  FDomainObject := TEntity(FView.DomainObject);
end;

procedure TFMXEntityFieldListEditor.FillEditor;
begin
  FListBox.BeginUpdate;
  FListBox.Items.Clear;
  CreateRows(FDomainObject, '');
  FListBox.EndUpdate;
end;

{ TFMXRadioEntitySelector }

procedure TFMXRadioEntitySelector.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEntities);
end;

function TFMXRadioEntitySelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TPanel.Create(nil);
  TPanel(Result).Name := 'radio';
  TPanel(Result).StyleLookup := 'pushpanel';
  FNeedCreateCaption := False;

  FFontSize := ALayout.Font.Size * 96 / 72;

  FEntities := TEntityList.Create(TInteractor(FView.Interactor).Domain, TInteractor(FView.Interactor).Session);
end;

procedure TFMXRadioEntitySelector.DoOnChange;
begin
  inherited;
  if FFieldDef.HasFlag(cRequired) then
    SetFieldValue(TComboBox(FControl).ItemIndex + 1)
  else
    SetFieldValue(TComboBox(FControl).ItemIndex);
end;

procedure TFMXRadioEntitySelector.FillEditor;
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

procedure TFMXRadioEntitySelector.FillList;
var
  vRadioEdit: TPanel;
  vRadioItem: TRadioButton;
  vLastPos: Single;
  vField: TEntityField;
  i {, c}: Integer;
  vEnt: TEntity;
  //vStepSize: Integer;
begin
  vField := FView.ExtractEntityField;
  vField.GetEntitiesForSelect(TInteractor(FView.Interactor).Session, FEntities);

  vRadioEdit := TPanel(FControl);

  while vRadioEdit.ControlsCount > 0 do
    vRadioEdit.Controls[0].Free;

  vRadioEdit.Controls.Clear;
  vLastPos := 0;
  for i := 0 to FEntities.Count - 1 do
  begin
    vEnt := FEntities[i];
    if not Assigned(vEnt) then Continue;

    vRadioItem := TRadioButton.Create(nil);
    vRadioItem.Align := TAlignLayout.Top;
    vRadioItem.Position.Y := vLastPos + 1;
    vLastPos := vRadioItem.Position.Y + vRadioItem.Height;
    vRadioItem.Text := SafeDisplayName(vEnt);
    vRadioItem.TextSettings.Font.Size := FFontSize;
    vRadioItem.StyledSettings := vRadioItem.StyledSettings - [TStyledSetting.Size];
    vRadioItem.TagObject := vEnt;
    vRadioItem.GroupName := FView.Name;
    if vEnt = TEntity(NativeInt(FView.FieldValue)) then
      vRadioItem.IsChecked := True;
    vRadioItem.OnChange := OnChange;
    TPanel(FControl).AddObject(vRadioItem);
  end;

//  c := 0;
//  for i := 0 to FEntities.Count - 1 do
//  begin
//    vEnt := FEntities[i];
//    if not Assigned(vEnt) then Continue; // todo: надо обработать cRequired
//
//    vRadioItem := TRadioButton.Create(nil);
//    vRadioItem.Width := vRadioEdit.Width;
//    vRadioItem.Parent := vRadioEdit;
//    vRadioItem.Text := SafeDisplayName(vEnt);
//    vRadioItem.TagObject := vEnt;
//    vRadioItem.Position.Y := vStepSize * c;
//    if vEnt = TEntity(NativeInt(FView.FieldValue)) then
//      vRadioItem.IsChecked := True;
//    vRadioItem.OnClick := OnChange;
//    Inc(c);
//  end;
end;

procedure TFMXRadioEntitySelector.OnChange(Sender: TObject);
begin
  SetFieldEntity(TEntity(TRadioButton(Sender).TagObject));
end;

procedure TFMXRadioEntitySelector.SwitchChangeHandlers(
  const AHandler: TNotifyEvent);
var
  i: Integer;
  vRadioEdit: TPanel;
begin
  vRadioEdit := TPanel(FControl);
  for i := 0 to vRadioEdit.ControlsCount - 1 do
    if Assigned(AHandler) then
      TRadioButton(vRadioEdit.Controls[i]).OnClick := OnChange
    else
      TRadioButton(vRadioEdit.Controls[i]).OnClick := nil;
end;

initialization

TPresenter.RegisterControlClass('FMX', uiEntityEdit, '', TFMXEntityFieldEditor);
TPresenter.RegisterControlClass('FMX', uiEntityEdit, 'simple', TFMXEntityFieldEditor);
TPresenter.RegisterControlClass('FMX', uiEntityEdit, 'select', TFMXEntitySelector);
TPresenter.RegisterControlClass('FMX', uiEntityEdit, 'radio', TFMXRadioEntitySelector);
TPresenter.RegisterControlClass('FMX', uiEntityEdit, 'fieldlist', TFMXEntityFieldListEditor)

end.
