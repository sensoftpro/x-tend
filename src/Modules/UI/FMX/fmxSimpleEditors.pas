unit fmxSimpleEditors;

interface

uses
  Classes, Variants, SysUtils, UITypes, Math,
  uDefinition, uEnumeration, uUIBuilder, uView, fmxArea, uLayout, uConsts, uEntity, uDomain;

type
  TFMXTextInfo = class(TFMXControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TFMXTextEdit = class(TFMXControl)
  private
    procedure OnPhoneKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFMXMemoFieldEditor = class (TFMXControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFMXIntegerFieldEditor = class(TFMXControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFMXSpinner = class(TFMXControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TFMXProgress = class(TFMXControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TFMXIntegerFlagsEditor = class(TFMXControl)
  private
    FDisplayFlagCount: Integer;
    FCaptions: TStrings;
    procedure FillList;
    procedure DoOnClick(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFMXEnumEditor = class(TFMXControl)
  private
    FEnum: TEnumeration;
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
    procedure DoOnClick(Sender: TObject);
    procedure DoChange(Sender: TObject);
  protected
    procedure DoBeforeFreeControl; override;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFMXFloatFieldEditor = class(TFMXControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFMXDateFieldEditor = class(TFMXControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    function GetNewValue: Variant; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFMXTimeFieldEditor = class(TFMXControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFMXBoolFieldEditor = class(TFMXControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFMXPagesFieldEditor = class(TFMXControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

implementation

uses
  FMX.ListBox, FMX.Types, FMX.Graphics, FMX.ExtCtrls, FMX.StdCtrls, FMX.Dialogs, FMX.Controls, FMX.ActnList, FMX.StdActns,
  FMX.Edit, FMX.Memo, FMX.SpinBox, FMX.TabControl, FMX.Text, FMX.DateTimeCtrls, FMX.EditBox, FMX.NumberBox,
  uPresenter, uFMXPresenter, uInteractor, uUtils;

{ TFMXPagesFieldEditor }

function TFMXPagesFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vPC: TTabControl;
  vTabLayout: TLayout;
  vChildArea: TUIArea;
  i: Integer;
begin
  FNeedCreateCaption := False;

  inherited;

  vPC := TTabControl.Create(nil);
  Result := vPC;
  vPC.Width := ALayout.Width;
  vPC.Height := ALayout.Height;

  vPC.Align := AlignToAlignLayout(ALayout.Align);
  vPC.Anchors := ALayout.Anchors;
  vPC.Position.X := ALayout.Left;
  vPC.Position.Y := ALayout.Top;

  if not ALayout.ShowCaption or ((ALayout.Items.Count > 0) and not ALayout.Items[0].ShowCaption) then
    vPC.TabPosition := TTabPosition.None
  else
    vPC.TabHeight := ALayout.Page_Height;

  vPC.Parent := TControl(GetRealControl(AParent));
  for i := 0 to ALayout.Items.Count - 1 do
  begin
    vTabLayout := ALayout.Items[i];

    vChildArea := TPresenter(FPresenter).CreateArea(FOwner, FView.Parent, vTabLayout);
    TControl(GetRealControl(vChildArea)).Parent := vPC;
    FOwner.AddArea(vChildArea);

    TInteractor(FView.Interactor).UIBuilder.CreateChildAreas(vChildArea, vChildArea.View, vTabLayout, '');
  end;
end;

procedure TFMXPagesFieldEditor.DoOnChange;
begin
end;

procedure TFMXPagesFieldEditor.FillEditor;
var
  vTag: Integer;
  vValue: Variant;
begin
  vValue := FView.FieldValue;
  if VarIsNull(vValue) then
    vTag := 0
  else begin
    if TFieldDef(FView.Definition).Kind = fkBoolean then
      vTag := IfThen(vValue, 1, 0)
    else if TFieldDef(FView.Definition).Kind = fkEnum then
    begin
      if TFieldDef(FView.Definition).Flags and cRequired = 1 then
        vValue := vValue - 1;
      vTag := vValue
    end
    else if TFieldDef(FView.Definition).Kind = fkInteger then
      vTag := vValue
    else if TFieldDef(FView.Definition).Kind = fkObject then
      vTag := TEntity(NativeInt(vValue)).ID
    else
      vTag := -1;
  end;

  if (vTag < TTabControl(FControl).TabCount) and (TTabControl(FControl).TabIndex <> vTag) then
    TTabControl(FControl).TabIndex := vTag;
end;

procedure TFMXPagesFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TTabControl(FControl).OnChange := AHandler;
end;

{ TTextInfo }

function TFMXTextInfo.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TLabel.Create(nil);
  TLabel(Result).Visible := True;
  TLabel(Result).WordWrap := True;
  if Assigned(FCreateParams) and (FCreateParams.Values['WordWrap'] = 'False') then
    TLabel(Result).WordWrap := False;
  TLabel(Result).AutoSize := False;
end;

procedure TFMXTextInfo.FillEditor;
var
  vEnum: TEnumeration;
  vEntity: TEntity;
  vColorField: TBaseField;
  vValue: Variant;
begin
  inherited;
  if VarIsNull(FView.FieldValue) then
    TLabel(FControl).Text := ''
  else if FFieldDef.Kind = fkCurrency then
  begin
    if FFieldDef.Format <> '' then
      TLabel(FControl).Text := FormatFloat(GetFormat, FView.FieldValue)
    else
      TLabel(FControl).Text := FormatFloat('#,##0.00;;0', FView.FieldValue);
  end
  else if FFieldDef.Kind = fkFloat then
  begin
    if FFieldDef.Format <> '' then
      TLabel(FControl).Text := FormatFloat(GetFormat, FView.FieldValue)
    else
      TLabel(FControl).Text := FView.FieldValue;
  end
  else if FFieldDef.Kind = fkDateTime then
  begin
    vValue := FView.FieldValue;
    if IsZero(vValue, 1e-6) then
      TLabel(FControl).Text := ''
    else if FFieldDef.Format <> '' then
      TLabel(FControl).Text := FormatDateTime(GetFormat, vValue)
    else
      TLabel(FControl).Text := FormatDateTime('dd.mm.yyyy hh:nn:ss', vValue);
  end
  else if FFieldDef.Kind = fkEnum then
  begin
    vEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
    if not Assigned(vEnum) then
    begin
      vEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
      TLabel(FControl).FontColor := TAlphaColor(TState(vEnum.Items[Integer(FView.FieldValue)]).Color);
        TLabel(FControl).StyledSettings := TLabel(FControl).StyledSettings - [TStyledSetting.FontColor];
    end;
    TLabel(FControl).Text := vEnum.Items[Integer(FView.FieldValue)].DisplayText;
  end
  else if FFieldDef.Kind = fkObject then
  begin
    vEntity := TEntity(FView.FieldEntity);
    if not Assigned(vEntity) then
      TLabel(FControl).Text := TObjectFieldDef(FView.Definition)._ContentDefinition._EmptyValue
    else begin
      TLabel(FControl).Text := vEntity['Name'];
      if vEntity.Definition.ColorFieldName <> '' then
      begin
        vColorField := vEntity.FieldByName(vEntity.Definition.ColorFieldName);
        TLabel(FControl).FontColor := TAlphaColor(vColorField.Value);
        TLabel(FControl).StyledSettings := TLabel(FControl).StyledSettings - [TStyledSetting.FontColor];
      end;
    end;
  end
  else
    TLabel(FControl).Text := FView.FieldValue;
end;

{ TFMXTextEdit }

function TFMXTextEdit.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TEdit.Create(nil);
  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TEdit(Result).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;

  if SameText('phone', FFieldDef.StyleName) then
    TEdit(Result).OnKeyDown := OnPhoneKeyDown
  else begin
    if SameText('mask', FFieldDef.StyleName) then
      TEdit(Result).Password := True;
    TEdit(Result).OnKeyDown := nil;
    TEdit(Result).OnKeyUp := nil;
  end;
end;

procedure TFMXTextEdit.OnPhoneKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if CharInSet(KeyChar, ['0'..'9', #8, '-', '+', '(', ')', ' ']) then Exit;

  Key := 0;
  keyChar := #0;
end;

procedure TFMXTextEdit.DoOnChange;
begin
  SetFieldValue(TEdit(FControl).Text)
end;

procedure TFMXTextEdit.FillEditor;
var
  vEdit: TEdit;
begin
  vEdit := TEdit(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Text := '';
    vEdit.Enabled := False;
  end
  else
  begin
    vEdit.Text := FView.FieldValue;
    vEdit.Enabled := True;
    vEdit.ReadOnly := FView.State < vsFullAccess;
  end;
end;

procedure TFMXTextEdit.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TEdit(FControl).OnChange := AHandler;
end;

{ TFMXMemoFieldEditor }

function TFMXMemoFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TMemo.Create(nil);

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TMemo(Result).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TFMXMemoFieldEditor.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TMemo(FControl).Text);
end;

procedure TFMXMemoFieldEditor.FillEditor;
var
  vEdit: TMemo;
begin
  vEdit := TMemo(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Text := '';
    vEdit.Enabled := False;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.Text := FView.FieldValue;
    vEdit.SelStart := 1000;
    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
      vEdit.TabStop := False
    else
      vEdit.TabStop := True;
  end;
end;

procedure TFMXMemoFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TMemo(FControl)) then
    TMemo(FControl).OnChange := AHandler;
end;

{ TFMXIntegerFieldEditor }

function TFMXIntegerFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  if FFieldDef.StyleName = 'simple' then
    Result := TSpinBox.Create(nil)
  else
    Result := TNumberBox.Create(nil);

  with TCustomEditBox(Result) do
  begin
    if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
      Max := TSimpleFieldDef(FFieldDef).MaxValue
    else
      Max := MaxDouble;

    if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
      Min := TSimpleFieldDef(FFieldDef).MinValue
    else
      Min := MinDouble;
  end;
end;

procedure TFMXIntegerFieldEditor.DoOnChange;
begin
  if TCustomEditBox(FControl).Value = 0 then
    SetFieldValue(Null)
  else
    SetFieldValue(Round(TCustomEditBox(FControl).Value));
end;

procedure TFMXIntegerFieldEditor.FillEditor;
var
  vEdit: TCustomEditBox;
begin
  vEdit := TCustomEditBox(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Value := 0;
    vEdit.Enabled := False;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.Value := FView.FieldValue;
    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
      vEdit.TabStop := False
    else
      vEdit.TabStop := FOwner.TabStop;
  end;
end;

procedure TFMXIntegerFieldEditor.SwitchChangeHandlers(
  const AHandler: TNotifyEvent);
begin
  inherited;
  TCustomEditBox(FControl).OnChange := AHandler
end;

{ TDESpinner }

function TFMXSpinner.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TAniIndicator.Create(nil);
  TAniIndicator(Result).Visible := False;
end;

procedure TFMXSpinner.FillEditor;
begin
  inherited;
  TControl(FControl).Visible := FView.FieldValue > 0;
  TAniIndicator(FControl).Enabled := TControl(FControl).Visible;
end;

{ TDEProgress }

function TFMXProgress.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TProgressBar.Create(nil);
  FNeedCreateCaption := False;
  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TProgressBar(Result).Max := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TFMXProgress.FillEditor;
begin
  inherited;
  TProgressBar(FControl).Value := FView.FieldValue;
end;

{ TFMXIntegerFlagsEditor }

procedure TFMXIntegerFlagsEditor.DoOnClick(Sender: TObject);
var
  i: Integer;
  vFlagsValue: Integer;
  vListItem: TCheckBox;
begin
  vFlagsValue := 0;

  if not Assigned(TGroupBox(FControl).Children) then exit;

  for i := 0 to TGroupBox(FControl).Children.Count - 1 do
  begin
    vListItem := TCheckBox(TGroupBox(FControl).Children[i]);
    if vListItem.IsChecked then
      vFlagsValue := vFlagsValue or Integer(vListItem.Tag);
  end;

  SetFieldValue(vFlagsValue);
end;

procedure TFMXIntegerFlagsEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FCaptions);
  TGroupBox(FControl).DeleteChildren;
end;

function TFMXIntegerFlagsEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TGroupBox.Create(nil);

  FDisplayFlagCount := 8;

  if Assigned(FCreateParams) then
  begin
    FDisplayFlagCount := Min(32, StrToIntDef(FCreateParams.Values['DisplayFlagCount'], 8));
    if FCreateParams.IndexOfName('ItemCaptions') > -1 then
      FCaptions := CreateDelimitedList(FCreateParams.Values['ItemCaptions'], ';');
  end;
end;

procedure TFMXIntegerFlagsEditor.FillEditor;
begin
  FillList;
  TGroupBox(FControl).Enabled := FView.State >= vsSelectOnly;
end;

procedure TFMXIntegerFlagsEditor.FillList;
var
  vListItem: TCheckBox;
  vBits, vBit: Integer;
  i: Integer;
begin
  vBits := FView.FieldValue;

  TGroupBox(FControl).DeleteChildren;
  for i := 0 to FDisplayFlagCount - 1 do
  begin
    vListItem := TCheckBox.Create(nil);
    vListItem.Align := TAlignLayout.Top;
    if Assigned(FCaptions) and (i < FCaptions.Count) then
      vListItem.Text := FCaptions[i]
    else
      vListItem.Text := IntToStr(i);
    vBit := 1 shl i;
    vListItem.Tag := vBit;
    vListItem.IsChecked := (vBits and vListItem.Tag) <> 0;
    vListItem.OnChange := DoOnClick;
    TGroupBox(FControl).AddObject(vListItem);
  end;
end;

procedure TFMXIntegerFlagsEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
var
  i: integer;
begin
  inherited;

  if not Assigned(TGroupBox(FControl).Children) then Exit;

  if Assigned(AHandler) then
    for i := 0 to TGroupBox(FControl).Children.Count - 1 do
      TCheckBox(TGroupBox(FControl).Children[i]).OnChange := DoOnClick
  else
    for i := 0 to TGroupBox(FControl).Children.Count - 1 do
      TCheckBox(TGroupBox(FControl).Children[i]).OnChange := nil;
end;

{ TFMXEnumEditor }

procedure TFMXEnumEditor.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TFMXEnumEditor.DoOnClick(Sender: TObject);
begin
  if FFieldDef.HasFlag(cRequired) then
    SetFieldValue((Sender as TRadioButton).Index + 1)
  else
    SetFieldValue((Sender as TRadioButton).Index);
end;

function TFMXEnumEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  if not Assigned(FEnum) then
    FEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);

  if FFieldDef.StyleName = 'radio' then
  begin
    Result := TGroupBox.Create(nil);
    TGroupBox(Result).Name := 'radio';
    TGroupBox(Result).Text := '';
    FNeedCreateCaption := False;
  end
  else
  begin
    Result := TComboBox.Create(nil);
    TComboBox(Result).OnPopup := CBOnInitPopup;
    TComboBox(Result).OnChange := DoChange;
  end;
end;

procedure TFMXEnumEditor.DoBeforeFreeControl;
begin
  inherited;
  if FFieldDef.StyleName = 'radio' then
    TGroupBox(FControl).DeleteChildren;
end;

procedure TFMXEnumEditor.DoChange;
begin
  if FFieldDef.StyleName <> 'radio' then
  begin
    if FFieldDef.HasFlag(cRequired) then
      SetFieldValue(TComboBox(FControl).ItemIndex + 1)
    else
      SetFieldValue(TComboBox(FControl).ItemIndex);
  end;
end;

procedure TFMXEnumEditor.FillEditor;
var
  vEdit: TComboBox;
  vRadioEdit: TGroupBox;
  vItemIndex: Integer;
  vRadioItem: TRadioButton;
begin
  FillList;

  if FFieldDef.StyleName = 'radio' then
  begin
    vRadioEdit := TGroupBox(FControl);
    if VarIsNull(FView.FieldValue) then
      vRadioEdit.Enabled := False
    else begin
      vItemIndex := FView.FieldValue;
      if FFieldDef.HasFlag(cRequired) then
        vItemIndex := vItemIndex - 1;
      vRadioItem := TRadioButton(vRadioEdit.Children[vItemIndex]);
      vRadioItem.OnChange := nil;
      vRadioItem.IsChecked := True;
      vRadioItem.OnChange := DoOnClick;
      vRadioEdit.Enabled := FView.State >= vsSelectOnly;
    end;
  end
  else
  begin
    vEdit := TComboBox(FControl);
    if VarIsNull(FView.FieldValue) then
      vEdit.Enabled := False
    else begin
      vItemIndex := FView.FieldValue;
      if FFieldDef.HasFlag(cRequired) then
        vItemIndex := vItemIndex - 1;
      vEdit.ItemIndex := vItemIndex;
      vEdit.Enabled := FView.State >= vsSelectOnly;
    end;
  end;
end;

procedure TFMXEnumEditor.FillList;
var
  vEnumItem: TEnumItem;
  vItems: TStrings;
  vRadioItem: TRadioButton;
begin
  if FFieldDef.StyleName = 'radio' then
  begin
    TGroupBox(FControl).DeleteChildren;
    for vEnumItem in FEnum do
      if not FFieldDef.HasFlag(cRequired) or (vEnumItem.ID > 0) then
      begin
        vRadioItem := TRadioButton.Create(nil);
        vRadioItem.Align := TAlignLayout.Top;
        vRadioItem.Text := vEnumItem.DisplayText;
        vRadioItem.OnChange := DoOnClick;
        TGroupBox(FControl).AddObject(vRadioItem);
      end;
  end
  else
  begin
    vItems := TComboBox(FControl).Items;

    vItems.BeginUpdate;
    try
      vItems.Clear;
      for vEnumItem in FEnum do
      begin
        if not FFieldDef.HasFlag(cRequired) or (vEnumItem.ID > 0) then
          vItems.Add(vEnumItem.DisplayText);
      end;
    finally
      vItems.EndUpdate;
    end;
  end;
end;

procedure TFMXEnumEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
var
  i: integer;
begin
  inherited;

  if not Assigned(TGroupBox(FControl).Children) then Exit;

  if Assigned(AHandler) then
    for i := 0 to TGroupBox(FControl).Children.Count - 1 do
      TCheckBox(TGroupBox(FControl).Children[i]).OnChange := DoOnClick
  else
    for i := 0 to TGroupBox(FControl).Children.Count - 1 do
      TCheckBox(TGroupBox(FControl).Children[i]).OnChange := nil;
end;

{ TFMXFloatFieldEditor }

function TFMXFloatFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vFormat: string;
  vPos: Integer;
begin
  if FFieldDef.StyleName = 'simple' then
    Result := TSpinBox.Create(nil)
  else
    Result := TNumberBox.Create(nil);

  TCustomEditBox(Result).ValueType := TNumValueType.Float;

  with TCustomEditBox(Result) do
  begin
    if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
      Max := TSimpleFieldDef(FFieldDef).MaxValue
    else
      Max := MaxDouble;

    if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
      Min := TSimpleFieldDef(FFieldDef).MinValue
    else
      Min := MinDouble;

    if (Length(FFieldDef.Format) > 0) or FFieldDef.Definition.FieldExists('Format') then
    begin
      vFormat := GetFormat;
      vPos := Pos('.', vFormat);
      if vPos > 0 then
        DecimalDigits := Length(vFormat) - vPos
      else
        DecimalDigits := 0;
    end;
  end;
end;

procedure TFMXFloatFieldEditor.DoOnChange;
begin
  if TCustomEditBox(FControl).Value = 0 then
    SetFieldValue(Null)
  else
    SetFieldValue(TCustomEditBox(FControl).Value);
end;

procedure TFMXFloatFieldEditor.FillEditor;
var
  vEdit: TCustomEditBox;
begin
  vEdit := TCustomEditBox(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Value := 0;
    vEdit.Enabled := False;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.Value := FView.FieldValue;
    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
      vEdit.TabStop := False
    else
      vEdit.TabStop := FOwner.TabStop;
  end;
end;

procedure TFMXFloatFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TCustomEditBox(FControl).OnChange := AHandler
end;

{ TFMXDateFieldEditor }

function TFMXDateFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TDateEdit.Create(nil);

  with TDateEdit(Result) do
  begin
    ShowClearButton := True;
    TodayDefault := True;
  end;
end;

function MyTryStrToDate(const AValue: string; var ADate: TDateTime): Boolean;
var
  vSL: TStrings;
  i: Integer;
  vYear: Word;
begin
  Result := True;
  vSL := CreateDelimitedList(AValue, '.');
  try
    if vSL.Count <> 3 then
      Exit;

    for i := 0 to vSL.Count - 1 do
      if StrToIntDef(vSL[i], -1) = -1 then
        Exit;

    vYear := StrToInt(vSL[2]);
    if vYear < 1900 then
      Exit;

    try
      ADate := EncodeDate(vYear, StrToInt(vSL[1]), StrToInt(vSL[0]));
    except
      Result := False;
    end;
//  ADate := ADate + Time; //чтобы сортировка по дате была правильней, todo: по хорошему надо чётко отделять дату и время
  finally
    vSL.Free;
  end;
end;

function MyTryStrToTime(const AValue: string; var ATime: TTime): Boolean;
var
  vSL: TStrings;
begin
  Result := True;
  vSL := CreateDelimitedList(AValue, ':');
  try
    while vSL.Count < 4 do
      vSL.Add('0');

    try
      ATime := EncodeTime(StrToIntDef(vSL[0], 0), StrToIntDef(vSL[1], 0),
        StrToIntDef(vSL[2], 0), StrToIntDef(vSL[3], 0));
    except
      Result := False;
    end;
//  ADate := ADate + Time; //чтобы сортировка по дате была правильней, todo: по хорошему надо чётко отделять дату и время
  finally
    vSL.Free;
  end;
end;

procedure TFMXDateFieldEditor.DoOnChange;
var
  vDate: TDateTime;
begin
  if MyTryStrToDate(TDateEdit(FControl).Text, vDate) then
    SetFieldValue(vDate)
  else
    SetFieldValue(Null);
end;

procedure TFMXDateFieldEditor.FillEditor;
var
  vEdit: TDateEdit;
  vDate: TDateTime;
begin
  vEdit := TDateEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.Date := Null;
  end
  else
  begin
    vEdit.Enabled := FView.State > vsDisabled;
    vDate := FView.FieldValue;
    if vDate < 2 then
      vEdit.Date := Null
    else
      vEdit.Date := vDate;

    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
      vEdit.TabStop := False
    else
      vEdit.TabStop := FOwner.TabStop;
  end;
end;

function TFMXDateFieldEditor.GetNewValue: Variant;
var
  vDate: TDateTime;
begin
  if MyTryStrToDate(TDateEdit(FControl).Text, vDate) then
    Result := vDate
  else
    Result := Null;
end;

procedure TFMXDateFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TDateEdit(FControl).OnChange := AHandler;
end;

{ TFMXTimeFieldEditor }

function TFMXTimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TTimeEdit.Create(nil);
  TTimeEdit(Result).ShowClearButton := True;
end;

procedure TFMXTimeFieldEditor.DoOnChange;
var
  vTime: TTime;
begin
  if MyTryStrToTime(TTimeEdit(FControl).Text, vTime) then
    SetFieldValue(Frac(vTime))
  else
    SetFieldValue(Null);
end;

procedure TFMXTimeFieldEditor.FillEditor;
var
  vEdit: TTimeEdit;
begin
  vEdit := TTimeEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.Time := Null;
  end
  else
  begin
    vEdit.Enabled := FView.State > vsDisabled;
    vEdit.Time := FView.FieldValue;

    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
      vEdit.TabStop := False
    else
      vEdit.TabStop := FOwner.TabStop;
  end;
end;

procedure TFMXTimeFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TTimeEdit(FControl).OnChange := AHandler;
end;

{ TFMXBoolFieldEditor }

function TFMXBoolFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FNeedCreateCaption := False;

  Result := TCheckBox.Create(nil);

  if Assigned(FCreateParams) and (FCreateParams.IndexOfName('Caption') >= 0) then
    TCheckBox(Result).Text := FCreateParams.Values['Caption']
  else
    TCheckBox(Result).Text := FOwner.GetFieldTranslation(FFieldDef);

  if Assigned(FCreateParams) and (FCreateParams.IndexOfName('Hint') >= 0) then
    TCheckBox(Result).Hint := FCreateParams.Values['Hint']
  else
    TCheckBox(Result).Hint := FOwner.GetFieldTranslation(FFieldDef, tpHint);
end;

procedure TFMXBoolFieldEditor.DoOnChange;
begin
  SetFieldValue(TCheckBox(FControl).IsChecked);
end;

procedure TFMXBoolFieldEditor.FillEditor;
begin
  if VarIsNull(FView.FieldValue) then
    TCheckBox(FControl).IsChecked := False
  else
    TCheckBox(FControl).IsChecked := FView.FieldValue;

  TCheckBox(FControl).Enabled := FView.State = vsFullAccess;
  if TCheckBox(FControl).Enabled then
    TCheckBox(FControl).TabStop := FOwner.TabStop
  else
    TCheckBox(FControl).TabStop := False;
end;

procedure TFMXBoolFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TCheckBox(FControl).OnChange := AHandler;
end;

initialization

TPresenter.RegisterControlClass('FMX', uiTextEdit, '', TFMXTextEdit);
TPresenter.RegisterControlClass('FMX', uiTextEdit, 'phone', TFMXTextEdit);
TPresenter.RegisterControlClass('FMX', uiTextEdit, 'mask', TFMXTextEdit);
TPresenter.RegisterControlClass('FMX', uiTextEdit, 'memo', TFMXMemoFieldEditor);
TPresenter.RegisterControlClass('FMX', uiTextEdit, 'info', TFMXTextInfo);

TPresenter.RegisterControlClass('FMX', uiIntegerEdit, '', TFMXIntegerFieldEditor);
TPresenter.RegisterControlClass('FMX', uiIntegerEdit, 'simple', TFMXIntegerFieldEditor);
TPresenter.RegisterControlClass('FMX', uiIntegerEdit, 'info', TFMXTextInfo);
TPresenter.RegisterControlClass('FMX', uiIntegerEdit, 'spinner', TFMXSpinner);
TPresenter.RegisterControlClass('FMX', uiIntegerEdit, 'progress', TFMXProgress);
TPresenter.RegisterControlClass('FMX', uiIntegerEdit, 'pages', TFMXPagesFieldEditor);
TPresenter.RegisterControlClass('FMX', uiIntegerEdit, 'flags', TFMXIntegerFlagsEditor);

TPresenter.RegisterControlClass('FMX', uiEnumEdit, '', TFMXEnumEditor);
TPresenter.RegisterControlClass('FMX', uiEnumEdit, 'radio', TFMXEnumEditor);
TPresenter.RegisterControlClass('FMX', uiEnumEdit, 'info', TFMXTextInfo);
TPresenter.RegisterControlClass('FMX', uiEnumEdit, 'pages', TFMXPagesFieldEditor);

TPresenter.RegisterControlClass('FMX', uiFloatEdit, '', TFMXFloatFieldEditor);
TPresenter.RegisterControlClass('FMX', uiFloatEdit, 'simple', TFMXFloatFieldEditor);
TPresenter.RegisterControlClass('FMX', uiFloatEdit, 'info', TFMXTextInfo);

TPresenter.RegisterControlClass('FMX', uiDateEdit, '', TFMXDateFieldEditor);
TPresenter.RegisterControlClass('FMX', uiDateEdit, 'time', TFMXTimeFieldEditor);
TPresenter.RegisterControlClass('FMX', uiDateEdit, 'info', TFMXTextInfo);

TPresenter.RegisterControlClass('FMX', uiCurrencyEdit, 'info', TFMXTextInfo);

TPresenter.RegisterControlClass('FMX', uiBoolEdit, '', TFMXBoolFieldEditor);
TPresenter.RegisterControlClass('FMX', uiBoolEdit, 'simple', TFMXBoolFieldEditor);
TPresenter.RegisterControlClass('FMX', uiBoolEdit, 'pages', TFMXPagesFieldEditor);

TPresenter.RegisterControlClass('FMX', uiEntityEdit, 'info', TFMXTextInfo);
TPresenter.RegisterControlClass('FMX', uiEntityEdit, 'pages', TFMXPagesFieldEditor);

end.
