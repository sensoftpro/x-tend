unit uniSimpleEditors;

interface

uses
  uniArea, uPresenter, uConsts, Variants, Math, SysUtils, System.UITypes, uniStringGrid, uConfiguration, uInteractor,
  uUtils,
  Classes, uCollection, Types, uDefinition, uEnumeration, uUIBuilder, uView, uEntity, uLayout, Graphics, Controls, ComCtrls;

type
  TUniGUIEnumEditor = class(TUniGUIControl)
  private
    FEnum: TEnumeration;
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIGraphicEnumSelector = class(TUniGUIControl)
  private
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
    procedure CBOnDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  protected
    FEnum: TEnumeration;

    procedure DoDrawItem(const ACanvas: TCanvas; const AID: Integer; const ARect: TRect;
      AState: TOwnerDrawState); virtual;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIFlagsEditor = class(TUniGUIControl)
  private
    FEnum: TEnumeration;
    procedure FillList;
    procedure CLBOnClickCheck(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUITextInfo = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TUniGUITextFieldEditor = class(TUniGUIControl)
  private
    procedure OnPhoneKeyPress(Sender: TObject; var Key: Char);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIMaskFieldEditor = class(TUniGUIControl)
  private
    procedure OnEditorClick(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIImagedAction = class(TUniGUIControl)
  private
    FTrueImageID: Integer;
    FFalseImageID: Integer;
    FTrueHint: string;
    FFalseHint: string;
    FActionView: TView;
    procedure OnButtonClick(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIMemoFieldEditor = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUITextSelector = class(TUniGUIControl)
  private
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUILogEditor = class(TUniGUIControl)
  private
    FListView: TUniStringGrid;
    FData: TStringList;
    procedure OnListViewData(Sender: TObject; Item: string);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure SetParent(const Value: TUIArea); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;

  TUniGUIIntegerFieldEditor = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIFloatFieldEditor = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIBoolFieldEditor = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIDateFieldEditor = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUITimeFieldEditor = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIDateTimeFieldEditor = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure SetParent(const AParent: TUIArea); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIBoolImages = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TUniGUIImageByString = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TUniGUISelectedCaptionBoolFieldEditor = class(TUniGUIControl)
  private
    FSelected: Boolean;
    FSelectBackColor, FDefaultTextColor: TColor;
    procedure OnClick(Sender: TObject);
    procedure UpdateView;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TUniGUISpinner = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TUniGUIProgress = class(TUniGUIControl)
  private
    FColor: string;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TUniGUIColorPicker = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure SetParent(const AParent: TUIArea); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;


implementation

uses
  uDomain, uniLabel, uniEdit, uniGUITypes, uniDateTimePicker, uniMemo, uniCheckbox, uniRadioGroup, uniComboBox,
  StdCtrls, uniButton, uniImageList, uniColorPalette, uniImage, uniHTMLFrame, UniProgressBar, uniGUIClasses;


{ TUniGUITextInfo }

function TUniGUITextInfo.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniLabel.Create(ExtractOwner(AParent));
  TUniLabel(Result).Transparent := True;
  TUniLabel(Result).TextConversion := txtHTML;
//  if Assigned(FCreateParams) and (FCreateParams.Values['WordWrap'] = 'False') then
//    TUniLabel(Result).WordWrap := False;
  TUniLabel(Result).AutoSize := False;
end;

procedure TUniGUITextInfo.FillEditor;
var
  vEnum: TEnumeration;
  vEntity: TEntity;
  vColorField: TBaseField;
  vValue: Variant;
begin
  if VarIsNull(FView.FieldValue) then
    TUniLabel(FControl).Caption := ''
  else if FFieldDef.Kind = fkCurrency then
  begin
    if FFieldDef.Format <> '' then
      TUniLabel(FControl).Caption := FormatFloat(GetFormat, FView.FieldValue)
    else
      TUniLabel(FControl).Caption := FormatFloat('#,##0.00;;0', FView.FieldValue);
  end
  else if FFieldDef.Kind = fkFloat then
  begin
    if FFieldDef.Format <> '' then
      TUniLabel(FControl).Caption := FormatFloat(GetFormat, FView.FieldValue)
    else
      TUniLabel(FControl).Caption := FView.FieldValue;
  end
  else if FFieldDef.Kind = fkDateTime then
  begin
    vValue := FView.FieldValue;
    if IsZero(vValue, 1e-6) then
      TUniLabel(FControl).Caption := ''
    else if FFieldDef.Format <> '' then
      TUniLabel(FControl).Caption := FormatDateTime(GetFormat, vValue)
    else
      TUniLabel(FControl).Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss', vValue);
  end
  else if FFieldDef.Kind = fkEnum then
  begin
    vEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
    if not Assigned(vEnum) then
    begin
      vEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
      TUniLabel(FControl).Font.Color := TState(vEnum.Items[Integer(FView.FieldValue)]).Color;
    end;
    TUniLabel(FControl).Caption := vEnum.Items[Integer(FView.FieldValue)].DisplayText;
  end
  else if FFieldDef.Kind = fkObject then
  begin
    vEntity := TEntity(FView.FieldEntity);
    if not Assigned(vEntity) then
      TUniLabel(FControl).Caption := TObjectFieldDef(FView.Definition)._ContentDefinition._EmptyValue
    else begin
      TUniLabel(FControl).Caption := vEntity['Name'];
      if vEntity.Definition.ColorFieldName <> '' then
      begin
        vColorField := vEntity.FieldByName(vEntity.Definition.ColorFieldName);
        TUniLabel(FControl).Font.Color := (vColorField.Value);
      end;
    end;
  end
  else
    TUniLabel(FControl).Caption := FView.FieldValue;
end;

{ TUniGUITextFieldEditor }

function TUniGUITextFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniEdit.Create(ExtractOwner(AParent));

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TUniEdit(Result).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;

  if SameText('phone', FFieldDef.StyleName) then
    TUniEdit(Result).OnKeyPress := OnPhoneKeyPress
  else begin
    if SameText('mask', FFieldDef.StyleName) then
      TUniEdit(Result).PasswordChar := '*';
    TUniEdit(Result).OnKeyPress := nil;
  end;
end;

procedure TUniGUITextFieldEditor.DoOnChange;
begin
  SetFieldValue(TUniEdit(FControl).Text);
end;

procedure TUniGUITextFieldEditor.FillEditor;
var
  vEdit: TUniEdit;
begin
  vEdit := TUniEdit(FControl);
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

procedure TUniGUITextFieldEditor.OnPhoneKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, ['0'..'9', #8, '-', '+', '(', ')', ' ']) then Exit;
  Key := #0;
end;

procedure TUniGUITextFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TUniEdit(FControl).OnChange := AHandler;
end;

{ TUniGUIIntegerFieldEditor }

function TUniGUIIntegerFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniNumberEdit.Create(ExtractOwner(AParent));

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TUniNumberEdit(Result).MaxValue := TSimpleFieldDef(FFieldDef).MaxValue;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
    TUniNumberEdit(Result).MinValue := TSimpleFieldDef(FFieldDef).MinValue;
end;

procedure TUniGUIIntegerFieldEditor.FillEditor;
var
  vEdit: TUniNumberEdit;
begin
  vEdit := TUniNumberEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.Value := 0;
  end
  else
  begin
    vEdit.Enabled := FView.State >= vsFullAccess;
    vEdit.Value := FView.FieldValue;
  end;
  vEdit.ShowTrigger := vEdit.Visible and vEdit.Enabled;
end;

procedure TUniGUIIntegerFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TUniNumberEdit(FControl).OnChange := AHandler;
end;

{ TUniGUIFloatFieldEditor }

function TUniGUIFloatFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniEdit.Create(ExtractOwner(AParent));
end;

procedure TUniGUIFloatFieldEditor.DoOnChange;
var
  vValue: Double;

  function ToFloatDef(const AStr: string): Double;
  var
    vStr: string;
  begin
    if FormatSettings.DecimalSeparator = '.' then
      vStr := StringReplace(Trim(AStr), ',', FormatSettings.DecimalSeparator, [rfReplaceAll])
    else
      vStr := StringReplace(Trim(AStr), '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
    Result := StrToFloatDef(vStr, 0);
  end;
begin
  if TUniEdit(FControl).Text = '' then
    vValue := 0
  else
    vValue := ToFloatDef(TUniEdit(FControl).Text);

  if IsZero(vValue) then
    SetFieldValue(Null)
  else
    SetFieldValue(vValue);
end;

procedure TUniGUIFloatFieldEditor.FillEditor;
var
  vEdit: TUniEdit;
begin
  vEdit := TUniEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Text := '';
    vEdit.Enabled := False;
  end
  else
  begin
    vEdit.Text := FloatToStr(FView.FieldValue);
    vEdit.Enabled := FView.State >= vsFullAccess;
  end;
end;

procedure TUniGUIFloatFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TUniEdit(FControl).OnChange := AHandler;
end;

{ TUniGUIDateFieldEditor }

function TUniGUIDateFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vDef: TSimpleFieldDef;
begin
  Result := TUniDateTimePicker.Create(ExtractOwner(AParent));

  TUniDateTimePicker(Result).Kind := tUniDate;

  vDef := TSimpleFieldDef(FFieldDef);
  if not VarIsNull(vDef.MaxValue) then
    TUniDateTimePicker(Result).MaxDate := vDef.MaxValue;
  if not VarIsNull(vDef.MinValue) then
    TUniDateTimePicker(Result).MinDate := vDef.MinValue;
end;

procedure TUniGUIDateFieldEditor.DoOnChange;
begin
  SetFieldValue(TUniDateTimePicker(FControl).DateTime);
end;

procedure TUniGUIDateFieldEditor.FillEditor;
var
  vEdit: TUniDateTimePicker;
  vDate: TDateTime;
begin
  vEdit := TUniDateTimePicker(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.DateTime := 0;
  end
  else begin
    vEdit.Enabled := FView.State > vsDisabled;
    vDate := FView.FieldValue;
    if vDate < 2 then
      vEdit.DateTime := 0
    else
      vEdit.DateTime := vDate;
  end;
end;

procedure TUniGUIDateFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TUniDateTimePicker(FControl).OnChange := AHandler;
end;

{ TUniGUITimeFieldEditor }

function TUniGUITimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniDateTimePicker.Create(ExtractOwner(AParent));

  TUniDateTimePicker(Result).Kind := tUniTime;
end;

procedure TUniGUITimeFieldEditor.DoOnChange;
begin
  SetFieldValue(TUniDateTimePicker(FControl).DateTime);
end;

procedure TUniGUITimeFieldEditor.FillEditor;
var
  vEdit: TUniDateTimePicker;
begin
  vEdit := TUniDateTimePicker(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.DateTime := 0;
  end
  else begin
    vEdit.Enabled := FView.State > vsDisabled;
    vEdit.DateTime := FView.FieldValue;
  end;
end;

procedure TUniGUITimeFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TUniDateTimePicker(FControl).OnChange := AHandler;
end;

{ TUniGUIDateTimeFieldEditor }

function TUniGUIDateTimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vDef: TSimpleFieldDef;
begin
  Result := TUniDateTimePicker.Create(ExtractOwner(AParent));

  TUniDateTimePicker(Result).Kind := tUniDateTime;

  vDef := TSimpleFieldDef(FFieldDef);
  if not VarIsNull(vDef.MaxValue) then
    TUniDateTimePicker(Result).MaxDate := vDef.MaxValue;
  if not VarIsNull(vDef.MinValue) then
    TUniDateTimePicker(Result).MinDate := vDef.MinValue;
end;

procedure TUniGUIDateTimeFieldEditor.DoOnChange;
begin
  SetFieldValue(TUniDateTimePicker(FControl).DateTime);
end;

procedure TUniGUIDateTimeFieldEditor.FillEditor;
var
  vEdit: TUniDateTimePicker;
begin
  vEdit := TUniDateTimePicker(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.DateTime := 0;
  end
  else begin
    vEdit.Enabled := FView.State > vsDisabled;
    vEdit.DateTime := Int(FView.FieldValue) +  Frac(FView.FieldValue);
  end;
end;

procedure TUniGUIDateTimeFieldEditor.SetParent(const AParent: TUIArea);
begin
  inherited SetParent(AParent);

  if Assigned(AParent) then
  begin
    TUniDateTimePicker(FControl).DateFormat := 'dd.MM.yyyy';
    TUniDateTimePicker(FControl).TimeFormat := 'HH:mm:ss';
  end;
end;

procedure TUniGUIDateTimeFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TUniDateTimePicker(FControl).OnChange := AHandler;
end;

{ TUniGUIMemoFieldEditor }

function TUniGUIMemoFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniMemo.Create(ExtractOwner(AParent));
  TUniMemo(Result).ScrollBars := ssVertical;
//  TUniMemo(Result). := True;
  TUniMemo(Result).ParentFont := True;
  TUniMemo(Result).ParentColor := True;

//  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
//    TUniMemo(Result). := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TUniGUIMemoFieldEditor.DoOnChange;
begin
  SetFieldValue(TUniMemo(FControl).Text);
end;

procedure TUniGUIMemoFieldEditor.FillEditor;
var
  vEdit: TUniMemo;
begin
  vEdit := TUniMemo(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Text := '';
    vEdit.Enabled := False;
  end
  else begin
    vEdit.Enabled := True;
    vEdit.Text := FView.FieldValue;
//    vEdit.Se := 1000;
    vEdit.ReadOnly := FView.State < vsFullAccess;
  end;
end;

procedure TUniGUIMemoFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TUniMemo(FControl).OnChange := AHandler;
end;

{ TUniGUIBoolFieldEditor }

function TUniGUIBoolFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FNeedCreateCaption := False;

  Result := TUniCheckBox.Create(ExtractOwner(AParent));

  if Assigned(FCreateParams) and (FCreateParams.IndexOfName('Caption') >= 0) then
    TUniCheckBox(Result).Caption := FCreateParams.Values['Caption']
  else
    TUniCheckBox(Result).Caption := FOwner.GetFieldTranslation(FFieldDef);

  if Assigned(FCreateParams) and (FCreateParams.IndexOfName('Hint') >= 0) then
    TUniCheckBox(Result).Hint := FCreateParams.Values['Hint']
  else
    TUniCheckBox(Result).Hint := FOwner.GetFieldTranslation(FFieldDef, tpHint);
end;

procedure TUniGUIBoolFieldEditor.DoOnChange;
begin
  SetFieldValue(TUniCheckBox(FControl).Checked);
end;

procedure TUniGUIBoolFieldEditor.FillEditor;
begin
  if VarIsNull(FView.FieldValue) then
    TUniCheckBox(FControl).Checked := False
  else
    TUniCheckBox(FControl).Checked := FView.FieldValue;

  TUniCheckBox(FControl).Enabled := FView.State = vsFullAccess;
end;

procedure TUniGUIBoolFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TUniCheckBox(FControl).OnClick := AHandler;
end;

{ TUniGUISpinner }

function TUniGUISpinner.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniHTMLFrame.Create(ExtractOwner(AParent));
  TUniHTMLFrame(Result).Width := ALayout.Width;
  TUniHTMLFrame(Result).Height := ALayout.Height;


  TUniHTMLFrame(Result).HTML.Text := '<html><head><script></script></head><body><div style="width: 100vh; background-color: red; height: 100vh;"></div></body></html>';
end;

procedure TUniGUISpinner.FillEditor;
begin
  inherited;

end;

{ TUniGUILogEditor }

procedure TUniGUILogEditor.DoBeforeFreeControl;
begin
  FreeAndNil(FData);
end;

function TUniGUILogEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FListView := TUniStringGrid.Create(ExtractOwner(AParent));
//  FListView.On := OnListViewData;
  FListView.AutoSize := True;
  FListView.ReadOnly := True;

  FData := TStringList.Create;

  Result := FListView;
end;

procedure TUniGUILogEditor.FillEditor;
begin
  inherited;

  FData.Text := FView.FieldValue;

  FListView.ColCount := FData.Count;

  FListView.Refresh;
end;

procedure TUniGUILogEditor.OnListViewData(Sender: TObject; Item: string);
begin
  Item := FData[FData.Count - 1];
end;

procedure TUniGUILogEditor.SetParent(const Value: TUIArea);
begin
  inherited;
//  FListView.O := vsReport;
end;

{ TUniGUIEnumEditor }

procedure TUniGUIEnumEditor.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

function TUniGUIEnumEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  if not Assigned(FEnum) then
    FEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);

  if FFieldDef.StyleName = 'radio' then
  begin
    Result := TUniRadioGroup.Create(ExtractOwner(AParent));
    TUniRadioGroup(Result).Name := 'radio';
    TUniRadioGroup(Result).Caption := '';
    FNeedCreateCaption := False;
  end
  else
  begin
    Result := TUniComboBox.Create(ExtractOwner(AParent));
    TUniComboBox(Result).Style := csDropDownList;
    if Assigned(FCreateParams) then
    begin
      if FCreateParams.Values['DropDownListStyle'] = 'Fixed' then
        TUniComboBox(Result).Style := csDropDown
      else if FCreateParams.Values['DropDownListStyle'] = 'Edit' then
        TUniComboBox(Result).Style := csDropDownList;
    end;
    TUniComboBox(Result).OnDropDown := CBOnInitPopup;
  end;
end;

procedure TUniGUIEnumEditor.DoOnChange;
begin
  if FFieldDef.StyleName = 'radio' then
  begin
    if FFieldDef.HasFlag(cRequired) then
      SetFieldValue(TUniRadioGroup(FControl).ItemIndex + 1)
    else
      SetFieldValue(TUniRadioGroup(FControl).ItemIndex);
  end
  else
  begin
    if FFieldDef.HasFlag(cRequired) then
      SetFieldValue(TUniComboBox(FControl).ItemIndex + 1)
    else
      SetFieldValue(TUniComboBox(FControl).ItemIndex);
  end;
end;

procedure TUniGUIEnumEditor.FillEditor;
var
  vEdit: TUniComboBox;
  vRadioEdit: TUniRadioGroup;
  vItemIndex: Integer;
begin
  FillList;

  if FFieldDef.StyleName = 'radio' then
  begin
    vRadioEdit := TUniRadioGroup(FControl);
    if VarIsNull(FView.FieldValue) then
    begin
      vRadioEdit.Enabled := False;
    end
    else begin
      vItemIndex := FView.FieldValue;
      if FFieldDef.HasFlag(cRequired) then
        vItemIndex := vItemIndex - 1;
      vRadioEdit.ItemIndex := vItemIndex;

      vRadioEdit.Enabled := FView.State >= vsSelectOnly;
    end;
  end
  else
  begin
    vEdit := TUniComboBox(FControl);
    if VarIsNull(FView.FieldValue) then
    begin
      vEdit.ItemIndex := -1;
      vEdit.Text := '';
      vEdit.Enabled := False;
    end
    else begin
      vItemIndex := FView.FieldValue;
      if FFieldDef.HasFlag(cRequired) then
        vItemIndex := vItemIndex - 1;
      vEdit.ItemIndex := vItemIndex;

      vEdit.Enabled := FView.State >= vsSelectOnly;
    end;
  end;
end;

procedure TUniGUIEnumEditor.FillList;
var
  vEnumItem: TEnumItem;
  vItems: TStrings;
begin
  if FFieldDef.StyleName = 'radio' then
  begin
    vItems := TUniRadioGroup(FControl).Items;
    vItems.BeginUpdate;
    try
      vItems.Clear;
      for vEnumItem in FEnum do
        if not FFieldDef.HasFlag(cRequired) or (vEnumItem.ID > 0) then
          vItems.Add(vEnumItem.DisplayText);
    finally
      vItems.EndUpdate;
    end;
  end
  else begin
    vItems := TUniComboBox(FControl).Items;

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

procedure TUniGUIEnumEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  if FFieldDef.StyleName = 'radio' then
    TUniRadioGroup(FControl).OnClick := AHandler
  else
    TUniComboBox(FControl).OnChange := AHandler;
end;

{ TUniGUIGraphicEnumSelector }

procedure TUniGUIGraphicEnumSelector.CBOnDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
end;

procedure TUniGUIGraphicEnumSelector.CBOnInitPopup(Sender: TObject);
begin
end;

function TUniGUIGraphicEnumSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
end;

procedure TUniGUIGraphicEnumSelector.DoDrawItem(const ACanvas: TCanvas; const AID: Integer; const ARect: TRect;
  AState: TOwnerDrawState);
begin
end;

procedure TUniGUIGraphicEnumSelector.DoOnChange;
begin
end;

procedure TUniGUIGraphicEnumSelector.FillEditor;
begin
end;

procedure TUniGUIGraphicEnumSelector.FillList;
begin
end;

procedure TUniGUIGraphicEnumSelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
end;

{ TUniGUIFlagsEditor }

procedure TUniGUIFlagsEditor.CLBOnClickCheck(Sender: TObject);
begin
end;

function TUniGUIFlagsEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
end;

procedure TUniGUIFlagsEditor.DoOnChange;
begin
end;

procedure TUniGUIFlagsEditor.FillEditor;
begin
end;

procedure TUniGUIFlagsEditor.FillList;
begin
end;

procedure TUniGUIFlagsEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
end;

{ TUniGUITextSelector }

procedure TUniGUITextSelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

function TUniGUITextSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniComboBox.Create(ExtractOwner(AParent));

  TUniComboBox(Result).OnDropDown := CBOnInitPopup;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TUniComboBox(Result).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TUniGUITextSelector.DoOnChange;
begin
  SetFieldValue(TUniComboBox(FControl).Text);
end;

procedure TUniGUITextSelector.FillEditor;
var
  vEdit: TUniComboBox;
begin
  FillList;

  vEdit := TUniComboBox(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Text := '';
    vEdit.Enabled := False;
  end
  else begin
    vEdit.Text := FView.FieldValue;
    vEdit.Enabled := FView.State >= vsSelectOnly;
  end;
end;

procedure TUniGUITextSelector.FillList;
var
  i: Integer;
  vDictionary: string;
  vFieldName: string;
  vPos: Integer;
  vCollection: TCollection;
  //vComPorts: TStrings;
begin
  TUniComboBox(FControl).Items.BeginUpdate;
  TUniComboBox(FControl).Items.Clear;

{  if FFieldDef.StyleName = 'comport' then
  begin
    vComPorts := TStringList.Create;
    EnumComPorts(vComPorts);
    for i := 0 to vComPorts.Count - 1 do
      TcxComboBox(FControl).Properties.Items.Add(vComPorts[i]);
    FreeAndNil(vComPorts);
    TcxComboBox(FControl).EditValue := FView.FieldValue;
  end
  else }
  begin
    vDictionary := TSimpleFieldDef(FFieldDef).Dictionary;
    vPos := Pos('.', vDictionary);
    if vPos > 0 then
    begin
      vFieldName := vDictionary;
      vDictionary := Copy(vDictionary, 1, vPos - 1);
      Delete(vFieldName, 1, vPos);
    end
    else
      vFieldName := '';

    if vDictionary <> '' then
    begin
      vCollection := TDomain(FView.Domain).CollectionByName(vDictionary);
      for i := 0 to vCollection.Count - 1 do
        if vFieldName <> '' then
          TUniComboBox(FControl).Items.Add(vCollection[i].FieldValues[vFieldName])
        else
          TUniComboBox(FControl).Items.Add(vCollection[i]['Name']);

      TUniComboBox(FControl).Text := FView.FieldValue;
    end;
  end;
  TUniComboBox(FControl).Items.EndUpdate;
end;

procedure TUniGUITextSelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TComboBox(FControl).OnChange := AHandler;
end;

{ TUniGUIMaskFieldEditor }

function TUniGUIMaskFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vMask: string;
begin
  Result := TUniEdit.Create(ExtractOwner(AParent));
  vMask := '';
  if Assigned(FFieldDef)then
  begin
    if SameText('phone', FFieldDef.StyleName) then
      vMask := '!\+9 (999\) 000-00-00;1;_'
    else if SameText('INN', FFieldDef.StyleName) then
      vMask := '9999999999999';
  end;
  TUniEdit(Result).InputMask.Mask := vMask;
end;

procedure TUniGUIMaskFieldEditor.DoOnChange;
begin
  SetFieldValue(TUniEdit(FControl).Text);
end;

procedure TUniGUIMaskFieldEditor.FillEditor;
var
  vEdit: TUniEdit;
begin
  vEdit := TUniEdit(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Text := '';
    vEdit.Enabled := False;
    vEdit.Cursor := crHandPoint;
    vEdit.OnClick := OnEditorClick;
  end
  else
  begin
    vEdit.Text := FView.FieldValue;
    vEdit.Enabled := True;
    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
    begin
      vEdit.Cursor := crHandPoint;
      vEdit.OnClick := OnEditorClick;
    end
    else begin
      vEdit.Cursor := crDefault;
      vEdit.OnClick := nil;
    end;
  end;
end;

procedure TUniGUIMaskFieldEditor.OnEditorClick(Sender: TObject);
begin
  if SameText('email', FFieldDef.StyleName) then
    TPresenter(FPresenter).OpenFile('mailto:' + FView.FieldValue)
  else if SameText('url', FFieldDef.StyleName) then
    TPresenter(FPresenter).OpenFile(FView.FieldValue);
end;

procedure TUniGUIMaskFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TUniEdit(FControl).OnChange := AHandler;
end;

{ TUniGUIImagedAction }

procedure TUniGUIImagedAction.DoBeforeFreeControl;
begin
  if Assigned(FActionView) then
  begin
    FActionView.RemoveListener(Self);
    FActionView := nil;
  end;
end;

function TUniGUIImagedAction.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vActionName: string;
  vImageSize: Integer;
  vButton: TUniButton;
begin
  if Assigned(FCreateParams) then
  begin
    vActionName := FCreateParams.Values['action'];
    vImageSize := StrToIntDef(FCreateParams.Values['ImageSize'], 16);
    FFalseImageID := FOwner.GetImageID(StrToIntDef(FCreateParams.Values['false'], -1));
    FTrueImageID := FOwner.GetImageID(StrToIntDef(FCreateParams.Values['true'], -1));
    FTrueHint := FCreateParams.Values['trueHint'];
    FFalseHint := FCreateParams.Values['falseHint'];
  end
  else begin
    vActionName := '';
    vImageSize := 16;
    FTrueImageID := FOwner.GetImageID(-1);
    FFalseImageID := FOwner.GetImageID(-1);
    FTrueHint := TFieldDef(FView.Definition)._Caption;
    FFalseHint := TFieldDef(FView.Definition)._Caption;
  end;

  if vActionName <> '' then
  begin
    FActionView := FOwner.RootView.BuildView(vActionName);
    FActionView.AddListener(Self);
    if FActionView.DefinitionKind = dkUndefined then
    begin
      FActionView.CleanView;
      FActionView := nil;
    end;
  end
  else
    FActionView := nil;

  vButton := TUniButton.Create(ExtractOwner(AParent));
  vButton.Images := TUniNativeImageList(FUIBuilder.Images[vImageSize]);
  vButton.OnClick := OnButtonClick;

  Result := vButton;
end;

procedure TUniGUIImagedAction.FillEditor;
begin
  if VarIsNull(FView.FieldValue) or (not FView.FieldValue) then
  begin
    TUniButton(FControl).ImageIndex := FFalseImageID;
    TUniButton(FControl).Hint := FTrueHint;
  end
  else
  begin
    TUniButton(FControl).ImageIndex := FTrueImageID;
    TUniButton(FControl).Hint := FFalseHint;
  end;

  TUniButton(FControl).Enabled := FView.State = vsFullAccess;
end;

procedure TUniGUIImagedAction.OnButtonClick(Sender: TObject);
begin
  if Assigned(FActionView) then
    FActionView.ExecuteAction(FOwner.Holder);
end;

procedure TUniGUIImagedAction.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TUniButton(FControl).OnClick := AHandler;
end;

{ TUniGUIColorPicker }

function TUniGUIColorPicker.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniColorPalette.Create(ExtractOwner(AParent));
end;

procedure TUniGUIColorPicker.DoOnChange;
begin
  SetFieldValue(TUniColorPalette(FControl).Color);
end;

procedure TUniGUIColorPicker.FillEditor;
var
  vEdit: TUniColorPalette;
begin
  vEdit := TUniColorPalette(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Color := 0;
    vEdit.Enabled := False;
  end else
  begin
    vEdit.Color := TColor(FView.FieldValue);
    vEdit.Enabled := FView.State >= vsFullAccess;
  end;
end;

procedure TUniGUIColorPicker.SetParent(const AParent: TUIArea);
begin
  inherited SetParent(AParent);
//  TUniColorPalette(FControl). := [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames];
end;

procedure TUniGUIColorPicker.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
//  TUniColorPalette(FControl).OnChange := AHandler;
end;

{ TUniGUIBoolImages }

function TUniGUIBoolImages.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniImage.Create(ExtractOwner(AParent));
  TUniImage(Result).Transparent := True;
end;

procedure TUniGUIBoolImages.FillEditor;
var
  i, vImageSize: Integer;
  vStream: TStream;
begin
  inherited;
  i := FCreateParams.IndexOfName(FView.FieldValue);

  if i < 0 then Exit;

  vImageSize := 16;
  if FCreateParams.IndexOfName('ImageSize') >= 0 then
    vImageSize := StrToIntDef(FCreateParams.Values['ImageSize'], 16);

  i := StrToIntDef(FCreateParams.Values[FView.FieldValue], 0);

  vStream := TConfiguration(TInteractor(FView.Interactor).Configuration).Icons.IconByIndex(i, vImageSize);
  if Assigned(vStream) then
  begin
    vStream.Position := 0;
    TUniImage(FControl).Picture.LoadFromStream(vStream);
    if FView.FieldValue then
      TUniImage(FControl).Hint := FCreateParams.Values['trueHint']
    else
      TUniImage(FControl).Hint := FCreateParams.Values['falseHint'];
    TUniImage(FControl).Parent.Invalidate;
  end;
end;

{ TUniGUIImageByString }

function TUniGUIImageByString.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniImage.Create(ExtractOwner(AParent));
  TUniImage(Result).Transparent := True;
  TUniImage(Result).Center := True;
end;

procedure TUniGUIImageByString.FillEditor;
var
  i, vImageSize: Integer;
  vStream: TStream;
begin
  inherited;

  if Assigned(FCreateParams) then
    i := FCreateParams.IndexOfName(FView.FieldValue)
  else
    i := -1;

  if i < 0 then Exit;

  i := StrToIntDef(FCreateParams.Values[FView.FieldValue], 0);
  vImageSize := StrToIntDef(FCreateParams.Values['ImageSize'], 16);

  vStream := TConfiguration(TInteractor(FView.Interactor).Configuration).Icons.IconByIndex(i, vImageSize);
  if Assigned(vStream) then
  begin
    vStream.Position := 0;
    TUniImage(FControl).Picture.LoadFromStream(vStream);
  end;
end;

{ TUniGUISelectedCaptionBoolFieldEditor }

function TUniGUISelectedCaptionBoolFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FNeedCreateCaption := False;

  Result := TUniLabel.Create(ExtractOwner(AParent));
  TUniLabel(Result).OnClick := OnClick;
  if Assigned(FCreateParams) then
    TUniLabel(Result).Caption := FCreateParams.Values['Caption'];
  TUniLabel(Result).Transparent := False;
  TUniLabel(Result).Cursor := crHandPoint;

  if ALayout.Kind = lkPanel then
  begin
    FSelectBackColor := AlphaColorToColor($FF5132);
    if Assigned(FCreateParams) and (FCreateParams.IndexOfName('select_backcolor') > -1) then
      FSelectBackColor := AlphaColorToColor(StrToIntDef('$' + FCreateParams.Values['select_backcolor'], 0));

    FDefaultTextColor := AlphaColorToColor(ALayout.Font.Color);
    TUniLabel(Result).Alignment := ALayout.Alignment;
  end;
end;

procedure TUniGUISelectedCaptionBoolFieldEditor.FillEditor;
begin
  if VarIsNull(FView.FieldValue) then
    FSelected := False
  else
    FSelected := FView.FieldValue;

  UpdateView;
end;

procedure TUniGUISelectedCaptionBoolFieldEditor.OnClick(Sender: TObject);
begin
  FSelected := not FSelected;

  SetFieldValue(FSelected);

  UpdateView;
end;

procedure TUniGUISelectedCaptionBoolFieldEditor.UpdateView;
begin
  if FSelected then
  begin
    TUniLabel(FControl).Font.Color := clWhite;
    TUniLabel(FControl).Color := FSelectBackColor;
    TUniLabel(FControl).Transparent := False;
  end
  else
  begin
    TUniLabel(FControl).Transparent := True;
    TUniLabel(FControl).Font.Color := FDefaultTextColor;
  end;
end;

{ TUniGUIProgress }

function TUniGUIProgress.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vScript: TUniControl;
begin
  Result := TUniProgressBar.Create(ExtractOwner(AParent));
  vScript := TUniHTMLFrame.Create(ExtractOwner(AParent));
  FColor := Cardinal(AlphaColorToColor(ALayout.Color)).ToString;
  TUniHTMLFrame(vScript).HTML.Text := 'document.getElementById(' + TUniProgressBar(Result).JSId + ').setStyle(' + 'background-color' + '#' + FColor + ')';
  TUniProgressBar(Result).Color := AlphaColorToColor(ALayout.Color);

  TUniProgressBar(Result).Text := '';
  FNeedCreateCaption := False;
//  TUniProgressBar(Result).Smooth := True;
  TUniProgressBar(Result).Position := 0;
  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TUniProgressBar(Result).Max := TSimpleFieldDef(FFieldDef).MaxValue
  else
    TUniProgressBar(Result).Max := 100;
end;

procedure TUniGUIProgress.FillEditor;
var
  vProgressBarID: string;
begin
  inherited;
  vProgressBarID := TUniProgressBar(FControl).JSId;
  TUniProgressBar(FControl).Position := FView.FieldValue;
//
//  TUniProgressBar(FControl).JSInterface.JSCall('document.getElementById(' + vProgressBarID + ').setStyle',
//   ['background-color', '#' + FColor]);
//  TUniProgressBar(FControl).JSInterface.JSCall('document.getElementById(' + vProgressBarID + ').setStyle',
//   ['background-image', 'none']);
end;

initialization
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, '', TUniGUITextFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'phone', TUniGUITextFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'email', TUniGUIMaskFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'url', TUniGUIMaskFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'INN', TUniGUIMaskFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'info', TUniGUITextInfo);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'log', TUniGUILogEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'memo', TUniGUIMemoFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiIntegerEdit, '', TUniGUIIntegerFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiIntegerEdit, 'simple', TUniGUIIntegerFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiFloatEdit, '', TUniGUIFloatFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiFloatEdit, 'currency_rate', TUniGUIFloatFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiCurrencyEdit, '', TUniGUIFloatFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiDateEdit, '', TUniGUIDateFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiDateEdit, 'time', TUniGUITimeFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiDateEdit, 'datetime', TUniGUIDateTimeFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiBoolEdit, '', TUniGUIBoolFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiBoolEdit, 'simple', TUniGUIBoolFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiBoolEdit, 'imaged_action', TUniGUIImagedAction);
  TPresenter.RegisterControlClass('Web.UniGUI', uiEnumEdit, '', TUniGUIEnumEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiEnumEdit, 'radio', TUniGUIEnumEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'ImageByString', TUniGUIImageByString);
  TPresenter.RegisterControlClass('Web.UniGUI', uiBoolEdit, 'images', TUniGUIBoolImages);
  TPresenter.RegisterControlClass('Web.UniGUI', uiBoolEdit, 'selected_caption', TUniGUISelectedCaptionBoolFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiIntegerEdit, 'spinner', TUniGUISpinner);
  TPresenter.RegisterControlClass('Web.UniGUI', uiIntegerEdit, 'progress', TUniGUIProgress);

end.
