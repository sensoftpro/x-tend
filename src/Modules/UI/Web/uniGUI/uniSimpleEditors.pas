unit uniSimpleEditors;

interface

uses
  uniArea, uPresenter, uConsts, Variants, Math, SysUtils, System.UITypes, uniStringGrid, uConfiguration, uInteractor,
  uUtils, Generics.Collections, Windows,
  Classes, uCollection, Types, uDefinition, uEnumeration, uUIBuilder, uView, uEntity, uLayout, Graphics, Controls,
  ComCtrls,

  uDomain, uniLabel, uniEdit, uniGUITypes, uniDateTimePicker, uniMemo, uniCheckbox, uniRadioGroup, uniComboBox, uniPanel,
  StdCtrls, uniButton, uniImageList, uniColorPalette, uniImage, uniHTMLFrame, uniProgressBar, uniGUIClasses,
  uniGUIApplication, uniGUIJSUtils, uUniGUIPresenter, uniPageControl, uniChart, uniMultiItem, uniListBox, uniFileUpload,
  uniCanvas, uniGUIFrame, uniColorButton;
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
  protected
    FEnum: TEnumeration;
    FComboBox: TUniComboBox;
    procedure DoDrawItem(const ACanvas: TUniCanvas; const AID: Integer; const ARect: TRect;
      AState: TOwnerDrawState); virtual;
    procedure DoBeforeFreeControl; override;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUILineStyleSelector = class(TUniGUIGraphicEnumSelector)
  protected
    procedure DoDrawItem(const ACanvas: TUniCanvas; const AID: Integer; const ARect: TRect;
      AState: TOwnerDrawState); override;
  end;

  TUniGUIFlagsEditor = class(TUniGUIControl)
  private
    FListBox: TUniListBox;
    FEnum: TEnumeration;
    procedure FillList;
    procedure CLBOnClickCheck(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIIntegerFlagsEditor = class(TUniGUIControl)
  private
    FListBox: TUniListBox;
    FDisplayFlagCount: Integer;
    FCaptions: TStrings;
    procedure FillList;
    procedure CLBOnClickCheck(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure DoOnExit(Sender: TObject); override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUITextInfo = class(TUniGUIControl)
  private
    FLabel: TUniLabel;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure DoOnChange; override;
    procedure FillEditor; override;
  end;

  TUniGUITextFieldEditor = class(TUniGUIControl)
  private
    FEdit: TUniEdit;
    procedure OnPhoneKeyPress(Sender: TObject; var Key: Char);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIMaskFieldEditor = class(TUniGUIControl)
  private
    FEdit: TUniEdit;
    FLabel: TUniLabel;
    procedure OnEditorClick(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIImagedAction = class(TUniGUIControl)
  private
    FButton: TUniButton;
    FTrueImageIndex: Integer;
    FFalseImageIndex: Integer;
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
  private
    FMemo: TUniMemo;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIEnumFieldEditor = class(TUniGUIControl)
  private
    FRadioGroup: TUniRadioGroup;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUITextSelector = class(TUniGUIControl)
  private
    FComboBox: TUniComboBox;
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUILogEditor = class(TUniGUIControl)
  private
    FListView: TUniStringGrid;
    FData: TStringList;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;

  TUniGUISelectFolderFieldEditor = class(TUniGUIControl)
  private
    FText: TUniEdit;
    FBtn: TUniButton;
    FAction: TUniFileUpload;
    procedure CLBBrowseForCFolder(Sender: TObject);
    procedure BrowseForFolder1Accept(Sender: TObject; AStream: TFileStream);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIFileNameFieldEditor = class(TUniGUIControl)
  private
    FBtn: TUniButton;
    FAction: TUniFileUpload;
    FText: TUniEdit;
    procedure CLBBrowseForCFolder(Sender: TObject);
    procedure OnAccept(Sender: TObject; AStream: TFileStream);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  public
    property TextEdit: TUniEdit read FText;
  end;

  TUniGUIIntegerFieldEditor = class(TUniGUIControl)
  private
    FNumberEdit: TUniNumberEdit;
    procedure CLBOnChangeValue(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure DoOnChange; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIFloatFieldEditor = class(TUniGUIControl)
  private
    FEdit: TUniEdit;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIBoolFieldEditor = class(TUniGUIControl)
  private
    FCheckBox: TUniCheckBox;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIDateFieldEditor = class(TUniGUIControl)
  private
    FDateTimePicker: TUniDateTimePicker;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUITimeFieldEditor = class(TUniGUIControl)
  private
    FDateTimePicker: TUniDateTimePicker;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIDateTimeFieldEditor = class(TUniGUIControl)
  private
    FDateTimePicker: TUniDateTimePicker;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure SetParent(const AParent: TUIArea); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIBoolImages = class(TUniGUIControl)
  private
    FImage: TUniImage;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;

  TUniGUIImageViewer = class(TUniGUIControl)
  private
    FImage: TUniImage;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;

  TUniGUIImageByString = class(TUniGUIControl)
  private
    FImage: TUniImage;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;

  TUniGUISelectedCaptionBoolFieldEditor = class(TUniGUIControl)
  private
    FSelected: Boolean;
    FInitialColor: TColor;
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
    FProgress: TUniProgressBar;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure DoOnChange; override;
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

  TUniGUIPagesFieldEditor = class(TUniGUIControl)
  private
    FPageControl: TUniPageControl;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TUniGUIGauge = class(TUniGUIControl)
  private
    FSeries: TUniGaugeSeries;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;


implementation


{ TUniGUITextInfo }

procedure TUniGUITextInfo.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FLabel);
end;

function TUniGUITextInfo.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FLabel := TUniLabel.Create(TUniPanel(Result));
  FLabel.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FLabel.LayoutConfig.Width := '100%';
  FLabel.LayoutConfig.Height := '100%';
  FLabel.Transparent := True;
  FLabel.BorderStyle := TUniBorderStyle.ubsNone;
  FLabel.TextConversion := txtRegular;
//  if Assigned(FCreateParams) and (FCreateParams.Values['WordWrap'] = 'False') then
//    TUniLabel(Result).WordWrap := False;
  FLabel.AutoSize := False;
end;

procedure TUniGUITextInfo.DoOnChange;
begin
  inherited;
  SetFieldValue(FLabel.Text);
end;

procedure TUniGUITextInfo.FillEditor;
var
  vEnum: TEnumeration;
  vEntity: TEntity;
  vColorField: TBaseField;
  vValue: Variant;
begin
  if VarIsNull(FView.FieldValue) then
    FLabel.Caption := ''
  else if FFieldDef.Kind = fkCurrency then
  begin
    if FFieldDef.Format <> '' then
      FLabel.Caption := FormatFloat(GetFormat, FView.FieldValue)
    else
      FLabel.Caption := FormatFloat('#,##0.00;;0', FView.FieldValue);
  end
  else if FFieldDef.Kind = fkFloat then
  begin
    if FFieldDef.Format <> '' then
      FLabel.Caption := FormatFloat(GetFormat, FView.FieldValue)
    else
      FLabel.Caption := FView.FieldValue;
  end
  else if FFieldDef.Kind = fkDateTime then
  begin
    vValue := FView.FieldValue;
    if IsZero(vValue, 1e-6) then
      FLabel.Caption := ''
    else if FFieldDef.Format <> '' then
      FLabel.Caption := FormatDateTime(GetFormat, vValue)
    else
      FLabel.Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss', vValue);
  end
  else if FFieldDef.Kind = fkEnum then
  begin
    vEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
    if not Assigned(vEnum) then
    begin
      vEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
      FLabel.Font.Color := TState(vEnum.Items[Integer(FView.FieldValue)]).Color;
    end;
    FLabel.Caption := vEnum.Items[Integer(FView.FieldValue)].DisplayText;
  end
  else if FFieldDef.Kind = fkObject then
  begin
    vEntity := TEntity(FView.FieldEntity);
    if not Assigned(vEntity) then
      FLabel.Caption := TObjectFieldDef(FView.Definition)._ContentDefinition._EmptyValue
    else begin
      FLabel.Caption := vEntity['Name'];
      if vEntity.Definition.ColorFieldName <> '' then
      begin
        vColorField := vEntity.FieldByName(vEntity.Definition.ColorFieldName);
        FLabel.Font.Color := (vColorField.Value);
      end;
    end;
  end
  else
    FLabel.Caption := FView.FieldValue;
end;

{ TUniGUITextFieldEditor }

procedure TUniGUITextFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEdit);
end;

function TUniGUITextFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FEdit := TUniEdit.Create(TUniPanel(Result));
  FEdit.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FEdit.LayoutConfig.Width := '100%';
  FEdit.LayoutConfig.Height := '100%';
  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    FEdit.MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;

  if SameText('phone', FFieldDef.StyleName) then
    FEdit.OnKeyPress := OnPhoneKeyPress
  else begin
    if SameText('mask', FFieldDef.StyleName) then
      FEdit.PasswordChar := '*';
    FEdit.OnKeyPress := nil;
  end;
end;

procedure TUniGUITextFieldEditor.DoOnChange;
begin
  SetFieldValue(FEdit.Text);
end;

procedure TUniGUITextFieldEditor.FillEditor;
var
  vEdit: TUniEdit;
begin
  vEdit := FEdit;
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
  FEdit.OnChange := AHandler;
end;

{ TUniGUIIntegerFieldEditor }

procedure TUniGUIIntegerFieldEditor.CLBOnChangeValue(Sender: TObject);
begin
   FOwner.OnChange(Sender);
end;

procedure TUniGUIIntegerFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FNumberEdit);
end;

function TUniGUIIntegerFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FNumberEdit := TUniNumberEdit.Create(TUniPanel(Result));
  FNumberEdit.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FNumberEdit.LayoutConfig.Width := '100%';
  FNumberEdit.LayoutConfig.Height := '100%';
  FNumberEdit.OnChangeValue := CLBOnChangeValue;
  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    FNumberEdit.MaxValue := TSimpleFieldDef(FFieldDef).MaxValue;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
    FNumberEdit.MinValue := TSimpleFieldDef(FFieldDef).MinValue;
end;

procedure TUniGUIIntegerFieldEditor.DoOnChange;
begin
  inherited DoOnChange;
  SetFieldValue(Round(FNumberEdit.Value));
end;

procedure TUniGUIIntegerFieldEditor.FillEditor;
var
  vEdit: TUniNumberEdit;
begin
  vEdit := FNumberEdit;

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
  FNumberEdit.OnChange := AHandler;
end;

{ TUniGUIFloatFieldEditor }

procedure TUniGUIFloatFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEdit);
end;

function TUniGUIFloatFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FEdit := TUniEdit.Create(TUniPanel(Result));
  FEdit.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FEdit.LayoutConfig.Width := '100%';
  FEdit.LayoutConfig.Height := '100%';
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
  if FEdit.Text = '' then
    vValue := 0
  else
    vValue := ToFloatDef(FEdit.Text);

  if IsZero(vValue) then
    SetFieldValue(Null)
  else
    SetFieldValue(vValue);
end;

procedure TUniGUIFloatFieldEditor.FillEditor;
var
  vEdit: TUniEdit;
begin
  vEdit := FEdit;

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
  FEdit.OnChange := AHandler;
end;

{ TUniGUIDateFieldEditor }

procedure TUniGUIDateFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FDateTimePicker);
end;

function TUniGUIDateFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vDef: TSimpleFieldDef;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FDateTimePicker := TUniDateTimePicker.Create(TUniPanel(Result));
  FDateTimePicker.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FDateTimePicker.LayoutConfig.Width := '100%';
  FDateTimePicker.LayoutConfig.Height := '100%';
  FDateTimePicker.Kind := tUniDate;

  vDef := TSimpleFieldDef(FFieldDef);
  if not VarIsNull(vDef.MaxValue) then
    FDateTimePicker.MaxDate := vDef.MaxValue;
  if not VarIsNull(vDef.MinValue) then
    FDateTimePicker.MinDate := vDef.MinValue;
end;

procedure TUniGUIDateFieldEditor.DoOnChange;
begin
  SetFieldValue(FDateTimePicker.DateTime);
end;

procedure TUniGUIDateFieldEditor.FillEditor;
var
  vEdit: TUniDateTimePicker;
  vDate: TDateTime;
begin
  vEdit := FDateTimePicker;

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
  FDateTimePicker.OnChange := AHandler;
end;

{ TUniGUITimeFieldEditor }

procedure TUniGUITimeFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FDateTimePicker);
end;

function TUniGUITimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FDateTimePicker := TUniDateTimePicker.Create(TUniPanel(Result));
  FDateTimePicker.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FDateTimePicker.LayoutConfig.Width := '100%';
  FDateTimePicker.LayoutConfig.Height := '100%';

  TUniDateTimePicker(Result).Kind := tUniTime;
end;

procedure TUniGUITimeFieldEditor.DoOnChange;
begin
  SetFieldValue(FDateTimePicker.DateTime);
end;

procedure TUniGUITimeFieldEditor.FillEditor;
var
  vEdit: TUniDateTimePicker;
begin
  vEdit := FDateTimePicker;

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
  FDateTimePicker.OnChange := AHandler;
end;

{ TUniGUIDateTimeFieldEditor }

procedure TUniGUIDateTimeFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FDateTimePicker);
end;

function TUniGUIDateTimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vDef: TSimpleFieldDef;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FDateTimePicker := TUniDateTimePicker.Create(TUniPanel(Result));
  FDateTimePicker.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FDateTimePicker.LayoutConfig.Width := '100%';
  FDateTimePicker.LayoutConfig.Height := '100%';

  FDateTimePicker.Kind := tUniDateTime;

  vDef := TSimpleFieldDef(FFieldDef);
  if not VarIsNull(vDef.MaxValue) then
    FDateTimePicker.MaxDate := vDef.MaxValue;
  if not VarIsNull(vDef.MinValue) then
    FDateTimePicker.MinDate := vDef.MinValue;
end;

procedure TUniGUIDateTimeFieldEditor.DoOnChange;
begin
  SetFieldValue(FDateTimePicker.DateTime);
end;

procedure TUniGUIDateTimeFieldEditor.FillEditor;
var
  vEdit: TUniDateTimePicker;
begin
  vEdit := FDateTimePicker;

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
    FDateTimePicker.DateFormat := 'dd.MM.yyyy';
    FDateTimePicker.TimeFormat := 'HH:mm:ss';
  end;
end;

procedure TUniGUIDateTimeFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  FDateTimePicker.OnChange := AHandler;
end;

{ TUniGUIMemoFieldEditor }

procedure TUniGUIMemoFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FMemo);
end;

function TUniGUIMemoFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FMemo := TUniMemo.Create(TUniPanel(Result));
  FMemo.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FMemo.LayoutConfig.Width := '100%';
  FMemo.LayoutConfig.Height := '100%';
  FMemo.ScrollBars := ssVertical;
  FMemo.ParentFont := True;
  FMemo.ParentColor := True;
end;

procedure TUniGUIMemoFieldEditor.DoOnChange;
begin
  SetFieldValue(FMemo.Text);
end;

procedure TUniGUIMemoFieldEditor.FillEditor;
var
  vEdit: TUniMemo;
begin
  vEdit := FMemo;
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
  FMemo.OnChange := AHandler;
end;

{ TUniGUIBoolFieldEditor }

procedure TUniGUIBoolFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FCheckBox);
end;

function TUniGUIBoolFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FNeedCreateCaption := False;

  Result := TUniPanel.Create(ExtractOwner(AParent));
  FCheckBox := TUniCheckBox.Create(TUniPanel(Result));
  FCheckBox.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FCheckBox.LayoutConfig.Width := '100%';
  FCheckBox.LayoutConfig.Height := '100%';
  if Assigned(FCreateParams) and (FCreateParams.IndexOfName('Caption') >= 0) then
    FCheckBox.Caption := FCreateParams.Values['Caption']
  else
    FCheckBox.Caption := FOwner.GetFieldTranslation(FFieldDef);

  if Assigned(FCreateParams) and (FCreateParams.IndexOfName('Hint') >= 0) then
    FCheckBox.Hint := FCreateParams.Values['Hint']
  else
    FCheckBox.Hint := FOwner.GetFieldTranslation(FFieldDef, tpHint);
end;

procedure TUniGUIBoolFieldEditor.DoOnChange;
begin
  SetFieldValue(FCheckBox.Checked);
end;

procedure TUniGUIBoolFieldEditor.FillEditor;
begin
  if VarIsNull(FView.FieldValue) then
    FCheckBox.Checked := False
  else
    FCheckBox.Checked := FView.FieldValue;

  FCheckBox.Enabled := FView.State = vsFullAccess;
end;

procedure TUniGUIBoolFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  FCheckBox.OnClick := AHandler;
end;

{ TUniGUISpinner }

function TUniGUISpinner.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniHTMLFrame.Create(ExtractOwner(AParent));
  TUniHTMLFrame(Result).Width := ALayout.Width;
  TUniHTMLFrame(Result).Height := ALayout.Height;


  TUniHTMLFrame(Result).HTML.Text := '<html><head><script></script></head><body>' +
  '<span class="loader"></span></body></html>';
end;

procedure TUniGUISpinner.FillEditor;
begin
  inherited;

end;

{ TUniGUILogEditor }

procedure TUniGUILogEditor.DoBeforeFreeControl;
begin
  FreeAndNil(FData);
  FreeAndNil(FListView);
end;

function TUniGUILogEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FListView := TUniStringGrid.Create(TUniPanel(Result));
  FListView.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FListView.LayoutConfig.Width := '100%';
  FListView.LayoutConfig.Height := '100%';
  FListView.AutoSize := True;
  FListView.ReadOnly := True;
  FListView.DefaultColWidth := TUniPanel(Result).Width - 10;
end;

procedure TUniGUILogEditor.FillEditor;
begin
  inherited;
  FListView.Cells[0,0] := FView.FieldValue;
  FListView.RowCount := 1;
  FListView.ColCount := 1;
  FListView.Refresh;
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

{ TUniGUIFlagsEditor }

procedure TUniGUIFlagsEditor.CLBOnClickCheck(Sender: TObject);
begin
  FOwner.OnChange(Sender);
end;

procedure TUniGUIFlagsEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FListBox);
end;

function TUniGUIFlagsEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FListBox := TUniListBox.Create(TUniPanel(Result));
  FListBox.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FListBox.LayoutConfig.Width := '100%';
  FListBox.LayoutConfig.Height := '100%';
  FListBox.OnClick := CLBOnClickCheck;
  FListBox.MultiSelect := True;
  //TUniListBox(Result). := True;
end;

procedure TUniGUIFlagsEditor.DoOnChange;
var
  i: Integer;
  vFlagsValue: Integer;
begin
  vFlagsValue := 0;
  for i := 0 to FListBox.Items.Count - 1 do
    if FListBox.Selected[i] then
      vFlagsValue := vFlagsValue or TEnumItem(FListBox.Items.Objects[i]).ID;

  SetFieldValue(vFlagsValue);
end;

procedure TUniGUIFlagsEditor.FillEditor;
begin
  FillList;
  FListBox.Enabled := FView.State >= vsSelectOnly;
end;

procedure TUniGUIFlagsEditor.FillList;
var
  vList: TUniListBox;
  vEnumItem: TEnumItem;
  vValue: Integer;
  vIndex: Integer;
begin
  vList := FListBox;
  vValue := FView.FieldValue;
  vList.Items.BeginUpdate;
  try
    vList.Items.Clear;
    for vEnumItem in FEnum do
    begin
      if vEnumItem.ID > 0 then
      begin
        vIndex := vList.Items.AddObject(vEnumItem.DisplayText, vEnumItem);
        vList.Selected[vIndex] := (vEnumItem.ID and vValue) <> 0;
      end;
    end;
  finally
    vList.Items.EndUpdate;
  end;
end;

procedure TUniGUIFlagsEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
end;

{ TUniGUITextSelector }

procedure TUniGUITextSelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TUniGUITextSelector.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FComboBox);
end;

function TUniGUITextSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FComboBox := TUniComboBox.Create(TUniPanel(Result));
  FComboBox.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FComboBox.LayoutConfig.Width := '100%';
  FComboBox.LayoutConfig.Height := '100%';
  FComboBox.OnDropDown := CBOnInitPopup;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    FComboBox.MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TUniGUITextSelector.DoOnChange;
begin
  SetFieldValue(FComboBox.Text);
end;

procedure TUniGUITextSelector.FillEditor;
var
  vEdit: TUniComboBox;
begin
  FillList;

  vEdit := FComboBox;
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
  FComboBox.Items.BeginUpdate;
  FComboBox.Items.Clear;

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
          FComboBox.Items.Add(vCollection[i].FieldValues[vFieldName])
        else
          FComboBox.Items.Add(vCollection[i]['Name']);

      FComboBox.Text := FView.FieldValue;
    end;
  end;
  FComboBox.Items.EndUpdate;
end;

procedure TUniGUITextSelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  FComboBox.OnChange := AHandler;
end;

{ TUniGUIMaskFieldEditor }

procedure TUniGUIMaskFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEdit);
end;

function TUniGUIMaskFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vMask: string;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FEdit := TUniEdit.Create(TUniPanel(Result));
  FLabel := nil;
  FEdit.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FEdit.LayoutConfig.Width := '100%';
  FEdit.LayoutConfig.Height := '100%';
  FEdit.LayoutConfig.Padding := 'unset';
  vMask := '';
  if Assigned(FFieldDef) then
  begin
    if SameText('phone', FFieldDef.StyleName) then
      vMask := '!\+9 (999\) 000-00-00;1;_'
    else if SameText('INN', FFieldDef.StyleName) then
      vMask := '999999999999';
  end;
  FEdit.InputMask.Mask := vMask;
end;

procedure TUniGUIMaskFieldEditor.DoOnChange;
begin
  SetFieldValue(FEdit.Text);
end;

procedure TUniGUIMaskFieldEditor.FillEditor;
var
  vEdit: TUniEdit;
begin
  vEdit := FEdit;
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
      FLabel := TUniLabel.Create(TUniPanel(FControl));
      FLabel.OnClick := OnEditorClick;
      FLabel.LayoutConfig.Width := '100%';
      FLabel.LayoutConfig.Height := '100%';
      FLabel.LayoutConfig.Padding := 'unset';
      FLabel.Text := FView.FieldValue;
      FLabel.Transparent := True;
      FLabel.Parent := TUniPanel(FControl);
      FEdit.Parent := nil;
      FLabel.Cursor := crHandPoint;
      FLabel.OnClick := OnEditorClick;
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
    FEdit.JSInterface.JSAdd('document.getElementById("'
      + FLabel.JSId +'").onclick = () => {window.open("mailto:' + FView.FieldValue + '", "_blank")};')
  else if SameText('url', FFieldDef.StyleName) then
    FEdit.JSInterface.JSAdd('document.getElementById("'
      + FLabel.JSId +'").onclick = () => {window.open("' + FView.FieldValue + '", "_blank")};');
end;

procedure TUniGUIMaskFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  FEdit.OnChange := AHandler;
end;

{ TUniGUIImagedAction }

procedure TUniGUIImagedAction.DoBeforeFreeControl;
begin
  if Assigned(FActionView) then
  begin
    FActionView.RemoveListener(Self);
    FActionView := nil;
  end;
  FreeAndNil(FButton);
end;

function TUniGUIImagedAction.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vActionName: string;
  vImageSize: Integer;
begin
  if Assigned(FCreateParams) then
  begin
    vActionName := FCreateParams.Values['action'];
    vImageSize := StrToIntDef(FCreateParams.Values['ImageSize'], 16);
    FFalseImageIndex := FOwner.GetImageIndex(FCreateParams.Values['false']);
    FTrueImageIndex := FOwner.GetImageIndex(FCreateParams.Values['true']);
    FTrueHint := FCreateParams.Values['trueHint'];
    FFalseHint := FCreateParams.Values['falseHint'];
  end
  else begin
    vActionName := '';
    vImageSize := 16;
    FTrueImageIndex := -1;
    FFalseImageIndex := -1;
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
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FButton := TUniButton.Create(TUniPanel(Result));
  FButton.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FButton.LayoutConfig.Width := '100%';
  FButton.LayoutConfig.Height := '100%';
  FButton.Images := TUniNativeImageList(FUIBuilder.Images[vImageSize]);
  FButton.OnClick := OnButtonClick;

end;

procedure TUniGUIImagedAction.FillEditor;
begin
  if VarIsNull(FView.FieldValue) or (not FView.FieldValue) then
  begin
    FButton.ImageIndex := FFalseImageIndex;
    FButton.Hint := FTrueHint;
  end
  else
  begin
    FButton.ImageIndex := FTrueImageIndex;
    FButton.Hint := FFalseHint;
  end;

  FButton.Enabled := FView.State = vsFullAccess;
end;

procedure TUniGUIImagedAction.OnButtonClick(Sender: TObject);
begin
  if Assigned(FActionView) then
    FActionView.ExecuteAction(FOwner.Holder);
end;

procedure TUniGUIImagedAction.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  FButton.OnClick := AHandler;
end;

{ TUniGUIColorPicker }

function TUniGUIColorPicker.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniColorButton.Create(ExtractOwner(AParent));
end;

procedure TUniGUIColorPicker.DoOnChange;
begin
  SetFieldValue(TUniColorButton(FControl).Color);
end;

procedure TUniGUIColorPicker.FillEditor;
var
  vEdit: TUniColorButton;
begin
  vEdit := TUniColorButton(FControl);

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
end;

procedure TUniGUIColorPicker.SwitchChangeHandlers(const AHandler: TNotifyEvent);
var
  vEdit: TUniColorButton;
begin
//  TUniColorPalette(FControl).OnChange := AHandler;
  vEdit := TUniColorButton(FControl);
  vEdit.OnClick := AHandler;
end;

{ TUniGUIBoolImages }

procedure TUniGUIBoolImages.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FImage);
end;

function TUniGUIBoolImages.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FImage := TUniImage.Create(TUniPanel(Result));
  FImage.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FImage.LayoutConfig.Width := '100%';
  FImage.LayoutConfig.Height := '100%';
  FImage.Transparent := True;
end;

procedure TUniGUIBoolImages.FillEditor;
var
  vImageSize: Integer;
  vName: string;
  i: Integer;
  vStream: TStream;
begin
  inherited;
  i := FCreateParams.IndexOfName(FView.FieldValue);

  if i < 0 then
    Exit;

  vImageSize := 16;
  if FCreateParams.IndexOfName('ImageSize') >= 0 then
    vImageSize := StrToIntDef(FCreateParams.Values['ImageSize'], 16);

  vName := FCreateParams.Values[FView.FieldValue];

  vStream := TConfiguration(TInteractor(FView.Interactor).Configuration).Icons.IconByName(vName, vImageSize);
  if Assigned(vStream) then
  begin
    vStream.Position := 0;
    FImage.Picture.LoadFromStream(vStream);
    if FView.FieldValue then
      FImage.Hint := FCreateParams.Values['trueHint']
    else
      FImage.Hint := FCreateParams.Values['falseHint'];
    FImage.Parent.Invalidate;
  end;
end;

{ TUniGUIImageByString }

procedure TUniGUIImageByString.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FImage);
end;

function TUniGUIImageByString.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FImage := TUniImage.Create(TUniPanel(Result));
  FImage.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FImage.LayoutConfig.Width := '100%';
  FImage.LayoutConfig.Height := '100%';
  FImage.Transparent := True;
  FImage.Center := True;
end;

procedure TUniGUIImageByString.FillEditor;
var
  i, vImageSize: Integer;
  vName: string;
  vStream: TStream;
begin
  inherited;

  if Assigned(FCreateParams) then
    i := FCreateParams.IndexOfName(FView.FieldValue)
  else
    i := -1;

  if i < 0 then
    Exit;

  vName := FCreateParams.Values[FView.FieldValue];
  vImageSize := StrToIntDef(FCreateParams.Values['ImageSize'], 16);

  vStream := TConfiguration(TInteractor(FView.Interactor).Configuration).Icons.IconByName(vName, vImageSize);
  if Assigned(vStream) then
  begin
    vStream.Position := 0;
    FImage.Picture.LoadFromStream(vStream);
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
  TUniLabel(Result).Cursor := crHandPoint;
  TUniLabel(Result).Transparent := False;
  FInitialColor := TUniLabel(Result).Color;

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
    TUniLabel(FControl).Color := FSelectBackColor;
    TUniLabel(FControl).Font.Color := clWhite;
  end
  else begin
    TUniLabel(FControl).Color := clWhite;
    TUniLabel(FControl).Font.Color := FDefaultTextColor;
  end;
end;

{ TUniGUIProgress }

procedure TUniGUIProgress.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FProgress);
end;

function TUniGUIProgress.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FProgress := TUniProgressBar.Create(TUniPanel(Result));
  FProgress.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FProgress.LayoutConfig.Width := '100%';
  FProgress.LayoutConfig.Height := '100%';
  FProgress.Color := AlphaColorToColor(ALayout.Color);
  FNeedCreateCaption := False;
  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    FProgress.Max := TSimpleFieldDef(FFieldDef).MaxValue
  else
    FProgress.Max := 100;
end;

procedure TUniGUIProgress.DoOnChange;
begin
  inherited;
  FProgress.Position := FView.FieldValue;
end;

procedure TUniGUIProgress.FillEditor;
begin
  inherited;
  FProgress.Position := FView.FieldValue;
end;

{ TUniGUIPagesFieldEditor }

function TUniGUIPagesFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vHideTabs: Boolean;
  vTabSheet: TUniTabSheet;
  i: Integer;
begin
  FNeedCreateCaption := False;

  FPageControl := TUniPageControl.Create(ExtractOwner(AParent));
  FPageControl.DoubleBuffered := True;
  Result := FPageControl;

  FPageControl.Align := TAlign(ALayout.Align);
  CopyMargins(FPageControl, ALayout);
  FPageControl.Anchors := ALayout.Anchors;

  if (ALayout.Page_Style <> psTabs) and (ALayout.Items.Count > 0) then
  begin
    FPageControl.Left := ALayout.Left + ALayout.Items[0].Left;
    FPageControl.Top := ALayout.Top + ALayout.Items[0].Top;
  end
  else begin
    FPageControl.Left := ALayout.Left;
    FPageControl.Top := ALayout.Top;
  end;

  vHideTabs := not ALayout.ShowCaption or ((ALayout.Items.Count > 0) and not ALayout.Items[0].ShowCaption);
  if not vHideTabs then
  begin
    FPageControl.AutoSize := False;
    FPageControl.Height := ALayout.Page_Height;
  end;

  for i := 0 to ALayout.Items.Count - 1 do
  begin
    vTabSheet := TUniTabSheet(FPageControl);
    vTabSheet.Caption := ALayout.Caption;
    vTabSheet.ImageIndex := AParent.GetImageIndex(ALayout.ImageID);
    vTabSheet.TabVisible := ALayout.ShowCaption;
    //vTabSheet.PageIndex := ;
  end;
end;

procedure TUniGUIPagesFieldEditor.FillEditor;
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

  if (vTag < FPageControl.PageCount) and (FPageControl.ActivePageIndex <> vTag) then
    FPageControl.ActivePageIndex := vTag;
end;

procedure TUniGUIPagesFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  FPageControl.OnChange := AHandler;
end;

{ TUniGUIImageViewer }

procedure TUniGUIImageViewer.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FImage);
end;

function TUniGUIImageViewer.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FImage := TUniImage.Create(TUniPanel(Result));
  FImage.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FImage.LayoutConfig.Width := '100%';
  FImage.LayoutConfig.Height := '100%';
end;

procedure TUniGUIImageViewer.FillEditor;
var
  vPicture: TPicture;
  vStream: TStream;
begin
  vStream := FView.FieldStream;
  if vStream = nil then
  begin
    FImage.Picture := nil;
    Exit;
  end;

  vPicture := TPicture.Create;
  try
    vStream.Position := 0;
    vPicture.Graphic.LoadFromStream(vStream);
    FImage.Picture.Assign(vPicture);
  finally
    vPicture.Graphic := nil;
    vPicture.Free;
  end;

  FImage.Enabled := FView.State = vsFullAccess;
end;

{ TUniGUIGauge }

procedure TUniGUIGauge.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FSeries);
end;

function TUniGUIGauge.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniChart.Create(ExtractOwner(AParent));
  FSeries := TUniGaugeSeries.Create(TUniChart(Result));
  FSeries.Parent := TUniChart(Result);
  FSeries.Needle := True;
  FSeries.DefaultColors := True;
  FNeedCreateCaption := False;
  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    FSeries.Max := TSimpleFieldDef(FFieldDef).MaxValue
  else
    FSeries.Max := 100;
  TUniChart(Result).Legend.Visible := False;
  TUniChart(Result).BorderStyle := ubsNone;
  TUniChart(Result).BorderOutline := False;
  TUniChart(Result).SeriesList.Add(FSeries);
  TUniChart(Result).TitleVisible := False;
  TUniChart(Result).Animate := True;
end;

procedure TUniGUIGauge.FillEditor;
begin
  inherited;
  FSeries.Clear;
  FSeries.Add(FView.FieldValue, 'value');
end;

{ TUniGUIIntegerFlagsEditor }

procedure TUniGUIIntegerFlagsEditor.CLBOnClickCheck(Sender: TObject);
begin
  FOwner.OnChange(Sender);
end;

procedure TUniGUIIntegerFlagsEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FCaptions);
  FreeAndNil(FListBox);
end;

function TUniGUIIntegerFlagsEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FListBox := TUniListBox.Create(TUniPanel(Result));
  FListBox.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FListBox.LayoutConfig.Width := '100%';
  FListBox.LayoutConfig.Height := '100%';
  FListBox.OnClick := CLBOnClickCheck;
  FListBox.MultiSelect := True;
  FDisplayFlagCount := 8;
end;

procedure TUniGUIIntegerFlagsEditor.DoOnChange;
var
  i: Integer;
  vFlagsValue: Integer;
begin
  vFlagsValue := 0;
  for i := 0 to FListBox.Items.Count - 1 do
    if FListBox.Selected[i] then
      vFlagsValue := vFlagsValue or NativeInt(FListBox.Items.Objects[i]);

  SetFieldValue(vFlagsValue);
end;

procedure TUniGUIIntegerFlagsEditor.DoOnExit(Sender: TObject);
begin
  FListBox.ItemIndex := -1;
end;

procedure TUniGUIIntegerFlagsEditor.FillEditor;
begin
  FillList;
  FListBox.Enabled := FView.State >= vsSelectOnly;
end;

procedure TUniGUIIntegerFlagsEditor.FillList;
var
  vList: TUniListBox;
  vText: string;
  vBits, vBit: Integer;
  i: Integer;
begin
  vList := FListBox;
  vBits := FView.FieldValue;
  vList.Items.BeginUpdate;
  try
    vList.Items.Clear;
    for i := 0 to FDisplayFlagCount - 1 do
    begin
      if Assigned(FCaptions) and (i < FCaptions.Count) then
        vText := FCaptions[i]
      else
        vText := IntToStr(i);
      vBit := 1 shl i;
      vList.Items.AddObject(vText, TObject(vBit));
      vList.Selected[i] := (vBit and vBits) <> 0;
    end;
  finally
    vList.Items.EndUpdate;
  end;
end;

procedure TUniGUIIntegerFlagsEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
end;

{ TUniGUISelectFolderFieldEditor }

procedure TUniGUISelectFolderFieldEditor.BrowseForFolder1Accept(Sender: TObject; AStream: TFileStream);
begin
  FText.Text := FAction.FileName;
end;

procedure TUniGUISelectFolderFieldEditor.CLBBrowseForCFolder(Sender: TObject);
begin
  FAction.Execute;
end;

procedure TUniGUISelectFolderFieldEditor.DoBeforeFreeControl;
begin
  FreeAndNil(FAction);
  FreeAndNil(FText);
  FreeAndNil(FBtn);
end;

function TUniGUISelectFolderFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vBase: TUniPanel;
begin
  vBase := TUniPanel.Create(ExtractOwner(AParent));

  FAction := TUniFileUpload.Create(ExtractOwner(AParent));
  FAction.OnCompleted := BrowseForFolder1Accept;
//  FAction.B := BeforeExecute;
  FAction.Title := '';

  FBtn := TUniButton.Create(ExtractOwner(AParent));
  FBtn.Align := alRight;
  FBtn.Parent := vBase;
  FBtn.Width := 40;
  FBtn.Images := TUniNativeImageList(FUIBuilder.Images[16]);
  FBtn.ImageIndex := 22;
  FBtn.OnClick := CLBBrowseForCFolder;

  FText := TUniEdit.Create(ExtractOwner(AParent));
  FText.Align := alClient;
  FText.Parent := vBase;

  Result := vBase;
end;

procedure TUniGUISelectFolderFieldEditor.DoOnChange;
begin
  SetFieldValue(FText.Text);
end;

procedure TUniGUISelectFolderFieldEditor.FillEditor;
begin
  TUniPanel(FControl).Caption := ''; //reset caption after TFieldArea.Create (FControl.Name := FFieldDef.Name;)

  if VarIsNull(FView.FieldValue) then
  begin
    FText.Text := '';
    FText.Hint := '';
    FText.Enabled := False;
  end
  else begin
    FText.Text := FView.FieldValue;
    FText.Hint := FView.FieldValue;
    FText.Enabled := True;
    FText.ReadOnly := FView.State < vsFullAccess;
  end;

  FText.Visible := FView.State > vsSelectOnly;
end;

procedure TUniGUISelectFolderFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  FText.OnChange := AHandler;
end;

{ TUniGUIFileNameFieldEditor }

procedure TUniGUIFileNameFieldEditor.CLBBrowseForCFolder(Sender: TObject);
begin
  FAction.Execute;
end;

procedure TUniGUIFileNameFieldEditor.DoBeforeFreeControl;
begin
  FreeAndNil(FAction);
  FreeAndNil(FBtn);
  FreeAndNil(FText);
end;

function TUniGUIFileNameFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vBase: TUniPanel;
begin
  vBase := TUniPanel.Create(ExtractOwner(AParent));

  FAction := TUniFileUpload.Create(ExtractOwner(AParent));
  FAction.OnCompleted := OnAccept;
  FAction.Title := '';
  if Assigned(FCreateParams) then
    FAction.Filter := FCreateParams.Values['filter'];

  FBtn := TUniButton.Create(ExtractOwner(AParent));
  FBtn.Align := alRight;
  FBtn.Parent := vBase;
  FBtn.Width := 40;
  FBtn.Images := TUniNativeImageList(FUIBuilder.Images[16]);
  FBtn.ImageIndex := 23;
  FBtn.OnClick := CLBBrowseForCFolder;

  FText := TUniEdit.Create(ExtractOwner(AParent));
  FText.Align := alClient;
  FText.Parent := vBase;

  Result := vBase;
end;

procedure TUniGUIFileNameFieldEditor.DoOnChange;
begin
  SetFieldValue(FText.Text);
end;

procedure TUniGUIFileNameFieldEditor.FillEditor;
begin
  TUniPanel(FControl).Caption := ''; //reset caption after TFieldArea.Create (FControl.Name := FFieldDef.Name;)

  if VarIsNull(FView.FieldValue) then
  begin
    FText.Text := '';
    FText.Hint := '';
    FText.Enabled := False;
  end
  else begin
    FText.Text := FView.FieldValue;
    FText.Hint := FView.FieldValue;
    FText.Enabled := True;
    FText.ReadOnly := FView.State < vsSelectOnly;
  end;

  TUniPanel(FControl).Visible := FView.State >= vsSelectOnly;
end;

procedure TUniGUIFileNameFieldEditor.OnAccept(Sender: TObject; AStream: TFileStream);
begin
  FText.Text := FAction.FileName;
  FText.Hint := FText.Text;
end;

procedure TUniGUIFileNameFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  FText.OnChange := AHandler;
end;

{ TUniGUIGraphicEnumSelector }

procedure TUniGUIGraphicEnumSelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TUniGUIGraphicEnumSelector.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FComboBox);
end;

function TUniGUIGraphicEnumSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  if not Assigned(FEnum) then
    FEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);

  Result := TUniPanel.Create(ExtractOwner(AParent));
  FComboBox := TUniComboBox.Create(TUniPanel(Result));
  FComboBox.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FComboBox.LayoutConfig.Width := '100%';
  FComboBox.LayoutConfig.Height := '100%';
  FComboBox.Style := csOwnerDrawFixed;
  FComboBox.OnDropDown := CBOnInitPopup;
end;

procedure TUniGUIGraphicEnumSelector.DoDrawItem(const ACanvas: TUniCanvas; const AID: Integer; const ARect: TRect;
  AState: TOwnerDrawState);
begin
end;

procedure TUniGUIGraphicEnumSelector.DoOnChange;
begin
  if FFieldDef.HasFlag(cRequired) then
    SetFieldValue(TComboBox(FControl).ItemIndex + 1)
  else
    SetFieldValue(TComboBox(FControl).ItemIndex);
end;

procedure TUniGUIGraphicEnumSelector.FillEditor;
var
  vEdit: TUniComboBox;
  vItemIndex: Integer;
begin
  FillList;

  vEdit := FComboBox;
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Text := '';
    vEdit.ItemIndex := -1; //FEnum.Items[0].DisplayText;
    vEdit.Enabled := False;
  end
  else begin
    vItemIndex := FView.FieldValue;
    if FFieldDef.HasFlag(cRequired) then
      vItemIndex := vItemIndex - 1;
    vEdit.ItemIndex := vItemIndex;
    //if vItemIndex = 0 then
    //  vEdit.EditValue := FEnum.Items[0].DisplayText;

    vEdit.Enabled := FView.State >= vsSelectOnly;
  end;
end;

procedure TUniGUIGraphicEnumSelector.FillList;
var
  vEnumItem: TEnumItem;
  vItems: TStrings;
begin
  vItems := FComboBox.Items;

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

procedure TUniGUIGraphicEnumSelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TComboBox(FControl).OnChange := AHandler;
end;

{ TUniGUILineStyleSelector }

procedure TUniGUILineStyleSelector.DoDrawItem(const ACanvas: TUniCanvas; const AID: Integer; const ARect: TRect;
  AState: TOwnerDrawState);
begin
end;

{ TUniGUIEnumFieldEditor }

function TUniGUIEnumFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
  var
  vCollections: TList<TCollection>;
  vCollection: TCollection;
  vEntity: TEntity;
begin
  Result := TUniPanel.Create(ExtractOwner(AParent));
  FRadioGroup := TUniRadioGroup.Create(TUniPanel(Result));
  FRadioGroup.Parent := TUniPanel(Result);
  TUniPanel(Result).BorderStyle := ubsNone;
  FRadioGroup.LayoutConfig.Width := '100%';
  FRadioGroup.LayoutConfig.Height := '100%';

  vCollections := TDomain(FView.Domain).CollectionsByDefinition(
    TEntityFieldDef(FFieldDef)._ContentDefinition);
  try
    for vCollection in vCollections do
      for vEntity in vCollection do
        if not vEntity.IsNew then
          FRadioGroup.Items.AddObject(vEntity['Name'], vEntity);
  finally
    FreeAndNil(vCollections);
  end;

  FRadioGroup.Caption := '';
end;

procedure TUniGUIEnumFieldEditor.DoOnChange;
begin
  SetFieldEntity(TEntity(FRadioGroup.Items.Objects[FRadioGroup.ItemIndex]));
end;

procedure TUniGUIEnumFieldEditor.FillEditor;
var
  vIndex: Integer;
begin
  if Assigned(FView.FieldEntity) then
  begin
    vIndex := FRadioGroup.Items.IndexOfObject(FView.FieldEntity);
    FRadioGroup.ItemIndex := vIndex;
  end
  else
    FRadioGroup.ItemIndex := -1;

  FRadioGroup.Enabled := Assigned(FView.FieldEntity) and (FView.State = vsFullAccess);
end;

procedure TUniGUIEnumFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  FRadioGroup.OnClick := AHandler;
end;

initialization
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, '', TUniGUITextFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'phone', TUniGUITextFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'email', TUniGUIMaskFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'url', TUniGUIMaskFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'INN', TUniGUIMaskFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'info', TUniGUITextInfo);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'log', TUniGUILogEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'dir', TUniGUISelectFolderFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'file', TUniGUIFileNameFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'memo', TUniGUIMemoFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'ImageByString', TUniGUIImageByString);
  TPresenter.RegisterControlClass('Web.UniGUI', uiTextEdit, 'selector', TUniGUITextSelector);
  TPresenter.RegisterControlClass('Web.UniGUI', uiIntegerEdit, '', TUniGUIIntegerFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiIntegerEdit, 'simple', TUniGUIIntegerFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiIntegerEdit, 'info', TUniGUITextInfo);
  TPresenter.RegisterControlClass('Web.UniGUI', uiIntegerEdit, 'gauge', TUniGUIGauge);
  TPresenter.RegisterControlClass('Web.UniGUI', uiIntegerEdit, 'pages', TUniGUIPagesFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiIntegerEdit, 'spinner', TUniGUISpinner);
  TPresenter.RegisterControlClass('Web.UniGUI', uiIntegerEdit, 'progress', TUniGUIProgress);
  TPresenter.RegisterControlClass('Web.UniGUI', uiFloatEdit, '', TUniGUIFloatFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiFloatEdit, 'currency_rate', TUniGUIFloatFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiFloatEdit, 'info', TUniGUITextInfo);
  TPresenter.RegisterControlClass('Web.UniGUI', uiFloatEdit, 'gauge', TUniGUIGauge);
  TPresenter.RegisterControlClass('Web.UniGUI', uiCurrencyEdit, '', TUniGUIFloatFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiCurrencyEdit, 'info', TUniGUITextInfo);
  TPresenter.RegisterControlClass('Web.UniGUI', uiDateEdit, '', TUniGUIDateFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiDateEdit, 'time', TUniGUITimeFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiDateEdit, 'datetime', TUniGUIDateTimeFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiDateEdit, 'info', TUniGUITextInfo);
  TPresenter.RegisterControlClass('Web.UniGUI', uiBoolEdit, '', TUniGUIBoolFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiBoolEdit, 'simple', TUniGUIBoolFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiBoolEdit, 'imaged_action', TUniGUIImagedAction);
  TPresenter.RegisterControlClass('Web.UniGUI', uiEnumEdit, '', TUniGUIEnumEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiEnumEdit, 'radio', TUniGUIEnumEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiEnumEdit, 'info', TUniGUITextInfo);
  TPresenter.RegisterControlClass('Web.UniGUI', uiEnumEdit, 'line_style', TUniGUILineStyleSelector);
  TPresenter.RegisterControlClass('Web.UniGUI', uiEnumEdit, 'pages', TUniGUIPagesFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiBoolEdit, 'images', TUniGUIBoolImages);
  TPresenter.RegisterControlClass('Web.UniGUI', uiBoolEdit, 'selected_caption', TUniGUISelectedCaptionBoolFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiBoolEdit, 'pages', TUniGUIPagesFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiEntityEdit, 'info', TUniGUITextInfo);
  TPresenter.RegisterControlClass('Web.UniGUI', uiEntityEdit, 'pages', TUniGUIPagesFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiEntityEdit, 'enum', TUniGUIEnumFieldEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiBLOBEdit, 'image', TUniGUIImageViewer);
  TPresenter.RegisterControlClass('Web.UniGUI', uiFlagEdit, '', TUniGUIFlagsEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiIntegerEdit, 'flags', TUniGUIIntegerFlagsEditor);
  TPresenter.RegisterControlClass('Web.UniGUI', uiColorEdit, '', TUniGUIColorPicker);
  TPresenter.RegisterControlClass('Web.UniGUI', uiColorEdit, 'simple', TUniGUIColorPicker);
end.
