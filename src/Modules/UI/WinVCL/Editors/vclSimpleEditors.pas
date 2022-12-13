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

unit vclSimpleEditors;

interface

uses
  Classes, Types, Graphics, ExtCtrls, StdCtrls, Dialogs, ActnList, StdActns, ComCtrls,

  vclArea, uDefinition, uEnumeration, uUIBuilder, uView, uLayout, Controls,

  //DevExpress
  cxSpinEdit, cxEdit, cxCalendar, cxDateUtils, cxTextEdit, cxRadioGroup, cxPC,
  cxLabel, cxCurrencyEdit, cxCheckBox, cxListBox, cxDropDownEdit,
  cxTL, cxGraphics, cxControls, dxBevel, cxMemo, cxButtons,
  cxGroupBox, cxTimeEdit, cxLookAndFeelPainters,
  dxGaugeCustomScale, dxGaugeQuantitativeScale, dxGaugeCircularScale, dxGaugeControl;

type
  TTextInfo = class (TVCLFieldArea)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TDEEditor = class (TVCLFieldArea)
  protected
    function GetDisabledBorderStyle: TcxEditBorderStyle;
    function GetBoxDisabledBorderStyle: TcxContainerBorderStyle;

    procedure ToggleButtons;
  end;

  TDEEnumEditor = class(TDEEditor)
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

  TDEGraphicEnumSelector = class(TDEEditor)
  private
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
    procedure CBOnDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect;
      AState: TOwnerDrawState);
  protected
    FEnum: TEnumeration;

    procedure DoDrawItem(const ACanvas: TcxCanvas; const AID: Integer; const ARect: TRect;
      AState: TOwnerDrawState); virtual;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDELineStyleSelector = class(TDEGraphicEnumSelector)
  protected
    procedure DoDrawItem(const ACanvas: TcxCanvas; const AID: Integer; const ARect: TRect;
      AState: TOwnerDrawState); override;
  end;

  TDEFlagsEditor = class(TDEEditor)
  private
    FEnum: TEnumeration;
    procedure FillList;
    procedure CLBOnClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    function GetLayoutPositionCount: Integer; override;
  end;

  TIntegerFlagsEditor = class(TDEEditor)
  private
    FDisplayFlagCount: Integer;
    FCaptions: TStrings;
    procedure FillList;
    procedure CLBOnClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure DoOnExit(Sender: TObject); override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEIntegerFieldEditor = class (TDEEditor)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure FocusedChanged(const AFocused: Boolean); override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEFloatFieldEditor = class (TDEEditor)
  private
//    FAfterPoint: Integer; // знаков после запятой
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDECurrencyFieldEditor = class (TDEEditor)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEDateFieldEditor = class (TDEEditor)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    function GetNewValue: Variant; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDETimeFieldEditor = class (TDEEditor)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
//    function GetNewValue: Variant; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEDateTimeFieldEditor = class (TDEEditor)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    function GetNewValue: Variant; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDETextFieldEditor = class (TDEEditor)
  private
    procedure OnPhoneKeyPress(Sender: TObject; var Key: Char);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TTextSelector = class (TDEEditor)
  private
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TSelectFolderFieldEditor = class (TDEEditor)
  private
    FText: TcxTextEdit;
    FBtn: TcxButton;
    FAction: TBrowseForFolder;
    procedure BrowseForFolder1Accept(Sender: TObject);
    procedure BeforeExecute(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFilenameFieldEditor = class (TDEEditor)
  private
    FBtn: TcxButton;
    FAction: TFileOpen;
    FText: TcxTextEdit;
    procedure OnAccept(Sender: TObject);
    procedure BeforeExecute(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  public
    property TextEdit: TcxTextEdit read FText;
  end;

  TDEMaskFieldEditor = class (TDEEditor)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TMRUFieldEditor = class (TDEEditor)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEMemoFieldEditor = class (TDEEditor)
  private
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    function GetLayoutPositionCount: Integer; override;
  end;

  TDELogFieldEditor = class (TDEMemoFieldEditor)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
  end;

  TDEBoolFieldEditor = class(TDEEditor)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TSelectedCaptionBoolFieldEditor = class(TDEEditor)
  private
    FSelected: Boolean;
    FSelectBackColor, FDefaultTextColor: TColor;
    procedure OnClick(Sender: TObject);
    procedure UpdateView;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TDEImagedAction = class(TDEEditor)
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
  public
    destructor Destroy; override;
  end;

  TDEPagesFieldEditor = class(TDEEditor)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEEnumFieldEditor = class (TDEEditor)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEImageEditor = class (TDEEditor)
  private
    procedure DoOnAssignPicture(Sender: TObject;
      const Picture: TPicture);
    procedure DoOnChangeImage(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    function GetLayoutPositionCount: Integer; override;
  end;

  TDEBLOBEditor = class(TDEEditor)
  private
    procedure DoOnChangeBLOB(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TImageByString = class (TVCLFieldArea)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TBoolImages = class (TVCLFieldArea)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TDEColorEditor = class(TDEEditor)
  private
    //FBasePanel: TPanel;
    //FColorDialog: TColorDialog;
    //FSelectBtn, FClearBtn: TButton;
    //FPaintBox: TPaintBox;
    //procedure OnSelectBtnClick(Sender: TObject);
    //procedure OnClearBtnClick(Sender: TObject);
    //procedure OnPaint(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;

    //function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    //procedure DoBeforeFreeControl; override;
    //procedure FillEditor; override;
  end;

  TColorEditor = class (TVCLFieldArea)
  private
    FBasePanel: TPanel;
    FColorDialog: TColorDialog;
    FSelectBtn, FClearBtn: TButton;
    FPaintBox: TPaintBox;
    procedure OnSelectBtnClick(Sender: TObject);
    procedure OnClearBtnClick(Sender: TObject);
    procedure OnPaint(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;

  // неопределённый по времени процесс
  TSpinner = class (TVCLFieldArea)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure AssignFromLayout(const ALayout: TLayout; const AParams: string); override;
  end;

  // определённый по времени процесс
  TProgress = class (TVCLFieldArea)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TGauge = class (TVCLFieldArea)
  private
    FGaugeControl: TdxGaugeControl;
    FGaugeControl1CircularHalfScale: TdxGaugeCircularHalfScale;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;

  TEntityBreadcrumb = class (TVCLFieldArea)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SetValidateDefinition(const ADefinition: TDefinition);
  end;

  TLogEditor = class(TVCLFieldArea)
  private
    FListView: TListView;
    FData: TStringList;
    procedure OnListViewData(Sender: TObject; Item: TListItem);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure SetParent(const Value: TUIArea); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;

implementation

uses
  Generics.Collections, TypInfo, Variants, SysUtils, Windows,
  Forms, Math, DateUtils, Messages, cxBlobEdit, cxImage, dxGDIPlusClasses, cxMaskEdit, cxMRUEdit,
  dxActivityIndicator, cxLookAndFeels, cxProgressBar, dxBreadcrumbEdit, cxCustomListBox, cxCheckListBox,
  dxColorEdit, dxColorGallery, dxCoreGraphics, {CPort,}

  uConfiguration, uDomain, uInteractor, uPresenter, uCollection, uEntity, uConsts, uUtils, UITypes;

{ TTextInfo }

function TTextInfo.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TcxLabel.Create(nil);
  TcxLabel(Result).Transparent := True;
  TcxLabel(Result).Properties.WordWrap := True;
  if Assigned(FCreateParams) and (FCreateParams.Values['WordWrap'] = 'False') then
  begin
    TcxLabel(Result).Properties.WordWrap := False;
    TcxLabel(Result).Properties.ShowEndEllipsis := True;
  end;
  TcxLabel(Result).AutoSize := False;
end;

procedure TTextInfo.FillEditor;
var
  vEnum: TEnumeration;
  vEntity: TEntity;
  vColorField: TBaseField;
  vValue: Variant;
begin
  inherited;
  if VarIsNull(FView.FieldValue) then
    TcxLabel(FControl).Caption := ''
  else if FFieldDef.Kind = fkCurrency then
  begin
    if FFieldDef.Format <> '' then
      TcxLabel(FControl).Caption := FormatFloat(GetFormat, FView.FieldValue)
    else
      TcxLabel(FControl).Caption := FormatFloat('#,##0.00;;0', FView.FieldValue);
  end
  else if FFieldDef.Kind = fkFloat then
  begin
    if FFieldDef.Format <> '' then
      TcxLabel(FControl).Caption := FormatFloat(GetFormat, FView.FieldValue)
    else
      TcxLabel(FControl).Caption := FView.FieldValue;
  end
  else if FFieldDef.Kind = fkDateTime then
  begin
    vValue := FView.FieldValue;
    if IsZero(vValue, 1e-6) then
      TcxLabel(FControl).Caption := ''
    else if FFieldDef.Format <> '' then
      TcxLabel(FControl).Caption := FormatDateTime(GetFormat, vValue)
    else
      TcxLabel(FControl).Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss', vValue);
  end
  else if FFieldDef.Kind = fkEnum then
  begin
    vEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
    if not Assigned(vEnum) then
    begin
      vEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
      TcxLabel(FControl).Style.Font.Color := TState(vEnum.Items[Integer(FView.FieldValue)]).Color;
    end;
    TcxLabel(FControl).Caption := vEnum.Items[Integer(FView.FieldValue)].DisplayText;
  end
  else if FFieldDef.Kind = fkObject then
  begin
    vEntity := TEntity(FView.FieldEntity);
    if not Assigned(vEntity) then
      TcxLabel(FControl).Caption := TObjectFieldDef(FView.Definition)._ContentDefinition._EmptyValue
    else begin
      TcxLabel(FControl).Caption := vEntity['Name'];
      if vEntity.Definition.ColorFieldName <> '' then
      begin
        vColorField := vEntity.FieldByName(vEntity.Definition.ColorFieldName);
        TcxLabel(FControl).Style.Font.Color := TColor(vColorField.Value);
      end;
    end;
  end
  else
    TcxLabel(FControl).Caption := FView.FieldValue;
end;

{ TDEIntegerEditControl }

function TDEIntegerFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TcxSpinEdit.Create(nil);

  with TcxSpinEdit(Result).Properties do
  begin
    ImmediatePost := True;
    UseNullString := True;
    ValueType := vtInt;
  end;
//  TcxSpinEdit(FInnerControl).OnKeyDown := OnWinControlKeyDown;

  with TcxSpinEdit(Result).Properties do
  begin
    if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    begin
      MaxValue := TSimpleFieldDef(FFieldDef).MaxValue;
      AssignedValues.MaxValue := True;
    end;

    if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
    begin
      MinValue := TSimpleFieldDef(FFieldDef).MinValue;
      AssignedValues.MinValue := True;
    end;

    if (Length(FFieldDef.Format) > 0) or FFieldDef.Definition.FieldExists('Format') then
      DisplayFormat := GetFormat;
  end;
end;

procedure TDEIntegerFieldEditor.DoOnChange;
begin
  if TcxSpinEdit(FControl).EditingValue = 0 then
    SetFieldValue(Null)
  else
    SetFieldValue(TcxSpinEdit(FControl).EditingValue);
end;

procedure TDEIntegerFieldEditor.FillEditor;
var
  vEdit: TcxSpinEdit;
begin
  vEdit := TcxSpinEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.EditValue := 0;
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.EditValue := FView.FieldValue;
    vEdit.Properties.ReadOnly := FView.State < vsFullAccess;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.Style.ButtonTransparency := ebtAlways;
      vEdit.Style.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else begin
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.Style.ButtonTransparency := ebtNone;
      vEdit.Style.Color := clWindow;
      vEdit.TabStop := FTabStop;
    end;
  end;

  vEdit.Properties.SpinButtons.Visible := vEdit.Visible and vEdit.Enabled and (not vEdit.Properties.ReadOnly);
end;

procedure TDEIntegerFieldEditor.FocusedChanged(const AFocused: Boolean);
begin
  inherited;
  if not AFocused then
  begin
    // обрабатываем 0
    TcxSpinEdit(FControl).PostEditValue;
    SetFieldValue(TcxSpinEdit(FControl).EditingValue);
  end;
end;

procedure TDEIntegerFieldEditor.SwitchChangeHandlers(
  const AHandler: TNotifyEvent);
begin
  inherited;
  TcxSpinEdit(FControl).Properties.OnChange := AHandler
end;

{ TDEFloatEditControl }

function TDEFloatFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  {Result := TcxMaskEdit.Create(nil);
  FAfterPoint := 4;
  if Length(FFieldDef.Format) > 0 then
    FAfterPoint := Length(FFieldDef.Format) - Pos('.', FFieldDef.Format);

  with TcxMaskEdit(Result).Properties do
  begin
    ImmediatePost := True;
    MaskKind := emkRegExpr;
    EditMask := '-?\d+[' + FormatSettings.DecimalSeparator + '.,]?\d+';
    ValidationOptions := [evoAllowLoseFocus];
  end;

  with TcxMaskEdit(Result).Properties do
  begin
    if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
      MaxValue := TSimpleFieldDef(FFieldDef).MaxValue;
    if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
      MinValue := TSimpleFieldDef(FFieldDef).MinValue;

    if (Length(FFieldDef.Format) > 0) or FFieldDef.Definition.FieldExists('Format') then
      DisplayFormat := GetFormat;
  end; }
  Result := TcxSpinEdit.Create(nil);

  with TcxSpinEdit(Result).Properties do
  begin
    ImmediatePost := True;
    UseNullString := True;
    ValueType := vtFloat;
  end;

  with TcxSpinEdit(Result).Properties do
  begin
    if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    begin
      MaxValue := TSimpleFieldDef(FFieldDef).MaxValue;
      AssignedValues.MaxValue := True;
    end;

    if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
    begin
      MinValue := TSimpleFieldDef(FFieldDef).MinValue;
      AssignedValues.MinValue := True;
    end;

    if (Length(FFieldDef.Format) > 0) or FFieldDef.Definition.FieldExists('Format') then
      DisplayFormat := GetFormat;
  end;
end;

procedure TDEFloatFieldEditor.DoOnChange;
{var
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
  end;  }
begin
  {if TcxMaskEdit(FControl).EditingValue = '' then
    vValue := 0
  else
    vValue := ToFloatDef(TcxMaskEdit(FControl).EditingValue);

  if (not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue)) and (vValue < TcxMaskEdit(FControl).Properties.MinValue) then
    vValue := TcxMaskEdit(FControl).Properties.MinValue;
  if (not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue)) and (vValue > TcxMaskEdit(FControl).Properties.MaxValue) then
    vValue := TcxMaskEdit(FControl).Properties.MaxValue;
  SetFieldValue(vValue);  }
  if TcxSpinEdit(FControl).EditingValue = 0 then
    SetFieldValue(Null)
  else
    SetFieldValue(TcxSpinEdit(FControl).EditingValue);
end;

procedure TDEFloatFieldEditor.FillEditor;
var
  vEdit: TcxSpinEdit;
begin
  vEdit := TcxSpinEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.EditValue := 0;
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.EditValue := FView.FieldValue;
    vEdit.Properties.ReadOnly := FView.State < vsFullAccess;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.Style.ButtonTransparency := ebtAlways;
      vEdit.Style.Color := clBtnFace;
      vEdit.TabStop := False;
      vEdit.Properties.SpinButtons.Visible := False;
    end
    else begin
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.Style.ButtonTransparency := ebtAlways;
      vEdit.Style.Color := clWindow;
      vEdit.TabStop := FTabStop;
      vEdit.Properties.SpinButtons.Visible := False; // пока скрываем, чтобы не переделывать лэйауты
    end;
  end;
end;

procedure TDEFloatFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TcxSpinEdit(FControl).Properties.OnChange := AHandler
end;

{ TDEDateEditControl }

function TDEDateFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vDef: TSimpleFieldDef;
begin
  Result := TcxDateEdit.Create(nil);

  with TcxDateEdit(Result) do
  begin
    Properties.ImmediatePost := True;
    Properties.DateButtons := [btnToday, btnClear];
    Properties.SaveTime := False;
    Properties.ShowTime := False;
    Properties.DateOnError := deNull;
  end;
//  TcxDateEdit(FInnerControl).OnKeyDown := OnWinControlKeyDown;

  vDef := TSimpleFieldDef(FFieldDef);
  with TcxDateEdit(Result) do
  begin
    Properties.ImmediatePost := True;
    if not VarIsNull(vDef.MaxValue) then
      Properties.MaxDate := vDef.MaxValue;
    if not VarIsNull(vDef.MinValue) then
      Properties.MinDate := vDef.MinValue;
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

procedure TDEDateFieldEditor.DoOnChange;
var
  vDate: TDateTime;
begin
  if MyTryStrToDate(TcxDateEdit(FControl).EditingText, vDate) then
    SetFieldValue(vDate)
  else
    SetFieldValue(Null);
end;

procedure TDEDateFieldEditor.FillEditor;
var
  vEdit: TcxDateEdit;
  vDate: TDateTime;
begin
  vEdit := TcxDateEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Properties.InputKind := ikStandard;
    vEdit.Enabled := False;
    vEdit.Date := NullDate;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.Enabled := FView.State > vsDisabled;
    vDate := FView.FieldValue;
    if vDate < 2 then
      vEdit.Date := NullDate
    else
      vEdit.Date := vDate;

    vEdit.Properties.ReadOnly := FView.State < vsFullAccess;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Properties.InputKind := ikStandard;
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.Style.ButtonTransparency := ebtAlways;
      vEdit.Style.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else begin
      vEdit.Properties.InputKind := ikMask;
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.Style.ButtonTransparency := ebtNone;
      vEdit.Style.Color := clWindow;
      vEdit.TabStop := FTabStop;
    end;
  end;

  ToggleButtons;
end;

function TDEDateFieldEditor.GetNewValue: Variant;
var
  vDate: TDateTime;
begin
  if MyTryStrToDate(TcxDateEdit(FControl).EditingText, vDate) then
    Result := vDate
  else
    Result := Null;
end;

procedure TDEDateFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TcxDateEdit(FControl).Properties.OnChange := AHandler;
end;

{ TDETextEditControl }

function TDETextFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  inherited;
  Result := TcxTextEdit.Create(nil);
  TcxTextEdit(Result).Properties.BeepOnError := True;
  TcxTextEdit(Result).ParentColor := True;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TcxTextEdit(Result).Properties.MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;

  if SameText('phone', FFieldDef.StyleName) then
    TcxTextEdit(Result).OnKeyPress := OnPhoneKeyPress
  else
    TcxTextEdit(Result).OnKeyPress := nil;
end;

procedure TDETextFieldEditor.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TcxTextEdit(FControl).EditingText);
end;

procedure TDETextFieldEditor.OnPhoneKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, ['0'..'9', #8, '-', '+', '(', ')', ' ']) then Exit;

  Key := #0;
end;

procedure TDETextFieldEditor.FillEditor;
var
  vEdit: TcxTextEdit;
begin
  vEdit := TcxTextEdit(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.EditValue := '';
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
    vEdit.Style.Color := clBtnFace;
  end
  else
  begin
    vEdit.EditValue := FView.FieldValue;
    vEdit.Enabled := True;
    vEdit.Properties.ReadOnly := FView.State < vsFullAccess;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.TabStop := False;
      vEdit.Style.Color := clBtnFace;
    end
    else begin
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.TabStop := FTabStop;
      vEdit.Style.Color := clWindow;
    end;
  end;
end;

procedure TDETextFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TcxTextEdit(FControl).Properties) then
    TcxTextEdit(FControl).Properties.OnChange := AHandler;
end;

{ TDEEnumEditControl }

function TDEEnumFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vRadioItem: TcxRadioGroupItem;
  vCollections: TList<TCollection>;
  vCollection: TCollection;
  vEntity: TEntity;
begin
  Result := TcxRadioGroup.Create(nil);
  TcxRadioGroup(Result).Style.BorderStyle := GetDisabledBorderStyle;
//  TcxRadioGroup(Result).OnKeyDown := OnWinControlKeyDown;

  vCollections := TDomain(FView.Domain).CollectionsByDefinition(
    TEntityFieldDef(FFieldDef)._ContentDefinition);
  try
    for vCollection in vCollections do
    begin
      for vEntity in vCollection do
        if not vEntity.IsNew then
        begin
          vRadioItem := TcxRadioGroup(Result).Properties.Items.Add;
          vRadioItem.Caption := vEntity['Name'];
          vRadioItem.Value := Integer(vEntity);
        end;
    end;
  finally
    FreeAndNil(vCollections);
  end;

  TcxRadioGroup(Result).Caption := '';
end;

procedure TDEEnumFieldEditor.DoOnChange;
begin
  SetFieldEntity(TEntity(Integer(TcxRadioGroup(FControl).EditingValue)));
end;

procedure TDEEnumFieldEditor.FillEditor;
begin
  if Assigned(FView.FieldEntity) then
    TcxRadioGroup(FControl).EditValue := Integer(FView.FieldValue)
  else
    TcxRadioGroup(FControl).EditValue := -1;

  TcxRadioGroup(FControl).Enabled := Assigned(FView.FieldEntity) and (FView.State = vsFullAccess);
end;

procedure TDEEnumFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TcxRadioGroup(FControl).Properties.OnChange := AHandler;
end;

{ TDECurrencyEditControl }

function TDECurrencyFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vDef: TSimpleFieldDef;
begin
  Result := TcxCurrencyEdit.Create(nil);

  with TcxCurrencyEdit(Result).Properties do
  begin
    ReadOnly := FView.State < vsFullAccess;
    DisplayFormat := GetFormat;//',0.00;-,0.00';
    MaxLength := 15;
  end;
//  TcxCurrencyEdit(FInnerControl).OnKeyDown := OnWinControlKeyDown;

  vDef := TSimpleFieldDef(FFieldDef);
  with TcxCurrencyEdit(Result).Properties do
  begin
    if not VarIsNull(vDef.MaxValue) then MaxValue := vDef.MaxValue;
    if not VarIsNull(vDef.MinValue) then MinValue := vDef.MinValue;
  end;
end;

procedure TDECurrencyFieldEditor.DoOnChange;
begin
  SetFieldValue(TcxCurrencyEdit(FControl).EditingValue);
end;

procedure TDECurrencyFieldEditor.FillEditor;
var
  vEdit: TcxCurrencyEdit;
begin
  vEdit := TcxCurrencyEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.EditValue := 0;
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.EditValue := FView.FieldValue;
    vEdit.Properties.ReadOnly := FView.State < vsFullAccess;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.Style.ButtonTransparency := ebtAlways;
      vEdit.Style.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else begin
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.Style.ButtonTransparency := ebtNone;
      vEdit.Style.Color := clWindow;
      vEdit.TabStop := FTabStop;
    end;
  end;
end;

procedure TDECurrencyFieldEditor.SwitchChangeHandlers(
  const AHandler: TNotifyEvent);
begin
  inherited;
  TcxCurrencyEdit(FControl).Properties.OnChange := AHandler;
end;

{ TDEBoolEditControl }

function TDEBoolFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FNeedCreateCaption := False;

  Result := TcxCheckBox.Create(nil);
  TcxCheckBox(Result).Properties.FullFocusRect := True;
  TcxCheckBox(Result).Transparent := True;

  if ALayout.Control is TPanel then
  begin
    if TPanel(ALayout.Control).Alignment <> taCenter then
    begin
      TcxCheckBox(Result).AutoSize := False;
      if TPanel(ALayout.Control).Alignment = taLeftJustify then
        TcxCheckBox(Result).Properties.Alignment := taCenter
      else
        TcxCheckBox(Result).Properties.Alignment := TPanel(ALayout.Control).Alignment;
    end;
  end;

//  TcxCheckBox(Result).OnKeyDown := OnWinControlKeyDown;

  if Assigned(CreateParams) and (CreateParams.IndexOfName('Caption') >= 0) then
    TcxCheckBox(Result).Caption := CreateParams.Values['Caption']
  else
    TcxCheckBox(Result).Caption := GetFieldTranslation(FFieldDef);

  if Assigned(CreateParams) and (CreateParams.IndexOfName('Hint') >= 0) then
    TcxCheckBox(Result).Hint := CreateParams.Values['Hint']
  else
    TcxCheckBox(Result).Hint := GetFieldTranslation(FFieldDef, tpHint);
end;

procedure TDEBoolFieldEditor.DoOnChange;
begin
  SetFieldValue(TcxCheckBox(FControl).Checked);
end;

procedure TDEBoolFieldEditor.FillEditor;
begin
  if VarIsNull(FView.FieldValue) then
    TcxCheckBox(FControl).Checked := False
  else
    TcxCheckBox(FControl).Checked := FView.FieldValue;

  TcxCheckBox(FControl).Enabled := FView.State = vsFullAccess;
  TcxCheckBox(FControl).Properties.ReadOnly := FView.State < vsFullAccess;
  if TcxCheckBox(FControl).Enabled then
    TcxCheckBox(FControl).TabStop := FTabStop
  else
    TcxCheckBox(FControl).TabStop := False;
end;

procedure TDEBoolFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TcxCheckBox(FControl).Properties.OnChange := AHandler;
end;

{ TDEImageEditor }

procedure TDEImageEditor.DoBeforeFreeControl;
begin
  inherited;
end;

function TDEImageEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TcxImage.Create(nil);
  TcxImage(Result).Properties.GraphicClassName := 'TdxSmartImage';
  TcxImage(Result).Properties.OnAssignPicture := DoOnAssignPicture;
  TcxImage(Result).Properties.OnChange := DoOnChangeImage;
  if Assigned(FCreateParams) and (FCreateParams.Values['ViewState'] = 'ReadOnly') then
  begin
    TcxImage(Result).Properties.ReadOnly := True;
    TcxImage(Result).Properties.PopupMenuLayout.MenuItems := [];
    TcxImage(Result).Properties.ShowFocusRect := False;
    TcxImage(Result).Style.BorderStyle := ebsNone;
    TcxImage(Result).Transparent := True;
    TcxImage(Result).ParentColor := True;
  end;
end;

procedure TDEImageEditor.DoOnAssignPicture(Sender: TObject;
  const Picture: TPicture);
var
  vStream: TStream;
begin
  if Assigned(Picture.Graphic) then
  begin
    vStream := TMemoryStream.Create;
    Picture.Graphic.SaveToStream(vStream);
    SetFieldStream(vStream);
  end
  else
    SetFieldStream(nil);
end;

procedure TDEImageEditor.DoOnChangeImage(Sender: TObject);
begin
  TcxImage(FControl).PostEditValue;
end;

procedure TDEImageEditor.FillEditor;
var
  vPicture: TPicture;
  vStr: AnsiString;
  vStream: TStream;
  vImage: TdxSmartImage;
begin
  inherited;
  vStream := FView.FieldStream;
  if vStream = nil then
  begin
    TcxImage(FControl).EditValue := '';
    Exit;
  end;

  vPicture := TPicture.Create;
  vImage := TdxSmartImage.Create;
  vPicture.Graphic := vImage;
  try
    vStream.Position := 0;
    vPicture.Graphic.LoadFromStream(vStream);
    SavePicture(vPicture, vStr);
    TcxImage(FControl).EditValue := vStr;
  finally
    vPicture.Graphic := nil;
    FreeAndNil(vImage);
    vPicture.Free;
  end;

  TcxImage(FControl).Enabled := FView.State = vsFullAccess;
end;

function TDEImageEditor.GetLayoutPositionCount: Integer;
begin
  Result := 2;
end;

{ TDEMemoFieldEditor }

function TDEMemoFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TcxMemo.Create(nil);
  TcxMemo(Result).Properties.ScrollBars := ssVertical;
  TcxMemo(Result).Properties.WantReturns := True;
  TcxMemo(Result).ParentFont := True;
  TcxMemo(Result).ParentColor := True;
  TcxMemo(Result).OnKeyDown := OnKeyDown;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TcxMemo(Result).Properties.MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TDEMemoFieldEditor.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TcxMemo(FControl).EditingText);
end;

procedure TDEMemoFieldEditor.FillEditor;
var
  vEdit: TcxMemo;
begin
  vEdit := TcxMemo(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.EditValue := '';
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.EditValue := FView.FieldValue;
    vEdit.SelStart := 1000;
    vEdit.Properties.ReadOnly := FView.State < vsFullAccess;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.Style.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else begin
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.Style.Color := clWindow;
      vEdit.TabStop := FTabStop;
    end;
  end;
end;

function TDEMemoFieldEditor.GetLayoutPositionCount: Integer;
begin
  Result := 2;
end;

procedure TDEMemoFieldEditor.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

end;

procedure TDEMemoFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TcxMemo(FControl).Properties) then
    TcxMemo(FControl).Properties.OnChange := AHandler;
end;

{ TDETimeFieldEditor }

function TDETimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vDef: TSimpleFieldDef;
begin
  Result := TcxTimeEdit.Create(nil);

  vDef := TSimpleFieldDef(FFieldDef);
  with TcxTimeEdit(Result) do
  begin
    Properties.ImmediatePost := True;
    if not VarIsNull(vDef.MaxValue) then
      Properties.MaxValue := vDef.MaxValue;
    if not VarIsNull(vDef.MinValue) then
      Properties.MinValue := vDef.MinValue;
  end;
end;

procedure TDETimeFieldEditor.DoOnChange;
var
  vTime: TTime;
begin
  if MyTryStrToTime(TcxTimeEdit(FControl).EditingText, vTime) then
    SetFieldValue(Frac(vTime))
  else
    SetFieldValue(Null);
end;

procedure TDETimeFieldEditor.FillEditor;
var
  vEdit: TcxTimeEdit;
begin
  vEdit := TcxTimeEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.Time := NullDate;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.Enabled := FView.State > vsDisabled;
    vEdit.Time := FView.FieldValue;

    vEdit.Properties.ReadOnly := FView.State < vsFullAccess;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.Style.ButtonTransparency := ebtAlways;
      vEdit.Style.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else begin
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.Style.ButtonTransparency := ebtNone;
      vEdit.Style.Color := clWindow;
      vEdit.TabStop := FTabStop;
    end;
  end;

  vEdit.Properties.SpinButtons.Visible := vEdit.Visible and vEdit.Enabled and (not vEdit.Properties.ReadOnly);
end;

//function TDETimeFieldEditor.GetNewValue: Variant;
//var
//  vTime: TTime;
//begin
//  if MyTryStrToTime(TcxTimeEdit(FControl).EditingText, vTime) then
//    Result := Frac(vTime)
//  else
//    Result := Null;
//end;

procedure TDETimeFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TcxTimeEdit(FControl).Properties.OnChange := AHandler;
end;

{ TDEMaskFieldEditor }

function TDEMaskFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vMask: string;
begin
  Result := TcxMaskEdit.Create(nil);
//  TcxMaskEdit(FControl).OnKeyDown := OnWinControlKeyDown;
  TcxMaskEdit(Result).Properties.ValidationOptions := [evoShowErrorIcon, evoAllowLoseFocus];

  vMask := '';
  if Assigned(FFieldDef)then
  begin
    TcxMaskEdit(Result).Properties.MaskKind := emkStandard;
    if SameText('phone', FFieldDef.StyleName) then
      vMask := '!\+9 (999\) 000-00-00;1;_'
    else if SameText('INN', FFieldDef.StyleName) then
      vMask := '000000000009;0;_'
    else if SameText('email', FFieldDef.StyleName) then
    begin
      TcxMaskEdit(Result).Properties.MaskKind := emkRegExpr;
      vMask := '[\w\-.]+@[\w\-]+(\.[\w\-]+)+ ';
    end
    else if SameText('url', FFieldDef.StyleName) then
    begin
      TcxMaskEdit(Result).Properties.MaskKind := emkRegExprEx;
      vMask := 'http\:\/\/(\w+(\.\w+)*@)?\w+\.\w+(\.\w+)*(/(\w+(/\w+)*/?)?)?';
    end;
  end;
  TcxMaskEdit(Result).Properties.EditMask := vMask;
end;

procedure TDEMaskFieldEditor.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TcxMaskEdit(FControl).EditingText);
end;

procedure TDEMaskFieldEditor.FillEditor;
var
  vEdit: TcxMaskEdit;
begin
  vEdit := TcxMaskEdit(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.EditValue := '';
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.EditValue := FView.FieldValue;
    vEdit.Enabled := True;
    vEdit.Properties.ReadOnly := FView.State < vsFullAccess;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.Style.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else begin
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.Style.Color := clWindow;
      vEdit.TabStop := FTabStop;
    end;
  end;
end;

procedure TDEMaskFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TcxMaskEdit(FControl).Properties) then
    TcxMaskEdit(FControl).Properties.OnChange := AHandler;
end;

{ TMRUFieldEditor }

function TMRUFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TcxMRUEdit.Create(nil);
  TcxMRUEdit(Result).Properties.ShowEllipsis := False;
end;

procedure TMRUFieldEditor.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TcxMRUEdit(FControl).EditingText);
end;

procedure TMRUFieldEditor.FillEditor;
var
  vEdit: TcxMRUEdit;
  vCollections: TList<TCollection>;
  vCollection: TCollection;
  vEntity: TEntity;
  vText: string;
begin
  vEdit := TcxMRUEdit(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.EditValue := '';
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.EditValue := FView.FieldValue;
    vEdit.Enabled := True;
    vEdit.Properties.ReadOnly := FView.State < vsFullAccess;

    if not vEdit.Properties.ReadOnly then
    begin
      vEdit.Properties.LookupItems.Clear;
      vCollections := TDomain(FView.Domain).CollectionsByDefinition(FFieldDef.Definition);
      try
        for vCollection in vCollections do
          for vEntity in vCollection do
          begin
            vText := vEntity[FFieldDef.Name];
            if (Length(vText) > 0) and (vEdit.Properties.LookupItems.IndexOf(vText) < 0) then
              vEdit.Properties.LookupItems.Append(vText);
          end;
      finally
        FreeAndNil(vCollections);
      end;
    end;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.Style.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else begin
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.Style.Color := clWindow;
      vEdit.TabStop := FTabStop;
    end;
  end;
end;

procedure TMRUFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TcxMRUEdit(FControl).Properties) then
    TcxMRUEdit(FControl).Properties.OnChange := AHandler;
end;

{ TColorEditor }

procedure TColorEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FColorDialog);
  FreeAndNil(FSelectBtn);
  FreeAndNil(FClearBtn);
  FreeAndNil(FPaintBox);
end;

function TColorEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FBasePanel := TPanel.Create(nil);
  FBasePanel.BevelOuter := bvNone;
  Result := FBasePanel;

  FColorDialog := TColorDialog.Create(nil);
  FSelectBtn := TButton.Create(nil);
  FClearBtn := TButton.Create(nil);
  FPaintBox := TPaintBox.Create(nil);
  with FSelectBtn do
  begin
    Parent := FBasePanel;
    Align := alRight;
    Caption := '...';
    OnClick := OnSelectBtnClick;
    Width := 30;
  end;
  with FClearBtn do
  begin
    Parent := FBasePanel;
    Align := alRight;
    Caption := 'x';
    OnClick := OnClearBtnClick;
    Width := 30;
  end;
  FPaintBox.Parent := FBasePanel;
  FPaintBox.Align := alClient;
  FPaintBox.OnPaint := OnPaint;
end;

procedure TColorEditor.FillEditor;
begin
  inherited;
  FColorDialog.Color := FView.FieldValue;
  FSelectBtn.Enabled := FView.State = vsFullAccess;
  FClearBtn.Enabled := FSelectBtn.Enabled;
end;

procedure TColorEditor.OnClearBtnClick(Sender: TObject);
begin
  SetFieldValue(cNullColor);
  FColorDialog.Color := cNullColor;
  FPaintBox.Invalidate;
end;

procedure TColorEditor.OnPaint(Sender: TObject);
begin
  if FColorDialog.Color = cNullColor then
    FPaintBox.Canvas.Brush.Color := clBtnFace
  else
    FPaintBox.Canvas.Brush.Color := FColorDialog.Color;
  FPaintBox.Canvas.FillRect(FPaintBox.ClientRect);
  if FColorDialog.Color = cNullColor then
    FPaintBox.Canvas.TextOut(5, 5,  TInteractor(FView.Interactor).Translate('txtNotAssigned', 'Не задан'));
end;

procedure TColorEditor.OnSelectBtnClick(Sender: TObject);
begin
  if FColorDialog.Execute({TWinControl(FInnerControl).Handle D7}) then
  begin
    SetFieldValue(FColorDialog.Color);
    FPaintBox.Invalidate;
  end;
end;

{ TDEDateTimeFieldEditor }

function TDEDateTimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vDef: TSimpleFieldDef;
begin
  Result := TcxDateEdit.Create(nil);

  with TcxDateEdit(Result) do
  begin
    Properties.ImmediatePost := True;
    Properties.Kind := ckDateTime;
    Properties.DateButtons := [btnNow, btnClear];
    Properties.SaveTime := True;
    Properties.ShowTime := True;
    Properties.DateOnError := deNull;
  end;
//  TcxDateEdit(FInnerControl).OnKeyDown := OnWinControlKeyDown;

  vDef := TSimpleFieldDef(FFieldDef);
  with TcxDateEdit(Result) do
  begin
    if not VarIsNull(vDef.MaxValue) then
      Properties.MaxDate := vDef.MaxValue;
    if not VarIsNull(vDef.MinValue) then
      Properties.MinDate := vDef.MinValue;
  end;
end;

procedure TDEDateTimeFieldEditor.DoOnChange;
var
  vDate: TDateTime;
begin
  vDate := StrToDateTimeDef(TcxDateEdit(FControl).EditingText, 0);
  SetFieldValue(vDate);
end;

procedure TDEDateTimeFieldEditor.FillEditor;
var
  vEdit: TcxDateEdit;
  vDate: TDateTime;
begin
  vEdit := TcxDateEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Properties.InputKind := ikStandard;
    vEdit.Enabled := False;
    vEdit.Date := NullDate;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.Enabled := FView.State > vsDisabled;
    vDate := FView.FieldValue;
    if vDate < 2 then
      vEdit.Date := NullDate
    else
      vEdit.Date := vDate;

    vEdit.Properties.ReadOnly := FView.State < vsFullAccess;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Properties.InputKind := ikStandard;
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.Style.ButtonTransparency := ebtAlways;
      vEdit.Style.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else begin
      vEdit.Properties.InputKind := ikMask;
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.Style.ButtonTransparency := ebtNone;
      vEdit.Style.Color := clWindow;
      vEdit.TabStop := FTabStop;
    end;
  end;

  ToggleButtons;
end;

function TDEDateTimeFieldEditor.GetNewValue: Variant;
begin
  Result := StrToDateTimeDef(TcxDateEdit(FControl).EditingText, 0);
end;

procedure TDEDateTimeFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TcxDateEdit(FControl).Properties.OnChange := AHandler;
end;

{ TFilenameFieldEditor }

procedure TFilenameFieldEditor.BeforeExecute(Sender: TObject);
begin
  if (Length(FText.Text) > 0) and DirectoryExists(ExtractFileDir(FText.Text)) then
    FAction.Dialog.InitialDir := ExtractFileDir(FText.Text);
end;

procedure TFilenameFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FAction);
end;

function TFilenameFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vBase: TPanel;
begin
  inherited;

  vBase := TPanel.Create(nil);
  vBase.BevelOuter := bvNone;

  FAction := TFileOpen.Create(nil);
  FAction.OnAccept := OnAccept;
  FAction.BeforeExecute := BeforeExecute;
  FAction.Caption := '';
  FAction.ImageIndex := 23;
  if Assigned(FCreateParams) then
    FAction.Dialog.Filter := FCreateParams.Values['filter'];

  FBtn := TcxButton.Create(nil);
  FBtn.Align := alRight;
  FBtn.Parent := vBase;
  FBtn.Width := 40;
  FBtn.OptionsImage.Images := TDragImageList(TInteractor(Interactor).Images[16]);
  FBtn.Action := FAction;

  FText := TcxTextEdit.Create(nil);
  FText.Align := alClient;
  FText.Parent := vBase;

  Result := vBase;
end;

procedure TFilenameFieldEditor.DoOnChange;
begin
  SetFieldValue(FText.EditingText);
end;

procedure TFilenameFieldEditor.FillEditor;
begin
  TPanel(FControl).Caption := ''; //reset caption after TVCLFieldArea.Create (FControl.Name := FFieldDef.Name;)

  if VarIsNull(FView.FieldValue) then
  begin
    FText.EditValue := '';
    FText.Enabled := False;
    FText.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    FText.EditValue := FView.FieldValue;
    FText.Hint := FView.FieldValue;
    FText.Enabled := True;
    FText.Properties.ReadOnly := FView.State < vsSelectOnly;

    if FText.Properties.ReadOnly then
    begin
      FText.Style.BorderStyle := GetDisabledBorderStyle;
      FText.Style.Color := clBtnFace;
      FText.TabStop := False;
    end
    else begin
      FText.Style.BorderStyle := ebsUltraFlat;
      FText.TabStop := FTabStop;
      FText.Style.Color := clWindow;
    end;
  end;

  FAction.Visible := FView.State >= vsSelectOnly;
end;

procedure TFilenameFieldEditor.OnAccept(Sender: TObject);
begin
  FText.Text := FAction.Dialog.FileName;
  FText.Hint := FText.Text;
end;

procedure TFilenameFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(FText.Properties) then
    FText.Properties.OnChange := AHandler;
end;

procedure TSelectFolderFieldEditor.BeforeExecute(Sender: TObject);
begin
  if (Length(FText.Text) > 0) and DirectoryExists(FText.Text) then
    FAction.Folder := FText.Text;
end;

procedure TSelectFolderFieldEditor.BrowseForFolder1Accept(Sender: TObject);
begin
  FText.Text := FAction.Folder;
end;

{ TSelectFolderFieldEditor }

procedure TSelectFolderFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FAction);
end;

function TSelectFolderFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vBase: TPanel;
begin
  inherited;

  vBase := TPanel.Create(nil);
  vBase.BevelOuter := bvNone;

  FAction := TBrowseForFolder.Create(nil);
  FAction.OnAccept := BrowseForFolder1Accept;
  FAction.BeforeExecute := BeforeExecute;
  FAction.Caption := '';
  FAction.ImageIndex := 22;

  FBtn := TcxButton.Create(nil);
  FBtn.Align := alRight;
  FBtn.Parent := vBase;
  FBtn.Width := 40;
  FBtn.OptionsImage.Images := TDragImageList(TInteractor(Interactor).Images[16]);
  FBtn.Action := FAction;

  FText := TcxTextEdit.Create(nil);
  FText.Align := alClient;
  FText.Parent := vBase;

  Result := vBase;
end;

procedure TSelectFolderFieldEditor.DoOnChange;
begin
  SetFieldValue(FText.EditingText);
end;

procedure TSelectFolderFieldEditor.FillEditor;
begin
  TPanel(FControl).Caption := ''; //reset caption after TVCLFieldArea.Create (FControl.Name := FFieldDef.Name;)

  if VarIsNull(FView.FieldValue) then
  begin
    FText.EditValue := '';
    FText.Enabled := False;
    FText.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    FText.EditValue := FView.FieldValue;
    FText.Hint := FView.FieldValue;
    FText.Enabled := True;
    FText.Properties.ReadOnly := FView.State < vsFullAccess;

    if FText.Properties.ReadOnly then
    begin
      FText.Style.BorderStyle := GetDisabledBorderStyle;
      FText.Style.Color := clBtnFace;
      FText.TabStop := False;
    end
    else begin
      FText.Style.BorderStyle := ebsUltraFlat;
      FText.TabStop := FTabStop;
      FText.Style.Color := clWindow;
    end;
  end;

  FAction.Visible := FView.State > vsSelectOnly;
end;

procedure TSelectFolderFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(FText.Properties) then
    FText.Properties.OnChange := AHandler;
end;

type
  TCrackedControl = class(TWinControl) end;

{ TSpinner }

procedure TSpinner.AssignFromLayout(const ALayout: TLayout; const AParams: string);
var
  vPanel: TCrackedControl;
  vColor: Cardinal;
  vR, vG, vB: Byte;
  vRGBColor: Integer;
begin
  inherited;

  if (ALayout.Control is TPanel) or (ALayout.Control is TMemo) then
  begin
    vPanel := TCrackedControl(ALayout.Control);
    if (FControl is TdxActivityIndicator) and not vPanel.ParentFont and (vPanel.Font.Color <> clWindowText) then
    begin
      vRGBColor := TColorRec.ColorToRGB(vPanel.Font.Color);
      vR := GetRValue(vRGBColor); vG := GetGValue(vRGBColor); vB := GetBValue(vRGBColor);
      vColor := TAlphaColorRec.Alpha or TAlphaColor(RGB(vB, vG, vR));
      if TdxActivityIndicator(FControl).Properties is TdxActivityIndicatorHorizontalDotsProperties then
        TdxActivityIndicatorHorizontalDotsProperties(TdxActivityIndicator(FControl).Properties).DotColor := vColor
      else if TdxActivityIndicator(FControl).Properties is TdxActivityIndicatorGravityDotsProperties then
        TdxActivityIndicatorGravityDotsProperties(TdxActivityIndicator(FControl).Properties).DotColor := vColor
      else if TdxActivityIndicator(FControl).Properties is TdxActivityIndicatorElasticCircleProperties then
        TdxActivityIndicatorElasticCircleProperties(TdxActivityIndicator(FControl).Properties).ArcColor := vColor;
    end;
  end;
end;

function TSpinner.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TdxActivityIndicator.Create(nil);
  TdxActivityIndicator(Result).Transparent := True;
  TdxActivityIndicator(Result).Visible := False;
  if Assigned(FCreateParams) then
  begin
    if FCreateParams.Values['type'] = 'GravityDots' then
      TdxActivityIndicator(Result).PropertiesClassName := 'TdxActivityIndicatorGravityDotsProperties'
    else if FCreateParams.Values['type'] = 'ElasticCircle' then
    begin
      TdxActivityIndicator(Result).PropertiesClassName := 'TdxActivityIndicatorElasticCircleProperties';
      TdxActivityIndicatorElasticCircleProperties(TdxActivityIndicator(Result).Properties).ArcThickness := StrToIntDef(FCreateParams.Values['ArcThickness'], 3);
    end
    else
    begin
      TdxActivityIndicatorHorizontalDotsProperties(TdxActivityIndicator(Result).Properties).DotSize := StrToIntDef(FCreateParams.Values['DotSize'], 5);
    end;
  end;
end;

procedure TSpinner.FillEditor;
begin
  inherited;
  TControl(FControl).Visible := FView.FieldValue > 0;
  TdxActivityIndicator(FControl).Active := TControl(FControl).Visible;
end;

{ TImageByString }

function TImageByString.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TImage.Create(nil);
  TImage(Result).Transparent := True;
  TImage(Result).Center := True;
end;

procedure TImageByString.FillEditor;
var
  i, vImageSize: Integer;
  vStream: TStream;
begin
  inherited;
  i := FCreateParams.IndexOfName(FView.FieldValue);

  if i < 0 then Exit;

  i := StrToIntDef(FCreateParams.Values[FView.FieldValue], 0);
  vImageSize := StrToIntDef(FCreateParams.Values['ImageSize'], 16);

  vStream := TConfiguration(TInteractor(FView.Interactor).Configuration).Icons.IconByIndex(i, vImageSize);
  if Assigned(vStream) then
  begin
    vStream.Position := 0;
    TImage(FControl).Picture.LoadFromStream(vStream);
  end;
end;

{ TTextSelector }

function TTextSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TcxComboBox.Create(nil);

  TcxComboBox(Result).Properties.BeepOnError := True;
  TcxComboBox(Result).Properties.OnInitPopup := CBOnInitPopup;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TcxComboBox(Result).Properties.MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TTextSelector.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TcxComboBox(FControl).EditingText);
end;

procedure TTextSelector.FillEditor;
var
  vEdit: TcxComboBox;
begin
  FillList;

  vEdit := TcxComboBox(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.EditValue := '';
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.EditValue := FView.FieldValue;
    vEdit.Enabled := True;
    vEdit.Properties.ReadOnly := FView.State < vsSelectOnly;

      // в смена DropDownListStyle ломает отображение текущего значения, оно сбрасывается в пустоту и запись нового значения не отрабатывает
{    if FView.State < vsFullAccess then
      vEdit.Properties.DropDownListStyle := lsFixedList
    else
      vEdit.Properties.DropDownListStyle := lsEditFixedList;
}
    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.Style.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else begin
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.TabStop := FTabStop;
      if FView.State = vsSelectOnly then
        vEdit.Style.Color := clBtnFace
      else
        vEdit.Style.Color := clWindow;
    end;
  end;
end;

procedure TTextSelector.FillList;
var
  i: Integer;
  vDictionary: string;
  vFieldName: string;
  vPos: Integer;
  vCollection: TCollection;
  //vComPorts: TStrings;
begin
  TcxComboBox(FControl).Properties.Items.BeginUpdate;
  TcxComboBox(FControl).Properties.Items.Clear;

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
          TcxComboBox(FControl).Properties.Items.Add(vCollection[i].FieldValues[vFieldName])
        else
          TcxComboBox(FControl).Properties.Items.Add(vCollection[i]['Name']);

      TcxComboBox(FControl).EditValue := FView.FieldValue;
    end;
  end;
  TcxComboBox(FControl).Properties.Items.EndUpdate;
end;

procedure TTextSelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TTextSelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TcxComboBox(FControl).Properties) then
    TcxComboBox(FControl).Properties.OnChange := AHandler;
end;

{ TDEEditor }

function TDEEditor.GetBoxDisabledBorderStyle: TcxContainerBorderStyle;
begin
  if StrToBoolDef(TDomain(FView.Domain).UserSettings.GetValue('Core', 'ShowBordersForDisabled'), True) then
    Result := TcxContainerBorderStyle.cbsUltraFlat
  else
    Result := cbsNone;
end;

function TDEEditor.GetDisabledBorderStyle: TcxEditBorderStyle;
begin
  if StrToBoolDef(TDomain(FView.Domain).UserSettings.GetValue('Core', 'ShowBordersForDisabled'), True) then
    Result := ebsUltraFlat
  else
    Result := ebsNone;
end;

type
  TcxEditCrack = class(TcxCustomEdit);

procedure TDEEditor.ToggleButtons;
var
  i: Integer;
  vEdit: TcxEditCrack;
  vVisible: Boolean;
begin
  if FControl is TcxCustomEdit then
  begin
    vEdit := TcxEditCrack(FControl);
    vVisible := vEdit.Visible and vEdit.Enabled and (not vEdit.Properties.ReadOnly);

    if vVisible then
    begin
      i := 0;
      while i < TcxCustomEditProperties(vEdit.Properties).Buttons.Count do
      begin
        TcxCustomEditProperties(vEdit.Properties).Buttons.Items[i].Visible := True;
        Inc(i);
      end;
     // for i := 0 to TcxCustomEditProperties(vEdit.Properties).Buttons.Count - 1 do
     //   TcxCustomEditProperties(vEdit.Properties).Buttons.Items[i].Visible := True;
    end
    else
      for i := TcxCustomEditProperties(vEdit.Properties).Buttons.Count - 1 downto 0 do
        TcxCustomEditProperties(vEdit.Properties).Buttons.Items[i].Visible := False;
  end;
end;

{ TDEBLOBEditor }

function TDEBLOBEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TcxBLOBEdit.Create(nil);
  TcxBLOBEdit(Result).AutoSize := False;
  TcxBLOBEdit(Result).Properties.Buttons.Clear;
  TcxBLOBEdit(Result).Properties.OnChange := DoOnChangeBLOB;
end;

procedure TDEBLOBEditor.DoOnChangeBLOB(Sender: TObject);
begin
  TcxBLOBEdit(FControl).PostEditValue;
end;

procedure TDEBLOBEditor.FillEditor;
var
  vValue: AnsiString;
  vStream: TStream;
begin
  inherited;
  vStream := FView.FieldStream;
  if vStream = nil then
  begin
    TcxBLOBEdit(FControl).EditValue := '';
    Exit;
  end;

  vStream.Position := 0;
  SetLength(vValue, vStream.Size);
  vStream.ReadBuffer(vValue[1], vStream.Size);

  TcxBLOBEdit(FControl).EditValue := vValue;
  TcxBLOBEdit(FControl).Enabled := FView.State = vsFullAccess;
end;

{ TDEEnumEditor }

procedure TDEEnumEditor.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

function TDEEnumEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  if not Assigned(FEnum) then
    FEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);

  if FFieldDef.StyleName = 'radio' then
  begin
    Result := TcxRadioGroup.Create(nil);
    TcxRadioGroup(Result).Transparent := True;
    TcxRadioGroup(Result).Style.BorderStyle := ebsNone;
    TcxRadioGroup(Result).Name := 'radio';
    TcxRadioGroup(Result).Caption := '';
    FNeedCreateCaption := False;
  end
  else
  begin
    Result := TcxComboBox.Create(nil);
    TcxComboBox(Result).Properties.DropDownListStyle := lsEditFixedList;
    if Assigned(FCreateParams) then
    begin
      if FCreateParams.Values['DropDownListStyle'] = 'Fixed' then
        TcxComboBox(Result).Properties.DropDownListStyle := lsFixedList
      else if FCreateParams.Values['DropDownListStyle'] = 'Edit' then
        TcxComboBox(Result).Properties.DropDownListStyle := lsEditList;
    end;
    TcxComboBox(Result).Properties.OnInitPopup := CBOnInitPopup;
  end;
end;

procedure TDEEnumEditor.DoOnChange;
begin
  if FFieldDef.StyleName = 'radio' then
  begin
    if FFieldDef.HasFlag(cRequired) then
      SetFieldValue(TcxRadioGroup(FControl).ItemIndex + 1)
    else
      SetFieldValue(TcxRadioGroup(FControl).ItemIndex);
  end
  else
  begin
    if FFieldDef.HasFlag(cRequired) then
      SetFieldValue(TcxComboBox(FControl).ItemIndex + 1)
    else
      SetFieldValue(TcxComboBox(FControl).ItemIndex);
  end;
end;

procedure TDEEnumEditor.FillEditor;
var
  vEdit: TcxComboBox;
  vRadioEdit: TcxRadioGroup;
  vItemIndex: Integer;
begin
  FillList;

  if FFieldDef.StyleName = 'radio' then
  begin
    vRadioEdit := TcxRadioGroup(FControl);
    if VarIsNull(FView.FieldValue) then
    begin
      vRadioEdit.Enabled := False;
      vRadioEdit.Properties.ReadOnly := True;
    end
    else begin
      vItemIndex := FView.FieldValue;
      if FFieldDef.HasFlag(cRequired) then
        vItemIndex := vItemIndex - 1;
      vRadioEdit.ItemIndex := vItemIndex;

      vRadioEdit.Properties.ReadOnly := FView.State < vsSelectOnly;
      vRadioEdit.Enabled := not vRadioEdit.Properties.ReadOnly;

      if vRadioEdit.Properties.ReadOnly then
      begin
//        vRadioEdit.Style.BorderStyle := GetDisabledBorderStyle;
        vRadioEdit.TabStop := False;
      end
      else begin
//        vRadioEdit.Style.BorderStyle := ebsUltraFlat;
        vRadioEdit.TabStop := FTabStop;
      end;
    end;
  end
  else
  begin
    vEdit := TcxComboBox(FControl);
    if VarIsNull(FView.FieldValue) then
    begin
      vEdit.EditValue := ''; //FEnum.Items[0].DisplayText;
      vEdit.Enabled := False;
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.Properties.ReadOnly := True;
    end
    else begin
      vItemIndex := FView.FieldValue;
      if FFieldDef.HasFlag(cRequired) then
        vItemIndex := vItemIndex - 1;
      vEdit.ItemIndex := vItemIndex;
      //if vItemIndex = 0 then
      //  vEdit.EditValue := FEnum.Items[0].DisplayText;

      vEdit.Properties.ReadOnly := FView.State < vsSelectOnly;
      vEdit.Enabled := not vEdit.Properties.ReadOnly;

      if vEdit.Properties.ReadOnly then
      begin
        vEdit.Style.BorderStyle := GetDisabledBorderStyle;
        vEdit.TabStop := False;
      end
      else begin
        vEdit.Style.BorderStyle := ebsUltraFlat;
        vEdit.TabStop := FTabStop;
      end;
    end;

    ToggleButtons;
  end;
end;

procedure TDEEnumEditor.FillList;
var
  vEnumItem: TEnumItem;
  vItems: TStrings;
  vRadioItems: TcxRadioGroupItems;
  vRadioItem: TcxRadioGroupItem;
begin
  if FFieldDef.StyleName = 'radio' then
  begin
    vRadioItems := TcxRadioGroup(FControl).Properties.Items;
    vRadioItems.BeginUpdate;
    try
      vRadioItems.Clear;
      for vEnumItem in FEnum do
        if not FFieldDef.HasFlag(cRequired) or (vEnumItem.ID > 0) then
        begin
          vRadioItem := vRadioItems.Add;
          vRadioItem.Caption := vEnumItem.DisplayText;
        end;
    finally
      vRadioItems.EndUpdate;
    end;
  end
  else
  begin
    vItems := TcxComboBox(FControl).Properties.Items;

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

procedure TDEEnumEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  if (FFieldDef.StyleName = 'radio') and Assigned(TcxRadioGroup(FControl).Properties) then
    TcxRadioGroup(FControl).Properties.OnChange := AHandler
  else if Assigned(TcxComboBox(FControl).Properties) then
    TcxComboBox(FControl).Properties.OnChange := AHandler;
end;

{ TDEGraphicEnumSelector }

procedure TDEGraphicEnumSelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TDEGraphicEnumSelector.CBOnDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer;
  const ARect: TRect; AState: TOwnerDrawState);
var
  vID: Integer;
begin
  ACanvas.FillRect(ARect);
  ACanvas.Pen.Color := ACanvas.Font.Color;

  vID := AIndex;
  if FFieldDef.HasFlag(cRequired) then
    vID := vID + 1;

  DoDrawItem(ACanvas, vID, ARect, AState);
end;

function TDEGraphicEnumSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  if not Assigned(FEnum) then
    FEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);

  Result := TcxComboBox.Create(nil);
  TcxComboBox(Result).Properties.DropDownListStyle := lsFixedList;
  TcxComboBox(Result).Properties.OnInitPopup := CBOnInitPopup;
  TcxComboBox(Result).Properties.OnDrawItem := CBOnDrawItem;
end;

procedure TDEGraphicEnumSelector.DoDrawItem(const ACanvas: TcxCanvas; const AID: Integer; const ARect: TRect;
  AState: TOwnerDrawState);
begin
end;

procedure TDEGraphicEnumSelector.DoOnChange;
begin
  if FFieldDef.HasFlag(cRequired) then
    SetFieldValue(TcxComboBox(FControl).ItemIndex + 1)
  else
    SetFieldValue(TcxComboBox(FControl).ItemIndex);
end;

procedure TDEGraphicEnumSelector.FillEditor;
var
  vEdit: TcxComboBox;
  vItemIndex: Integer;
begin
  FillList;

  vEdit := TcxComboBox(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.EditValue := ''; //FEnum.Items[0].DisplayText;
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
    vEdit.Properties.ReadOnly := True;
  end
  else begin
    vItemIndex := FView.FieldValue;
    if FFieldDef.HasFlag(cRequired) then
      vItemIndex := vItemIndex - 1;
    vEdit.ItemIndex := vItemIndex;
    //if vItemIndex = 0 then
    //  vEdit.EditValue := FEnum.Items[0].DisplayText;

    vEdit.Properties.ReadOnly := FView.State < vsSelectOnly;
    vEdit.Enabled := not vEdit.Properties.ReadOnly;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.TabStop := False;
    end
    else begin
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.TabStop := FTabStop;
    end;
  end;

  ToggleButtons;
end;

procedure TDEGraphicEnumSelector.FillList;
var
  vEnumItem: TEnumItem;
  vItems: TStrings;
begin
  vItems := TcxComboBox(FControl).Properties.Items;

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

procedure TDEGraphicEnumSelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  if Assigned(TcxComboBox(FControl).Properties) then
    TcxComboBox(FControl).Properties.OnChange := AHandler;
end;

{ TDELineStyleSelector }

procedure TDELineStyleSelector.DoDrawItem(const ACanvas: TcxCanvas; const AID: Integer; const ARect: TRect;
  AState: TOwnerDrawState);
begin
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := Graphics.TPenStyle(AID);

  ACanvas.MoveTo(ARect.Left + 8, ARect.CenterPoint.Y);
  ACanvas.LineTo(ARect.Right - 8, ARect.CenterPoint.Y);
end;

{ TDELogFieldEditor }

function TDELogFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := inherited DoCreateControl(AParent, ALayout);
  TcxMemo(Result).Properties.ScrollBars := ssVertical;
end;

{ TDEPagesFieldEditor }

function TDEPagesFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vSourcePC: TPageControl;
  vPC: TcxPageControl;
  vSourceTab: TTabSheet;
  vTabLayout: TLayout;
  vPage: TcxTabSheet;
  vChildArea: TVCLArea;
  i: Integer;
begin
  FNeedCreateCaption := False;

  inherited;

  vSourcePC := TPageControl(ALayout.Control);

  vPC := TcxPageControl.Create(nil);
  Result := vPC;
  vPC.DoubleBuffered := True;
  vPC.Width := vSourcePC.Width;
  vPC.Height := vSourcePC.Height;

  vPC.Font.Assign(vSourcePC.Font);
  vPC.Align := vSourcePC.Align;
  vPC.AlignWithMargins := vSourcePC.AlignWithMargins;
  vPC.Margins := vSourcePC.Margins;
  vPC.Anchors := vSourcePC.Anchors;
  vPC.Style := Integer(vSourcePC.Style);
  if (vSourcePC.Style <> TTabStyle.tsTabs) and (vSourcePC.PageCount > 0) then
  begin
    vPC.Left := vSourcePC.Left + vSourcePC.Pages[0].Left;
    vPC.Top := vSourcePC.Top + vSourcePC.Pages[0].Top;
  end
  else begin
    vPC.Left := vSourcePC.Left;
    vPC.Top := vSourcePC.Top;
  end;

  vPC.HideTabs := not vSourcePC.ShowHint or ((vSourcePC.PageCount > 0) and not vSourcePC.Pages[0].TabVisible);
  if not vPC.HideTabs then
  begin
    vPC.Properties.Options := vPC.Properties.Options + [pcoTopToBottomText];
    vPC.Properties.TabPosition := TcxTabPosition(vSourcePC.TabPosition);
    vPC.Properties.TabHeight := vSourcePC.TabHeight;
  end;

  // Нужно прописывать родителя, чтобы создавать вложенные сцены
  vPC.Parent := TWinControl(TVCLArea(AParent).InnerControl);

  for i := 0 to ALayout.Items.Count - 1 do
  begin
    vTabLayout := ALayout.Items[i];
    vSourceTab := TTabSheet(vTabLayout.Control);
    vPage := TcxTabSheet.Create(vPC);
    vPage.Caption := vSourceTab.Caption;
    vPage.ImageIndex := vSourceTab.ImageIndex;

    vChildArea := TVCLArea.Create(Self, FView.Parent, vSourceTab.Name, False, vPage, vTabLayout);
    AddArea(vChildArea);
    TInteractor(FView.Interactor).UIBuilder.CreateChildAreas(vChildArea, vTabLayout, '');
  end;
end;

procedure TDEPagesFieldEditor.DoOnChange;
//var
//  vTag: Integer;
begin
{  vTag := TcxPageControl(FControl).ActivePageIndex;
  if vTag < 0 then
    Exit;

  if TFieldDef(FView.Definition).Kind = fkBoolean then
  begin
    case vTag of
      0: SetFieldValue(False);
      1: SetFieldValue(True);
    else
      SetFieldValue(Null);
    end;
  end
  else if TFieldDef(FView.Definition).Kind = fkEnum then
    SetFieldValue(vTag); }
end;

procedure TDEPagesFieldEditor.FillEditor;
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
      vTag := TEntity(Integer(vValue)).ID
    else
      vTag := -1;
  end;

  if (vTag < TcxPageControl(FControl).PageCount) and (TcxPageControl(FControl).ActivePageIndex <> vTag) then
    TcxPageControl(FControl).ActivePageIndex := vTag;
end;

procedure TDEPagesFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TcxPageControl(FControl).OnChange := AHandler;
end;

{ TProgress }

function TProgress.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TcxProgressBar.Create(nil);
  TcxProgressBar(Result).AutoSize := TPanel(ALayout.Control).ShowCaption;
  TcxProgressBar(Result).Properties.SolidTextColor := True;
  TcxProgressBar(Result).Properties.ShowText := TPanel(ALayout.Control).ShowCaption;
  FNeedCreateCaption := False;
  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TcxProgressBar(Result).Properties.Max := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TProgress.FillEditor;
begin
  inherited;
  TcxProgressBar(FControl).Position := FView.FieldValue;
end;

{ TEntityBreadcrumb }

function TEntityBreadcrumb.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TdxBreadcrumbEdit.Create(nil);
  TdxBreadcrumbEdit(Result).Properties.PathEditor.PathDelimiter := '.';
end;

procedure TEntityBreadcrumb.FillEditor;
var
  vDefinition: TDefinition;
begin
  inherited;
  if FView.Parent.DefinitionKind = dkObjectField then
    vDefinition := TObjectFieldDef(FView.Parent.Definition)._ContentDefinition
  else
    vDefinition := TDefinition(FView.Parent.Definition);

  SetValidateDefinition(vDefinition);
//  TdxBreadcrumbEdit(FControl).Root
  if FView.FieldValue = '' then
    TdxBreadcrumbEdit(FControl).SelectedPath := '$'
  else
    TdxBreadcrumbEdit(FControl).SelectedPath := '$.' + FView.FieldValue;
end;

procedure TEntityBreadcrumb.SetValidateDefinition(const ADefinition: TDefinition);
var
  vNode: TdxBreadcrumbEditNode;
  procedure ProcessDef(const ADef: TDefinition; const ANode: TdxBreadcrumbEditNode);
  var
    vFieldDef: TFieldDef;
  begin
    for vFieldDef in ADef.Fields do
    begin
      if TInteractor(FView.Interactor).NeedSkipField(nil, vFieldDef) then
        Continue;

      vNode := ANode.AddChild;
      vNode.Name := vFieldDef.Name;
      if vFieldDef is TEntityFieldDef then
        ProcessDef(TEntityFieldDef(vFieldDef).ContentDefinitions[0], vNode)
    end;
  end;
begin
//  vEdit :=
//  TdxBreadcrumbEdit(FControl).BeginUpdate;
  try
    TdxBreadcrumbEdit(FControl).Root.Clear;
    TdxBreadcrumbEdit(FControl).Root.Name := '$';
    ProcessDef(ADefinition, TdxBreadcrumbEdit(FControl).Root);
  finally
//    TdxBreadcrumbEdit(FControl).EndUpdate;
  end;
end;

procedure TEntityBreadcrumb.DoOnChange;
var
  vPath: string;
begin
  vPath := TdxBreadcrumbEdit(FControl).SelectedPath;
  if Length(vPath) > 2 then
    Delete(vPath, 1, 2)
  else
    vPath := '';
  SetFieldValue(vPath);
end;

{ TDEFlagsEditor }

procedure TDEFlagsEditor.CLBOnClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
begin
  OnChange(Sender);
end;

function TDEFlagsEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  Result := TcxCheckListBox.Create(nil);
  TcxCheckListBox(Result).OnClickCheck := CLBOnClickCheck;
end;

procedure TDEFlagsEditor.DoOnChange;
var
  i: Integer;
  vFlagsValue: Integer;
  vListItem: TcxCheckListBoxItem;
begin
  vFlagsValue := 0;
  for i := 0 to TcxCheckListBox(FControl).Items.Count - 1 do
  begin
    vListItem := TcxCheckListBox(FControl).Items[i];
    if vListItem.Checked then
      vFlagsValue := vFlagsValue or TEnumItem(vListItem.ItemObject).ID;
  end;

  SetFieldValue(vFlagsValue);
end;

procedure TDEFlagsEditor.FillEditor;
var
  vList: TcxCheckListBox;
begin
  FillList;

  vList := TcxCheckListBox(FControl);
  vList.ReadOnly := FView.State < vsSelectOnly;
  vList.Enabled := not vList.ReadOnly;

  if vList.ReadOnly then
  begin
    vList.Style.BorderStyle := GetBoxDisabledBorderStyle;
    vList.TabStop := False;
  end
  else begin
    vList.Style.BorderStyle := cbsUltraFlat;
    vList.TabStop := FTabStop;
  end;
end;

procedure TDEFlagsEditor.FillList;
var
  vList: TcxCheckListBox;
  vEnumItem: TEnumItem;
  vListItem: TcxCheckListBoxItem;
  vValue: Integer;
begin
  vList := TcxCheckListBox(FControl);
  vValue := FView.FieldValue;
  vList.Items.BeginUpdate;
  try
    vList.Items.Clear;
    for vEnumItem in FEnum do
    begin
      if vEnumItem.ID > 0 then
      begin
        vListItem := vList.Items.Add;
        vListItem.Text := vEnumItem.DisplayText;
        vListItem.ItemObject := vEnumItem;
        vListItem.Checked := (vEnumItem.ID and vValue) <> 0;
      end;
    end;
  finally
    vList.Items.EndUpdate;
  end;
end;

function TDEFlagsEditor.GetLayoutPositionCount: Integer;
begin
  Result := 3;
end;

procedure TDEFlagsEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  if Assigned(AHandler) then
    TcxCheckListBox(FControl).OnClickCheck := CLBOnClickCheck
  else
    TcxCheckListBox(FControl).OnClickCheck := nil;
end;

{ TDEImagedAction }

destructor TDEImagedAction.Destroy;
begin

  inherited;
end;

procedure TDEImagedAction.DoBeforeFreeControl;
begin
  if Assigned(FActionView) then
  begin
    FActionView.RemoveListener(Self);
    FActionView := nil;
  end;
end;

function TDEImagedAction.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vActionName: string;
  vImageSize: Integer;
  vButton: TcxButton;
begin
  if Assigned(FCreateParams) then
  begin
    vActionName := FCreateParams.Values['action'];
    vImageSize := StrToIntDef(FCreateParams.Values['ImageSize'], 16);
    FFalseImageID := GetImageID(StrToIntDef(FCreateParams.Values['false'], -1));
    FTrueImageID := GetImageID(StrToIntDef(FCreateParams.Values['true'], -1));
    FTrueHint := FCreateParams.Values['trueHint'];
    FFalseHint := FCreateParams.Values['falseHint'];
  end
  else begin
    vActionName := '';
    vImageSize := 16;
    FTrueImageID := GetImageID(-1);
    FFalseImageID := GetImageID(-1);
    FTrueHint := TFieldDef(FView.Definition)._Caption;
    FFalseHint := TFieldDef(FView.Definition)._Caption;
  end;

  if vActionName <> '' then
  begin
    FActionView := FUIBuilder.RootView.BuildView(vActionName);
    FActionView.AddListener(Self);
    if FActionView.DefinitionKind = dkUndefined then
    begin
      FActionView.CleanView;
      FActionView := nil;
    end;
  end
  else
    FActionView := nil;

  vButton := TcxButton.Create(nil);
  vButton.OptionsImage.Images := TDragImageList(TInteractor(Interactor).Images[vImageSize]);
  vButton.PaintStyle := bpsGlyph;
  vButton.OnClick := OnButtonClick;

  Result := vButton;
end;

procedure TDEImagedAction.FillEditor;
begin
  if VarIsNull(FView.FieldValue) or (not FView.FieldValue) then
  begin
    TcxButton(FControl).OptionsImage.ImageIndex := FFalseImageID;
    TcxButton(FControl).Hint := FTrueHint;
  end
  else
  begin
    TcxButton(FControl).OptionsImage.ImageIndex := FTrueImageID;
    TcxButton(FControl).Hint := FFalseHint;
  end;

  TcxButton(FControl).Enabled := FView.State = vsFullAccess;
end;

procedure TDEImagedAction.OnButtonClick(Sender: TObject);
begin
  if Assigned(FActionView) then
    FActionView.ExecuteAction(Holder);
end;

procedure TDEImagedAction.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  //TcxButton(Result).OnClick := AHandler;
end;

{ TBoolImages }

function TBoolImages.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TImage.Create(nil);
  TImage(Result).Transparent := True;
end;

procedure TBoolImages.FillEditor;
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
    TImage(FControl).Picture.LoadFromStream(vStream);
    if FView.FieldValue then
      TImage(FControl).Hint := FCreateParams.Values['trueHint']
    else
      TImage(FControl).Hint := FCreateParams.Values['falseHint'];
    TImage(FControl).Parent.Invalidate;
  end;
end;

{ TDEColorEditor }

function TDEColorEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TdxColorEdit.Create(nil);
  TdxColorEdit(Result).Properties.ColorPalette := TdxColorPalette.cpExtended;
  TdxColorEdit(Result).Properties.ColorSet := TdxColorSet.csDefault;
end;

procedure TDEColorEditor.DoOnChange;
begin
  SetFieldValue(TdxColorEdit(FControl).ColorValue);
end;

procedure TDEColorEditor.FillEditor;
var
  vEdit: TdxColorEdit;
begin
  vEdit := TdxColorEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.ColorValue := 0;
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.ColorValue := FView.FieldValue;
    vEdit.Properties.ReadOnly := FView.State < vsFullAccess;

    if vEdit.Properties.ReadOnly then
    begin
      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.Style.ButtonTransparency := ebtAlways;
      vEdit.Style.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else begin
      vEdit.Style.BorderStyle := ebsUltraFlat;
      vEdit.Style.ButtonTransparency := ebtNone;
      vEdit.Style.Color := clWindow;
      vEdit.TabStop := FTabStop;
    end;
  end;
end;

procedure TDEColorEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TdxColorEdit(FControl).Properties.OnChange := AHandler;
end;

{ TGauge }

procedure TGauge.DoBeforeFreeControl;
begin
  inherited;
 // FGaugeControl.Parent := nil;
 // FreeAndNil(FGaugeControl1CircularHalfScale);
 // FreeAndNil(FGaugeControl);
end;

function TGauge.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vRange: TdxGaugeCircularScaleRange;
  vMax, vMin: Integer;
begin
  inherited;
  FGaugeControl := TdxGaugeControl.Create(nil);
  if Assigned(AParent.InnerControl) and (AParent.InnerControl is TWinControl) then
    FGaugeControl.Parent := TWinControl(AParent.InnerControl);
  FGaugeControl.Transparent := True;
  FGaugeControl.BorderStyle := cxcbsNone;
  FGaugeControl1CircularHalfScale := TdxGaugeCircularHalfScale(FGaugeControl.AddScale(TdxGaugeCircularHalfScale));
  FGaugeControl1CircularHalfScale.StyleName := 'Smart';
  FGaugeControl1CircularHalfScale.OptionsView.ShowTicks := False;
  FGaugeControl1CircularHalfScale.OptionsLayout.CenterPositionFactorX := 0.504975199699401900;
  FGaugeControl1CircularHalfScale.OptionsAnimate.Enabled := True;
  vRange := FGaugeControl1CircularHalfScale.Ranges.Add as TdxGaugeCircularScaleRange;
  vRange.ValueStart := 70;
  vRange.ValueEnd := 100;
  vRange.WidthFactor := 0.3;
  vRange.Color := dxColorToAlphaColor(clMaroon, 30);
  vRange.Visible := True;

  if Assigned(FCreateParams) then
  begin
    vMin := StrToIntDef(FCreateParams.Values['Min'], 0);
    vMax := StrToIntDef(FCreateParams.Values['Max'], 100);
    FGaugeControl1CircularHalfScale.OptionsView.MaxValue := vMax;
    FGaugeControl1CircularHalfScale.OptionsView.MinValue := vMin;

    vRange.ValueStart := vMax - (vMax - vMin) * 0.3;
    vRange.ValueEnd := vMax;
  end;

  Result := FGaugeControl;

end;

procedure TGauge.FillEditor;
begin
  inherited;
  FGaugeControl1CircularHalfScale.Value := FView.FieldValue;
end;

{ TIntegerFlagsEditor }

procedure TIntegerFlagsEditor.CLBOnClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
begin
  OnChange(Sender);
end;

procedure TIntegerFlagsEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FCaptions);
end;

function TIntegerFlagsEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vHorzLayout: Boolean;
begin
  Result := TcxCheckListBox.Create(nil);
  TcxCheckListBox(Result).OnClickCheck := CLBOnClickCheck;

  vHorzLayout := False;
  FDisplayFlagCount := 8;

  if Assigned(CreateParams) then
  begin
    FDisplayFlagCount := Min(32, StrToIntDef(CreateParams.Values['DisplayFlagCount'], 8));
    if CreateParams.IndexOfName('ItemCaptions') > -1 then
      FCaptions := CreateDelimitedList(CreateParams.Values['ItemCaptions'], ';');
    vHorzLayout := StrToIntDef(CreateParams.Values['HorzLayout'], 0) = 1;
  end;

  if vHorzLayout then
    TcxCheckListBox(Result).Columns := FDisplayFlagCount;
end;

procedure TIntegerFlagsEditor.DoOnChange;
var
  i: Integer;
  vFlagsValue: Integer;
  vListItem: TcxCheckListBoxItem;
begin
  vFlagsValue := 0;
  for i := 0 to TcxCheckListBox(FControl).Items.Count - 1 do
  begin
    vListItem := TcxCheckListBox(FControl).Items[i];
    if vListItem.Checked then
      vFlagsValue := vFlagsValue or Integer(vListItem.ItemObject);
  end;

  SetFieldValue(vFlagsValue);
end;

procedure TIntegerFlagsEditor.DoOnExit(Sender: TObject);
begin
  TcxCheckListBox(FControl).ItemIndex := -1;
end;

procedure TIntegerFlagsEditor.FillEditor;
var
  vList: TcxCheckListBox;
begin
  FillList;

  vList := TcxCheckListBox(FControl);
  vList.ReadOnly := FView.State < vsSelectOnly;
  vList.Enabled := not vList.ReadOnly;

  if vList.ReadOnly then
  begin
    vList.Style.BorderStyle := GetBoxDisabledBorderStyle;
    vList.TabStop := False;
  end
  else begin
    vList.Style.BorderStyle := cbsNone;
    vList.TabStop := FTabStop;
  end;
end;

procedure TIntegerFlagsEditor.FillList;
var
  vList: TcxCheckListBox;
  vListItem: TcxCheckListBoxItem;
  vBits, vBit: Integer;
  i: Integer;
begin
  vList := TcxCheckListBox(FControl);
  vBits := FView.FieldValue;
  vList.Items.BeginUpdate;
  try
    vList.Items.Clear;
    for i := 0 to FDisplayFlagCount - 1 do
    begin
      vListItem := vList.Items.Add;
      if Assigned(FCaptions) and (i < FCaptions.Count) then
        vListItem.Text := FCaptions[i]
      else
        vListItem.Text := IntToStr(i);
      vBit := 1 shl i;
      vListItem.ItemObject := TObject(vBit);
      vListItem.Checked := (vBit and vBits) <> 0;
    end;
  finally
    vList.Items.EndUpdate;
  end;
end;

procedure TIntegerFlagsEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  if Assigned(AHandler) then
    TcxCheckListBox(FControl).OnClickCheck := CLBOnClickCheck
  else
    TcxCheckListBox(FControl).OnClickCheck := nil;
end;

{ TLogEditor }

procedure TLogEditor.DoBeforeFreeControl;
begin
  FreeAndNil(FData);
  inherited;
end;

function TLogEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  inherited;
  FListView := TListView.Create(nil);
  FListView.OnData := OnListViewData;
  FListView.OwnerData := True;
  FListView.Columns.Add.AutoSize := True;
  FListView.ShowColumnHeaders := False;
  FListView.ReadOnly := True;

  FData := TStringList.Create;

  Result := FListView;
end;

procedure TLogEditor.FillEditor;
begin
  inherited;

  FData.Text := FView.FieldValue;

  FListView.Items.Count := FData.Count;

  FListView.Refresh;
end;

procedure TLogEditor.OnListViewData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := FData[FData.Count - Item.Index - 1];
end;

procedure TLogEditor.SetParent(const Value: TUIArea);
begin
  inherited;
  FListView.ViewStyle := vsReport;
end;

{ TSelectedCaptionBoolFieldEditor }

function TSelectedCaptionBoolFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FNeedCreateCaption := False;

  Result := TLabel.Create(nil);
  TLabel(Result).OnClick := OnClick;
  if Assigned(FCreateParams) then
    TLabel(Result).Caption := FCreateParams.Values['Caption'];
  TLabel(Result).Transparent := False;
  TLabel(Result).Cursor := crHandPoint;

  if ALayout.Control is TPanel then
  begin
    FSelectBackColor := AlphaColorToColor($FF5132);
    if Assigned(FCreateParams) and (FCreateParams.IndexOfName('select_backcolor') > -1) then
      FSelectBackColor := AlphaColorToColor(StrToIntDef('$' + FCreateParams.Values['select_backcolor'], 0));

    FDefaultTextColor := TPanel(ALayout.Control).Font.Color;
    TLabel(Result).Alignment := TPanel(ALayout.Control).Alignment;
  end;
end;

procedure TSelectedCaptionBoolFieldEditor.FillEditor;
begin
  if VarIsNull(FView.FieldValue) then
    FSelected := False
  else
    FSelected := FView.FieldValue;

  UpdateView;
end;

procedure TSelectedCaptionBoolFieldEditor.OnClick(Sender: TObject);
begin
  FSelected := not FSelected;

  SetFieldValue(FSelected);

  UpdateView;
end;

procedure TSelectedCaptionBoolFieldEditor.UpdateView;
begin
  if FSelected then
  begin
    TLabel(FControl).Font.Color := clWhite;
    TLabel(FControl).Color := FSelectBackColor;
    TLabel(FControl).Transparent := False;
  end
  else
  begin
    TLabel(FControl).Transparent := True;
    TLabel(FControl).Font.Color := FDefaultTextColor;
  end;
end;

initialization

RegisterClasses([TdxBevel, TcxLabel, TcxTreeList, TcxTreeListColumn]);
{ dx localization
cxSetResourceString(@cxSDateThursday, 'Donderdag'); //=Thursday
cxSetResourceString(@cxSDatePopupToday, 'Vandaag');//= 'Today';}

TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, '', TDETextFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'phone', TDETextFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'email', TDEMaskFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'url', TDEMaskFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'INN', TDEMaskFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'memo', TDEMemoFieldEditor);
//TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'log', TDELogFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'log', TLogEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'mru', TMRUFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'dir', TSelectFolderFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'file', TFilenameFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'ImageByString', TImageByString);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'selector', TTextSelector);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'comport', TTextSelector);
TPresenter.RegisterUIClass('Windows.DevExpress', uiTextEdit, 'fieldpath', TEntityBreadcrumb);
TPresenter.RegisterUIClass('Windows.DevExpress', uiIntegerEdit, '', TDEIntegerFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiIntegerEdit, 'simple', TDEIntegerFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiIntegerEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.DevExpress', uiIntegerEdit, 'spinner', TSpinner);
TPresenter.RegisterUIClass('Windows.DevExpress', uiIntegerEdit, 'progress', TProgress);
TPresenter.RegisterUIClass('Windows.DevExpress', uiIntegerEdit, 'gauge', TGauge);
TPresenter.RegisterUIClass('Windows.DevExpress', uiIntegerEdit, 'pages', TDEPagesFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiIntegerEdit, 'flags', TIntegerFlagsEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEnumEdit, '', TDEEnumEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEnumEdit, 'radio', TDEEnumEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEnumEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEnumEdit, 'line_style', TDELineStyleSelector);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEnumEdit, 'pages', TDEPagesFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiFlagEdit, '', TDEFlagsEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEntityEdit, 'enum', TDEEnumFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiFloatEdit, '', TDEFloatFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiFloatEdit, 'currency_rate', TDEFloatFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiFloatEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.DevExpress', uiFloatEdit, 'gauge', TGauge);
TPresenter.RegisterUIClass('Windows.DevExpress', uiDateEdit, '', TDEDateFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiDateEdit, 'time', TDETimeFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiDateEdit, 'datetime', TDEDateTimeFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiDateEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.DevExpress', uiCurrencyEdit, '', TDECurrencyFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiCurrencyEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.DevExpress', uiBoolEdit, '', TDEBoolFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiBoolEdit, 'simple', TDEBoolFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiBoolEdit, 'imaged_action', TDEImagedAction);
TPresenter.RegisterUIClass('Windows.DevExpress', uiBoolEdit, 'images', TBoolImages);
TPresenter.RegisterUIClass('Windows.DevExpress', uiBoolEdit, 'pages', TDEPagesFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiBoolEdit, 'selected_caption', TSelectedCaptionBoolFieldEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiColorEdit, '', TColorEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiColorEdit, 'simple', TDEColorEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiBLOBEdit, '', TDEBLOBEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiBLOBEdit, 'image', TDEImageEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEntityEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.DevExpress', uiEntityEdit, 'pages', TDEPagesFieldEditor);

end.
