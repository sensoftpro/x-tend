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

unit fmxSimpleEditors;

interface

uses
  Classes,
  uDefinition, uEnumeration, uUIBuilder, uView, uFMXArea, uBaseLayout,
  //FMX
  FMX.ListBox, FMX.Types, FMX.Graphics, FMX.ExtCtrls, FMX.StdCtrls, FMX.Dialogs, FMX.Controls, FMX.ActnList, FMX.StdActns;

type
  TTextInfo = class (TFMXFieldArea)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
  end;

  TDEEditor = class (TFMXFieldArea)
  protected
//    procedure ToggleButtons;
  end;

  TDEEnumEditor = class(TDEEditor)
  private
    FEnum: TEnumeration;
    procedure FillList;
    procedure CBOnPopup(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  {TDEGraphicEnumSelector = class(TDEEditor)
  private
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
    procedure CBOnDrawItem(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect;
      AState: TOwnerDrawState);
  protected
    FEnum: TEnumeration;

    procedure DoDrawItem(const ACanvas: TcxCanvas; const AID: Integer; const ARect: TRect;
      AState: TOwnerDrawState); virtual;
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;   }

  {TDELineStyleSelector = class(TDEGraphicEnumSelector)
  protected
    procedure DoDrawItem(const ACanvas: TcxCanvas; const AID: Integer; const ARect: TRect;
      AState: TOwnerDrawState); override;
  end;     }

  TDEFlagsEditor = class(TDEEditor)
  private
    FEnum: TEnumeration;
    procedure FillList;
    procedure LBOnClickCheck(Sender: TObject);
    procedure LBOnItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
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
    procedure LBOnItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
    procedure LBOnClickCheck(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEIntegerFieldEditor = class (TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure FocusedChanged(const AFocused: Boolean); override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  {TDEFloatFieldEditor = class (TDEEditor)
  private
    FAfterPoint: Integer; // знаков после запятой
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;  }

  TDECurrencyFieldEditor = class (TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEDateFieldEditor = class (TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    function GetNewValue: Variant; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDETimeFieldEditor = class (TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
//    function GetNewValue: Variant; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  {TDEDateTimeFieldEditor = class (TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    function GetNewValue: Variant; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;        }

  TDETextFieldEditor = class (TDEEditor)
  private
    procedure OnPhoneKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TTextSelector = class (TDEEditor)
  private
    procedure FillList;
    procedure CBOnPopup(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  {TSelectFolderFieldEditor = class (TDEEditor)
  private
    FText: TcxTextEdit;
    FBtn: TcxButton;
    FAction: TBrowseForFolder;
    procedure BrowseForFolder1Accept(Sender: TObject);
    procedure BeforeExecute(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
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
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  public
    property TextEdit: TcxTextEdit read FText;
  end;

  TDEMaskFieldEditor = class (TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  {TMRUFieldEditor = class (TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;          }

  TDEMemoFieldEditor = class (TDEEditor)
  private
    procedure OnKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    function GetLayoutPositionCount: Integer; override;
  end;

  TDEBoolFieldEditor = class(TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
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
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  public
    destructor Destroy; override;
  end;

  TDEPagesFieldEditor = class(TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  {TDEEnumFieldEditor = class (TDEEditor)
  private
    FGroupName: String;

    procedure RBOnItemSelect(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  {TDEImageEditor = class (TDEEditor)
  private
    procedure DoOnAssignPicture(Sender: TObject;
      const Picture: TPicture);
    procedure DoOnChangeImage(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    function GetLayoutPositionCount: Integer; override;
  end;

  {TDEBLOBEditor = class(TDEEditor)
  private
    procedure DoOnChangeBLOB(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
  end;

  {TImageByString = class (TFMXFieldArea)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
  end;

  TBoolImages = class (TFMXFieldArea)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
  end;

  {TDEColorEditor = class(TDEEditor)
  private
    //FBasePanel: TPanel;
    //FColorDialog: TColorDialog;
    //FSelectBtn, FClearBtn: TButton;
    //FPaintBox: TPaintBox;
    //procedure OnSelectBtnClick(Sender: TObject);
    //procedure OnClearBtnClick(Sender: TObject);
    //procedure OnPaint(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;

    //procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    //procedure DoBeforeFreeControl; override;
    //procedure FillEditor; override;
  end;

  {TColorEditor = class (TVCLFieldArea)
  private
    FBasePanel: TPanel;
    FColorDialog: TColorDialog;
    FSelectBtn, FClearBtn: TButton;
    FPaintBox: TPaintBox;
    procedure OnSelectBtnClick(Sender: TObject);
    procedure OnClearBtnClick(Sender: TObject);
    procedure OnPaint(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;   }

  // неопределённый по времени процесс
  {TSpinner = class (TVCLFieldArea)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure AssignFromLayout(const ALayout: TBaseLayout); override;
  end;

  // определённый по времени процесс
  {TProgress = class (TVCLFieldArea)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
  end;

  {TGauge = class (TVCLFieldArea)
  private
    FGaugeControl: TdxGaugeControl;
    FGaugeControl1CircularHalfScale: TdxGaugeCircularHalfScale;
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;

  {TEntityBreadcrumb = class (TVCLFieldArea)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SetValidateDefinition(const ADefinition: TDefinition);
  end;

  {TLogEditor = class(TFMXFieldArea)
  private
    FListView: TListView;
    FData: TStringList;
    procedure OnListViewData(Sender: TObject; Item: TListItem);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure SetParent(const Value: TUIArea); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;    }

implementation

uses
  Generics.Collections, TypInfo, Variants, SysUtils, Math, DateUtils, UITypes,
  FMX.Forms, FMX.SpinBox, FMX.DateTimeCtrls, FMX.Edit, FMX.Memo, FMX.TabControl, FMX.ImgList, FMX.Text,
  uConfiguration, uDomain, uInteractor, uPresenter, uCollection, uEntity, uConsts, uUtils;

{ TTextInfo }

procedure TTextInfo.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TLabel.Create(nil);
  TLabel(FControl).WordWrap := True;
  TLabel(FControl).StyledSettings := [TStyledSetting.Family];
  if Assigned(FCreateParams) and (FCreateParams.Values['WordWrap'] = 'False') then
    TLabel(FControl).WordWrap := False;
  TLabel(FControl).AutoSize := False;
end;

procedure TTextInfo.FillEditor;
var
  vEnum: TEnumeration;
  vEntity: TEntity;
  vColorField: TBaseField;
begin
  inherited;
  if VarIsNull(FView.FieldValue) then
    TLabel(FControl).Text := ''
  else if FFieldDef.Kind = fkCurrency then
  begin
    if FFieldDef.Format <> '' then
      TLabel(FControl).Text := FormatFloat(FFieldDef.Format, FView.FieldValue)
    else
      TLabel(FControl).Text := FormatFloat('#,##0.00;;0', FView.FieldValue);
  end
  else if FFieldDef.Kind = fkFloat then
  begin
    if FFieldDef.Format <> '' then
      TLabel(FControl).Text := FormatFloat(FFieldDef.Format, FView.FieldValue)
    else
      TLabel(FControl).Text := FView.FieldValue;
  end
  else if FFieldDef.Kind = fkDateTime then
  begin
    if FFieldDef.Format <> '' then
      TLabel(FControl).Text := FormatDateTime(FFieldDef.Format, FView.FieldValue)
    else
      TLabel(FControl).Text := FormatDateTime('dd.mm.yyyy hh:nn:ss', FView.FieldValue);
  end
  else if FFieldDef.Kind = fkEnum then
  begin
    vEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
    if not Assigned(vEnum) then
    begin
      vEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
      TLabel(FControl).TextSettings.FontColor := ColorToAlphaColor(TState(vEnum.Items[Integer(FView.FieldValue)]).Color);
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
        TLabel(FControl).TextSettings.FontColor := ColorToAlphaColor(TColor(vColorField.Value));
      end;
    end;
  end
  else
    TLabel(FControl).Text := FView.FieldValue;
end;

{ TDEIntegerEditControl }

procedure TDEIntegerFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TSpinBox.Create(nil);

  TSpinBox(FControl).ValueType := TNumValueType.Integer;
//  TSpinEdit(FInnerControl).OnKeyDown := OnWinControlKeyDown;

  with TSpinBox(FControl) do
  begin
    if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
      Max := TSimpleFieldDef(FFieldDef).MaxValue;

    if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
      Min := TSimpleFieldDef(FFieldDef).MinValue;
  end;
end;

procedure TDEIntegerFieldEditor.DoOnChange;
begin
  if TSpinBox(FControl).Value = 0 then
    SetFieldValue(Null)
  else
    SetFieldValue(TSpinBox(FControl).Value);
end;

procedure TDEIntegerFieldEditor.FillEditor;
var
  vEdit: TSpinBox;
begin
  vEdit := TSpinBox(FControl);

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
      vEdit.TabStop := True;
  end;

//  vEdit.Properties.SpinButtons.Visible := vEdit.Visible and vEdit.Enabled and (not vEdit.Properties.ReadOnly);
end;

procedure TDEIntegerFieldEditor.FocusedChanged(const AFocused: Boolean);
begin
  inherited;
  if not AFocused then
  begin
    // обрабатываем 0
//    TSpinBox(FControl).PostEditValue;
    SetFieldValue(TSpinBox(FControl).Value);
  end;
end;

procedure TDEIntegerFieldEditor.SwitchChangeHandlers(
  const AHandler: TNotifyEvent);
begin
  inherited;
  TSpinBox(FControl).OnChange := AHandler
end;

{ TDEFloatEditControl

procedure TDEFloatFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TMaskEdit.Create(nil);
  FAfterPoint := 4;
  if Length(FFieldDef.Format) > 0 then
    FAfterPoint := Length(FFieldDef.Format) - Pos('.', FFieldDef.Format);

  with TMaskEdit(FControl).Properties do
  begin
    ImmediatePost := True;
    MaskKind := emkRegExpr;
    EditMask := '-?\d+[' + FormatSettings.DecimalSeparator + '.,]?\d+';
    ValidationOptions := [evoAllowLoseFocus];
  end;

  with TMaskEdit(FControl).Properties do
  begin
    if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
      MaxValue := TSimpleFieldDef(FFieldDef).MaxValue;
    if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
      MinValue := TSimpleFieldDef(FFieldDef).MinValue;
  end;
end;

procedure TDEFloatFieldEditor.DoOnChange;
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
  if TMaskEdit(FControl).EditingValue = '' then
    vValue := 0
  else
    vValue := ToFloatDef(TMaskEdit(FControl).EditingValue);

  if (not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue)) and (vValue < TMaskEdit(FControl).Properties.MinValue) then
    vValue := TMaskEdit(FControl).Properties.MinValue;
  if (not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue)) and (vValue > TMaskEdit(FControl).Properties.MaxValue) then
    vValue := TMaskEdit(FControl).Properties.MaxValue;
  SetFieldValue(vValue);
end;

procedure TDEFloatFieldEditor.FillEditor;
var
  vEdit: TMaskEdit;
begin
  vEdit := TMaskEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.EditValue := 0;
    vEdit.Enabled := False;
    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.EditValue := RoundTo(FView.FieldValue, -FAfterPoint);
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
      vEdit.TabStop := True;
    end;
  end;
end;

procedure TDEFloatFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TMaskEdit(FControl).Properties.OnChange := AHandler
end;

{ TDEDateEditControl }

procedure TDEDateFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
//var
//  vDef: TSimpleFieldDef;
begin
  FControl := TDateEdit.Create(nil);

  TDateEdit(FControl).ShowClearButton := True;
//  TDateEdit(FInnerControl).OnKeyDown := OnWinControlKeyDown;

//  vDef := TSimpleFieldDef(FFieldDef);
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
  if MyTryStrToDate(TDateEdit(FControl).Text, vDate) then
    SetFieldValue(vDate)
  else
    SetFieldValue(Null);
end;

procedure TDEDateFieldEditor.FillEditor;
var
  vEdit: TDateEdit;
  vDate: TDateTime;
begin
  vEdit := TDateEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.Date := 0;
  end
  else
  begin
    vEdit.Enabled := FView.State > vsDisabled;
    vDate := FView.FieldValue;
    if vDate < 2 then
      vEdit.Date := 0
    else
      vEdit.Date := vDate;

    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
      vEdit.TabStop := False
    else
      vEdit.TabStop := True;
  end;

//  ToggleButtons;
end;

function TDEDateFieldEditor.GetNewValue: Variant;
var
  vDate: TDateTime;
begin
  if MyTryStrToDate(TDateEdit(FControl).Text, vDate) then
    Result := vDate
  else
    Result := Null;
end;

procedure TDEDateFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TDateEdit(FControl).OnChange := AHandler;
end;

{ TDETextEditControl }

procedure TDETextFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  inherited;
  FControl := TEdit.Create(nil);

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TEdit(FControl).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;

  if SameText('phone', FFieldDef.StyleName) then
    TEdit(FControl).OnKeyDown := OnPhoneKeyPress
  else
    TEdit(FControl).OnKeyDown := nil;
end;

procedure TDETextFieldEditor.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TEdit(FControl).Text);
end;

procedure TDETextFieldEditor.OnPhoneKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if CharInSet(KeyChar, ['0'..'9', #8, '-', '+', '(', ')', ' ']) then Exit;

  Key := 0;
  keyChar := #0;
end;

procedure TDETextFieldEditor.FillEditor;
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

    if vEdit.ReadOnly then
      vEdit.TabStop := False
    else
      vEdit.TabStop := True;
  end;
end;

procedure TDETextFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TEdit(FControl)) then
    TEdit(FControl).OnChange := AHandler;
end;

{ TDEEnumEditControl

procedure TDEEnumFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
var
  vRadioItem: TRadioButton;
  vCollections: TList<TCollection>;
  vCollection: TCollection;
  vEntity: TEntity;
begin
  FControl := TPanel.Create(nil);
  FGroupName := FFieldDef.Name;

//  TRadioGroup(Result).OnKeyDown := OnWinControlKeyDown;

  vCollections := TDomain(FView.Domain).CollectionsByDefinition(
    TEntityFieldDef(FFieldDef)._ContentDefinition);
  try
    for vCollection in vCollections do
    begin
      for vEntity in vCollection do
        if not vEntity.IsNew then
        begin
          vRadioItem := TRadioButton.Create(nil);
          vRadioItem.Parent := FControl;
          vRadioItem.Text := vEntity['Name'];
          vRadioItem.tag := Integer(vEntity);
        end;
    end;
  finally
    FreeAndNil(vCollections);
  end;
end;

procedure TDEEnumFieldEditor.DoOnChange;
begin
  SetFieldEntity(TEntity(TPanel(FControl).Tag));
end;

procedure TDEEnumFieldEditor.FillEditor;
begin
  TPanel(FControl).Enabled := Assigned(FView.FieldEntity) and (FView.State = vsFullAccess);

  if Assigned(FView.FieldEntity) then
    TPanel(FControl).Tag := Integer(FView.FieldValue)
  else
    TPanel(FControl).Tag := -1;

end;

procedure TDEEnumFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TRadioGroup(FControl).Properties.OnChange := AHandler;
end;

{ TDECurrencyEditControl }

procedure TDECurrencyFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
var
  vDef: TSimpleFieldDef;
begin
  FControl := TSpinBox.Create(nil);

  with TSpinBox(FControl) do
  begin
    ReadOnly := FView.State < vsFullAccess;
    MaxLength := 15;
    ValueType := TNumValueType.Float;

  end;
//  TCurrencyEdit(FInnerControl).OnKeyDown := OnWinControlKeyDown;

  vDef := TSimpleFieldDef(FFieldDef);
  with TSpinBox(FControl) do
  begin
    if not VarIsNull(vDef.MaxValue) then Max := vDef.MaxValue;
    if not VarIsNull(vDef.MinValue) then Min := vDef.MinValue;
  end;
end;

procedure TDECurrencyFieldEditor.DoOnChange;
begin
  SetFieldValue(TSpinBox(FControl).Value);
end;

procedure TDECurrencyFieldEditor.FillEditor;
var
  vEdit: TSpinBox;
begin
  vEdit := TSpinBox(FControl);

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
      vEdit.TabStop := True;
  end;
end;

procedure TDECurrencyFieldEditor.SwitchChangeHandlers(
  const AHandler: TNotifyEvent);
begin
  inherited;
  TSpinBox(FControl).OnChange := AHandler;
end;

{ TDEBoolEditControl }

procedure TDEBoolFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FNeedCreateCaption := False;

  FControl := TCheckBox.Create(nil);

  if ALayout.LayoutClass = 'TPanel' then
  begin
    if TAlignment(ALayout.ExtractInteger('Alignment')) <> taCenter then
    begin
      if TAlignment(ALayout.ExtractInteger('Alignment')) = taLeftJustify then
        TCheckBox(FControl).TextSettings.HorzAlign := TTextAlign.Center
      else
        TCheckBox(FControl).TextSettings.HorzAlign := TTextAlign.Leading;
    end;
  end;

//  TCheckBox(Result).OnKeyDown := OnWinControlKeyDown;

  if Assigned(CreateParams) and (CreateParams.IndexOfName('Caption') >= 0) then
    TCheckBox(FControl).Text := CreateParams.Values['Caption']
  else
    TCheckBox(FControl).Text := GetFieldTranslation(FFieldDef);

  if Assigned(CreateParams) and (CreateParams.IndexOfName('Hint') >= 0) then
    TCheckBox(FControl).Hint := CreateParams.Values['Hint']
  else
    TCheckBox(FControl).Hint := GetFieldTranslation(FFieldDef, tpHint);
end;

procedure TDEBoolFieldEditor.DoOnChange;
begin
  SetFieldValue(TCheckBox(FControl).IsChecked);
end;

procedure TDEBoolFieldEditor.FillEditor;
begin
  if VarIsNull(FView.FieldValue) then
    TCheckBox(FControl).IsChecked := False
  else
    TCheckBox(FControl).IsChecked := FView.FieldValue;

  TCheckBox(FControl).Enabled := FView.State = vsFullAccess;
  TCheckBox(FControl).TabStop := TCheckBox(FControl).Enabled;
end;

procedure TDEBoolFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TCheckBox(FControl).OnChange := AHandler;
end;

{ TDEImageEditor

procedure TDEImageEditor.DoBeforeFreeControl;
begin
  inherited;
end;

procedure TDEImageEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TImage.Create(nil);
  TImage(FControl).Properties.GraphicClassName := 'TdxSmartImage';
  TImage(FControl).Properties.OnAssignPicture := DoOnAssignPicture;
  TImage(FControl).Properties.OnChange := DoOnChangeImage;
  if Assigned(FCreateParams) and (FCreateParams.Values['ViewState'] = 'ReadOnly') then
  begin
    TImage(FControl).Properties.ReadOnly := True;
    TImage(FControl).Properties.PopupMenuLayout.MenuItems := [];
    TImage(FControl).Properties.ShowFocusRect := False;
    TImage(FControl).Style.BorderStyle := ebsNone;
    TImage(FControl).Transparent := True;
    TImage(FControl).ParentColor := True;
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
  TImage(FControl).PostEditValue;
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
    TImage(FControl).EditValue := '';
    Exit;
  end;

  vPicture := TPicture.Create;
  vImage := TdxSmartImage.Create;
  vPicture.Graphic := vImage;
  try
    vStream.Position := 0;
    vPicture.Graphic.LoadFromStream(vStream);
    SavePicture(vPicture, vStr);
    TImage(FControl).EditValue := vStr;
  finally
    vPicture.Graphic := nil;
    FreeAndNil(vImage);
    vPicture.Free;
  end;

  TImage(FControl).Enabled := FView.State = vsFullAccess;
end;

function TDEImageEditor.GetLayoutPositionCount: Integer;
begin
  Result := 2;
end;

{ TDEMemoFieldEditor }

procedure TDEMemoFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TMemo.Create(nil);
  TMemo(FControl).OnKeyDown := OnKeyDown;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TMemo(FControl).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TDEMemoFieldEditor.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TMemo(FControl).Text);
end;

procedure TDEMemoFieldEditor.FillEditor;
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

function TDEMemoFieldEditor.GetLayoutPositionCount: Integer;
begin
  Result := 2;
end;

procedure TDEMemoFieldEditor.OnKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin

end;

procedure TDEMemoFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TMemo(FControl)) then
    TMemo(FControl).OnChange := AHandler;
end;

{ TDETimeFieldEditor }

procedure TDETimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TTimeEdit.Create(nil);
end;

procedure TDETimeFieldEditor.DoOnChange;
var
  vTime: TTime;
begin
  if MyTryStrToTime(TTimeEdit(FControl).Text, vTime) then
    SetFieldValue(Frac(vTime))
  else
    SetFieldValue(Null);
end;

procedure TDETimeFieldEditor.FillEditor;
var
  vEdit: TTimeEdit;
begin
  vEdit := TTimeEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.Time := 0;
  end
  else
  begin
    vEdit.Enabled := FView.State > vsDisabled;
    vEdit.Time := FView.FieldValue;

    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
      vEdit.TabStop := False
    else
      vEdit.TabStop := True;
  end;

end;

//function TDETimeFieldEditor.GetNewValue: Variant;
//var
//  vTime: TTime;
//begin
//  if MyTryStrToTime(TTimeEdit(FControl).EditingText, vTime) then
//    Result := Frac(vTime)
//  else
//    Result := Null;
//end;

procedure TDETimeFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TTimeEdit(FControl).OnChange := AHandler;
end;

{ TDEMaskFieldEditor

procedure TDEMaskFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
var
  vMask: string;
begin
  FControl := TMaskEdit.Create(nil);
//  TMaskEdit(FControl).OnKeyDown := OnWinControlKeyDown;
  TMaskEdit(FControl).Properties.ValidationOptions := [evoShowErrorIcon, evoAllowLoseFocus];

  vMask := '';
  if Assigned(FFieldDef)then
  begin
    TMaskEdit(FControl).Properties.MaskKind := emkStandard;
    if SameText('phone', FFieldDef.StyleName) then
      vMask := '!\+9 (999\) 000-00-00;1;_'
    else if SameText('INN', FFieldDef.StyleName) then
      vMask := '000000000009;0;_'
    else if SameText('email', FFieldDef.StyleName) then
    begin
      TMaskEdit(FControl).Properties.MaskKind := emkRegExpr;
      vMask := '[\w\-.]+@[\w\-]+(\.[\w\-]+)+ ';
    end
    else if SameText('url', FFieldDef.StyleName) then
    begin
      TMaskEdit(FControl).Properties.MaskKind := emkRegExprEx;
      vMask := 'http\:\/\/(\w+(\.\w+)*@)?\w+\.\w+(\.\w+)*(/(\w+(/\w+)*/?)?)?';
    end;
  end;
  TMaskEdit(FControl).Properties.EditMask := vMask;
end;

procedure TDEMaskFieldEditor.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TMaskEdit(FControl).EditingText);
end;

procedure TDEMaskFieldEditor.FillEditor;
var
  vEdit: TMaskEdit;
begin
  vEdit := TMaskEdit(FControl);
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
      vEdit.TabStop := True;
    end;
  end;
end;

procedure TDEMaskFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TMaskEdit(FControl).Properties) then
    TMaskEdit(FControl).Properties.OnChange := AHandler;
end;

{ TMRUFieldEditor

procedure TMRUFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TMRUEdit.Create(nil);
  TMRUEdit(FControl).Properties.ShowEllipsis := False;
end;

procedure TMRUFieldEditor.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TMRUEdit(FControl).EditingText);
end;

procedure TMRUFieldEditor.FillEditor;
var
  vEdit: TMRUEdit;
  vCollections: TList<TCollection>;
  vCollection: TCollection;
  vEntity: TEntity;
  vText: string;
begin
  vEdit := TMRUEdit(FControl);
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
      vEdit.TabStop := True;
    end;
  end;
end;

procedure TMRUFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TMRUEdit(FControl).Properties) then
    TMRUEdit(FControl).Properties.OnChange := AHandler;
end;

{ TColorEditor

procedure TColorEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FColorDialog);
  FreeAndNil(FSelectBtn);
  FreeAndNil(FClearBtn);
  FreeAndNil(FPaintBox);
end;

procedure TColorEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FBasePanel := TPanel.Create(nil);
  FBasePanel.BevelOuter := bvNone;
  FControl := FBasePanel;

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
  if FColorDialog.Execute({TWinControl(FInnerControl).Handle D7) then
  begin
    SetFieldValue(FColorDialog.Color);
    FPaintBox.Invalidate;
  end;
end;

{ TDEDateTimeFieldEditor

procedure TDEDateTimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
var
  vDef: TSimpleFieldDef;
begin
  FControl := TDateEdit.Create(nil);

  with TDateEdit(FControl) do
  begin
    Properties.ImmediatePost := True;
    Properties.Kind := ckDateTime;
    Properties.DateButtons := [btnNow, btnClear];
    Properties.SaveTime := True;
    Properties.ShowTime := True;
    Properties.DateOnError := deNull;
  end;
//  TDateEdit(FInnerControl).OnKeyDown := OnWinControlKeyDown;

  vDef := TSimpleFieldDef(FFieldDef);
  with TDateEdit(FControl) do
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
  vDate := StrToDateTimeDef(TDateEdit(FControl).EditingText, 0);
  SetFieldValue(vDate);
end;

procedure TDEDateTimeFieldEditor.FillEditor;
var
  vEdit: TDateEdit;
  vDate: TDateTime;
begin
  vEdit := TDateEdit(FControl);

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
      vEdit.TabStop := True;
    end;
  end;

  ToggleButtons;
end;

function TDEDateTimeFieldEditor.GetNewValue: Variant;
begin
  Result := StrToDateTimeDef(TDateEdit(FControl).EditingText, 0);
end;

procedure TDEDateTimeFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TDateEdit(FControl).Properties.OnChange := AHandler;
end;

{ TFilenameFieldEditor

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

procedure TFilenameFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
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

  FBtn := TButton.Create(nil);
  FBtn.Align := alRight;
  FBtn.Parent := vBase;
  FBtn.Width := 40;
  FBtn.OptionsImage.Images := TDragImageList(TInteractor(Interactor).Images[16]);
  FBtn.Action := FAction;

  FText := TTextEdit.Create(nil);
  FText.Align := alClient;
  FText.Parent := vBase;

  FControl := vBase;
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
      FText.TabStop := True;
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

{ TSelectFolderFieldEditor

procedure TSelectFolderFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FAction);
end;

procedure TSelectFolderFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
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

  FBtn := TButton.Create(nil);
  FBtn.Align := alRight;
  FBtn.Parent := vBase;
  FBtn.Width := 40;
  FBtn.OptionsImage.Images := TDragImageList(TInteractor(Interactor).Images[16]);
  FBtn.Action := FAction;

  FText := TTextEdit.Create(nil);
  FText.Align := alClient;
  FText.Parent := vBase;

  FControl := vBase;
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
      FText.TabStop := True;
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

{ TSpinner

procedure TSpinner.AssignFromLayout(const ALayout: TBaseLayout);
var
  vPanel: TCrackedControl absolute ALayout;
  vColor: Cardinal;
  vR, vG, vB: Byte;
  vRGBColor: Integer;
begin
  inherited;

  if (ALayout.LayoutClass = 'TPanel') or (ALayout.LayoutClass = 'TMemo') then
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

procedure TSpinner.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TdxActivityIndicator.Create(nil);
  TdxActivityIndicator(FControl).Transparent := True;
  Control.Visible := False;
  if Assigned(FCreateParams) then
  begin
    if FCreateParams.Values['type'] = 'GravityDots' then
      TdxActivityIndicator(FControl).PropertiesClassName := 'TdxActivityIndicatorGravityDotsProperties'
    else if FCreateParams.Values['type'] = 'ElasticCircle' then
    begin
      TdxActivityIndicator(FControl).PropertiesClassName := 'TdxActivityIndicatorElasticCircleProperties';
      TdxActivityIndicatorElasticCircleProperties(TdxActivityIndicator(FControl).Properties).ArcThickness := StrToIntDef(FCreateParams.Values['ArcThickness'], 3);
    end
    else
    begin
      TdxActivityIndicatorHorizontalDotsProperties(TdxActivityIndicator(FControl).Properties).DotSize := StrToIntDef(FCreateParams.Values['DotSize'], 5);
    end;


  end;

end;

procedure TSpinner.FillEditor;
begin
  inherited;
  Control.Visible := FView.FieldValue > 0;
  TdxActivityIndicator(FControl).Active := Control.Visible;
end;

{ TImageByString

procedure TImageByString.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TImage.Create(nil);
  TImage(FControl).Transparent := True;
  TImage(FControl).Center := True;
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

procedure TTextSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TComboBox.Create(nil);

  TComboBox(FControl).OnPopup := CBOnPopup;
end;

procedure TTextSelector.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TComboBox(FControl).Selected.Text);
end;

procedure TTextSelector.FillEditor;
var
  vEdit: TComboBox;
begin
  FillList;

  vEdit := TComboBox(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.ItemIndex := -1;
    vEdit.Enabled := False;
  end
  else
  begin
    vEdit.ItemIndex := FView.FieldValue;
    vEdit.Enabled := True;

      // в смена DropDownListStyle ломает отображение текущего значения, оно сбрасывается в пустоту и запись нового значения не отрабатывает
{    if FView.State < vsFullAccess then
      vEdit.Properties.DropDownListStyle := lsFixedList
    else
      vEdit.Properties.DropDownListStyle := lsEditFixedList;
}
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
  TComboBox(FControl).Items.BeginUpdate;
  TComboBox(FControl).Items.Clear;

{  if FFieldDef.StyleName = 'comport' then
  begin
    vComPorts := TStringList.Create;
    EnumComPorts(vComPorts);
    for i := 0 to vComPorts.Count - 1 do
      TComboBox(FControl).Properties.Items.Add(vComPorts[i]);
    FreeAndNil(vComPorts);
    TComboBox(FControl).EditValue := FView.FieldValue;
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
          TComboBox(FControl).Items.Add(vCollection[i].FieldValues[vFieldName])
        else
          TComboBox(FControl).Items.Add(vCollection[i]['Name']);

      TComboBox(FControl).ItemIndex := FView.FieldValue;
    end;
  end;
  TComboBox(FControl).Items.EndUpdate;
end;

procedure TTextSelector.CBOnPopup(Sender: TObject);
begin
  FillList;
end;

procedure TTextSelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TComboBox(FControl)) then
    TComboBox(FControl).OnChange := AHandler;
end;

{ TDEEditor }

{type
  TEditCrack = class(TCustomEdit);

procedure TDEEditor.ToggleButtons;
var
  i: Integer;
  vEdit: TEditCrack;
  vVisible: Boolean;
begin
  if FControl is TCustomEdit then
  begin
    vEdit := TEditCrack(FControl);
    vVisible := vEdit.Visible and vEdit.Enabled and (not vEdit.Properties.ReadOnly);

    if vVisible then
    begin
      i := 0;
      while i < TCustomEditProperties(vEdit.Properties).Buttons.Count do
      begin
        TCustomEditProperties(vEdit.Properties).Buttons.Items[i].Visible := True;
        Inc(i);
      end;
     // for i := 0 to TCustomEditProperties(vEdit.Properties).Buttons.Count - 1 do
     //   TCustomEditProperties(vEdit.Properties).Buttons.Items[i].Visible := True;
    end
    else
      for i := TCustomEditProperties(vEdit.Properties).Buttons.Count - 1 downto 0 do
        TCustomEditProperties(vEdit.Properties).Buttons.Items[i].Visible := False;
  end;
end;     }

{ TDEBLOBEditor

procedure TDEBLOBEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TBLOBEdit.Create(nil);
  TBLOBEdit(FControl).AutoSize := False;
  TBLOBEdit(FControl).Properties.Buttons.Clear;
  TBLOBEdit(FControl).Properties.OnChange := DoOnChangeBLOB;
end;

procedure TDEBLOBEditor.DoOnChangeBLOB(Sender: TObject);
begin
  TBLOBEdit(FControl).PostEditValue;
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
    TBLOBEdit(FControl).EditValue := '';
    Exit;
  end;

  vStream.Position := 0;
  SetLength(vValue, vStream.Size);
  vStream.ReadBuffer(vValue[1], vStream.Size);

  TBLOBEdit(FControl).EditValue := vValue;
  TBLOBEdit(FControl).Enabled := FView.State = vsFullAccess;
end;

{ TDEEnumEditor }

procedure TDEEnumEditor.CBOnPopup(Sender: TObject);
begin
  FillList;
end;

procedure TDEEnumEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  if not Assigned(FEnum) then
    FEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);

  FControl := TComboBox.Create(nil);
  TComboBox(FControl).OnPopup := CBOnPopup;
end;

procedure TDEEnumEditor.DoOnChange;
begin
  if FFieldDef.HasFlag(cRequired) then
    SetFieldValue(TComboBox(FControl).ItemIndex + 1)
  else
    SetFieldValue(TComboBox(FControl).ItemIndex);
end;

procedure TDEEnumEditor.FillEditor;
var
  vEdit: TComboBox;
  vItemIndex: Integer;
begin
  FillList;

  vEdit := TComboBox(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.ItemIndex := -1; //FEnum.Items[0].DisplayText;
    vEdit.Enabled := False;
  end
  else begin
    vItemIndex := FView.FieldValue;
    if FFieldDef.HasFlag(cRequired) then
      vItemIndex := vItemIndex - 1;
    vEdit.ItemIndex := vItemIndex;

    vEdit.Enabled := not (FView.State < vsSelectOnly);
  end;
end;

procedure TDEEnumEditor.FillList;
var
  vEnumItem: TEnumItem;
  vItems: TStrings;
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

procedure TDEEnumEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  if Assigned(TComboBox(FControl)) then
    TComboBox(FControl).OnChange := AHandler;
end;

{ TDEGraphicEnumSelector

procedure TDEGraphicEnumSelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TDEGraphicEnumSelector.CBOnDrawItem(AControl: TCustomComboBox; ACanvas: TCanvas; AIndex: Integer;
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

procedure TDEGraphicEnumSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  if not Assigned(FEnum) then
    FEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);

  FControl := TComboBox.Create(nil);
  TComboBox(FControl).Properties.DropDownListStyle := lsFixedList;
  TComboBox(FControl).Properties.OnInitPopup := CBOnInitPopup;
  TComboBox(FControl).Properties.OnDrawItem := CBOnDrawItem;
end;

procedure TDEGraphicEnumSelector.DoDrawItem(const ACanvas: TCanvas; const AID: Integer; const ARect: TRect;
  AState: TOwnerDrawState);
begin
end;

procedure TDEGraphicEnumSelector.DoOnChange;
begin
  if FFieldDef.HasFlag(cRequired) then
    SetFieldValue(TComboBox(FControl).ItemIndex + 1)
  else
    SetFieldValue(TComboBox(FControl).ItemIndex);
end;

procedure TDEGraphicEnumSelector.FillEditor;
var
  vEdit: TComboBox;
  vItemIndex: Integer;
begin
  FillList;

  vEdit := TComboBox(FControl);
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
      vEdit.TabStop := True;
    end;
  end;

  ToggleButtons;
end;

procedure TDEGraphicEnumSelector.FillList;
var
  vEnumItem: TEnumItem;
  vItems: TStrings;
begin
  vItems := TComboBox(FControl).Properties.Items;

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
  if Assigned(TComboBox(FControl).Properties) then
    TComboBox(FControl).Properties.OnChange := AHandler;
end;

{ TDELineStyleSelector

procedure TDELineStyleSelector.DoDrawItem(const ACanvas: TCanvas; const AID: Integer; const ARect: TRect;
  AState: TOwnerDrawState);
begin
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := Graphics.TPenStyle(AID);

  ACanvas.MoveTo(ARect.Left + 8, ARect.CenterPoint.Y);
  ACanvas.LineTo(ARect.Right - 8, ARect.CenterPoint.Y);
end;

{ TDEPagesFieldEditor }

procedure TDEPagesFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
var
  vPanel: TPanel absolute ALayout;
  vPC: TTabControl;
  vSourceTab: TBaseLayout;
  vPage: TTabItem;
  vChildArea: TFMXArea;
  i: Integer;
begin
  FNeedCreateCaption := False;

  inherited;

  vPC := TTabControl.Create(nil);
  FControl := vPC;
  vPC.Width := ALayout.Width;
  vPC.Height := ALayout.Height;

  vPC.Align := AlignToAlignLayout(ALayout.Align);
  vPC.Margins.Create(ALayout.Margins);
  vPC.Anchors := ALayout.Anchors;
  vPC.Position.X := ALayout.Left;
  vPC.Position.Y := ALayout.Top;

//  vPC.HideTabs := not ALayout.ExtractBoolean('ShowHint') or ((ALayout.Childs.Count > 0) and not ALayout.Childs[0].ExtractBoolean('TabVisible'));
  vPC.TabHeight := ALayout.ExtractInteger('TabHeight');
//  vPC.TabPosition := TTabPosition(ALAyout.ExtractInteger('TabPosition'));


  // Нужно прописывать родителя, чтобы создавать вложенные сцены
  vPC.Parent := TFMXArea(AParent).Control;

  for i := 0 to ALayout.Childs.Count - 1 do
  begin
    vSourceTab := ALayout.Childs[i];
    vPage := TTabItem.Create(vPC);
    vPage.Text := vSourceTab.Caption;
    vPage.ImageIndex := vSourceTab.ExtractInteger('ImageIndex');

    vChildArea := TFMXArea.Create(Self, FView.Parent, vSourceTab.Name, False, vPage);
    AddArea(vChildArea);
    TInteractor(FView.Interactor).UIBuilder.CreateChildAreas(vChildArea, vSourceTab, '');
  end;
  ALayout.Childs.Clear;
end;

procedure TDEPagesFieldEditor.DoOnChange;
var
  vTag: Integer;
begin
  vTag := TTabControl(FControl).ActiveTab.Index;
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
    SetFieldValue(vTag);
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
      if TFieldDef(FView.Definition).Flags or cRequired = 1 then
        vValue := vValue - 1;
      vTag := vValue
    end
    else if TFieldDef(FView.Definition).Kind = fkInteger then
      vTag := vValue
    else
      vTag := -1;
  end;

  if (vTag < TTabControl(FControl).TabCount) and (TTabControl(FControl).TabIndex <> vTag) then
    TTabControl(FControl).TabIndex := vTag;
end;

procedure TDEPagesFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TTabControl(FControl).OnChange := AHandler;
end;

{ TProgress

procedure TProgress.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TProgressBar.Create(nil);
  TProgressBar(FControl).AutoSize := ALayout.ExtractBoolean('AutoSize');
  TProgressBar(FControl).Properties.SolidTextColor := True;
  TProgressBar(FControl).Properties.ShowText := ALayout.ExtractBoolean('ShowCaption');
  FNeedCreateCaption := False;
  TProgressBar(FControl).Properties.Max := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TProgress.FillEditor;
begin
  inherited;
  TProgressBar(FControl).Position := FView.FieldValue;
end;

{ TEntityBreadcrumb

procedure TEntityBreadcrumb.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TdxBreadcrumbEdit.Create(nil);
  TdxBreadcrumbEdit(FControl).Properties.PathEditor.PathDelimiter := '.';
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

procedure TDEFlagsEditor.LBOnClickCheck(Sender: TObject);
begin
  OnChange(Sender);
end;

procedure TDEFlagsEditor.LBOnItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  Item.IsChecked := not Item.IsChecked;
  Item.IsSelected := False;
end;

procedure TDEFlagsEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  FControl := TListBox.Create(nil);
  TListBox(FControl).ShowCheckboxes := true;
  TListBox(FControl).OnItemClick := LBOnItemClick;
  TListBox(FControl).OnChangeCheck := LBOnClickCheck;
end;

procedure TDEFlagsEditor.DoOnChange;
var
  i: Integer;
  vFlagsValue: Integer;
  vListItem: TListBoxItem;
begin
  vFlagsValue := 0;
  for i := 0 to TListBox(FControl).Items.Count - 1 do
  begin
    vListItem := TListBox(FControl).ListItems[i];
    if vListItem.IsChecked then
      vFlagsValue := vFlagsValue or TEnumItem(vListItem.Data).ID;
  end;

  SetFieldValue(vFlagsValue);
end;

procedure TDEFlagsEditor.FillEditor;
var
  vList: TListBox;
begin
  FillList;

  vList := TListBox(FControl);
  vList.ShowCheckboxes := FView.State < vsSelectOnly;
  vList.Enabled := not vList.ShowCheckboxes;

  if vList.ShowCheckboxes then
    vList.TabStop := False
  else
    vList.TabStop := True;
end;

procedure TDEFlagsEditor.FillList;
var
  vList: TListBox;
  vEnumItem: TEnumItem;
  vListItem: TListBoxItem;
  vValue: Integer;
begin
  vList := TListBox(FControl);
  vValue := FView.FieldValue;
  vList.Items.BeginUpdate;
  try
    vList.Items.Clear;
    for vEnumItem in FEnum do
    begin
      if vEnumItem.ID > 0 then
      begin
        vListItem := TListBoxItem.Create(vList);
        vListItem.Text := vEnumItem.DisplayText;
        vListItem.Data := vEnumItem;
        vListItem.IsChecked := (vEnumItem.ID and vValue) <> 0;
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
  begin
    TListBox(FControl).OnItemClick := LBOnItemClick;
    TListBox(FControl).OnChangeCheck := LBOnClickCheck;
  end
  else begin
    TListBox(FControl).OnItemClick := nil;
    TListBox(FControl).OnChangeCheck := nil;
  end;
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

procedure TDEImagedAction.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
var
  vActionName: string;
  vImageSize: Integer;
  vButton: TButton;
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

  vButton := TButton.Create(nil);
  vButton.Images := TImageList(TInteractor(Interactor).Images[vImageSize]);
  vButton.OnClick := OnButtonClick;

  FControl := vButton;
end;

procedure TDEImagedAction.FillEditor;
begin
  if VarIsNull(FView.FieldValue) or (not FView.FieldValue) then
  begin
    TButton(FControl).ImageIndex := FFalseImageID;
    TButton(FControl).Hint := FTrueHint;
  end
  else
  begin
    TButton(FControl).ImageIndex := FTrueImageID;
    TButton(FControl).Hint := FFalseHint;
  end;

  TButton(FControl).Enabled := FView.State = vsFullAccess;
end;

procedure TDEImagedAction.OnButtonClick(Sender: TObject);
begin
  if Assigned(FActionView) then
    FActionView.ExecuteAction(Holder);
end;

procedure TDEImagedAction.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  //TButton(Result).OnClick := AHandler;
end;

{ TBoolImages

procedure TBoolImages.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TImage.Create(nil);
  TImage(FControl).Transparent := True;
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

{ TDEColorEditor

procedure TDEColorEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  FControl := TdxColorEdit.Create(nil);
  TdxColorEdit(FControl).Properties.ColorPalette := TdxColorPalette.cpExtended;
  TdxColorEdit(FControl).Properties.ColorSet := TdxColorSet.csDefault;
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
      vEdit.TabStop := True;
    end;
  end;
end;

procedure TDEColorEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TdxColorEdit(FControl).Properties.OnChange := AHandler;
end;

{ TGauge

procedure TGauge.DoBeforeFreeControl;
begin
  inherited;
 // FGaugeControl.Parent := nil;
 // FreeAndNil(FGaugeControl1CircularHalfScale);
 // FreeAndNil(FGaugeControl);
end;

procedure TGauge.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
var
  vRange: TdxGaugeCircularScaleRange;
  vMax, vMin: Integer;
begin
  inherited;
  FGaugeControl := TdxGaugeControl.Create(nil);
  if Assigned(AParent.Control) and (AParent.Control is TWinControl) then
    FGaugeControl.Parent := TWinControl(AParent.Control);
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

  FControl := FGaugeControl;

end;

procedure TGauge.FillEditor;
begin
  inherited;
  FGaugeControl1CircularHalfScale.Value := FView.FieldValue;
end;

{ TIntegerFlagsEditor }

procedure TIntegerFlagsEditor.LBOnClickCheck(Sender: TObject);
begin
  OnChange(Sender);
end;

procedure TIntegerFlagsEditor.LBOnItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
  Item.IsChecked := not Item.IsChecked;
  Item.IsSelected := False;
end;

procedure TIntegerFlagsEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FCaptions);
end;

procedure TIntegerFlagsEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
var
  vHorzLayout: Boolean;
begin
  FControl := TListBox.Create(nil);
  TListBox(FControl).OnChangeCheck := LBOnClickCheck;
  TListBox(FControl).OnItemClick := LBOnItemClick;

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
    TListBox(FControl).Columns := FDisplayFlagCount;
end;

procedure TIntegerFlagsEditor.DoOnChange;
var
  i: Integer;
  vFlagsValue: Integer;
  vListItem: TListBoxItem;
begin
  vFlagsValue := 0;
  for i := 0 to TListBox(FControl).Items.Count - 1 do
  begin
    vListItem := TListBox(FControl).ListItems[i];
    if vListItem.IsChecked then
      vFlagsValue := vFlagsValue or Integer(vListItem.Data);
  end;

  SetFieldValue(vFlagsValue);
end;

procedure TIntegerFlagsEditor.FillEditor;
var
  vList: TListBox;
begin
  FillList;

  vList := TListBox(FControl);
  vList.ShowCheckboxes := FView.State < vsSelectOnly;
  vList.Enabled := not vList.ShowCheckboxes;

  if vList.ShowCheckboxes then
    vList.TabStop := False
  else
    vList.TabStop := True;
end;

procedure TIntegerFlagsEditor.FillList;
var
  vList: TListBox;
  vListItem: TListBoxItem;
  vBits, vBit: Integer;
  i: Integer;
begin
  vList := TListBox(FControl);
  vBits := FView.FieldValue;
  try
    vList.Items.Clear;
    for i := 0 to FDisplayFlagCount - 1 do
    begin
      vListItem := TListBoxItem.Create(vList);
      if Assigned(FCaptions) and (i < FCaptions.Count) then
        vListItem.Text := FCaptions[i]
      else
        vListItem.Text := IntToStr(i);
      vBit := 1 shl i;
      vListItem.Data := TObject(vBit);
      vListItem.IsChecked := (vBit and vBits) <> 0;
    end;
  finally
  end;
end;

procedure TIntegerFlagsEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  if Assigned(AHandler) then
  begin
    TListBox(FControl).OnChangeCheck := LBOnClickCheck;
    TListBox(FControl).OnItemClick := LBOnItemClick;
  end
  else
  begin
    TListBox(FControl).OnChangeCheck := LBOnClickCheck;
    TListBox(FControl).OnItemClick := LBOnItemClick;
  end;
end;

{ TLogEditor

procedure TLogEditor.DoBeforeFreeControl;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TLogEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  inherited;
  FListView := TListView.Create(nil);
  FListView.OnData := OnListViewData;
  FListView.OwnerData := True;
  FListView.Columns.Add.AutoSize := True;
  FListView.ShowColumnHeaders := False;
  FListView.ReadOnly := True;

  FData := TStringList.Create;

  FControl := FListView;
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
end;   }

initialization

RegisterClasses([{TBevel,}TLabel{, TTreeList, TTreeListColumn}]);
{ dx localization
cxSetResourceString(@cxSDateThursday, 'Donderdag'); //=Thursday
cxSetResourceString(@cxSDatePopupToday, 'Vandaag');//= 'Today';}

TPresenter.RegisterUIClass('FMX', uiTextEdit, '', TDETextFieldEditor);
TPresenter.RegisterUIClass('FMX', uiTextEdit, 'phone', TDETextFieldEditor);
//TPresenter.RegisterUIClass('FMX', uiTextEdit, 'email', TDEMaskFieldEditor);
//TPresenter.RegisterUIClass('FMX', uiTextEdit, 'url', TDEMaskFieldEditor);
//TPresenter.RegisterUIClass('FMX', uiTextEdit, 'INN', TDEMaskFieldEditor);
TPresenter.RegisterUIClass('FMX', uiTextEdit, 'memo', TDEMemoFieldEditor);
//TPresenter.RegisterUIClass('FMX', uiTextEdit, 'log', TLogEditor);
TPresenter.RegisterUIClass('FMX', uiTextEdit, 'info', TTextInfo);
//TPresenter.RegisterUIClass('FMX', uiTextEdit, 'mru', TMRUFieldEditor);
//TPresenter.RegisterUIClass('FMX', uiTextEdit, 'dir', TSelectFolderFieldEditor);
//TPresenter.RegisterUIClass('FMX', uiTextEdit, 'file', TFilenameFieldEditor);
//TPresenter.RegisterUIClass('FMX', uiTextEdit, 'ImageByString', TImageByString);
TPresenter.RegisterUIClass('FMX', uiTextEdit, 'selector', TTextSelector);
TPresenter.RegisterUIClass('FMX', uiTextEdit, 'comport', TTextSelector);
//TPresenter.RegisterUIClass('FMX', uiTextEdit, 'fieldpath', TEntityBreadcrumb);
TPresenter.RegisterUIClass('FMX', uiIntegerEdit, '', TDEIntegerFieldEditor);
TPresenter.RegisterUIClass('FMX', uiIntegerEdit, 'info', TTextInfo);
//TPresenter.RegisterUIClass('FMX', uiIntegerEdit, 'spinner', TSpinner);
//TPresenter.RegisterUIClass('FMX', uiIntegerEdit, 'progress', TProgress);
//TPresenter.RegisterUIClass('FMX', uiIntegerEdit, 'gauge', TGauge);
TPresenter.RegisterUIClass('FMX', uiIntegerEdit, 'pages', TDEPagesFieldEditor);
TPresenter.RegisterUIClass('FMX', uiIntegerEdit, 'flags', TIntegerFlagsEditor);
TPresenter.RegisterUIClass('FMX', uiEnumEdit, '', TDEEnumEditor);
TPresenter.RegisterUIClass('FMX', uiEnumEdit, 'radio', TDEEnumEditor);
TPresenter.RegisterUIClass('FMX', uiEnumEdit, 'info', TTextInfo);
//TPresenter.RegisterUIClass('FMX', uiEnumEdit, 'line_style', TDELineStyleSelector);
TPresenter.RegisterUIClass('FMX', uiEnumEdit, 'pages', TDEPagesFieldEditor);
TPresenter.RegisterUIClass('FMX', uiFlagEdit, '', TDEFlagsEditor);
//TPresenter.RegisterUIClass('FMX', uiEntityEdit, 'enum', TDEEnumFieldEditor);
//TPresenter.RegisterUIClass('FMX', uiFloatEdit, '', TDEFloatFieldEditor);
//TPresenter.RegisterUIClass('FMX', uiFloatEdit, 'currency_rate', TDEFloatFieldEditor);
TPresenter.RegisterUIClass('FMX', uiFloatEdit, 'info', TTextInfo);
//TPresenter.RegisterUIClass('FMX', uiFloatEdit, 'gauge', TGauge);
TPresenter.RegisterUIClass('FMX', uiDateEdit, '', TDEDateFieldEditor);
TPresenter.RegisterUIClass('FMX', uiDateEdit, 'time', TDETimeFieldEditor);
//TPresenter.RegisterUIClass('FMX', uiDateEdit, 'datetime', TDEDateTimeFieldEditor);
TPresenter.RegisterUIClass('FMX', uiDateEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('FMX', uiCurrencyEdit, '', TDECurrencyFieldEditor);
TPresenter.RegisterUIClass('FMX', uiCurrencyEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('FMX', uiBoolEdit, '', TDEBoolFieldEditor);
TPresenter.RegisterUIClass('FMX', uiBoolEdit, 'simple', TDEBoolFieldEditor);
TPresenter.RegisterUIClass('FMX', uiBoolEdit, 'imaged_action', TDEImagedAction);
//TPresenter.RegisterUIClass('FMX', uiBoolEdit, 'images', TBoolImages);
TPresenter.RegisterUIClass('FMX', uiBoolEdit, 'pages', TDEPagesFieldEditor);
//TPresenter.RegisterUIClass('FMX', uiColorEdit, '', TColorEditor);
//TPresenter.RegisterUIClass('FMX', uiColorEdit, 'simple', TDEColorEditor);
//TPresenter.RegisterUIClass('FMX', uiBLOBEdit, '', TDEBLOBEditor);
//TPresenter.RegisterUIClass('FMX', uiBLOBEdit, 'image', TDEImageEditor);
TPresenter.RegisterUIClass('FMX', uiEntityEdit, 'info', TTextInfo);

end.
