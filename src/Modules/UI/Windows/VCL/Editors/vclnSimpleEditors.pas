{ ---------------------------------------------------------------------------------
  X-Tend runtime

  Contributors:
  Vladimir Kustikov (kustikov@sensoft.pro)
  Sergey Arlamenkov (arlamenkov@sensoft.pro)

  You may retrieve the latest version of this file at the GitHub,
  located at https://github.com/sensoftpro/x-tend.git
  ---------------------------------------------------------------------------------
  MIT License

  Copyright © 2021 Sensoft

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
  --------------------------------------------------------------------------------- }

unit vclnSimpleEditors;

interface

uses
  Classes, Graphics, vcl.ExtCtrls, vcl.StdCtrls, vcl.Dialogs, vcl.Controls, ActnList, StdActns, vcl.Forms,
  // VCL
  Spin, Mask, Menus,

  vclnArea, uDefinition, uEnumeration, uUIBuilder, uView;


type
  TTextInfo = class(TVCLFieldArea)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
  end;

  TDEEditor = class(TVCLFieldArea)
  protected
    function GetDisabledBorderStyle: TBorderStyle;

  end;

  TDEEnumEditor = class(TDEEditor)
  private
    FEnum: TEnumeration;
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEFlagsEditor = class(TDEEditor)
  private
    FEnum: TEnumeration;
    procedure FillList;
    procedure CLBOnClickCheck(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    function GetLayoutPositionCount: Integer; override;
  end;

  TDEIntegerFieldEditor = class(TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure FocusedChanged(const AFocused: Boolean); override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEFloatFieldEditor = class(TDEEditor)
  private
    FAfterPoint: Integer; // знаков после запятой
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDECurrencyFieldEditor = class(TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEDateFieldEditor = class(TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    function GetNewValue: Variant;
  end;

  TDETimeFieldEditor = class(TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEDateTimeFieldEditor = class(TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDETextFieldEditor = class(TDEEditor)
  private
    procedure OnPhoneKeyPress(Sender: TObject; var Key: Char);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TTextSelector = class(TDEEditor)
  private
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TSelectFolderFieldEditor = class(TDEEditor)
  private
    FText: TEdit;
    FBtn: TButton;
    FAction: TBrowseForFolder;
    procedure BrowseForFolder1Accept(Sender: TObject);
    procedure BeforeExecute(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFilenameFieldEditor = class(TDEEditor)
  private
    FBtn: TButton;
    FAction: TFileOpen;
    FText: TEdit;
    procedure OnAccept(Sender: TObject);
    procedure BeforeExecute(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  public
    property TextEdit: TEdit read FText;
  end;

  TDEMaskFieldEditor = class(TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TMRUFieldEditor = class(TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEMemoFieldEditor = class(TDEEditor)
  private
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    function GetLayoutPositionCount: Integer; override;
  end;

  TDELogFieldEditor = class(TDEMemoFieldEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
  end;

  TDEBoolFieldEditor = class(TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
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
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEPagesFieldEditor = class(TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEEnumFieldEditor = class(TDEEditor)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TDEImageEditor = class(TDEEditor)
  private
    FBasePanel, FBtnPanel: TPanel;
    FImage: TImage;
    FLoadBtn, FClearBtn, FSaveBtn: TButton;
    procedure OnLoadBtnClick(Sender: TObject);
    procedure OnClearBtnClick(Sender: TObject);
    procedure OnSaveBtnClick(Sender: TObject);
    // procedure DoOnAssignPicture(Sender: TObject; Picture: TPicture);
    // procedure DoOnChangeImage(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    function GetLayoutPositionCount: Integer; override;
  end;

//  TDEBLOBEditor = class(TDEEditor)
//  private
//    procedure DoOnChangeBLOB(Sender: TObject);
//  protected
//    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
//    procedure FillEditor; override;
//  end;

  TImageByString = class(TVCLFieldArea)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
  end;

  TBoolImages = class(TVCLFieldArea)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
  end;

  TColorEditor = class(TVCLFieldArea)
  private
    FBasePanel: TPanel;
    FColorDialog: TColorDialog;
    FSelectBtn, FClearBtn: TButton;
    FPaintBox: TPaintBox;
    procedure OnSelectBtnClick(Sender: TObject);
    procedure OnClearBtnClick(Sender: TObject);
    procedure OnPaint(Sender: TObject);
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
  end;

  // неопределённый по времени процесс
  TSpinner = class(TVCLFieldArea)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
  end;

  // определённый по времени процесс
  TProgress = class(TVCLFieldArea)
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure FillEditor; override;
  end;

//  TEntityBreadcrumb = class(TVCLFieldArea)
//  protected
//    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
//    procedure FillEditor; override;
//    procedure DoOnChange; override;
//    procedure SetValidateDefinition(const ADefinition: TDefinition);
//  end;

const
  NullDate = -700000;

implementation

uses
  Generics.Collections, TypInfo, Variants, SysUtils, Windows,
  ComCtrls, Math, DateUtils, Messages, WinXCtrls, CheckLst, ExtDlgs,
//  cxBlobEdit, // Не работает
//  dxBreadcrumbEdit, // Не работает

  {CPort,}

  uConfiguration, uDomain, uInteractor, uPresenter, uCollection, uEntity, uConsts, uUtils;

{ TTextInfo }

procedure TTextInfo.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FControl := TLabel.Create(nil);
  TLabel(FControl).Transparent := True;
end;

procedure TTextInfo.FillEditor;
var
  vEnum: TEnumeration;
  vEntity: TEntity;
begin
  inherited;
  if VarIsNull(FView.FieldValue) then
    TLabel(FControl).Caption := ''
  else if FFieldDef.Kind = fkCurrency then
  begin
    if FFieldDef.Format <> '' then
      TLabel(FControl).Caption := FormatFloat(FFieldDef.Format, FView.FieldValue)
    else
      TLabel(FControl).Caption := FormatFloat('#,##0.00;;0', FView.FieldValue);
  end
  else if FFieldDef.Kind = fkFloat then
  begin
    if FFieldDef.Format <> '' then
      TLabel(FControl).Caption := FormatFloat(FFieldDef.Format, FView.FieldValue)
    else
      TLabel(FControl).Caption := FView.FieldValue;
  end
  else if FFieldDef.Kind = fkDateTime then
  begin
    if FFieldDef.Format <> '' then
      TLabel(FControl).Caption := FormatDateTime(FFieldDef.Format, FView.FieldValue)
    else
      TLabel(FControl).Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss', FView.FieldValue);
  end
  else if FFieldDef.Kind = fkEnum then
  begin
    vEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
    if not Assigned(vEnum) then
    begin
      vEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
      TLabel(FControl).Font.Color := TState(vEnum.Items[Integer(FView.FieldValue)]).Color;
    end;
    TLabel(FControl).Caption := vEnum.Items[Integer(FView.FieldValue)].DisplayText;
  end
  else if FFieldDef.Kind = fkObject then
  begin
    vEntity := TEntity(FView.FieldEntity);
    if not Assigned(vEntity) then
      TLabel(FControl).Caption := TObjectFieldDef(FView.Definition)._ContentDefinition._EmptyValue
    else
      TLabel(FControl).Caption := vEntity['Name'];
  end
  else
    TLabel(FControl).Caption := FView.FieldValue;
end;

{ TDEIntegerEditControl }

procedure TDEIntegerFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FControl := TSpinEdit.Create(nil);

  with TSpinEdit(FControl) do
  begin
    if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
      MaxValue := TSimpleFieldDef(FFieldDef).MaxValue;
    if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
      MinValue := TSimpleFieldDef(FFieldDef).MinValue;
  end;
end;

procedure TDEIntegerFieldEditor.DoOnChange;
begin
  if TSpinEdit(FControl).value = 0 then
    SetFieldValue(Null)
  else
    SetFieldValue(TSpinEdit(FControl).value);
end;

procedure TDEIntegerFieldEditor.FillEditor;
var
  vEdit: TSpinEdit;
begin
  vEdit := TSpinEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.value := 0;
    vEdit.Enabled := False;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.value := FView.FieldValue;
    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
    begin
      vEdit.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else
    begin
      vEdit.Color := clWindow;
      vEdit.TabStop := True;
    end;
  end;

  vEdit.Button.Visible := vEdit.Visible and vEdit.Enabled and (not vEdit.ReadOnly);
end;

procedure TDEIntegerFieldEditor.FocusedChanged(const AFocused: Boolean);
begin
  inherited;
  if not AFocused then
  begin
    // обрабатываем 0
//    TSpinEdit(FControl).Value := Integer(TSpinEdit(FControl).text);
    SetFieldValue(TSpinEdit(FControl).value);
  end;
end;

procedure TDEIntegerFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TSpinEdit(FControl).OnChange := AHandler
end;

{ TDEFloatEditControl }

procedure TDEFloatFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
var
  vFormat: string;
begin
  FControl := TEdit.Create(nil);
  FAfterPoint := 4;
  if GetUrlCommand(FFieldDef.StyleName) = 'formatted' then
  begin
    vFormat := GetUrlParam(FFieldDef.StyleName, 'format');
    FAfterPoint := Length(vFormat) - Pos('.', vFormat);
  end;

  // with TEdit(FControl) do
  // begin
  // ImmediatePost := True;
  // MaskKind := emkRegExpr;
  // EditMask := '-?\d+[' + FormatSettings.DecimalSeparator + '.,]?\d+';
  // ValidationOptions := [evoAllowLoseFocus];
  // end;
  //
  // with TEdit(FControl) do
  // begin
  // if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
  // MaxValue := TSimpleFieldDef(FFieldDef).MaxValue;
  // if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
  // MinValue := TSimpleFieldDef(FFieldDef).MinValue;
  // end;
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
  if TEdit(FControl).text = '' then
    vValue := 0
  else
    vValue := ToFloatDef(TEdit(FControl).text);

  // if (not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue)) and (vValue < TEdit(FControl).Properties.MinValue) then
  // vValue := TEdit(FControl).MinValue;
  // if (not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue)) and (vValue > TEdit(FControl).Properties.MaxValue) then
  // vValue := TEdit(FControl).MaxValue;
  SetFieldValue(vValue);
end;

procedure TDEFloatFieldEditor.FillEditor;
var
  vEdit: TEdit;
begin
  vEdit := TEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.text := '0';
    vEdit.Enabled := False;
    vEdit.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.text := floattostr(RoundTo(FView.FieldValue, -FAfterPoint));
    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
    begin
      vEdit.BorderStyle := GetDisabledBorderStyle;
      // vEdit.Style.ButtonTransparency := ebtAlways;
      vEdit.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else
    begin
      vEdit.BorderStyle := bsSingle;
      // vEdit.Style.ButtonTransparency := ebtNone;
      vEdit.Color := clWindow;
      vEdit.TabStop := True;
    end;
  end;
end;

procedure TDEFloatFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TEdit(FControl).OnChange := AHandler
end;

{ TDEDateEditControl }

procedure TDEDateFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
var
  vDef: TSimpleFieldDef;
begin
  FControl := TDateTimePicker.Create(nil);
  vDef := TSimpleFieldDef(FFieldDef);
  with TDateTimePicker(FControl) do
  begin
    if not VarIsNull(vDef.MaxValue) then
      MaxDate := vDef.MaxValue;
    if not VarIsNull(vDef.MinValue) then
      MinDate := vDef.MinValue;
  end;
end;

function MyTryStrToDate(const AValue: string; var ADate: TDateTime): Boolean;
var
  vSL: TStringList;
  i: Integer;
  vYear: Word;
begin
  Result := True;
  vSL := TStringList.Create;
  vSL.Delimiter := '.';
  vSL.DelimitedText := AValue;
  if vSL.Count <> 3 then
    Exit;

  for i := 0 to vSL.Count - 1 do
    if StrToIntDef(vSL[i], -1) = -1 then
      Exit;
  vYear := StrToInt(vSL[2]);
  try
    if vYear < 1900 then
      Exit;

    try
      ADate := EncodeDate(vYear, StrToInt(vSL[1]), StrToInt(vSL[0]));
    except
      Result := False;
    end;
    // ADate := ADate + Time; //чтобы сортировка по дате была правильней, todo: по хорошему надо чётко отделять дату и время
  finally
    vSL.free;
  end;
end;

function MyTryStrToTime(const AValue: string; var ATime: TTime): Boolean;
var
  vSL: TStringList;
begin
  Result := True;
  vSL := TStringList.Create;
  vSL.Delimiter := ':';
  vSL.DelimitedText := AValue;
  while vSL.Count < 4 do
    vSL.Add('0');
  try
    try
      ATime := EncodeTime(StrToIntDef(vSL[0], 0), StrToIntDef(vSL[1], 0), StrToIntDef(vSL[2], 0),
        StrToIntDef(vSL[3], 0));
    except
      Result := False;
    end;
    // ADate := ADate + Time; //чтобы сортировка по дате была правильней, todo: по хорошему надо чётко отделять дату и время
  finally
    vSL.free;
  end;
end;

procedure TDEDateFieldEditor.DoOnChange;
begin
  SetFieldValue(TDateTimePicker(FControl).Date);
end;

procedure TDEDateFieldEditor.FillEditor;
var
  vEdit: TDateTimePicker;
  vDate: TDateTime;
begin
  vEdit := TDateTimePicker(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.Date := 0;
  end
  else
  begin
    vDate := FView.FieldValue;
    if vDate < 2 then
      vEdit.Date := 0
    else
      vEdit.Date := vDate;
    vEdit.Enabled := False;
  end;
end;

function TDEDateFieldEditor.GetNewValue: Variant;
var
  vDate: TDateTime;
begin
  if MyTryStrToDate(datetostr(TDateTimePicker(FControl).Date), vDate) then
    Result := vDate
  else
    Result := Null;
end;

procedure TDEDateFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TDateTimePicker(FControl).OnChange := AHandler;
end;

{ TDETextEditControl }

procedure TDETextFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  inherited;
  FControl := TEdit.Create(nil);

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TEdit(FControl).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;

  if SameText('phone', FFieldDef.StyleName) then
    TEdit(FControl).OnKeyPress := OnPhoneKeyPress
  else
    TEdit(FControl).OnKeyPress := nil;
end;

procedure TDETextFieldEditor.DoOnChange;
begin
  // важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TEdit(FControl).text);
end;

procedure TDETextFieldEditor.OnPhoneKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, ['0' .. '9', #8, '-', '+', '(', ')', ' ']) then
    Exit;

  Key := #0;
end;

procedure TDETextFieldEditor.FillEditor;
var
  vEdit: TEdit;
begin
  vEdit := TEdit(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.text := '';
    vEdit.Enabled := False;
    vEdit.BorderStyle := GetDisabledBorderStyle;
    vEdit.Color := clBtnFace;
  end
  else
  begin
    vEdit.text := FView.FieldValue;
    vEdit.Enabled := True;
    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
    begin
      vEdit.BorderStyle := GetDisabledBorderStyle;
      vEdit.TabStop := False;
      vEdit.Color := clBtnFace;
    end
    else
    begin
      vEdit.BorderStyle := bsSingle;
      vEdit.TabStop := True;
      vEdit.Color := clWindow;
    end;
  end;
end;

procedure TDETextFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TEdit(FControl)) then
    TEdit(FControl).OnChange := AHandler;
end;

{ TDEEnumEditControl }

procedure TDEEnumFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
var
  vCollections: TList<TCollection>;
  vCollection: TCollection;
  vEntity: TEntity;
begin
  FControl := TRadioGroup.Create(nil);
  // TcxRadioGroup(Result).OnKeyDown := OnWinControlKeyDown;

  vCollections := TDomain(FView.Domain).CollectionsByDefinition(TEntityFieldDef(FFieldDef)._ContentDefinition);
  try
    for vCollection in vCollections do
    begin
      for vEntity in vCollection do
        if not vEntity.IsNew then
          TRadioGroup(FControl).Items.AddObject(vEntity['Name'], TObject(vEntity));
    end;
  finally
    FreeAndNil(vCollections);
  end;

  TRadioGroup(FControl).Caption := '';
end;

procedure TDEEnumFieldEditor.DoOnChange;
begin
  SetFieldEntity(TEntity(Integer(TRadioGroup(FControl).Items.Objects[TRadioGroup(FControl).itemIndex])));
end;

procedure TDEEnumFieldEditor.FillEditor;
begin
  if Assigned(FView.FieldEntity) then
    TRadioGroup(FControl).Items.Objects[TRadioGroup(FControl).itemIndex] := TObject(Integer(FView.FieldValue))
  else
    TRadioGroup(FControl).Items.Objects[TRadioGroup(FControl).itemIndex] := TObject(-1);

  TRadioGroup(FControl).Enabled := Assigned(FView.FieldEntity) and (FView.State = vsFullAccess);
end;

procedure TDEEnumFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TRadioGroup(FControl).OnClick := AHandler;
end;

{ TDECurrencyEditControl }

procedure TDECurrencyFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
// var
// vDef: TSimpleFieldDef;
begin
  FControl := TEdit.Create(nil);

  with TEdit(FControl) do
  begin
    ReadOnly := FView.State < vsFullAccess;
    // DisplayFormat := ',0.00;-,0.00';
    MaxLength := 15;
  end;
  // TcxCurrencyEdit(FInnerControl).OnKeyDown := OnWinControlKeyDown;

  // vDef := TSimpleFieldDef(FFieldDef);
  // with TEdit(FControl) do
  // begin
  // if not VarIsNull(vDef.MaxValue) then MaxValue := vDef.MaxValue;
  // if not VarIsNull(vDef.MinValue) then MinValue := vDef.MinValue;
  // end;
end;

procedure TDECurrencyFieldEditor.DoOnChange;
begin
  SetFieldValue(TEdit(FControl).text);
end;

procedure TDECurrencyFieldEditor.FillEditor;
var
  vEdit: TEdit;
begin
  vEdit := TEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.text := '0';
    vEdit.Enabled := False;
    vEdit.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.text := FView.FieldValue;
    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
    begin
      vEdit.BorderStyle := GetDisabledBorderStyle;
      // vEdit.ButtonTransparency := ebtAlways;
      vEdit.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else
    begin
      vEdit.BorderStyle := bsSingle;
      // vEdit.ButtonTransparency := ebtNone;
      vEdit.Color := clWindow;
      vEdit.TabStop := True;
    end;
  end;
end;

procedure TDECurrencyFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TEdit(FControl).OnChange := AHandler;
end;

{ TDEBoolEditControl }

procedure TDEBoolFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FNeedCreateCaption := False;

  FControl := TCheckBox.Create(nil);
  // TcxCheckBox(Result).OnKeyDown := OnWinControlKeyDown;

  TCheckBox(FControl).Caption := GetFieldTranslation(FFieldDef);
  TCheckBox(FControl).Hint := GetFieldTranslation(FFieldDef, tpHint);
end;

procedure TDEBoolFieldEditor.DoOnChange;
begin
  SetFieldValue(TCheckBox(FControl).Checked);
end;

procedure TDEBoolFieldEditor.FillEditor;
begin
  if VarIsNull(FView.FieldValue) then
    TCheckBox(FControl).Checked := False
  else
    TCheckBox(FControl).Checked := FView.FieldValue;

  TCheckBox(FControl).Enabled := FView.State = vsFullAccess;
  // TCheckBox(FControl).ReadOnly := FView.State < vsFullAccess;
  TCheckBox(FControl).TabStop := TCheckBox(FControl).Enabled;
end;

procedure TDEBoolFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TCheckBox(FControl).OnClick := AHandler;
end;

{ TDEImageEditor }

procedure TDEImageEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FImage);
  FreeAndNil(FLoadBtn);
  FreeAndNil(FClearBtn);
  FreeAndNil(FSaveBtn);
  FreeAndNil(FBtnPanel);
end;

procedure TDEImageEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FBasePanel := TPanel.Create(nil);
  FControl := FBasePanel;

  FBtnPanel := TPanel.Create(nil);
  FLoadBtn := TButton.Create(nil);
  FSaveBtn := TButton.Create(nil);
  FClearBtn := TButton.Create(nil);
  FImage := TImage.Create(nil);
  with FBtnPanel do
  begin
    Parent := FBasePanel;
    Align := alBottom;
    Caption := '';
    Height := 25;
  end;
  with FLoadBtn do
  begin
    Parent := FBtnPanel;
    Align := alRight;
    Caption := 'Load';
    OnClick := OnLoadBtnClick;
    width := Parent.width div 3;
  end;
  with FClearBtn do
  begin
    Parent := FBtnPanel;
    Align := alLeft;
    Caption := 'Clear';
    OnClick := OnClearBtnClick;
    width := Parent.width div 3;
  end;
  with FSaveBtn do
  begin
    Parent := FBtnPanel;
    Align := alClient;
    Caption := 'Save';
    OnClick := OnSaveBtnClick;
    width := Parent.width div 3;
  end;
  with FImage do
  begin
    Parent := FBasePanel;
    Align := alClient;
    Stretch := True;
  end;
  if Assigned(FCreateParams) and (FCreateParams.Values['ViewState'] = 'ReadOnly') then
    FBtnPanel.Visible := False;
end;

procedure TDEImageEditor.OnLoadBtnClick;
var
  vLoadDlg: TOpenPictureDialog;
  vStream: TStream;
begin
  vLoadDlg := TOpenPictureDialog.Create(nil);
  vStream := TMemoryStream.Create;
  try
    with vLoadDlg do
    begin
      Filter := 'All (*.bmp;*.jpg;*.jpeg;*.png)|*.bmp;*.jpg;*.jpeg;*.png|Bitmaps (*.bmp)|*.bmp|JPEG Images (*.jpg)|*.jpg|JPEG Images (*.jpeg)|*.jpeg|PNG Images (*.png)|*.png';
      FileName := '*.jpg';
      options := options + [ofPathMustExist, ofFileMustExist];
      if Execute then
      begin
        FImage.Picture.LoadFromFile(FileName);
        FImage.Picture.SaveToStream(vStream);
        SetFieldStream(vStream);
        // if vstream = FView.FieldStream then
        // showmessage('1');
      end;
    end;
  finally
    vLoadDlg.free;
    vStream.free;
  end;
end;

procedure TDEImageEditor.OnSaveBtnClick;
var
  vSaveDlg: TSavePictureDialog;
begin
  vSaveDlg := TSavePictureDialog.Create(nil);
  try
    with vSaveDlg do
    begin
      Filter := 'All (*.bmp;*.jpg;*.jpeg;*.png)|*.bmp;*.jpg;*.jpeg;*.png|Bitmaps (*.bmp)|*.bmp|JPEG Images (*.jpg)|*.jpg|JPEG Images (*.jpeg)|*.jpeg|PNG Images (*.png)|*.png';
      FileName := '*.jpg';
      options := options + [ofOverwritePrompt];
      if Execute then
        FImage.Picture.SaveToFile(FileName);
    end;
  finally
    vSaveDlg.free;
  end;
end;

procedure TDEImageEditor.OnClearBtnClick;
begin
  FImage.Picture := nil;
end;

procedure TDEImageEditor.FillEditor;
var
  // vPicture: TPicture;
  // vStr: AnsiString;
  vStream: TStream;
  // vImage: TImage;
begin
  inherited;
  vStream := FView.FieldStream;
  if vStream = nil then
  begin
    FImage.Picture := nil;
    Exit;
  end;

  // vPicture := TPicture.Create;
  // vImage := TImage.Create(nil);
  // FImage.Picture.Graphic := vImage;
  // vPicture.Graphic := vImage;
  try
    vStream.Position := 0;
    // FImage.Picture.LoadFromStream(vStream);
    // SavePicture(vPicture, vStr);
    // TcxImage(FControl).EditValue := vStr;
    FImage.Picture.LoadFromStream(vStream);
  finally
    // vPicture.Graphic := nil;
    // FreeAndNil(vImage);
    // vPicture.Free;
  end;

  FBtnPanel.Visible := FView.State = vsFullAccess;
  // TImage(FControl).Enabled := FView.State = vsFullAccess;
end;

function TDEImageEditor.GetLayoutPositionCount: Integer;
begin
  Result := 2;
end;

{ TDEMemoFieldEditor }

procedure TDEMemoFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FControl := TMemo.Create(nil);
  TMemo(FControl).ScrollBars := ssVertical;
  TMemo(FControl).WantReturns := True;
  TMemo(FControl).ParentFont := True;
  TMemo(FControl).OnKeyDown := OnKeyDown;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TMemo(FControl).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TDEMemoFieldEditor.DoOnChange;
begin
  // важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TMemo(FControl).text);
end;

procedure TDEMemoFieldEditor.FillEditor;
var
  vEdit: TMemo;
begin
  vEdit := TMemo(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.text := '';
    vEdit.Enabled := False;
    vEdit.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.Enabled := True;
    vEdit.text := FView.FieldValue;
    vEdit.SelStart := 1000;
    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
    begin
      vEdit.BorderStyle := GetDisabledBorderStyle;
      vEdit.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else
    begin
      vEdit.BorderStyle := bsSingle;
      vEdit.Color := clWindow;
      vEdit.TabStop := True;
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
  if Assigned(TMemo(FControl)) then
    TMemo(FControl).OnChange := AHandler;
end;

{ TDETimeFieldEditor }

procedure TDETimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
// var
// vDef: TSimpleFieldDef;
begin
  FControl := TDateTimePicker.Create(nil);

  // vDef := TSimpleFieldDef(FFieldDef);
  with TDateTimePicker(FControl) do
  begin
    Kind := dtkTime;
    // Properties.ImmediatePost := True;
    // if not VarIsNull(vDef.MaxValue) then
    // MaxValue := vDef.MaxValue;
    // if not VarIsNull(vDef.MinValue) then
    // MinValue := vDef.MinValue;
  end;
end;

procedure TDETimeFieldEditor.DoOnChange;
var
  vTime: TTime;
begin
  if MyTryStrToTime(timetostr(TDateTimePicker(FControl).time), vTime) then
    SetFieldValue(Frac(vTime))
  else
    SetFieldValue(Null);
end;

procedure TDETimeFieldEditor.FillEditor;
var
  vEdit: TDateTimePicker;
begin
  vEdit := TDateTimePicker(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.time := NullDate;
    // vEdit.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.time := FView.FieldValue;
    // (FView.State > vsDisabled) and not (FView.State < vsFullAccess);

    // if vEdit.Enabled then
    // begin
    // // vEdit.BorderStyle := GetDisabledBorderStyle;
    // // vEdit.ButtonTransparency := ebtAlways;
    // vEdit.Color := clWindow;
    // vEdit.TabStop := True;
    // end
    // else
    // begin
    // // vEdit.Style.BorderStyle := ebsUltraFlat;
    // // vEdit.Style.ButtonTransparency := ebtNone;
    // vEdit.Color := clBtnFace;
    // vEdit.TabStop := False;
    // end;
  end;
  vEdit.Enabled := False;
  // vEdit.SpinButtons.Visible := vEdit.Visible and vEdit.Enabled and (not vEdit.Properties.ReadOnly);
end;

procedure TDETimeFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TDateTimePicker(FControl).OnChange := AHandler;
end;

{ TDEMaskFieldEditor }

procedure TDEMaskFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
var
  vMask: string;
begin
  FControl := TEdit.Create(nil);
  // TEdit(FControl).OnKeyDown := OnWinControlKeyDown;
  // TEdit(FControl).Properties.ValidationOptions := [evoShowErrorIcon, evoAllowLoseFocus];

  vMask := '';
  // if Assigned(FFieldDef)then
  // begin
  // TEdit(FControl).Properties.MaskKind := emkStandard;
  // if SameText('phone', FFieldDef.StyleName) then
  // vMask := '!\+9 (999\) 000-00-00;1;_'
  // else if SameText('INN', FFieldDef.StyleName) then
  // vMask := '000000000009;0;_'
  // else if SameText('email', FFieldDef.StyleName) then
  // begin
  // TEdit(FControl).Properties.MaskKind := emkRegExpr;
  // vMask := '[\w\-.]+@[\w\-]+(\.[\w\-]+)+ ';
  // end
  // else if SameText('url', FFieldDef.StyleName) then
  // begin
  // TEdit(FControl).Properties.MaskKind := emkRegExprEx;
  // vMask := 'http\:\/\/(\w+(\.\w+)*@)?\w+\.\w+(\.\w+)*(/(\w+(/\w+)*/?)?)?';
  // end;
  // end;
  // TEdit(FControl).Properties.EditMask := vMask;
end;

procedure TDEMaskFieldEditor.DoOnChange;
begin
  // важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TEdit(FControl).text);
end;

procedure TDEMaskFieldEditor.FillEditor;
var
  vEdit: TEdit;
begin
  vEdit := TEdit(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.text := '';
    vEdit.Enabled := False;
    vEdit.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.text := FView.FieldValue;
    vEdit.Enabled := True;
    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
    begin
      vEdit.BorderStyle := GetDisabledBorderStyle;
      vEdit.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else
    begin
      vEdit.BorderStyle := bsSingle;
      vEdit.Color := clWindow;
      vEdit.TabStop := True;
    end;
  end;
end;

procedure TDEMaskFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TEdit(FControl)) then
    TEdit(FControl).OnChange := AHandler;
end;

{ TMRUFieldEditor }

procedure TMRUFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FControl := TEdit.Create(nil);
  // TcxMRUEdit(FControl).ShowEllipsis := False;
end;

procedure TMRUFieldEditor.DoOnChange;
begin
  // важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TEdit(FControl).text);
end;

procedure TMRUFieldEditor.FillEditor;
var
  vEdit: TEdit;
  // vCollections: TList<TCollection>;
  // vCollection: TCollection;
  // vEntity: TEntity;
  // vText: string;
begin
  vEdit := TEdit(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.text := '';
    vEdit.Enabled := False;
    // vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.text := FView.FieldValue;
    vEdit.Enabled := True;
    vEdit.ReadOnly := FView.State < vsFullAccess;

    // if not vEdit.ReadOnly then
    // begin
    // vEdit.Items.Clear;
    // vCollections := TDomain(FView.Domain).CollectionsByDefinition(FFieldDef.Definition);
    // try
    // for vCollection in vCollections do
    // for vEntity in vCollection do
    // begin
    // vText := vEntity[FFieldDef.Name];
    // if (Length(vText) > 0) and (vEdit.Items.IndexOf(vText) < 0) then
    // vEdit.Items.Append(vText);
    // end;
    // finally
    // FreeAndNil(vCollections);
    // end;
    // end;

    if vEdit.ReadOnly then
    begin
      // vEdit.BorderStyle := GetDisabledBorderStyle;
      vEdit.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else
    begin
      // vEdit.BorderStyle := ebsUltraFlat;
      vEdit.Color := clWindow;
      vEdit.TabStop := True;
    end;
  end;
end;

procedure TMRUFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(TComboBox(FControl)) then
    TComboBox(FControl).OnChange := AHandler;
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

procedure TColorEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FBasePanel := TPanel.Create(nil);
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
    width := 30;
  end;
  with FClearBtn do
  begin
    Parent := FBasePanel;
    Align := alRight;
    Caption := 'x';
    OnClick := OnClearBtnClick;
    width := 30;
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
    FPaintBox.Canvas.TextOut(5, 5, TInteractor(FView.Interactor).Translate('txtNotAssigned', 'Не задан'));
end;

procedure TColorEditor.OnSelectBtnClick(Sender: TObject);
begin
  if FColorDialog.Execute( { TWinControl(FInnerControl).Handle D7 } ) then
  begin
    SetFieldValue(FColorDialog.Color);
    FPaintBox.Invalidate;
  end;
end;

{ TDEDateTimeFieldEditor }

procedure TDEDateTimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
var
  vDef: TSimpleFieldDef;
begin
  FControl := TDateTimePicker.Create(nil);

  // TcxDateEdit(FInnerControl).OnKeyDown := OnWinControlKeyDown;

  vDef := TSimpleFieldDef(FFieldDef);
  with TDateTimePicker(FControl) do
  begin
    if not VarIsNull(vDef.MaxValue) then
      MaxDate := vDef.MaxValue;
    if not VarIsNull(vDef.MinValue) then
      MinDate := vDef.MinValue;
  end;
end;

procedure TDEDateTimeFieldEditor.DoOnChange;
var
  vDate: TDateTime;
begin
  vDate := StrToDateTimeDef(datetimetostr(TDateTimePicker(FControl).DateTime), 0);
  SetFieldValue(vDate);
end;

procedure TDEDateTimeFieldEditor.FillEditor;
var
  vEdit: TDateTimePicker;
  vDate: TDateTime;
begin
  vEdit := TDateTimePicker(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.Date := NullDate;
  end
  else
  begin
    vDate := FView.FieldValue;
    if vDate < 2 then
      vEdit.Date := 0
    else
      vEdit.Date := vDate;

    // (FView.State > vsDisabled) and not (FView.State < vsFullAccess);

    // if vEdit.Enabled then
    // begin
    // vEdit.Color := clWindow;
    // vEdit.TabStop := True;
    // end
    // else
    // begin
    // vEdit.Color := clBtnFace;
    // vEdit.TabStop := False;
    // end;
  end;
  vEdit.Enabled := False;
  // ToggleButtons;
end;

procedure TDEDateTimeFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TDateTimePicker(FControl).OnChange := AHandler;
end;

{ TFilenameFieldEditor }

procedure TFilenameFieldEditor.BeforeExecute(Sender: TObject);
begin
  if (Length(FText.text) > 0) and DirectoryExists(ExtractFileDir(FText.text)) then
    FAction.Dialog.InitialDir := ExtractFileDir(FText.text);
end;

procedure TFilenameFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FAction);
end;

procedure TFilenameFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
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
  FBtn.width := 40;
  FBtn.Images := TDragImageList(TInteractor(Interactor).Images[16]);
  FBtn.Action := FAction;

  FText := TEdit.Create(nil);
  FText.Align := alClient;
  FText.Parent := vBase;

  FControl := vBase;
end;

procedure TFilenameFieldEditor.DoOnChange;
begin
  SetFieldValue(FText.text);
end;

procedure TFilenameFieldEditor.FillEditor;
begin
  TPanel(FControl).Caption := ''; // reset caption after TVCLFieldArea.Create (FControl.Name := FFieldDef.Name;)

  if VarIsNull(FView.FieldValue) then
  begin
    FText.text := '';
    FText.Enabled := False;
    FText.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    FText.text := FView.FieldValue;
    FText.Hint := FView.FieldValue;
    FText.Enabled := True;
    FText.ReadOnly := FView.State < vsSelectOnly;

    if FText.ReadOnly then
    begin
      FText.BorderStyle := GetDisabledBorderStyle;
      FText.Color := clBtnFace;
      FText.TabStop := False;
    end
    else
    begin
      FText.BorderStyle := bsSingle;
      FText.TabStop := True;
      FText.Color := clWindow;
    end;
  end;

  FAction.Visible := FView.State >= vsSelectOnly;
end;

procedure TFilenameFieldEditor.OnAccept(Sender: TObject);
begin
  FText.text := FAction.Dialog.FileName;
  FText.Hint := FText.text;
end;

procedure TFilenameFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(FText) then
    FText.OnChange := AHandler;
end;

procedure TSelectFolderFieldEditor.BeforeExecute(Sender: TObject);
begin
  if (Length(FText.text) > 0) and DirectoryExists(FText.text) then
    FAction.Folder := FText.text;
end;

procedure TSelectFolderFieldEditor.BrowseForFolder1Accept(Sender: TObject);
begin
  FText.text := FAction.Folder;
end;

{ TSelectFolderFieldEditor }

procedure TSelectFolderFieldEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FAction);
end;

procedure TSelectFolderFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
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
  FBtn.width := 40;
  FBtn.Images := TDragImageList(TInteractor(Interactor).Images[16]);
  FBtn.Action := FAction;

  FText := TEdit.Create(nil);
  FText.Align := alClient;
  FText.Parent := vBase;

  FControl := vBase;
end;

procedure TSelectFolderFieldEditor.DoOnChange;
begin
  SetFieldValue(FText.text);
end;

procedure TSelectFolderFieldEditor.FillEditor;
begin
  TPanel(FControl).Caption := ''; // reset caption after TVCLFieldArea.Create (FControl.Name := FFieldDef.Name;)

  if VarIsNull(FView.FieldValue) then
  begin
    FText.text := '';
    FText.Enabled := False;
    FText.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    FText.text := FView.FieldValue;
    FText.Hint := FView.FieldValue;
    FText.Enabled := True;
    FText.ReadOnly := FView.State < vsFullAccess;

    if FText.ReadOnly then
    begin
      FText.BorderStyle := GetDisabledBorderStyle;
      FText.Color := clBtnFace;
      FText.TabStop := False;
    end
    else
    begin
      FText.BorderStyle := bsSingle;
      FText.TabStop := True;
      FText.Color := clWindow;
    end;
  end;

  FAction.Visible := FView.State > vsSelectOnly;
end;

procedure TSelectFolderFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(FText) then
    FText.OnChange := AHandler;
end;

{ TSpinner }

procedure TSpinner.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FControl := TActivityIndicator.Create(nil);
  // TActivityIndicator(FControl).Transparent := True;
end;

procedure TSpinner.FillEditor;
begin
  inherited;
  Control.Visible := FView.FieldValue > 0;
  TActivityIndicator(FControl).Animate := Control.Visible;
end;

{ TImageByString }

procedure TImageByString.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FControl := TImage.Create(nil);
  TImage(FControl).Transparent := True;
end;

procedure TImageByString.FillEditor;
var
  i: Integer;
  vStream: TStream;
begin
  inherited;
  i := FCreateParams.IndexOfName(FView.FieldValue);

  if i < 0 then
    Exit;

  i := StrToIntDef(FCreateParams.Values[FView.FieldValue], 0);

  vStream := TConfiguration(TInteractor(FView.Interactor).Configuration).Icons.IconByIndex(i, 16);
  if Assigned(vStream) then
  begin
    vStream.Position := 0;
    TImage(FControl).Picture.LoadFromStream(vStream);
  end;
end;

{ TTextSelector }

procedure TTextSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FControl := TComboBox.Create(nil);

  // TComboBox(FControl).Properties.BeepOnError := True;
  TComboBox(FControl).onDropDown := CBOnInitPopup;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TComboBox(FControl).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TTextSelector.DoOnChange;
begin
  // важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TComboBox(FControl).text);
end;

procedure TTextSelector.FillEditor;
var
  vEdit: TComboBox;
begin
  FillList;

  vEdit := TComboBox(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.text := '';
    vEdit.Enabled := False;
    // vEdit.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    vEdit.text := FView.FieldValue;
    vEdit.Enabled := True;
    // vEdit.Properties.ReadOnly := FView.State < vsSelectOnly;

    // в смена DropDownListStyle ломает отображение текущего значения, оно сбрасывается в пустоту и запись нового значения не отрабатывает
    { if FView.State < vsFullAccess then
      vEdit.Properties.DropDownListStyle := lsFixedList
      else
      vEdit.Properties.DropDownListStyle := lsEditFixedList;
    }
    // if vEdit.Properties.ReadOnly then
    // begin
    // // vEdit.Style.BorderStyle := GetDisabledBorderStyle;
    // vEdit.Style.Color := clBtnFace;
    // vEdit.TabStop := False;
    // end
    // else
    // begin
    // // vEdit.Style.BorderStyle := ebsUltraFlat;
    // vEdit.TabStop := True;
    // if FView.State = vsSelectOnly then
    // vEdit.Style.Color := clBtnFace
    // else
    // vEdit.Style.Color := clWindow;
    // end;
  end;
end;

procedure TTextSelector.FillList;
var
  i: Integer;
  vDictionary: string;
  vFieldName: string;
  vPos: Integer;
  vCollection: TCollection;
  // vComPorts: TStrings;
begin
  TComboBox(FControl).Items.BeginUpdate;
  TComboBox(FControl).Items.Clear;

  { if FFieldDef.StyleName = 'comport' then
    begin
    vComPorts := TStringList.Create;
    EnumComPorts(vComPorts);
    for i := 0 to vComPorts.Count - 1 do
    TComboBoxEx(FControl).Properties.Items.Add(vComPorts[i]);
    FreeAndNil(vComPorts);
    TComboBoxEx(FControl).EditValue := FView.FieldValue;
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

      TComboBox(FControl).text := FView.FieldValue;
    end;
  end;
  TComboBox(FControl).Items.EndUpdate;
end;

procedure TTextSelector.CBOnInitPopup(Sender: TObject);
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

function TDEEditor.GetDisabledBorderStyle: TBorderStyle;
begin
  if StrToBoolDef(TDomain(TInteractor(FView.Interactor).Domain).UserSettings.GetValue('Core', 'ShowBordersForDisabled'), True) then
    Result := bsSingle
  else
    Result := bsNone;
end;


  // procedure TDEEditor.ToggleButtons;
  // var
  // i: Integer;
  // vEdit: TcxEditCrack;
  // vVisible: Boolean;
  // begin
  // if FControl is TCustomEdit then
  // begin
  // vEdit := TcxEditCrack(FControl);
  // vVisible := vEdit.Visible and vEdit.Enabled and (not vEdit.Properties.ReadOnly);

  // if vVisible then
  // begin
  // i := 0;
  // while i < TCustomEditProperties(vEdit.Properties).Buttons.Count do
  // begin
  // TcxCustomEditProperties(vEdit.Properties).Buttons.Items[i].Visible := True;
  // Inc(i);
  // end;
  // for i := 0 to TcxCustomEditProperties(vEdit.Properties).Buttons.Count - 1 do
  // TcxCustomEditProperties(vEdit.Properties).Buttons.Items[i].Visible := True;
  // end
  // else
  // for i := TcxCustomEditProperties(vEdit.Properties).Buttons.Count - 1 downto 0 do
  // TcxCustomEditProperties(vEdit.Properties).Buttons.Items[i].Visible := False;
  // end;
  // end;

  { TDEBLOBEditor }

//procedure TDEBLOBEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
//begin
//  FControl := TcxBLOBEdit.Create(nil);
//  TcxBLOBEdit(FControl).AutoSize := False;
//  TcxBLOBEdit(FControl).Properties.Buttons.Clear;
//  TcxBLOBEdit(FControl).Properties.OnChange := DoOnChangeBLOB;
//end;
//
//procedure TDEBLOBEditor.DoOnChangeBLOB(Sender: TObject);
//begin
//  TcxBLOBEdit(FControl).PostEditValue;
//end;
//
//procedure TDEBLOBEditor.FillEditor;
//var
//  vValue: AnsiString;
//  vStream: TStream;
//begin
//  inherited;
//  vStream := FView.FieldStream;
//  if vStream = nil then
//  begin
//    TcxBLOBEdit(FControl).EditValue := '';
//    Exit;
//  end;
//
//  vStream.Position := 0;
//  SetLength(vValue, vStream.Size);
//  vStream.ReadBuffer(vValue[1], vStream.Size);
//
//  TcxBLOBEdit(FControl).EditValue := vValue;
//  TcxBLOBEdit(FControl).Enabled := FView.State = vsFullAccess;
//end;

{ TDEEnumEditor }

procedure TDEEnumEditor.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TDEEnumEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  if not Assigned(FEnum) then
    FEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);

  if FFieldDef.StyleName = 'radio' then
  begin
    FControl := TRadioGroup.Create(nil);
    // TcxRadioGroup(FControl).Transparent := True;
    // TcxRadioGroup(FControl).Style.BorderStyle := ebsNone;
    TRadioGroup(FControl).Name := 'radio';
    TRadioGroup(FControl).Caption := '';
    FNeedCreateCaption := False;
  end
  else
  begin
    FControl := TComboBox.Create(nil);
    TComboBox(FControl).Style := TComboBoxStyle.csDropDownList;
    if Assigned(FCreateParams) then
    begin
      if FCreateParams.Values['DropDownListStyle'] = 'Fixed' then
        TComboBox(FControl).Style := TComboBoxStyle.csDropDownList
      else if FCreateParams.Values['DropDownListStyle'] = 'Edit' then
        TComboBox(FControl).Style := TComboBoxStyle.csDropDown;
    end;
    TComboBox(FControl).onDropDown := CBOnInitPopup;
  end;
end;

procedure TDEEnumEditor.DoOnChange;
begin
  if FFieldDef.StyleName = 'radio' then
  begin
    if FFieldDef.HasFlag(cRequired) then
      SetFieldValue(TRadioGroup(FControl).itemIndex + 1)
    else
      SetFieldValue(TRadioGroup(FControl).itemIndex);
  end
  else
  begin
    if FFieldDef.HasFlag(cRequired) then
      SetFieldValue(TComboBox(FControl).itemIndex + 1)
    else
      SetFieldValue(TComboBox(FControl).itemIndex);
  end;
end;

procedure TDEEnumEditor.FillEditor;
var
  vEdit: TComboBox;
  vRadioEdit: TRadioGroup;
  vItemIndex: Integer;
begin
  FillList;

  if FFieldDef.StyleName = 'radio' then
  begin
    vRadioEdit := TRadioGroup(FControl);
    if VarIsNull(FView.FieldValue) then
    begin
      vRadioEdit.Enabled := False;
      // vRadioEdit.ReadOnly := True;
    end
    else
    begin
      vItemIndex := FView.FieldValue;
      if FFieldDef.HasFlag(cRequired) then
        vItemIndex := vItemIndex - 1;
      vRadioEdit.itemIndex := vItemIndex;

      // vRadioEdit.Properties.ReadOnly := FView.State < vsSelectOnly;
      // vRadioEdit.Enabled := not vRadioEdit.Properties.ReadOnly;

      // if vRadioEdit.Properties.ReadOnly then
      // begin
      // vRadioEdit.Style.BorderStyle := GetDisabledBorderStyle;
      // vRadioEdit.TabStop := False;
      // end
      // else
      // begin
      // vRadioEdit.Style.BorderStyle := ebsUltraFlat;
      // vRadioEdit.TabStop := True;
      // end;
    end;
  end
  else
  begin
    vEdit := TComboBox(FControl);
    if VarIsNull(FView.FieldValue) then
    begin
      vEdit.text := ''; // FEnum.Items[0].DisplayText;
      vEdit.Enabled := False;
      vEdit.Style := TComboBoxStyle(True);
    end
    else
    begin
      vItemIndex := FView.FieldValue;
      if FFieldDef.HasFlag(cRequired) then
        vItemIndex := vItemIndex - 1;
      vEdit.itemIndex := vItemIndex;
      if vItemIndex = 0 then
        vEdit.text := FEnum.Items[0].DisplayText;

      vEdit.Style := TComboBoxStyle(FView.State < vsSelectOnly);
      vEdit.Enabled := not Boolean(vEdit.Style);

      if Boolean(vEdit.Style) then
      begin
        // vEdit.Style.BorderStyle := GetDisabledBorderStyle;
        vEdit.TabStop := False;
      end
      else
      begin
        // vEdit.Style.BorderStyle := ebsUltraFlat;
        vEdit.TabStop := True;
      end;
    end;

    // ToggleButtons;
  end;
end;

procedure TDEEnumEditor.FillList;
var
  vEnumItem: TEnumItem;
  vItems: TStrings;
  vRadioItems: TStrings;
begin
  if FFieldDef.StyleName = 'radio' then
  begin
    vRadioItems := TRadioGroup(FControl).Items;
    vRadioItems.BeginUpdate;
    try
      vRadioItems.Clear;
      for vEnumItem in FEnum do
        if not FFieldDef.HasFlag(cRequired) or (vEnumItem.ID > 0) then
        begin
          vRadioItems.Add(vEnumItem.DisplayText);
          // vRadioItem.Caption := vEnumItem.DisplayText;
        end;
    finally
      vRadioItems.EndUpdate;
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

procedure TDEEnumEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  if (FFieldDef.StyleName = 'radio') and Assigned(TRadioGroup(FControl)) then
    TRadioGroup(FControl).OnClick := AHandler
  else if Assigned(TComboBox(FControl)) then
    TComboBox(FControl).OnChange := AHandler;
end;

{ TDELogFieldEditor }

procedure TDELogFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  inherited DoCreateControl(AParent, ALayout);
  TMemo(FControl).ScrollBars := ssBoth;
end;

{ TDEPagesFieldEditor }

procedure TDEPagesFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
var
  vPanel: TPanel absolute ALayout;
  vSourcePC: TPageControl absolute ALayout;
  vPC: TPageControl;
  vSourceTab: TTabSheet;
  vPage: TTabSheet;
  vChildArea: TVCLArea;
  i: Integer;
begin
  FNeedCreateCaption := False;

  inherited;

  vPC := TPageControl.Create(nil);
  FControl := vPC;
  vPC.DoubleBuffered := True;
  vPC.Width := vSourcePC.Width;
  vPC.Height := vSourcePC.Height;

  vPC.Font.Assign(vSourcePC.Font);
  vPC.Align := vSourcePC.Align;
  vPC.AlignWithMargins := vSourcePC.AlignWithMargins;
  vPC.Margins := vSourcePC.Margins;
  vPC.Anchors := vSourcePC.Anchors;
  vPC.Style := vSourcePC.Style;
  if (vSourcePC.Style <> TTabStyle.tsTabs) and (vSourcePC.PageCount > 0) then
  begin
    vPC.Left := vSourcePC.Left + vSourcePC.Pages[0].Left;
    vPC.Top := vSourcePC.Top + vSourcePC.Pages[0].Top;
  end
  else begin
    vPC.Left := vSourcePC.Left;
    vPC.Top := vSourcePC.Top;
  end;

  vPC.tag := Byte(not vSourcePC.ShowHint or ((vSourcePC.PageCount > 0) and not vSourcePC.Pages[0].TabVisible));
  if not Boolean(vPC.tag) then
  begin
//    vPC.Properties.Options := vPC.Properties.Options + [pcoTopToBottomText];
    vPC.TabPosition := vSourcePC.TabPosition;
    vPC.TabHeight := vSourcePC.TabHeight;
  end;

  // Нужно прописывать родителя, чтобы создавать вложенные сцены
  vPC.Parent := TWinControl(TVCLArea(AParent).Control);

  for i := 0 to vSourcePC.PageCount - 1 do
  begin
    vSourceTab := vSourcePC.Pages[i];
    vPage := TTabSheet.Create(vPC);
    vPage.Caption := vSourceTab.Caption;
    vPage.ImageIndex := vSourceTab.ImageIndex;
    vPage.TabVisible := not Boolean(vPC.Tag);

    vChildArea := TVCLArea.Create(Self, FView.Parent, vSourceTab.Name, False, vPage);
    AddArea(vChildArea);
    TInteractor(FView.Interactor).UIBuilder.CreateChildAreas(vChildArea, vSourceTab, '');
  end;

  for i := vSourcePC.PageCount - 1 downto 0 do
    vSourcePC.Pages[i].free;
end;

procedure TDEPagesFieldEditor.DoOnChange;
// var
// vTag: Integer;
begin
  { vTag := TPageControl(FControl).ActivePageIndex;
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
      if TFieldDef(FView.Definition).Flags or cRequired = 1 then
        vValue := vValue - 1;
      vTag := vValue
    end
    else if TFieldDef(FView.Definition).Kind = fkInteger then
      vTag := vValue
    else
      vTag := -1;
  end;

  if (vTag < TPageControl(FControl).PageCount) and (TPageControl(FControl).ActivePageIndex <> vTag) then
    TPageControl(FControl).ActivePageIndex := vTag;
end;

procedure TDEPagesFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TPageControl(FControl).OnChange := AHandler;
end;

{ TProgress }

procedure TProgress.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FControl := TProgressBar.Create(nil);
  // TProgressBar(FControl).AutoSize := TPanel(ALayout).ShowCaption;
  // TProgressBar(FControl).Properties.ShowText := TPanel(ALayout).ShowCaption;
  FNeedCreateCaption := False;
  TProgressBar(FControl).Max := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TProgress.FillEditor;
begin
  inherited;
  TProgressBar(FControl).Position := FView.FieldValue;
end;

{ TEntityBreadcrumb }

//procedure TEntityBreadcrumb.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
//begin
//  FControl := TdxBreadcrumbEdit.Create(nil);
//  TdxBreadcrumbEdit(FControl).Properties.PathEditor.PathDelimiter := '.';
//end;
//
//procedure TEntityBreadcrumb.FillEditor;
//var
//  vDefinition: TDefinition;
//begin
//  inherited;
//  if FView.Parent.DefinitionKind = dkObjectField then
//    vDefinition := TObjectFieldDef(FView.Parent.Definition)._ContentDefinition
//  else
//    vDefinition := TDefinition(FView.Parent.Definition);
//
//  SetValidateDefinition(vDefinition);
//  // TdxBreadcrumbEdit(FControl).Root
//  if FView.FieldValue = '' then
//    TdxBreadcrumbEdit(FControl).SelectedPath := '$'
//  else
//    TdxBreadcrumbEdit(FControl).SelectedPath := '$.' + FView.FieldValue;
//end;
//
//procedure TEntityBreadcrumb.SetValidateDefinition(const ADefinition: TDefinition);
//var
//  vNode: TdxBreadcrumbEditNode;
//  procedure ProcessDef(const ADef: TDefinition; const ANode: TdxBreadcrumbEditNode);
//  var
//    vFieldDef: TFieldDef;
//  begin
//    for vFieldDef in ADef.Fields do
//    begin
//      if TInteractor(FView.Interactor).NeedSkipField(nil, vFieldDef) then
//        Continue;
//
//      vNode := ANode.AddChild;
//      vNode.Name := vFieldDef.Name;
//      if vFieldDef is TEntityFieldDef then
//        ProcessDef(TEntityFieldDef(vFieldDef).ContentDefinitions[0], vNode)
//    end;
//  end;
//
//begin
//  // vEdit :=
//  // TdxBreadcrumbEdit(FControl).BeginUpdate;
//  try
//    TdxBreadcrumbEdit(FControl).Root.Clear;
//    TdxBreadcrumbEdit(FControl).Root.Name := '$';
//    ProcessDef(ADefinition, TdxBreadcrumbEdit(FControl).Root);
//  finally
//    // TdxBreadcrumbEdit(FControl).EndUpdate;
//  end;
//end;
//
//procedure TEntityBreadcrumb.DoOnChange;
//var
//  vPath: string;
//begin
//  vPath := TdxBreadcrumbEdit(FControl).SelectedPath;
//  if Length(vPath) > 2 then
//    Delete(vPath, 1, 2)
//  else
//    vPath := '';
//  SetFieldValue(vPath);
//end;

{ TDEFlagsEditor }

procedure TDEFlagsEditor.CLBOnClickCheck(Sender: TObject);
begin
  OnChange(Sender);
end;

procedure TDEFlagsEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  FControl := TCheckListBox.Create(nil);
  TCheckListBox(FControl).OnClickCheck := CLBOnClickCheck;
end;

procedure TDEFlagsEditor.DoOnChange;
var
  i: Integer;
  vFlagsValue: Integer;
  // vListItem: TCheckListBoxItem;
begin
  vFlagsValue := 0;
  for i := 0 to TCheckListBox(FControl).Items.Count - 1 do
  begin
    if TCheckListBox(FControl).Checked[i] then
      vFlagsValue := vFlagsValue or TEnumItem(TCheckListBox(FControl).Items.Objects[i]).ID;
  end;

  SetFieldValue(vFlagsValue);
end;

procedure TDEFlagsEditor.FillEditor;
// var
// vList: TCheckListBox;
begin
  FillList;

  // vList := TCheckListBox(FControl);
  // vList.ReadOnly := FView.State < vsSelectOnly;
  // vList.Enabled := not vList.ReadOnly;

  // if vList.ReadOnly then
  // begin
  // vList.Style.BorderStyle := GetBoxDisabledBorderStyle;
  // vList.TabStop := False;
  // end
  // else
  // begin
  // vList.Style.BorderStyle := cbsUltraFlat;
  // vList.TabStop := True;
  // end;
end;

procedure TDEFlagsEditor.FillList;
var
  vList: TCheckListBox;
  vEnumItem: TEnumItem;
  vListItem: Integer;
  vValue: Integer;
begin
  vList := TCheckListBox(FControl);
  vValue := FView.FieldValue;
  vList.Items.BeginUpdate;
  try
    vList.Items.Clear;
    for vEnumItem in FEnum do

      if vEnumItem.ID > 0 then
      begin
        vListItem := vList.Items.AddObject(vEnumItem.DisplayText, vEnumItem);
        // vListItem := vList.Items.Add;
        // vListItem.Text := vEnumItem.DisplayText;
        // vListItem.ItemObject := vEnumItem; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        vList.Checked[vListItem] := (vEnumItem.ID and vValue) <> 0;
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
    TCheckListBox(FControl).OnClickCheck := CLBOnClickCheck
  else
    TCheckListBox(FControl).OnClickCheck := nil;
end;

{ TDEImagedAction }

procedure TDEImagedAction.DoBeforeFreeControl;
begin
  if Assigned(FActionView) then
  begin
    FActionView.CleanView;
    FActionView := nil;
  end;
end;

procedure TDEImagedAction.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
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
  else
  begin
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
    if FActionView.DefinitionKind = dkUndefined then
    begin
      FActionView.CleanView;
      FActionView := nil;
    end;
  end
  else
    FActionView := nil;

  vButton := TButton.Create(nil);
  vButton.Images := TDragImageList(TInteractor(Interactor).Images[vImageSize]);
  // vButton.PaintStyle := bpsGlyph;
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
  // TButton(Result).OnClick := AHandler;
end;

{ TBoolImages }

procedure TBoolImages.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
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

  if i < 0 then
    Exit;

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
  end;
end;

initialization

RegisterClasses([TBevel, TLabel]);
{ dx localization
  cxSetResourceString(@cxSDateThursday, 'Donderdag'); //=Thursday
  cxSetResourceString(@cxSDatePopupToday, 'Vandaag');//= 'Today'; }

TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, '', TDETextFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'phone', TDETextFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'email', TDEMaskFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'url', TDEMaskFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'INN', TDEMaskFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'memo', TDEMemoFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'log', TDELogFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'mru', TMRUFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'dir', TSelectFolderFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'file', TFilenameFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'ImageByString', TImageByString);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'selector', TTextSelector);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'comport', TTextSelector);
//TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'fieldpath', TEntityBreadcrumb);
TPresenter.RegisterUIClass('Windows.VCL', uiIntegerEdit, '', TDEIntegerFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiIntegerEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.VCL', uiIntegerEdit, 'spinner', TSpinner);
TPresenter.RegisterUIClass('Windows.VCL', uiIntegerEdit, 'progress', TProgress);
TPresenter.RegisterUIClass('Windows.VCL', uiEnumEdit, '', TDEEnumEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiEnumEdit, 'radio', TDEEnumEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiEnumEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.VCL', uiEnumEdit, 'pages', TDEPagesFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiFlagEdit, '', TDEFlagsEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiEntityEdit, 'enum', TDEEnumFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiFloatEdit, '', TDEFloatFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiFloatEdit, 'formatted', TDEFloatFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiFloatEdit, 'currency_rate', TDEFloatFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiDateEdit, '', TDEDateFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiDateEdit, 'time', TDETimeFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiDateEdit, 'datetime', TDEDateTimeFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiDateEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.VCL', uiCurrencyEdit, '', TDECurrencyFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiCurrencyEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.VCL', uiBoolEdit, '', TDEBoolFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiBoolEdit, 'imaged_action', TDEImagedAction);
TPresenter.RegisterUIClass('Windows.VCL', uiBoolEdit, 'images', TBoolImages);
TPresenter.RegisterUIClass('Windows.VCL', uiBoolEdit, 'pages', TDEPagesFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiColorEdit, '', TColorEditor);
//TPresenter.RegisterUIClass('Windows.VCL', uiBLOBEdit, '', TDEBLOBEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiBLOBEdit, 'image', TDEImageEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiEntityEdit, 'info', TTextInfo);

end.
