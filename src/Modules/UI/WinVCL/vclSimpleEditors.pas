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
  Classes, Types, Graphics, Controls, ExtCtrls, StdCtrls, Dialogs, ActnList, StdActns, ComCtrls, Vcl.CheckLst,
  vclArea, uDefinition, uEnumeration, uUIBuilder, uView, uEntity, uLayout;

type
  TVCLTextInfo = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TVCLEnumEditor = class(TVCLControl)
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

  TVCLGraphicEnumSelector = class(TVCLControl)
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

  TVCLLineStyleSelector = class(TVCLGraphicEnumSelector)
  protected
    procedure DoDrawItem(const ACanvas: TCanvas; const AID: Integer; const ARect: TRect;
      AState: TOwnerDrawState); override;
  end;

  TVCLFlagsEditor = class(TVCLControl)
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

  TVCLIntegerFlagsEditor = class(TVCLControl)
  private
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

  TVCLIntegerFieldEditor = class(TVCLControl)
  private
    FUpDown: TUpDown;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure SetParent(const AParent: TUIArea); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLFloatFieldEditor = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLDateFieldEditor = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLTimeFieldEditor = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLDateTimeFieldEditor = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure SetParent(const AParent: TUIArea); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLTextFieldEditor = class(TVCLControl)
  private
    procedure OnPhoneKeyPress(Sender: TObject; var Key: Char);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLTextSelector = class(TVCLControl)
  private
    procedure FillList;
    procedure CBOnInitPopup(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLSelectFolderFieldEditor = class(TVCLControl)
  private
    FText: TEdit;
    FBtn: TButton;
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

  TVCLFileNameFieldEditor = class(TVCLControl)
  private
    FBtn: TButton;
    FAction: TFileOpen;
    FText: TEdit;
    procedure OnAccept(Sender: TObject);
    procedure BeforeExecute(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  public
    property TextEdit: TEdit read FText;
  end;

  TVCLMaskFieldEditor = class(TVCLControl)
  private
    procedure OnEditorClick(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLMemoFieldEditor = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLBoolFieldEditor = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLImagedAction = class(TVCLControl)
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

  TVCLPagesFieldEditor = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLEnumFieldEditor = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLImageViewer = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TVCLColorPicker = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure SetParent(const AParent: TUIArea); override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  // определённый по времени процесс
  TVCLProgress = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TVCLColorEditor = class(TVCLControl)
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

  TVCLBoolImages = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TVCLImageByString = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TVCLLogEditor = class(TVCLControl)
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

  TVCLSelectedCaptionBoolFieldEditor = class(TVCLControl)
  private
    FSelected: Boolean;
    FSelectBackColor, FDefaultTextColor: TColor;
    procedure OnClick(Sender: TObject);
    procedure UpdateView;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

implementation

uses
  Generics.Collections, TypInfo, Variants, SysUtils, Windows,
  Forms, Math, DateUtils, Messages, VCL.Mask,

  uConfiguration, uDomain, uInteractor, uPresenter, uWinVCLPresenter, uCollection, uConsts,
  uUtils, UITypes;

{ TVCLTextInfo }

function TVCLTextInfo.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TLabel.Create(nil);
  TLabel(Result).Transparent := True;
  TLabel(Result).WordWrap := True;
  if Assigned(FCreateParams) and (FCreateParams.Values['WordWrap'] = 'False') then
    TLabel(Result).WordWrap := False;
  TLabel(Result).AutoSize := False;
end;

procedure TVCLTextInfo.FillEditor;
var
  vEnum: TEnumeration;
  vEntity: TEntity;
  vColorField: TBaseField;
  vValue: Variant;
begin
  inherited;
  if VarIsNull(FView.FieldValue) then
    TLabel(FControl).Caption := ''
  else if FFieldDef.Kind = fkCurrency then
  begin
    if FFieldDef.Format <> '' then
      TLabel(FControl).Caption := FormatFloat(GetFormat, FView.FieldValue)
    else
      TLabel(FControl).Caption := FormatFloat('#,##0.00;;0', FView.FieldValue);
  end
  else if FFieldDef.Kind = fkFloat then
  begin
    if FFieldDef.Format <> '' then
      TLabel(FControl).Caption := FormatFloat(GetFormat, FView.FieldValue)
    else
      TLabel(FControl).Caption := FView.FieldValue;
  end
  else if FFieldDef.Kind = fkDateTime then
  begin
    vValue := FView.FieldValue;
    if IsZero(vValue, 1e-6) then
      TLabel(FControl).Caption := ''
    else if FFieldDef.Format <> '' then
      TLabel(FControl).Caption := FormatDateTime(GetFormat, vValue)
    else
      TLabel(FControl).Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss', vValue);
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
    else begin
      TLabel(FControl).Caption := vEntity['Name'];
      if vEntity.Definition.ColorFieldName <> '' then
      begin
        vColorField := vEntity.FieldByName(vEntity.Definition.ColorFieldName);
        TLabel(FControl).Font.Color := TColor(vColorField.Value);
      end;
    end;
  end
  else
    TLabel(FControl).Caption := FView.FieldValue;
end;

{ TDEIntegerEditControl }

procedure TVCLIntegerFieldEditor.DoBeforeFreeControl;
begin
  FUpDown.Associate := nil;
  FreeAndNil(FUpDown);
end;

function TVCLIntegerFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TEdit.Create(nil);

  FUpDown := TUpDown.Create(nil);

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    FUpDown.Max := TSimpleFieldDef(FFieldDef).MaxValue;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
    FUpDown.Min := TSimpleFieldDef(FFieldDef).MinValue;

  FUpDown.Thousands := True;
end;

procedure TVCLIntegerFieldEditor.DoOnChange;
begin
  if FUpDown.Position = 0 then
    SetFieldValue(Null)
  else
    SetFieldValue(FUpDown.Position);
end;

procedure TVCLIntegerFieldEditor.FillEditor;
var
  vEdit: TEdit;
begin
  vEdit := TEdit(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    FUpDown.Position := 0;
  end
  else
  begin
    vEdit.Enabled := FView.State >= vsFullAccess;
    FUpDown.Position := FView.FieldValue;
  end;

  FUpDown.Visible := vEdit.Visible and vEdit.Enabled;
end;

procedure TVCLIntegerFieldEditor.SetParent(const AParent: TUIArea);
begin
  inherited SetParent(AParent);

  if Assigned(AParent) then
  begin
    FUpDown.Parent := TWinControl(FControl).Parent;
    FUpDown.Associate := TEdit(FControl);
  end;
end;

procedure TVCLIntegerFieldEditor.SwitchChangeHandlers(
  const AHandler: TNotifyEvent);
begin
  inherited;
  TEdit(FControl).OnChange := AHandler;
end;

{ TVCLFloatEditControl }

function TVCLFloatFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TEdit.Create(nil);
end;

procedure TVCLFloatFieldEditor.DoOnChange;
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
  if TEdit(FControl).Text = '' then
    vValue := 0
  else
    vValue := ToFloatDef(TEdit(FControl).Text);

  if IsZero(vValue) then
    SetFieldValue(Null)
  else
    SetFieldValue(vValue);
end;

procedure TVCLFloatFieldEditor.FillEditor;
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
    vEdit.Text := FloatToStr(FView.FieldValue);
    vEdit.Enabled := FView.State >= vsFullAccess;
  end;
end;

procedure TVCLFloatFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TEdit(FControl).OnChange := AHandler;
end;

{ TDEDateEditControl }

function TVCLDateFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vDef: TSimpleFieldDef;
begin
  Result := TDateTimePicker.Create(nil);

  TDateTimePicker(Result).DateMode := dmUpDown;
  TDateTimePicker(Result).Kind := dtkDate;

  vDef := TSimpleFieldDef(FFieldDef);
  if not VarIsNull(vDef.MaxValue) then
    TDateTimePicker(Result).MaxDate := vDef.MaxValue;
  if not VarIsNull(vDef.MinValue) then
    TDateTimePicker(Result).MinDate := vDef.MinValue;
end;

procedure TVCLDateFieldEditor.DoOnChange;
begin
  SetFieldValue(TDateTimePicker(FControl).Date);
end;

procedure TVCLDateFieldEditor.FillEditor;
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
  else begin
    vEdit.Enabled := FView.State > vsDisabled;
    vDate := FView.FieldValue;
    if vDate < 2 then
      vEdit.Date := 0
    else
      vEdit.Date := vDate;
  end;
end;

procedure TVCLDateFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TDateTimePicker(FControl).OnChange := AHandler;
end;

{ TVCLTextEditControl }

function TVCLTextFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TEdit.Create(nil);

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TEdit(Result).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;

  if SameText('phone', FFieldDef.StyleName) then
    TEdit(Result).OnKeyPress := OnPhoneKeyPress
  else begin
    if SameText('mask', FFieldDef.StyleName) then
      TEdit(Result).PasswordChar := '*';
    TEdit(Result).OnKeyPress := nil;
  end;
end;

procedure TVCLTextFieldEditor.DoOnChange;
begin
  SetFieldValue(TEdit(FControl).Text);
end;

procedure TVCLTextFieldEditor.OnPhoneKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, ['0'..'9', #8, '-', '+', '(', ')', ' ']) then Exit;

  Key := #0;
end;

procedure TVCLTextFieldEditor.FillEditor;
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

procedure TVCLTextFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TEdit(FControl).OnChange := AHandler;
end;

{ TDEEnumEditControl }

function TVCLEnumFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vCollections: TList<TCollection>;
  vCollection: TCollection;
  vEntity: TEntity;
begin
  Result := TRadioGroup.Create(nil);

  vCollections := TDomain(FView.Domain).CollectionsByDefinition(
    TEntityFieldDef(FFieldDef)._ContentDefinition);
  try
    for vCollection in vCollections do
      for vEntity in vCollection do
        if not vEntity.IsNew then
          TRadioGroup(Result).Items.AddObject(vEntity['Name'], vEntity);
  finally
    FreeAndNil(vCollections);
  end;

  TRadioGroup(Result).Caption := '';
end;

procedure TVCLEnumFieldEditor.DoOnChange;
begin
  SetFieldEntity(TEntity(TRadioGroup(FControl).Items.Objects[TRadioGroup(FControl).ItemIndex]));
end;

procedure TVCLEnumFieldEditor.FillEditor;
var
  vIndex: Integer;
begin
  if Assigned(FView.FieldEntity) then
  begin
    vIndex := TRadioGroup(FControl).Items.IndexOfObject(FView.FieldEntity);
    TRadioGroup(FControl).ItemIndex := vIndex;
  end
  else
    TRadioGroup(FControl).ItemIndex := -1;

  TRadioGroup(FControl).Enabled := Assigned(FView.FieldEntity) and (FView.State = vsFullAccess);
end;

procedure TVCLEnumFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TRadioGroup(FControl).OnClick := AHandler;
end;

{ TVCLBoolFieldEditor }

function TVCLBoolFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FNeedCreateCaption := False;

  Result := TCheckBox.Create(nil);

  if Assigned(FCreateParams) and (FCreateParams.IndexOfName('Caption') >= 0) then
    TCheckBox(Result).Caption := FCreateParams.Values['Caption']
  else
    TCheckBox(Result).Caption := FOwner.GetFieldTranslation(FFieldDef);

  if Assigned(FCreateParams) and (FCreateParams.IndexOfName('Hint') >= 0) then
    TCheckBox(Result).Hint := FCreateParams.Values['Hint']
  else
    TCheckBox(Result).Hint := FOwner.GetFieldTranslation(FFieldDef, tpHint);
end;

procedure TVCLBoolFieldEditor.DoOnChange;
begin
  SetFieldValue(TCheckBox(FControl).Checked);
end;

procedure TVCLBoolFieldEditor.FillEditor;
begin
  if VarIsNull(FView.FieldValue) then
    TCheckBox(FControl).Checked := False
  else
    TCheckBox(FControl).Checked := FView.FieldValue;

  TCheckBox(FControl).Enabled := FView.State = vsFullAccess;
end;

procedure TVCLBoolFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TCheckBox(FControl).OnClick := AHandler;
end;

{ TVCLImageViewer }

function TVCLImageViewer.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TImage.Create(nil);
end;

procedure TVCLImageViewer.FillEditor;
var
  vPicture: TPicture;
  vStream: TStream;
begin
  vStream := FView.FieldStream;
  if vStream = nil then
  begin
    TImage(FControl).Picture := nil;
    Exit;
  end;

  vPicture := TPicture.Create;
  try
    vStream.Position := 0;
    vPicture.Graphic.LoadFromStream(vStream);
    TImage(FControl).Picture.Assign(vPicture);
  finally
    vPicture.Graphic := nil;
    vPicture.Free;
  end;

  TImage(FControl).Enabled := FView.State = vsFullAccess;
end;

{ TVCLMemoFieldEditor }

function TVCLMemoFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TMemo.Create(nil);
  TMemo(Result).ScrollBars := ssVertical;
  TMemo(Result).WantReturns := True;
  TMemo(Result).ParentFont := True;
  TMemo(Result).ParentColor := True;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TMemo(Result).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TVCLMemoFieldEditor.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TMemo(FControl).Text);
end;

procedure TVCLMemoFieldEditor.FillEditor;
var
  vEdit: TMemo;
begin
  vEdit := TMemo(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Text := '';
    vEdit.Enabled := False;
  end
  else begin
    vEdit.Enabled := True;
    vEdit.Text := FView.FieldValue;
    vEdit.SelStart := 1000;
    vEdit.ReadOnly := FView.State < vsFullAccess;
  end;
end;

procedure TVCLMemoFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TMemo(FControl).OnChange := AHandler;
end;

{ TVCLTimeFieldEditor }

function TVCLTimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TDateTimePicker.Create(nil);

  TDateTimePicker(Result).DateMode := dmUpDown;
  TDateTimePicker(Result).Kind := dtkTime;
end;

procedure TVCLTimeFieldEditor.DoOnChange;
begin
  SetFieldValue(TDateTimePicker(FControl).Time);
end;

procedure TVCLTimeFieldEditor.FillEditor;
var
  vEdit: TDateTimePicker;
begin
  vEdit := TDateTimePicker(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.Time := 0;
  end
  else begin
    vEdit.Enabled := FView.State > vsDisabled;
    vEdit.Time := FView.FieldValue;
  end;
end;

procedure TVCLTimeFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TDateTimePicker(FControl).OnChange := AHandler;
end;

{ TVCLMaskFieldEditor }

function TVCLMaskFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vMask: string;
begin
  Result := TMaskEdit.Create(nil);

  vMask := '';
  if Assigned(FFieldDef)then
  begin
    if SameText('phone', FFieldDef.StyleName) then
      vMask := '!\+9 (999\) 000-00-00;1;_'
    else if SameText('INN', FFieldDef.StyleName) then
      vMask := '000000000009;0;_';
  end;
  TMaskEdit(Result).EditMask := vMask;
end;

procedure TVCLMaskFieldEditor.DoOnChange;
begin
  SetFieldValue(TMaskEdit(FControl).Text);
end;

procedure TVCLMaskFieldEditor.FillEditor;
var
  vEdit: TMaskEdit;
begin
  vEdit := TMaskEdit(FControl);
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

procedure TVCLMaskFieldEditor.OnEditorClick(Sender: TObject);
begin
  if SameText('email', FFieldDef.StyleName) then
    TPresenter(FPresenter).OpenFile('mailto:' + FView.FieldValue)
  else if SameText('url', FFieldDef.StyleName) then
    TPresenter(FPresenter).OpenFile(FView.FieldValue);
end;

procedure TVCLMaskFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TMaskEdit(FControl).OnChange := AHandler;
end;

{ TVCLColorEditor }

procedure TVCLColorEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FColorDialog);
  FreeAndNil(FSelectBtn);
  FreeAndNil(FClearBtn);
  FreeAndNil(FPaintBox);
end;

function TVCLColorEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
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

procedure TVCLColorEditor.FillEditor;
begin
  FColorDialog.Color := FView.FieldValue;
  FSelectBtn.Enabled := FView.State = vsFullAccess;
  FClearBtn.Enabled := FSelectBtn.Enabled;
end;

procedure TVCLColorEditor.OnClearBtnClick(Sender: TObject);
begin
  SetFieldValue(cNullColor);
  FColorDialog.Color := cNullColor;
  FPaintBox.Invalidate;
end;

procedure TVCLColorEditor.OnPaint(Sender: TObject);
begin
  if FColorDialog.Color = cNullColor then
    FPaintBox.Canvas.Brush.Color := clBtnFace
  else
    FPaintBox.Canvas.Brush.Color := FColorDialog.Color;
  FPaintBox.Canvas.FillRect(FPaintBox.ClientRect);
  if FColorDialog.Color = cNullColor then
    FPaintBox.Canvas.TextOut(5, 5,  TInteractor(FView.Interactor).Translate('txtNotAssigned', 'Не задан'));
end;

procedure TVCLColorEditor.OnSelectBtnClick(Sender: TObject);
begin
  if FColorDialog.Execute({TWinControl(FInnerControl).Handle D7}) then
  begin
    SetFieldValue(FColorDialog.Color);
    FPaintBox.Invalidate;
  end;
end;

{ TVCLDateTimeFieldEditor }

function TVCLDateTimeFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vDef: TSimpleFieldDef;
begin
  Result := TDateTimePicker.Create(nil);

  TDateTimePicker(Result).DateMode := dmUpDown;
  TDateTimePicker(Result).Kind := dtkDate;

  vDef := TSimpleFieldDef(FFieldDef);
  if not VarIsNull(vDef.MaxValue) then
    TDateTimePicker(Result).MaxDate := vDef.MaxValue;
  if not VarIsNull(vDef.MinValue) then
    TDateTimePicker(Result).MinDate := vDef.MinValue;
end;

procedure TVCLDateTimeFieldEditor.DoOnChange;
var
  vDateTime: TDateTime;
begin
  vDateTime := TDateTimePicker(FControl).Date + TDateTimePicker(FControl).Time;
  SetFieldValue(vDateTime);
end;

procedure TVCLDateTimeFieldEditor.FillEditor;
var
  vEdit: TDateTimePicker;
begin
  vEdit := TDateTimePicker(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Enabled := False;
    vEdit.Date := 0;
    vEdit.Time := 0;
  end
  else begin
    vEdit.Enabled := FView.State > vsDisabled;
    vEdit.Date := Int(FView.FieldValue);
    vEdit.Time := Frac(FView.FieldValue);
  end;
end;

procedure TVCLDateTimeFieldEditor.SetParent(const AParent: TUIArea);
begin
  inherited SetParent(AParent);

  if Assigned(AParent) then
    TDateTimePicker(FControl).Format := 'dd.MM.yyyy HH:mm:ss';
end;

procedure TVCLDateTimeFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TDateTimePicker(FControl).OnChange := AHandler;
end;

{ TVCLFileNameFieldEditor }

procedure TVCLFileNameFieldEditor.BeforeExecute(Sender: TObject);
begin
  if (Length(FText.Text) > 0) and DirectoryExists(ExtractFileDir(FText.Text)) then
    FAction.Dialog.InitialDir := ExtractFileDir(FText.Text);
end;

procedure TVCLFileNameFieldEditor.DoBeforeFreeControl;
begin
  FreeAndNil(FAction);
end;

function TVCLFileNameFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vBase: TPanel;
begin
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
  FBtn.Images := TDragImageList(FUIBuilder.Images[16]);
  FBtn.Action := FAction;

  FText := TEdit.Create(nil);
  FText.Align := alClient;
  FText.Parent := vBase;

  Result := vBase;
end;

procedure TVCLFileNameFieldEditor.DoOnChange;
begin
  SetFieldValue(FText.Text);
end;

procedure TVCLFileNameFieldEditor.FillEditor;
begin
  TPanel(FControl).Caption := ''; //reset caption after TFieldArea.Create (FControl.Name := FFieldDef.Name;)

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

  FAction.Visible := FView.State >= vsSelectOnly;
end;

procedure TVCLFileNameFieldEditor.OnAccept(Sender: TObject);
begin
  FText.Text := FAction.Dialog.FileName;
  FText.Hint := FText.Text;
end;

procedure TVCLFileNameFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  FText.OnChange := AHandler;
end;

{ TVCLSelectFolderFieldEditor }

procedure TVCLSelectFolderFieldEditor.BeforeExecute(Sender: TObject);
begin
  if (Length(FText.Text) > 0) and DirectoryExists(FText.Text) then
    FAction.Folder := FText.Text;
end;

procedure TVCLSelectFolderFieldEditor.BrowseForFolder1Accept(Sender: TObject);
begin
  FText.Text := FAction.Folder;
end;

procedure TVCLSelectFolderFieldEditor.DoBeforeFreeControl;
begin
  FreeAndNil(FAction);
end;

function TVCLSelectFolderFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vBase: TPanel;
begin
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
  FBtn.Images := TDragImageList(FUIBuilder.Images[16]);
  FBtn.Action := FAction;

  FText := TEdit.Create(nil);
  FText.Align := alClient;
  FText.Parent := vBase;

  Result := vBase;
end;

procedure TVCLSelectFolderFieldEditor.DoOnChange;
begin
  SetFieldValue(FText.Text);
end;

procedure TVCLSelectFolderFieldEditor.FillEditor;
begin
  TPanel(FControl).Caption := ''; //reset caption after TFieldArea.Create (FControl.Name := FFieldDef.Name;)

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

  FAction.Visible := FView.State > vsSelectOnly;
end;

procedure TVCLSelectFolderFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  FText.OnChange := AHandler;
end;

{ TVCLImageByString }

function TVCLImageByString.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TImage.Create(nil);
  TImage(Result).Transparent := True;
  TImage(Result).Center := True;
end;

procedure TVCLImageByString.FillEditor;
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
    TImage(FControl).Picture.LoadFromStream(vStream);
  end;
end;

{ TTextSelector }

function TVCLTextSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TComboBox.Create(nil);

  TComboBox(Result).OnDropDown := CBOnInitPopup;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TComboBox(Result).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;
end;

procedure TVCLTextSelector.DoOnChange;
begin
  SetFieldValue(TComboBox(FControl).Text);
end;

procedure TVCLTextSelector.FillEditor;
var
  vEdit: TComboBox;
begin
  FillList;

  vEdit := TComboBox(FControl);
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

procedure TVCLTextSelector.FillList;
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
          TComboBox(FControl).Items.Add(vCollection[i].FieldValues[vFieldName])
        else
          TComboBox(FControl).Items.Add(vCollection[i]['Name']);

      TComboBox(FControl).Text := FView.FieldValue;
    end;
  end;
  TComboBox(FControl).Items.EndUpdate;
end;

procedure TVCLTextSelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TVCLTextSelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TComboBox(FControl).OnChange := AHandler;
end;

{ TVCLEnumEditor }

procedure TVCLEnumEditor.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

function TVCLEnumEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  if not Assigned(FEnum) then
    FEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);

  if FFieldDef.StyleName = 'radio' then
  begin
    Result := TRadioGroup.Create(nil);
    TRadioGroup(Result).Name := 'radio';
    TRadioGroup(Result).Caption := '';
    FNeedCreateCaption := False;
  end
  else
  begin
    Result := TComboBox.Create(nil);
    TComboBox(Result).Style := csDropDownList;
    if Assigned(FCreateParams) then
    begin
      if FCreateParams.Values['DropDownListStyle'] = 'Fixed' then
        TComboBox(Result).Style := csDropDown
      else if FCreateParams.Values['DropDownListStyle'] = 'Edit' then
        TComboBox(Result).Style := csDropDownList;
    end;
    TComboBox(Result).OnDropDown := CBOnInitPopup;
  end;
end;

procedure TVCLEnumEditor.DoOnChange;
begin
  if FFieldDef.StyleName = 'radio' then
  begin
    if FFieldDef.HasFlag(cRequired) then
      SetFieldValue(TRadioGroup(FControl).ItemIndex + 1)
    else
      SetFieldValue(TRadioGroup(FControl).ItemIndex);
  end
  else
  begin
    if FFieldDef.HasFlag(cRequired) then
      SetFieldValue(TComboBox(FControl).ItemIndex + 1)
    else
      SetFieldValue(TComboBox(FControl).ItemIndex);
  end;
end;

procedure TVCLEnumEditor.FillEditor;
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
    vEdit := TComboBox(FControl);
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

procedure TVCLEnumEditor.FillList;
var
  vEnumItem: TEnumItem;
  vItems: TStrings;
begin
  if FFieldDef.StyleName = 'radio' then
  begin
    vItems := TRadioGroup(FControl).Items;
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

procedure TVCLEnumEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  if FFieldDef.StyleName = 'radio' then
    TRadioGroup(FControl).OnClick := AHandler
  else
    TComboBox(FControl).OnChange := AHandler;
end;

{ TVCLGraphicEnumSelector }

procedure TVCLGraphicEnumSelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TVCLGraphicEnumSelector.CBOnDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  vID: Integer;
  vCanvas: TCanvas;
begin
  vCanvas := TComboBox(Control).Canvas;
  vCanvas.FillRect(Rect);
  vCanvas.Pen.Color := vCanvas.Font.Color;

  vID := Index;
  if FFieldDef.HasFlag(cRequired) then
    vID := vID + 1;

  DoDrawItem(vCanvas, vID, Rect, State);
end;

function TVCLGraphicEnumSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  if not Assigned(FEnum) then
    FEnum := TDomain(FView.Domain).Configuration.StateMachines.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);

  Result := TComboBox.Create(nil);
  TComboBox(Result).Style := csOwnerDrawFixed;
  TComboBox(Result).OnDropDown := CBOnInitPopup;
  TComboBox(Result).OnDrawItem := CBOnDrawItem;
end;

procedure TVCLGraphicEnumSelector.DoDrawItem(const ACanvas: TCanvas; const AID: Integer; const ARect: TRect;
  AState: TOwnerDrawState);
begin
end;

procedure TVCLGraphicEnumSelector.DoOnChange;
begin
  if FFieldDef.HasFlag(cRequired) then
    SetFieldValue(TComboBox(FControl).ItemIndex + 1)
  else
    SetFieldValue(TComboBox(FControl).ItemIndex);
end;

procedure TVCLGraphicEnumSelector.FillEditor;
var
  vEdit: TComboBox;
  vItemIndex: Integer;
begin
  FillList;

  vEdit := TComboBox(FControl);
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

procedure TVCLGraphicEnumSelector.FillList;
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

procedure TVCLGraphicEnumSelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TComboBox(FControl).OnChange := AHandler;
end;

{ TVCLLineStyleSelector }

procedure TVCLLineStyleSelector.DoDrawItem(const ACanvas: TCanvas; const AID: Integer; const ARect: TRect;
  AState: TOwnerDrawState);
begin
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Style := Graphics.TPenStyle(AID);

  ACanvas.MoveTo(ARect.Left + 8, ARect.CenterPoint.Y);
  ACanvas.LineTo(ARect.Right - 8, ARect.CenterPoint.Y);
end;

{ TVCLPagesFieldEditor }

function TVCLPagesFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vPC: TPageControl;
  vTabLayout: TLayout;
  vChildArea: TUIArea;
  vHideTabs: Boolean;
  vTabSheet: TTabSheet;
  i: Integer;
begin
  FNeedCreateCaption := False;

  vPC := TPageControl.Create(nil);
  Result := vPC;
  vPC.DoubleBuffered := True;
  vPC.Width := ALayout.Width;
  vPC.Height := ALayout.Height;

  CopyFontSettings(vPC.Font, ALayout);
  vPC.Align := TAlign(ALayout.Align);
  CopyMargins(vPC, ALayout);
  vPC.Anchors := ALayout.Anchors;
  vPC.Style := TTabStyle(ALayout.Page_Style);
  if (ALayout.Page_Style <> psTabs) and (ALayout.Items.Count > 0) then
  begin
    vPC.Left := ALayout.Left + ALayout.Items[0].Left;
    vPC.Top := ALayout.Top + ALayout.Items[0].Top;
  end
  else begin
    vPC.Left := ALayout.Left;
    vPC.Top := ALayout.Top;
  end;

  vHideTabs := not ALayout.ShowCaption or ((ALayout.Items.Count > 0) and not ALayout.Items[0].ShowCaption);
  if not vHideTabs then
  begin
    vPC.RaggedRight := False;
    vPC.TabPosition := TTabPosition(ALayout.Page_Position);
    vPC.TabHeight := ALayout.Page_Height;
  end;

  // Нужно прописывать родителя, чтобы создавать вложенные сцены
  vPC.Parent := TWinControl(GetVCLControl(AParent));

  for i := 0 to ALayout.Items.Count - 1 do
  begin
    vTabLayout := ALayout.Items[i];

    vChildArea := TPresenter(FPresenter).CreateArea(FOwner, FView.Parent, vTabLayout);
    vTabSheet := TTabSheet(GetVCLControl(vChildArea));
    // It should be set here, because parent area still has no control
    vTabSheet.Parent := vPC;
    vTabSheet.PageControl := vPC;
    vTabSheet.TabVisible := False;
    FOwner.AddArea(vChildArea);

    TInteractor(FView.Interactor).UIBuilder.CreateChildAreas(vChildArea, vChildArea.View, vTabLayout, '');
  end;
end;

procedure TVCLPagesFieldEditor.FillEditor;
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

  if (vTag < TPageControl(FControl).PageCount) and (TPageControl(FControl).ActivePageIndex <> vTag) then
    TPageControl(FControl).ActivePageIndex := vTag;
end;

procedure TVCLPagesFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TPageControl(FControl).OnChange := AHandler;
end;

{ TVCLProgress }

function TVCLProgress.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TProgressBar.Create(nil);
  TProgressBar(Result).BarColor := AlphaColorToColor(ALayout.Color);
  FNeedCreateCaption := False;
  TProgressBar(Result).Step := 1;
  TProgressBar(Result).Smooth := True;
  TProgressBar(Result).Position := 0;
  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TProgressBar(Result).Max := TSimpleFieldDef(FFieldDef).MaxValue
  else
    TProgressBar(Result).Max := 100;
end;

procedure TVCLProgress.FillEditor;
begin
  inherited;
  TProgressBar(FControl).Position := FView.FieldValue;
end;

{ TVCLFlagsEditor }

procedure TVCLFlagsEditor.CLBOnClickCheck(Sender: TObject);
begin
  FOwner.OnChange(Sender);
end;

function TVCLFlagsEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FEnum := TDomain(FView.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(FFieldDef).Dictionary);
  Result := TCheckListBox.Create(nil);
  TCheckListBox(Result).OnClickCheck := CLBOnClickCheck;
end;

procedure TVCLFlagsEditor.DoOnChange;
var
  i: Integer;
  vFlagsValue: Integer;
begin
  vFlagsValue := 0;
  for i := 0 to TCheckListBox(FControl).Items.Count - 1 do
    if TCheckListBox(FControl).Checked[i] then
      vFlagsValue := vFlagsValue or TEnumItem(TCheckListBox(FControl).Items.Objects[i]).ID;

  SetFieldValue(vFlagsValue);
end;

procedure TVCLFlagsEditor.FillEditor;
begin
  FillList;
  TCheckListBox(FControl).Enabled := FView.State >= vsSelectOnly;
end;

procedure TVCLFlagsEditor.FillList;
var
  vList: TCheckListBox;
  vEnumItem: TEnumItem;
  vValue: Integer;
  vIndex: Integer;
begin
  vList := TCheckListBox(FControl);
  vValue := FView.FieldValue;
  vList.Items.BeginUpdate;
  try
    vList.Items.Clear;
    for vEnumItem in FEnum do
    begin
      if vEnumItem.ID > 0 then
      begin
        vIndex := vList.Items.AddObject(vEnumItem.DisplayText, vEnumItem);
        vList.Checked[vIndex] := (vEnumItem.ID and vValue) <> 0;
      end;
    end;
  finally
    vList.Items.EndUpdate;
  end;
end;

procedure TVCLFlagsEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  if Assigned(AHandler) then
    TCheckListBox(FControl).OnClickCheck := CLBOnClickCheck
  else
    TCheckListBox(FControl).OnClickCheck := nil;
end;

{ TDEImagedAction }

procedure TVCLImagedAction.DoBeforeFreeControl;
begin
  if Assigned(FActionView) then
  begin
    FActionView.RemoveListener(Self);
    FActionView := nil;
  end;
end;

function TVCLImagedAction.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vActionName: string;
  vImageSize: Integer;
  vButton: TButton;
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

  vButton := TButton.Create(nil);
  vButton.Images := TDragImageList(FUIBuilder.Images[vImageSize]);
  vButton.OnClick := OnButtonClick;

  Result := vButton;
end;

procedure TVCLImagedAction.FillEditor;
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

procedure TVCLImagedAction.OnButtonClick(Sender: TObject);
begin
  if Assigned(FActionView) then
    FActionView.ExecuteAction(FOwner.Holder);
end;

procedure TVCLImagedAction.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  //TButton(Result).OnClick := AHandler;
end;

{ TVCLBoolImages }

function TVCLBoolImages.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TImage.Create(nil);
  TImage(Result).Transparent := True;
end;

procedure TVCLBoolImages.FillEditor;
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

{ TVCLColorPicker }

function TVCLColorPicker.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TColorBox.Create(nil);
end;

procedure TVCLColorPicker.DoOnChange;
begin
  SetFieldValue(TColorBox(FControl).Selected);
end;

procedure TVCLColorPicker.FillEditor;
var
  vEdit: TColorBox;
begin
  vEdit := TColorBox(FControl);

  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Selected := vEdit.NoneColorColor;
    vEdit.Enabled := False;
  end else
  begin
    vEdit.Selected := TColor(FView.FieldValue);
    vEdit.Enabled := FView.State >= vsFullAccess;
  end;
end;

procedure TVCLColorPicker.SetParent(const AParent: TUIArea);
begin
  inherited SetParent(AParent);
  TColorBox(FControl).Style := [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames];
end;

procedure TVCLColorPicker.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TColorBox(FControl).OnChange := AHandler;
end;

{ TVCLIntegerFlagsEditor }

procedure TVCLIntegerFlagsEditor.CLBOnClickCheck(Sender: TObject);
begin
  FOwner.OnChange(Sender);
end;

procedure TVCLIntegerFlagsEditor.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FCaptions);
end;

function TVCLIntegerFlagsEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vHorzLayout: Boolean;
begin
  Result := TCheckListBox.Create(nil);
  TCheckListBox(Result).OnClickCheck := CLBOnClickCheck;

  vHorzLayout := False;
  FDisplayFlagCount := 8;

  if Assigned(FCreateParams) then
  begin
    FDisplayFlagCount := Min(32, StrToIntDef(FCreateParams.Values['DisplayFlagCount'], 8));
    if FCreateParams.IndexOfName('ItemCaptions') > -1 then
      FCaptions := CreateDelimitedList(FCreateParams.Values['ItemCaptions'], ';');
    vHorzLayout := StrToIntDef(FCreateParams.Values['HorzLayout'], 0) = 1;
  end;

  if vHorzLayout then
    TCheckListBox(Result).Columns := FDisplayFlagCount;
end;

procedure TVCLIntegerFlagsEditor.DoOnChange;
var
  i: Integer;
  vFlagsValue: Integer;
begin
  vFlagsValue := 0;
  for i := 0 to TCheckListBox(FControl).Items.Count - 1 do
    if TCheckListBox(FControl).Checked[i] then
      vFlagsValue := vFlagsValue or NativeInt(TCheckListBox(FControl).Items.Objects[i]);

  SetFieldValue(vFlagsValue);
end;

procedure TVCLIntegerFlagsEditor.DoOnExit(Sender: TObject);
begin
  TCheckListBox(FControl).ItemIndex := -1;
end;

procedure TVCLIntegerFlagsEditor.FillEditor;
begin
  FillList;
  TCheckListBox(FControl).Enabled := FView.State >= vsSelectOnly;
end;

procedure TVCLIntegerFlagsEditor.FillList;
var
  vList: TCheckListBox;
  vText: string;
  vBits, vBit: Integer;
  i: Integer;
begin
  vList := TCheckListBox(FControl);
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
      vList.Checked[i] := (vBit and vBits) <> 0;
    end;
  finally
    vList.Items.EndUpdate;
  end;
end;

procedure TVCLIntegerFlagsEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  if Assigned(AHandler) then
    TCheckListBox(FControl).OnClickCheck := CLBOnClickCheck
  else
    TCheckListBox(FControl).OnClickCheck := nil;
end;

{ TVCLLogEditor }

procedure TVCLLogEditor.DoBeforeFreeControl;
begin
  FreeAndNil(FData);
end;

function TVCLLogEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FListView := TListView.Create(nil);
  FListView.OnData := OnListViewData;
  FListView.OwnerData := True;
  FListView.Columns.Add.AutoSize := True;
  FListView.ShowColumnHeaders := False;
  FListView.ReadOnly := True;

  FData := TStringList.Create;

  Result := FListView;
end;

procedure TVCLLogEditor.FillEditor;
begin
  inherited;

  FData.Text := FView.FieldValue;

  FListView.Items.Count := FData.Count;

  FListView.Refresh;
end;

procedure TVCLLogEditor.OnListViewData(Sender: TObject; Item: TListItem);
begin
  Item.Caption := FData[FData.Count - Item.Index - 1];
end;

procedure TVCLLogEditor.SetParent(const Value: TUIArea);
begin
  inherited;
  FListView.ViewStyle := vsReport;
end;

{ TVCLSelectedCaptionBoolFieldEditor }

function TVCLSelectedCaptionBoolFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FNeedCreateCaption := False;

  Result := TLabel.Create(nil);
  TLabel(Result).OnClick := OnClick;
  if Assigned(FCreateParams) then
    TLabel(Result).Caption := FCreateParams.Values['Caption'];
  TLabel(Result).Transparent := False;
  TLabel(Result).Cursor := crHandPoint;

  if ALayout.Kind = lkPanel then
  begin
    FSelectBackColor := AlphaColorToColor($FF5132);
    if Assigned(FCreateParams) and (FCreateParams.IndexOfName('select_backcolor') > -1) then
      FSelectBackColor := AlphaColorToColor(StrToIntDef('$' + FCreateParams.Values['select_backcolor'], 0));

    FDefaultTextColor := AlphaColorToColor(ALayout.Font.Color);
    TLabel(Result).Alignment := ALayout.Alignment;
  end;
end;

procedure TVCLSelectedCaptionBoolFieldEditor.FillEditor;
begin
  if VarIsNull(FView.FieldValue) then
    FSelected := False
  else
    FSelected := FView.FieldValue;

  UpdateView;
end;

procedure TVCLSelectedCaptionBoolFieldEditor.OnClick(Sender: TObject);
begin
  FSelected := not FSelected;

  SetFieldValue(FSelected);

  UpdateView;
end;

procedure TVCLSelectedCaptionBoolFieldEditor.UpdateView;
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

TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, '', TVCLTextFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, 'phone', TVCLTextFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, 'email', TVCLMaskFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, 'url', TVCLMaskFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, 'INN', TVCLMaskFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, 'memo', TVCLMemoFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, 'log', TVCLLogEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, 'info', TVCLTextInfo);
TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, 'dir', TVCLSelectFolderFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, 'file', TVCLFileNameFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, 'ImageByString', TVCLImageByString);
TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, 'selector', TVCLTextSelector);
TPresenter.RegisterControlClass('Windows.VCL', uiIntegerEdit, '', TVCLIntegerFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiIntegerEdit, 'simple', TVCLIntegerFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiIntegerEdit, 'info', TVCLTextInfo);
TPresenter.RegisterControlClass('Windows.VCL', uiIntegerEdit, 'progress', TVCLProgress);
TPresenter.RegisterControlClass('Windows.VCL', uiIntegerEdit, 'pages', TVCLPagesFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiIntegerEdit, 'flags', TVCLIntegerFlagsEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiEnumEdit, '', TVCLEnumEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiEnumEdit, 'radio', TVCLEnumEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiEnumEdit, 'info', TVCLTextInfo);
TPresenter.RegisterControlClass('Windows.VCL', uiEnumEdit, 'line_style', TVCLLineStyleSelector);
TPresenter.RegisterControlClass('Windows.VCL', uiEnumEdit, 'pages', TVCLPagesFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiFlagEdit, '', TVCLFlagsEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiEntityEdit, 'enum', TVCLEnumFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiFloatEdit, '', TVCLFloatFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiFloatEdit, 'currency_rate', TVCLFloatFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiFloatEdit, 'info', TVCLTextInfo);
TPresenter.RegisterControlClass('Windows.VCL', uiDateEdit, '', TVCLDateFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiDateEdit, 'time', TVCLTimeFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiDateEdit, 'datetime', TVCLDateTimeFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiDateEdit, 'info', TVCLTextInfo);
TPresenter.RegisterControlClass('Windows.VCL', uiCurrencyEdit, '', TVCLFloatFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiCurrencyEdit, 'info', TVCLTextInfo);
TPresenter.RegisterControlClass('Windows.VCL', uiBoolEdit, '', TVCLBoolFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiBoolEdit, 'simple', TVCLBoolFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiBoolEdit, 'imaged_action', TVCLImagedAction);
TPresenter.RegisterControlClass('Windows.VCL', uiBoolEdit, 'images', TVCLBoolImages);
TPresenter.RegisterControlClass('Windows.VCL', uiBoolEdit, 'pages', TVCLPagesFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiBoolEdit, 'selected_caption', TVCLSelectedCaptionBoolFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiColorEdit, '', TVCLColorEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiColorEdit, 'simple', TVCLColorPicker);
TPresenter.RegisterControlClass('Windows.VCL', uiBLOBEdit, 'image', TVCLImageViewer);
TPresenter.RegisterControlClass('Windows.VCL', uiEntityEdit, 'info', TVCLTextInfo);
TPresenter.RegisterControlClass('Windows.VCL', uiEntityEdit, 'pages', TVCLPagesFieldEditor);

end.
