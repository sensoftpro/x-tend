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

  Vcl.Samples.Spin;

type
  TTextInfo = class (TFieldArea)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
  end;

  TIntegerFieldEditor = class (TFieldArea)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure FocusedChanged(const AFocused: Boolean); override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TTextFieldEditor = class (TFieldArea)
  private
    procedure OnPhoneKeyPress(Sender: TObject; var Key: Char);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TFilenameFieldEditor = class (TFieldArea)
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

implementation

uses
  Generics.Collections, TypInfo, Variants, SysUtils, Windows,
  Forms, Math, DateUtils, Messages,

  uConfiguration, uDomain, uInteractor, uPresenter, uWinVCLPresenter, uCollection, uEntity, uConsts,
  uUtils, UITypes;

{ TTextInfo }

function TTextInfo.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TLabel.Create(nil);
  TLabel(Result).Transparent := True;
  TLabel(Result).WordWrap := True;
  if Assigned(FCreateParams) and (FCreateParams.Values['WordWrap'] = 'False') then
  begin
    TLabel(Result).WordWrap := False;
    TLabel(Result).EllipsisPosition := epEndEllipsis;
  end;
  TLabel(Result).AutoSize := False;
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

function TIntegerFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TSpinEdit.Create(nil);

  with TSpinEdit(Result) do
  begin
    if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
      MaxValue := TSimpleFieldDef(FFieldDef).MaxValue;

    if not VarIsNull(TSimpleFieldDef(FFieldDef).MinValue) then
      MinValue := TSimpleFieldDef(FFieldDef).MinValue;

//    if (Length(FFieldDef.Format) > 0) or FFieldDef.Definition.FieldExists('Format') then
//      DisplayFormat := GetFormat;
  end;
end;

procedure TIntegerFieldEditor.DoOnChange;
begin
  if TSpinEdit(FControl).Value = 0 then
    SetFieldValue(Null)
  else
    SetFieldValue(TSpinEdit(FControl).Value);
end;

procedure TIntegerFieldEditor.FillEditor;
var
  vEdit: TSpinEdit;
begin
  vEdit := TSpinEdit(FControl);

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
    begin
      vEdit.Color := clBtnFace;
      vEdit.TabStop := False;
    end
    else begin
      vEdit.Color := clWindow;
      vEdit.TabStop := TabStop;
    end;
  end;
end;

procedure TIntegerFieldEditor.FocusedChanged(const AFocused: Boolean);
begin
  inherited;
  if not AFocused then
  begin
    // обрабатываем 0
    SetFieldValue(TSpinEdit(FControl).Value);
  end;
end;

procedure TIntegerFieldEditor.SwitchChangeHandlers(
  const AHandler: TNotifyEvent);
begin
  inherited;
  TSpinEdit(FControl).OnChange := AHandler
end;

{ TDETextEditControl }

function TTextFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  inherited;
  Result := TEdit.Create(nil);
  TEdit(Result).ParentColor := True;

  if not VarIsNull(TSimpleFieldDef(FFieldDef).MaxValue) then
    TEdit(Result).MaxLength := TSimpleFieldDef(FFieldDef).MaxValue;

  if SameText('phone', FFieldDef.StyleName) then
    TEdit(Result).OnKeyPress := OnPhoneKeyPress
  else
    TEdit(Result).OnKeyPress := nil;
end;

procedure TTextFieldEditor.DoOnChange;
begin
  //важно использовать именно EditingText, иначе при PostMessage(KeyDown) Value = Null
  SetFieldValue(TEdit(FControl).Text);
end;

procedure TTextFieldEditor.OnPhoneKeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, ['0'..'9', #8, '-', '+', '(', ')', ' ']) then Exit;

  Key := #0;
end;

procedure TTextFieldEditor.FillEditor;
var
  vEdit: TEdit;
begin
  vEdit := TEdit(FControl);
  if VarIsNull(FView.FieldValue) then
  begin
    vEdit.Text := '';
    vEdit.Enabled := False;
//    vEdit.Style.BorderStyle := GetDisabledBorderStyle;
    vEdit.Color := clBtnFace;
  end
  else
  begin
    vEdit.Text := FView.FieldValue;
    vEdit.Enabled := True;
    vEdit.ReadOnly := FView.State < vsFullAccess;

    if vEdit.ReadOnly then
    begin
//      vEdit.Style.BorderStyle := GetDisabledBorderStyle;
      vEdit.TabStop := False;
      vEdit.Color := clBtnFace;
    end
    else begin
      vEdit.TabStop := TabStop;
      vEdit.Color := clWindow;
    end;
  end;
end;

procedure TTextFieldEditor.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  TEdit(FControl).OnChange := AHandler;
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

  FBtn := TButton.Create(nil);
  FBtn.Align := alRight;
  FBtn.Parent := vBase;
  FBtn.Width := 40;
  FBtn.Images := TDragImageList(TInteractor(Interactor).Images[16]);
  FBtn.Action := FAction;

  FText := TEdit.Create(nil);
  FText.Align := alClient;
  FText.Parent := vBase;

  Result := vBase;
end;

procedure TFilenameFieldEditor.DoOnChange;
begin
  SetFieldValue(FText.Text);
end;

procedure TFilenameFieldEditor.FillEditor;
begin
  TPanel(FControl).Caption := ''; //reset caption after TFieldArea.Create (FControl.Name := FFieldDef.Name;)

  if VarIsNull(FView.FieldValue) then
  begin
    FText.Text := '';
    FText.Enabled := False;
//    FText.Style.BorderStyle := GetDisabledBorderStyle;
  end
  else
  begin
    FText.Text := FView.FieldValue;
    FText.Hint := FView.FieldValue;
    FText.Enabled := True;
    FText.ReadOnly := FView.State < vsSelectOnly;

    if FText.ReadOnly then
    begin
//      FText.Style.BorderStyle := GetDisabledBorderStyle;
      FText.Color := clBtnFace;
      FText.TabStop := False;
    end
    else begin
      FText.TabStop := TabStop;
      FText.Color := clWindow;
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
  FText.OnChange := AHandler;
end;

initialization

RegisterClasses([TBevel, TLabel]);

TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, '', TTextFieldEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiTextEdit, 'info', TTextInfo);
TPresenter.RegisterUIClass('Windows.VCL', uiIntegerEdit, '', TIntegerFieldEditor);

//TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, '', TTextFieldEditor);
//TPresenter.RegisterControlClass('Windows.VCL', uiTextEdit, 'info', TTextInfo);
//TPresenter.RegisterControlClass('Windows.VCL', uiIntegerEdit, '', TIntegerFieldEditor);

end.
