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

unit vclEntityEditors;

interface

uses
  Classes, Generics.Collections, StdCtrls, ExtCtrls, ActnList, Types, Menus, Forms, uInteractor, Graphics, uConsts,
  Buttons, vclArea, uUIBuilder, uLayout, uView, uEntity, uEntityList, uDefinition, Controls;

type
  TVCLEntitySelector = class(TVCLControl)
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

  TCloseQueryEvent = procedure(Sender: TObject; var ACanClose: Boolean) of object;

  TUIPopupWindow = class
  private
    FInvokedBy: TWinControl;
    FOnClose: TNotifyEvent;
    FOnCloseQuery: TCloseQueryEvent;
    procedure OnShow(Sender: TObject);
    function GetSizeable: Boolean;
    procedure SetSizeable(const Value: Boolean);
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnDeactivate(Sender: TObject);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
  protected
    FInnerControl: TForm;
    FInteractor: TInteractor;
    procedure DoOnBeforeShow; virtual;
    procedure DoOnShow; virtual;
  public
    constructor Create(const AInteractor: TInteractor); virtual;
    destructor Destroy; override;

    procedure ShowFor(const AElement: TWinControl; const APopupContext: string);
    procedure Close;

    property Sizeable: Boolean read GetSizeable write SetSizeable;
    property InvokedBy: TWinControl read FInvokedBy;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property Color: TColor read GetColor write SetColor;
    property Form: TForm read FInnerControl;
  end;

  TOnSelectEntityEvent = procedure (const ANewEntity: TEntity) of object;

  TVCLEntityFieldEditor = class(TVCLControl)
  private
    FBasePanel: TPanel;
    FSelectButton: TComboBox;
    FTextEdit: TEdit;
    FbtnAdd: TButton;
    FEntities: TEntityList;
    FTypeSelectionMenu: TPopupMenu;
  protected
    procedure UpdateVisibility; virtual;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SetFocused(const Value: Boolean); override;
    procedure DoDeinit; override;
  end;

  TVCLRadioEntitySelector = class(TVCLControl)
  private
    FEntities: TEntityList;
    procedure FillList;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure DoOnChange; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
  end;

  TVCLLinkedEntityFieldEditor = class(TVCLEntityFieldEditor)
  private
    FLabel: TLabel;
    procedure OnLabelClick(Sender: TObject);
  protected
    procedure DoBeforeFreeControl; override;
    procedure UpdateVisibility; override;
  end;

implementation

uses
  SysUtils, Windows, Messages, Variants, StrUtils,
  uDomain, uChangeManager, uObjectField, uSession,
  uPresenter, uWinVCLPresenter, uUtils, uEnumeration;

function IsAlpha(c: Char): Boolean;
begin
  Result := ((c >= 'а') and (c <= 'я')) or ((c >= 'А') and (c <= 'Я')) or
    ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z'));
end;

function VKeytoWideString(const AKey: Word): WideString;
var
  WBuff: array [0..255] of WideChar;
  KeyboardState: TKeyboardState;
  UResult: Integer;
begin
  Result := '';
  GetKeyBoardState (KeyboardState);
  ZeroMemory(@WBuff[0], SizeOf(WBuff));
  UResult := ToUnicode(AKey, MapVirtualKey(AKey, 0), KeyboardState, WBuff, Length(WBuff), 0);
  if UResult > 0 then
    SetString(Result, WBuff, UResult)
  else if UResult = -1 then
    Result := WBuff;
end;

{ TVCLEntityFieldEditor }

procedure TVCLEntityFieldEditor.DoBeforeFreeControl;
begin
  inherited;

  FreeAndNil(FTypeSelectionMenu);
  FreeAndNil(FEntities);
  FreeAndNil(FTextEdit);
end;

function TVCLEntityFieldEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vButtonLayout: TLayout;
  vAddArea: TUIArea;
begin
  FEntities := nil;

  FBasePanel := TPanel.Create(nil);
  FBasePanel.BevelOuter := bvNone;
  FBasePanel.ShowCaption := False;

  FSelectButton := TComboBox.Create(nil);
  with FSelectButton do
  begin
    Parent := FBasePanel;
    Align := alClient;
    Width := FBasePanel.Width;
    Style := csDropDownList;
  end;

  FSelectButton.Enabled := True;

  FTextEdit := TEdit.Create(nil);
  FTextEdit.Parent := FBasePanel;
  FTextEdit.Align := alClient;
  FTextEdit.Visible := False;
  FTextEdit.ReadOnly := True;
  FTextEdit.Text := '';
  FTextEdit.TabStop := False;
  FTextEdit.Color := clBtnFace;

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

procedure TVCLEntityFieldEditor.DoDeinit;
begin
  inherited;
  FSelectButton.Text := '';
  FSelectButton.Enabled := False;

  FbtnAdd.Visible := True; // todo: create option
end;

procedure TVCLEntityFieldEditor.FillEditor;
var
  vEntity: TEntity;
  vDefinition: TDefinition;
begin
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

procedure TVCLEntityFieldEditor.SetFocused(const Value: Boolean);
begin
  if Value and FSelectButton.CanFocus then
    FSelectButton.SetFocus;
end;

{procedure TVCLEntityFieldEditor.ShowPopup(const AFilter: string);
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
    FSelectPopup.ShowFor(TWinControl(FControl), '');
  finally
    TPresenter(vInteractor.Presenter).SetCursor(vPrevCursor);
  end;
end;}

procedure TVCLEntityFieldEditor.UpdateVisibility;
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

{ TVCLLinkedEntityFieldEditor }

procedure TVCLLinkedEntityFieldEditor.DoBeforeFreeControl;
begin
  inherited DoBeforeFreeControl;
  FreeAndNil(FLabel);
end;

procedure TVCLLinkedEntityFieldEditor.OnLabelClick(Sender: TObject);
var
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(FView.Interactor);
  vInteractor.ViewEntity(FView);
end;

procedure TVCLLinkedEntityFieldEditor.UpdateVisibility;
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
      FLabel := TLabel.Create(nil);
      FLabel.Align := alClient;
      FLabel.Parent := FBasePanel;
      FLabel.Font.Color := clBlue;
      FLabel.Font.Style := [fsUnderline];
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
    FTextEdit.Text := FSelectButton.Text;
  end
  else
  begin
    FTextEdit.Visible := False;
    if Assigned(FLabel) then
      FLabel.Visible := False;

    FSelectButton.Visible := True;
  end;
end;

{ TVCLEntitySelector }

procedure TVCLEntitySelector.CBOnInitPopup(Sender: TObject);
begin
  FillList;
end;

procedure TVCLEntitySelector.DoBeforeFreeControl;
begin
  FreeAndNil(FEntities);
end;

function TVCLEntitySelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(FView.Interactor);
  FEntities := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  Result := TComboBox.Create(nil);
  TComboBox(Result).Style := csDropDownList;
  TComboBox(Result).OnDropDown := CBOnInitPopup;

  FFlat := (ALayout.BevelOuter = lbkNone) and (ALayout.BevelInner = lbkNone);
end;

procedure TVCLEntitySelector.DoOnChange;
var
  vEntity: TEntity;
begin
  if TComboBox(FControl).ItemIndex >= 0 then
    vEntity := TEntity(TComboBox(FControl).Items.Objects[TComboBox(FControl).ItemIndex])
  else
    vEntity := nil;
  SetFieldEntity(vEntity);
end;

procedure TVCLEntitySelector.FillEditor;
var
  vEdit: TComboBox;
  vEntity: TEntity;
begin
  FillList;

  vEdit := TComboBox(FControl);
  vEntity := FView.FieldEntity;
  if Assigned(vEntity) then
  begin
    vEdit.Text := vEntity['Name'];
    vEdit.Enabled := FView.State >= vsSelectOnly;
  end
  else begin
    vEdit.Text := '';
    vEdit.Enabled := False;
  end;
end;

procedure TVCLEntitySelector.FillList;
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

procedure TVCLEntitySelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TComboBox(FControl).OnChange := AHandler;
end;

{ TVCLRadioEntitySelector }

procedure TVCLRadioEntitySelector.DoBeforeFreeControl;
begin
  FreeAndNil(FEntities);
end;

function TVCLRadioEntitySelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := TRadioGroup.Create(nil);
  TRadioGroup(Result).Name := 'radio';
  TRadioGroup(Result).Caption := '';
  FNeedCreateCaption := False;

  FEntities := TEntityList.Create(TInteractor(FView.Interactor).Domain, TInteractor(FView.Interactor).Session);
end;

procedure TVCLRadioEntitySelector.DoOnChange;
var
  vIndex: Integer;
begin
//  if FFieldDef.HasFlag(cRequired) then
//    vIndex := TcxRadioGroup(FControl.Control).ItemIndex + 1
//  else
  vIndex := TRadioGroup(FControl).ItemIndex;
  SetFieldEntity(FEntities[vIndex]);
end;

procedure TVCLRadioEntitySelector.FillEditor;
var
  vRadioEdit: TRadioGroup;
begin
  FillList;

  vRadioEdit := TRadioGroup(FControl);
  if VarIsNull(FView.FieldValue) or (FView.FieldValue = 0) then
  begin
    vRadioEdit.Enabled := False;
  end
  else begin
    vRadioEdit.Enabled := FView.State >= vsSelectOnly
  end;
end;

procedure TVCLRadioEntitySelector.FillList;
var
  vRadioItems: TStrings;
  vField: TEntityField;
  i: Integer;
  vEnt: TEntity;
begin
  vField := FView.ExtractEntityField;
  vField.GetEntitiesForSelect(TInteractor(FView.Interactor).Session, FEntities);

  vRadioItems := TRadioGroup(FControl).Items;
  vRadioItems.BeginUpdate;
  try
    vRadioItems.Clear;
    for i := 0 to FEntities.Count - 1 do
    begin
      vEnt := FEntities[i];
      if Assigned(vEnt) then
      begin
        vRadioItems.AddObject(vEnt.DisplayName, vEnt);
        if vEnt = TEntity(NativeInt(FView.FieldValue)) then
          TRadioGroup(FControl).ItemIndex := TRadioGroup(FControl).Items.Count - 1;
      end;
      //todo: обработать <не задано> если нужно
    end;
  finally
    vRadioItems.EndUpdate;
  end;
end;

procedure TVCLRadioEntitySelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  TRadioGroup(FControl).OnClick := AHandler;
end;

{ TUIPopupWindow }

type
  TPopupForm = class (TForm)
  private
    FPrevActiveWindow: HWND;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMActivateApp(var Message: TWMActivateApp); message WM_ACTIVATEAPP;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

procedure TPopupForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := (Style or WS_POPUP) and (not WS_DLGFRAME);
  with Params do
  begin
//    Style := WS_POPUP or WS_BORDER;
//    if CheckWin32Version(5, 1) then
//      WindowClass.Style := WindowClass.style or CS_DROPSHADOW;
  end;
end;

procedure TUIPopupWindow.Close;
begin
  TForm(FInnerControl).Close;
end;

constructor TUIPopupWindow.Create(const AInteractor: TInteractor);
begin
  FInteractor := AInteractor;
  FInnerControl := TPopupForm.Create(nil);
  FInnerControl.BorderStyle := bsSizeable;
  FInnerControl.OnDeactivate := DoOnDeactivate;
  FInnerControl.Position := poDesigned; // так нужно, иначе первый раз присвоение координат не отрабатывает
  FInnerControl.OnKeyDown := OnKeyDown;
  FInnerControl.OnShow := OnShow;
  FInnerControl.KeyPreview := True;
end;

destructor TUIPopupWindow.Destroy;
begin
  FreeAndNil(FInnerControl);
  inherited;
end;

procedure TUIPopupWindow.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close
end;

procedure TUIPopupWindow.OnShow(Sender: TObject);
begin
  DoOnShow;
end;

function TUIPopupWindow.GetColor: TColor;
begin
  Result := TForm(FInnerControl).Color;
end;

function TUIPopupWindow.GetSizeable: Boolean;
begin
  Result := TForm(FInnerControl).BorderStyle = bsSizeable;
end;

procedure TUIPopupWindow.DoOnBeforeShow;
begin
end;

procedure TUIPopupWindow.DoOnDeactivate(Sender: TObject);
var
  vCanClose: Boolean;
begin
  vCanClose := True;
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(Self, vCanClose);

  if vCanClose then
  begin
    TForm(FInnerControl).Hide;
    if Assigned(FOnClose) then
      FOnClose(Self);
  end;
end;

procedure TUIPopupWindow.DoOnShow;
begin
end;

procedure TUIPopupWindow.SetColor(const Value: TColor);
begin
  TForm(FInnerControl).Color := Value;
end;

procedure TUIPopupWindow.SetSizeable(const Value: Boolean);
begin
  if Value then
    TForm(FInnerControl).BorderStyle := bsSizeable
  else
    TForm(FInnerControl).BorderStyle := bsSingle;
end;

procedure TUIPopupWindow.ShowFor(const AElement: TWinControl; const APopupContext: string);
var
  vPoint: TPoint;
begin
  FInvokedBy := AElement;

  vPoint := AElement.ClientToScreen(Point(0, AElement.Height + 1));
  FInnerControl.Left := vPoint.X;
  FInnerControl.Top := vPoint.Y;

  DoOnBeforeShow;

  if (FInnerControl.Top + FInnerControl.Height) > Screen.DesktopHeight then
    FInnerControl.Top := FInnerControl.Top - FInnerControl.Height - AElement.Height;

  FInnerControl.Show;

//  SetWindowPos(TForm(FInnerControl).Handle, HWND_TOP, 0, 0, 0, 0,
//    SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW or SWP_NOACTIVATE);
//  TForm(FInnerControl).Visible := True;
//  Focused := True;
end;

procedure TPopupForm.WMActivate(var Message: TWMActivate);
begin
  inherited;
  if Message.Active <> WA_INACTIVE then
  begin
    FPrevActiveWindow := Message.ActiveWindow;
    SendMessage(FPrevActiveWindow, WM_NCACTIVATE, WPARAM(True), 0);
  end;
end;

procedure TPopupForm.WMActivateApp(var Message: TWMActivateApp);
begin
  inherited;
  if not Message.Active then
  begin
    SendMessage(FPrevActiveWindow, WM_NCACTIVATE, WPARAM(False), 0);
    Close;
  end;
end;

initialization

TPresenter.RegisterControlClass('Windows.VCL', uiEntityEdit, '', TVCLEntityFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiEntityEdit, 'simple', TVCLEntityFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiEntityEdit, 'link', TVCLLinkedEntityFieldEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiEntityEdit, 'select', TVCLEntitySelector);
TPresenter.RegisterControlClass('Windows.VCL', uiEntityEdit, 'radio', TVCLRadioEntitySelector);

end.
