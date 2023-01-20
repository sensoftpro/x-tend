{---------------------------------------------------------------------------------
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
 ---------------------------------------------------------------------------------}

unit vclPopupForm;

interface

uses
  Windows, Controls, Classes, ActnList, Forms, Menus, Graphics, StdCtrls, ComCtrls, ExtCtrls,

  uConsts, uEntity, uEntityList, uInteractor, cxTL;

type
  TCloseQueryEvent = procedure(Sender: TObject; var ACanClose: Boolean) of object;

  TUIPopupWindow = class
  private
    FInnerControl: TForm;
    FInvokedBy: TWinControl;
    FOnClose: TNotifyEvent;
    FOnCloseQuery: TCloseQueryEvent;
    FInteractor: TInteractor;
    procedure OnShow(Sender: TObject);
    function GetSizeable: Boolean;
    procedure SetSizeable(const Value: Boolean);
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoOnDeactivate(Sender: TObject);
    function GetColor: TColor;
    procedure SetColor(const Value: TColor);
  protected
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

  TSelectEntityPopup = class (TUIPopupWindow)
  private
    FList: TcxTreeList;
    FTopLabel, FBottomLabel: TLabel;
    FOnEntitySelected: TOnSelectEntityEvent;
    FAllData: TEntityList;
    FSelectedEntity: TEntity;
    FTopPanel: TPanel;
    FFilterEdit: TEdit;
    FSearchType: TSearchType;
    FFilterText: string;
    procedure UpdateCount;
    procedure OnDblClick(Sender: TObject);
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpdateSizes;
    procedure Reload;
    function IsMatchToFilter(const AEntity: TEntity): Boolean;
    procedure OnFilterChange(Sender: TObject);
  protected
    procedure DoOnBeforeShow; override;
  public
    constructor Create(const AInteractor: TInteractor); override;
    destructor Destroy; override;

    procedure Init(const AItems: TEntityList; const AIsSelectMode: Boolean;
      const ASelectedEntity: TEntity; const AFilter: string);

    property OnEntitySelected: TOnSelectEntityEvent read FOnEntitySelected write FOnEntitySelected;
  end;

implementation

uses
  SysUtils, Messages, Types,

  uDefinition, uDomain, uObjectField, uUtils;

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
  {$R PopupForm.dfm}

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
  //Focused := True;
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

{ TSelectEntityPopup }

constructor TSelectEntityPopup.Create(const AInteractor: TInteractor);
begin
  inherited;

  FTopPanel := TPanel.Create(nil);
  FTopPanel.Height := 46;
  FTopPanel.Align := alTop;
  FTopPanel.Parent := FInnerControl;
  FTopPanel.BevelOuter := bvNone;
  FTopPanel.TabOrder := 0;

  FTopLabel := TLabel.Create(nil);
  FTopLabel.Parent := FTopPanel;
  FTopLabel.Align := alTop;
  FTopLabel.AlignWithMargins := True;
  FTopLabel.AutoSize := False;
  FTopLabel.Height := 17;

  FFilterEdit := TEdit.Create(nil);
  with FFilterEdit do
  begin
    Parent := FTopPanel;
    Left := 0;
    Top := FTopLabel.Top + FTopLabel.Height + 4;
    TabOrder := 0;
//    OnKeyDown := OnWinControlKeyDown;
    Width := 60;
    MaxLength := 12;
    AutoSelect := False;
  end;

  FList := TcxTreeList.Create(nil);
  FList.Parent := FInnerControl;
  FList.Align := alClient;
  FList.OnKeyDown := OnKeyDown;
  FList.OnDblClick := OnDblClick;
  FList.CreateColumn;
  FList.OptionsView.Headers := False;
  FList.OptionsView.ShowRoot := False;
  FList.OptionsView.ColumnAutoWidth := True;
  FList.OptionsSelection.HideSelection := False;
  FList.OptionsSelection.HideFocusRect := False;
  FList.OptionsSelection.MultiSelect := False;
  FList.OptionsData.Editing := False;
  FList.OptionsSelection.CellSelect := False;
  FList.Font.Size := 11;

  FBottomLabel := TLabel.Create(nil);
  FBottomLabel.Parent := FInnerControl;
  FBottomLabel.Align := alBottom;
  FBottomLabel.AlignWithMargins := True;
  FBottomLabel.Height := 17;
end;

destructor TSelectEntityPopup.Destroy;
begin
  FreeAndNil(FTopLabel);
  FreeAndNil(FTopPanel);
  FreeAndNil(FList);
  FreeAndNil(FBottomLabel);
  inherited;
end;

procedure TSelectEntityPopup.DoOnBeforeShow;
begin
  inherited;
  Reload;
end;

procedure TSelectEntityPopup.Init(const AItems: TEntityList; const AIsSelectMode: Boolean;
  const ASelectedEntity: TEntity; const AFilter: string);
var
  i: Integer;
  vCaption: string;
  vDomain: TDomain;
begin
  FAllData := AItems;

  if FAllData.Filler is TEntityField then
    FSearchType := TEntityField(FAllData.Filler).SearchType
  else
    FSearchType := stSearchFromBegin;

  vDomain := TDomain(FInteractor.Domain);

  if AItems.Count = 0 then
  begin
    FList.Visible := False;
    FFilterEdit.Visible := False;
    FTopLabel.Caption := vDomain.TranslateDefinition(AItems.MainDefinition) + ' ' +
      FInteractor.Translate('txtNotSelected', 'отсутствуют') + ' ';
    Sizeable := False;
    Color := clInfoBk;
    FBottomLabel.Visible := False;
  end
  else
  begin
    FList.Visible := True;
    FSelectedEntity := ASelectedEntity;

    if AItems.Count > 10 then
    begin
      FFilterText := AnsiUpperCase(Trim(AFilter));
      if Length(FFilterText) > 0 then
      begin
        FFilterEdit.OnChange := nil;
        FFilterEdit.Text := FFilterText;
        FFilterEdit.SelStart := 1000;
      end;
      FFilterEdit.Visible := True;
      FFilterEdit.OnChange := OnFilterChange;

      vCaption := '';
      for i := 0 to AItems.ContentDefinitions.Count - 1 do
      begin
        if Length(vCaption) > 0 then
          vCaption := vCaption + ', ';
        vCaption := vCaption + vDomain.TranslateDefinition(AItems.ContentDefinitions[i]);
      end;
      FTopPanel.Visible := True;
      FTopLabel.Caption := FInteractor.Translate('cptSelection', 'Выбор') + ': ' + vCaption;
      FBottomLabel.Visible := True;
    end
    else
    begin
      FTopPanel.Visible := False;
      FBottomLabel.Visible := False;
    end;
  end;
end;

procedure TSelectEntityPopup.OnFilterChange(Sender: TObject);
begin
  FFilterText := AnsiUpperCase(Trim(FFilterEdit.Text));
  Reload;
end;

procedure TSelectEntityPopup.OnDblClick(Sender: TObject);
begin
  if Assigned(FOnEntitySelected) and Assigned(FList.FocusedNode) then
    FOnEntitySelected(TEntity(FList.FocusedNode.Data));
  Close;
end;

procedure TSelectEntityPopup.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    OnDblClick(Self);
end;

function TSelectEntityPopup.IsMatchToFilter(const AEntity: TEntity): Boolean;
var
  vText: string;
  vST: TSearchType;
begin
  Result := Assigned(AEntity);
  if not Result then
    Exit(True);

  if AEntity.Deleted then
    Exit(False);

  if Length(FFilterText) > 0 then
  begin
    vText := AnsiUpperCase(AEntity.DisplayName);
    vST := FSearchType;
    Result :=
      ((vST = stSearchFromBegin) and (Pos(FFilterText, vText) = 1)) or
      ((vST = stSearchEverywhere) and (Pos(FFilterText, vText) > 0)) or
      ((vST = stSearchMultiEntrance) and IsTextMultiEqual(FFilterText, vText));
  end;
end;

procedure TSelectEntityPopup.Reload;
var
  i: Integer;
  vEntity: TEntity;
  vNode: TcxTreeListNode;
begin
  try
    FList.BeginUpdate;
    FList.Clear;

    FAllData.Resort;
    for i := 0 to FAllData.Count - 1 do
    begin
      vEntity := FAllData[i];

      if IsMatchToFilter(vEntity) then
      begin
        vNode := FList.Add;
        vNode.Data := vEntity;
        if not Assigned(vEntity) then
          vNode.Values[0] := FAllData.MainDefinition._EmptyValue
        else
          vNode.Values[0] := vEntity['Name'];
        if vEntity = FSelectedEntity then
          vNode.Focused := True;
      end;
    end;

  finally
    FList.EndUpdate;
    UpdateCount;
    UpdateSizes;
  end;
end;

procedure TSelectEntityPopup.UpdateCount;
begin
  FBottomLabel.Caption := 'Количество записей: ' + IntToStr(FList.Count);
end;

type
  TcxTreeListAccess = class (TcxTreeList);
  TcxTreeListNodeAccess = class (TcxTreeListNode);

procedure TSelectEntityPopup.UpdateSizes;
var
  vHeight, vRowCount, vRowHeight: Integer;
begin
  vHeight := 0;

  if FTopPanel.Visible then
  begin
    if FFilterEdit.Visible then
    begin
      if FTopPanel.AlignWithMargins then
        vHeight := vHeight + FTopPanel.Margins.ExplicitHeight
      else
        vHeight := vHeight + FTopPanel.Height;
    end
    else
    begin
      if FTopLabel.AlignWithMargins then
        vHeight := vHeight + FTopLabel.Margins.ExplicitHeight
      else
        vHeight := vHeight + FTopLabel.Height;
    end;
  end;

  if FBottomLabel.Visible then
  begin
    if FBottomLabel.AlignWithMargins then
      vHeight := vHeight + FBottomLabel.Margins.ExplicitHeight
    else
      vHeight := vHeight + FBottomLabel.Height;  
  end;
  
  if FList.Visible then
  begin
    TcxTreeListAccess(FList).ViewInfo.Calculate;
    vRowCount := FList.Count;
    vRowHeight := 0;
    if (vRowCount > 0) then
    begin
      if Assigned(TcxTreeListNodeAccess(FList.Items[0]).ViewData) then
        vRowHeight := TcxTreeListNodeAccess(FList.Items[0]).ViewData.Height
      else
        vRowHeight := 22;// default row height
    end;

    if vRowCount > 10 then vRowCount := 10;
    vHeight := vHeight + vRowCount * vRowHeight;
  end;

  if vHeight > 0 then
  begin
    FInnerControl.Width := InvokedBy.Width;
    FInnerControl.ClientHeight := vHeight + 2;
    if FList.Visible and Assigned(FList.FocusedNode) then
      FList.FocusedNode.MakeVisible;
  end
  else
  begin
    FInnerControl.ClientHeight := 0;
    FInnerControl.Width := 0;
  end;
end;

end.
