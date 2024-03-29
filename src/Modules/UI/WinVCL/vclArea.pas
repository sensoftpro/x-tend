﻿{---------------------------------------------------------------------------------
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

unit vclArea;

interface

uses
  Windows, Classes, Forms, Messages, Generics.Collections, Controls, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, Menus, UITypes, SysUtils,
  uConsts, uUIBuilder, uDefinition, uDomain, uEntity, uView, uLayout;

//////  1. Привязка сплиттера к контролу
//////  2. Привязка меню к контролу
//////  3. Обработка TabOrder
//////  4. Сохранение и восстановление размеров форм в презентере
//////  5. Name и Description
//////  6. Перенести AssignFromLayout в общий код создания
//////  7. Сделать TCollectionArea, перенести код в DoCreateControl
//////  8. Разбраться с надписью для полей
//////  9. Внутри у TNativeControl может не быть нативного контрола, а, например, html-текст
///  10. Работа с FParams
//////  11. Перенести общее поведение и обработчики в TPresenter
//////  12. Рефакторинг связки TUIArea + TNativeControl, распределение ответственности
///  13. Рефакторинг TLayout, добавление нужных полей и удаление лишних
///  14. Разделение TUIBuilder и отстроенных объектов
///  15. Рефакторинг работы с заголовками полей

type
  TVCLControl = class(TNativeControlHolder)
  private
    procedure BeforeContextMenuShow(Sender: TObject);
  protected
    function IndexOfSender(const ASender: TObject): Integer; override;

    procedure DoActivate(const AUrlParams: string); override;
    procedure DoClose(const AModalResult: Integer); override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;

    procedure AssignFromLayout(const ALayout: TLayout; const AParams: string); override;

    procedure SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl); override;
    procedure SetControl(const AControl: TObject); override;
    procedure SetParent(const AParent: TUIArea); override;
    function GetFocused: Boolean; override;
    procedure SetFocused(const Value: Boolean); override;
    function GetBounds: TRect; override;
    procedure SetBounds(const Value: TRect); override;
    function GetClientRect: TRect; override;
    function GetViewState: TViewState; override;
    procedure SetViewState(const AViewState: TViewState); override;
    function GetTabOrder: Integer; override;
    procedure SetTabOrder(const ATabOrder: Integer); override;
    function GetActiveChildArea: TUIArea; override;
    procedure SetActiveChildArea(const AArea: TUIArea); override;
    function GetModalResult: TModalResult; override;
    procedure SetModalResult(const AModalResult: TModalResult); override;
    function GetWindowState: TWindowState; override;
    procedure SetWindowState(const AWindowState: TWindowState); override;
    procedure SetAlignment(const AAlignment: TAlignment); override;

    function DoCreateCaption(const AParent: TUIArea; const ACaption, AHint: string): TObject; override;
    procedure PlaceLabel; override;
    procedure SetCaptionProperty(const ALayout: TLayout); virtual;
    procedure UpdateCaptionVisibility; override;
  public
    constructor Create(const AOwner: TUIArea; const AParams: string = ''); override;
    destructor Destroy; override;
  end;

  TVCLButton = class(TVCLControl)
  private
    FTypeSelectionMenu: TPopupMenu;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TVCLLink = class(TVCLControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TVCLMainMenuNavigation = class(TVCLControl)
  private
    FMenu: TMainMenu;
    FMenuItem: TMenuItem;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TVCLToolBarNavigation = class(TVCLControl)
  private
    FToolBar: TToolBar;
  protected
    procedure DoAfterSetParent(const AParent: TUIArea); override;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TVCLTreeViewNavigation = class(TVCLControl)
  private
    FTreeView: TTreeView;
    FDefaultWorkArea: string;
    procedure SelectNode(const ANode: TTreeNode);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
    procedure OnMouseClick(Sender: TObject);
    procedure Refill;
    //procedure OnCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    //procedure DrawButton(ARect: TRect; Node: TTreeNode);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
    procedure DoExecuteUIAction(const AView: TView); override;
  end;

  TVCLOneButtonNavigation = class(TVCLControl)
  private
    FButton: TButton;
    FMenu: TPopupMenu;
    FItem: TMenuItem;
    procedure OnClick(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TFloatFm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

{$R PopupForm.dfm}
{$R FloatForm.dfm}

procedure LockControl(const AWinControl: TWinControl; const ALock: Boolean);

implementation

uses
  Types, Graphics, Math, StrUtils, Generics.Defaults, Variants,

  uPresenter, uWinVCLPresenter, uConfiguration, uSession, uInteractor, uUtils, uCollection,
  vclSimpleEditors, uEntityList, uDomainUtils, uChangeManager;

type
  TCrackedWinControl = class(TWinControl) end;
  TCrackedControl = class(TControl) end;

procedure LockControl(const AWinControl: TWinControl; const ALock: Boolean);
begin
  if (AWinControl = nil) or (AWinControl.Handle = 0) then Exit;
  if ALock then
    SendMessage(AWinControl.Handle, WM_SETREDRAW, 0, 0)
  else
  begin
    SendMessage(AWinControl.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(AWinControl.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

{ TFloatFm }

procedure TFloatFm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

{ TVCLOneButtonNavigation }

function TVCLOneButtonNavigation.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vImageSize: Integer;
  vActionName: string;
  vAction: TActionDef;
begin
  FButton := TButton.Create(nil);
  FButton.Style := bsSplitButton;
  FButton.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
  CopyFontSettings(FButton.Font, ALayout);

  vImageSize := StrToIntDef(GetUrlParam(FParams, 'ImageSize'), 16);
  FButton.Images := TDragImageList(FUIBuilder.Images[vImageSize]);

  vActionName := GetUrlParam(FParams, 'Action');
  if Length(vActionName) > 0 then
  begin
    FView := FView.BuildView(vActionName);
    Assert(FView.DefinitionKind = dkAction);

    vAction := TActionDef(FView.Definition);
    FButton.ImageIndex := AParent.GetImageIndex(vAction._ImageID);
    FButton.Caption := AParent.GetTranslation(vAction, tpCaption);
    FButton.Hint := AParent.GetTranslation(vAction, tpHint);
    FButton.OnClick := AParent.OnAreaClick;
  end
  else
  begin
    FButton.ImageIndex := AParent.GetImageIndex(GetUrlParam(FParams, 'ImageIndex'));
    FButton.Caption := GetUrlParam(FParams, 'Caption');
    FButton.Hint := GetUrlParam(FParams, 'Hint');
    FButton.OnClick := OnClick;
  end;

  FMenu := TPopupMenu.Create(nil);
  FMenu.Images := TDragImageList(FUIBuilder.Images[16]);
  FButton.PopupMenu := FMenu;
  FButton.DropDownMenu := FMenu;

  Result := FButton;
end;

function TVCLOneButtonNavigation.DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vControl: TObject;
begin
  FItem := TMenuItem.Create(nil);
  FItem.Caption := ACaption;
  FItem.Hint := AHint;
  FItem.ImageIndex := AImageIndex;
  FItem.OnClick := FOwner.OnAreaClick;
  vControl := GetRealControl(AParent);
  if (ANavItem.Level > 0) and (vControl is TMenuItem) then
    TMenuItem(vControl).Add(FItem)
  else
    FMenu.Items.Add(FItem);
  Result := FItem;
end;

procedure TVCLOneButtonNavigation.OnClick(Sender: TObject);
var
  vPoint: TPoint;
begin
  vPoint := FButton.ClientToScreen(Point(0, FButton.Height));
  FMenu.Popup(vPoint.X, vPoint.Y);
end;

{ TVCLMainMenuNavigation }

function TVCLMainMenuNavigation.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FMenu := TMainMenu.Create(TComponent(GetRealControl(AParent)));
  FMenu.Images := TDragImageList(FUIBuilder.Images[16]);
  Result := FMenu;
end;

function TVCLMainMenuNavigation.DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vForm: TForm;
  vControl: TObject;
begin
  FMenuItem := TMenuItem.Create(nil);
  FMenuItem.Caption := ACaption;
  FMenuItem.Hint := AHint;
  FMenuItem.ImageIndex := AImageIndex;
  FMenuItem.OnClick := FOwner.OnAreaClick;

  Result := nil;
  if ANavItem.Level = 0 then
  begin
    FMenu.Items.Add(FMenuItem);
    Result := FMenuItem;
  end
  else begin
    vControl := GetRealControl(AParent);
    if vControl is TMenuItem then
    begin
      TMenuItem(vControl).Add(FMenuItem);
      Result := FMenuItem;
    end;
  end;

  if ANavItem.Id = 'Windows' then
  begin
    //FMenuItem.Visible := TInteractor(FView.Interactor).Layout = 'mdi';
    if FUIBuilder.IsMDIStyle and (Parent.NativeControl.IsForm) then
    begin
      vForm := TForm(GetRealControl(Parent));
      if vForm.FormStyle = fsMDIForm then
        vForm.WindowMenu := FMenuItem;
    end;
  end;
end;

{ TVCLToolBarNavigation }

function TVCLToolBarNavigation.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FToolBar := TToolBar.Create(TComponent(GetRealControl(AParent)));
  FToolBar.ShowCaptions := True;
  Result := FToolBar;
end;

function TVCLToolBarNavigation.DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vMenu: TPopupMenu;
  vMenuItem: TMenuItem;
  vLeft: Integer;
  vToolButton: TToolButton;
  vControl: TObject;
begin
  if ANavItem.Level = 0 then
  begin
    if FToolBar.ButtonCount > 0 then
    begin
      vToolButton := FToolBar.Buttons[FToolBar.ButtonCount - 1];
      vLeft := vToolButton.Left + vToolButton.Width + 10;
    end
    else
      vLeft := 0;

    vToolButton := TToolButton.Create(FToolBar);
    vToolButton.AutoSize := True;
    vToolButton.Left := vLeft;
    vToolButton.Caption := ACaption;
    vToolButton.Hint := AHint;
    vToolButton.ImageIndex := AImageIndex;
    vToolButton.OnClick := FOwner.OnAreaClick;

    Result := vToolButton;
  end
  else begin
    vControl := GetRealControl(AParent);

    vMenuItem := TMenuItem.Create(FToolBar);
    vMenuItem.Caption := ACaption;
    vMenuItem.Hint := AHint;
    vMenuItem.ImageIndex := AImageIndex;
    vMenuItem.OnClick := FOwner.OnAreaClick;

    if vControl is TToolButton then
    begin
      TToolButton(vControl).Style := tbsDropDown;
      if Assigned(TToolButton(vControl).DropdownMenu) then
        vMenu := TToolButton(vControl).DropdownMenu
      else
      begin
        vMenu := TPopupMenu.Create(FToolBar);
        TToolButton(vControl).DropdownMenu := vMenu;
      end;

      vMenu.Items.Add(vMenuItem);
    end
    else if vControl is TMenuItem then
      TMenuItem(vControl).Add(vMenuItem);

    Result := vMenuItem;
  end;
end;

procedure TVCLToolBarNavigation.DoAfterSetParent(const AParent: TUIArea);
var
  vToolButton: TToolButton;
  i, vImageSize: Integer;
begin
  vImageSize := StrToIntDef(GetUrlParam(FParams, 'ImageSize'), 16);
  FToolBar.Images := TDragImageList(FUIBuilder.Images[vImageSize]);
  FToolBar.AutoSize := True;
  for i := 0 to FToolBar.ButtonCount - 1 do
  begin
    vToolButton := FToolBar.Buttons[i];
    if Assigned(vToolButton.DropdownMenu) then
      vToolButton.DropdownMenu.Images := FToolBar.Images;
  end
end;

{ TVCLTreeViewNavigation }

function TVCLTreeViewNavigation.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FTreeView := TTreeView.Create(nil);
  //FTreeView.Images := TDragImageList(TInteractor(FView.Interactor).Images[16]);
  FTreeView.ReadOnly := True;
  FTreeView.DoubleBuffered := True;
  FTreeView.HideSelection := False;
  FTreeView.OnClick := OnMouseClick;
  FTreeView.OnKeyPress := OnKeyPress;
  //FTreeView.OnCustomDrawItem := OnCustomDrawItem;
  ALayout.Id := 'TreeView';

  FDefaultWorkArea := FCreateParams.Values['ContentWorkArea'];
  if FDefaultWorkArea = '' then
    FDefaultWorkArea := 'WorkArea';
  Result := FTreeView;
end;

function TVCLTreeViewNavigation.DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vParentNode: TTreeNode;
  vTreeNode: TTreeNode;
begin
  if ANavItem.IsLine then
    Exit(nil);

  if ANavItem.Level > 0 then
    vParentNode := TTreeNode(GetRealControl(AParent))
  else
    vParentNode := nil;

  vTreeNode := FTreeView.Items.AddChild(vParentNode, ACaption);
  vTreeNode.ImageIndex := AImageIndex;
  vTreeNode.SelectedIndex := -1;

  Result := vTreeNode;

  if Assigned(vParentNode) then
    vParentNode.Expand(True);
end;

procedure TVCLTreeViewNavigation.DoExecuteUIAction(const AView: TView);
begin
  if AView.Name = '#Refill' then
    Refill;
end;

{procedure TTreeViewArea.OnCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  NodeRect: TRect;
  vCanvas: TCanvas;
begin
  Exit;
  DefaultDraw := False;

  vCanvas := Sender.Canvas;

  NodeRect := Node.DisplayRect(True);
  vCanvas.FillRect(NodeRect);

  if cdsSelected in State then
  begin
    vCanvas.Font.Color := clBlack;
    vCanvas.Brush.Color := clWhite;
  end
  else
    vCanvas.Font.Color := clSilver;

  vCanvas.TextOut(NodeRect.Left, NodeRect.Top, Node.Text);
  NodeRect := Node.DisplayRect(False);

  NodeRect.Left := NodeRect.Left + (Node.Level * FTreeView.Indent);
  DrawButton(NodeRect, Node);
end;

procedure TTreeViewArea.DrawButton(ARect: TRect; Node: TTreeNode);
var
  cx, cy: Integer;
  vButtonSize: Integer;
begin
  vButtonSize := 5;
  cx := ARect.Left + FTreeView.Indent div 2;
  cy := ARect.Top + (ARect.Bottom - ARect.Top) div 2;
  with FTreeView.Canvas do
  begin
    Pen.Color := clGray;
    //draw horizontal line.
    if Node.HasChildren then
    begin
      PenPos := Point(cx + vButtonSize, cy);
      LineTo(ARect.Left + FTreeView.Indent - 1, cy);
    end
    else
    begin
      PenPos := Point(cx, cy);
      LineTo(ARect.Left + FTreeView.Indent - 1, cy);
    end;

    //draw half vertical line, top portion.
    PenPos := Point(cx, cy);
    LineTo(cx, ARect.Top-1);

   if ((Node.GetNextVisible <> nil) and (Node.GetNextVisible.Level = Node.Level)) or (Node.GetNextSibling <> nil) then
   //draw bottom portion of half vertical line.
   begin
     PenPos := Point(cx, cy);
     LineTo(cx, ARect.Bottom + 1);
   end;

   if Node.HasChildren then
   begin
     //Let"s try a circular button instead
      Rectangle(cx - vButtonSize, cy - vButtonSize, cx + vButtonSize, cy + vButtonSize);
    // Rectangle();
     //draw the horizontal indicator.
     PenPos := Point(cx - vButtonSize + 2, cy);
     LineTo(cx + vButtonSize - 2, cy);
     //draw the vertical indicator if the node is collapsed
     if not Node.Expanded then
     begin
       PenPos := Point(cx, cy - vButtonSize + 2);
       LineTo(cx, cy + vButtonSize - 2);
     end;
   end;
       //now connect vertical lines of higher level nodes.
    Node := Node.Parent;
    while Node <> nil do
    begin
     cx := cx - FTreeView.Indent;
     if Node.GetNextSibling <> nil then
     begin
       PenPos := Point(cx, ARect.Top);
       LineTo(cx, ARect.Bottom);
     end;
     Node := Node.Parent;
    end;
  end;
end;}

procedure TVCLTreeViewNavigation.OnKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and Assigned(FTreeView.Selected) then
    SelectNode(FTreeView.Selected);
end;

procedure TVCLTreeViewNavigation.OnMouseClick(Sender: TObject);
var
  vClickPos: TPoint;
  vNode: TTreeNode;
begin
  vClickPos :=  FTreeView.ScreenToClient(Mouse.CursorPos);
  vNode := FTreeView.GetNodeAt(vClickPos.X, vClickPos.Y);
  if Assigned(vNode) then
    SelectNode(vNode);
end;

procedure TVCLTreeViewNavigation.Refill;
begin
  FOwner.Clear;
  FTreeView.Items.Clear;
  FOwner.ProcessChilds;
end;

procedure TVCLTreeViewNavigation.SelectNode(const ANode: TTreeNode);
var
  vArea: TUIArea;
begin
  if not Assigned(ANode) then
    Exit;

  vArea := TUIArea(ANode.Data);
  if Assigned(vArea) then
    FOwner.ProcessAreaClick(vArea);
end;

{ TVCLButton }

procedure TVCLButton.DoBeforeFreeControl;
begin
  FreeAndNil(FTypeSelectionMenu);
end;

function TVCLButton.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vParams: TStrings;
  vButton: TButton;
  vActionDef: TDefinition;
  vDefinitions: TList<TDefinition>;
  i: Integer;
  vMenuItem: TMenuItem;
  vDefinition: TDefinition;
  vImageID: string;
  vImageIndex: Integer;
  vCaption: string;
  vImageSize: Integer;
  vComposition: string;
  vViewStyle: string;
  vOverriddenCaption, vOverriddenHint: string;
begin
  vActionDef := TDefinition(FView.Definition);

  vParams := CreateDelimitedList(FInternalParams, '&');
  try
    vImageSize := StrToIntDef(vParams.Values['ImageSize'], 16);
    vImageID := vParams.Values['ImageID'];
    if vImageID = '' then
      vImageID := vActionDef._ImageID;
    vComposition := Trim(vParams.Values['Composition']);
    vViewStyle := Trim(vParams.Values['ViewStyle']);
    vOverriddenCaption := Trim(vParams.Values['Caption']);
    vOverriddenHint := Trim(vParams.Values['Hint']);
  finally
    FreeAndNil(vParams);
  end;

  vButton := TButton.Create(nil);
  vButton.Images := TDragImageList(FUIBuilder.Images[vImageSize]);
  vImageIndex := GetImageIndex(vImageID);

  //TODO: We need another control or style here
  if (ALayout.BevelOuter = lbkNone) and (ALayout.BevelInner = lbkNone) then
  begin
    //vButton.SpeedButtonOptions.Flat := True;
    //vButton.SpeedButtonOptions.CanBeFocused := False;
  end;

  vCaption := GetTranslation(vActionDef);
  vButton.Hint := vCaption;
  if Length(vOverriddenCaption) > 0 then
    vCaption := vOverriddenCaption;
  if Length(vOverriddenHint) > 0 then
    vButton.Hint := vOverriddenHint;

  vButton.ImageIndex := -1;
  vButton.Caption := '';
  if (vButton.Images.Count + 1 >= vImageIndex) and (vImageIndex > 0) then
  begin
    if vComposition = '' then
    begin
      if ALayout.Button_ShowCaption then
        vButton.Caption := vCaption
      else begin
        vButton.ImageAlignment := TImageAlignment.iaCenter;
        vButton.ImageIndex := vImageIndex;
      end;
    end
    else if vComposition = 'TextOnly' then
      vButton.Caption := vCaption
    else if vComposition = 'ImageOnly' then
    begin
      vButton.ImageAlignment := TImageAlignment.iaCenter;
      vButton.ImageIndex := vImageIndex;
    end
    else begin
      vButton.Caption := vCaption;
      vButton.ImageIndex := vImageIndex;
      if vComposition = 'ImageRight' then
        vButton.ImageAlignment := TImageAlignment.iaRight
      else if vComposition = 'ImageTop' then
        vButton.ImageAlignment := TImageAlignment.iaTop
      else if vComposition = 'ImageBottom' then
        vButton.ImageAlignment := TImageAlignment.iaBottom
      else
        vButton.ImageAlignment := TImageAlignment.iaLeft;
    end;
  end
  else begin
    vButton.Caption := vCaption;
    vButton.WordWrap := True;
  end;

  if (vActionDef.Name = 'Add') and Assigned(FView.ParentDomainObject) and (FView.ParentDomainObject is TEntityList) then
  begin
    vDefinitions := TEntityList(FView.ParentDomainObject).ContentDefinitions;
    if vDefinitions.Count > 1 then
    begin
      FTypeSelectionMenu := TPopupMenu.Create(nil);
      FTypeSelectionMenu.Images := TDragImageList(FUIBuilder.Images[16]);
      for i := 0 to vDefinitions.Count - 1 do
      begin
        vDefinition := TDefinition(vDefinitions[i]);
        vMenuItem := TMenuItem.Create(nil);
        vMenuItem.Caption := GetTranslation(vDefinition);
        if Length(vOverriddenCaption) > 0 then
          vMenuItem.Caption := vOverriddenCaption;
        vMenuItem.ImageIndex := GetImageIndex(vDefinition._ImageID);
        vMenuItem.Tag := NativeInt(FOwner);
        vMenuItem.OnClick := FOwner.OnActionMenuSelected;
        FTypeSelectionMenu.Items.Add(vMenuItem);
      end;
      vButton.DropDownMenu := FTypeSelectionMenu;
      vButton.Style := bsSplitButton;
      vButton.ImageMargins.Left := 4;
      vButton.ImageMargins.Top := 4;
      vButton.ImageMargins.Right := 4;
      vButton.ImageMargins.Bottom := 4;
      ALayout.Width := 42;
    end
    else begin
      vButton.OnClick := FOwner.OnAreaClick;
      ALayout.Width := ALayout.Height;
    end;
  end
  else
    vButton.OnClick := FOwner.OnAreaClick;

  Result := vButton;
end;

procedure TVCLButton.RefillArea(const AKind: Word);
var
  vButton: TButton;
  vActionDef: TDefinition;
  vImageIndex: Integer;
begin
  if AKind <> dckContentTypeChanged then
  begin
    inherited RefillArea(AKind);
    Exit;
  end;

  vButton := TButton(FControl);

  vActionDef := TDefinition(FView.Definition);
  vImageIndex := GetImageIndex(vActionDef._ImageID);

  if (vButton.Images.Count + 1 >= vImageIndex) and (vImageIndex > 0) then
    vButton.ImageIndex := vImageIndex;

  vButton.Caption := GetTranslation(vActionDef);
  vButton.Hint := vButton.Caption;
end;

{ TVCLLink }

function TVCLLink.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vParams: TStrings;
  vLabel: TLabel;
  vActionDef: TDefinition;
  vComposition: string;
  vViewStyle: string;
  vOverriddenCaption, vOverriddenHint: string;
begin
  vParams := CreateDelimitedList(FInternalParams, '&');
  try
    vComposition := Trim(vParams.Values['Composition']);
    vViewStyle := Trim(vParams.Values['ViewStyle']);
    vOverriddenCaption := Trim(vParams.Values['Caption']);
    vOverriddenHint := Trim(vParams.Values['Hint']);
  finally
    FreeAndNil(vParams);
  end;

  vActionDef := TDefinition(FView.Definition);

  vLabel := TLabel.Create(nil);
  vLabel.Caption := FOwner.GetTranslation(vActionDef);
  vLabel.Hint := vLabel.Caption;
  if Length(vOverriddenCaption) > 0 then
    vLabel.Caption := vOverriddenCaption;
  if Length(vOverriddenHint) > 0 then
    vLabel.Hint := vOverriddenHint;
  vLabel.Cursor := crHandPoint;
  vLabel.Transparent := True;
  vLabel.Alignment := TAlignment.taLeftJustify;
  vLabel.Font.Color := AlphaColorToColor(ALayout.Font.Color);
  vLabel.Font.Style := [fsUnderline];
  vLabel.OnClick := FOwner.OnAreaClick;

  Result := vLabel;
end;

procedure TVCLLink.RefillArea(const AKind: Word);
var
  vLabel: TLabel;
  vActionDef: TDefinition;
begin
  if AKind <> dckContentTypeChanged then
  begin
    inherited RefillArea(AKind);
    Exit;
  end;

  vLabel := TLabel(FControl);

  vActionDef := TDefinition(FView.Definition);
  vLabel.Caption := GetTranslation(vActionDef);
  vLabel.Hint := vLabel.Caption;
end;

{ TVCLControl }

procedure TVCLControl.AssignFromLayout(const ALayout: TLayout; const AParams: string);
var
  vForm: TForm;
  vWS: Integer;
begin
  if FIsForm then
  begin
    vForm := TForm(FControl);

    if ALayout.Kind = lkFrame then
    begin
      if (ALayout.Tag and cFormResizable) > 0 then
      begin
        vForm.BorderStyle := bsSizeable;
        vForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      end;

      if (ALayout.Tag and cFormDisableMinimizeButton) > 0 then
        vForm.BorderIcons := vForm.BorderIcons - [biMinimize];

      if (ALayout.Tag and cFormDisableMaximizeButton) > 0 then
        vForm.BorderIcons := vForm.BorderIcons - [biMaximize];

      //if (ALayout.Tag and cFormPositionDesign) > 0 then
      //  vForm.Position := poDesigned;

      if (ALayout.Tag and cFormNotResizable) > 0 then
        vForm.BorderStyle := bsSingle;

      vWS := StrToIntDef(GetUrlParam(AParams, 'WindowState', ''), -1);
      if vWS > -1 then
        vForm.WindowState := TWindowState(vWS);

      if (vForm.WindowState = wsNormal) then
      begin
        if (vForm.FormStyle <> fsMDIChild) or ((ALayout.Tag and cFormUseDesignSizes) > 0) then
        begin
          vForm.ClientWidth := ALayout.Width;
          if FOwner.View.DefinitionKind = dkDomain then
            vForm.ClientHeight := ALayout.Height
          else
            vForm.ClientHeight := ALayout.Height + cServiceAreaHeight;
        end;
      end;

      CopyConstraints(vForm, ALayout);

      if ALayout.Caption <> '' then
        vForm.Caption := ALayout.Caption;
      vForm.Color := AlphaColorToColor(ALayout.Color);
    end
    else if ALayout.Kind = lkPanel then
    begin
      vForm.BorderStyle := bsSizeable;
      vForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      vForm.Caption := ALayout.Caption;
      vForm.ClientWidth := ALayout.Width;
      if FOwner.View.DefinitionKind = dkDomain then
        vForm.ClientHeight := ALayout.Height
      else
        vForm.ClientHeight := ALayout.Height + cServiceAreaHeight;
    end
    else if ALayout.Kind = lkPage then
    begin
      vForm.BorderStyle := bsSizeable;
      vForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      vForm.Caption := ALayout.Caption;
    end
    else
      Assert(False, 'Непонятно какой контрол в лэйауте');
  end
  else if ALayout.Kind = lkFrame then
  begin
    //vFrame.SetBounds(Control.Left, Control.Top, Control.Width, Control.Height);
    if (ALayout.Caption <> '') and (FControl is TTabSheet) then
    begin
      TTabSheet(FControl).Caption := ALayout.Caption;
      if Pos('=', ALayout.Caption) > 0 then // Hint содержит url-строку с параметрами
      begin
        TTabSheet(FControl).Caption := GetUrlParam(ALayout.Caption, 'Caption', '');
        TTabSheet(FControl).ImageIndex := FOwner.GetImageIndex(GetUrlParam(ALayout.Caption, 'ImageIndex', ''));
      end;
    end;
  end
  else if (ALayout.Kind in [lkPanel, lkMemo, lkShape]) and (FControl is TControl) then
  begin
    SetBounds(Rect(ALayout.Left, ALayout.Top,
      ALayout.Left + ALayout.Width, ALayout.Top + ALayout.Height));
    SetCaptionProperty(ALayout);

    CopyFontSettings(TCrackedWinControl(FControl).Font, ALayout);

    TControl(FControl).Anchors := ALayout.Anchors;
    TControl(FControl).Align := TAlign(ALayout.Align);
    CopyMargins(TControl(FControl), ALayout);
    if FControl is TWinControl then
      CopyPadding(TWinControl(FControl), ALayout);

    SetAlignment(ALayout.Alignment);

    TCrackedControl(FControl).Color := AlphaColorToColor(ALayout.Color);
    TCrackedControl(FControl).ParentColor := False;
    if FControl is TWinControl then
      TCrackedWinControl(FControl).ParentBackground := False;
  end;
end;

procedure TVCLControl.BeforeContextMenuShow(Sender: TObject);
var
  vMenu: TPopupMenu;
  vMenuItem: TMenuItem;

  procedure CheckMenuItems(const AMenuItem: TMenuItem);
  var
    vChildItem: TMenuItem;
    vArea: TUIArea;
  begin
    if AMenuItem.Count > 0 then
    begin
      for vChildItem in AMenuItem do
        CheckMenuItems(vChildItem);
      Exit;
    end;

    vArea := AreaFromSender(AMenuItem);
    if not Assigned(vArea) then
      Exit;
    if not Assigned(vArea.View) then
      Exit;
    if vArea.View.DefinitionKind <> dkAction then
      Exit;
    if not Assigned(vArea.View.DomainObject) then
      Exit;

    if TEntity(vArea.View.DomainObject).FieldExists('IsChecked') then
      AMenuItem.Checked := TEntity(vArea.View.DomainObject)['IsChecked'];
  end;

begin
  vMenu := TPopupMenu(Sender);
  for vMenuItem in vMenu.Items do
    CheckMenuItems(vMenuItem);
end;

constructor TVCLControl.Create(const AOwner: TUIArea; const AParams: string);
begin
  inherited Create(AOwner, AParams);
end;

destructor TVCLControl.Destroy;
var
  vControl: TObject;
  vIsFloat: Boolean;
begin
  if Assigned(FCaption) then
    TLabel(FCaption).Parent := nil;

  vControl := FControl;
  vIsFloat := (FOwner.Id = 'float') or (FOwner.Id = 'free');

  inherited Destroy;

  if not Assigned(vControl) then
    Exit;
  if not (vControl is TComponent) then
    Exit;

  if vControl is TForm then
  begin
    if (TForm(vControl).FormStyle = fsMDIChild) or vIsFloat then
      FreeAndNil(vControl);
  end
  else
    FreeAndNil(vControl);
end;

function ClientWindowProc(Wnd: HWND; Msg: Cardinal; wparam, lparam: Integer ): Integer; stdcall;
var
  f: Pointer;
begin
  f := Pointer(GetWindowLong(Wnd, GWL_USERDATA));
  case msg of
    WM_NCCALCSIZE:
      if (GetWindowLong(Wnd, GWL_STYLE ) and (WS_HSCROLL or WS_VSCROLL)) <> 0 then
        SetWindowLong(Wnd, GWL_STYLE, GetWindowLong(Wnd, GWL_STYLE) and not (WS_HSCROLL or WS_VSCROLL));
  end;
  Result := CallWindowProc(f, Wnd, Msg, wparam, lparam);
end;

procedure TVCLControl.DoActivate(const AUrlParams: string);
var
  vForm: TForm;
  vChangeTab: Boolean;
begin
  if FControl is TForm then
  begin
    vForm := TForm(FControl);
    if (vForm.FormStyle = fsMDIForm) and (vForm.ClientHandle > 0) and
     (GetWindowLong(vForm.ClientHandle, GWL_USERDATA ) = 0 {cannot subclass client window, userdata already in use}) then
    SetWindowLong(vForm.ClientHandle, GWL_USERDATA, SetWindowLong(vForm.ClientHandle, GWL_WNDPROC, NativeInt(@ClientWindowProc)));

    if vForm.FormStyle = fsMDIChild then
    begin
      if SameText(GetUrlParam(AUrlParams, 'State'), 'Max') then
        vForm.WindowState := wsMaximized
      else
        vForm.BringToFront;
    end;
  end
  else if FControl is TTabSheet then
  begin
    vChangeTab := SameText(GetUrlParam(AUrlParams, 'TabActivationOption', ''), 'ChangeTab');
    if Assigned(Parent.CreateParams) and (Parent.CreateParams.IndexOfName('TabActivationOption') >= 0) and
      (not SameText(Parent.CreateParams.Values['TabActivationOption'], 'ChangeTab')) then
      vChangeTab := False;
    if vChangeTab then
      TTabSheet(FControl).PageControl.ActivePage := TTabSheet(FControl);
  end;
end;

procedure TVCLControl.DoBeginUpdate;
begin
  if (not FIsForm) and (FControl is TWinControl) then
    LockControl(TWinControl(FControl), True);

  if FIsForm and FUIBuilder.IsMDIStyle and Assigned(Application.MainForm) then
    SendMessage(Application.MainForm.ClientHandle, WM_SETREDRAW, 0, 0);
end;

procedure TVCLControl.DoClose(const AModalResult: Integer);
var
  vForm: TForm;
begin
  if FIsForm then
  begin
    vForm := TForm(FControl);

    if AModalResult = mrNone then
      PostMessage(vForm.Handle, WM_CLOSE, 0, 0)
    else begin
      vForm.Close;
      vForm.ModalResult := AModalResult;
    end;
  end;
end;

function TVCLControl.DoCreateCaption(const AParent: TUIArea; const ACaption, AHint: string): TObject;
var
  vLabel: TLabel;
  vFontSize: Integer;
begin
  vLabel := TLabel.Create(nil);
  Result := vLabel;

  vLabel.Parent := TWinControl(GetRealControl(FParent));
  vLabel.Transparent := True;
  vLabel.Caption := ACaption;
  vLabel.Hint := AHint;

  if Assigned(FOwner.View.Parent) and (FOwner.View.Parent.DefinitionKind = dkAction)
    and TDefinition(FOwner.View.Parent.Definition).HasFlag(ccInstantExecution) then
  begin
    vLabel.ParentFont := True;
    vLabel.Font.Size := 9;
  end
  else begin
    vFontSize := vLabel.Font.Size;
    vFontSize := vFontSize - 3;
    if vFontSize < 8 then
      vFontSize := 8;
    vLabel.Font.Size := vFontSize;
    vLabel.Font.Color := clGray;
  end;
end;

procedure TVCLControl.DoEndUpdate;
begin
  if (not FIsForm) and (FControl is TWinControl) then
    LockControl(TWinControl(FControl), False);

  if FIsForm and FUIBuilder.IsMDIStyle and Assigned(Application.MainForm) then
  begin
    SendMessage(Application.MainForm.ClientHandle, WM_SETREDRAW, 1, 0);
    RedrawWindow(Application.MainForm.ClientHandle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

function TVCLControl.GetActiveChildArea: TUIArea;
begin
  if FControl is TPageControl then
    Result := AreaFromSender(TPageControl(FControl).ActivePage)
  else
    Result := nil;
end;

function TVCLControl.GetBounds: TRect;
begin
  Result := TControl(FControl).BoundsRect;
end;

function TVCLControl.GetClientRect: TRect;
begin
  if not (FControl is TForm) then
    Result := TControl(FControl).BoundsRect
  else if TForm(FControl).FormStyle = fsMDIForm then
    Windows.GetClientRect(TForm(FControl).ClientHandle, Result)
  else
    Windows.GetClientRect(TForm(FControl).Handle, Result);
end;

function TVCLControl.GetFocused: Boolean;
begin
  if FControl is TWinControl then
    Result := TWinControl(FControl).Focused
  else
    Result := False;
end;

function TVCLControl.GetModalResult: TModalResult;
begin
  if FControl is TForm then
    Result := TForm(FControl).ModalResult
  else
    Result := mrNone;
end;

function TVCLControl.GetTabOrder: Integer;
begin
  if (FControl is TWinControl) and TWinControl(FControl).TabStop then
    Result := TWinControl(FControl).TabOrder
  else
    Result := -1;
end;

function TVCLControl.GetViewState: TViewState;
begin
  if not (FControl is TControl) or FIsForm then
    Result := vsFullAccess
  else if not TControl(FControl).Visible then
    Result := vsHidden
  else if not TControl(FControl).Enabled then
    Result := vsDisabled
  else
    Result := vsFullAccess;
end;

function TVCLControl.GetWindowState: TWindowState;
begin
  if FControl is TForm then
    Result := TForm(FControl).WindowState
  else
    Result := inherited GetWindowState;
end;

function TVCLControl.IndexOfSender(const ASender: TObject): Integer;
begin
  Result := TMenuItem(ASender).MenuIndex;
end;

procedure TVCLControl.PlaceLabel;
var
  vSpace: Integer;
  vLabel: TLabel;
begin
  if FCaption = nil then
    Exit;

  vLabel := TLabel(FCaption);
  if FLabelPosition = lpTop then
  begin
    vLabel.Left := TControl(FControl).Left;
    vLabel.Top := TControl(FControl).Top - vLabel.Height - 4;
    vLabel.AutoSize := True;
    vLabel.Layout := tlTop;
  end
  else if FLabelPosition = lpLeft then
  begin
    vSpace := vLabel.Width + 8;
    vLabel.Left := TControl(FControl).Left - vSpace;
    vLabel.Top := TControl(FControl).Top + 3;
    vLabel.AutoSize := False;
  end;
  vLabel.Parent := TControl(FControl).Parent;
end;

procedure TVCLControl.SetActiveChildArea(const AArea: TUIArea);
begin
  //
end;

procedure TVCLControl.SetAlignment(const AAlignment: TAlignment);
begin
  if FControl.InheritsFrom(TLabel) then
    TLabel(FControl).Alignment := AAlignment
  else if FControl.InheritsFrom(TEdit) then
    TEdit(FControl).Alignment := AAlignment
  else if FControl.InheritsFrom(TStaticText) then
    TStaticText(FControl).Alignment := AAlignment
  else if FControl.InheritsFrom(TCheckBox) then
  begin
    if AAlignment = taRightJustify then
      TCheckBox(FControl).Alignment := taLeftJustify
    else
      TCheckBox(FControl).Alignment := taRightJustify;
  end
  else if FControl.InheritsFrom(TRadioButton) then
    TRadioButton(FControl).Alignment := AAlignment
  else if FControl is TVCLFileNameFieldEditor then
    TVCLFileNameFieldEditor(FControl).TextEdit.Alignment := AAlignment;
end;

procedure TVCLControl.SetBounds(const Value: TRect);
begin
  if FControl is TControl then
    TControl(FControl).SetBounds(Value.Left, Value.Top, Value.Width, Value.Height);

  PlaceLabel;
end;

procedure TVCLControl.SetCaptionProperty(const ALayout: TLayout);
begin
  if not Assigned(FCaption) then
    Exit;

  if ALayout.Kind in [lkMemo, lkPanel, lkShape] then
  begin
    FShowCaption := ALayout.ShowCaption;
    TLabel(FCaption).Visible := FShowCaption and (FOwner.View.State > vsHidden);
    if akBottom in ALayout.Anchors then
      TLabel(FCaption).Anchors := [akLeft, akBottom]
    else
      TLabel(FCaption).Anchors := [akLeft, akTop];

    if ALayout.Caption_AtLeft then
    begin
      FLabelPosition := lpLeft;
      PlaceLabel;
    end;
  end;
end;

procedure TVCLControl.SetControl(const AControl: TObject);
begin
  if not (AControl is TComponent) then
  begin
    if AControl is TTreeNode then
    begin
      if Assigned(FControl) then
        TTreeNode(FControl).Data := nil;

      FControl := AControl;

      if Assigned(FControl) then
        TTreeNode(FControl).Data := FOwner;

      FIsAutoReleased := True;
    end;

    Exit;
  end;

  if FControl is TWinControl then
  begin
    TCrackedWinControl(FControl).OnEnter := nil;
    TCrackedWinControl(FControl).OnExit := nil;
  end;

  inherited SetControl(AControl);

  if FControl is TWinControl then
  begin
    TCrackedWinControl(FControl).OnEnter := FOwner.OnEnter;
    TCrackedWinControl(FControl).OnExit := FOwner.OnExit;
  end;
end;

procedure TVCLControl.SetFocused(const Value: Boolean);
begin
  if not (FControl is TWinControl) then
    Exit;

  if Value and TWinControl(FControl).CanFocus then
    TWinControl(FControl).SetFocus;
end;

procedure TVCLControl.SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl);
var
  vPopupMenu: TPopupMenu;
begin
  if not Assigned(FControl) then
    Exit;

  if SameText(ATargetName, 'popup') then
  begin
    vPopupMenu := TPopupMenu(TVCLControl(ALinkedControl).Control);
    if not Assigned(vPopupMenu.OnPopup) then
      vPopupMenu.OnPopup := BeforeContextMenuShow;
    TCrackedControl(FControl).PopupMenu := vPopupMenu;
  end;
end;

procedure TVCLControl.SetModalResult(const AModalResult: TModalResult);
begin
  if FControl is TForm then
    TForm(FControl).ModalResult := AModalResult;
end;

procedure TVCLControl.SetParent(const AParent: TUIArea);
begin
  inherited SetParent(AParent);

  if not (FControl is TControl) then
    Exit;

  // Установка родителя для контрола
  if FIsForm or not Assigned(AParent) then
    TControl(FControl).Parent := nil
  else if not Assigned(TControl(FControl).Parent) then
    TControl(FControl).Parent := TWinControl(GetRealControl(AParent));
end;

procedure TVCLControl.SetTabOrder(const ATabOrder: Integer);
begin
  if not (FControl is TWinControl) then
    Exit;

  if ATabOrder >= 0 then
  begin
    TWinControl(FControl).TabStop := True;
    TWinControl(FControl).TabOrder := ATabOrder;
  end
  else
    TWinControl(FControl).TabStop := False;
end;

procedure TVCLControl.SetViewState(const AViewState: TViewState);
begin
  if FIsForm then
    Exit;

  if FControl is TControl then
  begin
    TControl(FControl).Visible := AViewState > vsHidden;
    TControl(FControl).Enabled := AViewState > vsDisabled;
  end
  else if FControl is TMenuItem then
  begin
    TMenuItem(FControl).Visible := AViewState > vsHidden;
    TMenuItem(FControl).Enabled := AViewState > vsDisabled;
  end
  else if FControl is TTreeNode then
  begin
    TTreeNode(FControl).Enabled := AViewState > vsDisabled;
  end;
end;

procedure TVCLControl.SetWindowState(const AWindowState: TWindowState);
begin
  if FControl is TForm then
    TForm(FControl).WindowState := AWindowState;
end;

procedure TVCLControl.UpdateCaptionVisibility;
begin
  if Assigned(FCaption) then
    TLabel(FCaption).Visible := FShowCaption and (FOwner.View.State > vsHidden);
end;

initialization

RegisterClasses([TLabel, TPanel, TSplitter, TImage, TBevel, TPageControl, TMemo, TTabSheet, TScrollBox, TShape, TPopupMenu]);

TPresenter.RegisterControlClass('Windows.VCL', uiNavigation, '', TVCLTreeViewNavigation);
TPresenter.RegisterControlClass('Windows.VCL', uiNavigation, 'TreeView', TVCLTreeViewNavigation);
TPresenter.RegisterControlClass('Windows.VCL', uiNavigation, 'MainMenu', TVCLMainMenuNavigation);
TPresenter.RegisterControlClass('Windows.VCL', uiNavigation, 'ToolBar', TVCLToolBarNavigation);
TPresenter.RegisterControlClass('Windows.VCL', uiEntityEdit, 'OneButton', TVCLOneButtonNavigation);
TPresenter.RegisterControlClass('Windows.DevExpress', uiEntityEdit, 'OneButton', TVCLOneButtonNavigation);

TPresenter.RegisterControlClass('Windows.VCL', uiAction, '', TVCLButton);
TPresenter.RegisterControlClass('Windows.VCL', uiAction, 'link', TVCLLink);

end.
