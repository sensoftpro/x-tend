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

unit vclArea;

interface

uses
  Windows, Classes, Forms, Messages, Generics.Collections, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons, Menus, UITypes, SysUtils,
  uConsts, uUIBuilder, uDefinition, uEntity, uView, uLayout;

//////  1. Привязка сплиттера к контролу
//////  2. Привязка меню к контролу
//////  3. Обработка TabOrder
///  4. Сохранение и восстановление размеров форм в презентере
//////  5. Name и Description
///  6. Перенести AssignFromLayout в общий код создания
///  7. Сделать TCollectionArea, перенести код в DoCreateControl
///  8. Разбраться с надписью для полей
///  9. Внутри у TNativeControl может не быть нативного контрола, а, например, html-текст
///  10. Работа с FParams
///  11. Перенести общее поведение и обработчики в TPresenter

type
  TVCLControl = class(TNativeControl)
  private
    procedure BeforeContextMenuShow(Sender: TObject);
  protected
    function IndexOfControl(const AControl: TObject): Integer; override;
    function AreaFromSender(const ASender: TObject): TUIArea; override;

    procedure DoActivate(const AUrlParams: string); override;
    procedure DoClose(const AModalResult: Integer); override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;

    procedure AssignFromLayout(const ALayout: TLayout; const AParams: string); override;

    procedure SetControl(const AControl: TObject); override;
    function GetControlInfo: string; override;
    procedure SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl); override;
    procedure SetParent(const AParent: TUIArea); override;
    function GetFocused: Boolean; override;
    procedure SetFocused(const Value: Boolean); override;
    function GetBounds: TRect; override;
    procedure SetBounds(const Value: TRect); override;
    function GetViewState: TViewState; override;
    procedure SetViewState(const AViewState: TViewState); override;
    function GetTabOrder: Integer; override;
    procedure SetTabOrder(const ATabOrder: Integer); override;
    function GetActiveChildArea: TUIArea; override;
    procedure SetActiveChildArea(const AArea: TUIArea); override;
    function GetModalResult: TModalResult; override;
    procedure SetModalResult(const AModalResult: TModalResult); override;
    procedure SetAlignment(const AAlignment: TAlignment); override;

    function DoCreateCaption(const AParent: TUIArea; const ACaption, AHint: string): TObject; override;
    procedure PlaceLabel; override;
    procedure SetCaptionProperty(const ALayout: TLayout); virtual;
    procedure UpdateCaptionVisibility; override;
  public
    constructor Create(const AOwner: TUIArea; const AControl: TObject); override;
    destructor Destroy; override;
  end;

  TButtonArea = class(TUIArea)
  private
    FTypeSelectionMenu: TPopupMenu;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure RefillArea(const AKind: Word); override;
  public
    destructor Destroy; override;
  end;

  TLinkArea = class(TUIArea)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TMainMenuArea = class(TNavigationArea)
  private
    FMenu: TMainMenu;
    FMenuItem: TMenuItem;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParentObj: TNativeControl; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TToolBarArea = class(TNavigationArea)
  private
    FToolBar: TToolBar;
    FToolButton: TToolButton;
    FMenuItem: TMenuItem;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParentObj: TNativeControl; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
    procedure DoAfterCreate(const AInteractor: TObject); override;
  end;

  TTreeViewArea = class(TNavigationArea)
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
    function DoCreateItem(const AParentObj: TNativeControl; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
    procedure DoExecuteUIAction(const AView: TView); override;
  end;

  TOneButtonArea = class(TNavigationArea)
  private
    FButton: TButton;
    FMenu: TPopupMenu;
    FItem: TMenuItem;
    procedure OnClick(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParentObj: TNativeControl; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

type
  TCanChangeFieldFunc = function(const AView: TView; const AEntity: TEntity; const AFieldName: string; const ANewValue: Variant): Boolean of object;

implementation

uses
  Types, Graphics, Math, StrUtils, Generics.Defaults, Variants,

  uDomain, uPresenter, uWinVCLPresenter, uConfiguration, uSession, uInteractor, uUtils, uCollection,
  vclSimpleEditors, uEntityList, uDomainUtils, uChangeManager;

type
  TCrackedWinControl = class(TWinControl) end;
  TCrackedControl = class(TControl) end;

  TUIAreaComparer = class(TComparer<TUIArea>)
  public
    function Compare(const ALeft, ARight: TUIArea): Integer; override;
  end;

const
  cServiceAreaHeight = 44;

procedure LockControl(const AWinControl: TWinControl; const ALock: Boolean);
begin
  if (AWinControl = nil) or (AWinControl.Handle = 0) then Exit;
  if ALock then
    SendMessage(AWinControl.Handle, WM_SETREDRAW, 0, 0)
  else
  begin
    SendMessage(AWinControl.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(AWinControl.Handle, nil, 0,
      RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

{ TUIAreaComparer }

function TUIAreaComparer.Compare(const ALeft, ARight: TUIArea): Integer;
begin
  Result := ARight.TabOrder - ALeft.TabOrder;
end;

{ TOneButtonArea }

function TOneButtonArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
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
  FButton.Images := TDragImageList(TInteractor(FView.Interactor).Images[vImageSize]);

  vActionName := GetUrlParam(FParams, 'Action');
  if Length(vActionName) > 0 then
  begin
    FView := FView.BuildView(vActionName);
    Assert(FView.DefinitionKind = dkAction);

    vAction := TActionDef(FView.Definition);
    FButton.ImageIndex := AParent.GetImageID(vAction._ImageID);
    FButton.Caption := AParent.GetTranslation(vAction, tpCaption);
    FButton.Hint := AParent.GetTranslation(vAction, tpHint);
    FButton.OnClick := AParent.OnAreaClick;
  end
  else
  begin
    FButton.ImageIndex := AParent.GetImageID(StrToIntDef(GetUrlParam(FParams, 'ImageIndex'), -1));
    FButton.Caption := GetUrlParam(FParams, 'Caption');
    FButton.Hint := GetUrlParam(FParams, 'Hint');
    FButton.OnClick := OnClick;
  end;

  FMenu := TPopupMenu.Create(nil);
  FMenu.Images := TDragImageList(TInteractor(FView.Interactor).Images[16]);
  FButton.PopupMenu := FMenu;
  FButton.DropDownMenu := FMenu;

  Result := FButton;
end;

function TOneButtonArea.DoCreateItem(const AParentObj: TNativeControl; const ANavItem: TNavigationItem;
  const ALevel: Integer; const ACaption, AHint: string; const AImageIndex: Integer): TObject;
begin
  FItem := TMenuItem.Create(nil);
  FItem.Caption := ACaption;
  FItem.Hint := AHint;
  FItem.ImageIndex := AImageIndex;
  FItem.OnClick := OnAreaClick;
  if Assigned(AParentObj) and (TVCLControl(AParentObj).Control is TMenuItem) then
    TMenuItem(TVCLControl(AParentObj).Control).Add(FItem)
  else
    FMenu.Items.Add(FItem);
  Result := FItem;
end;

procedure TOneButtonArea.OnClick(Sender: TObject);
var
  vPoint: TPoint;
begin
  vPoint := FButton.ClientToScreen(Point(0, FButton.Height));
  FMenu.Popup(vPoint.X, vPoint.Y);
end;

{ TMainMenuArea }

function TMainMenuArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FMenu := TMainMenu.Create(TComponent(AParent.InnerControl));
  FMenu.Images := TDragImageList(TInteractor(FView.Interactor).Images[16]);
  Result := FMenu;
end;

function TMainMenuArea.DoCreateItem(const AParentObj: TNativeControl; const ANavItem: TNavigationItem;
  const ALevel: Integer; const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vForm: TForm;
begin
  FMenuItem := TMenuItem.Create(nil);
  FMenuItem.Caption := ACaption;
  FMenuItem.Hint := AHint;
  FMenuItem.ImageIndex := AImageIndex;
  FMenuItem.OnClick := OnAreaClick;

  Result := nil;
  if ALevel = 0 then
  begin
    FMenu.Items.Add(FMenuItem);
    Result := FMenuItem;
  end
  else
  begin
    if Assigned(AParentObj) and (TVCLControl(AParentObj).Control is TMenuItem) then
    begin
      TMenuItem(TVCLControl(AParentObj).Control).Add(FMenuItem);
      Result := FMenuItem;
    end;
  end;

  if ANavItem.Id = 'Windows' then
  begin
    //FMenuItem.Visible := TInteractor(FView.Interactor).Layout = 'mdi';
    if (TInteractor(FView.Interactor).Layout = 'mdi') and (Parent.NativeControl.IsForm) then
    begin
      vForm := TForm(Parent.InnerControl);
      if vForm.FormStyle = fsMDIForm then
        vForm.WindowMenu := FMenuItem;
    end;
  end;
end;

{ TToolBarArea }

procedure TToolBarArea.DoAfterCreate(const AInteractor: TObject);
var
  vToolButton: TToolButton;
  i, vImageSize: Integer;
begin
  vImageSize := StrToIntDef(GetUrlParam(FParams, 'ImageSize'), 16);
  FToolBar.Images := TDragImageList(TInteractor(AInteractor).Images[vImageSize]);
  FToolBar.AutoSize := True;
  for i := 0 to FToolBar.ButtonCount - 1 do
  begin
    vToolButton := FToolBar.Buttons[i];
    if Assigned(vToolButton.DropdownMenu) then
      vToolButton.DropdownMenu.Images := FToolBar.Images;
  end
end;

function TToolBarArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FToolBar := TToolBar.Create(TComponent(AParent.InnerControl));
  FToolBar.ShowCaptions := True;
  Result := FToolBar;
end;

function TToolBarArea.DoCreateItem(const AParentObj: TNativeControl; const ANavItem: TNavigationItem; const ALevel: Integer;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vMenu: TPopupMenu;
  vLeft: Integer;
  vToolButton: TToolButton;
  vParentObj: TObject;
begin
  Result := nil;

  if ALevel = 0 then
  begin
    if FToolBar.ButtonCount > 0 then
    begin
      vToolButton := FToolBar.Buttons[FToolBar.ButtonCount - 1];
      vLeft := vToolButton.Left + vToolButton.Width + 10;
    end
    else
      vLeft := 0;

    FToolButton := TToolButton.Create(FToolBar);
    FToolButton.AutoSize := True;
    FToolButton.Left := vLeft;
    FToolButton.Caption := ACaption;
    FToolButton.Hint := AHint;
    FToolButton.ImageIndex := AImageIndex;
    FToolButton.OnClick := OnAreaClick;

    Result := FToolButton;
  end
  else begin
    if Assigned(AParentObj) then
    begin
      vParentObj := TVCLControl(AParentObj).Control;

      FMenuItem := TMenuItem.Create(FToolBar);
      FMenuItem.Caption := ACaption;
      FMenuItem.Hint := AHint;
      FMenuItem.ImageIndex := AImageIndex;
      FMenuItem.OnClick := OnAreaClick;

      if vParentObj is TToolButton then
      begin
        TToolButton(vParentObj).Style := tbsDropDown;
        if Assigned(TToolButton(vParentObj).DropdownMenu) then
          vMenu := TToolButton(vParentObj).DropdownMenu
        else
        begin
          vMenu := TPopupMenu.Create(FToolBar);
          TToolButton(vParentObj).DropdownMenu := vMenu;
        end;

        vMenu.Items.Add(FMenuItem);
      end
      else if vParentObj is TMenuItem then
      begin
        TMenuItem(vParentObj).Add(FMenuItem);
      end;

      Result := FMenuItem;
    end;
  end;
end;

{ TTreeViewArea }

function TTreeViewArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FTreeView := TTreeView.Create(nil);
  //FTreeView.Images := TDragImageList(TInteractor(FView.Interactor).Images[16]);
  FTreeView.ReadOnly := True;
  FTreeView.DoubleBuffered := True;
  FTreeView.HideSelection := False;
  FTreeView.OnClick := OnMouseClick;
  FTreeView.OnKeyPress := OnKeyPress;
  //FTreeView.OnCustomDrawItem := OnCustomDrawItem;
  FId := 'TreeView';

  FDefaultWorkArea := FCreateParams.Values['ContentWorkArea'];
  if FDefaultWorkArea = '' then
    FDefaultWorkArea := 'WorkArea';
  Result := FTreeView;
end;

function TTreeViewArea.DoCreateItem(const AParentObj: TNativeControl; const ANavItem: TNavigationItem;
  const ALevel: Integer; const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vParentNode: TTreeNode;
  vTreeNode: TTreeNode;
begin
  if ANavItem.IsLine then
    Exit(nil);

  if Assigned(AParentObj) then
    vParentNode := TTreeNode(TVCLControl(AParentObj).Control)
  else
    vParentNode := nil;

  vTreeNode := FTreeView.Items.AddChild(vParentNode, ACaption);
  vTreeNode.ImageIndex := AImageIndex;
  vTreeNode.SelectedIndex := -1;

  Result := vTreeNode;

  if Assigned(vParentNode) then
    vParentNode.Expand(True);
end;

procedure TTreeViewArea.DoExecuteUIAction(const AView: TView);
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

procedure TTreeViewArea.OnKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and Assigned(FTreeView.Selected) then
    SelectNode(FTreeView.Selected);
end;

procedure TTreeViewArea.OnMouseClick(Sender: TObject);
var
  vClickPos: TPoint;
  vNode: TTreeNode;
begin
  vClickPos :=  FTreeView.ScreenToClient(Mouse.CursorPos);
  vNode := FTreeView.GetNodeAt(vClickPos.X, vClickPos.Y);
  if Assigned(vNode) then
    SelectNode(vNode);
end;

procedure TTreeViewArea.Refill;
begin
  Clear;
  FTreeView.Items.Clear;
  ProcessChilds;
end;

procedure TTreeViewArea.SelectNode(const ANode: TTreeNode);
var
  vArea: TUIArea;
begin
  if not Assigned(ANode) then
    Exit;

  vArea := TUIArea(ANode.Data);
  if Assigned(vArea) then
    ProcessAreaClick(vArea);
end;

{ TButtonArea }

destructor TButtonArea.Destroy;
begin
  FreeAndNil(FTypeSelectionMenu);
  inherited Destroy;
end;

function TButtonArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vParams: TStrings;
  vButton: TButton;
  vActionDef: TDefinition;
  vDefinitions: TList<TDefinition>;
  i: Integer;
  vMenuItem: TMenuItem;
  vDefinition: TDefinition;
  vImageID: Integer;
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
    vImageID := StrToIntDef(vParams.Values['ImageID'], vActionDef._ImageID);
    vComposition := Trim(vParams.Values['Composition']);
    vViewStyle := Trim(vParams.Values['ViewStyle']);
    vOverriddenCaption := Trim(vParams.Values['Caption']);
    vOverriddenHint := Trim(vParams.Values['Hint']);
  finally
    FreeAndNil(vParams);
  end;

  vButton := TButton.Create(nil);
  vButton.Images := TDragImageList(TInteractor(Interactor).Images[vImageSize]);
  vImageID := GetImageID(vImageID);

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
  if (vButton.Images.Count + 1 >= vImageID) and (vImageID > 0) then
  begin
    if vComposition = '' then
    begin
      if ALayout.Button_ShowCaption then
        vButton.Caption := vCaption
      else begin
        vButton.ImageAlignment := TImageAlignment.iaCenter;
        vButton.ImageIndex := vImageID;
      end;
    end
    else if vComposition = 'TextOnly' then
      vButton.Caption := vCaption
    else if vComposition = 'ImageOnly' then
    begin
      vButton.ImageAlignment := TImageAlignment.iaCenter;
      vButton.ImageIndex := vImageID;
    end
    else begin
      vButton.Caption := vCaption;
      vButton.ImageIndex := vImageID;
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
      FTypeSelectionMenu.Images := TDragImageList(TInteractor(Interactor).Images[16]);
      for i := 0 to vDefinitions.Count - 1 do
      begin
        vDefinition := TDefinition(vDefinitions[i]);
        vMenuItem := TMenuItem.Create(nil);
        vMenuItem.Caption := GetTranslation(vDefinition);
        if Length(vOverriddenCaption) > 0 then
          vMenuItem.Caption := vOverriddenCaption;
        vMenuItem.ImageIndex := GetImageID(vDefinition._ImageID);
        vMenuItem.Tag := NativeInt(Self); //NativeInt(vButton);
        vMenuItem.OnClick := OnActionMenuSelected;
        FTypeSelectionMenu.Items.Add(vMenuItem);
      end;
      vButton.DropDownMenu := FTypeSelectionMenu;
      vButton.Style := bsSplitButton;
      vButton.ImageMargins.Left := 4;
      vButton.ImageMargins.Top := 4;
      vButton.ImageMargins.Right := 4;
      vButton.ImageMargins.Bottom := 4;
    end
    else begin
      vButton.OnClick := OnAreaClick;
      ALayout.Width := ALayout.Height;
    end;
  end
  else
    vButton.OnClick := OnAreaClick;

  Result := vButton;
end;

procedure TButtonArea.RefillArea(const AKind: Word);
var
  vButton: TButton;
  vActionDef: TDefinition;
  vImageID: Integer;
begin
  if AKind <> dckContentTypeChanged then
  begin
    inherited RefillArea(AKind);
    Exit;
  end;

  vButton := TButton(FControl);

  vActionDef := TDefinition(FView.Definition);
  vImageID := GetImageID(vActionDef._ImageID);

  if (vButton.Images.Count + 1 >= vImageID) and (vImageID > 0) then
    vButton.ImageIndex := vImageID;

  vButton.Caption := GetTranslation(vActionDef);
  vButton.Hint := vButton.Caption;
end;

{ TLinkArea }

function TLinkArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
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
  vLabel.Caption := GetTranslation(vActionDef);
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
  vLabel.OnClick := OnAreaClick;

  Result := vLabel;
end;

procedure TLinkArea.RefillArea(const AKind: Word);
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

function TVCLControl.AreaFromSender(const ASender: TObject): TUIArea;
begin
  if Assigned(ASender) and (ASender is TComponent) then
    Result := TUIArea(TComponent(ASender).Tag)
  else
    Result := nil;
end;

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

      if (ALayout.Tag and cFormPositionDesign) > 0 then
        vForm.Position := poDesigned;

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
        TTabSheet(FControl).ImageIndex := FOwner.GetImageId(StrToIntDef(GetUrlParam(ALayout.Caption, 'ImageIndex', ''), -1));
      end;
    end;
  end
  else if (ALayout.Kind in [lkPanel, lkMemo]) and (FControl is TControl) then
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

    if not FOwner.IsDefault then
    begin
      TCrackedControl(FControl).Color := AlphaColorToColor(ALayout.Color);
      TCrackedControl(FControl).ParentColor := False;
      if FControl is TWinControl then
        TCrackedWinControl(FControl).ParentBackground := False;
    end;
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

constructor TVCLControl.Create(const AOwner: TUIArea; const AControl: TObject);
begin
  inherited Create(AOwner, AControl);

  FIsForm := AControl is TForm;
  FIsAutoReleased := AControl is TMenuItem;
end;

destructor TVCLControl.Destroy;
var
  vControl: TObject;
  vIsFloat: Boolean;
begin
  if Assigned(FCaption) then
    TLabel(FCaption).Parent := nil;

  vControl := FControl;
  vIsFloat := FOwner.Id = 'float';

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
    SetWindowLong(vForm.ClientHandle, GWL_USERDATA, SetWindowLong(vForm.ClientHandle, GWL_WNDPROC, Integer(@ClientWindowProc)));

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
      TPageControl(TControl(FControl).Parent).ActivePage := TTabSheet(FControl);
  end;
end;

procedure TVCLControl.DoBeginUpdate;
begin
  if (not FIsForm) and (FControl is TWinControl) then
    LockControl(TWinControl(FControl), True);

  if FIsForm and (TInteractor(FInteractor).Layout = 'mdi') then
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

  vLabel.Parent := TWinControl(FParent.InnerControl);
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

  if FIsForm and (TInteractor(FInteractor).Layout = 'mdi') then
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

function TVCLControl.GetControlInfo: string;
begin
  if not Assigned(FControl) then
    Result := 'NULL'
  else
    Result := FControl.ClassName;
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

function TVCLControl.IndexOfControl(const AControl: TObject): Integer;
begin
  Result := TMenuItem(AControl).MenuIndex;
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
    TCheckBox(FControl).Alignment := AAlignment
  else if FControl.InheritsFrom(TRadioButton) then
    TRadioButton(FControl).Alignment := AAlignment
  else if FControl.InheritsFrom(TLabel) then
    TLabel(FControl).Alignment := AAlignment
  else if FControl.InheritsFrom(TEdit) then
    TEdit(FControl).Alignment:= AAlignment
  else if FOwner is TFilenameFieldEditor then
    TFilenameFieldEditor(FOwner).TextEdit.Alignment := AAlignment;
end;

procedure TVCLControl.SetBounds(const Value: TRect);
begin
  if FControl is TControl then
    TControl(FControl).SetBounds(Value.Left, Value.Top, Value.Width, Value.Height);

  PlaceLabel;
end;

procedure TVCLControl.SetCaptionProperty(const ALayout: TLayout);
begin
  if not Assigned(FCaption) or not Assigned(ALayout) then
    Exit;

  if ALayout.Kind in [lkMemo, lkPanel] then
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

      inherited SetControl(AControl);

      if Assigned(FControl) then
        TTreeNode(FControl).Data := FOwner;

      FIsAutoReleased := True;
    end;

    Exit;
  end;

  if Assigned(FControl) then
  begin
    TCrackedWinControl(FControl).Tag := 0;
    if FControl is TWinControl then
    begin
      TCrackedWinControl(FControl).OnEnter := nil;
      TCrackedWinControl(FControl).OnExit := nil;
    end;
  end;

  inherited SetControl(AControl);

  if Assigned(FControl) then
  begin
    if TCrackedWinControl(FControl).Tag = 0 then
      TCrackedWinControl(FControl).Tag := NativeInt(FOwner);
    if FControl is TWinControl then
    begin
      TCrackedWinControl(FControl).OnEnter := FOwner.OnEnter;
      TCrackedWinControl(FControl).OnExit := FOwner.OnExit;
    end;
  end;

  FIsForm := FControl is TForm;
  FIsAutoReleased := FControl is TMenuItem;
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
    vPopupMenu := TPopupMenu(ALinkedControl.Control);
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
    TControl(FControl).Parent := TWinControl(AParent.InnerControl);
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

procedure TVCLControl.UpdateCaptionVisibility;
begin
  if Assigned(FCaption) then
    TLabel(FCaption).Visible := FShowCaption and (FOwner.View.State > vsHidden);
end;

initialization

RegisterClasses([TLabel, TPanel, TSplitter, TImage, TBevel, TPageControl, TMemo, TTabSheet, TScrollBox, TShape, TPopupMenu]);

TPresenter.RegisterUIClass('Windows.VCL', uiNavigation, '', TTreeViewArea);
TPresenter.RegisterUIClass('Windows.VCL', uiNavigation, 'TreeView', TTreeViewArea);
TPresenter.RegisterUIClass('Windows.VCL', uiNavigation, 'MainMenu', TMainMenuArea);
TPresenter.RegisterUIClass('Windows.VCL', uiNavigation, 'ToolBar', TToolBarArea);
//TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, 'OneButton', TOneButtonArea);

TPresenter.RegisterUIClass('Windows.VCL', uiAction, '', TButtonArea);
TPresenter.RegisterUIClass('Windows.VCL', uiAction, 'link', TLinkArea);

end.
