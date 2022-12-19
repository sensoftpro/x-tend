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
  Windows, Classes, Forms, Messages, Generics.Collections, Controls, StdCtrls, ExtCtrls, Menus, UITypes, SysUtils,
  uConsts, uUIBuilder, uDefinition, uEntity, uView, uLayout;

type
  TVCLControl = class(TNativeControl)
  private
    procedure BeforeContextMenuShow(Sender: TObject);
  protected
    function IndexOfControl(const AControl: TObject): Integer; override;
    function AreaFromSender(const ASender: TObject): TUIArea; override;
    procedure PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DoClose(const AModalResult: Integer); override;
    function GetName: string; override;
    procedure DoActivate(const AUrlParams: string); override;

    function DoCreateCaption(const AParent: TUIArea; const ACaption, AHint: string): TObject; override;
    procedure SetParent(const AParent: TUIArea); override;
    procedure SetControl(const AControl: TObject); override;
    function DoGetDescription: string; override;
    procedure AssignFromLayout(const ALayout: TLayout; const AParams: string); override;
    procedure SetPopupArea(const APopupArea: TUIArea); override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    function GetFocused: Boolean; override;
    procedure SetFocused(const Value: Boolean); override;

    procedure ApplyTabStops(const ALayout: TLayout); override;
    procedure DoAfterChildAreasCreated; override;

    procedure PlaceLabel; override;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetCaptionProperty(const ALayout: TLayout); virtual;
  public
    constructor Create(const AOwner: TUIArea; const AControl: TObject); override;
    destructor Destroy; override;

    procedure SetViewState(const AValue: TViewState); override;
    procedure UpdateCaptionVisibility; override;
  end;

  TVCLArea = class(TUIArea)
  end;

  TButtonArea = class(TActionArea)
  private
    FTypeSelectionMenu: TPopupMenu;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure RefillArea(const AKind: Word); override;
  public
    destructor Destroy; override;
  end;

  TLinkArea = class(TActionArea)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure RefillArea(const AKind: Word); override;
  end;

type
  TCanChangeFieldFunc = function(const AView: TView; const AEntity: TEntity; const AFieldName: string; const ANewValue: Variant): Boolean of object;

implementation

uses
  Types, Graphics, Math, StrUtils, ComCtrls, Buttons,
  Generics.Defaults, Variants, cxGraphics, dxGDIPlusClasses, cxLabel, cxImage, cxEdit, cxTextEdit, cxPC, dxBar, dxNavBar, dxNavBarGroupItems,
  dxNavBarCollns, dxNavBarBase, dxNavBarExplorerViews,
  cxLookAndFeels, cxButtons, cxScrollBox, cxControls, cxSplitter,

  uDomain, uPresenter, uWinVCLPresenter, uConfiguration, uSession, uInteractor, uUtils, uCollection,
  vclSimpleEditors, uEntityList, uDomainUtils, uChangeManager;

type
  TCrackedWinControl = class(TWinControl) end;
  TCrackedControl = class(TControl) end;

type
  TUIAreaComparer = class(TComparer<TUIArea>)
  public
    function Compare(const ALeft, ARight: TUIArea): Integer; override;
  end;

  TMainMenuArea = class(TNavigationArea)
  private
    FMenu: TMainMenu;
    FMenuItem: TMenuItem;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TToolBarArea = class(TNavigationArea)
  private
    FToolBar: TToolBar;
    FToolButton: TToolButton;
    FMenuItem: TMenuItem;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
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
    function DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
    procedure DoExecuteUIAction(const AView: TView); override;
  end;

  TNavBarArea = class(TNavigationArea)
  private
    FNavBar: TdxNavBar;
    FNavBarGroup: TdxNavBarGroup;
    FNavBarItem: TdxNavBarItem;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TOneButtonArea = class(TNavigationArea)
  private
    FButton: TButton;
    FMenu: TPopupMenu;
    FItem: TMenuItem;
    procedure OnClick(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
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

{ TNavBarArea }

function TNavBarArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vColor: TColor;
begin
  FNavBar := TdxNavBar.Create(nil);
  FNavBar.DoubleBuffered := True;
  FNavBar.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
  FNavBar.SmallImages := TDragImageList(TInteractor(FView.Interactor).Images[16]);
  FNavBar.LargeImages := TDragImageList(TInteractor(FView.Interactor).Images[32]);
  FNavBar.View := StrToIntDef(GetUrlParam(ALayout.Params, 'NavBarKind'), 4);
  CopyFontSettings(FNavBar.OptionsStyle.DefaultStyles.GroupHeader.Font, ALayout);
  FNavBar.OptionsStyle.DefaultStyles.GroupHeader.Font.Size := 12;
  CopyFontSettings(FNavBar.OptionsStyle.DefaultStyles.Item.Font, ALayout);
  vColor := AlphaColorToColor(ALayout.Color);
  FNavBar.OptionsStyle.DefaultStyles.Background.BackColor := vColor;
  FNavBar.OptionsStyle.DefaultStyles.Background.BackColor2 := DimColor(vColor, 0.2);
  FNavBar.OptionsStyle.DefaultStyles.GroupBackground.BackColor := vColor;
  FNavBar.OptionsStyle.DefaultStyles.GroupBackground.BackColor2 := DimColor(vColor, 0.2);
  FNavBar.OptionsStyle.DefaultStyles.GroupHeader.BackColor := vColor;
  FNavBar.OptionsStyle.DefaultStyles.GroupHeader.BackColor2 := DimColor(vColor, 0.2);
  FNavBar.DragDropFlags := [];

  FNavBarGroup := nil;
  FNavBarItem := nil;

  Result := FNavBar;
end;

function TNavBarArea.DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem;
  const ALevel: Integer; const ACaption, AHint: string; const AImageIndex: Integer): TObject;
begin
  if ALevel = 0 then
  begin
    FNavBarGroup := FNavBar.Groups.Add;
    FNavBarGroup.LinksUseSmallImages := False;
    FNavBarGroup.OptionsExpansion.Expandable := False;
    FNavBarGroup.OptionsExpansion.ShowExpandButton := False;
    FNavBarGroup.Caption := ACaption;
    FNavBarGroup.Hint := AHint;
    FNavBarGroup.SmallImageIndex := AImageIndex;
    FNavBarGroup.LargeImageIndex := AImageIndex;
    FNavBarGroup.OnClick := nil;
    Result := FNavBarGroup;
  end
  else
  begin
    FNavBarItem := FNavBar.Items.Add;
    if Assigned(AParentObj) and (AParentObj is TdxNavBarGroup) then
      TdxNavBarGroup(AParentObj).CreateLink(FNavBarItem);
    FNavBarItem.Caption := ACaption;
    FNavBarItem.Hint := AHint;
    FNavBarItem.SmallImageIndex := AImageIndex;
    FNavBarItem.LargeImageIndex := AImageIndex;
    FNavBarItem.OnClick := OnAreaClick;
    Result := FNavBarItem;
  end;
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

function TOneButtonArea.DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem;
  const ALevel: Integer; const ACaption, AHint: string; const AImageIndex: Integer): TObject;
begin
  FItem := TMenuItem.Create(nil);
  FItem.Caption := ACaption;
  FItem.Hint := AHint;
  FItem.ImageIndex := AImageIndex;
  FItem.OnClick := OnAreaClick;
  if Assigned(AParentObj) then
  begin
    Assert(AParentObj is TMenuItem);
    TMenuItem(AParentObj).Add(FItem);
  end
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

function TMainMenuArea.DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem;
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
    if Assigned(AParentObj) and (AParentObj is TMenuItem) then
    begin
      TMenuItem(AParentObj).Add(FMenuItem);
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

function TToolBarArea.DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vMenu: TPopupMenu;
  vLeft: Integer;
  vToolButton: TToolButton;
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
      FMenuItem := TMenuItem.Create(FToolBar);
      FMenuItem.Caption := ACaption;
      FMenuItem.Hint := AHint;
      FMenuItem.ImageIndex := AImageIndex;
      FMenuItem.OnClick := OnAreaClick;

      if AParentObj is TToolButton then
      begin
        TToolButton(AParentObj).Style := tbsDropDown;
        if Assigned(TToolButton(AParentObj).DropdownMenu) then
          vMenu := TToolButton(AParentObj).DropdownMenu
        else
        begin
          vMenu := TPopupMenu.Create(FToolBar);
          TToolButton(AParentObj).DropdownMenu := vMenu;
        end;

        vMenu.Items.Add(FMenuItem);
      end
      else if AParentObj is TMenuItem then
      begin
        TMenuItem(AParentObj).Add(FMenuItem);
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

function TTreeViewArea.DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem;
  const ALevel: Integer; const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vParentNode: TTreeNode absolute AParentObj;
  vTreeNode: TTreeNode;
begin
  if ANavItem.IsLine then
    Exit(nil);

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
  vButton: TcxButton;
  vActionDef: TDefinition;
  vDefinitions: TList<TDefinition>;
  i: Integer;
  vMenuItem: TMenuItem;
  vDefinition: TDefinition;
  vImageID: Integer;
  vImageSize: Integer;
  vComposition: string;
  vViewStyle: string;
  vOverriddenCaption, vOverriddenHint: string;
begin
  vParams := CreateDelimitedList(FParams, '&');

  vActionDef := TDefinition(FView.Definition);

  vImageSize := StrToIntDef(vParams.Values['ImageSize'], 16);
  vImageID := StrToIntDef(vParams.Values['ImageID'], vActionDef._ImageID);
  vComposition := Trim(vParams.Values['Composition']);
  vViewStyle := Trim(vParams.Values['ViewStyle']);
  vOverriddenCaption := Trim(vParams.Values['Caption']);
  vOverriddenHint := Trim(vParams.Values['Hint']);

  vButton := TcxButton.Create(nil);
  vButton.OptionsImage.Images := TDragImageList(TInteractor(Interactor).Images[vImageSize]);
  vImageID := GetImageID(vImageID);

  if (ALayout.BevelOuter = lbkNone) and (ALayout.BevelInner = lbkNone) then
  begin
    vButton.SpeedButtonOptions.Flat := True;
    vButton.SpeedButtonOptions.CanBeFocused := False;
  end;

  vButton.Caption := GetTranslation(vActionDef);
  vButton.Hint := vButton.Caption;
  if Length(vOverriddenCaption) > 0 then
    vButton.Caption := vOverriddenCaption;
  if Length(vOverriddenHint) > 0 then
    vButton.Hint := vOverriddenHint;

  if (vButton.OptionsImage.Images.Count + 1 >= vImageID) and (vImageID > 0) then
  begin
    if vComposition = '' then
    begin
      if ALayout.Button_ShowCaption then
        vButton.PaintStyle := bpsDefault
      else
        vButton.PaintStyle := bpsGlyph;
    end
    else if vComposition = 'TextOnly' then
    begin
      vButton.PaintStyle := bpsCaption;
    end
    else if vComposition = 'ImageOnly' then
      vButton.PaintStyle := bpsGlyph
    else
    begin
      vButton.PaintStyle := bpsDefault;
      if vComposition = 'ImageRight' then
        vButton.Layout := TButtonLayout.blGlyphRight
      else if vComposition = 'ImageTop' then
        vButton.Layout := TButtonLayout.blGlyphTop
      else if vComposition = 'ImageBottom' then
        vButton.Layout := TButtonLayout.blGlyphBottom
      else
        vButton.Layout := TButtonLayout.blGlyphLeft;
    end;
    vButton.OptionsImage.ImageIndex := vImageID;
  end
  else
  begin
    vButton.PaintStyle := bpsCaption;
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
        vMenuItem.Tag := Integer(Self); //Integer(vButton);
        vMenuItem.OnClick := OnActionMenuSelected;
        FTypeSelectionMenu.Items.Add(vMenuItem);
      end;
      vButton.DropDownMenu := FTypeSelectionMenu;
      vButton.Kind := cxbkOfficeDropDown;
      vButton.OptionsImage.Margin := 4;
    end
    else begin
      vButton.OnClick := OnAreaClick;
      ALayout.Width := ALayout.Height;
    end;
  end
  else
    vButton.OnClick := OnAreaClick;

  AddParams(vParams);

  Result := vButton;
end;

procedure TButtonArea.RefillArea(const AKind: Word);
var
  vButton: TcxButton;
  vActionDef: TDefinition;
  vImageID: Integer;
begin
  if AKind <> dckContentTypeChanged then
  begin
    inherited RefillArea(AKind);
    Exit;
  end;

  vButton := TcxButton(FControl);

  vActionDef := TDefinition(FView.Definition);
  vImageID := GetImageID(vActionDef._ImageID);

  if (vButton.OptionsImage.Images.Count + 1 >= vImageID) and (vImageID > 0) then
    vButton.OptionsImage.ImageIndex := vImageID;

  vButton.Caption := GetTranslation(vActionDef);
  vButton.Hint := vButton.Caption;
end;

{ TLinkArea }

function TLinkArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vParams: TStrings;
  vLabel: TcxLabel;
  vActionDef: TDefinition;
  vComposition: string;
  vViewStyle: string;
  vOverriddenCaption, vOverriddenHint: string;
begin
  vParams := CreateDelimitedList(FParams, '&');

  vActionDef := TDefinition(FView.Definition);

  vComposition := Trim(vParams.Values['Composition']);
  vViewStyle := Trim(vParams.Values['ViewStyle']);
  vOverriddenCaption := Trim(vParams.Values['Caption']);
  vOverriddenHint := Trim(vParams.Values['Hint']);

  vLabel := TcxLabel.Create(nil);
  vLabel.Caption := GetTranslation(vActionDef);
  vLabel.Hint := vLabel.Caption;
  if Length(vOverriddenCaption) > 0 then
    vLabel.Caption := vOverriddenCaption;
  if Length(vOverriddenHint) > 0 then
    vLabel.Hint := vOverriddenHint;
  vLabel.Cursor := crHandPoint;
  vLabel.Transparent := True;
  vLabel.Properties.Alignment.Vert := TcxEditVertAlignment.taVCenter;
  vLabel.Style.TextColor := AlphaColorToColor(ALayout.Font.Color);
  //vLabel.Style.TextStyle := [fsUnderline];
  vLabel.Style.HotTrack := True;
  //vLabel.StyleHot.TextColor := clBlue;
  vLabel.StyleHot.TextStyle := [fsUnderline];
  vLabel.OnClick := OnAreaClick;

  AddParams(vParams);

  Result := vLabel;
end;

procedure TLinkArea.RefillArea(const AKind: Word);
var
  vLabel: TcxLabel;
  vActionDef: TDefinition;
begin
  if AKind <> dckContentTypeChanged then
  begin
    inherited RefillArea(AKind);
    Exit;
  end;

  vLabel := TcxLabel(FControl);

  vActionDef := TDefinition(FView.Definition);
  vLabel.Caption := GetTranslation(vActionDef);
  vLabel.Hint := vLabel.Caption;
end;

{ TVCLControl }

procedure TVCLControl.ApplyTabStops(const ALayout: TLayout);
begin
  if FControl is TWinControl then
  begin
    FTabStop := ALayout.TabStop;
    FTabOrder := ALayout.TabOrder;
    TWinControl(FControl).TabStop := FTabStop;
  end;
end;

function TVCLControl.AreaFromSender(const ASender: TObject): TUIArea;
begin
  if Assigned(ASender) and (ASender is TComponent) then
    Result := TUIArea(TComponent(ASender).Tag)
  else
    Result := nil;
end;

procedure TVCLControl.AssignFromLayout(const ALayout: TLayout; const AParams: string);
var
  //vFrame: TFrame;
  //vPanel: TCrackedWinControl;
  vForm: TForm;
  vAlignment: TAlignment;
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
    if (ALayout.Caption <> '') and (FControl is TcxTabSheet) then
    begin
      TcxTabSheet(FControl).Caption := ALayout.Caption;
      if Pos('=', ALayout.Caption) > 0 then // Hint содержит url-строку с параметрами
      begin
        TcxTabSheet(FControl).Caption := GetUrlParam(ALayout.Caption, 'Caption', '');
        TcxTabSheet(FControl).ImageIndex := FOwner.GetImageId(StrToIntDef(GetUrlParam(ALayout.Caption, 'ImageIndex', ''), -1));
      end;
    end;
  end
  else if (ALayout.Kind in [lkPanel, lkMemo]) and (FControl is TControl) then
  begin
    PlaceIntoBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    SetCaptionProperty(ALayout);

    if FControl is TcxCustomEdit then
      CopyFontSettings(TcxCustomEdit(FControl).Style.Font, ALayout)
    else
      CopyFontSettings(TCrackedWinControl(FControl).Font, ALayout);

    TControl(FControl).Anchors := ALayout.Anchors;
    TControl(FControl).Align := TAlign(ALayout.Align);
    CopyMargins(TControl(FControl), ALayout);
    if FControl is TWinControl then
      CopyPadding(TWinControl(FControl), ALayout);

    vAlignment := ALayout.Alignment;
    if vAlignment = taRightJustify then
    begin
      if FControl.InheritsFrom(TLabel) then
        TLabel(FControl).Alignment := taRightJustify
      else if FControl.InheritsFrom(TEdit) then
        TEdit(FControl).Alignment := taRightJustify
      else if FControl.InheritsFrom(TStaticText) then
      begin
        TStaticText(FControl).Alignment := taRightJustify;
        TStaticText(FControl).AutoSize := False;
        TStaticText(FControl).Height := ALayout.Height;
        TStaticText(FControl).Width := ALayout.Width;
      end
      else if FControl.InheritsFrom(TCheckBox) then
        TCheckBox(FControl).Alignment := taRightJustify
      else if FControl.InheritsFrom(TRadioButton) then
        TRadioButton(FControl).Alignment := taRightJustify
      else if FControl.InheritsFrom(TcxLabel) then
        TcxLabel(FControl).Properties.Alignment.Horz := taRightJustify
      else if FControl.InheritsFrom(TcxTextEdit) then
        TcxTextEdit(FControl).Properties.Alignment.Horz := taRightJustify
      else if FOwner is TFilenameFieldEditor then
        TFilenameFieldEditor(FOwner).TextEdit.Properties.Alignment.Horz := taRightJustify
    end;

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

procedure TVCLControl.DoActivate(const AUrlParams: string);
var
  vChangeTab: Boolean;
begin
  vChangeTab := SameText(GetUrlParam(AUrlParams, 'TabActivationOption', ''), 'ChangeTab');

  if (FControl is TcxTabSheet) then
  begin
    if Assigned(Parent.CreateParams) and (Parent.CreateParams.IndexOfName('TabActivationOption') >= 0) and
      (not SameText(Parent.CreateParams.Values['TabActivationOption'], 'ChangeTab')) then
      vChangeTab := False;
    if vChangeTab then
      TcxPageControl(TControl(FControl).Parent).Properties.ActivePage := TcxTabSheet(FControl);
  end
  else if (FControl is TForm) and (TForm(FControl).FormStyle = fsMDIChild) then
  begin
    if SameText(GetUrlParam(AUrlParams, 'State'), 'Max') then
      TForm(FControl).WindowState := wsMaximized
    else
    begin
      TForm(FControl).BringToFront;
    end;
  end;
end;

procedure TVCLControl.DoAfterChildAreasCreated;
var
  i: Integer;
  function FindControlForSplitter(const ASplitter: TcxSplitter): TControl;
  var
    c: Integer;
  begin
    Result := nil;
    for c := 0 to FOwner.Count - 1 do
      if (FOwner[c].InnerControl <> ASplitter) and (FOwner[c].InnerControl is TControl)
        and (TControl(FOwner[c].InnerControl).Align = ASplitter.Align) then
      begin
        Result := TControl(FOwner[c].InnerControl);
        Break;
      end;
  end;

  procedure ApplyTabOrder;
  var
    vList: TList<TUIArea>;
    v: Integer;
  begin
    vList := TList<TUIArea>.Create(TUIAreaComparer.Create);
    try
      for v := 0 to FOwner.Count - 1 do
        if (FOwner[v].InnerControl is TWinControl) and TWinControl(FOwner[v].InnerControl).TabStop then
          vList.Add(TUIArea(FOwner[v]));

      vList.Sort;

      for v := 0 to vList.Count - 1 do
        TWinControl(vList[v].InnerControl).TabOrder := vList[v].TabOrder;
    finally
      FreeAndNil(vList);
    end;
  end;
begin
  // прицепляем сплиттер к своему контролу, чтобы не отлипал в некоторых случаях, обрабатываем только простой случай: сплиттер с таким размещением один в текущей области
  for i := 0 to FOwner.Count - 1 do
    if FOwner[i].InnerControl is TcxSplitter then
      TcxSplitter(FOwner[i].InnerControl).Control := FindControlForSplitter(TcxSplitter(FOwner[i].InnerControl));

  if FOwner.Count > 1 then
    ApplyTabOrder;
end;

procedure TVCLControl.DoBeginUpdate;
begin
  if (not FIsForm) and (FControl is TWinControl) then
    LockControl(TWinControl(FControl), True);

  if FIsForm and (TInteractor(FInteractor).Layout = 'mdi') then
    SendMessage(Application.MainForm.ClientHandle, WM_SETREDRAW, 0, 0);
end;

procedure TVCLControl.DoClose(const AModalResult: Integer);
begin
  if FIsForm then
  begin
    if AModalResult = mrNone then
      PostMessage(TForm(FControl).Handle, WM_CLOSE, 0, 0)
    else begin
      TForm(FControl).Close;
      TForm(FControl).ModalResult := AModalResult;
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

function TVCLControl.DoGetDescription: string;
begin
  Result := inherited DoGetDescription;
  if Assigned(FControl) and (FControl is TControl) then
  begin
    if not TControl(FControl).Visible then
      Result := Result + ' HID';
    if not TControl(FControl).Enabled then
      Result := Result + ' DIS';
  end;
end;

function TVCLControl.GetFocused: Boolean;
begin
  if FControl is TWinControl then
    Result := TWinControl(FControl).Focused
  else
    Result := False;
end;

function TVCLControl.GetName: string;
begin
  if not Assigned(FControl) then
    Result := 'NULL'
  else if not (FControl is TControl) then
    Result := FControl.ClassName + ': ' + FOwner.Id
  else if TCrackedWinControl(FControl).Caption <> '' then
    Result := FControl.ClassName + ': ' + FOwner.Id + ' (' + TCrackedWinControl(FControl).Caption + ')'
  else
    Result := FControl.ClassName + ': ' + FOwner.Id;
end;

function TVCLControl.IndexOfControl(const AControl: TObject): Integer;
begin
  Result := TMenuItem(AControl).MenuIndex;
end;

procedure TVCLControl.PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer);
var
  vLeft, vTop, vWidth, vHeight: Integer;
begin
  if FControl is TControl then
  begin
    vLeft := IfThen(ALeft < 0, TControl(FControl).Left, ALeft);
    vTop := IfThen(ATop < 0, TControl(FControl).Top, ATop);
    vWidth := IfThen(AWidth < 0, TControl(FControl).Width, AWidth);
    vHeight := IfThen(AHeight < 0, TControl(FControl).Height, AHeight);

    TControl(FControl).SetBounds(vLeft, vTop, vWidth, vHeight);
  end;

  PlaceLabel;
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
    //vLabel.Layout := tlCenter;
    //vLabel.Height := Control.Height;
  end;
  vLabel.Parent := TControl(FControl).Parent;
end;

procedure TVCLControl.SetCaptionProperty(const ALayout: TLayout);
begin
  if not Assigned(FCaption) or not Assigned(ALayout) then
    Exit;

  if ALayout.Kind in [lkMemo, lkPanel] then
  begin
    FShowCaption := ALayout.ShowCaption;
    TLabel(FCaption).Visible := FShowCaption and (FOwner.View.State > vsHidden);
    if akTop in ALayout.Anchors then
      TLabel(FCaption).Anchors := [akLeft, akTop]
    else
      TLabel(FCaption).Anchors := [akLeft, akBottom];

    if ALayout.Caption_AtLeft then
      SetLabelPosition(lpLeft);
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
    TCrackedWinControl(FControl).Tag := Integer(FOwner);
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

procedure TVCLControl.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  PlaceLabel;
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

procedure TVCLControl.SetPopupArea(const APopupArea: TUIArea);
var
  vPopupMenu: TPopupMenu;
begin
  vPopupMenu := TPopupMenu(APopupArea.InnerControl);
  if not Assigned(vPopupMenu.OnPopup) then
    vPopupMenu.OnPopup := BeforeContextMenuShow;
  TCrackedControl(FControl).PopupMenu := vPopupMenu;
end;

procedure TVCLControl.SetViewState(const AValue: TViewState);
var
  vBoolValue: Boolean;
begin
  if FControl is TMenuItem then
  begin
    TMenuItem(FControl).Visible := AValue > vsHidden;
    TMenuItem(FControl).Enabled := AValue > vsDisabled;
  end
  else if (FControl is TControl) and (not FIsForm) then
  begin
    vBoolValue := AValue > vsHidden;

    if TControl(FControl).Visible <> vBoolValue then
      TControl(FControl).Visible := vBoolValue;

    vBoolValue := AValue > vsDisabled;
    if FControl is TcxTabSheet then
      TcxTabSheet(FControl).AllowCloseButton := vBoolValue
    else if TControl(FControl).Enabled <> vBoolValue then
      TControl(FControl).Enabled := vBoolValue;
  end;
end;

procedure TVCLControl.UpdateCaptionVisibility;
begin
  if Assigned(FCaption) then
    TLabel(FCaption).Visible := FShowCaption and (FOwner.View.State > vsHidden);
end;

initialization

RegisterClasses([TLabel, TPanel, TSplitter, TImage, TBevel, TPageControl, TMemo,
  TTabSheet, TToolBar, TToolButton, TBitBtn, TScrollBox, TMainMenu, TPopupMenu,
  TShape, TListView, TImageList]);

TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, '', TTreeViewArea);
TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, 'TreeView', TTreeViewArea);
TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, 'NavBar', TNavBarArea);
TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, 'MainMenu', TMainMenuArea);
TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, 'ToolBar', TToolBarArea);
//TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, 'OneButton', TOneButtonArea);

TPresenter.RegisterUIClass('Windows.DevExpress', uiAction, '', TButtonArea);
TPresenter.RegisterUIClass('Windows.DevExpress', uiAction, 'link', TLinkArea);

end.
