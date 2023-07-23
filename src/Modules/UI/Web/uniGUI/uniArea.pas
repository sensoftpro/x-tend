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

unit uniArea;

interface

uses
  System.Classes, System.Generics.Collections, System.Types,
  Vcl.Forms, System.UITypes, System.SysUtils,

  uniGUIForm, uniGUIRegClasses, uniGUIClasses,
  uniGUIAbstractClasses, uniGUIBaseClasses,
  uniMainMenu, uniButton, uniImageList, uniToolBar, uniTreeView,

  uConsts, uUIBuilder, uDefinition, uEntity, uView, uLayout;

type
  TUniGUIControl = class(TNativeControlHolder)
  private
    procedure BeforeContextMenuShow(Sender: TObject);
    procedure OnControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    function ExtractOwner(const AUIArea: TUIArea): TComponent;
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

  TUniGUIButton = class(TUniGUIControl)
  private
    FTypeSelectionMenu: TUniPopupMenu;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TUniGUILink = class(TUniGUIControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TUniGUIMainMenu = class(TUniGUIControl)
  private
    FMenu: TuniMainMenu;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TUniGUIToolBar = class(TUniGUIControl)
  private
    FToolBar: TuniToolBar;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TUniGUITreeMenu = class(TUniGUIControl)
  private
    FTreeView: TUniTreeView;
    FDefaultWorkArea: string;
    procedure SelectNode(const ANode: TUniTreeNode);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
    procedure OnMouseClick(Sender: TObject);
    procedure Refill;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
    procedure DoExecuteUIAction(const AView: TView); override;
  end;

implementation

uses
  Graphics, Math, StrUtils, Generics.Defaults, Variants,

  //UniGUI.ExtCtrls, UniGUI.Graphics, UniGUI.TabControl, UniGUI.Objects, UniGUI.ListView, UniGUI.ImgList,
  //UniGUI.ScrollBox, UniGUI.Memo, UniGUI.Edit,

  Windows, Vcl.Controls, uniGUITypes, uniBitBtn, uniMenuButton, uniLabel, uniPageControl, uniEdit,
  uniGUIApplication, UniGUIJSUtils,

  uPresenter, uUniGUIPresenter, uConfiguration, uSession, uInteractor, uUtils, uCollection,
  uEntityList, uDomainUtils, uChangeManager;

type
  TCrackedBitBtn = class(TUniCustomBitBtn) end;
  TCrackedUniControl = class(TUniControl) end;
  TCrackedUniForm = class(TUniForm) end;

{ TUniGUIMainMenu }

function TUniGUIMainMenu.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FMenu := TUniMainMenu.Create(ExtractOwner(AParent));
  FMenu.Images := TUniCustomImageList(FUIBuilder.Images[16]);
  Result := FMenu;
end;

function TUniGUIMainMenu.DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vControl: TObject;
  vMenuItem: TUniMenuItem;
begin
  vMenuItem := TUniMenuItem.Create(FMenu);
  vMenuItem.Caption := ACaption;
  vMenuItem.Hint := AHint;
  vMenuItem.ImageIndex := AImageIndex;
  vMenuItem.OnClick := FOwner.OnAreaClick;

  Result := nil;
  if ANavItem.Level = 0 then
  begin
    FMenu.Items.Add(vMenuItem);
    Result := vMenuItem;
  end
  else begin
    vControl := GetRealControl(AParent);
    if vControl is TUniMenuItem then
    begin
      TUniMenuItem(vControl).Add(vMenuItem);
      Result := vMenuItem;
    end;
  end;
end;

{ TUniGUIToolBar }

function TUniGUIToolBar.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vImageSize: Integer;
begin
  FToolBar := TUniToolBar.Create(TControl(GetRealControl(AParent)));
  FToolBar.ButtonAutoWidth := True;
  FToolBar.ButtonHeight := ALayout.Height;
  FToolBar.ShowCaptions := True;

  vImageSize := StrToIntDef(GetUrlParam(FParams, 'ImageSize'), 16);
  FToolBar.Images := TUniCustomImageList(FUIBuilder.Images[vImageSize]);

  Result := FToolBar;
end;

function TUniGUIToolBar.DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vMenu: TUniPopupMenu;
  vMenuItem: TUniMenuItem;
  vLeft: Integer;
  vToolButton: TUniToolButton;
  vControl: TObject;
begin
  if ANavItem.Level = 0 then
  begin
    if FToolBar.Buttons.Count > 0 then
    begin
      vToolButton := TUniToolButton(FToolBar.Buttons.Last);
      vLeft := vToolButton.Left + vToolButton.Width + 10;
    end
    else
      vLeft := 0;

    vToolButton := TUniToolButton.Create(FToolBar);
    vToolButton.Left := vLeft;
    vToolButton.Caption := ACaption;
    vToolButton.Hint := AHint;
    vToolButton.ImageIndex := AImageIndex;
    vToolButton.IconAlign := iaTop;
    vToolButton.OnClick := FOwner.OnAreaClick;

    Result := vToolButton;
  end
  else begin
    vControl := GetRealControl(AParent);

    vMenuItem := TUniMenuItem.Create(FToolBar);
    vMenuItem.Caption := ACaption;
    vMenuItem.Hint := AHint;
    vMenuItem.ImageIndex := AImageIndex;
    vMenuItem.OnClick := FOwner.OnAreaClick;

    if vControl is TUniToolButton then
    begin
      vToolButton := TUniToolButton(vControl);
      vToolButton.Style := tbsDropDown;
      if Assigned(vToolButton.DropdownMenu) then
        vMenu := vToolButton.DropdownMenu
      else begin
        vMenu := TUniPopupMenu.Create(FToolBar);
        vToolButton.DropdownMenu := vMenu;
        vToolButton.DropdownMenu.Images := FToolBar.Images;
      end;

      vMenu.Items.Add(vMenuItem);
    end
    else if vControl is TUniMenuItem then
      TUniMenuItem(vControl).Add(vMenuItem);

    Result := vMenuItem;
  end;
end;

{ TUniGUITreeMenu }

function TUniGUITreeMenu.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FTreeView := TUniTreeView.Create(ExtractOwner(AParent));
  FTreeView.Images := TUniCustomImageList(FUIBuilder.Images[16]);
  FTreeView.ShowIcons := False;
  FTreeView.ReadOnly := True;
  FTreeView.OnClick := OnMouseClick;
  FTreeView.OnKeyPress := OnKeyPress;

  UniSession.SetStyle(
    '#' + FTreeView.JSName + '_id .x-tree-view{'+
    '  background-color: ' + uniColor2Web(AlphaColorToColor(ALayout.Color)) + ';'+
    '}'+
    '#' + FTreeView.JSName + '_id .x-tree-view .x-grid-cell-inner-treecolumn{'+
    '  background-color: ' + uniColor2Web(AlphaColorToColor(ALayout.Color)) + ';'+
    '}'+
    '#' + FTreeView.JSName + '_id .x-tree-view .x-grid-cell-inner-treecolumn{'+
    '  color: ' + uniColor2Web(AlphaColorToColor(ALayout.Font.Color)) + ';'+
    '}');

  ALayout.Id := 'TreeView';

  FDefaultWorkArea := FCreateParams.Values['ContentWorkArea'];
  if FDefaultWorkArea = '' then
    FDefaultWorkArea := 'WorkArea';
  Result := FTreeView;

  FTreeView.FullExpand;
end;

function TUniGUITreeMenu.DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vParentNode: TUniTreeNode;
  vTreeNode: TUniTreeNode;
begin
  if ANavItem.IsLine then
    Exit(nil);

  if ANavItem.Level > 0 then
    vParentNode := TUniTreeNode(GetRealControl(AParent))
  else
    vParentNode := nil;

  vTreeNode := FTreeView.Items.AddChild(vParentNode, ACaption);
  vTreeNode.Font.Size := AParent.Layout.Font.Size;
  vTreeNode.ImageIndex := AImageIndex;
  vTreeNode.SelectedIndex := -1;

  Result := vTreeNode;

  if Assigned(vParentNode) then
    vParentNode.Expand(True);
end;

procedure TUniGUITreeMenu.DoExecuteUIAction(const AView: TView);
begin
  if AView.Name = '#Refill' then
    Refill;
end;

procedure TUniGUITreeMenu.OnKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and Assigned(FTreeView.Selected) then
    SelectNode(FTreeView.Selected);
end;

procedure TUniGUITreeMenu.OnMouseClick(Sender: TObject);
var
  vNode: TUniTreeNode;
begin
  vNode := FTreeView.Selected;
  if Assigned(vNode) then
    SelectNode(vNode);
end;

procedure TUniGUITreeMenu.Refill;
begin
  FOwner.Clear;
  FTreeView.Items.Clear;
  FOwner.ProcessChilds;
end;

procedure TUniGUITreeMenu.SelectNode(const ANode: TUniTreeNode);
var
  vArea: TUIArea;
begin
  if not Assigned(ANode) then
    Exit;

  vArea := TUIArea(ANode.Tag);
  if Assigned(vArea) then
    FOwner.ProcessAreaClick(vArea);
end;

{ TUniGUIButton }

procedure TUniGUIButton.DoBeforeFreeControl;
begin
  //FreeAndNil(FTypeSelectionMenu);
end;

function TUniGUIButton.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vParams: TStrings;
  vButton: TCrackedBitBtn;
  vActionDef: TDefinition;
  vDefinitions: TList<TDefinition>;
  i: Integer;
  vIsSelectable: Boolean;
  vHasPopupMenu: Boolean;
  vMenuItem: TUniMenuItem;
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

  vIsSelectable := (vActionDef.Name = 'Add') and Assigned(FView.ParentDomainObject)
    and (FView.ParentDomainObject is TEntityList);

  if vIsSelectable then
  begin
    vDefinitions := TEntityList(FView.ParentDomainObject).ContentDefinitions;
    vHasPopupMenu := vIsSelectable and (vDefinitions.Count > 1);
  end
  else
    vHasPopupMenu := False;

  if vHasPopupMenu then
    vButton := TCrackedBitBtn(TUniMenuButton.Create(ExtractOwner(AParent)))
  else
    vButton := TCrackedBitBtn(TUniBitBtn.Create(ExtractOwner(AParent)));

  vButton.Images := TUniCustomImageList(FUIBuilder.Images[vImageSize]);
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
      begin
        vButton.IconAlign := TUniIconAlign.iaLeft;
        vButton.ImageIndex := vImageIndex;
        vButton.Caption := vCaption;
      end
      else begin
        vButton.IconAlign := TUniIconAlign.iaCenter;
        vButton.ImageIndex := vImageIndex;
      end;
    end
    else if vComposition = 'TextOnly' then
      vButton.Caption := vCaption
    else if vComposition = 'ImageOnly' then
    begin
      vButton.IconAlign := TUniIconAlign.iaCenter;
      vButton.ImageIndex := vImageIndex;
    end
    else begin
      vButton.Caption := vCaption;
      vButton.ImageIndex := vImageIndex;
      if vComposition = 'ImageRight' then
        vButton.IconAlign := TUniIconAlign.iaRight
      else if vComposition = 'ImageTop' then
        vButton.IconAlign := TUniIconAlign.iaTop
      else if vComposition = 'ImageBottom' then
        vButton.IconAlign := TUniIconAlign.iaBottom
      else
        vButton.IconAlign := TUniIconAlign.iaLeft;
    end;
  end
  else begin
    vButton.Caption := vCaption;
  end;

  if vIsSelectable  then
  begin
    if vHasPopupMenu then
    begin
      FTypeSelectionMenu := TUniPopupMenu.Create(ExtractOwner(AParent));
      FTypeSelectionMenu.Images := TUniCustomImageList(FUIBuilder.Images[16]);
      vDefinitions := TEntityList(FView.ParentDomainObject).ContentDefinitions;
      for i := 0 to vDefinitions.Count - 1 do
      begin
        vDefinition := TDefinition(vDefinitions[i]);
        vMenuItem := TUniMenuItem.Create(FTypeSelectionMenu);
        vMenuItem.Caption := GetTranslation(vDefinition);
        if Length(vOverriddenCaption) > 0 then
          vMenuItem.Caption := vOverriddenCaption;
        vMenuItem.ImageIndex := GetImageIndex(vDefinition._ImageID);
        vMenuItem.Tag := NativeInt(FOwner);
        vMenuItem.OnClick := FOwner.OnActionMenuSelected;
        FTypeSelectionMenu.Items.Add(vMenuItem);
      end;
      TUniMenuButton(vButton).DropDownMenu := FTypeSelectionMenu;
      TUniMenuButton(vButton).IconPosition := TUniIconPosition.ipButtonEdge;
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

procedure TUniGUIButton.RefillArea(const AKind: Word);
var
  vButton: TCrackedBitBtn;
  vActionDef: TDefinition;
  vImageID: Integer;
begin
  if AKind <> dckContentTypeChanged then
  begin
    inherited RefillArea(AKind);
    Exit;
  end;

  vButton := TCrackedBitBtn(FControl);

  vActionDef := TDefinition(FView.Definition);
  vImageID := GetImageIndex(vActionDef._ImageID);

  if (vButton.Images.Count + 1 >= vImageID) and (vImageID > 0) then
    vButton.ImageIndex := vImageID;

  vButton.Text := GetTranslation(vActionDef);
  vButton.Hint := vButton.Text;
end;

{ TUniGUILink }

function TUniGUILink.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vParams: TStrings;
  vLabel: TUniLabel;
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

  vLabel := TUniLabel.Create(ExtractOwner(AParent));
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

procedure TUniGUILink.RefillArea(const AKind: Word);
var
  vLabel: TUniLabel;
  vActionDef: TDefinition;
begin
  if AKind <> dckContentTypeChanged then
  begin
    inherited RefillArea(AKind);
    Exit;
  end;

  vLabel := TUniLabel(FControl);

  vActionDef := TDefinition(FView.Definition);
  vLabel.Text := GetTranslation(vActionDef);
  vLabel.Hint := vLabel.Text;
end;

{ TUniGUIControl }

procedure TUniGUIControl.AssignFromLayout(const ALayout: TLayout; const AParams: string);
var
  vForm: TUniForm;
  vWS: Integer;
begin
  if FIsForm then
  begin
    vForm := TUniForm(FControl);

    if ALayout.Kind = lkFrame then
    begin
      if (ALayout.Tag and cFormResizable) > 0 then
      begin
        vForm.BorderStyle := TFormBorderStyle.bsSizeable;
        vForm.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
      end;

      if (ALayout.Tag and cFormDisableMinimizeButton) > 0 then
        vForm.BorderIcons := vForm.BorderIcons - [TBorderIcon.biMinimize];

      if (ALayout.Tag and cFormDisableMaximizeButton) > 0 then
        vForm.BorderIcons := vForm.BorderIcons - [TBorderIcon.biMaximize];

      if (ALayout.Tag and cFormPositionDesign) > 0 then
        vForm.Position := poDesigned;

      if (ALayout.Tag and cFormNotResizable) > 0 then
        vForm.BorderStyle := TFormBorderStyle.bsSingle;

      vWS := StrToIntDef(GetUrlParam(AParams, 'WindowState', ''), -1);
      if vWS > -1 then
        vForm.WindowState := TWindowState(vWS);

      if (vForm.WindowState = TWindowState.wsNormal) then
      begin
        //if {(vForm.FormStyle <> fsMDIChild) or} ((ALayout.Tag and cFormUseDesignSizes) > 0) then
        //begin
          vForm.Width := vForm.Width - vForm.ClientWidth + ALayout.Width;
          if FOwner.View.DefinitionKind = dkDomain then
            vForm.Height := vForm.Height - vForm.ClientHeight + ALayout.Height
          else
            vForm.Height := vForm.Height - vForm.ClientHeight + ALayout.Height + cServiceAreaHeight;
        //end;
      end;

      CopyConstraints(vForm.Constraints, ALayout);

      if ALayout.Caption <> '' then
        vForm.Caption := ALayout.Caption;
      vForm.Color := AlphaColorToColor(ALayout.Color);
    end
    else if ALayout.Kind = lkPanel then
    begin
      vForm.BorderStyle := bsSizeable;
      vForm.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
      vForm.Caption := ALayout.Caption;
      vForm.Width := vForm.Width - vForm.ClientWidth + ALayout.Width;
      if FOwner.View.DefinitionKind = dkDomain then
        vForm.Height := vForm.Height - vForm.ClientHeight + ALayout.Height
      else
        vForm.Height := vForm.Height - vForm.ClientHeight + ALayout.Height + cServiceAreaHeight;
    end
    else if ALayout.Kind = lkPage then
    begin
      vForm.BorderStyle := bsSizeable;
      vForm.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
      vForm.Caption := ALayout.Caption;
    end
    else
      Assert(False, 'Непонятно какой контрол в лэйауте');
  end
  else if ALayout.Kind = lkFrame then
  begin
    //vFrame.SetBounds(Control.Left, Control.Top, Control.Width, Control.Height);
    if (ALayout.Caption <> '') and (FControl is TUniTabSheet) then
    begin
      TUniTabSheet(FControl).Caption := ALayout.Caption;
      if Pos('=', ALayout.Caption) > 0 then // Hint содержит url-строку с параметрами
      begin
        TUniTabSheet(FControl).Caption := GetUrlParam(ALayout.Caption, 'Caption', '');
        TUniTabSheet(FControl).ImageIndex := FOwner.GetImageIndex(GetUrlParam(ALayout.Caption, 'ImageIndex', ''));
      end;
    end;
  end
  else if (ALayout.Kind in [lkPanel, lkMemo, lkShape]) and (FControl is TUniControl) then
  begin
    SetBounds(Rect(ALayout.Left, ALayout.Top,
      ALayout.Left + ALayout.Width, ALayout.Top + ALayout.Height));
    SetCaptionProperty(ALayout);

    CopyFontSettings(TCrackedUniControl(FControl).Font, ALayout);

    TUniControl(FControl).Anchors := ALayout.Anchors;
    TUniControl(FControl).Align := TAlign(ALayout.Align);
    CopyMargins(TUniControl(FControl), ALayout);
    CopyPadding(TUniControl(FControl), ALayout);

    SetAlignment(ALayout.Alignment);

    if ALayout.Kind <> lkShape then
    begin
      TCrackedUniControl(FControl).Color := AlphaColorToColor(ALayout.Color);
      TCrackedUniControl(FControl).ParentColor := False;
      TCrackedUniControl(FControl).ParentBackground := False;
    end;
  end;
end;

procedure TUniGUIControl.BeforeContextMenuShow(Sender: TObject);
var
  vMenu: TUniPopupMenu;
  i: Integer;

  procedure CheckMenuItems(const AMenuItem: TUniMenuItem);
  var
    i: Integer;
    vArea: TUIArea;
    vOnCheck: TNotifyEvent;
  begin
    if AMenuItem.Count > 0 then
    begin
      for i := 0 to AMenuItem.Count - 1 do
        CheckMenuItems(AMenuItem[i]);
      Exit;
    end;

    if not AMenuItem.CheckItem then
      Exit;

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
    begin
      if AMenuItem.Checked <> TEntity(vArea.View.DomainObject)['IsChecked'] then
      begin
        vOnCheck := AMenuItem.OnCheck;
        AMenuItem.OnCheck := nil;
        try
          AMenuItem.Checked := TEntity(vArea.View.DomainObject)['IsChecked'];
        finally
          AMenuItem.OnCheck := vOnCheck;
        end;
      end;
    end;
  end;

begin
  vMenu := TUniPopupMenu(Sender);
  for i := 0 to vMenu.Items.Count - 1 do
    CheckMenuItems(vMenu.Items[i]);
end;

constructor TUniGUIControl.Create(const AOwner: TUIArea; const AParams: string);
begin
  inherited Create(AOwner, AParams);
end;

destructor TUniGUIControl.Destroy;
begin
  if Assigned(FCaption) then
    TUniLabel(FCaption).Parent := nil;

  inherited Destroy;
end;

procedure TUniGUIControl.DoActivate(const AUrlParams: string);
var
  vForm: TUniForm;
  vChangeTab: Boolean;
begin
  if FControl is TUniForm then
  begin
    vForm := TUniForm(FControl);
    vForm.BringToFront;
  end
  else if FControl is TUniTabSheet then
  begin
    vChangeTab := SameText(GetUrlParam(AUrlParams, 'TabActivationOption', ''), 'ChangeTab');
    if Assigned(Parent.CreateParams) and (Parent.CreateParams.IndexOfName('TabActivationOption') >= 0) and
      (not SameText(Parent.CreateParams.Values['TabActivationOption'], 'ChangeTab')) then
      vChangeTab := False;
    if vChangeTab and Assigned(TUniTabSheet(FControl).PageControl) then
      TUniTabSheet(FControl).PageControl.ActivePage := TUniTabSheet(FControl);
  end;
end;

procedure TUniGUIControl.DoBeginUpdate;
begin
  if Assigned(FControl) and (TControl(FControl).Owner is TUniForm) then
    TUniForm(TControl(FControl).Owner).SuspendLayouts;
end;

procedure TUniGUIControl.DoClose(const AModalResult: Integer);
var
  vForm: TUniForm;
begin
  if FIsForm then
  begin
    vForm := TUniForm(FControl);

    if AModalResult = mrNone then
      vForm.Close
    else
      vForm.ModalResult := AModalResult;
  end;
end;

function TUniGUIControl.DoCreateCaption(const AParent: TUIArea; const ACaption, AHint: string): TObject;
var
  vLabel: TUniLabel;
  vFontSize: Integer;
begin
  vLabel := TUniLabel.Create(ExtractOwner(AParent));
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
    vLabel.Font.Color := TColorRec.Gray;
  end;
end;

procedure TUniGUIControl.DoEndUpdate;
begin
  if Assigned(FControl) and (TControl(FControl).Owner is TUniForm) then
    TUniForm(TControl(FControl).Owner).ResumeLayouts;
end;

function TUniGUIControl.ExtractOwner(const AUIArea: TUIArea): TComponent;
begin
  Result := TComponent(GetRealControl(AUIArea));
  if Assigned(Result) and not (Result is TUniForm) then
    Result := Result.Owner;
end;

function TUniGUIControl.GetActiveChildArea: TUIArea;
begin
  if FControl is TUniPageControl then
    Result := AreaFromSender(TUniPageControl(FControl).ActivePage)
  else
    Result := nil;
end;

function TUniGUIControl.GetBounds: TRect;
begin
  Result := TUniControl(FControl).BoundsRect;
end;

function TUniGUIControl.GetClientRect: TRect;
begin
  if not (FControl is TForm) then
    Result := TUniControl(FControl).BoundsRect
  else if True {TForm(FControl).FormStyle = fsMDIForm} then
    Result := TUniControl(FControl).ClientRect
  else
    Result := TUniControl(FControl).ClientRect;
end;

function TUniGUIControl.GetFocused: Boolean;
begin
  if FControl is TWinControl then
    Result := TWinControl(FControl).Focused
  else
    Result := False;
end;

function TUniGUIControl.GetModalResult: TModalResult;
begin
  if FControl is TUniForm then
    Result := TUniForm(FControl).ModalResult
  else
    Result := mrNone;
end;

function TUniGUIControl.GetTabOrder: Integer;
begin
  if (FControl is TWinControl) and TWinControl(FControl).TabStop then
    Result := TWinControl(FControl).TabOrder
  else
    Result := -1;
end;

function TUniGUIControl.GetViewState: TViewState;
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

function TUniGUIControl.GetWindowState: TWindowState;
begin
  if FControl is TForm then
    Result := TForm(FControl).WindowState
  else
    Result := inherited GetWindowState;
end;

function TUniGUIControl.IndexOfSender(const ASender: TObject): Integer;
begin
  Result := TUniMenuItem(ASender).MenuIndex;
end;

procedure TUniGUIControl.OnControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vMenu: TObject;
  vClientPos: TPoint;
  vParent: TObject;
begin
  if (Button = mbRight) and Assigned(TCrackedUniControl(Sender).PopupMenu) then
  begin
    vMenu := TCrackedUniControl(Sender).PopupMenu;

    BeforeContextMenuShow(vMenu);

    if vMenu is TUniPopupMenu then
    begin
      vClientPos := Point(X, Y);
      vParent := Sender;
      while Assigned(vParent) and not (vParent is TUniForm) do
      begin
        vClientPos := TCrackedUniControl(vParent).ClientToParent(vClientPos);
        vParent := TCrackedUniControl(vParent).Parent;
      end;

      TUniPopupMenu(vMenu).Popup(vClientPos.X, vClientPos.Y);
    end;
  end;
end;

procedure TUniGUIControl.PlaceLabel;
var
  vSpace: Integer;
  vLabel: TUniLabel;
begin
  if FCaption = nil then
    Exit;

  vLabel := TUniLabel(FCaption);
  if FLabelPosition = lpTop then
  begin
    vLabel.Left := TControl(FControl).Left;
    vLabel.Top := TControl(FControl).Top - vLabel.Height - 2;
    vLabel.AutoSize := True;
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

procedure TUniGUIControl.SetActiveChildArea(const AArea: TUIArea);
begin
  //
end;

procedure TUniGUIControl.SetAlignment(const AAlignment: TAlignment);
begin
  if FControl is TUniControl then
    TCrackedUniControl(FControl).Alignment := AAlignment;
end;

procedure TUniGUIControl.SetBounds(const Value: TRect);
begin
  if FControl is TUniControl then
    TUniControl(FControl).SetBounds(Value.Left, Value.Top, Value.Width, Value.Height);

  PlaceLabel;
end;

procedure TUniGUIControl.SetCaptionProperty(const ALayout: TLayout);
begin
  if not Assigned(FCaption) then
    Exit;

  if ALayout.Kind in [lkMemo, lkPanel, lkShape] then
  begin
    FShowCaption := ALayout.ShowCaption;
    TUniLabel(FCaption).Visible := FShowCaption and (FOwner.View.State > vsHidden);
    if akBottom in ALayout.Anchors then
      TUniLabel(FCaption).Anchors := [akLeft, akBottom]
    else
      TUniLabel(FCaption).Anchors := [akLeft, akTop];

    if ALayout.Caption_AtLeft then
    begin
      FLabelPosition := lpLeft;
      PlaceLabel;
    end;
  end;
end;

procedure TUniGUIControl.SetControl(const AControl: TObject);
begin
  if FControl is TUniTreeNode then
    TUniTreeNode(FControl).Tag := 0
  else if FControl is TUniControl then
  begin
    TCrackedUniControl(FControl).OnEnter := nil;
    TCrackedUniControl(FControl).OnExit := nil;
  end
  else if FControl is TUniForm then
  begin
    TCrackedUniForm(FControl).OnEnter := nil;
    TCrackedUniForm(FControl).OnExit := nil;
  end;

  inherited SetControl(AControl);

  if FControl is TUniTreeNode then
    TUniTreeNode(FControl).Tag := NativeInt(FOwner)
  else if FControl is TUniControl then
  begin
    TCrackedUniControl(FControl).OnEnter := FOwner.OnEnter;
    TCrackedUniControl(FControl).OnExit := FOwner.OnExit;
  end
  else if FControl is TUniForm then
  begin
    TCrackedUniForm(FControl).OnEnter := FOwner.OnEnter;
    TCrackedUniForm(FControl).OnExit := FOwner.OnExit;
  end;
end;

procedure TUniGUIControl.SetFocused(const Value: Boolean);
begin
  if FControl is TUniControl then
  begin
    if Value and TUniControl(FControl).CanFocus then
      TUniControl(FControl).SetFocus;
  end
  else if FControl is TUniForm then
  begin
    if Value and TUniForm(FControl).CanFocus then
      TUniForm(FControl).SetFocus;
  end;
end;

procedure TUniGUIControl.SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl);
var
  vPopupMenu: TUniPopupMenu;
begin
  if not Assigned(FControl) then
    Exit;

  if SameText(ATargetName, 'popup') then
  begin
    vPopupMenu := TUniPopupMenu(TNativeControlHolder(ALinkedControl).Control);
    TCrackedUniControl(FControl).PopupMenu := vPopupMenu;
    if not Assigned(TCrackedUniControl(FControl).OnMouseDown) then
      TCrackedUniControl(FControl).OnMouseDown := OnControlMouseDown;
  end;
end;

procedure TUniGUIControl.SetModalResult(const AModalResult: TModalResult);
begin
  if FControl is TUniForm then
    TUniForm(FControl).ModalResult := AModalResult;
end;

procedure TUniGUIControl.SetParent(const AParent: TUIArea);
begin
  inherited SetParent(AParent);

  if not (FControl is TUniControl) then
    Exit;

  // Установка родителя для контрола
  if FIsForm or not Assigned(AParent) then
    TUniControl(FControl).Parent := nil
  else if not Assigned(TControl(FControl).Parent) then
    TUniControl(FControl).Parent := TWinControl(GetRealControl(AParent));
end;

procedure TUniGUIControl.SetTabOrder(const ATabOrder: Integer);
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

procedure TUniGUIControl.SetViewState(const AViewState: TViewState);
begin
  if FIsForm then
    Exit;

  if FControl is TUniControl then
  begin
    TUniControl(FControl).Visible := AViewState > vsHidden;
    TUniControl(FControl).Enabled := AViewState > vsDisabled;
  end
  else if FControl is TUniMenuItem then
  begin
    TUniMenuItem(FControl).Visible := AViewState > vsHidden;
    TUniMenuItem(FControl).Enabled := AViewState > vsDisabled;
  end
  else if FControl is TUniTreeNode then
  begin
    TUniTreeNode(FControl).Visible := AViewState > vsHidden;
    TUniTreeNode(FControl).Enabled := AViewState > vsDisabled;
  end
  else begin
    Assert(False, FControl.ClassName);
  end;
end;

procedure TUniGUIControl.SetWindowState(const AWindowState: TWindowState);
begin
  if FControl is TUniForm then
    TUniForm(FControl).WindowState := AWindowState;
end;

procedure TUniGUIControl.UpdateCaptionVisibility;
begin
  if Assigned(FCaption) then
    TUniLabel(FCaption).Visible := FShowCaption and (FOwner.View.State > vsHidden);
end;

initialization

TPresenter.RegisterControlClass('Web.UniGUI', uiNavigation, '', TUniGUITreeMenu);
TPresenter.RegisterControlClass('Web.UniGUI', uiNavigation, 'TreeView', TUniGUITreeMenu);
TPresenter.RegisterControlClass('Web.UniGUI', uiNavigation, 'MainMenu', TUniGUIMainMenu);
TPresenter.RegisterControlClass('Web.UniGUI', uiNavigation, 'ToolBar', TUniGUIToolBar);

TPresenter.RegisterControlClass('Web.UniGUI', uiAction, '', TUniGUIButton);
TPresenter.RegisterControlClass('Web.UniGUI', uiAction, 'link', TUniGUILink);

end.
