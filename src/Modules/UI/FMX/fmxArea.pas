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

unit fmxArea;

interface

uses
  System.Classes, System.Generics.Collections, System.Types, System.UITypes, System.SysUtils,

  FMX.StdCtrls, FMX.Controls, FMX.Types, FMX.Layouts, FMX.Menus, FMX.Forms,
  FMX.Controls.Presentation, FMX.TreeView, FMX.ListBox,

  uConsts, uUIBuilder, uDefinition, uEntity, uView, uLayout;

type
  TFMXControl = class(TNativeControlHolder)
  private
    procedure BeforeContextMenuShow(Sender: TObject);
  protected
    function IndexOfSender(const ASender: TObject): Integer; override;

    procedure DoActivate(const AUrlParams: string); override;
    procedure DoClose(const AModalResult: Integer); override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    procedure DoRegisterChanges(const Value: Boolean); override;

    procedure AssignFromLayout(const ALayout: TLayout; const AParams: string); override;

    procedure SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl); override;
    procedure SetControl(const AControl: TObject); override;
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

  TFMXButton = class(TFMXControl)
  private
    FTypeSelectionMenu: TPopupMenu;
    procedure ShowPopup(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TFMXLink = class(TFMXControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TFMXMainMenuNavigation = class(TFMXControl)
  private
    FMenu: TMainMenu;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TFMXToolBarNavigation = class(TFMXControl)
  private
    FToolBar: TToolBar;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TFMXTreeViewNavigation = class(TFMXControl)
  private
    FTreeView: TTreeView;
    FDefaultWorkArea: string;
    procedure SelectNode(const ANode: TTreeViewItem);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Refill;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
    procedure DoExecuteUIAction(const AView: TView); override;
  end;

  TFMXNavBarNavigation = class(TFMXControl)
  private
    FNavBar: TPanel;
    FNavBarGroup: TExpander;
    FNavBarItem: TButton;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TFMXOneButtonNavigation = class(TFMXControl)
  private
    FButton: TButton;
    FPopupButton: TButton;
    FMenu: TPopupMenu;
    FItem: TMenuItem;
    procedure OnClick(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject;
      override;
  end;

  TFMXForm = class(TForm)
  end;

  TNoBorderForm = class(TForm)
  end;

  TSingleBorderForm = class(TForm)
  end;

function AlignToAlignLayout(const AAlign: Byte): TAlignLayout; overload;
function AlignToAlignLayout(const AAlign: TLayoutAlign): TAlignLayout; overload;
function DecimalDigitsFromFieldFormat(const AFormat: string): Integer;

implementation

{$R FMXForm.fmx}
{$R NoBorderForm.fmx}
{$R SingleBorderForm.fmx}

uses
  Math, StrUtils, Generics.Defaults, Variants,

  FMX.ExtCtrls, FMX.Graphics, FMX.TabControl, FMX.Objects, FMX.ListView, FMX.ImgList,
  FMX.ScrollBox, FMX.Memo, FMX.Edit,

  uPresenter, uFMXPresenter, uConfiguration, uSession, uInteractor, uUtils, uCollection,
  uEntityList, uDomainUtils, uChangeManager;

const
  fsUnderline = System.UITypes.TFontStyle.fsUnderline;
  clBtnFace = TColors.SysBtnFace;

function DecimalDigitsFromFieldFormat(const AFormat: string): Integer;
var
  vLength: integer;
  vDotPos: integer;
begin
  Result := 0;
  vLength := Length(AFormat);
  vDotPos := LastDelimiter('.', AFormat);
  if (vLength > 0) and (vDotPos > 0) then
    Result := Length(AFormat) - LastDelimiter('.', AFormat);
end;

function AlignToAlignLayout(const AAlign: Byte): TAlignLayout;
begin
  Case AAlign of
    1: Result := TAlignLayout.Top;
    2: Result := TAlignLayout.Bottom;
    3: Result := TAlignLayout.Left;
    4: Result := TAlignLayout.Right;
    5: Result := TAlignLayout.Client;
    6: Result := TAlignLayout.None;
  else
    Result := TAlignLayout(AAlign);
  end;
end;

function AlignToAlignLayout(const AAlign: TLayoutAlign): TAlignLayout;
begin
  Result := AlignToAlignLayout(Byte(AAlign));
end;

{ TFMXMainMenuNavigation }

function TFMXMainMenuNavigation.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vParentObject: TFmxObject;
begin
  vParentObject := TFmxObject(GetRealControl(AParent));
  FMenu := TMainMenu.Create(vParentObject);
  FMenu.Parent := vParentObject;
  FMenu.Images := TImageList(FUIBuilder.Images[16]);
  Result := FMenu;
end;

function TFMXMainMenuNavigation.DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vControl: TObject;
  vMenuItem: TMenuItem;
begin
  vMenuItem := TMenuItem.Create(nil);
  vMenuItem.Text := ACaption;
  vMenuItem.Hint := AHint;
  vMenuItem.ImageIndex := AImageIndex;
  vMenuItem.OnClick := FOwner.OnAreaClick;

  Result := nil;
  if ANavItem.Level = 0 then
  begin
    FMenu.AddObject(vMenuItem);
    Result := vMenuItem;
  end
  else begin
    vControl := GetRealControl(AParent);
    if vControl is TMenuItem then
    begin
      TMenuItem(vControl).AddObject(vMenuItem);
      Result := vMenuItem;
    end;
  end;
end;

{ TFMXToolBarNavigation }

function TFMXToolBarNavigation.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FToolBar := TToolBar.Create(TControl(GetRealControl(AParent)));
  Result := FToolBar;
end;

function TFMXToolBarNavigation.DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vButton: TButton;
  vLeft: Single;
  vImageSize: Integer;
begin
  if ANavItem.Level = 0 then
  begin
    if FToolBar.ChildrenCount > 0 then
    begin
      vButton := TButton(FToolBar.Children[FToolBar.ChildrenCount - 1]);
      vLeft := vButton.Position.X + vButton.Width + 10;
    end
    else
      vLeft := 0;

    vImageSize := StrToIntDef(GetUrlParam(FParams, 'ImageSize'), 16);

    vButton := TButton.Create(FToolBar);
    vButton.Position.X := vLeft;
    vButton.Text := ACaption;
    vButton.Hint := AHint;
    vButton.Images := TImageList(FUIBuilder.Images[vImageSize]);
    vButton.ImageIndex := AImageIndex;
    vButton.OnClick := FOwner.OnAreaClick;

    Result := vButton;
  end
  else
    Result := nil;
end;

{ TFMXTreeViewNavigation }

function TFMXTreeViewNavigation.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FTreeView := TTreeView.Create(nil);
  //FTreeView.Images := TImageList(FUIBuilder.Images[16]);
  FTreeView.Locked := True;
  FTreeView.OnMouseDown := OnMouseDown;
  ALayout.Id := 'TreeView';

  FDefaultWorkArea := FCreateParams.Values['ContentWorkArea'];
  if FDefaultWorkArea = '' then
    FDefaultWorkArea := 'WorkArea';
  Result := FTreeView;
end;

function TFMXTreeViewNavigation.DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vParentNode: TTreeViewItem;
  vTreeNode: TTreeViewItem;
begin
  if ANavItem.IsLine then
    Exit(nil);

  if ANavItem.Level > 0 then
    vParentNode := TTreeViewItem(GetRealControl(AParent))
  else
    vParentNode := nil;

  vTreeNode := TTreeViewItem.Create(nil);
  vTreeNode.ImageIndex := AImageIndex;
  vTreeNode.Text := ACaption;
  if ANavItem.Level > 0 then
  begin
    vParentNode := TTreeViewItem(GetRealControl(AParent));
    vParentNode.AddObject(vTreeNode);
  end
  else
    FTreeView.AddObject(vTreeNode);

  Result := vTreeNode;

  if Assigned(vParentNode) then
    vParentNode.Expand;
end;

procedure TFMXTreeViewNavigation.DoExecuteUIAction(const AView: TView);
begin
  if AView.Name = '#Refill' then
    Refill;
end;

procedure TFMXTreeViewNavigation.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  vNode: TTreeViewItem;
begin
  vNode := FTreeView.ItemByPoint(X, Y);
  if Assigned(vNode) then
    SelectNode(vNode);
end;

procedure TFMXTreeViewNavigation.Refill;
begin
  FOwner.Clear;
  FTreeView.Clear;
  FOwner.ProcessChilds;
end;

procedure TFMXTreeViewNavigation.SelectNode(const ANode: TTreeViewItem);
var
  vArea: TUIArea;
begin
  if not Assigned(ANode) then
    Exit;

  vArea := TUIArea(ANode.Tag);
  if Assigned(vArea) then
    FOwner.ProcessAreaClick(vArea);
end;

{ TFMXButton }

procedure TFMXButton.DoBeforeFreeControl;
begin
  FreeAndNil(FTypeSelectionMenu);
end;

function TFMXButton.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
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
  vButton.Images := TImageList(FUIBuilder.Images[vImageSize]);
  vImageIndex := GetImageIndex(vImageID);

  vCaption := GetTranslation(vActionDef);
  vButton.Hint := vCaption;
  if Length(vOverriddenCaption) > 0 then
    vCaption := vOverriddenCaption;
  if Length(vOverriddenHint) > 0 then
    vButton.Hint := vOverriddenHint;

  vButton.ImageIndex := -1;
  vButton.Text := '';
  if (vButton.Images.Count + 1 >= vImageIndex) and (vImageIndex > 0) then
  begin
    if vComposition = '' then
    begin
      if ALayout.Button_ShowCaption then
      begin
        vButton.Text := vCaption;
        vButton.ImageIndex := vImageIndex;
      end
      else begin
        vButton.ImageIndex := vImageIndex;
        vButton.StyleLookup := 'stepperbuttonleft';
        ALayout.Padding.Bottom := ALayout.Height - vImageSize - 4;
      end;
    end
    else if vComposition = 'TextOnly' then
      vButton.Text := vCaption
    else if vComposition = 'ImageOnly' then
    begin
      vButton.ImageIndex := vImageIndex;
      vButton.StyleLookup := 'stepperbuttonleft';
      ALayout.Padding.Bottom := ALayout.Height - vImageSize - 4;
    end
    else begin
      vButton.Text := vCaption;
      vButton.ImageIndex := vImageIndex;
//      if vComposition = 'ImageRight' then
//        vButton.Align := TAlignLayout.Right
      //else if vComposition = 'ImageTop' then
      //  vButton.ImageAlignment := TImageAlignment.iaTop
      //else if vComposition = 'ImageBottom' then
      //  vButton.ImageAlignment := TImageAlignment.iaBottom
//      else
//        vButton.TextSettings.HorzAlign := TTextAlign.Center;
    end;
  end
  else begin
    vButton.Text := vCaption;
    vButton.WordWrap := True;
  end;

  if (vActionDef.Name = 'Add') and (FView.ParentDomainObject is TEntityList) then
    vDefinitions := TEntityList(FView.ParentDomainObject).ContentDefinitions
  else if (vActionDef.Name = 'Create') and (FView.Parent.DefinitionKind = dkObjectField) then
    vDefinitions := TEntityFieldDef(FView.Parent.Definition).ContentDefinitions
  else
    vDefinitions := nil;

  if Assigned(vDefinitions) then
  begin
    if vDefinitions.Count > 1 then
    begin
      FTypeSelectionMenu := TPopupMenu.Create(nil);
      FTypeSelectionMenu.Images := TImageList(FUIBuilder.Images[16]);
      for i := 0 to vDefinitions.Count - 1 do
      begin
        vDefinition := TDefinition(vDefinitions[i]);
        vMenuItem := TMenuItem.Create(nil);
        vMenuItem.Text := GetTranslation(vDefinition);
        if Length(vOverriddenCaption) > 0 then
          vMenuItem.Text := vOverriddenCaption;
        vMenuItem.ImageIndex := GetImageIndex(vDefinition._ImageID);
        vMenuItem.Tag := NativeInt(FOwner);
        vMenuItem.OnClick := FOwner.OnActionMenuSelected;
        FTypeSelectionMenu.AddObject(vMenuItem);
      end;
      vButton.PopupMenu := FTypeSelectionMenu;
      FTypeSelectionMenu.Parent := vButton;
      vButton.OnClick := ShowPopup;
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

procedure TFMXButton.RefillArea(const AKind: Word);
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

  vButton.Text := GetTranslation(vActionDef);
  vButton.Hint := vButton.Text;
end;

procedure TFMXButton.ShowPopup(Sender: TObject);
var
  vPos: TPointF;
begin
  vPos := TControl(Sender).LocalToScreen(TControl(Sender).Position.Point);
  TButton(Sender).PopupMenu.Popup(vPos.X, vPos.Y);
end;

{ TFMXLink }

function TFMXLink.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
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
  vLabel.Text := FOwner.GetTranslation(vActionDef);
  vLabel.Hint := vLabel.Text;
  if Length(vOverriddenCaption) > 0 then
    vLabel.Text := vOverriddenCaption;
  if Length(vOverriddenHint) > 0 then
    vLabel.Hint := vOverriddenHint;
  vLabel.Cursor := crHandPoint;
  vLabel.TextSettings.HorzAlign := TTextAlign.Leading;
  vLabel.TextSettings.FontColor := ALayout.Font.Color;
  vLabel.Font.Style := [TFontStyle.fsUnderline];
  vLabel.StyledSettings := vLabel.StyledSettings - [TStyledSetting.FontColor, TStyledSetting.Style];
  vLabel.OnClick := FOwner.OnAreaClick;

  Result := vLabel;
end;

procedure TFMXLink.RefillArea(const AKind: Word);
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
  vLabel.Text := GetTranslation(vActionDef);
  vLabel.Hint := vLabel.Text;
end;

{ TFMXControl }

procedure TFMXControl.AssignFromLayout(const ALayout: TLayout; const AParams: string);
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
        vForm.BorderStyle := TFmxFormBorderStyle.Sizeable;
        vForm.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
      end;

      if (ALayout.Tag and cFormDisableMinimizeButton) > 0 then
        vForm.BorderIcons := vForm.BorderIcons - [TBorderIcon.biMinimize];

      if (ALayout.Tag and cFormDisableMaximizeButton) > 0 then
        vForm.BorderIcons := vForm.BorderIcons - [TBorderIcon.biMaximize];

      //if (ALayout.Tag and cFormPositionDesign) > 0 then
      //  vForm.Position := TFormPosition.Designed;

      if (ALayout.Tag and cFormNotResizable) > 0 then
        vForm.BorderStyle := TFmxFormBorderStyle.Single;

      vWS := StrToIntDef(GetUrlParam(AParams, 'WindowState', ''), -1);
      if vWS > -1 then
        vForm.WindowState := TWindowState(vWS);

      if (vForm.WindowState = TWindowState.wsNormal) then
      begin
        //if (ALayout.Tag and cFormUseDesignSizes) > 0 then
        //begin
          vForm.ClientWidth := ALayout.Width;
          if FOwner.View.DefinitionKind = dkDomain then
            vForm.ClientHeight := ALayout.Height
          else
            vForm.ClientHeight := ALayout.Height + cServiceAreaHeight;
        //end;
      end;

      if ALayout.Caption <> '' then
        vForm.Caption := ALayout.Caption;
//      if ALayout.Color <> TAlphaColorRec.Gray then
//      begin
//        vForm.Fill.Color := ALayout.Color;
//        vForm.Fill.Kind := TBrushKind.Solid;
//      end;
    end
    else if ALayout.Kind = lkPanel then
    begin
      vForm.BorderStyle := TFmxFormBorderStyle.Sizeable;
      vForm.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
      vForm.Caption := ALayout.Caption;
      vForm.ClientWidth := ALayout.Width;
      if FOwner.View.DefinitionKind = dkDomain then
        vForm.ClientHeight := ALayout.Height
      else
        vForm.ClientHeight := ALayout.Height + cServiceAreaHeight;
      vForm.Position := TFormPosition.ScreenCenter;
    end
    else if ALayout.Kind = lkPage then
    begin
      vForm.BorderStyle := TFmxFormBorderStyle.Sizeable;
      vForm.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
      vForm.Caption := ALayout.Caption;
    end
    else
      Assert(False, 'Непонятно какой контрол в лэйауте');
  end
  else if ALayout.Kind = lkFrame then
  begin
    if (ALayout.Caption <> '') and (FControl is TTabItem) then
    begin
      TTabItem(FControl).Text := ALayout.Caption;
      if Pos('=', ALayout.Caption) > 0 then // Hint содержит url-строку с параметрами
      begin
        TTabItem(FControl).Text := GetUrlParam(ALayout.Caption, 'Caption', '');
        TTabItem(FControl).ImageIndex := FOwner.GetImageIndex(GetUrlParam(ALayout.Caption, 'ImageIndex', ''));
      end;
    end;
  end
  else if (ALayout.Kind in [lkPanel, lkMemo, lkShape]) and (FControl is TControl) then
  begin
    SetBounds(Rect(ALayout.Left, ALayout.Top,
      ALayout.Left + ALayout.Width, ALayout.Top + ALayout.Height));
    SetCaptionProperty(ALayout);

    if FControl is TCustomEdit then
    begin
      CopyTextSettings(TCustomEdit(FControl).TextSettings, ALayout);
      TCustomEdit(FControl).StyledSettings := [];
    end
    else if FControl is TPresentedTextControl then
    begin
      CopyTextSettings(TPresentedTextControl(FControl).TextSettings, ALayout);
      TPresentedTextControl(FControl).StyledSettings := [];
    end
    else if FControl is TTextControl then
    begin
      CopyTextSettings(TTextControl(FControl).TextSettings, ALayout);
      TTextControl(FControl).StyledSettings := [];
    end;

    TControl(FControl).Anchors := ALayout.Anchors;
    if TControl(FControl).Align = TAlignLayout.None then
      TControl(FControl).Align := AlignToAlignLayout(ALayout.Align);
    CopyMargins(TControl(FControl), ALayout);
    CopyPadding(TControl(FControl), ALayout);
//    SetAlignment(ALayout.Alignment);

    // TODO
  end;
end;

procedure TFMXControl.BeforeContextMenuShow(Sender: TObject);
var
  vMenu: TPopupMenu;
  vMenuItem: TFmxObject;

  procedure CheckMenuItems(const AMenuItem: TMenuItem);
  var
    vChildItem: TFmxObject;
    vArea: TUIArea;
  begin
    if AMenuItem.ChildrenCount > 0 then
    begin
      for vChildItem in AMenuItem.Children do
        CheckMenuItems(TMenuItem(vChildItem));
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
      AMenuItem.IsChecked := TEntity(vArea.View.DomainObject)['IsChecked'];
  end;

begin
  vMenu := TPopupMenu(Sender);
  for vMenuItem in vMenu.Children do
    CheckMenuItems(TMenuItem(vMenuItem));
end;

constructor TFMXControl.Create(const AOwner: TUIArea; const AParams: string);
begin
  inherited Create(AOwner, AParams);
end;

destructor TFMXControl.Destroy;
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
    if vIsFloat then
      FreeAndNil(vControl);
  end
  else
    FreeAndNil(vControl);
end;

procedure TFMXControl.DoActivate(const AUrlParams: string);
var
  vForm: TForm;
  vChangeTab: Boolean;
begin
  if FControl is TForm then
  begin
    vForm := TForm(FControl);
    vForm.Activate;
  end
  else if FControl is TTabItem then
  begin
    vChangeTab := SameText(GetUrlParam(AUrlParams, 'TabActivationOption', ''), 'ChangeTab');
    if Assigned(Parent.CreateParams) and (Parent.CreateParams.IndexOfName('TabActivationOption') >= 0) and
      (not SameText(Parent.CreateParams.Values['TabActivationOption'], 'ChangeTab')) then
      vChangeTab := False;
    if vChangeTab then
      TTabItem(FControl).TabControl.ActiveTab := TTabItem(FControl);
  end;
end;

procedure TFMXControl.DoBeginUpdate;
begin
end;

procedure TFMXControl.DoClose(const AModalResult: Integer);
var
  vForm: TForm;
begin
  if FIsForm then
  begin
    vForm := TForm(FControl);

    if AModalResult = mrNone then
    begin
      TThread.CreateAnonymousThread(procedure
      begin
        TThread.Queue(nil, procedure
        begin
          vForm.Close;
        end);
      end).Start;
    end
    else begin
      if (FLayout.StyleName = 'modal') or (FLayout.StyleName = 'child') then
        if not TPresenter(FPresenter).CanCloseChildForm(vForm, AModalResult) then
          Exit;

      vForm.Close;
      vForm.ModalResult := AModalResult;
    end;
  end;
end;

function TFMXControl.DoCreateCaption(const AParent: TUIArea; const ACaption, AHint: string): TObject;
var
  vLabel: TLabel;
  vFontSize: Single;
begin
  vLabel := TLabel.Create(nil);
  Result := vLabel;

  vLabel.Parent := TFmxObject(GetRealControl(FParent));
  vLabel.TextSettings.Trimming := TTextTrimming.None;
  vLabel.Visible := True;
  vLabel.Text := ACaption;
  vLabel.Hint := AHint;

  if Assigned(FOwner.View.Parent) and (FOwner.View.Parent.DefinitionKind = dkAction)
    and TDefinition(FOwner.View.Parent.Definition).HasFlag(ccInstantExecution) then
  begin
    vLabel.StyledSettings := vLabel.StyledSettings - [TStyledSetting.Size];
    vLabel.Font.Size := 12;
  end
  else begin
    vLabel.StyledSettings := vLabel.StyledSettings - [TStyledSetting.Size, TStyledSetting.FontColor];
    vFontSize := vLabel.Font.Size;
    vFontSize := vFontSize - 3;
    if vFontSize < 8 then
      vFontSize := 8;
    vLabel.Font.Size := vFontSize * 96 / 72;
    vLabel.FontColor := TAlphaColorRec.Gray;
  end;
end;

procedure TFMXControl.DoEndUpdate;
begin
end;

procedure TFMXControl.DoRegisterChanges(const Value: Boolean);
var
  vForm: TCustomForm;
  vAddr: Pointer;
  vFormStates: TFmxFormStates;
begin
  if not (FControl is TCustomForm) then
    Exit;

  vForm := TCustomForm(FControl);
  vFormStates := vForm.FormState;
  vAddr := @vForm.FormState;

  if Value then
    TFmxFormStates(vAddr^) := vFormStates - [TFmxFormState.Showing]
  else
    TFmxFormStates(vAddr^) := vFormStates + [TFmxFormState.Showing];
end;

function TFMXControl.GetActiveChildArea: TUIArea;
begin
  if FControl is TTabControl then
    Result := AreaFromSender(TTabControl(FControl).ActiveTab)
  else
    Result := nil;
end;

function TFMXControl.GetBounds: TRect;
begin
  if FIsForm then
    Result := TRect.Create(TForm(FControl).Left, TForm(FControl).Top,
      TForm(FControl).Left + TForm(FControl).Width, TForm(FControl).Top + TForm(FControl).Height)
  else if FControl is TControl then
    Result := TControl(FControl).BoundsRect.Round
  else
    Result := TRect.Empty;
end;

function TFMXControl.GetFocused: Boolean;
begin
  if FControl is TControl then
    Result := TControl(FControl).IsFocused
  else
    Result := False;
end;

function TFMXControl.GetModalResult: TModalResult;
begin
  if FControl is TForm then
    Result := TForm(FControl).ModalResult
  else
    Result := mrNone;
end;

function TFMXControl.GetTabOrder: Integer;
begin
  if (FControl is TControl) and TControl(FControl).TabStop then
    Result := TControl(FControl).TabOrder
  else
    Result := -1;
end;

function TFMXControl.GetViewState: TViewState;
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

function TFMXControl.GetWindowState: TWindowState;
begin
  if FControl is TForm then
    Result := TForm(FControl).WindowState
  else
    Result := inherited GetWindowState;
end;

function TFMXControl.IndexOfSender(const ASender: TObject): Integer;
begin
  Result := TMenuItem(ASender).Index;
end;

procedure TFMXControl.PlaceLabel;
var
  vSpace: Single;
  vLabel: TLabel;
begin
  if FCaption = nil then
    Exit;

  vLabel := TLabel(FCaption);
  vLabel.WordWrap := false;
  if FLabelPosition = lpTop then
  begin
    vLabel.Position.X := TControl(FControl).Position.X;
    vLabel.Position.Y := TControl(FControl).Position.Y - vLabel.Height - 4;
    vLabel.AutoSize := True;
    vLabel.TextSettings.VertAlign := TTextAlign.Leading;
  end
  else if FLabelPosition = lpLeft then
  begin
    vSpace := vLabel.Width + 8;
    vLabel.Position.X := TControl(FControl).AbsoluteToLocal(PointF(0,0)).X - vSpace;
    vLabel.Position.Y := TControl(FControl).AbsoluteToLocal(PointF(0,0)).Y + 3;
    vLabel.AutoSize := False;
  end;
//  vLabel.Align := TAlignLayout.Center;
  vLabel.Parent := TControl(FControl).Parent;
end;

procedure TFMXControl.SetActiveChildArea(const AArea: TUIArea);
begin
  //
end;

procedure TFMXControl.SetAlignment(const AAlignment: TAlignment);
const
  cTextAligns: array[TAlignment] of TTextAlign =
    (TTextAlign.Leading, TTextAlign.Trailing, TTextAlign.Center);
  cLayoutAlign: array[TAlignment] of TALignLayout =
   (TALignLayout.Left, TALignLayout.Right, TALignLayout.Center);
begin
  if FControl is TCustomEdit then
    TCustomEdit(FControl).TextSettings.HorzAlign := cTextAligns[AAlignment]
  else if FControl is TPresentedTextControl then
    TPresentedTextControl(FControl).TextSettings.HorzAlign := cTextAligns[AAlignment]
  else if FControl is TTextControl then
    TTextControl(FControl).TextSettings.HorzAlign := cTextAligns[AAlignment]
//  else
//    TControl(FControl).Align := cLayoutAlign[AAlignment];
end;

procedure TFMXControl.SetBounds(const Value: TRect);
begin
  if FIsForm then
  begin
    RegisterChanges := False;
    try
      TForm(FControl).SetBoundsF(Value.Left, Value.Top, Value.Width, Value.Height);
    finally
      RegisterChanges := True;
    end;
  end
  else if FControl is TControl then
    TControl(FControl).SetBounds(Value.Left, Value.Top, Value.Width, Value.Height);

  PlaceLabel;
end;

procedure TFMXControl.SetCaptionProperty(const ALayout: TLayout);
begin
  if not Assigned(FCaption) then
    Exit;

  if ALayout.Kind in [lkMemo, lkPanel, lkShape] then
  begin
    FShowCaption := ALayout.ShowCaption;
    TLabel(FCaption).Visible := FShowCaption and (FOwner.View.State > vsHidden);
    if TAnchorKind.akBottom in ALayout.Anchors then
      TLabel(FCaption).Anchors := [TAnchorKind.akLeft, TAnchorKind.akBottom]
    else
      TLabel(FCaption).Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];

    if ALayout.Caption_AtLeft then
    begin
      FLabelPosition := lpLeft;
      PlaceLabel;
    end;
  end;
end;

procedure TFMXControl.SetControl(const AControl: TObject);
begin
  if FControl is TControl then
  begin
    TControl(FControl).OnEnter := nil;
    TControl(FControl).OnExit := nil;
  end;

  inherited SetControl(AControl);

  if FControl is TControl then
  begin
    TControl(FControl).OnEnter := FOwner.OnEnter;
    TControl(FControl).OnExit := FOwner.OnExit;
  end;
end;

procedure TFMXControl.SetFocused(const Value: Boolean);
begin
  if not (FControl is TControl) then
    Exit;

  if Value and TControl(FControl).CanFocus then
    TControl(FControl).SetFocus;
end;

procedure TFMXControl.SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl);
var
  vPopupMenu: TPopupMenu;
begin
  if not Assigned(FControl) then
    Exit;

  if SameText(ATargetName, 'popup') then
  begin
    vPopupMenu := TPopupMenu(TFMXControl(ALinkedControl).Control);
    if not Assigned(vPopupMenu.OnPopup) then
      vPopupMenu.OnPopup := BeforeContextMenuShow;
    TControl(FControl).PopupMenu := vPopupMenu;
  end;
end;

procedure TFMXControl.SetModalResult(const AModalResult: TModalResult);
begin
  if FControl is TForm then
    TForm(FControl).ModalResult := AModalResult;
end;

procedure TFMXControl.SetParent(const AParent: TUIArea);
begin
  inherited SetParent(AParent);

  if not (FControl is TControl) then
    Exit;

  // Установка родителя для контрола
  if FIsForm or not Assigned(AParent) then
    TControl(FControl).Parent := nil
  else if not Assigned(TControl(FControl).Parent) then
  begin
    try
      TControl(FControl).Parent := TFmxObject(GetRealControl(AParent));
    except
      // Fails after CloseAllPages
    end;
  end;
end;

procedure TFMXControl.SetTabOrder(const ATabOrder: Integer);
begin
  if not (FControl is TControl) then
    Exit;

  if ATabOrder >= 0 then
  begin
    TControl(FControl).TabStop := True;
    TControl(FControl).TabOrder := ATabOrder;
  end
  else
    TControl(FControl).TabStop := False;
end;

procedure TFMXControl.SetViewState(const AViewState: TViewState);
begin
  if FIsForm then
    Exit;

  if (FControl is TControl) and not (FControl is TAniIndicator) then
  begin
    TControl(FControl).Visible := AViewState > vsHidden;
    TControl(FControl).Enabled := AViewState > vsDisabled;
  end;
end;

procedure TFMXControl.SetWindowState(const AWindowState: TWindowState);
begin
  if FControl is TForm then
    TForm(FControl).WindowState := AWindowState;
end;

procedure TFMXControl.UpdateCaptionVisibility;
begin
  if Assigned(FCaption) then
    TLabel(FCaption).Visible := FShowCaption and (FOwner.View.State > vsHidden);
end;

{ TFMXNavBarNavigation }

function TFMXNavBarNavigation.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FNavBar := TPanel.Create(nil);
  FNavBarGroup := nil;
  FNavBarItem := nil;

  Result := FNavBar;
end;

function TFMXNavBarNavigation.DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem; const ACaption,
  AHint: string; const AImageIndex: Integer): TObject;
var
  vLastPos: Single;
begin
  if ANavItem.Level = 0 then
  begin
    vLastPos := 0;
    if Assigned(FNavBarGroup) then
      vLastPos := FNavBarGroup.Position.Y + FNavBarGroup.Height;
    FNavBarGroup := TExpander.Create(nil);
    FNavBarGroup.Align := TAlignLayout.Top;
    FNavBarGroup.Position.Y := vLastPos;
    FNavBarGroup.Height := 25;
    FNavBarGroup.Text := ACaption;
    FNavBarGroup.Hint := AHint;
    FNavBarGroup.OnClick := nil;
    FNavBar.AddObject(FNavBarGroup);
    Result := FNavBarGroup;
  end
  else
  begin
    FNavBarItem := TButton.Create(nil);
    FNavBarItem.Align := TAlignLayout.Top;
    FNavBarItem.Position.Y := FNavBarGroup.Height;
    FNavBarGroup.Height := FNavBarGroup.Height + FNavBarItem.Height;
    FNavBarItem.Text := ACaption;
    FNavBarItem.Hint := AHint;
    FNavBarItem.Images := TImageList(FUIBuilder.Images[32]);
    FNavBarItem.ImageIndex := AImageIndex;
    FNavBarItem.OnClick := FOwner.OnAreaClick;
    FNavBarGroup.AddObject(FNavBarItem);
    Result := FNavBarItem;
  end;
end;

{ TFMXOneButtonNavigation }

function TFMXOneButtonNavigation.DoCreateControl(const AParent: TUIArea;
  const ALayout: TLayout): TObject;
var
  vImageSize: Integer;
  vActionName: string;
  vAction: TActionDef;
begin
  FButton := TButton.Create(nil);
  FPopupButton := TButton.Create(FButton);

  FButton.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
  FButton.AddObject(FPopupButton);

  vImageSize := StrToIntDef(GetUrlParam(FParams, 'ImageSize'), 16);
  FButton.Images := TImageList(FUIBuilder.Images[vImageSize]);

  FPopupButton.Images := TImageList(FUIBuilder.Images[vImageSize]);
  FPopupButton.ImageIndex := AParent.GetImageIndex('arrow_down');

  vActionName := GetUrlParam(FParams, 'Action');
  if Length(vActionName) > 0 then
  begin
    FView := FView.BuildView(vActionName);
    Assert(FView.DefinitionKind = dkAction);

    vAction := TActionDef(FView.Definition);
    FButton.ImageIndex := AParent.GetImageIndex(vAction._ImageID);
    FButton.Text := AParent.GetTranslation(vAction, tpCaption);
    FButton.Hint := AParent.GetTranslation(vAction, tpHint);
    FButton.OnClick := AParent.OnAreaClick;
    FPopupButton.OnClick := OnClick;
  end
  else
  begin
    FButton.ImageIndex := AParent.GetImageIndex
      (GetUrlParam(FParams, 'ImageIndex'));
    FButton.Text := GetUrlParam(FParams, 'Caption');
    FButton.Hint := GetUrlParam(FParams, 'Hint');
    FButton.OnClick := OnClick;
  end;
  FMenu := TPopupMenu.Create(nil);
  FMenu.Parent := FButton;
  FMenu.PopupComponent := FButton;
  FMenu.Images := TImageList(FUIBuilder.Images[16]);
  FButton.PopupMenu := FMenu;

  FPopupButton.Images := TImageList(FUIBuilder.Images[16]);
  FPopupButton.Width := 20;
  FPopupButton.Align := TAlignLayout.Right;
  Result := FButton;
end;

function TFMXOneButtonNavigation.DoCreateItem(const AParent: TUIArea;
  const ANavItem: TNavigationItem; const ACaption, AHint: string;
  const AImageIndex: Integer): TObject;
var
  vControl: TObject;
begin
  FItem := TMenuItem.Create(nil);
  FItem.Text := ACaption;
  FItem.Hint := AHint;
  FItem.ImageIndex := AImageIndex;
  FItem.OnClick := FOwner.OnAreaClick;
  vControl := GetRealControl(AParent);
  if (ANavItem.Level > 0) and (vControl is TMenuItem) then
    TMenuItem(vControl).AddObject(FItem)
  else
    FMenu.AddObject(FItem);
  Result := FItem;
end;

procedure TFMXOneButtonNavigation.OnClick(Sender: TObject);
var
  vPoint: TPointF;
begin
  vPoint := FButton.LocalToScreen(PointF(0, FButton.Height));
  FMenu.Popup(vPoint.X, vPoint.Y);
end;

initialization

RegisterClasses([TLabel, TPanel, TSplitter, TImage, TMemo, TTabControl, TScrollBox, TShape, TPopupMenu]);

TPresenter.RegisterControlClass('FMX', uiNavigation, '', TFMXTreeViewNavigation);
TPresenter.RegisterControlClass('FMX', uiNavigation, 'TreeView', TFMXTreeViewNavigation);
TPresenter.RegisterControlClass('FMX', uiNavigation, 'MainMenu', TFMXMainMenuNavigation);
TPresenter.RegisterControlClass('FMX', uiNavigation, 'ToolBar', TFMXToolBarNavigation);
TPresenter.RegisterControlClass('FMX', uiNavigation, 'NavBar', TFMXNavBarNavigation);
TPresenter.RegisterControlClass('FMX', uiEntityEdit, 'OneButton', TFMXOneButtonNavigation);

TPresenter.RegisterControlClass('FMX', uiAction, '', TFMXButton);
TPresenter.RegisterControlClass('FMX', uiAction, 'link', TFMXLink);

end.
