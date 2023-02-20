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

unit dexArea;

interface

uses
  Windows, Classes, Forms, Messages, Generics.Collections, Controls, StdCtrls, ExtCtrls, Menus, UITypes, SysUtils,
  dxBar, dxNavBar, dxNavBarGroupItems, dxNavBarCollns, dxNavBarBase, dxNavBarExplorerViews,
  uConsts, uUIBuilder, vclArea, uDefinition, uEntity, uView, uLayout;

type
  TDEControl = class(TVCLControl)
  protected
    procedure DoActivate(const AUrlParams: string); override;

    procedure AssignFromLayout(const ALayout: TLayout; const AParams: string); override;

    procedure SetAlignment(const AAlignment: TAlignment); override;
    procedure SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl); override;
    function GetViewState: TViewState; override;
    procedure SetViewState(const AViewState: TViewState); override;
    function GetActiveChildArea: TUIArea; override;
    procedure SetActiveChildArea(const AArea: TUIArea); override;
  end;

  TDEButton = class(TDEControl)
  private
    FTypeSelectionMenu: TPopupMenu;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TDELink = class(TDEControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TDENavBarNavigation = class(TDEControl)
  private
    FNavBar: TdxNavBar;
    FNavBarGroup: TdxNavBarGroup;
    FNavBarItem: TdxNavBarItem;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

implementation

uses
  Types, Graphics, Math, StrUtils, ComCtrls, Buttons,
  Generics.Defaults, Variants,
  cxGraphics, dxGDIPlusClasses, cxLabel, cxImage, cxEdit, cxTextEdit, cxPC,
  cxLookAndFeels, cxButtons, cxScrollBox, cxControls, cxSplitter, cxCheckBox,

  uDomain, uPresenter, uWinVCLPresenter, uConfiguration, uSession, uInteractor, uUtils, uCollection,
  vclSimpleEditors, uEntityList, uDomainUtils, uChangeManager;

type
  TCrackedWinControl = class(TWinControl) end;
  TCrackedControl = class(TControl) end;

{ TDENavBarNavigation }

function TDENavBarNavigation.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vColor: TColor;
begin
  FNavBar := TdxNavBar.Create(nil);
  FNavBar.DoubleBuffered := True;
  FNavBar.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
  FNavBar.SmallImages := TDragImageList(FUIBuilder.Images[16]);
  FNavBar.LargeImages := TDragImageList(FUIBuilder.Images[32]);
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

function TDENavBarNavigation.DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vControl: TObject;
begin
  if ANavItem.Level = 0 then
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
    vControl := GetRealControl(AParent);
    if vControl is TdxNavBarGroup then
      TdxNavBarGroup(vControl).CreateLink(FNavBarItem);
    FNavBarItem.Caption := ACaption;
    FNavBarItem.Hint := AHint;
    FNavBarItem.SmallImageIndex := AImageIndex;
    FNavBarItem.LargeImageIndex := AImageIndex;
    FNavBarItem.OnClick := FOwner.OnAreaClick;
    Result := FNavBarItem;
  end;
end;

{ TDEButton }

procedure TDEButton.DoBeforeFreeControl;
begin
  FreeAndNil(FTypeSelectionMenu);
end;

function TDEButton.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vParams: TStrings;
  vButton: TcxButton;
  vActionDef: TDefinition;
  vDefinitions: TList<TDefinition>;
  i, vImageIndex: Integer;
  vMenuItem: TMenuItem;
  vDefinition: TDefinition;
  vImageID: string;
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
    vComposition := Trim(vParams.Values['Composition']);
    vViewStyle := Trim(vParams.Values['ViewStyle']);
    vOverriddenCaption := Trim(vParams.Values['Caption']);
    vOverriddenHint := Trim(vParams.Values['Hint']);
  finally
    FreeAndNil(vParams);
  end;

  vButton := TcxButton.Create(nil);
  vButton.OptionsImage.Images := TDragImageList(FUIBuilder.Images[vImageSize]);
  vImageIndex := FOwner.GetImageIndex(vImageID);

  if (ALayout.BevelOuter = lbkNone) and (ALayout.BevelInner = lbkNone) then
  begin
    vButton.SpeedButtonOptions.Flat := True;
    vButton.SpeedButtonOptions.CanBeFocused := False;
    vButton.SpeedButtonOptions.Transparent := True;
  end;

  vButton.Caption := FOwner.GetTranslation(vActionDef);
  vButton.Hint := vButton.Caption;
  if Length(vOverriddenCaption) > 0 then
    vButton.Caption := vOverriddenCaption;
  if Length(vOverriddenHint) > 0 then
    vButton.Hint := vOverriddenHint;

  if (vButton.OptionsImage.Images.Count + 1 >= vImageIndex) and (vImageIndex > 0) then
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
    vButton.OptionsImage.ImageIndex := vImageIndex;
  end
  else
  begin
    vButton.PaintStyle := bpsCaption;
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
      FTypeSelectionMenu.Images := TDragImageList(FUIBuilder.Images[16]);
      for i := 0 to vDefinitions.Count - 1 do
      begin
        vDefinition := TDefinition(vDefinitions[i]);
        vMenuItem := TMenuItem.Create(nil);
        vMenuItem.Caption := FOwner.GetTranslation(vDefinition);
        if Length(vOverriddenCaption) > 0 then
          vMenuItem.Caption := vOverriddenCaption;
        vMenuItem.ImageIndex := FOwner.GetImageIndex(vDefinition._ImageID);
        vMenuItem.Tag := NativeInt(FOwner);
        vMenuItem.OnClick := FOwner.OnActionMenuSelected;
        FTypeSelectionMenu.Items.Add(vMenuItem);
      end;
      vButton.DropDownMenu := FTypeSelectionMenu;
      vButton.Kind := cxbkOfficeDropDown;
      vButton.OptionsImage.Margin := 4;
      ALayout.Width := 42; //Round(ALayout.Height * 1.5);
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

procedure TDEButton.RefillArea(const AKind: Word);
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
  vImageID := FOwner.GetImageIndex(vActionDef._ImageID);

  if (vButton.OptionsImage.Images.Count + 1 >= vImageID) and (vImageID > 0) then
    vButton.OptionsImage.ImageIndex := vImageID;

  vButton.Caption := FOwner.GetTranslation(vActionDef);
  vButton.Hint := vButton.Caption;
end;

{ TDELink }

function TDELink.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vParams: TStrings;
  vLabel: TcxLabel;
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

  vLabel := TcxLabel.Create(nil);
  vLabel.Caption := FOwner.GetTranslation(vActionDef);
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
  vLabel.OnClick := FOwner.OnAreaClick;

  Result := vLabel;
end;

procedure TDELink.RefillArea(const AKind: Word);
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
  vLabel.Caption := FOwner.GetTranslation(vActionDef);
  vLabel.Hint := vLabel.Caption;
end;

{ TDEControl }

procedure TDEControl.AssignFromLayout(const ALayout: TLayout; const AParams: string);
begin
  if not FIsForm and (ALayout.Kind = lkFrame) then
  begin
    //vFrame.SetBounds(Control.Left, Control.Top, Control.Width, Control.Height);
    if (ALayout.Caption <> '') and (FControl is TcxTabSheet) then
    begin
      TcxTabSheet(FControl).Caption := ALayout.Caption;
      if Pos('=', ALayout.Caption) > 0 then // Hint содержит url-строку с параметрами
      begin
        TcxTabSheet(FControl).Caption := GetUrlParam(ALayout.Caption, 'Caption', '');
        TcxTabSheet(FControl).ImageIndex := FOwner.GetImageIndex(GetUrlParam(ALayout.Caption, 'ImageIndex', ''));
      end;
    end;
  end
  else
    inherited AssignFromLayout(ALayout, AParams);
end;

procedure TDEControl.DoActivate(const AUrlParams: string);
var
  vChangeTab: Boolean;
begin
  if (FControl is TcxTabSheet) then
  begin
    vChangeTab := SameText(GetUrlParam(AUrlParams, 'TabActivationOption', ''), 'ChangeTab');
    if Assigned(Parent.CreateParams) and (Parent.CreateParams.IndexOfName('TabActivationOption') >= 0) and
      (not SameText(Parent.CreateParams.Values['TabActivationOption'], 'ChangeTab')) then
      vChangeTab := False;
    if vChangeTab then
      TcxPageControl(TControl(FControl).Parent).Properties.ActivePage := TcxTabSheet(FControl);
  end
  else
    inherited DoActivate(AUrlParams);
end;

function TDEControl.GetActiveChildArea: TUIArea;
begin
  if FControl is TcxPageControl then
    Result := AreaFromSender(TcxPageControl(FControl).ActivePage)
  else
    Result := inherited GetActiveChildArea;
end;

function TDEControl.GetViewState: TViewState;
begin
  if (FControl is TcxTabSheet) and not TcxTabSheet(FControl).AllowCloseButton then
    Result := vsDisabled
  else
    Result := inherited GetViewState;
end;

procedure TDEControl.SetActiveChildArea(const AArea: TUIArea);
begin
  //
end;

procedure TDEControl.SetAlignment(const AAlignment: TAlignment);
begin
  if FControl.InheritsFrom(TcxLabel) then
    TcxLabel(FControl).Properties.Alignment.Horz := AAlignment
  else if FControl.InheritsFrom(TcxTextEdit) then
    TcxTextEdit(FControl).Properties.Alignment.Horz := AAlignment
  else if FControl.InheritsFrom(TcxCheckBox) then
    TcxCheckBox(FControl).Properties.Alignment := AAlignment
  else
    inherited SetAlignment(AAlignment);
end;

procedure TDEControl.SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl);
begin
  if SameText(ATargetName, 'splitter') then
  begin
    if FControl is TcxSplitter then
      TcxSplitter(FControl).Control := TControl(TDEControl(ALinkedControl).Control);
  end
  else
    inherited SetLinkedControl(ATargetName, ALinkedControl);
end;

procedure TDEControl.SetViewState(const AViewState: TViewState);
begin
  if FControl is TcxTabSheet then
    TcxTabSheet(FControl).AllowCloseButton := AViewState > vsDisabled
  else
    inherited SetViewState(AViewState);
end;

initialization

TPresenter.RegisterControlClass('Windows.DevExpress', uiNavigation, 'NavBar', TDENavBarNavigation);

TPresenter.RegisterControlClass('Windows.DevExpress', uiAction, '', TDEButton);
TPresenter.RegisterControlClass('Windows.DevExpress', uiAction, 'link', TDELink);

end.
