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

unit uDevExpressArea;

interface

uses
  Windows, Classes, Forms, Messages, Generics.Collections, Controls, StdCtrls, ExtCtrls, Menus, UITypes, SysUtils,
  uConsts, uUIBuilder, uDefinition, uEntity, uView, Graphics, uCustomVCLArea, uBaseLayout;

type
  TDevExpressFieldArea = class(TVCLFieldArea)
  protected
    procedure AssignFont(const AFont: TFont); override;
    function DoCreateChildArea(const ALayout: TBaseLayout; const AView: TView; const AParams: string = ''; const AOnClose: TProc = nil): TUIArea; override;
  end;

  TDevExpressArea = class(TCustomVCLArea)
  private
    procedure OnPCCanClose(Sender: TObject; var ACanClose: Boolean);
  protected
    procedure AssignFont(const AFont: TFont); override;
    procedure AssignAlignment(const AAlign: TAlignment); override;
    procedure ProcessFrame(const AFrame: TBaseLayout); override;
    procedure DoActivate(const AUrlParams: string); override;
    procedure DoAfterChildAreasCreated; override;
    function DoCreateChildAction(const ALayout: TBaseLayout; const AView: TView; const AParams: string = ''): TUIArea; override;
    function DoCreateChildArea(const ALayout: TBaseLayout; const AView: TView; const AParams: string = ''; const AOnClose: TProc = nil): TUIArea; override;
    function GetSelectedIndexFromControl(const AControl: TComponent; const AMenuItem: TMenuItem): Integer; override;
    procedure SetViewState(const AValue: TViewState); override;
    function GetNavigationAreaClass: TNavigationAreaClass; override;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TBaseLayout = nil; const AParams: string = ''); override;
    destructor Destroy; override;

  end;


implementation

uses
  Types,  Math, StrUtils, ComCtrls, Buttons,
  Generics.Defaults, Variants, cxGraphics, dxGDIPlusClasses, cxLabel, cxImage, cxEdit, cxTextEdit, cxPC, dxBar, dxNavBar, dxNavBarGroupItems,
  dxNavBarCollns, dxNavBarBase, dxNavBarExplorerViews,
  cxLookAndFeels, cxButtons, cxScrollBox, cxControls, cxSplitter,

  uDomain, uPresenter, uConfiguration, uSession, uInteractor, uUtils, uCollection,
  vclSimpleEditors, uEntityList, uDomainUtils;

type
  TCrackedControl = class(TWinControl) end;

  TDevExpressNavBarArea = class(TNavigationArea)
  private
    FNavBar: TdxNavBar;
    FNavBarGroup: TdxNavBarGroup;
    FNavBarItem: TdxNavBarItem;
  protected
    function GetControl: TObject; override;
    function DoCreateItem(const AParentObj: TObject; const AParams: string): TObject; override;
    procedure DoAssignItemOnClick(const AHandler: TNotifyEvent); override;
    procedure DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer); override;
  public
    constructor Create(const ASource: TBaseLayout; const AView: TView; const AArea: TCustomVCLArea; const AInteractor: TObject; const AParams: string); override;
    destructor Destroy; override;
  end;

{ TDevExpressArea }

procedure TDevExpressArea.AssignAlignment(const AAlign: TAlignment);
begin
  if FControl.InheritsFrom(TcxLabel) then
    TcxLabel(FControl).Properties.Alignment.Horz := AAlign
  else if FControl.InheritsFrom(TcxTextEdit) then
    TcxTextEdit(FControl).Properties.Alignment.Horz := AAlign;
end;

procedure TDevExpressArea.AssignFont(const AFont: TFont);
begin
  if FControl is TcxCustomEdit then
    TcxCustomEdit(FControl).Style.Font.Assign(AFont)
  else
    TCrackedControl(FControl).Font.Assign(AFont);
end;

constructor TDevExpressArea.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False; const AControl: TObject = nil; const ALayout: TBaseLayout = nil;
  const AParams: string = '');
begin
  inherited;

end;

destructor TDevExpressArea.Destroy;
begin

  inherited;
end;

procedure TDevExpressArea.DoActivate(const AUrlParams: string);
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
      TcxPageControl(Control.Parent).Properties.ActivePage := TcxTabSheet(FControl);
  end
  else
    inherited;
end;

procedure TDevExpressArea.DoAfterChildAreasCreated;
var
  i: Integer;
  function FindControlForSplitter(const ASplitter: TcxSplitter): TControl;
  var
    c: Integer;
  begin
    Result := nil;
    for c := 0 to Count - 1 do
      if (Areas[c].Control <> ASplitter) and (Areas[c].Control is TControl) and (TControl(Areas[c].Control).Align = ASplitter.Align) then
      begin
        Result := TControl(Areas[c].Control);
        Break;
      end;
  end;
begin
  // прицепляем сплиттер к своему контролу, чтобы не отлипал в некоторых случаях, обрабатываем только простой случай: сплиттер с таким размещением один в текущей области
  for i := 0 to Count - 1 do
    if Areas[i].Control is TcxSplitter then
      TcxSplitter(Areas[i].Control).Control := FindControlForSplitter(TcxSplitter(Areas[i].Control));
end;

function TDevExpressArea.DoCreateChildAction(const ALayout: TBaseLayout; const AView: TView; const AParams: string): TUIArea;
var
  vButton: TcxButton;
  vLabel: TcxLabel;
  vActionDef: TDefinition;
  vDefinitions: TList<TDefinition>;
  i: Integer;
  vMenuItem: TMenuItem;
  vDefinition: TDefinition;
  vOnClickHandler: TNotifyEvent;
  vImageID: Integer;
  vParams: TStrings;
  vImageSize: Integer;
  vComposition: string;
  vViewStyle: string;
  vOverriddenCaption, vOverriddenHint: string;
begin
  if AView.DefinitionKind = dkCollection then
    vOnClickHandler := OnOpenCollection
  else
    vOnClickHandler := OnExecuteAction;

  vActionDef := TDefinition(AView.Definition);

  vParams := CreateDelimitedList(AParams, '&');
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

  if vViewStyle = 'link' then
  begin
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
    vLabel.Style.TextColor := ALayout.Font.Color;
    //vLabel.Style.TextStyle := [fsUnderline];
    vLabel.Style.HotTrack := True;
    //vLabel.StyleHot.TextColor := clBlue;
    vLabel.StyleHot.TextStyle := [fsUnderline];
    vLabel.OnClick := vOnClickHandler;

    Result := TVCLAreaClass(Self.ClassType).Create(Self, AView, vActionDef.Name, False, vLabel);
    if AParams <> '' then
      Result.AddParams(CreateDelimitedList(AParams, '&'));

    Exit;
  end;

  vButton := TcxButton.Create(nil);

  vButton.OptionsImage.Images := TDragImageList(TInteractor(Interactor).Images[vImageSize]);

  vImageID := GetImageID(vImageID);

  if (ALayout.ExtractInteger('BevelOuter') = Ord(bvNone)) and (ALayout.ExtractInteger('BevelInner') = Ord(bvNone))
    and (ALayout.ExtractInteger('BevelKind') = Ord(TBevelKind.bkNone))
  then begin
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
      if ALayout.ExtractBoolean('ShowHint') then
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

  if (vActionDef.Name = 'Add') and Assigned(AView.ParentDomainObject) and (AView.ParentDomainObject is TEntityList) then
  begin
    vDefinitions := TEntityList(AView.ParentDomainObject).ContentDefinitions;
    if vDefinitions.Count > 1 then
    begin
      FPopupMenu := TPopupMenu.Create(nil);
      FPopupMenu.Images := TDragImageList(TInteractor(Interactor).Images[16]);
      for i := 0 to vDefinitions.Count - 1 do
      begin
        vDefinition := TDefinition(vDefinitions[i]);
        vMenuItem := TMenuItem.Create(nil);
        vMenuItem.Caption := GetTranslation(vDefinition);
        if Length(vOverriddenCaption) > 0 then
          vMenuItem.Caption := vOverriddenCaption;
        vMenuItem.ImageIndex := GetImageID(vDefinition._ImageID);
        vMenuItem.Tag := NativeInt(vButton);
        vMenuItem.OnClick := OnActionMenuSelected;
        FPopupMenu.Items.Add(vMenuItem);
      end;
      vButton.DropDownMenu := FPopupMenu;
      vButton.Kind := cxbkOfficeDropDown;
      vButton.OptionsImage.Margin := 4;
    end
    else begin
      vButton.OnClick := vOnClickHandler;
      ALayout.Width := ALayout.Height;
    end;
  end
  else
    vButton.OnClick := vOnClickHandler;

  Result := TDevExpressArea.Create(Self, AView, vActionDef.Name, False, vButton);
  if AParams <> '' then
    Result.AddParams(CreateDelimitedList(AParams, '&'));
end;

function TDevExpressArea.DoCreateChildArea(const ALayout: TBaseLayout; const AView: TView; const AParams: string; const AOnClose: TProc): TUIArea;
var
  vDomain: TDomain;
  vPos: Integer;
  vCaption: string;
  vUIParams: string;
  vStartPageName: string;
  vStartPageStr: string;
  vLabel: TcxLabel; //TStaticText;
  vImage: TcxImage;
  vPC: TcxPageControl;
  vTab: TcxTabSheet;
  vPanel: TPanel;
  vParams: TStrings;
  vBox: TcxScrollBox;
  vBevel: TBevel;
  vSplitter: TcxSplitter;
  vMenu: TMenu;
  vForm: TForm;
  vShape: TShape;
begin
  Result := nil;

  if not Assigned(ALayout) then
    Exit;

  vDomain := TDomain(TInteractor(Interactor).Domain);

  if ALayout.LayoutClass = 'TShape' then
  begin
    vShape := TShape.Create(nil);
    vShape.Left := ALayout.Left;
    vShape.Top := ALayout.Top;
    vShape.Width := ALayout.Width;
    vShape.Height := ALayout.Height;
    vShape.Anchors := ALayout.Anchors;
    vShape.Align := ALayout.Align;
    vShape.Hint := ALayout.Hint;
    vShape.Visible := ALayout.Visible;
    vShape.Pen.Color := ALayout.ExtractColor('Pen.Color');
    vShape.Brush.Color := ALayout.ExtractColor('Brush.Color');

    Result := TDevExpressArea.Create(Self, AView, '', False, vShape);
  end
  else if ALayout.LayoutClass = 'TLabel' then
  begin
    vLabel := TcxLabel.Create(nil);   // заменил с TLabel на TcxLabel, потому что TLabel мерцал при растягивании формы
    vLabel.Left := ALayout.Left;
    vLabel.Top := ALayout.Top;
    vLabel.Width := ALayout.Width;
    vLabel.Height := ALayout.Height;
    vLabel.Transparent := ALayout.ExtractBoolean('Transparent');
    vLabel.AutoSize := ALayout.ExtractBoolean('AutoSize');
    vLabel.Properties.WordWrap := ALayout.ExtractBoolean('WordWrap');
    vLabel.Style.Font.Size := ALayout.Font.Size;
    vLabel.Style.Font.Color := ALayout.Font.Color;
    vLabel.Style.Font.Style := ALayout.Font.Style;
    vLabel.Anchors := ALayout.Anchors;

    vCaption := ALayout.Caption;
    vUIParams := '';
    vPos := Pos('@', vCaption);
    if vPos > 1 then
    begin
      vUIParams := vCaption;
      Delete(vUIParams, 1, vPos);
      vCaption := Copy(vCaption, 1, vPos - 1);

      // Обработка служебных надписей
      if (vCaption = '$') and (AView.DefinitionKind = dkCollection) and (vUIParams = 'Caption') then
        vCaption := GetTranslation(TDefinition(AView.Definition))
      else
        // Обработать другие ситуации
    end
    else
      vCaption := ALayout.Caption; //FView.Interactor.Translate('@' + {FFullAreaName или FLayoutName +} '.' +
        //vSourceLabel.Name + '@Caption', vSourceLabel.Caption);

    vLabel.Caption := vCaption;

    Result := TDevExpressArea.Create(Self, AView, '', False, vLabel);
  end
  else if ALayout.LayoutClass = 'TImage' then
  begin
    vImage := TcxImage.Create(nil);
    vImage.Style.BorderStyle := ebsNone;
    vImage.ControlStyle := vImage.ControlStyle + [csOpaque];
    vImage.Width := ALayout.Width;
    vImage.Height := ALayout.Height;
    vImage.Left := ALayout.Left;
    vImage.Top := ALayout.Top;
    vImage.Picture := TPicture.Create;
    vImage.Picture.Graphic := StringToPictureGraphic(ALayout.ExtractString('Picture'));
    vImage.Anchors := ALayout.Anchors;
    vImage.Align := ALayout.Align;
    vImage.Transparent := True;
    vImage.Properties.ShowFocusRect := False;
    vImage.Properties.PopupMenuLayout.MenuItems := [];
    vImage.Properties.FitMode := ifmNormal;
    vImage.DoubleBuffered := True;
    vImage.Properties.Stretch := ALayout.ExtractBoolean('Stretch');
    vImage.Properties.Proportional := ALayout.ExtractBoolean('Proportional');
    vImage.Hint := ALayout.Hint;
    vImage.AlignWithMargins := ALayout.AlignWithMargins;
    vImage.Margins := ALayout.Margins;
    vImage.Properties.Center := False;

    Result := TDevExpressArea.Create(Self, AView, '', False, vImage);
  end
  else if ALayout.LayoutClass = 'TPageControl' then
  begin
    vPC := TcxPageControl.Create(nil);
    vPC.DoubleBuffered := True;
    vPC.Width := ALayout.Width;
    vPC.Height := ALayout.Height;
    vPC.Left := ALayout.Left;
    vPC.Top := ALayout.Top;
    vPC.Properties.TabPosition := TcxTabPosition(ALayout.ExtractInteger('TabPosition'));
    vPC.Properties.TabHeight := ALayout.ExtractInteger('TabHeight');
    vPC.Properties.TabWidth := ALayout.ExtractInteger('TabWidth');
    vPC.Font.Assign(ALayout.Font);
    vPC.Align := ALayout.Align;
    vPC.AlignWithMargins := ALayout.AlignWithMargins;
    vPC.Margins := ALayout.Margins;
    vPC.Anchors := ALayout.Anchors;
    vPC.Visible := ALayout.Visible;
    if (ALayout.Tag and cPCNavigatorFlag) > 0 then
    begin
      vPC.Properties.Rotate := True;
      vPC.Properties.Images := TDragImageList(TInteractor(Interactor).Images[32]);
      vPC.Properties.TabCaptionAlignment := taLeftJustify;
    end
    else
    begin
      vPC.Properties.Options := vPC.Properties.Options + [pcoTopToBottomText];
      vPC.LookAndFeel.NativeStyle := False;
      vPC.LookAndFeel.Kind := lfUltraFlat;
    end;

    Result := TDevExpressArea.Create(Self, AView, '', False, vPC);
  end
  else if ALayout.LayoutClass = 'TTabSheet' then
  begin
    if (TInteractor(AView.Interactor).Layout = 'mdi') and (ALayout.Tag = 11) then
    begin
      vForm := TForm.Create(nil);
      vForm.Caption := ALayout.Caption;
      vForm.Position := poDefault;
      vForm.FormStyle := fsMDIChild;
      vForm.OnClose := OnCloseMDIForm;
      vForm.ShowHint := True;
      if AView.DefinitionKind in [dkCollection, dkAction, dkEntity] then
        TDragImageList(TInteractor(Interactor).Images[16]).GetIcon(GetImageID(TDefinition(AView.Definition)._ImageID), vForm.Icon);
      Result := TVCLAreaClass(Self.ClassType).Create(Self, AView, ALayout.Name, False, vForm);
      TCustomVCLArea(Result).OnClose := AOnClose;
    end
    else
    begin
      vTab := TcxTabSheet.Create(Component);
      vTab.Caption := ALayout.Caption;
      vTab.ImageIndex := GetImageID(ALayout.ExtractInteger('ImageIndex'));

      vStartPageName := TDomain(FView.Domain).Settings.GetValue('Core', 'StartPage', '');
      vTab.AllowCloseButton := not SameText(ALayout.Name, vStartPageName);
      vTab.TabVisible := ALayout.ExtractBoolean('TabVisible');

      Result := TVCLAreaClass(Self.ClassType).Create(Self, AView, ALayout.Name, False, vTab);

//      if vSourceTabSheet.Visible then // это нужно? иначе лишние перерисовки контролов идут
//        TcxPageControl(FControl).ActivePage := vTab;
    end;
  end
  else if ALayout.LayoutClass = 'TMainMenu' then
  begin
    vMenu := TMainMenu.Create(Component);
    vMenu.Images := TDragImageList(TInteractor(Interactor).Images[16]);
    Result := TVCLAreaClass(Self.ClassType).Create(Self, AView, 'Menu', False, vMenu);
    CopyMenuItems(Result, AView, ALayout, vMenu.Items);
  end
  else if ALayout.LayoutClass = 'TPopupMenu' then
  begin
    vMenu := TPopupMenu.Create(Component);
    vMenu.Images := TDragImageList(TInteractor(Interactor).Images[16]);
    Result := TVCLAreaClass(Self.ClassType).Create(Self, AView, 'Popup', False, vMenu);
    CopyPopupMenuItems(Result, AView, ALayout, vMenu.Items);
  end
  else if ALayout.LayoutClass = 'TBevel' then
  begin
    vBevel := TBevel.Create(nil);
    vBevel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vBevel.Align := ALayout.Align;
    vBevel.Shape := TBevelShape(ALayout.ExtractInteger('Shape'));
    vBevel.Style := TBevelStyle(ALayout.ExtractInteger('Style'));
    Result := TDevExpressArea.Create(Self, AView, '-bevel-', False, vBevel);
  end
  else if ALayout.LayoutClass = 'TSplitter' then
  begin
    vSplitter := TcxSplitter.Create(nil);
    case ALayout.Align of
      alTop: vSplitter.AlignSplitter := salTop;
      alLeft: vSplitter.AlignSplitter := salLeft;
      alRight: vSplitter.AlignSplitter := salRight;
    else
      vSplitter.AlignSplitter := salBottom;
    end;
    vSplitter.Cursor := ALayout.ExtractInteger('Cursor');
    vSplitter.HotZone := TcxSimpleStyle.Create(vSplitter);
    vSplitter.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vSplitter.Color := ALayout.Color;
    vSplitter.ParentColor := False;
    vSplitter.NativeBackground := False;
    Result := TDevExpressArea.Create(Self, AView, '-splitter-', False, vSplitter);
  end
  else if ALayout.LayoutClass = 'TPanel' then
  begin
    if AParams <> '' then
      vParams := CreateDelimitedList(AParams, '&')
    else
      vParams := nil;

    if (ALayout.Caption = 'NavigationArea') then
    begin
      Result := CreateNavigationArea(ALayout, AView, AParams);
    end
    else if Assigned(vParams) and (vParams.Values['ViewType'] = 'Paged') then
    begin
      if TInteractor(FView.Interactor).Layout = 'mdi' then
      begin
        FUIBuilder.DefaultParams := AParams;
        FreeAndNil(vParams);
        Exit(nil);
      end;

      vPC := TcxPageControl.Create(nil);
      vPC.DoubleBuffered := True;
      vPC.Width := ALayout.Width;
      vPC.Height := ALayout.Height;
      vPC.Left := ALayout.Left;
      vPC.Top := ALayout.Top;
      vPC.Properties.Images := TDragImageList(TInteractor(Interactor).Images[16]);
      vPC.Properties.TabPosition := tpBottom;
      if vParams.Values['PageLayout'] = 'Top' then
        vPC.Properties.TabPosition := tpTop;
      vPC.Properties.CloseButtonMode := cbmActiveTab;
      vPC.OnCanClose := OnPCCanClose;
      vPC.Align := ALayout.Align;
      vPC.Anchors := ALayout.Anchors;
      vPC.Font.Assign(ALayout.Font);
      Result := TDevExpressArea.Create(Self, AView, Trim(ALayout.Caption), False, vPC);

      // Здесь можно подкорректировать параметры
      if StrToBoolDef(vDomain.UserSettings.GetValue('Core', 'ShowStartPage'), True) then
      begin
        vStartPageStr := vDomain.Settings.GetValue('Core', 'StartPage', '');

        vStartPageName := GetUrlCommand(vStartPageStr);
        if (vStartPageName <> '') and FileExists(vDomain.Configuration.FindLayoutFile(vStartPageName, LAYOUT_DFM_EXT)) then
        begin
          vParams.Values['Layout'] := vStartPageName;
          vParams.Values['View'] := '';
          vParams.Values['Options'] := DecodeUrl(GetUrlParam(vStartPageStr, 'Options', ''));
        end;
      end;

      FUIBuilder.PagedArea := Result;
    end
    else
    begin
      vPanel := TPanel.Create(nil);
      vPanel.Width := ALayout.Width;
      vPanel.Height := ALayout.Height;
      vPanel.Left := ALayout.Left;
      vPanel.Top := ALayout.Top;
      vPanel.Align := ALayout.Align;
      vPanel.AlignWithMargins := ALayout.AlignWithMargins;
      vPanel.Margins := ALayout.Margins;
      vPanel.Padding := ALayout.Padding;
      if AView.DefinitionKind <> dkListField then
        vPanel.BevelOuter := TBevelCut(ALayout.ExtractInteger('BevelOuter'))
      else
        vPanel.BevelOuter := bvNone;
      vPanel.Anchors := ALayout.Anchors;
      if not ALayout.ExtractBoolean('ParentBackground') then
      begin
        vPanel.Color := ALayout.Color;
        vPanel.ParentColor := False;
        vPanel.ParentBackground := False;
      end;
      Result := TDevExpressArea.Create(Self, AView, Trim(ALayout.Caption), False, vPanel, nil, AParams);
    end;

    Result.AddParams(vParams);
  end
  else if ALayout.LayoutClass = 'TScrollBox' then
  begin
    vBox := TcxScrollBox.Create(nil);
    vBox.Width := ALayout.Width;
    vBox.Height := ALayout.Height;
    vBox.Left := ALayout.Left;
    vBox.Top := ALayout.Top;
    vBox.Align := ALayout.Align;
    vBox.AlignWithMargins := ALayout.AlignWithMargins;
    vBox.Margins := ALayout.Margins;
    vBox.Padding := ALayout.Padding;
    vBox.LookAndFeel.ScrollbarMode := sbmClassic;
    if ALayout.ExtractInteger('BorderStyle') = Ord(bsNone) then
      vBox.BorderStyle := cxcbsNone;
    vBox.Anchors := ALayout.Anchors;
    Result := TDevExpressArea.Create(Self, AView, '', False, vBox);
  end
  else if ALayout.LayoutClass = 'TMemo' then
  begin
    vPanel := TPanel.Create(nil);
    vPanel.Width := ALayout.Width;
    vPanel.Height := ALayout.Height;
    vPanel.Left := ALayout.Left;
    vPanel.Top := ALayout.Top;
    vPanel.Align := ALayout.Align;
    vPanel.AlignWithMargins := ALayout.AlignWithMargins;
    vPanel.Margins := ALayout.Margins;
    vPanel.Padding := ALayout.Padding;
    if AView.DefinitionKind <> dkListField then
      vPanel.BevelOuter := TBevelCut(ALayout.ExtractInteger('BevelOuter'))
    else
      vPanel.BevelOuter := bvNone;
    vPanel.Anchors := ALayout.Anchors;
    if not ALayout.ExtractBoolean('ParentBackground') then
    begin
      vPanel.Color := ALayout.Color;
      vPanel.ParentColor := False;
      vPanel.ParentBackground := False;
    end;
    Result := TDevExpressArea.Create(Self, AView, Trim(ALayout.Caption), False, vPanel);
  end
  else
    Assert(False, 'Класс [' + ALayout.LayoutClass + '] не поддерживается для создания лэйаутов');
end;

function TDevExpressArea.GetNavigationAreaClass: TNavigationAreaClass;
begin
  Result := TDevExpressNavBarArea;
end;

function TDevExpressArea.GetSelectedIndexFromControl(const AControl: TComponent; const AMenuItem: TMenuItem): Integer;
begin
  Result := -1;
  if not Assigned(TcxButton(AControl).DropDownMenu) then
    Exit;
  Result := TPopupMenu(TcxButton(AControl).DropDownMenu).Items.IndexOf(AMenuItem);
end;

procedure TDevExpressArea.ProcessFrame(const AFrame: TBaseLayout);
begin
  if (AFrame.Hint <> '') and (FControl is TcxTabSheet) then
  begin
    TcxTabSheet(FControl).Caption := AFrame.Hint;
    if Pos('=', AFrame.Hint) > 0 then // Hint содержит url-строку с параметрами
    begin
      TcxTabSheet(FControl).Caption := GetUrlParam(AFrame.Hint, 'Caption', '');
      TcxTabSheet(FControl).ImageIndex := GetImageId(StrToIntDef(GetUrlParam(AFrame.Hint, 'ImageIndex', ''), -1));
    end;
  end;
end;

procedure TDevExpressArea.SetViewState(const AValue: TViewState);
var
  vBoolValue: Boolean;
begin
  if (FControl is TControl) and (not FIsForm) then
  begin
    vBoolValue := (AValue > vsHidden) and FStyle.Visible;

    if Control.Visible <> vBoolValue then
      Control.Visible := vBoolValue;

    vBoolValue := (AValue > vsDisabled) and FStyle.Enabled;
    if FControl is TcxTabSheet then
      TcxTabSheet(FControl).AllowCloseButton := vBoolValue
    else if Control.Enabled <> vBoolValue then
      Control.Enabled := vBoolValue;
  end
  else
    inherited;
end;

procedure TDevExpressArea.OnPCCanClose(Sender: TObject; var ACanClose: Boolean);
var
  vPC: TcxPageControl;
  vTab: TcxTabSheet;
  vPCArea: TUIArea;
  vTabArea: TUIArea;
begin
  // Closing will be done using TUIArea mechanics
  ACanClose := False;

  vPC := TcxPageControl(Sender);
  vTab := vPC.ActivePage;
  if vTab = nil then
    Exit;

  FUIBuilder.LastArea := nil;

  vPCArea := TUIArea(vPC.Tag);
  vTabArea := TUIArea(vTab.Tag);

  vPCArea.RemoveArea(vTabArea);

  FUIBuilder.PrintHierarchy;
end;

{ TNavBarArea }

constructor TDevExpressNavBarArea.Create(const ASource: TBaseLayout; const AView: TView; const AArea: TCustomVCLArea; const AInteractor: TObject; const AParams: string);
begin
  inherited;

  FNavBar := TdxNavBar.Create(nil);
  FNavBar.DoubleBuffered := True;
  FNavBar.Width := ASource.Width;
  FNavBar.Height := ASource.Height;
  FNavBar.Left := ASource.Left;
  FNavBar.Top := ASource.Top;
  FNavBar.SmallImages := TDragImageList(TInteractor(AInteractor).Images[16]);
  FNavBar.LargeImages := TDragImageList(TInteractor(AInteractor).Images[32]);
  FNavBar.View := StrToIntDef(GetUrlParam(AParams, 'NavBarKind'), 4);
  FNavBar.OptionsStyle.DefaultStyles.GroupHeader.Font.Assign(ASource.Font);
  FNavBar.OptionsStyle.DefaultStyles.GroupHeader.Font.Size := 12;
  FNavBar.OptionsStyle.DefaultStyles.Item.Font.Assign(ASource.Font);
  FNavBar.OptionsStyle.DefaultStyles.Background.BackColor := ASource.Color;
  FNavBar.OptionsStyle.DefaultStyles.Background.BackColor2 := DimColor(ASource.Color, 0.2);
  FNavBar.OptionsStyle.DefaultStyles.GroupBackground.BackColor := ASource.Color;
  FNavBar.OptionsStyle.DefaultStyles.GroupBackground.BackColor2 := DimColor(ASource.Color, 0.2);
  FNavBar.OptionsStyle.DefaultStyles.GroupHeader.BackColor := ASource.Color;
  FNavBar.OptionsStyle.DefaultStyles.GroupHeader.BackColor2 := DimColor(ASource.Color, 0.2);
  FNavBar.DragDropFlags := [];

  FNavBarGroup := nil;
  FNavBarItem := nil;
end;

destructor TDevExpressNavBarArea.Destroy;
begin
  FreeAndNil(FNavBar);
  inherited;
end;

procedure TDevExpressNavBarArea.DoAssignItemOnClick(const AHandler: TNotifyEvent);
begin
  if FCurrentLevel = 0 then
    FNavBarGroup.OnClick := AHandler
  else
    FNavBarItem.OnClick := AHandler;
end;

procedure TDevExpressNavBarArea.DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer);
begin
  if FCurrentLevel = 0 then
  begin
    FNavBarGroup.Caption := ACaption;
    FNavBarGroup.Hint := AHint;
    FNavBarGroup.SmallImageIndex := AImageIndex;
    FNavBarGroup.LargeImageIndex := AImageIndex;
  end
  else
  begin
    FNavBarItem.Caption := ACaption;
    FNavBarItem.Hint := AHint;
    FNavBarItem.SmallImageIndex := AImageIndex;
    FNavBarItem.LargeImageIndex := AImageIndex;
  end;
end;

function TDevExpressNavBarArea.DoCreateItem(const AParentObj: TObject; const AParams: string): TObject;
begin
  if FCurrentLevel = 0 then
  begin
    FNavBarGroup := FNavBar.Groups.Add;
    FNavBarGroup.LinksUseSmallImages := False;
    FNavBarGroup.OptionsExpansion.Expandable := False;
    FNavBarGroup.OptionsExpansion.ShowExpandButton := False;
    Result := FNavBarGroup;
  end
  else
  begin
    FNavBarItem := FNavBar.Items.Add;
    if Assigned(AParentObj) and (AParentObj is TdxNavBarGroup) then
      TdxNavBarGroup(AParentObj).CreateLink(FNavBarItem);
    Result := FNavBarItem;
  end;
end;

function TDevExpressNavBarArea.GetControl: TObject;
begin
  Result := FNavBar;
end;

{ TDevExpressFieldArea }

procedure TDevExpressFieldArea.AssignFont(const AFont: TFont);
begin
  if FControl is TcxCustomEdit then
    TcxCustomEdit(FControl).Style.Font.Assign(AFont)
  else
    TCrackedControl(FControl).Font.Assign(AFont);
end;

function TDevExpressFieldArea.DoCreateChildArea(const ALayout: TBaseLayout; const AView: TView; const AParams: string; const AOnClose: TProc): TUIArea;
var
  vForm: TForm;
  vTab: TcxTabSheet;
  vStartPageName: string;
begin
  Result := nil;

  if not Assigned(ALayout) then
    Exit;

  if ALayout.LayoutClass = 'TTabSheet' then
  begin
    if (TInteractor(AView.Interactor).Layout = 'mdi') and (ALayout.Tag = 11) then
    begin
      vForm := TForm.Create(nil);
      vForm.Caption := ALayout.Caption;
      vForm.Position := poDefault;
      vForm.FormStyle := fsMDIChild;
      vForm.OnClose := OnCloseMDIForm;
      vForm.ShowHint := True;
      if AView.DefinitionKind in [dkCollection, dkAction, dkEntity] then
        TDragImageList(TInteractor(Interactor).Images[16]).GetIcon(GetImageID(TDefinition(AView.Definition)._ImageID), vForm.Icon);
      Result := TDevExpressArea.Create(Self, AView, ALayout.Name, False, vForm);
      TCustomVCLArea(Result).OnClose := AOnClose;
    end
    else
    begin
      vTab := TcxTabSheet.Create(Component);
      vTab.Caption := ALayout.Caption;
      vTab.ImageIndex := GetImageID(ALayout.ExtractInteger('ImageIndex'));

      vStartPageName := TDomain(FView.Domain).Settings.GetValue('Core', 'StartPage', '');
      vTab.AllowCloseButton := not SameText(ALayout.Name, vStartPageName);
      vTab.TabVisible := ALayout.ExtractBoolean('TabVisible');

      Result := TDevExpressArea.Create(Self, AView, ALayout.Name, False, vTab);
    end;
  end;
end;

end.
