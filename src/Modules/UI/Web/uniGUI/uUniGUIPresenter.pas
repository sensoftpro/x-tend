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

unit uUniGUIPresenter;

interface

uses
  Messages, Classes, Generics.Collections, Generics.Defaults, ActnList, uConsts, StdCtrls, Buttons,
  ExtCtrls, TypInfo, Types, ComCtrls, SysUtils, Windows, Graphics,
  Variants, Vcl.Controls, Forms, Mask, Menus,

  uniGUIServer, uniGUIMainModule, uniGUIApplication, uIdCustomHTTPServer, uniGUIClasses, uniGUITypes,
  uniGUIForm, uniEdit, uniGUIBaseClasses, uniLabel, uniButton, uniImageList, uniPanel, uniPageControl,
  uniGUIFont, uniImage,

  uSettings, uDefinition, uPresenter, uInteractor, uView, uUIBuilder, uLayout, uEntity;

type
  TImageResolution = (ir16x16, ir24x24, ir32x32);

type
  // Service forms of uniGUI

  TUniServerModule = class(TUniGUIServerModule)
  protected
    procedure FirstInit; override;
  end;

  TUniMainModule = class(TUniGUIMainModule)
  end;

  TUniMainForm = class(TUniForm)
    procedure UniFormCreate(Sender: TObject);
    procedure UniFormDestroy(Sender: TObject);
  private
    FInteractor: TInteractor;
  end;

type
  TUniGUIPresenter = class(TPresenter)
  private
    FServerModule: TUniServerModule;
    procedure UniServerModuleBeforeInit(Sender: TObject);
    function FormInstance: TUniMainForm;
  protected
    procedure OnShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure OnChildFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnMenuItemCheck(Sender: TObject);
  protected
    procedure DoRun(const AParameter: string); override;
    {}procedure DoUnfreeze; override;
    {}procedure DoStop; override;

    procedure DoLogin(const ADomain: TObject); override;
    procedure DoLogout(const AInteractor: TInteractor); override;

    function GetNativeControlClass: TNativeControlClass; override;
    procedure DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType); override;
    procedure DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet; const AOnClose: TCloseProc = nil); override;
    procedure DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False); override;
    procedure DoShowOpenDialog(const AFileName: string; const ATitle, AFilter,
      ADefaultExt, ADefaultDir: string; const AOnClose: TCloseTextProc = nil); override;
    procedure DoShowSaveDialog(const AFileName: string; const ATitle, AFilter,
      ADefaultExt: string; const AOnClose: TCloseTextProc = nil); override;
    {}procedure DoSetCursor(const ACursorType: TCursorType); override;
    procedure DoCloseAllPages(const AInteractor: TInteractor); override;
    function CreateControl(const AParent: TUIArea; const AView: TView; const ALayout: TLayout;
      const AParams: string = ''): TObject; override;

    function GetImagePlaceholder(const ASize: Integer): TStream; override;
    function DoCreateImages(const ADomain: TObject; const AImages: TImages; const ASize: Integer): TObject; override;

    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); override;
  public
    procedure ShowUIArea(const AArea: TUIArea; const AAreaName: string; const ACaption: string); override;
  end;

procedure CopyFontSettings(const AFont: TUniFont; const ALayout: TLayout);
procedure CopyMargins(const AControl: TUniControl; const ALayout: TLayout);
procedure CopyPadding(const AControl: TUniControl; const ALayout: TLayout);
procedure CopyConstraints(const AConstraints: TSizeConstraints; const ALayout: TLayout);

implementation

uses
  Math, PngImage, UITypes, IOUtils, UniGUIVars, uniArea,
  uniScrollBox, uniMainMenu, uniSplitter,
  uPlatform, uIcon, uModule, uConfiguration, uDomain, uEntityList, uSession, uUtils;

{$R ServerModule.dfm}
{$R MainModule.dfm}
{$R Main.dfm}

type
  TCrackedUniControl = class(TUniControl) end;
  TCrackedNativeControlHolder = class(TNativeControlHolder) end;

procedure CopyFontSettings(const AFont: TUniFont; const ALayout: TLayout);
begin
  AFont.Name := ALayout.Font.Family;
  AFont.Color := AlphaColorToColor(ALayout.Font.Color);
  AFont.Size := ALayout.Font.Size;
  AFont.Style := ALayout.Font.Style;
end;

procedure CopyMargins(const AControl: TUniControl; const ALayout: TLayout);
begin
  if not ALayout.Margins.IsEmpty then
  begin
    AControl.AlignWithMargins := True;
    AControl.Margins.Left := ALayout.Margins.Left;
    AControl.Margins.Top := ALayout.Margins.Top;
    AControl.Margins.Right := ALayout.Margins.Right;
    AControl.Margins.Bottom := ALayout.Margins.Bottom;
  end
  else
    AControl.AlignWithMargins := False;
end;

procedure CopyPadding(const AControl: TUniControl; const ALayout: TLayout);
begin
  AControl.Padding.Left := ALayout.Padding.Left;
  AControl.Padding.Top := ALayout.Padding.Top;
  AControl.Padding.Right := ALayout.Padding.Right;
  AControl.Padding.Bottom := ALayout.Padding.Bottom;
end;

procedure CopyConstraints(const AConstraints: TSizeConstraints; const ALayout: TLayout);
begin
  AConstraints.MinWidth := ALayout.Constraints.MinWidth;
  AConstraints.MinHeight := ALayout.Constraints.MinHeight;
  AConstraints.MaxWidth := ALayout.Constraints.MaxWidth;
  AConstraints.MaxHeight := ALayout.Constraints.MaxHeight;
end;

{ TUniServerModule }

procedure TUniServerModule.FirstInit;
begin
  InitServerModule(Self);
  OnBeforeInit := TUniGUIPresenter(_Platform.Presenter).UniServerModuleBeforeInit;
  if TFile.Exists('CustomStyles.css') then
    Self.CustomFiles.Add('CustomStyles.css');
end;

{ TUniMainForm }

procedure TUniMainForm.UniFormCreate(Sender: TObject);
var
  vPresenter: TUniGUIPresenter;
  vDomain: TDomain;
  vSession: TUserSession;
  vMainFormName: string;
  vView: TView;
  vUIArea: TUIArea;
  vParams: TStrings;
  vViewName: string;
  vLayoutName: string;
  vFormLayout: TLayout;

  function ExtractValueFromStrings(const AList: TStrings; const AKey: string; const ADefault: string = ''): string;
  begin
    if AList.IndexOfName(AKey) >= 0 then
      Result := AList.Values[AKey]
    else
      Result := ADefault;
  end;

begin
  vDomain := _Platform.Domains[0];
  vSession := vDomain.Sessions.AddSession(nil);
  vPresenter := TUniGUIPresenter(_Platform.Presenter);

  vPresenter.SetApplicationUI(vDomain.AppTitle, vDomain.Configuration.IconFileName);

  FInteractor := TInteractor.Create(vPresenter, vSession);

  vView := FInteractor.RootView;

  vMainFormName := vDomain.Settings.GetValue('Core', 'MainForm', '');
  if Trim(vMainFormName) = '' then
    vMainFormName := 'MainForm';

  //if FInteractor.Layout = 'mdi' then
  //  PageMode := True;

  OnClose := vPresenter.DoMainFormClose;
  //OnShow := vPresenter.DoMainFormShow;
  Position := poScreenCenter;
  Caption := vDomain.AppTitle + ' (' + vSession.CurrentUserName + ')';
  ShowHint := True;

  vFormLayout := vDomain.UIBuilder.Layouts.CreateSimpleLayout(lkFrame);
  vFormLayout.Caption := Caption;
  vFormLayout.StyleName := '';
  vFormLayout.AreaKind := akForm;

  vUIArea := TUIArea.Create(nil, vView, vFormLayout);
  TCrackedNativeControlHolder(vUIArea.NativeControl).SetControl(Self);
  FInteractor.RootArea := vUIArea;
  FInteractor.CurrentArea := vUIArea;
  FInteractor.UIBuilder.ApplyLayout(vUIArea, vView, vMainFormName, '');

  if FInteractor.DefaultParams <> '' then
  begin
    vParams := CreateDelimitedList(FInteractor.DefaultParams, '&');
    try
      vViewName := ExtractValueFromStrings(vParams, 'View');
      vLayoutName := ExtractValueFromStrings(vParams, 'Layout');
      if (vViewName <> '') or (vLayoutName <> '') then
        FInteractor.UIBuilder.Navigate(vView.BuildView(vViewName), 'WorkArea',
          vLayoutName, '', nil, ExtractValueFromStrings(vParams, 'Caption'));
    finally
      FreeAndNil(vParams);
      FInteractor.DefaultParams := '';
    end;
  end;
end;

procedure TUniMainForm.UniFormDestroy(Sender: TObject);
begin
  if Assigned(FInteractor) and Assigned(FInteractor.Presenter) then
    TPresenter(FInteractor.Presenter).Logout(FInteractor);
end;

{ TUniGUIPresenter }

function TUniGUIPresenter.CreateControl(const AParent: TUIArea; const AView: TView;
  const ALayout: TLayout; const AParams: string): TObject;
var
  vDomain: TDomain;
  vInteractor: TInteractor;
  vUIBuilder: TUIBuilder;
  vStartPageName: string;
  vForm: TUniForm;
  //vShape: TShape;
  vLabel: TUniLabel;
  vImage: TUniImage;
  vPC: TUniPageControl;
  vTab: TUniTabSheet;
  vPanel: TUniPanel;
  vParams: TStrings;
  vBox: TUniScrollBox;
  //vBevel: TBevel;
  vSplitter: TUniSplitter;
  vMenu: TUniPopupMenu;
  vMenuItem: TUniMenuItem;
  i: Integer;
  vArea: TUIArea;
  vParentControl: TObject;
  vOwner: TComponent;
begin
  Result := nil;

  vInteractor := TInteractor(AView.Interactor);
  vUIBuilder := vInteractor.UIBuilder;
  vDomain := TDomain(vInteractor.Domain);
  vParentControl := GetRealControl(AParent);
  vOwner := TComponent(GetRealControl(AParent));

  if ALayout.Kind = lkShape then
  begin
    vPanel := TUniPanel.Create(vOwner);
    vPanel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vPanel.Anchors := ALayout.Anchors;
    vPanel.Align := TAlign(ALayout.Align);
    vPanel.Hint := ALayout.Hint;
    vPanel.Visible := ALayout.State > vsHidden;
    vPanel.Color := AlphaColorToColor(ALayout.Brush.Color);
    vPanel.ParentColor := False;
    vPanel.ParentBackground := False;
    CopyMargins(vPanel, ALayout);
    CopyConstraints(vPanel.Constraints, ALayout);

    Result := vPanel;
  end
  else if ALayout.Kind = lkLabel then
  begin
    vLabel := TUniLabel.Create(vOwner);
    vLabel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vLabel.Transparent := ALayout.Transparent;
    vLabel.AutoSize := ALayout.AutoSize;
    //vLabel.WordWrap := ALayout.WordWrap;
    CopyFontSettings(vLabel.Font, ALayout);
    vLabel.Anchors := ALayout.Anchors;

    if (ALayout.Caption  = '$') and (ALayout.UIParams = 'Caption') then
    begin
      if AView.DefinitionKind = dkCollection then
        vLabel.Caption := AParent.GetTranslation(TDefinition(AView.Definition))
      //else if AView.DefinitionKind in [dkListField..dkComplexField] then
      //  vLabel.Caption := AParent.GetTranslation(TFieldDef(AView.Definition))
      else
        vLabel.Caption := ALayout.Caption;
    end
    else
      vLabel.Caption := ALayout.Caption;

    Result := vLabel;
  end
  else if ALayout.Kind = lkImage then
  begin
    vImage := TUniImage.Create(vOwner);
    vImage.ControlStyle := vImage.ControlStyle + [csOpaque];
    vImage.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vImage.Anchors := ALayout.Anchors;
    vImage.Align := TAlign(ALayout.Align);
    vImage.Hint := ALayout.Hint;
    CopyMargins(vImage, ALayout);
    vImage.Transparent := ALayout.Transparent;
    vImage.AutoSize := ALayout.AutoSize;

    if Assigned(ALayout.Image_Picture) then
    begin
      ALayout.Image_Picture.Position := 0;
      vImage.LoadFromStream(ALayout.Image_Picture);
    end;

    vImage.Stretch := ALayout.Image_Stretch;
    vImage.Proportional := ALayout.Image_Proportional;
    vImage.Center := ALayout.Image_Center;

    Result := vImage;
  end
  else if ALayout.Kind = lkPages then
  begin
    vPC := TUniPageControl.Create(vOwner);
    vPC.DoubleBuffered := True;
    vPC.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    //vPC.TabPosition := TTabPosition(ALayout.Page_Position);
    //vPC.TabHeight := ALayout.Page_Height;
    //vPC.TabWidth := ALayout.Page_Width;
    vPC.Anchors := ALayout.Anchors;
    //CopyFontSettings(vPC.Font, ALayout);
    vPC.Align := TAlign(ALayout.Align);
    CopyMargins(vPC, ALayout);
    vPC.Visible := ALayout.State > vsHidden;
    vPC.Enabled := ALayout.State > vsDisabled;

    Result := vPC;
  end
  else if ALayout.Kind = lkPage then
  begin
    ALayout.Id := ALayout.Name;

    if vUIBuilder.IsMDIStyle and (ALayout.Tag = 11) then
    begin
      vForm := TUniForm.Create(vOwner);
      vForm.Caption := ALayout.Caption;
      vForm.Position := poDefault;
      //vForm.FormStyle := fsMDIChild;
      vForm.OnClose := OnCloseMDIForm;
      vForm.ShowHint := True;
      if AView.DefinitionKind in [dkCollection, dkAction, dkEntity] then
        TUniCustomImageList(vUIBuilder.Images[16]).GetIcon(AParent.GetImageIndex(TDefinition(AView.Definition)._ImageID), vForm.Icon);

      Result := vForm;
    end
    else begin
      vTab := TUniTabSheet.Create(vOwner);
      vTab.Caption := ALayout.Caption;
      vTab.ImageIndex := AParent.GetImageIndex(ALayout.ImageID);

      vStartPageName := vDomain.Settings.GetValue('Core', 'StartPage', '');
      vTab.Parent := TWinControl(vParentControl);
      vTab.TabVisible := ALayout.ShowCaption;
      if vParentControl is TUniPageControl then
        vTab.PageControl := TUniPageControl(vParentControl);

      if AParent = vInteractor.PagedArea then
      begin
        vTab.Closable := True;
        vTab.OnClose := OnTabClose;
      end;

      Result := vTab;
    end;
  end
  else if ALayout.Kind = lkBevel then
  begin
    vPanel := TUniPanel.Create(vOwner);
    vPanel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vPanel.Anchors := ALayout.Anchors;
    vPanel.Align := TAlign(ALayout.Align);
    CopyMargins(vPanel, ALayout);

    case ALayout.Bevel_Style of
      lbsLowered: vPanel.BorderStyle := TUniBorderStyle.ubsFrameLowered;
      lbsRaised: vPanel.BorderStyle := TUniBorderStyle.ubsFrameRaised;
    end;

    ALayout.Id := '-bevel-';
    Result := vPanel;
  end
  else if ALayout.Kind = lkSplitter then
  begin
    vSplitter := TUniSplitter.Create(vOwner);
    vSplitter.Align := TAlign(ALayout.Align);
    vSplitter.Cursor := ALayout.Cursor;
    vSplitter.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vSplitter.Color := AlphaColorToColor(ALayout.Color);
    vSplitter.ParentColor := False;

    ALayout.Id := '-splitter-';
    Result := vSplitter;
  end
  else if ALayout.Kind = lkPanel then
  begin
    if AParams <> '' then
      vParams := CreateDelimitedList(AParams, '&')
    else
      vParams := nil;

    ALayout.Id := Trim(ALayout.Caption);

    if Assigned(vParams) and (vParams.Values['ViewType'] = 'Paged') then
    begin
      if vUIBuilder.IsMDIStyle then
      begin
        TInteractor(AParent.Interactor).DefaultParams := AParams;
        FreeAndNil(vParams);
        Exit(nil);
      end;

      vPC := TUniPageControl.Create(vOwner);
      vPC.DoubleBuffered := True;
      vPC.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
      vPC.Images := TUniCustomImageList(vUIBuilder.Images[16]);
      if vParams.Values['PageLayout'] = 'Top' then
      else begin
        vPC.ClientEvents.Enabled := True;
        vPC.ClientEvents.UniEvents.Append(
          'tabPanel.beforeInit=function tabPanel.beforeInit(sender, config){config.tabPosition = ''bottom''}');
      end;
      vPC.Align := TAlign(ALayout.Align);
      vPC.Anchors := ALayout.Anchors;

      ALayout.Name := '-pages-';
      Result := vPC;
    end
    else
    begin
      vPanel := TUniPanel.Create(vOwner);
      vPanel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
      vPanel.Anchors := ALayout.Anchors;
      vPanel.Align := TAlign(ALayout.Align);
      CopyMargins(vPanel, ALayout);
      CopyFontSettings(vPanel.Font, ALayout);
      CopyPadding(vPanel, ALayout);
      if AView.DefinitionKind <> dkListField then
      begin
        if ALayout.BorderStyle = lbsSingle then
          vPanel.BorderStyle := TUniBorderStyle.ubsOutset
        else
          vPanel.BorderStyle := TUniBorderStyle.ubsNone;
      end
      else
        vPanel.BorderStyle := TUniBorderStyle.ubsNone;
      vPanel.Color := AlphaColorToColor(ALayout.Color);
      vPanel.ParentColor := False;
      vPanel.ParentBackground := False;

      Result := vPanel;
    end;

    FreeAndNil(vParams);
  end
  else if ALayout.Kind = lkScrollBox then
  begin
    vBox := TUniScrollBox.Create(vOwner);
    vBox.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vBox.Anchors := ALayout.Anchors;
    vBox.Align := TAlign(ALayout.Align);
    CopyMargins(vBox, ALayout);
    CopyPadding(vBox, ALayout);
    if ALayout.BorderStyle = lbsNone then
      vBox.BorderStyle := TUniBorderStyle.ubsNone;

    Result := vBox;
  end
  else if ALayout.Kind = lkMemo then
  begin
    vPanel := TUniPanel.Create(vOwner);
    vPanel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vPanel.Anchors := ALayout.Anchors;
    vPanel.Align := TAlign(ALayout.Align);
    CopyMargins(vPanel, ALayout);
    CopyPadding(vPanel, ALayout);
    CopyFontSettings(vPanel.Font, ALayout);
    if AView.DefinitionKind <> dkListField then
    begin
      if ALayout.BorderStyle = lbsSingle then
        vPanel.BorderStyle := TUniBorderStyle.ubsOutset
      else
        vPanel.BorderStyle := TUniBorderStyle.ubsNone;
    end
    else
      vPanel.BorderStyle := TUniBorderStyle.ubsNone;
    vPanel.Color := AlphaColorToColor(ALayout.Color);
    vPanel.ParentColor := False;
    vPanel.ParentBackground := False;

    ALayout.Id := Trim(ALayout.Caption);
    Result := vPanel;
  end
  else if ALayout.Kind = lkFrame then
  begin
    vForm := nil;

    if ALayout.StyleName = '' then
    begin
      vForm := nil;
    end
    // второстепенная автономная форма
    else if ALayout.StyleName = 'float' then
    begin
      //vArea := nil;
      if Assigned(vInteractor.RootArea) then
      begin
        for i := 0 to vInteractor.RootArea.Count - 1 do
        begin
          vArea := vInteractor.RootArea.Areas[i];
          if (vArea.View = AView) and (GetRealControl(vArea) is TUniForm) then
            Exit(vArea);
        end;
      end;

      vForm := TUniForm.Create(UniApplication);

      vForm.OnClose := DoFloatFormClose;
      vForm.Position := poScreenCenter;
      vForm.Font.Size := 12;
      vForm.Caption := ALayout.Caption;
      vForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      //if (AView.DefinitionKind in [dkCollection, dkAction, dkEntity]) then
      //  TDragImageList(vUIBuilder.Images[16]).GetIcon(vArea.GetImageID(TDefinition(AView.Definition)._ImageID), vForm.Icon);
    end
    // автономная форма со свободным отображением
    else if ALayout.StyleName = 'free' then
    begin
      vForm := nil;
      Exit;
      vForm.Position := poScreenCenter;
      vForm.Font.Size := 12;
      vForm.Caption := ALayout.Caption;
      vForm.BorderStyle := bsNone;
      vForm.BorderIcons := [];
      vForm.FormStyle := fsStayOnTop;
    end
    // дочерняя модальная форма
    else if (ALayout.StyleName = 'child') or (ALayout.StyleName = 'modal') then
    begin
      vForm := TUniForm.Create(UniApplication);
      vForm.OnClose := DoChildFormClose;
      vForm.OnKeyDown := OnChildFormKeyDown;
      vForm.KeyPreview := True;
      vForm.BorderIcons := [biSystemMenu];
      vForm.Position := poScreenCenter;
      vForm.Font.Size := 12;
      vForm.BorderStyle := bsSingle;  // for layouted form this property will be changed when assigned cEditFormResizable flag in Tag
    end;

    if vForm = nil then
      Exit(nil);

    vForm.Color := TColorRec.SysBtnFace;
    vForm.ShowHint := True;
    Assert(not Assigned(vForm.OnShow), 'vForm.OnShow already assigned');
    vForm.OnShow := DoOnFormShow;
    ALayout.Id := ALayout.StyleName;
    Result := vForm;
  end
  else if ALayout.Kind = lkAction then
  begin
    if (ALayout.StyleName = '') or (ALayout.StyleName = 'menu') then
    begin
      vMenu := TUniPopupMenu.Create(TComponent(vParentControl));
      vMenu.Images := TUniCustomImageList(vUIBuilder.Images[16]);
      ALayout.Name := '-popup-';
      Result := vMenu;
    end
    else begin
      vMenuItem := TUniMenuItem.Create(vOwner);
      if ALayout.StyleName = 'action' then
      begin
        vMenuItem.Caption := ALayout.Caption;
        vMenuItem.Hint := ALayout.Caption;
        vMenuItem.ImageIndex := AParent.GetImageIndex(ALayout.ImageID);
        vMenuItem.OnClick := AParent.OnAreaClick;

        if (AView.DefinitionKind = dkAction) and TDefinition(AView.Definition).FieldExists('IsChecked')
        then begin
          vMenuItem.AutoCheck := False;
          vMenuItem.CheckItem := True;
          vMenuItem.OnCheck := OnMenuItemCheck;
          vMenuItem.HideOnClick := True;
        end;

        if ALayout is TNavigationItem then
        begin
          vMenuItem.RadioItem := TNavigationItem(ALayout).RadioItem;
          vMenuItem.GroupIndex := TNavigationItem(ALayout).GroupIndex;
        end;
      end
      else if ALayout.StyleName = 'select' then
      begin
        vMenuItem.Caption := ALayout.Caption;
        vMenuItem.Hint := ALayout.Caption;
        vMenuItem.ImageIndex := AParent.GetImageIndex(ALayout.ImageID);
        vMenuItem.Tag := NativeInt(AParent);
        vMenuItem.OnClick := AParent.OnActionMenuSelected;
      end
      else
        vMenuItem.Caption := ALayout.Caption;

      if vParentControl is TUniPopupMenu then
        TUniPopupMenu(vParentControl).Items.Add(vMenuItem)
      else
        TUniMenuItem(vParentControl).Add(vMenuItem);

      if ALayout is TNavigationItem then
        ALayout.Id := TNavigationItem(ALayout).ViewName
      else
        ALayout.Id := ALayout.Caption;

      Result := vMenuItem;
    end;
  end
  else if ALayout.Kind <> lkNone then
    Result := nil
  else
    Assert(False, 'Пустой класс для лэйаута');
end;

function TUniGUIPresenter.DoCreateImages(const ADomain: TObject; const AImages: TImages; const ASize: Integer): TObject;
var
  vImageList: TUniNativeImageList;
  vStream: TStream;
  i: Integer;
begin
  vImageList := TUniNativeImageList.Create(nil);
  vImageList.Width := ASize;
  vImageList.Height := ASize;
  vImageList.DefaultOutputFormat := toPng;
  Result := vImageList;

  for i := 0 to AImages.Count - 1 do
  begin
    vStream := AImages[i];
    vStream.Position := 0;
    vImageList.AddImageStream(TCustomMemoryStream(vStream));
  end;
end;

function TUniGUIPresenter.GetImagePlaceholder(const ASize: Integer): TStream;
var
  vPlaceholder: TBitmap;
  vResDiv8: Integer;
begin
  vPlaceholder := TBitmap.Create;
  try
    vPlaceholder.SetSize(ASize, ASize);
    vPlaceholder.PixelFormat := pf32bit;
    vResDiv8 := Max(ASize div 8, 1);
    vPlaceholder.Canvas.Pen.Width := 1;
    vPlaceholder.Canvas.Pen.Color := TColorRec.Gray;
    vPlaceholder.Canvas.Rectangle(vResDiv8, vResDiv8, ASize - vResDiv8, ASize - vResDiv8);

    Result := TMemoryStream.Create;
    vPlaceholder.SaveToStream(Result);
  finally
    FreeAndNil(vPlaceholder);
  end;
end;

function TUniGUIPresenter.GetNativeControlClass: TNativeControlClass;
begin
  Result := TUniGUIControl;
end;

procedure TUniGUIPresenter.OnChildFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  vHandled: Boolean;
begin
  vHandled := False;
  DoChildFormKeyDown(Sender, Shift, Key, vHandled);
  if vHandled then
    Exit;
end;

procedure TUniGUIPresenter.OnMenuItemCheck(Sender: TObject);
var
  vMenuItem: TUniMenuItem;
  vMenu: TUniPopupMenu;
begin
  vMenuItem := TUniMenuItem(Sender);
  if vMenuItem.GetParentMenu is TUniPopupMenu then
    vMenu := TUniPopupMenu(vMenuItem.GetParentMenu)
  else
    vMenu := nil;
  vMenuItem.OnClick(Sender);
  vMenu.CloseMenu;
end;

procedure TUniGUIPresenter.OnShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  vShift: TShiftState;
begin
  Handled := False;
  vShift := [];
  if GetKeyState(VK_CONTROL) < 0 then
    Include(vShift, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then
    Include(vShift, ssAlt);
  if GetKeyState(VK_SHIFT) < 0 then
    Include(vShift, ssShift);
  DoProcessShortCut(vShift, Msg.CharCode, Handled);
  if Handled then
    Exit;
end;

procedure TUniGUIPresenter.DoLogin(const ADomain: TObject);
begin
  inherited DoLogin(ADomain);
end;

procedure TUniGUIPresenter.DoLogout(const AInteractor: TInteractor);
begin
  inherited DoLogout(AInteractor);
end;

procedure TUniGUIPresenter.ShowUIArea(const AArea: TUIArea;
  const AAreaName: string; const ACaption: string);
var
  vForm: TUniForm;
begin
  if (AAreaName = 'float') or (AAreaName = 'free') then
  begin
    vForm := TUniForm(GetRealControl(AArea));
    vForm.Show;
  end
  else if (AAreaName = 'child') or (AAreaName = 'modal') then
  begin
    vForm := TUniForm(GetRealControl(AArea));
    vForm.Caption := ACaption;
    vForm.ShowModal;
  end;
end;

procedure TUniGUIPresenter.UniServerModuleBeforeInit(Sender: TObject);
begin
  // Иконку в рантайме пока можно установить только так
  FServerModule := TUniServerModule(Sender);
  FServerModule.Favicon.LoadFromFile(_Platform.Domains[0].Configuration.IconFileName);

  //TUniServerModule(Sender).MainFormDisplayMode := mfPage;
end;

procedure TUniGUIPresenter.DoCloseAllPages(const AInteractor: TInteractor);
//var
//  i: Integer;
//  vMainForm: TForm;
begin
  if AInteractor.UIBuilder.IsMDIStyle then
  begin
    //vMainForm := TForm(GetNativeControl(AInteractor.RootArea));
    //if Assigned(vMainForm) and (vMainForm.FormStyle = fsMDIForm) then
    //  for i := vMainForm.MDIChildCount - 1 downto 0 do
    //    vMainForm.MDIChildren[i].Close;
  end;
end;

procedure TUniGUIPresenter.DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False);
begin
  // Показать содержимое файла в браузере или загрузить его для последующего открытия

//Есть проект который крутится на серваке, к нему обращаются пользователи снаружи, задача по нажатию на кнопку
//формировать отчет и открывать у пользователя в браузере, если есть плагин откроет страницу, нет - скачает и сохранит.
//Проблема в том что нельзя использовать ShellApi unit, потому что когда я вызываю гиперсылку, то отчет открывается
//на серваке а не на клиенте, что логично, Подскажите варианты запуска гиперссылки по кнопке. Спасибо.
//
//Используй CSS чтобы превратить ссылку в кнопку. Примеров полно, например http://webdesignerwall.com/tutorials/css3-gradient-buttons.
//Добавь параметр target=_blank в тег <a>, чтобы ссылка открывалась в новой вкладке/окне.
//Это самый простой вариант.

//  FFolder := FServerModule.FilesFolderPath+'PDFs\';
//  FUrl := FServerModule.FilesFolderURL+'pdfs/';
//  UniURLFrame1.URL := FUrl + <имя файла>;
//
//  function UniPDFForm: TUniPDFForm;
//  begin
//    Result := TUniPDFForm(UniMainModule.GetFormInstance(TUniPDFForm));
//  end;

// UniSession.BrowserWindow('http://google.com', 0, 0, '_blank');

//UniImage1.ClientEvents.ExtEvents ->
//
//function click(sender, eOpts)
//{
//    window.open('http://www.google.com', '_blank');
//}

//procedure TUniServerModule.UniGUIServerModuleCreate(Sender: TObject);
//begin
// MimeTable.AddMimeType('xls', 'Excel');
//end;
end;

procedure TUniGUIPresenter.DoRun(const AParameter: string);
begin
  Application.Title := cPlatformTitle;
  Application.Initialize;

  TUniServerModule.Create(Application);

  Application.OnShortCut := OnShortCut;

  Application.Run;
end;

procedure TUniGUIPresenter.DoSetCursor(const ACursorType: TCursorType);
begin
{ TODO -owa : Эта операция должна делаться открытием модальной формы со статусом }
end;

procedure TUniGUIPresenter.DoShowDialog(const ACaption, AText: string;
  const ADialogActions: TDialogResultSet; const AOnClose: TCloseProc);
var
  vButtons: TMsgDlgButtons;
begin
  vButtons := [];
  if drOk in ADialogActions then
    vButtons := vButtons + [mbOk];
  if drYes in ADialogActions then
    vButtons := vButtons + [mbYes];
  if drNo in ADialogActions then
    vButtons := vButtons + [mbNo];
  if drCancel in ADialogActions then
    vButtons := vButtons + [mbCancel];
  if vButtons = [] then
    Exit;

  FormInstance.MessageDlg(AText, mtConfirmation, vButtons, procedure(Sender: TComponent; AResult: Integer)
    var
      vRes: TDialogResult;
    begin
      if not Assigned(AOnClose) then
        Exit;

      if AResult = IDOK then
        vRes := drOk
      else if AResult = IDYES then
        vRes := drYes
      else if AResult = IDNO then
        vRes := drNo
      else if AResult = IDCANCEL then
        vRes := drCancel
      else
        vRes := drNone;

      AOnClose(vRes);
    end);
end;

procedure TUniGUIPresenter.DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType);
begin
  //FormInstance.ShowMessage(AText);
  case AMessageType of
    msInfo: FormInstance.MessageDlg(AText, mtInformation, [mbOk]);
    msWarning: FormInstance.MessageDlg(AText, mtWarning, [mbOk]);
    msError: FormInstance.MessageDlg(AText, mtError, [mbOk]);
  else
    FormInstance.ShowMessage(AText);
  end;
end;

procedure TUniGUIPresenter.DoShowOpenDialog(const AFileName: string; const ATitle,
  AFilter, ADefaultExt, ADefaultDir: string; const AOnClose: TCloseTextProc);
begin
//  Upload

// From UI
//  UniFileUpload1: TUniFileUpload;
//
//  if not DirectoryExists(UniServerModule.StartPath+'UploadFolder\') then
//    CreateDir(UniServerModule.StartPath+'UploadFolder\');
//
//  UniFileUpload1.Execute;
//  ...
//
//procedure UniFileUpload1 -> Completed(Sender: TObject;
//  AStream: TFileStream);
//var
//  DestName : string;
//  DestFolder : string;
//begin
//  if IsImage then
//  begin
//    UniImage1.Picture.LoadFromFile(AStream.FileName);
//    UniLabel3.Caption:='File Name: '+UniFileUpload1.FileName;
//  end
//  else
//  begin
//    DestFolder:=UniServerModule.StartPath+'UploadFolder\';
//    DestName:=DestFolder+ExtractFileName(UniFileUpload1.FileName);
//    UniLabel4.Caption:='File Name: '+UniFileUpload1.FileName;
//    CopyFile(PChar(AStream.FileName), PChar(DestName), False);
//    ShowMessage('File: '+UniFileUpload1.FileName+' Uploaded to folder: '+DestFolder);
//  end;
//end;

// Загрузка Drag-n-Drop
//procedure TUniFileUploadButton -> MultiCompleted(
//  Sender: TObject; Files: TUniFileInfoArray);
//var
//  I: Integer;
//begin
//  // process files after all files are uploaded
//  ClearPanel;
//  for I := Low(Files) to High(Files) do
//    if Assigned(Files[I].Stream) then
//      AddImage(Files[I].Stream);
//end;
end;

procedure TUniGUIPresenter.DoShowSaveDialog(const AFileName: string; const ATitle,
  AFilter, ADefaultExt: string; const AOnClose: TCloseTextProc);
begin
//  Download

// Загрузка через UniLabel
//  FName:='demo'+FormatDateTime('hhnnss', Time)+'.txt';
//   Через временную папку
//  UniMemo1.Lines.SaveToFile(UniServerModule.TempFolderPath+FName);
//  UniLabel1.Caption:='<a href="'+UniServerModule.TempFolderURL+FName+'" target=new>Click here to download: ('+FName+')</a>';
//   Через локальный кэш
//  UniMemo1.Lines.SaveToFile(UniServerModule.LocalCachePath+FName);
//  UniLabel1.Caption:='<a href="'+UniServerModule.LocalCacheURL+FName+'" target=new>Click here to download: ('+FName+')</a>';
//   Через глобальный кэш
//  UniMemo1.Lines.SaveToFile(UniServerModule.GlobalCachePath+FName);
//  UniLabel1.Caption:='<a href="'+UniServerModule.GlobalCacheURL+FName+'" target=new>Click here to download: ('+FName+')</a>';

// Прямая загрузка
//  UniSession.SendStream(S, 'Mydoc.txt');
//  UniSession.SendFile(UniServerModule.FilesFolderPath+ 'unidoc.doc', 'NewName.doc');
end;

procedure TUniGUIPresenter.DoStop;
begin
end;

procedure TUniGUIPresenter.DoUnfreeze;
begin
end;

function TUniGUIPresenter.FormInstance: TUniMainForm;
begin
  Result := TUniMainForm(TUniMainModule(UniApplication.UniMainModule).GetFormInstance(TUniMainForm));
end;

procedure TUniGUIPresenter.SetApplicationUI(const AAppTitle, AIconName: string);
begin
  FServerModule.Title := AAppTitle;
  { TODO -owa : Doesn't work, redo }
  // https://it-blackcat.blogspot.com/2020/04/variants-for-adding-favicon-in-uniGUI.html
  //if FileExists(AIconName) then
  //begin
  //  Application.Icon.LoadFromFile(AIconName);
  //  FServerModule.Favicon.LoadFromFile(AIconName);
  //end;

  // Настройка серверных сообщений
  FServerModule.ServerMessages.TerminateMessage := 'Вот и всё... До новых встреч!';
end;

initialization

RegisterServerModuleClass(TUniServerModule);
RegisterMainModuleClass(TUniMainModule);
RegisterAppFormClass(TUniMainForm);

TBaseModule.RegisterModule('UI', '', 'Web.UniGUI', TUniGUIPresenter);

end.
