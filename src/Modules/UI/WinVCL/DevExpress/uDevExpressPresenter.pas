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

unit uDevExpressPresenter;

interface

uses
  Messages, Classes, Generics.Collections, Generics.Defaults, ActnList, uConsts, StdCtrls, Buttons,
  ExtCtrls, TypInfo, Types, ComCtrls, SysUtils, Windows, Graphics,
  Variants, Controls, Forms, Mask, Menus,

  uDefinition, uPresenter, uWinVCLPresenter, uInteractor, uView, uSettings,

  StartForm, DebugInfoForm, SplashForm, uUIBuilder, uLayout;

type
  TDevExpressPresenter = class(TWinVCLPresenter)
  private
    FRowStyle: TObject;
  protected
    function GetNativeControlClass: TNativeControlClass; override;
    function DoCreateImages(const AInteractor: TInteractor; const AImages: TImages; const ASize: Integer): TObject; override;
  public
    constructor Create(const AName: string; const ASettings: TSettings); override;
    destructor Destroy; override;

    function CreateControl(const AParent: TUIArea; const AView: TView;
      const ALayout: TLayout; const AParams: string = ''): TObject; override;

    property RowStyle: TObject read FRowStyle;
  end;

implementation

uses
  Dialogs, Math, StrUtils, ShellAPI, UITypes, ActiveX, JPEG, PngImage,
  cxGraphics, dxGDIPlusClasses, cxImage, cxEdit, cxPC, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxMemo,
  cxLabel, cxTextEdit, cxButtons, cxScrollBox, cxControls, cxSplitter,

  uPlatform, uModule, uDomain, uUtils, uConfiguration, uChangeManager, uIcon, uEntity, uEntityList, uDEArea, uSession,
  uManagedForm, AboutForm, ReportConfigureForm, OptionsForm, LoginForm, FloatForm, uCollection;

const
  cPCNavigatorFlag = 1;
  cServiceAreaHeight = 44;

type
  TCrackedWinControl = class(TWinControl) end;
  TCrackedControl = class(TControl) end;
  TCrackedArea = class(TUIArea) end;

{ TDevExpressPresenter }

constructor TDevExpressPresenter.Create(const AName: string; const ASettings: TSettings);
begin
  inherited Create(AName, ASettings);

  FRowStyle := TcxStyle.Create(nil);
end;

function TDevExpressPresenter.CreateControl(const AParent: TUIArea;
  const AView: TView; const ALayout: TLayout; const AParams: string): TObject;
var
  vStartPageName: string;
  vLabel: TcxLabel;
  vImage: TcxImage;
  vPC: TcxPageControl;
  vTab: TcxTabSheet;
  vParams: TStrings;
  vBox: TcxScrollBox;
  vSplitter: TcxSplitter;
begin
  Result := nil;

  ALayout.Id := '';

  if ALayout.Kind = lkLabel then
  begin
    vLabel := TcxLabel.Create(nil);
    vLabel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vLabel.Transparent := ALayout.Transparent;
    vLabel.AutoSize := ALayout.AutoSize;
    vLabel.Properties.WordWrap := ALayout.WordWrap;
    CopyFontSettings(vLabel.Style.Font, ALayout);
    vLabel.Anchors := ALayout.Anchors;

    if (ALayout.Caption  = '$') and (ALayout.UIParams = 'Caption') then
    begin
      if AView.DefinitionKind = dkCollection then
        vLabel.Caption := AParent.GetTranslation(TDefinition(AView.Definition))
      else if AView.DefinitionKind in [dkListField..dkComplexField] then
        vLabel.Caption := AParent.GetFieldTranslation(TFieldDef(AView.Definition))
      else
        vLabel.Caption := ALayout.Caption;
    end
    else
      vLabel.Caption := ALayout.Caption;

    Result := vLabel;
  end
  else if ALayout.Kind = lkImage then
  begin
    vImage := TcxImage.Create(nil);
    vImage.Style.BorderStyle := ebsNone;
    vImage.ControlStyle := vImage.ControlStyle + [csOpaque];
    vImage.Properties.ShowFocusRect := False;
    vImage.Properties.PopupMenuLayout.MenuItems := [];
    vImage.Properties.FitMode := ifmNormal;
    vImage.DoubleBuffered := True;
    vImage.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vImage.Anchors := ALayout.Anchors;
    vImage.Align := TAlign(ALayout.Align);
    vImage.Hint := ALayout.Hint;
    CopyMargins(vImage, ALayout);
    vImage.Transparent := ALayout.Transparent;
    vImage.AutoSize := ALayout.AutoSize;

    SetPictureFromStream(vImage.Picture, ALayout);
    vImage.Properties.Stretch := ALayout.Image_Stretch;
    vImage.Properties.Proportional := ALayout.Image_Proportional;
    vImage.Properties.Center := ALayout.Image_Center;

    Result := vImage;
  end
  else if ALayout.Kind = lkPages then
  begin
    vPC := TcxPageControl.Create(nil);
    vPC.DoubleBuffered := True;
    if (ALayout.Tag and cPCNavigatorFlag) > 0 then
    begin
      vPC.Properties.Rotate := True;
      vPC.Properties.Images := TDragImageList(TInteractor(AView.Interactor).Images[32]);
      vPC.Properties.TabCaptionAlignment := taLeftJustify;
    end
    else
    begin
      vPC.Properties.Options := vPC.Properties.Options + [pcoTopToBottomText];
      vPC.LookAndFeel.NativeStyle := False;
      vPC.LookAndFeel.Kind := lfUltraFlat;
    end;

    vPC.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vPC.Properties.TabPosition := TcxTabPosition(ALayout.Page_Position);
    vPC.Properties.TabHeight := ALayout.Page_Height;
    vPC.Properties.TabWidth := ALayout.Page_Width;
    vPC.Anchors := ALayout.Anchors;
    CopyFontSettings(vPC.Font, ALayout);
    vPC.Align := TAlign(ALayout.Align);
    CopyMargins(vPC, ALayout);
    vPC.Visible := ALayout.State > vsHidden;
    vPC.Enabled := ALayout.State > vsDisabled;

    Result := vPC;
  end
  else if ALayout.Kind = lkPage then
  begin
    if (TInteractor(AView.Interactor).Layout = 'mdi') and (ALayout.Tag = 11) then
      Result := inherited CreateControl(AParent, AView, ALayout, AParams)
    else begin
      vTab := TcxTabSheet.Create(TComponent(GetVCLControl(AParent)));
      vTab.Caption := ALayout.Caption;
      vTab.ImageIndex := AParent.GetImageID(ALayout.ImageID);

      vStartPageName := TDomain(AParent.View.Domain).Settings.GetValue('Core', 'StartPage', '');
      vTab.AllowCloseButton := not SameText(ALayout.Name, vStartPageName);
      vTab.TabVisible := ALayout.ShowCaption;

      ALayout.Id := ALayout.Name;
      Result := vTab;
    end;
  end
  else if ALayout.Kind = lkSplitter then
  begin
    vSplitter := TcxSplitter.Create(nil);
    case ALayout.Align of
      lalTop: vSplitter.AlignSplitter := salTop;
      lalLeft: vSplitter.AlignSplitter := salLeft;
      lalRight: vSplitter.AlignSplitter := salRight;
    else
      vSplitter.AlignSplitter := salBottom;
    end;
    vSplitter.Cursor := ALayout.Cursor;
    vSplitter.HotZone := TcxSimpleStyle.Create(vSplitter);
    vSplitter.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vSplitter.Color := AlphaColorToColor(ALayout.Color);
    vSplitter.ParentColor := False;
    vSplitter.NativeBackground := False;

    ALayout.Id := '-splitter-';
    Result := vSplitter;
  end
  else if ALayout.Kind = lkPanel then
  begin
    if AParams <> '' then
      vParams := CreateDelimitedList(AParams, '&')
    else
      vParams := nil;

    if Assigned(vParams) and (vParams.Values['ViewType'] = 'Paged') then
    begin
      if TInteractor(AParent.View.Interactor).Layout = 'mdi' then
      begin
        AParent.UIBuilder.DefaultParams := AParams;
        FreeAndNil(vParams);
        Exit(nil);
      end;

      vPC := TcxPageControl.Create(nil);
      vPC.DoubleBuffered := True;
      vPC.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
      vPC.Properties.Images := TDragImageList(TInteractor(AView.Interactor).Images[16]);
      vPC.Properties.TabPosition := tpBottom;
      if vParams.Values['PageLayout'] = 'Top' then
        vPC.Properties.TabPosition := tpTop;
      vPC.Properties.CloseButtonMode := cbmActiveTab;
      vPC.OnCanClose := OnPCCanClose;
      vPC.Align := TAlign(ALayout.Align);
      vPC.Anchors := ALayout.Anchors;
      CopyFontSettings(vPC.Font, ALayout);

      FreeAndNil(vParams);
      ALayout.Id := Trim(ALayout.Caption);
      ALayout.Name := '-pages-';
      Result := vPC;
    end
    else begin
      FreeAndNil(vParams);
      Result := inherited CreateControl(AParent, AView, ALayout, AParams);
    end;
  end
  else if ALayout.Kind = lkScrollBox then
  begin
    vBox := TcxScrollBox.Create(nil);
    vBox.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vBox.Anchors := ALayout.Anchors;
    vBox.Align := TAlign(ALayout.Align);
    CopyMargins(vBox, ALayout);
    CopyPadding(vBox, ALayout);
    vBox.LookAndFeel.ScrollbarMode := sbmClassic;
    if ALayout.BorderStyle = lbsNone then
      vBox.BorderStyle := cxcbsNone;

    Result := vBox;
  end
  else if ALayout.Kind <> lkNone then
    Result := inherited CreateControl(AParent, AView, ALayout, AParams)
  else
    Assert(False, 'Пустой класс для лэйаута');
end;

destructor TDevExpressPresenter.Destroy;
begin
  FreeAndNil(FRowStyle);
  FreeAndNil(FSplashForm);

  inherited Destroy;
end;

function TDevExpressPresenter.DoCreateImages(const AInteractor: TInteractor; const AImages: TImages; const ASize: Integer): TObject;
var
  vImageList: TcxImageList;
  vImage: TdxPNGImage;
  vIndex: Integer;
  vStream: TStream;
  vBitmap: TBitmap;
  i: Integer;
begin
  vImageList := TcxImageList.Create(nil);
  vImageList.SetSize(ASize, ASize);
  Result := vImageList;

  for vIndex in AImages.Indices.Keys do
    AInteractor.StoreImageIndex(vIndex, AImages.Indices[vIndex]);

  vImage := TdxPNGImage.Create;
  vImageList.BeginUpdate;
  try
    for i := 0 to AImages.Count - 1 do
    begin
      vStream := AImages[i];
      vStream.Position := 0;
      vImage.LoadFromStream(vStream);
      vBitmap := vImage.GetAsBitmap;
      try
        vImageList.AddBitmap(vBitmap, nil, clnone, True, True)
      finally
        FreeAndNil(vBitmap);
      end;
    end;
  finally
    vImageList.EndUpdate;
    FreeAndNil(vImage);
  end;
end;

function TDevExpressPresenter.GetNativeControlClass: TNativeControlClass;
begin
  Result := TDEControl;
end;

initialization

TBaseModule.RegisterModule('UI', 'Windows.DevExpress', TDevExpressPresenter);

end.
