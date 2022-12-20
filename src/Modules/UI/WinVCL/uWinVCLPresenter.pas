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

unit uWinVCLPresenter;

interface

uses
  Messages, Classes, Generics.Collections, Generics.Defaults, ActnList, uConsts, StdCtrls, Buttons,
  ExtCtrls, TypInfo, Types, ComCtrls, SysUtils, Windows, Graphics,
  Variants, Controls, Forms, Mask, Menus,

  uDefinition, uPresenter, uInteractor, uView, uSettings,

  StartForm, DebugInfoForm, SplashForm, uUIBuilder, uLayout;

type
  TImageResolution = (ir16x16, ir24x24, ir32x32);

type
  TWinVCLPresenter = class(TPresenter)
  private
    procedure LoadImages(const AInteractor: TInteractor; const AImageList: TDragImageList; const AResolution: Integer);
    procedure DoToggleUI(const AVisible: Boolean); // Rethink
    procedure DoTrayIconClick(Sender: TObject);
  private
    FRowStyle: TObject;
    FNeedShowSplash: Boolean;
    procedure ArrangeMozaic(const AMDIForm: TForm);
    procedure RestoreChildForms(const AInteractor: TInteractor);
    procedure StoreChildForms(const AInteractor: TInteractor; const AMainForm: TForm);
    function GetLayoutKind(const AControl: TObject): TLayoutKind;
    procedure CopyControlPropertiesToLayout(const ALayout: TLayout; const AControl: TObject);

    procedure CopyPopupMenuItems(const AParent: TUIArea; const AView: TView;
      const ASrcItem: TNavigationItem; const ADestMenu: TMenuItem);

    procedure OnPCCanClose(Sender: TObject; var ACanClose: Boolean);
    procedure OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
  protected
    //FOnRFIDRead: TRFIDReadEvent;
    FTrayIcon: TTrayIcon;
    FStartForm: TStartFm;
    [Weak] FDebugForm: TDebugFm;
    [Weak] FSplashForm: TSplashFm;
    function ShowLoginForm(const AAppTitle: string; var ALoginName, APass{, ARFID}: string): Boolean;
    procedure DoChildFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoChildFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoFloatFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoMainFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoDebugFormClose(Sender: TObject; var Action: TCloseAction);
    procedure OnShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure DoOnFormShow(Sender: TObject);
    function MessageTypeToMBFlags(const AMessageType: TMessageType): Integer;
  protected
    procedure DoRun(const AParameter: string); override;
    procedure DoUnfreeze; override;
    procedure DoStop; override;

    function DoLogin(const ADomain: TObject): TInteractor; override;
    procedure DoLogout(const AInteractor: TInteractor); override;

    function GetNativeControlClass: TNativeControlClass; override;
    procedure DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType); override;
    function DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult; override;
    procedure DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False); override;
    function DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt, ADefaultDir: string): Boolean; override;
    function DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean; override;
    procedure DoSetCursor(const ACursorType: TCursorType); override;
    procedure DoCloseAllPages(const AInteractor: TInteractor); override;

    procedure OnDomainLoadProgress(const AProgress: Integer; const AInfo: string); override;
    procedure OnDomainError(const ACaption, AText: string); override;

    function DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject; override;

    procedure StoreUILayout(const AInteractor: TInteractor); override;
    procedure RestoreUILayout(const AInteractor: TInteractor); override;

    procedure DoEnumerateControls(const ALayout: TLayout; const AControl: TObject); override;
  public
    constructor Create(const AName: string; const ASettings: TSettings); override;
    destructor Destroy; override;

    procedure SetAsMainForm(const AForm: TForm);

    function CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView; const AAreaName: string;
      const ACallback: TNotifyEvent = nil; const ACaption: string = ''; const AOnClose: TProc = nil): TUIArea; override;
    function ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult; override;
    procedure CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea); override;

    function ShowPage(const AInteractor: TInteractor; const APageType: string; const AParams: TObject = nil): TDialogResult; override;
    procedure ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement); override;

    function CreateArea(const AParent: TUIArea; const ALayout: TLayout; const AView: TView;
      const AParams: string = ''; const AOnClose: TProc = nil): TUIArea; override;
    function CreateTempControl: TObject; override;
    function AppendServiceArea(const AParent: TUIArea): TUIArea; override;
    function CreatePopupArea(const AParent: TUIArea; const ALayout: TLayout): TUIArea; override;

    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); override;

    function GetWidthByType(const AWidth: Integer; const AFieldDef: TFieldDef): Integer;

    property RowStyle: TObject read FRowStyle;
  end;

procedure CopyFontSettings(const AFont: TFont; const ALayout: TLayout);
procedure CopyMargins(const AControl: TControl; const ALayout: TLayout);
procedure CopyPadding(const AControl: TWinControl; const ALayout: TLayout);
procedure CopyConstraints(const AControl: TControl; const ALayout: TLayout);

implementation

uses
  Dialogs, Math, StrUtils, ShellAPI, UITypes, ActiveX, JPEG, PngImage,
  cxGraphics, dxGDIPlusClasses, cxImage, cxEdit, cxPC, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxMemo,
  cxLabel, cxTextEdit, cxButtons, cxScrollBox, cxControls, cxSplitter,

  uPlatform, uModule, uDomain, uUtils, uConfiguration, uChangeManager, uIcon, uEntity, uEntityList, vclArea, uSession,
  uManagedForm, AboutForm, ReportConfigureForm, OptionsForm, LoginForm, FloatForm, uCollection;

const
  cPCNavigatorFlag = 1;
  cServiceAreaHeight = 44;

type
  TLoginedProc = procedure(const AInteractor: TInteractor) of object;
  TBeforeUIClosingFunc = function(const AInteractor: TInteractor): Boolean of object;

type
  TCrackedWinControl = class(TWinControl) end;
  TCrackedControl = class(TControl) end;
  TCrackedArea = class(TUIArea) end;

procedure CopyFontSettings(const AFont: TFont; const ALayout: TLayout);
begin
  AFont.Name := ALayout.Font.Family;
  AFont.Color := AlphaColorToColor(ALayout.Font.Color);
  AFont.Size := ALayout.Font.Size;
  AFont.Style := ALayout.Font.Style;
end;

procedure CopyMargins(const AControl: TControl; const ALayout: TLayout);
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

procedure CopyPadding(const AControl: TWinControl; const ALayout: TLayout);
begin
  AControl.Padding.Left := ALayout.Padding.Left;
  AControl.Padding.Top := ALayout.Padding.Top;
  AControl.Padding.Right := ALayout.Padding.Right;
  AControl.Padding.Bottom := ALayout.Padding.Bottom;
end;

procedure CopyConstraints(const AControl: TControl; const ALayout: TLayout);
begin
  AControl.Constraints.MinWidth := ALayout.Constraints.MinWidth;
  AControl.Constraints.MinHeight := ALayout.Constraints.MinHeight;
  AControl.Constraints.MaxWidth := ALayout.Constraints.MaxWidth;
  AControl.Constraints.MaxHeight := ALayout.Constraints.MaxHeight;
end;

{ TWinVCLPresenter }

function TWinVCLPresenter.AppendServiceArea(const AParent: TUIArea): TUIArea;
var
  vPanel: TPanel;
begin
  vPanel := TPanel.Create(nil);
  vPanel.BevelOuter := bvNone;
  vPanel.Height := cServiceAreaHeight;
  vPanel.Align := alBottom;
  Result := CreateFilledArea(AParent, AParent.View, '', True, vPanel);
end;

procedure TWinVCLPresenter.ArrangeMozaic(const AMDIForm: TForm);
var
  i, j: Integer;
  vClientRect: TRect;
  vTotalForms: Integer;
  vColCount: Integer;
  vMap: array of Integer;
  vRow: Integer;
  vTotal: Integer;
const
  cColumnCounts: array[1..30] of Integer = (1,2,2,2,2, 3,3,4,3,3, 3,4,4,4,5, 4,4,4,4,5, 5,5,5,6,5, 5,5,5,5,6);

  procedure LayoutForm(const ACol, ARow, AMainWidth, AMainHeight: Integer; const AForm: TForm);
  begin
    AForm.Width := AMainWidth div vColCount;
    AForm.Height := AMainHeight div vMap[ACol];
    AForm.Left := ACol * AForm.Width;
    AForm.Top := ARow * AForm.Height;
  end;
begin
  vTotalForms := AMDIForm.MDIChildCount;
  if vTotalForms <= 0 then
    Exit;

  // Формируем карту расположения окон
  if vTotalForms > 30 then
    vColCount := Floor(Sqrt(vTotalForms))
  else
    vColCount := cColumnCounts[vTotalForms];

  SetLength(vMap, vColCount);
  for i := 0 to vColCount - 1 do
    vMap[i] := vTotalForms div vColCount;
  for i := vColCount - vTotalForms mod vColCount to vColCount - 1 do
    vMap[i] := vMap[i] + 1;

  try
    GetClientRect(AMDIForm.ClientHandle, vClientRect);
    for i := 0 to AMDIForm.MDIChildCount - 1 do
    begin
      vTotal := 0;
      for j := 0 to vColCount - 1 do
      begin
        vTotal := vTotal + vMap[j];
        if i < vTotal then
        begin
          vRow := (i - (vTotal - vMap[j])) mod vMap[j];
          LayoutForm(j, vRow, vClientRect.Width, vClientRect.Height, AMDIForm.MDIChildren[i]);
          Break;
        end;
      end;
    end;
  finally
    SetLength(vMap, 0);
  end;
end;

procedure TWinVCLPresenter.ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement);
var
  vForm: TForm;
begin
  if AInteractor.Layout <> 'mdi'  then
    Exit;

  vForm := TForm(AInteractor.UIBuilder.RootArea.InnerControl);
  case AArrangeKind of
    waCascade:
      vForm.Cascade;
    waTileHorz:
      begin
        vForm.TileMode := tbHorizontal;
        vForm.Tile;
      end;
    waTileVert:
      begin
        vForm.TileMode := tbVertical;
        vForm.Tile;
      end;
    waMozaic:
      ArrangeMozaic(vForm);
  end;
end;

type
  TUIAreaCrack = class(TUIArea) end;

procedure TWinVCLPresenter.CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea);
var
  vForm: TForm;
begin
  if Assigned(AOldArea) then
  begin
    vForm := TForm(AOldArea.InnerControl);
    CloseAllPages(AInteractor);
    TUIAreaCrack(AOldArea).ClearContent;

    SetAsMainForm(TForm(ANewArea.InnerControl));

    vForm.Close;
    AOldArea.Free;
  end
  else
    SetAsMainForm(TForm(ANewArea.InnerControl));
end;

procedure TWinVCLPresenter.CopyControlPropertiesToLayout(const ALayout: TLayout; const AControl: TObject);
var
  vSourceLabel: TLabel;
  vSourceImage: TImage;
  vSourcePC: TPageControl;
  vSourceTabSheet: TTabSheet;
  vSourcePanel: TPanel;
  vSourceBox: TScrollBox;
  vSourceBevel: TBevel;
  vSourceSplitter: TSplitter;
  vSourceShape: TShape;
  vSourceMemo: TMemo;
  vSourceFrame: TFrame;
  vCaption: string;
  vUIParams: string;
  vPos: Integer;
  vImageStream: TStream;

  procedure CopyMargins(const AControl: TControl);
  begin
    if AControl.AlignWithMargins then
    begin
      ALayout.Margins.Left := AControl.Margins.Left;
      ALayout.Margins.Top := AControl.Margins.Top;
      ALayout.Margins.Right := AControl.Margins.Right;
      ALayout.Margins.Bottom := AControl.Margins.Bottom;
    end;
  end;

  procedure CopyPadding(const AControl: TWinControl);
  begin
    ALayout.Padding.Left := AControl.Padding.Left;
    ALayout.Padding.Top := AControl.Padding.Top;
    ALayout.Padding.Right := AControl.Padding.Right;
    ALayout.Padding.Bottom := AControl.Padding.Bottom;
  end;

  procedure CopyConstraints(const AControl: TControl);
  begin
    ALayout.Constraints.MinWidth := AControl.Constraints.MinWidth;
    ALayout.Constraints.MinHeight := AControl.Constraints.MinHeight;
    ALayout.Constraints.MaxWidth := AControl.Constraints.MaxWidth;
    ALayout.Constraints.MaxHeight := AControl.Constraints.MaxHeight;
  end;

  procedure CopyViewState(const AControl: TControl);
  begin
    if not AControl.Visible then
      ALayout.State := vsHidden
    else if not AControl.Enabled then
      ALayout.State := vsDisabled
    else
      ALayout.State := vsFullAccess;
  end;

  procedure CopyPenSettings(const APen: TPen);
  begin
    ALayout.Pen.Color := ColorToAlphaColor(APen.Color);
    ALayout.Pen.Width := APen.Width;
  end;

  procedure CopyBrushSettings(const ABrush: TBrush);
  begin
    ALayout.Brush.Color := ColorToAlphaColor(ABrush.Color);
  end;

  procedure CopyFontSettings(const AFont: TFont);
  begin
    ALayout.Font.Family := AFont.Name;
    ALayout.Font.Color := ColorToAlphaColor(AFont.Color);
    ALayout.Font.Size := Round(AFont.Size * AFont.PixelsPerInch / 96);
    ALayout.Font.Style := AFont.Style;
  end;

begin
  if ALayout.Kind = lkPanel then
  begin
    vSourcePanel := TPanel(AControl);
    ALayout.Left := vSourcePanel.Left;
    ALayout.Top := vSourcePanel.Top;
    ALayout.Width := vSourcePanel.Width;
    ALayout.Height := vSourcePanel.Height;
    ALayout.Anchors := vSourcePanel.Anchors;
    ALayout.Caption := vSourcePanel.Caption;
    ALayout.ShowCaption := vSourcePanel.ShowCaption;
    ALayout.Caption_AtLeft := vSourcePanel.DoubleBuffered;
    ALayout.Button_ShowCaption := vSourcePanel.ShowHint;
    CopyMargins(vSourcePanel);
    ALayout.Align := TLayoutAlign(vSourcePanel.Align);
    ALayout.Alignment := vSourcePanel.Alignment;
    CopyConstraints(vSourcePanel);
    CopyFontSettings(vSourcePanel.Font);
    CopyPadding(vSourcePanel);
    ALayout.BevelInner := TLayoutBevelKind(vSourcePanel.BevelInner);
    ALayout.BevelOuter := TLayoutBevelKind(vSourcePanel.BevelOuter);
    ALayout.Color := ColorToAlphaColor(vSourcePanel.Color);
  end
  else if ALayout.Kind = lkPage then
  begin
    vSourceTabSheet := TTabSheet(AControl);
    ALayout.Name := vSourceTabSheet.Name;
    ALayout.Tag := vSourceTabSheet.Tag;
    ALayout.Caption := vSourceTabSheet.Caption;
    ALayout.ImageID := vSourceTabSheet.ImageIndex;
    ALayout.ShowCaption := vSourceTabSheet.TabVisible;
  end
  else if ALayout.Kind = lkPages then
  begin
    vSourcePC := TPageControl(AControl);
    ALayout.Left := vSourcePC.Left;
    ALayout.Top := vSourcePC.Top;
    ALayout.Width := vSourcePC.Width;
    ALayout.Height := vSourcePC.Height;
    ALayout.Anchors := vSourcePC.Anchors;
    CopyMargins(vSourcePC);
    ALayout.Align := TLayoutAlign(vSourcePC.Align);
    CopyConstraints(vSourcePC);
    CopyFontSettings(vSourcePC.Font);
    CopyViewState(vSourcePC);
    ALayout.Tag := vSourcePC.Tag;
    ALayout.ShowCaption := vSourcePC.ShowHint;
    ALayout.Caption := vSourcePC.Hint;
    ALayout.Page_Style := TPageStyle(vSourcePC.Style);
    ALayout.Page_Position := TPagePosition(vSourcePC.TabPosition);
    ALayout.Page_Height := vSourcePC.TabHeight;
    ALayout.Page_Width := vSourcePC.TabWidth;
  end
  else if ALayout.Kind = lkMemo then
  begin
    vSourceMemo := TMemo(AControl);

    ALayout.Width := vSourceMemo.Width;
    ALayout.Height := vSourceMemo.Height;
    ALayout.Left := vSourceMemo.Left;
    ALayout.Top := vSourceMemo.Top;
    ALayout.Anchors := vSourceMemo.Anchors;
    vSourceMemo.WordWrap := False;
    vSourceMemo.WantReturns := False;
    ALayout.ShowCaption := vSourceMemo.ShowHint;
    ALayout.Caption := vSourceMemo.Lines.Text;
    ALayout.Caption_AtLeft := vSourceMemo.DoubleBuffered;
    CopyMargins(vSourceMemo);
    ALayout.Align := TLayoutAlign(vSourceMemo.Align);
    ALayout.Alignment := vSourceMemo.Alignment;
    CopyConstraints(vSourceMemo);
    CopyFontSettings(vSourceMemo.Font);
    CopyPadding(vSourceMemo);
    ALayout.BevelInner := TLayoutBevelKind(vSourceMemo.BevelInner);
    ALayout.BevelOuter := TLayoutBevelKind(vSourceMemo.BevelOuter);
    ALayout.Color := ColorToAlphaColor(vSourceMemo.Color);
  end
  else if ALayout.Kind = lkLabel then
  begin
    vSourceLabel := TLabel(AControl);
    ALayout.Left := vSourceLabel.Left;
    ALayout.Top := vSourceLabel.Top;
    ALayout.Width := vSourceLabel.Width;
    ALayout.Height := vSourceLabel.Height;
    ALayout.Anchors := vSourceLabel.Anchors;
    ALayout.Alignment := vSourceLabel.Alignment;
    ALayout.Transparent := vSourceLabel.Transparent;
    ALayout.AutoSize := vSourceLabel.AutoSize;
    ALayout.WordWrap := vSourceLabel.WordWrap;

    vCaption := vSourceLabel.Caption;
    vUIParams := '';
    vPos := Pos('@', vCaption);
    if vPos > 1 then
    begin
      vUIParams := vCaption;
      Delete(vUIParams, 1, vPos);
      vCaption := Copy(vCaption, 1, vPos - 1);
    end
    else
      vCaption := vSourceLabel.Caption;

    ALayout.Caption := vCaption;
    ALayout.UIParams := vUIParams;

    CopyFontSettings(vSourceLabel.Font);
  end
  else if ALayout.Kind = lkImage then
  begin
    vSourceImage := TImage(AControl);
    ALayout.Left := vSourceImage.Left;
    ALayout.Top := vSourceImage.Top;
    ALayout.Width := vSourceImage.Width;
    ALayout.Height := vSourceImage.Height;
    ALayout.Anchors := vSourceImage.Anchors;
    CopyMargins(vSourceImage);
    ALayout.Align := TLayoutAlign(vSourceImage.Align);
    ALayout.Hint := vSourceImage.Hint;
    ALayout.Transparent := vSourceImage.Transparent;
    ALayout.AutoSize := vSourceImage.AutoSize;
    vImageStream := TMemoryStream.Create;
    vSourceImage.Picture.Graphic.SaveToStream(vImageStream);
    ALayout.Image_Picture := vImageStream;
    ALayout.Image_Stretch := vSourceImage.Stretch;
    ALayout.Image_Proportional := vSourceImage.Proportional;
    ALayout.Image_Center := vSourceImage.Center;
  end
  else if ALayout.Kind = lkBevel then
  begin
    vSourceBevel := TBevel(AControl);
    ALayout.Left := vSourceBevel.Left;
    ALayout.Top := vSourceBevel.Top;
    ALayout.Width := vSourceBevel.Width;
    ALayout.Height := vSourceBevel.Height;
    ALayout.Anchors := vSourceBevel.Anchors;
    CopyMargins(vSourceBevel);
    CopyConstraints(vSourceBevel);
    ALayout.Align := TLayoutAlign(vSourceBevel.Align);
    ALayout.Bevel_Shape := TLayoutBevelShape(vSourceBevel.Shape);
    ALayout.Bevel_Style := TLayoutBevelStyle(vSourceBevel.Style);
  end
  else if ALayout.Kind = lkShape then
  begin
    vSourceShape := TShape(AControl);
    ALayout.Left := vSourceShape.Left;
    ALayout.Top := vSourceShape.Top;
    ALayout.Width := vSourceShape.Width;
    ALayout.Height := vSourceShape.Height;
    ALayout.Anchors := vSourceShape.Anchors;
    ALayout.Align := TLayoutAlign(vSourceShape.Align);
    CopyMargins(vSourceShape);
    ALayout.Hint := vSourceShape.Hint;
    CopyViewState(vSourceShape);
    CopyPenSettings(vSourceShape.Pen);
    CopyBrushSettings(vSourceShape.Brush);
    ALayout.Shape_Type := TLayoutShapeType(vSourceShape.Shape);
  end
  else if ALayout.Kind = lkSplitter then
  begin
    vSourceSplitter := TSplitter(AControl);
    ALayout.Left := vSourceSplitter.Left;
    ALayout.Top := vSourceSplitter.Top;
    ALayout.Width := vSourceSplitter.Width;
    ALayout.Height := vSourceSplitter.Height;
    ALayout.Align := TLayoutAlign(vSourceSplitter.Align);
    ALayout.Cursor := vSourceSplitter.Cursor;
    ALayout.Color := ColorToAlphaColor(vSourceSplitter.Color);
  end
  else if ALayout.Kind = lkScrollBox then
  begin
    vSourceBox := TScrollBox(AControl);
    ALayout.Left := vSourceBox.Left;
    ALayout.Top := vSourceBox.Top;
    ALayout.Width := vSourceBox.Width;
    ALayout.Height := vSourceBox.Height;
    ALayout.Anchors := vSourceBox.Anchors;
    ALayout.Align := TLayoutAlign(vSourceBox.Align);
    CopyMargins(vSourceBox);
    CopyPadding(vSourceBox);
    ALayout.BorderStyle := TLayoutBorderStyle(vSourceBox.BorderStyle);
  end
  else if ALayout.Kind = lkFrame then
  begin
    vSourceFrame := TFrame(AControl);
    ALayout.Width := vSourceFrame.ClientWidth;
    ALayout.Height := vSourceFrame.ClientHeight;
    ALayout.Tag := vSourceFrame.Tag;
    ALayout.Caption := vSourceFrame.Hint;
    ALayout.Color := ColorToAlphaColor(vSourceFrame.Color);
    //ALayout.ShowCaption := vSourceFrame.ShowCaption;
    //ALayout.Caption_AtLeft := vSourceFrame.DoubleBuffered;
    //ALayout.Button_ShowCaption := vSourceFrame.ShowHint;
    //CopyMargins(vSourceFrame);
    //ALayout.Align := TLayoutAlign(vSourceFrame.Align);
    //ALayout.Alignment := vSourceFrame.Alignment;
    //CopyConstraints(vSourceFrame);
    //CopyFontSettings(vSourceFrame.Font);
    //CopyPadding(vSourceFrame);
    //ALayout.BevelInner := TLayoutBevelKind(vSourceFrame.BevelInner);
    //ALayout.BevelOuter := TLayoutBevelKind(vSourceFrame.BevelOuter);

  end
  else if ALayout.Kind = lkGroup then
  begin

  end
  else if ALayout.Kind = lkAction then
  begin

  end
  else
    Assert(False, 'Тип контрола не поддерживается');
end;

procedure TWinVCLPresenter.CopyPopupMenuItems(const AParent: TUIArea; const AView: TView;
  const ASrcItem: TNavigationItem; const ADestMenu: TMenuItem);
var
  vCaption: string;
  vParams: TStrings;
  vActions: TList<TActionDef>;
  vReports: TList<TRTFReport>;
  vDefinitions: TList<TDefinition>;
  vId: string;
  i, j: Integer;
  vDefinition: TDefinition;
  vSrcItem: TNavigationItem;
  vDestItem: TMenuItem;
  vChildItem: TMenuItem;
  vView: TView;
  vAction: TActionDef;
  vReport: TRTFReport;
  vChildArea: TUIArea;
begin
  for i := 0 to ASrcItem.Items.Count - 1 do
  begin
    vSrcItem := TNavigationItem(ASrcItem.Items[i]);
    vCaption := vSrcItem.ViewName;

    if SameText(vCaption, '#Placeholder') then
    begin
      if AView.DefinitionKind <> dkCollection then
        Continue;

      vDefinition := TDefinition(AView.Definition);
      vActions := TList<TActionDef>.Create;
      vDefinition.GetAllActions(vActions);
      for vAction in vActions do
      begin
        if vAction.HasFlag(ccHideInMenu) then
          Continue;

        if vAction.HasFlag(ccContextAction) then
        begin
          vView := AParent.UIBuilder.RootView.BuildView(AView.Name + '/Selected/' + vAction.Name);
        end
        else
          vView := AParent.UIBuilder.RootView.BuildView(AView.Name + '/' + vAction.Name);

        if Assigned(vView) then
        begin
          if vView.DefinitionKind = dkAction then
          begin
            vDestItem := TMenuItem.Create(nil);
            vDestItem.Caption := AParent.GetTranslation(vAction);
            vDestItem.Hint := vDestItem.Caption;
            vDestItem.ImageIndex := AParent.GetImageID(vAction._ImageID);
            vDestItem.OnClick := AParent.OnAreaClick;
            vDestItem.RadioItem := vSrcItem.RadioItem;
            vDestItem.GroupIndex := vSrcItem.GroupIndex;

            if Assigned(vView.DomainObject) and TEntity(vView.DomainObject).FieldExists('IsChecked') then
              vDestItem.AutoCheck := True;

            vChildArea := CreateFilledArea(AParent, vView, vCaption, False, vDestItem, vSrcItem);
            TCrackedArea(vChildArea).UpdateArea(dckViewStateChanged);
            AParent.AddArea(vChildArea);

            ADestMenu.Add(vDestItem);
          end
          else
            vView.CleanView;
        end;
      end;

      if vActions.Count > 0 then
      begin
        vDestItem := TMenuItem.Create(nil);
        vDestItem.Caption := '-';
        ADestMenu.Add(vDestItem);
      end;

      FreeAndNil(vActions);

      vReports := TList<TRTFReport>.Create;
      vDefinition.GetAllReports(vReports);
      for vReport in vReports do
      begin
        vView := AParent.UIBuilder.RootView.BuildView(AView.Name + '/Selected/' + vReport.Name);

        if Assigned(vView) then
        begin
          if vView.DefinitionKind = dkAction then
          begin
            vDestItem := TMenuItem.Create(nil);
            vDestItem.Caption := AParent.GetTranslation(vReport);
            vDestItem.Hint := vDestItem.Caption;
            vDestItem.ImageIndex := 31;
            vDestItem.OnClick := AParent.OnAreaClick;

            vChildArea := CreateFilledArea(AParent, vView, vCaption, False, vDestItem, vSrcItem);
            TCrackedArea(vChildArea).UpdateArea(dckViewStateChanged);
            AParent.AddArea(vChildArea);

            ADestMenu.Add(vDestItem);
          end
          else
            vView.CleanView;
        end;
      end;

      if vReports.Count > 0 then
      begin
        vDestItem := TMenuItem.Create(nil);
        vDestItem.Caption := '-';
        ADestMenu.Add(vDestItem);
      end;

      FreeAndNil(vReports);

      Continue;
    end
    else if Pos('@', vCaption) > 0 then
    begin
      vParams := CreateDelimitedList(vCaption, '@');
      try
        vId := vParams.Values['Id'];
        vCaption := vParams.Values['Caption'];
      finally
        FreeAndNil(vParams);
      end;

      vDestItem := TMenuItem.Create(nil);
      if vId <> '' then
        vDestItem.Caption := TInteractor(AParent.Interactor).Translate(vId, vCaption)
      else
        vDestItem.Caption := vCaption;

      vChildArea := CreateFilledArea(AParent, AParent.View, vSrcItem.ViewName, False, vDestItem, vSrcItem);
      AParent.AddArea(vChildArea);

      ADestMenu.Add(vDestItem);
      CopyPopupMenuItems(vChildArea, AView, vSrcItem, vDestItem);

      Continue;
    end
    else
      vView := AView.BuildView(vCaption);

    vDestItem := TMenuItem.Create(nil);

    if Assigned(vView) and (vView.DefinitionKind = dkAction) then
    begin
      vAction := TActionDef(vView.Definition);
      vDestItem.Caption := AParent.GetTranslation(vAction);
      vDestItem.Hint := vDestItem.Caption;
      vDestItem.ImageIndex := AParent.GetImageID(vAction._ImageID);

      if (vAction.Name = 'Add') and Assigned(vView.ParentDomainObject) and (vView.ParentDomainObject is TEntityList) then
      begin
        vDefinitions := TEntityList(vView.ParentDomainObject).ContentDefinitions;
        if vDefinitions.Count > 1 then
        begin
          for j := 0 to vDefinitions.Count - 1 do
          begin
            vDefinition := TDefinition(vDefinitions[j]);
            vChildItem := TMenuItem.Create(nil);
            vChildItem.Caption := AParent.GetTranslation(vDefinition);
            vChildItem.OnClick := AParent.OnActionMenuSelected;
            vDestItem.Add(vChildItem);
          end;
        end
        else
          vDestItem.OnClick := AParent.OnAreaClick;
      end
      else
        vDestItem.OnClick := AParent.OnAreaClick;

      vDestItem.RadioItem := vSrcItem.RadioItem;
      vDestItem.GroupIndex := vSrcItem.GroupIndex;
      if Assigned(vView.DomainObject) and TEntity(vView.DomainObject).FieldExists('IsChecked') then
        vDestItem.AutoCheck := True;

      vChildArea := CreateFilledArea(AParent, vView, vCaption, False, vDestItem, vSrcItem);
      for j := 0 to vDestItem.Count - 1 do
        vDestItem[j].Tag := Integer(vChildArea);

      TCrackedArea(vChildArea).UpdateArea(dckViewStateChanged);

      AParent.AddArea(vChildArea);
    end
    else begin
      if Assigned(vView) and (vView.DefinitionKind = dkUndefined) then
        vView.CleanView;

      vDestItem.Caption := vCaption;
      vDestItem.Enabled := False;
    end;

    ADestMenu.Add(vDestItem);
  end;
end;

constructor TWinVCLPresenter.Create(const AName: string; const ASettings: TSettings);
begin
  inherited Create(AName, ASettings);

  FRowStyle := TcxStyle.Create(nil);

  if ASettings.KeyExists(AName, 'ShowSplash') then
    FNeedShowSplash := StrToBoolDef(ASettings.GetValue(AName, 'ShowSplash'), False)
  else
    FNeedShowSplash := StrToBoolDef(ASettings.GetValue('Core', 'ShowSplash'), False);
end;

type
  TImageHeaderGetter = packed record
    case Byte of
      0: (Bytes: array[0..3] of Byte);
      1: (Words: array[0..1] of Word);
      2: (Value: Cardinal);
  end;

function TWinVCLPresenter.CreateArea(const AParent: TUIArea; const ALayout: TLayout; const AView: TView;
  const AParams: string; const AOnClose: TProc): TUIArea;
var
  vDomain: TDomain;
  vStartPageName: string;
  vStartPageStr: string;
  vForm: TForm;
  vShape: TShape;
  vLabel: TcxLabel; //TStaticText;
  vImage: TcxImage;
  vPC: TcxPageControl;
  vTab: TcxTabSheet;
  vPanel: TPanel;
  vParams: TStrings;
  vBox: TcxScrollBox;
  vBevel: TBevel;
  vSplitter: TcxSplitter;

  procedure CopyPenSettings(const APen: TPen);
  begin
    APen.Color := AlphaColorToColor(ALayout.Pen.Color);
    APen.Width := ALayout.Pen.Width;
  end;

  procedure CopyBrushSettings(const ABrush: TBrush);
  begin
    ABrush.Color := AlphaColorToColor(ALayout.Brush.Color);
  end;

  procedure SetPictureFromStream(const APicture: TPicture);
  var
    vRecognizer: TImageHeaderGetter;
    vGraphic: TGraphic;
  begin
    if not Assigned(ALayout.Image_Picture) then
      Exit;

    ALayout.Image_Picture.Position := 0;
    ALayout.Image_Picture.Read(vRecognizer, SizeOf(TImageHeaderGetter));
    if (vRecognizer.Bytes[0] = $42) and (vRecognizer.Bytes[1] = $4D) then
      vGraphic := TBitmap.Create
    else if (vRecognizer.Bytes[0] = $FF) and (vRecognizer.Bytes[1] = $D8) and (vRecognizer.Bytes[2] = $FF) and (vRecognizer.Bytes[3] = $E0) then
      vGraphic := TJPEGImage.Create
    else if (vRecognizer.Bytes[1] = $50) and (vRecognizer.Bytes[2] = $4E) and (vRecognizer.Bytes[3] = $47) then
      vGraphic := TPngImage.Create
    else
      vGraphic := nil;
    if not Assigned(vGraphic) then
      Assert(False, 'Формат графического файла не поддерживается');

    try
      ALayout.Image_Picture.Position := 0;
      vGraphic.LoadFromStream(ALayout.Image_Picture);
      APicture.Assign(vGraphic);
    finally
      FreeAndNil(vGraphic);
    end;
  end;
begin
  Result := nil;

  if not Assigned(ALayout) then
    Exit;

  vDomain := TDomain(TInteractor(AView.Interactor).Domain);

  if ALayout.Kind = lkShape then
  begin
    vShape := TShape.Create(nil);
    vShape.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vShape.Anchors := ALayout.Anchors;
    vShape.Align := TAlign(ALayout.Align);
    vShape.Hint := ALayout.Hint;
    vShape.Visible := ALayout.State > vsHidden;
    CopyPenSettings(vShape.Pen);
    CopyBrushSettings(vShape.Brush);
    CopyMargins(vShape, ALayout);
    CopyConstraints(vShape, ALayout);
    vShape.Shape := TShapeType(ALayout.Shape_Type);

    Result := CreateFilledArea(AParent, AView, '', False, vShape, ALayout);
  end
  else if ALayout.Kind = lkLabel then
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
      //else if AView.DefinitionKind in [dkListField..dkComplexField] then
      //  vLabel.Caption := AParent.GetTranslation(TFieldDef(AView.Definition))
      else
        vLabel.Caption := ALayout.Caption;
    end
    else
      vLabel.Caption := ALayout.Caption;

    Result := CreateFilledArea(AParent, AView, '', False, vLabel, ALayout);
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

    SetPictureFromStream(vImage.Picture);
    vImage.Properties.Stretch := ALayout.Image_Stretch;
    vImage.Properties.Proportional := ALayout.Image_Proportional;
    vImage.Properties.Center := ALayout.Image_Center;

    Result := CreateFilledArea(AParent, AView, '', False, vImage, ALayout);
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

    Result := CreateFilledArea(AParent, AView, '', False, vPC, ALayout);
  end
  else if ALayout.Kind = lkPage then
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
        TDragImageList(TInteractor(AView.Interactor).Images[16]).GetIcon(AParent.GetImageID(TDefinition(AView.Definition)._ImageID), vForm.Icon);

      Result := CreateFilledArea(AParent, AView, ALayout.Name, False, vForm, ALayout);
      Result.OnClose := AOnClose;
    end
    else begin
      vTab := TcxTabSheet.Create(TComponent(AParent.InnerControl));
      vTab.Caption := ALayout.Caption;
      vTab.ImageIndex := AParent.GetImageID(ALayout.ImageID);

      vStartPageName := TDomain(AParent.View.Domain).Settings.GetValue('Core', 'StartPage', '');
      vTab.AllowCloseButton := not SameText(ALayout.Name, vStartPageName);
      vTab.TabVisible := ALayout.ShowCaption;

      Result := CreateFilledArea(AParent, AView, ALayout.Name, False, vTab, ALayout);
    end;
  end
  else if ALayout.Kind = lkBevel then
  begin
    vBevel := TBevel.Create(nil);
    vBevel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vBevel.Align := TAlign(ALayout.Align);
    CopyMargins(vBevel, ALayout);
    vBevel.Shape := TBevelShape(ALayout.Bevel_Shape);
    vBevel.Style := TBevelStyle(ALayout.Bevel_Style);

    Result := CreateFilledArea(AParent, AView, '-bevel-', False, vBevel, ALayout);
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

    Result := CreateFilledArea(AParent, AView, '-splitter-', False, vSplitter, ALayout);
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
      Result := CreateFilledArea(AParent, AView, Trim(ALayout.Caption), False, vPC, ALayout);

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

      AParent.UIBuilder.PagedArea := Result;
    end
    else
    begin
      vPanel := TPanel.Create(nil);
      vPanel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
      vPanel.Anchors := ALayout.Anchors;
      vPanel.Align := TAlign(ALayout.Align);
      CopyMargins(vPanel, ALayout);
      CopyFontSettings(vPanel.Font, ALayout);
      CopyPadding(vPanel, ALayout);
      if AView.DefinitionKind <> dkListField then
      begin
        vPanel.BevelInner := TBevelCut(ALayout.BevelInner);
        vPanel.BevelOuter := TBevelCut(ALayout.BevelOuter);
      end
      else begin
        vPanel.BevelInner := bvNone;
        vPanel.BevelOuter := bvNone;
      end;
      vPanel.Color := AlphaColorToColor(ALayout.Color);
      vPanel.ParentColor := False;
      vPanel.ParentBackground := False;

      Result := CreateFilledArea(AParent, AView, Trim(ALayout.Caption), False, vPanel, ALayout, AParams);
    end;

    Result.AddParams(vParams);
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

    Result := CreateFilledArea(AParent, AView, '', False, vBox, ALayout);
  end
  else if ALayout.Kind = lkMemo then
  begin
    vPanel := TPanel.Create(nil);
    vPanel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vPanel.Anchors := ALayout.Anchors;
    vPanel.Align := TAlign(ALayout.Align);
    CopyMargins(vPanel, ALayout);
    CopyPadding(vPanel, ALayout);
    CopyFontSettings(vPanel.Font, ALayout);
    if AView.DefinitionKind <> dkListField then
    begin
      vPanel.BevelInner := TBevelCut(ALayout.BevelInner);
      vPanel.BevelOuter := TBevelCut(ALayout.BevelOuter);
    end
    else begin
      vPanel.BevelInner := bvNone;
      vPanel.BevelOuter := bvNone;
    end;
    vPanel.Color := AlphaColorToColor(ALayout.Color);
    vPanel.ParentColor := False;
    vPanel.ParentBackground := False;

    Result := CreateFilledArea(AParent, AView, Trim(ALayout.Caption), False, vPanel, ALayout);
  end
  else if ALayout.Kind <> lkNone then
    Assert(False, 'Класс не поддерживается для создания лэйаутов')
  else
    Assert(False, 'Пустой класс для лэйаута');
end;

function TWinVCLPresenter.CreatePopupArea(const AParent: TUIArea; const ALayout: TLayout): TUIArea;
var
  vMenu: TPopupMenu;
  vView: TView;
begin
  if Assigned(ALayout) and Assigned(ALayout.Menu) then
  begin
    vMenu := TPopupMenu.Create(TComponent(AParent.InnerControl));
    vMenu.Images := TDragImageList(TInteractor(AParent.Interactor).Images[16]);
    vView := AParent.UIBuilder.RootView;
    Result := CreateFilledArea(AParent, vView, '-popup-', False, vMenu);
    CopyPopupMenuItems(AParent, AParent.View, ALayout.Menu, vMenu.Items);
  end
  else
    Result := nil;
end;

function TWinVCLPresenter.CreateTempControl: TObject;
begin
  Result := TFrame.Create(nil);
end;

function TWinVCLPresenter.CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView;
  const AAreaName: string; const ACallback: TNotifyEvent = nil; const ACaption: string = ''; const AOnClose: TProc = nil): TUIArea;
var
  vForm: TForm;
  vTimer: TTimer;
  i: Integer;
  vArea: TUIArea;
begin
  Result := nil; vTimer := nil; vForm := nil;

  if AAreaName = '' then
  begin
    Application.CreateForm(TForm, vForm);

    if AInteractor.Layout = 'mdi' then
      vForm.FormStyle := fsMDIForm;

    vForm.OnClose := DoMainFormClose;
    vForm.Position := poScreenCenter;
    vForm.Caption := TDomain(AInteractor.Domain).AppTitle + ' (' + TUserSession(AInteractor.Session).CurrentUserName + ')';
  end
  // второстепенная автономная форма
  else if AAreaName = 'float' then
  begin
    vArea := nil;
    for i := 0 to AParent.Count - 1 do
    begin
      vArea := AParent.Areas[i];
      if (vArea.View = AView) and (vArea.InnerControl is TForm) then
        Exit(vArea);
    end;

    vForm := TFloatFm.Create(nil);

    vForm.OnClose := DoFloatFormClose;
    vForm.Position := poMainFormCenter;
    vForm.Font.Size := 12;
    vForm.Caption := ACaption;
    vForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
    if Assigned(vArea) and (AView.DefinitionKind in [dkCollection, dkAction, dkEntity]) then
      TDragImageList(TInteractor(AInteractor).Images[16]).GetIcon(vArea.GetImageID(TDefinition(AView.Definition)._ImageID), vForm.Icon);
  end
  // дочерняя модальная форма
  else if (AAreaName = 'child') or (AAreaName = 'modal') then
  begin
    vForm := TForm.Create(nil);

    if Assigned(ACallback) then
    begin
      vForm.BorderIcons := [];
      vTimer := TTimer.Create(vForm);
      vTimer.Enabled := False;
      vTimer.OnTimer := ACallback;
    end
    else
    begin
      vForm.OnClose := DoChildFormClose;
      vForm.OnKeyDown := DoChildFormKeyDown;
      vForm.KeyPreview := True;
      vForm.BorderIcons := [biSystemMenu];
    end;
    vForm.Position := poMainFormCenter;
    vForm.Font.Size := 12;
    vForm.BorderStyle := bsSingle;  // for layouted form this property will be changed when assigned cEditFormResizable flag in Tag
  end;

  if vForm = nil then Exit;

  vForm.ShowHint := True;
  vForm.DisableAlign;
  Assert(not Assigned(vForm.OnShow), 'vForm.OnShow already assigned');
  vForm.OnShow := DoOnFormShow;
  try
    Result := CreateFilledArea(AParent, AView, AAreaName, True, vForm, nil, '');
    if Assigned(AOnClose) then
      Result.OnClose := AOnClose;

    if Assigned(ACallback) and Assigned(vTimer) then
      ACallback(vTimer);
  finally
    vForm.EnableAlign;
  end;
end;

destructor TWinVCLPresenter.Destroy;
begin
  FreeAndNil(FRowStyle);
  FreeAndNil(FSplashForm);

  inherited Destroy;
end;

function TWinVCLPresenter.DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject;
begin
  Result := TcxImageList.Create(nil);
  TcxImageList(Result).SetSize(ASize, ASize);
  LoadImages(AInteractor, TcxImageList(Result), ASize);
end;

procedure TWinVCLPresenter.DoDebugFormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FDebugForm := nil;
end;

procedure TWinVCLPresenter.DoEnumerateControls(const ALayout: TLayout; const AControl: TObject);
var
  vParentControl: TWinControl;
  vControl: TControl;
  vLayout: TLayout;
  i: Integer;

  procedure CopyMenuItems(const ASource: TMenuItem; const ADestination: TNavigationItem);
  var
    i: Integer;
    vNavItem: TNavigationItem;
  begin
    for i := 0 to ASource.Count - 1 do
    begin
      vNavItem := ADestination.Add(ASource[i].Caption);
      vNavItem.RadioItem := ASource[i].RadioItem;
      vNavItem.GroupIndex := ASource[i].GroupIndex;
      CopyMenuItems(ASource[i], vNavItem);
    end;
  end;
begin
  if not (AControl is TWinControl) then
    Exit;

  if ALayout.Kind = lkFrame then
    CopyControlPropertiesToLayout(ALayout, AControl);

  vParentControl := TWinControl(AControl);
  if Assigned(TCrackedControl(vParentControl).PopupMenu) then
  begin
    ALayout.Menu := TNavigationItem.Create(nil, '');
    CopyMenuItems(TCrackedControl(vParentControl).PopupMenu.Items, ALayout.Menu);
  end;

  for i := 0 to vParentControl.ControlCount - 1 do
  begin
    vControl := vParentControl.Controls[i];
    vLayout := TLayout.Create(GetLayoutKind(vControl));
    CopyControlPropertiesToLayout(vLayout, vControl);
    ALayout.Add(vLayout);
    DoEnumerateControls(vLayout, vControl);
  end;
end;

procedure TWinVCLPresenter.DoFloatFormClose(Sender: TObject; var Action: TCloseAction);
var
  vForm: TForm;
  vArea: TUIArea;
  vView: TView;
  vCloseView: TView;
  vInteractor: TInteractor;
  vCanBeClosed: Boolean;
begin
  vForm := TForm(Sender);
  vArea := TUIArea(vForm.Tag);
  vView := vArea.View;
  vCloseView := vView.BuildView('Close');
  vCloseView.AddListener(vArea);
  try
    vInteractor := TInteractor(vArea.Interactor);

    vCanBeClosed := not (Assigned(vCloseView) and (TCheckActionFlagsFunc(TConfiguration(vInteractor.Configuration).
      CheckActionFlagsFunc)(vCloseView) <> vsFullAccess));

    if vCanBeClosed then
    begin
      if Assigned(vArea.OnClose) then
        vArea.OnClose();

      // Возможно, нужно сбросить CurrentArea у UIBuilder-а в vArea.Parent

      vArea.SetHolder(nil);
      if Assigned(vArea.Parent) then
        vArea.Parent.RemoveArea(vArea);

      vInteractor.PrintHierarchy;
      //Action := caFree;
    end
    else
      Action := caNone;
  finally
    vCloseView.RemoveListener(vArea);
  end;
end;

function TWinVCLPresenter.GetLayoutKind(const AControl: TObject): TLayoutKind;
begin
  if AControl is TPanel then
    Result := lkPanel
  else if AControl is TTabSheet then
    Result := lkPage
  else if AControl is TPageControl then
    Result := lkPages
  else if AControl is TMemo then
    Result := lkMemo
  else if AControl is TLabel then
    Result := lkLabel
  else if AControl is TImage then
    Result := lkImage
  else if AControl is TScrollBox then
    Result := lkScrollBox
  else if AControl is TBevel then
    Result := lkBevel
  else if AControl is TSplitter then
    Result := lkSplitter
  else if AControl is TShape then
    Result := lkShape
  else if (AControl is TFrame) or (AControl is TForm) then
    Result := lkFrame
  else
    Result := lkNone;
end;

function TWinVCLPresenter.GetNativeControlClass: TNativeControlClass;
begin
  Result := TVCLControl;
end;

procedure TWinVCLPresenter.LoadImages(const AInteractor: TInteractor;
  const AImageList: TDragImageList; const AResolution: Integer);
var
  vConfiguration: TConfiguration;
  vImage: TdxPNGImage;
  vPlaceholder: TBitmap;

  function GetPlaceholder: TBitmap;
  var
    vResDiv8: Integer;
  begin
    if not Assigned(vPlaceholder) then
    begin
      vPlaceholder := TBitmap.Create;
      vPlaceholder.SetSize(AResolution, AResolution);
      vPlaceholder.PixelFormat := pf32bit;
      vResDiv8 := Max(AResolution div 8, 1);
      vPlaceholder.Canvas.Pen.Width := 1;
      vPlaceholder.Canvas.Rectangle(vResDiv8, vResDiv8, AResolution - vResDiv8, AResolution - vResDiv8);
    end;

    Result := vPlaceholder;
  end;

  procedure AppendIconsToImageList(const AIcons: TIcons);
  var
    vIndex: Integer;
    vStream: TStream;
    vBitmap: TBitmap;
  begin
    for vIndex in AIcons.IconIndices do
    begin
      AInteractor.StoreImageIndex(vIndex, AImageList.Count);

      vStream := AIcons.IconByIndex(vIndex, AResolution);
      if not Assigned(vStream) then
        AImageList.Add(GetPlaceholder, nil)
      else begin
        vStream.Position := 0;
        vImage.LoadFromStream(vStream);
        vBitmap := vImage.GetAsBitmap;
        try
          if AImageList is TcxImageList then
            TcxImageList(AImageList).AddBitmap(vBitmap, nil, clnone, True, True)
          else
            AImageList.Add(vBitmap, nil);
        finally
          if vIndex = 0 then
            vPlaceholder := vBitmap
          else
            vBitmap.Free;
        end;
      end;
    end;
  end;

begin
  vImage := TdxPNGImage.Create;
  vPlaceholder := nil;

  AImageList.BeginUpdate;
  try
    AppendIconsToImageList(FCommonIcons);

    vConfiguration := TConfiguration(AInteractor.Configuration);
    AppendIconsToImageList(vConfiguration.Icons);
  finally
    AImageList.EndUpdate;
  end;

  FreeAndNil(vImage);
  FreeAndNil(vPlaceholder);
end;

function TWinVCLPresenter.MessageTypeToMBFlags(const AMessageType: TMessageType): Integer;
begin
  Result := 0;
  case AMessageType of
    msInfo: Result := MB_ICONINFORMATION;
    msQuestion: Result := MB_ICONQUESTION;
    msWarning: Result := MB_ICONWARNING;
    msError: Result := MB_ICONERROR;
  end;
end;

procedure TWinVCLPresenter.OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
var
  vArea: TUIArea;
  vView: TView;
  vCloseView: TView;
  vInteractor: TInteractor;
  vCanBeClosed: Boolean;
  vCloseProc: TProc;
  vDelEntity: TEntity;
begin
  vArea := TUIArea(Pointer(TForm(Sender).Tag));
  vView := vArea.View;
  vInteractor := TInteractor(vArea.Interactor);

  vCloseView := vView.BuildView('Close');
  vCloseView.AddListener(vArea);
  vCloseProc := nil;
  vDelEntity := nil;

  try
    vCanBeClosed := not (Assigned(vCloseView) and (TCheckActionFlagsFunc(TConfiguration(vInteractor.Configuration).
      CheckActionFlagsFunc)(vCloseView) <> vsFullAccess));

    if (vView.DomainObject is TEntity) and TEntity(vView.DomainObject).InstanceOf('_FormLayout') then
      vDelEntity := TEntity(vView.DomainObject);

    if vCanBeClosed then
    begin
      vCloseProc := vArea.OnClose;
      vArea.SetControl(nil);
      vArea.UIBuilder.RootArea.RemoveArea(vArea); // the form will be destroyed here
    end
    else
      Action := caNone;
  finally
    vCloseView.RemoveListener(vArea);
  end;

  if vCanBeClosed and Assigned(vDelEntity) then
  begin
    TUserSession(vInteractor.Session).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
    begin
      vDelEntity.Delete(AHolder);
      Result := True;
    end, nil);
  end;

  if Assigned(vCloseProc) then
    vCloseProc;
end;

procedure TWinVCLPresenter.OnDomainError(const ACaption, AText: string);
begin
  ShowMessage(ACaption, AText, msError);
end;

procedure TWinVCLPresenter.OnDomainLoadProgress(const AProgress: Integer; const AInfo: string);
begin
  if FNeedShowSplash then
  begin
    FProgressInfo.SetProgress(AProgress, AInfo);
    ShowPage(nil, 'splash', FProgressInfo);
  end;
end;

procedure TWinVCLPresenter.OnPCCanClose(Sender: TObject; var ACanClose: Boolean);
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

  vPCArea := TUIArea(vPC.Tag);
  vTabArea := TUIArea(vTab.Tag);

  vPCArea.UIBuilder.LastArea := nil;

  vPCArea.RemoveArea(vTabArea);

  vPCArea.UIBuilder.PrintHierarchy;
end;

procedure TWinVCLPresenter.OnShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  vActiveInteractor: TInteractor;
  vView: TView;
begin
  vActiveInteractor := ActiveInteractor;
  if Assigned(vActiveInteractor)
    and (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_MENU) < 0) and (GetKeyState(VK_SHIFT) < 0) then
  begin
    if (Msg.CharCode = Ord('D')) or (Msg.CharCode = Ord('F')) then
    begin
      Handled := True;
      if Assigned(FDebugForm) then
        FreeAndNil(FDebugForm);
      ShowPage(vActiveInteractor, 'debug');
      if Assigned(FDebugForm) then
        SetForegroundWindow(FDebugForm.Handle);
    end
    else if Msg.CharCode = Ord('E') then
    begin
      Handled := True;
      if not Assigned(vActiveInteractor.UIBuilder.ActiveArea) then
        Exit;
      vView := vActiveInteractor.UIBuilder.ActiveArea.View;
      if not (vView.DefinitionKind in [dkAction, dkListField, dkObjectField, dkSimpleField]) then
        Exit;
      if not TUserSession(vActiveInteractor.Session).IsAdmin then
        Exit;

      vView.ElevateAccess;
    end
    else if Msg.CharCode = Ord('R') then
    begin
      Handled := True;
      TDomain(vActiveInteractor.Domain).ReloadChanges(TDomain(vActiveInteractor.Domain).DomainHolder);
    end;
  end;
end;

function TWinVCLPresenter.DoLogin(const ADomain: TObject): TInteractor;
var
  vDomain: TDomain absolute ADomain;
  vLogin, vPassword: string;
  //vRFID: string;
  vSession: TUserSession;
  vResult: Boolean;
  vMainFormName: string;
  vLayout: string;
  vUsers: TEntityList;
begin
  Result := nil;

  vLayout := vDomain.Settings.GetValue('Core', 'Layout', '');

  SetApplicationUI(vDomain.AppTitle, vDomain.Configuration.IconFileName);

  vUsers := TEntityList.Create(vDomain, vDomain.DomainSession);
  vDomain.GetEntityList(vDomain.DomainSession, vDomain.Configuration['SysUsers'], vUsers, '');
  try
    if vUsers.Count > 0 then
    begin
      if (vUsers.Count = 1) and (vDomain.Settings.GetValue('Core', 'AutoLogin', '') = '1') then
        vSession := vDomain.Sessions.AddSession(vUsers[0])
      else begin
        vLogin := vDomain.UserSettings.GetValue('Core', 'LastLogin', '');
        vPassword := '';
        //vRFID := '';
        vSession := nil;
        repeat
          //vDomain.SetRFIDHandler(FOnRFIDRead);
          //try
            vResult := ShowLoginForm(vDomain.AppTitle, vLogin, vPassword{, vRFID});
          //finally
          //  vDomain.SetRFIDHandler(nil);
          //end;

          if not vResult then
            Break;

          //if Trim(vRFID) <> '' then
          //begin
          //  vSession := vDomain.LoginByRFID(vRFID);
          //  if not Assigned(vSession) and (ShowYesNoDialog(_Platform.Translate('cptError', 'Ошибка'),
          //    _Platform.Translate('txtWrongLoginRFID', 'Считанная RFID-карта не принадлежит ни одному сотруднику')
          //    + #13#10 + _Platform.Translate('txtPromptTryAgain', 'Попробовать ещё раз?')) <> drYes) then Break;
          //end
          //else begin
            vSession := vDomain.Login(vLogin, vPassword);
            if not Assigned(vSession) and (ShowYesNoDialog(_Platform.Translate('cptError', 'Ошибка'),
              _Platform.Translate('txtWrongLoginOrPassword', 'Введены некорректные имя пользователя или пароль')
              + #13#10 + _Platform.Translate('txtPromptTryAgain', 'Попробовать ещё раз?')) <> drYes) then Break;
          //end;
        until Assigned(vSession);

        if not Assigned(vSession) then
        begin
          Application.Title := cPlatformTitle;
      //    Application.Icon //todo: вернуть иконку по умолчанию
          Exit;
        end;

        vDomain.UserSettings.SetValue('Core', 'LastLogin', vLogin);
      end;
    end
    else
      vSession := vDomain.DomainSession;
  finally
    FreeAndNil(vUsers);
  end;

  // Создаем корневую форму и интерактор для нее
  Result := TInteractor.Create(Self, vSession);

  vMainFormName := vDomain.Settings.GetValue('Core', 'MainForm', '');
  if Trim(vMainFormName) = '' then
    vMainFormName := 'MainForm';
  Result.UIBuilder.Navigate(nil, '', vMainFormName, '', vSession.NullHolder);

  TLoginedProc(vDomain.Configuration.LoginedProc)(Result);

  if Assigned(FDebugForm) then
    FDebugForm.AddInteractor(Result);
end;

procedure TWinVCLPresenter.DoLogout(const AInteractor: TInteractor);
begin
  Application.Title := cPlatformTitle;
  if Assigned(FDebugForm) then
    FDebugForm.RemoveInteractor(AInteractor);
end;

function TWinVCLPresenter.ShowLoginForm(const AAppTitle: string; var ALoginName, APass{, ARFID}: string): Boolean;
var
  vLoginForm: TLoginFm;
begin
  vLoginForm := TLoginFm.Create(nil);
  try
    vLoginForm.Init(AAppTitle);
    vLoginForm.LoginName := ALoginName;
    vLoginForm.Pass := APass;
    //vLoginForm.RFID := '';

    //FOnRFIDRead := vLoginForm.OnRFIDReceived;
    //try
      Result := vLoginForm.ShowModal = mrOk;
    //finally
    //  FOnRFIDRead := nil;
    //end;

    if Result then
    begin
      ALoginName := vLoginForm.LoginName;
      APass := vLoginForm.Pass;
      //ARFID := vLoginForm.RFID;
    end;
  finally
    FreeAndNil(vLoginForm);
  end;
end;

function TWinVCLPresenter.ShowPage(const AInteractor: TInteractor; const APageType: string;
  const AParams: TObject = nil): TDialogResult;
var
  vForm: TReportConfigureFm;
  vProgressInfo: TProgressInfo;
  vPageClass: TManagedFormClass;
begin
  Result := drNone;

  if APageType = 'about' then
  begin
    TAboutFm.ShowAbout(AInteractor);
  end
  else if (APageType = 'debug') then
  begin
    if _Platform.DeploymentType = 'dev' then
    begin
      if not Assigned(FDebugForm) then
      begin
        FDebugForm := TDebugFm.Create(nil);
        FDebugForm.AddInteractor(AInteractor);
        FDebugForm.OnClose := DoDebugFormClose;
        FDebugForm.Show;
      end;
      FDebugForm.UpdateDebugInfo;
    end;
  end
  else if APageType = 'rtf_reports' then
  begin
    vForm := TReportConfigureFm.Create(nil);
    vForm.Init(AInteractor);
    try
      vForm.ShowModal;
    finally
      vForm.Free;
    end;
  end
  else if APageType = 'options' then
  begin
    TOptionsFm.Edit(AInteractor);
  end
  else if APageType = 'splash' then
  begin
    vProgressInfo := TProgressInfo(AParams);
    if not Assigned(vProgressInfo) then
      Exit;

    if not Assigned(FSplashForm) then
      FSplashForm := TSplashFm.ShowSplash(Self, TDomain(vProgressInfo.Domain))
    else if vProgressInfo.Progress = 100 then
      FreeAndNil(FSplashForm)
    else
      FSplashForm.UpdateProgress(vProgressInfo.Progress, vProgressInfo.Info);
  end
  else begin
    vPageClass := TManagedFormClass(GetPageClass(FName, APageType));
    if Assigned(vPageClass) then
      TManagedForm.ShowPage(vPageClass, AInteractor);
  end;
end;

function TWinVCLPresenter.ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult;
var
  vView: TView;
  vForm: TForm;
  vCaption: string;

  function ModalResultToDialogResult(const AModalResult: TModalResult): TDialogResult;
  begin
    case AModalResult of
      mrOk: Result := drOk;
      mrCancel: Result := drCancel;
      mrYes: Result := drYes;
      mrNo: Result := drNo;
    else
      Result := drNone;
    end;
  end;

begin
  Result := drNone;
  if (AAreaName = '') or (AAreaName = 'float') then
  begin
    vForm := TForm(AArea.InnerControl);
    vForm.Show;
  end
  else if (AAreaName = 'child') or (AAreaName = 'modal') then
  begin
    vForm := TForm(AArea.InnerControl);
    vForm.ShowHint := True;
    vView := AArea.View;

    vCaption := GetUrlParam(AOptions, 'Caption', '');

    if vCaption = '' then
    begin
      // Definition может быть от листового поля
      if Assigned(vView.Definition) then
      begin
        if vForm.Caption = '' then
        begin
          if vView.DefinitionKind = dkObjectField then
            vForm.Caption := TDomain(AInteractor.Domain).TranslateFieldDef(TFieldDef(vView.Definition))
          else
            vForm.Caption := TDomain(AInteractor.Domain).TranslateDefinition(TDefinition(vView.Definition));

          if (Pos(AOptions, 'NoExtCaption') < 1) and (AAreaName = 'child') then
          begin
            if vView.DefinitionKind = dkAction then
              vForm.Caption := 'Параметры: ' + vForm.Caption
            else if vView.State >= vsSelectOnly {and Assigned(vArea.Holder) - у параметров нет холдера} then
              vForm.Caption := 'Редактирование: ' + vForm.Caption
            else
              vForm.Caption := 'Просмотр: ' + vForm.Caption;
          end;
        end;
      end
      else
        vForm.Caption := TDomain(AInteractor.Domain).AppTitle;
    end
    else
      vForm.Caption := vCaption;

    try
      Result := ModalResultToDialogResult(vForm.ShowModal);
    finally
      AArea.SetHolder(nil);
      if Assigned(AArea.Parent) then
        AArea.Parent.RemoveArea(AArea);
      vForm.Free;
    end;
  end;
end;

procedure TWinVCLPresenter.StoreUILayout(const AInteractor: TInteractor);
var
  vMainForm: TForm;
  vLayoutStr: string;

  procedure SaveForm(const AForm: TForm; const AName: string);
  begin
    vLayoutStr := IntToStr(AForm.Left) + ';' + IntToStr(AForm.Top) + ';' + IntToStr(AForm.Width) + ';' + IntToStr(AForm.Height) + ';' + IntToStr(Ord(AForm.WindowState));
    TDomain(AInteractor.Domain).UserSettings.SetValue(AName, 'Layout', vLayoutStr);
  end;
begin
  vMainForm := TForm(AInteractor.UIBuilder.RootArea.InnerControl);

  if not Assigned(vMainForm) then Exit;

  StoreChildForms(AInteractor, vMainForm);

  if (vMainForm.Position <> poDesigned) then Exit;

  SaveForm(vMainForm, 'MainForm');
end;

procedure TWinVCLPresenter.StoreChildForms(const AInteractor: TInteractor; const AMainForm: TForm);
var
  i: Integer;
  vForm: TForm;
  vArea: TUIArea;
  vView: TView;
  vEntity: TEntity;
begin
  if AInteractor.Layout = 'mdi' then
  begin
    for i := AMainForm.MDIChildCount - 1 downto 0 do
    begin
      vForm := AMainForm.MDIChildren[i];
      vArea := TUIArea(Pointer(vForm.Tag));
      vView := vArea.View;
      if Assigned(vView) and (vView.DomainObject is TEntity) then
      begin
        vEntity := TEntity(vView.DomainObject);

        if Assigned(vEntity) and vEntity.InstanceOf('_FormLayout') then
        begin
          TUserSession(AInteractor.Session).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
          begin
            vEntity._SetFieldValue(AHolder,'WindowState', vForm.WindowState);
            if vForm.WindowState = wsNormal then
            begin
              vEntity._SetFieldValue(AHolder,'Left', vForm.Left);
              vEntity._SetFieldValue(AHolder,'Top', vForm.Top);
              vEntity._SetFieldValue(AHolder,'Width', vForm.Width);
              vEntity._SetFieldValue(AHolder,'Height', vForm.Height);
            end;
            Result := True;
          end, nil);
        end;
      end;
    end;
  end;
end;

procedure TWinVCLPresenter.RestoreChildForms(const AInteractor: TInteractor);
var
  vDomain: TDomain;
  i, j: Integer;
  vEntity: TEntity;
  vCollection: TCollection;
begin
  if not Assigned(AInteractor) then Exit;

  vDomain := TDomain(AInteractor.Domain);

  for i := 0 to vDomain.Collections.Count - 1 do
  begin
    vCollection := vDomain.Collections[i];
    if vCollection.ContentDefinition.IsDescendantOf('_FormLayout') then
    begin
      for j := 0 to vCollection.Count - 1 do
      begin
        vEntity := vCollection[j];
        AInteractor.UIBuilder.Navigate(AInteractor.GetViewOfEntity(vEntity), 'WorkArea', vCollection.ContentDefinition.Name + 'EditForm',
          'Left=' + IntToStr(vEntity['Left']) +
          '&Top=' + IntToStr(vEntity['Top']) +
          '&Width=' + IntToStr(vEntity['Width']) +
          '&Height=' + IntToStr(vEntity['Height']) +
          '&WindowState=' + IntToStr(vEntity['WindowState'])
          ,nil, nil, vEntity['Name']);
      end;
    end;
  end;
end;

procedure TWinVCLPresenter.RestoreUILayout(const AInteractor: TInteractor);
var
  vMainForm: TForm;

  procedure LoadForm(const AForm: TForm; const AName: string);
  var
    vLayoutStr: string;
    vValues: TStrings;
  begin
    vLayoutStr := TDomain(AInteractor.Domain).UserSettings.GetValue(AName, 'Layout');
    if Length(vLayoutStr) = 0 then Exit;

    vValues := CreateDelimitedList(vLayoutStr, ';');
    try
      if vValues.Count <> 5 then Exit;

      AForm.Left := StrToIntdef(vValues[0], 100);
      AForm.Top := StrToIntdef(vValues[1], 100);
      AForm.Width := StrToIntdef(vValues[2], 1280);
      AForm.Height := StrToIntdef(vValues[3], 960);
      if TWindowState(StrToIntdef(vValues[4], 0)) = wsMaximized then
        AForm.WindowState := wsMaximized;
    finally
      FreeAndNil(vValues);
    end;
  end;
begin
  vMainForm := TForm(AInteractor.UIBuilder.RootArea.InnerControl);
  if (not Assigned(vMainForm)) or (vMainForm.Position <> poDesigned) then Exit;

  LoadForm(vMainForm, 'MainForm');

  RestoreChildForms(AInteractor);
end;

procedure TWinVCLPresenter.DoChildFormClose(Sender: TObject; var Action: TCloseAction);
var
  vForm: TForm;
  vArea: TUIArea;
  vInteractor: TInteractor;
  vView, vCloseView: TView;
  vHolder: TChangeHolder;
  vRes: TDialogResult;
  vChildArea: TUIArea;
  i: Integer;
  vCanBeClosed: Boolean;
  vCloseProc: TProc;

  function GetUnfilledRequiredFields(const AArea: TUIArea; var AFields: string): Boolean;
  var
    i: Integer;
    vFieldDef: TFieldDef;
    vChildArea: TUIArea;
  begin
    for i := 0 to AArea.Count - 1 do
    begin
      vChildArea := AArea.Areas[i];
      if (vChildArea.View.DefinitionKind in [dkListField, dkObjectField, dkSimpleField]) then
      begin
        vFieldDef := TFieldDef(vChildArea.View.Definition);
        if Assigned(vChildArea.View.ParentDomainObject)
          and not TEntity(vChildArea.View.ParentDomainObject).FieldByName(vFieldDef.Name).IsValid then
        begin
          if Length(AFields) > 0 then
            AFields := AFields + ', ';
          AFields := AFields + TDomain(vInteractor.Domain).TranslateFieldDef(vFieldDef);
        end;
      end
      else
        GetUnfilledRequiredFields(vChildArea, AFields);
    end;

    Result := Length(AFields) > 0;
  end;

  procedure DoValidation;
  var
    vEmptyRequiredFields: string;
    vEntity: TEntity;
    vSimilar: TEntity;
  begin
    if not (vView.DefinitionKind in [dkObjectField, dkEntity, dkAction]) then
      Exit;

    vEntity := TEntity(vView.DomainObject);
    if not Assigned(vEntity) then
      Exit;

    vEmptyRequiredFields := '';
    if GetUnfilledRequiredFields(vArea, vEmptyRequiredFields) then
    begin
      vInteractor.ShowMessage(vInteractor.Translate('msgRequiredFieldsAreEmpty', 'Не заполнены обязательные поля') +
        '. ' + #13#10 + vEmptyRequiredFields);
      Action := caNone;
    end;

    if Action <> caNone then
    begin
      vSimilar := vEntity.FindSimilar(vInteractor.Session);
      if Assigned(vSimilar) then
      begin
        vInteractor.ShowMessage(vInteractor.Translate('msgRecordExists', 'Такая запись уже существует') +
          ' [' + vSimilar['Name'] + ']');
        Action := caNone;
      end;
    end;
  end;
begin
  vForm := TForm(Sender);
  vArea := TUIArea(vForm.Tag);
  vHolder := TChangeHolder(vArea.ThisHolder);
  vView := vArea.View;
  vInteractor := TInteractor(vArea.Interactor);

  for i := 0 to vArea.Count - 1 do
  begin
    vChildArea := vArea[i];
    if vChildArea.NativeControl.IsForm then
    begin
      vInteractor.ShowMessage('Невозможно закрыть окно, так как есть другие программные окна, зависящие от него', msWarning);
      Action := caNone;
      Exit;
    end;
  end;

  vCloseProc := nil;
  vCloseView := vView.BuildView('Close');
  vCloseView.AddListener(vArea);
  try
    //vCanBeClosed := not (Assigned(vCloseView) and (TCheckActionFlagsFunc(TConfiguration(vInteractor.Configuration).
    //  CheckActionFlagsFunc)(vCloseView) <> vsFullAccess));
    vCanBeClosed := True;

    if vCanBeClosed then
    begin
      vCloseProc := vArea.OnClose;
      if vForm.ModalResult = mrOk then
        DoValidation
      else
      begin
        if Assigned(vHolder) and vHolder.IsVisibleModified then
        begin
          vRes := TPresenter(vInteractor.Presenter).ShowYesNoDialog(vForm.Caption,
            vInteractor.Translate('msgPromtSaveChanges', 'Сохранить изменения перед закрытием формы?'), True);
          if vRes = drYes then
          begin
            vForm.ModalResult := mrOk;
            DoValidation;
          end
          else if vRes = drNo then
            vForm.ModalResult := mrCancel
          else if vRes = drCancel then
            vForm.ModalResult := mrNone;
        end;
      end;
    end
    else
      Action := caNone;
  finally
    vCloseView.RemoveListener(vArea);
  end;

  if Assigned(vCloseProc) then
    vCloseProc;
end;

procedure TWinVCLPresenter.DoChildFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  vForm: TForm;
  vFormArea: TUIArea;
  vView: TView;
begin
  vForm := TForm(Sender);

  if (Key = VK_RETURN) and (ssCtrl in Shift) then
    vForm.ModalResult := mrOk
  else if Key = VK_RETURN then
  begin
    //if vForm.ControlCount < 3 then
    //  FForm.ModalResult := mrOk else
    if (not (ssShift in Shift)) and (not (vForm.ActiveControl is TMemo)) and (not (vForm.ActiveControl is TcxMemo)) then
      PostMessage(vForm.Handle, WM_NEXTDLGCTL, 0, 0);
  end
  else if Key = VK_ESCAPE then
  begin
    vFormArea := TUIArea(vForm.Tag);
    if Assigned(vFormArea) then
    begin
      vFormArea.UIBuilder.LastArea := nil;
      vView := vFormArea.View.ViewByName('Close');
      if not Assigned(vView) then
        vView := vFormArea.View.ViewByName('Cancel');
      if Assigned(vView) then
        vFormArea.ExecuteUIAction(vView)
      else begin
        vForm.ModalResult := mrCancel;
        vForm.Close;
      end;
    end
    else
      vForm.Close;
  end;
end;

procedure TWinVCLPresenter.DoCloseAllPages(const AInteractor: TInteractor);
var
  i: Integer;
  vMainForm: TForm;
begin
  if AInteractor.Layout = 'mdi' then
  begin
    vMainForm := TForm(AInteractor.UIBuilder.RootArea.InnerControl);
    if Assigned(vMainForm) and (vMainForm.FormStyle = fsMDIForm) then
      for i := vMainForm.MDIChildCount - 1 downto 0 do
        vMainForm.MDIChildren[i].Close;
  end;
end;

function ShellExecuteU(const AHandle: Cardinal; const AFileName, AParameters, ADirectory: string;
  const Await: Boolean = False): Cardinal;
var
  ExecInfo: TShellExecuteInfo;
  NeedUnitialize: Boolean;
  AShowCmd: Integer;
  AOperation: string;
  vExecuteResult: Boolean;
begin
  Assert(AFileName <> '');
  AOperation := 'open';
  AShowCmd := SW_SHOWNORMAL;
  NeedUnitialize := Succeeded(CoInitializeEx(nil, COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE));
  try
    FillChar(ExecInfo, SizeOf(ExecInfo), 0);
    ExecInfo.cbSize := SizeOf(ExecInfo);

    ExecInfo.Wnd := AHandle;
    ExecInfo.lpVerb := Pointer(AOperation);
    ExecInfo.lpFile := PChar(AFileName);
    ExecInfo.lpParameters := Pointer(AParameters);
    ExecInfo.lpDirectory := Pointer(ADirectory);
    ExecInfo.nShow := AShowCmd;
    ExecInfo.fMask := SEE_MASK_NOASYNC or SEE_MASK_FLAG_NO_UI;

    if AWait then
      ExecInfo.fMask := ExecInfo.fMask or SEE_MASK_NOCLOSEPROCESS;

    vExecuteResult := ShellExecuteEx(@ExecInfo);
    Result := ExecInfo.hInstApp;
    if not vExecuteResult then
      Exit;

    if Await then
    begin
      WaitForSingleObject(ExecInfo.hProcess, INFINITE);
      CloseHandle(ExecInfo.hProcess);
    end;
  finally
    if NeedUnitialize then
      CoUninitialize;
  end;
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

procedure TWinVCLPresenter.DoOnFormShow(Sender: TObject);
var
  vForm: TForm;
  vArea: TUIArea;
begin
  vForm := TForm(Sender);
  if (vForm.FormStyle = fsMDIForm) and (vForm.ClientHandle > 0) and
     (GetWindowLong(vForm.ClientHandle, GWL_USERDATA ) = 0 {cannot subclass client window, userdata already in use}) then
    SetWindowLong(vForm.ClientHandle, GWL_USERDATA, SetWindowLong(vForm.ClientHandle, GWL_WNDPROC, Integer(@ClientWindowProc)));

  vArea := TUIArea(vForm.Tag);
  vArea.Activate('');
end;

procedure TWinVCLPresenter.DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False);
var
  vExecResult: Cardinal;
begin
  vExecResult := ShellExecuteU(Application.Handle, AFileName, '', '', Await);
  if (vExecResult = SE_ERR_NOASSOC) and (ADefaultApp <> '') then
    ShellExecuteU(Application.Handle, ADefaultApp, AFileName, '', Await);
end;

procedure TWinVCLPresenter.DoMainFormClose(Sender: TObject; var Action: TCloseAction);
var
  vResult: Boolean;
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(TUIArea(TForm(Sender).Tag).Interactor);
  if Assigned(vInteractor) then
    vResult := TBeforeUIClosingFunc(TConfiguration(vInteractor.Configuration).BeforeUIClosingFunc)(vInteractor)
  else
    vResult := True;

  if vResult then
  begin
    StoreUILayout(vInteractor);
    if Assigned(vInteractor) then
      Logout(vInteractor);
    Action := caFree;
  end
  else
    Action := caNone;
end;

procedure TWinVCLPresenter.DoRun(const AParameter: string);
var
  vDomain: TDomain;
  vInteractor: TInteractor;
begin
  inherited;

  Application.Title := cPlatformTitle;
  Application.Initialize;

  Application.OnShortCut := OnShortCut;

  if _Platform.Domains.Count = 1 then
  begin
    vDomain := _Platform.Domains[0];
    vInteractor := Login(vDomain);
    if Assigned(vInteractor) and (AParameter <> '') then
      vDomain.ExecuteDefaultAction(TUserSession(vInteractor.Session), AParameter);
  end
  else
  begin
    Application.CreateForm(TStartFm, FStartForm);
    FStartForm.Init(Self);
  end;

  DoOnAppStarted; // здесь подгружаются размеры и позиция главной формы

  OnDomainLoadProgress(100, '');

  Application.HintHidePause := -1;
  Application.Run;
end;

procedure TWinVCLPresenter.DoSetCursor(const ACursorType: TCursorType);
begin
  Screen.Cursor := cCursors[ACursorType];
end;

function TWinVCLPresenter.DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult;
var
  vRes: Integer;
  vFlags: Integer;
begin
  if drOk in ADialogActions then
    vFlags := MB_OKCANCEL or MB_ICONWARNING or MB_DEFBUTTON2
  else if drYes in ADialogActions then
  begin
    if drCancel in ADialogActions then
      vFlags := MB_YESNOCANCEL or MB_ICONQUESTION
    else
      vFlags := MB_YESNO or MB_ICONQUESTION;
  end
  else begin
    Result := drCancel;
    Exit;
  end;

  vRes := Application.MessageBox(PChar(AText), PChar(ACaption), vFlags); //MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL));
    //MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US)); //MAKELANGID(LANG_FRENCH, SUBLANG_FRENCH));
  if vRes = IDOK then
    Result := drOk
  else if vRes = IDYES then
    Result := drYes
  else if vRes = IDNO then
    Result := drNo
  else if vRes = IDCANCEL then
    Result := drCancel
  else
    Result := drNone;
end;

procedure TWinVCLPresenter.DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType);
var
  vCaption: string;
begin
  if ACaption = '' then
    vCaption := 'Message'
  else
    vCaption := ACaption;
  Application.MessageBox(PChar(AText), PChar(vCaption), MB_OK or MessageTypeToMBFlags(AMessageType));
    //MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
end;

function TWinVCLPresenter.DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt,
  ADefaultDir: string): Boolean;
var
  vOpenDialog: TOpenDialog;
begin
  vOpenDialog := TOpenDialog.Create(nil);
  try
    if ATitle <> '' then
      vOpenDialog.Title := ATitle;
    vOpenDialog.Filter := AFilter;
    vOpenDialog.DefaultExt := ADefaultExt;
    vOpenDialog.FileName := AFileName;
    vOpenDialog.Options := vOpenDialog.Options + [ofFileMustExist, ofPathMustExist];
    if ADefaultDir <> '' then
      vOpenDialog.InitialDir := ADefaultDir
    else if AFileName <> '' then
      vOpenDialog.InitialDir := ExtractFilePath(AFileName);

    Result := vOpenDialog.Execute;
    if Result then
      AFileName := vOpenDialog.FileName;
  finally
    FreeAndNil(vOpenDialog);
  end;
end;

function TWinVCLPresenter.DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean;
var
  vSaveDialog: TSaveDialog;
begin
  vSaveDialog := TSaveDialog.Create(nil);
  try
    vSaveDialog.Title := ATitle;
    vSaveDialog.Filter := AFilter;
    vSaveDialog.DefaultExt := ADefaultExt;
    vSaveDialog.FileName := AFileName;
    Result := vSaveDialog.Execute;
    if Result then
      AFileName := vSaveDialog.FileName;
  finally
    FreeAndNil(vSaveDialog);
  end;
end;

procedure TWinVCLPresenter.DoStop;
begin
  FreeAndNil(FDebugForm);

  if Assigned(FStartForm) then
    FStartForm.Deinit;

  //Application.Terminate;
end;

procedure TWinVCLPresenter.DoToggleUI(const AVisible: Boolean);
begin
  if AVisible then
  begin
    if Assigned(FTrayIcon) then
    begin
      FTrayIcon.Visible := False;
      Application.MainForm.Show;
    end;
  end
  else begin
    if not Assigned(FTrayIcon) then
    begin
      FTrayIcon := TTrayIcon.Create(nil);
      FTrayIcon.OnClick := DoTrayIconClick;
    end;

    ShowWindow(Application.MainForm.Handle, SW_HIDE);  // Скрываем программу
    ShowWindow(Application.Handle, SW_HIDE);  // Скрываем кнопку с TaskBar'а
    SetWindowLong(Application.Handle, GWL_EXSTYLE,
    GetWindowLong(Application.Handle, GWL_EXSTYLE) or (not WS_EX_APPWINDOW));
    FTrayIcon.Visible := True;
    Application.MainForm.Hide;
    FTrayIcon.ShowBalloonHint; // показываем наше уведомление
    FTrayIcon.BalloonTitle := 'My Program';
    FTrayIcon.BalloonHint := 'Version';
  end;
end;

procedure TWinVCLPresenter.DoTrayIconClick(Sender: TObject);
begin
  DoToggleUI(True);
end;

procedure TWinVCLPresenter.DoUnfreeze;
begin
  Application.ProcessMessages;
end;

function TWinVCLPresenter.GetWidthByType(const AWidth: Integer; const AFieldDef: TFieldDef): Integer;
begin
  case AFieldDef.Kind of
    fkInteger: Result := 70;
    fkFloat: Result := 70;
    fkDateTime: Result := 80;
    fkCurrency: Result := 80;
  else
    Result := AWidth - 4;
  end;
end;

procedure TWinVCLPresenter.SetApplicationUI(const AAppTitle, AIconName: string);
begin
  Application.Title := AAppTitle;
  if FileExists(AIconName) then
    Application.Icon.LoadFromFile(AIconName);
end;

procedure TWinVCLPresenter.SetAsMainForm(const AForm: TForm);
var
  p: Pointer;
begin
  p := @Application.MainForm;
  Pointer(p^) := AForm;
end;

initialization

TBaseModule.RegisterModule('UI', 'Windows.DevExpress', TWinVCLPresenter);

end.
