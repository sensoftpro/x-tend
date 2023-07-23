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
  ExtCtrls, Types, ComCtrls, SysUtils, Windows, Graphics,
  Variants, Controls, Forms, Mask, Menus,

  uDefinition, uPresenter, uInteractor, uSession, uView, uSettings,

  DebugInfoForm, uUIBuilder, uLayout;

type
  TImageResolution = (ir16x16, ir24x24, ir32x32);

type
  TWinVCLPresenter = class(TPresenter)
  private
    procedure DoDebugFormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure SetAsMainForm(const AForm: TForm);
    function MessageTypeToMBFlags(const AMessageType: TMessageType): Integer;

    // Creation of Layout from DFM
    function GetLayoutKind(const AControl: TObject): TLayoutKind;
    procedure CopyControlPropertiesToLayout(const ALayout: TLayout; const AControl: TObject);
  protected
    [Weak] FDebugForm: TDebugFm;
    procedure OnShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure OnChildFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure DoRun(const AParameter: string); override;
    procedure DoUnfreeze; override;
    procedure DoStop; override;

    function AreaFromSender(const ASender: TObject): TUIArea; override;
    procedure DoLogout(const AInteractor: TInteractor); override;

    function GetNativeControlClass: TNativeControlClass; override;
    procedure CreateMainForm(const ASession: TUserSession); override;
    procedure DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType); override;
    procedure DoShowDialog(const ACaption, AText: string; const ADialogActions:
      TDialogResultSet; const AOnClose: TCloseProc = nil); override;
    procedure DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False); override;
    procedure DoShowOpenDialog(const AFileName: string; const ATitle, AFilter,
      ADefaultExt, ADefaultDir: string; const AOnClose: TCloseTextProc = nil); override;
    procedure DoShowSaveDialog(const AFileName: string; const ATitle, AFilter,
      ADefaultExt: string; const AOnClose: TCloseTextProc = nil); override;
    procedure DoSetCursor(const ACursorType: TCursorType); override;

    procedure DoArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement); override;
    procedure DoCloseAllPages(const AInteractor: TInteractor); override;

    function CreateControl(const AParent: TUIArea; const AView: TView; const ALayout: TLayout;
      const AParams: string = ''): TObject; override;

    function GetImagePlaceholder(const ASize: Integer): TStream; override;
    function DoCreateImages(const ADomain: TObject; const AImages: TImages; const ASize: Integer): TObject; override;

    procedure DoEnumerateControls(const ALayout: TLayout; const AControl: TObject); override; // DFM
    function CanLoadFromDFM: Boolean; override; // DFM

  public
    function CreateTempControl: TObject; override; // DFM

    procedure ShowUIArea(const AArea: TUIArea; const AAreaName: string; const ACaption: string); override;
    procedure ShowPage(const AInteractor: TInteractor; const APageType: string;
      const AParams: TObject = nil; const AOnClose: TCloseProc = nil); override;
    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); override;
  end;

procedure CopyFontSettings(const AFont: TFont; const ALayout: TLayout);
procedure CopyMargins(const AControl: TControl; const ALayout: TLayout);
procedure CopyPadding(const AControl: TWinControl; const ALayout: TLayout);
procedure CopyConstraints(const AControl: TControl; const ALayout: TLayout);
procedure CopyPenSettings(const APen: TPen; const ALayout: TLayout);
procedure CopyBrushSettings(const ABrush: TBrush; const ALayout: TLayout);
procedure SetPictureFromStream(const APicture: TPicture; const ALayout: TLayout);

implementation

uses
  Dialogs, Math, StrUtils, ShellAPI, UITypes, ActiveX, JPEG, PngImage, GIFImg, ImgList,
  uPlatform, uModule, uDomain, uUtils, uConfiguration, uChangeManager, uIcon,
  uEntity, uEntityList, vclArea, uCollection;

type
  TCrackedWinControl = class(TWinControl) end;
  TCrackedControl = class(TControl) end;

type
  TImageHeaderGetter = packed record
    case Byte of
      0: (Bytes: array[0..3] of Byte);
      1: (Words: array[0..1] of Word);
      2: (Value: Cardinal);
  end;

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

procedure CopyPenSettings(const APen: TPen; const ALayout: TLayout);
begin
  APen.Color := AlphaColorToColor(ALayout.Pen.Color);
  APen.Width := ALayout.Pen.Width;
end;

procedure CopyBrushSettings(const ABrush: TBrush; const ALayout: TLayout);
begin
  ABrush.Color := AlphaColorToColor(ALayout.Brush.Color);
end;

procedure SetPictureFromStream(const APicture: TPicture; const ALayout: TLayout);
var
  vRecognizer: TImageHeaderGetter;
  vGraphic: TGraphic;
begin
  if not Assigned(ALayout.Image_Picture) then
    Exit;
  if ALayout.Image_Picture.Size = 0 then
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

{ TWinVCLPresenter }

function TWinVCLPresenter.AreaFromSender(const ASender: TObject): TUIArea;
begin
  if ASender is TTreeNode then
    Result := TUIArea(TTreeNode(ASender).Data)
  else
    Result := inherited AreaFromSender(ASender);
end;

function TWinVCLPresenter.CanLoadFromDFM: Boolean;
begin
  Result := True;
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
    if vSourcePanel.Alignment = taCenter then
      ALayout.Alignment := taLeftJustify
    else if vSourcePanel.Alignment = taLeftJustify then
      ALayout.Alignment := taCenter
    else
      ALayout.Alignment := vSourcePanel.Alignment;
    CopyConstraints(vSourcePanel);
    CopyFontSettings(vSourcePanel.Font);
    CopyPadding(vSourcePanel);
    ALayout.BevelInner := TLayoutBevelKind(vSourcePanel.BevelInner);
    ALayout.BevelOuter := TLayoutBevelKind(vSourcePanel.BevelOuter);
    if (ALayout.BevelInner = lbkNone) and (ALayout.BevelOuter = lbkNone) then
      ALayout.BorderStyle := lbsNone
    else
      ALayout.BorderStyle := lbsSingle;
    ALayout.Color := ColorToAlphaColor(vSourcePanel.Color);
  end
  else if ALayout.Kind = lkPage then
  begin
    vSourceTabSheet := TTabSheet(AControl);
    ALayout.Name := vSourceTabSheet.Name;
    ALayout.Tag := vSourceTabSheet.Tag;
    ALayout.Caption := vSourceTabSheet.Caption;
    ALayout.ImageID := vSourceTabSheet.Hint;
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

    if Assigned(vSourceImage.Picture.Graphic) then
    begin
      vImageStream := TMemoryStream.Create;
      vSourceImage.Picture.Graphic.SaveToStream(vImageStream);
      ALayout.Image_Picture := vImageStream;
    end
    else
      ALayout.Image_Picture := nil;

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
  end
  else if ALayout.Kind = lkAction then
  begin
  end
  else
    Assert(False, 'Тип контрола не поддерживается');
end;

function TWinVCLPresenter.CreateControl(const AParent: TUIArea; const AView: TView;
  const ALayout: TLayout; const AParams: string): TObject;
var
  vDomain: TDomain;
  vInteractor: TInteractor;
  vUIBuilder: TUIBuilder;
  vStartPageName: string;
  vForm: TForm;
  vShape: TShape;
  vLabel: TLabel; //TStaticText;
  vImage: TImage;
  vPC: TPageControl;
  vTab: TTabSheet;
  vPanel: TPanel;
  vParams: TStrings;
  vBox: TScrollBox;
  vBevel: TBevel;
  vSplitter: TSplitter;
  vMenu: TPopupMenu;
  vMenuItem: TMenuItem;
  i: Integer;
  vArea: TUIArea;
  vParentControl, vControl: TObject;
begin
  Result := nil;

  vInteractor := TInteractor(AView.Interactor);
  vUIBuilder := vInteractor.UIBuilder;
  vDomain := TDomain(vInteractor.Domain);
  vParentControl := GetRealControl(AParent);

  if ALayout.Kind = lkShape then
  begin
    vShape := TShape.Create(nil);
    vShape.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vShape.Anchors := ALayout.Anchors;
    vShape.Align := TAlign(ALayout.Align);
    vShape.Hint := ALayout.Hint;
    vShape.Visible := ALayout.State > vsHidden;
    CopyPenSettings(vShape.Pen, ALayout);
    CopyBrushSettings(vShape.Brush, ALayout);
    CopyMargins(vShape, ALayout);
    CopyConstraints(vShape, ALayout);
    vShape.Shape := TShapeType(ALayout.Shape_Type);

    Result := vShape;
  end
  else if ALayout.Kind = lkLabel then
  begin
    vLabel := TLabel.Create(nil);
    vLabel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vLabel.Transparent := ALayout.Transparent;
    vLabel.AutoSize := ALayout.AutoSize;
    vLabel.WordWrap := ALayout.WordWrap;
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
    vImage := TImage.Create(nil);
    vImage.ControlStyle := vImage.ControlStyle + [csOpaque];
    vImage.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vImage.Anchors := ALayout.Anchors;
    vImage.Align := TAlign(ALayout.Align);
    vImage.Hint := ALayout.Hint;
    CopyMargins(vImage, ALayout);
    vImage.Transparent := ALayout.Transparent;
    vImage.AutoSize := ALayout.AutoSize;

    SetPictureFromStream(vImage.Picture, ALayout);
    vImage.Stretch := ALayout.Image_Stretch;
    vImage.Proportional := ALayout.Image_Proportional;
    vImage.Center := ALayout.Image_Center;

    Result := vImage;
  end
  else if ALayout.Kind = lkPages then
  begin
    vPC := TPageControl.Create(nil);
    vPC.DoubleBuffered := True;
    vPC.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vPC.TabPosition := TTabPosition(ALayout.Page_Position);
    vPC.TabHeight := ALayout.Page_Height;
    vPC.TabWidth := ALayout.Page_Width;
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
    ALayout.Id := ALayout.Name;

    if vUIBuilder.IsMDIStyle and (ALayout.Tag = 11) then
    begin
      vForm := TForm.Create(nil);
      vForm.Caption := ALayout.Caption;
      vForm.Position := poDefault;
      vForm.FormStyle := fsMDIChild;
      vForm.OnClose := OnCloseMDIForm;
      vForm.ShowHint := True;
      if AView.DefinitionKind in [dkCollection, dkAction, dkEntity] then
        TDragImageList(vUIBuilder.Images[16]).GetIcon(AParent.GetImageIndex(TDefinition(AView.Definition)._ImageID), vForm.Icon);

      Result := vForm;
    end
    else begin
      vTab := TTabSheet.Create(TWinControl(vParentControl));
      vTab.Caption := ALayout.Caption;
      vTab.ImageIndex := AParent.GetImageIndex(ALayout.ImageID);

      vStartPageName := vDomain.Settings.GetValue('Core', 'StartPage', '');
      vTab.Parent := TWinControl(vParentControl);
      vTab.TabVisible := ALayout.ShowCaption;
      if vParentControl is TPageControl then
        vTab.PageControl := TPageControl(vParentControl);

      Result := vTab;
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

    ALayout.Id := '-bevel-';
    Result := vBevel;
  end
  else if ALayout.Kind = lkSplitter then
  begin
    vSplitter := TSplitter.Create(nil);
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

      vPC := TPageControl.Create(nil);
      vPC.DoubleBuffered := True;
      vPC.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
      vPC.Images := TDragImageList(vUIBuilder.Images[16]);
      vPC.TabPosition := tpBottom;
      if vParams.Values['PageLayout'] = 'Top' then
        vPC.TabPosition := tpTop;
      //vPC.OnCanClose := OnPCCanClose;
      vPC.Align := TAlign(ALayout.Align);
      vPC.Anchors := ALayout.Anchors;
      CopyFontSettings(vPC.Font, ALayout);
	  
      ALayout.Name := '-pages-';
      Result := vPC;
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

      Result := vPanel;
    end;

    FreeAndNil(vParams);
  end
  else if ALayout.Kind = lkScrollBox then
  begin
    vBox := TScrollBox.Create(nil);
    vBox.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vBox.Anchors := ALayout.Anchors;
    vBox.Align := TAlign(ALayout.Align);
    CopyMargins(vBox, ALayout);
    CopyPadding(vBox, ALayout);
    if ALayout.BorderStyle = lbsNone then
      vBox.BorderStyle := bsNone;

    Result := vBox;
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

    ALayout.Id := Trim(ALayout.Caption);
    Result := vPanel;
  end
  else if ALayout.Kind = lkFrame then
  begin
    vForm := nil;

    if ALayout.StyleName = '' then
    begin
      Application.CreateForm(TForm, vForm);

      if vUIBuilder.IsMDIStyle then
        vForm.FormStyle := fsMDIForm;

      vForm.OnClose := DoMainFormClose;
      vForm.Position := poScreenCenter;
      vForm.Caption := vDomain.AppTitle + ' (' + TUserSession(vInteractor.Session).CurrentUserName + ')';

      SetAsMainForm(vForm);
    end
    // второстепенная автономная форма
    else if ALayout.StyleName = 'float' then
    begin
      vArea := nil;
      if Assigned(AParent) then
      begin
        for i := 0 to AParent.Count - 1 do
        begin
          vArea := AParent.Areas[i];
          vControl := GetRealControl(vArea);
          if (vArea.View = AView) and (vControl is TForm) then
            Exit(vControl);
        end;
      end;

      vForm := TFloatFm.Create(nil);

      vForm.OnClose := DoFloatFormClose;
      vForm.Position := poMainFormCenter;
      vForm.Font.Size := 12;
      vForm.Caption := ALayout.Caption;
      vForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      if (AView.DefinitionKind in [dkCollection, dkAction, dkEntity]) then
        TDragImageList(vUIBuilder.Images[16]).GetIcon(vArea.GetImageIndex(TDefinition(AView.Definition)._ImageID), vForm.Icon);
    end
    // автономная форма со свободным отображением
    else if ALayout.StyleName = 'free' then
    begin
      vForm := TForm.Create(nil);
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
      vForm := TForm.Create(nil);
      vForm.OnClose := DoChildFormClose;
      vForm.OnKeyDown := OnChildFormKeyDown;
      vForm.KeyPreview := True;
      vForm.BorderIcons := [biSystemMenu];
      vForm.Position := poMainFormCenter;
      vForm.Font.Size := 12;
      vForm.BorderStyle := bsSingle;  // for layouted form this property will be changed when assigned cEditFormResizable flag in Tag
    end;

    if vForm = nil then
      Exit(nil);

    vForm.Color := clBtnFace;
    vForm.ShowHint := True;
    vForm.DisableAlign;
    Assert(not Assigned(vForm.OnShow), 'vForm.OnShow already assigned');
    vForm.OnShow := DoOnFormShow;
    try
      ALayout.Id := ALayout.StyleName;
      Result := vForm;
    finally
      vForm.EnableAlign;
    end;
  end
  else if ALayout.Kind = lkAction then
  begin
    if (ALayout.StyleName = '') or (ALayout.StyleName = 'menu') then
    begin
      vMenu := TPopupMenu.Create(TComponent(vParentControl));
      vMenu.Images := TDragImageList(vUIBuilder.Images[16]);
      ALayout.Name := '-popup-';
      Result := vMenu;
    end
    else begin
      vMenuItem := TMenuItem.Create(nil);
      if ALayout.StyleName = 'action' then
      begin
        vMenuItem.Caption := ALayout.Caption;
        vMenuItem.Hint := ALayout.Caption;
        vMenuItem.ImageIndex := AParent.GetImageIndex(ALayout.ImageID);
        vMenuItem.OnClick := AParent.OnAreaClick;
        if ALayout is TNavigationItem then
        begin
          vMenuItem.RadioItem := TNavigationItem(ALayout).RadioItem;
          vMenuItem.GroupIndex := TNavigationItem(ALayout).GroupIndex;
        end;

        if Assigned(AView.DomainObject) and TEntity(AView.DomainObject).FieldExists('IsChecked') then
          vMenuItem.AutoCheck := True;
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

      if vParentControl is TPopupMenu then
        TPopupMenu(vParentControl).Items.Add(vMenuItem)
      else
        TMenuItem(vParentControl).Add(vMenuItem);

      if ALayout is TNavigationItem then
        ALayout.Id := TNavigationItem(ALayout).ViewName
      else
        ALayout.Id := ALayout.Caption;

      Result := vMenuItem;
    end;
  end
  else if ALayout.Kind <> lkNone then
    Assert(False, 'Класс не поддерживается для создания лэйаутов')
  else
    Assert(False, 'Пустой класс для лэйаута');
end;

procedure TWinVCLPresenter.CreateMainForm(const ASession: TUserSession);
begin
  inherited CreateMainForm(ASession);

  if Assigned(ASession) and Assigned(ASession.Interactor) then
  begin
    if Assigned(FDebugForm) then
      FDebugForm.AddInteractor(TInteractor(ASession.Interactor));
    if FStartParameter <> '' then
      TDomain(ASession.Domain).ExecuteDefaultAction(ASession, FStartParameter);
  end;
end;

function TWinVCLPresenter.CreateTempControl: TObject;
begin
  Result := TFrame.Create(nil);
end;

function TWinVCLPresenter.DoCreateImages(const ADomain: TObject; const AImages: TImages; const ASize: Integer): TObject;
var
  vImageList: TImageList;
  vImage: TPngImage;
  vStream: TStream;
  vBitmap: TBitmap;
  i: Integer;
begin
  vImageList := TImageList.Create(nil);
  vImageList.SetSize(ASize, ASize);
  vImageList.DrawingStyle := dsTransparent;
  vImageList.ColorDepth := cd32Bit;
  vImageList.BlendColor := clNone;
  vImageList.BkColor := clNone;
  vImageList.Masked := True;
  Result := vImageList;

  vImage := TPngImage.Create;
  vBitmap := TBitmap.Create;
  vBitmap.SetSize(ASize, ASize);
  vBitmap.PixelFormat := pf32bit;
  vBitmap.Canvas.Brush.Color := clBtnFace;

  vImageList.BeginUpdate;
  try
    for i := 0 to AImages.Count - 1 do
    begin
      vStream := AImages[i];
      vStream.Position := 0;
      vImage.LoadFromStream(vStream);
      vBitmap.Canvas.FillRect(Rect(0, 0, ASize, ASize));
      vImage.Draw(vBitmap.Canvas, Rect(0, 0, ASize, ASize));
      vImageList.Add(vBitmap, nil);
    end;
  finally
    vImageList.EndUpdate;
    FreeAndNil(vImage);
    FreeAndNil(vBitmap);
  end;
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

function TWinVCLPresenter.GetImagePlaceholder(const ASize: Integer): TStream;
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
    vPlaceholder.Canvas.Pen.Color := clGray;
    vPlaceholder.Canvas.Rectangle(vResDiv8, vResDiv8, ASize - vResDiv8, ASize - vResDiv8);

    Result := TMemoryStream.Create;
    vPlaceholder.SaveToStream(Result);
  finally
    FreeAndNil(vPlaceholder);
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

procedure TWinVCLPresenter.OnChildFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  vForm: TForm;
  vHandled: Boolean;
begin
  vHandled := False;
  DoChildFormKeyDown(Sender, Shift, Key, vHandled);
  if vHandled then
    Exit;

  vForm := TForm(Sender);

  if Key = vkReturn then
  begin
    //if vForm.ControlCount < 3 then
    //  FForm.ModalResult := mrOk else
    if (not (ssShift in Shift)) and (not (vForm.ActiveControl is TMemo))then
      PostMessage(vForm.Handle, WM_NEXTDLGCTL, 0, 0);
  end;
end;

procedure TWinVCLPresenter.OnShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  vActiveInteractor: TInteractor;
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

  vActiveInteractor := ActiveInteractor;
  if Assigned(vActiveInteractor) and (ssCtrl in vShift) and (ssAlt in vShift) and (ssShift in vShift) then
  begin
    if (Msg.CharCode = vkD) or (Msg.CharCode = vkF) then
    begin
      Handled := True;
      if Assigned(FDebugForm) then
        FreeAndNil(FDebugForm);
      vActiveInteractor.PrintHierarchy;
      if Assigned(FDebugForm) then
        SetForegroundWindow(FDebugForm.Handle);
    end
  end;
end;

procedure TWinVCLPresenter.DoLogout(const AInteractor: TInteractor);
begin
  inherited DoLogout(AInteractor);
  if Assigned(FDebugForm) then
    FDebugForm.RemoveInteractor(AInteractor);
end;

procedure TWinVCLPresenter.ShowPage(const AInteractor: TInteractor; const APageType: string;
  const AParams: TObject = nil; const AOnClose: TCloseProc = nil);
begin
  if (APageType = 'debug') then
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
  end;

  if Assigned(AOnClose) then
    AOnClose(drOk);
end;

procedure TWinVCLPresenter.ShowUIArea(const AArea: TUIArea;
  const AAreaName: string; const ACaption: string);
var
  vForm: TForm;
begin
  if (AAreaName = '') or (AAreaName = 'float') or (AAreaName = 'free') then
  begin
    vForm := TForm(GetRealControl(AArea));
    vForm.Show;
  end
  else if (AAreaName = 'child') or (AAreaName = 'modal') then
  begin
    vForm := TForm(GetRealControl(AArea));
    vForm.Caption := ACaption;
    vForm.ShowModal;
    vForm.Free;
  end;
end;

procedure TWinVCLPresenter.DoArrangePages(const AInteractor: TInteractor;
  const AArrangeKind: TWindowArrangement);
var
  vForm: TForm;
begin
  vForm := TForm(GetRealControl(AInteractor.RootArea));
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
      inherited DoArrangePages(AInteractor, AArrangeKind);
  end;
end;

procedure TWinVCLPresenter.DoCloseAllPages(const AInteractor: TInteractor);
var
  i: Integer;
  vMainForm: TForm;
begin
  if AInteractor.UIBuilder.IsMDIStyle then
  begin
    vMainForm := TForm(GetRealControl(AInteractor.RootArea));
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

procedure TWinVCLPresenter.DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False);
var
  vExecResult: Cardinal;
begin
  vExecResult := ShellExecuteU(Application.Handle, AFileName, '', '', Await);
  if (vExecResult = SE_ERR_NOASSOC) and (ADefaultApp <> '') then
    ShellExecuteU(Application.Handle, ADefaultApp, AFileName, '', Await);
end;

procedure TWinVCLPresenter.DoRun(const AParameter: string);
begin
  inherited DoRun(AParameter);

  Application.Initialize;

  Application.OnShortCut := OnShortCut;
  Application.HintHidePause := -1;
  
  Application.Run;
end;

procedure TWinVCLPresenter.DoSetCursor(const ACursorType: TCursorType);
begin
  Screen.Cursor := cCursors[ACursorType];
end;

procedure TWinVCLPresenter.DoShowDialog(const ACaption, AText: string;
  const ADialogActions: TDialogResultSet; const AOnClose: TCloseProc);
var
  vRes: Integer;
  vResult: TDialogResult;
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
  else
    Exit;

  vRes := Application.MessageBox(PChar(AText), PChar(ACaption), vFlags); //MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL));
    //MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US)); //MAKELANGID(LANG_FRENCH, SUBLANG_FRENCH));

  if Assigned(AOnClose) then
  begin
    if vRes = IDOK then
      vResult := drOk
    else if vRes = IDYES then
      vResult := drYes
    else if vRes = IDNO then
      vResult := drNo
    else if vRes = IDCANCEL then
      vResult := drCancel
    else
      vResult := drNone;
    AOnClose(vResult);
  end;
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

procedure TWinVCLPresenter.DoShowOpenDialog(const AFileName: string; const ATitle,
  AFilter, ADefaultExt, ADefaultDir: string; const AOnClose: TCloseTextProc);
var
  vOpenDialog: TOpenDialog;
  vFileName: string;
begin
  if not Assigned(AOnClose) then
    Exit;

  vFileName := AFileName;
  vOpenDialog := TOpenDialog.Create(nil);
  try
    if ATitle <> '' then
      vOpenDialog.Title := ATitle;
    vOpenDialog.Filter := AFilter;
    vOpenDialog.DefaultExt := ADefaultExt;
    vOpenDialog.FileName := vFileName;
    vOpenDialog.Options := vOpenDialog.Options + [ofFileMustExist, ofPathMustExist];
    if ADefaultDir <> '' then
      vOpenDialog.InitialDir := ADefaultDir
    else if vFileName <> '' then
      vOpenDialog.InitialDir := ExtractFilePath(vFileName);

    if vOpenDialog.Execute then
    begin
      vFileName := vOpenDialog.FileName;
      AOnClose(drOk, vFileName);
    end
    else
      AOnClose(drCancel, vFileName);
  finally
    FreeAndNil(vOpenDialog);
  end;
end;

procedure TWinVCLPresenter.DoShowSaveDialog(const AFileName: string; const ATitle,
  AFilter, ADefaultExt: string; const AOnClose: TCloseTextProc);
var
  vSaveDialog: TSaveDialog;
  vFileName: string;
begin
  if not Assigned(AOnClose) then
    Exit;

  vFileName := AFileName;
  vSaveDialog := TSaveDialog.Create(nil);
  try
    vSaveDialog.Title := ATitle;
    vSaveDialog.Filter := AFilter;
    vSaveDialog.DefaultExt := ADefaultExt;
    vSaveDialog.FileName := vFileName;
    if vSaveDialog.Execute then
    begin
      vFileName := vSaveDialog.FileName;
      AOnClose(drOk, vFileName);
    end
    else
      AOnClose(drCancel, vFileName);
  finally
    FreeAndNil(vSaveDialog);
  end;
end;

procedure TWinVCLPresenter.DoStop;
begin
  FreeAndNil(FDebugForm);
  //Application.Terminate;
end;

procedure TWinVCLPresenter.DoUnfreeze;
begin
  Application.ProcessMessages;
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

RegisterClasses([TPngImage, TJPEGImage, TGIFImage]);

TBaseModule.RegisterModule('UI', '', 'Windows.VCL', TWinVCLPresenter);

end.
