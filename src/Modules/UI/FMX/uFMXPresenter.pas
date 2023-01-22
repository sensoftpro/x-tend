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

unit uFMXPresenter;

interface

uses
  System.StartUpCopy, System.Classes, System.SysUtils, System.UITypes,

{$IFDEF MSWINDOWS }
  WinAPI.ShellApi, WinAPI.Windows, WinAPI.ActiveX,
{$ENDIF}
{$IFDEF ANDROID}
  FMX.Helpers.Android, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Net, Androidapi.JNI.JavaTypes, idUri,Androidapi.IOUtils,
{$ENDIF ANDROID}
{$IFDEF IOS}
  iOSapi.Foundation, FMX.Helpers.iOS,
{$ENDIF IOS}

  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Objects, FMX.Forms, FMX.Layouts,
  uPresenter, uSettings, uInteractor, uUIBuilder, uView, fmxArea, uLayout, uConsts, uDefinition, uIcon;

type
  TImageResolution = (ir16x16, ir24x24, ir32x32);

type
  TFMXPresenter = class(TPresenter)
  protected
    procedure OnChildFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
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
    function CreateControl(const AParent: TUIArea; const AView: TView; const ALayout: TLayout;
      const AParams: string = ''): TObject; override;

    function GetImagePlaceholder(const ASize: Integer): TStream; override;
    function DoCreateImages(const ADomain: TObject; const AImages: TImages; const ASize: Integer): TObject; override;

    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); override;
  public
    function ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult; override;
  end;

procedure CopyTextSettings(const ATextSettings: TTextSettings; const ALayout: TLayout);
procedure CopyMargins(const AControl: TControl; const ALayout: TLayout);
procedure CopyPadding(const AControl: TControl; const ALayout: TLayout);
procedure CopyPenSettings(const APen: TStrokeBrush; const ALayout: TLayout);
procedure CopyBrushSettings(const ABrush: TBrush; const ALayout: TLayout);

implementation

uses
  System.Generics.Collections, System.Math, System.Types, System.StrUtils,
  FMX.DialogService, FMX.Dialogs, FMX.MultiResBitmap,
  FMX.StdCtrls, FMX.ExtCtrls, FMX.Menus, FMX.TabControl, FMX.ListView, FMX.ImgList,
  FMX.Memo, FMX.Edit,
  uModule, uDomain, uPlatform, uSession, uUtils, uEntityList, uConfiguration,
  uChangeManager, uEntity;

procedure CopyTextSettings(const ATextSettings: TTextSettings; const ALayout: TLayout);
begin
  ATextSettings.Font.Family := ALayout.Font.Family;
  ATextSettings.FontColor := ALayout.Font.Color;
  ATextSettings.Font.Size := ALayout.Font.Size * 96 / 72;
  ATextSettings.Font.Style := ALayout.Font.Style;
end;

procedure CopyMargins(const AControl: TControl; const ALayout: TLayout);
begin
  AControl.Margins.Left := ALayout.Margins.Left;
  AControl.Margins.Top := ALayout.Margins.Top;
  AControl.Margins.Right := ALayout.Margins.Right;
  AControl.Margins.Bottom := ALayout.Margins.Bottom;
end;

procedure CopyPadding(const AControl: TControl; const ALayout: TLayout);
begin
  AControl.Padding.Left := ALayout.Padding.Left;
  AControl.Padding.Top := ALayout.Padding.Top;
  AControl.Padding.Right := ALayout.Padding.Right;
  AControl.Padding.Bottom := ALayout.Padding.Bottom;
end;

procedure CopyPenSettings(const APen: TStrokeBrush; const ALayout: TLayout);
begin
  APen.Color := ALayout.Pen.Color;
  APen.Thickness := ALayout.Pen.Width;
end;

procedure CopyBrushSettings(const ABrush: TBrush; const ALayout: TLayout);
begin
  ABrush.Color := ALayout.Brush.Color;
end;

{procedure SetPictureFromStream(const APicture: TPicture; const ALayout: TLayout);
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
end;}

{ TFMXPresenter }

function TFMXPresenter.CreateControl(const AParent: TUIArea; const AView: TView;
  const ALayout: TLayout; const AParams: string): TObject;
var
  vDomain: TDomain;
  vInteractor: TInteractor;
  vUIBuilder: TUIBuilder;
  vShape: TShape;
  vImage: TImage;
  vLabel: TLabel;
  vPC: TTabControl;
  vTab: TTabItem;
  vPanel: TPanel;
  vParams: TStrings;
  vBox: TScrollBox;
  vSplitter: TSplitter;
  vMenu: TPopupMenu;
  vMenuItem: TMenuItem;

  vForm: TForm;
  i: Integer;
  vArea: TUIArea;
  vParentControl: TFmxObject;
begin
  Result := nil;

  vInteractor := TInteractor(AView.Interactor);
  vUIBuilder := vInteractor.UIBuilder;
  vDomain := TDomain(vInteractor.Domain);
  vParentControl := TFmxObject(GetRealControl(AParent));

  if ALayout.Kind = lkShape then
  begin
    case ALayout.Shape_Type of
      lstRoundRect, lstRoundSquare: vShape := TRoundRect.Create(nil);
      lstEllipse: vShape := TEllipse.Create(nil);
      lstCircle: vShape := TCircle.Create(nil);
    else
      vShape := TRectangle.Create(nil);
    end;

    vShape.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vShape.Anchors := ALayout.Anchors;
    vShape.Align := AlignToAlignLayout(ALayout.Align);
    vShape.Hint := ALayout.Hint;
    vShape.Visible := ALayout.State > vsHidden;
    vShape.Stroke.Color := ALayout.Pen.Color;
    vShape.Stroke.Thickness := ALayout.Pen.Width;
    vShape.Fill.Color := ALayout.Brush.Color;
    CopyMargins(vShape, ALayout);

    Result := vShape;
  end
  else if ALayout.Kind = lkLabel then
  begin
    vLabel := TLabel.Create(nil);
    vLabel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vLabel.AutoSize := ALayout.AutoSize;
    vLabel.WordWrap := ALayout.WordWrap;
    CopyTextSettings(vLabel.TextSettings, ALayout);
    vLabel.Anchors := ALayout.Anchors;

    if (ALayout.Caption  = '$') and (ALayout.UIParams = 'Caption') then
    begin
      if AView.DefinitionKind = dkCollection then
        vLabel.Text := AParent.GetTranslation(TDefinition(AView.Definition))
      //else if AView.DefinitionKind in [dkListField..dkComplexField] then
      //  vLabel.Caption := AParent.GetTranslation(TFieldDef(AView.Definition))
      else
        vLabel.Text := ALayout.Caption;
    end
    else
      vLabel.Text := ALayout.Caption;

    Result := vLabel;
  end
  else if ALayout.Kind = lkImage then
  begin
    vImage := TImage.Create(nil);
    vImage.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vImage.Anchors := ALayout.Anchors;
    vImage.Align := AlignToAlignLayout(ALayout.Align);
    vImage.Hint := ALayout.Hint;
    CopyMargins(vImage, ALayout);
    vImage.MarginWrapMode := TImageWrapMode.Original;

    vImage.Bitmap.LoadFromStream(ALayout.Image_Picture);

    Result := vImage;
  end
  else if ALayout.Kind = lkPages then
  begin
    vPC := TTabControl.Create(nil);
    vPC.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vPC.TabPosition := TTabPosition(ALayout.Page_Position);
    vPC.TabHeight := ALayout.Page_Height;
    vPC.Anchors := ALayout.Anchors;
    vPC.Align := AlignToAlignLayout(ALayout.Align);
    CopyMargins(vPC, ALayout);
    vPC.Visible := ALayout.State > vsHidden;
    vPC.Enabled := ALayout.State > vsDisabled;

    if (ALayout.Tag and 1) > 0 then
      vPC.Images := TImageList(vUIBuilder.Images[32]);

    Result := vPC;
  end
  else if ALayout.Kind = lkPage then
  begin
    ALayout.Id := ALayout.Name;

    if vUIBuilder.IsMDIStyle and (ALayout.Tag = 11) then
    begin
      vForm := TForm.Create(nil);
      vForm.Caption := ALayout.Caption;
      vForm.Position := TFormPosition.Default;
      vForm.FormStyle := TFormStyle.Popup;
      { TODO 1 -oIterabili -cEvents : vForm.OnClose := OnCloseMDIForm }
      vForm.ShowHint := True;

      Result := vForm;
    end
    else begin
      vTab := TTabItem.Create(vParentControl);
      vTab.Text := ALayout.Caption;
      vTab.ImageIndex := AParent.GetImageID(ALayout.ImageID);
      vTab.Visible := ALayout.ShowCaption;

      Result := vTab;
    end;
  end
  else if ALayout.Kind = lkSplitter then
  begin
    vSplitter := TSplitter.Create(nil);
    vSplitter.Align := AlignToAlignLayout(ALayout.Align);
    vSplitter.Cursor := ALayout.Cursor;
    vSplitter.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);

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

      vPC := TTabControl.Create(nil);
      vPC.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
      vPC.Images := TImageList(vUIBuilder.Images[16]);
      vPC.TabPosition := TTabPosition.Bottom;
      if vParams.Values['PageLayout'] = 'Top' then
        vPC.TabPosition := TTabPosition.Top;
      vPC.Align := AlignToAlignLayout(ALayout.Align);
      vPC.Anchors := ALayout.Anchors;

      ALayout.Name := '-pages-';
      Result := vPC;
    end
    else
    begin
      vPanel := TPanel.Create(nil);
      vPanel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
      vPanel.Anchors := ALayout.Anchors;
      vPanel.Align := AlignToAlignLayout(ALayout.Align);
      CopyMargins(vPanel, ALayout);
      CopyPadding(vPanel, ALayout);
      if AView.DefinitionKind <> dkListField then
        vPanel.StyleLookup := IfThen(ALayout.BevelOuter <> lbkNone, '', 'pushpanel')
      else
        vPanel.StyleLookup := 'pushpanel';

      Result := vPanel;
    end;

    FreeAndNil(vParams);
  end
  else if ALayout.Kind = lkScrollBox then
  begin
    vBox := TScrollBox.Create(nil);
    vBox.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vBox.Anchors := ALayout.Anchors;
    vBox.Align := AlignToAlignLayout(ALayout.Align);
    CopyMargins(vBox, ALayout);
    CopyPadding(vBox, ALayout);

    Result := vBox;
  end
  else if ALayout.Kind = lkMemo then
  begin
    vPanel := TPanel.Create(nil);
    vPanel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vPanel.Anchors := ALayout.Anchors;
    vPanel.Align := AlignToAlignLayout(ALayout.Align);
    CopyMargins(vPanel, ALayout);
    CopyPadding(vPanel, ALayout);
    if AView.DefinitionKind <> dkListField then
      vPanel.StyleLookup := IfThen(ALayout.BevelOuter <> lbkNone, '', 'pushpanel')
    else
      vPanel.StyleLookup := 'pushpanel';

    ALayout.Id := Trim(ALayout.Caption);
    Result := vPanel;
  end
  else if ALayout.Kind = lkFrame then
  begin
    vForm := nil;

    if ALayout.StyleName = '' then
    begin
      Application.CreateForm(TFMXForm, vForm);
      Application.RealCreateForms;

      vForm.OnClose := DoMainFormClose;
      vForm.Position := TFormPosition.ScreenCenter;
      vForm.Caption := vDomain.AppTitle + ' (' + TUserSession(vInteractor.Session).CurrentUserName + ')';
    end
    // второстепенная автономная форма
    else if ALayout.StyleName = 'float' then
    begin
      if Assigned(AParent) then
      begin
        for i := 0 to AParent.Count - 1 do
        begin
          vArea := AParent.Areas[i];
          if (vArea.View = AView) and (GetRealControl(vArea) is TForm) then
            Exit(vArea);
        end;
      end;

      vForm := TFMXForm.Create(nil);

      vForm.OnClose := DoFloatFormClose;
      vForm.Position := TFormPosition.MainFormCenter;
      vForm.Caption := ALayout.Caption;
      vForm.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
     end
    // автономная форма со свободным отображением
    else if ALayout.StyleName = 'free' then
    begin
      vForm := TFMXForm.Create(nil);

      vForm.Position := TFormPosition.ScreenCenter;
      vForm.Caption := ALayout.Caption;
      vForm.BorderStyle := TfmxFormBorderStyle.None;
      vForm.BorderIcons := [];
      vForm.FormStyle := TFormStyle.StayOnTop;
    end
    // дочерняя модальная форма
    else if (ALayout.StyleName = 'child') or (ALayout.StyleName = 'modal') then
    begin
      vForm := TFMXForm.Create(nil);

      vForm.OnClose := DoChildFormClose;
      vForm.OnKeyDown := OnChildFormKeyDown;
      vForm.BorderIcons := [TBorderIcon.biSystemMenu];
      vForm.Position := TFormPosition.MainFormCenter;
      vForm.BorderStyle := TfmxFormBorderStyle.Single;
    end;

    if vForm = nil then
      Exit(nil);

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
      vMenu := TPopupMenu.Create(TComponent(vParentControl));
      vMenu.Images := TImageList(vUIBuilder.Images[16]);
      ALayout.Name := '-popup-';
      Result := vMenu;
    end
    else begin
      vMenuItem := TMenuItem.Create(nil);
      if ALayout.StyleName = 'action' then
      begin
        vMenuItem.Text := ALayout.Caption;
        vMenuItem.Hint := ALayout.Caption;
        vMenuItem.ImageIndex := AParent.GetImageID(ALayout.ImageID);
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
        vMenuItem.Text := ALayout.Caption;
        vMenuItem.Hint := ALayout.Caption;
        vMenuItem.ImageIndex := AParent.GetImageID(ALayout.ImageID);
        vMenuItem.Tag := NativeInt(AParent);
        vMenuItem.OnClick := AParent.OnActionMenuSelected;
      end
      else
        vMenuItem.Text := ALayout.Caption;

      if vParentControl is TPopupMenu then
        TPopupMenu(vParentControl).AddObject(vMenuItem)
      else
        TMenuItem(vParentControl).AddObject(vMenuItem);

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

function TFMXPresenter.DoCreateImages(const ADomain: TObject; const AImages: TImages; const ASize: Integer): TObject;
var
  vImageList: TImageList;
  vStream: TStream;
  vBitmap: TBitmap;
  i: Integer;

  procedure AppendImageList(const AImageList: TImageList; const ABitmap: TBitmap);
  var
    vBitmapItem: TCustomBitmapItem;
    vSource: TCustomSourceItem;
    vDestination: TCustomDestinationItem;
    vLayer: TLayer;
  begin
    vSource := AImageList.Source.Add;
    vSource.MultiResBitmap.SizeKind := TSizeKind.Custom;
    vSource.MultiResBitmap.Width := ABitmap.Width;
    vSource.MultiResBitmap.Height := ABitmap.Height;
    vBitmapItem := vSource.MultiResBitmap.Add;
    vBitmapItem.Bitmap.Assign(ABitmap);

    vDestination := AImageList.Destination.Add;
    vLayer := vDestination.Layers.Add;
    vLayer.SourceRect.Rect := TRectF.Create(TPoint.Zero, vSource.MultiResBitmap.Width,
      vSource.MultiResBitmap.Height);
    vLayer.Name := vSource.Name;
  end;

begin
  vImageList := TImageList.Create(nil);
  Result := vImageList;

  vBitmap := TBitmap.Create;
  vImageList.BeginUpdate;
  try
    for i := 0 to AImages.Count - 1 do
    begin
      vStream := AImages[i];
      vStream.Position := 0;
      vBitmap.LoadFromStream(vStream);
      AppendImageList(vImageList, vBitmap);
    end;
  finally
    vImageList.EndUpdate;
    FreeAndNil(vBitmap);
  end;
end;

function TFMXPresenter.GetImagePlaceholder(const ASize: Integer): TStream;
var
  vPlaceholder: TBitmap;
  vResDiv8: Integer;
begin
  vPlaceholder := TBitmap.Create;
  vPlaceholder.SetSize(ASize, ASize);
  vResDiv8 := Max(ASize div 8, 1);
  vPlaceholder.Canvas.BeginScene;
  try
    vPlaceholder.Canvas.Stroke.Thickness := 1;
    vPlaceholder.Canvas.Stroke.Color := TAlphaColorRec.Gray;
    vPlaceholder.Canvas.DrawRect(RectF(vResDiv8, vResDiv8, ASize - vResDiv8, ASize - vResDiv8), 0, 0, AllCorners, 1);
  finally
    vPlaceholder.Canvas.EndScene;
  end;

  Result := TMemoryStream.Create;
  vPlaceholder.SaveToStream(Result);
end;

function TFMXPresenter.GetNativeControlClass: TNativeControlClass;
begin
  Result := TFMXControl;
end;

procedure TFMXPresenter.OnChildFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var
  vHandled: Boolean;
begin
  vHandled := False;
  DoChildFormKeyDown(Sender, Shift, Key, vHandled);
  if vHandled then
    Exit;
end;

function TFMXPresenter.DoLogin(const ADomain: TObject): TInteractor;
begin
  Result := inherited DoLogin(ADomain);
  if not Assigned(Result) then
    Exit;
end;

procedure TFMXPresenter.DoLogout(const AInteractor: TInteractor);
begin
  inherited DoLogout(AInteractor);
end;

function TFMXPresenter.ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult;
var
  vView: TView;
  vForm: TForm;
  vCaption: string;
begin
  Result := drNone;
  if (AAreaName = '') or (AAreaName = 'float') or (AAreaName = 'free') then
  begin
    vForm := TForm(GetRealControl(AArea));
    vForm.Show;
  end
  else if (AAreaName = 'child') or (AAreaName = 'modal') then
  begin
    vForm := TForm(GetRealControl(AArea));
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
        AArea.Parent.RemoveArea(AArea)
      else
        AArea.Release;
      vForm.Free;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
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
{$ENDIF}

(*function TempPath : String;
begin
  {$IFDEF IOS}
  Result := TPath.GetLibraryPath+TPath.DirectorySeparatorChar;
  {$ELSE}
  Result := TPath.GetPublicPath+TPath.DirectorySeparatorChar;
  {$ENDIF}
end;
ну и соответственно прибавляю имя файла + расширение, т.е TempPath+<SomeName>+'.'+<Ext>*)

procedure TFMXPresenter.DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False);
var
  vExtFile: string;
{$IFDEF MSWINDOWS}
  vExecResult: Cardinal;
{$ELSE IFDEF ANDROID}
  vMime: JMimeTypeMap;
  vExtToMime: JString;
  vIntent: JIntent;
{$ELSE IFDEF IOS}
  vUrl: NSURL;
{$ENDIF}
begin
  vExtFile := LowerCase(Copy(ExtractFileExt(aFileName), 2, Length(aFileName)));

{$IFDEF ANDROID}
  vMime := TJMimeTypeMap.JavaClass.getSingleton();
  vExtToMime := vMime.getMimeTypeFromExtension(StringToJString(ExtFile));
  if ExtToMime <> nil then
  begin
    vIntent := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_VIEW);
    vIntent.setDataAndType(StrToJURI('file:' + AFileName), vExtToMime);
    try
      SharedActivity.startActivity(vIntent);
    except
      ShowError('Невозможно отобразить файл');
    end;
  end;
{$ELSE IFDEF MSWINDOWS}
  vExecResult := ShellExecuteU(0, AFileName, '', '', Await);
  if (vExecResult = SE_ERR_NOASSOC) and (ADefaultApp <> '') then
    ShellExecuteU(0, ADefaultApp, AFileName, '', Await);
{$ELSE}
  vUrl := TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(AFileName)));

  if SharedApplication.canOpenURL(vUrl) then
    SharedApplication.openURL(vUrl)
  else
    ShowError('Невозможно отобразить файл');
{$ENDIF}
end;

procedure TFMXPresenter.DoRun(const AParameter: string);
var
  vDomain: TDomain;
  vInteractor: TInteractor;
begin
  Application.Title := cPlatformTitle;
  Application.Initialize;

  vDomain := _Platform.Domains[0];
  vInteractor := Login(vDomain);
  if Assigned(vInteractor) and (AParameter <> '') then
    vDomain.ExecuteDefaultAction(TUserSession(vInteractor.Session), AParameter);

  if Assigned(vInteractor) then
    Application.Run;
end;

procedure TFMXPresenter.DoSetCursor(const ACursorType: TCursorType);
begin
  if Assigned(Application.MainForm) then
    Application.MainForm.Cursor := cCursors[ACursorType];
end;

function TFMXPresenter.DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult;
var
  vButtons: TMsgDlgButtons;
  vMsgDlgType: TMsgDlgType;
  vDefaultBtn: TMsgDlgBtn;
  vResult: TDialogResult;
begin
  if drOk in ADialogActions then
  begin
    vButtons := [TMsgDlgBtn.mbOk, TMsgDlgBtn.mbCancel];
    vMsgDlgType := TMsgDlgType.mtWarning;
    vDefaultBtn := TMsgDlgBtn.mbCancel;
  end
  else if drYes in ADialogActions then
  begin
    if drCancel in ADialogActions then
    begin
      vButtons := [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel];
      vMsgDlgType := TMsgDlgType.mtConfirmation;
      vDefaultBtn := TMsgDlgBtn.mbYes;
    end
    else begin
      vButtons := [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo];
      vMsgDlgType := TMsgDlgType.mtConfirmation;
      vDefaultBtn := TMsgDlgBtn.mbYes;
    end
  end
  else
    Exit(drCancel);

  TDialogService.MessageDialog(AText, vMsgDlgType, vButtons, vDefaultBtn, 0, TInputCloseDialogProc(procedure (const AResult: TModalResult)
    begin
      vResult := ModalResultToDialogResult(AResult);
    end));
  Result := vResult;
end;

procedure TFMXPresenter.DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType);
begin
  TDialogService.ShowMessage(AText);
end;

function TFMXPresenter.DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt,
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
    vOpenDialog.Options := vOpenDialog.Options + [TOpenOption.ofFileMustExist, TOpenOption.ofPathMustExist];;
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

function TFMXPresenter.DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean;
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

procedure TFMXPresenter.DoStop;
begin
  Application.Terminate;
end;

procedure TFMXPresenter.DoUnfreeze;
begin
  Application.ProcessMessages;
end;

procedure TFMXPresenter.SetApplicationUI(const AAppTitle, AIconName: string);
begin
  Application.Title := AAppTitle;
  //if FileExists(AIconName) then
  //  Application.Icon.LoadFromFile(AIconName);
end;

initialization

TBaseModule.RegisterModule('UI', '', 'FMX', TFMXPresenter);

end.
