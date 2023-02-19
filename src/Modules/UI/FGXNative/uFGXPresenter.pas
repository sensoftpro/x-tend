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

unit uFGXPresenter;

interface

uses
  System.Classes, System.SysUtils, System.UITypes,

  FGX.Application, FGX.Forms, FGX.Dialogs,

  uPresenter, uSettings, uInteractor, uUIBuilder, uSession, uView, fgxArea,
  uLayout, uConsts, uDefinition, uIcon;

type
  TImageResolution = (ir16x16, ir24x24, ir32x32);

type
  TFGXPresenter = class(TPresenter)
  protected
    procedure OnChildFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  protected
    procedure DoRun(const AParameter: string); override;
    procedure DoUnfreeze; override;
    procedure DoStop; override;

    procedure DoLogin(const ADomain: TObject); override;
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
    function CreateControl(const AParent: TUIArea; const AView: TView; const ALayout: TLayout;
      const AParams: string = ''): TObject; override;

    function GetImagePlaceholder(const ASize: Integer): TStream; override;
    function DoCreateImages(const ADomain: TObject; const AImages: TImages; const ASize: Integer): TObject; override;

    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); override;
  public
    procedure ShowUIArea(const AArea: TUIArea; const AAreaName: string; const ACaption: string); override;
  end;

{procedure CopyTextSettings(const ATextSettings: TTextSettings; const ALayout: TLayout);
procedure CopyMargins(const AControl: TControl; const ALayout: TLayout);
procedure CopyPadding(const AControl: TControl; const ALayout: TLayout);
procedure CopyPenSettings(const APen: TStrokeBrush; const ALayout: TLayout);
procedure CopyBrushSettings(const ABrush: TBrush; const ALayout: TLayout); }

implementation

uses
  System.Generics.Collections, System.Math, System.Types, System.StrUtils,

  uModule, uDomain, uPlatform, uUtils, uEntityList, uConfiguration,
  uChangeManager, uEntity;

{procedure CopyTextSettings(const ATextSettings: TTextSettings; const ALayout: TLayout);
begin
  ATextSettings.Font.Family := ALayout.Font.Family;
  ATextSettings.FontColor := ALayout.Font.Color;
  ATextSettings.Font.Size := ALayout.Font.Size * 96 / 72;
  ATextSettings.Font.Style := ALayout.Font.Style;
end;

procedure CopyMargins(const AControl: TControl; const ALayout: TLayout);
begin
  AControl.Margins.Left := Max(ALayout.Margins.Left, 1);
  AControl.Margins.Top := Max(ALayout.Margins.Top, 1);
  AControl.Margins.Right := Max(ALayout.Margins.Right, 1);
  AControl.Margins.Bottom := Max(ALayout.Margins.Bottom, 1);
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

{ TFGXPresenter }

function TFGXPresenter.CreateControl(const AParent: TUIArea; const AView: TView;
  const ALayout: TLayout; const AParams: string): TObject;
var
  vDomain: TDomain;
  vInteractor: TInteractor;
  vUIBuilder: TUIBuilder;
{  vShape: TShape;
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
  vBevelPanel: TPanel; }
  //vForm: TForm;
  i: Integer;
  vArea: TUIArea;
  //vParentControl: TFmxObject;
begin
  Result := nil;

  vInteractor := TInteractor(AView.Interactor);
  vUIBuilder := vInteractor.UIBuilder;
  vDomain := TDomain(vInteractor.Domain);
(*  vParentControl := TFmxObject(GetRealControl(AParent));

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
    vShape.Visible := not ALayout.Transparent;
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
    vLabel.StyledSettings := [];
    vLabel.Anchors := ALayout.Anchors;

    if (ALayout.Caption  = '$') and (ALayout.UIParams = 'Caption') then
    begin
      if AView.DefinitionKind = dkCollection then
        vLabel.Text := AParent.GetTranslation(TDefinition(AView.Definition))
      else if AView.DefinitionKind in [dkListField..dkComplexField] then
        vLabel.Text := AParent.GetFieldTranslation(TFieldDef(AView.Definition))
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
    if Assigned(ALayout.Image_Picture) then
    try
      vImage.Bitmap.LoadFromStream(ALayout.Image_Picture);
    except
    end;

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

      ALayout.Id := ALayout.Name;
      Result := vTab;
    end;
  end
  else if ALayout.Kind = lkBevel then
  begin
    vBevelPanel := TPanel.Create(nil);
    vBevelPanel.SetBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);
    vBevelPanel.Align := AlignToAlignLayout(ALayout.Align);
    vBevelPanel.StyleLookup := 'pushpanel';
    CopyMargins(vBevelPanel, ALayout);

    ALayout.Id := '-bevel-';
    Result := vBevelPanel;
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
//      vForm.Position := TFormPosition.ScreenCenter;
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
//      vForm.Position := TFormPosition.MainFormCenter;
      vForm.Caption := ALayout.Caption;
      vForm.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
     end
    // автономная форма со свободным отображением
    else if ALayout.StyleName = 'free' then
    begin
      vForm := TFMXForm.Create(nil);

//      vForm.Position := TFormPosition.ScreenCenter;
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
//      vForm.Position := TFormPosition.MainFormCenter;
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
    Assert(False, 'Класс №' + IntToStr(Integer(ALayout.Kind)) + ' не реализован' )
  else
    Assert(False, 'Пустой класс для лэйаута');  *)
end;

procedure TFGXPresenter.CreateMainForm(const ASession: TUserSession);
begin
  inherited CreateMainForm(ASession);

  if Assigned(ASession) and Assigned(ASession.Interactor) then
  begin
    if FStartParameter <> '' then
      TDomain(ASession.Domain).ExecuteDefaultAction(ASession, FStartParameter);

    //Application.Run;
  end;
end;

function TFGXPresenter.DoCreateImages(const ADomain: TObject; const AImages: TImages; const ASize: Integer): TObject;
var
  //vImageList: TImageList;
  vStream: TStream;
  //vBitmap: TBitmap;
  i: Integer;

  {procedure AppendImageList(const AImageList: TImageList; const ABitmap: TBitmap);
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
  end; }

begin
{  vImageList := TImageList.Create(nil);
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
  end; }
end;

function TFGXPresenter.GetImagePlaceholder(const ASize: Integer): TStream;
//var
//  vPlaceholder: TBitmap;
//  vResDiv8: Integer;
begin
{  vPlaceholder := TBitmap.Create;
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
  vPlaceholder.SaveToStream(Result);  }
end;

function TFGXPresenter.GetNativeControlClass: TNativeControlClass;
begin
  Result := TFGXControl;
end;

procedure TFGXPresenter.OnChildFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var
  vHandled: Boolean;
begin
  vHandled := False;
  DoChildFormKeyDown(Sender, Shift, Key, vHandled);
  if vHandled then
    Exit;
end;

procedure TFGXPresenter.DoLogin(const ADomain: TObject);
begin
  inherited DoLogin(ADomain);
end;

procedure TFGXPresenter.DoLogout(const AInteractor: TInteractor);
begin
  inherited DoLogout(AInteractor);
end;

procedure TFGXPresenter.ShowUIArea(const AArea: TUIArea;
  const AAreaName: string; const ACaption: string);
//var
//  vForm: TForm;
begin
{  if (AAreaName = '') or (AAreaName = 'float') or (AAreaName = 'free') then
  begin
    vForm := TForm(GetRealControl(AArea));
    vForm.Show;
  end
  else if (AAreaName = 'child') or (AAreaName = 'modal') then
  begin
    vForm := TForm(GetRealControl(AArea));
    vForm.Caption := ACaption;
    vForm.ShowModal;
  end;  }
end;

procedure TFGXPresenter.DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False);
var
  vExtFile: string;
{$IFDEF MSWINDOWS}
  vExecResult: Cardinal;
{$ELSE} {$IFDEF ANDROID}
//  vMime: JMimeTypeMap;
//  vExtToMime: JString;
//  vIntent: JIntent;
{$ELSE} {$IFDEF IOS}
  vUrl: NSURL;
{$ENDIF} {$ENDIF} {$ENDIF}
begin
  vExtFile := LowerCase(Copy(ExtractFileExt(aFileName), 2, Length(aFileName)));

{$IFDEF ANDROID}
{  vMime := TJMimeTypeMap.JavaClass.getSingleton();
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
  end;}
{$ELSE} {$IFDEF IOS}
  vUrl := TNSURL.Wrap(TNSURL.OCClass.URLWithString(StrToNSStr(AFileName)));

  if SharedApplication.canOpenURL(vUrl) then
    SharedApplication.openURL(vUrl)
  else
    ShowError('Невозможно отобразить файл');
{$ENDIF} {$ENDIF}
end;

procedure TFGXPresenter.DoRun(const AParameter: string);
var
  vDomain: TDomain;
begin
  inherited DoRun(AParameter);

  Application.Initialize;

  vDomain := _Platform.Domains[0];
  Login(vDomain);

  TfgDialogs.ShowMessage('Домен загружен');
end;

procedure TFGXPresenter.DoSetCursor(const ACursorType: TCursorType);
begin
  //if Assigned(Application.MainForm) then
  //  Application.MainForm.Cursor := cCursors[ACursorType];
end;

procedure TFGXPresenter.DoShowDialog(const ACaption, AText: string;
  const ADialogActions: TDialogResultSet; const AOnClose: TCloseProc);
var
  vButtons: TMsgDlgButtons;
  vMsgDlgType: TMsgDlgType;
  vDefaultBtn: TMsgDlgBtn;
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
    Exit;

  //TDialogService.MessageDialog(AText, vMsgDlgType, vButtons, vDefaultBtn, 0, TInputCloseDialogProc(procedure(const AResult: TModalResult)
  //  begin
  //    if Assigned(AOnClose) then
  //      AOnClose(ModalResultToDialogResult(AResult));
  //  end));
end;

procedure TFGXPresenter.DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType);
begin
  //TDialogService.ShowMessage(AText);
end;

procedure TFGXPresenter.DoShowOpenDialog(const AFileName: string; const ATitle,
  AFilter, ADefaultExt, ADefaultDir: string; const AOnClose: TCloseTextProc);
var
//  vOpenDialog: TOpenDialog;
  vFileName: string;
begin
  if not Assigned(AOnClose) then
    Exit;

  vFileName := AFileName;
{  vOpenDialog := TOpenDialog.Create(nil);
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

    if vOpenDialog.Execute then
    begin
      vFileName := vOpenDialog.FileName;
      AOnClose(drOk, vFileName);
    end
    else
      AOnClose(drCancel, vFileName);
  finally
    FreeAndNil(vOpenDialog);
  end;  }
end;

procedure TFGXPresenter.DoShowSaveDialog(const AFileName: string; const ATitle,
  AFilter, ADefaultExt: string; const AOnClose: TCloseTextProc);
var
  //vSaveDialog: TSaveDialog;
  vFileName: string;
begin
  if not Assigned(AOnClose) then
    Exit;

  vFileName := AFileName;
{  vSaveDialog := TSaveDialog.Create(nil);
  try
    vSaveDialog.Title := ATitle;
    vSaveDialog.Filter := AFilter;
    vSaveDialog.DefaultExt := ADefaultExt;
    vSaveDialog.FileName := AFileName;

    if vSaveDialog.Execute then
    begin
      vFileName := vSaveDialog.FileName;
      AOnClose(drOk, vFileName);
    end
    else
      AOnClose(drCancel, vFileName);
  finally
    FreeAndNil(vSaveDialog);
  end;  }
end;

procedure TFGXPresenter.DoStop;
begin
  //Application.Terminate;
end;

procedure TFGXPresenter.DoUnfreeze;
begin
  //Application.ProcessMessages;
end;

procedure TFGXPresenter.SetApplicationUI(const AAppTitle, AIconName: string);
begin
  //Application.Title := AAppTitle;
  //if FileExists(AIconName) then
  //  Application.Icon.LoadFromFile(AIconName);
end;

initialization

TBaseModule.RegisterModule('UI', '', 'FGXNative', TFGXPresenter);

end.
