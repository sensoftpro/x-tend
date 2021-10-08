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

unit uWinVCLPresenter;

interface

uses
  Messages, Classes, Generics.Collections, Generics.Defaults, ActnList, uConsts, StdCtrls, Buttons,
  ExtCtrls, TypInfo, Types, ComCtrls, SysUtils, Windows, Graphics,
  Variants, Controls, Forms, Mask, Menus,

  uDefinition, uPresenter, uInteractor, uView,

  StartForm, LoginForm, DebugInfoForm, SplashForm, uUIBuilder, vclArea;

type
  TImageResolution = (ir16x16, ir24x24, ir32x32);

type
  TWinVCLPresenter = class(TPresenter)
  private
    function InternalMessageBox(const AText, ACaption: string; Flags: Longint; ALangID: Word): Integer;
    procedure LoadImages(const AInteractor: TInteractor; const AImageList: TDragImageList; const AResolution: Integer);
  private
    FRowStyle: TObject;
    FLongOperationCount: Integer;
    FPrevCursor: TCursor;
  protected
    //FOnRFIDRead: TRFIDReadEvent;
    FTrayIcon: TTrayIcon;
    FStartForm: TStartFm;
    [Weak] FDebugForm: TDebugFm;
    [Weak] FSplashForm: TSplashFm;
    procedure DoFinalize;
    function ShowLoginForm(const AAppTitle: string; var ALoginName, APass{, ARFID}: string): Boolean;
    procedure DoChildFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoChildFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoMainFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoAuthFormNavigated(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
    procedure DoAuthFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoTrayIconClick(Sender: TObject);
    procedure DoDebugFormClose(Sender: TObject; var Action: TCloseAction);
    procedure OnShortCut(var Msg: TWMKey; var Handled: Boolean);
    function MessageTypeToMBFlags(const AMessageType: TMessageType): Integer;
  protected
    function GetPresenterName: string; override;

    procedure DoRun(const AParameter: string); override;
    procedure DoStop; override;

    function DoLogin(const ADomain: TObject): TInteractor; override;
    procedure DoLogout(const AInteractor: TInteractor); override;
    procedure DoAuthorize(const AAccount: TObject; const AURL: string; const AWidth, AHeight: Integer;
      const AOnNavigated: TNavigateEvent); override;

    procedure DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType); override;
    function DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult; override;
    procedure DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False); override;
    function DoSelectFile(var AFileName: string; const ADirectory: string = ''): Boolean; override;
    function DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt, ADefaultDir: string): Boolean; override;
    function DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean; override;

    procedure DoToggleUI(const AVisible: Boolean); override;
    procedure DoCloseAllPages(const AInteractor: TInteractor); override;

    function DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject; override;
  public
    constructor Create(const AStyleName: string); override;
    destructor Destroy; override;

    procedure SetAsMainForm(const AForm: TForm);

    function CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView;
      const AAreaName: string; const ACallback: TNotifyEvent = nil): TUIArea; override;
    function ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult; override;
    procedure CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea); override;
    function CreateFieldArea(const AParentArea: TUIArea; const ALayout: TObject;
      const AView: TView; const AStyleName, AParams: string): TUIArea; override;
    function ShowPage(const AInteractor: TInteractor; const APageType: string; const AParams: TObject = nil): TDialogResult; override;
    procedure ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement); override;
    procedure ShowLayout(const AInteractor: TInteractor; const ATargetAreaName, ALayoutName: string); override;

    procedure EnumerateControls(const ALayout: TObject; const AControls: TList<TObject>); override;
    function CreateLayoutArea(const ALayoutKind: TLayoutKind; const AParams: string = ''): TObject; override;
    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); override;
    procedure SetLayoutCaption(const ALayout: TObject; const ACaption: string); override;
    function GetLayoutCaption(const ALayout: TObject): string; override;
    function GetLayoutKind(const ALayout: TObject): TLayoutKind; override;

    function GetWidthByType(const AWidth: Integer; const AFieldDef: TFieldDef): Integer;

    procedure LongOperationStarted;
    procedure LongOperationEnded;

    property RowStyle: TObject read FRowStyle;
  end;

implementation

uses
  Dialogs, Math, MultiMon, StrUtils, ShellAPI, WinInet, SHDocVw, ActiveX,
  cxGraphics, dxGDIPlusClasses, cxImage, cxEdit, cxPC, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxMemo,

  uPlatform, uModule, uDomain, uUtils, uConfiguration, uChangeManager, uIcon, uEntity, uEntityList, uSession,
  uManagedForm, AboutForm, ReportConfigureForm, OptionsForm, uOAuthForm;

type
  TLoginedProc = procedure(const AInteractor: TInteractor) of object;
  TBeforeUIClosingFunc = function(const AInteractor: TInteractor): Boolean of object;

{ TWinVCLPresenter }

procedure TWinVCLPresenter.ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement);
var
  vForm: TForm;
begin
  if AInteractor.Layout <> 'mdi'  then
    Exit;

  vForm := TForm(TVCLArea(AInteractor.UIBuilder.RootArea).Control);
  case AArrangeKind of
    waCascade: vForm.Cascade;
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
    vForm := TForm(TVCLArea(AOldArea).Control);
    CloseAllPages(AInteractor);
    TUIAreaCrack(AOldArea).ClearContent;

    SetAsMainForm(TForm(TVCLArea(ANewArea).Control));

    vForm.Close;
    AOldArea.Free;
  end
  else
    SetAsMainForm(TForm(TVCLArea(ANewArea).Control));
end;

constructor TWinVCLPresenter.Create(const AStyleName: string);
begin
  inherited Create(AStyleName);
  FLongOperationCount := 0;
  FRowStyle := TcxStyle.Create(nil);
end;

function TWinVCLPresenter.CreateFieldArea(const AParentArea: TUIArea; const ALayout: TObject;
  const AView: TView; const AStyleName, AParams: string): TUIArea;
var
  vPos: Integer;
  vParams, vViewName: string;
  vFieldAreaClass: TVCLFieldAreaClass;
begin
  vViewName := AStyleName;
  vParams := '';
  vPos := Pos('?', AStyleName);
  if vPos > 0 then
  begin
    if Length(AStyleName) - vPos >= 2 then
      vParams := Copy(AStyleName, vPos + 1, Length(AStyleName) - vPos);
    vViewName := Copy(AStyleName, 1, vPos - 1);
  end;

  if AView.DefinitionKind = dkEntity then
    vFieldAreaClass := TVCLFieldAreaClass(GetUIClass(GetPresenterName, uiEntityEdit, vViewName))
  else if ALayout is TPageControl then
    vFieldAreaClass := TVCLFieldAreaClass(GetUIClass(GetPresenterName,
      ItemTypeByFieldType(TFieldDef(AView.Definition).Kind), 'pages'))
  else
    vFieldAreaClass := TVCLFieldAreaClass(GetUIClass(GetPresenterName,
      ItemTypeByFieldType(TFieldDef(AView.Definition).Kind), vViewName));

  if vParams = '' then
    vParams := AParams;

  Result := vFieldAreaClass.Create(AParentArea, ALayout, AView, vParams);
end;

function TWinVCLPresenter.CreateLayoutArea(const ALayoutKind: TLayoutKind; const AParams: string): TObject;
var
  vParams: TStrings;
begin
  vParams := CreateDelimitedList(AParams);
  case ALayoutKind of
    lkPanel: begin
        Result := TPanel.Create(nil);
        TPanel(Result).BevelOuter := bvNone;
      end;
    lkPage: begin
        Result := TTabSheet.Create(nil);
        TTabSheet(Result).Caption := vParams.Values['Caption'];
        TTabSheet(Result).ImageIndex := StrToIntDef(vParams.Values['ImageIndex'], -1);
        TTabSheet(Result).Name := vParams.Values['Name'];
        TTabSheet(Result).Tag := 11;
      end;
    lkFrame: Result := TFrame.Create(nil);
  else
    Result := nil;
  end;
  FreeAndNil(vParams);
end;

function TWinVCLPresenter.CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea;
  const AView: TView; const AAreaName: string; const ACallback: TNotifyEvent = nil): TUIArea;
var
  vForm: TForm;
  vTimer: TTimer;
begin
  if AAreaName = '' then
  begin
    Application.CreateForm(TForm, vForm);

    if (AInteractor.Layout = 'mdi') then
      vForm.FormStyle := fsMDIForm
    else
      vForm.FormStyle := fsNormal;

    vForm.OnClose := DoMainFormClose;
    vForm.Position := poScreenCenter;
    vForm.ShowHint := True;
    vForm.Caption := TDomain(AInteractor.Domain).AppTitle + ' (' + TUserSession(AInteractor.Session).CurrentUserName + ')';

    vForm.DisableAlign;
    try
      Result := TVCLArea.Create(AParent, AView, '', vForm);
    finally
      vForm.EnableAlign;
    end;
  end
  // второстепенная автономная форма
  else if AAreaName = 'float' then
  begin
    vForm := TForm.Create(nil);
    vForm.ShowHint := True;
    vForm.Position := poMainFormCenter;
    vForm.Font.Size := 12;

    vForm.DisableAlign;
    try
      Result := TVCLArea.Create(AParent, AView, '', vForm);
    finally
      vForm.EnableAlign;
    end;
  end
  // дочерняя модальная форма
  else if AAreaName = 'child' then
  begin
    vForm := TForm.Create(nil);
    vForm.ShowHint := True;

    vTimer := nil;

    if Assigned(ACallback) then
    begin
      vForm.BorderIcons := [];
      vTimer := TTimer.Create(vForm);
      vTimer.Enabled := False;
      vTimer.OnTimer := ACallback;
    end
    else begin
      vForm.OnClose := DoChildFormClose;
      vForm.OnKeyDown := DoChildFormKeyDown;
      vForm.KeyPreview := True;
      vForm.BorderIcons := [biSystemMenu];
    end;
    vForm.Position := poMainFormCenter;
    vForm.Font.Size := 12;
    vForm.BorderStyle := bsSingle;  // for layouted form this property will be changed when assigned cEditFormResizable flag in Tag
    vForm.DisableAlign;
    try
      Result := TVCLArea.Create(AParent, AView, '', vForm);
      if Assigned(ACallback) and Assigned(vTimer) then
        ACallback(vTimer);
    finally
      vForm.EnableAlign;
    end;
  end
  else
    Result := nil;
end;

destructor TWinVCLPresenter.Destroy;
begin
  FreeAndNil(FRowStyle);
  FreeAndNil(FSplashForm);

  DoFinalize;

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

procedure TWinVCLPresenter.OnShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  vActiveInteractor: TInteractor;
  vView: TView;
begin
  vActiveInteractor := ActiveInteractor;
  if Assigned(vActiveInteractor)
    and (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_MENU) < 0) and (GetKeyState(VK_SHIFT) < 0) then
  begin
    if Msg.CharCode = Ord('D') then
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
      TDomain(vActiveInteractor.Domain).ReloadChanges(nil, nil);
    end;
  end;
end;

procedure TWinVCLPresenter.DoFinalize;
begin

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
  Result.UIBuilder.Navigate(nil, '', vMainFormName, ''{, Result.UIHolder});

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

procedure TWinVCLPresenter.ShowLayout(const AInteractor: TInteractor; const ATargetAreaName, ALayoutName: string);
begin
  if Assigned(AInteractor) then
    AInteractor.UIBuilder.Navigate(nil, ATargetAreaName, ALayoutName);
end;

function TWinVCLPresenter.ShowLoginForm(const AAppTitle: string; var ALoginName, APass{, ARFID}: string): Boolean;
var
  vLoginForm: TLoginFm;
begin
  vLoginForm := TLoginFm.Create(nil);

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

  FreeAndNil(vLoginForm);
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
    if TDomain(AInteractor.Domain).DeploymentType = 'dev' then
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
    vPageClass := TManagedFormClass(GetPageClass(GetPresenterName, APageType));
    if Assigned(vPageClass) then
      TManagedForm.ShowPage(vPageClass, AInteractor);
  end;
end;

function TWinVCLPresenter.ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult;
var
  vArea: TVCLArea absolute AArea;
  vView: TView;
  vForm: TForm;

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
    vForm := TForm(vArea.Control);
    vForm.Show;
  end
  else if AAreaName = 'child' then
  begin
    vForm := TForm(vArea.Control);
    vForm.ShowHint := True;
    vView := vArea.View;

    // Definition может быть от листового поля
    if Assigned(vView.Definition) then
    begin
      if vForm.Caption = '' then
      begin
        if vView.DefinitionKind = dkObjectField then
          vForm.Caption := TDomain(AInteractor.Domain).TranslateFieldDef(TFieldDef(vView.Definition))
        else
          vForm.Caption := TDomain(AInteractor.Domain).TranslateDefinition(TDefinition(vView.Definition));

        if Pos(AOptions, 'NoExtCaption') < 1 then
        begin
          if vView.DefinitionKind = dkAction then
            vForm.Caption := 'Параметры: ' + vForm.Caption
          else if vView.State > vsSelectOnly {and Assigned(vArea.Holder) - у параметров нет холдера} then
            vForm.Caption := 'Редактирование: ' + vForm.Caption
          else
            vForm.Caption := 'Просмотр: ' + vForm.Caption;
        end;
      end;
    end
    else
      vForm.Caption := TDomain(AInteractor.Domain).AppTitle;

    try
      Result := ModalResultToDialogResult(vForm.ShowModal);
    finally
      vArea.SetHolder(nil);
      if Assigned(vArea.Parent) then
        vArea.Parent.RemoveArea(AArea);
      vForm.Free;
    end;
  end;
end;

procedure DeleteIECache;
var
  lpEntryInfo: PInternetCacheEntryInfo;
  hCacheDir: LongWord;
  dwEntrySize: LongWord;
begin
  dwEntrySize := 0;
  FindFirstUrlCacheEntry(nil, TInternetCacheEntryInfo(nil^), dwEntrySize);
  GetMem(lpEntryInfo, dwEntrySize);
  if dwEntrySize > 0 then lpEntryInfo^.dwStructSize := dwEntrySize;
  hCacheDir := FindFirstUrlCacheEntry(nil, lpEntryInfo^, dwEntrySize);
  if hCacheDir <> 0 then
  begin
    repeat
      DeleteUrlCacheEntry(lpEntryInfo^.lpszSourceUrlName);
      FreeMem(lpEntryInfo, dwEntrySize);
      dwEntrySize := 0;
      FindNextUrlCacheEntry(hCacheDir, TInternetCacheEntryInfo(nil^), dwEntrySize);
      GetMem(lpEntryInfo, dwEntrySize);
      if dwEntrySize > 0 then lpEntryInfo^.dwStructSize := dwEntrySize;
    until not FindNextUrlCacheEntry(hCacheDir, lpEntryInfo^, dwEntrySize);
  end;
  FreeMem(lpEntryInfo, dwEntrySize);
  FindCloseUrlCache(hCacheDir);
end;

type
  PAuthData = ^TAuthData;
  TAuthData = record
    Account: TObject;
    NavFunction: TNavigateEvent;
  end;

procedure TWinVCLPresenter.DoAuthFormClose(Sender: TObject; var Action: TCloseAction);
var
  vAuthForm: TfrmOAuth absolute Sender;
  vAuthData: PAuthData;
begin
  vAuthForm.wbrAuthData.OnNavigateComplete2 := nil;
  if vAuthForm.wbrAuthData.Busy then
    vAuthForm.wbrAuthData.Stop;

  vAuthData := PAuthData(vAuthForm.Tag);
  Dispose(vAuthData);

  Action := caFree;
end;

procedure TWinVCLPresenter.DoAuthFormNavigated(ASender: TObject; const pDisp: IDispatch; const URL: OleVariant);
var
  vAuthForm: TfrmOAuth;
  vAuthData: PAuthData;
begin
  vAuthForm := TfrmOAuth(TWebBrowser(ASender).Owner);
  vAuthData := PAuthData(vAuthForm.Tag);
  if not Assigned(vAuthData) then
    Exit;
  if not Assigned(vAuthData^.NavFunction) then
    Exit;

  if vAuthData^.NavFunction(vAuthData^.Account, URL) then
    vAuthForm.Close;
end;

procedure TWinVCLPresenter.DoAuthorize(const AAccount: TObject; const AURL: string;
  const AWidth, AHeight: Integer; const AOnNavigated: TNavigateEvent);
var
  vAuthForm: TfrmOAuth;
  vAuthData: PAuthData;
  vFlags: OleVariant;
begin
  try
    DeleteIECache;
  except
  end;

  vAuthForm := TfrmOAuth.Create(nil);
  vAuthForm.ClientWidth := 680;
  vAuthForm.ClientHeight := 540;
  vAuthForm.OnClose := DoAuthFormClose;

  New(vAuthData);
  vAuthData^.Account := AAccount;
  vAuthData^.NavFunction := AOnNavigated;
  vAuthForm.Tag := Integer(vAuthData);

  vAuthForm.wbrAuthData.OnNavigateComplete2 := DoAuthFormNavigated;
  try
    vFlags := Cardinal(navNoReadFromCache or navNoWriteToCache or navNoHistory);
    vAuthForm.wbrAuthData.Navigate2(AURL);
    vAuthForm.Show;
  except
    on E: Exception do
    begin
      vAuthForm.Free;
      raise;
    end;
  end;
end;

procedure TWinVCLPresenter.DoChildFormClose(Sender: TObject; var Action: TCloseAction);
var
  vForm: TForm;
  vArea: TUIArea;
  vInteractor: TInteractor;
  vView: TView;
  vHolder: TChangeHolder;
  vRes: TDialogResult;

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
    if vView.DefinitionKind <> dkEntity then
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
  vHolder := TChangeHolder(vArea.Holder);
  vView := vArea.View;
  vInteractor := TInteractor(vArea.Interactor);

  if vForm.ModalResult = mrOk then
    DoValidation
  else begin
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
end;

procedure TWinVCLPresenter.DoChildFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  vForm: TForm;
  vFormArea: TVCLArea;
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
    vFormArea := TVCLArea(vForm.Tag);
    if Assigned(vFormArea) then
    begin
      vFormArea.UIBuilder.LastArea := nil;
      vView := vFormArea.View.ViewByName('Close');
      if not Assigned(vView) then
        vView := vFormArea.View.ViewByName('Cancel');
      if Assigned(vView) then
        vFormArea.ExecuteUIAction(vView);
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
    vMainForm := TForm(TVCLArea(AInteractor.UIBuilder.RootArea).Control);
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

  DoOnAppStarted;

  Application.Run;
end;

function TWinVCLPresenter.DoSelectFile(var AFileName: string; const ADirectory: string = ''): Boolean;
var
  vOpenDialog: TOpenDialog;
begin
  vOpenDialog := TOpenDialog.Create(nil);
  if ADirectory <> '' then
    vOpenDialog.InitialDir := ADirectory
  else if AFileName <> '' then
    vOpenDialog.InitialDir := ExtractFilePath(AFileName);

  try
    Result := vOpenDialog.Execute;
    if Result then
      AFileName := vOpenDialog.FileName;
  finally
    vOpenDialog.Free;
  end;
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

  vRes := InternalMessageBox(AText, ACaption, vFlags, MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL));
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
  InternalMessageBox(AText, vCaption, MB_OK or MessageTypeToMBFlags(AMessageType),
    MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US));
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

  Application.Terminate;
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

  (*

__fastcall TMainForm::TMainForm(TComponent* Owner)
    : TForm(Owner)
{
  TrayIcon->Hint = "Сообщение ABS";
  TrayIcon->AnimateInterval = 200;

  TrayIcon->BalloonTitle = "Восстановить окно.";
  TrayIcon->BalloonHint =
    "Дважды щелкните значок на панели задач, чтобы восстановить окно.";
  TrayIcon->BalloonFlags = bfInfo;
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::ApplicationEventsMinimize(TObject *Sender)
{
    TrayIcon->Visible=true;
    Application->Minimize();
    ShowWindow(Application->Handle, SW_HIDE);
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::N1Click(TObject *Sender)
{
  TrayIcon->Visible = false;
  Show();
  WindowState = wsNormal;
  Application->BringToFront();
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormResize(TObject *Sender)
{
  if (WindowState == wsMinimized)
        {
    Application->ShowMainForm = false;
    ShowWindow(Handle,SW_HIDE);
    ShowWindow(Application->Handle,SW_HIDE);
    TrayIcon->Visible = true;
        }
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::N2Click(TObject *Sender)
{
Application->Terminate();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FormActivate(TObject *Sender)
{
  WindowState = wsMinimized;

  TrayIcon->Visible = true;
  TrayIcon->Animate = true;
  ShowWindow(Application->Handle, SW_HIDE);

}
//---------------------------------------------------------------------------
void __fastcall TMainForm::TrayIconDblClick(TObject *Sender)
{
  TrayIcon->Visible = false;
  Show();
  WindowState = wsNormal;
  Application->BringToFront();
}
//---------------------------------------------------------------------------

void __fastcall TMainForm::FormCloseQuery(TObject *Sender, bool &CanClose)
{
  CanClose = false;
  ShowWindow(Application->Handle, SW_HIDE);
  WindowState = wsMinimized;

  TrayIcon->Visible = true;
  TrayIcon->Animate = true;
  TrayIcon->ShowBalloonHint();
}

  *)
end;

procedure TWinVCLPresenter.DoTrayIconClick(Sender: TObject);
begin
  DoToggleUI(True);
end;

procedure TWinVCLPresenter.EnumerateControls(const ALayout: TObject; const AControls: TList<TObject>);
var
  vParentControl: TWinControl;
  i: Integer;
begin
  if not (ALayout is TWinControl) then
    Exit;
  if TWinControl(ALayout).ControlCount <= 0 then
    Exit;

  vParentControl := TWinControl(ALayout);
  {for i := 0 to vParentControl.ControlCount - 1 do
    if (vParentControl.Controls[i] is TWinControl) then
      AControls.Add(vParentControl.Controls[i]);

  AControls.Sort(TComparer<TObject>.Construct(function(const Left, Right: TObject): Integer
    begin
      Result := TWinControl(Left).TabOrder - TWinControl(Right).TabOrder;
    end));

  for i := vParentControl.ControlCount - 1 downto 0 do
    if not (vParentControl.Controls[i] is TWinControl) then
      AControls.Insert(0, vParentControl.Controls[i]);

  for i := 0 to vParentControl.ComponentCount - 1 do
    if vParentControl.Components[i] is TMenu then
      AControls.Insert(0, vParentControl.Components[i]);}

  for i := 0 to vParentControl.ComponentCount - 1 do
    if vParentControl.Components[i] is TMenu then
      AControls.Add(vParentControl.Components[i]);

  for i := 0 to vParentControl.ControlCount - 1 do
    AControls.Add(vParentControl.Controls[i]);
end;

function TWinVCLPresenter.GetPresenterName: string;
begin
  Result := 'WinVCL';
end;

function TWinVCLPresenter.GetLayoutCaption(const ALayout: TObject): string;
begin
  if ALayout is TPageControl then
    Result := TPageControl(ALayout).Hint
  else
    Result := TPanel(ALayout).Caption;
end;

function TWinVCLPresenter.GetLayoutKind(const ALayout: TObject): TLayoutKind;
begin
  if ALayout is TPanel then
    Result := lkPanel
  else if ALayout is TTabSheet then
    Result := lkPage
  else if ALayout is TPageControl then
    Result := lkPages
  else if ALayout is TMemo then
    Result := lkMemo
  else
    Result := lkFrame;
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

function TWinVCLPresenter.InternalMessageBox(const AText, ACaption: string; Flags: Longint; ALangID: Word): Integer;
var
  ActiveWindow, TaskActiveWindow: HWnd;
  MBMonitor, AppMonitor: HMonitor;
  MonInfo: TMonitorInfo;
  Rect: TRect;
  FocusState: TFocusState;
  WindowList: TTaskWindowList;
begin
  ActiveWindow := Application.ActiveFormHandle;
  if ActiveWindow = 0 then
    TaskActiveWindow := Application.Handle
  else
    TaskActiveWindow := ActiveWindow;
  MBMonitor := MonitorFromWindow(ActiveWindow, MONITOR_DEFAULTTONEAREST);
  AppMonitor := MonitorFromWindow(Application.Handle, MONITOR_DEFAULTTONEAREST);
  if MBMonitor <> AppMonitor then
  begin
    MonInfo.cbSize := Sizeof(TMonitorInfo);
    GetMonitorInfo(MBMonitor, @MonInfo);
    GetWindowRect(Application.Handle, Rect);
    SetWindowPos(Application.Handle, 0,
      MonInfo.rcMonitor.Left + ((MonInfo.rcMonitor.Right - MonInfo.rcMonitor.Left) div 2),
      MonInfo.rcMonitor.Top + ((MonInfo.rcMonitor.Bottom - MonInfo.rcMonitor.Top) div 2),
      0, 0, SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOSIZE or SWP_NOZORDER);
  end;
  WindowList := DisableTaskWindows(ActiveWindow);
  FocusState := SaveFocusState;
  if Application.UseRightToLeftReading then Flags := Flags or MB_RTLREADING;
  try
    Result := MessageBoxEx(TaskActiveWindow, PChar(AText), PChar(ACaption), Flags, ALangID);
  finally
    if MBMonitor <> AppMonitor then
      SetWindowPos(Application.Handle, 0,
        Rect.Left + ((Rect.Right - Rect.Left) div 2),
        Rect.Top + ((Rect.Bottom - Rect.Top) div 2),
        0, 0, SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOSIZE or SWP_NOZORDER);
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
    RestoreFocusState(FocusState);
  end;
end;

procedure TWinVCLPresenter.LongOperationEnded;
begin
  Dec(FLongOperationCount);
  if FLongOperationCount = 0 then
    Screen.Cursor := FPrevCursor;
end;

procedure TWinVCLPresenter.LongOperationStarted;
begin
  if FLongOperationCount = 0 then
  begin
    FPrevCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
  end;
  Inc(FLongOperationCount);
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

procedure TWinVCLPresenter.SetLayoutCaption(const ALayout: TObject; const ACaption: string);
begin
  TPanel(ALayout).Caption := ACaption;
end;

initialization

TBaseModule.RegisterModule('UI', 'Windows.DevExpress', TWinVCLPresenter);

end.
