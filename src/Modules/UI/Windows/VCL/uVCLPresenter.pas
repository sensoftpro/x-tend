{ ---------------------------------------------------------------------------------
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
  --------------------------------------------------------------------------------- }

unit uVCLPresenter;

interface

uses
  Messages, Classes, Generics.Collections, Generics.Defaults, ActnList, uConsts, StdCtrls, Buttons,
  ExtCtrls, TypInfo, Types, ComCtrls, SysUtils, Windows, Graphics, imaging.PngImage,
  Variants, Controls, Forms, Mask, Menus,

  uDefinition, uPresenter, uInteractor, uView, uSettings,

  StartVclForm, DebugInfoForm, SplashVclForm, uUIBuilder;

type
  TImageResolution = (ir16x16, ir24x24, ir32x32);

type
  TVCLPresenter = class(TPresenter)
  private
    procedure LoadImages(const AInteractor: TInteractor; const AImageList: TDragImageList; const AResolution: Integer);
    procedure DoToggleUI(const AVisible: Boolean); // Rethink
    procedure DoTrayIconClick(Sender: TObject);
  private
    FRowStyle: TObject;
    FNeedShowSplash: Boolean;
    procedure ArrangeMozaic(const AMDIForm: TForm);
  protected
    // FOnRFIDRead: TRFIDReadEvent;
    FTrayIcon: TTrayIcon;
    FStartForm: TStartVclFm;
    [Weak]
    FDebugForm: TDebugFm;
    [Weak]
    FSplashForm: TSplashVclFm;
    function ShowLoginForm(const AAppTitle: string; var ALoginName, APass { , ARFID } : string): Boolean;
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

    procedure DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType); override;
    function DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet)
      : TDialogResult; override;
    procedure DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False); override;
    function DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt, ADefaultDir: string)
      : Boolean; override;
    function DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean; override;
    procedure DoSetCursor(const ACursorType: TCursorType); override;
    procedure DoCloseAllPages(const AInteractor: TInteractor); override;

    procedure OnDomainLoadProgress(const AProgress: Integer; const AInfo: string); override;
    procedure OnDomainError(const ACaption, AText: string); override;

    function DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject; override;
    procedure StoreUILayout(const AInteractor: TInteractor); override;
    procedure RestoreUILayout(const AInteractor: TInteractor); override;
  public
    constructor Create(const AName: string; const ASettings: TSettings); override;
    destructor Destroy; override;

    procedure SetAsMainForm(const AForm: TForm);

    function CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView;
      const AAreaName: string; const ACallback: TNotifyEvent = nil; const ACaption: string = '';
      const AOnClose: TProc = nil): TUIArea; override;
    function ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string;
      var AArea: TUIArea): TDialogResult; override;
    procedure CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea); override;

    function ShowPage(const AInteractor: TInteractor; const APageType: string; const AParams: TObject = nil)
      : TDialogResult; override;
    procedure ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement); override;

    function CreateLayoutArea(const ALayoutKind: TLayoutKind; const AParams: string = ''): TObject; override;
    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); override;

    function GetWidthByType(const AWidth: Integer; const AFieldDef: TFieldDef): Integer;

    property RowStyle: TObject read FRowStyle;
  end;

implementation

uses
  Dialogs, Math, MultiMon, StrUtils, ShellAPI, ActiveX,

  uPlatform, uModule, uDomain, uUtils, uConfiguration, uChangeManager, uIcon, uEntity, uEntityList, vclnArea, uSession,
  uManagedVCLForm, AboutVCLForm, ReportConfigureVCLForm, LoginVCLForm;

type
  TLoginedProc = procedure(const AInteractor: TInteractor) of object;
  TBeforeUIClosingFunc = function(const AInteractor: TInteractor): Boolean of object;

  { TWinVCLPresenter }

procedure TVCLPresenter.ArrangeMozaic(const AMDIForm: TForm);
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

procedure TVCLPresenter.ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement);
var
  vForm: TForm;
begin
  if AInteractor.Layout <> 'mdi' then
    Exit;

  vForm := TForm(TVCLArea(AInteractor.UIBuilder.RootArea).Control);
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
  TUIAreaCrack = class(TUIArea)
  end;

procedure TVCLPresenter.CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea);
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

constructor TVCLPresenter.Create(const AName: string; const ASettings: TSettings);
begin
  inherited Create(AName, ASettings);
  if ASettings.KeyExists(AName, 'ShowSplash') then
    FNeedShowSplash := StrToBoolDef(ASettings.GetValue(AName, 'ShowSplash'), False)
  else
    FNeedShowSplash := StrToBoolDef(ASettings.GetValue('Core', 'ShowSplash'), False);
end;

function TVCLPresenter.CreateLayoutArea(const ALayoutKind: TLayoutKind; const AParams: string = ''): TObject;
var
  vParams: TStrings;
begin
  vParams := CreateDelimitedList(AParams);
  case ALayoutKind of
    lkPanel:
      begin
        Result := TPanel.Create(nil);
        TPanel(Result).BevelOuter := bvNone;
      end;
    lkPage:
      begin
        Result := TTabSheet.Create(nil);
        TTabSheet(Result).Caption := vParams.Values['Caption'];
        TTabSheet(Result).ImageIndex := StrToIntDef(vParams.Values['ImageIndex'], -1);
        TTabSheet(Result).Name := vParams.Values['Name'];
        TTabSheet(Result).Tag := 11;
      end;
    lkFrame:
      Result := TFrame.Create(nil);
  else
    Result := nil;
  end;
  FreeAndNil(vParams);
end;

function TVCLPresenter.CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView;
  const AAreaName: string; const ACallback: TNotifyEvent = nil; const ACaption: string = '';
  const AOnClose: TProc = nil): TUIArea;
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

    if (AInteractor.Layout = 'mdi') then
      vForm.FormStyle := fsMDIForm
    else
      vForm.FormStyle := fsNormal;

    vForm.OnClose := DoMainFormClose;
    vForm.Position := poScreenCenter;
    vForm.ShowHint := True;
    vForm.Caption := TDomain(AInteractor.Domain).AppTitle + ' (' + TUserSession(AInteractor.Session)
      .CurrentUserName + ')';

    vForm.DisableAlign;
    try
      Result := TVCLArea.Create(AParent, AView, '', True, vForm, nil, '');
    finally
      vForm.EnableAlign;
    end;
  end
  // второстепенная автономная форма
  else if AAreaName = 'float' then
  begin
    for i := 0 to AParent.Count - 1 do
    begin
      vArea := AParent.Areas[i];
      if (vArea.View = AView) and (TVCLArea(vArea).Control is TForm) then
        Exit(vArea);
    end;

    vForm := TForm.Create(nil);
    vForm.OnClose := DoFloatFormClose;
    vForm.Position := poMainFormCenter;
    vForm.Font.Size := 12;
    vForm.Caption := ACaption;
  end
  // дочерняя модальная форма
  else if (AAreaName = 'child') or (AAreaName = 'modal') then
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
    else
    begin
      vForm.OnClose := DoChildFormClose;
      vForm.OnKeyDown := DoChildFormKeyDown;
      vForm.KeyPreview := True;
      vForm.BorderIcons := [biSystemMenu];
    end;
    vForm.Position := poMainFormCenter;
    vForm.Font.Size := 12;
    vForm.BorderStyle := bsSingle;
    // for layouted form this property will be changed when assigned cEditFormResizable flag in Tag
  end;

  if vForm = nil then Exit;

  vForm.ShowHint := True;
  vForm.DisableAlign;
  Assert(not Assigned(vForm.OnShow), 'vForm.OnShow already assigned');
  vForm.OnShow := DoOnFormShow;
  try
    Result := TVCLArea.Create(AParent, AView, '', True, vForm, nil, '');
    if Assigned(AOnClose) then
      TVCLArea(Result).OnClose := AOnClose;

    if Assigned(ACallback) and Assigned(vTimer) then
      ACallback(vTimer);
  finally
    vForm.EnableAlign;
  end;
end;

destructor TVCLPresenter.Destroy;
begin
  FreeAndNil(FRowStyle);
  FreeAndNil(FSplashForm);

  inherited Destroy;
end;

function TVCLPresenter.DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject;
begin
  Result := TImageList.Create(nil);
  TImageList(Result).SetSize(ASize, ASize);
  LoadImages(AInteractor, TImageList(Result), ASize);
end;

procedure TVCLPresenter.DoDebugFormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FDebugForm := nil;
end;

procedure TVCLPresenter.DoFloatFormClose(Sender: TObject; var Action: TCloseAction);
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
      if Assigned(TVCLArea(vArea).OnClose) then
        TVCLArea(vArea).OnClose();

      // Возможно, нужно сбросить CurrentArea у UIBuilder-а в vArea.Parent

      vArea.SetHolder(nil);
      if Assigned(vArea.Parent) then
        vArea.Parent.RemoveArea(vArea);

      vInteractor.PrintHierarchy;
      Action := caFree;
    end
    else
      Action := caNone;
  finally
    vCloseView.RemoveListener(vArea);
  end;
end;

procedure TVCLPresenter.LoadImages(const AInteractor: TInteractor;
  const AImageList: TDragImageList; const AResolution: Integer);
var
  vConfiguration: TConfiguration;
  vImage: TPNGImage;
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
      else
      begin
        vStream.Position := 0;
        vImage.LoadFromStream(vStream);
        vBitmap := TBitmap.Create;
        vBitmap.Width := vImage.Width;
        vBitmap.Height := vImage.Height;
        vBitmap.PixelFormat := pf32bit;
        vImage.Draw(vBitMap.Canvas, Rect(0, 0,vImage.Width, vImage.Height));
        try
          if AImageList is TImageList then
            TImageList(AImageList).Add(vBitmap, nil)
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
  vImage := TPNGImage.Create;
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

function TVCLPresenter.MessageTypeToMBFlags(const AMessageType: TMessageType): Integer;
begin
  Result := 0;
  case AMessageType of
    msInfo:
      Result := MB_ICONINFORMATION;
    msQuestion:
      Result := MB_ICONQUESTION;
    msWarning:
      Result := MB_ICONWARNING;
    msError:
      Result := MB_ICONERROR;
  end;
end;

procedure TVCLPresenter.OnDomainError(const ACaption, AText: string);
begin
  ShowMessage(ACaption, AText, msError);
end;

procedure TVCLPresenter.OnDomainLoadProgress(const AProgress: Integer; const AInfo: string);
begin
  if FNeedShowSplash then
  begin
    FProgressInfo.SetProgress(AProgress, AInfo);
    ShowPage(nil, 'splash', FProgressInfo);
  end;
end;

procedure TVCLPresenter.OnShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  vActiveInteractor: TInteractor;
  vView: TView;
begin
  vActiveInteractor := ActiveInteractor;
  if Assigned(vActiveInteractor) and (GetKeyState(VK_CONTROL) < 0) and (GetKeyState(VK_MENU) < 0) and
    (GetKeyState(VK_SHIFT) < 0) then
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
      if not(vView.DefinitionKind in [dkAction, dkListField, dkObjectField, dkSimpleField]) then
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

function TVCLPresenter.DoLogin(const ADomain: TObject): TInteractor;
var
  vDomain: TDomain absolute ADomain;
  vLogin, vPassword: string;
  // vRFID: string;
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
      else
      begin
        vLogin := vDomain.UserSettings.GetValue('Core', 'LastLogin', '');
        vPassword := '';
        // vRFID := '';
        vSession := nil;
        repeat
          // vDomain.SetRFIDHandler(FOnRFIDRead);
          // try
          vResult := ShowLoginForm(vDomain.AppTitle, vLogin, vPassword { , vRFID } );
          // finally
          // vDomain.SetRFIDHandler(nil);
          // end;

          if not vResult then
            Break;

          // if Trim(vRFID) <> '' then
          // begin
          // vSession := vDomain.LoginByRFID(vRFID);
          // if not Assigned(vSession) and (ShowYesNoDialog(_Platform.Translate('cptError', 'Ошибка'),
          // _Platform.Translate('txtWrongLoginRFID', 'Считанная RFID-карта не принадлежит ни одному сотруднику')
          // + #13#10 + _Platform.Translate('txtPromptTryAgain', 'Попробовать ещё раз?')) <> drYes) then Break;
          // end
          // else begin
          vSession := vDomain.Login(vLogin, vPassword);
          if not Assigned(vSession) and (ShowYesNoDialog(_Platform.Translate('cptError', 'Ошибка'),
            _Platform.Translate('txtWrongLoginOrPassword', 'Введены некорректные имя пользователя или пароль') + #13#10
            + _Platform.Translate('txtPromptTryAgain', 'Попробовать ещё раз?')) <> drYes) then
            Break;
          // end;
        until Assigned(vSession);

        if not Assigned(vSession) then
        begin
          Application.Title := cPlatformTitle;
          // Application.Icon //todo: вернуть иконку по умолчанию
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
  Result.UIBuilder.Navigate(nil, '', vMainFormName, '' { , Result.UIHolder } );

  TLoginedProc(vDomain.Configuration.LoginedProc)(Result);

  if Assigned(FDebugForm) then
    FDebugForm.AddInteractor(Result);
end;

procedure TVCLPresenter.DoLogout(const AInteractor: TInteractor);
begin
  Application.Title := cPlatformTitle;
  if Assigned(FDebugForm) then
    FDebugForm.RemoveInteractor(AInteractor);
end;

function TVCLPresenter.ShowLoginForm(const AAppTitle: string; var ALoginName, APass { , ARFID } : string): Boolean;
var
  vLoginForm: TLoginVCLFm;
begin
  vLoginForm := TLoginVCLFm.Create(nil);
  try
    vLoginForm.Init(AAppTitle);
    vLoginForm.LoginName := ALoginName;
    vLoginForm.Pass := APass;
    // vLoginForm.RFID := '';

    // FOnRFIDRead := vLoginForm.OnRFIDReceived;
    // try
    Result := vLoginForm.ShowModal = mrOk;
    // finally
    // FOnRFIDRead := nil;
    // end;

    if Result then
    begin
      ALoginName := vLoginForm.LoginName;
      APass := vLoginForm.Pass;
      // ARFID := vLoginForm.RFID;
    end;
  finally
    FreeAndNil(vLoginForm);
  end;
end;

function TVCLPresenter.ShowPage(const AInteractor: TInteractor; const APageType: string; const AParams: TObject = nil)
  : TDialogResult;
var
  vForm: TReportConfigureVCLFm;
  vProgressInfo: TProgressInfo;
  vPageClass: TManagedFormClass;
begin
  Result := drNone;

  if APageType = 'about' then
  begin
    TAboutVCLFm.ShowAbout(AInteractor);
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
      FDebugForm.UpdateHoldersInfo;
    end;
  end
  else if APageType = 'rtf_reports' then
  begin
    vForm := TReportConfigureVCLFm.Create(nil);
    vForm.Init(AInteractor);
    try
      vForm.ShowModal;
    finally
      vForm.Free;
    end;
  end
  else if APageType = 'splash' then
  begin
    vProgressInfo := TProgressInfo(AParams);
    if not Assigned(vProgressInfo) then
      Exit;

    if not Assigned(FSplashForm) then
      FSplashForm := TSplashVclFm.ShowSplash(Self, TDomain(vProgressInfo.Domain))
    else if vProgressInfo.Progress = 100 then
      FreeAndNil(FSplashForm)
    else
      FSplashForm.UpdateProgress(vProgressInfo.Progress, vProgressInfo.Info);
  end
  else
  begin
    vPageClass := TManagedFormClass(GetPageClass(FName, APageType));
    if Assigned(vPageClass) then
      TManagedForm.ShowPage(vPageClass, AInteractor);
  end;
end;

function TVCLPresenter.ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string;
  var AArea: TUIArea): TDialogResult;
var
  vArea: TVCLArea absolute AArea;
  vView: TView;
  vForm: TForm;
  vCaption: string;

  function ModalResultToDialogResult(const AModalResult: TModalResult): TDialogResult;
  begin
    case AModalResult of
      mrOk:
        Result := drOk;
      mrCancel:
        Result := drCancel;
      mrYes:
        Result := drYes;
      mrNo:
        Result := drNo;
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
  else if (AAreaName = 'child') or (AAreaName = 'modal') then
  begin
    vForm := TForm(vArea.Control);
    vForm.ShowHint := True;
    vView := vArea.View;

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
      vArea.SetHolder(nil);
      if Assigned(vArea.Parent) then
        vArea.Parent.RemoveArea(AArea);
      vForm.Free;
    end;
  end;
end;

procedure TVCLPresenter.StoreUILayout(const AInteractor: TInteractor);
var
//  i: Integer;
  vMainForm: TForm;
  vLayoutStr: string;

  procedure SaveForm(const AForm: TForm; const AName: string);
  begin
    vLayoutStr := IntToStr(AForm.Left) + ';' + IntToStr(AForm.Top) + ';' + IntToStr(AForm.Width) + ';' + IntToStr(AForm.Height) + ';' + IntToStr(Ord(AForm.WindowState));
    TDomain(AInteractor.Domain).UserSettings.SetValue(AName, 'Layout', vLayoutStr);
  end;
begin
  vMainForm := TForm(TVCLArea(AInteractor.UIBuilder.RootArea).Control);
  if not Assigned(vMainForm) or (vMainForm.Position <> poDesigned) then Exit;

  SaveForm(vMainForm, 'MainForm');

{  if AInteractor.Layout = 'mdi' then
  begin
    if vMainForm.FormStyle = fsMDIForm then
      for i := vMainForm.MDIChildCount - 1 downto 0 do
      begin
        vForm := vMainForm.MDIChildren[i];
        SaveForm(vForm, 'mdi_' + vForm.Name);
      end;
  end;   }
end;

procedure TVCLPresenter.RestoreUILayout(const AInteractor: TInteractor);
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
  vMainForm := TForm(TVCLArea(AInteractor.UIBuilder.RootArea).Control);
  if (not Assigned(vMainForm)) or (vMainForm.Position <> poDesigned) then Exit;

  LoadForm(vMainForm, 'MainForm');
end;

procedure TVCLPresenter.DoChildFormClose(Sender: TObject; var Action: TCloseAction);
var
  vForm: TForm;
  vArea: TUIArea;
  vInteractor: TInteractor;
  vView, vCloseView: TView;
  vHolder: TChangeHolder;
  vRes: TDialogResult;
  vChildArea: TVCLArea;
  i: Integer;
  vCanBeClosed: Boolean;

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
        vInteractor.ShowMessage(vInteractor.Translate('msgRecordExists', 'Такая запись уже существует') + ' [' +
          vSimilar['Name'] + ']');
        Action := caNone;
      end;
    end;
  end;

begin
  vForm := TForm(Sender);
  vArea := TUIArea(vForm.Tag);
  vHolder := TChangeHolder(vArea.ThisHolder);//(vArea.Holder);
  vView := vArea.View;
  vInteractor := TInteractor(vArea.Interactor);

  for i := 0 to vArea.Count - 1 do
  begin
    vChildArea := TVCLArea(vArea[i]);
    if vChildArea.Control is TForm then
    begin
      vInteractor.ShowMessage('Невозможно закрыть окно, так как есть другие программные окна, зависящие от него', msWarning);
      Action := caNone;
      Exit;
    end;
  end;

  vCloseView := vView.BuildView('Close');
  vCloseView.AddListener(vArea);
  try
    vCanBeClosed := not (Assigned(vCloseView) and (TCheckActionFlagsFunc(TConfiguration(vInteractor.Configuration).
      CheckActionFlagsFunc)(vCloseView) <> vsFullAccess));

    if vCanBeClosed then
    begin
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
end;

procedure TVCLPresenter.DoChildFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    // if vForm.ControlCount < 3 then
    // FForm.ModalResult := mrOk else
    if (not(ssShift in Shift)) and (not(vForm.ActiveControl is TMemo)) and (not(vForm.ActiveControl is TMemo)) then
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

procedure TVCLPresenter.DoCloseAllPages(const AInteractor: TInteractor);
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

    if Await then
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

procedure TVCLPresenter.DoOnFormShow(Sender: TObject);
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

procedure TVCLPresenter.DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False);
var
  vExecResult: Cardinal;
begin
  vExecResult := ShellExecuteU(Application.Handle, AFileName, '', '', Await);
  if (vExecResult = SE_ERR_NOASSOC) and (ADefaultApp <> '') then
    ShellExecuteU(Application.Handle, ADefaultApp, AFileName, '', Await);
end;

procedure TVCLPresenter.DoMainFormClose(Sender: TObject; var Action: TCloseAction);
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

procedure TVCLPresenter.DoRun(const AParameter: string);
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
    Application.CreateForm(TStartVclFm, FStartForm);
    FStartForm.Init(Self);
  end;

  DoOnAppStarted;

  OnDomainLoadProgress(100, '');

  Application.HintHidePause := -1;
  Application.Run;
end;

procedure TVCLPresenter.DoSetCursor(const ACursorType: TCursorType);
begin
  Screen.Cursor := cCursors[ACursorType];
end;

function TVCLPresenter.DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult;
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
  else
  begin
    Result := drCancel;
    Exit;
  end;

  vRes := Application.MessageBox(PChar(AText), PChar(ACaption), vFlags); //MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL));
  // MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US)); //MAKELANGID(LANG_FRENCH, SUBLANG_FRENCH));
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

procedure TVCLPresenter.DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType);
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

function TVCLPresenter.DoShowOpenDialog(var AFileName: string;
  const ATitle, AFilter, ADefaultExt, ADefaultDir: string): Boolean;
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

function TVCLPresenter.DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean;
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

procedure TVCLPresenter.DoStop;
begin
  FreeAndNil(FDebugForm);

  if Assigned(FStartForm) then
    FStartForm.Deinit;

  Application.Terminate;
end;

procedure TVCLPresenter.DoToggleUI(const AVisible: Boolean);
begin
  if AVisible then
  begin
    if Assigned(FTrayIcon) then
    begin
      FTrayIcon.Visible := False;
      Application.MainForm.Show;
    end;
  end
  else
  begin
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

procedure TVCLPresenter.DoTrayIconClick(Sender: TObject);
begin
  DoToggleUI(True);
end;

procedure TVCLPresenter.DoUnfreeze;
begin
  Application.ProcessMessages;
end;

function TVCLPresenter.GetWidthByType(const AWidth: Integer; const AFieldDef: TFieldDef): Integer;
begin
  case AFieldDef.Kind of
    fkInteger:
      Result := 70;
    fkFloat:
      Result := 70;
    fkDateTime:
      Result := 80;
    fkCurrency:
      Result := 80;
  else
    Result := AWidth - 4;
  end;
end;

procedure TVCLPresenter.SetApplicationUI(const AAppTitle, AIconName: string);
begin
  Application.Title := AAppTitle;
  if FileExists(AIconName) then
    Application.Icon.LoadFromFile(AIconName);
end;

procedure TVCLPresenter.SetAsMainForm(const AForm: TForm);
var
  p: Pointer;
begin
  p := @Application.MainForm;
  Pointer(p^) := AForm;
end;

initialization

TBaseModule.RegisterModule('UI', 'Windows.VCL', TVCLPresenter);

end.
