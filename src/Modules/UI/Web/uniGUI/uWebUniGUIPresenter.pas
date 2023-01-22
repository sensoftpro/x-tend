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

unit uWebUniGUIPresenter;

interface

uses
  Messages, Classes, Generics.Collections, Generics.Defaults, ActnList, uConsts, StdCtrls, Buttons,
  ExtCtrls, TypInfo, Types, ComCtrls, SysUtils, Windows, Graphics,
  Variants, Controls, Forms, Mask, Menus,

  uniGUIServer, uniGUIMainModule, uniGUIApplication, uIdCustomHTTPServer, uniGUIClasses, uniGUITypes,
  uniGUIForm, uniEdit, uniGUIBaseClasses, uniLabel, uniButton, uniImageList, uniPanel, uniPageControl,

  uSettings, uDefinition, uPresenter, uInteractor, uView, uUIBuilder, uLayout, uEntity;

type
  TImageResolution = (ir16x16, ir24x24, ir32x32);

type
  // Служебные объекты uniGUI

  TUniServerModule = class(TUniGUIServerModule)
  protected
    procedure FirstInit; override;
  end;

  TUniMainModule = class(TUniGUIMainModule)
    procedure UniGUIMainModuleCreate(Sender: TObject);
  private
    FInteractor: TInteractor;
  public
    property Interactor: TInteractor read FInteractor;
  end;

  TUniMainForm = class(TUniForm)
    UniLabel1: TUniLabel;
    UniEdit1: TUniEdit;
    UniButton1: TUniButton;
    procedure UniButton1Click(Sender: TObject);
    procedure UniFormCreate(Sender: TObject);
  private
    FInteractor: TInteractor;
  public
  end;

type
  TWebUniGUIPresenter = class(TPresenter)
  private
    FServerModule: TUniServerModule;
    procedure LoadImages(const AInteractor: TInteractor; const AImageList: TUniNativeImageList; const AResolution: Integer);
    procedure UniServerModuleBeforeInit(Sender: TObject);
    function FormInstance: TUniMainForm;
  protected
    {check}procedure DoRun(const AParameter: string); override;
    procedure DoUnfreeze; override;
    procedure DoStop; override;

    {}function DoLogin(const ADomain: TObject): TInteractor; override;
    {}procedure DoLogout(const AInteractor: TInteractor); override;

    procedure DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType); override;
    function DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult; override;
    // Add DoPromt(const ACaption, AText: string): string;
    procedure DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False); override;
    function DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt, ADefaultDir: string): Boolean; override;
    function DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean; override;
    procedure DoSetCursor(const ACursorType: TCursorType); override;
    {}procedure DoCloseAllPages(const AInteractor: TInteractor); override;

    function DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject; override;
  public
    constructor Create(const AName: string; const ASettings: TSettings); override;
    destructor Destroy; override;

    {}function CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView; const AAreaName: string;
      const ACallback: TNotifyEvent = nil; const ACaption: string = ''; const AOnClose: TProc = nil): TUIArea; override;
    {}function ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult; override;
    {}procedure CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea); override;

    {+}function ShowPage(const AInteractor: TInteractor; const APageType: string; const AParams: TObject = nil): TDialogResult; override;
    {+}procedure ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement); override;

    procedure DoEnumerateControls(const ALayout: TObject; const AControls: TList<TObject>); override;
    function CreateLayoutArea(const ALayoutKind: TLayoutKind; const AParams: string = ''): TObject; override;
    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); override;
    procedure DoSetLayoutCaption(const ALayout: TObject; const ACaption: string); override;
    function DoGetLayoutCaption(const ALayout: TObject): string; override;
    function DoGetLayoutKind(const ALayout: TObject): TLayoutKind; override;
  end;

  TUniGUIPresenter = class(TPresenter)
  private
    FServerModule: TUniServerModule;
    procedure UniServerModuleBeforeInit(Sender: TObject);
    function FormInstance: TUniMainForm;

    {}procedure ArrangeMozaic(const AParentForm: TUniForm);
  protected
    procedure OnShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure OnChildFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure DoRun(const AParameter: string); override;
    {}procedure DoUnfreeze; override;
    {}procedure DoStop; override;

      function DoLogin(const ADomain: TObject): TInteractor; override;
    procedure DoLogout(const AInteractor: TInteractor); override;

    function GetNativeControlClass: TNativeControlClass; override;
    procedure DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType); override;
    function DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult; override;
    procedure DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False); override;
    function DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt, ADefaultDir: string): Boolean; override;
    function DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean; override;
    {}procedure DoSetCursor(const ACursorType: TCursorType); override;
    procedure DoCloseAllPages(const AInteractor: TInteractor); override;
      function CreateAreaContent(const AArea: TUIArea; const AView: TView;
        const ALayout: TLayout; const AParams: string = ''): TNativeControl; override;
      function CreateAreaContentItem(const AOwner, AArea: TUIArea; const ANavItem: TNavigationItem;
        const ACaption, AHint: string; const AImageIndex: Integer): TNativeControl; override;

      function GetImagePlaceholder(const ASize: Integer): TStream; override;
      function DoCreateImages(const ADomain: TObject; const AImages: TImages; const ASize: Integer): TObject; override;

      procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); override;
  protected
    function CreateControl(const AParent: TUIArea; const AView: TView; const ALayout: TLayout;
      const AParams: string = ''): TObject; virtual;
  public
      function ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult; override;
    {}procedure ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement); override;
  end;

  TUniArea = class(TUIArea)
  protected
    FControl: TUniControl;
    procedure DoClose(const AModalResult: Integer); override;
    function DoCreateChildArea(const ALayout: TObject; const AView: TView; const AParams: string = '';
      const AOnClose: TProc = nil): TUIArea; override;
    function DoCreateChildAction(const ALayout: TObject; const AView: TView; const AParams: string = ''): TUIArea; override;
    function AreaFromSender(const ASender: TObject): TUIArea; override;
    procedure AppendServiceArea(const ALayoutName: string); override;
    procedure PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetViewState(const AValue: TViewState); override;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TObject = nil; const AParams: string = ''); override;
    destructor Destroy; override;
  end;

function GetNativeControl(const AArea: TUIArea): TObject;

implementation

uses
  Math, PngImage, ImgList, UITypes, UniGUIVars,
  uPlatform, uIcon, uModule, uConfiguration, uDomain, uEntityList, uSession, uUtils;

{$R ServerModule.dfm}
{$R MainModule.dfm}
{$R Main.dfm} // Это необязательный ресурс

type
  TUniGUIControl = class(TNativeControl)
    Control: TObject;
  end;

function GetNativeControl(const AArea: TUIArea): TObject;
var
  vUniGUIControl: TUniGUIControl;
begin
  if not Assigned(AArea) then
    Exit(nil);

  vUniGUIControl := TUniGUIControl(AArea.NativeControl);
  if not Assigned(vUniGUIControl) then
    Result := nil
  else
    Result := vUniGUIControl.Control;
end;

{ TUniServerModule }

procedure TUniServerModule.FirstInit;
begin
  InitServerModule(Self);
  OnBeforeInit := TWebUniGUIPresenter(_Platform.Presenter).UniServerModuleBeforeInit;
end;

{ TUniMainModule }

procedure TUniMainModule.UniGUIMainModuleCreate(Sender: TObject);
var
  vDomain: TDomain;
  vSession: TUserSession;
  vUsers: TEntityList;
begin
  vDomain := _Platform.Domains[0];

  vUsers := TEntityList.Create(vDomain, vDomain.DomainSession);
  vDomain.GetEntityList(vDomain.DomainSession, vDomain.Configuration['SysUsers'], vUsers, '');
  try
    if vUsers.Count > 0 then
      vSession := vDomain.Sessions.AddSession(vUsers[0])
    else
      vSession := vDomain.DomainSession;
  finally
    FreeAndNil(vUsers);
  end;

  FInteractor := TInteractor.Create(_Platform.Presenter, vSession);
  //FInteractor.UIBuilder.SetRootArea(TWebArea.Create(nil, nil, '', Self));
end;

{ TUniMainForm }

procedure TUniMainForm.UniButton1Click(Sender: TObject);
begin
  with TUniEdit.Create(Self) do
  begin
    Parent := Self;
    Left := 20;
    Top := 23;
    Width := 200;
    Text := 'Кошка';
  end;
end;

procedure TUniMainForm.UniFormCreate(Sender: TObject);
var
  vPresenter: TWebUniGUIPresenter;
  vDomain: TDomain;
  vSession: TUserSession;
  vMainFormName: string;
  vView: TView;
  vUIArea: TUIArea;
  vParams: TStrings;
  vViewName: string;
  vLayoutName: string;

  function ExtractValueFromStrings(const AList: TStrings; const AKey: string; const ADefault: string = ''): string;
  begin
    if AList.IndexOfName(AKey) >= 0 then
      Result := AList.Values[AKey]
    else
      Result := ADefault;
  end;
begin
  FInteractor := TUniMainModule(UniApplication.UniMainModule).Interactor;
  vPresenter := TWebUniGUIPresenter(FInteractor.Presenter);
  vSession := TUserSession(FInteractor.Session);
  vDomain := TDomain(FInteractor.Domain);
  vView := FInteractor.RootView;

  vMainFormName := vDomain.Settings.GetValue('Core', 'MainForm', '');
  if Trim(vMainFormName) = '' then
    vMainFormName := 'MainForm';

  //if FInteractor.Layout = 'mdi' then
  //  PageMode := True;

  //OnClose := vPresenter.DoMainFormClose;
  //OnShow := vPresenter.DoMainFormShow;
  Position := poScreenCenter;
  Caption := vDomain.AppTitle + ' (' + vSession.CurrentUserName + ')';
  ShowHint := True;

  {vUIArea := TUniArea.Create(nil, vView, '', True, Self, nil, '');
  FInteractor.UIBuilder.SetRootArea(vUIArea);
  FInteractor.UIBuilder.ApplyLayout(vUIArea, vView, vMainFormName, ''); // >> MakeLayoutFromFile

  if FInteractor.UIBuilder.DefaultParams <> '' then
  begin
    vParams := CreateDelimitedList(FInteractor.UIBuilder.DefaultParams, '&');
    try
      vViewName := ExtractValueFromStrings(vParams, 'View');
      vLayoutName := ExtractValueFromStrings(vParams, 'Layout');
      if (vViewName <> '') or (vLayoutName <> '') then
        FInteractor.UIBuilder.Navigate(vView.BuildView(vViewName), 'WorkArea',
          vLayoutName, '', nil, nil, ExtractValueFromStrings(vParams, 'Caption'));
    finally
      FreeAndNil(vParams);
      FInteractor.UIBuilder.DefaultParams := '';
    end;
  end; }

  {with TUniEdit.Create(Self) do
  begin
    Parent := Self;
    Left := 300;
    Top := 200;
    Width := 400;
    Text := 'Эгегей';
  end;

  ShowToast('Привет');}
end;

{ TWebUniGUIPresenter }

procedure TWebUniGUIPresenter.UniServerModuleBeforeInit(Sender: TObject);
begin
  // Иконку в рантайме пока можно установить только так
  TUniServerModule(Sender).Favicon.LoadFromFile(_Platform.Domains[0].Configuration.IconFileName);
  //TUniServerModule(Sender).MainFormDisplayMode := mfPage;
end;

procedure TWebUniGUIPresenter.ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement);
//var
//  vForm: TForm;
begin
{  if AInteractor.Layout <> 'mdi'  then
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
  end;}
end;

procedure TWebUniGUIPresenter.CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea);
begin

end;

constructor TWebUniGUIPresenter.Create(const AName: string; const ASettings: TSettings);
begin
  inherited Create(AName, ASettings);
end;

function TWebUniGUIPresenter.CreateLayoutArea(const ALayoutKind: TLayoutKind; const AParams: string): TObject;
var
  vParams: TStrings;
begin
  vParams := CreateDelimitedList(AParams);
  case ALayoutKind of
    lkPanel, lkFrame: begin
        Result := TUniPanel.Create(nil);
        TUniPanel(Result).BorderStyle := ubsNone;
      end;
    lkPage: begin
        Result := TUniTabSheet.Create(nil);
        TUniTabSheet(Result).Caption := vParams.Values['Caption'];
        TUniTabSheet(Result).ImageIndex := StrToIntDef(vParams.Values['ImageIndex'], -1);
        TUniTabSheet(Result).Name := vParams.Values['Name'];
        TUniTabSheet(Result).Tag := 11;
      end;
  else
    Result := nil;
  end;
  FreeAndNil(vParams);
end;

function TWebUniGUIPresenter.CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView;
  const AAreaName: string; const ACallback: TNotifyEvent = nil; const ACaption: string = ''; const AOnClose: TProc = nil): TUIArea;
//var
//  vForm: TForm;
begin
{ TODO -owa : Implement it }

  if AAreaName = '' then
  begin
    {Application.CreateForm(TForm, vForm);

    if (AInteractor.Layout = 'mdi') then
      vForm.FormStyle := fsMDIForm
    else
      vForm.FormStyle := fsNormal;

    vForm.Position := poScreenCenter;
    vForm.ShowHint := True;
    vForm.Caption := TDomain(AInteractor.Domain).AppTitle + ' (' + TUserSession(AInteractor.Session).CurrentUserName + ')'; }
    Result := nil;
  end
  else
    Result := nil;
end;

destructor TWebUniGUIPresenter.Destroy;
begin
  inherited Destroy;
end;

function TWebUniGUIPresenter.DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject;
begin
{ TODO -owa : Implement it }
  Result := TUniNativeImageList.Create(nil);
  TUniNativeImageList(Result).Width := ASize;
  TUniNativeImageList(Result).Width := ASize;
  TUniNativeImageList(Result).DefaultOutputFormat := toPng;
  LoadImages(AInteractor, TUniNativeImageList(Result), ASize);
end;

function TWebUniGUIPresenter.DoLogin(const ADomain: TObject): TInteractor;
var
  vDomain: TDomain absolute ADomain;
begin
  SetApplicationUI(vDomain.AppTitle, vDomain.Configuration.IconFileName);
//  Result.UIBuilder.Navigate(nil, '', vMainFormName, ''{, Result.UIHolder});

  Result := nil;
end;

procedure TWebUniGUIPresenter.DoLogout(const AInteractor: TInteractor);
begin
end;

function TWebUniGUIPresenter.ShowPage(const AInteractor: TInteractor; const APageType: string;
  const AParams: TObject = nil): TDialogResult;
//var
//  vForm: TReportConfigureFm;
//  vProgressInfo: TProgressInfo;
//  vPageClass: TManagedFormClass;
begin
  Result := drNone;

  {if APageType = 'about' then
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
  end;}
end;

function TWebUniGUIPresenter.ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult;
begin
{ TODO -owa : Implement it }
  Result := drNone;
end;

procedure TWebUniGUIPresenter.DoCloseAllPages(const AInteractor: TInteractor);
begin

end;

procedure TWebUniGUIPresenter.DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False);
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

procedure TWebUniGUIPresenter.DoRun(const AParameter: string);
begin
  Application.Title := cPlatformTitle;
  Application.Initialize;
  FServerModule := TUniServerModule.Create(Application);

  Login(_Platform.Domains[0]);

  Application.Run;
end;

function TWebUniGUIPresenter.DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult;
var
  vRes: Integer;
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
    Exit(drCancel);

  vRes := FormInstance.MessageDlg(AText, mtConfirmation, vButtons);
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

procedure TWebUniGUIPresenter.DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType);
begin
  TUniMainForm(TUniMainModule(UniApplication.UniMainModule).GetFormInstance(TUniMainForm)).ShowMessage(AText);
  case AMessageType of
    msInfo: FormInstance.MessageDlg(AText, mtInformation, [mbOk]);
    msWarning: FormInstance.MessageDlg(AText, mtWarning, [mbOk]);
    msError: FormInstance.MessageDlg(AText, mtError, [mbOk]);
  else
    FormInstance.ShowMessage(AText);
  end;
end;

function TWebUniGUIPresenter.DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt,
  ADefaultDir: string): Boolean;
begin
//  Upload
  Result := False;

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

function TWebUniGUIPresenter.DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean;
begin
//  Download
  Result := False;

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

procedure TWebUniGUIPresenter.DoStop;
begin
  Application.Terminate;
end;

procedure TWebUniGUIPresenter.DoUnfreeze;
begin
end;

function TWebUniGUIPresenter.FormInstance: TUniMainForm;
begin
  Result := TUniMainForm(TUniMainModule(UniApplication.UniMainModule).GetFormInstance(TUniMainForm));
end;

procedure TWebUniGUIPresenter.LoadImages(const AInteractor: TInteractor; const AImageList: TUniNativeImageList;
  const AResolution: Integer);
var
  vConfiguration: TConfiguration;
  vImage: TPngImage;
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
  begin
    for vIndex in AIcons.IconIndices do
    begin
      AInteractor.StoreImageIndex(vIndex, AImageList.Count);

      vStream := AIcons.IconByIndex(vIndex, AResolution);
      if not Assigned(vStream) then
        AImageList.Add(GetPlaceholder, nil)
      else begin
        vStream.Position := 0;
        AImageList.AddImageStream(TCustomMemoryStream(vStream));
      end;
    end;
  end;

begin
  vImage := TPngImage.Create;
  vPlaceholder := nil;

  AppendIconsToImageList(FCommonIcons);

  vConfiguration := TConfiguration(AInteractor.Configuration);
  AppendIconsToImageList(vConfiguration.Icons);

  FreeAndNil(vImage);
  FreeAndNil(vPlaceholder);
end;

procedure TWebUniGUIPresenter.DoEnumerateControls(const ALayout: TObject; const AControls: TList<TObject>);
var
  vParentControl: TWinControl;
  i: Integer;
begin
  if not (ALayout is TWinControl) then
    Exit;
  if TWinControl(ALayout).ControlCount <= 0 then
    Exit;

  vParentControl := TWinControl(ALayout);
  for i := 0 to vParentControl.ComponentCount - 1 do
    if vParentControl.Components[i] is TMenu then
      AControls.Add(vParentControl.Components[i]);

  for i := 0 to vParentControl.ControlCount - 1 do
    AControls.Add(vParentControl.Controls[i]);
end;

function TWebUniGUIPresenter.DoGetLayoutCaption(const ALayout: TObject): string;
begin
  if ALayout is TPageControl then
    Result := TPageControl(ALayout).Hint
  else if ALayout is TMemo then
  begin
    TMemo(ALayout).WordWrap := False;
    TMemo(ALayout).WantReturns := False;
    Result := TMemo(ALayout).Lines.Text;
  end
  else
    Result := TPanel(ALayout).Caption;
end;

function TWebUniGUIPresenter.DoGetLayoutKind(const ALayout: TObject): TLayoutKind;
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

procedure TWebUniGUIPresenter.SetApplicationUI(const AAppTitle, AIconName: string);
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

procedure TWebUniGUIPresenter.DoSetCursor(const ACursorType: TCursorType);
begin
  { TODO -owa : Эта операция должна делаться открытием модальной формы со статусом }
end;

procedure TWebUniGUIPresenter.DoSetLayoutCaption(const ALayout: TObject; const ACaption: string);
begin
  TPanel(ALayout).Caption := ACaption;
end;

{ TUniGUIPresenter }

procedure TUniGUIPresenter.ArrangeMozaic(const AParentForm: TUniForm);
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
{  vTotalForms := AParentForm.ChildCount;
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
    GetClientRect(AParentForm.ClientHandle, vClientRect);
    for i := 0 to AParentForm.MDIChildCount - 1 do
    begin
      vTotal := 0;
      for j := 0 to vColCount - 1 do
      begin
        vTotal := vTotal + vMap[j];
        if i < vTotal then
        begin
          vRow := (i - (vTotal - vMap[j])) mod vMap[j];
          LayoutForm(j, vRow, vClientRect.Width, vClientRect.Height, AParentForm.MDIChildren[i]);
          Break;
        end;
      end;
    end;
  finally
    SetLength(vMap, 0);
  end;}
end;

procedure TUniGUIPresenter.ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement);
var
  vForm: TForm;
begin
  if not AInteractor.UIBuilder.IsMDIStyle then
    Exit;

{  vForm := TForm(GetNativeControl(AInteractor.RootArea));
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
  end;}
end;

function TUniGUIPresenter.CreateAreaContent(const AArea: TUIArea;
  const AView: TView; const ALayout: TLayout; const AParams: string): TNativeControl;
var
  vControl: TObject;
begin
  Result := GetNativeControlClass.Create(AArea, AParams);
  vControl := CreateControl(AArea.Parent, AView, ALayout, AParams);
  Result.CreateContent(vControl);
end;

function TUniGUIPresenter.CreateAreaContentItem(const AOwner, AArea: TUIArea;
  const ANavItem: TNavigationItem; const ACaption, AHint: string; const AImageIndex: Integer): TNativeControl;
var
  vControl: TObject;
begin
  Result := GetNativeControlClass.Create(AArea, '');
  vControl := TVCLControl(AOwner.NativeControl).CreateItem(AArea.Parent, ANavItem, ACaption, AHint, AImageIndex);
  Result.CreateContent(vControl);
end;

function TUniGUIPresenter.CreateControl(const AParent: TUIArea; const AView: TView;
  const ALayout: TLayout; const AParams: string): TObject;
{var
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
  vParentControl: TObject; }
begin
  Result := nil;

{  vInteractor := TInteractor(AView.Interactor);
  vUIBuilder := vInteractor.UIBuilder;
  vDomain := TDomain(vInteractor.Domain);
  vParentControl := GetNativeControl(AParent);

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
        TDragImageList(vUIBuilder.Images[16]).GetIcon(AParent.GetImageID(TDefinition(AView.Definition)._ImageID), vForm.Icon);

      Result := vForm;
    end
    else begin
      vTab := TTabSheet.Create(TWinControl(vParentControl));
      vTab.Caption := ALayout.Caption;
      vTab.ImageIndex := AParent.GetImageID(ALayout.ImageID);

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

      RestoreUILayout(vInteractor, vForm);

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
          if (vArea.View = AView) and (GetNativeControl(vArea) is TForm) then
            Exit(vArea);
        end;
      end;

      vForm := TFloatFm.Create(nil);

      vForm.OnClose := DoFloatFormClose;
      vForm.Position := poMainFormCenter;
      vForm.Font.Size := 12;
      vForm.Caption := ALayout.Caption;
      vForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      if (AView.DefinitionKind in [dkCollection, dkAction, dkEntity]) then
        TDragImageList(vUIBuilder.Images[16]).GetIcon(vArea.GetImageID(TDefinition(AView.Definition)._ImageID), vForm.Icon);
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
        vMenuItem.Caption := ALayout.Caption;
        vMenuItem.Hint := ALayout.Caption;
        vMenuItem.ImageIndex := AParent.GetImageID(ALayout.ImageID);
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
    Assert(False, 'Пустой класс для лэйаута'); }
end;

function TUniGUIPresenter.DoCreateImages(const ADomain: TObject; const AImages: TImages; const ASize: Integer): TObject;
var
  vImageList: TImageList;
  vImage: TPngImage;
  vIndex: Integer;
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

  for vIndex in AImages.Indices.Keys do
    TDomain(ADomain).UIBuilder.StoreImageIndex(vIndex, AImages.Indices[vIndex]);

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

function TUniGUIPresenter.GetImagePlaceholder(const ASize: Integer): TStream;
var
  vPlaceholder: TBitmap;
  vResDiv8: Integer;
begin
  vPlaceholder := TBitmap.Create;
  vPlaceholder.SetSize(ASize, ASize);
  vPlaceholder.PixelFormat := pf32bit;
  vResDiv8 := Max(ASize div 8, 1);
  vPlaceholder.Canvas.Pen.Width := 1;
  vPlaceholder.Canvas.Pen.Color := clGray;
  vPlaceholder.Canvas.Rectangle(vResDiv8, vResDiv8, ASize - vResDiv8, ASize - vResDiv8);

  Result := TMemoryStream.Create;
  vPlaceholder.SaveToStream(Result);
end;

function TUniGUIPresenter.GetNativeControlClass: TNativeControlClass;
begin
  Result := TUniGUIControl;
end;

procedure TUniGUIPresenter.OnChildFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

function TUniGUIPresenter.DoLogin(const ADomain: TObject): TInteractor;
begin
  Result := inherited DoLogin(ADomain);
end;

procedure TUniGUIPresenter.DoLogout(const AInteractor: TInteractor);
begin
  inherited DoLogout(AInteractor);
  StoreUILayout(AInteractor, TForm(GetNativeControl(AInteractor.RootArea)));
end;

function TUniGUIPresenter.ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult;
var
  vView: TView;
  vForm: TForm;
  vCaption: string;
begin
  Result := drNone;
  if (AAreaName = '') or (AAreaName = 'float') or (AAreaName = 'free') then
  begin
    vForm := TForm(GetNativeControl(AArea));
    vForm.Show;
  end
  else if (AAreaName = 'child') or (AAreaName = 'modal') then
  begin
    vForm := TForm(GetNativeControl(AArea));
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

procedure TUniGUIPresenter.UniServerModuleBeforeInit(Sender: TObject);
begin
  // Иконку в рантайме пока можно установить только так
  TUniServerModule(Sender).Favicon.LoadFromFile(_Platform.Domains[0].Configuration.IconFileName);
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
var
  vDomain: TDomain;
  vInteractor: TInteractor;
begin
  Application.Title := cPlatformTitle;
  Application.Initialize;

  Application.OnShortCut := OnShortCut;

  vDomain := _Platform.Domains[0];
  vInteractor := Login(vDomain);
  if Assigned(vInteractor) and (AParameter <> '') then
    vDomain.ExecuteDefaultAction(TUserSession(vInteractor.Session), AParameter);

  Application.Run;
end;

procedure TUniGUIPresenter.DoSetCursor(const ACursorType: TCursorType);
begin
{ TODO -owa : Эта операция должна делаться открытием модальной формы со статусом }
end;

function TUniGUIPresenter.DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult;
var
  vRes: Integer;
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
    Exit(drCancel);

  vRes := FormInstance.MessageDlg(AText, mtConfirmation, vButtons);
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

procedure TUniGUIPresenter.DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType);
begin
  TUniMainForm(TUniMainModule(UniApplication.UniMainModule).GetFormInstance(TUniMainForm)).ShowMessage(AText);
  case AMessageType of
    msInfo: FormInstance.MessageDlg(AText, mtInformation, [mbOk]);
    msWarning: FormInstance.MessageDlg(AText, mtWarning, [mbOk]);
    msError: FormInstance.MessageDlg(AText, mtError, [mbOk]);
  else
    FormInstance.ShowMessage(AText);
  end;
end;

function TUniGUIPresenter.DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt,
  ADefaultDir: string): Boolean;
begin
//  Upload
  Result := False;

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

function TUniGUIPresenter.DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean;
begin
//  Download
  Result := False;

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
  Application.Title := AAppTitle;
  if FileExists(AIconName) then
    Application.Icon.LoadFromFile(AIconName);
end;

{ TUniArea }

procedure TUniArea.AppendServiceArea(const ALayoutName: string);
begin

end;

function TUniArea.AreaFromSender(const ASender: TObject): TUIArea;
begin
  if Assigned(ASender) and (ASender is TComponent) then
    Result := TUIArea(TComponent(ASender).Tag)
  else
    Result := nil;
end;

constructor TUniArea.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean;
  const AControl, ALayout: TObject; const AParams: string);
begin
  inherited Create(AParent, AView, AId, AIsService, AControl, ALayout, AParams);
end;

destructor TUniArea.Destroy;
begin
  inherited Destroy;
end;

procedure TUniArea.DoClose(const AModalResult: Integer);
begin

end;

function TUniArea.DoCreateChildAction(const ALayout: TObject; const AView: TView; const AParams: string): TUIArea;
begin

end;

function TUniArea.DoCreateChildArea(const ALayout: TObject; const AView: TView; const AParams: string;
  const AOnClose: TProc): TUIArea;
begin

end;

procedure TUniArea.PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  FControl.SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TUniArea.SetViewState(const AValue: TViewState);
begin

end;

initialization

RegisterServerModuleClass(TUniServerModule);
RegisterMainModuleClass(TUniMainModule);
RegisterAppFormClass(TUniMainForm);

TBaseModule.RegisterModule('UI', 'Web.uniGUI', TWebUniGUIPresenter);

end.
