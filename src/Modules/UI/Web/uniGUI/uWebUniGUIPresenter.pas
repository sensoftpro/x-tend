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

unit uWebUniGUIPresenter;

interface

uses
  Messages, Classes, Generics.Collections, Generics.Defaults, ActnList, uConsts, StdCtrls, Buttons,
  ExtCtrls, TypInfo, Types, ComCtrls, SysUtils, Windows, Graphics,
  Variants, Controls, Forms, Mask, Menus,

  uniGUIServer, uniGUIMainModule, uniGUIApplication, uIdCustomHTTPServer, uniGUITypes,
  uniGUIForm, uniEdit, uniGUIBaseClasses, uniLabel, uniButton,

  uSettings, uPresenter, uInteractor, uView, uUIBuilder;

type
  TImageResolution = (ir16x16, ir24x24, ir32x32);

type
  // Служебные объекты uniGUI

  TUniServerModule = class(TUniGUIServerModule)
  private
  protected
    procedure FirstInit; override;
  public
  end;

  TUniMainModule = class(TUniGUIMainModule)
    procedure UniGUIMainModuleCreate(Sender: TObject);
  private
    FInteractor: TInteractor;
  public
  end;

  TUniMainForm = class(TUniForm)
    UniLabel1: TUniLabel;
    UniEdit1: TUniEdit;
    UniButton1: TUniButton;
    procedure UniButton1Click(Sender: TObject);
    procedure UniFormCreate(Sender: TObject);
  private
  public
  end;

type
  TWebUniGUIPresenter = class(TPresenter)
  protected
    procedure DoRun(const AParameter: string); override;
    procedure DoStop; override;

    function DoLogin(const ADomain: TObject): TInteractor; override;
    procedure DoLogout(const AInteractor: TInteractor); override;

    procedure DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType); override;
    function DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult; override;
    procedure DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False); override;
    function DoSelectFile(var AFileName: string; const ADirectory: string = ''): Boolean; override;
    function DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt, ADefaultDir: string): Boolean; override;
    function DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean; override;

    procedure DoCloseAllPages(const AInteractor: TInteractor); override;

    function DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject; override;
  public
    constructor Create(const AName: string; const ASettings: TSettings); override;
    destructor Destroy; override;

    function CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView; const AAreaName: string;
      const ACallback: TNotifyEvent = nil; const ACaption: string = ''; const AOnClose: TProc = nil): TUIArea; override;
    function ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult; override;
    procedure CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea); override;

    function ShowPage(const AInteractor: TInteractor; const APageType: string; const AParams: TObject = nil): TDialogResult; override;
    procedure ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement); override;

    procedure DoEnumerateControls(const ALayout: TObject; const AControls: TList<TObject>); override;
    function CreateLayoutArea(const ALayoutKind: TLayoutKind; const AParams: string = ''): TObject; override;
    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); override;
    procedure DoSetLayoutCaption(const ALayout: TObject; const ACaption: string); override;
    function DoGetLayoutCaption(const ALayout: TObject): string; override;
    function DoGetLayoutKind(const ALayout: TObject): TLayoutKind; override;
  end;

implementation

uses
  Math, UniGUIVars, uPlatform, uModule, uDomain, uEntityList, uSession;

{$R Forms/ServerModule.dfm}
{$R Forms/MainModule.dfm}
{$R Forms/Main.dfm} // Это необязательный ресурс

{ TUniServerModule }

procedure TUniServerModule.FirstInit;
begin
  InitServerModule(Self);
end;

{ TUniMainModule }

procedure TUniMainModule.UniGUIMainModuleCreate(Sender: TObject);
var
  vDomain: TDomain;
  vSession: TUserSession;
  vUsers: TEntityList;
begin
  vDomain := _Platform.Domains[0];

  //Application.Title := vDomain.AppTitle;
  //if FileExists(vDomain.Configuration.IconFileName) then
  //  Application.Icon.LoadFromFile(vDomain.Configuration.IconFileName);

  //Caption := vDomain.AppTitle;

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
begin
  ShowToast('Привет');
end;

{ TWebUniGUIPresenter }

procedure TWebUniGUIPresenter.ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement);
begin

end;

procedure TWebUniGUIPresenter.CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea);
begin

end;

constructor TWebUniGUIPresenter.Create(const AName: string; const ASettings: TSettings);
begin
  inherited Create(AName, ASettings);
end;

function TWebUniGUIPresenter.CreateLayoutArea(const ALayoutKind: TLayoutKind; const AParams: string): TObject;
begin
  Result := nil;
end;

function TWebUniGUIPresenter.CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView;
  const AAreaName: string; const ACallback: TNotifyEvent = nil; const ACaption: string = ''; const AOnClose: TProc = nil): TUIArea;
//var
//  vForm: TForm;
begin
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
  Result := nil;
end;

function TWebUniGUIPresenter.DoLogin(const ADomain: TObject): TInteractor;
begin

//  Result.UIBuilder.Navigate(nil, '', vMainFormName, ''{, Result.UIHolder});

  Result := nil;
end;

procedure TWebUniGUIPresenter.DoLogout(const AInteractor: TInteractor);
begin
end;

function TWebUniGUIPresenter.ShowPage(const AInteractor: TInteractor; const APageType: string;
  const AParams: TObject = nil): TDialogResult;
begin
  Result := drNone;
end;

function TWebUniGUIPresenter.ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult;
begin
  Result := drNone;
end;

procedure TWebUniGUIPresenter.DoCloseAllPages(const AInteractor: TInteractor);
begin

end;

procedure TWebUniGUIPresenter.DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False);
begin

end;

procedure TWebUniGUIPresenter.DoRun(const AParameter: string);
begin
  inherited;

  Application.Title := cPlatformTitle;
  Application.Initialize;
  TUniServerModule.Create(Application);

  Login(_Platform.Domains[0]);

  DoOnAppStarted;

  Application.Run;
end;

function TWebUniGUIPresenter.DoSelectFile(var AFileName: string; const ADirectory: string = ''): Boolean;
begin
  Result := False;
end;

function TWebUniGUIPresenter.DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult;
begin
  Result := drNone;
end;

procedure TWebUniGUIPresenter.DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType);
begin

end;

function TWebUniGUIPresenter.DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt,
  ADefaultDir: string): Boolean;
begin
  Result := False;
end;

function TWebUniGUIPresenter.DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean;
begin
  Result := False;
end;

procedure TWebUniGUIPresenter.DoStop;
begin
  Application.Terminate;
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
  Application.Title := AAppTitle;
  if FileExists(AIconName) then
    Application.Icon.LoadFromFile(AIconName);
end;

procedure TWebUniGUIPresenter.DoSetLayoutCaption(const ALayout: TObject; const ACaption: string);
begin
  TPanel(ALayout).Caption := ACaption;
end;

initialization

RegisterServerModuleClass(TUniServerModule);
RegisterMainModuleClass(TUniMainModule);
RegisterAppFormClass(TUniMainForm);

TBaseModule.RegisterModule('UI', 'Web.uniGUI', TWebUniGUIPresenter);

end.
