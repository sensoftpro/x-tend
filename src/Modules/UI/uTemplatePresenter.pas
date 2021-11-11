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

  uDefinition, uPresenter, uInteractor, uView, uUIBuilder;

type
  TImageResolution = (ir16x16, ir24x24, ir32x32);

type
  TWebUniGUIPresenter = class(TPresenter)
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
  end;

implementation

uses
  Math, ServerModule,

  uPlatform, uModule, uDomain, uUtils, uConfiguration, uChangeManager, uIcon, uEntity, uEntityList, uSession;

{ TWebUniGUIPresenter }

procedure TWebUniGUIPresenter.ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement);
begin

end;

procedure TWebUniGUIPresenter.CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea);
begin

end;

constructor TWebUniGUIPresenter.Create(const AStyleName: string);
begin
  inherited Create(AStyleName);
end;

function TWebUniGUIPresenter.CreateFieldArea(const AParentArea: TUIArea; const ALayout: TObject;
  const AView: TView; const AStyleName, AParams: string): TUIArea;
begin
  Result := nil;
end;

function TWebUniGUIPresenter.CreateLayoutArea(const ALayoutKind: TLayoutKind; const AParams: string): TObject;
begin
  Result := nil;
end;

function TWebUniGUIPresenter.CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea;
  const AView: TView; const AAreaName: string; const ACallback: TNotifyEvent = nil): TUIArea;
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
  Result := nil;
end;

procedure TWebUniGUIPresenter.DoLogout(const AInteractor: TInteractor);
begin
end;

procedure TWebUniGUIPresenter.ShowLayout(const AInteractor: TInteractor; const ATargetAreaName, ALayoutName: string);
begin
  if Assigned(AInteractor) then
    AInteractor.UIBuilder.Navigate(nil, ATargetAreaName, ALayoutName);
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

procedure TWebUniGUIPresenter.DoAuthorize(const AAccount: TObject; const AURL: string;
  const AWidth, AHeight: Integer; const AOnNavigated: TNavigateEvent);
begin

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

procedure TWebUniGUIPresenter.DoToggleUI(const AVisible: Boolean);
begin

end;

procedure TWebUniGUIPresenter.EnumerateControls(const ALayout: TObject; const AControls: TList<TObject>);
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

function TWebUniGUIPresenter.GetPresenterName: string;
begin
  Result := 'WebUniGUI';
end;

function TWebUniGUIPresenter.GetLayoutCaption(const ALayout: TObject): string;
begin
  if ALayout is TPageControl then
    Result := TPageControl(ALayout).Hint
  else
    Result := TPanel(ALayout).Caption;
end;

function TWebUniGUIPresenter.GetLayoutKind(const ALayout: TObject): TLayoutKind;
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

procedure TWebUniGUIPresenter.SetLayoutCaption(const ALayout: TObject; const ACaption: string);
begin
  TPanel(ALayout).Caption := ACaption;
end;

initialization

TBaseModule.RegisterModule('UI', 'Web.uniGUI', TWebUniGUIPresenter);

end.
