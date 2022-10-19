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

unit uDevExpressVCLPresenter;

interface

uses
  Messages, Classes, Generics.Collections, Generics.Defaults, ActnList, uConsts, StdCtrls, Buttons,
  ExtCtrls, TypInfo, Types, ComCtrls, SysUtils, Windows, Graphics,
  Variants, Controls, Forms, Mask, Menus,

  uIcon, uDefinition, uPresenter, uInteractor, uView, uSettings, uCustomVCLPresenter, uCustomVCLArea,

  StartForm, SplashForm, uUIBuilder;

type
  TDevExpressVCLPresenter = class(TCustomVCLPresenter)
  private
    FRowStyle: TObject;
  protected
    FStartForm: TStartFm;
    [Weak] FSplashForm: TSplashFm;
    procedure CreateStartForm; override;
    function DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject; override;
    procedure DoStop; override;
    function GetAreaClass: TVCLAreaClass; override;
    procedure ShowAboutForm(const AParams: TObject = nil); override;
    function ShowLoginForm(const AAppTitle: string; var ALoginName, APass: string): Boolean; override;
    procedure ShowReportForm(const AParams: TObject = nil); override;
    procedure ShowSplashForm(const AParams: TObject = nil); override;
    procedure AppendIconsToImageList(const AInteractor: TInteractor; const AImageList: TDragImageList; const AResolution: Integer; const AIcons: TIcons); override;
  public
    constructor Create(const AName: string; const ASettings: TSettings); override;
    destructor Destroy; override;

    property RowStyle: TObject read FRowStyle;
  end;

implementation

uses
  Dialogs, Math, StrUtils, ShellAPI, ActiveX,
  cxGraphics, dxGDIPlusClasses, cxImage, cxEdit, cxPC, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxMemo,

  uPlatform, uModule, uDomain, uUtils, uConfiguration, uChangeManager, uEntity, uEntityList, uDevExpressArea, uSession,
  uManagedForm, AboutForm, ReportConfigureForm, LoginForm;

{ TDevExpressVCLPresenter }

procedure TDevExpressVCLPresenter.AppendIconsToImageList(const AInteractor: TInteractor; const AImageList: TDragImageList; const AResolution: Integer;
  const AIcons: TIcons);
var
  vIndex: Integer;
  vStream: TStream;
  vBitmap: TBitmap;
  vImage: TdxPNGImage;
begin
  vImage := TdxPNGImage.Create;

  for vIndex in AIcons.IconIndices do
  begin
    AInteractor.StoreImageIndex(vIndex, AImageList.Count);

    vStream := AIcons.IconByIndex(vIndex, AResolution);

    if not Assigned(vStream) then
      AImageList.Add(GetImagePlaceholder(AResolution), nil)
    else
    begin
      vStream.Position := 0;
      vImage.LoadFromStream(vStream);
      vBitmap := vImage.GetAsBitmap;
      try
        if AImageList is TcxImageList then
          TcxImageList(AImageList).AddBitmap(vBitmap, nil, clnone, True, True)
        else
          AImageList.Add(vBitmap, nil);
      finally
        if vIndex > 0 then
          vBitmap.Free;
      end;
    end;
  end;

  FreeAndNil(vImage);
end;

constructor TDevExpressVCLPresenter.Create(const AName: string; const ASettings: TSettings);
begin
  inherited;
  FRowStyle := TcxStyle.Create(nil);
end;

procedure TDevExpressVCLPresenter.CreateStartForm;
begin
  Application.CreateForm(TStartFm, FStartForm);
  FStartForm.Init(Self);
end;

destructor TDevExpressVCLPresenter.Destroy;
begin
  FreeAndNil(FRowStyle);
  inherited;
end;

function TDevExpressVCLPresenter.DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject;
begin
  Result := TcxImageList.Create(nil);
  TcxImageList(Result).SetSize(ASize, ASize);
  LoadImages(AInteractor, TcxImageList(Result), ASize);
end;

procedure TDevExpressVCLPresenter.DoStop;
begin
  inherited;
  if Assigned(FStartForm) then
    FStartForm.Deinit;
end;

function TDevExpressVCLPresenter.GetAreaClass: TVCLAreaClass;
begin
  Result := TDevExpressArea;
end;

procedure TDevExpressVCLPresenter.ShowAboutForm(const AParams: TObject);
begin
  TAboutFm.ShowAbout(ActiveInteractor);
end;

function TDevExpressVCLPresenter.ShowLoginForm(const AAppTitle: string; var ALoginName, APass: string): Boolean;
var
  vLoginForm: TLoginFm;
begin
  vLoginForm := TLoginFm.Create(nil);
  try
    vLoginForm.Init(AAppTitle);
    vLoginForm.LoginName := ALoginName;
    vLoginForm.Pass := APass;

    Result := vLoginForm.ShowModal = mrOk;

    if Result then
    begin
      ALoginName := vLoginForm.LoginName;
      APass := vLoginForm.Pass;
    end;
  finally
    FreeAndNil(vLoginForm);
  end;
end;

procedure TDevExpressVCLPresenter.ShowReportForm(const AParams: TObject);
var
  vForm: TReportConfigureFm;
begin
  vForm := TReportConfigureFm.Create(nil);
  vForm.Init(ActiveInteractor);
  try
    vForm.ShowModal;
  finally
    vForm.Free;
  end;
end;

procedure TDevExpressVCLPresenter.ShowSplashForm(const AParams: TObject);
var
  vProgressInfo: TProgressInfo;
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
end;

initialization

TBaseModule.RegisterModule('UI', 'Windows.DevExpress', TDevExpressVCLPresenter);

end.
