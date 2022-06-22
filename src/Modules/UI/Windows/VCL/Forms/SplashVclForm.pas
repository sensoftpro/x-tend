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

unit SplashVclForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Menus, StdCtrls, ImgList, CheckLst, uPresenter, uDomain,
  Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TSplashVclFm = class(TForm)
    Image1: TImage;
    lblName: TLabel;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    lblInfo: TLabel;
    prbProgress: TProgressBar;
    lblSite: TLabel;
    procedure lblSiteClick(Sender: TObject);
  private
    FPresenter: TPresenter;
    procedure Init(const APresenter: TPresenter; const ADomain: TDomain);
  public
    procedure UpdateProgress(const AProgress: Integer; const AInfo: string);
    class function ShowSplash(const APresenter: TPresenter; const ADomain: TDomain): TSplashVclFm;
  end;

implementation

{$R *.dfm}

uses
  uConfiguration;

procedure TSplashVclFm.Init(const APresenter: TPresenter; const ADomain: TDomain);
var
  vSplashImage: TStream;
begin
  FPresenter := APresenter;
  if not Assigned(FPresenter) then
    Exit;
  lblName.Caption := ADomain.Translate('AppTitle', ADomain.Configuration._Caption);
  lblVersion.Caption := ADomain.Translate('@' + Self.ClassName + '.lblVersion@Caption', 'Версия') + ': ' +
    TConfiguration(ADomain.Configuration).Version;
  prbProgress.Position := 0;

  vSplashImage := ADomain.Configuration.Icons.Splash;
  if Assigned(vSplashImage) then
  begin
    vSplashImage.Position := 0;
    try
      // BUG: Если в проекте нет модуля, использующего FastReport, падает с ошибкой Unsupported Stream format
//      Image1.Picture.LoadFromStream(vSplashImage);
    except
      if ADomain.Configuration.Icons.SplashFileName <> '' then
        Image1.Picture.LoadFromFile(ADomain.Configuration.Icons.SplashFileName);
    end;
  end;
end;

procedure TSplashVclFm.lblSiteClick(Sender: TObject);
begin
  FPresenter.OpenFile(lblSite.Caption);
end;

class function TSplashVclFm.ShowSplash(const APresenter: TPresenter; const ADomain: TDomain): TSplashVclFm;
begin
  Result := TSplashVclFm.Create(nil);
  Result.Init(APresenter, ADomain);
  Result.Show;
  Result.Update;
end;

procedure TSplashVclFm.UpdateProgress(const AProgress: Integer; const AInfo: string);
begin
  prbProgress.Position := AProgress;
  prbProgress.Update;
  lblInfo.Caption := AInfo;
  lblInfo.Update;
  Application.ProcessMessages;
end;

end.
