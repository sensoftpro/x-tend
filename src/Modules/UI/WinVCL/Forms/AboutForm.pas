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

unit AboutForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  cxGraphics, Menus, StdCtrls, ImgList, CheckLst, cxLookAndFeels, cxLookAndFeelPainters,
  cxControls, cxContainer, cxEdit, cxLabel, uInteractor, dxGDIPlusClasses,
  Vcl.ExtCtrls, System.ImageList, Vcl.Buttons;

type
  TAboutFm = class(TForm)
    lblVersion: TLabel;
    btnOk: TButton;
    btnFeedback: TButton;
    lblCopyright: TLabel;
    lblSite: TcxLabel;
    lblMail: TcxLabel;
    Image1: TImage;
    lblName: TLabel;
    lblEmail: TLabel;
    lblWeb: TLabel;
    lblAllRightsReserved: TLabel;
    procedure lblSiteClick(Sender: TObject);
    procedure lblMailClick(Sender: TObject);
  private
    FInteractor: TInteractor;
    procedure Init(const AInteractor: TInteractor);
  public
    class procedure ShowAbout(const AInteractor: TInteractor);
  end;

implementation

{$R *.dfm}

uses
  uConfiguration, uDomain, uPresenter;

procedure TAboutFm.Init(const AInteractor: TInteractor);
begin
  FInteractor := AInteractor;
  if not Assigned(FInteractor) then
    Exit;
  Caption := AInteractor.Translate('@' + Self.ClassName + '@Caption', 'О программе');
  lblName.Caption := TDomain(AInteractor.Domain).AppName;
  lblVersion.Caption := AInteractor.Translate('@' + Self.ClassName + '.lblVersion@Caption', 'Версия') + ': ' +
    TConfiguration(AInteractor.Configuration).Version.ToString;
  lblWeb.Caption := AInteractor.Translate('@' + Self.ClassName + '.lblWeb@Caption', 'Сайт') + ': ';
  lblAllRightsReserved.Caption := AInteractor.Translate('@' + Self.ClassName + '.lblAllRightsReserved@Caption',
    'Все права защищены') + '.';
  btnFeedback.Caption := AInteractor.Translate('@' + Self.ClassName + '.btnFeedback@Caption', 'Обратная связь');
  btnOk.Caption := AInteractor.Translate('@btnOk@Caption', 'Ок');
end;

procedure TAboutFm.lblMailClick(Sender: TObject);
begin
  TPresenter(FInteractor.Presenter).OpenFile('mailto:' + lblMail.Caption);
end;

procedure TAboutFm.lblSiteClick(Sender: TObject);
begin
  TPresenter(FInteractor.Presenter).OpenFile(lblSite.Caption);
end;

class procedure TAboutFm.ShowAbout(const AInteractor: TInteractor);
var
  vForm: TAboutFm;
begin
  Application.CreateForm(TAboutFm, vForm);
  vForm.Init(AInteractor);
  vForm.ShowModal;
  vForm.Free;
end;

end.
