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

unit StartForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls;

type
  TStartFm = class(TForm)
    btnRun: TButton;
    lbxActiveDomains: TListBox;
    lblLanguage: TLabel;
    cbxLanguage: TComboBox;
    lblAvailableConfigurations: TLabel;
    procedure btnRunClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxActiveDomainsDblClick(Sender: TObject);
    procedure cbxLanguageChange(Sender: TObject);
  private
    FPresenter: TObject;
    procedure FillAvailableConfigurations;
  public
    procedure Init(const APresenter: TObject);
    procedure Deinit;
  end;

var
  StartFm: TStartFm;

implementation

uses
  uPlatform, uDomain, uPresenter, uLocalizator;

{$R *.dfm}

{ TStartFm }

procedure TStartFm.btnRunClick(Sender: TObject);
begin
  if lbxActiveDomains.ItemIndex >= 0 then
    TPresenter(FPresenter).Login(TDomain(lbxActiveDomains.Items.Objects[lbxActiveDomains.ItemIndex]));
end;

procedure TStartFm.cbxLanguageChange(Sender: TObject);
begin
  if cbxLanguage.ItemIndex < _Platform.Localizator.Languages.Count then
    _Platform.Language := cbxLanguage.Items[cbxLanguage.ItemIndex]
  else
    _Platform.Language := '';
  FillAvailableConfigurations;
end;

procedure TStartFm.Deinit;
begin
  FPresenter := nil;
end;

procedure TStartFm.FillAvailableConfigurations;
var
  vDomain: TDomain;
begin
  Caption := _Platform.Translate('@' + Self.ClassName + '@Caption', 'Выбор конфигурации');
  lblLanguage.Caption := _Platform.Translate('@' + Self.ClassName + '.lblLanguage@Caption', 'Язык платформы');
  lblAvailableConfigurations.Caption := _Platform.Translate('@' + Self.ClassName + '.lblAvailableConfigurations@Caption', 'Доступные конфигурации');
  btnRun.Caption := _Platform.Translate('@' + Self.ClassName + '.btnRun@Caption', 'Запуск');

  lbxActiveDomains.Items.Clear;
  for vDomain in _Platform.Domains do
  begin
    if vDomain.Configuration.Localizator.SupportLanguage(_Platform.Language) then
    begin
      vDomain.Language := _Platform.Language;
      lbxActiveDomains.Items.AddObject(vDomain.Translate('AppTitle', vDomain.Configuration._Caption), vDomain);
    end;
  end;

  if lbxActiveDomains.Items.Count > 0 then
    lbxActiveDomains.ItemIndex := 0;
end;

procedure TStartFm.FormShow(Sender: TObject);
begin
  if lbxActiveDomains.ItemIndex >= 0 then
    btnRun.SetFocus;
end;

procedure TStartFm.Init(const APresenter: TObject);
var
  vIndex: Integer;
  vLanguage: TLanguage;
begin
  FPresenter := APresenter;

  cbxLanguage.Items.Clear;
  for vLanguage in _Platform.Localizator.Languages do
    cbxLanguage.Items.Add(vLanguage.Name);
  cbxLanguage.Items.Add('Русский');

  vIndex := cbxLanguage.Items.IndexOf(_Platform.Language);
  if vIndex < 0 then
    vIndex := cbxLanguage.Items.Count;

  cbxLanguage.ItemIndex := vIndex;

  FillAvailableConfigurations;
end;

procedure TStartFm.lbxActiveDomainsDblClick(Sender: TObject);
begin
  if lbxActiveDomains.ItemIndex >= 0 then
    TPresenter(FPresenter).Login(TDomain(lbxActiveDomains.Items.Objects[lbxActiveDomains.ItemIndex]));
end;

end.
