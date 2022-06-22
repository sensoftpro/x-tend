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

unit LoginVclForm;

interface

uses
  Windows, Messages, Graphics, Forms,
  StdCtrls, Controls, ExtCtrls, Classes, Buttons, Mask, Vcl.Menus;

type
  TLoginVclFm = class(TForm)
    Image1: TImage;
    lblLogin: TLabel;
    lblPassword: TLabel;
    edLogin: TEdit;
    edPass: TMaskEdit;
    edRFID: TEdit;
    OkCancel: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    function GetLoginName: string;
    function GetPass: string;
    procedure SetLoginName(const Value: string);
    procedure SetPass(const Value: string);
    function GetRFID: string;
    procedure SetRFID(const Value: string);
  public
    procedure Init(const ACaption: string);

    property LoginName: string read GetLoginName write SetLoginName;
    property Pass: string read GetPass write SetPass;
    property RFID: string read GetRFID write SetRFID;
    procedure OnRFIDReceived(const ANewRFID: string);
  end;

implementation

uses
  uPlatform;

{$R *.dfm}

{ TLoginFm }

procedure TLoginVclFm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    ModalResult := mrOk;
end;

function TLoginVclFm.GetLoginName: string;
begin
  Result := edLogin.Text;
end;

function TLoginVclFm.GetPass: string;
begin
  Result := edPass.Text;
end;

function TLoginVclFm.GetRFID: string;
begin
  Result := edRFID.Text;
end;

procedure TLoginVclFm.Init(const ACaption: string);
begin
  Caption := ACaption;
  lblLogin.Caption := _Platform.Translate('@' + Self.ClassName + '.lblLogin@Caption', 'Пользователь');
  lblPassword.Caption := _Platform.Translate('@' + Self.ClassName + '.lblPassword@Caption', 'Пароль');
  btnOk.Caption := _Platform.Translate('@btnOk@Caption', 'Ок');
  btnCancel.Caption := _Platform.Translate('@btnCancel@Caption', 'Отмена');
end;

procedure TLoginVclFm.OnRFIDReceived(const ANewRFID: string);
begin
  SetRFID(ANewRFID);
  ModalResult := mrOk;
  Close;
end;

procedure TLoginVclFm.SetLoginName(const Value: string);
begin
  edLogin.Text := Value;
end;

procedure TLoginVclFm.SetPass(const Value: string);
begin
  edPass.Text := Value;
end;

procedure TLoginVclFm.SetRFID(const Value: string);
begin
  edRFID.Text := Value;
end;

end.
