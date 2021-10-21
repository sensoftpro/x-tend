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

unit TranslationEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, cxButtons, uInteractor, uLocalizator, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, Vcl.Menus, Vcl.ExtCtrls;

type
  TTranslationEditFm = class(TForm)
    cbxLanguage: TComboBox;
    lblLanguage: TLabel;
    memTranslation: TMemo;
    memSourceText: TMemo;
    lblKey: TLabel;
    lblKeyValue: TLabel;
    btnEdit: TButton;
    btnOk: TcxButton;
    btnCancel: TcxButton;
    procedure cbxLanguageChange(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FInteractor: TInteractor;
    FLocalizator: TCfgLocalizator;
    FTranslations: TStrings;
    FCurItemIndex: Integer;
    procedure Init(const AInteractor: TInteractor; const AKey: string);
    procedure FillMemo(const AMemo: TMemo; const AText: string);
    procedure SaveTranslation;
  public
    class function Edit(const AInteractor: TInteractor; const AKey: string): Boolean;
  end;

implementation

uses
  uDomain;

{$R *.dfm}

procedure TTranslationEditFm.btnEditClick(Sender: TObject);
var
  vOldName: string;
  vNewName: string;
begin
  vOldName := lblKeyValue.Caption;
  vNewName := Trim(InputBox(FLocalizator.Translate('txtEditKey', 'Редактирование ключа'),
    FLocalizator.Translate('txtInputNewName', 'Введите новое наименование ключа'), vOldName));
  if SameText(vOldName, vNewName) then
    Exit;

  if FLocalizator.RenameKey(vOldName, vNewName) then
    lblKeyValue.Caption := vNewName;
end;

procedure TTranslationEditFm.cbxLanguageChange(Sender: TObject);
begin
  SaveTranslation;

  if cbxLanguage.ItemIndex >= 0 then
  begin
    FCurItemIndex := cbxLanguage.ItemIndex;
    FillMemo(memTranslation, FTranslations[FCurItemIndex]);
  end;
end;

class function TTranslationEditFm.Edit(const AInteractor: TInteractor; const AKey: string): Boolean;
var
  vForm: TTranslationEditFm;
  i: Integer;
  vLanguage: TLanguage;
begin
  Application.CreateForm(TTranslationEditFm, vForm);
  try
    vForm.Init(AInteractor, AKey);

    Result := vForm.ShowModal = mrOk;
    if Result then
    begin
      vForm.SaveTranslation;
      for i := 0 to vForm.FLocalizator.Languages.Count - 1 do
      begin
        vLanguage := vForm.FLocalizator.Languages[i];
        vLanguage[AKey] := vForm.FTranslations[i];
      end;
    end;
  finally
    vForm.Free;
  end;
end;

procedure TTranslationEditFm.FillMemo(const AMemo: TMemo; const AText: string);
begin
  AMemo.Text := UnEscapeText(AText);
end;

procedure TTranslationEditFm.FormDestroy(Sender: TObject);
begin
  if Assigned(FTranslations) then
    FTranslations.Free;
end;

procedure TTranslationEditFm.Init(const AInteractor: TInteractor; const AKey: string);
var
  i: Integer;
  vLanguage: TLanguage;
begin
  FInteractor := AInteractor;
  FLocalizator := TDomain(FInteractor.Domain).Configuration.Localizator;
  Caption := FInteractor.Translate('@' + Self.ClassName + '@Caption', 'Перевод');
  lblKey.Caption := FInteractor.Translate('@' + Self.ClassName + '.lblKey@Caption', 'Ключ для перевода') + ':';
  lblLanguage.Caption := FInteractor.Translate('@' + Self.ClassName + '.lblLanguage@Caption', 'Язык перевода') + ':';
  lblKeyValue.Caption := FInteractor.Translate('@' + Self.ClassName + '.lblKeyValue@Caption', 'не задан');
  btnOk.Caption := FInteractor.Translate('@btnOk@Caption', 'Ок');
  btnCancel.Caption := FInteractor.Translate('@btnCancel@Caption', 'Отмена');

  FTranslations := TStringList.Create;
  for i := 0 to FLocalizator.Languages.Count - 1 do
  begin
    vLanguage := FLocalizator.Languages[i];
    cbxLanguage.Items.Add(vLanguage.Name);
    FTranslations.Add(vLanguage[AKey]);
  end;

  lblKeyValue.Caption := AKey;

  FillMemo(memSourceText, FLocalizator.DefaultLanguage[AKey]);

  FCurItemIndex := cbxLanguage.Items.IndexOf(TDomain(FInteractor.Domain).Language);
  cbxLanguage.ItemIndex := FCurItemIndex;
  if FCurItemIndex > -1 then
    FillMemo(memTranslation, FTranslations[FCurItemIndex]);
end;

procedure TTranslationEditFm.SaveTranslation;
begin
  FTranslations[FCurItemIndex] := EscapeText(memTranslation.Text);
end;

end.
