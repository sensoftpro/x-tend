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

unit LangVclEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, IOUtils,
  Controls, Forms, Grids, uInteractor,Vcl.Menus,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TLangVclEditFm = class(TForm)
    gridLanguages: TStringGrid;
    btnCancel: TButton;
    btnOk: TButton;
    procedure gridLanguagesDblClick(Sender: TObject);
  private
    FInteractor: TInteractor;
    procedure Init(const AInteractor: TInteractor);
  public
    class procedure Edit(const AInteractor: TInteractor);
  end;

implementation

uses
  IniFiles, TranslationVCLEditForm, uConfiguration, uLocalizator;

{$R *.dfm}

{ TLangEditFm }

class procedure TLangVclEditFm.Edit(const AInteractor: TInteractor);
var
  vForm: TLangVclEditFm;
  i, j: Integer;
  vKeys: TStringList;
  vKey: string;
  vLocalizator: TCfgLocalizator;
  vLanguage: TLanguage;
begin
  vLocalizator := TConfiguration(AInteractor.Configuration).Localizator;

  Application.CreateForm(TLangVclEditFm, vForm);
  vForm.Init(AInteractor);

  vForm.gridLanguages.ColCount := vLocalizator.Languages.Count + 2;
  vForm.gridLanguages.Cells[1, 0] := 'Default';
  for i := 0 to vLocalizator.Languages.Count - 1 do
  begin
    vLanguage := vLocalizator.Languages[i];
    vForm.gridLanguages.Cells[i+2, 0] := vLanguage.Name;
  end;

  vKeys := TStringList.Create;
  try
    vLocalizator.FillKeys(vKeys);
    vKeys.Sort;
    vForm.gridLanguages.RowCount := vKeys.Count + 1;
    for j := 0 to vKeys.Count - 1 do
    begin
      vKey := vKeys[j];
      vForm.gridLanguages.Cells[0, j+1] := vKey;
      vForm.gridLanguages.Cells[1, j+1] := vLocalizator.DefaultLanguage[vKey];
      for i := 0 to vLocalizator.Languages.Count - 1 do
      begin
        vLanguage := vLocalizator.Languages[i];
        vForm.gridLanguages.Cells[i+2, j+1] := vLanguage[vKey];
      end;
    end;

    if vForm.ShowModal = mrOk then
    begin
      for i := 0 to vLocalizator.Languages.Count - 1 do
      begin
        vLanguage := vLocalizator.Languages[i];
        for j := 0 to vKeys.Count - 1 do
          vLanguage[vKeys[j]] := vForm.gridLanguages.Cells[i+2, j+1];
        vLanguage.Save;
      end;
    end;
  finally
    vKeys.Free;
  end;

  vForm.Free;
end;

procedure TLangVclEditFm.gridLanguagesDblClick(Sender: TObject);
var
  vPosition: TPoint;
  vCol, vRow: Integer;
  vKey: string;
  i: Integer;
  vLocalizator: TCfgLocalizator;
  vLanguage: TLanguage;
begin
  vLocalizator := TConfiguration(FInteractor.Configuration).Localizator;

  vPosition := gridLanguages.ScreenToClient(Mouse.CursorPos);
  gridLanguages.MouseToCell(vPosition.X, vPosition.Y, vCol, vRow);
  if vRow > 0 then
  begin
    vKey := gridLanguages.Cells[0, vRow];
    if TTranslationVCLEditFm.Edit(FInteractor, vKey) then
    begin
      for i := 0 to vLocalizator.Languages.Count - 1 do
      begin
        vLanguage := vLocalizator.Languages[i];
        gridLanguages.Cells[i+2, vRow] := vLanguage[vKey];
      end;
    end;
  end;
end;

procedure TLangVclEditFm.Init(const AInteractor: TInteractor);
begin
  FInteractor := AInteractor;
  Caption := FInteractor.Translate('@' + Self.ClassName + '@Caption', 'Редактирование словарей');
  btnOk.Caption := FInteractor.Translate('@btnOk@Caption', 'Ок');
  btnCancel.Caption := FInteractor.Translate('@btnCancel@Caption', 'Отмена');
end;

end.
