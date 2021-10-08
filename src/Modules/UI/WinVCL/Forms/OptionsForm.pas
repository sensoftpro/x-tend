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

unit OptionsForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, OkCancelFrame, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxStyles, uInteractor, uLocalizator;

type
  TOptionsFm = class(TForm)
    chbShowStartPage: TCheckBox;
    OkCancelFrm1: TOkCancelFrm;
    lblLanguage: TLabel;
    btnEditDictionaries: TButton;
    lbxLanguages: TListBox;
    btnAddLanguage: TButton;
    chbShowHorzLinesInGrids: TCheckBox;
    chbShowBordersForDisabled: TCheckBox;
    chbMouseMultiSelectInGrids: TCheckBox;
    chbMarkRequiredFields: TCheckBox;
    procedure btnEditDictionariesClick(Sender: TObject);
  private
    FInteractor: TInteractor;
    FLocalizator: TCfgLocalizator;
    procedure Init(const AInteractor: TInteractor);
  public
    class function Edit(const AInteractor: TInteractor): Boolean;
  end;

implementation

{$R *.dfm}

uses
  StrUtils, uConfiguration, uDomain, uSettings;

{ TOptionsFm }

procedure TOptionsFm.btnEditDictionariesClick(Sender: TObject);
begin
  FInteractor.ShowMessage('Пока не реализовано');
end;

class function TOptionsFm.Edit(const AInteractor: TInteractor): Boolean;
var
  vForm: TOptionsFm;
  vSettings: TSettings;
  vLanguage: TLanguage;
begin
  vSettings := TDomain(AInteractor.Domain).UserSettings;
  Application.CreateForm(TOptionsFm, vForm);
  with vForm do
  begin
    Init(AInteractor);
    chbShowStartPage.Checked := StrToBoolDef(vSettings.GetValue('Core', 'ShowStartPage'), True);
    chbShowHorzLinesInGrids.Checked := StrToBoolDef(vSettings.GetValue('Core', 'ShowHorzLines'), True);
    chbShowBordersForDisabled.Checked := StrToBoolDef(vSettings.GetValue('Core', 'ShowBordersForDisabled'), True);
    chbMouseMultiSelectInGrids.Checked := StrToBoolDef(vSettings.GetValue('Core', 'MouseMultiSelectInGrids'), True);
    chbMarkRequiredFields.Checked := StrToBoolDef(vSettings.GetValue('Core', 'MarkRequiredFields'), True);

    lbxLanguages.Items.Clear;
    lbxLanguages.Items.Add(FInteractor.Translate('txtDefaultLanguage_Russian', 'По умолчанию (русский)'));
    for vLanguage in FLocalizator.Languages do
      lbxLanguages.Items.Add(vLanguage.Name);

    Result := vForm.ShowModal = mrOk;
    if Result then
    begin
      vSettings.SetValue('Core', 'ShowStartPage', IfThen(chbShowStartPage.Checked, '1', '0'));
      vSettings.SetValue('Core', 'ShowHorzLines', IfThen(chbShowHorzLinesInGrids.Checked, '1', '0'));
      vSettings.SetValue('Core', 'ShowBordersForDisabled', IfThen(chbShowBordersForDisabled.Checked, '1', '0'));
      vSettings.SetValue('Core', 'MouseMultiSelectInGrids', IfThen(chbMouseMultiSelectInGrids.Checked, '1', '0'));
      vSettings.SetValue('Core', 'MarkRequiredFields', IfThen(chbMarkRequiredFields.Checked, '1', '0'));
    end;
  end;

  FreeAndNil(vForm);
end;

procedure TOptionsFm.Init(const AInteractor: TInteractor);
begin
  FInteractor := AInteractor;
  FLocalizator := TConfiguration(FInteractor.Configuration).Localizator;
  OkCancelFrm1.Init(FInteractor);
  Caption := FInteractor.Translate('@' + Self.ClassName + '@Caption', 'Опции');
  lblLanguage.Caption := FInteractor.Translate('@' + Self.ClassName + '.lblLanguages@Caption', 'Языки приложения');
  btnAddLanguage.Caption := FInteractor.Translate('@' + Self.ClassName + '.btnAddLanguage@Caption', 'Добавить язык');
  btnEditDictionaries.Caption := FInteractor.Translate('@' + Self.ClassName + '.btnEditDictionaries@Caption', 'Редактировать словари');
  chbShowStartPage.Caption := FInteractor.Translate('@' + Self.ClassName + '.chbShowStartPage@Caption',
    'Показывать стартовую страницу при запуске приложения');
  chbShowHorzLinesInGrids.Caption := FInteractor.Translate('@' + Self.ClassName + '.chbShowHorzLinesInGrids@Caption',
    'Показывать горизонтальные линии в таблицах');
  chbShowBordersForDisabled.Caption := FInteractor.Translate('@' + Self.ClassName + '.chbShowBordersForDisabled@Caption',
    'Показывать обводку границ у неактивных полей');
  chbMouseMultiSelectInGrids.Caption := FInteractor.Translate('@' + Self.ClassName + '.chbMouseMultiSelectInGrids@Caption',
    'Множественный выбор строк в таблицах мышкой');
  chbMarkRequiredFields.Caption := FInteractor.Translate('@' + Self.ClassName + '.chbMarkRequiredFields@Caption',
    'Помечать обязательные поля символами **');
end;

end.
