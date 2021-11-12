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

unit uSamplePresenter;

interface

uses
  Messages, Classes, Generics.Collections, Generics.Defaults, ActnList, uConsts, Controls, StdCtrls,
  ExtCtrls, ComCtrls, Variants, Forms, Mask, Menus,

  uDefinition, uPresenter, uInteractor, uView, uSettings, uUIBuilder;

type
  TImageResolution = (ir16x16, ir24x24, ir32x32);

type
  TSamplePresenter = class(TPresenter)
  protected
    procedure DoRun(const AParameter: string); override;
    procedure DoStop; override;

    function DoLogin(const ADomain: TObject): TInteractor; override;
    procedure DoLogout(const AInteractor: TInteractor); override;
    procedure DoAuthorize(const AAccount: TObject; const AURL: string; const AWidth, AHeight: Integer;
      const AOnNavigated: TNavigateEvent); override;

    procedure DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType); override;
    function DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult; override;
    procedure DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False); override;
    function DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt, ADefaultDir: string): Boolean; override;
    function DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean; override;
    procedure DoCloseAllPages(const AInteractor: TInteractor); override;

    procedure OnDomainLoadProgress(const AProgress: Integer; const AInfo: string); override;
    procedure OnDomainError(const ACaption, AText: string); override;

    function DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject; override;
  public
    constructor Create(const AName: string; const ASettings: TSettings); override;
    destructor Destroy; override;

    function CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView;
      const AAreaName: string; const ACallback: TNotifyEvent = nil): TUIArea; override;
    function ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult; override;
    procedure CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea); override;

    function ShowPage(const AInteractor: TInteractor; const APageType: string; const AParams: TObject = nil): TDialogResult; override;
    procedure ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement); override;

    function CreateLayoutArea(const ALayoutKind: TLayoutKind; const AParams: string = ''): TObject; override;
    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); override;
  end;

implementation

uses
  IOUtils, SysUtils,
  uPlatform, uModule, uDomain, uUtils, uConfiguration, uChangeManager, uIcon, uEntity, uEntityList, uSession;

{ TSamplePresenter }

procedure TSamplePresenter.ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement);
begin
  // Упорядочить дочерние окна
  if AInteractor.Layout <> 'mdi'  then
    Exit;

  case AArrangeKind of
    waCascade: ;
    waTileHorz: ;
    waTileVert: ;
  end;
end;

procedure TSamplePresenter.CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea);
begin
  // !!!Только для главной страницы
  if Assigned(AOldArea) then
  begin
    // Очистить содержимое страницы
  end
  else
    // Установить новую главную страницу
end;

constructor TSamplePresenter.Create(const AName: string; const ASettings: TSettings);
begin
  inherited Create(AName, ASettings);
  // Проинициализировать презентер из секции с именем AName в настройках
end;

function TSamplePresenter.CreateLayoutArea(const ALayoutKind: TLayoutKind; const AParams: string): TObject;
var
  vParams: TStrings;
begin
  // REQUIRED
  // Создать и проинициализировать размеченные области лейаута по их типу
  vParams := CreateDelimitedList(AParams);
  try
    case ALayoutKind of
      lkPanel: Result := nil;
      lkPage: Result := nil;
      lkFrame: Result := nil;
    else
      Result := nil;
    end;
  finally
    FreeAndNil(vParams);
  end;
end;

function TSamplePresenter.CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea;
  const AView: TView; const AAreaName: string; const ACallback: TNotifyEvent = nil): TUIArea;
begin
  // REQUIRED
  Result := nil;
  if AAreaName = '' then
  begin
    // Создать главную страницу (главную форму) приложения, корректно обработать MDI
    if (AInteractor.Layout = 'mdi') then;
  end
  else if AAreaName = 'float' then
  begin
    // Создать второстепенную независимую страницу приложения
  end
  else if AAreaName = 'child' then
  begin
    // Создать дочернюю модальную страницу приложения
  end;
  // Результат: созданная страница, обернутая в TUIArea
  // Result := TSampleArea.Create(AParent, AView, '', True, <page>, nil, '');
end;

destructor TSamplePresenter.Destroy;
begin
  // Очистить все структуры, созданные в результате работы
  inherited Destroy;
end;

function TSamplePresenter.DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject;
begin
  // Создать структуру для хранения картинок
  // Заполнить ее из FCommonIcons и AInteractor.Configuration.Icons, сохранив индексацию
  // Результат: созданная структура
  Result := nil;
end;

function TSamplePresenter.DoLogin(const ADomain: TObject): TInteractor;
begin
  // REQUIRED
  // Получить от пользователя логин/пароль
  // Проверить, есть ли такой пользователь и создать сессию для него
  //   vSession := Domain.Login(vLogin, vPassword);
  // Если сессия не создана, такого пользователя нет, возвращаем nil
  // Обернуть сессию в интерактор
  //   Result := TInteractor.Create(Self, vSession);
  // Создать главную страницу приложения, лейаут по умолчанию = MainForm
  //   Result.UIBuilder.Navigate(nil, '', vMainFormName, '');
  // Выполнить действие по завершению логина
  //   TLoginedProc(vDomain.Configuration.LoginedProc)(Result);

  Result := nil;
end;

procedure TSamplePresenter.DoLogout(const AInteractor: TInteractor);
begin
  // Выполнить восстановление вида системы для неавторизованного пользователя
end;

function TSamplePresenter.ShowPage(const AInteractor: TInteractor; const APageType: string;
  const AParams: TObject = nil): TDialogResult;
begin
  Result := drNone;

  // Показать страницу по её уникальному имени
  if APageType = 'about' then
  else if (APageType = 'debug') and (_Platform.DeploymentType = 'dev') then
  else if APageType = 'rtf_reports' then
  else if APageType = 'options' then
  else if APageType = 'splash' then
    // AParams -> TProgressInfo
  else begin
    // Можно унифицировать процедуру, инстанцируя страницу по зарегистрированному для неё классу
    //   vPageClass := TSamplePageClass(GetPageClass(FName, APageType));
    //   if Assigned(vPageClass) then ShowPage(vPageClass, AInteractor);
  end;
end;

function TSamplePresenter.ShowUIArea(const AInteractor: TInteractor; const AAreaName: string;
  const AOptions: string; var AArea: TUIArea): TDialogResult;
begin
  // REQUIRED
  Result := drNone;

  // Показать созданную ранее страницу пользователю
  if (AAreaName = '') or (AAreaName = 'float') then
  begin
    // Обычную страницу
  end
  else if AAreaName = 'child' then
  begin
    // Модальную страницу
  end;
end;

procedure TSamplePresenter.DoAuthorize(const AAccount: TObject; const AURL: string;
  const AWidth, AHeight: Integer; const AOnNavigated: TNavigateEvent);
begin
  // Авторизация для внешних сервисов
  // Показать веб-форму для адреса в AURL, передать обработчик для завершения авторизации
  // Пример:
  //  Presenter.Authorize(AAccount, vSignInRequest, 680, 540, OnNavigatedToToken);

  //  function OnNavigatedToToken(const AAccount: TObject; const AURL: string): Boolean;
  //  var
  //    vAccount: TEntity absolute AAccount;
  //    vRedirectUrl: string;
  //  begin
  //    Result := False;
  //    vRedirectUrl := TDomain(Domain).Constant['VKRedirectUrl'];
  //    if not StartsText(vRedirectUrl, AURL) then
  //      Exit;
  //
  //    ИЗВЛЕКАЕМ ДАННЫЕ ИЗ vRedirectUrl и записываем их в переданный объект AAccount
  //    vAccount._SetFieldValue(Holder, 'AccessToken', vExtractor['access_token']);
  //    vAccount._SetFieldEntity(Holder, 'User', vUser);
  //    vAccount._SetFieldValue(Holder, 'IsConnected', True);
  //
  //    Result := True;
  //  end;
end;

procedure TSamplePresenter.DoCloseAllPages(const AInteractor: TInteractor);
begin
  // Обработка действия "Закрыть все окна"
end;

procedure TSamplePresenter.DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False);
begin
  // Открыть файл с помощью указанной программы или с помощью программы по умолчанию
end;

procedure TSamplePresenter.DoRun(const AParameter: string);
begin
  // REQUIRED

  // Инициализировать UI
  //   Application.Title := cPlatformTitle;
  //   Application.Initialize;

  // Создать необходимые UI классы для формирования интерфейсов и обработки внешних событий

  DoOnAppStarted;

  // Организовать цикл приема/передачи сообщений
  //   Application.Run;
end;

function TSamplePresenter.DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult;
begin
  Result := drNone;
  // Показать диалог с разными вариантами кнопок
end;

procedure TSamplePresenter.DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType);
begin
  // Вывести сообщение
end;

function TSamplePresenter.DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt,
  ADefaultDir: string): Boolean;
begin
  Result := False;
  // Показать диалог открытия файла
end;

function TSamplePresenter.DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean;
begin
  Result := False;
  // Показать диалог сохранения файла
end;

procedure TSamplePresenter.DoStop;
begin
  // Экстренно завершить выполнение
  //   Application.Terminate;
end;

procedure TSamplePresenter.OnDomainError(const ACaption, AText: string);
begin
  // Что-то сделать при ошибке в домене
end;

procedure TSamplePresenter.OnDomainLoadProgress(const AProgress: Integer; const AInfo: string);
begin
  // Уведомить пользователя о прогрессе загрузки приложения
end;

procedure TSamplePresenter.SetApplicationUI(const AAppTitle, AIconName: string);
begin
  // Установить заголовок приложения и его иконку
end;

initialization

TBaseModule.RegisterModule('UI', 'Sample', TSamplePresenter);

end.
