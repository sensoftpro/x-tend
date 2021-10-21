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

unit uPlatform;

interface

uses
  uFastClasses, uSettings, uModule, uDomain, uConfiguration, uLocalizator, uTranslator, uPresenter, uEnumeration;

type
  TPlatform = class
    class procedure Run;
  private
    // Доступные в рамках платформы сконфигурированные объекты
    FConfigurations: TObjectStringDictionary<TConfiguration>;
    FDomains: TObjectStringDictionary<TDomain>;
    FPresenter: TPresenter;
    FEnumerations: TEnumerations;
    // Настройки платформы
    FSettings: TSettings;
    FLocalizator: TLocalizator;
    FTranslator: TTranslator;
    FDeploymentType: string;
    FEnvironmentID: string;
    FWereErrors: Boolean;

    procedure Stop;
    procedure Start(const AParameter: string); // запускаем процессоры для приема внешних активностей
    procedure HandleExternalData(const AData: string);
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    function CreatePresenter(const ASettings: TSettings): TPresenter;
  public
    constructor Create(const ASettings: TSettings);
    destructor Destroy; override;

    function Translate(const AKey, ADefault: string): string;
    function ConfigurationByName(const AName: string): TConfiguration;
    function DomainByUId(const AUId: string): TDomain;
    function ResolveModuleClass(const ASettings: TSettings; const AName, AType: string; out AModuleName: string): TModuleClass;
    procedure Init;

    property Domains: TObjectStringDictionary<TDomain> read FDomains;
    property Presenter: TPresenter read FPresenter;
    property Enumerations: TEnumerations read FEnumerations;
    property Localizator: TLocalizator read FLocalizator;
    property Language: string read GetLanguage write SetLanguage;

    property DeploymentType: string read FDeploymentType;
    property EnvironmentID: string read FEnvironmentID;
  end;

var
  _Platform: TPlatform;

implementation

uses
  Classes, SysUtils, IOUtils, uConsts, uCodeConfiguration, uProcessMediator, uScript, uUtils;


const
  cBrushStyleNames: array[TBrushStyle] of string = ('Сплошная', 'Нет заливки',
    'Горизонтальная', 'Вертикальная', 'Прямая диагональная', 'Обратная диагональная',
    'Перекрестная', 'Диаг. перекрестная');

  cPenStyleNames: array[TPenStyle] of string = ('', '', '', '', '', '', 'Внутри', '', '');

  cTextPositionNames: array[TTextPosition] of string = ('<по умолчанию>',
    'Внутри', 'Слева', 'Справа', 'Сверху', 'Снизу', 'По центру');

  cPeriodTypeText: array[TPeriodType] of string =
    ('За всё время', 'За этот год', 'За этот месяц', 'За эту неделю', 'За сегодня',
    'За прошлый год', 'За прошлый месяц', 'За прошлую неделю', 'За вчерашний день');

//  cEngMonths: array[0..11] of string = ('January', 'February', 'March', 'April',
//    'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December');
  cMonthNames: array[TMonth] of string = ('Январь', 'Февраль', 'Март', 'Апрель',
    'Май', 'Июнь', 'Июль', 'Август', 'Сентябрь', 'Октябрь', 'Ноябрь', 'Декабрь');

  cUserNames: array[TUserKind] of string = ('Гость', 'Система', 'Пользователь', 'Администратор');

{ TPlatform }

function TPlatform.ConfigurationByName(const AName: string): TConfiguration;
begin
  Result := FConfigurations.ObjectByName(AName);
end;

constructor TPlatform.Create(const ASettings: TSettings);
var
  vLanguage: string;

  procedure AddEnums;
  begin
    FEnumerations.AddEnum<TPeriodType>('PeriodTypes').AddDisplayNames(cPeriodTypeText);
    FEnumerations.AddEnum<TBrushStyle>('BrushStyles').AddDisplayNames(cBrushStyleNames);
    FEnumerations.AddEnum<TPenStyle>('PenStyles').AddDisplayNames(cPenStyleNames);
    FEnumerations.AddEnum<TTextPosition>('TextPositions').AddDisplayNames(cTextPositionNames);
    FEnumerations.AddEnum<TMonth>('Months').AddDisplayNames(cMonthNames);
    FEnumerations.AddEnum<TUserKind>('UserKinds').AddDisplayNames(cUserNames);
  end;
begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  inherited Create;

  Randomize;

  FWereErrors := False;
  FSettings := ASettings;
  FConfigurations := TObjectStringDictionary<TConfiguration>.Create;
  FDomains := TObjectStringDictionary<TDomain>.Create;
  FEnumerations := TEnumerations.Create;
  AddEnums;

  FLocalizator := TLocalizator.Create(TPath.Combine(GetPlatformDir + PathDelim + 'res', 'translations'), TPath.Combine(GetPlatformDir, 'settings.ini'));
  vLanguage := ASettings.GetValue('Core', 'Language', '');
  FTranslator := TTranslator.Create(FLocalizator, vLanguage);

  FDeploymentType := FSettings.GetValue('Core', 'Deployment', 'prod');
  FEnvironmentID := FSettings.GetValue('Core', 'EnvironmentID', '');
  if FEnvironmentID = '' then
  begin
    FEnvironmentID := GUIDToString(NewGUID);
    FSettings.SetValue('Core', 'EnvironmentID', FEnvironmentID);
  end;

  _Platform := Self;
end;

function TPlatform.CreatePresenter(const ASettings: TSettings): TPresenter;
var
  vPresenterClass: TPresenterClass;
  vModuleName: string;
begin
  vPresenterClass := TPresenterClass(ResolveModuleClass(ASettings, 'UI', 'UI', vModuleName));
  if Assigned(vPresenterClass) then
    Result := vPresenterClass.Create(vModuleName, ASettings)
  else
    Result := nil;
end;

destructor TPlatform.Destroy;
begin
  Stop;

  FreeAndNil(FTranslator);
  FreeAndNil(FLocalizator);
  FreeAndNil(FDomains);
  FreeAndNil(FConfigurations);

  FreeAndNil(FPresenter);

  FreeAndNil(FEnumerations);

  FreeAndNil(FSettings);

  inherited Destroy;
end;

function TPlatform.DomainByUId(const AUId: string): TDomain;
begin
  Result := FDomains.ObjectByName(AUId);
end;

function TPlatform.GetLanguage: string;
begin
  Result := FTranslator.Language;
end;

procedure TPlatform.HandleExternalData(const AData: string);
var
  vDomain: TDomain;
begin
  if AData = '' then
    Exit;

  for vDomain in FDomains do
  begin
    if False then
      Continue;
    vDomain.ExecuteDefaultAction(nil, AData);
  end;
end;

procedure TPlatform.Init;
var
  vScripts: TStrings;
  vName: string;
  vConfiguration: TConfiguration;
  vDomain: TDomain;
begin
  FWereErrors := False;

  vScripts := TScript.GetRegisteredScripts;
  try
    for vName in vScripts do
    begin
      vConfiguration := TCodeConfiguration.Create(Self, vName);
      vConfiguration.Init;
      FConfigurations.AddObject(vName, vConfiguration);

      vDomain := TDomain.Create(Self, vConfiguration, vName, FSettings);
      FDomains.AddObject(vName, vDomain);
    end;
  finally
    FreeAndNil(vScripts);
  end;

  if FDomains.Count = 1 then
  begin
    FPresenter := CreatePresenter(FDomains[0].Settings);
    if Assigned(FPresenter) then
      FPresenter.SetDomain(FDomains[0]);
  end
  else
    FPresenter := CreatePresenter(FSettings);

  for vDomain in FDomains do
  begin
    vDomain.Init;
    FWereErrors := FWereErrors or vDomain.WereErrors;
  end;
end;

function TPlatform.ResolveModuleClass(const ASettings: TSettings; const AName, AType: string;
  out AModuleName: string): TModuleClass;
var
  vSectionName: string;
  vAvailableTypes: TStrings;
begin
  Result := nil;
  AModuleName := '';

  vSectionName := 'Modules';
  if (FDeploymentType <> '') and ASettings.KeyExists(vSectionName + '$' + FDeploymentType, AName) then
    vSectionName := vSectionName + '$' + FDeploymentType;

  // Пользователь знает о нужной секции
  if ASettings.KeyExists(vSectionName, AName) then
  begin
    AModuleName := Trim(ASettings.GetValue(vSectionName, AName, ''));
    Result := TBaseModule.GetModuleClass(AType, AModuleName);
    if not Assigned(Result) and (AModuleName <> '') and Assigned(FPresenter) then
      FPresenter.ShowMessage('Ошибка загрузки модуля',
        Format('Модуль [%s] типа [%s] не зарегистрирован в системе', [AName, AType]) + #13#10#13#10
        + 'Проверьте, что файл с модулем подключен к проекту и регистрация класса модуля в нём выполнена правильно:'
        + #13#10#13#10'initialization'#13#10'  TBaseModule.RegisterModule(''' + AType + ''', '''
        + AName + ''', {Класс модуля});');
    Exit;
  end;

  // Запрос презентера
  if SameText(AType, 'UI') then
  begin
    vAvailableTypes := TBaseModule.GetModuleNamesOfType(AType);
    if vAvailableTypes.Count > 0 then
    begin
      AModuleName := vAvailableTypes[0];
      Result := TBaseModule.GetModuleClass(AType, AModuleName);
      ASettings.SetValue('Modules', AName, AModuleName);
    end;
    Exit;
  end;

  // Нет возможностей для исправления
  if not Assigned(FPresenter) then
    Exit;

  vAvailableTypes := TBaseModule.GetModuleNamesOfType(AType);
  if vAvailableTypes.Count = 0 then
  begin
    FPresenter.ShowMessage('Загрузка модуля',
      Format('В системе не обнаружено ни одного класса типа [%s]', [AType]) + #13#10#13#10
      + 'Для загрузки модуля подключите содержащий его файл к проекту и перезапустите приложение');
    AModuleName := '';
  end
  else while vAvailableTypes.Count > 0 do
  begin
    if FPresenter.ShowYesNoDialog(Format('Загрузка модуля, case: %d', [vAvailableTypes.Count]),
      Format('В системе найден класс [%s], реализующий тип [%s]', [vAvailableTypes[0], AType]) + #13#10#13#10
      + Format('Хотите использовать его для модуля [%s] того же типа?', [AName])) = drYes
    then begin
      AModuleName := vAvailableTypes[0];
      Result := TBaseModule.GetModuleClass(AType, AModuleName);
      ASettings.SetValue('Modules', AName, AModuleName);
      Break;
    end
    else
      vAvailableTypes.Delete(0);
  end;

  FreeAndNil(vAvailableTypes);
end;

class procedure TPlatform.Run;
var
  vParameter: string;
  vSettings: TSettings;
  vIsSingleton: Boolean;
  vProcessMediator: TProcessMediator;
begin
  if ParamCount > 0 then
    vParameter := ParamStr(1)
  else
    vParameter := '';

  vSettings := TIniSettings.Create(TPath.Combine(GetPlatformDir, 'settings.ini'));
  vIsSingleton := SameText(vSettings.GetValue('Core', 'RunMode', ''), 'singleton');

  vProcessMediator := TProcessMediator.Create('Sensoft.Platform');

  if vIsSingleton and vProcessMediator.AlreadyExists then
  begin
    try
      vProcessMediator.SendData(vParameter);
    finally
      FreeAndNil(vProcessMediator);
      FreeAndNil(vSettings);
    end;

    Exit;
  end;

  try
    try
      _Platform := TPlatform.Create(vSettings);
      vProcessMediator.OnDataReceived := _Platform.HandleExternalData;
    except
      on E: Exception do
      begin
        FreeAndNil(vSettings);
        Exit;
      end;
    end;

    try
      try
        _Platform.Init;
        _Platform.Start(vParameter);
      except
        on E: Exception do
          raise Exception.Create('Ошибка приложения: ' + E.Message + #13#10'Дальнейшая работа невозможна');
      end;
    finally
      FreeAndNil(_Platform);
    end;
  finally
    FreeAndNil(vProcessMediator);
  end;
end;

procedure TPlatform.SetLanguage(const Value: string);
begin
  FTranslator.Language := Value;
end;

procedure TPlatform.Start(const AParameter: string);
var
  vDomain: TDomain;
begin
  if FWereErrors then
    Exit;

  for vDomain in FDomains do
    vDomain.Run;

  if Assigned(FPresenter) then
    FPresenter.Run(AParameter);
end;

procedure TPlatform.Stop;
var
  vDomain: TDomain;
begin
  if FWereErrors then
    Exit;

  for vDomain in FDomains do
    vDomain.Stop;

  if Assigned(FPresenter) then
    FPresenter.Stop;
end;

function TPlatform.Translate(const AKey, ADefault: string): string;
begin
  Result := FTranslator.Translate(AKey, ADefault);
end;

end.
