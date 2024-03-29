﻿{---------------------------------------------------------------------------------
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
    FProcessMediator: TObject;
    // Настройки платформы
    FSettings: TSettings;
    FLocalizator: TLocalizator;
    FTranslator: TTranslator;
    FDeploymentType: string;
    FEnvironmentID: string;
    FWereErrors: Boolean;

    procedure Start;
    procedure Stop;
    procedure HandleExternalData(const AData: string);
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    function CreatePresenter(const ASettings: TSettings): TPresenter;
  public
    constructor Create;
    destructor Destroy; override;

    function Translate(const AKey, ADefault: string): string;
    function ConfigurationByName(const AName: string): TConfiguration;
    function DomainByUId(const AUId: string): TDomain;
    function ResolveModuleName(const ASettings: TSettings; const AName: string): string;
    function ResolveModuleInfo(const ASettings: TSettings; const AName, AType: string): TModuleInfo;

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
{$IF DEFINED(MSWINDOWS)}
  WinApi.Windows,
{$ELSEIF DEFINED(POSIX)}
  Posix.Unistd,
{$ENDIF}
  Classes, SysUtils, StrUtils, IOUtils, SyncObjs, Threading, uConsts, uCodeConfiguration, uScript, uUtils;

const
  cUniqueAppName = 'Sensoft.Platform';
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

type
  TOnDataReceivedEvent = procedure(const AData: string) of object;

  TProcessMediator = class
  private
    FFileName: string;
    FEventName: string;
    FAlreadyExists: Boolean;
  {$IF DEFINED(MSWINDOWS)}
    FFileMapping: THandle;
  {$ELSEIF DEFINED(POSIX)}
    //FLockFile: TextFile;
  {$ENDIF}
    FBaseAddress: Pointer;
    FEvent: TEvent;
    FWaitingTask: ITask;
    FOnDataReceived: TOnDataReceivedEvent;
  public
    constructor Create(const AUniqueName: string; const ADataSize: Integer = 255);
    destructor Destroy; override;

    function SendData(const AData: string): Boolean;

    property AlreadyExists: Boolean read FAlreadyExists;
    property OnDataReceived: TOnDataReceivedEvent read FOnDataReceived write FOnDataReceived;
  end;

{ TPlatform }

function TPlatform.ConfigurationByName(const AName: string): TConfiguration;
begin
  Result := FConfigurations.ObjectByName(AName);
end;

constructor TPlatform.Create;
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

  FSettings := TIniSettings.Create(TPath.Combine(GetPlatformDir, 'settings.ini'));
  FConfigurations := TObjectStringDictionary<TConfiguration>.Create;
  FDomains := TObjectStringDictionary<TDomain>.Create;
  FEnumerations := TEnumerations.Create;
  AddEnums;

  FPresenter := CreatePresenter(FSettings);

  FLocalizator := TLocalizator.Create(TPath.Combine(GetPlatformDir, 'translations'), TPath.Combine(GetPlatformDir, 'settings.ini'));
  vLanguage := FSettings.GetValue('Core', 'Language', '');
  FTranslator := TTranslator.Create(FLocalizator, vLanguage);
  FProcessMediator := TProcessMediator.Create(cUniqueAppName);
  TProcessMediator(FProcessMediator).OnDataReceived := HandleExternalData;

  _Platform := Self;
end;

function TPlatform.CreatePresenter(const ASettings: TSettings): TPresenter;
var
  vPresenterInfo: TModuleInfo;
begin
  vPresenterInfo := ResolveModuleInfo(ASettings, 'UI', 'UI');
  if Assigned(vPresenterInfo) then
    Result := TPresenterClass(vPresenterInfo.ModuleClass).Create(vPresenterInfo.Name, ASettings)
  else
    Result := nil;

  FDeploymentType := ASettings.GetValue('Core', 'Deployment', 'prod');
  FEnvironmentID := ASettings.GetValue('Core', 'EnvironmentID', '');
  if FEnvironmentID = '' then
  begin
    FEnvironmentID := GUIDToString(NewGUID);
    ASettings.SetValue('Core', 'EnvironmentID', FEnvironmentID);
  end;
end;

destructor TPlatform.Destroy;
begin
  Stop;

  FreeAndNil(FProcessMediator);
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
    vDomain.ExecuteDefaultAction(vDomain.DomainSession, AData);
  end;
end;

procedure TPlatform.Start;
var
  vScripts: TStrings;
  vName: string;
  vConfigurationDir: string;
  vConfiguration: TConfiguration;
  vDomain: TDomain;
begin
  FWereErrors := False;
  vConfigurationDir := '';

  vScripts := TScript.GetRegisteredScripts;
  try
    for vName in vScripts do
    begin
      vConfiguration := TCodeConfiguration.Create(Self, vName);
      vConfiguration.Init;
      if vConfigurationDir = '' then
        vConfigurationDir := vConfiguration.ConfigurationDir;
      FConfigurations.AddObject(vName, vConfiguration);
    end;
  finally
    FreeAndNil(vScripts);
  end;

  for vConfiguration in FConfigurations do
  begin
    vDomain := TDomain.Create(Self, vConfiguration, vName, FSettings);
    FDomains.AddObject(vName, vDomain);
  end;

  for vDomain in FDomains do
  begin
    vDomain.Init;
    FWereErrors := FWereErrors or vDomain.WereErrors;
  end;

  if FWereErrors then
    Exit;

  for vDomain in FDomains do
    vDomain.Run;

  vDomain := _Platform.Domains[0];

  FPresenter.SetApplicationUI(vDomain.AppTitle, vDomain.Configuration.IconFileName);

  vDomain.NotifyLoadingProgress(100);
end;

function TPlatform.ResolveModuleInfo(const ASettings: TSettings; const AName, AType: string): TModuleInfo;
var
  vModuleName: string;
  vAvailableTypes: TStrings;
begin
  Result := nil;
  vModuleName := ResolveModuleName(ASettings, AName);

  // Пользователь знает о нужном модуле
  if vModuleName <> '' then
    Result := TBaseModule.GetModuleInfo(AType, vModuleName);

  if not Assigned(Result) then
  begin
    vAvailableTypes := TBaseModule.GetModuleNamesOfType(AType);
    try
      if vAvailableTypes.Count > 0 then
      begin
        vModuleName := vAvailableTypes[0];
        Result := TBaseModule.GetModuleInfo(AType, vModuleName);
        ASettings.SetValue('Modules', AName, vModuleName);
      end
      else if Assigned(FPresenter) then
      begin
        if vModuleName <> '' then
          FPresenter.ShowMessage('Ошибка загрузки модуля',
            Format('Модуль [%s] типа [%s] не зарегистрирован в системе', [AName, AType]) + #13#10#13#10
            + 'Проверьте, что файл с модулем подключен к проекту и регистрация класса модуля в нём выполнена правильно:'
            + #13#10#13#10'initialization'#13#10'  TBaseModule.RegisterModule(''' + AType + ''', '''
            + AName + ''', {Класс модуля});')
        else
          FPresenter.ShowMessage('Ошибка загрузки модуля',
            Format('В системе не обнаружено ни одного класса модуля типа [%s]', [AType]) + #13#10#13#10
            + 'Для загрузки модуля подключите к проекту содержащий его файл и перезапустите приложение');
      end;
    finally
      FreeAndNil(vAvailableTypes);
    end;
  end;
end;

function TPlatform.ResolveModuleName(const ASettings: TSettings; const AName: string): string;
var
  vSectionName: string;
begin
  vSectionName := 'Modules';
  if (FDeploymentType <> '') and ASettings.KeyExists(vSectionName + '$' + FDeploymentType, AName) then
    vSectionName := vSectionName + '$' + FDeploymentType;

  // Пользователь знает о нужной секции
  if ASettings.KeyExists(vSectionName, AName) then
    Result := Trim(ASettings.GetValue(vSectionName, AName, ''))
  else
    Result := '';
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
  try
    vIsSingleton := SameText(vSettings.GetValue('Core', 'RunMode', ''), 'singleton');
    if vSettings.KeyExists('Core', 'DevMode') then
      DeveloperMode := StrToIntDef(vSettings.GetValue('Core', 'DevMode', ''), 0) <> 0
    else
      DeveloperMode := True;
  finally
    FreeAndNil(vSettings);
  end;

  if vIsSingleton then
  begin
    vProcessMediator := TProcessMediator.Create(cUniqueAppName);
    try
      if vProcessMediator.AlreadyExists then
      begin
        vProcessMediator.SendData(vParameter);
        Exit;
      end;
    finally
      FreeAndNil(vProcessMediator);
    end;
  end;

  _Platform := TPlatform.Create;
  try
    _Platform.Start;

    // Deferred execution of an application initialization
    // !!! Doesn't work in UniGUI
    if SameText(_Platform.Presenter.Name, 'FMX') then
    begin
      TThread.CreateAnonymousThread(procedure
        begin
          TThread.Queue(nil, procedure
          begin
            _Platform.Presenter.Login(_Platform.Domains[0]);
          end);
        end).Start;
    end
    else if StartsText('Windows', _Platform.Presenter.Name) then
    begin
      _Platform.Presenter.Login(_Platform.Domains[0]);
    end;

    _Platform.Presenter.Run(vParameter);
  finally
    _Platform.Free; // в деструкторе ещё может быть использована переменная _Platform
    _Platform := nil;
  end;
end;

procedure TPlatform.SetLanguage(const Value: string);
begin
  FTranslator.Language := Value;
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

{ TProcessMediator }

constructor TProcessMediator.Create(const AUniqueName: string; const ADataSize: Integer);
begin
  inherited Create;

  FFileName := AUniqueName + '$File';
  FEventName := AUniqueName + '$Event';

{$IF DEFINED(MSWINDOWS)}
  FFileMapping := OpenFileMapping(FILE_MAP_WRITE or FILE_MAP_READ, False, PChar(FFileName));
  FAlreadyExists := FFileMapping <> 0;

  if not FAlreadyExists then
  begin
    FFileMapping := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, ADataSize, PChar(FFileName));
    if FFileMapping = 0 then
      FBaseAddress := nil
    else
      FBaseAddress := MapViewOfFile(FFileMapping, FILE_MAP_WRITE or FILE_MAP_READ, 0, 0, 0);

    FEvent := TEvent.Create(nil, False, False, FEventName);
    FWaitingTask := TTask.Run(procedure
      var
        vWaitResult: TWaitResult;
      begin
        while TTask.CurrentTask.Status = TTaskStatus.Running do
        begin
          vWaitResult := FEvent.WaitFor;
          if (vWaitResult = wrSignaled) and (TTask.CurrentTask.Status = TTaskStatus.Running) then
          begin
            if Assigned(FOnDataReceived) and Assigned(FBaseAddress) then
              TThread.Synchronize(nil, procedure
                begin
                  FOnDataReceived(PChar(FBaseAddress));
                end);
          end
          else
            Break;
        end;
      end);
  end
  else begin
    FBaseAddress := MapViewOfFile(FFileMapping, FILE_MAP_WRITE or FILE_MAP_READ, 0, 0, 0);
    FEvent := nil;
    FWaitingTask := nil;
  end;
{$ELSEIF DEFINED(POSIX)}
  FFileName := TPath.Combine(TPath.GetTempPath, 'Sensoft.Platform.lock');
  FAlreadyExists := TFile.Exists(FFileName);

  if not FAlreadyExists then
  begin
    FBaseAddress := nil;
    //AssignFile(FLockFile, '/tmp/Sensoft.Platform.lock');
    //Rewrite(FLockFile);
    //CloseFile(FLockFile);
  end
  else begin
    FBaseAddress := nil;
    FEvent := nil;
    FWaitingTask := nil;
  end;
{$ENDIF}
end;

destructor TProcessMediator.Destroy;
begin
  if Assigned(FWaitingTask) then
  begin
    FWaitingTask.Cancel;
    FWaitingTask := nil;
  end;

  if Assigned(FEvent) then
    FEvent.SetEvent;
  FreeAndNil(FEvent);

{$IF DEFINED(MSWINDOWS)}
  if Assigned(FBaseAddress) then
    UnmapViewOfFile(FBaseAddress);
  if FFileMapping <> 0 then
    CloseHandle(FFileMapping);
{$ELSEIF DEFINED(POSIX)}
  if FileExists('/tmp/Sensoft.Platform.lock') then
    DeleteFile('/tmp/Sensoft.Platform.lock');
{$ENDIF}

  inherited Destroy;
end;

function TProcessMediator.SendData(const AData: string): Boolean;
begin
  if not FAlreadyExists then
    Exit(False);
  FEvent := TEvent.Create(nil, False, False, FEventName);
  Result := Assigned(FBaseAddress) and Assigned(FEvent);
  if Result then
  begin
{$IFDEF MSWINDOWS}
    StrPCopy(FBaseAddress, PChar(AData));
{$ENDIF}
    FEvent.SetEvent;
  end;
end;

end.
