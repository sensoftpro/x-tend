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
  uFastClasses, uSettings, uDomain, uConfiguration, uLocalizator, uTranslator;

type
  TPlatform = class
    class procedure Run;
  private
    // Доступные в рамках платформы сконфигурированные объекты
    FConfigurations: TObjectStringDictionary<TConfiguration>;
    FDomains: TObjectStringDictionary<TDomain>;
    // Настройки платформы
    FSettings: TSettings;
    FLocalizator: TLocalizator;
    FTranslator: TTranslator;
    FWereErrors: Boolean;

    procedure Stop;
    procedure Start(const AParameter: string); // запускаем процессоры для приема внешних активностей
    procedure HandleExternalData(const AData: string);
    function GetLanguage: string;
    procedure SetLanguage(const Value: string);
    function GetActiveDomain: TDomain;
  public
    constructor CreateEmbedded;
    constructor Create(const ASettings: TSettings);
    destructor Destroy; override;

    function Translate(const AKey, ADefault: string): string;
    function ConfigurationByName(const AName: string): TConfiguration;
    function DomainByUid(const AUid: string): TDomain;
    procedure Init;

    property Domains: TObjectStringDictionary<TDomain> read FDomains;
    property ActiveDomain: TDomain read GetActiveDomain;
    property Localizator: TLocalizator read FLocalizator;
    property Language: string read GetLanguage write SetLanguage;
  end;

var
  _Platform: TPlatform;

implementation

uses
  Classes, SysUtils, IOUtils, uConsts, uCodeConfiguration, uProcessMediator, uScript;

{ TPlatform }

function TPlatform.ConfigurationByName(const AName: string): TConfiguration;
begin
  Result := FConfigurations.ObjectByName(AName);
end;

constructor TPlatform.Create(const ASettings: TSettings);
var
  vLanguage: string;
begin
{$IFDEF DEBUG}
//  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}

  inherited Create;

  Randomize;

  FWereErrors := False;
  FSettings := ASettings;
  FConfigurations := TObjectStringDictionary<TConfiguration>.Create;
  FDomains := TObjectStringDictionary<TDomain>.Create;

  FLocalizator := TLocalizator.Create(TPath.Combine(GetPlatformDir + PathDelim + 'res', 'translations'), TPath.Combine(GetPlatformDir, 'settings.ini'));
  vLanguage := ASettings.GetValue('Core', 'Language', '');
  FTranslator := TTranslator.Create(FLocalizator, vLanguage);

  _Platform := Self;
end;

constructor TPlatform.CreateEmbedded;
var
  vSettings: TSettings;
begin
  vSettings := TIniSettings.Create(TPath.Combine(GetPlatformDir, 'settings.ini'));

  Create(vSettings);
  Init;
  Start('');
end;

destructor TPlatform.Destroy;
begin
  Stop;

  FreeAndNil(FTranslator);
  FreeAndNil(FLocalizator);
  FreeAndNil(FDomains);
  FreeAndNil(FConfigurations);

  FreeAndNil(FSettings);

  inherited Destroy;
end;

function TPlatform.DomainByUid(const AUid: string): TDomain;
begin
  Result := FDomains.ObjectByName(AUid);
end;

function TPlatform.GetActiveDomain: TDomain;
begin
  if FDomains.Count = 1 then
    Result := FDomains[0]
  else begin
    Result := nil;
    Assert(FDomains.Count = 1, 'Неправильное количество доменов');
  end;
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

      FWereErrors := FWereErrors or vDomain.WereErrors;
    end;
  finally
    FreeAndNil(vScripts);
  end;
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
    vDomain.Run(AParameter);
end;

procedure TPlatform.Stop;
var
  vDomain: TDomain;
begin
  if FWereErrors then
    Exit;

  for vDomain in FDomains do
    vDomain.Stop;
end;

function TPlatform.Translate(const AKey, ADefault: string): string;
begin
  Result := FTranslator.Translate(AKey, ADefault);
end;

end.
