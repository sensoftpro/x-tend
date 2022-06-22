{---------------------------------------------------------------------------------
  X-Tend configuration example

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

unit uConfiguratorScript;

interface

uses
  Classes, uScript, uCollection, uEntity, uSession, uView, uConsts, uChangeManager, uInteractor;

type
  TConfiguratorScript = class(TScript)
  protected
    procedure DoInit; override;

    procedure DoCreateDefinitions; override;
    procedure DoCreateDefaultEntities(const ADomain: TObject; const AHolder: TChangeHolder); override;
    function GetFullText(const AEntity: TEntity; var AHandled: Boolean): string; override;
    function DoCheckActionFlags(const AView: TView; const AActionName: string;
      const AContext: TObject; const AParams: TEntity): TViewState; override;
    function CheckCanChangeField(const AView: TView; const AEntity: TEntity; const AFieldName: string;
      const ANewValue: Variant; var AHandled: Boolean): Boolean; override;
    procedure DoAfterEntityCreation(const AHolder: TChangeHolder; const AOwnerContext: TObject; const AEntity: TEntity); override;
    function DoExecuteAction(const AView: TView; const AActionName: string;
      const AContext: TObject; const AParams: TEntity; const AParentHolder: TChangeHolder): Boolean; override;
  end;

implementation

uses
  Windows, Dialogs, ClipBrd, Math, SysUtils, DateUtils, Variants, XMLIntf, XMLDoc, ZIP, RegularExpressions,
  IniFiles, IOUtils, StrUtils, Generics.Collections, uConfiguration, uDefinition, uDomain, uPresenter, uUtils, uEntityList;

const
  cAdminID = 1;
  cAdminsID = 1;

procedure TConfiguratorScript.DoInit;
begin
  FAppTitle := 'Конфигуратор решений';
  FVersion := '0.1';
end;

// Вспомогательные функции

function GenerateModules(const ADomain: TDomain; const AProjectName, ATitle, AVersion: string): string;
var
  vPlatformPath: string;
  vResPath: string;
  vSrcPath: string;
  vBinPath: string;
  vProcessingFileName: string;
  vTargetFileName: string;
  vHandledText: string;
  vDBName: string;
  vGUID: TGUID;
  vFileContent: TStringStream;
  vIniFile: TIniFile;
begin
  Result := '';
  vPlatformPath := ADomain.Constant['PlatformFolder'];
  if vPlatformPath = '' then
    vPlatformPath := ExtractFilePath(ParamStr(0)) + '..\'
  else
    vPlatformPath := IncludeTrailingPathDelimiter(vPlatformPath);

  vResPath := TPath.Combine(ADomain.Configuration.ConfigurationDir, 'templates');
  if not TDirectory.Exists(vResPath) then
    Exit;

  // Подготовка директории для исходников
  vSrcPath := vPlatformPath + 'src\Solutions\' + AProjectName;
  ForceDirectories(vSrcPath);

  // Копирование иконки проекта
  vProcessingFileName := TPath.Combine(vResPath, 'favicon.ico');
  vTargetFileName := TPath.Combine(vSrcPath, 'favicon.ico');
  if TFile.Exists(vProcessingFileName) and not TFile.Exists(vTargetFileName) then
    TFile.Copy(vProcessingFileName, vTargetFileName);

  vFileContent := TStringStream.Create('', TEncoding.UTF8);
  try
    // Подготовка *.dproj файла
    vProcessingFileName := TPath.Combine(vResPath, 'NewApplication.dproj');
    if TFile.Exists(vProcessingFileName) then
    begin
      vFileContent.LoadFromFile(vProcessingFileName);
      CreateGUID(vGUID);
      vHandledText := ReplaceStr(vFileContent.DataString, '$ProjectGuid$', UpperCase(GUIDToString(vGUID)));
      vHandledText := ReplaceStr(vHandledText, '$ProjectName$', AProjectName);
      vFileContent.Size := 0;
      vFileContent.WriteString(vHandledText);
      Result := TPath.Combine(vSrcPath, AProjectName + '.dproj');
      vFileContent.SaveToFile(Result);
    end;

    // Подготовка *.dpr файла
    vProcessingFileName := TPath.Combine(vResPath, 'NewApplication.dpr');
    if TFile.Exists(vProcessingFileName) then
    begin
      vFileContent.LoadFromFile(vProcessingFileName);
      vHandledText := ReplaceStr(vFileContent.DataString, 'NewApplication', AProjectName);
      vFileContent.Size := 0;
      vFileContent.WriteString(vHandledText);
      vFileContent.SaveToFile(TPath.Combine(vSrcPath, AProjectName + '.dpr'));
    end;

    // Подготовка скрипта (*.pas файла)
    vProcessingFileName := TPath.Combine(vResPath, 'NewScript.pas');
    if TFile.Exists(vProcessingFileName) then
    begin
      vFileContent.LoadFromFile(vProcessingFileName);
      vHandledText := ReplaceStr(vFileContent.DataString, 'NewApplication', AProjectName);
      vHandledText := ReplaceStr(vHandledText, '$ProjectTitle$', ATitle);
      vHandledText := ReplaceStr(vHandledText, '$ProjectVersion$', AVersion);
      vFileContent.Size := 0;
      vFileContent.WriteString(vHandledText);
      vFileContent.SaveToFile(TPath.Combine(vSrcPath, 'u' + AProjectName + 'Script.pas'));
    end;
  finally
    FreeAndNil(vFileContent);
  end;

  // Подготовка директории для бинарников
  vBinPath := vPlatformPath + 'bin\solutions\' + LowerCase(AProjectName);
  ForceDirectories(vBinPath);

  // Копирование пустой базы данных
  vDBName := LowerCase(AProjectName) + '.mdb';
  vProcessingFileName := TPath.Combine(vResPath, 'storage.mdb');
  vTargetFileName := TPath.Combine(vBinPath, vDBName);
  if TFile.Exists(vProcessingFileName) and not TFile.Exists(vTargetFileName) then
    TFile.Copy(vProcessingFileName, vTargetFileName);

  if TDirectory.Exists(TPath.Combine(vResPath, 'layouts')) then
    TDirectory.Copy(TPath.Combine(vResPath, 'layouts'), TPath.Combine(vBinPath, 'layouts'));

  vIniFile := TIniFile.Create(TPath.Combine(vBinPath, 'settings.ini'));
  try
    if not vIniFile.ValueExists('Core', 'Deployment') then
      vIniFile.WriteString('Core', 'Deployment', 'dev');
    if not vIniFile.ValueExists('Core', 'EnvironmentID') then
      vIniFile.WriteString('Core', 'EnvironmentID', UpperCase(GUIDToString(vGUID)));
    if not vIniFile.ValueExists('Core', 'AutoLogin') then
      vIniFile.WriteString('Core', 'AutoLogin', '1');
    if not vIniFile.ValueExists('MSAccess', 'Database') then
      vIniFile.WriteString('MSAccess', 'Database', vDBName);
    if not vIniFile.ValueExists('SQLite', 'Database') then
      vIniFile.WriteString('SQLite', 'Database', LowerCase(AProjectName) + '.db');
    if not vIniFile.ValueExists('Modules', 'DataStorage') then
      vIniFile.WriteString('Modules', 'DataStorage', 'SQLite');
    if not vIniFile.ValueExists('Modules', 'ReportEngine') then
      vIniFile.WriteString('Modules', 'ReportEngine', 'FastReport');
    if not vIniFile.ValueExists('Modules', 'ChartPainter') then
      vIniFile.WriteString('Modules', 'ChartPainter', 'VCL');
    if not vIniFile.ValueExists('Modules', 'UI') then
      vIniFile.WriteString('Modules', 'UI', 'Windows.DevExpress');
    if not vIniFile.ValueExists('Modules', 'TaskEngine') then
      vIniFile.WriteString('Modules', 'TaskEngine', 'SimpleEngine');
  finally
    FreeAndNil(vIniFile);
  end;
end;

// Scheduler

function DoScheduledAction(const ADomain: TObject): Integer;
begin
  Result := 0;
end;

// Handlers

procedure OnSomethingChanged(const ASession: TUserSession; const AHolder: TChangeHolder;
  const AFieldChain: string; const AEntity, AParam: TEntity);
begin
end;

{ TConfiguratorScript }

function TConfiguratorScript.CheckCanChangeField(const AView: TView; const AEntity: TEntity;
  const AFieldName: string; const ANewValue: Variant; var AHandled: Boolean): Boolean;
begin
  Result := True;
end;

procedure TConfiguratorScript.DoAfterEntityCreation(const AHolder: TChangeHolder;
  const AOwnerContext: TObject; const AEntity: TEntity);
begin
  // Здесь мы в контексте редактирования
  if AEntity.InstanceOf('') then
  begin
  end
  else
    inherited DoAfterEntityCreation(AHolder, AOwnerContext, AEntity);
end;

function TConfiguratorScript.DoCheckActionFlags(const AView: TView; const AActionName: string;
  const AContext: TObject; const AParams: TEntity): TViewState;
begin
  Result := inherited DoCheckActionFlags(AView, AActionName, AContext, AParams);
  if Result <> vsUndefined then
    Exit;
end;

procedure TConfiguratorScript.DoCreateDefaultEntities(const ADomain: TObject; const AHolder: TChangeHolder);
var
  vDomain: TDomain absolute ADomain;
  vCollection: TCollection;
begin
  inherited DoCreateDefaultEntities(ADomain, AHolder);

  vCollection := vDomain['SysUsers'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'Login;Name;!Password',
    ['admin', 'Администратор', Md5Hash('')], True);

  vCollection := vDomain['SysUsersRoles'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'User;Role;Flags', [
    Integer(vDomain.EntityByID('SysUsers', 1)), Integer(vDomain.EntityByID('SysRoles', 1))], True);

  vCollection := vDomain['SysConstants'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'Code;MailLogin;MailPassword', ['debug', 'noname@mail.ru', '']);
end;

procedure TConfiguratorScript.DoCreateDefinitions;
var
  vDefinition: TDefinition;
begin
  vDefinition := DefinitionByName('SysConstants');
  vDefinition.AddSimpleFieldDef('PlatformFolder', 'platform_folder', 'Корневая папка платформы', Null, Null, 255, fkString, 'dir');

  vDefinition := AddDefinition('_Configurations', '', 'Конфигурации', cNullItemName, ccHideInMenu, clkMixin);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Caption', 'caption', 'Заголовок', Null, Null, 80, fkString, '', '', vsFullAccess, cRequired or cLocalizable);
  vDefinition.AddSimpleFieldDef('Version', 'version', 'Версия', '0.1', Null, 15, fkString, '', '', vsFullAccess, cRequired);

  vDefinition := AddDefinition('Configurations', '_Configurations', 'Конфигурации', cNullItemName, 0).SetImageID(9);
  vDefinition.AddAction('MakeSolution', 'Создать приложение', 0);
end;

function TConfiguratorScript.DoExecuteAction(const AView: TView; const AActionName: string;
  const AContext: TObject; const AParams: TEntity; const AParentHolder: TChangeHolder): Boolean;
var
  vEntity: TEntity absolute AContext;
  vEntityList: TEntityList absolute AContext;
  vInteractor: TInteractor;
  vDomain: TDomain;
  vProjFileName: string;
begin
  Result := inherited DoExecuteAction(AView, AActionName, AContext, AParams, AParentHolder);
  if Result then
    Exit;

  vInteractor := TInteractor(AView.Interactor);
  vDomain := TDomain(vInteractor.Domain);

  // Обработка общих действий (конфигурация)
  if AActionName = cUnknownName then
  begin
    // Handle it
    Exit(True);
  end
  else if not Assigned(AContext) then
    Exit(False);

  // Обработка действий над списками
  if AContext is TEntityList then
  begin
    if (AActionName = cUnknownName) and vEntityList.MainDefinition.IsDescendantOf(cUnknownName) then
    begin
      // Handle it
      Exit(True);
    end;
    Exit(False);
  end;

  // Обработка действий над сущностями
  if vEntity.InstanceOf('Configurations') then
  begin
    if AActionName = 'MakeSolution' then
    begin
      vProjFileName := GenerateModules(vDomain, vEntity['Name'], vEntity['Caption'], vEntity['Version']);

      if Assigned(vInteractor) and Assigned(vInteractor.Presenter) then
      begin
        vInteractor.ShowMessage('Генерация выполнена успешно');
        TPresenter(vInteractor.Presenter).OpenFile(vProjFileName);
      end;
      Exit(True);
    end;
  end;

  Result := False;
end;

function TConfiguratorScript.GetFullText(const AEntity: TEntity; var AHandled: Boolean): string;
var
  vCollectionName: string;
begin
  vCollectionName := AEntity.CollectionName;
  if vCollectionName = cUnknownName then
    Result := ''
  else
    Result := inherited GetFullText(AEntity, AHandled);
end;

initialization

RegisterScript('Configurator', TConfiguratorScript);

end.
