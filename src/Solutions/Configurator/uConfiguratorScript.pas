{---------------------------------------------------------------------------------
  X-Tend configuration example

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
  Math, SysUtils, DateUtils, Variants, XMLIntf, XMLDoc, ZIP, RegularExpressions,
  IniFiles, IOUtils, StrUtils, Generics.Collections,
  uConfiguration, uDefinition, uDomain, uPresenter, uUtils, uEntityList, uObjectField;

type
  TTestingState = (tsReady, tsRunning, tsPaused, tsCompleted);

const
  cAdminID = 1;
  cAdminsID = 1;
  cTestingStateNames: array[TTestingState] of string = ('Готов', 'В процессе', 'Приостановлен', 'Завершён');

procedure TConfiguratorScript.DoInit;
begin
  FAppTitle := 'Конфигуратор решений';
  FVersion := '0.1';
end;

// Вспомогательные функции
const
  cProjectGroupItem =
    '        <Projects Include="$FullProjectName$.dproj">'#13#10 +
    '            <Dependencies/>'#13#10 +
    '        </Projects>'#13#10;
  cProjectGroupTarget =
    '    <Target Name="$FullProjectName$">'#13#10 +
    '        <MSBuild Projects="$FullProjectName$.dproj"/>'#13#10 +
    '    </Target>'#13#10 +
    '    <Target Name="$FullProjectName$:Clean">'#13#10 +
    '        <MSBuild Projects="$FullProjectName$.dproj" Targets="Clean"/>'#13#10 +
    '    </Target>'#13#10 +
    '    <Target Name="$FullProjectName$:Make">'#13#10 +
    '        <MSBuild Projects="$FullProjectName$.dproj" Targets="Make"/>'#13#10 +
    '    </Target>'#13#10;
  cDeployVCLProjectItem =
    '<DeployFile LocalName="$SrcPath$$ProjectName$.exe" Configuration="$CfgName$" Class="ProjectOutput"/>'#13#10;
  cDeployFMXProjectItem =
    '<DeployFile LocalName="$SrcPath$_$PlatformName$\$SrcResourceName$" Configuration="$CfgName$" Class="$ResourceClass$">'#13#10  +
    '    <Platform Name="$PlatformName$">'#13#10 +
    '        <RemoteName>$DestResourceName$</RemoteName>'#13#10 +
    '        <Overwrite>true</Overwrite>'#13#10 +
    '    </Platform>'#13#10 +
    '</DeployFile>'#13#10;
  cDeployFileItem =
    '<DeployFile LocalName="$SrcPath$$FileName$" Configuration="$CfgName$" Class="File">'#13#10 +
    '    <Platform Name="$PlatformName$">'#13#10 +
    '        <RemoteDir>$DestPath$</RemoteDir>'#13#10 +
    '        <RemoteName>$FileName$</RemoteName>'#13#10 +
    '        <Overwrite>true</Overwrite>'#13#10 +
    '    </Platform>'#13#10 +
    '</DeployFile>'#13#10 ;

function GenerateModules(const ADomain: TDomain; const AAppSettings: TEntity): string;
var
  vProjectName: string;
  vTitle, vVersion, vPackageName: string;
  vPlatformPath, vPlatformBinPath: string;
  vTemplatesPath: string;
  vSrcPath: string;
  vBinPath: string;
  vProcessingFileName: string;
  vTargetFileName: string;
  vHandledText: string;
  vGUID: TGUID;
  vFileContent: TStringStream;
  vIniFile: TIniFile;
  vFrameworks: TListField;
  vFrameworkRel, vFramework: TEntity;
  vProjectGroupItemsText: string;
  vProjectGroupTargetsText: string;
  vBuildTargets, vCleanTargets, vMakeTargets: string;
  vDeploymentResources: string;
  vModules: TStrings;
  vModuleName, vModulePath: string;
  vSolutionPath: string;

  procedure AddToProjectGroup(const AFullProjectName: string);
  var
    vGroupItem: string;
    vGroupTarget: string;
  begin
    vGroupItem := ReplaceStr(cProjectGroupItem, '$FullProjectName$', AFullProjectName);
    vProjectGroupItemsText := vProjectGroupItemsText + vGroupItem;

    vGroupTarget := ReplaceStr(cProjectGroupTarget, '$FullProjectName$', AFullProjectName);
    vProjectGroupTargetsText := vProjectGroupTargetsText + vGroupTarget;

    if vBuildTargets <> '' then
    begin
      vBuildTargets := vBuildTargets + ';';
      vCleanTargets := vCleanTargets + ';';
      vMakeTargets := vMakeTargets + ';';
    end;
    vBuildTargets := vBuildTargets + AFullProjectName;
    vCleanTargets := vCleanTargets + AFullProjectName + ':Clean';
    vMakeTargets := vMakeTargets + AFullProjectName + ':Make';
  end;

  procedure AddFileResource(const AName, ARelativePath, ASrcPath, ADestPath, APlatformName, ACfgName: string);
  var
    vResourceItem: string;
  begin
    vResourceItem := cDeployFileItem;
    vResourceItem := ReplaceStr(vResourceItem, '$SrcPath$', ASrcPath + ARelativePath);
    vResourceItem := ReplaceStr(vResourceItem, '$DestPath$', ADestPath + ARelativePath);
    vResourceItem := ReplaceStr(vResourceItem, '$FileName$', AName);
    vResourceItem := ReplaceStr(vResourceItem, '$CfgName$', ACfgName);
    vResourceItem := ReplaceStr(vResourceItem, '$PlatformName$', APlatformName);

    vDeploymentResources := vDeploymentResources + vResourceItem;
  end;


  procedure AddFileResources(const AName, ARelativePath, ASrcPath: string);
  begin
    AddFileResource(AName, ARelativePath, ASrcPath, '.\', 'Linux64', 'Debug');
    AddFileResource(AName, ARelativePath, ASrcPath, 'Contents\Resources\', 'OSXARM64', 'Debug');
    //AddFileResource(AName, ASrcPath, ADestPath, 'Linux64', 'Release');
    //AddFileResource(AName, ASrcPath, ADestPath, 'OSXARM64', 'Release');
  end;

  procedure AddResources(const ABasePath, ASubDirName, ASrcPath, AExtension: string);
  var
    vSearchPath: string;
    vFullBasePath: string;
    vFullBasePathLength: Integer;
    vFullFileName, vFileName: string;
    vRelativeFilePath: string;
  begin
    vSearchPath := TPath.Combine(ABasePath, ASubDirName);
    vFullBasePath := IncludeTrailingPathDelimiter(TPath.GetFullPath(ABasePath));
    vFullBasePathLength := Length(vFullBasePath);
    if TDirectory.Exists(vSearchPath) then
    begin
      for vFullFileName in TDirectory.GetFiles(vSearchPath, AExtension, TSearchOption.soAllDirectories) do
      begin
        vFileName := ExtractFileName(vFullFileName);
        vRelativeFilePath := ExtractFilePath(vFullFileName);
        Assert(StartsText(vFullBasePath, vRelativeFilePath), 'Несоответствие путей');
        Delete(vRelativeFilePath, 1, vFullBasePathLength);

        AddFileResources(vFileName, vRelativeFilePath, ASrcPath);
      end;
    end;
  end;

  procedure AddPlatformResources(const ABasePath, ASubDirName, ASrcPath, ADestPath, AExtension, APlatformName, ACfgName: string);
  var
    vSearchPath: string;
    vFullBasePath: string;
    vFullBasePathLength: Integer;
    vFullFileName, vFileName: string;
    vRelativeFilePath: string;
  begin
    vSearchPath := TPath.Combine(ABasePath, ASubDirName);
    vFullBasePath := IncludeTrailingPathDelimiter(TPath.GetFullPath(ABasePath));
    vFullBasePathLength := Length(vFullBasePath);
    if TDirectory.Exists(vSearchPath) then
    begin
      for vFullFileName in TDirectory.GetFiles(vSearchPath, AExtension, TSearchOption.soAllDirectories) do
      begin
        vFileName := ExtractFileName(vFullFileName);
        vRelativeFilePath := ExtractFilePath(vFullFileName);
        Assert(StartsText(vFullBasePath, vRelativeFilePath), 'Несоответствие путей');
        Delete(vRelativeFilePath, 1, vFullBasePathLength);

        AddFileResource(vFileName, vRelativeFilePath, ASrcPath, ADestPath, APlatformName, ACfgName);
      end;
    end;
  end;

  procedure AddProjectResource(const AFrameworkName, ASrcPath, ACfgName: string);
  var
    vFullProjectName: string;

    procedure InternalAddResource(const ATemplate, ASrcResName, ADestResName, AResClass, APlatformName: string);
    var
      vResourceItem: string;
    begin
      vResourceItem := ATemplate;
      vResourceItem := ReplaceStr(vResourceItem, '$SrcPath$', ASrcPath);
      vResourceItem := ReplaceStr(vResourceItem, '$SrcResourceName$', ASrcResName);
      vResourceItem := ReplaceStr(vResourceItem, '$DestResourceName$', ADestResName);
      vResourceItem := ReplaceStr(vResourceItem, '$ResourceClass$', AResClass);
      vResourceItem := ReplaceStr(vResourceItem, '$CfgName$', ACfgName);
      vResourceItem := ReplaceStr(vResourceItem, '$PlatformName$', APlatformName);

      vDeploymentResources := vDeploymentResources + vResourceItem;
    end;

  begin
    if AFrameworkName <> '' then
      vFullProjectName := vProjectName + '_' + AFrameworkName
    else
      vFullProjectName := vProjectName;

    if (AFrameworkName = 'FMX') then
    begin
      InternalAddResource(cDeployVCLProjectItem, vFullProjectName, vFullProjectName, 'ProjectOutput', 'Win32');
      InternalAddResource(cDeployVCLProjectItem, vFullProjectName, vFullProjectName, 'ProjectOutput', 'Win64');

      InternalAddResource(cDeployFMXProjectItem, vFullProjectName, vFullProjectName, 'ProjectOutput', 'Linux64');
      AddPlatformResources(TPath.Combine(vPlatformBinPath, '_Linux64'), '', '..\..\..\bin\_Linux64\', '.\', '*.so', 'Linux64', 'Debug');

      InternalAddResource(cDeployFMXProjectItem, vFullProjectName, vFullProjectName, 'ProjectOutput', 'OSXARM64');
      AddPlatformResources(TPath.Combine(vPlatformBinPath, '_OSXARM64'), '', '..\..\..\bin\_OSXARM64\', '.\', '*.dylib', 'OSXARM64', 'Debug');
      InternalAddResource(cDeployFMXProjectItem, vFullProjectName + '.entitlements', vFullProjectName + '.entitlements', 'ProjectOSXEntitlements', 'OSXARM64');
      InternalAddResource(cDeployFMXProjectItem, vFullProjectName + '.info.plist', 'Info.plist', 'ProjectOSXInfoPList', 'OSXARM64');
      InternalAddResource(cDeployFMXProjectItem, 'favicon.icns', vFullProjectName + '.icns', 'ProjectOSXResource', 'OSXARM64');
      if ACfgName = 'Debug' then
        InternalAddResource(cDeployFMXProjectItem, vFullProjectName + '.dSYM', vFullProjectName, 'ProjectOSXDebug', 'OSXARM64');
                  {<DeployFile LocalName="..\..\..\bin\favicon.icns" Configuration="Debug" Class="ProjectOSXResource">
                      <Platform Name="OSXARM64">
                          <RemoteName>Configurator.icns</RemoteName>
                          <Overwrite>true</Overwrite>
                      </Platform>
                  </DeployFile>

                  <DeployFile LocalName="favicon.icns" Configuration="Debug" Class="ProjectOSXResource">
                      <Platform Name="OSXARM64">
                          <RemoteName>Configurator_FMX.icns</RemoteName>
                          <Overwrite>true</Overwrite>
                      </Platform>
                  </DeployFile> }
    end
    else {if AFrameworkName = 'VCL' then}
    begin
      InternalAddResource(cDeployVCLProjectItem, vFullProjectName, vFullProjectName, 'ProjectOutput', 'Win32');
      InternalAddResource(cDeployVCLProjectItem, vFullProjectName, vFullProjectName, 'ProjectOutput', 'Win64');
    end;
  end;

  procedure CopyProjectFiles(const AFrameworkName: string);
  var
    vFileContent: TStringStream;
    vFrameworkSrcPath: string;
    vProcessingFileName: string;
    vFullProjectName: string;
  begin
    vFrameworkSrcPath := TPath.Combine(vTemplatesPath, AFrameworkName);
    if not TDirectory.Exists(vFrameworkSrcPath) then
      Exit;

    if AFrameworkName <> '' then
      vFullProjectName := vProjectName + '_' + AFrameworkName
    else
      vFullProjectName := vProjectName;

    AddProjectResource(AFrameworkName, '..\..\..\bin\', 'Debug');

    vFileContent := TStringStream.Create('', TEncoding.UTF8);
    try
      // Подготовка *.dproj файла
      vProcessingFileName := TPath.Combine(vFrameworkSrcPath, 'FullProjectName.dproj');
      if TFile.Exists(vProcessingFileName) then
      begin
        vFileContent.LoadFromFile(vProcessingFileName);
        CreateGUID(vGUID);
        vHandledText := ReplaceStr(vFileContent.DataString, '$ProjectGuid$', UpperCase(GUIDToString(vGUID)));
        vHandledText := ReplaceStr(vHandledText, '$FullProjectName$', vFullProjectName);
        vHandledText := ReplaceStr(vHandledText, '$ProjectName$', vProjectName);
        vHandledText := ReplaceStr(vHandledText, '$PackageName$', vPackageName);
        vHandledText := ReplaceStr(vHandledText, '$VerboseName$', vTitle);
        vHandledText := ReplaceStr(vHandledText, '$DeploymentResources$', vDeploymentResources);
        vFileContent.Size := 0;
        vFileContent.WriteString(vHandledText);
        vFileContent.SaveToFile(TPath.Combine(vSrcPath, vFullProjectName + '.dproj'));
      end;

      // Подготовка *.dpr файла
      vProcessingFileName := TPath.Combine(vFrameworkSrcPath, 'FullProjectName.dpr');
      if TFile.Exists(vProcessingFileName) then
      begin
        vFileContent.LoadFromFile(vProcessingFileName);
        vHandledText := ReplaceStr(vFileContent.DataString, 'FullProjectName', vFullProjectName);
        vHandledText := ReplaceStr(vHandledText, 'ProjectName', vProjectName);
        vFileContent.Size := 0;
        vFileContent.WriteString(vHandledText);
        vFileContent.SaveToFile(TPath.Combine(vSrcPath, vFullProjectName + '.dpr'));
      end;

      AddToProjectGroup(vFullProjectName);
    finally
      FreeAndNil(vFileContent);
    end;
  end;
begin
  Result := '';

  vPlatformPath := ADomain.Constant['PlatformFolder'];
  if vPlatformPath = '' then
    vPlatformPath := GetParentPath(GetPlatformDir);
  vPlatformBinPath := TPath.Combine(vPlatformPath, 'bin');

  vTemplatesPath := TPath.Combine(ADomain.Configuration.ConfigurationDir, 'templates');
  if not TDirectory.Exists(vTemplatesPath) then
    Exit;

  vProjectName := AAppSettings['ProjectName'];
  vTitle := AAppSettings['Caption'];
  vPackageName := AAppSettings['PackageName'];
  vVersion := AAppSettings['Version'];
  vProjectGroupItemsText := '';
  vProjectGroupTargetsText := '';
  vBuildTargets := '';
  vCleanTargets := '';
  vMakeTargets := '';
  vDeploymentResources := '';

  // Подготовка директории для исходников
  vSrcPath := TPath.Combine(vPlatformPath, 'src' + PathDelim + 'Solutions' + PathDelim + vProjectName);
  ForceDirectories(vSrcPath);

  // Подготовка директории для ресурсов
  vBinPath := TPath.Combine(vPlatformBinPath, 'solutions' + PathDelim + vProjectName.ToLowerInvariant);
  ForceDirectories(vBinPath);

  // Копирование иконки проекта в исходники и в ресурсы к бинарникам
  vProcessingFileName := TPath.Combine(vTemplatesPath, 'favicon.ico');
  vTargetFileName := TPath.Combine(vSrcPath, 'favicon.ico');
  if TFile.Exists(vProcessingFileName) and not TFile.Exists(vTargetFileName) then
    TFile.Copy(vProcessingFileName, vTargetFileName);
  vTargetFileName := TPath.Combine(vBinPath, 'favicon.ico');
  if TFile.Exists(vProcessingFileName) and not TFile.Exists(vTargetFileName) then
    TFile.Copy(vProcessingFileName, vTargetFileName);

  vProcessingFileName := TPath.Combine(vTemplatesPath, 'favicon.icns');
  vTargetFileName := TPath.Combine(vSrcPath, 'favicon.icns');
  if TFile.Exists(vProcessingFileName) and not TFile.Exists(vTargetFileName) then
    TFile.Copy(vProcessingFileName, vTargetFileName);
  vTargetFileName := TPath.Combine(vBinPath, 'favicon.icns');
  if TFile.Exists(vProcessingFileName) and not TFile.Exists(vTargetFileName) then
    TFile.Copy(vProcessingFileName, vTargetFileName);

  if TDirectory.Exists(TPath.Combine(vTemplatesPath, 'layouts')) then
    TDirectory.Copy(TPath.Combine(vTemplatesPath, 'layouts'), TPath.Combine(vBinPath, 'layouts'));

  vIniFile := TIniFile.Create(TPath.Combine(vBinPath, 'settings.ini'));
  try
    if not vIniFile.ValueExists('Core', 'Deployment') then
      vIniFile.WriteString('Core', 'Deployment', 'dev');
    if not vIniFile.ValueExists('Core', 'EnvironmentID') then
      vIniFile.WriteString('Core', 'EnvironmentID', UpperCase(GUIDToString(vGUID)));
    if not vIniFile.ValueExists('Core', 'AutoLogin') then
      vIniFile.WriteString('Core', 'AutoLogin', '1');
    if not vIniFile.ValueExists('SQLite', 'Database') then
      vIniFile.WriteString('SQLite', 'Database', LowerCase(vProjectName) + '.db');
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

  // Создание списка файлов для развертывания
  AddResources(vPlatformBinPath, 'icons', '..\..\..\bin\', '*.png');
  AddResources(vPlatformBinPath, 'images', '..\..\..\bin\', '*.png');
  AddResources(vPlatformBinPath, 'layouts', '..\..\..\bin\', '*.xtf');

  vModules := TStringList.Create;
  try
    for vModuleName in vModules do
    begin
      vModulePath := 'modules' + PathDelim + vModuleName.ToLowerInvariant + PathDelim;
      AddResources(vPlatformBinPath, vModulePath + 'icons', '..\..\..\bin\', '*.png');
      AddResources(vPlatformBinPath, vModulePath + 'images', '..\..\..\bin\', '*.png');
      AddResources(vPlatformBinPath, vModulePath + 'layouts', '..\..\..\bin\', '*.xtf');
    end;
  finally
    FreeAndNil(vModules);
  end;

  vSolutionPath := 'solutions' + PathDelim + vProjectName.ToLowerInvariant + PathDelim;
  AddResources(vPlatformBinPath, vSolutionPath + 'icons', '..\..\..\bin\', '*.png');
  AddResources(vPlatformBinPath, vSolutionPath + 'images', '..\..\..\bin\', '*.png');
  AddResources(vPlatformBinPath, vSolutionPath + 'layouts', '..\..\..\bin\', '*.xtf');

  // Добавление иконок
  vTargetFileName := TPath.Combine(TPath.Combine(vPlatformPath, 'bin'), 'favicon.ico');
  if TFile.Exists(vTargetFileName) then
    AddFileResources('favicon.ico', '', '..\..\..\bin\');
  vTargetFileName := TPath.Combine(vBinPath, 'favicon.ico');
  if TFile.Exists(vTargetFileName) then
    AddFileResources('favicon.ico', vSolutionPath, '..\..\..\bin\');

  // Добавление настроек
  vTargetFileName := TPath.Combine(TPath.Combine(vPlatformPath, 'bin'), 'settings.ini');
  if TFile.Exists(vTargetFileName) then
    AddFileResources('settings.ini', '', '..\..\..\bin\');
  vTargetFileName := TPath.Combine(vBinPath, 'settings.ini');
  if TFile.Exists(vTargetFileName) then
    AddFileResources('settings.ini', vSolutionPath, '..\..\..\bin\');

  vFileContent := TStringStream.Create('', TEncoding.ANSI);
  try
    // Создание файла скрипта (*.pas файла)
    vProcessingFileName := TPath.Combine(vTemplatesPath, 'NewScript.pas');
    if TFile.Exists(vProcessingFileName) then
    begin
      vFileContent.LoadFromFile(vProcessingFileName);
      vHandledText := ReplaceStr(vFileContent.DataString, 'ProjectName', vProjectName);
      vHandledText := ReplaceStr(vHandledText, '$ProjectTitle$', vTitle);
      vHandledText := ReplaceStr(vHandledText, '$ProjectVersion$', vVersion);
      vFileContent.Size := 0;
      vFileContent.WriteString(vHandledText);
      vFileContent.SaveToFile(TPath.Combine(vSrcPath, 'u' + vProjectName + 'Script.pas'));
    end;
  finally
    FreeAndNil(vFileContent);
  end;

  vFrameworks := TListField(AAppSettings.FieldByName('Frameworks'));
  for vFrameworkRel in vFrameworks do
  begin
    vFramework := vFrameworkRel.ExtractEntity('Framework');
    if Assigned(vFramework) then
      CopyProjectFiles(vFramework['Name']);
  end;

  // Создание проектной группы
  vFileContent := TStringStream.Create('', TEncoding.UTF8);
  try
    vProcessingFileName := TPath.Combine(vTemplatesPath, 'ProjectName.groupproj');
    if TFile.Exists(vProcessingFileName) then
    begin
      vFileContent.LoadFromFile(vProcessingFileName);
      CreateGUID(vGUID);
      vHandledText := ReplaceStr(vFileContent.DataString, '$ProjectGuid$', UpperCase(GUIDToString(vGUID)));
      vHandledText := ReplaceStr(vHandledText, '$GroupItems$', vProjectGroupItemsText);
      vHandledText := ReplaceStr(vHandledText, '$GroupTargets$', vProjectGroupTargetsText);
      vHandledText := ReplaceStr(vHandledText, '$BuildTargets$', vBuildTargets);
      vHandledText := ReplaceStr(vHandledText, '$CleanTargets$', vCleanTargets);
      vHandledText := ReplaceStr(vHandledText, '$MakeTargets$', vMakeTargets);
      vFileContent.Size := 0;
      vFileContent.WriteString(vHandledText);
      Result := TPath.Combine(vSrcPath, vProjectName + '.groupproj');
      vFileContent.SaveToFile(Result);
    end;
  finally
    FreeAndNil(vFileContent);
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
  if AEntity.InstanceOf('_Configurations') and (AFieldName = 'Name') then
  begin
    AHandled := True;
    //if not VarIsNull(ANewValue) then
    //  Result := IsValidIdentifier(ANewValue)
    //else
    //  Result := False;
  end;
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
    NativeInt(vDomain.EntityByID('SysUsers', 1)), NativeInt(vDomain.EntityByID('SysRoles', 1))], True);

  vCollection := vDomain['SysConstants'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'Code;MailLogin;MailPassword', ['debug', 'noname@mail.ru', '']);

  vCollection := vDomain['UIFrameworks'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'Name', ['VCL']);
  vCollection.CreateDefaultEntity(AHolder, 2, 'Name', ['FMX']);
  vCollection.CreateDefaultEntity(AHolder, 3, 'Name', ['UniGUI']);
  vCollection.CreateDefaultEntity(AHolder, 4, 'Name', ['FGX']);
end;

procedure TConfiguratorScript.DoCreateDefinitions;
var
  vDefinition: TDefinition;
begin
  AddStateMachine<TTestingState>('TestingStates').AddDisplayNames(cTestingStateNames);

  vDefinition := DefinitionByName('SysConstants');
  vDefinition.AddSimpleFieldDef('PlatformFolder', 'platform_folder', 'Корневая папка платформы', Null, Null, 255, fkString, 'dir');

  vDefinition := AddDefinition('_Configurations', '', 'Конфигурации', cNullItemName, ccHideInMenu, clkMixin);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('ProjectName', 'project_name', 'Имя проекта', Null, Null, 50, fkString, '', '', vsReadOnly, cRequired);
  vDefinition.AddSimpleFieldDef('Caption', 'caption', 'Заголовок', Null, Null, 80, fkString, '', '', vsFullAccess, cRequired or cLocalizable);
  vDefinition.AddSimpleFieldDef('PackageName', 'package_name', 'Пакет', Null, Null, 80, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Version', 'version', 'Версия', '0.1', Null, 15, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.RegisterReaction('ProjectName;PackageName', 'Name', TProc(procedure(
      const AHolder: TChangeHolder; const AFieldChain: string; const AEntity, AParameter: TEntity)
    var
      vProjectName: string;
    begin
      vProjectName := Sanitize(AEntity['Name']);
      AEntity._SetFieldValue(AHolder, 'ProjectName', vProjectName);
      AEntity._SetFieldValue(AHolder, 'PackageName', 'pro.sensoft.' + vProjectName.ToLowerInvariant);
    end));

  vDefinition := AddDefinition('Configurations', '_Configurations', 'Конфигурации', cNullItemName, 0).SetImageID('group16');
  vDefinition.AddListFieldDef('Frameworks', 'Configuration', 'Фреймворки для отображения', 'ConfigurationFrameworks', '', 'mtm?transit=Framework', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddAction('MakeSolution', 'Создать приложение', '');
  vDefinition.AddUniqueIndex('Name');

  vDefinition := AddDefinition('UIFrameworks', '', 'Фреймворки для отображения', cNullItemName, ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Название', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Description', 'description', 'Описание', Null, Null, 255, fkString, '', '', vsFullAccess, cLocalizable);

  vDefinition := AddDefinition('ConfigurationFrameworks', '', 'UI для конфигураций', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('Configuration', 'configuration', 'Конфигурация', '', 'Configurations', 0, vsSelectOnly, cRequired);
  vDefinition.AddEntityFieldDef('Framework', 'framework', 'Фреймворк', '', 'UIFrameworks', 0, vsSelectOnly, cRequired);
  vDefinition.AddUniqueIndex('Configuration,Framework');

  vDefinition := AddDefinition('Streets', '', 'Улицы', cNullItemName);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddListFieldDef('Districts', 'Street', 'Район', 'StreetDistrictRelations', '', 'mtm?transit=District', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddListFieldDef('Owners', 'Street', 'Владельцы', '_Owners', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddEntityFieldDef('Owner', 'owner', 'Главный', '', '_Owners', 0, vsFullAccess, cRequired);

  vDefinition := AddDefinition('_Owners', '', 'Владельцы', cNullItemName, 0, clkMixin);
  vDefinition.AddEntityFieldDef('Street', 'street', 'Улица', '', 'Streets', 0, vsHidden, cRequired);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);

  vDefinition := AddDefinition('Persons', '_Owners', 'Люди', cNullItemName);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Имя человека', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);

  vDefinition := AddDefinition('Animals', '_Owners', 'Животные', cNullItemName);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Кличка животного', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);

  vDefinition := AddDefinition('Districts', '', 'Районы', cNullItemName);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddStateFieldDef('State', 'state', 'Состояние', tsReady, 'TestingStates').SetFlags(cHideInGrid);
  vDefinition.AddEntityFieldDef('Owner', 'owner', 'Владелец', '', 'Persons', 0, vsFullAccess, 0, estUserSort, '');
  vDefinition.AddListFieldDef('Streets', 'District', 'Улица', 'StreetDistrictRelations', '', 'mtm?transit=Street', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddSimpleFieldDef('Phone', 'phone', 'Телефон', '8-921-777-33-44', Null, 50, fkString, 'phone', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Email', 'email', 'Email', 'ya@ya.com', Null, 50, fkString, 'email', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('URL', 'url', 'URL', 'sayt.domen/stranica', Null, 50, fkString, 'url', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('INN', 'inn', 'ИНН', '12345678901', Null, 50, fkString, 'INN', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Memo', 'memo', 'Memo', 'memo-memo-memo', Null, 50, fkString, 'memo', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Log', 'log', 'Log', 'log:log:log:log', Null, 50, fkString, 'log', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Info', 'info', 'info', 'info+info+info', Null, 50, fkString, 'info', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Mru', 'mru', 'mru', 'mru1', Null, 50, fkString, 'mru', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Dir', 'dir', 'dir', 'x:\mydir', Null, 50, fkString, 'dir', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('File', 'file', 'file', 'x:\mydir\myfile.file', Null, 50, fkString, 'file', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('ImageByString', 'imagebystring', 'ImageByString', 'ok', Null, 50, fkString, 'ImageByString?ok=1&problems=2&fail=3', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Selector', 'selector', 'selector', Null, Null, 50, fkString, 'selector', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('Comport', 'comport', 'comport', Null, Null, 50, fkString, 'comport', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('Fieldpath', 'fieldpath', 'fieldpath', Null, Null, 50, fkString, 'fieldpath', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('Integer', 'integer', 'Integer', 1234567890, -10, 1234567891, fkInteger, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('IntegerInfo', 'integerinfo', 'IntegerInfo', 1234567890, -10, 1234567891, fkInteger, 'info', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('IntegerSpinner', 'integerspinner', 'IntegerSpinner', 1, 0, 1, fkInteger, 'spinner', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('IntegerRef', 'integerref', 'IntegerRef', 75, Null, Null, fkInteger, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('IntegerPages', 'integerpages', 'IntegerPages', 0, 0, 100, fkInteger, 'pages', '', vsFullAccess, cRequired);
//  vDefinition.AddSimpleFieldDef('Enum', 'enum', 'Enum', Null, Null, Null, fkEnum, '', '', vsFullAccess, 0);
//  vDefinition.AddSimpleFieldDef('Enumradio', 'enumradio', 'Enumradio', Null, Null, Null, fkEnum, 'radio', '', vsFullAccess, 0);
//  vDefinition.AddSimpleFieldDef('Enuminfo', 'enuminfo', 'Enuminfo', Null, Null, Null, fkEnum, 'info', '', vsFullAccess, 0);
//  vDefinition.AddSimpleFieldDef('Enumlinestyle', 'enumlinestyle', 'Enumline_style', Null, Null, Null, fkEnum, 'line_style', '', vsFullAccess, 0);
//  vDefinition.AddSimpleFieldDef('Enumpages', 'enumpages', 'Enumpages', Null, Null, Null, fkEnum, 'pages', '', vsFullAccess, 0);
//  vDefinition.AddSimpleFieldDef('Flag', 'Flag', 'Flag', 123, Null, Null, fkEnum, '', '', vsFullAccess, 0);
//  vDefinition.AddSimpleFieldDef('EnumEntity', 'enumentity', 'EnumEntity', Null, Null, Null, fkEnum, 'enum', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('Float', 'float', 'Float', 12345.6789, -10.25, 123456789.6, fkFloat, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Floatcurrencyrate', 'floatcurrencyrate', 'Floatcurrencyrate', 12345.6789, -10.25, 123456789.6, fkFloat, 'currency_rate', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Floatinfo', 'floatinfo', 'Floatinfo', 12345.6789, -10.25, 123456789.6, fkFloat, 'info', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Floatgauge', 'floatgauge', 'Floatgauge', 12.6789, 0, 20.6, fkFloat, 'gauge', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Date', 'date', 'Date', 100000, Null, Null, fkDateTime, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Time', 'time', 'time', 0.752, Null, Null, fkDateTime, 'time', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Datetime', 'datetime', 'datetime', 100000.369, Null, Null, fkDateTime, 'datetime', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Dateinfo', 'dateinfo', 'Dateinfo', 100000.369, Null, Null, fkDateTime, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Currency', 'currency', 'Currency', 1000.3, 100.2, 20000000.4, fkCurrency, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Currencyinfo', 'currencyinfo', 'Currencyinfo', 1000.3, 100.2, 20000000.4, fkCurrency, 'info', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Bool', 'bool', 'Bool', True, Null, Null, fkBoolean, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Boolsimple', 'boolsimple', 'Boolsimple', True, Null, Null, fkBoolean, 'simple', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Boolimagedaction', 'boolimagedaction', 'Boolimagedaction', True, Null, Null, fkBoolean, 'imaged_action', '', vsFullAccess, cRequired);
//  vDefinition.AddSimpleFieldDef('Boolimages', 'boolimages', 'Boolimages', True, Null, Null, fkBoolean, 'images', '', vsFullAccess, cRequired);
//  vDefinition.AddSimpleFieldDef('Boolpages', 'boolpages', 'Boolpages', True, Null, Null, fkBoolean, 'pages', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Boolselectedcaption', 'boolselectedcaption', 'Boolselectedcaption', True, Null, Null, fkBoolean, 'selected_caption', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Color', 'color', 'Color', 1, Null, Null, fkColor, '', '', vsFullAccess, 0);
  vDefinition.AddSimpleFieldDef('Colorsimple', 'colorsimple', 'Colorsimple', 2, Null, Null, fkColor, 'simple', '', vsFullAccess, 0);
  vDefinition.AddComplexFieldDef('SimpleChart', 'simple_chart', 'Чарт', 'Tensor', 'chart');


//  uiBLOBEdit, '', TDEBLOBEditor);
//  uiBLOBEdit, 'image', TDEImageEditor);
//  uiEntityEdit, 'info', TTextInfo);
//  uiEntityEdit, 'pages', TDEPagesFieldEditor);

  vDefinition := AddDefinition('StreetDistrictRelations', '', 'Улицы-районы', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('District', 'district', 'Район', '', 'Districts', 0, vsSelectOnly, cRequired);
  vDefinition.AddEntityFieldDef('Street', 'street', 'Улица', '', 'Streets', 0, vsSelectOnly, cRequired);
  vDefinition.AddUniqueIndex('District,Street');
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
      vProjFileName := GenerateModules(vDomain, vEntity);

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
