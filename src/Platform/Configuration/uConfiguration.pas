{---------------------------------------------------------------------------------
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

unit uConfiguration;

interface

uses
  Classes, SysUtils, Generics.Collections, uDefinition, uEnumeration, uReaction,
  uIcon, uLocalizator, uUtils;

type
  TConfiguration = class;

  TReactionDef = class
  private
    FDefinitions: TStrings;
    FReactiveFields: TStrings;
    FChains: TStrings;
    FHandler: TProc;
  public
    constructor Create(const ADefinitionNames, AReactiveFields, AFieldChains: string; const AHandler: TProc);
    destructor Destroy; override;
  end;

  TConfiguration = class
  private
    [Weak] FPlatform: TObject;
    FName: string;
    FCaption: string;
    FVersion: TVersion;
    FConfigurationDir: string;
    FAppDataDir: string;
    FCacheDir: string;
    FRootDefinition: TDefinition;
    FDefinitions: TDefinitions;
    FActions: TActions;
    FEnumerations: TEnumerations;
    FStateMachines: TStateMachines;
    FIcons: TIcons;
    FIconFileName: string;
    FLocalizator: TCfgLocalizator;
    FComplexClasses: TComplexClasses;
    FPlannedJobs: TPlannedJobs;
    FReactionDefs: TObjectList<TReactionDef>;
    FResFolders: TStrings;
    procedure LoadDataFromFiles(const ADirectory: string);
    function GetDefinitionByName(const AName: string): TDefinition;
    procedure CreateReactionChains;
  protected
    FCreateDefaultEntitiesProc: TMethod;
    FExecuteDefaultActionFunc: TMethod;
    FDomainReadyProc: TMethod;
    FDomainStoppedProc: TMethod;
    FGetReportValueFunc: TMethod;
    FCheckActionFlagsFunc: TMethod;
    FExecuteActionFunc: TMethod;
    FExecuteCommandFunc: TMethod;
    FAfterEntityCreationProc: TMethod;
    FAfterEntitySavedProc: TMethod;
    FBeforeEntitySavingProc: TMethod;
    FBeforeEntityRemovingProc: TMethod;
    FFullTextFunc: TMethod;
    FCheckFieldFunc: TMethod;
    FLoginedProc: TMethod;
    FBeforeUIClosingFunc: TMethod;
    FCalculateStyleFunc: TMethod;
    FCanChangeFieldFunc: TMethod;
    FGetParamValueFunc: TMethod;
    FCreateContentTypeReactionProc: TMethod;

    function LoadConfiguration: string; virtual;
    function GetTitle: string; virtual;
    function GetVersionName: string; virtual;
  public
    constructor Create(const APlatform: TObject; const AName: string);
    destructor Destroy; override;

    procedure Init;

    // Наследование ресурсов
    function FindLeafFile(const ARelativeFilePath: string): string;
    function FindLayoutFile(const ARelativeFilePath, AExtension: string; const APostfix: string = ''): string;
    procedure RegisterReaction(const ADefinitionNames, AReactiveFields, AFieldChain: string;
      const AReactionProc: TProc);
    procedure AddInclusion(const AName: string);

    property Name: string read FName;
    property _Caption: string read FCaption;
    property Version: TVersion read FVersion;
    property VersionName: string read GetVersionName;
    property IconFileName: string read FIconFileName;
    property ConfigurationDir: string read FConfigurationDir;
    property AppDataDir: string read FAppDataDir;
    property CacheDir: string read FCacheDir;

    property RootDefinition: TDefinition read FRootDefinition;
    //property Inclusions: TObjectList<TCfgInclusion> read FInclusions;
    property Icons: TIcons read FIcons;
    property Localizator: TCfgLocalizator read FLocalizator;
    property Enumerations: TEnumerations read FEnumerations;
    property StateMachines: TStateMachines read FStateMachines;
    property ComplexClasses: TComplexClasses read FComplexClasses;
    property PlannedJobs: TPlannedJobs read FPlannedJobs;
    property Definitions: TDefinitions read FDefinitions;
    property DefinitionByName[const AName: string]: TDefinition read GetDefinitionByName; default;
    // Общесистемные действия и отчёты
    property Actions: TActions read FActions;

    property CreateDefaultEntitiesProc: TMethod read FCreateDefaultEntitiesProc;
    property ExecuteDefaultActionFunc: TMethod read FExecuteDefaultActionFunc;
    property DomainReadyProc: TMethod read FDomainReadyProc;
    property DomainStoppedProc: TMethod read FDomainStoppedProc;
    property GetReportValueFunc: TMethod read FGetReportValueFunc;
    property CheckActionFlagsFunc: TMethod read FCheckActionFlagsFunc;
    property ExecuteActionFunc: TMethod read FExecuteActionFunc;
    property ExecuteCommandFunc: TMethod read FExecuteCommandFunc;
    property AfterEntityCreationProc: TMethod read FAfterEntityCreationProc;
    property BeforeEntitySavingProc: TMethod read FBeforeEntitySavingProc;
    property AfterEntitySavedProc: TMethod read FAfterEntitySavedProc;
    property BeforeEntityRemovingProc: TMethod read FBeforeEntityRemovingProc;
    property FullTextFunc: TMethod read FFullTextFunc;
    property CheckFieldFunc: TMethod read FCheckFieldFunc;
    property LoginedProc: TMethod read FLoginedProc;
    property BeforeUIClosingFunc: TMethod read FBeforeUIClosingFunc;
    property CalculateStyleFunc: TMethod read FCalculateStyleFunc;
    property CanChangeFieldFunc: TMethod read FCanChangeFieldFunc;
    property GetParamValueFunc: TMethod read FGetParamValueFunc;
    property CreateContentTypeReactionProc: TMethod read FCreateContentTypeReactionProc;
  end;

  {TConfiguration = class
    - наименование
    - актуальная версия >> функционал по обновлению запущенных доменов
    - предыдущие версии для миграции
      - версия
      - список действий по переходу на следующую версию
    - владелец
    - стоимость инстанцирования
    - создатели
      - пользователь
      - инстанцированные домены
        - ограничения по времени
        - ограничения по пользователям
        - домен

  end;}

implementation

uses
  IOUtils, uConsts, uSettings;

{ TConfiguration }

procedure TConfiguration.AddInclusion(const AName: string);
begin
  FResFolders.Add(TPath.Combine(GetResDir, 'modules' + PathDelim + AName));
end;

constructor TConfiguration.Create(const APlatform: TObject; const AName: string);
var
  vAppPath: string;
begin
  inherited Create;

  FPlatform := APlatform;
  FName := AName;
  FIconFileName := '';
  FResFolders := TStringList.Create;
  vAppPath := cProductCreator + PathDelim + FName;

  FConfigurationDir := TPath.Combine(GetResDir, 'solutions' + PathDelim + FName.ToLowerInvariant);

{$IF DEFINED(MSWINDOWS)}
  FAppDataDir := TPath.Combine(TPath.GetCachePath, vAppPath);
  FCacheDir := TPath.Combine(TPath.GetCachePath, vAppPath);
{$ELSEiF DEFINED(LINUX) }
  FAppDataDir := TPath.Combine(TPath.GetCachePath, vAppPath);
  FCacheDir := TPath.Combine(TPath.GetCachePath, vAppPath);
{$ELSEIF DEFINED(ANDROID) OR DEFINED(IOS)}
  FAppDataDir := TPath.GetPublicPath;
  FCacheDir := TPath.GetCachePath;
{$ELSEIF DEFINED(IOS)}
  FAppDataDir := TPath.GetDocumentsPath;
  FCacheDir := TPath.GetCachePath;
{$ENDIF}

  if not TDirectory.Exists(FConfigurationDir) then
    TDirectory.CreateDirectory(FConfigurationDir);
  if not TDirectory.Exists(FAppDataDir) then
    TDirectory.CreateDirectory(FAppDataDir);
  if not TDirectory.Exists(FCacheDir) then
    TDirectory.CreateDirectory(FCacheDir);

  FLocalizator := TCfgLocalizator.Create(TPath.Combine(FConfigurationDir, 'translations'),
    TPath.Combine(FConfigurationDir, 'settings.ini'));
  FEnumerations := TEnumerations.Create;
  FStateMachines := TStateMachines.Create;
  FDefinitions := TDefinitions.Create(Self);
  FPlannedJobs := TPlannedJobs.Create;
  FIcons := TIcons.Create;

  FActions := TActions.Create(Self);
  FComplexClasses := TComplexClasses.Create(Self);
  FReactionDefs := TObjectList<TReactionDef>.Create;
end;

procedure TConfiguration.CreateReactionChains;
var
  vReaction: TReactionDef;
  vDefinitionName: string;
  vDefinition: TDefinition;

  function FindDefinitionByName(const ADefinitionName: string): TDefinition;
  var
    vDefinitionName: string;
    vParentDefinitionName: string;
    vPos: Integer;
  begin
    vDefinitionName := ADefinitionName;
    Result := GetDefinitionByName(vDefinitionName);
    if not Assigned(Result) then
      Result := FActions.ObjectByName(vDefinitionName);
    if not Assigned(Result) and (Pos('.', vDefinitionName) > 0) then
    begin
      vPos := Pos('.', vDefinitionName);
      vParentDefinitionName := Copy(vDefinitionName, 1, vPos - 1);
      System.Delete(vDefinitionName, 1, vPos);
      if FDefinitions.Exists(vParentDefinitionName) then
        Result := FDefinitions.ObjectByName(vParentDefinitionName).ActionByName(vDefinitionName)
      else if FActions.Exists(vParentDefinitionName) then
        Result := FActions.ObjectByName(vParentDefinitionName).ActionByName(vDefinitionName);
    end;
  end;

begin
  for vReaction in FReactionDefs do
  begin
    for vDefinitionName in vReaction.FDefinitions do
    begin
      vDefinition := FindDefinitionByName(vDefinitionName);
      if Assigned(vDefinition) then
        vDefinition.AppendReaction(vReaction.FReactiveFields, vReaction.FChains, vReaction.FHandler);
    end;
  end;
end;

destructor TConfiguration.Destroy;
begin
  FreeAndNil(FResFolders);
  FreeAndNil(FReactionDefs);
  FreeAndNil(FActions);

  FRootDefinition := nil;

  FreeAndNil(FIcons);
  FreeAndNil(FComplexClasses);
  FreeAndNil(FPlannedJobs);

  FreeAndNil(FDefinitions);

  FreeAndNil(FStateMachines);
  FreeAndNil(FEnumerations);

  FreeAndNil(FLocalizator);

  inherited Destroy;
end;

function TConfiguration.FindLayoutFile(const ARelativeFilePath, AExtension, APostfix: string): string;
var
  vLayoutFile: string;
begin
  if FCacheDir = FConfigurationDir then
    Result := ''
  else
  begin
    vLayoutFile := TPath.Combine(FConfigurationDir, 'layouts' + PathDelim + ARelativeFilePath);
    if (APostfix <> '') and FileExists(vLayoutFile + APostfix + AExtension) then
      Result := vLayoutFile + APostfix + AExtension
    else if FileExists(vLayoutFile + AExtension) then
      Result := vLayoutFile + AExtension
    else
      Result := '';
  end;
end;

function TConfiguration.FindLeafFile(const ARelativeFilePath: string): string;
begin
  Result := TPath.Combine(FConfigurationDir, ARelativeFilePath);
end;

function TConfiguration.GetDefinitionByName(const AName: string): TDefinition;
begin
  Result := FDefinitions.ObjectByName(AName);
end;

function TConfiguration.GetTitle: string;
begin
  Result := 'unnamed';
end;

function TConfiguration.GetVersionName: string;
begin
  Result := FVersion.ToString;
end;

procedure TConfiguration.Init;
var
  vResourcesDir: string;
  vDefinition: TDefinition;
  vAction: TActionDef;
begin
  FCaption := GetTitle;
  FVersion := LoadConfiguration;

  // Обновляем унаследованные поля
  for vDefinition in FDefinitions do
    vDefinition.UpdateInheritance;

  // Устанавливаем все связи с наследниками
  for vDefinition in FDefinitions do
    vDefinition.FillContentDefinitions;
  // Прописываем все реакции и транзиты с учетом наследования
  CreateReactionChains;
  // Отрабатываем ссылки на сущности и наследование
  for vDefinition in FDefinitions do
    vDefinition.CreateObjectRelations;
  // Отрабатываем списки и (важно!) транзитные поля
  for vDefinition in FDefinitions do
    vDefinition.CreateListRelations;

  // Отрабатываем ссылки на сущности и наследование
  for vAction in FActions.Objects do
    vAction.CreateObjectRelations;
  // Отрабатываем списки и (важно!) транзитные поля
  for vAction in FActions.Objects do
    vAction.CreateListRelations;

  for vResourcesDir in FResFolders do
    FIcons.Load(vResourcesDir);

  LoadDataFromFiles(FConfigurationDir);

  FRootDefinition := GetDefinitionByName('SysDefinitions');
end;

function TConfiguration.LoadConfiguration: string;
begin
  Result := '0.0';
end;

procedure TConfiguration.LoadDataFromFiles(const ADirectory: string);
var
  vDefinition: TDefinition;
  vSearchRec: TSearchRec;
  vDirName: string;
  vReportDirName: string;
  vFileExt: string;
begin
  // Загрузка конфигурационных картинок
  FIcons.Load(ADirectory);

  FIconFileName := TPath.Combine(ADirectory, 'favicon.ico');

  // Загрузка отчетов
  vDirName := TPath.Combine(ADirectory, 'reports');
  if not DirectoryExists(vDirName) then
  begin
    //Core.Logger.AddMessage('Текстовых отчетов нет');
    Exit;
  end;

  { TODO -owa : Переписать на нахождение отчетов по факту в файловой системе }
  for vDefinition in FDefinitions do
  begin
    vReportDirName := TPath.Combine(vDirName, vDefinition.Name);
    if not DirectoryExists(vReportDirName) then
      Continue;

    if SysUtils.FindFirst(vReportDirName + PathDelim + '*.*', faAnyFile, vSearchRec) <> 0 then
      Continue;

    repeat
      if (vSearchRec.Attr and faDirectory) = 0 then
      begin
        vFileExt := ExtractFileExt(vSearchRec.Name);
        if vFileExt = '.rtf' then
          vDefinition.AddRTFReport(ExtractFileName(vSearchRec.Name), ExtractFileName(vSearchRec.Name),
            TPath.Combine(vReportDirName, vSearchRec.Name));
      end;
    until SysUtils.FindNext(vSearchRec) <> 0;

    SysUtils.FindClose(vSearchRec);
  end;
end;

procedure TConfiguration.RegisterReaction(const ADefinitionNames, AReactiveFields, AFieldChain: string;
  const AReactionProc: TProc);
begin
  FReactionDefs.Add(TReactionDef.Create(ADefinitionNames, AReactiveFields, AFieldChain, AReactionProc));
end;

{ TReactionDef }

constructor TReactionDef.Create(const ADefinitionNames, AReactiveFields, AFieldChains: string; const AHandler: TProc);
begin
  inherited Create;

  FDefinitions := CreateDelimitedList(ADefinitionNames);
  FReactiveFields := CreateDelimitedList(AReactiveFields);
  FChains := CreateDelimitedList(AFieldChains);
  FHandler := AHandler;
end;

destructor TReactionDef.Destroy;
begin
  FreeAndNil(FDefinitions);
  FreeAndNil(FReactiveFields);
  FreeAndNil(FChains);
  FHandler := nil;

  inherited Destroy;
end;

end.
