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

unit uLocalizator;

interface

uses
  Classes, Generics.Collections;

type
  TLanguage = class
  private
    FName: string;
    FFileName: string;
    FDictionary: TStrings;
    function GetCount: Integer;
    function GetKey(const AIndex: Integer): string;
    function GetValue(const AKey: string): string;
    procedure SetValue(const AKey, Value: string);
  public
    constructor Create(const AName, AFileName: string);
    destructor Destroy; override;

    function KeyExists(const AKey: string): Boolean;
    procedure Save;

    property Name: string read FName;
    property Count: Integer read GetCount;
    property Keys[const AIndex: Integer]: string read GetKey;
    property Values[const AKey: string]: string read GetValue write SetValue; default;
  end;

  TLocalizator = class
  private
    FLangNames: TStrings;
    FLanguages: TObjectList<TLanguage>;
    FIniFileName: string;
    procedure EnumerateLanguages(const ADirName: string);
  protected
    FDefaultLanguage: TLanguage;
    FLangDir: string;
    FNeedTranslation: Boolean;
    procedure _Add(const AStorage: TStrings; const AKey, AValue: string);
    function CreateKey(const AText: string): string;
    procedure ChangeLanguage; virtual;
  public
    constructor Create(const ADataFolder, AIniFileName: string);
    destructor Destroy; override;

    function Translate(const AKey: string; const ADefault: string = ''): string;
    function SupportLanguage(const ALanguage: string): Boolean;
    function LanguageByName(const ALangName: string): TLanguage;

    // Заполнение через UI
    procedure FillKeys(const AKeys: TStrings);
    function RenameKey(const AOldName, ANewName: string): Boolean;

    // Вывод для выбора
    property Languages: TObjectList<TLanguage> read FLanguages;
    property DefaultLanguage: TLanguage read FDefaultLanguage;
    property LangDir: string read FLangDir;
  end;

  {TPresenterLocalizator = class(TLocalizator)
  private
    FCompFileName: string;
    FComponents: TStrings;
    procedure AddComponentTranslation(const AComponent: TComponent;
      const AChain, APropertyName, APrefix: string);
    procedure AddTranslation(const AChain, APrefix, AText: string);
    procedure ApplyComponentTranslation(const AComponent: TComponent;
      const AChain, APropertyName: string);
  protected
    procedure ChangeLanguage; override;
  public
    constructor Create(const ADataFolder, AIniFileName: string);
    destructor Destroy; override;

    procedure EnumerateUIComponents(const AComponent: TComponent;
      const AParentChain: string);
    procedure ApplyTranslation(const AComponent: TComponent;
      const AParentChain: string);
  end;}

  TCfgLocalizator = class(TLocalizator)
  protected
    procedure ChangeLanguage; override;
  public
    procedure AddTranslation(const AKey, AText: string);
    procedure EnumerateConfigurationElements(const AConfiguration: TObject);
  end;

function EscapeText(const AText: string): string;
function UnEscapeText(const AText: string): string;

implementation

uses
  IOUtils, SysUtils, StrUtils, IniFiles,
  //TypInfo, Forms, Controls, StdCtrls, ActnList, Menus,
  uConfiguration, uDefinition, uConsts;

function EscapeText(const AText: string): string;
begin
  Result := ReplaceStr(AText, #10, '');
  Result := ReplaceStr(Result, #13, #182);
end;

function UnEscapeText(const AText: string): string;
begin
  Result := ReplaceStr(AText, #182, #13#10);
end;

{ TLocalizator }

///type
///  TCrackedControl = class(TControl);

procedure TLocalizator._Add(const AStorage: TStrings; const AKey,
  AValue: string);
begin

end;

constructor TLocalizator.Create(const ADataFolder, AIniFileName: string);
var
  vIniFile: TMemIniFile;
begin
  inherited Create;

  FNeedTranslation := False;
  FIniFileName := AIniFileName;

  FLangDir := ADataFolder;
  if not DirectoryExists(FLangDir) then
    ForceDirectories(FLangDir);

  FLangNames := TStringList.Create;
  vIniFile := TMemIniFile.Create(FIniFileName);
  try
    vIniFile.ReadSectionValues('LangNames', FLangNames);
    if FLangNames.Count = 0 then
    begin
      FLangNames.Add('russian=Русский');
      FLangNames.Add('english=English');
    end;
  finally
    vIniFile.Free;
  end;

  FLanguages := TObjectList<TLanguage>.Create;

  if FileExists(TPath.Combine(FLangDir, 'default.lng')) then
    FDefaultLanguage := TLanguage.Create('', TPath.Combine(FLangDir, 'default.lng'));

  EnumerateLanguages(FLangDir);
end;

function TLocalizator.CreateKey(const AText: string): string;
var
  vChar: Char;
  vWereSpace: Boolean;
  vPos: Integer;
  vLength: Integer;
const
  cRusChars: string =
    'абвгдеёжзийклмнопрстуфхцчшщьыъэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯ';
  cConstChars: string =
    'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPRSTUVWXYZ0123456789';
  cResults: array[1..66] of string =
  ( 'a', 'b', 'v', 'g', 'd', 'e', 'yo', 'zh', 'z', 'i', 'j',
    'k', 'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'f',
    'kh', 'ts', 'ch', 'sh', 'shch', '', 'y', '', 'e', 'yu', 'ya',
    'A', 'B', 'V', 'G', 'D', 'E', 'Yo', 'Zh', 'Z', 'I', 'J',
    'K', 'L', 'M', 'N', 'O', 'P', 'R', 'S', 'T', 'U', 'F',
    'Kh', 'Ts', 'Ch', 'Sh', 'Shch', '', 'Y', '', 'E', 'Yu', 'Ya' );
begin
  Result := '';
  vWereSpace := True;

  for vChar in AText do
  begin
    vPos := Pos(vChar, cRusChars);
    if vPos > 0 then
    begin
      if (vPos <= 33) and vWereSpace then
        Result := Result + cResults[vPos + 33]
      else
        Result := Result + cResults[vPos];
      vWereSpace := False;
    end
    else begin
      vPos := Pos(vChar, cConstChars);
      if vPos > 0 then
      begin
        if (vPos <= 26) and vWereSpace then
          Result := Result + AnsiUpperCase(vChar)
        else
          Result := Result + vChar;
        vWereSpace := False;
      end
      else
        vWereSpace := True;
    end;
  end;

  vLength := Length(Result);
  if vLength > 100 then
    Delete(Result, 101, vLength - 100);
end;

destructor TLocalizator.Destroy;
begin
  FreeAndNil(FDefaultLanguage);
  FreeAndNil(FLanguages);
  FreeAndNil(FLangNames);

  inherited Destroy;
end;

procedure TLocalizator.ChangeLanguage;
begin
end;

procedure TLocalizator.EnumerateLanguages(const ADirName: string);
var
  vSearchEntry: TSearchRec;
  vFileName: string;
  vLanguage: TLanguage;
  vLangName: string;
begin
  if FindFirst(ADirName + '\*.lng', faAnyFile, vSearchEntry) = 0 then
  try
    repeat
      if AnsiLowerCase(vSearchEntry.Name) = 'default.lng' then
        Continue;

      vFileName := AnsiLowerCase(TPath.GetFileNameWithoutExtension(vSearchEntry.Name));
      if FLangNames.IndexOfName(vFileName) < 0 then
      begin
        vLangName := vFileName;
        FLangNames.Add(vFileName + '=' + vLangName);
      end
      else
        vLangName := FLangNames.Values[vFileName];

      vLanguage := TLanguage.Create(vLangName, TPath.Combine(ADirName, vSearchEntry.Name));
      FLanguages.Add(vLanguage);
    until FindNext(vSearchEntry) <> 0;
  finally
    FindClose(vSearchEntry);
  end;
end;

procedure TLocalizator.FillKeys(const AKeys: TStrings);
var
  i: Integer;
  vKey: string;
begin
  for i := 0 to FDefaultLanguage.Count - 1 do
  begin
    vKey := FDefaultLanguage.Keys[i];
    if AKeys.IndexOf(vKey) < 0 then
      AKeys.Add(vKey);
  end;
end;

function TLocalizator.LanguageByName(const ALangName: string): TLanguage;
var
  vLanguage: TLanguage;
begin
  for vLanguage in FLanguages do
    if SameText(vLanguage.Name, ALangName) then
      Exit(vLanguage);
  Result := nil;
end;

function TLocalizator.RenameKey(const AOldName, ANewName: string): Boolean;
begin
  Result := False;
end;

function TLocalizator.SupportLanguage(const ALanguage: string): Boolean;
begin
  Result := Assigned(LanguageByName(ALanguage));
end;

function TLocalizator.Translate(const AKey: string; const ADefault: string = ''): string;
begin
  if not FDefaultLanguage.KeyExists(AKey) then
    FDefaultLanguage[AKey] := ADefault;
  Result := ADefault;
end;

(*{ TPresenterLocalizator }

procedure TPresenterLocalizator.AddComponentTranslation(const AComponent: TComponent; const AChain, APropertyName,
  APrefix: string);
var
  vPropInfo: PPropInfo;
begin
  vPropInfo := GetPropInfo(AComponent.ClassInfo, APropertyName);
  if Assigned(vPropInfo) then
    AddTranslation(AChain + APropertyName, APrefix, GetPropValue(AComponent, vPropInfo, True));
end;

procedure TPresenterLocalizator.AddTranslation(const AChain, APrefix, AText: string);
var
  vIndex: Integer;
  vKey: string;
begin
  if AText <> '' then
  begin
    vIndex := FComponents.IndexOfName(AChain);
    if vIndex >= 0 then
      vKey := FComponents.ValueFromIndex[vIndex]
    else begin
      vKey := CreateKey(AText);
      if vKey <> '' then
        vKey := APrefix + vKey;
      Add(FComponents, AChain, vKey);
    end;

    Add(FDefaultDictionary, vKey, AText);
  end;
end;

procedure TPresenterLocalizator.ApplyComponentTranslation(const AComponent: TComponent; const AChain,
  APropertyName: string);
var
  vPropInfo: PPropInfo;
  vKey: string;
  vTranslation: string;
begin
  vPropInfo := GetPropInfo(AComponent.ClassInfo, APropertyName);
  if Assigned(vPropInfo) then
  begin
    vKey := FComponents.Values[AChain + APropertyName];
    vTranslation := GetPropValue(AComponent, vPropInfo, True);
    if vKey <> '' then
      vTranslation := Translate(vKey, vTranslation);
    SetPropValue(AComponent, vPropInfo, vTranslation);
  end;
end;

procedure TPresenterLocalizator.ApplyTranslation(const AComponent: TComponent; const AParentChain: string);
var
  i: integer;
  vChainName: string;
  vKey: string;
  vAlreadyTranslated: TList;
begin
  if not FNeedTranslation then
    Exit;

  if AComponent is TApplication then
  begin
    TApplication(AComponent).Hint :=
      Translate('AppHint', TApplication(AComponent).Hint);
    TApplication(AComponent).Title :=
      Translate('AppTitle', TApplication(AComponent).Title);
  end
  else if (AComponent is TForm) or (AComponent is TFrame)
    or (AComponent is THintWindow) then
  begin
    vChainName := AComponent.ClassName + '.';
    ApplyComponentTranslation(AComponent, vChainName, 'Hint');
    ApplyComponentTranslation(AComponent, vChainName, 'Caption');
    ApplyComponentTranslation(AComponent, vChainName, 'Title');
  end
  else if AComponent is TActionList then
  begin
    for i := 0 to TActionList(AComponent).ActionCount - 1 do
      ApplyTranslation(TActionList(AComponent).Actions[i], AParentChain);
    Exit;
  end
  else if AComponent is TMenu then
  begin
    for i := 0 to TMenu(AComponent).Items.Count - 1 do
      ApplyTranslation(TMenu(AComponent).Items[i], AParentChain);
    Exit;
  end
  else if AComponent is TMemo then
  begin
    vChainName := AParentChain + AComponent.Name + '.';
    vKey := FComponents.Values[vChainName + 'Text'];
    TMemo(AComponent).Text := UnEscapeText(Translate(vKey, EscapeText(TMemo(AComponent).Text)));
    ApplyComponentTranslation(AComponent, vChainName, 'Hint');
    Exit;
  end
  else if AComponent is TComponent then
  begin
    if AComponent.Name = '' then
      Exit;
    vChainName := AParentChain + AComponent.Name + '.';
    ApplyComponentTranslation(AComponent, vChainName, 'Hint');
    ApplyComponentTranslation(AComponent, vChainName, 'Caption');
    ApplyComponentTranslation(AComponent, vChainName, 'Text');
    ApplyComponentTranslation(AComponent, vChainName, 'TextHint');
    ApplyComponentTranslation(AComponent, vChainName, 'DialogCaption');
  end;

  vAlreadyTranslated := TList.Create;

  if AComponent is TWinControl then
    for i := 0 to TWinControl(AComponent).ControlCount - 1 do
    begin
      ApplyTranslation(TWinControl(AComponent).Controls[i], vChainName);
      vAlreadyTranslated.Add(TWinControl(AComponent).Controls[i]);
    end;

  for i := 0 to AComponent.ComponentCount - 1 do
    if vAlreadyTranslated.IndexOf(AComponent.Components[i]) < 0 then
      ApplyTranslation(AComponent.Components[i], vChainName);

  FreeAndNil(vAlreadyTranslated);
end;

procedure TPresenterLocalizator.ChangeLanguage;
begin
  ApplyTranslation(Application, '');
end;

constructor TPresenterLocalizator.Create(const ADataFolder, AIniFileName: string);
begin
  inherited Create(ADataFolder, AIniFileName);

  FComponents := THashedStringList.Create;
  FCompFileName := TPath.Combine(FLangDir, 'components.dmp');
  if FileExists(FCompFileName) then
    FComponents.LoadFromFile(FCompFileName);
end;

destructor TPresenterLocalizator.Destroy;
begin
  FComponents.SaveToFile(FCompFileName);
  FComponents.Free;

  inherited Destroy;
end;

procedure TPresenterLocalizator.EnumerateUIComponents(const AComponent: TComponent; const AParentChain: string);
var
  i: integer;
  vChainName: string;
  vValue: string;
begin
  if AComponent is TApplication then
  begin
    if FComponents.IndexOfName('Hint') < 0 then
    begin
      Add(FComponents, 'Hint', 'AppHint');
      Add(FDefaultDictionary, 'AppHint', TApplication(AComponent).Hint);
    end;
    if FComponents.IndexOfName('Title') < 0 then
    begin
      Add(FComponents, 'Title', 'AppTitle');
      Add(FDefaultDictionary, 'AppTitle', TApplication(AComponent).Title);
    end;
  end
  else if (AComponent is TForm) or (AComponent is TFrame)
    or (AComponent is THintWindow) then
  begin
    vChainName := AComponent.ClassName + '.';
    AddComponentTranslation(AComponent, vChainName, 'Hint', 'hnt');
    AddComponentTranslation(AComponent, vChainName, 'Caption', 'cpt');
    AddComponentTranslation(AComponent, vChainName, 'Title', 'ttl');
  end
  else if AComponent is TActionList then
  begin
    for i := 0 to TActionList(AComponent).ActionCount - 1 do
      EnumerateUIComponents(TActionList(AComponent).Actions[i], AParentChain);
    Exit;
  end
  else if AComponent is TMenu then
  begin
    for i := 0 to TMenu(AComponent).Items.Count - 1 do
      EnumerateUIComponents(TMenu(AComponent).Items[i], AParentChain);
  end
  else if AComponent is TMemo then
  begin
    vChainName := AParentChain + AComponent.Name + '.';
    vValue := EscapeText(TMemo(AComponent).Text);
    AddTranslation(vChainName + 'Text', 'txt', vValue);
    AddComponentTranslation(AComponent, vChainName, 'Hint', 'hnt');
    Exit;
  end
  else if AComponent is TComponent then
  begin
    if AComponent.Name = '' then
      Exit;
    vChainName := AParentChain + AComponent.Name + '.';
    AddComponentTranslation(AComponent, vChainName, 'Hint', 'hnt');
    AddComponentTranslation(AComponent, vChainName, 'Caption', 'cpt');
    AddComponentTranslation(AComponent, vChainName, 'Text', 'txt');
    AddComponentTranslation(AComponent, vChainName, 'TextHint', 'hnt');
    AddComponentTranslation(AComponent, vChainName, 'DialogCaption', 'cpt');
  end
  else
    Assert(False, 'Unknown class: ' + AComponent.ClassName);

  for i := 0 to AComponent.ComponentCount - 1 do
    EnumerateUIComponents(AComponent.Components[i], vChainName);
end;  *)

{ TCfgLocalizator }

procedure TCfgLocalizator.AddTranslation(const AKey, AText: string);
begin
  if AText <> '' then
    FDefaultLanguage[AKey] := AText;
end;

procedure TCfgLocalizator.ChangeLanguage;
begin
  // Что-то сделать при смене языка
end;

procedure TCfgLocalizator.EnumerateConfigurationElements(const AConfiguration: TObject);
var
  vConfiguration: TConfiguration;
  vDefinition: TDefinition;
  vAction: TActionDef;
  vRTFReport: TRTFReport;
  vReportDef: TReportDef;
  vFilter: TFilter;

  procedure EnumerateFields(const ADefinition: TDefinition);
  var
    vFieldDef: TFieldDef;
  begin
    for vFieldDef in ADefinition.Fields do
    begin
      if vFieldDef.HasFlag(cSystem) then
        Continue;

      if vFieldDef.Definition = ADefinition then
      begin
        AddTranslation(vFieldDef.FullName + '@Caption', vFieldDef._Caption);
        AddTranslation(vFieldDef.FullName + '@Hint', vFieldDef._Hint);
      end;
    end;
  end;

begin
  if FileExists(TPath.Combine(FLangDir, 'default.lng')) then
    Exit;

  if not Assigned(FDefaultLanguage) then
    FDefaultLanguage := TLanguage.Create('', TPath.Combine(FLangDir, 'default.lng'));

  vConfiguration := TConfiguration(AConfiguration);
  AddTranslation('AppTitle', vConfiguration._Caption);
  for vDefinition in vConfiguration.Definitions do
  begin
    AddTranslation(vDefinition.FullName + '@Caption', vDefinition._Caption);
    AddTranslation(vDefinition.FullName + '@EmptyValue', vDefinition._EmptyValue);
    AddTranslation(vDefinition.FullName + '@Prefix', vDefinition.Prefix);
    EnumerateFields(vDefinition);

    for vReportDef in vDefinition.Reports.Objects do
      if vReportDef.OwnerDefinition = vDefinition then
      begin
        AddTranslation(vReportDef.FullName + '@Caption', vReportDef._Caption);
        EnumerateFields(vReportDef);
      end;

    for vRTFReport in vDefinition.RTFReports.Objects do
      if vRTFReport.OwnerDefinition = vDefinition then
        AddTranslation(vRTFReport.FullName + '@Caption', vRTFReport._Caption);

    for vAction in vDefinition.Actions.Objects do
      if vAction.OwnerDefinition = vDefinition then
      begin
        AddTranslation(vAction.FullName + '@Caption', vAction._Caption);
        EnumerateFields(vAction);
      end;

    for vFilter in vDefinition.Filters.Values do
      if vFilter.OwnerDefinition = vDefinition then
        AddTranslation(vFilter.FullName + '@Caption', vFilter.Caption);
  end;
end;

{ TLanguage }

constructor TLanguage.Create(const AName, AFileName: string);
begin
  inherited Create;
  FName := AName;
  FFileName := AFileName;
  FDictionary := THashedStringList.Create;
  if FileExists(AFileName) then
    FDictionary.LoadFromFile(AFileName);
end;

destructor TLanguage.Destroy;
begin
  Save;
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

function TLanguage.GetCount: Integer;
begin
  Result := FDictionary.Count;
end;

function TLanguage.GetKey(const AIndex: Integer): string;
begin
  Result := FDictionary.Names[AIndex];
end;

function TLanguage.GetValue(const AKey: string): string;
begin
  Result := FDictionary.Values[AKey];
end;

function TLanguage.KeyExists(const AKey: string): Boolean;
begin
  Result := FDictionary.IndexOfName(AKey) >= 0;
end;

procedure TLanguage.Save;
begin
  FDictionary.SaveToFile(FFileName);
end;

procedure TLanguage.SetValue(const AKey, Value: string);
begin
  FDictionary.Values[AKey] := Value;
end;

end.

