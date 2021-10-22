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

unit uScriptExecutor;

interface

uses
  Classes, Generics.Collections, SyncObjs, uFastClasses, uTask, uCodeParser, uEntity, uSession,
  uChangeManager, uConsts, uScript, uModule, uView;

type
  TParameterType = (ptAny, ptString, ptInteger, ptFloat, ptDateTime, ptBoolean, ptObject, ptCommand, ptText);
  TExpressionNodeKind = (enkValue, enkVariable, enkNeg, enkAdd, enkSub, enkMul, enkDiv);

const
  SM_EVENT_FIRED = $0441;
type
  TEventFiredMessage = packed record
    Msg: Word;
    Kind: Word;
    Sender: TObject;
    Parameter: Integer;
    Reserved: Integer;
  end;

type
  TLogMessageEvent = procedure(const AText: string) of object;
  TCustomEvent = reference to procedure(const ATask: TTaskHandle; const AText: string);
  TScriptEndEvent = reference to procedure(const AFiber: TObject);

  TScriptExecutor = class;

  TExecutionFiber = class;

  TCodeBlock = class;

  TScriptVariable = class
  private
    FValueType: TParameterType;
    FValue: Variant;
    FIsLocal: Boolean;
    FName: string;
    FTextValue: string;
    FCaption: string;
    procedure SetValue(const Value: Variant);
  public
    constructor Create(const AName, ACaption, AValue: string; const AValueType: TParameterType;
      const AIsLocal: Boolean);

    procedure SetStringValue(const AStrValue: string);

    property Name: string read FName;
    property Caption: string read FCaption;
    property IsLocal: Boolean read FIsLocal;
    property TextValue: string read FTextValue;
    property ValueType: TParameterType read FValueType;

    property Value: Variant read FValue write SetValue;
  end;

  TScriptParameter = class
  private
    FOwner: TCodeBlock;
    FValueType: TParameterType;
    FName: string;
    FValue: string;
    FParameterDef: TEntity;
    FCalcTree: TCalcTree;
    function GetValue: Variant;
  public
    constructor Create(const AOwner: TCodeBlock; const AParameterDef: TEntity; const AValue: string);
    destructor Destroy; override;

    function Compile: Boolean;

    property Name: string read FName;
    property Value: Variant read GetValue;
  end;

  TScriptCommand = class
  private
    FOwner: TCodeBlock;
    FCommandDef: TEntity;
    FParams: TStrings;
    FName: string;
    function GetParamValue(const AName: string): Variant;
  public
    constructor Create(const AOwner: TCodeBlock; const ACommandDef: TEntity);
    destructor Destroy; override;

    function AddParameter(const AParameterDef: TEntity; const AValue: string): TScriptParameter;
    function Compile: Boolean;

    property Owner: TCodeBlock read FOwner;
    property Name: string read FName;
    property Params[const AName: string]: Variant read GetParamValue; default;
  end;

  TCodeBlock = class(TCalcEnvironment)
  protected
    FID: Integer;
    FExecutor: TScriptExecutor;
    FOwner: TCodeBlock;
    FScriptName: string;
    FCommands: TObjectList<TScriptCommand>;
    FVariables: TObjectStringDictionary<TScriptVariable>;
    FMethods: TObjectStringDictionary<TCodeBlock>;
  protected
    function GetVariableValue(const AVarName: string): Variant; override;
    function DoExecuteFunction(const AFuncName: string; const AParams: array of Variant): Variant; override;
  public
    constructor Create(const AExecutor: TScriptExecutor; const AOwner: TCodeBlock; const AName: string);
    destructor Destroy; override;

    function Compile: Boolean;
    procedure SetVariableValue(const ATask: TTaskHandle; const ASession: TUserSession; const AVarName: string; const AValue: Variant);

    property ScriptName: string read FScriptName;
    property Commands: TObjectList<TScriptCommand> read FCommands;
    property Variables: TObjectStringDictionary<TScriptVariable> read FVariables;
    property Methods: TObjectStringDictionary<TCodeBlock> read FMethods;
    property VariableValue[const AVarName: string]: Variant read GetVariableValue;
  end;

  TTextCodeBlock = class(TCodeBlock)
  private
    FAddress: string;
    FAddresses: TStrings;
    FParams: TDictionary<string, string>;
    FPostponedParams: TList<TScriptParameter>;
  protected
    procedure AddVariable(const AText: string; const AIsLocal: Boolean);
    procedure AddCommand(const AName: string);
    function AddMethod(const AName: string): TTextCodeBlock;
    procedure ParseParamsString(const ACommandName, AParamsStr: string);
  public
    procedure ParseCode(const ACode: string);
  end;

  TEntityCodeBlock = class(TCodeBlock)
  public
    procedure ExtractData(const AScript: TEntity);
  end;

  TExecutionContext = class
  private
    FMethod: TCodeBlock;
    FNextCommandIndex: Integer;
    FIsDone: Boolean;
  public
    constructor Create(const AMethod: TCodeBlock);
    destructor Destroy; override;

    function CheckIsDone: Boolean;
    function GetCommandForExecution: TScriptCommand;
    procedure FillExternalVariables(const AVarList: TList);
    function MethodByName(const AName: string): TCodeBlock;
    procedure SetNextCommandIndex(const ACommandIndex: Integer);

    property IsDone: Boolean read CheckIsDone write FIsDone;
  end;

  TExecutionFiber = class
  private
    FExecutor: TScriptExecutor;
    FMethod: TCodeBlock;
    FSession: TUserSession;
    FOnCustomEvent: TCustomEvent;
    FOnScriptEnd: TScriptEndEvent;

    FExecutionContexts: TObjectStack<TExecutionContext>;

    FID: Integer;
    FScriptName: string;
    FWakeTime: Cardinal;

    function GetDomain: TObject;
  public
    constructor Create(const AExecutor: TScriptExecutor; const AMethod: TCodeBlock;
      const ASession: TUserSession; const AOnCustomEvent: TCustomEvent = nil;
      const AOnScriptEnd: TScriptEndEvent = nil);
    destructor Destroy; override;

    procedure Suspend;
    procedure Resume;

    function Interrupt: Boolean;

    function GetExternalVariables: TList;

    function ExecuteMethod(const AName: string): Boolean;
    function ExecuteCommand(const ATask: TTaskHandle): Boolean;
    procedure SetNextCommand(const ACommandIndex: Integer);

    property Domain: TObject read GetDomain;
    property Session: TUserSession read FSession;
    property WakeTime: Cardinal read FWakeTime write FWakeTime;
    property OnCustomEvent: TCustomEvent read FOnCustomEvent;
    property OnScriptEnd: TScriptEndEvent read FOnScriptEnd;

    property ScriptName: string read FScriptName;
    property Code: TCodeBlock read FMethod;
    property ID: Integer read FID;
  end;

  TExecutionRuntime = class
  private
    FTaskEngine: TTaskEngine;
    FExecutor: TScriptExecutor;
    FFibers: TList<TExecutionFiber>;
    FFibersLock: TCriticalSection;
    FTask: TTaskHandle;
    FStopTask: TTaskHandle;
    procedure Run;
  public
    constructor Create(const AExecutor: TScriptExecutor);
    destructor Destroy; override;

    procedure AddFiber(const AFiber: TExecutionFiber);
    procedure StopFiber(const AFiberID: Integer);
    procedure Stop;
  end;

  TScriptExecutor = class(TDomainModule)
  private
    FRuntime: TExecutionRuntime;
    FOnLogMessage: TLogMessageEvent;
    FInProcess: Boolean;

    FListeners: TList;
    FUseLogging: Boolean;
    function ExecuteCommand(const ATask: TTaskHandle; const AFiber: TExecutionFiber; const ACommand: TScriptCommand): Boolean;
    procedure WriteToLog(const AText: string);
  protected
    [Weak] FTaskEngine: TTaskEngine;
    procedure DoExecuteCommand(const ATask: TTaskHandle; const AFiber: TExecutionFiber;
      const ACommand: TScriptCommand); virtual;
  public
    constructor Create(const ADomain: TObject; const AName: string); override;
    destructor Destroy; override;

    function Prepare(const AScript: TEntity): TCodeBlock;
    function Execute(const AMainMethod: TCodeBlock; const ASession: TUserSession;
      const AOnCustomEvent: TCustomEvent = nil; const AOnScriptEnd: TScriptEndEvent = nil): Integer;
    procedure Stop(const ASession: TUserSession);

    procedure StopFiber(const ASession: TUserSession; const AFiberID: Integer);

    procedure PrintText(const ATask: TTaskHandle; const AText: string);
    procedure NotifyEnd(const ATask: TTaskHandle; const AFiber: TExecutionFiber);

    function ExecuteScenario(const ASession: TUserSession; const AScenario: TEntity;
      var AParams: TDictionary<string, Variant>; const AOnCustomEvent: TCustomEvent = nil;
      const AOnScriptEnd: TScriptEndEvent = nil): Integer; overload;
    function ExecuteScenario(const ASession: TUserSession; const AIdent: string;
      var AParams: TDictionary<string, Variant>; const AOnCustomEvent: TCustomEvent = nil;
      const AOnScriptEnd: TScriptEndEvent = nil): Integer; overload;
    procedure StopScenario(const ASession: TUserSession; const AFiberID: Integer);

    //procedure Subscribe(const AListener: TObject);
    //procedure Unsubscribe(const AListener: TObject);
    //procedure NotifyListeners(const ATask: TTaskHandle; const ASender: TObject; const AKind: Word; const AValue: Integer);

    property OnLogMessage: TLogMessageEvent read FOnLogMessage write FOnLogMessage;
    property InProcess: Boolean read FInProcess;
    property UseLogging: Boolean read FUseLogging write FUseLogging;
  end;

  TScriptInclusion = class(TInclusion)
  protected
    procedure DoInit; override;

    procedure DoCreateDefinitions; override;
    procedure DoCreateDefaultEntities(const ADomain: TObject; const AHolder: TChangeHolder); override;
    function DoCheckActionFlags(const AView: TView; const AActionName: string;
      const AContext: TObject; const AParams: TEntity): TViewState; override;
    function DoExecuteAction(const AView: TView; const AActionName: string;
      const AContext: TObject; const AParams: TEntity; const AParentHolder: TChangeHolder): Boolean; override;
  end;

implementation

uses
  Types, SysUtils, Variants, StrUtils, RegularExpressions, Math, DateUtils,
  uPlatform, uDomain, uDefinition, uCollection, uInteractor, uPresenter, uObjectField, uEntityList, uSettings;

type
  TExecuteCommandFunc = function(const AExecutor: TObject; const ATask: TTaskHandle; const AFiber, ACommand: TObject): Boolean of object;

const
  cParameterTypeCaptions: array[TParameterType] of string =
    ('Любой', 'Строка', 'Целое число', 'Дробное число', 'Дата/время', 'Булево', 'Объект', 'Команда', 'Текст');

procedure TScriptInclusion.DoInit;
begin
  FAppTitle := 'Скриптовой движок';
  FVersion := '0.1';
end;

// Aux procedures

function GetSiblingCommand(const ACommand: TEntity; const AIsPrevious: Boolean): TEntity;
var
  vCodeBlock: TEntity;
  vLastOrder: Integer;
  vThisOrder: Integer;
  vTestingOrder: Integer;
  vCommands: TListField;
  vCommand: TEntity;
  i: Integer;
begin
  Result := nil;
  vCodeBlock := ACommand.ExtractEntity('Block');
  if Assigned(vCodeBlock) then
  begin
    if AIsPrevious then
      vLastOrder := -1
    else
      vLastOrder := MaxInt;
    vThisOrder := ACommand['Order'];
    vCommands := TListField(vCodeBlock.FieldByName('Commands'));
    for i := 0 to vCommands.Count - 1 do
    begin
      vCommand := TEntity(vCommands[i]);
      if ACommand = vCommand then
        Continue;
      vTestingOrder := vCommand['Order'];

      if AIsPrevious then
      begin
        if (vTestingOrder > vLastOrder) and (vThisOrder > vTestingOrder) then
        begin
          Result := vCommand;
          vLastOrder := vTestingOrder;
        end;
      end
      else begin
        if (vTestingOrder < vLastOrder) and (vThisOrder < vTestingOrder) then
        begin
          Result := vCommand;
          vLastOrder := vTestingOrder;
        end;
      end;
    end
  end
end;

function ConvertCommandsToCode(const AScript: TEntity; const AAddrPrefix: string): string;
var
  vVariables: TListField;
  vVariable: TEntity;
  vCommands: TEntityList;
  vCommand: TEntity;
  vMethods: TListField;
  vMethod: TEntity;
  i: Integer;
  vTemp: string;
  vCommandDef: TEntity;
  vPointedCommands: TList<TEntity>;
  vParameters: TListField;
  vParameter: TEntity;
  vIndex: Integer;
  vCommandName: string;
  vParams: string;

  function CommandNameToCamelCase(const ACommandName: string): string;
  begin
    Result := ACommandName;
  end;

  function CreateVariableLine(const AVariable: TEntity): string;
  begin
    Result := AVariable['Name'] + ': ';
    case SafeID(AVariable.ExtractEntity('Type')) of
      1: Result := Result + 'String';
      2: Result := Result + 'Integer';
      3: Result := Result + 'Float';
      4: Result := Result + 'DateTime';
      5: Result := Result + 'Boolean';
      6: Result := Result + 'Object';
    end;

    if AVariable['Value'] <> '' then
    begin
      if SafeID(AVariable.ExtractEntity('Type')) in [1, 4] then
        Result := Result + ' = "' + AVariable['Value'] + '"'
      else
        Result := Result + ' = ' + AVariable['Value'];
    end;

    if AVariable['Caption'] <> '' then
      Result := Result + ' // ' + AVariable['Caption'];
  end;

  function ParameterByName(const ACommand: TEntity; const AName: string): TEntity;
  var
    vParams: TListField;
    vParameter: TEntity;
  begin
    vParams := TListField(ACommand.FieldByName('Parameters'));
    for vParameter in vParams do
      if SameText(vParameter.ExtractEntity('ParameterDefinition')['Name'], AName) then
        Exit(vParameter);
    Result := nil;
  end;

begin
  Result := '';
  vTemp := '';

  if AScript.InstanceOf('Scripts') then
  begin
    vVariables := TListField(AScript.FieldByName('Variables'));
    for vVariable in vVariables do
    begin
      if SafeID(vVariable.ExtractEntity('Destination')) = 2 then
        Result := Result + #13#10'external ' + CreateVariableLine(vVariable)
      else
        vTemp := vTemp + #13#10'var ' + CreateVariableLine(vVariable);
    end;

    if (Result <> '') and (vTemp <> '') then
      Result := Result + #13#10 + vTemp
    else
      Result := Result + vTemp;

    vMethods := TListField(AScript.FieldByName('Methods'));
    for i := 0 to vMethods.Count - 1 do
    begin
      vMethod := vMethods[i];
      vTemp := #13#10'define ' + vMethod['Name'] + ConvertCommandsToCode(vMethod, 'Sub' + IntToStr(i) + '_') + #13#10'end';
      if (Result <> '') and (vTemp <> '') then
        Result := Result + #13#10 + vTemp
      else
        Result := Result + vTemp;
    end;
  end;

  vPointedCommands := TList<TEntity>.Create;
  vCommands := TEntityList.Create(AScript.Domain, nil);
  try
    TListField(AScript.FieldByName('Commands')).GetEntityList(nil, vCommands);

    for vCommand in vCommands do
    begin
      vCommandDef := vCommand.ExtractEntity('CommandDefinition');
      if not Assigned(vCommandDef) then
        Continue;

      if (vCommandDef['Name'] = 'IF') or (vCommandDef['Name'] = 'GOTO') then
      begin
        vParameter := ParameterByName(vCommand, 'NextCommand');
        if Assigned(vParameter) and vParameter.Definition.IsDescendantOf('CommandParameterValues') then
          vPointedCommands.Add(vParameter.ExtractEntity('Value'));
      end;
    end;

    vTemp := '';
    for vCommand in vCommands do
    begin
      vIndex := vPointedCommands.IndexOf(vCommand);
      if vIndex >= 0 then
        vTemp := vTemp + #13#10'>>' + AAddrPrefix + IntToStr(vIndex);

      if vCommand['Description'] <> '' then
        vTemp := vTemp + #13#10'//' + ReplaceStr(vCommand['Description'], #13#10, ' ');

      vCommandDef := vCommand.ExtractEntity('CommandDefinition');
      if SameText(vCommandDef['Name'], 'IF') then
      begin
        vParameter := ParameterByName(vCommand, 'NextCommand');
        vIndex := vPointedCommands.IndexOf(vParameter.ExtractEntity('Value'));
        if vIndex >= 0 then
          vTemp := vTemp + #13#10 + 'if (' + ParameterByName(vCommand, 'Condition')['Value'] + ') >>' + AAddrPrefix + IntToStr(vIndex)
        else
          vTemp := vTemp + #13#10 + 'if (' + ParameterByName(vCommand, 'Condition')['Value'] + ') >>' + AAddrPrefix + '???';
      end
      else if SameText(vCommandDef['Name'], 'GOTO') then
      begin
        vParameter := ParameterByName(vCommand, 'NextCommand');
        vIndex := vPointedCommands.IndexOf(vParameter.ExtractEntity('Value'));
        if vIndex >= 0 then
          vTemp := vTemp + #13#10 + 'goto >>' + AAddrPrefix + IntToStr(vIndex)
        else
          vTemp := vTemp + #13#10 + 'goto >>' + AAddrPrefix + '???';
      end
      else if SameText(vCommandDef['Name'], 'ASSIGN') then
        vTemp := vTemp + #13#10 + ParameterByName(vCommand, 'Variable')['Value'] + ' := ' + ParameterByName(vCommand, 'Value')['Value']
      else begin
        vCommandName := CommandNameToCamelCase(vCommandDef['Name']);
        vParameters := TListField(vCommandDef.FieldByName('Parameters'));
        if vParameters.Count = 0 then
          vTemp := vTemp + #13#10 + vCommandName + '()'
        else if vParameters.Count = 1 then
        begin
          vParameter := vParameters[0];
          vTemp := vTemp + #13#10 + vCommandName + '(' + ParameterByName(vCommand, vParameter['Name'])['Value'] + ')';
        end
        else begin
          vParams := '';
          for vParameter in vParameters do
          begin
            if vParams <> '' then
              vParams := vParams + ', ';
            vParams := vParams + vParameter['Name'] + ': ' + ParameterByName(vCommand, vParameter['Name'])['Value'];
          end;
          vTemp := vTemp + #13#10 + vCommandName + '(' + vParams + ')';
        end;
      end;
    end;
  finally
    FreeAndNil(vPointedCommands);
    FreeAndNil(vCommands);
  end;

  if AScript.InstanceOf('Scripts') then
    Result := '// Скрипт "' + AScript['Name'] + '"'#13#10 + Result;

  if (Result <> '') and (vTemp <> '') then
    Result := Result + #13#10 + vTemp
  else
    Result := Result + vTemp;
end;

// Handlers

procedure OnCommandDefinitionChanged(const ASession: TUserSession; const AHolder: TChangeHolder;
  const AFieldChain: string; const AEntity, AParam: TEntity);
var
  vList: TListField;
  i: Integer;
  vParamDefs: TListField;
  vParamDef: TEntity;
  vNewParameter: TEntity;
begin
  vList := TListField(AEntity.FieldByName('Parameters'));
  for i := vList.Count - 1 downto 0 do
    vList.RemoveListEntity(AHolder, vList[i]);

  vParamDefs := TListField(AParam.FieldByName('Parameters'));
  for i := 0 to vParamDefs.Count - 1 do
  begin
    vParamDef := TEntity(vParamDefs[i]);
    if TParameterType(vParamDef['Type']) = ptCommand then
      vNewParameter := TEntity(vList.AddListEntity(AHolder, 'CommandParameterValues'))
    else begin
      vNewParameter := TEntity(vList.AddListEntity(AHolder, 'SimpleParameterValues'));
      vNewParameter._SetFieldValue(AHolder, 'Value', vParamDef['DefaultValue']);
    end;

    vNewParameter._SetFieldEntity(AHolder, 'ParameterDefinition', vParamDef);
  end;
end;

procedure OnCommandTextChanged(const ASession: TUserSession; const AHolder: TChangeHolder;
  const AFieldChain: string; const AEntity, AParam: TEntity);
var
  vText: string;
  vCommandDefinition: TEntity;
  vParamText: string;
  vList: TListField;
  i: Integer;
  vParamDef: TEntity;
  vParameter: TEntity;
begin
  vText := '';
  vCommandDefinition := AEntity.ExtractEntity('CommandDefinition');
  if Assigned(vCommandDefinition) then
    vText := vCommandDefinition['Name']
  else begin
    vText := '???';
    Exit;
  end;

  vParamText := '';
  vList := TListField(AEntity.FieldByName('Parameters'));
  for i := 0 to vList.Count - 1 do
  begin
    vParameter := TEntity(vList[i]);
    if i > 0 then
      vParamText := vParamText + ', ';
    vParamDef := vParameter.ExtractEntity('ParameterDefinition');
    if Assigned(vParamDef) then
    begin
      vParamText := vParamText + vParamDef['Name'] + ': ';
      if TParameterType(vParamDef['Type']) = ptCommand then
        vParamText := vParamText + SafeDisplayName(vParameter.ExtractEntity('Value'))
      else
        vParamText := vParamText + vParameter['Value'];
    end
    else
      vParamText := vParamText + '???: ';
  end;

  AEntity._SetFieldValue(AHolder, 'Text', '  ' + vText + '(' + vParamText + ');');
end;

procedure OnCodeBlockChanged(const ASession: TUserSession; const AHolder: TChangeHolder;
  const AFieldChain: string; const AEntity, AParam: TEntity);
var
  vList: TListField;
  i: Integer;
  vMaxOrder: Integer;
begin
  // Установка порядка следования команды
  if Assigned(AParam) then
  begin
    vMaxOrder := -1;
    vList := TListField(AParam.FieldByName('Commands'));
    for i := 0 to vList.Count - 1 do
      vMaxOrder := Max(vMaxOrder, TEntity(vList[i])['Order']);
    AEntity._SetFieldValue(AHolder, 'Order', vMaxOrder + 1);
  end
  else
    AEntity._SetFieldValue(AHolder, 'Order', -1);
end;

{ TScriptExecutor }

constructor TScriptExecutor.Create(const ADomain: TObject; const AName: string);
begin
  inherited Create(ADomain, AName);

  FTaskEngine := TTaskEngine(TDomain(ADomain).Module['TaskEngine']);
  FRuntime := TExecutionRuntime.Create(Self);
  FListeners := TList.Create;
  FOnLogMessage := WriteToLog;
  FInProcess := False;
  FUseLogging := True;
end;

destructor TScriptExecutor.Destroy;
begin
  FTaskEngine := nil;
  FOnLogMessage := nil;

  FreeAndNil(FListeners);
  FreeAndNil(FRuntime);

  inherited Destroy;
end;

procedure TScriptExecutor.DoExecuteCommand(const ATask: TTaskHandle; const AFiber: TExecutionFiber;
  const ACommand: TScriptCommand);
var
  vCommandName: string;
begin
  vCommandName := ACommand.Name;
  if vCommandName = 'SCRIPT_END' then
  begin
    Exit;
  end
//  FSession.AtomicWrite(ATask, procedure
//    begin
//      FEnvironment.Log('Execution of command ' + SafeDisplayName(ACommand.ExtractEntity('CommandDefinition')));
//    end);
end;

function TScriptExecutor.Execute(const AMainMethod: TCodeBlock; const ASession: TUserSession;
  const AOnCustomEvent: TCustomEvent = nil; const AOnScriptEnd: TScriptEndEvent = nil): Integer;
var
  vFiber: TExecutionFiber;
  vVarList: TList;
  vInteractor: TInteractor;
  vCanRun: Boolean;
  vSettings: TSettings;
  vSection: string;
  i: Integer;
  vVariable: TScriptVariable;
begin
  Result := 0;
  vCanRun := True;

  vFiber := TExecutionFiber.Create(Self, AMainMethod, ASession, AOnCustomEvent, AOnScriptEnd);
  try
    // Отображение диалога
    if Assigned(ASession.Interactor) then
    begin
      vVarList := vFiber.GetExternalVariables;
      try
        if vVarList.Count > 0 then
        begin
          vInteractor := TInteractor(ASession.Interactor);
          vSettings := TDomain(FDomain).UserSettings;
          vSection := 'Script' + IntToStr(AMainMethod.FID);
          for i := 0 to vVarList.Count - 1 do
          begin
            vVariable := TScriptVariable(vVarList[i]);
            if vSettings.KeyExists(vSection, vVariable.Name) then
              vVariable.SetStringValue(vSettings.GetValue(vSection, vVariable.Name, ''));
          end;

          vCanRun := Assigned(vInteractor.Presenter) and (TPresenter(vInteractor.Presenter).ShowPage(
            vInteractor, 'parameters', vVarList) = drOk);

          // SaveVariables
          for i := 0 to vVarList.Count - 1 do
          begin
            vVariable := TScriptVariable(vVarList[i]);
            vSettings.SetValue(vSection, vVariable.Name, VarToStr(vVariable.Value));
          end;
        end;
      finally
        FreeAndNil(vVarList);
      end;
    end;
  finally
    if vCanRun then
    begin
      FInProcess := True;
      Result := vFiber.ID;
      FRuntime.AddFiber(vFiber);
    end
    else
      FreeAndNil(vFiber);
  end;
end;

function TScriptExecutor.ExecuteCommand(const ATask: TTaskHandle; const AFiber: TExecutionFiber;
  const ACommand: TScriptCommand): Boolean;
var
  vCommandName: string;
  vEntity: TEntity;
  vObject: TEntity;
  vListField: TListField;
  vPresenter: TPresenter;
begin
  //PrintText(ATask, 'Выполняем команду: ' + vCommandName);

  Result := True;
  if TExecuteCommandFunc(TDomain(FDomain).Configuration.ExecuteCommandFunc)(Self, ATask, AFiber, ACommand) then
    Exit;

  vPresenter := _Platform.Presenter;

  vCommandName := ACommand.Name;
  if vCommandName = 'WAIT' then
    AFiber.WakeTime := TThread.GetTickCount + ACommand['MSec']
  else if vCommandName = 'PRINT' then
    PrintText(ATask, VarToStr(ACommand['s']))
  else if vCommandName = 'ASSIGN' then
    ACommand.Owner.SetVariableValue(ATask, AFiber.Session, ACommand['Variable'], ACommand['Value'])
  else if vCommandName = 'GOTO' then
    AFiber.SetNextCommand(ACommand['NextCommand'])
  else if vCommandName = 'IF' then
  begin
    if Boolean(ACommand['Condition']) then
      AFiber.SetNextCommand(ACommand['NextCommand']);
  end
  else if vCommandName = 'CALL' then
  begin
    AFiber.ExecuteMethod(ACommand['MethodName']);
  end
  else if vCommandName = 'MESSAGE' then
  begin
    if Assigned(vPresenter) then
      TTaskHandle.SafeInvoke(ATask, procedure
      var
        vFlag: TMessageType;
      begin
        if ACommand['IconID'] = 1 then
          vFlag := msWarning
        else if ACommand['IconID'] = 2 then
          vFlag := msError
        else
          vFlag := msInfo;

        vPresenter.ShowMessage(TDomain(FDomain).AppTitle, VarToStrDef(ACommand['Text'], ''), vFlag);
      end);
  end
  else if vCommandName = 'ASK' then
  begin
    TTaskHandle.SafeInvoke(ATask, procedure
    var
      vResult: Boolean;
    begin
      if Assigned(vPresenter) then
        vResult := vPresenter.ShowYesNoDialog(TDomain(FDomain).AppTitle, VarToStrDef(ACommand['Question'], '')) = drYes
      else
        vResult := False;
      ACommand.Owner.SetVariableValue(ATask, AFiber.Session, ACommand['Result'], vResult);
    end);
  end
  else if vCommandName = 'SHOW_LAYOUT' then
  begin
    if Assigned(vPresenter) then
      TTaskHandle.SafeInvoke(ATask, procedure
        begin
          vPresenter.ShowLayout(TInteractor(AFiber.Session.Interactor), ACommand['TargetArea'], ACommand['Layout']);
        end);
  end
  else if vCommandName = 'FIND_OBJECT' then
  begin
    vObject := TDomain(FDomain).FindOneEntity(ACommand['Collection'], AFiber.Session, ACommand['Query'], []);
    ACommand.Owner.SetVariableValue(ATask, AFiber.Session, ACommand['Result'], Integer(vObject));
  end
  else if vCommandName = 'ADD_LIST_ENTITY' then
  begin
    vEntity := TEntity(Integer(ACommand['Object']));
    vObject := nil;
    if Assigned(vEntity) then
    begin
      vListField := TListField(vEntity.FieldByName(ACommand['ListName']));
      if Assigned(vListField) then
        AFiber.Session.AtomicModification(ATask, function(const AHolder: TChangeHolder): Boolean
          begin
            vObject := vListField.AddListEntity(AHolder, '');
            Result := True;
          end);
    end;

    ACommand.Owner.SetVariableValue(ATask, AFiber.Session, ACommand['Result'], Integer(vObject));
  end
  else if vCommandName = 'SCRIPT_END' then
    Exit
  else
    Result := False;
end;

function TScriptExecutor.ExecuteScenario(const ASession: TUserSession; const AScenario: TEntity;
  var AParams: TDictionary<string, Variant>; const AOnCustomEvent: TCustomEvent; const AOnScriptEnd: TScriptEndEvent): Integer;
var
  vCodeBlock: TCodeBlock;
  vKey: string;
  vPresenter: TPresenter;
begin
  Assert(Assigned(ASession), 'Пользовательская сессия работы должна быть задана');
  vPresenter := _Platform.Presenter;

  vCodeBlock := Prepare(AScenario);
  if not Assigned(vCodeBlock) then
  begin
    if Assigned(vPresenter) then
      vPresenter.ShowMessage('Ошибка', Format('Не удалось скомпилировать сценарий [%s]...', [AScenario['Name']]), msError);
    Exit(0);
  end;

  if Assigned(AParams) then
  begin
    for vKey in AParams.Keys do
      vCodeBlock.SetVariableValue(nil, ASession, vKey, AParams[vKey]);
    FreeAndNil(AParams);
  end;

  Result := Execute(vCodeBlock, ASession, AOnCustomEvent, AOnScriptEnd);
end;

function TScriptExecutor.ExecuteScenario(const ASession: TUserSession; const AIdent: string;
  var AParams: TDictionary<string, Variant>; const AOnCustomEvent: TCustomEvent; const AOnScriptEnd: TScriptEndEvent): Integer;
var
  vDomain: TDomain;
  vScenario: TEntity;
  vPresenter: TPresenter;
begin
  Assert(Assigned(ASession), 'Пользовательская сессия работы должна быть задана');
  vDomain := TDomain(ASession.Domain);
  vPresenter := _Platform.Presenter;

  vScenario := vDomain.FindOneEntity('Scripts', ASession, 'Ident=:Ident', [AIdent]);

  if not Assigned(vScenario) then
  begin
    vPresenter.ShowMessage('Ошибка', Format('Сценарий [%s] не найден в системе!', [AIdent]), msError);
    Exit(0);
  end;

  Result := ExecuteScenario(ASession, vScenario, AParams, AOnCustomEvent, AOnScriptEnd);
end;

procedure TScriptExecutor.NotifyEnd(const ATask: TTaskHandle; const AFiber: TExecutionFiber);
var
  vScriptName: string;
begin
  vScriptName := AFiber.ScriptName;
  TTaskHandle.SafeInvoke(ATask, procedure
    begin
      FInProcess := False;
      if Assigned(FOnLogMessage) then
        FOnLogMessage(FormatDateTime('hh:nn:ss.zzz', Now) + '  <<< Скрипт ' + vScriptName + ' завершён');
      if Assigned(AFiber.OnScriptEnd) then
        AFiber.OnScriptEnd(AFiber);
    end);
end;

{procedure TScriptExecutor.NotifyListeners(const ATask: TTaskHandle; const ASender: TObject; const AKind: Word; const AValue: Integer);
var
  vMessage: TEventFiredMessage;
begin
  if FListeners.Count = 0 then
    Exit;

  vMessage.Msg := SM_EVENT_FIRED;
  vMessage.Kind := AKind;
  vMessage.Sender := ASender;
  vMessage.Parameter := AValue;

  FTaskEngine.SafeInvoke(ATask, procedure
    var
      i: Integer;
      vListener: TObject;
    begin
      for i := FListeners.Count - 1 downto 0 do
      begin
        vListener := TObject(FListeners[i]);
        vListener.Dispatch(vMessage);
      end;
    end);
end; }

function TScriptExecutor.Prepare(const AScript: TEntity): TCodeBlock;
begin
  PrintText(nil, '>>> Старт выполнения скрипта: ' + AScript.DisplayName);

  Result := TTextCodeBlock.Create(Self, nil, AScript['Name']);
  TTextCodeBlock(Result).ParseCode(AScript['Code']);
  Result.FID := AScript.ID;

  if not Result.Compile then
  begin
    TDomain(AScript.Domain).Log('Код не может быть скомпилирован...');
    FreeAndNil(Result);
  end;
end;

procedure TScriptExecutor.PrintText(const ATask: TTaskHandle; const AText: string);
begin
  if Assigned(FOnLogMessage) then
    TTaskHandle.SafeInvoke(ATask, procedure
      begin
        FOnLogMessage(FormatDateTime('hh:nn:ss.zzz', Now) + '  ' + AText);
      end);
end;

procedure TScriptExecutor.Stop(const ASession: TUserSession);
begin
  FRuntime.Stop;
  FInProcess := False;
end;

procedure TScriptExecutor.StopFiber(const ASession: TUserSession; const AFiberID: Integer);
begin
  FRuntime.StopFiber(AFiberID);
end;

procedure TScriptExecutor.StopScenario(const ASession: TUserSession; const AFiberID: Integer);
begin
  StopFiber(ASession, AFiberID);
end;

{procedure TScriptExecutor.Subscribe(const AListener: TObject);
begin
  FListeners.Add(AListener);
end;

procedure TScriptExecutor.Unsubscribe(const AListener: TObject);
begin
  FListeners.Remove(AListener);
end; }

procedure TScriptExecutor.WriteToLog(const AText: string);
var
  vLog: string;
  vVisualInfo: TEntity;
begin
  if not FUseLogging then
    Exit;

  vVisualInfo := TDomain(FDomain).FirstEntity('VisualState');
  if not Assigned(vVisualInfo) then
    Exit;

  vLog := vVisualInfo['ExecutionLog'];
  if vLog = '' then
    vLog := AText
  else
    vLog := AText + #13#10 + vLog;
  vVisualInfo._SetFieldValue(nil, 'ExecutionLog', vLog);
end;

{ TExecutionFiber }

constructor TExecutionFiber.Create(const AExecutor: TScriptExecutor; const AMethod: TCodeBlock;
  const ASession: TUserSession; const AOnCustomEvent: TCustomEvent; const AOnScriptEnd: TScriptEndEvent);
begin
  inherited Create;

  FExecutor := AExecutor;
  FMethod := AMethod;
  FSession := ASession;
  FOnCustomEvent := AOnCustomEvent;
  FOnScriptEnd := AOnScriptEnd;
  FWakeTime := 0;
  FID := Random(10000000) + 1;

  FExecutionContexts := TObjectStack<TExecutionContext>.Create;
  FExecutionContexts.Push(TExecutionContext.Create(AMethod));

  FScriptName := AMethod.ScriptName;
end;

destructor TExecutionFiber.Destroy;
begin
  FreeAndNil(FExecutionContexts);
  FreeAndNil(FMethod);
  FExecutor := nil;
  FSession := nil;
  FOnCustomEvent := nil;

  inherited Destroy;
end;

function TExecutionFiber.ExecuteCommand(const ATask: TTaskHandle): Boolean;
var
  vExecContext: TExecutionContext;
begin
  vExecContext := FExecutionContexts.Peek;

  if vExecContext.IsDone then
  begin
    FExecutionContexts.Pop;
    if FExecutionContexts.Count > 0 then
    begin
      vExecContext := FExecutionContexts.Peek;
      if vExecContext.IsDone then
        Exit(False);
    end
    else
      Exit(False);
  end;

  try
    Result := FExecutor.ExecuteCommand(ATask, Self, vExecContext.GetCommandForExecution);
  except
    on E: Exception do
    begin
      FExecutor.PrintText(ATask, 'Ошибка: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TExecutionFiber.ExecuteMethod(const AName: string): Boolean;
var
  vMethod: TCodeBlock;
  vExecContext: TExecutionContext;
begin
  vExecContext := FExecutionContexts.Peek;
  vMethod := vExecContext.MethodByName(AName);
  Result := Assigned(vMethod);
  if Result then
  begin
    FExecutionContexts.Push(TExecutionContext.Create(vMethod));
    Resume;
  end;
end;

function TExecutionFiber.GetDomain: TObject;
begin
  Result := FExecutor.Domain;
end;

function TExecutionFiber.GetExternalVariables: TList;
begin
  Result := TList.Create;
  if FExecutionContexts.Count > 0 then
    FExecutionContexts.Peek.FillExternalVariables(Result);
end;

function TExecutionFiber.Interrupt: Boolean;
var
  vExecContext: TExecutionContext;
begin
  vExecContext := FExecutionContexts.Peek;
  vExecContext.IsDone := True;

  Result := not ExecuteMethod('ON_STOP');
end;

procedure TExecutionFiber.Resume;
begin
  FWakeTime := 0;
end;

procedure TExecutionFiber.SetNextCommand(const ACommandIndex: Integer);
var
  vExecContext: TExecutionContext;
begin
  vExecContext := FExecutionContexts.Peek;
  vExecContext.SetNextCommandIndex(ACommandIndex);
end;

procedure TExecutionFiber.Suspend;
begin
  FWakeTime := INFINITE;
end;

{ TExecutionRuntime }

procedure TExecutionRuntime.AddFiber(const AFiber: TExecutionFiber);
var
  vFiberCount: Integer;
begin
  repeat until FFibersLock.TryEnter;
  try
    FFibers.Add(AFiber);
    vFiberCount := FFibers.Count;
  finally
    FFibersLock.Leave;
  end;

  if vFiberCount = 1 then
    Run;
end;

constructor TExecutionRuntime.Create(const AExecutor: TScriptExecutor);
begin
  inherited Create;
  FExecutor := AExecutor;
  FTaskEngine := TTaskEngine(TDomain(FExecutor.Domain).Module['TaskEngine']);
  FFibers := TList<TExecutionFiber>.Create;
  FFibersLock := TCriticalSection.Create;
end;

destructor TExecutionRuntime.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FTask);

  FExecutor := nil;
  FTaskEngine := nil;

  for i := 0 to FFibers.Count - 1 do
    FFibers[i].Free;
  FreeAndNil(FFibers);

  FreeAndNil(FFibersLock);

  inherited Destroy;
end;

{function TExecutionRuntime.GetFiber(const AIndex: Integer): TExecutionFiber;
begin
  Result := TExecutionFiber(FFibers[AIndex]);
end;}

procedure TExecutionRuntime.Run;
begin
  if Assigned(FTask) then
    FreeAndNil(FTask);

  FTask := FTaskEngine.Execute('ScriptExecutor', procedure(const ATask: TTaskHandle)
    var
      i: Integer;
      vFibers: TList<TExecutionFiber>;
      vFiber: TExecutionFiber;
      vWaiterCount: Integer;
      vFiberCount: Integer;
    begin
      vFibers := TList<TExecutionFiber>.Create;
      try
        while ATask.State = tskRunning do
        begin
          repeat until FFibersLock.TryEnter;
          try
            vFibers.AddRange(FFibers);
          finally
            FFibersLock.Leave;
          end;

          vWaiterCount := 0;
          for i := vFibers.Count - 1 downto 0 do
          begin
            vFiber := vFibers[i];
            if vFiber.WakeTime > TThread.GetTickCount then
            begin
              Inc(vWaiterCount);
              vFibers.Delete(i);
            end
            else begin
              // В списке останутся только те нити, которые нужно удалить
              if not vFiber.ExecuteCommand(ATask) then
                FExecutor.NotifyEnd(ATask, vFiber)
              else
                vFibers.Delete(i);
            end;
          end;

          repeat until FFibersLock.TryEnter;
          try
            for i := FFibers.Count - 1 downto 0 do
              if vFibers.Contains(FFibers[i]) then
              begin
                FFibers[i].Free;
                FFibers.Delete(i);
              end;
            vFiberCount := FFibers.Count;
          finally
            FFibersLock.Leave;
          end;

          if vFiberCount = 0 then
            Exit;

          if vWaiterCount = vFiberCount then
            ATask.Wait(100);

          vFibers.Clear;
        end;
      finally
        FreeAndNil(vFibers);
      end;
    end);
end;

procedure TExecutionRuntime.Stop;
begin
  //if Assigned(FTask) then
  //  FTask.Cancel_;
  if Assigned(FStopTask) then
    Exit;

  FStopTask := FTaskEngine.Execute('Script stopper', procedure(const ATask: TTaskHandle)
    var
      vFibers: TList<TExecutionFiber>;
      vFiber: TExecutionFiber;
      i: Integer;
    begin
      vFibers := TList<TExecutionFiber>.Create;
      try
        repeat until FFibersLock.TryEnter;
        try
          vFibers.AddRange(FFibers);
        finally
          FFibersLock.Leave;
        end;

        for i := vFibers.Count - 1 downto 0 do
        begin
          vFiber := vFibers[i];
          if vFiber.Interrupt then
            FExecutor.NotifyEnd(ATask, vFiber)
          else
            vFibers.Delete(i);
        end;

        repeat until FFibersLock.TryEnter;
        try
          for i := FFibers.Count - 1 downto 0 do
            if vFibers.Contains(FFibers[i]) then
            begin
              FFibers[i].Free;
              FFibers.Delete(i);
            end;
        finally
          FFibersLock.Leave;
        end
      finally
        FreeAndNil(vFibers);
      end;

      FStopTask := nil;
    end);
end;

procedure TExecutionRuntime.StopFiber(const AFiberID: Integer);
begin
  if Assigned(FStopTask) then
    Exit;

  FStopTask := FTaskEngine.Execute('Fiber stopper', procedure(const ATask: TTaskHandle)
    var
      vFiber: TExecutionFiber;
      vFound: TExecutionFiber;
      i: Integer;
    begin
      vFound := nil;

      repeat until FFibersLock.TryEnter;
      try
        for i := 0 to FFibers.Count - 1 do
        begin
          vFiber := FFibers[i];
          if vFiber.ID = AFiberID then
          begin
            if vFiber.Interrupt then
            begin
              vFound := vFiber;
              FFibers.Delete(i);
            end;
            Break;
          end;
        end;
      finally
        FFibersLock.Leave;
      end;

      if Assigned(vFound) then
      begin
        FExecutor.NotifyEnd(ATask, vFound);
        vFound.Free;
      end;

      FStopTask := nil;
    end);
end;

{ TScriptVariable }

constructor TScriptVariable.Create(const AName, ACaption, AValue: string; const AValueType: TParameterType;
  const AIsLocal: Boolean);
begin
  inherited Create;
  FName := AName;
  FCaption := ACaption;
  FValueType := AValueType;
  FIsLocal := AIsLocal;
  SetStringValue(AValue);
end;

procedure TScriptVariable.SetStringValue(const AStrValue: string);

  function TryGetString(const AValue: string): Variant;
  begin
    if StartsStr('''', AValue) and EndsStr('''', AValue) then
      Result := Copy(AValue, 2, Length(AValue) - 2)
    else
      Result := AValue;
  end;

  function TryGetInteger(const AValue: string): Variant;
  var
    vResult: Integer;
  begin
    if TryStrToInt(AValue, vResult) then
      Result := vResult
    else
      Result := Null;
  end;

  function ForcedTryStrToFloat(const AValue: string; out AResValue: Double): Boolean;
  const
    cNumSymbols: string = '+-0123456789';
  var
    vValue: string;
  begin
    Result := False;
    if AValue = '' then
      Exit
    else if Pos(Copy(AValue, 1, 1), cNumSymbols) = 0 then
      Exit
    else begin
      if FormatSettings.DecimalSeparator = '.' then
        vValue := ReplaceStr(AValue, ',', '.')
      else if FormatSettings.DecimalSeparator = ',' then
        vValue := ReplaceStr(AValue, '.', ',')
      else
        vValue := AValue;
      Result := TryStrToFloat(vValue, AResValue);
    end;
  end;

  function TryGetFloat(const AValue: string): Variant;
  var
    vResult: Double;
  begin
    if ForcedTryStrToFloat(AValue, vResult) then
      Result := vResult
    else
      Result := Null;
  end;

  // Число (Double), литерал даты в формате ISO8601 (2013-10-18T20:36:22.966Z)
  function TryGetDateTime(const AValue: string): Variant;
  var
    vResult: Double;
    vDateTime: TDateTime;
  begin
    if ForcedTryStrToFloat(AValue, vResult) then
      Result := vResult
    else if StartsStr('D''', AValue) and EndsStr('''', AValue) then
    begin
      if TryISO8601ToDate(AValue, vDateTime, False) then
        Result := vDateTime
      else
        Result := Null;
    end
    else
      Result := Null;
  end;

  // Числа (0.0 = False, остальное True), значения True и False
  function TryGetBoolean(const AValue: string): Variant;
  var
    vResult: Boolean;
  begin
    if TryStrToBool(AValue, vResult) then
      Result := vResult
    else
      Result := Null;
  end;

begin
  FTextValue := AStrValue;
  case FValueType of
    ptString, ptText: FValue := TryGetString(AStrValue);
    ptInteger: FValue := TryGetInteger(AStrValue);
    ptFloat: FValue := TryGetFloat(AStrValue);
    ptDateTime: FValue := TryGetDateTime(AStrValue);
    ptBoolean: FValue := TryGetBoolean(AStrValue);
  else
    FValue := Null;
  end;
end;

procedure TScriptVariable.SetValue(const Value: Variant);
begin
  if (FValueType = ptInteger) and VarIsFloat(Value) then
    FValue := Round(Value)
  else
    FValue := Value;
end;

{ TScriptParameter }

function TScriptParameter.Compile: Boolean;
begin
  Result := True;
  FCalcTree := CreateCalcTree(FValue, FParameterDef['ValuesOnly']);
end;

constructor TScriptParameter.Create(const AOwner: TCodeBlock; const AParameterDef: TEntity; const AValue: string);
begin
  inherited Create;

  FOwner := AOwner;
  FParameterDef := AParameterDef;

  FValueType := TParameterType(FParameterDef['Type']);
  FName := FParameterDef['Name'];
  FValue := AValue;
end;

destructor TScriptParameter.Destroy;
begin
  FreeAndNil(FCalcTree);
  FParameterDef := nil;
  FOwner := nil;
  inherited Destroy;
end;

function TScriptParameter.GetValue: Variant;
begin
  Result := FCalcTree.Calculate(FOwner);
end;

{ TScriptCommand }

function TScriptCommand.AddParameter(const AParameterDef: TEntity; const AValue: string): TScriptParameter;
begin
  Result := TScriptParameter.Create(FOwner, AParameterDef, AValue);
  FParams.AddObject(Result.Name, Result);
end;

function TScriptCommand.Compile: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FParams.Count - 1 do
    Result := Result and TScriptParameter(FParams.Objects[i]).Compile;
end;

constructor TScriptCommand.Create(const AOwner: TCodeBlock; const ACommandDef: TEntity);
begin
  inherited Create;

  FOwner := AOwner;
  FCommandDef := ACommandDef;
  FName := FCommandDef['Name'];
  FParams := TStringList.Create;
end;

destructor TScriptCommand.Destroy;
var
  i: Integer;
begin
  for i := 0 to FParams.Count - 1 do
    TScriptParameter(FParams.Objects[i]).Free;
  FreeAndNil(FParams);

  FOwner := nil;
  FCommandDef := nil;

  inherited Destroy;
end;

function TScriptCommand.GetParamValue(const AName: string): Variant;
var
  vIndex: Integer;
begin
  vIndex := FParams.IndexOf(AName);
  if vIndex >= 0 then
    Result := TScriptParameter(FParams.Objects[vIndex]).Value
  else
    Result := Null;
end;

{ TCodeBlock }

function TCodeBlock.Compile: Boolean;
var
  vCommand: TScriptCommand;
begin
  Result := True;
  for vCommand in FCommands do
    Result := Result and vCommand.Compile;
end;

constructor TCodeBlock.Create(const AExecutor: TScriptExecutor; const AOwner: TCodeBlock; const AName: string);
begin
  inherited Create;

  FExecutor := AExecutor;
  FOwner := AOwner;
  FScriptName := AName;
  FCommands := TObjectList<TScriptCommand>.Create;
  FVariables := TObjectStringDictionary<TScriptVariable>.Create;
  FMethods := TObjectStringDictionary<TCodeBlock>.Create;
end;

destructor TCodeBlock.Destroy;
begin
  FreeAndNil(FMethods);
  FreeAndNil(FVariables);
  FreeAndNil(FCommands);

  FOwner := nil;
  FExecutor := nil;

  inherited Destroy;
end;

function TCodeBlock.DoExecuteFunction(const AFuncName: string; const AParams: array of Variant): Variant;
var
  vParamsCount: Integer;
  vFuncName, vFormat: string;
  vPos: Integer;
  vVarName: string;
  vVariable: TScriptVariable;
  vObject: TEntity;
  vPath: string;
begin
  vParamsCount := Length(AParams);
  vFuncName := AnsiLowerCase(AFuncName);
  if (vFuncName = 'sin') and (vParamsCount = 1) then
    Result := Sin(AParams[0])
  else if (vFuncName = 'str') and (vParamsCount >= 1) then
  begin
    if VarIsOrdinal(AParams[0]) then
      Result := VarToStr(AParams[0])
    else
    begin
      if vParamsCount = 2 then
        vFormat := '0.' + DupeString('#', AParams[1])
      else
        vFormat := '0.##';
      Result := FormatFloat(vFormat, AParams[0]);
    end;
  end
  else if vFuncName = 'random' then
    Result := Random
  else if (vFuncName = 'assigned') and (vParamsCount = 1) then
    Result := Assigned(TObject(Integer(AParams[0])))
  else if (vFuncName = 'float') and (vParamsCount = 1) then
    Result := Double(AParams[0])
  else if (vFuncName = 'int') and (vParamsCount = 1) then
    Result := IfThen(VarIsNull(AParams[0]), 0, Round(AParams[0]))
  else if (vFuncName = 'abs') and (vParamsCount = 1) then
    Result := IfThen(VarIsNull(AParams[0]), 0, Abs(AParams[0]))
  else if (vParamsCount = 1) and VarIsOrdinal(AParams[0]) then
  begin
    // Возможно, это индексатор из модели
    Result := Null;
    vPos := Pos('.', AFuncName);
    if vPos > 0 then
    begin
      vVarName := Copy(AFuncName, 1, vPos - 1);
      vPath := Copy(AFuncName, vPos + 1, Length(AFuncName) - vPos);
      vVariable := FVariables.ObjectByName(vVarName);
      if Assigned(vVariable) and (Integer(vVariable.Value) > 0) then
      begin
        vObject := TEntity(Integer(vVariable.Value));
        Result := Integer(TListField(vObject.FieldByName(vPath))[Integer(AParams[0])]);
      end;
    end;
  end
  else
    Result := Null;
end;

function TCodeBlock.GetVariableValue(const AVarName: string): Variant;
var
  vVariable: TScriptVariable;
  vPos: Integer;
  vVarName, vPath: string;
  vObject: TEntity;
begin
  Result := Null;
  vVariable := FVariables.ObjectByName(AVarName);
  if Assigned(vVariable) then
    Result := vVariable.Value
  else begin
    vPos := Pos('.', AVarName);
    if vPos > 0 then
    begin
      vVarName := Copy(AVarName, 1, vPos - 1);
      vVariable := FVariables.ObjectByName(vVarName);
      if Assigned(vVariable) then
      begin
        vObject := TEntity(Integer(vVariable.Value));
        if Assigned(vObject) then
        begin
          vPath := Copy(AVarName, vPos + 1, Length(AVarName) - vPos);
          Result := vObject.ExtractFieldValue(vPath, False);
        end;
      end
      else if Assigned(FOwner) then
        Result := FOwner.GetVariableValue(AVarName);
    end
    else if Assigned(FOwner) then
      Result := FOwner.GetVariableValue(AVarName);
  end;
end;

procedure TCodeBlock.SetVariableValue(const ATask: TTaskHandle; const ASession: TUserSession; const AVarName: string;
  const AValue: Variant);
var
  vVariable: TScriptVariable;
  vPos: Integer;
  vVarName, vPath: string;
  vObject: TEntity;
  vValue: Variant;
begin
  if AVarName = '' then
    Exit;

  vVariable := FVariables.ObjectByName(AVarName);
  if Assigned(vVariable) then
    vVariable.Value := AValue
  else begin
    vPos := Pos('.', AVarName);
    if vPos > 0 then
    begin
      vVarName := Copy(AVarName, 1, vPos - 1);
      vVariable := FVariables.ObjectByName(vVarName);
      if Assigned(vVariable) then
      begin
        vObject := TEntity(Integer(vVariable.Value));
        if Assigned(vObject) then
        begin
          vPath := Copy(AVarName, vPos + 1, Length(AVarName) - vPos);
          vValue := AValue;
          if vObject.FieldExists(vPath) and Assigned(ASession) then
            ASession.AtomicModification(ATask, function(const AHolder: TChangeHolder): Boolean
              begin
                vObject._SetFieldValue(AHolder, vPath, vValue);
                Result := True;
              end);
        end;
      end
      else if Assigned(FOwner) then
        FOwner.SetVariableValue(ATask, ASession, AVarName, AValue);
    end
    else if Assigned(FOwner) then
      FOwner.SetVariableValue(ATask, ASession, AVarName, AValue);
  end;
end;

{ TTextCodeBlock }

procedure TTextCodeBlock.AddCommand(const AName: string);
var
  vCommandDef: TEntity;
  vParameterDef: TEntity;
  vParametersField: TListField;
  vCommand: TScriptCommand;
  vScriptParam: TScriptParameter;
  vValueType: TParameterType;
  vDomain: TDomain;
  vKey: string;
  vValue: string;
  i: Integer;
begin
  vDomain := TDomain(FExecutor.Domain);
  vCommandDef := vDomain.FindOneEntity('ScriptCommandDefinitions', vDomain.DomainSession, 'Name=:Name', [AName]);
  if not Assigned(vCommandDef) then
    Exit;

  vCommand := TScriptCommand.Create(Self, vCommandDef);

  vParametersField := TListField(vCommandDef.FieldByName('Parameters'));
  for i := 0 to vParametersField.Count - 1 do
  begin
    vParameterDef := vParametersField[i];
    vKey := vParameterDef['Name'];
    if not FParams.TryGetValue(vKey, vValue) then
      vValue := vParameterDef['DefaultValue'];
    vScriptParam := vCommand.AddParameter(vParameterDef, vValue);

    vValueType := TParameterType(vParameterDef['Type']);
    if vValueType = ptCommand then
      FPostponedParams.Add(vScriptParam);
  end;

  if FAddress <> '' then
  begin
    FAddresses.AddObject(FAddress, vCommand);
    FAddress := '';
  end;
  FCommands.Add(vCommand);
end;

function TTextCodeBlock.AddMethod(const AName: string): TTextCodeBlock;
begin
  Result := TTextCodeBlock.Create(FExecutor, Self, AName);
  FMethods.AddObject(AName, Result);
end;

procedure TTextCodeBlock.AddVariable(const AText: string; const AIsLocal: Boolean);
var
  vAST: TExpressionAST;
  vNode: TASTNode;
  vPrevText: string;
  vPrevSymbol: string;
  i: Integer;
  vNodeText: string;
  vName, vValue, vCaption: string;
  vType: TParameterType;
  vVariable: TScriptVariable;
begin
  vAST := TExpressionAST.Create;
  try
    vAST.Tokenize(AText);
    vPrevText := ''; vPrevSymbol := '';
    vName := ''; vCaption := ''; vValue := '';
    vType := ptAny;
    for i := 0 to vAST.RootNode.Count - 1 do
    begin
      vNode := vAST.RootNode[i];
      if (vNode.Kind = nkSpace) or (Trim(vNode.FullText) = '') then
        Continue;

      vNodeText := vNode.Text;
      if vNode.Kind = nkLiteral then
      begin
        if vPrevSymbol = '' then
        begin
          if vName = '' then
            vName := vNodeText;
        end
        else if vPrevSymbol = ':' then
        begin
          if vType = ptAny then
          begin
            if SameText(vNodeText, 'String') then
              vType := ptString
            else if SameText(vNodeText, 'Integer') then
              vType := ptInteger
            else if SameText(vNodeText, 'Float') then
              vType := ptFloat
            else if SameText(vNodeText, 'DateTime') then
              vType := ptDateTime
            else if SameText(vNodeText, 'Boolean') then
              vType := ptBoolean
            else if SameText(vNodeText, 'Text') then
              vType := ptText
            else if SameText(vNodeText, 'Object') then
              vType := ptObject;
          end;
        end
        else if vPrevSymbol = '=' then
        begin
          if vValue = '' then
            vValue := vNode.FullText;
        end;
      end
      else if vNode.Kind = nkParenthesis then
        // Распарсить диапазоны из vNode.ChildText
      else if vNode.Kind = nkComment then
      begin
        if vCaption = '' then
          vCaption := vNode.ChildText;
      end;

      vPrevSymbol := IfThen(vNode.Kind in [nkSymbol, nkOperator], vNodeText, '');
    end;

    if (vName <> '') and (vType <> ptAny) then
    begin
      vVariable := TScriptVariable.Create(vName, vCaption, vValue, vType, AIsLocal);
      FVariables.AddObject(vName, vVariable);
    end;
  finally
    FreeAndNil(vAST);
  end;
end;

procedure TTextCodeBlock.ParseCode(const ACode: string);
var
  i: Integer;
  vCode: TStrings;
  vSavedCommand: string;
  vCommandStr: string;
  vScriptParam: TScriptParameter;
  vAddressIndex: Integer;
  vPos: Integer;
  vCommandName: string;
  vTempText: string;
  vMethod: TCodeBlock;
  vMethodCode: TStrings;
begin
  FAddress := '';
  FAddresses := TStringList.Create;
  FPostponedParams := TList<TScriptParameter>.Create;
  FParams := TDictionary<string, string>.Create;

  vCode := TStringList.Create;
  vCode.Text := ACode;

  i := 0;
  vSavedCommand := '';
  try
    while i < vCode.Count do
    begin
      FParams.Clear;
      vCommandStr := Trim(vCode[i]);
      vSavedCommand := vCommandStr;
      Inc(i);
      if (vCommandStr = '') or (Copy(vCommandStr, 1, 2) = '//') then
        Continue;

      // Обработка адресов переходов
      if Copy(vCommandStr, 1, 2) = '>>' then
      begin
        FAddress := Trim(vCommandStr);
        Continue;
      end;

      // Регистрация внешних переменных
      if StartsText('external ', vCommandStr) then
      begin
        Delete(vCommandStr, 1, 9);
        AddVariable(Trim(vCommandStr), False);
        Continue;
      end;

      // Регистрация локальных переменных
      if StartsText('var ', vCommandStr) then
      begin
        Delete(vCommandStr, 1, 4);
        AddVariable(Trim(vCommandStr), True);
        Continue;
      end;

      // Регистрация локальных методов
      if StartsText('define ', vCommandStr) then
      begin
        Delete(vCommandStr, 1, 7);
        vMethod := AddMethod(Trim(vCommandStr));
        vMethodCode := TStringList.Create;
        try
          while i < vCode.Count do
          begin
            vCommandStr := Trim(vCode[i]);
            Inc(i);
            if not SameText(vCommandStr, 'end') then
              vMethodCode.Add(vCommandStr)
            else
              Break;
          end;
          TTextCodeBlock(vMethod).ParseCode(vMethodCode.Text);
          vMethod.Compile;
        finally
          FreeAndNil(vMethodCode);
        end;

        Continue;
      end;

      // Комментарии
      vPos := Pos('//', vCommandStr);
      if vPos > 0 then
        Delete(vCommandStr, 1, vPos - 1);

      // Команда ASSIGN
      vPos := Pos(':=', vCommandStr);
      if vPos > 0 then
      begin
        vTempText := Trim(Copy(vCommandStr, 1, vPos - 1));
        Delete(vCommandStr, 1, vPos + 1);
        FParams.Add('Variable', vTempText);
        FParams.Add('Value', Trim(vCommandStr));
        AddCommand('ASSIGN');
        Continue;
      end;

      vPos := Pos('(', vCommandStr);
      if vPos = 0 then
      begin
        vPos := Pos(' ', vCommandStr);
        if vPos > 0 then
        begin
          vCommandName := Trim(Copy(vCommandStr, 1, vPos - 1));
          if SameText(vCommandName, 'goto') then
          begin
            Delete(vCommandStr, 1, vPos);
            FParams.Add('NextCommand', Trim(vCommandStr));
            AddCommand(UpperCase(vCommandName));
          end;
        end;
        Continue;
      end;

      vCommandName := Trim(Copy(vCommandStr, 1, vPos - 1));
      Delete(vCommandStr, 1, vPos);
      vPos := LastDelimiter(')', vCommandStr);
      if vPos = 0 then
        raise Exception.Create('Отсутствует закрывающая скобка'); // Уведомить об ошибке парсинга
      vTempText := Trim(Copy(vCommandStr, 1, vPos - 1));

      if SameText(vCommandName, 'if') then
      begin
        Delete(vCommandStr, 1, vPos);
        FParams.Add('Condition', vTempText);
        FParams.Add('NextCommand', Trim(vCommandStr));
        vCommandName := UpperCase(vCommandName);
      end
      else
        ParseParamsString(vCommandName, vTempText);

      AddCommand(vCommandName);
    end;
  except
    on E: Exception do
      raise Exception.Create(Format('Произошла ошибка при обработке строки %d: [%s]. Причина: ',
        [i, vSavedCommand]) + E.Message);
  end;

  for vScriptParam in FPostponedParams do
  begin
    vAddressIndex := FAddresses.IndexOf(vScriptParam.FValue);
    vScriptParam.FValue := IntToStr(FCommands.IndexOf(TScriptCommand(FAddresses.Objects[vAddressIndex])));
  end;

  FreeAndNil(FAddresses);
  FreeAndNil(FPostponedParams);
  FreeAndNil(FParams);
  FreeAndNil(vCode);
end;

procedure TTextCodeBlock.ParseParamsString(const ACommandName, AParamsStr: string);
var
  vDomain: TDomain;
  vCommandDef: TEntity;
  vParametersField: TListField;
  vAST: TExpressionAST;
  vNode, vWaitedNode: TASTNode;
  i: Integer;
  vNodeText: string;
  vCheckForKey: Boolean;
  vWaitedNodes: TList<TASTNode>;
  vKey, vValue: string;
  vParamIndex: Integer;
begin
  vDomain := TDomain(FExecutor.Domain);
  vCommandDef := vDomain.FindOneEntity('ScriptCommandDefinitions', vDomain.DomainSession, 'Name=:Name', [ACommandName]);
  if not Assigned(vCommandDef) then
    Exit;

  vParametersField := TListField(vCommandDef.FieldByName('Parameters'));

  vAST := TExpressionAST.Create;
  vWaitedNodes := TList<TASTNode>.Create;
  try
    vAST.Tokenize(AParamsStr);
    vKey := '';
    vCheckForKey := True;
    vParamIndex := 0;
    for i := 0 to vAST.RootNode.Count - 1 do
    begin
      vNode := vAST.RootNode[i];
      if vNode.Kind = nkSpace then
        Continue;

      if Trim(vNode.FullText) = '' then
        Continue;

      vNodeText := vNode.Text;
      if vCheckForKey then
      begin
        if vWaitedNodes.Count = 0 then
          vWaitedNodes.Add(vNode)
        else if (vWaitedNodes.Count = 1) and (vNodeText = ':') then
        begin
          vKey := vWaitedNodes[0].FullText;
          vWaitedNodes.Clear;
          vCheckForKey := False;
        end
        else begin
          vWaitedNodes.Add(vNode);
          vCheckForKey := False;
        end;
      end
      else begin
        if vNodeText = ',' then
        begin
          VValue := '';
          for vWaitedNode in vWaitedNodes do
            vValue := vValue + vWaitedNode.FullText;
          vWaitedNodes.Clear;

          if vKey = '' then
            vKey := vParametersField[vParamIndex]['Name'];
          FParams.Add(vKey, vValue);

          vKey := '';
          vParamIndex := vParamIndex + 1;
          vCheckForKey := True;
        end
        else
          vWaitedNodes.Add(vNode);
      end;
    end;

    if (vWaitedNodes.Count > 0) or (vKey <> '') then
    begin
      vValue := '';
      for vWaitedNode in vWaitedNodes do
        vValue := vValue + vWaitedNode.FullText;

      if vKey = '' then
        vKey := vParametersField[vParamIndex]['Name'];
      FParams.Add(vKey, vValue);
    end;
  finally
    FreeAndNil(vWaitedNodes);
    FreeAndNil(vAST);
  end;
end;

{ TEntityCodeBlock }

procedure TEntityCodeBlock.ExtractData(const AScript: TEntity);
var
  vVariables: TListField;
  i: Integer;
  vVariableDef: TEntity;
  vVariable: TScriptVariable;
  vCommandIds: TStrings;
  vCommands: TEntityList;
  vCommandDef: TEntity;
  vCommand: TScriptCommand;
  vNextCommand: TEntity;
  vParams: TListField;
  vParameter: TEntity;
  vParameterDef: TEntity;
  vPostponedParams: TList<TScriptParameter>;
  vScriptParam: TScriptParameter;
  vValueType: TParameterType;
  vValue: string;
begin
  vVariables := TListField(AScript.FieldByName('Variables'));
  for i := 0 to vVariables.Count - 1 do
  begin
    vVariableDef := TEntity(vVariables[i]);
    vVariable := TScriptVariable.Create(Trim(vVariableDef['Name']), vVariableDef['Caption'],
      vVariableDef['Value'], TParameterType(SafeID(vVariableDef.ExtractEntity('Type'))),
      SafeID(vVariableDef.ExtractEntity('Destination')) <> 2);
    FVariables.AddObject(vVariableDef['Name'], vVariable);
  end;

  vCommandIds := TStringList.Create;
  vPostponedParams := TList<TScriptParameter>.Create;
  vCommands := TEntityList.Create(AScript.Domain, nil);
  try
    TListField(AScript.FieldByName('Commands')).GetEntityList(nil, vCommands);
    for i := 0 to vCommands.Count - 1 do
    begin
      vCommandDef := vCommands[i].ExtractEntity('CommandDefinition');
      Assert(Assigned(vCommandDef), 'Нет определения для команды');

      vCommand := TScriptCommand.Create(Self, vCommandDef);
      FCommands.Add(vCommand);
      vCommandIds.Add(IntToStr(SafeID(vCommands[i])));
      vParams := TListField(vCommands[i].FieldByName('Parameters'));
      for vParameter in vParams do
      begin
        vParameterDef := vParameter.ExtractEntity('ParameterDefinition');
        vValueType := TParameterType(vParameterDef['Type']);
        if vValueType = ptCommand then
        begin
          vNextCommand := vParameter.ExtractEntity('Value');
          vValue := IntToStr(SafeID(vNextCommand));
          vPostponedParams.Add(vCommand.AddParameter(vParameterDef, vValue));
        end
        else begin
          vValue := VarToStr(vParameter['Value']);
          vCommand.AddParameter(vParameterDef, vValue);
        end;
      end;
    end;

    for vScriptParam in vPostponedParams do
      vScriptParam.FValue := IntToStr(vCommandIds.IndexOf(vScriptParam.FValue));
  finally
    FreeAndNil(vCommands);
    FreeAndNil(vCommandIds);
    FreeAndNil(vPostponedParams);
  end;
end;

{ TExecutionContext }

function TExecutionContext.CheckIsDone: Boolean;
begin
  Result := FIsDone or (FMethod.Commands.Count <= FNextCommandIndex);
end;

constructor TExecutionContext.Create(const AMethod: TCodeBlock);
begin
  inherited Create;
  FMethod := AMethod;
  FNextCommandIndex := 0;
  FIsDone := FMethod.Commands.Count <= FNextCommandIndex;
end;

destructor TExecutionContext.Destroy;
begin
  FMethod := nil;
  inherited Destroy;
end;

procedure TExecutionContext.FillExternalVariables(const AVarList: TList);
var
  vVariable: TScriptVariable;
begin
  for vVariable in FMethod.Variables do
    if not vVariable.IsLocal and not (vVariable.ValueType in [ptObject, ptCommand]) then
      AVarList.Add(vVariable);
end;

function TExecutionContext.GetCommandForExecution: TScriptCommand;
var
  vNextCommandIndex: Integer;
begin
  vNextCommandIndex := FNextCommandIndex;
  Inc(FNextCommandIndex);
  Result := FMethod.Commands[vNextCommandIndex];
end;

function TExecutionContext.MethodByName(const AName: string): TCodeBlock;
begin
  Result := FMethod.Methods.ObjectByName(AName);
end;

procedure TExecutionContext.SetNextCommandIndex(const ACommandIndex: Integer);
begin
  if ACommandIndex >= 0 then
    FNextCommandIndex := ACommandIndex
  else
    FNextCommandIndex := MaxInt;
end;

{ TScriptInclusion }

function TScriptInclusion.DoCheckActionFlags(const AView: TView; const AActionName: string;
  const AContext: TObject; const AParams: TEntity): TViewState;
var
  vEntity: TEntity absolute AContext;
begin
  Result := inherited DoCheckActionFlags(AView, AActionName, AContext, AParams);
  if Result <> vsUndefined then
    Exit;

  if AActionName = 'StopScenario' then
  begin
    if Assigned(AParams) and (AParams['FiberID'] > 0) then
      Result := vsFullAccess
    else
      Result := vsDisabled;
  end;

  if Assigned(AContext) and (AContext is TEntity) then
  begin
    if vEntity.InstanceOf('ScriptCommands') then
    begin
      if AActionName = 'MoveUp' then
      begin
        if Assigned(GetSiblingCommand(vEntity, True)) then
          Result := vsFullAccess
        else
          Result := vsDisabled;
      end
      else if AActionName = 'MoveDown' then
      begin
        if Assigned(GetSiblingCommand(vEntity, False)) then
          Result := vsFullAccess
        else
          Result := vsDisabled;
      end
    end;
  end;
end;

procedure TScriptInclusion.DoCreateDefaultEntities(const ADomain: TObject; const AHolder: TChangeHolder);
var
  vDomain: TDomain absolute ADomain;
  vCollection: TCollection;
  vCommands, vParameters: TCollection;
  vCommand: TEntity;
begin
  inherited DoCreateDefaultEntities(ADomain, AHolder);

  // Скрипты
  vCollection := vDomain['VariableDestinations'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'Name', ['Внутренняя'], True);
  vCollection.CreateDefaultEntity(AHolder, 2, 'Name', ['Внешняя'], True);

  vCollection := vDomain['ParameterTypes'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'Name', ['Строка'], True);
  vCollection.CreateDefaultEntity(AHolder, 2, 'Name', ['Целое число'], True);
  vCollection.CreateDefaultEntity(AHolder, 3, 'Name', ['Дробное число'], True);
  vCollection.CreateDefaultEntity(AHolder, 4, 'Name', ['Дата/время'], True);
  vCollection.CreateDefaultEntity(AHolder, 5, 'Name', ['Булево'], True);
  vCollection.CreateDefaultEntity(AHolder, 6, 'Name', ['Объект'], True);
  vCollection.CreateDefaultEntity(AHolder, 7, 'Name', ['Команда'], True);
  vCollection.CreateDefaultEntity(AHolder, 8, 'Name', ['Tекст'], True);

  vCommands := vDomain['ScriptCommandDefinitions'];
  vParameters := vDomain['CommandParameterDefinitions'];
  vCommand := vCommands.CreateDefaultEntity(AHolder, 1, 'Name', ['PRINT']);
  vParameters.CreateDefaultEntity(AHolder, 1, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 's', ptString, '', False, '""']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 2, 'Name', ['WAIT']);
  vParameters.CreateDefaultEntity(AHolder, 2, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'MSec', ptInteger, '', False, '1']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 3, 'Name', ['ASSIGN']);
  vParameters.CreateDefaultEntity(AHolder, 3, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Variable', ptString, '', True, '']);
  vParameters.CreateDefaultEntity(AHolder, 4, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Value', ptString, '', False, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 4, 'Name', ['GOTO']);
  vParameters.CreateDefaultEntity(AHolder, 5, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'NextCommand', ptCommand, '', False, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 5, 'Name', ['IF']);
  vParameters.CreateDefaultEntity(AHolder, 6, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Condition', ptBoolean, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 7, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'NextCommand', ptCommand, '', False, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 6, 'Name', ['CALL']);
  vParameters.CreateDefaultEntity(AHolder, 8, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'MethodName', ptString, '', True, '']);

  vCommands.CreateDefaultEntity(AHolder, 7, 'Name', ['SCRIPT_END']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 8, 'Name', ['SHOW_LAYOUT']);
  vParameters.CreateDefaultEntity(AHolder, 9, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'TargetArea', ptString, '', False, '""']);
  vParameters.CreateDefaultEntity(AHolder, 10, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Layout', ptString, '', False, '""']);

  vCommands.CreateDefaultEntity(AHolder, 9, 'Name', ['STAND_PREPARE']);

  vCommands.CreateDefaultEntity(AHolder, 10, 'Name', ['STAND_STOP']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 11, 'Name', ['MEASURE_START']);
  vParameters.CreateDefaultEntity(AHolder, 11, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'ScenarioName', ptString, 'Имя сценария', False, '"Scenario1"']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 12, 'Name', ['MEASURE_SAVE']);
  vParameters.CreateDefaultEntity(AHolder, 12, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Filename', ptString, '', False, '']);

  vCommands.CreateDefaultEntity(AHolder, 13, 'Name', ['MEASURE_STOP']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 14, 'Name', ['COUNT_START']);
  vParameters.CreateDefaultEntity(AHolder, 13, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'tractname', ptString, '', False, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 15, 'Name', ['COUNT_SAVE']);
  vParameters.CreateDefaultEntity(AHolder, 14, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Filename', ptString, '', False, '']);

  vCommands.CreateDefaultEntity(AHolder, 16, 'Name', ['COUNT_STOP']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 17, 'Name', ['STEPPER_INIT']);
  vParameters.CreateDefaultEntity(AHolder, 100, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Stepper', ptObject, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 15, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Current', ptInteger, '', False, '500']);
  vParameters.CreateDefaultEntity(AHolder, 16, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'ClockwiseDirection', ptBoolean, '', False, 'True']);
  vParameters.CreateDefaultEntity(AHolder, 17, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'PositiveBit', ptInteger, '', False, '-1']);
  vParameters.CreateDefaultEntity(AHolder, 18, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'NegativeBit', ptInteger, '', False, '-1']);
  vParameters.CreateDefaultEntity(AHolder, 19, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Microstep', ptInteger, '', False, '1']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 18, 'Name', ['STEPPER_MOVE_N']);
  vParameters.CreateDefaultEntity(AHolder, 101, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Stepper', ptObject, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 20, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Speed', ptInteger, 'In Hz', False, '200']);
  vParameters.CreateDefaultEntity(AHolder, 21, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'StepCount', ptInteger, '', False, '1000']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 19, 'Name', ['STEPPER_MOVE_TILL']);
  vParameters.CreateDefaultEntity(AHolder, 102, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Stepper', ptObject, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 22, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Speed', ptInteger, 'In Hz', False, '200']);
  vParameters.CreateDefaultEntity(AHolder, 23, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'StopMask', ptInteger, '', False, '0']);
  vParameters.CreateDefaultEntity(AHolder, 24, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'StopValueMask', ptInteger, '', False, '0']);
  vParameters.CreateDefaultEntity(AHolder, 25, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'CompletedSteps', ptInteger, '', True, '0']);
  vParameters.CreateDefaultEntity(AHolder, 26, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'LimitStepCount', ptInteger, '', False, '0']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 20, 'Name', ['STEPPER_STOP']);
  vParameters.CreateDefaultEntity(AHolder, 103, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Stepper', ptObject, '', False, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 21, 'Name', ['FIND_OBJECT']);
  vParameters.CreateDefaultEntity(AHolder, 27, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Collection', ptString, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 28, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Query', ptString, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 29, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Result', ptObject, '', True, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 22, 'Name', ['MESSAGE']);
  vParameters.CreateDefaultEntity(AHolder, 30, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Text', ptString, '', False, '""']);
  vParameters.CreateDefaultEntity(AHolder, 31, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'IconID', ptInteger, '', False, '0']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 23, 'Name', ['ASK']);
  vParameters.CreateDefaultEntity(AHolder, 32, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Question', ptString, '', False, '""']);
  vParameters.CreateDefaultEntity(AHolder, 33, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Result', ptBoolean, '', True, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 30, 'Name', ['TRACT_START']);
  vParameters.CreateDefaultEntity(AHolder, 50, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Tract', ptObject, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 51, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Exposition', ptInteger, '', False, '500']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 31, 'Name', ['TRACT_STOP']);
  vParameters.CreateDefaultEntity(AHolder, 52, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Tract', ptObject, '', False, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 32, 'Name', ['TRACT_RESET']);
  vParameters.CreateDefaultEntity(AHolder, 53, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Tract', ptObject, '', False, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 33, 'Name', ['CORRECT_VOLTAGE']);
  vParameters.CreateDefaultEntity(AHolder, 54, 'CommandDefinition;!Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'CheckObject', ptObject, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 55, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'NeedContinue', ptBoolean, '', True, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 34, 'Name', ['TRACT_SAVE']);
  vParameters.CreateDefaultEntity(AHolder, 56, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Tract', ptObject, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 57, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'FileName', ptString, '', False, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 35, 'Name', ['COPY_SPECTRUM']);
  vParameters.CreateDefaultEntity(AHolder, 58, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Source', ptObject, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 59, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'SourceField', ptString, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 60, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Destination', ptObject, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 61, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'DestField', ptString, '', False, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 36, 'Name', ['CHECK_INTENSITY']);
  vParameters.CreateDefaultEntity(AHolder, 62, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'CheckObject', ptObject, '', False, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 37, 'Name', ['CHECK_BACKGROUND']);
  vParameters.CreateDefaultEntity(AHolder, 63, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'CheckObject', ptObject, '', False, '']);

  vCommand := vCommands.CreateDefaultEntity(AHolder, 40, 'Name', ['ADD_LIST_ENTITY']);
  vParameters.CreateDefaultEntity(AHolder, 70, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Result', ptObject, '', True, '']);
  vParameters.CreateDefaultEntity(AHolder, 71, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'Object', ptObject, '', False, '']);
  vParameters.CreateDefaultEntity(AHolder, 72, 'CommandDefinition;Name;Type;Description;ValuesOnly;DefaultValue',
    [Integer(vCommand), 'ListName', ptString, '', False, '']);



//  vCollection := vDomain['SysSecuredObjects'];
//  AddSecuritySettings(vCollection, AHolder, 'Administrators', 'Comparer', '', vsDisabled);

  vCollection := vDomain['VisualState'];
  vCollection.CreateDefaultEntity(AHolder, 1, 'ExecutionLog', ['']);
end;

procedure TScriptInclusion.DoCreateDefinitions;
var
  vDefinition: TDefinition;
  vActionDef: TActionDef;
begin
  inherited DoCreateDefinitions;

  AddEnumeration<TParameterType>('ParameterTypes').AddDisplayNames(cParameterTypeCaptions);

  AddAction('ClearLog', 'Очистить журнал', 1006);
  AddAction('StopAll', 'Остановить все скрипты', 1007);

  vActionDef := AddAction('StopScenario', 'Прекратить', 1003, ccHideInMenu);
  vActionDef.AddSimpleFieldDef('FiberID', 'fiber_id', 'Ид. скрипта', 0, 1, Null, fkInteger, '', '', vsHidden);

  vDefinition := AddDefinition('VariableDestinations', '', 'Назначение переменной', 'Любое', ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess);
  vDefinition.AddUniqueIndex('Name@');

  vDefinition := AddDefinition('ParameterTypes', '', 'Тип значения параметров', 'Любой', ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess);
  vDefinition.AddUniqueIndex('Name@');

  vDefinition := AddDefinition('ScriptCommandDefinitions', '', 'Описание команд скрипта', cNullItemName, ccHideInMenu).SetImageID(1008);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 150, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddListFieldDef('Parameters', 'CommandDefinition', 'Параметры', 'CommandParameterDefinitions', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddUniqueIndex('Name@');

  vDefinition := AddDefinition('CommandParameterDefinitions', '', 'Описание параметров команд', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('CommandDefinition', 'command_definition', 'Описание', '', 'ScriptCommandDefinitions', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddSimpleFieldDef('Type', 'type', 'Тип', ptAny, Null, Null, fkEnum, '', '', vsFullAccess, cRequired, 'ParameterTypes');
  vDefinition.AddSimpleFieldDef('Description', 'description', 'Описание', Null, Null, 255, fkString, 'memo', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('ValuesOnly', 'values_only', 'Только значения', False, Null, Null, fkBoolean);
  vDefinition.AddSimpleFieldDef('DefaultValue', 'default_value', 'Значение по умолчанию', Null, Null, 255, fkString);
  vDefinition.AddUniqueIndex('CommandDefinition,Name@');

  vDefinition := AddDefinition('CodeBlocks', '', 'Скриптовые блоки', cNullItemName, 0, clkMixin);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 150, fkString, '', '', vsFullAccess);
  vDefinition.AddListFieldDef('Commands', 'Block', 'Команды', 'ScriptCommands', '', '', vsFullAccess, 0, estSortByOrder, '', rpStrong);
  vDefinition.AddSimpleFieldDef('Code', 'code', 'Код скрипта', Null, Null, 16000, fkString, 'log', '', vsFullAccess);

  vDefinition := AddDefinition('Scripts', 'CodeBlocks', 'Сценарии', cNullItemName).SetImageID(1000);
  vDefinition.AddSimpleFieldDef('Ident', 'ident', 'Уникальный идентификатор', Null, Null, 50, fkString, '', '', vsFullAccess, cRequired);
  vDefinition.AddListFieldDef('Variables', 'Script', 'Переменные', 'ScriptVariables', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddListFieldDef('Methods', 'Script', 'Методы', 'ScriptMethods', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddUniqueIndex('Name@;Ident@');
  vDefinition.AddAction('RunScript', 'Запустить', 1001);
  vDefinition.AddAction('ConvertToCode', 'Преобразовать в код', 1002);
  vDefinition.AddAction('Clone', 'Клонировать', 26);

  vDefinition := AddDefinition('ScriptMethods', 'CodeBlocks', 'Скриптовые методы', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('Script', 'script', 'Скрипт', '', 'Scripts', 0, vsHidden);
  vDefinition.AddUniqueIndex('Name@,Script');

  vDefinition := AddDefinition('ScriptVariables', '', 'Переменные скрипта', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('Script', 'script', 'Скрипт', '', 'Scripts', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('Name', 'name', 'Наименование', Null, Null, 150, fkString, '', '', vsFullAccess);
  vDefinition.AddSimpleFieldDef('Caption', 'caption', 'Заголовок', Null, Null, 150, fkString, '', '', vsFullAccess);
  vDefinition.AddEntityFieldDef('Destination', 'destination', 'Назначение', '', 'VariableDestinations', 1, vsSelectOnly, cRequired, estSortByID);
  vDefinition.AddEntityFieldDef('Type', 'type', 'Тип', '', 'ParameterTypes', 0, vsSelectOnly, cRequired, estSortByID);
  vDefinition.AddSimpleFieldDef('Value', 'value', 'Значение', Null, Null, 255, fkString);
  vDefinition.AddUniqueIndex('Name@,Script');

  vDefinition := AddDefinition('ScriptCommands', '', 'Команды скрипта', cNullItemName, ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Text', 'text', 'Текстовка', Null, Null, 255, fkString, '', '', vsFullAccess);
  vDefinition.AddEntityFieldDef('Block', 'block', 'Скриптовый блок', '', 'CodeBlocks', 0, vsHidden);
  vDefinition.AddSimpleFieldDef('Order', 'order', 'Порядок следования', -1, 0, Null, fkInteger, '', '', vsReadOnly);
  vDefinition.AddEntityFieldDef('CommandDefinition', 'command_definition', 'Команда', '', 'ScriptCommandDefinitions', 0, vsSelectOnly, cRequired);
  vDefinition.AddListFieldDef('Parameters', 'Command', 'Параметры', 'ParameterValues', '', '', vsFullAccess, 0, estUserSort, '', rpStrong);
  vDefinition.AddSimpleFieldDef('Description', 'description', 'Описание', Null, Null, 255, fkString, 'memo', '', vsFullAccess);
  vDefinition.AddAction('MoveUp', 'Сдвинуть вверх', 1004);
  vDefinition.AddAction('MoveDown', 'Сдвинуть вниз', 1005);
  RegisterReaction('ScriptCommands', 'Parameters', 'CommandDefinition', OnCommandDefinitionChanged);
  RegisterReaction('ScriptCommands', 'Text', 'CommandDefinition;Value.Parameters', OnCommandTextChanged);
  RegisterReaction('ScriptCommands', 'Order', 'Block', OnCodeBlockChanged);
  vDefinition.RegisterReaction('Name', 'Text;Description', TProc(procedure(const ASession: TUserSession; const AHolder: TChangeHolder;
      const AFieldChain: string; const AEntity, AParameter: TEntity)
    begin
      AEntity._SetFieldValue(AHolder, 'Name', AEntity['Text'] + '  ' + AEntity['Description']);
    end));

  vDefinition := AddDefinition('ParameterValues', '', 'Значения параметров', cNullItemName, ccHideInMenu, clkMixin);
  vDefinition.AddEntityFieldDef('Command', 'command', 'Команда', '', 'ScriptCommands', 0, vsHidden);
  vDefinition.AddEntityFieldDef('ParameterDefinition', 'parameter_definition', 'Параметр', '', 'CommandParameterDefinitions', 0, vsReadOnly, cRequired);
  vDefinition.AddSimpleFieldDef('Value', 'value', 'Значение', Null, Null, 1, fkString);

  vDefinition := AddDefinition('SimpleParameterValues', 'ParameterValues', 'Строковые параметры', cNullItemName, ccHideInMenu);
  vDefinition.AddSimpleFieldDef('Value', 'value', 'Значение', Null, Null, 255, fkString);

  vDefinition := AddDefinition('CommandParameterValues', 'ParameterValues', 'Значения переходов', cNullItemName, ccHideInMenu);
  vDefinition.AddEntityFieldDef('Value', 'value', 'Команда', '', 'ScriptCommands', 0, vsSelectOnly, 0, estSortByOrder, '', 'Block=Command.Block');

  vDefinition := AddDefinition('VisualState', '', '', '', ccNotSave);
  vDefinition.AddSimpleFieldDef('ExecutionLog', 'execution_log', 'Журнал выполнения', Null, Null, -1, fkString, 'log', '', vsReadOnly, cNotSave);
end;

function TScriptInclusion.DoExecuteAction(const AView: TView; const AActionName: string;
  const AContext: TObject; const AParams: TEntity; const AParentHolder: TChangeHolder): Boolean;
var
  vEntity: TEntity absolute AContext;
  vInteractor: TInteractor;
  vSession: TUserSession;
  vDomain: TDomain;
  vExecutor: TScriptExecutor;
  vSiblingCommand: TEntity;
  vCode: string;
begin
  Result := inherited DoExecuteAction(AView, AActionName, AContext, AParams, AParentHolder);
  if Result then
    Exit;

  vInteractor := TInteractor(AView.Interactor);
  vDomain := TDomain(vInteractor.Domain);
  vSession := TUserSession(vInteractor.Session);
  vExecutor := TScriptExecutor(vDomain.Module['ScriptExecutor']);

  Result := True;

  if AActionName = 'StopAll' then
  begin
    vExecutor.Stop(vSession);
    Exit;
  end
  else if not Assigned(AContext) then
    Exit(False);

  if AContext is TEntityList then
    Exit(False);

  if vEntity.InstanceOf('Scripts') then
  begin
    if AActionName = 'ConvertToCode' then
    begin
      vCode := ConvertCommandsToCode(vEntity, 'Addr_');
      vSession.AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
        begin
          vEntity._SetFieldValue(AHolder, 'Code', vCode);
          Result := True;
        end);
    end
    else if AActionName = 'Clone' then
    begin
      vInteractor.AtomicEditEntity(function(const AHolder: TObject): TView
        var
          vNewScript: TEntity;
          vParentObject: TObject;
        begin
          vDomain.IsAlive := False;
          try
            vNewScript := vEntity.Clone(AHolder, 'Code;Ident');
          finally
            vDomain.IsAlive := True;
          end;
          vParentObject := AView.Parent.ParentDomainObject;
          if not Assigned(vParentObject) or not (vParentObject is TEntityList) then
            Result := vInteractor.GetViewOfEntity(vNewScript)
          else begin
            TEntityList(vParentObject).SelectEntity(vNewScript);
            Result := AView.Parent;
          end;
        end, AParentHolder);
    end
    else
      Exit(False);
  end
  else if vEntity.CollectionName = 'ScriptCommands' then
  begin
    if (AActionName = 'MoveUp') or (AActionName = 'MoveDown') then
    begin
      vSiblingCommand := GetSiblingCommand(vEntity, AActionName = 'MoveUp');
      if not Assigned(vSiblingCommand) then
        Exit;

      vSession.AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
        var
          vTemp: Integer;
        begin
          vTemp := vSiblingCommand['Order'];
          vSiblingCommand._SetFieldValue(AHolder, 'Order', vEntity['Order']);
          vEntity._SetFieldValue(AHolder, 'Order', vTemp);
          Result := True;
        end);

      // Обновить сортировку
      GetParentListView(AView).NotifyUI(dckFieldChanged, vEntity);
    end
    else
      Result := False;
  end
  {else if (AActionName = 'ExecuteScenario') then
  begin
    if Assigned(AParams) and Assigned(AParams.ExtractEntity('Scenario')) then
    begin
      vScriptParams := TDictionary<string, Variant>.Create;

      vFiberID := ExecuteScenario(vSession, AParams.ExtractEntity('Scenario'), vScriptParams);

      vActionView := AView.Parent.BuildView('StopScenario');
      TEntity(vActionView.DomainObject)._SetFieldValue(nil, 'FiberID', vFiberID);
    end;
  end}
  else if (AActionName = 'StopScenario') then
  begin
    if Assigned(AParams) and (AParams['FiberID'] > 0) then
    begin
      vExecutor.StopScenario(vSession, AParams['FiberID']);
      TEntity(AView.DomainObject)._SetFieldValue(nil, 'FiberID', 0);
    end;
  end
  else
    Result := False;
end;

initialization

TBaseModule.RegisterModule('ScriptExecutor', 'Base', TScriptExecutor);
TBaseModule.RegisterModule('ScriptExecutor', 'ASW', TScriptExecutor);
RegisterInclusion('ScriptExecutor', TScriptInclusion);

end.
