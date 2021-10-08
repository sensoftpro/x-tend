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

unit uReaction;

interface

uses
  Classes, SysUtils, Generics.Collections, uFastClasses;

type
  TSchedulerHandler = Pointer;

  TPlannedJob = class
  private
    FName: string;
    FPeriod: Integer; // Секунды
    FHandler: TSchedulerHandler;
    FRefFieldName: string;
    FSnapToPeriod: Boolean;
  public
    constructor Create(const AName: string; const APeriod: Integer; const AHandler: TSchedulerHandler;
      const ARefFieldName: string; const ASnapToPeriod: Boolean);

    property Name: string read FName;
    property Period: Integer read FPeriod;
    property Handler: TSchedulerHandler read FHandler;
    property RefFieldName: string read FRefFieldName;
    property SnapToPeriod: Boolean read FSnapToPeriod;
  end;

  TPlannedJobs = TObjectList<TPlannedJob>;

  TReactionHandler = TProc;
  THandlers = TList<TReactionHandler>;

  TReactions = class
  private
    FOwner: TObject;
    FAllReactions: THandlers;
    FChainHandlers: TObjectStringDictionary<THandlers>;
    procedure AddReaction(const AFieldChain: string; const AReactionProc: TReactionHandler);
  public
    constructor Create(const AOwner: TObject);
    destructor Destroy; override;

    function FindReactions(const AFieldChain: string): THandlers;
    procedure RegisterReaction(const AFieldChains: TStrings; const AReactionProc: TReactionHandler);
    property AllReactions: THandlers read FAllReactions;
  end;

implementation

uses
  uUtils, uConfiguration, uDefinition;

{ TReactions }

procedure TReactions.AddReaction(const AFieldChain: string; const AReactionProc: TReactionHandler);
var
  vHandlers: THandlers;

  procedure InternalAddReaction(const AFieldChain: string; const ADefinition: TDefinition;
    const APrevField: TFieldDef; const AReactionProc: TReactionHandler);
  var
    vPath: TStrings;
    vContentDefinition: TDefinition;
    vDefinition: TDefinition;
    vFieldName: string;
    vFieldDef: TFieldDef;
  begin
    if Trim(AFieldChain) = '' then
      Exit;

    vPath := CreateDelimitedList(AFieldChain, '.');
    try
      vFieldName := vPath[vPath.Count - 1];
      vFieldDef := ADefinition.FieldByName(vFieldName);
      if not Assigned(vFieldDef) then
        Exit;

      vPath.Delete(vPath.Count - 1);
      if not Assigned(APrevField) then
        vFieldDef.FNotificationChains.AddListener(vPath.DelimitedText, ADefinition.Name, AReactionProc)
      else
        vFieldDef.FNotificationChains.AddTransit(vPath.DelimitedText, APrevField);

      if vPath.Count > 0 then
      begin
        vContentDefinition := TConfiguration(ADefinition.Configuration).DefinitionByName[TObjectFieldDef(vFieldDef).ContentDefinitionName];
        for vDefinition in vContentDefinition.InnerDefinitions do
          InternalAddReaction(vPath.DelimitedText, vDefinition, vFieldDef, AReactionProc);
      end;
    finally
      FreeAndNil(vPath);
    end;
  end;

begin
  if not FChainHandlers.TryGetObject(AFieldChain, vHandlers) then
  begin
    vHandlers := THandlers.Create;
    FChainHandlers.AddObject(AFieldChain, vHandlers)
  end;
  vHandlers.Add(AReactionProc);

  // Кэширование путей уведомлений
  InternalAddReaction(AFieldChain, TDefinition(FOwner), nil, AReactionProc);
end;

constructor TReactions.Create(const AOwner: TObject);
begin
  inherited Create;

  FOwner := AOwner;
  FAllReactions := THandlers.Create;
  FChainHandlers := TObjectStringDictionary<THandlers>.Create;
end;

destructor TReactions.Destroy;
begin
  FOwner := nil;
  FreeAndNil(FChainHandlers);
  FreeAndNil(FAllReactions);

  inherited Destroy;
end;

function TReactions.FindReactions(const AFieldChain: string): THandlers;
begin
  Result := FChainHandlers.ObjectByName(AFieldChain);
end;

procedure TReactions.RegisterReaction(const AFieldChains: TStrings; const AReactionProc: TReactionHandler);
var
  vChain: string;
begin
  FAllReactions.Add(AReactionProc);

  for vChain in AFieldChains do
    AddReaction(vChain, AReactionProc);
end;

{ TPlannedJob }

constructor TPlannedJob.Create(const AName: string; const APeriod: Integer; const AHandler: TSchedulerHandler;
  const ARefFieldName: string; const ASnapToPeriod: Boolean);
begin
  inherited Create;

  FName := AName;
  FPeriod := APeriod;
  FHandler := AHandler;
  FRefFieldName := ARefFieldName;
  FSnapToPeriod := ASnapToPeriod;
end;

end.
