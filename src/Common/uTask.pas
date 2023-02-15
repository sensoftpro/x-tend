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

unit uTask;

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, uModule;

type
  TTaskEngine = class;
  TTaskHandle = class;

  TTaskState = (tskInvalid, tskCreated, tskWaitingToRun, tskRunning, tskCompleted, tskWaitingForChildren, tskCanceled, tskError);

  TExecutionProc = reference to procedure(const ATask: TTaskHandle);

  ETaskException = class(Exception)
  end;

  TTaskHandle = class
  private
    FName: string;
    FCancellationToken: TEvent;
    FCompletionToken: TEvent;
    [Volatile] FState: Integer;

    procedure ExecuteAsync;

    function SetState(const AOldState, ANewState: TTaskState): Boolean;
    function GetState: TTaskState;
  protected
    //function DoCreateTask(const AExecutionProc: TExecutionProc): Pointer; virtual; abstract;
    //function DoGetTaskState: TTaskState; virtual; abstract;
    //procedure DoDestroyTask; virtual; abstract;
    //procedure DoStart; virtual; abstract;
    procedure DoExecute; virtual;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure Start(const ATaskEngine: TTaskEngine);
    function Wait(const APeriod: Cardinal): Boolean;
    procedure Cancel;

    class procedure SafeInvoke(const ATask: TTaskHandle; const AProc: TThreadProcedure); static;

    property State: TTaskState read GetState;
    property Name: string read FName;
  end;

  TLambdaTask = class(TTaskHandle)
  private
    FExecutionProc: TExecutionProc;
  protected
    procedure DoExecute; override;
  public
    constructor Create(const AName: string; const AExecutionProc: TExecutionProc);
    destructor Destroy; override;
  end;

  TSimpleTaskEngine = class;

  {TCalc<T> = class(TTask)
  protected
    function GetValue<T>: T; virtual;
  public
    property Value: T read GetValue<T>;
  end;}

  TExecutor = class(TThread)
  private
    FEngine: TSimpleTaskEngine;
    FCurrentTask: TTaskHandle;
  protected
    procedure Execute; override;
    procedure CancelTask;
  public
    constructor Create(const AEngine: TSimpleTaskEngine);
    destructor Destroy; override;
  end;

  TTaskEngine = class(TDomainModule)
  private
    FLambdas: TObjectList<TTaskHandle>;
    procedure NotifyCompleted(const ATask: TTaskHandle);
  protected
    procedure DoExecuteTask(const ATask: TTaskHandle); virtual; abstract;
  public
    constructor Create(const ADomain: TObject; const AName: string); override;
    destructor Destroy; override;

    // Creation and launch for an execution
    function Execute(const AName: string; const AExecutionProc: TExecutionProc): TTaskHandle;
    procedure ExecuteManaged(const AName: string; const AExecutionProc: TExecutionProc);
  end;

  TSimpleTaskEngine = class(TTaskEngine)
  private
    FAvailableThreadCount: Integer;
    FThreads: TObjectList<TExecutor>;
    FTasks: TList<TTaskHandle>;
    FTaskLock: TObject;
    FDestroying: Boolean;
    procedure CreateExecutor;
  private
    function GetTask: TTaskHandle;

    procedure AcquireThread;
    procedure ReleaseThread;
    procedure QueueTask(const ATask: TTaskHandle);
  protected
    procedure DoExecuteTask(const ATask: TTaskHandle); override;
  public
    constructor Create(const ADomain: TObject; const AName: string); override;
    destructor Destroy; override;
  end;

  {TPPLTask = class(TTaskHandle)
  protected
    function DoCreateTask(const AExecutionProc: TExecutionProc): Pointer; override;
    function DoGetTaskState: TTaskState; override;
    procedure DoDestroyTask; override;
  end;

  TPPLTaskEngine = class(TTaskEngine)
  public
    constructor Create(const ADomain: TObject); override;
    destructor Destroy; override;

    function CreateTask(const AExecutionProc: TExecutionProc): TTaskHandle; override;
    function CreateTypedTask(const ATypeName: string): TTaskHandle; override;
    function Execute(const AExecutionProc: TExecutionProc): TTaskHandle; override;
    function GetTaskState(const ATask: TTaskHandle): TTaskState; override;
    procedure SafeInvoke(const ATask: TTaskHandle; const AProc: TThreadProcedure); override;
    procedure CancelTask(const ATask: TTaskHandle); override;
    function WaitTask(const ATask: TTaskHandle; const APeriod: Cardinal): Boolean; override;
    procedure FinalizeTask(var ATask: TTaskHandle); override;
  end; }

  TTaskEngineClass = class of TTaskEngine;

implementation

uses
  Types, Math, Threading;

const
  cThreadCount = 4;

{ TExecutor }

procedure TExecutor.CancelTask;
begin
  // TODO: Sychronize FCurrentTask
  if Assigned(FCurrentTask) then
    FCurrentTask.Cancel;
end;

constructor TExecutor.Create(const AEngine: TSimpleTaskEngine);
begin
  inherited Create(True);
  FEngine := AEngine;
  FCurrentTask := nil;
end;

destructor TExecutor.Destroy;
begin
  FCurrentTask := nil;
  FEngine := nil;
  inherited Destroy;
end;

procedure TExecutor.Execute;
begin
  repeat
    // Бесконечно ожидаем "боевую" задачу на исполнение, или nil-задачу для завершения
    FCurrentTask := FEngine.GetTask;
    if Assigned(FCurrentTask) then
    begin
      //Notify('takes a task');
      TThread.Current.NameThreadForDebugging(FCurrentTask.Name);
      FEngine.AcquireThread;
      try
        FCurrentTask.ExecuteAsync;
      finally
        //Notify('task executed');
        FEngine.NotifyCompleted(FCurrentTask);
        FCurrentTask := nil;
        FEngine.ReleaseThread;
      end;
    end
    else
      Terminate;
  until Terminated;
end;

{ TSimpleTaskEngine }

procedure TSimpleTaskEngine.AcquireThread;
var
  vAvailableThreads: Integer;
begin
  vAvailableThreads := AtomicDecrement(FAvailableThreadCount);
  if vAvailableThreads <= 0 then
    CreateExecutor;
end;

constructor TSimpleTaskEngine.Create(const ADomain: TObject; const AName: string);
var
  i: Integer;
  vRunnableThreadCount: Integer;
begin
  inherited Create(ADomain, AName);

  FDestroying := False;
  FAvailableThreadCount := 0;

  vRunnableThreadCount := CPUCount;
  if vRunnableThreadCount < cThreadCount then
    vRunnableThreadCount := cThreadCount;

  FTasks := TList<TTaskHandle>.Create;
  FTaskLock := TObject.Create;

  FThreads := TObjectList<TExecutor>.Create;
  for i := 1 to vRunnableThreadCount do
    CreateExecutor;
end;

procedure TSimpleTaskEngine.CreateExecutor;
var
  vExecutor: TExecutor;
begin
  if FDestroying then
    Exit;

  AtomicIncrement(FAvailableThreadCount);

  vExecutor := TExecutor.Create(Self);
  FThreads.Add(vExecutor);
  vExecutor.Start;
end;

destructor TSimpleTaskEngine.Destroy;
var
  vThread: TExecutor;
begin
  FDestroying := True;

  MonitorEnter(FTaskLock);
  try
    // Отменяем все невыполненные задачи
    FTasks.Clear;
  finally
    MonitorExit(FTaskLock);
  end;

  // Дождаться выполнения задач, уже исполняемых в потоках
  for vThread in FThreads do
  begin
    // Ставим признак выхода
    vThread.Terminate;
    // Принудительно завершаем задачу, если она есть
    vThread.CancelTask;
    // Если мы в ожидании, добавим деблокирующую ожидание nil-задачу
    QueueTask(nil);
  end;

  for vThread in FThreads do
    vThread.WaitFor;

  FreeAndNil(FThreads);

  FreeAndNil(FTasks);
  FreeAndNil(FTaskLock);

  inherited Destroy;
end;

procedure TSimpleTaskEngine.DoExecuteTask(const ATask: TTaskHandle);
begin
  if not FDestroying then
    QueueTask(ATask);
end;

function TSimpleTaskEngine.GetTask: TTaskHandle;
begin
  if FDestroying then
    Exit(nil);

  MonitorEnter(FTaskLock);
  try
    while FTasks.Count = 0 do
      MonitorWait(FTaskLock, INFINITE);

    Result := FTasks[0];
    FTasks.Delete(0);
  finally
    MonitorExit(FTaskLock);
  end;
end;

procedure TSimpleTaskEngine.QueueTask(const ATask: TTaskHandle);
begin
  MonitorEnter(FTaskLock);
  try
    FTasks.Add(ATask);
    MonitorPulse(FTaskLock);
  finally
    MonitorExit(FTaskLock);
  end;
end;

procedure TSimpleTaskEngine.ReleaseThread;
begin
  AtomicIncrement(FAvailableThreadCount);
end;

{ TTaskHandle }

procedure TTaskHandle.Cancel;
var
  vWaitResult: TWaitResult;
begin
  if not SetState(tskRunning, tskCanceled) then
    Exit;

  CheckSynchronize;
  FCancellationToken.SetEvent;

  vWaitResult := wrTimeout;
  while vWaitResult <> wrSignaled do
    vWaitResult := FCompletionToken.WaitFor(1000);
end;

constructor TTaskHandle.Create(const AName: string);
begin
  inherited Create;

  FName := AName;
  FState := Integer(tskCreated);
  FCancellationToken := TEvent.Create(nil, False, False, '');
  FCompletionToken := TEvent.Create(nil, False, False, '');
end;

destructor TTaskHandle.Destroy;
begin
  Cancel;
  FreeAndNil(FCompletionToken);
  FreeAndNil(FCancellationToken);
  inherited Destroy;
end;

procedure TTaskHandle.DoExecute;
begin
end;

// Выполняется в потоке
procedure TTaskHandle.ExecuteAsync;
begin
  if not SetState(tskWaitingToRun, tskRunning) then
    Exit;

  try
    DoExecute;
    SetState(tskRunning, tskCompleted);
  except
    on E: Exception do
      SetState(tskRunning, tskError);
  end;

  FCompletionToken.SetEvent;
end;

function TTaskHandle.GetState: TTaskState;
begin
  Result := TTaskState(FState);
end;

class procedure TTaskHandle.SafeInvoke(const ATask: TTaskHandle; const AProc: TThreadProcedure);
begin
  // TODO: Проверить статусы
  if not Assigned(ATask) then
    AProc()
  else if ATask.State = tskRunning then
  begin
    // Выполняется синхронно
    TThread.Synchronize(nil, procedure
      begin
        if ATask.State in [tskRunning, tskCompleted] then
          AProc();
      end);
    if ATask.State = tskCanceled then
      raise ETaskException.Create('Task is canceled after synchronization');
  end;
end;

function TTaskHandle.SetState(const AOldState, ANewState: TTaskState): Boolean;
begin
  TInterlocked.CompareExchange(FState, Integer(ANewState), Integer(AOldState), Result);
end;

procedure TTaskHandle.Start(const ATaskEngine: TTaskEngine);
begin
  if Assigned(ATaskEngine) then
  begin
    if SetState(tskCreated, tskWaitingToRun) then
      ATaskEngine.DoExecuteTask(Self);
  end
  else
    DoExecute;
end;

function TTaskHandle.Wait(const APeriod: Cardinal): Boolean;
begin
  Result := FCancellationToken.WaitFor(APeriod) = wrTimeout;
  // Произошла отмена во время ожидания
  if not Result then
    raise ETaskException.Create('Task is canceled while waiting');
end;

{ TTaskEngine }

constructor TTaskEngine.Create(const ADomain: TObject; const AName: string);
begin
  inherited Create(ADomain, AName);
  FLambdas := TObjectList<TTaskHandle>.Create;
end;

destructor TTaskEngine.Destroy;
begin
  FreeAndNil(FLambdas);
  inherited Destroy;
end;

function TTaskEngine.Execute(const AName: string; const AExecutionProc: TExecutionProc): TTaskHandle;
begin
  Result := TLambdaTask.Create(AName, AExecutionProc);
  Result.Start(Self);
end;

procedure TTaskEngine.ExecuteManaged(const AName: string; const AExecutionProc: TExecutionProc);
var
  vTask: TLambdaTask;
begin
  vTask := TLambdaTask.Create('Managed:' + AName, AExecutionProc);
  FLambdas.Add(vTask);
  vTask.Start(Self);
end;

procedure TTaskEngine.NotifyCompleted(const ATask: TTaskHandle);
begin
  FLambdas.Remove(ATask);
end;

{ TLambdaTask }

constructor TLambdaTask.Create(const AName: string; const AExecutionProc: TExecutionProc);
begin
  inherited Create(AName);
  FExecutionProc := AExecutionProc;
end;

destructor TLambdaTask.Destroy;
begin
  FExecutionProc := nil;
  inherited Destroy;
end;

procedure TLambdaTask.DoExecute;
begin
  if Assigned(FExecutionProc) then
    FExecutionProc(Self);
end;

initialization
  TBaseModule.RegisterModule('TaskEngine', '', 'SimpleEngine', TSimpleTaskEngine);
  TBaseModule.RegisterModule('TaskEngine', '', 'Native', TSimpleTaskEngine);

end.
