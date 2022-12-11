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

unit uScheduler;

interface

uses
  Classes, Generics.Collections, SyncObjs, uReaction, uTask;

type
  TSchedulerProc = procedure(const ADomain: TObject);

  TWaitingJob = class
  private
    [Weak] FDomain: TObject;
    [Weak] PlannedJob: TPlannedJob;
    NextActivationTime: TDateTime;
    Busy: Boolean;
  public
    constructor Create(const ADomain: TObject; const APlannedJob: TPlannedJob);

    procedure CalcNextActivationTime(const ANow: TDateTime);
  end;

  TScheduler = class
  private
    [Weak] FDomain: TObject;
    [Weak] FTaskEngine: TTaskEngine;
    FWaitingJobs: TThreadList<TWaitingJob>;
    FInterval: Integer;
    FTask: TTaskHandle;
    procedure InsertOrdered(const AWaitingList: TList<TWaitingJob>; const AJob: TWaitingJob);
    procedure RunScheduler;
  public
    constructor Create(const ADomain: TObject; const AInterval: Integer);
    destructor Destroy; override;

    procedure ScheduleAll;
    procedure CancelAll;
  end;

implementation

uses
  SysUtils, DateUtils, Math, uDomain, Types;

{ TScheduler }

procedure TScheduler.CancelAll;
var
  i: Integer;
  vWaitingJobs: TList<TWaitingJob>;
  vJob: TWaitingJob;
begin
  vWaitingJobs := FWaitingJobs.LockList;
  try
    for i := vWaitingJobs.Count - 1 downto 0 do
    begin
      { TODO -owa : Проверить, чтобы задача не исполнялась прямо сейчас (подождать?? или что?) }
      if True then
      begin
        vJob := vWaitingJobs.Extract(vWaitingJobs[i]);
        FreeAndNil(vJob);
      end;
    end;
  finally
    FWaitingJobs.UnlockList;
  end;
end;

constructor TScheduler.Create(const ADomain: TObject; const AInterval: Integer);
begin
  inherited Create;

  FDomain := ADomain;
  FTaskEngine := TTaskEngine(TDomain(ADomain).Module['TaskEngine']);
  FWaitingJobs := TThreadList<TWaitingJob>.Create;
  FInterval := AInterval;
end;

destructor TScheduler.Destroy;
begin
  FreeAndNil(FTask);

  FreeAndNil(FWaitingJobs);
  FDomain := nil;

  inherited Destroy;
end;

procedure TScheduler.InsertOrdered(const AWaitingList: TList<TWaitingJob>; const AJob: TWaitingJob);
var
  i: Integer;
begin
  for i := 0 to AWaitingList.Count - 1 do
    if AJob.NextActivationTime < AWaitingList[i].NextActivationTime then
    begin
      AWaitingList.Insert(i, AJob);
      Exit;
    end;
  AWaitingList.Add(AJob);
end;

procedure TScheduler.RunScheduler;
begin
  FTask := FTaskEngine.Execute('Scheduler', procedure(const ATask: TTaskHandle)
    var
      vWaitingJobs: TList<TWaitingJob>;
      vJob: TWaitingJob;
      vPlannedJob: TPlannedJob;
      vNextActivationTime: TDateTime;
      vNextWakeUpInterval: Integer;
    begin
      while ATask.State = tskRunning do
      begin
        vWaitingJobs := FWaitingJobs.LockList;
        try
          vNextActivationTime := 0;
          while vWaitingJobs.Count > 0 do
          begin
            vJob := vWaitingJobs[0];
            vNextActivationTime := vJob.NextActivationTime;
            if vNextActivationTime > Now then
              Break;

            vJob := vWaitingJobs.Extract(vJob);
            vPlannedJob := vJob.PlannedJob;
            try
              vJob.Busy := True;
              TTaskHandle.SafeInvoke(ATask, procedure
                begin
                  try
                    TDomain(FDomain).Logger.AddMessage('### Execution of schedulled procedure: ' + vPlannedJob.Name);
                    TSchedulerProc(vPlannedJob.Handler)(FDomain);
                  except
                    on E: Exception do
                    begin
                      TDomain(FDomain).Logger.AddMessage('Ошибка при выполнении процедуры планировщика. Причина: ' + E.Message);
                    end;
                  end;
                end);
            finally
              vJob.Busy := False;
            end;

            vJob.CalcNextActivationTime(Now);
            InsertOrdered(vWaitingJobs, vJob);

            vNextActivationTime := 0;
          end;
        finally
          FWaitingJobs.UnlockList;
        end;

        // Рассчитываем время следующего просыпания
        vNextWakeUpInterval := FInterval * 1000;
        if vNextActivationTime > 0 then
          vNextWakeUpInterval := Min(vNextWakeUpInterval, Round((vNextActivationTime - Now) * MSecsPerDay));

        if vNextWakeUpInterval > 0 then
          ATask.Wait(vNextWakeUpInterval);
      end;
    end);
end;

procedure TScheduler.ScheduleAll;
var
  vPlannedJob: TPlannedJob;
  vJob: TWaitingJob;
  vWaitingJobs: TList<TWaitingJob>;
begin
  vWaitingJobs := FWaitingJobs.LockList;
  try
    for vPlannedJob in TDomain(FDomain).Configuration.PlannedJobs do
    begin
      vJob := TWaitingJob.Create(FDomain, vPlannedJob);
      InsertOrdered(vWaitingJobs, vJob);
    end;
  finally
    FWaitingJobs.UnlockList;
  end;

  if TDomain(FDomain).Configuration.PlannedJobs.Count > 0 then
    RunScheduler;
end;

{ TWaitingJob }

procedure TWaitingJob.CalcNextActivationTime(const ANow: TDateTime);
//ANow - время последнего исполнения
var
  vPeriodInSec: Cardinal;
  vNextSeconds: Cardinal;
begin
  if Assigned(FDomain) and (PlannedJob.RefFieldName <> '') then
  begin
    vPeriodInSec := StrToIntDef(TDomain(FDomain).Constant[PlannedJob.RefFieldName], -1);
    if vPeriodInSec <= 0 then
      vPeriodInSec := PlannedJob.PeriodInSec
  end
  else
    vPeriodInSec := PlannedJob.PeriodInSec;

  if not PlannedJob.SnapToPeriod then
    NextActivationTime := ANow + vPeriodInSec / SecsPerDay
  else begin
    vNextSeconds := (SecondOfTheDay(ANow) div vPeriodInSec + 1) * vPeriodInSec;
    NextActivationTime := Int(ANow) + vNextSeconds / SecsPerDay;
  end;
end;

constructor TWaitingJob.Create(const ADomain: TObject; const APlannedJob: TPlannedJob);
begin
  inherited Create;
  FDomain := ADomain;
  PlannedJob := APlannedJob;
  CalcNextActivationTime(Now);
  Busy := False;
end;

end.
