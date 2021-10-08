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

unit uProcessMediator;

interface

uses
  Classes, SyncObjs, Threading;

type
  TOnDataReceivedEvent = procedure(const AData: string) of object;

  TProcessMediator = class
  private
    FFileName: string;
    FEventName: string;
    FAlreadyExists: Boolean;
  {$IFDEF MSWINDOWS}
    FFileMapping: THandle;
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

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils;

{ TProcessMediator }

constructor TProcessMediator.Create(const AUniqueName: string; const ADataSize: Integer);
begin
  inherited Create;

  FFileName := AUniqueName + '$File';
  FEventName := AUniqueName + '$Event';

{$IFDEF MSWINDOWS}
  FFileMapping := OpenFileMapping(FILE_MAP_WRITE or FILE_MAP_READ, False, PChar(FFileName));
  FAlreadyExists := FFileMapping <> 0;
{$ELSE}
  FAlreadyExists := False;
{$ENDIF}

  if not FAlreadyExists then
  begin
{$IFDEF MSWINDOWS}
    FFileMapping := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, ADataSize, PChar(FFileName));
    if FFileMapping = 0 then
      FBaseAddress := nil
    else
      FBaseAddress := MapViewOfFile(FFileMapping, FILE_MAP_WRITE or FILE_MAP_READ, 0, 0, 0);
{$ELSE}
    FBaseAddress := nil;
{$ENDIF}

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
{$IFDEF MSWINDOWS}
    FBaseAddress := MapViewOfFile(FFileMapping, FILE_MAP_WRITE or FILE_MAP_READ, 0, 0, 0);
{$ELSE}
    FBaseAddress := nil;
{$ENDIF}
    FEvent := nil;
    FWaitingTask := nil;
  end;
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

{$IFDEF MSWINDOWS}
  if Assigned(FBaseAddress) then
    UnmapViewOfFile(FBaseAddress);
  if FFileMapping <> 0 then
    CloseHandle(FFileMapping);
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
