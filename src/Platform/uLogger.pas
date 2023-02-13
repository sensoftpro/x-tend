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

unit uLogger;

interface

uses
  Classes, uConsts, uModule;

type
  TLogger = class(TDomainModule)
  private
    FItems: TStrings;
    FFileName: string;
    FLogIndentLevel: Integer;
    FLogMessageKind: TMessageKind;
    FEnabled: Boolean;
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): string;
  public
    constructor Create(const ADomain: TObject; const AName: string); override;
    destructor Destroy; override;

    function AddMessage(const AMessage: string; const AMessageKind: TMessageKind = mkAny): string;
    function AddEnterMessage(const AMessage: string; const AMessageKind: TMessageKind = mkAny): string;
    function AddExitMessage(const AMessage: string; const AMessageKind: TMessageKind = mkAny): string;

    //procedure DisableLogging;
    //procedure EnableLogging;
    procedure SetTarget(const AFileName: string);
    procedure Flush;
    property Enabled: Boolean read FEnabled write FEnabled;

    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]: string read GetItem; default;
    property Lines: TStrings read FItems;
  end;

implementation

uses
  SysUtils;

{ TLogger }

function TLogger.AddEnterMessage(const AMessage: string; const AMessageKind: TMessageKind): string;
begin
  Result := AddMessage(AMessage, AMessageKind);
  FLogIndentLevel := FLogIndentLevel + 1;
end;

function TLogger.AddExitMessage(const AMessage: string; const AMessageKind: TMessageKind): string;
begin
  FLogIndentLevel := FLogIndentLevel - 1;
  Result := AddMessage(AMessage, AMessageKind);
end;

function TLogger.AddMessage(const AMessage: string; const AMessageKind: TMessageKind): string;
var
  vFile: TFileStream;
  vBuffer: TBytes;
begin
  Result := '';
  if not FEnabled then
    Exit;
  if (FLogMessageKind <> mkAny) and (AMessageKind <> FLogMessageKind) then
    Exit;

  Result := FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + cLogMessageTypes[AMessageKind] + ' ' + StringOfChar(' ', FLogIndentLevel * 2) + AMessage;
  try
    if FileExists(FFileName) then
    begin
      vFile := TFileStream.Create(FFileName, fmOpenWrite);
      vFile.Position := vFile.Size;
      vFile.Write(sLineBreak, Length(sLineBreak));
    end
    else
      vFile := TFileStream.Create(FFileName, fmCreate);

    try
      vBuffer := TEncoding.Default.GetBytes(Result);
      vFile.WriteData(vBuffer, Length(Result));
    finally
      vFile.Free;
    end;
  except
    Result := 'Cannot save: ' + Result;
  end;
end;

constructor TLogger.Create(const ADomain: TObject; const AName: string);
begin
  inherited Create(ADomain, AName);
  FItems := TStringList.Create;
  FLogIndentLevel := 0;
  FLogMessageKind := mkAny;
  FEnabled := True;
end;

destructor TLogger.Destroy;
begin
  Flush;
  FreeAndNil(FItems);

  inherited Destroy;
end;

procedure TLogger.Flush;
begin
  try
    FItems.SaveToFile(FFileName);
  except
  end;
end;

function TLogger.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TLogger.GetItem(const AIndex: Integer): string;
begin
  Result := FItems[AIndex];
end;

procedure TLogger.SetTarget(const AFileName: string);
begin
  FFileName := AFileName;
end;

end.
