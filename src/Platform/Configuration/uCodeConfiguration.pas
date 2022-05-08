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

unit uCodeConfiguration;

interface

uses
  uConfiguration, uScript;

type
  TCodeConfiguration = class(TConfiguration)
  private
    FScript: TScript;
    function MakeScriptMethod(const ACode: Pointer): TMethod;
  protected
    function LoadConfiguration: string; override;
    function GetTitle: string; override;
    function GetVersionName: string; override;
  public
    constructor Create(const APlatform: TObject; const AName: string);
    destructor Destroy; override;

    property Script: TScript read FScript;
  end;

implementation

uses
  SysUtils, uUtils;

{ TCodeConfiguration }

constructor TCodeConfiguration.Create(const APlatform: TObject; const AName: string);
begin
  inherited Create(APlatform, AName);

  FScript := TScript.AddScript(Self);

  FCreateDefaultEntitiesProc := MakeScriptMethod(@TScript.CreateDefaultEntities);
  FExecuteDefaultActionFunc := MakeScriptMethod(@TScript.ExecuteDefaultAction);
  FDomainReadyProc := MakeScriptMethod(@TScript.DomainReady);
  FDomainStoppedProc := MakeScriptMethod(@TScript.DomainStopped);
  FGetReportValueFunc := MakeScriptMethod(@TScript.GetReportValue);
  FCheckActionFlagsFunc := MakeScriptMethod(@TScript.CheckActionFlags);
  FExecuteActionFunc := MakeScriptMethod(@TScript.ExecuteAction);
  FExecuteCommandFunc := MakeScriptMethod(@TScript.ExecuteCommand);
  FAfterEntityCreationProc := MakeScriptMethod(@TScript.AfterEntityCreation);
  FBeforeEntitySavingProc := MakeScriptMethod(@TScript.BeforeEntitySaving);
  FAfterEntitySavedProc := MakeScriptMethod(@TScript.AfterEntitySaved);
  FBeforeEntityRemovingProc := MakeScriptMethod(@TScript.BeforeEntityRemoving);
  FFullTextFunc := MakeScriptMethod(@TScript.FullText);
  FCheckFieldFunc := MakeScriptMethod(@TScript.CheckField);
  FLoginedProc := MakeScriptMethod(@TScript.Logined);
  FBeforeUIClosingFunc := MakeScriptMethod(@TScript.BeforeUIClosing);
  FCalculateStyleFunc := MakeScriptMethod(@TScript.CalculateStyle);
  FCanChangeFieldFunc := MakeScriptMethod(@TScript.CanChangeField);
  FGetParamValueFunc := MakeScriptMethod(@TScript.GetParamValue);
end;

destructor TCodeConfiguration.Destroy;
begin
  FreeAndNil(FScript);
  inherited Destroy;
end;

function TCodeConfiguration.GetTitle: string;
begin
  Result := FScript.AppTitle;
end;

function TCodeConfiguration.GetVersionName: string;
begin
  Result := FScript.VersionName;
end;

function TCodeConfiguration.LoadConfiguration: string;
begin
  Result := FScript.CreateConfiguration;
end;

function TCodeConfiguration.MakeScriptMethod(const ACode: Pointer): TMethod;
begin
  Result := MakeMethod(ACode, FScript);
end;

end.

