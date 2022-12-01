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

unit uSettings;

interface

uses
  IniFiles, Classes;

type
  TSettings = class
  protected
    function DoGetValue(const ASection, AKey: string; const ADefault: string = ''): string; virtual; abstract;
    procedure DoSetValue(const ASection, AKey, AValue: string); virtual; abstract;
    procedure DoReadSection(const ASection: string; const AValues: TStrings); virtual; abstract;
    function CheckSectionExists(const ASection: string): Boolean; virtual; abstract;
    function CheckKeyExists(const ASection, AKey: string): Boolean; virtual; abstract;
    procedure DoDeleteKey(const ASection, AKey: string); virtual; abstract;
  public
    function SectionExists(const ASection: string): Boolean;
    function KeyExists(const ASection, AKey: string): Boolean;
    procedure DeleteKey(const ASection, AKey: string);
    procedure ReadSection(const ASection: string; const AValues: TStrings);
    function GetValue(const ASection, AKey: string; const ADefault: string = ''): string;
    procedure SetValue(const ASection, AKey, AValue: string);
  end;

  TIniSettings = class(TSettings)
  private
    FIniFile: TIniFile;
  protected
    function DoGetValue(const ASection, AKey: string; const ADefault: string = ''): string; override;
    procedure DoSetValue(const ASection, AKey, AValue: string); override;
    procedure DoReadSection(const ASection: string; const AValues: TStrings); override;
    function CheckSectionExists(const ASection: string): Boolean; override;
    function CheckKeyExists(const ASection, AKey: string): Boolean; override;
    procedure DoDeleteKey(const ASection, AKey: string); override;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, StrUtils;

{ TSettings }

procedure TSettings.DeleteKey(const ASection, AKey: string);
begin
  DoDeleteKey(ASection, AKey);
end;

function TSettings.GetValue(const ASection, AKey, ADefault: string): string;
begin
  Result := DoGetValue(ASection, AKey, ADefault);
end;

function TSettings.KeyExists(const ASection, AKey: string): Boolean;
begin
  Result := CheckKeyExists(ASection, AKey);
end;

procedure TSettings.ReadSection(const ASection: string; const AValues: TStrings);
begin
  DoReadSection(ASection, AValues);
end;

function TSettings.SectionExists(const ASection: string): Boolean;
begin
  Result := CheckSectionExists(ASection);
end;

procedure TSettings.SetValue(const ASection, AKey, AValue: string);
begin
  DoSetValue(ASection, AKey, AValue);
end;

{ TIniSettings }

function TIniSettings.CheckKeyExists(const ASection, AKey: string): Boolean;
begin
  Result := FIniFile.ValueExists(ASection, AKey);
end;

function TIniSettings.CheckSectionExists(const ASection: string): Boolean;
begin
  Result := FIniFile.SectionExists(ASection);
end;

constructor TIniSettings.Create(const AFileName: string);
begin
  FIniFile := TIniFile.Create(AFileName);
  inherited Create;
end;

destructor TIniSettings.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FIniFile);
end;

procedure TIniSettings.DoDeleteKey(const ASection, AKey: string);
begin
  FIniFile.DeleteKey(ASection, AKey);
end;

function TIniSettings.DoGetValue(const ASection, AKey, ADefault: string): string;
begin
  Result := FIniFile.ReadString(ASection, AKey, ADefault);
end;

procedure TIniSettings.DoReadSection(const ASection: string; const AValues: TStrings);
begin
  FIniFile.ReadSectionValues(ASection, AValues);
end;

procedure TIniSettings.DoSetValue(const ASection, AKey, AValue: string);
begin
  FIniFile.WriteString(ASection, AKey, AValue);
  FIniFile.UpdateFile;
end;

end.
