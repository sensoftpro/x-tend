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

unit uModule;

interface

uses
  Classes, Generics.Collections, uFastClasses;

type
  TBaseModule = class;
  TModuleClass = class of TBaseModule;

  TModuleRec = record
    Name: string;
    ModuleClass: TModuleClass;
  end;

  TBaseModule = class
  private
    class var RegisteredModules: TObjectStringDictionary<TList<TModuleRec>>;
  public
    class procedure RegisterModule(const AType, AName: string; const AClass: TModuleClass);
    class function GetModuleClass(const AType, AName: string): TModuleClass;
    class function GetModuleNamesOfType(const AType: string): TStrings;
  end;

  TDomainModule = class(TBaseModule)
  protected
    [Weak] FDomain: TObject;
    FName: string;
  public
    constructor Create(const ADomain: TObject; const AName: string); virtual;
    destructor Destroy; override;

    property Domain: TObject read FDomain;
    property Name: string read FName;
  end;

  TDomainModuleClass = class of TDomainModule;

implementation

uses
  SysUtils;

{ TBaseModule }

class function TBaseModule.GetModuleClass(const AType, AName: string): TModuleClass;
var
  vGroup: TList<TModuleRec>;
  vModuleRec: TModuleRec;
begin
  Result := nil;
  vGroup := RegisteredModules.ObjectByName(AType.ToLowerInvariant);
  if not Assigned(vGroup) then
    Exit;

  for vModuleRec in vGroup do
  begin
    if SameText(vModuleRec.Name, AName) then
    begin
      Result := vModuleRec.ModuleClass;
      Exit;
    end;
  end;
end;

class function TBaseModule.GetModuleNamesOfType(const AType: string): TStrings;
var
  vGroup: TList<TModuleRec>;
  vModuleRec: TModuleRec;
begin
  Result := TStringList.Create;
  vGroup := RegisteredModules.ObjectByName(AType.ToLowerInvariant);
  if Assigned(vGroup) then
    for vModuleRec in vGroup do
      Result.Add(vModuleRec.Name);
end;

class procedure TBaseModule.RegisterModule(const AType, AName: string; const AClass: TModuleClass);
var
  vGroup: TList<TModuleRec>;
  vModuleRec: TModuleRec;
begin
  vGroup := RegisteredModules.ObjectByName(AType.ToLowerInvariant);
  if not Assigned(vGroup) then
  begin
    vGroup := TList<TModuleRec>.Create;
    RegisteredModules.AddObject(AType.ToLowerInvariant, vGroup);
  end;

  vModuleRec.Name := AName.ToLowerInvariant;
  vModuleRec.ModuleClass := AClass;
  vGroup.Add(vModuleRec);
end;

{ TDomainModule }

constructor TDomainModule.Create(const ADomain: TObject; const AName: string);
begin
  inherited Create;
  FDomain := ADomain;
  FName := AName;
end;

destructor TDomainModule.Destroy;
begin
  FDomain := nil;
  inherited Destroy;
end;

initialization

TBaseModule.RegisteredModules := TObjectStringDictionary<TList<TModuleRec>>.Create;

finalization

FreeAndNil(TBaseModule.RegisteredModules);

end.

