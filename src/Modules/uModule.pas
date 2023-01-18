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
  Classes, Generics.Collections;

type
  TBaseModule = class;
  TModuleClass = class of TBaseModule;

  TModuleInfo = class
  private
    FName: string;
    FAncestors: string;
    FModuleClass: TModuleClass;
  public
    constructor Create(const AName, AAncestors: string; const AClass: TModuleClass);
    destructor Destroy; override;

    property Name: string read FName;
    property Ancestors: string read FAncestors;
    property ModuleClass: TModuleClass read FModuleClass;
  end;

  TBaseModule = class
  private
    class var RegisteredModules: TObjectDictionary<string, TObjectDictionary<string, TModuleInfo>>;
  public
    class procedure RegisterModule(const AType, AAncestors, AName: string; const AClass: TModuleClass);
    class function GetModuleInfo(const AType, AName: string): TModuleInfo;
    class function GetModuleNamesOfType(const AType: string): TStrings;
  end;

  TDomainModule = class(TBaseModule)
  protected
    [Weak] FDomain: TObject;
  public
    constructor Create(const ADomain: TObject; const AName: string); virtual;
    destructor Destroy; override;

    property Domain: TObject read FDomain;
  end;

  TDomainModuleClass = class of TDomainModule;

implementation

uses
  SysUtils;

class function TBaseModule.GetModuleInfo(const AType, AName: string): TModuleInfo;
var
  vGroup: TObjectDictionary<string, TModuleInfo>;
begin
  Result := nil;
  if RegisteredModules.TryGetValue(AType.ToLowerInvariant, vGroup) then
    if not vGroup.TryGetValue(AName.ToLowerInvariant, Result) then
      Result := nil;
end;

class function TBaseModule.GetModuleNamesOfType(const AType: string): TStrings;
var
  vGroup: TObjectDictionary<string, TModuleInfo>;
  vModuleInfo: TModuleInfo;
begin
  Result := TStringList.Create;
  if RegisteredModules.TryGetValue(AType.ToLowerInvariant, vGroup) then
    for vModuleInfo in vGroup.Values do
      Result.Add(vModuleInfo.Name);
end;

class procedure TBaseModule.RegisterModule(const AType, AAncestors, AName: string; const AClass: TModuleClass);
var
  vGroup: TObjectDictionary<string, TModuleInfo>;
  vModuleInfo: TModuleInfo;
begin
  if not RegisteredModules.TryGetValue(AType.ToLowerInvariant, vGroup) then
  begin
    vGroup := TObjectDictionary<string, TModuleInfo>.Create([doOwnsValues]);
    RegisteredModules.Add(AType.ToLowerInvariant, vGroup);
  end;

  vModuleInfo := TModuleInfo.Create(AName, AAncestors, AClass);
  vGroup.Add(AName.ToLowerInvariant, vModuleInfo);
end;

{ TDomainModule }

constructor TDomainModule.Create(const ADomain: TObject; const AName: string);
begin
  inherited Create;
  FDomain := ADomain;
end;

destructor TDomainModule.Destroy;
begin
  FDomain := nil;
  inherited Destroy;
end;

{ TModuleInfo }

constructor TModuleInfo.Create(const AName, AAncestors: string; const AClass: TModuleClass);
begin
  inherited Create;
  FName := AName;
  FAncestors := AAncestors;
  FModuleClass := AClass;
end;

destructor TModuleInfo.Destroy;
begin
  FModuleClass := nil;
  inherited Destroy;
end;

initialization

TBaseModule.RegisteredModules := TObjectDictionary<string, TObjectDictionary<string, TModuleInfo>>.Create([doOwnsValues]);

finalization

FreeAndNil(TBaseModule.RegisteredModules);

end.

