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

unit uMigration;

interface

uses
  Generics.Collections, Variants;

type
  TChangeStructureProc = reference to procedure(const AConfiguration: TObject);
  TTransferDataProc = reference to procedure(const ADomain: TObject);

  TVersionData = packed record
    case Boolean of
      True:
       (Major: Word; // Кардинальные изменения логики выполнения приложения
        Minor: Word; // Изменения структуры объектной модели
        Revision: Word; // Изменения поведения объектной модели и исправление ошибок
        Build: Word);
      False:
       (Parts: array[0..3] of Word);
  end;

  TMigration = class
  private
    FVersion: string;
    FVersionData: TVersionData;
    FCreationProc: TChangeStructureProc;
    FMigrationProc: TTransferDataProc;
    FDeletionProc: TChangeStructureProc;
    procedure Clear;
    function ParseVersionData(const AVersion: string): TVersionData;
  public
    constructor Create(const AVersion: string);
    destructor Destroy; override;

    procedure AddCreationProc(const ACreationProc: TChangeStructureProc);
    procedure AddMigrationProc(const AMigrationProc: TTransferDataProc);
    procedure AddDeletionProc(const ADeletionProc: TChangeStructureProc);

    function CreateNewStructures(const AConfiguration: TObject): Boolean;
    function DeleteOldStructures(const AConfiguration: TObject): Boolean;

    procedure Apply(const AConfiguration: TObject);

    function VersionGreaterThan(const AVersion: string): Boolean;

    property Version: string read FVersion;
    property MigrationProc: TTransferDataProc read FMigrationProc;
  end;

  TMigrations = class(TObjectList<TMigration>)
  public
    function AddMigration(const AVersion: string): TMigration;
    function NextMigrationIndex(const AVersion: string): Integer;
  end;

implementation

uses
  Classes, SysUtils, Math, uUtils;

{ TMigration }

procedure TMigration.AddCreationProc(const ACreationProc: TChangeStructureProc);
begin
  FCreationProc := ACreationProc;
end;

procedure TMigration.AddDeletionProc(const ADeletionProc: TChangeStructureProc);
begin
  FDeletionProc := ADeletionProc;
end;

procedure TMigration.AddMigrationProc(const AMigrationProc: TTransferDataProc);
begin
  FMigrationProc := AMigrationProc;
end;

procedure TMigration.Apply(const AConfiguration: TObject);
begin
  CreateNewStructures(AConfiguration);
  DeleteOldStructures(AConfiguration);
end;

procedure TMigration.Clear;
begin
  FCreationProc := nil;
  FMigrationProc := nil;
  FDeletionProc := nil;
end;

constructor TMigration.Create(const AVersion: string);
begin
  inherited Create;
  FVersion := AVersion;
  FVersionData := ParseVersionData(AVersion);
  Clear;
end;

function TMigration.CreateNewStructures(const AConfiguration: TObject): Boolean;
begin
  Result := Assigned(FCreationProc);
  if Result then
    FCreationProc(AConfiguration);
end;

function TMigration.DeleteOldStructures(const AConfiguration: TObject): Boolean;
begin
  Result := Assigned(FDeletionProc);
  if Result then
    FDeletionProc(AConfiguration);
end;

destructor TMigration.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TMigration.ParseVersionData(const AVersion: string): TVersionData;
var
  vVersionParts: TStrings;
  i: Integer;
begin
  for i := 0 to 3 do
    Result.Parts[i] := 0;

  vVersionParts := CreateDelimitedList(AVersion, '.');
  try
    for i := 0 to Min(vVersionParts.Count - 1, 2) do
      Result.Parts[i] := StrToIntDef(Trim(vVersionParts[i]), 0);
  finally
    FreeAndNil(vVersionParts);
  end;
end;

function TMigration.VersionGreaterThan(const AVersion: string): Boolean;
var
  vVersionData: TVersionData;
  i: Integer;
begin
  Result := False;
  vVersionData := ParseVersionData(AVersion);
  for i := 0 to 2 do
    if FVersionData.Parts[i] > vVersionData.Parts[i] then
    begin
      Result := True;
      Exit;
    end
    else if FVersionData.Parts[i] < vVersionData.Parts[i] then
      Exit;
end;

{ TMigrations }

function TMigrations.AddMigration(const AVersion: string): TMigration;
var
  vNextMigrationIndex: Integer;
begin
//TODO Not available now
  Assert(False);

  vNextMigrationIndex := NextMigrationIndex(AVersion);
  Result := TMigration.Create(AVersion);
  if vNextMigrationIndex < 0 then
    Add(Result)
  else
    Insert(vNextMigrationIndex, Result);
end;

function TMigrations.NextMigrationIndex(const AVersion: string): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].VersionGreaterThan(AVersion) then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

end.
