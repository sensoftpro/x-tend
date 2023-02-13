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

unit uIcon;

interface

uses
  Classes, Generics.Collections;

type
  TIconList = class
  private
    FResolution: Integer;
    FItems: TObjectDictionary<Integer, TStream>;
    FPlaceHolder: TStream;
  public
    constructor Create(const AResolution: Integer);
    destructor Destroy; override;

    procedure AddIcon(const AName: string; const AIndex: Integer; const AStream: TStream);
    function IconByIndex(const AIndex: Integer): TStream;

    property Resolution: Integer read FResolution;
  end;

  TIcons = class
  private
    FItems: TObjectDictionary<Integer, TIconList>;
    FImages: TObjectDictionary<string, TStream>;
    FIconIndices: TList<Integer>;
    FSplashImage: TStream;
    FSplashFileName: string;
    procedure AddIcon(const AFullFileName: string);
    procedure AddImage(const AFullFileName: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(const ADirectory: string);
    function IconByIndex(const AIndex: Integer; const AResolution: Integer = 16): TStream;
    function ImageByName(const AName: string): TStream;

    property Splash: TStream read FSplashImage;
    property SplashFileName: string read FSplashFileName;
    property IconIndices: TList<Integer> read FIconIndices;
  end;

implementation

uses
  SysUtils, IOUtils, RegularExpressions;

{ TIcons }

procedure TIcons.AddIcon(const AFullFileName: string);
const
  cImageFileNameRE = '^i(\d+)_(?:r(\d+)_)?(.*)\.([^.]+)$';
var
  vFileName: string;
  vMatch: TMatch;
  vIndex: Integer;
  vResolution: Integer;
  vName: string;
  vFileExt: string;
  vImageStream: TMemoryStream;
  vIconList: TIconList;
begin
  vFileName := ExtractFileName(AFullFileName);
  vMatch := TRegEx.Match(vFileName, cImageFileNameRE, [roIgnoreCase, roSingleLine]);
  if not vMatch.Success then
    Exit;

  vIndex := StrToIntDef(vMatch.Groups[1].Value, -1);
  if vIndex < 0 then
    Exit;

  if not FIconIndices.Contains(vIndex) then
    FIconIndices.Add(vIndex);

  vResolution := StrToIntDef(vMatch.Groups[2].Value, 16);
  if not FItems.TryGetValue(vResolution, vIconList) then
  begin
    vIconList := TIconList.Create(vResolution);
    FItems.Add(vResolution, vIconList);
  end;

  vName := vMatch.Groups[3].Value;
  vFileExt := vMatch.Groups[4].Value;
  if vFileExt = 'png' then
  begin
    vImageStream := TMemoryStream.Create;
    vImageStream.LoadFromFile(AFullFileName);
    vImageStream.Position := 0;
    vIconList.AddIcon(vName, vIndex, vImageStream);
  end;
end;

procedure TIcons.AddImage(const AFullFileName: string);
var
  vFileName: string;
  vImageStream: TMemoryStream;
begin
  vFileName := TPath.GetFileName{WithoutExtension}(AFullFileName);
  vImageStream := TMemoryStream.Create;
  vImageStream.LoadFromFile(AFullFileName);
  vImageStream.Position := 0;
  FImages.AddOrSetValue(vFileName, vImageStream);
end;

constructor TIcons.Create;
begin
  inherited Create;
  FIconIndices := TList<Integer>.Create;
  FItems := TObjectDictionary<Integer, TIconList>.Create([doOwnsValues]);
  FImages := TObjectDictionary<string, TStream>.Create([doOwnsValues]);
  FSplashImage := nil;
  FSplashFileName := '';
end;

destructor TIcons.Destroy;
begin
  FreeAndNil(FImages);
  FreeAndNil(FItems);
  FreeAndNil(FIconIndices);
  FreeAndNil(FSplashImage);

  inherited Destroy;
end;

function TIcons.IconByIndex(const AIndex, AResolution: Integer): TStream;
var
  vIconList: TIconList;
begin
  if FItems.TryGetValue(AResolution, vIconList) then
    Result := vIconList.IconByIndex(AIndex)
  else
    Result := nil;
end;

function TIcons.ImageByName(const AName: string): TStream;
begin
  if not FImages.TryGetValue(AName, Result) then
    Result := nil;
end;

procedure TIcons.Load(const ADirectory: string);
var
  vDirectory: string;
  vSearchRec: TSearchRec;
begin
  if not DirectoryExists(ADirectory) then
    Exit;

  // Загрузка сплэш экрана
  if FileExists(TPath.Combine(ADirectory, 'splash.jpg')) then
  begin
    if Assigned(FSplashImage) then
      FreeAndNil(FSplashImage);
    FSplashImage := TMemoryStream.Create;
    FSplashFileName := TPath.Combine(ADirectory, 'splash.jpg');
    TMemoryStream(FSplashImage).LoadFromFile(FSplashFileName);
  end;

  vDirectory := TPath.Combine(ADirectory, 'icons');
  if not DirectoryExists(vDirectory) then
    Exit;

  if SysUtils.FindFirst(vDirectory + PathDelim + '*.*', faNormal, vSearchRec) = 0 then
  begin
    repeat
      AddIcon(TPath.Combine(vDirectory, vSearchRec.Name));
    until SysUtils.FindNext(vSearchRec) <> 0;

    SysUtils.FindClose(vSearchRec);
  end;

  vDirectory := TPath.Combine(ADirectory, 'images');
  if not DirectoryExists(vDirectory) then
    Exit;

  if SysUtils.FindFirst(vDirectory + '\*.*', faNormal, vSearchRec) = 0 then
  begin
    repeat
      AddImage(TPath.Combine(vDirectory, vSearchRec.Name));
    until SysUtils.FindNext(vSearchRec) <> 0;

    SysUtils.FindClose(vSearchRec);
  end;
end;

{ TIconStorage }

procedure TIconList.AddIcon(const AName: string; const AIndex: Integer; const AStream: TStream);
begin
  if SameText(AName, 'placeholder') then
  begin
    if Assigned(FPlaceHolder) then
      FreeAndNil(FPlaceHolder);
    FPlaceHolder := AStream;
    Exit;
  end;

  FItems.AddOrSetValue(AIndex, AStream);
end;

constructor TIconList.Create(const AResolution: Integer);
begin
  inherited Create;
  FResolution := AResolution;
  FItems := TObjectDictionary<Integer, TStream>.Create([doOwnsValues]);
  FPlaceHolder := nil;
end;

destructor TIconList.Destroy;
begin
  FreeAndNil(FPlaceHolder);
  FreeAndNil(FItems);

  inherited Destroy;
end;

function TIconList.IconByIndex(const AIndex: Integer): TStream;
begin
  if not FItems.TryGetValue(AIndex, Result) then
    Result := FPlaceHolder;
end;

end.
