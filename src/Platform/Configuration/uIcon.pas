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
    FItems: TObjectDictionary<string, TStream>;
    FPlaceHolder: TStream;
  public
    constructor Create(const AResolution: Integer);
    destructor Destroy; override;

    procedure AddIcon(const AName: string; const AStream: TStream);
    function IconByName(const AName: string): TStream;

    property Items: TObjectDictionary<string, TStream> read FItems;
    property Resolution: Integer read FResolution;
  end;

  TIcons = class
  private
    FItems: TObjectDictionary<Integer, TIconList>;
    FImages: TObjectDictionary<string, TStream>;
    FSplashImage: TStream;
    FSplashFileName: string;
    procedure AddImage(const AFullFileName: string);
    function GetItems(const AResolution: Integer): TIconList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(const ADirectory: string);
    function IconByName(const AName: string; const AResolution: Integer = 16): TStream;
    function ImageByName(const AName: string): TStream;

    property Items[const AResolution: Integer]: TIconList read GetItems; default;
    property Splash: TStream read FSplashImage;
    property SplashFileName: string read FSplashFileName;
  end;

implementation

uses
  SysUtils, IOUtils, uConsts;

{ TIcons }

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
  FItems := TObjectDictionary<Integer, TIconList>.Create([doOwnsValues]);
  FImages := TObjectDictionary<string, TStream>.Create([doOwnsValues]);
  FSplashImage := nil;
  FSplashFileName := '';
end;

destructor TIcons.Destroy;
begin
  FreeAndNil(FImages);
  FreeAndNil(FItems);
  FreeAndNil(FSplashImage);

  inherited Destroy;
end;

function TIcons.GetItems(const AResolution: Integer): TIconList;
begin
  if not FItems.TryGetValue(AResolution, Result) then
  begin
    Result := TIconList.Create(AResolution);
    FItems.Add(AResolution, Result);
  end;
end;

function TIcons.IconByName(const AName: string; const AResolution: Integer): TStream;
var
  vName: string;
  vIconList: TIconList;
begin
  vName := AName.ToLowerInvariant;
  if FItems.TryGetValue(AResolution, vIconList) then
    Result := vIconList.IconByName(vName)
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
  vDirectories: TArray<string>;
  vDirName, vFullDirName: string;
  vResolution: Integer;
  vIconList: TIconList;
  vName, vExtension: string;
  vImageStream: TMemoryStream;
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

  vDirectories := TArray<string>(TDirectory.GetDirectories(vDirectory));
  for vFullDirName in vDirectories do
  begin
    vDirName := GetOwnerDirectoryName(vFullDirName);
    vResolution := StrToIntDef(Trim(vDirName), 0);
    if vResolution > 0 then
    begin
      vIconList := GetItems(vResolution);

      if SysUtils.FindFirst(vFullDirName + PathDelim + '*.*', faNormal, vSearchRec) = 0 then
      begin
        repeat
          vName := TPath.GetFileNameWithoutExtension(vSearchRec.Name);
          vExtension := TPath.GetExtension(vSearchRec.Name);
          if vExtension = '.png' then
          begin
            vImageStream := TMemoryStream.Create;
            vImageStream.LoadFromFile(TPath.Combine(vFullDirName, vSearchRec.Name));
            vImageStream.Position := 0;
            vIconList.AddIcon(vName, vImageStream);
          end;
        until SysUtils.FindNext(vSearchRec) <> 0;

        SysUtils.FindClose(vSearchRec);
      end;
    end;
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

procedure TIconList.AddIcon(const AName: string; const AStream: TStream);
var
  vName: string;
begin
  vName := AName.ToLowerInvariant;
  if SameStr(vName, 'placeholder') then
  begin
    if Assigned(FPlaceHolder) then
      FreeAndNil(FPlaceHolder);
    FPlaceHolder := AStream;
    Exit;
  end;

  FItems.AddOrSetValue(vName, AStream);
end;

constructor TIconList.Create(const AResolution: Integer);
begin
  inherited Create;
  FResolution := AResolution;
  FItems := TObjectDictionary<string, TStream>.Create([doOwnsValues]);
  FPlaceHolder := nil;
end;

destructor TIconList.Destroy;
begin
  FreeAndNil(FPlaceHolder);
  FreeAndNil(FItems);

  inherited Destroy;
end;

function TIconList.IconByName(const AName: string): TStream;
begin
  if not FItems.TryGetValue(AName, Result) then
    Result := FPlaceHolder;
end;

end.
