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

unit uDrawStyles;

interface

uses
  Types, uFastClasses, uModule, uIcon, uJSON, uConsts;

const
  FontStyleRegular    = Integer(0);
  FontStyleBold       = Integer(1);
  FontStyleItalic     = Integer(2);
  FontStyleUnderline  = Integer(4);
  FontStyleStrikeout  = Integer(8);

const
  DT_TOP = 0;
  DT_LEFT = 0;
  DT_CENTER = 1;
  DT_RIGHT = 2;
  DT_VCENTER = 4;
  DT_BOTTOM = 8;

type
  TGradientKind = (gkNone, gkHorizontal, gkVertical);

  TPainter = class;

  TStylePen = class
  private
    FPen: TObject;
    FColorPen: TObject;
    FColorized: Boolean;
    FName: string;
    FStrokeColor: Cardinal;
    FInitialStrokeWidth: Single;
    FStrokeWidth: Single;
    FStrokeStyle: TPenStyle;
    function GetPen: TObject;
  public
    constructor Create(const AName: string; const AColor: Cardinal = $FF000000;
      const AWidth: Single = 1.0; const AStyle: TPenStyle = psSolid);
    destructor Destroy; override;

    procedure ApplyZoom(const AZoom: Single);
    procedure RestoreZoom;

    procedure CreatePen(const APainter: TPainter);
    function Colorize(const AColor: Cardinal): Boolean;
    procedure RestoreColor;

    property Pen: TObject read GetPen;
    property ColorPen: TObject read FColorPen;
    property Name: string read FName;
    property Color: Cardinal read FStrokeColor;
    property Width: Single read FStrokeWidth;
    property Style: TPenStyle read FStrokeStyle;
  end;

  TStyleBrush = class
  private
    FBrush: TObject;
    FColorBrush: TObject;
    FColorized: Boolean;
    FName: string;
    FFillColor: Cardinal;
    FFillColorEx: Cardinal;
    FFillBackColor: Cardinal;
    FFillGradientKind: TGradientKind;
    FFillStyle: TBrushStyle;
    FFillRect: TRectF;
    procedure SetRect(const Value: TRectF);
    function GetBrush: TObject;
    function GetFillColor: Cardinal;
  public
    constructor Create(const AName: string; const AColor: Cardinal = $FF000000; const ABackColor: Cardinal = $FFFFFFFF;
      const AGradientKind: TGradientKind = gkNone; const AStyle: TBrushStyle = bsSolid);
    destructor Destroy; override;

    procedure CreateBrush(const APainter: TPainter);
    function Colorize(const AColor: Cardinal): Boolean;
    procedure RestoreColor;

    property Brush: TObject read GetBrush;
    property ColorBrush: TObject read FColorBrush;
    property Name: string read FName;
    property Color: Cardinal read GetFillColor;
    property BackColor: Cardinal read FFillBackColor;
    property GradientKind: TGradientKind read FFillGradientKind;
    property Style: TBrushStyle read FFillStyle;
    property Rect: TRectF read FFillRect write SetRect;
  end;

  TStyleFont = class
  private
    FFont: TObject;
    FColorFont: TObject;
    FColorized: Boolean;
    FName: string;
    FFontColor: Cardinal;
    FFontFamily: string;
    FFontStyle: Integer;
    FInitialFontSize: Single;
    FFontSize: Single;
    FFontAngle: Single;
    function GetFont: TObject;
  public
    constructor Create(const AName: string; const AFamily: string = 'Tahoma'; const ASize: Single = 20.0;
      const AColor: Cardinal = $FF000000; const AStyle: Integer = FontStyleRegular; const AAngle: Single = 0);
    destructor Destroy; override;

    procedure ApplyZoom(const AZoom: Single);
    procedure RestoreZoom;

    procedure CreateFont(const APainter: TPainter);
    function Colorize(const AColor: Cardinal): Boolean;
    procedure RestoreColor;

    property Font: TObject read GetFont;
    property ColorFont: TObject read FColorFont;
    property Name: string read FName;
    property Color: Cardinal read FFontColor;
    property Family: string read FFontFamily;
    property Style: Integer read FFontStyle;
    property Size: Single read FFontSize;
    property Angle: Single read FFontAngle;
  end;

  TStyleImage = class
  private
    FImage: TObject;
    FName: string;
    FFileName: string;
    FCutRect: TRect;
    FTransparent: Boolean;
    procedure SetImage(const Value: TObject);
  public
    constructor Create(const AName: string; const AFileName: string; const ATransparent: Boolean);
    destructor Destroy; override;

    function IsNinePatch: Boolean;

    property Image: TObject read FImage write SetImage;
    property Name: string read FName;
    property FileName: string read FFileName;
    property Transparent: Boolean read FTransparent;
    property CutRect: TRect read FCutRect write FCutRect;
  end;

  TDrawStyle = class
  private
    FName: string;
    FBrushes: TObjectStringDictionary<TStyleBrush>;
    FPens: TObjectStringDictionary<TStylePen>;
    FFonts: TObjectStringDictionary<TStyleFont>;
    FImages: TObjectStringDictionary<TStyleImage>;
    function GetMainColor: Cardinal;
    function GetBrushByName(const AName: string): TStyleBrush;
    function GetPenByName(const AName: string): TStylePen;
    function GetFontByName(const AName: string): TStyleFont;
    function GetImageByName(const AName: string): TStyleImage;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure Load(const AObject: TJSONObject);
    function Save: TJSONObject;

    function AddStrokeParams(const AName: string; const AColor: Cardinal = $FF000000;
      const AWidth: Single = 1.0; const AStyle: TPenStyle = psSolid): TStylePen;
    function AddFillParams(const AName: string; const AColor: Cardinal = $FF000000;
      const ABackColor: Cardinal = $FFFFFFFF; const AGradientKind: TGradientKind = gkNone;
      const AStyle: TBrushStyle = bsSolid): TStyleBrush;
    function AddFontParams(const AName: string; const AFamily: string = 'Tahoma';
      const ASize: Single = 20.0; const AColor: Cardinal = $FF000000;
      const AStyle: Integer = FontStyleRegular; const AAngle: Single = 0 {0..360}): TStyleFont;
    function AddImageParams(const AName: string; const AFileName: string;
      const ATransparent: Boolean = False): TStyleImage;
    procedure CreateDrawObjects(const APainter: TObject);

    procedure ApplyZoom(const AZoom: Single);
    procedure RestoreZoom;

    property MainColor: Cardinal read GetMainColor;
    property Name: string read FName write FName;
    property Brush[const AName: string]: TStyleBrush read GetBrushByName;
    property Pen[const AName: string]: TStylePen read GetPenByName;
    property Font[const AName: string]: TStyleFont read GetFontByName;
    property Image[const AName: string]: TStyleImage read GetImageByName;
  end;

  TDrawStyles = class
  private
    FStyles: TObjectStringDictionary<TDrawStyle>;
    function GetCount: Integer;
    function GetStyle(const AIndex: Integer): TDrawStyle;
  public
    constructor Create;
    destructor Destroy; override;

    function AddStyle(const AName: string): TDrawStyle;
    function StyleByName(const AName: string): TDrawStyle;
    function StyleExists(const AName: string): Boolean;

    procedure Load(const AArray: TJSONArray);
    function Save: TJSONArray;

    property Count: Integer read GetCount;
    property Style[const AIndex: Integer]: TDrawStyle read GetStyle; default;
  end;

  TNinePatchImage = class
  private
    FImageParts: array[0..2, 0..2] of TObject;
    FWidth, FHeight: Integer;
    FCountX: Integer;
    FCountY: Integer;
    FCutX1, FCutX2: Integer;
    FCutY1, FCutY2: Integer;

    function GetImagePart(const AX, AY: Integer): TObject;
  protected
    FImage: TObject;
    function GetWidth: Integer; virtual;
    function GetHeight: Integer; virtual;
    function CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject; virtual;
  public
    constructor Create(const AImage: TObject; const ACutX1, ACutX2, ACutY1, ACutY2: Integer);
    destructor Destroy; override;

    property ImageParts[const AX, AY: Integer]: TObject read GetImagePart; default;
    property Image: TObject read FImage;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property CountX: Integer read FCountX;
    property CountY: Integer read FCountY;
    property CutX1: Integer read FCutX1;
    property CutX2: Integer read FCutX2;
    property CutY1: Integer read FCutY1;
    property CutY2: Integer read FCutY2;
  end;

  TDrawContext = class
  protected
    function GetHeight: Integer; virtual; abstract;
    function GetWidth: Integer; virtual; abstract;
    function GetCanvas: TObject; virtual; abstract;
    function GetImage: TObject; virtual; abstract;
  public
    procedure SetSize(const AWidth, AHeight: Single); virtual; abstract;
    procedure SaveToFile(const AFileName: string); virtual; abstract;

    property Canvas: TObject read GetCanvas;
    property Image: TObject read GetImage;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
  end;

  TPainter = class(TBaseModule)
  private
    procedure DrawNinePatchImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
  protected
    FIcons: TIcons;
    FDrawStyles: TDrawStyles;
    FCanvas: TObject;
    procedure DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF); virtual; abstract;
    procedure DoDrawPie(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF;
      const AStartAngle, AEndAngle: Single); virtual; abstract;
    procedure DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF); virtual; abstract;
    procedure DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer); virtual; abstract;
    procedure DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject); virtual; abstract;
    procedure DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer); virtual; abstract;
    procedure DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer); virtual; abstract;
    procedure DoDrawRect(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF); virtual; abstract;
    function DoDrawText(const AFill: TStyleBrush; const AFont: TStyleFont; const AText: string; const ARect: TRectF;
      const AOptions: Cardinal; const AAngle: Single): TRectF; virtual; abstract;
    function GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF; virtual; abstract;
    procedure DoInvertRect(const ARect: TRectF); virtual; abstract;
    procedure DoDrawPixel(const AColor: Cardinal; const APoint: TPointF); virtual; abstract;
    procedure DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single); virtual; abstract;
    procedure DoDrawContext(const AContext: TDrawContext); virtual; abstract;
    procedure SetCanvas(const Value: TObject); virtual;

    procedure DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal); virtual; abstract;
    procedure DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal); virtual; abstract;
    procedure DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal); virtual; abstract;
  public
    constructor Create(const AIcons: TIcons); virtual;
    destructor Destroy; override;

    procedure ColorizeBrush(const AStyle: TDrawStyle; const AFillName: string; const AColor: Cardinal);
    procedure ColorizePen(const AStyle: TDrawStyle; const AStrokeName: string; const AColor: Cardinal);
    procedure ColorizeFont(const AStyle: TDrawStyle; const AFontName: string; const AColor: Cardinal);
    procedure Colorize(const AStyle: TDrawStyle; const AColor: Cardinal);
    procedure RestoreColors(const AStyle: TDrawStyle);

    function CreateBrush(const AFill: TStyleBrush): TObject; virtual; abstract;
    function CreatePen(const AStroke: TStylePen): TObject; virtual; abstract;
    function CreateFont(const AFont: TStyleFont): TObject; virtual; abstract;
    procedure CreateImage(const AImage: TStyleImage); virtual; abstract;
    function CreateDrawContext(const AWidth, AHeight: Single): TDrawContext; virtual; abstract;

    procedure DrawCircle(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const ACenterPoint: TPointF; const ARadius: Single);
    procedure DrawEllipse(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const ARect: TRectF);
    procedure DrawPie(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const ARect: TRectF;
      const AStartAngle, AEndAngle: Single);
    procedure DrawLine(const AStyle: TDrawStyle; const AStrokeName: string; const APoint1, APoint2: TPointF);
    procedure DrawPoint(const AStyle: TDrawStyle; const AFillName: string; const APoint: TPointF; const ASize: Single = 6);
    procedure DrawRegion(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const APoints: PPointF; const ACount: Integer);
    procedure DrawPath(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const APath: TObject);
    procedure DrawPolyline(const AStyle: TDrawStyle; const AStrokeName: string; const APoints: PPointF; const ACount: Integer); overload;
    procedure DrawPolyline(const AStyle: TDrawStyle; const AStrokeName: string; const APoints: array of TPointF); overload;
    procedure DrawBezier(const AStyle: TDrawStyle; const AStrokeName: string; const APoints: PPointF; const ACount: Integer);
    procedure DrawRect(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const ARect: TRectF);
    function DrawText(const AStyle: TDrawStyle; const AFillName, AFontName: string; const AText: string; const ARect: TRectF;
      const AOptions: Cardinal = 0; const AAngle: Single = 0): TRectF;
    function TextWidth(const AStyle: TDrawStyle; const AFontName: string; const AText: string): Single;
    function TextHeight(const AStyle: TDrawStyle; const AFontName: string; const AText: string): Single;
    function TextExtents(const AStyle: TDrawStyle; const AFontName: string; const AText: string): TSizeF;
    procedure InvertRect(const ARect: TRectF);
    procedure DrawPixel(const AColor: Cardinal; const APoint: TPointF);

    procedure DrawImage(const AStyle: TDrawStyle; const AImageName: string; const ARect: TRectF; const AOpacity: Single = 1.0); overload;
    procedure DrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single = 1.0); overload;

    procedure DrawContext(const AContext: TDrawContext);

    function AddStyle(const AName: string): TDrawStyle;
    function GetStyle(const AName: string): TDrawStyle;
    function StyleExists(const AName: string): Boolean;
    function BaseStyle: TDrawStyle;

    property Canvas: TObject read FCanvas write SetCanvas;
  end;

  TPainterClass = class of TPainter;

implementation

uses
  SysUtils, Math;

{ TDrawStyles }

function TDrawStyles.AddStyle(const AName: string): TDrawStyle;
var
  vName: string;
begin
  vName := AnsiLowerCase(Trim(AName));
  if vName = '' then
    raise Exception.Create('Draw style must have a name');
  if FStyles.Exists(vName) then
    raise Exception.Create('Style ' + vName + ' already exists');

  Result := TDrawStyle.Create(vName);
  FStyles.AddObject(vName, Result);
end;

constructor TDrawStyles.Create;
begin
  inherited Create;
  FStyles := TObjectStringDictionary<TDrawStyle>.Create;
end;

destructor TDrawStyles.Destroy;
begin
  FreeAndNil(FStyles);
  inherited Destroy;
end;

function TDrawStyles.GetCount: Integer;
begin
  Result := FStyles.Count;
end;

function TDrawStyles.GetStyle(const AIndex: Integer): TDrawStyle;
begin
  Result := FStyles[AIndex];
end;

procedure TDrawStyles.Load(const AArray: TJSONArray);
var
  jStyle: TJSONObject;
  vName: string;
  vStyle: TDrawStyle;
begin
  if Assigned(AArray) then
    for jStyle in AArray do
    begin
      vName := jStyle.ExtractString('name');
      if not FStyles.Exists(vName) then
      begin
        vStyle := AddStyle(vName);
        vStyle.Load(jStyle);
      end;
    end;
end;

function TDrawStyles.Save: TJSONArray;
var
  vStyle: TDrawStyle;
begin
  Result := TJSONArray.Create;
  for vStyle in FStyles do
    Result.Add(vStyle.Save)
end;

function TDrawStyles.StyleByName(const AName: string): TDrawStyle;
var
  vName: string;
begin
  vName := AnsiLowerCase(Trim(AName));
  Result := FStyles.ObjectByName(vName);
  Assert(Assigned(Result), Format('Стиль [%s] не задан', [vName]));
end;

function TDrawStyles.StyleExists(const AName: string): Boolean;
var
  vName: string;
begin
  vName := AnsiLowerCase(Trim(AName));
  Result := FStyles.Exists(vName);
end;

{ TDrawStyle }

procedure TDrawStyle.ApplyZoom(const AZoom: Single);
var
  vPen: TStylePen;
  vFont: TStyleFont;
begin
  for vPen in FPens do
    vPen.ApplyZoom(AZoom);
  for vFont in FFonts do
    vFont.ApplyZoom(AZoom);
end;

constructor TDrawStyle.Create(const AName: string);
begin
  inherited Create;

  FName := AName;
  FBrushes := TObjectStringDictionary<TStyleBrush>.Create;
  FPens := TObjectStringDictionary<TStylePen>.Create;
  FFonts := TObjectStringDictionary<TStyleFont>.Create;
  FImages := TObjectStringDictionary<TStyleImage>.Create;
end;

procedure TDrawStyle.CreateDrawObjects(const APainter: TObject);
var
  vPainter: TPainter absolute APainter;
  vBrush: TStyleBrush;
  vPen: TStylePen;
  vFont: TStyleFont;
  vImage: TStyleImage;
begin
  for vBrush in FBrushes do
    vBrush.CreateBrush(vPainter);
  for vPen in FPens do
    vPen.CreatePen(vPainter);
  for vFont in FFonts do
    vFont.CreateFont(vPainter);
  for vImage in FImages do
    vPainter.CreateImage(vImage);
end;

destructor TDrawStyle.Destroy;
begin
  FreeAndNil(FBrushes);
  FreeAndNil(FPens);
  FreeAndNil(FFonts);
  FreeAndNil(FImages);

  inherited Destroy;
end;

function TDrawStyle.GetBrushByName(const AName: string): TStyleBrush;
begin
  Result := FBrushes.ObjectByName(AName);
end;

function TDrawStyle.GetFontByName(const AName: string): TStyleFont;
begin
  Result := FFonts.ObjectByName(AName);
end;

function TDrawStyle.GetImageByName(const AName: string): TStyleImage;
begin
  Result := FImages.ObjectByName(AName);
end;

function TDrawStyle.GetMainColor: Cardinal;
begin
  if FBrushes.Count > 0 then
    Result := FBrushes[0].FFillColor
  else if FPens.Count > 0 then
    Result := FPens[0].FStrokeColor
  else if FFonts.Count > 0 then
    Result := FFonts[0].FFontColor
  else
    Result := 0;
end;

function TDrawStyle.GetPenByName(const AName: string): TStylePen;
begin
  Result := FPens.ObjectByName(AName);
end;

procedure TDrawStyle.Load(const AObject: TJSONObject);
var
  jArray: TJSONArray;
  jObject: TJSONObject;
  vName: string;
  vImage: TStyleImage;
begin
  FBrushes.Clear;
  FPens.Clear;
  FFonts.Clear;
  FImages.Clear;

  FName := AObject.ExtractString('name');

  jArray := AObject.ExtractArray('brushes');
  if Assigned(jArray) and (jArray.Size > 0) then
  begin
    for jObject in jArray do
    begin
      vName := jObject.ExtractString('name');
      AddFillParams(vName, jObject.ExtractColor('color', $FF000000), jObject.ExtractColor('backcolor', $FFFFFFFF),
        TGradientKind(jObject.ExtractInteger('gradient', Integer(gkNone))),
        TBrushStyle(jObject.ExtractInteger('style', Integer(bsSolid))));
    end;
  end;

  jArray := AObject.ExtractArray('pens');
  if Assigned(jArray) and (jArray.Size > 0) then
  begin
    for jObject in jArray do
    begin
      vName := jObject.ExtractString('name');
      AddStrokeParams(vName, jObject.ExtractColor('color', $FF000000), jObject.ExtractFloat('width', 1.0),
        TPenStyle(jObject.ExtractInteger('style', Integer(psSolid))));
    end;
  end;

  jArray := AObject.ExtractArray('fonts');
  if Assigned(jArray) and (jArray.Size > 0) then
  begin
    for jObject in jArray do
    begin
      vName := jObject.ExtractString('name');
      AddFontParams(vName, jObject.ExtractString('family', 'Tahoma'), jObject.ExtractFloat('size', 20.0),
        jObject.ExtractColor('color', $FF000000), jObject.ExtractInteger('style', FontStyleRegular),
        jObject.ExtractFloat('angle'));
    end;
  end;

  jArray := AObject.ExtractArray('images');
  if Assigned(jArray) and (jArray.Size > 0) then
  begin
    for jObject in jArray do
    begin
      vName := jObject.ExtractString('name');
      vImage := AddImageParams(vName, jObject.ExtractString('filename', ''), jObject.ExtractBoolean('transparent'));
      vImage.CutRect := Rect(jObject.ExtractInteger('cut_x1', -1), jObject.ExtractInteger('cut_y1', -1),
        jObject.ExtractInteger('cut_x2', -1), jObject.ExtractInteger('cut_y2', -1));
    end;
  end;
end;

procedure TDrawStyle.RestoreZoom;
var
  vPen: TStylePen;
  vFont: TStyleFont;
begin
  for vPen in FPens do
    vPen.RestoreZoom;
  for vFont in FFonts do
    vFont.RestoreZoom;
end;

function TDrawStyle.Save: TJSONObject;
var
  jArray: TJSONArray;
  jObject: TJSONObject;
  vBrush: TStyleBrush;
  vPen: TStylePen;
  vFont: TStyleFont;
  vImage: TStyleImage;
begin
  Result := TJSONObject.Create;
  Result.StoreString('name', FName);

  if FBrushes.Count > 0 then
  begin
    jArray := TJSONArray.Create;
    Result.AddPair('brushes', jArray);
    for vBrush in FBrushes do
    begin
      jObject := TJSONObject.Create;
      jObject.StoreString('name', vBrush.Name);
      jObject.StoreColor('color', vBrush.Color);
      jObject.StoreColor('backcolor', vBrush.BackColor);
      jObject.StoreInteger('gradient', Integer(vBrush.GradientKind));
      jObject.StoreInteger('style', Integer(vBrush.Style));
      jArray.Add(jObject);
    end;
  end;

  if FPens.Count > 0 then
  begin
    jArray := TJSONArray.Create;
    Result.AddPair('pens', jArray);
    for vPen in FPens do
    begin
      jObject := TJSONObject.Create;
      jObject.StoreString('name', vPen.Name);
      jObject.StoreColor('color', vPen.Color);
      jObject.StoreFloat('width', vPen.Width);
      jObject.StoreInteger('style', Integer(vPen.Style));
      jArray.Add(jObject);
    end;
  end;

  if FFonts.Count > 0 then
  begin
    jArray := TJSONArray.Create;
    Result.AddPair('fonts', jArray);
    for vFont in FFonts do
    begin
      jObject := TJSONObject.Create;
      jObject.StoreString('name', vFont.Name);
      jObject.StoreString('family', vFont.Family);
      jObject.StoreColor('color', vFont.Color);
      jObject.StoreInteger('style', vFont.Style);
      jObject.StoreFloat('size', vFont.Size);
      jObject.StoreFloat('angle', vFont.Angle);
      jArray.Add(jObject);
    end;
  end;

  if FImages.Count > 0 then
  begin
    jArray := TJSONArray.Create;
    Result.AddPair('images', jArray);
    for vImage in FImages do
    begin
      jObject := TJSONObject.Create;
      jObject.StoreString('name', vImage.Name);
      jObject.StoreString('filename', vImage.FileName);
      jObject.StoreBoolean('transparent', vImage.Transparent);
      if vImage.IsNinePatch then
      begin
        jObject.StoreInteger('cut_x1', vImage.CutRect.Left);
        jObject.StoreInteger('cut_y1', vImage.CutRect.Top);
        jObject.StoreInteger('cut_x2', vImage.CutRect.Right);
        jObject.StoreInteger('cut_y2', vImage.CutRect.Bottom);
      end;
      jArray.Add(jObject);
    end;
  end;
end;

function TDrawStyle.AddFillParams(const AName: string; const AColor, ABackColor: Cardinal;
  const AGradientKind: TGradientKind; const AStyle: TBrushStyle): TStyleBrush;
begin
  Assert(not FBrushes.Exists(AName), 'Объект с таким именем уже есть');
  Result := TStyleBrush.Create(AName, AColor, ABackColor, AGradientKind, AStyle);
  FBrushes.AddObject(AName, Result);
end;

function TDrawStyle.AddFontParams(const AName: string; const AFamily: string; const ASize: Single;
  const AColor: Cardinal; const AStyle: Integer; const AAngle: Single): TStyleFont;
begin
  Assert(not FFonts.Exists(AName), 'Объект с таким именем уже есть');
  Result := TStyleFont.Create(AName, AFamily, ASize, AColor, AStyle, AAngle);
  FFonts.AddObject(AName, Result);
end;

function TDrawStyle.AddImageParams(const AName, AFileName: string; const ATransparent: Boolean = False): TStyleImage;
begin
  Assert(not FImages.Exists(AName), 'Объект с таким именем уже есть');
  Result := TStyleImage.Create(AName, AFileName, ATransparent);
  FImages.AddObject(AName, Result);
end;

function TDrawStyle.AddStrokeParams(const AName: string; const AColor: Cardinal;
  const AWidth: Single; const AStyle: TPenStyle): TStylePen;
begin
  Assert(not FPens.Exists(AName), 'Объект с таким именем уже есть');
  Result := TStylePen.Create(AName, AColor, AWidth, AStyle);
  FPens.AddObject(AName, Result);
end;

{ TStylePen }

procedure TStylePen.ApplyZoom(const AZoom: Single);
begin
  FStrokeWidth := FStrokeWidth * AZoom;
end;

function TStylePen.Colorize(const AColor: Cardinal): Boolean;
begin
  FColorized := AColor <> FStrokeColor;
  Result := FColorized;
end;

constructor TStylePen.Create(const AName: string; const AColor: Cardinal; const AWidth: Single; const AStyle: TPenStyle);
begin
  inherited Create;
  FName := AName;
  FStrokeColor := AColor;
  FInitialStrokeWidth := AWidth;
  FStrokeWidth := AWidth;
  FStrokeStyle := AStyle;
  FPen := nil;
  FColorPen := nil;
  FColorized := False;
end;

procedure TStylePen.CreatePen(const APainter: TPainter);
begin
  FPen := APainter.CreatePen(Self);
  FColorPen := APainter.CreatePen(Self);
end;

destructor TStylePen.Destroy;
begin
  FreeAndNil(FPen);
  FreeAndNil(FColorPen);
  inherited Destroy;
end;

function TStylePen.GetPen: TObject;
begin
  if FColorized then
    Result := FColorPen
  else
    Result := FPen;
end;

procedure TStylePen.RestoreColor;
begin
  FColorized := False;
end;

procedure TStylePen.RestoreZoom;
begin
  FStrokeWidth := FInitialStrokeWidth;
end;

{ TStyleBrush }

function TStyleBrush.Colorize(const AColor: Cardinal): Boolean;
begin
  FColorized := AColor <> FFillColor;
  FFillColorEx := AColor;
  Result := FColorized;
end;

constructor TStyleBrush.Create(const AName: string; const AColor, ABackColor: Cardinal;
  const AGradientKind: TGradientKind; const AStyle: TBrushStyle);
begin
  inherited Create;
  FName := AName;
  FFillColor := AColor;
  FFillColorEx := AColor;
  FFillBackColor := ABackColor;
  FFillGradientKind := AGradientKind;
  FFillRect := TRectF.Empty;
  FFillStyle := AStyle;
  FBrush := nil;
  FColorBrush := nil;
  FColorized := False;
end;

procedure TStyleBrush.CreateBrush(const APainter: TPainter);
begin
  FBrush := APainter.CreateBrush(Self);
  FColorBrush := APainter.CreateBrush(Self);
end;

destructor TStyleBrush.Destroy;
begin
  FreeAndNil(FBrush);
  FreeAndNil(FColorBrush);
  inherited Destroy;
end;

function TStyleBrush.GetBrush: TObject;
begin
  if FColorized then
    Result := FColorBrush
  else
    Result := FBrush;
end;

function TStyleBrush.GetFillColor: Cardinal;
begin
  if FColorized then
    Result := FFillColorEx
  else
    Result := FFillColor;
end;

procedure TStyleBrush.RestoreColor;
begin
  FColorized := False;
end;

procedure TStyleBrush.SetRect(const Value: TRectF);
var
  vRect: TRectF;
begin
  vRect := Value;
  if FFillGradientKind = gkHorizontal then
    vRect.Bottom := vRect.Top + 1
  else if FFillGradientKind = gkVertical then
    vRect.Right := vRect.Left + 1;

  if FFillRect <> vRect then
    FFillRect := Value;
end;

{ TStyleFont }

procedure TStyleFont.ApplyZoom(const AZoom: Single);
begin
  FFontSize := FFontSize * AZoom;
end;

function TStyleFont.Colorize(const AColor: Cardinal): Boolean;
begin
  FColorized := AColor <> FFontColor;
  Result := FColorized;
end;

constructor TStyleFont.Create(const AName: string; const AFamily: string; const ASize: Single;
  const AColor: Cardinal; const AStyle: Integer; const AAngle: Single);
begin
  inherited Create;
  FName := AName;
  FFontFamily := AFamily;
  FInitialFontSize := ASize;
  FFontSize := ASize;
  FFontColor := AColor;
  FFontStyle := AStyle;
  FFontAngle := AAngle;
  FFont := nil;
  FColorFont := nil;
  FColorized := False;
end;

procedure TStyleFont.CreateFont(const APainter: TPainter);
begin
  FFont := APainter.CreateFont(Self);
  FColorFont := APainter.CreateFont(Self);
end;

destructor TStyleFont.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FColorFont);
  inherited Destroy;
end;

function TStyleFont.GetFont: TObject;
begin
  if FColorized then
    Result := FColorFont
  else
    Result := FFont;
end;

procedure TStyleFont.RestoreColor;
begin
  FColorized := False;
end;

procedure TStyleFont.RestoreZoom;
begin
  FFontSize := FInitialFontSize;
end;

{ TStyleImage }

constructor TStyleImage.Create(const AName: string; const AFileName: string; const ATransparent: Boolean);
begin
  inherited Create;
  FName := AName;
  FFileName := AFileName;
  FTransparent := ATransparent;
  FCutRect := Rect(-1, -1, -1, -1);
  FImage := nil;
end;

destructor TStyleImage.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

function TStyleImage.IsNinePatch: Boolean;
begin
  Result := (FCutRect.Left >= 0) or (FCutRect.Top >= 0) or (FCutRect.Right >= 0) or (FCutRect.Bottom >= 0);
end;

procedure TStyleImage.SetImage(const Value: TObject);
begin
  FImage.Free;
  FImage := Value;
end;

{ TNinePatchImage }

constructor TNinePatchImage.Create(const AImage: TObject; const ACutX1, ACutX2, ACutY1, ACutY2: Integer);
begin
  inherited Create;

  FImage := AImage;
  FWidth := GetWidth;
  FHeight := GetHeight;

  FCountX := 0;
  FCountY := 0;
  FCutX1 := ACutX1;
  FCutX2 := ACutX2;
  FCutY1 := ACutY1;
  FCutY2 := ACutY2;
  if ACutX1 >= 0 then
    Inc(FCountX);
  if ACutX2 >= 0 then
    Inc(FCountX);
  if ACutY1 >= 0 then
    Inc(FCountY);
  if ACutY2 >= 0 then
    Inc(FCountY);

  case FCountX of
    0: case FCountY of
        0: FImageParts[0,0] := CutImage(0, 0, FWidth, FHeight);
        1: FImageParts[0,0] := CutImage(0, 0, FWidth, FHeight);
        2: begin
            FImageParts[0,0] := CutImage(0, 0, FWidth, FCutY1);
            FImageParts[0,1] := CutImage(0, FCutY1, FWidth, FCutY2-FCutY1);
            FImageParts[0,2] := CutImage(0, FCutY2, FWidth, FHeight-FCutY2);
          end;
      end;
    1: case FCountY of
        0: FImageParts[0,0] := CutImage(0, 0, FWidth, FHeight);
        1: FImageParts[0,0] := CutImage(0, 0, FWidth, FHeight);
        2: begin
            FImageParts[0,0] := CutImage(0, 0, FWidth, FCutY1);
            FImageParts[0,1] := CutImage(0, FCutY1, FWidth, FCutY2-FCutY1);
            FImageParts[0,2] := CutImage(0, FCutY2, FWidth, FHeight-FCutY2);
          end;
      end;
    2: case FCountY of
        0: begin
            FImageParts[0,0] := CutImage(0, 0, FCutX1, FHeight);
            FImageParts[1,0] := CutImage(FCutX1, 0, FCutX2-FCutX1, FHeight);
            FImageParts[2,0] := CutImage(FCutX2, 0, FWidth-FCutX2, FHeight);
          end;
        1: begin
            FImageParts[0,0] := CutImage(0, 0, FCutX1, FHeight);
            FImageParts[1,0] := CutImage(FCutX1, 0, FCutX2-FCutX1, FHeight);
            FImageParts[2,0] := CutImage(FCutX2, 0, FWidth-FCutX2, FHeight);
          end;
        2: begin
            // Первая линия
            FImageParts[0,0] := CutImage(0, 0, FCutX1, FCutY1);
            FImageParts[1,0] := CutImage(FCutX1, 0, FCutX2-FCutX1, FCutY1);
            FImageParts[2,0] := CutImage(FCutX2, 0, FWidth-FCutX2, FCutY1);
            // Вторая линия
            FImageParts[0,1] := CutImage(0, FCutY1, FCutX1, FCutY2-FCutY1);
            FImageParts[1,1] := CutImage(FCutX1, FCutY1, FCutX2-FCutX1, FCutY2-FCutY1);
            FImageParts[2,1] := CutImage(FCutX2, FCutY1, FWidth-FCutX2, FCutY2-FCutY1);
            // Третья линия
            FImageParts[0,2] := CutImage(0, FCutY2, FCutX1, FHeight-FCutY2);
            FImageParts[1,2] := CutImage(FCutX1, FCutY2, FCutX2-FCutX1, FHeight-FCutY2);
            FImageParts[2,2] := CutImage(FCutX2, FCutY2, FWidth-FCutX2, FHeight-FCutY2);
          end;
      end;
  end;
end;

function TNinePatchImage.CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject;
begin
  Result := nil;
end;

destructor TNinePatchImage.Destroy;
var
  i, j: Integer;
begin
  inherited;
  for i := 0 to 2 do
    for j := 0 to 2 do
      FreeAndNil(FImageParts[i, j]);
  FreeAndNil(FImage);
end;

function TNinePatchImage.GetHeight: Integer;
begin
  Result := 0;
end;

function TNinePatchImage.GetImagePart(const AX, AY: Integer): TObject;
begin
  Result := FImageParts[AX, AY];
end;

function TNinePatchImage.GetWidth: Integer;
begin
  Result := 0;
end;

{ TPainter }

function TPainter.AddStyle(const AName: string): TDrawStyle;
begin
  Result := FDrawStyles.AddStyle(AName);
end;

function TPainter.BaseStyle: TDrawStyle;
begin
  if FDrawStyles.Count > 0 then
    Result := FDrawStyles[0]
  else
    Result := nil;
end;

procedure TPainter.Colorize(const AStyle: TDrawStyle; const AColor: Cardinal);
var
  vPen: TStylePen;
  vBrush: TStyleBrush;
  vFont: TStyleFont;
begin
  for vPen in AStyle.FPens do
  begin
    if vPen.Colorize(AColor) then
      DoColorizePen(vPen, AColor);
  end;
  for vBrush in AStyle.FBrushes do
  begin
    if vBrush.Colorize(AColor) then
      DoColorizeBrush(vBrush, AColor);
  end;
  for vFont in AStyle.FFonts do
  begin
    if vFont.Colorize(AColor) then
      DoColorizeFont(vFont, AColor);
  end;
end;

procedure TPainter.ColorizeBrush(const AStyle: TDrawStyle; const AFillName: string; const AColor: Cardinal);
var
  vBrush: TStyleBrush;
begin
  if Assigned(FCanvas) and Assigned(AStyle) then
  begin
    vBrush := AStyle.Brush[AFillName];
    if Assigned(vBrush) and vBrush.Colorize(AColor) then
      DoColorizeBrush(vBrush, AColor);
  end;
end;

procedure TPainter.ColorizeFont(const AStyle: TDrawStyle; const AFontName: string; const AColor: Cardinal);
var
  vFont: TStyleFont;
begin
  if Assigned(FCanvas) and Assigned(AStyle) then
  begin
    vFont := AStyle.Font[AFontName];
    if Assigned(vFont) and vFont.Colorize(AColor) then
      DoColorizeFont(vFont, AColor);
  end;
end;

procedure TPainter.ColorizePen(const AStyle: TDrawStyle; const AStrokeName: string; const AColor: Cardinal);
var
  vPen: TStylePen;
begin
  if Assigned(FCanvas) and Assigned(AStyle) then
  begin
    vPen := AStyle.Pen[AStrokeName];
    if Assigned(vPen) and vPen.Colorize(AColor) then
      DoColorizePen(vPen, AColor);
  end;
end;

constructor TPainter.Create(const AIcons: TIcons);
begin
  inherited Create;
  FCanvas := nil;
  FIcons := AIcons;
  FDrawStyles := TDrawStyles.Create;
end;

destructor TPainter.Destroy;
begin
  FCanvas := nil;
  FIcons := nil;
  FDrawStyles.Free;
  inherited Destroy;
end;

procedure TPainter.DrawNinePatchImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
var
  vImage: TNinePatchImage absolute AImage;
  X0, Y0: Single;
  vHeight: Single;
  vWidth: Single;
begin
  if SameValue(AOpacity, 0) then
    Exit;

  vWidth := ARect.Right - ARect.Left;
  vHeight := ARect.Bottom - ARect.Top;

  // Если мы не помещаемся в ARect, отрисовываем всю картинку
  if (vImage.Width > vWidth) or (vImage.Height > vHeight) then
  begin
    DoDrawImage(vImage.Image, ARect, AOpacity);
    Exit;
  end;

  X0 := (vWidth - vImage.Width) / 2;
  Y0 := (vHeight - vImage.Height) / 2;

  case vImage.CountX of
    0: case vImage.CountY of
        0: DoDrawImage(vImage[0,0], RectF(ARect.Left+X0, ARect.Top+Y0, ARect.Left+X0+vImage.Width, ARect.Top+Y0+vImage.Height), AOpacity);
        1: DoDrawImage(vImage[0,0], RectF(ARect.Left+0, ARect.Top+Y0, ARect.Left+vWidth, ARect.Top+Y0+vImage.Height), AOpacity);
        2: begin
            DoDrawImage(vImage[0,0], RectF(ARect.Left+X0, ARect.Top+0, ARect.Left+X0+vImage.Width, ARect.Top+vImage.CutY1), AOpacity);
            DoDrawImage(vImage[0,1], RectF(ARect.Left+X0, ARect.Top+vImage.CutY1, ARect.Left+X0+vImage.Width, ARect.Top+vHeight-(vImage.Height-vImage.CutY2)), AOpacity);
            DoDrawImage(vImage[0,2], RectF(ARect.Left+X0, ARect.Top+vHeight-(vImage.Height-vImage.CutY2), ARect.Left+X0+vImage.Width, ARect.Top+vHeight), AOpacity);
          end;
      end;
    1: case vImage.CountY of
        0: DoDrawImage(vImage[0,0], RectF(ARect.Left+X0, ARect.Top+0, ARect.Left+X0+vImage.Width, ARect.Top+vHeight), AOpacity);
        1: DoDrawImage(vImage[0,0], RectF(ARect.Left+0, ARect.Top+0, ARect.Left+vWidth, ARect.Top+vHeight), AOpacity);
        2: begin
            DoDrawImage(vImage[0,0], RectF(ARect.Left+0, ARect.Top+0, ARect.Left+vWidth, ARect.Top+vImage.CutY1), AOpacity);
            DoDrawImage(vImage[0,1], RectF(ARect.Left+0, ARect.Top+vImage.CutY1, ARect.Left+vWidth, ARect.Top+vHeight-(vImage.Height-vImage.CutY2)), AOpacity);
            DoDrawImage(vImage[0,2], RectF(ARect.Left+0, ARect.Top+vHeight-(vImage.Height-vImage.CutY2), ARect.Left+vWidth, ARect.Top+vHeight), AOpacity);
          end;
      end;
    2: case vImage.CountY of
        0: begin
            DoDrawImage(vImage[0,0], RectF(ARect.Left+0, ARect.Top+Y0, ARect.Left+vImage.CutX1, ARect.Top+Y0+vImage.Height), AOpacity);
            DoDrawImage(vImage[1,0], RectF(ARect.Left+vImage.CutX1, ARect.Top+Y0, ARect.Left+vWidth-(vImage.Width-vImage.CutX2), ARect.Top+Y0+vImage.Height), AOpacity);
            DoDrawImage(vImage[2,0], RectF(ARect.Left+vWidth-(vImage.Width-vImage.CutX2), ARect.Top+Y0, ARect.Left+vWidth, ARect.Top+Y0+vImage.Height), AOpacity);
          end;
        1: begin
            DoDrawImage(vImage[0,0], RectF(ARect.Left+0, ARect.Top+0, ARect.Left+vImage.CutX1, ARect.Top+vHeight), AOpacity);
            DoDrawImage(vImage[1,0], RectF(ARect.Left+vImage.CutX1, ARect.Top+0, ARect.Left+vWidth-(vImage.Width-vImage.CutX2), ARect.Top+vHeight), AOpacity);
            DoDrawImage(vImage[2,0], RectF(ARect.Left+vWidth-(vImage.Width-vImage.CutX2), ARect.Top+0, ARect.Left+vWidth, ARect.Top+vHeight), AOpacity);
          end;
        2: begin
            // Первая линия
            DoDrawImage(vImage[0,0], RectF(ARect.Left+0, ARect.Top+0, ARect.Left+vImage.CutX1, ARect.Top+vImage.CutY1), AOpacity);
            DoDrawImage(vImage[1,0], RectF(ARect.Left+vImage.CutX1, ARect.Top+0, ARect.Left+vWidth-(vImage.Width-vImage.CutX2), ARect.Top+vImage.CutY1), AOpacity);
            DoDrawImage(vImage[2,0], RectF(ARect.Left+vWidth-(vImage.Width-vImage.CutX2), ARect.Top+0, ARect.Left+vWidth, ARect.Top+vImage.CutY1), AOpacity);
            // Вторая линия
            DoDrawImage(vImage[0,1], RectF(ARect.Left+0, ARect.Top+vImage.CutY1, ARect.Left+vImage.CutX1, ARect.Top+vHeight-(vImage.Height-vImage.CutY2)), AOpacity);
            DoDrawImage(vImage[1,1], RectF(ARect.Left+vImage.CutX1, ARect.Top+vImage.CutY1, ARect.Left+vWidth-(vImage.Width-vImage.CutX2), ARect.Top+vHeight-(vImage.Height-vImage.CutY2)), AOpacity);
            DoDrawImage(vImage[2,1], RectF(ARect.Left+vWidth-(vImage.Width-vImage.CutX2), ARect.Top+vImage.CutY1, ARect.Left+vWidth, ARect.Top+vHeight-(vImage.Height-vImage.CutY2)), AOpacity);
            // Третья линия
            DoDrawImage(vImage[0,2], RectF(ARect.Left+0, ARect.Top+vHeight-(vImage.Height-vImage.CutY2), ARect.Left+vImage.CutX1, ARect.Top+vHeight), AOpacity);
            DoDrawImage(vImage[1,2], RectF(ARect.Left+vImage.CutX1, ARect.Top+vHeight-(vImage.Height-vImage.CutY2), ARect.Left+vWidth-(vImage.Width-vImage.CutX2), ARect.Top+vHeight), AOpacity);
            DoDrawImage(vImage[2,2], RectF(ARect.Left+vWidth-(vImage.Width-vImage.CutX2), ARect.Top+vHeight-(vImage.Height-vImage.CutY2), ARect.Left+vWidth, ARect.Top+vHeight), AOpacity);
          end;
      end;
  end;
end;

procedure TPainter.DrawBezier(const AStyle: TDrawStyle; const AStrokeName: string; const APoints: PPointF; const ACount: Integer);
begin
  if Assigned(FCanvas) and Assigned(AStyle) then
    DoDrawBezier(AStyle.Pen[AStrokeName], APoints, ACount);
end;

procedure TPainter.DrawCircle(const AStyle: TDrawStyle; const AFillName, AStrokeName: string;
  const ACenterPoint: TPointF; const ARadius: Single);
begin
  DrawEllipse(AStyle, AFillName, AStrokeName, RectF(ACenterPoint.X - ARadius,
    ACenterPoint.Y - ARadius, ACenterPoint.X + ARadius, ACenterPoint.Y + ARadius));
end;

procedure TPainter.DrawContext(const AContext: TDrawContext);
begin
  if Assigned(AContext) then
    DoDrawContext(AContext);
end;

procedure TPainter.DrawEllipse(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const ARect: TRectF);
begin
  if Assigned(FCanvas) and Assigned(AStyle) then
    DoDrawEllipse(AStyle.Brush[AFillName], AStyle.Pen[AStrokeName], ARect);
end;

procedure TPainter.DrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
begin
  DoDrawImage(AImage, ARect, AOpacity);
end;

procedure TPainter.DrawImage(const AStyle: TDrawStyle; const AImageName: string; const ARect: TRectF; const AOpacity: Single);
var
  vImage: TStyleImage;
begin
  if Assigned(FCanvas) and Assigned(AStyle) and not ARect.IsEmpty then
  begin
    vImage := AStyle.Image[AImageName];
    if Assigned(vImage) and Assigned(vImage.Image) then
    begin
      if vImage.IsNinePatch then
        DrawNinePatchImage(vImage.Image, ARect, AOpacity)
      else
        DoDrawImage(vImage.Image, ARect, AOpacity);
    end;
  end;
end;

procedure TPainter.DrawLine(const AStyle: TDrawStyle; const AStrokeName: string; const APoint1, APoint2: TPointF);
begin
  if Assigned(FCanvas) and Assigned(AStyle) then
    DoDrawLine(AStyle.Pen[AStrokeName], APoint1, APoint2);
end;

procedure TPainter.DrawPath(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const APath: TObject);
begin
  if Assigned(FCanvas) and Assigned(AStyle) then
    DoDrawPath(AStyle.Brush[AFillName], AStyle.Pen[AStrokeName], APath);
end;

procedure TPainter.DrawPie(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const ARect: TRectF;
  const AStartAngle, AEndAngle: Single);
begin
  if Assigned(FCanvas) and Assigned(AStyle) then
    DoDrawPie(AStyle.Brush[AFillName], AStyle.Pen[AStrokeName], ARect, AStartAngle, AEndAngle);
end;

procedure TPainter.DrawPixel(const AColor: Cardinal; const APoint: TPointF);
begin
  if Assigned(FCanvas) and ((AColor and $FF000000) shr 24 > 0) then
    DoDrawPixel(AColor, APoint);
end;

procedure TPainter.DrawPoint(const AStyle: TDrawStyle; const AFillName: string; const APoint: TPointF; const ASize: Single = 6);
var
  vFill: TStyleBrush;
begin
  if SameValue(ASize, 1) then
  begin
    vFill := AStyle.Brush[AFillName];
    if Assigned(vFill) then
      DrawPixel(vFill.Color, APoint);
  end
  else
    DrawCircle(AStyle, AFillName, '', APoint, ASize / 2);
end;

procedure TPainter.DrawPolyline(const AStyle: TDrawStyle; const AStrokeName: string; const APoints: array of TPointF);
begin
  if Assigned(FCanvas) and Assigned(AStyle) then
    DoDrawPolyline(AStyle.Pen[AStrokeName], @APoints, Length(APoints));
end;

procedure TPainter.DrawPolyline(const AStyle: TDrawStyle; const AStrokeName: string; const APoints: PPointF; const ACount: Integer);
begin
  if Assigned(FCanvas) and Assigned(AStyle) then
    DoDrawPolyline(AStyle.Pen[AStrokeName], APoints, ACount);
end;

procedure TPainter.DrawRect(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const ARect: TRectF);
begin
  if Assigned(FCanvas) and Assigned(AStyle) and not ARect.IsEmpty then
    DoDrawRect(AStyle.Brush[AFillName], AStyle.Pen[AStrokeName], ARect);
end;

procedure TPainter.DrawRegion(const AStyle: TDrawStyle; const AFillName, AStrokeName: string;
  const APoints: PPointF; const ACount: Integer);
begin
  if Assigned(FCanvas) and Assigned(AStyle) then
    DoDrawRegion(AStyle.Brush[AFillName], AStyle.Pen[AStrokeName], APoints, ACount);
end;

function TPainter.DrawText(const AStyle: TDrawStyle; const AFillName, AFontName: string; const AText: string;
  const ARect: TRectF; const AOptions: Cardinal = 0; const AAngle: Single = 0): TRectF;
begin
  //DrawRect(AStyle, 'tick.selected', 'tick.line', ARect);
  if Assigned(FCanvas) and Assigned(AStyle) and (AText <> '') and not ARect.IsEmpty then
    Result := DoDrawText(AStyle.Brush[AFillName], AStyle.Font[AFontName], AText, ARect, AOptions, AAngle)
  else
    Result := TRectF.Empty;
end;

function TPainter.GetStyle(const AName: string): TDrawStyle;
begin
  Result := FDrawStyles.StyleByName(AName);
end;

procedure TPainter.InvertRect(const ARect: TRectF);
begin
  DoInvertRect(ARect);
end;

procedure TPainter.RestoreColors(const AStyle: TDrawStyle);
var
  vPen: TStylePen;
  vBrush: TStyleBrush;
  vFont: TStyleFont;
begin
  for vPen in AStyle.FPens do
    vPen.RestoreColor;
  for vBrush in AStyle.FBrushes do
    vBrush.RestoreColor;
  for vFont in AStyle.FFonts do
    vFont.RestoreColor;
end;

procedure TPainter.SetCanvas(const Value: TObject);
begin
  FCanvas := Value;
end;

function TPainter.StyleExists(const AName: string): Boolean;
begin
  Result := FDrawStyles.StyleExists(AName);
end;

function TPainter.TextExtents(const AStyle: TDrawStyle; const AFontName: string; const AText: string): TSizeF;
begin
  if Assigned(FCanvas) and Assigned(AStyle) and (AText <> '')  then
    Result := GetTextExtents(AStyle.Font[AFontName], AText)
  else begin
    Result.cx := 0;
    Result.cy := 0;
  end;
end;

function TPainter.TextHeight(const AStyle: TDrawStyle; const AFontName: string; const AText: string): Single;
var
  vSize: TSizeF;
begin
  vSize := TextExtents(AStyle, AFontName, AText);
  Result := vSize.Height;
end;

function TPainter.TextWidth(const AStyle: TDrawStyle; const AFontName: string; const AText: string): Single;
var
  vSize: TSizeF;
begin
  vSize := TextExtents(AStyle, AFontName, AText);
  Result := vSize.Width;
end;

end.
