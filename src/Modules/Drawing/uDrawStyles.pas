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
  Types, Generics.Collections, uModule, uJSON, uConsts;

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
  TRenderQuality = (rqLow, rqNormal, rqHigh);

  TPainter = class;

  TStyleObject = class
  private
    procedure SetObject(const Value: TObject);
  protected
    FName: string;
    FObject: TObject;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    property Name: string read FName;
    property NativeObject: TObject read FObject write SetObject;
  end;

  TColoredStyleObject = class(TStyleObject)
  protected
    FColor: Cardinal;
  public
    constructor Create(const AName: string; const AColor: Cardinal);

    property Color: Cardinal read FColor write FColor;
  end;

  TStylePen = class(TColoredStyleObject)
  private
    FInitialStrokeWidth: Single;
    FStrokeWidth: Single;
    FStrokeStyle: TPenStyle;
    FQuality: TRenderQuality;
  public
    constructor Create(const AName: string; const AColor: Cardinal = $FF000000;
      const AWidth: Single = 1.0; const AStyle: TPenStyle = psSolid;
      const AQuality: TRenderQuality = rqNormal);

    procedure ApplyZoom(const AZoom: Single);
    procedure RestoreZoom;

    property Width: Single read FStrokeWidth;
    property Style: TPenStyle read FStrokeStyle;
    property Quality: TRenderQuality read FQuality;
  end;

  TStyleBrush = class(TColoredStyleObject)
  private
    FFillBackColor: Cardinal;
    FFillGradientKind: TGradientKind;
    FFillStyle: TBrushStyle;
    FFillRect: TRectF;
    procedure SetRect(const Value: TRectF);
  public
    constructor Create(const AName: string; const AColor: Cardinal = $FF000000; const ABackColor: Cardinal = $FFFFFFFF;
      const AGradientKind: TGradientKind = gkNone; const AStyle: TBrushStyle = bsSolid);

    property BackColor: Cardinal read FFillBackColor;
    property GradientKind: TGradientKind read FFillGradientKind;
    property Style: TBrushStyle read FFillStyle;
    property Rect: TRectF read FFillRect write SetRect;
  end;

  TStyleFont = class(TColoredStyleObject)
  private
    FFontFamily: string;
    FFontStyle: Integer;
    FInitialFontSize: Single;
    FFontSize: Single;
    FFontAngle: Single;
    FQuality: TRenderQuality;
  public
    constructor Create(const AName: string; const AFamily: string = 'Tahoma'; const ASize: Single = 20.0;
      const AColor: Cardinal = $FF000000; const AStyle: Integer = FontStyleRegular; const AAngle: Single = 0;
      const AQuality: TRenderQuality = rqNormal);

    procedure ApplyZoom(const AZoom: Single);
    procedure RestoreZoom;

    property Family: string read FFontFamily;
    property Style: Integer read FFontStyle;
    property Size: Single read FFontSize;
    property Angle: Single read FFontAngle;
    property Quality: TRenderQuality read FQuality;
  end;

  TStyleImage = class(TStyleObject)
  private
    FFileName: string;
    FCutRect: TRect;
    FTransparent: Boolean;
  public
    constructor Create(const AName: string; const AFileName: string; const ATransparent: Boolean);

    function IsNinePatch: Boolean;

    property FileName: string read FFileName;
    property Transparent: Boolean read FTransparent;
    property CutRect: TRect read FCutRect write FCutRect;
  end;

  TDrawStyle = class
  private
    FName: string;
    FBrushes: TObjectDictionary<string, TStyleBrush>;
    FPens: TObjectDictionary<string, TStylePen>;
    FFonts: TObjectDictionary<string, TStyleFont>;
    FImages: TObjectDictionary<string, TStyleImage>;
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
      const AWidth: Single = 1.0; const AStyle: TPenStyle = psSolid;
      const AQuality: TRenderQuality = rqNormal): TStylePen;
    function AddFillParams(const AName: string; const AColor: Cardinal = $FF000000;
      const ABackColor: Cardinal = $FFFFFFFF; const AGradientKind: TGradientKind = gkNone;
      const AStyle: TBrushStyle = bsSolid): TStyleBrush;
    function AddFontParams(const AName: string; const AFamily: string = 'Tahoma';
      const ASize: Single = 20.0; const AColor: Cardinal = $FF000000;
      const AStyle: Integer = FontStyleRegular; const AAngle: Single = 0 {0..360};
      const AQuality: TRenderQuality = rqNormal): TStyleFont;
    function AddImageParams(const AName: string; const AFileName: string;
      const ATransparent: Boolean = False): TStyleImage;
    procedure CreateDrawObjects(const APainter: TObject);

    procedure ApplyZoom(const AZoom: Single);
    procedure RestoreZoom;
    procedure Reset;

    property Name: string read FName write FName;
    property Brush[const AName: string]: TStyleBrush read GetBrushByName;
    property Pen[const AName: string]: TStylePen read GetPenByName;
    property Font[const AName: string]: TStyleFont read GetFontByName;
    property Image[const AName: string]: TStyleImage read GetImageByName;
  end;

  TDrawStyles = class
  private
    FStyles: TObjectDictionary<string, TDrawStyle>;
  public
    constructor Create;
    destructor Destroy; override;

    function AddStyle(const AName: string): TDrawStyle;
    function StyleByName(const AName: string): TDrawStyle;
    function StyleExists(const AName: string): Boolean;

    procedure Load(const AArray: TJSONArray);
    function Save: TJSONArray;
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
    FPainter: TObject;
    FImage: TObject;
    function GetWidth: Integer; virtual;
    function GetHeight: Integer; virtual;
    function CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject; virtual;
  public
    constructor Create(const APainter: TObject; const AImage: TObject; const ACutX1, ACutX2, ACutY1, ACutY2: Integer);
    destructor Destroy; override;

    property ImageParts[const AX, AY: Integer]: TObject read GetImagePart; default;
    property Image: TObject read FImage;
    property Painter: TObject read FPainter;
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
  private
    FClientRect: TRectF;
    function GetHeight: Integer;
    function GetWidth: Integer;
  protected
    procedure DoSetSize(const AWidth, AHeight: Single); virtual;
  public
    constructor Create(const AWidth, AHeight: Single);

    procedure SetSize(const AWidth, AHeight: Single);
    procedure SaveToFile(const AFileName: string); virtual;

    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property ClientRect: TRectF read FClientRect;
  end;

  TPainter = class
  private
    procedure DrawNinePatchImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
    function Colorize(const AStyleObject: TColoredStyleObject; const AColor: Cardinal): Cardinal;
  protected
    FDrawStyles: TDrawStyles;
    FContext: TDrawContext;
    procedure DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF); virtual; abstract;
    procedure DoDrawPie(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF;
      const AStartAngle, AEndAngle: Single); virtual; abstract;
    procedure DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF); virtual; abstract;
    procedure DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer); virtual; abstract;
    procedure DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject); virtual; abstract;
    procedure DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer); virtual; abstract;
    procedure DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer); virtual; abstract;
    procedure DoDrawRect(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF); virtual; abstract;
    procedure DoDrawText(const AFont: TStyleFont; const AText: string; const ARect: TRectF;
      const AOptions: Cardinal; const AAngle: Single); virtual; abstract;
    function GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF; virtual; abstract;
    procedure DoInvertRect(const ARect: TRectF); virtual; abstract;
    procedure DoClipRect(const ARect: TRectF); virtual; abstract;
    procedure DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single); virtual; abstract;
    procedure DoDrawContext(const AContext: TDrawContext); virtual; abstract;

    procedure DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal); virtual; abstract;
    procedure DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal); virtual; abstract;
    procedure DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal); virtual; abstract;
  public
    constructor Create(const AContainer: TObject); virtual;
    destructor Destroy; override;

    procedure CreateBrush(const AFill: TStyleBrush); virtual; abstract;
    procedure CreatePen(const AStroke: TStylePen); virtual; abstract;
    procedure CreateFont(const AFont: TStyleFont); virtual; abstract;
    procedure CreateImage(const AImage: TStyleImage); virtual; abstract;
    function CreateDrawContext(const AWidth, AHeight: Single): TDrawContext; virtual;

    procedure DrawCircle(const AStyle: TDrawStyle; const AFillName, AStrokeName: string;
      const ACenterPoint: TPointF; const ARadius: Single; const AColor: Cardinal = 0);
    procedure DrawEllipse(const AStyle: TDrawStyle; const AFillName, AStrokeName: string;
      const ARect: TRectF; const AColor: Cardinal = 0);
    procedure DrawPie(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const ARect: TRectF;
      const AStartAngle, AEndAngle: Single; const AColor: Cardinal = 0);
    procedure DrawLine(const AStyle: TDrawStyle; const AStrokeName: string; const APoint1, APoint2: TPointF;
      const ASnapToPixels: Boolean = True; const AColor: Cardinal = 0);
    procedure DrawPoint(const AStyle: TDrawStyle; const AFillName: string; const APoint: TPointF;
      const ASize: Single = 6; const AColor: Cardinal = 0);
    procedure DrawRegion(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const APoints: PPointF;
      const ACount: Integer; const AColor: Cardinal = 0);
    procedure DrawPath(const AStyle: TDrawStyle; const AFillName, AStrokeName: string;
      const APath: TObject; const AColor: Cardinal = 0);
    procedure DrawPolyline(const AStyle: TDrawStyle; const AStrokeName: string; const APoints: PPointF;
      const ACount: Integer; const AColor: Cardinal = 0); overload;
    procedure DrawPolyline(const AStyle: TDrawStyle; const AStrokeName: string; const APoints: array of TPointF;
      const AColor: Cardinal = 0); overload;
    procedure DrawBezier(const AStyle: TDrawStyle; const AStrokeName: string; const APoints: PPointF;
      const ACount: Integer; const AColor: Cardinal = 0);
    procedure DrawRect(const AStyle: TDrawStyle; const AFillName, AStrokeName: string;
      const ARect: TRectF; const ASnapToPixels: Boolean = True; const AColor: Cardinal = 0);
    procedure DrawText(const AStyle: TDrawStyle; const AFontName: string; const AText: string;
      const ARect: TRectF; const AOptions: Cardinal = 0; const AAngle: Single = 0; const AColor: Cardinal = 0);
    function TextWidth(const AStyle: TDrawStyle; const AFontName: string; const AText: string): Single;
    function TextHeight(const AStyle: TDrawStyle; const AFontName: string; const AText: string): Single;
    function TextExtents(const AStyle: TDrawStyle; const AFontName: string; const AText: string): TSizeF;
    procedure InvertRect(const ARect: TRectF);
    procedure ClipRect(const ARect: TRectF);

    procedure DrawImage(const AStyle: TDrawStyle; const AImageName: string; const ARect: TRectF; const AOpacity: Single = 1.0); overload;
    procedure DrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single = 1.0); overload;

    procedure DrawContext(const AContext: TDrawContext);

    function AddStyle(const AName: string): TDrawStyle;
    function GetStyle(const AName: string): TDrawStyle;
    function StyleExists(const AName: string): Boolean;

    procedure BeginPaint; virtual;
    procedure EndPaint; virtual;

    function SetContext(const AContext: TDrawContext): TDrawContext; virtual;
    property Context: TDrawContext read FContext;
  end;

  TPainterClass = class of TPainter;

type
  PPointFArray = ^TPointFArray;
  TPointFArray = array[0..0] of TPointF;

implementation

uses
  SysUtils, IOUtils, Math;

{ TDrawStyles }

function TDrawStyles.AddStyle(const AName: string): TDrawStyle;
var
  vName: string;
begin
  vName := AnsiLowerCase(Trim(AName));
  if vName = '' then
    raise Exception.Create('Draw style must have a name');
  if FStyles.ContainsKey(vName) then
    raise Exception.Create('Style ' + vName + ' already exists');

  Result := TDrawStyle.Create(vName);
  FStyles.Add(vName, Result);
end;

constructor TDrawStyles.Create;
begin
  inherited Create;
  FStyles := TObjectDictionary<string, TDrawStyle>.Create([doOwnsValues]);
end;

destructor TDrawStyles.Destroy;
begin
  FreeAndNil(FStyles);
  inherited Destroy;
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
      if not FStyles.ContainsKey(vName) then
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
  for vStyle in FStyles.Values do
    Result.Add(vStyle.Save)
end;

function TDrawStyles.StyleByName(const AName: string): TDrawStyle;
var
  vName: string;
begin
  vName := AnsiLowerCase(Trim(AName));
  if not FStyles.TryGetValue(vName, Result) then
    Assert(False, Format('Стиль [%s] не задан', [vName]));
end;

function TDrawStyles.StyleExists(const AName: string): Boolean;
var
  vName: string;
begin
  vName := AnsiLowerCase(Trim(AName));
  Result := FStyles.ContainsKey(vName);
end;

{ TDrawStyle }

procedure TDrawStyle.ApplyZoom(const AZoom: Single);
var
  vPen: TStylePen;
  vFont: TStyleFont;
begin
  for vPen in FPens.Values do
    vPen.ApplyZoom(AZoom);
  for vFont in FFonts.Values do
    vFont.ApplyZoom(AZoom);
end;

constructor TDrawStyle.Create(const AName: string);
begin
  inherited Create;

  FName := AName;
  FBrushes := TObjectDictionary<string, TStyleBrush>.Create([doOwnsValues]);
  FPens := TObjectDictionary<string, TStylePen>.Create([doOwnsValues]);
  FFonts := TObjectDictionary<string, TStyleFont>.Create([doOwnsValues]);
  FImages := TObjectDictionary<string, TStyleImage>.Create([doOwnsValues]);
end;

procedure TDrawStyle.CreateDrawObjects(const APainter: TObject);
var
  vPainter: TPainter absolute APainter;
  vBrush: TStyleBrush;
  vPen: TStylePen;
  vFont: TStyleFont;
  vImage: TStyleImage;
begin
  for vBrush in FBrushes.Values do
    if vBrush.Style <> bsClear then
      vPainter.CreateBrush(vBrush);
  for vPen in FPens.Values do
    if vPen.Style <> psClear then
      vPainter.CreatePen(vPen);
  for vFont in FFonts.Values do
    vPainter.CreateFont(vFont);
  for vImage in FImages.Values do
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
  if not FBrushes.TryGetValue(AName, Result) then
    Result := nil;
end;

function TDrawStyle.GetFontByName(const AName: string): TStyleFont;
begin
  if not FFonts.TryGetValue(AName, Result) then
    Result := nil;
end;

function TDrawStyle.GetImageByName(const AName: string): TStyleImage;
begin
  if not FImages.TryGetValue(AName, Result) then
    Result := nil;
end;

function TDrawStyle.GetPenByName(const AName: string): TStylePen;
begin
  if not FPens.TryGetValue(AName, Result) then
    Result := nil;
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
  for vPen in FPens.Values do
    vPen.RestoreZoom;
  for vFont in FFonts.Values do
    vFont.RestoreZoom;
end;

procedure TDrawStyle.Reset;
begin
  FBrushes.Clear;
  FPens.Clear;
  FFonts.Clear;
  FImages.Clear;
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
    for vBrush in FBrushes.Values do
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
    for vPen in FPens.Values do
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
    for vFont in FFonts.Values do
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
    for vImage in FImages.Values do
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
  Assert(not FBrushes.ContainsKey(AName), 'Объект с таким именем уже есть');
  Result := TStyleBrush.Create(AName, AColor, ABackColor, AGradientKind, AStyle);
  FBrushes.Add(AName, Result);
end;

function TDrawStyle.AddFontParams(const AName: string; const AFamily: string; const ASize: Single;
  const AColor: Cardinal; const AStyle: Integer; const AAngle: Single; const AQuality: TRenderQuality): TStyleFont;
begin
  Assert(not FFonts.ContainsKey(AName), 'Объект с таким именем уже есть');
  Result := TStyleFont.Create(AName, AFamily, ASize, AColor, AStyle, AAngle, AQuality);
  FFonts.Add(AName, Result);
end;

function TDrawStyle.AddImageParams(const AName, AFileName: string; const ATransparent: Boolean = False): TStyleImage;
var
  vFileName: string;
begin
  Assert(not FImages.ContainsKey(AName), 'Объект с таким именем уже есть');

  if not TFile.Exists(AFileName) then
    vFileName := 'res\images\' + AFileName
  else
    vFileName := AFileName;
  Result := TStyleImage.Create(AName, vFileName, ATransparent);
  FImages.Add(AName, Result);
end;

function TDrawStyle.AddStrokeParams(const AName: string; const AColor: Cardinal;
  const AWidth: Single; const AStyle: TPenStyle; const AQuality: TRenderQuality): TStylePen;
begin
  Assert(not FPens.ContainsKey(AName), 'Объект с таким именем уже есть');
  Result := TStylePen.Create(AName, AColor, AWidth, AStyle, AQuality);
  FPens.Add(AName, Result);
end;

{ TStylePen }

procedure TStylePen.ApplyZoom(const AZoom: Single);
begin
  FStrokeWidth := FStrokeWidth * AZoom;
end;

constructor TStylePen.Create(const AName: string; const AColor: Cardinal; const AWidth: Single;
  const AStyle: TPenStyle; const AQuality: TRenderQuality);
begin
  inherited Create(AName, AColor);
  FInitialStrokeWidth := AWidth;
  FStrokeWidth := AWidth;
  FStrokeStyle := AStyle;
  FQuality := AQuality;
end;

procedure TStylePen.RestoreZoom;
begin
  FStrokeWidth := FInitialStrokeWidth;
end;

{ TStyleBrush }

constructor TStyleBrush.Create(const AName: string; const AColor, ABackColor: Cardinal;
  const AGradientKind: TGradientKind; const AStyle: TBrushStyle);
begin
  inherited Create(AName, AColor);
  FFillBackColor := ABackColor;
  FFillGradientKind := AGradientKind;
  FFillRect := TRectF.Empty;
  FFillStyle := AStyle;
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

constructor TStyleFont.Create(const AName: string; const AFamily: string; const ASize: Single;
  const AColor: Cardinal; const AStyle: Integer; const AAngle: Single; const AQuality: TRenderQuality);
begin
  inherited Create(AName, AColor);
  FFontFamily := AFamily;
  FInitialFontSize := ASize;
  FFontSize := ASize;
  FFontStyle := AStyle;
  FFontAngle := AAngle;
  FQuality := AQuality;
end;

procedure TStyleFont.RestoreZoom;
begin
  FFontSize := FInitialFontSize;
end;

{ TStyleImage }

constructor TStyleImage.Create(const AName: string; const AFileName: string; const ATransparent: Boolean);
begin
  inherited Create(AName);
  FFileName := AFileName;
  FTransparent := ATransparent;
  FCutRect := Rect(-1, -1, -1, -1);
end;

function TStyleImage.IsNinePatch: Boolean;
begin
  Result := (FCutRect.Left >= 0) or (FCutRect.Top >= 0) or (FCutRect.Right >= 0) or (FCutRect.Bottom >= 0);
end;

{ TNinePatchImage }

constructor TNinePatchImage.Create(const APainter: TObject; const AImage: TObject; const ACutX1, ACutX2, ACutY1, ACutY2: Integer);
begin
  inherited Create;

  FPainter := APainter;
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
  FPainter := nil;
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

procedure TPainter.BeginPaint;
begin
end;

procedure TPainter.ClipRect(const ARect: TRectF);
begin
  if Assigned(FContext) then
    DoClipRect(ARect);
end;

function TPainter.Colorize(const AStyleObject: TColoredStyleObject; const AColor: Cardinal): Cardinal;
var
  vPen: TStylePen absolute AStyleObject;
  vBrush: TStyleBrush absolute AStyleObject;
  vFont: TStyleFont absolute AStyleObject;
begin
  Result := 0;
  if not Assigned(AStyleObject) or (AColor = 0) then
    Exit;
  if not Assigned(AStyleObject.NativeObject) then
    Exit;

  if AStyleObject.Color = AColor then
    Result := 0
  else begin
    Result := AStyleObject.Color;
    AStyleObject.Color := AColor;
    if AStyleObject is TStyleBrush then
      DoColorizeBrush(TStyleBrush(AStyleObject), AColor)
    else if AStyleObject is TStylePen then
      DoColorizePen(TStylePen(AStyleObject), AColor)
    else if AStyleObject is TStyleFont then
      DoColorizeFont(TStyleFont(AStyleObject), AColor);
  end;
end;

constructor TPainter.Create(const AContainer: TObject);
begin
  inherited Create;
  FDrawStyles := TDrawStyles.Create;
end;

function TPainter.CreateDrawContext(const AWidth, AHeight: Single): TDrawContext;
begin
  Result := nil;
end;

destructor TPainter.Destroy;
begin
  FreeAndNil(FDrawStyles);
  FreeAndNil(FContext);
  inherited Destroy;
end;

procedure TPainter.DrawNinePatchImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
var
  vImage: TNinePatchImage absolute AImage;
  X0, Y0: Single;
  vHeight: Single;
  vWidth: Single;
begin
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

procedure TPainter.DrawBezier(const AStyle: TDrawStyle; const AStrokeName: string; const APoints: PPointF;
  const ACount: Integer; const AColor: Cardinal = 0);
var
  vStroke: TStylePen;
  vSavedStrokeColor: Cardinal;
begin
  if Assigned(FContext) and Assigned(AStyle) and ((ACount - 1) mod 3 > 0) then
  begin
    vStroke := AStyle.Pen[AStrokeName];
    if Assigned(vStroke) and (vStroke.Width >= 0.3) then
    begin
      if AColor = 0 then
        DoDrawBezier(vStroke, APoints, ACount)
      else begin
        vSavedStrokeColor := Colorize(vStroke, AColor);
        try
          DoDrawBezier(vStroke, APoints, ACount);
        finally
          Colorize(vStroke, vSavedStrokeColor);
        end;
      end;
    end;
  end;
end;

procedure TPainter.DrawCircle(const AStyle: TDrawStyle; const AFillName, AStrokeName: string;
  const ACenterPoint: TPointF; const ARadius: Single; const AColor: Cardinal = 0);
begin
  DrawEllipse(AStyle, AFillName, AStrokeName, RectF(ACenterPoint.X - ARadius,
    ACenterPoint.Y - ARadius, ACenterPoint.X + ARadius, ACenterPoint.Y + ARadius), AColor);
end;

procedure TPainter.DrawContext(const AContext: TDrawContext);
begin
  if Assigned(AContext) then
    DoDrawContext(AContext);
end;

procedure TPainter.DrawEllipse(const AStyle: TDrawStyle; const AFillName, AStrokeName: string;
  const ARect: TRectF; const AColor: Cardinal = 0);
var
  vStroke: TStylePen;
  vFill: TStyleBrush;
  vSavedFillColor: Cardinal;
  vSavedStrokeColor: Cardinal;
begin
  if Assigned(FContext) and Assigned(AStyle) and not ARect.IsEmpty then
  begin
    vFill := AStyle.Brush[AFillName];
    vStroke := AStyle.Pen[AStrokeName];
    if Assigned(vStroke) and (vStroke.Width < 0.3) then
      vStroke := nil;
    if not Assigned(vFill) and not Assigned(vStroke) then
      Exit;

    if AColor = 0 then
      DoDrawEllipse(vFill, vStroke, ARect)
    else begin
      vSavedFillColor := Colorize(vFill, AColor);
      vSavedStrokeColor := Colorize(vStroke, AColor);
      try
        DoDrawEllipse(vFill, vStroke, ARect);
      finally
        Colorize(vFill, vSavedFillColor);
        Colorize(vStroke, vSavedStrokeColor);
      end;
    end;
  end;
end;

procedure TPainter.DrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
begin
  if Assigned(AImage) and not ARect.IsEmpty and not IsZero(AOpacity) then
    DoDrawImage(AImage, ARect, AOpacity);
end;

procedure TPainter.DrawImage(const AStyle: TDrawStyle; const AImageName: string; const ARect: TRectF; const AOpacity: Single);
var
  vImage: TStyleImage;
begin
  if Assigned(FContext) and Assigned(AStyle) and not ARect.IsEmpty and not IsZero(AOpacity) then
  begin
    vImage := AStyle.Image[AImageName];
    if Assigned(vImage) and Assigned(vImage.NativeObject) then
    begin
      if vImage.IsNinePatch then
        DrawNinePatchImage(vImage.NativeObject, ARect, AOpacity)
      else
        DoDrawImage(vImage.NativeObject, ARect, AOpacity);
    end;
  end;
end;

procedure TPainter.DrawLine(const AStyle: TDrawStyle; const AStrokeName: string;
  const APoint1, APoint2: TPointF; const ASnapToPixels: Boolean = True; const AColor: Cardinal = 0);
var
  vStroke: TStylePen;
  vSavedStrokeColor: Cardinal;
  vPoint1, vPoint2: TPointF;

  function RoundPointF(const APoint: TPointF; const ANeedRound: Boolean): TPointF;
  begin
    if ANeedRound then
      Result := PointF(Round(APoint.X), Round(APoint.Y))
    else
      Result := APoint;
  end;
begin
  if Assigned(FContext) and Assigned(AStyle) and not APoint1.EqualsTo(APoint2) then
  begin
    vStroke := AStyle.Pen[AStrokeName];
    if Assigned(vStroke) and (vStroke.Width >= 0.3) then
    begin
      if ASnapToPixels then
      begin
        vPoint1 := APoint1.Round;
        vPoint2 := APoint2.Round;
        if SameValue(vStroke.Width, Round(vStroke.Width), 0.1) and (Round(vStroke.Width) div 2 = 1) then
        begin
          vPoint1.Offset(-0.5, -0.5);
          vPoint2.Offset(-0.5, -0.5);
        end;
      end
      else begin
        vPoint1 := APoint1;
        vPoint2 := APoint2;
      end;

      if AColor = 0 then
        DoDrawLine(vStroke, vPoint1, vPoint2)
      else begin
        vSavedStrokeColor := Colorize(vStroke, AColor);
        try
          DoDrawLine(vStroke, vPoint1, vPoint2);
        finally
          Colorize(vStroke, vSavedStrokeColor);
        end;
      end;
    end;
  end;
end;

procedure TPainter.DrawPath(const AStyle: TDrawStyle; const AFillName, AStrokeName: string;
  const APath: TObject; const AColor: Cardinal = 0);
var
  vStroke: TStylePen;
  vFill: TStyleBrush;
  vSavedFillColor: Cardinal;
  vSavedStrokeColor: Cardinal;
begin
  if Assigned(FContext) and Assigned(AStyle) and Assigned(APath) then
  begin
    vFill := AStyle.Brush[AFillName];
    vStroke := AStyle.Pen[AStrokeName];
    if Assigned(vStroke) and (vStroke.Width < 0.3) then
      vStroke := nil;

    if not Assigned(vFill) and not Assigned(vStroke) then
      Exit;

    if AColor = 0 then
      DoDrawPath(vFill, vStroke, APath)
    else begin
      vSavedFillColor := Colorize(vFill, AColor);
      vSavedStrokeColor := Colorize(vStroke, AColor);
      try
        DoDrawPath(vFill, vStroke, APath);
      finally
        Colorize(vFill, vSavedFillColor);
        Colorize(vStroke, vSavedStrokeColor);
      end;
    end;
  end;
end;

procedure TPainter.DrawPie(const AStyle: TDrawStyle; const AFillName, AStrokeName: string; const ARect: TRectF;
  const AStartAngle, AEndAngle: Single; const AColor: Cardinal = 0);
var
  vStroke: TStylePen;
  vFill: TStyleBrush;
  vSavedFillColor: Cardinal;
  vSavedStrokeColor: Cardinal;
  vEndAngle: Single;
begin
  if Assigned(FContext) and Assigned(AStyle) and not ARect.IsEmpty and not SameValue(AStartAngle, AEndAngle) then
  begin
    vFill := AStyle.Brush[AFillName];
    vStroke := AStyle.Pen[AStrokeName];
    if Assigned(vStroke) and (vStroke.Width < 0.3) then
      vStroke := nil;

    if not Assigned(vFill) and not Assigned(vStroke) then
      Exit;

    vEndAngle := IfThen(AStartAngle > AEndAngle, AEndAngle + 360, AEndAngle);

    if AColor = 0 then
      DoDrawPie(vFill, vStroke, ARect, AStartAngle, vEndAngle)
    else begin
      vSavedFillColor := Colorize(vFill, AColor);
      vSavedStrokeColor := Colorize(vStroke, AColor);
      try
        DoDrawPie(vFill, vStroke, ARect, AStartAngle, vEndAngle);
      finally
        Colorize(vFill, vSavedFillColor);
        Colorize(vStroke, vSavedStrokeColor);
      end;
    end;
  end;
end;

procedure TPainter.DrawPoint(const AStyle: TDrawStyle; const AFillName: string; const APoint: TPointF;
  const ASize: Single = 6; const AColor: Cardinal = 0);
begin
  if Assigned(FContext) and Assigned(AStyle) and (ASize > 0.3) then
  begin
    if ASize < 1.8 then
      DrawRect(AStyle, AFillName, '', RectF(APoint.X, APoint.Y, APoint.X + ASize, APoint.Y + ASize), False, AColor)
    else
      DrawCircle(AStyle, AFillName, '', APoint, ASize / 2, AColor);
  end;
end;

procedure TPainter.DrawPolyline(const AStyle: TDrawStyle; const AStrokeName: string; const APoints: array of TPointF;
  const AColor: Cardinal = 0);
begin
  DrawPolyline(AStyle, AStrokeName, @APoints, Length(APoints), AColor);
end;

procedure TPainter.DrawPolyline(const AStyle: TDrawStyle; const AStrokeName: string; const APoints: PPointF;
  const ACount: Integer; const AColor: Cardinal = 0);
var
  vStroke: TStylePen;
  vSavedStrokeColor: Cardinal;
begin
  if Assigned(FContext) and Assigned(AStyle) and (ACount > 1) then
  begin
    vStroke := AStyle.Pen[AStrokeName];
    if Assigned(vStroke) and (vStroke.Width >= 0.3) then
    begin
      if AColor = 0 then
        DoDrawPolyline(vStroke, APoints, ACount)
      else begin
        vSavedStrokeColor := Colorize(vStroke, AColor);
        try
          DoDrawPolyline(vStroke, APoints, ACount);
        finally
          Colorize(vStroke, vSavedStrokeColor);
        end;
      end;
    end;
  end;
end;

procedure TPainter.DrawRect(const AStyle: TDrawStyle; const AFillName, AStrokeName: string;
  const ARect: TRectF; const ASnapToPixels: Boolean = True; const AColor: Cardinal = 0);
var
  vStroke: TStylePen;
  vFill: TStyleBrush;
  vSavedFillColor: Cardinal;
  vSavedStrokeColor: Cardinal;
  vRect: TRectF;
begin
  if Assigned(FContext) and Assigned(AStyle) and not ARect.IsEmpty then
  begin
    vFill := AStyle.Brush[AFillName];
    vStroke := AStyle.Pen[AStrokeName];
    if Assigned(vStroke) and (vStroke.Width < 0.3) then
      vStroke := nil;

    if not Assigned(vFill) and not Assigned(vStroke) then
      Exit;

    if ASnapToPixels then
      vRect := ARect.Round
    else
      vRect := ARect;

    if AColor = 0 then
      DoDrawRect(vFill, vStroke, vRect)
    else begin
      vSavedFillColor := Colorize(vFill, AColor);
      vSavedStrokeColor := Colorize(vStroke, AColor);
      try
        DoDrawRect(vFill, vStroke, vRect);
      finally
        Colorize(vFill, vSavedFillColor);
        Colorize(vStroke, vSavedStrokeColor);
      end;
    end;
  end;
end;

procedure TPainter.DrawRegion(const AStyle: TDrawStyle; const AFillName, AStrokeName: string;
  const APoints: PPointF; const ACount: Integer; const AColor: Cardinal = 0);
var
  vStroke: TStylePen;
  vFill: TStyleBrush;
  vSavedFillColor: Cardinal;
  vSavedStrokeColor: Cardinal;
begin
  if Assigned(FContext) and Assigned(AStyle) and Assigned(APoints) and (ACount > 2) then
  begin
    vFill := AStyle.Brush[AFillName];
    vStroke := AStyle.Pen[AStrokeName];
    if Assigned(vStroke) and (vStroke.Width < 0.3) then
      vStroke := nil;

    if not Assigned(vFill) and not Assigned(vStroke) then
      Exit;

    if AColor = 0 then
      DoDrawRegion(vFill, vStroke, APoints, ACount)
    else begin
      vSavedFillColor := Colorize(vFill, AColor);
      vSavedStrokeColor := Colorize(vStroke, AColor);
      try
        DoDrawRegion(vFill, vStroke, APoints, ACount);
      finally
        Colorize(vFill, vSavedFillColor);
        Colorize(vStroke, vSavedStrokeColor);
      end;
    end;
  end;
end;

procedure TPainter.DrawText(const AStyle: TDrawStyle; const AFontName: string; const AText: string;
  const ARect: TRectF; const AOptions: Cardinal = 0; const AAngle: Single = 0; const AColor: Cardinal = 0);
var
  vFont: TStyleFont;
  vSavedFontColor: Cardinal;
begin
  if Assigned(FContext) and Assigned(AStyle) and (Trim(AText) <> '') and not ARect.IsEmpty then
  begin
    vFont := AStyle.Font[AFontName];
    if Assigned(vFont) and (vFont.Size >= 3) then
    begin
      if AColor = 0 then
        DoDrawText(vFont, AText, ARect, AOptions, AAngle)
      else begin
        vSavedFontColor := Colorize(vFont, AColor);
        try
          DoDrawText(vFont, AText, ARect, AOptions, AAngle);
        finally
          Colorize(vFont, vSavedFontColor);
        end;
      end;
    end;
  end;
end;

procedure TPainter.EndPaint;
begin
end;

function TPainter.GetStyle(const AName: string): TDrawStyle;
begin
  Result := FDrawStyles.StyleByName(AName);
end;

procedure TPainter.InvertRect(const ARect: TRectF);
begin
  if Assigned(FContext) and not ARect.IsEmpty then
    DoInvertRect(ARect);
end;

function TPainter.SetContext(const AContext: TDrawContext): TDrawContext;
begin
  Result := FContext;
  FContext := AContext;
end;

function TPainter.StyleExists(const AName: string): Boolean;
begin
  Result := FDrawStyles.StyleExists(AName);
end;

function TPainter.TextExtents(const AStyle: TDrawStyle; const AFontName: string; const AText: string): TSizeF;
var
  vFont: TStyleFont;
begin
  Result.cx := 0;
  Result.cy := 0;

  if Assigned(FContext) and Assigned(AStyle) and (Trim(AText) <> '') then
  begin
    vFont := AStyle.Font[AFontName];
    if Assigned(vFont) then
      Result := GetTextExtents(AStyle.Font[AFontName], AText);
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

{ TStyleObject }

constructor TStyleObject.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FObject := nil;
end;

destructor TStyleObject.Destroy;
begin
  FreeAndNil(FObject);
  inherited Destroy;
end;

procedure TStyleObject.SetObject(const Value: TObject);
begin
  if FObject = Value then
    Exit;

  if Assigned(FObject) then
    FObject.Free;
  FObject := Value;
end;

{ TColoredStyleObject }

constructor TColoredStyleObject.Create(const AName: string; const AColor: Cardinal);
begin
  inherited Create(AName);
  FColor := AColor;
end;

{ TDrawContext }

constructor TDrawContext.Create(const AWidth, AHeight: Single);
begin
  inherited Create;
  FClientRect := RectF(0, 0, AWidth, AHeight);
end;

procedure TDrawContext.DoSetSize(const AWidth, AHeight: Single);
begin
end;

function TDrawContext.GetHeight: Integer;
begin
  Result := Round(FClientRect.Height);
end;

function TDrawContext.GetWidth: Integer;
begin
  Result := Round(FClientRect.Width);
end;

procedure TDrawContext.SaveToFile(const AFileName: string);
begin
  Assert(False, 'Not implemented yet');
end;

procedure TDrawContext.SetSize(const AWidth, AHeight: Single);
begin
  if not SameValue(AWidth, FClientRect.Width) or not SameValue(AHeight, FClientRect.Height) then
  begin
    DoSetSize(AWidth, AHeight);
    FClientRect := RectF(0, 0, AWidth, AHeight);
  end;
end;

end.
