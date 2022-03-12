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

unit uDirect2DPainter;

interface

uses
  Windows, Graphics, Types, Classes, Direct2D, D2D1, uDrawStyles, WinCodec, uD2DExtra, uScene, uWinScene, uConsts;

type
  TD2DPen = class
  private
    FStyle: ID2D1StrokeStyle;
    FBrush: ID2D1Brush;
  public
    constructor Create(const AStyle: ID2D1StrokeStyle; const ABrush: ID2D1Brush);
    destructor Destroy; override;

    property Style: ID2D1StrokeStyle read FStyle;
    property Brush: ID2D1Brush read FBrush;
  end;

  TD2DBrush = class
  private
    FBrush: ID2D1Brush;
  public
    constructor Create(const ABrush: ID2D1Brush);
    destructor Destroy; override;

    property Brush: ID2D1Brush read FBrush;
  end;

  TD2DFont = class
  private
    FTextFormat: IDWriteTextFormat;
    FBrush: ID2D1Brush;
  public
    constructor Create(const ATextFormat: IDWriteTextFormat; const ABrush: ID2D1Brush);
    destructor Destroy; override;

    property Brush: ID2D1Brush read FBrush;
    property TextFormat: IDWriteTextFormat read FTextFormat;
  end;

  TD2DPath = class
  private
    FGeometry: ID2D1Geometry;
  public
    constructor Create(const AGeometry: ID2D1Geometry);
    destructor Destroy; override;

    property Geometry: ID2D1Geometry read FGeometry;
  end;

  TD2DImage = class
  private
    FBitmap: ID2D1Bitmap;
  public
    constructor Create(const ABitmap: ID2D1Bitmap);
    destructor Destroy; override;

    property Bitmap: ID2D1Bitmap read FBitmap;
  end;

  TD2DNinePatchImage = class(TNinePatchImage)
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject; override;
  end;

  TD2DDrawContext = class(TDrawContext)
  private
    FPainter: TPainter;
    FCanvas: TCanvas;
    FHandle: THandle;
    FRenderTarget: ID2D1RenderTarget;
    FBitmapTarget: ID2D1Bitmap1;
    procedure CreateBitmapTarget(const AWidth, AHeight: Single);
  protected
    procedure DoSetSize(const AWidth, AHeight: Single); override;
  public
    procedure SaveToFile(const AFileName: string); override;
  public
    constructor Create(const APainter: TPainter; const AContainer: TDrawContainer; const AWidth, AHeight: Single);
    destructor Destroy; override;

    property RenderTarget: ID2D1RenderTarget read FRenderTarget;
    property BitmapTarget: ID2D1Bitmap1 read FBitmapTarget;
  end;

  TDirect2DPainter = class(TPainter)
  private
    FD2DFactory: ID2D1Factory;
    FImageFactory: IWICImagingFactory;
    function MakeD2DColorF(const AColor: Cardinal): TD2D1ColorF;
    function MakeD2DRectF(const ARect: TRectF; const ANeedShrink: Boolean = False): TD2D1RectF;
    function MakeD2DPointF(const APoint: TPointF): TD2D1Point2F; overload;
    function MakeD2DPointF(const X, Y: Single): TD2D1Point2F; overload;
    procedure CheckGradients(const AFill: TStyleBrush; const ARect: TRectF);
    procedure DrawGeometry(const AFill: TStyleBrush; const AStroke: TStylePen; const AGeometry: ID2D1Geometry); overload;
    procedure DrawGeometry(const AStroke: TStylePen; const AGeometry: ID2D1Geometry); overload;
    function PointsToArc(const ARect: TRectF; const AStartAngle, AEndAngle: Single; out ACenter, AStart, AEnd: TD2D1Point2F): TD2D1ArcSegment;
    function CutImage(const ASource: ID2D1Bitmap; const ALeft, ATop, AWidth, AHeight: Integer): ID2D1Bitmap;
    function CreateBitmap(const AWidth, AHeight: Integer): ID2D1Bitmap;
    function ThisRenderTarget: ID2D1RenderTarget;
  protected
    procedure DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF); override;
    procedure DoDrawPie(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF;
      const AStartAngle, AEndAngle: Single); override;
    procedure DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF); override;
    procedure DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer); override;
    procedure DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject); override;
    procedure DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer); override;
    procedure DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer); override;
    procedure DoDrawRect(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF); override;
    procedure DoDrawText(const AFont: TStyleFont; const AText: string; const ARect: TRectF;
      const AOptions: Cardinal; const AAngle: Single); override;
    function GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF; override;
    procedure DoInvertRect(const ARect: TRectF); override;
    procedure DoClipRect(const ARect: TRectF); override;
    procedure DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single); override;
    procedure DoDrawContext(const AContext: TDrawContext); override;

    procedure DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal); override;
    procedure DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal); override;
    procedure DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal); override;
  public
    constructor Create(const AContainer: TObject); override;
    destructor Destroy; override;

    procedure CreateBrush(const AFill: TStyleBrush); override;
    procedure CreatePen(const AStroke: TStylePen); override;
    procedure CreateFont(const AFont: TStyleFont); override;
    procedure CreateImage(const AImage: TStyleImage); override;
    function CreateDrawContext(const AWidth, AHeight: Single): TDrawContext; override;

    procedure BeginPaint; override;
    procedure EndPaint; override;
  end;

  TDirect2DScene = class(TWinScene)
  private
    FStaticContext: TD2DDrawContext;
  protected
    procedure DoDestroyScene; override;
    procedure DoRender(const ANeedFullRepaint: Boolean); override;
    procedure UpdateContexts(const AWidth, AHeight: Single); override;
    function CreatePainter(const AContainer: TObject): TPainter; override;
  end;

implementation

uses
  StrUtils, SysUtils, IOUtils, Math, ComObj, ActiveX, uModule, DxgiFormat;

function MakeD2DPointU(const x, y: Cardinal): TD2D1Point2U;
begin
  Result.x := x;
  Result.y := y;
end;

function MakeD2DRectU(const ALeft, ATop, AWidth, AHeight: Integer): TD2D1RectU;
begin
  Result.left := ALeft;
  Result.top := ATop;
  Result.right := ALeft + AWidth;
  Result.bottom := ATop + AHeight;
end;

{ TDirect2DPainter }

procedure TDirect2DPainter.BeginPaint;
begin
  ThisRenderTarget.BeginDraw;
end;

procedure TDirect2DPainter.CheckGradients(const AFill: TStyleBrush; const ARect: TRectF);
var
  vBrush: ID2D1LinearGradientBrush;
begin
  if AFill.GradientKind = gkNone then
    Exit;

  vBrush := ID2D1LinearGradientBrush(TD2DBrush(AFill.NativeObject).Brush);
  if (AFill.GradientKind = gkHorizontal) and (ARect.Width <> AFill.Rect.Width) then
  begin
    AFill.Rect := ARect;
    vBrush.SetEndPoint(D2D1PointF(ARect.Width, 0));
  end
  else if (AFill.GradientKind = gkVertical) and (ARect.Height <> AFill.Rect.Height)
  then begin
    AFill.Rect := ARect;
    vBrush.SetEndPoint(D2D1PointF(0, ARect.Height));
  end;
end;

constructor TDirect2DPainter.Create(const AContainer: TObject);
var
  vContainer: TDrawContainer absolute AContainer;
begin
  if (Win32MajorVersion < 6) or (Win32Platform <> VER_PLATFORM_WIN32_NT) then
    raise Exception.Create('Your Windows version do not support Direct2D');

  inherited Create(AContainer);

  FD2DFactory := D2DFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, nil);
  CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER, IID_IWICImagingFactory, FImageFactory);

  FContext := TD2DDrawContext.Create(Self, vContainer, vContainer.Width, vContainer.Height);
end;

function TDirect2DPainter.CreateBitmap(const AWidth, AHeight: Integer): ID2D1Bitmap;
var
  vProperties: TD2D1BitmapProperties;
begin
  vProperties := D2D1BitmapProperties(D2D1PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED), 96, 96);
  ThisRenderTarget.CreateBitmap(D2D1SizeU(AWidth, AHeight), nil, 0, vProperties, Result);
end;

procedure TDirect2DPainter.CreateBrush(const AFill: TStyleBrush);
var
  vGradientBrush: ID2D1LinearGradientBrush;
  vSolidBrush: ID2D1SolidColorBrush;
  vGradientProperties: TD2D1LinearGradientBrushProperties;
  vGradientStops: ID2D1GradientStopCollection;
  //vHatchStyle: THatchStyle;
  vGradientPositions: array[0..1] of TD2D1GradientStop;
begin
  if AFill.Style = bsSolid then
  begin
    if AFill.GradientKind <> gkNone then
    begin
      vGradientPositions[0] := D2D1GradientStop(0.0, MakeD2DColorF(AFill.Color));
      vGradientPositions[1] := D2D1GradientStop(1.0, MakeD2DColorF(AFill.BackColor));
      ThisRenderTarget.CreateGradientStopCollection(@vGradientPositions, 2, D2D1_GAMMA_2_2, D2D1_EXTEND_MODE_CLAMP, vGradientStops);
      ThisRenderTarget.CreateLinearGradientBrush(vGradientProperties, nil, vGradientStops, vGradientBrush);
      vGradientBrush.SetStartPoint(D2D1PointF(0, 0));
      AFill.NativeObject := TD2DBrush.Create(vGradientBrush);
    end
    else begin
      ThisRenderTarget.CreateSolidColorBrush(MakeD2DColorF(AFill.Color), nil, vSolidBrush);
      AFill.NativeObject := TD2DBrush.Create(vSolidBrush);
    end;
  end
  else begin
    ThisRenderTarget.CreateSolidColorBrush(MakeD2DColorF(AFill.Color), nil, vSolidBrush);
    AFill.NativeObject := TD2DBrush.Create(vSolidBrush);
    {case AFill.Style of
      bsHorizontal: vHatchStyle := THatchStyle.HatchStyleHorizontal;
      bsVertical: vHatchStyle := THatchStyle.HatchStyleVertical;
      bsFDiagonal: vHatchStyle := THatchStyle.HatchStyleForwardDiagonal;
      bsBDiagonal: vHatchStyle := THatchStyle.HatchStyleBackwardDiagonal;
      bsCross: vHatchStyle := THatchStyle.HatchStyleCross;
      bsDiagCross: vHatchStyle := THatchStyle.HatchStyleDiagonalCross;
    else
      vHatchStyle := THatchStyle.HatchStyleDiagonalCross
    end;
    vBrush := TGPHatchBrush.Create(vHatchStyle, AFill.Color, AFill.BackColor); }
  end;
end;

function TDirect2DPainter.CreateDrawContext(const AWidth, AHeight: Single): TDrawContext;
begin
  Result := TD2DDrawContext.Create(Self, nil, AWidth, AHeight);
end;

procedure TDirect2DPainter.CreateFont(const AFont: TStyleFont);
var
  vWeight: TDWriteFontWeight;
  vStyle: TDWriteFontStyle;
  vTextFormat: IDWriteTextFormat;
  vBrush: ID2D1SolidColorBrush;
  vDpiX, vDpiY: Single;
begin
  if (FontStyleItalic and AFont.Style) > 0 then
    vStyle := DWRITE_FONT_STYLE_ITALIC
  else
    vStyle := DWRITE_FONT_STYLE_NORMAL;
  if (FontStyleBold and AFont.Style) > 0 then
    vWeight := DWRITE_FONT_WEIGHT_BOLD
  else
    vWeight := DWRITE_FONT_WEIGHT_NORMAL;

  ThisRenderTarget.GetDpi(vDpiX, vDpiY);

  DWriteFactory.CreateTextFormat(PWideChar(AFont.Family), nil, vWeight, vStyle,
    DWRITE_FONT_STRETCH_NORMAL, MulDiv(Round(AFont.Size), Trunc(vDpiX), 72), 'en-us', vTextFormat);

  ThisRenderTarget.CreateSolidColorBrush(MakeD2DColorF(AFont.Color), nil, vBrush);

  AFont.NativeObject := TD2DFont.Create(vTextFormat, vBrush);
end;

procedure TDirect2DPainter.CreateImage(const AImage: TStyleImage);
var
  vBitmap: ID2D1Bitmap;
  vImage: TD2DImage;

  function LoadBitmapFromFile(const AFileName: string): ID2D1Bitmap;
  var
    vDecoder: IWICBitmapDecoder;
    vFrameDecode: IWICBitmapFrameDecode;
    vConverter: IWICFormatConverter;
    vStream: IWICStream;
  begin
    //vResult := FImageFactory.CreateDecoderFromFilename(PChar(AImage.FileName), nil,
    //  GENERIC_READ, WICDecodeMetadataCacheOnLoad, vDecoder);

    OleCheck(FImageFactory.CreateDecoder(GUID_ContainerFormatPng, TGUID.Empty, vDecoder));
    OleCheck(FImageFactory.CreateStream(vStream));
    OleCheck(vStream.InitializeFromFilename(PChar(AFileName), GENERIC_READ));

    OleCheck(vDecoder.Initialize(vStream, WICDecodeMetadataCacheOnLoad));

    OleCheck(vDecoder.GetFrame(0, vFrameDecode));

    OleCheck(FImageFactory.CreateFormatConverter(vConverter));

    OleCheck(vConverter.Initialize(vFrameDecode, GUID_WICPixelFormat32bppPBGRA,
        WICBitmapDitherTypeNone, nil, 0, WICBitmapPaletteTypeMedianCut));

    OleCheck(ThisRenderTarget.CreateBitmapFromWicBitmap(vConverter, nil, Result));
  end;

begin
  if not TFile.Exists(AImage.FileName) then
    Exit;

  vBitmap := LoadBitmapFromFile(AImage.FileName);
  vImage := TD2DImage.Create(vBitmap);

  if AImage.IsNinePatch then
    AImage.NativeObject := TD2DNinePatchImage.Create(Self, vImage, AImage.CutRect.Left, AImage.CutRect.Right,
      AImage.CutRect.Top, AImage.CutRect.Bottom)
  else
    AImage.NativeObject := vImage;
end;

procedure TDirect2DPainter.CreatePen(const AStroke: TStylePen);
var
  vStrokeProperties: TD2D1StrokeStyleProperties;
  vStyle: ID2D1StrokeStyle;
  vStrokeBrush: ID2D1SolidColorBrush;
const
  cDashStyles: array[psDash..psDashDotDot] of TD2D1DashStyle = (D2D1_DASH_STYLE_DASH,
    D2D1_DASH_STYLE_DOT, D2D1_DASH_STYLE_DASH_DOT, D2D1_DASH_STYLE_DASH_DOT_DOT);
begin
  if AStroke.Style in [psDash..psDashDotDot] then
  begin
    vStrokeProperties := D2D1StrokeStyleProperties(D2D1_CAP_STYLE_FLAT, D2D1_CAP_STYLE_FLAT, D2D1_CAP_STYLE_ROUND,
      D2D1_LINE_JOIN_ROUND, 10, cDashStyles[AStroke.Style], 0);
    FD2DFactory.CreateStrokeStyle(vStrokeProperties, nil, 0, vStyle);
  end
  else
    vStyle := nil;

  ThisRenderTarget.CreateSolidColorBrush(MakeD2DColorF(AStroke.Color), nil, vStrokeBrush);
  AStroke.NativeObject := TD2DPen.Create(vStyle, vStrokeBrush);
end;

function TDirect2DPainter.CutImage(const ASource: ID2D1Bitmap; const ALeft, ATop, AWidth, AHeight: Integer): ID2D1Bitmap;
var
  vBitmap: ID2D1Bitmap;
  vDestPoint: TD2D1Point2U;
  vSrcRect: TD2D1RectU;
begin
  vBitmap := CreateBitmap(AWidth, AHeight);
  vDestPoint := MakeD2DPointU(0, 0);
  vSrcRect := MakeD2DRectU(ALeft, ATop, AWidth, AHeight);
  vBitmap.CopyFromBitmap(vDestPoint, ASource, vSrcRect);
  Result := vBitmap;
end;

destructor TDirect2DPainter.Destroy;
begin
  FD2DFactory := nil;
  inherited Destroy;
end;

procedure TDirect2DPainter.DoClipRect(const ARect: TRectF);
begin
  // TODO
end;

procedure TDirect2DPainter.DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal);
begin
  ID2D1SolidColorBrush(TD2DBrush(AFill.NativeObject).Brush).SetColor(MakeD2DColorF(AColor));
end;

procedure TDirect2DPainter.DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal);
begin
  ID2D1SolidColorBrush(TD2DFont(AFont.NativeObject).Brush).SetColor(MakeD2DColorF(AColor));
end;

procedure TDirect2DPainter.DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal);
begin
  ID2D1SolidColorBrush(TD2DPen(AStroke.NativeObject).Brush).SetColor(MakeD2DColorF(AColor));
end;

procedure TDirect2DPainter.DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vPoints: PPointFArray;
  vGeometry: ID2D1PathGeometry;
  vSink: ID2D1GeometrySink;
  i: Integer;
begin
  vPoints := PPointFArray(APoints);

  FD2DFactory.CreatePathGeometry(vGeometry);
  vGeometry.Open(vSink);
  try
    vSink.BeginFigure(MakeD2DPointF(vPoints[0]), D2D1_FIGURE_BEGIN_FILLED);
    try
      i := 1;
      while i < ACount do
      begin
        vSink.AddBezier(D2D1BezierSegment(
          MakeD2DPointF(vPoints[i]), MakeD2DPointF(vPoints[i + 1]), MakeD2DPointF(vPoints[i + 2])));
        Inc(i,3);
      end;
    finally
      vSink.EndFigure(D2D1_FIGURE_END_OPEN);
    end;
  finally
    vSink.Close;
  end;

  DrawGeometry(AStroke, vGeometry);
end;

procedure TDirect2DPainter.DoDrawContext(const AContext: TDrawContext);
begin
  //BitBlt(TCanvas(Canvas).Handle, 0, 0, AContext.Width, AContext.Height,
  //  TCanvas(AContext.Canvas).Handle, 0, 0, SRCCOPY)
end;

procedure TDirect2DPainter.DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
var
  vRect: TD2DRectF;
  vEllipse: TD2D1Ellipse;
begin
  vRect := MakeD2DRectF(ARect);
  vEllipse := D2D1Ellipse(D2D1PointF(ARect.CenterPoint.X, ARect.CenterPoint.Y), ARect.Width / 2, ARect.Height / 2);

  if Assigned(AFill) then
  begin
    CheckGradients(AFill, ARect);
    ThisRenderTarget.FillEllipse(vEllipse, TD2DBrush(AFill.NativeObject).Brush);
  end;

  if Assigned(AStroke) then
    ThisRenderTarget.DrawEllipse(vEllipse, ID2D1Brush(TD2DPen(AStroke.NativeObject).Brush),
      AStroke.Width, TD2DPen(AStroke.NativeObject).Style);
end;

procedure TDirect2DPainter.DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
var
  vRect: TD2DRectF;
begin
  vRect := MakeD2DRectF(ARect);
  ThisRenderTarget.DrawBitmap(TD2DImage(AImage).Bitmap, @vRect, AOpacity);
end;

procedure TDirect2DPainter.DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF);
begin
  ThisRenderTarget.DrawLine(MakeD2DPointF(APoint1.X, APoint1.Y), MakeD2DPointF(APoint2.X, APoint2.Y),
    TD2DPen(AStroke.NativeObject).Brush, AStroke.Width, TD2DPen(AStroke.NativeObject).Style);
end;

procedure TDirect2DPainter.DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject);
begin
  DrawGeometry(AFill, AStroke, TD2DPath(APath).Geometry);
end;

function TDirect2DPainter.PointsToArc(const ARect: TRectF; const AStartAngle, AEndAngle: Single;
  out ACenter, AStart, AEnd: TD2D1Point2F): TD2D1ArcSegment;
var
  vSize: TD2D1SizeF;
  vAngleDiff: Single;

  function CalcEllipsePointCoord(const AAngle: Single): TD2D1Point2F;
  var
    vSin, vCos: Extended;
    t: Extended;   // параметр для уравнения эллипса
  begin
    SinCos(DegToRad(AAngle), vSin, vCos);
    t := ArcTan2(vSize.Width * vSin, vSize.Height * vCos);
    // считаем результат по параметрическому уравнению
    SinCos(t, vSin, vCos);
    Result := D2D1PointF(ACenter.x + vSize.Width * vCos, ACenter.Y + vSize.Height * vSin);
  end;

begin
  ACenter := D2D1PointF(ARect.CenterPoint.X, ARect.CenterPoint.Y);
  vSize := D2D1SizeF(ARect.Width / 2, ARect.Height / 2);

  Result.rotationAngle := 0;
  vAngleDiff := AEndAngle - AStartAngle;
  if vAngleDiff < 0 then
    vAngleDiff := vAngleDiff + 360;

  if vAngleDiff > 180 then
    Result.arcSize := D2D1_ARC_SIZE_LARGE
  else
    Result.arcSize := D2D1_ARC_SIZE_SMALL;
  Result.sweepDirection := D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE;
  Result.size := vSize;

  if AEndAngle > AStartAngle then
  begin
    AEnd := CalcEllipsePointCoord(AStartAngle);
    AStart := CalcEllipsePointCoord(AEndAngle);
  end
  else begin
    AStart := CalcEllipsePointCoord(AStartAngle);
    AEnd := CalcEllipsePointCoord(AEndAngle);
  end;

  Result.point := AEnd;
end;

procedure TDirect2DPainter.DoDrawPie(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF;
  const AStartAngle, AEndAngle: Single);
var
  vRect: TD2DRectF;
  vGeometry: ID2D1PathGeometry;
  vSink: ID2D1GeometrySink;
  vArcSegment: TD2D1ArcSegment;
  vStart, vEnd, vCenter: TD2D1Point2F;
begin
  vRect := MakeD2DRectF(ARect);
  vArcSegment := PointsToArc(ARect, AStartAngle, AEndAngle, vCenter, vStart, vEnd);

  FD2DFactory.CreatePathGeometry(vGeometry);
  vGeometry.Open(vSink);
  try
    vSink.BeginFigure(vCenter, D2D1_FIGURE_BEGIN_FILLED);
    try
      vSink.AddLine(vStart);
      vSink.AddArc(vArcSegment);
    finally
      vSink.EndFigure(D2D1_FIGURE_END_CLOSED);
    end;
  finally
    vSink.Close;
  end;

  DrawGeometry(AFill, AStroke, vGeometry);
end;

procedure TDirect2DPainter.DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vPoints: PPointFArray;
  vGeometry: ID2D1PathGeometry;
  vSink: ID2D1GeometrySink;
  i: Integer;
begin
  vPoints := PPointFArray(APoints);

  FD2DFactory.CreatePathGeometry(vGeometry);
  vGeometry.Open(vSink);
  try
    vSink.BeginFigure(MakeD2DPointF(vPoints[0]), D2D1_FIGURE_BEGIN_HOLLOW);
    try
      for i := 1 to ACount - 1 do
        vSink.AddLine(MakeD2DPointF(vPoints[i]));
    finally
      vSink.EndFigure(D2D1_FIGURE_END_OPEN);
    end;
  finally
    vSink.Close;
  end;

  DrawGeometry(AStroke, vGeometry);
end;

procedure TDirect2DPainter.DoDrawRect(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
var
  vRect: TD2DRectF;
begin
  vRect := MakeD2DRectF(ARect, True);

  if Assigned(AFill) then
  begin
    CheckGradients(AFill, ARect);
    ThisRenderTarget.FillRectangle(vRect, TD2DBrush(AFill.NativeObject).Brush);
  end;

  if Assigned(AStroke) then
    ThisRenderTarget.DrawRectangle(vRect, ID2D1Brush(TD2DPen(AStroke.NativeObject).Brush),
      AStroke.Width, TD2DPen(AStroke.NativeObject).Style);
end;

procedure TDirect2DPainter.DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen; const APoints: PPointF;
  const ACount: Integer);
var
  vPoints: PPointFArray;
  vGeometry: ID2D1PathGeometry;
  vSink: ID2D1GeometrySink;
  i: Integer;
begin
  vPoints := PPointFArray(APoints);

  FD2DFactory.CreatePathGeometry(vGeometry);
  vGeometry.Open(vSink);
  try
    vSink.BeginFigure(MakeD2DPointF(vPoints[0]), D2D1_FIGURE_BEGIN_HOLLOW);
    try
      for i := 1 to ACount - 1 do
        vSink.AddLine(MakeD2DPointF(vPoints[i]));
    finally
      vSink.EndFigure(D2D1_FIGURE_END_OPEN);
    end;
  finally
    vSink.Close;
  end;

  DrawGeometry(AStroke, vGeometry);
end;

procedure TDirect2DPainter.DoDrawText(const AFont: TStyleFont; const AText: string;
  const ARect: TRectF; const AOptions: Cardinal; const AAngle: Single);
var
  vTextRange: TDwriteTextRange;
  vTextLayout: IDWriteTextLayout;
  vTextMetrics: TDWriteTextMetrics;
  vStartPoint: TD2D1Point2F;
  vMatrix: TD2DMatrix3X2F;
  vCalcRect: TRectF;
  vCenterPoint: TPointF;
begin
  OleCheck(DWriteFactory.CreateTextLayout(PWideChar(AText), Length(AText),
    TD2DFont(AFont.NativeObject).TextFormat, 0, 0, vTextLayout));
  vTextRange.startPosition := 0;
  vTextRange.length := Length(AText);
  if (FontStyleUnderline and AFont.Style) > 0 then
    vTextLayout.SetUnderline(True, vTextRange);
  if (FontStyleStrikeout and AFont.Style) > 0 then
    vTextLayout.SetStrikethrough(True, vTextRange);

  vTextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP);

  OleCheck(vTextLayout.GetMetrics(vTextMetrics));

  if IsZero(AFont.Angle) then
  begin
    vStartPoint := D2D1PointF(ARect.Left - 0.5, ARect.Top - 0.5);
    if (DT_CENTER and AOptions) > 0 then
      vStartPoint.x := vStartPoint.x + (ARect.Width - vTextMetrics.width) / 2;
    if (DT_VCENTER and AOptions) > 0 then
      vStartPoint.y := vStartPoint.y + (ARect.Height - vTextMetrics.height) / 2;
    if (DT_RIGHT and AOptions) > 0 then
      vStartPoint.x := ARect.Right - vTextMetrics.width - 0.5;
    if (DT_BOTTOM and AOptions) > 0 then
      vStartPoint.y := ARect.Bottom - vTextMetrics.height - 0.5;

    ThisRenderTarget.DrawTextLayout(vStartPoint, vTextLayout,
      TD2DFont(AFont.NativeObject).Brush, D2D1_DRAW_TEXT_OPTIONS_NONE);
  end
  else begin
    vCenterPoint := ARect.CenterPoint;
    vCalcRect := RectF(vCenterPoint.X - ARect.Height / 2, vCenterPoint.Y - ARect.Width / 2,
      vCenterPoint.X + ARect.Height / 2, vCenterPoint.Y + ARect.Width / 2);
    vStartPoint := D2D1PointF(vCalcRect.Left - 0.5, vCalcRect.Top - 0.5);
    if (DT_CENTER and AOptions) > 0 then
      vStartPoint.x := vStartPoint.x + (vCalcRect.Width - vTextMetrics.width) / 2;
    if (DT_VCENTER and AOptions) > 0 then
      vStartPoint.y := vStartPoint.y + (vCalcRect.Height - vTextMetrics.height) / 2;
    if (DT_RIGHT and AOptions) > 0 then
      vStartPoint.x := vCalcRect.Right - vTextMetrics.width - 0.5;
    if (DT_BOTTOM and AOptions) > 0 then
      vStartPoint.y := vCalcRect.Bottom - vTextMetrics.height - 0.5;

    vMatrix := TD2DMatrix3X2F.Rotation(AFont.Angle, vCenterPoint.X, vCenterPoint.Y);
    ThisRenderTarget.SetTransform(vMatrix);
    try
      ThisRenderTarget.DrawTextLayout(vStartPoint, vTextLayout,
        TD2DFont(AFont.NativeObject).Brush, D2D1_DRAW_TEXT_OPTIONS_NONE);
    finally
      vMatrix := TD2DMatrix3X2F.Rotation(0, vCenterPoint.X, vCenterPoint.Y);
      ThisRenderTarget.SetTransform(vMatrix);
    end;
  end;
end;

procedure TDirect2DPainter.DoInvertRect(const ARect: TRectF);
var
  vDeviceContext: ID2D1DeviceContext;
  vMatrix: TD2D_MATRIX_5X4_F;
  vEffect: ID2D1Effect;
  vImage: ID2D1Image;
  vBitmap: ID2D1Bitmap;
  vDestPoint: TD2D1Point2U;
  vSrcRect: TD2D1RectU;
  vImageRect: TD2D1RectF;
  vTargetOffset: TD2D1Point2F;
begin
  vDeviceContext := ThisRenderTarget as ID2D1DeviceContext;
  if not Assigned(vDeviceContext) then
    Exit;

  vBitmap := CreateBitmap(Round(ARect.Width), Round(ARect.Height));

  vDestPoint := MakeD2DPointU(0, 0);
  vSrcRect := MakeD2DRectU(Round(ARect.Left), Round(ARect.Top), Round(ARect.Width), Round(ARect.Height));
  if not Succeeded(vBitmap.CopyFromRenderTarget(vDestPoint, ThisRenderTarget, vSrcRect)) then
    Exit;

  vImage := vBitmap as ID2D1Image;

  if Succeeded(vDeviceContext.CreateEffect(CLSID_D2D1Invert, vEffect)) then
    vEffect.SetInput(0, vImage)
  else if Succeeded(vDeviceContext.CreateEffect(CLSID_D2D1ColorMatrix, vEffect)) then
  begin
    vMatrix.Init(-1, 0, 0, 0,   0,-1, 0, 0,   0, 0,-1, 0,   0, 0, 0, 1,  1, 1, 1, 0);
    vEffect.SetInput(0, vImage);

    vEffect.SetValue(Cardinal(D2D1_COLORMATRIX_PROP_COLOR_MATRIX), D2D1_PROPERTY_TYPE_MATRIX_5X4, @vMatrix, SizeOf(vMatrix));
  end;

  vTargetOffset := D2D1PointF(Round(ARect.Left), Round(ARect.Top));
  vImageRect := D2D1RectF(0, 0, Round(ARect.Width), Round(ARect.Height));
  vDeviceContext.DrawImage(vEffect as ID2D1Image, @vTargetOffset, @vImageRect);
end;

procedure TDirect2DPainter.DrawGeometry(const AStroke: TStylePen; const AGeometry: ID2D1Geometry);
begin
  ThisRenderTarget.DrawGeometry(AGeometry, ID2D1Brush(TD2DPen(AStroke.NativeObject).Brush), AStroke.Width,
    TD2DPen(AStroke.NativeObject).Style);
end;

procedure TDirect2DPainter.DrawGeometry(const AFill: TStyleBrush; const AStroke: TStylePen; const AGeometry: ID2D1Geometry);
begin
  if Assigned(AFill) then
    ThisRenderTarget.FillGeometry(AGeometry, TD2DBrush(AFill.NativeObject).Brush);

  if Assigned(AStroke) then
    DrawGeometry(AStroke, AGeometry);
end;

procedure TDirect2DPainter.EndPaint;
begin
  ThisRenderTarget.EndDraw;
end;

function TDirect2DPainter.GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF;
var
  vTextLayout: IDWriteTextLayout;
  vTextMetrics: TDWriteTextMetrics;
begin
  OleCheck(DWriteFactory.CreateTextLayout(PWideChar(AText), Length(AText),
    TD2DFont(AFont.NativeObject).TextFormat, 0, 0, vTextLayout));
  vTextLayout.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP);
  vTextLayout.SetParagraphAlignment(DWRITE_PARAGRAPH_ALIGNMENT_NEAR);
  vTextLayout.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);

  OleCheck(vTextLayout.GetMetrics(vTextMetrics));

  Result.cx := Round(vTextMetrics.widthIncludingTrailingWhitespace);
  Result.cy := Round(vTextMetrics.height);
end;

function TDirect2DPainter.MakeD2DColorF(const AColor: Cardinal): TD2D1ColorF;
begin
  Result := D2D1ColorF(Byte(AColor shr 16) / 255.0, Byte(AColor shr 8) / 255.0,
    Byte(AColor) / 255.0, Byte(AColor shr 24) / 255.0);
end;

function TDirect2DPainter.MakeD2DPointF(const X, Y: Single): TD2D1Point2F;
begin
  Result.x := X + 0.5;
  Result.y := Y + 0.5;
end;

function TDirect2DPainter.MakeD2DPointF(const APoint: TPointF): TD2D1Point2F;
begin
  Result := MakeD2DPointF(APoint.X, APoint.Y);
end;

function TDirect2DPainter.MakeD2DRectF(const ARect: TRectF; const ANeedShrink: Boolean = False): TD2D1RectF;
var
  vCorrection: Single;
begin
  vCorrection := IfThen(ANeedShrink, 0.5, 0);

  Result.left := ARect.Left + vCorrection;
  Result.top := ARect.Top + vCorrection;
  Result.right := ARect.Right - vCorrection;
  Result.bottom := ARect.Bottom- vCorrection;
end;

function TDirect2DPainter.ThisRenderTarget: ID2D1RenderTarget;
begin
  Result := TD2DDrawContext(FContext).RenderTarget;
end;

{ TD2DNinePatchImage }

function TD2DNinePatchImage.CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject;
begin
  Result := TD2DImage.Create(TDirect2DPainter(FPainter).CutImage(TD2DImage(FImage).Bitmap, ALeft, ATop, AWidth, AHeight));
end;

function TD2DNinePatchImage.GetHeight: Integer;
var
  vSize: TD2DSizeF;
begin
  TD2DImage(FImage).Bitmap.GetSize(vSize);
  Result := Round(vSize.height);
end;

function TD2DNinePatchImage.GetWidth: Integer;
var
  vSize: TD2DSizeF;
begin
  TD2DImage(FImage).Bitmap.GetSize(vSize);
  Result := Round(vSize.width);
end;

{ TD2DPen }

constructor TD2DPen.Create(const AStyle: ID2D1StrokeStyle; const ABrush: ID2D1Brush);
begin
  inherited Create;
  FStyle := AStyle;
  FBrush := ABrush;
end;

destructor TD2DPen.Destroy;
begin
  FStyle := nil;
  FBrush := nil;
  inherited Destroy;
end;

{ TD2DFont }

constructor TD2DFont.Create(const ATextFormat: IDWriteTextFormat; const ABrush: ID2D1Brush);
begin
  inherited Create;
  FTextFormat := ATextFormat;
  FBrush := ABrush;
end;

destructor TD2DFont.Destroy;
begin
  FTextFormat := nil;
  FBrush := nil;
  inherited Destroy;
end;

{ TD2DBrush }

constructor TD2DBrush.Create(const ABrush: ID2D1Brush);
begin
  inherited Create;
  FBrush := ABrush;
end;

destructor TD2DBrush.Destroy;
begin
  FBrush := nil;
  inherited Destroy;
end;

{ TD2DPath }

constructor TD2DPath.Create(const AGeometry: ID2D1Geometry);
begin
  inherited Create;
  FGeometry := AGeometry;
end;

destructor TD2DPath.Destroy;
begin
  FGeometry := nil;
  inherited Destroy;
end;

{ TD2DImage }

constructor TD2DImage.Create(const ABitmap: ID2D1Bitmap);
begin
  inherited Create;
  FBitmap := ABitmap;
end;

destructor TD2DImage.Destroy;
begin
  FBitmap := nil;
  inherited Destroy;
end;

{ TDirect2DScene }

function TDirect2DScene.CreatePainter(const AContainer: TObject): TPainter;
var
  vContainer: TDrawContainer absolute AContainer;
begin
  Result := TDirect2DPainter.Create(AContainer);
  FStaticContext := TD2DDrawContext(Result.CreateDrawContext(vContainer.Width, vContainer.Height));
end;

procedure TDirect2DScene.DoDestroyScene;
begin
  inherited DoDestroyScene;
  FreeAndNil(FStaticContext);
end;

procedure TDirect2DScene.DoRender(const ANeedFullRepaint: Boolean);
var
  vDeviceContext: ID2D1DeviceContext;
  vOldTarget: ID2D1Image;
begin
  if ANeedFullRepaint then
  begin
    vDeviceContext := TDirect2DPainter(FPainter).ThisRenderTarget as ID2D1DeviceContext;
    vDeviceContext.GetTarget(vOldTarget);
    try
      vDeviceContext.SetTarget(FStaticContext.BitmapTarget as ID2D1Image);
      FPainter.BeginPaint;
      try
        FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
      finally
        FPainter.EndPaint;
      end;
    finally
      vDeviceContext.SetTarget(vOldTarget);
    end;
  end;

  FPainter.BeginPaint;
  try
    TDirect2DPainter(FPainter).ThisRenderTarget.DrawBitmap(FStaticContext.BitmapTarget);
    FRoot.RenderDynamic(FPainter, GetSceneRect);
  finally
    FPainter.EndPaint;
  end;
end;

procedure TDirect2DScene.UpdateContexts(const AWidth, AHeight: Single);
begin
  FStaticContext.SetSize(AWidth, AHeight);
end;

{ TD2DDrawContext }

constructor TD2DDrawContext.Create(const APainter: TPainter; const AContainer: TDrawContainer; const AWidth, AHeight: Single);
var
  vTargetProperties: TD2D1RenderTargetProperties;
  vPixelFormat: TD2D1PixelFormat;
begin
  FPainter := APainter;
  if Assigned(AContainer) then
  begin
    FCanvas := AContainer.Canvas;
    FHandle := AContainer.HWND;
    vPixelFormat := D2D1PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED);

    vTargetProperties := D2D1RenderTargetProperties(D2D1_RENDER_TARGET_TYPE_DEFAULT, vPixelFormat,
      0, 0, D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE);
    // Для DC канвы
    //D2DFactory.CreateDCRenderTarget(vTargetProperties, ID2D1DCRenderTarget(FRenderTarget));
    //ID2D1DCRenderTarget(FRenderTarget).BindDC(FHandle, Rect(0, 0, Round(AWidth), Round(AHeight)));

    // Для HWND канвы
    D2DFactory.CreateHwndRenderTarget(vTargetProperties, D2D1HwndRenderTargetProperties(
      FHandle, D2D1SizeU(Round(AWidth), Round(AHeight))), ID2D1HwndRenderTarget(FRenderTarget));

    FRenderTarget.SetAntialiasMode(D2D1_ANTIALIAS_MODE_PER_PRIMITIVE);
    FRenderTarget.SetTextAntialiasMode(D2D1_TEXT_ANTIALIAS_MODE_GRAYSCALE);
  end
  else begin
    FHandle := INVALID_HANDLE_VALUE;
    FRenderTarget := TD2DDrawContext(APainter.Context).RenderTarget;
    CreateBitmapTarget(AWidth, AHeight);
  end;
end;

procedure TD2DDrawContext.CreateBitmapTarget(const AWidth, AHeight: Single);
var
  vPixelFormat: TD2D1PixelFormat;
  vDeviceContext: ID2D1DeviceContext;
  vProperties: D2D1_BITMAP_PROPERTIES1;
begin
  vDeviceContext := FRenderTarget as ID2D1DeviceContext;
  vPixelFormat := D2D1PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM, D2D1_ALPHA_MODE_PREMULTIPLIED);
  vProperties := D2D1BitmapProperties1(D2D1_BITMAP_OPTIONS_TARGET, vPixelFormat, 96, 96);

  OleCheck(vDeviceContext.CreateBitmap(D2D1SizeU(Round(AWidth), Round(AHeight)),
    nil, 0, @vProperties, FBitmapTarget));
end;

destructor TD2DDrawContext.Destroy;
begin
  FRenderTarget := nil;
  FBitmapTarget := nil;
  inherited Destroy;
end;

procedure TD2DDrawContext.DoSetSize(const AWidth, AHeight: Single);
var
  vSize: D2D1_SIZE_U;
begin
  if Assigned(FBitmapTarget) then
  begin
    CreateBitmapTarget(AWidth, AHeight);
  end
  else if Assigned(FRenderTarget) then
  begin
    // Для DC канвы
    //if FHandle = FCanvas.Handle then
    //  Exit;

    //FHandle := FCanvas.Handle;
    //ID2D1DCRenderTarget(FRenderTarget).BindDC(FHandle, Rect(0, 0, Round(AWidth), Round(AHeight)));
    // Для HWND канвы
    vSize := D2D1SizeU(Round(AWidth), Round(AHeight));
    ID2D1HwndRenderTarget(FRenderTarget).Resize(vSize);
  end;
end;

procedure TD2DDrawContext.SaveToFile(const AFileName: string);
var
  vFileExt: string;
  vWICFormatGUID: TGUID;
  vWICFactory: IWICImagingFactory;
  vFileName: string;
  vStream: IWICStream;
  vEncoder: IWICBitmapEncoder;
  vFrameEncode: IWICBitmapFrameEncode;
  vWidth, vHeight: Cardinal;
  vFormat: TGUID;
  vBitmap: IWICBitmap;
  vEncoderOptions: IPropertyBag2;
begin
  vFileName := AFileName;

  vFileExt := TPath.GetExtension(vFileName);
  if vFileExt = '.png' then
    vWICFormatGUID := GUID_ContainerFormatPng
  else if (vFileExt = '.jpg') or (vFileExt = '.jpeg') then
    vWICFormatGUID := GUID_ContainerFormatJpeg
  else if vFileExt = '.bmp' then
    vWICFormatGUID := GUID_ContainerFormatBmp
  else if vFileExt = '.ico' then
    vWICFormatGUID := GUID_ContainerFormatIco
  else if vFileExt = '.tiff' then
    vWICFormatGUID := GUID_ContainerFormatTiff
  else if vFileExt = '.gif' then
    vWICFormatGUID := GUID_ContainerFormatGif
  else if vFileExt = '.wmp' then
    vWICFormatGUID := GUID_ContainerFormatWmp
  else
    Exit;

  vWICFactory := TDirect2DPainter(FPainter).FImageFactory;
  vFormat := GUID_WICPixelFormat32bppPBGRA;

  OleCheck(vWICFactory.CreateStream(vStream));
  OleCheck(vStream.InitializeFromFilename(PChar(vFileName), GENERIC_WRITE));

  // Create and initialize WIC Bitmap Encoder
  OleCheck(vWICFactory.CreateEncoder(vWICFormatGUID, TGUID.Empty, vEncoder));
  // ERROR HERE!!! Not supported
  OleCheck(vEncoder.Initialize(vStream, WICBitmapNoCache));

  // Create and initialize WIC Frame Encoder
  OleCheck(vEncoder.CreateNewFrame(vFrameEncode, vEncoderOptions));
  OleCheck(vFrameEncode.Initialize(nil));

  vFrameEncode.SetSize(vWidth, vHeight);
  vFrameEncode.SetPixelFormat(vFormat);
  vFrameEncode.WriteSource(vBitmap, nil);

  // Retrieve D2D Device
  //vDeviceContext := FRenderTarget as ID2D1DeviceContext;
  //vDeviceContext.GetDevice(vDevice);

  // Create IWICImageEncoder
  //vWICFactory.CreateImageEncoder(vDevice, vImageEncoder);
  //OleCheck(vImageEncoder.WriteFrame(FBitmapTarget as ID2D1Image, vFrameEncode, vParameters));
  OleCheck(vFrameEncode.Commit);
  OleCheck(vEncoder.Commit);

  // Flush all memory buffers to the next-level storage object.
  OleCheck(vStream.Commit(STGC_DEFAULT));
end;

initialization

TBaseModule.RegisterModule('Painting', 'Direct2D', TDirect2DScene);

end.

