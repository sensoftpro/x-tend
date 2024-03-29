unit uFMXPainter;

interface

uses
  Types, Classes, FMX.Graphics, uDrawStyles, FMX.Controls, FMX.Types, Math, FMX.Objects, IOUtils, Math.Vectors,
  UITypes, uConsts, SysUtils, FMX.Filter.Effects, FMX.Platform;

type
  TFMXContext = class(TDrawContext)
  private
    FBitmap: TBitmap;
    FCanvas: TCanvas;
  protected
    procedure DoSetSize(const AWidth, AHeight: Single); override;
  public
    constructor Create(const ACanvas: TCanvas; const AWidth, AHeight: Single);
    destructor Destroy; override;

    procedure SaveToFile(const AFileName: string); override;
    procedure UpdateCanvas(const ACanvas: TObject); override;

    property Bitmap: TBitmap read FBitmap;
    property Canvas: TCanvas read FCanvas;
  end;

  TFMXNinePatchImage = class(TNinePatchImage)
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject; override;
  end;

  TFMXPainter = class(TPainter)
  private
    FInvertEffect: TInvertEffect;
    FInvertBitmap: TBitmap;
    FSaveState: TCanvasSaveState;
    function ThisCanvas: TCanvas;
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
    procedure DoSetPixel(const AX, AY: Integer; const AColor: Cardinal); override;
    procedure DoDrawText(const AFont: TStyleFont; const AText: string; const ARect: TRectF;
      const AOptions: Cardinal; const AAngle: Single); override;
    function GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF; override;
    procedure DoClipRect(const ARect: TRectF); override;
    procedure DoInvertRect(const ARect: TRectF); override;
    procedure DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single); override;
    procedure DoDrawContext(const AContext: TDrawContext); override;
    procedure DoStretchDrawContext(const AContext: TDrawContext; const ARect: TRect); override;

    procedure DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal); override;
    procedure DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal); override;
    procedure DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal); override;
  public
    constructor Create(const AScene: TObject; const AContainer: TObject); override;
    destructor Destroy; override;

    procedure CreateBrush(const AFill: TStyleBrush); override;
    procedure CreatePen(const AStroke: TStylePen); override;
    procedure CreateFont(const AFont: TStyleFont); override;
    procedure CreateImage(const AImage: TStyleImage); override;
    function CreateDrawContext(const AWidth, AHeight: Single): TDrawContext; override;

    procedure BeginPaint; override;
    procedure EndPaint; override;
  end;

implementation

uses
  fmxScene, uModule;

function MulDiv(ANumber, ANumerator, ADenominator: Integer): Integer;
begin
  if ADenominator = 0 then
    Result := -1
  else
    Result := Round(Int64(ANumber) * Int64(ANumerator) / ADenominator);
end;

{ TFMXPainter }

procedure TFMXPainter.BeginPaint;
begin
  ThisCanvas.BeginScene;
end;

procedure TFMXPainter.EndPaint;
begin
  ThisCanvas.EndScene;
end;

constructor TFMXPainter.Create(const AScene: TObject; const AContainer: TObject);
var
  vContainer: TControl absolute AContainer;
begin
  inherited Create(AScene, AContainer);
  FContext := TFMXContext.Create(vContainer.Canvas, vContainer.Width, vContainer.Height);
  FInvertEffect := nil;
  FInvertBitmap := nil;
end;

function TFMXPainter.CreateDrawContext(const AWidth, AHeight: Single): TDrawContext;
begin
  Result := TFMXContext.Create(nil, AWidth, AHeight);
end;

procedure TFMXPainter.CreateBrush(const AFill: TStyleBrush);
var
  vBrush: TBrush;
begin
  if (AFill.GradientKind <> gkNone) then
  begin
    vBrush := TBrush.Create(TBrushKind.Gradient, AFill.Color);
    vBrush.Gradient.Color := AFill.Color;
    vBrush.Gradient.Color1 := AFill.BackColor;
    case AFill.GradientKind of
      gkVertical: vBrush.Gradient.Style := TGradientStyle.Linear;
      gkHorizontal: vBrush.Gradient.Style := TGradientStyle.Radial;
    end;
  end else
    vBrush := TBrush.Create(TBrushKind.Solid, AFill.Color);
  AFill.NativeObject := vBrush;
end;

procedure TFMXPainter.CreateFont(const AFont: TStyleFont);
var
  vFont: TFont;
  vMetricsService: IFMXDeviceMetricsService;
  vPPI: Integer;
begin
  vFont := TFont.Create;
  vPPI := 96;
  if TPlatformServices.Current.SupportsPlatformService(IFMXDeviceMetricsService, vMetricsService) then
  begin
    vPPI := vMetricsService.GetDisplayMetrics.PixelsPerInch;
  end;
  vFont.Size := MulDiv(Round(AFont.Size), vPPI, 72);

  vFont.Family := AFont.Family;
  vFont.Style := [];
  if (FontStyleBold and AFont.Style > 0) then
    vFont.Style := [TFontStyle.fsBold];
  if (FontStyleItalic and AFont.Style > 0) then
    vFont.Style := vFont.Style + [TFontStyle.fsItalic];
  if (FontStyleUnderline and AFont.Style > 0) then
    vFont.Style := vFont.Style + [TFontStyle.fsUnderline];
  if (FontStyleStrikeout and AFont.Style > 0) then
    vFont.Style := vFont.Style + [TFontStyle.fsStrikeOut];

  AFont.NativeObject := vFont;
end;

procedure TFMXPainter.CreateImage(const AImage: TStyleImage);
var
  vImage: TBitmap;
begin
  if Assigned(AImage) then
  begin
    if not TFile.Exists(AImage.FileName) then
      Exit;
	  
    vImage := TBitmap.Create;
    vImage.LoadFromFile(AImage.FileName);

    if AImage.IsNinePatch then
      AImage.NativeObject := TFMXNinePatchImage.Create(Self, vImage, AImage.CutRect.Left, AImage.CutRect.Right,
        AImage.CutRect.Top, AImage.CutRect.Bottom)
    else
      AImage.NativeObject := vImage;
  end;
end;

procedure TFMXPainter.CreatePen(const AStroke: TStylePen);
var
  vStroke: TStrokeBrush;
begin
  vStroke := TStrokeBrush.Create(TBrushKind.Solid, AStroke.Color);
  vStroke.Thickness := AStroke.Width;
  case AStroke.Style of
    psDot: begin
      vStroke.Dash := TStrokeDash.Custom;
      vStroke.SetCustomDash(TArray<Single>.Create(3,3), 0)
    end;
    psDash: vStroke.Dash := TStrokeDash.Dash;
    psDashDot: vStroke.Dash := TStrokeDash.DashDot;
    psDashDotDot: vStroke.Dash := TStrokeDash.DashDotDot;
    psSolid: vStroke.Dash := TStrokeDash.Solid;
  end;
  vStroke.Join := TStrokeJoin.Round;

  AStroke.NativeObject := vStroke;
end;

destructor TFMXPainter.Destroy;
begin
  FreeAndNil(FInvertEffect);
  FreeAndNil(FInvertBitmap);
  inherited Destroy;
end;

procedure TFMXPainter.DoClipRect(const ARect: TRectF);
begin
  if not IsRectEmpty(ARect) then
  begin
    FSaveState := ThisCanvas.SaveState;
    ThisCanvas.IntersectClipRect(ARect);
  end
  else
    ThisCanvas.RestoreState(FSaveState);
end;

procedure TFMXPainter.DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal);
begin
  TBrush(AFill.NativeObject).Color := AColor;
end;

procedure TFMXPainter.DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal);
begin
  ThisCanvas.Fill.Color := AColor;
end;

procedure TFMXPainter.DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal);
begin
  TStrokeBrush(AStroke.NativeObject).Color := AColor;
end;

procedure TFMXPainter.DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vPath: TPathData;
  vPoints: PPointFArray absolute APoints;
  i: Integer;
begin
  vPath := TPathData.Create;
  i := 0;
  while i < ACount - 1 do
    begin
      vPath.MoveTo(vPoints[0 + (Round(i/3) * 3)].Round);
      vPath.CurveTo(vPoints[i + 1].Round, vPoints[i + 2].Round, vPoints[i + 3].Round);
      Inc(i,3);
      ThisCanvas.DrawPath(vPath, 1, TStrokeBrush(AStroke.NativeObject));
      vPath.Clear;
    end;
  FreeAndNil(vPath);
end;

procedure TFMXPainter.DoDrawContext(const AContext: TDrawContext);
begin
  if Assigned(TFMXContext(AContext).Bitmap) then
    ThisCanvas.DrawBitmap(TFMXContext(AContext).Bitmap, AContext.ClientRect,
      FContext.ClientRect, 1, True);
end;

procedure TFMXPainter.DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
begin
  if Assigned(AFill) then
    ThisCanvas.FillEllipse(ARect, 1, TBrush(AFill.NativeObject));
  if Assigned(AStroke) then
    ThisCanvas.DrawEllipse(ARect, 1, TStrokeBrush(AStroke.NativeObject));
end;

procedure TFMXPainter.DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
begin
  ThisCanvas.DrawBitmap(TBitmap(AImage), TBitmap(AImage).BoundsF, ARect.Round, 1, True);
end;

procedure TFMXPainter.DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF);
begin
  ThisCanvas.DrawLine(APoint1  + PointF(0.5, 0.5), APoint2 + PointF(0.5, 0.5), 1, TStrokeBrush(AStroke.NativeObject));
end;

procedure TFMXPainter.DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject);
begin
end;

procedure TFMXPainter.DoDrawPie(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF;
  const AStartAngle, AEndAngle: Single);
  function CorrectAngle(const AAngle: Single): Single;
  var
    vAngle: Single;
  begin
    vAngle := AAngle - Floor(AAngle / 360) * 360;

    if IsZero(Frac(vAngle / 90)) then
      Result := vAngle
    else
    begin
      Result := RadToDeg(ArcTan(Tan(DegToRad(vAngle)) * ARect.Width / ARect.Height));
      if Result < 0 then
        Result := Result + 180;
      if vAngle > 180 then
        Result := Result + 180;
    end;
  end;

var
  vStartAngle: Single;
  vEndAngle: Single;
  vSweepAngle: Single;
  vCentre: TPointF;
  vPath: TPathData;
  vWidth, vHeight, vTheta, vAngleIncrement, vX, vY, vEnd: Single;
begin
  vStartAngle := -CorrectAngle(AStartAngle);
  vEndAngle := CorrectAngle(AEndAngle);
  vSweepAngle := vEndAngle - vStartAngle;
  if vSweepAngle < 0 then
    vSweepAngle := vSweepAngle + 360;

  vWidth  := ARect.Width;
  vHeight := ARect.Height;
  vCentre := ARect.CenterPoint;
  vAngleIncrement := DegToRad(vSweepAngle) / vSweepAngle;

  vPath := TPathData.Create;
  vPath.MoveTo(vCentre);

  vTheta := vStartAngle;
  vEnd := DegToRad(vEndAngle);
  while vTheta < vEnd do
  begin
    vTheta := vTheta + vAngleIncrement;
    vX := vWidth / 2 * cos(vTheta);
    vY := vHeight / 2 * sin(vTheta);
    vPath.LineTo(PointF(vX, vY) + vCentre);
  end;

  vPath.LineTo(vCentre);

  if Assigned(AFill) then
    ThisCanvas.FillPath(vPath, 1, TBrush(AFill.NativeObject));
  if Assigned(AStroke) then
    ThisCanvas.DrawPath(vPath, 1, TStrokeBrush(AStroke.NativeObject));
  FreeAndNil(vPath);
end;

procedure TFMXPainter.DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vPath: TPathData;
  vPoints: TArray<TPoint>;
  i: Integer;
  function PPointFToPoints(const APoints: PPointF; const ACount: Integer): TArray<TPoint>;
  var
    j: Integer;
  begin
    SetLength(Result, ACount);
    for j := 0 to ACount - 1 do
      Result[j] := PPointF(NativeInt(APoints) + j * SizeOf(TPointF))^.Round;
  end;
begin
  vPath := TPathData.Create;
  vPoints := PPointFToPoints(APoints, ACount);
  vPath.MoveTo(vPoints[0]);
  for i := 1 to ACount - 1 do
  begin
    vPath.LineTo(vPoints[i] + PointF(0.5, 0.5));
  end;
  ThisCanvas.DrawPath(vPath, 1, TStrokeBrush(AStroke.NativeObject));
  FreeAndNil(vPath);
end;

procedure TFMXPainter.DoDrawRect(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
begin
  if Assigned(AFill) then
    ThisCanvas.FillRect(ARect, ARect.Width, ARect.Height, [], 1, TBrush(AFill.NativeObject), TCornerType.Round);
  if Assigned(AStroke) then
    ThisCanvas.DrawRect(ARect, ARect.Width, ARect.Height, [], 1, TStrokeBrush(AStroke.NativeObject), TCornerType.Round);
end;

procedure TFMXPainter.DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen; const APoints: PPointF;
  const ACount: Integer);
var
  vPoints: PPointFArray absolute APoints;
  vPathData: TPathData;
  i: Integer;
begin
  vPathData := TPathData.Create;
  vPathData.MoveTo(vPoints[0]);
  for i := 1 to ACount - 1 do
  begin
    vPathData.LineTo(vPoints[i]);
  end;
  if Assigned(AStroke) then
    ThisCanvas.DrawPath(vPathData, 1, TStrokeBrush(AStroke.NativeObject));

  if Assigned(AFill) then
    ThisCanvas.FillPath(vPathData, 1, TBrush(AFill.NativeObject));
  FreeAndNil(vPathData);
end;

procedure TFMXPainter.DoDrawText(const AFont: TStyleFont; const AText: string; const ARect: TRectF;
  const AOptions: Cardinal; const AAngle: Single);
var
  vSize: TSizeF;
  vR: TRectF;
  vHorzAlign: TTextAlign;
  vVertAlign: TTextAlign;
  vCanvas: TCanvas;
  vSaveMatrix: TMatrix;
  vMatrix: TMatrix;
begin
  vCanvas := ThisCanvas;
  vCanvas.Font.Family := TFont(AFont.NativeObject).Family;
  vCanvas.Font.Size := TFont(AFont.NativeObject).Size;
  vCanvas.Font.Style := TFont(AFont.NativeObject).Style;
  vCanvas.Fill.Color := AFont.Color;
  vR := ARect;
  if (DT_CENTER and AOptions) > 0 then
    vHorzAlign := TTextAlign.Center
  else if (DT_RIGHT and AOptions) > 0 then
    vHorzAlign := TTextAlign.Trailing
  else
    vHorzAlign := TTextAlign.Leading;

  if (DT_VCENTER and AOptions) > 0 then
    vVertAlign := TTextAlign.Center
  else if (DT_BOTTOM and AOptions) > 0 then
    vVertAlign := TTextAlign.Trailing
  else
    vVertAlign := TTextAlign.Leading;

  if IsZero(AAngle) then
    vCanvas.FillText(vR, AText, False, 1, [], vHorzAlign, vVertAlign)
  else begin
    vSize := GetTextExtents(AFont, AText);

    vR := RectF(0, -vSize.cy / 2, vSize.cx, vSize.cy / 2);
    vSaveMatrix := vCanvas.Matrix;
    try
      vMatrix := TMatrix.CreateRotation(DegToRad(AAngle));
      vMatrix.m31 := Round(ARect.Left + ARect.Width / 2);
      vMatrix.m32 := Round(ARect.Bottom);
      vCanvas.MultiplyMatrix(vMatrix);

      vCanvas.FillText(vR, AText, False, 1, [], vHorzAlign, vVertAlign);
    finally
      vCanvas.SetMatrix(vSaveMatrix);
    end;
  end;
end;

procedure TFMXPainter.DoInvertRect(const ARect: TRectF);
var
  vImageContext: TDrawContext;
begin
  vImageContext := TFMXScene(FScene).ImageContext;
  if not Assigned(vImageContext) then
    Exit;

  if not Assigned(FInvertEffect) then
    FInvertEffect := TInvertEffect.Create(nil);
  if not Assigned(FInvertBitmap) then
    FInvertBitmap := TBitmap.Create;

  FInvertBitmap.SetSize(Round(ARect.Width), Round(ARect.Height));
  FInvertBitmap.CopyFromBitmap(TFMXContext(vImageContext).Bitmap, ARect.Round, 0, 0);

  FInvertEffect.ProcessEffect(ThisCanvas, FInvertBitmap, 0);

  ThisCanvas.DrawBitmap(FInvertBitmap, FInvertBitmap.BoundsF, ARect, 1, True);
end;

procedure TFMXPainter.DoSetPixel(const AX, AY: Integer; const AColor: Cardinal);
var
  vPixelRegion: TRectF;
  vPixelPos: TPointF;
begin
  ThisCanvas.Stroke.Color := AColor;
  vPixelPos := ThisCanvas.AlignToPixel(TPointF.Create(AX, AY));
  vPixelRegion := TRectF.Create(vPixelPos, 1, 1);
  ThisCanvas.DrawRect(vPixelRegion, 0, 0, AllCorners, 1);
end;

procedure TFMXPainter.DoStretchDrawContext(const AContext: TDrawContext;
  const ARect: TRect);
begin
  if Assigned(TFMXContext(AContext).Bitmap) then
    ThisCanvas.DrawBitmap(TFMXContext(AContext).Bitmap, AContext.ClientRect, ARect, 1, True);
end;

function TFMXPainter.GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF;
var
  R: TRectF;
begin
  ThisCanvas.Font.Family := TFont(AFont.NativeObject).Family;
  ThisCanvas.Font.Size := TFont(AFont.NativeObject).Size;
  ThisCanvas.Font.Style := TFont(AFont.NativeObject).Style;

  R := RectF(0, 0, 10000, 10000);
  ThisCanvas.MeasureText(R, AText, False, [], TTextAlign.Leading, TTextAlign.Leading);
  Result.cx := R.Right;
  Result.cy := R.Bottom - 1;
  Result := Result.Round;
end;

function TFMXPainter.ThisCanvas: TCanvas;
begin
  Result := TFMXContext(FContext).Canvas;
end;

{ TFMXNinePatchImage }

function TFMXNinePatchImage.CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject;
var
  vOldBitmap, vNewBitmap: TBitmap;
begin
  vOldBitmap := TBitmap(FImage);
  vNewBitmap := TBitmap.Create;
  vNewBitmap.Width := AWidth;
  vNewBitmap.Height := AHeight;
  vNewBitmap.CopyFromBitmap(vOldBitmap, Rect(ALeft, ATop, ALeft + AWidth, ATop + AHeight), 0, 0);
  Result := vNewBitmap;
end;

function TFMXNinePatchImage.GetHeight: Integer;
begin
  Result := TBitmap(FImage).Height;
end;

function TFMXNinePatchImage.GetWidth: Integer;
begin
  Result := TBitmap(FImage).Width;
end;

{ TFMXContext }

constructor TFMXContext.Create(const ACanvas: TCanvas; const AWidth, AHeight: Single);
begin
  inherited Create(AWidth, AHeight);

  if not Assigned(ACanvas) then
  begin
    FBitmap := TBitmap.Create(Round(AWidth), Round(AHeight));
    FCanvas := TCanvasManager.CreateFromBitmap(FBitmap, TCanvasQuality.HighPerformance);
  end
  else begin
    FCanvas := ACanvas;
    FBitmap := nil;
  end;
end;

destructor TFMXContext.Destroy;
begin
  if Assigned(FBitmap) then
  begin
    FreeAndNil(FCanvas);
    FreeAndNil(FBitmap);
  end
  else
    FCanvas := nil;
end;

procedure TFMXContext.DoSetSize(const AWidth, AHeight: Single);
begin
  inherited DoSetSize(AWidth, AHeight);
  if Assigned(FBitmap) then
  begin
    FBitmap.SetSize(Round(AWidth), Round(AHeight));
    FreeAndNil(FCanvas);
    FCanvas := TCanvasManager.CreateFromBitmap(FBitmap, TCanvasQuality.HighPerformance);
  end;
end;

procedure TFMXContext.SaveToFile(const AFileName: string);
begin
  FBitmap.SaveToFile(AFileName, nil);
end;

procedure TFMXContext.UpdateCanvas(const ACanvas: TObject);
begin
  Assert(not Assigned(FBitmap), 'Attempt to refresh bitmap canvas');
  FCanvas := TCanvas(ACanvas);
end;

initialization

TBaseModule.RegisterModule('Painting', '', 'FMX', TFMXScene);

end.
