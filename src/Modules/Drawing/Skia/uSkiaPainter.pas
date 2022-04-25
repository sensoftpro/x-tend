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

unit uSkiaPainter;

interface

uses
  Windows, Graphics, Types, Classes, ExtCtrls, Skia, uScene, uWinScene, uDrawStyles, uConsts;

type
  TSkiaPen = class
  private
    FPen: ISkPaint;
  public
    constructor Create(const APen: ISkPaint);
    destructor Destroy; override;

    property Pen: ISkPaint read FPen;
  end;

  TSkiaBrush = class
  private
    FBrush: ISkPaint;
  public
    constructor Create(const ABrush: ISkPaint);
    destructor Destroy; override;

    property Brush: ISkPaint read FBrush;
  end;

  TSkiaFont = class
  private
    FFont: ISkFont;
    FBrush: ISkPaint;
    FMetrics: TSkFontMetrics;
  public
    constructor Create(const AFont: ISkFont; const ABrush: ISkPaint);
    destructor Destroy; override;

    property Font: ISkFont read FFont;
    property Brush: ISkPaint read FBrush;
    property Metrics: TSkFontMetrics read FMetrics;
  end;

  TSkiaImage = class
  private
    FImage: ISkImage;
  public
    constructor Create(const AImage: ISkImage);
    destructor Destroy; override;

    property Image: ISkImage read FImage;
  end;

  TSkiaPath = class
  private
    FPath: ISkPath;
  public
    constructor Create;
    destructor Destroy; override;

    property Path: ISkPath read FPath;
  end;

  TSkiaNinePatchImage = class(TNinePatchImage)
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject; override;
  end;

  TSkiaPainter = class(TPainter)
  private
    FSurface: ISkSurface;
    FCanvas: ISkCanvas;
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
  end;

  TSkiaScene = class(TScene)
  private
    FPanel: TMyPanel;
    FContainer: TDrawContainer;

    FDrawBufferDC: HDC;
    FSurface: ISkSurface;
    FDrawBuffer: HBITMAP;
    FStaticImage: ISkImage;

    procedure CreateBuffer(const AMemDC: HDC; out ABuffer: HBITMAP; out AData: Pointer; out AStride: Integer);
    procedure DeleteBuffers;
  protected
    function DoCreateScene(const APlaceholder: TObject): TPainter; override;
    procedure DoDestroyScene; override;

    procedure DoActivate; override;
    procedure SetEnabled(const AValue: Boolean); override;

    function GetSceneRect: TRectF; override;
    function GetClientPos: TPointF; override;

    procedure DoRender(const ANeedFullRepaint: Boolean); override;
    procedure UpdateContexts(const AWidth, AHeight: Single); override;
    function CreatePainter(const AContainer: TObject): TPainter; override;
  public
    procedure SaveToFile(const AFileName: string; const AWaterMark: string = ''); override;
  end;

implementation

uses
  Controls, StrUtils, SysUtils, IOUtils, Math, uModule;

{ TSkiaPainter }

constructor TSkiaPainter.Create(const AContainer: TObject);
var
  vContainer: TDrawContainer absolute AContainer;
begin
  inherited Create(AContainer);
  FContext := TDrawContext.Create(0, 0);
end;

procedure TSkiaPainter.CreateBrush(const AFill: TStyleBrush);
var
  vBrush: ISkPaint;
begin
  vBrush := TSkPaint.Create;
  vBrush.AntiAlias := True;
  vBrush.Style := TSkPaintStyle.Fill;
  vBrush.Color := AFill.Color;

  if AFill.Style = bsSolid then
  begin
    if AFill.GradientKind <> gkNone then
    begin
      // Создать градиентную кисть, привязать ее к TStyleBrush
      AFill.NativeObject := TSkiaBrush.Create(vBrush);
    end
    else begin
      // Создать сплошную кисть, привязать ее к TStyleBrush
      AFill.NativeObject := TSkiaBrush.Create(vBrush);
    end;
  end
  else begin
    // Создать штриховую кисть, привязать ее к TStyleBrush
    AFill.NativeObject := TSkiaBrush.Create(vBrush);
  end;
end;

function TSkiaPainter.CreateDrawContext(const AWidth, AHeight: Single): TDrawContext;
begin
  Result := nil;
end;

procedure TSkiaPainter.CreateFont(const AFont: TStyleFont);
var
  vTypeface: ISkTypeface;
  vStyle: TSkFontStyle;
  vFont: ISkFont;
  vBrush: ISkPaint;
begin
  // Проанализировать стиль шрифта
  if (FontStyleItalic and AFont.Style) > 0 then
  begin
    if (FontStyleBold and AFont.Style) > 0 then
      vStyle := TSkFontStyle.BoldItalic
    else
      vStyle := TSkFontStyle.Italic;
  end
  else if (FontStyleBold and AFont.Style) > 0 then
    vStyle := TSkFontStyle.Bold
  else
    vStyle := TSkFontStyle.Normal;

  // Создать описатель шрифта и, при необходимости, кисть для его отрисовки
  vTypeface := TSkTypeface.MakeFromName(AFont.Family, vStyle);
  vFont := TSkFont.Create(vTypeface, AFont.Size * 96 / 72);
  //vFont.Edging := TSkFontEdging.AntiAlias;

  vBrush := TSkPaint.Create;
  vBrush.AntiAlias := True;
  vBrush.Style := TSkPaintStyle.Fill;
  vBrush.Color := AFont.Color;

  // Привязать все созданные объекты к TStyleFont
  AFont.NativeObject := TSkiaFont.Create(vFont, vBrush);
end;

procedure TSkiaPainter.CreateImage(const AImage: TStyleImage);
var
  vSkiaImage: ISkImage;
  vImage: TSkiaImage;
begin
  if not TFile.Exists(AImage.FileName) then
    Exit;

  // Загрузить рисунок из файла и привязать его к TStyleImage
  vSkiaImage := TSkImage.MakeFromEncoded(TFile.ReadAllBytes(AImage.FileName));

  vImage := TSkiaImage.Create(vSkiaImage);

  if AImage.IsNinePatch then
    AImage.NativeObject := TSkiaNinePatchImage.Create(Self, vImage, AImage.CutRect.Left, AImage.CutRect.Right,
      AImage.CutRect.Top, AImage.CutRect.Bottom)
  else
    AImage.NativeObject := vImage;
end;

procedure TSkiaPainter.CreatePen(const AStroke: TStylePen);
const
  cDashIntervals: array[psDash..psDashDotDot] of TArray<Single> = ([18, 6], [3, 3], [9, 6, 3, 6], [9, 3, 3, 3, 3, 3]);
var
  vPaint: ISkPaint;
  vDashPathEffect: ISkPathEffect;
begin
  vPaint := TSkPaint.Create;
  vPaint.Style := TSkPaintStyle.Stroke;
  vPaint.StrokeWidth := AStroke.Width;
  vPaint.StrokeJoin := TSkStrokeJoin.Miter;
  vPaint.StrokeCap := TSkStrokeCap.Butt;
  vPaint.AntiAlias := True;
  vPaint.Color := AStroke.Color;

  if AStroke.Style in [psDash..psDashDotDot] then
  begin
    vDashPathEffect := TSkPathEffect.MakeDash(cDashIntervals[AStroke.Style], 0);
    vPaint.PathEffect := vDashPathEffect;
  end;

  // Привязать созданные перья к TStylePen
  AStroke.NativeObject := TSkiaPen.Create(vPaint);
end;

destructor TSkiaPainter.Destroy;
begin
  // Очистить все созданные в конструкторе объекты
  inherited Destroy;
end;

procedure TSkiaPainter.DoClipRect(const ARect: TRectF);
begin
  FCanvas.ClipRect(ARect);
end;

procedure TSkiaPainter.DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal);
begin
  TSkiaBrush(AFill.NativeObject).Brush.Color := AColor;
end;

procedure TSkiaPainter.DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal);
begin
  TSkiaFont(AFont.NativeObject).Brush.Color := AColor;
end;

procedure TSkiaPainter.DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal);
begin
  TSkiaPen(AStroke.NativeObject).Pen.Color := AColor;
end;

procedure TSkiaPainter.DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vPoints: PPointFArray absolute APoints;
  vPath: ISkPath;
  vPathBuilder: ISkPathBuilder;
  i: Integer;
begin
  vPathBuilder := TSkPathBuilder.Create;
  try
    vPathBuilder.MoveTo(vPoints[0]);
    i := 1;
    while i < ACount do
    begin
      vPathBuilder.CubicTo(vPoints[i], vPoints[i + 1], vPoints[i + 2]);
      Inc(i, 3);
    end;
  finally
    vPath := vPathBuilder.Detach;
  end;

  FCanvas.DrawPath(vPath, TSkiaPen(AStroke.NativeObject).Pen);
end;

procedure TSkiaPainter.DoDrawContext(const AContext: TDrawContext);
begin
end;

procedure TSkiaPainter.DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
begin
  if Assigned(AFill) then
    FCanvas.DrawOval(ARect, TSkiaBrush(AFill.NativeObject).Brush);

  if Assigned(AStroke) then
    FCanvas.DrawOval(ARect, TSkiaPen(AStroke.NativeObject).Pen);
end;

procedure TSkiaPainter.DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
begin
  if not Assigned(AImage) then
    Exit;
  if Assigned(TSkiaImage(AImage).Image) then
    FCanvas.DrawImageRect(TSkiaImage(AImage).Image, ARect);
end;

procedure TSkiaPainter.DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF);
begin
  FCanvas.DrawLine(APoint1, APoint2, TSkiaPen(AStroke.NativeObject).Pen);
end;

procedure TSkiaPainter.DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject);
begin
  if Assigned(AFill) then
    FCanvas.DrawPath(TSkiaPath(APath).Path, TSkiaBrush(AFill.NativeObject).Brush);

  if Assigned(AStroke) then
    FCanvas.DrawPath(TSkiaPath(APath).Path, TSkiaPen(AStroke.NativeObject).Pen);
end;

procedure TSkiaPainter.DoDrawPie(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF;
  const AStartAngle, AEndAngle: Single);
var
  vStartAngle: Single;
  vEndAngle: Single;
  vSweepAngle: Single;

  function CorrectAngle(const AAngle: Single): Single;
  var
    vAngle: Single;
  begin
    vAngle := AAngle - Floor(AAngle / 360) * 360;

    if IsZero(Frac(vAngle / 90)) then
      Result := vAngle
    else begin
      Result := RadToDeg(ArcTan(Tan(DegToRad(vAngle)) * ARect.Width / ARect.Height));
      if Result < 0 then
        Result := Result + 180;
      if vAngle > 180 then
        Result := Result + 180;
    end;
  end;

begin
  vStartAngle := CorrectAngle(AStartAngle);
  vEndAngle := CorrectAngle(AEndAngle);
  vSweepAngle := vEndAngle - vStartAngle;
  if vSweepAngle < 0 then
    vSweepAngle := vSweepAngle + 360;

  if Assigned(AFill) then
    FCanvas.DrawArc(ARect, vStartAngle, vSweepAngle, True, TSkiaBrush(AFill.NativeObject).Brush);

  if Assigned(AStroke) then
    FCanvas.DrawArc(ARect, vStartAngle, vSweepAngle, True, TSkiaPen(AStroke.NativeObject).Pen);
end;

procedure TSkiaPainter.DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vPoints: PPointFArray absolute APoints;
  vPath: ISkPath;
  vPathBuilder: ISkPathBuilder;
  i: Integer;
begin
  vPathBuilder := TSkPathBuilder.Create;
  try
    vPathBuilder.MoveTo(vPoints[0]);
    for i := 1 to ACount - 1 do
      vPathBuilder.LineTo(vPoints[i]);
  finally
    vPath := vPathBuilder.Detach;
  end;

  FCanvas.DrawPath(vPath, TSkiaPen(AStroke.NativeObject).Pen);
end;

procedure TSkiaPainter.DoDrawRect(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
begin
  if Assigned(AFill) then
  begin
    if ((AFill.GradientKind = gkHorizontal) and (ARect.Width <> AFill.Rect.Width))
        or ((AFill.GradientKind = gkVertical) and (ARect.Height <> AFill.Rect.Height))
    then begin
      if AFill.GradientKind = gkHorizontal then
        TSkiaBrush(AFill.NativeObject).Brush.Shader := TSkShader.MakeGradientLinear(TPointF.Create(0, 0),
          TPointF.Create(ARect.Width, 0), [AFill.Color, AFill.BackColor], TSkTileMode.Clamp)
      else
        TSkiaBrush(AFill.NativeObject).Brush.Shader := TSkShader.MakeGradientLinear(TPointF.Create(0, 0),
          TPointF.Create(0, ARect.Height), [AFill.Color, AFill.BackColor], TSkTileMode.Clamp);

      AFill.Rect := ARect;
    end;

    FCanvas.DrawRect(ARect, TSkiaBrush(AFill.NativeObject).Brush);
  end;

  if Assigned(AStroke) then
    FCanvas.DrawRect(ARect, TSkiaPen(AStroke.NativeObject).Pen);
end;

procedure TSkiaPainter.DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen; const APoints: PPointF;
  const ACount: Integer);
var
  vPoints: PPointFArray absolute APoints;
  vPath: ISkPath;
  vPathBuilder: ISkPathBuilder;
  i: Integer;
begin
  vPathBuilder := TSkPathBuilder.Create;
  try
    vPathBuilder.MoveTo(vPoints[0]);
    for i := 1 to ACount - 1 do
      vPathBuilder.LineTo(vPoints[i]);
    vPathBuilder.Close;
  finally
    vPath := vPathBuilder.Detach;
  end;

  if Assigned(AFill) then
    FCanvas.DrawPath(vPath, TSkiaBrush(AFill.NativeObject).Brush);

  if Assigned(AStroke) then
    FCanvas.DrawPath(vPath, TSkiaPen(AStroke.NativeObject).Pen);
end;

procedure TSkiaPainter.DoDrawText(const AFont: TStyleFont; const AText: string;
  const ARect: TRectF; const AOptions: Cardinal; const AAngle: Single);
var
  vTextMetrics: TSizeF;
  vStartPoint: TPointF;
  vCalcRect: TRectF;
  vCenterPoint: TPointF;

  function CalcStartPoint(const ATextRect: TRectF): TPointF;
  begin
    Result := PointF(ATextRect.Left, ATextRect.Top);
    if (DT_CENTER and AOptions) > 0 then
      Result.X := Result.X + (ATextRect.Width - vTextMetrics.cx) / 2;
    if (DT_VCENTER and AOptions) > 0 then
      Result.Y := Result.Y + (ATextRect.Height - vTextMetrics.cy) / 2;
    if (DT_RIGHT and AOptions) > 0 then
      Result.X := ATextRect.Right - vTextMetrics.cx;
    if (DT_BOTTOM and AOptions) > 0 then
      Result.Y := ATextRect.Bottom - vTextMetrics.cy;

    // Коррекция по базовой линии
    Result.Y := Result.Y + TSkiaFont(AFont.NativeObject).Metrics.CapHeight +
      TSkiaFont(AFont.NativeObject).Metrics.Descent / 2;
  end;
begin
  // Проанализировать стиль шрифта и опции для вывода
  vTextMetrics := GetTextExtents(AFont, AText);

  if IsZero(AFont.Angle) then
  begin
    vStartPoint := CalcStartPoint(ARect);
    FCanvas.DrawSimpleText(AText, vStartPoint.X, vStartPoint.Y, TSkiaFont(AFont.NativeObject).Font,
      TSkiaFont(AFont.NativeObject).Brush);
  end
  else begin
    vCenterPoint := ARect.CenterPoint;
    vCalcRect := RectF(vCenterPoint.X - ARect.Height / 2, vCenterPoint.Y - ARect.Width / 2,
      vCenterPoint.X + ARect.Height / 2, vCenterPoint.Y + ARect.Width / 2);
    vStartPoint := CalcStartPoint(vCalcRect);

    FCanvas.Save;
    try
      FCanvas.Rotate(AFont.Angle, vCenterPoint.X, vCenterPoint.Y);
      FCanvas.DrawSimpleText(AText, vStartPoint.X, vStartPoint.Y, TSkiaFont(AFont.NativeObject).Font,
        TSkiaFont(AFont.NativeObject).Brush);
    finally
      FCanvas.Restore;
    end;
  end;
end;

procedure TSkiaPainter.DoInvertRect(const ARect: TRectF);
var
  vBrush: ISkPaint;
  vImage: ISkImage;
const
  vMatrix: TSkColorMatrix = (-1, 0, 0, 0, 1,   0, -1, 0, 0, 1,   0, 0, -1, 0, 1,   0, 0, 0, 1, 0);
begin
  vBrush := TSkPaint.Create;
  vBrush.ColorFilter := TSkColorFilter.MakeMatrix(vMatrix);
  vImage := FSurface.MakeImageSnapshot(ARect.Round);
  FCanvas.DrawImage(vImage, ARect.Left, ARect.Top, vBrush);
end;

function TSkiaPainter.GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF;
var
  vMetrics: TSkFontMetrics;
  vTextRect: TRectF;
begin
  vMetrics := TSkiaFont(AFont.NativeObject).Metrics;
  Result.cx := TSkiaFont(AFont.NativeObject).Font.MeasureText(AText, vTextRect);
  Result.cy := vMetrics.Descent + vMetrics.CapHeight; // AFont.Size * 96 / 72;

  //Result.cx := TSkiaFont(AFont.NativeObject).Font.MeasureText(AText, vTextRect);
  //Result.cx := vTextRect.Width;
  //Result.cy := vTextRect.Height;
end;

{ TSkiaNinePatchImage }

function TSkiaNinePatchImage.CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject;
var
  vImage: ISkImage;
begin
  vImage := TSkiaImage(FImage).Image;
  Result := TSkiaImage.Create(vImage.MakeSubset(Rect(ALeft, ATop, ALeft + AWidth, ATop + AHeight)));
end;

function TSkiaNinePatchImage.GetHeight: Integer;
begin
  Result := TSkiaImage(FImage).Image.Height;
end;

function TSkiaNinePatchImage.GetWidth: Integer;
begin
  Result := TSkiaImage(FImage).Image.Width;
end;

{ TSkiaScene }

procedure TSkiaScene.CreateBuffer(const AMemDC: HDC; out ABuffer: HBITMAP; out AData: Pointer; out AStride: Integer);
const
  ColorMasks: array[0..2] of DWORD = ($00FF0000, $0000FF00, $000000FF);
var
  LBitmapInfo: PBitmapInfo;
begin
  AStride := BytesPerScanline(FPanel.Width, 32, 32);
  GetMem(LBitmapInfo, SizeOf(TBitmapInfoHeader) + SizeOf(ColorMasks));
  try
    LBitmapInfo.bmiHeader := Default(TBitmapInfoHeader);
    LBitmapInfo.bmiHeader.biSize        := SizeOf(TBitmapInfoHeader);
    LBitmapInfo.bmiHeader.biWidth       := FPanel.Width;
    LBitmapInfo.bmiHeader.biHeight      := -FPanel.Height;
    LBitmapInfo.bmiHeader.biPlanes      := 1;
    LBitmapInfo.bmiHeader.biBitCount    := 32;
    LBitmapInfo.bmiHeader.biCompression := BI_BITFIELDS;
    LBitmapInfo.bmiHeader.biSizeImage   := AStride * FPanel.Height;
    Move(ColorMasks[0], LBitmapInfo.bmiColors[0], SizeOf(ColorMasks));
    ABuffer := CreateDIBSection(AMemDC, LBitmapInfo^, DIB_RGB_COLORS, AData, 0, 0);
    if ABuffer <> 0 then
      GdiFlush;
  finally
    FreeMem(LBitmapInfo);
  end;
end;

function TSkiaScene.CreatePainter(const AContainer: TObject): TPainter;
begin
  Result := TSkiaPainter.Create(AContainer);
end;

procedure TSkiaScene.DeleteBuffers;
begin
  if FDrawBuffer <> 0 then
  begin
    DeleteObject(FDrawBuffer);
    FDrawBuffer := 0;
  end;

  if FDrawBufferDC <> 0 then
  begin
    DeleteDC(FDrawBufferDC);
    FDrawBufferDC := 0;
  end;
end;

procedure TSkiaScene.DoActivate;
begin
  if not FPanel.Focused then
    FPanel.SetFocus;
end;

function TSkiaScene.DoCreateScene(const APlaceholder: TObject): TPainter;
var
  vControl: TWinControl absolute APlaceholder;
  vContainer: TDrawContainer;
begin
  FDrawBufferDC := 0;

  FPanel := TMyPanel.Create(vControl);
  FPanel.BevelOuter := bvNone;
  FPanel.Align := alClient;
  FPanel.Constraints.MinWidth := 200;
  FPanel.Constraints.MinHeight := 100;
  FPanel.Parent := vControl;

  FPanel.ControlStyle := FPanel.ControlStyle + [csOpaque];
  FPanel.OnMouseWheel := OnMouseWheel;
  FPanel.OnKeyDown := OnKeyDown;
  FPanel.OnKeyUp := OnKeyUp;

  FPanel.OnMouseDown := OnMouseDown;
  FPanel.OnMouseUp := OnMouseUp;
  FPanel.OnDblClick := OnDblClick;
  FPanel.OnMouseMove := OnMouseMove;
  FPanel.OnMouseLeave := OnMouseLeave;

  FPanel.DoubleBuffered := True;
  FPanel.TabStop := True;

  vContainer := TDrawContainer.Create(FPanel.Handle, FPanel.Canvas, FPanel.ClientWidth, FPanel.ClientHeight);
  try
    Result := CreatePainter(vContainer);
  finally
    FreeAndNil(vContainer);
  end;
end;

procedure TSkiaScene.DoDestroyScene;
begin
  FreeAndNil(FContainer);

  FPanel.OnResize := nil;
  FPanel.OnMouseWheel := nil;
  FPanel.OnKeyUp := nil;
  FPanel.OnKeyDown := nil;
  FPanel.OnPaint := nil;
  FPanel.OnMouseDown := nil;
  FPanel.OnMouseUp := nil;
  FPanel.OnDblClick := nil;
  FPanel.OnMouseMove := nil;
  FPanel.OnMouseLeave := nil;

  FPanel := nil;

  DeleteBuffers;
end;

procedure TSkiaScene.DoRender(const ANeedFullRepaint: Boolean);
var
  vOldObj: HGDIOBJ;
  vDrawBufferData: Pointer;
  vDrawBufferStride: Integer;
begin
  if FDrawBufferDC = 0 then
  begin
    FDrawBufferDC := CreateCompatibleDC(0);
    if FDrawBufferDC = 0 then
      Exit;
  end;

  if FDrawBuffer = 0 then
  begin
    CreateBuffer(FDrawBufferDC, FDrawBuffer, vDrawBufferData, vDrawBufferStride);
    FSurface := TSkSurface.MakeRasterDirect(TSkImageInfo.Create(FPanel.Width, FPanel.Height), vDrawBufferData, vDrawBufferStride);
    TSkiaPainter(FPainter).FSurface := FSurface;
    TSkiaPainter(FPainter).FCanvas := FSurface.Canvas;
  end;

  if ANeedFullRepaint or not Assigned(FStaticImage) then
  begin
    FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
    //FStaticImage := FSurface.MakeImageSnapshot;
  end;
  //else
    //FSurface.Canvas.DrawImage(FStaticImage, 0, 0);


  //FSurface.Canvas.Save;
  vOldObj := SelectObject(FDrawBufferDC, FDrawBuffer);
  try
    FRoot.RenderDynamic(FPainter, GetSceneRect);
    BitBlt(FPanel.Canvas.Handle, 0, 0, FPanel.Width, FPanel.Height, FDrawBufferDC, 0, 0, SRCCOPY)
  finally
    if vOldObj <> 0 then
      SelectObject(FDrawBufferDC, vOldObj);
    //FSurface.Canvas.Restore;
  end;
end;

function TSkiaScene.GetClientPos: TPointF;
var
  vClientPos: TPoint;
begin
  GetCursorPos(vClientPos);
  Result := FPanel.ScreenToClient(vClientPos);
end;

function TSkiaScene.GetSceneRect: TRectF;
begin
  Result := FPanel.ClientRect;
end;

procedure TSkiaScene.SaveToFile(const AFileName, AWaterMark: string);
begin
  DoRender(True);

  TFile.WriteAllBytes(AFileName, FStaticImage.EncodeToBytes(TSkEncodedImageFormat.PNG));
  //TFile.WriteAllBytes('output.webp', GetImage.EncodeToBytes(TSkEncodedImageFormat.WEBP, 80));
  //TFile.WriteAllBytes('output.jpg', GetImage.EncodeToBytes(TSkEncodedImageFormat.JPEG, 80));
end;

procedure TSkiaScene.SetEnabled(const AValue: Boolean);
begin
  inherited SetEnabled(AValue);

  if AValue then
  begin
    FPanel.OnResize := OnResize;
    FPanel.OnPaint := OnPaint;
  end
  else begin
    FPanel.OnResize := nil;
    FPanel.OnPaint := nil;
  end;
end;

procedure TSkiaScene.UpdateContexts(const AWidth, AHeight: Single);
begin
  DeleteBuffers;
end;

{ TSkiaBrush }

constructor TSkiaBrush.Create(const ABrush: ISkPaint);
begin
  inherited Create;
  FBrush := ABrush;
end;

destructor TSkiaBrush.Destroy;
begin
  FBrush := nil;
  inherited Destroy;
end;

{ TSkiaPen }

constructor TSkiaPen.Create(const APen: ISkPaint);
begin
  inherited Create;
  FPen := APen;
end;

destructor TSkiaPen.Destroy;
begin
  FPen := nil;
  inherited Destroy;
end;

{ TSkiaImage }

constructor TSkiaImage.Create(const AImage: ISkImage);
begin
  inherited Create;
  FImage := AImage;
end;

destructor TSkiaImage.Destroy;
begin
  FImage := nil;
  inherited Destroy;
end;

{ TSkiaFont }

constructor TSkiaFont.Create(const AFont: ISkFont; const ABrush: ISkPaint);
begin
  inherited Create;
  FFont := AFont;
  FBrush := ABrush;
  FFont.GetMetrics(FMetrics);
end;

destructor TSkiaFont.Destroy;
begin
  FFont := nil;
  FBrush := nil;
  inherited Destroy;
end;

{ TSkiaPath }

constructor TSkiaPath.Create;
begin
  inherited Create;
  FPath := TSkPath.Create;
end;

destructor TSkiaPath.Destroy;
begin
  FPath := nil;
  inherited Destroy;
end;

initialization

TBaseModule.RegisterModule('Painting', 'Skia', TSkiaScene);

end.

