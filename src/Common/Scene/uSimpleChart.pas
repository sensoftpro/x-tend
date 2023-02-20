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

unit uSimpleChart;

interface

uses
  Classes, Types, UITypes, UIConsts, Generics.Collections,
  uScene, uChartUtils, uDrawStyles, uTensor;

type
  TLightArea = record
    Start: Integer;
    Finish: Integer;
  end;

  TSimpleChart = class(TSceneObject)
  protected
    FChartRect: TRectF;
    FChartSelectionArea: TRectF;
    FSelectionRect: TRectF;
    FRealSelectionRect: TRectF;
    FScrollRect: TRectF;
    FThumbRect: TRectF;
    FScaleRect: TRectF; // Прямоугольник шкалы оси Y
    FLightAreas: TList<TLightArea>;

    FXMetrics: TAxisMetrics;
    FExtraMetrics: TAxisMetrics;
    FYMetrics: TAxisMetrics;

    FInitialViewportStart: Integer;
    FViewportStart: Integer;
    FViewportWidth: Integer;

    FMousePos: TPointF;
    FClickPos: TPointF;
    FClickedXValue: Double;
    FMoveMode: TMoveMode;
    FChartMode: TChartMode;

    procedure SetViewportStart(const Value: Double);
    procedure SetViewportWidth(const Value: Double);
    function XValueAtPos(const X: Single; const ASerieNo: Integer = 0): Double;
  private
    function DrawChart(const APainter: TPainter; const ARect: TRectF;
      const AShowXCaption, AShowXScale, AShowYCaption, AShowYScale, AUseViewport: Boolean): TRectF;
    procedure DrawScrollArea(const APainter: TPainter);
    procedure DrawChannelInfo(const APainter: TPainter; const ARect: TRectF; const APoint: TPointF);
  protected
    // Data
    function MinDimension: Integer; virtual; abstract;
    function MaxDimension: Integer; virtual; abstract;
    function MinValue: Double; virtual; abstract;
    function MaxValue: Double; virtual; abstract;
    function GetValue(const AIndex: Integer): Double; virtual; abstract;
    function GetExtraValue(const AIndex: Integer): Double; virtual; abstract;
    function GetXCaption: string; virtual;
    function GetYCaption: string; virtual;
    function GetExtraScalesCount: Integer; virtual;
    procedure DrawXLegend(const APainter: TPainter; const ARect: TRectF; const APoint: TPointF); virtual;
    procedure DrawSelectionObjects(const APainter: TPainter; const ARect: TRectF); virtual;
    procedure DrawStaticObjects(const APainter: TPainter; const ARect: TRectF); virtual;
    procedure DrawValues(const APainter: TPainter; const ARect: TRectF;
      const AViewportStart, AViewportWidth: Integer); virtual;

    procedure DoExecuteUIAction(const AArea, AView: TObject); virtual;
  protected
    procedure Deactivate(const AHovering: TSceneObject); override;
    procedure DoRenderStatic(const APainter: TPainter; const ARect: TRectF;
      const AMode: TScenePaintMode = spmNormal); override;
    procedure DoRenderDynamic(const APainter: TPainter; const ARect: TRectF); override;
    procedure DoRectChanged(const AOldRect, ANewRect: TRectF); override;
    procedure HandleMouseDown(Button: TMouseButton; Shift: TShiftState;
      ClientPos: TPointF; var AHandled: Boolean); override;
    procedure HandleMouseUp(Button: TMouseButton; Shift: TShiftState;
      ClientPos: TPointF; var AHandled: Boolean); override;
    procedure HandleDblClick(ClientPos: TPointF; var AHandled: Boolean); override;
    procedure HandleMouseMove(Shift: TShiftState; ClientPos: TPointF;
      var AHandled: Boolean); override;
    procedure HandleKeyDown(var Key: Word; Shift: TShiftState;
      var AHandled: Boolean); override;
    procedure HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      ClientPos: TPointF; var AHandled: Boolean); override;
  public
    constructor Create(const AScene: TScene; const AParent: TSceneObject);
    destructor Destroy; override;

    procedure AddLightArea(const AStart, AFinish: Integer);
    procedure RemoveLightAreas;

    procedure ResetZooming;
    function IsScaled(const AViewportStart, AViewportWidth: Integer): Boolean;

    function IsSelectionVisible: Boolean;
    procedure Select(const ALeftX, ARightX: Double);
    procedure ResetSelection;
  end;

  TChartClass = class of TSimpleChart;

  TDataChart = class(TSimpleChart)
  private
    FTensor: TTensor;
    FMin, FMax: Double;
  protected
    function MinDimension: Integer; override;
    function MaxDimension: Integer; override;
    function MinValue: Double; override;
    function MaxValue: Double; override;
    function GetValue(const AIndex: Integer): Double; override;
    function GetExtraValue(const AIndex: Integer): Double; override;
  public
    constructor Create(const AScene: TScene; const AParent: TSceneObject);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, Math, uConsts;

{ TSimpleChart }

procedure TSimpleChart.AddLightArea(const AStart, AFinish: Integer);
var
  vNewArea: TLightArea;
begin
  vNewArea.Start := AStart;
  vNewArea.Finish := AFinish;
  FLightAreas.Add(vNewArea);
  Invalidate;
end;

constructor TSimpleChart.Create(const AScene: TScene; const AParent: TSceneObject);
var
  vRect: TRectF;
begin
  if Assigned(AParent) then
    vRect := AParent.ClientRect
  else
    vRect := AScene.ClientRect;

  inherited Create(AScene, AParent, vRect);

  Anchors := [soaLeft, soaTop, soaRight, soaBottom];

  FMousePos := PointF(-1, -1);
  FLightAreas := TList<TLightArea>.Create;
  FChartMode := cmLinear;

  ResetZooming;

  FClickPos := PointF(-1, -1);
  FMoveMode := mmSimple;
  FScaleRect := Rect(0, 0, 0, 0);
  ResetSelection;

  // Заполнение стилей
  if CreateStyle('chart') then
  begin
    FStyle.AddFontParams('watermark', 'Tahoma', 10, TAlphaColorRec.Blue);
    FStyle.AddFillParams('background', TAlphaColorRec.Silver, AppendColor(TAlphaColorRec.Silver, $001E1E1E), gkVertical); // background
    FStyle.AddStrokeParams('grid', MakeColor(TAlphaColorRec.Gray, 1{0.3}), 1, psDot); // grid
    FStyle.AddStrokeParams('axis', TAlphaColorRec.Gray, 2); // axis
    FStyle.AddFontParams('title', 'Tahoma', 12, TAlphaColorRec.Black); // Заголовок чарта
    FStyle.AddFontParams('x.tick', 'Tahoma', 10, TAlphaColorRec.Black); // значения по шкале X
    FStyle.AddFontParams('extra.tick', 'Tahoma', 8, TAlphaColorRec.Navy); // доп. значения по шкале X
    FStyle.AddFontParams('y.tick', 'Tahoma', 10, TAlphaColorRec.Black); // значения по шкале Y
    FStyle.AddFontParams('power', 'Tahoma', 7, TAlphaColorRec.Black); // степень по шкале Y
    FStyle.AddStrokeParams('line.values', TAlphaColorRec.Red, 1); // values
    FStyle.AddFillParams('hist.values', TAlphaColorRec.Red and $BFFFFFFF); // values
    FStyle.AddFillParams('spot.values', TAlphaColorRec.Red); // values
    FStyle.AddFillParams('back.values', TAlphaColorRec.Red and $7FFFFFFF); // стиль фонового спектра
    FStyle.AddFontParams('x.title', 'Tahoma', 10, TAlphaColorRec.Black); // заголовок оси X
    FStyle.AddFontParams('y.title', 'Tahoma', 10, TAlphaColorRec.Black, FontStyleStrikeout or FontStyleUnderline, 270, rqHigh); // заголовок оси Y
    FStyle.AddStrokeParams('tick.line', TAlphaColorRec.Gray, 1); // линия-указатель текущего значения X
    FStyle.AddFillParams('tick.selected', MakeColor(TAlphaColorRec.Black, 0.5)); // область вывода текущего значения X
    FStyle.AddFillParams('extra.tick.selected', MakeColor(TAlphaColorRec.Navy, 0.5)); // область вывода доп. значения X
    FStyle.AddFontParams('tick.selected', 'Tahoma', 10, TAlphaColorRec.White); // текст для вывода текущих значений X
    FStyle.AddFontParams('extra.tick.selected', 'Tahoma', 8, TAlphaColorRec.White); // текст для вывода доп. значений X
    FStyle.AddFillParams('scroll.fill', MakeColor(TAlphaColorRec.Black, 40 / 256));
    FStyle.AddStrokeParams('scroll.stroke', TAlphaColorRec.Gray, 1);
    FStyle.AddFillParams('lightarea', $FFB8CBB4, AppendColor(TAlphaColorRec.Silver, $00191919), gkVertical);
    FStyle.AddFontParams('info.text', 'Tahoma', 8, $FF303030);
    FStyle.AddStrokeParams('peak', TAlphaColorRec.Skyblue);
    FStyle.AddFontParams('peak', 'Tahoma', 8, $FF303030);
    FStyle.AddImageParams('peak', 'panel_peak.png', False).CutRect := Rect(7, 27, 8, 28);
    FStyle.AddStrokeParams('gaussian', $FF00A0FF, 2);
    FStyle.AddFillParams('geometry', MakeColor(TAlphaColorRec.Red, 20/255));
    FStyle.AddFontParams('geometry', 'Tahoma', 7, TAlphaColorRec.Maroon);
    FStyle.AddStrokeParams('nuclide', TAlphaColorRec.Deeppink, 1);
    FStyle.AddFillParams('nuclide', MakeColor(TAlphaColorRec.Deeppink, 0.5));
    FStyle.AddFillParams('_nuclide', TAlphaColorRec.Deeppink);
    FStyle.AddFontParams('nuclide', 'Tahoma', 8, TAlphaColorRec.Deeppink);
    FStyle.AddFontParams('yield', 'Tahoma', 7, TAlphaColorRec.Deeppink);

    FStyle.CreateDrawObjects(AScene.Painter);
  end;
end;

procedure TSimpleChart.Deactivate(const AHovering: TSceneObject);
begin
  inherited Deactivate(AHovering);
  FMousePos := PointF(-1, -1);
  Invalidate(ssUsed);
end;

destructor TSimpleChart.Destroy;
begin
  FreeAndNil(FLightAreas);

  // Сначала удаляем все подписки на события
  inherited Destroy;
end;

procedure TSimpleChart.DoExecuteUIAction(const AArea, AView: TObject);
begin
end;

procedure TSimpleChart.DoRectChanged(const AOldRect, ANewRect: TRectF);
begin
  inherited DoRectChanged(AOldRect, ANewRect);
  ResetSelection;
end;

procedure TSimpleChart.DoRenderStatic(const APainter: TPainter; const ARect: TRectF;
  const AMode: TScenePaintMode = spmNormal);
var
  vRect: TRectF;
  vTextHeight: Single;
  vRatio: TPointF;
begin
  vRect := ARect;

  APainter.DrawRect(FStyle, 'background', '', vRect);
  APainter.DrawBezier(FStyle, 'peak', PPointF(TArray<TPointF>.Create(PointF(50,0), PointF(100,20),
    PointF(120,50),  PointF(160, 100), PointF(250, 50), PointF(160, 100), PointF(223, 32),
    PointF(245, 132), PointF(609, 100), PointF(50, 50))), 10, TAlphaColorRec.Green);

  vTextHeight := 0.8 * APainter.TextHeight(FStyle, 'x.title', 'fg');
  InflateRect(vRect, -vTextHeight, -vTextHeight);

  FChartRect := DrawChart(APainter, vRect, False, True, True, True, True);
  vRatio := PointF(FXMetrics.Ratio, FYMetrics.Ratio);

  FChartSelectionArea := RectF(FChartRect.Left, vRect.Top, FChartRect.Right, vRect.Bottom);
  if IsScaled(FViewportStart, FViewportWidth) then
  begin
    FChartSelectionArea.Bottom := FScrollRect.Top - 3;
    DrawScrollArea(APainter);
  end
  else
    FThumbRect := TRectF.Empty;

  FXMetrics.Ratio := vRatio.X;
  FYMetrics.Ratio := vRatio.Y;

  // Отрисовываем дочерние объекты
  inherited DoRenderStatic(APainter, ARect, AMode);

  APainter.DrawImage(FStyle, 'peak', RectF(200, 100, 450, 300), 0.7);
  APainter.DrawEllipse(FStyle, '_nuclide', 'peak', RectF(500, 100, 650, 200));
  APainter.DrawPie(FStyle, '_nuclide', 'peak', RectF(100, 350, 350, 420), 360, 320);
  APainter.DrawRegion(FStyle, '_nuclide', 'line.values', PPointF(TArray<TPointF>.Create(
    PointF(0, 0), PointF(80, 20), PointF(50, 50), PointF(80, 80),
    PointF(50, 100), PointF(40, 20), PointF(0, 0))), 7, TAlphaColorRec.Navy);
end;

procedure TSimpleChart.DoRenderDynamic(const APainter: TPainter; const ARect: TRectF);
var
  vPaintRect: TRectF;
begin
  // Отрисовываем дочерние объекты
  inherited DoRenderDynamic(APainter, ARect);

  FRealSelectionRect := Rect(0, 0, 0, 0);
  if IsSelectionVisible then
  begin
    vPaintRect := FSelectionRect;
    vPaintRect.Left := Max(vPaintRect.Left, FViewportStart);
    vPaintRect.Right := Min(vPaintRect.Right, FViewportStart + FViewportWidth - 1);
    vPaintRect.Left := FChartRect.Left + (vPaintRect.Left - FViewportStart) / FXMetrics.Ratio;
    vPaintRect.Right := FChartRect.Left + (vPaintRect.Right - FViewportStart) / FXMetrics.Ratio;
    FRealSelectionRect := vPaintRect;

    APainter.InvertRect(RectF(vPaintRect.Left, FChartRect.Top, vPaintRect.Right, FChartRect.Bottom));

    DrawSelectionObjects(APainter, vPaintRect);

    DrawXLegend(APainter, FChartRect, vPaintRect.TopLeft);
    DrawXLegend(APainter, FChartRect, vPaintRect.BottomRight);
  end;

  // Выводим указатель текущего канала
  if not SameValue(FMousePos.X, -1) and not SameValue(FMousePos.Y, -1) {and (FMoveMode <> mmSelect)} then
  begin
    APainter.DrawLine(FStyle, 'tick.line',
      PointF(FMousePos.X, FChartRect.Top), PointF(FMousePos.X, FChartRect.Bottom), True);
    DrawChannelInfo(APainter, FChartRect, FMousePos);
  end;
end;

procedure TSimpleChart.DrawChannelInfo(const APainter: TPainter; const ARect: TRectF; const APoint: TPointF);
var
  vInfo: string;
  vRect: TRectF;
  vTextSize: TSizeF;
  vXValue: Double;
begin
  vXValue := XValueAtPos(APoint.X);
  if vXValue < 0 then
    Exit;

  DrawXLegend(APainter, ARect, APoint);

  vInfo := FormatFloat('0.##', GetValue(Round(vXValue)));
  vTextSize := APainter.TextExtents(FStyle, 'info.text', vInfo);

  // Формирование прямоугольника для вывода текста
  vRect := ARect;
  vRect.Left := APoint.X + 1;
  vRect.Bottom := APoint.Y - 1;
  vRect.Top := vRect.Bottom - 1.5 * vTextSize.cy;
  if vRect.Top < ARect.Top then
    OffsetRect(vRect, 0, ARect.Top - vRect.Top);
  vRect.Right := vRect.Left + vTextSize.cx + vTextSize.cy;
  if vRect.Right > ARect.Right then
  begin
    vRect.Right := APoint.X - 1;
    vRect.Left := Max(ARect.Left, vRect.Right - vTextSize.cx - vTextSize.cy);
  end;

  OffsetRect(vRect, vTextSize.cy / 4, 0);

  APainter.DrawText(FStyle, 'info.text', vInfo, vRect, DT_LEFT);
end;

function TSimpleChart.DrawChart(const APainter: TPainter; const ARect: TRectF;
  const AShowXCaption, AShowXScale, AShowYCaption, AShowYScale, AUseViewPort: Boolean): TRectF;
var
  vViewportStart: Integer;
  vViewportWidth: Integer;
  vMinDimension: Integer;
  vMaxDimension: Integer;
  vScrollHeight: Single;
  vExtraHeight: Single;
  vShowXScale: Boolean;
  vShowXCaption: Boolean;
  vTextHeight: Single;
  vExtraTextHeight: Single;
  vTextWidth: Single;
  vTextRect: TRectF;
  vLightArea: TLightArea;
  vYAxisName: string;
  vOptions: TChartPaintOptions;
begin
  Result := ARect;

  if AUseViewPort then
  begin
    vViewportStart := FViewportStart;
    vViewportWidth := FViewportWidth;
  end
  else begin
    vViewportStart := MinDimension;
    vViewportWidth := MaxDimension + 1;
  end;
  vMinDimension := vViewportStart;
  vMaxDimension := vViewportStart + vViewportWidth - 1;

  vTextHeight := APainter.TextHeight(FStyle, 'x.title', 'fg');
  vExtraTextHeight := APainter.TextHeight(FStyle, 'extra.tick', 'fg');

  // X CAPTION
  vShowXCaption := AShowXCaption and (Result.Height >= 60);
  if vShowXCaption then
    Result.Bottom := Result.Bottom - vTextHeight * 1.3;

  // X SCROLL
  if IsScaled(vViewportStart, vViewportWidth) then
  begin
    vScrollHeight := Min(cScrollAreaHeight - cScaleSpace, (Result.Height - cScaleSpace) / 2);
    Result.Bottom := Result.Bottom - vScrollHeight - cScaleSpace;
    FScrollRect.Top := Result.Bottom + cScaleSpace;
    FScrollRect.Bottom := FScrollRect.Top + vScrollHeight;
  end
  else begin
    FScrollRect.Top := Result.Bottom;
    FScrollRect.Bottom := FScrollRect.Top;
  end;

  // X SCALES
  vExtraHeight := vExtraTextHeight * GetExtraScalesCount;
  vShowXScale := AShowXScale and (Result.Bottom - Result.Top - (vTextHeight * 3 / 2) - cScaleSpace - vExtraHeight >= 8);
  if vShowXScale then
  begin
    Result.Top := Result.Top + vTextHeight / 2;
    Result.Bottom := Result.Bottom - vTextHeight - cScaleSpace - vExtraHeight;
  end
  else if Result.Bottom <= Result.Top then
  begin
    Result.Bottom := Result.Top;
    Exit;
  end;

  // Y CAPTION
  if AShowYCaption then
  begin
    vYAxisName := GetYCaption;
    Result.Left := Result.Left + vTextHeight * 1.3;
  end;

  // Y SCALE
  vOptions := [cpoHighlightZero, cpoAlwaysIncludeZero, cpoIntOnly];
  if not AShowYScale then
    vOptions := vOptions + [cpoUtilizeAllSpace];
  if FChartMode = cmLog then
    vOptions := vOptions + [cpoLogarithmic];

  FYMetrics := CalcAxisMetrics(MinValue, MaxValue, Result.Height, vTextHeight * 2,
    aoVertical, 0, vOptions);

  // Y SCALE RECTANGLES
  if AShowYScale then
  begin
    vTextWidth := CalcAxisTextWidth(FYMetrics, APainter, FStyle, 'y.tick');

    FYMetrics.Rect := Result;
    FYMetrics.Rect.Right := Result.Left + vTextWidth;
    FScaleRect := FYMetrics.Rect;
    // обрезаем область для чарта слева
    Result.Left := Result.Left + vTextWidth + cScaleSpace;
  end;

  FScrollRect.Left := Result.Left;
  FScrollRect.Right := Result.Right;

  // HIGHLIGHT
  for vLightArea in FLightAreas do
    if (vLightArea.Start < vViewportStart + vViewportWidth - 1) and (vLightArea.Finish > vViewportStart) then
    begin
      APainter.DrawRect(FStyle, 'lightarea', '',
        RectF(Result.Left + (Max(vLightArea.Start, vViewportStart) - vViewportStart) / FXMetrics.Ratio,
        Result.Top, Result.Left + (Min(vLightArea.Finish, vViewportStart + vViewportWidth - 1) - vViewportStart) /
        FXMetrics.Ratio + 1, Result.Bottom));
    end;

  // X SCALES
  vTextWidth := APainter.TextWidth(FStyle, 'x.tick', IntToStr(vMaxDimension));
  FXMetrics := CalcAxisMetrics(vMinDimension, vMaxDimension, Result.Width,
    vTextWidth * 1.3, aoHorizontal, 0, [cpoHighlightZero, cpoUtilizeAllSpace, cpoIntOnly]);
  FXMetrics.Rect := Result;
  FXMetrics.Rect.Top := Result.Bottom + cScaleSpace;
  FXMetrics.Rect.Bottom := FXMetrics.Rect.Top + vTextHeight;

  vTextWidth := APainter.TextWidth(FStyle, 'extra.tick',
    FormatFloat('0', GetExtraValue(vViewportStart + vViewportWidth - 1))) * 1.5;
  FExtraMetrics := CalcAxisMetrics(GetExtraValue(vViewportStart),
    GetExtraValue(vViewportStart + vViewportWidth - 1), Result.Width, vTextWidth,
    aoHorizontal, 0, [cpoUtilizeAllSpace, cpoHideBoundAxis]);
  FExtraMetrics.Rect := FXMetrics.Rect;
  FExtraMetrics.Rect.Top := FExtraMetrics.Rect.Bottom;
  FExtraMetrics.Rect.Bottom := FExtraMetrics.Rect.Top + vTextHeight;

  if AUseViewport then
    DrawStaticObjects(APainter, Result);

  // Выводим области подписей по осям
  if AShowYScale then
    DrawAxisText(FYMetrics, APainter, FStyle, 'y.tick', Result, False);

  if vShowXScale then
  begin
    DrawAxisText(FXMetrics, APainter, FStyle, 'x.tick', Result);
    if vExtraHeight > 0 then
      DrawAxisText(FExtraMetrics, APainter, FStyle, 'extra.tick', Result, False);
  end;

  // Выводим подпись по оси X
  if vShowXCaption then
  begin
    vTextRect := Result;
    vTextRect.Bottom := ARect.Bottom;
    vTextRect.Top := vTextRect.Bottom - vTextHeight;
    APainter.DrawText(FStyle, 'x.title', GetXCaption, vTextRect, DT_CENTER);
  end;

  // Выводим подпись по оси Y
  if AShowYCaption and (vYAxisName <> '') then
  begin
    vTextRect := Result;
    vTextRect.Left := ARect.Left;
    vTextRect.Right := vTextRect.Left + vTextHeight;
    vTextWidth := APainter.TextWidth(FStyle, 'y.title', vYAxisName);
    vTextRect.Top := vTextRect.Top + (vTextRect.Bottom - vTextRect.Top - vTextWidth) / 2;
    vTextRect.Bottom := vTextRect.Top + vTextWidth;

    APainter.DrawText(FStyle, 'y.title', vYAxisName, vTextRect, 0, 270);
  end;

  DrawValues(APainter, Result, vViewportStart, vViewportWidth);
end;

procedure TSimpleChart.DrawScrollArea(const APainter: TPainter);
var
  vScale: Double;
  vRect: TRectF;
  vPaintRect: TRectF;
  vX: Single;
begin
  vRect := FScrollRect;
  DrawChart(APainter, FScrollRect, False, False, False, False, False);
  FScrollRect := vRect;

  InflateRect(vRect, 0, 4);
  vScale := FScrollRect.Width / MaxDimension;

  APainter.DrawPolyline(FStyle, 'scroll.stroke',
    [PointF(vRect.Left, vRect.Top), PointF(vRect.Left, vRect.Bottom),
    PointF(vRect.Right, vRect.Bottom), PointF(vRect.Right, vRect.Top)]);

  FThumbRect := vRect;

  if FViewportStart > 0 then
  begin
    vPaintRect := vRect;
    vX := FScrollRect.Left + FViewportStart * vScale;
    vPaintRect.Right := vX;
    FThumbRect.Left := vX;
    APainter.DrawRect(FStyle, 'scroll.fill', '', vPaintRect);
    APainter.DrawPolyline(FStyle, 'scroll.stroke',
      [PointF(vRect.Left, vRect.Top), PointF(vX, vRect.Top),
      PointF(vX, vRect.Bottom)]);
  end;

  if FViewportStart + FViewportWidth - 1 < MaxDimension then
  begin
    vPaintRect := vRect;
    vX := FScrollRect.Left + (FViewportStart + FViewportWidth - 1) * vScale;
    vPaintRect.Left := vX;
    FThumbRect.Right := vX;
    APainter.DrawRect(FStyle, 'scroll.fill', '', vPaintRect);
    APainter.DrawPolyline(FStyle, 'scroll.stroke',
      [PointF(vRect.Right, vRect.Top), PointF(vX, vRect.Top),
      PointF(vX, vRect.Bottom)]);
  end;
end;

procedure TSimpleChart.DrawSelectionObjects(const APainter: TPainter; const ARect: TRectF);
begin
end;

procedure TSimpleChart.DrawStaticObjects(const APainter: TPainter; const ARect: TRectF);
begin
end;

procedure TSimpleChart.DrawValues(const APainter: TPainter; const ARect: TRectF; const AViewportStart,
  AViewportWidth: Integer);
var
  vPoints: array of TPointF;
  i: Integer;
begin
  // Выводим сами данные
  SetLength(vPoints, AViewportWidth);
  try
    for i := 0 to AViewportWidth - 1 do
      vPoints[i] := PointF(ARect.Left + i / FXMetrics.Ratio,
        ARect.Bottom - (GetValue(AViewportStart + i) - FYMetrics.StartValue) / FYMetrics.Ratio);
    APainter.DrawPolyline(FStyle, 'line.values', PPointF(vPoints), Length(vPoints));
  finally
    Finalize(vPoints);
  end;
end;

procedure TSimpleChart.DrawXLegend(const APainter: TPainter; const ARect: TRectF; const APoint: TPointF);
var
  vXValue: Double;
  vValue: string;
  vMaxWidth: Single;
  vRect: TRectF;
begin
  vXValue := XValueAtPos(APoint.X);
  if vXValue < 0 then
    Exit;

  vValue := IntToStr(Round(vXValue));
  vMaxWidth := APainter.TextWidth(FStyle, 'tick.selected', vValue) + 6;

  vRect := ARect;
  vRect.Top := ARect.Bottom + cScaleSpace;
  vRect.Bottom := vRect.Top + APainter.TextHeight(FStyle, 'tick.selected', 'fg');
  vRect.Left := APoint.X - vMaxWidth / 2;
  vRect.Right := vRect.Left + vMaxWidth;

  APainter.DrawRect(FStyle, 'tick.selected', '', vRect);
  APainter.DrawText(FStyle, 'tick.selected', vValue, vRect, DT_CENTER or DT_VCENTER);
end;

function TSimpleChart.GetExtraScalesCount: Integer;
begin
  Result := 0;
end;

function TSimpleChart.GetXCaption: string;
begin
  Result := 'Номер измерения';
end;

function TSimpleChart.GetYCaption: string;
begin
  Result := 'Значения по Y';
end;

procedure TSimpleChart.HandleDblClick(ClientPos: TPointF; var AHandled: Boolean);
begin
  ResetZooming;
  Invalidate;
end;

procedure TSimpleChart.HandleKeyDown(var Key: Word; Shift: TShiftState; var AHandled: Boolean);
var
  vIncrement: Integer;
begin
  if Shift = [] then
  begin
    if Key = vkLeft then
    begin
      SetViewportStart(FViewportStart - FViewportWidth / 5);
      Invalidate;
    end
    else if Key = vkRight then
    begin
      SetViewportStart(FViewportStart + FViewportWidth / 5);
      Invalidate;
    end;
  end
  // ctrl + стрелки влево-вправо двигают текущее выделение на 5 каналов;
  // shift + стрелки увеличивают выделение на 5 каналов справа;
  // зажатый альт + действия выше = тоже самое только с одним каналом;
  // если выделения не было, то стрелки с шифтом начнут его с крайнего левого канала в видимом окне (т.е. учитываем увеличение).
  else
  begin
    if ssAlt in Shift then
      vIncrement := 1
    else
      vIncrement := 5;
    if Key = vkLeft then
      vIncrement := -vIncrement
    else if Key <> vkRight then
      Exit;

    if (ssCtrl in Shift) and IsSelectionVisible then
    begin
      FSelectionRect.Left := FSelectionRect.Left + vIncrement;
      if FSelectionRect.Left < FViewportStart then
        FSelectionRect.Left := MinDimension;
      FSelectionRect.Left := Min(FSelectionRect.Left, FViewportStart + FViewportWidth - 1);

      FSelectionRect.Right := FSelectionRect.Right + vIncrement;
      if FSelectionRect.Right < FViewportStart then
        FSelectionRect.Right := MinDimension;
      FSelectionRect.Right := Min(FSelectionRect.Right, FViewportStart + FViewportWidth - 1);
      Invalidate;
    end
    else if ssShift in Shift then
    begin
      if IsSelectionVisible then
      begin
        FSelectionRect.Right := Max(FSelectionRect.Right + vIncrement, FSelectionRect.Left);
        FSelectionRect.Right := Min(FSelectionRect.Right, FViewportStart + FViewportWidth - 1);
        Invalidate;
      end
      else
      begin
        if vIncrement <= 0 then
          Exit;
        FSelectionRect := FChartRect;
        FSelectionRect.Left := FViewportStart;
        FSelectionRect.Right := Min(FSelectionRect.Left + vIncrement, FViewportStart + FViewportWidth - 1);
        Invalidate;
      end;
    end;
  end;
end;

procedure TSimpleChart.HandleMouseDown(Button: TMouseButton; Shift: TShiftState; ClientPos: TPointF;
  var AHandled: Boolean);
var
  X: Single;
  vWidth: Single;
begin
  if PtInRect(FChartSelectionArea, ClientPos) then
    FMousePos := ClientPos
  else
    FMousePos := Point(-1, -1);

  if Button <> TMouseButton.mbLeft then
    Exit;

  // Щелкнули в область скролла
  X := ClientPos.X;
  if PtInRect(FScrollRect, ClientPos) then
  begin
    ResetSelection;
    if PtInRect(FThumbRect, ClientPos) then
    begin
      FClickPos := ClientPos;
      FInitialViewportStart := FViewportStart;
      FMoveMode := mmScroll;
    end
    else
    begin
      SetViewportStart(FViewportStart +
        (X - (FThumbRect.Left + FThumbRect.Width / 2)) * MaxDimension / FScrollRect.Width);
      Invalidate;
    end;

    Exit;
  end;

  if (ClientPos.Y >= FChartSelectionArea.Top) and (ClientPos.Y <= FChartSelectionArea.Bottom) then
  begin
    if (ssCtrl in Shift) and (X >= FChartRect.Left) and (X <= FChartRect.Right) then
    begin
      ResetSelection;
      FMoveMode := mmSimple;
      FClickedXValue := XValueAtPos(X);

      // Ищем пик и показываем его
      vWidth := Min(30, FViewportWidth / 8);
      FSelectionRect := RectF(Max(FViewportStart, FClickedXValue - vWidth), FChartRect.Top,
        Min(FViewportStart + FViewportWidth, FClickedXValue + vWidth), FChartRect.Bottom);
    end
    else begin
      if X < FChartRect.Left then
        X := FChartRect.Left
      else if X > FChartRect.Right then
        X := FChartRect.Right;
      FClickedXValue := XValueAtPos(X);

      FMoveMode := mmPreSelect;
      FClickPos := ClientPos;
    end;
  end;

  Invalidate;
end;

procedure TSimpleChart.HandleMouseMove(Shift: TShiftState; ClientPos: TPointF; var AHandled: Boolean);
var
  vSelectionRect: TRectF;
  vMousePos: TPointF;
  vXValue: Double;
  vIncrement: Integer;
  X: Single;
begin
  if PtInRect(FChartSelectionArea, ClientPos) then
    vMousePos := ClientPos
  else
    vMousePos := Point(-1, -1);

  X := ClientPos.X;
  if ssLeft in Shift then
  begin
    if (FMoveMode = mmPreSelect) and (vMousePos.X >= 0) and (Abs(FClickPos.X - vMousePos.X) >= 4) then
      FMoveMode := mmSelect;

    if FMoveMode = mmSelect then
    begin
      // Определяем размеры нового прямоугольника
      if X < FChartRect.Left then
        X := FChartRect.Left
      else if X > FChartRect.Right then
        X := FChartRect.Right;
      vXValue := XValueAtPos(X);

      if vXValue < FClickedXValue then
        vSelectionRect := RectF(vXValue, FChartRect.Top, FClickedXValue, FChartRect.Bottom)
      else
        vSelectionRect := RectF(FClickedXValue, FChartRect.Top, vXValue, FChartRect.Bottom);

      if EqualRect(FSelectionRect, vSelectionRect) and (FMousePos = vMousePos) then
        Exit;

      FSelectionRect := vSelectionRect;
      FMousePos := vMousePos;
      Invalidate(ssUsed);
    end
    else if FMoveMode = mmScroll then
    begin
      vIncrement := Round((X - FClickPos.X) * MaxDimension /
        (FScrollRect.Right - FScrollRect.Left));
      if vIncrement <> 0 then
      begin
        SetViewportStart(FInitialViewportStart + vIncrement);
        Invalidate;
      end;
    end;
  end
  else
  begin
    if FMousePos = vMousePos then
      Exit;

    FMousePos := vMousePos;
    // Нарисовать на имеющемся рисунке
    Invalidate(ssUsed);
  end;
end;

procedure TSimpleChart.HandleMouseUp(Button: TMouseButton; Shift: TShiftState; ClientPos: TPointF;
  var AHandled: Boolean);
begin
  if PtInRect(FChartSelectionArea, ClientPos) then
    FMousePos := ClientPos
  else
    FMousePos := Point(-1, -1);

  if Button <> TMouseButton.mbLeft then
    Exit;

  if (FMoveMode = mmPreSelect) and not (ssCtrl in Shift) then
  begin
    if PtInRect(FRealSelectionRect, ClientPos) then
    begin
      SetViewportWidth(FSelectionRect.Right - FSelectionRect.Left + 1);
      SetViewportStart(FSelectionRect.Left);
    end;
    ResetSelection;
    Invalidate;
  end;

  FMoveMode := mmSimple;
end;

procedure TSimpleChart.HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer; ClientPos: TPointF;
  var AHandled: Boolean);
var
  vOldViewPortWidth: Integer;
  vXValue: Double;
  vFactor: Double;
begin
  vXValue := XValueAtPos(ClientPos.X, 0);
  if vXValue < 0 then
    Exit;

  // Увеличение / уменьшение
  vOldViewPortWidth := FViewportWidth;
  if WheelDelta < 0 then
    SetViewportWidth(FViewportWidth * cZoomFactor)
  else
    SetViewportWidth(FViewportWidth / cZoomFactor);

  vFactor := (vXValue - FViewportStart) / (vOldViewPortWidth - 1);
  SetViewportStart(vXValue - (FViewportWidth - 1) * vFactor);

  Invalidate;
end;

function TSimpleChart.IsScaled(const AViewportStart, AViewportWidth: Integer): Boolean;
begin
  Result := not((AViewportStart = 0) and (MaxDimension + 1 = AViewportWidth));
end;

function TSimpleChart.IsSelectionVisible: Boolean;
begin
  Result := not IsRectEmpty(FSelectionRect) and
    (FSelectionRect.Left < FViewportStart + FViewportWidth - 1) and
    (FSelectionRect.Right > FViewportStart);
end;

procedure TSimpleChart.RemoveLightAreas;
begin
  if FLightAreas.Count > 0 then
  begin
    FLightAreas.Clear;
    Invalidate;
  end;
end;

procedure TSimpleChart.ResetSelection;
begin
  FSelectionRect := TRectF.Empty;
end;

procedure TSimpleChart.ResetZooming;
begin
  FViewportStart := MinDimension;
  FViewportWidth := MaxDimension - MinDimension - FViewportStart + 1;
end;

procedure TSimpleChart.Select(const ALeftX, ARightX: Double);
begin
  FSelectionRect := RectF(ALeftX, FChartRect.Top, ARightX, FChartRect.Bottom);
  Invalidate(ssUsed);
end;

procedure TSimpleChart.SetViewportStart(const Value: Double);
begin
  if Value < MinDimension then
    FViewportStart := MinDimension
  else if Value + FViewportWidth - 1 > MaxDimension then
    FViewportStart := MaxDimension + 1 - FViewportWidth
  else
    FViewportStart := Round(Value);
end;

procedure TSimpleChart.SetViewportWidth(const Value: Double);
var
  vValue: Integer;
begin
  vValue := Round(Value);
  if vValue < cMinVisibleValues then
    vValue := cMinVisibleValues
  else if vValue > MaxDimension + 1 then
    vValue := MaxDimension + 1;

  if FViewportStart + vValue - 1 > MaxDimension then
  begin
    FViewportStart := MaxDimension + 1 - vValue;
    if FViewportStart + vValue - 1 > MaxDimension then
    begin
      vValue := MaxDimension + 1 - FViewportStart;
      if vValue < cMinVisibleValues then
      begin
        vValue := cMinVisibleValues;
        FViewportStart := MaxDimension + 1 - vValue;
      end;
    end;
  end;

  FViewportWidth := vValue;
end;

function TSimpleChart.XValueAtPos(const X: Single; const ASerieNo: Integer = 0): Double;
begin
  Result := -1;
  if (X < FChartRect.Left) or (X > FChartRect.Right) then
    Exit;

  Result := FViewportStart + (X - FChartRect.Left) * (FViewportWidth - 1) / FChartRect.Width;
  if Result > MaxDimension then
    Result := MaxDimension;
end;

{ TDataChart }

constructor TDataChart.Create(const AScene: TScene; const AParent: TSceneObject);
begin
  FTensor := TTensor.Create([{50 + Random(100)}100]);
  FTensor.Fill(dfkRandom, 0, 1000);
  FTensor.MinMax(FMin, FMax);

  inherited Create(AScene, AParent);
end;

destructor TDataChart.Destroy;
begin
  FreeAndNil(FTensor);
  inherited Destroy;
end;

function TDataChart.GetExtraValue(const AIndex: Integer): Double;
begin
  Result := 0;
end;

function TDataChart.GetValue(const AIndex: Integer): Double;
begin
  Result := FTensor.FlatValues[AIndex];
end;

function TDataChart.MaxDimension: Integer;
begin
  Result := FTensor.Size - 1;
end;

function TDataChart.MaxValue: Double;
begin
  Result := FMax;
end;

function TDataChart.MinDimension: Integer;
begin
  Result := 0;
end;

function TDataChart.MinValue: Double;
begin
  Result := FMin;
end;

end.
