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

unit uChartUtils;

interface

uses
  Types, uDrawStyles;

const
  cColumnsAir = 12;
  cScaleSpace = 8;
  cEnergyScaleSpace = 0;
  cScrollAreaHeight = 60 + cScaleSpace;
  cZoomFactor = 1.3;
  cMinVisibleValues = 15;
  cSerieLegendWidth = 8;
  cColorPanelWidth = 12;

type
  TSerieKind = (srkMeasurement, srkBackground, srkReference);

  TMoveMode = (mmSimple, mmPreSelect, mmSelect, mmScroll);

  TAxisOrientation = (aoVertical, aoHorizontal);

  TVertScaleMode = (vsmCount, vsmSpeed);
  TChartMode = (cmLinear, cmLog);

  TChartScaleFactors = record
    // Количество надписей, которое поместится в указанный размер
    ItemsCount: Integer;
    ScaleStep: Double;
    Ratio: Double;
  end;

  TChartPaintOption = (
    // отображать значения сверху вниз и справа налево
    cpoReverse,
    // выделять нулевые оси
    cpoHighlightZero,
    // использовать все доступное место под данные
    cpoUtilizeAllSpace,
    // не рисовать крайние границы графика
    cpoHideBoundAxis,
    // если диапазон (мин-макс) не содержит ноль, включить его как одну из границ
    cpoAlwaysIncludeZero,
    // логарифмический масштаб
    cpoLogarithmic,
    // использовать только целые числа
    cpoIntOnly);
  TChartPaintOptions = set of TChartPaintOption;

  TAxisMetrics = record
    // Количество надписей, которое поместится в указанный размер
    ItemsCount: Integer;
    ScaleStep: Double;
    Ratio: Double;
    // Значение, с которого начнется подписывание
    StartValue: Double;
    // Минимальное значение
    MinValue: Double;
    MaxValue: Double;
    // Диапазон всех возможных значений
    Range: Double;
    // Прямоугольник для вывода
    Rect: TRectF;
    // Полная ширина шкалы
    Width: Single;
    // Горизонтальная или вертикальная метрика
    Orientation: TAxisOrientation;
    // Отображать шкалу в реверсном направлении
    Options: TChartPaintOptions;
    // Формат для вывода шкалы
    Format: string;
    // Смещение начала координат
    Shift: Integer;
  end;

procedure CalcAxisFactors(var AFactors: TChartScaleFactors;
  const AMaxValue: Double; const APossibleItemsCount: Integer; const ASize: Single;
  const ANeedAllSpace: Boolean);

function CalcAxisMetrics(const AMinValue, AMaxValue: Double; const AAreaSize,
  AItemSize: Single; const AOrientation: TAxisOrientation; const AShift: Integer;
  const AOptions: TChartPaintOptions): TAxisMetrics;

function CalcAxisTextWidth(const AMetrics: TAxisMetrics; const APainter: TPainter; const AStyle: TDrawStyle;
  const AFontName: string): Single;

procedure DrawAxisText(const AMetrics: TAxisMetrics; const APainter: TPainter; const AStyle: TDrawStyle;
  const AFontName: string; const AChartRect: TRectF; const IsLeftAlign: Boolean = True);

implementation

uses
  SysUtils, StrUtils, Math;

function CalcStep(const AMaxValue: Double; const AItemCount: Integer;
  const AUseOnlyIntegers: Boolean = True): Double;
var
  vStep: Double;
  vKoef: Double;
begin
  if AMaxValue < 0 then
  begin
    Result := 1;
    Exit;
  end;
  vStep := AMaxValue / AItemCount;
  vKoef := 1;
  if vStep > 10 then
    while vStep > 10 do
    begin
      vStep := vStep / 10;
      vKoef := vKoef * 10;
    end
  else if vStep < 1 then
  begin
    if AUseOnlyIntegers then
      vStep := 1
    else
      while vStep < 1 do
      begin
        vStep := vStep * 10;
        vKoef := vKoef / 10;
      end;
  end;

  if vStep = 1 then
    Result := vKoef
  else if vStep <= 2 then
    Result := 2 * vKoef
  else if vStep <= 5 then
    Result := 5 * vKoef
  else
    Result := 10 * vKoef;
end;

procedure CalcAxisFactors(var AFactors: TChartScaleFactors;
  const AMaxValue: Double; const APossibleItemsCount: Integer; const ASize: Single;
  const ANeedAllSpace: Boolean);
begin
  // Не учитываются отрицательные значения
  if AMaxValue = 0 then
  begin
    AFactors.ScaleStep := 1;
    AFactors.ItemsCount := 1;
  end
  else
  begin
    AFactors.ScaleStep := CalcStep(AMaxValue, APossibleItemsCount);
    AFactors.ItemsCount := Ceil(Abs(AMaxValue) / AFactors.ScaleStep);
  end;

  // Коэффициент вывода по оси Y
  if ANeedAllSpace then
    AFactors.Ratio := AMaxValue / ASize
  else
    AFactors.Ratio := (AFactors.ScaleStep * AFactors.ItemsCount) / ASize;

  if SameValue(AFactors.Ratio, 0) then
    AFactors.Ratio := 1;
end;

function CalcAxisMetrics(const AMinValue, AMaxValue: Double; const AAreaSize,
  AItemSize: Single; const AOrientation: TAxisOrientation; const AShift: Integer;
  const AOptions: TChartPaintOptions): TAxisMetrics;
var
  vPossibleItemCount: Integer;
  vAxisStep: Double;
  vFloatDigitCount: Integer;
begin
  Result.Orientation := AOrientation;
  Result.Options := AOptions;
  Result.Width := AAreaSize;
  Result.Shift := AShift;
  if AItemSize > 0.001 then
    vPossibleItemCount := Max(Trunc(AAreaSize / AItemSize), 1)
  else
    vPossibleItemCount := 1;

  if cpoLogarithmic in AOptions then
  begin
    Result.MinValue := Ceil(Log10(Max(AMinValue, 0.1)));
    Result.MaxValue := Ceil(Log10(Max(AMaxValue, 1)));
  end
  else if cpoAlwaysIncludeZero in AOptions then
  begin
    Result.MinValue := IfThen(AMinValue > 0, 0, AMinValue);
    Result.MaxValue := IfThen(AMaxValue < 0, 0, AMaxValue);
  end
  else begin
    Result.MinValue := AMinValue;
    Result.MaxValue := AMaxValue;
  end;
  Result.Format := '0';

  if SameValue(Result.MinValue, Result.MaxValue) then
  begin
    Result.Range := 1;
    Result.ScaleStep := 1;
    Result.ItemsCount := 1;
    Result.StartValue := 0;
    Result.MinValue := 0;
    Result.MaxValue := 1;
  end
  else begin
    vAxisStep := CalcStep(Result.MaxValue - Result.MinValue, vPossibleItemCount, cpoIntOnly in AOptions);
    vFloatDigitCount := Floor(Log10(vAxisStep));
    if vFloatDigitCount < 0 then
      Result.Format := '0.' + StringOfChar('#', Abs(vFloatDigitCount));

    Result.ScaleStep := vAxisStep;

    if cpoUtilizeAllSpace in Result.Options then
    begin
      Result.ItemsCount := Trunc(-Result.MinValue / vAxisStep) + Trunc(Result.MaxValue / vAxisStep);
      Result.StartValue := Ceil(Result.MinValue / vAxisStep) * vAxisStep;
    end
    else begin
      Result.ItemsCount := Ceil(-Result.MinValue / vAxisStep) + Ceil(Result.MaxValue / vAxisStep);
      Result.StartValue := Floor(Result.MinValue / vAxisStep) * vAxisStep;
      Result.MinValue := Result.StartValue;
      Result.MaxValue := Result.MinValue + vAxisStep * Result.ItemsCount;
    end;
  end;

  if AAreaSize = 0 then
    Result.Ratio := 1
  else
    Result.Ratio := (Result.MaxValue - Result.MinValue) / AAreaSize;

//  if SameValue(Result.Ratio, 0) then
//    Result.Ratio := 1;
end;

function CalcAxisTextWidth(const AMetrics: TAxisMetrics; const APainter: TPainter; const AStyle: TDrawStyle;
  const AFontName: string): Single;
var
  i: Integer;
  vCurValue: Double;
  vCurText: string;
  vWidth: Single;
begin
  Result := 0;
  if cpoLogarithmic in AMetrics.Options then
  begin
    for i := -1 to AMetrics.ItemsCount - 1 do
    begin
      if i <= 1 then
        vWidth := APainter.TextWidth(AStyle, AFontName, '0.1')
      else begin
        vWidth := APainter.TextWidth(AStyle, AFontName, '10');
        vWidth := vWidth + APainter.TextWidth(AStyle, 'power', IntToStr(i)) + 1
      end;

      if vWidth > Result then
        Result := vWidth;
    end;
  end
  else begin
    for i := 0 to AMetrics.ItemsCount do
    begin
      vCurValue := AMetrics.StartValue + i * AMetrics.ScaleStep;
      vCurText := FormatFloat(AMetrics.Format, vCurValue);
      vWidth := APainter.TextWidth(AStyle, AFontName, vCurText);
      if vWidth > Result then
        Result := vWidth;
    end;
  end;
end;

procedure DrawAxisText(const AMetrics: TAxisMetrics; const APainter: TPainter; const AStyle: TDrawStyle;
  const AFontName: string; const AChartRect: TRectF; const IsLeftAlign: Boolean = True);
var
  i: Integer;
  vCurValue: Double;
  vCurText: string;
  vTextSize: Single;
  vTextRect: TRectF;
  vPowerRect: TRectF;
  vCurPosition: Single;
  vStrokeName: string;

  function GetChartLocation(const AChartRect: TRectF;
    const AOrientation: TAxisOrientation; const AXOR: Boolean): Single;
  begin
    if AOrientation = aoVertical then
    begin
      if AXOR then
        Result := AChartRect.Top
      else
        Result := AChartRect.Bottom;
    end
    else begin
      if AXOR then
        Result := AChartRect.Left
      else
        Result := AChartRect.Right;
    end;
  end;

begin
  vTextRect := AMetrics.Rect;

  // Дорисовывание границ чарта
  if not (cpoHideBoundAxis in AMetrics.Options) then
  begin
    if not SameValue(AMetrics.StartValue, AMetrics.MinValue) then
    begin
      vCurPosition := GetChartLocation(AChartRect, AMetrics.Orientation, (cpoReverse in AMetrics.Options) xor True);
      if AMetrics.Orientation = aoVertical then
        APainter.DrawLine(AStyle, 'grid', PointF(AChartRect.Left, vCurPosition), PointF(AChartRect.Right, vCurPosition))
      else
        APainter.DrawLine(AStyle, 'grid', PointF(vCurPosition, AChartRect.Top), PointF(vCurPosition, AChartRect.Bottom));
    end;

    if not SameValue(AMetrics.StartValue + AMetrics.ItemsCount * AMetrics.ScaleStep, AMetrics.MaxValue) then
    begin
      vCurPosition := GetChartLocation(AChartRect, AMetrics.Orientation, (cpoReverse in AMetrics.Options) xor False);
      if AMetrics.Orientation = aoVertical then
        APainter.DrawLine(AStyle, 'grid', PointF(AChartRect.Left, vCurPosition), PointF(AChartRect.Right, vCurPosition))
      else
        APainter.DrawLine(AStyle, 'grid', PointF(vCurPosition, AChartRect.Top), PointF(vCurPosition, AChartRect.Bottom));
    end;
  end;

  if AMetrics.Orientation = aoVertical then
  begin
    vTextSize := APainter.TextHeight(AStyle, AFontName, '0');

    if cpoLogarithmic in AMetrics.Options then
    begin
      for i := -1 to AMetrics.ItemsCount - 1 do
      begin
        vCurValue := i;
        if vCurValue > AMetrics.MaxValue then
          Break;

        if i = -1 then
          vCurText := '0.1'
        else if i = 0 then
          vCurText := '1'
        else
          vCurText := '10';

        if cpoReverse in AMetrics.Options then
          vCurPosition := AChartRect.Top + (vCurValue - AMetrics.MinValue) / AMetrics.Ratio
        else
          vCurPosition := AChartRect.Bottom - (vCurValue - AMetrics.MinValue) / AMetrics.Ratio;

        if (vCurValue = -1) and (cpoHighlightZero in AMetrics.Options) then
          vStrokeName := 'axis'
        else
          vStrokeName := 'grid';
        APainter.DrawLine(AStyle, vStrokeName, PointF(AChartRect.Left, vCurPosition),
          PointF(AChartRect.Right, vCurPosition));

        vTextRect := AMetrics.Rect;
        vTextRect.Top := vCurPosition - vTextSize / 2;
        vTextRect.Bottom := vTextRect.Top + vTextSize;

        if i > 1 then
        begin
          vPowerRect := vTextRect;
          vPowerRect.Top := vPowerRect.Top - vPowerRect.Height / 4;
          vPowerRect.Bottom := vPowerRect.Top + APainter.TextHeight(AStyle, 'power', '0');

          if IsLeftAlign then
          begin
            vPowerRect.Left := vPowerRect.Left + APainter.TextWidth(AStyle, AFontName, vCurText) + 1;
            vPowerRect.Right := vPowerRect.Left + APainter.TextWidth(AStyle, 'power', IntToStr(i));
            vTextRect.Right := vTextRect.Left + APainter.TextWidth(AStyle, AFontName, vCurText);
            APainter.DrawText(AStyle, 'power', IntToStr(i), vPowerRect, DT_LEFT);
          end
          else begin
            vPowerRect.Left := vPowerRect.Right - APainter.TextWidth(AStyle, 'power', IntToStr(i));
            vTextRect.Right := vPowerRect.Left - 1;
            vTextRect.Left := vTextRect.Right - APainter.TextWidth(AStyle, AFontName, vCurText);
            APainter.DrawText(AStyle, 'power', IntToStr(i), vPowerRect, DT_LEFT);
          end;
        end;

        if IsLeftAlign then
          APainter.DrawText(AStyle, AFontName, vCurText, vTextRect, DT_LEFT)
        else
          APainter.DrawText(AStyle, AFontName, vCurText, vTextRect, DT_RIGHT);
      end;
    end
    else begin
      for i := 0 to AMetrics.ItemsCount do
      begin
        vCurValue := AMetrics.StartValue + i * AMetrics.ScaleStep;
        if vCurValue > AMetrics.MaxValue then
          Break;
        vCurText := FormatFloat(AMetrics.Format, vCurValue);
        if cpoReverse in AMetrics.Options then
          vCurPosition := AChartRect.Top + (vCurValue - AMetrics.MinValue) / AMetrics.Ratio
        else
          vCurPosition := AChartRect.Bottom - (vCurValue - AMetrics.MinValue) / AMetrics.Ratio;

        if (vCurValue = 0) and (cpoHighlightZero in AMetrics.Options) then
          vStrokeName := 'axis'
        else
          vStrokeName := 'grid';
        APainter.DrawLine(AStyle, vStrokeName, PointF(AChartRect.Left, vCurPosition),
          PointF(AChartRect.Right, vCurPosition));

        vTextRect.Top := vCurPosition - vTextSize / 2;
        vTextRect.Bottom := vTextRect.Top + vTextSize;

        if IsLeftAlign then
          APainter.DrawText(AStyle, AFontName, vCurText, vTextRect, DT_LEFT)
        else
          APainter.DrawText(AStyle, AFontName, vCurText, vTextRect, DT_RIGHT);
      end;
    end;
  end
  else begin
    for i := 0 to AMetrics.ItemsCount do
    begin
      vCurValue := AMetrics.StartValue + i * AMetrics.ScaleStep;
      if vCurValue > AMetrics.MaxValue then
        Break;
      vCurText := FormatFloat(AMetrics.Format, vCurValue);
      if cpoReverse in AMetrics.Options then
        vCurPosition := AChartRect.Right - (vCurValue - AMetrics.MinValue) / AMetrics.Ratio
      else
        vCurPosition := AChartRect.Left + (vCurValue - AMetrics.MinValue) / AMetrics.Ratio;

      if (vCurPosition < AChartRect.Left) or (vCurPosition > AChartRect.Right) then
        Continue;

      if not (cpoHideBoundAxis in AMetrics.Options) then
      begin
        if (vCurValue = 0) and (cpoHighlightZero in AMetrics.Options) then
          vStrokeName := 'axis'
        else
          vStrokeName := 'grid';
        APainter.DrawLine(AStyle, vStrokeName, PointF(vCurPosition, AChartRect.Top),
          PointF(vCurPosition, AChartRect.Bottom));
      end;

      vTextSize := APainter.TextWidth(AStyle, AFontName, vCurText);
      vTextRect.Left := vCurPosition - vTextSize / 2;
      vTextRect.Right := vTextRect.Left + vTextSize;

      APainter.DrawText(AStyle, AFontName, vCurText, vTextRect, DT_CENTER);
    end;
  end;
end;

end.
