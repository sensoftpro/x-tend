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

unit uSamplePainter;

interface

uses
  Windows, Types, Classes, uScene, uWinScene, uDrawStyles, uConsts;

type
  TSamplePen = class
  end;

  TSampleBrush = class
  end;

  TSampleFont = class
  end;

  TSampleImage = class
  end;

  TSamplePath = class
  end;

  TSampleNinePatchImage = class(TNinePatchImage)
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject; override;
  end;

  TSampleDrawContext = class(TDrawContext)
  protected
    procedure DoSetSize(const AWidth, AHeight: Single); override;
  public
    procedure SaveToFile(const AFileName: string); override;
  end;

  TSamplePainter = class(TPainter)
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

    function SetContext(const AContext: TDrawContext): TDrawContext; override;

    procedure BeginPaint; override;
    procedure EndPaint; override;
  end;

  TSampleScene = class(TWinScene)
  private
    FStaticContext: TSampleDrawContext;
  protected
    procedure DoDestroyScene; override;
    procedure DoRender(const ANeedFullRepaint: Boolean); override;
    procedure UpdateContexts(const AWidth, AHeight: Single); override;
    function CreatePainter(const AContainer: TObject): TPainter; override;
  end;

implementation

uses
  StrUtils, SysUtils, IOUtils, Math, uModule;

{ TSamplePainter }

procedure TSamplePainter.BeginPaint;
begin

end;

constructor TSamplePainter.Create(const AContainer: TObject);
var
  vContainer: TDrawContainer absolute AContainer;
begin
  inherited Create(AContainer);

  FContext := TSampleDrawContext.Create(vContainer.Width, vContainer.Height);
end;

procedure TSamplePainter.CreateBrush(const AFill: TStyleBrush);
begin
  if AFill.Style = bsSolid then
  begin
    if AFill.GradientKind <> gkNone then
    begin
      // Создать градиентную кисть, привязать ее к TStyleBrush
      AFill.NativeObject := TSampleBrush.Create();
    end
    else begin
      // Создать сплошную кисть, привязать ее к TStyleBrush
      AFill.NativeObject := TSampleBrush.Create();
    end;
  end
  else begin
    // Создать штриховую кисть, привязать ее к TStyleBrush
    AFill.NativeObject := TSampleBrush.Create();
  end;
end;

function TSamplePainter.CreateDrawContext(const AWidth, AHeight: Single): TDrawContext;
begin
  Result := TSampleDrawContext.Create(AWidth, AHeight);
end;

procedure TSamplePainter.CreateFont(const AFont: TStyleFont);
begin
  // Проанализировать стиль шрифта

  // Создать описатель шрифта и, при необходимости, кисть для его отрисовки

  // Привязать все созданные объекты к TStyleFont
  AFont.NativeObject := TSampleFont.Create();
end;

procedure TSamplePainter.CreateImage(const AImage: TStyleImage);
var
  vImage: TSampleImage;
begin
  if not TFile.Exists(AImage.FileName) then
    Exit;

  // Загрузить рисунок из файла и привязать его к TStyleImage
  vImage := TSampleImage.Create();

  if AImage.IsNinePatch then
    AImage.NativeObject := TSampleNinePatchImage.Create(Self, vImage, AImage.CutRect.Left, AImage.CutRect.Right,
      AImage.CutRect.Top, AImage.CutRect.Bottom)
  else
    AImage.NativeObject := vImage;
end;

procedure TSamplePainter.CreatePen(const AStroke: TStylePen);
const
  cDashStyles: array[psDash..psDashDotDot] of TObject = (nil, nil, nil, nil);
begin
  if AStroke.Style in [psDash..psDashDotDot] then
    // Создать штриховое перо
  else
    // Создать непрерывное перо

  // Привязать созданные перья к TStylePen
  AStroke.NativeObject := TSamplePen.Create();
end;

destructor TSamplePainter.Destroy;
begin
  // Очистить все созданные в конструкторе объекты
  inherited Destroy;
end;

procedure TSamplePainter.DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal);
begin

end;

procedure TSamplePainter.DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal);
begin

end;

procedure TSamplePainter.DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal);
begin

end;

procedure TSamplePainter.DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
begin

end;

procedure TSamplePainter.DoDrawContext(const AContext: TDrawContext);
begin

end;

procedure TSamplePainter.DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
begin
  if Assigned(AFill) then
  begin
    // Нарисовать заливку
  end;

  if Assigned(AStroke) then
  begin
    // Нарисовать Обводку
  end;
end;

procedure TSamplePainter.DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
begin

end;

procedure TSamplePainter.DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF);
begin

end;

procedure TSamplePainter.DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject);
begin
  if Assigned(AFill) then
  begin
    // Нарисовать заливку
  end;

  if Assigned(AStroke) then
  begin
    // Нарисовать Обводку
  end;
end;

procedure TSamplePainter.DoDrawPie(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF;
  const AStartAngle, AEndAngle: Single);
begin
  if Assigned(AFill) then
  begin
    // Нарисовать заливку
  end;

  if Assigned(AStroke) then
  begin
    // Нарисовать Обводку
  end;
end;

procedure TSamplePainter.DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
begin

end;

procedure TSamplePainter.DoDrawRect(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
begin
  if Assigned(AFill) then
  begin
    // Нарисовать заливку
  end;

  if Assigned(AStroke) then
  begin
    // Нарисовать Обводку
  end;
end;

procedure TSamplePainter.DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen; const APoints: PPointF;
  const ACount: Integer);
begin
  if Assigned(AFill) then
  begin
    // Нарисовать заливку
  end;

  if Assigned(AStroke) then
  begin
    // Нарисовать Обводку
  end;
end;

procedure TSamplePainter.DoDrawText(const AFont: TStyleFont; const AText: string;
  const ARect: TRectF; const AOptions: Cardinal; const AAngle: Single);
begin
  // Проанализировать стиль шрифта и опции для вывода

  // Вывести текст с учетом угла наклона (градусы)
end;

procedure TSamplePainter.DoInvertRect(const ARect: TRectF);
begin

end;

procedure TSamplePainter.EndPaint;
begin

end;

function TSamplePainter.GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF;
begin

end;

function TSamplePainter.SetContext(const AContext: TDrawContext): TDrawContext;
begin
  Result := inherited SetContext(AContext);
end;

{ TSampleNinePatchImage }

function TSampleNinePatchImage.CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject;
begin
  // Вырезать новый TSampleImage из FImage
  Result := nil;
end;

function TSampleNinePatchImage.GetHeight: Integer;
begin
  // Взять высоту из FImage
  Result := 0;
end;

function TSampleNinePatchImage.GetWidth: Integer;
begin
  // Взять ширину из FImage
  Result := 0;
end;

{ TSampleScene }

function TSampleScene.CreatePainter(const AContainer: TObject): TPainter;
var
  vContainer: TDrawContainer absolute AContainer;
begin
  Result := TSamplePainter.Create(AContainer);
  FStaticContext := TSampleDrawContext(Result.CreateDrawContext(vContainer.Width, vContainer.Height));
end;

procedure TSampleScene.DoDestroyScene;
begin
  inherited DoDestroyScene;
  FreeAndNil(FStaticContext);
end;

procedure TSampleScene.DoRender(const ANeedFullRepaint: Boolean);
var
  vOldContext: TDrawContext;
begin
  if ANeedFullRepaint then
  begin
    // Отрисовываем редко изменяемое содержимое в статический буфер
    vOldContext := FPainter.SetContext(FStaticContext);
    try
      FPainter.BeginPaint;
      try
        FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
      finally
        FPainter.EndPaint;
      end;
    finally
      FPainter.SetContext(vOldContext);
    end;
  end;

  FPainter.BeginPaint;
  try
    FPainter.DrawContext(FStaticContext);
    FRoot.RenderDynamic(FPainter, GetSceneRect);
  finally
    FPainter.EndPaint;
  end;
end;

procedure TSampleScene.UpdateContexts(const AWidth, AHeight: Single);
begin
  FStaticContext.SetSize(AWidth, AHeight);
end;

{ TSampleDrawContext }

procedure TSampleDrawContext.DoSetSize(const AWidth, AHeight: Single);
begin
  // Отреагировать на изменение размера, установить нужные внутренние размеры контекста
end;

procedure TSampleDrawContext.SaveToFile(const AFileName: string);
begin

end;

initialization

TBaseModule.RegisterModule('Painting', 'Sample', TSampleScene);

end.

