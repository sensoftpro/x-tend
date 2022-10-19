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

unit uVCLPainter;

interface

uses
  Windows, Types, Classes, Graphics, uDrawStyles,

  uWinScene,   //todo: нужно выделить общий vcl-код из devexpress
  uVCLScene    //todo: нужно выделить общий vcl-код из devexpress
  ;

type
  TPointArray = array of TPoint;

  TVCLPainter = class(TPainter)
  private
    FClearPen: TPen;
    FClearBrush: TBrush;
    FBlendBitmap: TBitmap;
    FClippedRegion: HRGN;
    function PPointFToPoints(const APoints: PPointF; const ACount: Integer): TPointArray;
    procedure InternalFillRect(const AFill: TStyleBrush; const ARect: TRect);
    function CorrectColor(const AColor: Cardinal): TColor;
    //function SplitText(const AText: string; const AMaxWidth: Integer): TStrings;
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
    procedure DoDrawText(const AFont: TStyleFont; const AText: string; const ARect: TRectF;
      const AOptions: Cardinal; const AAngle: Single); override;
    function GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF; override;
    procedure DoClipRect(const ARect: TRectF); override;
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
  end;

  TVCLScene = class(TWinCanvasScene)
  protected
    function CreatePainter(const AContainer: TObject): TPainter; override;
  end;

implementation

uses
  StrUtils, SysUtils, PngImage, UITypes, UIConsts, IOUtils, Math, uModule;

{ TVCLPainter }

procedure TVCLPainter.DoClipRect(const ARect: TRectF);
var
  vRect: TRect;
begin
  vRect := ARect.Round;

  if FClippedRegion > 0 then
    DeleteObject(HRGN(FClippedRegion));

  if not vRect.IsEmpty then
    FClippedRegion := CreateRectRgn(vRect.Left, vRect.Top, vRect.Right, vRect.Bottom)
  else
    FClippedRegion := HRGN(nil);

  SelectClipRgn(ThisCanvas.Handle, FClippedRegion);
end;

procedure TVCLPainter.DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal);
begin
  TBrush(AFill.NativeObject).Color := CorrectColor(AColor);
end;

procedure TVCLPainter.DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal);
begin
  TFont(AFont.NativeObject).Color := CorrectColor(AColor);
end;

procedure TVCLPainter.DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal);
begin
  TPen(AStroke.NativeObject).Color := CorrectColor(AColor);
end;

function TVCLPainter.CorrectColor(const AColor: Cardinal): TColor;
begin
  Result := TColor(RGB(GetBValue(AColor), GetGValue(AColor), GetRValue(AColor)));
end;

constructor TVCLPainter.Create(const AContainer: TObject);
var
  vContainer: TDrawContainer absolute AContainer;
begin
  inherited Create(AContainer);

  FContext := TWinDrawContext.Create(Self, vContainer.Canvas, vContainer.Width, vContainer.Height);
  FClippedRegion := 0;

  FClearPen := TPen.Create;
  FClearPen.Style := Graphics.psClear;
  FClearBrush := TBrush.Create;
  FClearBrush.Style := Graphics.bsClear;

  FBlendBitmap := TBitmap.Create;
  FBlendBitmap.PixelFormat := pf32bit;
  FBlendBitmap.SetSize(1, 1);
end;

procedure TVCLPainter.CreateBrush(const AFill: TStyleBrush);
var
  vBrush: TBrush;
begin
  if (AFill.GradientKind = gkNone) and (AFill.Color and $FF000000 = $FF000000) then
  begin
    vBrush := TBrush.Create;
    vBrush.Color := CorrectColor(AFill.Color);
    vBrush.Style := Graphics.TBrushStyle(AFill.Style);
  end
  else
    vBrush := nil;

  AFill.NativeObject := vBrush;
end;

function TVCLPainter.CreateDrawContext(const AWidth, AHeight: Single): TDrawContext;
begin
  Result := TWinDrawContext.Create(Self, nil, AWidth, AHeight);
end;

procedure TVCLPainter.CreateFont(const AFont: TStyleFont);
var
  vFont: TFont;
begin
  vFont := TFont.Create;
  vFont.Color := CorrectColor(AFont.Color);
  vFont.Size := Round(AFont.Size);
  vFont.Name := AFont.Family;
  vFont.Style := [];
  if (FontStyleBold and AFont.Style > 0) then
    vFont.Style := vFont.Style + [fsBold];
  if (FontStyleItalic and AFont.Style > 0) then
    vFont.Style := vFont.Style + [fsItalic];
  if (FontStyleUnderline and AFont.Style > 0) then
    vFont.Style := vFont.Style + [fsUnderline];
  if (FontStyleStrikeout and AFont.Style > 0) then
    vFont.Style := vFont.Style + [fsStrikeOut];

  //vFont.Orientation := Round(AFont.Angle * 10);
  if AFont.Angle > 0 then
    vFont.Orientation := 900
  else
    vFont.Orientation := 0;

  case AFont.Quality of
    rqLow: vFont.Quality := fqNonAntialiased;
    rqHigh: vFont.Quality := fqAntialiased;
  else
    vFont.Quality := fqDefault;
  end;

  AFont.NativeObject := vFont;
end;

procedure TVCLPainter.CreateImage(const AImage: TStyleImage);
var
  vImage: TGraphic;
begin
  if Assigned(AImage) then
  begin
    if TFile.Exists(AImage.FileName) then
    begin
      vImage := TPngImage.Create;
      vImage.LoadFromFile(AImage.FileName);
    end
    else
      Exit;

    if AImage.IsNinePatch then
      AImage.NativeObject := TWinNinePatchImage.Create(Self, vImage, AImage.CutRect.Left, AImage.CutRect.Right,
        AImage.CutRect.Top, AImage.CutRect.Bottom)
    else
      AImage.NativeObject := vImage;
  end;
end;

procedure TVCLPainter.CreatePen(const AStroke: TStylePen);
var
  vPen: TPen;
begin
  vPen := TPen.Create;
  vPen.Color := CorrectColor(AStroke.Color);
  vPen.Width := Round(AStroke.Width);
  vPen.Style :=  Graphics.TPenStyle(AStroke.Style);

  AStroke.NativeObject := vPen;
end;

destructor TVCLPainter.Destroy;
begin
  FreeAndNil(FClearPen);
  FreeAndNil(FClearBrush);
  FreeAndNil(FBlendBitmap);
  inherited Destroy;
end;

procedure TVCLPainter.DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vOldPen: TPen;
  vPoints: TPointArray;
begin
  vOldPen := ThisCanvas.Pen;

  vPoints := PPointFToPoints(APoints, ACount);
  ThisCanvas.Pen := TPen(AStroke.NativeObject);
  try
    ThisCanvas.PolyBezier(vPoints);
  finally
    SetLength(vPoints, 0);
    ThisCanvas.Pen := vOldPen;
  end;
end;

procedure TVCLPainter.DoDrawContext(const AContext: TDrawContext);
begin
  BitBlt(ThisCanvas.Handle, 0, 0, AContext.Width, AContext.Height,
    TWinDrawContext(AContext).Handle, 0, 0, SRCCOPY);
end;

procedure TVCLPainter.DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
var
  vOldPen: TPen;
  vOldBrush: TBrush;
begin
  vOldPen := ThisCanvas.Pen;
  vOldBrush := ThisCanvas.Brush;
  if Assigned(AStroke) then
    ThisCanvas.Pen := TPen(AStroke.NativeObject)
  else
    ThisCanvas.Pen := FClearPen;
  if Assigned(AFill) then
    ThisCanvas.Brush := TBrush(AFill.NativeObject)
  else
    ThisCanvas.Brush := FClearBrush;
  try
    ThisCanvas.Ellipse(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  finally
    ThisCanvas.Pen := vOldPen;
    ThisCanvas.Brush := vOldBrush;
  end;
end;

procedure TVCLPainter.DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
var
  vBitmap: TBitmap absolute AImage;
  vBlendFunction: TBlendFunction;
  vRect: TRect;
begin
  vRect := ARect.Round;

  if (AImage is TBitmap) and (vRect.Width = vBitmap.Width) and (vRect.Height = vBitmap.Height) then
  begin
    if SameValue(AOpacity, 1) then
      BitBlt(TWinDrawContext(FContext).Handle, vRect.Left, vRect.Top, vRect.Width,
        vRect.Height, vBitmap.Canvas.Handle, 0, 0, SRCCOPY)
    else begin
      vBlendFunction.BlendOp := AC_SRC_OVER;
      vBlendFunction.BlendFlags := 0;
      vBlendFunction.SourceConstantAlpha := Round(AOpacity * 255);
      vBlendFunction.AlphaFormat := 0;

      AlphaBlend(ThisCanvas.Handle, vRect.Left, vRect.Top, vRect.Width,
        vRect.Height, vBitmap.Canvas.Handle, 0, 0, vBitmap.Width, vBitmap.Height, vBlendFunction);
    end;
  end
  else
    ThisCanvas.StretchDraw(ARect.Round, TGraphic(AImage));
end;

procedure TVCLPainter.DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF);
var
  vOldPen: TPen;
  vOldBrushStyle: TBrushStyle;
begin
  vOldPen := ThisCanvas.Pen;
  vOldBrushStyle := ThisCanvas.Brush.Style;

  ThisCanvas.Pen := TPen(AStroke.NativeObject);
  ThisCanvas.Brush.Style := bsClear;
  try
    ThisCanvas.MoveTo(Round(APoint1.X), Round(APoint1.Y));
    ThisCanvas.LineTo(Round(APoint2.X), Round(APoint2.Y));
  finally
    ThisCanvas.Pen := vOldPen;
    ThisCanvas.Brush.Style := vOldBrushStyle;
  end;
end;

procedure TVCLPainter.DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject);
var
  vOldPen: TPen;
  vOldBrush: TBrush;
begin
  vOldPen := ThisCanvas.Pen;
  vOldBrush := ThisCanvas.Brush;
  if Assigned(AStroke) then
    ThisCanvas.Pen := TPen(AStroke.NativeObject)
  else
    ThisCanvas.Pen := FClearPen;
  if Assigned(AFill) then
    ThisCanvas.Brush := TBrush(AFill.NativeObject)
  else
    ThisCanvas.Brush := FClearBrush;
  try
    // Does not supported
  finally
    ThisCanvas.Pen := vOldPen;
    ThisCanvas.Brush := vOldBrush;
  end;
end;

procedure TVCLPainter.DoDrawPie(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF;
  const AStartAngle, AEndAngle: Single);
var
  vOldPen: TPen;
  vOldBrush: TBrush;
  vCenterPoint: TPointF;
  vRadius: Single;
  vStartPoint: TPointF;
  vEndPoint: TPointF;
  vAngle: Double;
begin
  vOldPen := ThisCanvas.Pen;
  vOldBrush := ThisCanvas.Brush;
  if Assigned(AStroke) then
    ThisCanvas.Pen := TPen(AStroke.NativeObject)
  else
    ThisCanvas.Pen := FClearPen;
  if Assigned(AFill) then
    ThisCanvas.Brush := TBrush(AFill.NativeObject)
  else
    ThisCanvas.Brush := FClearBrush;
  try
    vCenterPoint := ARect.CenterPoint;
    vRadius := Hypot(ARect.Width, ARect.Height);

    vAngle := DegToRad(AEndAngle);
    vStartPoint := ARect.CenterPoint;
    vStartPoint.Offset(vRadius * Cos(vAngle), vRadius * Sin(vAngle));

    vAngle := DegToRad(AStartAngle);
    vEndPoint := ARect.CenterPoint;
    vEndPoint.Offset(vRadius * Cos(vAngle), vRadius * Sin(vAngle));

    ThisCanvas.Pie(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom),
      Round(vStartPoint.X), Round(vStartPoint.Y), Round(vEndPoint.X), Round(vEndPoint.Y));
  finally
    ThisCanvas.Pen := vOldPen;
    ThisCanvas.Brush := vOldBrush;
  end;
end;

procedure TVCLPainter.DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vOldPen: TPen;
  vPoints: TPointArray;
  vOldBrushStyle: TBrushStyle;
begin
  vOldPen := ThisCanvas.Pen;
  vOldBrushStyle := ThisCanvas.Brush.Style;

  vPoints := PPointFToPoints(APoints, ACount);
  ThisCanvas.Pen := TPen(AStroke.NativeObject);
  ThisCanvas.Brush.Style := bsClear;
  try
    ThisCanvas.Polyline(vPoints);
  finally
    SetLength(vPoints, 0);
    ThisCanvas.Pen := vOldPen;
    ThisCanvas.Brush.Style := vOldBrushStyle;
  end;
end;

procedure TVCLPainter.DoDrawRect(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
var
  vRect: TRect;
  vOldPen: TPen;
  vOldBrush: TBrush;
begin
  vRect := ARect.Round;

  if Assigned(AFill) then
    InternalFillRect(AFill, vRect);

  if Assigned(AStroke) then
  begin
    vOldPen := ThisCanvas.Pen;
    vOldBrush := ThisCanvas.Brush;
    ThisCanvas.Pen := TPen(AStroke.NativeObject);
    ThisCanvas.Brush := FClearBrush;
    try
      ThisCanvas.Rectangle(vRect.Left, vRect.Top, vRect.Right, vRect.Bottom);
    finally
      ThisCanvas.Pen := vOldPen;
      ThisCanvas.Brush := vOldBrush;
    end;
  end;
end;

procedure TVCLPainter.DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen; const APoints: PPointF;
  const ACount: Integer);
var
  vOldPen: TPen;
  vOldBrush: TBrush;
  vPoints: TPointArray;
begin
  vPoints := PPointFToPoints(APoints, ACount);

  vOldPen := ThisCanvas.Pen;
  vOldBrush := ThisCanvas.Brush;
  if Assigned(AStroke) then
    ThisCanvas.Pen := TPen(AStroke.NativeObject)
  else
    ThisCanvas.Pen := FClearPen;
  if Assigned(AFill) then
    ThisCanvas.Brush := TBrush(AFill.NativeObject)
  else
    ThisCanvas.Brush := FClearBrush;

  try
    ThisCanvas.Polygon(vPoints);
  finally
    ThisCanvas.Pen := vOldPen;
    ThisCanvas.Brush := vOldBrush;
  end;

  SetLength(vPoints, 0);
end;

procedure TVCLPainter.DoDrawText(const AFont: TStyleFont; const AText: string;
  const ARect: TRectF; const AOptions: Cardinal; const AAngle: Single);
var
  vOldFont: TFontRecall;
  vRect: TRect;
begin
  vOldFont := TFontRecall.Create(ThisCanvas.Font);
  try
    vRect := ARect.Round;
    SetBkMode(ThisCanvas.Handle, 1);
    ThisCanvas.Font.Assign(TFont(AFont.NativeObject));
    if AFont.Angle = 0 then
    begin
      if AOptions and (DT_CENTER or DT_RIGHT) > 0 then
        Windows.DrawText(ThisCanvas.Handle, AText, Length(AText), vRect, AOptions or DT_NOPREFIX)
      else
        Windows.ExtTextOut(ThisCanvas.Handle, vRect.Left, vRect.Top, 0, nil, AText, Length(AText), nil);
        //FContext.TextOut(vRect.Left, vRect.Top, AText);
    end
    else
      ThisCanvas.TextOut(vRect.Left, vRect.Bottom, AText);
  finally
    SetBkMode(ThisCanvas.Handle, 0);
    FreeAndNil(vOldFont);
  end;
end;

procedure TVCLPainter.DoInvertRect(const ARect: TRectF);
var
  vRect: TRect;
begin
  vRect := ARect.Round;
  if IsRectEmpty(vRect) then
    Exit;

  BitBlt(ThisCanvas.Handle, vRect.Left, vRect.Top, vRect.Width, vRect.Height,
    ThisCanvas.Handle, 0, 0, DSTINVERT);
end;

function TVCLPainter.GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF;
var
  vOldFont: TFont;
begin
  vOldFont := ThisCanvas.Font;
  ThisCanvas.Font := TFont(AFont.NativeObject);
  try
    Result := ThisCanvas.TextExtent(AText);
  finally
    ThisCanvas.Font := vOldFont;
  end;
end;

procedure TVCLPainter.InternalFillRect(const AFill: TStyleBrush; const ARect: TRect);
var
  vOldBrush: TBrush;
  vVertexes: array[0..1] of TTriVertex;
  vGradRect: TGradientRect;
  vBlendFunction: TBlendFunction;
  vStartColor: TColor;
  vEndColor: TColor;
begin
  if Assigned(AFill.NativeObject) then
  begin
    vOldBrush := ThisCanvas.Brush;
    ThisCanvas.Brush := TBrush(AFill.NativeObject);
    try
      ThisCanvas.FillRect(ARect);
    finally
      ThisCanvas.Brush := vOldBrush;
    end;
    Exit;
  end;
{$R-}
  if AFill.GradientKind <> gkNone then
  begin
    vStartColor := AFill.Color;
    vEndColor := AFill.BackColor;

    // «аполнение структур дл€ вывода
    vVertexes[0].Red := GetBValue(vStartColor) shl 8;
    vVertexes[0].Green := GetGValue(vStartColor) shl 8;
    vVertexes[0].Blue := GetRValue(vStartColor) shl 8;
    vVertexes[0].Alpha := Byte(vStartColor shr 24) shl 8;
    vVertexes[0].x := ARect.Left;
    vVertexes[0].y := ARect.Top;
    vVertexes[1].Red := GetBValue(vEndColor) shl 8;
    vVertexes[1].Green := GetGValue(vEndColor) shl 8;
    vVertexes[1].Blue := GetRValue(vEndColor) shl 8;
    vVertexes[1].Alpha := Byte(vEndColor shr 24) shl 8;
    vVertexes[1].x := ARect.Right;
    vVertexes[1].y := ARect.Bottom;
    vGradRect.UpperLeft := 0;
    vGradRect.LowerRight := 1;

    if AFill.GradientKind = gkHorizontal then
      GradientFill(ThisCanvas.Handle, @vVertexes[0], 2, @vGradRect, 1, GRADIENT_FILL_RECT_H)
    else
      GradientFill(ThisCanvas.Handle, @vVertexes[0], 2, @vGradRect, 1, GRADIENT_FILL_RECT_V);
  end
  else begin
    vBlendFunction.BlendOp := AC_SRC_OVER;
    vBlendFunction.BlendFlags := 0;
    vBlendFunction.SourceConstantAlpha := Byte(AFill.Color shr 24);
    vBlendFunction.AlphaFormat := 0;

    FBlendBitmap.Canvas.Pixels[0, 0] := CorrectColor(AFill.Color);

    AlphaBlend(ThisCanvas.Handle, ARect.Left, ARect.Top, ARect.Width, ARect.Height,
      FBlendBitmap.Canvas.Handle, 0, 0, 1, 1, vBlendFunction);
  end;
{$R+}
end;

function TVCLPainter.PPointFToPoints(const APoints: PPointF; const ACount: Integer): TPointArray;
var
  i: Integer;
begin
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := PPointF(Integer(APoints) + i * SizeOf(TPointF))^.Round;
end;

function TVCLPainter.ThisCanvas: TCanvas;
begin
  Result := TWinDrawContext(FContext).Canvas;
end;

{function TVCLPainter.SplitText(const AText: string; const AMaxWidth: Integer): TStrings;
var
  i: Integer;
  vText: string;
  vPrevAllowedPos: Integer;
  vWidth: Integer;
  vPos: Integer;
  vCandidate: string;
  vTest: string;
  vLength: Integer;
begin
  Result := TStringList.Create;
  // ќбрабатываем сначала переводы строк
  Result.Text := AText;
  // ј теперь каждую строку по отдельности

  i := 0;
  while i < Result.Count do
  begin
    vText := Result[i];
    vWidth := FContext.TextWidth(vText);
    // “екуща€ строка не помещаетс€ в границы
    if vWidth > AMaxWidth then
    begin
      vPrevAllowedPos := 0;
      vPos := Pos(' ', vText);
      if vPos > 0 then
      begin
        while vPos > 0 do
        begin
          vCandidate := Copy(vText, 1, vPos - 1);
          vWidth := FContext.TextWidth(vCandidate);
          if vWidth > AMaxWidth then
          begin
            if vPrevAllowedPos > 0 then
            begin
              vCandidate := Copy(vText, 1, vPrevAllowedPos - 1);
              Delete(vText, 1, vPrevAllowedPos);
              Result[i] := vCandidate;
              Result.Insert(i+1, vText);
              Break;
            end
            else begin
              // нам не повезло: текст без пробелов, но все равно не влазит
              vLength := Length(vCandidate) - 1;
              repeat
                vTest := Copy(vCandidate, 1, vLength);
                vWidth := FContext.TextWidth(vTest);
                vLength := vLength - 1;
              until (vLength = 0) or (vWidth <= AMaxWidth);
              Delete(vText, 1, vLength);
              Result[i] := vTest;
              Result.Insert(i+1, vText);
              Break;
            end;
          end
          else begin
            vPrevAllowedPos := vPos;
            vPos := PosEx(' ', vText, vPos + 1);
            if vPos = 0 then
            begin
              Delete(vText, 1, vPrevAllowedPos);
              Result[i] := vCandidate;
              Result.Insert(i+1, vText);
            end;
          end;
        end;
      end
      else begin
        // нам не повезло: текст без пробелов, но все равно не влазит
        vLength := Length(vText) - 1;
        repeat
          vTest := Copy(vText, 1, vLength);
          vWidth := FContext.TextWidth(vTest);
          vLength := vLength - 1;
        until (vLength = 0) or (vWidth <= AMaxWidth);
        Delete(vText, 1, vLength);
        Result[i] := vTest;
        Result.Insert(i+1, vText);
      end;
    end;

    i := i + 1;
  end;
end; }

{ TVCLScene }

function TVCLScene.CreatePainter(const AContainer: TObject): TPainter;
var
  vContainer: TDrawContainer absolute AContainer;
begin
  Result := TVCLPainter.Create(AContainer);
  FCachedDrawContext := TWinDrawContext(Result.CreateDrawContext(vContainer.Width, vContainer.Height));
  FDrawContext := TWinDrawContext(Result.CreateDrawContext(vContainer.Width, vContainer.Height));
end;

initialization

TBaseModule.RegisterModule('Painting', 'VCL', TVCLScene);

end.
