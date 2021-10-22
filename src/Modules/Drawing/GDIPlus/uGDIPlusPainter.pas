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

unit uGDIPlusPainter;

interface

uses
  Windows, Types, Classes, Graphics, GDIPAPI, GDIPOBJ, uDrawStyles, uIcon;

type
  TVCLNinePatchImage = class(TNinePatchImage)
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject; override;
  end;

  TBitmapDrawContext = class(TDrawContext)
  private
    FBitmap: TBitmap;
  protected
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    function GetCanvas: TObject; override;
    function GetImage: TObject; override;
  public
    procedure SetSize(const AWidth, AHeight: Single); override;
    procedure SaveToFile(const AFileName: string); override;
  public
    constructor Create(const AWidth, AHeight: Single);
    destructor Destroy; override;
  end;

  TPointArray = array of TPoint;

  TVCLPainter = class(TPainter)
  private
    FContext: TCanvas;
    FClearPen: TPen;
    FClearBrush: TBrush;
    FBlendBitmap: TBitmap;
    function PPointFToPoints(const APoints: PPointF; const ACount: Integer): TPointArray;
    procedure InternalFillRect(const AFill: TStyleBrush; const ARect: TRect);
    function CorrectColor(const AColor: Cardinal): TColor;
    //function SplitText(const AText: string; const AMaxWidth: Integer): TStrings;
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
    function DoDrawText(const AFill: TStyleBrush; const AFont: TStyleFont; const AText: string; const ARect: TRectF;
      const AOptions: Cardinal; const AAngle: Single): TRectF; override;
    function GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF; override;
    procedure DoInvertRect(const ARect: TRectF); override;
    procedure DoDrawPixel(const AColor: Cardinal; const APoint: TPointF); override;
    procedure DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single); override;
    procedure DoDrawContext(const AContext: TDrawContext); override;
    procedure SetCanvas(const Value: TObject); override;

    procedure DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal); override;
    procedure DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal); override;
    procedure DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal); override;
  public
    constructor Create(const AIcons: TIcons); override;
    destructor Destroy; override;

    function CreateBrush(const AFill: TStyleBrush): TObject; override;
    function CreatePen(const AStroke: TStylePen): TObject; override;
    function CreateFont(const AFont: TStyleFont): TObject; override;
    procedure CreateImage(const AImage: TStyleImage); override;
    function CreateDrawContext(const AWidth, AHeight: Single): TDrawContext; override;
  end;

  TGDIPlusPainter = class(TPainter)
  private
    FGPCanvas: TGPGraphics;
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
    function DoDrawText(const AFill: TStyleBrush; const AFont: TStyleFont; const AText: string; const ARect: TRectF;
      const AOptions: Cardinal; const AAngle: Single): TRectF; override;
    function GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF; override;
    procedure DoInvertRect(const ARect: TRectF); override;
    procedure DoDrawPixel(const AColor: Cardinal; const APoint: TPointF); override;
    procedure DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single); override;
    procedure DoDrawContext(const AContext: TDrawContext); override;
    procedure SetCanvas(const Value: TObject); override;

    procedure DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal); override;
    procedure DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal); override;
    procedure DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal); override;
  public
    constructor Create(const AIcons: TIcons); override;
    destructor Destroy; override;

    function CreateBrush(const AFill: TStyleBrush): TObject; override;
    function CreatePen(const AStroke: TStylePen): TObject; override;
    function CreateFont(const AFont: TStyleFont): TObject; override;
    procedure CreateImage(const AImage: TStyleImage); override;
    function CreateDrawContext(const AWidth, AHeight: Single): TDrawContext; override;
  end;

implementation

uses
  StrUtils, SysUtils, PngImage, UITypes, UIConsts, IOUtils, Math, uModule, uConsts;

{ TVCLNinePatchImage }

function TVCLNinePatchImage.CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject;
var
  i: integer;
  vTempBmp: TBitmap;
begin
  if FImage is TPngImage then
  begin
    TPngImage(FImage).CreateAlpha;
    Result := TPngImage.CreateBlank(COLOR_RGBALPHA, 8, AWidth, AHeight);
    BitBlt(TPngImage(Result).Canvas.Handle, 0, 0, AWidth, AHeight,
      TPngImage(FImage).Canvas.Handle, ALeft, ATop, SRCCOPY);
    for i := 0 to AHeight - 1 do
      CopyMemory(TPngImage(Result).AlphaScanline[i],
        PByte(Integer(TPngImage(FImage).AlphaScanline[i + ATop]) + ALeft), AWidth);
  end
  else if FImage is TBitmap then
  begin
    Result := TBitmap.Create;
    TBitmap(Result).SetSize(AWidth, AHeight);
    BitBlt(TBitmap(Result).Canvas.Handle, 0, 0, AWidth, AHeight,
      TBitmap(FImage).Canvas.Handle, ALeft, ATop, SRCCOPY);
  end
  else if FImage is TIcon then
  begin
    Result := TBitmap.Create;
    TBitmap(Result).SetSize(AWidth, AHeight);
    vTempBmp := TBitmap.Create;
    vTempBmp.SetSize(GetWidth, GetHeight);
    vTempBmp.Assign(TGraphic(FImage));
    TBitmap(Result).PixelFormat := vTempBmp.PixelFormat;
    TBitmap(Result).Transparent := vTempBmp.Transparent;
    TBitmap(Result).TransparentColor := vTempBmp.TransparentColor;
    TBitmap(Result).TransparentMode := vTempBmp.TransparentMode;
    TBitmap(Result).AlphaFormat := vTempBmp.AlphaFormat;
    BitBlt(TBitmap(Result).Canvas.Handle, 0, 0, AWidth, AHeight,
      vTempBmp.Canvas.Handle, ALeft, ATop, SRCCOPY);
    FreeAndNil(vTempBmp);
  end
  {else if Source is TGIFImage then
  begin
    Result := TBitmap.Create;
    Result.SetSize(AWidth, AHeight);
    BitBlt(TBitmap(Result).Canvas.Handle, 0, 0, AWidth, AHeight,
      TGIFImage(Source).Bitmap.Canvas.Handle, ALeft, ATop, SRCCOPY);
  end}
  else begin // emf, wmf, jpeg, tiff
    Result := TBitmap.Create;
    TBitmap(Result).SetSize(AWidth, AHeight);
    vTempBmp := TBitmap.Create;
    vTempBmp.SetSize(GetWidth, GetHeight);
    vTempBmp.Canvas.Draw(0, 0, TGraphic(FImage));
    BitBlt(TBitmap(Result).Canvas.Handle, 0, 0, AWidth, AHeight,
      vTempBmp.Canvas.Handle, ALeft, ATop, SRCCOPY);
    FreeAndNil(vTempBmp);
  end;
end;

function TVCLNinePatchImage.GetHeight: Integer;
begin
  Result := TGraphic(FImage).Height;
end;

function TVCLNinePatchImage.GetWidth: Integer;
begin
  Result := TGraphic(FImage).Width;
end;

{ TBitmapDrawContext }

constructor TBitmapDrawContext.Create(const AWidth, AHeight: Single);
begin
  inherited Create;
  FBitmap := TBitmap.Create;
  SetSize(AWidth, AHeight);
  FBitmap.PixelFormat := pf32bit;
end;

destructor TBitmapDrawContext.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

function TBitmapDrawContext.GetCanvas: TObject;
begin
  Result := FBitmap.Canvas;
end;

function TBitmapDrawContext.GetHeight: Integer;
begin
  Result := FBitmap.Height;
end;

function TBitmapDrawContext.GetImage: TObject;
begin
  Result := FBitmap;
end;

function TBitmapDrawContext.GetWidth: Integer;
begin
  Result := FBitmap.Width;
end;

procedure TBitmapDrawContext.SaveToFile(const AFileName: string);
begin
  FBitmap.SaveToFile(AFileName);
end;

procedure TBitmapDrawContext.SetSize(const AWidth, AHeight: Single);
begin
  FBitmap.SetSize(Round(AWidth), Round(AHeight));
end;

{ TGDIPlusPainter }

procedure TGDIPlusPainter.DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal);
begin
  if Assigned(AFill.ColorBrush) then
    TGPSolidBrush(AFill.ColorBrush).SetColor(AColor);
end;

procedure TGDIPlusPainter.DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal);
begin
  //if Assigned(AFont.ColorFont) then
  //  TGPFont(AFont.ColorFont).Color := CorrectColor(AColor);
end;

procedure TGDIPlusPainter.DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal);
begin
  if Assigned(AStroke.ColorPen) then
    TGPPen(AStroke.ColorPen).SetColor(AColor);
end;

constructor TGDIPlusPainter.Create(const AIcons: TIcons);
begin
  inherited Create(AIcons);
  FGPCanvas := nil;
end;

function TGDIPlusPainter.CreateBrush(const AFill: TStyleBrush): TObject;
var
  vBrush: TGPBrush;
  vRect: TGPRectF;
  vMode: LinearGradientMode;
  vHatchStyle: THatchStyle;
begin
  Result := nil;
  if not Assigned(AFill) then
    Exit;

  if AFill.Style <> bsClear then
  begin
    if AFill.Style = bsSolid then
    begin
      if (AFill.GradientKind <> gkNone) and not AFill.Rect.IsEmpty then
      begin
        vRect := MakeRect(AFill.Rect.Left, AFill.Rect.Top, AFill.Rect.Width, AFill.Rect.Height);

        case AFill.GradientKind of
          gkVertical: vMode := LinearGradientModeVertical;
        else
          vMode := LinearGradientModeHorizontal;
        end;

        vBrush := TGPLinearGradientBrush.Create(vRect, AFill.Color, AFill.BackColor, vMode);
      end
      else
        vBrush := TGPSolidBrush.Create(AFill.Color);
    end
    else begin
      case AFill.Style of
        bsHorizontal: vHatchStyle := THatchStyle.HatchStyleHorizontal;
        bsVertical: vHatchStyle := THatchStyle.HatchStyleVertical;
        bsFDiagonal: vHatchStyle := THatchStyle.HatchStyleForwardDiagonal;
        bsBDiagonal: vHatchStyle := THatchStyle.HatchStyleBackwardDiagonal;
        bsCross: vHatchStyle := THatchStyle.HatchStyleCross;
        bsDiagCross: vHatchStyle := THatchStyle.HatchStyleDiagonalCross;
      else
        vHatchStyle := THatchStyle.HatchStyleDiagonalCross
      end;
      vBrush := TGPHatchBrush.Create(vHatchStyle, AFill.Color, AFill.BackColor);
    end;
  end
  else
    vBrush := nil;

  Result := vBrush;
end;

function TGDIPlusPainter.CreateDrawContext(const AWidth, AHeight: Single): TDrawContext;
begin
  Result := TBitmapDrawContext.Create(AWidth, AHeight);
end;

function TGDIPlusPainter.CreateFont(const AFont: TStyleFont): TObject;
begin
  if Assigned(AFont) then
    Result := TGPFont.Create(AFont.Family, AFont.Size, AFont.Style)
  else
    Result := nil;
end;

procedure TGDIPlusPainter.CreateImage(const AImage: TStyleImage);
var
  vImageStream: TStream;
  vImage: TGraphic;
begin
  if Assigned(AImage) then
  begin
    vImageStream := FIcons.ImageByName(AImage.FileName);
    if not Assigned(vImageStream) then
      Exit;

    vImageStream.Position := 0;
    if TPath.GetExtension(AImage.FileName) = '.png' then
    begin
      vImage := TPngImage.Create;
      vImage.LoadFromStream(vImageStream);
    end
    else begin
      vImage := TBitmap.Create;
      vImage.LoadFromStream(vImageStream);
      vImage.Transparent := AImage.Transparent;
    end;

    if AImage.IsNinePatch then
      AImage.Image := TVCLNinePatchImage.Create(vImage, AImage.CutRect.Left, AImage.CutRect.Right,
        AImage.CutRect.Top, AImage.CutRect.Bottom)
    else
      AImage.Image := vImage;
  end;
end;

function TGDIPlusPainter.CreatePen(const AStroke: TStylePen): TObject;
var
  vPen: TGPPen;
begin
  Result := nil;
  if not Assigned(AStroke) then
    Exit;

  if AStroke.Style <> psClear then
  begin
    vPen := TGPPen.Create(AStroke.Color, AStroke.Width);
    vPen.SetLineJoin(LineJoinRound);
    case AStroke.Style of
      psSolid: vPen.SetDashStyle(TDashStyle.DashStyleSolid);
      psDash: vPen.SetDashStyle(TDashStyle.DashStyleDash);
      psDot: vPen.SetDashStyle(TDashStyle.DashStyleDot);
      psDashDot: vPen.SetDashStyle(TDashStyle.DashStyleDashDot);
      psDashDotDot: vPen.SetDashStyle(TDashStyle.DashStyleDashDotDot);
      psInsideFrame: begin
        vPen.SetDashStyle(TDashStyle.DashStyleSolid);
        vPen.SetAlignment(TPenAlignment.PenAlignmentInset);
      end;
      //psUserStyle: vPen.SetDashStyle(TDashStyle.DashStyleCustom);
    else
      vPen.SetDashStyle(TDashStyle.DashStyleSolid);
    end;
  end
  else
    vPen := nil;

  Result := vPen;
end;

destructor TGDIPlusPainter.Destroy;
begin
  if Assigned(FGPCanvas) then
    FreeAndNil(FGPCanvas);
  inherited Destroy;
end;

procedure TGDIPlusPainter.SetCanvas(const Value: TObject);
begin
  inherited SetCanvas(Value);

  if Assigned(FGPCanvas) then
    FreeAndNil(FGPCanvas);

  if not Assigned(Value) then
    Exit;

  FGPCanvas := TGPGraphics.Create(TCanvas(Value).Handle);
  FGPCanvas.SetPageUnit(UnitPixel);
  FGPCanvas.SetSmoothingMode(SmoothingModeAntiAlias);
end;

function TGDIPlusPainter.GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF;
var
  vResult: TGPRectF;
begin
  if not Assigned(AFont) then
  begin
    Result.cx := 0;
    Result.cy := 0;
    Exit;
  end;

  FGPCanvas.MeasureString(AText, Length(AText), TGPFont(AFont.Font), MakePoint(0.0, 0.0), vResult);
  Result.cx := vResult.Width;
  Result.cy := vResult.Height;
end;

procedure TGDIPlusPainter.DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
begin
  if Assigned(AStroke) then
    FGPCanvas.DrawBeziers(TGPPen(AStroke.Pen), PGPPointF(APoints), ACount);
end;

procedure TGDIPlusPainter.DoDrawContext(const AContext: TDrawContext);
begin
  BitBlt(TCanvas(Canvas).Handle, 0, 0, AContext.Width, AContext.Height,
    TCanvas(AContext.Canvas).Handle, 0, 0, SRCCOPY);
end;

procedure TGDIPlusPainter.DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
var
  vRect: TGPRectF;
begin
  vRect := MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height);

  if Assigned(AFill) then
    FGPCanvas.FillEllipse(TGPBrush(AFill.Brush), vRect);

  if Assigned(AStroke) and (AStroke.Width > 0.3) then
    FGPCanvas.DrawEllipse(TGPPen(AStroke.Pen), vRect);
end;

procedure TGDIPlusPainter.DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
var
  vBitmap: TBitmap absolute AImage;
  vBlendFunction: TBlendFunction;
  vRect: TRect;
begin
  if SameValue(AOpacity, 0) then
    Exit;

  vRect := ARect.Round;
  if Assigned(AImage) and not vRect.IsEmpty then
  begin
    if (AImage is TBitmap) and (vRect.Width = vBitmap.Width) and (vRect.Height = vBitmap.Height) then
    begin
      if SameValue(AOpacity, 1) then
        BitBlt(TCanvas(FCanvas).Handle, vRect.Left, vRect.Top, vRect.Width,
          vRect.Height, vBitmap.Canvas.Handle, 0, 0, SRCCOPY)
      else begin
        vBlendFunction.BlendOp := AC_SRC_OVER;
        vBlendFunction.BlendFlags := 0;
        vBlendFunction.SourceConstantAlpha := Round(AOpacity * 255);
        vBlendFunction.AlphaFormat := 0;

        AlphaBlend(TCanvas(FCanvas).Handle, vRect.Left, vRect.Top, vRect.Width,
          vRect.Height, vBitmap.Canvas.Handle, 0, 0, vBitmap.Width, vBitmap.Height, vBlendFunction);
      end;
    end
    else
      TCanvas(FCanvas).StretchDraw(ARect.Round, TGraphic(AImage));
  end;
end;

procedure TGDIPlusPainter.DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF);
begin
  if Assigned(AStroke) then
    FGPCanvas.DrawLine(TGPPen(AStroke.Pen), TGPPointF(APoint1), TGPPointF(APoint2));
end;

procedure TGDIPlusPainter.DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject);
var
  vPath: TGPGraphicsPath;
begin
  vPath := TGPGraphicsPath(APath);

  if Assigned(AFill) then
    FGPCanvas.FillPath(TGPBrush(AFill.Brush), vPath);

  if Assigned(AStroke.Pen) and (AStroke.Width > 0.3) then
    FGPCanvas.DrawPath(TGPPen(AStroke.Pen), vPath);
end;

procedure TGDIPlusPainter.DoDrawPie(const AFill: TStyleBrush; const AStroke: TStylePen;
  const ARect: TRectF; const AStartAngle, AEndAngle: Single);
var
  vRect: TGPRectF;
begin
  vRect := MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height);

  if Assigned(AFill) then
    FGPCanvas.FillPie(TGPBrush(AFill.Brush), vRect, AStartAngle, AEndAngle - AStartAngle);

  if Assigned(AStroke) and (AStroke.Width > 0.3) then
    FGPCanvas.DrawPie(TGPPen(AStroke.Pen), vRect, AStartAngle, AEndAngle - AStartAngle);
end;

procedure TGDIPlusPainter.DoDrawPixel(const AColor: Cardinal; const APoint: TPointF);
begin
  //FGPCanvas.SetPixel(Round(APoint.X), Round(APoint.Y), AColor);
  SetPixel(TCanvas(FCanvas).Handle, Round(APoint.X), Round(APoint.Y), CorrectColor(AColor));
end;

procedure TGDIPlusPainter.DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
begin
  if Assigned(AStroke) then
    FGPCanvas.DrawLines(TGPPen(AStroke.Pen), PGPPointF(APoints), ACount);
end;

procedure TGDIPlusPainter.DoDrawRect(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
var
  vRect: TGPRectF;
begin
  vRect := MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height);

  if Assigned(AFill) then
  begin
    if AFill.GradientKind <> gkNone then
    begin
      if ((AFill.GradientKind = gkHorizontal) and (ARect.Width <> AFill.Rect.Width))
        or ((AFill.GradientKind = gkVertical) and (ARect.Height <> AFill.Rect.Height))
      then begin
        AFill.Rect := ARect;
        CreateBrush(AFill);
      end;
    end;

    FGPCanvas.FillRectangle(TGPBrush(AFill.Brush), vRect);
  end;

  if Assigned(AStroke) and (AStroke.Width > 0.3) then
    FGPCanvas.DrawRectangle(TGPPen(AStroke.Pen), vRect);
end;

procedure TGDIPlusPainter.DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen;
  const APoints: PPointF; const ACount: Integer);
begin
  if Assigned(AFill) then
    FGPCanvas.FillPolygon(TGPBrush(AFill.Brush), PGPPointF(APoints), ACount);

  if Assigned(AStroke) and (AStroke.Width > 0.3) then
    FGPCanvas.DrawPolygon(TGPPen(AStroke.Pen), PGPPointF(APoints), ACount);
end;

function TGDIPlusPainter.DoDrawText(const AFill: TStyleBrush; const AFont: TStyleFont;
  const AText: string; const ARect: TRectF; const AOptions: Cardinal; const AAngle: Single): TRectF;
var
  vStringFormat: TGPStringFormat;
  vMatrix: TGPMatrix;
  vResult: TGPRectF;
begin
  Result := TRectF.Empty;

  if not Assigned(AFill) or not Assigned(AFont) then
    Exit;

  if AFont.Size < 3 then
    Exit;

  FGPCanvas.MeasureString(AText, Length(AText), TGPFont(AFont.Font), MakePoint(0.0, 0.0), vResult);
  vResult.Height := vResult.Height + 0.6;
  vResult.X := ARect.Left + (ARect.Width - vResult.Width) / 2;
  vResult.Y := ARect.Top + (ARect.Height - vResult.Height) / 2;

  Result := RectF(vResult.X, vResult.Y, vResult.X + vResult.Width, vResult.Y + vResult.Height);

  vStringFormat := TGPStringFormat.Create;
  if DT_CENTER and AOptions > 0 then
    vStringFormat.SetAlignment(StringAlignmentCenter)
  else if DT_RIGHT and AOptions > 0 then
    vStringFormat.SetAlignment(StringAlignmentFar)
  else
    vStringFormat.SetAlignment(StringAlignmentNear);

  if DT_VCENTER and AOptions > 0 then
    vStringFormat.SetLineAlignment(StringAlignmentCenter)
  else if DT_BOTTOM and AOptions > 0 then
    vStringFormat.SetLineAlignment(StringAlignmentFar)
  else
    vStringFormat.SetLineAlignment(StringAlignmentNear);

  try
    FGPCanvas.SetTextRenderingHint(TextRenderingHintAntiAlias);
    if AAngle <> 0 then
    begin
      vMatrix := TGPMatrix.Create;
      vMatrix.RotateAt(AAngle, MakePoint(vResult.X + vResult.Width / 2, vResult.Y + vResult.Height / 2));
      FGPCanvas.SetTransform(vMatrix);
      try
        FGPCanvas.DrawString(AText, Length(AText), TGPFont(AFont.Font), vResult, vStringFormat, TGPBrush(AFill.Brush));
      finally
        FGPCanvas.ResetTransform;
      end;
    end
    else
      FGPCanvas.DrawString(AText, Length(AText), TGPFont(AFont.Font), vResult, vStringFormat, TGPBrush(AFill.Brush));
  finally
    FreeAndNil(vStringFormat);
  end;
end;

procedure TGDIPlusPainter.DoInvertRect(const ARect: TRectF);
var
  vRect: TRect;
begin
  vRect := ARect.Round;
  if IsRectEmpty(vRect) then
    Exit;

  BitBlt(TCanvas(FCanvas).Handle, vRect.Left, vRect.Top, vRect.Width, vRect.Height,
    TCanvas(FCanvas).Handle, 0, 0, DSTINVERT);
end;

//******************************************************************************
//  Рисуем текст GDI+
//******************************************************************************
{function DrawGDIPlusText (ACanvas : TCanvas; ARect : TRect; Angle, ASize : double; AText : string; AZoom : double = 1) : boolean;
var clr : TColor;
    grp : TGPGraphics;
    brh : TGPSolidBrush;
    nam : TGPFontFamily;
    fsl : FontStyle;
    fnt : TGPFont;
    pnt : TGPPointF;
begin
  grp := TGPGraphics.Create(ACanvas.Handle);
  try
    with ACanvas do begin
      clr := Font.Color;
      //-- создаем название шрифта ---------------------------------------------
      nam := TGPFontFamily.Create(Font.Name);
      //-- определяем стиль шрифта ---------------------------------------------
      fsl := FontStyleRegular;
      if fsBold in Font.Style then fsl := fsl + FontStyleBold;
      if fsItalic in Font.Style then fsl := fsl + FontStyleItalic;
      if fsUnderline in Font.Style then fsl := fsl + FontStyleUnderline;
      if fsStrikeOut in Font.Style then fsl := fsl + FontStyleStrikeout;
      //-- создаем кисть для шрифта, цвет шрифта -------------------------------
      brh := TGPSolidBrush.Create(MakeColor(GetRValue(clr),GetGValue(clr),GetBValue(clr)));
      //-- создаем шрифт без масштаба, в "родном" размере ----------------------
      Fnt := TGPFont.Create(nam, ASize * Font.PixelsPerInch / 72, fsl, UnitPixel);
      //-- устанавливаем антиалиасинг с "растягиванием" по расчетной ширине ----
      grp.SetTextRenderingHint(TextRenderingHintAntiAlias);
      //-- готовим точку начала отрисовки --------------------------------------
      pnt := MakePoint(ARect.Left*1.0, ARect.Top*1.0);
      //-- точка трансформации, если угол, то вращение будет вокруг этих координат
      grp.TranslateTransform(pnt.X,pnt.y);
      //-- если указан угол, применяем трансформацию вращения ------------------
      if Angle <> 0 then begin
        //-- применяем трансформацию вращения ----------------------------------
        grp.RotateTransform(Angle);
      end;
      //-- рисуем текст теперь от начала "новых" координат -------------------
      pnt := MakePoint(0.0,0.0);
      //-- если указан масштаб, применяем трансформацию масштаба ------------------
      if AZoom <> 1 then begin
        grp.ScaleTransform(AZoom,AZoom);
      end;
      //-- рисуем текст без указания длины -------------------------------------
      grp.DrawString(AText, -1, Fnt, pnt, brh);
    end;
  except
    result := false;
  end;
  Fnt.free;
  brh.free;
  nam.free;
  grp.free;
end; }

{ TVCLPainter }

procedure TVCLPainter.DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal);
begin
  if Assigned(AFill.ColorBrush) then
    TBrush(AFill.ColorBrush).Color := CorrectColor(AColor);
end;

procedure TVCLPainter.DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal);
begin
  if Assigned(AFont.ColorFont) then
    TFont(AFont.ColorFont).Color := CorrectColor(AColor);
end;

procedure TVCLPainter.DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal);
begin
  if Assigned(AStroke.ColorPen) then
    TPen(AStroke.ColorPen).Color := CorrectColor(AColor);
end;

function TVCLPainter.CorrectColor(const AColor: Cardinal): TColor;
begin
  Result := TColor(RGB(GetBValue(AColor), GetGValue(AColor), GetRValue(AColor)));
end;

constructor TVCLPainter.Create(const AIcons: TIcons);
begin
  inherited Create(AIcons);

  FContext := nil;

  FClearPen := TPen.Create;
  FClearPen.Style := Graphics.psClear;
  FClearBrush := TBrush.Create;
  FClearBrush.Style := Graphics.bsClear;

  FBlendBitmap := TBitmap.Create;
  FBlendBitmap.PixelFormat := pf32bit;
  FBlendBitmap.SetSize(1, 1);
end;

function TVCLPainter.CreateBrush(const AFill: TStyleBrush): TObject;
var
  vBrush: TBrush;
begin
  Result := nil;
  if not Assigned(AFill) then
    Exit;

  if (AFill.Style <> bsClear) and (AFill.GradientKind = gkNone)
    and (AFill.Color and $FF000000 = $FF000000) then
  begin
    vBrush := TBrush.Create;
    vBrush.Color := CorrectColor(AFill.Color);
    vBrush.Style := Graphics.TBrushStyle(AFill.Style);
  end
  else
    vBrush := nil;

  Result := vBrush;
end;

function TVCLPainter.CreateDrawContext(const AWidth, AHeight: Single): TDrawContext;
begin
  Result := TBitmapDrawContext.Create(AWidth, AHeight);
end;

function TVCLPainter.CreateFont(const AFont: TStyleFont): TObject;
var
  vFont: TFont;
begin
  if Assigned(AFont) then
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

    Result := vFont;
  end
  else
    Result := nil;
end;

procedure TVCLPainter.CreateImage(const AImage: TStyleImage);
var
  vImageStream: TStream;
  vImage: TGraphic;
begin
  if Assigned(AImage) then
  begin
    vImageStream := FIcons.ImageByName(AImage.FileName);
    if not Assigned(vImageStream) then
      Exit;

    vImageStream.Position := 0;
    if TPath.GetExtension(AImage.FileName) = '.png' then
    begin
      vImage := TPngImage.Create;
      vImage.LoadFromStream(vImageStream);
    end
    else begin
      vImage := TBitmap.Create;
      vImage.LoadFromStream(vImageStream);
      vImage.Transparent := AImage.Transparent;
    end;

    if AImage.IsNinePatch then
      AImage.Image := TVCLNinePatchImage.Create(vImage, AImage.CutRect.Left, AImage.CutRect.Right,
        AImage.CutRect.Top, AImage.CutRect.Bottom)
    else
      AImage.Image := vImage;
  end;
end;

function TVCLPainter.CreatePen(const AStroke: TStylePen): TObject;
var
  vPen: TPen;
begin
  Result := nil;
  if not Assigned(AStroke) then
    Exit;

  if AStroke.Style <> psClear then
  begin
    vPen := TPen.Create;
    vPen.Color := CorrectColor(AStroke.Color);
    vPen.Width := Round(AStroke.Width);
    vPen.Style :=  Graphics.TPenStyle(AStroke.Style);
  end
  else
    vPen := nil;

  Result := vPen;
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
  vOldPen := FContext.Pen;

  vPoints := PPointFToPoints(APoints, ACount);
  FContext.Pen := TPen(AStroke.Pen);
  try
    FContext.PolyBezier(vPoints);
  finally
    SetLength(vPoints, 0);
    FContext.Pen := vOldPen;
  end;
end;

procedure TVCLPainter.DoDrawContext(const AContext: TDrawContext);
begin
  BitBlt(FContext.Handle, 0, 0, AContext.Width, AContext.Height,
    TCanvas(AContext.Canvas).Handle, 0, 0, SRCCOPY);
end;

procedure TVCLPainter.DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
var
  vOldPen: TPen;
  vOldBrush: TBrush;
begin
  vOldPen := FContext.Pen;
  vOldBrush := FContext.Brush;
  if Assigned(AStroke) then
    FContext.Pen := TPen(AStroke.Pen)
  else
    FContext.Pen := FClearPen;
  if Assigned(AFill) then
    FContext.Brush := TBrush(AFill.Brush)
  else
    FContext.Brush := FClearBrush;
  try
    FContext.Ellipse(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  finally
    FContext.Pen := vOldPen;
    FContext.Brush := vOldBrush;
  end;
end;

procedure TVCLPainter.DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
var
  vBitmap: TBitmap absolute AImage;
  vBlendFunction: TBlendFunction;
  vRect: TRect;
begin
  if SameValue(AOpacity, 0) then
    Exit;

  vRect := ARect.Round;
  if Assigned(AImage) and not vRect.IsEmpty then
  begin
    if (AImage is TBitmap) and (vRect.Width = vBitmap.Width) and (vRect.Height = vBitmap.Height) then
    begin
      if SameValue(AOpacity, 1) then
        BitBlt(TCanvas(FCanvas).Handle, vRect.Left, vRect.Top, vRect.Width,
          vRect.Height, vBitmap.Canvas.Handle, 0, 0, SRCCOPY)
      else begin
        vBlendFunction.BlendOp := AC_SRC_OVER;
        vBlendFunction.BlendFlags := 0;
        vBlendFunction.SourceConstantAlpha := Round(AOpacity * 255);
        vBlendFunction.AlphaFormat := 0;

        AlphaBlend(TCanvas(FCanvas).Handle, vRect.Left, vRect.Top, vRect.Width,
          vRect.Height, vBitmap.Canvas.Handle, 0, 0, vBitmap.Width, vBitmap.Height, vBlendFunction);
      end;
    end
    else
      TCanvas(FCanvas).StretchDraw(ARect.Round, TGraphic(AImage));
  end;
end;

procedure TVCLPainter.DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF);
var
  vOldPen: TPen;
begin
  vOldPen := FContext.Pen;
  FContext.Pen := TPen(AStroke.Pen);
  try
    FContext.MoveTo(Round(APoint1.X), Round(APoint1.Y));
    FContext.LineTo(Round(APoint2.X), Round(APoint2.Y));
  finally
    FContext.Pen := vOldPen;
  end;
end;

procedure TVCLPainter.DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject);
var
  vOldPen: TPen;
  vOldBrush: TBrush;
begin
  vOldPen := FContext.Pen;
  vOldBrush := FContext.Brush;
  if Assigned(AStroke) then
    FContext.Pen := TPen(AStroke.Pen)
  else
    FContext.Pen := FClearPen;
  if Assigned(AFill) then
    FContext.Brush := TBrush(AFill.Brush)
  else
    FContext.Brush := FClearBrush;
  try
    // Does not supported
  finally
    FContext.Pen := vOldPen;
    FContext.Brush := vOldBrush;
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
  vOldPen := FContext.Pen;
  vOldBrush := FContext.Brush;
  if Assigned(AStroke) then
    FContext.Pen := TPen(AStroke.Pen)
  else
    FContext.Pen := FClearPen;
  if Assigned(AFill) then
    FContext.Brush := TBrush(AFill.Brush)
  else
    FContext.Brush := FClearBrush;
  try
    vCenterPoint := ARect.CenterPoint;
    vRadius := Hypot(ARect.Width, ARect.Height);

    vAngle := DegToRad(AStartAngle);
    vStartPoint := ARect.CenterPoint;
    vStartPoint.Offset(vRadius * Sin(vAngle), vRadius * Cos(vAngle));

    vAngle := DegToRad(AEndAngle);
    vEndPoint := ARect.CenterPoint;
    vEndPoint.Offset(vRadius * Sin(vAngle), vRadius * Cos(vAngle));

    FContext.Pie(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom),
      Round(vStartPoint.X), Round(vStartPoint.Y), Round(vEndPoint.X), Round(vEndPoint.Y));
  finally
    FContext.Pen := vOldPen;
    FContext.Brush := vOldBrush;
  end;
end;

procedure TVCLPainter.DoDrawPixel(const AColor: Cardinal; const APoint: TPointF);
begin
  SetPixel(FContext.Handle, Round(APoint.X), Round(APoint.Y), CorrectColor(AColor));
end;

procedure TVCLPainter.DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vOldPen: TPen;
  vPoints: TPointArray;
begin
  vOldPen := FContext.Pen;

  vPoints := PPointFToPoints(APoints, ACount);
  FContext.Pen := TPen(AStroke.Pen);
  try
    FContext.Polyline(vPoints);
  finally
    SetLength(vPoints, 0);
    FContext.Pen := vOldPen;
  end;
end;

procedure TVCLPainter.DoDrawRect(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
var
  vRect: TRect;
  vOldPen: TPen;
  vOldBrush: TBrush;
begin
  vRect := ARect.Round;

  if vRect.IsEmpty then
    Exit;

  if Assigned(AFill) then
    InternalFillRect(AFill, vRect);

  if Assigned(AStroke) then
  begin
    vOldPen := FContext.Pen;
    vOldBrush := FContext.Brush;
    FContext.Pen := TPen(AStroke.Pen);
    FContext.Brush := FClearBrush;
    try
      FContext.Rectangle(vRect.Left, vRect.Top, vRect.Right, vRect.Bottom);
    finally
      FContext.Pen := vOldPen;
      FContext.Brush := vOldBrush;
    end;
  end;
end;

procedure TVCLPainter.DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen; const APoints: PPointF;
  const ACount: Integer);
var
  vOldPen: TPen;
  vPoints: TPointArray;
  vRegion: HRGN;
  vRgnRect: TRect;
begin
  vPoints := PPointFToPoints(APoints, ACount);

  if Assigned(AFill) then
  begin
    vRegion := CreatePolygonRgn(APoints, SizeOf(vPoints) div SizeOf(TPoint), WINDING);
    GetRgnBox(vRegion, vRgnRect);

    SelectClipRgn(FContext.Handle, vRegion);
    try
      InternalFillRect(AFill, vRgnRect);
    finally
      SelectClipRgn(FContext.Handle, 0);
      DeleteObject(vRegion);
    end;
  end;

  if Assigned(AStroke) then
  begin
    vOldPen := FContext.Pen;
    FContext.Pen := TPen(AStroke.Pen);
    try
      FContext.Polygon(vPoints);
    finally
      FContext.Pen := vOldPen;
    end;
  end;

  SetLength(vPoints, 0);
end;

function TVCLPainter.DoDrawText(const AFill: TStyleBrush; const AFont: TStyleFont; const AText: string;
  const ARect: TRectF; const AOptions: Cardinal; const AAngle: Single): TRectF;
var
  vOldFont: TFontRecall;
  vRect: TRect;
begin
  Result := TRectF.Empty;

  if not Assigned(AFont) then
    Exit;

  vOldFont := TFontRecall.Create(FContext.Font);
  try
    vRect := ARect.Round;
    SetBkMode(FContext.Handle, 1);
    FContext.Font.Assign(TFont(AFont.Font));
    if AFont.Angle = 0 then
    begin
      if AOptions and (DT_CENTER or DT_RIGHT) > 0 then
        Windows.DrawText(FContext.Handle, AText, Length(AText), vRect, AOptions or DT_NOPREFIX)
      else
        Windows.ExtTextOut(FContext.Handle, vRect.Left, vRect.Top, 0, nil, AText, Length(AText), nil);
        //FContext.TextOut(vRect.Left, vRect.Top, AText);
    end
    else
      FContext.TextOut(vRect.Left, vRect.Bottom, AText);
    Result := vRect;
  finally
    SetBkMode(FContext.Handle, 0);
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

  BitBlt(TCanvas(FCanvas).Handle, vRect.Left, vRect.Top, vRect.Width, vRect.Height,
    TCanvas(FCanvas).Handle, 0, 0, DSTINVERT);
end;

function TVCLPainter.GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF;
var
  vOldFont: TFont;
begin
  vOldFont := FContext.Font;
  FContext.Font := TFont(AFont.Font);
  try
    Result := FContext.TextExtent(AText);
  finally
    FContext.Font := vOldFont;
  end;
end;

procedure TVCLPainter.InternalFillRect(const AFill: TStyleBrush; const ARect: TRect);
var
  vOldBrush: TBrush;
  vVertexes: array[0..1] of TTriVertex;
  vGradRect: TGradientRect;
  vBlendFunction: TBlendFunction;
  vStartColor: Cardinal;
  vEndColor: Cardinal;
begin
  if Assigned(AFill.Brush) then
  begin
    vOldBrush := FContext.Brush;
    FContext.Brush := TBrush(AFill.Brush);
    try
      FContext.FillRect(ARect);
    finally
      FContext.Brush := vOldBrush;
    end;
    Exit;
  end;

  if AFill.GradientKind <> gkNone then
  begin
    vStartColor := AFill.Color;
    vEndColor := AFill.BackColor;

    // Заполнение структур для вывода
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
      GradientFill(FContext.Handle, @vVertexes[0], 2, @vGradRect, 1, GRADIENT_FILL_RECT_H)
    else
      GradientFill(FContext.Handle, @vVertexes[0], 2, @vGradRect, 1, GRADIENT_FILL_RECT_V);
  end
  else begin
    vBlendFunction.BlendOp := AC_SRC_OVER;
    vBlendFunction.BlendFlags := 0;
    vBlendFunction.SourceConstantAlpha := Byte(AFill.Color shr 24);
    vBlendFunction.AlphaFormat := 0;

    FBlendBitmap.Canvas.Pixels[0, 0] := CorrectColor(AFill.Color);

    AlphaBlend(FContext.Handle, ARect.Left, ARect.Top, ARect.Width, ARect.Height,
      FBlendBitmap.Canvas.Handle, 0, 0, 1, 1, vBlendFunction);
  end;
end;

function TVCLPainter.PPointFToPoints(const APoints: PPointF; const ACount: Integer): TPointArray;
var
  i: Integer;
begin
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := PPointF(Integer(APoints) + i * SizeOf(TPointF))^.Round;
end;

procedure TVCLPainter.SetCanvas(const Value: TObject);
begin
  inherited SetCanvas(Value);

  FContext := TCanvas(Value);
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
  // Обрабатываем сначала переводы строк
  Result.Text := AText;
  // А теперь каждую строку по отдельности

  i := 0;
  while i < Result.Count do
  begin
    vText := Result[i];
    vWidth := FContext.TextWidth(vText);
    // Текущая строка не помещается в границы
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

initialization

TBaseModule.RegisterModule('Painting', 'GDIPlus', TGDIPlusPainter);
TBaseModule.RegisterModule('Painting', 'VCL', TVCLPainter);

end.
