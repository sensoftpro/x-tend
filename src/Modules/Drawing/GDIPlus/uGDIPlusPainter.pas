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
  Windows, Types, Classes, Graphics, GDIPOBJ, uDrawStyles, uWinScene, uConsts;

type
  TGDIPlusDrawContext = class(TDrawContext)
  private
    FGPCanvas: TGPGraphics;
    FBitmap: TBitmap;
    FCanvas: TCanvas;
    FHandle: THandle;
    function GetHandle: THandle;
  protected
    procedure DoSetSize(const AWidth, AHeight: Single); override;
  public
    constructor Create(const APainter: TPainter; const AContainer: TDrawContainer; const AWidth, AHeight: Single);
    destructor Destroy; override;

    procedure SaveToFile(const AFileName: string); override;

    property GPCanvas: TGPGraphics read FGPCanvas;
    property Handle: THandle read GetHandle;
    property Canvas: TCanvas read FCanvas;
  end;

  TGPStyleFont = class
  private
    FFont: TGPFont;
    FBrush: TGPBrush;
  public
    constructor Create(const AFont: TGPFont; const ABrush: TGPBrush);
    destructor Destroy; override;

    property Font: TGPFont read FFont;
    property Brush: TGPBrush read FBrush;
  end;

  TGDIPlusPainter = class(TPainter)
  private
    function ThisGPCanvas: TGPGraphics;
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

    procedure CreateBrush(const AFill: TStyleBrush); override;
    procedure CreatePen(const AStroke: TStylePen); override;
    procedure CreateFont(const AFont: TStyleFont); override;
    procedure CreateImage(const AImage: TStyleImage); override;
    function CreateDrawContext(const AWidth, AHeight: Single): TDrawContext; override;
  end;

  TGDIPlusScene = class(TWinCanvasScene)
  protected
    function CreatePainter(const AContainer: TObject): TPainter; override;
  end;

implementation

uses
  SysUtils, PngImage, IOUtils, Math, GDIPAPI, uModule;

{ TGDIPlusPainter }

procedure TGDIPlusPainter.DoClipRect(const ARect: TRectF);
begin
  //TODO
end;

procedure TGDIPlusPainter.DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal);
begin
  TGPSolidBrush(AFill.NativeObject).SetColor(AColor);
end;

procedure TGDIPlusPainter.DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal);
begin
  TGPSolidBrush(TGPStyleFont(AFont.NativeObject).Brush).SetColor(AColor);
end;

procedure TGDIPlusPainter.DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal);
begin
  TGPPen(AStroke.NativeObject).SetColor(AColor);
end;

constructor TGDIPlusPainter.Create(const AContainer: TObject);
var
  vContainer: TDrawContainer absolute AContainer;
begin
  inherited Create(AContainer);

  FContext := TGDIPlusDrawContext.Create(Self, vContainer, vContainer.Width, vContainer.Height);
end;

procedure TGDIPlusPainter.CreateBrush(const AFill: TStyleBrush);
var
  vBrush: TGPBrush;
  vRect: TGPRectF;
  vMode: LinearGradientMode;
  vHatchStyle: THatchStyle;
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

  AFill.NativeObject := vBrush;
end;

function TGDIPlusPainter.CreateDrawContext(const AWidth, AHeight: Single): TDrawContext;
begin
  Result := TGDIPlusDrawContext.Create(Self, nil, AWidth, AHeight);
end;

procedure TGDIPlusPainter.CreateFont(const AFont: TStyleFont);
begin
  AFont.NativeObject := TGPStyleFont.Create(TGPFont.Create(AFont.Family, AFont.Size, AFont.Style),
    TGPSolidBrush.Create(AFont.Color));
end;

procedure TGDIPlusPainter.CreateImage(const AImage: TStyleImage);
var
  vImage: TGraphic;
begin
  if TFile.Exists(AImage.FileName) then
  begin
    if TPath.GetExtension(AImage.FileName) = '.png' then
    begin
      vImage := TPngImage.Create;
      vImage.LoadFromFile(AImage.FileName);
    end
    else begin
      vImage := TBitmap.Create;
      vImage.LoadFromFile(AImage.FileName);
      vImage.Transparent := AImage.Transparent;
    end;
  end
  else
    Exit;

  if AImage.IsNinePatch then
    AImage.NativeObject := TWinNinePatchImage.Create(Self, vImage, AImage.CutRect.Left, AImage.CutRect.Right,
      AImage.CutRect.Top, AImage.CutRect.Bottom)
  else
    AImage.NativeObject := vImage;
end;

procedure TGDIPlusPainter.CreatePen(const AStroke: TStylePen);
var
  vPen: TGPPen;
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

  AStroke.NativeObject := vPen;
end;

function TGDIPlusPainter.ThisGPCanvas: TGPGraphics;
begin
  Result := TGDIPlusDrawContext(FContext).GPCanvas;
end;

function TGDIPlusPainter.GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF;
var
  vResult: TGPRectF;
begin
  ThisGPCanvas.MeasureString(AText, Length(AText), TGPStyleFont(AFont.NativeObject).Font, MakePoint(0.0, 0.0), vResult);
  Result.cx := vResult.Width;
  Result.cy := vResult.Height;
end;

procedure TGDIPlusPainter.DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
begin
  ThisGPCanvas.DrawBeziers(TGPPen(AStroke.NativeObject), PGPPointF(APoints), ACount);
end;

procedure TGDIPlusPainter.DoDrawContext(const AContext: TDrawContext);
begin
  BitBlt(TGDIPlusDrawContext(FContext).Handle, 0, 0, AContext.Width, AContext.Height,
    TGDIPlusDrawContext(AContext).Handle, 0, 0, SRCCOPY);
end;

procedure TGDIPlusPainter.DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
var
  vRect: TGPRectF;
begin
  vRect := MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height);

  if Assigned(AFill) then
    ThisGPCanvas.FillEllipse(TGPBrush(AFill.NativeObject), vRect);

  if Assigned(AStroke) then
    ThisGPCanvas.DrawEllipse(TGPPen(AStroke.NativeObject), vRect);
end;

procedure TGDIPlusPainter.DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
var
  vBitmap: TBitmap absolute AImage;
  vBlendFunction: TBlendFunction;
  vRect: TRect;
begin
  vRect := ARect.Round;
  if Assigned(AImage) and not vRect.IsEmpty then
  begin
    if (AImage is TBitmap) and (vRect.Width = vBitmap.Width) and (vRect.Height = vBitmap.Height) then
    begin
      if SameValue(AOpacity, 1) then
        BitBlt(TGDIPlusDrawContext(FContext).Handle, vRect.Left, vRect.Top, vRect.Width,
          vRect.Height, vBitmap.Canvas.Handle, 0, 0, SRCCOPY)
      else begin
        vBlendFunction.BlendOp := AC_SRC_OVER;
        vBlendFunction.BlendFlags := 0;
        vBlendFunction.SourceConstantAlpha := Round(AOpacity * 255);
        vBlendFunction.AlphaFormat := 0;

        AlphaBlend(TGDIPlusDrawContext(FContext).Handle, vRect.Left, vRect.Top, vRect.Width,
          vRect.Height, vBitmap.Canvas.Handle, 0, 0, vBitmap.Width, vBitmap.Height, vBlendFunction);
      end;
    end
    else
      TGDIPlusDrawContext(FContext).Canvas.StretchDraw(ARect.Round, TGraphic(AImage));
  end;
end;

procedure TGDIPlusPainter.DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF);
begin
  ThisGPCanvas.DrawLine(TGPPen(AStroke.NativeObject), TGPPointF(APoint1), TGPPointF(APoint2));
end;

procedure TGDIPlusPainter.DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject);
var
  vPath: TGPGraphicsPath;
begin
  vPath := TGPGraphicsPath(APath);

  if Assigned(AFill) then
    ThisGPCanvas.FillPath(TGPBrush(AFill.NativeObject), vPath);

  if Assigned(AStroke) then
    ThisGPCanvas.DrawPath(TGPPen(AStroke.NativeObject), vPath);
end;

procedure TGDIPlusPainter.DoDrawPie(const AFill: TStyleBrush; const AStroke: TStylePen;
  const ARect: TRectF; const AStartAngle, AEndAngle: Single);
var
  vRect: TGPRectF;
begin
  vRect := MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height);

  if Assigned(AFill) then
    ThisGPCanvas.FillPie(TGPBrush(AFill.NativeObject), vRect, AStartAngle, AEndAngle - AStartAngle);

  if Assigned(AStroke) then
    ThisGPCanvas.DrawPie(TGPPen(AStroke.NativeObject), vRect, AStartAngle, AEndAngle - AStartAngle);
end;

procedure TGDIPlusPainter.DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
begin
  ThisGPCanvas.DrawLines(TGPPen(AStroke.NativeObject), PGPPointF(APoints), ACount);
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

    ThisGPCanvas.FillRectangle(TGPBrush(AFill.NativeObject), vRect);
  end;

  if Assigned(AStroke) then
    ThisGPCanvas.DrawRectangle(TGPPen(AStroke.NativeObject), vRect);
end;

procedure TGDIPlusPainter.DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen;
  const APoints: PPointF; const ACount: Integer);
begin
  if Assigned(AFill) then
    ThisGPCanvas.FillPolygon(TGPBrush(AFill.NativeObject), PGPPointF(APoints), ACount);

  if Assigned(AStroke) then
    ThisGPCanvas.DrawPolygon(TGPPen(AStroke.NativeObject), PGPPointF(APoints), ACount);
end;

procedure TGDIPlusPainter.DoDrawText(const AFont: TStyleFont; const AText: string;
  const ARect: TRectF; const AOptions: Cardinal; const AAngle: Single);
var
  vStringFormat: TGPStringFormat;
  vMatrix: TGPMatrix;
  vResult: TGPRectF;
begin
  ThisGPCanvas.MeasureString(AText, Length(AText), TGPStyleFont(AFont.NativeObject).Font, MakePoint(0.0, 0.0), vResult);
  vResult.Height := vResult.Height + 0.6;
  vResult.X := ARect.Left + (ARect.Width - vResult.Width) / 2;
  vResult.Y := ARect.Top + (ARect.Height - vResult.Height) / 2;

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
    ThisGPCanvas.SetTextRenderingHint(TextRenderingHintAntiAlias);
    if AAngle <> 0 then
    begin
      vMatrix := TGPMatrix.Create;
      vMatrix.RotateAt(AAngle, MakePoint(vResult.X + vResult.Width / 2, vResult.Y + vResult.Height / 2));
      ThisGPCanvas.SetTransform(vMatrix);
      try
        ThisGPCanvas.DrawString(AText, Length(AText), TGPStyleFont(AFont.NativeObject).Font, vResult, vStringFormat,
          TGPStyleFont(AFont.NativeObject).Brush);
      finally
        ThisGPCanvas.ResetTransform;
      end;
    end
    else
      ThisGPCanvas.DrawString(AText, Length(AText), TGPStyleFont(AFont.NativeObject).Font, vResult, vStringFormat,
        TGPStyleFont(AFont.NativeObject).Brush);
  finally
    FreeAndNil(vStringFormat);
  end;
end;

procedure TGDIPlusPainter.DoInvertRect(const ARect: TRectF);
var
  vRect: TRect;
begin
  vRect := ARect.Round;

  BitBlt(TGDIPlusDrawContext(FContext).Handle, vRect.Left, vRect.Top, vRect.Width, vRect.Height,
    TGDIPlusDrawContext(FContext).Handle, 0, 0, DSTINVERT);
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

{ TGPStyleFont }

constructor TGPStyleFont.Create(const AFont: TGPFont; const ABrush: TGPBrush);
begin
  inherited Create;
  FFont := AFont;
  FBrush := ABrush;
end;

destructor TGPStyleFont.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FBrush);
  inherited Destroy;
end;

{ TGDIPlusDrawContext }

constructor TGDIPlusDrawContext.Create(const APainter: TPainter; const AContainer: TDrawContainer; const AWidth, AHeight: Single);
begin
  inherited Create(AWidth, AHeight);

  if not Assigned(AContainer) then
  begin
    FBitmap := TBitmap.Create;
    FBitmap.PixelFormat := pf32bit;
    FBitmap.SetSize(Round(AWidth), Round(AHeight));
    FCanvas := FBitmap.Canvas;
  end
  else begin
    FBitmap := nil;
    FCanvas := AContainer.Canvas;
  end;

  FHandle := FCanvas.Handle;
  FGPCanvas := TGPGraphics.Create(FHandle);
  FGPCanvas.SetPageUnit(UnitPixel);
  FGPCanvas.SetSmoothingMode(SmoothingModeAntiAlias);
end;

destructor TGDIPlusDrawContext.Destroy;
begin
  FreeAndNil(FGPCanvas);
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TGDIPlusDrawContext.DoSetSize(const AWidth, AHeight: Single);
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.SetSize(Round(AWidth), Round(AHeight));

    if FHandle = FCanvas.Handle then
      Exit;

    FHandle := FCanvas.Handle;

    if Assigned(FGPCanvas) then
      FGPCanvas.Free;

    FGPCanvas := TGPGraphics.Create(FHandle);
    FGPCanvas.SetPageUnit(UnitPixel);
    FGPCanvas.SetSmoothingMode(SmoothingModeAntiAlias);
  end;
end;

function TGDIPlusDrawContext.GetHandle: THandle;
begin
  Result := FCanvas.Handle;
end;

procedure TGDIPlusDrawContext.SaveToFile(const AFileName: string);
begin
  if Assigned(FBitmap) then
    FBitmap.SaveToFile(AFileName);
end;

{ TGDIPlusScene }

function TGDIPlusScene.CreatePainter(const AContainer: TObject): TPainter;
var
  vContainer: TDrawContainer absolute AContainer;
begin
  Result := TGDIPlusPainter.Create(AContainer);
  FCachedDrawContext := TGDIPlusDrawContext(Result.CreateDrawContext(vContainer.Width, vContainer.Height));
  FDrawContext := TGDIPlusDrawContext(Result.CreateDrawContext(vContainer.Width, vContainer.Height));
end;

initialization

TBaseModule.RegisterModule('Painting', 'GDIPlus', TGDIPlusScene);

end.
