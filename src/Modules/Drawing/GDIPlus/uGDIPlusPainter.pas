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
  Windows, Types, Classes, Graphics, GDIPOBJ, uDrawStyles, uScene, uWinScene, uConsts, Vcl.Controls;

type
  TGDIPlusDrawContext = class(TDrawContext)
  private
    FGPCanvas: TGPGraphics;
    FGPBitmap: TGPBitmap;
    FGPCachedBitmap: TGPCachedBitmap;
    FBitmap: TBitmap;
    FHBitmap: HBITMAP;
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
    property GPCachedBitmap: TGPCachedBitmap read FGPCachedBitmap;
    property Bitmap: HBITMAP read FHBitmap;
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
    FGPCanvas: TGPGraphics;
    FDC: HDC;
    FGPBitmap: TGPBitmap;
    FStaticContext: TGDIPlusDrawContext;
    FContainer: TDrawContainer;

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

  TGDIPlusScene = class(TScene)
  private
    FStaticContext: TDrawContext;
    FDynamicContext: TDrawContext;
    FPanel: TMyPanel;
    FDrawBufferDC: HDC;
  protected
    procedure DoActivate; override;
    procedure SetEnabled(const AValue: Boolean); override;
    function DoCreateScene(const APlaceholder: TObject): TPainter; override;
    procedure DoDestroyScene; override;
    function GetSceneRect: TRectF; override;
    function GetClientPos: TPointF; override;

    function CreatePainter(const AContainer: TObject): TPainter; override;
    procedure UpdateContexts(const AWidth, AHeight: Single); override;
  public
    procedure DoRender(const ANeedFullRepaint: Boolean); override;
  end;

implementation

uses
  SysUtils, PngImage, IOUtils, Math, GDIPAPI, uModule;

{ TGDIPlusPainter }

destructor TGDIPlusPainter.Destroy;
begin
  inherited;
  FContainer.Free;
  FreeAndNil(FContext);

end;

procedure TGDIPlusPainter.DoClipRect(const ARect: TRectF);
var
  vRect: TGPRectF;
begin
  vRect := MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
  ThisGPCanvas.SetClip(vRect, CombineModeReplace);
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

procedure TGDIPlusPainter.BeginPaint;
var
  vDC: HDC;
  vBitmap: HBITMAP;
begin
  inherited;
  if not Assigned(FContext) then
    FContext := TGDIPlusDrawContext.Create(Self, FContainer, FContainer.Width, FContainer.Height);
  FGPCanvas := TGPGraphics.Create((TGDIPlusDrawContext(FContext).Handle));
  FGPCanvas.SetPageUnit(UnitPixel);
  FGPCanvas.SetSmoothingMode(SmoothingModeAntiAlias);



//  FDrawContext := TGDIPlusDrawContext(FContext);

end;

constructor TGDIPlusPainter.Create(const AContainer: TObject);
var
  vContainer: TDrawContainer absolute AContainer;
begin
  inherited Create(AContainer);
  FContainer := TDrawContainer.Create(vContainer.HWND, vContainer.Canvas, vContainer.Width, vContainer.Height);
  FContext := TGDIPlusDrawContext.Create(self, vContainer, vContainer.Width, vContainer.Height);
  FDC := TGDIPlusDrawContext(FContext).Handle;
//  FContext := TGDIPlusDrawContext.Create(Self, vContainer, vContainer.Width, vContainer.Height);
//  FGPCanvas := TGDIPlusDrawContext(FContext).GPCanvas;
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
      bsCross: vHatchStyle := HatchStyleCross;
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
  vDashPattern: TArray<Single>;
begin
  vPen := TGPPen.Create(AStroke.Color, AStroke.Width);
  vPen.SetLineJoin(LineJoinRound);
  vDashPattern := TArray<Single>.Create(3,3);

  case AStroke.Style of
    psSolid: vPen.SetDashStyle(DashStyleSolid);
    psDash: vPen.SetDashStyle(DashStyleDash);
    psDot: vPen.SetDashPattern(PSingle(vDashPattern), 2);
    psDashDot: vPen.SetDashStyle(DashStyleDashDot);
    psDashDotDot: vPen.SetDashStyle(DashStyleDashDotDot);
    psInsideFrame: begin
      vPen.SetDashStyle(DashStyleSolid);
      vPen.SetAlignment(PenAlignmentInset);
    end;
    //psUserStyle: vPen.SetDashStyle(TDashStyle.DashStyleCustom);
  else
    vPen.SetDashStyle(DashStyleSolid);
  end;

  AStroke.NativeObject := vPen;
end;

function TGDIPlusPainter.ThisGPCanvas: TGPGraphics;
begin
  Result := FGPCanvas;
end;

function TGDIPlusPainter.GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF;
var
  vResult: TGPRectF;
begin
  ThisGPCanvas.MeasureString(AText, Length(AText), TGPStyleFont(AFont.NativeObject).Font, MakePoint(0.0, 0.0), vResult);
  Result.cx := vResult.Width;
  Result.cy := vResult.Height;
end;

function TGDIPlusPainter.SetContext(const AContext: TDrawContext): TDrawContext;
begin
  Result := FContext;
  FContext := AContext;

//  FGPCanvas := TGPGraphics.Create(TGDIPlusDrawContext(AContext).Handle);
//  FGPCanvas.SetPageUnit(UnitPixel);
//  FGPCanvas.SetSmoothingMode(SmoothingModeAntiAlias);
end;

procedure TGDIPlusPainter.DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vPoints: TArray<TPointF> absolute APoints;
  vGPPoints: TArray<TGPPointF>;
  i: Integer;
begin
  SetLength(vGPPoints, 4);
  i := 0;
  while i < Length(vPoints) do
  begin
    vGPPoints[0] := TGPPointF(vPoints[0 + (Round(i/3) * 3)]);
    vGPPoints[1] := TGPPointF(vPoints[i + 1]);
    vGPPoints[2] := TGPPointF(vPoints[i + 2]);
    vGPPoints[3] := TGPPointF(vPoints[i + 3]);
    ThisGPCanvas.DrawBeziers(TGPPen(AStroke.NativeObject), PGPPointF(vGPPoints), 4);
    Inc(i,3);
  end;
end;

procedure TGDIPlusPainter.DoDrawContext(const AContext: TDrawContext);
var
  vFlag: TStatus;
  vError: DWORD;
  vB: boolean;
  vDC1, vDC2: HDC;
begin
//  TGDIPlusDrawContext(FContext).FGPBitmap;
//  FDrawContext.SaveToFile('test123.bmp');
//  vDC1 := TGDIPlusDrawContext(FContext).FGPCanvas.GetHDC;
  vDC2 := TGDIPlusDrawContext(AContext).FGPCanvas.GetHDC;
  vB := BitBlt(FContainer.DC, 0, 0, AContext.Width, AContext.Height,
    vDC2, 0, 0, SRCCOPY or $40000000);
  vError := GetLastError;
//  ReleaseDC(FContainer.HWND ,vDC1);
//  ReleaseDC(FContainer.HWND, vDC2);



//  vFlag := ThisGpCanvas.DrawCachedBitmap(TGDIPlusDrawContext(AContext).GPCachedBitmap, 0,0);

//  FGPCanvas.DrawImage(TGDIPlusDrawContext(AContext).FGPBitmap, 0,0);
//  FGPCanvas.Flush(FlushIntentionSync);
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
//      TGDIPlusDrawContext(FContext).Canvas.StretchDraw(ARect.Round, TGraphic(AImage));
  end;
end;

procedure TGDIPlusPainter.DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF);
begin
  FGPCanvas.DrawLine(TGPPen(AStroke.NativeObject), TGPPointF(APoint1), TGPPointF(APoint2));
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

procedure TGDIPlusPainter.EndPaint;
var
  vHBitmap: HBITMAP;
  vB: Boolean;
  vError: DWORD;

begin
  inherited;
//  FGPBitmap.GetRawFormat(veqr);
//  FGPBitmap.Save('testtest.bmp', veqr);
//  FGPCanvas.DrawImage(FGPBitmap, FContainer.Width, FContainer.Height);
//  vb := BitBlt(FGPCanvas.GetHDC, 0,0, FContainer.Width, FContainer.Height, FDC, 0, 0, SRCCOPY);
  vError := GetLastError;
  DeleteObject(TGDIPlusDrawContext(FContext).Bitmap);
  DeleteDC(TGDIPlusDrawContext(FContext).Handle);
  FreeAndNil(FContext);
  FGPCanvas.Free;
//  FContext.Free;
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
    FHandle := CreateCompatibleDC(TGDIPlusPainter(APainter).FContainer.DC);
    FHBitmap := CreateCompatibleBitmap(FHandle, Round(AWidth), Round(AHeight));
    SelectObject(FHandle, FHBitmap);
    FGPCanvas := TGPGraphics.Create(TGPBitmap.Create(Round(AWidth), Round(AHeight)));
    FGPBitmap := TGPBitmap.Create(Round(AWidth), Round(AHeight), FGPCanvas);
    FGPCanvas.SetPageUnit(UnitPixel);
    FGPCanvas.SetSmoothingMode(SmoothingModeAntiAlias);
  end
  else begin
    FBitmap := nil;
//    FCanvas := AContainer.Canvas;
    FGPCanvas := TGPGraphics.Create(AContainer.DC);
    FGPBitmap := TGPBitmap.Create(AContainer.Width, AContainer.Height, FGPCanvas);
    FGPCanvas.SetPageUnit(UnitPixel);
    FGPCanvas.SetSmoothingMode(SmoothingModeAntiAlias);
    FHandle := FGPCanvas.GetHDC;
    FGPCanvas.ReleaseHDC(FHandle);
  end;


end;

destructor TGDIPlusDrawContext.Destroy;
begin
  DeleteDC(FHandle);
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
  Result := FHandle;
end;

procedure TGDIPlusDrawContext.SaveToFile(const AFileName: string);
var
  vCLSidEncoder: TGUID;
begin
  if Assigned(FGPBitmap) then
  begin
    FGPBitmap.GetRawFormat(vCLSidEncoder);
    FGPBitmap.Save(AFileName, vCLSidEncoder);
  end;
end;

{ TGDIPlusScene }

function TGDIPlusScene.CreatePainter(const AContainer: TObject): TPainter;
var
  vContainer: TDrawContainer absolute AContainer;
begin
  Result := TGDIPlusPainter.Create(AContainer);
  FStaticContext := TGDIPlusDrawContext(Result.CreateDrawContext(vContainer.Width, vContainer.Height));
end;

function TGDIPlusScene.GetClientPos: TPointF;
var
  vClientPos: TPoint;
begin
  GetCursorPos(vClientPos);
  Result := FPanel.ScreenToClient(vClientPos);
end;

function TGDIPlusScene.GetSceneRect: TRectF;
begin
  Result := FPanel.ClientRect;
end;

procedure TGDIPlusScene.SetEnabled(const AValue: Boolean);
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

procedure TGDIPlusScene.DoActivate;
function AllParentsVisible(const AControl: TControl): Boolean;
  var
    vParent: TControl;
  begin
    Result := True;
    vParent := AControl.Parent;
    while Assigned(vParent) do
    begin
      if not vParent.Visible then
      begin
        Result := False;
        Break;
      end;
      vParent := vParent.Parent;
    end;
  end;
begin
  if (not FPanel.Focused) and FPanel.CanFocus and AllParentsVisible(FPanel) then
    FPanel.SetFocus;
end;

function TGDIPlusScene.DoCreateScene(const APlaceholder: TObject): TPainter;
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

procedure TGDIPlusScene.DoDestroyScene;
begin
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
end;

procedure TGDIPlusScene.DoRender(const ANeedFullRepaint: Boolean);
var
  vOldContext: TDrawContext;
begin
  if ANeedFullRepaint then
  begin

    FPainter.BeginPaint;
    FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
//    TGDIPlusPainter(FPainter).FStaticContext := TGDIPlusDrawContext(FPainter.Context);
    FRoot.RenderDynamic(FPainter, GetSceneRect);
    FPainter.EndPaint;
//    FPainter.SetContext(vOldContext);
  end else
  begin

//    vOldContext := FPainter.SetContext(TGDIPlusPainter(FPainter).FStaticContext);
//    vOldContext := FPainter.SetContext(FStaticContext);
    FStaticContext := TGDIPlusDrawContext.Create(FPainter, nil, FPanel.Width, FPanel.Height);
    FPainter.BeginPaint;
    vOldContext := FPainter.SetContext(FStaticContext);
    FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
    FRoot.RenderDynamic(FPainter, GetSceneRect);
    FPainter.SetContext(vOldContext);
    FPainter.DrawContext(FStaticContext);
    FPainter.EndPaint;
//    FPainter.SetContext(vOldContext);
  end;

//    FPainter.SetContext(FStaticContext);


//  vOldContext := FPainter.SetContext(FStaticContext);
//  try
//    FPainter.BeginPaint;
//    try
////      FPainter.DrawContext(vOldContext);
//    finally
//      FPainter.EndPaint;
//    end;
//  finally
////    FPainter.SetContext(vOldContext);
//  end;

end;


procedure TGDIPlusScene.UpdateContexts(const AWidth, AHeight: Single);
begin
  inherited;

end;

initialization

TBaseModule.RegisterModule('Painting', 'GDIPlus', TGDIPlusScene);

end.
