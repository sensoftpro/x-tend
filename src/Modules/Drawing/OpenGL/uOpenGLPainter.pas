unit uOpenGLPainter;

interface

uses
  Windows, Graphics, Types, Classes, ExtCtrls, dglOpenGL, uScene, vclScene,
  uDrawStyles, uConsts, Vcl.Controls, System.SysUtils,
  System.IOUtils, System.Math, Vcl.Imaging.pngimage, uFreeType,
  Generics.Collections, uModule;

type
  TColor = packed record
    Red: Extended;
    Green: Extended;
    Blue: Extended;
    Alpha: Extended;
  end;

  TOpenGLTexture = class
  private
    FID: GLuint;
    FGLWidth, FGLHeight: Integer;
    FWidth, FHeight: Integer;
    FUVCoords: TArray<Single>;
    function GetPixels(): TArray<GLubyte>;
  public
    destructor Destroy; override;

    procedure LoadTextureFromImage(const AFileName: string);
    procedure LoadTextureFromRawBytes(const APixels: TArray<TArray<TArray<GLubyte>>>; const AWidth, AHeight: Integer);
    function LoadTexture(const AWidth, AHeight: Integer; const APixels: TArray<GLubyte>): GLuint; overload;
    function LoadTexture(const AWidth, AHeight: Integer;
     const APixels: TArray<GLubyte>; const AInputMode: Integer): GLuint; overload;
    procedure RecalculateUV(const AWidth, AHeight: Integer);
    function NeedsRecreation: Boolean;

    property UVCoords: TArray<Single> read FUVCoords;
    property ID: GLuint read FID;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property GLHeight: Integer read FGLHeight;
    property GLWidth: Integer read FGLWidth;
    property Pixels: TArray<GLubyte> read GetPixels;
  end;

  TOpenGLDrawContext = class(TDrawContext)
  private
    FOffscreen: Boolean;
    FBuffer: TArray<GLubyte>;
    FTexture: TOpenGLTexture;
    FWidth, FHeight: Integer;
    FHasTexture: Boolean;
  public
    constructor Create(const AWidth, AHeight: Integer);
    destructor Destroy; override;

    procedure ConvertBufferToTexture;
    property Texture: TOpenGLTexture read FTexture;
    property IsOffscreen: Boolean read FOffscreen write FOffscreen;
    property Buffer: TArray<GLubyte> read FBuffer write FBuffer;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

  TCharacter = class
  public
    Texture: TOpenGLTexture;
    Width: Cardinal;
    Height: Cardinal;
    BearingLeft: Cardinal;
    BearingTop: Cardinal;
    AdvanceX: Cardinal;
    AdvanceY: Cardinal;
    procedure RecreateTexture(const ABitmap: TFTBitmap);
    constructor Create(const ATexture: TOpenGLTexture; const AWidth, AHeight, ABearingLeft,
     ABearingTop, AAdvanceX, AAdvanceY: Cardinal);
    destructor Destroy; override;
  end;

  TTrueTypeFont = class
  private
    FCharacters: TDictionary<WideChar, TCharacter>;
    FSize: Single;
    FFTFace: TFTFace;
    FOptions: Integer;
    FStyle: Integer;
    FAngle: Single;
    procedure DrawUnderline(const APoint1, APoint2: TPointF);
    procedure ResizeFont(const ASize: Single);
    procedure DrawStrikeout(const APoint1, APoint2: TPointF; const ATextHeight: Single);
    procedure DrawCharacter(const ARect: TRectF; const ACharTexture: TOpenGLTexture);
  public
    constructor Create(const AChars: TDictionary<WideChar, TCharacter>; const AFTFace: TFTFace);
    destructor Destroy; override;

    procedure DrawText(const ARect: TRectF; const AText: string; const AColor: TColor);
    function MeasureText(const AText: string): TSizeF;

    property FTFace: TFTFace read FFTFace write FFTFace;
    property Size: Single read FSize write ResizeFont;
    property Options: Integer read FOptions write FOptions;
    property Style: Integer read FStyle write FStyle;
    property Angle: Single read FAngle write FAngle;
    property Characters: TDictionary<WideChar, TCharacter> read FCharacters write FCharacters;
  end;

  PTrueTypeFont = ^TTrueTypeFont;

  TOpenGLImage = class
  private
    FWidth, FHeight: Integer;
    FTexture: TOpenGLTexture;
  public
    constructor Create(const AImage: TStyleImage); overload;
    constructor Create(const AWidth, AHeight: Integer;const APixels: TArray<GLubyte>); overload;
    constructor Create(const AWidth, AHeight: Integer;const APixels: TArray<TArray<TArray<GLubyte>>>); overload;
    destructor Destroy(); override;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Texture: TOpenGLTexture read FTexture;
  end;

  TOpenGLNinePatchImage = class(TNinePatchImage)
  private
    FPixels: TArray<TArray<TArray<GLubyte>>>;
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject; override;
  end;

  TOpenGLPainter = class(TPainter)
  private
    FRC: HGLRC;
    FDC: HDC;
    FPixelFormat: Integer;
    FUsedFontFamilies: TDictionary<string, PTrueTypeFont>;
    procedure SetDCPixelFormat;
    procedure DrawLine(const APoint1, APoint2: TPointF);
  protected
    procedure DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF); override;
    procedure DoDrawPie(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF; const AStartAngle, AEndAngle: Single); override;
    procedure DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF); override;
    procedure DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer); override;
    procedure DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject); override;
    procedure DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer); override;
    procedure DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer); override;
    procedure DoDrawRect(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF); override;
    procedure DoDrawText(const AFont: TStyleFont; const AText: string; const ARect: TRectF; const AOptions: Cardinal; const AAngle: Single); override;
    function GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF; override;
    procedure DoInvertRect(const ARect: TRectF); override;
    procedure DoClipRect(const ARect: TRectF); override;
    procedure DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single); override;
    procedure DoDrawContext(const AContext: TDrawContext); override;
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
    function SetContext(const AContext: TDrawContext): TDrawContext; override;

    procedure BeginPaint; override;
    procedure EndPaint; override;
  end;

  TOpenGLScene = class(TScene)
  private
    FStaticContext: TDrawContext;
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
  function NextPow2(const x: Cardinal): Cardinal; overload;
  function NextPow2(const x: Single): Cardinal; overload;
  function HexToRGBA(const AColor: Cardinal): TColor;
implementation

{ TOpenGLPainter }

function NextPow2(const x: Single): Cardinal;
begin
  Result := NextPow2(Cardinal(Round(x)));
end;

function HexToRGBA(const AColor: Cardinal): TColor;
begin
  Result.Alpha := (AColor shr 24) / 255;
  Result.Red := (AColor shr 16 and $FF) / 255;
  Result.Green := (AColor shr 8 and $FF) / 255;
  Result.Blue := (AColor and $FF) / 255;
end;

procedure TOpenGLPainter.BeginPaint;
begin
  inherited;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

constructor TOpenGLPainter.Create(const AScene: TObject; const AContainer: TObject);
var
  vContainer: TWinDrawContainer absolute AContainer;
begin
  inherited Create(AScene, AContainer);
  InitOpenGL;
  FDC := GetDC(vContainer.HWND);
  SetDCPixelFormat;
  FRC := wglCreateContext(FDC);
  ActivateRenderingContext(FDC, FRC);
  FContext := TDrawContext.Create(vContainer.Width, vContainer.Height);
  glViewport(0, 0, FContext.Width, FContext.Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluOrtho2D(0, FContext.Width, 0, FContext.Height);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glEnable(GL_COLOR_BUFFER_BIT);
  glEnable(GL_BLEND);
end;

procedure TOpenGLPainter.CreateBrush(const AFill: TStyleBrush);
begin
  AFill.NativeObject := TStyleBrush.Create(AFill.Name, AFill.Color, AFill.BackColor, AFill.GradientKind, AFill.Style);
end;

function TOpenGLPainter.CreateDrawContext(const AWidth, AHeight: Single): TDrawContext;
begin
  FRC := wglCreateContext(FDC);
  ActivateRenderingContext(FDC, FRC);
  Result := TOpenGLDrawContext.Create(Round(AWidth), Round(AHeight));
end;

procedure TOpenGLPainter.CreateFont(const AFont: TStyleFont);
var
  vFontP: PTrueTypeFont;
  vFont: TTrueTypeFont;
  vFace: TFTFace;
  vLibrary: TFTLibrary;
  vTexture: TOpenGLTexture;
  vCharacter: TCharacter;
  vPixels: TArray<GLubyte>;
  vCharacters: TDictionary<WideChar, TCharacter>;
  i, j, k: Cardinal;
  vHeight, vWidth: Cardinal;
begin
  if not Assigned(FUsedFontFamilies) then
    FUsedFontFamilies := TDictionary<string, PTrueTypeFont>.Create;

  if FUsedFontFamilies.TryGetValue(AFont.Family, vFontP) then
  begin
    vCharacters := TDictionary<Char, TCharacter>.Create;
    for i := 0 to vFontP^.Characters.Values.Count do
    begin
      if vFontP^.Characters.TryGetValue(WideChar(i), vCharacter) then
        vCharacters.Add(WideChar(i), vCharacter);
    end;
    vFont := TTrueTypeFont.Create(vCharacters, vFontP^.FTFace);
    vFont.Style := AFont.Style;
    vFont.Angle := AFont.Angle;
    vFont.Size := AFont.Size;
    AFont.NativeObject := vFont;
    Exit;
  end;

  vCharacters := TObjectDictionary<Char, TCharacter>.Create([doOwnsValues]);
  FT_Init_FreeType(vLibrary);
  vFace := TFTFace.Create('C:/Windows/Fonts/' + AnsiString(AFont.Family) + '.ttf', 0);
  vFace.SetCharSize(0, Round(AFont.Size * 64 * 96 / 72), 0, 0);
  for i := 0 to 3728 do
  begin
    if FT_Load_Glyph(vFace, FT_Get_Char_Index(vFace, i), [ftlfRender]) <> 0 then
      continue;

    vWidth := NextPow2(vFace.Glyph.Bitmap.Width);
    vHeight := NextPow2(vFace.Glyph.Bitmap.Rows);

    SetLength(vPixels, vWidth * vHeight * 2);
    for j := 0 to vHeight - 1 do
    begin
      for k := 0 to vWidth - 1 do
      begin
        if ((k >= vFace.Glyph.Bitmap.Width) or (j >= vFace.Glyph.Bitmap.Rows)) then
          vPixels[2 * (k + j * vWidth)] := 0
        else
          vPixels[2 * (k + j * vWidth)] := Byte(vFace.Glyph.Bitmap.Buffer[k + vFace.Glyph.Bitmap.Width * j]);
        vPixels[2 * (k + j * vWidth) + 1] := vPixels[2 * (k + j * vWidth)];
      end;
    end;

    vTexture := TOpenGLTexture.Create;
    vTexture.LoadTexture(vFace.Glyph.Bitmap.Width, vFace.Glyph.Bitmap.Rows, vPixels, GL_LUMINANCE_ALPHA);
    vCharacter := TCharacter.Create(vTexture, vFace.Glyph.Bitmap.Width, vFace.Glyph.Bitmap.Rows,
     vFace.Glyph.BitmapLeft, vFace.Glyph.BitmapTop, vFace.Glyph.Advance.X shr 6, vFace.Glyph.Advance.Y shr 6);
    vCharacters.Add(WideChar(i), vCharacter);
  end;

  vFont := TTrueTypeFont.Create(vCharacters, vFace);
  vFont.Style := AFont.Style;
  vFont.Angle := AFont.Angle;
  vFont.Size := AFont.Size;
  AFont.NativeObject := vFont;
  FUsedFontFamilies.Add(AFont.Family, @AFont.NativeObject);
end;

procedure TOpenGLPainter.CreateImage(const AImage: TStyleImage);
var
  vOpenGLImage: TOpenGLImage;
begin
  if TFile.Exists(AImage.FileName) then
  begin
    vOpenGLImage := TOpenGLImage.Create(AImage);
  end
  else
    Exit;
  if AImage.IsNinePatch then
  begin
    AImage.NativeObject := TOpenGLNinePatchImage.Create(Self, vOpenGLImage, AImage.CutRect.Left, AImage.CutRect.Right, AImage.CutRect.Top, AImage.CutRect.Bottom);
  end
  else
    AImage.NativeObject := vOpenGLImage;
end;

procedure TOpenGLPainter.CreatePen(const AStroke: TStylePen);
begin
  AStroke.NativeObject := TStylePen.Create(AStroke.Name, AStroke.Color, AStroke.Width, AStroke.Style, AStroke.Quality);;
end;

destructor TOpenGLPainter.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FUsedFontFamilies);
  wglDeleteContext(FRC);
  wglMakeCurrent(0, 0);
//  FT_Done_Library(vLibrary);
end;

procedure TOpenGLPainter.DoClipRect(const ARect: TRectF);
var
  vRect: TRect;
begin
  vRect := ARect.Round;
  glEnable(GL_SCISSOR_TEST);
  if vRect = Rect(0,0,0,0) then
    glDisable(GL_SCISSOR_TEST);
  glScissor(vRect.Left, FContext.Height - vRect.Bottom, vRect.Width, vRect.Height);
end;

procedure TOpenGLPainter.DoColorizeBrush(const AFill: TStyleBrush; const AColor: Cardinal);
begin
  AFill.Color := AColor;
end;

procedure TOpenGLPainter.DoColorizeFont(const AFont: TStyleFont; const AColor: Cardinal);
begin
  AFont.Color := AColor;
end;

procedure TOpenGLPainter.DoColorizePen(const AStroke: TStylePen; const AColor: Cardinal);
begin
  AStroke.Color := AColor;
end;

procedure TOpenGLPainter.DoDrawBezier(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vGLPoints: Array[0..3] of Array[0..2] of GLfloat;
  vColor: TColor;
  i,j: Integer;
  vPoints: TArray<TPointF> absolute APoints;
begin
  i := 0;
  vColor := HexToRGBA(AStroke.Color);
  while i < Length(vPoints) do
  begin
    vGLPoints[0, 0] := vPoints[0 + (Round(i/3) * 3)].X;
    vGLPoints[0, 1] := FContext.Height - vPoints[0 + (Round(i/3) * 3)].Y;
    vGLPoints[0, 2] := 0;

    vGLPoints[1, 0] := vPoints[i + 1].X;
    vGLPoints[1, 1] := FContext.Height - vPoints[i + 1].Y;
    vGLPoints[1, 2] := 0;

    vGLPoints[2, 0] := vPoints[i + 2].X;
    vGLPoints[2, 1] := FContext.Height - vPoints[i + 2].Y;
    vGLPoints[2, 2] := 0;

    vGLPoints[3, 0] := vPoints[i + 3].X;
    vGLPoints[3, 1] := FContext.Height - vPoints[i + 3].Y;
    vGLPoints[3, 2] := 0;

    glMap1f(GL_MAP1_VERTEX_3, 0.0, 1.0, 3, 4, @vGLPoints[0,0]);
    glEnable(GL_MAP1_VERTEX_3);

    glColor4f(vColor.Red, vColor.Green, vColor.Blue, vColor.Alpha);
    glBegin(GL_LINE_STRIP);
    for j := 0 to 100 do
      glEvalCoord1f(GLfloat(j) / 100);
    glEnd();
    Inc(i,3);
  end;
  glDisable(GL_MAP1_VERTEX_3);
end;

procedure TOpenGLPainter.DoDrawContext(const AContext: TDrawContext);
begin
//  vContext := TOpenGLDrawContext(AContext);
//  vRect := RectF(0,0, NextPow2(vContext.Width), NextPow2(vContext.Height));
//  DrawTexture(vRect, vContext.Texture.ID);
end;

procedure TOpenGLPainter.DoDrawEllipse(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);

  procedure drawOval(const ARect: TRectF; const AColor: TColor; AMode: GLenum = GL_TRIANGLE_FAN);
  var
    vCentre: TPointF;
    vWidth, vHeight, vTheta, vAngleIncrement, vX, vY: Single;
  begin
    vWidth := ARect.Width;
    vHeight := ARect.Height;
    vCentre := ARect.CenterPoint;
    vCentre.Y := FContext.Height - vCentre.Y;
    vAngleIncrement := 2 * Pi / 360;
    glPushMatrix();
    glTranslatef(vCentre.X, vCentre.Y, 0);
    glBegin(AMode);
    glColor4f(AColor.Red, AColor.Green, AColor.Blue, AColor.Alpha);
    if AMode = GL_TRIANGLE_FAN then
      glVertex2f(0, 0);
    vTheta := 0.0;
    while vTheta < 2 * Pi do
    begin
      vTheta := vTheta + vAngleIncrement;
      vX := vWidth / 2 * cos(vTheta);
      vY := vHeight / 2 * sin(vTheta);
      glVertex2f(vX, vY);
    end;
    glEnd;
    glPopMatrix;
  end;

begin
  inherited;
  if Assigned(AFill) then
    drawOval(ARect, HexToRGBA(AFill.Color));
  if Assigned(AStroke) then
    drawOval(ARect, HexToRGBA(AStroke.Color), GL_LINE_LOOP);
end;

procedure TOpenGLPainter.DoDrawImage(const AImage: TObject; const ARect: TRectF; const AOpacity: Single);
var
  vLocation: TPointF;
  vWidth, vHeight: Single;
  vImage: TOpenGLImage;
begin
  vLocation.X := ARect.Left;
  vLocation.Y := FContext.Height - ARect.Bottom;
  vWidth := ARect.Width;
  vHeight := ARect.Height;
  vImage := TOpenGLImage(AImage);

  if not Assigned(AImage) then
    Exit;
  if Assigned(vImage.Texture) then
  begin
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glColor4f(1, 1, 1, 1);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glBindTexture(GL_TEXTURE_2D, vImage.Texture.ID);
    glBegin(GL_QUADS);
    glTexCoord2f(vImage.Texture.UVCoords[0], vImage.Texture.UVCoords[1]);
    glVertex2f(vLocation.X, vLocation.Y);
    glTexCoord2f(vImage.Texture.UVCoords[2], vImage.Texture.UVCoords[3]);
    glVertex2f(vLocation.X + vWidth, vLocation.Y);
    glTexCoord2f(vImage.Texture.UVCoords[4], vImage.Texture.UVCoords[5]);
    glVertex2f(vLocation.X + vWidth, vLocation.Y + vHeight);
    glTexCoord2f(vImage.Texture.UVCoords[6], vImage.Texture.UVCoords[7]);
    glVertex2f(vLocation.X, vLocation.Y + vHeight);
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);
    glDisable(GL_BLEND);
    glDisable(GL_TEXTURE_2D);
  end;
end;

procedure TOpenGLPainter.DoDrawLine(const AStroke: TStylePen; const APoint1, APoint2: TPointF);
var
  vColor: TColor;
begin
  vColor := HexToRGBA(AStroke.Color);
  glLineWidth(AStroke.Width);
  if AStroke.Style in [psDot..psDashDotDot] then
  begin
    glEnable(GL_LINE_STIPPLE);
    glLineStipple(1, $F0F0);
  end;
  glColor4f(vColor.Red, vColor.Green, vColor.Blue, vColor.Alpha);
  DrawLine(APoint1, APoint2);
  glDisable(GL_LINE_STIPPLE);
end;

procedure TOpenGLPainter.DoDrawPath(const AFill: TStyleBrush; const AStroke: TStylePen; const APath: TObject);
begin
  //TODO:
end;

procedure TOpenGLPainter.DoDrawPie(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF; const AStartAngle, AEndAngle: Single);
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
    else
    begin
      Result := RadToDeg(ArcTan(Tan(DegToRad(vAngle)) * ARect.Width / ARect.Height));
      if Result < 0 then
        Result := Result + 180;
      if vAngle > 180 then
        Result := Result + 180;
    end;
  end;

  procedure drawArc(const AColor: TColor; const AMode: GLenum = GL_TRIANGLE_FAN);
  var
    vCentre: TPointF;
    vWidth, vHeight, vTheta, vAngleIncrement, vX, vY, vEnd: Single;
  begin
    vWidth := ARect.Width;
    vHeight := ARect.Height;
    vCentre := ARect.CenterPoint;
    vCentre.Y := FContext.Height - vCentre.Y;
    vAngleIncrement := DegToRad(vSweepAngle) / vSweepAngle;
    glPushMatrix;
    glTranslatef(vCentre.X, vCentre.Y, 0);
    glBegin(AMode);
    glColor4f(AColor.Red, AColor.Green, AColor.Blue, AColor.Alpha);
    if AMode = GL_TRIANGLE_FAN then
      glVertex2f(0, 0);
    vTheta := vStartAngle;
    vEnd := DegToRad(vEndAngle);
    while vTheta < vEnd do
    begin
      vTheta := vTheta + vAngleIncrement;
      vX := vWidth / 2 * cos(vTheta);
      vY := vHeight / 2 * sin(-vTheta);
      glVertex2f(vX, vY);
    end;
    if AMode <> GL_TRIANGLE_FAN then
    begin
      glVertex2f(0, 0);
      glVertex2f((vWidth / 2), 0);
    end;
    glEnd;
    glPopMatrix;
  end;

begin
  vStartAngle := CorrectAngle(AStartAngle);
  vEndAngle := CorrectAngle(AEndAngle);
  vSweepAngle := vEndAngle - vStartAngle;
  if vSweepAngle < 0 then
    vSweepAngle := vSweepAngle + 360;
  if Assigned(AFill) then
    drawArc(HexToRGBA(AFill.Color));
  if Assigned(AStroke) then
    drawArc(HexToRGBA(AStroke.Color), GL_LINE_STRIP);
end;

procedure TOpenGLPainter.DoDrawPolyline(const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  i: Integer;
  vPoints: PPointFArray absolute APoints;
  vPoint1, vPoint2: TPointF;
begin
  for i := 0 to ACount - 2 do
  begin
    vPoint1 := vPoints[i];
    vPoint2 := vPoints[i + 1];
    DoDrawLine(AStroke, vPoint1, vPoint2);
  end
end;

procedure TOpenGLPainter.DoDrawRect(const AFill: TStyleBrush; const AStroke: TStylePen; const ARect: TRectF);
var
  vLocation: TPointF;
  vWidth, vHeight: Single;
  vColor, vBackColor: TColor;
begin
  vLocation.X := ARect.Left;
  vLocation.Y := FContext.Height - ARect.Bottom;
  vWidth := ARect.Width;
  vHeight := ARect.Height;
  vColor := HexToRGBA(AFill.Color);
  if AFill.GradientKind <> gkNone then
    vBackColor := HexToRGBA(AFill.BackColor);

  if vColor.Alpha <> 1.0 then
  begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end;
  glBegin(GL_QUADS);
  if AFill.GradientKind = gkVertical then
    glColor4f(vBackColor.Red, vBackColor.Green, vBackColor.Blue, vBackColor.Alpha)
  else
    glColor4f(vColor.Red, vColor.Green, vColor.Blue, vColor.Alpha);
  glVertex2f(vLocation.X, vLocation.Y);
  if AFill.GradientKind <> gkNone then
    glColor4f(vBackColor.Red, vBackColor.Green, vBackColor.Blue, vBackColor.Alpha);
  glVertex2f(vLocation.X + vWidth, vLocation.Y);

  if AFill.GradientKind = gkHorizontal then
    glColor4f(vBackColor.Red, vBackColor.Green, vBackColor.Blue, vBackColor.Alpha)
  else if  AFill.GradientKind = gkVertical then
    glColor4f(vColor.Red, vColor.Green, vColor.Blue, vColor.Alpha);
  glVertex2f(vLocation.X + vWidth, vLocation.Y + vHeight);

  if AFill.GradientKind <> gkNone then
    glColor4f(vColor.Red, vColor.Green, vColor.Blue, vColor.Alpha);
  glVertex2f(vLocation.X, vLocation.Y + vHeight);
  glEnd;

  glDisable(GL_BLEND);
end;

procedure TOpenGLPainter.DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vPoints: TArray<TPointF> absolute APoints;
  vPoint1, vPoint2: TPointF;
  i: Integer;
  vColor: TColor;
begin
  if Assigned(AFill) then
  begin
    vColor := HexToRGBA(AFill.Color);
    glColor4f(vColor.Red, vColor.Green, vColor.Blue, vColor.Alpha);
//    glBegin(GL_POLYGON);
//    for i := 0 to ACount - 1 do
//    begin
//      glVertex2f(vPoints[i].X, FContext.Height - vPoints[i].Y);
//    end;
//    glVertex2f(vPoints[0].X, FContext.Height - vPoints[0].Y);
//    glEnd;
  end;

  if Assigned(AStroke) then
  begin
    vColor := HexToRGBA(AStroke.Color);
    glColor4f(vColor.Red, vColor.Green, vColor.Blue, vColor.Alpha);
    glBegin(GL_LINES);
    for i := 0 to ACount - 2 do
    begin
      vPoint1 := vPoints[i];
      vPoint2 := vPoints[i + 1];
      glVertex2f(vPoint1.X, FContext.Height - vPoint1.Y);
      glVertex2f(vPoint2.X, FContext.Height - vPoint2.Y);
    end;
    glVertex2f(vPoints[ACount - 1].X, FContext.Height - vPoints[ACount - 1].Y);
    glVertex2f(vPoints[0].X, FContext.Height - vPoints[0].Y);
    glEnd;
  end;
end;

procedure TOpenGLPainter.DoDrawText(const AFont: TStyleFont; const AText: string; const ARect: TRectF; const AOptions: Cardinal; const AAngle: Single);
var
  vLocation: TPointF;
begin
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);

  vLocation.X := ARect.Left;
  vLocation.Y := FContext.Height - ARect.Bottom;

  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  TTrueTypeFont(AFont.NativeObject).Options := AOptions;

  TTrueTypeFont(AFont.NativeObject).DrawText(RectF(vLocation.X, vLocation.Y + ARect.Height,
   vLocation.X + ARect.Width, vLocation.Y), AText, HexToRGBA(AFont.Color));

  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
end;

procedure TOpenGLPainter.DoInvertRect(const ARect: TRectF);
var
  vLocation: TPointF;
  VWidth, vHeight: Single;
begin
  vLocation.X := ARect.Left;
  vLocation.Y := FContext.Height - ARect.Bottom;
  VWidth := ARect.Width;
  vHeight := ARect.Height;
  glEnable(GL_COLOR_LOGIC_OP);
  glLogicOp(GL_INVERT);
  glBegin(GL_QUADS);
  glVertex2f(vLocation.X, vLocation.Y);
  glVertex2f(vLocation.X + VWidth, vLocation.Y);
  glVertex2f(vLocation.X + VWidth, vLocation.Y + vHeight);
  glVertex2f(vLocation.X, vLocation.Y + vHeight);
  glEnd;
  glDisable(GL_COLOR_LOGIC_OP);
end;

procedure TOpenGLPainter.DrawLine(const APoint1, APoint2: TPointF);
begin
  glBegin(GL_LINES);
  glVertex2f(APoint1.X, FContext.Height - APoint1.Y);
  glVertex2f(APoint2.X, FContext.Height - APoint2.Y);
  glEnd;
end;

procedure TOpenGLPainter.EndPaint;
begin
  inherited;
//  if TOpenGLDrawContext(FContext).IsOffScreen then
//  begin
//    vDrawContext := TOpenGLDrawContext(FContext);
//    glReadBuffer(GL_FRONT);
//    glReadPixels(0, 0, NextPow2(vDrawContext.Width), NextPow2(vDrawContext.Height), GL_RGBA,
//     GL_UNSIGNED_BYTE, @vDrawContext.Buffer[0]);
//    vDrawContext.ConvertBufferToTexture;
//    FContext := vDrawContext;
//
//  end;

  SwapBuffers(FDC);
end;

function TOpenGLPainter.GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF;
begin
  Result := TTrueTypeFont(AFont.NativeObject).MeasureText(AText);
end;

function TOpenGLPainter.SetContext(const AContext: TDrawContext): TDrawContext;
begin
  Result := FContext;
  FContext := AContext
end;

procedure TOpenGLPainter.SetDCPixelFormat;
var
  pfd: TPixelFormatDescriptor;
begin
  if FPixelFormat = 0 then
  begin
    FillChar(pfd, SizeOf(pfd), 0);
    pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_DOUBLEBUFFER or PFD_SUPPORT_OPENGL;
    pfd.cDepthBits := 32;
    pfd.cColorBits := 32;
    FPixelFormat := ChoosePixelFormat(FDC, @pfd);
  end;
  SetPixelFormat(FDC, FPixelFormat, @pfd);
end;

{ TpenGLScene }

function TOpenGLScene.CreatePainter(const AContainer: TObject): TPainter;
var
  vContainer: TWinDrawContainer absolute AContainer;
begin
  Result := TOpenGLPainter.Create(Self, AContainer);
  FStaticContext := TOpenGLDrawContext.Create(vContainer.Width, vContainer.Height);
  TOpenGLDrawContext(FStaticContext).IsOffscreen := True;
end;

procedure TOpenGLScene.DoActivate;
begin
  if not FPanel.Focused then
    FPanel.SetFocus;
end;

function TOpenGLScene.DoCreateScene(const APlaceholder: TObject): TPainter;
var
  vControl: TWinControl absolute APlaceholder;
  vContainer: TWinDrawContainer;
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

  vContainer := TWinDrawContainer.Create(FPanel.Handle, FPanel.Canvas, FPanel.ClientWidth, FPanel.ClientHeight);
  try
    Result := CreatePainter(vContainer);
  finally
    FreeAndNil(vContainer);
  end;
end;

procedure TOpenGLScene.DoDestroyScene;
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
  FreeAndNil(FStaticContext);
end;

procedure TOpenGLScene.DoRender(const ANeedFullRepaint: Boolean);
begin
  FPainter.BeginPaint;
  try
    FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
    FRoot.RenderDynamic(FPainter, GetSceneRect);
  finally
    FPainter.EndPaint;
  end;
end;

function TOpenGLScene.GetClientPos: TPointF;
var
  vClientPos: TPoint;
begin
  GetCursorPos(vClientPos);
  Result := FPanel.ScreenToClient(vClientPos);
end;

function TOpenGLScene.GetSceneRect: TRectF;
begin
  Result := FPanel.ClientRect;
end;

procedure TOpenGLScene.SetEnabled(const AValue: Boolean);
begin
  inherited SetEnabled(AValue);

  if AValue then
  begin
    FPanel.OnResize := OnResize;
    FPanel.OnPaint := OnPaint;
  end
  else
  begin
    FPanel.OnResize := nil;
    FPanel.OnPaint := nil;
  end;
end;

procedure TOpenGLScene.UpdateContexts(const AWidth, AHeight: Single);
begin
  inherited;
  glViewport(0, 0, Round(AWidth), Round(AHeight));
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluOrtho2D(0, AWidth, 0, AHeight);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;
{ TOpenGLNinePatchImage }

function TOpenGLNinePatchImage.CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject;
var
  vImage: TOpenGLImage;
  vPixels: TArray<TArray<TArray<GLubyte>>>;
  i, j, x, y: Integer;
begin
  if not Assigned(FPixels) then
  begin
    vImage := TOpenGLImage(FImage);
    SetLength(FPixels, vImage.Texture.GLWidth, vImage.Texture.GLHeight, 4);
    for i := 0 to vImage.Texture.GLHeight - 1 do
    begin
      for j := 0 to vImage.Texture.GLWidth - 1 do
      begin
        FPixels[j, i, 0] := vImage.Texture.Pixels[((j + (i * vImage.Texture.GLWidth)) * 4) + 0];
        FPixels[j, i, 1] := vImage.Texture.Pixels[((j + (i * vImage.Texture.GLWidth)) * 4) + 1];
        FPixels[j, i, 2] := vImage.Texture.Pixels[((j + (i * vImage.Texture.GLWidth)) * 4) + 2];
        FPixels[j, i, 3] := vImage.Texture.Pixels[((j + (i * vImage.Texture.GLWidth)) * 4) + 3];
      end;
    end;
  end;
  SetLength(vPixels, NextPow2(AWidth), NextPow2(AHeight), 4);
  x := 0;
  y := 0;
  for i := ATop to ATop + AHeight do
  begin
    for j := ALeft to ALeft + AWidth do
    begin
      if x > j then
        Dec(x);
      vPixels[x, y] := FPixels[j, i];
      Inc(x);
    end;
    if y > i then
      Dec(y);
    Inc(y);
    x := 0;
  end;
  Result := TOpenGLImage.Create(AWidth, AHeight, vPixels);
end;

function TOpenGLNinePatchImage.GetHeight: Integer;
begin
  Result := TOpenGLImage(FImage).Height;
end;

function TOpenGLNinePatchImage.GetWidth: Integer;
begin
  Result := TOpenGLImage(FImage).Width;
end;

{ TOpenGLImage }
constructor TOpenGLImage.Create(const AImage: TStyleImage);
begin
  FTexture := TOpenGLTexture.Create;

  FTexture.LoadTextureFromImage(AImage.FileName);

  FWidth := FTexture.Width;
  FHeight := FTexture.Height;
end;

constructor TOpenGLImage.Create(const AWidth, AHeight: Integer;const APixels: TArray<GLubyte>);
begin
  FTexture := TOpenGLTexture.Create;
  FTexture.LoadTexture(AWidth, AHeight, APixels);
end;

constructor TOpenGLImage.Create(const AWidth, AHeight: Integer;const APixels: TArray<TArray<TArray<GLubyte>>>);
begin
  FTexture := TOpenGLTexture.Create;
  FTexture.LoadTextureFromRawBytes(APixels, AWidth, AHeight);
  FWidth := FTexture.Width;
  FHeight := FTexture.Height;
end;

destructor TOpenGLImage.Destroy;
begin
  inherited;
  FreeAndNil(FTexture);
end;

{ TOpenGLTexture }

destructor TOpenGLTexture.Destroy;
begin
  glDeleteTextures(1, @FID);
end;

function TOpenGLTexture.GetPixels: TArray<GLubyte>;
begin
  SetLength(Result, FGLWidth * FGLHeight * 4);
  glBindTexture(GL_TEXTURE_2D, FID);
  glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, @Result[0]);
  glBindTexture(GL_TEXTURE_2D, 0);
end;

function TOpenGLTexture.LoadTexture(const AWidth, AHeight: Integer;const APixels: TArray<GLubyte>): GLuint;
begin
  FGLWidth := NextPow2(AWidth);
  FGLHeight := NextPow2(AHeight);
  FHeight := AHeight;
  FWidth := AWidth;
  FUVCoords := [0, Height / GLHeight, Width / GLWidth, Height / GLHeight, Width / GLWidth, 0, 0, 0];
  glEnable(GL_TEXTURE_2D);
  glGenTextures(1, @FID);
  glBindTexture(GL_TEXTURE_2D, FID);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, FGLWidth, FGLHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, @APixels[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glBindTexture(GL_TEXTURE_2D, 0);
  Result := FID;
end;

function TOpenGLTexture.LoadTexture(const AWidth, AHeight: Integer; const APixels: TArray<GLubyte>;
  const AInputMode: Integer): GLuint;
begin
  FGLWidth := NextPow2(AWidth);
  FGLHeight := NextPow2(AHeight);
  FHeight := AHeight;
  FWidth := AWidth;
  FUVCoords := [0, Height / GLHeight, Width / GLWidth, Height / GLHeight, Width / GLWidth, 0, 0, 0];
  glEnable(GL_TEXTURE_2D);
  glGenTextures(1, @FID);
  glBindTexture(GL_TEXTURE_2D, FID);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, FGLWidth, FGLHeight, 0, AInputMode, GL_UNSIGNED_BYTE, @APixels[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glBindTexture(GL_TEXTURE_2D, 0);
  Result := FID;
end;

procedure TOpenGLTexture.LoadTextureFromImage(const AFileName: string);
var
  vImage: TPngImage;
  i, j, cur: Cardinal;
  vPixels: TArray<GLubyte>;
begin
  if not TFile.Exists(AFileName) then
    Exit;
  vImage := TPngImage.Create;
  vImage.LoadFromFile(AFileName);
  SetLength(vPixels, NextPow2(vImage.Width) * NextPow2(vImage.Height) * 4);
  cur := 0;
  for i := 0 to NextPow2(vImage.Height) - 1 do
  begin
    for j := 0 to NextPow2(vImage.Width) - 1 do
    begin
      vPixels[cur + 0] := GetRValue(vImage.Canvas.Pixels[j, i]);
      vPixels[cur + 1] := GetGValue(vImage.Canvas.Pixels[j, i]);
      vPixels[cur + 2] := GetBValue(vImage.Canvas.Pixels[j, i]);
      vPixels[cur + 3] := vImage.AlphaScanline[i]^[j];
      Inc(cur, 4);
    end;
  end;
  LoadTexture(vImage.Width, vImage.Height, vPixels);
  vImage.Free;
end;

procedure TOpenGLTexture.LoadTextureFromRawBytes(const APixels: TArray<TArray<TArray<GLubyte>>>; const AWidth, AHeight: Integer);
var
  i, j, cur: Cardinal;
  vPixels: TArray<GLubyte>;
begin
  SetLength(vPixels, NextPow2(AWidth) * NextPow2(AHeight) * 4);
  cur := 0;
  for i := 0 to NextPow2(AHeight) - 1 do
  begin
    for j := 0 to NextPow2(AWidth) - 1 do
    begin
      vPixels[cur + 0] := APixels[j, i, 0];
      vPixels[cur + 1] := APixels[j, i, 1];
      vPixels[cur + 2] := APixels[j, i, 2];
      vPixels[cur + 3] := APixels[j, i, 3];
      Inc(cur, 4);
    end;
  end;

  LoadTexture(AWidth, AHeight, vPixels);
end;

function TOpenGLTexture.NeedsRecreation: Boolean;
begin
  Result := (NextPow2(FWidth) <> Cardinal(FGLWidth)) or (NextPow2(FHeight) <> Cardinal(FGLHeight));
end;

procedure TOpenGLTexture.RecalculateUV(const AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FUVCoords := [0, Height / GLHeight, Width / GLWidth, Height / GLHeight, Width / GLWidth, 0, 0, 0];
end;

function NextPow2(const x: Cardinal): Cardinal;
var
  vMaxTextureSize: GLuint;
begin
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @vMaxTextureSize);
  Result := 2;
  while (Result < x) or (x > vMaxTextureSize) do
    Result := Result shl 1;
end;

{ TOpenGLPainter.TCharacter }

constructor TCharacter.Create(const ATexture: TOpenGLTexture; const AWidth, AHeight, ABearingLeft, ABearingTop, AAdvanceX, AAdvanceY: Cardinal);
begin
  Texture := ATexture;
  Width := AWidth;
  Height := AHeight;
  BearingLeft := ABearingLeft;
  BearingTop := ABearingTop;
  AdvanceX := AAdvanceX;
  AdvanceY := AAdvanceY;
end;

{ TTrueTypeFont }

constructor TTrueTypeFont.Create(const AChars: TDictionary<WideChar, TCharacter>; const AFTFace: TFTFace);
begin
  FCharacters := AChars;
  FFTFace := AFTFace;
end;

destructor TTrueTypeFont.Destroy;
begin
  FCharacters.Clear;
  FreeAndNil(FCharacters);
//  FT_Done_Face(FFTFace);
end;

procedure TTrueTypeFont.DrawCharacter(const ARect: TRectF; const ACharTexture: TOpenGLTexture);
var
  vUVCoords: TArray<Single>;
begin
  vUVCoords := ACharTexture.UVCoords;
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glBindTexture(GL_TEXTURE_2D, ACharTexture.ID);
  glBegin(GL_QUADS);
  glTexCoord2f(vUVCoords[0], vUVCoords[1]);
  glVertex2f(ARect.Left, ARect.Bottom);
  glTexCoord2f(vUVCoords[2], vUVCoords[3]);
  glVertex2f(ARect.Right,ARect.Bottom);
  glTexCoord2f(vUVCoords[4], vUVCoords[5]);
  glVertex2f(ARect.Right,ARect.Top);
  glTexCoord2f(vUVCoords[6], vUVCoords[7]);
  glVertex2f(ARect.Left, ARect.Top);
  glEnd;
  glBindTexture(GL_TEXTURE_2D, 0);
  glDisable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
end;

procedure TTrueTypeFont.DrawStrikeout(const APoint1, APoint2: TPointF; const ATextHeight: Single);
var
  vRadAngle: Single;
begin
  vRadAngle := DegToRad(FAngle);
  glBegin(GL_LINES);
  glVertex2f(APoint1.X + (ATextHeight * sin(vRadAngle))/ 2, APoint1.Y + (ATextHeight * cos(vRadAngle)) / 2);
  glVertex2f(APoint2.X + (ATextHeight * sin(vRadAngle)) / 2, APoint2.Y + (ATextHeight * cos(vRadAngle)) / 2);
  glEnd;
end;

procedure TTrueTypeFont.DrawText(const ARect: TRectF; const AText: string; const AColor: TColor);
var
  c: Char;
  delta: Extended;
  i: Integer;
  vChar: TCharacter;
  vLocation: TPointF;
  vStartLocation, vEndLocation: TPointF;
  vTextMetric: TSizeF;
begin
  vTextMetric := MeasureText(AText);
  if (FOptions and DT_LEFT) > 0 then
    vLocation.X := ARect.Left
  else if (FOptions and DT_CENTER) > 0 then
  begin
    delta := ARect.Width - vTextMetric.cx * cos(DegToRad(FAngle));
    vLocation.X := ARect.Left + delta / 2
  end
  else if (FOptions and DT_RIGHT) > 0 then
    vLocation.X := ARect.Right - vTextMetric.cx * cos(DegToRad(FAngle))
  else
   vLocation.X := ARect.Right - vTextMetric.cx * cos(DegToRad(FAngle));

  if (FOptions and DT_TOP) > 0 then
    vLocation.Y := ARect.Top
  else if (FOptions and DT_VCENTER) > 0 then
  begin
    delta := ARect.Height - vTextMetric.cy;
    vLocation.Y := ARect.Top + delta / 2
  end
  else if (FOptions and DT_BOTTOM) > 0 then
    vLocation.Y := ARect.Bottom
  else
   vLocation.Y := ARect.Bottom;

  CopyMemory(@vStartLocation, @vLocation, SizeOf(vLocation));

  for i := 1 to AText.Length do
  begin
    c := AText[i];

    if i <> 1 then
    begin
      delta := vChar.AdvanceX - vChar.AdvanceX * cos(DegToRad(FAngle));
      vLocation.X := vLocation.X + vChar.AdvanceX * cos(DegToRad(FAngle));
      vLocation.Y := vLocation.Y + delta;
    end;
    if FCharacters.TryGetValue(c, vChar) = True then
    begin
      glPushMatrix;
      glTranslatef(vLocation.X, vLocation.Y, 0);
      glRotatef(360 - FAngle, 0, 0, 1);
      glTranslatef(-vLocation.X, -vLocation.Y, 0);

      glColor4f(AColor.Red, AColor.Green, AColor.Blue, AColor.Alpha);
      if AColor.Red <> 0 then
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      DrawCharacter(RectF(vLocation.X, vLocation.Y - (vChar.Height - vChar.BearingTop) + vChar.Height,
       vLocation.X + vChar.Width, vLocation.Y - (vChar.Height - vChar.BearingTop)), vChar.Texture);
      if (FStyle and FontStyleBold) > 0 then
      begin
        DrawCharacter(RectF(vLocation.X + 1, vLocation.Y + vChar.Height,
          vLocation.X + vChar.Width + 1, vLocation.Y - (vChar.Height - vChar.BearingTop)), vChar.Texture);
      end;

      glPopMatrix;
    end
  end;

  vEndLocation := TPointF.Create(vLocation.X + vChar.Width * cos(DegToRad(FAngle)),
   vLocation.Y - vChar.Width * sin(DegToRad(FAngle)));
  if (FStyle and FontStyleUnderline) > 0 then
    DrawUnderline(vStartLocation, vEndLocation);
  if (FStyle and FontStyleStrikeout) > 0 then
    DrawStrikeout(vStartLocation, vEndLocation, vTextMetric.cy / 96 * 72)
end;

procedure TTrueTypeFont.DrawUnderline(const APoint1, APoint2: TPointF);
begin
  glBegin(GL_LINES);
  glVertex2f(APoint1.X, APoint1.Y);
  glVertex2f(APoint2.X, APoint2.Y);
  glEnd;
end;

function TTrueTypeFont.MeasureText(const AText: string): TSizeF;
var
  vTotalWidth: Single;
  vChar: TCharacter;
  i: Integer;
begin
  vTotalWidth := 0;
  Result.cy := 0;
  FFTFace.SetCharSize(0, Round(FSize * 64 * 96 / 72), 0, 0);

  for i := 1 to AText.Length do
  begin
    if FCharacters.TryGetValue(Char(AText[i]), vChar) then
    begin
    if FT_Load_Glyph(FFTFace, FT_Get_Char_Index(FFTFace, Cardinal(AText[i])), [ftlfNoBitmap]) <> 0 then
      continue;
    vChar.Height := FFTFace.Glyph.Bitmap.Rows;
    vChar.BearingTop := FFTFace.Glyph.BitmapTop;
    vChar.Width := FFTFace.Glyph.Bitmap.Width;
    vChar.AdvanceX := FFTFace.Glyph.Advance.X shr 6;
    vChar.Texture.RecalculateUV(vChar.Width, vChar.Height);
    if vChar.Texture.NeedsRecreation then
    begin
      if FT_Load_Glyph(FFTFace, FT_Get_Char_Index(FFTFace, Cardinal(AText[i])), [ftlfRender]) <> 0 then
        continue;
      vChar.RecreateTexture(FFTFace.Glyph.Bitmap);
    end;
    vTotalWidth := vChar.Width + vTotalWidth;
    if (Result.cy <= vChar.Height * 96 / 72) then
      Result.cy := vChar.Height * 96 / 72;
    end;
  end;
  Result.cx := vTotalWidth;
end;

procedure TTrueTypeFont.ResizeFont(const ASize: Single);
begin
  FSize := ASize;
end;

{ TOpenGLDrawContext }

procedure TOpenGLDrawContext.ConvertBufferToTexture;
begin
  if not FHasTexture then
  begin
    glEnable(GL_TEXTURE_2D);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    FTexture := TOpenGLTexture.Create;
    FTexture.LoadTexture(FWidth, FHeight, FBuffer);
    FHasTexture := True;
  end else
    Exit;
end;

constructor TOpenGLDrawContext.Create(const AWidth, AHeight: Integer);
begin
  SetLength(FBuffer, NextPow2(AWidth) * NextPow2(AHeight) * 4);
  FOffscreen := False;
  FWidth := AWidth;
  FHeight := AHeight;
end;

destructor TOpenGLDrawContext.Destroy;
begin
  FBuffer := nil;
  FreeAndNil(FTexture);
  inherited;
end;

destructor TCharacter.Destroy;
begin
  FreeAndNil(Texture);
end;

procedure TCharacter.RecreateTexture(const ABitmap: TFTBitmap);
var
  vWidth, vHeight: Cardinal;
  vPixels: TArray<GLubyte>;
  j, k: Cardinal;
begin
  vWidth := NextPow2(ABitmap.Width);
  vHeight := NextPow2(ABitmap.Rows);

  SetLength(vPixels, vWidth * vHeight * 2);
  for j := 0 to vHeight - 1 do
  begin
    for k := 0 to vWidth - 1 do
    begin
      if ((k >= ABitmap.Width) or (j >= ABitmap.Rows)) then
        vPixels[2 * (k + j * vWidth)] := 0
      else
        vPixels[2 * (k + j * vWidth)] := Byte(ABitmap.Buffer[k + ABitmap.Width * j]);
      vPixels[2 * (k + j * vWidth) + 1] := vPixels[2 * (k + j * vWidth)];
    end;
  end;
  FreeAndNil(Texture);

  Texture := TOpenGLTexture.Create;
  Texture.LoadTexture(Width, Height, vPixels, GL_LUMINANCE_ALPHA);
end;

initialization
  TBaseModule.RegisterModule('Painting', '', 'OpenGL', TOpenGLScene);
end.
