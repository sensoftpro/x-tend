unit uOpenGLPainter;

interface

uses
  Windows, Graphics, Types, Classes, ExtCtrls, dglOpenGL, uScene, uWinScene,
  uDrawStyles, uConsts, Vcl.Controls, System.SysUtils, Vcl.StdCtrls,
  System.IOUtils, System.Math, Vcl.Imaging.pngimage, uFreeType,
  Generics.Collections, uModule;

type
  TColor = packed record
    Red, Green, Blue, Alpha: Extended;
  end;
  TPointArray = array of TPoint;
  TGLUtils = class(TObject)
    class function NextPow2(const x: Cardinal): Cardinal; overload; static;
    class function NextPow2(const x: Single): Cardinal; overload; static;
    class function HexToRGBA(const AColor: Cardinal): TColor; static;
    class function PPointFToPoints(const APoints: PPointF; const ACount: Integer): TPointArray; static;
  end;

  TOpenGLTexture = class(TObject)
  private
    FID: GLuint;
    FGLWidth, FGLHeight, FWidth, FHeight: Integer;
    FUVCoords: TArray<Single>;
    function LoadTexture(AWidth, AHeight: Integer; APixels: TArray<GLubyte>): GLuint;
    function GetPixels(): TArray<GLubyte>;
  public
    property UVCoords: TArray<Single> read FUVCOords;
    property ID: GLuint read FID;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property GLHeight: Integer read FGLHeight;
    property GLWidth: Integer read FGLWidth;

    property Pixels: TArray<GLubyte> read GetPixels;
    destructor Destroy; override;
    procedure LoadTextureFromImage(AFileName: string);
    procedure LoadTextureFromRawBytes(const APixels: TArray<TArray<TArray<GLubyte>>>; AWidth, AHeight: Integer);
  end;

  TCharacter = record
    TextureID, Width, Height, BearingLeft, BearingTop, AdvanceX, AdvanceY: Cardinal;
    class function Create(ATextureID, AWidth, AHeight, ABearingLeft, ABearingTop, AAdvanceX, AAdvanceY: Cardinal): TCharacter; static;

  end;

  TTrueTypeFont = class(TObject)
  private
    FCharacters: TDictionary<WideChar, TCharacter>;

  public
    property Characters: TDictionary<WideChar, TCharacter> read FCharacters write FCharacters;
    constructor Create(const AChars: TDictionary<WideChar, TCharacter>);
    destructor Destroy; override;
  end;

  PTrueTypeFont = ^TTrueTypeFont;

  TOpenGLImage = class(TObject)
  private
    FWidth, FHeight: Integer;
    FTexture: TOpenGLTexture;
  public
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    destructor Destroy(); override;
    property Texture: TOpenGLTexture read FTexture;

    constructor Create(const AImage: TStyleImage); overload;
    constructor Create(AWidth, AHeight: Integer; APixels: TArray<GLubyte>); overload;
    constructor Create(AWidth, AHeight: Integer; APixels: TArray<TArray<TArray<GLubyte>>>); overload;
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
    var
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

implementation

{ TOpenGLPainter }

procedure TOpenGLPainter.BeginPaint;
begin
  inherited;
  glClearColor(0.1, 0.1, 0.1, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

constructor TOpenGLPainter.Create(const ACOntainer: TObject);
var
  vContainer: TDrawContainer absolute ACOntainer;
begin
  inherited Create(ACOntainer);
  InitOpenGL;
  FDC := GetDC(vContainer.HWND);
  SetDCPixelFormat;
  FContext := CreateDrawContext(vContainer.Width, vContainer.Height);
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
  if AFill.Style = bsSolid then
  begin
    if AFill.GradientKind <> gkNone then
    begin
      // Создать градиентную кисть, привязать ее к TStyleBrush
      AFill.NativeObject := TStyleBrush.Create(AFill.Name, AFill.Color, AFill.BackColor, AFill.GradientKind, AFill.Style);
    end
    else
    begin
      // Создать сплошную кисть, привязать ее к TStyleBrush
      AFill.NativeObject := TStyleBrush.Create(AFill.Name, AFill.Color, AFill.BackColor, AFill.GradientKind, AFill.Style);
    end;
  end
  else
  begin
    // Создать штриховую кисть, привязать ее к TStyleBrush
    AFill.NativeObject := TStyleBrush.Create(AFill.Name, AFill.Color, AFill.BackColor, AFill.GradientKind, AFill.Style);
  end;
end;

function TOpenGLPainter.CreateDrawContext(const AWidth, AHeight: Single): TDrawContext;
begin
  FRC := wglCreateContext(FDC);
  ActivateRenderingContext(FDC, FRC);
  Result := TDrawContext.Create(AWidth, AHeight);
end;

procedure TOpenGLPainter.CreateFont(const AFont: TStyleFont);
var
  vFontP: PTrueTypeFont;
  vFace: TFTFace;
  vLibrary: TFTLibrary;
  vTextureID: Cardinal;
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
    AFont.NativeObject := TTrueTypeFont.Create(vCharacters);
    Exit;
  end;
  vCharacters := TDictionary<Char, TCharacter>.Create;
  FT_Init_FreeType(vLibrary);

  vFace := TFTFace.Create('C:/Windows/Fonts/' + AnsiString(AFont.Family) + '.ttf', 0);
  vFace.SetCharSize(0, Round(AFont.Size * 64 * 96 / 72), 0, 0);
  for i := 0 to 3728 do
  begin
    if FT_Load_Glyph(vFace, FT_Get_Char_Index(vFace, i), [ftlfRender]) <> 0 then
      continue;

    glGenTextures(1, @vTextureID);
    glBindTexture(GL_TEXTURE_2D, vTextureID);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

    vWidth := TGLUtils.NextPow2(vFace.glyph.bitmap.width);
    vHeight := TGLUtils.NextPow2(vFace.glyph.bitmap.rows);

    SetLength(vPixels, vWidth * vHeight * 2);
    for j := 0 to vHeight - 1 do
    begin
      for k := 0 to vWidth - 1 do
      begin
        if ((k >= vFace.glyph.bitmap.width) or (j >= vFace.glyph.bitmap.rows)) then
          vPixels[2 * (k + j * vWidth)] := 0
        else
          vPixels[2 * (k + j * vWidth)] := byte(vFace.glyph.bitmap.buffer[k + vFace.glyph.bitmap.width * j]);
        vPixels[2 * (k + j * vWidth) + 1] := vPixels[2 * (k + j * vWidth)];
      end;
    end;

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, vWidth, vHeight, 0, GL_LUMINANCE_ALPHA, GL_UNSIGNED_BYTE, @vPixels[0]);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    vCharacter := TCharacter.Create(vTextureID, vFace.Glyph.Bitmap.Width, vFace.Glyph.Bitmap.Rows, vFace.glyph.BitmapLeft, vFace.glyph.BitmapTop, vFace.Glyph.Advance.X shr 6, vFace.Glyph.Advance.Y shr 6);
    glBindTexture(GL_TEXTURE_2D, 0);
    vCharacters.Add(WideChar(i), vCharacter);
  end;

  FT_Done_Face(vFace);
  FT_Done_Library(vLibrary);
  AFont.NativeObject := TTrueTypeFont.Create(vCharacters);
  FUsedFontFamilies.Add(AFont.Family, @AFont.NativeObject);
end;

procedure TOpenGLPainter.CreateImage(const AImage: TStyleImage);
var
  vOpenGLImage: TOpenGLImage;
begin
  vOpenGLImage := TOpenGLImage.Create(AImage);
  if AImage.IsNinePatch then
  begin
    AImage.NativeObject := TOpenGLNinePatchImage.Create(Self, vOpenGLImage, AImage.CutRect.Left, AImage.CutRect.Right, AImage.CutRect.Top, AImage.CutRect.Bottom);
  end
  else
    AImage.NativeObject := vOpenGLImage;

end;

procedure TOpenGLPainter.CreatePen(const AStroke: TStylePen);
const
  cDashStyles: array[psDash..psDashDotDot] of TObject = (nil, nil, nil, nil);
var
  vPen: TStylePen;
begin
  if AStroke.Style in [psDash..psDashDotDot] then
  begin
    vPen := TStylePen.Create(AStroke.Name, AStroke.Color, AStroke.Width, AStroke.Style, AStroke.Quality);
  end
  else
  begin
    vPen := TStylePen.Create(AStroke.Name, AStroke.Color, AStroke.Width, AStroke.Style, AStroke.Quality);
  end;

  AStroke.NativeObject := vPen;

end;

destructor TOpenGLPainter.Destroy;
begin
  inherited Destroy;
  FUsedFontFamilies.Free;
  wglDeleteContext(FRC);
  wglMakeCurrent(0, 0);
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
  //Displays only one bezier i.e doesnt support PolyBezier
  i := 0;
  vColor := TGLUtils.HexToRGBA(AStroke.Color);
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
    glColor4f(AColor.Red, AColor.Green, AColor.Blue, AColor.Alpha);
    glBegin(AMode);
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
    drawOval(ARect, TGLUtils.HexToRGBA(AFill.Color));
  if Assigned(AStroke) then
    drawOval(ARect, TGLUtils.HexToRGBA(AStroke.Color), GL_LINE_LOOP);
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
  //TODO: Обрабатывать все виды линий psDot, psDashDot, psDashDotDot ..
  vColor := TGLUtils.HexToRGBA(AStroke.Color);
  glLineWidth(AStroke.Width);
  if AStroke.Style = psDot then
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
    drawArc(TGLUtils.HexToRGBA(AFill.Color));
  if Assigned(AStroke) then
    drawArc(TGLUtils.HexToRGBA(AStroke.Color), GL_LINE_STRIP);

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
  vColor: TColor;
begin
  vLocation.X := ARect.Left;
  vLocation.Y := FContext.Height - ARect.Bottom;
  vWidth := ARect.Width;
  vHeight := ARect.Height;
  vColor := TGLUtils.HexToRGBA(AFill.Color);

  if vColor.Alpha <> 1.0 then
  begin
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  end;
  glColor4f(vColor.Red, vColor.Green, vColor.Blue, vColor.Alpha);
  glBegin(GL_QUADS);
  glVertex2f(vLocation.X, vLocation.Y);
  glVertex2f(vLocation.X + vWidth, vLocation.Y);
  glVertex2f(vLocation.X + vWidth, vLocation.Y + vHeight);
  glVertex2f(vLocation.X, vLocation.Y + vHeight);
  glEnd;

  glDisable(GL_BLEND);
end;

procedure TOpenGLPainter.DoDrawRegion(const AFill: TStyleBrush; const AStroke: TStylePen; const APoints: PPointF; const ACount: Integer);
var
  vPoints: PPointFArray absolute APoints;
  vColor: TColor;
  i: Integer;
  vPoint1, vPoint2: TPointF;
begin

  if Assigned(AFill) then
  begin
    //TODO

  end;

  if Assigned(AStroke) then
  begin
    vColor := TGLUtils.HexToRGBA(AStroke.Color);
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
  vColor: TColor;
  c: Char;
  i, delta: Cardinal;
  vFlag: Boolean;
  vChar: TCharacter;
  vLocation: TPointF;
  vWidth, vHeight, vMaxWidth, vMaxHeight: Single;
begin
  vLocation.X := ARect.Right - (ARect.Width / 2);
  vLocation.Y := FContext.Height - ARect.Bottom - (ARect.Height / 2);
  delta := 0;
  vMaxHeight := 0;
  vMaxWidth := 0;
  glEnable(GL_TEXTURE_2D);
  glEnable(GL_BLEND);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  vColor := TGLUtils.HexToRGBA(AFont.Color);
  glColor4f(vColor.Red, vColor.Green, vColor.Blue, vColor.Alpha);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  if AAngle > 90 then
    vLocation.Y := FContext.Height - ARect.Top;
  vFlag := False;
  for i := strlen(PWideChar(AText)) downto 1 do
  begin
    c := AText[i];
    if TTrueTypeFont(AFont.NativeObject).Characters.TryGetValue(c, vChar) = True then
      glBindTexture(GL_TEXTURE_2D, vChar.TextureID);
    if (AFont.Style and FontStyleBold) = 1 then
      vChar.AdvanceX := vChar.AdvanceX + 1;

    if (vChar.AdvanceY <> 0) then
      vLocation.Y := vLocation.Y + vChar.AdvanceY;

    if (vMaxWidth < vChar.Width) then
      vMaxWidth := TGLUtils.NextPow2(vChar.Width);
    vWidth := TGLUtils.NextPow2(vChar.Width);

    if (vMaxHeight < vChar.Height) then
    begin
      vMaxHeight := TGLUtils.NextPow2(vChar.Height);
      delta := Round(vMaxHeight) - vChar.Height - 1;
    end;
    vHeight := TGLUtils.NextPow2(vChar.Height);

    if i = strlen(PWideChar(AText)) then
      vLocation.X := Round(vLocation.X + vChar.AdvanceX);

    if ((vMaxWidth * strlen(PWideChar(AText))) > (ARect.Left + ARect.Width)) then
    begin
      vLocation.Y := Round(vLocation.Y - vChar.AdvanceX);
    end
    else
      vLocation.X := Round(vLocation.X - vChar.AdvanceX);

    if (vMaxHeight <> vHeight) and (vFlag = False) then
    begin
      vLocation.X := vLocation.X - delta;
      vFlag := True;
    end
    else if (vMaxHeight = vHeight) and (vFlag = True) then
    begin
      vLocation.X := vLocation.X + delta;
      vFlag := False;
    end;

    glPushMatrix;
    glTranslatef(vLocation.X, vLocation.Y, 0);
    glRotatef(360 - AAngle, 0, 0, 1);
    glTranslatef(-vLocation.X, -vLocation.Y, 0);
//    glColor4f(0,0,0,1);
//    glDisable(GL_TEXTURE_2D);
    glBegin(GL_QUADS);
    glTexCoord2f(0, 1);
    glVertex2f(vLocation.X, vLocation.Y);
    glTexCoord2f(1, 1);
    glVertex2f(vLocation.X + vWidth, vLocation.Y);
    glTexCoord2f(1, 0);
    glVertex2f(vLocation.X + vWidth, vLocation.Y + vHeight);
    glTexCoord2f(0, 0);
    glVertex2f(vLocation.X, vLocation.Y + vHeight);
    if (AFont.Style and FontStyleBold) = 1 then
    begin
      glTexCoord2f(0, 1);
      glVertex2f(vLocation.X + 1, vLocation.Y);
      glTexCoord2f(1, 1);
      glVertex2f(vLocation.X + 1 + vWidth, vLocation.Y);
      glTexCoord2f(1, 0);
      glVertex2f(vLocation.X + 1 + vWidth, vLocation.Y + vHeight);
      glTexCoord2f(0, 0);
      glVertex2f(vLocation.X + 1, vLocation.Y + vHeight);

    end;
    glEnd;
    glBindTexture(GL_TEXTURE_2D, 0);

    glPopMatrix;
  end;
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
  SwapBuffers(FDC);
end;

function TOpenGLPainter.GetTextExtents(const AFont: TStyleFont; const AText: string): TSizeF;
var
  vChar: TCharacter;
  i: Cardinal;
  vTotalWidth: Single;
begin
  vTotalWidth := 0;
  for i := 1 to strlen(PWideChar(AText)) do
  begin
    if TTrueTypeFont(AFont.NativeObject).Characters.TryGetValue(Char(AText[i]), vChar) then
    begin
      vTotalWidth := vChar.Width + vTotalWidth;
      Result.cy := vChar.Height * 96 / 72;
    end;
  end;

  Result.cx := vTotalWidth;

end;

class function TGLUtils.HexToRGBA(const AColor: Cardinal): TColor;
begin
  Result.Alpha := (AColor shr 24) / 255;
  Result.Red := (AColor shr 16 and $FF) / 255;
  Result.Green := (AColor shr 8 and $FF) / 255;
  Result.Blue := (AColor and $FF) / 255;
end;

function TOpenGLPainter.SetContext(const AContext: TDrawContext): TDrawContext;
begin
  Result := AContext;
  glViewport(0, 0, FContext.Width, FContext.Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluOrtho2D(0, FContext.Width, 0, FContext.Height);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
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
begin
  Result := TOpenGLPainter.Create(AContainer);
end;

procedure TOpenGLScene.DoActivate;
begin
  if not FPanel.Focused then
    FPanel.SetFocus;
end;

function TOpenGLScene.DoCreateScene(const APlaceholder: TObject): TPainter;
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
end;

procedure TOpenGLScene.DoRender(const ANeedFullRepaint: Boolean);
var
  vOldContext: TDrawContext;
begin
  vOldContext := FPainter.SetContext(FStaticContext);
  try
    FPainter.BeginPaint;
    try
      FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
      FRoot.RenderDynamic(FPainter, GetSceneRect);
    finally
      FPainter.EndPaint;
    end;
  finally
    FPainter.SetContext(vOldContext);
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
  SetLength(vPixels, TGLUtils.NextPow2(AWidth), TGLUtils.NextPow2(AHeight), 4);
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
  //Добавить проверку на расширение файлов
  FTexture := TOpenGLTexture.Create;

  FTexture.LoadTextureFromImage(AImage.FileName);

  FWidth := FTexture.Width;
  FHeight := FTexture.Height;
end;

constructor TOpenGLImage.Create(AWidth, AHeight: Integer; APixels: TArray<GLubyte>);
begin
  FTexture := TOpenGLTexture.Create;
  FTexture.LoadTexture(AWidth, AHeight, APixels);
end;

constructor TOpenGLImage.Create(AWidth, AHeight: Integer; APixels: TArray<TArray<TArray<GLubyte>>>);
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

function TOpenGLTexture.LoadTexture(AWidth, AHeight: Integer; APixels: TArray<GLubyte>): GLuint;
begin
  FGLWidth := TGLUtils.NextPow2(AWidth);
  FGLHeight := TGLUtils.NextPow2(AHeight);
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

procedure TOpenGLTexture.LoadTextureFromImage(AFileName: string);
var
  vImage: TPngImage;
  i, j, cur: Cardinal;
  vPixels: TArray<GLubyte>;
begin
  if not TFile.Exists(AFileName) then
    Exit;
  vImage := TPngImage.Create;
  vImage.LoadFromFile(AFileName);
  SetLength(vPixels, TGLUtils.NextPow2(vImage.Width) * TGLUtils.NextPow2(vImage.Height) * 4);
  cur := 0;
  for i := 0 to TGLUtils.NextPow2(vImage.Height) - 1 do
  begin
    for j := 0 to TGLUtils.NextPow2(vImage.Width) - 1 do
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

procedure TOpenGLTexture.LoadTextureFromRawBytes(const APixels: TArray<TArray<TArray<GLubyte>>>; AWidth, AHeight: Integer);
var
  i, j, cur: Cardinal;
  vPixels: TArray<GLubyte>;
begin
  SetLength(vPixels, TGLUtils.NextPow2(AWidth) * TGLUtils.NextPow2(AHeight) * 4);
  cur := 0;
  for i := 0 to TGLUtils.NextPow2(AHeight) - 1 do
  begin
    for j := 0 to TGLUtils.NextPow2(AWidth) - 1 do
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

class function TGLUtils.NextPow2(const x: Cardinal): Cardinal;
var
  vMaxTextureSize: GLuint;
//    i, cur: Cardinal;
begin
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @vMaxTextureSize);
  Result := 2;
  while (Result < x) or (x > vMaxTextureSize) do
    Result := Result shl 1;
end;

{ TOpenGLPainter.TCharacter }

class function TCharacter.Create(ATextureID, AWidth, AHeight, ABearingLeft, ABearingTop, AAdvanceX, AAdvanceY: Cardinal): TCharacter;
begin
  Result.TextureID := ATextureID;
  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.BearingLeft := ABearingLeft;
  Result.BearingTop := ABearingTop;
  Result.AdvanceX := AAdvanceX;
  Result.AdvanceY := AAdvanceY;

end;

{ TTrueTypeFont }

constructor TTrueTypeFont.Create(const AChars: TDictionary<WideChar, TCharacter>);
begin
  FCharacters := AChars;
end;

destructor TTrueTypeFont.Destroy;
begin
  FreeAndNil(FCharacters);
end;

class function TGLUtils.NextPow2(const x: Single): Cardinal;
begin
  Result := TGLUtils.NextPow2(Cardinal(Round(x)));
end;

class function TGLUtils.PPointFToPoints(const APoints: PPointF; const ACount: Integer): TPointArray;
var
  i: Integer;
begin
  SetLength(Result, ACount);
  for i := 0 to ACount - 1 do
    Result[i] := PPointF(Integer(APoints) + i * SizeOf(TPointF))^.Round;
end;

initialization
  TBaseModule.RegisterModule('Painting', 'OpenGL', TOpenGLScene);

end.

