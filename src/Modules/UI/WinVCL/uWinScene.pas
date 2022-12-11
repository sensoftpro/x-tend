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

unit uWinScene;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, ExtCtrls,
  SysUtils, Types, UITypes, uScene, uDrawStyles;

type
  TLightPanel = class(TPanel)
  protected
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    property OnMouseWheel;
    property OnKeyDown;
    property OnKeyUp;
    property Canvas;
  end;

  TMyPanel = class(TLightPanel)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  public
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TDrawContainer = class
  private
    FHWND: THandle;
    FCanvas: TCanvas;
    FWidth, FHeight: Integer;
    function GetDC: THandle;
  public
    constructor Create(const AHWND: THandle; const ACanvas: TCanvas; const AWidth, AHeight: Single);
    destructor Destroy; override;

    property HWND: THandle read FHWND;
    property Canvas: TCanvas read FCanvas;
    property DC: THandle read GetDC;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

  TWinDrawContext = class(TDrawContext)
  private
    FBitmap: TBitmap;
    FCanvas: TCanvas;
    function GetDC: THandle;
  protected
    procedure DoSetSize(const AWidth, AHeight: Single); override;
  public
    procedure SaveToFile(const AFileName: string); override;
  public
    constructor Create(const APainter: TPainter; const ACanvas: TCanvas; const AWidth, AHeight: Single);
    destructor Destroy; override;

    property Canvas: TCanvas read FCanvas;
    property Handle: THandle read GetDC;
  end;

  TWinNinePatchImage = class(TNinePatchImage)
  protected
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject; override;
  end;

  TWinScene = class(TScene)
  private
    FPanel: TMyPanel;
    FContainer: TDrawContainer;
  protected
    function DoCreateScene(const APlaceholder: TObject): TPainter; override;
    procedure DoDestroyScene; override;
    procedure DoActivate; override;
    procedure SetEnabled(const AValue: Boolean); override;

    function GetSceneRect: TRectF; override;
    function GetClientPos: TPointF; override;
    function GetScaleFactor: Single; override;
  public
    property Panel: TMyPanel read FPanel;
  end;

  TWinCanvasScene = class(TWinScene)
  protected
    FCachedDrawContext: TDrawContext;
    FDrawContext: TDrawContext;
  protected
    procedure DoDestroyScene; override;
    procedure DoRender(const ANeedFullRepaint: Boolean); override;
    procedure UpdateContexts(const AWidth, AHeight: Single); override;
    function GetImageContext: TDrawContext; override;
  end;

implementation

uses
  PngImage;

{ TLightPanel }

procedure TLightPanel.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 0;
end;

procedure TLightPanel.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTARROWS;
end;

{ TMyPanel }

procedure TMyPanel.WMPaint(var Msg: TWMPaint);
var
  PaintStruct: TPaintStruct;
begin
  BeginPaint(Handle, PaintStruct);
  try
    if Assigned(FOnPaint) then
      FOnPaint(Self);
  finally
    EndPaint(Handle, PaintStruct);
  end;
end;

{ TWinNinePatchImage }

function TWinNinePatchImage.CutImage(const ALeft, ATop, AWidth, AHeight: Integer): TObject;
var
  i: Integer;
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

function TWinNinePatchImage.GetHeight: Integer;
begin
  Result := TGraphic(FImage).Height;
end;

function TWinNinePatchImage.GetWidth: Integer;
begin
  Result := TGraphic(FImage).Width;
end;

{ TWinScene }

function TWinScene.GetClientPos: TPointF;
var
  vClientPos: TPoint;
begin
  GetCursorPos(vClientPos);
  Result := FPanel.ScreenToClient(vClientPos);
end;

function TWinScene.GetSceneRect: TRectF;
begin
  Result := FPanel.ClientRect;
end;

function TWinScene.GetScaleFactor: Single;
begin
  Result := GetDeviceCaps(GetDC(0), LOGPIXELSY) / 96;
end;

procedure TWinScene.SetEnabled(const AValue: Boolean);
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

procedure TWinScene.DoActivate;
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

function TWinScene.DoCreateScene(const APlaceholder: TObject): TPainter;
var
  vControl: TWinControl absolute APlaceholder;
  vContainer: TDrawContainer;
begin
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

procedure TWinScene.DoDestroyScene;
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
end;

{ TWinCanvasScene }

procedure TWinCanvasScene.DoDestroyScene;
begin
  FreeAndNil(FDrawContext);
  FreeAndNil(FCachedDrawContext);
  inherited DoDestroyScene;
end;

procedure TWinCanvasScene.DoRender(const ANeedFullRepaint: Boolean);
var
  vContext: TDrawContext;
begin
  FPainter.BeginPaint;
  try
    if ANeedFullRepaint then
    begin
      vContext := FPainter.SetContext(FCachedDrawContext);
      try
        FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
        //FCachedDrawContext.SaveToFile('static.bmp');
      finally
        FPainter.SetContext(vContext);
      end;
    end;

    vContext := FPainter.SetContext(FDrawContext);
    try
      FPainter.DrawContext(FCachedDrawContext);
      FRoot.RenderDynamic(FPainter, GetSceneRect);
      //FDrawContext.SaveToFile('dynamic.bmp');
    finally
      FPainter.SetContext(vContext);
    end;
  finally
    FPainter.EndPaint;
  end;

  FPainter.DrawContext(FDrawContext);
end;

function TWinCanvasScene.GetImageContext: TDrawContext;
begin
  Result := FCachedDrawContext;
end;

procedure TWinCanvasScene.UpdateContexts(const AWidth, AHeight: Single);
begin
  FCachedDrawContext.SetSize(AWidth, AHeight);
  FDrawContext.SetSize(AWidth, AHeight);
end;

{ TWinDrawContext }

constructor TWinDrawContext.Create(const APainter: TPainter; const ACanvas: TCanvas; const AWidth, AHeight: Single);
begin
  if not Assigned(ACanvas) then
  begin
    FBitmap := TBitmap.Create;
    FBitmap.PixelFormat := pf32bit;
    FBitmap.SetSize(Round(AWidth), Round(AHeight));
    FCanvas := FBitmap.Canvas;
  end
  else begin
    FCanvas := ACanvas;
    FBitmap := nil;
  end;

  inherited Create(AWidth, AHeight);
end;

destructor TWinDrawContext.Destroy;
begin
  FCanvas := nil;
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TWinDrawContext.DoSetSize(const AWidth, AHeight: Single);
begin
  if Assigned(FBitmap) then
    FBitmap.SetSize(Round(AWidth), Round(AHeight));
end;

function TWinDrawContext.GetDC: THandle;
begin
  Result := FCanvas.Handle;
end;

procedure TWinDrawContext.SaveToFile(const AFileName: string);
begin
  if Assigned(FBitmap) then
    FBitmap.SaveToFile(AFileName);
end;

{ TDrawContainer }

constructor TDrawContainer.Create(const AHWND: THandle; const ACanvas: TCanvas; const AWidth, AHeight: Single);
begin
  inherited Create;
  FHWND := AHWND;
  FCanvas := ACanvas;
  FWidth := Round(AWidth);
  FHeight := Round(AHeight);
end;

destructor TDrawContainer.Destroy;
begin
  FCanvas := nil;
  inherited Destroy;
end;

function TDrawContainer.GetDC: THandle;
begin
  Result := FCanvas.Handle;
end;

end.
