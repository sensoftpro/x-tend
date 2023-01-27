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

unit vclScene;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, ExtCtrls,
  SysUtils, Types, UITypes, uScene, uDrawStyles, uCommonVCLPainter;

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

  TWinDrawContainer = TDrawContainer;

  TWinScene = class(TScene)
  private
    FPanel: TMyPanel;
  protected
    function DoCreateScene(const APlaceholder: TObject): TPainter; override;
    procedure DoDestroyScene; override;
    procedure DoActivate; override;
    procedure DoRedraw; override;
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

procedure TWinScene.DoRedraw;
begin
  FPanel.Repaint;
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

end.
