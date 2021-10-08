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

unit uWinScene;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, ExtCtrls,
  SysUtils, Types, UITypes, uScene;

type
  TMyPanel = class(TPanel)
  protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  public
    property OnMouseWheel;
    property OnKeyDown;
    property OnKeyUp;
  end;

  TWinScene = class(TScene)
  protected
    FPanel: TMyPanel;
    FPaintArea: TPaintBox;

    procedure DoCreateScene(const APlaceholder: TObject); override;
    procedure DoDestroyScene; override;
    procedure DoActivate; override;
    procedure DoRender; override;
    procedure DoEnableResize(const AOn: Boolean); override;

    function GetSceneRect: TRectF; override;
    function GetClientPos: TPointF; override;
  public
    property Panel: TMyPanel read FPanel;
  end;

implementation

{ TMyPanel }

procedure TMyPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 0;
end;

procedure TMyPanel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
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
  Result := FPaintArea.ClientRect;
end;

procedure TWinScene.DoActivate;
begin
  if not FPanel.Focused then
    FPanel.SetFocus;
end;

procedure TWinScene.DoCreateScene(const APlaceholder: TObject);
var
  vControl: TWinControl absolute APlaceholder;
begin
  FPanel := TMyPanel.Create(vControl);
  FPanel.BevelOuter := bvNone;
  FPanel.Align := alClient;
  FPanel.Constraints.MinWidth := 200;
  FPanel.Constraints.MinHeight := 100;
  FPanel.Parent := vControl;

  // Для того, чтобы панель приняла нормальный размер
  if Assigned(vControl) then
    FDrawContext.SetSize(FPanel.ClientWidth, FPanel.ClientHeight);

  FPanel.ControlStyle := FPanel.ControlStyle + [csOpaque];
  FPanel.OnMouseWheel := OnMouseWheel;
  FPanel.OnKeyDown := OnKeyDown;
  FPanel.OnKeyUp := OnKeyUp;
  FPanel.DoubleBuffered := True;
  FPanel.TabStop := True;

  FPaintArea := TPaintBox.Create(FPanel);
  FPaintArea.Parent := FPanel;
  FPaintArea.Align := alClient;
  FPaintArea.OnPaint := OnPaint;
  FPaintArea.OnMouseDown := OnMouseDown;
  FPaintArea.OnMouseUp := OnMouseUp;
  FPaintArea.OnDblClick := OnDblClick;
  FPaintArea.OnMouseMove := OnMouseMove;
  FPaintArea.OnMouseLeave := OnMouseLeave;
end;

procedure TWinScene.DoDestroyScene;
begin
  FPanel.OnResize := nil;
  FPanel.OnMouseWheel := nil;
  FPanel.OnKeyDown := nil;
  FPaintArea.OnPaint := nil;
  FPaintArea.OnMouseDown := nil;
  FPaintArea.OnMouseUp := nil;
  FPaintArea.OnDblClick := nil;
  FPaintArea.OnMouseMove := nil;
  FPaintArea.OnMouseLeave := nil;
  FPaintArea := nil;
  FPanel := nil;
end;

procedure TWinScene.DoEnableResize(const AOn: Boolean);
begin
  if AOn then
    FPanel.OnResize := OnResize
  else
    FPanel.OnResize := nil;
end;

procedure TWinScene.DoRender;
begin
  inherited DoRender;

  BitBlt(FPaintArea.Canvas.Handle, 0, 0, FDrawContext.Width, FDrawContext.Height,
    TCanvas(FDrawContext.Canvas).Handle, 0, 0, SRCCOPY);
end;

end.
