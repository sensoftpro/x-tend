unit uniScene;

interface

uses
  Controls, Messages, Classes, Graphics, SysUtils, UITypes, uScene, uDrawStyles,
  uniGUIClasses, uniGUIAbstractClasses, uniGUIBaseClasses, uniGUITypes, UniCanvas,
  Types;

type
  TUniGUIScene = class(TScene)
  private
    FPanel: TUniCanvas;
    procedure DoAlignPosition(Sender: TWinControl;
      Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer;
      var AlignRect: TRect; AlignInfo: TAlignInfo);
    procedure DoResize(Sender: TUniControl; OldWidth,
      OldHeight: Integer);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X: Integer; Y:Integer);
    procedure OnCanvasReady(Sender: TObject);
  protected
    function DoCreateScene(const APlaceholder: TObject): TPainter; override;
    procedure DoDestroyScene; override;
    procedure DoActivate; override;
    procedure DoRedraw; override;
    procedure SetEnabled(const AValue: Boolean); override;
    procedure DoRender(const ANeedFullRepaint: Boolean); override;

    function GetSceneRect: TRectF; override;
    function GetClientPos: TPointF; override;
    function GetScaleFactor: Single; override;
  public
    property Control: TUniCanvas read FPanel;
  end;

implementation

uses
  Windows, uCommonVCLPainter, uniPanel, uniGUIForm, uModule;

type
  TCrackedUniCanvas = class(TUniCanvas) end;

{ TUniGUIScene }

procedure TUniGUIScene.DoActivate;
begin
  // SetFocus
end;

procedure TUniGUIScene.DoAlignPosition(Sender: TWinControl; Control: TControl;
  var NewLeft, NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect;
  AlignInfo: TAlignInfo);
begin
  OnResize(Sender);
end;

function TUniGUIScene.DoCreateScene(const APlaceholder: TObject): TPainter;
var
  vControl: TUniControl absolute APlaceholder;
  vContainer: TDrawContainer;
begin
  if APlaceholder is TUniForm then
    TUniForm(APlaceholder).OnResize := OnResize
  else if APlaceholder is TUniPanel then
    TUniPanel(APlaceholder).OnResize := DoResize;
  FPanel := TUniCanvas.Create(vControl);
  FPanel.Align := alClient;
  FPanel.Constraints.MinWidth := 200;
  FPanel.Constraints.MinHeight := 100;
  FPanel.Parent := vControl;

  FPanel.ControlStyle := FPanel.ControlStyle + [csOpaque];
  TCrackedUniCanvas(FPanel).OnMouseWheel := OnMouseWheel;
  TCrackedUniCanvas(FPanel).OnResize := DoResize;
  FPanel.OnKeyDown := OnKeyDown;
  FPanel.OnKeyUp := OnKeyUp;

  FPanel.OnMouseDown := OnMouseDown;
  FPanel.OnMouseUp := OnMouseUp;
  FPanel.OnDblClick := OnDblClick;
  TCrackedUniCanvas(FPanel).OnMouseMove := DoMouseMove;
  FPanel.OnMouseLeave := OnMouseLeave;
  FPanel.OnCanvasReady := OnCanvasReady;

  FPanel.DoubleBuffered := True;
  FPanel.TabStop := True;

  vContainer := TDrawContainer.Create(0, FPanel.BitmapCanvas, FPanel.ClientWidth, FPanel.ClientHeight);
  try
    Result := TCommonVCLPainter.Create(Self, vContainer);
  finally
    FreeAndNil(vContainer);
  end;
end;

procedure TUniGUIScene.DoDestroyScene;
begin
  FPanel.OnAlignPosition := nil;
  TCrackedUniCanvas(FPanel).OnMouseWheel := nil;
  FPanel.OnKeyUp := nil;
  FPanel.OnKeyDown := nil;
  FPanel.OnCanvasReady := nil;
  //FPanel.OnPaint := nil;
  FPanel.OnMouseDown := nil;
  FPanel.OnMouseUp := nil;
  FPanel.OnDblClick := nil;
  TCrackedUniCanvas(FPanel).OnMouseMove := nil;
  FPanel.OnMouseLeave := nil;

  FPanel := nil;
end;

procedure TUniGUIScene.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  OnMouseMove(Sender, Shift, X, Y);
  DoRedraw;
end;

procedure TUniGUIScene.DoRedraw;
begin
  OnPaint(FPanel);
end;

procedure TUniGUIScene.DoRender(const ANeedFullRepaint: Boolean);
begin
  FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
  FRoot.RenderDynamic(FPainter, GetSceneRect);
end;

procedure TUniGUIScene.DoResize(Sender: TUniControl; OldWidth,
  OldHeight: Integer);
begin
  FPanel.Align := alNone;
  FPanel.Align := alClient;
  FPanel.Bitmap.SetSize(FPanel.Width, FPanel.Height);
  OnResize(Sender);
  DoRedraw;
end;

function TUniGUIScene.GetClientPos: TPointF;
var
  vClientPos: TPoint;
begin
  GetCursorPos(vClientPos);
  Result := FPanel.ScreenToClient(vClientPos);
end;

function TUniGUIScene.GetScaleFactor: Single;
begin
  Result := 1;
end;

function TUniGUIScene.GetSceneRect: TRectF;
begin
  Result := FPanel.BoundsRect;
  Result.Offset(-Result.Left, -Result.Top);
end;

procedure TUniGUIScene.OnCanvasReady(Sender: TObject);
begin
  OnResize(Sender);
  DoRedraw;
end;

procedure TUniGUIScene.SetEnabled(const AValue: Boolean);
begin
  inherited SetEnabled(AValue);

  if AValue then
    FPanel.OnAlignPosition := DoAlignPosition
  else
    FPanel.OnAlignPosition := nil;
end;

initialization

TBaseModule.RegisterModule('Painting', '', 'UniGUI', TUniGUIScene);

end.
