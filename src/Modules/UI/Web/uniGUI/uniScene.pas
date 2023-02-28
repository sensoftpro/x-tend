unit uniScene;

interface

uses
  Controls, Messages, Classes, Graphics, SysUtils, UITypes, uScene, uDrawStyles,
  uniGUIClasses, uniGUIAbstractClasses, uniGUIBaseClasses, uniGUITypes, UniCanvas,
  Types, uniGUIApplication;

type
  TUniGUIScene = class(TScene)
  private
    FPanel: TUniCanvas;
    FMousePos: TPointF;
    procedure DoAlignPosition(Sender: TWinControl;
      Control: TControl; var NewLeft, NewTop, NewWidth, NewHeight: Integer;
      var AlignRect: TRect; AlignInfo: TAlignInfo);
    procedure DoResize(Sender: TUniControl; OldWidth,
      OldHeight: Integer);
    procedure OnAjaxEvent(Sender: TComponent; EventName: string; Params: TUniStrings);
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

//  FPanel.OnMouseDown := OnMouseDown;
//  FPanel.OnMouseUp := OnMouseUp;
  FPanel.OnDblClick := OnDblClick;
  FPanel.OnAjaxEvent := OnAjaxEvent;
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
begin
  UniSession.JSCode('updateMousePosition("' + FPanel.JSName + '");');
  Result := FMousePos;
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

procedure TUniGUIScene.OnAjaxEvent(Sender: TComponent; EventName: string; Params: TUniStrings);
var
  X,Y: Integer;
  vMouseButton: TMouseButton;
  vHandled: Boolean;
  vShiftState: TShiftState;
begin
  if EventName = 'canvasmousemove' then
  begin
    X := Params['X'].AsInteger;
    Y := Params['Y'].AsInteger;
    vShiftState := [];
    case Params['ss'].AsInteger of
      0: vShiftState := [ssShift];
      1: vShiftState := [ssAlt];
      2: vShiftState := [ssCtrl];
      3: vShiftState := [ssLeft];
      4: vShiftState := [ssRight];
      5: vShiftState := [ssMiddle];
      9: vShiftState := [ssCommand];
    end;
    OnMouseMove(Sender, vShiftState, X, Y);
  end
  else if EventName = 'canvasmousedown' then
  begin
    X := Params['X'].AsInteger;
    Y := Params['Y'].AsInteger;
    vShiftState := [];
    case Params['ss'].AsInteger of
      0: vShiftState := [ssShift];
      1: vShiftState := [ssAlt];
      2: vShiftState := [ssCtrl];
      3: vShiftState := [ssLeft];
      4: vShiftState := [ssRight];
      5: vShiftState := [ssMiddle];
      9: vShiftState := [ssCommand];
    end;
    vMouseButton := TMouseButton(Params['mb'].AsInteger);
    OnMouseDown(Sender, vMouseButton , vShiftState, X, Y);
  end
  else if EventName = 'canvasmouseup' then
  begin
    X := Params['X'].AsInteger;
    Y := Params['Y'].AsInteger;
    vShiftState := [];
    case Params['ss'].AsInteger of
      0: vShiftState := [ssShift];
      1: vShiftState := [ssAlt];
      2: vShiftState := [ssCtrl];
      3: vShiftState := [ssLeft];
      4: vShiftState := [ssRight];
      5: vShiftState := [ssMiddle];
      9: vShiftState := [ssCommand];
    end;
    vMouseButton := TMouseButton(Params['mb'].AsInteger);
    OnMouseUp(Sender, vMouseButton , vShiftState, X, Y);
  end
  else if EventName = 'canvasmousewheel' then
  begin
    X := Params['X'].AsInteger;
    Y := Params['Y'].AsInteger;
    vHandled := false;
    OnMouseWheel(Sender, [], Params['wd'].AsInteger , Point(X,Y), vHandled);
  end
  else if EventName = 'canvasmouseupdate' then
  begin
    X := Params['X'].AsInteger;
    Y := Params['Y'].AsInteger;
    FMousePos := PointF(X,Y);
  end;
  Repaint;
end;

procedure TUniGUIScene.OnCanvasReady(Sender: TObject);
begin
  OnResize(Sender);
  DoRedraw;
  UniSession.JSCode('bindCanvas("' + FPanel.JSName + '");');
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
