unit uFMXScene;

interface

uses
  Messages, Classes, FMX.Graphics, FMX.Controls, FMX.Objects, FMX.Types, FMX.Forms,
  SysUtils, UITypes, uScene, uDrawStyles, uFMXPainter, FMX.StdCtrls, FMX.Platform, Types;

type
  TFMXContainer = class
  public
    Scene: TScene;
    Control: TControl;
    constructor Create(const AScene: TScene; const AControl: TControl);
  end;

  TFMXScene = class(TScene)
  private
    FPlaceholder: TPaintBox;
    FCachedDrawContext: TDrawContext;
    procedure OnFMXScenePaint(Sender: TObject; Canvas: TCanvas);
    procedure OnFMXSceneMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure OnFMXSceneKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure OnFMXSceneKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure OnFMXSceneMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnFMXSceneMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnFMXSceneMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
  protected
    function DoCreateScene(const APlaceholder: TObject): TPainter; override;
    procedure DoDestroyScene; override;
    procedure DoActivate; override;
    procedure DoRedraw; override;
    procedure SetEnabled(const AValue: Boolean); override;

    procedure DoRender(const ANeedFullRepaint: Boolean); override;
    procedure UpdateContexts(const AWidth, AHeight: Single); override;
    function GetImageContext: TDrawContext; override;

    function GetSceneRect: TRectF; override;
    function GetClientPos: TPointF; override;
    function GetScaleFactor: Single; override;
  end;

implementation

procedure TFMXScene.DoActivate;
  function AllParentsVisible(const AControl: TControl): Boolean;
  var
    vParent: TControl;
  begin
    Result := True;
    vParent := AControl.ParentControl;
    while Assigned(vParent) do
    begin
      if not vParent.Visible then
      begin
        Result := False;
        Break;
      end;
      vParent := vParent.ParentControl;
    end;
  end;
begin
  if (not FPlaceholder.IsFocused) and FPlaceholder.CanFocus and AllParentsVisible(FPlaceholder) then
    FPlaceholder.SetFocus;
end;

function TFMXScene.DoCreateScene(const APlaceholder: TObject): TPainter;
begin
  FPlaceholder := TPaintBox(APlaceholder);

  FPlaceholder.OnMouseWheel := OnFMXSceneMouseWheel;
  FPlaceholder.OnKeyDown := OnFMXSceneKeyDown;
  FPlaceholder.OnKeyUp := OnFMXSceneKeyUp;

  FPlaceholder.OnMouseDown := OnFMXSceneMouseDown;
  FPlaceholder.OnMouseUp := OnFMXSceneMouseUp;
  FPlaceholder.OnDblClick := OnDblClick;
  FPlaceholder.OnMouseMove := OnFMXSceneMouseMove;
  FPlaceholder.OnMouseLeave := OnMouseLeave;

  FPlaceholder.CanFocus := True;
  FPlaceholder.TabStop := True;

  Result := TFMXPainter.Create(Self, FPlaceholder);
  FCachedDrawContext := Result.CreateDrawContext(FPlaceholder.Width, FPlaceholder.Height);
end;

procedure TFMXScene.DoDestroyScene;
begin
  FreeAndNil(FCachedDrawContext);

  FPlaceholder.OnMouseWheel := nil;
  FPlaceholder.OnKeyDown := nil;
  FPlaceholder.OnKeyUp := nil;

  FPlaceholder.OnMouseDown := nil;
  FPlaceholder.OnMouseUp := nil;
  FPlaceholder.OnDblClick := nil;
  FPlaceholder.OnMouseMove := nil;
  FPlaceholder.OnMouseLeave := nil;

  FPlaceholder := nil;
end;

procedure TFMXScene.DoRedraw;
begin
  FPlaceholder.Repaint;
end;

procedure TFMXScene.DoRender(const ANeedFullRepaint: Boolean);
var
  vContext: TDrawContext;
begin
  if ANeedFullRepaint then
  begin
    vContext := FPainter.SetContext(FCachedDrawContext);
    FPainter.BeginPaint;
    try
      FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
    finally
      FPainter.EndPaint;
      FPainter.SetContext(vContext);
    end;
  end;

  FPainter.DrawContext(FCachedDrawContext);
  FRoot.RenderDynamic(FPainter, GetSceneRect);
end;

function TFMXScene.GetClientPos: TPointF;
begin
  Result := FPlaceholder.AbsoluteToLocal(Screen.MousePos);
end;

function TFMXScene.GetImageContext: TDrawContext;
begin
  Result := FCachedDrawContext;
end;

function TFMXScene.GetScaleFactor: Single;
var
  ScreenService: IFMXScreenService;
begin
  Result := 1;
  if TPlatformServices.Current.SupportsPlatformService (IFMXScreenService, IInterface(ScreenService)) then
    Result := ScreenService.GetScreenScale;
end;

function TFMXScene.GetSceneRect: TRectF;
begin
  Result := FPlaceholder.LocalRect;
end;

procedure TFMXScene.OnFMXSceneKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  OnKeyDown(Sender, Key, Shift);
end;

procedure TFMXScene.OnFMXSceneKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  OnKeyUp(Sender, Key, Shift);
end;

procedure TFMXScene.OnFMXSceneMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  OnMouseDown(Sender, Button, Shift, Integer(Round(X)), Round(Y));
end;

procedure TFMXScene.OnFMXSceneMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  OnMouseMove(Sender, Shift, Integer(Round(X)), Round(Y));
end;

procedure TFMXScene.OnFMXSceneMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  OnMouseUp(Sender, Button, Shift, Integer(Round(X)), Round(Y));
end;

procedure TFMXScene.OnFMXSceneMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  OnMouseWheel(Sender, Shift, WheelDelta, Point(0, 0), Handled);
end;

procedure TFMXScene.OnFMXScenePaint(Sender: TObject; Canvas: TCanvas);
begin
  OnPaint(Sender);
end;

procedure TFMXScene.SetEnabled(const AValue: Boolean);
begin
  inherited SetEnabled(AValue);

  if AValue then
  begin
    FPlaceholder.OnResize := OnResize;
    FPlaceholder.OnPaint := OnFMXScenePaint;
  end
  else begin
    FPlaceholder.OnResize := nil;
    FPlaceholder.OnPaint := nil;
  end;
end;

procedure TFMXScene.UpdateContexts(const AWidth, AHeight: Single);
begin
  FCachedDrawContext.SetSize(AWidth, AHeight);
end;

{ TFMXContainer }

constructor TFMXContainer.Create(const AScene: TScene;
  const AControl: TControl);
begin
  inherited Create;

  Scene := AScene;
  Control := AControl;
end;

end.
