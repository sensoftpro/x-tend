unit uFMXScene;

interface

uses
  Messages, Classes, FMX.Graphics, FMX.Controls, FMX.Objects, FMX.Types, FMX.Forms,
  SysUtils, UITypes, uScene, uDrawStyles, uFMXPainter, FMX.StdCtrls, FMX.Platform, Types;

type
  TFMXContainer = class
  private
    FPlaceholder: TPaintBox;
    FCanvas: TCanvas;
  public
    constructor Create(const APlaceholder: TPaintBox);
    destructor Destroy; override;

    property Placeholder: TPaintBox read FPlaceholder;
    property Canvas: TCanvas read FCanvas write FCanvas;
  end;

  TFMXScene = class(TScene)
  private
    FPlaceholder: TPaintBox;
    FCachedDrawContext: TDrawContext;
    FDrawContext: TDrawContext;
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

    function CreatePainter(const AContainer: TObject): TPainter; override;
  end;

implementation

function TFMXScene.CreatePainter(const AContainer: TObject): TPainter;
var
  vContainer: TPaintBox absolute AContainer;
begin
  Result := TFMXPainter.Create(AContainer);
  FCachedDrawContext := Result.CreateDrawContext(vContainer.Width, vContainer.Height);
  FDrawContext := Result.CreateDrawContext(vContainer.Width, vContainer.Height);
end;

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
var
  vPlaceholder: TControl absolute APlaceholder;
begin
  FPlaceholder := TPaintBox(vPlaceholder);

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

  Result := CreatePainter(FPlaceholder);
end;

procedure TFMXScene.DoDestroyScene;
begin
end;

procedure TFMXScene.DoRedraw;
begin
  FPlaceholder.Repaint;
end;

procedure TFMXScene.DoRender(const ANeedFullRepaint: Boolean);
begin
  FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
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
  FDrawContext.SetSize(AWidth, AHeight);
end;

{ TFMXContainer }

constructor TFMXContainer.Create(const APlaceholder: TPaintBox);
begin
  inherited Create;
  FPlaceholder := APlaceholder;
  FCanvas := nil;
end;

destructor TFMXContainer.Destroy;
begin
  FPlaceholder := nil;
  FCanvas := nil;
  inherited Destroy;
end;

end.
