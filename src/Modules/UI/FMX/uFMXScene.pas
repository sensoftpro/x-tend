unit uFMXScene;

interface

uses
  Messages, Classes, FMX.Graphics, FMX.Controls, FMX.ExtCtrls, FMX.Types, FMX.Forms,
  SysUtils, UITypes, uScene, uDrawStyles, uFMXPainter, FMX.StdCtrls, FMX.Platform, Types;

type
  TDrawArea = class(TControl)
    private
      FOnPaint: TNotifyEvent;
    protected
      procedure Paint; override;
//      procedure Paint(Sender: TObject; Canvas: TCanvas); override;
    public
      property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TFMXScene = class(TScene)
  private
    FPlaceholder: TDrawArea;
    FCachedDrawContext: TDrawContext;
    FDrawContext: TDrawContext;
  protected
    function DoCreateScene(const APlaceholder: TObject): TPainter; override;
    procedure DoDestroyScene; override;
    procedure DoActivate; override;
    procedure SetEnabled(const AValue: Boolean); override;

    procedure DoRender(const ANeedFullRepaint: Boolean); override;
    procedure UpdateContexts(const AWidth, AHeight: Single); override;
    function GetImageContext: TDrawContext; override;

    function GetSceneRect: TRectF; override;
    function GetClientPos: TPointF; override;
    function GetScaleFactor: Single; override;

    function CreatePainter(const AContainer: TObject): TPainter; override;

    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); overload;
    procedure OnKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState); overload;
    procedure OnKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState); overload;
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single); overload;
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single); overload;
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single); overload;
  end;

implementation

function TFMXScene.CreatePainter(const AContainer: TObject): TPainter;
var
  vContainer: TDrawArea absolute AContainer;
begin
  Result := TFMXPainter.Create(vContainer);
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
  if (not FPlaceholder.IsFocused) and FPlaceholder.CanFocus and AllParentsVisible(FPlaceholder.GetControls[0]) then
    FPlaceholder.SetFocus;
end;

function TFMXScene.DoCreateScene(const APlaceholder: TObject): TPainter;
var
  vPlaceholder: TControl absolute APlaceholder;
begin
  FPlaceholder := TDrawArea.Create(vPlaceholder);
  FPlaceholder.Align := TAlignLayout.Client;
  FPlaceholder.Parent := vPlaceholder;
  FPlaceholder.Position.Y := vPlaceholder.Position.Y;
//  FPlaceholder.SetBoundsRect(vPlaceholder.AbsoluteRect);

  FPlaceholder.OnMouseWheel := OnMouseWheel;
  FPlaceholder.OnKeyDown := OnKeyDown;
  FPlaceholder.OnKeyUp := OnKeyUp;

  FPlaceholder.OnMouseDown := OnMouseDown;
  FPlaceholder.OnMouseUp := OnMouseUp;
  FPlaceholder.OnDblClick := OnDblClick;
  FPlaceholder.OnMouseMove := OnMouseMove;
  FPlaceholder.OnMouseLeave := OnMouseLeave;

  FPlaceholder.TabStop := True;
//  FPlaceholder.PaintTo(FPlaceholder.Canvas, FPlaceholder.GetAbsoluteRect, vPlaceholder);

  Result := CreatePainter(FPlaceholder);
end;

procedure TFMXScene.DoDestroyScene;
begin
end;

procedure TFMXScene.DoRender(const ANeedFullRepaint: Boolean);
var
  vContext: TDrawContext;
begin
//  FPainter.BeginPaint;
//  try
//    if ANeedFullRepaint then
//    begin
//      vContext := FPainter.SetContext(FCachedDrawContext);
//      try
//        FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
////        FCachedDrawContext.SaveToFile('static.bmp');
//      finally
//        FPainter.SetContext(vContext);
//      end;
//    end;
//
//    vContext := FPainter.SetContext(FDrawContext);
//    try
//      FPainter.DrawContext(FCachedDrawContext);
//      FRoot.RenderDynamic(FPainter, GetSceneRect);
////      FDrawContext.SaveToFile('dynamic.bmp');
//    finally
//      FPainter.SetContext(vContext);
//    end;
//  finally
//    FPainter.DrawContext(FDrawContext);
//    FPainter.EndPaint;
//  end;
  FPAinter.BeginPaint;
  if (ANeedFullRepaint) then
    FRoot.RenderStatic(FPainter, GetSceneRect, spmNormal);
  FRoot.RenderDynamic(FPainter, GetSceneRect);
  FPainter.EndPaint;
end;

function TFMXScene.GetClientPos: TPointF;
begin
  Result := Screen.MousePos;
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
  Result := FPlaceholder.GetLocalRect;
end;

procedure TFMXScene.OnKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  OnKeyDown(Sender, Key, Shift);
end;

procedure TFMXScene.OnKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  OnKeyUp(Sender, Key, Shift);
end;

procedure TFMXScene.OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  OnMouseDown(Sender, Button, Shift, Integer(Round(X)), Round(Y));
end;

procedure TFMXScene.OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  OnMouseMove(Sender, Shift, Integer(Round(X)), Round(Y));
end;

procedure TFMXScene.OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  OnMouseUp(Sender, Button, Shift, Integer(Round(X)), Round(Y));
end;

procedure TFMXScene.OnMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  OnMouseWheel(Sender, Shift, WheelDelta, Point(0,0), Handled);
end;

procedure TFMXScene.SetEnabled(const AValue: Boolean);
begin
  inherited SetEnabled(AValue);

  if AValue then
  begin
    FPlaceholder.OnResize := OnResize;
    FPlaceholder.OnPaint := OnPaint;
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

{ TDrawArea }

procedure TDrawArea.Paint;
begin
  inherited;
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

end.
