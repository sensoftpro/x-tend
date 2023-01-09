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

unit uScene;

interface

uses
  Classes, SysUtils, Types, UITypes, Generics.Collections, uModule, uDrawStyles;

type
  TSceneObjectAnchor = (soaLeft, soaTop, soaRight, soaBottom);
  TSceneObjectAnchors = set of TSceneObjectAnchor;
  TSceneState = (ssCreating, ssNormal, ssUsed, ssDirty, ssDestroying);
  TScenePaintMode = (spmNormal, spmTile);

  TScene = class;

  TSceneObject = class
  private
    // Иерархия визуализации
    [Weak] FParent: TSceneObject;
    FChildren: TObjectList<TSceneObject>;

    // Будет отрисован?
    FVisible: Boolean;
    FFocused: Boolean;
    FHot: Boolean;
  protected
    [Weak] FScene: TScene;
    // Смещение относительно начала сцены
    FOffset: TPointF;
    // Область родителя, которую занимает этот объект
    FRect: TRectF;
    // Привязки для правильного растягивания/сжимания объектов
    FAnchors: TSceneObjectAnchors;
    // Стиль отображения
    [Weak] FStyle: TDrawStyle;

    procedure SetRect(const Value: TRectF);
    function PointInObject(const APoint: TPointF): Boolean;
    function SceneToClient(const APoint: TPointF): TPointF;

    property Parent: TSceneObject read FParent;
    property Children: TObjectList<TSceneObject> read FChildren;
  protected
    function CreateStyle(const AName: string): Boolean;
    // Действия по попадании мыши в границы этого объекта
    procedure Activate; virtual;
    // Действия по уходу мыши из границ этого объекта
    procedure Deactivate(const AHovering: TSceneObject); virtual;
    procedure SetFocused(const AValue: Boolean); virtual;

    procedure DoAddChild(const AChild: TSceneObject); virtual;
    procedure DoDeleteChild(const AChild: TSceneObject); virtual;

    // Вызываются исключительно сценой для отрисовки
    procedure DoRenderStatic(const APainter: TPainter; const ARect: TRectF;
      const AMode: TScenePaintMode = spmNormal); virtual;
    procedure DoRenderDynamic(const APainter: TPainter; const ARect: TRectF); virtual;

    // Уведомляет сцену о необходимости перерисовки
    procedure Invalidate(const AState: TSceneState = ssDirty);

    // Позволяет отреагировать на изменение родительской области
    procedure DoRectChanged(const AOldRect, ANewRect: TRectF); virtual;
    // Определяет верхний элемент, содержащий заданную точку
    function TryToActivate(const APoint: TPointF): TSceneObject; virtual;

    // Обработчики взаимодействия со средой
    procedure HandleMouseDown(Button: TMouseButton; Shift: TShiftState;
      ClientPos: TPointF; var AHandled: Boolean); virtual;
    procedure HandleMouseUp(Button: TMouseButton; Shift: TShiftState;
      ClientPos: TPointF; var AHandled: Boolean); virtual;
    procedure HandleDblClick(ClientPos: TPointF; var AHandled: Boolean); virtual;
    procedure HandleMouseMove(Shift: TShiftState; ClientPos: TPointF;
      var AHandled: Boolean); virtual;
    procedure HandleKeyDown(var Key: Word; Shift: TShiftState;
      var AHandled: Boolean); virtual;
    procedure HandleKeyUp(var Key: Word; Shift: TShiftState;
      var AHandled: Boolean); virtual;
    procedure HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      ClientPos: TPointF; var AHandled: Boolean); virtual;
  public
    constructor Create(const AScene: TScene; const AParent: TSceneObject;
      const ARect: TRectF);
    destructor Destroy; override;

    procedure RenderStatic(const APainter: TPainter; const ARect: TRectF;
      const AMode: TScenePaintMode = spmNormal);
    procedure RenderDynamic(const APainter: TPainter; const ARect: TRectF);

    procedure Repaint;

    procedure RectChanged(const AOldRect, ANewRect: TRectF);

    property ClientRect: TRectF read FRect;
    property Anchors: TSceneObjectAnchors read FAnchors write FAnchors;
    property Visible: Boolean read FVisible write FVisible;
    property Focused: Boolean read FFocused write SetFocused;
  end;

  TSceneGroup = class(TSceneObject)
  private
    FHotIndex: Integer;
    FCapturedIndex: Integer;
    FTileRect: TRectF;
    FTileImage: TDrawContext;
    FChildRects: TList<TRectF>;
    FStartPos: TPointF;
    function GetHotIndex(const AClientPos: TPointF): Integer;
    procedure UpdatePosition(const ANewPos: TPointF);
  protected
    procedure Deactivate(const AHovering: TSceneObject); override;
    procedure DoAddChild(const AChild: TSceneObject); override;
    procedure DoRenderStatic(const APainter: TPainter; const ARect: TRectF;
      const AMode: TScenePaintMode = spmNormal); override;
    procedure DoRectChanged(const AOldRect, ANewRect: TRectF); override;
    function TryToActivate(const APoint: TPointF): TSceneObject; override;

    // Обработчики взаимодействия со средой
    procedure HandleMouseDown(Button: TMouseButton; Shift: TShiftState;
      ClientPos: TPointF; var AHandled: Boolean); override;
    procedure HandleMouseUp(Button: TMouseButton; Shift: TShiftState;
      ClientPos: TPointF; var AHandled: Boolean); override;
    procedure HandleDblClick(ClientPos: TPointF; var AHandled: Boolean); override;
    procedure HandleMouseMove(Shift: TShiftState; ClientPos: TPointF;
      var AHandled: Boolean); override;
    procedure HandleKeyDown(var Key: Word; Shift: TShiftState;
      var AHandled: Boolean); override;
    procedure HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      ClientPos: TPointF; var AHandled: Boolean); override;
  public
    constructor Create(const AScene: TScene; const AParent: TSceneObject;
      const ARect, ATileRect: TRectF);
    destructor Destroy; override;
  end;

  TAliveRect = class(TSceneObject)
  private
    FInitialRect: TRectF;
    FActiveRect: TRectF;
    FFactor: Double;
    procedure UpdateActiveRect;
  protected
    procedure Activate; override;
    procedure Deactivate(const AHovering: TSceneObject); override;
    procedure DoRenderStatic(const APainter: TPainter; const ARect: TRectF;
      const AMode: TScenePaintMode = spmNormal); override;
    procedure DoRectChanged(const AOldRect, ANewRect: TRectF); override;
  public
    constructor Create(const AScene: TScene; const AParent: TSceneObject;
      const ARect: TRectF; const AColor, AFocusColor: TAlphaColor; const AFactor: Double);
  end;

  TScene = class(TBaseModule)
  private
    [Weak] FFocusedObject: TSceneObject;
    [Weak] FHoveredObject: TSceneObject;
    FEnabled: Boolean;
    procedure SetFocusedObject(const Value: TSceneObject);
    procedure SetHoveredObject(const Value: TSceneObject);
  protected
    FRoot: TSceneObject;
    FPainter: TPainter;
    FLockCount: Integer;
    FMousePos: TPointF;
    FState: TSceneState;

    procedure OnResize(Sender: TObject);
    procedure OnPaint(Sender: TObject);
    procedure OnMouseLeave(Sender: TObject);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OnDblClick(Sender: TObject);
    procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure OnMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Render;
    procedure Redraw;
    procedure Invalidate(const AState: TSceneState = ssDirty);
  protected
    function DoCreateScene(const APlaceholder: TObject): TPainter; virtual; abstract;
    procedure DoDestroyScene; virtual; abstract;
    procedure DoActivate; virtual;
    procedure DoRender(const ANeedFullRepaint: Boolean); virtual;
    procedure DoRedraw; virtual;
    procedure SetEnabled(const AValue: Boolean); virtual;
    function GetSceneRect: TRectF; virtual; abstract;
    function GetClientPos: TPointF; virtual; abstract;
    function CreatePainter(const AContainer: TObject): TPainter; virtual; abstract;
    function GetImageContext: TDrawContext; virtual;
    procedure UpdateContexts(const AWidth, AHeight: Single); virtual;
    function GetScaleFactor: Single; virtual;
  public
    constructor Create(const APlaceholder: TObject); virtual;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Repaint;
    procedure FullRefresh;
    procedure Activate;
    procedure SaveToFile(const AFileName: string; const AWaterMark: string = ''); virtual;

    property Painter: TPainter read FPainter;
    property FocusedObject: TSceneObject read FFocusedObject write SetFocusedObject;
    property HoveredObject: TSceneObject read FHoveredObject write SetHoveredObject;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property ClientRect: TRectF read GetSceneRect;
    property ScaleFactor: Single read GetScaleFactor;
  end;

  TSceneClass = class of TScene;

implementation

{ TSceneObject }

procedure TSceneObject.Activate;
begin
  if FHot then
    Exit;

  FHot := True;
  if Assigned(FParent) then
    FParent.Activate;
end;

constructor TSceneObject.Create(const AScene: TScene; const AParent: TSceneObject;
  const ARect: TRectF);
begin
  inherited Create;

  FScene := AScene;
  if Assigned(AParent) then
    FParent := AParent
  else if Assigned(FScene.FRoot) then
    FParent := FScene.FRoot;
  FRect := ARect;
  FAnchors := [soaLeft, soaTop];

  FVisible := True;
  FHot := False;
  FFocused := False;

  FChildren := TObjectList<TSceneObject>.Create;

  if Assigned(FParent) then
    FParent.DoAddChild(Self);

  FOffset := PointF(0, 0);
end;

function TSceneObject.CreateStyle(const AName: string): Boolean;
begin
  Result := not FScene.FPainter.StyleExists(AName);
  if Result then
    FStyle := FScene.FPainter.AddStyle(AName)
  else
    FStyle := FScene.FPainter.GetStyle(AName);
end;

procedure TSceneObject.Deactivate(const AHovering: TSceneObject);
begin
  if not FHot or (Self = AHovering) then
    Exit;

  FHot := False;
  if Assigned(FParent) then
    FParent.Deactivate(AHovering);
end;

destructor TSceneObject.Destroy;
begin
  FParent := nil;

  FChildren.Free;

  if Self = FScene.HoveredObject then
    FScene.HoveredObject := nil;

  if Self = FScene.FocusedObject then
    FScene.FocusedObject := nil;

  FStyle := nil;
  FParent := nil;
  FScene := nil;

  inherited Destroy;
end;

procedure TSceneObject.DoAddChild(const AChild: TSceneObject);
begin
  FChildren.Add(AChild);
end;

procedure TSceneObject.DoDeleteChild(const AChild: TSceneObject);
begin
  FChildren.Remove(AChild);
end;

procedure TSceneObject.DoRectChanged(const AOldRect, ANewRect: TRectF);
var
  vSize: Single;
  vDiff: Single;
  vNewRect: TRectF;
begin
  vNewRect := FRect;

  vDiff := ANewRect.Width - AOldRect.Width;
  if (soaLeft in FAnchors) and (soaRight in FAnchors) then
    vNewRect.Right := FRect.Right + vDiff
  else begin
    vSize := FRect.Width;
    if soaRight in FAnchors then
    begin
      vNewRect.Right := FRect.Right + vDiff;
      vNewRect.Left := vNewRect.Right - vSize;
    end
    else if not (soaLeft in FAnchors) then
    begin
      if AOldRect.Width = vSize then
        vNewRect.Left := FRect.Left + vDiff / 2
      else
        vNewRect.Left := FRect.Left * (ANewRect.Width - vSize) / (AOldRect.Width - vSize);

      vNewRect.Right := vNewRect.Left + vSize;
    end;
  end;

  vDiff := ANewRect.Height - AOldRect.Height;
  if (soaTop in FAnchors) and (soaBottom in FAnchors) then
    vNewRect.Bottom := FRect.Bottom + vDiff
  else begin
    vSize := FRect.Height;
    if soaBottom in FAnchors then
    begin
      vNewRect.Bottom := FRect.Bottom + vDiff;
      vNewRect.Top := vNewRect.Bottom - vSize;
    end
    else if not (soaTop in FAnchors) then
    begin
      if AOldRect.Height = vSize then
        vNewRect.Top := FRect.Top + vDiff / 2
      else
        vNewRect.Top := FRect.Top * (ANewRect.Height - vSize) / (AOldRect.Height - vSize);

      vNewRect.Bottom := vNewRect.Top + vSize;
    end;
  end;

  SetRect(vNewRect);
end;

procedure TSceneObject.DoRenderStatic(const APainter: TPainter; const ARect: TRectF;
  const AMode: TScenePaintMode = spmNormal);
var
  vSceneObject: TSceneObject;
begin
  for vSceneObject in FChildren do
    vSceneObject.RenderStatic(APainter, ARect, AMode);
end;

procedure TSceneObject.DoRenderDynamic(const APainter: TPainter; const ARect: TRectF);
var
  vSceneObject: TSceneObject;
begin
  for vSceneObject in FChildren do
    vSceneObject.RenderDynamic(APainter, ARect);
end;

procedure TSceneObject.HandleKeyDown(var Key: Word; Shift: TShiftState;
  var AHandled: Boolean);
begin
end;

procedure TSceneObject.HandleKeyUp(var Key: Word; Shift: TShiftState; var AHandled: Boolean);
begin
end;

procedure TSceneObject.HandleDblClick(ClientPos: TPointF; var AHandled: Boolean);
begin
end;

procedure TSceneObject.HandleMouseDown(Button: TMouseButton; Shift: TShiftState;
  ClientPos: TPointF; var AHandled: Boolean);
begin
end;

procedure TSceneObject.HandleMouseMove(Shift: TShiftState; ClientPos: TPointF;
  var AHandled: Boolean);
begin
end;

procedure TSceneObject.HandleMouseUp(Button: TMouseButton; Shift: TShiftState;
  ClientPos: TPointF; var AHandled: Boolean);
begin
end;

procedure TSceneObject.HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  ClientPos: TPointF; var AHandled: Boolean);
begin
end;

procedure TSceneObject.Invalidate(const AState: TSceneState = ssDirty);
begin
  FScene.Invalidate(AState);
end;

function TSceneObject.PointInObject(const APoint: TPointF): Boolean;
begin
  Result := PtInRect(FRect, SceneToClient(APoint));
end;

procedure TSceneObject.RectChanged(const AOldRect, ANewRect: TRectF);
begin
  if FAnchors <> [soaTop, soaLeft] then
    DoRectChanged(AOldRect, ANewRect);
end;

procedure TSceneObject.RenderStatic(const APainter: TPainter; const ARect: TRectF;
  const AMode: TScenePaintMode = spmNormal);
var
  vRealRect: TRectF;
begin
  if FVisible then
  begin
    if AMode = spmNormal then
    begin
      vRealRect := FRect;
      FOffset := ARect.TopLeft;
      OffsetRect(vRealRect, ARect.Left, ARect.Top);
    end
    else
      vRealRect := ARect;

    DoRenderStatic(APainter, vRealRect, AMode);
  end;
end;

procedure TSceneObject.RenderDynamic(const APainter: TPainter; const ARect: TRectF);
var
  vRealRect: TRectF;
begin
  if FVisible then
  begin
    vRealRect := FRect;
    OffsetRect(vRealRect, ARect.Left, ARect.Top);
    DoRenderDynamic(APainter, vRealRect);
  end;
end;

procedure TSceneObject.Repaint;
begin
  FScene.Repaint;
end;

function TSceneObject.SceneToClient(const APoint: TPointF): TPointF;
begin
  Result := PointF(APoint.X - FOffset.X, APoint.Y - FOffset.Y);
end;

procedure TSceneObject.SetFocused(const AValue: Boolean);
begin
  if FFocused = AValue then
    Exit;

  FFocused := AValue;
  Invalidate;
end;

procedure TSceneObject.SetRect(const Value: TRectF);
var
  vSceneObject: TSceneObject;
begin
  if EqualRect(FRect, Value) then
    Exit;

  FScene.BeginUpdate;
  try
    for vSceneObject in FChildren do
      vSceneObject.RectChanged(FRect, Value);
  finally
    FRect := Value;
    FScene.EndUpdate;
  end;
end;

function TSceneObject.TryToActivate(const APoint: TPointF): TSceneObject;
var
  i: Integer;
begin
  Result := nil;

  if not FVisible then
    Exit;

  for i := FChildren.Count - 1 downto 0 do
  begin
    Result := FChildren[i].TryToActivate(APoint);
    if Assigned(Result) then
      Exit;
  end;

  if PointInObject(APoint) then
    Result := Self;
end;

{ TScene }

procedure TScene.Activate;
begin
  DoActivate;
  if Assigned(FRoot) and (FRoot.Children.Count > 0) then
    SetFocusedObject(FRoot.Children[0]);
end;

procedure TScene.BeginUpdate;
begin
  FLockCount := FLockCount + 1;
end;

constructor TScene.Create(const APlaceholder: TObject);
begin
  inherited Create;

  FState := ssCreating;
  FEnabled := False;

  FFocusedObject := nil;
  FHoveredObject := nil;

  FLockCount := 0;

  FPainter := DoCreateScene(APlaceholder);

  FRoot := TSceneObject.Create(Self, nil, GetSceneRect);
  FMousePos := PointF(-1, -1);

  Enabled := True;
  FState := ssDirty;
end;

destructor TScene.Destroy;
begin
  FState := ssDestroying;
  FreeAndNil(FPainter);

  DoDestroyScene;

  FFocusedObject := nil;
  FHoveredObject := nil;
  FreeAndNil(FRoot);

  inherited Destroy;
end;

procedure TScene.EndUpdate;
begin
  FLockCount := FLockCount - 1;
  if FLockCount = 0 then
    Render;
end;

procedure TScene.FullRefresh;
begin
  Invalidate(ssDirty);
  Repaint;
end;

function TScene.GetImageContext: TDrawContext;
begin
  Result := nil;
end;

Function TScene.GetScaleFactor: Single;
begin
  Result := 1;
end;

procedure TScene.Invalidate(const AState: TSceneState = ssDirty);
begin
  if FState in [ssNormal, ssUsed] then
    FState := AState;
end;

procedure TScene.DoActivate;
begin
end;

procedure TScene.DoRedraw;
begin
  OnPaint(nil);
end;

procedure TScene.DoRender(const ANeedFullRepaint: Boolean);
begin
end;

procedure TScene.OnDblClick(Sender: TObject);
var
  vClientPos: TPointF;
  vHandled: Boolean;
  vCurObject: TSceneObject;
begin
  DoActivate;

  vClientPos := GetClientPos;
  if not PtInRect(FRoot.ClientRect, vClientPos) then
    Exit;

  if Assigned(FHoveredObject) then
  begin
    vHandled := True;
    vCurObject := FHoveredObject;
    repeat
      vCurObject.HandleDblClick(vCurObject.SceneToClient(vClientPos), vHandled);
      vCurObject := vCurObject.Parent;
    until vHandled or not Assigned(vCurObject);
    Render;
  end;
end;

procedure TScene.OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  vHandled: Boolean;
  vCurObject: TSceneObject;
begin
  if Assigned(FFocusedObject) then
  begin
    vHandled := True;
    vCurObject := FFocusedObject;
    repeat
      vCurObject.HandleKeyDown(Key, Shift, vHandled);
      vCurObject := vCurObject.Parent;
    until vHandled or not Assigned(vCurObject);
    Render;
  end;
end;

procedure TScene.OnKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  vHandled: Boolean;
  vCurObject: TSceneObject;
begin
  if Assigned(FFocusedObject) then
  begin
    vHandled := True;
    vCurObject := FFocusedObject;
    repeat
      vCurObject.HandleKeyUp(Key, Shift, vHandled);
      vCurObject := vCurObject.Parent;
    until vHandled or not Assigned(vCurObject);
    Render;
  end;
end;

procedure TScene.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vClientPos: TPointF;
  vHandled: Boolean;
  vCurObject: TSceneObject;
begin
  DoActivate;

  vClientPos := PointF(X, Y);
  SetFocusedObject(FRoot.TryToActivate(vClientPos));
  if Assigned(FFocusedObject) then
  begin
    vHandled := True;
    vCurObject := FFocusedObject;
    repeat
      vCurObject.HandleMouseDown(Button, Shift, vCurObject.SceneToClient(vClientPos), vHandled);
      vCurObject := vCurObject.Parent;
    until vHandled or not Assigned(vCurObject);
    Render;
  end;
end;

procedure TScene.OnMouseLeave(Sender: TObject);
begin
  if (FMousePos.X < 0) and (FMousePos.Y < 0) then
    Exit;

  FMousePos := PointF(-1, -1);
  SetHoveredObject(nil);
end;

procedure TScene.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  vMousePos: TPointF;
  vHandled: Boolean;
  vCurObject: TSceneObject;
begin
  if PtInRect(FRoot.ClientRect, Point(X, Y)) then
    vMousePos := PointF(X, Y)
  else
    vMousePos := PointF(-1, -1);

  if FMousePos = vMousePos then
    Exit;

  FMousePos := vMousePos;
  //if ssLeft in Shift then
  //begin
    // Зажата кнопка мыши, это либо перемещение, либо выделение
    // пока не обрабатываем, временная заглушка
  //end
  //else begin
    SetHoveredObject(FRoot.TryToActivate(FMousePos));
    if Assigned(FHoveredObject) then
    begin
      vHandled := True;
      vCurObject := FHoveredObject;
      repeat
        vCurObject.HandleMouseMove(Shift, vCurObject.SceneToClient(FMousePos), vHandled);
        vCurObject := vCurObject.Parent;
      until vHandled or not Assigned(vCurObject);
      Render;
    end;
  //end;
end;

procedure TScene.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vClientPos: TPointF;
  vHandled: Boolean;
  vCurObject: TSceneObject;
begin
  if Assigned(FFocusedObject) then
  begin
    vClientPos := PointF(X, Y);
    vHandled := True;
    vCurObject := FFocusedObject;
    repeat
      vCurObject.HandleMouseUp(Button, Shift, vCurObject.SceneToClient(vClientPos), vHandled);
      vCurObject := vCurObject.Parent;
    until vHandled or not Assigned(vCurObject);
    Render;
  end;
end;

procedure TScene.OnMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  vClientPos: TPointF;
  vCurObject: TSceneObject;
begin
  Handled := False;

  vClientPos := GetClientPos;
  if not PtInRect(FRoot.ClientRect, vClientPos) then
    Exit;

  Handled := True;

  if Assigned(FHoveredObject) then
  begin
    vCurObject := FHoveredObject;
    repeat
      vCurObject.HandleMouseWheel(Shift, WheelDelta, vCurObject.SceneToClient(vClientPos), Handled);
      vCurObject := vCurObject.Parent;
    until Handled or not Assigned(vCurObject);
    Render;
  end;
end;

procedure TScene.OnPaint(Sender: TObject);
var
  vNeedFullRepaint: Boolean;
begin
  if FLockCount > 0 then
    Exit;

  if FState = ssNormal then
    Invalidate(ssUsed);

  vNeedFullRepaint := FState = ssDirty;

  FState := ssNormal;

  DoRender(vNeedFullRepaint);
end;

procedure TScene.OnResize(Sender: TObject);
var
  vSceneRect: TRectF;
begin
  vSceneRect := GetSceneRect;
  if EqualRect(FRoot.ClientRect, vSceneRect) and (FState <> ssCreating) then
    Exit;

  FState := ssNormal;
  FPainter.Context.SetSize(vSceneRect.Width, vSceneRect.Height);
  UpdateContexts(vSceneRect.Width, vSceneRect.Height);

  BeginUpdate;
  try
    FMousePos := PointF(-1, -1);
    FRoot.SetRect(RectF(0, 0, vSceneRect.Width, vSceneRect.Height));
    Invalidate;
  finally
    EndUpdate;
  end;
end;

procedure TScene.Redraw;
begin
  if not (FState in [ssUsed, ssDirty]) then
    Exit;

  DoRedraw;
end;

procedure TScene.Render;
begin
  if not (FState in [ssUsed, ssDirty]) then
    Exit;

  DoRedraw;
end;

procedure TScene.Repaint;
begin
  if FState = ssNormal then
    Invalidate;
  if (FState in [ssUsed, ssDirty]) and (FLockCount = 0) then
    Render;
end;

procedure TScene.SaveToFile(const AFileName, AWaterMark: string);
var
  vImageContext: TDrawContext;
begin
  vImageContext := GetImageContext;
  if Assigned(vImageContext) then
  begin
    Repaint;
    vImageContext.SaveToFile(AFileName);
  end;
end;

procedure TScene.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue;
end;

procedure TScene.SetFocusedObject(const Value: TSceneObject);
begin
  if FState in [ssCreating, ssDestroying] then
    Exit;

  if FFocusedObject = Value then
    Exit;

  if Assigned(FFocusedObject) then
    FFocusedObject.Focused := False;
  FFocusedObject := Value;
  if Assigned(FFocusedObject) then
    FFocusedObject.Focused := True;

  Render;
end;

procedure TScene.SetHoveredObject(const Value: TSceneObject);
begin
  if FState in [ssCreating, ssDestroying] then
    Exit;

  if FHoveredObject = Value then
    Exit;

  // В процедурах активации/деактивации нужно фиксировать области для перерисовки
  if Assigned(FHoveredObject) then
    FHoveredObject.Deactivate(Value);
  FHoveredObject := Value;
  if Assigned(FHoveredObject) then
    FHoveredObject.Activate;

  Render;
end;

procedure TScene.UpdateContexts(const AWidth, AHeight: Single);
begin
end;

{ TAliveRect }

procedure TAliveRect.Activate;
begin
  inherited Activate;
  SetRect(FActiveRect);
  Invalidate;
end;

constructor TAliveRect.Create(const AScene: TScene; const AParent: TSceneObject;
  const ARect: TRectF; const AColor, AFocusColor: TAlphaColor; const AFactor: Double);
begin
  inherited Create(AScene, AParent, ARect);

  if CreateStyle('rect') then
  begin
    FStyle.AddFillParams('normal', AColor);
    FStyle.AddFillParams('focused', AFocusColor);

    FStyle.CreateDrawObjects(AScene.Painter);
  end;

  FFactor := AFactor;
  FInitialRect := FRect;
  UpdateActiveRect;
end;

procedure TAliveRect.Deactivate(const AHovering: TSceneObject);
begin
  inherited Deactivate(AHovering);
  SetRect(FInitialRect);
  Invalidate;
end;

procedure TAliveRect.DoRectChanged(const AOldRect, ANewRect: TRectF);
begin
  inherited DoRectChanged(AOldRect, ANewRect);
  FInitialRect := FRect;
  UpdateActiveRect;
end;

procedure TAliveRect.DoRenderStatic(const APainter: TPainter; const ARect: TRectF;
  const AMode: TScenePaintMode = spmNormal);
begin
  if Focused then
    APainter.DrawRect(FStyle, 'focused', '', ARect)
  else
    APainter.DrawRect(FStyle, 'normal', '', ARect);

  inherited DoRenderStatic(APainter, ARect, AMode);
end;

procedure TAliveRect.UpdateActiveRect;
begin
  FActiveRect.Left := FRect.Left - (FRect.Right - FRect.Left) * (FFactor - 1) / 2;
  FActiveRect.Right := FRect.Right + (FRect.Right - FRect.Left) * (FFactor - 1) / 2;
  FActiveRect.Top := FRect.Top - (FRect.Bottom - FRect.Top) * (FFactor - 1) / 2;
  FActiveRect.Bottom := FRect.Bottom + (FRect.Bottom - FRect.Top) * (FFactor - 1) / 2;
end;

{ TSceneGroup }

constructor TSceneGroup.Create(const AScene: TScene;
  const AParent: TSceneObject; const ARect, ATileRect: TRectF);
begin
  inherited Create(AScene, AParent, ARect);

  if CreateStyle('group') then
    FStyle.AddStrokeParams('frame', TAlphaColorRec.Alpha or TAlphaColor($333333), 2);

  FHotIndex := -1;
  FCapturedIndex := -1;
  FTileRect := ATileRect;
  FAnchors := [soaLeft, soaRight, soaTop, soaBottom];

  FTileImage := AScene.FPainter.CreateDrawContext(ATileRect.Width, ATileRect.Height);

  FChildRects := TList<TRectF>.Create;
end;

procedure TSceneGroup.Deactivate(const AHovering: TSceneObject);
begin
  inherited Deactivate(AHovering);
  if FCapturedIndex > 0 then
    FCapturedIndex := -1;
  if FHotIndex > 0 then
    FHotIndex := -1;
  if FChildren.Count > 0 then
    TSceneObject(FChildren[0]).Deactivate(AHovering);
end;

destructor TSceneGroup.Destroy;
begin
  inherited Destroy;

  FreeAndNil(FChildRects);
  FreeAndNil(FTileImage);
end;

procedure TSceneGroup.DoAddChild(const AChild: TSceneObject);
var
  vRect: TRectF;
  vTileRect: TRectF;
  vStart: TPointF;
begin
  inherited DoAddChild(AChild);
  vRect := ClientRect;
  OffsetRect(vRect, -vRect.Left, -vRect.Top);
  AChild.SetRect(vRect);
  AChild.Anchors := [soaLeft, soaRight, soaTop, soaBottom];

  vStart.X := FRect.Right - 10 - FTileRect.Width;
  vStart.Y := FRect.Bottom - 10 - FTileRect.Height - (FChildren.Count - 2) * (FTileRect.Height + 10);
  vTileRect := RectF(vStart.X, vStart.Y, vStart.X + FTileRect.Width, vStart.Y + FTileRect.Height);
  FChildRects.Add(vTileRect);
end;

procedure TSceneGroup.DoRectChanged(const AOldRect, ANewRect: TRectF);
var
  i: Integer;
  vRect: TRectF;
  vSize: Single;
begin
  inherited DoRectChanged(AOldRect, ANewRect);

  for i := 1 to FChildRects.Count - 1 do
  begin
    vRect := FChildRects[i];
    vSize := vRect.Width;
    vRect.Right := vRect.Right + ANewRect.Width - AOldRect.Width;
    vRect.Left := vRect.Right - vSize;
    vSize := vRect.Height;
    vRect.Bottom := vRect.Bottom + ANewRect.Height - AOldRect.Height;
    vRect.Top := vRect.Bottom - vSize;
    FChildRects[i] := vRect;
  end;
end;

procedure TSceneGroup.DoRenderStatic(const APainter: TPainter; const ARect: TRectF;
  const AMode: TScenePaintMode);
var
  i: Integer;
  vRect: TRectF;
  vTileRect: TRectF;
begin
  if AMode <> spmNormal then
    Exit;
  if FChildren.Count = 0 then
    Exit;

  if not FChildren[0].Visible then
  begin
    for i := FChildren.Count - 1 downto 1 do
      if FChildren[i].Visible then
      begin
        FChildren.Exchange(0, i);
        Break;
      end;
  end;

  APainter.SetContext(FTileImage);
  FChildren[0].RenderStatic(APainter, ARect, AMode);

  vTileRect := FTileRect;
  InflateRect(vTileRect, -1, -1);

  for i := 1 to FChildren.Count - 1 do
  begin
    if not FChildren[i].Visible then
      Continue;

    FChildren[i].RenderStatic(APainter, vTileRect, spmTile);
    APainter.DrawRect(FStyle, '', 'frame', FTileRect);

    vRect := FChildRects[i];
    OffsetRect(vRect, ARect.Left, ARect.Top);
    //if i = FHotIndex then
    //  APainter.DrawImage(FTileImage.Image, vRect)
    //else
    //  APainter.DrawImage(FTileImage.Image, vRect, 0.5);
  end;
end;

function TSceneGroup.GetHotIndex(const AClientPos: TPointF): Integer;
var
  i: Integer;
  vRect: TRectF;
begin
  if FChildren.Count = 0 then
  begin
    Result := -1;
    Exit;
  end;

  Result := 0;
  for i := 1 to FChildRects.Count - 1 do
  begin
    vRect := FChildRects[i];
    vRect.Offset(FRect.Left, FRect.Top);
    if vRect.Contains(AClientPos) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TSceneGroup.HandleDblClick(ClientPos: TPointF; var AHandled: Boolean);
begin
  FHotIndex := GetHotIndex(ClientPos);
  if FHotIndex < 0 then
    Exit;

  if FHotIndex = 0 then
    FChildren[0].HandleDblClick(ClientPos, AHandled)
  else begin
    FChildren.Exchange(0, FHotIndex);
    Invalidate;
  end;
end;

procedure TSceneGroup.HandleKeyDown(var Key: Word; Shift: TShiftState;
  var AHandled: Boolean);
begin
  if FChildren.Count > 0 then
    FChildren[0].HandleKeyDown(Key, Shift, AHandled);
end;

procedure TSceneGroup.HandleMouseDown(Button: TMouseButton; Shift: TShiftState;
  ClientPos: TPointF; var AHandled: Boolean);
var
  vHotIndex: Integer;
begin
  vHotIndex := GetHotIndex(ClientPos);
  if vHotIndex = 0 then
    FChildren[0].HandleMouseDown(Button, Shift, ClientPos, AHandled)
  else if vHotIndex > 0 then
  begin
    if Button <> TMouseButton.mbLeft then
      Exit;
    FStartPos := ClientPos;
    FCapturedIndex := vHotIndex;
  end;
end;

procedure TSceneGroup.HandleMouseMove(Shift: TShiftState; ClientPos: TPointF;
  var AHandled: Boolean);
var
  vNewHotIndex: Integer;
begin
  if FCapturedIndex > 0 then
  begin
    UpdatePosition(ClientPos);
    Exit;
  end;

  vNewHotIndex := GetHotIndex(ClientPos);
  FChildren[0].HandleMouseMove(Shift, ClientPos, AHandled);

  if FHotIndex = vNewHotIndex then
    Exit;

  FHotIndex := vNewHotIndex;
  Invalidate;
end;

procedure TSceneGroup.HandleMouseUp(Button: TMouseButton; Shift: TShiftState;
  ClientPos: TPointF; var AHandled: Boolean);
begin
  if FCapturedIndex < 0 then
    FChildren[0].HandleMouseUp(Button, Shift, ClientPos, AHandled)
  else begin
    UpdatePosition(ClientPos);
    FCapturedIndex := -1;
  end;
end;

procedure TSceneGroup.HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  ClientPos: TPointF; var AHandled: Boolean);
begin
  if FChildren.Count > 0 then
    FChildren[0].HandleMouseWheel(Shift, WheelDelta, ClientPos, AHandled);
end;

function TSceneGroup.TryToActivate(const APoint: TPointF): TSceneObject;
begin
  if PointInObject(APoint) then
    Result := Self
  else
    Result := nil;
end;

procedure TSceneGroup.UpdatePosition(const ANewPos: TPointF);
begin
  // TODO: Сделать так, чтобы прямоугольники не выходили за границы (0, 0, FRect.Width, FRect.Height)
  FChildRects[FCapturedIndex].Offset(ANewPos.X - FStartPos.X, ANewPos.Y - FStartPos.Y);
  FStartPos := ANewPos;

  Invalidate;
end;

end.
