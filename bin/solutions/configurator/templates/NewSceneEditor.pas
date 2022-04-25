unit uNewSceneEditor;

interface

uses
  Types, Classes, UITypes, vclBlobEditors, uScene, uDrawStyles, uView, uUIBuilder, uEntity, uTensor;

type
  TNewSceneEditor = class;

  TNewSceneObject = class(TSceneObject)
  private
    FDomain: TObject;
    FUIArea: TUIArea;
    FRootEntity: TEntity;
    FMousePos: TPointF;
    procedure Draw(const APainter: TPainter; const ARect: TRectF);
  protected
    procedure Deactivate(const AHovering: TSceneObject); override;
    procedure DoRenderStatic(const APainter: TPainter; const ARect: TRectF;
      const AMode: TScenePaintMode = spmNormal); override;
    procedure DoRenderDynamic(const APainter: TPainter; const ARect: TRectF); override;
    procedure DoRectChanged(const AOldRect, ANewRect: TRectF); override;
    procedure HandleMouseDown(Button: TMouseButton; Shift: TShiftState;
      ClientPos: TPointF; var AHandled: Boolean); override;
    procedure HandleMouseUp(Button: TMouseButton; Shift: TShiftState;
      ClientPos: TPointF; var AHandled: Boolean); override;
    procedure HandleDblClick(ClientPos: TPointF; var AHandled: Boolean); override;
    procedure HandleMouseMove(Shift: TShiftState; ClientPos: TPointF;
      var AHandled: Boolean); override;
    procedure HandleKeyDown(var Key: Word; Shift: TShiftState;
      var AHandled: Boolean); override;
    procedure HandleKeyUp(var Key: Word; Shift: TShiftState;
      var AHandled: Boolean); override;
    procedure HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      ClientPos: TPointF; var AHandled: Boolean); override;
  public
    constructor Create(const AScene: TScene; const AParent: TSceneObject; const AUIArea: TNewSceneEditor);
    destructor Destroy; override;

    function UpdateBinding(const ABinding: TObject): Boolean;
    procedure ExecuteUIAction(const AArea: TUIArea; const AView: TView);
  end;

  TNewSceneEditor = class(TFieldSceneArea)
  protected
    FNewSceneObject: TNewSceneObject;
    procedure RefillArea(const AKind: Word); override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure DoCreateControl(const AParent: TUIArea; const ANewScene: TObject); override;
    procedure DoExecuteUIAction(const AView: TView); override;
  end;

implementation

uses
  SysUtils, Math, UIConsts, uDomain, uComplexObject, uDefinition, uInteractor, uPresenter, uSimpleField, uObjectField, uConsts;

{ TNewSceneObject }

constructor TNewSceneObject.Create(const AScene: TScene; const AParent: TSceneObject;
  const AUIArea: TNewSceneEditor);
var
  vRect: TRectF;
begin
  if Assigned(AParent) then
    vRect := AParent.ClientRect
  else
    vRect := AScene.ClientRect;

  UpdateBinding(AUIArea);

  inherited Create(AScene, AParent, vRect);

  FUIArea := AUIArea;
  FDomain := AUIArea.View.Domain;
  FMousePos := PointF(-1, -1);

  Anchors := [soaLeft, soaTop, soaRight, soaBottom];

  // Запомнить масштабы, положение мыши и т.п.
  // Обновить данные Zoom и Selection

  // Заполнение стилей
  if CreateStyle('NewScene') then
  begin
    FStyle.AddFillParams('background', TAlphaColorRec.Silver, AppendColor(TAlphaColorRec.Silver, $001E1E1E), gkVertical); // background
    FStyle.AddStrokeParams('grid', TAlphaColorRec.Gray, 1, psDot); // grid
    FStyle.AddFontParams('x.title', 'Tahoma', 10, TAlphaColorRec.Black); // заголовок оси X
    FStyle.AddFontParams('y.title', 'Tahoma', 10, TAlphaColorRec.Black, 0, 270); // заголовок оси Y
    FStyle.AddFillParams('tick.selected', MakeColor(TAlphaColorRec.Black, 0.5)); // область вывода текущего значения X
    FStyle.AddFillParams('scroll.fill', MakeColor(TAlphaColorRec.Black, 40 / 256));
    FStyle.AddImageParams('panel', 'panel.png', False).CutRect := Rect(7, 7, 8, 8);

    FStyle.CreateDrawObjects(AScene.Painter);
  end;
end;

procedure TNewSceneObject.Deactivate(const AHovering: TSceneObject);
begin
  // Сбросить запомненные координаты мыши, обновить состояние/отрисовку
end;

destructor TNewSceneObject.Destroy;
begin
  FDomain := nil;
  inherited Destroy;
end;

procedure TNewSceneObject.DoRectChanged(const AOldRect, ANewRect: TRectF);
begin
  inherited DoRectChanged(AOldRect, ANewRect);
  // Обновить запомненные координаты мыши
end;

procedure TNewSceneObject.DoRenderDynamic(const APainter: TPainter; const ARect: TRectF);
begin
  // Отрисовываем дочерние объекты
  inherited DoRenderDynamic(APainter, ARect);

  // Отрисовка текущей позиции отрисовки (линия курсора, выделение области и т.п.)
end;

procedure TNewSceneObject.DoRenderStatic(const APainter: TPainter; const ARect: TRectF; const AMode: TScenePaintMode);
begin
  // Полная отрисовка статической (редко изменяемой) области
  Draw(APainter, ARect);

  // Отрисовываем дочерние объекты
  inherited DoRenderStatic(APainter, ARect, AMode);
end;

procedure TNewSceneObject.Draw(const APainter: TPainter; const ARect: TRectF);
begin
  APainter.DrawRect(FStyle, 'background', '', ARect);
end;

procedure TNewSceneObject.ExecuteUIAction(const AArea: TUIArea; const AView: TView);
begin
  // Выполнить действия из контекстного меню или привязанные к конфигурации через #
end;

procedure TNewSceneObject.HandleDblClick(ClientPos: TPointF; var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
end;

procedure TNewSceneObject.HandleKeyDown(var Key: Word; Shift: TShiftState; var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
end;

procedure TNewSceneObject.HandleKeyUp(var Key: Word; Shift: TShiftState; var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
end;

procedure TNewSceneObject.HandleMouseDown(Button: TMouseButton; Shift: TShiftState; ClientPos: TPointF;
  var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
end;

procedure TNewSceneObject.HandleMouseMove(Shift: TShiftState; ClientPos: TPointF; var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
  FMousePos := ClientPos;
  //Invalidate(ssUsed);
end;

procedure TNewSceneObject.HandleMouseUp(Button: TMouseButton; Shift: TShiftState; ClientPos: TPointF;
  var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
end;

procedure TNewSceneObject.HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer; ClientPos: TPointF;
  var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
end;

function TNewSceneObject.UpdateBinding(const ABinding: TObject): Boolean;
var
  vView: TView;
  vField: TEntityField;
  vPrevRootEntity: TEntity;
begin
  if FUIArea = ABinding then
    Exit(False);

  FUIArea := TUIArea(ABinding);
  if not Assigned(FUIArea) then
    Exit(True);

  FRootEntity := nil;

  vView := FUIArea.View;
  if not Assigned(vView) then
    Exit(True);

  vPrevRootEntity := FRootEntity;
  if vView.DefinitionKind = dkEntity then
    FRootEntity := TEntity(vView.DomainObject)
  else begin
    vField := vView.ExtractEntityField;
    if Assigned(vField) then
      FRootEntity := TEntity(vField.Entity);
  end;

  Result := vPrevRootEntity <> FRootEntity;
  if not Assigned(FRootEntity) then
    Exit;
end;

{ TNewSceneEditor }

procedure TNewSceneEditor.DoCreateControl(const AParent: TUIArea; const ANewScene: TObject);
begin
  inherited DoCreateControl(AParent, ANewScene);
  FId := 'NewSceneEditor';
  FNewSceneObject := TNewSceneObject.Create(FScene, nil, Self);
end;

procedure TNewSceneEditor.DoExecuteUIAction(const AView: TView);
begin
  FNewSceneObject.ExecuteUIAction(Self, AView);
end;

procedure TNewSceneEditor.RefillArea(const AKind: Word);
begin
  if FNewSceneObject.UpdateBinding(Self) then
    FNewSceneObject.Invalidate(ssDirty)
  else
    FNewSceneObject.Invalidate(ssUsed);
  FNewSceneObject.Repaint;
end;

procedure TNewSceneEditor.UpdateArea(const AKind: Word; const AParameter: TEntity);
begin
  if AKind = dckViewStateChanged then
  begin
    FNewSceneObject.Invalidate(ssDirty);
    FNewSceneObject.Repaint;
  end
  else
    RefillArea(AKind);
end;

initialization

TPresenter.RegisterUIClass('Windows.DevExpress', uiEntityEdit, 'new_scene_editor', TNewSceneEditor);

end.

