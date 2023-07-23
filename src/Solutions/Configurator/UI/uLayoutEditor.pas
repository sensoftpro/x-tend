unit uLayoutEditor;

interface

uses
  Types, Classes, UITypes, vclBlobEditors, uScene, uDrawStyles, uView, uUIBuilder, uEntity, uTensor;

type
  TLayoutEditor = class;

  TLayoutObject = class(TSceneObject)
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
    constructor Create(const AScene: TScene; const AParent: TSceneObject; const AUIArea: TLayoutEditor);
    destructor Destroy; override;

    function UpdateBinding(const ABinding: TObject): Boolean;
    procedure ExecuteUIAction(const AArea: TUIArea; const AView: TView);
  end;

  TLayoutEditor = class(TFieldSceneArea)
  protected
    FLayoutObject: TLayoutObject;
    procedure RefillArea(const AKind: Word); override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure DoExecuteUIAction(const AView: TView); override;
  end;

implementation

uses
  SysUtils, Math, UIConsts, uDomain, uComplexObject, uDefinition, uInteractor, uPresenter, uSimpleField, uObjectField, uConsts;

{ TLayoutObject }

constructor TLayoutObject.Create(const AScene: TScene; const AParent: TSceneObject;
  const AUIArea: TLayoutEditor);
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
  if CreateStyle('Layout') then
  begin
    FStyle.AddFillParams('background', $FF052D49); // background
    FStyle.AddStrokeParams('grid', TAlphaColorRec.Gray, 1, psDot); // grid
    FStyle.AddFontParams('x.title', 'Tahoma', 10, TAlphaColorRec.Black); // заголовок оси X
    FStyle.AddFontParams('y.title', 'Tahoma', 10, TAlphaColorRec.Black, 0, 270); // заголовок оси Y
    FStyle.AddFillParams('tick.selected', MakeColor(TAlphaColorRec.Black, 0.5)); // область вывода текущего значения X
    FStyle.AddFillParams('scroll.fill', MakeColor(TAlphaColorRec.Black, 40 / 256));
    FStyle.AddImageParams('panel', 'panel.png', False).CutRect := Rect(7, 7, 8, 8);

    FStyle.CreateDrawObjects(AScene.Painter);
  end;
end;

procedure TLayoutObject.Deactivate(const AHovering: TSceneObject);
begin
  // Сбросить запомненные координаты мыши, обновить состояние/отрисовку
end;

destructor TLayoutObject.Destroy;
begin
  FDomain := nil;
  inherited Destroy;
end;

procedure TLayoutObject.DoRectChanged(const AOldRect, ANewRect: TRectF);
begin
  inherited DoRectChanged(AOldRect, ANewRect);
  // Обновить запомненные координаты мыши
end;

procedure TLayoutObject.DoRenderDynamic(const APainter: TPainter; const ARect: TRectF);
begin
  // Отрисовываем дочерние объекты
  inherited DoRenderDynamic(APainter, ARect);

  // Отрисовка текущей позиции отрисовки (линия курсора, выделение области и т.п.)
end;

procedure TLayoutObject.DoRenderStatic(const APainter: TPainter; const ARect: TRectF; const AMode: TScenePaintMode);
begin
  // Полная отрисовка статической (редко изменяемой) области
  Draw(APainter, ARect);

  // Отрисовываем дочерние объекты
  inherited DoRenderStatic(APainter, ARect, AMode);
end;

procedure TLayoutObject.Draw(const APainter: TPainter; const ARect: TRectF);
begin
  APainter.DrawRect(FStyle, 'background', '', ARect);
end;

procedure TLayoutObject.ExecuteUIAction(const AArea: TUIArea; const AView: TView);
begin
  // Выполнить действия из контекстного меню или привязанные к конфигурации через #
end;

procedure TLayoutObject.HandleDblClick(ClientPos: TPointF; var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
end;

procedure TLayoutObject.HandleKeyDown(var Key: Word; Shift: TShiftState; var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
end;

procedure TLayoutObject.HandleKeyUp(var Key: Word; Shift: TShiftState; var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
end;

procedure TLayoutObject.HandleMouseDown(Button: TMouseButton; Shift: TShiftState; ClientPos: TPointF;
  var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
end;

procedure TLayoutObject.HandleMouseMove(Shift: TShiftState; ClientPos: TPointF; var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
  FMousePos := ClientPos;
  //Invalidate(ssUsed);
end;

procedure TLayoutObject.HandleMouseUp(Button: TMouseButton; Shift: TShiftState; ClientPos: TPointF;
  var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
end;

procedure TLayoutObject.HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer; ClientPos: TPointF;
  var AHandled: Boolean);
begin
  // Обработка события, AHandled установлен в True
  // При изменениях модели, влияющих на отрисовку, нужно вызвать Invalidate(ssUsed или ssDirty)
end;

function TLayoutObject.UpdateBinding(const ABinding: TObject): Boolean;
var
  vView: TView;
  vField: TEntityField;
  vPrevRootEntity: TEntity;
begin
  // Нужно, чтобы разыменовать сложные нативные объекты и загрузить их из файловой системы
  // Пример см. в TSpectrumChart (ASWPrime)
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

{ TLayoutEditor }

procedure TLayoutEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
begin
  inherited DoCreateControl(AParent, ALayout);
  FId := 'LayoutEditor';
  FLayoutObject := TLayoutObject.Create(FScene, nil, Self);
end;

procedure TLayoutEditor.DoExecuteUIAction(const AView: TView);
begin
  FLayoutObject.ExecuteUIAction(Self, AView);
end;

procedure TLayoutEditor.RefillArea(const AKind: Word);
begin
  if FLayoutObject.UpdateBinding(Self) then
    FLayoutObject.Invalidate(ssDirty)
  else
    FLayoutObject.Invalidate(ssUsed);
  FLayoutObject.Repaint;
end;

procedure TLayoutEditor.UpdateArea(const AKind: Word; const AParameter: TEntity);
begin
  if AKind = dckViewStateChanged then
  begin
    FLayoutObject.Invalidate(ssDirty);
    FLayoutObject.Repaint;
  end
  else
    RefillArea(AKind);
end;

initialization

TPresenter.RegisterUIClass('Windows.DevExpress', uiEntityEdit, 'layout_editor', TLayoutEditor);

end.

