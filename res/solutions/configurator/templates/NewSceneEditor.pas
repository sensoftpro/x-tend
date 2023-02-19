unit NewSceneEditor;

interface

uses
  Types, Classes, UITypes, vclBlobEditors, uScene, uDrawStyles, uView, uUIBuilder, uEntity, uTensor;

type
  TNewSceneEditor = class;

  TNewSceneObject = class(TSceneObject)
  private
    FDomain: TObject;
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
    procedure HandleMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      ClientPos: TPointF; var AHandled: Boolean); override;
  public
    constructor Create(const AScene: TScene; const AParent: TSceneObject; const AFieldArea: TNewSceneEditor);
    destructor Destroy; override;

    procedure UpdateBinding(const ABinding: TObject);
    procedure ExecuteUIAction(const AArea: TUIArea; const AView: TView);
  end;

  TNewSceneEditor = class(TFieldSceneArea)
  protected
    FNewSceneObject: TNewSceneObject;
    procedure RefillArea; override;
    procedure DoCreateControl(const ALayout: TObject); override;
    procedure DoExecuteUIAction(const AView: TView); override;
  end;

implementation

uses
  SysUtils, Math, UIConsts, uDomain, uComplexObject, uDefinition, uInteractor, uPresenter, uSimpleField, uObjectField, uConsts;

{ TNewSceneObject }

constructor TNewSceneObject.Create(const AScene: TScene; const AParent: TSceneObject;
  const AFieldArea: TNewSceneEditor);
var
  vRect: TRectF;
begin
  if Assigned(AParent) then
    vRect := AParent.ClientRect
  else
    vRect := AScene.ClientRect;

  UpdateBinding(AFieldArea);

  inherited Create(AScene, AParent, vRect);

  FDomain := AFieldArea.View.Domain;

  Anchors := [soaLeft, soaTop, soaRight, soaBottom];

  // Запомнить масштабы, положение мыши и т.п.
  // Обновить данные Zoom и Selection

  // Заполнение стилей
  if CreateStyle('NewSceneObject') then
  begin
    FStyle.AddFillParams('background', TAlphaColorRec.Silver, AppendColor(TAlphaColorRec.Silver, $001E1E1E), gkVertical); // background
    FStyle.AddStrokeParams('grid', TAlphaColorRec.Gray, 1, psDot); // grid
    FStyle.AddFontParams('x.title', 'Tahoma', 10, TAlphaColorRec.Black); // заголовок оси X
    FStyle.AddFontParams('y.title', 'Tahoma', 10, TAlphaColorRec.Black, 0, 270); // заголовок оси Y
    FStyle.AddFillParams('tick.selected', MakeColor(TAlphaColorRec.Black, 0.5)); // область вывода текущего значения X
    FStyle.AddFillParams('scroll.fill', MakeColor(TAlphaColorRec.Black, 40 / 256));
    FStyle.AddImageParams('peak', 'panel_peak.png', False).CutRect := Rect(7, 27, 8, 28);

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

procedure TNewSceneObject.UpdateBinding(const ABinding: TObject);
begin
  // Нужно, чтобы разыменовать сложные нативные объекты и загрузить их из файловой системы
  // Пример см. в TSpectrumChart (ASWPrime)
end;

{ TNewSceneEditor }

procedure TNewSceneEditor.DoCreateControl(const ALayout: TObject);
begin
  inherited DoCreateControl(ALayout);
  FId := 'NewSceneEditor';
  FNewSceneObject := TNewSceneObject.Create(FScene, nil, Self);
end;

procedure TNewSceneEditor.DoExecuteUIAction(const AView: TView);
begin
  FNewSceneObject.ExecuteUIAction(Self, AView);
end;

procedure TNewSceneEditor.RefillArea;
begin
  FNewSceneObject.UpdateBinding(Self);
  inherited RefillArea;
end;

end.

