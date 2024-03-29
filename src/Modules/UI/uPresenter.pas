﻿{---------------------------------------------------------------------------------
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

unit uPresenter;

interface

uses
  Classes, Generics.Collections, SysUtils, UITypes, Types,
  uModule, uSettings, uDefinition, uInteractor, uSession, uView, uConsts, uUIBuilder, uLayout, uIcon;

const
  cPCNavigatorFlag = 1;
  cServiceAreaHeight = 44;

type
  { НАВИГАЦИЯ И ИДЕНТИФИКАЦИЯ ПОЛЕЙ }
  {
    1. НАВИГАЦИЯ
    1.1. Считаем, что мы находимся в контексте домена. Если это не так, слева от указанных адресов нужно будет добавить
         путь к домену, например: /domains/ef89-767e98-656e-daaa или /domains/1 в зависимости от места запуска
    1.2. Доступ к домену:
         /?layout=<LayoutName> - заготовка главной формы
    1.3. Доступ к коллекции:
         /<Collection>?<params>, где
         <params> - параметры, описывающие, как открыть коллекцию
           - mode=(view|edit) - режим открытия коллекции;
           - layout=<LayoutName> - заготовка области, на которой откроется коллекция
         Если лэйаута нет, можно указать, каким образом автокомпоновщик сможет настроить элементы.
         Список этих параметров больше относится к именованию и приведен ниже в соответствующем разделе.
    1.4. Доступ к сущности:
         /<Collection>/<EntityPath>?<params>
         <EntityPath> - путь, по которому можно получить конкретную сущность. Он может принимать следующие значения:
           - Current - текущая известная сущность для указанной коллекции. Это свойство определено для нескольких
               коллекций. В коллекции SysUsers это текущий пользователь, в коллекции SysConstants - текущие настройки
               окружения, в котором исполняется домен.
           - <ID> - числовой идентификатор сущности в коллекции.
           - другие варианты являются зависимыми от контекста и не позволяют однозначно описать путь.
         <params> - параметры, описывающие, как открыть сущность:
           - mode=(view|edit) - режим открытия сущности;
           - layout=<LayoutName> - заготовка области, на которой откроется сущность;
           - focus=<FieldName> - имя поля, на котором нужно установить фокус.
         При отсутствии лэйаута, можно указать дополнительные параметры (см. ниже).
    1.5. Вызов действий:
         /act<Action>?<params> - общесистемное действие;
         /<Collection>/act<Action>?<params> - действие над коллекцией, при этом коллекция будет контекстом;
         /<Collection>/<EntityPath>/act<Action>?<params> - действие над сущностью, передаваемой в виде контекста.
         ?? Подумать, ложатся ли сюда действия ADD, EDIT, VIEW, DELETE, LINK, UNLINK ??
         При наличии у действия параметров они могут быть переданы списком <params>. Если параметры у действия есть,
           но они не переданы, система должна организовать создание и показ диалога ввода параметров.
    1.6. Вызов отчетов:
         Логика работы с отчетами такая же, как и с действиями, за исключением префикса rep- вместо act-.

    2. ИДЕНТИФИКАЦИЯ ПОЛЕЙ В ЛЭЙАУТАХ
       По большому счету идентификация не сильно отличается от навигации, поэтому акцент будет сделан на отличиях
    2.1. Идентификация коллекций:
         <Collection>?<params>, где
         <params> - параметры, описывающие, как отобразить коллекцию в листовое поле:
           - view=<ViewName> - тип контрола, который нужно создать (для каждого типа поля есть свой набор типов);
           - q="<Query>" - фильтр, который нужно применить к коллекции при ее отображении;
           - sort="<FieldList>" - список полей, разделенных ";". По умолчанию сортировка по возрастанию.
               Для сортировки поля по убыванию после него нужно добавить DESC (пример: sort="a;b DESC;c")
           - show_fields=<FieldList> - список выводимых в список полей;
           - ignore_fields=<FieldList> - список полей, которые не нужно показывать;
           - caption=<Name> - подпись, которая будет отображаться над списком.
    2.2. Идентификация сущностей:
         <Collection>/<EntityPath>, где
         <EntityPath> в связи с появлением контекста может дополнительно принимать следующие значения:
           - Current - описано выше;
           - <ID> - описано выше;
           - First, Last - первая и последняя сущность коллекции, соответственно. Порядок определяется по ID,
               или же может быть установлен явно в параметрах по ключу sort;
           - Selected - указывает, что сущность выбрана пользователем. Это значение будет динамически обновляться при
               изменениях, инициированных пользователем в UI (выбор в списке, отмена выбора)/
           Выборка для значений First и Last производится по параметрам списка или коллекции.
    2.3. Идентификация действий:
         act<Action>?<params> - общесистемное действие;
         <Collection>/act<Action>?<params> - действие над коллекцией, при этом коллекция будет контекстом;
         <Collection>/<EntityPath>/act<Action>?<params> - действие над сущностью, передаваемой в виде контекста.
         При этом в параметрах так же, как и для коллекций, можно задать стиль отображения:
           - view=<ViewName> - тип контрола, который нужно создать;
           - caption=<Caption> - выводимая на контроле надпись;
           - image=<ImageID> - уникальный идентификатор картинки, которую нужно вывести (при необходимости).
    2.4. Идентификация отчетов:
         Логика работы с отчетами такая же, как и с действиями, за исключением префикса rep- вместо act-.
    2.5. Идентификация полей:
         <PathToField>/<Field>?<params>, где
         <PathToField> - путь до сущности, которую нужно разыменовать. Это может быть:
           - сама сущность (п.2.2),
           - поле встроенного в форму параметра действия или отчета (п.2.3-2.4),
           - разыменованное объектное или листовое поле.
         Важно, что тип поля задается последним элементом пути и все параметры отображения относятся к нему.
           При этом количество промежуточных разыменований может быть произвольным. Каждое разыменование отделено от
           другого прямым слешем и оно однозначно указывает на следующее поле.
           Объектное поле разыменовывается путем получения его сушности.
           Листовое поле должно разыменовывается для конкретной сущности, которая либо указана явно, либо использует
           выбор, сделанный пользователем. Таким образом, изменение сущности, входящей в иерархию отображения,
           или же изменение пользовательского выбора должно инициировать перезаполнение дерева подчиненных элементов.
         <params> - параметры, описывающие, как отобразить поле. Такие же, как для коллекции (п.2.1)
    2.6. Иногда бывает необходимо сделать два или более элементов отображения для одного поля. Например, для списка
         мы хотим сравнивать попарно его элементы. Для идентификации такого поведения после имени поля необходимо
         дописать ~ и порядковый номер, который будет использован при дальнейших разыменованиях.
         Пример: Currencies~0 или Currencies~211. Номер нужен только для визуальной части.

    3. АЛГОРИТМ РАБОТЫ
    3.1. Все перемещения между лэйаутами делаются с помощью метода Navigate(). Это позволяет формировать
         "хлебные крошки" и использовать в системе относительные пути. В качестве параметра этому методу передается
         путь к сущности, которая будет открыта, и лэйаут, в котором она откроется.
    3.2. Лэйаут всегда имеет ссылку на корневую сущность (в качестве нее может быть и коллекция)
    3.3. Лэйаут может содержать в себе служебные области, используемые для подчиненных лэйаутов. В эти области
         будут подгружаться зависимые лэйауты, связанные с выбором пользователя.
    3.4.
    3.5.
  }


  { доступ к платформе, предоставление UI
    - тип соединения (HTTP, Socket, Pipes, Messenger, Direct)
    - настройки соединения
    - обработчики пользовательских запросов
    - доменные соединения
      - имя бота-обработчика
      - путь от корня до этого домена
  }

  TControlClassInfo = class
  private
    FName: string;
    FType: TUIItemType;
    FControlClass: TNativeControlClass;
  public
    constructor Create(const AControlType: TUIItemType; const AStyleName: string;
      const AControlClass: TNativeControlClass);
  end;

  TImages = class
  private
    FSize: Integer;
    FItems: TList<TStream>;
    FIndices: TDictionary<string, Integer>;
    FPlaceholder: TStream;
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): TStream;
  public
    constructor Create(const ASize: Integer);
    destructor Destroy; override;

    procedure Add(const AName: string; const AIndex: Integer; const AStream: TStream);
    procedure FillWithPlaceholder(const APlaceholder: TStream);

    property Items[const AIndex: Integer]: TStream read GetItem; default;
    property Count: Integer read GetCount;
    property Indices: TDictionary<string, Integer> read FIndices;
    property Placeholder: TStream read FPlaceholder;
  end;

  TPresenter = class(TBaseModule)
  private
    class var RegisteredCanvasClasses: TObjectDictionary<string, TDictionary<string, TClass>>;
    class var RegisteredControlClasses: TObjectDictionary<string, TObjectDictionary<string, TControlClassInfo>>;
    class var RegisteredSceneObjectClasses: TDictionary<string, TDomainSceneObjectClass>;
  public
    class procedure RegisterCanvasClass(const APresenterName: string; const ACanvasTypeName: string; const ACanvasClass: TClass);
    class procedure RegisterControlClass(const APresenterName: string; const AControlType: TUIItemType;
      const AStyleName: string; const AControlClass: TNativeControlClass);
    class procedure RegisterSceneObjectClass(const AName: string; const ASceneObjectClass: TDomainSceneObjectClass);

    class function GetSceneObjectClass(const AName: string): TDomainSceneObjectClass;
  private
    FNativeControlClass: TNativeControlClass;

    function CreateItem(const AOwner, AArea: TUIArea; const ANavItem: TNavigationItem;
      const AView: TView): TNativeControl;
    function GetControlClass(const AControlType: TUIItemType; const AStyleName: string): TNativeControlClass;
    function CreateAreaContent(const AArea: TUIArea; const AView: TView;
      const ALayout: TLayout; const AParams: string = ''): TNativeControl;
    function CreateAreaContentItem(const AOwner, AArea: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TNativeControl;
    procedure StoreUILayout(const AInteractor: TInteractor; const AForm: TNativeControl);
    procedure RestoreUILayout(const AInteractor: TInteractor; const AForm: TNativeControl);
    procedure ArrangeCascade(const AClientRect: TRect; const AChildForms: TList<TNativeControl>);
    procedure ArrangeHorz(const AClientRect: TRect; const AChildForms: TList<TNativeControl>);
    procedure ArrangeVert(const AClientRect: TRect; const AChildForms: TList<TNativeControl>);
    procedure ArrangeMozaic(const AClientRect: TRect; const AChildForms: TList<TNativeControl>);
  protected
    procedure OnTabClose(Sender: TObject; var AllowClose: Boolean);
    procedure OnPCCanClose(Sender: TObject; var ACanClose: Boolean);
    procedure OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
    procedure DoChildFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoFloatFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoMainFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoOnFormShow(Sender: TObject);
    procedure DoChildFormKeyDown(Sender: TObject; const AShift: TShiftState; const AKey: Word; var AHandled: Boolean);
    procedure DoProcessShortCut(const AShift: TShiftState; const AKey: Word; var AHandled: Boolean);

    function ModalResultToDialogResult(const AModalResult: TModalResult): TDialogResult;
  protected
    FName: string;
    FStartParameter: string;
    FCursorType: TCursorType;
    FInteractors: TList<TInteractor>;
    FCommonIcons: TIcons;

    procedure DoRun(const AParameter: string); virtual;
    procedure DoUnfreeze; virtual;
    procedure DoStop; virtual;

    function AreaFromSender(const ASender: TObject): TUIArea; virtual;
    procedure DoLogin(const ADomain: TObject); virtual;
    procedure DoLogout(const AInteractor: TInteractor); virtual;

    function GetNativeControlClass: TNativeControlClass; virtual; abstract;
    procedure CreateMainForm(const ASession: TUserSession); virtual;
    procedure DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType); virtual; abstract;
    procedure DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet; const AOnClose: TCloseProc = nil); virtual; abstract;
    procedure DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False); virtual;
    procedure DoShowOpenDialog(const AFileName: string; const ATitle, AFilter,
      ADefaultExt, ADefaultDir: string; const AOnClose: TCloseTextProc = nil); virtual;
    procedure DoShowSaveDialog(const AFileName: string; const ATitle, AFilter,
      ADefaultExt: string; const AOnClose: TCloseTextProc = nil); virtual;
    procedure DoSetCursor(const ACursorType: TCursorType); virtual;
    procedure DoArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement); virtual;
    procedure DoCloseAllPages(const AInteractor: TInteractor); virtual;
    function CreateControl(const AParent: TUIArea; const AView: TView; const ALayout: TLayout;
      const AParams: string = ''): TObject; virtual;

    procedure CopyPopupMenuItems(const AParent: TUIArea; const AView: TView;
      const ASrcItem: TNavigationItem; const ADestArea: TUIArea);

    function GetImagePlaceholder(const ASize: Integer): TStream; virtual; abstract;
    function DoCreateImages(const ADomain: TObject; const AImages: TImages;
      const ASize: Integer): TObject; virtual; abstract;

    procedure DoEnumerateControls(const ALayout: TLayout; const AControl: TObject); virtual;
    function CanLoadFromDFM: Boolean; virtual;

    function ActiveInteractor: TInteractor;
  public
    function CanCloseChildForm(const AForm: TObject; const AModalResult: Integer): Boolean;
    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); virtual; abstract;
  public
    constructor Create(const AName: string; const ASettings: TSettings); virtual;
    destructor Destroy; override;
    procedure Run(const AParameter: string = '');
    procedure Unfreeze;
    procedure Stop;

    procedure Login(const ADomain: TObject);
    procedure Logout(const AInteractor: TInteractor);

    procedure EnumerateControls(const ALayout: TLayout; const AControl: TObject);

    procedure ShowUIArea(const AArea: TUIArea; const AAreaName: string; const ACaption: string); virtual;
    procedure ShowPage(const AInteractor: TInteractor; const APageType: string;
      const AParams: TObject = nil; const AOnClose: TCloseProc = nil); virtual;
    procedure ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement);
    procedure CloseAllPages(const AInteractor: TInteractor);

    function CreateNativeControl(const AArea: TUIArea; const AView: TView; const ALayout: TLayout;
      const AControlType: TUIItemType; const AStyleName, AParams: string): TNativeControl;
    function CreateArea(const AParent: TUIArea; const AView: TView; const ALayout: TLayout;
      const AParams: string = ''; const AOnClose: TCloseProc = nil): TUIArea;

    function CreateTempControl: TObject; virtual; abstract;

    function SetCursor(const ACursorType: TCursorType): TCursorType;
    function GetWidthByType(const AWidth: Integer; const AFieldDef: TFieldDef): Integer;
    function GetCanvasClass(const ACanvasTypeName: string): TClass;
    function GetRealControl(const AArea: TUIArea): TObject;

    procedure ShowMessage(const ACaption, AText: string; const AMessageType: TMessageType = msNone);
    procedure ShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet;
      const AOnClose: TCloseProc = nil);
    procedure ShowYesNoDialog(const ACaption, AText: string;
      const AWithCancel: Boolean = False; const AOnClose: TCloseProc = nil);
    procedure ShowOkCancelDialog(const ACaption, AText: string; const AOnClose: TCloseProc = nil);
    procedure OpenFile(const AFileName: string; const ADefaultApp: string = ''; const Await: Boolean = False);
    procedure ShowOpenDialog(const AFileName: string; const ATitle, AFilter, ADefaultExt,
      ADefaultDir: string; const AOnClose: TCloseTextProc = nil);
    procedure ShowSaveDialog(const AFileName: string; const ATitle, AFilter, ADefaultExt: string;
      const AOnClose: TCloseTextProc = nil);

    function CreateImages(const ADomain: TObject; const ASize: Integer): TObject;
    property Name: string read FName;
    property LoadFromDFM: Boolean read CanLoadFromDFM;
  end;

  TPresenterClass = class of TPresenter;

implementation

uses
  Math, IOUtils, StrUtils, uUtils, uPlatform, uConfiguration, uDomain, uEntity, uEntityList, uChangeManager;

type
  TLoginedProc = procedure(const AInteractor: TInteractor) of object;
  TBeforeUIClosingFunc = function(const AInteractor: TInteractor; const AOnClose: TCloseProc): TCloseAction of object;

{ TPresenter }

function TPresenter.ActiveInteractor: TInteractor;
begin
  if FInteractors.Count > 0 then
    Result := FInteractors[0]
  else
    Result := nil;
end;

function TPresenter.AreaFromSender(const ASender: TObject): TUIArea;
begin
  if ASender is TComponent then
    Result := TUIArea(TComponent(ASender).Tag)
  else
    Result := nil;
end;

procedure TPresenter.ArrangeCascade(const AClientRect: TRect;
  const AChildForms: TList<TNativeControl>);
var
  vHeaderHeight: Integer;
  vFormBoundRect: TRect;
  vFormClientRect: TRect;
  i, vIndex: Integer;
  vMaxCount: Integer;
  vWidth, vHeight: Integer;
begin
  vFormBoundRect := AChildForms[0].Bounds;
  vFormClientRect := AChildForms[0].ClientRect;
  vHeaderHeight := (vFormBoundRect.Height - vFormClientRect.Height) -
    (vFormBoundRect.Width - vFormClientRect.Width) div 2;

  vMaxCount := Max(1, Round(AClientRect.Height * 0.45 / vHeaderHeight));
  vWidth := Max(50, AClientRect.Width - (vMaxCount - 1) * vHeaderHeight);
  vHeight := Max(35, AClientRect.Height - (vMaxCount - 1) * vHeaderHeight);

  for i := 0 to AChildForms.Count - 1 do
  begin
    vIndex := i mod vMaxCount;
    AChildForms[i].Bounds := Rect(vHeaderHeight * vIndex, vHeaderHeight * vIndex,
      vHeaderHeight * vIndex + vWidth, vHeaderHeight * vIndex + vHeight);
  end;
end;

procedure TPresenter.ArrangeHorz(const AClientRect: TRect;
  const AChildForms: TList<TNativeControl>);
var
  i: Integer;
  vWidth: Integer;
  vStart: Integer;
begin
  vStart := 0;
  for i := 0 to AChildForms.Count - 1 do
  begin
    vWidth := (AClientRect.Width - vStart) div (AChildForms.Count - i);
    AChildForms[i].Bounds := Rect(vStart, AClientRect.Top, vStart + vWidth, AClientRect.Bottom);
    vStart := vStart + vWidth;
  end;
end;

procedure TPresenter.ArrangeMozaic(const AClientRect: TRect;
  const AChildForms: TList<TNativeControl>);
var
  i, j: Integer;
  vColCount: Integer;
  vMap: array of Integer;
  vRow: Integer;
  vTotal: Integer;
const
  cColumnCounts: array[1..30] of Integer =
    (1,2,2,2,2, 3,3,4,3,3, 3,4,4,4,5, 4,4,4,4,5, 5,5,5,6,5, 5,5,5,5,6);

  procedure PlaceForm(const ACol, ARow, AMainWidth, AMainHeight: Integer; const AForm: TNativeControl);
  var
    vRect: TRect;
  begin
    vRect.Width := AMainWidth div vColCount;
    vRect.Height := AMainHeight div vMap[ACol];
    vRect.Left := ACol * vRect.Width;
    vRect.Top := ARow * vRect.Height;
    AForm.Bounds := vRect;
  end;
begin
  // Формируем карту расположения окон
  if AChildForms.Count > 30 then
    vColCount := Floor(Sqrt(AChildForms.Count))
  else
    vColCount := cColumnCounts[AChildForms.Count];

  SetLength(vMap, vColCount);
  for i := 0 to vColCount - 1 do
    vMap[i] := AChildForms.Count div vColCount;
  for i := vColCount - AChildForms.Count mod vColCount to vColCount - 1 do
    vMap[i] := vMap[i] + 1;

  try
    for i := 0 to AChildForms.Count - 1 do
    begin
      vTotal := 0;
      for j := 0 to vColCount - 1 do
      begin
        vTotal := vTotal + vMap[j];
        if i < vTotal then
        begin
          vRow := (i - (vTotal - vMap[j])) mod vMap[j];
          PlaceForm(j, vRow, AClientRect.Width, AClientRect.Height, AChildForms[i]);
          Break;
        end;
      end;
    end;
  finally
    SetLength(vMap, 0);
  end;
end;

procedure TPresenter.ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement);
begin
  if AInteractor.UIBuilder.IsMDIStyle then
    DoArrangePages(AInteractor, AArrangeKind);
end;

procedure TPresenter.ArrangeVert(const AClientRect: TRect;
  const AChildForms: TList<TNativeControl>);
var
  i: Integer;
  vHeight: Integer;
  vStart: Integer;
begin
  vStart := 0;
  for i := 0 to AChildForms.Count - 1 do
  begin
    vHeight := (AClientRect.Height - vStart) div (AChildForms.Count - i);
    AChildForms[i].Bounds := Rect(AClientRect.Left, vStart, AClientRect.Right, vStart + vHeight);
    vStart := vStart + vHeight;
  end;
end;

function TPresenter.CanCloseChildForm(const AForm: TObject; const AModalResult: Integer): Boolean;
var
  vArea: TUIArea;

  function GetUnfilledRequiredFields(const AArea: TUIArea; var AFields: string): Boolean;
  var
    i: Integer;
    vFieldDef: TFieldDef;
    vChildArea: TUIArea;
  begin
    for i := 0 to AArea.Count - 1 do
    begin
      vChildArea := AArea.Areas[i];
      if (vChildArea.View.DefinitionKind in [dkListField, dkObjectField, dkSimpleField]) then
      begin
        vFieldDef := TFieldDef(vChildArea.View.Definition);
        if Assigned(vChildArea.View.ParentDomainObject)
          and not TEntity(vChildArea.View.ParentDomainObject).FieldByName(vFieldDef.Name).IsValid then
        begin
          if Length(AFields) > 0 then
            AFields := AFields + ', ';
          AFields := AFields + TDomain(AArea.Domain).TranslateFieldDef(vFieldDef);
        end;
      end
      else
        GetUnfilledRequiredFields(vChildArea, AFields);
    end;

    Result := Length(AFields) > 0;
  end;

  function DoValidation: Boolean;
  var
    vView: TView;
    vEmptyRequiredFields: string;
    vEntity: TEntity;
    vSimilar: TEntity;
    vInteractor: TInteractor;
  begin
    Result := True;
    vView := vArea.View;
    if not (vView.DefinitionKind in [dkObjectField, dkEntity, dkAction]) then
      Exit;

    vEntity := TEntity(vView.DomainObject);
    if not Assigned(vEntity) then
      Exit;

    vInteractor := TInteractor(vArea.Interactor);
    vEmptyRequiredFields := '';
    if GetUnfilledRequiredFields(vArea, vEmptyRequiredFields) then
    begin
      vInteractor.ShowMessage(vInteractor.Translate('msgRequiredFieldsAreEmpty', 'Не заполнены обязательные поля') +
        '. ' + #13#10 + vEmptyRequiredFields);
      Exit(False);
    end;

    vSimilar := vEntity.FindSimilar(vInteractor.Session);
    if Assigned(vSimilar) then
    begin
      vInteractor.ShowMessage(vInteractor.Translate('msgRecordExists', 'Такая запись уже существует') +
        ' [' + vSimilar['Name'] + ']');
      Exit(False);
    end;
  end;
begin
  vArea := AreaFromSender(AForm);

  if AModalResult = mrOk then
    Result := DoValidation
  else
    Result := True;
end;

function TPresenter.CanLoadFromDFM: Boolean;
begin
  Result := False;
end;

procedure TPresenter.CloseAllPages(const AInteractor: TInteractor);
var
  i: Integer;
  vChildArea: TUIArea;
begin
  if AInteractor.UIBuilder.IsMDIStyle then
  begin
    DoCloseAllPages(AInteractor);
    Exit;
  end;

  if not Assigned(AInteractor.PagedArea) then
    Exit;

  for i := AInteractor.PagedArea.Count - 1 downto 0 do
  begin
    vChildArea := AInteractor.PagedArea.Areas[i];
    AInteractor.PagedArea.RemoveArea(vChildArea);
  end;
end;

procedure TPresenter.CopyPopupMenuItems(const AParent: TUIArea; const AView: TView; const ASrcItem: TNavigationItem;
  const ADestArea: TUIArea);
var
  vCaption: string;
  vActions: TList<TActionDef>;
  vReports: TList<TRTFReport>;
  vDefinitions: TList<TDefinition>;
  i, j: Integer;
  vDefinition: TDefinition;
  vSrcItem: TNavigationItem;
  vChildItem: TLayout;
  vView: TView;
  vAction: TActionDef;
  vReport: TRTFReport;
  vChildArea: TUIArea;
  vDefArea: TUIArea;
begin
  for i := 0 to ASrcItem.Items.Count - 1 do
  begin
    vSrcItem := TNavigationItem(ASrcItem.Items[i]);
    vCaption := vSrcItem.ViewName;

    if SameText(vCaption, '#Placeholder') then
    begin
      if AView.DefinitionKind <> dkCollection then
        Continue;

      vDefinition := TDefinition(AView.Definition);
      vActions := TList<TActionDef>.Create;
      vDefinition.GetAllActions(vActions);
      for vAction in vActions do
      begin
        if vAction.HasFlag(ccHideInMenu) then
          Continue;

        if vAction.HasFlag(ccContextAction) then
        begin
          vView := AParent.RootView.BuildView(AView.Name + '/Selected/' + vAction.Name);
        end
        else
          vView := AParent.RootView.BuildView(AView.Name + '/' + vAction.Name);

        if Assigned(vView) then
        begin
          if vView.DefinitionKind = dkAction then
          begin
            vChildItem := AParent.UIBuilder.Layouts.CreateSimpleLayout(lkAction);
            vChildItem.StyleName := 'action';
            vChildItem.Caption := AParent.GetTranslation(vAction);
            vChildItem.Hint := vChildItem.Caption;
            vChildItem.ImageID := vAction._ImageID;

            vChildArea := CreateArea(AParent, vView, vChildItem);
            vChildArea.UpdateArea(dckViewStateChanged);
            AParent.AddArea(vChildArea);
          end
          else
            vView.CleanView;
        end;
      end;

      if vActions.Count > 0 then
      begin
        vChildItem := AParent.UIBuilder.Layouts.CreateSimpleLayout(lkAction);
        vChildItem.StyleName := 'line';
        vChildItem.Caption := '-';
        vChildArea := CreateArea(AParent, AParent.RootView, vChildItem);
        AParent.AddArea(vChildArea);
      end;

      FreeAndNil(vActions);

      vReports := TList<TRTFReport>.Create;
      vDefinition.GetAllReports(vReports);
      for vReport in vReports do
      begin
        vView := AParent.RootView.BuildView(AView.Name + '/Selected/' + vReport.Name);

        if Assigned(vView) then
        begin
          if vView.DefinitionKind = dkAction then
          begin
            vChildItem := AParent.UIBuilder.Layouts.CreateSimpleLayout(lkAction);
            vChildItem.StyleName := 'action';
            vChildItem.Caption := AParent.GetTranslation(vReport);
            vChildItem.Hint := vChildItem.Caption;
            vChildItem.ImageID := 'printer';

            vChildArea := CreateArea(AParent, vView, vChildItem);
            vChildArea.UpdateArea(dckViewStateChanged);
            AParent.AddArea(vChildArea);
          end
          else
            vView.CleanView;
        end;
      end;

      if vReports.Count > 0 then
      begin
        vChildItem := AParent.UIBuilder.Layouts.CreateSimpleLayout(lkAction);
        vChildItem.StyleName := 'line';
        vChildItem.Caption := '-';
        vChildArea := CreateArea(AParent, AParent.RootView, vChildItem);
        AParent.AddArea(vChildArea);
      end;

      FreeAndNil(vReports);

      Continue;
    end
    else if Pos('@', vCaption) > 0 then
    begin
      vSrcItem.StyleName := 'group';
      vChildArea := CreateArea(AParent, AParent.View, vSrcItem);
      AParent.AddArea(vChildArea);

      CopyPopupMenuItems(vChildArea, AView, vSrcItem, vChildArea);

      Continue;
    end
    else
      vView := AView.BuildView(vCaption);

    if Assigned(vView) and (vView.DefinitionKind = dkAction) then
    begin
      vAction := TActionDef(vView.Definition);
      vSrcItem.StyleName := 'action';
      vSrcItem.Caption := AParent.GetTranslation(vAction);
      vSrcItem.Hint := vSrcItem.Caption;
      vSrcItem.ImageID := vAction._ImageID;

      if (vAction.Name = 'Add') and Assigned(vView.ParentDomainObject) and (vView.ParentDomainObject is TEntityList) then
      begin
        vDefinitions := TEntityList(vView.ParentDomainObject).ContentDefinitions;
        if vDefinitions.Count > 1 then
        begin
          vSrcItem.StyleName := 'group';
          vChildArea := CreateArea(AParent, vView, vSrcItem);
          for j := 0 to vDefinitions.Count - 1 do
          begin
            vDefinition := TDefinition(vDefinitions[j]);
            vChildItem := AParent.UIBuilder.Layouts.CreateSimpleLayout(lkAction);
            vChildItem.StyleName := 'select';
            vChildItem.Caption := AParent.GetTranslation(vDefinition);
            vChildItem.Hint := vChildItem.Caption;
            vChildItem.ImageID := vDefinition._ImageID;
            vDefArea := CreateArea(vChildArea, AParent.RootView, vChildItem);
            vChildArea.AddArea(vDefArea);
          end;
        end
        else begin
          vChildArea := CreateArea(AParent, vView, vSrcItem);
          vChildArea.UpdateArea(dckViewStateChanged);
        end;
      end
      else begin
        vChildArea := CreateArea(AParent, vView, vSrcItem);
        vChildArea.UpdateArea(dckViewStateChanged);
      end;

      AParent.AddArea(vChildArea);
    end
    else begin
      if Assigned(vView) and (vView.DefinitionKind = dkUndefined) then
        vView.CleanView;

      vSrcItem.StyleName := 'group';
      vSrcItem.Caption := vCaption;
      vChildArea := CreateArea(AParent, AParent.RootView, vSrcItem);
      vChildArea.NativeControl.ViewState := vsDisabled;
      AParent.AddArea(vChildArea);
    end;
  end;
end;

constructor TPresenter.Create(const AName: string; const ASettings: TSettings);
var
  vThemeName: string;
  vThemeDir: string;
begin
  inherited Create;

  FName := AName;
  FStartParameter := '';
  FNativeControlClass := GetNativeControlClass;
  FCursorType := crtDefault;
  FCommonIcons := TIcons.Create;
  FCommonIcons.Load(GetPlatformDir);

  vThemeName := Trim(ASettings.GetValue('Core', 'Theme', ''));
  if vThemeName <> '' then
  begin
    vThemeDir := TPath.Combine(GetPlatformDir, 'themes' + PathDelim + vThemeName);
    if TDirectory.Exists(vThemeDir)  then
      FCommonIcons.Load(vThemeDir);
  end;

  FInteractors := TList<TInteractor>.Create;
{$IFDEF MSWINDOWS}
  if Length(GetPlatformDir) > 190 then
    ShowMessage('Предупреждение', 'Программа расположена по слишком длинному пути, она может работать неправильно.', msWarning);
{$ENDIF}
end;

function TPresenter.CreateArea(const AParent: TUIArea; const AView: TView;
  const ALayout: TLayout; const AParams: string; const AOnClose: TCloseProc): TUIArea;
var
  vDomain: TDomain;
  vInteractor: TInteractor;
  vStartPageName: string;
  vStartPageStr: string;
  vParams: TStrings;
  vLayoutExt: string;
begin
  vInteractor := TInteractor(AView.Interactor);
  vDomain := TDomain(vInteractor.Domain);

  if ALayout.AreaKind = akForm then
  begin
    Result := TUIArea.Create(AParent, AView, ALayout);
    if Assigned(AOnClose) then
      Result.OnClose := AOnClose;
  end
  else begin
    Result := TUIArea.Create(AParent, AView, ALayout, AParams);

    if ALayout.Name = '-popup-' then
      CopyPopupMenuItems(Result, AParent.View, TNavigationItem(ALayout), Result)
    else if ALayout.Name = '-pages-' then
    begin
      if AParams <> '' then
        vParams := CreateDelimitedList(AParams, '&')
      else
        vParams := nil;

      // Здесь можно подкорректировать параметры
      if StrToBoolDef(vDomain.UserSettings.GetValue('Core', 'ShowStartPage'), True) then
      begin
        vStartPageStr := vDomain.Settings.GetValue('Core', 'StartPage', '');

        vStartPageName := GetUrlCommand(vStartPageStr);
        vLayoutExt := IfThen(LoadFromDFM, LAYOUT_DFM_EXT, LAYOUT_XTF_EXT);
        if Assigned(vParams) and (vStartPageName <> '') and FileExists(vDomain.Configuration.FindLayoutFile(vStartPageName, vLayoutExt)) then
        begin
          vParams.Values['Layout'] := vStartPageName;
          vParams.Values['View'] := '';
        end;
      end;

      Result.AddParams(vParams);

      TInteractor(AParent.Interactor).PagedArea := Result;
    end;
  end;
end;

function TPresenter.CreateAreaContent(const AArea: TUIArea;
  const AView: TView; const ALayout: TLayout; const AParams: string): TNativeControl;
var
  vControl: TObject;
begin
  Result := GetNativeControlClass.Create(AArea, AParams);
  vControl := CreateControl(AArea.Parent, AView, ALayout, AParams);
  Result.CreateContent(vControl);
end;

function TPresenter.CreateAreaContentItem(const AOwner, AArea: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TNativeControl;
var
  vControlClass: TNativeControlClass;
  vControl: TObject;
begin
  vControlClass := GetNativeControlClass;
  Result := vControlClass.Create(AArea, '');
  if vControlClass.InheritsFrom(TNativeControlHolder) then
    vControl := TNativeControlHolder(AOwner.NativeControl).CreateItem(AArea.Parent, ANavItem, ACaption, AHint, AImageIndex)
  else
    vControl := nil;
  Result.CreateContent(vControl);
end;

function TPresenter.CreateControl(const AParent: TUIArea; const AView: TView;
  const ALayout: TLayout; const AParams: string): TObject;
begin
  Result := nil;
end;

function TPresenter.CreateImages(const ADomain: TObject; const ASize: Integer): TObject;
var
  vConfiguration: TConfiguration;
  vImages: TImages;
  vPlaceholder: TStream;
  vName: string;

  procedure AppendIconsToImages(const AIcons: TIconList; const AImages: TImages);
  var
    vName: string;
    vStream: TStream;
    vIndex: Integer;
  begin
    if not Assigned(AIcons) then Exit;
    
    for vName in AIcons.Items.Keys do
    begin
      vStream := AIcons.IconByName(vName);
      vIndex := TDomain(ADomain).UIBuilder.GetImageIndex(vName);
      AImages.Add(vName, vIndex, vStream);
      if vIndex < 0 then
        TDomain(ADomain).UIBuilder.StoreImageIndex(vName, vImages.Indices[vName]);
    end;
  end;
begin
  vImages := TImages.Create(ASize);
  try
    vConfiguration := TDomain(ADomain).Configuration;
    AppendIconsToImages(FCommonIcons[ASize], vImages);
    AppendIconsToImages(vConfiguration.Icons[ASize], vImages);

    vPlaceholder := vImages.Placeholder;
    if not Assigned(vPlaceholder) then
      vPlaceholder := GetImagePlaceholder(ASize);

    if Assigned(vPlaceholder) then
      vImages.FillWithPlaceholder(vPlaceholder);

    for vName in vImages.Indices.Keys do
      TDomain(ADomain).UIBuilder.StoreImageIndex(vName, vImages.Indices[vName]);

    Result := DoCreateImages(ADomain, vImages, ASize);
  finally
    if not Assigned(vImages.Placeholder) then
      FreeAndNil(vPlaceholder);
    FreeAndNil(vImages);
  end;
end;

function TPresenter.CreateItem(const AOwner, AArea: TUIArea;
  const ANavItem: TNavigationItem; const AView: TView): TNativeControl;
var
  vDefinition: TDefinition;
  vEntity: TEntity;
  vCaption, vHint: string;
  vImageID: string;
begin
  if AOwner.Layout.AreaKind <> akNavigation then
    Exit(nil);

  vCaption := ANavItem.Caption;
  vHint := ANavItem.Hint;
  vImageID := ANavItem.ImageID;

  if AView.DefinitionKind = dkDomain then
    Result := CreateAreaContentItem(AOwner, AArea, ANavItem, vCaption, vHint, AOwner.GetImageIndex(vImageID))
  else if AView.DefinitionKind in [dkAction, dkCollection] then
  begin
    vDefinition := TDefinition(AView.Definition);
    if vCaption = '' then
      vCaption := AOwner.GetTranslation(vDefinition);
    if vHint = '' then
      vHint := vCaption;
    if vImageID = '' then
      vImageID := vDefinition._ImageID;

    Result := CreateAreaContentItem(AOwner, AArea, ANavItem, vCaption, vHint, AOwner.GetImageIndex(vImageID));
  end
  else if (AView.DefinitionKind in [dkEntity, dkObjectField]) and (AView.DomainObject is TEntity) then
  begin
    vEntity := AView.DomainObject as TEntity;
    if (vCaption = '') and Assigned(vEntity) then
      vCaption := SafeDisplayName(vEntity, 'NULL');
    if vHint = '' then
      vHint := vCaption;

    Result := CreateAreaContentItem(AOwner, AArea, ANavItem, vCaption, vHint, AOwner.GetImageIndex(vImageID));
  end
  else
    Result := nil;
end;

procedure TPresenter.CreateMainForm(const ASession: TUserSession);
var
  vMainFormName: string;
  vDomain: TDomain;
  vInteractor: TInteractor;
begin
  if not Assigned(ASession) then
    Exit;

  vInteractor := TInteractor.Create(Self, ASession);
  FInteractors.Add(vInteractor);

  if not SameText(FName, 'Web.UniGUI') then
  begin
    vDomain := TDomain(ASession.Domain);
    vMainFormName := vDomain.Settings.GetValue('Core', 'MainForm', '');
    if Trim(vMainFormName) = '' then
      vMainFormName := 'MainForm';
    vDomain.UIBuilder.Navigate(vInteractor.RootView, '', vMainFormName, '', ASession.NullHolder);

    RestoreUILayout(vInteractor, vInteractor.RootArea.NativeControl);

    TLoginedProc(vDomain.Configuration.LoginedProc)(vInteractor);
  end;
end;

function TPresenter.CreateNativeControl(const AArea: TUIArea; const AView: TView; const ALayout: TLayout;
  const AControlType: TUIItemType; const AStyleName, AParams: string): TNativeControl;
var
  vParams: string;
  vStyleName: string;
  vOwner: TUIArea;
  vControlType: TUIItemType;
  vControlClass: TNativeControlClass;
begin
  vStyleName := GetUrlParam(AParams, 'view');
  if vStyleName = '' then
    vStyleName := GetUrlParam(AParams, 'style');
  if vStyleName = '' then
    vStyleName := GetUrlParam(AParams, 'ViewStyle');
  if vStyleName = '' then
    vStyleName := GetUrlParam(AParams, 'ViewType');

  if (vStyleName = '') and (AView.DefinitionKind in [dkListField..dkComplexField]) then
    vStyleName := TFieldDef(AView.Definition).StyleName;

  vControlType := AControlType;
  if vControlType = uiUnknown then
  begin
    if AView.DefinitionKind = dkEntity then
      vControlType := uiEntityEdit
    else if ALayout.AreaKind = akNavigation then
      vControlType := uiNavigation
    else if ALayout.AreaKind in [akAutoDetect, akForm] then
      vControlType := uiDecor
    else if ALayout.AreaKind = akList then
      vControlType := uiCollection
    else if ALayout.AreaKind = akAction then
      vControlType := uiAction
    else
      vControlType := ItemTypeByFieldType(TFieldDef(AView.Definition).Kind);
  end;

  if ALayout.Kind = lkPages then
    vStyleName := 'pages'
  else
    vStyleName := GetUrlCommand(vStyleName, vStyleName);
  vParams := ExtractUrlParams(vStyleName);
  if vParams = '' then
    vParams := AParams
  else
    vParams := vParams + '&' + AParams;

  vControlClass := TNativeControlClass(GetControlClass(vControlType, vStyleName));
  if ALayout.AreaKind in [akAutoDetect, akForm] then
  begin
    if ALayout.Kind = lkNavItem then
    begin
      vOwner := AArea.Parent;
      while Assigned(vOwner) and (vOwner.Layout.Kind = lkNavItem) do
        vOwner := vOwner.Parent;

      Result := CreateItem(vOwner, AArea, TNavigationItem(ALayout), AView);
    end
    else
      Result := CreateAreaContent(AArea, AView, ALayout, vParams);
  end
  else if Assigned(vControlClass) then
  begin
    Result := vControlClass.Create(AArea, vParams);
    Result.CreateContent(nil);
  end
  else begin
    AArea.UIBuilder.Layouts.ApplyAbsentLayout(ALayout);
    Result := CreateAreaContent(AArea, AView, ALayout, vParams);
  end;
end;

destructor TPresenter.Destroy;
begin
  FreeAndNil(FInteractors);
  FreeAndNil(FCommonIcons);
  inherited Destroy;
end;

procedure TPresenter.DoArrangePages(const AInteractor: TInteractor;
  const AArrangeKind: TWindowArrangement);
var
  i: Integer;
  vClientRect: TRect;
  vFormControl: TNativeControl;
  vForms: TList<TNativeControl>;
begin
  vForms := TList<TNativeControl>.Create;
  try
    for i := 0 to AInteractor.RootArea.Count - 1 do
      if AInteractor.RootArea.Areas[i].NativeControl.IsForm then
        vForms.Add(AInteractor.RootArea.Areas[i].NativeControl);

    if vForms.Count = 0 then
      Exit;

    vFormControl := AInteractor.RootArea.NativeControl;
    vClientRect := vFormControl.ClientRect;

    case AArrangeKind of
      waCascade: ArrangeCascade(vClientRect, vForms);
      waTileHorz: ArrangeHorz(vClientRect, vForms);
      waTileVert: ArrangeVert(vClientRect, vForms);
      waMozaic: ArrangeMozaic(vClientRect, vForms);
    else
      //waNone: do nothing
    end;
  finally
    FreeAndNil(vForms);
  end;
end;

procedure TPresenter.DoChildFormClose(Sender: TObject; var Action: TCloseAction);
var
  vArea: TUIArea;
  vInteractor: TInteractor;
  vHolder: TChangeHolder;
  vChildArea: TUIArea;
  i: Integer;
  vCloseProc: TCloseProc;
  vModalResult: TModalResult;

  function GetUnfilledRequiredFields(const AArea: TUIArea; var AFields: string): Boolean;
  var
    i: Integer;
    vFieldDef: TFieldDef;
    vChildArea: TUIArea;
  begin
    for i := 0 to AArea.Count - 1 do
    begin
      vChildArea := AArea.Areas[i];
      if (vChildArea.View.DefinitionKind in [dkListField, dkObjectField, dkSimpleField]) then
      begin
        vFieldDef := TFieldDef(vChildArea.View.Definition);
        if Assigned(vChildArea.View.ParentDomainObject)
          and not TEntity(vChildArea.View.ParentDomainObject).FieldByName(vFieldDef.Name).IsValid then
        begin
          if Length(AFields) > 0 then
            AFields := AFields + ', ';
          AFields := AFields + TDomain(vInteractor.Domain).TranslateFieldDef(vFieldDef);
        end;
      end
      else
        GetUnfilledRequiredFields(vChildArea, AFields);
    end;

    Result := Length(AFields) > 0;
  end;

  function DoValidation: Boolean;
  var
    vView: TView;
    vEmptyRequiredFields: string;
    vEntity: TEntity;
    vSimilar: TEntity;
  begin
    Result := True;
    vView := vArea.View;
    if not (vView.DefinitionKind in [dkObjectField, dkEntity, dkAction]) then
      Exit;

    vEntity := TEntity(vView.DomainObject);
    if not Assigned(vEntity) then
      Exit;

    vEmptyRequiredFields := '';
    if GetUnfilledRequiredFields(vArea, vEmptyRequiredFields) then
    begin
      vInteractor.ShowMessage(vInteractor.Translate('msgRequiredFieldsAreEmpty', 'Не заполнены обязательные поля') +
        '. ' + #13#10 + vEmptyRequiredFields);
      Exit(False);
    end;

    vSimilar := vEntity.FindSimilar(vInteractor.Session);
    if Assigned(vSimilar) then
    begin
      vInteractor.ShowMessage(vInteractor.Translate('msgRecordExists', 'Такая запись уже существует') +
        ' [' + vSimilar['Name'] + ']');
      Exit(False);
    end;
  end;
begin
  vArea := AreaFromSender(Sender);
  vInteractor := TInteractor(vArea.Interactor);
  vHolder := TChangeHolder(vArea.ThisHolder);
  vModalResult := vArea.NativeControl.ModalResult;
  if vModalResult = mrNone then
    vModalResult := mrCancel;
  Action := TCloseAction.caNone;

  if vArea.IsClosing or (vModalResult = mrOk)
    or not Assigned(vHolder) or not vHolder.IsVisibleModified then
  begin
    for i := 0 to vArea.Count - 1 do
    begin
      vChildArea := vArea[i];
      if vChildArea.NativeControl.IsForm then
      begin
        if Assigned(vInteractor) then
          vInteractor.ShowMessage('Невозможно закрыть окно, так как есть другие программные окна, зависящие от него', msWarning);
        Exit;
      end;
    end;

    if not CanCloseChildForm(Sender, vModalResult) then
      Exit;

    Action := TCloseAction.caFree;

    vCloseProc := vArea.OnClose;

    vInteractor.LastArea := nil;
    vInteractor.CurrentArea := vInteractor.AreaStack.Pop;

    vArea.SetHolder(nil);
    if Assigned(vArea.Parent) then
      vArea.Parent.RemoveArea(vArea)
    else
      vArea.Release;

    if Assigned(vCloseProc) then
      vCloseProc(ModalResultToDialogResult(vModalResult));
  end
  else begin
    ShowYesNoDialog('Подтвердите', vInteractor.Translate('msgPromtSaveChanges',
      'Сохранить изменения перед закрытием формы?'), True, procedure(const AResult: TDialogResult)
      begin
        if AResult = drYes then
          vArea.Close(mrOk)
        else if AResult = drNo then
          vArea.Close(mrCancel);
      end);
  end;
end;

procedure TPresenter.DoChildFormKeyDown(Sender: TObject; const AShift: TShiftState; const AKey: Word; var AHandled: Boolean);
var
  vFormArea: TUIArea;
  vView: TView;
begin
  vFormArea := AreaFromSender(Sender);

  AHandled := True;
  if (AKey = vkReturn) and (ssCtrl in AShift) then
    vFormArea.Close(mrOk)
  else if AKey = vkEscape then
  begin
    TInteractor(vFormArea.Interactor).LastArea := nil;
    vView := vFormArea.View.ViewByName('Close');
    if not Assigned(vView) then
      vView := vFormArea.View.ViewByName('Cancel');
    if Assigned(vView) then
      vFormArea.ExecuteUIAction(vView)
    else
      vFormArea.Close(mrCancel);
  end
  else
    AHandled := False;
end;

procedure TPresenter.DoCloseAllPages(const AInteractor: TInteractor);
begin
end;

procedure TPresenter.DoEnumerateControls(const ALayout: TLayout; const AControl: TObject);
begin
end;

procedure TPresenter.DoFloatFormClose(Sender: TObject; var Action: TCloseAction);
var
  vArea: TUIArea;
  vView: TView;
  vCloseView: TView;
  vInteractor: TInteractor;
  vCanBeClosed: Boolean;
begin
  vArea := AreaFromSender(Sender);
  vView := vArea.View;
  vCloseView := vView.BuildView('Close');
  vCloseView.AddListener(vArea);
  try
    vInteractor := TInteractor(vArea.Interactor);

    vCanBeClosed := not (Assigned(vCloseView) and (TCheckActionFlagsFunc(TConfiguration(vInteractor.Configuration).
      CheckActionFlagsFunc)(vCloseView, nil) <> vsFullAccess));

    if vCanBeClosed then
    begin
      if Assigned(vArea.OnClose) then
        vArea.OnClose(drOk);

      // Возможно, нужно сбросить CurrentArea у UIBuilder-а в vArea.Parent

      vArea.SetHolder(nil);
      if Assigned(vArea.Parent) then
        vArea.Parent.RemoveArea(vArea);

      if SameText(FName, 'Web.UniGUI') then
        Action := TCloseAction.caFree
      else
        Action := TCloseAction.caNone;

      if Assigned(vInteractor.UIBuilder) then
        vInteractor.PrintHierarchy;
    end
    else
      Action := TCloseAction.caNone;
  finally
    vCloseView.RemoveListener(vArea);
  end;
end;

procedure TPresenter.DoLogin(const ADomain: TObject);
var
  vDomain: TDomain absolute ADomain;
  vSession: TUserSession;
  vUsers: TEntityList;
  vLoginData: TEntity;
  vResult: TDialogResult;
begin
  vSession := nil;
  vUsers := TEntityList.Create(vDomain, vDomain.DomainSession);
  vDomain.GetEntityList(vDomain.DomainSession, vDomain.Configuration['SysUsers'], vUsers, '');
  try
    if vUsers.Count = 0 then
    begin
      vSession := vDomain.Sessions.AddSession(nil);
      CreateMainForm(vSession);

      Exit;
    end
    else if (vUsers.Count = 1) and (vDomain.Settings.GetValue('Core', 'AutoLogin', '') = '1') then
    begin
      vSession := vDomain.Sessions.AddSession(vUsers[0]);
      CreateMainForm(vSession);

      Exit;
    end;
  finally
    FreeAndNil(vUsers);
  end;

  vLoginData := vDomain.FirstEntity('SysServices');
  Assert(Assigned(vLoginData), '');

  vLoginData.Populate('Login;Password', [vDomain.UserSettings.GetValue('Core', 'LastLogin', ''), '']);

  if SameText(FName, 'Web.UniGUI') then
  begin
    vDomain.UIBuilder.Navigate(TInteractor(vDomain.DomainInteractor).RootView,
      'modal', 'LoginForm', '', nil, vDomain.AppTitle, procedure(const AResult: TDialogResult)
      begin
        if AResult = drOk then
        begin
          vSession := vDomain.Login(vLoginData['Login'], vLoginData['Password']);
          if Assigned(vSession) then
          begin
            vLoginData._SetFieldValue(vDomain.DomainHolder, 'Password', '~');
            vDomain.UserSettings.SetValue('Core', 'LastLogin', vLoginData['Login']);

            CreateMainForm(vSession);
            Exit;
          end
          else begin
            ShowYesNoDialog(_Platform.Translate('cptError', 'Ошибка'), _Platform.Translate('txtWrongLoginOrPassword',
              'Введены некорректные имя пользователя или пароль') + #13#10 + _Platform.Translate('txtPromptTryAgain',
              'Попробовать ещё раз?'), False, procedure(const AResult: TDialogResult)
              begin
                if AResult = drYes then
                  DoLogin(ADomain)
                else
                  SetApplicationUI(cPlatformTitle);
              end);
          end;
        end
        else
          SetApplicationUI(cPlatformTitle);
      end)
  end
  else begin
    vDomain.UIBuilder.Navigate(TInteractor(vDomain.DomainInteractor).RootView,
      'modal', 'LoginForm', '', nil, vDomain.AppTitle, procedure(const AResult: TDialogResult)
      begin
        vResult := AResult;
      end);

    if vResult = drOk then
    begin
      vSession := vDomain.Login(vLoginData['Login'], vLoginData['Password']);
      if Assigned(vSession) then
      begin
        vLoginData._SetFieldValue(vDomain.DomainHolder, 'Password', '~');
        vDomain.UserSettings.SetValue('Core', 'LastLogin', vLoginData['Login']);

        CreateMainForm(vSession);
        Exit;
      end
      else begin
        ShowYesNoDialog(_Platform.Translate('cptError', 'Ошибка'), _Platform.Translate('txtWrongLoginOrPassword',
          'Введены некорректные имя пользователя или пароль') + #13#10 + _Platform.Translate('txtPromptTryAgain',
          'Попробовать ещё раз?'), False, procedure(const AResult: TDialogResult)
          begin
            if AResult = drYes then
              DoLogin(ADomain)
            else
              Stop;
          end);
      end;
    end
    else
      Stop;
  end;
end;

procedure TPresenter.DoLogout(const AInteractor: TInteractor);
begin
  StoreUILayout(AInteractor, AInteractor.RootArea.NativeControl);
end;

procedure TPresenter.DoMainFormClose(Sender: TObject; var Action: TCloseAction);
var
  vArea: TUIArea;
  vInteractor: TInteractor;
begin
  vArea := AreaFromSender(Sender);
  vInteractor := TInteractor(vArea.Interactor);
  if vArea.IsClosing or not Assigned(vInteractor) then
  begin
    if not SameText(FName, 'Web.UniGUI') then
    begin
      if Assigned(vInteractor) then
        Logout(vInteractor);
    end;

    Action := TCloseAction.caFree;
    Exit;
  end;

  Action := TBeforeUIClosingFunc(TConfiguration(vInteractor.Configuration).
    BeforeUIClosingFunc)(vInteractor, procedure(const AResult: TDialogResult)
    begin
      if AResult = drYes then
        vArea.Close(mrNone);
    end);

  if not SameText(FName, 'Web.UniGUI') then
    Action := TCloseAction.caNone;
end;

procedure TPresenter.DoOnFormShow(Sender: TObject);
var
  vArea: TUIArea;
begin
  vArea := AreaFromSender(Sender);
  vArea.Activate('');
end;

procedure TPresenter.DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False);
begin
end;

procedure TPresenter.DoProcessShortCut(const AShift: TShiftState; const AKey: Word; var AHandled: Boolean);
var
  vActiveInteractor: TInteractor;
  vView: TView;
  vViewExists: Boolean;
  vShortCut: TShortCut;
  vActionAreas: TList<TUIArea>;
  vActionArea: TUIArea;

  function FindAreaByShortCut(const AResultList: TList<TUIArea>; const AParentArea: TUIArea;
    const ASkipArea: TUIArea = nil): TUIArea;
  var
    i, j: Integer;
    vArea: TUIArea;
  begin
    Result := nil;
    if not Assigned(AParentArea) then
      Exit;
    if not AParentArea.Count = 0 then
      Exit;

    for i := 0 to AParentArea.Count - 1 do
    begin
      vArea := AParentArea.Areas[i];
      if vArea = ASkipArea then
        Continue;

      if vArea.View.DefinitionKind in [dkDomain, dkCollection, dkEntity, dkListField, dkObjectField] then
        FindAreaByShortCut(AResultList, vArea)
      else if vArea.View.DefinitionKind = dkAction then
      begin
        if TActionDef(vArea.View.Definition).ShortCutExists(vShortCut) then
        begin
          // Для гридов с PopupMenu действие добавится дважды, проверяем уникальность по View
          vViewExists := False;
          for j := 0 to AResultList.Count - 1 do
          begin
            vViewExists := AResultList[j].View = vArea.View;
            if vViewExists then
              Break;
          end;

          if not vViewExists then
            AResultList.Add(vArea);
        end;
      end;

      if AResultList.Count > 1 then
        Exit;
    end;
  end;

  procedure FindAreaByShortCutOnCurrentArea(const AResultList: TList<TUIArea>;
    const AStartArea, AEndArea: TUIArea);
  var
    vCurArea, vParentArea: TUIArea;
  begin
    if not Assigned(AStartArea) then
      FindAreaByShortCut(AResultList, AEndArea)
    else begin
      vCurArea := nil;
      vParentArea := AStartArea;
      repeat
        FindAreaByShortCut(AResultList, vParentArea, vCurArea);
        if (AResultList.Count > 0) or (vParentArea = AEndArea) then
          Exit;

        vCurArea := vParentArea;
        vParentArea := vCurArea.Parent;
      until not Assigned(vParentArea);
    end;
  end;
begin
  vActiveInteractor := ActiveInteractor;
  if Assigned(vActiveInteractor) then
  begin
    if (ssCtrl in AShift) and (ssAlt in AShift) and (ssShift in AShift) then
    begin
      if AKey = vkE then
      begin
        AHandled := True;
        if not Assigned(vActiveInteractor.ActiveArea) then
          Exit;
        vView := vActiveInteractor.ActiveArea.View;
        if not (vView.DefinitionKind in [dkAction, dkListField, dkObjectField, dkSimpleField]) then
          Exit;
        if not TUserSession(vActiveInteractor.Session).IsAdmin then
          Exit;

        vView.ElevateAccess;
        Exit;
      end
      else if AKey = vkR then
      begin
        AHandled := True;
        TDomain(vActiveInteractor.Domain).ReloadChanges(TDomain(vActiveInteractor.Domain).DomainHolder);
        Exit;
      end;
    end;

    vShortCut := KeyStateToShortCut(AShift, AKey);
    if vShortCut = 0 then
      Exit;

    vActionAreas := TList<TUIArea>.Create;
    try
      FindAreaByShortCutOnCurrentArea(vActionAreas, vActiveInteractor.ActiveArea,
        vActiveInteractor.CurrentArea);
      if vActionAreas.Count = 1 then
      begin
        AHandled := True;
        vActionArea := vActionAreas[0];
        vActionArea.ExecuteUIAction(vActionArea.View);
      end;
    finally
      FreeAndNil(vActionAreas);
    end;
  end;
end;

procedure TPresenter.DoRun(const AParameter: string);
begin
  FStartParameter := AParameter;
end;

procedure TPresenter.DoSetCursor(const ACursorType: TCursorType);
begin
end;

procedure TPresenter.DoShowOpenDialog(const AFileName: string; const ATitle, AFilter,
  ADefaultExt, ADefaultDir: string; const AOnClose: TCloseTextProc);
begin
end;

procedure TPresenter.DoShowSaveDialog(const AFileName: string; const ATitle, AFilter,
  ADefaultExt: string; const AOnClose: TCloseTextProc);
begin
end;

procedure TPresenter.DoStop;
begin
end;

procedure TPresenter.DoUnfreeze;
begin
end;

procedure TPresenter.EnumerateControls(const ALayout: TLayout; const AControl: TObject);
begin
  DoEnumerateControls(ALayout, AControl);
end;

function TPresenter.GetCanvasClass(const ACanvasTypeName: string): TClass;

  function InternalGetCanvasClass(const APresenterName: string): TClass;
  var
    vClassesList: TDictionary<string, TClass>;
  begin
    Result := nil;
    if RegisteredCanvasClasses.TryGetValue(APresenterName, vClassesList) then
      if not vClassesList.TryGetValue(ACanvasTypeName, Result) then
        Result := nil;
  end;

  function FindCanvasClass(const APresenterName: string): TClass;
  var
    vPresenterName: string;
    vModuleInfo: TModuleInfo;
    vAncestors: TStrings;
    i: Integer;
  begin
    vPresenterName := APresenterName.ToLowerInvariant;

    Result := InternalGetCanvasClass(vPresenterName);
    if Assigned(Result) then
      Exit;

    vModuleInfo := TBaseModule.GetModuleInfo('UI', APresenterName);
    if not Assigned(vModuleInfo) then
      Exit;

    vAncestors := CreateDelimitedList(vModuleInfo.Ancestors);
    try
      for i := 0 to vAncestors.Count - 1 do
      begin
        Result := FindCanvasClass(Trim(vAncestors[i]));
        if Assigned(Result) then
          Exit;
      end;
    finally
      FreeAndNil(vAncestors);
    end;

    Result := nil;
  end;
begin
  Result := FindCanvasClass(FName);
end;

function TPresenter.GetControlClass(const AControlType: TUIItemType;
  const AStyleName: string): TNativeControlClass;
var
  vTypeName, vStyleName: string;

  function InternalGetControlClass(const APresenterName, ATypeName, AStyleName: string): TNativeControlClass;
  var
    vClassName: string;
    vClassInfo: TControlClassInfo;
    vClassesList: TObjectDictionary<string, TControlClassInfo>;
  begin
    Result := nil;
    if not RegisteredControlClasses.TryGetValue(APresenterName, vClassesList) then
      Exit;

    if AStyleName = '' then
      vClassName := ATypeName
    else
      vClassName := ATypeName + '_' + AStyleName;

    if vClassesList.TryGetValue(vClassName, vClassInfo) then
      Result := vClassInfo.FControlClass
    else
      Result := nil;
  end;

  function FindControlClass(const APresenterName, ATypeName, AStyleName: string): TNativeControlClass;
  var
    vPresenterName: string;
    vModuleInfo: TModuleInfo;
    vAncestors: TStrings;
    i: Integer;
  begin
    vPresenterName := APresenterName.ToLowerInvariant;
    Result := InternalGetControlClass(vPresenterName, ATypeName, AStyleName);
    if Assigned(Result) then
      Exit;

    vModuleInfo := TBaseModule.GetModuleInfo('UI', APresenterName);
    if not Assigned(vModuleInfo) then
      Exit;

    vAncestors := CreateDelimitedList(vModuleInfo.Ancestors);
    try
      for i := 0 to vAncestors.Count - 1 do
      begin
        Result := FindControlClass(Trim(vAncestors[i]), ATypeName, AStyleName);
        if Assigned(Result) then
          Exit;
      end;
    finally
      FreeAndNil(vAncestors);
    end;

    Result := nil;
  end;
begin
  vTypeName := cControlTypeNames[AControlType].ToLowerInvariant;
  vStyleName := AStyleName.ToLowerInvariant;

  Result := FindControlClass(FName, vTypeName, vStyleName);
  if not Assigned(Result) and (vStyleName <> '') then
    Result := FindControlClass(FName, vTypeName, '');
end;

function TPresenter.GetRealControl(const AArea: TUIArea): TObject;
var
  vNativeControl: TNativeControl;
begin
  if not Assigned(AArea) then
    Exit(nil);

  vNativeControl := AArea.NativeControl;
  if vNativeControl is TNativeControlHolder then
    Result := TNativeControlHolder(vNativeControl).Control
  else
    Result := nil;
end;

class function TPresenter.GetSceneObjectClass(const AName: string): TDomainSceneObjectClass;
begin
  if not RegisteredSceneObjectClasses.TryGetValue(AName.ToLowerInvariant, Result) then
    Result := nil;
end;

function TPresenter.GetWidthByType(const AWidth: Integer; const AFieldDef: TFieldDef): Integer;
begin
  case AFieldDef.Kind of
    fkInteger: Result := 70;
    fkFloat: Result := 70;
    fkDateTime: Result := 80;
    fkCurrency: Result := 80;
  else
    Result := AWidth - 4;
  end;
end;

procedure TPresenter.Login(const ADomain: TObject);
begin
  DoLogin(ADomain);
end;

procedure TPresenter.Logout(const AInteractor: TInteractor);
begin
  DoLogout(AInteractor);
  TDomain(AInteractor.Domain).Logout(TUserSession(AInteractor.Session));
  FInteractors.Remove(AInteractor);
end;

function TPresenter.ModalResultToDialogResult(const AModalResult: TModalResult): TDialogResult;
begin
  case AModalResult of
    mrOk: Result := drOk;
    mrCancel: Result := drCancel;
    mrYes: Result := drYes;
    mrNo: Result := drNo;
  else
    Result := drNone;
  end;
end;

procedure TPresenter.OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
var
  vArea: TUIArea;
  vView: TView;
  vCloseView: TView;
  vInteractor: TInteractor;
  vCanBeClosed: Boolean;
  vCloseProc: TCloseProc;
  vDelEntity: TEntity;
begin
  vArea := AreaFromSender(Sender);
  vView := vArea.View;
  vInteractor := TInteractor(vArea.Interactor);

  vCloseView := vView.BuildView('Close');
  vCloseView.AddListener(vArea);
  vCloseProc := nil;
  vDelEntity := nil;

  try
    vCanBeClosed := not (Assigned(vCloseView) and (TCheckActionFlagsFunc(TConfiguration(vInteractor.Configuration).
      CheckActionFlagsFunc)(vCloseView, nil) <> vsFullAccess));

    if (vView.DomainObject is TEntity) and TEntity(vView.DomainObject).InstanceOf('_FormLayout') then
      vDelEntity := TEntity(vView.DomainObject);

    if vCanBeClosed then
    begin
      vCloseProc := vArea.OnClose;
      vArea.UnbindContent(True);
      vArea.RootArea.RemoveArea(vArea); // the form will be destroyed here
    end
    else
      Action := TCloseAction.caNone;
  finally
    vCloseView.RemoveListener(vArea);
  end;

  if vCanBeClosed and Assigned(vDelEntity) then
  begin
    TUserSession(vInteractor.Session).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
    begin
      vDelEntity.Delete(AHolder);
      Result := True;
    end, nil);
  end;

  if Assigned(vCloseProc) then
    vCloseProc(drOk);
end;

procedure TPresenter.OnPCCanClose(Sender: TObject; var ACanClose: Boolean);
var
  vPCArea: TUIArea;
  vTabArea: TUIArea;
begin
  // Closing will be done using TUIArea mechanics
  ACanClose := False;

  vPCArea := AreaFromSender(Sender);
  vTabArea := vPCArea.ActiveChildArea;
  if not Assigned(vTabArea) then
    Exit;

  TInteractor(vPCArea.Interactor).LastArea := nil;

  vPCArea.RemoveArea(vTabArea);

  TInteractor(vPCArea.Interactor).PrintHierarchy;
end;

procedure TPresenter.OnTabClose(Sender: TObject; var AllowClose: Boolean);
var
  vPCArea: TUIArea;
  vTabArea: TUIArea;
begin
  // Closing will be done using TUIArea mechanics
  AllowClose := False;

  vTabArea := AreaFromSender(Sender);
  vPCArea := vTabArea.Parent;

  TInteractor(vPCArea.Interactor).LastArea := nil;

  vPCArea.RemoveArea(vTabArea);

  TInteractor(vPCArea.Interactor).PrintHierarchy;
end;

procedure TPresenter.OpenFile(const AFileName: string; const ADefaultApp: string = ''; const Await: Boolean = False);
begin
  DoOpenFile(AFileName, ADefaultApp, Await);
end;

class procedure TPresenter.RegisterCanvasClass(const APresenterName, ACanvasTypeName: string; const ACanvasClass: TClass);
var
  vPresenterName: string;
  vClassesList: TDictionary<string, TClass>;
begin
  vPresenterName := APresenterName.ToLowerInvariant;

  if not RegisteredCanvasClasses.TryGetValue(vPresenterName, vClassesList) then
  begin
    vClassesList := TDictionary<string, TClass>.Create;
    RegisteredCanvasClasses.Add(vPresenterName, vClassesList);
  end
  else
    Assert(not vClassesList.ContainsKey(ACanvasTypeName), 'Canvas already registered for type: "' + ACanvasTypeName + '"');

  vClassesList.Add(ACanvasTypeName, ACanvasClass);
end;

class procedure TPresenter.RegisterControlClass(const APresenterName: string; const AControlType: TUIItemType;
  const AStyleName: string; const AControlClass: TNativeControlClass);
var
  vPresenterName: string;
  vTypeName, vStyleName, vClassName: string;
  vClassesList: TObjectDictionary<string, TControlClassInfo>;
  vClassInfo: TControlClassInfo;
begin
  vPresenterName := APresenterName.ToLowerInvariant;
  vTypeName := cControlTypeNames[AControlType].ToLowerInvariant;
  vStyleName := Trim(AStyleName.ToLowerInvariant);
  if Trim(AStyleName) <> '' then
    vClassName := vTypeName + '_' + vStyleName
  else
    vClassName := vTypeName;

  if not RegisteredControlClasses.TryGetValue(vPresenterName, vClassesList) then
  begin
    vClassesList := TObjectDictionary<string, TControlClassInfo>.Create([doOwnsValues]);
    RegisteredControlClasses.Add(vPresenterName, vClassesList);
  end
  else
    Assert(not vClassesList.TryGetValue(vClassName, vClassInfo),
      'Control class already registered for type: "' + vTypeName + '", style name: "' + vStyleName + '"');

  vClassInfo := TControlClassInfo.Create(AControlType, vStyleName, AControlClass);
  vClassesList.Add(vClassName, vClassInfo);
end;

class procedure TPresenter.RegisterSceneObjectClass(const AName: string;
  const ASceneObjectClass: TDomainSceneObjectClass);
begin
  RegisteredSceneObjectClasses.AddOrSetValue(Aname.ToLowerInvariant, ASceneObjectClass);
end;

procedure TPresenter.RestoreUILayout(const AInteractor: TInteractor;
  const AForm: TNativeControl);
var
  vLayoutStr: string;
  vValues: TStrings;
  vRect: TRect;
begin
  //if (not Assigned(vMainForm)) or (vMainForm.Position <> poDesigned) then Exit;

  vLayoutStr := TDomain(AInteractor.Domain).UserSettings.GetValue('MainForm', 'Layout');
  if Length(vLayoutStr) = 0 then Exit;

  vValues := CreateDelimitedList(vLayoutStr, ';');
  try
    if vValues.Count <> 5 then Exit;

    vRect.Left := StrToIntDef(vValues[0], 100);
    vRect.Top := StrToIntDef(vValues[1], 100);
    vRect.Width := StrToIntDef(vValues[2], 1280);
    vRect.Height := StrToIntDef(vValues[3], 960);
    AForm.Bounds := vRect;
    if TWindowState(StrToIntDef(vValues[4], 0)) = TWindowState.wsMaximized then
      AForm.WindowState := TWindowState.wsMaximized
    else
      AForm.WindowState := TWindowState.wsNormal;
  finally
    FreeAndNil(vValues);
  end;
end;

procedure TPresenter.Run(const AParameter: string = '');
begin
  DoRun(AParameter);
end;

function TPresenter.SetCursor(const ACursorType: TCursorType): TCursorType;
begin
  Result := FCursorType;
  if FCursorType = ACursorType then
    Exit;
  FCursorType := ACursorType;
  DoSetCursor(FCursorType);
end;

procedure TPresenter.ShowDialog(const ACaption, AText: string;
  const ADialogActions: TDialogResultSet; const AOnClose: TCloseProc = nil);
begin
  DoShowDialog(ACaption, AText, ADialogActions, AOnClose);
end;

procedure TPresenter.ShowMessage(const ACaption, AText: string; const AMessageType: TMessageType = msNone);
begin
  DoShowMessage(ACaption, AText, AMessageType);
end;

procedure TPresenter.ShowOkCancelDialog(const ACaption, AText: string; const AOnClose: TCloseProc = nil);
begin
  DoShowDialog(ACaption, AText, [drOk, drCancel], AOnClose);
end;

procedure TPresenter.ShowOpenDialog(const AFileName: string; const ATitle, AFilter,
  ADefaultExt, ADefaultDir: string; const AOnClose: TCloseTextProc);
begin
  DoShowOpenDialog(AFileName, ATitle, AFilter, ADefaultExt, ADefaultDir, AOnClose);
end;

procedure TPresenter.ShowPage(const AInteractor: TInteractor; const APageType: string;
  const AParams: TObject = nil; const AOnClose: TCloseProc = nil);
begin
end;

procedure TPresenter.ShowSaveDialog(const AFileName: string; const ATitle, AFilter,
  ADefaultExt: string; const AOnClose: TCloseTextProc);
begin
  DoShowSaveDialog(AFileName, ATitle, AFilter, ADefaultExt, AOnClose);
end;

procedure TPresenter.ShowUIArea(const AArea: TUIArea; const AAreaName: string; const ACaption: string);
begin
end;

procedure TPresenter.ShowYesNoDialog(const ACaption, AText: string;
  const AWithCancel: Boolean; const AOnClose: TCloseProc);
var
  vDialogButtons: TDialogResultSet;
begin
  vDialogButtons := [drYes, drNo];
  if AWithCancel then
    Include(vDialogButtons, drCancel);
  DoShowDialog(ACaption, AText, vDialogButtons, AOnClose);
end;

procedure TPresenter.Stop;
begin
  DoStop;
end;

procedure TPresenter.StoreUILayout(const AInteractor: TInteractor;
  const AForm: TNativeControl);
var
  vLayoutStr: string;
  vRect: TRect;
begin
  vRect := AForm.Bounds;
  vLayoutStr := IntToStr(vRect.Left) + ';' + IntToStr(vRect.Top) + ';' + IntToStr(vRect.Width) + ';' +
    IntToStr(vRect.Height) + ';' + IntToStr(Ord(AForm.WindowState));
  TDomain(AInteractor.Domain).UserSettings.SetValue('MainForm', 'Layout', vLayoutStr);
end;

procedure TPresenter.Unfreeze;
begin
  DoUnfreeze;
end;

{ TProgressInfo

constructor TProgressInfo.Create;
begin
  inherited Create;
  FProgress := 0;
  FInfo := '';
  FStartTime := Now;
end;

procedure TProgressInfo.SetProgress(const AProgress: Integer; const AInfo: string);
var
  vElapsedTime: Double;
begin
  FProgress := AProgress;
  vElapsedTime := Max((Now - FStartTime) * SecsPerDay, 0);
  FInfo := '[' + FormatFloat('0.##', vElapsedTime) + '] ' + AInfo + '...';
end; }

{ TControlClassInfo }

constructor TControlClassInfo.Create(const AControlType: TUIItemType; const AStyleName: string;
  const AControlClass: TNativeControlClass);
begin
  FName := AStyleName;
  FType := AControlType;
  FControlClass := AControlClass;
end;

{ TImages }

procedure TImages.Add(const AName: string; const AIndex: Integer; const AStream: TStream);
var
  i: Integer;
begin
  if AName = '' then
    FPlaceholder := AStream;

  if AIndex < 0 then
  begin
    FIndices.Add(AName, FItems.Count);
    FItems.Add(AStream);
  end
  else if AIndex >= FItems.Count then
  begin
    for i := FItems.Count to AIndex - 1 do
      FItems.Add(nil);
    FItems.Add(AStream);
  end
  else
    FItems[AIndex] := AStream;
end;

constructor TImages.Create(const ASize: Integer);
begin
  inherited Create;
  FSize := ASize;
  FItems := TList<TStream>.Create;
  FIndices := TDictionary<string, Integer>.Create;
  FPlaceholder := nil;
end;

destructor TImages.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FIndices);
  FPlaceholder := nil;
  inherited Destroy;
end;

procedure TImages.FillWithPlaceholder(const APlaceholder: TStream);
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    if not Assigned(FItems[i]) then
      FItems[i] := APlaceholder;
end;

function TImages.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TImages.GetItem(const AIndex: Integer): TStream;
begin
  Result := FItems[AIndex];
end;

initialization

TPresenter.RegisteredCanvasClasses := TObjectDictionary<string, TDictionary<string, TClass>>.Create([doOwnsValues]);
TPresenter.RegisteredControlClasses := TObjectDictionary<string, TObjectDictionary<string, TControlClassInfo>>.Create([doOwnsValues]);
TPresenter.RegisteredSceneObjectClasses := TDictionary<string, TDomainSceneObjectClass>.Create;

finalization

FreeAndNil(TPresenter.RegisteredCanvasClasses);
FreeAndNil(TPresenter.RegisteredControlClasses);
FreeAndNil(TPresenter.RegisteredSceneObjectClasses);

end.
