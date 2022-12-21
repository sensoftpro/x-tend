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

unit uPresenter;

interface

uses
  Classes, Generics.Collections, SysUtils, UITypes, Types,
  uModule, uSettings, uDefinition, uInteractor, uView, uConsts, uUIBuilder, uLayout, uIcon;

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

  TUIClassInfo = class
  private
    FName: string;
    FType: TUIItemType;
    FItemClass: TUIAreaClass;
  public
    constructor Create(const AItemType: TUIItemType;
      const AViewName: string; const AItemClass: TUIAreaClass);
  end;

  TControlClassInfo = class
  private
    FName: string;
    FType: TUIItemType;
    FControlClass: TNativeControlClass;
  public
    constructor Create(const AControlType: TUIItemType; const AStyleName: string;
      const AControlClass: TNativeControlClass);
  end;

  TProgressInfo = class
  private
    FProgress: Integer;
    FInfo: string;
    FStartTime: TDateTime;
  public
    Domain: TObject;

    constructor Create;

    procedure SetProgress(const AProgress: Integer; const AInfo: string = '');

    property Progress: Integer read FProgress;
    property Info: string read FInfo;
  end;

  TStartedEvent = reference to procedure;

  TPresenter = class(TBaseModule)
  private
    class var RegisteredUIClasses: TObjectDictionary<string, TObjectDictionary<string, TUIClassInfo>>;
    class var RegisteredPages: TObjectDictionary<string, TDictionary<string, TClass>>;
    class var RegisteredControlClasses: TObjectDictionary<string, TObjectDictionary<string, TControlClassInfo>>;
  public
    class procedure RegisterUIClass(const APresenterName: string; const AItemType: TUIItemType;
      const AViewName: string; const AElementClass: TUIAreaClass);
    class procedure RegisterPage(const APresenterName: string; const APageName: string; const APageClass: TClass);
    class procedure RegisterControlClass(const APresenterName: string; const AControlType: TUIItemType;
      const AStyleName: string; const AControlClass: TNativeControlClass);
    class function GetUIClass(const APresenterName: string; const AItemType: TUIItemType;
      const AViewName: string): TUIAreaClass;
    class function GetPageClass(const APresenterName: string; const APageName: string): TClass;
    class function GetControlClass(const APresenterName: string; const AControlType: TUIItemType;
      const AStyleName: string): TNativeControlClass;
  private
    FNativeControlClass: TNativeControlClass;
    FOnAppStarted: TStartedEvent;
  protected
    procedure OnPCCanClose(Sender: TObject; var ACanClose: Boolean);
    procedure OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
    procedure DoChildFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoFloatFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoMainFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoOnFormShow(Sender: TObject);
  protected
    FName: string;
    FCursorType: TCursorType;
    FProgressInfo: TProgressInfo;
    FInteractors: TObjectList<TInteractor>;
    FCommonIcons: TIcons;
    FNeedShowSplash: Boolean;

    procedure DoOnAppStarted;

    procedure DoRun(const AParameter: string); virtual;
    procedure DoUnfreeze; virtual;
    procedure DoStop; virtual;

      function AreaFromSender(const ASender: TObject): TUIArea; virtual;
      function DoLogin(const ADomain: TObject): TInteractor; virtual;
      function ShowLoginForm(const AAppTitle: string; var ALoginName, APass: string): Boolean; virtual;
      procedure DoLogout(const AInteractor: TInteractor); virtual;

    function GetNativeControlClass: TNativeControlClass; virtual; abstract;
    procedure DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType); virtual; abstract;
    function DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult; virtual; abstract;
    procedure DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False); virtual;
    function DoSelectFile(var AFileName: string; const ADirectory: string = ''): Boolean; virtual;
    function DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt, ADefaultDir: string): Boolean; virtual;
    function DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean; virtual;
    procedure DoSetCursor(const ACursorType: TCursorType); virtual;
    procedure DoCloseAllPages(const AInteractor: TInteractor); virtual; abstract;

    function DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject; virtual; abstract;

    procedure OnDomainLoadProgress(const AProgress: Integer; const AInfo: string);
    procedure OnDomainError(const ACaption, AText: string);

    procedure StoreUILayout(const AInteractor: TInteractor); virtual;
    procedure RestoreUILayout(const AInteractor: TInteractor); virtual;

    procedure DoEnumerateControls(const ALayout: TLayout; const AControl: TObject); virtual;

    function ActiveInteractor: TInteractor;
  public
    constructor Create(const AName: string; const ASettings: TSettings); virtual;
    destructor Destroy; override;
    procedure Run(const AParameter: string = '');
    procedure Unfreeze;
    procedure Stop;

    function Login(const ADomain: TObject): TInteractor;
    procedure Logout(const AInteractor: TInteractor);

    function CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView; const AAreaName: string;
      const ACaption: string = ''; const AOnClose: TProc = nil): TUIArea; virtual;
    function ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult; virtual;
    function CreateFilledArea(const AParent: TUIArea; const AView: TView; const ALayout: TLayout; const AId: string;
      const AIsService: Boolean = False; const AControl: TObject = nil; const AParams: string = ''): TUIArea; virtual;

    // Layouts operations
    function CreateFieldArea(const AParentArea: TUIArea; const ALayout: TLayout;
      const AView: TView; const AStyleName, AParams: string): TUIArea;
    function CreateActionArea(const AParentArea: TUIArea; const ALayout: TLayout;
      const AView: TView; const AStyleName, AParams: string): TUIArea;
    function CreateCollectionArea(const AParentArea: TUIArea; const ALayout: TLayout;
      const AView: TView; const AStyleName, AParams: string): TUIArea;
    function CreateNavigationArea(const AParentArea: TUIArea; const ALayout: TLayout;
      const AView: TView; const AStyleName, AParams: string): TUIArea;
    function CreateNativeControl(const AArea: TUIArea; const ALayout: TLayout;
      const AView: TView; const AControlType: TUIItemType; const AStyleName, AParams: string): TNativeControl;
    procedure ShowLayout(const AInteractor: TInteractor; const ATargetAreaName, ALayoutName: string);
    procedure EnumerateControls(const ALayout: TLayout; const AControl: TObject);

    function ShowPage(const AInteractor: TInteractor; const APageType: string; const AParams: TObject = nil): TDialogResult; virtual;
    procedure ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement); virtual;
    procedure CloseAllPages(const AInteractor: TInteractor);

    function CreateArea(const AParent: TUIArea; const ALayout: TLayout; const AView: TView;
      const AParams: string = ''; const AOnClose: TProc = nil): TUIArea; virtual; abstract;
    function CreateTempControl: TObject; virtual; abstract;
    function CreatePopupArea(const AParent: TUIArea; const ALayout: TLayout): TUIArea; virtual; abstract;

    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); virtual; abstract;
    function SetCursor(const ACursorType: TCursorType): TCursorType;
    function GetWidthByType(const AWidth: Integer; const AFieldDef: TFieldDef): Integer;

    procedure ShowMessage(const ACaption, AText: string; const AMessageType: TMessageType = msNone);
    function ShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult;
    function ShowYesNoDialog(const ACaption, AText: string; const AWithCancel: Boolean = False): TDialogResult;
    function ShowOkCancelDialog(const ACaption, AText: string): TDialogResult;
    procedure OpenFile(const AFileName: string; const ADefaultApp: string = ''; const Await: Boolean = False);
    function ShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt, ADefaultDir: string): Boolean;
    function ShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean;

    function CreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject;
    property OnAppStarted: TStartedEvent read FOnAppStarted write FOnAppStarted;
    property Name: string read FName;
    property NativeControlClass: TNativeControlClass read FNativeControlClass;
  end;

  TPresenterClass = class of TPresenter;

implementation

uses
  Math, IOUtils, uUtils, uPlatform, uConfiguration, uDomain, uSession,
  uEntity, uEntityList, uChangeManager;

type
  TLoginedProc = procedure(const AInteractor: TInteractor) of object;
  TBeforeUIClosingFunc = function(const AInteractor: TInteractor): Boolean of object;

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
  Result := nil;
end;

procedure TPresenter.ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement);
begin
end;

procedure TPresenter.CloseAllPages(const AInteractor: TInteractor);
var
  i: Integer;
  vChildArea: TUIArea;
begin
  if AInteractor.Layout = 'mdi' then
  begin
    DoCloseAllPages(AInteractor);
    Exit;
  end;

  if not Assigned(AInteractor.UIBuilder.PagedArea) then
    Exit;

  for i := AInteractor.UIBuilder.PagedArea.Count - 1 downto 0 do
  begin
    vChildArea := AInteractor.UIBuilder.PagedArea.Areas[i];
    AInteractor.UIBuilder.PagedArea.RemoveArea(vChildArea);
  end;
end;

constructor TPresenter.Create(const AName: string; const ASettings: TSettings);
var
  vStyleName: string;
begin
  inherited Create;

  FName := AName;
  FNativeControlClass := GetNativeControlClass;
  FCursorType := crtDefault;
  vStyleName := ASettings.GetValue('Core', 'Style', 'default');
  FCommonIcons := TIcons.Create;
  FCommonIcons.Load(TPath.Combine(GetPlatformDir, 'res' + PathDelim + 'Styles' + PathDelim + vStyleName));
  FInteractors := TObjectList<TInteractor>.Create;
  FProgressInfo := TProgressInfo.Create;

  if ASettings.KeyExists(AName, 'ShowSplash') then
    FNeedShowSplash := StrToBoolDef(ASettings.GetValue(AName, 'ShowSplash'), False)
  else
    FNeedShowSplash := StrToBoolDef(ASettings.GetValue('Core', 'ShowSplash'), False);
end;

function TPresenter.CreateActionArea(const AParentArea: TUIArea; const ALayout: TLayout; const AView: TView;
  const AStyleName, AParams: string): TUIArea;
var
  vActionAreaClass: TUIAreaClass;
begin
  vActionAreaClass := TUIAreaClass(GetUIClass(FName, uiAction, AStyleName));

  Result := vActionAreaClass.Create(AParentArea, AView, ALayout, 'Action', False, nil, AParams);
end;

function TPresenter.CreateCollectionArea(const AParentArea: TUIArea; const ALayout: TLayout; const AView: TView; const AStyleName,
  AParams: string): TUIArea;
var
  //vParams, vViewName: string;
  vCollectionAreaClass: TUIAreaClass;
begin
  //vViewName := GetUrlCommand(AStyleName, AStyleName);
  //vParams := ExtractUrlParams(AStyleName);

  vCollectionAreaClass := TUIAreaClass(GetUIClass(FName, uiCollection, AStyleName));

  Result := vCollectionAreaClass.Create(AParentArea, AView, ALayout, 'List', False, nil, AParams);
end;

function TPresenter.CreateFieldArea(const AParentArea: TUIArea; const ALayout: TLayout;
  const AView: TView; const AStyleName, AParams: string): TUIArea;
var
  vParams, vViewName: string;
  vControlType: TUIItemType;
  vFieldAreaClass: TUIAreaClass;
begin
  if Assigned(ALayout) and (ALayout.Kind = lkPages) then
    vViewName := 'pages'
  else
    vViewName := GetUrlCommand(AStyleName, AStyleName);
  vParams := ExtractUrlParams(AStyleName);

  if AView.DefinitionKind = dkEntity then
    vControlType := uiEntityEdit
  else
    vControlType := ItemTypeByFieldType(TFieldDef(AView.Definition).Kind);
  vFieldAreaClass := TUIAreaClass(GetUIClass(FName, vControlType, vViewName));

  if vParams = '' then
    vParams := AParams
  else
    vParams := vParams + '&' + AParams;

  Result := vFieldAreaClass.Create(AParentArea, AView, ALayout, '', False, nil, vParams);
end;

function TPresenter.CreateFilledArea(const AParent: TUIArea; const AView: TView; const ALayout: TLayout; const AId: string;
  const AIsService: Boolean = False; const AControl: TObject = nil; const AParams: string = ''): TUIArea;
begin
  Result := TUIArea.Create(AParent, AView, ALayout, AId, AIsService, nil, AParams);
  Result.SetControl(AControl);
end;

function TPresenter.CreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject;
begin
  Result := DoCreateImages(AInteractor, ASize);
end;

function TPresenter.CreateNativeControl(const AArea: TUIArea; const ALayout: TLayout; const AView: TView;
  const AControlType: TUIItemType; const AStyleName, AParams: string): TNativeControl;
var
  vParams: string;
  vStyleName: string;
  vControlClass: TNativeControlClass;
begin
  if ALayout.Kind = lkPages then
    vStyleName := 'pages'
  else
    vStyleName := GetUrlCommand(AStyleName, AStyleName);
  vParams := ExtractUrlParams(AStyleName);

  vControlClass := TNativeControlClass(GetControlClass(FName, AControlType, vStyleName));
  if not Assigned(vControlClass) then
    Assert(Assigned(vControlClass), 'Control class not found for type: "' + cControlTypeNames[AControlType] +
      '", style name: "' + vStyleName + '" in UI: ' + ClassName);

  if vParams = '' then
    vParams := AParams
  else
    vParams := vParams + '&' + AParams;

  //Result := vControlClass.Create(AArea, ALayout, AView, vParams);
  Result := nil;
end;

function TPresenter.CreateNavigationArea(const AParentArea: TUIArea; const ALayout: TLayout; const AView: TView;
  const AStyleName, AParams: string): TUIArea;
var
  vNavigationAreaClass: TUIAreaClass;
begin
  vNavigationAreaClass := TUIAreaClass(GetUIClass(FName, uiNavigation, AStyleName));
  if Assigned(vNavigationAreaClass) then
    Result := vNavigationAreaClass.Create(AParentArea, AView, ALayout, 'Navigation', False, nil, AParams)
  else
    Result := nil;

  Assert(Assigned(Result), 'Не удалось создать навигационную область ' + AStyleName);
end;

function TPresenter.CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView;
  const AAreaName: string; const ACaption: string = ''; const AOnClose: TProc = nil): TUIArea;
begin
  Result := nil;
end;

destructor TPresenter.Destroy;
begin
  FreeAndNil(FProgressInfo);
  FreeAndNil(FInteractors);
  FreeAndNil(FCommonIcons);
  inherited Destroy;
end;

procedure TPresenter.DoChildFormClose(Sender: TObject; var Action: TCloseAction);
var
  vArea: TUIArea;
  vInteractor: TInteractor;
  vView, vCloseView: TView;
  vHolder: TChangeHolder;
  vRes: TDialogResult;
  vChildArea: TUIArea;
  i: Integer;
  vCanBeClosed: Boolean;
  vCloseProc: TProc;

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

  procedure DoValidation;
  var
    vEmptyRequiredFields: string;
    vEntity: TEntity;
    vSimilar: TEntity;
  begin
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
      Action := TCloseAction.caNone;
    end;

    if Action <> TCloseAction.caNone then
    begin
      vSimilar := vEntity.FindSimilar(vInteractor.Session);
      if Assigned(vSimilar) then
      begin
        vInteractor.ShowMessage(vInteractor.Translate('msgRecordExists', 'Такая запись уже существует') +
          ' [' + vSimilar['Name'] + ']');
        Action := TCloseAction.caNone;
      end;
    end;
  end;
begin
  vArea := AreaFromSender(Sender);
  vHolder := TChangeHolder(vArea.ThisHolder);
  vView := vArea.View;
  vInteractor := TInteractor(vArea.Interactor);

  for i := 0 to vArea.Count - 1 do
  begin
    vChildArea := vArea[i];
    if vChildArea.NativeControl.IsForm then
    begin
      vInteractor.ShowMessage('Невозможно закрыть окно, так как есть другие программные окна, зависящие от него', msWarning);
      Action := TCloseAction.caNone;
      Exit;
    end;
  end;

  vCloseProc := nil;
  vCloseView := vView.BuildView('Close');
  vCloseView.AddListener(vArea);
  try
    //vCanBeClosed := not (Assigned(vCloseView) and (TCheckActionFlagsFunc(TConfiguration(vInteractor.Configuration).
    //  CheckActionFlagsFunc)(vCloseView) <> vsFullAccess));
    vCanBeClosed := True;

    if vCanBeClosed then
    begin
      vCloseProc := vArea.OnClose;
      if vArea.NativeControl.ModalResult = mrOk then
        DoValidation
      else
      begin
        if Assigned(vHolder) and vHolder.IsVisibleModified then
        begin
          vRes := TPresenter(vInteractor.Presenter).ShowYesNoDialog('Подтвердите', //vForm.Caption,
            vInteractor.Translate('msgPromtSaveChanges', 'Сохранить изменения перед закрытием формы?'), True);
          if vRes = drYes then
          begin
            vArea.NativeControl.ModalResult := mrOk;
            DoValidation;
          end
          else if vRes = drNo then
            vArea.NativeControl.ModalResult := mrCancel
          else if vRes = drCancel then
            vArea.NativeControl.ModalResult := mrNone;
        end;
      end;
    end
    else
      Action := TCloseAction.caNone;
  finally
    vCloseView.RemoveListener(vArea);
  end;

  if Assigned(vCloseProc) then
    vCloseProc;
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
      CheckActionFlagsFunc)(vCloseView) <> vsFullAccess));

    if vCanBeClosed then
    begin
      if Assigned(vArea.OnClose) then
        vArea.OnClose();

      // Возможно, нужно сбросить CurrentArea у UIBuilder-а в vArea.Parent

      vArea.SetHolder(nil);
      if Assigned(vArea.Parent) then
        vArea.Parent.RemoveArea(vArea);

      vInteractor.PrintHierarchy;
      //Action := caFree;
    end
    else
      Action := TCloseAction.caNone;
  finally
    vCloseView.RemoveListener(vArea);
  end;
end;

function TPresenter.DoLogin(const ADomain: TObject): TInteractor;
var
  vDomain: TDomain absolute ADomain;
  vLogin, vPassword: string;
  //vRFID: string;
  vSession: TUserSession;
  vResult: Boolean;
  vMainFormName: string;
  vLayout: string;
  vUsers: TEntityList;
begin
  Result := nil;

  vLayout := vDomain.Settings.GetValue('Core', 'Layout', '');

  SetApplicationUI(vDomain.AppTitle, vDomain.Configuration.IconFileName);

  vUsers := TEntityList.Create(vDomain, vDomain.DomainSession);
  vDomain.GetEntityList(vDomain.DomainSession, vDomain.Configuration['SysUsers'], vUsers, '');
  try
    if vUsers.Count > 0 then
    begin
      if (vUsers.Count = 1) and (vDomain.Settings.GetValue('Core', 'AutoLogin', '') = '1') then
        vSession := vDomain.Sessions.AddSession(vUsers[0])
      else begin
        vLogin := vDomain.UserSettings.GetValue('Core', 'LastLogin', '');
        vPassword := '';
        vSession := nil;
        repeat
          vResult := ShowLoginForm(vDomain.AppTitle, vLogin, vPassword{, vRFID});
          if not vResult then
            Break;

          vSession := vDomain.Login(vLogin, vPassword);
          if not Assigned(vSession) and (ShowYesNoDialog(_Platform.Translate('cptError', 'Ошибка'),
            _Platform.Translate('txtWrongLoginOrPassword', 'Введены некорректные имя пользователя или пароль')
            + #13#10 + _Platform.Translate('txtPromptTryAgain', 'Попробовать ещё раз?')) <> drYes) then Break;
        until Assigned(vSession);

        if not Assigned(vSession) then
        begin
          SetApplicationUI(cPlatformTitle); //todo: вернуть иконку по умолчанию
          Exit;
        end;

        vDomain.UserSettings.SetValue('Core', 'LastLogin', vLogin);
      end;
    end
    else
      vSession := vDomain.DomainSession;
  finally
    FreeAndNil(vUsers);
  end;

  // Создаем корневую форму и интерактор для нее
  Result := TInteractor.Create(Self, vSession);

  vMainFormName := vDomain.Settings.GetValue('Core', 'MainForm', '');
  if Trim(vMainFormName) = '' then
    vMainFormName := 'MainForm';
  Result.UIBuilder.Navigate(nil, '', vMainFormName, '', vSession.NullHolder);

  TLoginedProc(vDomain.Configuration.LoginedProc)(Result);
end;

procedure TPresenter.DoLogout(const AInteractor: TInteractor);
begin
end;

procedure TPresenter.DoMainFormClose(Sender: TObject; var Action: TCloseAction);
var
  vResult: Boolean;
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(AreaFromSender(Sender).Interactor);
  if Assigned(vInteractor) then
    vResult := TBeforeUIClosingFunc(TConfiguration(vInteractor.Configuration).BeforeUIClosingFunc)(vInteractor)
  else
    vResult := True;

  if vResult then
  begin
    StoreUILayout(vInteractor);
    if Assigned(vInteractor) then
      Logout(vInteractor);
    Action := TCloseAction.caFree;
  end
  else
    Action := TCloseAction.caNone;
end;

procedure TPresenter.DoOnAppStarted;
var
  i: Integer;
begin
  for i := 0 to FInteractors.Count - 1 do
    RestoreUILayout(FInteractors[i]);
  if Assigned(FOnAppStarted) then
    FOnAppStarted;
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

procedure TPresenter.DoRun;
begin
end;

function TPresenter.DoSelectFile(var AFileName: string; const ADirectory: string): Boolean;
begin
  Result := False;
end;

procedure TPresenter.DoSetCursor(const ACursorType: TCursorType);
begin
end;

function TPresenter.DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt,
  ADefaultDir: string): Boolean;
begin
  Result := False;
end;

function TPresenter.DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean;
begin
  Result := False;
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

class function TPresenter.GetControlClass(const APresenterName: string; const AControlType: TUIItemType;
  const AStyleName: string): TNativeControlClass;
var
  vTypeName, vStyleName: string;
  vClassesList: TObjectDictionary<string, TControlClassInfo>;
  vClassInfo: TControlClassInfo;
begin
  Result := nil;
  if not RegisteredControlClasses.TryGetValue(APresenterName, vClassesList) then
    Exit;

  vTypeName := cControlTypeNames[AControlType];
  vStyleName := AnsiLowerCase(AStyleName);

  if not vClassesList.TryGetValue(vTypeName + vStyleName, vClassInfo) and (vStyleName <> '') then
    if not vClassesList.TryGetValue(vTypeName, vClassInfo) then
      vClassInfo := nil;

  if Assigned(vClassInfo) then
    Result := vClassInfo.FControlClass
  else
    Assert(False, 'Control class not found for type: "' + vTypeName +
      '", style Name: "' + vStyleName + '" in UI: ' + ClassName);
end;

class function TPresenter.GetPageClass(const APresenterName, APageName: string): TClass;
var
  vClassesList: TDictionary<string, TClass>;
begin
  Result := nil;
  if not RegisteredPages.TryGetValue(APresenterName, vClassesList) then
    Exit;

  if not vClassesList.TryGetValue(APageName, Result) then
    Assert(False, 'Page is not found for type: "' + APageName +
      '" in presenter: "' + APresenterName + '" classified as: ' + ClassName);
end;

class function TPresenter.GetUIClass(const APresenterName: string; const AItemType: TUIItemType; const AViewName: string): TUIAreaClass;
var
  vTypeName, vViewName: string;
  vClassesList: TObjectDictionary<string, TUIClassInfo>;
  vClassInfo: TUIClassInfo;
begin
  Result := nil;
  if not RegisteredUIClasses.TryGetValue(APresenterName, vClassesList) then
    Exit;

  vTypeName := cControlTypeNames[AItemType];
  vViewName := AnsiLowerCase(AViewName);

  if not vClassesList.TryGetValue(vTypeName + vViewName, vClassInfo) and (vViewName <> '') then
    if not vClassesList.TryGetValue(vTypeName, vClassInfo) then
      vClassInfo := nil;

  if Assigned(vClassInfo) then
    Result := vClassInfo.FItemClass
  else
    Assert(False, 'UI Class not found for type: "' + vTypeName +
    '", View Name: "' + vViewName + '" in UI: ' + ClassName);
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

function TPresenter.Login(const ADomain: TObject): TInteractor;
begin
  Result := DoLogin(ADomain);
  if Assigned(Result) then
    FInteractors.Add(Result);
end;

procedure TPresenter.Logout(const AInteractor: TInteractor);
begin
  DoLogout(AInteractor);
  FInteractors.Remove(AInteractor);
end;

procedure TPresenter.OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
var
  vArea: TUIArea;
  vView: TView;
  vCloseView: TView;
  vInteractor: TInteractor;
  vCanBeClosed: Boolean;
  vCloseProc: TProc;
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
      CheckActionFlagsFunc)(vCloseView) <> vsFullAccess));

    if (vView.DomainObject is TEntity) and TEntity(vView.DomainObject).InstanceOf('_FormLayout') then
      vDelEntity := TEntity(vView.DomainObject);

    if vCanBeClosed then
    begin
      vCloseProc := vArea.OnClose;
      vArea.SetControl(nil);
      vArea.UIBuilder.RootArea.RemoveArea(vArea); // the form will be destroyed here
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
    vCloseProc;
end;

procedure TPresenter.OnDomainError(const ACaption, AText: string);
begin
  ShowMessage(ACaption, AText, msError);
end;

procedure TPresenter.OnDomainLoadProgress(const AProgress: Integer; const AInfo: string);
begin
  if FNeedShowSplash then
  begin
    FProgressInfo.SetProgress(AProgress, AInfo);
    ShowPage(nil, 'splash', FProgressInfo);
  end;
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

  vPCArea.UIBuilder.LastArea := nil;

  vPCArea.RemoveArea(vTabArea);

  vPCArea.UIBuilder.PrintHierarchy;
end;

procedure TPresenter.OpenFile(const AFileName: string; const ADefaultApp: string = ''; const Await: Boolean = False);
begin
  DoOpenFile(AFileName, ADefaultApp, Await);
end;

class procedure TPresenter.RegisterControlClass(const APresenterName: string; const AControlType: TUIItemType;
  const AStyleName: string; const AControlClass: TNativeControlClass);
var
  vTypeName, vStyleName: string;
  vClassesList: TObjectDictionary<string, TControlClassInfo>;
  vClassInfo: TControlClassInfo;
begin
  vTypeName := cControlTypeNames[AControlType];
  vStyleName := AnsiLowerCase(AStyleName);
  if not RegisteredControlClasses.TryGetValue(APresenterName, vClassesList) then
  begin
    vClassesList := TObjectDictionary<string, TControlClassInfo>.Create([doOwnsValues]);
    RegisteredControlClasses.Add(APresenterName, vClassesList);
  end
  else
    Assert(not vClassesList.TryGetValue(vTypeName + vStyleName, vClassInfo),
      'Control class already registered for type: "' + vTypeName + '", style name: "' + vStyleName + '"');

  vClassInfo := TControlClassInfo.Create(AControlType, vStyleName, AControlClass);
  vClassesList.Add(vTypeName + vStyleName, vClassInfo);
end;

class procedure TPresenter.RegisterPage(const APresenterName, APageName: string; const APageClass: TClass);
var
  vClassesList: TDictionary<string, TClass>;
begin
  if not RegisteredPages.TryGetValue(APresenterName, vClassesList) then
  begin
    vClassesList := TDictionary<string, TClass>.Create;
    RegisteredPages.Add(APresenterName, vClassesList);
  end
  else
    Assert(not vClassesList.ContainsKey(APageName), 'Page already registered for type: "' + APageName + '"');

  vClassesList.Add(APageName, APageClass);
end;

class procedure TPresenter.RegisterUIClass(const APresenterName: string; const AItemType: TUIItemType;
  const AViewName: string; const AElementClass: TUIAreaClass);
var
  vTypeName, vViewName: string;
  vClassesList: TObjectDictionary<string, TUIClassInfo>;
  vClassInfo: TUIClassInfo;
begin
  vTypeName := cControlTypeNames[AItemType];
  vViewName := AnsiLowerCase(AViewName);
  if not RegisteredUIClasses.TryGetValue(APresenterName, vClassesList) then
  begin
    vClassesList := TObjectDictionary<string, TUIClassInfo>.Create([doOwnsValues]);
    RegisteredUIClasses.Add(APresenterName, vClassesList);
  end
  else
    Assert(not vClassesList.TryGetValue(vTypeName + vViewName, vClassInfo),
      'UI Class already registered for type: "' + vTypeName + '", View Name: "' + vViewName + '"');

  vClassInfo := TUIClassInfo.Create(AItemType, vViewName, AElementClass);
  vClassesList.Add(vTypeName + vViewName, vClassInfo);
end;

procedure TPresenter.RestoreUILayout(const AInteractor: TInteractor);
begin
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

function TPresenter.ShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult;
begin
  Result := DoShowDialog(ACaption, AText, ADialogActions);
end;

procedure TPresenter.ShowLayout(const AInteractor: TInteractor; const ATargetAreaName, ALayoutName: string);
begin
  if Assigned(AInteractor) then
    AInteractor.UIBuilder.Navigate(nil, ATargetAreaName, ALayoutName, '', TUserSession(AInteractor.Session).NullHolder);
end;

function TPresenter.ShowLoginForm(const AAppTitle: string; var ALoginName, APass: string): Boolean;
begin
  Result := False;
end;

procedure TPresenter.ShowMessage(const ACaption, AText: string; const AMessageType: TMessageType = msNone);
begin
  DoShowMessage(ACaption, AText, AMessageType);
end;

function TPresenter.ShowOkCancelDialog(const ACaption, AText: string): TDialogResult;
begin
  Result := DoShowDialog(ACaption, AText, [drOk, drCancel]);
end;

function TPresenter.ShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt, ADefaultDir: string): Boolean;
begin
  Result := DoShowOpenDialog(AFileName, ATitle, AFilter, ADefaultExt, ADefaultDir);
end;

function TPresenter.ShowPage(const AInteractor: TInteractor; const APageType: string; const AParams: TObject = nil): TDialogResult;
begin
  Result := drNone;
end;

function TPresenter.ShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean;
begin
  Result := DoShowSaveDialog(AFileName, ATitle, AFilter, ADefaultExt);
end;

function TPresenter.ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult;
begin
  Result := drNone;
end;

function TPresenter.ShowYesNoDialog(const ACaption, AText: string; const AWithCancel: Boolean): TDialogResult;
var
  vDialogButtons: TDialogResultSet;
begin
  vDialogButtons := [drYes, drNo];
  if AWithCancel then
    Include(vDialogButtons, drCancel);
  Result := DoShowDialog(ACaption, AText, vDialogButtons);
end;

procedure TPresenter.Stop;
begin
  DoStop;
end;

procedure TPresenter.StoreUILayout(const AInteractor: TInteractor);
begin
end;

procedure TPresenter.Unfreeze;
begin
  DoUnfreeze;
end;

{ TUIClassInfo }

constructor TUIClassInfo.Create(const AItemType: TUIItemType; const AViewName: string;
  const AItemClass: TUIAreaClass);
begin
  FName := AViewName;
  FType := AItemType;
  FItemClass := AItemClass;
end;

{ TProgressInfo }

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
end;

{ TControlClassInfo }

constructor TControlClassInfo.Create(const AControlType: TUIItemType; const AStyleName: string;
  const AControlClass: TNativeControlClass);
begin
  FName := AStyleName;
  FType := AControlType;
  FControlClass := AControlClass;
end;

initialization

TPresenter.RegisteredUIClasses := TObjectDictionary<string, TObjectDictionary<string, TUIClassInfo>>.Create([doOwnsValues]);
TPresenter.RegisteredPages := TObjectDictionary<string, TDictionary<string, TClass>>.Create([doOwnsValues]);
TPresenter.RegisteredControlClasses := TObjectDictionary<string, TObjectDictionary<string, TControlClassInfo>>.Create([doOwnsValues]);

finalization

FreeAndNil(TPresenter.RegisteredUIClasses);
FreeAndNil(TPresenter.RegisteredPages);
FreeAndNil(TPresenter.RegisteredControlClasses);

end.
