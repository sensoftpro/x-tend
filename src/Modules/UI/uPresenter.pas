{---------------------------------------------------------------------------------
  X-Tend runtime

  Contributors:
    Vladimir Kustikov (kustikov@sensoft.pro)
    Sergey Arlamenkov (arlamenkov@sensoft.pro)

  You may retrieve the latest version of this file at the GitHub,
  located at https://github.com/sensoftpro/x-tend.git
 ---------------------------------------------------------------------------------
  MIT License

  Copyright © 2021 Sensoft

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
  Classes, Generics.Collections, uModule, uSettings, uInteractor, uView, uConsts, uUIBuilder, uIcon;

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

  TNavigateEvent = function(const AAccount: TObject; const AUrl: string): Boolean of object;

  TUIClassInfo = class
  private
    FName: string;
    FType: TUIItemType;
    FItemClass: TUIAreaClass;
  public
    constructor Create(const AItemType: TUIItemType;
      const AViewName: string; const AItemClass: TUIAreaClass);
  end;

  TProgressInfo = class
  private
    FProgress: Integer;
    FInfo: string;
    FStartTime: Cardinal;
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
  public
    class procedure RegisterUIClass(const APresenterName: string; const AItemType: TUIItemType;
      const AViewName: string; const AElementClass: TUIAreaClass);
    class procedure RegisterPage(const APresenterName: string; const APageName: string; const APageClass: TClass);
    class function GetUIClass(const APresenterName: string; const AItemType: TUIItemType;
      const AViewName: string): TUIAreaClass;
    class function GetPageClass(const APresenterName: string; const APageName: string): TClass;
  private
    FOnAppStarted: TStartedEvent;
  protected
    FName: string;
    FProgressInfo: TProgressInfo;
    FInteractors: TObjectList<TInteractor>;
    FCommonIcons: TIcons;

    procedure DoOnAppStarted;
    function ItemTypeByFieldType(const AFieldKind: TFieldKind): TUIItemType;

    procedure DoRun(const AParameter: string); virtual;
    procedure DoStop; virtual;

    function DoLogin(const ADomain: TObject): TInteractor; virtual;
    procedure DoLogout(const AInteractor: TInteractor); virtual;
    procedure DoAuthorize(const AAccount: TObject; const AUrl: string; const AWidth, AHeight: Integer;
      const AOnNavigated: TNavigateEvent); virtual;

    procedure DoShowMessage(const ACaption, AText: string; const AMessageType: TMessageType); virtual; abstract;
    function DoShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult; virtual; abstract;
    procedure DoOpenFile(const AFileName: string; const ADefaultApp: string; const Await: Boolean = False); virtual;
    function DoSelectFile(var AFileName: string; const ADirectory: string = ''): Boolean; virtual;
    function DoShowOpenDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt, ADefaultDir: string): Boolean; virtual;
    function DoShowSaveDialog(var AFileName: string; const ATitle, AFilter, ADefaultExt: string): Boolean; virtual;
    procedure DoCloseAllPages(const AInteractor: TInteractor); virtual; abstract;

    function DoCreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject; virtual; abstract;

    procedure OnDomainLoadProgress(const AProgress: Integer; const AInfo: string); virtual;
    procedure OnDomainError(const ACaption, AText: string); virtual;

    function ActiveInteractor: TInteractor;
  public
    constructor Create(const AName: string; const ASettings: TSettings); virtual;
    destructor Destroy; override;
    procedure Run(const AParameter: string = '');
    procedure Stop;

    function Login(const ADomain: TObject): TInteractor;
    procedure Logout(const AInteractor: TInteractor);

    procedure Authorize(const AAccount: TObject; const AUrl: string; const AWidth, AHeight: Integer;
      const AOnNavigated: TNavigateEvent);

    function CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView;
      const AAreaName: string; const ACallback: TNotifyEvent = nil): TUIArea; virtual;
    function ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult; virtual;
    procedure CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea); virtual;

    // Layouts operations
    function CreateFieldArea(const AParentArea: TUIArea; const ALayout: TObject;
      const AView: TView; const AStyleName, AParams: string): TUIArea;
    function CreateActionArea(const AParentArea: TUIArea; const ALayout: TObject;
      const AView: TView; const AStyleName, AParams: string): TUIArea;
    function CreateCollectionArea(const AParentArea: TUIArea; const ALayout: TObject;
      const AView: TView; const AStyleName, AParams: string): TUIArea;
    procedure ShowLayout(const AInteractor: TInteractor; const ATargetAreaName, ALayoutName: string);
    procedure EnumerateControls(const ALayout: TObject; const AControls: TList<TObject>);
    procedure SetLayoutCaption(const ALayout: TObject; const ACaption: string);
    function GetLayoutCaption(const ALayout: TObject): string;
    function GetLayoutKind(const ALayout: TObject): TLayoutKind;

    function ShowPage(const AInteractor: TInteractor; const APageType: string; const AParams: TObject = nil): TDialogResult; virtual;
    procedure ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement); virtual;
    procedure CloseAllPages(const AInteractor: TInteractor);

    function CreateLayoutArea(const ALayoutKind: TLayoutKind; const AParams: string = ''): TObject; virtual; abstract;
    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); virtual; abstract;

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
  end;

  TPresenterClass = class of TPresenter;

implementation

uses
  IOUtils, SysUtils, TypInfo, {>> Windows} Controls, StdCtrls, ExtCtrls, ComCtrls, Menus, {Windows <<}
  uDefinition, uUtils;

{ TPresenter }

function TPresenter.ActiveInteractor: TInteractor;
begin
  if FInteractors.Count > 0 then
    Result := FInteractors[0]
  else
    Result := nil;
end;

procedure TPresenter.ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement);
begin
end;

procedure TPresenter.Authorize(const AAccount: TObject; const AUrl: string;
  const AWidth, AHeight: Integer; const AOnNavigated: TNavigateEvent);
begin
  DoAuthorize(AAccount, AUrl, AWidth, AHeight, AOnNavigated);
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

procedure TPresenter.CloseUIArea(const AInteractor: TInteractor; const AOldArea, ANewArea: TUIArea);
begin
end;

constructor TPresenter.Create(const AName: string; const ASettings: TSettings);
var
  vStyleName: string;
begin
  inherited Create;

  FName := AName;
  vStyleName := ASettings.GetValue('Core', 'Style', 'default');
  FCommonIcons := TIcons.Create;
  FCommonIcons.Load(TPath.Combine(GetPlatformDir, 'res' + PathDelim + 'Styles' + PathDelim + vStyleName));
  FInteractors := TObjectList<TInteractor>.Create;
  FProgressInfo := TProgressInfo.Create;
end;

function TPresenter.CreateActionArea(const AParentArea: TUIArea; const ALayout: TObject; const AView: TView;
  const AStyleName, AParams: string): TUIArea;
var
  vParams, vViewName: string;
  vActionAreaClass: TUIAreaClass;
begin
  vViewName := GetUrlCommand(AStyleName, AStyleName);
  vParams := ExtractUrlParams(AStyleName);

  vActionAreaClass := TUIAreaClass(GetUIClass(FName, uiAction, vViewName));

  Result := vActionAreaClass.Create(AParentArea, AView, '', False, nil, ALayout, vParams);
end;

function TPresenter.CreateCollectionArea(const AParentArea: TUIArea; const ALayout: TObject; const AView: TView; const AStyleName,
  AParams: string): TUIArea;
var
  vParams, vViewName: string;
  vCollectionAreaClass: TUIAreaClass;
begin
  vViewName := GetUrlCommand(AStyleName, AStyleName);
  vParams := ExtractUrlParams(AStyleName);

  vCollectionAreaClass := TUIAreaClass(GetUIClass(FName, uiCollection, vViewName));

  Result := vCollectionAreaClass.Create(AParentArea, AView, 'List', False, nil, ALayout, vParams);
end;

function TPresenter.CreateFieldArea(const AParentArea: TUIArea; const ALayout: TObject;
  const AView: TView; const AStyleName, AParams: string): TUIArea;
var
  vParams, vViewName: string;
  vFieldAreaClass: TUIAreaClass;
begin
  vViewName := GetUrlCommand(AStyleName, AStyleName);
  vParams := ExtractUrlParams(AStyleName);

  if AView.DefinitionKind = dkEntity then
    vFieldAreaClass := TUIAreaClass(GetUIClass(FName, uiEntityEdit, vViewName))
  else if ALayout is TPageControl then
    vFieldAreaClass := TUIAreaClass(GetUIClass(FName, ItemTypeByFieldType(TFieldDef(AView.Definition).Kind), 'pages'))
  else
    vFieldAreaClass := TUIAreaClass(GetUIClass(FName, ItemTypeByFieldType(TFieldDef(AView.Definition).Kind), vViewName));

  if vParams = '' then
    vParams := AParams;

  Result := vFieldAreaClass.Create(AParentArea, AView, '', False, nil, ALayout, vParams);
end;

function TPresenter.CreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject;
begin
  Result := DoCreateImages(AInteractor, ASize);
end;

function TPresenter.CreateUIArea(const AInteractor: TInteractor; const AParent: TUIArea; const AView: TView;
  const AAreaName: string; const ACallback: TNotifyEvent = nil): TUIArea;
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

procedure TPresenter.DoAuthorize(const AAccount: TObject; const AUrl: string;
  const AWidth, AHeight: Integer; const AOnNavigated: TNavigateEvent);
begin
end;

function TPresenter.DoLogin(const ADomain: TObject): TInteractor;
begin
  Result := nil;
end;

procedure TPresenter.DoLogout(const AInteractor: TInteractor);
begin
end;

procedure TPresenter.DoOnAppStarted;
begin
  if Assigned(FOnAppStarted) then
    FOnAppStarted;
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

procedure TPresenter.EnumerateControls(const ALayout: TObject; const AControls: TList<TObject>);
var
  vParentControl: TWinControl;
  i: Integer;
begin
  if not (ALayout is TWinControl) then
    Exit;
  if TWinControl(ALayout).ControlCount <= 0 then
    Exit;

  vParentControl := TWinControl(ALayout);
  for i := 0 to vParentControl.ComponentCount - 1 do
    if vParentControl.Components[i] is TMenu then
      AControls.Add(vParentControl.Components[i]);

  for i := 0 to vParentControl.ControlCount - 1 do
    AControls.Add(vParentControl.Controls[i]);
end;

function TPresenter.GetLayoutCaption(const ALayout: TObject): string;
begin
  if ALayout is TPageControl then
    Result := TPageControl(ALayout).Hint
  else if ALayout is TMemo then
  begin
    TMemo(ALayout).WordWrap := False;
    TMemo(ALayout).WantReturns := False;
    Result := TMemo(ALayout).Lines.Text;
  end
  else
    Result := TPanel(ALayout).Caption;
end;

function TPresenter.GetLayoutKind(const ALayout: TObject): TLayoutKind;
begin
  if ALayout is TPanel then
    Result := lkPanel
  else if ALayout is TTabSheet then
    Result := lkPage
  else if ALayout is TPageControl then
    Result := lkPages
  else if ALayout is TMemo then
    Result := lkMemo
  else
    Result := lkFrame;
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

  vTypeName := GetEnumName(TypeInfo(TUIItemType), Integer(AItemType));
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

function TPresenter.ItemTypeByFieldType(const AFieldKind: TFieldKind): TUIItemType;
begin
  Result := uiTextEdit;
  case AFieldKind of
    fkInteger: Result := uiIntegerEdit;
    fkEnum: Result := uiEnumEdit;
    fkFlag: Result := uiFlagEdit;
    fkFloat: Result := uiFloatEdit;
    fkString: Result := uiTextEdit;
    fkDateTime: Result := uiDateEdit;
    fkBoolean: Result := uiBoolEdit;
    fkCurrency: Result := uiCurrencyEdit;
    fkObject: Result := uiEntityEdit;
    fkList: Result := uiListEdit;
    fkBlob: Result := uiBLOBEdit;
    fkComplex: Result := uiComplexEdit;
    fkColor: Result := uiColorEdit;
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

procedure TPresenter.OnDomainError(const ACaption, AText: string);
begin
end;

procedure TPresenter.OnDomainLoadProgress(const AProgress: Integer; const AInfo: string);
begin
end;

procedure TPresenter.OpenFile(const AFileName: string; const ADefaultApp: string = ''; const Await: Boolean = False);
begin
  DoOpenFile(AFileName, ADefaultApp, Await);
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
  vTypeName := GetEnumName(TypeInfo(TUIItemType), Integer(AItemType));
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

procedure TPresenter.Run(const AParameter: string = '');
begin
  DoRun(AParameter);
end;

procedure TPresenter.SetLayoutCaption(const ALayout: TObject; const ACaption: string);
begin
  TPanel(ALayout).Caption := ACaption;
end;

function TPresenter.ShowDialog(const ACaption, AText: string; const ADialogActions: TDialogResultSet): TDialogResult;
begin
  Result := DoShowDialog(ACaption, AText, ADialogActions);
end;

procedure TPresenter.ShowLayout(const AInteractor: TInteractor; const ATargetAreaName, ALayoutName: string);
begin
  if Assigned(AInteractor) then
    AInteractor.UIBuilder.Navigate(nil, ATargetAreaName, ALayoutName);
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
  FStartTime := TThread.GetTickCount;
end;

procedure TProgressInfo.SetProgress(const AProgress: Integer; const AInfo: string);
begin
  FProgress := AProgress;
  FInfo := '[' + FormatFloat('0.##', (TThread.GetTickCount - FStartTime) / 1000) + '] ' + AInfo + '...';
end;

initialization

TPresenter.RegisteredUIClasses := TObjectDictionary<string, TObjectDictionary<string, TUIClassInfo>>.Create([doOwnsValues]);
TPresenter.RegisteredPages := TObjectDictionary<string, TDictionary<string, TClass>>.Create([doOwnsValues]);

finalization

FreeAndNil(TPresenter.RegisteredUIClasses);
FreeAndNil(TPresenter.RegisteredPages);

end.
