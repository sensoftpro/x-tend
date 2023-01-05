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

  TImages = class
  private
    FSize: Integer;
    FItems: TList<TStream>;
    FIndices: TDictionary<Integer, Integer>;
    FPlaceholder: TStream;
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): TStream;
  public
    constructor Create(const ASize: Integer);
    destructor Destroy; override;

    procedure Add(const AIndex: Integer; const AStream: TStream);
    procedure FillWithPlaceholder(const APlaceholder: TStream);

    property Items[const AIndex: Integer]: TStream read GetItem; default;
    property Count: Integer read GetCount;
    property Indices: TDictionary<Integer, Integer> read FIndices;
    property Placeholder: TStream read FPlaceholder;
  end;

  TPresenter = class(TBaseModule)
  private
    class var RegisteredPages: TObjectDictionary<string, TDictionary<string, TClass>>;
    class var RegisteredControlClasses: TObjectDictionary<string, TObjectDictionary<string, TControlClassInfo>>;
  public
    class procedure RegisterPage(const APresenterName: string; const APageName: string; const APageClass: TClass);
    class procedure RegisterControlClass(const APresenterName: string; const AControlType: TUIItemType;
      const AStyleName: string; const AControlClass: TNativeControlClass);
    class function GetPageClass(const APresenterName: string; const APageName: string): TClass;
    class function GetControlClass(const APresenterName: string; const AControlType: TUIItemType;
      const AStyleName: string): TNativeControlClass;
  private
    FNativeControlClass: TNativeControlClass;

    function CreateItem(const AOwner, AArea: TUIArea; const ANavItem: TNavigationItem;
      const AView: TView): TNativeControl;
  protected
    procedure OnPCCanClose(Sender: TObject; var ACanClose: Boolean);
    procedure OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
    procedure DoChildFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoFloatFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoMainFormClose(Sender: TObject; var Action: TCloseAction);
    procedure DoOnFormShow(Sender: TObject);
    procedure DoChildFormKeyDown(Sender: TObject; const AShift: TShiftState; const AKey: Word; var AHandled: Boolean);
    procedure DoProcessShortCut(const AShift: TShiftState; const AKey: Word; var AHandled: Boolean);
  protected
    FName: string;
    FCursorType: TCursorType;
    FProgressInfo: TProgressInfo;
    FInteractors: TObjectList<TInteractor>;
    FCommonIcons: TIcons;
    FNeedShowSplash: Boolean;

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
    function CreateAreaContent(const AArea: TUIArea; const AView: TView;
      const ALayout: TLayout; const AParams: string = ''): TNativeControl; virtual;
    function CreateAreaContentItem(const AOwner, AArea: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TNativeControl; virtual;

    procedure CopyPopupMenuItems(const AParent: TUIArea; const AView: TView;
      const ASrcItem: TNavigationItem; const ADestArea: TUIArea);

    function GetImagePlaceholder(const ASize: Integer): TStream; virtual; abstract;
    function DoCreateImages(const AInteractor: TInteractor; const AImages: TImages;
      const ASize: Integer): TObject; virtual; abstract;

    procedure OnDomainLoadProgress(const AProgress: Integer; const AInfo: string);
    procedure OnDomainError(const ACaption, AText: string);

    procedure DoEnumerateControls(const ALayout: TLayout; const AControl: TObject); virtual;

    function ActiveInteractor: TInteractor;
    procedure SetApplicationUI(const AAppTitle: string; const AIconName: string = ''); virtual; abstract;
  public
    constructor Create(const AName: string; const ASettings: TSettings); virtual;
    destructor Destroy; override;
    procedure Run(const AParameter: string = '');
    procedure Unfreeze;
    procedure Stop;

    function Login(const ADomain: TObject): TInteractor;
    procedure Logout(const AInteractor: TInteractor);

    procedure EnumerateControls(const ALayout: TLayout; const AControl: TObject);

    function ShowUIArea(const AInteractor: TInteractor; const AAreaName: string; const AOptions: string; var AArea: TUIArea): TDialogResult; virtual;
    function ShowPage(const AInteractor: TInteractor; const APageType: string; const AParams: TObject = nil): TDialogResult; virtual;
    procedure ArrangePages(const AInteractor: TInteractor; const AArrangeKind: TWindowArrangement); virtual;
    procedure CloseAllPages(const AInteractor: TInteractor);

    function CreateNativeControl(const AArea: TUIArea; const AView: TView; const ALayout: TLayout;
      const AControlType: TUIItemType; const AStyleName, AParams: string): TNativeControl;
    function CreateArea(const AParent: TUIArea; const AView: TView; const ALayout: TLayout;
      const AParams: string = ''; const AOnClose: TProc = nil): TUIArea;

    function CreateTempControl: TObject; virtual; abstract;

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
    property Name: string read FName;
  end;

  TPresenterClass = class of TPresenter;

implementation

uses
  Math, IOUtils, uUtils, uPlatform, uConfiguration, uDomain, uSession,
  uEntity, uEntityList, uChangeManager;

type
  TLoginedProc = procedure(const AInteractor: TInteractor) of object;
  TBeforeUIClosingFunc = function(const AInteractor: TInteractor): Boolean of object;
  //TCrackedArea = class(TUIArea) end;

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
          vView := AParent.UIBuilder.RootView.BuildView(AView.Name + '/Selected/' + vAction.Name);
        end
        else
          vView := AParent.UIBuilder.RootView.BuildView(AView.Name + '/' + vAction.Name);

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
        vChildArea := CreateArea(AParent, AParent.UIBuilder.RootView, vChildItem);
        AParent.AddArea(vChildArea);
      end;

      FreeAndNil(vActions);

      vReports := TList<TRTFReport>.Create;
      vDefinition.GetAllReports(vReports);
      for vReport in vReports do
      begin
        vView := AParent.UIBuilder.RootView.BuildView(AView.Name + '/Selected/' + vReport.Name);

        if Assigned(vView) then
        begin
          if vView.DefinitionKind = dkAction then
          begin
            vChildItem := AParent.UIBuilder.Layouts.CreateSimpleLayout(lkAction);
            vChildItem.StyleName := 'action';
            vChildItem.Caption := AParent.GetTranslation(vReport);
            vChildItem.Hint := vChildItem.Caption;
            vChildItem.ImageID := 31;

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
        vChildArea := CreateArea(AParent, AParent.UIBuilder.RootView, vChildItem);
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
            vDefArea := CreateArea(vChildArea, AParent.UIBuilder.RootView, vChildItem);
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
      vChildArea := CreateArea(AParent, AParent.UIBuilder.RootView, vSrcItem);
      vChildArea.NativeControl.ViewState := vsDisabled;
      AParent.AddArea(vChildArea);
    end;
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

function TPresenter.CreateArea(const AParent: TUIArea; const AView: TView;
  const ALayout: TLayout; const AParams: string; const AOnClose: TProc): TUIArea;
var
  vDomain: TDomain;
  vInteractor: TInteractor;
  vStartPageName: string;
  vStartPageStr: string;
  vParams: TStrings;
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
        if Assigned(vParams) and (vStartPageName <> '') and FileExists(vDomain.Configuration.FindLayoutFile(vStartPageName, LAYOUT_DFM_EXT)) then
        begin
          vParams.Values['Layout'] := vStartPageName;
          vParams.Values['View'] := '';
        end;
      end;

      Result.AddParams(vParams);

      AParent.UIBuilder.PagedArea := Result;
    end;
  end;
end;

function TPresenter.CreateAreaContent(const AArea: TUIArea;
  const AView: TView; const ALayout: TLayout; const AParams: string): TNativeControl;
begin
  Result := GetNativeControlClass.Create(AArea, AParams);
  Result.CreateContent(nil);
end;

function TPresenter.CreateAreaContentItem(const AOwner, AArea: TUIArea; const ANavItem: TNavigationItem;
  const ACaption, AHint: string; const AImageIndex: Integer): TNativeControl;
begin
  Result := GetNativeControlClass.Create(AArea, '');
  Result.CreateContent(nil);
end;

function TPresenter.CreateImages(const AInteractor: TInteractor; const ASize: Integer): TObject;
var
  vConfiguration: TConfiguration;
  vImages: TImages;
  vPlaceholder: TStream;

  procedure AppendIconsToImages(const AIcons: TIcons; const AImages: TImages);
  var
    vIndex: Integer;
    vStream: TStream;
  begin
    for vIndex in AIcons.IconIndices do
    begin
      vStream := AIcons.IconByIndex(vIndex, ASize);
      AImages.Add(vIndex, vStream);
    end;
  end;
begin
  vImages := TImages.Create(ASize);
  try
    AppendIconsToImages(FCommonIcons, vImages);

    vConfiguration := TConfiguration(AInteractor.Configuration);
    AppendIconsToImages(vConfiguration.Icons, vImages);

    vPlaceholder := vImages.Placeholder;
    if not Assigned(vPlaceholder) then
      vPlaceholder := GetImagePlaceholder(ASize);

    if Assigned(vPlaceholder) then
      vImages.FillWithPlaceholder(vPlaceholder);

    Result := DoCreateImages(AInteractor, vImages, ASize);
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
  vImageID: Integer;
begin
  if AOwner.Layout.AreaKind <> akNavigation then
    Exit(nil);

  vCaption := ANavItem.Caption;
  vHint := ANavItem.Hint;
  vImageID := ANavItem.ImageID;

  if AView.DefinitionKind = dkDomain then
    Result := CreateAreaContentItem(AOwner, AArea, ANavItem, vCaption, vHint, AOwner.GetImageID(vImageID))
  else if AView.DefinitionKind in [dkAction, dkCollection] then
  begin
    vDefinition := TDefinition(AView.Definition);
    if vCaption = '' then
      vCaption := AOwner.GetTranslation(vDefinition);
    if vHint = '' then
      vHint := vCaption;
    if vImageID < 0 then
      vImageID := vDefinition._ImageID;

    Result := CreateAreaContentItem(AOwner, AArea, ANavItem, vCaption, vHint, AOwner.GetImageID(vImageID));
  end
  else if (AView.DefinitionKind in [dkEntity, dkObjectField]) and (AView.DomainObject is TEntity) then
  begin
    vEntity := AView.DomainObject as TEntity;
    if (vCaption = '') and Assigned(vEntity) then
      vCaption := SafeDisplayName(vEntity, 'NULL');
    if vHint = '' then
      vHint := vCaption;

    Result := CreateAreaContentItem(AOwner, AArea, ANavItem, vCaption, vHint, AOwner.GetImageID(vImageID));
  end
  else
    Result := nil;
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

  vControlClass := TNativeControlClass(GetControlClass(FName, vControlType, vStyleName));
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
    Randomize;
    ALayout.Kind := lkPanel;
    ALayout.Font.Size := 10;
    ALayout.Font.Color := $FF shl 24 + Random(256) shl 16 + Random(256) shl 8 + Random(256);
    ALayout.Font.Family := 'Tahoma';
    ALayout.Color := $FF shl 24 + Random(256) shl 16 + Random(256) shl 8 + Random(256);
    ALayout.ShowCaption := True;
    ALayout.Caption := ALayout.Caption;
    ALayout.BevelInner := lbkRaised;
    ALayout.BevelOuter := lbkLowered;

    Result := CreateAreaContent(AArea, AView, ALayout, vParams);
  end;
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
    vFormArea.UIBuilder.LastArea := nil;
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
    if Assigned(vInteractor) then
      Logout(vInteractor);
    Action := TCloseAction.caFree;
  end
  else
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
begin
  vActiveInteractor := ActiveInteractor;
  if Assigned(vActiveInteractor) and (ssCtrl in AShift) and (ssAlt in AShift) and (ssShift in AShift) then
  begin
    if AKey = vkE then
    begin
      AHandled := True;
      if not Assigned(vActiveInteractor.UIBuilder.ActiveArea) then
        Exit;
      vView := vActiveInteractor.UIBuilder.ActiveArea.View;
      if not (vView.DefinitionKind in [dkAction, dkListField, dkObjectField, dkSimpleField]) then
        Exit;
      if not TUserSession(vActiveInteractor.Session).IsAdmin then
        Exit;

      vView.ElevateAccess;
    end
    else if AKey = vkR then
    begin
      AHandled := True;
      TDomain(vActiveInteractor.Domain).ReloadChanges(TDomain(vActiveInteractor.Domain).DomainHolder);
    end;
  end;
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
    Result := nil;
  //else
  //  Assert(False, 'Control class not found for type: "' + vTypeName +
  //    '", style Name: "' + vStyleName + '" in UI: ' + ClassName);
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
      vArea.UnbindContent(True);
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

procedure TPresenter.Unfreeze;
begin
  DoUnfreeze;
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

{ TImages }

procedure TImages.Add(const AIndex: Integer; const AStream: TStream);
begin
  FIndices.Add(AIndex, FItems.Count);
  if AIndex = 0 then
    FPlaceholder := AStream;
  FItems.Add(AStream);
end;

constructor TImages.Create(const ASize: Integer);
begin
  inherited Create;
  FSize := ASize;
  FItems := TList<TStream>.Create;
  FIndices := TDictionary<Integer, Integer>.Create;
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

TPresenter.RegisteredPages := TObjectDictionary<string, TDictionary<string, TClass>>.Create([doOwnsValues]);
TPresenter.RegisteredControlClasses := TObjectDictionary<string, TObjectDictionary<string, TControlClassInfo>>.Create([doOwnsValues]);

finalization

FreeAndNil(TPresenter.RegisteredPages);
FreeAndNil(TPresenter.RegisteredControlClasses);

end.
