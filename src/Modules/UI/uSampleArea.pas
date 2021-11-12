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

unit uSampleArea;

interface

uses
  Windows, Classes, Generics.Collections, Controls, StdCtrls, ExtCtrls, Menus, UITypes, uConsts, uUIBuilder,
  uDefinition, uEntity, uView;

type
  TSampleArea = class(TUIArea)
  private
    FOriginLeft, FOriginTop: Integer;  // for correct order during alignment
    FIsForm: Boolean;

    procedure OnActionMenuSelected(Sender: TObject);
    procedure OnEnter(Sender: TObject);
    procedure OnExit(Sender: TObject);
    procedure OnExecuteAction(Sender: TObject);
    procedure OnOpenCollection(Sender: TObject);
  protected
    procedure DoClose(const AModalResult: Integer); override;
    function DoCreateChildArea(const ALayout: TObject; const AView: TView; const AParams: string = ''): TUIArea; override;
    function DoCreateChildAction(const ALayout: TObject; const AView: TView; const AParams: string = ''): TUIArea; override;
    function DoCreateChildList(const ALayout: TObject; const AView: TView; const AParams: string = ''): TUIArea; override;
    function DoCreateChildEditor(const ALayout: TObject; const AView: TView; const AParams: string = ''): TUIArea; override;
    function DoCreateChildEmptyArea(const AView: TView): TUIArea; override;
    function AreaFromSender(const ASender: TObject): TUIArea; override;
    procedure AppendServiceArea(const ALayoutName: string); override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure DoActivate(const AAreaState: string = ''); override;
  protected
    procedure PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer); override;

    function GetName: string; override; // Для отладки
    procedure SetControl(const AControl: TObject); override;
    procedure SetParent(const Value: TUIArea); override;
    procedure UnbindContent; override;
    procedure AssignFromLayout(const ALayout: TObject); override;
    procedure ArrangeChildAreas; override;
    procedure SaveLayoutToFile(const AFileName: string); override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure SetViewState(const AValue: TViewState); override;

    procedure RefillArea(const AKind: Word); virtual;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TObject = nil; const AParams: string = ''); override;
    destructor Destroy; override;
  end;

  TSampleFieldArea = class(TSampleArea)
  protected
    FFieldDef: TFieldDef;

    procedure OnChange(Sender: TObject);

    // Not implemented here
    procedure FillEditor; virtual;
    procedure DoCreateControl(const ALayout: TObject); virtual;
    procedure AfterParentChanged; virtual;
    procedure DoBeforeFreeControl; virtual;
    procedure DoDeinit; virtual;
    procedure DoOnChange; virtual;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); virtual;

    function GetName: string; override;
    procedure RefillArea(const AKind: Word); override;

    procedure SetFieldValue(const AValue: Variant);
    procedure SetFieldEntity(const AEntity: TEntity);
    procedure SetFieldStream(const AStream: TStream);
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TObject = nil; const AParams: string = ''); override;
    destructor Destroy; override;

    procedure Deinit;
  end;

implementation

uses
  Forms, Messages, Types, Graphics, Math, SysUtils, StrUtils, ComCtrls, Buttons,
  Generics.Defaults, cxGraphics, dxGDIPlusClasses, cxLabel, cxImage, cxEdit, cxTextEdit, cxPC, dxBar,
  cxLookAndFeels, cxButtons, cxScrollBox, cxControls, cxSplitter, dxActivityIndicator,

  uDomain, uPresenter, uConfiguration, uSession, uInteractor, uUtils, uEntityList, uDomainUtils;

type
  TCanChangeFieldFunc = function(const AView: TView; const AEntity: TEntity; const AFieldName: string): Boolean of object;
  TCrackedControl = class(TWinControl) end;

  TLayoutParam = class
    Name: string;
    Value: Variant;
    constructor Create(const AName: string);
  end;

  TUILayout = class
  private
    FParams: TList;
    FParent: TUILayout;
    FChildLayouts: TList;
    FName: string;
    FType: string;
    FLevel: Integer;
    procedure Clear;
    procedure FillLayoutParams(const ALayout: TUILayout; const AComponent: TComponent);
    procedure GenerateDFMText(const AText: TStrings; const ALevel: Integer);
    procedure GeneratePASText(const AText: TStrings);
    function FindChild(const AName: string): TUILayout;
    function ParamByName(const AName: string): TLayoutParam;
    function CreateChild: TUILayout;
    function GenerateUniqueName(const AComponent: TComponent): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Build(const ASampleArea: TSampleArea);
    procedure SaveToDFM(const AFileName: string);
  end;

const
  cServiceAreaHeight = 44;

{ TSampleArea }

procedure TSampleArea.AppendServiceArea(const ALayoutName: string);
var
  vPanel: TPanel;
  vArea: TSampleArea;
begin
  vPanel := TPanel.Create(nil);
  vPanel.BevelOuter := bvNone;
  vPanel.Height := cServiceAreaHeight;
  vPanel.Align := alBottom;
  vArea := TSampleArea.Create(Self, FView, '', True, vPanel);
  AddArea(vArea);
  FUIBuilder.ApplyLayout(vArea, FView, ALayoutName);
end;

function TSampleArea.AreaFromSender(const ASender: TObject): TUIArea;
begin
  Result := nil;
  // Получить область из ASender
end;

procedure TSampleArea.ArrangeChildAreas;
begin
  // AUTO GENERATED LAYOUT
  // Расположить сгенерированные области внутри себя
end;

procedure TSampleArea.AssignFromLayout(const ALayout: TObject);
var
  vPanel: TPanel absolute ALayout;
begin
  if FIsForm then
  begin
    // Применяем настройки для форм
    if ALayout is TFrame then
      // Забрать настройки из фрейма
    else begin
      // Настроить по умолчанию
    end;
  end
  else if ALayout is TFrame then
  begin
  end
  else if (ALayout is TPanel) or (ALayout is TMemo) then
  begin
    PlaceIntoBounds(vPanel.Left, vPanel.Top, vPanel.Width, vPanel.Height);
  end;
end;

procedure TSampleArea.BeginUpdate;
begin
  // Заблокировать перерисовку области
end;

constructor TSampleArea.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
  const AControl: TObject = nil; const ALayout: TObject = nil; const AParams: string = '');
begin
  inherited Create(AParent, AView, AId, AIsService, AControl, ALayout, AParams);
end;

procedure TSampleArea.DoActivate(const AAreaState: string = '');
begin
  // Отобразить скрытый Tab, или вывести на передний план форму
end;

procedure TSampleArea.DoClose(const AModalResult: Integer);
begin
  if FIsForm then
  begin
    // Корректно закрыть модальную или основную форму
  end;
end;

function TSampleArea.DoCreateChildAction(const ALayout: TObject; const AView: TView; const AParams: string = ''): TUIArea;
var
  vNewControl: TObject;
  vActionDef: TDefinition;
  vDefinitions: TList<TDefinition>;
  i: Integer;
  vDefinition: TDefinition;
  vOnClickHandler: TNotifyEvent;
  vImageID: Integer;
  vParams: TStrings;
  vImageSize: Integer;
  vComposition: string;
  vViewStyle: string;
begin
  if AView.DefinitionKind = dkCollection then
    vOnClickHandler := OnOpenCollection
  else
    vOnClickHandler := OnExecuteAction;

  vActionDef := TDefinition(AView.Definition);

  vParams := CreateDelimitedList(AParams, '&');
  try
    vImageSize := StrToIntDef(vParams.Values['ImageSize'], 16);
    vComposition := Trim(vParams.Values['Composition']);
    vViewStyle := Trim(vParams.Values['ViewStyle']);
  finally
    FreeAndNil(vParams);
  end;

  // Создать кнопки по стилю
  vNewControl := TObject.Create; // <= GetTranslation(vActionDef);

  if (vActionDef.Name = 'Add') and Assigned(AView.ParentDomainObject) and (AView.ParentDomainObject is TEntityList) then
  begin
    vDefinitions := TEntityList(AView.ParentDomainObject).ContentDefinitions;
    if vDefinitions.Count > 1 then
    begin
      // Создать меню для выбора вариантов, и заполнить его
      for i := 0 to vDefinitions.Count - 1 do
      begin
        vDefinition := TDefinition(vDefinitions[i]);
        //vMenuItem := TMenuItem.Create(nil);
        //vMenuItem.Caption := GetTranslation(vDefinition);
        //vMenuItem.ImageIndex := GetImageID(vDefinition._ImageID);
        //vMenuItem.Tag := Integer(vNewControl);
        //vMenuItem.OnClick := OnActionMenuSelected;
      end;
    end
    else begin
      //vNewControl.OnClick := vOnClickHandler;
    end;
  end
  else begin
    //vNewControl.OnClick := vOnClickHandler;
  end;

  Result := TSampleArea.Create(Self, AView, vActionDef.Name, False, vNewControl);
  if AParams <> '' then
    Result.AddParams(CreateDelimitedList(AParams, '&'));
end;

function TSampleArea.DoCreateChildArea(const ALayout: TObject; const AView: TView; const AParams: string = ''): TUIArea;
var
  vSourceLabel: TLabel absolute ALayout;
  vSourceImage: TImage absolute ALayout;
  vSourcePC: TPageControl absolute ALayout;
  vSourceTabSheet: TTabSheet absolute ALayout;
  vSourceToolBar: TToolBar absolute ALayout;
  vSourcePanel: TPanel absolute ALayout;
  vSourceBox: TScrollBox absolute ALayout;
  vSourceMemo: TMemo absolute ALayout;
  vDomain: TDomain;
  vNewControl: TObject;
  vPos: Integer;
  vCaption: string;
  vUIParams: string;
  vStartPageName: string;
  vStartPageStr: string;
  vParams: TStrings;
begin
  // Создать UI области и контролы
  Result := nil;
  if not Assigned(ALayout) then
    Exit;

  vDomain := TDomain(TInteractor(Interactor).Domain);

  if ALayout is TLabel then
  begin
    vNewControl := TObject.Create; // << vSourceLabel

    vCaption := vSourceLabel.Caption;
    vUIParams := '';
    vPos := Pos('@', vCaption);
    if vPos > 1 then
    begin
      vUIParams := vCaption;
      Delete(vUIParams, 1, vPos);
      vCaption := Copy(vCaption, 1, vPos - 1);

      // Обработка служебных надписей
      if (vCaption = '$') and (AView.DefinitionKind = dkCollection) and (vUIParams = 'Caption') then
        vCaption := GetTranslation(TDefinition(AView.Definition));
    end
    else
      vCaption := vSourceLabel.Caption; //FView.Interactor.Translate('@' + {FFullAreaName или FLayoutName +} '.' +
        //vSourceLabel.Name + '@Caption', vSourceLabel.Caption);

    // Установить Caption
    Result := TSampleArea.Create(Self, AView, '', False, vNewControl);
  end
  else if ALayout is TImage then
  begin
    vNewControl := TObject.Create; // << vSourceImage
    Result := TSampleArea.Create(Self, AView, '', False, vNewControl);
  end
  else if ALayout is TPageControl then
  begin
    vNewControl := TObject.Create; // << vSourcePC
    Result := TSampleArea.Create(Self, AView, '', False, vNewControl);
  end
  else if ALayout is TTabSheet then
  begin
    if (TInteractor(AView.Interactor).Layout = 'mdi') and (vSourceTabSheet.Tag = 11) then
    begin
      vNewControl := TObject.Create; // << vSourceTabSheet
      Result := TSampleArea.Create(Self, AView, vSourceTabSheet.Name, False, vNewControl);
    end
    else begin
      vNewControl := TObject.Create; // << vSourceTabSheet
      Result := TSampleArea.Create(Self, AView, vSourceTabSheet.Name, False, vNewControl);
    end;
  end
  else if ALayout is TToolBar then
  begin
    vNewControl := TObject.Create; // << vSourceTToolbar
    Result := TSampleArea.Create(Self, AView, '', False, vNewControl);

    // Подвязать картинки
    //vResolution := StrToIntDef(TDomain(TInteractor(Interactor).Domain).Settings.GetValue('Core',
    //  'CfgImagesResolution'), 32);
    //vToolBar.Images := TDragImageList(TInteractor(AView.Interactor).Images[vResolution]);
  end
  else if ALayout is TToolButton then
  else if ALayout is TMainMenu then
  begin
    vNewControl := TObject.Create; // << TMenu(ALayout)
    Result := TSampleArea.Create(Self, AView, 'Menu', False, vNewControl);

    //vMenu.Images := TDragImageList(TInteractor(Interactor).Images[16]);
    //CopyMenuItems(Result, TMenu(ALayout).Items, vMenu.Items);
  end
  else if ALayout is TPopupMenu then
  begin
    vNewControl := TObject.Create; // << TMenu(ALayout)
    Result := TSampleArea.Create(Self, AView, 'Popup', False, vNewControl);

    //vMenu.Images := TDragImageList(TInteractor(Interactor).Images[16]);
    //CopyMenuItems(Result, TMenu(ALayout).Items, vMenu.Items);
  end
  else if ALayout is TBitBtn then
  else if ALayout is TBevel then
  else if ALayout is TSplitter then
  else if ALayout is TPanel then
  begin
    if AParams <> '' then
      vParams := CreateDelimitedList(AParams, '&')
    else
      vParams := nil;

    if Assigned(vParams) and (vParams.Values['ViewType'] = 'Paged') then
    begin
      if TInteractor(FView.Interactor).Layout = 'mdi' then
      begin
        FUIBuilder.DefaultParams := AParams;
        FreeAndNil(vParams);
        Exit(nil);
      end;

      vNewControl := TObject.Create; // << vSourcePanel
      Result := TSampleArea.Create(Self, AView, Trim(vSourcePanel.Caption), False, vNewControl);

      // Здесь можно подкорректировать параметры
      if StrToBoolDef(vDomain.UserSettings.GetValue('Core', 'ShowStartPage'), True) then
      begin
        vStartPageStr := vDomain.Settings.GetValue('Core', 'StartPage', '');

        vStartPageName := GetUrlCommand(vStartPageStr);
        if (vStartPageName <> '') and FileExists(vDomain.Configuration.FindLayoutFile(vStartPageName, LAYOUT_DFM_EXT)) then
        begin
          vParams.Values['Layout'] := vStartPageName;
          vParams.Values['View'] := '';
          vParams.Values['Options'] := DecodeUrl(GetUrlParam(vStartPageStr, 'Options', ''));
        end;
      end;

      FUIBuilder.PagedArea := Result;
    end
    else
    begin
      vNewControl := TObject.Create; // << vSourcePanel
      Result := TSampleArea.Create(Self, AView, Trim(vSourcePanel.Caption), False, vNewControl);
    end;

    Result.AddParams(vParams);
  end
  else if ALayout is TScrollBox then
  begin
    vNewControl := TObject.Create; // << vSourceBox
    Result := TSampleArea.Create(Self, AView, '', False, vNewControl);
  end
  else if ALayout is TMemo then
  begin
    vNewControl := TObject.Create; // << vSourceMemo
    Result := TSampleArea.Create(Self, AView, Trim(vSourceMemo.Hint), False, vNewControl);
  end
  else
    Assert(False, 'Класс [' + ALayout.ClassName + '] не поддерживается для создания лэйаутов');
end;

function TSampleArea.DoCreateChildEditor(const ALayout: TObject; const AView: TView; const AParams: string): TUIArea;
var
  vStyleName: string;
begin
  vStyleName := GetUrlParam(AParams, 'view');

  if vStyleName = '' then
  begin
    if AView.DefinitionKind in [dkListField, dkObjectField, dkSimpleField, dkComplexField] then
      vStyleName := TFieldDef(AView.Definition).StyleName;
  end;

  Result := TPresenter(FUIBuilder.Presenter).CreateFieldArea(Self, ALayout, AView, vStyleName, AParams);
end;

function TSampleArea.DoCreateChildEmptyArea(const AView: TView): TUIArea;
var
  vNewControl: TObject;
begin
  vNewControl := TObject.Create; // << Создать область
  Result := TSampleArea.Create(Self, AView, 'empty', False, vNewControl);
end;

function TSampleArea.DoCreateChildList(const ALayout: TObject; const AView: TView; const AParams: string): TUIArea;
var
  vStyleName: string;
begin
  vStyleName := GetUrlParam(AParams, 'view');

  if vStyleName = '' then
  begin
    if AView.DefinitionKind in [dkListField, dkObjectField, dkSimpleField, dkComplexField] then
      vStyleName := TFieldDef(AView.Definition).StyleName;
  end;

  Result := TPresenter(FUIBuilder.Presenter).CreateCollectionArea(Self, ALayout, AView, vStyleName, AParams);
end;

procedure TSampleArea.EndUpdate;
begin
  // Разблокировать перерисовку области, выполнить её
end;

function TSampleArea.GetName: string;
begin
  Result := 'Unknown';
  // Сформировать описание для этой области и ее внутреннего содержимого
end;

procedure TSampleArea.OnActionMenuSelected(Sender: TObject);
var
  vArea: TSampleArea;
  vSelectedIndex: Integer;
begin
  vSelectedIndex := -1;
  vArea := nil;
  // Определить, какой пункт меню был выбран >> vSelectedIndex
  // Вычислить, к чему было привязано это меню >> vArea

  Assert(vSelectedIndex >= 0, 'Выбран неизвестный пункт меню');
  TEntity(vArea.View.DomainObject)._SetFieldValue(nil, 'SelectedIndex', vSelectedIndex);

  FUIBuilder.LastArea := vArea;
  vArea.ExecuteUIAction(vArea.View);
end;

procedure TSampleArea.OnEnter(Sender: TObject);
var
  vArea: TUIArea;
begin
  vArea := AreaFromSender(Sender);
  if Assigned(FUIBuilder) then
  begin
    FUIBuilder.ActiveArea := vArea;
    FUIBuilder.PrintHierarchy;
  end;
end;

procedure TSampleArea.OnExecuteAction(Sender: TObject);
var
  vArea: TUIArea;
begin
  vArea := AreaFromSender(Sender);
  if not Assigned(vArea) then
    Exit;

  FUIBuilder.LastArea := vArea;

  if Assigned(vArea.View) then
    vArea.ExecuteUIAction(vArea.View);
end;

procedure TSampleArea.OnExit(Sender: TObject);
begin
  if Assigned(FUIBuilder) then
    FUIBuilder.ActiveArea := nil;
end;

procedure TSampleArea.OnOpenCollection(Sender: TObject);
var
  vArea: TUIArea;
  vView: TView;
  vLayout: string;
  vWorkArea: string;
begin
  vArea := AreaFromSender(Sender);
  vView := vArea.View;

  FUIBuilder.LastArea := vArea;
  vLayout := vView.QueryParameter('ContentLayout', 'Collection');
  vWorkArea := vView.QueryParameter('ContentWorkArea', 'WorkArea');
  if vArea.QueryParameter('ContentWorkArea', '') <> '' then
    vWorkArea := vArea.QueryParameter('ContentWorkArea', '');
  if vArea.QueryParameter('ContentLayout', '') <> '' then
    vLayout := vArea.QueryParameter('ContentLayout', '');

  FUIBuilder.Navigate(vView, vWorkArea, vLayout);
end;

procedure TSampleArea.PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  // Поместить нативный контрол в область
  FOriginLeft := ALeft;
  FOriginTop := ATop;
end;

procedure TSampleArea.RefillArea(const AKind: Word);
var
  vEntity: TEntity;
begin
  if FView.DefinitionKind = dkEntity then
  begin
    if AKind = dckViewStateChanged then
    begin

    end
    else begin
      vEntity := TEntity(FView.DomainObject);
      Clear;
      if Assigned(vEntity) then
        if QueryParameter('view') <> '' then
          //CreateEntityArea(AParentArea, ALayout, AView, QueryParameter('view')): TUIArea;
        else if QueryParameter('childLayout') <> '' then
          FUIBuilder.ApplyLayout(Self, FView, Trim(QueryParameter('childLayout')))
        else
          FUIBuilder.ApplyLayout(Self, FView, vEntity.Definition.Name + 'EditForm');
    end;
  end
  else if FView.DefinitionKind = dkCollection then
  begin
  end;
end;

destructor TSampleArea.Destroy;
var
  vControl: TObject;
begin
  vControl := FControl;

  inherited Destroy;

  if Assigned(vControl) then;
    // Очистить vControl
end;

procedure TSampleArea.SaveLayoutToFile(const AFileName: string);
begin
  // TODO Переделать
end;

procedure TSampleArea.SetControl(const AControl: TObject);
begin
  inherited SetControl(AControl);
  // Привязать обработчики событий
end;

procedure TSampleArea.SetParent(const Value: TUIArea);
begin
  inherited SetParent(Value);
  // Установить отношения между внутренними контролами областей
end;

procedure TSampleArea.SetViewState(const AValue: TViewState);
begin
  if not Assigned(FControl) then
    Exit;
  // Установить внутреннему контролу свойства Visible, Enabled и ReadOnly
end;

procedure TSampleArea.UnbindContent;
begin
  FControl := nil;
end;

procedure TSampleArea.UpdateArea(const AKind: Word; const AParameter: TEntity = nil);
begin
  RefillArea(AKind);

  if not Assigned(FControl) then
    Exit;

  if not FIsForm then
  begin
    //if (FControl is TWinControl) and Assigned(TWinControl(FControl)) then
    //  TWinControl(FControl).Parent.DisableAlign;

    try
      SetViewState(FView.State);

      // Если не сделать проверку, при изменениях отображение начинает "плыть"
      // НЕ РАБОТАЕТ! Перемаргивает при навигации по гриду
      {if (FControl is TcxButton) and (TcxButton(FControl).Align <> alNone) then
      begin
        // restore origin values to correct alignment order
        if Control.Left <> FOriginLeft then
          Control.Left := FOriginLeft;
        if Control.Top <> FOriginTop then
          Control.Top := FOriginTop;
      end;}
    finally
      //if (FControl is TWinControl) and Assigned(TWinControl(FControl)) then
      //  TWinControl(FControl).Parent.EnableAlign;
    end;
  end;
end;

{ TSampleFieldArea }

procedure TSampleFieldArea.AfterParentChanged;
begin
end;

constructor TSampleFieldArea.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
  const AControl: TObject = nil; const ALayout: TObject = nil; const AParams: string = '');
var
  vPopupArea: TSampleArea;
  vPopupMenu: TPopupMenu;
begin
  FFieldDef := TFieldDef(AView.Definition);

  FId := FFieldDef.Name;
  FUId := FFieldDef.Name;
  inherited Create(AParent, AView, AId, AIsService, AControl, ALayout, AParams);

  DoCreateControl(ALayout);

  Assert(Assigned(FControl), 'Не создан контрол для ' + FFieldDef.Name);

  // Установка контекстного меню
  if Assigned(ALayout) and (ALayout is TPanel) and Assigned(TPanel(ALayout).PopupMenu)
    and Assigned(FControl) and (FControl is TControl)
  then begin
    vPopupArea := TSampleArea(Parent.AreaById('Popup'));
    if Assigned(vPopupArea) then
    begin
      // Создать контекстное меню
    end;
  end;

  SetControl(FControl);

  SetParent(AParent);

  // Нужно делать после задания родителя, так как надпись использует родительский шрифт
  CreateCaption(FFieldDef);

  AfterParentChanged;
end;

procedure TSampleFieldArea.Deinit;
begin
  DoDeinit;
end;

destructor TSampleFieldArea.Destroy;
begin
  DoBeforeFreeControl;
  inherited Destroy;
end;

procedure TSampleFieldArea.DoBeforeFreeControl;
begin
end;

procedure TSampleFieldArea.DoCreateControl(const ALayout: TObject);
begin
end;

procedure TSampleFieldArea.DoDeinit;
begin
end;

procedure TSampleFieldArea.DoOnChange;
begin
end;

procedure TSampleFieldArea.FillEditor;
begin
end;

function TSampleFieldArea.GetName: string;
begin
  Result := FFieldDef.Name;
end;

procedure TSampleFieldArea.OnChange(Sender: TObject);
var
  vEntity: TEntity;
begin
  vEntity := TEntity(FView.ParentDomainObject);
  if not TCanChangeFieldFunc(TDomain(FView.Domain).Configuration.CanChangeFieldFunc)(FView, vEntity, FFieldDef.Name) then
  begin
    RefillArea(dckFieldChanged);
    Exit;
  end;

  FView.AddListener(FUIBuilder.RootArea);
  try
    // Отключить прослушивание событий
    SwitchChangeHandlers(nil);
    FView.RemoveListener(Self);
    try
      DoOnChange;
    finally
      SwitchChangeHandlers(OnChange);
      FView.AddListener(Self);
    end;
  finally
    FView.RemoveListener(FUIBuilder.RootArea);
  end;
end;

procedure TSampleFieldArea.RefillArea(const AKind: Word);
var
  vEntity: TEntity;
begin
  vEntity := TEntity(FView.ParentDomainObject);
  if not Assigned(vEntity) then
    Exit;

  try
    SwitchChangeHandlers(nil);
    try
      if not Assigned(FView.ParentDomainObject) then
        DoDeinit
      else begin
        FillEditor;
      end;
    finally
      SwitchChangeHandlers(OnChange);
    end;
  except
    TInteractor(FView.Interactor).ShowMessage('Error in RefillArea, Field: ' + FFieldDef.Name);
  end;
end;

procedure TSampleFieldArea.SetFieldEntity(const AEntity: TEntity);
begin
  FView.SetFieldEntity(Holder, AEntity);
end;

procedure TSampleFieldArea.SetFieldStream(const AStream: TStream);
begin
  FView.SetFieldStream(Holder, AStream);
end;

procedure TSampleFieldArea.SetFieldValue(const AValue: Variant);
begin
  FView.SetFieldValue(Holder, AValue);
end;

procedure TSampleFieldArea.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
end;

end.
