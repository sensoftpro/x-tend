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

unit uUIBuilder;

interface

uses
  Classes, Generics.Collections, Generics.Defaults, UITypes, SysUtils, uConsts, uView, uDefinition, uEntity, uSession, uLayout;

type
  TUIBuilder = class;
  TUIArea = class;
  TUIAreaClass = class of TUIArea;

  TNativeControl = class
  protected
    [Weak] FOwner: TUIArea;
    [Weak] FParent: TUIArea;
    [Weak] FControl: TObject;
    [Weak] FInteractor: TObject;
    [Weak] FUIBuilder: TUIBuilder;

    FIsForm: Boolean;
    FIsAutoReleased: Boolean;

    function AreaFromSender(const ASender: TObject): TUIArea; virtual;
    procedure PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer); virtual;
    procedure DoClose(const AModalResult: Integer); virtual;
    function GetName: string; virtual;
    procedure DoActivate(const AUrlParams: string); virtual;
    procedure SetParent(const AParent: TUIArea); virtual;
    procedure SetControl(const AControl: TObject); virtual;
    function DoGetDescription: string; virtual;

    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate; virtual;

    procedure UnbindContent;
  public
    constructor Create(const AOwner: TUIArea; const AControl: TObject); virtual;
    destructor Destroy; override;

    procedure SetViewState(const AValue: TViewState); virtual;

    property Owner: TUIArea read FOwner;
    property Parent: TUIArea read FParent write SetParent;
    property Control: TObject read FControl write SetControl;
    property IsForm: Boolean read FIsForm;
  end;

  TNativeControlClass = class of TNativeControl;

  TUIArea = class
  private
    [Weak] FParent: TUIArea;
    [Weak] FHolder: TObject;
    FAreas: TList<TUIArea>;
    FParams: TStrings;
    FIsService: Boolean;
    FUpdateCount: Integer;
    function GetArea(const AIndex: Integer): TUIArea;
    function GetCount: Integer;
    function GetInteractor: TObject;
    function GetPresenter: TObject;
    function GetDomain: TObject;
    function GetHolder: TObject;
    procedure TrySubscribeView;
    procedure TryUnsubscribeView;
    procedure SetView(const Value: TView);
    procedure DisableContent;
    procedure AfterChildAreasCreated;
    function ParentInUpdate: Boolean;
    procedure SetLayout(const Value: TLayout);
    function GetInnerControl: TObject;
  protected
    FNativeControl: TNativeControl;
    FLayout: TLayout;
    FInternalParams: string;
    FCreateParams: TStrings;
    function DoCreateChildArea(const ALayout: TLayout; const AView: TView; const AParams: string = ''; const AOnClose: TProc = nil): TUIArea; virtual; abstract;
    function DoCreateChildAction(const ALayout: TLayout; const AView: TView; const AParams: string = ''): TUIArea;
    function DoCreateChildList(const ALayout: TLayout; const AView: TView; const AParams: string = ''): TUIArea;
    function DoCreateChildNavigation(const ALayout: TLayout; const AView: TView; const AParams: string = ''): TUIArea; virtual;
    function DoCreateChildEditor(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
    function CreateChildLayoutedArea(const ALayout: TLayout; const AView: TView;
      const AChildLayoutName: string; const AParams: string): TUIArea;
    function CreateChildArea(const AChildView: TView; const ALayout: TLayout; const AParams: string; const AOnClose: TProc = nil): TUIArea;

    procedure CreateCaption(const AFieldDef: TFieldDef); virtual;
    function AppendServiceArea: TUIArea; virtual; abstract;

    function GetAreaByView(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
    procedure Clear;
    procedure ClearContent;
    // Отвязать все нативные элементы
    procedure UnbindContent;

    function GetFieldTranslation(const AFieldDef: TFieldDef; const ATranslationPart: TTranslationPart = tpCaption): string;
    function GetTranslation(const ADefinition: TDefinition; const ATranslationPart: TTranslationPart = tpCaption): string;

    procedure ProcessAreaClick(const AArea: TUIArea);
  protected
    [Weak] FUIBuilder: TUIBuilder;
    [Weak] FView: TView;
    [Weak] FSession: TUserSession;
    FId: string;
    FUId: string;

    function GetName: string; virtual;
    procedure DoAfterChildAreasCreated; virtual;
    procedure DoOnExit(Sender: TObject); virtual;
    procedure DoActivate(const AUrlParams: string); virtual;
    // Отвязать все обработчики
    procedure DoDisableContent; virtual;
    procedure DoExecuteUIAction(const AView: TView); virtual;
    procedure SetParent(const Value: TUIArea); virtual;
    procedure SetControl(const AControl: TObject); virtual;

    function TryCreatePopupArea(const ALayout: TLayout): TUIArea; virtual;
    procedure SetPopupArea(const APopupArea: TUIArea); virtual;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; virtual;
    procedure AssignFromLayout(const ALayout: TLayout; const AParams: string); virtual;
    procedure ArrangeChildAreas; virtual;
    procedure SaveLayoutToFile(const AFileName: string); virtual;
    procedure RefillArea(const AKind: Word); virtual;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); virtual;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure DM_ViewChanged(var AMessage: TViewChangedMessage); message DM_VIEW_CHANGED;

    function NativeControlClass: TNativeControlClass;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = ''); virtual;
    destructor Destroy; override;

    // Полная очистка и удаление
    procedure Release;

    procedure AddArea(const AArea: TUIArea);
    procedure RemoveArea(var AArea: TUIArea);
    procedure Close(const AModalResult: Integer);
    function TextHierarchy(const AIndent: string = ''): string;
    function AreaById(const AId: string; const ARecoursive: Boolean = True): TUIArea;
    function GetImageID(const AImageID: Integer): Integer;
    procedure ExecuteUIAction(const AView: TView);

    function LessThanUIState(const ADefinition: TDefinition; const ASession: TObject; const AState: TViewState): Boolean;

    procedure Activate(const AUrlParams: string);
    procedure OnEnter(Sender: TObject);
    procedure OnExit(Sender: TObject);
    procedure OnAreaClick(Sender: TObject);

    procedure AddParams(const AParams: TStrings);
    procedure SetHolder(const AHolder: TObject);
    function QueryParameter(const AName: string; const ADefaultValue: string = ''): string;
    procedure SetBounds(const ALeft, ATop, AWidth, AHeight: Integer);

    property InnerControl: TObject read GetInnerControl;
    property Count: Integer read GetCount;
    property Areas[const AIndex: Integer]: TUIArea read GetArea; default;
    property Parent: TUIArea read FParent;
    property Layout: TLayout read FLayout write SetLayout;
    property CreateParams: TStrings read FCreateParams;
    property InternalParams: string read FInternalParams;
    property UIBuilder: TUIBuilder read FUIBuilder;
    property View: TView read FView; // write SetView;
    property Presenter: TObject read GetPresenter;
    property Interactor: TObject read GetInteractor;
    property Domain: TObject read GetDomain;
    property Name: string read GetName;
    property Id: string read FId;
    property UId: string read FUId write FUId;
    property Holder: TObject read GetHolder;
    property ThisHolder: TObject read FHolder;
  end;

  (*TNavigationArea = class(TUIArea)
  private
    function CreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem;
      const AView: TView; const ALevel: Integer): TObject;
  protected
    FInitialMenu: TNavigationItem;
    FParams: string;
    function DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; virtual; abstract;
    procedure DoAfterCreate(const AInteractor: TObject); virtual;
    procedure DoProcessChilds(const AParentArea: TUIArea; const AView: TView; const ANavItem: TNavigationItem;
      const ALevel: Integer); virtual;
    function TryCreatePopupArea(const ALayout: TLayout): TUIArea; override;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = ''); override;
    destructor Destroy; override;

    procedure ProcessChilds;
  end; *)

  TUIBuilder = class
  private
    FRootView: TView;
    FRootArea: TUIArea;
    FDefaultParams: string;
    [Weak] FCurrentArea: TUIArea;
    [Weak] FLastArea: TUIArea;
    [Weak] FActiveArea: TUIArea;
    [Weak] FPagedArea: TUIArea;
    [Weak] FInteractor: TObject;
    [Weak] FPresenter: TObject;

    function MakeLayoutFromFile(const AFileName: string; const AParams: string): TLayout;
    function MakeDefaultLayout(const AView: TView; const ALayoutName: string): TLayout;
    procedure SetLastArea(const Value: TUIArea);
    procedure SetPagedArea(const Value: TUIArea);

    function GetTranslation(const ADefinition: TDefinition; const ATranslationPart: TTranslationPart = tpCaption): string;
    function GetImageID(const AImageID: Integer): Integer;
    procedure GetLayoutName(const AEntity: TEntity; const AParams: string; var ALayoutName: string);
  public
    constructor Create(const AInteractor: TObject);
    destructor Destroy; override;

    procedure ApplyLayout(const AArea: TUIArea; const AView: TView; const ALayoutName: string; const AParams: string);

    function Navigate(const AView: TView; const AAreaName, ALayoutName: string;
      const AOptions: string = ''; const AChangeHolder: TObject = nil; const ACallback: TNotifyEvent = nil;
      const ACaption: string = ''; const AOnClose: TProc = nil): TDialogResult;
    function GetFieldTranslation(const AFieldDef: TFieldDef; const ATranslationPart: TTranslationPart = tpCaption): string;

    procedure CreateChildAreas(const AArea: TUIArea; const ALayout: TLayout; const AParams: string);
    procedure CloseCurrentArea(const AModalResult: Integer);
    procedure PrintHierarchy;
    procedure ProcessAreaDeleting(const AArea: TUIArea);

    property RootView: TView read FRootView;
    property RootArea: TUIArea read FRootArea;
    property CurrentArea: TUIArea read FCurrentArea;
    property DefaultParams: string read FDefaultParams write FDefaultParams;
    property PagedArea: TUIArea read FPagedArea write SetPagedArea;
    property LastArea: TUIArea read FLastArea write SetLastArea;
    property ActiveArea: TUIArea read FActiveArea write FActiveArea;
    property Presenter: TObject read FPresenter;
  end;

implementation

uses
  {DO NOT ADD VCL UNITS HERE (Controls, Forms...)}
  StrUtils, IOUtils, Windows, Messages,
  uPlatform, uPresenter, uInteractor, uConfiguration, uChangeManager,
  uUtils, uDomain, uObjectField, uEntityList;

{ TUIBuilder }

procedure TUIBuilder.ApplyLayout(const AArea: TUIArea; const AView: TView; const ALayoutName: string; const AParams: string);
var
  vFileName: string;
  vUser: TEntity;
  vRoles: TListField;
  vRole: TEntity;
  vPostfix, vParams: string;
  vLayout: TLayout;
begin
  vParams := AParams;
  if Pos('childLayout=' + ALayoutName, AParams) > 0 then
    vParams := RemoveUrlParam(vParams, 'childLayout');

  vUser := TUserSession(TInteractor(FInteractor).Session).CurrentUser;
  vRoles := TListField(vUser.FieldByName('Roles'));
  if vRoles.Count <> 1 then
  begin
    if TUserSession(TInteractor(FInteractor).Session).IsAdmin then
      vPostfix := '_Administrators'
    else
      vPostfix := '';
  end
  else begin
    vRole := TEntity(vRoles[0]).ExtractEntity('Role');
    if Assigned(vRole) then
      vPostfix := '_' + vRole['Code']
  end;

  vFileName := TConfiguration(TInteractor(FInteractor).Configuration).FindLayoutFile(ALayoutName, LAYOUT_DFM_EXT, vPostfix);
  if FileExists(vFileName) then
    vLayout := MakeLayoutFromFile(vFileName, vParams)
  else
    vLayout := MakeDefaultLayout(AView, ALayoutName);

  AArea.BeginUpdate;
  try
    TPresenter(FPresenter).EnumerateControls(vLayout);
    AArea.AssignFromLayout(vLayout, AParams);
    AArea.SetView(AView);
    CreateChildAreas(AArea, vLayout, AParams);
    AArea.SetBounds(
      StrToIntDef(GetUrlParam(AParams, 'Left', ''), -1),
      StrToIntDef(GetUrlParam(AParams, 'Top', ''), -1),
      StrToIntDef(GetUrlParam(AParams, 'Width', ''), -1),
      StrToIntDef(GetUrlParam(AParams, 'Height', ''), -1)
    );
  finally
    AArea.EndUpdate;
  end;
end;

procedure TUIBuilder.CloseCurrentArea(const AModalResult: Integer);
begin
  FCurrentArea.Close(AModalResult);
end;

constructor TUIBuilder.Create(const AInteractor: TObject);
begin
  inherited Create;
  FInteractor := AInteractor;
  FPresenter := TInteractor(FInteractor).Presenter;
  FRootArea := nil;
  FCurrentArea := nil;
  FLastArea := nil;
  FActiveArea := nil;
  FPagedArea := nil;
  FDefaultParams := '';
  FRootView := TView.Create(TInteractor(FInteractor), nil, '');
end;

procedure TUIBuilder.CreateChildAreas(const AArea: TUIArea; const ALayout: TLayout; const AParams: string);
var
  i: Integer;
begin
  AArea.Layout := ALayout;
  for i := 0 to ALayout.Items.Count - 1 do
    AArea.CreateChildArea(AArea.View, ALayout.Items[i], AParams);
  AArea.AfterChildAreasCreated;
end;

destructor TUIBuilder.Destroy;
begin
  SetLastArea(nil);
  FCurrentArea := nil;
  FPagedArea := nil;
  FRootArea.Release;
  FreeAndNil(FRootView);
  FPresenter := nil;
  FInteractor := nil;
  inherited Destroy;
end;

function TUIBuilder.GetTranslation(const ADefinition: TDefinition;
  const ATranslationPart: TTranslationPart = tpCaption): string;
begin
  if Assigned(FInteractor) then
    Result := TDomain(TInteractor(FInteractor).Domain).TranslateDefinition(ADefinition, ATranslationPart)
  else begin
    case ATranslationPart of
      tpCaption: Result := ADefinition._Caption;
      tpEmptyValue: Result := TDefinition(ADefinition)._EmptyValue;
      tpPrefix: Result := TDefinition(ADefinition).Prefix;
    else
      Result := '';
    end;
  end;
end;

function TUIBuilder.GetFieldTranslation(const AFieldDef: TFieldDef; const ATranslationPart: TTranslationPart): string;
begin
  if Assigned(FInteractor) then
    Result := TDomain(TInteractor(FInteractor).Domain).TranslateFieldDef(AFieldDef, ATranslationPart)
  else begin
    case ATranslationPart of
      tpCaption: Result := AFieldDef._Caption;
      tpHint: Result := AFieldDef._Hint;
    else
      Result := '';
    end;
  end;
end;

function TUIBuilder.GetImageID(const AImageID: Integer): Integer;
begin
  Result := TInteractor(FInteractor).GetImageIndex(AImageID);
end;

function TUIBuilder.MakeDefaultLayout(const AView: TView; const ALayoutName: string): TLayout;
var
  vDefinition: TDefinition;
  vFieldDef: TFieldDef;
  vAction: TActionDef;
  vView: TView;
  vLayout: TLayout;
  vOneRowHeight: Integer;
  function CalcLayoutPositionCount(const AFieldKind: TFieldKind; const AViewName: string): Integer;
  begin
    if ((AFieldKind = fkString) and (AViewName = 'memo')) or (AFieldKind = fkBlob) then
      Result := 2
    else if AFieldKind = fkList then
      Result := 3
    else
      Result := 1;
  end;
begin
  Result := nil;
  if not Assigned(AView) or not (AView.DefinitionKind in [dkEntity, dkAction, dkObjectField]) then
    Exit;

  if AView.DefinitionKind = dkObjectField then
  begin
    if AView.DomainObject is TEntity then
      vDefinition := TEntity(AView.DomainObject).Definition
    else
      vDefinition := TDefinition(TObjectFieldDef(AView.Definition).ContentDefinitions[0])
  end
  else
    vDefinition := TDefinition(AView.Definition);

  if not Assigned(vDefinition) then
    Exit;

  Result := TPresenter(FPresenter).CreateLayoutArea(lkPanel);

  for vAction in vDefinition.Actions.Objects do
  begin
    vView := AView.BuildView(vAction.Name);
    if Assigned(vView) then
    begin
      if vView.State > vsHidden then
      begin
        vLayout := TPresenter(FPresenter).CreateLayoutArea(lkPanel);
        TPresenter(FPresenter).SetLayoutCaption(vLayout, vAction.Name);
        Result.Add(vLayout);
      end
      else
        vView.CleanView;
    end;
  end;

  vOneRowHeight := TPresenter(Presenter).GetLayoutFontHeight(Result) + 8;

  for vFieldDef in vDefinition.Fields do
  begin
    vView := AView.BuildView(vFieldDef.Name);
    if Assigned(vView) then
    begin
      if not TInteractor(FInteractor).NeedSkipField(nil, vFieldDef) and (vFieldDef.Kind <> fkComplex) then
      begin
        vLayout := TPresenter(FPresenter).CreateLayoutArea(lkPanel);
        TPresenter(FPresenter).SetLayoutCaption(vLayout, vFieldDef.Name);
        TPresenter(FPresenter).SetLayoutBounds(vLayout, 0, 0, cDefaultColumnWidth, CalcLayoutPositionCount(vFieldDef.Kind, vFieldDef.StyleName) * (vOneRowHeight + cBetweenRows) - cBetweenRows);
        Result.Add(vLayout);
      end
      else
        vView.CleanView;
    end;
  end;

  Result.ArrangeChildAreas;

  if (_Platform.DeploymentType = 'dev') or (_Platform.DeploymentType = 'mock') then
    Result.SaveToDFM(TPath.Combine(TPath.GetTempPath, ALayoutName + '_auto'));
end;

function TUIBuilder.MakeLayoutFromFile(const AFileName: string; const AParams: string): TLayout;
var
  vFileStream: TStream;
  vMemStream: TStream;
  vFrame: TComponent;
begin
  Result := TPresenter(FPresenter).CreateLayoutArea(lkFrame);
  vFrame := TComponent(Result.Control);
  vFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  vMemStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(vFileStream, vMemStream);

    vMemStream.Position := 0;
    vMemStream.ReadComponent(vFrame);
  finally
    FreeAndNil(vFileStream);
    FreeAndNil(vMemStream);
  end;
end;

procedure TUIBuilder.GetLayoutName(const AEntity: TEntity; const AParams: string; var ALayoutName: string);
var
  vLayout, vOperation, vDefValue: string;

  function GetPostfix: string;
  begin
    if vOperation = 'slap' then
      Result := 'SlapForm'
    else
      Result := 'EditForm';
  end;

  function ExtractPrefix(var ALayoutPath: string): string;
  var
    vList: TStrings;
    i: Integer;
  begin
    Result := '';
    if Pos('#', ALayoutPath) = 0 then Exit;

    vList := CreateDelimitedList(ALayoutPath, '#');
    try
      for i := vList.Count - 2 downto 0 do
        try
          Result := AEntity[vList[i]] + Result;
        except
        end;

      ALayoutPath := vList[vList.Count - 1];
    finally
      FreeAndNil(vList);
    end;
  end;

  function GetFromLayoutPath(const ALayoutPath: string): string;
  var
    vPath, vPrefix: string;
  begin
    vPath := ALayoutPath;
    vPrefix := ExtractPrefix(vPath);
    Result := vPrefix + vPath + GetPostfix;
  end;
begin
  vOperation := GetUrlParam(AParams, 'operation');
  vDefValue := '';
  if vOperation = 'opencollection' then
    vDefValue := 'Collection';

  vLayout := GetUrlParam(AParams, 'ContentLayout', vDefValue);

  if vLayout = '@Path' then
    vLayout := GetFromLayoutPath(Trim(GetUrlParam(AParams, 'LayoutPath', '')));

  if (vLayout = '') and Assigned(AEntity) then
  begin
    if Length(AEntity.Definition.LayoutMask) > 0 then
      vLayout := GetFromLayoutPath(AEntity.Definition.LayoutMask);

    if (vLayout = '') or (not FileExists(TConfiguration(TInteractor(FInteractor).Configuration).FindLayoutFile(vLayout, LAYOUT_DFM_EXT))) then
      vLayout := AEntity.Definition.Name + GetPostfix;
  end;

  ALayoutName := vLayout;
end;

function TUIBuilder.Navigate(const AView: TView; const AAreaName, ALayoutName: string;
  const AOptions: string = ''; const AChangeHolder: TObject = nil; const ACallback: TNotifyEvent = nil;
  const ACaption: string = ''; const AOnClose: TProc = nil): TDialogResult;
var
  vLayoutName: string;
  vAreaName: string;
  vViewName: string;
  vLastCurrentArea: TUIArea;
  vUIArea: TUIArea;
  vTabArea: TUIArea;
  vServiceArea: TUIArea;
  vView: TView;
  vLayout: TLayout;
  vTabParams: string;
  vPageID: string;
  vImageID: Integer;
  vDefaultCaption: string;
  vIsMainForm: Boolean;
  vParams: TStrings;
  vOptions: string;

  function ExtractValueFromStrings(const AList: TStrings; const AKey: string; const ADefault: string = ''): string;
  begin
    if AList.IndexOfName(AKey) >= 0 then
      Result := AList.Values[AKey]
    else
      Result := ADefault;
  end;
begin
  ///  AAreaName - идентификатор области, указывающий на область для заполнения
  ///  ALayoutName - имя лэйаута, которое будет использовано для заполнения области

  /// Нужно учитывать область, которая останется владельцем
  vIsMainForm := False;
  vLastCurrentArea := FCurrentArea;
  vLayoutName := ALayoutName;

  if Assigned(AView) then
  begin
    Assert(AView.DefinitionKind in [dkEntity, dkAction, dkObjectField, dkCollection], 'Показываем непонятно что');
    if vLayoutName = '' then
    begin
      if AView.DomainObject is TEntity then
        GetLayoutName(AView.DomainObject as TEntity, AOptions, vLayoutName)
      else
        GetLayoutName(nil, AOptions, vLayoutName);
    end;
    vView := AView;
  end
  else
    vView := FRootView;

  if (AAreaName = 'WorkArea') and (TInteractor(FInteractor).Layout <> 'mdi') and not Assigned(FRootArea.AreaById(AAreaName)) then
    vAreaName := 'child'
  else
    vAreaName := AAreaName;

  vUIArea := TPresenter(FPresenter).CreateUIArea(TInteractor(FInteractor), FCurrentArea, vView, vAreaName, ACallback, ACaption, AOnClose);
  // Главная форма и форма редактирования
  if Assigned(vUIArea) then
  begin
    vUIArea.SetHolder(AChangeHolder);
    vUIArea.BeginUpdate;
    try
      if vAreaName = '' then
      begin
        TPresenter(FPresenter).CloseUIArea(TInteractor(FInteractor), FRootArea, vUIArea);

        FRootArea := vUIArea;
        ApplyLayout(vUIArea, vView, vLayoutName, AOptions);
        vIsMainForm := True;
      end
      else begin
        ///todo Нужна явная область вместо FCurrentArea!!! Записывать ее в UIBuilder перед Navigate

        if FCurrentArea.FAreas.IndexOf(vUIArea) >= 0 then
        begin
          Result := TPresenter(TInteractor(FInteractor).Presenter).ShowUIArea(TInteractor(FInteractor), vAreaName, AOptions, vUIArea);
          Exit;
        end;

        FCurrentArea.AddArea(vUIArea);
        ApplyLayout(vUIArea, vView, vLayoutName, AOptions);
        if not Assigned(ACallback) and (vAreaName = 'child') then
        begin
          vServiceArea := vUIArea.AppendServiceArea;
          vUIArea.AddArea(vServiceArea);
          if vView.State >= vsSelectOnly {and Assigned(AChangeHolder) - у параметров нет холдера} then
            ApplyLayout(vServiceArea, vUIArea.View, 'OkCancel', '')
          else
            ApplyLayout(vServiceArea, vUIArea.View, 'Close', '');
        end;
      end;

      if vAreaName <> 'float' then
        FCurrentArea := vUIArea;
    finally
      vUIArea.EndUpdate;
    end;
  end
  else begin
    vUIArea := FRootArea.AreaById(vAreaName);
    if not Assigned(vUIArea) then
    begin
      if vAreaName = 'WorkArea' then
        vUIArea := FRootArea
      else
        Assert(False, 'Область [' + vAreaName + '] не найдена');
    end;

    if (vUIArea.QueryParameter('ViewType') = 'Paged') or (vUIArea = FRootArea) then
    begin
      if Assigned(AView) then
      begin
        vParams := CreateDelimitedList(AOptions, '&');
        try
          vPageID := ExtractValueFromStrings(vParams, 'Cube', '');
          vImageID := StrToIntDef(ExtractValueFromStrings(vParams, 'ImageID', ''), -1);
        finally
          FreeAndNil(vParams);
        end;

        if vPageID <> '' then
          vPageID := ReplaceText(AView.FullName, '/', '_') + '_' + vPageID
        else
          vPageID := ReplaceText(AView.FullName, '/', '_');
        vPageID := ReplaceText(vPageID, '~', '_');
      end
      else
      begin
        vPageID := vLayoutName;
        vImageID := StrToIntDef(GetUrlParam(AOptions, 'ImageID', '-1'), -1);
      end;

      vTabArea := vUIArea.AreaById(vPageID, False);
      if not Assigned(vTabArea) then
      begin
        if vView.DefinitionKind in [dkAction, dkCollection] then
        begin
          if ACaption <> '' then
            vTabParams := 'Caption=' + ACaption
          else begin
            vDefaultCaption := GetTranslation(TDefinition(vView.Definition));
            if Assigned(FLastArea) and (FLastArea.View = vView) then
              vTabParams := 'Caption=' + FLastArea.QueryParameter('Caption', vDefaultCaption)
            else
              vTabParams := 'Caption=' + vDefaultCaption;
          end;

          if vImageID < 0 then
            vImageID := GetImageID(TDefinition(vView.Definition)._ImageID)
          else
            vImageID := GetImageID(vImageID);
          vTabParams := vTabParams + ';ImageIndex=' + IntToStr(vImageID);
          vTabParams := vTabParams + ';Name=' + vPageID;
        end
        else if ACaption <> '' then
          vTabParams := 'Caption=' + ACaption + ';ImageIndex=' + IntToStr(GetImageID(StrToIntDef(GetUrlParam(AOptions, 'ImageID', '-1'), 0))) + ';Name=' + vPageID
        else
          vTabParams := 'Caption=Стартовая страница;ImageIndex=' + IntToStr(GetImageID(StrToIntDef(GetUrlParam(AOptions, 'ImageID', '-1'), 0))) + ';Name=' + vPageID;

        vLayout := TPresenter(FPresenter).CreateLayoutArea(lkPage, vTabParams);
        //vUIArea.Layout := vLayout;
        try
          vTabArea := vUIArea.CreateChildArea(vView, vLayout, AOptions, AOnClose);
          vTabArea.BeginUpdate;
          try
            vTabArea.SetHolder(AChangeHolder);
            if AOptions <> '' then
              vTabArea.AddParams(CreateDelimitedList(AOptions, '&'));
            ApplyLayout(vTabArea, vView, vLayoutName, AOptions);
          finally
            vTabArea.EndUpdate;
          end;
        finally
          //vTab.Free;
        end;
      end;

      vTabArea.Activate(FDefaultParams + IfThen(FDefaultParams = '', '', '&') + 'TabActivationOption=' + GetUrlParam(AOptions, 'TabActivationOption', 'ChangeTab'));
    end
    else begin
      vView.AddListener(FRootArea);
      vUIArea.BeginUpdate;
      try
        vUIArea.Clear;
        ApplyLayout(vUIArea, vView, vLayoutName, AOptions);
        vUIArea.SetHolder(AChangeHolder);
      finally
        vView.RemoveListener(FRootArea);
        vUIArea.EndUpdate;
      end;
    end;
  end;

  PrintHierarchy;

  vOptions := AOptions;

  if ACaption <> '' then
  begin
    if vOptions <> '' then
      vOptions := vOptions + '&';
    vOptions := vOptions + 'Caption=' + ACaption;
  end;

  Result := TPresenter(TInteractor(FInteractor).Presenter).ShowUIArea(TInteractor(FInteractor), vAreaName, vOptions, vUIArea);
  if Result > drNone then
    FCurrentArea := vLastCurrentArea;

  if vIsMainForm and (FDefaultParams <> '') then
  begin
    vParams := CreateDelimitedList(FDefaultParams, '&');
    try
      vViewName := ExtractValueFromStrings(vParams, 'View');
      vLayoutName := ExtractValueFromStrings(vParams, 'Layout');
      if (vViewName <> '') or (vLayoutName <> '') then
        Navigate(FRootView.BuildView(vViewName), 'WorkArea',
          vLayoutName, '', nil, nil, ExtractValueFromStrings(vParams, 'Caption'));
    finally
      FreeAndNil(vParams);
      FDefaultParams := '';
    end;
  end;

  PrintHierarchy;
end;

procedure TUIBuilder.PrintHierarchy;
begin
  TPresenter(FPresenter).ShowPage(TInteractor(FInteractor), 'debug');
end;

procedure TUIBuilder.ProcessAreaDeleting(const AArea: TUIArea);
begin
  if FRootArea = AArea then
    FRootArea := nil;
  if FCurrentArea = AArea then
    FCurrentArea := nil;
  if FActiveArea = AArea then
    FActiveArea := nil;
  if FPagedArea = AArea then
    FPagedArea := nil;
  //if FLastArea = AArea then
  //  FLastArea := nil;
end;

procedure TUIBuilder.SetLastArea(const Value: TUIArea);
begin
  if FLastArea = Value then
    Exit;

  if Assigned(FLastArea) and not Assigned(FLastArea.UIBuilder) then
  begin
    FLastArea.UnbindContent;
    FLastArea.Free;
  end;
  FLastArea := Value;
end;

procedure TUIBuilder.SetPagedArea(const Value: TUIArea);
begin
  Assert(not Assigned(FPagedArea), 'Область страниц инициализирована дважды!');
  FPagedArea := Value;
end;

{ TUIArea }

procedure TUIArea.Activate(const AUrlParams: string);
var
  vArea: TUIArea;
begin
  for vArea in FAreas do
    vArea.Activate(AUrlParams);
  DoActivate(AUrlParams);
end;

procedure TUIArea.AddArea(const AArea: TUIArea);
begin
  AArea.SetParent(Self);
  FAreas.Add(AArea);
end;

procedure TUIArea.AddParams(const AParams: TStrings);
begin
  if Assigned(FParams) then
    FreeAndNil(FParams);

  FParams := AParams;
end;

procedure TUIArea.AfterChildAreasCreated;
begin
  DoAfterChildAreasCreated;
end;

function TUIArea.AreaById(const AId: string; const ARecoursive: Boolean = True): TUIArea;
var
  i: Integer;
begin
  if (AId = '') or (AId = 'child') or (AId = 'modal') then
  begin
    Result := nil;
    Exit;
  end;

  if SameText(FId, AId) then
  begin
    Result := Self;
    Exit;
  end;

  for i := 0 to FAreas.Count - 1 do
  begin
    if ARecoursive then
    begin
      Result := GetArea(i).AreaById(AId);
      if Assigned(Result) then
        Exit;
    end
    else begin
      Result := GetArea(i);
      if SameText(Result.Id, AId) then
        Exit;
    end;
  end;
  Result := nil;
end;

procedure TUIArea.ArrangeChildAreas;
begin
end;

procedure TUIArea.AssignFromLayout(const ALayout: TLayout; const AParams: string);
begin
end;

procedure TUIArea.BeginUpdate;
begin
  if (FUpdateCount > 0) or ParentInUpdate then Exit;

  Inc(FUpdateCount);

  FNativeControl.DoBeginUpdate;
end;

procedure TUIArea.Clear;
var
  i: Integer;
  vArea: TUIArea;
begin
  // Обработка сохранения данных при очистке области
  if Assigned(FHolder) then
  begin
    if TChangeHolder(FHolder).IsModified then
      TUserSession(FView.Session).Save(TChangeHolder(FHolder))
    else
      TUserSession(FView.Session).Cancel(TChangeHolder(FHolder));
    FHolder := nil;
  end;

  for i := 0 to FAreas.Count - 1 do
  begin
    vArea := GetArea(i);
    vArea.ClearContent;
    if vArea <> FUIBuilder.LastArea then
      FreeAndNil(vArea);
  end;
  FAreas.Clear;
end;

procedure TUIArea.ClearContent;
begin
  Clear;
  SetParent(nil);
  FreeAndNil(FAreas);
  FreeAndNil(FParams);
  if Assigned(FHolder) then
    if Assigned(TChangeHolder(FHolder).Session) then
      TUserSession(TChangeHolder(FHolder).Session).Cancel(TChangeHolder(FHolder));

  FHolder := nil;

  TryUnsubscribeView;
  FView := nil;

  FUIBuilder.ProcessAreaDeleting(Self);
  FUIBuilder := nil;
end;

procedure TUIArea.Close(const AModalResult: Integer);
begin
  FNativeControl.DoClose(AModalResult);
end;

constructor TUIArea.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
  const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = '');
var
  vPopupArea: TUIArea;
  vControl: TObject;
begin
  inherited Create;

  FId := AId;
  FUId := AId;
  FIsService := AIsService;
  FLayout := ALayout;
  FUpdateCount := 0;

  FInternalParams := AParams;
  FCreateParams := nil;
  if Length(AParams) > 0 then
    FCreateParams := CreateDelimitedList(AParams, '&');

  FAreas := TList<TUIArea>.Create;
  FHolder := nil;
  FUIBuilder := TInteractor(AView.Interactor).UIBuilder;
  FSession := TUserSession(AView.Session);

  FView := AView;
  if not FIsService then
    TrySubscribeView;

  FParent := AParent;
  if not Assigned(AControl) then
    vControl := DoCreateControl(AParent, ALayout)
  else
    vControl := AControl;
  FNativeControl := NativeControlClass.Create(Self, vControl);
  SetControl(vControl);
  SetParent(AParent);

  if not Assigned(ALayout) then
    Exit;

  vPopupArea := TryCreatePopupArea(ALayout);
  if Assigned(vPopupArea) then
  begin
    SetPopupArea(vPopupArea);
    FAreas.Add(vPopupArea);
  end;
end;

procedure TUIArea.CreateCaption(const AFieldDef: TFieldDef);
begin
end;

function TUIArea.CreateChildArea(const AChildView: TView; const ALayout: TLayout; const AParams: string; const AOnClose: TProc = nil): TUIArea;
var
  vPresenter: TPresenter;
  vView: TView;
  vDefaultViewName: string;
  vDefaultView: TView;
  vPos: Integer;
  vQuery: string;
  vCaption: string;
  vLayout: string;
  vAlreadyAssigned: Boolean;
begin
  vAlreadyAssigned := False;
  vPresenter := TPresenter(GetPresenter);
  Assert(ALayout.Kind <> lkNone, 'Не задан тип лэйаута');
  if not (ALayout.Kind in [lkPanel, lkPages, lkMemo]) then
  begin
    Result := DoCreateChildArea(ALayout, AChildView, '', AOnClose)
  end
  else begin
    vCaption := Trim(vPresenter.GetLayoutCaption(ALayout));
    ALayout.SetUrl(vCaption);

    vQuery := '';
    vPos := Pos('?', vCaption);
    if vPos > 0 then
    begin
      if Length(vCaption) - vPos >= 2 then
        vQuery := Copy(vCaption, vPos + 1, Length(vCaption) - vPos);
      vCaption := Copy(vCaption, 1, vPos - 1);
    end;
    if vQuery = '' then
      vQuery := AParams;

    vPos := Pos('@', vCaption);
    if vPos = 1 then
    begin
      // Именованные области
      Delete(vCaption, 1, 1);

      if SameText(vCaption, 'Navigation') {}or SameText(vCaption, 'NavigationArea'){} then
      begin
        Result := DoCreateChildNavigation(ALayout, FView, vQuery);
      end
      else begin
        vPresenter.SetLayoutCaption(ALayout, vCaption);
        Result := DoCreateChildArea(ALayout, FView, vQuery);
      end;
    end
    else if vCaption = '$' then
    begin
      if FView.DefinitionKind = dkCollection then
      begin
        Result := DoCreateChildList(ALayout, FView, vQuery);
        if Assigned(Result) then
          Result.UpdateArea(dckFieldChanged);
      end
      else if FView.DefinitionKind in [dkListField, dkObjectField, dkComplexField, dkEntity] then
      begin
        Result := DoCreateChildEditor(ALayout, FView, vQuery);
        if Assigned(Result) then
          Result.UpdateArea(dckFieldChanged);
      end
      else
        Result := DoCreateChildArea(ALayout, FView);
    end
    else if vCaption = '' then
      Result := DoCreateChildArea(ALayout, FView)
    else begin
      if Assigned(FView) then
        vView := FView.BuildView(vCaption)
      else
        vView := nil;

      if Assigned(vView) then
      begin
        if (vView.DefinitionKind <> dkUndefined) or (vCaption = '#Placeholder') then
        begin
          Result := GetAreaByView(ALayout, vView, vQuery);
          if Assigned(Result) then
          begin
            Result.AssignFromLayout(ALayout, AParams);
            Result.UpdateArea(dckFieldChanged);
            vAlreadyAssigned := True;
          end
          else
            vView.CleanView;
        end
        else
          Result := DoCreateChildArea(ALayout, vView);
      end
      // Иначе область или редактор скрыты по соображениям безопасности ->>> пересмотреть
      // Это плейсхолдер, или же неправильно указано наименование поля
      else
        Result := DoCreateChildArea(ALayout, FView);
    end;
  end;

  if Assigned(Result) then
  begin
    AddArea(Result);
    if not vAlreadyAssigned then
      Result.AssignFromLayout(ALayout, AParams);
    vLayout := Result.QueryParameter('layout');
    vDefaultViewName := Result.QueryParameter('view');
    if (vLayout <> '') or (vDefaultViewName <> '') then
    begin
      if Pos('WorkArea', Result.UId) = 1 then
      begin
        if vDefaultViewName <> '' then
          vDefaultView := FUIBuilder.RootView.BuildView(vDefaultViewName)
        else
          vDefaultView := nil;

        FUIBuilder.Navigate(vDefaultView, Result.UId, vLayout, Result.QueryParameter('Options'), nil, nil, Result.QueryParameter('Caption'));
      end
      else
        FUIBuilder.ApplyLayout(Result, FView, vLayout, vQuery)
    end
    else if not vAlreadyAssigned then //#Check!
      FUIBuilder.CreateChildAreas(Result, ALayout, vQuery);
  end
  else
    ALayout.Free;
end;

function TUIArea.CreateChildLayoutedArea(const ALayout: TLayout; const AView: TView;
  const AChildLayoutName: string; const AParams: string): TUIArea;
var
  vChildLayoutName: string;
  vParams: TStrings;
begin
  Result := DoCreateChildArea(ALayout, AView, AParams);
  Result.SetParent(Self);

  vChildLayoutName := AView.QueryParameter('layout');
  if (Trim(vChildLayoutName) = '') and (AParams <> '') then
  begin
    vParams := CreateDelimitedList(AParams, '&');
    try
      vChildLayoutName := vParams.Values['childlayout'];
    finally
      FreeAndNil(vParams);
    end;
  end;

  if Trim(vChildLayoutName) = '' then
    vChildLayoutName := AChildLayoutName;

  FUIBuilder.ApplyLayout(Result, AView, vChildLayoutName, AParams);
end;

destructor TUIArea.Destroy;
begin
  FreeAndNil(FCreateParams);
  FreeAndNil(FNativeControl);
  if not (FLayout is TNavigationItem) then
    FreeAndNil(FLayout);
  //if Assigned(FLayout) and FLayout.IsOwner then
  //  FLayout.Control.Free;
  //FLayout := nil;
  inherited Destroy;
end;

procedure TUIArea.DisableContent;
var
  vArea: TUIArea;
begin
  DoDisableContent;
  for vArea in FAreas do
    vArea.DisableContent;
end;

procedure TUIArea.DM_ViewChanged(var AMessage: TViewChangedMessage);
begin
  if not Assigned(FView) then
    Exit;

  if FView.DefinitionKind = dkDomain then
    Exit;
  if FView <> AMessage.View then
    Exit;
  // Закомментировано из-за контрола TEntityFieldListEditor (на поля подписывается общий контрол)
  //Assert(FView = AMessage.View, 'Sender and UI view are mismatch');
  try
    UpdateArea(AMessage.Kind, TEntity(AMessage.Parameter));
  except
    on E: Exception do raise;
  end;
  if FView.DefinitionKind = dkAction then
  begin
    if TDefinition(FView.Definition).HasFlag(ccInstantExecution) then
      ExecuteUIAction(FView);
  end;
end;

procedure TUIArea.DoActivate(const AUrlParams: string);
begin
  FNativeControl.DoActivate(AUrlParams);
end;

procedure TUIArea.DoAfterChildAreasCreated;
begin
end;

function TUIArea.DoCreateChildAction(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
var
  vStyleName: string;
begin
  vStyleName := GetUrlParam(AParams, 'ViewStyle'{'view'});

  Result := TPresenter(FUIBuilder.Presenter).CreateActionArea(Self, ALayout, AView, vStyleName, AParams);
end;

function TUIArea.DoCreateChildEditor(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
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

function TUIArea.DoCreateChildList(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
var
  vStyleName: string;
begin
  vStyleName := GetUrlParam(AParams, 'view');
  Result := TPresenter(FUIBuilder.Presenter).CreateCollectionArea(Self, ALayout, AView, vStyleName, AParams);
end;

function TUIArea.DoCreateChildNavigation(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
begin
  Result := nil;
end;

function TUIArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := nil;
end;

procedure TUIArea.DoDisableContent;
begin
end;

procedure TUIArea.DoExecuteUIAction(const AView: TView);
begin
end;

procedure TUIArea.DoOnExit(Sender: TObject);
begin
end;

procedure TUIArea.EndUpdate;
begin
  if ParentInUpdate then Exit;
  
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      FNativeControl.DoEndUpdate;
  end;
end;

procedure TUIArea.ExecuteUIAction(const AView: TView);
var
  vRefArea: TUIArea;
  vCurArea: TUIArea;
  vParentHolder: TObject;
  vParentObject: TObject;
  vIsSlave: Boolean;

  vInteractor: TInteractor;
  vAction: TActionDef;
  vParams: TEntity;
  vNeedClearParams: Boolean;
  vNeedShowParams: Boolean;
  vVisibleFieldCount: Integer;
begin
  if AView.UIContext <> '' then
  begin
    vCurArea := Parent;
    repeat
      vRefArea := vCurArea.AreaById(AView.UIContext);
      vCurArea := vCurArea.Parent;
    until Assigned(vRefArea) or not Assigned(vCurArea);

    if Assigned(vRefArea) then
    begin
      // Ввести параметры
      vInteractor := TInteractor(AView.Interactor);
      vAction := TActionDef(AView.Definition);
      vParams := TEntity(AView.DomainObject);

      vNeedClearParams := False;
      try
        if Assigned(vParams) then
        begin
          vVisibleFieldCount := vParams.VisibleFieldCount(vInteractor.Session);
          vNeedShowParams := (vVisibleFieldCount > 0) and vAction.HasFlag(ccAlwaysShowParameters);
          if not vParams.IsValid or vNeedShowParams then
          begin
            vNeedClearParams := not vParams.IsValid and not vNeedShowParams;
            if not vInteractor.AtomicEditParams(AView) then
              Exit;
          end;
        end;

        vRefArea.DoExecuteUIAction(AView);
      finally
        if vNeedClearParams then
          vParams.ResetToDefault(TUserSession(AView.Session).NullHolder);
      end;
    end;
  end
  else begin
    vIsSlave := (AView.Name = 'Save') or SameText(QueryParameter('place'), 'embedded');
    if not vIsSlave then
    begin
      vParentObject := AView.ParentDomainObject;

      ///// TODO Что-то здесь неправильно: можно
      ///     1. Можно развернуть через Definition у View
      ///     2. Почему проверяется RelationPower на rpStrong?
      if Assigned(vParentObject) and (vParentObject is TEntityList) then
        vIsSlave := (TEntityList(vParentObject).FillerKind = lfkList) and
          (TListField(TEntityList(vParentObject).Filler).RelationPower = rpStrong)
      else if Assigned(AView.Parent) then
      begin
        vParentObject := AView.Parent.ParentDomainObject;
        if Assigned(vParentObject) and (vParentObject is TEntityList) then
          vIsSlave := (TEntityList(vParentObject).FillerKind = lfkList) and
            (TListField(TEntityList(vParentObject).Filler).RelationPower = rpStrong);
      end;
    end;

    if vIsSlave then
      vParentHolder := GetHolder
    else
      vParentHolder := nil;

    AView.ExecuteAction(vParentHolder);
  end;
end;

function TUIArea.GetArea(const AIndex: Integer): TUIArea;
begin
  Result := TUIArea(FAreas[AIndex]);
end;

function TUIArea.GetAreaByView(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
var
  vFieldDef: TFieldDef;
  vParams: TStrings;
begin
  Result := nil;

  vParams := CreateDelimitedList(AParams, '&');
  try
    case AView.DefinitionKind of
      dkDomain: {do nothings};
      dkEntity:
        if SameText(vParams.Values['ViewType'], 'Action') then
          Result := DoCreateChildAction(ALayout, AView, AParams)
        else if vParams.Values['view'] <> '' then
          Result := DoCreateChildEditor(ALayout, AView, AParams)
        else
          Result := DoCreateChildArea(ALayout, AView, AParams);
      dkCollection:
        begin
          if SameText(vParams.Values['ViewType'], 'Action') then
            Result := DoCreateChildAction(ALayout, AView, AParams)
          else
            Result := CreateChildLayoutedArea(ALayout, AView, 'Collection', AParams);
        end;
      dkAction: Result := DoCreateChildAction(ALayout, AView, AParams);
      dkObjectField, dkSimpleField:
        begin
          vFieldDef := TFieldDef(AView.Definition);
          if vFieldDef.Name = 'ID' then
          begin
            Result := DoCreateChildEditor(ALayout, AView, AParams);
          end
          else begin
            if not TInteractor(Interactor).NeedSkipField(nil, vFieldDef, False)
              or (Assigned(AView.Parent) and (AView.Parent.DefinitionKind = dkAction))
            then begin
              if vParams.Values['childlayout'] <> '' then
                Result := CreateChildLayoutedArea(ALayout, AView, vParams.Values['childlayout'], AParams)
              else
                Result := DoCreateChildEditor(ALayout, AView, AParams);
            end;
          end;
        end;
      dkComplexField:
        Result := CreateChildLayoutedArea(ALayout, AView, TComplexFieldDef(AView.Definition).ObjectKindName, AParams);
      dkListField:
        begin
          if vParams.Values['view'] <> '' then
            Result := DoCreateChildEditor(ALayout, AView, AParams)
          else begin
            Result := CreateChildLayoutedArea(ALayout, AView, 'DefaultList', AParams);
            Result.CreateCaption(TFieldDef(AView.Definition));
          end;
        end;
    end;
  finally
    FreeAndNil(vParams);
  end;
end;

function TUIArea.GetTranslation(const ADefinition: TDefinition; const ATranslationPart: TTranslationPart): string;
begin
  Result := FUIBuilder.GetTranslation(ADefinition, ATranslationPart);
end;

function TUIArea.LessThanUIState(const ADefinition: TDefinition; const ASession: TObject;
  const AState: TViewState): Boolean;
var
  vSecuredState: TViewState;
begin
  if not Assigned(ASession) then
    Result := ADefinition.UIState <= AState
  else begin
    vSecuredState := ADefinition.UIState and TUserSession(ASession).GetUIState(ADefinition.Name, nil);
    Result := vSecuredState <= AState;
  end;
end;

function TUIArea.NativeControlClass: TNativeControlClass;
begin
  Result := TPresenter(Presenter).NativeControlClass;
end;

function TUIArea.GetCount: Integer;
begin
  Result := FAreas.Count;
end;

function TUIArea.GetDomain: TObject;
begin
  Result := FView.Domain;
end;

function TUIArea.GetFieldTranslation(const AFieldDef: TFieldDef; const ATranslationPart: TTranslationPart): string;
begin
  Result := FUIBuilder.GetFieldTranslation(AFieldDef, ATranslationPart);
end;

function TUIArea.GetHolder: TObject;
begin
  if Assigned(FView) and (FView.DefinitionKind = dkDomain) and not Assigned(Parent) then
    Result := FHolder
  else if not Assigned(FHolder) and Assigned(Parent) then
    Result := FParent.GetHolder
  else
    Result := FHolder;
end;

function TUIArea.GetImageID(const AImageID: Integer): Integer;
begin
  Result := FUIBuilder.GetImageID(AImageID);
end;

function TUIArea.GetInnerControl: TObject;
begin
  Result := FNativeControl.Control;
end;

function TUIArea.GetInteractor: TObject;
begin
  if Assigned(FUIBuilder) then
    Result := FUIBuilder.FInteractor
  else
    Result := nil;
end;

function TUIArea.GetName: string;
begin
  Result := FNativeControl.GetName;
end;

function TUIArea.GetPresenter: TObject;
begin
  Result := FUIBuilder.FPresenter;
end;

procedure TUIArea.OnAreaClick(Sender: TObject);
var
  vArea: TUIArea;
begin
  vArea := FNativeControl.AreaFromSender(Sender);
  if Assigned(vArea) then
    ProcessAreaClick(vArea);
end;

procedure TUIArea.OnEnter(Sender: TObject);
var
  vArea: TUIArea;
begin
  vArea := FNativeControl.AreaFromSender(Sender);
  if Assigned(FUIBuilder) then
  begin
    FUIBuilder.ActiveArea := vArea;
    FUIBuilder.PrintHierarchy;
  end;
end;

procedure TUIArea.OnExit(Sender: TObject);
begin
  if Assigned(FUIBuilder) then
    FUIBuilder.ActiveArea := nil;

  DoOnExit(Sender);
end;

function TUIArea.ParentInUpdate: Boolean;
begin
  Result := Assigned(FParent) and (FParent.ParentInUpdate or (FParent.FUpdateCount > 0));
end;

procedure TUIArea.ProcessAreaClick(const AArea: TUIArea);
var
  vView: TView;
  vNavItem: TNavigationItem;
  vWorkArea: string;
  vLayoutName: string;
  vHolder: TChangeHolder;
  vDefaultWorkArea: string;
  vOwner: TLayout;
begin
  FUIBuilder.LastArea := AArea;
  if not Assigned(AArea) then
    Exit;

  vView := AArea.View;
  if not Assigned(vView) then
    Exit;

  vNavItem := TNavigationItem(AArea.Layout);
  Assert(Assigned(vNavItem), 'Ууупс!');

  vOwner := vNavItem.Owner;
  if Assigned(vOwner) then
    vDefaultWorkArea := vOwner.ExtractString('contentworkarea', 'WorkArea')
  else
    vDefaultWorkArea := 'WorkArea';

  if vView.DefinitionKind = dkAction then
    AArea.ExecuteUIAction(vView)
  else if vView.DefinitionKind = dkCollection then
  begin
    vWorkArea := vNavItem.ContentWorkArea;
    if vWorkArea = '' then
      vWorkArea := vDefaultWorkArea;
    vLayoutName := vNavItem.ContentLayout;
    if vLayoutName = '' then
      vLayoutName := 'Collection';

    FUIBuilder.Navigate(vView, vWorkArea, vLayoutName, 'operation=opencollection',
      nil, nil, vNavItem.ContentCaption);
  end
  else if vView.DefinitionKind in [dkEntity, dkObjectField] then
  begin
    vWorkArea := vNavItem.ContentWorkArea;
    if vWorkArea = '' then
      vWorkArea := vDefaultWorkArea;

    vHolder := TUserSession(FView.Session).Edit(nil);
    FUIBuilder.Navigate(vView, vWorkArea, vNavItem.ContentLayout, 'operation=slap',
      vHolder, nil, vNavItem.ContentCaption);
  end;
end;

function TUIArea.QueryParameter(const AName: string; const ADefaultValue: string = ''): string;
var
  vName: string;
begin
  vName := Trim(AName);
  if Assigned(FParams) and (FParams.IndexOfName(vName) >= 0) then
    Result := FParams.Values[vName]
  else
    Result := ADefaultValue;
end;

procedure TUIArea.RefillArea(const AKind: Word);
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
        else if QueryParameter('childLayout') <> '' then  //todo: перенести это в GetLayoutName
          FUIBuilder.ApplyLayout(Self, FView, Trim(QueryParameter('childLayout')), '')
        else
          FUIBuilder.ApplyLayout(Self, FView, vEntity.Definition.Name + 'EditForm', '');
    end;
  end
  else if FView.DefinitionKind = dkCollection then
  begin
  end;
end;

procedure TUIArea.Release;
begin
  DisableContent;
  ClearContent;
  DisposeOf;
end;

procedure TUIArea.RemoveArea(var AArea: TUIArea);
var
  vIndex: Integer;
begin
  vIndex := FAreas.IndexOf(AArea);
  if vIndex >= 0 then
    FAreas.Delete(vIndex);
  AArea.Release;
end;

procedure TUIArea.SaveLayoutToFile(const AFileName: string);
begin
end;

procedure TUIArea.SetBounds(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  FNativeControl.PlaceIntoBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TUIArea.SetControl(const AControl: TObject);
begin
  FNativeControl.Control := AControl;
end;

procedure TUIArea.SetHolder(const AHolder: TObject);
begin
  FHolder := AHolder;
end;

procedure TUIArea.SetLayout(const Value: TLayout);
begin
  if FLayout = Value then
    Exit;

  if not Assigned(FLayout) then
    FLayout := Value
  else
    FLayout.ContentLayout := Value;
end;

procedure TUIArea.SetParent(const Value: TUIArea);
begin
  FParent := Value;
  FNativeControl.Parent := Value;
end;

procedure TUIArea.SetPopupArea(const APopupArea: TUIArea);
begin
end;

procedure TUIArea.SetView(const Value: TView);
begin
  if FView = Value then
    Exit;

  TryUnsubscribeView;
  FView := Value;
  TrySubscribeView;
end;

function TUIArea.TextHierarchy(const AIndent: string): string;
var
  i: Integer;
  vViewName: string;
  vModifier: string;
  vIndex: string;
  vClassName: string;
begin
  if FView.FullName = '' then
    vViewName := ''
  else
    vViewName := ', view: ' + FView.FullName;

  vModifier := '';
  if Assigned(FHolder) then
    vModifier := '[+]';
  if Self = FUIBuilder.CurrentArea then
    vModifier := vModifier + 'C';
  if Self = FUIBuilder.LastArea then
    vModifier := vModifier + 'L';
  if Self = FUIBuilder.ActiveArea then
    vModifier := vModifier + 'A';

  if vModifier <> '' then
    vModifier := '>>' + vModifier + ': ';

  if Assigned(FLayout) then
    vIndex := '/' + IntToStr(FLayout._Index) + '/ '
  else
    vIndex := '// ';

  if Assigned(FNativeControl.Control) then
    vClassName := FNativeControl.Control.ClassName
  else
    vClassName := '???';

  Result := AIndent + vModifier + vIndex + GetName + ':' + Self.ClassName + ':' + vClassName + vViewName +
    FNativeControl.DoGetDescription + #13#10;
  for i := 0 to FAreas.Count - 1 do
    Result := Result + GetArea(i).TextHierarchy(AIndent + '    ');
end;

function TUIArea.TryCreatePopupArea(const ALayout: TLayout): TUIArea;
begin
  Result := nil;
end;

procedure TUIArea.TrySubscribeView;
begin
  if Assigned(FView) then
    FView.AddListener(Self);
end;

procedure TUIArea.TryUnsubscribeView;
begin
  if Assigned(FView) then
    FView.RemoveListener(Self);
end;

procedure TUIArea.UnbindContent;
begin
  FNativeControl.UnbindContent;
end;

procedure TUIArea.UpdateArea(const AKind: Word; const AParameter: TEntity = nil);
begin
end;
(*
{ TNavigationArea }

constructor TNavigationArea.Create(const AParent: TUIArea; const AView: TView; const AId: string;
  const AIsService: Boolean; const AControl: TObject; const ALayout: TLayout; const AParams: string);
begin
  inherited Create(AParent, AView, AId, AIsService, AControl, ALayout, AParams);
  FParams := AParams;
  Assert(Assigned(ALayout) and Assigned(ALayout.Menu), 'Для навигационной области не задано меню');
  FInitialMenu := ALayout.Menu;
end;

function TNavigationArea.CreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const AView: TView;
  const ALevel: Integer): TObject;
var
  vDefinition: TDefinition;
  vEntity: TEntity;
  vCaption, vHint: string;
  vImageID: Integer;
begin
  vCaption := ANavItem.Caption;
  vHint := ANavItem.Hint;
  vImageID := ANavItem.ImageID;

  if AView.DefinitionKind in [dkAction, dkCollection] then
  begin
    vDefinition := TDefinition(AView.Definition);
    if vCaption = '' then
      vCaption := GetTranslation(vDefinition);
    if vHint = '' then
      vHint := vCaption;
    if vImageID < 0 then
      vImageID := vDefinition._ImageID;

    Result := DoCreateItem(AParentObj, ANavItem, ALevel, vCaption, vHint, GetImageID(vImageID));
  end
  else if (AView.DefinitionKind in [dkEntity, dkObjectField]) and (AView.DomainObject is TEntity) then
  begin
    vEntity := AView.DomainObject as TEntity;
    if (vCaption = '') and Assigned(vEntity) then
      vCaption := SafeDisplayName(vEntity, 'NULL');
    if vHint = '' then
      vHint := vCaption;

    Result := DoCreateItem(AParentObj, ANavItem, ALevel, vCaption, vCaption, GetImageID(vImageID));
  end
  else
    Result := nil;
end;

destructor TNavigationArea.Destroy;
begin
  FInitialMenu := nil;
  inherited Destroy;
end;

procedure TNavigationArea.DoAfterCreate(const AInteractor: TObject);
begin
end;

procedure TNavigationArea.DoProcessChilds(const AParentArea: TUIArea; const AView: TView;
  const ANavItem: TNavigationItem; const ALevel: Integer);
var
  i: Integer;
  vNavItem: TNavigationItem;
  vParentObj: TObject;
  vViewPath: TStrings;

  procedure CreateGroupNode(const ACurrentItem: TNavigationItem; const ACurrentView: TView);
  var
    vControl: TObject;
    vGroupArea: TUIArea;
    vDefinition: TDefinition;
    vDefinitions: TList<TDefinition>;
  begin
    vControl := DoCreateItem(vParentObj, ACurrentItem, ALevel, ACurrentItem.Caption, ACurrentItem.Hint,
      GetImageID(ACurrentItem.ImageID));
    if not Assigned(vControl) then
      Exit;

    vGroupArea := MainUIClass.Create(AParentArea, ACurrentView, '', False, vControl, nil, '');
    AParentArea.AddArea(vGroupArea);

    if ACurrentItem.Id = 'Libraries' then
    begin
      vDefinitions := TList<TDefinition>.Create;
      try
        TConfiguration(TInteractor(Interactor).Configuration).Definitions.DefinitionsByKind(vDefinitions, clkLibrary);
        vDefinitions.Sort(TComparer<TDefinition>.Construct(function(const Left, Right: TDefinition): Integer
          begin
            Result := CompareText(Left._Caption, Right._Caption);
          end));
        for vDefinition in vDefinitions do
          if not LessThanUIState(vDefinition, TInteractor(Interactor).Session, vsReadOnly)
            and not vDefinition.HasFlag(ccNotSave) and not vDefinition.HasFlag(ccHideInMenu)
          then begin
            //#Check Возможно, лучше явно отстроить эти области без изменения vNavItem
            ACurrentItem.Add(vDefinition.Name);
          end;
      finally
        FreeAndNil(vDefinitions);
      end;
    end;

    DoProcessChilds(vGroupArea, ACurrentView, ACurrentItem, ALevel + 1);
  end;

  procedure CreateNavigationNode(const ACurrentItem: TNavigationItem; const ACurrentView: TView);
  var
    vControl: TObject;
    vNavArea: TUIArea;
  begin
    vControl := CreateItem(vParentObj, ACurrentItem, ACurrentView, ALevel);
    if not Assigned(vControl) then
      Exit;

    vNavArea := MainUIClass.Create(AParentArea, ACurrentView, '', False, vControl, nil, '');
    AParentArea.AddArea(vNavArea);

    DoProcessChilds(vNavArea, ACurrentView, ACurrentItem, ALevel + 1);
  end;

  procedure ProcessNavigationNode(const ACurrentItem: TNavigationItem; const ACurrentView: TView; const AViewPath: TStrings);
  var
    vViewName: string;
    vNextView: TView;
    vEntityList: TEntityList;
    vEntity: TEntity;
  begin
    if AViewPath.Count = 0 then
    begin
      CreateNavigationNode(ACurrentItem, ACurrentView);
      Exit;
    end;

    vViewName := Trim(AViewPath[0]);
    AViewPath.Delete(0);
    try
      if vViewName = '' then
        ProcessNavigationNode(ACurrentItem, ACurrentView, AViewPath)
      else if vViewName = '-' then
        CreateGroupNode(ACurrentItem, ACurrentView)
      else if Pos('@', vViewName) = 1 then
      begin
        // Нужно итерироваться
        if vViewName = '@' then
        begin
          if ACurrentView.DefinitionKind in [dkCollection, dkListField] then
          begin
            vEntityList := TEntityList(ACurrentView.DomainObject);
            for vEntity in vEntityList do
            begin
              vNextView := TInteractor(ACurrentView.Interactor).GetViewOfEntity(vEntity);
              ProcessNavigationNode(ACurrentItem, vNextView, AViewPath)
            end;
          end
          else
            Assert(False, 'Тип не поддерживается для итераций в меню');
        end
        else
          CreateGroupNode(ACurrentItem, ACurrentView);
      end
      else begin
        vNextView := ACurrentView.BuildView(vViewName);
        ProcessNavigationNode(ACurrentItem, vNextView, AViewPath);
      end;
    finally
      AViewPath.Insert(0, vViewName);
    end;
  end;
begin
  for i := 0 to ANavItem.Count - 1 do
  begin
    vNavItem := ANavItem[i];

    if AParentArea = Self then
      vParentObj := nil
    else
      vParentObj := AParentArea.Control;

    vViewPath := CreateDelimitedList(vNavItem.ViewName, '/');
    try
      ProcessNavigationNode(vNavItem, AView, vViewPath);
    finally
      FreeAndNil(vViewPath);
    end;
  end;
end;

procedure TNavigationArea.ProcessChilds;
begin
  DoProcessChilds(Self, FView, FInitialMenu, 0);
  DoAfterCreate(FView.Interactor);
end;

function TNavigationArea.TryCreatePopupArea(const ALayout: TLayout): TUIArea;
begin
  Result := nil;
end; *)

{ TNativeControl }

function TNativeControl.AreaFromSender(const ASender: TObject): TUIArea;
begin
  Result := nil;
end;

constructor TNativeControl.Create(const AOwner: TUIArea; const AControl: TObject);
begin
  inherited Create;
  FOwner := AOwner;
  FParent := AOwner.Parent;
  FControl := AControl;
  FInteractor := AOwner.Interactor;
  FUIBuilder := AOwner.UIBuilder;
  FIsForm := False;
  FIsAutoReleased := False;
end;

destructor TNativeControl.Destroy;
begin
  FOwner := nil;
  FParent := nil;
  FControl := nil;
  FInteractor := nil;
  FUIBuilder := nil;
  inherited Destroy;
end;

procedure TNativeControl.DoActivate(const AUrlParams: string);
begin
end;

procedure TNativeControl.DoBeginUpdate;
begin
end;

procedure TNativeControl.DoClose(const AModalResult: Integer);
begin
end;

procedure TNativeControl.DoEndUpdate;
begin
end;

function TNativeControl.DoGetDescription: string;
begin
  Result := '';
end;

function TNativeControl.GetName: string;
begin
  Result := '[area]';
end;

procedure TNativeControl.PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer);
begin
end;

procedure TNativeControl.SetControl(const AControl: TObject);
begin
  FControl := AControl;
end;

procedure TNativeControl.SetParent(const AParent: TUIArea);
begin
  FParent := AParent;
end;

procedure TNativeControl.SetViewState(const AValue: TViewState);
begin
end;

procedure TNativeControl.UnbindContent;
begin
  if FIsAutoReleased then
    FControl := nil;
end;

end.
