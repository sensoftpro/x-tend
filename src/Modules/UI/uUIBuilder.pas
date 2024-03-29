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

unit uUIBuilder;

interface

uses
  Classes, Types, Generics.Collections, Generics.Defaults, UITypes, SysUtils,
  uConsts, uView, uDefinition, uEntity, uSession, uScene, uLayout;

type
  TLabelPosition = (lpTop, lpLeft);

  TCloseProc = reference to procedure(const AResult: TDialogResult);
  TCloseTextProc = reference to procedure(const AResult: TDialogResult; const AText: string);

  TUIBuilder = class;
  TUIArea = class;

  TNativeControl = class
  private
    FRegisterChanges: Boolean;
    function GetDescription: string;
  protected
    [Weak] FOwner: TUIArea;
    [Weak] FParent: TUIArea;
    [Weak] FLayout: TLayout;
    [Weak] FView: TView;
    [Weak] FCaption: TObject;
    [Weak] FPresenter: TObject;
    [Weak] FInteractor: TObject;
    [Weak] FDomain: TObject;
    [Weak] FUIBuilder: TUIBuilder;
    [Weak] FCreateParams: TStrings;
    [Weak] FFieldDef: TFieldDef;
    FInternalParams: string;

    FParams: string;
    FIsForm: Boolean;
    FIsAutoReleased: Boolean;
    FShowCaption: Boolean;
    FLabelPosition: TLabelPosition;
    FNeedCreateCaption: Boolean;

    function IndexOfSender(const ASender: TObject): Integer; virtual;
    function AreaFromSender(const ASender: TObject): TUIArea; virtual;

    procedure DoActivate(const AUrlParams: string); virtual;
    procedure DoClose(const AModalResult: Integer); virtual;
    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate; virtual;
    procedure DoRegisterChanges(const Value: Boolean); virtual;

    function DoCreateCaption(const AParent: TUIArea; const ACaption, AHint: string): TObject; virtual;
    procedure DoAfterSetParent(const AParent: TUIArea); virtual;
    procedure DoBeforeFreeControl; virtual;
    procedure FillEditor; virtual;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); virtual;
    procedure DoOnChange; virtual;
    procedure FocusedChanged(const AFocused: Boolean); virtual;
    procedure DoDisableContent; virtual;
    procedure DoDeinit; virtual;
    procedure DoOnExit(Sender: TObject); virtual;
    function GetNewValue: Variant; virtual;
    procedure DoExecuteUIAction(const AView: TView); virtual;

    procedure AssignFromLayout(const ALayout: TLayout; const AParams: string); virtual;

    function GetControlInfo: string; virtual;
    procedure SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl); virtual;
    procedure SetParent(const AParent: TUIArea); virtual;
    function GetFocused: Boolean; virtual;
    procedure SetFocused(const Value: Boolean); virtual;
    function GetBounds: TRect; virtual;
    procedure SetBounds(const Value: TRect); virtual;
    function GetClientRect: TRect; virtual;
    function GetViewState: TViewState; virtual;
    procedure SetViewState(const AViewState: TViewState); virtual;
    function GetTabOrder: Integer; virtual;
    procedure SetTabOrder(const ATabOrder: Integer); virtual;
    function GetActiveChildArea: TUIArea; virtual;
    procedure SetActiveChildArea(const AArea: TUIArea); virtual;
    function GetModalResult: TModalResult; virtual;
    procedure SetModalResult(const AModalResult: TModalResult); virtual;
    function GetWindowState: TWindowState; virtual;
    procedure SetWindowState(const AWindowState: TWindowState); virtual;
    procedure SetAlignment(const AAlignment: TAlignment); virtual;

    procedure PlaceLabel; virtual;
    procedure UpdateCaptionVisibility; virtual;

    procedure RefillArea(const AKind: Word); virtual;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); virtual;

    procedure UnbindContent(const AForceUnbind: Boolean = False); virtual;
  protected
    function GetFieldTranslation(const AFieldDef: TFieldDef; const ATranslationPart: TTranslationPart = tpCaption): string;
    function GetTranslation(const ADefinition: TDefinition; const ATranslationPart: TTranslationPart = tpCaption): string;
    function GetImageIndex(const AImageID: string): Integer;
    procedure SetFieldValue(const AValue: Variant);
    procedure SetFieldEntity(const AEntity: TEntity);
    procedure SetFieldStream(const AStream: TStream);
    function GetDisplayFormat(const AFieldDef: TFieldDef; const AEntity: TEntity): string;
    function GetFormat: string;
    function GetRealControl(const AArea: TUIArea): TObject;
    function ParentInUpdate: Boolean;
  public
    constructor Create(const AOwner: TUIArea; const AParams: string = ''); virtual;
    destructor Destroy; override;

    procedure CreateCaption(const AFieldDef: TFieldDef);
    procedure CreateContent(const AContent: TObject); virtual;
    procedure AfterSetParent(const AParent: TUIArea);

    property Domain: TObject read FDomain;
    property Owner: TUIArea read FOwner;
    property Parent: TUIArea read FParent write SetParent;
    property View: TView read FView;
    property Focused: Boolean read GetFocused write SetFocused;
    property Bounds: TRect read GetBounds write SetBounds;
    property ClientRect: TRect read GetClientRect;
    property ViewState: TViewState read GetViewState write SetViewState;
    property TabOrder: Integer read GetTabOrder write SetTabOrder;
    property Description: string read GetDescription;
    property ActiveChildArea: TUIArea read GetActiveChildArea write SetActiveChildArea;
    property ModalResult: TModalResult read GetModalResult write SetModalResult;
    property WindowState: TWindowState read GetWindowState write SetWindowState;
    property RegisterChanges: Boolean read FRegisterChanges write DoRegisterChanges;

    property IsForm: Boolean read FIsForm;
    property ShowCaption: Boolean read FShowCaption;
  end;

  TNativeControlClass = class of TNativeControl;

  TNativeControlHolder = class(TNativeControl)
  protected
    [Weak] FControl: TObject;

    procedure SetControl(const AControl: TObject); virtual;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; virtual;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; virtual;

    function GetControlInfo: string; override;
    procedure UnbindContent(const AForceUnbind: Boolean); override;
  public
    procedure CreateContent(const AContent: TObject); override;
    function CreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject;

    property Control: TObject read FControl;
  end;

  TDomainSceneObject = class(TSceneObject)
  protected
    FOwner: TUIArea;
    procedure UpdateBinding(const ABinding: TObject); virtual;
    procedure ExecuteUIAction(const AArea: TUIArea; const AView: TView); virtual;
  public
    constructor Create(const AOwner: TUIArea; const AScene: TScene); virtual;
    destructor Destroy; override;
  end;

  TDomainSceneObjectClass = class of TDomainSceneObject;

  TUIArea = class
  private
    [Weak] FParent: TUIArea;
    [Weak] FHolder: TObject;
    [Weak] FInteractor: TObject;
    [Weak] FRootView: TView;
    [Weak] FRootArea: TUIArea;
    [Weak] FUIBuilder: TUIBuilder;
    [Weak] FView: TView;
    [Weak] FSession: TUserSession;

    FAreas: TList<TUIArea>;
    FId: string;
    FNativeControl: TNativeControl;
    FLayout: TLayout;
    FInitialMenu: TNavigationItem;
    FFieldDef: TFieldDef;
    FDefinitionName: string;

    FInternalParams: string;
    FCreateParams: TStrings;
    FParams: TStrings;
    FIsService: Boolean;
    FUpdateCount: Integer;
    FOnClose: TCloseProc;
    FIsClosing: Boolean;
    function GetArea(const AIndex: Integer): TUIArea;
    function GetCount: Integer;
    function GetPresenter: TObject;
    function GetDomain: TObject;
    function GetHolder: TObject;
    procedure TrySubscribeView;
    procedure TryUnsubscribeView;
    procedure DisableContent;
    procedure SetView(const Value: TView);
    procedure SetLayout(const Value: TLayout);
    procedure AfterChildAreasCreated;
    function ParentInUpdate: Boolean;
    function GetTabOrder: Integer;
    function GetTabStop: Boolean;
    function GetActiveChildArea: TUIArea;
    function GetName: string;
    function GetRegisterChanges: Boolean;
    procedure SetRegisterChanges(const Value: Boolean);
  private
    procedure DoProcessChilds(const AParent: TUIArea; const AView: TView; const ANavItem: TNavigationItem);
    function DoCreateChildArea(const ALayout: TLayout; const AView: TView; const AParams: string = ''; const AOnClose: TCloseProc = nil): TUIArea;
    function DoCreateChildAction(const ALayout: TLayout; const AView: TView; const AParams: string = ''): TUIArea;
    function DoCreateChildList(const ALayout: TLayout; const AView: TView; const AParams: string = ''): TUIArea;
    function DoCreateChildNavigation(const ALayout: TLayout; const AView: TView; const AParams: string = ''): TUIArea;
    function DoCreateChildEditor(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
    function CreateChildLayoutedArea(const ALayout: TLayout; const AView: TView;
      const AChildLayoutName: string; const AParams: string): TUIArea;

    procedure DoExecuteUIAction(const AView: TView);
    procedure SetParent(const Value: TUIArea);

    function CanChangeArea: Boolean;
    function GetAreaByView(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
    procedure ClearContent;
  protected
    procedure DM_ViewChanged(var AMessage: TViewChangedMessage); message DM_VIEW_CHANGED;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const ALayout: TLayout; const AParams: string = '');
    destructor Destroy; override;

    // Полная очистка и удаление
    procedure Clear;
    procedure Release;
    procedure ProcessAreaClick(const AArea: TUIArea);
    function CreateChildArea(const AChildView: TView; const ALayout: TLayout; const AParams: string; const AOnClose: TCloseProc = nil): TUIArea;

    procedure AddArea(const AArea: TUIArea);
    procedure RemoveArea(var AArea: TUIArea);
    procedure Close(const AModalResult: Integer = mrOk);
    function TextHierarchy(const AIndent: string = ''): string;
    function AreaById(const AId: string; const ARecoursive: Boolean = True): TUIArea;
    function AreaByView(const AView: TView): TUIArea;
    function Contains(const AArea: TUIArea): Boolean;
    function ContainView(const AView: TView): Boolean;
    procedure ExecuteUIAction(const AView: TView);
    procedure UnbindContent(const AForceUnbind: Boolean = False);
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil);

    function LessThanUIState(const ADefinition: TDefinition; const ASession: TObject; const AState: TViewState): Boolean;
    function GetFieldTranslation(const AFieldDef: TFieldDef; const ATranslationPart: TTranslationPart = tpCaption): string;
    function GetTranslation(const ADefinition: TDefinition; const ATranslationPart: TTranslationPart = tpCaption): string;
    function GetImageIndex(const AImageID: string): Integer;

    // For interactor
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AssignFromLayout(const ALayout: TLayout; const AParams: string);

    procedure Validate;
    procedure Deinit;
    procedure Activate(const AUrlParams: string);
    procedure OnEnter(Sender: TObject);
    procedure OnExit(Sender: TObject);
    procedure OnActionMenuSelected(Sender: TObject);
    procedure OnAreaClick(Sender: TObject);
    procedure OnChange(Sender: TObject);
    procedure ProcessChilds;

    procedure AddParams(const AParams: TStrings);
    function QueryParameter(const AName: string; const ADefaultValue: string = ''): string;
    procedure SetBounds(const ALeft, ATop, AWidth, AHeight: Integer);
    procedure SetHolder(const AHolder: TObject);

    property FieldDef: TFieldDef read FFieldDef;
    property DefinitionName: string read FDefinitionName;

    property NativeControl: TNativeControl read FNativeControl;
    property Count: Integer read GetCount;
    property Areas[const AIndex: Integer]: TUIArea read GetArea; default;
    property ActiveChildArea: TUIArea read GetActiveChildArea;
    property Parent: TUIArea read FParent;
    property Layout: TLayout read FLayout;
    property CreateParams: TStrings read FCreateParams;
    property InternalParams: string read FInternalParams;
    property RootView: TView read FRootView;
    property RootArea: TUIArea read FRootArea;
    property UIBuilder: TUIBuilder read FUIBuilder;
    property View: TView read FView;
    property Presenter: TObject read GetPresenter;
    property Interactor: TObject read FInteractor;
    property Domain: TObject read GetDomain;
    property Name: string read GetName;
    property Id: string read FId write FId;
    property Holder: TObject read GetHolder;
    property ThisHolder: TObject read FHolder;
    property TabOrder: Integer read GetTabOrder;
    property TabStop: Boolean read GetTabStop;
    property OnClose: TCloseProc read FOnClose write FOnClose;
    property IsClosing: Boolean read FIsClosing;
    property RegisterChanges: Boolean read GetRegisterChanges write SetRegisterChanges;
  end;

  TUIAreaComparer = class(TComparer<TUIArea>)
  public
    function Compare(const ALeft, ARight: TUIArea): Integer; override;
  end;

  TLayouts = class
  private
    FItems: TObjectDictionary<string,TLayout>;
    FNames: TDictionary<string,string>; // алиасы -> одно имя лэйаута
    [Weak] FUIBuilder: TUIBuilder;
    function MakeLayoutFromFile(const AFileName: string): TLayout;
    function MakeDefaultLayout(const AView: TView; const ALayoutName: string): TLayout;
  public
    constructor Create(const AUIBuilder: TUIBuilder);
    destructor Destroy; override;

    function ApplyAbsentLayout(const ASourceLayout: TLayout = nil): TLayout;
    function CreateSimpleLayout(const ALayoutKind: TLayoutKind): TLayout;
    function GetLayout(const ALayoutName: string; const AView: TView): TLayout;
  end;

  TUIBuilder = class
  private
    [Weak] FDomain: TObject;
    [Weak] FPresenter: TObject;
    FLayouts: TLayouts;
    FImageLists: TObjectDictionary<Integer, TObject>;
    FImageMap: TDictionary<string, Integer>;
    FIsMDIStyle: Boolean;

    function GetImages(const AResolution: Integer): TObject;

    function GetTranslation(const ADefinition: TDefinition; const ATranslationPart: TTranslationPart = tpCaption): string;
    function GetFieldTranslation(const AFieldDef: TFieldDef; const ATranslationPart: TTranslationPart = tpCaption): string;
    procedure GetLayoutName(const AEntity: TEntity; const AParams: string; var ALayoutName: string);
    procedure ScaleLayout(const ALayout: TLayout; const AScale: Single = 1.0);
  public
    constructor Create(const ADomain: TObject);
    destructor Destroy; override;

    procedure ApplyLayout(const AArea: TUIArea; const AView: TView; const ALayoutName: string; const AParams: string);
    procedure CreateChildAreas(const AArea: TUIArea; const AView: TView; const ALayout: TLayout; const AParams: string);

    procedure Navigate(const AView: TView; const AAreaName, ALayoutName: string;
      const AOptions: string = ''; const AChangeHolder: TObject = nil;
      const ACaption: string = ''; const AOnClose: TCloseProc = nil);

    procedure StoreImageIndex(const AImageID: string; const AImageIndex: Integer);
    function GetImageIndex(const AImageID: string): Integer;

    property Domain: TObject read FDomain;
    property Presenter: TObject read FPresenter write FPresenter;
    property Layouts: TLayouts read FLayouts;
    property IsMDIStyle: Boolean read FIsMDIStyle write FIsMDIStyle;
    property Images[const AResolution: Integer]: TObject read GetImages;
  end;

type
  TCanChangeFieldFunc = function(const AView: TView; const AEntity: TEntity; const AFieldName: string; const ANewValue: Variant): Boolean of object;

implementation

uses
  {DO NOT ADD VCL UNITS HERE (Controls, Forms...)}
  StrUtils, IOUtils, UIConsts, Variants, Math,
  uPlatform, uPresenter, uInteractor, uConfiguration, uDomain, uChangeManager,
  uUtils, uObjectField, uEntityList;

{ TLayouts }

constructor TLayouts.Create(const AUIBuilder: TUIBuilder);
begin
  FUIBuilder := AUIBuilder;
  FItems := TObjectDictionary<string,TLayout>.Create([doOwnsValues]);
  FNames := TDictionary<string,string>.Create;
end;

destructor TLayouts.Destroy;
var
  i: Integer;
  vArr: TArray<TLayout>;
begin
  vArr :=  FItems.Values.ToArray;
  for i := 0 to High(vArr) do
    vArr[i].ClearChilds;
  FreeAndNil(FItems);
  FreeAndNil(FNames);

  inherited;
end;

function TLayouts.ApplyAbsentLayout(const ASourceLayout: TLayout = nil): TLayout;
begin
  if not Assigned(ASourceLayout) then
    Result := CreateSimpleLayout(lkShape)
  else
    Result := ASourceLayout;

  Randomize;
  Result.Kind := lkShape;
  Result.ShowCaption := True;
  Result.Pen.Color := Cardinal($FF shl 24 + Random(256) shl 16 + Random(256) shl 8 + Random(256));
  Result.Pen.Width := 1;
  Result.Brush.Color := Cardinal($FF shl 24 + Random(256) shl 16 + Random(256) shl 8 + Random(256));
end;

function TLayouts.CreateSimpleLayout(const ALayoutKind: TLayoutKind): TLayout;
begin
  if not (ALayoutKind in [lkFrame, lkPanel, lkPage, lkAction, lkShape]) then
    Exit(nil);

  Result := TLayout.Create(ALayoutKind);
  Result.Presenter := FUIBuilder.Presenter;
  if ALayoutKind in [lkPanel, lkFrame] then
  begin
    Result.Font.Size := 10;
    Result.Font.Color := ColorToAlphaColor(TColorRec.SysWindowText);
    Result.Font.Family := 'Tahoma';
    Result.Color := ColorToAlphaColor(TColorRec.SysBtnFace);
    Result.ShowCaption := True;
    Result.BevelOuter := lbkNone;
  end
  else if ALayoutKind = lkPage then
  begin
    Result.State := vsFullAccess;
    Result.ShowCaption := True;
    Result.Tag := 11;
  end;
end;

function TLayouts.GetLayout(const ALayoutName: string; const AView: TView): TLayout;
var
  vInteractor: TInteractor;
  vFileName, vPostfix: string;
  vUser: TEntity;
  vRoles: TListField;
  vRole: TEntity;
  vCanLoadFromDFM: Boolean;
  vLayoutExt: string;
begin
  if FNames.TryGetValue(ALayoutName, vFileName) then
  begin
    if FItems.TryGetValue(vFileName, Result) then
      Result := Result.Clone
    else
      Result := ApplyAbsentLayout;
    Exit;
  end;

  vPostfix := '';
  if Assigned(AView) and Assigned(AView.Interactor) then
  begin
    vInteractor := TInteractor(AView.Interactor);

    vUser := TUserSession(vInteractor.Session).CurrentUser;
    if Assigned(vUser) then
    begin
      vRoles := TListField(vUser.FieldByName('Roles'));
      if vRoles.Count <> 1 then
      begin
        if TUserSession(vInteractor.Session).IsAdmin then
          vPostfix := '_Administrators';
      end
      else begin
        vRole := TEntity(vRoles[0]).ExtractEntity('Role');
        if Assigned(vRole) then
          vPostfix := '_' + vRole['Code'];
      end;
    end;
  end;

  vCanLoadFromDFM := TPresenter(FUIBuilder.Presenter).LoadFromDFM;
  vLayoutExt := IfThen(vCanLoadFromDFM, LAYOUT_DFM_EXT, LAYOUT_XTF_EXT);

  vFileName := TDomain(FUIBuilder.Domain).Configuration.FindLayoutFile(ALayoutName, vLayoutExt, vPostfix);
  if vFileName = '' then
  begin
    vFileName := TPath.Combine(GetPlatformDir, 'layouts' + PathDelim + ALayoutName + vLayoutExt);
    if not TFile.Exists(vFileName) then
      vFileName := '';
  end;

  if FItems.TryGetValue(vFileName, Result) then
  begin
    FNames.Add(ALayoutName, vFileName);
    Result := Result.Clone;
    Exit;
  end;

  if FileExists(vFileName) then
  begin
    if vCanLoadFromDFM then
    begin
      Result := MakeLayoutFromFile(vFileName);
      Result.Save(ChangeFileExt(vFileName, LAYOUT_XTF_EXT));
    end
    else begin
      Result := TLayout.Create(lkNone);
      Result.Load(vFileName);
    end;
  end
  else begin
    vFileName := ALayoutName + '_auto_';
    Result := MakeDefaultLayout(AView, ALayoutName);
  end;

  if Assigned(Result) then
  begin
    FNames.Add(ALayoutName, vFileName);
    FItems.Add(vFileName, Result);
    Result := Result.Clone
  end
  else
    Result := ApplyAbsentLayout;
end;

function TLayouts.MakeDefaultLayout(const AView: TView; const ALayoutName: string): TLayout;
var
  vDefinition: TDefinition;
  vFieldDef: TFieldDef;
  vAction: TActionDef;
  vView: TView;
  vLayout: TLayout;
  vOneRowHeight: Integer;

  function CalcLayoutPositionCount(const AFieldKind: TFieldKind; const AViewName: string): Integer;
  begin
    if ((AFieldKind = fkString) and (AViewName = 'memo')) or ((AFieldKind = fkBlob) and (AViewName = 'image'))
      or ((AFieldKind = fkList) and (AViewName = 'parameters'))
    then
      Result := 2
    else if (AFieldKind = fkList) and ((AViewName = 'selector') or (AViewName = 'multiselect')
      or (AViewName = 'multiselect3') or (AViewName = 'multiselect'))
    then
      Result := 5
    else if (AFieldKind = fkList) and (AViewName = 'paged') then
      Result := 6
    else if (AFieldKind = fkList) and (AViewName = 'mtm') then
      Result := 4
    else if AFieldKind in [fkList, fkFlag] then
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

  Result := CreateSimpleLayout(lkFrame);

  vOneRowHeight := Result.Font.Size * 96 div 72 + 8;

  for vAction in vDefinition.Actions.Objects do
  begin
    vView := AView.BuildView(vAction.Name);
    if Assigned(vView) then
    begin
      if vView.State > vsHidden then
      begin
        vLayout := CreateSimpleLayout(lkPanel);
        vLayout.Caption := vAction.Name;
        vLayout.Left := 0;
        vLayout.Top := 0;
        vLayout.Width := cDefaultColumnWidth;
        vLayout.Height := vOneRowHeight;
        Result.Add(vLayout);
      end
      else
        vView.CleanView;
    end;
  end;

  AView.AddListener(nil);
  try
    for vFieldDef in vDefinition.Fields do
    begin
      vView := AView.BuildView(vFieldDef.Name);
      if Assigned(vView) then
      begin
        if not TInteractor(AView.Interactor).NeedSkipField(nil, vFieldDef) and (vFieldDef.Kind <> fkComplex) then
        begin
          vLayout := CreateSimpleLayout(lkPanel);
          vLayout.Caption := vFieldDef.Name;
          vLayout.Left := 0;
          vLayout.Top := 0;
          vLayout.Width := cDefaultColumnWidth;
          vLayout.Height := CalcLayoutPositionCount(vFieldDef.Kind, vFieldDef.StyleName) * (vOneRowHeight + cBetweenRows) - cBetweenRows;
          Result.Add(vLayout);
        end
        else
          vView.CleanView;
      end;
    end;
  finally
    AView.RemoveListener(nil);
  end;

  Result.ArrangeChildAreas;

  if (_Platform.DeploymentType = 'dev') or (_Platform.DeploymentType = 'mock') then
    Result.SaveToDFM(TPath.Combine(TConfiguration(vDefinition.Configuration).TempDir, ALayoutName + '_auto'));
end;

function TLayouts.MakeLayoutFromFile(const AFileName: string): TLayout;
var
  vFileStream: TStream;
  vMemStream: TStream;
  vFrame: TComponent;
begin
  Result := CreateSimpleLayout(lkFrame);
  vFrame := TComponent(TPresenter(FUIBuilder.Presenter).CreateTempControl);
  try
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

    TPresenter(FUIBuilder.Presenter).EnumerateControls(Result, vFrame);
  finally
    FreeAndNil(vFrame);
  end;
end;

{ TUIBuilder }

procedure TUIBuilder.ScaleLayout(const ALayout: TLayout; const AScale: Single);
var
  vChild: TLayout;
begin
  if SameValue(AScale, 1.0, 1e-6) then
    Exit;

  ALayout.Left := Round(ALayout.Left * AScale);
  ALayout.Top := Round(ALayout.Top * AScale);
  ALayout.Width := Round(ALayout.Width * AScale);
  ALayout.Height := Round(ALayout.Height * AScale);
  ALayout.Font.Size := Round(ALayout.Font.Size * AScale);
  for vChild in ALayout.Items do
    ScaleLayout(vChild);
end;

procedure TUIBuilder.ApplyLayout(const AArea: TUIArea; const AView: TView; const ALayoutName: string; const AParams: string);
var
  vLayout: TLayout;
begin
  vLayout := FLayouts.GetLayout(ALayoutName, AView);
  ScaleLayout(vLayout);

  AArea.BeginUpdate;
  try
    AArea.AssignFromLayout(vLayout, AParams);
    AArea.SetView(AView);
    AArea.SetLayout(vLayout);
    CreateChildAreas(AArea, AView, vLayout, AParams);
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

constructor TUIBuilder.Create(const ADomain: TObject);
begin
  inherited Create;
  //FInteractor := AInteractor;
  FPresenter := _Platform.Presenter;
  FDomain := ADomain;

  FLayouts := TLayouts.Create(Self);

  FImageMap := TDictionary<string, Integer>.Create;
  if not SameText(TPresenter(FPresenter).Name,'Web.UniGUI') then
    FImageLists := TObjectDictionary<Integer, TObject>.Create([doOwnsValues])
  else
    FImageLists := TObjectDictionary<Integer, TObject>.Create;

  FIsMDIStyle := SameText(TDomain(FDomain).Settings.GetValue('Core', 'Layout'), 'mdi') ;
end;

procedure TUIBuilder.CreateChildAreas(const AArea: TUIArea; const AView: TView; const ALayout: TLayout; const AParams: string);
var
  i: Integer;
begin
  for i := 0 to ALayout.Items.Count - 1 do
    AArea.CreateChildArea(AView, ALayout.Items[i], AParams);
  AArea.AfterChildAreasCreated;
end;

destructor TUIBuilder.Destroy;
begin
  FreeAndNil(FImageMap);
  FreeAndNil(FImageLists);
  FreeAndNil(FLayouts);
  FPresenter := nil;
  FDomain := nil;
  inherited Destroy;
end;

function TUIBuilder.GetTranslation(const ADefinition: TDefinition;
  const ATranslationPart: TTranslationPart = tpCaption): string;
begin
  Result := TDomain(FDomain).TranslateDefinition(ADefinition, ATranslationPart)
end;

function TUIBuilder.GetFieldTranslation(const AFieldDef: TFieldDef; const ATranslationPart: TTranslationPart): string;
begin
  Result := TDomain(FDomain).TranslateFieldDef(AFieldDef, ATranslationPart);
end;

function TUIBuilder.GetImageIndex(const AImageID: string): Integer;
begin
  if not FImageMap.TryGetValue(AImageID, Result) then
    Result := -1;
end;

function TUIBuilder.GetImages(const AResolution: Integer): TObject;
begin
  if not FImageLists.TryGetValue(AResolution, Result) then
  begin
    Result := TPresenter(FPresenter).CreateImages(FDomain, AResolution);
    FImageLists.AddOrSetValue(AResolution, Result);
  end;
end;

procedure TUIBuilder.GetLayoutName(const AEntity: TEntity; const AParams: string; var ALayoutName: string);
var
  vLayout, vOperation, vDefValue, vLayoutExt: string;

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

    vLayoutExt := IfThen(TPresenter(Presenter).LoadFromDFM, LAYOUT_DFM_EXT, LAYOUT_XTF_EXT);
    if (vLayout = '') or (not FileExists(TDomain(FDomain).Configuration.FindLayoutFile(vLayout, vLayoutExt))) then
      vLayout := AEntity.Definition.Name + GetPostfix;
  end;

  ALayoutName := vLayout;
end;

procedure TUIBuilder.Navigate(const AView: TView; const AAreaName, ALayoutName: string;
  const AOptions: string = ''; const AChangeHolder: TObject = nil;
  const ACaption: string = ''; const AOnClose: TCloseProc = nil);
var
  vInteractor: TInteractor;
  vFormLayout: TLayout;
  vLayoutName: string;
  vAreaName: string;
  vViewName: string;
  vUIArea: TUIArea;
  vTabArea: TUIArea;
  vServiceLayout: TLayout;
  vServiceArea: TUIArea;
  vLayout: TLayout;
  vPageID: string;
  vImageID: string;
  vDefaultCaption: string;
  vParams: TStrings;
  vCaption: string;

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
  if (AAreaName = 'free') and SameText(TPresenter(FPresenter).Name, 'Web.UniGUI') then
    Exit;

  if Assigned(AView) then
    vInteractor := TInteractor(AView.Interactor)
  else begin
    // Redo later
    Exit;
  end;

  vLayoutName := ALayoutName;

  Assert(AView.DefinitionKind in [dkDomain, dkEntity, dkAction, dkObjectField, dkCollection], 'Показываем непонятно что');
  if vLayoutName = '' then
  begin
    if AView.DomainObject is TEntity then
      GetLayoutName(AView.DomainObject as TEntity, AOptions, vLayoutName)
    else
      GetLayoutName(nil, AOptions, vLayoutName);
  end;

  if (AAreaName = 'WorkArea') and not FIsMDIStyle and not Assigned(vInteractor.RootArea.AreaById(AAreaName)) then
    vAreaName := 'child'
  else
    vAreaName := AAreaName;

  // Check for the same form
  if ((vAreaName = 'float') or (vAreaName = 'free')) and Assigned(vInteractor.RootArea) then
  begin
    vUIArea := vInteractor.RootArea.AreaByView(AView);
    if Assigned(vUIArea) then
    begin
      // TODO Activate only?
      TPresenter(FPresenter).ShowUIArea(vUIArea, vAreaName, ACaption);
      Exit;
    end;
  end;

  // Creation of forms
  if (vAreaName = '') or (vAreaName = 'float') or (vAreaName = 'free')
    or (vAreaName = 'child') or (vAreaName = 'modal') then
  begin
    vInteractor.AreaStack.Push(vInteractor.CurrentArea);

    vFormLayout := FLayouts.CreateSimpleLayout(lkFrame);
    vFormLayout.Caption := ACaption;
    vFormLayout.StyleName := vAreaName;
    vFormLayout.AreaKind := akForm;

    vUIArea := TPresenter(FPresenter).CreateArea(vInteractor.CurrentArea, AView, vFormLayout, '', AOnClose);

    vUIArea.SetHolder(AChangeHolder);

    // Main (application) form
    if vAreaName = '' then
    begin
      vUIArea.BeginUpdate;
      try
        vInteractor.RootArea := vUIArea;
        vInteractor.CurrentArea := vUIArea;
        vUIArea.RegisterChanges := False;
        try
          ApplyLayout(vUIArea, AView, vLayoutName, AOptions);
        finally
          vUIArea.RegisterChanges := True;
        end;

        TPresenter(FPresenter).ShowUIArea(vUIArea, vAreaName, ACaption);

        if vInteractor.DefaultParams <> '' then
        begin
          vParams := CreateDelimitedList(vInteractor.DefaultParams, '&');
          try
            vViewName := ExtractValueFromStrings(vParams, 'View');
            vLayoutName := ExtractValueFromStrings(vParams, 'Layout');
            if (vViewName <> '') or (vLayoutName <> '') then
              Navigate(vInteractor.RootView.BuildView(vViewName), 'WorkArea', vLayoutName, '', nil, ExtractValueFromStrings(vParams, 'Caption'));
          finally
            FreeAndNil(vParams);
            vInteractor.DefaultParams := '';
          end;
        end;
      finally
        vUIArea.EndUpdate;
      end;
    end
    else if (vAreaName = 'float') or (vAreaName = 'free') then
    begin
      vUIArea.BeginUpdate;
      try
        if Assigned(vInteractor.RootArea) then
          vInteractor.RootArea.AddArea(vUIArea)
        else
          vInteractor.RootArea := vUIArea;
        ApplyLayout(vUIArea, AView, vLayoutName, AOptions);
      finally
        vUIArea.EndUpdate;
      end;

      TPresenter(FPresenter).ShowUIArea(vUIArea, vAreaName, ACaption);
    end
    else if (vAreaName = 'child') or (vAreaName = 'modal') then
    begin
      if ACaption = '' then
      begin
        // Definition может быть от листового поля
        if Assigned(AView.Definition) then
        begin
          if AView.DefinitionKind = dkObjectField then
            vCaption := TDomain(vInteractor.Domain).TranslateFieldDef(TFieldDef(AView.Definition))
          else
            vCaption := TDomain(vInteractor.Domain).TranslateDefinition(TDefinition(AView.Definition));

          if (Pos(AOptions, 'NoExtCaption') < 1) and (AAreaName = 'child') then
          begin
            if AView.DefinitionKind = dkAction then
              vCaption := 'Параметры: ' + vCaption
            else if AView.State >= vsSelectOnly {and Assigned(vArea.Holder) - у параметров нет холдера} then
              vCaption := 'Редактирование: ' + vCaption
            else
              vCaption := 'Просмотр: ' + vCaption;
          end;
        end
        else
          vCaption := TDomain(vInteractor.Domain).AppTitle;
      end
      else
        vCaption := ACaption;

      vUIArea.BeginUpdate;
      try
        if Assigned(vInteractor.CurrentArea) then
          vInteractor.CurrentArea.AddArea(vUIArea);
        vInteractor.CurrentArea := vUIArea;
        ApplyLayout(vUIArea, AView, vLayoutName, AOptions);

        if vAreaName = 'child' then
        begin
          vServiceLayout := FLayouts.CreateSimpleLayout(lkPanel);
          vServiceLayout.Height := cServiceAreaHeight;
          vServiceLayout.Align := lalBottom;
          vServiceArea := vUIArea.DoCreateChildArea(vServiceLayout, vInteractor.RootView);
          vUIArea.AddArea(vServiceArea);
          if AView.State >= vsSelectOnly {and Assigned(AChangeHolder) - у параметров нет холдера} then
            ApplyLayout(vServiceArea, vUIArea.View, 'OkCancel', '')
          else
            ApplyLayout(vServiceArea, vUIArea.View, 'Close', '');
        end;
      finally
        vUIArea.EndUpdate;
      end;

      TPresenter(FPresenter).ShowUIArea(vUIArea, vAreaName, ACaption);
    end
    else
      Assert(False, 'Form style [' + vAreaName + '] is not supported');
  end
  // Creation of an internal area
  else begin
    vUIArea := vInteractor.RootArea.AreaById(vAreaName);
    if not Assigned(vUIArea) then
    begin
      if vAreaName = 'WorkArea' then
        vUIArea := vInteractor.RootArea
      else
        Assert(False, 'Область [' + vAreaName + '] не найдена');
    end;

    // Creation of new page for area content
    if vUIArea.QueryParameter('ViewType') = 'Paged' then
    begin
      vImageID := GetUrlParam(AOptions, 'ImageID');
      if Assigned(AView) then
      begin
        vPageID := GetUrlParam(AOptions, 'Cube');
        if vPageID <> '' then
          vPageID := ReplaceText(AView.FullName, '/', '_') + '_' + vPageID
        else
          vPageID := ReplaceText(AView.FullName, '/', '_');
        vPageID := ReplaceText(vPageID, '~', '_');
      end
      else
        vPageID := vLayoutName;

      vTabArea := vUIArea.AreaById(vPageID, False);
      if not Assigned(vTabArea) then
      begin
        vLayout := FLayouts.CreateSimpleLayout(lkPage);
        vLayout.Name := vPageID;
        vLayout.ImageID := vImageID;

        if AView.DefinitionKind in [dkAction, dkCollection] then
        begin
          if ACaption <> '' then
            vLayout.Caption := ACaption
          else begin
            vDefaultCaption := GetTranslation(TDefinition(AView.Definition));
            if Assigned(vInteractor.LastArea) and (vInteractor.LastArea.View = AView) then
              vLayout.Caption := vInteractor.LastArea.QueryParameter('Caption', vDefaultCaption)
            else
              vLayout.Caption := vDefaultCaption;
          end;

          if vImageID <> '' then
            vLayout.ImageID := TDefinition(AView.Definition)._ImageID;
        end
        else if (AView.DefinitionKind = dkEntity) and Assigned(AView.DomainObject) then
          vLayout.Caption := SafeDisplayName(TEntity(AView.DomainObject), ACaption)
        else if ACaption <> '' then
          vLayout.Caption := ACaption
        else
          vLayout.Caption := 'Стартовая страница';

        if FIsMDIStyle then
          vLayout.AreaKind := akForm;

        vTabArea := vUIArea.CreateChildArea(AView, vLayout, AOptions, AOnClose);
        vTabArea.BeginUpdate;
        try
          vTabArea.SetHolder(AChangeHolder);
          ApplyLayout(vTabArea, AView, vLayoutName, AOptions);
        finally
          vTabArea.EndUpdate;
        end;
      end;

      vTabArea.Activate(vInteractor.DefaultParams + IfThen(vInteractor.DefaultParams = '', '', '&') +
        'TabActivationOption=' + GetUrlParam(AOptions, 'TabActivationOption', 'ChangeTab'));
    end
    // Overwrite area content
    else begin
      AView.AddListener(vInteractor.RootArea);
      vUIArea.BeginUpdate;
      try
        vUIArea.Clear;
        ApplyLayout(vUIArea, AView, vLayoutName, AOptions);
        vUIArea.SetHolder(AChangeHolder);
      finally
        AView.RemoveListener(vInteractor.RootArea);
        vUIArea.EndUpdate;
      end;
    end;
  end;

  vInteractor.PrintHierarchy;
end;

procedure TUIBuilder.StoreImageIndex(const AImageID: string; const AImageIndex: Integer);
begin
  if not FImageMap.ContainsKey(AImageID) then
    FImageMap.Add(AImageID, AImageIndex);
end;

{ TUIAreaComparer }

function TUIAreaComparer.Compare(const ALeft, ARight: TUIArea): Integer;
begin
  Result := ARight.TabOrder - ALeft.TabOrder;
end;

{ TUIArea }

procedure TUIArea.Activate(const AUrlParams: string);
var
  vArea: TUIArea;
begin
  for vArea in FAreas do
    vArea.Activate(AUrlParams);
  FNativeControl.DoActivate(AUrlParams);
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
  if Assigned(AParams) then
    FLayout.Params := AParams.DelimitedText
  else
    FLayout.Params := '';
end;

procedure TUIArea.AfterChildAreasCreated;
var
  i, j: Integer;
  vList: TList<TUIArea>;
begin
  // Прицепляем сплиттер к своему контролу, чтобы не отлипал в некоторых случаях,
  // обрабатываем только простой случай: сплиттер с таким размещением один в текущей области
  for i := 0 to FAreas.Count - 1 do
    if Assigned(FAreas[i].Layout) and (FAreas[i].Layout.Kind = lkSplitter) then
    begin
      for j := 0 to FAreas.Count - 1 do
        if (FAreas[j] <> FAreas[i]) and (FAreas[j].Layout.Align = FAreas[i].Layout.Align) then
        begin
          FAreas[i].NativeControl.SetLinkedControl('splitter', FAreas[j].NativeControl);
          Break;
        end;
    end;

  if FAreas.Count <= 1 then
    Exit;

  // Apply TabOrder to all created areas
  vList := TList<TUIArea>.Create(TUIAreaComparer.Create);
  try
    for i := 0 to FAreas.Count - 1 do
      if FAreas[i].TabStop then
        vList.Add(TUIArea(FAreas[i]));

    vList.Sort;

    for i := 0 to vList.Count - 1 do
      vList[i].NativeControl.TabOrder := vList[i].TabOrder;
  finally
    FreeAndNil(vList);
  end;
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

function TUIArea.AreaByView(const AView: TView): TUIArea;
var
  i: Integer;
begin
  for i := 0 to FAreas.Count - 1 do
    if FAreas[i].View = AView then
      Exit(FAreas[i]);
  Result := nil;
end;

procedure TUIArea.AssignFromLayout(const ALayout: TLayout; const AParams: string);
begin
  FNativeControl.RegisterChanges := False;
  try
    FNativeControl.AssignFromLayout(ALayout, AParams);
  finally
    FNativeControl.RegisterChanges := True;
  end;
end;

procedure TUIArea.BeginUpdate;
begin
  if (FUpdateCount > 0) or ParentInUpdate then Exit;

  Inc(FUpdateCount);

  FNativeControl.DoBeginUpdate;
end;

function TUIArea.CanChangeArea: Boolean;
var
  vEntity: TEntity;
begin
  if FLayout.AreaKind = akField then
  begin
    vEntity := TEntity(FView.ParentDomainObject);
    Result := TCanChangeFieldFunc(TDomain(FView.Domain).Configuration.CanChangeFieldFunc)
      (FView, vEntity, FDefinitionName, FNativeControl.GetNewValue);
  end
  else
    Result := False;
end;

procedure TUIArea.Clear;
var
  i: Integer;
  vArea: TUIArea;
begin
  // Обработка сохранения данных при очистке области
  if Assigned(FHolder) then
  begin
    if TUserSession(TChangeHolder(FHolder).Session).IsValid then
    begin
      if TChangeHolder(FHolder).IsModified then
        TUserSession(FView.Session).Save(TChangeHolder(FHolder))
      else
        TUserSession(FView.Session).Cancel(TChangeHolder(FHolder));
    end;
    FHolder := nil;
  end;

  for i := 0 to FAreas.Count - 1 do
  begin
    vArea := GetArea(i);
    vArea.ClearContent;
    if vArea <> TInteractor(FInteractor).LastArea then
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

  TInteractor(FInteractor).ProcessAreaDeleting(Self);
  FInteractor := nil;
end;

procedure TUIArea.Close(const AModalResult: Integer = mrOk);
begin
  FIsClosing := True;
  FNativeControl.DoClose(AModalResult);
end;

function TUIArea.Contains(const AArea: TUIArea): Boolean;
begin
  Result := FAreas.Contains(AArea);
end;

function TUIArea.ContainView(const AView: TView): Boolean;
var
  i: Integer;
begin
  for i := 0 to FAreas.Count - 1 do
    if FAreas[i].View = AView then
      Exit(True);
  Result := False;
end;

constructor TUIArea.Create(const AParent: TUIArea; const AView: TView; const ALayout: TLayout; const AParams: string = '');
var
  vPopupArea: TUIArea;
begin
  inherited Create;

  FParent := AParent;
  FLayout := ALayout;
  FIsService := ALayout.AreaKind = akForm;
  FInitialMenu := nil;
  FUpdateCount := 0;
  FIsClosing := False;

  FInternalParams := AParams;
  FCreateParams := nil;
  if Length(AParams) > 0 then
    FCreateParams := CreateDelimitedList(AParams, '&');

  FAreas := TList<TUIArea>.Create;
  FHolder := nil;
  FInteractor := AView.Interactor;
  FRootView := TInteractor(FInteractor).RootView;
  FRootArea := TInteractor(FInteractor).RootArea;
  FUIBuilder := TInteractor(FInteractor).UIBuilder;
  FSession := TUserSession(AView.Session);

  FView := AView;
  if not FIsService then
    TrySubscribeView;

  ALayout.Params := AParams;
  if Assigned(FCreateParams) then
    ALayout.Id := FCreateParams.Values['Id'];

  if (ALayout.AreaKind = akField) and (AView.DefinitionKind <> dkEntity) then
  begin
    FFieldDef := TFieldDef(AView.Definition);
    FDefinitionName := FFieldDef.Name;
  end
  else begin
    FFieldDef := nil;
    if Assigned(AView.Definition) then
      FDefinitionName := TDefinition(AView.Definition).Name
    else
      FDefinitionName := '';
  end;

  FNativeControl := TPresenter(Presenter).CreateNativeControl(Self, AView, ALayout, uiUnknown, '', AParams);
  FNativeControl.Parent := FParent;
  FNativeControl.AfterSetParent(FParent);
  FNativeControl.TabOrder := FLayout.TabOrder;

  FId := FLayout.Id;

  // Creation of area's context menu
  if FLayout.AreaKind = akNavigation then
  begin
    Assert(Assigned(FLayout.Menu), 'Для навигационной области не задано меню');
    FInitialMenu := ALayout.Menu;
  end
  else if Assigned(FLayout.Menu) then
  begin
    vPopupArea := TPresenter(Presenter).CreateArea(Self, FRootView, FLayout.Menu);
    FNativeControl.SetLinkedControl('popup', vPopupArea.NativeControl);
    FAreas.Add(vPopupArea);
  end;

  // Нужно делать после задания родителя, так как надпись использует родительский шрифт
  if Assigned(FFieldDef) then
    FNativeControl.CreateCaption(FFieldDef);
end;

function TUIArea.CreateChildArea(const AChildView: TView; const ALayout: TLayout; const AParams: string; const AOnClose: TCloseProc = nil): TUIArea;
var
  vView: TView;
  vDefaultViewName: string;
  vDefaultView: TView;
  vPos: Integer;
  vQuery: string;
  vCaption: string;
  vLayoutName: string;
  vAlreadyAssigned: Boolean;
begin
  vAlreadyAssigned := False;
  Assert(ALayout.Kind <> lkNone, 'Не задан тип лэйаута');
  if not (ALayout.Kind in [lkPanel, lkPages, lkMemo]) then
  begin
    Result := DoCreateChildArea(ALayout, AChildView, '', AOnClose)
  end
  else begin
    vCaption := Trim(ALayout.Caption);
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
        ALayout.Caption := vCaption;
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
      if Assigned(AChildView) then  // was FView
        vView := AChildView.BuildView(vCaption)  // was FView
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
    vLayoutName := Result.QueryParameter('layout');
    vDefaultViewName := Result.QueryParameter('view');
    if (vLayoutName <> '') or (vDefaultViewName <> '') then
    begin
      if Pos('WorkArea', Result.Id) = 1 then
      begin
        if vDefaultViewName <> '' then
          vDefaultView := FRootView.BuildView(vDefaultViewName)
        else
          vDefaultView := nil;

        FUIBuilder.Navigate(vDefaultView, Result.Id, vLayoutName, Result.QueryParameter('Options'), nil, Result.QueryParameter('Caption'));
      end
      else
        FUIBuilder.ApplyLayout(Result, FView, vLayoutName, vQuery)
    end
    // it should be implemented for process embedded itemss
    //else if Result.IsDefault and (ALayout.Items.Count > 0) then
    //  FUIBuilder.CreateChildAreas(Result, FView, ALayout, vQuery)
    else if not vAlreadyAssigned then //#Check!
      FUIBuilder.CreateChildAreas(Result, Result.View, ALayout, vQuery);
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

procedure TUIArea.Deinit;
begin
  FNativeControl.DoDeinit;
end;

destructor TUIArea.Destroy;
begin
  FOnClose := nil;
  FInitialMenu := nil;
  FreeAndNil(FCreateParams);
  FreeAndNil(FNativeControl);
  if not (FLayout is TNavigationItem) then
    FreeAndNil(FLayout);

  inherited Destroy;
end;

procedure TUIArea.DisableContent;
var
  vArea: TUIArea;
begin
  FNativeControl.DoDisableContent;
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

function TUIArea.DoCreateChildAction(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
begin
  ALayout.AreaKind := akAction;
  Result := TUIArea.Create(Self, AView, ALayout, AParams);
end;

function TUIArea.DoCreateChildArea(const ALayout: TLayout; const AView: TView; const AParams: string;
  const AOnClose: TCloseProc): TUIArea;
begin
  Result := TPresenter(FUIBuilder.Presenter).CreateArea(Self, AView, ALayout, AParams, AOnClose);
end;

function TUIArea.DoCreateChildEditor(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
begin
  ALayout.AreaKind := akField;
  Result := TUIArea.Create(Self, AView, ALayout, AParams);
end;

function TUIArea.DoCreateChildList(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
begin
  ALayout.AreaKind := akList;
  ALayout.Id := 'List';
  Result := TUIArea.Create(Self, AView, ALayout, AParams);
end;

function TUIArea.DoCreateChildNavigation(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
var
  vNavArea: TUIArea;
begin
  Assert(Assigned(ALayout.Menu), 'У панели навигации [' + AView.InitialName + '] не задано PopupMenu.');

  ALayout.AreaKind := akNavigation;
  vNavArea := TUIArea.Create(Self, AView, ALayout, AParams);
  vNavArea.ProcessChilds;

  Result := vNavArea;
end;

procedure TUIArea.DoExecuteUIAction(const AView: TView);
begin
  FNativeControl.DoExecuteUIAction(AView);
end;

procedure TUIArea.DoProcessChilds(const AParent: TUIArea;
  const AView: TView; const ANavItem: TNavigationItem);
var
  i: Integer;
  vNavItem: TNavigationItem;
  vViewPath: TStrings;

  procedure CreateGroupNode(const ACurrentItem: TNavigationItem; const ACurrentView: TView);
  var
    vGroupArea: TUIArea;
    vDefinition: TDefinition;
    vDefinitions: TList<TDefinition>;
  begin
    ACurrentItem.Kind := lkNavItem;
    vGroupArea := TPresenter(Presenter).CreateArea(AParent, ACurrentView, ACurrentItem);
    if not Assigned(vGroupArea) then
      Exit;

    AParent.AddArea(vGroupArea);

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
    end
    else if (ACurrentItem.Id = 'Windows') and FUIBuilder.IsMDIStyle then
    begin
      ACurrentItem.Insert(0, 'ArrangeMozaic');
      ACurrentItem.Insert(1, 'ArrangeCascade');
      ACurrentItem.Insert(2, 'ArrangeHorz');
      ACurrentItem.Insert(3, 'ArrangeVert');
      if ACurrentItem.Items.Count > 4 then
        ACurrentItem.Insert(4, '-');
    end;

    DoProcessChilds(vGroupArea, ACurrentView, ACurrentItem);
  end;

  procedure CreateNavigationNode(const ACurrentItem: TNavigationItem; const ACurrentView: TView);
  var
    vNavArea: TUIArea;
  begin
    ACurrentItem.Kind := lkNavItem;
    vNavArea := TPresenter(Presenter).CreateArea(AParent, ACurrentView, ACurrentItem);
    if not Assigned(vNavArea) then
      Exit;

    AParent.AddArea(vNavArea);

    DoProcessChilds(vNavArea, ACurrentView, ACurrentItem);
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
  if FLayout.AreaKind <> akNavigation then
    Exit;

  for i := 0 to ANavItem.Items.Count - 1 do
  begin
    vNavItem := TNavigationItem(ANavItem.Items[i]);

    vViewPath := CreateDelimitedList(vNavItem.ViewName, '/');
    try
      ProcessNavigationNode(vNavItem, AView, vViewPath);
    finally
      FreeAndNil(vViewPath);
    end;
  end;
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
  if (AView.UIContext <> '') {or (Pos('#', AView.Name) = 1)} then
  begin
    vRefArea := nil;
    vCurArea := Parent;
    if FLayout is TNavigationItem then
    begin
      repeat
        if vCurArea.Layout = TNavigationItem(FLayout).Owner then
          vRefArea := vCurArea
        else
          vCurArea := vCurArea.Parent;
      until Assigned(vRefArea) or not Assigned(vCurArea);
    end
    else begin
      repeat
        vRefArea := vCurArea.AreaById(AView.UIContext);
        vCurArea := vCurArea.Parent;
      until Assigned(vRefArea) or not Assigned(vCurArea);
    end;

    if not Assigned(vRefArea) then
      Exit;

    // Ввести параметры
    vInteractor := TInteractor(AView.Interactor);
    vAction := TActionDef(AView.Definition);
    vParams := TEntity(AView.DomainObject);

    if Assigned(vParams) then
    begin
      vVisibleFieldCount := vParams.VisibleFieldCount(vInteractor.Session);
      vNeedShowParams := (vVisibleFieldCount > 0) and vAction.HasFlag(ccAlwaysShowParameters);
      if not vParams.IsValid or vNeedShowParams then
      begin
        // Showing parameters dialog
        vNeedClearParams := not vParams.IsValid and not vNeedShowParams;
        vInteractor.AtomicEditParams(AView, '', '', procedure(const AResult: TDialogResult)
          begin
            if AResult = drOk then
              vRefArea.DoExecuteUIAction(AView);
            if vNeedClearParams then
              vParams.ResetToDefault(TUserSession(AView.Session).NullHolder);
          end);
        Exit;
      end;
    end;

    vRefArea.DoExecuteUIAction(AView);
  end
  else begin
    vIsSlave := (AView.Name = 'Save') or SameText(QueryParameter('place'), 'embedded')
      or (Assigned(FCreateParams) and SameText(FCreateParams.Values['place'], 'embedded'));
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

function TUIArea.GetActiveChildArea: TUIArea;
begin
  Result := FNativeControl.ActiveChildArea;
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
        //Result := CreateChildLayoutedArea(ALayout, AView, TComplexFieldDef(AView.Definition).ObjectKindName, AParams);
        Result := DoCreateChildEditor(ALayout, AView, AParams);
      dkListField:
        begin
          if vParams.Values['view'] <> '' then
            Result := DoCreateChildEditor(ALayout, AView, AParams)
          else begin
            Result := CreateChildLayoutedArea(ALayout, AView, 'DefaultList', AParams);
            Result.NativeControl.CreateCaption(TFieldDef(AView.Definition));
          end;
        end;
    end;
  finally
    FreeAndNil(vParams);
  end;
end;

function TUIArea.GetTabOrder: Integer;
begin
  Result := FLayout.TabOrder
end;

function TUIArea.GetTabStop: Boolean;
begin
  Result := FLayout.TabOrder >= 0
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

  if not Assigned(Result) then
    Result := TDomain(FView.Domain).DomainHolder;
end;

function TUIArea.GetImageIndex(const AImageID: string): Integer;
begin
  Result := FUIBuilder.GetImageIndex(AImageID);
end;

function TUIArea.GetName: string;
begin
  if FDefinitionName <> '' then
    Result := FNativeControl.Description  + ', Def: ' + FDefinitionName
  else
    Result := FNativeControl.Description;
end;

function TUIArea.GetPresenter: TObject;
begin
  Result := FUIBuilder.FPresenter;
end;

function TUIArea.GetRegisterChanges: Boolean;
begin
  Result := FNativeControl.RegisterChanges;
end;

procedure TUIArea.OnActionMenuSelected(Sender: TObject);
var
  vArea: TUIArea;
  vSelectedIndex: Integer;
begin
  vArea := FNativeControl.AreaFromSender(Sender);
  if not Assigned(vArea) then
    Exit;
  if not Assigned(vArea.View) then
    Exit;
  if not Assigned(vArea.View.DomainObject) then
    Exit;

  vSelectedIndex := FNativeControl.IndexOfSender(Sender);
  Assert(vSelectedIndex >= 0, 'Выбран неизвестный пункт меню');

  TEntity(vArea.View.DomainObject)._SetFieldValue(FSession.NullHolder, 'SelectedIndex', vSelectedIndex);
  ProcessAreaClick(vArea);
end;

procedure TUIArea.OnAreaClick(Sender: TObject);
var
  vArea: TUIArea;
begin
  vArea := FNativeControl.AreaFromSender(Sender);
  if Assigned(vArea) then
    ProcessAreaClick(vArea);
end;

procedure TUIArea.OnChange(Sender: TObject);
begin
  if not CanChangeArea then
  begin
    FNativeControl.RefillArea(dckFieldChanged);
    Exit;
  end;

  FView.AddListener(FRootArea);
  try
    // Отключить прослушивание событий
    FNativeControl.SwitchChangeHandlers(nil);
    FView.RemoveListener(Self);
    try
      FNativeControl.DoOnChange;
    finally
      FNativeControl.SwitchChangeHandlers(OnChange);
      FView.AddListener(Self);
    end;
  finally
    FView.RemoveListener(FRootArea);
  end;

  Validate;
end;

procedure TUIArea.OnEnter(Sender: TObject);
var
  vArea: TUIArea;
begin
  vArea := FNativeControl.AreaFromSender(Sender);
  if Assigned(FInteractor) then
  begin
    TInteractor(FInteractor).ActiveArea := vArea;
    TInteractor(FInteractor).PrintHierarchy;
  end;
end;

procedure TUIArea.OnExit(Sender: TObject);
begin
  if Assigned(FUIBuilder) and Assigned(FInteractor) then
    TInteractor(FInteractor).ActiveArea := nil;

  if Assigned(FNativeControl) then
    FNativeControl.DoOnExit(Sender);
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
  vCaption: string;
  vHolder: TChangeHolder;
begin
  TInteractor(FInteractor).LastArea := AArea;
  if not Assigned(AArea) then
    Exit;

  vView := AArea.View;
  if not Assigned(vView) then
    Exit;

  if AArea.Layout is TNavigationItem then
  begin
    vNavItem := TNavigationItem(AArea.Layout);

    vWorkArea := vNavItem.ContentWorkArea;
    if vWorkArea = '' then
    begin
      if Assigned(vNavItem.Owner) then
        vWorkArea := vNavItem.Owner.ExtractString('contentworkarea', 'WorkArea')
      else
        vWorkArea := 'WorkArea';
    end;
    vLayoutName := vNavItem.ContentLayout;
    vCaption := vNavItem.ContentCaption;
  end
  else begin
    vWorkArea := 'WorkArea';
    vLayoutName := '';
    vCaption := '';
  end;

  if vView.DefinitionKind = dkAction then
    AArea.ExecuteUIAction(vView)
  else if vView.DefinitionKind = dkCollection then
  begin
    if vLayoutName = '' then
      vLayoutName := 'Collection';
    FUIBuilder.Navigate(vView, vWorkArea, vLayoutName, 'operation=opencollection', nil, vCaption);
  end
  else if vView.DefinitionKind in [dkEntity, dkObjectField] then
  begin
    vHolder := TUserSession(FView.Session).Edit(nil);
    FUIBuilder.Navigate(vView, vWorkArea, vLayoutName, 'operation=slap', vHolder, vCaption);
  end;
end;

procedure TUIArea.ProcessChilds;
begin
  if FLayout.AreaKind = akNavigation then
    DoProcessChilds(Self, FView, FInitialMenu);
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

procedure TUIArea.SetBounds(const ALeft, ATop, AWidth, AHeight: Integer);
var
  vBounds: TRect;
  vLeft, vTop, vWidth, vHeight: Integer;
begin
  vBounds := FNativeControl.Bounds;
  vLeft := IfThen(ALeft < 0, vBounds.Left, ALeft);
  vTop := IfThen(ATop < 0, vBounds.Top, ATop);
  vWidth := IfThen(AWidth < 0, vBounds.Width, AWidth);
  vHeight := IfThen(AHeight < 0, vBounds.Height, AHeight);

  FNativeControl.Bounds := Rect(vLeft, vTop, vLeft + vWidth, vTop + vHeight);
end;

procedure TUIArea.SetHolder(const AHolder: TObject);
begin
  FHolder := AHolder;
end;

procedure TUIArea.SetLayout(const Value: TLayout);
begin
  FLayout.ContentLayout := Value;
end;

procedure TUIArea.SetParent(const Value: TUIArea);
begin
  FParent := Value;
  FNativeControl.Parent := Value;
end;

procedure TUIArea.SetRegisterChanges(const Value: Boolean);
begin
  FNativeControl.RegisterChanges := Value;
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
begin
  if FView.FullName = '' then
    vViewName := ''
  else
    vViewName := ', view: "' + FView.FullName + '"';

  vModifier := '';
  if Assigned(FHolder) then
    vModifier := '[+]';
  if Self = TInteractor(FInteractor).CurrentArea then
    vModifier := vModifier + 'C';
  if Self = TInteractor(FInteractor).LastArea then
    vModifier := vModifier + 'L';
  if Self = TInteractor(FInteractor).ActiveArea then
    vModifier := vModifier + 'A';

  if vModifier <> '' then
    vModifier := '>>' + vModifier + ': ';

  vIndex := '/' + IntToStr(FLayout._Index) + '/ ';

  Result := AIndent + vModifier + vIndex + GetName + vViewName + #13#10;
  for i := 0 to FAreas.Count - 1 do
    Result := Result + GetArea(i).TextHierarchy(AIndent + '    ');
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

procedure TUIArea.UnbindContent(const AForceUnbind: Boolean = False);
begin
  FNativeControl.UnbindContent(AForceUnbind);
end;

procedure TUIArea.UpdateArea(const AKind: Word; const AParameter: TEntity = nil);
begin
  FNativeControl.UpdateArea(AKind, AParameter);
end;

procedure TUIArea.Validate;
var
  vEntity: TEntity;
begin
  vEntity := TEntity(FView.ParentDomainObject);
  if not Assigned(vEntity) then
    Exit;

//  SetWarningVisible(vEntity.FieldByName(FFieldDef.Name).ValidationStatus = vsInvalid);
end;

{ TNativeControl }

function TNativeControl.AreaFromSender(const ASender: TObject): TUIArea;
begin
  if Assigned(ASender) and (ASender is TComponent) then
    Result := TUIArea(TComponent(ASender).Tag)
  else
    Result := nil;
end;

procedure TNativeControl.AssignFromLayout(const ALayout: TLayout; const AParams: string);
begin
end;

constructor TNativeControl.Create(const AOwner: TUIArea; const AParams: string = '');
begin
  inherited Create;

  FOwner := AOwner;
  FParent := AOwner.Parent;
  FLayout := AOwner.Layout;
  FView := AOwner.View;
  FParams := AParams;
  FPresenter := AOwner.Presenter;
  FInteractor := AOwner.Interactor;
  FDomain := TDomain(TInteractor(FInteractor).Domain);
  FUIBuilder := AOwner.UIBuilder;
  FFieldDef := AOwner.FieldDef;
  FCreateParams := AOwner.CreateParams;
  FInternalParams := AOwner.InternalParams;

  FNeedCreateCaption := True;
  FShowCaption := True;

  FIsForm := FLayout.AreaKind = akForm;
  FIsAutoReleased := FLayout is TNavigationItem;
end;

procedure TNativeControl.CreateCaption(const AFieldDef: TFieldDef);
var
  vInteractor: TInteractor;
  vMarkRequiredFields: Boolean;
  vCaption: string;
  vHint: string;
begin
  if not Assigned(AFieldDef) then
    Exit;

  vInteractor := TInteractor(FView.Interactor);

  if FNeedCreateCaption then
  begin
    if Assigned(FCreateParams) and (FCreateParams.IndexOfName('Caption') >= 0) then
      vCaption := FCreateParams.Values['Caption']
    else
      vCaption := GetFieldTranslation(AFieldDef);

    if Assigned(FCreateParams) and (FCreateParams.IndexOfName('Hint') >= 0) then
      vHint := FCreateParams.Values['Hint']
    else
      vHint := GetFieldTranslation(AFieldDef, tpHint);

    FShowCaption := True;
    vMarkRequiredFields := StrToBoolDef(TDomain(FDomain).UserSettings.GetValue('Core', 'MarkRequiredFields'), True);

    if AFieldDef.HasFlag(cRequired) then
    begin
      if vMarkRequiredFields then
        vCaption := vCaption + '**';
      vHint := vHint + vInteractor.Translate('txtRequired', 'Обязательное');
    end
    else if AFieldDef.HasFlag(cRecommended) then
    begin
      if vMarkRequiredFields then
        vCaption := vCaption + '*';
      vHint := vHint + vInteractor.Translate('txtRecommendedToFill', 'Рекомендуется заполнить');
    end;

    FCaption := DoCreateCaption(FOwner, vCaption, vHint);
  end;
end;

procedure TNativeControl.CreateContent(const AContent: TObject);
begin
end;

destructor TNativeControl.Destroy;
begin
  DoBeforeFreeControl;

  if not SameText(TPresenter(FPresenter).Name, 'Web.UniGUI') then
    FreeAndNil(FCaption)
  else
    FCaption := nil;

  FOwner := nil;
  FParent := nil;
  FInteractor := nil;
  FUIBuilder := nil;
  FFieldDef := nil;

  inherited Destroy;
end;

procedure TNativeControl.DoActivate(const AUrlParams: string);
begin
end;

procedure TNativeControl.DoAfterSetParent(const AParent: TUIArea);
begin
end;

procedure TNativeControl.DoBeforeFreeControl;
begin
end;

procedure TNativeControl.DoBeginUpdate;
begin
end;

procedure TNativeControl.DoClose(const AModalResult: Integer);
begin
end;

function TNativeControl.DoCreateCaption(const AParent: TUIArea; const ACaption, AHint: string): TObject;
begin
  Result := nil;
end;

procedure TNativeControl.AfterSetParent(const AParent: TUIArea);
begin
  DoAfterSetParent(AParent);
end;

procedure TNativeControl.DoDeinit;
begin
end;

procedure TNativeControl.DoDisableContent;
begin
end;

procedure TNativeControl.DoEndUpdate;
begin
end;

procedure TNativeControl.DoExecuteUIAction(const AView: TView);
begin
end;

procedure TNativeControl.DoOnChange;
begin
end;

procedure TNativeControl.DoOnExit(Sender: TObject);
begin
end;

procedure TNativeControl.DoRegisterChanges(const Value: Boolean);
begin
  FRegisterChanges := Value;
end;

procedure TNativeControl.FillEditor;
begin
end;

procedure TNativeControl.FocusedChanged(const AFocused: Boolean);
begin
  PlaceLabel;
  if not AFocused then
    FOwner.Validate;
end;

function TNativeControl.GetActiveChildArea: TUIArea;
begin
  Result := nil;
end;

function TNativeControl.GetBounds: TRect;
begin
  Result := TRect.Empty;
end;

function TNativeControl.GetClientRect: TRect;
begin
  Result := GetBounds;
end;

function TNativeControl.GetControlInfo: string;
begin
  Result := '[???]';
end;

function TNativeControl.GetFieldTranslation(const AFieldDef: TFieldDef;
  const ATranslationPart: TTranslationPart): string;
begin
  Result := FUIBuilder.GetFieldTranslation(AFieldDef, ATranslationPart);
end;

function TNativeControl.GetFocused: Boolean;
begin
  Result := False;
end;

function TNativeControl.GetFormat: string;
begin
  if (FLayout.AreaKind = akField) and (FView.ParentDomainObject is TEntity) then
    Result := GetDisplayFormat(FFieldDef, FView.ParentDomainObject as TEntity)
  else
    Result := '';
end;

function TNativeControl.GetImageIndex(const AImageID: string): Integer;
begin
  Result := FUIBuilder.GetImageIndex(AImageID);
end;

function TNativeControl.GetModalResult: TModalResult;
begin
  Result := mrNone;
end;

function TNativeControl.GetNewValue: Variant;
begin
  Result := Null;
end;

function TNativeControl.GetRealControl(const AArea: TUIArea): TObject;
begin
  if Assigned(FPresenter) then
    Result := TPresenter(FPresenter).GetRealControl(AArea)
  else
    Result := nil;
end;

function TNativeControl.GetDescription: string;
var
  vViewState: TViewState;
begin
  vViewState := GetViewState;
  if vViewState = vsUndefined then
    Result := '???'
  else
    Result := cViewStateNames[vViewState];

  Result := Result + ' ' + GetControlInfo;
  if FOwner.Id <> '' then
    Result := Result + ', Id= "' + FOwner.Id + '"';
  if FLayout.Caption <> '' then
    Result := Result + ', Caption= "' + FLayout.Caption + '"';
end;

function TNativeControl.GetDisplayFormat(const AFieldDef: TFieldDef; const AEntity: TEntity): string;
var
  i: Integer;
  vFieldName: string;
  vFormat: string;
begin
  if not Assigned(FFieldDef) then
    Exit('');

  if Length(AFieldDef.Format) > 0 then
    vFormat := AFieldDef.Format
  else if AEntity.FieldExists('Format') then
    vFormat := AEntity['Format'];

  i := Pos('@field=', vFormat);

  if i > 0 then
  begin
    if Assigned(AEntity) then
    begin
      vFieldName := Copy(vFormat, i + Length('@field='), Length(vFormat) - i);
      try
        Result := AEntity[vFieldName];
        Exit;
      except
      end;
    end;
  end;

  Result := vFormat;
end;

function TNativeControl.GetTabOrder: Integer;
begin
  Result := -1;
end;

function TNativeControl.GetTranslation(const ADefinition: TDefinition;
  const ATranslationPart: TTranslationPart): string;
begin
  Result := FUIBuilder.GetTranslation(ADefinition, ATranslationPart);
end;

function TNativeControl.GetViewState: TViewState;
begin
  Result := vsUndefined;
end;

function TNativeControl.GetWindowState: TWindowState;
begin
  Result := TWindowState.wsNormal;
end;

function TNativeControl.IndexOfSender(const ASender: TObject): Integer;
begin
  Result := -1;
end;

function TNativeControl.ParentInUpdate: Boolean;
begin
  Result := FOwner.ParentInUpdate;
end;

procedure TNativeControl.PlaceLabel;
begin
end;

procedure TNativeControl.RefillArea(const AKind: Word);
var
  vEntity: TEntity;
begin
  UpdateCaptionVisibility;

  if FView.DefinitionKind = dkEntity then
  begin
    if AKind = dckViewStateChanged then
    begin

    end
    else begin
      vEntity := TEntity(FView.DomainObject);
      FOwner.Clear;
      if Assigned(vEntity) then
        if FOwner.QueryParameter('view') <> '' then
          //CreateEntityArea(AParentArea, ALayout, AView, QueryParameter('view')): TUIArea;
        else if FOwner.QueryParameter('childLayout') <> '' then  //todo: перенести это в GetLayoutName
          FUIBuilder.ApplyLayout(FOwner, FView, Trim(FOwner.QueryParameter('childLayout')), '')
        else
          FUIBuilder.ApplyLayout(FOwner, FView, vEntity.Definition.Name + 'EditForm', '');
    end;
  end
  else if FView.DefinitionKind = dkCollection then
  begin
  end
  else if FView.DefinitionKind in [dkSimpleField, dkObjectField, dkComplexField, dkListField] then
  begin
    vEntity := TEntity(FView.ParentDomainObject);
    if not Assigned(vEntity) then
      Exit;

    try
      SwitchChangeHandlers(nil);
      if not Assigned(FView.ParentDomainObject) then
        DoDeinit
      else begin
        FillEditor;
        FOwner.Validate;
      end;
      SwitchChangeHandlers(FOwner.OnChange);
    except
      TInteractor(FInteractor).ShowMessage('Error in FillEditorFromModel, Field: ' + FFieldDef.Name);
    end;
  end;
end;

procedure TNativeControl.SetActiveChildArea(const AArea: TUIArea);
begin
end;

procedure TNativeControl.SetAlignment(const AAlignment: TAlignment);
begin
end;

procedure TNativeControl.SetBounds(const Value: TRect);
begin
end;

procedure TNativeControl.SetFieldEntity(const AEntity: TEntity);
begin
  if FLayout.AreaKind = akField then
    FView.SetFieldEntity(FOwner.Holder, AEntity);
end;

procedure TNativeControl.SetFieldStream(const AStream: TStream);
begin
  if FLayout.AreaKind = akField then
    FView.SetFieldStream(FOwner.Holder, AStream);
end;

procedure TNativeControl.SetFieldValue(const AValue: Variant);
begin
  if FLayout.AreaKind = akField then
    FView.SetFieldValue(FOwner.Holder, AValue);
end;

procedure TNativeControl.SetFocused(const Value: Boolean);
begin
end;

procedure TNativeControl.SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl);
begin
end;

procedure TNativeControl.SetModalResult(const AModalResult: TModalResult);
begin
end;

procedure TNativeControl.SetParent(const AParent: TUIArea);
begin
  FParent := AParent;
end;

procedure TNativeControl.SetTabOrder(const ATabOrder: Integer);
begin
end;

procedure TNativeControl.SetViewState(const AViewState: TViewState);
begin
end;

procedure TNativeControl.SetWindowState(const AWindowState: TWindowState);
begin
end;

procedure TNativeControl.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
end;

procedure TNativeControl.UnbindContent(const AForceUnbind: Boolean = False);
begin
end;

procedure TNativeControl.UpdateArea(const AKind: Word; const AParameter: TEntity);
begin
  RefillArea(AKind);

  if not FIsForm then
    SetViewState(FView.State);
end;

procedure TNativeControl.UpdateCaptionVisibility;
begin
end;

{ TNativeControlHolder }

procedure TNativeControlHolder.CreateContent(const AContent: TObject);
begin
  if Assigned(AContent) then
    SetControl(AContent)
  else
    SetControl(DoCreateControl(FParent, FLayout));
end;

function TNativeControlHolder.CreateItem(const AParent: TUIArea;
  const ANavItem: TNavigationItem; const ACaption, AHint: string;
  const AImageIndex: Integer): TObject;
begin
  Result := DoCreateItem(AParent, ANavItem, ACaption, AHint, AImageIndex);
end;

function TNativeControlHolder.DoCreateControl(const AParent: TUIArea;
  const ALayout: TLayout): TObject;
begin
  Result := nil;
end;

function TNativeControlHolder.DoCreateItem(const AParent: TUIArea;
  const ANavItem: TNavigationItem; const ACaption, AHint: string;
  const AImageIndex: Integer): TObject;
begin
  Result := nil;
end;

function TNativeControlHolder.GetControlInfo: string;
begin
  if not Assigned(FControl) then
    Result := 'NULL'
  else
    Result := FControl.ClassName;
end;

procedure TNativeControlHolder.SetControl(const AControl: TObject);
begin
  FControl := AControl;

  if (FControl is TComponent) and (TComponent(FControl).Tag = 0) then
    TComponent(FControl).Tag := NativeInt(FOwner);
end;

procedure TNativeControlHolder.UnbindContent(const AForceUnbind: Boolean);
begin
  if (AForceUnbind or FIsAutoReleased) then
  begin
    //if not FIsForm then
      //FControl := nil
    //else if FLayout.StyleName <> 'modal' then
      FControl := nil;
  end;
end;

{ TDomainSceneObject }

constructor TDomainSceneObject.Create(const AOwner: TUIArea; const AScene: TScene);
begin
  FOwner := AOwner;
  inherited Create(AScene, nil, AScene.ClientRect);
end;

destructor TDomainSceneObject.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

procedure TDomainSceneObject.ExecuteUIAction(const AArea: TUIArea;
  const AView: TView);
begin
end;

procedure TDomainSceneObject.UpdateBinding(const ABinding: TObject);
begin
end;

end.
