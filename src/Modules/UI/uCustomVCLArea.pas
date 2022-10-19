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

unit uCustomVCLArea;

interface

uses
  Windows, Classes, Forms, Messages, Generics.Collections, Graphics, Controls, StdCtrls, ExtCtrls, Menus, UITypes, SysUtils,
  uConsts, uUIBuilder, uDefinition, uEntity, uView, uScene, uSimpleChart, uBaseLayout;

type
  TCustomVCLArea = class;

  TButtonDesc = class
  public
    Caption: string;
    Hint: string;
    View: string;
    Layout: string;
    WorkArea: string;
    Options: string;
    Id: string;
    ImageID: Integer;
    ColorField: string;
    GroupField: string;

    function GenerateOptions(const ADelimiter: Char = '&'): string;
  end;

  TNavigationAreaClass = class of TNavigationArea;

  TNavigationArea = class abstract
  private
    FView: TView;
    FParentArea: TCustomVCLArea;
  protected
    FCurrentLevel: Integer;
    FParams: string;
    function GetControl: TObject; virtual; abstract;
    function DoCreateItem(const AParentObj: TObject; const AParams: string): TObject; virtual; abstract;
    procedure DoAssignItemOnClick(const AHandler: TNotifyEvent); virtual; abstract;
    procedure DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer); virtual; abstract;
    procedure DoAfterCreate(const AInteractor: TObject); virtual;
  public
    constructor Create(const ASource: TBaseLayout; const AView: TView; const AArea: TCustomVCLArea; const AInteractor: TObject; const AParams: string); virtual;
    destructor Destroy; override;

    function CreateItem(const AParentObj: TObject; const ALevel: Integer; const AParams: string): TObject;
    procedure AssignItemOnClick(const AHandler: TNotifyEvent);
    procedure AssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer);
    procedure AfterCreate(const AInteractor: TObject);

    property Control: TObject read GetControl;
    property View: TView read FView;
  end;

  TLabelPosition = (lpTop, lpLeft);

  TVCLFieldArea = class;
  TVCLFieldAreaClass = class of TVCLFieldArea;
  TVCLAreaClass = class of TCustomVCLArea;

  TCustomVCLArea = class(TUIArea)
  private
    FOriginLeft, FOriginTop: Integer;  // for correct order during alignment
    FLabelPosition: TLabelPosition;
    FCaption: TLabel;
    FIsAutoReleased: Boolean;
    FOnClose: TProc;
    function CreateButtonDesc(const AText: string): TButtonDesc;
    // Выполнение действий (Actions и переходы)
    procedure ExplicitNavigate(Sender: TObject);
    procedure SetLabelPosition(const Value: TLabelPosition);
    function GetControl: TControl;
    function GetComponent: TComponent;
  protected
    FIsForm: Boolean;
    FPopupMenu: TPopupMenu;
    FNeedCreateCaption: Boolean;
    FShowCaption: Boolean;
    procedure PlaceLabel;
    procedure OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
    procedure OnActionMenuSelected(Sender: TObject);
    function LessThanUIState(const ADefinition: TDefinition; const ASession: TObject; const AState: TViewState): Boolean;
    procedure CopyMenuItems(const AParent: TUIArea; const AView: TView; const ASrcMenu: TBaseLayout; const ADestMenu: TMenuItem);
    procedure CopyPopupMenuItems(const AParent: TUIArea; const AView: TView; const ASrcMenu: TBaseLayout; const ADestMenu: TMenuItem);
    function AddDefinition(const AParent: TUIArea; const AView: TView; const AMenu: TMenuItem; const ADefinition: TDefinition; const ALayoutName: string = ''): TUIArea;
    function CreateNavigationArea(const ASourcePanel: TBaseLayout; const AView: TView; const AParams: string): TUIArea;
    procedure DoClose(const AModalResult: Integer); override;
    function AreaFromSender(const ASender: TObject): TUIArea; override;
    procedure AppendServiceArea(const ALayoutName: string); override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure DoActivate(const AUrlParams: string); override;
  protected
    procedure PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer); override;

    function GetName: string; override;
    procedure SetParent(const Value: TUIArea); override;
    procedure SetControl(const AControl: TObject); override;
    procedure UnbindContent; override;
    procedure AssignFromLayout(const ALayout: TBaseLayout); override;
    procedure ArrangeChildAreas; override;
    procedure SaveLayoutToFile(const AFileName: string); override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure SetViewState(const AValue: TViewState); override;
    procedure CreateCaption(const AFieldDef: TFieldDef); override;
    function DoGetDescription: string; override;
    procedure RefillArea(const AKind: Word); override;

    procedure SetCaptionProperty(const ALayout: TBaseLayout); virtual;
    procedure AssignFont(const AFont: TFont); virtual;
    procedure AssignAlignment(const AAlign: TAlignment); virtual;
    procedure ProcessFrame(const AFrame: TBaseLayout); virtual;
    function GetSelectedIndexFromControl(const AControl: TComponent; const AMenuItem: TMenuItem): Integer; virtual;
    function GetNavigationAreaClass: TNavigationAreaClass; virtual;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TBaseLayout = nil; const AParams: string = ''); override;
    destructor Destroy; override;

    property Component: TComponent read GetComponent;
    property Control: TControl read GetControl;
    property OnClose: TProc read FOnClose write FOnClose;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition;
  end;

  TVCLFieldArea = class(TCustomVCLArea)
  private
    procedure Validate;
    procedure BeforeContextMenuShow(Sender: TObject);
  protected
    FFieldDef: TFieldDef;
    FDefinitionName: string;

    procedure OnChange(Sender: TObject);

    // Not implemented here
    procedure FillEditor; virtual;
    procedure DoBeforeFreeControl; virtual;
    procedure DoDeinit; virtual;
    procedure DoOnChange; virtual;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); virtual;
    function GetNewValue: Variant; virtual;

    // Partially implemented, can be overridden in descendants
    function GetLayoutPositionCount: Integer; virtual;

    function GetFocused: Boolean;
    procedure SetFocused(const Value: Boolean); virtual;
    function GetDefaultFocusedChild: TWinControl; virtual;
    procedure FocusedChanged(const AFocused: Boolean); virtual;

    function GetName: string; override;
    procedure RefillArea(const AKind: Word); override;

    procedure SetFieldValue(const AValue: Variant);
    procedure SetFieldEntity(const AEntity: TEntity);
    procedure SetFieldStream(const AStream: TStream);
    function DoCreateChildArea(const ALayout: TBaseLayout; const AView: TView; const AParams: string = ''; const AOnClose: TProc = nil): TUIArea; override;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TBaseLayout = nil; const AParams: string = ''); override;
    destructor Destroy; override;

    procedure Deinit;

    property Focused: Boolean read GetFocused write SetFocused;
    property LayoutPositionCount: Integer read GetLayoutPositionCount;
  end;

  TFieldSceneArea = class(TVCLFieldArea)
  protected
    FScene: TScene;
    procedure DoActivate(const AAreaState: string = ''); override;
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
    procedure DoDisableContent; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TFieldChartArea = class(TFieldSceneArea)
  protected
    FChart: TSimpleChart;
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout); override;
  end;

type
  TCanChangeFieldFunc = function(const AView: TView; const AEntity: TEntity; const AFieldName: string; const ANewValue: Variant): Boolean of object;

const
  cPCNavigatorFlag = 1;

implementation

uses
  Types, Math, StrUtils, ComCtrls, Buttons,
  Generics.Defaults, Variants,

  uDomain, uPresenter, uConfiguration, uSession, uInteractor, uUtils, uCollection, uEntityList, uDomainUtils, uVCLScene, uPLatform;

type

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

    procedure Build(const AVCLArea: TCustomVCLArea);
    procedure SaveToDFM(const AFileName: string);
  end;

  TMainMenuArea = class(TNavigationArea)
  private
    FMenu: TMainMenu;
    FMenuItem: TMenuItem;
  protected
    function GetControl: TObject; override;
    function DoCreateItem(const AParentObj: TObject; const AParams: string): TObject; override;
    procedure DoAssignItemOnClick(const AHandler: TNotifyEvent); override;
    procedure DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer); override;
  public
    constructor Create(const ASource: TBaseLayout; const AView: TView; const AArea: TCustomVCLArea; const AInteractor: TObject; const AParams: string); override;
    destructor Destroy; override;
  end;

  TToolBarArea = class(TNavigationArea)
  private
    FToolBar: TToolBar;
    FToolButton: TToolButton;
    FMenuItem: TMenuItem;
  protected
    function GetControl: TObject; override;
    function DoCreateItem(const AParentObj: TObject; const AParams: string): TObject; override;
    procedure DoAssignItemOnClick(const AHandler: TNotifyEvent); override;
    procedure DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer); override;
    procedure DoAfterCreate(const AInteractor: TObject); override;
  public
    constructor Create(const ASource: TBaseLayout; const AView: TView; const AArea: TCustomVCLArea; const AInteractor: TObject; const AParams: string); override;
    destructor Destroy; override;
  end;

  TTreeViewArea = class(TNavigationArea)
  private
    FTreeView: TTreeView;
    FItem: TTreeNode;
    procedure OnDblClick(Sender: TObject);
  protected
    function GetControl: TObject; override;
    function DoCreateItem(const AParentObj: TObject; const AParams: string): TObject; override;
    procedure DoAssignItemOnClick(const AHandler: TNotifyEvent); override;
    procedure DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer); override;
  public
    constructor Create(const ASource: TBaseLayout; const AView: TView; const AArea: TCustomVCLArea; const AInteractor: TObject; const AParams: string); override;
    destructor Destroy; override;
  end;

  TOneButtonArea = class(TNavigationArea)
  private
    FButton: TButton;
    FMenu: TPopupMenu;
    FItem: TMenuItem;
    procedure OnClick(Sender: TObject);
  protected
    function GetControl: TObject; override;
    function DoCreateItem(const AParentObj: TObject; const AParams: string): TObject; override;
    procedure DoAssignItemOnClick(const AHandler: TNotifyEvent); override;
    procedure DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer); override;
  public
    constructor Create(const ASource: TBaseLayout; const AView: TView; const AArea: TCustomVCLArea; const AInteractor: TObject; const AParams: string); override;
    destructor Destroy; override;
  end;

const
  cServiceAreaHeight = 44;

procedure LockControl(const AWinControl: TWinControl; const ALock: Boolean);
begin
  if (AWinControl = nil) or (AWinControl.Handle = 0) then Exit;
  if ALock then
    SendMessage(AWinControl.Handle, WM_SETREDRAW, 0, 0)
  else
  begin
    SendMessage(AWinControl.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(AWinControl.Handle, nil, 0,
      RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

{ TCustomVCLArea }

procedure TCustomVCLArea.AppendServiceArea(const ALayoutName: string);
var
  vPanel: TPanel;
  vArea: TCustomVCLArea;
begin
  vPanel := TPanel.Create(nil);
  vPanel.BevelOuter := bvNone;
  vPanel.Height := cServiceAreaHeight;
  vPanel.Align := alBottom;
  vArea := TVCLAreaClass(Self.ClassType).Create(Self, FView, '', True, vPanel);
  AddArea(vArea);
  FUIBuilder.ApplyLayout(vArea, FView, ALayoutName, '');
end;

function TCustomVCLArea.AreaFromSender(const ASender: TObject): TUIArea;
begin
  if Assigned(ASender) and (ASender is TComponent) then
    Result := TUIArea(TComponent(ASender).Tag)
  else
    Result := nil;
end;

procedure TCustomVCLArea.ArrangeChildAreas;
const
  cColumnWidth = 250;
  cBetweenRows = 27;
  cBetweenColumns = 25;
  cSideRate = 3/2;
  cBorder = 12;
var
  vTotalHeight: Integer;
  vEditor: TCustomVCLArea;
  vCurColumnHeight: Integer;
  vRealMaxHeightInColumn: Integer;
  vOneRowHeight: Integer;
  vColumnCount: Integer;
  i: Integer;
  vRealColumnCount: Integer;
  vBestRate: Double;
  vHeightInColumn: Integer;
  vCurRate: Double;
  vBestColumnCount: Integer;

  function CalcLayoutPositionCount(const AArea: TUIArea): Integer;
  begin
    if AArea is TVCLFieldArea then
      Result := TVCLFieldArea(AArea).LayoutPositionCount
    else if AArea.View.DefinitionKind = dkAction then
      Result := 1
    else
      Result := 3;
  end;
begin
  vOneRowHeight := Abs(TCrackedWinControl(FControl).Font.Height) + 8; //editor + label + borders

  vTotalHeight := 0;
  for i := 0 to Count - 1 do
    vTotalHeight := vTotalHeight + CalcLayoutPositionCount(Areas[i]) * (vOneRowHeight + cBetweenRows);
  vBestRate := -1000;
  vBestColumnCount := -1;
  // select best columnt count
  for vColumnCount := 1 to 4 do
  begin
    vHeightInColumn := vTotalHeight div vColumnCount;
    vCurColumnHeight := 0;
    vRealMaxHeightInColumn := 0;
    for i := 0 to Count - 1 do
    begin
      vCurColumnHeight := vCurColumnHeight + CalcLayoutPositionCount(Areas[i]) * (vOneRowHeight + cBetweenRows);
      if vCurColumnHeight >= vHeightInColumn then
      begin
        if vCurColumnHeight > vRealMaxHeightInColumn then
          vRealMaxHeightInColumn := vCurColumnHeight;
        vCurColumnHeight := 0;
      end;
    end;
    //vCurRate := width/height
    vCurRate := (vColumnCount * (cColumnWidth + cBetweenColumns) - cBetweenColumns) / vRealMaxHeightInColumn;
    if Min(vCurRate, cSideRate) / Max(vCurRate, cSideRate) > Min(vBestRate, cSideRate) / Max(vBestRate, cSideRate) then
    begin
      vBestColumnCount := vColumnCount;
      vBestRate := vCurRate;
    end
    else
      Break;
  end;
  vHeightInColumn := vTotalHeight div vBestColumnCount;
  vCurColumnHeight := 0;
  vRealMaxHeightInColumn := 0;
  vRealColumnCount := 1;

  for i := 0 to Count - 1 do
  begin
    vEditor := TCustomVCLArea(Areas[i]);
    vEditor.PlaceIntoBounds(
      cBorder + (vRealColumnCount - 1) * (cColumnWidth + cBetweenColumns), cBorder + cBetweenRows + vCurColumnHeight,
      cColumnWidth, CalcLayoutPositionCount(vEditor) * (vOneRowHeight + cBetweenRows) - cBetweenRows);

    vCurColumnHeight := vCurColumnHeight + TVCLFieldArea(vEditor).Control.Height + cBetweenRows;
    if vCurColumnHeight >= vHeightInColumn then
    begin
      if vCurColumnHeight > vRealMaxHeightInColumn then
        vRealMaxHeightInColumn := vCurColumnHeight;
      vCurColumnHeight := 0;
      if i < Count - 1 then  // not last
        vRealColumnCount := vRealColumnCount + 1;
    end;
  end;

  Control.ClientWidth := (vRealColumnCount * (cColumnWidth + cBetweenColumns) - cBetweenColumns) + cBorder*2;
  Control.ClientHeight := vRealMaxHeightInColumn + cBorder * 2 + 8;
  if FIsForm and (FView.DefinitionKind <> dkDomain) then
    Control.ClientHeight := Control.ClientHeight + cServiceAreaHeight;
end;

procedure TCustomVCLArea.AssignAlignment(const AAlign: TAlignment);
begin
end;

procedure TCustomVCLArea.AssignFont(const AFont: TFont);
begin
end;

procedure TCustomVCLArea.AssignFromLayout(const ALayout: TBaseLayout);
var
  vForm: TForm;
  vAlignment: TAlignment;
begin
  if FIsForm then
  begin
    vForm := TForm(FControl);

    if ALayout.LayoutClass = 'TFrame' then
    begin
      if (ALayout.Tag and cEditFormResizable) > 0 then
      begin
        vForm.BorderStyle := bsSizeable;
        vForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      end;
      if (ALayout.Tag and cMainFormPositionDesign) > 0 then
        vForm.Position := poDesigned;

      if vForm.WindowState = wsNormal then
      begin
        if vForm.FormStyle <> fsMDIChild then
        begin
          vForm.ClientWidth := ALayout.ExtractInteger('ClientWidth');
          if FView.DefinitionKind = dkDomain then
            vForm.ClientHeight := ALayout.ExtractInteger('ClientHeight')
          else
            vForm.ClientHeight := ALayout.ExtractInteger('ClientHeight') + cServiceAreaHeight;
        end;
      end;

      vForm.Constraints.MinHeight := ALayout.ExtractInteger('MinHeight');
      vForm.Constraints.MinWidth := ALayout.ExtractInteger('MinWidth');

      if ALayout.Hint <> '' then
        vForm.Caption := ALayout.Hint;
      vForm.Color := ALayout.Color;
    end
    else begin
      vForm.BorderStyle := bsSizeable;
      vForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      vForm.Caption := ALayout.Caption;
    end;
  end
  else if ALayout.LayoutClass = 'TFrame' then
  begin
    ProcessFrame(ALayout);
  end
  else if ((ALayout.LayoutClass = 'TPanel') or (ALayout.LayoutClass = 'TMemo')) and (FControl is TControl) then
  begin
    PlaceIntoBounds(ALayout.Left, ALayout.Top, ALayout.Width, ALayout.Height);

    SetCaptionProperty(ALayout);
    AssignFont(ALayout.Font);

    Control.Anchors := ALayout.Anchors;
    Control.Align := ALayout.Align;
    Control.AlignWithMargins := ALayout.AlignWithMargins;
    Control.Margins := ALayout.Margins;
    if Control is TWinControl then
      TWinControl(Control).Padding := ALayout.Padding;

    vAlignment := taLeftJustify;

    if (ALayout.ClassName = 'TMemo') or (ALayout.LayoutClass = 'TPanel') then
      vAlignment := TAlignment(ALayout.ExtractInteger('Alignment'));

    if vAlignment = taRightJustify then
    begin
      if FControl.InheritsFrom(TLabel) then
        TLabel(FControl).Alignment := taRightJustify
      else if FControl.InheritsFrom(TEdit) then
        TEdit(FControl).Alignment := taRightJustify
      else if FControl.InheritsFrom(TStaticText) then
      begin
        TStaticText(FControl).Alignment := taRightJustify;
        TStaticText(FControl).AutoSize := False;
        TStaticText(FControl).Height := ALayout.Height;
        TStaticText(FControl).Width := ALayout.Width;
      end
      else if FControl.InheritsFrom(TCheckBox) then
        TCheckBox(FControl).Alignment := taRightJustify
      else if FControl.InheritsFrom(TRadioButton) then
        TRadioButton(FControl).Alignment := taRightJustify
      else
        AssignAlignment(vAlignment);
    end;

    if not ALayout.ExtractBoolean('ParentBackground') then
    begin
      TCrackedWinControl(FControl).Color := ALayout.Color;
      TCrackedWinControl(FControl).ParentColor := False;
      TCrackedWinControl(FControl).ParentBackground := False;
    end;

  end;
end;

procedure TCustomVCLArea.BeginUpdate;
begin
  if FControl is TWinControl then
    TWinControl(FControl).DisableAlign;

  if TInteractor(Interactor).Layout = 'mdi' then
    SendMessage(Application.MainForm.ClientHandle, WM_SETREDRAW, 0, 0);
end;

constructor TCustomVCLArea.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
  const AControl: TObject = nil; const ALayout: TBaseLayout = nil; const AParams: string = '');
begin
  FNeedCreateCaption := True;

  inherited Create(AParent, AView, AId, AIsService, AControl, ALayout, AParams);
end;

function TCustomVCLArea.CreateButtonDesc(const AText: string): TButtonDesc;
var
  vSplitter: TStrings;
begin
  Result := TButtonDesc.Create;
  vSplitter := CreateDelimitedList(AText, '@');
  try
    Result.Caption := vSplitter.Values['Caption'];
    Result.Hint := vSplitter.Values['Hint'];
    Result.View := vSplitter.Values['View'];
    Result.Layout := vSplitter.Values['Layout'];
    if vSplitter.Values['Action'] <> '' then
      Result.View := vSplitter.Values['Action'];
    Result.WorkArea := vSplitter.Values['WorkArea'];
    Result.Options := vSplitter.Values['Options'];
    Result.Id := vSplitter.Values['Id'];
    Result.ImageID := StrToIntDef(Trim(vSplitter.Values['ImageID']), 0);
    Result.ColorField := vSplitter.Values['Color'];
    Result.GroupField := vSplitter.Values['Group'];
  finally
    FreeAndNil(vSplitter);
  end;

  if (Pos('=', AText) = 0) then
    Result.View := Trim(AText);
end;

procedure TCustomVCLArea.CreateCaption(const AFieldDef: TFieldDef);
var
  vInteractor: TInteractor;
  vMarkRequiredFields: Boolean;
begin
  if not Assigned(AFieldDef) then
    Exit;

  vInteractor := TInteractor(FView.Interactor);

  FLabelPosition := lpTop;

  if FNeedCreateCaption then
  begin
    FCaption := TLabel.Create(nil);
    FCaption.Parent := Control.Parent;
    FCaption.Transparent := True;

    if Assigned(CreateParams) and (CreateParams.IndexOfName('Caption') >= 0) then
      FCaption.Caption := CreateParams.Values['Caption']
    else
      FCaption.Caption := GetFieldTranslation(AFieldDef);

    if Assigned(CreateParams) and (CreateParams.IndexOfName('Hint') >= 0) then
      FCaption.Hint := CreateParams.Values['Hint']
    else
      FCaption.Hint := GetFieldTranslation(AFieldDef, tpHint);

    FShowCaption := True;
    vMarkRequiredFields := StrToBoolDef(TDomain(vInteractor.Domain).UserSettings.GetValue('Core', 'MarkRequiredFields'), True);

    if AFieldDef.HasFlag(cRequired) then
    begin
      if vMarkRequiredFields then
        FCaption.Caption := FCaption.Caption + '**';
      FCaption.Hint := FCaption.Hint + vInteractor.Translate('txtRequired', 'Обязательное');
    end
    else if AFieldDef.HasFlag(cRecommended) then
    begin
      if vMarkRequiredFields then
        FCaption.Caption := FCaption.Caption + '*';
      FCaption.Hint := FCaption.Hint + vInteractor.Translate('txtRecommendedToFill', 'Рекомендуется заполнить');
    end;

    if Assigned(FView.Parent) and (FView.Parent.DefinitionKind = dkAction) and TDefinition(FView.Parent.Definition).HasFlag(ccInstantExecution) then
    begin
      FCaption.ParentFont := True;
      FCaption.Font.Size := 9;
    end
    else begin
      FCaption.Font.Color := clGray;
      FCaption.Font.Size := FCaption.Font.Size - 3;
      if FCaption.Font.Size < 8 then
        FCaption.Font.Size := 8;
    end;
  end;
end;

function TCustomVCLArea.CreateNavigationArea(const ASourcePanel: TBaseLayout; const AView: TView; const AParams: string): TUIArea;
var
  vNavArea: TNavigationArea;
  vViewType: string;
  procedure AddDefinitionToMenu(const AMenu: TBaseLayout; const ADefinition: TDefinition);
  var
    vDestItem: TBaseLayout;
  begin
    vDestItem := TBaseLayout.Create;
    vDestItem.Caption := ADefinition.Name;
    AMenu.AddChild(vDestItem);
  end;

  procedure ProcessChilds(const AMenu:TBaseLayout; const AParentArea: TUIArea; const AParentView: TView; const AParentObj: TObject; const ALevel: Integer);
  var
    i: Integer;
    vMI: TBaseLayout;
    vView: TView;
    vDefinition: TDefinition;
    vGroupArea: TCustomVCLArea;
    vImageID, vUrl, vLayoutName, vCaption, vHint: string;
    vImageIndex: Integer;
    vControl: TObject;
    vDefinitions: TList<TDefinition>;
  begin
    for i := 0 to AMenu.Items.Count - 1 do
    begin
      vMI := AMenu.Items[i];

      vControl := vNavArea.CreateItem(AParentObj, ALevel, vMI.Caption);

      if Pos('@', vMI.Caption) = 1 then
      begin
        vView := AParentView;
        vNavArea.AssingItemProperties(GetUrlParam(vMI.Caption, 'Caption'), GetUrlParam(vMI.Caption, 'Hint'), GetImageID(StrToIntDef(GetUrlParam(vMI.Caption, 'ImageIndex'), -1)));
      end
      else if vMI.Caption = '-' then
      begin
        vView := AParentView;
        vNavArea.AssingItemProperties('-', '', -1);
      end
      else
      begin
        vView := AParentView.BuildView(vMI.Caption);

        if vView.DefinitionKind in [dkAction, dkCollection] then
        begin
          vDefinition := TDefinition(vView.Definition);
          vUrl := vMI.Caption;

          if vView.DefinitionKind = dkAction then
          begin
            Assert(vMI.Items.Count = 0, 'Для действия ' + vMI.Caption + ' заданы дочерние элементы. Ошибка конфигурирования.');
            vNavArea.AssignItemOnClick(OnExecuteAction);
          end
          else if vView.DefinitionKind = dkCollection then
          begin
            if vMI.Items.Count = 0 then
            begin
              vNavArea.AssignItemOnClick(ExplicitNavigate);

              vLayoutName := GetUrlParam(vMI.Caption, 'Layout');

              if vLayoutName = '' then
                vLayoutName := 'Collection';
              vUrl := 'View=' + vView.InitialName + '@WorkArea=WorkArea@Layout=' + vLayoutName;
            end;
          end;

          vCaption := GetUrlParam(vMI.Caption, 'Caption');
          if vCaption= '' then
            vCaption := GetTranslation(vDefinition);

          vHint := GetUrlParam(vMI.Caption, 'Hint');
          if vHint = '' then
            vHint := vCaption;

          vImageID := GetUrlParam(vMI.Caption, 'ImageIndex');
          if vImageID = '' then
            vImageIndex := GetImageID(vDefinition._ImageID)
          else
            vImageIndex := GetImageID(StrToIntDef(vImageID, GetImageID(vDefinition._ImageID)));

          vNavArea.AssingItemProperties(vCaption, vHint, vImageIndex);

  //        TCustomVCLArea(vGroupArea).UpdateArea(dckViewStateChanged); это нужно?

        end
        else
          Assert(False, 'DefinitionKind must be dkAction or dkCollection for ' + vMI.Caption);
      end;

      vGroupArea := TVCLAreaClass(Self.ClassType).Create(AParentArea, vView, vUrl, False, vControl);

      AParentArea.AddArea(vGroupArea);

      if GetUrlParam(vMI.Caption, 'Id') = 'Libraries' then
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
            then
              AddDefinitionToMenu(vMI, vDefinition);
        finally
          FreeAndNil(vDefinitions);
        end;
      end;

      ProcessChilds(vMI, vGroupArea, vView, vControl, ALevel + 1);
    end;
  end;

begin
  Assert(Assigned(ASourcePanel.Menu), 'У панели с именем ' + ASourcePanel.ExtractString('Name') + ' задан Caption ' + ASourcePanel.ExtractString('Caption') + '. Это навигационная область. Для неё нужно указать PopupMenu.');

  vViewType := GetUrlParam(AParams, 'ViewType');

  if vViewType = 'NavBar' then
    vNavArea := GetNavigationAreaClass.Create(ASourcePanel, AView, Self, Interactor, AParams)
  else if vViewType = 'MainMenu' then
    vNavArea := TMainMenuArea.Create(ASourcePanel, AView, Self, Interactor, AParams)
  else if vViewType = 'ToolBar' then
    vNavArea := TToolBarArea.Create(ASourcePanel, AView, Self, Interactor, AParams)
  else if vViewType = 'TreeView' then
    vNavArea := TTreeViewArea.Create(ASourcePanel, AView, Self, Interactor, AParams)
  else
    vNavArea := TOneButtonArea.Create(ASourcePanel, AView, Self, Interactor, AParams);

  Result := TVCLAreaClass(Self.ClassType).Create(Self, vNavArea.View, Trim(ASourcePanel.Caption), False, vNavArea.Control);

  ProcessChilds(ASourcePanel.Menu, Result, AView, nil, 0);

  vNavArea.AfterCreate(Interactor);
end;

procedure TCustomVCLArea.DoActivate(const AUrlParams: string);
begin
  if (FControl is TForm) and (TForm(FControl).FormStyle = fsMDIChild) then
  begin
    if SameText(GetUrlParam(AUrlParams, 'State'), 'Max') then
      TForm(FControl).WindowState := wsMaximized
    else
    begin
      TForm(FControl).BringToFront;
    end;
  end;
end;

procedure TCustomVCLArea.DoClose(const AModalResult: Integer);
begin
  if FIsForm then
  begin
    if AModalResult = mrNone then
      PostMessage(TForm(FControl).Handle, WM_CLOSE, 0, 0)
    else begin
      TForm(FControl).Close;
      TForm(FControl).ModalResult := AModalResult;
    end;
  end;
end;

function TCustomVCLArea.LessThanUIState(const ADefinition: TDefinition; const ASession: TObject; const AState: TViewState): Boolean;
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

function TCustomVCLArea.AddDefinition(const AParent: TUIArea; const AView: TView; const AMenu: TMenuItem; const ADefinition: TDefinition;
  const ALayoutName: string = ''): TUIArea;
var
  vDestItem: TMenuItem;
  vLayoutName, vCaption: string;
begin
  vDestItem := TMenuItem.Create(nil);
  AMenu.Add(vDestItem);

  if ALayoutName = '' then
    vLayoutName := 'Collection'
  else
    vLayoutName := ALayoutName;
  vDestItem.Caption := GetTranslation(ADefinition);
  vDestItem.Hint := vDestItem.Caption;
  vDestItem.ImageIndex := GetImageID(ADefinition._ImageID);
  vDestItem.OnClick := ExplicitNavigate;

  vCaption := 'View=' + ADefinition.Name + '@WorkArea=WorkArea@Layout=' + vLayoutName;

  Result := TVCLAreaClass(Self.ClassType).Create(AParent, AView, vCaption, False, vDestItem);
  TCustomVCLArea(Result).UpdateArea(dckViewStateChanged);
  AParent.AddArea(Result);
end;

procedure TCustomVCLArea.CopyMenuItems(const AParent: TUIArea; const AView: TView; const ASrcMenu: TBaseLayout; const ADestMenu: TMenuItem);
var
  i: Integer;
  vCaption, vLayoutName: string;
  vParams: TStrings;
  vId: string;
  vImageID: string;
  vSrcItem: TBaseLayout;
  vDestItem: TMenuItem;
  vView: TView;
  vChildArea: TUIArea;
  vAction: TActionDef;
  vDefinition: TDefinition;
  vDefinitions: TList<TDefinition>;
  vForm: TForm;
begin
  for i := 0 to ASrcMenu.Items.Count - 1 do
  begin
    vSrcItem := ASrcMenu.Items[i];
    vCaption := vSrcItem.Caption;
    vDestItem := TMenuItem.Create(nil);
    vDestItem.ImageIndex := -1;
    ADestMenu.Add(vDestItem);

    if vCaption = '-' then
    begin
      vDestItem.Caption := vCaption;
      vDestItem.Enabled := False;
      vChildArea := TVCLAreaClass(Self.ClassType).Create(AParent, AParent.View, vCaption, False, vDestItem);
    end
    else if Pos('@', vCaption) > 0 then
    begin
      vParams := CreateDelimitedList(vCaption, '@');
      try
        vId := vParams.Values['Id'];
        vCaption := vParams.Values['Caption'];
      finally
        FreeAndNil(vParams);
      end;

      if vId <> '' then
        vDestItem.Caption := TInteractor(Interactor).Translate(vId, vCaption)
      else
        vDestItem.Caption := vCaption;

      vChildArea := TVCLAreaClass(Self.ClassType).Create(AParent, AParent.View, vSrcItem.Caption, False, vDestItem);
      if vId = 'Documents' then
      begin
        vDefinitions := TList<TDefinition>.Create;
        try
          TConfiguration(TInteractor(Interactor).Configuration).Definitions.DefinitionsByKind(vDefinitions, clkDocument);
          for vDefinition in vDefinitions do
            if not LessThanUIState(vDefinition, TInteractor(Interactor).Session, vsReadOnly)
              and not vDefinition.HasFlag(ccNotSave) and not vDefinition.HasFlag(ccHideInMenu)
            then
              AddDefinition(vChildArea, AView, vDestItem, vDefinition);
        finally
          FreeAndNil(vDefinitions);
        end;
      end
      else if vId = 'Libraries' then
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
            then
              AddDefinition(vChildArea, AView, vDestItem, vDefinition);
        finally
          FreeAndNil(vDefinitions);
        end;
      end
      else if vId = 'Windows' then
        vDestItem.Visible := TInteractor(Interactor).Layout = 'mdi';

      CopyMenuItems(vChildArea, AView, vSrcItem, vDestItem);

      if (vId = 'Windows') and vDestItem.Visible and Assigned(TCustomVCLArea(AParent.Parent).Control) then
      begin
        if TCustomVCLArea(AParent.Parent).Control is TForm then
        begin
          vForm := TForm(TCustomVCLArea(AParent.Parent).Control);
          if vForm.FormStyle = fsMDIForm then
            vForm.WindowMenu := vDestItem;
        end;
      end;
    end
    else begin
      vView := FUIBuilder.RootView.BuildView(vCaption);
      if vView.DefinitionKind = dkAction then
      begin
        vAction := TActionDef(vView.Definition);
        vDestItem.Caption := GetUrlParam(vCaption, 'Caption');
        if vDestItem.Caption = '' then
          vDestItem.Caption := GetTranslation(vAction);
        vDestItem.Hint := vDestItem.Caption;
        vImageID := Trim(GetUrlParam(vCaption, 'ImageID'));
        if vImageID = '' then
          vDestItem.ImageIndex := GetImageID(vAction._ImageID)
        else
          vDestItem.ImageIndex := GetImageID(StrToIntDef(vImageID, GetImageID(vAction._ImageID)));

        vDestItem.OnClick := OnExecuteAction;

        vChildArea := TVCLAreaClass(Self.ClassType).Create(AParent, vView, vCaption, False, vDestItem);
        TCustomVCLArea(vChildArea).UpdateArea(dckViewStateChanged);
      end
      else if vView.DefinitionKind = dkCollection then
      begin
        vDefinition := TDefinition(vView.Definition);

        vDestItem.Caption := GetUrlParam(vCaption, 'Caption');
        if vDestItem.Caption = '' then
          vDestItem.Caption := GetTranslation(vDefinition);
        vDestItem.Hint := vDestItem.Caption;
        vImageID := Trim(GetUrlParam(vCaption, 'ImageID'));
        if vImageID = '' then
          vDestItem.ImageIndex := GetImageID(vDefinition._ImageID)
        else
          vDestItem.ImageIndex := GetImageID(StrToIntDef(vImageID, GetImageID(vDefinition._ImageID)));
        vDestItem.OnClick := ExplicitNavigate;

        vLayoutName := GetUrlParam(vCaption, 'Layout');
        if vLayoutName = '' then
          vLayoutName := 'Collection';
        vCaption := 'View=' + vView.InitialName + '@WorkArea=WorkArea@Layout=' + vLayoutName;

        vChildArea := TVCLAreaClass(Self.ClassType).Create(AParent, vView, vCaption, False, vDestItem);
        TCustomVCLArea(vChildArea).UpdateArea(dckViewStateChanged);
      end
      else begin
        vView.CleanView;
        vDestItem.Caption := vCaption;
        vDestItem.Enabled := False;
        vChildArea := TVCLAreaClass(Self.ClassType).Create(AParent, AParent.View, vCaption, False, vDestItem);
      end;
    end;

    AParent.AddArea(vChildArea);
  end;
end;

procedure TCustomVCLArea.CopyPopupMenuItems(const AParent: TUIArea; const AView: TView; const ASrcMenu: TBaseLayout; const ADestMenu: TMenuItem);
var
  vActions: TList<TActionDef>;
  vReports: TList<TRTFReport>;
  vDefinitions: TList<TDefinition>;
  vId, vCaption: string;
  i, j: Integer;
  vDefinition: TDefinition;
  vSrcItem: TBaseLayout;
  vDestItem: TMenuItem;
  vChildItem: TMenuItem;
  vView: TView;
  vAction: TActionDef;
  vReport: TRTFReport;
  vChildArea: TCustomVCLArea;
  vParams: TStrings;
begin
  for i := 0 to ASrcMenu.Items.Count - 1 do
  begin
    vSrcItem := ASrcMenu.Items[i];
    vCaption := vSrcItem.Caption;

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
            vDestItem := TMenuItem.Create(nil);
            vDestItem.Caption := GetTranslation(vAction);
            vDestItem.Hint := vDestItem.Caption;
            vDestItem.ImageIndex := GetImageID(vAction._ImageID);
            vDestItem.OnClick := OnExecuteAction;
            vDestItem.RadioItem := vSrcItem.ExtractBoolean('RadioItem');
            vDestItem.GroupIndex := vSrcItem.ExtractInteger('GroupIndex');

            if Assigned(vView.DomainObject) and TEntity(vView.DomainObject).FieldExists('IsChecked') then
              vDestItem.AutoCheck := True;

            vChildArea := TVCLAreaClass(Self.ClassType).Create(AParent, vView, vCaption, False, vDestItem);
            vChildArea.UpdateArea(dckViewStateChanged);
            AParent.AddArea(vChildArea);

            ADestMenu.Add(vDestItem);
          end
          else
            vView.CleanView;
        end;
      end;

      if vActions.Count > 0 then
      begin
        vDestItem := TMenuItem.Create(nil);
        vDestItem.Caption := '-';
        ADestMenu.Add(vDestItem);
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
            vDestItem := TMenuItem.Create(nil);
            vDestItem.Caption := GetTranslation(vReport);
            vDestItem.Hint := vDestItem.Caption;
            vDestItem.ImageIndex := 31;
            vDestItem.OnClick := OnExecuteAction;

            vChildArea := TVCLAreaClass(Self.ClassType).Create(AParent, vView, vCaption, False, vDestItem);
            vChildArea.UpdateArea(dckViewStateChanged);
            AParent.AddArea(vChildArea);

            ADestMenu.Add(vDestItem);
          end
          else
            vView.CleanView;
        end;
      end;

      if vReports.Count > 0 then
      begin
        vDestItem := TMenuItem.Create(nil);
        vDestItem.Caption := '-';
        ADestMenu.Add(vDestItem);
      end;

      FreeAndNil(vReports);

      Continue;
    end
    else if Pos('@', vCaption) > 0 then
    begin
      vParams := CreateDelimitedList(vCaption, '@');
      try
        vId := vParams.Values['Id'];
        vCaption := vParams.Values['Caption'];
      finally
        FreeAndNil(vParams);
      end;

      vDestItem := TMenuItem.Create(nil);
      if vId <> '' then
        vDestItem.Caption := TInteractor(Interactor).Translate(vId, vCaption)
      else
        vDestItem.Caption := vCaption;

      vChildArea := TVCLAreaClass(Self.ClassType).Create(AParent, AParent.View, vSrcItem.Caption, False, vDestItem);
      AParent.AddArea(vChildArea);

      ADestMenu.Add(vDestItem);
      CopyPopupMenuItems(vChildArea, AView, vSrcItem, vDestItem);

      Continue;
    end
    else
      vView := AView.BuildView(vCaption);

    vDestItem := TMenuItem.Create(nil);

    if Assigned(vView) and (vView.DefinitionKind = dkAction) then
    begin
      vAction := TActionDef(vView.Definition);
      vDestItem.Caption := GetTranslation(vAction);
      vDestItem.Hint := vDestItem.Caption;
      vDestItem.ImageIndex := GetImageID(vAction._ImageID);

      if (vAction.Name = 'Add') and Assigned(vView.ParentDomainObject) and (vView.ParentDomainObject is TEntityList) then
      begin
        vDefinitions := TEntityList(vView.ParentDomainObject).ContentDefinitions;
        if vDefinitions.Count > 1 then
        begin
          for j := 0 to vDefinitions.Count - 1 do
          begin
            vDefinition := TDefinition(vDefinitions[j]);
            vChildItem := TMenuItem.Create(nil);
            vChildItem.Caption := GetTranslation(vDefinition);
            vChildItem.Tag := NativeInt(vDestItem);
            vChildItem.OnClick := OnActionMenuSelected;
            vDestItem.Add(vChildItem);
          end;
        end
        else
          vDestItem.OnClick := OnExecuteAction;
      end
      else
        vDestItem.OnClick := OnExecuteAction;

      vDestItem.RadioItem := vSrcItem.ExtractBoolean('RadioItem');
      vDestItem.GroupIndex := vSrcItem.ExtractInteger('GroupIndex');
      if Assigned(vView.DomainObject) and TEntity(vView.DomainObject).FieldExists('IsChecked') then
        vDestItem.AutoCheck := True;

      vChildArea := TVCLAreaClass(Self.ClassType).Create(AParent, vView, vCaption, False, vDestItem);
      vChildArea.UpdateArea(dckViewStateChanged);

      AParent.AddArea(vChildArea);
    end
    else begin
      if Assigned(vView) and (vView.DefinitionKind = dkUndefined) then
        vView.CleanView;

      vDestItem.Caption := vCaption;
      vDestItem.Enabled := False;
    end;

    ADestMenu.Add(vDestItem);
  end;
end;

function TCustomVCLArea.DoGetDescription: string;
begin
  Result := inherited DoGetDescription;
  if Assigned(FControl) and (FControl is TControl) then
  begin
    if not Control.Visible then
      Result := Result + ' HID';
    if not Control.Enabled then
      Result := Result + ' DIS';
  end;
end;

procedure TCustomVCLArea.EndUpdate;
begin
  if FControl is TWinControl then
    TWinControl(FControl).EnableAlign;

  if TInteractor(Interactor).Layout = 'mdi' then
  begin
    SendMessage(Application.MainForm.ClientHandle, WM_SETREDRAW, 1, 0);
    RedrawWindow(Application.MainForm.ClientHandle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

procedure TCustomVCLArea.ExplicitNavigate(Sender: TObject);
var
  vButtonDesc: TButtonDesc;
  vArea: TUIArea;
  vView: TView;
  vParentHolder: TObject;
  vParentObject: TObject;
  vIsSlave: Boolean;
  vCaption: string;
  vOptions, vWorkArea: string;
begin
  vArea := TCustomVCLArea(TWinControl(Sender).Tag);
  vButtonDesc := CreateButtonDesc(vArea.Id);
  vOptions := vButtonDesc.GenerateOptions('&');
  try
    FUIBuilder.LastArea := vArea;
    if vButtonDesc.Layout <> '' then
    begin
      if vButtonDesc.View <> '' then
        vView := FUIBuilder.RootView.BuildView(vButtonDesc.View)
      else
        vView := nil;
      vCaption := vButtonDesc.Caption;
      if vCaption = '' then
        vCaption := vArea.View.QueryParameter('Caption');
      if Trim(vArea.View.QueryText) <> '' then
        vOptions := vArea.View.QueryText;
      vOptions := RemoveUrlParam(vOptions, 'Layout');
      vWorkArea := GetUrlParam(vOptions, 'ContentWorkArea', vButtonDesc.WorkArea);
      FUIBuilder.Navigate(vView, vWorkArea, vButtonDesc.Layout, vOptions, nil, nil, vCaption);
    end
    else if (vButtonDesc.View <> '') and (Copy(vButtonDesc.View, 1, 1) <> '#') then
    begin
      vView := FUIBuilder.RootView.BuildView(vButtonDesc.View);
      vIsSlave := (vView.Name = 'Save') or SameText(vArea.QueryParameter('place'), 'embedded');
      if not vIsSlave then
      begin
        vParentObject := vView.ParentDomainObject;

        if Assigned(vParentObject) and (vParentObject is TEntityList) then
          vIsSlave := TEntityList(vParentObject).FillerKind = lfkList
        else if Assigned(vView.Parent) then
        begin
          vParentObject := vView.Parent.ParentDomainObject;
          if Assigned(vParentObject) and (vParentObject is TEntityList) then
            vIsSlave := TEntityList(vParentObject).FillerKind = lfkList;
        end;
      end;

      if vIsSlave then
        vParentHolder := Holder
      else
        vParentHolder := nil;

      vView.ExecuteAction(vParentHolder);
    end;
  finally
    vButtonDesc.Free;
  end;
end;

function TCustomVCLArea.GetComponent: TComponent;
begin
  Result := TComponent(FControl)
end;

function TCustomVCLArea.GetControl: TControl;
begin
  if FControl is TControl then
    Result := TControl(FControl)
  else
    Result := nil;
end;

function TCustomVCLArea.GetName: string;
begin
  if not Assigned(FControl) then
    Result := 'NULL'
  else if not (FControl is TControl) then
    Result := FControl.ClassName + ': ' + Id
  else if TCrackedWinControl(FControl).Caption <> '' then
    Result := FControl.ClassName + ': ' + Id + ' (' +  TCrackedWinControl(FControl).Caption + ')'
  else
    Result := FControl.ClassName + ': ' + Id;
end;

function TCustomVCLArea.GetNavigationAreaClass: TNavigationAreaClass;
begin
  Result := nil;
end;

function TCustomVCLArea.GetSelectedIndexFromControl(const AControl: TComponent; const AMenuItem: TMenuItem): Integer;
begin
  Result := -1;
end;

procedure TCustomVCLArea.OnActionMenuSelected(Sender: TObject);
var
  vControl: TComponent;
  vMenuItem: TMenuItem;
  vArea: TCustomVCLArea;
  vSelectedIndex: Integer;
begin
  vMenuItem := TMenuItem(Sender);
  vControl := TComponent(vMenuItem.Tag);
  vArea := TCustomVCLArea(vControl.Tag);
  if not Assigned(vArea) then
    Exit;
  if not Assigned(vArea.View) then
    Exit;
  if not Assigned(vArea.View.DomainObject) then
    Exit;

  if vControl is TMenuItem then
    vSelectedIndex := TMenuItem(vControl).IndexOf(vMenuItem)
  else
    vSelectedIndex := GetSelectedIndexFromControl(vControl, vMenuItem);

  Assert(vSelectedIndex >= 0, 'Выбран неизвестный пункт меню');
  TEntity(vArea.View.DomainObject)._SetFieldValue(FSession.NullHolder, 'SelectedIndex', vSelectedIndex);
  OnExecuteAction(vControl);
end;

procedure TCustomVCLArea.OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
var
  vArea: TUIArea;
  vView: TView;
  vCloseView: TView;
  vInteractor: TInteractor;
  vCanBeClosed: Boolean;
  vCloseProc: TProc;
begin
  vArea := TUIArea(Pointer(TForm(Sender).Tag));
  vView := vArea.View;
  vCloseView := vView.BuildView('Close');
  vCloseView.AddListener(vArea);
  vCloseProc := nil;

  try
    vInteractor := TInteractor(vArea.Interactor);

    vCanBeClosed := not (Assigned(vCloseView) and (TCheckActionFlagsFunc(TConfiguration(vInteractor.Configuration).
      CheckActionFlagsFunc)(vCloseView) <> vsFullAccess));

    if vCanBeClosed then
    begin
      vCloseProc := TCustomVCLArea(vArea).OnClose;
      TCustomVCLArea(vArea).SetControl(nil);
      FUIBuilder.RootArea.RemoveArea(vArea);
    end
    else
      Action := caNone;
  finally
    vCloseView.RemoveListener(vArea);
  end;

  if Assigned(vCloseProc) then
    vCloseProc;
end;

procedure TCustomVCLArea.PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not (FControl is TControl) then Exit;

  Control.SetBounds(ALeft, ATop, AWidth, AHeight);
  FOriginLeft := ALeft;
  FOriginTop := ATop;
  PlaceLabel;
end;

procedure TCustomVCLArea.PlaceLabel;
var
  vSpace: Integer;
begin
  if FCaption = nil then
    Exit;

  if FLabelPosition = lpTop then
  begin
    FCaption.Left := Control.Left;
    FCaption.Top := Control.Top - FCaption.Height - 4;
    FCaption.AutoSize := True;
    FCaption.Layout := tlTop;
  end
  else if FLabelPosition = lpLeft then
  begin
    vSpace := FCaption.Width + 8;
    FCaption.Left := Control.Left - vSpace;
    FCaption.Top := Control.Top + 3;
    FCaption.AutoSize := False;
    //FCaption.Layout := tlCenter;
    //FCaption.Height := Control.Height;
  end;
  FCaption.Parent := Control.Parent;
end;

procedure TCustomVCLArea.ProcessFrame(const AFrame: TBaseLayout);
begin
end;

procedure TCustomVCLArea.RefillArea(const AKind: Word);
begin
  inherited RefillArea(AKind);

  if Assigned(FCaption) then
    FCaption.Visible := FShowCaption and (FView.State > vsHidden);
end;

destructor TCustomVCLArea.Destroy;
var
  vControl: TComponent;
begin
  FOnClose := nil;

  if Assigned(FCaption) then
  begin
    FCaption.Parent := nil;
    FreeAndNil(FCaption);
  end;

  //??? надо проверить
  if Assigned(FPopupMenu) then
    FreeAndNil(FPopupMenu);

  vControl := Component;
  inherited Destroy;

  if not Assigned(vControl) then
    Exit;

  if FIsForm then
  begin
    if TForm(vControl).FormStyle = fsMDIChild then
      FreeAndNil(vControl)
  end
  else
    FreeAndNil(vControl);
end;

procedure TCustomVCLArea.SaveLayoutToFile(const AFileName: string);
var
  vLayout: TUILayout;
begin
  vLayout := TUILayout.Create;
  try
    vLayout.Build(Self);
    vLayout.SaveToDFM(AFileName);
  finally
    vLayout.Free;
  end;
end;

procedure TCustomVCLArea.SetCaptionProperty(const ALayout: TBaseLayout);
begin
  if not Assigned(FCaption) then
    Exit;

  if Assigned(ALayout) then
  begin
    FShowCaption := ALayout.ExtractBoolean('ShowCaption');
    FCaption.Visible := FShowCaption and (FView.State > vsHidden);
    if akTop in ALayout.Anchors then
      FCaption.Anchors := [akLeft, akTop]
    else
      FCaption.Anchors := [akLeft, akBottom];

    if ALayout.DoubleBuffered then
      SetLabelPosition(lpLeft);
  end;
end;

procedure TCustomVCLArea.SetControl(const AControl: TObject);
begin
  if not (AControl is TComponent) then
    Exit;

  if Assigned(FControl) then
  begin
    TCrackedControl(FControl).Tag := 0;
    if FControl is TWinControl then
    begin
      TCrackedWinControl(FControl).OnEnter := nil;
      TCrackedWinControl(FControl).OnExit := nil;
    end;
  end;

  inherited SetControl(AControl);

  if Assigned(FControl) then
  begin
    TCrackedControl(FControl).Tag := NativeInt(Self);
    if FControl is TWinControl then
    begin
      TCrackedWinControl(FControl).OnEnter := OnEnter;
      TCrackedWinControl(FControl).OnExit := OnExit;
    end;
  end;

  FIsForm := FControl is TForm;
  FIsAutoReleased := FControl is TMenuItem;
end;

procedure TCustomVCLArea.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  PlaceLabel;
end;

procedure TCustomVCLArea.SetParent(const Value: TUIArea);
begin
  inherited SetParent(Value);

  if not Assigned(Control) then
    Exit;

  // Установка родителя для контрола
  if Control is TTabSheet then
  begin
    if Assigned(Value) then
    begin
     // Assert(TCustomVCLArea(Value).Control is TPageControl, TCustomVCLArea(Value).Name + ': ' + TCustomVCLArea(Value).Control.ClassName);
     // if TCustomVCLArea(Value).Control is TPageControl then
        TTabSheet(Control).PageControl := TPageControl(TCustomVCLArea(Value).Control)
     // else
     //   TTabSheet(Control).Caption := TCustomVCLArea(Value).Name + ': ' + TCustomVCLArea(Value).Control.ClassName
    end
    else
      TTabSheet(Control).PageControl := nil;
  end
  else if FIsForm or not Assigned(Value) then
    Control.Parent := nil
  else
    Control.Parent := TWinControl(TCustomVCLArea(Value).Control);
end;

procedure TCustomVCLArea.SetViewState(const AValue: TViewState);
var
  vBoolValue: Boolean;
begin
  if FControl is TMenuItem then
  begin
    TMenuItem(FControl).Visible := (AValue > vsHidden) and FStyle.Visible;
    TMenuItem(FControl).Enabled := (AValue > vsDisabled) and FStyle.Enabled;
  end
  else if (FControl is TControl) and (not FIsForm) then
  begin
    vBoolValue := (AValue > vsHidden) and FStyle.Visible;

    if Control.Visible <> vBoolValue then
      Control.Visible := vBoolValue;

    if Control.Enabled <> vBoolValue then
      Control.Enabled := vBoolValue;
  end;
end;

procedure TCustomVCLArea.UnbindContent;
begin
  if FIsAutoReleased then
    FControl := nil;
end;

procedure TCustomVCLArea.UpdateArea(const AKind: Word; const AParameter: TEntity = nil);
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

{ TVCLFieldArea }

procedure TVCLFieldArea.BeforeContextMenuShow(Sender: TObject);
var
  vMenu: TPopupMenu;
  vMenuItem: TMenuItem;

  procedure CheckMenuItems(const AMenuItem: TMenuItem);
  var
    vChildItem: TMenuItem;
    vArea: TCustomVCLArea;
  begin
    if AMenuItem.Count > 0 then
    begin
      for vChildItem in AMenuItem do
        CheckMenuItems(vChildItem);
      Exit;
    end;

    vArea := TCustomVCLArea(AMenuItem.Tag);
    if not Assigned(vArea) then
      Exit;
    if not Assigned(vArea.View) then
      Exit;
    if vArea.View.DefinitionKind <> dkAction then
      Exit;
    if not Assigned(vArea.View.DomainObject) then
      Exit;

    if TEntity(vArea.View.DomainObject).FieldExists('IsChecked') then
      AMenuItem.Checked := TEntity(vArea.View.DomainObject)['IsChecked'];
  end;

begin
  vMenu := TPopupMenu(Sender);
  for vMenuItem in vMenu.Items do
    CheckMenuItems(vMenuItem);
end;

constructor TVCLFieldArea.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
  const AControl: TObject = nil; const ALayout: TBaseLayout = nil; const AParams: string = '');
var
  vPopupArea: TCustomVCLArea;
  vPopupMenu: TPopupMenu;
begin
  if AView.DefinitionKind in [dkListField, dkObjectField, dkSimpleField, dkComplexField] then
  begin
    FFieldDef := TFieldDef(AView.Definition);
    FDefinitionName := FFieldDef.Name;
  end
  else begin
    FFieldDef := nil;
    FDefinitionName := TDefinition(AView.Definition).Name;
  end;

  FId := FDefinitionName;
  FUId := FDefinitionName;

  inherited Create(AParent, AView, AId, AIsService, AControl, ALayout, AParams);

  Assert(Assigned(FControl), 'Не создан контрол для ' + FDefinitionName);

  // Установка контекстного меню
  if Assigned(ALayout) and (ALayout.LayoutClass = 'TPanel') and Assigned(ALayout.Menu)
    and Assigned(FControl) and (FControl is TControl)
  then begin
    vPopupArea := TCustomVCLArea(Parent.AreaById('Popup')); // TPopupMenu always created on form (todo: get TForm area)
    if Assigned(vPopupArea) then
    begin
      vPopupMenu := TPopupMenu(vPopupArea.Component);
      if not Assigned(vPopupMenu.OnPopup) then
        vPopupMenu.OnPopup := BeforeContextMenuShow;
      TCrackedWinControl(Control).PopupMenu := vPopupMenu;
    end;
  end;

  // Нужно делать после задания родителя, так как надпись использует родительский шрифт
  CreateCaption(FFieldDef);

  if Assigned(Component) then
  begin
    Component.Name := FDefinitionName;
    if Component is TPanel then
      TPanel(Component).Caption := '';
  end;
end;

procedure TVCLFieldArea.Deinit;
begin
  DoDeinit;
end;

destructor TVCLFieldArea.Destroy;
begin
  DoBeforeFreeControl;

  if Assigned(FCaption) then
  begin
    FCaption.Parent := nil;
    FreeAndNil(FCaption);
  end;

  inherited Destroy;
end;

procedure TVCLFieldArea.DoBeforeFreeControl;
begin
end;

function TVCLFieldArea.DoCreateChildArea(const ALayout: TBaseLayout; const AView: TView; const AParams: string; const AOnClose: TProc): TUIArea;
var
  vForm: TForm;
  vTab: TTabSheet;
  vStartPageName: string;
begin
  Result := nil;

  if not Assigned(ALayout) then
    Exit;

  if ALayout.LayoutClass = 'TTabSheet' then
  begin
    if (TInteractor(AView.Interactor).Layout = 'mdi') and (ALayout.Tag = 11) then
    begin
      vForm := TForm.Create(nil);
      vForm.Caption := ALayout.Caption;
      vForm.Position := poDefault;
      vForm.FormStyle := fsMDIChild;
      vForm.OnClose := OnCloseMDIForm;
      vForm.ShowHint := True;
      if AView.DefinitionKind in [dkCollection, dkAction, dkEntity] then
        TDragImageList(TInteractor(Interactor).Images[16]).GetIcon(GetImageID(TDefinition(AView.Definition)._ImageID), vForm.Icon);
      Result := TCustomVCLArea.Create(Self, AView, ALayout.Name, False, vForm);
      TCustomVCLArea(Result).OnClose := AOnClose;
    end
    else
    begin
      vTab := TTabSheet.Create(Component);
      vTab.Caption := ALayout.Caption;
      vTab.ImageIndex := GetImageID(ALayout.ExtractInteger('ImageIndex'));

      vStartPageName := TDomain(FView.Domain).Settings.GetValue('Core', 'StartPage', '');
      vTab.TabVisible := ALayout.ExtractBoolean('TabVisible');

      Result := TCustomVCLArea.Create(Self, AView, ALayout.Name, False, vTab);
    end;
  end;
end;

procedure TVCLFieldArea.DoDeinit;
begin
end;

procedure TVCLFieldArea.DoOnChange;
begin
end;

procedure TVCLFieldArea.FillEditor;
begin
end;

procedure TVCLFieldArea.FocusedChanged(const AFocused: Boolean);
begin
  if Assigned(FCaption) then
    PlaceLabel;

  if not AFocused then
    Validate;
end;

function TVCLFieldArea.GetDefaultFocusedChild: TWinControl;
begin
  Result := TWinControl(FControl);
end;

function TVCLFieldArea.GetFocused: Boolean;
begin
  Result := TWinControl(FControl).Focused;
end;

function TVCLFieldArea.GetLayoutPositionCount: Integer;
begin
  // Переделать
  Result := 1;
end;

function TVCLFieldArea.GetName: string;
begin
  Result := FDefinitionName;
end;

function TVCLFieldArea.GetNewValue: Variant;
begin
  Result := Null;
end;

procedure TVCLFieldArea.OnChange(Sender: TObject);
var
  vEntity: TEntity;
begin
  vEntity := TEntity(FView.ParentDomainObject);
  if not TCanChangeFieldFunc(TDomain(FView.Domain).Configuration.CanChangeFieldFunc)(FView, vEntity, FDefinitionName, GetNewValue) then
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

  Validate;
end;

procedure TVCLFieldArea.RefillArea(const AKind: Word);
var
  vEntity: TEntity;
begin
  if Assigned(FCaption) then
    FCaption.Visible := FShowCaption and (FView.State > vsHidden);

  vEntity := TEntity(FView.ParentDomainObject);
  if not Assigned(vEntity) then
    Exit;

  try
    SwitchChangeHandlers(nil);
    if not Assigned(FView.ParentDomainObject) then
      DoDeinit
    else begin
      FillEditor;
      Validate;
    end;
    SwitchChangeHandlers(OnChange);
  except
    TInteractor(FView.Interactor).ShowMessage('Error in FillEditorFromModel, Field: ' + FFieldDef.Name);
  end;
end;

procedure TVCLFieldArea.SetFieldEntity(const AEntity: TEntity);
begin
  FView.SetFieldEntity(Holder, AEntity);
end;

procedure TVCLFieldArea.SetFieldStream(const AStream: TStream);
begin
  FView.SetFieldStream(Holder, AStream);
end;

procedure TVCLFieldArea.SetFieldValue(const AValue: Variant);
begin
  FView.SetFieldValue(Holder, AValue);
end;

procedure TVCLFieldArea.SetFocused(const Value: Boolean);
var
  vControl: TWinControl;
begin
  if Value and TWinControl(FControl).CanFocus then
  begin
    vControl := GetDefaultFocusedChild;
    if Assigned(vControl) then
      vControl.SetFocus;
  end;
end;

procedure TVCLFieldArea.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
end;

procedure TVCLFieldArea.Validate;
var
  vEntity: TEntity;
begin
  vEntity := TEntity(FView.ParentDomainObject);
  if not Assigned(vEntity) then
    Exit;

//  SetWarningVisible(vEntity.FieldByName(FFieldDef.Name).ValidationStatus = vsInvalid);
end;

{ TButtonDesc }

function TButtonDesc.GenerateOptions(const ADelimiter: Char): string;
var
  vResult: TStrings;
begin
  vResult := CreateDelimitedList('', ADelimiter);
  try
    if Caption <> '' then
      vResult.Add('Caption=' + Caption);
    if Hint <> '' then
      vResult.Add('Hint=' + Hint);
    if Id <> '' then
      vResult.Add('Id=' + Id);
    if ImageID > 0 then
      vResult.Add('ImageID=' + IntToStr(ImageID));
    if ColorField <> '' then
      vResult.Add('Color=' + ColorField);
    if GroupField <> '' then
      vResult.Add('Group=' + GroupField);
  finally
    Result := vResult.DelimitedText;
    FreeAndNil(vResult);
  end;
end;

{ TLayoutParam }

constructor TLayoutParam.Create(const AName: string);
begin
  Name := AName;
end;

{ TUILayout }

function TUILayout.CreateChild: TUILayout;
begin
  Result := TUILayout.Create;
  Result.FParent := Self;
  Result.FLevel := FLevel + 1;
  FChildLayouts.Add(Result);
end;

procedure TUILayout.Build(const AVCLArea: TCustomVCLArea);
  procedure Process(const ALayout: TUILayout; const AVCLArea: TCustomVCLArea);
  var
    i: Integer;
  begin
    if Assigned(AVCLArea.Control) then
      ALayout.FillLayoutParams(ALayout, AVCLArea.Control);
    for i := 0 to AVCLArea.Count - 1 do
      Process(ALayout.CreateChild, TCustomVCLArea(AVCLArea[i]));
  end;
begin
  Clear;
  Process(Self, AVCLArea);
end;

procedure TUILayout.Clear;
var
  i: Integer;
begin
  for i := 0 to FChildLayouts.Count - 1 do
    TUILayout(FChildLayouts[i]).Free;
  FChildLayouts.Clear;

  for i := 0 to FParams.Count - 1 do
    TLayoutParam(FParams[i]).Free;
  FParams.Clear;
end;

constructor TUILayout.Create;
begin
  FName := 'emptyname';
  FType := 'emptytype';
  FChildLayouts := TList.Create;
  FParams := TList.Create;
end;

function TUILayout.FindChild(const AName: string): TUILayout;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FChildLayouts.Count - 1 do
    if TUILayout(FChildLayouts[i]).FName = AName then
    begin
      Result := TUILayout(FChildLayouts[i]);
      Break;
    end;
end;

destructor TUILayout.Destroy;
begin
  Clear;
  FreeAndNil(FParams);
  FreeAndNil(FChildLayouts);
  inherited;
end;

procedure TUILayout.GenerateDFMText(const AText: TStrings; const ALevel: Integer);
var
  i: Integer;
  vSelfIndent, vChildIndent: string;
  vParam: TLayoutParam;
begin
  vSelfIndent := DupeString('  ', ALevel);
  vChildIndent := DupeString('  ', ALevel + 1);
  AText.Append(vSelfIndent + 'object ' + FName + ': ' + FType);
  for i := 0 to FParams.Count - 1 do
  begin
    vParam := TLayoutParam(FParams[i]);
    AText.Append(vChildIndent +  vParam.Name + ' = ' + VarToString(vParam.Value, ''));
  end;
  for i := 0 to FChildLayouts.Count - 1 do
    TUILayout(FChildLayouts[i]).GenerateDFMText(AText, ALevel + 1);
  AText.Append(vSelfIndent + 'end');
end;

procedure TUILayout.GeneratePASText(const AText: TStrings);
var
  vStr: string;
begin
  vStr :=
    'unit ' + FName + ';' + #13#10#13#10 +
    'interface' + #13#10#13#10 +
    'uses' + #13#10 +
    '  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms;'#13#10#13#10 +
    'type'#13#10 +
    '  ' + FType + ' = class(TFrame)'#13#10 +
    '  end;'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    'end.';
  AText.Append(vStr);
end;

function TUILayout.GenerateUniqueName(const AComponent: TComponent): string;
begin
  Result := Copy(AComponent.ClassName, 2, Length(AComponent.ClassName)) + IntToStr(FLevel);
  if Assigned(FParent) then
    while FParent.FindChild(Result) <> nil do
      Result := Result + 'a';
end;

procedure TUILayout.FillLayoutParams(const ALayout: TUILayout; const AComponent: TComponent);
var
  vName: string;
begin
  vName := AComponent.Name;
  if Length(vName) = 0 then
    vName := GenerateUniqueName(AComponent);

  ALayout.FName := vName;
  if AComponent is TForm then
    ALayout.FType := AComponent.ClassName
  else
    ALayout.FType := 'TPanel';

  if AComponent is TControl then
  begin
    ALayout.ParamByName('Width').Value := TControl(AComponent).Width;
    ALayout.ParamByName('Height').Value := TControl(AComponent).Height;
    ALayout.ParamByName('Left').Value := TControl(AComponent).Left;
    ALayout.ParamByName('Top').Value := TControl(AComponent).Top;
  end;

  if AComponent is TWinControl then
    if not (AComponent is TForm) then
      ALayout.ParamByName('Caption').Value := QuotedStr(vName);

  if AComponent is TPageControl then
    ALayout.ParamByName('PagePosition').Value := TPageControl(AComponent).TabPosition;

  if AComponent is TImage then
    ALayout.ParamByName('Stretch').Value := TImage(AComponent).Stretch;

  if AComponent is TPanel then
  begin
    if (not TPanel(AComponent).ParentColor) then
      ALayout.ParamByName('Color').Value := TPanel(AComponent).Color;
  end;
end;

function TUILayout.ParamByName(const AName: string): TLayoutParam;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FParams.Count - 1 do
    if TLayoutParam(FParams[i]).Name = AName then
    begin
      Result := TLayoutParam(FParams[i]);
      Break;
    end;
  if Result = nil then
  begin
    Result := TLayoutParam.Create(AName);
    FParams.Add(Result);
  end;
end;

procedure TUILayout.SaveToDFM(const AFileName: string);
var
  vFileName: string;
  vFile: TStringList;
begin
  vFileName := AFileName + '.dfm';
  vFile := TStringList.Create;
  try
    GenerateDFMText(vFile, 0);
    vFile.SaveToFile(vFileName);
  finally
    vFile.Free;
  end;

  vFileName := AFileName + '.pas';
  vFile := TStringList.Create;
  try
    GeneratePASText(vFile);
    vFile.SaveToFile(vFileName);
  finally
    vFile.Free;
  end;
end;

{ TNavigationArea }

procedure TNavigationArea.AfterCreate(const AInteractor: TObject);
begin
  DoAfterCreate(AInteractor);
end;

procedure TNavigationArea.AssignItemOnClick(const AHandler: TNotifyEvent);
begin
  DoAssignItemOnClick(AHandler);
end;

procedure TNavigationArea.AssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer);
begin
  DoAssingItemProperties(ACaption, AHint, AImageIndex);
end;

constructor TNavigationArea.Create(const ASource: TBaseLayout; const AView: TView; const AArea: TCustomVCLArea; const AInteractor: TObject; const AParams: string);
begin
  FView := AView;
  FParams := AParams;
  FParentArea := AArea;
end;

function TNavigationArea.CreateItem(const AParentObj: TObject; const ALevel: Integer; const AParams: string): TObject;
begin
  FCurrentLevel := ALevel;
  Result := DoCreateItem(AParentObj, AParams);
end;

destructor TNavigationArea.Destroy;
begin
  inherited;
end;

procedure TNavigationArea.DoAfterCreate(const AInteractor: TObject);
begin
end;

{ TOneButtonArea }

constructor TOneButtonArea.Create(const ASource: TBaseLayout; const AView: TView; const AArea: TCustomVCLArea; const AInteractor: TObject; const AParams: string);
var
  vImageSize: Integer;
  vActionName: string;
  vAction: TActionDef;
begin
  inherited;

  FButton := TButton.Create(nil);
  FButton.Style := bsSplitButton;
  FButton.Width := ASource.Width;
  FButton.Height := ASource.Height;
  FButton.Left := ASource.Left;
  FButton.Top := ASource.Top;
  FButton.Font.Assign(ASource.Font);

  vImageSize := StrToIntDef(GetUrlParam(AParams, 'ImageSize'), 16);
  FButton.Images := TDragImageList(TInteractor(AInteractor).Images[vImageSize]);

  vActionName := GetUrlParam(AParams, 'Action');
  if Length(vActionName) > 0 then
  begin
    FView := AView.BuildView(vActionName);
    Assert(FView.DefinitionKind = dkAction);

    vAction := TActionDef(FView.Definition);
    FButton.ImageIndex := AArea.GetImageID(vAction._ImageID);
    FButton.Caption := AArea.GetTranslation(vAction, tpCaption);
    FButton.Hint := AArea.GetTranslation(vAction, tpHint);
    FButton.OnClick := AArea.OnExecuteAction;
  end
  else
  begin
    FButton.ImageIndex := AArea.GetImageID(StrToIntDef(GetUrlParam(AParams, 'ImageIndex'), -1));
    FButton.Caption := GetUrlParam(AParams, 'Caption');
    FButton.Hint := GetUrlParam(AParams, 'Hint');
    FButton.OnClick := OnClick;
  end;

  FMenu := TPopupMenu.Create(nil);
  FMenu.Images := TDragImageList(TInteractor(AInteractor).Images[16]);
  FButton.PopupMenu := FMenu;
  FButton.DropDownMenu := FMenu;
end;

destructor TOneButtonArea.Destroy;
begin
  FreeAndNil(FButton);
  inherited;
end;

procedure TOneButtonArea.DoAssignItemOnClick(const AHandler: TNotifyEvent);
begin
  FItem.OnClick := AHandler;
end;

procedure TOneButtonArea.DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer);
begin
  FItem.Caption := ACaption;
  FItem.Hint := AHint;
  FItem.ImageIndex := AImageIndex;
end;

function TOneButtonArea.DoCreateItem(const AParentObj: TObject; const AParams: string): TObject;
begin
  FItem := TMenuItem.Create(nil);
  if Assigned(AParentObj) then
  begin
    Assert(AParentObj is TMenuItem);
    TMenuItem(AParentObj).Add(FItem);
  end
  else
    FMenu.Items.Add(FItem);
  Result := FItem;
end;

function TOneButtonArea.GetControl: TObject;
begin
  Result := FButton;
end;

procedure TOneButtonArea.OnClick(Sender: TObject);
var
  vPoint: TPoint;
begin
  vPoint := FButton.ClientToScreen(Point(0, FButton.Height));
  FMenu.Popup(vPoint.X, vPoint.Y);
end;

{ TMainMenuArea }

constructor TMainMenuArea.Create(const ASource: TBaseLayout; const AView: TView; const AArea: TCustomVCLArea; const AInteractor: TObject; const AParams: string);
begin
  inherited;
  FMenu := TMainMenu.Create(AArea.Component);
  FMenu.Images := TDragImageList(TInteractor(AInteractor).Images[16]);
end;

destructor TMainMenuArea.Destroy;
begin
  FreeAndNil(FMenu);
  inherited;
end;

procedure TMainMenuArea.DoAssignItemOnClick(const AHandler: TNotifyEvent);
begin
  FMenuItem.OnClick := AHandler;
end;

procedure TMainMenuArea.DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer);
begin
  FMenuItem.Caption := ACaption;
  FMenuItem.Hint := AHint;
  FMenuItem.ImageIndex := AImageIndex;
end;

function TMainMenuArea.DoCreateItem(const AParentObj: TObject; const AParams: string): TObject;
var
  vId: string;
  vForm: TForm;
begin
  FMenuItem := TMenuItem.Create(nil);
  Result := nil;
  if FCurrentLevel = 0 then
  begin
    FMenu.Items.Add(FMenuItem);

    Result := FMenuItem;
  end
  else
  begin
    if Assigned(AParentObj) and (AParentObj is TMenuItem) then
    begin
      TMenuItem(AParentObj).Add(FMenuItem);
      Result := FMenuItem;
    end;
  end;

  FMenuItem.Visible := TInteractor(FView.Interactor).Layout = 'mdi';
  vId := GetUrlParam(AParams, 'Id');
  if (vId = 'Windows') and FMenuItem.Visible and Assigned(FParentArea.Control) then
  begin
    if FParentArea.Control is TForm then
    begin
      vForm := TForm(FParentArea.Control);
      if vForm.FormStyle = fsMDIForm then
        vForm.WindowMenu := FMenuItem;
    end;
  end;
end;

function TMainMenuArea.GetControl: TObject;
begin
  Result := FMenu;
end;

{ TToolBarArea }

constructor TToolBarArea.Create(const ASource: TBaseLayout; const AView: TView; const AArea: TCustomVCLArea; const AInteractor: TObject; const AParams: string);
begin
  inherited;
  FToolBar := TToolBar.Create(AArea.Component);
  FToolBar.ShowCaptions := True;
end;

destructor TToolBarArea.Destroy;
begin
  FreeAndNil(FToolBar);
  inherited;
end;

procedure TToolBarArea.DoAfterCreate(const AInteractor: TObject);
var
  vToolButton: TToolButton;
  i, vImageSize: Integer;
begin
  vImageSize := StrToIntDef(GetUrlParam(FParams, 'ImageSize'), 16);
  FToolBar.Images := TDragImageList(TInteractor(AInteractor).Images[vImageSize]);
  FToolBar.AutoSize := True;
  for i := 0 to FToolBar.ButtonCount - 1 do
  begin
    vToolButton := FToolBar.Buttons[i];
    if Assigned(vToolButton.DropdownMenu) then
      vToolButton.DropdownMenu.Images := FToolBar.Images;
  end
end;

procedure TToolBarArea.DoAssignItemOnClick(const AHandler: TNotifyEvent);
begin
  if FCurrentLevel = 0 then
  begin
    FToolButton.OnClick := AHandler;
  end
  else
  begin
    FMenuItem.OnClick := AHandler;
  end;
end;

procedure TToolBarArea.DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer);
begin
  if FCurrentLevel = 0 then
  begin
    FToolButton.Caption := ACaption;
    FToolButton.Hint := AHint;
    FToolButton.ImageIndex := AImageIndex;
  end
  else
  begin
    FMenuItem.Caption := ACaption;
    FMenuItem.Hint := AHint;
    FMenuItem.ImageIndex := AImageIndex;
  end;
end;

function TToolBarArea.DoCreateItem(const AParentObj: TObject; const AParams: string): TObject;
var
  vMenu: TPopupMenu;
  vLeft: Integer;
  vToolButton: TToolButton;
begin
  Result := nil;

  if FCurrentLevel = 0 then
  begin
    if FToolBar.ButtonCount > 0 then
    begin
      vToolButton := FToolBar.Buttons[FToolBar.ButtonCount - 1];
      vLeft := vToolButton.Left + vToolButton.Width + 10;
    end
    else
      vLeft := 0;

    FToolButton := TToolButton.Create(FToolBar);
    FToolButton.AutoSize := True;
    FToolButton.Left := vLeft;

    Result := FToolButton;
  end
  else
  begin
    if Assigned(AParentObj) then
    begin
      FMenuItem := TMenuItem.Create(FToolBar);

      if AParentObj is TToolButton then
      begin
        TToolButton(AParentObj).Style := tbsDropDown;
        if Assigned(TToolButton(AParentObj).DropdownMenu) then
          vMenu := TToolButton(AParentObj).DropdownMenu
        else
        begin
          vMenu := TPopupMenu.Create(FToolBar);
          TToolButton(AParentObj).DropdownMenu := vMenu;

        end;

        vMenu.Items.Add(FMenuItem);
      end
      else if AParentObj is TMenuItem then
      begin
        TMenuItem(AParentObj).Add(FMenuItem);
      end;

      Result := FMenuItem;
    end;
  end;
end;

function TToolBarArea.GetControl: TObject;
begin
  Result := FToolBar;
end;

{ TTreeViewArea }

constructor TTreeViewArea.Create(const ASource: TBaseLayout; const AView: TView; const AArea: TCustomVCLArea; const AInteractor: TObject; const AParams: string);
begin
  inherited;
  Assert(False, 'Not implemented');
  FTreeView := TTreeView.Create(nil);
  FTreeView.Images := TDragImageList(TInteractor(AInteractor).Images[16]);
  FTreeView.OnDblClick := OnDblClick;
end;

destructor TTreeViewArea.Destroy;
begin
  FreeAndNil(FTreeView);
  inherited;
end;

type
  TMyObj = class
    AHandler: TNotifyEvent;
  end;

procedure TTreeViewArea.DoAssignItemOnClick(const AHandler: TNotifyEvent);
begin
  FItem.Data := PMethod(@AHandler);
end;

procedure TTreeViewArea.DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer);
begin
  FItem.Text := ACaption;
  FItem.ImageIndex := AImageIndex;
end;

function TTreeViewArea.DoCreateItem(const AParentObj: TObject; const AParams: string): TObject;
begin
  if Assigned(AParentObj) and (AParentObj is TTreeNode) then
    FItem := FTreeView.Items.AddChild(TTreeNode(AParentObj), 'test')
  else
    FItem := FTreeView.Items.AddChild(nil, 'test2');

  Result := FItem;
end;

function TTreeViewArea.GetControl: TObject;
begin
  Result := FTreeView;
end;

procedure TTreeViewArea.OnDblClick(Sender: TObject);
var
  vNode: TTreeNode;
  vPoint: TPoint;
  vPMethod: PMethod;
  vMethod: TMethod;
begin
  vPoint := FTreeView.ScreenToClient(Mouse.CursorPos);
  vNode := FTreeView.GetNodeAt(vPoint.X, vPoint.Y);
  if Assigned(vNode) then
  begin
    if Assigned(vNode.Data) then
    begin
      vPMethod := PMethod(@vNode.Data);
      vMethod := vPMethod^;
      TNotifyEvent(vMethod)(vNode);
    end;
  end;
end;

{ TFieldSceneArea }

procedure TFieldSceneArea.DoActivate(const AAreaState: string);
begin
  inherited;
  FScene.Activate;
end;

procedure TFieldSceneArea.DoBeforeFreeControl;
begin
  FScene.Free;
end;

procedure TFieldSceneArea.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
var
  vDomain: TDomain;
  vSceneClass: TSceneClass;
  vModuleName: string;
begin
  vDomain := TDomain(FView.Domain);
  vSceneClass := TSceneClass(_Platform.ResolveModuleClass(vDomain.Settings, 'ChartPainter', 'Painting', vModuleName));
  FScene := vSceneClass.Create(TCustomVCLArea(AParent).Control);
  FControl := TWinScene(FScene).Panel;
end;

procedure TFieldSceneArea.DoDisableContent;
begin
  FScene.Enabled := False;
end;

procedure TFieldSceneArea.FillEditor;
begin
  inherited;
  // FView
  // FFieldDef
  // FControl
end;

procedure TFieldSceneArea.RefillArea(const AKind: Word);
begin
  if AKind <> dckNameChanged then
    FScene.Repaint;
end;

{ TFieldChartArea }

procedure TFieldChartArea.DoCreateControl(const AParent: TUIArea; const ALayout: TBaseLayout);
begin
  inherited DoCreateControl(AParent, ALayout);
  FId := 'Chart';
  FChart := TDataChart.Create(FScene, nil);
end;

initialization

RegisterClasses([TLabel, TPanel, TSplitter, TImage, TBevel, TPageControl, TMemo,
  TTabSheet, TToolBar, TToolButton, TBitBtn, TScrollBox, TMainMenu, TPopupMenu,
  TShape, TListView, TImageList]);

end.
