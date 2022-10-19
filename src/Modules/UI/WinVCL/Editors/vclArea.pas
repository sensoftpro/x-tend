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

unit vclArea;

interface

uses
  Windows, Classes, Forms, Messages, Generics.Collections, Controls, StdCtrls, ExtCtrls, Menus, UITypes, SysUtils,
  uConsts, uUIBuilder, uDefinition, uEntity, uView;

type
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

  TLabelPosition = (lpTop, lpLeft);

  TVCLFieldArea = class;
  TVCLFieldAreaClass = class of TVCLFieldArea;

  TVCLArea = class(TUIArea)
  private
    FOriginLeft, FOriginTop: Integer;  // for correct order during alignment
    FLabelPosition: TLabelPosition;
    FCaption: TLabel;
    FIsForm: Boolean;
    FIsAutoReleased: Boolean;
    FOnClose: TProc;
    function CreateButtonDesc(const AText: string): TButtonDesc;
    // Выполнение действий (Actions и переходы)
    procedure ExplicitNavigate(Sender: TObject);
    procedure OnPCCanClose(Sender: TObject; var ACanClose: Boolean);
    procedure OnActionMenuSelected(Sender: TObject);
    function GetNavigableCollections: TList<TDefinition>;
    function CreateChildToolButton(const AParent: TUIArea; const AViewPath: string): TUIArea;
    function CreateChildButton(const AParent: TUIArea; const ASourceBtn: TWinControl): TUIArea;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
    function GetControl: TControl;
    function GetComponent: TComponent;
    function CreateNavigationArea(const ASourcePanel: TPanel; const AView: TView; const AParams: string): TUIArea;
    function LessThanUIState(const ADefinition: TDefinition; const ASession: TObject; const AState: TViewState): Boolean;
  protected
    FPopupMenu: TPopupMenu;
    FNeedCreateCaption: Boolean;
    FShowCaption: Boolean;
    procedure PlaceLabel;
    procedure DoClose(const AModalResult: Integer); override;
    function DoCreateChildArea(const ALayout: TObject; const AView: TView; const AParams: string = ''; const AOnClose: TProc = nil): TUIArea; override;
    function DoCreateChildAction(const ALayout: TObject; const AView: TView; const AParams: string = ''): TUIArea; override;
    function AreaFromSender(const ASender: TObject): TUIArea; override;
    procedure AppendServiceArea(const ALayoutName: string); override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure DoActivate(const AUrlParams: string); override;
    procedure DoAfterChildAreasCreated; override;
  protected
    procedure PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer); override;

    function GetName: string; override;
    procedure SetParent(const Value: TUIArea); override;
    procedure SetControl(const AControl: TObject); override;
    procedure UnbindContent; override;
    procedure AssignFromLayout(const ALayout: TObject); override;
    procedure ArrangeChildAreas; override;
    procedure SaveLayoutToFile(const AFileName: string); override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure SetViewState(const AValue: TViewState); override;
    procedure CreateCaption(const AFieldDef: TFieldDef); override;
    function DoGetDescription: string; override;
    procedure RefillArea(const AKind: Word); override;

    procedure SetCaptionProperty(const ALayout: TObject); virtual;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TObject = nil; const AParams: string = ''); override;
    destructor Destroy; override;

    property Component: TComponent read GetComponent;
    property Control: TControl read GetControl;
    property OnClose: TProc read FOnClose write FOnClose;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition;
  end;

  TVCLFieldArea = class(TVCLArea)
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
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TObject = nil; const AParams: string = ''); override;
    destructor Destroy; override;

    procedure Deinit;

    property Focused: Boolean read GetFocused write SetFocused;
    property LayoutPositionCount: Integer read GetLayoutPositionCount;
  end;

type
  TCanChangeFieldFunc = function(const AView: TView; const AEntity: TEntity; const AFieldName: string; const ANewValue: Variant): Boolean of object;

implementation

uses
  Types, Graphics, Math, StrUtils, ComCtrls, Buttons,
  Generics.Defaults, Variants, cxGraphics, dxGDIPlusClasses, cxLabel, cxImage, cxEdit, cxTextEdit, cxPC, dxBar, dxNavBar, dxNavBarGroupItems,
  dxNavBarCollns, dxNavBarBase, dxNavBarExplorerViews,
  cxLookAndFeels, cxButtons, cxScrollBox, cxControls, cxSplitter,

  uDomain, uPresenter, uConfiguration, uSession, uInteractor, uUtils, uCollection,
  vclSimpleEditors, uEntityList, uDomainUtils;

type
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

    procedure Build(const AVCLArea: TVCLArea);
    procedure SaveToDFM(const AFileName: string);
  end;

  TNavigationArea = class abstract
  private
    FView: TView;
  protected
    FCurrentLevel: Integer;
    FParams: string;
    function GetControl: TObject; virtual; abstract;
    function DoCreateItem(const AParentObj: TObject): TObject; virtual; abstract;
    procedure DoAssignItemOnClick(const AHandler: TNotifyEvent); virtual; abstract;
    procedure DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer); virtual; abstract;
    procedure DoAfterCreate(const AInteractor: TObject); virtual;
  public
    constructor Create(const ASource: TPanel; const AView: TView; const AArea: TVCLArea; const AInteractor: TObject; const AParams: string); virtual;
    destructor Destroy; override;

    function CreateItem(const AParentObj: TObject; const ALevel: Integer): TObject;
    procedure AssignItemOnClick(const AHandler: TNotifyEvent);
    procedure AssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer);
    procedure AfterCreate(const AInteractor: TObject);

    property Control: TObject read GetControl;
    property View: TView read FView;
  end;

  TMainMenuArea = class(TNavigationArea)
  private
    FMenu: TMainMenu;
    FMenuItem: TMenuItem;
  protected
    function GetControl: TObject; override;
    function DoCreateItem(const AParentObj: TObject): TObject; override;
    procedure DoAssignItemOnClick(const AHandler: TNotifyEvent); override;
    procedure DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer); override;
  public
    constructor Create(const ASource: TPanel; const AView: TView; const AArea: TVCLArea; const AInteractor: TObject; const AParams: string); override;
    destructor Destroy; override;
  end;

  TToolBarArea = class(TNavigationArea)
  private
    FToolBar: TToolBar;
    FToolButton: TToolButton;
    FMenuItem: TMenuItem;
  protected
    function GetControl: TObject; override;
    function DoCreateItem(const AParentObj: TObject): TObject; override;
    procedure DoAssignItemOnClick(const AHandler: TNotifyEvent); override;
    procedure DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer); override;
    procedure DoAfterCreate(const AInteractor: TObject); override;
  public
    constructor Create(const ASource: TPanel; const AView: TView; const AArea: TVCLArea; const AInteractor: TObject; const AParams: string); override;
    destructor Destroy; override;
  end;

  TTreeViewArea = class(TNavigationArea)
  private
    FTreeView: TTreeView;
    FItem: TTreeNode;
    procedure OnDblClick(Sender: TObject);
  protected
    function GetControl: TObject; override;
    function DoCreateItem(const AParentObj: TObject): TObject; override;
    procedure DoAssignItemOnClick(const AHandler: TNotifyEvent); override;
    procedure DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer); override;
  public
    constructor Create(const ASource: TPanel; const AView: TView; const AArea: TVCLArea; const AInteractor: TObject; const AParams: string); override;
    destructor Destroy; override;
  end;

  TNavBarArea = class(TNavigationArea)
  private
    FNavBar: TdxNavBar;
    FNavBarGroup: TdxNavBarGroup;
    FNavBarItem: TdxNavBarItem;
  protected
    function GetControl: TObject; override;
    function DoCreateItem(const AParentObj: TObject): TObject; override;
    procedure DoAssignItemOnClick(const AHandler: TNotifyEvent); override;
    procedure DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer); override;
  public
    constructor Create(const ASource: TPanel; const AView: TView; const AArea: TVCLArea; const AInteractor: TObject; const AParams: string); override;
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
    function DoCreateItem(const AParentObj: TObject): TObject; override;
    procedure DoAssignItemOnClick(const AHandler: TNotifyEvent); override;
    procedure DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer); override;
  public
    constructor Create(const ASource: TPanel; const AView: TView; const AArea: TVCLArea; const AInteractor: TObject; const AParams: string); override;
    destructor Destroy; override;
  end;

const
  cServiceAreaHeight = 44;
  cPCNavigatorFlag = 1;

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

{ TVCLArea }

procedure TVCLArea.AppendServiceArea(const ALayoutName: string);
var
  vPanel: TPanel;
  vArea: TUIArea;
begin
  vPanel := TPanel.Create(nil);
  vPanel.BevelOuter := bvNone;
  vPanel.Height := cServiceAreaHeight;
  vPanel.Align := alBottom;
  vArea := TVCLArea.Create(Self, FView, '', True, vPanel);
  AddArea(vArea);
  FUIBuilder.ApplyLayout(vArea, FView, ALayoutName, '');
end;

function TVCLArea.AreaFromSender(const ASender: TObject): TUIArea;
begin
  if Assigned(ASender) and (ASender is TComponent) then
    Result := TUIArea(TComponent(ASender).Tag)
  else
    Result := nil;
end;

procedure TVCLArea.ArrangeChildAreas;
const
  cColumnWidth = 250;
  cBetweenRows = 27;
  cBetweenColumns = 25;
  cSideRate = 3/2;
  cBorder = 12;
var
  vTotalHeight: Integer;
  vEditor: TVCLArea;
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
  vOneRowHeight := Abs(TCrackedControl(FControl).Font.Height) + 8; //editor + label + borders

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
    vEditor := TVCLArea(Areas[i]);
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

procedure TVCLArea.AssignFromLayout(const ALayout: TObject);
var
  vPage: TTabSheet absolute ALayout;
  vFrame: TFrame absolute ALayout;
  vPanel: TCrackedControl absolute ALayout;
  vForm: TForm;
  vAlignment: TAlignment;
begin
  if FIsForm then
  begin
    vForm := TForm(FControl);

    if ALayout is TFrame then
    begin
      if (vFrame.Tag and cEditFormResizable) > 0 then
      begin
        vForm.BorderStyle := bsSizeable;
        vForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      end;
      if (vFrame.Tag and cMainFormPositionDesign) > 0 then
        vForm.Position := poDesigned;

      if (vForm.WindowState = wsNormal) then
      begin
        if vForm.FormStyle <> fsMDIChild then
        begin
          vForm.ClientWidth := vFrame.ClientWidth;
          if FView.DefinitionKind = dkDomain then
            vForm.ClientHeight := vFrame.ClientHeight
          else
            vForm.ClientHeight := vFrame.ClientHeight + cServiceAreaHeight;
        end;
      end;

      vForm.Constraints.MinHeight := vFrame.Constraints.MinHeight;
      vForm.Constraints.MinWidth := vFrame.Constraints.MinWidth;

      if vFrame.Hint <> '' then
        vForm.Caption := vFrame.Hint;
      vForm.Color := vFrame.Color;
    end
    else begin
      vForm.BorderStyle := bsSizeable;
      vForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      vForm.Caption := vPage.Caption;
    end;
  end
  else if ALayout is TFrame then
  begin
    //vFrame.SetBounds(Control.Left, Control.Top, Control.Width, Control.Height);
    if (vFrame.Hint <> '') and (FControl is TcxTabSheet) then
    begin
      TcxTabSheet(FControl).Caption := vFrame.Hint;
      if Pos('=', vFrame.Hint) > 0 then // Hint содержит url-строку с параметрами
      begin
        TcxTabSheet(FControl).Caption := GetUrlParam(vFrame.Hint, 'Caption', '');
        TcxTabSheet(FControl).ImageIndex := GetImageId(StrToIntDef(GetUrlParam(vFrame.Hint, 'ImageIndex', ''), -1));
      end;
    end;
  end
  else if ((ALayout is TPanel) or (ALayout is TMemo)) and (FControl is TControl) then
  begin
    PlaceIntoBounds(vPanel.Left, vPanel.Top, vPanel.Width, vPanel.Height);
    SetCaptionProperty(vPanel);

    if FControl is TcxCustomEdit then
      TcxCustomEdit(FControl).Style.Font.Assign(vPanel.Font)
    else
      TCrackedControl(FControl).Font.Assign(vPanel.Font);

    Control.Anchors := vPanel.Anchors;
    Control.Align := vPanel.Align;
    Control.AlignWithMargins := vPanel.AlignWithMargins;
    Control.Margins := vPanel.Margins;
    if Control is TWinControl then
      TWinControl(Control).Padding := vPanel.Padding;

    vAlignment := taLeftJustify;

    if ALayout is TPanel then
      vAlignment := TPanel(ALayout).Alignment
    else if ALayout is TMemo then
      vAlignment := TMemo(ALayout).Alignment;

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
        TStaticText(FControl).Height := vPanel.Height;
        TStaticText(FControl).Width := vPanel.Width;
      end
      else if FControl.InheritsFrom(TCheckBox) then
        TCheckBox(FControl).Alignment := taRightJustify
      else if FControl.InheritsFrom(TRadioButton) then
        TRadioButton(FControl).Alignment := taRightJustify
      else if FControl.InheritsFrom(TcxLabel) then
        TcxLabel(FControl).Properties.Alignment.Horz := taRightJustify
      else if FControl.InheritsFrom(TcxTextEdit) then
        TcxTextEdit(FControl).Properties.Alignment.Horz := taRightJustify
      else if Self is TFilenameFieldEditor then
        TFilenameFieldEditor(Self).TextEdit.Properties.Alignment.Horz := taRightJustify
    end;

    if not vPanel.ParentBackground then
    begin
      TCrackedControl(FControl).Color := vPanel.Color;
      TCrackedControl(FControl).ParentColor := False;
      TCrackedControl(FControl).ParentBackground := False;
    end;

  end;
end;

procedure TVCLArea.BeginUpdate;
begin
  if FControl is TWinControl then
    TWinControl(FControl).DisableAlign;

  if TInteractor(Interactor).Layout = 'mdi' then
    SendMessage(Application.MainForm.ClientHandle, WM_SETREDRAW, 0, 0);
end;

constructor TVCLArea.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
  const AControl: TObject = nil; const ALayout: TObject = nil; const AParams: string = '');
begin
  FNeedCreateCaption := True;

  if not Assigned(AControl) then
    FNeedCreateCaption := True;

  inherited Create(AParent, AView, AId, AIsService, AControl, ALayout, AParams);
end;

function TVCLArea.CreateButtonDesc(const AText: string): TButtonDesc;
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

procedure TVCLArea.CreateCaption(const AFieldDef: TFieldDef);
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

function TVCLArea.CreateChildButton(const AParent: TUIArea; const ASourceBtn: TWinControl): TUIArea;
var
  vSourceBtn: TBitBtn absolute ASourceBtn;
  vButton: TcxButton;
  vButtonDesc: TButtonDesc;
  vView: TView;
begin
  vButton := TcxButton.Create(nil);
  vButton.Align := vSourceBtn.Align;
  vButton.AlignWithMargins := vSourceBtn.AlignWithMargins;
  vButton.Margins := vSourceBtn.Margins;
  vButton.Padding := vSourceBtn.Padding;
  vButton.Width := vSourceBtn.Width;
  vButton.Height := vSourceBtn.Height;
  vButton.Left := vSourceBtn.Left;
  vButton.Top := vSourceBtn.Top;
  vButton.Glyph.Assign(vSourceBtn.Glyph);
  vButton.Anchors := vSourceBtn.Anchors;
  vButton.Font.Assign(vSourceBtn.Font);
  vButton.Visible := vSourceBtn.Visible;

  vButton.OnClick := ExplicitNavigate;

  vButtonDesc := CreateButtonDesc(vSourceBtn.Caption);
  try
    vButton.Caption := vButtonDesc.Caption;
    vButton.Hint := vButtonDesc.Hint;

    if vButtonDesc.View <> '' then
    begin
      vView := FUIBuilder.RootView.BuildView(vButtonDesc.View);
      Result := TVCLArea.Create(Self, vView, vSourceBtn.Caption, False, vButton);
      Result.UId := vButtonDesc.Id;
      TVCLArea(Result).UpdateArea(dckViewStateChanged);
    end
    else
      Result := TVCLArea.Create(Self, FView, vSourceBtn.Caption, False, vButton);
  finally
    vButtonDesc.Free;
  end;
end;

function TVCLArea.CreateChildToolButton(const AParent: TUIArea; const AViewPath: string): TUIArea;
var
  vLeft: Integer;
  vToolBar: TToolBar;
  vToolButton: TToolButton;
  vButtonDesc: TButtonDesc;
  vView: TView;
begin
  vToolBar := TToolBar(TVCLArea(AParent).Control);
  if vToolBar.ButtonCount > 0 then
  begin
    vToolButton := vToolBar.Buttons[vToolBar.ButtonCount - 1];
    vLeft := vToolButton.Left + vToolButton.Width + 10;
  end
  else
    vLeft := 0;

  vToolButton := TToolButton.Create(vToolBar);
  vToolButton.AutoSize := True;
  vToolButton.Left := vLeft;

  vToolButton.OnClick := ExplicitNavigate;

  vButtonDesc := CreateButtonDesc(AViewPath);
  try
    vToolButton.Caption := vButtonDesc.Caption;
    vToolButton.Hint := vButtonDesc.Hint;

    if vButtonDesc.View <> '' then
    begin
      vView := FUIBuilder.RootView.BuildView(vButtonDesc.View);
      if vView.DefinitionKind in [dkCollection, dkAction] then
      begin
        if vToolButton.Caption = '' then
          vToolButton.Caption := GetTranslation(TDefinition(vView.Definition));
        vToolButton.Hint := vToolButton.Caption;
        if vButtonDesc.ImageID > 0 then
          vToolButton.ImageIndex := GetImageID(vButtonDesc.ImageID)
        else
          vToolButton.ImageIndex := GetImageID(TDefinition(vView.Definition)._ImageID);
      end
      else
        Assert(False, 'Ситуация пока не обрабатывается');

      Result := TVCLArea.Create(Self, vView, AViewPath, False, vToolButton);
      Result.UId := vButtonDesc.Id;
      TVCLArea(Result).UpdateArea(dckViewStateChanged);
    end
    else
      Result := TVCLArea.Create(Self, FView, AViewPath, False, vToolButton);
  finally
    vButtonDesc.Free;
  end;
end;

function TVCLArea.CreateNavigationArea(const ASourcePanel: TPanel; const AView: TView; const AParams: string): TUIArea;
var
  vNavArea: TNavigationArea;
  vViewType: string;
  procedure AddDefinitionToMenu(const AMenu: TMenuItem; const ADefinition: TDefinition);
  var
    vDestItem: TMenuItem;
  begin
    vDestItem := TMenuItem.Create(nil);
    AMenu.Add(vDestItem);
    vDestItem.Caption := ADefinition.Name;// + '?WorkArea=WorkArea&Layout=Collection&ImageID=' + IntToStr(ADefinition._ImageID);
  end;

  procedure ProcessChilds(const AMenuItem: TMenuItem; const AParentArea: TUIArea; const AParentView: TView; const AParentObj: TObject; const ALevel: Integer);
  var
    i: Integer;
    vMI: TMenuItem;
    vView: TView;
    vDefinition: TDefinition;
    vGroupArea: TVCLArea;
    vImageID, vUrl, vLayoutName, vCaption, vHint: string;
    vImageIndex: Integer;
    vControl: TObject;
    vDefinitions: TList<TDefinition>;
  begin
    for i := 0 to AMenuItem.Count - 1 do
    begin
      vMI := AMenuItem[i];

      vControl := vNavArea.CreateItem(AParentObj, ALevel);

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
            Assert(vMI.Count = 0, 'Для действия ' + vMI.Caption + ' заданы дочерние элементы. Ошибка конфигурирования.');
            vNavArea.AssignItemOnClick(OnExecuteAction);
          end
          else if vView.DefinitionKind = dkCollection then
          begin
            if vMI.Count = 0 then
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

  //        TVCLArea(vGroupArea).UpdateArea(dckViewStateChanged); это нужно?

        end
        else
          Assert(False, 'DefinitionKind must be dkAction or dkCollection for ' + vMI.Caption);
      end;

      vGroupArea := TVCLArea.Create(AParentArea, vView, vUrl, False, vControl);

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
  Assert(Assigned(ASourcePanel.PopupMenu), 'У панели с именем ' + ASourcePanel.Name + ' задан Caption ' + ASourcePanel.Caption + '. Это навигационная область. Для неё нужно указать PopupMenu.');

  vViewType := GetUrlParam(AParams, 'ViewType');

  if vViewType = 'NavBar' then
    vNavArea := TNavBarArea.Create(ASourcePanel, AView, Self, Interactor, AParams)
  else if vViewType = 'MainMenu' then
    vNavArea := TMainMenuArea.Create(ASourcePanel, AView, Self, Interactor, AParams)
  else if vViewType = 'ToolBar' then
    vNavArea := TToolBarArea.Create(ASourcePanel, AView, Self, Interactor, AParams)
  else if vViewType = 'TreeView' then
    vNavArea := TTreeViewArea.Create(ASourcePanel, AView, Self, Interactor, AParams)
  else
    vNavArea := TOneButtonArea.Create(ASourcePanel, AView, Self, Interactor, AParams);

  Result := TVCLArea.Create(Self, vNavArea.View, Trim(ASourcePanel.Caption), False, vNavArea.Control);

  ProcessChilds(ASourcePanel.PopupMenu.Items, Result, AView, nil, 0);

  vNavArea.AfterCreate(Interactor);
end;

procedure TVCLArea.DoActivate(const AUrlParams: string);
var
  vChangeTab: Boolean;
begin
  vChangeTab := SameText(GetUrlParam(AUrlParams, 'TabActivationOption', ''), 'ChangeTab');

  if (FControl is TcxTabSheet) then
  begin
    if Assigned(Parent.CreateParams) and (Parent.CreateParams.IndexOfName('TabActivationOption') >= 0) and
      (not SameText(Parent.CreateParams.Values['TabActivationOption'], 'ChangeTab')) then
      vChangeTab := False;
    if vChangeTab then
      TcxPageControl(Control.Parent).Properties.ActivePage := TcxTabSheet(FControl);
  end
  else if (FControl is TForm) and (TForm(FControl).FormStyle = fsMDIChild) then
  begin
    if SameText(GetUrlParam(AUrlParams, 'State'), 'Max') then
      TForm(FControl).WindowState := wsMaximized
    else
    begin
      TForm(FControl).BringToFront;
    end;
  end;
end;

procedure TVCLArea.DoAfterChildAreasCreated;
var
  i: Integer;
  function FindControlForSplitter(const ASplitter: TcxSplitter): TControl;
  var
    c: Integer;
  begin
    Result := nil;
    for c := 0 to Count - 1 do
      if (Areas[c].Control <> ASplitter) and (Areas[c].Control is TControl) and (TControl(Areas[c].Control).Align = ASplitter.Align) then
      begin
        Result := TControl(Areas[c].Control);
        Break;
      end;
  end;
begin
  // прицепляем сплиттер к своему контролу, чтобы не отлипал в некоторых случаях, обрабатываем только простой случай: сплиттер с таким размещением один в текущей области
  for i := 0 to Count - 1 do
    if Areas[i].Control is TcxSplitter then
      TcxSplitter(Areas[i].Control).Control := FindControlForSplitter(TcxSplitter(Areas[i].Control));
end;

procedure TVCLArea.DoClose(const AModalResult: Integer);
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

function TVCLArea.DoCreateChildAction(const ALayout: TObject; const AView: TView; const AParams: string = ''): TUIArea;
var
  vButton: TcxButton;
  vLabel: TcxLabel;
  vActionDef: TDefinition;
  vDefinitions: TList<TDefinition>;
  i: Integer;
  vMenuItem: TMenuItem;
  vDefinition: TDefinition;
  vOnClickHandler: TNotifyEvent;
  vImageID: Integer;
  vParams: TStrings;
  vImageSize: Integer;
  vComposition: string;
  vViewStyle: string;
  vOverriddenCaption, vOverriddenHint: string;
begin
  if AView.DefinitionKind = dkCollection then
    vOnClickHandler := OnOpenCollection
  else
    vOnClickHandler := OnExecuteAction;

  vActionDef := TDefinition(AView.Definition);

  vParams := CreateDelimitedList(AParams, '&');
  try
    vImageSize := StrToIntDef(vParams.Values['ImageSize'], 16);
    vImageID := StrToIntDef(vParams.Values['ImageID'], vActionDef._ImageID);
    vComposition := Trim(vParams.Values['Composition']);
    vViewStyle := Trim(vParams.Values['ViewStyle']);
    vOverriddenCaption := Trim(vParams.Values['Caption']);
    vOverriddenHint := Trim(vParams.Values['Hint']);
  finally
    FreeAndNil(vParams);
  end;

  if vViewStyle = 'link' then
  begin
    vLabel := TcxLabel.Create(nil);
    vLabel.Caption := GetTranslation(vActionDef);
    vLabel.Hint := vLabel.Caption;
    if Length(vOverriddenCaption) > 0 then
      vLabel.Caption := vOverriddenCaption;
    if Length(vOverriddenHint) > 0 then
      vLabel.Hint := vOverriddenHint;
    vLabel.Cursor := crHandPoint;
    vLabel.Transparent := True;
    vLabel.Properties.Alignment.Vert := TcxEditVertAlignment.taVCenter;
    vLabel.Style.TextColor := TPanel(ALayout).Font.Color;
    //vLabel.Style.TextStyle := [fsUnderline];
    vLabel.Style.HotTrack := True;
    //vLabel.StyleHot.TextColor := clBlue;
    vLabel.StyleHot.TextStyle := [fsUnderline];
    vLabel.OnClick := vOnClickHandler;

    Result := TVCLArea.Create(Self, AView, vActionDef.Name, False, vLabel);
    if AParams <> '' then
      Result.AddParams(CreateDelimitedList(AParams, '&'));

    Exit;
  end;

  vButton := TcxButton.Create(nil);

  vButton.OptionsImage.Images := TDragImageList(TInteractor(Interactor).Images[vImageSize]);

  vImageID := GetImageID(vImageID);

  if (TPanel(ALayout).BevelOuter = bvNone) and (TPanel(ALayout).BevelInner = bvNone)
    and (TPanel(ALayout).BevelKind = TBevelKind.bkNone)
  then begin
    vButton.SpeedButtonOptions.Flat := True;
    vButton.SpeedButtonOptions.CanBeFocused := False;
  end;

  vButton.Caption := GetTranslation(vActionDef);
  vButton.Hint := vButton.Caption;
  if Length(vOverriddenCaption) > 0 then
    vButton.Caption := vOverriddenCaption;
  if Length(vOverriddenHint) > 0 then
    vButton.Hint := vOverriddenHint;

  if (vButton.OptionsImage.Images.Count + 1 >= vImageID) and (vImageID > 0) then
  begin
    if vComposition = '' then
    begin
      if TPanel(ALayout).ShowHint then
        vButton.PaintStyle := bpsDefault
      else
        vButton.PaintStyle := bpsGlyph;
    end
    else if vComposition = 'TextOnly' then
    begin
      vButton.PaintStyle := bpsCaption;
    end
    else if vComposition = 'ImageOnly' then
      vButton.PaintStyle := bpsGlyph
    else
    begin
      vButton.PaintStyle := bpsDefault;
      if vComposition = 'ImageRight' then
        vButton.Layout := TButtonLayout.blGlyphRight
      else if vComposition = 'ImageTop' then
        vButton.Layout := TButtonLayout.blGlyphTop
      else if vComposition = 'ImageBottom' then
        vButton.Layout := TButtonLayout.blGlyphBottom
      else
        vButton.Layout := TButtonLayout.blGlyphLeft;
    end;
    vButton.OptionsImage.ImageIndex := vImageID;
  end
  else
  begin
    vButton.PaintStyle := bpsCaption;
    vButton.WordWrap := True;
  end;

  if (vActionDef.Name = 'Add') and Assigned(AView.ParentDomainObject) and (AView.ParentDomainObject is TEntityList) then
  begin
    vDefinitions := TEntityList(AView.ParentDomainObject).ContentDefinitions;
    if vDefinitions.Count > 1 then
    begin
      FPopupMenu := TPopupMenu.Create(nil);
      FPopupMenu.Images := TDragImageList(TInteractor(Interactor).Images[16]);
      for i := 0 to vDefinitions.Count - 1 do
      begin
        vDefinition := TDefinition(vDefinitions[i]);
        vMenuItem := TMenuItem.Create(nil);
        vMenuItem.Caption := GetTranslation(vDefinition);
        if Length(vOverriddenCaption) > 0 then
          vMenuItem.Caption := vOverriddenCaption;
        vMenuItem.ImageIndex := GetImageID(vDefinition._ImageID);
        vMenuItem.Tag := Integer(vButton);
        vMenuItem.OnClick := OnActionMenuSelected;
        FPopupMenu.Items.Add(vMenuItem);
      end;
      vButton.DropDownMenu := FPopupMenu;
      vButton.Kind := cxbkOfficeDropDown;
      vButton.OptionsImage.Margin := 4;
    end
    else begin
      vButton.OnClick := vOnClickHandler;
      TWinControl(ALayout).Width := TWinControl(ALayout).Height;
    end;
  end
  else
    vButton.OnClick := vOnClickHandler;

  Result := TVCLArea.Create(Self, AView, vActionDef.Name, False, vButton);
  if AParams <> '' then
    Result.AddParams(CreateDelimitedList(AParams, '&'));
end;

function TVCLArea.LessThanUIState(const ADefinition: TDefinition; const ASession: TObject; const AState: TViewState): Boolean;
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

function TVCLArea.DoCreateChildArea(const ALayout: TObject; const AView: TView; const AParams: string = ''; const AOnClose: TProc = nil): TUIArea;
var
  vSourceLabel: TLabel absolute ALayout;
  vSourceImage: TImage absolute ALayout;
  vSourcePC: TPageControl absolute ALayout;
  vSourceTabSheet: TTabSheet absolute ALayout;
  vSourceToolBar: TToolBar absolute ALayout;
  vSourceToolButton: TToolButton absolute ALayout;
  vSourceBtn: TBitBtn absolute ALayout;
  vSourcePanel: TPanel absolute ALayout;
  vSourceBox: TScrollBox absolute ALayout;
  vSourceBevel: TBevel absolute ALayout;
  vSourceSplitter: TSplitter absolute ALayout;
  vSourceShape: TShape absolute ALayout;
  vSourceListView: TListView absolute ALayout;
  vSourceImageList: TImageList absolute ALayout;
  vDomain: TDomain;
  vPos: Integer;
  vCaption: string;
  vUIParams: string;
  vStartPageName: string;
  vStartPageStr: string;
  vLabel: TcxLabel; //TStaticText;
  vImage: TcxImage;
  vPC: TcxPageControl;
  vToolBar: TToolBar;
  vTab: TcxTabSheet;
  vPanel: TPanel;
  vParams: TStrings;
  vBox: TcxScrollBox;
  vBevel: TBevel;
  vSplitter: TcxSplitter;
  vResolution: Integer;
  vNavItems: TList<TDefinition>;
  vNavDefinition: TDefinition;
  vChildArea: TUIArea;
  vMenu: TMenu;
  vForm: TForm;
  vShape: TShape;
  vListView: TListView;
  vImageList: TImageList;

  function AddDefinition(const AParent: TUIArea; const AMenu: TMenuItem; const ADefinition: TDefinition;
    const ALayoutName: string = ''): TUIArea;
  var
    vDestItem: TMenuItem;
    vLayoutName: string;
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

    Result := TVCLArea.Create(AParent, AView, vCaption, False, vDestItem);
    TVCLArea(Result).UpdateArea(dckViewStateChanged);
    AParent.AddArea(Result);
  end;

  procedure CopyMenuItems(const AParent: TUIArea; const ASrcMenu, ADestMenu: TMenuItem);
  var
    i: Integer;
    vCaption, vLayoutName: string;
    vParams: TStrings;
    vId: string;
    vImageID: string;
    vSrcItem: TMenuItem;
    vDestItem: TMenuItem;
    vView: TView;
    vChildArea: TUIArea;
    vAction: TActionDef;
    vDefinition: TDefinition;
    vDefinitions: TList<TDefinition>;
  begin
    for i := 0 to ASrcMenu.Count - 1 do
    begin
      vSrcItem := ASrcMenu[i];
      vCaption := vSrcItem.Caption;
      vDestItem := TMenuItem.Create(nil);
      vDestItem.ImageIndex := -1;
      ADestMenu.Add(vDestItem);

      if vCaption = '-' then
      begin
        vDestItem.Caption := vCaption;
        vDestItem.Enabled := False;
        vChildArea := TVCLArea.Create(AParent, AParent.View, vCaption, False, vDestItem);
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

        vChildArea := TVCLArea.Create(AParent, AParent.View, vSrcItem.Caption, False, vDestItem);
        if vId = 'Documents' then
        begin
          vDefinitions := TList<TDefinition>.Create;
          try
            TConfiguration(TInteractor(Interactor).Configuration).Definitions.DefinitionsByKind(vDefinitions, clkDocument);
            for vDefinition in vDefinitions do
              if not LessThanUIState(vDefinition, TInteractor(Interactor).Session, vsReadOnly)
                and not vDefinition.HasFlag(ccNotSave) and not vDefinition.HasFlag(ccHideInMenu)
              then
                AddDefinition(vChildArea, vDestItem, vDefinition);
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
                AddDefinition(vChildArea, vDestItem, vDefinition);
          finally
            FreeAndNil(vDefinitions);
          end;
        end
        else if vId = 'Windows' then
          vDestItem.Visible := TInteractor(Interactor).Layout = 'mdi';

        CopyMenuItems(vChildArea, vSrcItem, vDestItem);

        if (vId = 'Windows') and vDestItem.Visible and Assigned(TVCLArea(AParent.Parent).Control) then
        begin
          if TVCLArea(AParent.Parent).Control is TForm then
          begin
            vForm := TForm(TVCLArea(AParent.Parent).Control);
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

          vChildArea := TVCLArea.Create(AParent, vView, vCaption, False, vDestItem);
          TVCLArea(vChildArea).UpdateArea(dckViewStateChanged);
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

          vChildArea := TVCLArea.Create(AParent, vView, vCaption, False, vDestItem);
          TVCLArea(vChildArea).UpdateArea(dckViewStateChanged);
        end
        else begin
          vView.CleanView;
          vDestItem.Caption := vCaption;
          vDestItem.Enabled := False;
          vChildArea := TVCLArea.Create(AParent, AParent.View, vCaption, False, vDestItem);
        end;
      end;

      AParent.AddArea(vChildArea);
    end;
  end;

  procedure CopyPopupMenuItems(const AParent: TUIArea; const ASrcMenu, ADestMenu: TMenuItem);
  var
    vActions: TList<TActionDef>;
    vReports: TList<TRTFReport>;
    vDefinitions: TList<TDefinition>;
    vId: string;
    i, j: Integer;
    vDefinition: TDefinition;
    vSrcItem: TMenuItem;
    vDestItem: TMenuItem;
    vChildItem: TMenuItem;
    vView: TView;
    vAction: TActionDef;
    vReport: TRTFReport;
    vChildArea: TVCLArea;
  begin
    for i := 0 to ASrcMenu.Count - 1 do
    begin
      vSrcItem := ASrcMenu[i];
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
              vDestItem.RadioItem := vSrcItem.RadioItem;
              vDestItem.GroupIndex := vSrcItem.GroupIndex;

              if Assigned(vView.DomainObject) and TEntity(vView.DomainObject).FieldExists('IsChecked') then
                vDestItem.AutoCheck := True;

              vChildArea := TVCLArea.Create(AParent, vView, vCaption, False, vDestItem);
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

              vChildArea := TVCLArea.Create(AParent, vView, vCaption, False, vDestItem);
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

        vChildArea := TVCLArea.Create(AParent, AParent.View, vSrcItem.Caption, False, vDestItem);
        AParent.AddArea(vChildArea);

        ADestMenu.Add(vDestItem);
        CopyPopupMenuItems(vChildArea, vSrcItem, vDestItem);

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
              vChildItem.Tag := Integer(vDestItem);
              vChildItem.OnClick := OnActionMenuSelected;
              vDestItem.Add(vChildItem);
            end;
          end
          else
            vDestItem.OnClick := OnExecuteAction;
        end
        else
          vDestItem.OnClick := OnExecuteAction;
        
        vDestItem.RadioItem := vSrcItem.RadioItem;
        vDestItem.GroupIndex := vSrcItem.GroupIndex;
        if Assigned(vView.DomainObject) and TEntity(vView.DomainObject).FieldExists('IsChecked') then
          vDestItem.AutoCheck := True;

        vChildArea := TVCLArea.Create(AParent, vView, vCaption, False, vDestItem);
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

  procedure SetupBar(const ABar: TdxBar);
  begin
    ABar.DockingStyle := dsTop;
    ABar.DockedDockingStyle := dsTop;
    ABar.AllowClose := False;
    ABar.AllowCustomizing := False;
    ABar.AllowQuickCustomizing := False;
    ABar.AllowReset := False;
    ABar.UseRestSpace := True;
    ABar.NotDocking := [dsNone, dsTop, dsRight, dsBottom, dsLeft];
  end;

  function AddMenuButton(const AParent: TUIArea; const AViewName: string): TUIArea;
  var
    vBar: TdxBar;
    vNavBar: TdxBarManager;
    vButton: TdxBarButton;
    vView: TView;
    vAction: TActionDef;
    vDefinition: TDefinition;
  begin
    vBar := TdxBar(TVCLArea(AParent).Control);
    vNavBar := vBar.BarManager; 

    vButton := vNavBar.AddButton;
    vAction := TConfiguration(TInteractor(Interactor).Configuration).Actions.ObjectByName(AViewName);
    if Assigned(vAction) then
    begin
      vButton.Caption := GetTranslation(vAction);
      vButton.Hint := vButton.Caption;
      vButton.ImageIndex := GetImageID(vAction._ImageID);
      vButton.OnClick := OnExecuteAction;

      vView := FUIBuilder.RootView.BuildView(AViewName);
      Result := TVCLArea.Create(AParent, vView, AViewName, False, vButton);
      TVCLArea(Result).UpdateArea(dckViewStateChanged);
    end
    else begin
      vDefinition := TConfiguration(TInteractor(Interactor).Configuration).DefinitionByName[AViewName];
      if Assigned(vDefinition) then
      begin
        vButton.Caption := GetTranslation(vDefinition);
        vButton.Hint := vButton.Caption;
        vButton.ImageIndex := GetImageID(vDefinition._ImageID);
        vButton.OnClick := OnExecuteAction;

        vView := FUIBuilder.RootView.BuildView(AViewName);
        Result := TVCLArea.Create(AParent, vView, 
          'View=' + AViewName + '@WorkArea=WorkArea@Layout=Collection', False, vButton);
        TVCLArea(Result).UpdateArea(dckViewStateChanged);     
      end
      else begin
        vButton.Caption := AViewName;
        vButton.Hint := vButton.Caption;
        Result := TVCLArea.Create(AParent, AParent.View, AViewName, False, vButton);
      end;
    end;

    vBar.ItemLinks.Add(vButton);
    AParent.AddArea(Result);
  end;

  function AddMenuSeparator(const AParent: TUIArea): TUIArea;
  var
    vBar: TdxBar;
    vNavBar: TdxBarManager;
    vSeparator: TdxBarItem;
  begin
    vBar := TdxBar(TVCLArea(AParent).Control);
    vNavBar := vBar.BarManager; 
  
    vSeparator := vNavBar.AddItem(TdxBarSeparator);
    vBar.ItemLinks.Add(vSeparator);

    Result := TVCLArea.Create(AParent, AParent.View, '-separator-', False, vSeparator);
    AParent.AddArea(Result);
  end;

  function AddMenuGroup(const AParent: TUIArea; const ACaption: string): TUIArea;
  var
    vBar: TdxBar;
    vNavBar: TdxBarManager;
    vSubGroup: TdxBarSubItem;
  begin
    vBar := TdxBar(TVCLArea(AParent).Control);
    vNavBar := vBar.BarManager; 
    
    vSubGroup := vNavBar.AddSubItem;
    vSubGroup.Caption := ACaption;
    vBar.ItemLinks.Add(vSubGroup);

    Result := TVCLArea.Create(AParent, AParent.View, ACaption, False, vSubGroup);
    AParent.AddArea(Result);
  end;

  {procedure PopulateViewItem(const AParent: TUIArea);
  begin
    vMenuGroup := AddMenuGroup(nil, FInteractor.Translate('cptView', 'Вид'));
    AddMenuButton(vMenuGroup, BuildAction(FActionList, FInteractor.Translate('cptShowStatus', 'Показать строку статуса'), '', -1, nil));
    AddMenuButton(vMenuGroup, BuildAction(FActionList, FInteractor.Translate('cptWorkAreaAsTabs', 'Рабочая область в виде закладок'), '', -1, nil));
  end;

  procedure PopulateWindowsItem(const AParent: TUIArea);
  begin
    vMenuGroup := AddMenuGroup(nil, FInteractor.Translate('cptWindows', 'Окна'));
    if FWorkArea is TMDIWorkArea then
    begin
      AddMenuButton(vMenuGroup, BuildAction(FActionList, FInteractor.Translate('cptCascade', 'Каскадом'), '', 0, DoWindowAction, Integer(waCascade)));
      AddMenuButton(vMenuGroup, BuildAction(FActionList, FInteractor.Translate('cptVert', 'Вертикально'), '', 0, DoWindowAction, Integer(waTileVert)));
      AddMenuButton(vMenuGroup, BuildAction(FActionList, FInteractor.Translate('cptHorz', 'Горизонтально'), '', 0, DoWindowAction, Integer(waTileHorz)));
      AddMenuButton(vMenuGroup, BuildAction(FActionList, '-', '', -1, nil));
    end;
    AddMenuButton(vMenuGroup, BuildAction(FActionList, FInteractor.Translate('cptCloseAll', 'Закрыть все'), '', -1, DoWindowAction, -1));
  end;

  procedure PopulateDocsAndLibItems(const AParent: TUIArea);
  var
    i: Integer;
    vDefinition: TDefinition;
  begin
    vDefinitions := TList.Create;
    TDomain(Domain).Configuration.GetDefinitionsByKind(vDefinitions, clkDocument);
    vMenuGroup := AddMenuGroup(nil, FInteractor.Translate('cptDocuments', 'Документы'));
    for i := 0 to vDefinitions.Count - 1 do
    begin
      vDefinition := TDefinition(vDefinitions[i]);
      if not vDefinition.HasUIFlag(Session, ccHideInMenu) then
        AddMenuButton(vMenuGroup, BuildAction(FCfgActionList, vDefinition.GetCaption(FInteractor),
          '', vDefinition.ImageID - 1, DoOpenCollection, Integer(vDefinition)));
    end;

    vDefinitions.Clear;
    TDomain(Domain).Configuration.GetDefinitionsByKind(vDefinitions, clkLibrary);
    vDefinitions.Sort(CompareLibs);
    vMenuGroup := AddMenuGroup(nil, FInteractor.Translate('cptLibraries', 'Справочники'));
    for i := 0 to vDefinitions.Count - 1 do
    begin
      vDefinition := TDefinition(vDefinitions[i]);
      if not vDefinition.HasUIFlag(Session, ccHideInMenu) then
        AddMenuButton(vMenuGroup, BuildAction(FCfgActionList, vDefinition.GetCaption(FInteractor),
          '', vDefinition.ImageID - 1, DoOpenCollection, Integer(vDefinition)));
    end;

    vDefinitions.Free;
  end;}

  procedure CreateMainMenu(const AParent: TUIArea; const ANavBar: TdxBarManager);
  var
    vMainMenu: TdxBar;
    vBarArea: TVCLArea;
    vGroupArea: TUIArea;
  begin
    vMainMenu := ANavBar.AddToolBar(True);
    vMainMenu.Images := TDragImageList(TInteractor(Interactor).Images[16]);
    SetupBar(vMainMenu);
    vBarArea := TVCLArea.Create(AParent, AView, 'NavMainMenu', False, vMainMenu);
    AParent.AddArea(vBarArea);

    vGroupArea := AddMenuGroup(vBarArea, TInteractor(Interactor).Translate('cptSystem', 'Система'));
    if TUserSession(TInteractor(Interactor).Session).IsAdmin then
    begin
    {$IFNDEF DISABLE_LOGIN_FORM}
      AddMenuButton(vGroupArea, 'ChangePassword');
    {$ENDIF}    
      //AddMenuButton(vGroupArea, 'SysUsers');

      AddMenuButton(vGroupArea, 'GetAllUpdates');
      AddMenuButton(vGroupArea, 'ApplyAllUpdates');
      AddMenuButton(vGroupArea, 'ActualizeData');

      AddMenuButton(vGroupArea, 'ShowSysLog');
      AddMenuButton(vGroupArea, 'SetupRTFReports');
      AddMenuButton(vGroupArea, 'ShowSettings');
      AddMenuButton(vGroupArea, 'ShowOptions');

      if vDomain.Settings.GetValue('Core', 'StartPage', '') <> '' {and >> проверить, что такой лэйаут есть} then
        AddMenuButton(vGroupArea, 'ShowStartPage');
    end;
    AddMenuButton(vGroupArea, 'LoadChanges');
    AddMenuSeparator(vGroupArea);
    AddMenuButton(vGroupArea, 'Quit');

    //PopulateViewItem;

    //PopulateDocsAndLibItems;

    //PopulateWindowsItem;

    AddMenuButton(vBarArea, 'ShowAbout');    
  end;
  
  procedure CreateToolBar(const AParent: TUIArea; const ANavBar: TdxBarManager);
  var
    vCfgImagesResolution: Integer;
    vConfiguration: TConfiguration;
    vDefinition: TDefinition;
    vNavigationItems: TList<TDefinition>;
    vButton: TdxBarLargeButton;
    vView: TView;
    vToolBar: TdxBar;
    vStream: TStream;
    vBarArea: TVCLArea;
    vButtonArea: TVCLArea;
  begin
    vToolBar := ANavBar.AddToolBar;
    SetupBar(vToolBar);
    vBarArea := TVCLArea.Create(AParent, AView, 'NavToolBar', False, vToolBar);
    AParent.AddArea(vBarArea);   
  
    vCfgImagesResolution := StrToIntDef(vDomain.Settings.GetValue('Core', 'CfgImagesResolution'), 32);
    vConfiguration := vDomain.Configuration;

    vNavigationItems := GetNavigableCollections;
    try
      for vDefinition in vNavigationItems do
      begin
        vButton := TdxBarLargeButton(ANavBar.AddItem(TdxBarLargeButton));
        vButton.OnClick := ExplicitNavigate;
        vButton.Caption := GetTranslation(vDefinition);
        vButton.Hint := vButton.Caption;
        vButton.GlyphLayout := glTop;
        vButton.PaintStyle := psCaptionGlyph;
        vButton.AutoGrayScale := False;

        vView := FUIBuilder.RootView.BuildView(vDefinition.Name);
        vButtonArea := TVCLArea.Create(vBarArea, vView,
          'View=' + vDefinition.Name + '@WorkArea=WorkArea@Layout=Collection', False, vButton);
        vButtonArea.UpdateArea(dckViewStateChanged);
        vBarArea.AddArea(vButtonArea);

        vStream := vConfiguration.Icons.IconByIndex(vDefinition._ImageID, vCfgImagesResolution);
        if Assigned(vStream) then
        begin
          vStream.Position := 0;
          vButton.LargeGlyph.LoadFromStream(vStream);
        end;
        vToolBar.ItemLinks.Add(vButton);
      end;
    finally
      FreeAndNil(vNavigationItems);
    end;
  end;

  function CreateNavigation(const AParentArea: TUIArea): TUIArea;
  var
    vNavigationBar: TdxBarManager;
  begin
    vNavigationBar := TdxBarManager.Create(nil); //<= TForm?
    vNavigationBar.CanCustomize := False;
    Result := TVCLArea.Create(Self, AView, 'NavBar', False, vNavigationBar);
    
    vNavigationBar.BeginUpdate;
    try
      CreateMainMenu(Result, vNavigationBar);
      CreateToolBar(Result, vNavigationBar);
    finally
      vNavigationBar.EndUpdate;
    end;
  end;

begin
  Result := nil;

  if not Assigned(ALayout) then
    Exit;

  vDomain := TDomain(TInteractor(Interactor).Domain);

  if ALayout is TShape then
  begin
    vShape := TShape.Create(nil);
    vShape.Left := vSourceShape.Left;
    vShape.Top := vSourceShape.Top;
    vShape.Width := vSourceShape.Width;
    vShape.Height := vSourceShape.Height;
    vShape.Anchors := vSourceShape.Anchors;
    vShape.Align := vSourceShape.Align;
    vShape.Hint := vSourceShape.Hint;
    vShape.Visible := vSourceShape.Visible;
    vShape.Pen.Color := vSourceShape.Pen.Color;
    vShape.Brush.Color := vSourceShape.Brush.Color;

    Result := TVCLArea.Create(Self, AView, '', False, vShape);
  end 
  else if ALayout is TListView then
  begin
    vListView := TListView.Create(nil);
    vListView.Left := vSourceListView.Left;
    vListView.Top := vSourceListView.Top;
    vListView.Width := vSourceListView.Width;
    vListView.Height := vSourceListView.Height;
    vListView.Anchors := vSourceListView.Anchors;
    vListView.Align := vSourceListView.Align;
    vListView.Hint := vSourceListView.Hint;
    vListView.Visible := vSourceListView.Visible;
    vListView.Constraints := vSourceListView.Constraints;
    vListView.ViewStyle := vSourceListView.ViewStyle;
    vListView.Columns.Assign(vSourceListView.Columns);
    vListView.GridLines := vSourceListView.GridLines;
    vListView.Font.Size := vSourceListView.Font.Size;
    vListView.Font.Color := vSourceListView.Font.Color;
    vListView.Font.Style := vSourceListView.Font.Style;
//    vListView.Items.Assign(vSourceListView.Items);
    vListView.Name := vSourceListView.Name;

    Result := TVCLArea.Create(Self, AView, '', False, vListView);
  end 
  else if ALayout is TImageList then
  begin
    vImageList := TImageList.Create(nil);
    vImageList.Width := vSourceImageList.Width;
    vImageList.Height := vSourceImageList.Height;

    Result := TVCLArea.Create(Self, AView, '', False, vImageList);
  end
  else if ALayout is TLabel then
  begin
    vLabel := TcxLabel.Create(nil);   // заменил с TLabel на TcxLabel, потому что TLabel мерцал при растягивании формы
    vLabel.Left := vSourceLabel.Left;
    vLabel.Top := vSourceLabel.Top;
    vLabel.Width := vSourceLabel.Width;
    vLabel.Height := vSourceLabel.Height;
    vLabel.Transparent := vSourceLabel.Transparent;
    vLabel.AutoSize := vSourceLabel.AutoSize;
    vLabel.Properties.WordWrap := vSourceLabel.WordWrap;
    vLabel.Style.Font.Size := vSourceLabel.Font.Size;
    vLabel.Style.Font.Color := vSourceLabel.Font.Color;
    vLabel.Style.Font.Style := vSourceLabel.Font.Style;
    vLabel.Anchors := vSourceLabel.Anchors;

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
        vCaption := GetTranslation(TDefinition(AView.Definition))
      else
        // Обработать другие ситуации
    end
    else
      vCaption := vSourceLabel.Caption; //FView.Interactor.Translate('@' + {FFullAreaName или FLayoutName +} '.' +
        //vSourceLabel.Name + '@Caption', vSourceLabel.Caption);

    vLabel.Caption := vCaption;

    Result := TVCLArea.Create(Self, AView, '', False, vLabel);
  end
  else if ALayout is TImage then
  begin
    vImage := TcxImage.Create(nil);
    vImage.Style.BorderStyle := ebsNone;
    vImage.ControlStyle := vImage.ControlStyle + [csOpaque];
    vImage.Width := vSourceImage.Width;
    vImage.Height := vSourceImage.Height;
    vImage.Left := vSourceImage.Left;
    vImage.Top := vSourceImage.Top;
    vImage.Picture.Assign(vSourceImage.Picture);
    vImage.Anchors := vSourceImage.Anchors;
    vImage.Align := vSourceImage.Align;
    vImage.Transparent := True;
    vImage.Properties.ShowFocusRect := False;
    vImage.Properties.PopupMenuLayout.MenuItems := [];
    vImage.Properties.FitMode := ifmNormal;
    vImage.DoubleBuffered := True;
    vImage.Properties.Stretch := vSourceImage.Stretch;
    vImage.Properties.Proportional := vSourceImage.Proportional;
    vImage.Hint := vSourceImage.Hint;
    vImage.AlignWithMargins := vSourceImage.AlignWithMargins;
    vImage.Margins := vSourceImage.Margins;
    vImage.Properties.Center := False;

    Result := TVCLArea.Create(Self, AView, '', False, vImage);
  end
  else if ALayout is TPageControl then
  begin
    vPC := TcxPageControl.Create(nil);
    vPC.DoubleBuffered := True;
    vPC.Width := vSourcePC.Width;
    vPC.Height := vSourcePC.Height;
    vPC.Left := vSourcePC.Left;
    vPC.Top := vSourcePC.Top;
    vPC.Properties.TabPosition := TcxTabPosition(vSourcePC.TabPosition);
    vPC.Properties.TabHeight := vSourcePC.TabHeight;
    vPC.Properties.TabWidth := vSourcePC.TabWidth;
    vPC.Font.Assign(vSourcePC.Font);
    vPC.Align := vSourcePC.Align;
    vPC.AlignWithMargins := vSourcePC.AlignWithMargins;
    vPC.Margins := vSourcePC.Margins;
    vPC.Anchors := vSourcePC.Anchors;
    vPC.Visible := vSourcePC.Visible;
    if (vSourcePC.Tag and cPCNavigatorFlag) > 0 then
    begin
      vPC.Properties.Rotate := True;
      vPC.Properties.Images := TDragImageList(TInteractor(Interactor).Images[32]);
      vPC.Properties.TabCaptionAlignment := taLeftJustify;
    end
    else
    begin
      vPC.Properties.Options := vPC.Properties.Options + [pcoTopToBottomText];
      vPC.LookAndFeel.NativeStyle := False;
      vPC.LookAndFeel.Kind := lfUltraFlat;
    end;

    Result := TVCLArea.Create(Self, AView, '', False, vPC);
  end
  else if ALayout is TTabSheet then
  begin
    if (TInteractor(AView.Interactor).Layout = 'mdi') and (vSourceTabSheet.Tag = 11) then
    begin
      vForm := TForm.Create(nil);
      vForm.Caption := vSourceTabSheet.Caption;
      vForm.Position := poDefault;
      vForm.FormStyle := fsMDIChild;
      vForm.OnClose := OnCloseMDIForm;
      vForm.ShowHint := True;
      if AView.DefinitionKind in [dkCollection, dkAction, dkEntity] then
        TDragImageList(TInteractor(Interactor).Images[16]).GetIcon(GetImageID(TDefinition(AView.Definition)._ImageID), vForm.Icon);
      Result := TVCLArea.Create(Self, AView, vSourceTabSheet.Name, False, vForm);
      TVCLArea(Result).OnClose := AOnClose;
    end
    else begin
      vTab := TcxTabSheet.Create(Component);
      vTab.Caption := vSourceTabSheet.Caption;
      vTab.ImageIndex := GetImageID(vSourceTabSheet.ImageIndex);

      vStartPageName := TDomain(FView.Domain).Settings.GetValue('Core', 'StartPage', '');
      vTab.AllowCloseButton := not SameText(vSourceTabSheet.Name, vStartPageName);
      vTab.TabVisible := vSourceTabSheet.TabVisible;

      Result := TVCLArea.Create(Self, AView, vSourceTabSheet.Name, False, vTab);

//      if vSourceTabSheet.Visible then // это нужно? иначе лишние перерисовки контролов идут
//        TcxPageControl(FControl).ActivePage := vTab;
    end;
  end
  else if ALayout is TToolBar then
  begin
    vToolBar := TToolBar.Create(nil);
    vToolBar.Align := vSourceToolBar.Align;
    vToolBar.ShowCaptions := vSourceToolBar.ShowCaptions;
    vToolBar.DrawingStyle := vSourceToolBar.DrawingStyle;
    vToolBar.AutoSize := vSourceToolBar.AutoSize;
    vToolBar.Font.Size := vSourceToolBar.Font.Size;
    vToolBar.List := vSourceToolBar.List;
    vToolBar.Transparent := vSourceToolBar.Transparent;
    vToolBar.Color := vSourceToolBar.Color;

    Result := TVCLArea.Create(Self, AView, '', False, vToolBar);

    vResolution := StrToIntDef(TDomain(TInteractor(Interactor).Domain).Settings.GetValue('Core',
      'CfgImagesResolution'), 32);
    vToolBar.Images := TDragImageList(TInteractor(AView.Interactor).Images[vResolution]);

    if LowerCase(vSourceToolBar.Caption) = '@navigation' then
    begin
      vNavItems := GetNavigableCollections;
      try
        for vNavDefinition in vNavItems do
        begin
          vChildArea := CreateChildToolButton(Result, 'View=' + vNavDefinition.Name +
            '@WorkArea=WorkArea@Layout=Collection');
          Result.AddArea(vChildArea);
        end;
      finally
        FreeAndNil(vNavItems);
      end;
    end;
  end
  else if ALayout is TToolButton then
    Result := CreateChildToolButton(Self, vSourceToolButton.Caption)
  else if ALayout is TMainMenu then
  begin
    vMenu := TMainMenu.Create(Component);
    vMenu.Images := TDragImageList(TInteractor(Interactor).Images[16]);
    Result := TVCLArea.Create(Self, AView, 'Menu', False, vMenu);
    CopyMenuItems(Result, TMenu(ALayout).Items, vMenu.Items);
  end
  else if ALayout is TPopupMenu then
  begin
    vMenu := TPopupMenu.Create(Component);
    vMenu.Images := TDragImageList(TInteractor(Interactor).Images[16]);
    Result := TVCLArea.Create(Self, AView, 'Popup', False, vMenu);
    CopyPopupMenuItems(Result, TMenu(ALayout).Items, vMenu.Items);
  end
  else if ALayout is TBitBtn then
    Result := CreateChildButton(Self, vSourceBtn)
  else if ALayout is TBevel then
  begin
    vBevel := TBevel.Create(nil);
    vBevel.SetBounds(vSourceBevel.Left, vSourceBevel.Top, vSourceBevel.Width, vSourceBevel.Height);
    vBevel.Align := vSourceBevel.Align;
    vBevel.Shape := vSourceBevel.Shape;
    vBevel.Style := vSourceBevel.Style;
    Result := TVCLArea.Create(Self, AView, '-bevel-', False, vBevel);
  end
  else if ALayout is TSplitter then
  begin
    vSplitter := TcxSplitter.Create(nil);
    case vSourceSplitter.Align of
      alTop: vSplitter.AlignSplitter := salTop;
      alLeft: vSplitter.AlignSplitter := salLeft;
      alRight: vSplitter.AlignSplitter := salRight;
    else
      vSplitter.AlignSplitter := salBottom;
    end;
    vSplitter.Cursor := vSourceSplitter.Cursor;
    //vSplitter.Control := TVCLArea(TInteractor(Interactor).UIBuilder.RootArea).Control;
    //vSplitter.AllowHotZoneDrag := False;
    vSplitter.HotZone := TcxSimpleStyle.Create(vSplitter);
    vSplitter.SetBounds(vSourceSplitter.Left, vSourceSplitter.Top, vSourceSplitter.Width, vSourceSplitter.Height);
    vSplitter.Color := vSourceSplitter.Color;
    vSplitter.ParentColor := False;
    vSplitter.NativeBackground := False;
    Result := TVCLArea.Create(Self, AView, '-splitter-', False, vSplitter);
  end
  else if ALayout is TPanel then
  begin
    if AParams <> '' then
      vParams := CreateDelimitedList(AParams, '&')
    else
      vParams := nil;

    if (vSourcePanel.Caption = 'NavigationArea') then
    begin
      Result := CreateNavigationArea(vSourcePanel, AView, AParams);
    end
    else if LowerCase(vSourcePanel.Caption) = '@navigation' then
    begin  
      Result := CreateNavigation(Self);
    end
    else if Assigned(vParams) and (vParams.Values['ViewType'] = 'Paged') then
    begin
      if TInteractor(FView.Interactor).Layout = 'mdi' then
      begin
        FUIBuilder.DefaultParams := AParams;
        FreeAndNil(vParams);
        Exit(nil);
      end;

      vPC := TcxPageControl.Create(nil);
      vPC.DoubleBuffered := True;
      vPC.Width := vSourcePanel.Width;
      vPC.Height := vSourcePanel.Height;
      vPC.Left := vSourcePanel.Left;
      vPC.Top := vSourcePanel.Top;
      vPC.Properties.Images := TDragImageList(TInteractor(Interactor).Images[16]);
      vPC.Properties.TabPosition := tpBottom;
      if vParams.Values['PageLayout'] = 'Top' then
        vPC.Properties.TabPosition := tpTop;
      vPC.Properties.CloseButtonMode := cbmActiveTab;
      vPC.OnCanClose := OnPCCanClose;
      vPC.Align := vSourcePanel.Align;
      vPC.Anchors := vSourcePanel.Anchors;
      vPC.Font.Assign(vSourcePanel.Font);
      Result := TVCLArea.Create(Self, AView, Trim(vSourcePanel.Caption), False, vPC);

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
      vPanel := TPanel.Create(nil);
      vPanel.Width := vSourcePanel.Width;
      vPanel.Height := vSourcePanel.Height;
      vPanel.Left := vSourcePanel.Left;
      vPanel.Top := vSourcePanel.Top;
      vPanel.Align := vSourcePanel.Align;
      vPanel.AlignWithMargins := vSourcePanel.AlignWithMargins;
      vPanel.Margins := vSourcePanel.Margins;
      vPanel.Padding := vSourcePanel.Padding;
      if AView.DefinitionKind <> dkListField then
        vPanel.BevelOuter := vSourcePanel.BevelOuter
      else
        vPanel.BevelOuter := bvNone;
      vPanel.Anchors := vSourcePanel.Anchors;
      if not vSourcePanel.ParentBackground then
      begin
        vPanel.Color := vSourcePanel.Color;
        vPanel.ParentColor := False;
        vPanel.ParentBackground := False;
      end;
      //Result := TVCLArea.Create(Self, AView, Trim(vSourcePanel.Caption), False, vPanel, nil, AParams);
      Result := TUIAreaClass(Self.ClassType).Create(Self, AView, Trim(vSourcePanel.Caption), False, vPanel, nil, AParams);
    end;

    Result.AddParams(vParams);
  end
  else if ALayout is TScrollBox then
  begin
    vBox := TcxScrollBox.Create(nil);
    vBox.Width := vSourceBox.Width;
    vBox.Height := vSourceBox.Height;
    vBox.Left := vSourceBox.Left;
    vBox.Top := vSourceBox.Top;
    vBox.Align := vSourceBox.Align;
    vBox.AlignWithMargins := vSourceBox.AlignWithMargins;
    vBox.Margins := vSourceBox.Margins;
    vBox.Padding := vSourceBox.Padding;
    vBox.LookAndFeel.ScrollbarMode := sbmClassic;
    if vSourceBox.BorderStyle = bsNone then
      vBox.BorderStyle := cxcbsNone;
    vBox.Anchors := vSourceBox.Anchors;
    Result := TVCLArea.Create(Self, AView, '', False, vBox);
  end
  else if ALayout is TMemo then
  begin
    vPanel := TPanel.Create(nil);
    vPanel.Width := vSourcePanel.Width;
    vPanel.Height := vSourcePanel.Height;
    vPanel.Left := vSourcePanel.Left;
    vPanel.Top := vSourcePanel.Top;
    vPanel.Align := vSourcePanel.Align;
    vPanel.AlignWithMargins := vSourcePanel.AlignWithMargins;
    vPanel.Margins := vSourcePanel.Margins;
    vPanel.Padding := vSourcePanel.Padding;
    if AView.DefinitionKind <> dkListField then
      vPanel.BevelOuter := vSourcePanel.BevelOuter
    else
      vPanel.BevelOuter := bvNone;
    vPanel.Anchors := vSourcePanel.Anchors;
    if not vSourcePanel.ParentBackground then
    begin
      vPanel.Color := vSourcePanel.Color;
      vPanel.ParentColor := False;
      vPanel.ParentBackground := False;
    end;
    Result := TVCLArea.Create(Self, AView, Trim(vSourcePanel.Caption), False, vPanel);
  end
  else
    Assert(False, 'Класс [' + ALayout.ClassName + '] не поддерживается для создания лэйаутов');
end;

function TVCLArea.DoGetDescription: string;
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

procedure TVCLArea.EndUpdate;
begin
  if FControl is TWinControl then
    TWinControl(FControl).EnableAlign;

  if TInteractor(Interactor).Layout = 'mdi' then
  begin
    SendMessage(Application.MainForm.ClientHandle, WM_SETREDRAW, 1, 0);
    RedrawWindow(Application.MainForm.ClientHandle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

procedure TVCLArea.ExplicitNavigate(Sender: TObject);
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
  vArea := TVCLArea(TWinControl(Sender).Tag);
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

function TVCLArea.GetComponent: TComponent;
begin
  Result := TComponent(FControl)
end;

function TVCLArea.GetControl: TControl;
begin
  if FControl is TControl then
    Result := TControl(FControl)
  else
    Result := nil;
end;

function TVCLArea.GetName: string;
begin
  if not Assigned(FControl) then
    Result := 'NULL'
  else if not (FControl is TControl) then
    Result := FControl.ClassName + ': ' + Id
  else if TCrackedControl(FControl).Caption <> '' then
    Result := FControl.ClassName + ': ' + Id + ' (' +  TCrackedControl(FControl).Caption + ')'
  else
    Result := FControl.ClassName + ': ' + Id;
end;

function TVCLArea.GetNavigableCollections: TList<TDefinition>;
var
  i: Integer;
  vSecuredState: TViewState;
  vSession: TUserSession;
begin
  Result := TList<TDefinition>.Create;
  TConfiguration(TInteractor(FView.Interactor).Configuration).Definitions.DefinitionsByKind(Result, clkDocument);
  TConfiguration(TInteractor(FView.Interactor).Configuration).Definitions.DefinitionsByKind(Result, clkLibrary);
  TConfiguration(TInteractor(FView.Interactor).Configuration).Definitions.DefinitionsByKind(Result, clkMixin);

  vSession := TUserSession(TInteractor(FView.Interactor).Session);
  for i := Result.Count - 1 downto 0 do
  begin
    if Assigned(vSession) then
    begin
      vSecuredState := Result[i].UIState and vSession.GetUIState(Result[i].Name, nil);
      if vSecuredState <> vsFullAccess then
        Result.Delete(i);
    end
    else if Result[i].UIState <> vsFullAccess then
      Result.Delete(i);
  end;

  Result.Sort(TComparer<TDefinition>.Construct(function(const Left, Right: TDefinition): Integer
    begin
      Result := Left.OrderNum - Right.OrderNum;
    end));
end;

procedure TVCLArea.OnActionMenuSelected(Sender: TObject);
var
  vControl: TComponent;
  vMenuItem: TMenuItem;
  vArea: TVCLArea;
  vSelectedIndex: Integer;
begin
  vMenuItem := TMenuItem(Sender);
  vControl := TComponent(vMenuItem.Tag);
  vArea := TVCLArea(vControl.Tag);
  if not Assigned(vArea) then
    Exit;
  if not Assigned(vArea.View) then
    Exit;
  if not Assigned(vArea.View.DomainObject) then
    Exit;

  if vControl is TMenuItem then
    vSelectedIndex := TMenuItem(vControl).IndexOf(vMenuItem)
  else begin
    if not Assigned(TcxButton(vControl).DropDownMenu) then
      Exit;
    vSelectedIndex := TPopupMenu(TcxButton(vControl).DropDownMenu).Items.IndexOf(vMenuItem);
  end;
    
  Assert(vSelectedIndex >= 0, 'Выбран неизвестный пункт меню');
  TEntity(vArea.View.DomainObject)._SetFieldValue(FSession.NullHolder, 'SelectedIndex', vSelectedIndex);
  OnExecuteAction(vControl);
end;

procedure TVCLArea.OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
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
      vCloseProc := TVCLArea(vArea).OnClose;
      TVCLArea(vArea).SetControl(nil);
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

procedure TVCLArea.OnPCCanClose(Sender: TObject; var ACanClose: Boolean);
var
  vPC: TcxPageControl;
  vTab: TcxTabSheet;
  vPCArea: TUIArea;
  vTabArea: TUIArea;
begin
  // Closing will be done using TUIArea mechanics
  ACanClose := False;

  vPC := TcxPageControl(Sender);
  vTab := vPC.ActivePage;
  if vTab = nil then
    Exit;

  FUIBuilder.LastArea := nil;

  vPCArea := TUIArea(vPC.Tag);
  vTabArea := TUIArea(vTab.Tag);

  vPCArea.RemoveArea(vTabArea);

  FUIBuilder.PrintHierarchy;
end;

procedure TVCLArea.PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FControl is TControl then
    Control.SetBounds(ALeft, ATop, AWidth, AHeight);
  FOriginLeft := ALeft; FOriginTop := ATop;
  PlaceLabel;
end;

procedure TVCLArea.PlaceLabel;
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

procedure TVCLArea.RefillArea(const AKind: Word);
begin
  inherited RefillArea(AKind);

  if Assigned(FCaption) then
    FCaption.Visible := FShowCaption and (FView.State > vsHidden);
end;

destructor TVCLArea.Destroy;
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

procedure TVCLArea.SaveLayoutToFile(const AFileName: string);
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

procedure TVCLArea.SetCaptionProperty(const ALayout: TObject);
var
  vPanel: TPanel absolute ALayout;
begin
  if not Assigned(FCaption) then
    Exit;

  if Assigned(vPanel) then
  begin
    FShowCaption := vPanel.ShowCaption;
    FCaption.Visible := FShowCaption and (FView.State > vsHidden);
    if akTop in vPanel.Anchors then
      FCaption.Anchors := [akLeft, akTop]
    else
      FCaption.Anchors := [akLeft, akBottom];

    if vPanel.DoubleBuffered then
      SetLabelPosition(lpLeft);
  end;
end;

procedure TVCLArea.SetControl(const AControl: TObject);
begin
  if not (AControl is TComponent) then
    Exit;

  if Assigned(FControl) then
  begin
    TCrackedControl(FControl).Tag := 0;
    if FControl is TWinControl then
    begin
      TCrackedControl(FControl).OnEnter := nil;
      TCrackedControl(FControl).OnExit := nil;
    end;
  end;

  inherited SetControl(AControl);

  if Assigned(FControl) then
  begin
    TCrackedControl(FControl).Tag := Integer(Self);
    if FControl is TWinControl then
    begin
      TCrackedControl(FControl).OnEnter := OnEnter;
      TCrackedControl(FControl).OnExit := OnExit;
    end;
  end;

  FIsForm := FControl is TForm;
  FIsAutoReleased := FControl is TMenuItem;
end;

procedure TVCLArea.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  PlaceLabel;
end;

procedure TVCLArea.SetParent(const Value: TUIArea);
begin
  inherited SetParent(Value);

  if not (FControl is TControl) then
    Exit;

  // Установка родителя для контрола
  if FIsForm or not Assigned(Value) then
    Control.Parent := nil
  else
    Control.Parent := TWinControl(TVCLArea(Value).Control);
end;

procedure TVCLArea.SetViewState(const AValue: TViewState);
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

    vBoolValue := (AValue > vsDisabled) and FStyle.Enabled;
    if FControl is TcxTabSheet then
      TcxTabSheet(FControl).AllowCloseButton := vBoolValue
    else if Control.Enabled <> vBoolValue then
      Control.Enabled := vBoolValue;
  end;
end;

procedure TVCLArea.UnbindContent;
begin
  if FIsAutoReleased then
    FControl := nil;
end;

procedure TVCLArea.UpdateArea(const AKind: Word; const AParameter: TEntity = nil);
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
    vArea: TVCLArea;
  begin
    if AMenuItem.Count > 0 then
    begin
      for vChildItem in AMenuItem do
        CheckMenuItems(vChildItem);
      Exit;
    end;

    vArea := TVCLArea(AMenuItem.Tag);
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
  const AControl: TObject = nil; const ALayout: TObject = nil; const AParams: string = '');
var
  vPopupArea: TVCLArea;
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
  if Assigned(ALayout) and (ALayout is TPanel) and Assigned(TPanel(ALayout).PopupMenu)
    and Assigned(FControl) and (FControl is TControl)
  then begin
    vPopupArea := TVCLArea(Parent.AreaById('Popup')); // TPopupMenu always created on form (todo: get TForm area)
    if Assigned(vPopupArea) then
    begin
      vPopupMenu := TPopupMenu(vPopupArea.Component);
      if not Assigned(vPopupMenu.OnPopup) then
        vPopupMenu.OnPopup := BeforeContextMenuShow;
      TCrackedControl(Control).PopupMenu := vPopupMenu;
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

procedure TUILayout.Build(const AVCLArea: TVCLArea);
  procedure Process(const ALayout: TUILayout; const AVCLArea: TVCLArea);
  var
    i: Integer;
  begin
    if Assigned(AVCLArea.Control) then
      ALayout.FillLayoutParams(ALayout, AVCLArea.Control);
    for i := 0 to AVCLArea.Count - 1 do
      Process(ALayout.CreateChild, TVCLArea(AVCLArea[i]));
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

constructor TNavigationArea.Create(const ASource: TPanel; const AView: TView; const AArea: TVCLArea; const AInteractor: TObject; const AParams: string);
begin
  FView := AView;
  FParams := AParams;
end;

function TNavigationArea.CreateItem(const AParentObj: TObject; const ALevel: Integer): TObject;
begin
  FCurrentLevel := ALevel;
  Result := DoCreateItem(AParentObj);
end;

destructor TNavigationArea.Destroy;
begin
  inherited;
end;

procedure TNavigationArea.DoAfterCreate(const AInteractor: TObject);
begin
end;

{ TNavBarArea }

constructor TNavBarArea.Create(const ASource: TPanel; const AView: TView; const AArea: TVCLArea; const AInteractor: TObject; const AParams: string);
begin
  inherited;

  FNavBar := TdxNavBar.Create(nil);
  FNavBar.DoubleBuffered := True;
  FNavBar.Width := ASource.Width;
  FNavBar.Height := ASource.Height;
  FNavBar.Left := ASource.Left;
  FNavBar.Top := ASource.Top;
  FNavBar.SmallImages := TDragImageList(TInteractor(AInteractor).Images[16]);
  FNavBar.LargeImages := TDragImageList(TInteractor(AInteractor).Images[32]);
  FNavBar.View := StrToIntDef(GetUrlParam(AParams, 'NavBarKind'), 4);
  FNavBar.OptionsStyle.DefaultStyles.GroupHeader.Font.Assign(ASource.Font);
  FNavBar.OptionsStyle.DefaultStyles.GroupHeader.Font.Size := 12;
  FNavBar.OptionsStyle.DefaultStyles.Item.Font.Assign(ASource.Font);
  FNavBar.OptionsStyle.DefaultStyles.Background.BackColor := ASource.Color;
  FNavBar.OptionsStyle.DefaultStyles.Background.BackColor2 := DimColor(ASource.Color, 0.2);
  FNavBar.OptionsStyle.DefaultStyles.GroupBackground.BackColor := ASource.Color;
  FNavBar.OptionsStyle.DefaultStyles.GroupBackground.BackColor2 := DimColor(ASource.Color, 0.2);
  FNavBar.OptionsStyle.DefaultStyles.GroupHeader.BackColor := ASource.Color;
  FNavBar.OptionsStyle.DefaultStyles.GroupHeader.BackColor2 := DimColor(ASource.Color, 0.2);
  FNavBar.DragDropFlags := [];

  FNavBarGroup := nil;
  FNavBarItem := nil;
end;

destructor TNavBarArea.Destroy;
begin
  FreeAndNil(FNavBar);
  inherited;
end;

procedure TNavBarArea.DoAssignItemOnClick(const AHandler: TNotifyEvent);
begin
  if FCurrentLevel = 0 then
    FNavBarGroup.OnClick := AHandler
  else
    FNavBarItem.OnClick := AHandler;
end;

procedure TNavBarArea.DoAssingItemProperties(const ACaption, AHint: string; const AImageIndex: Integer);
begin
  if FCurrentLevel = 0 then
  begin
    FNavBarGroup.Caption := ACaption;
    FNavBarGroup.Hint := AHint;
    FNavBarGroup.SmallImageIndex := AImageIndex;
    FNavBarGroup.LargeImageIndex := AImageIndex;
  end
  else
  begin
    FNavBarItem.Caption := ACaption;
    FNavBarItem.Hint := AHint;
    FNavBarItem.SmallImageIndex := AImageIndex;
    FNavBarItem.LargeImageIndex := AImageIndex;
  end;
end;

function TNavBarArea.DoCreateItem(const AParentObj: TObject): TObject;
begin
  if FCurrentLevel = 0 then
  begin
    FNavBarGroup := FNavBar.Groups.Add;
    FNavBarGroup.LinksUseSmallImages := False;
    FNavBarGroup.OptionsExpansion.Expandable := False;
    FNavBarGroup.OptionsExpansion.ShowExpandButton := False;
    Result := FNavBarGroup;
  end
  else
  begin
    FNavBarItem := FNavBar.Items.Add;
    if Assigned(AParentObj) and (AParentObj is TdxNavBarGroup) then
      TdxNavBarGroup(AParentObj).CreateLink(FNavBarItem);
    Result := FNavBarItem;
  end;
end;

function TNavBarArea.GetControl: TObject;
begin
  Result := FNavBar;
end;

{ TOneButtonArea }

constructor TOneButtonArea.Create(const ASource: TPanel; const AView: TView; const AArea: TVCLArea; const AInteractor: TObject; const AParams: string);
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

function TOneButtonArea.DoCreateItem(const AParentObj: TObject): TObject;
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

constructor TMainMenuArea.Create(const ASource: TPanel; const AView: TView; const AArea: TVCLArea; const AInteractor: TObject; const AParams: string);
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

function TMainMenuArea.DoCreateItem(const AParentObj: TObject): TObject;
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
end;

function TMainMenuArea.GetControl: TObject;
begin
  Result := FMenu;
end;

{ TToolBarArea }

constructor TToolBarArea.Create(const ASource: TPanel; const AView: TView; const AArea: TVCLArea; const AInteractor: TObject; const AParams: string);
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

function TToolBarArea.DoCreateItem(const AParentObj: TObject): TObject;
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
      FMenuItem := TMenuItem.Create(nil);

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

constructor TTreeViewArea.Create(const ASource: TPanel; const AView: TView; const AArea: TVCLArea; const AInteractor: TObject; const AParams: string);
begin
  inherited;
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

function TTreeViewArea.DoCreateItem(const AParentObj: TObject): TObject;
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

initialization

RegisterClasses([TLabel, TPanel, TSplitter, TImage, TBevel, TPageControl, TMemo,
  TTabSheet, TToolBar, TToolButton, TBitBtn, TScrollBox, TMainMenu, TPopupMenu,
  TShape, TListView, TImageList]);

end.
