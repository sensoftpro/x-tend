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

unit vclArea;

interface

uses
  Windows, Classes, Forms, Messages, Generics.Collections, Controls, StdCtrls, ExtCtrls, Menus, UITypes, SysUtils,
  uConsts, uUIBuilder, uDefinition, uEntity, uView, uLayout;

type
  TVCLControl = class(TNativeControl)
  protected
    function AreaFromSender(const ASender: TObject): TUIArea; override;
    procedure PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DoClose(const AModalResult: Integer); override;
    function GetName: string; override;
    procedure DoActivate(const AUrlParams: string); override;
    procedure SetViewState(const AValue: TViewState); override;
    procedure SetParent(const AParent: TUIArea); override;
    procedure SetControl(const AControl: TObject); override;
    function DoGetDescription: string; override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
  public
    constructor Create(const AOwner: TUIArea; const AControl: TObject); override;
  end;

  TLabelPosition = (lpTop, lpLeft);

  TVCLFieldArea = class;
  TVCLFieldAreaClass = class of TVCLFieldArea;

  TVCLArea = class(TUIArea)
  private
    FLabelPosition: TLabelPosition;
    FCaption: TLabel;
    FOnClose: TProc;
    // Выполнение действий (Actions и переходы)
    procedure OnPCCanClose(Sender: TObject; var ACanClose: Boolean);
    procedure OnActionMenuSelected(Sender: TObject);
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
    function GetControl: TObject;
    function GetComponent: TComponent;
    procedure BeforeContextMenuShow(Sender: TObject);
    procedure CopyPopupMenuItems(const AParent: TUIArea; const AView: TView;
      const ASrcItem: TNavigationItem; const ADestMenu: TMenuItem);
  protected
    FNeedCreateCaption: Boolean;
    FShowCaption: Boolean;
    FTabStop: Boolean;
    FTabOrder: Integer;
    procedure PlaceLabel;
    function GetDisplayFormat(const AFieldDef: TFieldDef; const AEntity: TEntity): string;

    function DoCreateChildArea(const ALayout: TLayout; const AView: TView; const AParams: string = ''; const AOnClose: TProc = nil): TUIArea; override;
    function DoCreateChildNavigation(const ALayout: TLayout; const AView: TView; const AParams: string = ''): TUIArea; override;
    function AppendServiceArea: TUIArea; override;
    procedure CreateCaption(const AFieldDef: TFieldDef); override;
    procedure DoAfterChildAreasCreated; override;

    property FControl: TObject read GetControl;
  protected
    function TryCreatePopupArea(const ALayout: TLayout): TUIArea; override;
    procedure SetPopupArea(const APopupArea: TUIArea); override;
    procedure AssignFromLayout(const ALayout: TLayout; const AParams: string); override;
    procedure ArrangeChildAreas; override;
    procedure SaveLayoutToFile(const AFileName: string); override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure RefillArea(const AKind: Word); override;

    procedure SetCaptionProperty(const ALayout: TLayout); virtual;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = ''); override;
    destructor Destroy; override;

    property Component: TComponent read GetComponent;
    //property Control: TControl read GetControl;
    property OnClose: TProc read FOnClose write FOnClose;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition;
  end;

  TVCLFieldArea = class(TVCLArea)
  private
    procedure Validate;

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

    function GetFormat: string;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = ''); override;
    destructor Destroy; override;

    procedure Deinit;

    property Focused: Boolean read GetFocused write SetFocused;
    property LayoutPositionCount: Integer read GetLayoutPositionCount;
  end;

  TActionArea = class(TVCLArea)
  protected
    FParams: string;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = ''); override;
  end;

  TButtonArea = class(TActionArea)
  private
    FTypeSelectionMenu: TPopupMenu;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure RefillArea(const AKind: Word); override;
  public
    destructor Destroy; override;
  end;

  TLinkArea = class(TActionArea)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TNavigationArea = class(TVCLArea)
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
  vclSimpleEditors, uEntityList, uDomainUtils, uChangeManager;

type
  TCrackedWinControl = class(TWinControl) end;
  TCrackedControl = class(TControl) end;

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

  TMainMenuArea = class(TNavigationArea)
  private
    FMenu: TMainMenu;
    FMenuItem: TMenuItem;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TToolBarArea = class(TNavigationArea)
  private
    FToolBar: TToolBar;
    FToolButton: TToolButton;
    FMenuItem: TMenuItem;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
    procedure DoAfterCreate(const AInteractor: TObject); override;
  end;

  TTreeViewArea = class(TNavigationArea)
  private
    FTreeView: TTreeView;
    FDefaultWorkArea: string;
    procedure SelectNode(const ANode: TTreeNode);
    procedure OnKeyPress(Sender: TObject; var Key: Char);
    procedure OnMouseClick(Sender: TObject);
    procedure Refill;
    //procedure OnCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    //procedure DrawButton(ARect: TRect; Node: TTreeNode);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
    procedure DoExecuteUIAction(const AView: TView); override;
  end;

  TNavBarArea = class(TNavigationArea)
  private
    FNavBar: TdxNavBar;
    FNavBarGroup: TdxNavBarGroup;
    FNavBarItem: TdxNavBarItem;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TOneButtonArea = class(TNavigationArea)
  private
    FButton: TButton;
    FMenu: TPopupMenu;
    FItem: TMenuItem;
    procedure OnClick(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
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

function TVCLArea.AppendServiceArea: TUIArea;
var
  vPanel: TPanel;
begin
  vPanel := TPanel.Create(nil);
  vPanel.BevelOuter := bvNone;
  vPanel.Height := cServiceAreaHeight;
  vPanel.Align := alBottom;
  Result := TVCLArea.Create(Self, FView, '', True, vPanel);
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
    vEditor := TVCLArea(Areas[i]);
    vEditor.SetBounds(
      cBorder + (vRealColumnCount - 1) * (cColumnWidth + cBetweenColumns), cBorder + cBetweenRows + vCurColumnHeight,
      cColumnWidth, CalcLayoutPositionCount(vEditor) * (vOneRowHeight + cBetweenRows) - cBetweenRows);

    vCurColumnHeight := vCurColumnHeight + TControl(TVCLFieldArea(vEditor).FControl).Height + cBetweenRows;
    if vCurColumnHeight >= vHeightInColumn then
    begin
      if vCurColumnHeight > vRealMaxHeightInColumn then
        vRealMaxHeightInColumn := vCurColumnHeight;
      vCurColumnHeight := 0;
      if i < Count - 1 then  // not last
        vRealColumnCount := vRealColumnCount + 1;
    end;
  end;

  TControl(FControl).ClientWidth := (vRealColumnCount * (cColumnWidth + cBetweenColumns) - cBetweenColumns) + cBorder*2;
  TControl(FControl).ClientHeight := vRealMaxHeightInColumn + cBorder * 2 + 8;
  if FNativeControl.IsForm and (FView.DefinitionKind <> dkDomain) then
    TControl(FControl).ClientHeight := TControl(FControl).ClientHeight + cServiceAreaHeight;
end;

procedure TVCLArea.AssignFromLayout(const ALayout: TLayout; const AParams: string);
var
  //vPage: TTabSheet absolute ALayout;
  vFrame: TFrame;
  vPanel: TCrackedWinControl;
  vForm: TForm;
  vAlignment: TAlignment;
  vWS: Integer;
begin
  if FNativeControl.IsForm then
  begin
    vForm := TForm(FControl);

    if ALayout.Control is TFrame then
    begin
      vFrame := TFrame(ALayout.Control);
      if (vFrame.Tag and cFormResizable) > 0 then
      begin
        vForm.BorderStyle := bsSizeable;
        vForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize];
      end;

      if (vFrame.Tag and cFormDisableMinimizeButton) > 0 then
        vForm.BorderIcons := vForm.BorderIcons - [biMinimize];

      if (vFrame.Tag and cFormDisableMaximizeButton) > 0 then
        vForm.BorderIcons := vForm.BorderIcons - [biMaximize];

      if (vFrame.Tag and cFormPositionDesign) > 0 then
        vForm.Position := poDesigned;

      if (vFrame.Tag and cFormNotResizable) > 0 then
        vForm.BorderStyle := bsSingle;

      vWS := StrToIntDef(GetUrlParam(AParams, 'WindowState', ''), -1);
      if vWS > -1 then
        vForm.WindowState := TWindowState(vWS);

      if (vForm.WindowState = wsNormal) then
      begin
        if (vForm.FormStyle <> fsMDIChild) or ((vFrame.Tag and cFormUseDesignSizes) > 0) then
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
      vForm.Caption := TTabSheet(ALayout.Control).Caption;
    end;
  end
  else if ALayout.Control is TFrame then
  begin
    vFrame := TFrame(ALayout.Control);
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
  else if ((ALayout.Control is TPanel) or (ALayout.Control is TMemo)) and (FControl is TControl) then
  begin
    vPanel := TCrackedWinControl(ALayout.Control);

    SetBounds(vPanel.Left, vPanel.Top, vPanel.Width, vPanel.Height);
    SetCaptionProperty(ALayout);

    if FControl is TcxCustomEdit then
      TcxCustomEdit(FControl).Style.Font.Assign(vPanel.Font)
    else
      TCrackedWinControl(FControl).Font.Assign(vPanel.Font);

    TControl(FControl).Anchors := vPanel.Anchors;
    TControl(FControl).Align := vPanel.Align;
    TControl(FControl).AlignWithMargins := vPanel.AlignWithMargins;
    TControl(FControl).Margins := vPanel.Margins;
    if FControl is TWinControl then
      TWinControl(FControl).Padding := vPanel.Padding;

    vAlignment := taLeftJustify;

    if ALayout.Control is TPanel then
      vAlignment := TPanel(ALayout.Control).Alignment
    else if ALayout.Control is TMemo then
      vAlignment := TMemo(ALayout.Control).Alignment;

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
      TCrackedWinControl(FControl).Color := vPanel.Color;
      TCrackedWinControl(FControl).ParentColor := False;
      TCrackedWinControl(FControl).ParentBackground := False;
    end;

  end;
end;

procedure TVCLArea.BeforeContextMenuShow(Sender: TObject);
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

procedure TVCLArea.CopyPopupMenuItems(const AParent: TUIArea; const AView: TView;
  const ASrcItem: TNavigationItem; const ADestMenu: TMenuItem);
var
  vCaption: string;
  vParams: TStrings;
  vActions: TList<TActionDef>;
  vReports: TList<TRTFReport>;
  vDefinitions: TList<TDefinition>;
  vId: string;
  i, j: Integer;
  vDefinition: TDefinition;
  vSrcItem: TNavigationItem;
  vDestItem: TMenuItem;
  vChildItem: TMenuItem;
  vView: TView;
  vAction: TActionDef;
  vReport: TRTFReport;
  vChildArea: TVCLArea;
begin
  for i := 0 to ASrcItem.Items.Count - 1 do
  begin
    vSrcItem := TNavigationItem(ASrcItem.Items[i]);
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
            vDestItem.OnClick := OnAreaClick;
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
            vDestItem.OnClick := OnAreaClick;

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
            vChildItem.Tag := Integer(vDestItem);
            vChildItem.OnClick := OnActionMenuSelected;
            vDestItem.Add(vChildItem);
          end;
        end
        else
          vDestItem.OnClick := OnAreaClick;
      end
      else
        vDestItem.OnClick := OnAreaClick;

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

constructor TVCLArea.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
  const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = '');
begin
  FNeedCreateCaption := True;

  if not Assigned(AControl) then
    FNeedCreateCaption := True;

  inherited Create(AParent, AView, AId, AIsService, AControl, ALayout, AParams);

  if Assigned(ALayout) and (ALayout.Control is TWinControl) then
  begin
    if FControl is TWinControl then
    begin
      FTabStop := TWinControl(ALayout.Control).TabStop;
      FTabOrder := TWinControl(ALayout.Control).TabOrder;
      TWinControl(FControl).TabStop := FTabStop;
    end;
  end;
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
    FCaption.Parent := TControl(FControl).Parent;
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

type
  TVCLAreaComparer = class(TComparer<TVCLArea>)
  public
    function Compare(const ALeft, ARight: TVCLArea): Integer; override;
  end;

function TVCLAreaComparer.Compare(const ALeft, ARight: TVCLArea): Integer;
begin
  Result := ARight.FTabOrder - ALeft.FTabOrder;
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
      if (Areas[c].InnerControl <> ASplitter) and (Areas[c].InnerControl is TControl)
        and (TControl(Areas[c].InnerControl).Align = ASplitter.Align) then
      begin
        Result := TControl(Areas[c].InnerControl);
        Break;
      end;
  end;

  procedure ApplyTabOrder;
  var
    vList: TList<TVCLArea>;
    v: Integer;
  begin
    vList := TList<TVCLArea>.Create(TVCLAreaComparer.Create);
    try
      for v := 0 to Count - 1 do
        if (Areas[v].InnerControl is TWinControl) and TWinControl(Areas[v].InnerControl).TabStop then
          vList.Add(TVCLArea(Areas[v]));

      vList.Sort;

      for v := 0 to vList.Count - 1 do
        TWinControl(vList[v].InnerControl).TabOrder := vList[v].FTabOrder;
    finally
      FreeAndNil(vList);
    end;
  end;
begin
  // прицепляем сплиттер к своему контролу, чтобы не отлипал в некоторых случаях, обрабатываем только простой случай: сплиттер с таким размещением один в текущей области
  for i := 0 to Count - 1 do
    if Areas[i].InnerControl is TcxSplitter then
      TcxSplitter(Areas[i].InnerControl).Control := FindControlForSplitter(TcxSplitter(Areas[i].InnerControl));

  if Count > 1 then
    ApplyTabOrder;
end;

function TVCLArea.DoCreateChildArea(const ALayout: TLayout; const AView: TView; const AParams: string = ''; const AOnClose: TProc = nil): TUIArea;
var
  vSourceLabel: TLabel;
  vSourceImage: TImage;
  vSourcePC: TPageControl;
  vSourceTabSheet: TTabSheet;
  vSourcePanel: TPanel;
  vSourceBox: TScrollBox;
  vSourceBevel: TBevel;
  vSourceSplitter: TSplitter;
  vSourceShape: TShape;
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
  vMenuArea: TVCLArea;

begin
  Result := nil;

  if not Assigned(ALayout) then
    Exit;

  vDomain := TDomain(TInteractor(Interactor).Domain);

  if ALayout.Control is TShape then
  begin
    vSourceShape := TShape(ALayout.Control);
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
    vShape.AlignWithMargins := vSourceShape.AlignWithMargins;
    vShape.Margins := vSourceShape.Margins;

    Result := TVCLArea.Create(Self, AView, '', False, vShape, ALayout);
  end
  else if ALayout.Control is TLabel then
  begin
    vSourceLabel := TLabel(ALayout.Control);
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

    Result := TVCLArea.Create(Self, AView, '', False, vLabel, ALayout);
  end
  else if ALayout.Control is TImage then
  begin
    vSourceImage := TImage(ALayout.Control);
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
    vImage.Transparent := vSourceImage.Transparent;
    vImage.Properties.ShowFocusRect := False;
    vImage.Properties.PopupMenuLayout.MenuItems := [];
    vImage.Properties.FitMode := ifmNormal;
    vImage.DoubleBuffered := True;
    vImage.Properties.Stretch := vSourceImage.Stretch;
    vImage.Properties.Proportional := vSourceImage.Proportional;
    vImage.Hint := vSourceImage.Hint;
    vImage.AlignWithMargins := vSourceImage.AlignWithMargins;
    vImage.Margins := vSourceImage.Margins;
    vImage.Properties.Center := vSourceImage.Center;

    Result := TVCLArea.Create(Self, AView, '', False, vImage, ALayout);
  end
  else if ALayout.Control is TPageControl then
  begin
    vSourcePC := TPagecontrol(ALayout.Control);
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

    Result := TVCLArea.Create(Self, AView, '', False, vPC, ALayout);
  end
  else if ALayout.Control is TTabSheet then
  begin
    vSourceTabSheet := TTabSheet(ALayout.Control);
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
      Result := TVCLArea.Create(Self, AView, vSourceTabSheet.Name, False, vForm, ALayout);
      TVCLArea(Result).OnClose := AOnClose;
    end
    else begin
      vTab := TcxTabSheet.Create(Component);
      vTab.Caption := vSourceTabSheet.Caption;
      vTab.ImageIndex := GetImageID(vSourceTabSheet.ImageIndex);

      vStartPageName := TDomain(FView.Domain).Settings.GetValue('Core', 'StartPage', '');
      vTab.AllowCloseButton := not SameText(vSourceTabSheet.Name, vStartPageName);
      vTab.TabVisible := vSourceTabSheet.TabVisible;

      Result := TVCLArea.Create(Self, AView, vSourceTabSheet.Name, False, vTab, ALayout);

//      if vSourceTabSheet.Visible then // это нужно? иначе лишние перерисовки контролов идут
//        TcxPageControl(FControl.Control).ActivePage := vTab;
    end;
  end
  else if ALayout.Control is TPopupMenu then
  begin
    Result := nil;
  end
  else if ALayout.Control is TBevel then
  begin
    vSourceBevel := TBevel(ALayout.Control);
    vBevel := TBevel.Create(nil);
    vBevel.SetBounds(vSourceBevel.Left, vSourceBevel.Top, vSourceBevel.Width, vSourceBevel.Height);
    vBevel.Align := vSourceBevel.Align;
    vBevel.Shape := vSourceBevel.Shape;
    vBevel.Style := vSourceBevel.Style;
    vBevel.AlignWithMargins := vSourceBevel.AlignWithMargins;
    vBevel.Margins := vSourceBevel.Margins;
    Result := TVCLArea.Create(Self, AView, '-bevel-', False, vBevel, ALayout);
  end
  else if ALayout.Control is TSplitter then
  begin
    vSourceSplitter := TSplitter(ALayout.Control);
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
    Result := TVCLArea.Create(Self, AView, '-splitter-', False, vSplitter, ALayout);
  end
  else if ALayout.Control is TPanel then
  begin
    vSourcePanel := TPanel(ALayout.Control);

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
      Result := TVCLArea.Create(Self, AView, Trim(vSourcePanel.Caption), False, vPC, ALayout);

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
      Result := TVCLArea.Create(Self, AView, Trim(vSourcePanel.Caption), False, vPanel, ALayout, AParams);
    end;

    Result.AddParams(vParams);
  end
  else if ALayout.Control is TScrollBox then
  begin
    vSourceBox := TScrollBox(ALayout.Control);
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
  else if ALayout.Control is TMemo then
  begin
    vSourcePanel := TPanel(ALayout.Control);

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
  else if Assigned(ALayout.Control) then
    Assert(False, 'Класс [' + ALayout.Control.ClassName + '] не поддерживается для создания лэйаутов')
  else
    Assert(False, 'Пустой класс для лэйаута');
end;

function TVCLArea.DoCreateChildNavigation(const ALayout: TLayout; const AView: TView; const AParams: string): TUIArea;
var
  vStyleName: string;
  vSourcePanel: TPanel;
  vNavArea: TNavigationArea;
begin
  vSourcePanel := TPanel(ALayout.Control);
  Assert(Assigned(vSourcePanel.PopupMenu), 'У панели с именем ' + vSourcePanel.Name + ' задан Caption ' + vSourcePanel.Caption + '. Это навигационная область. Для неё нужно указать PopupMenu.');

  vStyleName := GetUrlParam(AParams, 'ViewType');
  vNavArea := TNavigationArea(TPresenter(FUIBuilder.Presenter).CreateNavigationArea(Self, ALayout, AView, vStyleName, AParams));
  vNavArea.ProcessChilds;

  Result := vNavArea;
end;

function TVCLArea.GetComponent: TComponent;
begin
  Result := TComponent(FNativeControl.Control)
end;

function TVCLArea.GetControl: TObject;
begin
  Result := FNativeControl.Control;
end;

function TVCLArea.GetDisplayFormat(const AFieldDef: TFieldDef; const AEntity: TEntity): string;
var
  i: Integer;
  vFieldName: string;
  vFormat: string;
begin
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
  OnAreaClick(vControl);
end;

procedure TVCLArea.OnCloseMDIForm(Sender: TObject; var Action: TCloseAction);
var
  vArea: TUIArea;
  vView: TView;
  vCloseView: TView;
  vInteractor: TInteractor;
  vCanBeClosed: Boolean;
  vCloseProc: TProc;
  vDelEntity: TEntity;
begin
  vArea := TUIArea(Pointer(TForm(Sender).Tag));
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
      vCloseProc := TVCLArea(vArea).OnClose;
      TVCLArea(vArea).SetControl(nil);
      FUIBuilder.RootArea.RemoveArea(vArea); // the form will be destroyed here
    end
    else
      Action := caNone;
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

procedure TVCLArea.PlaceLabel;
var
  vSpace: Integer;
begin
  if FCaption = nil then
    Exit;

  if FLabelPosition = lpTop then
  begin
    FCaption.Left := TControl(FControl).Left;
    FCaption.Top := TControl(FControl).Top - FCaption.Height - 4;
    FCaption.AutoSize := True;
    FCaption.Layout := tlTop;
  end
  else if FLabelPosition = lpLeft then
  begin
    vSpace := FCaption.Width + 8;
    FCaption.Left := TControl(FControl).Left - vSpace;
    FCaption.Top := TControl(FControl).Top + 3;
    FCaption.AutoSize := False;
    //FCaption.Layout := tlCenter;
    //FCaption.Height := Control.Height;
  end;
  FCaption.Parent := TControl(FControl).Parent;
end;

procedure TVCLArea.RefillArea(const AKind: Word);
begin
  inherited RefillArea(AKind);

  if Assigned(FCaption) then
    FCaption.Visible := FShowCaption and (FView.State > vsHidden);
end;

destructor TVCLArea.Destroy;
var
  vControl: TObject;
begin
  FOnClose := nil;

  if Assigned(FCaption) then
  begin
    FCaption.Parent := nil;
    FreeAndNil(FCaption);
  end;

  vControl := FControl;

  inherited Destroy;

  if not Assigned(vControl) then
    Exit;
  if not (vControl is TComponent) then
    Exit;

  if vControl is TForm then
  begin
    if (TForm(vControl).FormStyle = fsMDIChild) or (FId = 'float') then
      FreeAndNil(vControl);
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

procedure TVCLArea.SetCaptionProperty(const ALayout: TLayout);
var
  vPanel: TPanel;
begin
  if not Assigned(FCaption) or not Assigned(ALayout) then
    Exit;

  vPanel := TPanel(ALayout.Control);
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

procedure TVCLArea.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  PlaceLabel;
end;

procedure TVCLArea.SetPopupArea(const APopupArea: TUIArea);
var
  vPopupMenu: TPopupMenu;
begin
  vPopupMenu := TPopupMenu(APopupArea.InnerControl);
  if not Assigned(vPopupMenu.OnPopup) then
    vPopupMenu.OnPopup := BeforeContextMenuShow;
  TCrackedControl(FControl).PopupMenu := vPopupMenu;
end;

function TVCLArea.TryCreatePopupArea(const ALayout: TLayout): TUIArea;
var
  vMenu: TPopupMenu;
  vView: TView;
begin
  if Assigned(ALayout) and Assigned(ALayout.Menu) then
  begin
    vMenu := TPopupMenu.Create(Component);
    vMenu.Images := TDragImageList(TInteractor(Interactor).Images[16]);
    vView := TInteractor(FView.Interactor).UIBuilder.RootView;
    Result := TVCLArea.Create(Self, vView, '-popup-', False, vMenu);
    CopyPopupMenuItems(Self, FView, ALayout.Menu, vMenu.Items);
  end
  else
    Result := nil;
end;

procedure TVCLArea.UpdateArea(const AKind: Word; const AParameter: TEntity = nil);
begin
  RefillArea(AKind);

  if not Assigned(FControl) then
    Exit;

  if not FNativeControl.IsForm then
    SetViewState(FView.State);
end;

{ TVCLFieldArea }

constructor TVCLFieldArea.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
  const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = '');
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

function TVCLFieldArea.GetFormat: string;
begin
  Result := GetDisplayFormat(FFieldDef, FView.ParentDomainObject as TEntity);
end;

function TVCLFieldArea.GetLayoutPositionCount: Integer;
begin
  //todo: Переделать
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
    if Assigned(AVCLArea.InnerControl) then
      ALayout.FillLayoutParams(ALayout, TComponent(AVCLArea.InnerControl));
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

    vGroupArea := TVCLArea.Create(AParentArea, ACurrentView, '', False, vControl, ACurrentItem, '');
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

    vNavArea := TVCLArea.Create(AParentArea, ACurrentView, '', False, vControl, ACurrentItem, '');
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
  for i := 0 to ANavItem.Items.Count - 1 do
  begin
    vNavItem := TNavigationItem(ANavItem.Items[i]);

    if AParentArea = Self then
      vParentObj := nil
    else
      vParentObj := AParentArea.InnerControl;

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
end;

{ TNavBarArea }

function TNavBarArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  ASource: TPanel;
  AParams: string;
begin
  ASource := TPanel(ALayout.Control);
  AParams := FCreateParams.DelimitedText;

  FNavBar := TdxNavBar.Create(nil);
  FNavBar.DoubleBuffered := True;
  FNavBar.Width := ASource.Width;
  FNavBar.Height := ASource.Height;
  FNavBar.Left := ASource.Left;
  FNavBar.Top := ASource.Top;
  FNavBar.SmallImages := TDragImageList(TInteractor(FView.Interactor).Images[16]);
  FNavBar.LargeImages := TDragImageList(TInteractor(FView.Interactor).Images[32]);
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

  Result := FNavBar;
end;

function TNavBarArea.DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem;
  const ALevel: Integer; const ACaption, AHint: string; const AImageIndex: Integer): TObject;
begin
  if ALevel = 0 then
  begin
    FNavBarGroup := FNavBar.Groups.Add;
    FNavBarGroup.LinksUseSmallImages := False;
    FNavBarGroup.OptionsExpansion.Expandable := False;
    FNavBarGroup.OptionsExpansion.ShowExpandButton := False;
    FNavBarGroup.Caption := ACaption;
    FNavBarGroup.Hint := AHint;
    FNavBarGroup.SmallImageIndex := AImageIndex;
    FNavBarGroup.LargeImageIndex := AImageIndex;
    FNavBarGroup.OnClick := nil;
    Result := FNavBarGroup;
  end
  else
  begin
    FNavBarItem := FNavBar.Items.Add;
    if Assigned(AParentObj) and (AParentObj is TdxNavBarGroup) then
      TdxNavBarGroup(AParentObj).CreateLink(FNavBarItem);
    FNavBarItem.Caption := ACaption;
    FNavBarItem.Hint := AHint;
    FNavBarItem.SmallImageIndex := AImageIndex;
    FNavBarItem.LargeImageIndex := AImageIndex;
    FNavBarItem.OnClick := OnAreaClick;
    Result := FNavBarItem;
  end;
end;

{ TOneButtonArea }

function TOneButtonArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  ASource: TPanel;
  vParent: TVCLArea absolute AParent;
  vImageSize: Integer;
  vActionName: string;
  vAction: TActionDef;
begin
  ASource := TPanel(ALayout.Control);

  FButton := TButton.Create(nil);
  FButton.Style := bsSplitButton;
  FButton.Width := ASource.Width;
  FButton.Height := ASource.Height;
  FButton.Left := ASource.Left;
  FButton.Top := ASource.Top;
  FButton.Font.Assign(ASource.Font);

  vImageSize := StrToIntDef(GetUrlParam(FParams, 'ImageSize'), 16);
  FButton.Images := TDragImageList(TInteractor(FView.Interactor).Images[vImageSize]);

  vActionName := GetUrlParam(FParams, 'Action');
  if Length(vActionName) > 0 then
  begin
    FView := FView.BuildView(vActionName);
    Assert(FView.DefinitionKind = dkAction);

    vAction := TActionDef(FView.Definition);
    FButton.ImageIndex := vParent.GetImageID(vAction._ImageID);
    FButton.Caption := vParent.GetTranslation(vAction, tpCaption);
    FButton.Hint := vParent.GetTranslation(vAction, tpHint);
    FButton.OnClick := vParent.OnAreaClick;
  end
  else
  begin
    FButton.ImageIndex := vParent.GetImageID(StrToIntDef(GetUrlParam(FParams, 'ImageIndex'), -1));
    FButton.Caption := GetUrlParam(FParams, 'Caption');
    FButton.Hint := GetUrlParam(FParams, 'Hint');
    FButton.OnClick := OnClick;
  end;

  FMenu := TPopupMenu.Create(nil);
  FMenu.Images := TDragImageList(TInteractor(FView.Interactor).Images[16]);
  FButton.PopupMenu := FMenu;
  FButton.DropDownMenu := FMenu;

  Result := FButton;
end;

function TOneButtonArea.DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem;
  const ALevel: Integer; const ACaption, AHint: string; const AImageIndex: Integer): TObject;
begin
  FItem := TMenuItem.Create(nil);
  FItem.Caption := ACaption;
  FItem.Hint := AHint;
  FItem.ImageIndex := AImageIndex;
  FItem.OnClick := OnAreaClick;
  if Assigned(AParentObj) then
  begin
    Assert(AParentObj is TMenuItem);
    TMenuItem(AParentObj).Add(FItem);
  end
  else
    FMenu.Items.Add(FItem);
  Result := FItem;
end;

procedure TOneButtonArea.OnClick(Sender: TObject);
var
  vPoint: TPoint;
begin
  vPoint := FButton.ClientToScreen(Point(0, FButton.Height));
  FMenu.Popup(vPoint.X, vPoint.Y);
end;

{ TMainMenuArea }

function TMainMenuArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FMenu := TMainMenu.Create(TVCLArea(AParent).Component);
  FMenu.Images := TDragImageList(TInteractor(FView.Interactor).Images[16]);
  Result := FMenu;
end;

function TMainMenuArea.DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem;
  const ALevel: Integer; const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vForm: TForm;
begin
  FMenuItem := TMenuItem.Create(nil);
  FMenuItem.Caption := ACaption;
  FMenuItem.Hint := AHint;
  FMenuItem.ImageIndex := AImageIndex;
  FMenuItem.OnClick := OnAreaClick;

  Result := nil;
  if ALevel = 0 then
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

  if ANavItem.Id = 'Windows' then
  begin
    //FMenuItem.Visible := TInteractor(FView.Interactor).Layout = 'mdi';
    if (TInteractor(FView.Interactor).Layout = 'mdi') and (Parent.InnerControl is TForm) then
    begin
      vForm := TForm(Parent.InnerControl);
      if vForm.FormStyle = fsMDIForm then
        vForm.WindowMenu := FMenuItem;
    end;
  end;
end;

{ TToolBarArea }

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

function TToolBarArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FToolBar := TToolBar.Create(TVCLArea(AParent).Component);
  FToolBar.ShowCaptions := True;
  Result := FToolBar;
end;

function TToolBarArea.DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem; const ALevel: Integer;
  const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vMenu: TPopupMenu;
  vLeft: Integer;
  vToolButton: TToolButton;
begin
  Result := nil;

  if ALevel = 0 then
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
    FToolButton.Caption := ACaption;
    FToolButton.Hint := AHint;
    FToolButton.ImageIndex := AImageIndex;
    FToolButton.OnClick := OnAreaClick;

    Result := FToolButton;
  end
  else begin
    if Assigned(AParentObj) then
    begin
      FMenuItem := TMenuItem.Create(FToolBar);
      FMenuItem.Caption := ACaption;
      FMenuItem.Hint := AHint;
      FMenuItem.ImageIndex := AImageIndex;
      FMenuItem.OnClick := OnAreaClick;

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

{ TTreeViewArea }

function TTreeViewArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FTreeView := TTreeView.Create(nil);
  //FTreeView.Images := TDragImageList(TInteractor(FView.Interactor).Images[16]);
  FTreeView.ReadOnly := True;
  FTreeView.DoubleBuffered := True;
  FTreeView.HideSelection := False;
  FTreeView.OnClick := OnMouseClick;
  FTreeView.OnKeyPress := OnKeyPress;
  //FTreeView.OnCustomDrawItem := OnCustomDrawItem;
  FId := 'TreeView';

  FDefaultWorkArea := FCreateParams.Values['ContentWorkArea'];
  if FDefaultWorkArea = '' then
    FDefaultWorkArea := 'WorkArea';
  Result := FTreeView;
end;

function TTreeViewArea.DoCreateItem(const AParentObj: TObject; const ANavItem: TNavigationItem;
  const ALevel: Integer; const ACaption, AHint: string; const AImageIndex: Integer): TObject;
var
  vParentNode: TTreeNode absolute AParentObj;
  vTreeNode: TTreeNode;
begin
  if ANavItem.IsLine then
    Exit(nil);

  vTreeNode := FTreeView.Items.AddChild(vParentNode, ACaption);
  vTreeNode.ImageIndex := AImageIndex;
  vTreeNode.SelectedIndex := -1;

  Result := vTreeNode;

  if Assigned(vParentNode) then
    vParentNode.Expand(True);
end;

procedure TTreeViewArea.DoExecuteUIAction(const AView: TView);
begin
  if AView.Name = '#Refill' then
    Refill;
end;

{procedure TTreeViewArea.OnCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  NodeRect: TRect;
  vCanvas: TCanvas;
begin
  Exit;
  DefaultDraw := False;

  vCanvas := Sender.Canvas;

  NodeRect := Node.DisplayRect(True);
  vCanvas.FillRect(NodeRect);

  if cdsSelected in State then
  begin
    vCanvas.Font.Color := clBlack;
    vCanvas.Brush.Color := clWhite;
  end
  else
    vCanvas.Font.Color := clSilver;

  vCanvas.TextOut(NodeRect.Left, NodeRect.Top, Node.Text);
  NodeRect := Node.DisplayRect(False);

  NodeRect.Left := NodeRect.Left + (Node.Level * FTreeView.Indent);
  DrawButton(NodeRect, Node);
end;

procedure TTreeViewArea.DrawButton(ARect: TRect; Node: TTreeNode);
var
  cx, cy: Integer;
  vButtonSize: Integer;
begin
  vButtonSize := 5;
  cx := ARect.Left + FTreeView.Indent div 2;
  cy := ARect.Top + (ARect.Bottom - ARect.Top) div 2;
  with FTreeView.Canvas do
  begin
    Pen.Color := clGray;
    //draw horizontal line.
    if Node.HasChildren then
    begin
      PenPos := Point(cx + vButtonSize, cy);
      LineTo(ARect.Left + FTreeView.Indent - 1, cy);
    end
    else
    begin
      PenPos := Point(cx, cy);
      LineTo(ARect.Left + FTreeView.Indent - 1, cy);
    end;

    //draw half vertical line, top portion.
    PenPos := Point(cx, cy);
    LineTo(cx, ARect.Top-1);

   if ((Node.GetNextVisible <> nil) and (Node.GetNextVisible.Level = Node.Level)) or (Node.GetNextSibling <> nil) then
   //draw bottom portion of half vertical line.
   begin
     PenPos := Point(cx, cy);
     LineTo(cx, ARect.Bottom + 1);
   end;

   if Node.HasChildren then
   begin
     //Let"s try a circular button instead
      Rectangle(cx - vButtonSize, cy - vButtonSize, cx + vButtonSize, cy + vButtonSize);
    // Rectangle();
     //draw the horizontal indicator.
     PenPos := Point(cx - vButtonSize + 2, cy);
     LineTo(cx + vButtonSize - 2, cy);
     //draw the vertical indicator if the node is collapsed
     if not Node.Expanded then
     begin
       PenPos := Point(cx, cy - vButtonSize + 2);
       LineTo(cx, cy + vButtonSize - 2);
     end;
   end;
       //now connect vertical lines of higher level nodes.
    Node := Node.Parent;
    while Node <> nil do
    begin
     cx := cx - FTreeView.Indent;
     if Node.GetNextSibling <> nil then
     begin
       PenPos := Point(cx, ARect.Top);
       LineTo(cx, ARect.Bottom);
     end;
     Node := Node.Parent;
    end;
  end;
end;}

procedure TTreeViewArea.OnKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and Assigned(FTreeView.Selected) then
    SelectNode(FTreeView.Selected);
end;

procedure TTreeViewArea.OnMouseClick(Sender: TObject);
var
  vClickPos: TPoint;
  vNode: TTreeNode;
begin
  vClickPos :=  FTreeView.ScreenToClient(Mouse.CursorPos);
  vNode := FTreeView.GetNodeAt(vClickPos.X, vClickPos.Y);
  if Assigned(vNode) then
    SelectNode(vNode);
end;

procedure TTreeViewArea.Refill;
begin
  Clear;
  FTreeView.Items.Clear;
  ProcessChilds;
end;

procedure TTreeViewArea.SelectNode(const ANode: TTreeNode);
var
  vArea: TUIArea;
begin
  if not Assigned(ANode) then
    Exit;

  vArea := TUIArea(ANode.Data);
  if Assigned(vArea) then
    ProcessAreaClick(vArea);
end;

{ TButtonArea }

destructor TButtonArea.Destroy;
begin
  FreeAndNil(FTypeSelectionMenu);
  inherited Destroy;
end;

function TButtonArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vParams: TStrings;
  vButton: TcxButton;
  vActionDef: TDefinition;
  vDefinitions: TList<TDefinition>;
  i: Integer;
  vMenuItem: TMenuItem;
  vDefinition: TDefinition;
  vImageID: Integer;
  vImageSize: Integer;
  vComposition: string;
  vViewStyle: string;
  vOverriddenCaption, vOverriddenHint: string;
begin
  vParams := CreateDelimitedList(FParams, '&');

  vActionDef := TDefinition(FView.Definition);

  vImageSize := StrToIntDef(vParams.Values['ImageSize'], 16);
  vImageID := StrToIntDef(vParams.Values['ImageID'], vActionDef._ImageID);
  vComposition := Trim(vParams.Values['Composition']);
  vViewStyle := Trim(vParams.Values['ViewStyle']);
  vOverriddenCaption := Trim(vParams.Values['Caption']);
  vOverriddenHint := Trim(vParams.Values['Hint']);

  vButton := TcxButton.Create(nil);
  vButton.OptionsImage.Images := TDragImageList(TInteractor(Interactor).Images[vImageSize]);
  vImageID := GetImageID(vImageID);

  if (TPanel(ALayout.Control).BevelOuter = bvNone) and (TPanel(ALayout.Control).BevelInner = bvNone)
    and (TPanel(ALayout.Control).BevelKind = TBevelKind.bkNone)
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
      if TPanel(ALayout.Control).ShowHint then
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

  if (vActionDef.Name = 'Add') and Assigned(FView.ParentDomainObject) and (FView.ParentDomainObject is TEntityList) then
  begin
    vDefinitions := TEntityList(FView.ParentDomainObject).ContentDefinitions;
    if vDefinitions.Count > 1 then
    begin
      FTypeSelectionMenu := TPopupMenu.Create(nil);
      FTypeSelectionMenu.Images := TDragImageList(TInteractor(Interactor).Images[16]);
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
        FTypeSelectionMenu.Items.Add(vMenuItem);
      end;
      vButton.DropDownMenu := FTypeSelectionMenu;
      vButton.Kind := cxbkOfficeDropDown;
      vButton.OptionsImage.Margin := 4;
    end
    else begin
      vButton.OnClick := OnAreaClick;
      TWinControl(ALayout.Control).Width := TWinControl(ALayout.Control).Height;
    end;
  end
  else
    vButton.OnClick := OnAreaClick;

  AddParams(vParams);

  Result := vButton;
end;

procedure TButtonArea.RefillArea(const AKind: Word);
var
  vButton: TcxButton;
  vActionDef: TDefinition;
  vImageID: Integer;
begin
  if AKind <> dckContentTypeChanged then
  begin
    inherited RefillArea(AKind);
    Exit;
  end;

  vButton := TcxButton(FControl);

  vActionDef := TDefinition(FView.Definition);
  vImageID := GetImageID(vActionDef._ImageID);

  if (vButton.OptionsImage.Images.Count + 1 >= vImageID) and (vImageID > 0) then
    vButton.OptionsImage.ImageIndex := vImageID;

  vButton.Caption := GetTranslation(vActionDef);
  vButton.Hint := vButton.Caption;
end;

{ TLinkArea }

function TLinkArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vParams: TStrings;
  vLabel: TcxLabel;
  vActionDef: TDefinition;
  vComposition: string;
  vViewStyle: string;
  vOverriddenCaption, vOverriddenHint: string;
begin
  vParams := CreateDelimitedList(FParams, '&');

  vActionDef := TDefinition(FView.Definition);

  vComposition := Trim(vParams.Values['Composition']);
  vViewStyle := Trim(vParams.Values['ViewStyle']);
  vOverriddenCaption := Trim(vParams.Values['Caption']);
  vOverriddenHint := Trim(vParams.Values['Hint']);

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
  vLabel.Style.TextColor := TPanel(ALayout.Control).Font.Color;
  //vLabel.Style.TextStyle := [fsUnderline];
  vLabel.Style.HotTrack := True;
  //vLabel.StyleHot.TextColor := clBlue;
  vLabel.StyleHot.TextStyle := [fsUnderline];
  vLabel.OnClick := OnAreaClick;

  AddParams(vParams);

  Result := vLabel;
end;

procedure TLinkArea.RefillArea(const AKind: Word);
var
  vLabel: TcxLabel;
  vActionDef: TDefinition;
begin
  if AKind <> dckContentTypeChanged then
  begin
    inherited RefillArea(AKind);
    Exit;
  end;

  vLabel := TcxLabel(FControl);

  vActionDef := TDefinition(FView.Definition);
  vLabel.Caption := GetTranslation(vActionDef);
  vLabel.Hint := vLabel.Caption;
end;

{ TActionArea }

constructor TActionArea.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean;
  const AControl: TObject; const ALayout: TLayout; const AParams: string);
begin
  FParams := AParams;
  inherited Create(AParent, AView, AId, AIsService, AControl, ALayout, AParams);
end;

{ TVCLControl }

function TVCLControl.AreaFromSender(const ASender: TObject): TUIArea;
begin
  if Assigned(ASender) and (ASender is TComponent) then
    Result := TUIArea(TComponent(ASender).Tag)
  else
    Result := nil;
end;

constructor TVCLControl.Create(const AOwner: TUIArea; const AControl: TObject);
begin
  inherited Create(AOwner, AControl);
  FIsForm := AControl is TForm;
  FIsAutoReleased := AControl is TMenuItem;
end;

procedure TVCLControl.DoActivate(const AUrlParams: string);
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
      TcxPageControl(TControl(FControl).Parent).Properties.ActivePage := TcxTabSheet(FControl);
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

procedure TVCLControl.DoBeginUpdate;
begin
  if (not FIsForm) and (FControl is TWinControl) then
    LockControl(TWinControl(FControl), True);

  if FIsForm and (TInteractor(FInteractor).Layout = 'mdi') then
    SendMessage(Application.MainForm.ClientHandle, WM_SETREDRAW, 0, 0);
end;

procedure TVCLControl.DoClose(const AModalResult: Integer);
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

procedure TVCLControl.DoEndUpdate;
begin
  if (not FIsForm) and (FControl is TWinControl) then
    LockControl(TWinControl(FControl), False);

  if FIsForm and (TInteractor(FInteractor).Layout = 'mdi') then
  begin
    SendMessage(Application.MainForm.ClientHandle, WM_SETREDRAW, 1, 0);
    RedrawWindow(Application.MainForm.ClientHandle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);
  end;
end;

function TVCLControl.DoGetDescription: string;
begin
  Result := inherited DoGetDescription;
  if Assigned(FControl) and (FControl is TControl) then
  begin
    if not TControl(FControl).Visible then
      Result := Result + ' HID';
    if not TControl(FControl).Enabled then
      Result := Result + ' DIS';
  end;
end;

function TVCLControl.GetName: string;
begin
  if not Assigned(FControl) then
    Result := 'NULL'
  else if not (FControl is TControl) then
    Result := FControl.ClassName + ': ' + FOwner.Id
  else if TCrackedWinControl(FControl).Caption <> '' then
    Result := FControl.ClassName + ': ' + FOwner.Id + ' (' + TCrackedWinControl(FControl).Caption + ')'
  else
    Result := FControl.ClassName + ': ' + FOwner.Id;
end;

procedure TVCLControl.PlaceIntoBounds(const ALeft, ATop, AWidth, AHeight: Integer);
var
  vLeft, vTop, vWidth, vHeight: Integer;
begin
  if FControl is TControl then
  begin
    vLeft := IfThen(ALeft < 0, TControl(FControl).Left, ALeft);
    vTop := IfThen(ATop < 0, TControl(FControl).Top, ATop);
    vWidth := IfThen(AWidth < 0, TControl(FControl).Width, AWidth);
    vHeight := IfThen(AHeight < 0, TControl(FControl).Height, AHeight);

    TControl(FControl).SetBounds(vLeft, vTop, vWidth, vHeight);
  end;

  TVCLArea(FOwner).PlaceLabel;
end;

procedure TVCLControl.SetControl(const AControl: TObject);
begin
  if not (AControl is TComponent) then
  begin
    if AControl is TTreeNode then
    begin
      if Assigned(FControl) then
        TTreeNode(FControl).Data := nil;

      inherited SetControl(AControl);

      if Assigned(FControl) then
        TTreeNode(FControl).Data := FOwner;

      FIsAutoReleased := True;
    end;

    Exit;
  end;

  if Assigned(FControl) then
  begin
    TCrackedWinControl(FControl).Tag := 0;
    if FControl is TWinControl then
    begin
      TCrackedWinControl(FControl).OnEnter := nil;
      TCrackedWinControl(FControl).OnExit := nil;
    end;
  end;

  inherited SetControl(AControl);

  if Assigned(FControl) then
  begin
    TCrackedWinControl(FControl).Tag := Integer(FOwner);
    if FControl is TWinControl then
    begin
      TCrackedWinControl(FControl).OnEnter := FOwner.OnEnter;
      TCrackedWinControl(FControl).OnExit := FOwner.OnExit;
    end;
  end;

  FIsForm := FControl is TForm;
  FIsAutoReleased := FControl is TMenuItem;
end;

procedure TVCLControl.SetParent(const AParent: TUIArea);
begin
  inherited SetParent(AParent);

  if not (FControl is TControl) then
    Exit;

  // Установка родителя для контрола
  if FIsForm or not Assigned(AParent) then
    TControl(FControl).Parent := nil
  else if not Assigned(TControl(FControl).Parent) then
    TControl(FControl).Parent := TWinControl(TVCLArea(AParent).InnerControl);
end;

procedure TVCLControl.SetViewState(const AValue: TViewState);
var
  vBoolValue: Boolean;
begin
  if FControl is TMenuItem then
  begin
    TMenuItem(FControl).Visible := AValue > vsHidden;
    TMenuItem(FControl).Enabled := AValue > vsDisabled;
  end
  else if (FControl is TControl) and (not FIsForm) then
  begin
    vBoolValue := AValue > vsHidden;

    if TControl(FControl).Visible <> vBoolValue then
      TControl(FControl).Visible := vBoolValue;

    vBoolValue := AValue > vsDisabled;
    if FControl is TcxTabSheet then
      TcxTabSheet(FControl).AllowCloseButton := vBoolValue
    else if TControl(FControl).Enabled <> vBoolValue then
      TControl(FControl).Enabled := vBoolValue;
  end;
end;

initialization

RegisterClasses([TLabel, TPanel, TSplitter, TImage, TBevel, TPageControl, TMemo,
  TTabSheet, TToolBar, TToolButton, TBitBtn, TScrollBox, TMainMenu, TPopupMenu,
  TShape, TListView, TImageList]);

TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, '', TTreeViewArea);
TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, 'TreeView', TTreeViewArea);
TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, 'NavBar', TNavBarArea);
TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, 'MainMenu', TMainMenuArea);
TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, 'ToolBar', TToolBarArea);
//TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, 'OneButton', TOneButtonArea);

TPresenter.RegisterUIClass('Windows.DevExpress', uiAction, '', TButtonArea);
TPresenter.RegisterUIClass('Windows.DevExpress', uiNavigation, 'link', TLinkArea);

end.
