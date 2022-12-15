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

unit vclListEditors;

interface

uses
  Generics.Collections, Graphics, Classes, Controls, Types, CheckLst, ExtCtrls, ComCtrls,

  uEntity, uEntityList, vclArea, uView, uUIBuilder, uLayout, uDefinition, uConsts,

  cxCheckListBox, cxLookAndFeelPainters, dxColorEdit,
  cxTL, cxGrid, cxGridTableView, cxCustomData, cxGridLevel, cxGridCustomTableView, cxGridChartView, cxStyles,
  cxVGrid, cxCalendar, cxCheckBox, cxGraphics, cxControls, cxLookAndFeels, cxContainer,
  cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxSpinEdit, cxInplaceContainer, cxCustomPivotGrid, cxPivotGrid,
  cxExportPivotGridLink, cxTLData, cxButtonEdit, cxEditRepositoryItems, cxPC;

type
  TEntityListSelector = class (TVCLFieldArea)
  private
    FListBox: TCheckListBox;
    FEntityList: TEntityList;
    procedure OnWinControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    procedure DoOnChange; override;
  end;

  TEntityListSelector2 = class(TVCLFieldArea)
  private
    FListBox: TcxCheckListBox;
    FEntityList: TEntityList;
    procedure OnClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    procedure DoOnChange; override;
  end;

  TEntityListSelector3 = class(TVCLFieldArea)
  private
    FListBox: TcxCheckListBox;
    FLookupCollection: string;
    FLookupField: string;
    FMasterList: TEntityList;
    FDestroing: Boolean;
    function FindMasterItem(const ALookupItem: TEntity): TEntity;
    procedure OnClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
    procedure FillLookupList;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    procedure DoOnChange; override;
    procedure DoDisableContent; override;
  end;

  TPagedEntityListSelector = class(TVCLFieldArea)
  private
    FPages: TcxPageControl;
    FEntityList: TEntityList;
    FInUpdate: Boolean;
    FCreatedViews: TList<TView>;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    procedure DoOnChange; override;
  end;

  TEntityListSelectorMTM = class(TVCLFieldArea) // many to many link
  private
    FListBox: TcxCheckListBox;
    FEntityList: TEntityList;
    FTransitFieldDef: TObjectFieldDef;
    FTransitDefinition: TDefinition;
    FFilterName: string;
    procedure FillList;
    procedure OnClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    procedure DoOnChange; override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
  end;

  TParametersEditor = class(TVCLFieldArea)
  private
    FGrid: TcxVerticalGrid;
    FParams: TList<TEntity>;
    procedure ClearRows;
    procedure AddRow(const AName: string; const AValue: Variant; const AType: TEntity);
    procedure OnValueChanged(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
  end;

  TColumnBinding = class
  private
    FFieldName: string;
    FSanitizedFieldName: string;
    FFieldDef: TFieldDef;
    FAfterPoint: Integer; // знаков после запятой для Float (здесь для оптимизации)
    FColumn: TcxGridColumn;
  public
    constructor Create(const AFieldName: string; const AFieldDef: TFieldDef);

    property FieldName: string read FFieldName;
    property SanitizedFieldName: string read FSanitizedFieldName;
    property FieldDef: TFieldDef read FFieldDef;
    property AfterPoint: Integer read FAfterPoint;
  end;

  TUserDataSource = class(TcxCustomDataSource)
  private
    FEntities: TList<TEntity>;
    FColumns: TObjectList<TColumnBinding>;
    FIsLoading: Boolean;
    FEditor: TUIArea;
  protected
    function AppendRecord: TcxDataRecordHandle; override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    function GetItemHandle(AItemIndex: Integer): TcxDataItemHandle; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    function IsNativeCompare: Boolean; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  public
    constructor Create(const AEditor: TUIArea);
    destructor Destroy; override;

    procedure DataChanged; override;
    function GetRecordHandleByIndex(ARecordIndex: Integer): TcxDataRecordHandle; override;

    function AddColumn(const AFieldName: string; const AFieldDef: TFieldDef): TColumnBinding;

    procedure Add(const AEntity: TEntity; const AUpdateSelection: Boolean);
    procedure Remove(const AEntity: TEntity; const AUpdateSelection: Boolean);
    procedure Update;
    procedure FocusRow(const ARowIndex: Integer);

    property Data: TList<TEntity> read FEntities;
    property IsLoading: Boolean read FIsLoading;
  end;

  TCollectionEditor = class(TUIArea)
  private
    FGrid: TcxGrid;
    FMasterTableView: TcxGridTableView;
    FBGStyle: TcxStyle;
    FHeaderStyle: TcxStyle;
    FMasterDS: TUserDataSource;
    FAllData: TEntityList;
    FFilterText: string;
    FFilterDateFrom, FFilterDateTo: TDateTime;
    FFilterDateActive: Boolean;
    FLayoutExists: Boolean;
    FGroupFieldName: string;
    FColorFieldName: string;
    FEditRepository: TcxEditRepository;
    procedure DoOnCompare(ADataController: TcxCustomDataController; ARecordIndex1, ARecordIndex2, AItemIndex: Integer;
      const V1, V2: Variant; var Compare: Integer);
    procedure DoOnCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
      AShift: TShiftState; var AHandled: Boolean);
    procedure DoOnSelectionChanged(Sender: TcxCustomGridTableView);
    procedure DoCanSelectRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; var AAllow: Boolean);
    procedure CreateColumnsFromModel(const AFields: string = '');
    procedure EnableColumnParamsChangeHandlers(const AEnable: Boolean);
    procedure ShowFooter(const AColumn: TcxGridColumn; const AAgg: TAggregationKind);
    procedure CreateColumn(const AFieldDef: TFieldDef; const AFieldName: string; const AOverriddenCaption: string;
      const AWidth: Integer; const ASortOrder: TSortOrder; const AAgg: TAggregationKind);
    procedure OnGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
    procedure BeforeContextMenuShow(Sender: TObject);
    procedure SaveColumnWidths;
    procedure DoOnColumnPosChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
    procedure DoOnColumnSizeChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
    procedure DoOnHeaderClick(Sender: TObject);
    procedure LoadColumnWidths;
    function FindColumnByFieldName(const AFieldName: string): TcxGridColumn;
    function IsMatchToFilter(const AEntity: TEntity): Boolean;
    function GetSearchType: TSearchType;
    procedure ExportToExcel;
    procedure CreateColumnForAction(const AActionDef: TActionDef; const AFieldName: string);
    procedure CreateBtnPropertiesForAction(const AActionDef: TActionDef);
    procedure OnActionGetProperties(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
    procedure ExportToCsv;
  protected
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure DoExecuteUIAction(const AView: TView); override;
    procedure SetPopupArea(const APopupArea: TUIArea); override;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = ''); override;
    destructor Destroy; override;
  end;

  TColumnListEditor = class (TVCLFieldArea)
  private
    FGrid: TcxGrid;
    FMasterTableView: TcxGridTableView;
    FBGStyle: TcxStyle;
    FHeaderStyle: TcxStyle;
    FContentStyle: TcxStyle;
    FSelectionStyle: TcxStyle;
    FMasterDS: TUserDataSource;
    FAllData: TEntityList;
    FLayoutExists: Boolean;
    FEditRepository: TcxEditRepository;
    procedure DoOnCompare(ADataController: TcxCustomDataController; ARecordIndex1, ARecordIndex2, AItemIndex: Integer;
      const V1, V2: Variant; var Compare: Integer);
    procedure DoOnCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
      AShift: TShiftState; var AHandled: Boolean);
    procedure DoOnFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord,
      AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
    procedure CreateColumnsFromModel(const AFields: string = '');
    procedure EnableColumnParamsChangeHandlers(const AEnable: Boolean);
    procedure ShowFooter(const AColumn: TcxGridColumn; const AAgg: TAggregationKind);
    procedure CreateColumn(const AFieldDef: TFieldDef; const AFieldName: string; const AOverriddenCaption: string;
      const AWidth: Integer; const ASortOrder: TSortOrder; const AAgg: TAggregationKind);
    procedure OnGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
    procedure OnExitFromInplaceEditor(Sender: TObject);
    procedure OnInitEdit(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit);

    procedure BeforeContextMenuShow(Sender: TObject);
    procedure SaveColumnWidths;
    procedure DoOnColumnPosChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
    procedure DoOnColumnSizeChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
    procedure DoOnHeaderClick(Sender: TObject);
    procedure LoadColumnWidths;
    procedure OnDrawEnumItem_LineStyle(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect;
      AState: TOwnerDrawState);
    procedure GetPropertiesForEdit(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure SetPopupArea(const APopupArea: TUIArea); override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
  public
    destructor Destroy; override;
  end;

  TListEditor = class (TVCLFieldArea)
  private
    FList: TcxTreeList;
    procedure OnSelectionChanged(Sender: TObject);
    procedure OnDblClick(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
  end;

  TPivotDataSource = class(TcxCustomDataSource)
  private
    FEntities: TList<TEntity>;
    FColumns: TObjectList<TColumnBinding>;
  protected
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant; override;
  public
    constructor Create;
    destructor Destroy; override;

    function AddColumn(const AFieldName: string; const AFieldDef: TFieldDef): TColumnBinding;

    property RecordCount: Integer read GetRecordCount;
    property Data: TList<TEntity> read FEntities;
  end;

  TPivotGrid = class (TUIArea)
  private
    FPivot: TcxPivotGrid;
    FCube: TDataCube;
    FAllData: TEntityList;
    FMasterDS: TPivotDataSource;
    function CreatePivotField(const AField: TCubeField; const AWidth: Integer = 150): TcxPivotGridField;
    procedure FillGrid;
    procedure BeforeContextMenuShow(Sender: TObject);
    procedure DoOnGridDblClick(Sender: TObject);
    procedure ReloadData(const AFilter: string);
    procedure ExportToExcel;
  protected
    procedure DoExecuteUIAction(const AView: TView); override;
    procedure SetPopupArea(const APopupArea: TUIArea); override;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = ''); override;
    destructor Destroy; override;
  end;

  TTreeLevel = class
    Def: TDefinition;
    ListFieldName: string;
    PaintListFieldNodes: Boolean;
  public
    constructor Create(const AListFieldName: string);
    destructor Destroy; override;
  end;

  TTreeCollectionEditor = class (TUIArea)
  private
    FTreeList: TcxVirtualTreeList;
    FBGStyle: TcxStyle;
    FHeaderStyle: TcxStyle;
    FAllData: TEntityList;
//    FFilterText: string;
//    FFilterDateFrom, FFilterDateTo: TDateTime;
//    FFilterDateActive: Boolean;
    FDomain: TObject;
    FLevels: TObjectList<TTreeLevel>;
//    procedure DoOnCompare(ADataController: TcxCustomDataController; ARecordIndex1, ARecordIndex2, AItemIndex: Integer;
//      const V1, V2: Variant; var Compare: Integer);
    procedure DoOnTableViewDblClick(Sender: TObject);
    procedure DoOnSelectionChanged(Sender: TObject);
    procedure DoOnSorted(Sender: TObject);
//    procedure DoCanSelectRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; var AAllow: Boolean);
    procedure CreateColumnsFromModel(const AFields: string = '');
    procedure EnableColumnParamsChangeHandlers(const AEnable: Boolean);
//    procedure ShowFooter(const AColumn: TcxGridColumn; const AAgg: TAggregationKind);
    procedure CreateColumn(const AFieldDef: TFieldDef; const AFieldName: string; const AOverriddenCaption: string;
      const AWidth: Integer; const ASortOrder: TSortOrder; const AAgg: TAggregationKind);
//    procedure OnGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
//      AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
    procedure BeforeContextMenuShow(Sender: TObject);
    procedure SaveColumnWidths;
    procedure DoOnColumnPosChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
    procedure DoOnColumnSizeChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
//    procedure DoOnHeaderClick(Sender: TObject);
    procedure LoadColumnWidths;
//    function FindColumnByFieldName(const AFieldName: string): TcxTreeListColumn;
//    function IsMatchToFilter(const AEntity: TEntity): Boolean;
//    function GetSearchType: TSearchType;
//    procedure TreeListExpanding(Sender: TcxCustomTreeList;
//      ANode: TcxTreeListNode; var Allow: Boolean);
//    procedure FullExpand1Click(Sender: TObject);
    procedure TreeListGetChildCount(Sender: TcxCustomTreeList; AParentNode: TcxTreeListNode; var ACount: Integer);
    procedure TreeListGetNodeValue(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AColumn: TcxTreeListColumn; var AValue: Variant);
    procedure CreateLevels(const ALayout: TLayout);
    function GetLevel(const AIndex: Integer): TTreeLevel;
    function GetEntityCount(const ANode: TcxTreeListNode): Integer;
    function GetEntity(const ANode: TcxTreeListNode; const ACurrLevel: Integer): TEntity;
  protected
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure DoExecuteUIAction(const AView: TView); override;
    procedure SetPopupArea(const APopupArea: TUIArea); override;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
      const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = ''); override;
    destructor Destroy; override;
  end;

implementation

uses
  TypInfo, Windows, SysUtils, Messages, Math, Variants, DateUtils, Dialogs, Menus, ShellApi, IOUtils, StrUtils,

  uWinVCLPresenter, uObjectField, uInteractor, uEnumeration, uSession, uChangeManager,
  uConfiguration, uDomain, uQueryDef, uQuery, uUtils, uPresenter, uSettings, uCollection,

  dxCore, cxGridStrs, cxPivotGridStrs, cxDataStorage, cxGridCustomView, cxImageComboBox, cxDateUtils,
  cxGridExportLink, cxProgressBar, dxColorGallery;

type
  TCalculateStyleFunc = function(const AViewName: string; const AEntity: TEntity): TColor of object;

const
  cColWidthDelim = ',';

function GetRelColor(const ASourceColor: TColor;
  const ARelRed, ARelGreen, ARelBlue: integer): TColor;
var
  vR, vG, vB: integer;
begin
  Result := ColorToRGB(ASourceColor);

  vR := GetRValue(Result) + ARelRed;
  vG := GetGValue(Result) + ARelGreen;
  vB := GetBValue(Result) + ARelBlue;

  if vR > 255 then
    vR := 255
  else if vR < 0 then
    vR := 0;

  if vG > 255 then
    vG := 255
  else if vG < 0 then
    vG := 0;

  if vB > 255 then
    vB := 255
  else if vB < 0 then
    vB := 0;

  Result := RGB(vR, vG, vB);
end;

function GetBGColor(const ADefinition: TDefinition; const ASourceColor: TColor): TColor;
var
  vBG: TBackground;
begin
  Result := ColorToRGB(ASourceColor);
  if ADefinition = nil then Exit;

  vBG := ADefinition.Background;
  if vBG.Color <> cNullColor then
    Result := ColorToRGB(vBG.Color);
  Result := GetRelColor(Result, vBG.RelativeRed, vBG.RelativeGreen, vBG.RelativeBlue) and $FFFFFF;
end;

function CompareEntities(const AEnt1, AEnt2: TEntity; const AFieldDef: TFieldDef; const AFieldName: string): Integer;
var
  vValue1, vValue2: Variant;
begin
  if Assigned(AEnt1) <> Assigned(AEnt2) then
  begin
    if Assigned(AEnt1) then
      Result := 1
    else
      Result := -1;
  end
  else if not Assigned(AEnt1) then
    Result := 0
  else if AEnt1.IsService = AEnt2.IsService then
  begin
    if (AFieldDef = nil) or ((AFieldDef.Definition.Kind = clkMixin) and (AEnt1.FieldByName(AFieldDef.Name).FieldDef.Kind <> AEnt2.FieldByName(AFieldDef.Name).FieldDef.Kind)) then
    begin
      Result := CompareStr(AEnt1.DisplayName, AEnt2.DisplayName)
    end
    else if AFieldDef.Kind = fkObject then
      Result := CompareEntities(AEnt1.ExtractEntity(AFieldName), AEnt2.ExtractEntity(AFieldName), nil, '')
    else if AFieldDef.Kind in [fkString..fkCurrency] then
    begin
      Result := 0;
      vValue1 := AEnt1.ExtractFieldValue(AFieldName);
      vValue2 := AEnt2.ExtractFieldValue(AFieldName);
      if VarIsNull(vValue1) then
        Result := -1;
      if VarIsNull(vValue2) then
      begin
        Result := Result + 1;
        Exit;
      end
      else if Result < 0 then
        Exit;

      if AFieldDef.Kind = fkString then
        Result := CompareStr(vValue1, vValue2)
      else if AFieldDef.Kind = fkDateTime then
        Result := CompareDateTime(vValue1, vValue2)
      else if AFieldDef.Kind = fkBoolean then
        Result := Integer(vValue1) - Integer(vValue2)
      else
        Result := CompareValue(vValue1, vValue2);
    end
    else
      Result := 0;
  end
  else begin
    if AEnt1.IsService then
      Result := -1
    else
      Result := 1;
  end;
end;

procedure OnTableViewDblClick(const AView: TView; const AArea: TUIArea);
var
  vDblClickView: TView;
begin
  vDblClickView := AView.BuildView('Selected/#HandleDblClick');
  try
    AArea.ExecuteUIAction(vDblClickView);
  finally
    vDblClickView.CleanView;
  end;
  //vDblClickView.ExecuteAction(AArea.Holder);
end;

procedure GetContentStyle(const AArea: TUIArea; const AList: TEntityList; const AColorFieldName: string;
  const AFilteredList: TList<TEntity>; Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
var
  vEntity: TEntity;
  vDefinition: TDefinition;
  vColorField: TBaseField;
  vState: TState;
  vColor: TColor;
  vStyle: TcxStyle;
begin
  vStyle := TcxStyle(TWinVCLPresenter(AArea.Presenter).RowStyle);
  vStyle.Color := clDefault;
  vStyle.TextColor := clDefault;

  vEntity := AFilteredList[ARecord.RecordIndex];
  if Assigned(vEntity) then
  begin
    vDefinition := AList.MainDefinition;
    if LowerCase(AColorFieldName) = 'script' then
    begin
      vColor := TCalculateStyleFunc(TConfiguration(vDefinition.Configuration).CalculateStyleFunc)(AArea.View.InitialName, vEntity);
      if vColor = cNullColor then
      begin
        if vStyle.Color <> clDefault then
        begin
          vColor := GetBGColor(vDefinition, vStyle.Color);
          vStyle.Color := vColor;
        end;
      end
      else
        vStyle.Color := vColor;
    end
    else if vDefinition.ColorTarget <> ctNone then
    begin
      vColor := cNullColor;
      vColorField := vEntity.FieldByName(AColorFieldName);
      if vColorField.FieldKind = fkColor then
      begin
        if Assigned(vColorField) then
          vColor := vColorField.Value;
      end
      else if vColorField.FieldKind = fkEnum then
      begin
        vState := vEntity.EntityState;
        if Assigned(vState) then
          vColor := vState.Color;
      end;

      if vColor <> cNullColor then
      begin
        if vDefinition.ColorTarget = ctBackground then
          vStyle.Color := vColor
        else if vDefinition.ColorTarget = ctText then
          vStyle.TextColor := vColor;
      end;
    end;

    vColor := cNullColor;
    if vEntity.IsService then
      vColor := TDomain(AArea.Domain).Constant['ServiceRecordColor']//clNavy
    else if vEntity.IsNew then
      vColor := TDomain(AArea.Domain).Constant['NewRecordColor']
    else if (AItem.Name = 'TotalAmount') and (vEntity['TotalAmount'] < 0) then
      vColor := clRed;

    if vColor <> cNullColor then
      vStyle.TextColor := vColor;

    if (vDefinition.ColorTarget <> ctBackground) and (vStyle.Color <> clDefault) then
      vStyle.Color := GetBGColor(vDefinition, vStyle.Color);
  end;

  AStyle := vStyle;
end;

procedure SetGridColumnParams(const AMasterTableView: TcxGridTableView; const AColName: string; const AWidth,
  AColIndex, ARowIndex: Integer; const AVisible: Boolean; const ASortOrder: TSortOrder);
var
  vCol: TcxGridColumn;
  function GetColumnByName: TcxGridColumn;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to AMasterTableView.ColumnCount - 1 do
      if AMasterTableView.Columns[i].Name = AColName then
      begin
        Result := AMasterTableView.Columns[i];
        Break;
      end;
  end;
begin
  vCol := GetColumnByName;
  if vCol = nil then Exit;

  vCol.Width := AWidth;
  vCol.Index := AColIndex;
  vCol.Visible := AVisible;
  vCol.SortOrder := TcxDataSortOrder(ASortOrder);
end;

function LoadGridColumnWidths(const ADomain: TObject; const AMasterTableView: TcxGridTableView; const AObjectName: string): Boolean;
var
  vSectionName: string;
  vSettings: TSettings;
  vCols: TStringList;
  i, vColIndex, vRowIndex: Integer;
  vSortOrder: TSortOrder;
  vValues: TStrings;
  vPath: string;
  vConfiguration: TConfiguration;
  vWidth: Integer;
  vVisible: Boolean;
  vOnSortingChanged: TNotifyEvent;
begin
  vSettings := TDomain(ADomain).UserSettings;
  vSectionName := 'Columns$' + AObjectName;

  Result := vSettings.SectionExists(vSectionName);

  if not Result then
  begin
    // Совместимость со старым кодом
    vConfiguration := TDomain(ADomain).Configuration;
    vPath := vConfiguration.FindLayoutFile(AObjectName, '.col');
    if not FileExists(vPath) then
      Exit;

    vCols := TStringList.Create;
    vCols.LoadFromFile(vPath);
    for i := 0 to vCols.Count - 1 do
      vSettings.SetValue(vSectionName, vCols.Names[i], vCols.ValueFromIndex[i]);
  end
  else begin
    vCols := TStringList.Create;
    vSettings.ReadSection(vSectionName, vCols);
  end;

  vValues := CreateDelimitedList('', cColWidthDelim);
  vOnSortingChanged := AMasterTableView.DataController.OnSortingChanged;
  AMasterTableView.DataController.OnSortingChanged := nil;
  AMasterTableView.BeginUpdate;
  try
    for i := 0 to vCols.Count - 1 do
    begin
      vValues.DelimitedText := vCols.ValueFromIndex[i];
      vWidth := 50; vColIndex := 0; vRowIndex := 0; vSortOrder := uConsts.soNone; vVisible := True;
      if vValues.Count > 0 then
        vWidth := StrToIntDef(vValues[0], 50);
      if vValues.Count > 1 then
        vColIndex := StrToIntDef(vValues[1], 0);
      if vValues.Count > 2 then
        vRowIndex := StrToIntDef(vValues[2], 0);
      if vValues.Count > 3 then
        vVisible := StrToBoolDef(vValues[3], True);
      if vValues.Count > 4 then
        vSortOrder := TSortOrder(StrToIntDef(vValues[4], 0));

      SetGridColumnParams(AMasterTableView, vCols.Names[i], vWidth, vColIndex, vRowIndex, vVisible, vSortOrder);
    end;
  finally
    AMasterTableView.EndUpdate;
    AMasterTableView.DataController.OnSortingChanged := vOnSortingChanged;
    FreeAndNil(vCols);
    FreeAndNil(vValues)
  end;
end;

procedure SaveGridColumnWidths(const ADomain: TObject; const AMasterTableView: TcxGridTableView; const AObjectName: string);
var
  vSectionName: string;
  vSettings: TSettings;
  vColumn: TcxGridColumn;
  vValues: string;
  i: Integer;
begin
  vSettings := TDomain(ADomain).UserSettings;
  vSectionName := 'Columns$' + AObjectName;
  for i := 0 to AMasterTableView.ColumnCount - 1 do
  begin
    vColumn := AMasterTableView.Columns[i];
    vValues := IntToStr(AMasterTableView.Columns[i].Width) + cColWidthDelim +
      IntToStr(AMasterTableView.Columns[i].Index) + cColWidthDelim +
      IntToStr(0) + cColWidthDelim +
      BoolToStr(AMasterTableView.Columns[i].Visible) + cColWidthDelim +
      IntToStr(Integer(AMasterTableView.Columns[i].SortOrder));
    vSettings.SetValue(vSectionName, vColumn.Name, vValues);
  end;
end;

procedure SetTreeColumnParams(const ATreeList: TcxCustomTreeList; const AColName: string; const AWidth,
  AColIndex, ARowIndex: Integer; const AVisible: Boolean; const ASortOrder: TSortOrder);
var
  vCol: TcxTreeListColumn;
  function GetColumnByName: TcxTreeListColumn;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to ATreeList.ColumnCount - 1 do
      if ATreeList.Columns[i].Name = AColName then
      begin
        Result := ATreeList.Columns[i];
        Break;
      end;
  end;
begin
  vCol := GetColumnByName;
  if vCol = nil then Exit;

  vCol.Width := AWidth;
  vCol.ItemIndex := AColIndex;
  vCol.Visible := AVisible;
  vCol.SortOrder := TcxDataSortOrder(ASortOrder);
end;

procedure LoadTreeColumnWidths(const ADomain: TObject; const ATreeList: TcxCustomTreeList; const AObjectName: string);
var
  vSectionName: string;
  vSettings: TSettings;
  vCols: TStringList;
  i, vColIndex, vRowIndex: Integer;
  vSortOrder: TSortOrder;
  vValues: TStrings;
  vPath: string;
  vConfiguration: TConfiguration;
  vWidth: Integer;
  vVisible: Boolean;
//  vOnSortingChanged: TNotifyEvent;
begin
  vSettings := TDomain(ADomain).UserSettings;
  vSectionName := 'Columns$' + AObjectName;

  if not vSettings.SectionExists(vSectionName) then
  begin
    // Совместимость со старым кодом
    vConfiguration := TDomain(ADomain).Configuration;
    vPath := vConfiguration.FindLayoutFile(AObjectName, '.col');
    if not FileExists(vPath) then
      Exit;

    vCols := TStringList.Create;
    vCols.LoadFromFile(vPath);
    for i := 0 to vCols.Count - 1 do
      vSettings.SetValue(vSectionName, vCols.Names[i], vCols.ValueFromIndex[i]);
  end
  else begin
    vCols := TStringList.Create;
    vSettings.ReadSection(vSectionName, vCols);
  end;

  vValues := CreateDelimitedList('', cColWidthDelim);
//  vOnSortingChanged := ATreeList.OnSorted;
//  ATreeList.OnSorted := nil;
  ATreeList.BeginUpdate;
  try
    for i := 0 to vCols.Count - 1 do
    begin
      vValues.DelimitedText := vCols.ValueFromIndex[i];
      vWidth := 50; vColIndex := 0; vRowIndex := 0; vSortOrder := uConsts.soNone; vVisible := True;
      if vValues.Count > 0 then
        vWidth := StrToIntDef(vValues[0], 50);
      if vValues.Count > 1 then
        vColIndex := StrToIntDef(vValues[1], 0);
      if vValues.Count > 2 then
        vRowIndex := StrToIntDef(vValues[2], 0);
      if vValues.Count > 3 then
        vVisible := StrToBoolDef(vValues[3], True);
      if vValues.Count > 4 then
        vSortOrder := TSortOrder(StrToIntDef(vValues[4], 0));

      SetTreeColumnParams(ATreeList, vCols.Names[i], vWidth, vColIndex, vRowIndex, vVisible, vSortOrder);
    end;
  finally
    ATreeList.EndUpdate;
//    ATreeList.OnSorted := vOnSortingChanged;
    FreeAndNil(vCols);
    FreeAndNil(vValues)
  end;
end;

procedure SaveTreeColumnWidths(const ADomain: TObject; const ATreeList: TcxCustomTreeList; const AObjectName: string);
var
  vSectionName: string;
  vSettings: TSettings;
  vColumn: TcxTreeListColumn;
  vValues: string;
  i: Integer;
begin
  vSettings := TDomain(ADomain).UserSettings;
  vSectionName := 'Columns$' + AObjectName;
  for i := 0 to ATreeList.ColumnCount - 1 do
  begin
    vColumn := ATreeList.Columns[i];
    vValues := IntToStr(ATreeList.Columns[i].Width) + cColWidthDelim +
      IntToStr(ATreeList.Columns[i].ItemIndex) + cColWidthDelim +
      IntToStr(0) + cColWidthDelim +
      BoolToStr(ATreeList.Columns[i].Visible) + cColWidthDelim +
      IntToStr(Integer(ATreeList.Columns[i].SortOrder));
    vSettings.SetValue(vSectionName, vColumn.Name, vValues);
  end;
end;

procedure FillEnumList(const ADomain: TDomain; const AItems: TStrings; const AColumn: TcxGridColumn);
var
  vEnumItem: TEnumItem;
  vEnum: TEnumeration;
  vFieldDef: TFieldDef;
begin
  // TODO Обработать то, что привязка может быть некорректной
  vFieldDef := TColumnBinding(AColumn.DataBinding.Data).FieldDef;
  vEnum := ADomain.Configuration.Enumerations.ObjectByName(TSimpleFieldDef(vFieldDef).Dictionary);
  if not Assigned(vEnum) then
    vEnum := ADomain.Configuration.StateMachines.ObjectByName(TSimpleFieldDef(vFieldDef).Dictionary);

  AItems.BeginUpdate;
  try
    AItems.Clear;
    for vEnumItem in vEnum do
    begin
      if not vFieldDef.HasFlag(cRequired) or (vEnumItem.ID > 0) then
        AItems.Add(vEnumItem.DisplayText);
    end;
  finally
    AItems.EndUpdate;
  end;
end;

{ TEntityListSelector }

procedure TEntityListSelector.DoBeforeFreeControl;
begin
  inherited;
  //FControl.Control := nil;
  //FreeAndNil(FListBox);
end;

function TEntityListSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FListBox := TCheckListBox.Create(nil);
  Result := FListBox;

  FEntityList := nil;
  FListBox.OnKeyDown := OnWinControlKeyDown;
end;

procedure TEntityListSelector.DoOnChange;
var
  i: Integer;
  vList: TListField;
  vHolder: TObject;
begin
  vList := FView.ExtractListField;
  if not Assigned(vList) then
    Exit;

  vHolder := Holder;
  for i := 0 to FListBox.Count - 1 do
    if FListBox.Checked[i] then
      vList.LinkListEntity(vHolder, TEntity(FListBox.Items.Objects[i]))
    else
      vList.UnlinkListEntity(vHolder, TEntity(FListBox.Items.Objects[i]));
end;

procedure TEntityListSelector.FillEditor;
var
  i: Integer;
  vInteractor: TInteractor;
  vList: TListField;
begin
  vList := FView.ExtractListField;
  if not Assigned(vList) then
    Exit;

  vInteractor := TInteractor(FView.Interactor);

  if FEntityList = nil then
    FEntityList := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  vList.GetEntitiesForSelect(vInteractor.Session, FEntityList);

  FListBox.Clear;

  for i := 0 to FEntityList.Count - 1 do
    FListBox.Items.AddObject(SafeDisplayName(FEntityList[i]), FEntityList[i]);
end;

procedure TEntityListSelector.OnWinControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  i: Integer;
  vList: TListField;
  vHolder: TObject;
begin
  vList := FView.ExtractListField;
  if not Assigned(vList) then
    Exit;

  if (Key = 65{a}) and (ssCtrl in Shift) then
  begin
    SwitchChangeHandlers(nil);

    vHolder := Holder;
    for i := 0 to FListBox.Count - 1 do
    begin
      FListBox.Checked[i] := True;
      vList.LinkListEntity(vHolder, TEntity(FListBox.Items.Objects[i]));
    end;
    SwitchChangeHandlers(OnChange);
  end;
end;

procedure TEntityListSelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  FListBox.OnClickCheck := AHandler;
end;

{ TEntityListSelector2 }

function TEntityListSelector2.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FListBox := TcxCheckListBox.Create(nil);
  Result := FListBox;
  FEntityList := nil;
end;

procedure TEntityListSelector2.DoOnChange;
var
  i: Integer;
  vList: TListField;
  vHolder: TObject;
begin
  vList := FView.ExtractListField;
  if not Assigned(vList) then
    Exit;

  vHolder := Holder;
  for i := 0 to FListBox.Count - 1 do
    if FListBox.Items[i].Checked then
      vList.LinkListEntity(vHolder, TEntity(FListBox.Items.Objects[i]))
    else
      vList.UnlinkListEntity(vHolder, TEntity(FListBox.Items.Objects[i]));
end;

procedure TEntityListSelector2.FillEditor;
var
  i: Integer;
  vItem: TcxCheckListBoxItem;
  vInteractor: TInteractor;
  vList: TListField;
begin
  vList := FView.ExtractListField;
  if not Assigned(vList) then
    Exit;

  vInteractor := TInteractor(FView.Interactor);

  if FEntityList = nil then
    FEntityList := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  vList.GetAllEntitiesForSelect(TInteractor(FView.Interactor).Session, FEntityList);

  FListBox.Clear;

  for i := 0 to FEntityList.Count - 1 do
  begin
    vItem := FListBox.Items.Add;
    vItem.Text := SafeDisplayName(FEntityList[i]);
    vItem.ItemObject := FEntityList[i];
    //for j := 0 to TListField(Field).Count - 1 do
    vItem.Checked := vList.Contains(FEntityList[i]);
  end;
end;

procedure TEntityListSelector2.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(AHandler) then
    FListBox.OnClickCheck := OnClickCheck
  else
    FListBox.OnClickCheck := nil;
end;

procedure TEntityListSelector2.OnClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
begin
  DoOnChange;
end;

{ TListEditor }

procedure TListEditor.DoBeforeFreeControl;
begin
  FList.OnSelectionChanged := nil;
  inherited;
end;

function TListEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FList := TcxTreeList.Create(nil);
  Result := FList;

  FList.OnSelectionChanged := OnSelectionChanged;
  FList.OnDblClick := OnDblClick;
  FList.CreateColumn;
  FList.OptionsView.Headers := True;
  if FCreateParams.Values['ShowHeader'] = 'No' then
    FList.OptionsView.Headers := False;
  FList.OptionsView.ShowRoot := False;
  FList.OptionsView.ColumnAutoWidth := True;
  FList.OptionsSelection.HideSelection := False;
  FList.OptionsSelection.HideFocusRect := False;
  FList.OptionsSelection.MultiSelect := False;
  if FList.ColumnCount > 1 then
  begin
    if StrToBoolDef(TDomain(TInteractor(FView.Interactor).Domain).UserSettings.GetValue('Core', 'ShowHorzLines'), True) then
      FList.OptionsView.GridLines := tlglBoth
    else
      FList.OptionsView.GridLines := tlglVert;
  end;
  FList.OptionsData.Editing := False;
  FList.OptionsSelection.CellSelect := False;
  FList.Font.Size := 10;
end;

procedure TListEditor.FillEditor;
var
  i: Integer;
  vEntity, vSelectedEntity: TEntity;
  vNode: TcxTreeListNode;
  vAllData: TEntityList;
begin
  FList.BeginUpdate;
  try
    vSelectedEntity := nil;
    if FList.SelectionCount > 0 then
      vSelectedEntity := TEntity(FList.Selections[0].Data);

    FList.Clear;
    vAllData := TEntityList(FView.DomainObject);
    vAllData.Resort;

    for i := 0 to vAllData.Count - 1 do
    begin
      vEntity := vAllData[i];

      if (not vEntity.Deleted) {IsMatchToFilter(vEnt))} then
      begin
        vNode := FList.Add;
        vNode.Data := vEntity;
        vNode.Values[0] := SafeDisplayName(vEntity);
        if vEntity = vSelectedEntity then
          vNode.Focused := True;
      end;
    end;

  finally
    FList.EndUpdate;
  end;
end;

procedure TListEditor.OnDblClick(Sender: TObject);
var
  vSelectedView: TView;
  vActionView: TView;
  vArea: TUIArea;
  vHolder: TObject;
begin
  if not TcxTreeList(Sender).HitTest.HitAtNode then Exit;
  vArea := TUIArea(TComponent(Sender).Tag);
  Assert(Assigned(vArea), 'Нет холдера для грида');

  vHolder := vArea.Holder;
  vSelectedView := FView.ViewByName('Selected');
  if not Assigned(vSelectedView) then
    Exit;
  vActionView := vSelectedView.ViewByName('Edit');
  if Assigned(vActionView) and (vActionView.State = vsFullAccess) then
    vActionView.ExecuteAction(vHolder)
  else begin
    vActionView := vSelectedView.ViewByName('View');
    if Assigned(vActionView) then
      vActionView.ExecuteAction(vHolder);
  end;
end;

procedure TListEditor.OnSelectionChanged(Sender: TObject);
var
  vEntityList: TEntityList;
  i: Integer;
  vList: TList<TEntity>;
begin
  vEntityList := TEntityList(TView(FView).DomainObject);

  vList := TList<TEntity>.Create;
  try
    for i := 0 to FList.SelectionCount - 1 do
      vList.Add(TEntity(FList.Selections[i].Data));
    vEntityList.SelectEntities(vList);
  finally
    FreeAndNil(vList);
  end;
end;

function TreeListFindFunc(ANode: TcxTreeListNode; AData: Pointer): Boolean;
begin
  Result := (ANode.Data = AData);
end;

procedure TListEditor.UpdateArea(const AKind: Word; const AParameter: TEntity);
var
  vNode: TcxTreeListNode;

  function FindNodeByEntity(const AEntity: TEntity): TcxTreeListNode;
  begin
    Result := FList.Find(AEntity, FList.Root, False, True, TreeListFindFunc);
  end;

begin
//  inherited UpdateArea(AKind, AParameter);
//  Exit;

  if AKind = dckEntityChanged then
  begin
    if not Assigned(AParameter) then
      Exit;
    vNode := FindNodeByEntity(AParameter);
    if Assigned(vNode) then
      vNode.Values[0] := SafeDisplayName(AParameter);
  end
  else if AKind = dckListAdded then
  begin
    vNode := FList.Add;
    vNode.Data := AParameter;
    vNode.Values[0] := SafeDisplayName(AParameter);
  end
  else if AKind = dckListRemoved then
  begin
    vNode := FindNodeByEntity(AParameter);
    if Assigned(vNode) then
      vNode.Delete;
  end
  else if AKind = dckSelectionChanged then
  begin
    // Обработка выделенной ноды
  end
  else if AKind = dckViewStateChanged then
    // Сделать что-то с видимостью
  else
    RefillArea(AKind);
end;

{ TCollectionEditor }

procedure TCollectionEditor.DoOnCompare(ADataController: TcxCustomDataController;
  ARecordIndex1, ARecordIndex2, AItemIndex: Integer; const V1, V2: Variant;
  var Compare: Integer);
var
  vSortColumn: TcxGridColumn;
  vColumnBinding: TColumnBinding;
begin
  if (ARecordIndex1 < 0) or (ARecordIndex2 < 0) then
  begin
    Compare := 0;
    Exit;
  end;

  vSortColumn := FMasterTableView.Columns[AItemIndex];
  vColumnBinding := TColumnBinding(vSortColumn.DataBinding.Data);
  if not Assigned(vColumnBinding) then Exit;
  
  Compare := CompareEntities(FMasterDS.Data[ARecordIndex1], FMasterDS.Data[ARecordIndex2],
    vColumnBinding.FieldDef, vColumnBinding.FieldName);
end;

procedure TCollectionEditor.BeforeContextMenuShow(Sender: TObject);
var
  vMenu: TPopupMenu;
  vMenuItem: TMenuItem;
  vArea: TUIArea;
  vParams: TEntity;
  i: Integer;
begin
  vMenu := TPopupMenu(Sender);
  if vMenu.Items.Count <= 0 then
    Exit;

  for i := 0 to vMenu.Items.Count - 1 do
  begin
    vMenuItem := vMenu.Items[i];
    vArea := TUIArea(vMenuItem.Tag);
    if not Assigned(vArea) then
      Continue;

    if not Assigned(vArea.View) then
      Continue;

    if vArea.View.DefinitionKind <> dkAction then
      Continue;

    if vArea.View.Name = '#GroupByColumn' then
    begin
      vParams := TEntity(vArea.View.DomainObject);
      vMenuItem.Checked := FMasterTableView.OptionsView.GroupByBox;
      if vParams['IsChecked'] <> vMenuItem.Checked then
        vParams._SetFieldValue(FSession.NullHolder, 'IsChecked', vMenuItem.Checked);
    end;
  end;

  vMenuItem := vMenu.Items[vMenu.Items.Count - 1];
  vMenuItem.Caption := TInteractor(Interactor).Translate('txtRecordCount', 'Записей') + ': ' + IntToStr(FAllData.Count);
end;

constructor TCollectionEditor.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
  const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = '');
var
  vDefinition: TDefinition;
  vView: TView;
  vColumn: TcxGridColumn;
  vFields: string;
begin
  FBGStyle := TcxStyle.Create(nil);
  FHeaderStyle := TcxStyle.Create(nil);
  FEditRepository := TcxEditRepository.Create(nil);
  FMasterDS := TUserDataSource.Create(Self);

  FMasterTableView := TcxGridTableView.Create(nil);
  FMasterTableView.DataController.OnCompare := DoOnCompare;
  FMasterTableView.OptionsData.Editing := False;
  FMasterTableView.OptionsData.Deleting := False;
  FMasterTableView.OptionsData.Inserting := False;
  FMasterTableView.OptionsCustomize.ColumnsQuickCustomization := True;
//  FMasterTableView.OptionsSelection.CellSelect := False;
  FMasterTableView.OptionsSelection.MultiSelect := True;
  FMasterTableView.OptionsSelection.HideSelection := True;

  if StrToBoolDef(TDomain(TInteractor(AView.Interactor).Domain).UserSettings.GetValue('Core', 'MouseMultiSelectInGrids'), False) then
  begin
    FMasterTableView.OptionsSelection.CheckBoxVisibility := [cbvDataRow, cbvColumnHeader];
    FMasterTableView.OptionsSelection.ShowCheckBoxesDynamically := True;
  end
  else
  begin
    FMasterTableView.OptionsSelection.CheckBoxVisibility := [];
    FMasterTableView.OptionsSelection.ShowCheckBoxesDynamically := False;
  end;

  FMasterTableView.OptionsView.GroupByBox := False;

  if StrToBoolDef(TDomain(TInteractor(AView.Interactor).Domain).UserSettings.GetValue('Core', 'ShowHorzLines'), True) then
    FMasterTableView.OptionsView.GridLines := glBoth
  else
    FMasterTableView.OptionsView.GridLines := glVertical;

  FMasterTableView.OptionsBehavior.CellHints := True;
  FMasterTableView.DataController.CustomDataSource := FMasterDS;
  FMasterTableView.OnCellDblClick:= DoOnCellDblClick;
  FMasterTableView.OnSelectionChanged := DoOnSelectionChanged;
  FMasterTableView.OnCanSelectRecord := DoCanSelectRecord;
  FMasterTableView.DataController.Summary.Options := [soSelectedRecords, soMultipleSelectedRecords];
  FMasterTableView.Styles.Background := FBGStyle;
  FMasterTableView.Styles.Header := FHeaderStyle;

  FGrid := TcxGrid.Create(nil);
  FGrid.LookAndFeel.NativeStyle := False;
  FGrid.LookAndFeel.Kind := lfFlat;

  if ALayout.Control is TPanel then
    FGrid.Align := TPanel(ALayout.Control).Align
  else
    FGrid.Align := alClient;
  FGrid.Levels.Add.GridView := FMasterTableView;
//  FGrid.Levels.Add.GridView := FChartView;
  FGrid.Font.Size := 12;

  inherited Create(AParent, AView, AId, AIsService, FGrid, ALayout, AParams);

  // after inherited Create, Interactor must be initialized
  cxSetResourceString(@scxGridGroupByBoxCaption, TInteractor(Interactor).Translate('txtMoveColumnForGrouping',
    'Перетащите сюда колонку для группировки'));
  FMasterTableView.OptionsView.NoDataToDisplayInfoText := TInteractor(Interactor).Translate('@Grid@NoDataText', '<Нет данных>');
  FAllData := TEntityList(FView.DomainObject);

  FBGStyle.Color := GetBGColor(FAllData.MainDefinition, clWindow);
  FHeaderStyle.Font.Size := 10;
  FHeaderStyle.TextColor := clGray;

  EnableColumnParamsChangeHandlers(False);

  if Assigned(FCreateParams) then
  begin
    vFields := FCreateParams.Values['fields'];
    if FCreateParams.Values['editingInGrid'] = 'true' then
    begin
      FMasterTableView.OptionsData.Editing := True;
      FMasterTableView.OptionsSelection.CellSelect := True;
    end;
  end
  else
    vFields := '';

  if vFields = '' then
    vFields := AParent.QueryParameter('fields');

  CreateColumnsFromModel(vFields);
  LoadColumnWidths;

  EnableColumnParamsChangeHandlers(True);

  vDefinition := FAllData.MainDefinition;
  FGroupFieldName := Trim(AParent.QueryParameter('Group'));
  if (FGroupFieldName = '') and Assigned(vDefinition) then
    FGroupFieldName := Trim(vDefinition.GroupFieldName);
  if FGroupFieldName = '-' then
    FGroupFieldName := '';

  FColorFieldName := Trim(AParent.QueryParameter('Color'));
  if (FColorFieldName = '') and Assigned(vDefinition) then
    FColorFieldName := Trim(vDefinition.ColorFieldName);
  if FColorFieldName = '-' then
    FColorFieldName := '';

  if (FGroupFieldName <> '') and (FAllData.FillerKind = lfkDefinition) then
  begin
    vColumn := FindColumnByFieldName(FGroupFieldName);
    if Assigned(vColumn) then
    begin
      vColumn.Visible := False;
      vColumn.GroupIndex := 0;
      vView := FView.BuildView('#GroupByColumn');
      TEntity(vView.DomainObject)._SetFieldValue(FSession.NullHolder, 'IsChecked', True);
      FMasterTableView.OptionsView.GroupByBox := True;
      FMasterTableView.DataController.Options := FMasterTableView.DataController.Options + [dcoGroupsAlwaysExpanded];
    end;
  end;
end;

procedure TCollectionEditor.CreateColumnsFromModel(const AFields: string = '');
var
  i: Integer;
  vFieldDef: TFieldDef;
  vActionDef: TActionDef;
  vWidth: Integer;
  vAggType: TAggregationKind;
  vMainDefinition: TDefinition;
  vFields: TStrings;
  vFieldName: string;
begin
  FMasterTableView.BeginUpdate;
  try
    vMainDefinition := FAllData.MainDefinition;
    if AFields = '' then
    begin
      for vFieldDef in vMainDefinition.Fields do
      begin
        if TInteractor(FView.Interactor).NeedSkipColumn(FAllData, vFieldDef) then
          Continue;

        vWidth := TWinVCLPresenter(TInteractor(FView.Interactor).Presenter).GetWidthByType(100, vFieldDef);
        { TODO -cUI refactoring : Перенести описание аггрегаций в слой описания UI }
        vAggType := vMainDefinition.Reductions.ReductionForField(vFieldDef.Name);
        CreateColumn(vFieldDef, vFieldDef.Name, GetFieldTranslation(vFieldDef),
          vWidth, uConsts.soNone, vAggType);
      end;

      // Очень сильно тормозит на прокрутке списков
      //if FAllData.ContentDefinitions.Count > 1 then
      //  CreateColumn(nil, '', 50, uConsts.soNone, akNotDefined);
    end
    else begin
      vFields := CreateDelimitedList(AFields);
      try
        for i := 0 to vFields.Count - 1 do
        begin
          vFieldName := vFields[i];
          vFieldDef := vMainDefinition.ExtractFieldDef(vFieldName);
          if Assigned(vFieldDef) then
          begin
            vWidth := TWinVCLPresenter(TInteractor(FView.Interactor).Presenter).GetWidthByType(100, vFieldDef);
            vAggType := TDefinition(vFieldDef.Definition).Reductions.ReductionForField(vFieldName);

            CreateColumn(vFieldDef, vFieldName, GetFieldTranslation(vFieldDef), vWidth, uConsts.soNone, vAggType);
          end
          else
          begin
            vActionDef := vMainDefinition.ActionByName(vFieldName);
            if Assigned(vActionDef) then
              CreateColumnForAction(vActionDef, vFieldName);
          end;
        end;
      finally
        vFields.Free;
      end;
    end;
  finally
    FMasterTableView.EndUpdate;
  end;
end;

destructor TCollectionEditor.Destroy;
begin
  if Assigned(FMasterTableView.PopupMenu) then
  begin
    TPopupMenu(FMasterTableView.PopupMenu).OnPopup := nil;
    FMasterTableView.PopupMenu := nil;
  end;

  FAllData := nil;

  FreeAndNil(FMasterTableView);
  FreeAndNil(FMasterDS);
  FreeAndNil(FBGStyle);
  FreeAndNil(FHeaderStyle);
  FreeAndNil(FEditRepository);

  inherited Destroy;
end;

function TCollectionEditor.GetSearchType: TSearchType;
begin
  //todo:
 { if FAllData.Filler is TEntityField then
    Result := TEntityField(FAllData.Filler).SearchType
  else }
    Result := stSearchFromBegin;
end;

procedure TCollectionEditor.ExportToExcel;
  function GetFileName(out AFileName: string): Boolean;
  var
    vSaveDialog: TSaveDialog;
  begin
    vSaveDialog := TSaveDialog.Create(nil);
    try
      vSaveDialog.Filter := '*.xls|*.xls';
      vSaveDialog.InitialDir := GetDesktopDir;
      vSaveDialog.FileName := StringReplace(FAllData.MainDefinition.Name, '/', '_', [rfReplaceAll]) + '.xls';
      vSaveDialog.Options := vSaveDialog.Options + [ofOverwritePrompt];
      vSaveDialog.DefaultExt := '*.xls';

      Result := vSaveDialog.Execute;

      if Result then
        AFileName := ChangeFileExt(vSaveDialog.FileName, '.xls');

    finally
      vSaveDialog.Free;
    end;
  end;
var
  vFileName: string;
begin
  if not GetFileName(vFileName) then Exit;

  ExportGridToExcel(vFileName, FGrid, True, FMasterTableView.Controller.SelectedRowCount < 2, False);

  if FileExists(vFileName) and (TPresenter(Presenter).ShowYesNoDialog('Export', 'Хотите открыть этот файл?') = drYes) then
    TPresenter(Presenter).OpenFile(vFileName);
end;

procedure TCollectionEditor.ExportToCsv;
  function GetFileName(out AFileName: string): Boolean;
  var
    vSaveDialog: TSaveDialog;
  begin
    vSaveDialog := TSaveDialog.Create(nil);
    try
      vSaveDialog.Filter := '*.csv|*.csv';
      vSaveDialog.InitialDir := GetDesktopDir;
      vSaveDialog.FileName := StringReplace(FAllData.MainDefinition.Name, '/', '_', [rfReplaceAll]) + '.csv';
      vSaveDialog.Options := vSaveDialog.Options + [ofOverwritePrompt];
      vSaveDialog.DefaultExt := '*.csv';

      Result := vSaveDialog.Execute;

      if Result then
        AFileName := ChangeFileExt(vSaveDialog.FileName, '.csv');

    finally
      vSaveDialog.Free;
    end;
  end;
var
  vFileName: string;
begin
  if not GetFileName(vFileName) then Exit;

  ExportGridToCSV(vFileName, FGrid, True, FMasterTableView.Controller.SelectedRowCount < 2, ';', 'csv');

  if FileExists(vFileName) and (TPresenter(Presenter).ShowYesNoDialog('Export', 'Хотите открыть этот файл?') = drYes) then
    TPresenter(Presenter).OpenFile(vFileName);
end;

function TCollectionEditor.FindColumnByFieldName(const AFieldName: string): TcxGridColumn;
var
  i: Integer;
  vName: string;
begin
  Result := nil;
  for i := 0 to FMasterTableView.ColumnCount - 1 do
  begin
    vName := TColumnBinding(FMasterTableView.Columns[i].DataBinding.Data).FieldName;
    if vName = AFieldName then
    begin
      Result := FMasterTableView.Columns[i];
      Break;
    end;
  end;
end;

function TCollectionEditor.IsMatchToFilter(const AEntity: TEntity): Boolean;
var
  vFilterText, vText: string;
  vST: TSearchType;
  vDate: TDateTime;
begin
  Result := True;

  vFilterText := Trim(FFilterText);
  if Length(vFilterText) > 0 then
  begin
    vFilterText := AnsiUpperCase(vFilterText);

    if FMasterTableView.ColumnCount = 1 then
    begin
      vText := AnsiUpperCase(AEntity.DisplayName);
      vST := GetSearchType;
      Result :=
        ((vST = stSearchFromBegin) and (Pos(vFilterText, vText) = 1)) or
        ((vST = stSearchEverywhere) and (Pos(vFilterText, vText) > 0)) or
        ((vST = stSearchMultiEntrance) and IsTextMultiEqual(vFilterText, vText));
    end
    else
    begin
      vText := AnsiUpperCase(AEntity.FullText);
      Result := Pos(vFilterText, vText) > 0;
    end;
  end;

  if Result and FFilterDateActive and AEntity.Definition.FieldExists('DocDate') then
  begin
    if (FFilterDateFrom = NullDate) and (FFilterDateTo = NullDate) then
      Exit;

    vDate := AEntity['DocDate'];
    vDate := Trunc(vDate);

    if (FFilterDateFrom = NullDate) then
      Result := vDate <= FFilterDateTo
    else if (FFilterDateTo = NullDate) then
      Result := vDate >= FFilterDateFrom
    else
      Result := (vDate >= FFilterDateFrom) and (vDate <= FFilterDateTo);
  end;
end;

procedure TCollectionEditor.UpdateArea(const AKind: Word; const AParameter: TEntity = nil);
var
  i: Integer;
  vEntity: TEntity;
  vNewFocusedRowIndex: Integer;
  vRow: TcxCustomGridRow;
begin
  if AKind = dckEntityChanged then
  begin
    if not FMasterDS.IsLoading then
    begin
      //if FMasterTableView.OptionsView.GroupByBox then
      //  FMasterTableView.DataController.CustomDataSource.DataChanged
      //else
        FMasterTableView.Invalidate(True);
    end;
  end
  else if AKind = dckEntitySaved then
  begin
    FMasterTableView.OnSelectionChanged := nil;
    try
      FMasterTableView.DataController.CustomDataSource.DataChanged;
    finally
      FMasterTableView.OnSelectionChanged := DoOnSelectionChanged;
    end;
  end
  else if AKind = dckListAdded then
  begin
    FMasterDS.Add(AParameter, not TDomain(Domain).LoadingChanges);
  end
  else if AKind = dckListRemoved then
  begin
    FMasterDS.Remove(AParameter, not TDomain(Domain).LoadingChanges);
  end
  else if AKind = dckSelectionChanged then
  begin
    vNewFocusedRowIndex := FMasterDS.FEntities.IndexOf(AParameter);
    if vNewFocusedRowIndex <> FMasterTableView.DataController.FocusedRowIndex then
    begin
      vRow := FMasterTableView.ViewData.Rows[vNewFocusedRowIndex];
      vRow.Selected := True;
      vRow.Focused := True;
    end;
  end
  else if AKind = dckViewStateChanged then
    // Сделать что-то с видимостью
  else //if AKind = dckFieldChanged then
  begin // FieldChanged, FilterChanged
    FMasterTableView.OnSelectionChanged := nil;
    FMasterTableView.BeginUpdate;
    try
      FMasterDS.Data.Clear;

      FAllData.Resort;
      for i := 0 to FAllData.Count - 1 do
      begin
        vEntity := FAllData[i];
        if not vEntity.Deleted and IsMatchToFilter(vEntity) then
          FMasterDS.Data.Add(vEntity);
      end;

      FMasterDS.Update;
    finally
      FMasterTableView.EndUpdate;
      FMasterTableView.OnSelectionChanged := DoOnSelectionChanged;
    end;

    if not FLayoutExists then
    begin
	 // TODO: Разобраться!
     // FMasterTableView.ApplyBestFit;
     // SaveColumnWidths;
    end;

    //FMasterDS.FocusRow(0);
  end;
end;

procedure TCollectionEditor.EnableColumnParamsChangeHandlers(const AEnable: Boolean);
begin
  if AEnable then
  begin
    FMasterTableView.OnColumnPosChanged := DoOnColumnPosChanged;
    FMasterTableView.OnColumnSizeChanged := DoOnColumnSizeChanged;
    FMasterTableView.DataController.OnSortingChanged := DoOnHeaderClick;
  end
  else
  begin
    FMasterTableView.OnColumnPosChanged := nil;
    FMasterTableView.OnColumnSizeChanged := nil;
    FMasterTableView.DataController.OnSortingChanged := nil;
  end;
end;

procedure TCollectionEditor.ShowFooter(const AColumn: TcxGridColumn; const AAgg: TAggregationKind);
begin
  if AAgg = akNotDefined then Exit;
  FMasterTableView.OptionsView.Footer := True;

  if AAgg = akCount then
    AColumn.Summary.FooterKind := skCount
  else if AAgg = akSum then
  begin
    AColumn.Summary.FooterKind := skSum;
    AColumn.Summary.FooterFormat := '0.00';
  end
  else if AAgg = akAverage then
    AColumn.Summary.FooterKind := skAverage;
end;

procedure TCollectionEditor.CreateColumn(const AFieldDef: TFieldDef; const AFieldName: string;
  const AOverriddenCaption: string; const AWidth: Integer;
  const ASortOrder: TSortOrder; const AAgg: TAggregationKind);
var
  vCol: TcxGridColumn;
  vColumnBinding: TColumnBinding;
  vComboItem: TcxImageComboBoxItem;
  i: Integer;
  vDefinition: TDefinition;
  vImageID: Integer;
  vStyleName: string;
begin
  vCol := FMasterTableView.CreateColumn;
  vCol.Caption := AOverriddenCaption;
  vColumnBinding := FMasterDS.AddColumn(AFieldName, AFieldDef);
  vCol.DataBinding.Data := vColumnBinding;
  vColumnBinding.FColumn := vCol;

  if Assigned(AFieldDef) then
  begin
    vCol.Name := vColumnBinding.SanitizedFieldName;
    if AFieldDef.Kind in [fkInteger, fkFloat, fkCurrency] then
    begin
      vStyleName := GetUrlCommand(AFieldDef.StyleName);

      if vStyleName = 'progress' then
      begin
        vCol.PropertiesClass := TcxProgressBarProperties;
        //TcxProgressBarProperties(vCol.Properties).BarStyle := cxbsAnimation;
        //TcxProgressBarProperties(vCol.Properties).SolidTextColor := True;
        TcxProgressBarProperties(vCol.Properties).Max := TSimpleFieldDef(AFieldDef).MaxValue;
      end
      else
      begin
        vCol.PropertiesClassName := 'TcxSpinEditProperties';   // need for Excel export

        if AFieldDef.Kind in [fkFloat, fkCurrency] then
          TcxSpinEditProperties(vCol.Properties).ValueType := vtFloat;

        if Length(AFieldDef.Format) > 0 then
          TcxSpinEditProperties(vCol.Properties).DisplayFormat := AFieldDef.Format;

        if not VarIsNull(TSimpleFieldDef(AFieldDef).MinValue) then
          TcxSpinEditProperties(vCol.Properties).MinValue := TSimpleFieldDef(AFieldDef).MinValue;
        if not VarIsNull(TSimpleFieldDef(AFieldDef).MaxValue) then
          TcxSpinEditProperties(vCol.Properties).MaxValue := TSimpleFieldDef(AFieldDef).MaxValue;
      end;
      vCol.HeaderAlignmentHorz := taRightJustify;
      vCol.FooterAlignmentHorz := taRightJustify;
      vCol.Properties.Alignment.Horz := taRightJustify;
    end
    else if AFieldDef.Kind = fkDateTime then
    begin
      if AFieldDef.StyleName = 'time' then
        vCol.PropertiesClassName := 'TcxTimeEditProperties'
      else
      begin
        vCol.PropertiesClass := TcxDateEditProperties;  // need for Excel export
        TcxDateEditProperties(vCol.Properties).DisplayFormat := GetUrlParam(AFieldDef.StyleName, 'format');
      end;
    end
    else if AFieldDef.Kind = fkBoolean then
    begin
      vCol.PropertiesClass := TcxCheckBoxProperties;
      TcxCheckBoxProperties(vCol.Properties).DisplayChecked := 'Да';
      TcxCheckBoxProperties(vCol.Properties).DisplayUnchecked := 'Нет';
    end
    else if AFieldDef.Kind = fkColor then
    begin
      vCol.PropertiesClassName := 'TdxColorEditProperties';
      TdxColorEditProperties(vCol.Properties).ColorPalette := TdxColorPalette.cpExtended;
      TdxColorEditProperties(vCol.Properties).ColorSet := TdxColorSet.csDefault;
    end
    else if (AFieldDef.Kind = fkEnum) then
    begin
      if (AFieldDef.StyleName = 'images') then
      begin
        vCol.PropertiesClass := TcxImageComboBoxProperties;
        TcxImageComboBoxProperties(vCol.Properties).Images := TDragImageList(TInteractor(FView.Interactor).Images[16]);
        for i := 0 to FAllData.ContentDefinitions.Count - 1 do
        begin
          vDefinition := TDefinition(FAllData.ContentDefinitions[i]);
          vComboItem := TcxImageComboBoxProperties(vCol.Properties).Items.Add;
          vImageID := GetImageID(vDefinition._ImageID);
          vComboItem.Value := vImageID;
          vComboItem.ImageIndex := vImageID;
        end;
      end
      else
      begin
        vCol.PropertiesClass := TcxComboBoxProperties;
        TcxComboBoxProperties(vCol.Properties).DropDownListStyle := lsFixedList;
        FillEnumList(TDomain(FView.Domain), TcxComboBoxProperties(vCol.Properties).Items, vCol);
      end;
    end;

    vCol.SortOrder := TcxDataSortOrder(ASortOrder);
    vCol.Styles.OnGetContentStyle := OnGetContentStyle;
    if not Assigned(vCol.Properties) then
      vCol.PropertiesClass := TcxTextEditProperties;
    vCol.Properties.ReadOnly := AFieldDef.UIState < vsSelectOnly;

  end
  else
  begin
    vCol.PropertiesClass := TcxImageComboBoxProperties;
    TcxImageComboBoxProperties(vCol.Properties).Images := TDragImageList(TInteractor(FView.Interactor).Images[16]);
    for i := 0 to FAllData.ContentDefinitions.Count - 1 do
    begin
      vDefinition := TDefinition(FAllData.ContentDefinitions[i]);
      vComboItem := TcxImageComboBoxProperties(vCol.Properties).Items.Add;
      vImageID := GetImageID(vDefinition._ImageID);
      vComboItem.Value := vImageID;
      vComboItem.ImageIndex := vImageID;
    end;
  end;

  ShowFooter(vCol, AAgg);
end;

procedure TCollectionEditor.CreateBtnPropertiesForAction(const AActionDef: TActionDef);
var
  vEditRepositoryButtonItem: TcxEditRepositoryButtonItem;
  vBtn: TcxEditButton;  
  procedure CreateProps(const AEnabled: Boolean; const APostfix: string);
  begin
    vEditRepositoryButtonItem := TcxEditRepositoryButtonItem(FEditRepository.CreateItem(TcxEditRepositoryButtonItem));
    vEditRepositoryButtonItem.Name := AActionDef.Name + APostfix;
    vEditRepositoryButtonItem.Properties.ViewStyle := vsButtonsAutoWidth;
    vEditRepositoryButtonItem.Properties.Images := TDragImageList(TInteractor(FView.Interactor).Images[16]);
    vBtn := vEditRepositoryButtonItem.Properties.Buttons[0];
    vBtn.Kind := bkGlyph;
    vBtn.ImageIndex := GetImageID(AActionDef._ImageID);
    vBtn.Hint := AActionDef._Caption; 
    vBtn.Enabled := AEnabled;
  end;
begin
  CreateProps(True, 'Enabled');
  CreateProps(False, 'Disabled');
end;

procedure TCollectionEditor.CreateColumnForAction(const AActionDef: TActionDef; const AFieldName: string);
var
  vCol: TcxGridColumn;
begin
  vCol := FMasterTableView.CreateColumn;
  vCol.Name := AFieldName;
  vCol.Caption := '';
  vCol.OnGetProperties := OnActionGetProperties;
  vCol.Options.Editing := True;
  vCol.Options.ShowEditButtons := isebAlways; 
  vCol.DataBinding.Data := AActionDef;
  CreateBtnPropertiesForAction(AActionDef);
end;

procedure TCollectionEditor.OnActionGetProperties(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
var
  vCol: TcxGridColumn;
  vActionDef: TActionDef;
  vEditRepositoryItem: TcxEditRepositoryItem;
  vBtnEnabled: Boolean;
begin
  vCol := TcxGridColumn(Sender);
  vActionDef := TActionDef(vCol.DataBinding.Data);
  vBtnEnabled := ARecord.Index mod 2 = 0;  // todo: здесь рассчитать доступность кнопки
  vEditRepositoryItem := FEditRepository.ItemByName(vActionDef.Name + IfThen(vBtnEnabled, 'Enabled', 'Disabled'));
  if Assigned(vEditRepositoryItem) then
    AProperties := vEditRepositoryItem.Properties;
end;

procedure TCollectionEditor.OnGetContentStyle(Sender: TcxCustomGridTableView;
  ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
begin
  GetContentStyle(Self, FAllData, FColorFieldName, FMasterDS.Data, Sender, ARecord, AItem, AStyle);
end;

procedure TCollectionEditor.SaveColumnWidths;
begin
  SaveGridColumnWidths(TInteractor(FView.Interactor).Domain, FMasterTableView, FView.InitialName);
end;

procedure TCollectionEditor.SetPopupArea(const APopupArea: TUIArea);
var
  vPopupMenu: TPopupMenu;
begin
  vPopupMenu := TPopupMenu(APopupArea.InnerControl);
  vPopupMenu.OnPopup := BeforeContextMenuShow;
  FMasterTableView.PopupMenu := vPopupMenu;
end;

procedure TCollectionEditor.LoadColumnWidths;
begin
  FLayoutExists := LoadGridColumnWidths(TInteractor(FView.Interactor).Domain, FMasterTableView, FView.InitialName);
end;

procedure TCollectionEditor.DoCanSelectRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  var AAllow: Boolean);
begin
  AAllow := not (ARecord is TcxGridGroupRow);
end;

procedure TCollectionEditor.DoExecuteUIAction(const AView: TView);
var
  vParams: TEntity;
  vDateFrom, vDateTo: TDateTime;
  vIsPeriodActive: Boolean;
  i: Integer;
begin
  if AView.Name = '#ExportToCsv' then
    ExportToExcel
  else if AView.Name = '#ExportToCsv2' then
    ExportToCsv
  else if AView.Name = '#FilterByText' then
  begin
    if not SameText(FFilterText, TEntity(AView.DomainObject)['Text']) then
    begin
      FFilterText := TEntity(AView.DomainObject)['Text'];
      UpdateArea(dckFilterChanged, nil);
    end;
  end
  else if AView.Name = '#FilterByPeriod' then
  begin
    vDateFrom := TEntity(AView.DomainObject)['FromDate'];
    vDateTo := TEntity(AView.DomainObject)['ToDate'];
    vIsPeriodActive := TEntity(AView.DomainObject)['IsActive'];

    if (not SameDate(vDateFrom, FFilterDateFrom)) or (not SameDate(vDateTo, FFilterDateTo))
      or (vIsPeriodActive <> FFilterDateActive) then
    begin
      if vDateFrom = cNullDateTime then
        FFilterDateFrom := NullDate
      else
        FFilterDateFrom := vDateFrom;

      if vDateTo = cNullDateTime then
        FFilterDateTo := NullDate
      else
        FFilterDateTo := vDateTo;

      FFilterDateActive := vIsPeriodActive;

      UpdateArea(dckFilterChanged, nil);
    end;
  end
  else if AView.Name = '#GroupByColumn' then
  begin
    vParams := TEntity(AView.DomainObject);
    vParams._SetFieldValue(FSession.NullHolder, 'IsChecked', not vParams['IsChecked']);
    FMasterTableView.OptionsView.GroupByBox := vParams['IsChecked'];
    if not FMasterTableView.OptionsView.GroupByBox then // remove groups
      for i := 0 to FMasterTableView.ColumnCount - 1 do
        if FMasterTableView.Columns[i].GroupIndex >= 0 then
        begin
          FMasterTableView.Columns[i].Visible := True;
          FMasterTableView.Columns[i].GroupIndex := -1;
        end;
  end
  else if AView.Name = '#ApplyBestFit' then
    FMasterTableView.ApplyBestFit;
end;

procedure TCollectionEditor.DoOnColumnPosChanged(Sender: TcxGridTableView;
  AColumn: TcxGridColumn);
begin
  SaveColumnWidths;
end;

procedure TCollectionEditor.DoOnColumnSizeChanged(Sender: TcxGridTableView;
  AColumn: TcxGridColumn);
begin
  SaveColumnWidths;
end;

procedure TCollectionEditor.DoOnHeaderClick(Sender: TObject);
begin
  SaveColumnWidths;
end;

procedure TCollectionEditor.DoOnSelectionChanged(Sender: TcxCustomGridTableView);
var
  vEntityList: TEntityList;
  vList: TList<TEntity>;
  i: Integer;
begin
  vEntityList := TEntityList(TView(FView).DomainObject);

  vList := TList<TEntity>.Create;
  try
    for i := 0 to FMasterTableView.Controller.SelectedRecordCount - 1 do
      vList.Add(FMasterDS.Data[FMasterTableView.Controller.SelectedRecords[i].RecordIndex]);

    vEntityList.SelectEntities(vList);
  finally
    FreeAndNil(vList);
  end;
end;

procedure TCollectionEditor.DoOnCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
  AShift: TShiftState; var AHandled: Boolean);
begin
  if AButton = mbLeft then
    OnTableViewDblClick(FView, Self);
end;

{ TUserDataSource }

procedure TUserDataSource.Add(const AEntity: TEntity; const AUpdateSelection: Boolean);
var
  i: Integer;
begin
  if FEntities.IndexOf(AEntity) >= 0 then
    Exit;

  FEntities.Add(AEntity);
  DataController.AppendRecord;
  DataChanged;

  if AUpdateSelection then
  begin
    i := DataController.GetRowIndexByRecordIndex(FEntities.Count - 1, False);
    FocusRow(i);
  end;
end;

function TUserDataSource.AddColumn(const AFieldName: string; const AFieldDef: TFieldDef): TColumnBinding;
begin
  Result := TColumnBinding.Create(AFieldName, AFieldDef);
  FColumns.Add(Result);
end;

function TUserDataSource.AppendRecord: TcxDataRecordHandle;
begin
  Result := TcxDataRecordHandle(FEntities.Last);
end;

constructor TUserDataSource.Create(const AEditor: TUIArea);
begin
  FEntities := TList<TEntity>.Create;
  FColumns := TObjectList<TColumnBinding>.Create;
  FIsLoading := False;
  FEditor := AEditor;
end;

procedure TUserDataSource.DataChanged;
begin
  inherited DataChanged;
end;

procedure TUserDataSource.DeleteRecord(ARecordHandle: TcxDataRecordHandle);
begin
  inherited;

end;

destructor TUserDataSource.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FEntities);
  inherited;
end;

procedure TUserDataSource.FocusRow(const ARowIndex: Integer);
begin
  try
    if (ARowIndex >= 0) and (ARowIndex < FEntities.Count) then
    begin
      DataController.ClearSelection;
      DataController.SelectRows(ARowIndex, ARowIndex); // select new row
      DataController.FocusSelectedRow(0);
    end;
  except
  end;
end;

function TUserDataSource.GetItemHandle(AItemIndex: Integer): TcxDataItemHandle;
begin
  Result := TcxDataItemHandle(TcxCustomGridTableItem(DataController.GetItem(AItemIndex)).DataBinding.Data);
end;

function TUserDataSource.GetRecordCount: Integer;
begin
  Result := FEntities.Count;
end;

function TUserDataSource.GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle;
begin
  Result := TcxDataRecordHandle(FEntities[ARecordIndex]);
end;

function TUserDataSource.GetRecordHandleByIndex(ARecordIndex: Integer): TcxDataRecordHandle;
begin
  Result := TcxDataRecordHandle(FEntities[ARecordIndex]);
end;

function TUserDataSource.GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;
var
  vColumnBinding: TColumnBinding;
  vFieldDef: TFieldDef;
  vField: TBaseField;
  vFieldName: string;
  vEntity: TEntity;
  vEnumeration: TEnumeration;
begin
  vEntity := nil;
  Result := Null;
  FIsLoading := True;
  try
    try
      vEntity := TEntity(ARecordHandle);
      if not Assigned(vEntity) then
        Exit;

      if (AItemHandle = nil) or (not (TObject(AItemHandle) is TColumnBinding)) then Exit;

      vColumnBinding := TColumnBinding(AItemHandle);

      vFieldName := vColumnBinding.FieldName;
      vField := vEntity.FieldByName(vFieldName);
      if Assigned(vField) then
        vFieldDef := vField.FieldDef
      else
        vFieldDef := vColumnBinding.FieldDef;

      if vFieldDef = nil then
      begin
        Result := vEntity.DisplayName;
        Exit;
      end;

      Result := vEntity.ExtractFieldValue(vFieldName);
      if VarIsNull(Result) then
        Exit;

      if vFieldDef.Kind = fkDateTime then
      begin
        if (TDateTime(Result) < 2) and (vFieldDef.StyleName <> 'time') then
          Result := Null
        else if vFieldDef.StyleName = 'date' then // cut the time
          Result := Trunc(Result);
      end
      else if vFieldDef.Kind = fkEnum then
      begin
        vEnumeration := TDomain(vEntity.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(vFieldDef).Dictionary);
        if Assigned(vEnumeration) then
          Result := vEnumeration.Items[Integer(vEntity.FieldByName(vFieldDef.Name).Value)].DisplayText
        else
          Result := vEntity.GetStateCaption(vFieldName);
      end
      else if vFieldDef.Kind in [fkInteger, fkFloat, fkCurrency] then
      begin
        if vFieldDef.Kind in [fkFloat, fkCurrency] then
          Result := RoundTo(Result, -vColumnBinding.AfterPoint);
      end;
    except
      on E: Exception do
        if Assigned(vEntity) then
          TDomain(vEntity.Domain).Logger.AddMessage('Ошибка получения данных из модели. Поле [' + vFieldName + ']');
    end;
  finally
    FIsLoading := False;
  end;
end;

function TUserDataSource.IsNativeCompare: Boolean;
begin
  Result := True;
end;

procedure TUserDataSource.Remove(const AEntity: TEntity; const AUpdateSelection: Boolean);
begin
  if AUpdateSelection then
  begin
    DataController.ClearSelection;
    DataController.FocusedRecordIndex := -1;
  end;
  DataController.DeleteRecord(FEntities.IndexOf(AEntity));
  FEntities.Remove(AEntity);
  DataChanged;
end;

procedure TUserDataSource.SetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant);
var
  vColumnBinding: TColumnBinding;
  vFieldDef: TFieldDef;
  vFieldName: string;
  vEntity, vEntityValue: TEntity;
  vEnumeration: TEnumeration;
  vInteractor: TInteractor;
  vValue: Variant;
  vItem: TEnumItem;
  vField: TBaseField;
  vEntities: TEntityList;
  vEntityField: TEntityField;
  i: Integer;
  procedure DoChange;
  begin
    TUserSession(vInteractor.Session).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
      begin
        vEntity._SetFieldValue(AHolder, vFieldName, vValue);
        Result := True;
      end, TChangeHolder(FEditor.Holder));
  end;
begin
  vEntity := nil;
  FIsLoading := True;
  try
    try
      vEntity := TEntity(ARecordHandle);
      if not Assigned(vEntity) then
        Exit;

      if (AItemHandle = nil) or (not (TObject(AItemHandle) is TColumnBinding)) then Exit;

      vColumnBinding := TColumnBinding(AItemHandle);

      vFieldName := vColumnBinding.FieldName;
      vField := vEntity.FieldByName(vFieldName);

      if Assigned(vField) then
        vFieldDef := vField.FieldDef
      else
        vFieldDef := vColumnBinding.FieldDef;

      if vFieldDef = nil then
        Exit;

      vInteractor := TInteractor(FEditor.View.Interactor);

      if vFieldDef.Kind = fkDateTime then
      begin
      {  if (TDateTime(Result) < 2) and (vFieldDef.StyleName <> 'time') then
          Result := Null
        else if vFieldDef.StyleName = 'date' then // cut the time
          Result := Trunc(Result);  }
      end
      else if vFieldDef.Kind = fkEnum then
      begin
        vEnumeration := TDomain(vEntity.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(vFieldDef).Dictionary);
        if Assigned(vEnumeration) then
        begin
          vItem := vEnumeration.GetItemByDisplayText(AValue);
          if Assigned(vItem) then
          begin
            vValue := vItem.ID;
            DoChange;
          end;
        end;
      end
      else if vFieldDef.Kind in [fkInteger, fkFloat, fkCurrency, fkColor, fkString, fkBoolean] then
      begin
        vValue := AValue;
        DoChange;
      end
      else if vFieldDef.Kind = fkObject then
      begin
        vEntityValue := nil;
        vEntities := TEntityList.Create(vInteractor.Domain, vInteractor.Session);
        try
          vEntityField := TEntityField(vField);
          vEntityField.GetEntitiesForSelect(vInteractor.Session, vEntities);

          for i := 0 to vEntities.Count - 1 do
            if SafeDisplayName(vEntities[i]) = AValue then
            begin
              vEntityValue := vEntities[i];
              Break;
            end;
        finally
          FreeAndNil(vEntities);
        end;

        TUserSession(vInteractor.Session).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
        begin
          vEntity._SetFieldEntity(AHolder, vFieldName, vEntityValue);
          Result := True;
        end, TChangeHolder(FEditor.Holder));
      end;

    except
      on E: Exception do
        if Assigned(vEntity) then
          TDomain(vEntity.Domain).Logger.AddMessage('Ошибка обновления данных в модели. Поле [' + vFieldName + ']');
    end;
  finally
    FIsLoading := False;
  end;
end;

procedure TUserDataSource.Update;
begin
  DataController.ClearSelection;
  DataChanged;
end;

{ TColumnListEditor }

procedure TColumnListEditor.BeforeContextMenuShow(Sender: TObject);
var
  vMenu: TPopupMenu;
  vMenuItem: TMenuItem;
  vArea: TUIArea;
  vParams: TEntity;
  i: Integer;
begin
  vMenu := TPopupMenu(Sender);
  if vMenu.Items.Count <= 0 then
    Exit;

  for i := 0 to vMenu.Items.Count - 1 do
  begin
    vMenuItem := vMenu.Items[i];
    vArea := TUIArea(vMenuItem.Tag);
    if not Assigned(vArea) then
      Continue;

    if not Assigned(vArea.View) then
      Continue;

    if vArea.View.DefinitionKind <> dkAction then
      Continue;

    if vArea.View.Name = '#GroupByColumn' then
    begin
      vParams := TEntity(vArea.View.DomainObject);
      vMenuItem.Checked := FMasterTableView.OptionsView.GroupByBox;
      if vParams['IsChecked'] <> vMenuItem.Checked then
        vParams._SetFieldValue(FSession.NullHolder, 'IsChecked', vMenuItem.Checked);
    end;
  end;

  vMenuItem := vMenu.Items[vMenu.Items.Count - 1];
  vMenuItem.Caption := TInteractor(Interactor).Translate('txtRecordCount', 'Записей') + ': ' + IntToStr(FAllData.Count);
end;

procedure TColumnListEditor.GetPropertiesForEdit(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  var AProperties: TcxCustomEditProperties);
var
  vColumnBinding: TColumnBinding;
  vEntity: TEntity;
  vFieldName: string;
  vField: TBaseField;
  vFieldDef: TFieldDef;
  vProps: TcxCustomEditProperties;

  function GetProps(const AFieldDef: TFieldDef): TcxCustomEditProperties;
  var
    vFieldKind: TFieldKind;
    vEditRepositoryItem: TcxEditRepositoryItem;
    vName: string;
    vEntityField: TEntityField;
    i: Integer;
    vInteractor: TInteractor;
    vEntities: TEntityList;
  begin
    if not Assigned(FEditRepository) then
      FEditRepository := TcxEditRepository.Create(nil);

    vFieldKind := AFieldDef.Kind;
    vName := vFieldDef.FullName.Replace('.', '_');// + IntToStr(Ord(AFieldKind));
    if vFieldDef.Kind = fkObject then
    begin
      vEntityField := TEntityField(vField);
      if Assigned(vEntityField.ContentDefinition) then
      begin
        if vEntityField.ContentDefinition.Name <> '~' then
          vName := vName + '_' + vEntityField.ContentDefinition.Name
        else begin
          vEntityField.RestoreContentDefinition;
          if (vEntityField.ContentDefinition.Name = '~') and Assigned(vEntityField.Entity) then
            vEntityField.SetContentDefinition(Holder, vEntityField.Entity.Definition);
          if (vEntityField.ContentDefinition.Name <> '~') then
            vName := vName + '_' + vEntityField.ContentDefinition.Name;
        end;
      end;
    end;

    vEditRepositoryItem := FEditRepository.ItemByName(vName);
    if not Assigned(vEditRepositoryItem) then
    begin
      if vFieldKind = fkInteger then
      begin
        vEditRepositoryItem := FEditRepository.CreateItem(TcxEditRepositorySpinItem);
        TcxEditRepositorySpinItem(vEditRepositoryItem).Properties.ValueType := vtInt;
        TcxEditRepositorySpinItem(vEditRepositoryItem).Properties.Alignment.Horz := taRightJustify;
        if AFieldDef.Format <> '' then
          TcxEditRepositorySpinItem(vEditRepositoryItem).Properties.DisplayFormat := GetDisplayFormat(AFieldDef, FView.ParentDomainObject as TEntity);
      end
      else if vFieldKind in [fkFloat, fkCurrency] then
      begin
        vEditRepositoryItem := FEditRepository.CreateItem(TcxEditRepositorySpinItem);
        TcxEditRepositorySpinItem(vEditRepositoryItem).Properties.ValueType := vtFloat;
        TcxEditRepositorySpinItem(vEditRepositoryItem).Properties.Alignment.Horz := taRightJustify;
        if AFieldDef.Format <> '' then
          TcxEditRepositorySpinItem(vEditRepositoryItem).Properties.DisplayFormat := GetDisplayFormat(AFieldDef, FView.ParentDomainObject as TEntity);
      end
      else if vFieldKind = fkBoolean then
      begin
        vEditRepositoryItem := FEditRepository.CreateItem(TcxEditRepositoryCheckBoxItem);
        TcxEditRepositoryCheckBoxItem(vEditRepositoryItem).Properties.ImmediatePost := True;
      end
      else if vFieldKind = fkObject then
      begin
        vEditRepositoryItem := FEditRepository.CreateItem(TcxEditRepositoryComboBoxItem);
        vInteractor := TInteractor(FView.Interactor);
        vEntities := TEntityList.Create(vInteractor.Domain, vInteractor.Session);
        try
          vEntityField := TEntityField(vField);

          vEntityField.GetEntitiesForSelect(vInteractor.Session, vEntities);

          TcxComboBoxProperties(TcxEditRepositoryComboBoxItem(vEditRepositoryItem).Properties).Items.Clear;
          for i := 0 to vEntities.Count - 1 do
            TcxComboBoxProperties(TcxEditRepositoryComboBoxItem(vEditRepositoryItem).Properties).Items.AddObject(SafeDisplayName(vEntities[i]), vEntities[i]);

        finally
          FreeAndNil(vEntities);
        end;
      end
    end;

    Result := nil;
    if Assigned(vEditRepositoryItem) then
    begin
      vEditRepositoryItem.Name := vName;
      Result := vEditRepositoryItem.Properties;
    end;
  end;
begin
  vColumnBinding := TColumnBinding(Sender.DataBinding.Data);
  vFieldName := vColumnBinding.FieldName;

  vEntity := FMasterDS.Data[ARecord.RecordIndex];

  vField := vEntity.FieldByName(vFieldName);
  if Assigned(vField) then
    vFieldDef := vField.FieldDef
  else
    vFieldDef := vColumnBinding.FieldDef;

  if vFieldDef.Kind in [fkInteger, fkFloat, fkCurrency, fkBoolean, fkObject] then
  begin
    vProps := GetProps(vFieldDef);
    if Assigned(vProps) then
      AProperties := vProps;
  end;
end;

procedure TColumnListEditor.CreateColumn(const AFieldDef: TFieldDef; const AFieldName: string;
  const AOverriddenCaption: string; const AWidth: Integer; const ASortOrder: TSortOrder; const AAgg: TAggregationKind);
var
  vCol: TcxGridColumn;
  vColumnBinding: TColumnBinding;
  vComboItem: TcxImageComboBoxItem;
  i: Integer;
  vDefinition: TDefinition;
  vImageID: Integer;
  vStyleName: string;
begin
  vCol := FMasterTableView.CreateColumn;
  vCol.Options.Editing := AFieldDef.UIState > vsReadOnly;
  vCol.Caption := AOverriddenCaption;
  vColumnBinding := FMasterDS.AddColumn(AFieldName, AFieldDef);
  vCol.DataBinding.Data := vColumnBinding;
  vColumnBinding.FColumn := vCol;

  if AFieldDef.Definition.Kind = clkMixin then
    vCol.OnGetProperties := GetPropertiesForEdit;

  if Assigned(AFieldDef) then
  begin
    vCol.Name := vColumnBinding.SanitizedFieldName;
    if AFieldDef.Kind in [fkInteger, fkFloat, fkCurrency] then
    begin
      vStyleName := GetUrlCommand(AFieldDef.StyleName);

      if vStyleName = 'progress' then
      begin
        vCol.PropertiesClass := TcxProgressBarProperties;
        //TcxProgressBarProperties(vCol.Properties).BarStyle := cxbsAnimation;
        TcxProgressBarProperties(vCol.Properties).Max := TSimpleFieldDef(AFieldDef).MaxValue;
      end
      else
      begin
        vCol.PropertiesClassName := 'TcxSpinEditProperties';   // need for Excel export

        if AFieldDef.Kind in [fkFloat, fkCurrency] then
          TcxSpinEditProperties(vCol.Properties).ValueType := vtFloat;

        if Length(AFieldDef.Format) > 0 then
          TcxSpinEditProperties(vCol.Properties).DisplayFormat := GetDisplayFormat(AFieldDef, FView.ParentDomainObject as TEntity);

        if not VarIsNull(TSimpleFieldDef(AFieldDef).MinValue) then
          TcxSpinEditProperties(vCol.Properties).MinValue := TSimpleFieldDef(AFieldDef).MinValue;
        if not VarIsNull(TSimpleFieldDef(AFieldDef).MaxValue) then
          TcxSpinEditProperties(vCol.Properties).MaxValue := TSimpleFieldDef(AFieldDef).MaxValue;

        //TcxSpinEditProperties(vCol.Properties).OnChange := OnColumnChange;
        FMasterTableView.OnInitEdit := OnInitEdit;
      end;

      vCol.HeaderAlignmentHorz := taRightJustify;
      vCol.FooterAlignmentHorz := taRightJustify;
      vCol.Properties.Alignment.Horz := taRightJustify;
    end
    else if AFieldDef.Kind = fkDateTime then
    begin
      if AFieldDef.StyleName = 'time' then
        vCol.PropertiesClassName := 'TcxTimeEditProperties'
      else
      begin
        vCol.PropertiesClass := TcxDateEditProperties;  // need for Excel export
        TcxDateEditProperties(vCol.Properties).DisplayFormat := GetUrlParam(AFieldDef.StyleName, 'format');
      end;
    end
    else if AFieldDef.Kind = fkBoolean then
      vCol.PropertiesClassName := 'TcxCheckBoxProperties'
    else if AFieldDef.Kind = fkColor then
    begin
      vCol.PropertiesClassName := 'TdxColorEditProperties';
      TdxColorEditProperties(vCol.Properties).ColorPalette := TdxColorPalette.cpExtended;
      TdxColorEditProperties(vCol.Properties).ColorSet := TdxColorSet.csDefault;
    end
    else if AFieldDef.Kind = fkEnum then
    begin
      vCol.PropertiesClass := TcxComboBoxProperties;

      TcxComboBoxProperties(vCol.Properties).DropDownListStyle := lsFixedList;
      FillEnumList(TDomain(FView.Domain), TcxComboBoxProperties(vCol.Properties).Items, vCol);
      if AFieldDef.StyleName = 'line_style' then
      begin
        TcxComboBoxProperties(vCol.Properties).OnDrawItem := OnDrawEnumItem_LineStyle;
        TcxComboBoxProperties(vCol.Properties).OnDrawItem := nil;//todo: пока отключил, нужно переопределить отрисовку для ячейки в гриде, иначе в гриде только текст рисуется
      end;
    end;

    vCol.SortOrder := TcxDataSortOrder(ASortOrder);
    vCol.Styles.OnGetContentStyle := OnGetContentStyle;
  end
  else
  begin
    vCol.PropertiesClass := TcxImageComboBoxProperties;
    TcxImageComboBoxProperties(vCol.Properties).Images := TDragImageList(TInteractor(FView.Interactor).Images[16]);
    for i := 0 to FAllData.ContentDefinitions.Count - 1 do
    begin
      vDefinition := TDefinition(FAllData.ContentDefinitions[i]);
      vComboItem := TcxImageComboBoxProperties(vCol.Properties).Items.Add;
      vImageID := GetImageID(vDefinition._ImageID);
      vComboItem.Value := vImageID;
      vComboItem.ImageIndex := vImageID;
    end;
  end;

  ShowFooter(vCol, AAgg);
end;

procedure TColumnListEditor.CreateColumnsFromModel(const AFields: string = '');
var
  i: Integer;
  vFieldDef: TFieldDef;
  vWidth: Integer;
  vAggType: TAggregationKind;
  vMainDefinition: TDefinition;
  vFields: TStrings;
  vFieldName: string;
begin
  EnableColumnParamsChangeHandlers(False);
  FMasterTableView.BeginUpdate;

  try
    vMainDefinition := FAllData.MainDefinition;
    if AFields = '' then
    begin
      for vFieldDef in vMainDefinition.Fields do
      begin
        if TInteractor(FView.Interactor).NeedSkipColumn(FAllData, vFieldDef) then
          Continue;

        vWidth := TWinVCLPresenter(TInteractor(FView.Interactor).Presenter).GetWidthByType(100, vFieldDef);
        { TODO -cUI refactoring : Перенести описание аггрегаций в слой описания UI }
        vAggType := vMainDefinition.Reductions.ReductionForField(vFieldDef.Name);
        CreateColumn(vFieldDef, vFieldDef.Name, GetFieldTranslation(vFieldDef), vWidth, uConsts.soNone, vAggType);
      end;

      //if FAllData.ContentDefinitions.Count > 1 then
      //  CreateColumn(nil, '', 50, uConsts.soNone, akNotDefined);
    end
    else begin
      vFields := CreateDelimitedList(AFields);
      try
        for i := 0 to vFields.Count - 1 do
        begin
          vFieldName := vFields[i];
          vFieldDef := vMainDefinition.ExtractFieldDef(vFieldName);
          if not Assigned(vFieldDef) then
            Continue;

          vWidth := TWinVCLPresenter(Presenter).GetWidthByType(100, vFieldDef);
          vAggType := TDefinition(vFieldDef.Definition).Reductions.ReductionForField(vFieldName);

          CreateColumn(vFieldDef, vFieldName, GetFieldTranslation(vFieldDef), vWidth, uConsts.soNone, vAggType);
        end;
      finally
        vFields.Free;
      end;
    end;
  finally
    FMasterTableView.EndUpdate;
    EnableColumnParamsChangeHandlers(True);
  end;
end;

destructor TColumnListEditor.Destroy;
begin
  FreeAndNil(FEditRepository);
  inherited;
end;

procedure TColumnListEditor.DoBeforeFreeControl;
begin
  if Assigned(FMasterTableView.PopupMenu) then
  begin
    TPopupMenu(FMasterTableView.PopupMenu).OnPopup := nil;
    FMasterTableView.PopupMenu := nil;
  end;

  FAllData := nil;

  FMasterTableView.OnFocusedRecordChanged := nil;
  FreeAndNil(FMasterTableView);
  FreeAndNil(FMasterDS);
  FreeAndNil(FBGStyle);
  FreeAndNil(FHeaderStyle);
  FreeAndNil(FContentStyle);
  FreeAndNil(FSelectionStyle);
end;

function TColumnListEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vFields: string;
  vListField: TListField;
begin
  FBGStyle := TcxStyle.Create(nil);
  FHeaderStyle := TcxStyle.Create(nil);
  FContentStyle := TcxStyle.Create(nil);
  FSelectionStyle := TcxStyle.Create(nil);
  FMasterDS := TUserDataSource.Create(Self);

  FMasterTableView := TcxGridTableView.Create(nil);
  FMasterTableView.DataController.OnCompare := DoOnCompare;
  FMasterTableView.OptionsData.Editing := False;
  FMasterTableView.OptionsData.Deleting := False;
  FMasterTableView.OptionsData.Inserting := False;
  FMasterTableView.OptionsCustomize.ColumnsQuickCustomization := True;
  FMasterTableView.OptionsSelection.CellSelect := False;
  FMasterTableView.OptionsSelection.MultiSelect := True;
  FMasterTableView.OptionsView.GroupByBox := False;
  vListField := AParent.View.ExtractListField;
  if Assigned(vListField) and (vListField.SortType <> estUserSort) then
    FMasterTableView.OptionsCustomize.ColumnSorting := False;

  //FMasterTableView.OptionsView.Header := TObjectFieldDef(FFieldDef).SortType <> estSortByOrder;
  if StrToBoolDef(TDomain(TInteractor(FView.Interactor).Domain).UserSettings.GetValue('Core', 'MouseMultiSelectInGrids'), False) then
  begin
    FMasterTableView.OptionsSelection.CheckBoxVisibility := [cbvDataRow, cbvColumnHeader];
    FMasterTableView.OptionsSelection.ShowCheckBoxesDynamically := True;
  end
  else
  begin
    FMasterTableView.OptionsSelection.CheckBoxVisibility := [];
    FMasterTableView.OptionsSelection.ShowCheckBoxesDynamically := False;
  end;

  if StrToBoolDef(TDomain(TInteractor(FView.Interactor).Domain).UserSettings.GetValue('Core', 'ShowHorzLines'), True) then
    FMasterTableView.OptionsView.GridLines := glBoth
  else
    FMasterTableView.OptionsView.GridLines := glVertical;

  FMasterTableView.OptionsBehavior.CellHints := True;
  FMasterTableView.DataController.CustomDataSource := FMasterDS;
  FMasterTableView.OnCellDblClick := DoOnCellDblClick;
//  FMasterTableView.OnKeyDown := OnFilterKeyDown;
  FMasterTableView.OnFocusedRecordChanged := DoOnFocusedRecordChanged;
  FMasterTableView.DataController.Summary.Options := [soSelectedRecords, soMultipleSelectedRecords];
  FMasterTableView.Styles.Background := FBGStyle;
  FMasterTableView.Styles.Header := FHeaderStyle;
  FMasterTableView.Styles.Content := FContentStyle;
  FMasterTableView.Styles.Selection := FSelectionStyle;
  FMasterTableView.Styles.Inactive := FSelectionStyle;

  if Assigned(ALayout) and (ALayout.Control is TPanel) then
  begin
    FContentStyle.TextColor := TPanel(ALayout.Control).Font.Color;
    FSelectionStyle.TextColor := clHighlightText;
    FSelectionStyle.Color := clHighlight;
  end;

  FGrid := TcxGrid.Create(nil);
  Result := FGrid;

  FGrid.LookAndFeel.NativeStyle := False;
  FGrid.LookAndFeel.Kind := lfFlat;

  FGrid.Align := alClient;
  FGrid.Levels.Add.GridView := FMasterTableView;
  FGrid.Font.Size := 12;

  cxSetResourceString(@scxGridGroupByBoxCaption, TInteractor(Interactor).Translate('txtMoveColumnForGrouping',
    'Перетащите сюда колонку для группировки'));
  FMasterTableView.OptionsView.NoDataToDisplayInfoText := TInteractor(Interactor).Translate('@Grid@NoDataText', '<Нет данных>');
  FAllData := TEntityList(FView.DomainObject);

  FBGStyle.Color := GetBGColor(FAllData.MainDefinition, clWindow);
  FHeaderStyle.Font.Size := 10;
  FHeaderStyle.TextColor := clGray;

  if Assigned(FCreateParams) then
  begin
    vFields := FCreateParams.Values['fields'];
    if FCreateParams.Values['editingInGrid'] = 'true' then
    begin
      FMasterTableView.OptionsData.Editing := True;
      FMasterTableView.OptionsSelection.CellSelect := True;
    end;
  end
  else
    vFields := '';
  if vFields = '' then
    vFields := AParent.QueryParameter('fields');

  CreateColumnsFromModel(vFields);
  LoadColumnWidths;
end;

procedure TColumnListEditor.DoOnCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
  AShift: TShiftState; var AHandled: Boolean);
begin
  if AButton = mbLeft then
    OnTableViewDblClick(FView, Self);
end;

procedure TColumnListEditor.DoOnColumnPosChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
begin
  SaveColumnWidths;
end;

procedure TColumnListEditor.DoOnColumnSizeChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
begin
  SaveColumnWidths;
end;

procedure TColumnListEditor.DoOnCompare(ADataController: TcxCustomDataController; ARecordIndex1, ARecordIndex2,
  AItemIndex: Integer; const V1, V2: Variant; var Compare: Integer);
var
  vSortColumn: TcxGridColumn;
  vColumnBinding: TColumnBinding;
begin
  if (ARecordIndex1 < 0) or (ARecordIndex2 < 0) then
  begin
    Compare := 0;
    Exit;
  end;

  vSortColumn := FMasterTableView.Columns[AItemIndex];
  vColumnBinding := TColumnBinding(vSortColumn.DataBinding.Data);
  Compare := CompareEntities(FMasterDS.Data[ARecordIndex1], FMasterDS.Data[ARecordIndex2],
    vColumnBinding.FieldDef, vColumnBinding.FieldName);
end;

procedure TColumnListEditor.DoOnFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord,
  AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
var
  vEntityList: TEntityList;
begin
  if APrevFocusedRecord = AFocusedRecord then
    Exit;

  vEntityList := TEntityList(TView(FView).DomainObject);
  if not Assigned(AFocusedRecord) then
//    vEntityList.SelectEntity(nil)
  else begin
    vEntityList.SelectEntity(TEntity(FMasterDS.Data[AFocusedRecord.RecordIndex]));
  end;
end;

procedure TColumnListEditor.DoOnHeaderClick(Sender: TObject);
begin
  SaveColumnWidths;
end;

procedure TColumnListEditor.EnableColumnParamsChangeHandlers(const AEnable: Boolean);
begin
  if AEnable then
  begin
    FMasterTableView.OnColumnPosChanged := DoOnColumnPosChanged;
    FMasterTableView.OnColumnSizeChanged := DoOnColumnSizeChanged;
    FMasterTableView.DataController.OnSortingChanged := DoOnHeaderClick;
  end
  else
  begin
    FMasterTableView.OnColumnPosChanged := nil;
    FMasterTableView.OnColumnSizeChanged := nil;
    FMasterTableView.DataController.OnSortingChanged := nil;
  end;
end;

procedure TColumnListEditor.LoadColumnWidths;
begin
  FLayoutExists := LoadGridColumnWidths(TInteractor(FView.Interactor).Domain, FMasterTableView, FFieldDef.FullName);
end;

procedure TColumnListEditor.OnDrawEnumItem_LineStyle(AControl: TcxCustomComboBox; ACanvas: TcxCanvas; AIndex: Integer; const ARect: TRect;
  AState: TOwnerDrawState);
var
  vID: Integer;
begin
  ACanvas.FillRect(ARect);
  ACanvas.Pen.Color := ACanvas.Font.Color;
  ACanvas.Pen.Width := 1;

  vID := AIndex;
  if FFieldDef.HasFlag(cRequired) then
    vID := vID + 1;

  ACanvas.Pen.Style := Graphics.TPenStyle(vID);

  ACanvas.MoveTo(ARect.Left + 8, ARect.CenterPoint.Y);
  ACanvas.LineTo(ARect.Right - 8, ARect.CenterPoint.Y);
end;

procedure TColumnListEditor.OnExitFromInplaceEditor(Sender: TObject);
var
  vEntityList: TEntityList;
  vView: TView;
  vEntity: TEntity;
  vColumn: TcxGridColumn;
  vBinding: TColumnBinding;
begin
  if not (Sender is TcxCustomInnerTextEdit) then
    Exit;

  vEntityList := TEntityList(FView.DomainObject);
  if not Assigned(vEntityList) then
    Exit;

  vEntity := TEntity(vEntityList.Selected);
  if not Assigned(vEntity) then
    Exit;

  vColumn := FMasterTableView.Controller.FocusedColumn;
  if not Assigned(vColumn) then
    Exit;

  vBinding := TColumnBinding(vColumn.DataBinding.Data);
  if not Assigned(vBinding) then
    Exit;

  vView := FView.BuildView('Selected/' + vBinding.FieldDef.Name);
  if not Assigned(vView) then
    Exit;

  vView.AddListener(FUIBuilder.RootArea);
  try
    if not TCanChangeFieldFunc(TDomain(FView.Domain).Configuration.CanChangeFieldFunc)
      (vView, vEntity, vBinding.FieldDef.Name, StrToIntDef(TcxCustomInnerTextEdit(Sender).Text, 0)) then
    begin
      if Assigned(FMasterTableView.Controller) and Assigned(FMasterTableView.Controller.EditingController) then
      begin
        // Для того, чтобы откатить изменения, сделанные через UpDown
        FMasterTableView.Controller.EditingController.HideEdit(False);
        // Для того, чтобы откатить изменения, сделанные прямым набором текста
        vEntity._RestoreFieldValue(Holder, vBinding.FieldDef.Name);
      end;
      Exit;
    end;
  finally
    vView.RemoveListener(FUIBuilder.RootArea);
  end;
end;

procedure TColumnListEditor.OnGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
begin
  GetContentStyle(Self, FAllData, FAllData.MainDefinition.ColorFieldName, FMasterDS.Data, Sender, ARecord, AItem, AStyle);
end;

type
  TWinControlAccess = class(TWinControl);

procedure TColumnListEditor.OnInitEdit(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem;
  AEdit: TcxCustomEdit);
begin
  if Assigned(AEdit.InnerControl) then
    TWinControlAccess(AEdit.InnerControl).OnExit := OnExitFromInplaceEditor;
end;

procedure TColumnListEditor.SaveColumnWidths;
begin
  SaveGridColumnWidths(TInteractor(FView.Interactor).Domain, FMasterTableView, FFieldDef.FullName);
end;

procedure TColumnListEditor.SetPopupArea(const APopupArea: TUIArea);
var
  vPopupMenu: TPopupMenu;
begin
  vPopupMenu := TPopupMenu(APopupArea.InnerControl);
  vPopupMenu.OnPopup := BeforeContextMenuShow;
  FMasterTableView.PopupMenu := vPopupMenu;
end;

procedure TColumnListEditor.ShowFooter(const AColumn: TcxGridColumn; const AAgg: TAggregationKind);
begin
  if AAgg = akNotDefined then Exit;
  FMasterTableView.OptionsView.Footer := True;

  if AAgg = akCount then
    AColumn.Summary.FooterKind := skCount
  else if AAgg = akSum then
  begin
    AColumn.Summary.FooterKind := skSum;
    AColumn.Summary.FooterFormat := '0.00';
  end
  else if AAgg = akAverage then
    AColumn.Summary.FooterKind := skAverage;
end;

procedure TColumnListEditor.UpdateArea(const AKind: Word; const AParameter: TEntity);
var
  i: Integer;
  vEntity: TEntity;
  vNewFocusedRowIndex: Integer;
  vRow: TcxCustomGridRow;
begin
  if AKind = dckEntityChanged then
  begin
    FMasterTableView.OnFocusedRecordChanged := nil;
    try
      if not FMasterDS.IsLoading then
        FMasterTableView.Invalidate(True) //CustomDataSource.DataChanged
    finally
      FMasterTableView.OnFocusedRecordChanged := DoOnFocusedRecordChanged;
    end;
  end
  // Нужно отключать dckEntityChanged
  //else if AKind = dckEntitySaved then
  //  FMasterTableView.DataController.CustomDataSource.DataChanged
  else if AKind = dckListAdded then
  begin
    TUserDataSource(FMasterTableView.DataController.CustomDataSource).Add(AParameter, not TDomain(Domain).LoadingChanges);
  end
  else if AKind = dckListRemoved then
  begin
    TUserDataSource(FMasterTableView.DataController.CustomDataSource).Remove(AParameter, not TDomain(Domain).LoadingChanges);
  end
  else if (AKind = dckSelectionChanged) or (AKind = dckListScrollUpdate)  then
  begin
    if AKind = dckListScrollUpdate then
      FMasterTableView.Controller.ClearSelection;
    vNewFocusedRowIndex := FMasterDS.FEntities.IndexOf(AParameter);
   // if vNewFocusedRowIndex <> FMasterTableView.DataController.FocusedRowIndex then //отключил проверку чтобы всегда скролилось
    begin
      vRow := FMasterTableView.ViewData.Rows[vNewFocusedRowIndex];
      vRow.Selected := True;
      vRow.Focused := True;
    end;
  end
  else if AKind = dckViewStateChanged then
    // Сделать что-то с видимостью
  else begin
    FMasterTableView.BeginUpdate;
    try
      FMasterDS.Data.Clear;

      FAllData.Resort;
      for i := 0 to FAllData.Count - 1 do
      begin
        vEntity := FAllData[i];
        if (not vEntity.Deleted) {IsMatchToFilter(vEnt))} then
          FMasterDS.Data.Add(vEntity);
      end;

      FMasterDS.Update;
    finally
      FMasterTableView.EndUpdate;
      FMasterTableView.DataController.ClearSelection;
      FMasterTableView.DataController.FocusedRecordIndex := -1;
    end;

    if (FAllData.Count > 0) and (FAllData.Selection.Count = 0) then
      FAllData.SelectEntity(FAllData[0]);
   // TODO Разобраться
   // if not FLayoutExists then
   //   FMasterTableView.ApplyBestFit;
  end;
end;

{ TParametersEditor }

procedure TParametersEditor.AddRow(const AName: string; const AValue: Variant; const AType: TEntity);
var
  vRow: TcxEditorRow;
begin
  vRow := TcxEditorRow(FGrid.Add(TcxEditorRow));
  vRow.Properties.Caption := AName;

  case SafeID(AType) of
    1: vRow.Properties.EditPropertiesClassName := 'TcxTextEditProperties';
    2: vRow.Properties.EditPropertiesClassName := 'TcxSpinEditProperties';
    3: begin
        vRow.Properties.EditPropertiesClassName := 'TcxSpinEditProperties';
        TcxSpinEditProperties(vRow.Properties.EditProperties).ValueType := cxSpinEdit.vtFloat;
      end;
    4: vRow.Properties.EditPropertiesClassName := 'TcxDateEditProperties';
    5: vRow.Properties.EditPropertiesClassName := 'TcxCheckBoxProperties';
    6: vRow.Properties.EditPropertiesClassName := 'TcxTextEditProperties'; // Combobox
  else
    Assert(False, 'unsupported type ' + AType['Name'] + ' for variable: ' + AName);
  end;

  vRow.Properties.EditProperties.OnEditValueChanged := OnValueChanged;
  vRow.Properties.Value := AValue;
end;

procedure TParametersEditor.ClearRows;
begin
  FGrid.ClearRows;
end;

procedure TParametersEditor.DoBeforeFreeControl;
begin
  FreeAndNil(FParams);
  inherited;
end;

function TParametersEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FGrid := TcxVerticalGrid.Create(nil);
  Result := FGrid;
  FGrid.Width := 400;

  FParams := TList<TEntity>.Create;
end;

procedure TParametersEditor.FillEditor;
var
  vParameter: TEntity;
  vGridHeight, i: Integer;
begin
  ClearRows;
  FParams.Clear;
  for vParameter in FView.ExtractListField do
    if SafeID(vParameter.ExtractEntity('Destination')) = 2 then
    begin
      FParams.Add(vParameter);
      AddRow(vParameter['Caption'], vParameter['Value'], vParameter.ExtractEntity('Type'));
    end;

  vGridHeight := 0;
  for i := 0 to FGrid.Rows.Count - 1 do
    vGridHeight := vGridHeight + FGrid.ViewInfo.CalcRowHeight(FGrid.Rows[i]) + FGrid.ViewInfo.DividerWidth;

  FGrid.Height := FGrid.Margins.Top + FGrid.Margins.Bottom + vGridHeight + 4;

  FGrid.OptionsView.RowHeaderWidth := FGrid.Width div 2;
end;

procedure TParametersEditor.OnValueChanged(Sender: TObject);
var
  vRow: TcxEditorRow;
  vIndex: Integer;
  vValue: Variant;
begin
  vRow := TcxEditorRow(FGrid.FocusedRow);
  if not Assigned(vRow) then
    Exit;

  vIndex := FGrid.Rows.IndexOf(vRow);
  if vIndex >= 0 then
  begin
    case SafeID(FParams[vIndex].ExtractEntity('Type')) of
      1: vValue := TcxTextEdit(Sender).EditingValue;
      2: vValue := TcxSpinEdit(Sender).EditingValue;
      3: vValue := TcxSpinEdit(Sender).EditingValue;
      4: vValue := TcxDateEdit(Sender).EditingValue;
      5: vValue := TcxCheckBox(Sender).EditingValue;
    else
      vValue := Null;
    end;

    vRow.Properties.Value := vValue;
    FParams[vIndex]._SetFieldValue(Holder, 'Value', vValue)
  end
  else
    ShowMessage('Wrong index');
end;

procedure TParametersEditor.UpdateArea(const AKind: Word; const AParameter: TEntity);
begin
  if AKind <> dckEntityChanged then
    FillEditor;
end;

{ TColumnBinding }

constructor TColumnBinding.Create(const AFieldName: string; const AFieldDef: TFieldDef);
begin
  inherited Create;
  FFieldName := AFieldName;
  FSanitizedFieldName := StringReplace(AFieldName, '.', '__', [rfReplaceAll]);
  FFieldDef := AFieldDef;
  FAfterPoint := 4;
  {if (FFieldDef.Kind in [fkFloat, fkCurrency]) and (Length(FFieldDef.Format) > 0) then
  begin
    vPos := Pos('.', FFieldDef.Format);
    if vPos = 0 then
      vPos := Pos(',', FFieldDef.Format);
    FAfterPoint := Length(FFieldDef.Format) - vPos;
  end;}
end;

{ TPivotGrid }

function TPivotGrid.CreatePivotField(const AField: TCubeField; const AWidth: Integer = 150): TcxPivotGridField;
begin
  FMasterDS.AddColumn(AField.Field.Name, AField.Field);

  Result := FPivot.CreateField;
  Result.Width := AWidth;

  Result.Caption := AField.Caption;
  case AField.ReductionKind of
    akCount: Result.SummaryType := TcxPivotGridSummaryType.stCount;
    akSum: Result.SummaryType := TcxPivotGridSummaryType.stSum;
    akAverage: Result.SummaryType := TcxPivotGridSummaryType.stAverage;
    akMin: Result.SummaryType := TcxPivotGridSummaryType.stMin;
    akMax: Result.SummaryType := TcxPivotGridSummaryType.stMax;
  else
    Result.SummaryType := TcxPivotGridSummaryType.stCustom
  end;

  Result.Visible := AField.Visible;
  case AField.AreaKind of
    cakRow: Result.Area := TcxPivotGridFieldArea.faRow;
    cakColumn: Result.Area := TcxPivotGridFieldArea.faColumn;
    cakData: Result.Area := TcxPivotGridFieldArea.faData;
    cakFilter: Result.Area := TcxPivotGridFieldArea.faFilter;
  else
    Result.Visible := False;
  end;

  if SameText(AField.PeriodReduction, 'Year') then
    Result.GroupInterval := TcxPivotGridGroupInterval.giDateYear
  else if SameText(AField.PeriodReduction, 'Quarter') then
    Result.GroupInterval := TcxPivotGridGroupInterval.giDateQuarter
  else if SameText(AField.PeriodReduction, 'Month') then
    Result.GroupInterval := TcxPivotGridGroupInterval.giDateMonth
  else
    Result.GroupInterval := TcxPivotGridGroupInterval.giDefault;

  if AField.Field.Kind = fkInteger then
    Result.DataBinding.ValueType := 'Integer'
  else if AField.Field.Kind = fkCurrency then
    Result.DataBinding.ValueType := 'Currency'
  else if AField.Field.Kind = fkFloat then
    Result.DataBinding.ValueType := 'Float'
  else if AField.Field.Kind = fkDateTime then
    Result.DataBinding.ValueType := 'DateTime';
end;

procedure TPivotGrid.BeforeContextMenuShow(Sender: TObject);
var
  vMenu: TPopupMenu;
  vMenuItem: TMenuItem;
  vArea: TUIArea;
  //vParams: TEntity;
  i: Integer;
begin
  vMenu := TPopupMenu(Sender);
  if vMenu.Items.Count <= 0 then
    Exit;

  for i := 0 to vMenu.Items.Count - 1 do
  begin
    vMenuItem := vMenu.Items[i];
    vArea := TUIArea(vMenuItem.Tag);
    if not Assigned(vArea) then
      Continue;

    if not Assigned(vArea.View) then
      Continue;

    if vArea.View.DefinitionKind <> dkAction then
      Continue;

    {if vArea.View.Name = '#GroupByColumn' then
    begin
      vParams := TEntity(vArea.View.DomainObject);
      vMenuItem.Checked := FMasterTableView.OptionsView.GroupByBox;
      if vParams['IsChecked'] <> vMenuItem.Checked then
        vParams._SetFieldValue(FSession.NullHolder, 'IsChecked', vMenuItem.Checked);
    end;}
  end;

  vMenuItem := vMenu.Items[vMenu.Items.Count - 1];
  vMenuItem.Caption := TInteractor(Interactor).Translate('txtRecordCount', 'Записей') + ': ' + IntToStr(FAllData.Count);
end;

constructor TPivotGrid.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
  const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = '');
begin
  FPivot := TcxPivotGrid.Create(nil);
  FId := 'Pivot';

  if ALayout.Control is TPanel then
    FPivot.Align := TPanel(ALayout.Control).Align
  else
    FPivot.Align := alClient;

  FPivot.OptionsCustomize.SortingByGroupValues := True;
  FPivot.OptionsCustomize.Hiding := True;
  //FPivot.OptionsCustomize.Moving := False;
  //FPivot.OptionsCustomize.Filtering := False;
  FPivot.OptionsCustomize.QuickPrefiltering := False;

  //FPivot.OptionsView.ColumnFields := False;
  FPivot.OptionsView.RowFields := False;
  FPivot.OptionsView.DataFields := False;
  FPivot.OptionsView.FilterFields := False;
  FPivot.OptionsView.ColumnGrandTotalText := 'ИТОГО';
  FPivot.OptionsView.RowGrandTotalText := 'ИТОГО';

  cxSetResourceString(@scxDataField, 'Данные');
  cxSetResourceString(@scxDropFilterFields, 'Перетащите поля фильтров сюда');
  cxSetResourceString(@scxDropDataItems, 'Перетащите поля данных сюда');
  cxSetResourceString(@scxDropRowFields, 'Перетащите поля строк сюда');
  cxSetResourceString(@scxDropColumnFields, 'Перетащите поля колонок сюда');
  cxSetResourceString(@scxGrandTotal, 'ОБЩЕЕ ИТОГО');
  cxSetResourceString(@scxNoDataToDisplay, '<Нет данных для отображения>');
  cxSetResourceString(@scxAddTo, 'Вставить');
  cxSetResourceString(@scxDragItems, 'Доступные поля');
  cxSetResourceString(@scxFieldListCaption, 'Список полей');
  cxSetResourceString(@scxRowArea, 'Строки');
  cxSetResourceString(@scxColumnArea, 'Колонки');
  cxSetResourceString(@scxFilterArea, 'Фильтры');
  cxSetResourceString(@scxDataArea, 'Данные');
  cxSetResourceString(@scxGroupTotal, '%s Итого');
  cxSetResourceString(@scxGroupCount, '%s Кол-во');
  cxSetResourceString(@scxGroupSum, '%s Сумма');
  cxSetResourceString(@scxGroupMin, '%s Мин.');
  cxSetResourceString(@scxGroupMax, '%s Макс.');
  cxSetResourceString(@scxGroupAverage, '%s Среднее');
  cxSetResourceString(@scxGroupStdDev, '%s Ст.откл.');
  cxSetResourceString(@scxGroupStdDevP, '%s Ст.откл.P');
  cxSetResourceString(@scxGroupVariance,  '%s Вариация');
  cxSetResourceString(@scxGroupVarianceP, '%s ВариацияP');
  cxSetResourceString(@scxGroupCustom, '%s Польз.');
  cxSetResourceString(@scxOthers, 'Прочие');
  cxSetResourceString(@scxPivotGridShowAll, '(Все)');
  cxSetResourceString(@scxPivotGridOk, 'Ок');
  cxSetResourceString(@scxPivotGridCancel, 'Отмена');
  cxSetResourceString(@scxQuarterFormat, 'Кв. %d');
  cxSetResourceString(@scxHide, 'Скрыть');
  cxSetResourceString(@scxOrder,  'Порядок полей');
  cxSetResourceString(@scxMoveToBeginning, 'Переместить в начало');
  cxSetResourceString(@scxMoveToEnd, 'Переместить в конец');
  cxSetResourceString(@scxMoveToLeft, 'Сдвинуть левее');
  cxSetResourceString(@scxMoveToRight, 'Сдвинуть правее');
  cxSetResourceString(@scxExpand, 'Развернуть');
  cxSetResourceString(@scxCollapse, 'Свернуть');
  cxSetResourceString(@scxExpandAll, 'Развернуть всё');
  cxSetResourceString(@scxCollapseAll, 'Свернуть всё');
  cxSetResourceString(@scxShowCustomization, 'Показать список полей');
  cxSetResourceString(@scxHideCustomization, 'Скрыть список полей');
  cxSetResourceString(@scxShowPrefilterDialog, 'Настроить фильтрацию');
  cxSetResourceString(@scxSortGroupByThisColumn, 'Сортировать "%s" по этой колонке');
  cxSetResourceString(@scxSortGroupByThisRow, 'Сортировать "%s" по этой строке');
  cxSetResourceString(@scxRemoveAllSorting, 'Убрать все сортировки');
  cxSetResourceString(@scxPrefilterIsEmpty, '<Фильтр не задан>');
  cxSetResourceString(@scxPrefilterCustomizeButtonCaption, 'Фильтрация...');

  inherited Create(AParent, AView, AId, AIsService, FPivot, ALayout, AParams);

  FAllData := TEntityList(AView.DomainObject);
  FMasterDS := TPivotDataSource.Create;

  FCube := FAllData.MainDefinition.DataCubes.Cube[AParent.QueryParameter('Cube', '')];
  FillGrid;

  FPivot.DataController.CustomDataSource := FMasterDS;
  FPivot.OnDblClick := DoOnGridDblClick;
  FPivot.ApplyBestFit;
end;

destructor TPivotGrid.Destroy;
begin
  if Assigned(FPivot.PopupMenu) then
  begin
    TPopupMenu(FPivot.PopupMenu).OnPopup := nil;
    FPivot.PopupMenu := nil;
  end;

  FPivot.OnDblClick := nil;

  FCube := nil;
  FAllData := nil;

  FPivot.DataController.CustomDataSource := nil;
  FreeAndNil(FMasterDS);
  inherited Destroy;
end;

procedure TPivotGrid.DoExecuteUIAction(const AView: TView);
//var
//  vParams: TEntity;
//  vDateFrom, vDateTo: TDateTime;
//  vIsPeriodActive: Boolean;
//  i: Integer;
begin
  if AView.Name = '#ExportToCsv' then
    ExportToExcel
  else if AView.Name = '#ApplyBestFit' then
    FPivot.ApplyBestFit
  else if AView.Name = '#DateReduction' then
  begin
    {vParams := TEntity(AView.DomainObject);
    vParams._SetFieldValue(FSession.NullHolder, 'IsChecked', not vParams['IsChecked']);
    FMasterTableView.OptionsView.GroupByBox := vParams['IsChecked'];
    if not FMasterTableView.OptionsView.GroupByBox then // remove groups
      for i := 0 to FMasterTableView.ColumnCount - 1 do
        if FMasterTableView.Columns[i].GroupIndex >= 0 then
        begin
          FMasterTableView.Columns[i].Visible := True;
          FMasterTableView.Columns[i].GroupIndex := -1;
        end;}
  end;
end;

procedure TPivotGrid.DoOnGridDblClick(Sender: TObject);
begin
  FPivot.ApplyBestFit;
end;

procedure TPivotGrid.ExportToExcel;
  function GetFileName(out AFileName: string): Boolean;
  var
    vSaveDialog: TSaveDialog;
    vFileName: string;
  begin
    vSaveDialog := TSaveDialog.Create(nil);
    try
      vSaveDialog.Filter := '*.xls|*.xls';
      vSaveDialog.InitialDir := GetDesktopDir;

      vFileName := StringReplace(FAllData.MainDefinition.Name, '/', '_', [rfReplaceAll]);
      if Assigned(FCube) then
        vFileName := vFileName + '_' + FCube.Name + '.xls'
      else
        vFileName := vFileName + '.xls';
      vSaveDialog.FileName := vFileName;
      vSaveDialog.Options := vSaveDialog.Options + [ofOverwritePrompt];
      vSaveDialog.DefaultExt := '*.xls';

      Result := vSaveDialog.Execute;

      if Result then
        AFileName := ChangeFileExt(vSaveDialog.FileName, '.xls');

    finally
      FreeAndNil(vSaveDialog);
    end;
  end;
var
  vFileName: string;
begin
  if not GetFileName(vFileName) then Exit;

  cxExportPivotGridDataToExcel(vFileName, FPivot);

  if FileExists(vFileName) and (TPresenter(Presenter).ShowYesNoDialog('Export', 'Хотите открыть этот файл?') = drYes) then
    TPresenter(Presenter).OpenFile(vFileName);
end;

procedure TPivotGrid.FillGrid;
var
  vField: TCubeField;
  vUIField: TcxPivotGridField;
begin
  FPivot.BeginUpdate;
  try
    if Assigned(FCube) then
    begin
      for vField in FCube.Fields do
      begin
        vUIField := CreatePivotField(vField);
        vUIField.AllowedAreas := [faData, faColumn, faRow];
      end;
    end
  finally
    FPivot.EndUpdate;
  end;

  ReloadData(FCube.Filter);
end;

procedure TPivotGrid.ReloadData(const AFilter: string);
var
  vQueryDef: TQueryDef;
  vQuery: TQueryExecutor;
  i: Integer;
  vEntity: TEntity;
begin
  FMasterDS.Data.Clear;

  if AFilter <> '' then
  begin
    vQueryDef := TQueryDef.Create(FAllData.MainDefinition.Name, AFilter);
    vQuery := TQueryExecutor.Create(vQueryDef);
    try
      for i := 0 to FAllData.Count - 1 do
      begin
        vEntity := FAllData[i];
        if not vEntity.Deleted and vQuery.IsMatch(FView.Session, vEntity) then
          FMasterDS.Data.Add(vEntity);
      end;
    finally
      FreeAndNil(vQuery);
      FreeAndNil(vQueryDef);
    end;
  end
  else begin
    for i := 0 to FAllData.Count - 1 do
      begin
        vEntity := FAllData[i];
        if not vEntity.Deleted then
          FMasterDS.Data.Add(vEntity);
      end;
  end;
end;

procedure TPivotGrid.SetPopupArea(const APopupArea: TUIArea);
var
  vPopupMenu: TPopupMenu;
begin
  vPopupMenu := TPopupMenu(APopupArea.InnerControl);
  vPopupMenu.OnPopup := BeforeContextMenuShow;
  FPivot.PopupMenu := vPopupMenu;
end;

{ TPivotDataSource }

function TPivotDataSource.AddColumn(const AFieldName: string; const AFieldDef: TFieldDef): TColumnBinding;
begin
  Result := TColumnBinding.Create(AFieldName, AFieldDef);
  FColumns.Add(Result);
end;

constructor TPivotDataSource.Create;
begin
  inherited Create;
  FEntities := TList<TEntity>.Create;
  FColumns := TObjectList<TColumnBinding>.Create;
end;

destructor TPivotDataSource.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FEntities);
  inherited Destroy;
end;

function TPivotDataSource.GetRecordCount: Integer;
begin
  Result := FEntities.Count;
end;

function TPivotDataSource.GetValue(
  ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;
var
  vEntity: TEntity;
begin
  vEntity := FEntities[Integer(ARecordHandle)];
  if FColumns[Integer(AItemHandle)].FieldDef.Kind = fkEnum then
    Result := vEntity.FieldToString(FColumns[Integer(AItemHandle)].FieldName)
  else
    Result := vEntity[FColumns[Integer(AItemHandle)].FieldName];
end;

{ TTreeCollectionEditor }

procedure TTreeCollectionEditor.BeforeContextMenuShow(Sender: TObject);
begin

end;

procedure TTreeCollectionEditor.DoOnSelectionChanged(Sender: TObject);
var
  vEntityList: TEntityList;
  vList: TList<TEntity>;
begin
  if FTreeList.SelectionCount = 0 then Exit;

  vEntityList := TEntityList(TView(FView).DomainObject);
  vList := TList<TEntity>.Create;
  try
    vList.Add(GetEntity(FTreeList.Selections[0], FTreeList.Selections[0].Level));

    vEntityList.SelectEntities(vList);
  finally
    FreeAndNil(vList);
  end;
end;

constructor TTreeCollectionEditor.Create(const AParent: TUIArea; const AView: TView; const AId: string; const AIsService: Boolean = False;
  const AControl: TObject = nil; const ALayout: TLayout = nil; const AParams: string = '');
var
  vFields: string;
begin
  FDomain := TInteractor(AView.Interactor).Domain;

  FBGStyle := TcxStyle.Create(nil);
  FHeaderStyle := TcxStyle.Create(nil);

  FTreeList := TcxVirtualTreeList.Create(nil);
  FTreeList.OptionsData.Editing := False;
  FTreeList.OptionsData.Deleting := False;
  FTreeList.OptionsData.Inserting := False;
  FTreeList.OptionsCustomizing.ColumnsQuickCustomization := True;
  FTreeList.OptionsCustomizing.NodeSizing := False;
  FTreeList.OptionsCustomizing.RowSizing := False;
  FTreeList.OptionsCustomizing.BandVertSizing := False;
  FTreeList.OptionsCustomizing.BandHorzSizing := False;
  FTreeList.OptionsCustomizing.ColumnVertSizing := False;
  FTreeList.OptionsSelection.CellSelect := False;
  FTreeList.OptionsSelection.HideSelection := True;
  FTreeList.OptionsSelection.MultiSelect := False;

  if StrToBoolDef(TDomain(FDomain).UserSettings.GetValue('Core', 'ShowHorzLines'), True) then
    FTreeList.OptionsView.GridLines := tlglBoth
  else
    FTreeList.OptionsView.GridLines := tlglVert;

  FTreeList.OptionsBehavior.CellHints := True;
  FTreeList.OnDblClick := DoOnTableViewDblClick;
  FTreeList.OnSelectionChanged := DoOnSelectionChanged;
  FTreeList.OnGetChildCount := TreeListGetChildCount;
  FTreeList.OnGetNodeValue := TreeListGetNodeValue;
//  FList.OnCanSelectRecord := DoCanSelectRecord;

  FTreeList.Styles.Background := FBGStyle;
  FTreeList.Styles.ColumnHeader := FHeaderStyle;
  FTreeList.LookAndFeel.NativeStyle := False;
  FTreeList.LookAndFeel.Kind := lfFlat;

  if ALayout.Control is TPanel then
    FTreeList.Align := TPanel(ALayout.Control).Align
  else
    FTreeList.Align := alClient;

  FTreeList.Font.Size := 12;

  inherited Create(AParent, AView, AId, AIsService, FTreeList, ALayout, AParams);

  FAllData := TEntityList(FView.DomainObject);

  FBGStyle.Color := GetBGColor(FAllData.MainDefinition, clWindow);
  FHeaderStyle.Font.Size := 10;
  FHeaderStyle.TextColor := clGray;

  EnableColumnParamsChangeHandlers(False);

  vFields := AParent.QueryParameter('fields');
  CreateColumnsFromModel(vFields);
  LoadColumnWidths;
  CreateLevels(ALayout);

  EnableColumnParamsChangeHandlers(True);
end;

procedure TTreeCollectionEditor.CreateColumn(const AFieldDef: TFieldDef; const AFieldName, AOverriddenCaption: string; const AWidth: Integer;
  const ASortOrder: TSortOrder; const AAgg: TAggregationKind);
var
  vCol: TcxTreeListColumn;
  vComboItem: TcxImageComboBoxItem;
  i: Integer;
  vDefinition: TDefinition;
  vImageID: Integer;
begin
  vCol := FTreeList.CreateColumn;
  vCol.Caption.Text := AOverriddenCaption;
  vCol.DataBinding.Data := AFieldDef;

  if Assigned(AFieldDef) then
  begin
    vCol.Name := AFieldName;
    if AFieldDef.Kind in [fkInteger, fkFloat, fkCurrency] then
    begin
      vCol.PropertiesClassName := 'TcxSpinEditProperties';   // need for Excel export
//      vCol.HeaderAlignmentHorz := taRightJustify;
//      vCol.FooterAlignmentHorz := taRightJustify;
      vCol.Properties.Alignment.Horz := taRightJustify;
    end
    else if AFieldDef.Kind = fkDateTime then
      vCol.PropertiesClassName := 'TcxDateEditProperties'  // need for Excel export
    else if AFieldDef.Kind = fkBoolean then
      vCol.PropertiesClassName := 'TcxCheckBoxProperties'
    else if AFieldDef.Kind = fkColor then
      vCol.PropertiesClassName := 'TdxColorEditProperties';

//    vCol.SortOrder := TcxDataSortOrder(ASortOrder);
//    vCol.Styles.OnGetContentStyle := OnGetContentStyle;
  end
  else
  begin
    vCol.PropertiesClass := TcxImageComboBoxProperties;
    TcxImageComboBoxProperties(vCol.Properties).Images := TDragImageList(TInteractor(FView.Interactor).Images[16]);
    for i := 0 to FAllData.ContentDefinitions.Count - 1 do
    begin
      vDefinition := TDefinition(FAllData.ContentDefinitions[i]);
      vComboItem := TcxImageComboBoxProperties(vCol.Properties).Items.Add;
      vImageID := GetImageID(vDefinition._ImageID);
      vComboItem.Value := vImageID;
      vComboItem.ImageIndex := vImageID;
    end;
  end;

//  ShowFooter(vCol, AAgg);
end;

procedure TTreeCollectionEditor.CreateColumnsFromModel(const AFields: string);
var
  i: Integer;
  vFieldDef: TFieldDef;
  vWidth: Integer;
  vAggType: TAggregationKind;
  vMainDefinition: TDefinition;
  vFields: TStrings;
  vFieldName: string;
begin
  EnableColumnParamsChangeHandlers(False);
  FTreeList.BeginUpdate;

  try
    vMainDefinition := FAllData.MainDefinition;
    if AFields = '' then
    begin
      for vFieldDef in vMainDefinition.Fields do
      begin
        if TInteractor(FView.Interactor).NeedSkipColumn(FAllData, vFieldDef) then
          Continue;

        vWidth := TWinVCLPresenter(TInteractor(FView.Interactor).Presenter).GetWidthByType(100, vFieldDef);
        { TODO -cUI refactoring : Перенести описание аггрегаций в слой описания UI }
        vAggType := vMainDefinition.Reductions.ReductionForField(vFieldDef.Name);
        CreateColumn(vFieldDef, vFieldDef.Name, GetFieldTranslation(vFieldDef), vWidth, uConsts.soNone, vAggType);
      end;

      //if FAllData.ContentDefinitions.Count > 1 then
      //  CreateColumn(nil, '', 50, uConsts.soNone, akNotDefined);
    end
    else begin
      vFields := CreateDelimitedList(AFields);
      try
        for i := 0 to vFields.Count - 1 do
        begin
          vFieldName := vFields[i];
          vFieldDef := vMainDefinition.ExtractFieldDef(vFieldName);
          if not Assigned(vFieldDef) then
            Continue;

          vWidth := TWinVCLPresenter(Presenter).GetWidthByType(100, vFieldDef);
          vAggType := TDefinition(vFieldDef.Definition).Reductions.ReductionForField(vFieldName);

          CreateColumn(vFieldDef, vFieldName, GetFieldTranslation(vFieldDef), vWidth, uConsts.soNone, vAggType);
        end;
      finally
        vFields.Free;
      end;
    end;
  finally
    FTreeList.EndUpdate;
    EnableColumnParamsChangeHandlers(True);
  end;
end;

procedure TTreeCollectionEditor.CreateLevels(const ALayout: TLayout);
var
  vPanel: TPanel;
  vTreeLevels: string;
  vList: TStrings;
  i: Integer;

  procedure CreateLevel(const ADef: TDefinition);
  var
    vFieldDef: TFieldDef;
  begin
    for vFieldDef in ADef.Fields do
      if vFieldDef is TListFieldDef then
        FLevels.Add(TTreeLevel.Create(vFieldDef.Name));
  end;
begin
  FLevels := TObjectList<TTreeLevel>.Create;
  //  Assert(Length(vTreeLevels) > 0, 'Parameter "TreeLevels" not defined in layout: ' + vLayout.Caption + '. Specify "TreeLevels=<ListField>[|<ChildListField>]');

  vPanel := TPanel(ALayout.Control);
  vTreeLevels := GetUrlParam(vPanel.Caption, 'TreeLevels');

  if Length(vTreeLevels) > 0 then
  begin
    vList := CreateDelimitedList(vTreeLevels, '|');

    for i := 0 to vList.Count - 1 do
      FLevels.Add(TTreeLevel.Create(vList[i]));

    FreeAndNil(vList);
  end
  else
    CreateLevel(FAllData.MainDefinition);

end;

destructor TTreeCollectionEditor.Destroy;
begin
  if Assigned(FTreeList.PopupMenu) then
  begin
    TPopupMenu(FTreeList.PopupMenu).OnPopup := nil;
    FTreeList.PopupMenu := nil;
  end;

  FAllData := nil;

  FreeAndNil(FBGStyle);
  FreeAndNil(FHeaderStyle);
  FreeAndNil(FLevels);

  inherited Destroy;
end;

procedure TTreeCollectionEditor.DoExecuteUIAction(const AView: TView);
begin
  inherited;

end;

procedure TTreeCollectionEditor.DoOnColumnPosChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
begin
  SaveColumnWidths;
end;

procedure TTreeCollectionEditor.DoOnColumnSizeChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
begin
  SaveColumnWidths;
end;


procedure TTreeCollectionEditor.DoOnSorted(Sender: TObject);
begin

end;

procedure TTreeCollectionEditor.DoOnTableViewDblClick(Sender: TObject);
begin
  OnTableViewDblClick(FView, Self);
end;

procedure TTreeCollectionEditor.EnableColumnParamsChangeHandlers(const AEnable: Boolean);
begin
  if AEnable then
  begin
    FTreeList.OnColumnPosChanged := DoOnColumnPosChanged;
    FTreeList.OnColumnSizeChanged := DoOnColumnSizeChanged;
    FTreeList.OnSorted := DoOnSorted;
  end
  else
  begin
    FTreeList.OnColumnPosChanged := nil;
    FTreeList.OnColumnSizeChanged := nil;
    FTreeList.OnSorted := nil;
  end;
end;

function TTreeCollectionEditor.GetLevel(const AIndex: Integer): TTreeLevel;
var
  vLevelIndex: Integer;
begin
  Result := nil;
  if FLevels.Count = 0 then Exit;

  vLevelIndex := AIndex;

  if vLevelIndex > FLevels.Count - 1 then
    vLevelIndex := FLevels.Count - 1;

  Result := FLevels[vLevelIndex];
end;

function TTreeCollectionEditor.GetEntityCount(const ANode: TcxTreeListNode): Integer;
var
  vEntity: TEntity;
  vFieldName: string;
begin
  Result := 0;
  if FLevels.Count = 0 then Exit;

  if ANode.Level < 0 then
    Result := FAllData.Count
  else
  begin
    vEntity := GetEntity(ANode, ANode.Level);
    vFieldName := GetLevel(ANode.Level).ListFieldName;
    if vEntity.FieldExists(vFieldName) then
      Result := TListField(vEntity.FieldByName(vFieldName)).Count
    else
      Result := 0;
  end;
end;

function TTreeCollectionEditor.GetEntity(const ANode: TcxTreeListNode; const ACurrLevel: Integer): TEntity;
var
  vEnt: TEntity;
  vListField: TListField;
begin
  if ACurrLevel = 0 then
    Result := FAllData[ANode.Index]
  else
  begin
    vEnt := GetEntity(ANode.Parent, ANode.Level - 1);
    vListField := TListField(vEnt.FieldByName(GetLevel(ACurrLevel - 1).ListFieldName));
    Result := vListField[ANode.Index];
  end;
end;

procedure TTreeCollectionEditor.LoadColumnWidths;
begin
  LoadTreeColumnWidths(FDomain, FTreeList, TDefinition(FView.Definition).Name);
end;

procedure TTreeCollectionEditor.SaveColumnWidths;
begin
  SaveTreeColumnWidths(FDomain, FTreeList, TDefinition(FView.Definition).Name);
end;

procedure TTreeCollectionEditor.SetPopupArea(const APopupArea: TUIArea);
var
  vPopupMenu: TPopupMenu;
begin
  vPopupMenu := TPopupMenu(APopupArea.InnerControl);
  vPopupMenu.OnPopup := BeforeContextMenuShow;
  FTreeList.PopupMenu := vPopupMenu;
end;

procedure TTreeCollectionEditor.TreeListGetChildCount(Sender: TcxCustomTreeList; AParentNode: TcxTreeListNode; var ACount: Integer);
begin
  ACount := GetEntityCount(AParentNode);
end;

procedure TTreeCollectionEditor.TreeListGetNodeValue(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AColumn: TcxTreeListColumn;
  var AValue: Variant);
var
  vFieldDef: TFieldDef;
  vEntity: TEntity;
  vEnumeration: TEnumeration;
begin
  vFieldDef := TFieldDef(AColumn.DataBinding.Data);
  vEntity := GetEntity(ANode, ANode.Level);

  if (vEntity = nil) or (not vEntity.FieldExists(vFieldDef.Name)) then
  begin
    AValue := Null;
    Exit;
  end;

  AValue := vEntity[vFieldDef.Name];

  if vFieldDef.Kind = fkDateTime then
  begin
    if (TDateTime(AValue) < 2) and (vFieldDef.StyleName <> 'time') then
      AValue := Null
    else if vFieldDef.StyleName = 'date' then // cut the time
      AValue := Trunc(AValue);
  end
  else if vFieldDef.Kind = fkFloat then
    AValue := RoundTo(AValue, -4) //todo: get format from style
  else if vFieldDef.Kind = fkEnum then
  begin
    vEnumeration := TDomain(vEntity.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(vFieldDef).Dictionary);
    if Assigned(vEnumeration) then
      AValue := vEnumeration.Items[Integer(vEntity.FieldByName(vFieldDef.Name).Value)].DisplayText
    else
      AValue := vEntity.GetStateCaption(vFieldDef.Name);
  end
  else

end;

procedure TTreeCollectionEditor.UpdateArea(const AKind: Word; const AParameter: TEntity);
{var
  i: Integer;
  vEntity, vSelectedEntity: TEntity;
  vNewFocusedRowIndex: Integer;
  vNode: TcxTreeListNode; }
begin
  if AKind = dckEntityChanged then
    FTreeList.Invalidate
  else if AKind = dckEntitySaved then
  begin
   { FList.OnSelectionChanged := nil;
    try
      FMasterTableView.DataController.CustomDataSource.DataChanged;
    finally
      FList.OnSelectionChanged := DoOnSelectionChanged;
    end;   }
  end
  else if AKind = dckListAdded then
  begin

  end
  else if AKind = dckListRemoved then
  begin
  {  FMasterTableView.OnSelectionChanged := nil;
    try
      TUserDataSource(FMasterTableView.DataController.CustomDataSource).Remove(AParameter);
    finally
      FMasterTableView.OnSelectionChanged := DoOnSelectionChanged;
    end; }
  end
  else if AKind = dckSelectionChanged then
  begin
   { vNewFocusedRowIndex := FMasterDS.FEntities.IndexOf(AParameter);
    if vNewFocusedRowIndex <> FMasterTableView.DataController.FocusedRowIndex then
    begin
      vRow := FMasterTableView.ViewData.Rows[vNewFocusedRowIndex];
      vRow.Selected := True;
      vRow.Focused := True;
    end;  }
  end
  else if AKind = dckViewStateChanged then
    // Сделать что-то с видимостью
  else //if AKind = dckFieldChanged then
  begin // FieldChanged, FilterChanged
    FAllData.Resort;
    FTreeList.FullRefresh;
   { FList.BeginUpdate;
    try
      vSelectedEntity := nil;
      if FList.SelectionCount > 0 then
        vSelectedEntity := TEntity(FList.Selections[0].Data);

      FList.Clear;
      FAllData.Resort;

      for i := 0 to FAllData.Count - 1 do
      begin
        vEntity := FAllData[i];

        if (not vEntity.Deleted) and IsMatchToFilter(vEntity)  then
        begin
          vNode := FList.Add;
          vNode.Data := vEntity;
          vNode.Values[0] := SafeDisplayName(vEntity);
          if vEntity = vSelectedEntity then
            vNode.Focused := True;
        end;
      end;

    finally
      FList.EndUpdate;
    end;}
  end;
end;

{ TTreeLevel }

constructor TTreeLevel.Create(const AListFieldName: string);
begin
  ListFieldName := AListFieldName;
  PaintListFieldNodes := True;
end;

destructor TTreeLevel.Destroy;
begin
  Def := nil;
  inherited;
end;

{ TEntityListSelectorMTM }

procedure TEntityListSelectorMTM.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEntityList);
  //FControl.Control := nil;
  //FreeAndNil(FListBox);
end;

function TEntityListSelectorMTM.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vListFieldDef: TListFieldDef;
  vTransitFieldName: string;
  vParentEntity: TEntity;
  vTransitContentType: string;
  vTransitDefinitionName: string;
  vTransitFieldDef: TFieldDef;
  vInteractor: TInteractor;
begin
  inherited;
  FListBox := TcxCheckListBox.Create(nil);
  Result := FListBox;
  Assert(FView.Definition is TListFieldDef, 'FView.Definition не является TListFieldDef');
  vListFieldDef := TListFieldDef(FView.Definition);
  FFilterName := GetUrlParam(vListFieldDef.StyleName, 'filter', '');
  vTransitFieldName := GetUrlParam(vListFieldDef.StyleName, 'transit');
  Assert(Length(vTransitFieldName) > 0, 'Не задано транзитное поле. Параметр transit.');
  Assert(vListFieldDef._ContentDefinition.FieldExists(vTransitFieldName), 'Указанное имя транзитного поля не существует: ' + vTransitFieldName);

  vTransitFieldDef := vListFieldDef._ContentDefinition.FieldByName(vTransitFieldName);
  Assert(vTransitFieldDef is TObjectFieldDef, 'Указанное транзитное поле не является TObjectFieldDef');

  FTransitFieldDef := TObjectFieldDef(vTransitFieldDef);
  FTransitDefinition := FTransitFieldDef._ContentDefinition;
  if FTransitDefinition.Name = '~' then
  begin
    vTransitContentType := {FTransitFieldDef.ContentTypeLocator; //}GetUrlParam(vListFieldDef.StyleName, 'contentType');
    if (FView.ParentDomainObject is TEntity) and (Length(vTransitContentType) > 0) then
    begin
      vParentEntity := TEntity(FView.ParentDomainObject);
      try
        vTransitDefinitionName := vParentEntity[vTransitContentType];
        FTransitDefinition := TDomain(Domain).Configuration.DefinitionByName[vTransitDefinitionName];
      except
        Assert(False, 'Неверно указан тип для транзитного поля: ' + vTransitDefinitionName);
      end;
    end;
  end;

  vInteractor := TInteractor(FView.Interactor);

  FEntityList := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  FListBox.OnClickCheck := OnClickCheck;
end;

procedure TEntityListSelectorMTM.DoOnChange;
begin
  inherited;

end;

procedure TEntityListSelectorMTM.FillEditor;
begin
  inherited;

end;

procedure TEntityListSelectorMTM.FillList;
var
  i: Integer;
  vItem: TcxCheckListBoxItem;
  vSelectedList: TEntityList;
  vList: TList<TEntity>;
  vSelectedIndex: Integer;
  vEntity, vParentParameter: TEntity;
begin
  TDomain(Domain).GetEntityList(FView.Session, FTransitDefinition, FEntityList, '');

  vSelectedList := TEntityList(FView.DomainObject);
  vList := TList<TEntity>.Create;
  try
    for i := 0 to vSelectedList.Count - 1 do
      vList.Add(vSelectedList[i].ExtractEntity(FTransitFieldDef.Name));

    FListBox.Items.BeginUpdate;
    try
      FListBox.Items.Clear;

      vParentParameter := nil;
      if FView.ParentDomainObject is TEntity then
      begin
        vParentParameter := TEntity(FView.ParentDomainObject);
        if vParentParameter.FieldExists(FFilterName) then
          vParentParameter := vParentParameter.ExtractEntity(FFilterName)
        else
          vParentParameter := nil;
      end;

      for i := 0 to FEntityList.Count - 1 do
      begin
        vEntity := FEntityList[i];
        if Assigned(vParentParameter) and (vEntity.ExtractEntity(FFilterName) <> vParentParameter) then
          Continue;

        vItem := FListBox.Items.Add;

        if FCreateParams.Values['DisplayName'] = '' then
          vItem.Text := vEntity.DisplayName
        else
          vItem.Text := vEntity[FCreateParams.Values['DisplayName']];

        vSelectedIndex := vList.IndexOf(vEntity);
        vItem.Checked := vSelectedIndex >= 0;
        if vItem.Checked then
          vItem.ItemObject := vSelectedList[vSelectedIndex]
        else
          vItem.ItemObject := FEntityList[i];
      end;
    finally
      FListBox.Items.EndUpdate;
    end;
  finally
    FreeAndNil(vList);
  end;
end;

procedure TEntityListSelectorMTM.OnClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
var
  vEntity: TEntity;
begin
  vEntity := TEntity(FListBox.Items[AIndex].ItemObject);

  if ANewState = cbsChecked then
  begin
    Assert(vEntity.Definition.IsDescendantOf(FEntityList.MainDefinition.Name), 'К записи привязан объект неправильного типа');
    TUserSession(FView.Session).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
      var
        vTransitEntity: TEntity;
      begin
        vTransitEntity := TEntityList(FView.DomainObject).AddEntity(AHolder, '', FTransitFieldDef.Name, [Integer(vEntity)]);
        //vTransitEntity._SetFieldEntity(AHolder, FTransitField.Name, vEntity);
        FListBox.Items[AIndex].ItemObject := vTransitEntity;
        Result := True;
      end, TChangeHolder(Holder));
  end
  else begin
    Assert(vEntity.Definition.IsDescendantOf(TEntityList(FView.DomainObject).MainDefinition.Name), 'К записи привязан объект неправильного типа');
    FListBox.Items[AIndex].ItemObject := vEntity.ExtractEntity(FTransitFieldDef.Name);
    TUserSession(FView.Session).AtomicModification(nil, function(const AHolder: TChangeHolder): Boolean
      begin
        TEntityList(FView.DomainObject).RemoveEntity(AHolder, vEntity);
        Result := True;
      end, TChangeHolder(Holder));
  end;
end;

procedure TEntityListSelectorMTM.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(AHandler) then
    FListBox.OnClickCheck := OnClickCheck
  else
    FListBox.OnClickCheck := nil;
end;

procedure TEntityListSelectorMTM.UpdateArea(const AKind: Word; const AParameter: TEntity);
{var
  i: Integer;
  vEntity: TEntity;
  vNewFocusedRowIndex: Integer;
  vRow: TcxCustomGridRow; }
begin
  FillList;
 { if AKind = dckEntityChanged then
  begin
    FMasterTableView.OnFocusedRecordChanged := nil;
    try
      if not FMasterDS.IsLoading then
        FMasterTableView.Invalidate(True) //CustomDataSource.DataChanged
    finally
      FMasterTableView.OnFocusedRecordChanged := DoOnFocusedRecordChanged;
    end;
  end
  // Нужно отключать dckEntityChanged
  //else if AKind = dckEntitySaved then
  //  FMasterTableView.DataController.CustomDataSource.DataChanged
  else if AKind = dckListAdded then
  begin
    TUserDataSource(FMasterTableView.DataController.CustomDataSource).Add(AParameter, not TDomain(Domain).LoadingChanges);
  end
  else if AKind = dckListRemoved then
  begin
    TUserDataSource(FMasterTableView.DataController.CustomDataSource).Remove(AParameter, not TDomain(Domain).LoadingChanges);
  end
  else if AKind = dckSelectionChanged then
  begin
    vNewFocusedRowIndex := FMasterDS.FEntities.IndexOf(AParameter);
    if vNewFocusedRowIndex <> FMasterTableView.DataController.FocusedRowIndex then
    begin
      vRow := FMasterTableView.ViewData.Rows[vNewFocusedRowIndex];
      vRow.Selected := True;
      vRow.Focused := True;
    end;
  end
  else if AKind = dckViewStateChanged then
    // Сделать что-то с видимостью
  else begin
    FMasterTableView.BeginUpdate;
    try
      FMasterDS.Data.Clear;

      FAllData.Resort;
      for i := 0 to FAllData.Count - 1 do
      begin
        vEntity := FAllData[i];
        if (not vEntity.Deleted) {IsMatchToFilter(vEnt))} //then
 {         FMasterDS.Data.Add(vEntity);
      end;

      FMasterDS.Update;
    finally
      FMasterTableView.EndUpdate;
      FMasterTableView.DataController.ClearSelection;
      FMasterTableView.DataController.FocusedRecordIndex := -1;
    end;
    if not FLayoutExists then
      FMasterTableView.ApplyBestFit;
  end;    }
end;

{ TPagedEntityListSelector }

procedure TPagedEntityListSelector.DoBeforeFreeControl;
var
  vView: TView;
begin
  inherited;
  //FControl.Control := nil;
  //FreeAndNil(FPages);
  for vView in FCreatedViews do
    vView.RemoveListener(Self);
  FreeAndNil(FCreatedViews);
end;

function TPagedEntityListSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FPages := TcxPageControl.Create(nil);
  Result := FPages;
  FEntityList := nil;
  FInUpdate := False;
  FCreatedViews := TList<TView>.Create;
end;

procedure TPagedEntityListSelector.DoOnChange;
begin
  inherited;

end;

procedure TPagedEntityListSelector.FillEditor;
var
  i: Integer;
  vSelectedList: TEntityList;
  vLayout: TLayout;
  vTab: TComponent;
  vTabArea, vArea: TUIArea;
  vView: TView;
  vTabParams: string;
begin
  if FInUpdate then Exit;

  vSelectedList := TEntityList(FView.DomainObject);

  if vSelectedList.Count = Count then Exit;

  FPages.Properties.BeginUpdate;
  FInUpdate := True;
  try
    while Count > 0 do
    begin
      vArea := Areas[0];
      RemoveArea(vArea);
    end;

    for i := 0 to vSelectedList.Count - 1 do
    begin
      if FCreateParams.Values['DisplayName'] = '' then
        vTabParams := 'caption=' + vSelectedList[i].DisplayName
      else
        vTabParams := 'caption=' + vSelectedList[i][FCreateParams.Values['DisplayName']];

      vLayout := TPresenter(Presenter).CreateLayoutArea(lkPage, vTabParams);
      vTab := TComponent(vLayout.Control);
      try
        vTab.Tag := 0;
        vView := FView.BuildView(IntToStr(i));
        vView.AddListener(Self);
        FCreatedViews.Add(vView);
        vTabArea := CreateChildArea(vView, vLayout, '');
        TInteractor(Interactor).UIBuilder.ApplyLayout(vTabArea, vView, FCreateParams.Values['layout'], '');
      finally
        //vTab.Free;
      end;
    end;
//    FPages.Properties.HideTabs := FPages.PageCount < 2;
  finally
    FPages.Properties.EndUpdate;
    FInUpdate := False;
  end;

end;

procedure TPagedEntityListSelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;

end;

{ TEntityListSelector3 }

procedure TEntityListSelector3.FillLookupList;
var
  vCollection: TCollection;
  vLookupItem: TEntity;
  vItem: TcxCheckListBoxItem;

  function IsChanged: Boolean;
  var
    i: Integer;
  begin
    if vCollection.Count <> FListBox.Items.Count then Exit(True);

    for i := 0 to FListBox.Items.Count - 1 do
      if FListBox.Items[i].ItemObject <> vCollection[i] then Exit(True);

    Result := False;
  end;
begin
  inherited;

  vCollection := TDomain(Domain)[FLookupCollection];

  if not IsChanged then Exit;

  FListBox.Items.BeginUpdate;
  try
    FListBox.Items.Clear;

    for vLookupItem in vCollection do
    begin
      vItem := FListBox.Items.Add;
      vItem.Text := SafeDisplayName(vLookupItem);
      vItem.ItemObject := vLookupItem;
    end;
  finally
    FListBox.Items.EndUpdate;
  end;
end;

function TEntityListSelector3.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FListBox := TcxCheckListBox.Create(nil);
  Result := FListBox;
  FMasterList := TEntityList(FView.DomainObject);
  FLookupCollection := TDefinition(FView.Definition).Name;
  Assert(Assigned(FCreateParams) and (FCreateParams.IndexOfName('lookupfield') > -1), 'не задан параметер lookupfield');
  FLookupField := FCreateParams.Values['lookupfield'];
  FDestroing := False;
end;

procedure TEntityListSelector3.DoDisableContent;
begin
  inherited;
  FDestroing := True;
end;

procedure TEntityListSelector3.DoOnChange;
begin
end;

procedure TEntityListSelector3.FillEditor;
var
  i: Integer;
  vItem: TcxCheckListBoxItem;
  vLookupItem: TEntity;
begin
  if FDestroing then Exit;

  FillLookupList;

  for i := 0 to FListBox.Count - 1 do
  begin
    vItem := FListBox.Items[i];
    vLookupItem := TEntity(vItem.ItemObject);
    vItem.Checked := FindMasterItem(vLookupItem) <> nil;
  end;
end;

function TEntityListSelector3.FindMasterItem(const ALookupItem: TEntity): TEntity;
var
  vMasterList: TEntityList;
begin
  vMasterList := TEntityList(FView.DomainObject);
  for Result in vMasterList do
    if Result.ExtractEntity(FLookupField) = ALookupItem then
      Exit;
  Result := nil;
end;

procedure TEntityListSelector3.OnClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
var
  vHolder: TObject;
  vMasterItem, vLookupItem: TEntity;
begin
  vHolder := Holder;

  vLookupItem := TEntity(FListBox.Items[AIndex].ItemObject);
  vMasterItem := FindMasterItem(vLookupItem);

  if ANewState = cbsChecked then
  begin
    if not Assigned(vMasterItem) then
    begin
      vMasterItem := FMasterList.AddEntity(vHolder, FMasterList.MainDefinition.Name, '', []);
      vMasterItem._SetFieldEntity(vHolder, FLookupField, vLookupItem);
    end;
  end
  else
    if Assigned(vMasterItem) then
      FMasterList.RemoveEntity(vHolder, vMasterItem);
end;

procedure TEntityListSelector3.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(AHandler) then
    FListBox.OnClickCheck := OnClickCheck
  else
    FListBox.OnClickCheck := nil;
end;

initialization

TPresenter.RegisterUIClass('Windows.DevExpress', uiListEdit, '', TColumnListEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiListEdit, 'simple', TListEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiListEdit, 'selector', TEntityListSelector); //deprecated
TPresenter.RegisterUIClass('Windows.DevExpress', uiListEdit, 'multiselect', TEntityListSelector2);
TPresenter.RegisterUIClass('Windows.DevExpress', uiListEdit, 'multiselect3', TEntityListSelector3);
TPresenter.RegisterUIClass('Windows.DevExpress', uiListEdit, 'mtm', TEntityListSelectorMTM);
TPresenter.RegisterUIClass('Windows.DevExpress', uiListEdit, 'parameters', TParametersEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiListEdit, 'paged', TPagedEntityListSelector);

TPresenter.RegisterUIClass('Windows.DevExpress', uiCollection, '', TCollectionEditor);
TPresenter.RegisterUIClass('Windows.DevExpress', uiCollection, 'Pivot', TPivotGrid);
TPresenter.RegisterUIClass('Windows.DevExpress', uiCollection, 'Tree', TTreeCollectionEditor);

end.

