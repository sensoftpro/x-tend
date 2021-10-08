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

unit vclListEditors;

interface

uses
  Generics.Collections, Graphics, Classes, Controls, Types, CheckLst, ExtCtrls,

  uEntity, uEntityList, vclArea, vclBlobEditors, uView, uUIBuilder, uDefinition, uConsts, uScene,

  cxCheckListBox, cxLookAndFeelPainters, dxColorEdit,
  cxTL, cxGrid, cxGridTableView, cxCustomData, cxGridLevel, cxGridCustomTableView, cxGridChartView, cxStyles,
  cxVGrid, cxCalendar, cxCheckBox, cxGraphics, cxControls, cxLookAndFeels, cxContainer,
  cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxSpinEdit, cxInplaceContainer, cxCustomPivotGrid, cxPivotGrid,
  cxExportPivotGridLink, cxTLData;

type
  TEntityListSelector = class (TVCLFieldArea)
  private
    FListBox: TCheckListBox;
    FEntityList: TEntityList;
    procedure OnWinControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure DoCreateControl(const ALayout: TObject); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    function GetLayoutPositionCount: Integer; override;
    function GetDefaultFocusedChild: TWinControl; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    procedure DoOnChange; override;
  end;

  TEntityListSelector2 = class(TVCLFieldArea)
  private
    FListBox: TcxCheckListBox;
    FEntityList: TEntityList;
    procedure OnClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
  protected
    procedure DoCreateControl(const ALayout: TObject); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    function GetLayoutPositionCount: Integer; override;
    function GetDefaultFocusedChild: TWinControl; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    procedure DoOnChange; override;
  end;

  TEntityListSelectorMTM = class(TVCLFieldArea) // many to many link
  private
    FListBox: TcxCheckListBox;
    FEntityList: TEntityList;
    FTransitField: TObjectFieldDef;
    procedure FillList;
    procedure OnClickCheck(Sender: TObject; AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
  protected
    procedure DoCreateControl(const ALayout: TObject); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    function GetLayoutPositionCount: Integer; override;
    function GetDefaultFocusedChild: TWinControl; override;
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
    procedure DoCreateControl(const ALayout: TObject); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    function GetLayoutPositionCount: Integer; override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
  end;

  TColumnBinding = class
  private
    FFieldName: string;
    FSanitizedFieldName: string;
    FFieldDef: TFieldDef;
    FAfterPoint: Integer; // знаков после запятой для Float (здесь для оптимизации)
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
  protected
    function AppendRecord: TcxDataRecordHandle; override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    function GetItemHandle(AItemIndex: Integer): TcxDataItemHandle; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    function IsNativeCompare: Boolean; override;
  public
    constructor Create;
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

  TCollectionEditor = class(TVCLArea)
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
    procedure DoOnCompare(ADataController: TcxCustomDataController; ARecordIndex1, ARecordIndex2, AItemIndex: Integer;
      const V1, V2: Variant; var Compare: Integer);
    procedure DoOnTableViewDblClick(Sender: TObject);
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
  protected
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure DoExecuteUIAction(const AView: TView); override;
  public
    constructor Create(const AParent: TUIArea; const ALayout: TObject; const AView: TView; const AId: string);
    destructor Destroy; override;
  end;

  TColumnListEditor = class (TVCLFieldArea)
  private
    FGrid: TcxGrid;
    FMasterTableView: TcxGridTableView;
    FBGStyle: TcxStyle;
    FHeaderStyle: TcxStyle;
    FMasterDS: TUserDataSource;
    FAllData: TEntityList;
    FLayoutExists: Boolean;
    procedure DoOnCompare(ADataController: TcxCustomDataController; ARecordIndex1, ARecordIndex2, AItemIndex: Integer;
      const V1, V2: Variant; var Compare: Integer);
    procedure DoOnTableViewDblClick(Sender: TObject);
    procedure DoOnFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord,
      AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
    procedure CreateColumnsFromModel(const AFields: string = '');
    procedure EnableColumnParamsChangeHandlers(const AEnable: Boolean);
    procedure ShowFooter(const AColumn: TcxGridColumn; const AAgg: TAggregationKind);
    procedure CreateColumn(const AFieldDef: TFieldDef; const AFieldName: string; const AOverriddenCaption: string;
      const AWidth: Integer; const ASortOrder: TSortOrder; const AAgg: TAggregationKind);
    procedure OnGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
    procedure BeforeContextMenuShow(Sender: TObject);
    procedure SaveColumnWidths;
    procedure DoOnColumnPosChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
    procedure DoOnColumnSizeChanged(Sender: TcxGridTableView; AColumn: TcxGridColumn);
    procedure DoOnHeaderClick(Sender: TObject);
    procedure LoadColumnWidths;
  protected
    procedure DoCreateControl(const ALayout: TObject); override;
    procedure DoBeforeFreeControl; override;
    function GetLayoutPositionCount: Integer; override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
  end;

  TListEditor = class (TVCLFieldArea)
  private
    FList: TcxTreeList;
    procedure OnSelectionChanged(Sender: TObject);
    procedure OnDblClick(Sender: TObject);
  protected
    procedure DoCreateControl(const ALayout: TObject); override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    function GetLayoutPositionCount: Integer; override;
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

  TPivotGrid = class (TVCLArea)
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
  public
    constructor Create(const AParent: TUIArea; const ALayout: TObject; const AView: TView; const AId: string; const AParams: string);
    destructor Destroy; override;
  end;

  TTreeLevel = class
    Def: TDefinition;
    ListFieldNames: TStrings;
    PaintListFieldNodes: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTreeCollectionEditor = class (TVCLArea)
  private
    FTreeList: TcxVirtualTreeList;
    FBGStyle: TcxStyle;
    FHeaderStyle: TcxStyle;
    FAllData: TEntityList;
//    FFilterText: string;
//    FFilterDateFrom, FFilterDateTo: TDateTime;
//    FFilterDateActive: Boolean;
    FDomain: TObject;
    FLevels: TList<TTreeLevel>;
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
    procedure CreateLevels(const ALayout: TObject);
    function GetLevel(const AIndex: Integer): TTreeLevel;
    function GetEntityCount(const ANode: TcxTreeListNode): Integer;
    function GetEntity(const ANode: TcxTreeListNode; const ACurrLevel: Integer): TEntity;
  protected
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure DoExecuteUIAction(const AView: TView); override;
  public
    constructor Create(const AParent: TUIArea; const ALayout: TObject; const AView: TView; const AId: string);
    destructor Destroy; override;
  end;

const
  cColWidthDelim = ',';

  cFooterCount = 1;
  cFooterSum = 2;
  cFooterAvg = 3;

procedure LoadTreeColumnWidths(const ADomain: TObject; const ATreeList: TcxCustomTreeList; const AObjectName: string);
procedure SaveTreeColumnWidths(const ADomain: TObject; const ATreeList: TcxCustomTreeList; const AObjectName: string);
procedure OnTableViewDblClick(const AView: TView; const AArea: TUIArea);

implementation

uses
  TypInfo, Windows, SysUtils, Messages, Math, Variants, DateUtils, Dialogs, Menus, ShellApi, IOUtils,

  uWinVCLPresenter, uObjectField, uInteractor, uEnumeration, uSession, uChangeManager,
  uConfiguration, uDomain, uQueryDef, uQuery, uUtils, uPresenter, uSettings, uDrawStyles, uWinScene, uCollection,

  dxCore, cxGridStrs, cxPivotGridStrs, cxDataStorage, cxGridCustomView, cxImageComboBox, cxDateUtils,
  cxGridExportLink, cxProgressBar;

type
  TCalculateStyleFunc = function(const AViewName: string; const AEntity: TEntity): TColor of object;

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
    if AFieldDef = nil then
      Result := CompareStr(AEnt1.DisplayName, AEnt2.DisplayName)
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

procedure GetContentStyle(const AArea: TVCLArea; const AList: TEntityList; const AColorFieldName: string;
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

    if vEntity.IsService then
      vStyle.TextColor := clSilver
    else if (AItem.Name = 'TotalAmount') and (vEntity['TotalAmount'] < 0) then
      vStyle.TextColor := clRed;

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

{ TEntityListSelector }

procedure TEntityListSelector.DoBeforeFreeControl;
begin
  inherited;
  FControl := nil;
  FreeAndNil(FListBox);
end;

procedure TEntityListSelector.DoCreateControl(const ALayout: TObject);
begin
  inherited;
  FListBox := TCheckListBox.Create(nil);
  FEntityList := nil;
  FControl := FListBox;
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
      vList.AddToList(vHolder, TEntity(FListBox.Items.Objects[i]))
    else
      vList.DeleteFromList(vHolder, TEntity(FListBox.Items.Objects[i]));
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

function TEntityListSelector.GetDefaultFocusedChild: TWinControl;
begin
  Result := FListBox;
end;

function TEntityListSelector.GetLayoutPositionCount: Integer;
begin
  Result := 5;
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
      vList.AddToList(vHolder, TEntity(FListBox.Items.Objects[i]));
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

procedure TEntityListSelector2.DoBeforeFreeControl;
begin
  inherited;
  FControl := nil;
  FreeAndNil(FListBox);
end;

procedure TEntityListSelector2.DoCreateControl(const ALayout: TObject);
begin
  inherited;
  FListBox := TcxCheckListBox.Create(nil);
  FEntityList := nil;
  FControl := FListBox;
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

function TEntityListSelector2.GetDefaultFocusedChild: TWinControl;
begin
  Result := FListBox;
end;

function TEntityListSelector2.GetLayoutPositionCount: Integer;
begin
  Result := 5;
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

procedure TListEditor.DoCreateControl(const ALayout: TObject);
begin
  inherited;

  FList := TcxTreeList.Create(nil);
  FList.OnSelectionChanged := OnSelectionChanged;
  FList.OnDblClick := OnDblClick;
  FList.CreateColumn;
  FList.OptionsView.Headers := True;
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
  FControl := FList;
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

function TListEditor.GetLayoutPositionCount: Integer;
begin
  Result := 3;
end;

procedure TListEditor.OnDblClick(Sender: TObject);
var
  vSelectedView: TView;
  vActionView: TView;
  vArea: TVCLArea;
  vHolder: TObject;
begin
  vArea := TVCLArea(TComponent(Sender).Tag);
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
  Compare := CompareEntities(FMasterDS.Data[ARecordIndex1], FMasterDS.Data[ARecordIndex2],
    vColumnBinding.FieldDef, vColumnBinding.FieldName);
end;

procedure TCollectionEditor.BeforeContextMenuShow(Sender: TObject);
var
  vMenu: TPopupMenu;
  vMenuItem: TMenuItem;
  vArea: TVCLArea;
  vParams: TEntity;
  i: Integer;
begin
  vMenu := TPopupMenu(Sender);
  if vMenu.Items.Count <= 0 then
    Exit;

  for i := 0 to vMenu.Items.Count - 1 do
  begin
    vMenuItem := vMenu.Items[i];
    vArea := TVCLArea(vMenuItem.Tag);
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
        vParams._SetFieldValue(nil, 'IsChecked', vMenuItem.Checked);
    end;
  end;

  vMenuItem := vMenu.Items[vMenu.Items.Count - 1];
  vMenuItem.Caption := TInteractor(Interactor).Translate('txtRecordCount', 'Записей') + ': ' + IntToStr(FAllData.Count);
end;

constructor TCollectionEditor.Create(const AParent: TUIArea; const ALayout: TObject; const AView: TView; const AId: string);
var
  vDefinition: TDefinition;
  vView: TView;
  vColumn: TcxGridColumn;
  vPopupArea: TVCLArea;
  vPopupMenu: TPopupMenu;
  vFields: string;
begin
  FBGStyle := TcxStyle.Create(nil);
  FHeaderStyle := TcxStyle.Create(nil);

  FMasterDS := TUserDataSource.Create;

  FMasterTableView := TcxGridTableView.Create(nil);
  FMasterTableView.DataController.OnCompare := DoOnCompare;
  FMasterTableView.OptionsData.Editing := False;
  FMasterTableView.OptionsData.Deleting := False;
  FMasterTableView.OptionsData.Inserting := False;
  FMasterTableView.OptionsCustomize.ColumnsQuickCustomization := True;
  FMasterTableView.OptionsSelection.CellSelect := False;
  FMasterTableView.OptionsSelection.MultiSelect := True;
  FMasterTableView.OptionsSelection.HideSelection := True;

  if StrToBoolDef(TDomain(TInteractor(AView.Interactor).Domain).UserSettings.GetValue('Core', 'MouseMultiSelectInGrids'), True) then
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
  FMasterTableView.OnDblClick := DoOnTableViewDblClick;
  FMasterTableView.OnSelectionChanged := DoOnSelectionChanged;
  FMasterTableView.OnCanSelectRecord := DoCanSelectRecord;
  FMasterTableView.DataController.Summary.Options := [soSelectedRecords, soMultipleSelectedRecords];
  FMasterTableView.Styles.Background := FBGStyle;
  FMasterTableView.Styles.Header := FHeaderStyle;

  if Assigned(ALayout) and (ALayout is TPanel) and Assigned(TPanel(ALayout).PopupMenu) then
  begin
    vPopupArea := TVCLArea(AParent.AreaById('Popup'));
    { TODO -owa : Нужно найти другой способ привязки меню }
    if not Assigned(vPopupArea) and Assigned(AParent.Parent) then
      vPopupArea := TVCLArea(AParent.Parent.AreaById('Popup'));
    if Assigned(vPopupArea) then
    begin
      vPopupMenu := TPopupMenu(vPopupArea.Component);
      vPopupMenu.OnPopup := BeforeContextMenuShow;
      FMasterTableView.PopupMenu := vPopupMenu;
    end;
  end;

  FGrid := TcxGrid.Create(nil);
  FGrid.LookAndFeel.NativeStyle := False;
  FGrid.LookAndFeel.Kind := lfFlat;

  if ALayout is TPanel then
    FGrid.Align := TPanel(ALayout).Align
  else
    FGrid.Align := alClient;
  FGrid.Levels.Add.GridView := FMasterTableView;
//  FGrid.Levels.Add.GridView := FChartView;
  FGrid.Font.Size := 12;

  inherited Create(AParent, AView, AId, FGrid);

  // after inherited Create, Interactor must be initialized
  cxSetResourceString(@scxGridGroupByBoxCaption, TInteractor(Interactor).Translate('txtMoveColumnForGrouping',
    'Перетащите сюда колонку для группировки'));
  FMasterTableView.OptionsView.NoDataToDisplayInfoText := TInteractor(Interactor).Translate('@Grid@NoDataText', '<Нет данных>');
  FAllData := TEntityList(FView.DomainObject);

  FBGStyle.Color := GetBGColor(FAllData.MainDefinition, clWindow);
  FHeaderStyle.Font.Size := 10;
  FHeaderStyle.TextColor := clGray;

  EnableColumnParamsChangeHandlers(False);

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
      TEntity(vView.DomainObject)._SetFieldValue(nil, 'IsChecked', True);
      FMasterTableView.OptionsView.GroupByBox := True;
    end;
  end;
end;

procedure TCollectionEditor.CreateColumnsFromModel(const AFields: string = '');
var
  i: Integer;
  vFieldDef: TFieldDef;
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
          if not Assigned(vFieldDef) then
            Continue;

          vWidth := TWinVCLPresenter(TInteractor(FView.Interactor).Presenter).GetWidthByType(100, vFieldDef);
          vAggType := TDefinition(vFieldDef.Definition).Reductions.ReductionForField(vFieldName);

          CreateColumn(vFieldDef, vFieldName, GetFieldTranslation(vFieldDef), vWidth, uConsts.soNone, vAggType);
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

  ExportGridToExcel(vFileName, FGrid, True, True, False);

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
      FMasterTableView.Invalidate(True);
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
      FMasterTableView.ApplyBestFit;
      SaveColumnWidths;
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

  if Assigned(AFieldDef) then
  begin
    vCol.Name := vColumnBinding.SanitizedFieldName;
    if AFieldDef.Kind in [fkInteger, fkFloat, fkCurrency] then
    begin
      vStyleName := GetUrlCommand(AFieldDef.StyleName);
      if vStyleName = 'progress' then
      begin
        vCol.PropertiesClass := TcxProgressBarProperties;
        TcxProgressBarProperties(vCol.Properties).BarStyle := cxbsAnimation;
      end
      else
      begin
        vCol.PropertiesClassName := 'TcxSpinEditProperties';   // need for Excel export
        if vStyleName = 'formatted' then
        begin
          if AFieldDef.Kind = fkFloat then
            TcxSpinEditProperties(vCol.Properties).ValueType := vtFloat;
          TcxSpinEditProperties(vCol.Properties).DisplayFormat := GetUrlParam(AFieldDef.StyleName, 'format');
        end;
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
        vCol.PropertiesClass := TcxDateEditProperties  // need for Excel export
    end
    else if AFieldDef.Kind = fkBoolean then
    begin
      vCol.PropertiesClass := TcxCheckBoxProperties;
      TcxCheckBoxProperties(vCol.Properties).DisplayChecked := 'Да';
      TcxCheckBoxProperties(vCol.Properties).DisplayUnchecked := 'Нет';
    end
    else if AFieldDef.Kind = fkColor then
      vCol.PropertiesClass := TdxColorEditProperties
    else if (AFieldDef.Kind = fkEnum) and (AFieldDef.StyleName = 'images') then
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

procedure TCollectionEditor.OnGetContentStyle(Sender: TcxCustomGridTableView;
  ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
begin
  GetContentStyle(Self, FAllData, FColorFieldName, FMasterDS.Data, Sender, ARecord, AItem, AStyle);
end;

procedure TCollectionEditor.SaveColumnWidths;
begin
  SaveGridColumnWidths(TInteractor(FView.Interactor).Domain, FMasterTableView, FView.InitialName);
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
    vParams._SetFieldValue(nil, 'IsChecked', not vParams['IsChecked']);
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

procedure TCollectionEditor.DoOnTableViewDblClick(Sender: TObject);
begin
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
    DataController.ClearSelection;

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

constructor TUserDataSource.Create;
begin
  FEntities := TList<TEntity>.Create;
  FColumns := TObjectList<TColumnBinding>.Create;
  FIsLoading := False;
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

function TUserDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  vColumnBinding: TColumnBinding;
  vFieldDef: TFieldDef;
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

      vColumnBinding := TColumnBinding(AItemHandle);
      vFieldDef := vColumnBinding.FieldDef;
      vFieldName := vColumnBinding.FieldName;

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
      else if vFieldDef.Kind = fkFloat then
        Result := RoundTo(Result, -vColumnBinding.AfterPoint)
      else if vFieldDef.Kind = fkEnum then
      begin
        vEnumeration := TDomain(vEntity.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(vFieldDef).Dictionary);
        if Assigned(vEnumeration) then
          Result := vEnumeration.Items[Integer(vEntity.FieldByName(vFieldDef.Name).Value)].DisplayText
        else
          Result := vEntity.GetStateCaption(vFieldName);
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
  vArea: TVCLArea;
  vParams: TEntity;
  i: Integer;
begin
  vMenu := TPopupMenu(Sender);
  if vMenu.Items.Count <= 0 then
    Exit;

  for i := 0 to vMenu.Items.Count - 1 do
  begin
    vMenuItem := vMenu.Items[i];
    vArea := TVCLArea(vMenuItem.Tag);
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
        vParams._SetFieldValue(nil, 'IsChecked', vMenuItem.Checked);
    end;
  end;

  vMenuItem := vMenu.Items[vMenu.Items.Count - 1];
  vMenuItem.Caption := TInteractor(Interactor).Translate('txtRecordCount', 'Записей') + ': ' + IntToStr(FAllData.Count);
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
  vCol.Caption := AOverriddenCaption;
  vColumnBinding := FMasterDS.AddColumn(AFieldName, AFieldDef);
  vCol.DataBinding.Data := vColumnBinding;

  if Assigned(AFieldDef) then
  begin
    vCol.Name := vColumnBinding.SanitizedFieldName;
    if AFieldDef.Kind in [fkInteger, fkFloat, fkCurrency] then
    begin
      vStyleName := GetUrlCommand(AFieldDef.StyleName);
      if vStyleName = 'progress' then
      begin
        vCol.PropertiesClass := TcxProgressBarProperties;
        TcxProgressBarProperties(vCol.Properties).BarStyle := cxbsAnimation;
      end
      else
      begin
        vCol.PropertiesClassName := 'TcxSpinEditProperties';   // need for Excel export
        if vStyleName = 'formatted' then
        begin
          if AFieldDef.Kind = fkFloat then
            TcxSpinEditProperties(vCol.Properties).ValueType := vtFloat;
          TcxSpinEditProperties(vCol.Properties).DisplayFormat := GetUrlParam(AFieldDef.StyleName, 'format');
        end;
        vCol.HeaderAlignmentHorz := taRightJustify;
        vCol.FooterAlignmentHorz := taRightJustify;
        vCol.Properties.Alignment.Horz := taRightJustify;
      end;
    end
    else if AFieldDef.Kind = fkDateTime then
    begin
      if AFieldDef.StyleName = 'time' then
        vCol.PropertiesClassName := 'TcxTimeEditProperties'
      else
        vCol.PropertiesClass := TcxDateEditProperties  // need for Excel export
    end
    else if AFieldDef.Kind = fkBoolean then
      vCol.PropertiesClassName := 'TcxCheckBoxProperties'
    else if AFieldDef.Kind = fkColor then
      vCol.PropertiesClassName := 'TdxColorEditProperties';

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
end;

procedure TColumnListEditor.DoCreateControl(const ALayout: TObject);
var
  vFields: string;
  vPopupArea: TVCLArea;
  vPopupMenu: TPopupMenu;
begin
  inherited;
  FBGStyle := TcxStyle.Create(nil);
  FHeaderStyle := TcxStyle.Create(nil);

  FMasterDS := TUserDataSource.Create;

  FMasterTableView := TcxGridTableView.Create(nil);
  FMasterTableView.DataController.OnCompare := DoOnCompare;
  FMasterTableView.OptionsData.Editing := False;
  FMasterTableView.OptionsData.Deleting := False;
  FMasterTableView.OptionsData.Inserting := False;
  FMasterTableView.OptionsCustomize.ColumnsQuickCustomization := True;
  FMasterTableView.OptionsSelection.CellSelect := False;
  FMasterTableView.OptionsSelection.MultiSelect := True;
  FMasterTableView.OptionsView.GroupByBox := False;

  //FMasterTableView.OptionsView.Header := TObjectFieldDef(FFieldDef).SortType <> estSortByOrder;
  if StrToBoolDef(TDomain(TInteractor(FView.Interactor).Domain).UserSettings.GetValue('Core', 'MouseMultiSelectInGrids'), True) then
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
  FMasterTableView.OnDblClick := DoOnTableViewDblClick;
//  FMasterTableView.OnKeyDown := OnFilterKeyDown;
  FMasterTableView.OnFocusedRecordChanged := DoOnFocusedRecordChanged;
  FMasterTableView.DataController.Summary.Options := [soSelectedRecords, soMultipleSelectedRecords];
  FMasterTableView.Styles.Background := FBGStyle;
  FMasterTableView.Styles.Header := FHeaderStyle;

  if Assigned(ALayout) and (ALayout is TPanel) and Assigned(TPanel(ALayout).PopupMenu) then
  begin
    vPopupArea := TVCLArea(Parent.AreaById('Popup'));
    { TODO -owa : Нужно найти другой способ привязки меню }
    if not Assigned(vPopupArea) and Assigned(Parent.Parent) then
      vPopupArea := TVCLArea(Parent.Parent.AreaById('Popup'));
    if Assigned(vPopupArea) then
    begin
      vPopupMenu := TPopupMenu(vPopupArea.Component);
      vPopupMenu.OnPopup := BeforeContextMenuShow;
      FMasterTableView.PopupMenu := vPopupMenu;
    end;
  end;

  FGrid := TcxGrid.Create(nil);
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

  vFields := TUIArea(Parent).QueryParameter('fields');
  CreateColumnsFromModel(vFields);
  LoadColumnWidths;

  FControl := FGrid;
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

procedure TColumnListEditor.DoOnTableViewDblClick(Sender: TObject);
begin
  OnTableViewDblClick(FView, Self);
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

function TColumnListEditor.GetLayoutPositionCount: Integer;
begin
  Result := 3;
end;

procedure TColumnListEditor.LoadColumnWidths;
begin
  FLayoutExists := LoadGridColumnWidths(TInteractor(FView.Interactor).Domain, FMasterTableView, FFieldDef.FullName);
end;

procedure TColumnListEditor.OnGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
begin
  GetContentStyle(Self, FAllData, FAllData.MainDefinition.ColorFieldName, FMasterDS.Data, Sender, ARecord, AItem, AStyle);
end;

procedure TColumnListEditor.SaveColumnWidths;
begin
  SaveGridColumnWidths(TInteractor(FView.Interactor).Domain, FMasterTableView, FFieldDef.FullName);
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
        if (not vEntity.Deleted) {IsMatchToFilter(vEnt))} then
          FMasterDS.Data.Add(vEntity);
      end;

      FMasterDS.Update;
    finally
      FMasterTableView.EndUpdate;
      FMasterTableView.DataController.ClearSelection;
      FMasterTableView.DataController.FocusedRecordIndex := -1;
    end;
    if not FLayoutExists then
      FMasterTableView.ApplyBestFit;
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

procedure TParametersEditor.DoCreateControl(const ALayout: TObject);
begin
  inherited;

  FGrid := TcxVerticalGrid.Create(nil);
  FGrid.Width := 400;
  FControl := FGrid;

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

function TParametersEditor.GetLayoutPositionCount: Integer;
begin
  Result := 2;
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
var
  vFormat: string;
begin
  inherited Create;
  FFieldName := AFieldName;
  FSanitizedFieldName := StringReplace(AFieldName, '.', '__', [rfReplaceAll]);
  FFieldDef := AFieldDef;
  FAfterPoint := 4;
  if (FFieldDef.Kind = fkFloat) and (GetUrlCommand(FFieldDef.StyleName) = 'formatted') then
  begin
    vFormat := GetUrlParam(FFieldDef.StyleName, 'format');
    FAfterPoint := Length(vFormat) - Pos('.', vFormat);
  end;
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
  vArea: TVCLArea;
  //vParams: TEntity;
  i: Integer;
begin
  vMenu := TPopupMenu(Sender);
  if vMenu.Items.Count <= 0 then
    Exit;

  for i := 0 to vMenu.Items.Count - 1 do
  begin
    vMenuItem := vMenu.Items[i];
    vArea := TVCLArea(vMenuItem.Tag);
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
        vParams._SetFieldValue(nil, 'IsChecked', vMenuItem.Checked);
    end;}
  end;

  vMenuItem := vMenu.Items[vMenu.Items.Count - 1];
  vMenuItem.Caption := TInteractor(Interactor).Translate('txtRecordCount', 'Записей') + ': ' + IntToStr(FAllData.Count);
end;

constructor TPivotGrid.Create(const AParent: TUIArea; const ALayout: TObject; const AView: TView; const AId: string;
  const AParams: string);
var
  vPopupArea: TVCLArea;
  vPopupMenu: TPopupMenu;
begin
  FPivot := TcxPivotGrid.Create(nil);

  if ALayout is TPanel then
    FPivot.Align := TPanel(ALayout).Align
  else
    FPivot.Align := alClient;

  if Assigned(ALayout) and (ALayout is TPanel) and Assigned(TPanel(ALayout).PopupMenu) then
  begin
    vPopupArea := TVCLArea(AParent.AreaById('Popup'));
    { TODO -owa : Нужно найти другой способ привязки меню }
    if not Assigned(vPopupArea) and Assigned(AParent.Parent) then
      vPopupArea := TVCLArea(AParent.Parent.AreaById('Popup'));
    if Assigned(vPopupArea) then
    begin
      vPopupMenu := TPopupMenu(vPopupArea.Component);
      vPopupMenu.OnPopup := BeforeContextMenuShow;
      FPivot.PopupMenu := vPopupMenu;
    end;
  end;

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

  inherited Create(AParent, AView, AId, FPivot);

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
    vParams._SetFieldValue(nil, 'IsChecked', not vParams['IsChecked']);
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

constructor TTreeCollectionEditor.Create(const AParent: TUIArea; const ALayout: TObject; const AView: TView; const AId: string);
var
  vPopupArea: TVCLArea;
  vPopupMenu: TPopupMenu;
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

  if Assigned(ALayout) and (ALayout is TPanel) and Assigned(TPanel(ALayout).PopupMenu) then
  begin
    vPopupArea := TVCLArea(AParent.AreaById('Popup'));
    { TODO -owa : Нужно найти другой способ привязки меню }
    if not Assigned(vPopupArea) and Assigned(AParent.Parent) then
      vPopupArea := TVCLArea(AParent.Parent.AreaById('Popup'));
    if Assigned(vPopupArea) then
    begin
      vPopupMenu := TPopupMenu(vPopupArea.Component);
      vPopupMenu.OnPopup := BeforeContextMenuShow;
      FTreeList.PopupMenu := vPopupMenu;
    end;
  end;

  FTreeList.LookAndFeel.NativeStyle := False;
  FTreeList.LookAndFeel.Kind := lfFlat;

  if ALayout is TPanel then
    FTreeList.Align := TPanel(ALayout).Align
  else
    FTreeList.Align := alClient;

  FTreeList.Font.Size := 12;

  inherited Create(AParent, AView, AId, FTreeList);

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

procedure TTreeCollectionEditor.CreateLevels(const ALayout: TObject);
var
  vLayout: TPanel absolute ALayout;
  vTreeLevels: string;
  vList: TStrings;
  i: Integer;
  vLevel: TTreeLevel;

  procedure CreateLevel(const ADef: TDefinition);
  var
    vFieldDef: TFieldDef;
  begin
    vLevel := TTreeLevel.Create;
    for vFieldDef in ADef.Fields do
      if vFieldDef is TListFieldDef then
      begin
        vLevel.ListFieldNames.Add(vFieldDef.Name);
        FLevels.Add(vLevel);
      end;
  end;
begin
  FLevels := TList<TTreeLevel>.Create;
  //  Assert(Length(vTreeLevels) > 0, 'Parameter "TreeLevels" not defined in layout: ' + vLayout.Caption + '. Specify "TreeLevels=<ListField>[|<ChildListField>]');

  vTreeLevels := GetUrlParam(vLayout.Caption, 'TreeLevels');

  if Length(vTreeLevels) > 0 then
  begin
    vList := CreateDelimitedList(vTreeLevels, '|');

    for i := 0 to vList.Count - 1 do
    begin
      vLevel := TTreeLevel.Create;
      vLevel.ListFieldNames.Add(vList[i]);
      FLevels.Add(vLevel);
    end;

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
    vFieldName := GetLevel(ANode.Level).ListFieldNames[0];
    if vEntity.FieldExists(vFieldName) then
      Result := vEntity.GetFieldList(vFieldName).Count
    else
      Result := 0;
  end;
end;

function TTreeCollectionEditor.GetEntity(const ANode: TcxTreeListNode; const ACurrLevel: Integer): TEntity;
var
  vEnt: TEntity;
begin
  if ACurrLevel = 0 then
    Result := FAllData[ANode.Index]
  else
  begin
    vEnt := GetEntity(ANode.Parent, ANode.Level - 1);
    Result := vEnt.GetFieldList(GetLevel(ACurrLevel - 1).ListFieldNames[0])[ANode.Index];
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

constructor TTreeLevel.Create;
begin
  ListFieldNames := TStringList.Create;
  PaintListFieldNodes := True;
end;

destructor TTreeLevel.Destroy;
begin
  FreeAndNil(ListFieldNames);
  inherited;
end;

{ TEntityListSelectorMTM }

procedure TEntityListSelectorMTM.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEntityList);
  FControl := nil;
  FreeAndNil(FListBox);
end;

procedure TEntityListSelectorMTM.DoCreateControl(const ALayout: TObject);
var
  vListFieldDef: TListFieldDef;
  vTransitFieldName: string;
  vTransitField: TFieldDef;
  vInteractor: TInteractor;
begin
  inherited;
  FListBox := TcxCheckListBox.Create(nil);
  FControl := FListBox;

  Assert(FView.Definition is TListFieldDef, 'FView.Definition не является TListFieldDef');
  vListFieldDef := TListFieldDef(FView.Definition);
  vTransitFieldName := GetUrlParam(vListFieldDef.StyleName, 'transit');
  Assert(Length(vTransitFieldName) > 0, 'Не задано транзитное поле. Параметр transit.');
  Assert(vListFieldDef._ContentDefinition.FieldExists(vTransitFieldName), 'Указанное имя транзитного поля не существует: ' + vTransitFieldName);

  vTransitField := vListFieldDef._ContentDefinition.FieldByName(vTransitFieldName);
  Assert(vTransitField is TObjectFieldDef, 'Указанное транзитное поле не является TObjectFieldDef');

  FTransitField := TObjectFieldDef(vTransitField);

  vInteractor := TInteractor(FView.Interactor);

  FEntityList := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  TDomain(Domain).GetEntityList(FView.Session, FTransitField._ContentDefinition, FEntityList, '');

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
begin
  vSelectedList := TEntityList(FView.DomainObject);
  vList := TList<TEntity>.Create;

  for i := 0 to vSelectedList.Count - 1 do
    vList.Add(vSelectedList[i].ExtractEntity(FTransitField.Name));

  FListBox.Items.BeginUpdate;
  try
    FListBox.Items.Clear;

    for i := 0 to FEntityList.Count - 1 do
    begin
      vItem := FListBox.Items.Add;
      vItem.Text := FEntityList[i].DisplayName;
      vSelectedIndex := vList.IndexOf(FEntityList[i]);
      vItem.Checked := vSelectedIndex >= 0;
      if vItem.Checked then
        vItem.ItemObject := vSelectedList[vSelectedIndex]
      else
        vItem.ItemObject := FEntityList[i];
    end;
  finally
    FListBox.Items.EndUpdate;
    FreeAndNil(vList);
  end;
end;

function TEntityListSelectorMTM.GetDefaultFocusedChild: TWinControl;
begin
  Result := FListBox;
end;

function TEntityListSelectorMTM.GetLayoutPositionCount: Integer;
begin
  Result := 4;
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
        vTransitEntity := TEntityList(FView.DomainObject).AddEntity(AHolder, '');
        vTransitEntity._SetFieldEntity(AHolder, FTransitField.Name, vEntity);
        FListBox.Items[AIndex].ItemObject := vTransitEntity;
        Result := True;
      end, TChangeHolder(Holder));
  end
  else begin
    Assert(vEntity.Definition.IsDescendantOf(TEntityList(FView.DomainObject).MainDefinition.Name), 'К записи привязан объект неправильного типа');
    FListBox.Items[AIndex].ItemObject := vEntity.ExtractEntity(FTransitField.Name);
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

initialization

TPresenter.RegisterUIClass('WinVCL', uiListEdit, '', TColumnListEditor);
TPresenter.RegisterUIClass('WinVCL', uiListEdit, 'simple', TListEditor);
TPresenter.RegisterUIClass('WinVCL', uiListEdit, 'selector', TEntityListSelector); //deprecated
TPresenter.RegisterUIClass('WinVCL', uiListEdit, 'multiselect', TEntityListSelector2);
TPresenter.RegisterUIClass('WinVCL', uiListEdit, 'mtm', TEntityListSelectorMTM);
TPresenter.RegisterUIClass('WinVCL', uiListEdit, 'parameters', TParametersEditor);

end.

