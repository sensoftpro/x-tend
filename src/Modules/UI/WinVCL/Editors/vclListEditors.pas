{ ---------------------------------------------------------------------------------
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
  --------------------------------------------------------------------------------- }

unit vclListEditors;

interface

uses
  Generics.Collections, Graphics, Classes, Controls, Types, CheckLst, ExtCtrls,

  uEntity, uEntityList, vclArea, uView, uUIBuilder, uDefinition, uConsts, uCollection, uLayout,
  Spin, Vcl.ComCtrls, windows, valEdit;

type
  TEntityListSelectorMTM = class(TFieldArea) // many to many link
  private
    FListBox: TCheckListBox;
    FEntityList: TEntityList;
    FCheckList: TList<Boolean>;
    FTransitField: TObjectFieldDef;
    procedure FillList;
    procedure OnClickCheck(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    procedure DoOnChange; override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
  end;

  TCollectionEditor = class(TUIArea)
  private
    FGrid: TListView;
    FAllData: TEntityList;
    FEntities: TList<TEntity>;
    FFilterText: string;
    FFilterDateFrom, FFilterDateTo: TDateTime;
    FFilterDateActive: Boolean;
    FLayoutExists: Boolean;
    procedure DoOnCompare(Sender: TObject; AItem1, AItem2: TListItem; Data: Integer; var Compare: Integer);
    procedure DoOnTableViewDblClick(Sender: TObject);
    procedure DoOnSelectItem(Sender: TObject; AItem: TListItem; ASelected: Boolean);
    procedure DoOnItemChecked(Sender: TObject; AItem: TListItem);
    procedure CreateColumnsFromModel(const AFields: string = '');
    procedure EnableColumnParamsChangeHandlers(const AEnable: Boolean);
    procedure CreateColumn(const AFieldDef: TFieldDef; const AOverriddenCaption: string; const AWidth: Integer);

    procedure SaveColumnWidths(Sender: TObject);
    procedure LoadColumnWidths;
    {function FindColumnByFieldName(const AFieldName: string): TListColumn;}

    procedure FillRow(AEntity: TEntity);
    function CreateRow(AEntity: TEntity): Boolean;
    function GetValue(AEntity: TEntity; AColumn: TListColumn): Variant;

    function IsMatchToFilter(const AEntity: TEntity): Boolean;
    function GetSearchType: uConsts.TSearchType;
    procedure ExportToExcel;
  protected
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure DoExecuteUIAction(const AView: TView); override;
  public
    constructor Create(const AParent: TUIArea; const AView: TView; const ALayout: TLayout; const AId: string;
      const AIsService: Boolean = False; const AControl: TObject = nil; const AParams: string = ''); override;
    destructor Destroy; override;

    procedure BeforeContextMenuShow(Sender: TObject); // не нужен?
  end;

  TColumnListEditor = class(TFieldArea)
  private
    FGrid: TListView;
    FAllData: TEntityList;
    FEntities: TList<TEntity>;
    FLayoutExists: Boolean;
    procedure DoOnCompare(Sender: TObject; AItem1, AItem2: TListItem; Data: Integer; var Compare: Integer);
    procedure DoOnTableViewDblClick(Sender: TObject);
    Procedure DoOnItemChecked(Sender: TObject; AItem: TListItem);
    procedure DoOnSelectItem(Sender: TObject; AItem: TListItem; ASelected: Boolean);
    procedure CreateColumnsFromModel(const AFields: string = '');
    procedure EnableColumnParamsChangeHandlers(const AEnable: Boolean);
    procedure CreateColumn(const AFieldDef: TFieldDef; const AOverriddenCaption: string; const AWidth: Integer);
    procedure SaveColumnWidths(Sender: TObject);
    procedure LoadColumnWidths;
    function CreateRow(AEntity: TEntity): Boolean;
    procedure FillRow(AEntity: TEntity);
    function GetValue(AEntity: TEntity; AColumn: TListColumn): Variant;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
  public
    procedure BeforeContextMenuShow(Sender: TObject); // не нужен?
  end;

implementation

uses
  TypInfo, SysUtils, Messages, Math, Variants, DateUtils, Dialogs, Menus, ShellApi, IOUtils,

  uObjectField, uInteractor, uEnumeration, uSession, uChangeManager,
  uConfiguration, uDomain, uQueryDef, uQuery, uUtils, uPresenter, uSettings;

type
  TCalculateStyleFunc = function(const AViewName: string; const AEntity: TEntity): TColor of object;

const
  cColWidthDelim = ',';

function GetRelColor(const ASourceColor: TColor; const ARelRed, ARelGreen, ARelBlue: Integer): TColor;
var
  vR, vG, vB: Integer;
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
  if ADefinition = nil then
    Exit;

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
    else if AFieldDef.Kind in [fkString .. fkCurrency] then
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
  else
  begin
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
  // vDblClickView.ExecuteAction(AArea.Holder);
end;

procedure SetGridColumnParams(const AListView: TListView; const AColName: string;
  const AWidth, AColIndex, ARowIndex: Integer; const AVisible: Boolean; const ASortOrder: TSortOrder);
var
  vCol: TListColumn;
  function GetColumnByName: TListColumn;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to AListView.Columns.Count - 1 do
      if TFieldDef(AListView.Columns[i].tag).Name = AColName then
      begin
        Result := AListView.Columns[i];
        Break;
      end;
  end;

begin
  vCol := GetColumnByName;
  if vCol = nil then
    Exit;

  vCol.Width := AWidth;
  try
    vCol.Index := AColIndex;
  except
  end;
end;

function LoadGridColumnWidths(const ADomain: TObject; const AListView: TListView; const AObjectName: string): Boolean;
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
  // vOnSortingChanged: TNotifyEvent;
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
  else
  begin
    vCols := TStringList.Create;
    vSettings.ReadSection(vSectionName, vCols);
  end;

  vValues := CreateDelimitedList('', cColWidthDelim);
  // vOnSortingChanged := AMasterTableView.DataController.OnSortingChanged;
  // AMasterTableView.DataController.OnSortingChanged := nil;
  AListView.Items.BeginUpdate;
  try
    for i := 0 to vCols.Count - 1 do
    begin
      vValues.DelimitedText := vCols.ValueFromIndex[i];
      vWidth := 50;
      vColIndex := 0;
      vRowIndex := 0;
      vSortOrder := uConsts.soNone;
      vVisible := True;
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

      SetGridColumnParams(AListView, vCols.Names[i], vWidth, vColIndex, vRowIndex, vVisible, vSortOrder);
    end;
  finally
    AListView.Items.EndUpdate;
    // AMasterTableView.DataController.OnSortingChanged := vOnSortingChanged;
    FreeAndNil(vCols);
    FreeAndNil(vValues)
  end;
end;

procedure SaveGridColumnWidths(const ADomain: TObject; const AListView: TListView; const AObjectName: string);
var
  vSectionName: string;
  vSettings: TSettings;
  vColumn: TListColumn;
  vValues: string;
  i: Integer;
begin
  vSettings := TDomain(ADomain).UserSettings;
  vSectionName := 'Columns$' + AObjectName;
  for i := 0 to AListView.Columns.Count - 1 do
  begin
    vColumn := AListView.Columns[i];
    vValues := IntToStr(AListView.Columns[i].Width) + cColWidthDelim + IntToStr(AListView.Columns[i].Index) +
      cColWidthDelim + IntToStr(0) + cColWidthDelim { +
      BoolToStr(AListView.Columns[i].Visible) } + cColWidthDelim { +
      IntToStr(Integer(AListView.Columns[i].SortOrder)) };
    vSettings.SetValue(vSectionName, TFieldDef(vColumn.tag).Name, vValues);
  end;
end;

{ TCollectionEditor }

// Events

procedure TCollectionEditor.DoOnCompare(Sender: TObject; AItem1, AItem2: TListItem; Data: Integer;
  var Compare: Integer);
// var
// vSortColumn: TListColumn;
begin
  if not Assigned(AItem1) or not Assigned(AItem2) then
  begin
    Compare := 0;
    Exit;
  end;

  // Compare := CompareEntities(TEntity(AItem1.Data), TEntity(AItem2.Data), vColumnBinding.FieldDef,
  // vColumnBinding.FieldName);
end;

procedure TCollectionEditor.DoOnItemChecked(Sender: TObject; AItem: TListItem);
var
  vEntityList: TEntityList;
  vItem: TListItem;
  vList: TList<TEntity>;
  i: Integer;
begin
  if not AItem.Checked then
    AItem.Selected := False
  else
    AItem.Selected := True;
  vEntityList := TEntityList(TView(FView).DomainObject);
  vItem := FGrid.Selected;
  vList := TList<TEntity>.Create;
  try

    for i := 0 to TListView(Sender).SelCount - 1 do
    begin
      vList.Add(vItem.Data);
      vItem := FGrid.GetNextItem(vItem, sdBelow, [isSelected]);
    end;

    vEntityList.SelectEntities(vList);
  finally
    FreeAndNil(vList);
  end;
end;

procedure TCollectionEditor.DoOnSelectItem(Sender: TObject; AItem: TListItem; ASelected: Boolean);
begin
  if AItem.Selected then
    AItem.Checked := True
  Else
    AItem.Checked := False;
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
    vArea := TUIArea(vMenuItem.tag);
    if not Assigned(vArea) then
      Continue;

    if not Assigned(vArea.View) then
      Continue;

    if vArea.View.DefinitionKind <> dkAction then
      Continue;

    if vArea.View.Name = '#GroupByColumn' then
    begin
      vParams := TEntity(vArea.View.DomainObject);
      // vMenuItem.Checked := FGrid.Items.;
      if vParams['IsChecked'] <> vMenuItem.Checked then
        vParams._SetFieldValue(nil, 'IsChecked', vMenuItem.Checked);
    end;
  end;

  vMenuItem := vMenu.Items[vMenu.Items.Count - 1];
  vMenuItem.Caption := TInteractor(Interactor).Translate('txtRecordCount', 'Записей') + ': ' + IntToStr(FAllData.Count);
end;

// Creating

constructor TCollectionEditor.Create(const AParent: TUIArea; const AView: TView; const ALayout: TLayout; const AId: string;
      const AIsService: Boolean = False; const AControl: TObject = nil; const AParams: string = '');
var
  // vDefinition: TDefinition;
  // vView: TView;
  // vColumn: TListColumn;
  vFields: string;
begin
  FGrid := TListView.Create(nil);
  FGrid.Items := TListItems.Create(FGrid);
  FEntities := TList<TEntity>.Create;

  FGrid.ViewStyle := vsReport;
  FGrid.MultiSelect := True;
  FGrid.HideSelection := True;
  FGrid.RowSelect := True;
  FGrid.GridLines := True;

  FGrid.OnCompare := DoOnCompare;
  FGrid.OnItemChecked := DoOnItemChecked;
  FGrid.OnDblClick := DoOnTableViewDblClick;
  FGrid.OnSelectItem := DoOnSelectItem;

  inherited Create(AParent, AView, ALayout, AId, AIsService, FGrid, AParams);

  if StrToBoolDef(TDomain(TInteractor(FView.Interactor).Domain).UserSettings.GetValue('Core', 'MouseMultiSelectInGrids'), True) then
    FGrid.Checkboxes := True
  else
    FGrid.Checkboxes := False;

  if ALayout.Kind = lkPanel then
    FGrid.Align := TPanel(ALayout).Align
  else
    FGrid.Align := alClient;

  // after inherited Create, Interactor must be initialized
  // cxSetResourceString(@scxGridGroupByBoxCaption, TInteractor(Interactor).Translate('txtMoveColumnForGrouping',
  // 'Перетащите сюда колонку для группировки'));
  FAllData := TEntityList(FView.DomainObject);

  FGrid.Color := GetBGColor(FAllData.MainDefinition, clWindow);
  FGrid.Font.Size := 12;

  EnableColumnParamsChangeHandlers(False);

  vFields := AParent.QueryParameter('fields');
  CreateColumnsFromModel(vFields);
  LoadColumnWidths;

  EnableColumnParamsChangeHandlers(True);

  // vDefinition := FAllData.MainDefinition;
  // FGroupFieldName := Trim(AParent.QueryParameter('Group'));
  // if (FGroupFieldName = '') and Assigned(vDefinition) then
  // FGroupFieldName := Trim(vDefinition.GroupFieldName);
  // if FGroupFieldName = '-' then
  // FGroupFieldName := '';
  //
  // FColorFieldName := Trim(AParent.QueryParameter('Color'));
  // if (FColorFieldName = '') and Assigned(vDefinition) then
  // FColorFieldName := Trim(vDefinition.ColorFieldName);
  // if FColorFieldName = '-' then
  // FColorFieldName := '';
  //
  // if (FGroupFieldName <> '') and (FAllData.FillerKind = lfkDefinition) then
  // begin
  // vColumn := FindColumnByFieldName(FGroupFieldName);
  // if Assigned(vColumn) then
  // begin
  // vColumn.Width := 0;
  // // vColumn.GroupIndex := 0;
  // vView := FView.BuildView('#GroupByColumn');
  // TEntity(vView.DomainObject)._SetFieldValue(nil, 'IsChecked', True);
  // // FMasterTableView.OptionsView.GroupByBox := True;
  // end;
  // end;
end;

procedure TCollectionEditor.CreateColumnsFromModel(const AFields: string = '');
var
  i: Integer;
  vFieldDef: TFieldDef;
  vWidth: Integer;
  vMainDefinition: TDefinition;
  vFields: TStrings;
  vFieldName: string;
begin
  FGrid.Items.BeginUpdate;
  try
    vMainDefinition := FAllData.MainDefinition;
    if AFields = '' then
    begin
      for vFieldDef in vMainDefinition.Fields do
      begin
        if TInteractor(FView.Interactor).NeedSkipColumn(FAllData, vFieldDef) then
          Continue;

        vWidth := TPresenter(TInteractor(FView.Interactor).Presenter).GetWidthByType(100, vFieldDef);
        { TODO -cUI refactoring : Перенести описание аггрегаций в слой описания UI }
        CreateColumn(vFieldDef, GetFieldTranslation(vFieldDef), vWidth);
      end;

      // Очень сильно тормозит на прокрутке списков
      // if FAllData.ContentDefinitions.Count > 1 then
      // CreateColumn(nil, '', 50, uConsts.soNone, akNotDefined);
    end
    else
    begin
      vFields := CreateDelimitedList(AFields);
      try
        for i := 0 to vFields.Count - 1 do
        begin
          vFieldName := vFields[i];
          vFieldDef := vMainDefinition.ExtractFieldDef(vFieldName);
          if not Assigned(vFieldDef) then
            Continue;

          vWidth := TPresenter(TInteractor(FView.Interactor).Presenter).GetWidthByType(100, vFieldDef);

          CreateColumn(vFieldDef, GetFieldTranslation(vFieldDef), vWidth);
        end;
      finally
        vFields.Free;
      end;
    end;
  finally
    FGrid.Items.EndUpdate;
  end;
end;

procedure TCollectionEditor.CreateColumn(const AFieldDef: TFieldDef; const AOverriddenCaption: string;
  const AWidth: Integer);
var
  vCol: TListColumn;
begin
  vCol := FGrid.Columns.Add;
  vCol.Caption := AOverriddenCaption;
  vCol.Width := round(AWidth * 1.5);
  vCol.tag := NativeInt(AFieldDef);
end;

function TCollectionEditor.CreateRow(AEntity: TEntity): Boolean;
var
  vListItem: TListItem;
  i: Integer;
begin
  Result := True;
  if FEntities.IndexOf(AEntity) >= 0 then
    Exit(False);
  FEntities.Add(AEntity);
  vListItem := FGrid.Items.Add;
  vListItem.Data := AEntity;
  for i := 1 to FGrid.Columns.Count - 1 do
    vListItem.SubItems.Add('');
end;

procedure TCollectionEditor.FillRow(AEntity: TEntity);
var
  vListItem: TListItem;
  vValue: Variant;
  i: Integer;
begin
  if Assigned(AEntity) then
  begin
    vListItem := FGrid.FindData(0, AEntity, True, False);
    for i := 0 to FGrid.Columns.Count - 1 do
    begin
      vValue := GetValue(AEntity, FGrid.Columns[i]);
      if (vValue = null) then
        vValue := '';
      if i = 0 then
        vListItem.Caption := vValue
      else
        vListItem.SubItems[i - 1] := vValue;
    end;
  end;
end;

// Services

destructor TCollectionEditor.Destroy;
begin
  if Assigned(FGrid.PopupMenu) then
  begin
    TPopupMenu(FGrid.PopupMenu).OnPopup := nil;
    FGrid.PopupMenu := nil;
  end;

  FAllData := nil;
  FreeAndNil(FEntities);

  inherited Destroy;
end;

function TCollectionEditor.GetSearchType: uConsts.TSearchType;
begin
  // todo:
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
  if not GetFileName(vFileName) then
    Exit;

  // ExportGridToExcel(vFileName, FGrid, True, True, False);

  if FileExists(vFileName) and (TPresenter(Presenter).ShowYesNoDialog('Export', 'Хотите открыть этот файл?') = drYes)
  then
    TPresenter(Presenter).OpenFile(vFileName);
end;

{function TCollectionEditor.FindColumnByFieldName(const AFieldName: string): TListColumn;
var
  i: Integer;
  vName: string;
begin
  Result := nil;
  for i := 0 to FGrid.Columns.Count - 1 do
  begin
    vName := FGrid.Columns[i].Caption;
    if vName = AFieldName then
    begin
      Result := FGrid.Columns[i];
      Break;
    end;
  end;
end;   }

function TCollectionEditor.IsMatchToFilter(const AEntity: TEntity): Boolean;
var
  vFilterText, vText: string;
  vST: uConsts.TSearchType;
  vDate: TDateTime;
begin
  Result := True;

  vFilterText := Trim(FFilterText);
  if Length(vFilterText) > 0 then
  begin
    vFilterText := AnsiUpperCase(vFilterText);

    if FGrid.Columns.Count = 1 then
    begin
      vText := AnsiUpperCase(AEntity.DisplayName);
      vST := GetSearchType;
      Result := ((vST = stSearchFromBegin) and (Pos(vFilterText, vText) = 1)) or
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
    if (FFilterDateFrom = -700000) and (FFilterDateTo = -700000) then
      Exit;

    vDate := AEntity['DocDate'];
    vDate := Trunc(vDate);

    if (FFilterDateFrom = -700000) then
      Result := vDate <= FFilterDateTo
    else if (FFilterDateTo = -700000) then
      Result := vDate >= FFilterDateFrom
    else
      Result := (vDate >= FFilterDateFrom) and (vDate <= FFilterDateTo);
  end;
end;

function TCollectionEditor.GetValue(AEntity: TEntity; AColumn: TListColumn): Variant;
var
  vFieldDef: TFieldDef;
  vFieldName: string;
  vEnumeration: TEnumeration;
begin
  Result := null;
  try
    if not Assigned(AEntity) then
      Exit;

    vFieldDef := TFieldDef(AColumn.tag);
    vFieldName := vFieldDef.Name;

    if vFieldDef = nil then
    begin
      Result := AEntity.DisplayName;
      Exit;
    end;

    Result := AEntity.ExtractFieldValue(vFieldName);
    if VarIsNull(Result) then
      Exit;

    if vFieldDef.Kind = fkDateTime then
    begin
      if (TDateTime(Result) < 2) and (vFieldDef.StyleName <> 'time') then
        Result := null
      else if vFieldDef.StyleName = 'date' then // cut the time
        Result := Trunc(Result);
    end
    else if vFieldDef.Kind = fkFloat then
      Result := RoundTo(Result, -2)
    else if vFieldDef.Kind = fkEnum then
    begin
      vEnumeration := TDomain(AEntity.Domain).Configuration.Enumerations.ObjectByName
        (TSimpleFieldDef(vFieldDef).Dictionary);
      if Assigned(vEnumeration) then
        Result := vEnumeration.Items[Integer(AEntity.FieldByName(vFieldDef.Name).Value)].DisplayText
      else
        Result := AEntity.GetStateCaption(vFieldName);
    end;
  except
    on E: Exception do
      if Assigned(AEntity) then
        TDomain(AEntity.Domain).Logger.AddMessage('Ошибка получения данных из модели. Поле [' + vFieldName + ']');
  end;
end;

procedure TCollectionEditor.UpdateArea(const AKind: Word; const AParameter: TEntity = nil);
var
  i: Integer;
  vEntity: TEntity;
  vRow: TListItem;
begin
  case AKind of
    dckFieldChanged, dckFilterChanged: // 1, 128
      begin
        FGrid.OnSelectItem := nil;
        FGrid.OnItemChecked := nil;
        FGrid.Items.BeginUpdate;
        try
          FGrid.Clear;

          FAllData.Resort;
          FEntities.Clear;
          for i := 0 to FAllData.Count - 1 do
          begin
            vEntity := FAllData[i];
            if not vEntity.Deleted and IsMatchToFilter(vEntity) then
            begin
              if CreateRow(vEntity) then
                FillRow(vEntity);
            end
          end;
          FGrid.Update;
          FGrid.Selected := FGrid.Items[0];
        finally
          FGrid.Items.EndUpdate;
          FGrid.Selected := nil;
          FGrid.OnSelectItem := DoOnSelectItem;
          FGrid.OnItemChecked := DoOnItemChecked;
        end;

        if not FLayoutExists then
        begin
          // TODO: Разобраться!
          // FMasterTableView.ApplyBestFit;
          // SaveColumnWidths;
        end;
      end;
    dckEntityDeleted: // 2
      begin

      end;
    dckEntityChanged, dckNameChanged: // 4, 512
      begin
        FGrid.OnSelectItem := nil;
        FGrid.OnItemChecked := nil;
        try
          FillRow(AParameter);
        finally
          FGrid.OnSelectItem := DoOnSelectItem;
          FGrid.OnItemChecked := DoOnItemChecked;
        end
      end;
    dckSelectionChanged: // 8
      begin
//        vNewFocusedRowIndex := FAllData.Selection.IndexOf(AParameter);
//        if vNewFocusedRowIndex <> FGrid.ItemIndex then
//        begin
//          vRow := FGrid.Items[vNewFocusedRowIndex];
//        end;
      end;
    dckListAdded: // 16  //OnAddNewEntity
      begin
        CreateRow(AParameter);
        // FillRow(AParameter);
      end;
    dckListRemoved: // 32 //OnCancelAddingNewEntity, OnDeleteEntity
      begin
        vRow := FGrid.FindData(0, AParameter, True, False);
        if Assigned(vRow) then
        begin
          FGrid.Items.Delete(vRow.Index);
          FEntities.Remove(vRow.Data);
        end;
      end;
    dckViewStateChanged: // 64
      begin

      end;
    dckEntitySaved: // 256
      begin

      end;
  end;
  // else if AKind = dckEntitySaved then
  // begin
  // end
  // else if AKind = dckListAdded then
  // begin
  // end
  // else if AKind = dckListRemoved then
  // begin
  // end
  // else if AKind = dckSelectionChanged then
  // begin
  // end
  // else if AKind = dckViewStateChanged then
  // // Сделать что-то с видимостью
  // else // if AKind = dckFieldChanged then
  // begin // FieldChanged, FilterChanged
  // end;
end;

procedure TCollectionEditor.EnableColumnParamsChangeHandlers(const AEnable: Boolean);
begin
  if AEnable then
    FGrid.OnColumnDragged := SaveColumnWidths
  else
    FGrid.OnColumnDragged := nil;
end;

procedure TCollectionEditor.SaveColumnWidths;
begin
  SaveGridColumnWidths(TInteractor(FView.Interactor).Domain, FGrid, FView.InitialName);
end;

procedure TCollectionEditor.LoadColumnWidths;
begin
  FLayoutExists := LoadGridColumnWidths(TInteractor(FView.Interactor).Domain, FGrid, FView.InitialName);
end;

procedure TCollectionEditor.DoExecuteUIAction(const AView: TView);
var
  // vParams: TEntity;
  vDateFrom, vDateTo: TDateTime;
  vIsPeriodActive: Boolean;
  // i: Integer;
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

    if (not SameDate(vDateFrom, FFilterDateFrom)) or (not SameDate(vDateTo, FFilterDateTo)) or
      (vIsPeriodActive <> FFilterDateActive) then
    begin
      if vDateFrom = cNullDateTime then
        FFilterDateFrom := -700000
      else
        FFilterDateFrom := vDateFrom;

      if vDateTo = cNullDateTime then
        FFilterDateTo := -700000
      else
        FFilterDateTo := vDateTo;

      FFilterDateActive := vIsPeriodActive;

      UpdateArea(dckFilterChanged, nil);
    end;
  end;
  // else if AView.Name = '#GroupByColumn' then
  // begin
  // vParams := TEntity(AView.DomainObject);
  // vParams._SetFieldValue(nil, 'IsChecked', not vParams['IsChecked']);
  // FMasterTableView.OptionsView.GroupByBox := vParams['IsChecked'];
  // if not FMasterTableView.OptionsView.GroupByBox then // remove groups
  // for i := 0 to FMasterTableView.ColumnCount - 1 do
  // if FMasterTableView.Columns[i].GroupIndex >= 0 then
  // begin
  // FMasterTableView.Columns[i].Visible := True;
  // FMasterTableView.Columns[i].GroupIndex := -1;
  // end;
  // end
  // else if AView.Name = '#ApplyBestFit' then
  // FMasterTableView.ApplyBestFit;
end;

procedure TCollectionEditor.DoOnTableViewDblClick(Sender: TObject);
begin
  OnTableViewDblClick(FView, Self);
end;

{ TColumnListEditor }

// Events

procedure TColumnListEditor.DoOnCompare(Sender: TObject; AItem1, AItem2: TListItem; Data: Integer;
  var Compare: Integer);
begin
  if not Assigned(AItem1) or not Assigned(AItem2) then
  begin
    Compare := 0;
    Exit;
  end;

  // vSortColumn := FMasterTableView.Columns[AItemIndex];
  // vColumnBinding := TColumnBinding(vSortColumn.DataBinding.Data);
  // Compare := CompareEntities(FMasterDS.Data[ARecordIndex1], FMasterDS.Data[ARecordIndex2], vColumnBinding.FieldDef,
  // vColumnBinding.FieldName);
end;

procedure TColumnListEditor.DoBeforeFreeControl;
begin
  if Assigned(FGrid.PopupMenu) then
  begin
    TPopupMenu(FGrid.PopupMenu).OnPopup := nil;
    FGrid.PopupMenu := nil;
  end;

  FAllData := nil;
  FreeAndNil(FEntities);

  FGrid.OnSelectItem := nil;
end;

procedure TColumnListEditor.DoOnSelectItem(Sender: TObject; AItem: TListItem; ASelected: Boolean);
begin
  if AItem.Selected then
    AItem.Checked := True
  Else
    AItem.Checked := False;
end;

procedure TColumnListEditor.DoOnItemChecked(Sender: TObject; AItem: TListItem);
var
  vEntityList: TEntityList;
  vItem: TListItem;
  vList: TList<TEntity>;
  i: Integer;
begin
  if AItem.Checked then
    AItem.Selected := True
  else
    AItem.Selected := False;
  vEntityList := TEntityList(TView(FView).DomainObject);
  vItem := FGrid.Selected;
  vList := TList<TEntity>.Create;
  try

    for i := 0 to TListView(Sender).SelCount - 1 do
    begin
      vList.Add(vItem.Data);
      vItem := FGrid.GetNextItem(vItem, sdBelow, [isSelected]);
    end;

    vEntityList.SelectEntities(vList);
  finally
    FreeAndNil(vList);
  end;
end;

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
    vArea := TUIArea(vMenuItem.tag);
    if not Assigned(vArea) then
      Continue;

    if not Assigned(vArea.View) then
      Continue;

    if vArea.View.DefinitionKind <> dkAction then
      Continue;

    if vArea.View.Name = '#GroupByColumn' then
    begin
      vParams := TEntity(vArea.View.DomainObject);
      // vMenuItem.Checked := FGrid.OptionsView.GroupByBox;
      if vParams['IsChecked'] <> vMenuItem.Checked then
        vParams._SetFieldValue(nil, 'IsChecked', vMenuItem.Checked);
    end;
  end;

  vMenuItem := vMenu.Items[vMenu.Items.Count - 1];
  vMenuItem.Caption := TInteractor(Interactor).Translate('txtRecordCount', 'Записей') + ': ' + IntToStr(FAllData.Count);
end;

procedure TColumnListEditor.DoOnTableViewDblClick(Sender: TObject);
begin
  OnTableViewDblClick(FView, Self);
end;

// Creating

function TColumnListEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vFields: string;
begin
  FGrid := TListView.Create(nil);
  FGrid.Items := TListItems.Create(FGrid);
  FEntities := TList<TEntity>.Create;

  FGrid.ViewStyle := vsReport;
  FGrid.MultiSelect := True;
  FGrid.RowSelect := True;
  FGrid.GridLines := True;

  FGrid.OnCompare := DoOnCompare;
  FGrid.OnItemChecked := DoOnItemChecked;
  FGrid.OnDblClick := DoOnTableViewDblClick;
  FGrid.OnSelectItem := DoOnSelectItem;

  // FMasterTableView.OptionsView.Header := TObjectFieldDef(FFieldDef).SortType <> estSortByOrder;
  if StrToBoolDef(TDomain(TInteractor(FView.Interactor).Domain).UserSettings.GetValue('Core',
    'MouseMultiSelectInGrids'), True) then
    FGrid.Checkboxes := True
  else
    FGrid.Checkboxes := False;

  // FMasterTableView.OnKeyDown := OnFilterKeyDown;

  if ALayout.Kind = lkPanel then
    FGrid.Align := TPanel(ALayout).Align
  else
    FGrid.Align := alClient;

  // cxSetResourceString(@scxGridGroupByBoxCaption, TInteractor(Interactor).Translate('txtMoveColumnForGrouping',
  // 'Перетащите сюда колонку для группировки'));
  // FMasterTableView.OptionsView.NoDataToDisplayInfoText := TInteractor(Interactor)
  // .Translate('@Grid@NoDataText', '<Нет данных>');
  FAllData := TEntityList(FView.DomainObject);

  FGrid.Color := GetBGColor(FAllData.MainDefinition, clWindow);
  FGrid.Font.Size := 12;
  Result := FGrid;

  // if Assigned(FCreateParams) then
  // begin
  // vFields := FCreateParams.Values['fields'];
  // end
  // else
  // vFields := '';

  // if vFields = '' then
  vFields := AParent.QueryParameter('fields');

  CreateColumnsFromModel(vFields);
  LoadColumnWidths;
end;

procedure TColumnListEditor.CreateColumnsFromModel(const AFields: string = '');
var
  i: Integer;
  vFieldDef: TFieldDef;
  vWidth: Integer;
  vMainDefinition: TDefinition;
  vFields: TStrings;
  vFieldName: string;
begin
  EnableColumnParamsChangeHandlers(False);
 // FGrid.Items.BeginUpdate;
  try
    vMainDefinition := FAllData.MainDefinition;
    if AFields = '' then
    begin
      for vFieldDef in vMainDefinition.Fields do
      begin
        if TInteractor(FView.Interactor).NeedSkipColumn(FAllData, vFieldDef) then
          Continue;

        vWidth := TPresenter(TInteractor(FView.Interactor).Presenter).GetWidthByType(100, vFieldDef);
        { TODO -cUI refactoring : Перенести описание аггрегаций в слой описания UI }
        // vAggType := vMainDefinition.Reductions.ReductionForField(vFieldDef.Name);
        CreateColumn(vFieldDef, GetFieldTranslation(vFieldDef), vWidth);
      end;

      // if FAllData.ContentDefinitions.Count > 1 then
      // CreateColumn(nil, '', 50, uConsts.soNone, akNotDefined);
    end
    else
    begin
      vFields := CreateDelimitedList(AFields);
      try
        for i := 0 to vFields.Count - 1 do
        begin
          vFieldName := vFields[i];
          vFieldDef := vMainDefinition.ExtractFieldDef(vFieldName);
          if not Assigned(vFieldDef) then
            Continue;

          vWidth := TPresenter(Presenter).GetWidthByType(100, vFieldDef);
          // vAggType := TDefinition(vFieldDef.Definition).Reductions.ReductionForField(vFieldName);

          CreateColumn(vFieldDef, GetFieldTranslation(vFieldDef), vWidth);
        end;
      finally
        vFields.Free;
      end;
    end;
  finally
//    FGrid.Items.EndUpdate;
    EnableColumnParamsChangeHandlers(True);
  end;
end;

procedure TColumnListEditor.CreateColumn(const AFieldDef: TFieldDef; const AOverriddenCaption: string;
  const AWidth: Integer);
var
  vCol: TListColumn;
begin
  vCol := FGrid.Columns.Add;
  vCol.Caption := AOverriddenCaption;
  vCol.Width := round(AWidth * 1.5);
  vCol.tag := NativeInt(AFieldDef);
end;

function TColumnListEditor.CreateRow(AEntity: TEntity): Boolean;
var
  vListItem: TListItem;
  i: Integer;
begin
  Result := True;
  if FEntities.IndexOf(AEntity) >= 0 then
    Exit(False);
  FEntities.Add(AEntity);
  vListItem := FGrid.Items.Add;
  vListItem.Data := AEntity;
  for i := 1 to FGrid.Columns.Count - 1 do
    vListItem.SubItems.Add('');
end;

procedure TColumnListEditor.FillRow(AEntity: TEntity);
var
  vListItem: TListItem;
  vValue: Variant;
  i: Integer;
begin
  vListItem := FGrid.FindData(0, AEntity, True, False);
  for i := 0 to FGrid.Columns.Count - 1 do
  begin
    vValue := GetValue(AEntity, FGrid.Columns[i]);
    if (vValue = null) then
      vValue := '';
    if i = 0 then
      vListItem.Caption := vValue
    else
      vListItem.SubItems[i - 1] := vValue;
  end;
end;

// procedure TColumnListEditor.DoOnFocusedRecordChanged(Sender: TcxCustomGridTableView;
// APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
// var
// vEntityList: TEntityList;
// begin
// if APrevFocusedRecord = AFocusedRecord then
// Exit;
//
// vEntityList := TEntityList(TView(FView).DomainObject);
// if not Assigned(AFocusedRecord) then
// // vEntityList.SelectEntity(nil)
// else
// begin
// vEntityList.SelectEntity(TEntity(FGrid.Selected.Data));
// end;
// end;

function TColumnListEditor.GetValue(AEntity: TEntity; AColumn: TListColumn): Variant;
var
  vFieldDef: TFieldDef;
  vFieldName: string;
  vEnumeration: TEnumeration;
begin
  Result := null;
  try
    if not Assigned(AEntity) then
      Exit;

    vFieldDef := TFieldDef(AColumn.tag);
    vFieldName := vFieldDef.Name;

    if vFieldDef = nil then
    begin
      Result := AEntity.DisplayName;
      Exit;
    end;

    Result := AEntity.ExtractFieldValue(vFieldName);
    if VarIsNull(Result) then
      Exit;

    if vFieldDef.Kind = fkDateTime then
    begin
      if (TDateTime(Result) < 2) and (vFieldDef.StyleName <> 'time') then
        Result := null
      else if vFieldDef.StyleName = 'date' then // cut the time
        Result := Trunc(Result);
    end
    else if vFieldDef.Kind = fkFloat then
      Result := RoundTo(Result, -2)
    else if vFieldDef.Kind = fkEnum then
    begin
      vEnumeration := TDomain(AEntity.Domain).Configuration.Enumerations.ObjectByName
        (TSimpleFieldDef(vFieldDef).Dictionary);
      if Assigned(vEnumeration) then
        Result := vEnumeration.Items[Integer(AEntity.FieldByName(vFieldDef.Name).Value)].DisplayText
      else
        Result := AEntity.GetStateCaption(vFieldName);
    end;
  except
    on E: Exception do
      if Assigned(AEntity) then
        TDomain(AEntity.Domain).Logger.AddMessage('Ошибка получения данных из модели. Поле [' + vFieldName + ']');
  end;
end;

procedure TColumnListEditor.EnableColumnParamsChangeHandlers(const AEnable: Boolean);
begin
  if AEnable then
    FGrid.OnColumnDragged := SaveColumnWidths
  else
    FGrid.OnColumnDragged := nil;
end;

procedure TColumnListEditor.LoadColumnWidths;
begin
  FLayoutExists := LoadGridColumnWidths(TInteractor(FView.Interactor).Domain, FGrid, FFieldDef.FullName);
end;

procedure TColumnListEditor.SaveColumnWidths(Sender: TObject);
begin
  SaveGridColumnWidths(TInteractor(FView.Interactor).Domain, FGrid, FFieldDef.FullName);
end;

procedure TColumnListEditor.UpdateArea(const AKind: Word; const AParameter: TEntity);
var
  i: Integer;
  vEntity: TEntity;
  vRow: TListItem;
begin
  case AKind of
    dckFieldChanged, dckFilterChanged: // 1, 128
      begin
        FGrid.OnSelectItem := nil;
        FGrid.OnItemChecked := nil;
        FGrid.Items.BeginUpdate;
        try
          FGrid.Clear;

          FAllData.Resort;
          for i := 0 to FAllData.Count - 1 do
          begin
            vEntity := FAllData[i];
            if not vEntity.Deleted then
            begin
              if CreateRow(vEntity) then
                FillRow(vEntity);
            end
          end;
          FGrid.Update;
          FGrid.Selected := FGrid.Items[0];
        finally
          FGrid.Items.EndUpdate;
          FGrid.Selected := nil;
          FGrid.OnSelectItem := DoOnSelectItem;
          FGrid.OnItemChecked := DoOnItemChecked;
        end;

        if not FLayoutExists then
        begin
          // TODO: Разобраться!
          // FMasterTableView.ApplyBestFit;
          // SaveColumnWidths;
        end;
      end;
    dckEntityDeleted: // 2
      begin

      end;
    dckEntityChanged, dckNameChanged: // 4, 512
      begin
        FGrid.OnSelectItem := nil;
        FGrid.OnItemChecked := nil;
        try
          FillRow(AParameter);
        finally
          FGrid.OnSelectItem := DoOnSelectItem;
          FGrid.OnItemChecked := DoOnItemChecked;
        end
      end;
    dckSelectionChanged: // 8
      begin
//        vNewFocusedRowIndex := FAllData.Selection.IndexOf(AParameter);
//        if vNewFocusedRowIndex <> FGrid.ItemIndex then
//        begin
//          vRow := FGrid.Items[vNewFocusedRowIndex];
//        end;
      end;
    dckListAdded: // 16  //OnAddNewEntity
      begin
        CreateRow(AParameter);
        // FillRow(AParameter);
      end;
    dckListRemoved: // 32 //OnCancelAddingNewEntity, OnDeleteEntity
      begin
        vRow := FGrid.FindData(0, AParameter, True, False);
        if Assigned(vRow) then
        begin
          FGrid.Items.Delete(vRow.Index);
          FEntities.Remove(vRow.Data);
        end;
      end;
    dckViewStateChanged: // 64
      begin

      end;
    dckEntitySaved: // 256
      begin

      end;
  end;
end;

{ TEntityListSelectorMTM }

procedure TEntityListSelectorMTM.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEntityList);
  FreeAndNil(FListBox);
  FCheckList.Free;
end;

function TEntityListSelectorMTM.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vListFieldDef: TListFieldDef;
  vTransitFieldName: string;
  vTransitField: TFieldDef;
  vInteractor: TInteractor;
begin
  inherited;
  FListBox := TCheckListBox.Create(nil);
  FCheckList := TList<Boolean>.Create;

  Assert(FView.Definition is TListFieldDef, 'FView.Definition не является TListFieldDef');
  vListFieldDef := TListFieldDef(FView.Definition);
  vTransitFieldName := GetUrlParam(vListFieldDef.StyleName, 'transit');
  Assert(Length(vTransitFieldName) > 0, 'Не задано транзитное поле. Параметр transit.');
  Assert(vListFieldDef._ContentDefinition.FieldExists(vTransitFieldName),
    'Указанное имя транзитного поля не существует: ' + vTransitFieldName);

  vTransitField := vListFieldDef._ContentDefinition.FieldByName(vTransitFieldName);
  Assert(vTransitField is TObjectFieldDef, 'Указанное транзитное поле не является TObjectFieldDef');

  FTransitField := TObjectFieldDef(vTransitField);

  vInteractor := TInteractor(FView.Interactor);

  FEntityList := TEntityList.Create(vInteractor.Domain, vInteractor.Session);

  TDomain(Domain).GetEntityList(FView.Session, FTransitField._ContentDefinition, FEntityList, '');

  FListBox.OnClickCheck := OnClickCheck;
  Result := FListBox;
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
  vItem: Integer;
  vSelectedList: TEntityList;
  vList: TList<TEntity>;
  vSelectedIndex: Integer;
begin
  vSelectedList := TEntityList(FView.DomainObject);
  vList := TList<TEntity>.Create;
  try
    for i := 0 to vSelectedList.Count - 1 do
      vList.Add(vSelectedList[i].ExtractEntity(FTransitField.Name));

    FListBox.Items.BeginUpdate;
    try
      FListBox.Items.Clear;
      FCheckList.Clear;

      for i := 0 to FEntityList.Count - 1 do
      begin
        vItem := FListBox.Items.Add(FEntityList[i].DisplayName);
        vSelectedIndex := vList.IndexOf(FEntityList[i]);
        FListBox.Checked[vItem] := vSelectedIndex >= 0;
        FCheckList.Add(vSelectedIndex >= 0);
        if FCheckList[vItem] then
          FListBox.Items.Objects[vItem] := vSelectedList[vSelectedIndex]
        else
          FListBox.Items.Objects[vItem] := FEntityList[i];
      end;
    finally
      FListBox.Items.EndUpdate;
    end;
  finally
    FreeAndNil(vList);
  end;
end;

procedure TEntityListSelectorMTM.OnClickCheck(Sender: TObject);
var
  vEntity: TEntity;
  AIndex: Integer;
begin
  for AIndex := 0 to FCheckList.Count do
    if FListBox.Checked[AIndex] <> FCheckList[AIndex] then
      Break;
  FCheckList[AIndex] := FListBox.Checked[AIndex];
  vEntity := TEntity(FListBox.Items.Objects[AIndex]);
  if FCheckList[AIndex] then
  begin
    Assert(vEntity.Definition.IsDescendantOf(FEntityList.MainDefinition.Name),
      'К записи привязан объект неправильного типа');
    TUserSession(FView.Session).AtomicModification(nil,
      function(const AHolder: TChangeHolder): Boolean
      var
        vTransitEntity: TEntity;
      begin
        vTransitEntity := TEntityList(FView.DomainObject).AddEntity(AHolder, '', FTransitField.Name, [NativeInt(vEntity)]);
//        vTransitEntity._SetFieldEntity(AHolder, FTransitField.Name, vEntity);
        FListBox.Items.Objects[AIndex] := vTransitEntity;
        Result := True;
      end, TChangeHolder(Holder));
  end
  else
  begin
    Assert(vEntity.Definition.IsDescendantOf(TEntityList(FView.DomainObject).MainDefinition.Name),
      'К записи привязан объект неправильного типа');
    FListBox.Items.Objects[AIndex] := vEntity.ExtractEntity(FTransitField.Name);
    TUserSession(FView.Session).AtomicModification(nil,
      function(const AHolder: TChangeHolder): Boolean
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
{ var
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
    if (not vEntity.Deleted) {IsMatchToFilter(vEnt)) } // then
  { FMasterDS.Data.Add(vEntity);
    end;

    FMasterDS.Update;
    finally
    FMasterTableView.EndUpdate;
    FMasterTableView.DataController.ClearSelection;
    FMasterTableView.DataController.FocusedRecordIndex := -1;
    end;
    if not FLayoutExists then
    FMasterTableView.ApplyBestFit;
    end; }
end;

initialization

TPresenter.RegisterUIClass('Windows.VCL', uiListEdit, '', TColumnListEditor); // !
//TPresenter.RegisterUIClass('Windows.VCL', uiListEdit, 'simple', TListEditor);
TPresenter.RegisterUIClass('Windows.VCL', uiListEdit, 'mtm', TEntityListSelectorMTM);

TPresenter.RegisterUIClass('Windows.VCL', uiCollection, '', TCollectionEditor); // !

//TPresenter.RegisterControlClass('Windows.VCL', uiListEdit, '', TColumnListEditor); // !
////TPresenter.RegisterControlClass('Windows.VCL', uiListEdit, 'simple', TListEditor);
//TPresenter.RegisterControlClass('Windows.VCL', uiListEdit, 'mtm', TEntityListSelectorMTM);
//
//TPresenter.RegisterControlClass('Windows.VCL', uiCollection, '', TCollectionEditor); // !

end.
