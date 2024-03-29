﻿{ ---------------------------------------------------------------------------------
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
  --------------------------------------------------------------------------------- }

unit vclListEditors;

interface

uses
  Generics.Collections, Graphics, Classes, Controls, Types, CheckLst, ExtCtrls,

  uEntity, uEntityList, vclArea, uView, uUIBuilder, uDefinition, uConsts, uCollection, uLayout, Spin, Vcl.ComCtrls,
  Windows, valEdit;

type
  TEntityListSelectorMTM = class(TVCLControl) // many to many link
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

  TColumnBinding = class
  private
    FFieldDef: TFieldDef;
    FIsAscSort: Boolean;
  public
    constructor Create(const AFieldDef: TFieldDef);
  end;

  TGridEditor = class(TVCLControl)
  private
    FGrid: TListView;
    FAllData: TEntityList;
    FEntities: TList<TEntity>;
    FFilterText: string;
    FFilterDateFrom, FFilterDateTo: TDateTime;
    FFilterDateActive: Boolean;
    FLayoutExists: Boolean;
    FColumns: TObjectList<TColumnBinding>;
    procedure DoOnTableViewDblClick(Sender: TObject);
    procedure DoOnSelectItem(Sender: TObject; AItem: TListItem; ASelected: Boolean);
    procedure DoOnItemChecked(Sender: TObject; AItem: TListItem);
    procedure OnColumnClick(Sender: TObject; Column: TListColumn);
    procedure OnColumnResize(Sender: TCustomListview; columnindex: Integer; columnwidth: Integer);
    procedure CreateColumnsFromModel(const AFields: string = '');
    procedure EnableColumnParamsChangeHandlers(const AEnable: Boolean);
    procedure CreateColumn(const AFieldDef: TFieldDef; const AOverriddenCaption: string; const AWidth: Integer);
    procedure BeforeContextMenuShow(Sender: TObject);
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
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure DoExecuteUIAction(const AView: TView); override;
    procedure SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl); override;
    procedure DoAfterSetParent(const AParent: TUIArea); override;
  public
    property Columns: TObjectList<TColumnBinding> read FColumns;
  end;

implementation

uses
  TypInfo, SysUtils, Messages, Math, Variants, DateUtils, Dialogs, Menus, ShellApi, IOUtils, CommCtrl,

  uWinVCLPresenter, uObjectField, uInteractor, uEnumeration, uSession, uChangeManager,
  uConfiguration, uDomain, uQueryDef, uQuery, uUtils, uPresenter, uSettings;

type
  TLVColumnResizeEvent = procedure(Sender: TCustomListview; columnindex: Integer; columnwidth: Integer) of object;

  // добавляет событие OnColumnResize в TListview
  TPBExListview = class(TListview)
  private
    FBeginColumnResizeEvent: TLVColumnResizeEvent;
    FEndColumnResizeEvent: TLVColumnResizeEvent;
    FColumnResizeEvent: TLVColumnResizeEvent;
  protected
    procedure DoBeginColumnResize(columnindex, columnwidth: Integer); virtual;
    procedure DoEndColumnResize(columnindex, columnwidth: Integer); virtual;
    procedure DoColumnResize(columnindex, columnwidth: Integer); virtual;
    procedure WMNotify(var Msg: TWMNotify); message WM_NOTIFY;
    function FindColumnIndex(pHeader: pNMHdr): Integer;
    function FindColumnWidth(pHeader: pNMHdr): Integer;
    procedure CreateWnd; override;
  published
    property OnBeginColumnResize: TLVColumnResizeEvent read FBeginColumnResizeEvent write FBeginColumnResizeEvent;
    property OnEndColumnResize: TLVColumnResizeEvent read FEndColumnResizeEvent write FEndColumnResizeEvent;
    property OnColumnResize: TLVColumnResizeEvent read FColumnResizeEvent write FColumnResizeEvent;
  end;

procedure TPBExListview.DoBeginColumnResize(columnindex, columnwidth: Integer);
begin
  if Assigned(FBeginColumnResizeEvent) then
    FBeginColumnResizeEvent(Self, columnindex, columnwidth);
end;

procedure TPBExListview.DoEndColumnResize(columnindex, columnwidth: Integer);
begin
  if Assigned(FEndColumnResizeEvent) then
    FEndColumnResizeEvent(Self, columnindex, columnwidth);
end;

procedure TPBExListview.DoColumnResize(columnindex, columnwidth: Integer);
begin
  if Assigned(FColumnResizeEvent) then
    FColumnResizeEvent(Self, columnindex, columnwidth);
end;

function TPBExListview.FindColumnIndex(pHeader: pNMHdr): Integer;
var
  hwndHeader: HWND;
  iteminfo: THdItem;
  ItemIndex: Integer;
  buf: array [0..128] of Char;
begin
  Result := -1;
  hwndHeader := pHeader^.hwndFrom;
  ItemIndex := pHDNotify(pHeader)^.Item;
  FillChar(iteminfo, SizeOf(iteminfo), 0);
  iteminfo.Mask := HDI_TEXT;
  iteminfo.pszText := buf;
  iteminfo.cchTextMax := SizeOf(buf) - 1;
  Header_GetItem(hwndHeader, ItemIndex, iteminfo);
  if CompareStr(Columns[ItemIndex].Caption, iteminfo.pszText) = 0 then
    Result := ItemIndex
  else
  begin
    for ItemIndex := 0 to Columns.Count - 1 do
      if CompareStr(Columns[ItemIndex].Caption, iteminfo.pszText) = 0 then
      begin
        Result := ItemIndex;
        Break;
      end;
  end;
end;

procedure TPBExListview.WMNotify(var Msg: TWMNotify);
begin
  inherited;
  case Msg.NMHdr^.code of
    HDN_ENDTRACK:
      DoEndColumnResize(FindColumnIndex(Msg.NMHdr),
        FindColumnWidth(Msg.NMHdr));
    HDN_BEGINTRACK:
      DoBeginColumnResize(FindColumnIndex(Msg.NMHdr),
        FindColumnWidth(Msg.NMHdr));
    HDN_TRACK:
      DoColumnResize(FindColumnIndex(Msg.NMHdr),
        FindColumnWidth(Msg.NMHdr));
  end;
end;

procedure TPBExListview.CreateWnd;
var
  wnd: HWND;
begin
  inherited;
  wnd := GetWindow(Handle, GW_CHILD);
  SetWindowLong(wnd, GWL_STYLE,
    GetWindowLong(wnd, GWL_STYLE) and not HDS_FULLDRAG);
end;

function TPBExListview.FindColumnWidth(pHeader: pNMHdr): Integer;
begin
  Result := -1;
  if Assigned(PHDNotify(pHeader)^.pItem) and
    ((PHDNotify(pHeader)^.pItem^.mask and HDI_WIDTH) <> 0) then
    Result := PHDNotify(pHeader)^.pItem^.cxy;
end;

constructor TColumnBinding.Create(const AFieldDef: TFieldDef);
begin
  FFieldDef := AFieldDef;
  FIsAscSort := True;
end;

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
      Result := CompareEntities(AEnt1.ExtractEntity(AFieldDef.Name), AEnt2.ExtractEntity(AFieldDef.Name), nil, '')
    else if AFieldDef.Kind in [fkString .. fkCurrency] then
    begin
      Result := 0;
      vValue1 := AEnt1.ExtractFieldValue(AFieldDef.Name);
      vValue2 := AEnt2.ExtractFieldValue(AFieldDef.Name);
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
end;

procedure SetGridColumnParams(const AControl: TGridEditor; const AListView: TListView; const AColName: string;
  const AWidth, AColIndex, ARowIndex: Integer; const AVisible: Boolean; const ASortOrder: TSortOrder);
var
  vCol: TListColumn;
  function GetColumnByName: TListColumn;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to AListView.Columns.Count - 1 do
      if AControl.Columns[AListView.Columns[i].Tag].FFieldDef.Name = AColName then
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
  vCol.Index := AColIndex;
end;

function LoadGridColumnWidths(const ADomain: TObject; const AControl: TGridEditor;
  const AListView: TListView; const AObjectName: string): Boolean;
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

      SetGridColumnParams(AControl, AListView, vCols.Names[i], vWidth, vColIndex, vRowIndex, vVisible, vSortOrder);
    end;
  finally
    AListView.Items.EndUpdate;
    FreeAndNil(vCols);
    FreeAndNil(vValues)
  end;
end;

procedure SaveGridColumnWidths(const ADomain: TObject; const AControl: TGridEditor;
  const AListView: TListView; const AObjectName: string);
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
      cColWidthDelim + IntToStr(0) + cColWidthDelim + cColWidthDelim;
    vSettings.SetValue(vSectionName, AControl.Columns[vColumn.Tag].FFieldDef.Name, vValues);
  end;
end;

function CustomSortProc(const AItem1, AItem2: TListItem; const ABinding: TColumnBinding): Integer; stdcall;
var
  vEnt1, vEnt2: TEntity;
  vFieldDef: TFieldDef;
begin
  vEnt1 := TEntity(AItem1.Data);
  vEnt2 := TEntity(AItem2.Data);
  vFieldDef := ABinding.FFieldDef;

  Result := CompareEntities(vEnt1, vEnt2, vFieldDef, '');
  if not ABinding.FIsAscSort then
    Result := -Result;
  ABinding.FIsAscSort := not ABinding.FIsAscSort;
end;

{ TGridEditor }

procedure TGridEditor.OnColumnClick(Sender: TObject; Column: TListColumn);
var
  vColumnBinding: TColumnBinding;
begin
  vColumnBinding := FColumns[Column.Tag];
  FGrid.CustomSort(@CustomSortProc, NativeInt(vColumnBinding));
end;

procedure TGridEditor.OnColumnResize(Sender: TCustomListview; columnindex, columnwidth: Integer);
begin
  SaveColumnWidths(Sender);
end;

procedure TGridEditor.DoOnItemChecked(Sender: TObject; AItem: TListItem);
begin
  AItem.Selected := AItem.Checked;
end;

procedure TGridEditor.DoOnSelectItem(Sender: TObject; AItem: TListItem; ASelected: Boolean);
var
  vEntityList: TEntityList;
  vItem: TListItem;
  vList: TList<TEntity>;
  i: Integer;
begin
  vEntityList := TEntityList(TView(FView).DomainObject);
  vList := TList<TEntity>.Create;
  try
    vItem := FGrid.Selected;
    for i := 0 to FGrid.SelCount - 1 do
    begin
      vList.Add(vItem.Data);
      vItem := FGrid.GetNextItem(vItem, sdBelow, [isSelected]);
    end;

    vEntityList.SelectEntities(vList);
  finally
    FreeAndNil(vList);
  end;
end;

procedure TGridEditor.BeforeContextMenuShow(Sender: TObject);
var
  vMenu: TPopupMenu;
  vMenuItem: TMenuItem;
  vArea: TVCLControl;
  vParams: TEntity;
  i: Integer;
begin
  vMenu := TPopupMenu(Sender);
  if vMenu.Items.Count <= 0 then
    Exit;

  for i := 0 to vMenu.Items.Count - 1 do
  begin
    vMenuItem := vMenu.Items[i];
    vArea := TVCLControl(vMenuItem.Tag);
    if not Assigned(vArea) then
      Continue;

    if not Assigned(vArea.View) then
      Continue;

    if vArea.View.DefinitionKind <> dkAction then
      Continue;

    if vArea.View.Name = '#GroupByColumn' then
    begin
      vParams := TEntity(vArea.View.DomainObject);
      if vParams['IsChecked'] <> vMenuItem.Checked then
        vParams._SetFieldValue(nil, 'IsChecked', vMenuItem.Checked);
    end;
  end;

  vMenuItem := vMenu.Items[vMenu.Items.Count - 1];
  vMenuItem.Caption := TInteractor(FInteractor).Translate('txtRecordCount', 'Записей') + ': ' + IntToStr(FAllData.Count);
end;

procedure TGridEditor.CreateColumnsFromModel(const AFields: string = '');
var
  i: Integer;
  vFieldDef: TFieldDef;
  vWidth: Integer;
  vMainDefinition: TDefinition;
  vFields: TStrings;
  vFieldName: string;
begin
  FGrid.Columns.BeginUpdate;
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
        CreateColumn(vFieldDef, GetFieldTranslation(vFieldDef), vWidth);
      end;
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

          vWidth := TWinVCLPresenter(TInteractor(FView.Interactor).Presenter).GetWidthByType(100, vFieldDef);

          CreateColumn(vFieldDef, GetFieldTranslation(vFieldDef), vWidth);
        end;
      finally
        vFields.Free;
      end;
    end;
  finally
    FGrid.Columns.EndUpdate;
  end;
end;

procedure TGridEditor.CreateColumn(const AFieldDef: TFieldDef; const AOverriddenCaption: string;
  const AWidth: Integer);
var
  vCol: TListColumn;
  vIndex: Integer;
begin
  vCol := FGrid.Columns.Add;
  vCol.Caption := AOverriddenCaption;
  vCol.Width := Round(AWidth * 1.5);
  vIndex := FColumns.Add(TColumnBinding.Create(AFieldDef));
  vCol.Tag := vIndex;
end;

function TGridEditor.CreateRow(AEntity: TEntity): Boolean;
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

procedure TGridEditor.FillRow(AEntity: TEntity);
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

function TGridEditor.GetSearchType: uConsts.TSearchType;
begin
  // todo:
  { if FAllData.Filler is TEntityField then
    Result := TEntityField(FAllData.Filler).SearchType
    else }
  Result := stSearchFromBegin;
end;

procedure TGridEditor.ExportToExcel;
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

  if FileExists(vFileName) then
    TPresenter(FPresenter).ShowYesNoDialog('Export', 'Хотите открыть этот файл?', False,
      procedure(const AResult: TDialogResult)
      begin
        if AResult = drYes then
          TPresenter(FPresenter).OpenFile(vFileName);
      end);
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

function TGridEditor.IsMatchToFilter(const AEntity: TEntity): Boolean;
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

function TGridEditor.GetValue(AEntity: TEntity; AColumn: TListColumn): Variant;
var
  vFieldDef: TFieldDef;
  vFieldName: string;
  vEnumeration: TEnumeration;
begin
  Result := null;
  try
    if not Assigned(AEntity) then
      Exit;

    vFieldDef := FColumns[AColumn.Tag].FFieldDef;
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
        Result := vEnumeration.Items[NativeInt(AEntity.FieldByName(vFieldDef.Name).Value)].DisplayText
      else
        Result := AEntity.GetStateCaption(vFieldName);
    end;
  except
    on E: Exception do
      if Assigned(AEntity) then
        TDomain(AEntity.Domain).Logger.AddMessage('Ошибка получения данных из модели. Поле [' + vFieldName + ']');
  end;
end;

procedure TGridEditor.UpdateArea(const AKind: Word; const AParameter: TEntity = nil);
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
      end;
    dckListAdded: // 16  //OnAddNewEntity
      begin
        CreateRow(AParameter);
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

procedure TGridEditor.EnableColumnParamsChangeHandlers(const AEnable: Boolean);
begin
  if AEnable then
    FGrid.OnColumnDragged := SaveColumnWidths
  else
    FGrid.OnColumnDragged := nil;
end;

procedure TGridEditor.SaveColumnWidths(Sender: TObject);
begin
  SaveGridColumnWidths(TInteractor(FView.Interactor).Domain, Self, FGrid, FView.InitialName);
end;

procedure TGridEditor.SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl);
var
  vPopupMenu: TPopupMenu;
begin
  if ATargetName = 'popup' then
  begin
    vPopupMenu := TPopupMenu(TVCLControl(ALinkedControl).Control);
    vPopupMenu.OnPopup := BeforeContextMenuShow;
    FGrid.PopupMenu := vPopupMenu;
  end;
end;

procedure TGridEditor.DoAfterSetParent(const AParent: TUIArea);
var
  vFields: string;
begin
  if FGrid.Columns.Count > 0 then Exit; //avoid repeated call of CreateColumnsFromModel

  EnableColumnParamsChangeHandlers(False);

  if Assigned(FCreateParams) then
  begin
    vFields := FCreateParams.Values['fields'];
    if FCreateParams.Values['editingInGrid'] = 'true' then
    begin
      FGrid.ReadOnly := False;
    end;
  end
  else
    vFields := '';

  if vFields = '' then
    vFields := AParent.QueryParameter('fields');

  vFields := AParent.QueryParameter('fields');
  CreateColumnsFromModel(vFields);

  LoadColumnWidths;

  EnableColumnParamsChangeHandlers(True);
end;

procedure TGridEditor.LoadColumnWidths;
begin
  FLayoutExists := LoadGridColumnWidths(TInteractor(FView.Interactor).Domain, Self, FGrid, FView.InitialName);
end;

procedure TGridEditor.DoBeforeFreeControl;
begin
  if Assigned(FGrid.PopupMenu) then
  begin
    TPopupMenu(FGrid.PopupMenu).OnPopup := nil;
    FGrid.PopupMenu := nil;
  end;

  FAllData := nil;
  FreeAndNil(FEntities);
  FreeAndNil(FColumns);
end;

function TGridEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FGrid := TPBExListview.Create(nil);
  Result := FGrid;

  FEntities := TList<TEntity>.Create;
  FAllData := TEntityList(FView.DomainObject);
  FColumns := TObjectList<TColumnBinding>.Create;

  FGrid.ViewStyle := vsReport;
  FGrid.MultiSelect := True;
  FGrid.HideSelection := True;
  FGrid.RowSelect := True;
  FGrid.Checkboxes := StrToBoolDef(TDomain(FDomain).UserSettings.GetValue('Core', 'MouseMultiSelectInGrids'), False);
  FGrid.GridLines := StrToBoolDef(TDomain(TInteractor(FInteractor).Domain).UserSettings.GetValue('Core', 'ShowHorzLines'), True);
  FGrid.Color := GetBGColor(FAllData.MainDefinition, clWindow);
  FGrid.Font.Size := 10;
  FGrid.ReadOnly := True;

  FGrid.OnItemChecked := DoOnItemChecked;
  FGrid.OnDblClick := DoOnTableViewDblClick;
  FGrid.OnSelectItem := DoOnSelectItem;
  FGrid.OnColumnClick := OnColumnClick;
  TPBExListview(FGrid).OnEndColumnResize := OnColumnResize;

  if ALayout.Kind = lkPanel then
    FGrid.Align := TAlign(ALayout.Align)
  else
    FGrid.Align := alClient;
end;

procedure TGridEditor.DoExecuteUIAction(const AView: TView);
var
  vDateFrom, vDateTo: TDateTime;
  vIsPeriodActive: Boolean;
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
end;

procedure TGridEditor.DoOnTableViewDblClick(Sender: TObject);
begin
  OnTableViewDblClick(FView, FOwner);
end;

{ TEntityListSelectorMTM }

procedure TEntityListSelectorMTM.DoBeforeFreeControl;
begin
  inherited;
  FreeAndNil(FEntityList);
  FreeAndNil(FCheckList);
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
  Result := FListBox;
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

  TDomain(FDomain).GetEntityList(FView.Session, FTransitField._ContentDefinition, FEntityList, '');

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
      end, TChangeHolder(FOwner.Holder));
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
      end, TChangeHolder(FOwner.Holder));
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
begin
  FillList;
end;

initialization

TPresenter.RegisterControlClass('Windows.VCL', uiListEdit, '', TGridEditor);
TPresenter.RegisterControlClass('Windows.VCL', uiListEdit, 'mtm', TEntityListSelectorMTM);

TPresenter.RegisterControlClass('Windows.VCL', uiCollection, '', TGridEditor);

end.
