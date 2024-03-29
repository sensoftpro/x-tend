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

unit fmxListEditors;

interface

uses
  System.Generics.Collections, System.Classes, System.Types, System.Rtti,
  FMX.Graphics, FMX.Controls, FMX.ExtCtrls, FMX.StdCtrls, FMX.Grid, FMX.Grid.Style,
  FMX.ListBox,

  uEntity, uEntityList, uView, uUIBuilder, uDefinition, uConsts, uLayout, fmxArea;

type
  TFMXGridEditor = class(TFMXControl)
  private
    FGrid: TGrid;
    FAllData: TEntityList;
    FFilterText: string;
    FFilterDateFrom, FFilterDateTo: TDateTime;
    FFilterDateActive: Boolean;
    FLayoutExists: Boolean;
    FGroupFieldName: string;
    FColorFieldName: string;
    procedure DoOnCellDblClick(const Column: TColumn; const Row: Integer);
    procedure DoGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure DoSetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure DoOnSelectionChanged(Sender: TObject);
    procedure DoOnDrawCellBackground(Sender: TObject; const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
    const Row: Integer; const Value: TValue; const State: TGridDrawStates);
    procedure DoOnDrawCell(Sender: TObject; const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
    const Row: Integer; const Value: TValue; const State: TGridDrawStates);

    procedure CreateColumnsFromModel(const AFields: string = '');
    procedure EnableColumnParamsChangeHandlers(const AEnable: Boolean);
    procedure CreateColumn(const AFieldDef: TFieldDef; const AFieldName: string; const AOverriddenCaption: string;
      const AWidth: Integer; const ASortOrder: TSortOrder; const AAgg: TAggregationKind);
    procedure BeforeContextMenuShow(Sender: TObject);
    procedure SaveColumnWidths;
    procedure DoOnColumnPosChanged(Column: TColumn; FromIndex, ToIndex: Integer);
    procedure DoOnColumnSizeChanged(Sender: TObject);
    procedure DoOnHeaderClick(Sender: TColumn);
    procedure LoadColumnWidths;
    procedure CreateColumnForAction(const AActionDef: TActionDef; const AFieldName: string);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
    procedure DoExecuteUIAction(const AView: TView); override;
    procedure DoBeforeFreeControl; override;
    procedure SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl); override;
  end;

  TFMXEntityListSelectorMTM = class(TFMXControl) // many to many link
  private
    FListBox: TListBox;
    FEntityList: TEntityList;
    FTransitFieldDef: TObjectFieldDef;
    FTransitDefinition: TDefinition;
    FFilterName: string;
    procedure FillList;
    procedure DoOnChangeCheck(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
  end;

  TFMXEntityListSelector = class(TFMXControl)
  private
    FListBox: TListBox;
    FEntityList: TEntityList;
    procedure DoOnChangeCheck(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    procedure DoOnChange; override;
  end;

  TFMXEntityListSelector3 = class(TFMXControl)
  private
    FListBox: TListBox;
    FLookupCollection: string;
    FLookupField: string;
    FMasterList: TEntityList;
    FDestroing: Boolean;
    function FindMasterItem(const ALookupItem: TEntity): TEntity;
    procedure DoOnChangeCheck(Sender: TObject);
    procedure FillLookupList;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure FillEditor; override;
    procedure SwitchChangeHandlers(const AHandler: TNotifyEvent); override;
    procedure DoDisableContent; override;
  end;

implementation

uses
  TypInfo, SysUtils, Math, Variants, DateUtils, IOUtils, StrUtils, UITypes,
  FMX.Menus, FMX.Types, FMX.Consts, FMX.Dialogs,
  uObjectField, uInteractor, uEnumeration, uSession, uChangeManager, uCollection,
  uConfiguration, uDomain, uQueryDef, uQuery, uUtils, uPresenter, uFMXPresenter, uSettings;

type
  TCalculateStyleFunc = function(const AViewName: string; const AEntity: TEntity): TAlphaColor of object;

const
  cColWidthDelim = ',';

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
    if (AFieldDef = nil) or (AFieldDef.Definition.Kind = clkMixin) then
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

procedure SetGridColumnParams(const AGrid: TGrid; const AColName: string; const AWidth,
  AColIndex, ARowIndex: Integer; const AVisible: Boolean; const ASortOrder: TSortOrder);
var
  vCol: TColumn;
  function GetColumnByName: TColumn;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to AGrid.ColumnCount - 1 do
      if AGrid.Columns[i].Name = AColName then
      begin
        Result := AGrid.Columns[i];
        Break;
      end;
  end;
begin
  vCol := GetColumnByName;
  if vCol = nil then Exit;

  vCol.Width := AWidth;
  vCol.Index := AColIndex;
  vCol.Visible := AVisible;
end;

function LoadGridColumnWidths(const ADomain: TObject; const AGrid: TGrid; const AObjectName: string): Boolean;
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
  else begin
    vCols := TStringList.Create;
    vSettings.ReadSection(vSectionName, vCols);
  end;

  vValues := CreateDelimitedList('', cColWidthDelim);
  AGrid.BeginUpdate;
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

      SetGridColumnParams(AGrid, vCols.Names[i], vWidth, vColIndex, vRowIndex, vVisible, vSortOrder);
    end;
  finally
    AGrid.EndUpdate;
    FreeAndNil(vCols);
    FreeAndNil(vValues)
  end;
end;

procedure SaveGridColumnWidths(const ADomain: TObject; const AGrid: TGrid; const AObjectName: string);
var
  vSectionName: string;
  vSettings: TSettings;
  vColumn: TColumn;
  vValues: string;
  i: Integer;
begin
  vSettings := TDomain(ADomain).UserSettings;
  vSectionName := 'Columns$' + AObjectName;
  for i := 0 to AGrid.ColumnCount - 1 do
  begin
    vColumn := AGrid.Columns[i];
    vValues := FloatToStr(AGrid.Columns[i].Width) + cColWidthDelim +
      IntToStr(AGrid.Columns[i].Index) + cColWidthDelim +
      IntToStr(0) + cColWidthDelim +
      BoolToStr(AGrid.Columns[i].Visible) + cColWidthDelim;
    vSettings.SetValue(vSectionName, vColumn.Name, vValues);
  end;
end;

procedure FillEnumList(const ADomain: TDomain; const AItems: TStrings; const AFieldDef: TFieldDef);
var
  vEnumItem: TEnumItem;
  vEnum: TEnumeration;
begin
  // TODO Обработать то, что привязка может быть некорректной
  vEnum := ADomain.Configuration.Enumerations.ObjectByName(TSimpleFieldDef(AFieldDef).Dictionary);
  if not Assigned(vEnum) then
    vEnum := ADomain.Configuration.StateMachines.ObjectByName(TSimpleFieldDef(AFieldDef).Dictionary);

  AItems.BeginUpdate;
  try
    AItems.Clear;
    for vEnumItem in vEnum do
    begin
      if not AFieldDef.HasFlag(cRequired) or (vEnumItem.ID > 0) then
        AItems.Add(vEnumItem.DisplayText);
    end;
  finally
    AItems.EndUpdate;
  end;
end;

function GetRowColor(const AArea: TUIArea; const AEntity: TEntity; const AColorFieldName: string): TColor;
var
  vDefinition: TDefinition;
  vColorField: TBaseField;
  vState: TState;
begin
  Result := cNullColor;

  if Assigned(AEntity) then
  begin
    vDefinition := AEntity.Definition;
    if LowerCase(AColorFieldName) = 'script' then
      Result := TCalculateStyleFunc(TConfiguration(vDefinition.Configuration).CalculateStyleFunc)(AArea.View.InitialName, AEntity)
    else if vDefinition.ColorTarget <> ctNone then
    begin
      Result := cNullColor;
      vColorField := AEntity.FieldByName(AColorFieldName);
      if vColorField.FieldKind = fkColor then
      begin
        if Assigned(vColorField) then
          Result := vColorField.Value;
      end
      else if vColorField.FieldKind = fkEnum then
      begin
        vState := AEntity.EntityState;
        if Assigned(vState) then
          Result := vState.Color;
      end;
    end;
  end;

  if Result = cNullColor then
    Result := TColorRec.SysNone;
end;

{ TFMXGridEditor }

procedure TFMXGridEditor.BeforeContextMenuShow(Sender: TObject);
var
  vMenu: TPopupMenu;
  vMenuItem: TMenuItem;
  vArea: TUIArea;
//  vParams: TEntity;
  i: Integer;
begin
  vMenu := TPopupMenu(Sender);
  if vMenu.ItemsCount <= 0 then
    Exit;

  for i := 0 to vMenu.ItemsCount - 1 do
  begin
    vMenuItem := vMenu.Items[i];
    vArea := TUIArea(vMenuItem.Tag);
    if not Assigned(vArea) then
      Continue;

    if not Assigned(vArea.View) then
      Continue;

    if vArea.View.DefinitionKind <> dkAction then
      Continue;

//    if vArea.View.Name = '#GroupByColumn' then
//    begin
//      vParams := TEntity(vArea.View.DomainObject);
//      vMenuItem.IsChecked := FMasterTableView.OptionsView.GroupByBox;
//      if vParams['IsChecked'] <> vMenuItem.IsChecked then
//        vParams._SetFieldValue(TUserSession(FView.Session).NullHolder, 'IsChecked', vMenuItem.IsChecked);
//    end;
  end;

  vMenuItem := vMenu.Items[vMenu.ItemsCount - 1];
  vMenuItem.Text := TInteractor(FInteractor).Translate('txtRecordCount', 'Записей') + ': ' + IntToStr(FAllData.Count);
end;

function TFMXGridEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vDefinition: TDefinition;
  vFields: string;
  i: integer;
begin
  FGrid := TGrid.Create(nil);
  Result := FGrid;
  FGrid.Options := [TGridOption.AlternatingRowBackground, TGridOption.ColumnResize, TGridOption.ColumnMove, TGridOption.Header,
    TGridOption.AutoDisplacement, TGridOption.ColLines];
  FGrid.ShowHint := True;

  if StrToBoolDef(TDomain(TInteractor(FInteractor).Domain).UserSettings.GetValue('Core', 'ShowHorzLines'), True) then
    FGrid.Options := FGrid.Options + [TGridOption.RowLines];

  if ALayout.Kind = lkPanel then
    FGrid.Align := AlignToAlignLayout(ALayout.Align)
  else
    FGrid.Align := TAlignLayout.Client;

  FGrid.TextSettings.Font.Size := 12;
  FGrid.StyledSettings := FGrid.StyledSettings - [TStyledSetting.Size];

  // after inherited Create, Interactor must be initialized
  FAllData := TEntityList(FView.DomainObject);
  FGrid.RowCount := FAllData.Count;

//  FBGStyle.Color := GetBGColor(FAllData.MainDefinition, clWindow);

  EnableColumnParamsChangeHandlers(False);

  if Assigned(FCreateParams) then
  begin
    vFields := FCreateParams.Values['fields'];
    if FCreateParams.Values['editingInGrid'] = 'true' then
    begin
      FGrid.Options := FGrid.Options + [TGridOption.Editing] - [TGridOption.RowSelect];
      FGrid.OnCellDblClick := nil;
      FGrid.OnSetValue := DoSetValue;
    end;
  end
  else
    vFields := '';

  if vFields = '' then
    vFields := AParent.QueryParameter('fields');

  CreateColumnsFromModel(vFields);
  LoadColumnWidths;

  FGrid.OnGetValue := DoGetValue;

  EnableColumnParamsChangeHandlers(True);

  for i := 0 to FGrid.ColumnCount - 1 do
    FGrid.Columns[i].OnResized := DoOnColumnSizeChanged;

  FGrid.OnCellDblClick := DoOnCellDblClick;
  FGrid.OnSelChanged := DoOnSelectionChanged;
  FGrid.OnHeaderClick := DoOnHeaderClick;

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

  FGrid.OnDrawColumnBackground := DoOnDrawCellBackground;
  FGrid.OnDrawColumnCell := DoOnDrawCell;
end;

procedure TFMXGridEditor.CreateColumnsFromModel(const AFields: string = '');
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
  FGrid.BeginUpdate;
  try
    vMainDefinition := FAllData.MainDefinition;
    if AFields = '' then
    begin
      for vFieldDef in vMainDefinition.Fields do
      begin
        if TInteractor(FView.Interactor).NeedSkipColumn(FAllData, vFieldDef) then
          Continue;

        vWidth := TFMXPresenter(TInteractor(FView.Interactor).Presenter).GetWidthByType(100, vFieldDef);
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
            vWidth := TFMXPresenter(TInteractor(FView.Interactor).Presenter).GetWidthByType(100, vFieldDef);
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
    FGrid.EndUpdate;
  end;
end;

procedure TFMXGridEditor.DoBeforeFreeControl;
begin
  if Assigned(FGrid.PopupMenu) then
  begin
    TPopupMenu(FGrid.PopupMenu).OnPopup := nil;
    FGrid.PopupMenu := nil;
  end;

  FAllData := nil;
end;

procedure TFMXGridEditor.EnableColumnParamsChangeHandlers(const AEnable: Boolean);
begin
  if AEnable then
    FGrid.OnColumnMoved := DoOnColumnPosChanged
  else
    FGrid.OnColumnMoved := nil;
end;

{procedure TFMXGridEditor.ShowFooter(const AColumn: TcxGridColumn; const AAgg: TAggregationKind);
begin
  if AAgg = akNotDefined then Exit;
  FMasterTableView.OptionsView.Footer := True;

  if AAgg = akCount then
    AColumn.FooterKind := skCount
  else if AAgg = akSum then
  begin
    AColumn.Summary.FooterKind := skSum;
    AColumn.Summary.FooterFormat := '0.00';
  end
  else if AAgg = akAverage then
    AColumn.Summary.FooterKind := skAverage;
end;     }

procedure TFMXGridEditor.CreateColumn(const AFieldDef: TFieldDef; const AFieldName: string;
  const AOverriddenCaption: string; const AWidth: Integer;
  const ASortOrder: TSortOrder; const AAgg: TAggregationKind);
var
  vCol: TColumn;
begin
  if Assigned(AFieldDef) then
  begin
    case AFieldDef.Kind of
      fkInteger, fkColor:
          vCol := TIntegerColumn.Create(FGrid);
      fkFloat:
        begin
          vCol := TFloatColumn.Create(FGrid);
          if AFieldDef.Format <> '' then
            TFloatColumn(vCol).DecimalDigits := DecimalDigitsFromFieldFormat(GetDisplayFormat(AFieldDef, FView.ParentDomainObject as TEntity));
        end;
      fkCurrency:
        begin
          vCol := TCurrencyColumn.Create(FGrid);
          if AFieldDef.Format <> '' then
            TCurrencyColumn(vCol).DecimalDigits := DecimalDigitsFromFieldFormat(GetDisplayFormat(AFieldDef, FView.ParentDomainObject as TEntity));
        end;
      fkDateTime:
        if AFieldDef.StyleName = 'time' then
          begin
            vCol := TTimeColumn.Create(FGrid);
            TTimeColumn(vCol).Format := GetUrlParam(AFieldDef.StyleName, 'format');
          end
        else
          begin
            vCol := TDateColumn.Create(FGrid);
            TDateColumn(vCol).Format := GetUrlParam(AFieldDef.StyleName, 'format');
          end;
      fkBoolean:
        begin
          vCol := TCheckColumn.Create(FGrid);
          TCheckColumn(vCol).HorzAlign := TTextAlign.Center;
        end;
      fkEnum, fkObject:
        begin
          vCol := TPopupColumn.Create(FGrid);
          if AFieldDef.Kind = fkEnum then
            FillEnumList(TDomain(FView.Domain), TPopupColumn(vCol).Items, AFieldDef);
        end;
      else
        vCol := TStringColumn.Create(FGrid);
    end;

    vCol.Header := AOverriddenCaption;
    vCol.ShowHint := true;
    vCol.TagString := AFieldName;
    vCol.Width := AWidth;
    vCol.ReadOnly := AFieldDef.UIState < vsSelectOnly;
    vCol.TagObject := AFieldDef;
    FGrid.AddObject(vCol);
  end;

//  ShowFooter(vCol, AAgg);
end;

{procedure TFMXGridEditor.CreateBtnPropertiesForAction(const AActionDef: TActionDef);
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
    vBtn.ImageIndex := GetImageIndex(AActionDef._ImageID);
    vBtn.Hint := AActionDef._Caption;
    vBtn.Enabled := AEnabled;
  end;
begin
  CreateProps(True, 'Enabled');
  CreateProps(False, 'Disabled');
end;   }

procedure TFMXGridEditor.CreateColumnForAction(const AActionDef: TActionDef; const AFieldName: string);
var
  vCol: TColumn;
begin
  vCol := TColumn.Create(FGrid);
  vCol.Name := AFieldName;
  vCol.Header := '';
//  vCol.OnGetProperties := OnActionGetProperties;
//  vCol.Options.Editing := True;
//  vCol.Options.ShowEditButtons := isebAlways;
//  vCol.DataBinding.Data := AActionDef;
//  CreateBtnPropertiesForAction(AActionDef);
end;

procedure TFMXGridEditor.SaveColumnWidths;
begin
  SaveGridColumnWidths(TInteractor(FView.Interactor).Domain, FGrid, FView.InitialName);
end;

procedure TFMXGridEditor.LoadColumnWidths;
begin
  FLayoutExists := LoadGridColumnWidths(TInteractor(FView.Interactor).Domain, FGrid, FView.InitialName);
end;

procedure TFMXGridEditor.DoExecuteUIAction(const AView: TView);
var
  vDateFrom, vDateTo: TDateTime;
  vIsPeriodActive: Boolean;
begin
  if AView.Name = '#FilterByText' then
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
        FFilterDateFrom := 0
      else
        FFilterDateFrom := vDateFrom;

      if vDateTo = cNullDateTime then
        FFilterDateTo := 0
      else
        FFilterDateTo := vDateTo;

      FFilterDateActive := vIsPeriodActive;

      UpdateArea(dckFilterChanged, nil);
    end;
  end;
end;

procedure TFMXGridEditor.DoOnColumnPosChanged(Column: TColumn; FromIndex, ToIndex: Integer);
begin
  SaveColumnWidths;
end;

procedure TFMXGridEditor.DoOnColumnSizeChanged(Sender: TObject);
begin
  if ParentInUpdate then Exit;
  // todo: порешать на какие правильные события подвязаться для сохранения информации по колонкам, сейчас слишком часто вызывается сохранение
//  SaveColumnWidths;
end;

procedure TFMXGridEditor.DoOnDrawCell(Sender: TObject; const Canvas: TCanvas;
  const Column: TColumn; const Bounds: TRectF; const Row: Integer;
  const Value: TValue; const State: TGridDrawStates);
var
  vFieldDef: TFieldDef;
  vFieldName: string;
  vField: TBaseField;
  vEntity: TEntity;
begin
  vFieldName := Column.TagString;
  vEntity := FAllData[Row];
  vField := vEntity.FieldByName(vFieldName);

  if Assigned(vField) then
    vFieldDef := vField.FieldDef
  else
    vFieldDef := TFieldDef(Column.TagObject);

  if vFieldDef.Kind = fkColor then
    Canvas.ClearRect(Bounds, ColorToAlphaColor(vEntity.ExtractFieldValue(Column.TagString)));
end;

procedure TFMXGridEditor.DoOnDrawCellBackground(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  vEntity: TEntity;
  vColor: TColor;
begin
  vEntity := FAllData[Row];
  vColor := GetRowColor(FOwner, vEntity, FAllData.MainDefinition.ColorFieldName);
  if vColor <> cNullColor then
    Canvas.ClearRect(Bounds, ColorToAlphaColor(vColor));
end;

procedure TFMXGridEditor.DoOnHeaderClick(Sender: TColumn);
begin
  SaveColumnWidths;
end;

procedure TFMXGridEditor.DoOnSelectionChanged(Sender: TObject);
var
  vEntityList: TEntityList;
begin
  if (FGrid.Selected = -1) then 
    Exit;
  vEntityList := TEntityList(TView(FView).DomainObject);
  vEntityList.SelectEntity(FAllData.Entity[FGrid.Selected]);
end;

procedure TFMXGridEditor.DoSetValue(Sender: TObject; const ACol, ARow: Integer;
  const Value: TValue);
var
  vColumn: TColumn;
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
    TUserSession(vInteractor.Session).AtomicModification(nil,
      function(const AHolder: TChangeHolder): Boolean
      begin
        vEntity._SetFieldValue(AHolder, vFieldName, vValue);
        Result := True;
      end, TChangeHolder(FOwner.Holder));
  end;

begin
  vEntity := nil;
    try
      vEntity := FAllData.Entity[ARow];
      if not Assigned(vEntity) then
        Exit;

      vColumn := FGrid.Columns[ACol];

      vFieldName := vColumn.TagString;
      vField := vEntity.FieldByName(vFieldName);

      if Assigned(vField) then
        vFieldDef := vField.FieldDef
      else
        vFieldDef := TFieldDef(vColumn.TagObject);

      if vFieldDef = nil then
        Exit;

      vInteractor := TInteractor(FOwner.View.Interactor);

      if vFieldDef.Kind = fkDateTime then
      begin
        { if (TDateTime(Result) < 2) and (vFieldDef.StyleName <> 'time') then
          Result := Null
          else if vFieldDef.StyleName = 'date' then // cut the time
          Result := Trunc(Result); }
      end
      else if vFieldDef.Kind = fkEnum then
      begin
        vEnumeration := TDomain(vEntity.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(vFieldDef).Dictionary);
        if Assigned(vEnumeration) then
        begin
          vItem := vEnumeration.GetItemByDisplayText(Value.AsString);
          if Assigned(vItem) then
          begin
            vValue := vItem.ID;
            DoChange;
          end;
        end;
      end
      else if vFieldDef.Kind in [fkInteger, fkColor] then
      begin
        vValue := StrToIntDef(Value.AsString, 0);
        DoChange;
      end
      else if vFieldDef.Kind in [fkFloat, fkCurrency] then
      begin
        vValue := StrToFloatDef(Value.AsString, 0);
        DoChange;
      end
      else if vFieldDef.Kind = fkString then
      begin
        vValue := Value.AsString;
        DoChange;
      end
      else if vFieldDef.Kind = fkBoolean then
      begin
        vValue := Value.AsBoolean;
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
            if SafeDisplayName(vEntities[i]) = Value.AsString then
            begin
              vEntityValue := vEntities[i];
              Break;
            end;
        finally
          FreeAndNil(vEntities);
        end;

        TUserSession(vInteractor.Session).AtomicModification(nil,
          function(const AHolder: TChangeHolder): Boolean
          begin
            vEntity._SetFieldEntity(AHolder, vFieldName, vEntityValue);
            Result := True;
          end, TChangeHolder(FOwner.Holder));
      end;

    except
      on E: Exception do
        if Assigned(vEntity) then
          TDomain(vEntity.Domain).Logger.AddMessage('Ошибка обновления данных в модели. Поле [' + vFieldName + ']');
    end;
end;

procedure TFMXGridEditor.DoOnCellDblClick(const Column: TColumn; const Row: Integer);
begin
  OnTableViewDblClick(FView, FOwner);
end;

procedure TFMXGridEditor.DoGetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
var
  vColumn: TColumn;
  vEntity: TEntity;
  vField: TBaseField;
  vFieldDef: TFieldDef;
  vEnumeration: TEnumeration;
begin
  vColumn := FGrid.Columns[ACol];
  vEntity := FAllData.Entity[ARow];
  try
    if not Assigned(vEntity) then
      Exit;
    vColumn := FGrid.Columns[ACol];
    vField := vEntity.FieldByName(vColumn.TagString);
    if Assigned(vField) then
      vFieldDef := vField.FieldDef
    else
      vFieldDef := TFieldDef(vColumn.TagObject);

    if vFieldDef = nil then
    begin
      Value := vEntity.DisplayName;
      Exit;
    end;

    Value := TValue.FromVariant(vEntity.ExtractFieldValue(vColumn.TagString));
    if Value.IsEmpty then
      Exit;

    if vFieldDef.Kind = fkColor then
      Value := IntToHex(Value.AsInteger)
    else if vFieldDef.Kind = fkDateTime then
    begin
      if (Value.AsExtended < 2) and (vFieldDef.StyleName <> 'time') then
        Value := TValue.FromVariant(Null)
      else if vFieldDef.StyleName = 'date' then // cut the time
        Value := Trunc(Value.AsExtended);
    end
    else if vFieldDef.Kind = fkEnum then
    begin
      vEnumeration := TDomain(vEntity.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(vFieldDef).Dictionary);
      if Assigned(vEnumeration) then
        Value := vEnumeration.Items[Integer(vEntity.FieldByName(vFieldDef.Name).Value)].DisplayText
      else
        Value := vEntity.GetStateCaption(vColumn.Name);
    end;
  except
    on E: Exception do
      if Assigned(vEntity) then
        TDomain(vEntity.Domain).Logger.AddMessage('Ошибка получения данных из модели. Поле [' + vColumn.Name + ']');
  end;
end;

procedure TFMXGridEditor.SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl);
var
  vPopupMenu: TPopupMenu;
begin
  if ATargetName = 'popup' then
  begin
    vPopupMenu := TPopupMenu(TFMXControl(ALinkedControl).Control);
    vPopupMenu.OnPopup := BeforeContextMenuShow;
    FGrid.PopupMenu := vPopupMenu;
  end;
end;

procedure TFMXGridEditor.UpdateArea(const AKind: Word; const AParameter: TEntity = nil);
begin
  if (AKind = dckEntityDeleted) or (AKind = dckEntityChanged) or (AKind = dckListAdded) or (AKind = dckListRemoved) or (AKind = dckFilterChanged)
     or (AKind = dckEntitySaved) or (AKind = dckListEndUpdate) then
  begin
    FGrid.RowCount := 0;
    FGrid.RowCount := FAllData.Count;
  end;
end;

{ TFMXEntityListSelectorMTM }

procedure TFMXEntityListSelectorMTM.DoBeforeFreeControl;
begin
  FreeAndNil(FEntityList);
  inherited;
end;

function TFMXEntityListSelectorMTM.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
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
  FListBox := TListBox.Create(nil);
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
    vTransitContentType := { FTransitFieldDef.ContentTypeLocator; // } GetUrlParam(vListFieldDef.StyleName, 'contentType');
    if (FView.ParentDomainObject is TEntity) and (Length(vTransitContentType) > 0) then
    begin
      vParentEntity := TEntity(FView.ParentDomainObject);
      try
        vTransitDefinitionName := vParentEntity[vTransitContentType];
        FTransitDefinition := TDomain(FDomain).Configuration.DefinitionByName[vTransitDefinitionName];
      except
        Assert(False, 'Неверно указан тип для транзитного поля: ' + vTransitDefinitionName);
      end;
    end;
  end;

  FListBox.ShowCheckboxes := True;
  FListBox.OnChangeCheck := DoOnChangeCheck;

  vInteractor := TInteractor(FView.Interactor);

  FEntityList := TEntityList.Create(vInteractor.Domain, vInteractor.Session);
end;

procedure TFMXEntityListSelectorMTM.FillList;
var
  i: Integer;
  vItem: TListBoxItem;
  vSelectedList: TEntityList;
  vList: TList<TEntity>;
  vSelectedIndex: Integer;
  vEntity, vParentParameter: TEntity;
begin
  TDomain(FDomain).GetEntityList(FView.Session, FTransitDefinition, FEntityList, '');

  vSelectedList := TEntityList(FView.DomainObject);
  vList := TList<TEntity>.Create;
  try
    for i := 0 to vSelectedList.Count - 1 do
      vList.Add(vSelectedList[i].ExtractEntity(FTransitFieldDef.Name));

    while FListBox.Items.Count > 0 do
      FListBox.Items.Delete(0);

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

      vItem := TListBoxItem.Create(FListBox);

      if not Assigned(FCreateParams) or (FCreateParams.Values['DisplayName'] = '') then
        vItem.Text := vEntity.DisplayName
      else
        vItem.Text := vEntity[FCreateParams.Values['DisplayName']];

      vSelectedIndex := vList.IndexOf(vEntity);
      vItem.IsChecked := vSelectedIndex >= 0;
      if vItem.IsChecked then
        vItem.TagObject := vSelectedList[vSelectedIndex]
      else
        vItem.TagObject := FEntityList[i];

      FListBox.AddObject(vItem);
    end;

  finally
    FreeAndNil(vList);
  end;
end;

procedure TFMXEntityListSelectorMTM.DoOnChangeCheck(Sender: TObject);
var
  vEntity: TEntity;
  vItem: TListBoxItem absolute Sender;
begin

  vEntity := TEntity(vItem.TagObject);

  if vItem.IsChecked then
  begin
    Assert(vEntity.Definition.IsDescendantOf(FEntityList.MainDefinition.Name), 'К записи привязан объект неправильного типа');
    TUserSession(FView.Session).AtomicModification(nil,
      function(const AHolder: TChangeHolder): Boolean
      var
        vTransitEntity: TEntity;
      begin
        vTransitEntity := TEntityList(FView.DomainObject).AddEntity(AHolder, '', FTransitFieldDef.Name, [NativeInt(vEntity)]);
        // vTransitEntity._SetFieldEntity(AHolder, FTransitField.Name, vEntity);
        vItem.TagObject := vTransitEntity;
        Result := True;
      end, TChangeHolder(FOwner.Holder));
  end
  else
  begin
    Assert(vEntity.Definition.IsDescendantOf(TEntityList(FView.DomainObject).MainDefinition.Name), 'К записи привязан объект неправильного типа');
    vItem.TagObject := vEntity.ExtractEntity(FTransitFieldDef.Name);
    TUserSession(FView.Session).AtomicModification(nil,
      function(const AHolder: TChangeHolder): Boolean
      begin
        TEntityList(FView.DomainObject).RemoveEntity(AHolder, vEntity);
        Result := True;
      end, TChangeHolder(FOwner.Holder));
  end;
end;

procedure TFMXEntityListSelectorMTM.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(AHandler) then
    FListBox.OnChangeCheck := DoOnChangeCheck
  else
    FListBox.OnChangeCheck := nil;
end;

procedure TFMXEntityListSelectorMTM.UpdateArea(const AKind: Word; const AParameter: TEntity);
begin
  FillList;
end;

{ TFMXEntityListSelector }

function TFMXEntityListSelector.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FListBox := TListBox.Create(nil);
  FListBox.ShowCheckboxes := True;
  FListBox.OnChangeCheck := DoOnChangeCheck;
  Result := FListBox;
  FEntityList := nil;
end;

procedure TFMXEntityListSelector.DoOnChange;
var
  i: Integer;
  vList: TListField;
  vHolder: TObject;
  vItem: TListBoxItem;
begin
  vList := FView.ExtractListField;
  if not Assigned(vList) then
    Exit;

  vHolder := FOwner.Holder;
  for i := 0 to FListBox.Count - 1 do
  begin
    vItem := TListBoxItem(FListBox.ItemByIndex(i));
    if vItem.IsChecked then
      vList.LinkListEntity(vHolder, TEntity(vItem.TagObject))
    else
      vList.UnlinkListEntity(vHolder, TEntity(vItem.TagObject));
  end;
end;

procedure TFMXEntityListSelector.FillEditor;
var
  i: Integer;
  vItem: TListBoxItem;
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

  while FListBox.Items.Count > 0 do
    FListBox.Items.Delete(0);

  for i := 0 to FEntityList.Count - 1 do
  begin
    vItem := TListBoxItem.Create(nil);
    vItem.Text := SafeDisplayName(FEntityList[i]);
    vItem.TagObject := FEntityList[i];
    vItem.IsChecked := vList.Contains(FEntityList[i]);
    FListBox.AddObject(vItem);
  end;
end;

procedure TFMXEntityListSelector.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(AHandler) then
    FListBox.OnChangeCheck := DoOnChangeCheck
  else
    FListBox.OnChangeCheck := nil;
end;

procedure TFMXEntityListSelector.DoOnChangeCheck(Sender: TObject);
begin
  DoOnChange;
end;

{ TFMXEntityListSelector3 }

procedure TFMXEntityListSelector3.FillLookupList;
var
  vCollection: TCollection;
  vLookupItem: TEntity;
  vItem: TListBoxItem;

  function IsChanged: Boolean;
  var
    i: Integer;
  begin
    if vCollection.Count <> FListBox.ChildrenCount then
      Exit(True);

    for i := 0 to FListBox.Count - 1 do
      if FListBox.ItemByIndex(i).TagObject <> vCollection[i] then
        Exit(True);

    Result := False;
  end;

begin
  inherited;

  vCollection := TDomain(FDomain)[FLookupCollection];

  if not IsChanged then
    Exit;

  while FListBox.Items.Count > 0 do
    FListBox.Items.Delete(0);

  for vLookupItem in vCollection do
  begin
    vItem := TListBoxItem.Create(FListBox);
    vItem.Text := SafeDisplayName(vLookupItem);
    vItem.TagObject := vLookupItem;
    FListBox.AddObject(vItem);
  end;
end;

function TFMXEntityListSelector3.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  FListBox := TListBox.Create(nil);
  Result := FListBox;
  FMasterList := TEntityList(FView.DomainObject);
  FLookupCollection := TDefinition(FView.Definition).Name;
  Assert(Assigned(FCreateParams) and (FCreateParams.IndexOfName('lookupfield') > -1), 'не задан параметер lookupfield');
  FLookupField := FCreateParams.Values['lookupfield'];
  FDestroing := False;
  FListBox.ShowCheckboxes := True;
  FListBox.OnChangeCheck := DoOnChangeCheck;
end;

procedure TFMXEntityListSelector3.DoDisableContent;
begin
  inherited;
  FDestroing := True;
end;

procedure TFMXEntityListSelector3.FillEditor;
var
  i: Integer;
  vItem: TListBoxItem;
  vLookupItem: TEntity;
begin
  if FDestroing then
    Exit;

  FillLookupList;

  for i := 0 to FListBox.Count - 1 do
  begin
    vItem := FListBox.ItemByIndex((i));
    vLookupItem := TEntity(vItem.TagObject);
    vItem.IsChecked := FindMasterItem(vLookupItem) <> nil;
  end;
end;

function TFMXEntityListSelector3.FindMasterItem(const ALookupItem: TEntity): TEntity;
var
  vMasterList: TEntityList;
begin
  vMasterList := TEntityList(FView.DomainObject);
  for Result in vMasterList do
    if Result.ExtractEntity(FLookupField) = ALookupItem then
      Exit;
  Result := nil;
end;

procedure TFMXEntityListSelector3.DoOnChangeCheck(Sender: TObject);
var
  vHolder: TObject;
  vMasterItem, vLookupItem: TEntity;
begin
  vHolder := FOwner.Holder;

  vLookupItem := TEntity(TListBoxItem(Sender).TagObject);
  vMasterItem := FindMasterItem(vLookupItem);

  if TListBoxItem(Sender).IsChecked then
  begin
    if not Assigned(vMasterItem) then
    begin
      vMasterItem := FMasterList.AddEntity(vHolder, FMasterList.MainDefinition.Name, '', []);
      vMasterItem._SetFieldEntity(vHolder, FLookupField, vLookupItem);
    end;
  end
  else if Assigned(vMasterItem) then
    FMasterList.RemoveEntity(vHolder, vMasterItem);
end;

procedure TFMXEntityListSelector3.SwitchChangeHandlers(const AHandler: TNotifyEvent);
begin
  inherited;
  if Assigned(AHandler) then
    FListBox.OnChangeCheck := DoOnChangeCheck
  else
    FListBox.OnChangeCheck := nil;
end;

initialization

TPresenter.RegisterControlClass('FMX', uiListEdit, '', TFMXGridEditor);
TPresenter.RegisterControlClass('FMX', uiListEdit, 'multiselect', TFMXEntityListSelector);
TPresenter.RegisterControlClass('FMX', uiListEdit, 'multiselect3', TFMXEntityListSelector3);
TPresenter.RegisterControlClass('FMX', uiListEdit, 'mtm', TFMXEntityListSelectorMTM);

TPresenter.RegisterControlClass('FMX', uiCollection, '', TFMXGridEditor);

end.

