{---------------------------------------------------------------------------------
  X-Tend runtime

  Contributors:
    Vladimir Kustikov (kustikov@sensoft.pro)
    Sergey Arlamenkov (arlamenkov@sensoft.pro)

  You may retrieve the latest version of this file at the GitHub,
  located at https://github.com/sensoftpro/x-tend.git
 ---------------------------------------------------------------------------------
  MIT License

  Copyright © 2022 Sensoft

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

unit uVST;

interface

uses
  uEntity, uEntityList, vclArea, uView, uUIBuilder, uDefinition, uConsts, uLayout,

  Classes, VirtualTrees, Graphics, Types;

type
  TVSTListEditor = class (TFieldArea)
  private
    FVST: TVirtualStringTree;
    FAllData: TEntityList;
    FColorFieldName: string;
    procedure CreateColumnsFromModel(const AFields: string = '');
    procedure CreateColumn(const AFieldDef: TFieldDef; const AFieldName: string; const AOverriddenCaption: string;
      const AWidth: Integer; const ASortOrder: TSortOrder; const AAgg: TAggregationKind);
    procedure ClearColumns;
    function LoadColumnWidths: Boolean;
    procedure SaveColumnWidths;
    procedure SetGridColumnParams(const AColName: string; const AWidth, AColIndex, ARowIndex: Integer; const AVisible: Boolean; const ASortOrder: TSortOrder);
    procedure OnGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure OnInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure OnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: UnicodeString);
    procedure OnDblClick(Sender: TObject);
    procedure OnBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    function GetRowColor(const AEntity: TEntity): TColor;
    procedure OnColumnResize(Sender: TVTHeader; Column: TColumnIndex);
    procedure OnAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure OnRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure SetPopupArea(const APopupArea: TUIArea); override;
    procedure UpdateArea(const AKind: Word; const AParameter: TEntity = nil); override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  uPresenter, uInteractor, uWinVCLPresenter, uUtils, uSettings, uConfiguration, uDomain, uEnumeration,

  SysUtils, Variants;

type
  TColumnInfo = class
    FieldDef: TFieldDef;
  end;

  TCalculateStyleFunc = function(const AViewName: string; const AEntity: TEntity): TColor of object;

const
  cColWidthDelim = ',';


{ TVSTListEditor }

procedure TVSTListEditor.SetGridColumnParams(const AColName: string; const AWidth,
  AColIndex, ARowIndex: Integer; const AVisible: Boolean; const ASortOrder: TSortOrder);
var
  vCol: TVirtualTreeColumn;
  function GetColumnByName: TVirtualTreeColumn;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to FVST.Header.Columns.Count - 1 do
      if TColumnInfo(FVST.Header.Columns[i].Tag).FieldDef.Name = AColName then
      begin
        Result := FVST.Header.Columns[i];
        Break;
      end;
  end;
begin
  vCol := GetColumnByName;
  if vCol = nil then Exit;

  vCol.Width := AWidth;
  vCol.Index := AColIndex;

  if AVisible then
    vCol.Options := vCol.Options + [coVisible]
  else
    vCol.Options := vCol.Options - [coVisible];

  if ASortOrder = soAscending then
    vCol.DefaultSortDirection := sdAscending
  else
    vCol.DefaultSortDirection := sdDescending;
end;

function TVSTListEditor.LoadColumnWidths: Boolean;
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
  vDomain: TDomain;
begin
  vDomain := TDomain(TInteractor(FView.Interactor).Domain);
  vSettings := vDomain.UserSettings;
  vSectionName := 'Columns$' + FFieldDef.FullName;

  Result := vSettings.SectionExists(vSectionName);

  if not Result then
  begin
    // Совместимость со старым кодом
    vConfiguration := vDomain.Configuration;
    vPath := vConfiguration.FindLayoutFile(FFieldDef.FullName, '.col');
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
  FVST.BeginUpdate;
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

      SetGridColumnParams(vCols.Names[i], vWidth, vColIndex, vRowIndex, vVisible, vSortOrder);
    end;
  finally
    FVST.EndUpdate;
    FreeAndNil(vCols);
    FreeAndNil(vValues)
  end;
end;

procedure TVSTListEditor.OnBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect; var ItemColor: TColor;
  var EraseAction: TItemEraseAction);
var
  vEntity: TEntity;
begin
  vEntity := FAllData[Node.Index];
  ItemColor := GetRowColor(vEntity);
end;

procedure TVSTListEditor.OnColumnResize(Sender: TVTHeader; Column: TColumnIndex);
begin
  SaveColumnWidths
end;

procedure TVSTListEditor.OnAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  FAllData.SelectEntity(FAllData[Node.Index]);
end;

procedure TVSTListEditor.OnRemoveFromSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin

end;

procedure TVSTListEditor.OnDblClick(Sender: TObject);
var
  vDblClickView: TView;
begin
  vDblClickView := FView.BuildView('Selected/#HandleDblClick');
  try
    ExecuteUIAction(vDblClickView);
  finally
    vDblClickView.CleanView;
  end;
end;

procedure TVSTListEditor.OnGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
 // NodeDataSize :=
end;

procedure TVSTListEditor.OnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: UnicodeString);
var
  vFieldDef: TFieldDef;
  vFieldName: string;
  vEntity: TEntity;
  vEnumeration: TEnumeration;
  vColumn: TVirtualTreeColumn;
  vValue: Variant;
  vFormat: string;
begin
  vEntity := FAllData[Node.Index];
  try
    if not Assigned(vEntity) then
      Exit;

    vColumn := FVST.Header.Columns[Column];

    vFieldDef := TColumnInfo(vColumn.Tag).FieldDef;

    if not Assigned(vFieldDef) then
    begin
      CellText := vEntity.DisplayName;
      Exit;
    end;

    vFieldName := TColumnInfo(vColumn.Tag).FieldDef.Name;
    vFormat := GetDisplayFormat(vFieldDef, vEntity);

    vValue := vEntity.ExtractFieldValue(vFieldName);
    if VarIsNull(vValue) then
      Exit;

    if vFieldDef.Kind = fkDateTime then
    begin
      if (TDateTime(vValue) < 2) and (vFieldDef.StyleName <> 'time') then
        CellText := ''
      else if vFieldDef.StyleName = 'date' then
        CellText := FormatDate(vValue)
      else
        CellText := FormatDateTime(vFormat, vValue);
    end
    else if vFieldDef.Kind = fkEnum then
    begin
      vEnumeration := TDomain(vEntity.Domain).Configuration.Enumerations.ObjectByName(TSimpleFieldDef(vFieldDef).Dictionary);
      if Assigned(vEnumeration) then
        CellText := vEnumeration.Items[Integer(vEntity.FieldByName(vFieldDef.Name).Value)].DisplayText
      else
        CellText := vEntity.GetStateCaption(vFieldName);
    end
    else if vFieldDef.Kind in [fkInteger, fkFloat, fkCurrency] then
    begin
      if vFieldDef.Kind in [fkFloat, fkCurrency] then
        CellText := FormatFloat(vFormat, vValue)
      else
        CellText := IntToStr(vValue);
    end
    else
      CellText := vValue;
  except
    on E: Exception do
    begin
      if Assigned(vEntity) then
        TDomain(vEntity.Domain).Logger.AddMessage('Ошибка получения данных из модели. Поле [' + vFieldName + ']');
      CellText := E.Message;
    end;
  end;
end;

procedure TVSTListEditor.OnInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin

end;

procedure TVSTListEditor.SaveColumnWidths;
var
  vSectionName: string;
  vSettings: TSettings;
  vColumn: TVirtualTreeColumn;
  vValues: string;
  i: Integer;
begin
  vSettings := TDomain(TInteractor(FView.Interactor).Domain).UserSettings;
  vSectionName := 'Columns$' + FFieldDef.FullName;
  for i := 0 to FVST.Header.Columns.Count - 1 do
  begin
    vColumn := FVST.Header.Columns[i];
    vValues := IntToStr(vColumn.Width) + cColWidthDelim +
      IntToStr(vColumn.Index) + cColWidthDelim +
      IntToStr(0) + cColWidthDelim +
      BoolToStr(coVisible in vColumn.Options) + cColWidthDelim +
      IntToStr(Integer(vColumn.DefaultSortDirection));
    vSettings.SetValue(vSectionName, TColumnInfo(vColumn.Tag).FieldDef.Name, vValues);
  end;
end;

procedure TVSTListEditor.ClearColumns;
var
  i: Integer;
begin
  for i := 0 to FVST.Header.Columns.Count - 1 do
    TColumnInfo(FVST.Header.Columns[i].Tag).Free;
  FVST.Header.Columns.Clear;
end;

procedure TVSTListEditor.CreateColumn(const AFieldDef: TFieldDef; const AFieldName, AOverriddenCaption: string; const AWidth: Integer;
  const ASortOrder: TSortOrder; const AAgg: TAggregationKind);
var
  vColumn: TVirtualTreeColumn;
  vColumnInfo: TColumnInfo;
begin
  vColumn := FVST.Header.Columns.Add;
  vColumn.Text := AOverriddenCaption;
  vColumn.Width := AWidth;
  vColumnInfo := TColumnInfo.Create;
  vColumnInfo.FieldDef := AFieldDef;
  vColumn.Tag := Integer(vColumnInfo);
end;

procedure TVSTListEditor.CreateColumnsFromModel(const AFields: string);
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
  FVST.BeginUpdate;
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
//              CreateColumnForAction(vActionDef, vFieldName);
          end;
        end;
      finally
        vFields.Free;
      end;
    end;
  finally
    FVST.EndUpdate;
  end;
end;

destructor TVSTListEditor.Destroy;
begin
  ClearColumns;
  inherited;
end;

procedure TVSTListEditor.DoBeforeFreeControl;
begin
  inherited;

end;

function TVSTListEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vFields: string;
  vDefinition: TDefinition;
begin
  inherited;

  FVST := TVirtualStringTree.Create(nil);
  FVST.TreeOptions.SelectionOptions := FVST.TreeOptions.SelectionOptions + [toFullRowSelect, toSimpleDrawSelection];
  FVST.TreeOptions.PaintOptions := FVST.TreeOptions.PaintOptions + [toShowHorzGridLines{, toUseBlendedSelection}] - [toShowRoot];
  FVST.Header.Options := FVST.Header.Options + [hoVisible];
  FVST.OnGetText := OnGetText;
  FVST.OnDblClick := OnDblClick;
  FVST.OnBeforeItemErase := OnBeforeItemErase;
  FVST.OnGetNodeDataSize := OnGetNodeDataSize;
  FVST.OnInitNode := OnInitNode;
  FVST.OnColumnResize := OnColumnResize;
  FVST.OnAddToSelection := OnAddToSelection;
  FVST.OnRemoveFromSelection := OnRemoveFromSelection;
  FVST.HintMode := hmTooltip;
  FVST.Colors.UnfocusedSelectionBorderColor := FVST.Colors.FocusedSelectionColor;

  Result := FVST;

  FAllData := TEntityList(FView.DomainObject);
  vDefinition := FAllData.MainDefinition;

  FColorFieldName := Trim(AParent.QueryParameter('Color'));
  if (FColorFieldName = '') and Assigned(vDefinition) then
    FColorFieldName := Trim(vDefinition.ColorFieldName);
  if FColorFieldName = '-' then
    FColorFieldName := '';

  if Assigned(FCreateParams) then
  begin
    vFields := FCreateParams.Values['fields'];
    if FCreateParams.Values['editingInGrid'] = 'true' then
    begin
//      FMasterTableView.OptionsData.Editing := True;
//      FMasterTableView.OptionsSelection.CellSelect := True;
    end;
  end
  else
    vFields := '';

  if vFields = '' then
    vFields := AParent.QueryParameter('fields');

  CreateColumnsFromModel(vFields);
  LoadColumnWidths;
end;

function TVSTListEditor.GetRowColor(const AEntity: TEntity): TColor;
var
  vDefinition: TDefinition;
  vColorField: TBaseField;
  vState: TState;
begin
  Result := clDefault;

  if not Assigned(AEntity) then Exit;

  vDefinition := FAllData.MainDefinition;
  if LowerCase(FColorFieldName) = 'script' then
  begin
    Result := TCalculateStyleFunc(TConfiguration(vDefinition.Configuration).CalculateStyleFunc)(FView.InitialName, AEntity);
  end
  else if vDefinition.ColorTarget <> uConsts.ctNone then
  begin
    Result := cNullColor;
    vColorField := AEntity.FieldByName(FColorFieldName);
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

  if Result = cNullColor then
    Result := clDefault;
end;

procedure TVSTListEditor.SetPopupArea(const APopupArea: TUIArea);
begin
  inherited;

end;

procedure TVSTListEditor.UpdateArea(const AKind: Word; const AParameter: TEntity);
var
  vNewNode: PVirtualNode;

  function FindNodeByEntity(const AEntity: TEntity): PVirtualNode;
  var
    vNode: PVirtualNode;
  begin
    Result := nil;
    vNode := FVST.GetFirstVisible;
    while Assigned(vNode) do
    begin
      if FAllData[vNode.Index] = AEntity then
      begin
        Result := vNode;
        Break;
      end;
      vNode := FVST.GetNextVisible(vNode);
    end;
  end;
begin
  if AKind = dckEntityChanged then
  begin
    FVST.Invalidate;
  end
  else if AKind = dckListAdded then
  begin
    FVST.Invalidate;
  end
  else if AKind = dckListRemoved then
  begin
    FVST.Invalidate;
  end
  else if (AKind = dckSelectionChanged) or (AKind = dckListScrollUpdate)  then
  begin
    if AKind = dckListScrollUpdate then
      FVST.ClearSelection;

    vNewNode := FindNodeByEntity(AParameter);
    if Assigned(vNewNode) then
    begin
      FVST.Selected[vNewNode] := True;
      FVST.ScrollIntoView(vNewNode, False);
    end;
  end
  else if AKind = dckViewStateChanged then
    // Сделать что-то с видимостью
  else
  begin
    if FVST.RootNodeCount <> Cardinal(FAllData.Count) then
    begin
      FVST.RootNodeCount := FAllData.Count;
      FAllData.Resort;
    end
    else
      FVST.Invalidate;

    if (FAllData.Count > 0) and (FAllData.Selection.Count = 0) then
      FAllData.SelectEntity(FAllData[0]);
  end;
end;

initialization

TPresenter.RegisterUIClass('Windows.VCL', uiListEdit, 'vst', TVSTListEditor);

end.
