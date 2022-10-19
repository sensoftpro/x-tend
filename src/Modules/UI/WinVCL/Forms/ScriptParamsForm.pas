unit ScriptParamsForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, OkCancelFrame, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, Vcl.Grids, cxSpinEdit,
  cxStyles, cxInplaceContainer, cxVGrid, cxCalendar, cxCheckBox, cxDataControllerConditionalFormattingRulesManagerDialog, cxMemo,
  uScriptExecutor, uInteractor;

type
  TScriptParamsFm = class(TForm)
    OkCancelFrm1: TOkCancelFrm;
    vgParams: TcxVerticalGrid;
    procedure FormCreate(Sender: TObject);
  private
    procedure AddVariable(const AVarName, AName, AValue: string; const AType: TParameterType);
    function GetVariableValue(const AIndex: Integer): Variant;
  public
    class function Edit(const AInteractor: TInteractor; const AParameters: TObject): TModalResult;
  end;

implementation

{$R *.dfm}

{ TScriptParamsFm }

procedure TScriptParamsFm.AddVariable(const AVarName, AName, AValue: string; const AType: TParameterType);
var
  vRow: TcxEditorRow;
  vGridHeight, i: Integer;
begin
  vRow := TcxEditorRow(vgParams.Add(TcxEditorRow));
  vRow.Properties.Caption := AName;

  if AType = ptString then
    vRow.Properties.EditPropertiesClassName := 'TcxTextEditProperties'
  else if AType = ptText then
  begin
    vRow.Height := 56;
    vRow.Properties.EditPropertiesClassName := 'TcxMemoProperties';
    TcxMemoProperties(vRow.Properties.EditProperties).ScrollBars := ssVertical;
  end
  else if AType = ptInteger then
  begin
    if AVarName = 'OutputUnits' then
    begin
      vRow.Properties.EditPropertiesClassName := 'TcxComboBoxProperties';
      TcxComboBoxProperties(vRow.Properties.EditProperties).DropDownListStyle := lsFixedList;
      TcxComboBoxProperties(vRow.Properties.EditProperties).Items.Add('Бк');
      TcxComboBoxProperties(vRow.Properties.EditProperties).Items.Add('ppm');
      vRow.Properties.Value := TcxComboBoxProperties(vRow.Properties.EditProperties).Items[StrToIntDef(AValue, 0)];
    end
    else
      vRow.Properties.EditPropertiesClassName := 'TcxSpinEditProperties';
  end
  else if AType = ptFloat then
  begin
    vRow.Properties.EditPropertiesClassName := 'TcxSpinEditProperties';
    TcxSpinEditProperties(vRow.Properties.EditProperties).ValueType := cxSpinEdit.vtFloat;
  end
  else if AType = ptDateTime then
    vRow.Properties.EditPropertiesClassName := 'TcxDateEditProperties'
  else if AType = ptBoolean then
    vRow.Properties.EditPropertiesClassName := 'TcxCheckBoxProperties'
  else
    Assert(False, 'unsupported type ' + IntToStr(Ord(AType)) + ' for variable: ' + AName);

  if AVarName <> 'OutputUnits' then
    vRow.Properties.Value := AValue;

  vGridHeight := 0;
  for i := 0 to vgParams.Rows.Count - 1 do
    vGridHeight := vGridHeight + vgParams.ViewInfo.CalcRowHeight(vgParams.Rows[i]) + vgParams.ViewInfo.DividerWidth;

  ClientHeight := vgParams.Margins.Top + vgParams.Margins.Bottom + vGridHeight + OkCancelFrm1.Height + 4;
end;

class function TScriptParamsFm.Edit(const AInteractor: TInteractor; const AParameters: TObject): TModalResult;
var
  vForm: TScriptParamsFm;
  i: Integer;
  vParameter: TScriptVariable;
begin
  vForm := TScriptParamsFm.Create(nil);
  try
    for i := 0 to TList(AParameters).Count - 1 do
    begin
      vParameter := TScriptVariable(TList(AParameters)[i]);
      vForm.AddVariable(vParameter.Name, vParameter.Caption, vParameter.TextValue, vParameter.ValueType);
    end;

    Result := vForm.ShowModal;
    if Result <> mrOk then
      Exit;

    for i := 0 to TList(AParameters).Count - 1 do
    begin
      vParameter := TScriptVariable(TList(AParameters)[i]);
      vParameter.SetStringValue(VarToStr(vForm.GetVariableValue(i)));
    end;
  finally
    FreeAndNil(vForm);
  end;
end;

procedure TScriptParamsFm.FormCreate(Sender: TObject);
begin
  vgParams.ClearRows;
end;

function TScriptParamsFm.GetVariableValue(const AIndex: Integer): Variant;
var
  vCBProperty: TcxComboBoxProperties;
begin
  if vgParams.Rows.Count > AIndex then
  begin
    if TcxEditorRow(vgParams.Rows[AIndex]).Properties.EditProperties is TcxComboBoxProperties then
    begin
      vCBProperty := TcxComboBoxProperties(TcxEditorRow(vgParams.Rows[AIndex]).Properties.EditProperties);
      Result := IntToStr(vCBProperty.Items.IndexOf(TcxEditorRow(vgParams.Rows[AIndex]).Properties.Value));
    end
    else
      Result := TcxEditorRow(vgParams.Rows[AIndex]).Properties.Value;
  end
  else
    Result := Null;
end;

end.
