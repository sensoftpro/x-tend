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

unit ReportConfigureVclForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, ActnList, uDefinition, uInteractor, System.Actions;

type
  TReportParamList = class;

  TReportParameter = class
  private
    FOwner: TReportParamList;
    FInitialParamName: string;
    FParamName: string;
    FLength: Integer;
    FPosition: Integer;
    function GetChanged: Boolean;
  public
    constructor Create(const AOwner: TReportParamList;
      const AParamName: string; const APosition: Integer);

    procedure Cancel;
    procedure Save;

    property ParamName: string read FParamName write FParamName;
    property Position: Integer read FPosition write FPosition;
    property ParamLength: Integer read FLength;
    property Changed: Boolean read GetChanged;
  end;

  TReportParamList = class
  private
    FList: TList;
    FReportContent: string;
    function GetCount: Integer;
    function GetReportParameter(const AIndex: Integer): TReportParameter;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddReportParameter(const AParamName: string;
      const APosition: Integer);
    procedure Clear;
    procedure ExtractParameters(const AReportContent: string);
    procedure RecalcPosition(const APosition, AShift: Integer);

    property Count: Integer read GetCount;
    property ReportParameters[const AIndex: Integer]: TReportParameter
      read GetReportParameter; default;
  end;

type
  TReportConfigureVclFm = class(TForm)
    btnLoadReport: TButton;
    lbxReportParameters: TListBox;
    redtTemplate: TRichEdit;
    cbxTemplates: TComboBox;
    btnSaveReport: TButton;
    gbxReportParamSettings: TGroupBox;
    lblParameter: TLabel;
    mmParameter: TMemo;
    lblPosition: TLabel;
    edtPosition: TEdit;
    lblLength: TLabel;
    edtLength: TEdit;
    btnApply: TButton;
    btnCancel: TButton;
    alFormActions: TActionList;
    actApply: TAction;
    actCancel: TAction;
    procedure FormCreate(Sender: TObject);
    procedure btnLoadReportClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbxReportParametersClick(Sender: TObject);
    procedure mmParameterChange(Sender: TObject);
    procedure alFormActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actCancelExecute(Sender: TObject);
    procedure actApplyExecute(Sender: TObject);
    procedure btnSaveReportClick(Sender: TObject);
  private
    FInteractor: TInteractor;
    FCurrentReport: TRTFReport;
    FReportParameters: TReportParamList;
    FCurrentParam: TReportParameter;

    procedure PopulateReportsTemplates;
    procedure PopulateReportParameters;
    procedure PopulateParameterWidget;

    function BuildContent(const AContent: string; const AReportParam:
      TReportParameter; const AHighlight: Boolean = False): string;
    procedure ShowRTFDocument;
  public
    procedure Init(const AInteractor: TInteractor);
  end;

implementation

uses
  Generics.Collections, uConfiguration, uConsts;

{$R *.dfm}

procedure TReportConfigureVclFm.actApplyExecute(Sender: TObject);
begin
  if Assigned(FCurrentParam) then
  begin
    FCurrentReport.Content.Position := 0;
    FCurrentReport.Content.WriteString(
      BuildContent(FCurrentReport.Content.DataString, FCurrentParam));

    FCurrentParam.Save;

    ShowRTFDocument;
  end;
end;

procedure TReportConfigureVclFm.actCancelExecute(Sender: TObject);
begin
  if Assigned(FCurrentParam) then
  begin
    FCurrentParam.Cancel;
    PopulateParameterWidget;
  end;
end;

procedure TReportConfigureVclFm.alFormActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actApply.Enabled := Assigned(FCurrentParam) and FCurrentParam.Changed;
  actCancel.Enabled := actApply.Enabled;
end;

procedure TReportConfigureVclFm.btnLoadReportClick(Sender: TObject);
begin
  if cbxTemplates.ItemIndex < 0 then
    Exit;

  FCurrentParam := nil;  
  FCurrentReport := TRTFReport(
    cbxTemplates.Items.Objects[cbxTemplates.ItemIndex]);

  FReportParameters.ExtractParameters(FCurrentReport.Content.DataString);
  PopulateReportParameters;

  ShowRTFDocument;
end;

procedure TReportConfigureVclFm.btnSaveReportClick(Sender: TObject);
begin
  if Assigned(FCurrentReport) then
    FCurrentReport.Save;
end;

function TReportConfigureVclFm.BuildContent(const AContent: string;
  const AReportParam: TReportParameter; const AHighlight: Boolean): string;
var
  vContent: string;
begin
  if not Assigned(AReportParam) then
  begin
    Result := AContent;
    Exit;
  end;
  
  vContent := AContent;
  Result := Copy(vContent, 1, AReportParam.Position - 1);
  Delete(vContent, 1, AReportParam.Position + AReportParam.ParamLength - 1);
  if AHighlight then
    Result := Result + '\b ' + AReportParam.ParamName + '\b0 ' + vContent
  else
    Result := Result + AReportParam.ParamName + vContent;
end;

procedure TReportConfigureVclFm.FormCreate(Sender: TObject);
begin
  FReportParameters := TReportParamList.Create;
  FCurrentReport := nil;
  FCurrentParam := nil;
end;

procedure TReportConfigureVclFm.FormDestroy(Sender: TObject);
begin
  FReportParameters.Free;
end;

procedure TReportConfigureVclFm.Init(const AInteractor: TInteractor);
begin
  FInteractor := AInteractor;
  Caption := FInteractor.Translate('@' + Self.ClassName + '@Caption', 'Настройка шаблонов отчетов');
  gbxReportParamSettings.Caption := FInteractor.Translate('@' + Self.ClassName + '.gbxReportParamSettings@Caption',
    ' ' + 'Настройки параметра отчета' + ' ');
  lblParameter.Caption := FInteractor.Translate('@' + Self.ClassName + '.lblParameter@Caption', 'Наименование') + ':';
  lblPosition.Caption := FInteractor.Translate('@' + Self.ClassName + '.lblPosition@Caption', 'Позиция в документе') + ':';
  lblLength.Caption := FInteractor.Translate('@' + Self.ClassName + '.lblLength@Caption', 'Длина') + ':';
  actApply.Caption := FInteractor.Translate('@' + Self.ClassName + '.actApply@Caption', 'Применить');
  actCancel.Caption := FInteractor.Translate('@' + Self.ClassName + '.actCancel@Caption', 'Отмена');

  PopulateReportsTemplates;
end;

procedure TReportConfigureVclFm.lbxReportParametersClick(Sender: TObject);
var
  vIndex: Integer;
begin
  vIndex := lbxReportParameters.ItemIndex;
  if vIndex < 0 then
    FCurrentParam := nil
  else
    FCurrentParam := FReportParameters[vIndex];

  PopulateParameterWidget;

  ShowRTFDocument;
end;

procedure TReportConfigureVclFm.mmParameterChange(Sender: TObject);
begin
  if Assigned(FCurrentParam) then
    FCurrentParam.ParamName := mmParameter.Lines.Text;
end;

procedure TReportConfigureVclFm.PopulateParameterWidget;
begin
  if FCurrentParam = nil then
    Exit;

  mmParameter.Lines.Text := FCurrentParam.ParamName;
  edtPosition.Text := IntToStr(FCurrentParam.Position);
  edtLength.Text := IntToStr(FCurrentParam.ParamLength);
end;

procedure TReportConfigureVclFm.PopulateReportParameters;
var
  i: Integer;
begin
  lbxReportParameters.Items.BeginUpdate;
  try
    lbxReportParameters.Clear;
    for i := 0 to FReportParameters.Count - 1 do
      lbxReportParameters.Items.AddObject(FReportParameters[i].ParamName,
        FReportParameters[i]);
  finally
    lbxReportParameters.Items.EndUpdate;
  end;
end;

procedure TReportConfigureVclFm.PopulateReportsTemplates;
var
  vRTFReport: TRTFReport;
  vDefinitions: TList<TDefinition>;
  vDefinition: TDefinition;
begin
  cbxTemplates.Items.BeginUpdate;
  vDefinitions := TList<TDefinition>.Create;
  try
    cbxTemplates.Clear;
    TConfiguration(FInteractor.Configuration).Definitions.DefinitionsByKind(vDefinitions, clkLibrary);
    TConfiguration(FInteractor.Configuration).Definitions.DefinitionsByKind(vDefinitions, clkDocument);
    for vDefinition in vDefinitions do
    begin
      for vRTFReport in vDefinition.RTFReports.Objects do
        cbxTemplates.Items.AddObject(vDefinition.Name + '.' + vRTFReport.Name, vRTFReport);
    end;
  finally
    vDefinitions.Free;
    cbxTemplates.Items.EndUpdate;
  end;
end;

procedure TReportConfigureVclFm.ShowRTFDocument;
var
  vContent: string;
  vTempStream: TStringStream;
begin
  if FCurrentReport = nil then
    Exit;

  vContent := BuildContent(FCurrentReport.Content.DataString, FCurrentParam, True);
  vTempStream := TStringStream.Create(vContent);
  try
    redtTemplate.Lines.LoadFromStream(vTempStream);
  finally
    vTempStream.Free;
  end;
end;

{ TReportParameter }

procedure TReportParameter.Cancel;
begin
  FParamName := FInitialParamName;
end;

constructor TReportParameter.Create(const AOwner: TReportParamList;
  const AParamName: string; const APosition: Integer);
begin
  inherited Create;

  FOwner := AOwner;
  FParamName := AParamName;
  FInitialParamName := AParamName;
  FPosition := APosition;
  FLength := Length(AParamName);
end;

function TReportParameter.GetChanged: Boolean;
begin
  Result := FInitialParamName <> FParamName;
end;

procedure TReportParameter.Save;
var
  vNewLength: Integer;
begin
  if FParamName = FInitialParamName then
    Exit;

  vNewLength := Length(FParamName);
  if FLength <> vNewLength then
  begin
    FOwner.RecalcPosition(FPosition, vNewLength - FLength);
    FLength := vNewLength;
  end;

  FInitialParamName := FParamName;
end;

{ TReportParamList }

procedure TReportParamList.AddReportParameter(const AParamName: string;
  const APosition: Integer);
var
  vParameter: TReportParameter;
begin
  vParameter := TReportParameter.Create(Self, AParamName, APosition);
  FList.Add(vParameter);
end;

procedure TReportParamList.Clear;
var
  i: Integer;
begin
  if not Assigned(FList) then
    Exit;

  for i := 0 to FList.Count - 1 do
    TReportParameter(FList[i]).Free;

  FList.Clear;
end;

constructor TReportParamList.Create;
begin
  inherited Create;

  FList := TList.Create;
  FReportContent := '';
end;

destructor TReportParamList.Destroy;
begin
  Clear;
  FList.Free;

  inherited Destroy;
end;

procedure TReportParamList.ExtractParameters(const AReportContent: string);
var
  vReportContent: string;
  vParamName: string;
  vLen: Integer;
  vLastPos: Integer;
  vPos: Integer;
const
  cMarker = '$$';
begin
  FReportContent := AReportContent;
  vReportContent := AReportContent;
  vLen := Length(cMarker);
  vLastPos := 0;

  Clear;
  vPos := Pos(cMarker, vReportContent);
  while vPos > 0 do
  begin
    Delete(vReportContent, 1, vPos - 1 + vLen);
    vLastPos := vLastPos + vPos + vLen;

    vPos := Pos(cMarker, vReportContent);
    Assert(vPos > 0, 'Wrong report template!');
    vParamName := Copy(vReportContent, 1, vPos - 1);
    AddReportParameter(vParamName, vLastPos);
    vLastPos := vLastPos + vPos;

    Delete(vReportContent, 1, vPos - 1 + vLen);

    vPos := Pos(cMarker, vReportContent);
  end;
end;

function TReportParamList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TReportParamList.GetReportParameter(
  const AIndex: Integer): TReportParameter;
begin
  Result := TReportParameter(FList[AIndex]);
end;

procedure TReportParamList.RecalcPosition(const APosition, AShift: Integer);
var
  i: Integer;
  vParameter: TReportParameter;
begin
  for i := 0 to FList.Count - 1 do
  begin
    vParameter := GetReportParameter(i);
    if vParameter.Position > APosition then
      vParameter.Position := vParameter.Position + AShift;
  end;
end;

end.
