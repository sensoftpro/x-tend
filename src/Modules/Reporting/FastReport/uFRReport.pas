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

unit uFRReport;

interface

uses
  Classes, uReport, frxClass;

type
  TFRReportData = class(TReportData)
  private
    FPage: TfrxPage;
    procedure OnManualBuildReport(Page: TfrxPage);
  protected
    procedure DoShowBand(const ABandName: string); override;
    procedure DoShowReport(const AReportContent: TStream; const AFileName: string); override;
    procedure DoGeneratePDF(const AReportContent: TStream; const AFileName: string); override;
  end;


implementation

uses
  SysUtils, Variants, frxExportRTF, frxExportPDF, frxExportCSV, frxExportXLS, frxRich, uModule, uDefinition;

{ TFRReportData }

procedure TFRReportData.DoGeneratePDF(const AReportContent: TStream; const AFileName: string);
var
  vReport: TfrxReport;
  vPDFExport: TfrxPDFExport;
begin
  vReport := TfrxReport.Create(nil);
  vPDFExport := TfrxPDFExport.Create(vReport);
  vPDFExport.Producer := 'Sensoft llc.';
  vPDFExport.ProtectionFlags := [ePrint];
  vPDFExport.FileName := AFileName;
  vPDFExport.ShowDialog := False;
  //vPDFExport.ShowProgress := False;
  vPDFExport.OpenAfterExport := False;

  try
    // Формируем сам отчет
    vReport.LoadFromStream(AReportContent);
    // Все формирование отчета происходит в контекстно зависимой переменной
    vReport.OnManualBuild := OnManualBuildReport;

    if FInputParams.Definition.FieldExists('Blank') then
    begin
      if FInputParams['Blank'] then
        TfrxBand(vReport.FindObject('Title')).Visible := False;
    end;
    vReport.PrepareReport;
    vReport.Export(vPDFExport);
  finally
    vPDFExport.Free;
    vReport.Free;
  end;
end;

procedure TFRReportData.DoShowBand(const ABandName: string);
var
  vBand: TfrxBand;
  i: Integer;
  vMemo: TfrxMemoView;
  vPicture: TfrxPictureView;
  vOldData: TStringList;
  vPictureData: Variant;
begin
  vBand := FPage.Report.FindObject(ABandName) as TfrxBand;
  if Assigned(vBand) then
  begin
    vOldData := TStringList.Create;
    try
      for i := 0 to vBand.Objects.Count - 1 do
      begin
        if TObject(vBand.Objects[i]) is TfrxMemoView then
        begin
          vMemo := TfrxMemoView(vBand.Objects[i]);
          vOldData.AddObject(vMemo.Text, vMemo);
          vMemo.Text := FillReportData(vMemo.Text);
        end
        else if TObject(vBand.Objects[i]) is TfrxPictureView then
        begin
          vPicture := TfrxPictureView(vBand.Objects[i]);
          { TODO Возможна утечка памяти}
          vPicture.Picture.Bitmap.FreeImage;
          vPictureData := ValueByName(vPicture.TagStr).Value;
          if not VarIsNull(vPictureData) and VarIsOrdinal(vPictureData) then
            vPicture.LoadPictureFromStream(TMemoryStream(Integer(vPictureData)));
        end;
      end;
      FPage.Report.Engine.ShowBand(vBand);
      for i := 0 to vOldData.Count - 1 do
        TfrxMemoView(vOldData.Objects[i]).Text := vOldData[i];
    finally
      vOldData.Free;
    end;
  end;
end;

procedure TFRReportData.DoShowReport(const AReportContent: TStream; const AFileName: string);
var
  vReport: TfrxReport;
  vRTFExport: TfrxRTFExport;
  vPDFExport: TfrxPDFExport;
  vCSVExport: TfrxCSVExport;
  vXLSExport: TfrxXLSExport;
  vRichText: TfrxRichObject;
begin
  vReport := TfrxReport.Create(nil);

  vRichText := TfrxRichObject.Create(vReport);

  vRTFExport := TfrxRTFExport.Create(vReport);
  vRTFExport.ExportPageBreaks := True;
  vRTFExport.ExportPictures := False;
  vRTFExport.ExportNotPrintable := False;
  vRTFExport.Wysiwyg := True;
  vRTFExport.ShowDialog := False;
  vRTFExport.FileName := AFileName + '.rtf';
  vRTFExport.OpenAfterExport := True;

  vPDFExport := TfrxPDFExport.Create(vReport);
  vPDFExport.Producer := 'Sensoft llc.';
  vPDFExport.ProtectionFlags := [ePrint];
  vPDFExport.FileName := AFileName + '.pdf';
  vPDFExport.ShowDialog := False;
  vPDFExport.OpenAfterExport := True;

  vCSVExport := TfrxCSVExport.Create(vReport);
  //vCSVExport.Separator := #9;
  vCSVExport.FileName := AFileName + '.csv';
  vCSVExport.ShowDialog := False;
  vCSVExport.OpenAfterExport := True;

  vXLSExport := TfrxXLSExport.Create(vReport);
  vXLSExport.FileName := AFileName + '.xls';
  vXLSExport.ShowDialog := False;
  vXLSExport.Wysiwyg := True;
  vXLSExport.OpenExcelAfterExport := True;

  try
    vReport.LoadFromStream(AReportContent);
    // Все формирование отчета происходит в контекстно зависимой переменной
    vReport.OnManualBuild := OnManualBuildReport;

    if Assigned(FInputParams) and FInputParams.Definition.FieldExists('Blank') then
    begin
      if FInputParams['Blank'] then
        TfrxBand(vReport.FindObject('Title')).Visible := False;
    end;

    vReport.ShowReport;
  finally
    vPDFExport.Free;
    vRTFExport.Free;
    vCSVExport.Free;
    vXLSExport.Free;
    vRichText.Free;
    vReport.Free;
  end;
end;

procedure TFRReportData.OnManualBuildReport(Page: TfrxPage);
begin
  FPage := Page;
  try
    PrintData(GetLayers);
  finally
    FPage := nil;
  end;
end;

initialization

TBaseModule.RegisterModule('Reporting', 'FastReport', TFRReportData);

end.

