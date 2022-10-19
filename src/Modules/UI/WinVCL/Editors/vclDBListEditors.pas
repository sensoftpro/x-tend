unit vclDBListEditors;

interface

uses
  DB, DBGrids, uUIBuilder, vclArea, uDomainDataSet;

type
  TDBColumnListEditor = class (TVCLFieldArea)
  private
    FGrid: TDBGrid;
    FDataSource: TDataSource;
    FDataset: TDomainDataSet;
  protected
    procedure DoCreateControl(const AParent: TUIArea; const ALayout: TObject); override;
    procedure DoBeforeFreeControl; override;
  end;

implementation

uses
  SysUtils, Classes, Controls, uConsts, uPresenter, uEntityList, uWinVCLPresenter, uInteractor, uDefinition, uDomain;

{ TDBColumnListEditor }

procedure TDBColumnListEditor.DoBeforeFreeControl;
begin
  FDataset.Close;
  FreeAndNil(FDataSource);
  FreeAndNil(FDataset);
end;

procedure TDBColumnListEditor.DoCreateControl(const AParent: TUIArea; const ALayout: TObject);
var
  i: Integer;
  vEntList: TEntityList;
  vFieldDef: TFieldDef;
  vInteractor: TInteractor;
begin
  vInteractor := TInteractor(FView.Interactor);
  vEntList := TEntityList(FView.DomainObject);
  FDataset := TDomainDataSet.Create(nil);
  FDataset.AssignEntityList(vEntList, FView);
  FDataset.Open;

  FDataSource := TDataSource.Create(nil);
  FDataSource.DataSet := FDataset;

  FGrid := TDBGrid.Create(nil);
  FGrid.Align := alClient;
  FGrid.Font.Size := 12;
  FGrid.DataSource := FDataSource;

  for i := 0 to FDataset.Fields.Count - 1 do
  begin
    vFieldDef := vEntList.MainDefinition.FieldByName(FDataset.Fields[i].FieldName);
    FDataset.Fields[i].DisplayLabel := TDomain(vInteractor.Domain).TranslateFieldDef(vFieldDef, tpCaption);
//    FDataset.Fields[i].DisplayWidth := TWinVCLPresenter(vInteractor.Presenter).GetWidthByType(100, vFieldDef); так не работает
  end;

  for i := 0 to FGrid.Columns.Count - 1 do
  begin
    vFieldDef := vEntList.MainDefinition.FieldByName(FGrid.Columns[i].Field.FieldName);
    FGrid.Columns[i].Width := TWinVCLPresenter(vInteractor.Presenter).GetWidthByType(100, vFieldDef);
  end;

  FControl := FGrid;
end;

initialization

TPresenter.RegisterUIClass('WinVCL', uiListEdit, 'db', TDBColumnListEditor);

end.
