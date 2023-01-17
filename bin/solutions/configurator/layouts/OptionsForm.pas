unit OptionsForm;

interface

uses
  Controls, Forms, ExtCtrls, StdCtrls, dxGDIPlusClasses, System.Classes;

type
  TEditForm = class(TFrame)
    btnOk: TPanel;
    btnCancel: TPanel;
    chbShowHorzLinesInGrids: TPanel;
    chbShowBordersForDisabled: TPanel;
    chbMouseMultiSelectInGrids: TPanel;
    chbMarkRequiredFields: TPanel;
    Label1: TLabel;
  end;

implementation

{$R *.dfm}

end.
