unit DataCube;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.Menus;

type
  TEditForm = class(TFrame)
    Panel1: TPanel;
    GridPopup: TPopupMenu;
    ExportToCsv2: TMenuItem;
    N2: TMenuItem;
    RecordCount1: TMenuItem;
    ApplyBestFitPivot1: TMenuItem;
  end;

implementation

{$R *.dfm}

end.
