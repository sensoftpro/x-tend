unit DefaultList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.Menus;

type
  TEditForm = class(TFrame)
    Panel1: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel2: TPanel;
    GridPopup: TPopupMenu;
    Add1: TMenuItem;
    Edit1: TMenuItem;
    Delete1: TMenuItem;
    View1: TMenuItem;
    N1: TMenuItem;
    Placeholder1: TMenuItem;
    N2: TMenuItem;
    RecordCount1: TMenuItem;
  end;

implementation

{$R *.dfm}

end.
