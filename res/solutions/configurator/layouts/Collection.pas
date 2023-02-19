unit Collection;

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
    Panel7: TPanel;
    Panel8: TPanel;
    Panel2: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel17: TPanel;
    Panel19: TPanel;
    Panel18: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    GridPopup: TPopupMenu;
    Add1: TMenuItem;
    Edit1: TMenuItem;
    Delete1: TMenuItem;
    View1: TMenuItem;
    N1: TMenuItem;
    Placeholder1: TMenuItem;
    ExportToCsv1: TMenuItem;
    ExportToCsv2: TMenuItem;
    N2: TMenuItem;
    RecordCount1: TMenuItem;
    Panel12: TPanel;
    Label1: TLabel;
    Panel27: TPanel;
    SelectedOpenInPage1: TMenuItem;
    ApplyBestFit1: TMenuItem;
    N3: TMenuItem;
    SystemIdSystemCaption1: TMenuItem;
    Quit1: TMenuItem;
    N4: TMenuItem;
    LoadChanges1: TMenuItem;
    ShowOptions1: TMenuItem;
    ShowSettings1: TMenuItem;
    ShowSysLog1: TMenuItem;
    Storytale1: TMenuItem;
  end;

implementation

{$R *.dfm}

end.
