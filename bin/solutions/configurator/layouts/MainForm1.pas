unit MainForm1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.StdCtrls, Vcl.Buttons, dxGDIPlusClasses, System.ImageList, Vcl.ImgList, Vcl.Menus;

type
  TEditForm = class(TFrame)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    MainMenu: TPopupMenu;
    System1: TMenuItem;
    ShowSysLog1: TMenuItem;
    ShowSettings1: TMenuItem;
    ShowOptions1: TMenuItem;
    LoadChanges1: TMenuItem;
    N1: TMenuItem;
    Quit1: TMenuItem;
    Libraries1: TMenuItem;
    ConfigurationsCaption1: TMenuItem;
    Configurations1: TMenuItem;
    Windows1: TMenuItem;
    CloseAllPages1: TMenuItem;
    ShowAbout1: TMenuItem;
    Label1: TLabel;
    Bevel1: TBevel;
  end;

implementation

{$R *.dfm}

end.

