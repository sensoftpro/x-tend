unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.StdCtrls, Vcl.Buttons, dxGDIPlusClasses, System.ImageList, Vcl.ImgList, Vcl.Menus;

type
  TEditForm = class(TFrame)
    WorkArea: TPanel;
    MainMenu: TPopupMenu;
    System1: TMenuItem;
    ShowOptions1: TMenuItem;
    N1: TMenuItem;
    Quit1: TMenuItem;
    ShowAbout1: TMenuItem;
    ShowSettings1: TMenuItem;
    ToolBar: TPopupMenu;
    SysUsersWorkAreaWorkAreaLayoutCollection1: TMenuItem;
    ConfigurationsWorkAreaWorkAreaLayoutCollection1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    StreetsWorkAreaWorkAreaLayoutCollection1: TMenuItem;
    DistrictsWorkAreaWorkAreaLayoutCollection1: TMenuItem;
    Panel3: TPanel;
    Panel4: TPanel;
    ConfigurationsCaption1: TMenuItem;
    Configurations1: TMenuItem;
    DistrictsWorkAreaWorkAreaLayoutCollection2: TMenuItem;
    SystemIdSystemCaption1: TMenuItem;
    Quit2: TMenuItem;
    Parent3Caption31: TMenuItem;
    ShowAbout2: TMenuItem;
    Panel5: TPanel;
  end;

implementation

{$R *.dfm}

end.

