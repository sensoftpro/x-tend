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
    Panel1: TPanel;
    Panel2: TPanel;
  end;

implementation

{$R *.dfm}

end.

