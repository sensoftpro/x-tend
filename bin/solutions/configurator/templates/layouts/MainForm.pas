unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.StdCtrls, Vcl.Buttons, dxGDIPlusClasses, System.ImageList, Vcl.ImgList, Vcl.Menus;

type
  TEditForm = class(TFrame)
    WorkArea: TPanel;
    MainMenu: TPopupMenu;
    SystemCaption1: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    DetectorsLayoutDetectorsCollectionContentWorkAreamodal1: TMenuItem;
    N5: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    WindowsCaption1: TMenuItem;
    ArrangeCascade1: TMenuItem;
    ArrangeVert1: TMenuItem;
    ArrangeHorz1: TMenuItem;
    N8: TMenuItem;
    CloseAllWindows2: TMenuItem;
    ShowAbout2: TMenuItem;
    ShowSysLog2: TMenuItem;
    Panel6: TPanel;
    ToolBar: TPopupMenu;
    Panel1: TPanel;
    SysUsers1: TMenuItem;
  end;

implementation

{$R *.dfm}

end.

