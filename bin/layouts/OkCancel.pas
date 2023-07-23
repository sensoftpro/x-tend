unit OkCancel;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.StdCtrls, Vcl.Buttons, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, Vcl.Menus, cxButtons;

type
  TEditForm = class(TFrame)
    btnOk: TPanel;
    btnCancel: TPanel;
    Panel1: TPanel;
  end;

implementation

{$R *.dfm}

end.
