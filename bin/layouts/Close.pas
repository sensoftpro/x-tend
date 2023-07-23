unit Close;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.StdCtrls, Vcl.Buttons, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, Vcl.Menus, cxButtons;

type
  TEditForm = class(TFrame)
    btnClose: TPanel;
  end;

implementation

{$R *.dfm}

end.
