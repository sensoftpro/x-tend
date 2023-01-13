unit LoginForm;

interface

uses
  Controls, Forms, ExtCtrls, StdCtrls, dxGDIPlusClasses, System.Classes;

type
  TEditForm = class(TFrame)
    Image1: TImage;
    lblLogin: TLabel;
    lblPassword: TLabel;
    edLogin: TPanel;
    edPass: TPanel;
    btnOk: TPanel;
    btnCancel: TPanel;
  end;

implementation

{$R *.dfm}

end.
