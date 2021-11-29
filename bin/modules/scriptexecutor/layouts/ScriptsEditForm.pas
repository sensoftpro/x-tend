unit ScriptsEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Vcl.ExtCtrls, Vcl.ComCtrls;

type
  TForm = class(TFrame)
    Variables: TPanel;
    Methods: TPanel;
    SelectedMethod: TPanel;
    Panel5: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Panel1: TPanel;
    TabSheet2: TTabSheet;
    Panel6: TPanel;
    Panel7: TPanel;
    ident: TPanel;
    pnlTop: TPanel;
    pnlLeft: TPanel;
    pnlClient: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
  end;

implementation

{$R *.dfm}

end.
