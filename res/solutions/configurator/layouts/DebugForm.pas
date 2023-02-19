unit DebugForm;

interface

uses
  Controls, Forms, ExtCtrls, StdCtrls, dxGDIPlusClasses, System.Classes,
  Vcl.ComCtrls;

type
  TEditForm = class(TFrame)
    pcTabs: TPageControl;
    TabSheet1: TTabSheet;
    spltCommon: TSplitter;
    memArea: TPanel;
    pnlView: TPanel;
    TabSheet2: TTabSheet;
    memHolders: TPanel;
    memLog: TPanel;
  end;

implementation

{$R *.dfm}

end.
