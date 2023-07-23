unit DistrictsEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, dxGDIPlusClasses;

type
  TForm = class(TFrame)
    pgc1: TPageControl;
    tsString: TTabSheet;
    tsInteger: TTabSheet;
    tsFloat: TTabSheet;
    tsOthers: TTabSheet;
    tsDate: TTabSheet;
    tsDecor: TTabSheet;
    IntegerProgress: TPanel;
    IntegerGauge: TPanel;
    IntegerFlags: TPanel;
    Integer: TPanel;
    IntegerInfo: TPanel;
    IntegerSpinner: TPanel;
    Float: TPanel;
    Floatcurrencyrate: TPanel;
    Floatinfo: TPanel;
    Floatgauge: TPanel;
    Date: TPanel;
    Time: TPanel;
    Datetime: TPanel;
    Dateinfo: TPanel;
    Selector: TPanel;
    Comport: TPanel;
    Currency: TPanel;
    Currencyinfo: TPanel;
    Bool: TPanel;
    Boolsimple: TPanel;
    Boolimagedaction: TPanel;
    Boolselectedcaption: TPanel;
    Color: TPanel;
    Colorsimple: TPanel;
    lbl1: TLabel;
    lbl2: TLabel;
    img1: TImage;
    shp1: TShape;
    bvl1: TBevel;
    spl1: TSplitter;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    pnlBase: TPanel;
    pnlTop: TPanel;
    Name: TPanel;
    pnlStreets: TPanel;
    pnl1: TPanel;
    pnl2: TPanel;
    Bevel1: TBevel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Panel4: TPanel;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel15: TPanel;
    TabSheet7: TTabSheet;
    Panel7: TPanel;
    Panel8: TPanel;
  end;

implementation

{$R *.dfm}

end.
