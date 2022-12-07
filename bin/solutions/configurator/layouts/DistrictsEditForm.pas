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
  end;

implementation

{$R *.dfm}

end.
