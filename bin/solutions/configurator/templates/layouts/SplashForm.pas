unit SplashForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Vcl.ExtCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxLabel, Vcl.StdCtrls, dxGDIPlusClasses;

type
  TEditForm = class(TFrame)
    lblVersion: TLabel;
    lblCopyright: TLabel;
    lblName: TPanel;
    lblVersionName: TPanel;
    lblCopyrightName: TPanel;
    lblInfo: TPanel;
    lblSite: TPanel;
    prbProgress: TPanel;
  end;

implementation

{$R *.dfm}

end.
