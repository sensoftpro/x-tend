unit AboutForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Vcl.ExtCtrls,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxLabel, Vcl.StdCtrls, dxGDIPlusClasses;

type
  TEditForm = class(TFrame)
    lblVersion: TLabel;
    lblCopyright: TLabel;
    Image1: TImage;
    lblName: TPanel;
    lblEmail: TLabel;
    lblWeb: TLabel;
    lblAllRightsReserved: TLabel;
    btnOk: TPanel;
    btnFeedback: TPanel;
    lblSite: TPanel;
    lblMail: TPanel;
  end;

implementation

{$R *.dfm}

end.
