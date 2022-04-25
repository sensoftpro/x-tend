program Layouts;

uses
  Vcl.Forms,
  uMainForm in 'uMainForm.pas' {frMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrMain, frMain);
  Application.Run;
end.

