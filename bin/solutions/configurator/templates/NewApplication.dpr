program NewApplication;

uses
  Dialogs,
  SysUtils,
{$I ..\..\files.inc}
{$I ..\..\Modules\Storage\SQLite\files.inc}
{$I ..\..\Modules\Reporting\FastReport\files.inc}
{$I ..\..\Modules\Drawing\VCL\files.inc}
{$I ..\..\Modules\UI\WinVCL\DevExpress\files.inc}
  uNewApplicationScript in 'uNewApplicationScript.pas';

{$R *.res}

begin
  try
    TPlatform.Run;
  except
    on E: Exception do
      ShowMessage('Ошибка старта приложения: ' + E.Message);
  end;
end.
