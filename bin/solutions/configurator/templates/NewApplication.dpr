program NewApplication;

uses
  Dialogs,
  SysUtils,
{$I ..\..\files.inc}
{$I ..\..\Common\Scene\files.inc}
{$I ..\..\Modules\Storage\OLEDB\files.inc}
{$I ..\..\Modules\Reporting\FastReport\files.inc}
{$I ..\..\Modules\Drawing\GDIPlus\files.inc}
{$I ..\..\Modules\UI\WinVCL\files.inc}
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
