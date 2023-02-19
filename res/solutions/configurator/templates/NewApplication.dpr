program NewApplication;

{$DEFINE VCL_UI}

uses
  {$IFDEF VCL_UI} Dialogs, {$ENDIF}
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
{$IFDEF VCL_UI}
    on E: Exception do
      ShowMessage('Ошибка старта приложения: ' + E.Message);
{$ENDIF}
  end;
end.
