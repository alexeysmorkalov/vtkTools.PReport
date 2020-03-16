program Demo;

{$i ..\..\demo.inc}

uses
  SysUtils,
  Forms,
  HW_Form in 'HW_Form.pas' {HWForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'TxPReport demo';
  Application.CreateForm(THWForm, HWForm);
  Application.Run;
end.
