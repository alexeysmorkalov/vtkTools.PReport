program Demo;

{$i ..\..\demo.inc}

uses
  Forms,
  HW_Form in 'HW_Form.pas' {HWForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'WIN PReport demo';
  Application.CreateForm(THWForm, HWForm);
  Application.Run;
end.
