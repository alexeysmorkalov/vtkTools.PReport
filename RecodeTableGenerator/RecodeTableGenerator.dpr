program RecodeTableGenerator;

uses
  Forms,
  HW_Form in 'HW_Form.pas' {HWForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Recode table generator for TprTxReport';
  Application.CreateForm(THWForm, HWForm);
  Application.Run;
end.
