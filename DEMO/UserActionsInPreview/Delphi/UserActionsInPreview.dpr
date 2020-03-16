program UserActionsInPreview;

{$i ..\..\demo.inc}

uses
  Forms,
  HW_Form in 'HW_Form.pas' {HWForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'User actions in preview';
  Application.CreateForm(THWForm, HWForm);
  Application.Run;
end.
