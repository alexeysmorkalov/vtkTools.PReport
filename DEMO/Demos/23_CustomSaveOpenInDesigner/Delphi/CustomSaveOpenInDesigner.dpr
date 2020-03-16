program CustomSaveOpenInDesigner;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Save_Form in 'Save_Form.pas' {SaveForm},
  Load_Form in 'Load_Form.pas' {LoadForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TSaveForm, SaveForm);
  Application.CreateForm(TLoadForm, LoadForm);
  Application.Run;
end.
