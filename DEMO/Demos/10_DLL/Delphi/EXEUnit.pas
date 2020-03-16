unit EXEUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    hLibrary : THandle; 
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  proc : pointer;
  p2 : procedure;
begin
hLibrary := LoadLibrary(PChar(ExtractFilePath(ParamStr(0))+'DLLProject.dll'));
if hLibrary=0 then
  raise Exception.Create('Error load library '+ExtractFilePath(ParamStr(0))+'DLLProject.dll');

proc := GetProcAddress(hLibrary,'ShowForm');
if proc=nil then
  begin
    FreeLibrary(hLibrary);
    hLibrary := 0;
    raise Exception.Create('Function not found [ShowForm]');
  end;
Button1.Enabled := false;
p2 := proc;
p2;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
if hLibrary<>0 then
  FreeLibrary(hLibrary);
end;

end.
