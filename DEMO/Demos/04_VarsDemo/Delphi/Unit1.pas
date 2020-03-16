unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, pr_Common, pr_Classes, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    prReport1: TprReport;
    Query1: TQuery;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
if prReport1.PrepareReport then
  prReport1.PreviewPreparedReport(true);
end;

end.
 