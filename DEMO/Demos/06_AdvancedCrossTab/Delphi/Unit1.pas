unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, pr_Common, pr_Classes, StdCtrls;

type
  TForm1 = class(TForm)
    Deb: TQuery;
    Krd: TQuery;
    Table1: TTable;
    prReport1: TprReport;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
Table1.DatabaseName := ExtractFilePath(ParamStr(0));
Deb.DatabaseName := ExtractFilePath(ParamStr(0));
Krd.DatabaseName := ExtractFilePath(ParamStr(0));
Table1.Open;
Deb.Open;
Krd.Open;
end;

end.
