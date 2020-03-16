unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, pr_Common, pr_Classes, pr_TxClasses, StdCtrls;

type
  TForm1 = class(TForm)
    HorDataset: TTable;
    VertDataset: TTable;
    prReport1: TprReport;
    prTxReport1: TprTxReport;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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

procedure TForm1.Button2Click(Sender: TObject);
begin
if prTxReport1.PrepareReport then
  prTxReport1.PreviewPreparedReport(true);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
HorDataset.Open;
VertDataset.Open;
end;

end.
