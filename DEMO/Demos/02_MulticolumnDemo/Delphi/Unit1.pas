unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, pr_Common, pr_Classes, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    prReport1: TprReport;
    Table1: TTable;
    Button1: TButton;
    RadioGroup1: TRadioGroup;
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
if RadioGroup1.ItemIndex=0 then
  TprHDetailBand(prReport1.Pages[0].Bands.ByBandType[bthDetail]).ColDirection := prcdTopBottomLeftRight
else
  if RadioGroup1.ItemIndex=1 then
    TprHDetailBand(prReport1.Pages[0].Bands.ByBandType[bthDetail]).ColDirection := prcdLeftRightTopBottom;
if prReport1.PrepareReport then
  prReport1.PreviewPreparedReport(true);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
RadioGroup1.ItemIndex := 0;
end;

end.
