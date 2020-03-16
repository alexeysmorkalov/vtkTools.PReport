unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, pr_Common, pr_TxClasses, StdCtrls;

type
  TForm1 = class(TForm)
    customer: TTable;
    Label1: TLabel;
    Button1: TButton;
    prTxReport1: TprTxReport;
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
prTxReport1.PrepareReport;
prTxReport1.PreviewPreparedReport(true);
end;

end.
