unit DLLUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, pr_TxClasses, pr_Common, pr_Classes, StdCtrls, pr_Designer, pr_TxDesigner;

type
  TForm1 = class(TForm)
    Table1: TTable;
    prReport1: TprReport;
    prTxReport1: TprTxReport;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
prReport1.DesignReport(true);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
prTxReport1.DesignReport(true);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
prReport1.PrepareReport;
prReport1.PreviewPreparedReport(true);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
prTxReport1.PrepareReport;
prTxReport1.PreviewPreparedReport(true);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action := caFree;
end;

end.
