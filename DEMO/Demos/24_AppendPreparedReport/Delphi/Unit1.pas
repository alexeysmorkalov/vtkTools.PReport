unit Unit1;

interface

{$I pr.inc}

uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DBTables, DB, pr_Common, pr_Classes,
  pr_Designer, pr_TxDesigner, ComCtrls, pr_TxClasses;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    QCUSTOMER: TQuery;
    QORDER: TQuery;
    CUSTOMER: TTable;
    prMainReport: TprReport;
    prRecordReport: TprReport;
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    GroupBox2: TGroupBox;
    Button3: TButton;
    Button4: TButton;
    PB: TProgressBar;
    prTxMainReport: TprTxReport;
    prTxRecordReport: TprTxReport;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GenerateMainReport(AMainReport, ARecordReport: TprCustomReport);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GenerateMainReport(AMainReport, ARecordReport: TprCustomReport);
var
  ms: TMemoryStream;
begin
  AMainReport.ClearPreparedReport;
  PB.Min := 0;
  PB.Max := CUSTOMER.RecordCount;
  PB.Position := 0;
  PB.Visible := True;
  try
    CUSTOMER.First;
    while not CUSTOMER.Eof do
    begin
      PB.Position := PB.Position + 1;
      Update;
      
      QORDER.Close;
      QORDER.ParamByName('custno').AsInteger := CUSTOMER.FieldByName('custno').AsInteger;
      QORDER.Open;

      QCUSTOMER.Close;
      QCUSTOMER.ParamByName('custno').AsInteger := CUSTOMER.FieldByName('custno').AsInteger;
      QCUSTOMER.Open;

      ARecordReport.PrepareReport;

      ms := TMemoryStream.Create;
      try
        ARecordReport.SavePreparedReport(ms);
        ms.Seek(0, soFromBeginning);
        AMainReport.AppendPreparedReport(ms);
      finally
        ms.Free;
      end;

      CUSTOMER.Next;
    end;
  finally
    PB.Visible := False;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  prRecordReport.DesignReport(True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  GenerateMainReport(prMainReport, prRecordReport);
  prMainReport.PreviewPreparedReport(True);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  prTxRecordReport.DesignReport(True);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  GenerateMainReport(prTxMainReport, prTxRecordReport);
  prTxMainReport.PreviewPreparedReport(True);
end;

end.
