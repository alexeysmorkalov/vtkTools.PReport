unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, DBTables, pr_TxClasses, pr_Common, pr_Classes,
  pr_Designer, pr_TxDesigner;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    customers: TTable;
    prReport1: TprReport;
    prTxReport1: TprTxReport;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure prReport1FoundPrintVariable(Sender: TObject;
      const AVariableName: String; var AVariableValue: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  prReport1.DesignReport(True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  prReport1.PrepareReport;
  prReport1.PreviewPreparedReport(True);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  prTxReport1.DesignReport(True);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  prTxReport1.PrepareReport;
  prTxReport1.PreviewPreparedReport(True);
end;

procedure TForm1.prReport1FoundPrintVariable(Sender: TObject;
  const AVariableName: String; var AVariableValue: String);
begin
  if (AVariableName = 'PT') then
  begin
    AVariableValue := DateTimeToStr(Now);
  end;
end;

end.
