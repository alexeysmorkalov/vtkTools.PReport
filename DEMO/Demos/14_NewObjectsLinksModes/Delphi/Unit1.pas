unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, pr_Common, pr_Classes, pr_Designer, pr_TxClasses, pr_TxDesigner;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    prReport1: TprReport;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    prTxReport1: TprTxReport;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button2Click(Sender: TObject);
begin
  prReport1.DesignReport(True);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  prReport1.PrepareReport;
  prReport1.PreviewPreparedReport(True);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  prTxReport1.DesignReport(True);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  prTxReport1.PrepareReport;
  prTxReport1.PreviewPreparedReport(True);
end;

end.
