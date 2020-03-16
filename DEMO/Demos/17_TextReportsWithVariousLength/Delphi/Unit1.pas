unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, pr_Common, pr_TxClasses, DB, DBTables,
  pr_TxDesigner, pr_TxConsts, pr_Utils;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    edRowsPerPage: TEdit;
    bTemplate: TButton;
    bPreview: TButton;
    bPrint: TButton;
    Label2: TLabel;
    edPrinterName: TComboBox;
    prTxReport1: TprTxReport;
    Table1: TTable;
    procedure bTemplateClick(Sender: TObject);
    procedure bPreviewClick(Sender: TObject);
    procedure bPrintClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure InitReport;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.InitReport;
begin
  // change count of rows per page
  TprTxPage(prTxReport1.Pages[0]).LineNum := StrToInt(edRowsPerPage.Text);
end;

procedure TForm1.bTemplateClick(Sender: TObject);
begin
  InitReport;
  prTxReport1.DesignReport(True);
end;

procedure TForm1.bPreviewClick(Sender: TObject);
begin
  InitReport;
  prTxReport1.PrepareReport;
  prTxReport1.PreviewPreparedReport(True);
end;

procedure TForm1.bPrintClick(Sender: TObject);
begin
  if edPrinterName.ItemIndex >= 0 then
  begin
    InitReport;
    prTxReport1.PrepareReport;
    prTxReport1.PrinterName := edPrinterName.Items[edPrinterName.ItemIndex];
    prTxReport1.ESCModelName := '';
    prTxReport1.PaperType := ptPage; // this is default option
    prTxReport1.UseLinesOnPage := False; // use page breaks created when report was generated
    prTxReport1.PrintPreparedReport;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdatePrintersList(edPrinterName.Items, '');
  if edPrinterName.Items.Count > 0 then
    edPrinterName.ItemIndex := 0;
  bPrint.Enabled := edPrinterName.Items.Count > 0;
end;

end.
