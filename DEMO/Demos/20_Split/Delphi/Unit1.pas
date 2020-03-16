unit Unit1;

interface

{$I pr.inc}
uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, DB, DBTables, StdCtrls, pr_Common, pr_Classes,
  pr_Designer, pr_TxClasses, pr_TxDesigner, pr_Dataset, pr_ArrayDataset;

type
  TForm1 = class(TForm)
    Table1: TTable;
    prReport1: TprReport;
    prTxReport1: TprTxReport;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Button5: TButton;
    Button6: TButton;
    GroupBox5: TGroupBox;
    Button7: TButton;
    Button8: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    GroupBox6: TGroupBox;
    Button3: TButton;
    Button4: TButton;
    prReport2: TprReport;
    biolife: TTable;
    prTxReport2: TprTxReport;
    GroupBox7: TGroupBox;
    Label1: TLabel;
    edFileName: TEdit;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Button9: TButton;
    prFileReport: TprReport;
    prTxFileReport: TprTxReport;
    prFileDataset: TprArrayDataset;
    cbRotate: TCheckBox;
    prFileRotatedReport: TprReport;
    prArrayDatasetTemp: TprArrayDataset;
    procedure Button5Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure prFileDatasetGetFieldValue(Sender: TprArrayDataset;
      ARecNo: Integer; const AFieldName: String; var AFieldValue: Variant);
    procedure RadioButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button5Click(Sender: TObject);
begin
  prReport1.DesignReport(True);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  prReport2.DesignReport(True);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  prReport1.PrepareReport;
  prReport1.PreviewPreparedReport(True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  prReport2.PrepareReport;
  prReport2.PreviewPreparedReport(True);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  prTxReport1.DesignReport(True);
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
  prTxReport1.PrepareReport;
  prTxReport1.PreviewPreparedReport(True);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  prTxReport2.DesignReport(True);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  prTxReport2.PrepareReport;
  prTxReport2.PreviewPreparedReport(True);
end;

procedure TForm1.Button9Click(Sender: TObject);
begin
  if RadioButton1.Checked then
  begin
    if cbRotate.Checked then
    begin
      prFileRotatedReport.PrepareReport;
      prFileRotatedReport.PreviewPreparedReport(True);
    end
    else
    begin
      prFileReport.PrepareReport;
      prFileReport.PreviewPreparedReport(True);
    end;
  end
  else
  begin
    prTxFileReport.PrepareReport;
    prTxFileReport.PreviewPreparedReport(True);
  end;
end;

procedure TForm1.prFileDatasetGetFieldValue(Sender: TprArrayDataset;
  ARecNo: Integer; const AFieldName: String; var AFieldValue: Variant);
var
  AStrings: TStringList;
begin
  if CompareText(AFieldName, 'FileName') = 0 then
    AFieldValue := edFileName.Text
  else
  begin
    AStrings := TStringList.Create;
    try
      AStrings.LoadFromFile(edFileName.Text);
      AFieldValue := AStrings.Text;
    finally
      AStrings.Free;
    end;
  end;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  cbRotate.Enabled := RadioButton1.Checked;
end;

end.
