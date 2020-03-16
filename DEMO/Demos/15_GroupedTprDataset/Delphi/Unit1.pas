unit Unit1;

interface

{$I pr.inc}

uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, pr_Common, pr_Classes, FileDataset, typinfo, DB,
  DBTables;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    edDirectory: TEdit;
    Label2: TLabel;
    edOrder: TComboBox;
    bPrint: TButton;
    prReport1: TprReport;
    Table1: TTable;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bPrintClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Dataset: TprFileDataset;
  end;

var
  Form1: TForm1;

implementation

uses
  vgr_Functions;
  
{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  edDirectory.Text := ExtractFileDir(ParamStr(0));
  GetEnumNamesToStrings(TypeInfo(TprFileDatasetOrder), edOrder.Items, 2);
  edOrder.ItemIndex := 1;

  Dataset := TprFileDataset.Create(Self);
  Dataset.Name := 'FileDataset';

  TprHDetailBand(prReport1.FindObject('prHDetailBand1')).DataSetName := Dataset.Name;
  prReport1.Values[0].DataSetName := Dataset.Name;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Dataset.Free;
end;

procedure TForm1.bPrintClick(Sender: TObject);
begin
  Dataset.Directory := edDirectory.Text;
  Dataset.Order := TprFileDatasetOrder(edOrder.ItemIndex);
  case TprFileDatasetOrder(edOrder.ItemIndex) of
    doFileName: prReport1.Groups[0].Valid := 'FileDataset.FileName';
    doFileExt: prReport1.Groups[0].Valid := 'FileDataset.FileExt';
    doCreated: prReport1.Groups[0].Valid := 'FileDataset.Created';
  end;

  prReport1.Variables.AddVariable('Directory', Dataset.Directory);
  prReport1.Variables.AddVariable('Order', GetEnumName(TypeInfo(TprFileDatasetOrder), edOrder.ItemIndex));

  prReport1.PrepareReport;
  prReport1.PreviewPreparedReport(True);
end;

end.
