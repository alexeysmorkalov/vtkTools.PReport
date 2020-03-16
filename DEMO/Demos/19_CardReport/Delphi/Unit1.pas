unit Unit1;

interface

{$I pr.inc}

uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Math,

  pr_ArrayDataset, DB, DBTables, pr_Common, pr_Classes, pr_Designer,
  pr_Dataset;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label2: TLabel;
    Table: TTable;
    prReport1: TprReport;
    HorzDataset: TprArrayDataset;
    VertDataset: TprArrayDataset;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure prReport1BandGenerateCell(Sender: TObject;
      HorzBandInfo: TprGenHorzBandInfo; VertBandInfo: TprGenVertBandInfo);
    procedure prReport1InitDetailBandDataSet(Sender: TObject;
      DetailBand: TprBand; DataSet: TObject; const DataSetName: String);
  private
    { Private declarations }
    FFirstCall: Boolean;
    FPerWidth: Integer;
    FPerHeight: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1Change(nil);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  prReport1.DesignReport(True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FPerWidth := StrToInt(Edit1.Text);
  FPerHeight := StrToInt(Edit2.Text);
  HorzDataset.RecCount := FPerWidth;
  VertDataset.RecCount := FPerHeight;
  Table.First;
  FFirstCall := True;
  prReport1.PrepareReport;
  prReport1.PreviewPreparedReport(True);
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  if (Table.RecordCount mod StrToIntDef(Edit1.Text, 1)) = 0 then
    Edit2.Text := IntToStr(Table.RecordCount div StrToIntDef(Edit1.Text, 1))
  else
    Edit2.Text := IntToStr(Table.RecordCount div StrToIntDef(Edit1.Text, 1) + 1);
end;

procedure TForm1.prReport1BandGenerateCell(Sender: TObject;
  HorzBandInfo: TprGenHorzBandInfo; VertBandInfo: TprGenVertBandInfo);
begin
  if (VertBandInfo <> nil) and (VertBandInfo.Band.Name = 'prVDetailBand1') then
  begin
    if not FFirstCall then
      Table.Next;
    FFirstCall := False;
  end;
end;

procedure TForm1.prReport1InitDetailBandDataSet(Sender: TObject;
  DetailBand: TprBand; DataSet: TObject; const DataSetName: String);
begin
  if DataSetName = 'HorzDataset' then
  begin
    HorzDataset.RecCount := Min(FPerWidth, Table.RecordCount - Table.RecNo);
    HorzDataset.First;
  end;
end;

end.
 