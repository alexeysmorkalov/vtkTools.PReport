unit Unit1;

{$I pr.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DBTables, DB, pr_Common, pr_Classes,
  pr_Designer, pr_TxClasses, pr_TxDesigner;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    edMainReport: TComboBox;
    Label2: TLabel;
    edDetailsReport: TComboBox;
    customer: TTable;
    orders: TQuery;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CustomersReport: TprReport;
    ShortOrdersListSub: TprReport;
    OrdersListSub: TprReport;
    Button4: TButton;
    TxCustomersReport: TprTxReport;
    TxOrdersListSub: TprTxReport;
    procedure FormCreate(Sender: TObject);
    procedure edMainReportClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure CustomersReportBandGenerateCell(Sender: TObject;
      HorzBandInfo: TprGenHorzBandInfo; VertBandInfo: TprGenVertBandInfo);
    procedure CustomersReportBeginSubReportGenerate(Sender: TObject;
      ASubReport: TprCustomReport; ASubReportUserData: Integer);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    function GetMainReport: TprCustomReport;
    function GetDetailReport: TprCustomReport;
  public
    { Public declarations }
    procedure AddMainReports;
    procedure AddDetailsReports;

    property MainReport: TprCustomReport read GetMainReport;
    property DetailReport: TprCustomReport read GetDetailReport;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.GetMainReport: TprCustomReport;
begin
  with edMainReport do
  begin
    if (ItemIndex >= 0) and (ItemIndex < Items.Count) then
      Result := TprCustomReport(Items.Objects[ItemIndex])
    else
      Result := nil;
  end;
end;

function TForm1.GetDetailReport: TprCustomReport;
begin
  with edDetailsReport do
  begin
    if (ItemIndex >= 0) and (ItemIndex < Items.Count) then
      Result := TprCustomReport(Items.Objects[ItemIndex])
    else
      Result := nil;
  end;
end;

procedure TForm1.AddMainReports;
begin
  edMainReport.Items.Clear;
  edMainReport.Items.AddObject(CustomersReport.Title, CustomersReport);
  edMainReport.Items.AddObject(TxCustomersReport.Title, TxCustomersReport);
  if edMainReport.Items.Count > 0 then
    edMainReport.ItemIndex := 0;
end;

procedure TForm1.AddDetailsReports;
begin
  edDetailsReport.Items.Clear;
  edDetailsReport.Items.AddObject('(none)', nil);
  
  if MainReport = CustomersReport then
  begin
    edDetailsReport.Items.AddObject(ShortOrdersListSub.Title, ShortOrdersListSub);
    edDetailsReport.Items.AddObject(OrdersListSub.Title, OrdersListSub);
  end
  else
    if MainReport = TxCustomersReport then
    begin
      edDetailsReport.Items.AddObject(TxOrdersListSub.Title, TxOrdersListSub)
    end;

  if edDetailsReport.Items.Count > 0 then
    edDetailsReport.ItemIndex := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddMainReports;
  AddDetailsReports;
end;

procedure TForm1.edMainReportClick(Sender: TObject);
begin
  AddDetailsReports;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ASubReportName: string;
begin
  ActiveControl := edMainReport;

  if MainReport = nil then
    exit;

  if DetailReport = nil then
    ASubReportName := ''
  else
    ASubReportName := DetailReport.Name;

  with MainReport.FindBand('DetailBand') as TprCustomHDetailBand do
    SubReportName := ASubReportName;

  MainReport.DesignReport(True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ActiveControl := edDetailsReport;

  if DetailReport = nil then
    exit;

  orders.ParamByName('CustNo').AsInteger := 1221;
  DetailReport.DesignReport(True);
end;

procedure TForm1.CustomersReportBandGenerateCell(Sender: TObject;
  HorzBandInfo: TprGenHorzBandInfo; VertBandInfo: TprGenVertBandInfo);
begin
  if (HorzBandInfo <> nil) and (HorzBandInfo.Band.Name = 'DetailBand') then
  begin
    // save in the SubReportUserData the key of current record in the dataset of the main report.
    HorzBandInfo.SubReportUserData := customer.FieldByName('CustNo').AsInteger;
  end;
end;

procedure TForm1.CustomersReportBeginSubReportGenerate(Sender: TObject;
  ASubReport: TprCustomReport; ASubReportUserData: Integer);
begin
  // open the parameterized query that is used to build the sub-report.
  orders.Close;
  orders.ParamByName('custno').AsInteger := ASubReportUserData;
  orders.Open;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ActiveControl := edDetailsReport;

  if DetailReport = nil then
    exit;

  orders.ParamByName('CustNo').AsInteger := 1221;
  DetailReport.PrepareReport;
  DetailReport.PreviewPreparedReport(True);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  ASubReportName: string;
begin
  ActiveControl := edMainReport;

  if MainReport = nil then
    exit;

  if DetailReport = nil then
    ASubReportName := ''
  else
    ASubReportName := DetailReport.Name;

  with MainReport.FindBand('DetailBand') as TprCustomHDetailBand do
    SubReportName := ASubReportName;

  MainReport.PrepareReport;
  MainReport.PreviewPreparedReport(True);
end;

end.
