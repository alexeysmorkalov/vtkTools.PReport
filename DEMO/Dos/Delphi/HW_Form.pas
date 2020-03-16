unit HW_Form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus,

  pr_Common, pr_Txclasses, pr_Txdesigner, ImgList, Db, DBTables, Buttons;

type
  THWForm = class(TForm)
    CBTemplates: TComboBox;
    PMWindows: TPopupMenu;
    ImageList1: TImageList;
    LookupsParts: TTable;
    ItemsLinkedToOrders: TTable;
    ItemsLinkedToOrdersOrderNo: TFloatField;
    ItemsLinkedToOrdersItemNo: TFloatField;
    ItemsLinkedToOrdersPartNo: TFloatField;
    ItemsLinkedToOrdersQty: TIntegerField;
    ItemsLinkedToOrdersDiscount: TFloatField;
    ItemsLinkedToOrdersListPrice: TCurrencyField;
    ItemsLinkedToOrdersDescription: TStringField;
    OrdersLinkedToCustomers: TTable;
    customers: TTable;
    orders: TTable;
    parts: TTable;
    Table1: TTable;
    Table2: TTable;
    biolife: TTable;
    CustomersByName: TTable;
    Q_SPOST: TQuery;
    sVert: TTable;
    sHor: TTable;
    RepQuery: TQuery;
    RepQueryCustNo: TFloatField;
    RepQueryCompany: TStringField;
    RepQueryAddr1: TStringField;
    RepQueryAddr2: TStringField;
    RepQueryCity: TStringField;
    RepQueryState: TStringField;
    RepQueryZip: TStringField;
    RepQueryCountry: TStringField;
    RepQueryPhone: TStringField;
    RepQueryFAX: TStringField;
    RepQueryTaxRate: TFloatField;
    RepQueryContact: TStringField;
    RepQueryLastInvoiceDate: TDateTimeField;
    RepQueryOrderNo: TFloatField;
    RepQueryCustNo_1: TFloatField;
    RepQuerySaleDate: TDateTimeField;
    RepQueryShipDate: TDateTimeField;
    RepQueryEmpNo: TIntegerField;
    RepQueryShipToContact: TStringField;
    RepQueryShipToAddr1: TStringField;
    RepQueryShipToAddr2: TStringField;
    RepQueryShipToCity: TStringField;
    RepQueryShipToState: TStringField;
    RepQueryShipToZip: TStringField;
    RepQueryShipToCountry: TStringField;
    RepQueryShipToPhone: TStringField;
    RepQueryShipVIA: TStringField;
    RepQueryPO: TStringField;
    RepQueryTerms: TStringField;
    RepQueryPaymentMethod: TStringField;
    RepQueryItemsTotal: TCurrencyField;
    RepQueryTaxRate_1: TFloatField;
    RepQueryFreight: TCurrencyField;
    RepQueryAmountPaid: TCurrencyField;
    RepQueryOrderNo_1: TFloatField;
    RepQueryItemNo: TFloatField;
    RepQueryPartNo: TFloatField;
    RepQueryQty: TIntegerField;
    RepQueryDiscount: TFloatField;
    RepQueryPartNo_1: TFloatField;
    RepQueryVendorNo: TFloatField;
    RepQueryDescription: TStringField;
    RepQueryOnHand: TFloatField;
    RepQueryOnOrder: TFloatField;
    RepQueryCost: TCurrencyField;
    RepQueryListPrice: TCurrencyField;
    Database: TDatabase;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    CBShowProgress: TCheckBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure bDesignClick(Sender: TObject);
    procedure bPreviewClick(Sender: TObject);
    procedure CBTemplatesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ToolbarButton971Click(Sender: TObject);
    procedure customersAfterScroll(DataSet: TDataSet);
    procedure OrdersLinkedToCustomersAfterScroll(DataSet: TDataSet);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
  private
    { Private declarations }
    procedure OnDestroyDesigner(Sender : TObject);
    procedure OnDesignerSaveTemplate(Sender : TObject; var FileName : string; var Stream : TStream; var CancelSave : boolean; var IsBinaryFormat : boolean; IsSaveAs : boolean);

    procedure InitTemplatesList;
    function  InitReport : TprTxReport;
  public
    { Public declarations }
  end;

var
  HWForm: THWForm;

implementation

uses
  pr_TxConsts;

{$R *.DFM}

procedure THWForm.OnDestroyDesigner;
begin
CBTemplates.Repaint;
end;

procedure THWForm.OnDesignerSaveTemplate;
var
  I, II: integer;
  S: string;
begin
  S := ExtractFileName(FileName);
  if IsSaveAs then
    FileName := ExtractFilePath(ParamStr(0)) + 'Reports\' + S;
  II := CBTemplates.ItemIndex;
  I := CBTemplates.Items.IndexOfObject(Sender);
  if I <> -1 then
  begin
    if S = '' then
      CBTemplates.Items[I] := ' '
    else
      CBTemplates.Items[I] := S;
  end;
  CBTemplates.ItemIndex := ii;
end;

function THWForm.InitReport;
begin
Result:=TprTxReport.Create(Self);
Result.Copies := 3;
Result.OnDestroyDesigner:=OnDestroyDesigner;
Result.OnDestroyPreview :=OnDestroyDesigner;
Result.OnCreateDesigner :=OnDestroyDesigner;
Result.OnCreatePreview  :=OnDestroyDesigner;
Result.OnDesignerSaveTemplate   :=OnDesignerSaveTemplate;
end;

procedure THWForm.InitTemplatesList;
var
  SearchRec : TSearchRec;
begin
CBTemplates.Clear;

if FindFirst(ExtractFilePath(ParamStr(0))+'Reports\*.prt', faAnyFile, SearchRec)=0 then
  CBTemplates.Items.Add(SearchRec.Name);
while FindNext(SearchRec) = 0 do
  CBTemplates.Items.Add(SearchRec.Name);
FindClose(SearchRec);

if CBTemplates.Items.Count>0 then
  CBTemplates.ItemIndex:=0;
end;

procedure THWForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
ThousandSeparator:=' ';

Left := 0;
Top := 0;

// Формируем список
InitTemplatesList;

// Доступ к данным
Database.Params.Values['Path']:=ExtractFilePath(ParamStr(0))+'..\..\DBF';
Database.Open;
for i:=0 to ComponentCount-1 do
  if Components[i] is TDataset then
    TDataSet(Components[i]).Open;
end;

procedure THWForm.bDesignClick(Sender: TObject);
var
  r : TprTxReport;
begin
if CBTemplates.Items.Objects[CBTemplates.ItemIndex]=nil then
  begin
    R:=InitReport;
    R.LoadTemplateFromFile(ExtractFilePath(ParamStr(0))+'Reports\'+CBTemplates.Items[CBTemplates.ItemIndex],false);
    CBTemplates.Items.Objects[CBTemplates.ItemIndex]:=R;
  end
else
  R:=TprTxReport(CBTemplates.Items.Objects[CBTemplates.ItemIndex]);

R.DesignReport(false);
end;

procedure THWForm.bPreviewClick(Sender: TObject);
var
  R : TprTxReport;
  tc : cardinal;
begin
if CBTemplates.Items.Objects[CBTemplates.ItemIndex]=nil then
  begin
    R := InitReport;
    R.LoadTemplateFromFile(ExtractFilePath(ParamStr(0))+'Reports\'+CBTemplates.Items[CBTemplates.ItemIndex],false);
    CBTemplates.Items.Objects[CBTemplates.ItemIndex]:=R;
  end
else
  R:=TprTxReport(CBTemplates.Items.Objects[CBTemplates.ItemIndex]);

R.ShowProgress:=CBShowProgress.Checked;
if R.PreviewForm=nil then
  begin
    tc := GetTickCount;
    R.PrepareReport;
    tc := GetTickCount-tc;
    Label1.Caption := Trim(Format('Report generate time: %-12.4f',[tc/1000]))+' sec';
  end;

if R.ReportPrepared then
  R.PreviewPreparedReport(false);
end;

procedure THWForm.CBTemplatesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  Report : TprTxReport;
begin
Report:=TprTxReport(CBTemplates.Items.Objects[index]);
with CBTemplates.Canvas do
  begin
    FillRect(Rect);
    if Report<>nil then
      begin
        Font.Style:=[fsBold];
        if Report.DesignerForm<>nil then
          ImageList1.Draw(CBTemplates.Canvas,Rect.Left+1,Rect.Top,0);
        if Report.PreviewForm<>nil then
          ImageList1.Draw(CBTemplates.Canvas,Rect.Left+ImageList1.Width+2,Rect.Top,1);
      end
    else
      Font.Style:=[];
    TextOut(Rect.Left+ImageList1.Width*2+2,Rect.Top+1,CBTemplates.Items[index]);
  end;
end;

procedure THWForm.ToolbarButton971Click(Sender: TObject);
var
  R : TprTxReport;
begin
R:=InitReport;
CBTemplates.Items.AddObject(' ',R);
CBTemplates.ItemIndex:=CBTemplates.Items.Count-1;
R.DesignReport(false);
end;

procedure THWForm.customersAfterScroll(DataSet: TDataSet);
begin
OrdersLinkedToCustomers.Filter:=Format('[CUSTNO]=%d',[Customers.FieldByName('CUSTNO').AsInteger]);
OrdersLinkedToCustomers.Filtered:=true;
end;

procedure THWForm.OrdersLinkedToCustomersAfterScroll(DataSet: TDataSet);
begin
ItemsLinkedToOrders.Filter:=Format('[OrderNo]=%d',[OrdersLinkedToCustomers.FieldByName('ORDERNO').AsInteger]);
ItemsLinkedToOrders.Filtered:=true;
end;

procedure THWForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i : integer;
begin
for i:=0 to CBTemplates.Items.Count-1 do
  if CBTemplates.Items.Objects[i]<>nil then
    with TprCustomReport(CBTemplates.Items.Objects[i]) do
      begin
        OnDestroyDesigner := nil;
        OnDestroyPreview := nil;
        OnCreateDesigner := nil;
        OnCreatePreview := nil;
        OnDesignerSaveTemplate := nil;
        OnUnknownVariable := nil;
      end;
end;

procedure THWForm.SpeedButton4Click(Sender: TObject);
var
  i : integer;
begin
for i:=0 to CBTemplates.Items.Count-1 do
  if CBTemplates.Items.Objects[i]<>nil then
    TprCustomReport(CBTemplates.Items.Objects[i]).ClearPreparedReport;
end;

procedure THWForm.SpeedButton5Click(Sender: TObject);
var
  i : integer;
begin
for i:=0 to CBTemplates.Items.Count-1 do
  if CBTemplates.Items.Objects[i]<>nil then
    begin
      TprCustomReport(CBTemplates.Items.Objects[i]).Free;
      CBTemplates.Items.Objects[i] := nil;
    end;
CBTemplates.Repaint;
end;

end.

