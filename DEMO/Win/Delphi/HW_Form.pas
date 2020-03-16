unit HW_Form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ImgList, Db, DBTables, Buttons,

  pr_Utils, pr_Common, pr_classes, pr_Designer, pr_Parser, pr_Dataset, pr_ShapeObj;

type
  THWForm = class(TForm)
    CBTemplates: TComboBox;
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
    prStringsDataset1: TprStringsDataset;
    CompsDataset: TprEventsDataset;
    RTF: TTable;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    CBShowProgress: TCheckBox;
    Label1: TLabel;
    CBCanUserEdit: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure bDesignClick(Sender: TObject);
    procedure bPreviewClick(Sender: TObject);
    procedure CBTemplatesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ToolbarButton971Click(Sender: TObject);
    procedure customersAfterScroll(DataSet: TDataSet);
    procedure OrdersLinkedToCustomersAfterScroll(DataSet: TDataSet);
    procedure FormDestroy(Sender: TObject);
    procedure CompsDatasetActive(prDataset: TprDataset;
      var IsActive: Boolean);
    procedure CompsDatasetEof(prDataset: TprDataset; var IsEof: Boolean);
    procedure CompsDatasetFirst(prDataset: TprDataset);
    procedure CompsDatasetGetFieldsList(prDataset: TprDataset;
      L: TStrings);
    procedure CompsDatasetGetFieldValue(prDataset: TprDataset;
      const FieldName: String; var FieldValue: Variant);
    procedure CompsDatasetNext(prDataset: TprDataset);
    procedure CompsDatasetPrior(prDataset: TprDataset);
    procedure CompsDatasetOpen(prDataset: TprDataset);
    procedure CompsDatasetRecordCount(prDataset: TprDataset;
      var RecordCount: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
  private
    { Private declarations }
    SumsPosts : TStringList;
    sums : array of double;
    CDIndex : integer;
     
    procedure OnDestroyDesigner(Sender : TObject);
    procedure OnDesignerSaveTemplate(Sender : TObject; var FileName : string; var Stream : TStream; var CancelSave : boolean; var IsBinaryFormat : boolean; IsSaveAs : boolean);

    procedure InitTemplatesList;
    function InitReport : TprReport;

    procedure OnUnknownVariable(Sender : TObject; const VarName : string;
                                var Value : TprVarValue;
                                var IsProcessed : boolean);
  public
    { Public declarations }
  end;

var
  HWForm: THWForm;

implementation

{$R *.DFM}
procedure THWForm.OnDestroyDesigner;
begin
CBTemplates.Repaint;
end;

procedure THWForm.OnDesignerSaveTemplate;
var
  i,ii : integer;
  S: string;
begin
  S := ExtractFileName(FileName);
  if IsSaveAs then
    FileName := ExtractFilePath(ParamStr(0))+'Reports\' + S;
  ii := CBTemplates.ItemIndex;
  i := CBTemplates.Items.IndexOfObject(Sender);
  if i<>-1 then
  begin
    if S = '' then
      CBTemplates.Items[i] := ' '
    else
      CBTemplates.Items[i] := S;
  end;
  CBTemplates.ItemIndex := ii;
end;

procedure THWForm.OnUnknownVariable;
var
  i : integer;
begin
if CompText(VarName,'POST_SUM')=0 then
  begin
    IsProcessed:=true;
    _vSetAsDouble(Value,0);
    if not sHor.Active or not sVert.Active then
      exit;

    i:=SumsPosts.IndexOf(Format('%s;%s',[sHor.FieldByName('KS').AsString,sVert.FieldByName('KS').AsString]));
    if i<>-1 then
      _vSetAsDouble(Value,sums[i])
    else
      _vSetAsDouble(Value,0);
  end;
end;

function THWForm.InitReport;
begin
Result:=TprReport.Create(Self);
Result.OnDestroyDesigner := OnDestroyDesigner;
Result.OnDestroyPreview := OnDestroyDesigner;
Result.OnCreateDesigner := OnDestroyDesigner;
Result.OnCreatePreview := OnDestroyDesigner;
Result.OnDesignerSaveTemplate := OnDesignerSaveTemplate;
Result.OnUnknownVariable := OnUnknownVariable;
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
SumsPosts:=TStringList.Create;

Left := 0;
Top := 0;

// make list of templates
InitTemplatesList;

// connect to database
Database.Params.Values['Path'] := ExtractFilePath(ParamStr(0))+'..\..\DBF';
Database.Open;
for i:=0 to ComponentCount-1 do
  if Components[i] is TDataset then
    TDataSet(Components[i]).Open;

i:=0;
SetLength(sums,Q_SPOST.RecordCount);
Q_SPOST.First;
while not Q_SPOST.Eof do
  begin
    sums[i]:=Q_SPOST.FieldByName('S').AsFloat;
    SumsPosts.Add(Format('%s;%s',[Q_SPOST.FieldByName('DKS').AsString,Q_SPOST.FieldByName('KKS').AsString]));

    Inc(i);
    Q_SPOST.Next;
  end;
end;

procedure THWForm.bDesignClick(Sender: TObject);
var
  r : TprReport;
begin
if CBTemplates.Items.Objects[CBTemplates.ItemIndex]=nil then
  begin
    R:=InitReport;
    R.LoadTemplateFromFile(ExtractFilePath(ParamStr(0))+'Reports\'+CBTemplates.Items[CBTemplates.ItemIndex],false);
    CBTemplates.Items.Objects[CBTemplates.ItemIndex]:=R;
  end
else
  R:=TprReport(CBTemplates.Items.Objects[CBTemplates.ItemIndex]);

R.DesignReport(false);
end;

procedure THWForm.bPreviewClick(Sender: TObject);
var
  R : TprReport;
  tc : cardinal;
begin

if CBTemplates.Items.Objects[CBTemplates.ItemIndex]=nil then
  begin
    R:=InitReport;
    R.LoadTemplateFromFile(ExtractFilePath(ParamStr(0))+'Reports\'+CBTemplates.Items[CBTemplates.ItemIndex],false);
    CBTemplates.Items.Objects[CBTemplates.ItemIndex]:=R;
  end
else
  R:=TprReport(CBTemplates.Items.Objects[CBTemplates.ItemIndex]);

R.ShowProgress := CBShowProgress.Checked;
R.CanUserEdit := CBCanUserEdit.Checked;
if R.CanUserEdit then
  begin
    R.PreviewParams.Options := [prpoAllowChangePreviewMode,prpoShowMenu,prpoAllowShowHideToolbars,prpoAllowDragToolbars];
    R.PreviewParams.ShowToolbars := [prptPreviewCommon,prptEdit,prptInsertObject,prptText,prptBorders,prptAlign,prptSize,prptNudge,prptObjects,prptObject];
  end
else
  begin
    R.PreviewParams.Options := [];
    R.PreviewParams.ShowToolbars := [prptPreviewCommon];
  end;

if R.PreviewForm=nil then
  begin
    tc := GetTickCount;
    R.PrepareReport;
    tc := GetTickCount-tc;
    Label1.Caption := Trim(Format('Report generate time: %-12.4f',[tc/1000]))+' sec';
  end;

if R.ReportPrepared then
  R.PreviewPreparedReport(false)
else
  if R.ActionCanceled then
    ShowMessage('The report''s generating is cancelled by the user');
end;

procedure THWForm.CBTemplatesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  Report : TprReport;
begin
Report:=TprReport(CBTemplates.Items.Objects[index]);
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
      Font.Style := [];
    TextOut(Rect.Left+ImageList1.Width*2+2,Rect.Top+1,CBTemplates.Items[index]);
  end;
end;

procedure THWForm.ToolbarButton971Click(Sender: TObject);
var
  R : TprReport;
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

procedure THWForm.FormDestroy(Sender: TObject);
begin
SumsPosts.Free;
end;

procedure THWForm.CompsDatasetActive(prDataset: TprDataset;
  var IsActive: Boolean);
begin
IsActive:=true;
end;

procedure THWForm.CompsDatasetEof(prDataset: TprDataset;
  var IsEof: Boolean);
begin
IsEof:=CDIndex>=ComponentCount;
end;

procedure THWForm.CompsDatasetFirst(prDataset: TprDataset);
begin
CDIndex:=0;
end;                                          

procedure THWForm.CompsDatasetGetFieldsList(prDataset: TprDataset;
  L: TStrings);
begin
L.Add('NAME');
L.Add('CLASSNAME');
end;

procedure THWForm.CompsDatasetGetFieldValue(prDataset: TprDataset;
  const FieldName: String; var FieldValue: Variant);
begin
if CompText(FieldName,'NAME')=0 then
  FieldValue:=Components[CDIndex].Name
else
  if CompText(FieldName,'CLASSNAME')=0 then
    FieldValue:=Components[CDIndex].ClassName
end;

procedure THWForm.CompsDatasetNext(prDataset: TprDataset);
begin
if CDIndex<=ComponentCount then
  Inc(CDIndex);
end;

procedure THWForm.CompsDatasetPrior(prDataset: TprDataset);
begin
if CDIndex>0 then
  Dec(CDIndex);
end;

procedure THWForm.CompsDatasetOpen(prDataset: TprDataset);
begin
CDIndex:=0;
end;

procedure THWForm.CompsDatasetRecordCount(prDataset: TprDataset;
  var RecordCount: Integer);
begin
RecordCount:=ComponentCount;
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

