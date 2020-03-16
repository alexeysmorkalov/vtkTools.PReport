unit HW_Form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, StdCtrls, pr_Common, pr_Classes, pr_TxClasses;

type
  THWForm = class(TForm)
    Database: TDatabase;
    Query1: TQuery;
    Customers: TTable;
    Button1: TButton;
    Button2: TButton;
    rCustomers: TprReport;
    rOrders: TprReport;
    rOrder: TprReport;
    Query2: TQuery;
    rTxCustomers: TprTxReport;
    rTxOrders: TprTxReport;
    rTxOrder: TprTxReport;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure rCustomersPreviewGetUserData(Sender: TObject; Obj: TprObj;
      ObjRec: TprObjRec; var PreviewUserData: TprPreviewUserData);
    procedure rCustomersPreviewMouseMove(Sender: TObject;
      PreviewUserData: TprPreviewUserData; var cur: TCursor;
      var HighlightObject: Boolean);
    procedure rCustomersPreviewMouseDown(Sender: TObject;
      PreviewUserData: TprPreviewUserData; Shift : TShiftState);
    procedure rOrdersPreviewGetUserData(Sender: TObject; Obj: TprObj;
      ObjRec: TprObjRec; var PreviewUserData: TprPreviewUserData);
    procedure rOrdersPreviewMouseMove(Sender: TObject;
      PreviewUserData: TprPreviewUserData; var cur: TCursor;
      var HighlightObject: Boolean);
    procedure rTxCustomersPreviewGetUserData(Sender: TObject; Obj: TprObj;
      ObjRec: TprObjRec; var PreviewUserData: TprPreviewUserData);
    procedure rTxCustomersPreviewMouseDown(Sender: TObject;
      PreviewUserData: TprPreviewUserData; Shift : TShiftState);
    procedure rTxOrdersPreviewGetUserData(Sender: TObject; Obj: TprObj;
      ObjRec: TprObjRec; var PreviewUserData: TprPreviewUserData);
    procedure Button2Click(Sender: TObject);
    procedure rTxOrdersPreviewDblClick(Sender: TObject;
      PreviewUserData: TprPreviewUserData);
    procedure rOrdersPreviewDblClick(Sender: TObject;
      PreviewUserData: TprPreviewUserData);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HWForm: THWForm;

implementation

{$R *.DFM}

procedure THWForm.FormCreate(Sender: TObject);
begin
Database.Connected := false;
Database.Params.Values['Path'] := {ExtractFilePath(ParamStr(0))+}'..\..\DBF';
Database.Connected := true;
Customers.Open;
Left := 0;
Top := 0;
end;

procedure THWForm.Button1Click(Sender: TObject);
begin
rCustomers.PrepareReport;
rCustomers.PreviewPreparedReport(false);
end;

procedure THWForm.rCustomersPreviewGetUserData(Sender: TObject;
  Obj: TprObj; ObjRec: TprObjRec; var PreviewUserData: TprPreviewUserData);
begin
if Obj.Name='prMemoObj2' then
  begin
    PreviewUserData := TprPreviewUserData.Create;
    PreviewUserData.Tag := Customers.FieldByName('CustNo').AsInteger;
  end;
end;

procedure THWForm.rCustomersPreviewMouseMove(Sender: TObject;
  PreviewUserData: TprPreviewUserData; var cur: TCursor;
  var HighlightObject: Boolean);
begin
if PreviewUserData<>nil then
  begin
    cur := crHandPoint;
    HighlightObject := true;
  end;
end;

procedure THWForm.rCustomersPreviewMouseDown(Sender: TObject;
  PreviewUserData: TprPreviewUserData; Shift : TShiftState);
begin
if (PreviewUserData<>nil) and (ssLeft in Shift) then
  begin
    if Customers.Locate('CustNo',PreviewUserData.Tag,[]) then
      begin
        Query1.Close;
        Query1.Params[0].AsInteger := PreviewUserData.Tag;
        Query1.Open;
        rOrders.PrepareReport;
        rOrders.PreviewPreparedReport(false);
      end;
  end;
end;

procedure THWForm.rOrdersPreviewGetUserData(Sender: TObject; Obj: TprObj;
  ObjRec: TprObjRec; var PreviewUserData: TprPreviewUserData);
begin
if Obj.Band.BandType=bthDetail then
  begin
    PreviewUserData := TprPreviewUserData.Create;
    PreviewUserData.Tag := Query1.FieldByName('OrderNo').AsInteger;
  end;
end;

procedure THWForm.rOrdersPreviewMouseMove(Sender: TObject;
  PreviewUserData: TprPreviewUserData; var cur: TCursor;
  var HighlightObject: Boolean);
begin
if PreviewUserData<>nil then
  begin
    cur := crHandPoint;
    HighlightObject := true;
  end;
end;

procedure THWForm.rTxCustomersPreviewGetUserData(Sender: TObject;
  Obj: TprObj; ObjRec: TprObjRec; var PreviewUserData: TprPreviewUserData);
begin
if Obj.Name='prTxMemoObj2' then
  begin
    PreviewUserData := TprPreviewUserData.Create;
    PreviewUserData.Tag := Customers.FieldByName('CustNo').AsInteger;
  end;
end;

procedure THWForm.rTxCustomersPreviewMouseDown(Sender: TObject;
  PreviewUserData: TprPreviewUserData; Shift : TShiftState);
begin
if PreviewUserData<>nil then
  begin
    if Customers.Locate('CustNo',PreviewUserData.Tag,[]) then
      begin
        Query1.Close;
        Query1.Params[0].AsInteger := PreviewUserData.Tag;
        Query1.Open;
        rTxOrders.PrepareReport;
        rTxOrders.PreviewPreparedReport(false);
      end;
  end;
end;

procedure THWForm.rTxOrdersPreviewGetUserData(Sender: TObject; Obj: TprObj;
  ObjRec: TprObjRec; var PreviewUserData: TprPreviewUserData);
begin
if Obj.Band.BandType=bthDetail then
  begin
    PreviewUserData := TprPreviewUserData.Create;
    PreviewUserData.Tag := Query1.FieldByName('OrderNo').AsInteger;
  end;
end;

procedure THWForm.Button2Click(Sender: TObject);
begin
rTxCustomers.PrepareReport;
rTxCustomers.PreviewPreparedReport(false);
end;

procedure THWForm.rTxOrdersPreviewDblClick(Sender: TObject;
  PreviewUserData: TprPreviewUserData);
begin
if PreviewUserData<>nil then
  begin
    if Query1.Locate('OrderNo',PreviewUserData.Tag,[]) then
      if Customers.Locate('CustNo',Query1.FieldByName('CustNo').AsInteger,[]) then
        begin
          Query2.Close;
          Query2.Params[0].AsInteger := PreviewUserData.Tag;
          Query2.Open;
          rTxOrder.PrepareReport;
          rTxOrder.PreviewPreparedReport(false);
        end;
  end;
end;

procedure THWForm.rOrdersPreviewDblClick(Sender: TObject;
  PreviewUserData: TprPreviewUserData);
begin
if PreviewUserData<>nil then
  begin
    if Query1.Locate('OrderNo',PreviewUserData.Tag,[]) then
      if Customers.Locate('CustNo',Query1.FieldByName('CustNo').AsInteger,[]) then
        begin
          Query2.Close;
          Query2.Params[0].AsInteger := PreviewUserData.Tag;
          Query2.Open;
          rOrder.PrepareReport;
          rOrder.PreviewPreparedReport(false);
        end;
  end;
end;

end.
