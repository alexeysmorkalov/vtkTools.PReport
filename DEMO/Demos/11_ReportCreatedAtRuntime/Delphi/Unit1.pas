unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, StdCtrls, Buttons, ExtCtrls, math,

  pr_Common, pr_Classes, pr_Designer;

type
  TForm1 = class(TForm)
    prReport: TprReport;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    CBHeaderDateTime: TCheckBox;
    CBHeader: TCheckBox;
    CBHeaderCount: TCheckBox;
    Label1: TLabel;
    EDTitle: TEdit;
    CBDetail: TCheckBox;
    Label3: TLabel;
    CBAutoDetailHeight: TCheckBox;
    Bevel2: TBevel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    CBGrouping: TCheckBox;
    Label4: TLabel;
    EDGroupBy: TComboBox;
    Label5: TLabel;
    Bevel3: TBevel;
    CBAggCount: TCheckBox;
    CBAggMax: TCheckBox;
    SpeedButton3: TSpeedButton;
    CBShowFooter: TCheckBox;
    CBShowHeader: TCheckBox;
    SpeedButton4: TSpeedButton;
    bGenerate: TButton;
    bDesign: TButton;
    bPreview: TButton;
    fdTitleFont: TFontDialog;
    fdDetailFont: TFontDialog;
    fdGroupHeaderFont: TFontDialog;
    fdGroupFooterFont: TFontDialog;
    Customer: TQuery;
    procedure FormCreate(Sender: TObject);
    procedure bGenerateClick(Sender: TObject);
    procedure bDesignClick(Sender: TObject);
    procedure CBHeaderClick(Sender: TObject);
    procedure CBDetailClick(Sender: TObject);
    procedure CBGroupingClick(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure bPreviewClick(Sender: TObject);
  private
    { Private declarations }
    function CalcMemoHeight(Memo : TStrings; Font : TFont) : integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

function TForm1.CalcMemoHeight(Memo : TStrings; Font : TFont) : integer;
var
  i : integer;
begin
Canvas.Font.Assign(Font);
Result := 2;
for i:=0 to Memo.Count-1 do
  if Memo[i]='' then
    Result := Result+Canvas.TextHeight('Wg')
  else
    Result := Result+Canvas.TextHeight(Memo[i]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
EDGroupBy.ItemIndex := 0;
end;

procedure TForm1.bGenerateClick(Sender: TObject);
var
  p : TprPage;
  m : TprMemoObj;
  v : TprValue;
  o : integer;
  b : TprBand;
  g : TprGroup;
  bDetail : TprHDetailBand;

  function AddDetailField(const Text : string; Left,Top,Width : integer) : TprMemoObj;
  var
    morv : TprMemoObjRecVersion;
  begin
  Result := TprMemoObj.Create(prReport.prOwner);
  Result.Band := bDetail;
  morv := TprMemoObjRecVersion(Result.dRec.Versions[0]);
  morv.Memo.Text := Text;
  morv.Font.Assign(fdDetailFont.Font);
  Result.dRec.pRect := Rect(Left,Top,Left+Width,Top+CalcMemoHeight(morv.Memo,morv.Font));
  morv.CanResizeY := CBAutoDetailHeight.Checked;
  morv.WordWrap := CBAutoDetailHeight.Checked;
  morv.lBorder.Show := false;
  morv.tBorder.Show := false;
  morv.rBorder.Show := false;
  morv.bBorder.Show := false;
  end;

begin
prReport.ClearTemplate;
prReport.PrinterName := prGetDefaultPrinterName; // windows default printer
prReport.Title := EDTitle.Text;

p := TprPage.Create(prReport.prOwner);
p.Report := prReport;

if CBHeader.Checked then
  begin
    // generate report header band
    o := 0;
    b := TprHTitleBand.Create(prReport.prOwner);
    with TprHTitleBand(b) do
      begin
        Page := p;
        Height := 64;
      end;
    p.UpdateBandsPageRect; // !!! calculate place of added band on page
    
    if CBHeaderDateTime.Checked then
      begin
        m := TprMemoObj.Create(prReport.prOwner);
        m.Band := b;
        with TprMemoObjRecVersion(m.dRec.Versions[0]) do
          begin
            Memo.Add('[StartDateTime]');
            Font.Size := 6;
            Font.Color := clGray;
            lBorder.Show := false;
            tBorder.Show := false;
            rBorder.Show := false;
            bBorder.Show := false;
            m.dRec.pRect := Rect(4,4,100,4+CalcMemoHeight(Memo,Font));
            o := m.dRec.pRect.Bottom+2;
          end;
      end;
    if CBHeaderCount.Checked then
      begin
        // Create report aggregate variable
        v := TprValue(prReport.Values.Add);
        v.Name := 'aggCustomersCount';
        v.CalcOn := cvtDatasetNext;
        v.DataSetName := Customer.Name;
        v.ResetOn := rvtReport;
        v.AggFunction := prafCount;
        // create memo obj
        m := TprMemoObj.Create(prReport.prOwner);
        m.Band := b;
        with TprMemoObjRecVersion(m.dRec.Versions[0]) do
          begin
            Memo.Add('Count of customers: [aggCustomersCount]');
            hAlign := prhCenter;
            Font.Size := 8;
            Font.Style := [fsBold];
            FillColor := clSilver;
            lBorder.Show := false;
            tBorder.Show := false;
            rBorder.Show := false;
            bBorder.Show := false;
            m.dRec.pRect := Rect(b.dPageRect.Right-b.dPageRect.Left-200,4,b.dPageRect.Right-b.dPageRect.Left-4,4+CalcMemoHeight(Memo,Font));
            o := Max(o,m.dRec.pRect.Bottom+2);
          end;
      end;

    // title memo obj
    m := TprMemoObj.Create(prReport.prOwner);
    m.Band := b;
    with TprMemoObjRecVersion(m.dRec.Versions[0]) do
      begin
        Memo.Add(EDTitle.Text);
        hAlign := prhCenter;
        vAlign := prvCenter;
        Font.Assign(fdTitleFont.Font);
        FillColor := clNavy;
        lBorder.Show := false;
        tBorder.Show := false;
        rBorder.Show := false;
        bBorder.Show := false;
        m.dRec.pRect := Rect(4,o,b.dPageRect.Right-b.dPageRect.Left-4,o+4+CalcMemoHeight(Memo,Font));
      end;
  end;

if CBDetail.Checked then
  begin
    bDetail := TprHDetailBand.Create(prReport.prOwner);
    bDetail.Name := 'Name';
    bDetail.DataSetName := Customer.Name;
    bDetail.Page := p;
    bDetail.Height := 40;
    p.UpdateBandsPageRect; // !!! calculate place of added band on page

    AddDetailField(Format('[%s.CustNo]',[Customer.Name]),4,0,50);
    AddDetailField(Format('[%s.Company]',[Customer.Name]),58,0,150);
    if CBAutoDetailHeight.Checked then
      begin
        bDetail.ResizeMode := prbrmMaxObj;
        AddDetailField(Format('[%s.Addr1+%s.Addr2]',[Customer.Name,Customer.Name]),212,0,200);
      end
    else
      AddDetailField(Format('[%s.Addr1]'#13#10'[%s.Addr2]',[Customer.Name,Customer.Name]),212,0,200);
    AddDetailField(Format('[%s.City]',[Customer.Name]),416,0,100);
    m := AddDetailField(Format('Phone: [%s.Phone]'#13#10'Fax: [%s.Fax]',[Customer.Name,Customer.Name]),520,0,100);
    AddDetailField(Format('[%s.Contact]',[Customer.Name]),624,0,100);
    bDetail.Height := m.dRec.pRect.Bottom+4;
  end;

if CBGrouping.Checked and (bDetail<>nil) then
  begin
    g := TprGroup.Create(prReport.prOwner);
    g.Report := prReport;
    g.Name := 'Group';
    g.DetailBand := bDetail;
    case EDGroupBy.ItemIndex of
      0: g.Valid := Format('Copy(%s.Company,1,1)',[Customer.Name]);
      1: g.Valid := Customer.Name+'.Country';
      2: g.Valid := Customer.Name+'.State';
    end;

    if CBShowHeader.Checked then
      begin
        b := TprHGroupHeaderBand.Create(prReport.prOwner);
        b.Page := p;
        b.Name := 'GroupHeader';
        TprHGroupHeaderBand(b).Group := g;
        TprHGroupHeaderBand(b).Height := 20;
        p.UpdateBandsPageRect; // !!! calculate place of added band on page

        m := TprMemoObj.Create(prReport.prOwner);
        m.Band := b;
        with TprMemoObjRecVersion(m.dRec.Versions[0]) do
          begin
            Memo.Add('Group header, group value:');
            hAlign := prhRight;
            vAlign := prvCenter;
            Font.Assign(fdGroupHeaderFont.Font);
            Font.Style := [];
            FillColor := clSilver;
            lBorder.Show := false;
            tBorder.Show := false;
            rBorder.Show := false;
            bBorder.Show := false;
            m.dRec.pRect := Rect(4,4,304,4+4+CalcMemoHeight(Memo,Font));
          end;
        m := TprMemoObj.Create(prReport.prOwner);
        m.Band := b;
        with TprMemoObjRecVersion(m.dRec.Versions[0]) do
          begin
            Memo.Add(Format('[%s]',[g.Valid]));
            hAlign := prhLeft;
            vAlign := prvCenter;
            Font.Assign(fdGroupHeaderFont.Font);
            Font.Style := Font.Style+[fsBold];
            FillColor := clSilver;
            lBorder.Show := false;
            tBorder.Show := false;
            rBorder.Show := false;
            bBorder.Show := false;
            m.dRec.pRect := Rect(312,4,612,4+4+CalcMemoHeight(Memo,Font));
          end;
        TprHGroupHeaderBand(b).Height := m.dRec.pRect.Bottom+4;
        p.UpdateBandsPageRect; // !!! calculate place of added band on page
      end;

    if CBShowFooter.Checked then
      begin
        b := TprHGroupFooterBand.Create(prReport.prOwner);
        b.Page := p;
        b.Name := 'GroupFooter';
        TprHGroupFooterBand(b).Group := g;
        TprHGroupFooterBand(b).Height := 20;
        p.UpdateBandsPageRect; // !!! calculate place of added band on page

        m := nil;
        if CBAggCount.Checked then
          begin
            v := TprValue(prReport.Values.Add);
            v.Name := 'aggGroupCount';
            v.CalcOn := cvtDatasetNext;
            v.DataSetName := Customer.Name;
            v.ResetOn := rvtGroup;
            v.Group := g;
            v.AggFunction := prafCount;

            m := TprMemoObj.Create(prReport.prOwner);
            m.Band := b;
            with TprMemoObjRecVersion(m.dRec.Versions[0]) do
              begin
                Memo.Add('Count: [aggGroupCount]');
                hAlign := prhLeft;
                vAlign := prvCenter;
                Font.Assign(fdGroupFooterFont.Font);
                tBorder.Show := false;
                rBorder.Show := false;
                m.dRec.pRect := Rect(4,4,304,4+4+CalcMemoHeight(Memo,Font));
              end;
          end;

        if CBAggMax.Checked then
          begin
            v := TprValue(prReport.Values.Add);
            v.Name := 'aggGroupMax';
            v.CalcOn := cvtDatasetNext;
            v.DataSetName := Customer.Name;
            v.ResetOn := rvtGroup;
            v.Group := g;
            v.AggFunction := prafMax;
            v.Formula := Customer.Name+'.CustNo';

            m := TprMemoObj.Create(prReport.prOwner);
            m.Band := b;
            with TprMemoObjRecVersion(m.dRec.Versions[0]) do
              begin
                Memo.Add('Max CustNo: [aggGroupMax]');
                hAlign := prhRight;
                vAlign := prvCenter;
                Font.Assign(fdGroupFooterFont.Font);
                lBorder.Show := false;
                tBorder.Show := false;
                m.dRec.pRect := Rect(b.dPageRect.Right-b.dPageRect.Left-304,4,b.dPageRect.Right-b.dPageRect.Left-4,4+4+CalcMemoHeight(Memo,Font));
              end;
          end;
        if m<>nil then
          begin
            TprHGroupHeaderBand(b).Height := m.dRec.pRect.Bottom+4;
            p.UpdateBandsPageRect; // !!! calculate place of added band on page
          end;
      end;
  end;

// initialization DataSet - Customer
Customer.Close;
if CBGrouping.Checked then
  begin
    case EDGroupBy.ItemIndex of
      0: Customer.SQL.Text := 'select * from customer order by company';
      1: Customer.SQL.Text := 'select * from customer order by city';
      2: Customer.SQL.Text := 'select * from customer order by state';
    end;
  end
else
  begin
    Customer.SQL.Text := 'select * from customer';
  end;
ShowMessage('The template of the report is successfully generated');
end;

procedure TForm1.bDesignClick(Sender: TObject);
begin
prReport.DesignReport(true);
end;

procedure TForm1.CBHeaderClick(Sender: TObject);
begin
CBHeaderDateTime.Enabled := CBHeader.Checked;
CBHeaderCount.Enabled := CBHeader.Checked;
EDTitle.Enabled := CBHeader.Checked;
SpeedButton2.Enabled := CBHeader.Checked;
end;

procedure TForm1.CBDetailClick(Sender: TObject);
begin
CBAutoDetailHeight.Enabled := CBDetail.Checked;
SpeedButton1.Enabled := CBDetail.Checked;
CBGrouping.Checked := CBDetail.Checked;
CBGrouping.Enabled := CBDetail.Checked;
CBGroupingClick(nil);
end;

procedure TForm1.CBGroupingClick(Sender: TObject);
begin
EDGroupBy.Enabled := CBGrouping.Checked;
CBShowHeader.Enabled := CBGrouping.Checked;
CBShowFooter.Enabled := CBGrouping.Checked;
SpeedButton3.Enabled := CBGrouping.Checked;
SpeedButton4.Enabled := CBGrouping.Checked;
CBAggCount.Enabled := CBGrouping.Checked;
CBAggMax.Enabled := CBGrouping.Checked;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
fdTitleFont.Execute;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
fdDetailFont.Execute;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
fdGroupHeaderFont.Execute;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
fdGroupFooterFont.Execute;
end;

procedure TForm1.bPreviewClick(Sender: TObject);
begin
if prReport.PrepareReport then
  prReport.PreviewPreparedReport(true);
end;

end.
