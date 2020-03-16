unit Main;

interface

{$I pr.inc}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  pr_Common, pr_Classes, ExtCtrls, pr_CommonDesignerPanel, pr_DesignerPanel,
  StdCtrls, Buttons, ComCtrls, Grids, DBGrids, Db, DBTables, ShellApi,
  pr_CommonPreviewPanel, pr_PreviewPanel, pr_DesignerFunctions;

type
  TFormMain = class(TForm)
    prReport: TprReport;
    Panel1: TPanel;
    PageControl: TPageControl;
    TSAbout: TTabSheet;
    RichEdit: TRichEdit;
    TSData: TTabSheet;
    DataSet: TTable;
    DataSource: TDataSource;
    DBGrid1: TDBGrid;
    Panel2: TPanel;
    Panel3: TPanel;
    TSDesign: TTabSheet;
    Panel6: TPanel;
    prDesignerPanel: TprDesignerPanel;
    Panel8: TPanel;
    Label4: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    DragList: TListView;
    TSPreview: TTabSheet;
    prPreviewPanel: TprPreviewPanel;
    Panel7: TPanel;
    Banner: TImage;
    ScrollBox1: TScrollBox;
    SBPreview: TSpeedButton;
    SBDesign: TSpeedButton;
    SBData: TSpeedButton;
    SBAbout: TSpeedButton;
    Memo1: TMemo;
    Panel4: TPanel;
    MRepVars: TMemo;
    Panel9: TPanel;
    Label1: TLabel;
    Panel5: TPanel;
    Panel10: TPanel;
    Label2: TLabel;
    MVars: TMemo;
    procedure SBAboutClick(Sender: TObject);
    procedure SBDataClick(Sender: TObject);
    procedure SBDesignClick(Sender: TObject);
    procedure prDesignerPanelDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure prDesignerPanelDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure prReportUnknownVariable(Sender: TObject;
      const VarName: String; var Value: TprVarValue;
      var IsProcessed: Boolean);
    procedure SBPreviewClick(Sender: TObject);
    procedure BannerClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Page : TprCustomPage;
    TitleBand : TprCustomHTitleBand;
    DetailBand : TprCustomHDetailBand;
    procedure FillReportVars;
    procedure PrepareDragList;
    procedure PrepareTemplate;
    procedure TemplateAddObj;
    function DragSourceIsDataField(const Source : TObject) : boolean;
    function DragSourceIsBLOBField(const Source : TObject) : boolean;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

procedure TFormMain.SBAboutClick(Sender: TObject);
begin
  PageControl.ActivePage := TSAbout;
end;

procedure TFormMain.SBDataClick(Sender: TObject);
begin
  PageControl.ActivePage := TSData;
end;

procedure TFormMain.SBDesignClick(Sender: TObject);
begin
  PrepareDragList;
  if Page = nil then
    PrepareTemplate;
  prDesignerPanel.UpdateCurPage;
  PageControl.ActivePage := TSDesign;
  SBPreview.Enabled := true;
end;

procedure TFormMain.PrepareTemplate;
begin
  Page := prReport.AddPage;
  prDesignerPanel.ActivePageIndex := 0;
  TitleBand := TprHTitleBand(Page.InsertBand(bthTitle));
  TitleBand.Height := 76;
  DetailBand := TprHDetailBand(Page.InsertBand(bthDetail));
  DetailBand.DataSetName := DataSet.Name;
  DetailBand.Height := 40;
  DetailBand.ResizeMode := prbrmMaxObj;
  TemplateAddObj;
end;

procedure TFormMain.TemplateAddObj;
var
  m : TprMemoObj;
  img : TprImageObj;
begin
  m := TprMemoObj.Create(prReport.prOwner);
  with m,TprMemoObjRecVersion(m.dRec.Versions[0]) do
  begin
    Band := TitleBand;
    Memo.Add('Bio Life REPORT');
    Memo.Add('Traditional Inprise Fishes');
    lBorder.Show := false;
    rBorder.Show := false;
    tBorder.Show := false;
    bBorder.Color := clNavy;
    hAlign := prhCenter;
    vAlign := prvCenter;
    Font.Color := clMaroon;
    Font.Size := 18;
    dRec.Left := 110;
    dRec.Top := 10;
    dRec.Right := 607;
    dRec.Bottom := 67;
  end;
  m := TprMemoObj.Create(prReport.prOwner);
  with m,TprMemoObjRecVersion(m.dRec.Versions[0]) do
  begin
    Band := DetailBand;
    Memo.Add('[DataSet.Common_Name]');
    bBorder.Show := false;
    rBorder.Show := false;
    tBorder.Color := clNavy;
    lBorder.Color := clNavy;
    hAlign := prhCenter;
    vAlign := prvCenter;
    Font.Size := 12;
    dRec.Left := 56;
    dRec.Top := 8;
    dRec.Right := 266;
    dRec.Bottom := 32;
  end;
  m := TprMemoObj.Create(prReport.prOwner);
  with m,TprMemoObjRecVersion(m.dRec.Versions[0]) do
  begin
    Band := DetailBand;
    Memo.Add('[DataSet.Notes]');
    bBorder.Show := false;
    rBorder.Show := false;
    tBorder.Color := clNavy;
    lBorder.Color := clNavy;
    hAlign := prhLeft;
    vAlign := prvCenter;
    Font.Size := 8;
    dRec.Left := 286;
    dRec.Top := 8;
    dRec.Right := 466;
    dRec.Bottom := 32;
    CanResizeX := False;
    CanResizeY := True;
    WordWrap := True;
    FillColor := clSilver
  end;
  img := TprImageObj.Create(prReport.prOwner);
  img.Band := DetailBand;
  with img,TprImageObjRecVersion(img.dRec.Versions[0]) do
  begin
    ImageSource := isDBFieldName;
    DBFieldName := 'DataSet.Graphic';
    dRec.Left := 476 ;
    dRec.Top :=  4 ;
    dRec.Right := img.dRec.Left + 50;
    dRec.Bottom := img.dRec.Top + 50;
    DrawMode := prdmResizeHeightWidth;
  end;
end;

procedure TFormMain.PrepareDragList;
var
  i : integer;
begin
  DragList.Items.Clear;
  for i := 0 to DataSet.Fields.Count - 1 do
    DragList.Items.Add.Caption := (Format('%s.%s',[DataSet.Name,DataSet.Fields[i].FullName]));
  for i := 0 to MVars.Lines.Count-1 do
    DragList.Items.Add.Caption := (MVars.Lines.Names[i]);
  for i := 0 to MRepVars.Lines.Count-1 do
    DragList.Items.Add.Caption := (MRepVars.Lines.Names[i]);
end;

procedure TFormMain.prDesignerPanelDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  dc : TprDesignComponent;
  PointInfo : TprPointInfo;
  ResizeMode : TprResizeType;
  LinkMode : TprLinkType;
begin
  prDesignerPanel.GetPointInfoAt(x,y,dc,PointInfo,ResizeMode,LinkMode);
  Accept := ((dc <> nil) and ((dc.ClassNameIs('TprHDetailBand')) or (dc.ClassNameIs('TprHTitleBand') and not DragSourceIsDataField(Source))));
end;

function TFormMain.DragSourceIsDataField(const Source : TObject) : boolean;
begin
  Result := (Source = DragList) and (AnsiCompareText(Copy(TListView(Source).Selected.Caption,1,8),DataSet.Name+'.') = 0) and (DataSet.FindField(Copy(TListView(Source).Selected.Caption,9,20)) <> nil)
end;

function TFormMain.DragSourceIsBLOBField(const Source : TObject) : boolean;
begin
  Result := (Source = DragList) and (AnsiCompareText(Copy(TListView(Source).Selected.Caption,1,8),DataSet.Name+'.') = 0) and (DataSet.FindField(Copy(TListView(Source).Selected.Caption,9,20)) <> nil) and
    (DataSet.FieldByName(Copy(TListView(Source).Selected.Caption,9,20)).DataType = ftGraphic);
end;

procedure TFormMain.prDesignerPanelDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  m : TprMemoObj;
  img : TprImageObj;
  dc : TprDesignComponent;
  PointInfo : TprPointInfo;
  ResizeMode : TprResizeType;
  LinkMode : TprLinkType;
  P : TPoint;
begin
  P := Point(x,y);
  prDesignerPanel.GetPointInfoAt(x,y,dc,PointInfo,ResizeMode,LinkMode);
  if (Dc = nil) or not ((dc is TprHDetailBand) or (dc is TprHTitleBand)) then Exit;

  if (Source = DragList) then
  begin
    if DragSourceIsBLOBField(Source) then
    begin
      img := TprImageObj.Create(prReport.prOwner);
      img.Band := TprBand(Dc);
      with TprImageObjRecVersion(img.dRec.Versions[0]) do
      begin
        ImageSource := isDBFieldName;
        DBFieldName := TListView(Source).Selected.Caption;
        // use ClientToBandPoint method
        img.dRec.Left := prDesignerPanel.ClientToBandPoint(img.Band,P).x;
        img.dRec.Top :=  prDesignerPanel.ClientToBandPoint(img.Band,P).y;

        img.dRec.Right := img.dRec.Left + 50;
        img.dRec.Bottom := img.dRec.Top + 50;
        DrawMode := prdmResizeHeightWidth;
      end;
    end
    else
    begin
      m := TprMemoObj.Create(prReport.prOwner);
      m.Band := TprBand(Dc);
      with TprMemoObjRecVersion(m.dRec.Versions[0]) do
      begin
        if DragSourceIsDataField(Source) then
          Memo.Add(Format('[%s.GetFieldValue("%s")]',[DataSet.Name, Copy(TListView(Source).Selected.Caption,9,20)]))
        else
          Memo.Add(Format('[%s]',[TListView(Source).Selected.Caption]));
        hAlign := prhLeft;
        vAlign := prvCenter;
        Font.Style := Font.Style+[fsBold];
        FillColor := clSilver;
        lBorder.Show := false;
        tBorder.Show := false;
        rBorder.Show := false;
        bBorder.Show := false;

        m.dRec.Left := prDesignerPanel.ClientToBandPoint(m.Band,P).x;
        m.dRec.Top :=  prDesignerPanel.ClientToBandPoint(m.Band,P).y;

        m.dRec.Right := m.dRec.Left + 150;
        m.dRec.Bottom := m.dRec.Top + 24;
        prDesignerPanel.SelectObject(m);
        prDesignerPanel.AlignAction(aacAlignToGridAll);
        CanResizeX := False;
        CanResizeY := True;
        WordWrap := True;
      end;
    end;
  end;
  prDesignerPanel.UpdateCurPage;
end;

procedure TFormMain.prReportUnknownVariable(Sender: TObject;
  const VarName: String; var Value: TprVarValue; var IsProcessed: Boolean);
begin
  Value.vType := prvvtString;
  Value.vString := MVars.Lines.Values[VarName];
  IsProcessed := true;
end;

procedure TFormMain.SBPreviewClick(Sender: TObject);
begin
  prReport.PrepareReport;
  prPreviewPanel.Report := nil;
  prPreviewPanel.Report := prReport;
  PageControl.ActivePage := TSPreview;
end;

procedure TFormMain.FillReportVars;
var
  i : integer;
begin
  prReport.Variables.Clear;
  with prReport.Variables.AddVariable do
  begin
    Name :=  'varString';
    AsString := 'Custom string';
  end;
  with prReport.Variables.AddVariable do
  begin
    Name :=  'varInteger';
    AsInteger := 231134;
  end;
  with prReport.Variables.AddVariable do
  begin
    Name :=  'varDouble';
    AsDouble := Now;
  end;
  with prReport.Variables.AddVariable do
  begin
    Name :=  'varDateTime';
    AsDateTime := Now;
  end;
  with prReport.Variables.AddVariable do
  begin
    Name :=  'varVariant';
    AsVariant := 'vtkTools';
  end;
  with prReport.Variables.AddVariable do
  begin
    Name :=  'varFormula';
    Formula := 'DataSet.Length_in * 2.54';
  end;
  MRepVars.Lines.Clear;
  for i := 0 to prReport.Variables.Count-1 do
    if prReport.Variables[i].Calculated then
      MRepVars.Lines.Add(Format('%s=%s',[prReport.Variables[i].Name,prReport.Variables[i].Formula]))
    else
      MRepVars.Lines.Add(Format('%s=%s',[prReport.Variables[i].Name,prReport.Variables[i].AsString]));

end;

procedure TFormMain.BannerClick(Sender: TObject);
begin
  ShellExecute(0,PChar('open'),PChar('http://www.vtktools.ru'),nil,nil,SW_SHOWNORMAL);
end;

procedure TFormMain.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = TSAbout then
    SBAbout.Click
  else
    if PageControl.ActivePage = TSData then
      SBData.Click
    else
      if PageControl.ActivePage = TSDesign then
        SBDesign.Click
      else
        if PageControl.ActivePage = TSPreview then
          SBPreview.Click;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FillReportVars;
end;

end.
