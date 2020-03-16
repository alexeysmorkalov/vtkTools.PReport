{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxExportParams;

interface

{$I PR.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons,

  pr_Common, pr_txclasses, pr_MultiLang;

type
  TprTxExportParamsForm = class(TprForm)
    Label1: TLabel;
    EDExportFileName: TEdit;
    bFileName: TSpeedButton;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    RBAll: TRadioButton;
    RBPagesRange: TRadioButton;
    RBPagesList: TRadioButton;
    EDExportPages: TEdit;
    EDExportFromPage: TEdit;
    EDExportToPage: TEdit;
    RBLinesRange: TRadioButton;
    EDExportFromLine: TEdit;
    EDExportToLine: TEdit;
    Label3: TLabel;
    EDExportESCModelName: TComboBox;
    bOK: TButton;
    bCancel: TButton;
    SaveDialog: TSaveDialog;
    prMLRes1: TprMLRes;
    EDExportCodePage: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    EDExportFilter: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure bFileNameClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure RBPagesRangeClick(Sender: TObject);
    procedure RBPagesListClick(Sender: TObject);
    procedure RBLinesRangeClick(Sender: TObject);
    procedure EDExportFromPageChange(Sender: TObject);
    procedure EDExportPagesChange(Sender: TObject);
    procedure EDExportFromLineChange(Sender: TObject);
    procedure EDExportFilterChange(Sender: TObject);
  private
    { Private declarations }
    PagesCount : integer;
  public
    { Public declarations }
    Report : TprTxReport;
    function EditParams(_Report : TprTxReport) : boolean;
  end;

implementation

uses
  pr_Strings, pr_Utils, pr_TxUtils, pr_TxConsts;
  
{$R *.DFM}

procedure TprTxExportParamsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

procedure TprTxExportParamsForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
LoadResImage(bFileName.Glyph,'OPEN');
EDExportESCModelName.Items.Add(prLoadStr(sExportNotUseESCModels));
for i:=0 to TxReportOptions.ESCModelsCount-1 do
  EDExportESCModelName.Items.Add(TxReportOptions.ESCModels[i].ModelName);
for i:=integer(Low(TprTxCodePage)) to integer(High(TprTxCodePage)) do
  EDExportCodePage.Items.Add(prLoadStr(sTxCodePageTitlesOffset-i));
end;

function TprTxExportParamsForm.EditParams;
var
  i : integer;
  s : string;
begin
Report := _Report;

s := '*.*|*.*';
for i:=0 to High(prExportFiltersInfos) do
  if Report is prExportFiltersInfos[i].ReportRef then
    begin
      s := s+Format('|%s (*.%s)|*.%s',[prExportFiltersInfos[i].ExportFilterDesc,prExportFiltersInfos[i].ExportFilterExtention,prExportFiltersInfos[i].ExportFilterExtention]);
      EDExportFilter.Items.AddObject(prExportFiltersInfos[i].ExportFilterDesc,@prExportFiltersInfos[i]);
      if AnsiCompareText(Report.ExportFilter,prExportFiltersInfos[i].ExportFilterClassRef.ClassName)=0 then
        EDExportFilter.ItemIndex := EDExportFilter.Items.Count-1;
    end;
if (EDExportFilter.ItemIndex=-1) and (EDExportFilter.Items.Count>0) then
  EDExportFilter.ItemIndex := 0;
SaveDialog.Filter := s;

PagesCount := TxReportOptions.ParsePages(Report.TextDevice.SList,false,0,nil);
RBPagesRange.Enabled := PagesCount>1;
Label2.Enabled := PagesCount>1;
RBPagesList.Enabled := PagesCount>1;
EDExportPages.Enabled := PagesCount>1;
EDExportFromPage.Enabled := PagesCount>1;
EDExportToPage.Enabled := PagesCount>1;
EDExportFromLine.Enabled := Report.TextDevice.SList.Count>1;
EDExportToLine.Enabled := Report.TextDevice.SList.Count>1;
RBLinesRange.Enabled := Report.TextDevice.SList.Count>1;
Label5.Enabled := Report.TextDevice.SList.Count>1;

if Report.ExportFromPage>0 then
  EDExportFromPage.Text := IntToStr(Report.ExportFromPage);
if Report.ExportToPage>0 then
  EDExportToPage.Text := IntToStr(Report.ExportToPage);
if Report.ExportFromLine>0 then
  EDExportFromLine.Text := IntToStr(Report.ExportFromLine);
if Report.ExportToLine>0 then
  EDExportToLine.Text := IntToStr(Report.ExportToLine);
EDExportPages.Text := Report.ExportPages;

EDExportFileName.Text := Report.ExportFileName;
RBAll.Checked := (Report.ExportPagesMode=ppmAll) or (PagesCount=1);
RBPagesRange.Checked := (Report.ExportPagesMode=ppmPagesRange) and (PagesCount>1);
RBPagesList.Checked := (Report.ExportPagesMode=ppmPagesList) and (PagesCount>1);
RBLinesRange.Checked := (prtxeoLinesRange in Report.ExportTxOptions) and (Report.TextDevice.SList.Count>1);

EDExportCodePage.ItemIndex := integer(Report.ExportCodePage);
if (prtxeoUseESCModel in Report.ExportTxOptions) and (TxReportOptions.ESCModelIndexByModelName(Report.ExportESCModelName)<>-1) then
  EDExportESCModelName.ItemIndex := TxReportOptions.ESCModelIndexByModelName(Report.ExportESCModelName)+1
else
  EDExportESCModelName.ItemIndex := 0;
Result := ShowModal=mrOk;
end;

procedure TprTxExportParamsForm.bFileNameClick(Sender: TObject);
var
  i : integer;
  ext : string;
begin
SaveDialog.FileName := EDExportFileName.Text;
if EDExportFilter.ItemIndex>=0 then
  SaveDialog.FilterIndex := 2+EDExportFilter.ItemIndex;
if SaveDialog.Execute then
  begin
    ext := ExtractFileExt(SaveDialog.FileName);
    if ext='' then
      begin
        if SaveDialog.FilterIndex>=2 then
          begin
            EDExportFilter.ItemIndex := SaveDialog.FilterIndex-2;
            EDExportFileName.Text := SaveDialog.FileName+'.'+PprExportFilterRegInfo(EDExportFilter.Items.Objects[SaveDialog.FilterIndex-2]).ExportFilterExtention
          end
        else
          EDExportFileName.Text := SaveDialog.FileName;
      end
    else
      begin
        EDExportFileName.Text := SaveDialog.FileName;
        Delete(ext,1,1);
        i := 0;
        while (i<EDExportFilter.Items.Count) and (AnsiCompareText(PprExportFilterRegInfo(EDExportFilter.Items.Objects[i]).ExportFilterExtention,ext)<>0) do Inc(i);
        if i<EDExportFilter.Items.Count then
          EDExportFilter.ItemIndex := i;
      end;
  end;
end;

procedure TprTxExportParamsForm.bOKClick(Sender: TObject);
var
  pl : TList;
  FromPage,ToPage,FromLine,ToLine,valCode : integer;
begin
FromPage := 0;
ToPage := 0;
FromLine := 0;
ToLine := 0;
if EDExportFilter.ItemIndex=-1 then
  begin
    ActiveControl := EDExportFilter;
    MBError(prLoadStr(sExportFileNotDefined));
    exit;
  end;
if Trim(EDExportFileName.Text)='' then
  begin
    ActiveControl := EDExportFileName;
    MBError(prLoadStr(sExportFileNotDefined));
    exit;
  end;
if RBPagesRange.Checked then
  begin
    val(EDExportFromPage.Text,FromPage,valCode);
    if (valCode<>0) or (FromPage<1) or (FromPage>PagesCount) then
      begin
        ActiveControl := EDExportFromPage;
        MBError(prLoadStr(sExportPagesRangeNotValid));
        exit;
      end;
    val(EDExportToPage.Text,ToPage,valCode);
    if (valCode<>0) or (ToPage<1) or (ToPage>PagesCount) then
      begin
        ActiveControl := EDExportToPage;
        MBError(prLoadStr(sExportPagesRangeNotValid));
        exit;
      end;
    if FromPage>ToPage then
      begin
        ActiveControl := EDExportFromPage;
        MBError(prLoadStr(sExportPagesRangeNotValid));
        exit;
      end
  end
else
  if RBPagesList.Checked then
    begin
      pl := TList.Create;
      try
        TextToPageList(EDExportPages.Text,pl);
        if (pl.Count<1) or (integer(pl[0])<1) or (integer(pl[0])>PagesCount) or (integer(pl[pl.Count-1])<1) or (integer(pl[pl.Count-1])>PagesCount) then
          begin
            ActiveControl := EDExportPages;
            MBError(prLoadStr(sExportPagesListNotValid));
            exit;
          end;
      finally
        pl.Free;
      end;
    end
  else
    if RBLinesRange.Checked then
      begin
        val(EDExportFromLine.Text,FromLine,valCode);
        if (valCode<>0) or (FromLine<1) or (FromLine>Report.TextDevice.SList.Count) then
          begin
            ActiveControl := EDExportFromLine;
            MBError(prLoadStr(sExportLinesRangeNotValid));
            exit;
          end;
        val(EDExportToLine.Text,ToLine,valCode);
        if (valCode<>0) or (ToLine<1) or (ToLine>Report.TextDevice.SList.Count) then
          begin
            ActiveControl := EDExportToLine;
            MBError(prLoadStr(sExportLinesRangeNotValid));
            exit;
          end;
        if FromLine>ToLine then
          begin
            ActiveControl := EDExportFromLine;
            MBError(prLoadStr(sExportLinesRangeNotValid));
            exit;
          end
      end;

Report.ExportFilter := PprExportFilterRegInfo(EDExportFilter.Items.Objects[EDExportFilter.ItemIndex]).ExportFilterClassRef.ClassName;
Report.ExportFileName := EDExportFileName.Text;
Report.ExportTxOptions := Report.ExportTxOptions-[prtxeoLinesRange];
if RBPagesRange.Checked then
  Report.ExportPagesMode := ppmPagesRange
else
  if RBPagesList.Checked then
    Report.ExportPagesMode := ppmPagesList
  else
    if RBLinesRange.Checked then
      Report.ExportTxOptions := Report.ExportTxOptions+[prtxeoLinesRange]
    else
      Report.ExportPagesMode := ppmAll;
Report.ExportFromPage := FromPage;
Report.ExportToPage := ToPage;
Report.ExportPages := EDExportPages.Text;
Report.ExportFromLine := FromLine;
Report.ExportToLine := ToLine;
Report.ExportCodePage := TprTxCodePage(EDExportCodePage.ItemIndex);
if EDExportESCModelName.ItemIndex>0 then
  begin
    Report.ExportESCModelName := EDExportESCModelName.Items[EDExportESCModelName.ItemIndex];
    Report.ExportTxOptions := Report.ExportTxOptions+[prtxeoUseESCModel];
  end
else
  begin
    Report.ExportESCModelName := '';
    Report.ExportTxOptions := Report.ExportTxOptions-[prtxeoUseESCModel];
  end;
ModalResult := mrOk;
end;

procedure TprTxExportParamsForm.RBPagesRangeClick(Sender: TObject);
begin
ActiveControl := EDExportFromPage;
end;

procedure TprTxExportParamsForm.RBPagesListClick(Sender: TObject);
begin
ActiveControl := EDExportPages;
end;

procedure TprTxExportParamsForm.RBLinesRangeClick(Sender: TObject);
begin
ActiveControl := EDExportFromLine;
end;

procedure TprTxExportParamsForm.EDExportFromPageChange(Sender: TObject);
begin
RBPagesRange.Checked := true;
end;

procedure TprTxExportParamsForm.EDExportPagesChange(Sender: TObject);
begin
RBPagesList.Checked := true;
end;

procedure TprTxExportParamsForm.EDExportFromLineChange(Sender: TObject);
begin
RBLinesRange.Checked := true;
end;

procedure TprTxExportParamsForm.EDExportFilterChange(Sender: TObject);
var
  s : string;
begin
if (Trim(EDExportFileName.Text)<>'') and (EDExportFilter.ItemIndex<>-1) then
  begin
    s := ExtractFilePath(Trim(EDExportFileName.Text))+ExtractFileNameOnly(Trim(EDExportFileName.Text));
    EDExportFileName.Text := s+'.'+PprExportFilterRegInfo(EDExportFilter.Items.Objects[EDExportFilter.ItemIndex]).ExportFilterExtention;
  end;
end;

end.
