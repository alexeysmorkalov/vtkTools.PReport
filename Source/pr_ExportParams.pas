{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_ExportParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons,

  pr_Common, pr_classes, pr_MultiLang, ExtCtrls;
  

{$I pr.inc}

type
  TprExportParamsForm = class(TprForm)
    prMLRes1: TprMLRes;
    Label1: TLabel;
    EDExportFileName: TEdit;
    GroupBox1: TGroupBox;
    RBAll: TRadioButton;
    RBPagesRange: TRadioButton;
    RBPagesList: TRadioButton;
    EDExportPages: TEdit;
    CBShowAfterGenerate: TCheckBox;
    bOK: TButton;
    bCancel: TButton;
    EDExportFromPage: TEdit;
    Label2: TLabel;
    EDExportToPage: TEdit;
    bFileName: TSpeedButton;
    RGExportPrecision: TRadioGroup;
    SaveDialog: TSaveDialog;
    Label3: TLabel;
    EDExportFilter: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure bFileNameClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure RBPagesRangeClick(Sender: TObject);
    procedure RBPagesListClick(Sender: TObject);
    procedure EDExportFromPageChange(Sender: TObject);
    procedure EDExportPagesChange(Sender: TObject);
    procedure EDExportFilterChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Report : TprReport;
    function EditParams(_Report : TprReport) : boolean;
  end;

implementation

uses
  pr_Strings, pr_Utils;

{$R *.DFM}

procedure TprExportParamsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

procedure TprExportParamsForm.FormCreate(Sender: TObject);
begin
LoadResImage(bFileName.Glyph,'OPEN');
RGExportPrecision.Items.Clear;
RGExportPrecision.Items.Add(prLoadStr(sExportPrecisionLow));
RGExportPrecision.Items.Add(prLoadStr(sExportPrecisionNormal));
RGExportPrecision.Items.Add(prLoadStr(sExportPrecisionHigh));
end;

function TprExportParamsForm.EditParams;
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

EDExportFileName.Text := Report.ExportFileName;
RBPagesRange.Enabled := Report.EndPagesCount>1;
Label2.Enabled := Report.EndPagesCount>1;
RBPagesList.Enabled := Report.EndPagesCount>1;
EDExportPages.Enabled := Report.EndPagesCount>1;
EDExportFromPage.Enabled := Report.EndPagesCount>1;
EDExportToPage.Enabled := Report.EndPagesCount>1;

if Report.ExportFromPage>0 then
  EDExportFromPage.Text := IntToStr(Report.ExportFromPage);
if Report.ExportToPage>0 then
  EDExportToPage.Text := IntToStr(Report.ExportToPage);
EDExportPages.Text := Report.ExportPages;

RBAll.Checked := (Report.ExportPagesMode=ppmAll) or (Report.EndPagesCount=1);
RBPagesRange.Checked := (Report.ExportPagesMode=ppmPagesRange) and (Report.EndPagesCount>1);
RBPagesList.Checked := (Report.ExportPagesMode=ppmPagesList) and (Report.EndPagesCount>1);

CBShowAfterGenerate.Checked := preoShowAfterGenerate in Report.ExportOptions;
if Report.ExportPrecision=Report.ExportPrecisionLow then
  RGExportPrecision.ItemIndex := 0
else
  if Report.ExportPrecision=Report.ExportPrecisionNormal then
    RGExportPrecision.ItemIndex := 1
  else
    if Report.ExportPrecision=Report.ExportPrecisionHigh then
      RGExportPrecision.ItemIndex := 2;
Result := ShowModal=mrOk;
end;

procedure TprExportParamsForm.bFileNameClick(Sender: TObject);
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

procedure TprExportParamsForm.bOKClick(Sender: TObject);
var
  s : string;
  pl : TList;
  FromPage,ToPage,valCode : integer;
begin
FromPage := 0;
ToPage := 0;
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
    if (valCode<>0) or (FromPage<1) or (FromPage>Report.EndPagesCount) then
      begin
        ActiveControl := EDExportFromPage;
        MBError(prLoadStr(sExportPagesRangeNotValid));
        exit;
      end;
    val(EDExportToPage.Text,ToPage,valCode);
    if (valCode<>0) or (ToPage<1) or (ToPage>Report.EndPagesCount) then
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
        if (pl.Count<1) or (integer(pl[0])<1) or (integer(pl[0])>Report.EndPagesCount) or (integer(pl[pl.Count-1])<1) or (integer(pl[pl.Count-1])>Report.EndPagesCount) then
          begin
            ActiveControl := EDExportPages;
            MBError(prLoadStr(sExportPagesListNotValid));
            exit;
          end;
      finally
        pl.Free;
      end;
    end;

Report.ExportFilter := PprExportFilterRegInfo(EDExportFilter.Items.Objects[EDExportFilter.ItemIndex]).ExportFilterClassRef.ClassName;
Report.ExportFileName := EDExportFileName.Text;

if (Trim(Report.ExportFileName)<>'') and (EDExportFilter.ItemIndex<>-1) then
  begin
    s := ExtractFilePath(Trim(Report.ExportFileName))+ExtractFileNameOnly(Trim(Report.ExportFileName));
    Report.ExportFileName := s+'.'+PprExportFilterRegInfo(EDExportFilter.Items.Objects[EDExportFilter.ItemIndex]).ExportFilterExtention;
  end;

if RBPagesRange.Checked then
  Report.ExportPagesMode := ppmPagesRange
else
  if RBPagesList.Checked then
    Report.ExportPagesMode := ppmPagesList
  else
    Report.ExportPagesMode := ppmAll;
Report.ExportFromPage := FromPage;
Report.ExportToPage := ToPage;
Report.ExportPages := EDExportPages.Text;
if CBShowAfterGenerate.Checked then
  Report.ExportOptions := Report.ExportOptions+[preoShowAfterGenerate]
else
  Report.ExportOptions := Report.ExportOptions-[preoShowAfterGenerate];
case RGExportPrecision.ItemIndex of
  0: Report.ExportPrecision := Report.ExportPrecisionLow;
  1: Report.ExportPrecision := Report.ExportPrecisionNormal;
  2: Report.ExportPrecision := Report.ExportPrecisionHigh;  
end;
ModalResult := mrOk;
end;

procedure TprExportParamsForm.RBPagesRangeClick(Sender: TObject);
begin
ActiveControl := EDExportFromPage;
end;

procedure TprExportParamsForm.RBPagesListClick(Sender: TObject);
begin
ActiveControl := EDExportPages;
end;

procedure TprExportParamsForm.EDExportFromPageChange(Sender: TObject);
begin
RBPagesRange.Checked := true;
end;

procedure TprExportParamsForm.EDExportPagesChange(Sender: TObject);
begin
RBPagesList.Checked := true;
end;

procedure TprExportParamsForm.EDExportFilterChange(Sender: TObject);
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
