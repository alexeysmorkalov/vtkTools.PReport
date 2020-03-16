{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxExportFilters;

{$i pr.inc}

interface

uses
  Classes, SysUtils, Windows, shellapi, dialogs, math,
  graphics, richedit,

  pr_Common, pr_TxClasses, pr_TxUtils, pr_TxConsts;

type

/////////////////////////////////////////////////
//
// TprTxTXTExportFilter
//
/////////////////////////////////////////////////
TprTxTXTExportFilter = class(TprCustomExportFilter)
public
  procedure SaveToFile(Report : TprCustomReport); override;
end;


implementation

uses
  pr_Strings, pr_MultiLang, pr_Utils;

/////////////////////////////////////////////////
//
// TprTxTXTExportFilter
//
/////////////////////////////////////////////////
procedure TprTxTXTExportFilter.SaveToFile(Report : TprCustomReport);
var
  pCap : string;
  hFile : THandle;
  ESCModel : TprESCModel;
  TxReport : TprTxReport;
  pl,PagesList : TList;
  i,FromLine,ToLine : integer;

  procedure ExportLine(const s : string);
  var
    b : string;
    i,l,n : integer;
    BytesWrited : cardinal;
  begin
  l := Length(s);
  i := 1;
  while i<=l do
    begin
      if s[i]=ESCSymbol then
        begin
          Inc(i);
          if s[i]=ESCSymbolForExCommands then
            begin
              if ESCModel<>nil then
                b := b+ESCModel.GetRealESCCommandForESCPrefix(Copy(s,i,3));
              i := i+3;
            end
          else
            if ESCModel<>nil then
              b := b+ESCModel.GetRealESCCommandForESCPrefix(s[i]);
        end
      else
        if s[i]=ESCCustomCommandSymbol then
          begin
            n := i;
            repeat
              Inc(i);
            until (i>l) or (s[i]=ESCCustomCommandSymbol);
            if ESCModel <> nil then
              b := b + Copy(s, n + 1, i - n - 1);
          end
        else
          b := b+s[i];
      Inc(i);
    end;
  if (TxReport.ExportCodePage=prtxcpWIN1251) and (b<>'') then
    TxReport.OEMtoWIN(@(b[1]),@(b[1]));
  b := b+#13#10;
  if not WriteFile(hFile,b[1],Length(b),BytesWrited,nil) or (BytesWrited<>cardinal(Length(b))) then
    raise Exception.CreateFmt(prLoadStr(sExportTXTErrorWriteFile),[SysErrorMessage(GetLastError)]);
  end;

  procedure ExportPage(PageIndex : integer);
  var
    i,l : integer;
  begin
  if PageIndex>=PagesList.Count-1 then
    l := TxReport.TextDevice.SList.Count
  else
    l := integer(PagesList[PageIndex+1]);
  for i:=integer(PagesList[PageIndex]) to l-1 do
    ExportLine(TxReport.TextDevice.SList[i]);
  end;
  
begin
TxReport := TprTxReport(Report);
hFile := CreateFile(PChar(TxReport.ExportFileName),
                    GENERIC_WRITE,
                    FILE_SHARE_READ,
                    nil,
                    CREATE_ALWAYS,
                    FILE_ATTRIBUTE_NORMAL,
                    0);
if hFile=INVALID_HANDLE_VALUE then
  raise Exception.CreateFmt(prLoadStr(sExportTXTErrorCreateFile),[SysErrorMessage(GetLastError)]);
if prtxeoUseESCModel in TxReport.ExportTxOptions then
  ESCModel := TxReport.ExportESCModel
else
  ESCModel := nil;
pl := nil;
PagesList := nil;
if not (prtxeoLinesRange in TxReport.ExportTxOptions) then
  begin
    if TxReport.ExportPagesMode=ppmPagesList then
      pl := TList.Create;
    if TxReport.ExportPagesMode in [ppmPagesList,ppmPagesRange] then
      PagesList := TList.Create;
  end;
try
  if PagesList<>nil then
    TxReportOptions.ParsePages(TxReport.TextDevice.SList,false,0,PagesList);
  if pl<>nil then
    TextToPageList(TxReport.ExportPages,pl);

  if preoShowProgress in TxReport.ExportOptions then
    begin
      if prtxeoLinesRange in TxReport.ExportTxOptions then
        i := (TxReport.ExportToLine-TxReport.ExportFromLine) div 100
      else
        if TxReport.ExportPagesMode=ppmPagesRange then
          i := TxReport.ExportToPage-TxReport.ExportFromPage
        else
          if TxReport.ExportPagesMode=ppmPagesList then
            i := pl.Count
          else
            i := TxReport.TextDevice.SList.Count div 100;
      TxReport.CreateProgressForm(Format(prLoadStr(sExportReportCaption),[TxReport.ExportFileName]),i);
    end;

  if (prtxeoLinesRange in TxReport.ExportTxOptions) or (TxReport.ExportPagesMode=ppmAll) then
    begin
      if prtxeoLinesRange in TxReport.ExportTxOptions then
        begin
          FromLine := TxReport.ExportFromLine-1;
          ToLine := TxReport.ExportToLine-1;
        end
      else
        begin
          FromLine := 0;
          ToLine := TxReport.TextDevice.SList.Count-1;
        end;
      pCap := prLoadStr(sExportReportProgressLines);
      for i:=FromLine to ToLine do
        begin
          if (i mod 100)=0 then
            TxReport.UpdateProgressForm(Format(pCap,[i+1,ToLine+1]));
          ExportLine(TxReport.TextDevice.SList[i]);
        end;
    end
  else
    begin
      pCap := prLoadStr(sExportReportProgress);
      case TxReport.ExportPagesMode of
        ppmPagesRange:
          for i:=TxReport.ExportFromPage to TxReport.ExportToPage do
            begin
              TxReport.UpdateProgressForm(Format(pCap,[i-TxReport.ExportFromPage,TxReport.ExportToPage-TxReport.ExportFromPage+1]));
              ExportPage(i-1);
            end;
        ppmPagesList:
          for i:=0 to pl.Count-1 do
            begin
              TxReport.UpdateProgressForm(Format(pCap,[i+1,pl.Count]));
              ExportPage(integer(pl[i])-1);
            end;
      end;
    end;

finally
  CloseHandle(hFile);
  if pl<>nil then
    pl.Free;
  if PagesList<>nil then
    PagesList.Free;
  TxReport.CloseProgressForm;
end;
end;

initialization

prRegisterExportFilter(TprTxTXTExportFilter,TprTxReport,'txt',prLoadStr(sTXTExportFilterDesc));

end.
