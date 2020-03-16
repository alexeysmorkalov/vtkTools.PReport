{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_ExportFilters;

{$i pr.inc}

interface

uses
  Classes, SysUtils, Windows, shellapi, dialogs, math,
  graphics, richedit,

  vteExcel, vteExcelTypes,

  pr_Common, pr_Classes, pr_XLSConts;

type

/////////////////////////////////////////////////
//
// TCol
//
/////////////////////////////////////////////////
TCol = class(TObject)
  X : integer;
  Index : integer;
  constructor CreateCol(_X : integer);
end;

/////////////////////////////////////////////////
//
// TRow
//
/////////////////////////////////////////////////
TRow = class(TObject)
  Y : integer;
  Index : integer;
  PageIndex : integer;
  constructor CreateRow(_Y : integer; _PageIndex : integer);
end;

/////////////////////////////////////////////////
//
// rObjPlace
//
/////////////////////////////////////////////////
rObjPlace = record
  LeftCol : TCol;
  RightCol : TCol;
  TopRow : TRow;
  BottomRow : TRow;
end;
pObjPlace = ^rObjPlace;
rObjPlaceArray = array [0..MaxInt div 4 div sizeof(rObjPlace)] of rObjPlace;
pObjPlaceArray = ^rObjPlaceArray;

/////////////////////////////////////////////////
//
// TprvteExportFilter
//
/////////////////////////////////////////////////
TprvteExportFilter = class(TprCustomExportFilter)
protected
  procedure SetBorders(rng : TvteXLSRange; lBorder,tBorder,rBorder,bBorder : TprFrameLine);
  procedure BuildRange(v: TprExObjRecVersion; rng: TvteXLSRange; ADataAsString: Boolean);
  procedure BuildWorkbook(Report : TprCustomReport; wb : TvteXLSWorkbook);
  procedure MakeSave(Report : TprCustomReport; wb : TvteXLSWorkbook); virtual; abstract;
public
  procedure SaveToFile(Report : TprCustomReport); override;
end;

/////////////////////////////////////////////////
//
// TprXLSExportFilter
//
/////////////////////////////////////////////////
TprXLSExportFilter = class(TprvteExportFilter)
protected
  procedure MakeSave(Report : TprCustomReport; wb : TvteXLSWorkbook); override;
end;

/////////////////////////////////////////////////
//
// TprHTMLExportFilter
//
/////////////////////////////////////////////////
TprHTMLExportFilter = class(TprvteExportFilter)
protected
  procedure MakeSave(Report : TprCustomReport; wb : TvteXLSWorkbook); override;
end;

implementation

uses
  pr_Utils, pr_Strings, pr_MultiLang;

/////////////////////////////////////////////////
//
// TCol
//
/////////////////////////////////////////////////
constructor TCol.CreateCol;
begin
inherited Create;
X := _X;
end;

/////////////////////////////////////////////////
//
// TRow
//
/////////////////////////////////////////////////
constructor TRow.CreateRow;
begin
inherited Create;
Y := _Y;
PageIndex := _PageIndex;
end;

function SortCols(Item1,Item2 : pointer) : integer;
begin
Result := TCol(Item1).X-TCol(Item2).X;
end;

function SortRows(Item1,Item2 : pointer) : integer;
begin
if TRow(Item1).PageIndex=TRow(Item2).PageIndex then
  Result := TRow(Item1).Y-TRow(Item2).Y
else
  Result := TRow(Item1).PageIndex-TRow(Item2).PageIndex;
end;

/////////////////////////////////////////////////
//
// TprvteExportFilter
//
/////////////////////////////////////////////////
procedure TprvteExportFilter.SaveToFile(Report : TprCustomReport);
var
  wb : TvteXLSWorkbook;
begin
wb := TvteXLSWorkbook.Create;
try
  BuildWorkbook(Report,wb);
  MakeSave(Report,wb);
  if preoShowAfterGenerate in Report.ExportOptions then
    ShellExecute(0,'open',PChar(Report.ExportFileName),'','',SW_SHOWNORMAL);
finally
  wb.Free;
end;
end;

procedure TprvteExportFilter.SetBorders(rng : TvteXLSRange; lBorder,tBorder,rBorder,bBorder : TprFrameLine);
  procedure SetBorder(bt : TvteXLSBorderType; b : TprFrameLine);
  begin
  if not b.Show then exit;
  rng.Borders[bt].Color := b.Color;
  rng.Borders[bt].LineStyle := XLSLineStyle[b.Style];
  rng.Borders[bt].Weight := vtexlThin;
  end;
begin
SetBorder(vtexlEdgeLeft,lBorder);
SetBorder(vtexlEdgeTop,tBorder);
SetBorder(vtexlEdgeRight,rBorder);
SetBorder(vtexlEdgeBottom,bBorder);
end;

procedure TprvteExportFilter.BuildRange(v: TprExObjRecVersion; rng: TvteXLSRange; ADataAsString: Boolean);
var
  s,s2 : string;
  vExtended : extended;
  vInteger,valCode : integer;
begin
if v is TprMemoObjRecVersion then
  begin
    with TprMemoObjRecVersion(v) do
      begin
        s := Memo.Text;
        Delete(s,Length(s)-1,2);
        if ADataAsString then
          rng.Value := StringReplace(s,#13,'',[rfReplaceAll])
        else
        begin
          // check value version
          s2 := StringReplace(s,ThousandSeparator,'',[rfReplaceAll]);
          s2 := StringReplace(s2,CurrencyString,'',[rfReplaceAll]);
          val(s2,vInteger,valCode);
          if valCode=0 then
            begin
              rng.Value := vInteger;
              if pos(ThousandSeparator,s)<>0 then
                rng.Format := '# ##0';
            end
          else
            if TextToFloat(PChar(s2),vExtended,fvExtended) then
              begin
                rng.Value := vExtended;
                if pos(CurrencyString,s)<>0 then
                  rng.Format := '# ##0.00;-# ##0.00';
              end
            else
              rng.Value := StringReplace(s,#13,'',[rfReplaceAll]);
        end;
        rng.VerticalAlignment := XLSVertAlignment[vAlign];
        rng.HorizontalAlignment := XLSHorAlignment[hAlign];
        rng.Font.Assign(Font);
        rng.WrapText := WordWrap or (Memo.Count>1);
        if Rotate90 then
          rng.Rotation := 90;
        SetBorders(rng,lBorder,tBorder,rBorder,bBorder);
        if (FillColor<>clNone) and (FillColor<>clWhite) then
          begin
            rng.ForegroundFillPatternColor := FillColor;
            rng.FillPattern := vtefpSolid;
          end;
      end
  end
else
if v is TprImageObjRecVersion then
  begin
    with TprImageObjRecVersion(v) do
      begin
        rng.Worksheet.AddImage(rng.Place.Left,rng.Place.Top,rng.Place.Right+1,rng.Place.Bottom+1,Picture,false);
      end;
  end
else
if v is TprRichObjRecVersion then
  begin
    with TprRichObjRecVersion(v) do
      begin
        SetBorders(rng,lBorder,tBorder,rBorder,bBorder);
        rng.Value := StringReplace(GetText,#13,'',[rfReplaceAll]);
        rng.VerticalAlignment := vtexlVAlignTop;
        rng.HorizontalAlignment := vtexlHAlignLeft;
        rng.WrapText := true;
      end;
  end;
end;

procedure TprvteExportFilter.BuildWorkbook(Report : TprCustomReport; wb : TvteXLSWorkbook);
var
  v : TprExObjRecVersion;
  pl : TList;
  ep : TprEndPage;
  pi : TprPageInfo;
  sh : TvteXLSWorksheet;
  pop : pObjPlace;
  rng : TvteXLSRange;
  r,pr : TRow;
  pCap1 : string;
  Cols,Rows : TList;
  pObjsPlaces : pObjPlaceArray;
  n,rStart,epStart,i,j,k,m : integer;

  function CEP(v1,v2 : integer) : boolean;
  begin
  Result := Abs(v1-v2)<=TprReport(Report).ExportPrecision;
  end;

  function IsExportPage(PageNumber : integer) : boolean;
  begin
  PageNumber := PageNumber+1;
  Result := (Report.ExportPagesMode=ppmAll) or
            ((Report.ExportPagesMode=ppmPagesRange) and (PageNumber>=Report.ExportFromPage) and (PageNumber<=Report.ExportToPage)) or
            ((Report.ExportPagesMode=ppmPagesList) and (pl.IndexOf(pointer(PageNumber))<>-1));
  end;

begin
pObjsPlaces := nil;
Cols := TList.Create;
Rows := TList.Create;
if Report.ExportPagesMode=ppmPagesList then pl := TList.Create
                                       else pl := nil;
pCap1 := prLoadStr(sExportReportProgress);
if preoShowProgress in Report.ExportOptions then
  begin
    case Report.ExportPagesMode of
      ppmAll : j := Report.EndPagesCount;
      ppmPagesRange : j := Report.ExportToPage-Report.ExportFromPage+1;
      ppmPagesList : j := pl.Count;
      else j := 0;
    end;
    Report.CreateProgressForm(Format(prLoadStr(sExportReportCaption),[Report.ExportFileName]),j);
  end;
try
  if pl<>nil then
    TextToPageList(Report.ExportPages,pl);
  i := 0;
  while i<Report.EndPagesCount do
    begin
      if IsExportPage(i) then
        begin
          pi := TprEndPage(Report.EndPages[i]).PageInfo;
          epStart := i;
          pObjsPlaces := nil;
          n := 0;
          while (i<Report.EndPagesCount) and
                (pi.PageWidth=TprEndPage(Report.EndPages[i]).PageInfo.PageWidth) and
                (pi.PageHeight=TprEndPage(Report.EndPages[i]).PageInfo.PageHeight) and
                (pi.lMargin=TprEndPage(Report.EndPages[i]).PageInfo.lMargin) and
                (pi.tMargin=TprEndPage(Report.EndPages[i]).PageInfo.tMargin) and
                (pi.rMargin=TprEndPage(Report.EndPages[i]).PageInfo.rMargin) and
                (pi.bMargin=TprEndPage(Report.EndPages[i]).PageInfo.bMargin) do
            begin
              if IsExportPage(i) then
                begin
                  ep := TprEndPage(Report.EndPages[i]);
                  rStart := Rows.Count;
                  ReallocMem(pObjsPlaces,sizeof(rObjPlace)*(ep.VL.Count+n));
                  for j:=0 to ep.VL.Count-1 do
                    begin
                      v := TprExObjRecVersion(ep.VL[j]);
                      pop := @pObjsPlaces[n];
                      k := 0;
                      while (k<Cols.Count) and not CEP(TCol(Cols[k]).X,v.GeneratedRect.Left) do Inc(k);
                      if k>=Cols.Count then
                        pop.LeftCol := TCol(Cols[Cols.Add(TCol.CreateCol(v.GeneratedRect.Left))])
                      else
                        pop.LeftCol := TCol(Cols[k]);
                      k := 0;
                      while (k<Cols.Count) and not CEP(TCol(Cols[k]).X,v.GeneratedRect.Right) do Inc(k);
                      if k>=Cols.Count then
                        pop.RightCol := TCol(Cols[Cols.Add(TCol.CreateCol(v.GeneratedRect.Right))])
                      else
                        pop.RightCol := TCol(Cols[k]);

                      k := rStart;
                      while (k<Rows.Count) and not CEP(TRow(Rows[k]).Y,v.GeneratedRect.Top) do Inc(k);
                      if k>=Rows.Count then
                        pop.TopRow := TRow(Rows[Rows.Add(TRow.CreateRow(v.GeneratedRect.Top,i))])
                      else
                        pop.TopRow := TRow(Rows[k]);
                      k := rStart;
                      while (k<Rows.Count) and not CEP(TRow(Rows[k]).Y,v.GeneratedRect.Bottom) do Inc(k);
                      if k>=Rows.Count then
                        pop.BottomRow := TRow(Rows[Rows.Add(TRow.CreateRow(v.GeneratedRect.Bottom,i))])
                      else
                        pop.BottomRow := TRow(Rows[k]);
                      Inc(n);
                    end;
                end;
              Inc(i);
            end;

          Cols.Sort(SortCols);
          Rows.Sort(SortRows);

          for j:=0 to Cols.Count-1 do
            TCol(Cols[j]).Index := j;
          for j:=0 to Rows.Count-1 do
            TRow(Rows[j]).Index := j;

          sh := wb.AddSheet;
          sh.PageSetup.PaperSize := TvteXLSPaperSizeType(TprEndPage(Report.EndPages[epStart]).PageInfo.PaperSize);
          sh.PageSetup.Orientation := TvteXLSOrientationType(TprEndPage(Report.EndPages[epStart]).PageInfo.Orientation);
          sh.PageSetup.LeftMargin := 0; 
          sh.PageSetup.TopMargin := 0;
          sh.PageSetup.RightMargin := 0;
          sh.PageSetup.BottomMargin := 0;
          sh.PageSetup.HeaderMargin := 0;
          sh.PageSetup.FooterMargin := 0;

          for j:=1 to Cols.Count-1 do
            sh.Cols[j-1].PixelWidth := TCol(Cols[j]).X-TCol(Cols[j-1]).X;
          n := 0;
          for j:=1 to Rows.Count-1 do
            begin
              r := TRow(Rows[j]);
              pr := TRow(Rows[j-1]);
              if r.PageIndex=pr.PageIndex then
                begin
                  sh.Rows[n].PixelHeight := r.Y-pr.Y;
                  Inc(n);
                end
              else
                begin
                  sh.AddPageBreakAfterRow(n);
                end;
            end;

          // moving objects
          j := epStart;
          n := 0;
          m := 0;
          while j<i do
            begin
              if IsExportPage(j) then
                begin
                  Report.UpdateProgressForm(Format(pCap1,[j+1,Report.EndPagesCount]));
                  ep := TprEndPage(Report.EndPages[j]);
                  for k:=0 to ep.VL.Count-1 do
                    begin
                      v := TprExObjRecVersion(ep.VL[k]);
                      pop := @pObjsPlaces[n];
                      rng := sh.Ranges[pop.LeftCol.Index,pop.TopRow.Index-m,pop.RightCol.Index-1,pop.BottomRow.Index-1-m];
                      BuildRange(v,rng,TprReport(Report).ExportDataAsStrings);
                      Inc(n);
                    end;
                  Inc(m);
                end;
              Inc(j);
            end;

          for j:=0 to Rows.Count-1 do
            TRow(Rows[j]).Free;
          Rows.Clear; // !!!
          for j:=0 to Cols.Count-1 do
            TCol(Cols[j]).Free;
          Cols.Clear; // !!!
          ReallocMem(pObjsPlaces,0);
        end
      else
        Inc(i); 
    end;
finally
  if pObjsPlaces<>nil then
    FreeMem(pObjsPlaces);
  for j:=0 to Cols.Count-1 do
    TCol(Cols[j]).Free;
  Cols.Free; // !!!
  for j:=0 to Rows.Count-1 do
    TRow(Rows[j]).Free;
  Rows.Free; // !!!
  pl.Free;
  Report.CloseProgressForm;
end;
end;

/////////////////////////////////////////////////
//
// TprXLSExportFilter
//
/////////////////////////////////////////////////
procedure TprXLSExportFilter.MakeSave(Report : TprCustomReport; wb : TvteXLSWorkbook);
begin
wb.SaveAsXLSToFile(Report.ExportFileName);
end;

/////////////////////////////////////////////////
//
// TprHTMLExportFilter
//
/////////////////////////////////////////////////
procedure TprHTMLExportFilter.MakeSave(Report : TprCustomReport; wb : TvteXLSWorkbook);
begin
wb.SaveAsHTMLToFile(Report.ExportFileName);
end;

initialization

prRegisterExportFilter(TprXLSExportFilter,TprReport,'xls',prLoadStr(sXLSExportFilterDesc));
prRegisterExportFilter(TprHTMLExportFilter,TprReport,'htm',prLoadStr(sHTMExportFilterDesc));

end.

