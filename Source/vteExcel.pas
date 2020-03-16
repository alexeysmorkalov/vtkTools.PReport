
{******************************************}
{                                          }
{           vtk Export library             }
{                                          }
{      Copyright (c) 2002 by vtkTools      }
{                                          }
{******************************************}

unit vteExcel;

interface

{$i vtk.inc}

uses
  Windows, SysUtils, Classes, Graphics, math,
  {$IFDEF VTK_D6_OR_D7} variants, {$ENDIF}

  BIFF8_Types, vteExcelTypes, vteConsts ;

const
  sXLSWorksheetTitlePrefix = 'Sheet';
//  sDefaultFontName = 'Arial';
  XLSDefaultRowHeight = $00FF;
  XLSDefaultColumnWidthInChars = $0008;
  MaxDefaultColors = 16;
  MaxBiffRecordSize = 8228;
  mergeBlockItemsCount = 1024;//(MaxBiffRecordSize-4-sizeof(rb8MERGE)) div sizeof(rb8MERGErec);

  sErrorInvalidPictureFormat = 'Invalid picture format';

type
TvteXLSWorkbook = class;
TvteXLSWorksheet = class;
TvteXLSRow = class;
TvteXLSCol = class;
TCardinalArray = array [0..MaxInt div SizeOf(Cardinal) div 4] of cardinal;
PCardinalArray = ^TCardinalArray;

///////////////////////////
//
// TvteXLSBorder
//
///////////////////////////
TvteXLSBorder = class(TObject)
private
  FColor : TColor;
  FLineStyle : TvteXLSLineStyleType;
  FWeight : TvteXLSWeightType;
public
  property Color : TColor read FColor write FColor;
  property LineStyle : TvteXLSLineStyleType read FLineStyle write FLineStyle;
  property Weight : TvteXLSWeightType read FWeight write FWeight;

  constructor Create;
  destructor Destroy; override; 
end;

///////////////////////////
//
// TvteXLSBorders
//
///////////////////////////
TvteXLSBorders = class(TObject)
private
  FBorders : array [TvteXLSBorderType] of TvteXLSBorder;
  function GetItem(i : TvteXLSBorderType) : TvteXLSBorder;
public
  constructor Create;
  destructor Destroy; override;
  procedure SetAttributes(
    ABorders : TvteXLSBorderTypes;
    AColor : TColor;
    ALineStyle : TvteXLSLineStyleType;
    AWeight : TvteXLSWeightType);
  property Borders[i : TvteXLSBorderType] : TvteXLSBorder read GetItem; default;
end;

TDynWordArray = array of word;
///////////////////////////
//
// TvteXLSRange
//
//////////////////////////
TvteXLSRange = class(TObject)
private
  FWorksheet : TvteXLSWorksheet;
  FPlace : TRect;
  FBorders : TvteXLSBorders;
  FFont : TFont;
  FHorizontalAlignment : TvteXLSHorizontalAlignmentType;
  FVerticalAlignment : TvteXLSVerticalAlignmentType;
  FWrapText : boolean;
  FRotation : byte;
  FFormat : string;
  FValue : Variant;
  FFillPattern : TvteXLSFillPattern;
  FForegroundFillPatternColor : TColor;
  FBackgroundFillPatternColor : TColor;
  FFormula : string;

  FExportData : pointer;

  function GetWorkbook : TvteXLSWorkbook;
  function GetCellDataType : TvteCellDataType;
  procedure SetValue(Value : Variant);
public
  property Worksheet : TvteXLSWorksheet read FWorksheet;
  property Workbook : TvteXLSWorkbook read GetWorkbook;
  property Place : TRect read FPlace;
  property Borders : TvteXLSBorders read FBorders;
  property Font : TFont read FFont;
  property HorizontalAlignment : TvteXLSHorizontalAlignmentType read FHorizontalAlignment write FHorizontalAlignment;
  property VerticalAlignment : TvteXLSVerticalAlignmentType read FVerticalAlignment write FVerticalAlignment;
  property Value : Variant read FValue write SetValue;
  property WrapText : boolean read FWrapText write FWrapText;
  property Rotation : byte read FRotation write FRotation;
  property Format : string read FFormat write FFormat;
  property FillPattern : TvteXLSFillPattern read FFillPattern write FFillPattern;
  property ForegroundFillPatternColor : TColor read FForegroundFillPatternColor write FForegroundFillPatternColor;
  property BackgroundFillPatternColor : TColor read FBackgroundFillPatternColor write FBackgroundFillPatternColor;

  property ExportData : pointer read FExportData write FExportData;
  property CellDataType : TvteCellDataType read GetCellDataType;

  property Formula : string read FFormula write FFormula;

  constructor Create(AWorksheet : TvteXLSWorksheet);
  destructor Destroy; override;
end;

/////////////////////////////
//
// TvteXLSRow
//
/////////////////////////////
TvteXLSRow = class(TObject)
private
  FInd : integer;
  FHeight : integer;
  function GetPixelHeight : integer;
  procedure SetPixelHeight(value: integer);
  function GetInchHeight : double;
  procedure SetInchHeight(value: double);
  function GetCentimeterHeight : double;
  procedure SetCentimeterHeight(value: double);
  function GetExcelHeight : double;
  procedure SetExcelHeight(value: double);
public
  property Ind : integer read FInd;
  property Height : integer read FHeight write FHeight;
  property PixelHeight : integer read GetPixelHeight write SetPixelHeight;
  property InchHeight: double read GetInchHeight write SetInchHeight;
  property CentimeterHeight: double read GetCentimeterHeight write SetCentimeterHeight;
  property ExcelHeight: double read GetExcelHeight write SetExcelHeight;
  constructor Create;
end;

/////////////////////////////
//
// TvteXLSCol
//
/////////////////////////////
TvteXLSCol = class(TObject)
private
  FInd : integer;
  FWidth : integer;
  procedure SetWidth(Value : integer);
  function GetPixelWidth : integer;
  procedure SetPixelWidth(value: integer);
  function GetInchWidth : double;
  procedure SetInchWidth(value: double);
  function GetCentimeterWidth : double;
  procedure SetCentimeterWidth(value: double);
  function GetExcelWidth : double;
  procedure SetExcelWidth(value: double);
public
  property Ind : integer read FInd write FInd;
  property Width : integer read FWidth write SetWidth;
  property PixelWidth : integer read GetPixelWidth write SetPixelWidth;
  property InchWidth: double read GetInchWidth write SetInchWidth;
  property CentimeterWidht: double read GetCentimeterWidth write SetCentimeterWidth;
  property ExcelWidth : double read GetExcelWidth write SetExcelWidth;
  constructor Create;
end;

//////////////////////////////
//
// TvteXLSPageSetup
//
//////////////////////////////
TvteXLSPageSetup = class(TObject)
private
  FBlackAndWhite : boolean;
  FCenterFooter : string;
  FCenterHeader : string;
  FCenterHorizontally : boolean;
  FCenterVertically : boolean;
  FDraft : boolean;
  FFirstPageNumber : integer;
  FFitToPagesTall : integer;
  FFitToPagesWide : integer;
  FLeftFooter : string;
  FLeftHeader : string;
  FOrder : TvteXLSOrderType;
  FOrientation : TvteXLSOrientationType;
  FPaperSize : TvteXLSPaperSizeType;
  FPrintGridLines : boolean;
  FPrintHeaders : boolean;
  FPrintNotes : boolean;
  FRightFooter : string;
  FRightHeader : string;
  FLeftMargin : double;
  FRightMargin : double;
  FTopMargin : double;
  FBottomMargin : double;
  FFooterMargin : double;
  FHeaderMargin : double;
  FZoom : integer;
  FCopies : integer;
  FUseScale: Boolean;
public
  property LeftFooter : string read FLeftFooter write FLeftFooter;
  property LeftHeader : string read FLeftHeader write FLeftHeader;
  property CenterFooter : string read FCenterFooter write FCenterFooter;
  property CenterHeader : string read FCenterHeader write FCenterHeader;
  property RightFooter : string read FRightFooter write FRightFooter;
  property RightHeader : string read FRightHeader write FRightHeader;

  property CenterHorizontally : boolean read FCenterHorizontally write FCenterHorizontally;
  property CenterVertically : boolean read FCenterVertically write FCenterVertically;

  property LeftMargin : double read FLeftMargin write FLeftMargin;
  property RightMargin : double read FRightMargin write FRightMargin;
  property TopMargin : double read FTopMargin write FTopMargin;
  property BottomMargin : double read FBottomMargin write FBottomMargin;

  property HeaderMargin : double read FHeaderMargin write FHeaderMargin;
  property FooterMargin : double read FFooterMargin write FFooterMargin;

  property PaperSize : TvteXLSPaperSizeType read FPaperSize write FPaperSize;
  property Orientation : TvteXLSOrientationType read FOrientation write FOrientation;
  property Order : TvteXLSOrderType read FOrder write FOrder;
  property FirstPageNumber : integer read FFirstPageNumber write FFirstPageNumber;
  property FitToPagesTall : integer read FFitToPagesTall write FFitToPagesTall;
  property FitToPagesWide : integer read FFitToPagesWide write FFitToPagesWide;
  property Copies : integer read FCopies write FCopies;
  property Zoom : integer read FZoom write FZoom;
  property UseScale: Boolean read FUseScale write FUseScale;

  property BlackAndWhite : boolean read FBlackAndWhite write FBlackAndWhite;
  property Draft : boolean read FDraft write FDraft;
  property PrintNotes : boolean read FPrintNotes write FPrintNotes;

  property PrintGridLines : boolean read FPrintGridLines write FPrintGridLines;
  property PrintHeaders : boolean read FPrintHeaders write FPrintHeaders;
  
  constructor Create;
end;

/////////////////////////////////////////////////
//
// TvteImage
//
/////////////////////////////////////////////////
TvteImage = class(TObject)
private
  FLeft : integer;
  FLeftCO : integer;
  FTop : integer;
  FTopCO : integer;
  FRight : integer;
  FRightCO : integer;
  FBottom : integer;
  FBottomCO : integer;
  FPicture : TPicture;
  FOwnsImage : boolean;
  FBorderLineColor : TColor;
  FBorderLineStyle : TvteXLSImageBorderLineStyle;
  FBorderLineWeight : TvteXLSImageBorderLineWeight;
  FScalePercentX : integer;
  FScalePercentY : integer;
public
  property Left : integer read FLeft write FLeft;
  property LeftCO : integer read FLeftCO write FLeftCO;
  property Top : integer read FTop write FTop;
  property TopCO : integer read FTopCO write FTopCO;
  property Right : integer read FRight write FRight;
  property RightCO : integer read FRightCO write FRightCO;
  property Bottom : integer read FBottom write FBottom;
  property BottomCO : integer read FBottomCO write FBottomCO;
  property Picture : TPicture read FPicture;
  property BorderLineColor : TColor read FBorderLineColor write FBorderLineColor;
  property BorderLineStyle : TvteXLSImageBorderLineStyle read FBorderLineStyle write FBorderLineStyle;
  property BorderLineWeight : TvteXLSImageBorderLineWeight read FBorderLineWeight write FBorderLineWeight;
  property ScalePercentX : integer read FScalePercentX write FScalePercentX;
  property ScalePercentY : integer read FScalePercentY write FScalePercentY;

  constructor Create(_Left,_Top,_Right,_Bottom : integer; _Picture : TPicture; _OwnsImage : boolean);
  constructor CreateScaled(_Left,_LeftCO,_Top,_TopCO,_ScalePercentX,_ScalePercentY : integer; _Picture : TPicture; _OwnsImage : boolean);
  constructor CreateWithOffsets(_Left,_LeftCO,_Top,_TopCO,_Right,_RightCO,_Bottom,_BottomCO : integer; _Picture : TPicture; _OwnsImage : boolean);
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TvteImages
//
/////////////////////////////////////////////////
TvteImages = class(TList)
private
  function GetItm(i : integer) : TvteImage;
public
  property Items[i : integer] : TvteImage read GetItm; default;
  procedure Clear; override;
  destructor Destroy; override;
end;

//////////////////////////////
//
// TvteXLSWorksheet
//
//////////////////////////////
TvteXLSWorksheet = class(TObject)
private
  FWorkbook : TvteXLSWorkbook;
  FTitle : string;
  FPageSetup : TvteXLSPageSetup;
  FImages : TvteImages;

  FRanges : TList;
  FCols : TList;
  FRows : TList;
  FPageBreaks : TList;

  FDimensions : TRect; // sizes of worksheet in cells

  FMaxRangeLength : integer;

  procedure SetTitle(Value : string);
//  function GetRange(xl,yt,xr,yb : integer) : TvteXLSRange;
  function GetCol(ColIndex : integer) : TvteXLSCol;
  function GetRow(RowIndex : integer) : TvteXLSRow;
  function GetRangesCount : integer;
  function GetXLSRange(RangeIndex : integer) : TvteXLSRange;
  function GetColsCount : integer;
  function GetRowsCount : integer;
  function GetIndexInWorkBook : integer;
  function GetColByIndex(i : integer) : TvteXLSCol;
  function GetRowByIndex(i : integer) : TvteXLSRow;
  function GetPageBreak(i : integer) : integer;
  function GetPageBreaksCount : integer;

//  function AddRange(xl,yt,xr,yb : integer) : TvteXLSRange;
  function AddRow(RowIndex : integer) : TvteXLSRow;
  function AddCol(ColIndex : integer) : TvteXLSCol;

//  function FindRange(xl,yt,xr,yb : integer) : TvteXLSRange;

  procedure  SetMaxRangeLength(const Value : integer);
  procedure  ResetMaxRangeLength;
  function GetRangeSp(xl,yt,xr,yb : integer) : TvteXLSRange;
  function SeekTop(const Value : integer) : integer;
  function ScanGet(const Index : integer; const R : TRect; var RemoveFlag : boolean) : TvteXLSRange;

public
  property Title : string read FTitle write SetTitle;
  property PageSetup : TvteXLSPageSetup read FPageSetup;
  property Ranges[xl,yt,xr,yb : integer] : TvteXLSRange read GetRangeSp; default;
  property Cols[ColIndex : integer] : TvteXLSCol read GetCol;
  property Rows[RowIndex : integer] : TvteXLSRow read GetRow;
  property RangeByIndex[RangeIndex : integer] : TvteXLSRange read GetXLSRange;
  property RangesCount : integer read GetRangesCount;
  property ColByIndex[ColIndex : integer] : TvteXLSCol read GetColByIndex;
  property ColsCount : integer read GetColsCount;
  property RowByIndex[RowIndex : integer] : TvteXLSRow read GetRowByIndex;
  property RowsCount : integer read GetRowsCount;
  property IndexInWorkBook : integer read GetIndexInWorkBook;
  property Images : TvteImages read FImages;
  property PageBreaks[i : integer] : integer read GetPageBreak;
  property PageBreaksCount : integer read GetPageBreaksCount;

  property Workbook : TvteXLSWorkbook read FWorkbook;
  property Dimensions : TRect read FDimensions;

  function GetDefaultColumnPixelWidth : integer;
  function GetDefaultRowPixelHeight : integer;

  function FindRow(RowIndex : integer) : TvteXLSRow;
  function FindCol(ColIndex : integer) : TvteXLSCol;
  function FindPageBreak(RowNumber : integer) : integer;

  function AddImage(Left,Top,Right,Bottom : integer; Picture : TPicture; OwnsImage : boolean) : TvteImage;
  function AddImageWithOffsets(Left,LeftCO,Top,TopCO,Right,RightCO,Bottom,BottomCO : integer; Picture : TPicture; OwnsImage : boolean) : TvteImage;
  function AddImageScaled(Left,LeftCO,Top,TopCO,ScalePercentX,ScalePercentY : integer; Picture : TPicture; OwnsImage : boolean) : TvteImage;

  procedure AddPageBreakAfterRow(RowNumber : integer);
  procedure DeletePageBreakAfterRow(RowNumber : integer);

  constructor Create(AWorkbook : TvteXLSWorkbook);
  destructor Destroy; override;
end;

////////////////////////////
//
// TvteXLSWorkbook
//
////////////////////////////
TvteXLSWorkbook = class(TObject)
private
  FUserNameOfExcel : string;
  FSheets : TList;

  procedure SetUserNameOfExcel(Value : string);
  procedure ClearSheets;
  function GetSheetsCount : integer;
  function GetXLSWorkSheet(i : integer) : TvteXLSWorkSheet;
public
  property UserNameOfExcel : string read FUserNameOfExcel write SetUserNameOfExcel;
  property SheetsCount : integer read GetSheetsCount;
  property Sheets[i : integer] : TvteXLSWorkSheet read GetXLSWorkSheet;

  procedure SaveAsXLSToFile(const FileName : string);
  procedure SaveAsHTMLToFile(const FileName : string);

  function AddSheet : TvteXLSWorksheet;
  function GetSheetIndex(const SheetTitle : string) : integer; // version 1.3

  procedure Clear;
  constructor Create;
  destructor Destroy; override;
end;

///////////////////////////
//
// TvteCustomWriter
//
///////////////////////////
TvteCustomWriter = class(TObject)
public
  procedure Save(WorkBook : TvteXLSWorkbook; const FileName : string);  virtual;
end;

function PointInRect(X,Y : integer; const R : TRect) : boolean;
function RectOverRect(const r1 : TRect; const r2 : TRect) : boolean;

implementation

uses
  vteWriters;

function GetCharacterWidth: integer;
var
  F : TFont;
  DC : HDC;
  sz : TSize;
  TM : TEXTMETRIC;
begin
  DC := GetDC(0);
  F := TFont.Create;
  try
    F.Name := vteDefFontFace;
    F.Size := vteDefFontSize;
    SelectObject(DC,F.Handle);
    GetTextMetrics(DC,TM);
    Result := sz.cx;
    Result := TM.tmAveCharWidth + TM.tmExternalLeading;
  finally
    F.Free;
  end;
  ReleaseDC(0,DC);
end;

function GetPixelPerInch: integer;
var
  DC : HDC;
begin
  DC := GetDC(0);
  Result := GetDeviceCaps(DC,LOGPIXELSX);
  ReleaseDC(0,DC);
end;

function PointInRect(X,Y : integer; const R : TRect) : boolean;
begin
Result:=((X>=R.Left) and (X<=R.Right)) and ((Y>=R.Top) and (Y<=R.Bottom))
end;

function RectOverRect(const r1 : TRect; const r2 : TRect) : boolean;
begin
Result :=
  (((r1.Left >= r2.Left) and (r1.Left <= r2.Right)) or ((r1.Right >= r2.Left) and (r1.Right <= r2.Right)) or ((r1.Left <= r2.Left) and (r1.Right >= r2.Left)))
  and
  (((r1.Top >= r2.Top) and (r1.Top <= r2.Bottom)) or ((r1.Bottom >= r2.Top) and (r1.Bottom <= r2.Bottom)) or ((r1.Top <= r2.Top) and (r1.Bottom >= r2.Top)));
end;

function RectEqualRect(const r1 : TRect; const r2 : TRect) : boolean;
begin
  Result := (r1.Top = r2.Top) and (r1.Left = r2.Left) and (r1.Bottom = r2.Bottom) and (r1.Right = r2.Right);
end;
///////////////////////////
//
// TvteXLSRow
//
///////////////////////////
constructor TvteXLSRow.Create;
begin
inherited Create;
Height := XLSDefaultRowHeight;
end;

function TvteXLSRow.GetPixelHeight : integer;
begin
  Result := MulDiv(Height,GetPixelPerInch,(vtePointPerInch*20));
end;

procedure TvteXLSRow.SetPixelHeight(value: integer);
begin
  Height := MulDiv(value,vtePointPerInch*20,GetPixelPerInch);
end;

function TvteXLSRow.GetInchHeight : double;
begin
  Result := Height/(vtePointPerInch*20);
end;

procedure TvteXLSRow.SetInchHeight(value: double);
begin
  Height := Round(value*vtePointPerInch*20);
end;

function TvteXLSRow.GetCentimeterHeight : double;
begin
  Result := InchHeight*vteSmRepInch;
end;

procedure TvteXLSRow.SetCentimeterHeight(value: double);
begin
  InchHeight := value/vteSmRepInch;
end;

function TvteXLSRow.GetExcelHeight : double;
begin
  Result := Height/20;
end;

procedure TvteXLSRow.SetExcelHeight(value: double);
begin
  Height := round(value*20);
end;

////////////////////////////
//
// TvteXLSCol
//
////////////////////////////
constructor TvteXLSCol.Create;
begin
inherited Create;
Width := XLSDefaultColumnWidthInChars*256;
end;

procedure TvteXLSCol.SetWidth(Value : integer);
begin
  FWidth := Min(Value,65280);
end;

function TvteXLSCol.GetPixelWidth : integer;
begin
  Result :=  MulDiv(GetCharacterWidth,Width,256);
end;

procedure TvteXLSCol.SetPixelWidth(value: integer);
begin
  Width := MulDiv(256,Value,GetCharacterWidth);
end;

function TvteXLSCol.GetInchWidth : double;
begin
  Result := GetCharacterWidth*Width/(256*GetPixelPerInch);
end;

procedure TvteXLSCol.SetInchWidth(value: double);
begin
  Width := Round(256*GetPixelPerInch*value/GetCharacterWidth);
end;

function TvteXLSCol.GetCentimeterWidth : double;
begin
  Result := InchWidth*vteSmRepInch;
end;

procedure TvteXLSCol.SetCentimeterWidth(value: double);
begin
  InchWidth := value/vteSmRepInch;
end;

function TvteXLSCol.GetExcelWidth : double;
begin
  if Width >= $1b6 then
    Result := (Width - $b6)/256
  else
    Result := Width/$1b6;
end;

procedure TvteXLSCol.SetExcelWidth(value: double);
begin
  if (value >= 1) then
    Width := round(value * 256) + $b6
  else
    Width := round(value*$1b6);
end;

///////////////////////////
//
// TvteXLSBorder
//
///////////////////////////
constructor TvteXLSBorder.Create;
begin
inherited;
// Init to default values
FLineStyle := vtelsNone;
FWeight := vtexlHairline;
FColor := clBlack;
end;

destructor TvteXLSBorder.Destroy;
begin
inherited;
end;

///////////////////////////
//
// TvteXLSBorders
//
///////////////////////////
constructor TvteXLSBorders.Create;
var
  i : TvteXLSBorderType;
begin
inherited;
for i:=Low(TvteXLSBorderType) to High(TvteXLSBorderType) do
  FBorders[i] := TvteXLSBorder.Create;
end;

destructor TvteXLSBorders.Destroy;
var
  i : TvteXLSBorderType;
begin
for i:=Low(TvteXLSBorderType) to High(TvteXLSBorderType) do
  FBorders[i].Free;
inherited;
end;

function TvteXLSBorders.GetItem;
begin
Result := FBorders[i];
end;

procedure TvteXLSBorders.SetAttributes(
    ABorders : TvteXLSBorderTypes;
    AColor : TColor;
    ALineStyle : TvteXLSLineStyleType;
    AWeight : TvteXLSWeightType);
var
  i : Integer;
begin
  for i := integer(Low(TvteXLSBorderType)) to integer(High(TvteXLSBorderType)) do
    if TvteXLSBorderType(i) in ABorders then
    begin
      with Borders[TvteXLSBorderType(i)] do
      begin
        Color := AColor;
        LineStyle := ALineStyle;
        Weight := AWeight;
      end;
    end;
end;

///////////////////////////
//
// TvteXLSRange
//
//////////////////////////
constructor TvteXLSRange.Create;
begin
inherited Create;
FVerticalAlignment := vtexlVAlignBottom;
FHorizontalAlignment := vtexlHAlignGeneral;
FWorksheet := AWorksheet;
FBorders := TvteXLSBorders.Create;
FFont := TFont.Create;
FFont.Name := vteDefFontFace;
FFont.Size := vteDefFontSize;
FFont.Color := clBlack;
end;

destructor TvteXLSRange.Destroy;
begin
inherited;
FBorders.Free;
FFont.Free;
end;

function TvteXLSRange.GetWorkbook;
begin
Result := nil;
if FWorksheet<>nil then
  Result := FWorksheet.Workbook;
end;

procedure TvteXLSRange.SetValue;
begin
if (VarType(Value)=varOleStr) or (VarType(Value)=varString) then
  FValue := StringReplace(VarToStr(Value),#13#10,#10,[rfReplaceAll])
else
  FValue := Value;
end;

function TvteXLSRange.GetCellDataType;
var
  vt : integer;
begin
if Formula='' then
  begin
    vt := VarType(FValue);
    if (vt=varSmallint) or
       (vt=varInteger) or
       (vt=varSingle) or
       (vt=varDouble) or
       (vt=varCurrency) or
       (vt=varByte) then
      Result := vtecdtNumber
    else
      Result := vtecdtString;
  end
else
  Result := vtecdtFormula;
end;

//////////////////////////////
//
// TvteXLSPageSetup
//
//////////////////////////////
constructor TvteXLSPageSetup.Create;
begin
inherited;
FLeftMargin := 2;
FRightMargin := 2;
FTopMargin := 2.5;
FBottomMargin := 2.5;
FPaperSize := vtexlPaperA4;
FZoom := 100;
FitToPagesTall := 1;
FitToPagesWide := 1;
FirstPageNumber := 1;
FUseScale := True;
end;

/////////////////////////////////////////////////
//
// TvteImage
//
/////////////////////////////////////////////////
constructor TvteImage.Create(_Left,_Top,_Right,_Bottom : integer; _Picture : TPicture; _OwnsImage : boolean); 
begin
inherited Create;
Left := _Left;
Top := _Top;
Right := _Right;
Bottom := _Bottom;
FOwnsImage := _OwnsImage;
FBorderLineColor := clWhite;
FBorderLineStyle := vteblsSolid;
FBorderLineWeight := vteblwHairline;
FScalePercentX := 0;
FScalePercentY := 0;
if FOwnsImage then
  begin
    FPicture := TPicture.Create;
    FPicture.Assign(_Picture);
  end
else
  FPicture := _Picture;
// if FPicture.Bitmap=nil then
//   raise Exception.Create(sErrorInvalidPictureFormat);
end;

constructor TvteImage.CreateWithOffsets(_Left,_LeftCO,_Top,_TopCO,_Right,_RightCO,_Bottom,_BottomCO : integer; _Picture : TPicture; _OwnsImage : boolean);
begin
Create(_Left,_Top,_Right,_Bottom,_Picture,_OwnsImage);
FLeftCO := _LeftCO;
FTopCO := _TopCO;
FRightCO := _RightCO;
FBottomCO := _BottomCO;
FScalePercentX := 0;
FScalePercentY := 0;
end;

constructor TvteImage.CreateScaled(_Left,_LeftCO,_Top,_TopCO,_ScalePercentX,_ScalePercentY : integer; _Picture : TPicture; _OwnsImage : boolean);
begin
CreateWithOffsets(_Left,_LeftCO,_Top,_TopCO,-1,-1,-1,-1,_Picture,_OwnsImage);
FScalePercentX := _ScalePercentX;
FScalePercentY := _ScalePercentY;
end;

destructor TvteImage.Destroy;
begin
if FOwnsImage then
  FPicture.Free;
inherited;
end;

/////////////////////////////////////////////////
//
// TvteImages
//
/////////////////////////////////////////////////
destructor TvteImages.Destroy;
begin
Clear;
inherited;
end;

function TvteImages.GetItm;
begin
Result := TvteImage(inherited Items[i]);
end;

procedure TvteImages.Clear;
var
  i : integer;
begin
for i:=0 to Count-1 do
  Items[i].Free;
inherited;
end;

//////////////////////////////
//
// TvteXLSWorksheet
//
//////////////////////////////
constructor TvteXLSWorksheet.Create;
var
  i,j : integer;
begin
inherited Create;
FDimensions := Rect(-1,-1,-1,-1);
FWorkbook := AWorkbook;
FRanges := TList.Create;
FCols := TList.Create;
FRows := TList.Create;
FPageSetup := TvteXLSPageSetup.Create;
FImages := TvteImages.Create;
FPageBreaks := TList.Create;

i := Workbook.FSheets.Count+1;
while true do
  begin
    j := 0;
    while (j<FWorkbook.FSheets.Count) and
          (AnsiCompareText(TvteXLSWorksheet(FWorkbook.FSheets[j]).Title,
                           sXLSWorksheetTitlePrefix+IntToStr(i))=0) do Inc(j);
    if (j>=FWorkbook.FSheets.Count) or
       (AnsiCompareText(TvteXLSWorksheet(FWorkbook.FSheets[j]).Title,
                        sXLSWorksheetTitlePrefix+IntToStr(i))<>0) then
      break;
    Inc(i);
  end;
Title := sXLSWorksheetTitlePrefix+IntToStr(i);
end;

destructor TvteXLSWorksheet.Destroy;
var
  i : integer;
begin
for i:=0 to FRanges.Count-1 do
  TvteXLSRange(FRanges[i]).Free;
for i:=0 to FCols.Count-1 do
  TvteXLSCol(FCols[i]).Free;
for i:=0 to FRows.Count-1 do
  TvteXLSRow(FRows[i]).Free;
FPageBreaks.Free;
FRanges.Free;
FCols.Free;
FRows.Free;
FPageSetup.Free;
FImages.Free;
inherited;
end;

function TvteXLSWorksheet.GetIndexInWorkBook;
begin
if WorkBook=nil then
  Result := -1
else
  Result := WorkBook.FSheets.IndexOf(Self);
end;

procedure TvteXLSWorksheet.SetTitle;
begin
FTitle := Trim(Copy(Value,1,31));
end;

function TvteXLSWorksheet.GetColByIndex(i : integer) : TvteXLSCol;
begin
Result := TvteXLSCol(FCols[i]);
end;

function TvteXLSWorksheet.GetRowByIndex(i : integer) : TvteXLSRow;
begin
Result := TvteXLSRow(FRows[i]);
end;

function TvteXLSWorksheet.GetColsCount : integer;
begin
Result := FCols.Count;
end;

function TvteXLSWorksheet.GetRowsCount : integer;
begin
Result := FRows.Count;
end;

function TvteXLSWorksheet.GetRangesCount : integer;
begin
Result := FRanges.Count;
end;

function TvteXLSWorksheet.GetXLSRange(RangeIndex : integer) : TvteXLSRange;
begin
Result := TvteXLSRange(FRanges[RangeIndex]);
end;

function TvteXLSWorksheet.GetCol;
begin
Result := FindCol(ColIndex);
if Result=nil then
  Result := AddCol(ColIndex);
end;

function TvteXLSWorksheet.GetRow;
begin
Result := FindRow(RowIndex);
if Result=nil then
  Result := AddRow(RowIndex);
end;

function TvteXLSWorksheet.FindRow;
var
  i : integer;
begin
Result := nil;
for i:=0 to FRows.Count-1 do
  if TvteXLSRow(FRows[i]).Ind=RowIndex then
    begin
      Result := TvteXLSRow(FRows[i]);
      break;
    end;
end;

function TvteXLSWorksheet.AddRow;
begin
Result := TvteXLSRow.Create;
Result.FInd := RowIndex;
FRows.Add(Result);
// change FDimensions
if (FDimensions.Top=-1) or (RowIndex<FDimensions.Top) then
  FDimensions.Top := RowIndex;
if (FDimensions.Bottom=-1) or (RowIndex>FDimensions.Bottom) then
  FDimensions.Bottom := RowIndex;
end;

function TvteXLSWorksheet.FindCol;
var
  i : integer;
begin
Result := nil;
for i:=0 to FCols.Count-1 do
  if TvteXLSCol(FCols[i]).Ind=ColIndex then
    begin
      Result := TvteXLSCol(FCols[i]);
      break;
    end;
end;

function TvteXLSWorksheet.GetDefaultColumnPixelWidth : integer;
begin
  Result :=  GetCharacterWidth*(XLSDefaultColumnWidthInChars+1);
end;

function TvteXLSWorksheet.GetDefaultRowPixelHeight : integer;
begin
  Result := MulDiv(XLSDefaultRowHeight,GetPixelPerInch,(vtePointPerInch*20));
end;

function TvteXLSWorksheet.AddCol;
begin
Result := TvteXLSCol.Create;
Result.Ind := ColIndex;
FCols.Add(Result);
// change FDimensions
if (FDimensions.Left=-1) or (ColIndex<FDimensions.Left) then
  FDimensions.Left := ColIndex;
if (FDimensions.Right=-1) or (ColIndex>FDimensions.Right) then
  FDimensions.Right := ColIndex;
end;

procedure  TvteXLSWorksheet.SetMaxRangeLength(const Value : integer);
begin
  if Value > FMaxRangeLength then
    FMaxRangeLength := Value;
end;

procedure  TvteXLSWorksheet.ResetMaxRangeLength;
var
  i : integer;
begin
  FMaxRangeLength := 1;
  for i := 0 to FRanges.Count-1 do
    SetMaxRangeLength(RangeByIndex[i].Place.Bottom - RangeByIndex[i].Place.Top + 1);
end;

function TvteXLSWorksheet.SeekTop(const Value : integer) : integer;
var
  h_rule,l_rule,m_rule : integer;
begin
  if (FRanges.Count = 0) then
    Result := 0
  else
  if (FRanges.Count = 1) then
  begin
    if (RangeByIndex[0].Place.Top < Value) then
      Result := FRanges.Count
    else
      Result := 0;
  end
  else
  if RangeByIndex[FRanges.Count-1].Place.Top < Value then
      Result := FRanges.Count
  else
  begin
    l_rule := 0;
    h_rule := FRanges.Count - 1;
    repeat
      m_rule:=(l_rule + h_rule) div 2;
      if RangeByIndex[m_rule].Place.Top < Value then
        l_rule := m_rule + 1
      else
        h_rule := l_rule;
    until l_rule = h_rule;
    Result := l_rule;
  end;
end;

function TvteXLSWorksheet.ScanGet(const Index : integer; const R : TRect; var RemoveFlag : boolean) : TvteXLSRange;
var
  pos,i : integer;
  fl_delete, fl_seek : boolean;
begin
  Result := nil;
  fl_seek := false;
  if Index = FRanges.Count then
  begin
    Result := TvteXLSRange.Create(Self);
    Result.FPlace := R;
    FRanges.Add(Result);
  end
  else
  begin
    i := Index;
    pos := Index;
    repeat
      fl_delete := false;
      if RectEqualRect(RangeByIndex[i].Place,R) then
      begin
        Result := RangeByIndex[i];
        fl_seek := true;
      end
      else
      if RectOverRect(RangeByIndex[i].Place,R ) then
      begin
        RangeByIndex[i].Free;
        FRanges.Delete(i);
        RemoveFlag := true;
        fl_delete := true;
      end;
      if not (fl_delete or (RangeByIndex[pos].Place.Top > R.Top)) then
        Inc(pos);
      if not (fl_delete or (RangeByIndex[i].Place.Top > R.Bottom)) then
        Inc(i);
    until (i = FRanges.Count) or (RangeByIndex[i].Place.Top > R.Bottom);
    if Result = nil then
    begin
      Result := TvteXLSRange.Create(Self);
      Result.FPlace := R;
    end;
    if not fl_seek then
    begin
      if pos < FRanges.Count then
        FRanges.Insert(pos,Result)
      else
        FRanges.Add(Result);
    end;
  end;
end;


function TvteXLSWorksheet.GetRangeSp(xl,yt,xr,yb : integer) : TvteXLSRange;
var
  Index : integer;
  RemoveFlag : boolean;
  R : TRect;
begin
  RemoveFlag := false;
  R := Rect(xl,yt,xr,yb);
  Index := SeekTop(yt - FMaxRangeLength + 1);
  if Index = FRanges.Count then
  begin
    // Add range to list
    Result := TvteXLSRange.Create(Self);
    Result.FPlace := R;
    FRanges.Add(Result);
  end
  else
    Result := ScanGet(Index,R,RemoveFlag);
  if RemoveFlag then
    ResetMaxRangeLength
  else
    SetMaxRangeLength(yb-yt+1);

if (FDimensions.Left=-1) or (r.Left<FDimensions.Left) then
  FDimensions.Left := r.Left;
if (FDimensions.Top=-1) or (r.Top<FDimensions.Top) then
  FDimensions.Top := r.Top;
if (FDimensions.Right=-1) or (r.Right>FDimensions.Right) then
  FDimensions.Right := r.Right;
if (FDimensions.Bottom=-1) or (r.Bottom>FDimensions.Bottom) then
  FDimensions.Bottom := r.Bottom;

end;

{function TvteXLSWorksheet.GetRange;
var
  Index : integer;
begin
Result := FindRange(xl,yt,xr,yb);
if Result=nil then
  Result := AddRange(xl,yt,xr,yb); // create range
end;
}

{function TvteXLSWorksheet.FindRange(xl,yt,xr,yb : integer): TvteXLSRange;
var
  i : integer;
begin
Result := nil;
i := 0;
while i<FRanges.Count do
  begin
    Result := TvteXLSRange(FRanges[i]);
    if (Result.Place.Left=xl) and
       (Result.Place.Top=yt) and
       (Result.Place.Right=xr) and
       (Result.Place.Bottom=yb) then break;
    Inc(i);
  end;
if i>=FRanges.Count then
  Result := nil;
end;

function TvteXLSWorksheet.AddRange;
var
  i : integer;
  r : TRect;
  ran : TvteXLSRange;
begin
r := Rect(xl,yt,xr,yb);
i := 0;
while i<FRanges.Count do
  begin
    ran := TvteXLSRange(FRanges[i]);
    if RectOverRect(r,ran.Place) then
      begin
        ran.Free;
        FRanges.Delete(i)
      end
    else
      Inc(i);
  end;
// create range
Result := TvteXLSRange.Create(Self);
Result.FPlace := r;
FRanges.Add(Result);
if (FDimensions.Left=-1) or (r.Left<FDimensions.Left) then
  FDimensions.Left := r.Left;
if (FDimensions.Top=-1) or (r.Top<FDimensions.Top) then
  FDimensions.Top := r.Top;
if (FDimensions.Right=-1) or (r.Right>FDimensions.Right) then
  FDimensions.Right := r.Right;
if (FDimensions.Bottom=-1) or (r.Bottom>FDimensions.Bottom) then
  FDimensions.Bottom := r.Bottom;
end;

}
function TvteXLSWorksheet.AddImage(Left,Top,Right,Bottom : integer; Picture : TPicture; OwnsImage : boolean) : TvteImage;
begin
Result := Images[FImages.Add(TvteImage.Create(Left,Top,Right,Bottom,Picture,OwnsImage))];
end;

function TvteXLSWorksheet.AddImageWithOffsets(Left,LeftCO,Top,TopCO,Right,RightCO,Bottom,BottomCO : integer; Picture : TPicture; OwnsImage : boolean) : TvteImage;
begin
Result := Images[FImages.Add(TvteImage.CreateWithOffsets(Left,LeftCO,Top,TopCO,Right,RightCO,Bottom,BottomCO,Picture,OwnsImage))];
end;

function TvteXLSWorksheet.AddImageScaled(Left,LeftCO,Top,TopCO,ScalePercentX,ScalePercentY : integer; Picture : TPicture; OwnsImage : boolean) : TvteImage;
begin
Result := Images[FImages.Add(TvteImage.CreateScaled(Left,LeftCO,Top,TopCO,ScalePercentX,ScalePercentY,Picture,OwnsImage))];
end;

function TvteXLSWorksheet.GetPageBreak;
begin
Result := integer(FPageBreaks[i]);
end;

function TvteXLSWorksheet.GetPageBreaksCount;
begin
Result := FPageBreaks.Count; 
end;

procedure TvteXLSWorksheet.AddPageBreakAfterRow(RowNumber : integer);
begin
if FPageBreaks.IndexOf(pointer(RowNumber))=-1 then
  FPageBreaks.Add(pointer(RowNumber));
end;

procedure TvteXLSWorksheet.DeletePageBreakAfterRow(RowNumber : integer);
begin
FPageBreaks.Remove(pointer(RowNumber));
end;

function TvteXLSWorksheet.FindPageBreak(RowNumber : integer) : integer;
begin
Result := FPageBreaks.IndexOf(pointer(RowNumber));
end;

////////////////////////////////
//
// TvteXLSWorkbook
//
////////////////////////////////
constructor TvteXLSWorkbook.Create;
begin
inherited;
UserNameOfExcel := 'PReport';
FSheets := TList.Create;
end;

destructor TvteXLSWorkbook.Destroy;
begin
ClearSheets;
FSheets.Free;
inherited;
end;

procedure TvteXLSWorkbook.ClearSheets;
var
  i : integer;
begin
for i:=0 to FSheets.Count-1 do
  TvteXLSWorkSheet(FSheets[i]).Free;
FSheets.Clear;
end;

procedure TvteXLSWorkbook.SetUserNameOfExcel;
begin
FUserNameOfExcel := Trim(Copy(Value,1,66));
end;

function TvteXLSWorkbook.GetSheetsCount : integer;
begin
Result := FSheets.Count;
end;

function TvteXLSWorkbook.GetXLSWorkSheet(i : integer) : TvteXLSWorkSheet;
begin
Result := TvteXLSWorkSheet(FSheets[i]);
end;

procedure TvteXLSWorkbook.SaveAsXLSToFile;
var
  Writer : TvteExcelWriter;
begin
  Writer := TvteExcelWriter.Create;
  try
    Writer.Save(Self,FileName);
  finally
    Writer.Free;
  end;
end;

procedure TvteXLSWorkbook.SaveAsHTMLToFile(const FileName : string);
var
  Writer : TvteHTMLWriter;
begin
  Writer := TvteHTMLWriter.Create;
  try
    Writer.Save(Self, FileName);
  finally
    Writer.Free;
  end;
end;

function TvteXLSWorkbook.AddSheet;
begin
Result := TvteXLSWorkSheet.Create(Self);
FSheets.Add(Result);
end;

function TvteXLSWorkbook.GetSheetIndex(const SheetTitle : string) : integer; // version 1.3
begin
Result := 0;
while (Result<SheetsCount) and (Sheets[Result].Title<>SheetTitle) do Inc(Result);
if Result>=SheetsCount then
  Result := -1;
end;

procedure TvteXLSWorkbook.Clear;
begin
ClearSheets;
end;

///////////////////////////
//
// TvteCustomWriter
//
///////////////////////////
procedure TvteCustomWriter.Save(WorkBook : TvteXLSWorkbook; const FileName : string);
begin
end;

end.

