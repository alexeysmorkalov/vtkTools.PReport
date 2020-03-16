{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the declarations of PReport classes creating the Windows reports.
<ul>
<li>TprReport - Main component. It contains all necessary methods for storing,
building, previewing, printing and exporting Windows reports.</li>
<li>TprPage - Represents the separate page of the report template for the TprReport component.</li>
<li>TprEndPage - Represents the generated page for the TprReport component.</li>
</ul>}
unit pr_Classes;

{$i pr.inc}

{$DEFINE DEBUG}

interface

uses
  SysUtils, Windows, Classes, Graphics, WinSpool, DB,
  Dialogs, typinfo, Math, Forms, IniFiles, Messages, Controls,
  stdctrls, CommDlg, comctrls, ActiveX, ComObj,
  {$IFDEF PR_D6_D7} Variants, Types, {$ENDIF}
  richedit, menus,
  {$IFDEF PR_D5_D6_D7} jpeg, {$ENDIF}
  vgr_GUIFunctions, vgr_Functions,

  Pr_Utils, pr_XLSConts, pr_Common, pr_Progress, pr_MultiLang;

const
{Specifies the offset of the text from left edge for TprMemoObj object.
Syntax:
  oLeft = 1;}
  oLeft = 1; // for TprMemoObj offsets from borders of rect
{Specifies the offset of the text from top edge for TprMemoObj object.
Syntax:
  oTop = 1;}
  oTop = 1;
{Specifies the offset of the text from right edge for TprMemoObj object.
Syntax:
  oRight = 1;}
  oRight = 1;
{Specifies the offset of the text from bottom edge for TprMemoObj object.
Syntax:
  oBottom = 1;}
  oBottom = 1;
{Specifies the line spacing for TprMemoObj object.
Syntax:
  StepLine = 0;}
  StepLine = 0;

{Syntax:
  MillimetersPerInch = 25.4;}
  MillimetersPerInch = 25.4;
{Syntax:
  HundredthMillimetersPerInch = 2540;}
  HundredthMillimetersPerInch = 2540;
{Syntax:
  ThousandsMillimetersPerInch = 25400;}
  ThousandsMillimetersPerInch = 25400;
{Syntax:
  SantimetersPerInch = MillimetersPerInch / 10;}
  SantimetersPerInch = MillimetersPerInch / 10;
{Syntax:
  MetersPerInch = MillimetersPerInch / 1000;}
  MetersPerInch = MillimetersPerInch / 1000;

{Specifies the code of default paper size.
Syntax:
  A4_PaperSizeCode = DMPAPER_A4;}
  A4_PaperSizeCode = DMPAPER_A4;
{Specifies the width (in 1/10 mm) of default paper size.
Syntax:
  A4_PaperWidth = 2100;}
  A4_PaperWidth = 2100;
{Specifies the height (in 1/10 mm) of default paper size.
Syntax:
  A4_PaperHeight = 2970;}
  A4_PaperHeight = 2970;

type

TprPage = class;
TprReport = class;
TprEndPage = class;
TprExObj = class;

{For internal use.
Syntax:
  TprPreviewDrawMode = (dmDraw, dmFind, dmFindFirst);}
TprPreviewDrawMode = (dmDraw, dmFind, dmFindFirst);
{Describes the various units of measurement which are used for defining objects' sizes.
Items:
  prpsuPixels - Pixels.
  prpsuMM - Millimeters.
  prpsuSm - Centimeters.
  prpsuInch - Inches.
  prpsuM - Metres.
Syntax:
  TprPosSizeUnits = (prpsuPixels, prpsuMM, prpsuSm, prpsuInch, prpsuM);}
TprPosSizeUnits = (prpsuPixels, prpsuMM, prpsuSm, prpsuInch, prpsuM);

{For internal use.
Syntax:
  rPrPreviewDrawInfo = record
    DrawMode : TprPreviewDrawMode;
    FindText : string;
    CaseSensitive : boolean;
    FindList : TList;
    StatusBar : TStatusBar;
    ProgressBar : TProgressBar;
  end;}
rPrPreviewDrawInfo = record
  DrawMode : TprPreviewDrawMode;
  FindText : string;
  CaseSensitive : boolean;
  FindList : TList;
  StatusBar : TStatusBar;
  ProgressBar : TProgressBar;
end;
{Pointer to the rPrPreviewDrawInfo structure.
Syntax:
  PPrPreviewDrawInfo = ^rPrPreviewDrawInfo;
See also:
  rPrPreviewDrawInfo}
PPrPreviewDrawInfo = ^rPrPreviewDrawInfo;

{For internal use.
Syntax:
  rPrDrawInfo = record
    kx: Double;
    ky: Double;
    prnkx: Double;
    prnky: Double;
    bRect: TRect;
    IsPrinter: boolean;
    Report: TprReport;
    ppdi: PPrPreviewDrawInfo;
  end;}
rPrDrawInfo = record
  kx: Double;
  ky: Double;
  prnkx: Double;
  prnky: Double;
  bRect: TRect;
  IsPrinter: boolean;
  Report: TprReport;
  ppdi: PPrPreviewDrawInfo;
end;
{Pointer to the rPrDrawInfo structure.
Syntax:
  PPrDrawInfo = ^rPrDrawInfo;
See also:
  rPrDrawInfo}
PPrDrawInfo = ^rPrDrawInfo;

/////////////////////////////////////////////////
//
// TprPageInfo
//
/////////////////////////////////////////////////
{Represents the page's size.}
TprPageInfo = class(TPersistent)
private
  FPageWidth : integer;
  FPageHeight : integer;
  FPaperSize : integer;
  FOrientation : TprPrinterOrientation;
  FlMargin : extended;
  FtMargin : extended;
  FrMargin : extended;
  FbMargin : extended;
  FOnChange : TNotifyEvent;
  procedure SetPageWidth(Value : integer);
  procedure SetPageHeight(Value : integer);
  procedure SetPaperSize(Value : integer);
  procedure SetOrientation(Value : TprPrinterOrientation);
  procedure SetlMargin(Value : extended);
  procedure SettMargin(Value : extended);
  procedure SetrMargin(Value : extended);
  procedure SetbMargin(Value : extended);
  procedure DoChanged;
public
{Specifies the page's width in 1/10 mm.}
  property PageWidth: Integer read FPageWidth write SetPageWidth;
{Specifies the page's height in 1/10 mm.}
  property PageHeight: Integer read FPageHeight write SetPageHeight; 
{Specifies the code of the paper's size for page, for example: DMPAPER_LETTER. DMPAPER_A4 and so on.}
  property PaperSize: Integer read FPaperSize write SetPaperSize;
{Specifies a value indicating the horizontal or vertical orientation of the page.
See also:
  TprPrinterOrientation}
  property Orientation: TprPrinterOrientation read FOrientation write SetOrientation;
{Specifies the left margin in mm.}
  property lMargin: extended read FlMargin write SetlMargin;
{Specifies the top margin in mm.}
  property tMargin: extended read FtMargin write SettMargin;
{Specifies the right margin in mm.}
  property rMargin: extended read FrMargin write SetrMargin;
{Specifies the bottom margin in mm.}
  property bMargin: extended read FbMargin write SetbMargin;
{Occurs when any property is changed.}
  property OnChange: TNotifyEvent read FOnChange write FOnChange;

{Copies the properties from another object.}
  procedure Assign(Source: TPersistent); override;
{Saves the object's properties in the stream.
Parameters:
  Stream - The destination stream.}
  procedure Save(Stream: TStream);
{Loads the object's properties from the stream.
Parameters:
  Stream - The source stream.}
  procedure Load(Stream: TStream);
{Creates an instance of the TprPageInfo class.}
  constructor Create;
end;

{Defines the various modes of changing of the paper's sizes on the stage of report printing.
Items:
  psmToNearest - The nearest paper size is used.
  psmToDefined - The defined size of paper is used.
  psmUserSelected - The end-user can select the paper's size in the built-in dialog window.}
TprPageScaleMode = (psmToNearest, psmToDefined, psmUserSelected);
/////////////////////////////////////////////////
//
// TprPageScaleInfo
//
/////////////////////////////////////////////////
{Defines how the paper's size can be changed on the stage of report printing
if the specified for page the size of paper is not found.
Example:
  // The report template is designed for A3 paper.
  // to correctly print it on the A4 paper you must do:
  var
    APage: TprPage;
  begin
    ...
    APage := AReport.Pages[0];
    APage.PageScaleInfo.FitObjects := True;
    APage.PageScaleInfo.PaperSize := DMPAPER_A4;
    APage.PageScaleInfo.ScaleMode := psmToDefined;
    ...
  end;
See also:
  TprPageInfo}
TprPageScaleInfo = class(TPersistent)
private
  FPaperSize : integer;
  FOrientation : TprPrinterOrientation;
  FPageWidth : integer;
  FPageHeight : integer;
  FScaleMode : TprPageScaleMode;
  FFitObjects : boolean;
public
{Creates an instance of the TprPageScaleInfo class.}
  constructor Create;
{Copies the contents of another, similar object.
Parameters:
  Source - The source object.}
  procedure Assign(Source : TPersistent); override;
{Saves the object's properties in the stream.
Parameters:
  Stream - The destination stream.}
  procedure Save(Stream: TStream);
{Loads the object's properties from the stream.
Parameters:
  Stream - The source stream.}
  procedure Load(Stream: TStream);
published
{Specifies the paper's orientation which will be used if ScaleMode equals to psmToDefined.
See also:
  ScaleMode, TprPrinterOrientation}
  property Orientation: TprPrinterOrientation read FOrientation write FOrientation default poPortrait;
{Specifies the code of the paper's size
(for example: DMPAPER_LETTER. DMPAPER_A4 and so on), which will be used if ScaleMode equals to psmToDefined.
See also:
  ScaleMode}
  property PaperSize: Integer read FPaperSize write FPaperSize default A4_PaperSizeCode;
{Specifies the page's width which will be used if ScaleMode equals to psmToDefined (in 1/10 mm).}
  property PageWidth: Integer read FPageWidth write FPageWidth default A4_PaperWidth;
{Specifies the page's height which will be used if ScaleMode equals to psmToDefined (in 1/10 mm).}
  property PageHeight: Integer read FPageHeight write FPageHeight default A4_PaperHeight;
{Specifies how the paper's size can be changed on the stage of report printing
if the specified for page the size of paper is not found.
See also:
  TprPageScaleMode}
  property ScaleMode: TprPageScaleMode read FScaleMode write FScaleMode default psmToDefined;
{Specifies the value indicating whether the page content must be automatically scaled to
fit in the page rectangle.}
  property FitObjects: Boolean read FFitObjects write FFitObjects default true;
end;

/////////////////////////////////////////////////
//
// TprFindText
//
/////////////////////////////////////////////////
{For internal use.}
TprFindText = class
  TextRect: TRect;
  constructor CreateFT(_TextRect: TRect);
end;

/////////////////////////////////////////////////
//
// TprExObjRecVersion
//
/////////////////////////////////////////////////
{Base class for all objects' views in the TprReport.
See also:
  TprObjRecVersion}
TprExObjRecVersion = class(TprObjRecVersion)
private
  FPreviewUserData: TprPreviewUserData;
protected
  procedure ReadGeneratedRect(Stream : TStream);
  procedure WriteGeneratedRect(Stream : TStream);
  procedure ReadPreviewUserData(Stream : TStream);
  procedure WritePreviewUserData(Stream : TStream);
  procedure DefineProperties(Filer : TFiler); override;
  procedure Scale(cx1, cx2, cy1, cy2 : integer); virtual;
public
{Specifies the coordinates of the object view on the generated page.
Coordinates are specified in pixels and are relative to top-left page corner.}
  GeneratedRect: TRect;

{Returns the scaled coordinates of the object view.
Parameters:
  pdi - Specifies the scaling.
See also:
  pPrDrawInfo, rPrDrawInfo
Return value:
  Returns the TRect structure containing the scaled coordinates of the object view.}
  function GetRealRect(pdi: pPrDrawInfo): TRect;
{Sets the scaled coordinates to the object view.
Parameters:
  Value - TRect structure containing the new scaled coordinates.
  pdi - Specifies the scaling.}
  procedure SetRealRect(const Value: TRect; pdi: pPrDrawInfo);

{Renders the object view on the device context.
This is abstract method, it must be overrided in derived class.
Parameters:
  DC - The device context.
  pdi - Specifies additional information for painting.}
  procedure Draw(DC: HDC; pdi: PPrDrawInfo); virtual; abstract;

{Fills the properties of the object view with default values, it is called
from the preview in design mode, when the end-user adds new object on page.}
  procedure InitInDesigner; virtual; abstract;

{Copies the properties of another object.
Parameters:
  Source - The source object.}
  procedure Assign(Source: TPersistent); override;

{Returns a TprPreviewUserData object that was returned in the OnPreviewGetUserData.
See also:
  TprCustomReport.OnPreviewGetUserData}
  property PreviewUserData: TprPreviewUserData read FPreviewUserData;
end;

/////////////////////////////////////////////////
//
// TprExObjRec
//
/////////////////////////////////////////////////
{Base class for a record of object properties in the TprReport.}
TprExObjRec = class(TprObjRec)
private
  function GetObj: TprExObj;
public
{See:
  TprObjRec.ThirdPass}
  procedure ThirdPass(AEndPage: TprCustomEndPage; Device: TObject; const r: TRect); override;

{Returns reference to an instance of TprExObj class containing this TprExObjRec object.
This reference is passed to the constructor.}
  property Obj: TprExObj read GetObj;
end;

/////////////////////////////////////////////////
//
// TprExObj
//
/////////////////////////////////////////////////
{Base class for all TprPReport objects, such as: TprMemoObj, TprImageObj and so on.}
TprExObj = class(TprObj)
private
  function GetReport: TprReport;
public
{Returns the TprReport component which owns a object.}
  property Report: TprReport read GetReport;
end;

/////////////////////////////////////////////////
//
// TprFrameLine
//
/////////////////////////////////////////////////
{Represents a border of TprMemoObj and TprRichObj objects.}
TprFrameLine = class(TPersistent)
private
  FShow: Boolean;
  FStyle: TPenStyle;
  FColor: TColor;
  FWidth: Integer;
public
{Copies the properties of another object.
Parameters:
  Source - The source object.}
  procedure Assign(Source: TPersistent); override;
published
{Specifies value indicating whether a border is visible.}
  property Show: boolean read FShow write FShow default false;
{Specifies the border's style.}
  property Style: TPenStyle read FStyle write FStyle default psSolid;
{Specifies the border's color.}
  property Color: TColor read FColor write FColor default 0;
{Specifies the border's width in pixels.}
  property Width: integer read FWidth write FWidth default 0;
end;

/////////////////////////////////////////////////
//
// TprMemoObjRecVersion
//
/////////////////////////////////////////////////
{Represents the object view for TprMemoObj.
See also:
  TprMemoObj}
TprMemoObjRecVersion = class(TprExObjRecVersion)
private
  FMemo: TStrings;
  FlBorder: TprFrameLine;
  FrBorder: TprFrameLine;
  FtBorder: TprFrameLine;
  FbBorder: TprFrameLine;
  FFillColor: TColor;
  FhAlign: TprHAlign;
  FvAlign: TprVAlign;
  FFont: TFont;
  FRotate90: boolean;
  FDeleteEmptyLinesAtEnd: boolean;
  FDeleteEmptyLines: boolean;
  FCanResizeX: boolean;
  FCanResizeY: boolean;
  FWordWrap: boolean;
  FJustifyLastLine: Boolean;
  FEolIsEndOfParagraph: Boolean;

  SecondPassNeeded : boolean; // true - in second pass Memo must be reformatted
  procedure SettBorder(Value: TprFrameLine);
  procedure SetlBorder(Value: TprFrameLine);
  procedure SetrBorder(Value: TprFrameLine);
  procedure SetbBorder(Value: TprFrameLine);
  procedure SetMemo(Value: TStrings);
protected
  procedure ReadGeneratedData(Stream : TStream);
  procedure WriteGeneratedData(Stream : TStream);
  procedure ReadFontSize(Reader : TReader);
  procedure WriteFontSize(Writer : TWriter);
  procedure DefineProperties(Filer : TFiler); override;

  procedure CalcLayout(ADC: HDC;
                       pdi: PprDrawInfo;
                       ANeedLinesWidth: Boolean;
                       const AObjectRect: TRect;
                       var ARect: TRect;
                       var AInnerRect: TRect;
                       var ATextHeight: Integer;
                       var ATopLeft: TPoint;
                       var ALeftBorderSize: Integer;
                       var ATopBorderSize: Integer;
                       var ARightBorderSize: Integer;
                       var ABottomBorderSize: Integer;
                       var ALineHeight: Integer;
                       var ALinesWidth: TprDynIntegerArray;
                       var ALines: TvgrWrapLineDynArray);

  procedure InternalDraw(const ARect: TRect; ADC: HDC; pdi: pprDrawInfo; ADesignTime: Boolean);
public
{Creates an instance of the TprMemoObjRecVersion class.}
  constructor Create(Collection: TCollection); override;
{Frees an instance of the TprMemoObjRecVersion class.}
  destructor Destroy; override;

{See:
  TprObjRecVersion.Assign}
  procedure Assign(Source: TPersistent); override;

{See:
  TprExObjRecVersion.Draw}
  procedure Draw(DC: HDC; pdi: PPrDrawInfo); override;
{See:
  TprExObjRecVersion.InitInDesigner}
  procedure InitInDesigner; override;
published
{Specifies the memo's content.}
  property Memo: TStrings read FMemo write SetMemo;
{Specifies the properties of left border.
See also:
  TprFrameLine}
  property lBorder: TprFrameLine read FlBorder write SetlBorder;
{Specifies the properties of right border.
See also:
  TprFrameLine}
  property rBorder: TprFrameLine read FrBorder write SetrBorder;
{Specifies the properties of top border.
See also:
  TprFrameLine}
  property tBorder: TprFrameLine read FtBorder write SettBorder;
{Specifies the properties of bottom border.
See also:
  TprFrameLine}
  property bBorder: TprFrameLine read FbBorder write SetbBorder;
{Specifies the background color. May be clNone, in this case the object is transparent.}
  property FillColor: TColor read FFillColor write FFillColor;
{Specifies the horizontal alignment of the text in a object.
See also:
  TprHAlign}
  property hAlign: TprHAlign read FhAlign write FhAlign default prhLeft;
{Specifies the vertical alignment of the text in a object.
See also:
  TprVAlign}
  property vAlign: TprvAlign read FvAlign write FvAlign;
{Specifies the font.}
  property Font: TFont read FFont write FFont;
{Specifies the value indicating whether the text in the object is rotated on 90 degrees.}
  property Rotate90: Boolean read FRotate90 write FRotate90 default False;
{Specifies the value indicating whether the empty lines of text at the bottom of object must be deleted,
on the stage of generating of the report.}
  property DeleteEmptyLinesAtEnd: Boolean read FDeleteEmptyLinesAtEnd write FDeleteEmptyLinesAtEnd default false;
{Specifies the value indicating whether the empty lines of text must be deleted,
on the stage of generating of the report.}
  property DeleteEmptyLines: Boolean read FDeleteEmptyLines write FDeleteEmptyLines default false;
{Specifies the value indicating whether the object’s width should be automatically calculated based
on the widest string. If this property is true then the object width can be
changed on the stage of generating of report.
This property is not used if the WordWrap property equals to true.}
  property CanResizeX: Boolean read FCanResizeX write FCanResizeX default false;
{Specifies the value indicating whether the object's height should be automatically calculated based on
the count of lines in the Memo property. If this property is true then the object height can be
changed on the stage of generating of report.}
  property CanResizeY: Boolean read FCanResizeY write FCanResizeY default false;
{Specifies the value indicating whether a object automatically wraps words to the beginning
of the next line when necessary.}
  property WordWrap: Boolean read FWordWrap write FWordWrap default false;
{Specifies the value indicating whether the last line of memo must be justified when
hAlign equals prhJustify.
See also:
  EolIsEndOfParagraph, hAlign}
  property JustifyLastLine: Boolean read FJustifyLastLine write FJustifyLastLine default false;
{Specifies the value indicating whether the string ended with CRLF characters should be
processed as end of paragraph - such string are not justified when hAlign equals to prhJustify.
See also:
  JustifyLastLine, hAlign}
  property EolIsEndOfParagraph: Boolean read FEolIsEndOfParagraph write FEolIsEndOfParagraph default true;
end;

/////////////////////////////////////////////////
//
// TprMemoObjRec
//
/////////////////////////////////////////////////
{Represents a record of properties for the TprMemoObj object.
See also:
  TprMemoObj, TprMemoObjRecVersion.}
TprMemoObjRec = class(TprExObjRec)
private
  FCanSplit: Boolean;
protected
  function GetVersionClass: TprObjVersionClass; override;

  function GetSupportSplitting: Boolean; override;
  function GetCanSplitValue: Boolean; override;
  procedure SetCanSplitValue(Value: Boolean); override;
  function GetCanSplit(AByHorizontal: Boolean; ASplitPos: Integer): Boolean; override;
  function Split(AByHorizontal: Boolean; ASplitPos: Integer; var AAddToSplitted: Integer): TprObjRec; override;
public
{See:
  TprObjRec.SecondPass}
  procedure SecondPass; override;
published
{Specifies the value indicating whether the object can split should it fall on a page break.}
  property CanSplit;
end;

/////////////////////////////////////////////////
//
// TprMemoObj
//
/////////////////////////////////////////////////
{"Text" object represents framed rectangle with multiline text inside it.
You can set frame type for each border, color and width; all font attributes,
text align and rotation. In the memo of "Text" object you can place multiline
text with variables, data fields or expressions.
As all other objects in PReport the TprMemoObj can have many views (it has one view after creating)
the necessary  object view is selected on the stage of report generating.
TprMemoObj supports the "inplace editing" at design-time.
See also:
  TprMemoObjRecVersion, TprMemoObjRec}
TprMemoObj = class(TprExObj)
private
  function GetVersion(Index: Integer): TprMemoObjRecVersion;
  function GetGenVersion(Index: Integer): TprMemoObjRecVersion;
  function GetDefVersion: TprMemoObjRecVersion;
  function GetGenCurVersion: TprMemoObjRecVersion;
protected
  procedure InitdRec; override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
public
{See:
  TprObj.DrawDesign}
  procedure DrawDesign(DC: HDC; ExData: Pointer; const DrawRect: TRect); override;
{See:
  TprObj.GetDesc}
  function GetDesc: string; override;

{See:
  TprObj.DsgnIsTransparent}
  function DsgnIsTransparent: Boolean; override;
{See:
  TprObj.DsgnAllowInplaceEdit}
  function DsgnAllowInplaceEdit: boolean; override;
{See:
  TprObj.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: Boolean); override;

{See:
  TprObj.InplaceEdit}
  procedure InplaceEdit(_Parent: TWinControl; var InplaceEditor: TWinControl; const InplaceRect: TRect; ExData: pointer); override;
{See:
  TprObj.SaveInplaceEdit}
  procedure SaveInplaceEdit(InplaceEditor: TWinControl); override;

{See:
  TprObj.FirstPass}
  procedure FirstPass; override;

{See:
  TprObj.Versions}
  property Versions[Index: Integer]: TprMemoObjRecVersion read GetVersion;
{See:
  TprObj.GenVersions}
  property GenVersions[Index: Integer]: TprMemoObjRecVersion read GetGenVersion;
{See:
  TprObj.DefVersion}
  property DefVersion: TprMemoObjRecVersion read GetDefVersion;
{See:
  TprObj.GenCurVersion}
  property GenCurVersion: TprMemoObjRecVersion read GetGenCurVersion;
end;

{Describes the supported RTF versions.}
  TprRtfVersion = (prrv3_09x, prrv3_0, prrv4_1);

/////////////////////////////////////////////////
//
// TprRichObjRecVersion
//
/////////////////////////////////////////////////
{Represents the object view for TprRichObj.
See also:
  TprRichObj}
TprRichObjRecVersion = class(TprExObjRecVersion)
private
  FlBorder: TprFrameLine;
  FrBorder: TprFrameLine;
  FtBorder: TprFrameLine;
  FbBorder: TprFrameLine;
  FDeleteEmptyLinesAtEnd: Boolean;
  FDeleteEmptyLines: Boolean;
  FCanResizeY: Boolean;
  FWordWrap: Boolean;
  FhwndRich: HWND;
  FhwndRichFind: HWND;

  SecondPassNeeded : boolean;

  procedure SettBorder(Value: TprFrameLine);
  procedure SetlBorder(Value: TprFrameLine);
  procedure SetrBorder(Value: TprFrameLine);
  procedure SetbBorder(Value: TprFrameLine);
protected
  procedure ReadRichText(Stream: TStream);
  procedure WriteRichText(Stream: TStream);
  procedure DefineProperties(Filer: TFiler); override;

  function CreateRichEditWindow: HWND;

  procedure InternalDraw(DC: HDC; const ARect: TRect; pdi: PPrDrawInfo);
public
{Creates an instance of the TprRichObjRecVersion class.}
  constructor Create(Collection: TCollection); override;
{Frees an instance of the TprRichObjRecVersion class.}
  destructor Destroy; override;

{See:
  TprExObjRecVersion.Assign}
  procedure Assign(Source: TPersistent); override;

{See:
  TprExObjRecVersion.Draw}
  procedure Draw(DC: HDC; pdi: PPrDrawInfo); override;
{See:
  TprExObjRecVersion.InitInDesigner}
  procedure InitInDesigner; override;
{Returns the object content as plain text.}
  function GetText: string;
{Returns the object content as RTF text.}
  function GetRtf: string;
{Sets the object RTF text.}
  procedure SetRtf(const Value: string);

{Returns a handle to the richedit window.}
  property hwndRich: HWND read FhwndRich;
published
{Specifies the properties of left border.
See also:
  TprFrameLine}
  property lBorder: TprFrameLine read FlBorder write SetlBorder;
{Specifies the properties of right border.
See also:
  TprFrameLine}
  property rBorder: TprFrameLine read FrBorder write SetrBorder;
{Specifies the properties of top border.
See also:
  TprFrameLine}
  property tBorder: TprFrameLine read FtBorder write SettBorder;
{Specifies the properties of bottom border.
See also:
  TprFrameLine}
  property bBorder: TprFrameLine read FbBorder write SetbBorder;
{Specifies the value indicating whether the empty lines of text at the bottom of object must be deleted,
on the stage of generating of the report.}
  property DeleteEmptyLinesAtEnd: boolean read FDeleteEmptyLinesAtEnd write FDeleteEmptyLinesAtEnd default false;
{Specifies the value indicating whether the empty lines of text must be deleted,
on the stage of generating of the report.}
  property DeleteEmptyLines: boolean read FDeleteEmptyLines write FDeleteEmptyLines default false;
{Specifies the value indicating whether the object's height should be automatically calculated based on
the its content. If this property is true then the object height can be
changed on the stage of generating of report.}
  property CanResizeY: boolean read FCanResizeY write FCanResizeY default false;
{Specifies value indicating whether a object automatically wraps words to the beginning
of the next line when necessary.}
  property WordWrap: boolean read FWordWrap write FWordWrap default True;
end;

/////////////////////////////////////////////////
//
// TprRichObjRec
//
/////////////////////////////////////////////////
{Represents a record of properties for the TprRichObjRec object.
See also:
  TprRichObj, TprRichObjRecVersion.}
TprRichObjRec = class(TprExObjRec)
private
  FCanSplit: Boolean;
protected
  function GetVersionClass: TprObjVersionClass; override;
//  function CreateCopy: TprObjRec; override;
public
{See:
  TprObjRec.SecondPass}
  procedure SecondPass; override;

  function GetSupportSplitting: Boolean; override;
  function GetCanSplitValue: Boolean; override;
  procedure SetCanSplitValue(Value: Boolean); override;
  function GetCanSplit(AByHorizontal: Boolean; ASplitPos: Integer): Boolean; override;
  function Split(AByHorizontal: Boolean; ASplitPos: Integer; var AAddToSplitted: Integer): TprObjRec; override;
published
{Specifies the value indicating whether the object can split should it fall on a page break.}
  property CanSplit;
end;

/////////////////////////////////////////////////
//
// TprRichObj
//
/////////////////////////////////////////////////
{Rich text object is intended for inserting RTF text in your report. Supports RTF 1.2.
You can set frame type for each border, in the object's text you can place variables,
data fields or expressions (in square bracets).
As all other objects in PReport the TprRichObj can have many views (it has one view after creating)
the necessary  object view is selected on the stage of report generating.
See also:
  TprRichObjRecVersion}
TprRichObj = class(TprExObj)
private
  function GetVersion(Index: Integer): TprRichObjRecVersion;
  function GetGenVersion(Index: Integer): TprRichObjRecVersion;
  function GetDefVersion: TprRichObjRecVersion;
  function GetGenCurVersion: TprRichObjRecVersion;
protected
  procedure InitdRec; override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
public
{See:
  TprObj.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: boolean); override;

{See:
  TprObj.DrawDesign}
  procedure DrawDesign(DC: HDC; ExData: pointer; const DrawRect: TRect); override;
{See:
  TprObj.GetDesc}
  function GetDesc: string; override;
{See:
  TprObj.FirstPass}
  procedure FirstPass; override;

{See:
  TprObj.Versions}
  property Versions[Index: Integer]: TprRichObjRecVersion read GetVersion;
{See:
  TprObj.GenVersions}
  property GenVersions[Index: Integer]: TprRichObjRecVersion read GetGenVersion;
{See:
  TprObj.DefVersion}
  property DefVersion: TprRichObjRecVersion read GetDefVersion;
{See:
  TprObj.GenCurVersion}
  property GenCurVersion: TprRichObjRecVersion read GetGenCurVersion;
end;

{Describes the source of image for the TprImageObjRecVersion object.
Items:
  isPicture - The object keeps the image in the Picture property.
  isFileName - The object uses as the source of image the file, name of file is stored in the FileName property.
  isDBFieldName - The object uses as the source of image the field of database.}
TprImageSource = (isPicture, isFileName, isDBFieldName);
{Describes the image align.
Items:
  prdmCenter - Specifies the value indicating whether the image is centered in the object.
  prdmStretch - Specifies the value indicating whether the image should be changed so that it exactly fits the bounds of the image control.
  prdmStretchProp - Specifies the value indicating whether the image should be changed, without distortion, so that it fits the bounds of the image control.
  prdmResizeHeightWidth - Specifies the value indicating whether the object sizes itself automatically to accommodate the dimensions of the image.
Syntax:
  TprImageDrawMode = (prdmCenter, prdmStretch, prdmStretchProp, prdmResizeHeightWidth);}
TprImageDrawMode = (prdmCenter, prdmStretch, prdmStretchProp, prdmResizeHeightWidth);
/////////////////////////////////////////////////
//
// TprImageObjRecVersion
//
/////////////////////////////////////////////////
{Represents the object view for TprImageObj.
See also:
  TprImageObj}
TprImageObjRecVersion = class(TprExObjRecVersion)
private
  FImageSource : TprImageSource;
  FPicture : TPicture;
  FFileName : string;
  FDBFieldName : string;
  FDrawMode : TprImageDrawMode;
  FFillColor : TColor;

  procedure SetPicture(Value: TPicture);
protected
  function GetPicture(Report : TprCustomReport): TPersistent;
public
{Creates an instance of the TprImageObjRecVersion class.}
  constructor Create(Collection: TCollection); override;
{Frees an instance of the TprImageObjRecVersion class.}
  destructor Destroy; override;

{See:
  TprExObjRecVersion.Assign}
  procedure Assign(Source : TPersistent); override;

{See:
  TprExObjRecVersion.Draw}
  procedure Draw(DC: HDC; pdi: PPrDrawInfo); override;
{See:
  TprExObjRecVersion.InitInDesigner}
  procedure InitInDesigner; override;
published
{Specifies the source of image for the object.
See also:
  TprImageSource}
  property ImageSource: TprImageSource read FImageSource write FImageSource default isPicture;
{Specifies the image that is used when ImageSource equals to isPicture.
See also:
  ImageSource}
  property Picture: TPicture read FPicture write SetPicture;
{Specifies the name of file that is used when ImageSource equals to isFileName.
See also:
  ImageSource}
  property FileName: string read FFileName write FFileName;
{Specifies the name of database field that is used when ImageSource equals to isDBFieldName.
See also:
  ImageSource}
  property DBFieldName: string read FDBFieldName write FDBFieldName;
{Specifies the image align.
See also:
  TprImageDrawMode}
  property DrawMode: TprImageDrawMode read FDrawMode write FDrawMode default prdmCenter;
{Specifies the background color.}
  property FillColor: TColor read FFillColor write FFillColor;
end;

/////////////////////////////////////////////////
//
// TprImageObjRec
//
/////////////////////////////////////////////////
{Represents a record of properties for the TprImageObjRec object.
See also:
  TprImageObj, TprImageObjRecVersion.}
TprImageObjRec = class(TprExObjRec)
protected
  function GetVersionClass: TprObjVersionClass; override;
//  function  CreateCopy: TprObjRec; override;
public
{See:
  TprObjRec.SecondPass}
  procedure SecondPass; override;
end;

/////////////////////////////////////////////////
//
// TprImageObj
//
/////////////////////////////////////////////////
{TprImageObj object is intended for inserting pictures (BMP/WMF/ICO graphic files) in your report.
As all other objects in PReport the TprImageObj can have many views (it has one view after creating)
the necessary  object view is selected on the stage of report generating.
See also:
  TprImageObjRecVersion}
TprImageObj = class(TprExObj)
private
  function GetVersion(Index: Integer): TprImageObjRecVersion;
  function GetGenVersion(Index: Integer): TprImageObjRecVersion;
  function GetDefVersion: TprImageObjRecVersion;
  function GetGenCurVersion: TprImageObjRecVersion;
protected
  procedure InitdRec; override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
public
{See:
  TprObj.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean); override;

{See:
  TprObj.DrawDesign}
  procedure DrawDesign(DC: HDC; ExData: Pointer; const DrawRect: TRect); override;
{See:
  TprObj.GetDesc}
  function GetDesc: string; override;
{See:
  TprObj.FirstPass}
  procedure FirstPass; override;

{See:
  TprObj.Versions}
  property Versions[Index: Integer]: TprImageObjRecVersion read GetVersion;
{See:
  TprObj.GenVersions}
  property GenVersions[Index: Integer]: TprImageObjRecVersion read GetGenVersion;
{See:
  TprObj.DefVersion}
  property DefVersion: TprImageObjRecVersion read GetDefVersion;
{See:
  TprObj.GenCurVersion}
  property GenCurVersion: TprImageObjRecVersion read GetGenCurVersion;
end;


/////////////////////////////////////////////////
//
// TprHTitleBand
//
/////////////////////////////////////////////////
{Implements the horizontal title band in TprReport.}
TprHTitleBand = class(TprCustomHTitleBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprHSummaryBand
//
/////////////////////////////////////////////////
{Implements the horizontal summary band in TprReport.}
TprHSummaryBand = class(TprCustomHSummaryBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprHPageHeaderBand
//
/////////////////////////////////////////////////
{Implements the horizontal page header band in TprReport.}
TprHPageHeaderBand = class(TprCustomHPageHeaderBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprHPageFooterBand
//
/////////////////////////////////////////////////
{Implements the horizontal page footer band in TprReport.}
TprHPageFooterBand = class(TprCustomHPageFooterBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprHDetailBand
//
/////////////////////////////////////////////////
{Implements the horizontal detail band in TprReport.}
TprHDetailBand = class(TprCustomHDetailBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprHDetailHeaderBand
//
/////////////////////////////////////////////////
{Implements the horizontal detail header band in TprReport.}
TprHDetailHeaderBand = class(TprCustomHDetailHeaderBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprHDetailFooterBand
//
/////////////////////////////////////////////////
{Implements the horizontal detail footer band in TprReport.}
TprHDetailFooterBand = class(TprCustomHDetailFooterBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprHGroupHeaderBand
//
/////////////////////////////////////////////////
{Implements the horizontal group header band in TprReport.}
TprHGroupHeaderBand = class(TprCustomHGroupHeaderBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprHGroupFooterBand
//
/////////////////////////////////////////////////
{Implements the horizontal group footer band in TprReport.}
TprHGroupFooterBand = class(TprCustomHGroupFooterBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;







/////////////////////////////////////////////////
//
// TprVTitleBand
//
/////////////////////////////////////////////////
{Implements the vertical title band in TprReport.}
TprVTitleBand = class(TprCustomVTitleBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprVSummaryBand
//
/////////////////////////////////////////////////
{Implements the vertical summary band in TprReport.}
TprVSummaryBand = class(TprCustomVSummaryBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprVPageHeaderBand
//
/////////////////////////////////////////////////
{Implements the vertical page header band in TprReport.}
TprVPageHeaderBand = class(TprCustomVPageHeaderBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprVPageFooterBand
//
/////////////////////////////////////////////////
{Implements the vertical page footer band in TprReport.}
TprVPageFooterBand = class(TprCustomVPageFooterBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprVDetailBand
//
/////////////////////////////////////////////////
{Implements the horizontal page footer band in TprReport.}
TprVDetailBand = class(TprCustomVDetailBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprVDetailHeaderBand
//
/////////////////////////////////////////////////
{Implements the vertical detail band in TprReport.}
TprVDetailHeaderBand = class(TprCustomVDetailHeaderBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprVDetailFooterBand
//
/////////////////////////////////////////////////
{Implements the vertical detail header band in TprReport.}
TprVDetailFooterBand = class(TprCustomVDetailFooterBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprVGroupHeaderBand
//
/////////////////////////////////////////////////
{Implements the vertical group header band in TprReport.}
TprVGroupHeaderBand = class(TprCustomVGroupHeaderBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////////////////////////////
//
// TprVGroupFooterBand
//
/////////////////////////////////////////////////
{Implements the vertical group footer band in TprReport.}
TprVGroupFooterBand = class(TprCustomVGroupFooterBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;








/////////////////////////////////////////////////
//
// TprPage
//
/////////////////////////////////////////////////
{Represents the separate page of the TprReport report template.
See also:
  TprReport}
TprPage = class(TprCustomPage)
private
  FPageInfo : TprPageInfo;
  FPageScaleInfo : TprPageScaleInfo;

  FPixelPageWidth : integer;
  FPixelPageHeight : integer;
  FPixelPageRect : TRect;

  FGenPixelPageRect : TRect;

  FDsgnWidthDelta : integer;
  FDsgnHeightDelta : integer;
  procedure OnPageInfoChanged(Sender : TObject);
  procedure UpdatePixelSizes;
  function StoredDsgnWidth : boolean;
  function StoredDsgnHeight : boolean;
protected
  function GetPaperSize : integer;
  procedure SetPaperSize(Value : integer);
  function GetOrientation : TprPrinterOrientation;
  procedure SetOrientation(Value : TprPrinterOrientation);
  function GetlMargin : extended;
  procedure SetlMargin(Value : extended);
  function GettMargin : extended;
  procedure SettMargin(Value : extended);
  function GetrMargin : extended;
  procedure SetrMargin(Value : extended);
  function GetbMargin : extended;
  procedure SetbMargin(Value : extended);
  procedure SetPageScaleInfo(Value : TprPageScaleInfo);
  function GetWidth : integer;
  procedure SetWidth(Value : integer);
  function GetHeight : integer;
  procedure SetHeight(Value : integer);
  function GetDsgnWidth : integer;
  procedure SetDsgnWidth(Value : integer);
  function GetDsgnHeight : integer;
  procedure SetDsgnHeight(Value : integer);
  procedure Loaded; override;
  procedure ReportSetted; override;

  function DsgnPageRect: TRect; override;
  function GenPageRect: TRect; override;
public
{Specifies the size of the page.
See also:
  TprPageInfo}
  property PageInfo: TprPageInfo read FPageInfo;
{Returns the page's width in pixels.}
  property PixelPageWidth: integer read FPixelPageWidth;
{Returns the page's height in pixels.}
  property PixelPageHeight: integer read FPixelPageHeight;
{Creates an instance of the TprPage class.
Parameters:
  AOwner - The component - owner.}
  constructor Create(AOwner : TComponent); override;
{Frees an instance of the TprPage class.}
  destructor Destroy; override;
published
{Specifies the page's width in 1/10 mm.}
  property Width: Integer read GetWidth write SetWidth;
{Specifies the page's height in 1/10 mm.}
  property Height: Integer read GetHeight write SetHeight;
{Specifies the code of the paper's size for page, for example: DMPAPER_LETTER. DMPAPER_A4 and so on.}
  property PaperSize: Integer read GetPaperSize write SetPaperSize;
{Specifies a value indicating the horizontal or vertical orientation of the page.
See also:
  TprPrinterOrientation}
  property Orientation: TprPrinterOrientation read GetOrientation write SetOrientation;
{Specifies the left margin in mm.}
  property lMargin: Extended read GetlMargin write SetlMargin;
{Specifies the right margin in mm.}
  property rMargin: Extended read GetrMargin write SetrMargin;
{Specifies the top margin in mm.}
  property tMargin: Extended read GettMargin write SettMargin;
{Specifies the bottom margin in mm.}
  property bMargin: Extended read GetbMargin write SetbMargin;
{Defines how the paper's size can be changed on the stage of report printing
if the specified for page the size of paper is not found.}
  property PageScaleInfo: TprPageScaleInfo read FPageScaleInfo write SetPageScaleInfo;
{Specifies the page's width in 1/10 mm in the design mode.
This property equals to Width property by default.
Use this property if you want to create the report template which is not fitted in the default page width at design-time.}
  property DsgnWidth: Integer read GetDsgnWidth write SetDsgnWidth stored StoredDsgnWidth;
{Specifies the page's height in 1/10 mm in the design mode.
This property equals to Height property by default.
Use this property if you want to create the report template which is not fitted in the default page height at design-time.}
  property DsgnHeight: Integer read GetDsgnHeight write SetDsgnHeight stored StoredDsgnHeight;
end;

/////////////////////////////////////////////////
//
// TprEndPage
//
/////////////////////////////////////////////////
{Represents the separate page of the generated TprReport report.}
TprEndPage = class(TprCustomEndPage)
private
  FPageInfo : TprPageInfo;
  FPageScaleInfo : TprPageScaleInfo;
  FVL : TList;
  FPixelPageWidth : integer;
  FPixelPageHeight : integer;
  FPixelPageRect : TRect;
  procedure ClearVL;
  procedure OnPageInfoChanged(Sender : TObject);
  function GetReport: TprReport;
protected
  function GetWidth : integer; override;
  function GetHeight : integer; override;

  procedure Scale(cx1, cx2, cy1, cy2: Integer);

  procedure ThirdPass; override;
public
{Returns the TprReport object containg this object.
See also:
  TprCustomReport}
  property Report: TprReport read GetReport;
{Specifies the size of the page.
This property is copied from the TprPage object, which generates this page.
See also:
  TprPageInfo, TprPage.PageInfo}
  property PageInfo: TprPageInfo read FPageInfo;
{Defines how the paper's size can be changed on the stage of report printing
if the specified for page the size of paper is not found.
This property is copied from the TprPage object, which generates this page.
See also:
  TprPageScaleInfo, TprPage.PageScaleInfo}
  property PageScaleInfo: TprPageScaleInfo read FPageScaleInfo;
{This list contains all objects on the generated page.
Each element of the VL list is the instance of the TprExObjRecVersion class.}
  property VL: TList read FVL;
{Returns the width of the generated page in pixels.}
  property PixelPageWidth : integer read FPixelPageWidth;
{Returns the height of the generated page in pixels.}
  property PixelPageHeight : integer read FPixelPageHeight;
{Returns the page's rectangle in pixels, this property
equals to Rect(0, 0, PixelPageWidth, PixelPageHeight)}
  property PixelPageRect : TRect read FPixelPageRect;

{Saves the page's content in the stream.
Parameters:
  Stream - The destination stream.}
  procedure Save(Stream: TStream);
{Loads the page's content from the stream.
Parameters:
  Stream - The source stream.}
  procedure Load(Stream: TStream);

{Creates an instance of the TprEndPage class.
Parameters:
  _Page - The TprPage object on the basis of which this page is generated.}
  constructor Create(_Page: TprCustomPage); override;
{Creates an instance of the TprEndPage class.
Parameters:
  _Report - The TprReport object containing this page.}
  constructor CreateEmpty(_Report: TprCustomReport); override;
{Frees an instance of the TprEndPage class.}
  destructor Destroy; override;
end;

TArrayWord = array [0..16383] of Word;
PArrayWord = ^TArrayWord;
TArrayTPoint = array [0..16383] of TPoint;
PArrayTPoint = ^TArrayTPoint;

/////////////////////////////////////////////////
//
// TprPrinter
//
/////////////////////////////////////////////////
{TprReport object manages the printing.
Instance of this class is created by TprReport object.
See also:
  TprReport}
TprPrinter = class(TObject)
private
  FPrinters : TStringList;
  FPaperNames : TStringList;
  FPrinterIndex : Integer;
  FPageInfo : TprPageInfo;

  FInfoInitializated : boolean;
  FStructuresInitializated : boolean;

  FPixelPageWidth : integer;
  FPixelPageHeight : integer;
  FPixelPageRect : TRect;
  FPaperSizes : PArrayWord;
  FPaperSizesCount : integer;
  FPaperDimensions : PArrayTPoint;
  FPaperDimensionsCount : integer;

  FPrinterDC : HDC;
  FTitle : string;

  FDeviceName : string;
  FDriverName : string;
  FPortName : string;
  FDevNames : PDevNames;
  FDevMode : PDevMode;
  FDevNamesSize : integer;
  FDevModeSize : integer;

  FmmlMargin : integer;
  FmmtMargin : integer;
  FmmrMargin : integer;
  FmmbMargin : integer;

  FmmMaxPageWidth : integer;
  FmmMaxPageHeight : integer;

  FPixelsPerX : integer;
  FPixelsPerY : integer;
  FscrPixelsPerX : integer;
  FscrPixelsPerY : integer;
  FCreatedscrPixelsPerX : integer;
  FCreatedscrPixelsPerY : integer;

  procedure UpdatePrintersList;
  procedure SetPrinterIndex(Value : integer);
  procedure SetPrinterName(Value : string);
  function  GetPrinterName : string;
  procedure ClearStructures;
  procedure ClearInfo;
  function GetPaperSize(PaperSizeIndex : integer) : integer;
  function GetPaperWidth(PaperSize : integer) : integer;
  function GetPaperHeight(PaperSize : integer) : integer;
  function GetPaperSizes(PaperSize : integer) : TPoint;
public
{The printer's device context, this property has valid value after call of BeginDoc method.}
  property PrinterDC: HDC read FPrinterDC;
{Specifies the title of the printing job.}
  property Title: string read FTitle write FTitle;

{Returns the device name for the currently selected printer.
See also:
  PrinterName}
  property DeviceName: string read FDeviceName;
{Returns the driver name for the currently selected printer.
See also:
  PrinterName}
  property DriverName: string read FDriverName;
{Returns the port name for the currently selected printer.
See also:
  PrinterName}
  property PortName: string read FPortName;

{Number of pixels per logical inch along the width.}
  property PixelsPerX: Integer read FPixelsPerX;
{Number of pixels per logical inch along the height.}
  property PixelsPerY: Integer read FPixelsPerY;
{Number of pixels per logical inch along the screen width.
In a system with multiple display monitors, this value is the same for all monitors.}
  property scrPixelsPerX: Integer read FscrPixelsPerX;
{Number of pixels per logical inch along the screen height.
In a system with multiple display monitors, this value is the same for all monitors.}
  property scrPixelsPerY: Integer read FscrPixelsPerY;
{Specifies the value of the scrPixelsPerX property which was used when report template is created.}
  property CreatedscrPixelsPerX: Integer read FCreatedscrPixelsPerX;
{Specifies the value of the scrPixelsPerY property which was used when report template is created.}
  property CreatedscrPixelsPerY: Integer read FCreatedscrPixelsPerY;

{Specifies the page's size which is installed for printer.}
  property PageInfo: TprPageInfo read FPageInfo;
{Returns the page's width in pixels.}
  property PixelPageWidth: Integer read FPixelPageWidth;
{Returns the page's height in pixels.}
  property PixelPageHeight: Integer read FPixelPageHeight;
{Returns the page's rectangle in pixels.}
  property PixelPageRect: TRect read FPixelPageRect;

{Specifies the minimal left margin for printer in mm.}
  property mmlMargin: Integer read FmmlMargin;
{Specifies the minimal top margin for printer in mm.}
  property mmtMargin: Integer read FmmtMargin;
{Specifies the minimal right margin for printer in mm.}
  property mmrMargin: Integer read FmmrMargin;
{Specifies the minimal bottom margin for printer in mm.}
  property mmbMargin: Integer read FmmbMargin;

{Returns the maximum page width which is supported by current printer.}
  property mmMaxPageWidth: integer read FmmMaxPageWidth;
{Returns the maximum page height which is supported by current printer.}
  property mmMaxPageHeight: integer read FmmMaxPageHeight;

{Returns the TStringList object containing the paper sizes that are supported by the current printer.}
  property PaperNames: TStringList read FPaperNames;
{Lists the codes of the paper sizes which are supported by the current printer.}
  property PaperSizes[PaperSizeIndex: Integer] : integer read GetPaperSize;
{Returns an amount of the codes of the paper sizes which are supported by the default printer.}
  property PaperSizesCount: Integer read FPaperSizesCount;
{Returns the width (in 1/10 mm) for the specified code of the paper's size.}
  property PaperWidths[PaperSize: Integer]: Integer read GetPaperWidth;
{Returns the height (in 1/10 mm) for the specified code of the paper's size.}
  property PaperHeights[PaperSize: Integer]: Integer read GetPaperHEight;

{Lists all printers installed in Windows.}
  property Printers: TStringList read FPrinters;
{Specifies the index of current printer in the Printers property.}
  property PrinterIndex: Integer read FPrinterIndex write SetPrinterIndex;
{Specifies the name of the current printer.}
  property PrinterName: string read GetPrinterName write SetPrinterName;

{Fills the internal fields of the TprPrinter object with values from
current printer.
Return value:
  Returns the true value if printer is opened successfully.}
  function InitStructures: Boolean;
{Fills the internal fields of the TprPrinter object with values from
current printer.
Return value:
  Returns the true value if printer is opened successfully.}
  function InitInfo: Boolean;
{Sets the default windows printer as current printer.}
  procedure SetToDefaultPrinter;

{Sets the paper size of the current printer.
Parameters:
  _PaperSize - The code of the paper size (DMPAPER_LETER, DMPAPER_A4 and so on).
  _Orientation - The paper's orientation.
  _PageWidth - The paper's width in 1/10 mm, this parameter is used if the _PaperSize equals to -1.
  _PageHeight - The paper's height in 1/10 mm, this parameter is used if the _PaperSize equals to -1.
Return value:
  Returns the true value if the specified paper size is supported by the printer
and installed successfully.}
  function SetPageSize(_PaperSize: Integer; _Orientation: TprPrinterOrientation; _PageWidth, _PageHeight: Integer): Boolean;

{Initializes the current printer, raizes an exception if printer failed to initialize.}
  procedure CheckPrinter;
{Starts a new print job.}
  procedure BeginDoc;
{Ends the print job.}
  procedure EndDoc;
{Starts a new page.}
  procedure NewPage;

{Specifies a printer's driver parameters.
Parameters:
  NewDevMode - Pointer to the DEVMODE structure containing the device-specific initialization data for the device driver.
  NewDevModeSize - Size of the DEVMODE structure.
Return value:
  Returns the true value if the parameters of the printer's driver are installed successfully.}
  function SetDevMode(NewDevMode: PDevMode; NewDevModeSize: Integer): Boolean;
{Returns the index of the specified code of the paper's size in the PaperSizes property.
Parameters:
  PaperSize - The code of the paper's size (DMPAPER_A4, DMPAPER_LETER).}
  function GetPaperSizeIndex(PaperSize: Integer): Integer;
{Searches the predefined size of paper (code of which exists in the PaperSizes property)
by its width and height.
Parameters:
  PaperWidth - The paper's width in 1/10 mm.
  PaperHeight - The paper's height in 1/10 mm.
  PaperSize - Contains code of the paper's size if it is found.
  PaperOrientation - Contains the paper's orientation.
  fFindInMM - Specifies the value indicating whether the paper's sizes (PaperWidth and PaperHeight)
must be rounded the the millimeters.
Return value:
  Returns the true if the predefined paper size is found.}
  function FindPaperSize(PaperWidth: Integer; PaperHeight: Integer; var PaperSize: Integer; var PaperOrientation: TprPrinterOrientation; fFindInMM: Boolean): Boolean;

{Creates an instance of the TprPrinter class.}
  constructor Create;
{Frees an instance of the TprPrinter class.}
  destructor Destroy; override;
end;

{Describes the options of the built-in preview window.
Items:
  prpoShowMenu - The main menu is visible.
  prpoAllowShowHideToolbars - Allows the showing / hiding of toolbars on the preview form.
  prpoAllowDragToolbars - Allows the toolbars dragging.
  prpoAllowChangePreviewMode - Allows the preview mode changing, the user can switch between
modes of viewing or editing.
Syntax:
  TprPreviewOptions = (prpoShowMenu, prpoAllowShowHideToolbars, prpoAllowDragToolbars, prpoAllowChangePreviewMode);}
TprPreviewOptions = (prpoShowMenu, prpoAllowShowHideToolbars, prpoAllowDragToolbars, prpoAllowChangePreviewMode);

{Describes the options of the built-in preview window.
Syntax:
  TprPreviewOptionsSet = set of TprPreviewOptions;
See also:
  TprPreviewOptions}
TprPreviewOptionsSet = set of TprPreviewOptions;

{Describes the preview toolbars.
Syntax:
  TprPreviewToolbars = (prptPreviewCommon,prptEdit,prptInsertObject,prptText,prptBorders,prptAlign,prptSize,prptNudge,prptObjects,prptObject);}
TprPreviewToolbars = (prptPreviewCommon,prptEdit,prptInsertObject,prptText,prptBorders,prptAlign,prptSize,prptNudge,prptObjects,prptObject);

{Describes the preview toolbars.
Syntax:
  TprPreviewToolbarsSet = set of TprPreviewToolbars;
See also:
  TprPreviewToolbars}
TprPreviewToolbarsSet = set of TprPreviewToolbars;
/////////////////////////////////////////////////
//
// TprPreviewParams
//
/////////////////////////////////////////////////
{Describes the options for built-in preview window.
See also:
  TprPreviewOptionsSet, TprPreviewToolbarsSet}
TprPreviewParams = class(TPersistent)
private
  FOptions: TprPreviewOptionsSet;
  FShowToolbars : TprPreviewToolbarsSet;
public
{Creates an instance of the TprPreviewParams class.}
  constructor Create;
published
{Specifies the set of actions which are available for user.
See also:
  TprPreviewOptionsSet}
  property Options: TprPreviewOptionsSet read FOptions write FOptions default [];
{Specifies the set of visible toolbars.
See also:
  TprPreviewToolbarsSet}
  property ShowToolbars: TprPreviewToolbarsSet read FShowToolbars write FShowToolbars default [prptPreviewCommon];
end;

/////////////////////////////////////////////////
//
// TprReport
//
/////////////////////////////////////////////////
{TprReport component is PReport component creating the Windows reports.
It contains all necessary methods for storing, building, previewing, printing and exporting reports.
Through Pages property you can access to the pages of the report template.
EndPages property allows you to manipulate with pages of prepared report.
LoadTemplate, SaveTemplate, LoadTemplateFromFile, SaveTemplateToFile, methods allows you to store and retrieve the report template.
LoadPreparedReport, SavePreparedReport, LoadPreparedReportFromFile, SavePreparedReportToFile methods
allows you to store and retrive the prepared report.
DesignReport method runs report designer, if it included in application.
PrepareReport method starts report building.
PreviewPreparedReport method opens the built-in preview window.
PrintPreparedReport method prints report.
ExportTo method exports it to export filter.}
TprReport = class(TprCustomReport)
private
  FPreparedReportFormatVersion : integer;
  FprPrinter : TprPrinter;
  FExportPrecision : integer;
  FExportPrecisionLow : integer;
  FExportPrecisionNormal : integer;
  FExportPrecisionHigh : integer;
  FExportDataAsStrings: Boolean;
  FPreviewParams : TprPreviewParams;
  FUseMMWhenCheckPaperSizes: Boolean;
  FOnPreviewModeChanged : TNotifyEvent;

  function GetPage(Index: Integer): TprPage;
  function GetEndPage(Index: Integer): TprEndPage;
protected
  function GetDesignerFormClass : string; override;
  function GetPreviewFormClass : string; override;
  function GetPreparedReportEmpty : boolean; override;
  procedure InternalLoadPreparedReport(Stream : TStream);
  procedure ReadLOGPIXELSX(Reader : TReader);
  procedure WriteLOGPIXELSX(Writer : TWriter);
  procedure ReadLOGPIXELSY(Reader : TReader);
  procedure WriteLOGPIXELSY(Writer : TWriter);
  procedure DefineProperties(Filer : TFiler); override;
  procedure Loaded; override;
  function CreatePage : TprCustomPage; override;

  function GetPrinterName : string; override;
  procedure SetPrinterName(Value : string); override;

  function CreateEndPage(Page : TprCustomPage) : TprCustomEndPage; override;
  function CreateEmptyEndPage : TprCustomEndPage; override;
public
{Returns the TprPrinter object created by the TprReport component for managing the printing.
See also:
  TprPrinter}
  property prPrinter: TprPrinter read FprPrinter;
{Lists the pages of the report template.
See also:
  TprPage, PagesCount}
  property Pages[index: integer]: TprPage read GetPage;
{Lists the generated pages of the report.
See also:
  TprEndPage, EndPagesCount}
  property EndPages[index: integer]: TprEndPage read GetEndPage;

{See:
  TprCustomReport.GetBandClass}
  function GetBandClass(BandType : TprBandType) : TprBandClass; override;

{See:
  TprCustomReport.SetupPrintParams}
  function SetupPrintParams: Boolean; override;
{See:
  TprCustomReport.PrintPreparedReport}
  function PrintPreparedReport: Boolean; override;

{See:
  TprCustomReport.LoadPreparedReport}
  procedure LoadPreparedReport(Stream: TStream); override;
{See:
  TprCustomReport.AppendPreparedReport}
  procedure AppendPreparedReport(Stream: TStream); override;
{See:
  TprCustomReport.SavePreparedReport}
  procedure SavePreparedReport(Stream: TStream); override;

{See:
  TprCustomReport.SetupExportParams}
  function SetupExportParams: Boolean; override;
{See:
  TprCustomReport.ExportTo}
  procedure ExportTo; override;

{Creates an instance of the TprReport class.
Parameters:
  AOwner - The component - owner.}
  constructor Create(AOwner: TComponent); override;
{Frees an instance of the TprReport class.}
  destructor Destroy; override;
published
{Specifies the export precision.}
  property ExportPrecision: Integer read FExportPrecision write FExportPrecision default 1;
{Specifies the value of the ExportPrecision property that will be used if the user
will select "Low" export precision in the built-in dialog of the export parameters.}
  property ExportPrecisionLow: Integer read FExportPrecisionLow write FExportPrecisionLow default 10;
{Specifies the value of the ExportPrecision property that will be used if the user
will select "Normal" export precision in the built-in dialog of the export parameters.}
  property ExportPrecisionNormal: Integer read FExportPrecisionNormal write FExportPrecisionNormal default 5;
{Specifies the value of the ExportPrecision property that will be used if the user
will select "High" export precision in the built-in dialog of the export parameters.}
  property ExportPrecisionHigh: Integer read FExportPrecisionHigh write FExportPrecisionHigh default 1;
{Specifies the value indicating whether the objects' text should be exported without converting.
If this property equals to false the objects' text will be converted to the suitable value (to numbers, to dates).
This property is used only in Excel export.}
  property ExportDataAsStrings: Boolean read FExportDataAsStrings write FExportDataAsStrings default false;

{Specifies the parameters for built-in preview window.
See also:
  TprPreviewParams}
  property PreviewParams : TprPreviewParams read FPreviewParams write FPreviewParams;

  property UseMMWhenCheckPaperSizes: Boolean read FUseMMWhenCheckPaperSizes write FUseMMWhenCheckPaperSizes default True; 

{This event occurs when user change current work mode of the preview window (editing / viewing only).}
  property OnPreviewModeChanged: TNotifyEvent read FOnPreviewModeChanged write FOnPreviewModeChanged;
end;

{Returns the width of the border in pixels, returns 0 if border is not visible.}
function GetBW(b: TprFrameLine): Integer;
{Converts the value in pixels to the specified units and returns it as string.
Parameters:
  Value - The value in pixels.
  ToU - The destination units.
  IsHor - Indicates whether the value measures the horizontal dimension.}
function prConvertFromPixelsString(Value : integer; ToU : TprPosSizeUnits; IsHor : boolean) : string;
{Converts the value in pixels to the specified units.
Parameters:
  Value - The value in pixels.
  ToU - The destination units.
  IsHor - Indicates whether the value measures the horizontal dimension.}
function prConvertFromPixels(Value : integer; ToU : TprPosSizeUnits; IsHor : boolean) : double;
{Converts the value in the specified units to pixels.
Parameters:
  Value - The value in specified units.
  FromU - The source units.
  IsHor - Indicates whether the value measures the horizontal dimension.}
function prConvertToPixels(Value : double; FromU : TprPosSizeUnits; IsHor : boolean) : integer;

function GetRichTextRect(APixelsRect: TRect; ADpiX, ADpiY: Integer; AWordWrap, AIgnoreBottomBound: Boolean): TRect;
function FormatRichText(Report: TprCustomReport; hwnd: HWND; DeleteEmptyLines, DeleteEmptyLinesAtEnd: Boolean): Boolean;
procedure CopyRichText(hwndSource, hwndDest: HWND); overload;
procedure CopyRichText(hwndSource: HWND; RichEditDest: TRichEdit); overload;
procedure CopyRichText(RichEditSource: TRichEdit; hwndDest: HWND); overload;
{Draws the rich text on the specified device context.
Parameters:
  ADC - The device context.
  hwnd - The handle of rich edit window.
  ARect - The bounds rectangle.
  AWordWrap - Indicates whether the text must be wrapped.
  AIgnoreBottomBound - Indicates whether the APixelsRect.Bottom must be ignored when rich text is formatted.}
procedure DrawRichText(ADC: HDC; hwnd: HWND; const APixelsRect: TRect; AWordWrap: Boolean; AIgnoreBottomBound: Boolean);
procedure MeasureRichText(ADC: HDC;
                          hwnd: HWND;
                          const APixelsRect: TRect;
                          AWordWrap: Boolean;
                          AIgnoreBottomBound: Boolean;
                          var AMeasuredPixelsRect: TRect;
                          var AFittedChars: Integer;
                          var AAllCharsFitted: Boolean);

procedure SaveRichTextToStream(hwnd: HWND; AStream: TStream; var ARichTextLength: Integer; AFlags: Integer); overload;
procedure SaveRichTextToStream(hwnd: HWND; AStream: TStream; var ARichTextLength: Integer); overload;
procedure LoadRichTextFromStream(hwnd: HWND; AStream: TStream; ARichTextLength: Integer; AFlags: Integer);
procedure SaveRichTextToFile(hwnd: HWND; const AFileName: string);
procedure LoadRichTextFromFile(hwnd: HWND; const AFileName: string);
function GetRichEditText(hwnd: HWND; ASelectionOnly: Boolean): string;
function GetRichEditRtfText(hwnd: HWND; ASelectionOnly: Boolean): string;
function GetRichEditRtfTextLength(hwnd: HWND): Integer;
function GetRichEditTextLength(hwnd: HWND): Integer;

procedure SaveObjectsVersionsToStream(Stream: TStream; LV : TList);
procedure LoadObjectsVersionsFromStream(Stream: TStream; LV : TList);

function prCreatePen(APenStyle: TPenStyle; APenWidth: Integer; APenColor: TColor): HPEN;
function prCreateBrush(ABrushStyle: TBrushStyle; ABrushColor: TColor): HBRUSH;

var
{Contains the handle to the loaded RTF library.}
  RtfDllHandle: HINST = 0;
{Contains the name of the loaded RTF library.}
  RtfDllName: string = '';

implementation

uses
  pr_Parser, pr_Preview, pr_ExportParams, pr_Strings, pr_ExportFilters,
  pr_SelectPrintPaperSize, pr_ShapeObj, pr_BarCodeObj;

type
  TprCommonAlign = (praToMin, praCenter, praToMax, praToJustify);
  TprObjRecAccess = class(TprObjRec)
  end;

  TprCustomReportAccess = class(TprCustomReport)
  end;

  TprPaperInfo = record
    Typ: Integer;
    Name: string;
    X,Y: Integer;
  end;

  rRtfVersionInfo = record
    RtfVersion: TprRtfVersion;
    DllName: string;
    WindowClassName: string;
  end;
  
const
  Rich2FindTextTextColor = clWhite;
  Rich2FindTextBackColor = clBlack;
  RichFindTextTextColor = clBlue;
  MaxRichTextWidthTwips = 100000000;
  MaxRichTextHeightTwips = 100000000;
  DefaultRtfWindowClass = 'RICHEDIT';
  DefaultRtfWindowWidth = 5000;
  DefaultRtfWindowHeight = 5000;

  PreparedReportSavePrefix = $FFFFFFFF;
  PreparedReportSaveFormatVersion = $00000002;

  PAPERCOUNT = 66;

  MaxRtfLibraries = 2;
  RtfLibraries: array [1 .. MaxRtfLibraries] of string =
    ('msftedit.dll', 'riched20.dll');

  MaxRtfVersions = 3;
  RtfVersions: array [1 .. MaxRtfVersions] of rRtfVersionInfo =
    ((RtfVersion: prrv4_1; DllName: 'msftedit.dll'; WindowClassName: 'RICHEDIT50W'),
     (RtfVersion: prrv3_0; DllName: 'riched20.dll'; WindowClassName: 'RichEdit20W'),
     (RtfVersion: prrv3_09x; DllName: 'riched20.dll'; WindowClassName: 'RichEdit20A'));
  

var
  PenStyles: array[TPenStyle] of DWORD = (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL, PS_INSIDEFRAME);
  BrushStyles: array[TBrushStyle] of DWORD = (0, 0, HS_HORIZONTAL, HS_VERTICAL, HS_FDIAGONAL, HS_BDIAGONAL, HS_CROSS, HS_DIAGCROSS);
  PaperInfo: Array[0..PAPERCOUNT-1] of TprPaperInfo = (
    (Typ:1;  Name: 'Letter, 8 1/2 x 11"'; X:2159; Y:2794),
    (Typ:2;  Name: 'Letter small, 8 1/2 x 11"'; X:2159; Y:2794),
    (Typ:3;  Name: 'Tabloid, 11 x 17"'; X:2794; Y:4318),
    (Typ:4;  Name: 'Ledger, 17 x 11"'; X:4318; Y:2794),
    (Typ:5;  Name: 'Legal, 8 1/2 x 14"'; X:2159; Y:3556),
    (Typ:6;  Name: 'Statement, 5 1/2 x 8 1/2"'; X:1397; Y:2159),
    (Typ:7;  Name: 'Executive, 7 1/4 x 10 1/2"'; X:1842; Y:2667),
    (Typ:8;  Name: 'A3 297 x 420 μμ'; X:2970; Y:4200),
    (Typ:9;  Name: 'A4 210 x 297 μμ'; X:2100; Y:2970),
    (Typ:10; Name: 'A4 small sheet, 210 x 297 μμ'; X:2100; Y:2970),
    (Typ:11; Name: 'A5 148 x 210 μμ'; X:1480; Y:2100),
    (Typ:12; Name: 'B4 250 x 354 μμ'; X:2500; Y:3540),
    (Typ:13; Name: 'B5 182 x 257 μμ'; X:1820; Y:2570),
    (Typ:14; Name: 'Folio, 8 1/2 x 13"'; X:2159; Y:3302),
    (Typ:15; Name: 'Quarto Sheet, 215 x 275 μμ'; X:2150; Y:2750),
    (Typ:16; Name: '10 x 14"'; X:2540; Y:3556),
    (Typ:17; Name: '11 x 17"'; X:2794; Y:4318),
    (Typ:18; Name: 'Note, 8 1/2 x 11"'; X:2159; Y:2794),
    (Typ:19; Name: '9 Envelope, 3 7/8 x 8 7/8"'; X:984;  Y:2254),
    (Typ:20; Name: '#10 Envelope, 4 1/8  x 9 1/2"'; X:1048; Y:2413),
    (Typ:21; Name: '#11 Envelope, 4 1/2 x 10 3/8"'; X:1143; Y:2635),
    (Typ:22; Name: '#12 Envelope, 4 3/4 x 11"'; X:1207; Y:2794),
    (Typ:23; Name: '#14 Envelope, 5 x 11 1/2"'; X:1270; Y:2921),
    (Typ:24; Name: 'C Sheet, 17 x 22"'; X:4318; Y:5588),
    (Typ:25; Name: 'D Sheet, 22 x 34"'; X:5588; Y:8636),
    (Typ:26; Name: 'E Sheet, 34 x 44"'; X:8636; Y:11176),
    (Typ:27; Name: 'DL Envelope, 110 x 220 μμ'; X:1100; Y:2200),
    (Typ:28; Name: 'C5 Envelope, 162 x 229 μμ'; X:1620; Y:2290),
    (Typ:29; Name: 'C3 Envelope,  324 x 458 μμ'; X:3240; Y:4580),
    (Typ:30; Name: 'C4 Envelope,  229 x 324 μμ'; X:2290; Y:3240),
    (Typ:31; Name: 'C6 Envelope,  114 x 162 μμ'; X:1140; Y:1620),
    (Typ:32; Name: 'C65 Envelope, 114 x 229 μμ'; X:1140; Y:2290),
    (Typ:33; Name: 'B4 Envelope,  250 x 353 μμ'; X:2500; Y:3530),
    (Typ:34; Name: 'B5 Envelope,  176 x 250 μμ'; X:1760; Y:2500),
    (Typ:35; Name: 'B6 Envelope,  176 x 125 μμ'; X:1760; Y:1250),
    (Typ:36; Name: 'Italy Envelope, 110 x 230 μμ'; X:1100; Y:2300),
    (Typ:37; Name: 'Monarch Envelope, 3 7/8 x 7 1/2"'; X:984;  Y:1905),
    (Typ:38; Name: '6 3/4 Envelope, 3 5/8 x 6 1/2"'; X:920;  Y:1651),
    (Typ:39; Name: 'US Std Fanfold, 14 7/8 x 11"'; X:3778; Y:2794),
    (Typ:40; Name: 'German Std Fanfold, 8 1/2 x 12"'; X:2159; Y:3048),
    (Typ:41; Name: 'German Legal Fanfold, 8 1/2 x 13"'; X:2159; Y:3302),
    (Typ:42; Name: 'B4 (ISO) 250 x 353 μμ'; X:2500; Y:3530),
    (Typ:43; Name: 'Japanese Postcard 100 x 148 μμ'; X:1000; Y:1480),
    (Typ:44; Name: '9 x 11"'; X:2286; Y:2794),
    (Typ:45; Name: '10 x 11"'; X:2540; Y:2794),
    (Typ:46; Name: '15 x 11"'; X:3810; Y:2794),
    (Typ:47; Name: 'Envelope Invite 220 x 220 μμ'; X:2200; Y:2200),
    (Typ:50; Name: 'Letter Extra 9 \ 275 x 12"'; X:2355; Y:3048),
    (Typ:51; Name: 'Legal Extra 9 \275 x 15"'; X:2355; Y:3810),
    (Typ:52; Name: 'Tabloid Extra 11.69 x 18"'; X:2969; Y:4572),
    (Typ:53; Name: 'A4 Extra 9.27 x 12.69"'; X:2354; Y:3223),
    (Typ:54; Name: 'Letter Transverse 8 \275 x 11"'; X:2101; Y:2794),
    (Typ:55; Name: 'A4 Transverse 210 x 297 μμ'; X:2100; Y:2970),
    (Typ:56; Name: 'Letter Extra Transverse 9\275 x 12"'; X:2355; Y:3048),
    (Typ:57; Name: 'SuperASuperAA4 227 x 356 μμ'; X:2270; Y:3560),
    (Typ:58; Name: 'SuperBSuperBA3 305 x 487 μμ'; X:3050; Y:4870),
    (Typ:59; Name: 'Letter Plus 8.5 x 12.69"'; X:2159; Y:3223),
    (Typ:60; Name: 'A4 Plus 210 x 330 μμ'; X:2100; Y:3300),
    (Typ:61; Name: 'A5 Transverse 148 x 210 μμ'; X:1480; Y:2100),
    (Typ:62; Name: 'B5 (JIS) Transverse 182 x 257 μμ'; X:1820; Y:2570),
    (Typ:63; Name: 'A3 Extra 322 x 445 μμ'; X:3220; Y:4450),
    (Typ:64; Name: 'A5 Extra 174 x 235 μμ'; X:1740; Y:2350),
    (Typ:65; Name: 'B5 (ISO) Extra 201 x 276 μμ'; X:2010; Y:2760),
    (Typ:66; Name: 'A2 420 x 594 μμ'; X:4200; Y:5940),
    (Typ:67; Name: 'A3 Transverse 297 x 420 μμ'; X:2970; Y:4200),
    (Typ:68; Name: 'A3 Extra Transverse 322 x 445 μμ'; X:3220; Y:4450));

function prCreateBrush(ABrushStyle: TBrushStyle; ABrushColor: TColor): HBRUSH;
var
  ALogBrush: TLogBrush;
begin
  if (ABrushStyle = bsClear) or (ABrushColor = clNone) then
  begin
    ALogBrush.lbStyle := BS_NULL;
    ALogBrush.lbColor := GetRGBColor(ABrushColor);
    ALogBrush.lbHatch := 0;
    Result := CreateBrushIndirect(ALogBrush);
  end
  else
    if ABrushStyle = bsSolid then
      Result := CreateSolidBrush(GetRGBColor(ABrushColor))
    else
    begin
      ALogBrush.lbStyle := BS_HATCHED;
      ALogBrush.lbColor := GetRGBColor(ABrushColor);
      ALogBrush.lbHatch := BrushStyles[ABrushStyle];
      Result := CreateBrushIndirect(ALogBrush);
    end;
end;

function prCreatePen(APenStyle: TPenStyle; APenWidth: Integer; APenColor: TColor): HPEN;
var
  ABrush: tagLOGBRUSH;
begin
  if (APenWidth = 0) or (APenStyle = psClear) or (APenColor = clNone) then
    Result := Windows.CreatePen(PS_NULL, 1, GetRGBColor(APenColor))
  else
    if APenWidth = 1 then
      Result := Windows.CreatePen(PenStyles[APenStyle], 1, GetRGBColor(APenColor))
    else
    begin
      ABrush.lbStyle := BS_SOLID;
      ABrush.lbColor := GetRGBColor(APenColor);
      Result := ExtCreatePen(PS_GEOMETRIC or PS_ENDCAP_SQUARE or PenStyles[APenStyle], APenWidth, ABrush, 0, nil);
    end;
end;

function CalcMemoHeight(ALineCount: Integer; ALineHeight: Integer): Integer; overload;
begin
  Result := ALineCount * ALineHeight + Max(0, (ALineCount - 1) * StepLine);
end;

function CalcMemoHeight(Memo: TStrings; ALineHeight: Integer): Integer; overload;
begin
  Result := CalcMemoHeight(Memo.Count, ALineHeight);
end;

function CalcMemoHeight(ADC: HDC; Memo: TStrings) : integer; overload;
var
  ATextMetrics: tagTEXTMETRIC;
begin
  GetTextMetrics(ADC, ATextMetrics);
  Result := CalcMemoHeight(Memo, ATextMetrics.tmHeight);
end;                     

function CalcMemoHeight(ADC: HDC; ALineCount: Integer): Integer; overload;
var
  ATextMetrics: tagTEXTMETRIC;
begin
  GetTextMetrics(ADC, ATextMetrics);
  Result := CalcMemoHeight(ALineCount, ATextMetrics.tmHeight);
end;

function prConvertFromPixelsString;
const
  aformats : array [TprPosSizeUnits] of string = ('0','0.00','0.000','0.000','0.00000');
begin
Result:=FormatFloat(aformats[ToU],prConvertFromPixels(Value,ToU,IsHor));
end;

function prConvertFromPixels(Value : integer; ToU : TprPosSizeUnits; IsHor : boolean) : double;
var
  ppi : integer;
begin
Result := 0.0;
if IsHor then ppi := prGetScreenPixelsPerX
         else ppi := prGetScreenPixelsPerY;
case ToU of
  prpsuPixels: Result:=Value;
  prpsuMM    : Result:=Value / ppi * MillimetersPerInch;
  prpsuSm    : Result:=Value / ppi * SantimetersPerInch;
  prpsuInch  : Result:=Value / ppi;
  prpsuM     : Result:=Value / ppi * MetersPerInch;
end;
end;

function prConvertToPixels;
var
  ppi : integer;
begin
Result := 0;
if IsHor then ppi := prGetScreenPixelsPerX
         else ppi := prGetScreenPixelsPerY;
case FromU of
  prpsuPixels : Result := Round(Value);
  prpsuMM : Result := Round(Value * ppi / MillimetersPerInch);
  prpsuSm : Result := Round(Value * ppi / SantimetersPerInch);
  prpsuInch : Result := Round(Value * ppi);
  prpsuM : Result := Round(Value * ppi / MetersPerInch);
end;
end;

function GetBW;
begin
  if b.Show then
    Result := b.Width
  else
    Result := 0;
end;

function mm(v: integer) : integer;
begin
  Result := Round(v / 10);
end;


//{$DEFINE OLD_WORDWRAP}

{$IFDEF OLD_WORDWRAP}
procedure WrapMemo(DC : HDC; Memo : TStrings; w : integer);
var
  s : string;
  l : TStringList;
  sz : tagSize;
  i,ls,p1,p2 : integer;
begin
l := TStringList.Create;
try
  for i:=0 to Memo.Count-1 do
    begin
      s := Memo[i];
      repeat
        ls := Length(s);
        p1 := ls;
        while p1>0 do
          begin
            GetTextExtentPoint32(DC,PChar(s),p1,sz);
            if sz.cx<=w then break;
            Dec(p1);
          end;
        // width of string from 1 to p1 less then w
        if p1<ls then
          begin
            if s[p1+1]=' ' then
              begin
                while (p1<=ls) and (s[p1]=' ') do Inc(p1);
              end
            else
              begin
                p2 := p1;
                while (p2>0) and not(s[p2] in WrapChars) do Dec(p2);
                if p2>0 then
                  p1 := p2;
              end;
            l.Add(Trim(Copy(s,1,p1)));
            Delete(s,1,p1)
          end
        else
          break;
      until false;
      l.Add(s);
    end;
  Memo.Assign(l);
finally
  l.Free;
end;
end;

{$ELSE}

{
procedure WrapMemo(DC : HDC; Memo : TStrings; w : integer);
var
  l : TStringList;
  s : string;
  sz : TSize;
  i,ls,p1,p2,p3 : integer;
begin
l := TStringList.Create;
try
  for i:=0 to Memo.Count-1 do
    begin
      s := Memo[i];
      ls := Length(s);
      if ls = 0 then
        l.Add(s)
      else
      begin
        p1 := 1;
        p2 := 1;
        while p2<=ls do
        begin
          GetTextExtentExPoint(DC,PChar(@s[p2]),ls-p2+1,w,@p1,nil,sz);
          if p2+p1<=ls then
            begin
              //
              if s[p1+p2]=' ' then
                begin
                  while (p1+p2<=ls) and (s[p1+p2]=' ') do Inc(p1);
                end
              else
                begin
                  p3 := p1+p2-1;
                  while (p3>=p2) and not(s[p3] in WrapChars) do Dec(p3);
                  if p3>=p2 then
                    p1 := p3-p2+1;
                end;
            end;
          if p1=0 then
            p1 :=1;
          // add string part from p2 to p1
          if l.Count = 0 then
            l.Add(TrimRight(Copy(s, p2, p1)))
          else
            l.Add(Trim(Copy(s,p2,p1)));
          p2 := p2+p1;
        end;
      end;
    end;
  Memo.Assign(l);
finally
  l.Free;
end;
end;
}
{$ENDIF}

procedure SaveDCObjects(DC : HDC; var hpn : HPEN; var hbr : HBRUSH; var hfn : HFONT);
begin
hpn := GetCurrentObject(DC,OBJ_PEN);
hbr := GetCurrentObject(DC,OBJ_BRUSH);
hfn := GetCurrentObject(DC,OBJ_FONT);
end;

procedure RestoreDCObjects(DC : HDC; var hpn : HPEN; var hbr : HBRUSH; var hfn : HFONT);
begin
SelectObject(DC,hpn);
SelectObject(DC,hbr);
SelectObject(DC,hfn);
end;

//////////////////////
//
// TprExObjRecVersion
//
//////////////////////
function TprExObjRecVersion.GetRealRect(pdi : pPrDrawInfo) : TRect;
begin
  Result := MulRect(GeneratedRect,pdi.kx,pdi.ky);
end;

procedure TprExObjRecVersion.SetRealRect(const Value : TRect; pdi : pPrDrawInfo);
begin
  GeneratedRect := DivRect(Value, pdi.kx, pdi.ky);
end;

procedure TprExObjRecVersion.Assign;
begin
inherited;
if Source is TprExObjRecVersion then
  with TprExObjRecVersion(Source) do
    begin
      Self.GeneratedRect := GeneratedRect;
      Self.FPreviewUserData := PreviewUserData;
    end;
end;

procedure TprExObjRecVersion.DefineProperties;
begin
inherited;
Filer.DefineBinaryProperty('DrawRect',
                           ReadGeneratedRect,
                           WriteGeneratedRect,
                           not((GeneratedRect.Left=0) and
                               (GeneratedRect.Top=0) and
                               (GeneratedRect.Right=0) and
                               (GeneratedRect.Bottom=0)));
Filer.DefineBinaryProperty('PreviewUserData',
                           ReadPreviewUserData,
                           WritePreviewUserData,
                           PreviewUserData<>nil);
end;

procedure TprExObjRecVersion.ReadGeneratedRect;
begin
Stream.ReadBuffer(GeneratedRect,sizeof(GeneratedRect));
end;

procedure TprExObjRecVersion.WriteGeneratedRect;
begin
Stream.WriteBuffer(GeneratedRect,sizeof(GeneratedRect));
end;

procedure TprExObjRecVersion.ReadPreviewUserData;
begin
FPreviewUserData := CreateprPreviewUserData(ReadString(Stream));
FPreviewUserData.LoadFromStream(Stream);
end;

procedure TprExObjRecVersion.WritePreviewUserData;
begin
WriteString(Stream,PreviewUserData.ClassName);
PreviewUserData.SaveToStream(Stream);
end;

procedure TprExObjRecVersion.Scale;
begin
  GeneratedRect := MulDivRect(GeneratedRect,cx1,cx2,cy1,cy2);
end;

/////////////////////////////////////////////////
//
// TprExObjRec
//
/////////////////////////////////////////////////
function TprExObjRec.GetObj: TprExObj;
begin
  Result := TprExObj(inherited Obj);
end;

procedure TprExObjRec.ThirdPass;
var
  v: TprExObjRecVersion;
begin
  if not Versions[CurVersion].Visible then exit;

  v := TprExObjRecVersion(GetVersionClass.Create(nil));
  v.Assign(Versions[CurVersion]);
  v.FPreviewUserData := PreviewUserData;
  FPreviewUserData := nil;
  v.GeneratedRect := r;

  TprEndPage(AEndPage).vl.Add(v);
end;

/////////////////////////////////////////////////
//
// TprExObj
//
/////////////////////////////////////////////////
function TprExObj.GetReport: TprReport;
begin
  Result := TprReport(inherited Report);
end;

///////////////////////////
//
// TprFindText
//
///////////////////////////
constructor TprFindText.CreateFT;
begin
inherited;
TextRect := _TextRect;
end;

//////////////////////
//
// TprFrameLine
//
//////////////////////
procedure TprFrameLine.Assign;
begin
with Source as TprFrameLine do
  begin
    Self.FShow := FShow;
    Self.FStyle := FStyle;
    Self.FColor := FColor;
    Self.FWidth := FWidth;
  end;
end;

function DrawTextJustify(DC: HDC;
                         S: PChar;
                         ls: Integer;
                         yOffs: Integer;
                         xOffs: Integer;
                         Rotate90: Boolean;
                         AllSize: Integer;
                         JustifySize: Integer;
                         TextRect: TRect;
                         Align: TprCommonAlign;
                         pdi: PprDrawInfo) : integer;
var
  r: TRect;
  sz: TSize;
  aDx: PIntArray;
  tl, fCompare : cardinal;
  pStart, Width, p, lft, i, offs : integer;
  b1: Integer;
  szbuf: TSize;
begin
  offs := 0;
  case Align of
    praCenter: if AllSize > JustifySize then offs := (AllSize - JustifySize) div 2;
    praToMax : if AllSize > JustifySize then offs := AllSize - JustifySize;
  end;

  if Rotate90 then yOffs := yOffs - Offs
              else xOffs := xOffs + Offs;

  GetTextExtentPoint32(DC, s, ls, sz);
  Result := sz.cy;
  aDx := nil;
  try
    if ((sz.cx = JustifySize) and (Align <> praToJustify)) or (ls = 1) then
    begin
      // simple output text
      ExtTextOut(DC,
                 xOffs,
                 yOffs,
                 ETO_CLIPPED,
                 @TextRect,
                 s,
                 ls,
                 nil);
    end
    else
    begin
      // get symbols positions
      GetMem(aDx, 4 * ls);
      if GetTextExtentExPoint(DC, s, ls, sz.cx, @b1, @(aDx^[0]), szbuf) then
        begin
          for I := ls - 1 downto 1 do
            aDx^[i] := aDx^[i] - aDx^[i-1];
          if Align = praToJustify then
            JustifyArray(@(aDx^[0]), ls - 1, AllSize, sz.cx, True, S)
          else
            JustifyArray(@(aDx^[0]), ls - 1, JustifySize, sz.cx, False, nil);
          ExtTextOut(DC,
                     xOffs,
                     yOffs,
                     ETO_CLIPPED,
                     @TextRect,
                     s,
                     ls,
                     @(aDx^[0]));
        end;
    end;

    if (pdi^.ppdi<>nil) and (pdi^.ppdi^.DrawMode in [dmFind,dmFindFirst]) then
    begin
      fCompare := SORT_STRINGSORT;
      if not pdi^.ppdi^.CaseSensitive then
        fCompare := fCompare or NORM_IGNORECASE;

      // It is necessary invert  rectangles with suitable strings
      tl := GetThreadLocale;
      p := 0;
      lft := Length(pdi^.ppdi^.FindText);
      while p<=ls-lft do
        begin
          if CompareString(tl,
                           fCompare,
                           s+p,
                           lft,
                           PChar(pdi^.ppdi^.FindText),
                           lft)=2 then
            begin
              // The text is found, we getting it a rectangle
              if aDx=nil then
                begin
                  GetTextExtentPoint32(DC,@(s[0]),p,sz);
                  pStart:=sz.cx;
                  GetTextExtentPoint32(DC,@(s[p]),lft,sz);
                  Width :=sz.cx;
                end
              else
                begin
                  pStart:=0;
                  i     :=0;
                  while i<p do
                    begin pStart:=pStart+aDx^[i]; Inc(i); end;
                  Width :=0;
                  while (i<p+lft) and (i<ls-1) do
                    begin Width:=Width+aDx^[i]; Inc(i); end;
                  if (i>=ls-1) and (ls=p+lft) then
                    begin
                      // add width of last symbol
                      GetTextExtentPoint32(DC,s+ls-1,1,sz);
                      Width:=Width+sz.cx;
                    end;
                end;

              if Rotate90 then
                r:=Rect(xoffs,yoffs-pStart-Width,xoffs+Result,yoffs-pStart)
              else
                r:=Rect(xoffs+pStart,yoffs,xoffs+pStart+Width,yoffs+Result);
              InvertRect(DC,r);

              if pdi^.ppdi^.DrawMode=dmFindFirst then
                // It is necessary to add, rectangles of the found fragments
                pdi^.ppdi^.FindList.Add(TprFindText.CreateFT(r));
            end;
          Inc(p);
        end;
    end;
  finally
    if aDx <> nil then
      FreeMem(aDx);
  end;
end;

procedure DrawObjBorders(ADC: HDC;
                         const ARect: TRect;
                         ALeftBorder,
                         ATopBorder,
                         ARightBorder,
                         ABottomBorder: TprFrameLine;
                         ALeftBorderSize,
                         ATopBorderSize,
                         ARightBorderSize,
                         ABottomBorderSize: Integer);
var
  NewH, OldH: HGDIOBJ;
  w1, w2: integer;
  nbr, obr: HBRUSH;
  lbr: tagLOGBRUSH;

  procedure SetPS(ABorder: TprFrameLine; AWidth: integer);
  begin
    NewH := prCreatePen(ABorder.Style, AWidth, ABorder.Color);
    OldH := SelectObject(ADC, NewH);
  end;

begin
  if (ATopBorder.Show and ALeftBorder.Show and ABottomBorder.Show and ARightBorder.Show) and
     ((ATopBorder.Style = ALeftBorder.Style) and (ATopBorder.Style = ABottomBorder.Style) and (ATopBorder.Style = ARightBorder.Style)) and
     ((ATopBorder.Color = ALeftBorder.Color) and (ATopBorder.Color = ABottomBorder.Color) and (ATopBorder.Color = ARightBorder.Color)) and
     ((ATopBorder.Width = ALeftBorder.Width) and (ATopBorder.Width = ABottomBorder.Width) and (ATopBorder.Width = ARightBorder.Width)) then
  begin
    if ATopBorder.Show then
      begin
        w1 := ATopBorderSize div 2;
        w2 := ATopBorderSize mod 2;

        SetPS(ATopBorder, ATopBorderSize);
        lbr.lbStyle := BS_NULL;
        nbr := CreateBrushIndirect(lbr);
        obr := SelectObject(ADC, nbr);

        Rectangle(ADC,
                  ARect.Left + w1,
                  ARect.Top + w1,
                  ARect.Right - w1 + (w2 xor 1),
                  ARect.Bottom - w1 + (w2 xor 1));

        SelectObject(ADC, obr);
        DeleteObject(nbr);
        SelectObject(ADC, OldH);
        DeleteObject(NewH);
      end;
  end
else
  begin
    if ATopBorder.Show then
      begin
        SetPS(ATopBorder,ATopBorderSize);

        w1 := ATopBorderSize div 2;
        w2 := (ATopBorderSize div 2) + (ATopBorderSize mod 2);
        MoveToEx(ADC, ARect.Left + w1, ARect.Top + w1, nil);
        LineTo(ADC, ARect.Right - w2, ARect.Top + w1);

        SelectObject(ADC, OldH);
        DeleteObject(NewH);
      end;

    if ARightBorder.Show then
      begin
        SetPS(ARightBorder,ARightBorderSize);

        w1 := ARightBorderSize div 2;
        w2 := (ARightBorderSize div 2) + (ARightBorderSize mod 2);
        MoveToEx(ADC, ARect.Right - w2, ARect.Top + w1, nil);
        LineTo(ADC, ARect.Right - w2, ARect.Bottom - w2);

        SelectObject(ADC, OldH);
        DeleteObject(NewH);
      end;

    if ABottomBorder.Show then
      begin
        SetPS(ABottomBorder,ABottomBorderSize);

        w1 := ABottomBorderSize div 2;
        w2 := (ABottomBorderSize div 2) + (ABottomBorderSize mod 2);
        MoveToEx(ADC, ARect.Right - w2, ARect.Bottom - w2, nil);
        LineTo(ADC, ARect.Left + w1, ARect.Bottom - w2);

        SelectObject(ADC, OldH);
        DeleteObject(NewH);
      end;

    if ALeftBorder.Show then
      begin
        SetPS(ALeftBorder, ALeftBorderSize);

        w1 := ALeftBorderSize div 2;
        w2 := (ALeftBorderSize div 2) + (ALeftBorderSize mod 2);
        MoveToEx(ADC, ARect.Left + w1, ARect.Bottom - w2, nil);
        LineTo(ADC, ARect.Left + w1,ARect.Top + w1);

        SelectObject(ADC, OldH);
        DeleteObject(NewH);
      end;
  end;
end;

procedure DrawDesignObjBorders(ADC: HDC;
                               const ARect: TRect;
                               ALeftBorder,
                               ATopBorder,
                               ARightBorder,
                               ABottomBorder: TprFrameLine);
begin
  // If the borders from different directions identical is received that, at once we draw
  // rectangle to avoid errors for want of process of scaling
  if not (ATopBorder.Show or ALeftBorder.Show or ABottomBorder.Show or ARightBorder.Show) then
    DrawAngleRect(ADC, ARect)
  else
    DrawObjBorders(ADC,
                   ARect,
                   ALeftBorder,
                   ATopBorder,
                   ARightBorder,
                   ABottomBorder,
                   ALeftBorder.Width,
                   ATopBorder.Width,
                   ARightBorder.Width,
                   ABottomBorder.Width);
end;

/////////////////////////////////////////////////
//
// TprMemoObjRecVersion
//
/////////////////////////////////////////////////
constructor TprMemoObjRecVersion.Create;
begin
  inherited;
  FMemo := TStringList.Create;
  FFont := TFont.Create;
  FlBorder := TprFrameLine.Create;
  FtBorder := TprFrameLine.Create;
  FrBorder := TprFrameLine.Create;
  FbBorder := TprFrameLine.Create;
  FEolIsEndOfParagraph := True;
end;

destructor TprMemoObjRecVersion.Destroy;
begin
  FMemo.Free;
  FFont.Free;
  FlBorder.Free;
  FtBorder.Free;
  FrBorder.Free;
  FbBorder.Free;
  inherited;
end;

procedure TprMemoObjRecVersion.SettBorder(Value: TprFrameLine);
begin
  FtBorder.Assign(Value);
end;

procedure TprMemoObjRecVersion.SetlBorder(Value: TprFrameLine);
begin
  FlBorder.Assign(Value);
end;

procedure TprMemoObjRecVersion.SetrBorder(Value: TprFrameLine);
begin
  FrBorder.Assign(Value);
end;

procedure TprMemoObjRecVersion.SetbBorder(Value: TprFrameLine);
begin
  FbBorder.Assign(Value);
end;

procedure TprMemoObjRecVersion.SetMemo(Value: TStrings);
begin
  FMemo.Assign(Value);
end;

procedure TprMemoObjRecVersion.DefineProperties;
begin
inherited;
Filer.DefineProperty('FontSize',ReadFontSize,WriteFontSize,true);
Filer.DefineBinaryProperty('GeneratedData',
                           ReadGeneratedData,
                           WriteGeneratedData,
                           false);
end;

procedure TprMemoObjRecVersion.ReadFontSize;
begin
Font.Size := Reader.ReadInteger;
end;

procedure TprMemoObjRecVersion.WriteFontSize;
begin
Writer.WriteInteger(Font.Size);
end;

procedure TprMemoObjRecVersion.ReadGeneratedData;
var
  n : integer;
begin
Stream.Seek(4,soFromCurrent);
Stream.ReadBuffer(n,4);
Stream.Seek(n*4,soFromCurrent);
end;

procedure TprMemoObjRecVersion.WriteGeneratedData;
begin
end;

procedure TprMemoObjRecVersion.Assign;
begin
with Source as TprMemoObjRecVersion do
  begin
    Self.SecondPassNeeded := SecondPassNeeded;
    Self.FFillColor := FFillColor;
    Self.FhAlign := FhAlign;
    Self.FvAlign := FvAlign;
    Self.FRotate90 := FRotate90;
    Self.FDeleteEmptyLinesAtEnd := FDeleteEmptyLinesAtEnd;
    Self.FDeleteEmptyLines := FDeleteEmptyLines;
    Self.FCanResizeX := FCanResizeX;
    Self.FCanResizeY := FCanResizeY;
    Self.FWordWrap := FWordWrap;

    Self.FJustifyLastLine := FJustifyLastLine;
    Self.FEolIsEndOfParagraph := FEolIsEndOfParagraph;

    Self.FlBorder.Assign(FlBorder);
    Self.FtBorder.Assign(FtBorder);
    Self.FbBorder.Assign(FbBorder);
    Self.FrBorder.Assign(FrBorder);

    Self.FFont.Assign(FFont);
    Self.FMemo.Assign(FMemo);
  end;
inherited;
end;

procedure TprMemoObjRecVersion.InitInDesigner;
  procedure InitFrameLine(fl : TprFrameLine);
  begin
  fl.Show := true;
  fl.Width := 1;
  fl.Color := clBlack;
  fl.Style := psSolid;
  end;
begin
FillColor := clWhite;
InitFrameLine(lBorder);
InitFrameLine(rBorder);
InitFrameLine(tBorder);
InitFrameLine(bBorder);
Font.Name := sArialFont;
Font.Size := 10;
Font.Charset := DEFAULT_CHARSET;
end;

procedure TprMemoObjRecVersion.CalcLayout(ADC: HDC;
                                          pdi: PprDrawInfo;
                                          ANeedLinesWidth: Boolean;
                                          const AObjectRect: TRect;
                                          var ARect: TRect;
                                          var AInnerRect: TRect;
                                          var ATextHeight: Integer;
                                          var ATopLeft: TPoint;
                                          var ALeftBorderSize: Integer;
                                          var ATopBorderSize: Integer;
                                          var ARightBorderSize: Integer;
                                          var ABottomBorderSize: Integer;
                                          var ALineHeight: Integer;
                                          var ALinesWidth: TprDynIntegerArray;
                                          var ALines: TvgrWrapLineDynArray);
var
  I, AWrapTextSize: Integer;
  ANewFont, AOldFont: HFONT;
  ASize: TSize;
  ATextMetrics: tagTEXTMETRIC;
  S: string;
  AText: string;
begin
  if lBorder.Show then
    ALeftBorderSize := Max(1, Round(lBorder.Width * pdi.kx))
  else
    ALeftBorderSize := 0;
  if tBorder.Show then
    ATopBorderSize := Max(1, Round(tBorder.Width * pdi.ky))
  else
    ATopBorderSize := 0;
  if rBorder.Show then
    ARightBorderSize := Max(1, Round(rBorder.Width * pdi.kx))
  else
    ARightBorderSize := 0;
  if bBorder.Show then
    ABottomBorderSize := Max(1, Round(bBorder.Width * pdi.ky))
  else
    ABottomBorderSize := 0;

  ARect := MulRect(AObjectRect, pdi.kx, pdi.ky);

  AInnerRect := Rect(ARect.Left + ALeftBorderSize + Round(oLeft * pdi.kx),
                     ARect.Top + ATopBorderSize + Round(oTop * pdi.ky),
                     ARect.Right - ARightBorderSize - Round(oRight * pdi.kx),
                     ARect.Bottom - ABottomBorderSize - Round(oBottom * pdi.ky));

  if Rotate90 then
  begin
    ANewFont := Create90Font(Font);
    AWrapTextSize := AObjectRect.Bottom - AObjectRect.Top - oTop - oBottom - GetBW(tBorder) - GetBW(bBorder);
  end
  else
  begin
    ANewFont := CreateAPIFont(Font);
    AWrapTextSize := AObjectRect.Right - AObjectRect.Left - oLeft - oRight - GetBW(rBorder) - GetBW(lBorder);
  end;
  AOldFont := SelectObject(ADC, ANewFont);

  GetTextMetrics(ADC, ATextMetrics);
  ALineHeight := ATextMetrics.tmHeight;

  AText := Memo.Text;
  WrapMemo(ADC, AText, AWrapTextSize, ALines, WordWrap, DeleteEmptyLines, DeleteEmptyLinesAtEnd);
  ATextHeight := Round(CalcMemoHeight(ADC, Length(ALines)) * pdi.ky);
  SetLength(ALinesWidth, Length(ALines));

  if ANeedLinesWidth then
    for I := 0 to High(ALines) do
    begin
      S := Copy(AText, ALines[I].Start, ALines[I].Length);
      if pdi.IsPrinter and (pdi.Report <> nil) then
        TprReport(pdi.Report).ParsePrintingString(S);

      GetTextExtentPoint32(ADC, PChar(S), Length(S), ASize);
      ALinesWidth[I] := Round(ASize.cx * pdi.kx);
    end;

  SelectObject(ADC, AOldFont);
  DeleteObject(ANewFont);

  with AInnerRect do
    if Rotate90 then
    begin
      ATopLeft.Y := Bottom;
      ATopLeft.X := Left;
      case hAlign of
        prhCenter:
          if Right - Left > ATextHeight then
            ATopLeft.X := Left + (Right - Left - ATextHeight) div 2;
        prhRight:
          if Right - Left > ATextHeight then
            ATopLeft.X := Right - ATextHeight;
      end;
    end
    else
    begin
      ATopLeft.X := Left;
      ATopLeft.Y := Top;
      case vAlign of
        prvCenter:
          if Bottom - Top > ATextHeight then
            ATopLeft.Y := Top + (Bottom - Top - ATextHeight) div 2;
        prvBottom:
          if Bottom - Top > ATextHeight then
            ATopLeft.Y := Bottom - ATextHeight;
      end;
    end;
end;

procedure TprMemoObjRecVersion.InternalDraw(const ARect: TRect; ADC: HDC; pdi: pprDrawInfo; ADesignTime: Boolean);
const
  AConv: array [TprVAlign] of TprCommonAlign = (praToMax, praCenter, praToMin);
var
  AText: string;
  ARealRect, AInnerRect: TRect;
  ABrush: HBRUSH;
  AHeights: PIntArray;
  ANewFont, AOldFont: HFONT;
  ATopLeft: TPoint;
  AFillColor: TColor;
  ALineHeight: Integer;
  ARealTextWidths : TprDynIntegerArray;
  ATextAlign: TprCommonAlign;
  ALinesHigh, APerY, I, AAllTextSize, ARealTextHeight, ALeftBorderSize, ATopBorderSize, ARightBorderSize, ABottomBorderSize: integer;
  ALines: TvgrWrapLineDynArray;
  ATextMetrics: tagTEXTMETRIC;
  S: string;
  ATa: TprCommonAlign;

  // Creates rotated on 90 degrees HFONT,
  // scaling is taken into account
  function _Create90Font(Font: TFont) : HFont;
  var
    F: TLogFont;
  begin
    GetObject(Font.Handle, SizeOf(TLogFont), @F);
    F.lfEscapement := 900;
    F.lfOrientation := 900;
    if pdi.IsPrinter then
      F.lfHeight := Round(-MulDiv(Font.Size, APerY, 72) * pdi.prnky)
    else
      F.lfHeight := Round(F.lfHeight * pdi.ky);
    Result := CreateFontIndirect(F);
  end;

  // Creates HFONT,
  // scaling is taken into account
  function _CreateAPIFont(Font: TFont) : HFont;
  var
    F: TLogFont;
  begin
    GetObject(Font.Handle, SizeOf(TLogFont), @F);
    if pdi.IsPrinter then
      F.lfHeight := Round(-MulDiv(Font.Size, APerY, 72) * pdi.prnky)
    else
      F.lfHeight := Round(F.lfHeight * pdi.ky);
    Result := CreateFontIndirect(F);
  end;

begin
  APerY := GetDeviceCaps(ADC, LOGPIXELSY);

  CalcLayout(ScreenDC{ADC},
             pdi,
             True,
             ARect,
             ARealRect,
             AInnerRect,
             ARealTextHeight,
             ATopLeft,
             ALeftBorderSize,
             ATopBorderSize,
             ARightBorderSize,
             ABottomBorderSize,
             ALineHeight,
             ARealTextWidths,
             ALines);

  AFillColor := FillColor;
  if AFillColor <> clNone then
  begin
    if ADesignTime or (AFillColor <> clWhite) then
    begin
      // background
      ABrush := CreateSolidBrush(GetRGBColor(AFillColor));
      FillRect(ADC, ARealRect, ABrush);
      DeleteObject(ABrush);
    end;
  end;

  // borders of object
  if ADesignTime then
    DrawDesignObjBorders(ADC, ARealRect, lBorder, tBorder, rBorder, bBorder)
  else
    DrawObjBorders(ADC, ARealRect, lBorder, tBorder, rBorder, bBorder, ALeftBorderSize, ATopBorderSize, ARightBorderSize, ABottomBorderSize);

  if Memo.Count > 0 then
  begin
    if Rotate90 then
    begin
      ANewFont := _Create90Font(Font);
      AAllTextSize := AInnerRect.Bottom - AInnerRect.Top;
      ATextAlign := AConv[vAlign]; //TprCommonAlign(vAlign),
    end
    else
    begin
      ANewFont := _CreateAPIFont(Font);
      AAllTextSize := AInnerRect.Right - AInnerRect.Left;
      ATextAlign := TprCommonAlign(hAlign);
    end;
    AOldFont := SelectObject(ADC, ANewFont);

    ALinesHigh := High(ALines);

    GetTextMetrics(ADC, ATextMetrics);
      
    GetMem(AHeights, ALinesHigh * 4);
    FillChar(AHeights^, ALinesHigh * 4, #0);
    JustifyArray(AHeights,
                 ALinesHigh,
                 ARealTextHeight,
                 CalcMemoHeight(ADC, ALinesHigh + 1),
                 False,
                 nil);

    SetTextColor(ADC, GetRGBColor(Font.Color));
    SetBkMode(ADC, TRANSPARENT);
    try
      AText := Memo.Text;
      for I := 0 to ALinesHigh do
      begin
        S := Copy(AText, ALines[I].Start, ALines[I].Length);
        if pdi.IsPrinter and (pdi.Report <> nil) then
          TprReport(pdi.Report).ParsePrintingString(S);

        if S <> '' then
        begin
          ATa := ATextAlign;
          if ATextAlign = praToJustify then
          begin
            if (I = ALinesHigh) and not JustifyLastLine then
            begin
              // last line
              Ata := praToMin;
            end
            else
            begin
              if ALines[I].CreatedFromCrLf and EolIsEndOfParagraph and ((I <> ALinesHigh) or not JustifyLastLine) then
                Ata := praToMin;
            end;
          end;
          
          DrawTextJustify(ADC,
                          PChar(S),
                          Length(S),
                          ATopLeft.Y,
                          ATopLeft.X,
                          Rotate90,
                          AAllTextSize,
                          ARealTextWidths[I],
                          AInnerRect,
                          ATa, //TprCommonAlign(vAlign),
                          pdi);
        end;

        if Rotate90 then
        begin
          ATopLeft.X := ATopLeft.X + StepLine + ATextMetrics.tmHeight;
          if I < ALinesHigh then
            ATopLeft.X := ATopLeft.X + AHeights^[I];
        end
        else
        begin
          ATopLeft.Y := ATopLeft.Y + StepLine + ATextMetrics.tmHeight;
          if I < ALinesHigh then
            ATopLeft.Y := ATopLeft.Y + aHeights^[I];
        end;
      end;

    finally
      SetBkMode(ADC,OPAQUE);
      SelectObject(ADC, AOldFont);
      DeleteObject(ANewFont);
      FreeMem(AHeights);
    end;
  end;
end;

procedure TprMemoObjRecVersion.Draw(DC: HDC; pdi: PPrDrawInfo);
begin
  InternalDraw(GeneratedRect, DC, pdi, False);
end;

/////////////////////////////////////////////////
//
// TprMemoObjRec
//
/////////////////////////////////////////////////
{
function TprMemoObjRec.CreateCopy;
begin
  Result := TprMemoObjRec.Create(Self.Page,Self.Obj);
  Result.Assign(Self);
end;
}

function TprMemoObjRec.GetVersionClass: TprObjVersionClass;
begin
  Result := TprMemoObjRecVersion;
end;

function TprMemoObjRec.GetSupportSplitting: Boolean;
begin
  Result := True;
end;

function TprMemoObjRec.GetCanSplitValue: Boolean;
begin
  Result := FCanSplit;
end;

procedure TprMemoObjRec.SetCanSplitValue(Value: Boolean);
begin
  FCanSplit := Value;
end;

function TprMemoObjRec.GetCanSplit(AByHorizontal: Boolean; ASplitPos: Integer): Boolean;
var
  v: TprMemoObjRecVersion;
  rdi: rprDrawInfo;
  ARect: TRect;
  AInnerRect: TRect;
  ATopLeft: TPoint;
  ALinesWidth: TprDynIntegerArray;
  ALineHeight, ATextHeight, ALeftBorderSize, ATopBorderSize, ARightBorderSize, ABottomBorderSize: Integer;
  ALines: TvgrWrapLineDynArray;
begin
  Result := FCanSplit;
  if Result then
  begin
    v := TprMemoObjRecVersion(Versions[CurVersion]);
    if v.Rotate90 then
      Result := AByHorizontal
    else
      Result := not AByHorizontal;
      
    if Result then
    begin
      with rdi do
      begin
        kx := 1;
        ky := 1;
        prnkx := 1;
        prnky := 1;
        IsPrinter := False;
        ppdi := nil;
      end;

      v.CalcLayout(ScreenDC,
                   @rdi,
                   False,
                   Rect(0, 0, Width, Height),
                   ARect,
                   AInnerRect,
                   ATextHeight,
                   ATopLeft,
                   ALeftBorderSize,
                   ATopBorderSize,
                   ARightBorderSize,
                   ABottomBorderSize,
                   ALineHeight,
                   ALinesWidth,
                   ALines);


      if v.Rotate90 then
        Result := (ASplitPos < ATopLeft.X) or (ASplitPos >= ATopLeft.X + ALineHeight)
      else
        Result := (ASplitPos < ATopLeft.Y) or (ASplitPos >= ATopLeft.Y + ALineHeight);
    end;
  end;
end;

function TprMemoObjRec.Split(AByHorizontal: Boolean; ASplitPos: Integer; var AAddToSplitted: Integer): TprObjRec;
var
  V: TprMemoObjRecVersion;
  rdi: rprDrawInfo;
  ARect: TRect;
  AInnerRect: TRect;
  ATopLeft: TPoint;
  ALinesWidth: TprDynIntegerArray;
  I, ALineCount, ALineHeight, ATextHeight, ALeftBorderSize, ATopBorderSize, ARightBorderSize, ABottomBorderSize: Integer;
  ALines: TvgrWrapLineDynArray;

  procedure _Split(var ANewMin, ANewMax, AOldMin, AOldMax, AOldLineCount: Integer;
                   ATextStart: Integer;
                   ACanResize: Boolean;
                   AMinBorderSize: Integer;
                   AMaxBorderSize: Integer;
                   AMinOffset: Integer;
                   AMaxOffset: Integer);
  var
    I, ANewLineCount, ANewHeight: Integer;
  begin
    AAddToSplitted := 0;
    AOldMax := AOldMin + ASplitPos;

    if (V.vAlign = prvTop) or
       V.CanResizeY or
       (FpRect.Bottom - FpRect.Top < ATextHeight) then
    begin
      // top align
      AOldLineCount := Max(1, (ASplitPos - ATextStart) div ALineHeight);
      ANewLineCount := Max(0, Length(ALines) - AOldLineCount);

      if ACanResize and (ANewLineCount > 0) then
      begin
        ANewHeight := AMinOffset +
                      AMaxOffset +
                      CalcMemoHeight(ANewLineCount, ALineHeight) +
                      AMinBorderSize +
                      AMaxBorderSize;
        AAddToSplitted := Max(0, ANewHeight - (ANewMax - ANewMin - ASplitPos));
      end
      else
        ANewHeight := ANewMax - ANewMin - ASplitPos;
    end
    else
    begin
      AOldLineCount := Max(0, (ASplitPos - ATextStart) div ALineHeight);
      ANewLineCount := Max(0, Length(ALines) - AOldLineCount);

      if v.vAlign = prvCenter then
      begin
        // center align
        ANewHeight := ANewMax - ANewMin - ASplitPos;

        // change alignment
        for I := 0 to Result.Versions.Count - 1 do
          TprMemoObjRecVersion(Result.Versions[I]).vAlign := prvBottom;

        for I := 0 to Versions.Count - 1 do
          TprMemoObjRecVersion(Versions[I]).vAlign := prvTop;
      end
      else
      begin
        // bottom align
        ANewHeight := AMinOffset +
                      AMaxOffset +
                      CalcMemoHeight(ANewLineCount, ALineHeight) +
                      AMinBorderSize +
                      AMaxBorderSize;
        if ANewHeight > ANewMax - ANewMin - ASplitPos then
          AAddToSplitted := Max(0, ANewHeight - (ANewMax - ANewMin - ASplitPos))
        else
          ANewHeight := ANewMax - ANewMin - ASplitPos;
      end;
    end;

    ANewMin := 0;
    ANewMax := ANewMin + ANewHeight;
  end;

begin
  Result := CreateCopy;

  with rdi do
  begin
    kx := 1;
    ky := 1;
    prnkx := 1;
    prnky := 1;
    IsPrinter := False;
    ppdi := nil;
  end;

  V := TprMemoObjRecVersion(Versions[CurVersion]);
  V.CalcLayout(ScreenDC,
               @rdi,
               False,
               Rect(0, 0, Width, Height),
               ARect,
               AInnerRect,
               ATextHeight,
               ATopLeft,
               ALeftBorderSize,
               ATopBorderSize,
               ARightBorderSize,
               ABottomBorderSize,
               ALineHeight,
               ALinesWidth,
               ALines);

  if v.Rotate90 then
  begin
    _Split(FpRect.Left,
           FpRect.Right,
           TprObjRecAccess(Result).FpRect.Left,
           TprObjRecAccess(Result).FpRect.Right,
           ALineCount,
           ATopLeft.X,
           v.CanResizeX,
           ALeftBorderSize,
           ARightBorderSize,
           oLeft,
           oRight);
  end
  else
  begin
    _Split(FpRect.Top,
           FpRect.Bottom,
           TprObjRecAccess(Result).FpRect.Top,
           TprObjRecAccess(Result).FpRect.Bottom,
           ALineCount,
           ATopLeft.Y,
           v.CanResizeY,
           ATopBorderSize,
           ABottomBorderSize,
           oTop,
           oBottom);
  end;

  // delete lines from below version
  for I := 0 to Versions.Count - 1 do
  begin
    V := TprMemoObjRecVersion(Versions[I]);

    V.CalcLayout(ScreenDC,
                 @rdi,
                 False,
                 Rect(0, 0, Width, Height),
                 ARect,
                 AInnerRect,
                 ATextHeight,
                 ATopLeft,
                 ALeftBorderSize,
                 ATopBorderSize,
                 ARightBorderSize,
                 ABottomBorderSize,
                 ALineHeight,
                 ALinesWidth,
                 ALines);
    
    if Length(ALines) > ALineCount then
      V.Memo.Text := Copy(V.Memo.Text, ALines[ALineCount].Start, MaxInt)
    else
      V.Memo.Clear;
  end;

  // delete lines from above version
  if ALineCount >= 0 then
    for I := 0 to Result.Versions.Count - 1 do
    begin
      V := TprMemoObjRecVersion(Result.Versions[I]);

      V.CalcLayout(ScreenDC,
                   @rdi,
                   False,
                   Rect(0, 0, Width, Height),
                   ARect,
                   AInnerRect,
                   ATextHeight,
                   ATopLeft,
                   ALeftBorderSize,
                   ATopBorderSize,
                   ARightBorderSize,
                   ABottomBorderSize,
                   ALineHeight,
                   ALinesWidth,
                   ALines);

      if Length(ALines) >= ALineCount then
        V.Memo.Text := Copy(V.Memo.Text, 1, ALines[ALineCount - 1].Start + ALines[ALineCount - 1].Length - 1)
      else
        V.Memo.Clear;
    end;
end;

procedure TprMemoObjRec.SecondPass;
var
  V: TprMemoObjRecVersion;
  I: Integer;
begin
  inherited; // calculate version of object

  for I := 0 to Versions.Count - 1 do
    if TprMemoObjRecVersion(Versions[I]).SecondPassNeeded then
    begin
      V := TprMemoObjRecVersion(Versions[I]);
      Container.FormatStrings(V.Memo,
                              V.Memo,
                              V.DeleteEmptyLines,
                              V.DeleteEmptyLinesAtEnd);
    end;
end;

/////////////////////////////////////////////////
//
// TprMemoObj
//
/////////////////////////////////////////////////
function TprMemoObj.GetVersion(Index: Integer): TprMemoObjRecVersion;
begin
  Result := TprMemoObjRecVersion(inherited Versions[Index]);
end;

function TprMemoObj.GetGenVersion(Index: Integer): TprMemoObjRecVersion;
begin
  Result := TprMemoObjRecVersion(inherited GenVersions[Index]);
end;

function TprMemoObj.GetDefVersion: TprMemoObjRecVersion;
begin
  Result := TprMemoObjRecVersion(inherited DefVersion);
end;

function TprMemoObj.GetGenCurVersion: TprMemoObjRecVersion;
begin
  Result := TprMemoObjRecVersion(inherited GenCurversion);
end;

function TprMemoObj.DsgnAllowInplaceEdit : boolean;
begin
Result := true;
end;

procedure TprMemoObj.OnDsgnPopupMenuClick(Sender : TObject);
begin
with TprMemoObjRecVersion(DefVersion) do
  case TMenuItem(Sender).Tag of
    -1: hAlign := prhLeft;
    -2: hAlign := prhCenter;
    -3: hAlign := prhRight;
    -4: vAlign := prvTop;
    -5: vAlign := prvCenter;
    -6: vAlign := prvBottom;
    -7: DeleteEmptyLines := not DeleteEmptyLines;
    -8: DeleteEmptyLinesAtEnd := not DeleteEmptyLinesAtEnd;
    -9: CanResizeX := not CanResizeX;
    -10: CanResizeY := not CanResizeY;
    -11: WordWrap := not WordWrap;
    -12: hAlign := prhJustify;
  end;
DsgnNotifyDesigner;
end;

procedure TprMemoObj.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
var
  m : TMenuItem;
begin
inherited;
if Popup.Items.Count>0 then
  AddPopupMenuItem(Popup,nil,0,'',nil,'',0,false,false);
m := AddPopupMenuItem(Popup,nil,sHorizontalAlignment,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sHorizontalAlignLeft,'HLEFT',OnDsgnPopupMenuClick,'',-1,true,TprMemoObjRecVersion(DefVersion).hAlign=prhLeft);
AddPopupMenuItem(Popup,m,sHorizontalAlignCenter,'HCENTER',OnDsgnPopupMenuClick,'',-2,true,TprMemoObjRecVersion(DefVersion).hAlign=prhCenter);
AddPopupMenuItem(Popup,m,sHorizontalAlignRight,'HRIGHT',OnDsgnPopupMenuClick,'',-3,true,TprMemoObjRecVersion(DefVersion).hAlign=prhRight);
AddPopupMenuItem(Popup,m,sTextAlignJustify,'HJUSTIFY',OnDsgnPopupMenuClick,'',-12,true,TprMemoObjRecVersion(DefVersion).hAlign=prhJustify);

m := AddPopupMenuItem(Popup,nil,sVerticalAlignment,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sVerticalAlignTop,'VTOP',OnDsgnPopupMenuClick,'',-4,true,TprMemoObjRecVersion(DefVersion).vAlign=prvTop);
AddPopupMenuItem(Popup,m,sVerticalAlignCenter,'VCENTER',OnDsgnPopupMenuClick,'',-5,true,TprMemoObjRecVersion(DefVersion).vAlign=prvCenter);
AddPopupMenuItem(Popup,m,sVerticalAlignBottom,'VBOTTOM',OnDsgnPopupMenuClick,'',-6,true,TprMemoObjRecVersion(DefVersion).vAlign=prvBottom);

AddPopupMenuItem(Popup,nil,0,'',nil,'',0,false,false);

m := AddPopupMenuItem(Popup,nil,sAddititional,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sDeleteEmptyLines,'',OnDsgnPopupMenuClick,'',-7,true,TprMemoObjRecVersion(DefVersion).DeleteEmptyLines);
AddPopupMenuItem(Popup,m,sDeleteEmptyLinesAtEnd,'',OnDsgnPopupMenuClick,'',-8,true,TprMemoObjRecVersion(DefVersion).DeleteEmptyLinesAtEnd);
AddPopupMenuItem(Popup,m,sResizeHorizontally,'',OnDsgnPopupMenuClick,'',-9,true,TprMemoObjRecVersion(DefVersion).CanResizeX);
AddPopupMenuItem(Popup,m,sResizeVertically,'',OnDsgnPopupMenuClick,'',-10,true,TprMemoObjRecVersion(DefVersion).CanResizeY);
AddPopupMenuItem(Popup,m,sWordWrap,'',OnDsgnPopupMenuClick,'',-11,true,TprMemoObjRecVersion(DefVersion).WordWrap);
end;

procedure TprMemoObj.InplaceEdit;
var
  r : TRect;
  v : TprMemoObjRecVersion;
begin
v := TprMemoObjRecVersion(DefVersion);
InplaceEditor := TMemo.Create(_Parent);

with TMemo(InplaceEditor) do
  begin
    WordWrap := false;
    ParentCtl3D := False;
    Ctl3D := False;
    TabStop := False;
    BorderStyle := bsNone;
    DoubleBuffered := False;

    if v.Rotate90 then
      begin
        Left := InplaceRect.Left+GetBW(v.lBorder);
        Top := InplaceRect.Top+GetBW(v.tBorder);
        Width := InplaceRect.Bottom-InplaceRect.Top-GetBW(v.tBorder)-GetBW(v.bBorder);
        Height := InplaceRect.Right-InplaceRect.Left-GetBW(v.lBorder)-GetBW(v.rBorder);
      end
    else
      begin
        Left := InplaceRect.Left+GetBW(v.lBorder);
        Top := InplaceRect.Top+GetBW(v.tBorder);
        Width := InplaceRect.Right-InplaceRect.Left-GetBW(v.lBorder)-GetBW(v.rBorder);
        Height := InplaceRect.Bottom-InplaceRect.Top-GetBW(v.tBorder)-GetBW(v.bBorder);
      end;
    if v.FillColor=clNone then
      Color := clWindow
    else
      Color := v.FillColor;
    Parent := _Parent;

    Font.Assign(v.Font);
    Lines.Assign(v.Memo);

    r := Rect(0,0,Width,Height);
    SendMessage(InplaceEditor.Handle, EM_SETRECT, 0, LongInt(@R));
    SendMessage(InplaceEditor.Handle, EM_SETSEL, 0, 0);

    Show;
    SetFocus;
  end;
end;

procedure TprMemoObj.SaveInplaceEdit;
begin
TprMemoObjRecVersion(dRec.Versions[dRec.DefVersion]).Memo.Assign(TMemo(InplaceEditor).Lines);
end;

procedure TprMemoObj.InitdRec;
begin
FdRec := TprMemoObjRec.Create(nil,Self);
TprExObjRecVersion(dRec.Versions.Add).InitInDesigner;
end;

function TprMemoObj.GetDesc;
var
  i : integer;
  v : TprMemoObjRecVersion;
begin
v:=TprMemoObjRecVersion(dRec.Versions[dRec.DefVersion]);
i:=0;
while (i<v.Memo.Count) and
      (Trim(v.Memo[i])='') do Inc(i);
if i<v.Memo.Count then
  Result:=Trim(v.Memo[i])
else
  Result:=inherited GetDesc;
end;

function TprMemoObj.DsgnIsTransparent: Boolean;
begin
  Result := DefVersion.FillColor = clNone;
end;

procedure TprMemoObj.DrawDesign(DC: HDC; ExData: Pointer; const DrawRect: TRect);
var
  rdi: rprDrawInfo;
begin
  with rdi do
  begin
    kx := 1;
    ky := 1;
    prnkx := 1;
    prnky := 1;
    IsPrinter := False;
    Report := nil;
    ppdi := nil;
  end;
  DefVersion.InternalDraw(DrawRect, DC, @rdi, True);
end;

procedure TprMemoObj.FirstPass;
var
  V: TprMemoObjRecVersion;
  sz: tagSize;
  I, AMaxWidth, ATextSize, ALinesSize: Integer;
  OldFont, NewFont: HFONT;
  AAutoLinesSize, AAutoTextSize, ManuallyProcessed: Boolean;
  ALines: TvgrWrapLineDynArray;
begin
  if FaRec = nil then
    FaRec := TprMemoObjRec.Create(nil,Self);
  aRec.Assign(dRec);

  // try to calculate version
  aRec.FirstPass;

  // call event (OnFirstPassObject)
  DoOnFirstPassObject(ManuallyProcessed);

  // calculate aRec.Memo
  if not ManuallyProcessed then
    for I := 0 to aRec.Versions.Count-1 do
      with TprMemoObjRecVersion(aRec.Versions[i]) do
        SecondPassNeeded := not Band.Report.FormatStrings(Memo,
                                                          Memo,
                                                          DeleteEmptyLines,
                                                          DeleteEmptyLinesAtEnd);

  V := TprMemoObjRecVersion(aRec.Versions[aRec.CurVersion]);

  aRec.pRect := dRec.pRect;

  ATextSize := 0;
  ALinesSize := 0;

  if V.Rotate90 then
  begin
    NewFont := Create90Font(V.Font);
    OldFont := SelectObject(ScreenDC, NewFont);
    AAutoTextSize := V.CanResizeY;
    AAutoLinesSize := V.CanResizeX;
    AMaxWidth := dRec.Height - GetBW(v.rBorder) - GetBW(v.lBorder) - oTop - oBottom;
  end
  else
  begin
    NewFont := CreateAPIFont(v.Font);
    OldFont := SelectObject(ScreenDC,NewFont);
    AAutoTextSize := V.CanResizeX;
    AAutoLinesSize := V.CanResizeY;
    AMaxWidth := dRec.Width - GetBW(v.rBorder) - GetBW(v.lBorder) - oLeft - oRight; 
  end;

  try
    if not V.WordWrap and AAutoTextSize then
    begin
      ATextSize := 0;
      for I := 0 to V.Memo.Count - 1 do
      begin
        GetTextExtentPoint32(ScreenDC, PChar(V.Memo[i]), Length(V.Memo[i]), sz);
        if ATextSize < sz.cx then
          ATextSize := sz.cx;
      end;
    end;

    if AAutoLinesSize then
    begin
      WrapMemo(ScreenDC,
               V.Memo.Text,
               AMaxWidth,
               ALines,
               V.WordWrap,
               V.DeleteEmptyLines,
               V.DeleteEmptyLinesAtEnd);

      ALinesSize := CalcMemoHeight(ScreenDC, Length(ALines));
    end;

  finally
    SelectObject(ScreenDC, OldFont);
    DeleteObject(NewFont);
  end;

  if V.Rotate90 then
  begin
    if V.CanResizeX then
      TprMemoObjRec(aRec).FpRect.Right := TprMemoObjRec(aRec).FpRect.Left + ALinesSize + GetBW(V.lBorder) + GetBW(V.rBorder) + oLeft + oRight;
    if V.CanResizeY then
      TprMemoObjRec(aRec).FpRect.Bottom := TprMemoObjRec(aRec).FpRect.Top + ATextSize + GetBW(V.tBorder) + GetBW(V.bBorder) + oTop + oBottom;
  end
  else
  begin
    if V.CanResizeY then
      TprMemoObjRec(aRec).FpRect.Bottom := TprMemoObjRec(aRec).FpRect.Top + ALinesSize + GetBW(V.tBorder) + GetBW(V.bBorder) + oTop + oBottom;
    if V.CanResizeX then
      TprMemoObjRec(aRec).FpRect.Right := TprMemoObjRec(aRec).FpRect.Left + ATextSize + GetBW(V.lBorder) + GetBW(V.rBorder) + oLeft + oRight;
  end;

  inherited;

{$IFNDEF PR_OLD_ALIGN_MODE}
  if V.Rotate90 then
  begin
    if V.CanResizeY then
      with TprMemoObjRec(aRec) do
        case V.vAlign of
          prvTop: OffsetRect(FpRect, 0, dRec.Height - Height);
          prvCenter: OffsetRect(FpRect, 0, (dRec.Height - Height) div 2);
        end;
  end
  else
  begin
    if V.CanResizeX then
      with TprMemoObjRec(aRec) do
        case V.hAlign of
          prhRight: OffsetRect(FpRect, dRec.Width - Width, 0);
          prhCenter: OffsetRect(FpRect, (dRec.Width - Width) div 2, 0);
        end;
  end;
{$ENDIF}
end;










type

rRTFStreamIn = record
  Text : string;
end;
pRTFStreamIn = ^rRTFStreamIn;

rRTFStreamOut = record
  Text : PChar;
  Size : integer;
  Pos : integer;
end;
pRTFStreamOut = ^rRTFStreamOut;

rRTFReplaceData = record
  hwnd : THandle;
  Offs : integer;
end;
pRTFReplaceData = ^rRTFReplaceData;

function FormatRichTextWriteCallback(dwCookie : LongInt; Buf : PByte; cb : LongInt; var pcb : LongInt) : integer; stdcall;
var
  l : integer;
begin
l := Length(pRTFStreamIn(dwCookie)^.Text);
SetLength(pRTFStreamIn(dwCookie)^.Text,l+cb);
MoveMemory(@(pRTFStreamIn(dwCookie)^.Text[l+1]),Buf,cb);
pcb := cb;
Result := 0;
end;

function FormatRichTextReadCallback(dwCookie : LongInt; Buf : PByte; cb : LongInt; var pcb : LongInt) : integer; stdcall;
begin
with pRTFStreamOut(dwCookie)^ do
  begin
    pcb := Min(cb,Size-Pos);
    MoveMemory(Buf,@(Text[Pos]),pcb);
    Pos := Pos+pcb;
  end;
Result := 0;
end;

procedure RTFReplaceCallback(FromPos,Count : integer; const Buf : PChar; BufSize : integer; Flags : TprFormatReplaceCallBackOptionsSet; CallBackData : pointer);
var
  cr : _CHARRANGE;
  es : _EDITSTREAM;
  rSO : rRTFStreamOut;
  rd : pRTFReplaceData;
begin
rd := pRTFReplaceData(CallBackData);
cr.cpMin := rd.Offs+FromPos-1;
cr.cpMax := rd.Offs+FromPos+Count-1;
rd.Offs := rd.Offs+BufSize-Count;
SendMessage(rd.hwnd,EM_EXSETSEL,0,integer(@cr));
if prfrcRTF in Flags then
  begin
    rSO.Text := Buf;
    rSO.Size := BufSize;
    rSO.Pos := 0;
    es.dwCookie := integer(@rSO);
    es.pfnCallback := @FormatRichTextReadCallback;
    SendMessage(rd.hwnd,EM_STREAMIN,SFF_SELECTION or SF_RTF,integer(@es));
  end
else
  begin
    SendMessage(rd.hwnd,EM_REPLACESEL,0,integer(Buf));
  end;
end;

function FormatRichText;
var
  es : _EDITSTREAM;
  cr : _CHARRANGE;
  rd : rRTFReplaceData;
  rSI : rRTFStreamIn;
  s : string;
  i,LinesCount,LineIndex : integer;
begin
// write all text into stream
rSI.Text := '';
ZeroMemory(@es,sizeof(es));
es.dwCookie := integer(@rSI);
es.pfnCallback := @FormatRichTextWriteCallback;
SendMessage(hwnd,EM_STREAMOUT,SF_TEXT,integer(@es));
rd.hwnd := hwnd;
rd.Offs := 0;
Result := TprParser(TprCustomReportAccess(Report).Parser).FormatTemplateEx(rSI.Text,RTFReplaceCallback,pointer(@rd),s);
if Result then
  begin
    // delete empty lines
    LinesCount := SendMessage(hwnd,EM_GETLINECOUNT,0,0);
    if DeleteEmptyLines then
      begin
        i := 0;
        while i<LinesCount do
          begin
            LineIndex := SendMessage(hwnd,EM_LINEINDEX,i,0);
            if SendMessage(hwnd,EM_LINELENGTH,LineIndex,0)=0 then
              begin
                // empty line
                if i=LinesCount-1 then
                  begin
                    cr.cpMin := LineIndex-1;
                    cr.cpMax := -1;
                  end
                else
                  begin
                    cr.cpMin := LineIndex;
                    cr.cpMax := SendMessage(hwnd,EM_LINEINDEX,i+1,0)
                  end;
                SendMessage(hwnd,EM_EXSETSEL,0,integer(@cr));
                SendMessage(hwnd,EM_REPLACESEL,0,integer(PChar('')));
                Dec(LinesCount);
              end
            else
              Inc(i);
          end;
      end
    else
      if DeleteEmptyLinesAtEnd then
        begin
          i := LinesCount-1;
          while i>=0 do
            begin
              LineIndex := SendMessage(hwnd,EM_LINEINDEX,i,0);
              if SendMessage(hwnd,EM_LINELENGTH,LineIndex,0)<>0 then
                begin
                  if i<LinesCount-1 then
                    begin
                      cr.cpMin := SendMessage(hwnd,EM_LINEINDEX,i+1,0)-1;
                      cr.cpMax := -1;
                      SendMessage(hwnd,EM_EXSETSEL,0,integer(@cr));
                      SendMessage(hwnd,EM_REPLACESEL,0,integer(PChar('')));
                    end;
                  break;
                end;
              Dec(i);
            end
        end;
  end;
end;

function GetRichTextRect(APixelsRect: TRect; ADpiX, ADpiY: Integer; AWordWrap, AIgnoreBottomBound: Boolean): TRect;
begin
  with APixelsRect do
  begin
    Result.Left := MulDiv(Left, 1440, ADpiX);
    Result.Top := MulDiv(Top, 1440, ADpiY);
    if AWordWrap then
      Result.Right := MulDiv(Right, 1440, ADpiX)
    else
      Result.Right := MaxRichTextWidthTwips;
    if AIgnoreBottomBound then
      Result.Bottom := MaxRichTextHeightTwips
    else
      Result.Bottom := MulDiv(Bottom, 1440, ADpiY);
  end;
end;

procedure CopyRichText(hwndSource, hwndDest: HWND);
var
  ms: TMemoryStream;
  ALength: Integer;
begin
  ms := TMemoryStream.Create;
  try
    SaveRichTextToStream(hwndSource, ms, ALength);
    ms.Seek(0, soFromBeginning);
    LoadRichTextFromStream(hwndDest, ms, ALength, SF_RTF);
  finally
    ms.Free;
  end;
end;

procedure CopyRichText(hwndSource: HWND; RichEditDest: TRichEdit);
var
  ms: TMemoryStream;
  ALength: Integer;
begin
  ms := TMemoryStream.Create;
  try
    SaveRichTextToStream(hwndSource, ms, ALength);
    ms.Seek(0, soFromBeginning);
    RichEditDest.Lines.LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure CopyRichText(RichEditSource: TRichEdit; hwndDest: HWND);
begin
  CopyRichText(RichEditSource.Handle, hwndDest);
end;

procedure DrawRichText(ADC: HDC; hwnd: HWND; const APixelsRect: TRect; AWordWrap: Boolean; AIgnoreBottomBound: Boolean);
var
  ADpiX, ADpiY: Integer;
  AFormatRange: _FORMATRANGE;
  AOldRgn, ANewRgn: HRGN;
  ALastIndex: Integer;
  AOrgEx: TPoint;
begin
  ADpiX := GetDeviceCaps(ADC, LOGPIXELSX);
  ADpiY := GetDeviceCaps(ADC, LOGPIXELSY);

  ZeroMemory(@AFormatRange, SizeOf(AFormatRange));
  with AFormatRange do
  begin
    hdc := ADC;
    hdcTarget := ADC;
    chrg.cpMin := 0;
    chrg.cpMax := -1;
    rc := GetRichTextRect(APixelsRect, ADpiX, ADpiY, AWordWrap, AIgnoreBottomBound);
    rcPage := rc;
  end;

{$IFDEF DEBUG}
  with AFormatRange.rc do
    DbgFileStrFmt('DrawRichText: AFormatRange.rc (BEFORE) = {%d, %d, %d, %d)'#13#10, [Left, Top, Right, Bottom]);
{$ENDIF}

  AOldRgn := CreateRectRgnIndirect(Rect(0, 0, 0, 0));
  if GetClipRgn(ADC, AOldRgn) = 0 then
  begin
    DeleteObject(AOldRgn);
    AOldRgn := 0;
  end;
  // The SelectClipRgn function ignores the ViewPort point, therefore we must
  // add it to the clipping region
  GetViewportOrgEx(ADC, AOrgEx);
  with APixelsRect, AOrgEx do
    ANewRgn := CreateRectRgn(Left + X, Top + Y, Right + X, Bottom + Y);
  SelectClipRgn(ADC, ANewRgn);

  ALastIndex := SendMessage(hwnd, EM_FORMATRANGE, 1, integer(@AFormatRange));
  SendMessage(hwnd, EM_FORMATRANGE, 1, 0);

  DeleteObject(ANewRgn);
  SelectClipRgn(ADC, AOldRgn);
  if AOldRgn <> 0 then
    DeleteObject(AOldRgn);

{$IFDEF DEBUG}
  with AFormatRange.rc do
    DbgFileStrFmt('DrawRichText: LastIndex: %d, AFormatRange.rc = {%d, %d, %d, %d)'#13#10, [ALastIndex, Left, Top, Right, Bottom]);
{$ENDIF}
end;

procedure MeasureRichText(ADC: HDC;
                          hwnd: HWND;
                          const APixelsRect: TRect;
                          AWordWrap: Boolean;
                          AIgnoreBottomBound: Boolean;
                          var AMeasuredPixelsRect: TRect;
                          var AFittedChars: Integer;
                          var AAllCharsFitted: Boolean);
var
  ADpiX, ADpiY: Integer;
  AFormatRange: _FORMATRANGE;
  ATotalChars: Integer;
  S: string;
begin
  ADpiX := GetDeviceCaps(ADC, LOGPIXELSX);
  ADpiY := GetDeviceCaps(ADC, LOGPIXELSY);

  ZeroMemory(@AFormatRange, SizeOf(AFormatRange));
  with AFormatRange do
  begin
    hdc := ADC;
    hdcTarget := ADC;
    chrg.cpMin := 0;
    chrg.cpMax := -1;
    rc := GetRichTextRect(APixelsRect, ADpiX, ADpiY, AWordWrap, AIgnoreBottomBound);
    rcPage := rc;
  end;

  AFittedChars := SendMessage(hwnd, EM_FORMATRANGE, 0, integer(@AFormatRange));
  SendMessage(hwnd, EM_FORMATRANGE, 0, 0);

  with AFormatRange.rc do
  begin
    AMeasuredPixelsRect.Left := MulDiv(Left, ADpiX, 1440);
    AMeasuredPixelsRect.Top := MulDiv(Top, ADpiY, 1440);
    AMeasuredPixelsRect.Right := MulDiv(Right, ADpiX, 1440);
    AMeasuredPixelsRect.Bottom := MulDiv(Bottom, ADpiY, 1440);
  end;

  ATotalChars := GetRichEditTextLength(hwnd);

  AAllCharsFitted := ATotalChars <= AFittedChars;
  if not AAllCharsFitted then
  begin
    SendMessage(hwnd, EM_SETSEL, AFittedChars, -1);
    S := GetRichEditText(hwnd, True);
    AAllCharsFitted := Length(S) <= 0;
  end;
end;

type
  rSaveLoadRichTextFromStreamData = record
    Stream: TStream;
    Position: Integer;
    Length: Integer;
  end;
  pSaveLoadRichTextFromStreamData = ^rSaveLoadRichTextFromStreamData;

function SaveRichTextToStreamCallback(dwCookie: LongInt; Buf: PByte; cb: LongInt; var pcb: LongInt): Integer; stdcall;
begin
  with pSaveLoadRichTextFromStreamData(dwCookie)^ do
  begin
    pcb := Stream.Write(Buf^, cb);
    Position := Position + pcb;
    Length := Length + pcb;
  end;

  Result := 0;
end;

function LoadRichTextFromStreamCallback(dwCookie: LongInt; Buf: PByte; cb: LongInt; var pcb: LongInt): Integer; stdcall;
begin
  with pSaveLoadRichTextFromStreamData(dwCookie)^ do
  begin
    if Position = Length then
      pcb := 0
    else
    begin
      pcb := Stream.Read(Buf^, Min(cb, Length - Position));
      Position := Position + pcb;
    end;
  end;

  Result := 0;
end;

procedure SaveRichTextToStream(hwnd: HWND; AStream: TStream; var ARichTextLength: Integer);
begin
  SaveRichTextToStream(hwnd, AStream, ARichTextLength, SF_RTF);
end;

procedure SaveRichTextToStream(hwnd: HWND; AStream: TStream; var ARichTextLength: Integer; AFlags: Integer);
var
  AData: rSaveLoadRichTextFromStreamData;
  AEditStream: _EDITSTREAM;
begin
  with AData do
  begin
    Stream := AStream;
    Position := 0;
    Length := 0;
  end;

  ZeroMemory(@AEditStream, SizeOf(AEditStream));
  with AEditStream do
  begin
    dwCookie := Integer(@AData);
    pfnCallback := @SaveRichTextToStreamCallback;
  end;

  SendMessage(hwnd, EM_STREAMOUT, AFlags, Integer(@AEditStream));

  ARichTextLength := AData.Length;
end;

procedure LoadRichTextFromStream(hwnd: HWND; AStream: TStream; ARichTextLength: Integer; AFlags: Integer);
var
  AData: rSaveLoadRichTextFromStreamData;
  AEditStream: _EDITSTREAM;
begin
  with AData do
  begin
    Stream := AStream;
    Position := 0;
    Length := ARichTextLength;
  end;

  ZeroMemory(@AEditStream, SizeOf(AEditStream));
  with AEditStream do
  begin
    dwCookie := Integer(@AData);
    pfnCallback := @LoadRichTextFromStreamCallback;
  end;

  SendMessage(hwnd, EM_STREAMIN, AFlags, Integer(@AEditStream));
end;

procedure SaveRichTextToFile(hwnd: HWND; const AFileName: string);
var
  AFileStream: TFileStream;
  ATemp: Integer;
begin
  AFileStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveRichTextToStream(hwnd, AFileStream, ATemp);
  finally
    AFileStream.Free;
  end;
end;

procedure LoadRichTextFromFile(hwnd: HWND; const AFileName: string);
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadRichTextFromStream(hwnd, AFileStream, AFileStream.Size, SF_RTF);
  finally
    AFileStream.Free;
  end;
end;

function GetRichEditText(hwnd: HWND; ASelectionOnly: Boolean): string;
var
  AMemoryStream: TMemoryStream;
  ALength: Integer;
begin
  AMemoryStream := TMemoryStream.Create;
  try
    if ASelectionOnly then
      SaveRichTextToStream(hwnd, AMemoryStream, ALength, SF_TEXT or SFF_SELECTION)
    else
      SaveRichTextToStream(hwnd, AMemoryStream, ALength, SF_TEXT);
    SetLength(Result, ALength);
    CopyMemory(@(Result[1]), AMemoryStream.Memory, ALength);
  finally
    AMemoryStream.Free;
  end;
end;

function GetRichEditRtfText(hwnd: HWND; ASelectionOnly: Boolean): string;
var
  AMemoryStream: TMemoryStream;
  ALength: Integer;
begin
  AMemoryStream := TMemoryStream.Create;
  try
    if ASelectionOnly then
      SaveRichTextToStream(hwnd, AMemoryStream, ALength, SF_RTF or SFF_SELECTION)
    else
      SaveRichTextToStream(hwnd, AMemoryStream, ALength, SF_RTF);
    SetLength(Result, ALength);
    CopyMemory(@(Result[1]), AMemoryStream.Memory, ALength);
  finally
    AMemoryStream.Free;
  end;
end;

procedure SetRichEditRtfText(hwnd: HWND; const ARtfText: string);
var
  AMemoryStream: TMemoryStream;
begin
  AMemoryStream := TMemoryStream.Create;
  try
    AMemoryStream.Write(ARtfText[1], Length(ARtfText));
    AMemoryStream.Seek(0, soFromBeginning);
    LoadRichTextFromStream(hwnd, AMemoryStream, Length(ARtfText), SF_RTF);
  finally
    AMemoryStream.Free;
  end;
end;

function GetRichEditTextLengthCallback(dwCookie: LongInt; Buf: PByte; cb: LongInt; var pcb: LongInt): Integer; stdcall;
begin
  PInteger(dwCookie)^ := PInteger(dwCookie)^ + cb; 
  pcb := cb;
  Result := 0;
end;

function GetRichEditRtfTextLength(hwnd: HWND): Integer;
var
  AEditStream: _EDITSTREAM;
begin
  Result := 0;
  ZeroMemory(@AEditStream, SizeOf(AEditStream));
  with AEditStream do
  begin
    dwCookie := Integer(@Result);
    pfnCallback := @GetRichEditTextLengthCallback;
  end;

  SendMessage(hwnd, EM_STREAMOUT, SF_RTF, Integer(@AEditStream))
end;

function GetRichEditTextLength(hwnd: HWND): Integer;
var
  AEditStream: _EDITSTREAM;
begin
  Result := 0;
  ZeroMemory(@AEditStream, SizeOf(AEditStream));
  with AEditStream do
  begin
    dwCookie := Integer(@Result);
    pfnCallback := @GetRichEditTextLengthCallback;
  end;

  SendMessage(hwnd, EM_STREAMOUT, SF_TEXT, Integer(@AEditStream))
end;

/////////////////////////////////////////////////
//
// TprRichObjRecVersion
//
/////////////////////////////////////////////////
constructor TprRichObjRecVersion.Create;
var
  I: Integer;
begin
  inherited;

  FlBorder := TprFrameLine.Create;
  FtBorder := TprFrameLine.Create;
  FrBorder := TprFrameLine.Create;
  FbBorder := TprFrameLine.Create;
  FWordWrap := True;

  // load RTF dll
  for I := 1 to MaxRtfLibraries do
  begin
    RtfDllHandle :=  LoadLibrary(PChar(RtfLibraries[I]));
    if RtfDllHandle <> 0 then
    begin
      RtfDllName := RtfLibraries[I];
      break;
    end;
  end;

  FhwndRich := CreateRichEditWindow;
end;

destructor TprRichObjRecVersion.Destroy;
begin
  FlBorder.Free;
  FtBorder.Free;
  FrBorder.Free;
  FbBorder.Free;

  if FhwndRich <> 0 then
  begin
    DestroyWindow(FhwndRich);
    FhwndRich := 0;
  end;

  if FhwndRichFind <> 0 then
  begin
    DestroyWindow(FhwndRichFind);
    FhwndRichFind := 0;
  end;

  inherited;
end;

type
  IRichEditOleCallback = interface(IUnknown)
    ['{00020d03-0000-0000-c000-000000000046}']
    function GetNewStorage(var stg: IStorage): HResult; stdcall;
    function GetInPlaceContext(var Frame: IOleInPlaceFrame; var Doc: IOleInPlaceUIWindow; lpFrameInfo: POleInPlaceFrameInfo): HResult; stdcall;
    function ShowContainerUI(fShow: BOOL): HResult; stdcall;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage; cp: Longint): HResult; stdcall;
    function DeleteObject(const oleobj: IOleObject): HResult; stdcall;
    function QueryAcceptData(const dataobj: IDataObject; var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD; var dataobj: IDataObject): HResult; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD; var dwEffect: DWORD): HResult; stdcall;
    function GetContextMenu(seltype: Word; const oleobj: IOleObject; const chrg: TCharRange; var menu: HMENU): HResult; stdcall;
  end;

  TprRichEditOleCallback = class(TObject, IUnknown, IRichEditOleCallback)
  public
    // IUnknown
    function QueryInterface(const iid: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Longint; stdcall;
    function _Release: Longint; stdcall;
    // IRichEditOleCallback
    function GetNewStorage(var stg: IStorage): HResult; stdcall;
    function GetInPlaceContext(var Frame: IOleInPlaceFrame; var Doc: IOleInPlaceUIWindow; lpFrameInfo: POleInPlaceFrameInfo): HResult; stdcall;
    function ShowContainerUI(fShow: BOOL): HResult; stdcall;
    function QueryInsertObject(const clsid: TCLSID; const stg: IStorage; cp: Longint): HResult; stdcall;
    function DeleteObject(const oleobj: IOleObject): HResult; stdcall;
    function QueryAcceptData(const dataobj: IDataObject; var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HResult; stdcall;
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; stdcall;
    function GetClipboardData(const chrg: TCharRange; reco: DWORD; var dataobj: IDataObject): HResult; stdcall;
    function GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD; var dwEffect: DWORD): HResult; stdcall;
    function GetContextMenu(seltype: Word; const oleobj: IOleObject; const chrg: TCharRange; var menu: HMENU): HResult; stdcall;
  end;

var
  prRichEditOleCallback: TprRichEditOleCallback;

/////////////////////////////////////////////////
//
// TprRichEditOleCallback
//
/////////////////////////////////////////////////
function TprRichEditOleCallback.QueryInterface(const iid: TGUID; out Obj): HResult;
begin
  if GetInterface(iid, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TprRichEditOleCallback._AddRef: Longint;
begin
  Result := -1;
end;

function TprRichEditOleCallback._Release: Longint;
begin
  Result := -1;
end;

function TprRichEditOleCallback.GetNewStorage(var stg: IStorage): HResult;
var
  ALockBytes: ILockBytes;
begin
  stg := nil;
  CreateILockBytesOnHGlobal(0, true, ALockBytes);
  StgCreateDocfileOnILockBytes(ALockBytes, STGM_READWRITE or STGM_SHARE_EXCLUSIVE or STGM_CREATE, 0, stg);
  ALockBytes := nil;
  Result := S_OK;
end;

function TprRichEditOleCallback.GetInPlaceContext(var Frame: IOleInPlaceFrame; var Doc: IOleInPlaceUIWindow; lpFrameInfo: POleInPlaceFrameInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TprRichEditOleCallback.ShowContainerUI(fShow: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

function TprRichEditOleCallback.QueryInsertObject(const clsid: TCLSID; const stg: IStorage; cp: Longint): HResult;
begin
  Result := S_OK;
end;

function TprRichEditOleCallback.DeleteObject(const oleobj: IOleObject): HResult;
begin
  Result := S_OK;
end;

function TprRichEditOleCallback.QueryAcceptData(const dataobj: IDataObject; var cfFormat: TClipFormat; reco: DWORD; fReally: BOOL; hMetaPict: HGLOBAL): HResult;
begin
  Result := S_OK;
end;

function TprRichEditOleCallback.ContextSensitiveHelp(fEnterMode: BOOL): HResult;
begin
  Result := S_OK;
end;

function TprRichEditOleCallback.GetClipboardData(const chrg: TCharRange; reco: DWORD; var dataobj: IDataObject): HResult;
begin
  Result := E_NOTIMPL;
end;

function TprRichEditOleCallback.GetDragDropEffect(fDrag: BOOL; grfKeyState: DWORD; var dwEffect: DWORD): HResult;
begin
  Result := E_NOTIMPL;
end;

function TprRichEditOleCallback.GetContextMenu(seltype: Word; const oleobj: IOleObject; const chrg: TCharRange; var menu: HMENU): HResult;
begin
  Result := E_NOTIMPL;
end;

function TprRichObjRecVersion.CreateRichEditWindow: HWND;
var
  S: string;
  I: Integer;
begin
  S := '';
  if RtfDllHandle = 0 then
  begin
    S := DefaultRtfWindowClass;
    Result := CreateWindowEx(0,
                              PChar(S),
                              '',
                              WS_HSCROLL or
                              WS_VSCROLL or
                              ES_NOHIDESEL or
                              ES_AUTOVSCROLL or
                              ES_MULTILINE or
                              ES_SAVESEL,
                              0, 0, DefaultRtfWindowWidth, DefaultRtfWindowHeight,
                              0,
                              0,
                              0,
                              nil);

  end
  else
  begin
    for I := 1 to MaxRtfVersions do
      with RtfVersions[I] do
      begin
        S := WindowClassName;
        Result := CreateWindowEx(0,
                                  PChar(S),
                                  '',
                                  WS_HSCROLL or
                                  WS_VSCROLL or
                                  ES_NOHIDESEL or
                                  ES_AUTOVSCROLL or
                                  ES_MULTILINE or
                                  ES_SAVESEL,
                                  0, 0, DefaultRtfWindowWidth, DefaultRtfWindowHeight,
                                  0,
                                  0,
                                  0,
                                  nil);
        if Result <> 0 then
          break;
      end;
  end;

  // This code provides the support for embedded objects into RTF text (images, OLE objects and so on).
  // This is a hack, code creates an RichTextBox.OleCallback object which
  // is used by standard RichTextBox object for support OLE objects in the RTF text.
  SendMessage(Result, EM_SETOLECALLBACK, 0, Integer(Pointer(prRichEditOleCallback as IRichEditOleCallback)));

  // set background color to white
  SendMessage(Result, EM_SETBKGNDCOLOR, 0, GetRGBColor(clWhite));

  // do it for support justify alignment
  SendMessage(Result, WM_USER + $CA{EM_SETTYPOGRAPHYOPTIONS}, $01{TO_ADVANCEDTYPOGRAPHY}, $01{TO_ADVANCEDTYPOGRAPHY});

  if Result = 0 then
    raise Exception.CreateFmt('Error while create the RichEdit window, WindowClass = [%s], GetLastError = [%d]', [S, GetLastError]);
end;

procedure TprRichObjRecVersion.SettBorder(Value: TprFrameLine);
begin
  FtBorder.Assign(Value);
end;

procedure TprRichObjRecVersion.SetlBorder(Value: TprFrameLine);
begin
  FlBorder.Assign(Value);
end;

procedure TprRichObjRecVersion.SetrBorder(Value: TprFrameLine);
begin
  FrBorder.Assign(Value);
end;

procedure TprRichObjRecVersion.SetbBorder(Value: TprFrameLine);
begin
  FbBorder.Assign(Value);
end;

procedure TprRichObjRecVersion.DefineProperties;
begin
  inherited;
  Filer.DefineBinaryProperty('RichText',
                             ReadRichText,
                             WriteRichText,
                             true);
end;

procedure TprRichObjRecVersion.ReadRichText(Stream: TStream);
var
  AByteCount: Integer;
begin
  Stream.Read(AByteCount, 4);
  LoadRichTextFromStream(FhwndRich, Stream, AByteCount, SF_RTF);
end;

procedure TprRichObjRecVersion.WriteRichText(Stream: TStream);
var
  AStartPosition, APosition, AByteCount: Integer;
begin
  AStartPosition := Stream.Position;
  Stream.Write(AByteCount, 4);
  SaveRichTextToStream(FhwndRich, Stream, AByteCount);

  APosition := Stream.Position;
  Stream.Position := AStartPosition;
  Stream.Write(AByteCount, 4);
  Stream.Position := APosition;
end;

procedure TprRichObjRecVersion.Assign;
begin
  with Source as TprRichObjRecVersion do
  begin
    CopyRichText(FhwndRich, Self.FhwndRich);
    Self.SecondPassNeeded := SecondPassNeeded;
    Self.FDeleteEmptyLinesAtEnd := FDeleteEmptyLinesAtEnd;
    Self.FDeleteEmptyLines := FDeleteEmptyLines;
    Self.FCanResizeY := FCanResizeY;
    Self.FWordWrap := FWordWrap;

    Self.FlBorder.Assign(FlBorder);
    Self.FtBorder.Assign(FtBorder);
    Self.FbBorder.Assign(FbBorder);
    Self.FrBorder.Assign(FrBorder);
  end;
  inherited;
end;

procedure TprRichObjRecVersion.InternalDraw(DC: HDC; const ARect: TRect; pdi: PPrDrawInfo);
var
  p: Integer;
  ft: TFindTextW;
  cf2: TCharFormat2;
  cf: TCharFormat;
  hwnd: THandle;
  r: TRect;
  flFind: Boolean;
  fFind: Integer;
  mf: TMetaFile;
  mfc: TMetaFileCanvas;
{$IFNDEF PRINT_RTF_AS_METAFILE}
  Flw, Ftw, Frw, Fbw: Integer;
{$ENDIF}
begin
{$IFNDEF PRINT_RTF_AS_METAFILE}
  if lBorder.Show then Flw := Max(1, Round(lBorder.Width * pdi.kx))
                  else Flw := 0;
  if tBorder.Show then Ftw := Max(1, Round(tBorder.Width * pdi.ky))
                  else Ftw := 0;
  if rBorder.Show then Frw := Max(1, Round(rBorder.Width * pdi.kx))
                  else Frw := 0;
  if bBorder.Show then Fbw := Max(1, Round(bBorder.Width * pdi.ky))
                  else Fbw := 0;
  
  if pdi.IsPrinter then
  begin
    // print mode
    DrawObjBorders(DC, ARect, lBorder, tBorder, rBorder, bBorder, Flw, Ftw, Frw, Fbw);

    r := Rect(ARect.Left + Flw + Round(oLeft * pdi.kx),
              ARect.Top + Ftw + Round(oTop * pdi.ky),
              ARect.Right - Frw - Round(oRight * pdi.kx),
              ARect.Bottom - Fbw - Round(oBottom * pdi.ky));

    DrawRichText(DC, FhwndRich, r, WordWrap, True);
  end
else
{$ENDIF}
  begin
    // Preview mode
    // object will drawn on metafile DC then the metafile wiil be painted on DC
    r := Rect(0, 0, GeneratedRect.Right - GeneratedRect.Left, GeneratedRect.Bottom - GeneratedRect.Top);
    mf := TMetaFile.Create;
    mfc := nil;
    try
      mf.Width := GeneratedRect.Right - GeneratedRect.Left;
      mf.Height := GeneratedRect.Bottom - GeneratedRect.Top;
      mfc := TMetaFileCanvas.Create(mf,0);

      DrawObjBorders(mfc.Handle, r, lBorder, tBorder, rBorder, bBorder, lBorder.Width, tBorder.Width, rBorder.Width, bBorder.Width);

      hwnd := FhwndRich;
      if not pdi.IsPrinter then
      begin
        case pdi.ppdi.DrawMode of
          dmFindFirst :
            begin
              if FhwndRichFind = 0 then
                FhwndRichFind := CreateRichEditWindow;

              CopyRichText(FhwndRich, FhwndRichFind);

              if pdi.ppdi.CaseSensitive then
                fFind := FR_MATCHCASE
              else
                fFind := 0;
//              fFind := fFind or FR_DOWN;

              ft.lpstrText := StringToOleStr(pdi.ppdi.FindText);
              ft.chrg.cpMin := 0;
              ft.chrg.cpMax := -1;

              ZeroMemory(@cf2, sizeof(cf2));
              cf2.cbSize := sizeof(cf2);
              cf2.crTextColor := Rich2FindTextTextColor;
              cf2.crBackColor := Rich2FindTextBackColor;
              cf2.dwMask := CFM_COLOR or CFM_BACKCOLOR;

              ZeroMemory(@cf, sizeof(cf));
              cf.cbSize := sizeof(cf);
              cf.crTextColor := RichFindTextTextColor;
              cf.dwMask := CFM_COLOR;

              SendMessage(FhwndRichFind, EM_SETSEL, 0, 1);

              flFind := false;
              while true do
              begin
                p := SendMessage(FhwndRichFind, EM_FINDTEXT, fFind, Integer(@ft));
                if p = -1 then break;
                flFind := true;
//                SendMessage(FhwndRichFind, EM_EXSETSEL, 0, Integer(@ft.chrgText));
                if SendMessage(FhwndRichFind, EM_SETCHARFORMAT, SCF_SELECTION, Integer(@cf2))=0 then
                  SendMessage(FhwndRichFind, EM_SETCHARFORMAT, SCF_SELECTION, Integer(@cf));
//                ft.chrg.cpMin := ft.chrgText.cpMax + 1;
              end;
              if flFind then
                pdi^.ppdi^.FindList.Add(TprFindText.CreateFT(ARect));

              hwnd := FhwndRichFind;
            end;
          dmFind :
            hwnd := FhwndRichFind;
        end;
      end;

      with R do
        R := Rect(Left + GetBW(lBorder) + oLeft,
                  Top + GetBW(tBorder) + oTop,
                  Right - GetBW(rBorder) - oRight,
                  Bottom - GetBW(tBorder) - oBottom);

      DrawRichText(mfc.Handle, hwnd, R, WordWrap, True);

      mfc.Free;
      mfc := nil;
      PlayEnhMetaFile(DC, mf.Handle, ARect);
    finally
      if mfc <> nil then
        mfc.Free;
      mf.Free;
    end;
  end;
end;

procedure TprRichObjRecVersion.Draw;
var
  ARealRect: TRect;
begin
  ARealRect := GetRealRect(pdi);
  InternalDraw(DC, ARealRect, pdi);
end;

function TprRichObjRecVersion.GetText : string;
begin
  Result := GetRichEditText(FhwndRich, False);
end;

function TprRichObjRecVersion.GetRtf: string;
begin
  Result := GetRichEditRtfText(FhwndRich, False);
end;

procedure TprRichObjRecVersion.SetRtf(const Value: string);
begin
  SetRichEditRtfText(FhwndRich, Value);
end;

procedure TprRichObjRecVersion.InitInDesigner;

  procedure InitFrameLine(fl : TprFrameLine);
  begin
    fl.Show := true;
    fl.Width := 1;
    fl.Color := clBlack;
    fl.Style := psSolid;
  end;
  
begin
  InitFrameLine(lBorder);
  InitFrameLine(rBorder);
  InitFrameLine(tBorder);
  InitFrameLine(bBorder);
end;

//////////////////////
//
// TprRichObjRec
//
//////////////////////
function TprRichObjRec.GetVersionClass: TprObjVersionClass;
begin
  Result := TprRichObjRecVersion;
end;

procedure TprRichObjRec.SecondPass;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Versions.Count - 1 do
    with TprRichObjRecVersion(Versions[I]) do
      if SecondPassNeeded then
        FormatRichText(Obj.Report, FhwndRich, DeleteEmptyLines, DeleteEmptyLinesAtEnd);
end;

function TprRichObjRec.GetSupportSplitting: Boolean;
begin
  Result := True;
end;

function TprRichObjRec.GetCanSplitValue: Boolean;
begin
  Result := FCanSplit;
end;

procedure TprRichObjRec.SetCanSplitValue(Value: Boolean);
begin
  FCanSplit := Value;
end;

function TprRichObjRec.GetCanSplit(AByHorizontal: Boolean; ASplitPos: Integer): Boolean;
var
  V: TprRichObjRecVersion;
  AMeasuredRect: TRect;
  ATotalChars, AFittedChars: Integer;
  S: string;
  AAllCharsFitted: Boolean;
begin
  Result := FCanSplit and not AByHorizontal;
  if Result then
  begin
    V := TprRichObjRecVersion(Versions[CurVersion]);

    MeasureRichText(ScreenDC,
                    V.hwndRich,
                    Rect(0,
                         0,
                         Width - GetBW(V.lBorder) - GetBW(V.rBorder) - oLeft - oRight,
                         ASplitPos - GetBW(V.tBorder) - GetBW(V.bBorder) - oTop - oBottom),
                    V.WordWrap,
                    False,
                    AMeasuredRect,
                    AFittedChars,
                    AAllCharsFitted);
    ATotalChars := GetRichEditTextLength(V.hwndRich);
{$IFDEF DEBUG}
    DbgFileStrFmt('ATotalChars = %d, AFittedChars = %d'#13#10, [ATotalChars, AFittedChars]);
{$ENDIF}

    Result := AFittedChars > 0; // at least one symbol must be fitted
    if Result then
    begin
      SendMessage(V.hwndRich, EM_SETSEL, AFittedChars, -1);
      S := GetRichEditText(V.hwndRich, True);
      Result := Length(S) > 0;
    end;
  end;
end;

function TprRichObjRec.Split(AByHorizontal: Boolean; ASplitPos: Integer; var AAddToSplitted: Integer): TprObjRec;
var
  VSource, VDest: TprRichObjRecVersion;
  I: Integer;
  AHeight, AMaxSourceHeight, ALeftBorderSize, ATopBorderSize, ARightBorderSize, ABottomBorderSize: Integer;
  AMeasuredRect: TRect;
  AFittedChars: Integer;
  AAllCharsFitted: Boolean;

  function GetHeightOfObject(V: TprRichObjRecVersion): Integer;
  var
    AMeasuredRect: TRect;
    AFittedChars: Integer;
    AAllCharsFitted: Boolean;
  begin
    MeasureRichText(ScreenDC,
                    V.hwndRich,
                    Rect(0, 0, Width - ALeftBorderSize - ARightBorderSize, 0),
                    V.WordWrap,
                    True,
                    AMeasuredRect,
                    AFittedChars,
                    AAllCharsFitted);

    if not AAllCharsFitted then
    begin
      // not all characters are fit in specified rect
      AMeasuredRect.Bottom := MaxRichTextHeightTwips;
    end;

    Result := (AMeasuredRect.Bottom - AMeasuredRect.Top) + GetBW(V.tBorder) + GetBW(v.bBorder) + oTop + oBottom;
  end;

begin
  Result := CreateCopy;

{$IFDEF DEBUG}
  DbgFileStrFmt('ASplitPos: %d'#13#10, [ASplitPos]);
{$ENDIF}

  AMaxSourceHeight := 0;
  for I := 0 to Versions.Count - 1 do
  begin
    VSource := TprRichObjRecVersion(Versions[I]);
    VDest := TprRichObjRecVersion(Result.Versions[I]);

    ALeftBorderSize := GetBW(VSource.lBorder) + oLeft;
    ARightBorderSize := GetBW(VSource.rBorder) + oRight;
    ATopBorderSize := GetBW(VSource.tBorder) + oTop;
    ABottomBorderSize := GetBW(VSource.bBorder) + oBottom;

    MeasureRichText(ScreenDC,
                    VSource.hwndRich,
                    Rect(0,
                         0,
                         Width - ALeftBorderSize - ARightBorderSize,
                         ASplitPos - ATopBorderSize - ABottomBorderSize),
                    VSource.WordWrap,
                    False,
                    AMeasuredRect,
                    AFittedChars,
                    AAllCharsFitted);

{
    SendMessage(VSource.hwndRich, EM_SETSEL, AFittedChars, -1);
    S := GetRichEditRtfText(VSource.hwndRich, True);
    SetRichEditRtfText(VSource.hwndRich, S);

    AHeight := GetHeightOfObject(VSource);
    if AHeight > AMaxSourceHeight then
      AMaxSourceHeight := AHeight;

    SendMessage(VDest.hwndRich, EM_SETSEL, 0, AFittedChars);
    S := GetRichEditRtfText(VDest.hwndRich, True);
    SetRichEditRtfText(VDest.hwndRich, S);
}


    // chars from 0 to ALastIndex - 1 are leave in the current object
    SendMessage(VSource.hwndRich, EM_SETSEL, 0, AFittedChars);
    SendMessage(VSource.hwndRich, EM_REPLACESEL, 0, Integer(PChar('')));

    AHeight := GetHeightOfObject(VSource);
    if AHeight > AMaxSourceHeight then
      AMaxSourceHeight := AHeight;

    // chars from ALastIndex moved to the new created object
    SendMessage(VDest.hwndRich, EM_SETSEL, AFittedChars, -1);
    SendMessage(VDest.hwndRich, EM_REPLACESEL, 0, Integer(PChar('')));

  end;

  FpRect.Bottom := FpRect.Top + AMaxSourceHeight;
  with TprObjRecAccess(Result) do
    FpRect.Bottom := FpRect.Top + ASplitPos;
end;

/////////////////////////////////////////////////
//
// TprRichObj
//
/////////////////////////////////////////////////
function TprRichObj.GetVersion(Index: Integer): TprRichObjRecVersion;
begin
  Result := TprRichObjRecVersion(inherited Versions[Index]);
end;

function TprRichObj.GetGenVersion(Index: Integer): TprRichObjRecVersion;
begin
  Result := TprRichObjRecVersion(inherited GenVersions[Index]);
end;

function TprRichObj.GetDefVersion: TprRichObjRecVersion;
begin
  Result := TprRichObjRecVersion(inherited DefVersion);
end;

function TprRichObj.GetGenCurVersion: TprRichObjRecVersion;
begin
  Result := TprRichObjRecVersion(inherited GenCurversion);
end;

procedure TprRichObj.InitdRec;
begin
  FdRec := TprRichObjRec.Create(nil, Self);
  TprExObjRecVersion(dRec.Versions.Add).InitInDesigner;
end;

function TprRichObj.GetDesc;
begin
Result := inherited GetDesc;
end;

procedure TprRichObj.DrawDesign(DC: HDC; ExData: pointer; const DrawRect: TRect);
var
  rdi: rprDrawInfo;
  ABrush: HBRUSH;
begin
  with rdi do
  begin
    kx := 1;
    ky := 1;
    prnkx := 1;
    prnky := 1;
    IsPrinter := True; // do not use *scaled* drawing
    Report := nil;
    ppdi := nil;
  end;

  ABrush := CreateSolidBrush(GetRGBColor(clWhite));
  with DrawRect, DefVersion do
    FillRect(DC,Rect(Left + GetBW(lBorder),
                     Top + GetBW(tBorder),
                     Right - GetBW(rBorder),
                     Bottom - GetBW(bBorder)), ABrush);
  DeleteObject(ABrush);

  DefVersion.InternalDraw(DC, DrawRect, @rdi);
end;

{
var
  V: TprRichObjRecVersion;
  r : TRect;
  fr : _FORMATRANGE;
  nbr : HBRUSH;
  OldRgn,rgn : HRGN;
  PixelsPerX,PixelsPerY : integer;
begin
  v := TprRichObjRecVersion(dRec.Versions[dRec.DefVersion]);
r := DrawRect;

nbr := CreateSolidBrush(clWhite);
FillRect(DC,Rect(r.Left+GetBW(v.lBorder),
                 r.Top+GetBW(v.tBorder),
                 r.Right-GetBW(v.rBorder),
                 r.Bottom-GetBW(v.bBorder)),nbr);
DeleteObject(nbr);

DrawDesignObjBorders(DC,r,v.lBorder,v.tBorder,v.rBorder,v.bBorder);

// now - draw Rich text
r:=Rect(r.Left+GetBW(v.lBorder)+oLeft,
        r.Top+GetBW(v.tBorder)+oTop,
        r.Right-GetBW(v.rBorder)-oRight,
        r.Bottom-GetBW(v.bBorder)-oBottom);

// r - rect available for drawing
PixelsPerX := GetDeviceCaps(DC,LOGPIXELSX);
PixelsPerY := GetDeviceCaps(DC,LOGPIXELSY);
ZeroMemory(@fr,sizeof(fr));
fr.hdc := DC;
fr.hdcTarget := DC;
fr.chrg.cpMin := -1;
fr.chrg.cpMax := -1;

if v.WordWrap then
  begin
    fr.rc := Rect(MulDiv(r.Left,1440,PixelsPerX),
                  MulDiv(r.Top,1440,PixelsPerY),
                  MulDiv(r.Right,1440,PixelsPerX),
                  MulDiv(MaxRichTextHeight,1440,PixelsPerY));
  end
else
  begin
    fr.rc := Rect(MulDiv(r.Left,1440,PixelsPerX),
                  MulDiv(r.Top,1440,PixelsPerY),
                  MulDiv(MaxRichTextWidth,1440,PixelsPerX),
                  MulDiv(MaxRichTextHeight,1440,PixelsPerY));
  end;
fr.rcPage := fr.rc;

OldRgn := CreateRectRgnIndirect(r);
GetClipRgn(DC,OldRgn);
Rgn := CreateRectRgnIndirect(r);
SelectClipRgn(DC,Rgn);
SendMessage(v.FhwndRich,EM_FORMATRANGE,1,integer(@fr));
SendMessage(v.FhwndRich,EM_FORMATRANGE,0,0);
DeleteObject(Rgn);
SelectClipRgn(DC,OldRgn);
DeleteObject(OldRgn);
end;
}

procedure TprRichObj.FirstPass;
var
  V: TprRichObjRecVersion;
  ManuallyProcessed : boolean;
  I, AFittedChars: Integer;
  AMeasuredRect: TRect;
  AAllCharsFitted: Boolean;
begin
  if FaRec = nil then
    FaRec := TprRichObjRec.Create(nil,Self);
  aRec.Assign(dRec);

  //
  aRec.FirstPass;

  //
  DoOnFirstPassObject(ManuallyProcessed);

  if not ManuallyProcessed then
    for I := 0 to aRec.Versions.Count - 1 do
      with TprRichObjRecVersion(aRec.Versions[I]) do
        SecondPassNeeded := not FormatRichText(Report, FhwndRich, DeleteEmptyLines, DeleteEmptyLinesAtEnd);

  V := TprRichObjRecVersion(aRec.Versions[aRec.CurVersion]);
  aRec.pRect := dRec.pRect;
  if V.CanResizeY then
  begin
    MeasureRichText(ScreenDC,
                    V.hwndRich,
                    Rect(0, 0, TprRichObjRec(aRec).Width - GetBW(V.rBorder) - GetBW(V.lBorder) - oLeft - oRight, 0),
                    V.WordWrap,
                    True,
                    AMeasuredRect,
                    AFittedChars,
                    AAllCharsFitted);
    if not AAllCharsFitted then
    begin
      AMeasuredRect.Bottom := MaxRichTextHeightTwips;
    end;

    TprRichObjRec(aRec).FpRect.Bottom := TprRichObjRec(aRec).FpRect.Top + (AMeasuredRect.Bottom - AMeasuredRect.Top) + GetBW(v.tBorder) + GetBW(v.bBorder) + oTop + oBottom;
  end;
  
  inherited;
end;

procedure TprRichObj.OnDsgnPopupMenuClick(Sender : TObject);
begin
with TprRichObjRecVersion(DefVersion) do
  case TMenuItem(Sender).Tag of
    -1: DeleteEmptyLines := not DeleteEmptyLines;
    -2: DeleteEmptyLinesAtEnd := not DeleteEmptyLinesAtEnd;
    -3: CanResizeY := not CanResizeY;
    -4: WordWrap := not WordWrap;
  end;
DsgnNotifyDesigner;
end;

procedure TprRichObj.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
  inherited;
  if Popup.Items.Count>0 then
    AddPopupMenuItem(Popup,nil,0,'',nil,'',0,false,false);
  AddPopupMenuItem(Popup,nil,sDeleteEmptyLines,'',OnDsgnPopupMenuClick,'',-1,true,TprRichObjRecVersion(DefVersion).DeleteEmptyLines);
  AddPopupMenuItem(Popup,nil,sDeleteEmptyLinesAtEnd,'',OnDsgnPopupMenuClick,'',-2,true,TprRichObjRecVersion(DefVersion).DeleteEmptyLinesAtEnd);
  AddPopupMenuItem(Popup,nil,sResizeVertically,'',OnDsgnPopupMenuClick,'',-3,true,TprRichObjRecVersion(DefVersion).CanResizeY);
  AddPopupMenuItem(Popup,nil,sWordWrap,'',OnDsgnPopupMenuClick,'',-4,true,TprRichObjRecVersion(DefVersion).WordWrap);
end;

/////////////////////////////////////////////////
//
// TprImageObjRecVersion
//
/////////////////////////////////////////////////
constructor TprImageObjRecVersion.Create;
begin
  inherited;
  FPicture := TPicture.Create;
  FDrawMode := prdmCenter;
end;

destructor TprImageObjRecVersion.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TprImageObjRecVersion.Assign;
begin
with Source as TprImageObjRecVersion do
  begin
    Self.FImageSource := ImageSource;
    Self.FFileName := FileName;
    Self.FDBFieldName := DBFieldName;
    Self.FDrawMode := DrawMode;
    Self.FFillColor := FillColor;
    Self.FPicture.Assign(Picture);
  end;
inherited;
end;

procedure TprImageObjRecVersion.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

function TprImageObjRecVersion.GetPicture(Report: TprCustomReport): TPersistent;
var
  f : TField;
  DataSet   : TComponent;
  FieldName : string;
  ms: TMemoryStream;
begin
  Result := nil;
case ImageSource of
  isFileName:
    begin
      Result := TPicture.Create;
      TPicture(Result).LoadFromFile(FileName);
    end;
  isDBFieldName:
    begin
      Report.TranslateObjectName(DBFieldName,DataSet,FieldName);
      if DataSet is TDataSet then
        begin
          f := TDataSet(DataSet).FindField(FieldName);
          if (f <> nil) and f.IsBlob then
            begin
              try
                Result := TPicture.Create;
                Result.Assign(f);
              except
                if Result <> nil then
                begin
                  Result.Free;
                  Result := nil;
                end;

                ms := TMemoryStream.Create;
                try
                  TBlobField(f).SaveToStream(ms);
                  try
                    ms.Seek(0, soFromBeginning);
                    Result := TBitmap.Create;
                    TBitmap(Result).LoadFromStream(ms);
                  except
                    if Result <> nil then
                    begin
                      Result.Free;
                      Result := nil;
                    end;
{$IFDEF PR_D5_D6_D7}
                    try
                      ms.Seek(0, soFromBeginning);
                      Result := TJPEGImage.Create;
                      TJPEGImage(Result).LoadFromStream(ms);
                    except
                      if Result <> nil then
                      begin
                        Result.Free;
                        Result := nil;
                      end;
                    end;
{$ENDIF}     
                  end;
                finally
                  ms.Free;
                end;
              end;
            end;
        end;
    end;
end;
end;

procedure TprImageObjRecVersion.Draw(DC: HDC; pdi: PPrDrawInfo);
const
  ADrawMode: array [TprImageDrawMode] of TvgrImageDrawMode = (vgridmCenter, vgridmStretch, vgridmStretchProp, vgridmStretch);
var
  nbr : HBRUSH;
  spn : HPEN;
  sbr : HBRUSH;
  sfn : HFONT;
  Canvas : TCanvas;
  RealRect : TRect;
  ANewPen, AOldPen: HPEN;
  ANewBrush, AOldBrush: HBRUSH;
begin
  RealRect := GetRealRect(pdi);
  SaveDCObjects(DC, spn, sbr, sfn);
  Canvas := TCanvas.Create;
  Canvas.Handle := DC;

  try
    if (FillColor<>clNone) and (FillColor<>clWhite) then
    begin
      nbr := CreateSolidBrush(GetRGBColor(FillColor));
      FillRect(Canvas.Handle,RealRect,nbr);
      DeleteObject(nbr);
    end;

    if Picture.Graphic = nil then
    begin
      ANewPen := CreatePen(PS_SOLID, 1, GetRGBColor(clRed));
      AOldPen := SelectObject(Canvas.Handle, ANewPen);
      ANewBrush := CreateSolidBrush(GetRGBColor(clWhite));
      AOldBrush := SelectObject(Canvas.Handle, ANewBrush);

      Rectangle(Canvas.Handle, RealRect.Left, RealRect.Top, RealRect.Right, RealRect.Bottom);

      MoveToEx(Canvas.Handle, RealRect.Left, RealRect.Top, nil);
      LineTo(Canvas.Handle, RealRect.Right, RealRect.Bottom);
      MoveToEx(Canvas.Handle, RealRect.Left, RealRect.Bottom, nil);
      LineTo(Canvas.Handle, RealRect.Right, RealRect.Top);

      SelectObject(Canvas.Handle, AOldPen);
      DeleteObject(ANewPen);
      SelectObject(Canvas.Handle, AOldBrush);
      DeleteObject(ANewBrush);
    end
    else
      DrawImage(Canvas, Picture.Graphic, RealRect, pdi.kx, pdi.ky, ADrawMode[DrawMode]);

  finally
    Canvas.Free;
    RestoreDCObjects(DC, spn, sbr, sfn);
  end;
end;

procedure TprImageObjRecVersion.InitInDesigner;
begin
FillColor := clNone;
end;

//////////////////////
//
// TprImageObjRec
//
//////////////////////
{
function TprImageObjRec.CreateCopy;
begin
Result:=TprImageObjRec.Create(Self.Page,Self.Obj);
Result.Assign(Self);
end;
}

function TprImageObjRec.GetVersionClass: TprObjVersionClass;
begin
  Result := TprImageObjRecVersion;
end;

procedure TprImageObjRec.SecondPass;
begin
end;

//////////////////////////////
//
// TprImageObj
//
//////////////////////////////
function TprImageObj.GetVersion(Index: Integer): TprImageObjRecVersion;
begin
  Result := TprImageObjRecVersion(inherited Versions[Index]);
end;

function TprImageObj.GetGenVersion(Index: Integer): TprImageObjRecVersion;
begin
  Result := TprImageObjRecVersion(inherited GenVersions[Index]);
end;

function TprImageObj.GetDefVersion: TprImageObjRecVersion;
begin
  Result := TprImageObjRecVersion(inherited DefVersion);
end;

function TprImageObj.GetGenCurVersion: TprImageObjRecVersion;
begin
  Result := TprImageObjRecVersion(inherited GenCurversion);
end;

procedure TprImageObj.InitdRec;
begin
FdRec := TprImageObjRec.Create(nil,Self);
TprExObjRecVersion(dRec.Versions.Add).InitInDesigner;
end;

function TprImageObj.GetDesc;
var
  v : TprImageObjRecVersion;
begin
v:=TprImageObjRecVersion(dRec.Versions[dRec.DefVersion]);
case v.ImageSource of
  isFileName   : Result:=Format(prLoadStr(sImageObjDescFileMask),[v.FileName]);
  isDBFieldName: Result:=Format(prLoadStr(sImageObjDescDBFieldMask),[v.DBFieldName]);
  else           Result:=inherited GetDesc;
end;
end;

procedure TprImageObj.DrawDesign;
const
  ADrawMode: array [TprImageDrawMode] of TvgrImageDrawMode = (vgridmCenter, vgridmStretch, vgridmStretchProp, vgridmStretch);
var
  v : TprImageObjRecVersion;
  r : TRect;
  s1 : string;
  nbr : HBRUSH;
  Canvas : TCanvas;
  nfn,ofn : HFONT;

  spn : HPEN;
  sbr : HBRUSH;
  sfn : HFONT;
begin
  v := TprImageObjRecVersion(dRec.Versions[dRec.DefVersion]);
  r := DrawRect;

  if v.FillColor = clNone then
    nbr := CreateSolidBrush(clWhite)
  else
    nbr := CreateSolidBrush(v.FillColor);
  FillRect(DC, r, nbr);
  DeleteObject(nbr);
  DrawAngleRect(DC,r);

  if v.ImageSource=isPicture then
  begin
    if (v.Picture <> nil) and (v.Picture.Graphic <> nil) then
    begin
      SaveDCObjects(DC, spn, sbr, sfn);
      Canvas := TCanvas.Create;
      Canvas.Handle := DC;
      try

        DrawImage(Canvas, v.Picture.Graphic, r, 1, 1, ADrawMode[v.DrawMode]);

      finally
        Canvas.Free;
        RestoreDCObjects(DC, spn, sbr, sfn);
      end;
    end;
  end
  else
  begin
    if v.ImageSource=isFileName then begin s1 := Format(prLoadStr(sImageObjDescFileMask),[v.FileName]) end
                                else begin s1 := Format(prLoadStr(sImageObjDescDBFieldMask),[v.DBFieldName]) end;

    nfn:=CreateDefFont(DC,8,clBlack);
    ofn:=SelectObject(DC,nfn);

    SetBkMode(DC,TRANSPARENT);
    DrawText(DC,PChar(s1),Length(s1),r,DT_LEFT);
    SetBkMode(DC,OPAQUE);

    SelectObject(DC,ofn);
    DeleteObject(nfn);
  end;
end;

procedure TprImageObj.FirstPass;
var
  v : TprImageObjRecVersion;
  p : TPersistent;
  i : integer;
  ManuallyProcessed : boolean;
begin
if FaRec=nil then
  FaRec := TprImageObjRec.Create(nil,Self);
aRec.Assign(dRec);

aRec.FirstPass;

DoOnFirstPassObject(ManuallyProcessed);

if not ManuallyProcessed then
  for i:=0 to aRec.Versions.Count-1 do
    with TprImageObjRecVersion(aRec.Versions[i]) do
      begin
        if ImageSource = isPicture then
          continue;

        p := GetPicture(Band.Report);
        if p <> nil then
        begin
          Picture.Assign(p);
          p.Free;
        end;
      end;

  v := TprImageObjRecVersion(aRec.Versions[aRec.CurVersion]);
  aRec.pRect := dRec.pRect;

  //
  if v.DrawMode=prdmResizeHeightWidth then
  begin
    if (v.Picture <> nil) and (v.Picture.Width <> 0) and (v.Picture.Height<>0) then
    begin
      TprRichObjRec(aRec).FpRect.Right := TprRichObjRec(aRec).FpRect.Left + v.Picture.Width;
      TprRichObjRec(aRec).FpRect.Bottom := TprRichObjRec(aRec).FpRect.Top + v.Picture.Height;
    end;
  end;

  inherited;
end;

procedure TprImageObj.OnDsgnPopupMenuClick(Sender : TObject);
begin
with TprImageObjRecVersion(DefVersion) do
  case TMenuItem(Sender).Tag of
    -1: DrawMode := prdmCenter;
    -2: DrawMode := prdmStretch;
    -3: DrawMode := prdmStretchProp;
    -4: DrawMode := prdmResizeHeightWidth;
  end;
DsgnNotifyDesigner;
end;

procedure TprImageObj.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
inherited;
if Popup.Items.Count>0 then
  AddPopupMenuItem(Popup,nil,0,'',nil,'',0,false,false);

AddPopupMenuItem(Popup,nil,sImageEditorDrawModeCenter,'',OnDsgnPopupMenuClick,'',-1,true,TprImageObjRecVersion(DefVersion).DrawMode=prdmCenter);
AddPopupMenuItem(Popup,nil,sImageEditorDrawModeStretch,'',OnDsgnPopupMenuClick,'',-2,true,TprImageObjRecVersion(DefVersion).DrawMode=prdmStretch);
AddPopupMenuItem(Popup,nil,sImageEditorDrawModeStretchProp,'',OnDsgnPopupMenuClick,'',-3,true,TprImageObjRecVersion(DefVersion).DrawMode=prdmStretchProp);
AddPopupMenuItem(Popup,nil,sImageEditorDrawModeAsImageSize,'',OnDsgnPopupMenuClick,'',-4,true,TprImageObjRecVersion(DefVersion).DrawMode=prdmResizeHeightWidth);
end;













procedure TprHBand_DrawDesign(Band : TprCustomHBand; DC : HDC; const DrawRect : TRect; ColCount : integer);
var
  s : string;
  sz : tagSize;
  npn,opn : HPEN;
  nfn,ofn : HFONT;
  i,ColWidth : integer;
begin
npn := CreatePen(PS_DOT,1,clBlue);
opn := SelectObject(DC,npn);

nfn := CreateDefFont(DC,8,clBlack);
ofn := SelectObject(DC,nfn);

try
  pr_Common.DrawRect(DC,DrawRect);

  s := Band.GetDrawDesignCaption;
  GetTextExtentPoint32(DC,PChar(s),Length(s),sz);
  ExtTextOut(DC,
             DrawRect.Left+2,
             DrawRect.Bottom-sz.cy-2,
             ETO_CLIPPED,
             @DrawRect,
             PChar(s),
             Length(s),
             nil);

  // Draw columns
  if ColCount>1 then
    begin
      ColWidth := (DrawRect.Right-DrawRect.Left) div ColCount;
      for i:=1 to ColCount-1 do
        begin
          MoveToEx(DC,DrawRect.Left+ColWidth*i,DrawRect.Top,nil);
          LineTo(DC,DrawRect.Left+ColWidth*i,DrawRect.Bottom);
        end;
    end;
finally
  SelectObject(DC,opn);
  SelectObject(DC,ofn);
  DeleteObject(npn);
  DeleteObject(nfn);
end;
end;

//////////////////////////
//
// TprHTitleBand
//
//////////////////////////
procedure TprHTitleBand.DrawDesign;
begin
TprHBand_DrawDesign(Self,DC,DrawRect,0);
end;

/////////////////////////
//
// TprHSummaryBand
//
/////////////////////////
procedure TprHSummaryBand.DrawDesign;
begin
TprHBand_DrawDesign(Self,DC,DrawRect,0);
end;

//////////////////////////
//
// TprHPageHeaderBand
//
//////////////////////////
procedure TprHPageHeaderBand.DrawDesign;
begin
TprHBand_DrawDesign(Self,DC,DrawRect,0);
end;

/////////////////////////////
//
// TprHPageFooterBand
//
/////////////////////////////
procedure TprHPageFooterBand.DrawDesign;
begin
TprHBand_DrawDesign(Self,DC,DrawRect,0);
end;

//////////////////////////
//
// TprHDetailBand
//
//////////////////////////
procedure TprHDetailBand.DrawDesign;
begin
TprHBand_DrawDesign(Self,DC,DrawRect,ColCount);
end;

////////////////////////////////
//
// TprHDetailHeaderBand
//
////////////////////////////////
procedure TprHDetailHeaderBand.DrawDesign;
begin
TprHBand_DrawDesign(Self,DC,DrawRect,ColCount);
end;

////////////////////////////////
//
// TprHDetailFooterBand
//
////////////////////////////////
procedure TprHDetailFooterBand.DrawDesign;
begin
TprHBand_DrawDesign(Self,DC,DrawRect,ColCount);
end;

////////////////////////////////////
//
// TprHGroupHeaderBand
//
////////////////////////////////////
procedure TprHGroupHeaderBand.DrawDesign;
begin
TprHBand_DrawDesign(Self,DC,DrawRect,ColCount);
end;

////////////////////////////////////
//
// TprHGroupFooterBand
//
////////////////////////////////////
procedure TprHGroupFooterBand.DrawDesign;
begin
TprHBand_DrawDesign(Self,DC,DrawRect,ColCount);
end;





////////////////////////////////
//
// VERTICAL BANDS
//
////////////////////////////////
procedure TprVBand_DrawDesign(Band : TprBand; DC : HDC; const DrawRect : TRect);
var
  s : string;
  sz : tagSize;
  npn,opn : HPEN;
  nfn,ofn : HFONT;
begin
npn := CreatePen(PS_DOT,1,clRed);
opn := SelectObject(DC,npn);

nfn := Create90DefFont(DC,8,clBlack);
ofn := SelectObject(DC,nfn);

try
  pr_Common.DrawRect(DC,DrawRect);

  s := Band.GetDrawDesignCaption;
  GetTextExtentPoint32(DC,PChar(s),Length(s),sz);
  ExtTextOut(DC,
             DrawRect.Right-sz.cy-2,
             DrawRect.Bottom-2,
             ETO_CLIPPED,
             @DrawRect,
             PChar(s),
             Length(s),
             nil);
finally
  SelectObject(DC,opn);
  SelectObject(DC,ofn);
  DeleteObject(npn);
  DeleteObject(nfn);
end;
end;

//////////////////////////
//
// TprVTitleBand
//
//////////////////////////
procedure TprVTitleBand.DrawDesign;
begin
TprVBand_DrawDesign(Self,DC,DrawRect);
end;

/////////////////////////
//
// TprVSummaryBand
//
/////////////////////////
procedure TprVSummaryBand.DrawDesign;
begin
TprVBand_DrawDesign(Self,DC,DrawRect);
end;

//////////////////////////
//
// TprVPageHeaderBand
//
//////////////////////////
procedure TprVPageHeaderBand.DrawDesign;
begin
TprVBand_DrawDesign(Self,DC,DrawRect);
end;

/////////////////////////////
//
// TprVPageFooterBand
//
/////////////////////////////
procedure TprVPageFooterBand.DrawDesign;
begin
TprVBand_DrawDesign(Self,DC,DrawRect);
end;

//////////////////////////
//
// TprVDetailBand
//
//////////////////////////
procedure TprVDetailBand.DrawDesign;
begin
TprVBand_DrawDesign(Self,DC,DrawRect);
end;



////////////////////////////////
//
// TprVDetailHeaderBand
//
////////////////////////////////
procedure TprVDetailHeaderBand.DrawDesign;
begin
TprVBand_DrawDesign(Self,DC,DrawRect);
end;

////////////////////////////////
//
// TprVDetailFooterBand
//
////////////////////////////////
procedure TprVDetailFooterBand.DrawDesign;
begin
TprVBand_DrawDesign(Self,DC,DrawRect);
end;

////////////////////////////////////
//
// TprVGroupHeaderBand
//
////////////////////////////////////
procedure TprVGroupHeaderBand.DrawDesign;
begin
TprVBand_DrawDesign(Self,DC,DrawRect);
end;

////////////////////////////////////
//
// TprVGroupFooterBand
//
////////////////////////////////////
procedure TprVGroupFooterBand.DrawDesign;
begin
TprVBand_DrawDesign(Self,DC,DrawRect);
end;








/////////////////////////////////////////////////
//
// TprPageScaleInfo
//
/////////////////////////////////////////////////
constructor TprPageScaleInfo.Create;
begin
inherited;
FPaperSize := A4_PaperSizeCode;
FOrientation := poPortrait;
FPageWidth := A4_PaperWidth;
FPageHeight := A4_PaperHeight;
FScaleMode := psmToDefined; //psmToNearest; //psmToDefined;
FFitObjects := true;
end;

procedure TprPageScaleInfo.Assign(Source : TPersistent);
begin
with Source as TprPageScaleInfo do
  begin
    Self.FPaperSize := PaperSize;
    Self.FOrientation := Orientation;
    Self.FPageWidth := PageWidth;
    Self.FPageHeight := PageHeight;
    Self.FScaleMode := ScaleMode;
    Self.FFitObjects := FitObjects;
  end;
end;

procedure TprPageScaleInfo.Save(Stream : TStream);
begin
Stream.Write(FPaperSize,4);
Stream.Write(FOrientation,sizeof(FOrientation));
Stream.Write(FPageWidth,4);
Stream.Write(FPageHeight,4);
Stream.Write(FScaleMode,sizeof(FScaleMode));
Stream.Write(FFitObjects,sizeof(FFitObjects));
end;

procedure TprPageScaleInfo.Load(Stream : TStream);
begin
Stream.Read(FPaperSize,4);
Stream.Read(FOrientation,sizeof(FOrientation));
Stream.Read(FPageWidth,4);
Stream.Read(FPageHeight,4);
Stream.Read(FScaleMode,sizeof(FScaleMode));
Stream.Read(FFitObjects,sizeof(FFitObjects));
end;

/////////////////////////////////////////////////
//
// TprPageInfo
//
/////////////////////////////////////////////////
constructor TprPageInfo.Create;
begin
inherited;
FOrientation := poPortrait;
FPageWidth := A4_PaperWidth; // width for A4 (210 mm)
FPageHeight := A4_PaperHeight; // height for A4 (297 mm)
FPaperSize := A4_PaperSizeCode;
FlMargin := 10; // all margins - 10 mm
FtMargin := 10;
FrMargin := 10;
FbMargin := 10;
end;

procedure TprPageInfo.SetPageWidth(Value : integer);
begin
if FPageWidth=Value then exit;
FPageWidth := Value;
DoChanged;
end;

procedure TprPageInfo.SetPageHeight(Value : integer);
begin
if FPageHeight=Value then exit;
FPageHeight := Value;
DoChanged;
end;

procedure TprPageInfo.SetPaperSize(Value : integer);
begin
if FPaperSize=Value then exit;
FPaperSize := Value;
DoChanged;
end;

procedure TprPageInfo.SetOrientation(Value : TprPrinterOrientation);
begin
if FOrientation=Value then exit;
FOrientation := Value;
DoChanged;
end;

procedure TprPageInfo.SetlMargin(Value : extended);
begin
if FlMargin=Value then exit;
FlMargin := Value;
DoChanged;
end;

procedure TprPageInfo.SettMargin(Value : extended);
begin
if FtMargin=Value then exit;
FtMargin := Value;
DoChanged;
end;

procedure TprPageInfo.SetrMargin(Value : extended);
begin
if FrMargin=Value then exit;
FrMargin := Value;
DoChanged;
end;

procedure TprPageInfo.SetbMargin(Value : extended);
begin
if FbMargin=Value then exit;
FbMargin := Value;
DoChanged;
end;

procedure TprPageInfo.DoChanged;
begin
if Assigned(FOnChange) then
  FOnChange(Self);
end;

procedure TprPageInfo.Assign(Source : TPersistent);
begin
with TprPageInfo(Source) do
  begin
    Self.FOrientation := Orientation;
    Self.FPageWidth := PageWidth;
    Self.FPageHeight := PageHeight;
    Self.FPaperSize := PaperSize;
    Self.FlMargin := lMargin;
    Self.FtMargin := tMargin;
    Self.FrMargin := rMargin;
    Self.FbMargin := bMargin;
  end;
DoChanged;
end;

procedure TprPageInfo.Save(Stream : TStream);
begin
Stream.Write(FPageWidth,4);
Stream.Write(FPageHeight,4);
Stream.Write(FPaperSize,4);
Stream.Write(FOrientation,sizeof(FOrientation));
Stream.Write(FlMargin,4);
Stream.Write(FrMargin,4);
Stream.Write(FtMargin,4);
Stream.Write(FbMargin,4);
end;

procedure TprPageInfo.Load(Stream : TStream);
begin
Stream.Read(FPageWidth,4);
Stream.Read(FPageHeight,4);
Stream.Read(FPaperSize,4);
Stream.Read(FOrientation,sizeof(FOrientation));
Stream.Read(FlMargin,4);
Stream.Read(FrMargin,4);
Stream.Read(FtMargin,4);
Stream.Read(FbMargin,4);
DoChanged;
end;

//////////////////////////
//
// TprEndPage
//
//////////////////////////
constructor TprEndPage.CreateEmpty;
begin
inherited;
FVL := TList.Create;
FPageInfo := TprPageInfo.Create;
FPageInfo.OnChange := OnPageInfoChanged;
FPageScaleInfo := TprPageScaleInfo.Create;
OnPageInfoChanged(FPageInfo);
end;

constructor TprEndPage.Create;
begin
inherited;
FPageInfo.Assign(TprPage(_Page).PageInfo);
FPageScaleInfo.Assign(TprPage(_Page).PageScaleInfo);
end;

destructor TprEndPage.Destroy;
begin
ClearVL;
FVL.Free;
FPageInfo.Free;
FPageScaleInfo.Free;
inherited;
end;

function TprEndPage.GetReport: TprReport;
begin
  Result := TprReport(inherited Report);
end;

function TprEndPage.GetWidth : integer;
begin
Result := PageInfo.PageWidth;
end;

function TprEndPage.GetHeight : integer;
begin
Result := PageInfo.PageHeight;
end;

procedure TprEndPage.ClearVL;
var
  i : integer;
begin
for i:=0 to VL.Count-1 do
  TprExObjRecVersion(VL[i]).Free;
VL.Clear;
end;

procedure TprEndPage.OnPageInfoChanged(Sender : TObject);
begin
FPixelPageWidth := MulDiv(PageInfo.PageWidth,
                          TprReport(Report).prPrinter.scrPixelsPerX*100,
                          ThousandsMillimetersPerInch);
FPixelPageHeight := MulDiv(PageInfo.PageHeight,
                           TprReport(Report).prPrinter.scrPixelsPerY*100,
                           ThousandsMillimetersPerInch);
                           
FPixelPageRect.Left := Round(PageInfo.lMargin*
                             TprReport(Report).prPrinter.scrPixelsPerX*1000/
                             ThousandsMillimetersPerInch);
FPixelPageRect.Top := Round(PageInfo.tMargin*
                            TprReport(Report).prPrinter.scrPixelsPerY*1000/
                            ThousandsMillimetersPerInch);
FPixelPageRect.Right := MulDiv(PageInfo.PageWidth,
                               TprReport(Report).prPrinter.scrPixelsPerX*100,
                               ThousandsMillimetersPerInch)-
                        Round(PageInfo.rMargin*
                              TprReport(Report).prPrinter.scrPixelsPerX*1000/
                              ThousandsMillimetersPerInch);
FPixelPageRect.Bottom := MulDiv(PageInfo.PageHeight,
                                TprReport(Report).prPrinter.scrPixelsPerY*100,
                                ThousandsMillimetersPerInch)-
                         Round(PageInfo.bMargin*
                               TprReport(Report).prPrinter.scrPixelsPerY*1000/
                               ThousandsMillimetersPerInch);
end;

procedure TprEndPage.ThirdPass;
var
  I: Integer;
begin
  for I := 0 to oRecsCount - 1 do
  begin
    oRec[I].SecondPass;
    oRec[I].ThirdPass(Self, TObject(ScreenDC), oRec[I].pRect);
  end;
end;

procedure SaveObjectsVersionsToStream;
var
  l : TList;
  w : TWriter;
  i,j : integer;
begin
l := TList.Create;
try
  for i:=0 to LV.Count-1 do
    begin
      // find collection for this object
      j := 0;
      while (j<l.Count) and (not(TObject(LV[i]) is TCollection(l[j]).ItemClass)) do Inc(j);
      if j>=l.Count then
        j := l.Add(TCollection.Create(TCollectionItemClass(TObject(LV[i]).ClassType)));
      TCollection(l[j]).Add.Assign(LV[i]);
    end;

  i := l.Count;
  Stream.Write(i,4);
  for i:=0 to l.Count-1 do
    begin
      WriteString(Stream,TCollection(l[i]).ItemClass.ClassName);
      w := TWriter.Create(Stream,1024);
      try
        w.WriteCollection(TCollection(l[i]));
      finally
        w.Free;
      end;
    end;
finally
  for i:=0 to l.Count-1 do
    TCollection(l[i]).Free;
  l.Free;
end;
end;

procedure TprEndPage.Save(Stream : TStream);
begin
PageInfo.Save(Stream);
PageScaleInfo.Save(Stream);
SaveObjectsVersionsToStream(Stream,VL);
end;

procedure LoadObjectsVersionsFromStream;
var
  c : TCollection;
  r : TReader;
  ci : TCollectionItem;
  i,n,j : integer;
  ciClass : string;
begin
Stream.Read(n,4);
for i:=0 to n-1 do
  begin
    ciClass := ReadString(Stream);
    c := TCollection.Create(TCollectionItemClass(GetClass(ciClass)));
    try
      r := TReader.Create(Stream,1024);
      try
        r.ReadValue;
        r.ReadCollection(c);
      finally
        r.Free;
      end;

      for j:=0 to c.Count-1 do
        begin
          ci:=TCollectionItemClass(GetClass(ciClass)).Create(nil);
          ci.Assign(c.Items[j]);
          LV.Add(ci);
        end;
    finally
      c.Free;
    end;
  end;
end;

procedure TprEndPage.Load(Stream : TStream);
var
  i : integer;
begin
PageInfo.Load(Stream);
if TprReport(Report).FPreparedReportFormatVersion<2 then
  begin
    // !!! need skip dummy 80 bytes for compatible with old versions
    Stream.Position := Stream.Position+80;
    // calculate FPageWidth, FPageHeight based on PaperSize
    i := 0;
    while (i<PAPERCOUNT) and (PaperInfo[i].Typ<>PageInfo.PaperSize) do Inc(i);
    if i<PAPERCOUNT then
      begin
        PageInfo.PageWidth := PaperInfo[i].X;
        PageInfo.PageHeight := PaperInfo[i].Y;
      end
    else
      begin
        PageInfo.PageWidth := A4_PaperWidth;
        PageInfo.PageHeight := A4_PaperHeight;
      end;
  end
else
  begin
    PageScaleInfo.Load(Stream);
  end;
Scale(TprReport(Report).prPrinter.scrPixelsPerX,
      TprReport(Report).prPrinter.CreatedscrPixelsPerX,
      TprReport(Report).prPrinter.scrPixelsPerY,
      TprReport(Report).prPrinter.CreatedscrPixelsPerY);
LoadObjectsVersionsFromStream(Stream,VL);
for i:=0 to VL.Count-1 do
  TprExObjRecVersion(VL[i]).Scale(TprReport(Report).prPrinter.scrPixelsPerX,
                                  TprReport(Report).prPrinter.CreatedscrPixelsPerX,
                                  TprReport(Report).prPrinter.scrPixelsPerY,
                                  TprReport(Report).prPrinter.CreatedscrPixelsPerY);
end;

procedure TprEndPage.Scale(cx1,cx2,cy1,cy2 : integer);
begin
end;

/////////////////////////////////////////////////
//
// TprPage
//
/////////////////////////////////////////////////
constructor TprPage.Create;
begin
inherited;
dPageRect := @FPixelPageRect;
FPageInfo := TprPageInfo.Create;
FPageInfo.OnChange := OnPageInfoChanged;
FPageScaleInfo := TprPageScaleInfo.Create;
FDsgnWidthDelta := 0;
FDsgnHeightDelta := 0;
end;

destructor TprPage.Destroy;
begin
FPageInfo.Free;
FPageScaleInfo.Free;
inherited;
end;

function TprPage.GetWidth : integer;
begin
Result := PageInfo.PageWidth;
end;

procedure TprPage.SetWidth(Value : integer);
begin
PageInfo.PageWidth := Value;
end;

function TprPage.GetHeight : integer;
begin
Result := PageInfo.PageHeight;
end;

procedure TprPage.SetHeight(Value : integer);
begin
PageInfo.PageHeight := Value;
end;

function TprPage.GetDsgnWidth : integer;
begin
Result := FDsgnWidthDelta+PageInfo.PageWidth;
end;

procedure TprPage.SetDsgnWidth(Value : integer);
begin
if FDsgnWidthDelta=Value-PageInfo.PageWidth then exit;
FDsgnWidthDelta := Value-PageInfo.PageWidth;
UpdatePixelSizes;
UpdateBandsPageRect;
end;

function TprPage.GetDsgnHeight : integer;
begin
Result := FDsgnHeightDelta+PageInfo.PageHeight;
end;

procedure TprPage.SetDsgnHeight(Value : integer);
begin
if FDsgnHeightDelta=Value-PageInfo.PageHeight then exit;
FDsgnHeightDelta := Value-PageInfo.PageHeight;
UpdatePixelSizes;
UpdateBandsPageRect;
end;

procedure TprPage.Loaded;
begin
inherited;
if PageInfo.PaperSize<>-1 then
  begin
// correct PageInfo for Landscape and other
//    if PageInfo.Orientation=poLandscape then
//      Exchange(PageInfo.FPageHeight,PageInfo.FPageWidth);
//    UpdatePixelSizes;
  end;
UpdateBandsPageRect;
end;

procedure TprPage.SetPageScaleInfo(Value : TprPageScaleInfo);
begin
FPageScaleInfo.Assign(Value);
end;

procedure TprPage.UpdatePixelSizes;
begin
FPixelPageWidth := MulDiv(DsgnWidth,
                          TprReport(Report).prPrinter.scrPixelsPerX*100,
                          ThousandsMillimetersPerInch);
FPixelPageHeight := MulDiv(DsgnHeight,
                           TprReport(Report).prPrinter.scrPixelsPerY*100,
                           ThousandsMillimetersPerInch);
                           
FPixelPageRect.Left := Round(PageInfo.lMargin*
                             TprReport(Report).prPrinter.scrPixelsPerX*1000/
                             ThousandsMillimetersPerInch);
FPixelPageRect.Top := Round(PageInfo.tMargin*
                            TprReport(Report).prPrinter.scrPixelsPerY*1000/
                            ThousandsMillimetersPerInch);
FPixelPageRect.Right := MulDiv(DsgnWidth,
                               TprReport(Report).prPrinter.scrPixelsPerX*100,
                               ThousandsMillimetersPerInch)-
                        Round(PageInfo.rMargin*
                              TprReport(Report).prPrinter.scrPixelsPerX*1000/
                              ThousandsMillimetersPerInch);
FPixelPageRect.Bottom := MulDiv(DsgnHeight,
                                TprReport(Report).prPrinter.scrPixelsPerY*100,
                                ThousandsMillimetersPerInch)-
                         Round(PageInfo.bMargin*
                               TprReport(Report).prPrinter.scrPixelsPerY*1000/
                               ThousandsMillimetersPerInch);

FGenPixelPageRect.Left := Round(PageInfo.lMargin*
                                TprReport(Report).prPrinter.scrPixelsPerX*1000/
                                ThousandsMillimetersPerInch);
FGenPixelPageRect.Top := Round(PageInfo.tMargin*
                               TprReport(Report).prPrinter.scrPixelsPerY*1000/
                               ThousandsMillimetersPerInch);
FGenPixelPageRect.Right := MulDiv(PageInfo.PageWidth,
                                  TprReport(Report).prPrinter.scrPixelsPerX*100,
                                  ThousandsMillimetersPerInch)-
                        Round(PageInfo.rMargin*
                              TprReport(Report).prPrinter.scrPixelsPerX*1000/
                              ThousandsMillimetersPerInch);
FGenPixelPageRect.Bottom := MulDiv(PageInfo.PageHeight,
                                   TprReport(Report).prPrinter.scrPixelsPerY*100,
                                   ThousandsMillimetersPerInch)-
                         Round(PageInfo.bMargin*
                               TprReport(Report).prPrinter.scrPixelsPerY*1000/
                               ThousandsMillimetersPerInch);
end;

procedure TprPage.OnPageInfoChanged(Sender : TObject);
begin
UpdatePixelSizes;
UpdateBandsPageRect;
end;

function TprPage.StoredDsgnWidth : boolean;
begin
Result := FDsgnWidthDelta<>0; 
end;

function TprPage.StoredDsgnHeight : boolean;
begin
Result := FDsgnHeightDelta<>0; 
end;

procedure TprPage.ReportSetted;
begin
OnPageInfoChanged(FPageInfo);
end;

function TprPage.DsgnPageRect;
begin
Result := FPixelPageRect;
end;

function TprPage.GenPageRect : TRect;
begin
Result := FGenPixelPageRect;
end;

function TprPage.GetPaperSize : integer;
begin
Result := PageInfo.PaperSize;
end;

procedure TprPage.SetPaperSize(Value : integer);
begin
PageInfo.PaperSize := Value;
end;

function TprPage.GetOrientation : TprPrinterOrientation;
begin
Result := PageInfo.Orientation;
end;

procedure TprPage.SetOrientation(Value : TprPrinterOrientation);
begin
PageInfo.Orientation := Value;
end;

function TprPage.GetlMargin : extended;
begin
Result := PageInfo.lMargin;
end;

procedure TprPage.SetlMargin(Value : extended);
begin
PageInfo.lMargin := Value;
end;

function TprPage.GettMargin : extended;
begin
Result := PageInfo.tMargin;
end;

procedure TprPage.SettMargin(Value : extended);
begin
PageInfo.tMargin := Value;
end;

function TprPage.GetrMargin : extended;
begin
Result := PageInfo.rMargin;
end;

procedure TprPage.SetrMargin(Value : extended);
begin
PageInfo.rMargin := Value;
end;

function TprPage.GetbMargin : extended;
begin
Result := PageInfo.bMargin;
end;

procedure TprPage.SetbMargin(Value : extended);
begin
PageInfo.bMargin := Value;
end;

//////////////////////////////
//
// TprPrinter
//
//////////////////////////////
constructor TprPrinter.Create;
begin
inherited Create;
FPageInfo := TprPageInfo.Create;
FDevNames := nil;
FDevNamesSize := 0;
FDevMode := nil;
FDevModeSize := 0;
FPrinterIndex := -1;
FPrinters := TStringList.Create;
FPaperNames := TStringList.Create;

prGetScreenPixelsPerInch(FscrPixelsPerX,FscrPixelsPerY);
FCreatedscrPixelsPerX := FscrPixelsPerX;
FCreatedscrPixelsPerY := FscrPixelsPerY;

UpdatePrintersList;
SetToDefaultPrinter;
end;

destructor TprPrinter.Destroy;
begin
ClearStructures;
ClearInfo;
FPrinters.Free;
FPaperNames.Free;
FPageInfo.Free;
inherited;
end;

procedure TprPrinter.UpdatePrintersList;
begin
pr_Utils.UpdatePrintersList(FPrinters,prLoadStr(sDefaultPrinterName));
end;

procedure TprPrinter.SetToDefaultPrinter;
var
  pn : string;
  pi : integer;
begin
pi := 0;
pn := prGetDefaultPrinterName;
if pn<>'' then
  pi := FPrinters.IndexOf(pn);
PrinterIndex := pi;
end;

procedure TprPrinter.SetPrinterIndex;
begin
if FPrinterIndex<>Value then
  begin
    FPrinterIndex := Value;
    ClearStructures;
    ClearInfo;
  end;
end;

procedure TprPrinter.SetPrinterName;
var
  i : integer;
begin
i := FPrinters.IndexOf(Value);
if i=-1 then
  PrinterIndex := 0 // virtual printer
else
  PrinterIndex := i;
end;

function TprPrinter.GetPrinterName;
begin
Result := FPrinters[FPrinterIndex];
end;

procedure TprPrinter.ClearStructures;
begin
FStructuresInitializated:=false;
FDriverName := '';
FDeviceName := '';
FPortName := '';
if FDevNames<>nil then
  begin
    FreeMem(FDevNames);
    FDevNames := nil;
    FDevNamesSize := 0;
  end;
if FDevMode<>nil then
  begin
    FreeMem(FDevMode);
    FDevMode := nil;
    FDevModeSize := 0;
  end;
end;

procedure TprPrinter.ClearInfo;
begin
FInfoInitializated := false;
FPixelsPerX := 0;
FPixelsPerY := 0;
FPaperNames.Clear;
if FPaperSizes<>nil then
  begin
    FreeMem(FPaperSizes);
    FPaperSizes := nil;
  end;
if FPaperDimensions<>nil then
  begin
    FreeMem(FPaperDimensions);
    FPaperDimensions := nil;
  end;
if FPrinterDC<>0 then
  begin
    DeleteDC(FPrinterDC);
    FPrinterDC := 0;
  end;
end;

// init structures DevNames and DevMode
function TprPrinter.InitStructures;
var
  pi2: PPrinterInfo2;
  Offset: PChar;
  hPrinter: THandle;
  OldError: UINT;
  TempDevMode: TDevMode;
  BytesNeeded: cardinal;
begin
  Result := False;
  ClearStructures;

  if FPrinterIndex = 0 then
  begin
    // virtual printer selected
    FDeviceName := prLoadStr(sDefaultPrinterName);
  end
  else
  begin
    // real printer selected
    hPrinter := 0;
    pi2 := nil;
    if not OpenPrinter(PChar(PrinterName), hPrinter, nil) then
      exit;

    try
      GetPrinter(hPrinter,2,nil,0,@BytesNeeded);
      if BytesNeeded=0 then
        begin
          ClosePrinter(hPrinter);
          exit;
        end;

      GetMem(pi2,BytesNeeded);
      if not GetPrinter(hPrinter,2,pi2,BytesNeeded,@BytesNeeded) then exit;

      FDevNamesSize := sizeof(TDevNames)+strlen(pi2.pPrinterName)+strlen(pi2.pDriverName)+strlen(pi2.pPortName)+3;
      GetMem(FDevNames,FDevNamesSize);

      Offset := PChar(FDevNames)+SizeOf(TDevnames);
      FDevNames^.wDriverOffset := Longint(Offset)-Longint(FDevNames);
      Offset := StrECopy(Offset, pi2.pDriverName)+1;
      FDevNames^.wDeviceOffset := Longint(Offset)-Longint(FDevNames);
      Offset := StrECopy(Offset, pi2.pPrinterName) + 1;
      FDevNames^.wOutputOffset := Longint(Offset)-Longint(FDevNames);
      StrCopy(Offset, pi2.pPortName);

      SetLength(FDeviceName,strlen(pi2.pPrinterName));
      MoveMemory(@(FDeviceName[1]),pi2.pPrinterName,strlen(pi2.pPrinterName));
      SetLength(FDriverName,strlen(pi2.pDriverName));
      MoveMemory(@(FDriverName[1]),pi2.pDriverName,strlen(pi2.pDriverName));
      SetLength(FPortName,strlen(pi2.pPortName));
      MoveMemory(@(FPortName[1]),pi2.pPortName,strlen(pi2.pPortName));

      // getting DevMode 
      OldError := SetErrorMode(SEM_FAILCRITICALERRORS);
      try
        FDevModeSize := DocumentProperties(0,hPrinter,PChar(FDeviceName),TempDevMode,TempDevMode,0);
      finally
        SetErrorMode(OldError);
      end;

      if FDevModeSize<=0 then
        FDevMode := AllocMem(sizeof(TDeviceMode))
      else
        FDevMode := AllocMem(FDevModeSize);
      FDevMode.dmSize := sizeof(TDeviceMode);
      if FDevModeSize <= 0 then
        begin
          FDevMode.dmFields := DM_ORIENTATION or DM_PAPERSIZE;
          FDevMode.dmOrientation := DMORIENT_PORTRAIT;
          FDevMode.dmPaperSize := DMPAPER_A4;
        end
      else
        begin
          if DocumentProperties(0, hPrinter, PChar(FDeviceName), FDevMode^, FDevMode^, DM_OUT_BUFFER)<0 then
          begin
            FreeMem(FDevMode);
            FDevMode := nil;
            FDevModeSize := 0;
          end
          else
          begin
            FDevMode.dmFields := FDevMode.dmFields and (not DM_COLLATE) and (not DM_COPIES);
          end;
        end;
    finally
      FreeMem(pi2);
      if hPrinter<>0 then
        ClosePrinter(hPrinter);
    end;

    if FDevMode=nil then
      begin
        ClearStructures;
        Result := false;
        exit;
      end;
  end;
  FStructuresInitializated := true;
  Result := true;
end;

function TprPrinter.InitInfo;
var
  sz : dword;
  Buffer : PChar;
  i,Count : integer;
  OldError : UINT;
begin
Result := false;
ClearInfo;

if not FStructuresInitializated then exit;

if FPrinterIndex=0 then
  begin
    FPaperSizesCount := PAPERCOUNT;
    FPaperDimensionsCount := PAPERCOUNT;
    GetMem(FPaperSizes,FPaperSizesCount*2);
    GetMem(FPaperDimensions,FPaperDimensionsCount*sizeof(TPoint));
    for i:=0 to PAPERCOUNT-1 do
      begin
        FPaperNames.Add(PaperInfo[i].Name);
        FPaperSizes[i] := PaperInfo[i].Typ;
        FPaperDimensions[i].x := PaperInfo[i].X;
        FPaperDimensions[i].y := PaperInfo[i].Y;
      end;

    FPixelsPerX := 600;
    FPixelsPerY := 600;

    // margins in millimeters
    FmmlMargin := 5;
    FmmtMargin := 5;
    FmmrMargin := 5;
    FmmbMargin := 5;

    // fill PageInfo with information about current settings
    PageInfo.PaperSize := A4_PaperSizeCode;
    PageInfo.PageWidth := A4_PaperWidth;
    PageInfo.PageHeight := A4_PaperHeight;
    PageInfo.Orientation := poPortrait;
    PageInfo.lMargin := FmmlMargin;
    PageInfo.tMargin := FmmtMargin;
    PageInfo.rMargin := FmmrMargin;
    PageInfo.bMargin := FmmbMargin;

    // max sizes
    FmmMaxPageWidth := 4200;
    FmmMaxPageHeight := 5940;

    // physicals sizes of the paper
    FPixelPageWidth := MulDiv(PageInfo.PageWidth,FPixelsPerX*100,ThousandsMillimetersPerInch);
    FPixelPageHeight := MulDiv(PageInfo.PageHeight,FPixelsPerY*100,ThousandsMillimetersPerInch);
    FPixelPageRect.Left := MulDiv(FmmlMargin,FPixelsPerX*1000,ThousandsMillimetersPerInch);
    FPixelPageRect.Top := MulDiv(FmmtMargin,FPixelsPerY*1000,ThousandsMillimetersPerInch);
    FPixelPageRect.Right := FPixelPageWidth-MulDiv(FmmrMargin,FPixelsPerX*1000,ThousandsMillimetersPerInch);
    FPixelPageRect.Bottom := FPixelPageHeight-MulDiv(FmmbMargin,FPixelsPerY*1000,ThousandsMillimetersPerInch);
  end
else
  begin
    OldError := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      FPrinterDC := CreateIC(PChar(FDriverName),PChar(FDeviceName),PChar(FPortName),FDevMode);
    finally
      SetErrorMode(OldError);
    end;
    if PrinterDC=0 then exit;

    FPixelsPerX := GetDeviceCaps(PrinterDC, LOGPIXELSX);
    FPixelsPerY := GetDeviceCaps(PrinterDC, LOGPIXELSY);

    // supported paper sizes
    OldError := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      FPaperSizesCount := DeviceCapabilities(PChar(FDeviceName), PChar(FPortName), DC_PAPERS, nil, FDevMode);
      if FPaperSizesCount>0 then
        begin
          GetMem(FPaperSizes,FPaperSizesCount*2);
          DeviceCapabilities(PChar(FDeviceName), PChar(FPortName), DC_PAPERS, PChar(FPaperSizes), FDevMode);
        end
      else
        begin
          GetMem(FPaperSizes,sizeof(Word));
          FPaperSizes[0] := A4_PaperSizeCode;
        end;

      // supported paper dimensions
      FPaperDimensionsCount := DeviceCapabilities(PChar(FDeviceName), PChar(FPortName), DC_PAPERS, nil, FDevMode);
      if FPaperDimensionsCount>0 then
        begin
          GetMem(FPaperDimensions,FPaperDimensionsCount*sizeof(TPoint));
          DeviceCapabilities(PChar(FDeviceName), PChar(FPortName), DC_PAPERSIZE, PChar(FPaperDimensions), FDevMode);
        end
      else
        begin
          GetMem(FPaperDimensions,sizeof(TPoint));
          FPaperDimensions^[0].x := A4_PaperWidth;
          FPaperDimensions^[0].y := A4_PaperHeight;
        end;

      // max sizes
      sz := DeviceCapabilities(PChar(FDeviceName),PChar(FPortName),DC_MAXEXTENT,nil,FDevMode);
      FmmMaxPageWidth := sz and $FFFF;
      FmmMaxPageHeight := sz shr 16;

      // supported paper names
      Count := DeviceCapabilities(PChar(FDeviceName), PChar(FPortName), DC_PAPERNAMES, nil, FDevMode);
      if Count>0 then
        begin
          GetMem(Buffer,Count*64);
          try
            DeviceCapabilities(PChar(FDeviceName), PChar(FPortName), DC_PAPERNAMES, Buffer, FDevMode);
            FPaperNames.Clear;
            for i := 0 to Count - 1 do
              FPaperNames.Add(Buffer+i*64);
          finally
            FreeMem(Buffer);
          end;
        end
      else
        begin
          FPaperNames.Clear;
          FPaperNames.Add(PaperInfo[DMPAPER_A4].Name);
        end;
    finally
      SetErrorMode(OldError);
    end;

    // physicals sizes of the paper
    FPixelPageWidth := GetDeviceCaps(FPrinterDC,PHYSICALWIDTH);
    FPixelPageHeight := GetDeviceCaps(FPrinterDC,PHYSICALHEIGHT);
    FPixelPageRect.Left := GetDeviceCaps(FPrinterDC,PHYSICALOFFSETX);
    FPixelPageRect.Top := GetDeviceCaps(FPrinterDC,PHYSICALOFFSETY);
    FPixelPageRect.Right := FPixelPageRect.Left+GetDeviceCaps(FPrinterDC,HORZRES);
    FPixelPageRect.Bottom := FPixelPageRect.Top+GetDeviceCaps(FPrinterDC,VERTRES);

    // margins in millimeters
    FmmlMargin := MulDiv(FPixelPageRect.Left,ThousandsMillimetersPerInch,PixelsPerX*1000);
    FmmtMargin := MulDiv(FPixelPageRect.Top,ThousandsMillimetersPerInch,PixelsPerY*1000);
    FmmrMargin := MulDiv(FPixelPageWidth-FPixelPageRect.Right,ThousandsMillimetersPerInch,PixelsPerX*1000);
    FmmbMargin := MulDiv(FPixelPageHeight-FPixelPageRect.Bottom,ThousandsMillimetersPerInch,PixelsPerY*1000);


    // fill PageInfo with information about current settings
    if (FDevMode.dmFields and DM_PAPERSIZE)<>0 then
      PageInfo.PaperSize := FDevMode.dmPaperSize
    else
      PageInfo.PaperSize := -1;

//    PageInfo.PageWidth := (GetDeviceCaps(FPrinterDC,HORZSIZE)+FmmlMargin+FmmrMargin)*10;
//    PageInfo.PageHeight := (GetDeviceCaps(FPrinterDC,VERTSIZE)+FmmtMargin+FmmbMargin)*10;
    PageInfo.PageWidth := MulDiv(FPixelPageWidth,ThousandsMillimetersPerInch,PixelsPerX*1000)*10;
    PageInfo.PageHeight := MulDiv(FPixelPageHeight,ThousandsMillimetersPerInch,PixelsPerY*1000)*10;
    PageInfo.lMargin := FmmlMargin;
    PageInfo.tMargin := FmmtMargin;
    PageInfo.rMargin := FmmrMargin;
    PageInfo.bMargin := FmmbMargin;

    // paper orientation
    if (FDevMode.dmFields and DM_ORIENTATION)<>0 then
      begin
        case FDevMode.dmOrientation of
          dmorient_portrait : PageInfo.Orientation := poPortrait;
          dmorient_landscape : PageInfo.Orientation := poLandscape;
        end;
      end;
  end;

FInfoInitializated := true;
Result := true;
end;

function TprPrinter.SetDevMode(NewDevMode : PDevMode; NewDevModeSize : integer) : boolean;
begin
Result := false;
if not InitStructures then exit;

if (FDevModeSize<>NewDevModeSize) and (FDevModeSize<>0) then
  begin
    FreeMem(FDevMode);
    FDevMode := nil;
  end;

if FDevMode=nil then
  GetMem(FDevMode,NewDevModeSize);

MoveMemory(FDevMode,NewDevMode,NewDevModeSize);
FDevModeSize := NewDevModeSize;

if not InitInfo then exit;
Result := true;
end;

function TprPrinter.GetPaperSize(PaperSizeIndex : integer) : integer;
begin
if (PaperSizeIndex<0) or (PaperSizeIndex>=FPaperSizesCount) then
  raise Exception.CreateFmt(prLoadStr(sPrinterErrorInvalidPaperSizeIndex),[PaperSizeIndex]);
Result := FPaperSizes[PaperSizeIndex];
end;

function TprPrinter.GetPaperSizes(PaperSize : integer) : TPoint;
var
  i : integer;
begin
i := GetPaperSizeIndex(PaperSize);
if i=-1 then
  raise Exception.CreateFmt(prLoadStr(sPrinterErrorUnsupportedPaperSize),[PrinterName,PaperSize]);
Result.x := FPaperDimensions[i].x;
Result.y := FPaperDimensions[i].y;
end;

function TprPrinter.GetPaperWidth(PaperSize : integer) : integer;
begin
Result := GetPaperSizes(PaperSize).x;
end;

function TprPrinter.GetPaperHeight(PaperSize : integer) : integer;
begin
Result := GetPaperSizes(PaperSize).y;
end;

function TprPrinter.SetPageSize(_PaperSize : integer; _Orientation : TprPrinterOrientation; _PageWidth,_PageHeight : integer) : boolean;
var
  hPrinter : THandle;
begin
if _PaperSize = -1 then
  begin
    FDevMode.dmFields := (FDevMode.dmFields or DM_PAPERLENGTH or DM_PAPERWIDTH) and not DM_PAPERSIZE;
    FDevMode.dmPaperLength := _PageHeight;
    FDevMode.dmPaperWidth := _PageWidth;
  end
else
  begin
    FDevMode.dmFields := (FDevMode.dmFields or DM_PAPERSIZE) and not DM_PAPERLENGTH and not DM_PAPERWIDTH;
    FDevMode.dmPaperSize := _PaperSize;
  end;

FDevMode.dmFields := FDevMode.dmFields or DM_ORIENTATION;
if _Orientation=poPortrait then
  FDevMode.dmOrientation := dmorient_portrait
else
  FDevMode.dmOrientation := dmorient_landscape;

if OpenPrinter(PChar(DeviceName),hPrinter,nil) then
  begin
    DocumentProperties(0,hPrinter,PChar(FDeviceName),FDevMode^,FDevMode^,DM_IN_BUFFER or DM_OUT_BUFFER);
    ClosePrinter(hPrinter);
    InitInfo;
{    if _PaperSize>0 then
      Result := (PageInfo.PaperSize=_PaperSize) and (PageInfo.Orientation=_Orientation)
    else}
    Result := (PageInfo.PageWidth=_PageWidth) and (PageInfo.PageHeight=_PageHeight);
  end
else
  raise Exception.CreateFmt(prLoadStr(sPrinterErrorOpenPrinter),[PrinterName,SysErrorMessage(GetLastError)]);
end;

function TprPrinter.GetPaperSizeIndex(PaperSize : Integer) : Integer;
begin
Result := 0;
while (Result<FPaperSizesCount) and (FPaperSizes[Result]<>PaperSize) do Inc(Result);
if Result>=FPaperSizesCount then
  Result := -1;
end;

function TprPrinter.FindPaperSize(PaperWidth : integer; PaperHeight : integer; var PaperSize : integer; var PaperOrientation : TprPrinterOrientation; fFindInMM : boolean) : boolean;
var
  i : integer;
begin
for i:=0 to FPaperDimensionsCount-1 do
  if ((PaperWidth=FPaperDimensions[i].x) and (PaperHeight=FPaperDimensions[i].y)) or
     (fFindInMM and (mm(PaperWidth)=mm(FPaperDimensions[i].x)) and (mm(PaperHeight)=mm(FPaperDimensions[i].y))) then
    begin
      PaperOrientation := poPortrait;
      PaperSize := FPaperSizes[i];
      Result := true;
      exit;
    end
  else
    if ((PaperHeight=FPaperDimensions[i].x) and (PaperWidth=FPaperDimensions[i].y)) or
       (fFindInMM and (mm(PaperHeight)=mm(FPaperDimensions[i].x)) and (mm(PaperWidth)=mm(FPaperDimensions[i].y))) then
      begin
        PaperOrientation := poLandscape;
        PaperSize := FPaperSizes[i];
        Result := true;
        exit;
      end;
Result := false;
end;

procedure TprPrinter.CheckPrinter;
begin
  if not FStructuresInitializated and not InitStructures then
    raise Exception.Create(prLoadStr(sPrinterErrorUnaibleInitializeStructures));
  if not FInfoInitializated and not InitInfo then
    raise Exception.Create(prLoadStr(sPrinterErrorUnaibleInitializeInfo));
end;

procedure TprPrinter.BeginDoc;
var
  DocInfo : TDocInfo;
begin
if FPrinterDC<>0 then
  begin
    DeleteDC(FPrinterDC);
    FPrinterDC := 0;
  end;
FPrinterDC := CreateDC(PChar(FDriverName),PChar(FDeviceName),PChar(FPortName),FDevMode);
if FPrinterDC=0 then
  raise Exception.Create(prLoadStr(sPrinterErrorUnaibleCreateCanvas));

ZeroMemory(@DocInfo,sizeof(TDocInfo));
DocInfo.cbSize := sizeof(TDocInfo);
DocInfo.lpszDocName := PChar(FTitle);
DocInfo.lpszOutput := nil;
//SetAbortProc(DC, AbortProc);
StartDoc(FPrinterDC,DocInfo);
StartPage(FPrinterDC);
end;

procedure TprPrinter.EndDoc;
begin
Windows.EndPage(FPrinterDC);
Windows.EndDoc(FPrinterDC);
DeleteDC(FPrinterDC);
FPrinterDC := 0;
end;

procedure TprPrinter.NewPage;
begin
EndPage(FPrinterDC);
StartPage(FPrinterDC);
end;

/////////////////////////////////////////////////
//
// TprPreviewParams
//
/////////////////////////////////////////////////
constructor TprPreviewParams.Create;
begin
  inherited;
  FOptions := [];
  FShowToolbars := [prptPreviewCommon];
end;

/////////////////////////////////////////////////
//
// TprReport
//
/////////////////////////////////////////////////
constructor TprReport.Create;
begin
inherited;

FUseMMWhenCheckPaperSizes := True;
FPreviewParams := TprPreviewParams.Create;
FExportPrecisionLow := 10;
FExportPrecisionNormal := 5;
FExportPrecisionHigh := 1;
FExportPrecision := FExportPrecisionHigh;

FprPrinter := TprPrinter.Create;
end;

destructor TprReport.Destroy;
begin
prPrinter.Free;
FPreviewParams.Free;
inherited;
end;

procedure TprReport.Loaded;
var
  i,j,k : integer;
begin
inherited;
if (prPrinter.CreatedscrPixelsPerX<>prPrinter.scrPixelsPerX) or
   (prPrinter.CreatedscrPixelsPerY<>prPrinter.scrPixelsPerY) then
  // scale all objects in report 
  for i:=0 to PagesCount-1 do
    begin
      for j:=0 to Pages[i].Bands.Count-1 do
        begin
          if Pages[i].Bands[j].BandType in HorizontalBands then
            TprCustomHBand(Pages[i].Bands[j]).Height := MulDiv(TprCustomHBand(Pages[i].Bands[j]).Height,prPrinter.scrPixelsPerY,prPrinter.CreatedscrPixelsPerY)
          else
            TprCustomVBand(Pages[i].Bands[j]).Width := MulDiv(TprCustomVBand(Pages[i].Bands[j]).Width,prPrinter.scrPixelsPerX,prPrinter.CreatedscrPixelsPerX);
          for k:=0 to Pages[i].Bands[j].Objects.Count-1 do
            Pages[i].Bands[j].Objects[k].dRec.pRect := MulDivRect(Pages[i].Bands[j].Objects[k].dRec.pRect,prPrinter.scrPixelsPerX,prPrinter.CreatedscrPixelsPerX,prPrinter.scrPixelsPerY,prPrinter.CreatedscrPixelsPerY);
        end;
      Pages[i].UpdateBandsPageRect;
    end;
end;

function TprReport.CreatePage : TprCustomPage;
begin
Result := TprPage.Create(prOwner);
Result.Report := Self;
end;

procedure TprReport.ReadLOGPIXELSX;
begin
prPrinter.FCreatedscrPixelsPerX := Reader.ReadInteger;
end;

procedure TprReport.WriteLOGPIXELSX;
begin
Writer.WriteInteger(GetDeviceCaps(ScreenDC, LOGPIXELSX));
end;

procedure TprReport.ReadLOGPIXELSY;
begin
prPrinter.FCreatedscrPixelsPerY := Reader.ReadInteger;
end;

procedure TprReport.WriteLOGPIXELSY;
begin
Writer.WriteInteger(GetDeviceCaps(ScreenDC, LOGPIXELSY));
end;

procedure TprReport.DefineProperties;
begin
inherited;
Filer.DefineProperty('LOGPIXELSX',ReadLOGPIXELSX,WriteLOGPIXELSX,true);
Filer.DefineProperty('LOGPIXELSY',ReadLOGPIXELSY,WriteLOGPIXELSY,true);
end;

function TprReport.GetPage(Index: Integer): TprPage;
begin
  Result := TprPage(inherited Pages[Index]);
end;

function TprReport.GetEndPage(Index: Integer): TprEndPage;
begin
  Result := TprEndPage(inherited EndPages[Index]);
end;

function TprReport.GetPrinterName : string;
begin
Result := prPrinter.PrinterName;
end;

procedure TprReport.SetPrinterName(Value : string);
begin
prPrinter.PrinterName := Value;
end;

function TprReport.GetDesignerFormClass;
begin
Result := 'TprDesignerForm';
end;

function TprReport.GetPreviewFormClass;
begin
Result := 'TprPreviewForm';
end;

function TprReport.CreateEndPage;
begin
Result := TprEndPage.Create(Page as TprPage);
end;

function TprReport.CreateEmptyEndPage;
begin
Result:=TprEndPage.CreateEmpty(Self);
end;

function TprReport.GetPreparedReportEmpty : boolean;
begin
Result := EndPagesCount=0;
end;

procedure TprReport.InternalLoadPreparedReport;
var
  ep : TprEndPage;
  buf : cardinal;
  i,b,j : integer;
begin
Stream.Read(buf,4);
if buf=PreparedReportSavePrefix then
  begin
    Stream.Read(FPreparedReportFormatVersion,4);
    case FPreparedReportFormatVersion of
      1..2 : begin
               Stream.Read(prPrinter.FCreatedscrPixelsPerX,4);
               Stream.Read(prPrinter.FCreatedscrPixelsPerY,4);
             end;
    end;
  end
else
  begin
    Stream.Seek(0,soFromBeginning);
    prPrinter.FCreatedscrPixelsPerX := prPrinter.scrPixelsPerX;
    prPrinter.FCreatedscrPixelsPerY := prPrinter.scrPixelsPerY;
    FPreparedReportFormatVersion := 0;
  end;
Stream.Read(b,4);
for i:=0 to b-1 do
  begin
    ep := TprEndPage(CreateEmptyEndPage);
    try
      ep.Load(Stream);
      for j:=0 to ep.VL.Count-1 do
        if TprExObjRecVersion(ep.VL[j]).PreviewUserData<>nil then
          AddPreviewUserData(TprExObjRecVersion(ep.VL[j]).PreviewUserData);
    except
      ep.Free;
      raise;
    end;
    AddEndPageToList(ep);
  end;
end;

procedure TprReport.LoadPreparedReport;
begin
ClearEndPages;
InternalLoadPreparedReport(Stream);
inherited;
end;

procedure TprReport.AppendPreparedReport;
begin
InternalLoadPreparedReport(Stream);
inherited;
end;

procedure TprReport.SavePreparedReport;
var
  b,i : integer;
  buf : cardinal;
begin
buf := PreparedReportSavePrefix;
Stream.Write(buf,4);
buf := PreparedReportSaveFormatVersion;
Stream.Write(buf,4);
Stream.Write(prPrinter.scrPixelsPerX,4);
Stream.Write(prPrinter.scrPixelsPerY,4);
b := EndPagesCount;
Stream.Write(b,4);
if (Stream is TMemoryStream) and (b>0) then
  begin
    i := 0;
    repeat
      TprEndPage(EndPages[i]).Save(Stream);
      Inc(i);
      if Stream.Size=Stream.Position then
        Stream.Size := muldiv(b,Stream.Size,i);
    until (i>=b);
    Stream.Size := Stream.Position;
  end
else
  begin
    for i:=0 to b-1 do
      TprEndPage(EndPages[i]).Save(Stream);
  end;
inherited;
end;

const
  IDD_PRINTTEMPLATE = 1001;
  IDC_PAGESLIST = 1000;
  IDC_ALL = 1056;
  IDC_PAGES = 1058;
  IDC_SELECTION = 1057;
  IDC_EDITPAGESLIST = 1001;
  IDC_FROMPAGE = 1152;
  IDC_TOPAGE = 1153;
  IDC_COPIES = 1154;
  IDC_COLLATE = 1041;

var
  cpd : PPrintDlg;

function DialogHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
const
  APagesMode: array [TprPrintPagesMode] of integer = (IDC_ALL, IDC_PAGES, IDC_SELECTION, IDC_PAGESLIST);
var
  pl: TList;
  Buf: string;
  lMin, lMax, CtrlID: Integer;
begin
Result := 0;

case Msg of
  WM_INITDIALOG:
    begin
      cpd := PPrintDlg(lParam);

      SetDlgItemText(Wnd, IDC_EDITPAGESLIST, PChar(TprReport(cpd.lCustData).PrintPages));

      UnCheck(Wnd, [IDC_PAGESLIST]);

      Check(Wnd, [APagesMode[TprReport(cpd.lCustData).PrintPagesMode]]);

      CenterWindow(Wnd);
    end;
  WM_COMMAND:
    begin
      case wParam of
        IDOK:
          begin
            if IsChecked(Wnd,IDC_PAGESLIST) then
              begin
                // check entered page list
                pl := TList.Create;
                try
                  Buf := GetText(wnd,IDC_EDITPAGESLIST);
                  if (Buf<>'') and TextToPageList(Buf,pl) then
                    begin
                      lMin := integer(pl[0]);
                      lMax := integer(pl[pl.Count-1]);
                      if (lMin<cpd.nMinPage) or
                         (lMin>cpd.nMaxPage) or
                         (lMax<cpd.nMinPage) or
                         (lMax>cpd.nMaxPage) then
                        begin
                          SetFocus(GetDlgItem(Wnd,IDC_EDITPAGESLIST));
                          Application.MessageBox(PChar(Format(prLoadStr(sSetupPrintError2),[cpd.nMinPage,cpd.nMaxPage])),PChar(prLoadStr(sAttention)),MB_ICONEXCLAMATION+MB_OK);
                          Result := 1;
                        end;
                    end
                  else
                    begin
                      Application.MessageBox(PChar(prLoadStr(sSetupPrintError1)),PChar(prLoadStr(sAttention)),MB_OK+MB_ICONEXCLAMATION);
                      Result := 1;
                    end;
                finally
                  pl.Free
                end;
              end;

            if Result = 0 then
              begin
                TprReport(cpd.lCustData).PrintPages := GetText(wnd,IDC_EDITPAGESLIST);

                if IsChecked(wnd,IDC_PAGESLIST) then
                  TprReport(cpd.lCustData).PrintPagesMode := ppmPagesList
                else
                  if IsChecked(wnd,IDC_ALL) then
                    TprReport(cpd.lCustData).PrintPagesMode := ppmAll
                  else
                    if IsChecked(wnd,IDC_PAGES) then
                      TprReport(cpd.lCustData).PrintPagesMode := ppmPagesRange
                    else
                      Result := 1;
              end;
          end
        else
          case WParam shr 16 of
            BN_CLICKED :
              begin
                CtrlID := GetDlgCtrlID(LParam);
                case CtrlID of
                  IDC_PAGESLIST:
                    begin
                      UnCheck(wnd,[IDC_ALL,IDC_PAGES,IDC_SELECTION]);
                      Check(wnd,[IDC_PAGESLIST]);
                      SetFocus(GetDlgItem(Wnd,IDC_EDITPAGESLIST));
                      Result := 1;
                    end;
                  IDC_ALL,IDC_PAGES,IDC_SELECTION:
                    begin
                      UnCheck(wnd,[IDC_PAGESLIST]);
                    end;
                end;
              end;
            EN_CHANGE:
              begin
                case GetDlgCtrlID(lParam) of
                  IDC_EDITPAGESLIST:
                    begin
                      Check(wnd,[IDC_PAGESLIST]);
                      UnCheck(wnd,[IDC_ALL,IDC_PAGES,IDC_SELECTION]);
                    end;
                  IDC_FROMPAGE,IDC_TOPAGE:
                    begin
                      UnCheck(wnd,[IDC_PAGESLIST]);
                    end;
                end;
              end;
          end;
      end;
    end;
end;
end;

function TprReport.SetupPrintParams;
var
  pd: tagPDA;
  DevMode: PDevMode;
  DevNames: PDevNames;

  function CreateMemHandle(Data: Pointer; DataSize: Integer): THandle;
  var
    ABuffer: Pointer;
  begin
    Result := GlobalAlloc(GHND, DataSize);
    ABuffer := GlobalLock(Result);
    MoveMemory(ABuffer, Data, DataSize);
    GlobalUnlock(Result);
  end;

begin
  ZeroMemory(@pd, sizeof(pd));
  if Screen.ActiveForm <> nil then
    pd.hWndOwner := Screen.ActiveForm.Handle;
  pd.lStructSize := sizeof(pd);
  pd.hInstance := SysInit.hInstance;
  pd.lpPrintTemplateName := PChar(IDD_PRINTTEMPLATE);

  pd.nMinPage := 1;
  pd.nMaxPage := Max(pd.nMinPage, EndPagesCount);
  pd.nFromPage := Min(Max(1, Self.FromPage), pd.nMaxPage);
  pd.nToPage := Min(Max(1, Self.ToPage), pd.nMaxPage);
  pd.lCustData := integer(Self);
  pd.lpfnPrintHook := @DialogHook;
  pd.Flags := PD_HIDEPRINTTOFILE or
              PD_NONETWORKBUTTON or
              PD_ENABLEPRINTHOOK or
              PD_NOSELECTION or
              PD_ENABLEPRINTTEMPLATE;
  case Self.PrintPagesMode of
    ppmAll: pd.Flags := pd.Flags + PD_ALLPAGES;
    ppmPagesRange: pd.Flags := pd.Flags + PD_PAGENUMS;
  end;
  pd.nCopies := Max(Self.Copies, 1);
  if Self.Collate then
    pd.Flags := pd.Flags or PD_COLLATE;

  if prPrinter.PrinterIndex = 0 then
    prPrinter.SetToDefaultPrinter;

  if prPrinter.FStructuresInitializated or prPrinter.InitStructures then
  begin
    pd.hDevNames := CreateMemHandle(prPrinter.FDevNames, prPrinter.FDevNamesSize);

    pd.hDevMode := CreateMemHandle(prPrinter.FDevMode, prPrinter.FDevModeSize);

    DevMode := GlobalLock(pd.hDevMode);
    DevMode.dmFields := DevMode.dmFields or DM_COPIES or DM_COLLATE;
    DevMode.dmCopies := pd.nCopies;
    if (pd.Flags and PD_COLLATE) <> 0 then
      DevMode.dmCollate := DMCOLLATE_TRUE
    else
      DevMode.dmCollate := DMCOLLATE_FALSE;
    GlobalUnlock(pd.hDevMode)
  end;

  try
    Result := PrintDlg(pd);
    if Result then
    begin
      DevNames := PDevNames(GlobalLock(pd.hDevNames));
      DevMode := PDevMode(GlobalLock(pd.hDevMode));

      if DevMode.dmFields and DM_COLLATE <> 0 then
      begin
        Self.Collate := DevMode.dmCollate = DMCOLLATE_TRUE;
        DevMode.dmCollate := DMCOLLATE_TRUE;
      end
      else
        Self.Collate := (pd.Flags and PD_COLLATE) <> 0;

      if DevMode.dmFields and DM_COPIES <> 0 then
      begin
        Self.Copies := DevMode.dmCopies;
        DevMode.dmCopies := 1;
      end
      else
        Self.Copies := pd.nCopies;

      Self.FromPage := pd.nFromPage;
      Self.ToPage := pd.nToPage;

      prPrinter.PrinterName := StrPas(PChar(DevNames) + DevNames^.wDeviceOffset);
      Result := prPrinter.SetDevMode(DevMode, GlobalSize(pd.hDevMode));
      GlobalUnlock(pd.hDevNames);
      GlobalUnLock(pd.hDevMode);

      if not Result then
      begin
        if not Result then
          MBError(Format(prLoadStr(sPrinterErrorUnknown),[prPrinter.PrinterName]));
      end;
    end;
    if Assigned(OnCloseSetupDialog) then
      OnCloseSetupDialog(Self, Result);
  finally
    GlobalFree(pd.hDevNames);
    GlobalFree(pd.hDevMode);
  end;
end;

function TprReport.PrintPreparedReport;
var
  f : boolean;
  L : TList;
  rdi : rprDrawInfo;
  i, j, _FromPage, _ToPage : integer;

  procedure PrintPage(pn : Integer);
  var
    fPaperChanged : boolean;
    ep : TprEndPage;
    pps : pprUserSelectPaperSize;
    OldOrgEx : TPoint;
    Orientation : TprPrinterOrientation;
    i,dx,dy : integer;
    k: Double;
    ObjectsRect : TRect;
    PaperWidth,PaperHeight,PaperSize : integer;

    function DPIX(X: Integer): Integer;
    begin
      if prPrinter.PixelsPerX > prPrinter.PixelsPerY then
        Result := MulDiv(X, prPrinter.PixelsPerX, prPrinter.PixelsPerY)
      else
        Result := X;
    end;

    function DPIY(Y: Integer): Integer;
    begin
      if prPrinter.PixelsPerY > prPrinter.PixelsPerX then
        Result := MulDiv(Y, prPrinter.PixelsPerY, prPrinter.PixelsPerX)
      else
        Result := Y;
    end;

    procedure GetScaleKoef(var kx,ky : Double; x1,x2,y1,y2 : integer; var k : Double);
    begin
      if x1 / x2 > y1 / y2 then
        k := y1 / y2
      else
        k := x1 / x2;

      kx := kx * k;
      ky := ky * k;
    end;

    function IsEqualsPaperSizes(APrinterPageInfo: TprPageInfo; APageWidth, APageHeight: Integer): Boolean;
    begin
      if UseMMWhenCheckPaperSizes then
        Result := (mm(APrinterPageInfo.PageWidth) = mm(APageWidth)) and
                  (mm(APrinterPageInfo.PageHeight) = mm(APageHeight))
      else
        Result := (APrinterPageInfo.PageWidth = APageWidth) and
                  (APrinterPageInfo.PageHeight = APageHeight);
    end;

  begin
  ep := TprEndPage(EndPages[pn]);

  rdi.prnkx := 1;
  rdi.prnky := 1;
  rdi.kx := 1;
  rdi.ky := 1;

  fPaperChanged := false;
  if not IsEqualsPaperSizes(prPrinter.PageInfo, ep.PageInfo.PageWidth, ep.PageInfo.PageHeight) then
    begin
      // init page set paper sizes
      if not prPrinter.FindPaperSize(ep.PageInfo.PageWidth,ep.PageInfo.PageHeight,PaperSize,Orientation,true) then
        begin
          // check max paper sizes for printer
          if ((prPrinter.mmMaxPageWidth>=ep.PageInfo.PageWidth) and
              (prPrinter.mmMaxPageHeight>=ep.PageInfo.PageHeight)) or
             ((prPrinter.mmMaxPageWidth>=ep.PageInfo.PageHeight) and
              (prPrinter.mmMaxPageHeight>=ep.PageInfo.PageWidth)) then
            begin
              PaperWidth := ep.PageInfo.PageWidth;
              PaperHeight := ep.PageInfo.PageHeight;
              if PaperWidth>PaperHeight then
                Orientation := poLandscape
              else
                Orientation := poPortrait;
              PaperSize := -1;
            end
          else
            begin
              // pagesize not supported by selected printer
              // select other pagesize and scale page
              case ep.PageScaleInfo.ScaleMode of
                psmToNearest:
                  begin
                    // find nearest suppoted papersize
                    PaperSize := -1;
                    PaperWidth := Min(prPrinter.mmMaxPageWidth,prPrinter.mmMaxPageHeight);
                    PaperHeight := Max(prPrinter.mmMaxPageWidth,prPrinter.mmMaxPageHeight);
                    if ep.PageInfo.PageWidth>ep.PageInfo.PageHeight then
                      Orientation := poLandscape
                    else
                      Orientation := poPortrait;
                  end;
                psmToDefined:
                  begin
                    // select paper size which defined in ep.PageScaleInfo
                    PaperSize := ep.PageScaleInfo.PaperSize;
                    Orientation := ep.PageScaleInfo.Orientation;
                    PaperWidth := ep.PageScaleInfo.PageWidth;
                    PaperHeight := ep.PageScaleInfo.PageHeight;
                  end;
                else
                  begin
                    // Show dialog window in which user select needed papersize
                    // and store selected setting
                    i := 0;
                    while (i<L.Count) and
                          ((pprUserSelectPaperSize(L[i]).GenPageWidth<>ep.PageInfo.PageWidth) or
                           (pprUserSelectPaperSize(L[i]).GenPageHeight<>ep.PageInfo.PageHeight)) do Inc(i);
                    if i>=L.Count then
                      begin
                        GetMem(pps,sizeof(rprUserSelectPaperSize));
                        pps.GenPageWidth := ep.PageInfo.PageWidth;
                        pps.GenPageHeight := ep.PageInfo.PageHeight;
                        TprSelectPrintPaperSizeForm.Create(Application).SelectPrintPaperSize(Self,ep,pps^);
                        if pps.DontAsk then
                          L.Add(pps);
                      end
                    else
                      pps := pprUserSelectPaperSize(L[i]);
                    if pps.Action=pusaSkip then
                      exit
                    else
                      begin
                        PaperSize := pps.PaperSize;
                        Orientation := pps.Orientation;
                        PaperWidth := pps.PageWidth;
                        PaperHeight := pps.PageHeight;
                      end;
                    if not pps.DontAsk then
                      FreeMem(pps);
                  end;
              end;
            end;
        end
      else
        begin
          // paper exists
          i := prPrinter.GetPaperSizeIndex(PaperSize);
          if Orientation=poPortrait then
            begin
              PaperWidth := prPrinter.PaperWidths[prPrinter.PaperSizes[i]];
              PaperHeight := prPrinter.PaperHeights[prPrinter.PaperSizes[i]];
            end
          else
            begin
              PaperHeight := prPrinter.PaperWidths[prPrinter.PaperSizes[i]];
              PaperWidth := prPrinter.PaperHeights[prPrinter.PaperSizes[i]];
            end;
        end;

      // initialize printer
      fPaperChanged := not IsEqualsPaperSizes(prPrinter.PageInfo, PaperWidth, PaperHeight);
      if fPaperChanged then
        begin
          if f then
            prPrinter.EndDoc;
          if not prPrinter.SetPageSize(PaperSize,Orientation,PaperWidth,PaperHeight) then
            begin
            end;
          prPrinter.BeginDoc;
        end;
    end;
  if not fPaperChanged then
    begin
      if f then
        prPrinter.NewPage
      else
        prPrinter.BeginDoc;
    end;

  GetScaleKoef(rdi.prnkx, rdi.prnky,
               prPrinter.PageInfo.PageWidth,
               ep.PageInfo.PageWidth,
               prPrinter.PageInfo.PageHeight,
               ep.PageInfo.PageHeight,
               k);

  GetScaleKoef(rdi.kx, rdi.ky,
               prPrinter.PixelPageWidth,
               DPIX(ep.PixelPageWidth),
               prPrinter.PixelPageHeight,
               DPIY(ep.PixelPageHeight),
               k);
  // now - analyze minimal printer margins,
  // Probably with allowance for of margins of the printer not all can place on page
  dx := 0;
  dy := 0;
  if ep.PageScaleInfo.FitObjects then
    begin
      ObjectsRect := Rect(MaxInt,MaxInt,Low(integer),Low(integer));
      for i:=0 to ep.VL.Count-1 do
        with TprExObjRecVersion(ep.vl[i]).GeneratedRect do
        begin
          if ObjectsRect.Left > Left then
            ObjectsRect.Left := Left;
          if ObjectsRect.Top > Top then
            ObjectsRect.Top := Top;
          if ObjectsRect.Right < Right then
            ObjectsRect.Right := Right;
          if ObjectsRect.Bottom < Bottom then
            ObjectsRect.Bottom := Bottom;
        end;
      with ObjectsRect do
      begin
        Left := DPIX(Left);
        Top := DPIY(Top);
        Right := DPIX(Right);
        Bottom := DPIY(Bottom);
      end;
      ObjectsRect := MulRect(ObjectsRect, rdi.kx, rdi.ky);

      if (ObjectsRect.Left < prPrinter.PixelPageRect.Left) or
         (ObjectsRect.Top < prPrinter.PixelPageRect.Top) or
         (ObjectsRect.Right > prPrinter.PixelPageRect.Right) or
         (ObjectsRect.Bottom > prPrinter.PixelPageRect.Bottom) then
        begin
          // we must correct rect
          if (ObjectsRect.Right - ObjectsRect.Left > prPrinter.PixelPageRect.Right - prPrinter.PixelPageRect.Left) or
             (ObjectsRect.Bottom - ObjectsRect.Top > prPrinter.PixelPageRect.Bottom - prPrinter.PixelPageRect.Top) then
            begin
              // we must scale ObjectsRect
              GetScaleKoef(rdi.prnkx, rdi.prnky,
                           prPrinter.PixelPageRect.Right - prPrinter.PixelPageRect.Left,
                           ObjectsRect.Right - ObjectsRect.Left,
                           prPrinter.PixelPageRect.Bottom - prPrinter.PixelPageRect.Top,
                           ObjectsRect.Bottom - ObjectsRect.Top,
                           k);
              rdi.kx := rdi.kx * k;
              rdi.ky := rdi.ky * k;
              ObjectsRect := MulRect(ObjectsRect, k, k);
            end;

          // offset ObjectsRect
          if ObjectsRect.Left < prPrinter.PixelPageRect.Left then
            dx := prPrinter.PixelPageRect.Left - ObjectsRect.Left
          else
            if ObjectsRect.Right > prPrinter.PixelPageRect.Right then
              dx := ObjectsRect.Right - prPrinter.PixelPageRect.Right
            else
              dx := 0;
          if ObjectsRect.Top<prPrinter.PixelPageRect.Top then
            dy := prPrinter.PixelPageRect.Top - ObjectsRect.Top
          else
            if ObjectsRect.Bottom > prPrinter.PixelPageRect.Bottom then
              dy := ObjectsRect.Bottom - prPrinter.PixelPageRect.Bottom
            else
              dy := 0;
        end;
    end;

  // correct kx, ky - use printer DPI
  if prPrinter.PixelsPerX > prPrinter.PixelsPerY then
    rdi.kx := rdi.kx * prPrinter.PixelsPerX / prPrinter.PixelsPerY
  else
    rdi.ky := rdi.ky * prPrinter.PixelsPerY / prPrinter.PixelsPerX;

  SetViewportOrgEx(prPrinter.PrinterDC,
                   -prPrinter.PixelPageRect.Left + dx,
                   -prPrinter.PixelPageRect.Top + dy,
                   @OldOrgEx);
  for i:=0 to ep.vl.Count-1 do
    TprExObjRecVersion(ep.vl[i]).Draw(prPrinter.PrinterDC, @rdi);
  SetViewportOrgEx(prPrinter.PrinterDC,OldOrgEx.X,OldOrgEx.Y,nil);

  f := true;
  end;

begin
  Result := false;

  if (prPrinter.PrinterName = '') or
     (prPrinter.PrinterName = prLoadStr(sDefaultPrinterName)) or
     (prPrinter.Printers.IndexOf(prPrinter.PrinterName) = -1) then
     prPrinter.PrinterName := prGetDefaultPrinterName;

  prPrinter.CheckPrinter;

  FActionCanceled := false;
  rdi.IsPrinter := true;
  rdi.Report := Self;
  rdi.ppdi := nil;
  _FromPage := 0;
  _ToPage := EndPagescount-1;
  if PrintPagesMode = ppmPagesRange then
  begin
    _FromPage := Min(_ToPage, Max(_FromPage, FromPage - 1));
    _ToPage := Max(_FromPage, Min(_ToPage, ToPage - 1));
  end;

  if ShowProgress then
    CreateProgressForm(prLoadStr(sPrintReportCaption),0);
  
  if Title<>'' then
    prPrinter.Title := Title
  else
    prPrinter.Title := prLoadStr(sNoReportName);

  DoOnPrintStart;

  L := TList.Create;
  try
    try
      f := false;
      
      if Collate then
      begin
        for I := 1 to Copies do
        begin
          FPrintingCopy := I;
          if PrintPagesMode = ppmPagesList then
          begin
            for j:=0 to PrintPagesList.Count-1 do
            begin
              FPrintingPage := integer(PrintPagesList[j]);
              if (FPrintingPage > 0) and (FPrintingPage <= EndPagesCount) then
              begin
                UpdateProgressForm(Format(prLoadStr(sPrintReport),[FPrintingPage, FPrintingCopy, PrintPagesList.Count, Copies]));
                PrintPage(FPrintingPage - 1);
              end;
            end;
          end
          else
          begin
            for J := _FromPage to _ToPage do
            begin
              FPrintingPage := J + 1;
              UpdateProgressForm(Format(prLoadStr(sPrintReport),[FPrintingPage, FPrintingCopy, _ToPage-_FromPage+1,Copies]));
              PrintPage(J);
            end;
          end;
        end;
      end
      else
      begin
        if PrintPagesMode = ppmPagesList then
        begin
          for I := 0 to PrintPagesList.Count - 1 do
            for J := 1 to Copies do
            begin
              FPrintingCopy := J;
              FPrintingPage := Integer(PrintPagesList[i]);
              if (FPrintingPage > 0) and (FPrintingPage <= EndPagesCount) then
              begin
                UpdateProgressForm(Format(prLoadStr(sPrintReport), [FPrintingPage, FPrintingCopy, PrintPagesList.Count, Copies]));
                PrintPage(FPrintingPage - 1);
              end;
            end;
        end
        else
        begin
          for I := _FromPage to _ToPage do
            for J := 1 to Copies do
            begin
              FPrintingPage := I + 1;
              FPrintingCopy := J;
              UpdateProgressForm(Format(prLoadStr(sPrintReport),[FPrintingPage, FPrintingCopy, _ToPage - _FromPage + 1, Copies]));
              PrintPage(I);
            end;
        end;
      end;

      Result := true;
  
    except
      on E : Exception do
        begin
          if E is EActionCanceled then
            FActionCanceled := true
          else
            raise;
        end;
    end
  
  finally
    for i:=0 to L.Count-1 do
      FreeMem(pprUserSelectPaperSize(L[i]));
    L.Free;
    prPrinter.EndDoc;
    CloseProgressForm;
    if Result then
      DoOnPrintComplete;
  end;
end;

function TprReport.GetBandClass;
begin
Result := TprBandClass(GetClass('Tpr'+Copy(GetEnumName(TypeInfo(TprBandType),integer(BandType)),3,Length(GetEnumName(TypeInfo(TprBandType),integer(BandType))))+'Band'));
end;

function TprReport.SetupExportParams;
begin
Result := TprExportParamsForm.Create(nil).EditParams(Self);
end;

procedure TprReport.ExportTo;
begin
if EndPagesCount<=0 then
  raise Exception.Create(prLoadStr(sReportEmptyInExport));
inherited;
end;

var
  I: Integer;

initialization

  prRichEditOleCallback := TprRichEditOleCallback.Create;

  RegisterClass(TprHTitleBand);
  RegisterClass(TprHSummaryBand);
  RegisterClass(TprHPageHeaderBand);
  RegisterClass(TprHPageFooterBand);
  RegisterClass(TprHDetailBand);
  RegisterClass(TprHDetailHeaderBand);
  RegisterClass(TprHDetailFooterBand);
  RegisterClass(TprHGroupHeaderBand);
  RegisterClass(TprHGroupFooterBand);
  RegisterClass(TprVTitleBand);
  RegisterClass(TprVSummaryBand);
  RegisterClass(TprVPageHeaderBand);
  RegisterClass(TprVPageFooterBand);
  RegisterClass(TprVDetailBand);
  RegisterClass(TprVDetailHeaderBand);
  RegisterClass(TprVDetailFooterBand);
  RegisterClass(TprVGroupHeaderBand);
  RegisterClass(TprVGroupFooterBand);

  RegisterClass(TprPage);
  RegisterClass(TprMemoObj);
  RegisterClass(TprImageObj);
  RegisterClass(TprRichObj);
  RegisterClass(TprImageObj);

  RegisterClass(TprMemoObjRecVersion);
  RegisterClass(TprImageObjRecVersion);
  RegisterClass(TprRichObjRecVersion);

  // register bands
  for I := integer(Low(TprBandType)) to integer(High(TprBandType)) do
    prRegisterBand(TprBandClass(GetClass('Tpr'+Copy(GetEnumName(TypeInfo(TprBandType),i),3,Length(GetEnumName(TypeInfo(TprBandType),i)))+'Band')),
                   TprReport,
                   'TprBandEditorForm');

  // register objects
  prRegisterObj(TprMemoObj,
                TprMemoObjRec,
                TprReport,
                sMemoObjCaption,
                sMemoObjHint,
                'TprMemoEditorForm',
                'TprPrvMemoEditorForm');
                
  prRegisterObj(TprImageObj,
                TprImageObjRec,
                TprReport,
                sImageObjCaption,
                sImageObjHint,
                'TprImageEditorForm',
                'TprPrvImageEditorForm');

  prRegisterObj(TprRichObj,
                TprRichObjRec,
                TprReport,
                sRichObjCaption,
                sRichObjHint,
                'TprRichEditorForm',
                'TprPrvRichEditorForm');

finalization

  FreeAndNil(prRichEditOleCallback);
{
  if RtfDllHandle <> 0 then
  begin
    FreeLibrary(RtfDllHandle);
    RtfDllHandle := 0;
  end;
}

end.

