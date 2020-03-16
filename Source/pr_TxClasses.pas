{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the declarations of PReport classes creating the Text reports.
<ul>
<li>TprTxReport - Main component. It contains all necessary methods for storing,
building, previewing, printing and exporting Windows reports.</li>
<li>TprTxPage - Represents the separate page of the report template for the TprTxReport component.</li>
<li>TprTxEndPage - Represents the generated page for the TprTxReport component.</li>
</ul>}
unit pr_TxClasses;

{$i pr.inc}

interface

uses
  SysUtils, Windows, Classes, Graphics, WinSpool, DB,
  Dialogs, typinfo, Pr_Utils, Math, Forms, IniFiles, Messages, Controls,
  stdctrls, menus, CommDlg, 
  {$ifdef PR_D6_D7} types, {$endif}

  pr_Common, pr_Progress, pr_TxConsts, pr_MultiLang;

type

TprTextDevice = class;
TprTxReport = class;

/////////////////////////////////////////////////
//
// rTxExData
//
/////////////////////////////////////////////////
{Internal structure. Represents information about font
used in the report designer.
Syntax:
rTxExData = record
  FontSize: integer;
  FontName: string;
  SymbolSize: TSize;
end;}
rTxExData = record
  FontSize: integer;
  FontName: string;
  SymbolSize: TSize;
end;
{Pointer to the rTxExData structure.}
PTxExData = ^rTxExData;

/////////////////////////////////////////////////
//
// TprTxMemoObjRecVersion
//
/////////////////////////////////////////////////
{Represents the object view for TprTxMemoObj.
See also:
  TprTxMemoObj}
TprTxMemoObjRecVersion = class(TprObjRecVersion)
private
  FMemo: TStrings;
  FDeleteEmptyLinesAtEnd: Boolean;
  FDeleteEmptyLines: Boolean;
  FCanResizeX: Boolean;
  FCanResizeY: Boolean;
  FJustifyLastLine: Boolean;
  FEolIsEndOfParagraph: Boolean;

  FhAlign : TprHAlign;
  FvAlign : TprvAlign;
  FWordWrap : boolean;
  FDefaultFont : boolean;
  FTxFontStyleEx : TprTxFontStyle;
  FTxFontOptionsEx : TprTxFontOptions;

  FBrdLeft: Char;
  FBrdLeftTop: Char;
  FBrdTop: Char;
  FBrdRightTop: Char;
  FBrdRight: Char;
  FBrdRightBottom: Char;
  FBrdBottom: Char;
  FBrdLeftBottom: Char;

  SecondPassNeeded : boolean; // true - on second pass Memo must be reformatted
  procedure SetTxFontOptionsEx(Value : TprTxFontOptions);
  function StoredTxFontOptionsEx : boolean;
  function StoredTxFontStyleEx : boolean;
protected
  procedure DefineProperties(Filer : TFiler); override;
  procedure ReadTxFontStyle(Reader : TReader);
  procedure WriteTxFontStyle(Writer : TWriter);
  procedure ReadTxFontOptions(Reader : TReader);
  procedure WriteTxFontOptions(Writer : TWriter);
  procedure ReadTxFontStyleEx(Reader : TReader);
  procedure WriteTxFontStyleEx(Writer : TWriter);
  procedure ReadTxFontOptionsEx(Reader : TReader);
  procedure WriteTxFontOptionsEx(Writer : TWriter);

  procedure CalcInnerRect(const AWholeRect: TRect; var AInnerRect: TRect);
  procedure BuildBorders(AWidth, AHeight: Integer; var ALeft, ATop, ARight, ABottom: string; var ATopOffs, ABottomOffs: Integer);
public
{Specifies the font style for object.
See also:
  TprTxFontStyle}
  property TxFontStyleEx: TprTxFontStyle read FTxFontStyleEx write FTxFontStyleEx;
{Specifies the font options for object (bold, italic).
See also:
  TprTxFontStyle}
  property TxFontOptionsEx: TprTxFontOptions read FTxFontOptionsEx write SetTxFontOptionsEx;
{See:
  TprObjRecVersion.Assign}
  procedure Assign(Source: TPersistent); override;

{Creates an instance of the TprTxMemoObjRecVersion class.}
  constructor Create(Collection: TCollection); override;
{Frees an instance of the TprTxMemoObjRecVersion class.}
  destructor Destroy; override;
published
{Specifies the memo's content.}
  property Memo : TStrings read FMemo write FMemo;
{Specifies the value indicating whether the empty lines of text at the bottom of object must be deleted,
on the stage of generating of the report.}
  property DeleteEmptyLinesAtEnd : boolean read FDeleteEmptyLinesAtEnd write FDeleteEmptyLinesAtEnd default false;
{Specifies the value indicating whether the empty lines of text must be deleted,
on the stage of generating of the report.}
  property DeleteEmptyLines : boolean read FDeleteEmptyLines write FDeleteEmptyLines default false;
{Specifies the value indicating whether the object’s width should be automatically calculated based
on the widest string. If this property is true then the object width can be
changed on the stage of generating of report.
This property is not used if the WordWrap property equals to true.}
  property CanResizeX : boolean read FCanResizeX write FCanResizeX default false;
{Specifies the value indicating whether the object's height should be automatically calculated based on
the count of lines in the Memo property. If this property is true then the object height can be
changed on the stage of generating of report.}
  property CanResizeY : boolean read FCanResizeY write FCanResizeY default false;

{Specifies the horizontal alignment of the text in a object.
See also:
  TprHAlign}
  property hAlign : TprHAlign read FhAlign write FhAlign default prhLeft;
{Specifies the vertical alignment of the text in a object.
See also:
  TprVAlign}
  property vAlign : TprvAlign read FvAlign write FvAlign default prvTop;
{Specifies the value indicating whether the default font (specified for page) must be used for
printing the memo. The TxFontStyleEx and TxFontOptionsEx properties
are ignored if this property equals to true.}
  property DefaultFont : boolean read FDefaultFont write FDefaultFont default false;
{Specifies value indicating whether a object automatically wraps words to the beginning
of the next line when necessary.}
  property WordWrap : boolean read FWordWrap write FWordWrap default false;
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
{Specifies the char displaying as the left side of border.
Char must be specified in OEM codepage.}
  property BrdLeft: Char read FBrdLeft write FBrdLeft default #0;
{Specifies the char displaying as the top-left corner of border.
Char must be specified in OEM codepage.}
  property BrdLeftTop: Char read FBrdLeftTop write FBrdLeftTop default #0;
{Specifies the char displaying as the top side of border.
Char must be specified in OEM codepage.}
  property BrdTop: Char read FBrdTop write FBrdTop default #0;
{Specifies the char displaying as the top-right corner of border.
Char must be specified in OEM codepage.}
  property BrdRightTop: Char read FBrdRightTop write FBrdRightTop default #0;
{Specifies the char displaying as the right side of border.
Char must be specified in OEM codepage.}
  property BrdRight: Char read FBrdRight write FBrdRight default #0;
{Specifies the char displaying as the right-bottom corner of border.
Char must be specified in OEM codepage.}
  property BrdRightBottom: Char read FBrdRightBottom write FBrdRightBottom default #0;
{Specifies the char displaying as the bottom side of border.
Char must be specified in OEM codepage.}
  property BrdBottom: Char read FBrdBottom write FBrdBottom default #0;
{Specifies the char displaying as the left-bottom corner of border.
Char must be specified in OEM codepage.}
  property BrdLeftBottom: Char read FBrdLeftBottom write FBrdLeftBottom default #0;
end;

/////////////////////////////////////////////////
//
// TprTxMemoObjRec
//
/////////////////////////////////////////////////
{Represents a record of properties for the TprTxMemoObj object.
See also:
  TprTxMemoObj, TprTxMemoObjRecVersion.}
TprTxMemoObjRec = class(TprObjRec)
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
  TprObjRec.ThirdPass}
  procedure ThirdPass(AEndPage: TprCustomEndPage; Device: TObject; const r: TRect); override;
{See:
  TprObjRec.SecondPass}
  procedure SecondPass; override;
published
{Specifies the value indicating whether the object can split should it fall on a page break.}
  property CanSplit;
end;

/////////////////////////////////////////////////
//
// TprTxMemoObj
//
/////////////////////////////////////////////////
{"Text" object represents rectangle with multiline text inside it.
You can set all font attributes and text align.
In the memo of "Text" object you can place multiline
text with variables, data fields or expressions.
As all other objects in PReport the TprTxMemoObj can have many views (it has one view after creating)
the necessary  object view is selected on the stage of report generating.
TprTxMemoObj supports the "inplace editing" at design-time.
See also:
  TprTxMemoObjRecVersion, TprTxMemoObjRec}
TprTxMemoObj = class(TprObj)
private
  function GetVersion(Index: Integer): TprTxMemoObjRecVersion;
  function GetGenVersion(Index: Integer): TprTxMemoObjRecVersion;
  function GetDefVersion: TprTxMemoObjRecVersion;
  function GetGenCurVersion: TprTxMemoObjRecVersion;
protected
  procedure DrawBorder(DC: HDC; const APixelRect: TRect; ACharWidth, ACharHeight: Integer; v: TprTxMemoObjRecVersion; AExData: PTxExData);
  procedure InitdRec; override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
  procedure OnDsgnPopupMenuFontStyleClick(Sender : TObject);
  procedure OnDsgnPopupMenuFontOptionClick(Sender : TObject);
public
{See:
  TprObj.DrawDesign}
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
{See:
  TprObj.GetDesc}
  function GetDesc : string; override;

{See:
  TprObj.DsgnAllowInplaceEdit}
  function DsgnAllowInplaceEdit : boolean; override;
{See:
  TprObj.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean); override;
{See:
  TprObj.InplaceEdit}
  procedure InplaceEdit(_Parent : TWinControl; var InplaceEditor : TWinControl; const InplaceRect : TRect; ExData : pointer); override;
{See:
  TprObj.SaveInplaceEdit}
  procedure SaveInplaceEdit(InplaceEditor : TWinControl); override;
{See:
  TprObj.FirstPass}
  procedure FirstPass; override;

{See:
  TprObj.Versions}
  property Versions[Index: Integer]: TprTxMemoObjRecVersion read GetVersion;
{See:
  TprObj.GenVersions}
  property GenVersions[Index: Integer]: TprTxMemoObjRecVersion read GetGenVersion;
{See:
  TprObj.DefVersion}
  property DefVersion: TprTxMemoObjRecVersion read GetDefVersion;
{See:
  TprObj.GenCurVersion}
  property GenCurVersion: TprTxMemoObjRecVersion read GetGenCurVersion;
end;

/////////////////////////////////////////////////
//
// TprTxCommandObjRecVersion
//
/////////////////////////////////////////////////
{Represents the object view for TprTxCommandObj.
See also:
  TprTxCommandObj}
TprTxCommandObjRecVersion = class(TprObjRecVersion)
private
  FTxCommands: TList;
  function GetTxCommand(i : integer) : TprTxCommand;
  function GetTxCommandsCount : integer;
protected
  procedure DefineProperties(Filer : TFiler); override;
  procedure ReadTxCommands(Reader : TReader);
  procedure WriteTxCommands(Writer : TWriter);
  function StoredTxCommands: boolean;
public
{Lists the commands which are defined for this object.
See also:
  TprTxCommand, TprTxReportOptions}
  property TxCommands[I: integer]: TprTxCommand read GetTxCommand;
{Returns the number of items in the TxCommands property. }
  property TxCommandsCount: Integer read GetTxCommandsCount;

{See:
  TprObjRecVersion.Assign}
  procedure Assign(Source: TPersistent); override;
{Adds the command to the commands' list.
Parameter:
  TxCommand - The TprTxCommand object to add.
See also:
  TprTxCommand}
  procedure AddTxCommand(TxCommand: TprTxCommand); overload;
{Adds the command to the commands' list.
Parameter:
  TxCommandName - The name of the TprTxCommand object.
See also:
  TprTxCommand}
  procedure AddTxCommand(const TxCommandName: string); overload;
{Removes the command to the commands' list.
Parameter:
  TxCommand - The TprTxCommand object to remove.
See also:
  TprTxCommand}
  procedure RemoveTxCommand(TxCommand: TprTxCommand); overload;
{Removes the command to the commands' list.
Parameter:
  TxCommand - The name of the TprTxCommand object to remove.
See also:
  TprTxCommand}
  procedure RemoveTxCommand(const TxCommandName: string); overload;
{Clears the commands' list.}
  procedure ClearTxCommands;

{Creates an instance of the TprTxObjRecVersion class.}
  constructor Create(Collection: TCollection); override;
{Frees an instance of the TprTxObjRecVersion class.}
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TprTxCommandObjRec
//
/////////////////////////////////////////////////
{Represents a record of properties for the TprTxCommandObj object.
See also:
  TprTxCommandObj, TprTxCommandObjRecVersion.}
TprTxCommandObjRec = class(TprObjRec)
protected
  function GetVersionClass: TprObjVersionClass; override;
public
{See:
  TprObjRec.ThirdPass}
  procedure ThirdPass(AEndPage: TprCustomEndPage; Device: TObject; const r : TRect); override;
{See:
  TprObjRec.SecondPass}
  procedure SecondPass; override;
end;

/////////////////////////////////////////////////
//
// TprTxCommandObj
//
/////////////////////////////////////////////////
{Represents the object of TprTxReport that can be used to send to printer the special commands.
Each command is a sequence of chars that is executed by printer, examples of commands:<br>
- form feed.<br>
- carriage return.<br>
- and so on.<br>
The commands which are supported by TprTxReport are listed in the TxRo.ini file, in the
[TxCommands] section. These commands are loaded at programm startup and stored in the
TprTxReportOptions object, use the TxReportOptions global variable to
get access to the loaded instance of the TprTxReportOptions class.
Each printer command is represented by the TprTxCommand object.
Example:
  // this procedure prints all supported commands.
  procedure PrintCommands;
  var
    I: Integer;
    ACommand: TprTxCommand;
  begin
    for I := 0 to TxReportOptions.TxCommandsCount - 1 do
    begin
      ACommand := TxReportOptions.TxCommands[I];
      WriteLn(Format('%d. Command name: %s, Command description: %s', [I + 1, ACommand.Name, ACommand.Description]));
    end;
  end;

  // this event handle adds form feed command to the TprTxCommandObj object.
  procedure TForm1.prTxReport1FirstPassObject(Sender: TObject; Obj: TprObj;
    var ManuallyProcessed: Boolean);
  var
    v: TprTxCommandObjRecVersion;
  begin
    if Obj.Name = 'FormFeedCommand' then
    begin
      with TprTxCommandObj(Obj).GenCurVersion do
      begin
        // FFormFeed - a certain internal variable
        if FFormFeed then
          AddTxCommand('txcFormFeed') // add a form feed command
        else
          ClearTxCommands; // clear commands list
      end;
    end;
  end;

See also:
  TprTxCommandObjRecVersion, TprTxCommand, TprTxReportOptions}
TprTxCommandObj = class(TprObj)
private
  function GetVersion(Index: Integer): TprTxCommandObjRecVersion;
  function GetGenVersion(Index: Integer): TprTxCommandObjRecVersion;
  function GetDefVersion: TprTxCommandObjRecVersion;
  function GetGenCurVersion: TprTxCommandObjRecVersion;
protected
  procedure InitdRec; override;
public
{See:
  TprObj.DrawDesign}
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
{See:
  TprObj.GetDesc}
  function GetDesc : string; override;
{See:
  TprObj.FirstPass}
  procedure FirstPass; override;

{See:
  TprObj.Versions}
  property Versions[Index: Integer]: TprTxCommandObjRecVersion read GetVersion;
{See:
  TprObj.GenVersions}
  property GenVersions[Index: Integer]: TprTxCommandObjRecVersion read GetGenVersion;
{See:
  TprObj.DefVersion}
  property DefVersion: TprTxCommandObjRecVersion read GetDefVersion;
{See:
  TprObj.GenCurVersion}
  property GenCurVersion: TprTxCommandObjRecVersion read GetGenCurVersion;
end;

/////////////////////////////////////////////////
//
// TprTxHTitleBand
//
/////////////////////////////////////////////////
{Implements the horizontal title band in TprTxReport.}
TprTxHTitleBand = class(TprCustomHTitleBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

//////////////////////////////
//
// TprTxHSummaryBand
//
//////////////////////////////
{Implements the horizontal summary band in TprTxReport.}
TprTxHSummaryBand = class(TprCustomHSummaryBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////
//
// TprTxHPageHeaderBand
//
/////////////////////////
{Implements the horizontal page header band in TprTxReport.}
TprTxHPageHeaderBand = class(TprCustomHPageHeaderBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////
//
// TprTxHPageFooterBand
//
/////////////////////////
{Implements the horizontal page footer band in TprTxReport.}
TprTxHPageFooterBand = class(TprCustomHPageFooterBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

///////////////////////////////////
//
// TprTxHDetailBand
//
///////////////////////////////////
{Implements the horizontal detail band in TprTxReport.}
TprTxHDetailBand = class(TprCustomHDetailBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

///////////////////////////
//
// TprTxHDetailHeaderBand
//
///////////////////////////
{Implements the horizontal detail header band in TprTxReport.}
TprTxHDetailHeaderBand = class(TprCustomHDetailHeaderBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

////////////////////////////
//
// TprTxHDetailFooterBand
//
////////////////////////////
{Implements the horizontal detail footer band in TprTxReport.}
TprTxHDetailFooterBand = class(TprCustomHDetailFooterBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

///////////////////////////
//
// TprTxHGroupHeaderBand
//
///////////////////////////
{Implements the horizontal group header band in TprTxReport.}
TprTxHGroupHeaderBand = class(TprCustomHGroupHeaderBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

///////////////////////////
//
// TprTxHGroupFooterBand
//
///////////////////////////
{Implements the horizontal group footer band in TprTxReport.}
TprTxHGroupFooterBand = class(TprCustomHGroupFooterBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;







///////////////////////////////
//
// TprTxVTitleBand
//
///////////////////////////////
{Implements the vertical title band in TprTxReport.}
TprTxVTitleBand = class(TprCustomVTitleBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

//////////////////////////////
//
// TprTxVSummaryBand
//
//////////////////////////////
{Implements the vertical summary band in TprTxReport.}
TprTxVSummaryBand = class(TprCustomVSummaryBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////
//
// TprTxVPageHeaderBand
//
/////////////////////////
{Implements the vertical page header band in TprTxReport.}
TprTxVPageHeaderBand = class(TprCustomVPageHeaderBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

/////////////////////////
//
// TprTxVPageFooterBand
//
/////////////////////////
{Implements the vertical page footer band in TprTxReport.}
TprTxVPageFooterBand = class(TprCustomVPageFooterBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

///////////////////////////////////
//
// TprTxVDetailBand
//
///////////////////////////////////
{Implements the horizontal page footer band in TprTxReport.}
TprTxVDetailBand = class(TprCustomVDetailBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

///////////////////////////
//
// TprTxVDetailHeaderBand
//
///////////////////////////
{Implements the vertical detail band in TprTxReport.}
TprTxVDetailHeaderBand = class(TprCustomVDetailHeaderBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

////////////////////////////
//
// TprTxVDetailFooterBand
//
////////////////////////////
{Implements the vertical detail header band in TprTxReport.}
TprTxVDetailFooterBand = class(TprCustomVDetailFooterBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

///////////////////////////
//
// TprTxVGroupHeaderBand
//
///////////////////////////
{Implements the vertical group header band in TprTxReport.}
TprTxVGroupHeaderBand = class(TprCustomVGroupHeaderBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;

///////////////////////////
//
// TprTxVGroupFooterBand
//
///////////////////////////
{Implements the vertical group footer band in TprTxReport.}
TprTxVGroupFooterBand = class(TprCustomVGroupFooterBand)
public
  procedure DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect); override;
end;



{Describes the type of page of the report template.
Items:
  tptPage - The page of report template has the specified number of lines and the generated report will
contain the set of pages separated by the form feed.
  tptRoll - The page of report template has the "unlimited" length an the generated report
will not contain the form feed, use this setting if you want to print the report on roll.
Syntax:
  TprTxPageType = (tptPage, tptRoll);}
TprTxPageType = (tptPage, tptRoll);
/////////////////////////////////////////////////
//
// TprTxPage
//
/////////////////////////////////////////////////
{Represents the separate page of the TprTxReport report template.
See also:
  TprTxReport}
TprTxPage = class(TprCustomPage)
private
  FColNum : integer;
  FLineNum : integer;
  FPageType : TprTxPageType;
  FDefTxFontStyleEx : TprTxFontStyle;
  FDefTxFontOptionsEx : TprTxFontOptions;
  FDsgnColNumDelta : integer;
  FDsgnLineNumDelta : integer;
  procedure SetDefTxFontOptionsEx(Value : TprTxFontOptions);
  function StoredDefTxFontOptionsEx : boolean;
  function StoredDefTxFontStyleEx : boolean;
  function StoredDsgnColNum : boolean;
  function StoredDsgnLineNum : boolean;
  function GetDsgnColNum : integer;
  procedure SetDsgnColNum(Value : integer);
  function GetDsgnLineNum : integer;
  procedure SetDsgnLineNum(Value : integer);
protected
  procedure DefineProperties(Filer : TFiler); override;
  procedure ReadDefTxFontStyle(Reader : TReader);
  procedure WriteDefTxFontStyle(Writer : TWriter);
  procedure ReadDefTxFontOptions(Reader : TReader);
  procedure WriteDefTxFontOptions(Writer : TWriter);
  procedure ReadDefTxFontStyleEx(Reader : TReader);
  procedure WriteDefTxFontStyleEx(Writer : TWriter);
  procedure ReadDefTxFontOptionsEx(Reader : TReader);
  procedure WriteDefTxFontOptionsEx(Writer : TWriter);
  procedure ReadHeight(Reader : TReader);
  procedure ReadWidth(Reader : TReader);
  procedure Loaded; override;
  function DsgnPageRect: TRect; override;
  function GenPageRect: TRect; override;
public
{Specifies the font style that will be used by default for all TprMemoObj objects on the page.
The TprMemoObj can override this setting, it contains the TxFontStyleEx and DefaultFont properties that can be
used for defining the another font style.
This property equals to nil by default that means that the currently selected font style will be used
for printing the page content.
See also:
  TprTxFontStyle, TprTxMemoObj, TprTxMemoObjRecVersion, TprTxMemoObjRecVersion.TxFontStyleEx, TprTxMemoObjRecVersion.DefaultFont}
  property DefTxFontStyleEx: TprTxFontStyle read FDefTxFontStyleEx write FDefTxFontStyleEx;
{Specifies the font options that will be used by default for all TprMemoObj objects on the page.
The TprMemoObj can override this setting, it contains the TxFontOptionsEx and DefaultFont properties that can be
used for defining the another font options.
This property is empty by default that means that the currently selected font options will be used
for printing the page content.
See also:
  TprTxFontOption, TprTxFontOptions, TprTxMemoObj, TprTxMemoObjRecVersion, TprTxMemoObjRecVersion.TxFontOptionsEx, TprTxMemoObjRecVersion.DefaultFont}
  property DefTxFontOptionsEx: TprTxFontOptions read FDefTxFontOptionsEx write SetDefTxFontOptionsEx stored StoredDefTxFontOptionsEx;

{Creates an instance of the TprTxPage class.
Parameters:
  AOwner - The component - owner.}
  constructor Create(AOwner : TComponent); override;
{Frees an instance of the TprTxPage class.}
  destructor Destroy; override;
published
{Specifies the type of page of the report template.
See also:
  TprTxPageType}
  property PageType: TprTxPageType read FPageType write FPageType default tptPage;
{Specifies the number of lines per height of page.
At the stage of generating of report the report engine inserts the form feed command each time when
the number of lines on the currently generated page becomes more than value of this property.
This property is ignored if TprTxPageType equals to tptRoll.
See also:
  PageType, ColNum}
  property LineNum: integer read FLineNum write FLineNum default 60;
{Specifies the number of chars per width of page.
See also:
  LineNum}
  property ColNum: integer read FColNum write FColNum default 80;
{Specifies the page's width in chars in the design mode.
This property equals to Width property by default.
Use this property if you want to create the report template which is not fitted in the default page width at design-time.}
  property DsgnColNum: integer read GetDsgnColNum write SetDsgnColNum stored StoredDsgnColNum;
{Specifies the page's height in chars in the design mode.
This property equals to Height property by default.
Use this property if you want to create the report template which is not fitted in the default page height at design-time.}
  property DsgnLineNum: integer read GetDsgnLineNum write SetDsgnLineNum stored StoredDsgnLineNum;
end;

/////////////////////////////////////////////////
//
// TprTxEndPage
//
/////////////////////////////////////////////////
{Represents the separate page of the generated TprTxReport report.
As against TprReport objects TprTxEndPage are stored only at a stage of
the report's building and can not be accessible after the report generating.
For TprTxReport the generated report is a list of
strings in which pages are divided by a prefix of a txcFormFeed command.
See also:
  TprTxReport}
TprTxEndPage = class(TprCustomEndPage)
private
  FColNum : integer;
  FLineNum : integer;
  FPageType : TprTxPageType;
  FTextDevice : TprTextDevice;
  FCurDefTxFontStyleEx : TprTxFontStyle;
  FCurDefTxFontOptionsEx : TprTxFontOptions;
  procedure SetCurDefTxFontOptionsEx(Value : TprTxFontOptions);
  function GetReport: TprTxReport;
protected
  function GetWidth : integer; override;
  function GetHeight : integer; override;

  procedure ThirdPass; override;

  property CurDefTxFontStyleEx: TprTxFontStyle read FCurDefTxFontStyleEx write FCurDefTxFontStyleEx;
  property CurDefTxFontOptionsEx: TprTxFontOptions read FCurDefTxFontOptionsEx write SetCurDefTxFontOptionsEx;
public
{Returns the number of chars per width of page.
See also:
  LineNum}
  property ColNum: integer read FColNum;
{Returns the number of lines per height of page.
See also:
  ColNum}
  property LineNum: integer read FLineNum;
{Returns the type of page.
See also:
  TprTxPageType}
  property PageType: TprTxPageType read FPageType;
{Returns the TprTextDevice object that is used for writing the objects' content in the
generated report.
See also:
  TprTextDevice}
  property TextDevice: TprTextDevice read FTextDevice;
{Returns the TprTxReport object containg this object.
See also:
  TprCustomReport}
  property Report: TprTxReport read GetReport;

{Creates an instance of the TprTxEndPage class.
Parameters:
  _Page - The TprTxPage object on the basis of which this page is generated.}
  constructor Create(_Page: TprCustomPage); override;
{Creates an instance of the TprTxEndPage class.
Parameters:
  _Report - The TprTxReport object containing this page.}
  constructor CreateEmpty(_Report: TprCustomReport); override;
{Frees an instance of the TprTxEndPage class.}
  destructor Destroy; override;
end;

{Internal structure.
Syntax:
rTextDeviceRec = packed record
  rPlace: TRect;
  UserData: TprPreviewUserData;
end;}
rTextDeviceRec = packed record
  rPlace : TRect;
  UserData : TprPreviewUserData;
end;
{Pointer to the rTextDeviceRec structure.}
pTextDeviceRec = ^rTextDeviceRec;

//////////////////////////////////
//
// TprTextDevice
//
//////////////////////////////////
{TprTextDevice is class that is used for writing the content of TprTxReport objects into the generated report.
For TprTxReport the generated report is a list of
strings in which pages are divided by a prefix of a txcFormFeed command.
The strings may contain the special chars' sequences that identify the
printer's commands.
Instance of this class is created by TprReport object.
See also:
  TprTxReport}
TprTextDevice = class(TObject)
private
  Report : TprTxReport;
  CurEndPage : TprTxEndPage;
  CurTopLine : integer;
  CureFormat : string;
  CurDefTxFontStyleEx : TprTxFontStyle;
  CurDefTxFontOptionsEx : TprTxFontOptions;
  procedure ClearRecs;
protected
  procedure BeginEndPage(EndPage: TprTxEndPage);
  procedure Reset;
//  procedure DrawLine(const S: string);
public
  Recs: TList;
{Represents the generated TprTxReport report.}
  SList: TStrings;

{Places the list of strings in the report.
Parameters:
  Memo - The strings' list.
  r - Specifies the text's coordinates relative to the top-left corner of the currently generated page of template.
  vAlign - Specifies the vertical text alignment in the rect.
  hAlign - Specifies the horizontal text alignment in the rect.}
  procedure PlaceMemo(Memo: TStrings; const r : TRect; vAlign : TprVAlign; hAlign : TprHAlign);
{Places the TprTxMemoObjRecVersion object in the report.
Parameters:
  rec - The TprTxMemoObjRecVersion object.
  r - Specifies the object's coordinates relative to the top-left corner of the currently generated page of template.
  PreviewUserData - The TprPreviewUserData object that was returned in the OnPreviewGetUserData event.}
  procedure PlaceTxMemo(rec: TprTxMemoObjRecVersion; r : TRect; PreviewUserData : TprPreviewUserData);
{Places the TprTxCommandObjRecVersion object in the report.
Parameters:
  rec - The TprTxCommandObjRecVersion object.
  r - Specifies the object's coordinates relative to the top-left corner of the currently generated page of template.
  PreviewUserData - The TprPreviewUserData object that was returned in the OnPreviewGetUserData event.}
  procedure PlaceTxCommand(rec: TprTxCommandObjRecVersion; const r : TRect; PreviewUserData : TprPreviewUserData);

{Loads the object's content from stream.
Parameters:
  Stream - The source stream.}
  procedure LoadFromStream(Stream: TStream);
{Appends the content from stream to the this object.
Parameters:
  Stream - The source stream.}
  procedure AppendFromStream(Stream: TStream);
{Saves the object's content in the stream.
Parameters:
  Stream - The destination stream.}
  procedure SaveToStream(Stream: TStream);

{Creates an instance of the TprTextDevice class.}
  constructor CreateTextDevice(_Report: TprTxReport);
{Frees an instance of the TprTextDevice class.}
  destructor Destroy; override;
end;

{Describes the export options for TprTxReport.
Items:
  prtxeoLinesRange - The range of lines must be exported.
  prtxeoUseESCModel - All printer commands in the report must be converted to the native
printer commands.}
TprTxExportOptions = (prtxeoLinesRange, prtxeoUseESCModel);
{Describes the export options for TprTxReport.
See also:
  TprTxExportOptions}
TprTxExportOptionsSet = set of TprTxExportOptions;
{Describes the character set for export:
Items:
  prtxcpWIN1251 - the report will be exported in the ANSI character set.
  prtxcpDOS866 - the report will be exported in the DOS character set.}
TprTxCodePage = (prtxcpDOS866, prtxcpWIN1251);

{TprOnPrintLine is the type for TprTxReport.OnPrintLine event.
Event occurs before printing of a line of the report.
Parameters:
  Sender - TprTxReport object firing an event.
  ALineNo - Specifies the number of printed line in the report, zero for first line.
  ALineNoInPage - Specifies the number of printed line within of current page, zero for first line on page.
  AWrapIndex - If line is wrapped then specifies the index of string part, typically this parameter equals to 0.
  ALine - The text of line, can be changed.
  AFinishPrinting - Set this parameter to true to finish the printing.}
TprOnPrintLine = procedure(Sender: TObject; ALineNo: Integer; ALineNoInPage: Integer; AWrapIndex: Integer; var ALineText: string; var AFinishPrinting: Boolean) of object;
///////////////////////////////////
//
// TprTxReport
//
///////////////////////////////////
{TprTxReport component is PReport component creating the TEXT reports.
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
ExportTo method exports it to export filter.
TprTxReport supports the special printer commands, like - form feed, line spacing and so on, also
you can define additional printer commands.}
TprTxReport = class(TprCustomReport)
private
  FPrinterName : string;
  FESCModelName : string;
  FRecodeTableName : string;

  FLeftSpaces : integer;
  FWrapAfterColumn : integer;
  FStartNewLineOnWrap : boolean;
  FEjectPageAfterPrint : boolean;

  FPaperType : TprTxPaperType;

  FUseLinesOnPage : boolean;
  FLinesOnPage : integer;

  FMakeFormFeedOnRulon : boolean;
  FPrintRulonMode : TprTxPrintRulonMode;
  FFromLine : integer;
  FToLine : integer;

  FExportTxOptions : TprTxExportOptionsSet;
  FExportFromLine : integer;
  FExportToLine : integer;
  FExportESCModelName : string;
  FExportCodePage : TprTxCodePage;

  FRecodeTable : TprTxRecodeTable;
  FESCModel : TprESCModel;
  FExportESCModel : TprESCModel;

  FOnPrintLine: TprOnPrintLine;

  FTextDevice : TprTextDevice;

  function GetPage(Index: Integer): TprTxPage;
  function GetEndPage(Index: Integer): TprTxEndPage;

  procedure SetRecodeTableName(Value : string);
  procedure SetESCModelName(Value : string);
  procedure SetExportESCModelName(Value : string);
  function GetFontCharSet : integer;
protected
  function GetDesignerFormClass : string; override;
  function GetPreviewFormClass : string; override;
  function CheckEndPagesCountOnPreview : boolean; override;
  function GetPreparedReportEmpty : boolean; override;
  function CreatePage : TprCustomPage; override;

  function GetPrinterName : string; override;
  procedure SetPrinterName(Value : string); override;

  function CreateEndPage(Page : TprCustomPage) : TprCustomEndPage; override;
  function CreateEmptyEndPage : TprCustomEndPage; override;

  procedure DoOnPrintLine(ALineNo: Integer; ALineNoInPage: Integer; AWrapIndex: Integer; var ALineText: string; var AFinishPrinting: Boolean);
public
{Returns the TprTextDevice object.
See also:
  TprTextDevice}
  property TextDevice: TprTextDevice read FTextDevice;
{Returns the TprTxRecodeTable object a name of which is specified by the RecodeTableName property.
See also:
  RecodeTableName, TprTxRecodeTable}
  property RecodeTable: TprTxRecodeTable read FRecodeTable;
{Returns the TprESCModel object a name of which is specified by the ESCModelName property.
See also:
  ESCModelName, TprESCModel}
  property ESCModel: TprESCModel read FESCModel;
{Returns the TprESCModel object a name of which is specified by the ExportESCModelName property.
See also:
  ExportESCModelName, TprESCModel}
  property ExportESCModel: TprESCModel read FExportESCModel;
  property FontCharSet: integer read GetFontCharSet;
{Lists the pages of the report template.
See also:
  TprTxPage, PagesCount}
  property Pages[index: integer]: TprTxPage read GetPage;
{Lists the generated pages of the report.
Property can be used on the stage of report building only.
See also:
  TprEndPage, EndPagesCount}
  property EndPages[index: integer]: TprTxEndPage read GetEndPage;

{See:
  TprCustomReport.GetBandClass}
  function GetBandClass(BandType : TprBandType) : TprBandClass; override;

{See:
  TprCustomReport.ClearPreparedReport}
  procedure ClearPreparedReport; override;
{See:
  TprCustomReport.PrepareReport}
  function PrepareReport : boolean; override;
{See:
  TprCustomReport.SetupPrintParams}
  function SetupPrintParams : boolean; override;
{See:
  TprCustomReport.PrintPreparedReport}
  function PrintPreparedReport : boolean; override;

{See:
  TprCustomReport.LoadPreparedReport}
  procedure LoadPreparedReport(Stream : TStream); override;
{See:
  TprCustomReport.AppendPreparedReport}
  procedure AppendPreparedReport(Stream : TStream); override;
{See:
  TprCustomReport.SavePreparedReport}
  procedure SavePreparedReport(Stream : TStream); override;

{See:
  TprCustomReport.SetupExportParams}
  function SetupExportParams : boolean; override;
{See:
  TprCustomReport.ExportTo}
  procedure ExportTo; override;

{Converts the string from ANSI to OEM.
The TprRecodeTable object returned by the RecodeTable property is used for converting.
Parameters:
  sSource - The source string.
  sDest - The destination string.}
  procedure WinToOem(sSource,sDest : PChar); overload;
  procedure WinToOem(const sSource: string; var sDest: string); overload;
  function WinToOem(sSource: char): char; overload;
{Converts the string from OEM to ANSI.
The TprRecodeTable object returned by the RecodeTable property is used for converting.
Parameters:
  sSource - The source string.
  sDest - The destination string.}
  procedure OemToWin(sSource,sDest : PChar); overload;
  procedure OemToWin(const sSource: string; var sDest: string); overload;
  function OemToWin(sSource: char): char; overload;

{Returns the used TprTxRecodeTable object: from RecodeTable property or
(if ReodeTable is null) TxReportOptions.DefaultRecodeTable.}
  function GetUsedRecodeTable: TprTxRecodeTable;

{Creates an instance of the TprTxReport class.
Parameters:
  AOwner - The component - owner.}
  constructor Create(AOwner : TComponent); override;
{Frees an instance of the TprTxReport class.}
  destructor Destroy; override;
published
{Specifies the name of the ESC model of printer which is used for converting
between TprTxReport commands and native printer commands.

The huge amount of printers' models exists in the world and the majority is have
specific sets of commands which can be used in a text mode, for example
For matrix printers of firm "EPSON" the command of inclusion italic a font consists of two symbols - #27#52,
For laser printers of firm "Hewlett Packard" - #27#40#115#49#83.
The concept ESC of model of the printer has been entered to provide printing of the
TprTxReport reports on the various printers (with various commands' sets).

The list of supported ESC models is loaded from the TxRo.ini file
at programm startup and stored in the TprTxReportOptions object,
use the TxReportOptions global variable to get access to the loaded instance
of the TprTxReportOptions class.

This property can be empty, in this case the PReport will search the
ESC model automatically.
See also:
  ESCModel, TprESCModel, TprTxReportOptions, TprTxReportOptions.ESCModels, TprTxReportOptions.ESCModelsCount}
  property ESCModelName : string read FESCModelName write SetESCModelName;
{Specifies the name of recode table which is used for text converting between
ANSI character set (used in Windows) and OEM character set (used by the printers in the text mode).

The list of recode tables is loaded from the TxRo.ini file
at programm startup and stored in the TprTxReportOptions object,
use the TxReportOptions global variable to get access to the loaded instance
of the TprTxReportOptions class.

See also:
  RecodeTable, TprRecodeTable, TprTxReportOptions, TprTxReportOptions.RecodeTables, TprTxReportOptions.TxRecodeTablesCount}
  property RecodeTableName : string read FRecodeTableName write SetRecodeTableName;

{Specifies the left margin in chars, used at printing.}
  property LeftSpaces : integer read FLeftSpaces write FLeftSpaces default 0;
{Specifies the number of chars per column, can be used if the report width is more than width of paper on which report is printed.
If this parameter is less than 1 it is ignored.}
  property WrapAfterColumn : integer read FWrapAfterColumn write FWrapAfterColumn default -1;
{Indicates whether new line must be started when string is wrapped or the report
must be printed in few columns.}
  property StartNewLineOnWrap : boolean read FStartNewLineOnWrap write FStartNewLineOnWrap default false;
{Indicates whether the form feed must be done when printing is finished.}
  property EjectPageAfterPrint : boolean read FEjectPageAfterPrint write FEjectPageAfterPrint default true;

{Specifies the paper type - pages or roll.}
  property PaperType: TprTxPaperType read FPaperType write FPaperType default ptPage;

{Indicates whether the pages list must be formed automatically.
Usually this property is used when you want to print the report generated for roll on the separated pages.}
  property UseLinesOnPage : boolean read FUseLinesOnPage write FUseLinesOnPage default false;
{Specifies the number of lines per page if pages list must be formed automatically (used if UseLinesOnPage is true).
Usually this property is used when you want to print the report generated for roll on the separated pages.}
  property LinesOnPage : integer read FLinesOnPage write FLinesOnPage default 60;

{Indicates whether the form feed for each page must be done if roll is used (PaperType equals to ptRulon).}
  property MakeFormFeedOnRulon : boolean read FMakeFormFeedOnRulon write FMakeFormFeedOnRulon default false;
{Specifies the set of lines to printing, used if PaperType equals to ptRulon.
See also:
  TprTxPrintRulonMode}
  property PrintRulonMode : TprTxPrintRulonMode read FPrintRulonMode write FPrintRulonMode default prmAllLines;
{Indicates the line on which the print job is to begin.
This value is used only when the PrintRulonMode property equals to prmLinesRange and PaperType equals to ptRulon.
See also:
  ToLine, PrintRulonMode}
  property FromLine : integer read FFromLine write FFromLine default -1;
{Indicates the page on which the print job is to end.
This value is used only when the PrintRulonMode property equals to prmLinesRange and PaperType equals to ptRulon.
See also:
  FromLine, PrintRulonMode}
  property ToLine : integer read FToLine write FToLine default -1;

{Specifies the options of export.
See also:
  TprTxExportOptionsSet}
  property ExportTxOptions : TprTxExportOptionsSet read FExportTxOptions write FExportTxOptions default [];
{Indicates the line on which the export is to begin.
See also:
  ExportToLine}
  property ExportFromLine : integer read FExportFromLine write FExportFromLine default -1;
{Indicates the line on which the export is to end.
See also:
  ExportFromLine}
  property ExportToLine : integer read FExportToLine write FExportToLine default -1;
{Specifies the name of the ESC model of printer which is used for converting
between TprTxReport commands and native printer commands.
If this property is empty the printer commands are not exported.
See also:
  ESCModelName, TprESCModel}
  property ExportESCModelName : string read FExportESCModelName write SetExportESCModelName;
{Specifies the export code page.
See also:
  TprTxCodePage}
  property ExportCodePage: TprTxCodePage read FExportCodePage write FExportCodePage default prtxcpDOS866;
{Event occurs before printing of a line of the report.
See also:
  TOnPrintLine}
  property OnPrintLine: TprOnPrintLine read FOnPrintLine write FOnPrintLine;
end;

{Creates a fixed font, used in design-time.
Parameters:
  ADpiY - The dpi of device context in which font will be used.
  ExData - Pointer to the rTxExData structure.
Return value:
  Returns the handle of font.}
function CreateTxFont(ADpiY: Integer; ExData: PTxExData): HFONT;
function CreateTxDraftFont(const AFontName: string; AFontSize: Integer): HFONT;

implementation

uses
  pr_Strings, pr_TxPreview, pr_TxUtils, pr_TxExportParams,
  pr_TxExportFilters, pr_TxAddon, vgr_Functions, vgr_GUIFunctions;

type
  TprObjRecAccess = class(TprObjRec)
  end;

function CreateTxDraftFont(const AFontName: string; AFontSize: Integer): HFONT;
var
  ALogFont: TLogFont;
begin
  ZeroMemory(@ALogFont, SizeOf(ALogFont));
  with ALogFont do
  begin
    lfHeight := -MulDiv(AFontSize, Screen.PixelsPerInch, 72);
    lfWeight := FW_NORMAL;
    lfCharSet := OEM_CHARSET;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfQuality := DRAFT_QUALITY;
    lfPitchAndFamily := DEFAULT_PITCH;
    lfItalic := 0;
    lfUnderline := 0;
    lfCharSet := OEM_CHARSET;
    StrPCopy(lfFaceName, AFontName);
  end;
  Result := CreateFontIndirect(ALogFont);
end;

function CreateTxFont(ADpiY: Integer; ExData: PTxExData): HFONT;
var
  AFont: TLogFont;
begin
  FillChar(AFont, SizeOf(AFont), #0);
  with AFont do
  begin
    lfHeight := -MulDiv(ExData.FontSize, ADpiY, 72);
    lfWeight := FW_NORMAL;
    lfCharSet := OEM_CHARSET;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfQuality := DEFAULT_QUALITY;
    lfPitchAndFamily := DEFAULT_PITCH;
    lfItalic := 0;
    lfUnderline := 0;
    lfCharSet := OEM_CHARSET;
    StrPCopy(lfFaceName, ExData.FontName);
  end;
  Result := CreateFontIndirect(AFont);
end;

/////////////////////////
//
// TprTxMemoObjRecVersion
//
/////////////////////////
constructor TprTxMemoObjRecVersion.Create;
begin
  inherited;
  FTxFontOptionsEx := TprTxFontOptions.Create;
  FMemo := TStringList.Create;
  FEolIsEndOfParagraph := True;
end;

destructor TprTxMemoObjRecVersion.Destroy;
begin
  FMemo.Free;
  FTxFontOptionsEx.Free;
  inherited;
end;

procedure TprTxMemoObjRecVersion.Assign;
begin
  with Source as TprTxMemoObjRecVersion do
  begin
    Self.SecondPassNeeded := SecondPassNeeded;
    Self.FhAlign := FhAlign;
    Self.FvAlign := FvAlign;
    Self.FDeleteEmptyLinesAtEnd := FDeleteEmptyLinesAtEnd;
    Self.FDeleteEmptyLines := FDeleteEmptyLines;
    Self.FCanResizeX := FCanResizeX;
    Self.FCanResizeY := FCanResizeY;
    Self.FWordWrap := FWordWrap;
    Self.FDefaultFont := FDefaultFont;
    Self.FTxFontStyleEx := FTxFontStyleEx;
    Self.FJustifyLastLine := FJustifyLastLine;
    Self.FEolIsEndOfParagraph := FEolIsEndOfParagraph;
    Self.FBrdLeft := FBrdLeft;
    Self.FBrdTop := FBrdTop;
    Self.FBrdLeftTop := FBrdLeftTop;
    Self.FBrdRightTop := FBrdRightTop;
    Self.FBrdRight := FBrdRight;
    Self.FBrdRightBottom := FBrdRightBottom;
    Self.FBrdBottom := FBrdBottom;
    Self.FBrdLeftBottom := FBrdLeftBottom;
    Self.FTxFontOptionsEx.Assign(FTxFontOptionsEx);
    Self.FMemo.Assign(FMemo);
  end;
  inherited;
end;

procedure TprTxMemoObjRecVersion.SetTxFontOptionsEx(Value : TprTxFontOptions);
begin
  FTxFontOptionsEx.Assign(Value);
end;

function TprTxMemoObjRecVersion.StoredTxFontOptionsEx : boolean;
begin
  Result := FTxFontOptionsEx.Count>0;
end;

function TprTxMemoObjRecVersion.StoredTxFontStyleEx : boolean;
begin
  Result := FTxFontStyleEx<>nil;
end;

procedure TprTxMemoObjRecVersion.DefineProperties(Filer : TFiler);
begin
  inherited;
  Filer.DefineProperty('TxFontStyle', ReadTxFontStyle, WriteTxFontStyle, False);
  Filer.DefineProperty('TxFontOptions', ReadTxFontOptions, WriteTxFontOptions, False);
  Filer.DefineProperty('TxFontStyleEx', ReadTxFontStyleEx, WriteTxFontStyleEx, StoredTxFontStyleEx);
  Filer.DefineProperty('TxFontOptionsEx', ReadTxFontOptionsEx, WriteTxFontOptionsEx, StoredTxFontOptionsEx);
end;

procedure TprTxMemoObjRecVersion.ReadTxFontStyle(Reader : TReader);
begin
FTxFontStyleEx := TxReportOptions.FindTxFontStyle(Reader.ReadIdent);
end;

procedure TprTxMemoObjRecVersion.WriteTxFontStyle(Writer : TWriter);
begin
end;

procedure TprTxMemoObjRecVersion.ReadTxFontOptions(Reader : TReader);
var
  s : string;
  TxFontOption : TprTxFontOption;
begin
  Reader.ReadValue;
  while true do
  begin
    s := Reader.ReadStr;
    if s='' then break;
    TxFontOption := TxReportOptions.FindTxFontOption(s);
    if TxFontOption<>nil then
      FTxFontOptionsEx.Add(TxFontOption);
  end;
end;

procedure TprTxMemoObjRecVersion.WriteTxFontOptions(Writer : TWriter);
begin
end;

procedure TprTxMemoObjRecVersion.ReadTxFontStyleEx(Reader : TReader);
begin
FTxFontStyleEx := TxReportOptions.FindTxFontStyle(Reader.ReadIdent);
end;

procedure TprTxMemoObjRecVersion.WriteTxFontStyleEx(Writer : TWriter);
begin
if FTxFontStyleEx<>nil then
  Writer.WriteIdent(FTxFontStyleEx.Name);
end;

procedure TprTxMemoObjRecVersion.ReadTxFontOptionsEx(Reader : TReader);
var
  TxFontOption : TprTxFontOption;
begin
Reader.ReadListBegin;
while not Reader.EndOfList do
  begin
    TxFontOption := TxReportOptions.FindTxFontOption(Reader.ReadIdent);
    if TxFontOption<>nil then
      FTxFontOptionsEx.Add(TxFontOption);
  end;
Reader.ReadListEnd;
end;

procedure TprTxMemoObjRecVersion.WriteTxFontOptionsEx(Writer : TWriter);
var
  i : integer;
begin
Writer.WriteListBegin;
for i:=0 to FTxFontOptionsEx.Count-1 do
  Writer.WriteIdent(FTxFontOptionsEx[i].Name);
Writer.WriteListEnd;
end;

procedure TprTxMemoObjRecVersion.CalcInnerRect(const AWholeRect: TRect; var AInnerRect: TRect);
begin
  AInnerRect := AWholeRect;
  if BrdLeft <> #0 then
    Inc(AInnerRect.Left);
  if BrdTop <> #0 then
    Inc(AInnerRect.Top);
  if BrdRight <> #0 then
    Dec(AInnerRect.Right);
  if BrdBottom <> #0 then
    Dec(AInnerRect.Bottom);
end;

procedure TprTxMemoObjRecVersion.BuildBorders(AWidth, AHeight: Integer; var ALeft, ATop, ARight, ABottom: string; var ATopOffs, ABottomOffs: Integer);
begin
  ALeft := '';
  ATop := '';
  ARight := '';
  ABottom := '';
  ATopOffs := 0;
  ABottomOffs := 0;

  if AWidth = 1 then
  begin
    if BrdLeft <> #0 then
    begin
      ALeft := MakeStr(BrdLeft, AHeight);
      exit;
    end
    else
      if BrdRight <> #0 then
      begin
        ALeft := MakeStr(BrdRight, AHeight);
        exit;
      end;
  end;

  if AHeight = 1 then
  begin
    if BrdTop <> #0 then
    begin
      ATop := MakeStr(BrdTop, AWidth);
      exit;
    end
    else
      if BrdBottom <> #0 then
      begin
        ATop := MakeStr(BrdBottom, AWidth);
        exit;
      end;
  end;

  if BrdLeft <> #0 then
  begin
    if (BrdTop = #0) or (BrdLeftTop = #0) then
      ALeft := BrdLeft
    else
      ALeft := BrdLeftTop;
    ALeft := ALeft + MakeStr(BrdLeft, AHeight - 2);
    if (BrdBottom = #0) or (BrdLeftBottom = #0) then
      ALeft := ALeft + BrdLeft
    else
      ALeft := ALeft + BrdLeftBottom;
  end;

  if BrdRight <> #0 then
  begin
    if (BrdTop = #0) or (BrdRightTop = #0) then
      ARight := BrdRight
    else
      ARight := BrdRightTop;
    ARight := ARight + MakeStr(BrdRight, AHeight - 2);
    if (BrdBottom = #0) or (BrdRightBottom = #0) then
      ARight := ARight + BrdRight
    else
      ARight := ARight + BrdRightBottom;
  end;

  if BrdTop <> #0 then
  begin
    ATopOffs := 0;
    if BrdLeft = #0 then
      ATop := BrdTop
    else
    begin
      Inc(ATopOffs);
      ATop := '';
    end;
    ATop := ATop + MakeStr(BrdTop, AWidth - 2);
    if BrdRight = #0 then
      ATop := ATop + BrdTop;
  end;

  if BrdBottom <> #0 then
  begin
    ABottomOffs := 0;
    if BrdLeft = #0 then
      ABottom := BrdBottom
    else
    begin
      Inc(ABottomOffs);
      ABottom := '';
    end;
    ABottom := ABottom + MakeStr(BrdBottom, AWidth - 2);
    if BrdRight = #0 then
      ABottom := ABottom + BrdBottom;
  end;
end;

/////////////////////////////////////////////////
//
// TprTxMemoObjRec
//
/////////////////////////////////////////////////
function TprTxMemoObjRec.GetVersionClass: TprObjVersionClass;
begin
  Result := TprTxMemoObjRecVersion;
end;

function TprTxMemoObjRec.GetSupportSplitting: Boolean;
begin
  Result := True;
end;

function TprTxMemoObjRec.GetCanSplitValue: Boolean;
begin
  Result := FCanSplit;
end;

procedure TprTxMemoObjRec.SetCanSplitValue(Value: Boolean);
begin
  FCanSplit := Value;
end;

function TprTxMemoObjRec.GetCanSplit(AByHorizontal: Boolean; ASplitPos: Integer): Boolean;
begin
  // if object has a top border then it can not be split at 1 position
  // if object has a bottom border then it can not be split at Height - 1 position
  // if object has the top and bottom borders then it can be split if ASplitPos > 2 and ASplitPos < Height - 2
  Result := not AByHorizontal and FCanSplit;
  if Result then
    with TprTxMemoObjRecVersion(Versions[CurVersion]) do
      Result := ASplitPos > (Integer(BrdTop <> #0) + Integer(BrdBottom <> #0));
end;

function TprTxMemoObjRec.Split(AByHorizontal: Boolean; ASplitPos: Integer; var AAddToSplitted: Integer): TprObjRec;
var
  V: TprTxMemoObjRecVersion;
  I, ALineCount, AWrapTextSize: Integer;
  AText: string;
  ALines: TvgrWrapLineDynArray;
  ABrdLeft, ABrdTop, ABrdRight, AbrdBottom: Integer;

  procedure _Split(var ANewMin, ANewMax, AOldMin, AOldMax, AOldLineCount: Integer;
                   ACanResize: Boolean;
                   AMinBorderSize: Integer;
                   AMaxBorderSize: Integer);
  var
    I, ANewLineCount, ANewHeight: Integer;
  begin
    AAddToSplitted := 0;
    AOldMax := AOldMin + ASplitPos;

    if (v.vAlign = prvTop) or
       v.CanResizeY or
       (FpRect.Bottom - FpRect.Top - AMinBorderSize - AMaxBorderSize < Length(ALines)) then
    begin
      // top align
      AOldLineCount := Min(Length(ALines), ASplitPos - AMinBorderSize - AMaxBorderSize);
      ANewLineCount := Max(0, Length(ALines) - AOldLineCount);

      if ACanResize and (ANewLineCount > 0) then
        ANewHeight := ANewLineCount + AMinBorderSize + AMaxBorderSize
      else
        ANewHeight := ANewMax - ANewMin - ASplitPos + AMinBorderSize + AMaxBorderSize;

      AAddToSplitted := Max(0, ANewHeight - (ANewMax - ANewMin - ASplitPos));
    end
    else
    begin
      if v.vAlign = prvCenter then
      begin
        // center align
        AOldLineCount := Max(0, ASplitPos -
                                AMinBorderSize -
                                AMaxBorderSize -
                                (FpRect.Bottom - FpRect.Top - AMinBorderSize - AMaxBorderSize - Length(ALines)) div 2);

        ANewHeight := ANewMax - ANewMin  - ASplitPos + AMinBorderSize + AMaxBorderSize;
        AAddToSplitted := Max(0, ANewHeight - (ANewMax - ANewMin - ASplitPos));

        // change alignment
        for I := 0 to Result.Versions.Count - 1 do
          TprTxMemoObjRecVersion(Result.Versions[I]).vAlign := prvBottom;

        for I := 0 to Versions.Count - 1 do
          TprTxMemoObjRecVersion(Versions[I]).vAlign := prvTop;
      end
      else
      begin
        // bottom align
        AOldLineCount := Max(0, ASplitPos -
                                (FpRect.Bottom -
                                 FpRect.Top -
                                 Length(ALines)));

        ANewHeight := ANewMax - ANewMin  - ASplitPos + AMinBorderSize + AMaxBorderSize;
        AAddToSplitted := Max(0, ANewHeight - (ANewMax - ANewMin - ASplitPos));
      end;
    end;

    ANewMin := 0;
    ANewMax := ANewMin + ANewHeight;
  end;

begin
  Result := CreateCopy;

  V := TprTxMemoObjRecVersion(Versions[CurVersion]);

  ABrdLeft := Integer(V.BrdLeft <> #0);
  ABrdTop := Integer(V.BrdTop <> #0);
  ABrdRight := Integer(V.BrdRight <> #0);
  ABrdBottom := Integer(V.BrdBottom <> #0);

  AText := V.Memo.Text;
  AWrapTextSize := TprObjRecAccess(Result).FpRect.Right - TprObjRecAccess(Result).FpRect.Left - ABrdLeft - ABrdRight;
  WrapMemoChars(AText,
                AWrapTextSize,
                ALines,
                V.WordWrap,
                V.DeleteEmptyLines,
                V.DeleteEmptyLinesAtEnd); 

  _Split(FpRect.Top,
         FpRect.Bottom,
         TprObjRecAccess(Result).FpRect.Top,
         TprObjRecAccess(Result).FpRect.Bottom,
         ALineCount,
         V.CanResizeY,
         ABrdTop,
         ABrdBottom);

  // delete lines from below version
  for I := 0 to Versions.Count - 1 do
  begin
    V := TprTxMemoObjRecVersion(Versions[I]);

    AText := V.Memo.Text;
    WrapMemoChars(AText, AWrapTextSize, ALines, V.WordWrap, V.DeleteEmptyLines, V.DeleteEmptyLinesAtEnd);
    if Length(ALines) > ALineCount then
      V.Memo.Text := Copy(AText, ALines[ALineCount].Start, MaxInt)
    else
      V.Memo.Clear;
  end;

  // delete lines from above version
  if ALineCount >= 0 then
  begin
    for I := 0 to Result.Versions.Count - 1 do
    begin
      V := TprTxMemoObjRecVersion(Result.Versions[I]);

      AText := V.Memo.Text;
      WrapMemoChars(AText, AWrapTextSize, ALines, V.WordWrap, V.DeleteEmptyLines, V.DeleteEmptyLinesAtEnd);
      if (Length(ALines) >= ALineCount) and (ALineCount > 0) then
        V.Memo.Text := Copy(AText, 1, ALines[ALineCount - 1].Start + ALines[ALineCount - 1].Length - 1)
      else
        V.Memo.Clear;
    end;
  end;
end;

procedure TprTxMemoObjRec.SecondPass;
var
  I: Integer;
  V: TprTxMemoObjRecVersion;
begin
  inherited;

  for I := 0 to Versions.Count - 1 do
    if TprTxMemoObjRecVersion(Versions[I]).SecondPassNeeded then
    begin
      V := TprTxMemoObjRecVersion(Versions[I]);
      Container.FormatStrings(V.Memo,
                              V.Memo,
                              v.DeleteEmptyLines,
                              v.DeleteEmptyLinesAtEnd);
    end;
end;

procedure TprTxMemoObjRec.ThirdPass;
var
  V: TprTxMemoObjRecVersion;
begin
  if not Versions[CurVersion].Visible then exit;

  V := TprTxMemoObjRecVersion(Versions[CurVersion]);
  TprTextDevice(Device).PlaceTxMemo(V, r, PreviewUserData);
  FPreviewUserData := nil;
end;

//////////////////////////////
//
// TprTxMemoObj
//
//////////////////////////////
function TprTxMemoObj.GetVersion(Index: Integer): TprTxMemoObjRecVersion;
begin
  Result := TprTxMemoObjRecVersion(inherited Versions[Index]);
end;

function TprTxMemoObj.GetGenVersion(Index: Integer): TprTxMemoObjRecVersion;
begin
  Result := TprTxMemoObjRecVersion(inherited GenVersions[Index]);
end;

function TprTxMemoObj.GetDefVersion: TprTxMemoObjRecVersion;
begin
  Result := TprTxMemoObjRecVersion(inherited DefVersion);
end;

function TprTxMemoObj.GetGenCurVersion: TprTxMemoObjRecVersion;
begin
  Result := TprTxMemoObjRecVersion(inherited GenCurversion);
end;

function TprTxMemoObj.DsgnAllowInplaceEdit : boolean;
begin
Result := true;
end;

procedure TprTxMemoObj.OnDsgnPopupMenuFontStyleClick(Sender : TObject);
begin
with TprTxMemoObjRecVersion(DefVersion) do
  TxFontStyleEx := TxReportOptions.TxFontStyles[TMenuItem(Sender).Tag];
DsgnNotifyDesigner;
end;

procedure TprTxMemoObj.OnDsgnPopupMenuFontOptionClick(Sender : TObject);
begin
with TprTxMemoObjRecVersion(DefVersion) do
  if TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[TMenuItem(Sender).Tag]) then
    TxFontOptionsEx.Remove(TxReportOptions.TxFontOptions[TMenuItem(Sender).Tag])
  else
    TxFontOptionsEx.Add(TxReportOptions.TxFontOptions[TMenuItem(Sender).Tag]);
DsgnNotifyDesigner;
end;

procedure TprTxMemoObj.OnDsgnPopupMenuClick(Sender : TObject);
begin
with TprTxMemoObjRecVersion(DefVersion) do
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
    -12: DefaultFont := not DefaultFont;
    -13: hAlign := prhJustify;
  end;
DsgnNotifyDesigner;
end;

procedure TprTxMemoObj.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
var
  i : integer;
  m : TMenuItem;
begin
inherited;
if Popup.Items.Count>0 then
  AddPopupMenuItem(Popup,nil,0,'',nil,'',0,false,false);
m := AddPopupMenuItem(Popup,nil,sHorizontalAlignment,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sHorizontalAlignLeft,'HLEFT',OnDsgnPopupMenuClick,'',-1,true,TprTxMemoObjRecVersion(DefVersion).hAlign=prhLeft);
AddPopupMenuItem(Popup,m,sHorizontalAlignCenter,'HCENTER',OnDsgnPopupMenuClick,'',-2,true,TprTxMemoObjRecVersion(DefVersion).hAlign=prhCenter);
AddPopupMenuItem(Popup,m,sHorizontalAlignRight,'HRIGHT',OnDsgnPopupMenuClick,'',-3,true,TprTxMemoObjRecVersion(DefVersion).hAlign=prhRight);
AddPopupMenuItem(Popup,m,sTextAlignJustify,'HJUSTIFY',OnDsgnPopupMenuClick,'',-13,true,TprTxMemoObjRecVersion(DefVersion).hAlign=prhJustify);

m := AddPopupMenuItem(Popup,nil,sVerticalAlignment,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sVerticalAlignTop,'VTOP',OnDsgnPopupMenuClick,'',-4,true,TprTxMemoObjRecVersion(DefVersion).vAlign=prvTop);
AddPopupMenuItem(Popup,m,sVerticalAlignCenter,'VCENTER',OnDsgnPopupMenuClick,'',-5,true,TprTxMemoObjRecVersion(DefVersion).vAlign=prvCenter);
AddPopupMenuItem(Popup,m,sVerticalAlignBottom,'VBOTTOM',OnDsgnPopupMenuClick,'',-6,true,TprTxMemoObjRecVersion(DefVersion).vAlign=prvBottom);

AddPopupMenuItem(Popup,nil,0,'',nil,'',0,false,false);

AddPopupMenuItem(Popup,nil,sTxMemoObjDefaultFont,'',OnDsgnPopupMenuClick,'',-7,true,TprTxMemoObjRecVersion(DefVersion).DefaultFont);
m := AddPopupMenuItem(Popup,nil,sTxMemoObjFontStyle,'',nil,'',0,true,false);
for i:=0 to TxReportOptions.TxFontStylesCount-1 do
  AddPopupMenuItemS(Popup,
                    m,
                    TxReportOptions.TxFontStyles[i].Description,
                    '',
                    OnDsgnPopupMenuFontStyleClick,
                    '',
                    i,
                    true,
                    (TprTxMemoObjRecVersion(DefVersion).TxFontStyleEx<>nil) and
                    (TprTxMemoObjRecVersion(DefVersion).TxFontStyleEx.Name=TxReportOptions.TxFontStyles[i].Name));

m := AddPopupMenuItem(Popup,nil,sTxMemoObjFontOptions,'',nil,'',0,true,false);
for i:=0 to TxReportOptions.TxFontOptionsCount-1 do
  AddPopupMenuItemS(Popup,
                    m,
                    TxReportOptions.TxFontOptions[i].Description,
                    '',
                    OnDsgnPopupMenuFontOptionClick,
                    '',
                    i,
                    true,
                    TprTxMemoObjRecVersion(DefVersion).TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[i]));

m := AddPopupMenuItem(Popup,nil,sAddititional,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sDeleteEmptyLines,'',OnDsgnPopupMenuClick,'',-7,true,TprTxMemoObjRecVersion(DefVersion).DeleteEmptyLines);
AddPopupMenuItem(Popup,m,sDeleteEmptyLinesAtEnd,'',OnDsgnPopupMenuClick,'',-8,true,TprTxMemoObjRecVersion(DefVersion).DeleteEmptyLinesAtEnd);
AddPopupMenuItem(Popup,m,sResizeHorizontally,'',OnDsgnPopupMenuClick,'',-9,true,TprTxMemoObjRecVersion(DefVersion).CanResizeX);
AddPopupMenuItem(Popup,m,sResizeVertically,'',OnDsgnPopupMenuClick,'',-10,true,TprTxMemoObjRecVersion(DefVersion).CanResizeY);
AddPopupMenuItem(Popup,m,sWordWrap,'',OnDsgnPopupMenuClick,'',-11,true,TprTxMemoObjRecVersion(DefVersion).WordWrap);
end;

procedure TprTxMemoObj.InplaceEdit(_Parent : TWinControl; var InplaceEditor : TWinControl; const InplaceRect : TRect; ExData : pointer);
var
  r : TRect;
  v : TprTxMemoObjRecVersion;
  s : string;
  i,l : integer;
begin
v := TprTxMemoObjRecVersion(dRec.Versions[dRec.DefVersion]);
InplaceEditor := TprTxOEMMemo.Create(_Parent);

with TprTxOEMMemo(InplaceEditor) do
  begin
    Report := TprTxReport(Self.Report);
    WordWrap := false;
    ParentCtl3D := False;
    Ctl3D := False;
    TabStop := False;
    BorderStyle := bsNone;
    DoubleBuffered := False;
    Left := InplaceRect.Left;
    Top := InplaceRect.Top-1;
    Width := InplaceRect.Right-InplaceRect.Left;
    Height := InplaceRect.Bottom-InplaceRect.Top+2;
    Parent := _Parent;
    Font.Size := PTxExData(ExData).FontSize;

    Lines.BeginUpdate;
    try
      Lines.Clear;
      for i:=0 to v.Memo.Count-1 do
        begin
          l := Length(v.Memo[i]);
          SetLength(s,l);
          MoveMemory(@(s[1]),@(v.Memo[i][1]),l);
          TprTxReport(Report).WinToOem(PChar(s),PChar(s));
          Lines.Add(s);
        end;
    finally
      Lines.EndUpdate;
    end;

    r:=Rect(0,0,Width,Height);
    SendMessage(InplaceEditor.Handle,EM_SETRECT,0,LongInt(@R));
    SendMessage(InplaceEditor.Handle,EM_SETSEL,0,0);

    Show;
    SetFocus;
  end;
end;

procedure TprTxMemoObj.SaveInplaceEdit;
var
  i : integer;
  v : TprTxMemoObjRecVersion;
begin
v:=TprTxMemoObjRecVersion(dRec.Versions[dRec.DefVersion]);
with TprTxOEMMemo(InplaceEditor) do
  begin
    v.Memo.Clear;
    for i:=0 to Lines.Count-1 do
      begin
        v.Memo.Add(Lines[i]);
        TprTxReport(Report).OemToWin(PChar(v.Memo[i]),PChar(v.Memo[i]));
      end;
  end;
end;

procedure TprTxMemoObj.InitdRec;
begin
FdRec := TprTxMemoObjRec.Create(nil,Self);
dRec.Versions.Add;
end;

function TprTxMemoObj.GetDesc;
var
  i : integer;
  v : TprTxMemoObjRecVersion;
begin
v:=TprTxMemoObjRecVersion(dRec.Versions[dRec.DefVersion]);
i:=0;
while (i<v.Memo.Count) and
      (Trim(v.Memo[i])='') do Inc(i);
if i<v.Memo.Count then
  Result:=Trim(v.Memo[i])
else
  Result:=inherited GetDesc;
end;

procedure TprTxMemoObj.DrawBorder(DC: HDC; const APixelRect: TRect; ACharWidth, ACharHeight: Integer; v: TprTxMemoObjRecVersion; AExData: PTxExData);
var
  ALeft, ATop, ARight, ABottom: string;
  ATopOffs, ABottomOffs: Integer;

  procedure DrawHorz(X, Y: Integer; const s: string);
  begin
    ExtTextOut(DC,
               APixelRect.Left + X * AExData.SymbolSize.cx,
               APixelRect.Top + Y * AExData.SymbolSize.cy,
               ETO_CLIPPED,
               @APixelRect,
               PChar(S),
               Length(S),
               nil);
  end;

  procedure DrawVert(X, Y: Integer; const s: string);
  var
    I: Integer;
  begin
    for I := Y to Y + Length(s) - 1 do
    begin
      ExtTextOut(DC,
                 APixelRect.Left + X * AExData.SymbolSize.cx,
                 APixelRect.Top + I * AExData.SymbolSize.cy,
                 ETO_CLIPPED,
                 @APixelRect,
                 @(S[I - Y + 1]),
                 1,
                 nil);
    end;
  end;
  
begin
  if (ACharHeight <= 0) or (ACharWidth <= 0) then
    exit;

  v.BuildBorders(ACharWidth, ACharHeight, ALeft, ATop, ARight, ABottom, ATopOffs, ABottomOffs);

  if ALeft <> '' then
    DrawVert(0, 0, ALeft);

  if ARight <> '' then
    DrawVert(ACharWidth - 1, 0, ARight);

  if ATop <> '' then
    DrawHorz(ATopOffs, 0, ATop);

  if ABottom <> '' then
    DrawHorz(ABottomOffs, ACharHeight - 1, ABottom);
end;

procedure TprTxMemoObj.DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect);
var
  s: string;
  v: TprTxMemoObjRecVersion;
  CalcH: integer;
  nfn,ofn: HFONT;
  i,l,CurY,CurX: integer;

  AInnerRect: TRect;
begin
  v := TprTxMemoObjRecVersion(DefVersion);

  pr_Common.DrawRect(DC, DrawRect, clGray, clWhite);

  // select font
  nfn := CreateTxFont(GetDeviceCaps(DC, LOGPIXELSY), PTxExData(ExData));
  ofn := SelectObject(DC,nfn);
  SetBkMode(DC,TRANSPARENT);
  try
    // draw a border
    v.CalcInnerRect(Rect(0, 0, dRec.pRect.Right - dRec.pRect.Left, dRec.pRect.Bottom - dRec.pRect.Top), AInnerRect);
    DrawBorder(DC, DrawRect, dRec.pRect.Right - dRec.pRect.Left, dRec.pRect.Bottom - dRec.pRect.Top, v, PTxExData(ExData));

    if (AInnerRect.Bottom - AInnerRect.Top > 0) and (AInnerRect.Right - AInnerRect.Left > 0) then
    begin
      CalcH := v.Memo.Count;
      case v.vAlign of
        prvCenter:
          if AInnerRect.Bottom - AInnerRect.Top < CalcH then
            CurY := 0
          else
            CurY := (AInnerRect.Bottom - AInnerRect.Top - CalcH) div 2;

        prvBottom:
          if AInnerRect.Bottom - AInnerRect.Top < CalcH then
            CurY := 0
          else
            CurY := AInnerRect.Bottom - AInnerRect.Top - CalcH

        else
          CurY := 0;
      end;
      CurY := CurY + AInnerRect.Top;

      for I := 0 to Math.Min(AInnerRect.Bottom - AInnerRect.Top - 1, v.Memo.Count - 1) do
      begin
        TprTxReport(Report).WinToOem(v.Memo[I], s);
        L := Length(s);

        case v.hAlign of
          prhCenter:
            if AInnerRect.Right - AInnerRect.Left < l then
              CurX := 0
            else
              CurX := (AInnerRect.Right - AInnerRect.Left - l) div 2;

          prhRight:
            if AInnerRect.Right - AInnerRect.Left < l then
              CurX := 0
            else
              CurX := AInnerRect.Right - AInnerRect.Left - l

          else
            CurX := 0;
        end;
        CurX := CurX + AInnerRect.Left;

        ExtTextOut(DC,
                   DrawRect.Left + CurX * PTxExData(ExData).SymbolSize.cx,
                   DrawRect.Top + CurY * PTxExData(ExData).SymbolSize.cy,
                   ETO_CLIPPED,
                   @DrawRect,
                   PChar(s),
                   Min(L, AInnerRect.Right - AInnerRect.Left),
                   nil);
        Inc(CurY);
      end;
    end;
  finally
      SetBkMode(DC,OPAQUE);
    SelectObject(DC,ofn);
    DeleteObject(nfn);
  end;
  inherited;
end;

procedure TprTxMemoObj.FirstPass;
var
  V: TprTxMemoObjRecVersion;
  I, AMaxWidth: Integer;
  ManuallyProcessed: Boolean;
  AText: string;
  ALines: TvgrWrapLineDynArray;
  AWrapSize: Integer;
begin
  if FaRec = nil then
    FaRec := TprTxMemoObjRec.Create(nil,Self);
  aRec.Assign(dRec);

  // select version
  aRec.FirstPass;

  // OnFirstPassObject
  DoOnFirstPassObject(ManuallyProcessed);

  // generate aRec.Memo
  if not ManuallyProcessed then
    for i:=0 to aRec.Versions.Count-1 do
      with TprTxMemoObjRecVersion(aRec.Versions[i]) do
        SecondPassNeeded := not Band.Report.FormatStrings(Memo,
                                                          Memo,
                                                          DeleteEmptyLines,
                                                          DeleteEmptyLinesAtEnd);

  V := TprTxMemoObjRecVersion(aRec.Versions[aRec.CurVersion]);

  aRec.pRect := dRec.pRect;

  AText := V.Memo.Text;
  AWrapSize := TprTxMemoObjRec(aRec).Width;
  if v.BrdLeft <> #0 then
    Dec(AWrapSize);
  if v.BrdRight <> #0 then
    Dec(AWrapSize);
  if (AWrapSize > 0) or (not V.WordWrap) then
  begin
    WrapMemoChars(AText,
                  AWrapSize,
                  ALines,
                  V.WordWrap,
                  V.DeleteEmptyLines,
                  V.DeleteEmptyLinesAtEnd);
    if V.CanResizeY then
    begin
      TprTxMemoObjRec(aRec).FpRect.Bottom := TprTxMemoObjRec(aRec).FpRect.Top + Length(ALines);
      if v.BrdTop <> #0 then
        Inc(TprTxMemoObjRec(aRec).FpRect.Bottom);
      if v.BrdBottom <> #0 then
        Inc(TprTxMemoObjRec(aRec).FpRect.Bottom);
    end;

    // calculate DX and DY
    if not V.WordWrap and V.CanResizeX then
    begin
      // DX = Length of the longest string in Memo
      AMaxWidth := 0;
      for I := 0 to High(ALines) do
        if ALines[I].Length > AMaxWidth then
          AMaxWidth := ALines[I].Length;
      TprTxMemoObjRec(aRec).FpRect.Right := TprTxMemoObjRec(aRec).FpRect.Left + AMaxWidth;

      if v.BrdLeft <> #0 then
        Inc(TprTxMemoObjRec(aRec).FpRect.Right);
      if v.BrdRight <> #0 then
        Inc(TprTxMemoObjRec(aRec).FpRect.Right);
    end;
  end;

  inherited;

{$IFNDEF PR_OLD_ALIGN_MODE}
  if v.CanResizeX then
    with TprTxMemoObjRec(aRec) do
      case v.hAlign of
        prhRight: OffsetRect(FpRect, dRec.Width - Width, 0);
        prhCenter: OffsetRect(FpRect, (dRec.Width - Width) div 2, 0);
      end;
{$ENDIF}
end;

/////////////////////////////////////////////////
//
// TprTxCommandObjRecVersion
//
/////////////////////////////////////////////////
constructor TprTxCommandObjRecVersion.Create(Collection : TCollection);
begin
inherited Create(Collection);
FTxCommands := TList.Create;
end;

destructor TprTxCommandObjRecVersion.Destroy;
begin
FTxCommands.Free;
inherited Destroy;
end;

procedure TprTxCommandObjRecVersion.DefineProperties(Filer : TFiler);
begin
inherited;
Filer.DefineProperty('TxCommands',ReadTxCommands,WriteTxCommands,StoredTxCommands);
end;

procedure TprTxCommandObjRecVersion.ReadTxCommands(Reader : TReader);
var
  TxCommand : TprTxCommand;
begin
Reader.ReadListBegin;
while not Reader.EndOfList do
  begin
    TxCommand := TxReportOptions.FindTxCommand(Reader.ReadIdent);
    if TxCommand<>nil then
      FTxCommands.Add(TxCommand);
  end;
Reader.ReadListEnd;
end;

procedure TprTxCommandObjRecVersion.WriteTxCommands(Writer : TWriter);
var
  i : integer;
begin
Writer.WriteListBegin;
for i:=0 to FTxCommands.Count-1 do
  Writer.WriteIdent(TxCommands[i].Name);
Writer.WriteListEnd;
end;

function TprTxCommandObjRecVersion.StoredTxCommands : boolean;
begin
Result := FTxCommands.Count>0;
end;

function TprTxCommandObjRecVersion.GetTxCommand(i : integer) : TprTxCommand;
begin
Result := TprTxCommand(FTxCommands[i]);
end;

function TprTxCommandObjRecVersion.GetTxCommandsCount : integer;
begin
Result := FTxCommands.Count;
end;

procedure TprTxCommandObjRecVersion.Assign(Source : TPersistent);
var
  i : integer;
begin
with Source as TprTxCommandObjRecVersion do
  begin
    Self.FTxCommands.Clear;
    for i:=0 to TxCommandsCount-1 do
      Self.FTxCommands.Add(TxCommands[i]);
  end;
end;

procedure TprTxCommandObjRecVersion.ClearTxCommands;
begin
  FTxCommands.Clear;
end;

procedure TprTxCommandObjRecVersion.AddTxCommand(TxCommand: TprTxCommand);
begin
  if FTxCommands.IndexOf(TxCommand) = -1 then
    FTxCommands.Add(TxCommand);
end;

procedure TprTxCommandObjRecVersion.AddTxCommand(const TxCommandName: string);
var
  ACommand: TprTxCommand;
begin
  ACommand := TxReportOptions.FindTxCommand(TxCommandName);
  if FTxCommands.IndexOf(ACommand) = -1 then
    FTxCommands.Add(ACommand);
end;

procedure TprTxCommandObjRecVersion.RemoveTxCommand(TxCommand: TprTxCommand);
begin
  FTxCommands.Remove(TxCommand);
end;

procedure TprTxCommandObjRecVersion.RemoveTxCommand(const TxCommandName: string);
begin
  FTxCommands.Remove(TxReportOptions.FindTxCommand(TxCommandName));
end;

/////////////////////////////////////////////////
//
// TprTxCommandObjRec
//
/////////////////////////////////////////////////
function TprTxCommandObjRec.GetVersionClass: TprObjVersionClass;
begin
  Result := TprTxCommandObjRecVersion;
end;

procedure TprTxCommandObjRec.SecondPass;
begin
inherited;
end;

procedure TprTxCommandObjRec.ThirdPass;
var
  v : TprTxCommandObjRecVersion;
begin
if not Versions[CurVersion].Visible then exit;

v := TprTxCommandObjRecVersion(Versions[CurVersion]);
TprTextDevice(Device).PlaceTxCommand(v,r,PreviewUserData);
FPreviewUserData := nil;
end;

/////////////////////////////////////////////////
//
// TprTxCommandObj
//
/////////////////////////////////////////////////
function TprTxCommandObj.GetVersion(Index: Integer): TprTxCommandObjRecVersion;
begin
  Result := TprTxCommandObjRecVersion(inherited Versions[Index]);
end;

function TprTxCommandObj.GetGenVersion(Index: Integer): TprTxCommandObjRecVersion;
begin
  Result := TprTxCommandObjRecVersion(inherited GenVersions[Index]);
end;

function TprTxCommandObj.GetDefVersion: TprTxCommandObjRecVersion;
begin
  Result := TprTxCommandObjRecVersion(inherited DefVersion);
end;

function TprTxCommandObj.GetGenCurVersion: TprTxCommandObjRecVersion;
begin
  Result := TprTxCommandObjRecVersion(inherited GenCurversion);
end;

procedure TprTxCommandObj.InitdRec;
begin
FdRec := TprTxCommandObjRec.Create(nil,Self);
dRec.Versions.Add;
end;

function TprTxCommandObj.GetDesc;
var
  i : integer;
  v : TprTxCommandObjRecVersion;
begin
Result := '';
v := TprTxCommandObjRecVersion(dRec.Versions[dRec.DefVersion]);
for i:=0 to v.TxCommandsCount-1 do
  Result := Result+v.TxCommands[i].Description+'; ';
Delete(Result,Length(Result)-1,2);
if Result='' then
  Result := inherited GetDesc;
end;

procedure TprTxCommandObj.DrawDesign(DC : HDC; ExData : pointer; const DrawRect : TRect);
var
  r : TRect;
  nbr : HBRUSH;
  opn,npn : HPEN;
begin
r := DrawRect;
npn := CreatePen(PS_SOLID,1,GetRGBColor(clWindowFrame));
opn := SelectObject(DC,npn);
nbr := CreateSolidBrush(GetRGBColor(clBtnFace));
FillRect(DC,r,nbr);
pr_Common.DrawRect(DC,r);
SelectObject(DC,opn);
DeleteObject(npn);
DeleteObject(nbr);

MoveToEx(DC,r.Left+1,r.Top+1,nil);
LineTo(DC,r.Left+5,r.Top+1);
MoveToEx(DC,r.Left+1,r.Top+1,nil);
LineTo(DC,r.Left+1,r.Top+5);
MoveToEx(DC,r.Left+2,r.Top+2,nil);
LineTo(DC,r.Left+2,r.Top+4);
MoveToEx(DC,r.Left+2,r.Top+2,nil);
LineTo(DC,r.Left+4,r.Top+2);

inherited;
end;

procedure TprTxCommandObj.FirstPass;
var
  ManuallyProcessed : boolean;
begin
if FaRec=nil then
  FaRec := TprTxCommandObjRec.Create(nil,Self);
aRec.Assign(dRec);

// select version
aRec.FirstPass;

// OnFirstPassObject
ManuallyProcessed := false;
DoOnFirstPassObject(ManuallyProcessed);

// generate aRec.Memo
aRec.pRect := dRec.pRect;

inherited;
end;





procedure TprTxHBand_DrawDesign(Band : TprCustomHBand; DC : HDC; ExData : pointer; const DrawRect : TRect; ColCount : integer);
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
  SelectObject(DC,ofn);
  SelectObject(DC,opn);
  DeleteObject(nfn);
  DeleteObject(npn);
end;
end;

//////////////////////////
//
// TprTxHTitleBand
//
//////////////////////////
procedure TprTxHTitleBand.DrawDesign;
begin
TprTxHBand_DrawDesign(Self,DC,ExData,DrawRect,0);
end;

/////////////////////////
//
// TprTxHSummaryBand
//
/////////////////////////
procedure TprTxHSummaryBand.DrawDesign;
begin
TprTxHBand_DrawDesign(Self,DC,ExData,DrawRect,0);
end;

//////////////////////////
//
// TprTxHPageHeaderBand
//
//////////////////////////
procedure TprTxHPageHeaderBand.DrawDesign;
begin
TprTxHBand_DrawDesign(Self,DC,ExData,DrawRect,0);
end;

/////////////////////////////
//
// TprTxHPageFooterBand
//
/////////////////////////////
procedure TprTxHPageFooterBand.DrawDesign;
begin
TprTxHBand_DrawDesign(Self,DC,ExData,DrawRect,0);
end;

//////////////////////////
//
// TprTxHDetailBand
//
//////////////////////////
procedure TprTxHDetailBand.DrawDesign;
begin
TprTxHBand_DrawDesign(Self,DC,ExData,DrawRect,ColCount);
end;

////////////////////////////////
//
// TprTxHDetailHeaderBand
//
////////////////////////////////
procedure TprTxHDetailHeaderBand.DrawDesign;
begin
TprTxHBand_DrawDesign(Self,DC,ExData,DrawRect,ColCount);
end;

////////////////////////////////
//
// TprTxHDetailFooterBand
//
////////////////////////////////
procedure TprTxHDetailFooterBand.DrawDesign;
begin
TprTxHBand_DrawDesign(Self,DC,ExData,DrawRect,ColCount);
end;

////////////////////////////////////
//
// TprTxHGroupHeaderBand
//
////////////////////////////////////
procedure TprTxHGroupHeaderBand.DrawDesign;
begin
TprTxHBand_DrawDesign(Self,DC,ExData,DrawRect,ColCount);
end;

////////////////////////////////////
//
// TprTxHGroupFooterBand
//
////////////////////////////////////
procedure TprTxHGroupFooterBand.DrawDesign;
begin
TprTxHBand_DrawDesign(Self,DC,ExData,DrawRect,ColCount);
end;





/////////////////////////////////////////////////
//
// VERTICAL BANDS
//
/////////////////////////////////////////////////

procedure TprTxVBand_DrawDesign(Band : TprBand; DC : HDC; ExData : pointer; const DrawRect : TRect);
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
  SelectObject(DC,ofn);
  SelectObject(DC,opn);
  DeleteObject(nfn);
  DeleteObject(npn);
end;
end;

//////////////////////////
//
// TprTxVTitleBand
//
//////////////////////////
procedure TprTxVTitleBand.DrawDesign;
begin
TprTxVBand_DrawDesign(Self,DC,ExData,DrawRect);
end;

/////////////////////////
//
// TprTxVSummaryBand
//
/////////////////////////
procedure TprTxVSummaryBand.DrawDesign;
begin
TprTxVBand_DrawDesign(Self,DC,ExData,DrawRect);
end;

//////////////////////////
//
// TprTxVPageHeaderBand
//
//////////////////////////
procedure TprTxVPageHeaderBand.DrawDesign;
begin
TprTxVBand_DrawDesign(Self,DC,ExData,DrawRect);
end;

/////////////////////////////
//
// TprTxVPageFooterBand
//
/////////////////////////////
procedure TprTxVPageFooterBand.DrawDesign;
begin
TprTxVBand_DrawDesign(Self,DC,ExData,DrawRect);
end;

//////////////////////////
//
// TprTxVDetailBand
//
//////////////////////////
procedure TprTxVDetailBand.DrawDesign;
begin
TprTxVBand_DrawDesign(Self,DC,ExData,DrawRect);
end;

////////////////////////////////
//
// TprTxVDetailHeaderBand
//
////////////////////////////////
procedure TprTxVDetailHeaderBand.DrawDesign;
begin
TprTxVBand_DrawDesign(Self,DC,ExData,DrawRect);
end;

////////////////////////////////
//
// TprTxVDetailFooterBand
//
////////////////////////////////
procedure TprTxVDetailFooterBand.DrawDesign;
begin
TprTxVBand_DrawDesign(Self,DC,ExData,DrawRect);
end;

////////////////////////////////////
//
// TprTxVGroupHeaderBand
//
////////////////////////////////////
procedure TprTxVGroupHeaderBand.DrawDesign;
begin
TprTxVBand_DrawDesign(Self,DC,ExData,DrawRect);
end;

////////////////////////////////////
//
// TprTxVGroupFooterBand
//
////////////////////////////////////
procedure TprTxVGroupFooterBand.DrawDesign;
begin
TprTxVBand_DrawDesign(Self,DC,ExData,DrawRect);
end;







//////////////////////////
//
// TprTxEndPage
//
//////////////////////////
constructor TprTxEndPage.CreateEmpty;
begin
inherited CreateEmpty(_Report);
FCurDefTxFontOptionsEx := TprTxFontOptions.Create;
end;

constructor TprTxEndPage.Create;
begin
CreateEmpty(_Page.Report);

FColNum := TprTxPage(_Page).ColNum;
FLineNum := TprTxPage(_Page).LineNum;
FPageType := TprTxPage(_Page).PageType;

FTextDevice := TprTxReport(_Page.Report).TextDevice;
CurDefTxFontStyleEx := TprTxPage(_Page).DefTxFontStyleEx;
CurDefTxFontOptionsEx := TprTxPage(_Page).DefTxFontOptionsEx;
end;

destructor TprTxEndPage.Destroy;
begin
FCurDefTxFontOptionsEx.Free;
inherited;
end;

function TprTxEndPage.GetWidth : integer;
begin
Result := ColNum;
end;

function TprTxEndPage.GetHeight : integer;
begin
Result := LineNum;
end;

procedure TprTxEndPage.SetCurDefTxFontOptionsEx(Value : TprTxFontOptions);
begin
FCurDefTxFontOptionsEx.Assign(Value);
end;

function TprTxEndPage.GetReport: TprTxReport;
begin
  Result := TprTxReport(inherited Report);
end;

procedure TprTxEndPage.ThirdPass;
var
  i : integer;
begin
TextDevice.BeginEndPage(Self);
for i:=0 to oRecsCount-1 do
  begin
    oRec[i].SecondPass;
    oRec[i].ThirdPass(Self, Report.TextDevice, oRec[i].pRect);
  end;
end;


/////////////////////////////////////////////////
//
// TprTxPage
//
/////////////////////////////////////////////////
constructor TprTxPage.Create;
begin
inherited;
FDefTxFontOptionsEx := TprTxFontOptions.Create;
ColNum := 80;
LineNum := 60;
FDsgnColNumDelta := 0;
FDsgnLineNumDelta := 0;
end;

destructor TprTxPage.Destroy;
begin
FDefTxFontOptionsEx.Free;
inherited;
end;

procedure TprTxPage.Loaded;
begin
inherited;
UpdateBandsPageRect;
end;

procedure TprTxPage.DefineProperties(Filer : TFiler);
begin
inherited;
Filer.DefineProperty('DefTxFontStyle',ReadDefTxFontStyle,WriteDefTxFontStyle,false);
Filer.DefineProperty('DefTxFontOptions',ReadDefTxFontOptions,WriteDefTxFontOptions,false);
Filer.DefineProperty('DefTxFontStyleEx',ReadDefTxFontStyleEx,WriteDefTxFontStyleEx,StoredDefTxFontStyleEx);
Filer.DefineProperty('DefTxFontOptionsEx',ReadDefTxFontOptionsEx,WriteDefTxFontOptionsEx,StoredDefTxFontOptionsEx);
Filer.DefineProperty('Height',ReadHeight,nil,false);
Filer.DefineProperty('Width',ReadWidth,nil,false);
end;

procedure TprTxPage.ReadHeight;
begin
Reader.ReadInteger;
end;

procedure TprTxPage.ReadWidth;
begin
Reader.ReadInteger;
end;

procedure TprTxPage.ReadDefTxFontStyle(Reader : TReader);
begin
FDefTxFontStyleEx := TxReportOptions.FindTxFontStyle(Reader.ReadIdent);
end;

procedure TprTxPage.WriteDefTxFontStyle(Writer : TWriter);
begin
end;

procedure TprTxPage.ReadDefTxFontOptions(Reader : TReader);
var
  s : string;
  TxFontOption : TprTxFontOption;
begin
Reader.ReadValue;
while true do
  begin
    s := Reader.ReadStr;
    if s='' then break;
    TxFontOption := TxReportOptions.FindTxFontOption(s);
    if TxFontOption<>nil then
      FDefTxFontOptionsEx.Add(TxFontOption);
  end;
end;

procedure TprTxPage.WriteDefTxFontOptions(Writer : TWriter);
begin
end;

procedure TprTxPage.ReadDefTxFontStyleEx(Reader : TReader);
begin
FDefTxFontStyleEx := TxReportOptions.FindTxFontStyle(Reader.ReadIdent);
end;

procedure TprTxPage.WriteDefTxFontStyleEx(Writer : TWriter);
begin
if FDefTxFontStyleEx<>nil then
  Writer.WriteIdent(FDefTxFontStyleEx.Name);
end;

procedure TprTxPage.ReadDefTxFontOptionsEx(Reader : TReader);
var
  TxFontOption : TprTxFontOption;
begin
Reader.ReadListBegin;
while not Reader.EndOfList do
  begin
    TxFontOption := TxReportOptions.FindTxFontOption(Reader.ReadIdent);
    if TxFontOption<>nil then
      FDefTxFontOptionsEx.Add(TxFontOption);
  end;
Reader.ReadListEnd;
end;

procedure TprTxPage.WriteDefTxFontOptionsEx(Writer : TWriter);
var
  i : integer;
begin
Writer.WriteListBegin;
for i:=0 to FDefTxFontOptionsEx.Count-1 do
  Writer.WriteIdent(FDefTxFontOptionsEx[i].Name);
Writer.WriteListEnd;
end;

function TprTxPage.StoredDefTxFontOptionsEx : boolean;
begin
Result := FDefTxFontOptionsEx.Count>0;
end;

function TprTxPage.StoredDefTxFontStyleEx : boolean;
begin
Result := FDefTxFontStyleEx<>nil;
end;

function TprTxPage.GetDsgnColNum : integer;
begin
Result := FDsgnColNumDelta+FColNum;
end;

procedure TprTxPage.SetDsgnColNum(Value : integer);
begin
if FDsgnColNumDelta=Value-FColNum then exit;
FDsgnColNumDelta := Value-FColNum;
UpdateBandsPageRect;
end;

function TprTxPage.GetDsgnLineNum : integer;
begin
Result := FDsgnLineNumDelta+FLineNum;
end;

procedure TprTxPage.SetDsgnLineNum(Value : integer);
begin
if FDsgnLineNumDelta=Value-FLineNum then exit;
FDsgnLineNumDelta := Value-FLineNum;
UpdateBandsPageRect;
end;

function TprTxPage.StoredDsgnColNum : boolean;
begin
Result := FDsgnColNumDelta<>0;
end;

function TprTxPage.StoredDsgnLineNum : boolean;
begin
Result := FDsgnLineNumDelta<>0;
end;

procedure TprTxPage.SetDefTxFontOptionsEx(Value : TprTxFontOptions);
begin
FDefTxFontOptionsEx.Assign(Value);
end;

function TprTxPage.DsgnPageRect : TRect;
begin
Result := Classes.Rect(0,0,DsgnColNum,DsgnLineNum)
end;

function TprTxPage.GenPageRect : TRect;
begin
Result := Classes.Rect(0,0,FColNum,FLineNum)
end;

////////////////////////////
//
// TprTextDevice
//
////////////////////////////
constructor TprTextDevice.CreateTextDevice;
begin
inherited;
Report := _Report;
CurEndPage := nil;
CurTopLine := 0;
SList := TStringList.Create;
Recs := TList.Create;
CurDefTxFontOptionsEx := TprTxFontOptions.Create;
end;

destructor TprTextDevice.Destroy;
begin
ClearRecs;
Recs.Free;
SList.Free;
CurDefTxFontOptionsEx.Free;
inherited;
end;

procedure TprTextDevice.ClearRecs;
var
  i : integer;
begin
for i:=0 to Recs.Count-1 do
  FreeMem(Recs[i]);
Recs.Clear;
end;

procedure TprTextDevice.Reset;
begin
ClearRecs;
SList.Clear;
CurEndPage := nil;
CurTopLine := 0;
CureFormat := '';
CurDefTxFontStyleEx := nil;
CurDefTxFontOptionsEx.Clear;
end;

procedure TprTextDevice.BeginEndPage;
var
  sFormat : string;
begin
if CurEndPage<>nil then
  begin
    // Probably it is necessary to insert character formfeed
    if (CurEndPage.PageType=tptPage) or (EndPage.PageType=tptPage) then
      begin
        // inserting
        if SList.Count>0 then
          SList[SList.Count-1] := SList[SList.Count-1]+TxReportOptions.GetFormFeedFullESCPrefix;
      end;
  end;
CurEndPage := EndPage;
CurTopLine := SList.Count;

if (CurDefTxFontStyleEx<>EndPage.CurDefTxFontStyleEx) or (not CurDefTxFontOptionsEx.Equals(EndPage.CurDefTxFontOptionsEx)) then
  begin
    // It is necessary to change default styles, in the beginning CureFormat
    if CureFormat<>'' then
      begin
        if SList.Count=0 then
          SList.Add('');
        SList[SList.Count-1] := SList[SList.Count-1]+CureFormat;
      end;

    // make new formats
    sFormat := TxReportOptions.GetTxFontStyleESCPrefix(EndPage.CurDefTxFontStyleEx)+TxReportOptions.GetTxFontOptionsESCPrefixOn(EndPage.CurDefTxFontOptionsEx);
    CureFormat := TxReportOptions.GetTxFontOptionsESCPrefixOff(EndPage.CurDefTxFontOptionsEx);

    CurDefTxFontStyleEx := EndPage.CurDefTxFontStyleEx;
    CurDefTxFontOptionsEx.Assign(EndPage.CurDefTxFontOptionsEx);

    if sFormat<>'' then
      begin
        if SList.Count=0 then
          SList.Add('');
        SList[SList.Count-1] := SList[SList.Count-1]+sFormat;
      end;
  end;
end;

//
// For want of conclusion it is necessary to take into account ESC sequences,
// the coordinates of a rectangle R are transmitted disregarding
// these sequences
//
procedure TprTextDevice.PlaceTxMemo(rec: TprTxMemoObjRecVersion; r: TRect; PreviewUserData: TprPreviewUserData);
var
  tdr: pTextDeviceRec;
  S, AText, Buf, sFormat, eFormat: string;
  I, J, K, N, M, Y, ALineCount: Integer;
  ALines: TvgrWrapLineDynArray;
  A: PIntArray;
  AInnerRect: TRect;
  ALineText, ABrdLeft, ABrdTop, ABrdRight, ABrdBottom: string;
  AHeight, AWidth, ALineLengthAvail, ALineLength, ABrdTopOffs, ABrdBottomOffs, ALineLeft, ALineRight, ATextStart, ATextLen, ALineIndex: Integer;
  ARealAlign: TprHAlign;
begin
  r.Left := Max(r.Left, 0);
  r.Right := Max(r.Right, 0);
  r.Top := Max(r.Top, 0);
  r.Bottom := Max(r.Bottom, 0);
  if (r.Left = r.Right) or (r.Top = r.Bottom) then exit;

  GetMem(tdr, sizeof(rTextDeviceRec));
  with tdr^ do
  begin
    rPlace := Rect(r.Left, r.Top + CurTopLine, r.Right - 1, r.Bottom + CurTopLine - 1);
    UserData := PreviewUserData;
  end;
  Recs.Add(tdr);

  // calc inner rect (substract borders)
  rec.CalcInnerRect(r, AInnerRect);

  // prepare text to print
  AText := rec.Memo.Text;
  WrapMemoChars(AText,
                AInnerRect.Right - AInnerRect.Left,
                ALines, rec.WordWrap,
                rec.DeleteEmptyLines,
                rec.DeleteEmptyLinesAtEnd);
  ALineCount := Length(ALines);

  // prepare borders
  rec.BuildBorders(r.Right - r.Left,
                   r.Bottom - r.Top,
                   ABrdLeft,
                   ABrdTop,
                   ABrdRight,
                   ABrdBottom,
                   ABrdTopOffs,
                   ABrdBottomOffs);
  TprTxReport(Report).OemToWin(ABrdLeft, ABrdLeft);
  TprTxReport(Report).OemToWin(ABrdTop, ABrdTop);
  TprTxReport(Report).OemToWin(ABrdRight, ABrdRight);
  TprTxReport(Report).OemToWin(ABrdBottom, ABrdBottom);

  // align vertically
  ATextLen := Min(ALineCount, AInnerRect.Bottom - AInnerRect.Top);
  case rec.vAlign of
    prvCenter:
      if AInnerRect.Bottom - AInnerRect.Top < ATextLen then
        ATextStart := 0
      else
        ATextStart := (AInnerRect.Bottom - AInnerRect.Top - ATextLen) div 2;
    prvBottom:
      if AInnerRect.Bottom - AInnerRect.Top < ATextLen then
        ATextStart := 0
      else
        ATextStart := AInnerRect.Bottom - AInnerRect.Top - ATextLen
    else
      ATextStart := 0;
  end;
  ATextStart := ATextStart + AInnerRect.Top - r.Top;


  AHeight := r.Bottom - r.Top;
  AWidth := r.Right - r.Left;
  for I := 0 to AHeight - 1 do
  begin
    // calculate a line to drawing
    if (I = 0) and (ABrdTop <> '') then
    begin
      S := ABrdTop;
      if ABrdLeft <> '' then
        S := ABrdLeft[1] + S;
      if ABrdRight <> '' then
        S := S + ABrdRight[1]
    end
    else
    begin
      if (I = AHeight - 1) and (ABrdBottom <> '') then
      begin
        S := ABrdBottom;
        if ABrdLeft <> '' then
          S := ABrdLeft[Length(ABrdLeft)] + S;
        if ABrdRight <> '' then
          S := S + ABrdRight[Length(ABrdRight)];
      end
      else
      begin
        if (ABrdLeft = '') and (ABrdRight = '') and ((I < ATextStart) or (I >= ATextStart + ATextLen)) then
          continue;
          
        S := MakeStr(' ', AWidth);
        if ABrdLeft <> '' then
        begin
          S[1] := ABrdLeft[I + 1];
          ALineLeft := 2;
        end
        else
          ALineLeft := 1;
          
        if ABrdRight <> '' then
        begin
          S[AWidth] := ABrdRight[I + 1];
          ALineRight := AWidth - 1;
        end
        else
          ALineRight := AWidth;

        if (I >= ATextStart) and (I < ATextStart + ATextLen) then
        begin
          ALineIndex := I - ATextStart;
          ALineText := Copy(AText, ALines[ALineIndex].Start, ALines[ALineIndex].Length);
          ALineLength := Length(ALineText);
          ALineLengthAvail := ALineRight - ALineLeft + 1;

          ARealAlign := rec.hAlign;
          if ALineLengthAvail <= ALineLength then
            ARealAlign := prhLeft
          else
            if ARealAlign = prhJustify then
            begin
              if (ALineIndex = ALineCount - 1) and not rec.JustifyLastLine then
              begin
                // last line
                ARealAlign := prhLeft;
              end
              else
              begin
                if ALines[ALineIndex].CreatedFromCrLf and
                   rec.EolIsEndOfParagraph and
                   ((ALineIndex <> ALineCount - 1) or not rec.JustifyLastLine) then
                  ARealAlign := prhLeft;
              end;
            end;

          case ARealAlign of
            prhLeft:
              Move(ALineText[1], S[ALineLeft], Min(ALineLengthAvail, ALineLength));
            prhCenter:
              Move(ALineText[1], S[ALineLeft + (ALineLengthAvail - ALineLength) div 2], ALineLength);
            prhRight:
              Move(ALineText[1], S[ALineRight - ALineLength + 1], ALineLength);
            prhJustify:
              begin
                // string must be justified
                A := AllocMem(ALineLength * SizeOf(Integer));
                try
                  JustifyArray(A, ALineLength, ALineLengthAvail, ALineLength, True, PChar(ALineText));
                  K := ALineLeft;
                  for N := 1 to ALineLength do
                  begin
                    S[K] := ALineText[N];
                    K := K + 1 + A^[N - 1];
                  end;
                finally
                  FreeMem(A);
                end;
              end;
          end;
        end
      end;
    end;

    // S has a text to print
    Y := CurTopLine + I + r.Top;

    while SList.Count <= Y do
      SList.Add('');


    Buf := SList[Y];
    ALineLength := Length(Buf);

    // We define an item with which it is necessary to place string Memo[i]
    J := 1;
    N := TxReportOptions.ESCSkipTo(Buf, r.Left + 1, J);

    if J > ALineLength then
      // N means contains an amount of real characters in string
      // real position (j) With allowance for
      // ESC specifiers possible to calculate so:
      J := J + r.Left - N
    else
    begin
      // It is necessary to count how many in string of real characters,
      K := J;
      Dec(J);
      N := N + TxReportOptions.ESCSkipTo(Buf, r.Right + 1, K);
    end;

    if r.Right > N then
      // add spaces, n - count of real symbols
      Buf := AddCharR(' ', Buf, r.Right - N + ALineLength);

    // j - The item with which is necessary to place string Memo[i]
    // Buf - The destination string
    sFormat := '';
    eFormat := '';
    if not rec.DefaultFont then
    begin
      if rec.TxFontStyleEx <> CurDefTxFontStyleEx then
      begin
        sFormat := sFormat+TxReportOptions.GetTxFontStyleESCPrefix(rec.TxFontStyleEx);
        eFormat := eFormat+TxReportOptions.GetTxFontStyleESCPrefix(CurDefTxFontStyleEx);
      end;
      for M := 0 to TxReportOptions.TxFontOptionsCount - 1 do
        if CurDefTxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[M]) and
           not rec.TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[M]) then
        begin
          // must off this style
          sFormat := sFormat + TxReportOptions.GetTxFontOptionESCPrefixOff(TxReportOptions.TxFontOptions[M]);
          eFormat := TxReportOptions.GetTxFontOptionESCPrefixOn(TxReportOptions.TxFontOptions[M]) + eFormat;
          end
        else
          if not CurDefTxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[M]) and
             rec.TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[M]) then
          begin
            // must on this style
            sFormat := sFormat + TxReportOptions.GetTxFontOptionESCPrefixOn(TxReportOptions.TxFontOptions[M]);
            eFormat := TxReportOptions.GetTxFontOptionESCPrefixOff(TxReportOptions.TxFontOptions[M]) + eFormat;
          end;
      end;

      if sFormat <> '' then
      begin
        Insert(sFormat, Buf, J);
        J := J + Length(sFormat);
      end;

      TprTxReport(Report).WinToOem(PChar(S), PChar(S));
      Move(S[1], Buf[j], Length(S));

      if eFormat <> '' then
        Insert(eFormat, Buf, J + AWidth);

      SList[Y] := Buf;
  end;

{
  for I := 0 to CalcH - 1 do
    begin
      S := Copy(AText, ALines[I].Start, ALines[I].Length);
      y := CurTopLine + i + r.Top + CurY;

      while SList.Count <= y do
        SList.Add('');

      Buf := SList[y];
      l := Length(Buf);

      // We define an item with which it is necessary to place string Memo[i]
      j := 1;
      n := TxReportOptions.ESCSkipTo(Buf,r.Left+1,j);

      if j>l then
        // N means contains an amount of real characters in string
        // real position (j) With allowance for
        // ESC specifiers possible to calculate so:
        j := j+r.Left-n
      else
        begin
          // It is necessary to count how many in string of real characters,
          k := j;
          Dec(j);
          n := n+TxReportOptions.ESCSkipTo(Buf,r.Right+1,k);
        end;

      if r.Right>n then
        // add spaces, n - count of real symbols
        Buf := AddCharR(' ',Buf,r.Right-n+l);

      // j - The item with which is necessary to place string Memo[i]
      // Buf - String in which to place
      sFormat := '';
      eFormat := '';
      if not rec.DefaultFont then
        begin
          if rec.TxFontStyleEx<>CurDefTxFontStyleEx then
            begin
              sFormat := sFormat+TxReportOptions.GetTxFontStyleESCPrefix(rec.TxFontStyleEx);
              eFormat := eFormat+TxReportOptions.GetTxFontStyleESCPrefix(CurDefTxFontStyleEx);
            end;
          for m:=0 to TxReportOptions.TxFontOptionsCount-1 do
            if CurDefTxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[m]) and
               not rec.TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[m]) then
              begin
                // must off this style
                sFormat := sFormat+TxReportOptions.GetTxFontOptionESCPrefixOff(TxReportOptions.TxFontOptions[m]);
                eFormat := TxReportOptions.GetTxFontOptionESCPrefixOn(TxReportOptions.TxFontOptions[m])+eFormat;
              end
            else
              if not CurDefTxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[m]) and
                 rec.TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[m]) then
                begin
                  // must on this style
                  sFormat := sFormat+TxReportOptions.GetTxFontOptionESCPrefixOn(TxReportOptions.TxFontOptions[m]);
                  eFormat := TxReportOptions.GetTxFontOptionESCPrefixOff(TxReportOptions.TxFontOptions[m])+eFormat;
                end;
        end;
  
      if sFormat<>'' then
        begin
          Insert(sFormat,Buf,j);
          j := j+Length(sFormat);
        end;
  
      FillMemory(@(Buf[j]),w,32);
  
      l := Length(S);
      TprTxReport(Report).WinToOem(PChar(S), PChar(S));

      ATa := rec.hAlign;
      if W <= L then
        ATa := prhLeft
      else
        if rec.hAlign = prhJustify then
        begin
          if (I = ALineCount - 1) and not rec.JustifyLastLine then
          begin
            // last line
            ATa := prhLeft;
          end
          else
          begin
            if ALines[I].CreatedFromCrLf and rec.EolIsEndOfParagraph and ((I <> ALineCount - 1) or not rec.JustifyLastLine) then
              ATa := prhLeft;
          end;
        end;

      case ATa of
        prhLeft:
          MoveMemory(@(Buf[j]),@(s[1]),Min(w,l));
        prhCenter:
          MoveMemory(@(Buf[j+((w-l) div 2)]),@(s[1]),l);
        prhRight:
          MoveMemory(@(Buf[j+w-l]),@(s[1]),l);
        prhJustify:
          begin
            // string must be justified
            A := AllocMem(l * SizeOf(Integer));
            try
              JustifyArray(a, l, w, l, True, PChar(S));
              K := J;
              for N := 1 to L do
              begin
                Buf[K] := S[N];
                K := K + 1 + A^[N - 1];
              end;
            finally
              FreeMem(A);
            end;
          end;
      end;

      if eFormat<>'' then
        Insert(eFormat,Buf,j+w);

      SList[y] := Buf;
    end;
}
end;

procedure TprTextDevice.PlaceTxCommand(rec : TprTxCommandObjRecVersion; const r : TRect; PreviewUserData : TprPreviewUserData);
var
  s,Buf : string;
  i,j,y : integer;
begin
y := CurTopLine+r.Top;

while SList.Count<=y do
  SList.Add('');

Buf := SList[y];

// We define an item with which it is necessary to place string Memo[i]
j := 1;
i := TxReportOptions.ESCSkipTo(Buf,r.Left+1,j);

if j>Length(Buf) then
  begin
    // N means contains an amount of real characters in string
    // real position (j) With allowance for
    // ESC specifiers possible to calculate so:
    Buf := Buf+MakeStr(' ',r.Left-i+1);
    j := j+r.Left-i;
  end
else
  Dec(j);
// insert TxCommands in position j
s := '';
for i:=0 to rec.TxCommandsCount-1 do
  s := s+ESCSymbol+rec.TxCommands[i].FullPrefix;
Insert(s,Buf,j);

SList[y] := Buf;
end;

procedure TprTextDevice.PlaceMemo;
var
  s,Buf : string;
  i,l,w,y,CurY,CurX,CalcH : integer;
begin
CalcH:=Memo.Count;
case vAlign of
  prvCenter:
    if r.Bottom-r.Top<CalcH then CurY:=0
                            else CurY:=(r.Bottom-r.Top-CalcH) div 2;
  prvBottom:
    if r.Bottom-r.Top<CalcH then CurY:=0
                            else CurY:=r.Bottom-r.Top-CalcH
  else
    CurY:=0;
end;

for i:=0 to Memo.Count-1 do
  begin
    y:=CurTopLine+i+r.Top+CurY;

    while SList.Count<=y do
      SList.Add('');

    l:=Length(Memo[i]);
    SetLength(s,l);
    MoveMemory(@(s[1]),@(Memo[i][1]),l);
    Report.WinToOem(PChar(s),PChar(s));

    w   :=Min(r.Right-r.Left,l);
    CurX:=r.Left;
    case hAlign of
      prhCenter:
        if r.Right-r.Left>l then CurX:=CurX+(r.Right-r.Left-l) div 2;
      prhRight:
        if r.Right-r.Left>l then CurX:=CurX+r.Right-r.Left-l;
    end;

    if w>0 then
      begin
        Buf:=AddChar(' ',SList[y],CurX+w-Length(SList[y])+1);

        MoveMemory(@(Buf[CurX+1]),@(s[1]),w);

        SList[y]:=Buf;
      end;
  end;
end;

const
  TextDeviceSignature = $0101;

procedure TprTextDevice.LoadFromStream;
var
  r : TRect;
  s : string;
  tdr : pTextDeviceRec;
  i,n : integer;
  PreviewUserData : TprPreviewUserData;
begin
Stream.Read(n,4);
if n=TextDeviceSignature then
  begin
    Stream.Read(n,sizeof(n));
    for i:=0 to n-1 do
      begin
        ReadRect(Stream,r);
        s := ReadString(Stream);
        if s<>'' then
          begin
            PreviewUserData := CreateprPreviewUserData(s);
            PreviewUserData.LoadFromStream(Stream);
          end
        else
          PreviewUserData := nil;

        GetMem(tdr,sizeof(rTextDeviceRec));
        with tdr^ do
          begin
            rPlace := r;
            UserData := PreviewUserData;
          end;
        Recs.Add(tdr);
        if PreviewUserData<>nil then
          Report.AddPreviewUserData(PreviewUserData);
      end;
  end
else
  Stream.Seek(0,soFromBeginning);
SList.LoadFromStream(Stream);
end;

procedure TprTextDevice.AppendFromStream;
var
  r : TRect;
  s : string;
  l : TStringList;
  tdr : pTextDeviceRec;
  i,n : integer;
  PreviewUserData : TprPreviewUserData;
begin
Stream.Read(n,4);
if n=TextDeviceSignature then
  begin
    Stream.Read(n,sizeof(n));
    for i:=0 to n-1 do
      begin
        ReadRect(Stream,r);
        r.Top := r.Top+SList.Count;
        r.Bottom := r.Bottom+SList.Count;
        s := ReadString(Stream);
        if s<>'' then
          begin
            PreviewUserData := CreateprPreviewUserData(s);
            PreviewUserData.LoadFromStream(Stream);
          end
        else
          PreviewUserData := nil;

        GetMem(tdr,sizeof(rTextDeviceRec));
        with tdr^ do
          begin
            rPlace := r;
            UserData := PreviewUserData;
          end;
        Recs.Add(tdr);
        if PreviewUserData<>nil then
          Report.AddPreviewUserData(PreviewUserData);
      end;
  end
else
  Stream.Seek(0,soFromBeginning);
l := TStringList.Create;
try
  l.LoadFromStream(Stream);
  SList.AddStrings(l);
finally
  l.Free;
end;
end;

procedure TprTextDevice.SaveToStream;
var
  i,n : integer;

  procedure WriteRec(Rec : pTextDeviceRec);
  begin
  WriteRect(Stream,Rec.rPlace);
  if Rec.UserData<>nil then
    begin
      WriteString(Stream,Rec.UserData.ClassName);
      Rec.UserData.SaveToStream(Stream);
    end
  else
    WriteString(Stream,'');
  end;

begin
n := TextDeviceSignature;
Stream.Write(n,4);
n := Recs.Count;
Stream.Write(n,sizeof(n));
if (Stream is TMemoryStream) and (n>0) then
  begin
    i := 0;
    repeat
      WriteRec(pTextDeviceRec(Recs[i]));
      Inc(i);
      if Stream.Size=Stream.Position then
        Stream.Size := muldiv(n,Stream.Size,i);
    until (i>=n);
    Stream.Size := Stream.Position;
  end
else
  for i:=0 to Recs.Count-1 do
    WriteRec(pTextDeviceRec(Recs[i]));
SList.SaveToStream(Stream);
end;

///////////////////////////
//
// TprTxReport
//
///////////////////////////
constructor TprTxReport.Create;
begin
inherited;
FTextDevice := TprTextDevice.CreateTextDevice(Self);
FWrapAfterColumn := -1;
FEjectPageAfterPrint := true;
FPaperType := ptPage;
FUseLinesOnPage := false;
FLinesOnPage := 60;
FMakeFormFeedOnRulon := false;
FFromLine := -1;
FToLine := -1;
FExportFromLine := -1;
FExportToLine := -1;
end;

destructor TprTxReport.Destroy;
begin
FTextDevice.Free;
inherited;
end;

procedure TprTxReport.DoOnPrintLine(ALineNo: Integer; ALineNoInPage: Integer; AWrapIndex: Integer; var ALineText: string; var AFinishPrinting: Boolean);
begin
  if Assigned(FOnPrintLine) then
  begin
    AFinishPrinting := False;
    FOnPrintLine(Self, ALineNo, ALineNoInPage, AWrapIndex, ALineText, AFinishPrinting);
  end;

  if not AFinishPrinting then
    ParsePrintingString(ALineText);
end;

function TprTxReport.CreatePage : TprCustomPage;
begin
Result := TprTxPage.Create(prOwner);
Result.Report := Self;
end;

function TprTxReport.GetUsedRecodeTable: TprTxRecodeTable;
begin
  if RecodeTable <> nil then
    Result := RecodeTable
  else
    if TxReportOptions.DefaultRecodeTable <> nil then
      Result := TxReportOptions.DefaultRecodeTable
    else
      Result := nil;
end;

procedure TprTxReport.WinToOem(sSource,sDest : PChar);
var
  ARecodeTable: TprTxRecodeTable;
begin
  ARecodeTable := GetUsedRecodeTable;
  if ARecodeTable <> nil then
    ARecodeTable.WINtoOEM(sSource, sDest)
  else
    MoveMemory(sDest, sSource, strlen(sSource));
end;

procedure TprTxReport.WinToOem(const sSource: string; var sDest: string);
var
  ARecodeTable: TprTxRecodeTable;
begin
  ARecodeTable := GetUsedRecodeTable;
  if ARecodeTable <> nil then
    ARecodeTable.WINtoOEM(sSource, sDest)
  else
    sDest := sSource;
end;

function TprTxReport.WinToOem(sSource: char): char;
var
  ARecodeTable: TprTxRecodeTable;
begin
  ARecodeTable := GetUsedRecodeTable;
  if ARecodeTable <> nil then
    Result := ARecodeTable.WINtoOEM(sSource)
  else
    Result := sSource;
end;

procedure TprTxReport.OemToWin(sSource,sDest: PChar);
begin
if RecodeTable<>nil then
  RecodeTable.OemToWin(sSource,sDest)
else
  if TxReportOptions.DefaultRecodeTable<>nil then
    TxReportOptions.DefaultRecodeTable.OemToWin(sSource,sDest)
  else
    MoveMemory(sDest,sSource,strlen(sSource));
end;

procedure TprTxReport.OemToWin(const sSource: string; var sDest: string);
var
  ARecodeTable: TprTxRecodeTable;
begin
  ARecodeTable := GetUsedRecodeTable;
  if ARecodeTable <> nil then
    ARecodeTable.OemToWin(sSource, sDest)
  else
    sDest := sSource;
end;

function TprTxReport.OemToWin(sSource: char): char;
var
  ARecodeTable: TprTxRecodeTable;
begin
  ARecodeTable := GetUsedRecodeTable;
  if ARecodeTable <> nil then
    Result := ARecodeTable.OemToWin(sSource)
  else
    Result := sSource;
end;

function TprTxReport.GetPage(Index: Integer): TprTxPage;
begin
  Result := TprTxPage(inherited Pages[Index]);
end;

function TprTxReport.GetEndPage(Index: Integer): TprTxEndPage;
begin
  Result := TprTxEndPage(inherited EndPages[Index]);
end;

procedure TprTxReport.SetRecodeTableName(Value : string);
begin
if FRecodeTableName=Value then exit;
FRecodeTable := TxReportOptions.FindRecodeTable(Value);
if FRecodeTable<>nil then
  FRecodeTableName := Value
else
  FRecodeTableName := '';
end;

procedure TprTxReport.SetESCModelName(Value : string);
begin
if FESCModelName=Value then exit;
FESCModel := TxReportOptions.FindESCModel(Value);
if FESCModel<>nil then
  FESCModelName := Value
else
  FESCModelName := '';
end;

procedure TprTxReport.SetExportESCModelName(Value : string);
begin
if FExportESCModelName=Value then exit;
FExportESCModel := TxReportOptions.FindESCModel(Value);
if FExportESCModel<>nil then
  FExportESCModelName := Value
else
  FExportESCModelName := '';
end;

function TprTxReport.GetFontCharSet : integer;
begin
if (RecodeTable<>nil) or (TxReportOptions.DefaultRecodeTable<>nil) then
  Result := OEM_CHARSET
else
  Result := DEFAULT_CHARSET;
end;

function TprTxReport.GetDesignerFormClass;
begin
Result := 'TprTxDesignerForm';
end;

function TprTxReport.GetPreviewFormClass;
begin
Result := 'TprTxPreviewForm';
end;

function TprTxReport.CheckEndPagesCountOnPreview;
begin
Result := false;
end;

function TprTxReport.CreateEndPage;
begin
Result := TprTxEndPage.Create(Page as TprTxPage);
end;

function TprTxReport.CreateEmptyEndPage;
begin
Result := TprTxEndPage.CreateEmpty(Self);
end;

procedure TprTxReport.SetPrinterName;
begin
FPrinterName := Value;
end;

function TprTxReport.GetPrinterName;
begin
Result := FPrinterName;
end;

function TprTxReport.GetPreparedReportEmpty : boolean;
begin
Result := TextDevice.SList.Count=0;
end;

function TprTxReport.SetupPrintParams;
begin
Result := TxReportOptions.TxSetupPrintParams(Screen.ActiveForm.Handle,
                                             TxReportOptions.ParsePages(TextDevice.SList,UseLinesOnPage,LinesOnPage,nil),
                                             TextDevice.SList.Count,
                                             FLinesOnPage,
                                             FFromPage,
                                             FToPage,
                                             FPrintPagesMode,
                                             FESCModelName,
                                             FPrinterName,
                                             FUseLinesOnPage,
                                             FWrapAfterColumn,
                                             FMakeFormFeedOnRulon,
                                             FLeftSpaces,
                                             FPrintRulonMode,
                                             FFromLine,
                                             FToLine,
                                             FPrintPages,
                                             PrintPagesList,
                                             FPaperType,
                                             FCopies,
                                             FEjectPageAfterPrint);
if Assigned(OnCloseSetupDialog) then
  OnCloseSetupDialog(Self,Result);
end;

procedure TprTxReport.ClearPreparedReport;
begin
inherited;
TextDevice.Reset;
end;

function TprTxReport.PrepareReport;
begin
TextDevice.Reset;
Result := inherited PrepareReport;
ClearEndPages;
end;

procedure PrintStringCallback(ALineNo: Integer; ALineNoInPage: Integer; AWrapIndex: Integer; var S: string; var AFinishPrinting: Boolean; AData: Pointer);
begin
  TprTxReport(AData).DoOnPrintLine(ALineNo, ALineNoInPage, AWrapIndex, S, AFinishPrinting); // ParsePrintingString(S);
end;

function TprTxReport.PrintPreparedReport;
begin
  Result := false;
  if ShowProgress then
    CreateProgressForm(prLoadStr(sPrintReportCaption), 0);

  try
    try
      if (PrinterName = '') or (PrinterName = prLoadStr(sDefaultPrinterName)) then
        PrinterName := prGetDefaultPrinterName; // We take the printer by default and ESC a model for this printer

      DoOnPrintStart;

      Result := TxReportOptions.TxPrintStrings(TextDevice.SList,
                                               PrinterName,
                                               ESCModelName,
                                               Title,
                                               Copies,
                                               WrapAfterColumn,
                                               LeftSpaces,
                                               PaperType,
                                               PrintRulonMode,
                                               MakeFormFeedOnRulon,
                                               FromLine,
                                               ToLine,
                                               PrintPagesMode,
                                               LinesOnPage,
                                               UseLinesOnPage,
                                               PrintPagesList,
                                               FromPage,
                                               ToPage,
                                               FPrintingPage,
                                               FPrintingCopy,
                                               nil,
                                               StartNewLineOnWrap,
                                               EjectPageAfterPrint,
                                               @PrintStringCallback,
                                               Self);
    except
      on E : Exception do
      begin
        if E is EActionCanceled then
          FActionCanceled := true
        else
          raise;
      end;
    end;
  finally
    CloseProgressForm;
    if Result and Assigned(OnPrintComplete) then
      OnPrintComplete(Self);
  end;
end;

function TprTxReport.GetBandClass;
begin
  Result := TprBandClass(GetClass('TprTx'+Copy(GetEnumName(TypeInfo(TprBandType),integer(BandType)),3,Length(GetEnumName(TypeInfo(TprBandType),integer(BandType))))+'Band'));
end;

procedure TprTxReport.LoadPreparedReport;
begin
  TextDevice.Reset;
  TextDevice.LoadFromStream(Stream);
  inherited;
end;

procedure TprTxReport.AppendPreparedReport;
begin
  TextDevice.AppendFromStream(Stream);
  inherited;
end;

procedure TprTxReport.SavePreparedReport;
begin
  TextDevice.SaveToStream(Stream);
  inherited;
end;

function TprTxReport.SetupExportParams;
begin
  Result := TprTxExportParamsForm.Create(nil).EditParams(Self);
end;

procedure TprTxReport.ExportTo;
begin
  if TextDevice.SList.Count<=0 then
    raise Exception.Create(prLoadStr(sReportEmptyInExport));
  inherited;
end;

var
  i : integer;

initialization

  RegisterClass(TprTxHTitleBand);
  RegisterClass(TprTxHSummaryBand);
  RegisterClass(TprTxHPageHeaderBand);
  RegisterClass(TprTxHPageFooterBand);
  RegisterClass(TprTxHDetailBand);
  RegisterClass(TprTxHDetailHeaderBand);
  RegisterClass(TprTxHDetailFooterBand);
  RegisterClass(TprTxHGroupHeaderBand);
  RegisterClass(TprTxHGroupFooterBand);
  RegisterClass(TprTxVTitleBand);
  RegisterClass(TprTxVSummaryBand);
  RegisterClass(TprTxVPageHeaderBand);
  RegisterClass(TprTxVPageFooterBand);
  RegisterClass(TprTxVDetailBand);
  RegisterClass(TprTxVDetailHeaderBand);
  RegisterClass(TprTxVDetailFooterBand);
  RegisterClass(TprTxVGroupHeaderBand);
  RegisterClass(TprTxVGroupFooterBand);
  
  RegisterClass(TprTxPage);
  RegisterClass(TprTxMemoObj);
  RegisterClass(TprTxCommandObj);
  RegisterClass(TprTxReport);
  
  // register bands
  for i:=integer(Low(TprBandType)) to integer(High(TprBandType)) do
    prRegisterBand(TprBandClass(GetClass('TprTx'+Copy(GetEnumName(TypeInfo(TprBandType),integer(i)),3,Length(GetEnumName(TypeInfo(TprBandType),i)))+'Band')),
                   TprTxReport,
                   'TprBandEditorForm');
  
  // register objects                 
  prRegisterObj(TprTxMemoObj,
                TprTxMemoObjRec,
                TprTxReport,
                sTxMemoObjCaption,
                sTxMemoObjHint,
                'TprTxMemoEditorForm',
                '');
  prRegisterObj(TprTxCommandObj,
                TprTxCommandObjRec,
                TprTxReport,
                sTxCommandObjCaption,
                sTxCommandObjHint,
                'TprTxCommandEditorForm',
                '');
  
end.

