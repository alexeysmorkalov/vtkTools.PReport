{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the auxiliary classes, used by the TprTxReport.}
unit pr_TxConsts;

interface

{$I PR.INC}

uses
  SysUtils, Classes, Graphics, typinfo, windows, WinSpool, CommDlg, Forms,
  messages, inifiles, math,

  pr_Common;

const
{Name of the font used by designer of the TprTxReport by default.
Syntax:
  sOemFontName = 'Courier New';}
  sOemFontName = 'Courier New';
{Syntax:
  ESCSymbol = #27;}
  ESCSymbol = #27;
{Syntax:
  ESCCustomCommandSymbol = #26;}
  ESCCustomCommandSymbol = #26;
{Syntax:
  ESCSymbolForExCommands = #27;}
  ESCSymbolForExCommands = #27;

type
  TprTxFontOption = class;
  TprTxReportOptions = class;
{Describes the paper type for text report.
Items:
  ptPage - The paper is the separated pages, usually when text report is printed on this paper
it should scroll paper at the end of each page.
  ptRulon - The paper is the roll.}
  TprTxPaperType = (ptPage, ptRulon);
{Describes the printed range of the text report if roll is used.
Items:
  prmAllLines - All lines of the report should be printed.
  prmLinesRange - Specified range of the report's lines should be printed.}
  TprTxPrintRulonMode = (prmAllLines, prmLinesRange);

  /////////////////////////////////////////////////
  //
  // TprTxCommand
  //
  /////////////////////////////////////////////////
{Represents the separate command, which can be executed by the printer in text mode.
The list of all supported commands loaded from the TxRO.ini file at the programm startup.}
  TprTxCommand = class(TObject)
  private
    FName: string;
    FPrefix: string;
    FFullPrefix: string;
    FDescription: string;
  public
{Returns the name of command, for example:<br>
txcReset - Resets the printer.<br>
txcFormFeed - Form feed.<br>
txcBoldOn - <br>}
    property Name: string read FName;
{Returns the unique command prefix, its length must equal to one or three chars.
This prefix is used on the stage of the report printing to identify the
command and finding the native command which can be executed by printer.
For example, after generating the TprTxReport report it contains lines:<br>
  #27IMy report#27i<br>
<br>
  Field      Field      Field<br>
  title 1    title 2    title 3<br>
<br>
  Value      Value      Value<br>
<br>
In the first line the text "My report" is placed between two commands.<br>
#27I - Specifies the command for On italic font.<br>
#27i - Specifies the command for Off italic font<br>
Symbol [I] is a prefix of the txcItalicOn command, symbol [i] is a prefix of the
txcItalicOff command.}
    property Prefix: string read FPrefix;
{Returns the "full" command prefix, if the length of the command prefix equals to one char
then FullPrefix equals to Prefix. If the length of the command prefix equals to three chars
the FullPrefix equals to [ESCSymbolForExCommands + Prefix].}
    property FullPrefix: string read FFullPrefix;
{Returns the command's description, for example:<br>
The txcReset command has description - "Reset priter settings to default".}
    property Description: string read FDescription;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxFontOption
  //
  /////////////////////////////////////////////////
{Represents the separate font option in the text report.
Option is described by two commands, the first switchs on the font option,
the second switchs off the font option.
See also:
  TprTxFontStyle}
  TprTxFontOption = class(TObject)
  private
    FName : string;
    FTxCommandOn : TprTxCommand;
    FTxCommandOff : TprTxCommand;
    FDescription : string;
  public
{Returns the name of the font option, for example: <b>tfoCondensed</b>.}
    property Name: string read FName;
{Specifies the command which swiths on the font option.
See also:
  TprTxCommand}
    property TxCommandOn: TprTxCommand read FTxCommandOn write FTxCommandOn;
{Specifies the command which swiths off the font option.
See also:
  TprTxCommand}
    property TxCommandOff: TprTxCommand read FTxCommandOff write FTxCommandOff;
{Returns the description of the font option.}
    property Description: string read FDescription;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxFontOptions
  //
  /////////////////////////////////////////////////
{Represents the list of the font options supported by TprTxReport.
This list is loaded from the TxRO.ini file at the programm startup.}
  TprTxFontOptions = class(TList)
  private
    function GetItm(i : integer) : TprTxFontOption;
  public
{Lists the font options in the list.
Parameters:
  I - The index of the object.
See also:
  TprTxFontOption}
    property Items[i: integer]: TprTxFontOption read GetItm; default;
{Copies the list's content from another object.
Parameters:
  Source - The source object.}
    procedure Assign(Source: TprTxFontOptions);
{Returns the true value if the specified TprTxFontOptions object equals
to this object.
Parameters:
  Value - Specifies the another TprTxFontOptions object to test.
Return value:
  Returns the true if objects are identical.}
    function Equals(Value: TprTxFontOptions): Boolean;
{Returns the true if specified TprTxFontOption object exists in the list.
Parameters:
  TxFontOption - The TprTxFontOption object for search.
Return value:
  Returns the true if object is found.}
    function Contains(TxFontOption: TprTxFontOption): Boolean;
{Removes the specified TprTxFontOption object from the list.
Parameters:
  TxFontOption - The TprTxFontOption object to remove.}
    procedure Remove(TxFontOption: TprTxFontOption);
{Adds the TprTxFontOption object in the list.
Parameters:
  TxFontOption - The TprTxFontOption object to add.
Return value:
  Returns the index of the added object in the list.}
    function Add(TxFontOption : TprTxFontOption) : integer;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxFontStyle
  //
  /////////////////////////////////////////////////
{Represents the separate font style in the text report.
Style is described by command which select a style.
See also:
  TprTxFontOption}
  TprTxFontStyle = class(TObject)
  private
    FName: string;
    FTxCommand: TprTxCommand;
    FDescription: string;
  public
{Returns the name of the font style, for example: <b>tfsNormal</b>.}
    property Name: string read FName;
{Specifies the command which selects this font style.
See also:
  TprTxCommand}
    property TxCommand: TprTxCommand read FTxCommand write FTxCommand;
{Returns the description of the font style.}
    property Description: string read FDescription;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxBorderScheme
  //
  /////////////////////////////////////////////////
{Represents the predefined border schemas for TprTxMemoObj version.
Each scheme is defined by the eight chars for:<br>
left side of border<br>
left-top corner of border<br>
top side of border<br>
right-top corner of border<br>
right side of border<br>
right-bottom corner of border<br>
bottom side of border<br>
left-bottom corner of border<br>
The list of schemas can be defined for:<br>
- each recode table with using of the national characters.
These border schemas can be used when this recode table is specified
in the TprTxReport.RecodeTableName property only.<br>
- all reports (value of TprReport.RecodeTableName property is not used).
These border scheamas can use characters with codes 1..127 only.<br>
See also:
  TprTxRecodeTable}
  TprTxBorderScheme = class(TObject)
  private
    FCaption: string;
    FBrdLeft: Char;
    FBrdLeftTop: Char;
    FBrdTop: Char;
    FBrdRightTop: Char;
    FBrdRight: Char;
    FBrdRightBottom: Char;
    FBrdBottom: Char;
    FBrdLeftBottom: Char;
  public
{Creates an instance of the TprTxBorderScheme object.}
    constructor Create(ACaption: string; ABrdLeft, ABrdLeftTop, ABrdTop, ABrdRightTop, ABrdRight, ABrdRightBottom, ABrdBottom, ABrdLeftBottom: Char);
{Caption of scheme.}
    property Caption: string read FCaption;
{Specifies the char displaying as the left side of border.
Char must be specified in OEM codepage.}
    property BrdLeft: Char read FBrdLeft;
{Specifies the char displaying as the top-left corner of border.
Char must be specified in OEM codepage.}
    property BrdLeftTop: Char read FBrdLeftTop;
{Specifies the char displaying as the top side of border.
Char must be specified in OEM codepage.}
    property BrdTop: Char read FBrdTop;
{Specifies the char displaying as the top-right corner of border.
Char must be specified in OEM codepage.}
    property BrdRightTop: Char read FBrdRightTop;
{Specifies the char displaying as the right side of border.
Char must be specified in OEM codepage.}
    property BrdRight: Char read FBrdRight;
{Specifies the char displaying as the right-bottom corner of border.
Char must be specified in OEM codepage.}
    property BrdRightBottom: Char read FBrdRightBottom;
{Specifies the char displaying as the bottom side of border.
Char must be specified in OEM codepage.}
    property BrdBottom: Char read FBrdBottom;
{Specifies the char displaying as the left-bottom corner of border.
Char must be specified in OEM codepage.}
    property BrdLeftBottom: Char read FBrdLeftBottom;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxRecodeTable
  //
  /////////////////////////////////////////////////
{Represents the recode table which is used for text converting between ANSI character set (used in Windows) and OEM character set (used by the printers in the text mode).
Each record table is defined by the two arrays of chars, the first array is used for 
converting from OEM to ANSI (OEMtoWINTable property)
and the second is used for converting from ANSI to OEM (WINtoOEMTable property).
The legth of each array equals to 128 chars and is formed on the basis of rule (for OEMtoWINTable):
the first element contains the code of #128 (dec) OEM symbol in the ANSI character set,
the second contains the code of #129 (dec) OEM symbol in the ANSI character set and so on.

Usually the second record table (ANSI to OEM) can be automatically generated and can be missed 
in the TxRo.ini file.
The list of the supported record tables is loaded from the TxRo.ini file at application startup.}
  TprTxRecodeTable = class(TObject)
  private
    FBorderSchemas: TList;
    FRecodeTableName : string;
    FDescription : string;
    FOEMtoWINTable : array [0..127] of char;
    FWINtoOEMTable : array [0..127] of char;
    function GetOEMtoWINTable : PChar;
    function GetWINtoOEMTable : PChar;
    procedure Decode(sSource,sDest,Table : PChar);
    function GetBorderScheme(Index: Integer): TprTxBorderScheme;
    function GetBorderSchemeCount: Integer;
  public
{Creates an instance of the TprTxRecodeTable object.
Do not call the constructor, the all available TprTxRecodeTable objects
will be created at programm startup.}
    constructor Create;
{Frees an instance of the TprTxRecodeTable object.}
    destructor Destroy; override;
{Returns the name of the table, for example: <b>Russian866and1251</b>.
This name must be used when you want to define the recode table in the TprTxReport.RecodeTableName property.
See also:
  TprTxReport.RecodeTableName}
    property RecodeTableName: string read FRecodeTableName;
{Returns the description of the recode table, for example: <b>Russian DOS866 and Windows 1251</b>.}
    property Description: string read FDescription;
{Returns the pointer to array of chars which is used for OEM to ANSI converting.
The length of array is always 128 chars.
The first element contains the code of #128 (dec) OEM symbol in the ANSI character set,
the second element contains the code of #129 (dec) OEM symbol in the ANSI character set and so on.}
    property OEMtoWINTable: PChar read GetOEMtoWINTable;
{Returns the pointer to array of chars which is used for ANSI to OEM converting.
The length of array is always 128 chars.
The first element contains the code of #128 (dec) ANSI symbol in the OEM character set,
the second element contains the code of #129 (dec) ANSI symbol in the OEM character set and so on.}
    property WINtoOEMTable: PChar read GetWINtoOEMTable;
{Returns the TprTxBorderScheme object by its index.
See also:
  BorderSchemeCount}
    property BorderScheme[Index: Integer]: TprTxBorderScheme read GetBorderScheme;
{Returns the count of border schemas.
See also:
  BorderScheme}
    property BorderSchemeCount: Integer read GetBorderSchemeCount;
{Converts the string from ANSI to OEM.
Parameters:
  sSource - The source string.
  sDest - The destination string.}
    procedure WINtoOEM(sSource, sDest: PChar); overload;
    procedure WINtoOEM(const sSource: string; var sDest: string); overload;
    function WINtoOEM(sSource: char): char; overload;
{Converts the string from OEM to ANSI.
Parameters:
  sSource - The source string.
  sDest - The destination string.}
    procedure OEMtoWIN(sSource, sDest: PChar); overload;
    procedure OEMtoWIN(const sSource: string; var sDest: string); overload;
    function OEMtoWIN(sSource: char): char; overload;

{Searches the border scheme by its chars.
Return value:
  Returns the index of found scheme in the BorderScheme property or -1.}
    function IndexOfBorderScheme(ALeft, ALeftTop, ATop, ARightTop, ARight, ARightBottom, ABottom, ALeftBottom: Char): Integer;
  end;

  /////////////////////////////////////////////////
  //
  // TprESCModelItem
  //
  /////////////////////////////////////////////////
{Represents the separate item of the ESC model.
See also:
  TprESCModel}
  TprESCModelItem = class(TObject)
  private
    FTxCommand : TprTxCommand;
    FESCCommand : string;
  public
{Returns the TprTxCommand object.}
    property TxCommand: TprTxCommand read FTxCommand;
{Returns the native ESC command which should be used for command specified in the TxCommand property.}
    property ESCCommand: string read FESCCommand write FESCCommand;
  end;

  /////////////////////////////////////////////////
  //
  // TprESCModel
  //
  /////////////////////////////////////////////////
{Represents the ESC model of printer which is used for converting between TprTxReport commands and native printer commands.
The huge amount of printers' models exists in the world and the majority is have
specific sets of commands which can be used in a text mode, for example
For matrix printers of firm "EPSON" the command of inclusion italic a font consists of two symbols - #27#52,
For laser printers of firm "Hewlett Packard" - #27#40#115#49#83.
The concept ESC of model of the printer has been entered to provide printing of the TprTxReport reports on the various printers 
(with various commands' sets).
The TprESCModel class represents this ESC model.
This class contains properties and methods for converting the TprTxReport commands  (TprTxCommand class) to 
native commands of the printer (which are represented by sequence of bytes).
See also:
  TprTxCommand}
  TprESCModel = class(TObject)
  private
    FModelName : string;
    FPrinterDriver : string;
    FESCs : TList;
    function GetESC(i : integer) : TprESCModelItem;
  public
{Lists the TprESCModelItem objects in the list.
See also:
  TprESCModelItem}
    property ESCs[i: integer] : TprESCModelItem read GetESC;
{Returns the unique name of model, for example: <b>ESCModel_HP</b>.}
    property ModelName: string read FModelName;
{This property is used for searching the ESC model by the Windows printer driver name,
it contains a string which must be contained in the printer driver name.
For example, when user selects printer in the printer setup dialog
the TprTxReport searches automatically ESC model for selected printer with using of this property.}
    property PrinterDriver : string read FPrinterDriver;
{Returns the native printer command by the prefix of the TprTxCommand.
Parameters:
  Prefix - The prefix of the TprTxCommand object.
See also:
  TprTxCommand, TprTxCommand.Prefix}
    function GetRealESCCommandForESCPrefix(const Prefix: string) : string;
{Returns the native printer command by the name of TprTxCommand object.
Parameters:
  TxCommand - The name of the TprTxCommand object.
See also:
  TprTxCommand, TprTxCommand}
    function GetRealESCCommand(const TxCommand: string) : string;
{Returns the native printer command for form feed.}
    function GetFormFeedRealESCCommand: string;
{Creates an instance of the TprESCModel class.}
    constructor Create;
{Frees an instance of the TprESCModel class.}
    destructor Destroy; override;
  end;

{Type of the callback procedure which will be called each time when line of the report
will be printed.
Parameters:
  ALineNo - Specifies the number of current line in the report, zero for first line.
  ALineNoInPage - Specifies the number of current line within of current page, zero for first line.
  AWrapIndex - If line is wrapped then specifies the index of string part, typically this parameter equals to 0.
  S - The string to print.
  AFinishPrinting - Set this parameter to true to finish the printing.
  AData - The user-defined data.}
  TprPrintStringCallback = procedure (ALineNo: Integer; ALineNoInPage: Integer; AWrapIndex: Integer; var S: string; var AFinishPrinting: Boolean; AData: Pointer);

  /////////////////////////////////////////////////
  //
  // TprTxReportOptions
  //
  /////////////////////////////////////////////////
{Contains the list of registered ESC models, font options, font styles, record tables and so on.
Instance of this class is created automatically at programm startup and
is generated on the basis of content of the TxRo.ini file.
See also:
  TprTxCommand, TprESCModel, TprTxFontOption, TprTxFontStyle}
  TprTxReportOptions = class(TObject)
  private
    FBorderSchemas: TList;
    FRecodeTables : TList;
    FESCModels : TList;
    FTxCommands : TList;
    FTxFontStyles : TList;
    FTxFontOptions : TList;
    FDefaultRecodeTable : TprTxRecodeTable;
    function GetTxRecodeTable(i : integer) : TprTxRecodeTable;
    function GetESCModel(i : integer) : TprESCModel;
    function GetTxCommand(i : integer) : TprTxCommand;
    function GetTxFontStyle(i : integer) : TprTxFontStyle;
    function GetTxFontOption(i : integer) : TprTxFontOption;
    function GetTxRecodeTablesCount : integer;
    function GetESCModelsCount : integer;
    function GetTxCommandsCount : integer;
    function GetTxFontStylesCount : integer;
    function GetTxFontOptionsCount : integer;
    function GetBorderScheme(Index: Integer): TprTxBorderScheme;
    function GetBorderSchemeCount: Integer;

    procedure InternalLoadFromIni(Ini: TCustomIniFile);
  public
{Returns the TprTxBorderScheme object by its index.
See also:
  BorderSchemeCount}
    property BorderScheme[Index: Integer]: TprTxBorderScheme read GetBorderScheme;
{Returns the count of border schemas.
See also:
  BorderScheme}
    property BorderSchemeCount: Integer read GetBorderSchemeCount;
{Lists the registered recode tables.
Each recode table is represented in the TxRo.ini file as separate INI section.
Parameters:
  I - The index of recode table.
See also:
  TprTxRecodeTable, TxRecodeTablesCount}
    property RecodeTables[I: integer] : TprTxRecodeTable read GetTxRecodeTable;
{Returns the number of recode tables in the RecodeTables property.}
    property TxRecodeTablesCount : integer read GetTxRecodeTablesCount;
{Lists the registered ESC models.
Each ESC model is represented in the TxRo.ini file as separate INI section.
Parameters:
  I - The index of the ESC model.
See also:
  TprESCModel, ESCModelsCount}
    property ESCModels[i : integer] : TprESCModel read GetESCModel;
{Returns the number of items in the ESCModels property.
See also:
  ESCModels}
    property ESCModelsCount : integer read GetESCModelsCount;
{Lists the registered commands.
Parameters:
  I - The index of the command.
See also:
  TprTxCommand, TxCommandsCount}
    property TxCommands[i : integer] : TprTxCommand read GetTxCommand;
{Returns the number of items in the TxCommands property.
See also:
  TxCommands}
    property TxCommandsCount : integer read GetTxCommandsCount;
{Lists the registered font styles.
Parameters:
  I - The index of the font style.
See also:
  TprTxFontStyle, TxFontStylesCount}
    property TxFontStyles[i : integer] : TprTxFontStyle read GetTxFontStyle;
{Returns the number of items in the TxFontStyles property.
See also:
  TxFontStyles}
    property TxFontStylesCount : integer read GetTxFontStylesCount;
{Lists the registered font options.
Parameters:
  I - The index of the font option.
See also:
  TprTxFontOption, TxFontOptionsCount}
    property TxFontOptions[i : integer] : TprTxFontOption read GetTxFontOption;
{Returns the number of items in the TxFontOptions property.
See also:
  TxFontOptions}
    property TxFontOptionsCount: integer read GetTxFontOptionsCount;
{Specifies the default record table, this table will be used if value of
TprTxReport.RecodeTableName property is not specified.
See also:
  TprTxRecodeTable}
    property DefaultRecodeTable: TprTxRecodeTable read FDefaultRecodeTable;

{Returns the index of the TprTxFontStyle object in the TxFontStyles property.
Return value:
  Returns index of object or -1 if object is not found.}
    function IndexOfTxFontStyle(TxFontStyle: TprTxFontStyle): Integer;
{Returns the index of the TprTxFontOption object in the TxFontOptions property
by the name of TprTxFontOption object.
Parameters:
  TxFontOptionName - The name of TprTxFontOption object
Return value:
  Returns index of object or -1 if object is not found.}
    function IndexOfTxFontOption(const TxFontOptionName: string) : integer;
{Searches the TprTxRecodeTable object by name.
Parameters:
  RecodeTableName - The name of the recode table.
Return value:
  Returns the found TprTxRecodeTable object or nil.}
    function FindRecodeTable(const RecodeTableName : string) : TprTxRecodeTable;
{Searches the TprESCModel object by the name of the Windows printer driver.
Parameters:
  DriverName - The name of the Windows printer driver.
Return value:
  Returns the found TprESCModel object or nil.}
    function FindESCModelByDriverName(const DriverName : string) : TprESCModel;
{Searches the TprESCModel object by the name.
Parameters:
  DriverName - The name of the TprESCModel.
Return value:
  Returns the found TprESCModel object or nil.}
    function FindESCModel(const ESCModelName : string) : TprESCModel;
{Searches the TprTxCommand object by name.
Parameters:
  TxCommandName - The value of the TprTxCommand.Name property.
Return value:
  Returns the found TprTxCommand object or nil.}
    function FindTxCommand(const TxCommandName : string) : TprTxCommand;
{Searches the TprTxFontStyle object by name.
Parameters:
  TxFontStyle - The value of the TprTxFontStyle.Name property.
Return value:
  Returns the found TprTxFontStyle object or nil.}
    function FindTxFontStyle(const TxFontStyle : string) : TprTxFontStyle;
{Searches the TprTxFontOption object by name.
Parameters:
  TxFontOption - The value of the TprTxFontOption.Name property.
Return value:
  Returns the found TprTxFontOption object or nil.}
    function FindTxFontOption(const TxFontOption : string) : TprTxFontOption;
{Returns the index of the TprESCModel object in the ESCModels property by name.
Parameters:
  ModelName - The value of the TprESCMode.Name property.
Return value:
  Returns the index of object or -1 if object is not found.}
    function ESCModelIndexByModelName(const ModelName : string) : integer;

{Returns the command's prefix for TprTxFontStyle object.
See also:
  TprTxFontStyle, TprTxCommand.Prefix}
    function GetTxFontStyleESCPrefix(TxFontStyle: TprTxFontStyle): string;
{Returns the commands' prefixes which switchs ON the font options which are specified
by FontOptions parameter.
See also:
  TprTxFontOption, TprTxFontOptions, TprTxCommand.Prefix}
    function GetTxFontOptionsESCPrefixOn(FontOptions: TprTxFontOptions): string;
{Returns the commands' prefixes which switchs OFF the font options which are specified
by FontOptions parameter.
See also:
  TprTxFontOption, TprTxFontOptions, TprTxCommand.Prefix}
    function GetTxFontOptionsESCPrefixOff(FontOptions: TprTxFontOptions): string;
{Returns the command's prefix which switchs ON the font option which are specified
by TxFontOption parameter.
See also:
  TprTxFontOption, TprTxCommand.Prefix}
    function GetTxFontOptionESCPrefixOn(TxFontOption: TprTxFontOption): string;
{Returns the command's prefix which switchs OFF the font option which are specified
by TxFontOption parameter.
See also:
  TprTxFontOption, TprTxCommand.Prefix}
    function GetTxFontOptionESCPrefixOff(TxFontOption: TprTxFontOption): string;
{Returns the full command's prefix for form feed.}
    function GetFormFeedFullESCPrefix: string;
{Returns the command's prefix for form feed.}
    function GetFormFeedESCPrefix: string;

{Searches the TprESCModel object which corresponds to the specified printer.
Parameters:
  PrinterName - The name of printer.
Return value:
  Returns the fount TprESCModel object or nil.}
    function GetESCModelForPrinter(const PrinterName: string): TprESCModel;
{Removes all commands from the string, for example if string contains:<br>
#27I"My report"#27"i"<br>
then this function returns:
"My report".}
    function RemoveESCFromString(const s: string): string;
{Skips the specified number of commands in the string an returns the position in the string.}
    function ESCSkipTo(const s : string; SkipCount : integer; var p : integer) : integer;
{Prints the strings on the printer in text mode. This procedure called by the TprTxReport component,
from PrintPreparedReport method. The strings can contains TprTxReport commands.
Parameters:
  Lines - strings to print.
  PrinterName - The printer.
  ESCModelName - The name of ESC model, which is used to convert the TprTxReport commands to native printer commands.
This parameter can be empty, in this case the ESC model will be selected automatically.
  ReportTitle - The title of print job.
  Copies - The number of copies to print.
  WrapAfterColumn - Specifies the number of chars per column, can be used if the report width is more than width of paper.
If this parameter is less than 1 it is ignored.
  LeftSpaces - Specifies the left margin in chars.
  PaperType - Specifies the paper type - pages or roll.
  PrintRulonMode - Specifies the part of strings to print if roll is used - all lines or specified range.
  MakeFormFeedOnRulon - Indicates whether the form feed for each page must be done if roll is used.
  FromLine, ToLine - Specifies the part of strings which must be printed if
PaperType equals to ptRulon and PrintRulonMode equals to prmLinesRange, from 1 to Lines.Count.
  PrintPagesMode - Specifies the part of strings to prin is the pages are used (PaperType equals to ptPage).
  LinesOnPage - Specifies the number of lines per page if pages list must be formed automatically (used if UseLinesOnPage is true).
  UseLinesOnPage - Indicates whether the pages list must be formed automatically.
  PrintPagesList - Specifies the list of numbers of pages to print, is used if
PaperType equals to ptPage and PrintPagesMode equals to ppmPagesList.
  FromPage, ToPage - Specifies the pages which must be printed if
PaperType equals to ptPage and PrintRulonMode equals to prmPagesRange, from 1.
  APrintingPage - This variable will be changed each time when new page is started.
  APrintingCopy - This variable will be changed each time when new copy is started.
  RecodeTable - The TprTxRecodeTable object, which is used to convert strings from ANSI to OEM, can
be NIL, in this case - converting is not applied.
  StartNewLineOnWrap - Indicates whether new line must be started when string is wrapped or strings
must be printed in few columns.
  EjectPageAfterPrint - Indicates whether the form feed must be done when printing is finished.
  APrintStringCallback - Pointer to the callback function, which is called each time before printing of string.
  APrintStringCallbackData - The used-defined data which will be passed to the APrintStringCallback function.
Return value:
  Returns the true if the printing finished successfully.}
    function TxPrintStrings(Lines: TStrings;
                            const PrinterName: string; // name of printer
                            const ESCModelName: string; // ESC model of printer
                            const ReportTitle: string; // name of task in Spooler Windows
                            Copies: integer;
                            WrapAfterColumn: Integer;
                            LeftSpaces: Integer;
                            PaperType: TprTxPaperType; // Roller or Pages
                            PrintRulonMode: TprTxPrintRulonMode;
                            MakeFormFeedOnRulon: Boolean;
                            FromLine: Integer;
                            ToLine: Integer;
                            PrintPagesMode: TprPrintPagesMode;
                            LinesOnPage: Integer;
                            UseLinesOnPage: Boolean;
                            PrintPagesList: TList;
                            FromPage: integer;
                            ToPage: integer;
                            var APrintingPage: Integer;
                            var APrintingCopy: Integer;
                            RecodeTable: TprTxRecodeTable=nil;
                            StartNewLineOnWrap: boolean=false;
                            EjectPageAfterPrint: boolean=false;
                            APrintStringCallback: TprPrintStringCallback = nil;
                            APrintStringCallbackData: Pointer = nil
                            ) : boolean;
{Opens the built-in "Print setup" dialog. This procedure called by the TprTxReport component,
from SetupPrintParams method.
Parameters:
  hWndOwner - The handle of parent window.
  MaxPage - Specifies the maximum value for the page range specified in the From and To page edit controls.
  MaxLines - Specifies the maximum value for the line range specified in the From and To line edit controls.
  LinesOnPage - Specifies the initial value for the lines on page edit control.
  FromPage - Specifies the initial value for the starting page edit control.
When PrintDlg returns, nFromPage is the starting page specified by the user.
  ToPage - Specifies the initial value for the ending page edit control.
When PrintDlg returns, nToPage is the ending page specified by the user.
Return value:
  Returns the true if user press "OK" button in the dialog.}
    function TxSetupPrintParams(hWndOwner : THandle; 
                                MaxPage : integer;
                                MaxLines : integer;
                                var LinesOnPage : integer;
                                var FromPage : integer;
                                var ToPage : integer;
                                var PrintPagesMode : TprPrintPagesMode;
                                var ESCModelName : string;
                                var PrinterName : string;
                                var UseLinesOnPage : boolean;
                                var WrapAfterColumn : integer;
                                var MakeFormFeedOnRulon : boolean;
                                var LeftSpaces : integer;
                                var PrintRulonMode : TprTxPrintRulonMode;
                                var FromLine : integer;
                                var ToLine : integer;
                                var PrintPages : string;
                                PrintPagesList : TList;
                                var PaperType : TprTxPaperType;
                                var Copies : integer;
                                var EjectPageAfterPrint : boolean): Boolean;
{Returns the string length without commands. For example if string contains:<br>
#27I"My report"#27"i"<br> then 9 will be returned.}
    function ESCStringLengthWithoutESC(const s : string) : integer;
{Forms the list of pages for specified strings.
New page is detected when:<br>
- The form feed command is found.<br>
- The number of successively going lines more than is determined in the LinesOnPage parameter (used if UseLinesOnPage equals to true).<br>
Parameters:
  Lines - The strings list to parse.
  UseLinesOnPage - Indicates whether the LinesOnPage parameter must be used for forming the pages list.
  LinesOnPage - Specifies the maximum number of strings per page.
  PagesList - Must be not NIL, receives the number of strings for each page.
Return value:
  Returns the number of formed pages.
Example:
  var
    I: Integer;
    PagesList: TList;
    APageCount: Integer;
  begin
    PagesList := TList.Create;
    try
      // max is 40 lines per page
      APageCount := ParsePages(MyStrings, true, 40, PagesList);
      writeln('Count of pages = ' + IntToStr(APageCount));
      for I := 0 to PagesList.Count - 1 do
        writeln(Format('Number of lines in the %d page = %d', [i + 1, integer(PagesList[i])]));
    finally
      PagesList.Free;
    end;
  end;}
    function ParsePages(Lines: TStrings;
                        UseLinesOnPage: boolean;
                        LinesOnPage: integer;
                        PagesList: TList) : integer;

{Loads the content of this TprTxReportOptions object from INI file.
Parameters:
  FileName - The name of INI file.}
    procedure LoadFromFile(const FileName : string);
{Loads the content of this TprTxReportOptions object from resources.
Parameters:
  FileName - The name of INI file.}
    procedure LoadFromRes;

{Creates an instance of the TprTxReportOptions class.}
    constructor Create;
{Frees an instance of the TprTxReportOptions class.}
    destructor Destroy; override;
  end;

var
{Contains the reference to the TprTxReportOptions object which was created at programm startup.
See also:
  TprTxReportOptions}
  TxReportOptions: TprTxReportOptions;
{Contains the full name of the TxRO.ini file wich was loaded at programm startup.}
  prTxReportOptionsFileName: string;
                                                                                                                                                                                                                                                                                  {249}
implementation

uses
  vgr_Functions, pr_Utils, pr_Strings, pr_MultiLang;

{$R ..\Res\prTxRO.res}

const
_sTxReportOptionsFileName = 'TxRO.ini';
sFormFeedTxCommand = 'txcFormFeed';
sESCModelCanNotBeFound = 'Can not found ESC model for printer [%s]';

type
/////////////////////////////////////////////////
//
// rSetupPrintParams
//
/////////////////////////////////////////////////
rSetupPrintParams = record
  MaxLines : integer;
  PrintPagesMode : TprPrintPagesMode;
  ESCModelName : string;
  PrinterName : string;
  UseLinesOnPage : boolean;
  WrapAfterColumn : integer;
  MakeFormFeedOnRulon : boolean;
  LeftSpaces : integer;
  PrintRulonMode : TprTxPrintRulonMode;
  FromLine : integer;
  ToLine : integer;
  PrintPages : string;
  PrintPagesList : TList;
  PaperType : TprTxPaperType;
  LinesOnPage : integer;
  EjectPageAfterPrint : boolean;
  Copies : integer;
end;
pSetupPrintParams = ^rSetupPrintParams;

/////////////////////////////////////////////////
//
// TprStreamIniFile
//
/////////////////////////////////////////////////
TprStreamIniFile = class(TMemIniFile)
public
  constructor Create(AStream: TStream);
end;

constructor TprStreamIniFile.Create(AStream: TStream);
var
  List: TStringList;
begin
  inherited Create('');

  List := TStringList.Create;
  try
    List.LoadFromStream(AStream);
    SetStrings(List);
  finally
    List.Free;
  end;
end;

/////////////////////////////////////////////////
//
// TprTxFontOptions
//
/////////////////////////////////////////////////
function TprTxFontOptions.GetItm(i : integer) : TprTxFontOption;
begin
Result := TprTxFontOption(inherited Items[i]);
end;

procedure TprTxFontOptions.Assign(Source : TprTxFontOptions);
var
  i : integer;
begin
Clear;
for i:=0 to Source.Count-1 do
  Add(Source[i]);
end;

function TprTxFontOptions.Equals(Value : TprTxFontOptions) : boolean;
var
  i : integer;
begin
Result := Count=Value.Count;
if Result then
  begin
    i := 0;
    while (i<Count) and (Items[i]=Value[i]) do Inc(i);
    Result := i>=Count;
  end;
end;

function TprTxFontOptions.Contains(TxFontOption : TprTxFontOption) : boolean;
begin
Result := IndexOf(TxFontOption)<>-1;
end;

procedure TprTxFontOptions.Remove(TxFontOption : TprTxFontOption);
var
  i : integer;
begin
i := IndexOf(TxFontOption);
if i<>-1 then
  Delete(i);
end;

function TprTxFontOptions.Add(TxFontOption : TprTxFontOption) : integer;
begin
if IndexOf(TxFontOption)=-1 then
  Result := inherited Add(TxFontOption)
else
  Result := -1;
end;

/////////////////////////////////////////////////
//
// TprTxBorderScheme
//
/////////////////////////////////////////////////
constructor TprTxBorderScheme.Create(ACaption: string; ABrdLeft, ABrdLeftTop, ABrdTop, ABrdRightTop, ABrdRight, ABrdRightBottom, ABrdBottom, ABrdLeftBottom: Char);
begin
  inherited Create;
  FCaption := ACaption;
  FBrdLeft := Char(ABrdLeft);
  FBrdLeftTop := Char(ABrdLeftTop);
  FBrdTop := Char(ABrdTop);
  FBrdRightTop := Char(ABrdRightTop);
  FBrdRight := Char(ABrdRight);
  FBrdRightBottom := Char(ABrdRightBottom);
  FBrdBottom := Char(ABrdBottom);
  FBrdLeftBottom := Char(ABrdLeftBottom);
end;

/////////////////////////////////////////////////
//
// TprTxCodePage
//
/////////////////////////////////////////////////
constructor TprTxRecodeTable.Create;
begin
  inherited Create;
  FBorderSchemas := TList.Create;
end;

destructor TprTxRecodeTable.Destroy;
begin
  FreeList(FBorderSchemas);
  inherited;
end;

function TprTxRecodeTable.GetBorderScheme(Index: Integer): TprTxBorderScheme;
begin
  Result := TprTxBorderScheme(FBorderSchemas[Index]);
end;

function TprTxRecodeTable.GetBorderSchemeCount: Integer;
begin
  Result := FBorderSchemas.Count;
end;

function TprTxRecodeTable.GetOEMtoWINTable : PChar;
begin
  Result := @FOEMtoWINTable[0];
end;

function TprTxRecodeTable.GetWINtoOEMTable : PChar;
begin
  Result := @FWINtoOEMTable[0];
end;

procedure TprTxRecodeTable.Decode(sSource, sDest, Table: PChar);
var
  I: Integer;
begin
  for I := 0 to lstrlen(sSource) - 1 do
    if byte(sSource[I]) >= 128 then
      sDest[I] := Table[byte(sSource[I]) - 128];
end;

procedure TprTxRecodeTable.WINtoOEM(sSource,sDest : PChar);
begin
  Decode(sSource, sDest, WINtoOEMTable);
end;

procedure TprTxRecodeTable.WINtoOEM(const sSource: string; var sDest: string);
begin
  SetLength(sDest, Length(sSource));
  MoveMemory(@sDest[1], @sSource[1], Length(sSource));
  Decode(PChar(sDest), PChar(sDest), WINtoOEMTable);
end;
 
function TprTxRecodeTable.WINtoOEM(sSource: char): char;
begin
  if byte(sSource) >= 128  then
    Result := WINtoOEMTable[byte(sSource) - 128]
  else
    Result := sSource;
end;

procedure TprTxRecodeTable.OEMtoWIN(sSource,sDest : PChar);
begin
  Decode(sSource, sDest, OEMtoWINTable);
end;

procedure TprTxRecodeTable.OEMtoWIN(const sSource: string; var sDest: string);
begin
  SetLength(sDest, Length(sSource));
  MoveMemory(@sDest[1], @sSource[1], Length(sSource));
  Decode(PChar(sDest), PChar(sDest), OEMtoWINTable);
end;

function TprTxRecodeTable.OEMtoWIN(sSource: char): char;
begin
  if byte(sSource) >= 128  then
    Result := OEMtoWINTable[byte(sSource) - 128]
  else
    Result := sSource;
end;

function TprTxRecodeTable.IndexOfBorderScheme(ALeft, ALeftTop, ATop, ARightTop, ARight, ARightBottom, ABottom, ALeftBottom: Char): Integer;
begin
  for Result := 0 to BorderSchemeCount - 1 do
    with BorderScheme[Result] do
      if (BrdLeft = ALeft) and (BrdLeftTop = ALeftTop) and (BrdTop = ATop) and (BrdRightTop = ARightTop) and
         (BrdRight = ARight) and (BrdRightBottom = ARightBottom) and (BrdBottom = ABottom) and (BrdLeftBottom = ALeftBottom) then
        exit;
  Result := -1;
end;

/////////////////////////////////////////////////
//
// TprESCModel
//
/////////////////////////////////////////////////
constructor TprESCModel.Create;
begin
inherited Create;
FESCs := TList.Create;
end;

destructor TprESCModel.Destroy;
begin
FreeListItems(FESCs);
FESCs.Destroy;
inherited;
end;

function TprESCModel.GetESC(i : integer) : TprESCModelItem;
begin
Result := TprESCModelItem(FESCs[i]);
end;

function TprESCModel.GetRealESCCommandForESCPrefix(const Prefix : string) : string;
var
  i : integer;
begin
i := 0;
while (i<FESCs.Count) and ((ESCs[i].TxCommand=nil) or (ESCs[i].TxCommand.Prefix<>Prefix)) do Inc(i);
if (i<FESCs.Count) and (ESCs[i].TxCommand<>nil) then
  Result := ESCs[i].ESCCommand
else
  Result := '';
end;

function TprESCModel.GetRealESCCommand(const TxCommand : string) : string;
var
  i : integer;
begin
Result := '';
i := 0;
while (i<FESCs.Count) and ((ESCs[i].TxCommand=nil) or (AnsiCompareText(ESCs[i].TxCommand.Name,TxCommand)<>0)) do Inc(i);
if (i<FESCs.Count) and (ESCs[i].TxCommand<>nil) then
  Result := ESCs[i].ESCCommand
else
  Result := '';
end;

function TprESCModel.GetFormFeedRealESCCommand : string;
begin
Result := GetRealESCCommand(sFormFeedTxCommand);
end;

/////////////////////////////////////////////////
//
// TprTxReportOptions
//
/////////////////////////////////////////////////
constructor TprTxReportOptions.Create;
begin
  inherited;
  FRecodeTables := TList.Create;
  FESCModels := TList.Create;
  FTxCommands := TList.Create;
  FTxFontStyles := TList.Create;
  FTxFontOptions := TList.Create;
  FBorderSchemas := TList.Create;
end;

destructor TprTxReportOptions.Destroy;
begin
  FreeList(FRecodeTables);
  FreeList(FESCModels);
  FreeList(FTxCommands);
  FreeList(FTxFontStyles);
  FreeList(FTxFontOptions);
  FreeList(FBorderSchemas);
  inherited;
end;

function TprTxReportOptions.GetTxRecodeTable(i : integer) : TprTxRecodeTable;
begin
Result := TprTxRecodeTable(FRecodeTables[i]);
end;

function TprTxReportOptions.GetESCModel(i : integer) : TprESCModel;
begin
Result := TprESCModel(FESCModels[i]);
end;

function TprTxReportOptions.GetTxCommand(i : integer) : TprTxCommand;
begin
Result := TprTxCommand(FTxCommands[i]);
end;

function TprTxReportOptions.GetTxFontStyle(i : integer) : TprTxFontStyle;
begin
Result := TprTxFontStyle(FTxFontStyles[i]);
end;

function TprTxReportOptions.GetTxFontOption(i : integer) : TprTxFontOption;
begin
Result := TprTxFontOption(FTxFontOptions[i]);
end;

function TprTxReportOptions.GetTxCommandsCount : integer;
begin
Result := FTxCommands.Count;
end;

function TprTxReportOptions.GetTxRecodeTablesCount : integer;
begin
Result := FRecodeTables.Count;
end;

function TprTxReportOptions.GetESCModelsCount : integer;
begin
Result := FESCModels.Count;
end;

function TprTxReportOptions.GetTxFontStylesCount : integer;
begin
Result := FTxFontStyles.Count;
end;

function TprTxReportOptions.GetTxFontOptionsCount : integer;
begin
Result := FTxFontOptions.Count;
end;

function TprTxReportOptions.GetBorderScheme(Index: Integer): TprTxBorderScheme;
begin
  Result := TprTxBorderScheme(FBorderSchemas[Index]);
end;

function TprTxReportOptions.GetBorderSchemeCount: Integer;
begin
  Result := FBorderSchemas.Count;
end;

function TprTxReportOptions.GetTxFontStyleESCPrefix(TxFontStyle : TprTxFontStyle) : string;
begin
if TxFontStyle=nil then
  Result := ''
else
  Result := ESCSymbol+TxFontStyle.TxCommand.FullPrefix
end;

function TprTxReportOptions.GetTxFontOptionsESCPrefixOn(FontOptions : TprTxFontOptions) : string;
var
  i : integer;
begin
Result := '';
for i:=0 to FontOptions.Count-1 do
  Result := Result+ESCSymbol+FontOptions[i].TxCommandOn.FullPrefix;
end;

function TprTxReportOptions.GetTxFontOptionsESCPrefixOff(FontOptions : TprTxFontOptions) : string;
var
  i : integer;
begin
Result := '';
for i:=0 to FontOptions.Count-1 do
  Result := ESCSymbol+FontOptions[i].TxCommandOff.FullPrefix+Result;
end;

function TprTxReportOptions.GetTxFontOptionESCPrefixOn(TxFontOption : TprTxFontOption) : string;
begin
Result := ESCSymbol+TxFontOption.TxCommandOn.FullPrefix;
end;

function TprTxReportOptions.GetTxFontOptionESCPrefixOff(TxFontOption : TprTxFontOption) : string;
begin
Result := ESCSymbol+TxFontOption.TxCommandOff.FullPrefix;
end;

function TprTxReportOptions.IndexOfTxFontStyle(TxFontStyle : TprTxFontStyle) : integer;
begin
Result := FTxFontStyles.IndexOf(TxFontStyle);
end;

function TprTxReportOptions.IndexOfTxFontOption(const TxFontOptionName : string) : integer;
begin
Result := 0;
while (Result<TxFontOptionsCount) and (AnsiCompareText(TxFontOptions[Result].Name,TxFontOptionName)<>0) do Inc(Result);
if Result>=TxFontOptionsCount then
  Result := -1;
end;

function TprTxReportOptions.FindRecodeTable(const RecodeTableName : string) : TprTxRecodeTable;
var
  i : integer;
begin
i := 0;
while (i<FRecodeTables.Count) and (AnsiCompareText(RecodeTables[i].RecodeTableName,RecodeTableName)<>0) do Inc(i);
if i>=FRecodeTables.Count then
  Result := nil
else
  Result := RecodeTables[i];
end;

function TprTxReportOptions.FindESCModelByDriverName(const DriverName : string) : TprESCModel;
var
  i : integer;
begin
i := 0;
while (i<ESCModelsCount) and (pos(AnsiUpperCase(ESCModels[i].PrinterDriver),AnsiUpperCase(DriverName))=0) do Inc(i);
if i<ESCModelsCount then
  Result := ESCModels[i]
else
  Result := nil;
end;

function TprTxReportOptions.FindESCModel(const ESCModelName : string) : TprESCModel;
var
  i : integer;
begin
i := 0;
while (i<ESCModelsCount) and (AnsiCompareText(ESCModels[i].ModelName,ESCModelName)<>0) do Inc(i);
if i<ESCModelsCount then
  Result := ESCModels[i]
else
  Result := nil;
end;

function TprTxReportOptions.FindTxCommand(const TxCommandName : string) : TprTxCommand;
var
  i : integer;
begin
i := 0;
while (i<TxCommandsCount) and (AnsiCompareText(TxCommands[i].Name,TxCommandName)<>0) do Inc(i);
if i>=TxCommandsCount then
  Result := nil
else
  Result := TxCommands[i];
end;

function TprTxReportOptions.FindTxFontStyle(const TxFontStyle : string) : TprTxFontStyle;
var
  i : integer;
begin
i := 0;
while (i<TxFontStylesCount) and (AnsiCompareText(TxFontStyles[i].Name,TxFontStyle)<>0) do Inc(i);
if i>=TxFontStylesCount then
  Result := nil
else
  Result := TxFontStyles[i];
end;

function TprTxReportOptions.FindTxFontOption(const TxFontOption : string) : TprTxFontOption;
var
  i : integer;
begin
i := 0;
while (i<TxFontOptionsCount) and (AnsiCompareText(TxFontOptions[i].Name,TxFontOption)<>0) do Inc(i);
if i>=TxFontOptionsCount then
  Result := nil
else
  Result := TxFontOptions[i];
end;

function TprTxReportOptions.ESCModelIndexByModelName(const ModelName : string) : integer;
begin
Result := 0;
while (Result<ESCModelsCount) and (AnsiCompareText(ESCModels[Result].ModelName,ModelName)<>0) do Inc(Result);
if Result>=ESCModelsCount then
  Result := -1;
end;

function TprTxReportOptions.RemoveESCFromString(const s : string) : string;
var
  j,l,i : integer;
begin
l := Length(s);
j := 1;
i := 1;
SetLength(Result,l);
while j<=l do
  begin
    if s[j]=ESCSymbol then
      begin
        Inc(j);
        if s[j]=ESCSymbolForExCommands then
          j := j+3;
      end
    else
      if s[j]=ESCCustomCommandSymbol then
        repeat
          Inc(j);
        until (j>l) or (s[j]=ESCCustomCommandSymbol)
      else
        begin
          Result[i] := s[j];
          Inc(i);
        end;
    Inc(j);
  end;
SetLength(Result,i-1);
end;

function TprTxReportOptions.ESCSkipTo(const s : string; SkipCount : integer; var p : integer) : integer;
var
  LenStr : integer;
begin
Result := 0;
LenStr := Length(s);
while (p<=LenStr) and (Result<SkipCount) do
  begin
    if s[p]=ESCSymbol then
      begin
        Inc(p);
        if s[p]=ESCSymbolForExCommands then
          p := p+3;
      end
    else
      if s[p]=ESCCustomCommandSymbol then
        repeat
          Inc(p);
        until (p>LenStr) or (s[p]=ESCCustomCommandSymbol)
      else
        Inc(Result);
    Inc(p);
  end;
end;

function TprTxReportOptions.GetESCModelForPrinter(const PrinterName : string) : TprESCModel;
var
  Buf,Buffer : PChar;
  i,BytesNeeded,NumInfo : cardinal;
begin
Result := nil;
Buffer := nil;
BytesNeeded := 0;
EnumPrinters(PRINTER_ENUM_LOCAL or PRINTER_ENUM_CONNECTIONS,nil,2,Buffer,0,BytesNeeded,NumInfo);
if BytesNeeded<>0 then
  begin
    GetMem(Buffer,BytesNeeded);
    try
      if EnumPrinters(PRINTER_ENUM_LOCAL or PRINTER_ENUM_CONNECTIONS,nil,2,Buffer,BytesNeeded,BytesNeeded,NumInfo) then
        begin
          i := 0;
          Buf := Buffer;
          while (i<NumInfo) and (StrPas(PPrinterInfo2(Buf).pPrinterName)<>PrinterName) do
            begin
              Inc(i);
              Buf := Buf+sizeof(TPrinterInfo2);
            end;
          if i<NumInfo then
            Result := FindESCModelByDriverName(StrPas(PPrinterInfo2(Buf).pDriverName));
        end;
    finally
      FreeMem(Buffer);
    end;
  end;
end;

function TprTxReportOptions.TxPrintStrings;
label
  lb_StopRoll;

var
  h : Cardinal;
  DocInfo : DOC_INFO_1;
  ESCModel : TprESCModel;
  PagesList : TList;
  EscCommand,FormFeedESCPrefix : string;
  PrinterOpened,DocStarted,PageStarted : boolean;
  wc,WrapCount,i,j,k,l,ml,_FromLine,_ToLine : integer;

  // write string part
  function PrintLine(ALineNo: Integer; ALineNoInPage: Integer; const s: string; wc: integer; PrintFormFeed: boolean): Boolean;
  var
    b2 : string;
    i,l,sl,el,n : integer;
    BytesWritten : cardinal;
    AFinishPrinting: Boolean;
  begin
    Result := True;
    l := Length(s);
    if WrapAfterColumn < 1 then
    begin
      sl := 1;
      el := l + 1;
    end
    else
    begin
      i := 1;
      ESCSkipTo(s,wc*WrapAfterColumn,i);
      sl := i;
      ESCSkipTo(s,WrapAfterColumn,i);
      el := i;
    end;
    if (sl>l) and ((l>0) or (wc>0)) and StartNewLineOnWrap then
      exit;
  
    i := 1;
    b2 := '';
    while i<=l do
    begin
      if s[i]=ESCSymbol then
        begin
          // get real ESC command for this ESC specifier and ESC model
          Inc(i);
          if s[i]=ESCSymbolForExCommands then
            begin
              EscCommand := Copy(s,i,3);
              i := i+3;
            end
          else
            EscCommand := s[i];
          if PrintFormFeed or (EscCommand<>FormFeedESCPrefix) then
            b2 := b2+ESCModel.GetRealESCCommandForESCPrefix(EscCommand);
        end
      else
        if s[i] = ESCCustomCommandSymbol then
          begin
            n := i;
            repeat
              Inc(i);
            until (i > l) or (s[i] = ESCCustomCommandSymbol);
            b2 := b2+Copy(s, n + 1, i - n - 1);
          end
        else
          if (i>=sl) and (i<el) then
            b2 := b2+s[i];
      Inc(i);
    end;

    AFinishPrinting := False;
    if Assigned(APrintStringCallback) then
      APrintStringCallback(ALineNo, ALineNoInPage, wc, b2, AFinishPrinting, APrintStringCallbackData);
    Result := not AFinishPrinting;

    if Result then
    begin
      if RecodeTable <> nil then
        RecodeTable.WINtoOEM(PChar(b2), PChar(b2));

      b2 := b2 + #13#10;
      WritePrinter(h, @(b2[1]), Length(b2), BytesWritten);
    end;
  end;

  procedure CheckStartPagePrinter;
  begin
  PageStarted:=StartPagePrinter(h);
  if not PageStarted then
    raise Exception.CreateFmt(prLoadStr(sErrorTxStartPage),[SysErrorMessage(GetLastError)]);
  end;

  procedure EjectPage;
  var
    b : string;
    BytesWritten : cardinal;
  begin
  b := ESCModel.GetFormFeedRealESCCommand;
  WritePrinter(h,@(b[1]),Length(b),BytesWritten);
  end;

  procedure PrintPage(PageIndex : integer);
  var
    i,j,l : integer;
  begin
  if PageIndex>=PagesList.Count-1 then
    l := Lines.Count
  else
    l := integer(PagesList[PageIndex+1]);

  if StartNewLineOnWrap then
    begin
      for i := integer(PagesList[PageIndex]) to l - 1 do
        for j := 0 to WrapCount do
          if not PrintLine(i, i - integer(PagesList[PageIndex]), Lines[i], j, false) then
            exit; 
      EjectPage;
    end
  else
    begin
      for j:=0 to WrapCount do
        begin
          for i := integer(PagesList[PageIndex]) to l - 1 do
            if not PrintLine(i, i - integer(PagesList[PageIndex]), Lines[i], j, false) then
              exit;
          EjectPage;
        end;
    end;
  end;

begin
ESCModel := nil;
if ESCModelName='' then
  begin
    if PrinterName<>'' then
      ESCModel := GetESCModelForPrinter(PrinterName);
  end
else
  ESCModel := FindESCModel(ESCModelName);
if ESCModel = nil then
  raise Exception.CreateFmt(sESCModelCanNotBeFound, [PrinterName]);
FormFeedESCPrefix := GetFormFeedESCPrefix;

Result := false;
PrinterOpened := false;
DocStarted := false;
PageStarted := false;
PagesList := TList.Create;
try
  if not OpenPrinter(PChar(PrinterName),h,nil) then
    raise Exception.CreateFmt(prLoadStr(sErrorTxOpenPrinter),[SysErrorMessage(GetLastError)]);
  PrinterOpened := true;

  DocInfo.pDocName := PChar(ReportTitle);
  DocInfo.pOutputFile := nil;
  DocInfo.pDatatype := 'RAW';
  if StartDocPrinter(h,1,@DocInfo)=0 then
    raise Exception.CreateFmt(prLoadStr(sErrorTxStartDoc),[SysErrorMessage(GetLastError)]);
  DocStarted := true;

  CheckStartPagePrinter;

  WrapCount := 0;
  if WrapAfterColumn>1 then
    begin
      ml := ESCStringLengthWithoutESC(Lines[0]);
      for i:=1 to Lines.Count-1 do
        begin
          l := ESCStringLengthWithoutESC(Lines[i]);
          if ml<l then
            ml := l;
        end;

      WrapCount := ml div WrapAfterColumn;
      if (ml mod WrapAfterColumn)=0 then
        Dec(WrapCount);
      if WrapCount=-1 then
        WrapCount := 0;
    end;

  if PaperType=ptPage then
    ParsePages(Lines, UseLinesOnPage, LinesOnPage, PagesList);

  case PaperType of
    ptRulon:
      begin
        APrintingPage := 1;
        _FromLine := 0;
        _ToLine := Lines.Count-1;
        case PrintRulonMode of
          prmLinesRange:
            begin
              _FromLine := Min(_ToLine, Max(_FromLine, FromLine - 1));
              _ToLine := Max(_FromLine, Min(_ToLine, ToLine - 1));
            end;
        end;
        if Copies <= 0 then
          Copies := 1;

        for I := 1 to Copies do
        begin
          APrintingCopy := I;
          if StartNewLineOnWrap then
          begin
            for J := _FromLine to _ToLine do
              for wc := 0 to WrapCount do
                if not PrintLine(J, J, Lines[j], wc, MakeFormFeedOnRulon) then
                  goto lb_StopRoll
          end
          else
          begin
            for wc := 0 to WrapCount do
              for J := _FromLine to _ToLine do
                if not PrintLine(J, J, Lines[j], wc, MakeFormFeedOnRulon) then
                  goto lb_StopRoll
          end;
        end;
lb_StopRoll:
      end;

    ptPage:
      begin
      for J := 1 to Copies do
        begin
          APrintingCopy := J;
          case PrintPagesMode of
            ppmAll, ppmPagesRange:
              begin
                i := 0;
                k := PagesList.Count - 1;
                if PrintPagesMode = ppmPagesRange then
                begin
                  i := Min(k, Max(FromPage - 1, i));
                  k := Max(i, Min(k, ToPage - 1));
                end;
                for i := i to k do
                begin
                  APrintingPage := I + 1;
                  PrintPage(i);
                end;
              end;
            ppmPagesList:
              begin
                for i := 0 to PrintPagesList.Count - 1 do
                begin
                  APrintingPage := integer(PrintPagesList[i]);
                  if (APrintingPage > 0) and (APrintingPage <= PagesList.Count) then
                    PrintPage(APrintingPage - 1);
                end;
              end;
          end;
        end;
      end;
  end;

  if EjectPageAfterPrint then
    EjectPage;

  Result := true;

finally
  PagesList.Free;
  if PageStarted then
    EndPagePrinter(h);
  if DocStarted then
    EndDocPrinter(h);
  if PrinterOpened then
    ClosePrinter(h);
end;
end;

//
// Calc real size of string without Esc specifiers
//
function TprTxReportOptions.ESCStringLengthWithoutESC(const s : string) : integer;
var
  p : integer;
begin
p := 1;
Result := ESCSkipTo(s,Length(s),p);
end;

function TprTxReportOptions.ParsePages(Lines : TStrings;
                                       UseLinesOnPage : boolean;
                                       LinesOnPage : integer;
                                       PagesList : TList) : integer;
var
  i,j : integer;
  Buf : string;
begin
Result := 1;
if PagesList<>nil then
  PagesList.Add(pointer(0));
Buf := GetFormFeedFullESCPrefix;
if Buf='' then
  exit; // FormFeed prefix not defined

j := 0;
for i:=0 to Lines.Count-1 do
  begin
    if pos(Buf,Lines[i])<>0 then
      begin
        if PagesList<>nil then
          PagesList.Add(pointer(i+1));
        j := 0;
        Inc(Result);
      end
    else
      begin
        if (UseLinesOnPage) and (j>=LinesOnPage) then
          begin
            if PagesList<>nil then
              PagesList.Add(pointer(i+1));
            j := 0;
            Inc(Result);
          end;
      end;
    Inc(j);
  end;
end;

function TprTxReportOptions.GetFormFeedFullESCPrefix : string;
var
  TxCommand : TprTxCommand;
begin
TxCommand := FindTxCommand(sFormFeedTxCommand);
if TxCommand<>nil then
  Result := ESCSymbol+TxCommand.FullPrefix;
end;

function TprTxReportOptions.GetFormFeedESCPrefix : string;
var
  TxCommand : TprTxCommand;
begin
TxCommand := FindTxCommand(sFormFeedTxCommand);
if TxCommand<>nil then
  Result := TxCommand.Prefix;
end;

const
  IDD_PRINTTEMPLATE = 1002;
  IDC_PAGESLIST = 1000;
  IDC_ALL = 1056;
  IDC_PAGES = 1058;
  IDC_SELECTION = 1057;
  IDC_EDITPAGESLIST = 1001;
  IDC_FROMPAGE = 1152;
  IDC_TOPAGE = 1153;
  IDC_PROPERTIES = 1025;

  IDC_PAPERPAGE = 1007;
  IDC_PAPERRULON = 1008;

  IDC_RULONALLLINES = 1059;
  IDC_RULONLINESRANGE = 1060;

  IDC_LINESFROM = 1155;
  IDC_LINESTO = 1156;

  IDC_ESCMODEL = 1002;

  IDC_PRINTERS = 1139;

  IDC_MAKEFORMFEEDONRULON = 1006;
  IDC_LEFTSPACES = 1003;

  IDC_USELINESONPAGE = 1004;
  IDC_LINESONPAGE = 1157;

  IDC_WRAPAFTERCOLUMN = 1005;
  IDC_EJECTPAGEAFTERPRINT = 1009;

  IDC_COPIES = 1154;
  IDC_ICON = 1086;
  IDC_COLLATE = 1041;

var
  cpd : PPrintDlg;
  pspr : pSetupPrintParams;

function DialogHook(Wnd: HWnd; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
var
  pl : TList;
  Buf : string;
  bBuf : LongBool;
  ESCModel : TprESCModel;
  i,lMin,lMax,CtrlID,TextLength : integer;
begin
Result := 0;

case Msg of
  WM_INITDIALOG:
    begin
      cpd := PPrintDlg(lParam);
      pspr := pSetupPrintParams(cpd.lCustData);

      ShowWindow(GetDlgItem(Wnd, IDC_PROPERTIES), SW_HIDE);
      ShowWindow(GetDlgItem(Wnd, IDC_COLLATE), SW_HIDE);

      SetDlgItemText(Wnd,IDC_EDITPAGESLIST,PChar(pspr.PrintPages));
      SetDlgItemInt(Wnd,IDC_LINESFROM,pspr.FromLine,false);
      SetDlgItemInt(Wnd,IDC_LINESTO,pspr.ToLine,false);
      SetDlgItemInt(Wnd,IDC_LINESONPAGE,pspr.LinesOnPage,false);
      SetDlgItemInt(Wnd,IDC_WRAPAFTERCOLUMN,pspr.WrapAfterColumn,false);

      UnCheck(Wnd,[IDC_PAGESLIST,
               IDC_PAGES,
               IDC_SELECTION,
               IDC_ALL,
               IDC_MAKEFORMFEEDONRULON,
               IDC_PAPERRULON,
               IDC_PAPERPAGE,
               IDC_USELINESONPAGE,
               IDC_RULONALLLINES,
               IDC_RULONLINESRANGE,
               IDC_EJECTPAGEAFTERPRINT]);


      if pspr.MakeFormFeedOnRulon then
        Check(Wnd,[IDC_MAKEFORMFEEDONRULON]);

      SetDlgItemInt(Wnd,IDC_LEFTSPACES,pspr.LeftSpaces,false);

      case pspr.PaperType of
        ptRulon:
          begin
            Check(Wnd,[IDC_PAPERRULON]);
            case pspr.PrintRulonMode of
              prmAllLines : Check(Wnd,[IDC_RULONALLLINES]);
              prmLinesRange : Check(Wnd,[IDC_RULONLINESRANGE]);
            end;
          end;
        ptPage :
          begin
            Check(Wnd,[IDC_PAPERPAGE]);
            case pspr.PrintPagesMode of
              ppmPagesList : Check(Wnd,[IDC_PAGESLIST]);
              ppmPagesRange : Check(Wnd,[IDC_PAGES]);
              ppmSelection : Check(Wnd,[IDC_SELECTION]);
                        else Check(Wnd,[IDC_ALL]);
            end;
          end;
      end;

      for i:=0 to TxReportOptions.ESCModelsCount-1 do
        SendDlgItemMessage(Wnd,IDC_ESCMODEL,CB_ADDSTRING,0,integer(PChar(TxReportOptions.ESCModels[i].ModelName)));

      SendDlgItemMessage(Wnd,IDC_ESCMODEL,CB_SELECTSTRING,0,integer(PChar(pspr.ESCModelName)));
      SendDlgItemMessage(Wnd,IDC_PRINTERS,CB_SELECTSTRING,0,integer(PChar(pspr.PrinterName)));
      SendMessage(Wnd,WM_COMMAND,(CBN_SELCHANGE shl 16) or IDC_PRINTERS,GetDlgItem(Wnd,IDC_PRINTERS));

      if pspr.UseLinesOnPage then
        Check(Wnd,[IDC_USELINESONPAGE]);

      if pspr.EjectPageAfterPrint then
        Check(Wnd,[IDC_EJECTPAGEAFTERPRINT]);

      CenterWindow(Wnd);
    end;
  WM_COMMAND:
    begin
      case wParam of
        IDOK:
          begin
            ////////////////////////
            // [OK] pressed
            ////////////////////////

            // Check ESCMODEL
            i := SendDlgItemMessage(Wnd,IDC_ESCMODEL,CB_GETCURSEL,0,0);
            if i=CB_ERR then
              Result := 1
            else
              begin
                TextLength := SendDlgItemMessage(Wnd,IDC_ESCMODEL,CB_GETLBTEXTLEN,i,0)+1;
                Buf := MakeStr(' ',TextLength);
                if SendDlgItemMessage(Wnd,IDC_ESCMODEL,CB_GETLBTEXT,i,integer(@(Buf[1])))<>CB_ERR then
                  begin
                    i := TxReportOptions.ESCModelIndexByModelName(Buf);
                    if i=-1 then
                      Result := 1;
                  end;
              end;
            if Result=1 then
              Application.MessageBox(PChar(prLoadStr(sSetupPrintError5)),PChar(prLoadStr(sAttention)),MB_OK+MB_ICONEXCLAMATION);

            // Check Lines range
            if (Result=0) and
               (IsChecked(Wnd,IDC_RULONLINESRANGE)) then
              begin
                lMin:=GetDlgItemInt(Wnd,IDC_LINESFROM,bBuf,false);
                lMax:=GetDlgItemInt(Wnd,IDC_LINESTO,bBuf,false);
                if lMin>lMax then
                  begin
                    Application.MessageBox(PChar(prLoadStr(sSetupPrintError3)),PChar(prLoadStr(sAttention)),MB_OK+MB_ICONEXCLAMATION);
                    Result:=1;
                  end;
                if (lMin<=0) or (lMax>pspr.MaxLines) then
                  begin
                    Application.MessageBox(PChar(Format(prLoadStr(sSetupPrintError4),[1,pspr.MaxLines])),PChar(prLoadStr(sAttention)),MB_OK+MB_ICONEXCLAMATION);
                    Result:=1;
                  end;
              end;

            // check pages range
            if (Result=0) and IsChecked(Wnd,IDC_PAGESLIST) then
              begin
                Buf := GetText(Wnd,IDC_EDITPAGESLIST);
                pl := TList.Create;
                try
                  if (Buf<>'') and TextToPageList(Buf,pl)  then
                    begin
                      lMin := integer(pl[0]);
                      lMax := integer(pl[pl.Count-1]);
                      if (lMin<cpd.nMinPage) or (lMin>cpd.nMaxPage) or (lMax<cpd.nMinPage) or (lMax>cpd.nMaxPage) then
                        begin
                          SetFocus(GetDlgItem(Wnd,IDC_EDITPAGESLIST));
                          Application.MessageBox(PChar(Format(prLoadStr(sSetupPrintError2),[cpd.nMinPage,cpd.nMaxPage])),PChar(prLoadStr(sAttention)),MB_ICONEXCLAMATION+MB_OK);
                          Result:=1;
                        end;
                    end
                  else
                    begin
                      Application.MessageBox(PChar(prLoadStr(sSetupPrintError1)),PChar(prLoadStr(sAttention)),MB_OK+MB_ICONEXCLAMATION);
                      Result := 1;
                    end;
                finally
                  pl.Free;
                end;
              end;

            // check LinesOnPage
            if (Result=0) and IsChecked(Wnd,IDC_USELINESONPAGE) then
              begin
                i := GetDlgItemInt(Wnd,IDC_LINESONPAGE,bBuf,false);
                if i<=0 then
                  begin
                    Application.MessageBox(PChar(prLoadStr(sSetupPrintError6)),PChar(prLoadStr(sAttention)),MB_OK+MB_ICONEXCLAMATION);
                    Result := 1;
                  end;
              end;

            if Result=0 then
              begin
                //////////////////////
                // save entered data
                //////////////////////
                pspr.ESCModelName   :=GetSelText(Wnd,IDC_ESCMODEL);
                pspr.PrinterName    :=GetSelText(Wnd,IDC_PRINTERS);

                pspr.EjectPageAfterPrint := IsChecked(Wnd,IDC_EJECTPAGEAFTERPRINT);
                pspr.UseLinesOnPage :=IsChecked(Wnd,IDC_USELINESONPAGE);
                pspr.LinesOnPage    :=GetDlgItemInt(Wnd,IDC_LINESONPAGE,bBuf,false);

                pspr.WrapAfterColumn:=GetDlgItemInt(Wnd,IDC_WRAPAFTERCOLUMN,bBuf,false);

                pspr.MakeFormFeedOnRulon:=IsChecked(Wnd,IDC_MAKEFORMFEEDONRULON);
                pspr.LeftSpaces         :=GetDlgItemInt(Wnd,IDC_LEFTSPACES,bBuf,false);
                pspr.Copies := GetDlgItemInt(Wnd,IDC_COPIES,bBuf,false);

                if IsChecked(Wnd,IDC_PAPERRULON) then
                  pspr.PaperType:=ptRulon
                else
                  if IsChecked(Wnd,IDC_PAPERPAGE) then
                    pspr.PaperType:=ptPage;

                pspr.FromLine :=GetDlgItemInt(Wnd,IDC_LINESFROM,bBuf,false);
                pspr.ToLine   :=GetDlgItemInt(Wnd,IDC_LINESTO,bBuf,false);

                if IsChecked(Wnd,IDC_RULONALLLINES) then
                  pspr.PrintRulonMode:=prmAllLines
                else
                  if IsChecked(Wnd,IDC_RULONLINESRANGE) then
                    pspr.PrintRulonMode:=prmLinesRange;

                pspr.PrintPages:=GetText(Wnd,IDC_EDITPAGESLIST);
                TextToPageList(pspr.PrintPages,pspr.PrintPagesList);

                if IsChecked(Wnd,IDC_PAGESLIST) then
                  pspr.PrintPagesMode:=ppmPagesList
                else
                  if IsChecked(Wnd,IDC_ALL) then
                    pspr.PrintPagesMode:=ppmAll
                  else
                    if IsChecked(Wnd,IDC_PAGES) then
                      pspr.PrintPagesMode:=ppmPagesRange;
              end;
          end
        else
          case WParam shr 16 of
            BN_CLICKED :
              begin
                CtrlID:=GetDlgCtrlID(LParam);
                case CtrlID of
                  IDC_PAPERPAGE:
                    begin
                      UnCheck(Wnd,[IDC_PAPERRULON,IDC_RULONALLLINES,IDC_RULONLINESRANGE]);
                      Check(Wnd,[IDC_PAPERPAGE]);
                    end;
                  IDC_PAPERRULON:
                    begin
                      UnCheck(Wnd,[IDC_PAPERPAGE,IDC_ALL,IDC_PAGES,IDC_SELECTION,IDC_PAGESLIST]);
                      Check(Wnd,[IDC_PAPERRULON]);
                    end;
                  IDC_RULONALLLINES:
                    begin
                      UnCheck(Wnd,[IDC_PAPERPAGE,IDC_RULONLINESRANGE,IDC_ALL,IDC_PAGES,IDC_SELECTION,IDC_PAGESLIST]);
                      Check(Wnd,[IDC_PAPERRULON,IDC_RULONALLLINES]);
                    end;
                  IDC_RULONLINESRANGE:
                    begin
                      UnCheck(Wnd,[IDC_PAPERPAGE,IDC_RULONALLLINES,IDC_ALL,IDC_PAGES,IDC_SELECTION,IDC_PAGESLIST]);
                      Check(Wnd,[IDC_PAPERRULON,IDC_RULONLINESRANGE]);

                      SetFocus(GetDlgItem(Wnd,IDC_LINESFROM));
                    end;
                  IDC_PAGESLIST:
                    begin
                      UnCheck(Wnd,[IDC_PAPERRULON,IDC_RULONALLLINES,IDC_RULONLINESRANGE,IDC_ALL,IDC_PAGES,IDC_SELECTION]);
                      Check(Wnd,[IDC_PAPERPAGE,IDC_PAGESLIST]);

                      SetFocus(GetDlgItem(Wnd,IDC_EDITPAGESLIST));
                      Result:=1;
                    end;
                  IDC_ALL,IDC_PAGES,IDC_SELECTION:
                    begin
                      UnCheck(Wnd,[IDC_PAPERRULON,IDC_PAGESLIST,IDC_RULONALLLINES,IDC_RULONLINESRANGE]);
                      Check(Wnd,[IDC_PAPERPAGE]);
                    end;
                end;
              end;
            EN_CHANGE:
              begin
                case GetDlgCtrlID(lParam) of
                  IDC_EDITPAGESLIST:
                    begin
                      UnCheck(Wnd,[IDC_PAPERRULON,IDC_RULONALLLINES,IDC_RULONLINESRANGE,IDC_ALL,IDC_PAGES,IDC_SELECTION]);
                      Check(Wnd,[IDC_PAPERPAGE,IDC_PAGESLIST]);
                    end;
                  IDC_FROMPAGE,IDC_TOPAGE:
                    begin
                      UnCheck(Wnd,[IDC_PAPERRULON,IDC_RULONALLLINES,IDC_RULONLINESRANGE,IDC_ALL,IDC_SELECTION]);
                      Check(Wnd,[IDC_PAPERPAGE]);
                    end;
                  IDC_LINESFROM,IDC_LINESTO:
                    begin
                      UnCheck(Wnd,[IDC_PAPERPAGE,IDC_RULONALLLINES,IDC_ALL,IDC_PAGES,IDC_SELECTION,IDC_PAGESLIST]);
                      Check(Wnd,[IDC_PAPERRULON,IDC_RULONLINESRANGE]);
                    end;
                end;
              end;
            CBN_SELCHANGE:
              begin
                case GetDlgCtrlID(lParam) of
                  IDC_PRINTERS:
                    begin
                      i := SendDlgItemMessage(Wnd,IDC_PRINTERS,CB_GETCURSEL,0,0);
                      if i<>CB_ERR then
                        begin
                          TextLength := SendDlgItemMessage(Wnd,IDC_PRINTERS,CB_GETLBTEXTLEN,i,0)+1;
                          Buf := MakeStr(' ',TextLength);
                          if SendDlgItemMessage(Wnd,IDC_PRINTERS,CB_GETLBTEXT,i,integer(@(Buf[1])))<>CB_ERR then
                            begin
                              ESCModel := TxReportOptions.FindESCModelByDriverName(Buf);
                              if ESCModel<>nil then
                                SendDlgItemMessage(Wnd,IDC_ESCMODEL,CB_SELECTSTRING,0,integer(PChar(ESCModel.ModelName)))
                              else
                                SendDlgItemMessage(Wnd,IDC_ESCMODEL,CB_SETCURSEL,-1,0)
                            end;
                        end;
                    end;
                end;
              end;
          end;
      end;
    end;
end;
end;

function TprTxReportOptions.TxSetupPrintParams;
var
  pd : tagPDA;
  spr : rSetupPrintParams;
begin
  spr.MaxLines := MaxLines;
  spr.PrintPagesMode := PrintPagesMode;
  spr.ESCModelName := ESCModelName;
  spr.PrinterName := PrinterName;
  spr.UseLinesOnPage := UseLinesOnPage;
  if WrapAfterColumn = -1 then
    spr.WrapAfterColumn := 0
  else
    spr.WrapAfterColumn := WrapAfterColumn;
  spr.MakeFormFeedOnRulon := MakeFormFeedOnRulon;
  spr.LeftSpaces := LeftSpaces;
  spr.PrintRulonMode := PrintRulonMode;
  if FromLine = -1 then
    spr.FromLine := 1
  else
    spr.FromLine := Min(Max(1,FromLine), MaxLines);
  if ToLine = -1 then
    spr.ToLine := MaxLines
  else
    spr.ToLine := Min(Max(1, ToPage), MaxLines);
  spr.PrintPages := PrintPages;
  spr.PrintPagesList := PrintPagesList;
  spr.PaperType := PaperType;
  spr.LinesOnPage := LinesOnPage;
  spr.EjectPageAfterPrint := EjectPageAfterPrint;
  spr.Copies := Copies;
  
  ZeroMemory(@pd, sizeof(pd));
  pd.hWndOwner := hWndOwner;
  pd.lStructSize := sizeof(pd);
  pd.hInstance := SysInit.hInstance;
  pd.lpPrintTemplateName :=PChar(IDD_PRINTTEMPLATE);
  pd.nCopies := Copies;
  if pd.nCopies = 0 then
    pd.nCopies := 1;
  
  pd.nMinPage := 1;
  pd.nMaxPage := Max(MaxPage, pd.nMinPage);
  pd.nFromPage := Min(Max(1,FromPage),pd.nMaxPage);
  pd.nToPage := Min(Max(1,ToPage),pd.nMaxPage);
  
  pd.lCustData := integer(@spr);
  pd.lpfnPrintHook := @DialogHook;
  pd.Flags := PD_HIDEPRINTTOFILE or PD_NONETWORKBUTTON or
              PD_ENABLEPRINTHOOK or PD_NOSELECTION or
              PD_ENABLEPRINTTEMPLATE;
  case PrintPagesMode of
    ppmAll : pd.Flags := pd.Flags+PD_ALLPAGES;
    ppmPagesRange : pd.Flags := pd.Flags+PD_PAGENUMS;
  end;

  Result := PrintDlg(pd);
  if Result then
  begin
    Copies := spr.Copies;
    FromPage := pd.nFromPage;
    ToPage := pd.nToPage;

    PrintPagesMode := spr.PrintPagesMode;
    ESCModelName := spr.ESCModelName;
    PrinterName := spr.PrinterName;
    UseLinesOnPage := spr.UseLinesOnPage;
    WrapAfterColumn := spr.WrapAfterColumn;
    MakeFormFeedOnRulon := spr.MakeFormFeedOnRulon;
    LeftSpaces := spr.LeftSpaces;
    PrintRulonMode := spr.PrintRulonMode;
    FromLine := spr.FromLine;
    ToLine := spr.ToLine;
    PrintPages := spr.PrintPages;
    PaperType := spr.PaperType;
    LinesOnPage := spr.LinesOnPage;
    EjectPageAfterPrint := spr.EjectPageAfterPrint;
  end;
end;

procedure TprTxReportOptions.InternalLoadFromIni(Ini: TCustomIniFile);
const
  sTxIniTxCommandsSectionName = 'TxCommands';
  sTxIniTxFontStylesSectionName = 'TxFontStyles';
  sTxIniTxFontOptionsSectionName = 'TxFontOptions';
  sTxIniGeneralSection = 'General';

  sTxIniErrorInvalidSymbol = 'Invalid prefix for TxCommand [%s]';
  sTxIniErrorTxCommandNotFoundForFontStyle = 'TxCommand [%s] in FontStyle [%s] not found';
  sTxIniErrorTxCommandNotFoundForFontOption = 'TxCommand [%s] in FontOption [%s] not found';
  sTxIniErrorTxCommandNotFoundForESCModel = 'TxCommand [%s] in ESCModel [%s] not found';
  sTxIniErrorInvalidRealESCCommand = 'Invalid real ESC command in ESCModel [%s] for TxCommand [%s]';
  sTxIniErrorInvalidTableForRecodeTable = 'Invalid table for RecodeTable [%s]';
  sTxIniErrorInvalidLengthTableForRecodeTable = 'Length of table for RecodeTable [%s] must be 128';

  sTxIniErrorInvalidBorderScheme = 'Invalid border scheme [%s]';
var
  ESCModel : TprESCModel;
  FontStyle : TprTxFontStyle;
  FontOption : TprTxFontOption;
  RecodeTable : TprTxRecodeTable;
  i,j,k,eCode : integer;
  l,lSections : TStringList;
  ESCModelItem : TprESCModelItem;
  Name,Value,RealESCCommand : string;
  TxCommand,TxCommandOn,TxCommandOff : TprTxCommand;
  BorderScheme: TprTxBorderScheme;

  function GetTxCommand(const ErrorMessage : string) : TprTxCommand;
  var
    Buf : string;
  begin
  Buf := ExtractSubStr(Value,k,[',']);
  Result := FindTxCommand(Buf);
  if Result=nil then
    raise Exception.CreateFmt(ErrorMessage,[Buf,Name]);
  end;

  function CodesToString(const CodesString,ErrorMessage : string) : string;
  var
    i,j : integer;
  begin
  Result := '';
  i := Length(CodesString);
  while i>0 do
    begin
      j := i;
      while (i>0) and (CodesString[i]<>'#') do Dec(i);
      val(Copy(CodesString,i+1,j-i),j,eCode);
      if (eCode<>0) or (j<0) or (j>255) then
        raise Exception.Create(ErrorMessage);
      Result := char(j)+Result;
      Dec(i);
    end;
  end;

  procedure LoadBorderSchemas(const ASection: string; L: TList);
  var
    J, K, P: Integer;
    Name, Borders, Value: string;
  begin
    J := ini.ReadInteger(ASection, 'BorderSchemasCount', 0);
    for K := 1 to J do
    begin
      Value := ini.ReadString(ASection, 'BorderScheme' + IntToStr(K), '');
      P := pos(',', Value);
      if P <> 0 then
      begin
        Name := Copy(Value, 1, P - 1);
        Borders := CodesToString(Copy(Value, P + 1, MaxInt), Format(sTxIniErrorInvalidBorderScheme, [Name]));
        if Length(Borders) <> 8 then
          raise Exception.CreateFmt(sTxIniErrorInvalidBorderScheme, [Name]);
      end;

      BorderScheme := TprTxBorderScheme.Create(Name,
                                               Borders[1],
                                               Borders[2],
                                               Borders[3],
                                               Borders[4],
                                               Borders[5],
                                               Borders[6],
                                               Borders[7],
                                               Borders[8]);
      L.Add(BorderScheme);
    end;
  end;

begin
l := TStringList.Create;
lSections := TStringList.Create;
try
  ini.ReadSectionValues(sTxIniTxCommandsSectionName,l);
  for i:=0 to l.Count-1 do
    begin
      Name := l.Names[i];
      Value := Trim(l.Values[Name]);
      j := 1;
      while (j<=Length(Value)) and (Value[j]<>' ') do Inc(j);
      if ((j-1)<>1) and ((j-1)<>3) then
        raise Exception.CreateFmt(sTxIniErrorInvalidSymbol,[Name]);

      TxCommand := TprTxCommand.Create;
      TxCommand.FName := Trim(Name);
      TxCommand.FPrefix := Copy(Value,1,j-1);
      TxCommand.FFullPrefix := Copy(Value,1,j-1);
      if (j-1)=3 then
        TxCommand.FFullPrefix := ESCSymbolForExCommands+TxCommand.FFullPrefix;
      TxCommand.FDescription := Trim(Copy(Value,j,Length(Value)));
      FTxCommands.Add(TxCommand);
    end;

  l.Clear;
  ini.ReadSectionValues(sTxIniTxFontStylesSectionName,l);
  for i:=0 to l.Count-1 do
    begin
      Name := l.Names[i];
      Value := Trim(l.Values[Name]);
      k := 1;
      TxCommand := GetTxCommand(sTxIniErrorTxCommandNotFoundForFontStyle);

      FontStyle := TprTxFontStyle.Create;
      FontStyle.FName := Trim(Name);
      FontStyle.FTxCommand := TxCommand;
      FontStyle.FDescription := Trim(Copy(Value,k,Length(Value)));
      FTxFontStyles.Add(FontStyle);
    end;

  l.Clear;
  ini.ReadSectionValues(sTxIniTxFontOptionsSectionName,l);
  for i:=0 to l.Count-1 do
    begin
      Name := l.Names[i];
      Value := Trim(l.Values[Name]);

      k := 1;
      TxCommandOn := GetTxCommand(sTxIniErrorTxCommandNotFoundForFontOption);
      TxCommandOff := GetTxCommand(sTxIniErrorTxCommandNotFoundForFontOption);

      FontOption := TprTxFontOption.Create;
      FontOption.FName := Trim(Name);
      FontOption.FTxCommandOn := TxCommandOn;
      FontOption.FTxCommandOff := TxCommandOff;
      FontOption.FDescription := Trim(Copy(Value,k,Length(Value)));
      FTxFontOptions.Add(FontOption);
    end;

  ini.ReadSections(lSections);
  for i:=0 to lSections.Count-1 do
    begin
      if AnsiCompareText(Copy(lSections[i],1,9),'ESCModel_')=0 then
        begin
          ESCModel := TprESCModel.Create;
          try
            ESCModel.FModelName := ini.ReadString(lSections[i],'ModelName',lSections[i]);
            ESCModel.FPrinterDriver := ini.ReadString(lSections[i],'PrinterDriver','');
            l.Clear;
            ini.ReadSectionValues(lSections[i],l);
            for j:=0 to l.Count-1 do
              begin
                Name := Trim(l.Names[j]);
                if (AnsiCompareText(Name,'ModelName')=0) or (AnsiCompareText(Name,'PrinterDriver')=0) then
                  continue;
                TxCommand := FindTxCommand(Name);
                if TxCommand=nil then
                  raise Exception.CreateFmt(sTxIniErrorTxCommandNotFoundForESCModel,[Name,ESCModel.ModelName]);
                RealESCCommand := CodesToString(Trim(l.Values[Name]),Format(sTxIniErrorInvalidRealESCCommand,[ESCModel.ModelName,TxCommand.Name]));
                if RealESCCommand<>'' then
                  begin
                    ESCModelItem := TprESCModelItem.Create;
                    ESCModelItem.FTxCommand := TxCommand;
                    ESCModelItem.FESCCommand := RealESCCommand;
                    ESCModel.FESCs.Add(ESCModelItem);
                  end;
              end;
          except
            ESCModel.Free;
            raise;
          end;
          FESCModels.Add(ESCModel);
        end
      else
        if AnsiCompareText(Copy(lSections[i],1,12),'RecodeTable_')=0 then
          begin
            Value := CodesToString(Trim(ini.ReadString(lSections[i],'OEMtoWINTable','')),
                                   Format(sTxIniErrorInvalidTableForRecodeTable,[lSections[i]]));
            if Length(Value)<>128 then
              raise Exception.CreateFmt(sTxIniErrorInvalidLengthTableForRecodeTable,[lSections[i]]);
            RecodeTable := TprTxRecodeTable.Create;
            RecodeTable.FRecodeTableName := Copy(lSections[i],13,Length(lSections[i]));
            RecodeTable.FDescription := ini.ReadString(lSections[i],'Description',RecodeTable.RecodeTableName);
            MoveMemory(RecodeTable.OEMtoWINTable,@Value[1],128);
            // build WINtoOEMTable
            for j:=128 to 255 do
              begin
                k := 0;
                while (k<128) and (RecodeTable.FOEMtoWINTable[k]<>char(j)) do Inc(k);
                if k<128 then
                  RecodeTable.FWINtoOEMTable[j-128] := char(k+128)
                else
                  RecodeTable.FWINtoOEMTable[j-128] := #32;
              end;

            // loading the border schemas
            LoadBorderSchemas(lSections[i], RecodeTable.FBorderSchemas);

            FRecodeTables.Add(RecodeTable);
          end;
    end;

  // read common border schemas
  LoadBorderSchemas('CommonBorderSchemas', FBorderSchemas);

  FDefaultRecodeTable := FindRecodeTable(ini.ReadString(sTxIniGeneralSection,'DefaultRecodeTable',''));
finally
  l.Free;
  lSections.Free;
end;
end;

procedure TprTxReportOptions.LoadFromFile(const FileName : string);
var
  ini : TIniFile;
begin
  ini := TIniFile.Create(FileName);
  try
    InternalLoadFromIni(ini);
  finally
    ini.Free;
  end;
end;

procedure TprTxReportOptions.LoadFromRes;
var
  AStream: TResourceStream;
  AIni: TprStreamIniFile;
begin
  if FindResource(hInstance, 'TXRO', RT_RCDATA) <> 0 then
  begin
    AStream := TResourceStream.Create(hInstance, 'TxRO', RT_RCDATA);
    AIni := TprStreamIniFile.Create(AStream);
    try
      InternalLoadFromIni(AIni);
    finally
      AIni.Free;
      AStream.Free;
    end;
  end;
end;

initialization

prTxReportOptionsFileName := GetFindFileName(_sTxReportOptionsFileName);
TxReportOptions := TprTxReportOptions.Create;
try
  if FileExists(prTxReportOptionsFileName) then
    TxReportOptions.LoadFromFile(prTxReportOptionsFileName)
  else
    TxReportOptions.LoadFromRes;
except
  on E : Exception do
    MBError(Format('Error while loading TprTxReportOptions from file'#13'%s'#13#13'%s',[_sTxReportOptionsFileName,E.Message]));
end;

finalization

TxReportOptions.Free;

end.


