{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Main unit of the PReport library.
Contains the base classes:
<ul>
<li>TprCustomReport - Base class for TprReport and TprTxReport components.</li>
<li>TprCustomPage - Base class for the page of the report template.</li>
<li>TprCustomEndPage - Base class for the report's generated pages.</li>
</ul>}
unit pr_Common;

{$i pr.inc}

interface

uses
  SysUtils, Windows, Classes, Graphics, WinSpool, DB,
  Dialogs, typinfo, Math, Forms, IniFiles, Messages, Controls, menus,
  stdctrls, CommDlg, ComCtrls, ActnList, vgr_Functions, vgr_GUIFunctions,
  {$ifdef PR_D6_D7} Types, Variants, {$endif}

  pr_CommonClasses, pr_Progress, pr_MultiLang, pr_Dataset, pr_Utils;

{$R ..\res\prStrings.res}
{$R ..\res\prDialogs.res}
{$R ..\res\preport.res}
{$R ..\res\primages.res}

//{$DEFINE DEBUG_FIRSTLINKEDVECTOR}
//{$DEFINE DEBUG_DUMPAGGVALUES}

const
  WS_EX_LAYERED_PR = $00080000;
  LWA_ALPHA_PR = $00000002;

  AngleSize = 5;
  _sPrIniFileName = 'pr.ini';
  sDefaultObjResName = 'TprDefaultObjResName';
  MAX_PAGEHEADERSFOOTERS = 3;
  MAX_PAGEBANDSORDER = 8;
  MAX_PAGEBANDSORDERVERT = 8;
  sPReportVersion = '1.9.9.1';
  sArialFont = 'Arial';
  sFixedFont = 'Courier New';
  GenGridUpdateCounter = 100;

//  WrapChars = [' ','-',';'];

type
TprGenCell = class;
TprGenVector = class;
TprGenHorzVector = class;
TprGenVertVector = class;
TprGenGrid = class;
TprBand = class;
TprBands = class;
TprObj = class;
TprValue = class;
TprObjClass = class of TprObj;
TprObjRecClass = class of TprObjRec;
TprObjVersionClass = class of TprObjRecVersion;
TprGroup = class;
TprGroups = class;
TprBandClass = class of TprBand;
TprCustomEndPage = class;
TprCustomPage = class;
TprCustomReport = class;
TprDesigner = class;
TprDesignerClass = class of TprDesigner;
TprPreview = class;
TprPreviewClass = class of TprPreview;
TprCustomReportClass = class of TprCustomReport;
TprGenBandInfo = class;
TprObjRec = class;

TprDynIntegerArray = array of integer;

{Specifies page orientation.
Items:
  poPortrait - portrait orientation.
  poLandscape - landscape orientation.}
TprPrinterOrientation = (poPortrait, poLandscape);
{TprHAlign specifies how text is aligned within a object.
Items:
  prhLeft - Text is left-justified: Lines all begin at the left edge of the object.
  prhCenter - Text is centered in the object.
  prhRight - Text is right-justified: Lines all end at the right edge of the object.}
TprHAlign = (prhLeft, prhCenter, prhRight, prhJustify);
{TprVAlign indicates where text appears within the client area of an object.
Items:
  prvTop - The text appears at the top of the its control.
  prvCenter - The text is vertically centered in the control.
  prvBottom - The text appears along the bottom of the control.}
TprVAlign = (prvTop,prvCenter,prvBottom);

{TprColDirectionType indicates order of the column generating for multicolimn reports.
Items:
  prcdTopBottomLeftRight - first - from top to bottom, then from left to right.
  prcdLeftRightTopBottom - first - from left to right, then from top to bottom.}
TprColDirectionType = (prcdTopBottomLeftRight, prcdLeftRightTopBottom);

{Specifies type of value in parser.
Items:
  prvvtBoolean - Boolean value.
  prvvtInteger - Integer value;
  prvvtDateTime - Date, Time or DateTime value.
  prvvtString - String value.
  prvvtDouble - numeric value.
  prvvtObject - reference to object.
  prvvtNull - nil value.
  prvvtNotCalced - for internal use only.}
TprVarValueType = (prvvtBoolean,prvvtInteger,prvvtDateTime,prvvtString,prvvtDouble,prvvtObject,prvvtNull,prvvtNotCalced);
(*$NODEFINE TprVarValueType*)
(*$HPPEMIT 'namespace Pr_common'*)
(*$HPPEMIT '{'*)

(*$HPPEMIT '#pragma option push -b-'*)
(*$HPPEMIT 'enum TprVarValueType { prvvtBoolean, prvvtInteger, prvvtDateTime, prvvtString, prvvtDouble, prvvtObject,'*)
(*$HPPEMIT '	prvvtNull, prvvtNotCalced };'*)
(*$HPPEMIT '#pragma option pop'*)

  /////////////////////////////////////////////////
  //
  // TprVarValue
  //
  /////////////////////////////////////////////////
{Describes the value which is used in the parser.
Syntax:
  TprVarValue = record
    vType : TprVarValueType;
    vString : string;
    case TprVarValueType of
      prvvtBoolean : (vBoolean : boolean);
      prvvtInteger : (vInteger : integer);
      prvvtDateTime : (vDateTime : TDateTime);
      prvvtDouble : (vDouble : double);
      prvvtObject : (vObject : TObject);
  end;}
  TprVarValue = record
    vType: TprVarValueType;
    vString: string;
    case TprVarValueType of
      prvvtBoolean: (vBoolean : boolean);
      prvvtInteger: (vInteger : integer);
      prvvtDateTime: (vDateTime : TDateTime);
      prvvtDouble: (vDouble : double);
      prvvtObject: (vObject : TObject);
  end;

(*$NODEFINE TprVarValue*)
(*$HPPEMIT 'struct TprVarValue'*)
(*$HPPEMIT '{'*)
(*$HPPEMIT '	TprVarValueType vType;'*)
(*$HPPEMIT '	AnsiString vString;'*)
(*$HPPEMIT '	double vData;'*)
(*$HPPEMIT '} ;'*)

//(*$NODEFINE TprVarValue*)
//(*$HPPEMIT 'struct TprVarValue'*)
//(*$HPPEMIT '{'*)
//(*$HPPEMIT '	TprVarValueType vType;'*)
//(*$HPPEMIT '	AnsiString vString;'*)
//(*$HPPEMIT '	union values'*)
//(*$HPPEMIT '	{'*)
//(*$HPPEMIT '		struct a'*)
//(*$HPPEMIT '		{'*)
//(*$HPPEMIT '			System::TObject* vObject;'*)
//(*$HPPEMIT '		};'*)
//(*$HPPEMIT '		struct b'*)
//(*$HPPEMIT '		{'*)
//(*$HPPEMIT '			double vDouble;'*)
//(*$HPPEMIT '		};'*)
//(*$HPPEMIT '		struct c'*)
//(*$HPPEMIT '		{'*)
//(*$HPPEMIT '			System::TDateTime vDateTime;'*)
//(*$HPPEMIT '		};'*)
//(*$HPPEMIT '		struct d'*)
//(*$HPPEMIT '		{'*)
//(*$HPPEMIT '			int vInteger;'*)
//(*$HPPEMIT '		};'*)
//(*$HPPEMIT '		struct e'*)
//(*$HPPEMIT '		{'*)
//(*$HPPEMIT '			bool vBoolean;'*)
//(*$HPPEMIT '		};'*)
//(*$HPPEMIT '	};'*)
//(*$HPPEMIT '} ;'*)

{Pointer to a TprVarValue structure.}
  PprVarValue  = ^TprVarValue;
{Dynamic array of TprVarValue structures.}
  TprVarsArray = array of TprVarValue;

/////////////////////////////////////////////////
//
// TprPersistent
//
/////////////////////////////////////////////////
{Base class that is derived from TPersistent that can generate an event when its properties are changed.}
TprPersistent = class(TPersistent)
private
  FOnChange: TNotifyEvent;
protected
  procedure DoChanged;
public
{Occurs when the object's properties is changed.}
  property OnChange: TNotifyEvent read FOnChange write FOnChange;
end;

/////////////////////////////////////////////////
//
// TprOptions
//
/////////////////////////////////////////////////
{Base class for classes that are intended for holding options.
This class declares the methods for saving and loading the properties from the INI file.}
TprOptions = class(TprPersistent)
public
{Writes the object's properties to the INI file.
Parameters:
  IniFile - The TIniFile object.
  SectionName - The name of section in the INI file.
  Prefix - The prefix for values' names.}
  procedure WriteToIni(IniFile : TIniFile; const SectionName, Prefix: string); virtual;
{Reads the object's properties to the INI file.
Parameters:
  IniFile - The TIniFile object.
  SectionName - The name of section in the INI file.
  Prefix - The prefix for values' names.}
  procedure ReadFromIni(IniFile : TIniFile; const SectionName, Prefix: string); virtual;
end;


{TOnScroll is the type for TprScrollBox.OnScroll event.
Syntax:
  TOnScroll = procedure (Sender: TObject; Msg: TWMScroll) of object;
Parameters:
  Sender - object, which fires an event.
  Msg - TWMScroll structure.}
TOnScroll = procedure (Sender: TObject; Msg: TWMScroll) of object;
/////////////////////////////////////////////////
//
// TprScrollBox
//
/////////////////////////////////////////////////
{Internal class, used as base class for TprCustomPreviewPanel and TprCustomDesignerPanel.
Defines events: OnVScroll and OnHScroll.
See also:
  TprCustomPreviewPanel, TprCustomDesignerPanel, TprPreviewPanel,
  TprTxPreviewPanel, TprDesignerPanel, TprTxDesignerPanel.}
TprScrollBox = class(TScrollBox)
private
  FOnVScroll : TOnScroll;
  FOnHScroll : TOnScroll;
  FAutoScrollInViewEnabled : boolean;
  procedure WMVScroll(var Msg : TWMScroll); message WM_VSCROLL;
  procedure WMHScroll(var Msg : TWMScroll); message WM_HSCROLL;
  function GetVertScrollPageSize : integer;
  function GetHorzScrollPageSize : integer;
protected
  function AutoScrollEnabled: Boolean; override;
//  procedure AutoScrollInView(AControl: TControl); override;
public
{Returns page size for a vertical scroll bar.}
  property VertScrollPageSize: Integer read GetVertScrollPageSize;
{Returns page size for a horizontal scroll bar.}
  property HorzScrollPageSize: Integer read GetHorzScrollPageSize;
{Indicates whether automatic scrolling is enabled.}
  property AutoScrollInViewEnabled: Boolean read FAutoScrollInViewEnabled write FAutoScrollInViewEnabled;
{Occurs when the user scrolls the control contents horizontally with the mouse or keyboard.
See also:
  TOnScroll}
  property OnVScroll: TOnScroll read FOnVScroll write FOnVScroll;
{Occurs when the user scrolls the control contents vertically with the mouse or keyboard.
See also:
  TOnScroll}
  property OnHScroll: TOnScroll read FOnHScroll write FOnHScroll;
end;

/////////////////////////////////////////////////
//
// TprForm
//
/////////////////////////////////////////////////
{Base class for all forms in PReport.}
TprForm = class(TForm)
protected
  procedure prRestoreProperties(Ini : TIniFile; sn : string); virtual;
  procedure prSaveProperties(Ini : TIniFile; sn : string); virtual;
public
  procedure AfterConstruction; override;
  procedure BeforeDestruction; override;
end;

/////////////////////////////////////////////////
//
// TprToolWindowForm
//
/////////////////////////////////////////////////
{Base class for all popup forms in PReport.}
TprToolWindowForm = class(TprForm)
private
  FAlphaBlendValue : integer;
  procedure SetAlphaBlendValue(Value : integer);
  function GetOpacity : integer;
  procedure SetOpacity(Value : integer);
protected
  function GetParentControl : TControl; virtual; abstract;
  procedure DoShow; override;
  procedure CreateWindowHandle(const Params : TCreateParams); override;
  procedure InitAlphaBlend;
public
{Creates an instance of TprToolWindowForm class.}
  constructor Create(AOwner : TComponent); override;

  procedure UpdateInfo; virtual; abstract;
  procedure SetFocusOnFirstControl; virtual;

{Specifies the degree of translucency on a translucent form.
Set AlphaBlendValue to a value between 0 and 255 to indicate the degree of
translucency when the AlphaBlend property is true.
A value of 0 indicates a completely transparent window.
A value of 255 indicates complete opacity.}
  property AlphaBlendValue: Integer read FAlphaBlendValue write SetAlphaBlendValue;
  property Opacity: Integer read GetOpacity write SetOpacity;
end;

/////////////////////////////////////////////////
//
// TprDesignerAndPreviewBaseForm
//
/////////////////////////////////////////////////
{Base class for built-in designer and preview form.
See also:
  TprDesignerForm, TprPreviewForm, TprTxDesignerForm, TprTxPreviewForm.}
TprDesignerAndPreviewBaseForm = class(TprForm)
private
  FShortCuts: TObject;
protected
  procedure Activate; override;
  procedure Deactivate; override;
  procedure RestoreShortCuts;
  procedure RemoveShortCuts;
public
  procedure AfterConstruction; override;
  procedure BeforeDestruction; override;
end;

{Describes current mouse operation in designer.
Items:
  mmNone - No mouse operation is active.
  mmSelect - Selecting of objects.
  mmRegionDrag - Drag one object.
  mmRegionResize - Resizing of object.
  mmRegionLink - Establish link from one object to another.
  mmSelectedResize - Resizing of selected objects.
  mmSelectedRegionsDrag - Drag of selected objects.
  mmInsertObj - Inserting of object.
Syntax:
  TprMouseMode = (mmNone, mmSelect, mmRegionDrag, mmRegionResize,
                  mmRegionLink, mmSelectedResize, mmSelectedRegionsDrag, mmInsertObj)}
TprMouseMode = (mmNone,mmSelect,mmRegionDrag,mmRegionResize,mmRegionLink,mmSelectedResize,mmSelectedRegionsDrag,mmInsertObj);
/////////////////////////////////////////////////
//
// TprDesigner
//
/////////////////////////////////////////////////
{Base class for built-in designers.
See also:
  TprDesignerForm, TprTxDesignerForm.}
TprDesigner = class(TprDesignerAndPreviewBaseForm)
protected
  FReport : TprCustomReport;
  procedure Loaded; override;
public
{Creates an instance of the TprDesigner class.
Parameters:
  AOwner - Owner for created designer window.
  _Report - Report which is edited by designer.
See also:
  Report}
  constructor CreateDesigner(AOwner: TComponent; _Report: TprCustomReport); virtual;
{Specifies report, which is edited by designer.}
  property Report: TprCustomReport read FReport;
{Completely update designer.}
  procedure UpdateCurrentPage; virtual; abstract;

  procedure CreateWnd; override;
  procedure BeforeDestruction; override;
end;

/////////////////////////////////////////////////
//
// TprPreview
//
/////////////////////////////////////////////////
{Base class for built-in preview windows.
See also:
  TprPreviewForm, TprTxPreviewForm.}
TprPreview  = class(TprDesignerAndPreviewBaseForm)
private
  FReport: TprCustomReport;

  function GetPageIndex: Integer;
  procedure SetPageIndex(Value: Integer);
  function GetPageCount: Integer;
protected
  function GetPreviewPanel: TObject; virtual; abstract;

  procedure Loaded; override;
  procedure DoOnCustomAction;

{Returns the TprCustomPreviewPanel object, used in the preview form.}
  property PreviewPanel: TObject read GetPreviewPanel;
public
{Creates an instance of the TprPreview class.
Parameters:
  AOwner - Owner for created preview window.
  _Report - Previewed report.
See also:
  Report}
  constructor CreatePreview(AOwner: TComponent; _Report: TprCustomReport); virtual;

  procedure CreateWnd; override;
  procedure BeforeDestruction; override;

{Displays the page with specified index.}
  procedure GotoPage(APageIndex: Integer);
{Displays the previous page.}
  procedure GotoPriorPage;
{Displays the next page.}
  procedure GotoNextPage;
{Displays the first page.}
  procedure GotoFirstPage;
{Displays the last page.}
  procedure GotoLastPage;
  
{Specifies report, which is previewed.}
  property Report: TprCustomReport read FReport;
{Specifies the zero-base index of the displayed page.}
  property PageIndex: Integer read GetPageIndex write SetPageIndex;
{Returns the number of pages in the connected report, or -1 if report is not connected.}
  property PageCount: Integer read GetPageCount;
end;

{Describes a position within a designer panel.
Items:
  piNone - Empty area.
  piRegionInside - Within of object (band or object)
  piRegionResize - Within of the object resize point.
  piRegionLink - Within area from which a link can be established.
Syntax:
  TprPointInfo = (piNone, piRegionInside, piRegionResize, piRegionLink, piSelectedResize)}
TprPointInfo = (piNone, piRegionInside, piRegionResize, piRegionLink, piSelectedResize);
{Describes type of resize operation.
Syntax:
  TprResizeType = (ppLeftTop, ppTop, ppRightTop, ppRight, ppRightBottom, ppBottom, ppLeftBottom, ppLeft)}
TprResizeType = (ppLeftTop, ppTop, ppRightTop, ppRight, ppRightBottom, ppBottom, ppLeftBottom, ppLeft);
{Describes a set of allowed resize operatons.
Syntax:
  TprResizeTypeSet = set of TprResizeType}
TprResizeTypeSet = set of TprResizeType;
{Describes type of link.
Items:
  ltLeft - Link is defined from left edge of object.
  ltTop - Link is defined from top edge of object.
  ltRight - Link is defined from right edge of object.
  ltBottom - Link is defined from bottom edge of object.
Syntax:
  ltLeft, ltTop, ltRight, ltBottom}
TprLinkType = (ltLeft, ltTop, ltRight, ltBottom);
{Describes a set of allowed link operatons.
Syntax:
  TprLinkTypeSet = set of TprLinkType}
TprLinkTypeSet = set of TprLinkType;

/////////////////////////////////////////////////
//
// TprPreviewUserData
//
/////////////////////////////////////////////////
{Describes a "user data", instance of this class can be associated with object of report template.
The developer can define child class (derived from TprPreviewUserData) to describe
the data.
See also:
  TprCustomReport.OnPreviewGetUserData}
TprPreviewUserData = class(TPersistent)
private
  FTag: Integer;
public
{Copies the contents of another, similar object.
Parameters:
  Source - The source object.}
  procedure Assign(Source: TPersistent); override;
{Saves the object contents to a stream.
Parameters:
  Stream - Instance of the TStream object.}
  procedure SaveToStream(Stream: TStream); virtual;
{Loads the object contents from a stream.
Parameters:
  Stream - Instance of the TStream object.}
  procedure LoadFromStream(Stream: TStream); virtual;
{Stores an integer value.
Tag has no predefined meaning.
The Tag property is provided for the convenience of developers.
It can be used for storing an additional integer value or it can be typecast to
any 32-bit value such as a component reference or a pointer.}
  property Tag: Integer read FTag write FTag;
end;

/////////////////////////////////////////////////
//
// TprObjRecVersion
//
/////////////////////////////////////////////////
{Base class for a separate object view.
In PReport each object can have multiply views. Each view contains set of object properties
and is identified by a formula, which returns boolean value.
During the report generating the formulas are calculated
and an object view is chosen which formula returns true.
If all formulas return false, a default view is selected.
Object can have one view with empty formula in this case this view is used.
An object view does not contain coordinates of object, it contains only visual properties like:
Text, Font, Background color and so on.
See also:
  TprObjRec, TprObj}
TprObjRecVersion = class(TCollectionItem)
private
  FFormula : string;
  FVisible : boolean;
  FCompiledFormula : string;
  FMayBeUse : boolean;
  FSecondPassCalcCurVersionNeeded : boolean;
public
{Creates an instance of the TprObjRec class.}
  constructor Create(Collection: TCollection); override;
{Copies the contents of another, similar object.}
  procedure Assign(Source: TPersistent); override;
published
{Specifies a formula, which identifies a object view,
it must return a boolen value.
During the report generating the formula is calculated
and an object view is chosen which formula returns true.
If all formulas return false, a default view is selected.
This property may contains a empty string in this case the
formula result equals to false.}
  property Formula: String read FFormula write FFormula;
{Visibility of the object view. Non-visible objects don't show when building report.}
  property Visible: Boolean read FVisible write FVisible;
end;

/////////////////////////////////////////////////
//
// TprObjRecVersions
//
/////////////////////////////////////////////////
{Represents a list of the object views.}
TprObjRecVersions = class(TCollection)
private
  function GetItm(I: integer) : TprObjRecVersion;
public
{Lists the object views in the list.
Use Items to access individual views in the collection.
The value of the Index parameter corresponds to the Index property of TprObjRecVersion.
It represents the position of the item in the collection.}
  property Items[I: integer]: TprObjRecVersion read GetItm; default;
end;

/////////////////////////////////////////////////
//
// TprObjRec
//
/////////////////////////////////////////////////
{Base class for a record of object properties.
Instance of this class has list of the object views, TprObjRec
contains information about coordinates of object.
Each object uses two instances of the TprObjRec class.
First is used when the report template is edited and second
is created when report is generated.
See also:
  TprObjRecVersion, TprObjRecVerions, TprObj}
TprObjRec = class(TPersistent)
private
  FVersions : TprObjRecVersions;
  FDefVersion : integer;           // default version
  FSecondPassCalcCurVersionNeeded : boolean;
  FWidthAsVerticalBand : boolean;
  FHeightAsHorizontalBand : boolean;
  FObj : TprObj;
{$IFDEF PG}
  FPage : TprCustomEndPage;
{$ENDIF}
  FContainer: IprReportContainer;
  FCurVersion : integer; // version selected in report generate
  function GetLeft : integer;
  function GetTop : integer;
  function GetRight : integer;
  function GetBottom : integer;
  procedure SetLeft(Value : integer);
  procedure SetTop(Value : integer);
  procedure SetRight(Value : integer);
  procedure SetBottom(Value : integer);
  procedure SetpRect(Value : TRect);
  function GetWidth: Integer;
  function GetHeight: Integer;
protected
  FpRect: TRect;
  FPreviewUserData: TprPreviewUserData;
  function CreateVersions: TprObjRecVersions; virtual;
  function GetVersionClass: TprObjVersionClass; virtual; abstract;

  function GetSupportSplitting: Boolean; virtual;
  function GetCanSplitValue: Boolean; virtual;
  procedure SetCanSplitValue(Value: Boolean); virtual;
  function GetCanSplit(AByHorizontal: Boolean; ASplitPos: Integer): Boolean; virtual;
  function Split(AByHorizontal: Boolean; ASpitPos: Integer; var AAddToSplitted: Integer): TprObjRec; virtual;

  function CreateCopy: TprObjRec; virtual;

  property CanSplit: Boolean read GetCanSplitValue write SetCanSplitValue default False;
  property Container: IprReportContainer read FContainer;
public
{Creates an instance of the TprObjRec class.
Parameters:
  _Page - Instance of the TprCustomEndPage class.
  _Obj - Instance of the TprObj class, which holds this object.}
  constructor Create(AContainer: IprReportContainer; _Obj: TprObj); virtual;
{Frees an instance of the TprObjRec class.}
  destructor Destroy; override;

{Returns reference to an instance of TprObj class, which holds this TprObjRec object.
This reference is passed to the constructor.}
  property Obj: TprObj read FObj;
{Returns reference to an instance TprCustomEndPage class.
This reference is passed to the constructor.}
{$IFDEF PG}
  property Page: TprCustomEndPage read FPage;
{$ENDIF}
{Specifies area which is occupied by object, coordinates is relative to the
top-left corner of the band which holds object.
Coordinates are specified in pixels.}
  property pRect: TRect read FpRect write SetpRect;
{Returns "user data" which are returned in the TprCustomReport.OnPreviewGetUserData.
This property is nil by default and can be used when report is generated only.
See also:
  TprPreviewUserData, TprCustomReport.OnPreviewGetUserData}
  property PreviewUserData: TprPreviewUserData read FPreviewUserData;
{Returns the index of the current object view, which is selected when report is generated.
Value of this property can be changed by the FirstPass and SecondPass methods.
These methods calculate the formulas of the objects views and select the necessary object view,
index of this view is stored in this property.
This property can be used when report is generated only.
See also:
  Versions, DefVersion, FirstPass, SecondPass}
  property CurVersion: Integer read FCurVersion;

{FirstPass is called in the first pass of the report generating.
This method calculates all view formulas and change value of DefVersion property.
Do not call this method directly.
See also:
  DefVersion}
  procedure FirstPass; virtual;
{SecondPass is called in the second pass of the report generating.
This methods calculates the view formulas, which could not be calculated
in the first pass (if a view format has aggregate value).
Do not call this method directly.
See also:
  DefVersion}
  procedure SecondPass; virtual;
{ThirdPass is called to place the selected object view on the resulting page of the report.
Parameters:
  Device - This parameter is nil for TprReport and contains
reference to the TprTextDevice object for TprTxReport.
  r - Specifies area which is occupied by object on resulting page.}
  procedure ThirdPass(AEndPage: TprCustomEndPage; Device: TObject; const r: TRect); virtual; abstract;

  procedure Assign(Source: TPersistent); override;

{Saves contents of the object to the stream.
This method is used in clipboard operations.}
  procedure Save(Stream: TStream); virtual;
{Loads contents of the object from the stream.
This method is used in clipboard operations.}
  procedure Load(Stream: TStream); virtual;
published
{Specifies index of the default object view.
This view is showed in design-time and used by default.}
  property DefVersion : integer read FDefVersion write FDefVersion default 0;
{Specifies list of the object views.}
  property Versions : TprObjRecVersions read FVersions write FVersions;
{Specifies X coordinate of the top-left object edge, relative to the top-left corner
of the object band. Value of this property is same as pRect.Left.
See also:
  pRect, Top, Right, Bottom}
  property Left: integer read GetLeft write SetLeft;
{Specifies Y coordinate of the top-left object edge, relative to the top-left corner
of the object band. Value of this property is same as pRect.Top.
See also:
  pRect, Left, Right, Bottom}
  property Top: integer read GetTop write SetTop;
{Specifies X coordinate of the bottom-right object edge, relative to the top-left corner
of the object band. Value of this property is same as pRect.Right.
See also:
  pRect, Left, Top, Bottom}
  property Right: integer read GetRight write SetRight;
{Specifies Y coordinate of the bottom-right object edge, relative to the top-left corner
of the object band. Value of this property is same as pRect.Bottom.
See also:
  pRect, Left, Top, Right}
  property Bottom: integer read GetBottom write SetBottom;
{Specifies the width of object.
See also:
  Height}
  property Width: Integer read GetWidth;
{Specifies the height of object.
See also:
  Width}
  property Height: Integer read GetHeight;
{Specifies boolean value which indicates when width of the object must
equals to width of the vertical band which contains this object.
See also:
  WidthAsVerticalBand}
  property WidthAsVerticalBand: Boolean read FWidthAsVerticalBand write FWidthAsVerticalBand default false;
{Specifies boolean value which indicates when height of the object must
equals to height of the horizontal band which contains this object.
See also:
  WidthAsVerticalBand}
  property HeightAsHorizontalBand: Boolean read FHeightAsHorizontalBand write FHeightAsHorizontalBand default false;
end;

/////////////////////////////////////////////////
//
// TprObjs
//
/////////////////////////////////////////////////
{Represents list of TprObj objects.}
TprObjs = class(TList)
private
  function GetItm(index: Integer): TprObj;
public
{Lists the objects in the list.}
  property Items[Index: Integer]: TprObj read GetItm; default;
end;

/////////////////////////////////////////////////
//
// TprDesignComponent
//
/////////////////////////////////////////////////
{Base class for all bands and objects.
This class declares methods which are used by the report designer.
See also:
  TprObj, TprBand}
TprDesignComponent = class(TComponent)
public
{Determines how the user can modify the size of the object in the report designer.
This method returns empty set by default.
Return value:
  Returns the set of the allowed resizings.}
  function DsgnAllowResizeTypes: TprResizeTypeSet; virtual;
{Determines the links types which user can define from object.
This method returns empty set by default.
Return value:
  Returns the set of the allowed links.
See also:
  DsgnAllowLinkWith, DsgnLink}
  function DsgnAllowLinkTypes: TprLinkTypeSet; virtual;
{Determines if the user can drag the object in the report designer,
returns false by default.
Return value:
  Returns the true value if the user can drag object in the report designer.}
  function DsgnAllowDrag: Boolean; virtual;
{Returns the true value if the object can be inplace-edited,
returns false by default.
Return value:
  Returns the true value if the object can be inplace-edited.}
  function DsgnAllowInplaceEdit: Boolean; virtual;
{Returns the true value if the object can be linked to the OtherObject component.
Parameters:
  OtherObject - The object to check.
See also:
  DsgnAllowLinkTypes, DsgnLink}
  function DsgnAllowLinkWith(OtherObject: TprDesignComponent): Boolean; virtual;
{Returns the true value if object is transparent and must be painted after of
other objects.}
  function DsgnIsTransparent: Boolean; virtual;

{Is called when the link is established from this object to another.
Parameters:
  Linked - The object with which link is establshed.
  LinkMode - The link type.
  LinkAccepted - Set this parameter to false to cancel a link.
  ExData - Contains nil for TprReport and pointer to the rTxExData structure for TprTxReport.
See also:
  TprLinkType, DsgnAllowLinkTypes, DsgnAllowLinkWith}
  procedure DsgnLink(Linked: TprDesignComponent; LinkType: TprLinkType; var LinkAccepted: boolean; ExData: pointer); virtual;
{Is called when the object is dragged.
Parameters:
  dx - The horizontal offset of the object.
  dy - The vertical offset of the object.
  DragAccepted - Set this parameter to false to cancel a dragging.
See also:
  DsgnAllowDrag}
  procedure DsgnDrag(dx, dy: integer; var DragAccepted: boolean; ExData: pointer); virtual;
{Is called when the object is resized.
Parameters:
  oTop - The offset of the TOP coordinate of object.
  oLeft - The offset of the LEFT coordinate of object.
  oBottom - The offset of the BOTTOM coordinate of object.
  oRight - The offset of the RIGHT coordinate of object.
  ResizeAccepted - Set this parameter to false to cancel a resizing.
  ExData - Contains nil for TprReport and pointer to the rTxExData structure for TprTxReport.
See also:
  DsgnAllowDrag}
  procedure DsgnResize(oTop, oLeft, oBottom, oRight: Integer; var ResizeAccepted: Boolean; ExData: Pointer); virtual;
{Is called the object is deleted.
The destructor is called in this method by default.}
  procedure DsgnDelete; virtual;
{Is called when the popup menu is created for object.
Parameters:
  Popup - The TPopupMenu object.
  OneObjectSelected - Contains the true value if one object is selected.}
  procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: Boolean); virtual;
end;
{TprDesignComponentClass is the class type of a TprDesignComponent descendant.}
TprDesignComponentClass = class of TprDesignComponent;

/////////////////////////////////////////////////
//
// TprNotifyLink
//
/////////////////////////////////////////////////
{Internal class, used for establishing notifications between a report and designers and previews.}
TprNotifyLink = class(TObject)
private
  FOnNotify: TNotifyEvent;
protected
  procedure DoNotify(Source: TObject);
public
{Occurs when the contents of the report is changed.}
  property OnNotify: TNotifyEvent read FOnNotify write FOnNotify;
end;

{Describes the work mode of the link from the top edge of the object.
Items:
  prlmMaxBottom -
  prlmMinBottom -
Syntax:
  TprVLinkMode = (prlmMaxBottom, prlmMinBottom)}
TprVLinkMode = (prlmMaxBottom, prlmMinBottom);
{Describes the work mode of the link from the left edge of the object.
Items:
  prlmMaxRight - 
  prlmMinRight -
Syntax:
  TprHLinkMode = (prlmMaxRight, prlmMinRight)}
TprHLinkMode = (prlmMaxRight, prlmMinRight);
{Describes the work mode of the link from the bottom edge of the object.
Items:
  prrmMaxBottom -
  prrmMinBottom -
  prrmMaxHeight -
  prrmMinHeight -
  prrmHeightSum - 
Syntax:
  TprVResizeMode = (prrmMaxBottom, prrmMinBottom, prrmMaxHeight, prrmMinHeight, prrmHeightSum)}
TprVResizeMode = (prrmMaxBottom, prrmMinBottom, prrmMaxHeight, prrmMinHeight, prrmHeightSum);
{Describes the work mode of the link from the right edge of the object.
Items:
  prrmMaxRight -
  prrmMinRight -
  prrmMaxWidth -
  prrmMinWidth -
  prrmWidthSum - 
Syntax:
  TprHResizeMode = (prrmMaxRight, prrmMinRight, prrmMaxWidth, prrmMinWidth, prrmWidthSum)}
TprHResizeMode = (prrmMaxRight, prrmMinRight, prrmMaxWidth, prrmMinWidth, prrmWidthSum);

/////////////////////////////////////////////////
//
// TprObj
//
/////////////////////////////////////////////////
{Base class for all PReport objects, such as: TprMemoObj, TprImageObj, TprTxMemoObj and so on.
Object contains information about links with other object and
record of object parameters (class derived from TprObjRec).
TprObj class also overrides methods: DsgnResize, DsgnDrag and so on.
See also:
  TprObjRec, TprObjRecVersion}
TprObj = class(TprDesignComponent)
private
  FBand : TprBand;
  FirstPassProcessed : boolean;
  FTopMode : TprVLinkMode;
  FLeftMode : TprHLinkMode;
  FWidthMode : TprHResizeMode;
  FHeightMode : TprVResizeMode;

  FTopObjsNames : TStrings; 
  FLeftObjsNames : TStrings;
  FWidthObjsNames : TStrings;
  FHeightObjsNames : TStrings;

  FTopObjs: TprObjs;
  FLeftObjs: TprObjs;
  FWidthObjs: TprObjs;
  FHeightObjs: TprObjs;

  procedure SetdRec(Value: TprObjRec);
  procedure SetBand(Value: TprBand);
  function GetReport: TprCustomReport;

  function GetVersionCount: Integer;
  function GetGenVersionCount: Integer;
  function GetVersion(Index: Integer): TprObjRecVersion;
  function GetGenVersion(Index: Integer): TprObjRecVersion;
  function GetDefVersion: TprObjRecVersion;
  function GetGenCurVersion: TprObjRecVersion;
protected
  FdRec : TprObjRec;
  FaRec : TprObjRec;

  procedure InitdRec; virtual; abstract;

  procedure ReadLeft(Reader : TReader);
  procedure WriteLeft(Writer : TWriter);
  procedure ReadTop(Reader : TReader);
  procedure WriteTop(Writer : TWriter);
  procedure ReadWidth(Reader : TReader);
  procedure WriteWidth(Writer : TWriter);
  procedure ReadHeight(Reader : TReader);
  procedure WriteHeight(Writer : TWriter);
  procedure ReadVisible(Reader: TReader);

  procedure DefineProperties(Filer : TFiler); override;

  procedure SetParentComponent(Value : TComponent); override;
  function GetChildOwner : TComponent; override;

  procedure Notification(AComponent : TComponent; Operation : TOperation); override;

  procedure OnDsgnPopupMenuClick(Sender : TObject);
  procedure DoOnFirstPassObject(var ManuallyProcessed : boolean);
  procedure DsgnNotifyDesigner;

  procedure FirstPass; virtual;
public
{Creates an instance of the TprObj class.
Parameters:
  AOwner - The owner component.}
  constructor Create(AOwner: TComponent); override;
{Frees an instance of TprObj class.}
  destructor Destroy; override;

{Returns the description of object, which is showed in designer.
This method returns the component name by default, can be overridden in the descendant classes.}
  function GetDesc: string; virtual;
{Draws the object in the report designer.
Parameters:
  DC - Handle to the device context.
  ExData - Contains nil for TprReport or a pointer to the rTxExData structure for TprTxReport.
  DrawRext - Determines the area occupied by object.}
  procedure DrawDesign(DC: HDC; ExData: Pointer; const DrawRect: TRect); virtual;

{Determines how the user can modify the size of the object in the report designer.
This method returns the [ppLeftTop,ppTop,ppRightTop,ppRight,ppRightBottom,ppBottom,ppLeftBottom,ppLeft] value
by default, TprObj supports all resizing types.
Return value:
  Returns the set of the allowed resizings.}
  function DsgnAllowResizeTypes: TprResizeTypeSet; override;
{Determines if the user can drag the object in the report designer,
returns true by default.
Return value:
  Returns the true value if the user can drag object in the report designer.}
  function DsgnAllowDrag: Boolean; override;
{Determines the links types which user can define from object.
This method returns the [ltLeft, ltTop, ltRight, ltBottom] value by default.
Return value:
  Returns the set of the allowed links.
See also:
  DsgnAllowLinkWith, DsgnLink}
  function DsgnAllowLinkTypes: TprLinkTypeSet; override;
{Returns the true value if the object can be linked to the OtherObject component.
TprObj class returns true if OtherObject is TprObj and belongs to the same band.
Parameters:
  OtherObject - The object to check.
See also:
  DsgnAllowLinkTypes, DsgnLink}
  function DsgnAllowLinkWith(OtherObject: TprDesignComponent): Boolean; override;
{Is called when the object is resized.
Parameters:
  oTop - The offset of the TOP coordinate of object.
  oLeft - The offset of the LEFT coordinate of object.
  oBottom - The offset of the BOTTOM coordinate of object.
  oRight - The offset of the RIGHT coordinate of object.
  ResizeAccepted - Set this parameter to false to cancel a resizing.
  ExData - Contains nil for TprReport and pointer to the rTxExData structure for TprTxReport.
See also:
  DsgnAllowDrag}
  procedure DsgnResize(oTop, oLeft, oBottom, oRight: Integer; var ResizeAccepted: Boolean; ExData: Pointer); override;
{Is called when the object is dragged.
Parameters:
  dx - The horizontal offset of the object.
  dy - The vertical offset of the object.
  DragAccepted - Set this parameter to false to cancel a dragging.
See also:
  DsgnAllowDrag}
  procedure DsgnDrag(dx, dy: Integer; var DragAccepted: Boolean; ExData: Pointer); override;
{Is called when the link is established from this object to another.
Parameters:
  Linked - The object with which link is establshed.
  LinkMode - The link type.
  LinkAccepted - Set this parameter to false to cancel a link.
  ExData - Contains nil for TprReport and pointer to the rTxExData structure for TprTxReport.
See also:
  TprLinkType, DsgnAllowLinkTypes, DsgnAllowLinkWith}
  procedure DsgnLink(Linked: TprDesignComponent; LinkType: TprLinkType; var LinkAccepted: Boolean; ExData: Pointer); override;
{Is called when the popup menu is created for object.
Parameters:
  Popup - The TPopupMenu object.
  OneObjectSelected - Contains the false value if one object is selected.}
  procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: Boolean); override;

{Starts the inplace editing of object.
Parameter:
  _Parent - The parent control.
  InplaceEditor - Returns the control which is used for editing.
  InplaceRect - Specifies a bounds for control which is used for editing.
  ExData - Contains nil for TprReport and pointer to the rTxExData structure for TprTxReport.}
  procedure InplaceEdit(_Parent: TWinControl; var InplaceEditor: TWinControl; const InplaceRect: TRect; ExData: Pointer); virtual;
{Ends the inplace editing and saves the user's changes to the control.
Parameters:
  InplaceEditor - The control which is used for editing.}
  procedure SaveInplaceEdit(InplaceEditor: TWinControl); virtual;

{Is called when report template is loaded. Can be used for the some initializations.}
  procedure AfterReportLoaded; virtual;

  function HasParent: boolean; override;
  function GetParentComponent: TComponent; override;

{Specifies a band which contains a object.}
  property Band: TprBand read FBand write SetBand;
{Returns the TprCustomReport component which owns a object.}
  property Report: TprCustomReport read GetReport;
{Returns a TprObjRec object, which is created in the time of the report building.
This property can be used when the report is builded in the FirsPass method
for example.}
  property aRec: TprObjRec read FaRec;
{Specifies the list of objects with which connections from the top edge are defined.
These connections influence on the TOP object coordinate.
See also:
  TopMode}
  property TopObjs: TprObjs read FTopObjs;
{Specifies the list of objects with which connections from the left edge are defined.
These connections influence on the LEFT object coordinate.
See also:
  LeftMode}
  property LeftObjs: TprObjs read FLeftObjs;
{Specifies the list of objects with which connections from right edge are defined.
These connections influence on the width of object.
See also:
  WidthMode}
  property WidthObjs: TprObjs read FWidthObjs;
{Specifies the list of objects with which connections from bottom edge are defined.
These connections influence on the height of object.
See also:
  HeightMode}
  property HeightObjs: TprObjs read FHeightObjs;

{Returns the number of the object's views in the Versions property.
This property can be used only for the report template creating.
See also:
  Versions}
  property VersionCount: Integer read GetVersionCount;
{Returns the number of the object's views in the GenVersions property.
This property can be used when the report is builded in the FirsPass method
for example.
See also:
  GenVersions}
  property GenVersionCount: Integer read GetGenVersionCount;
{Use this property to iterate the object's views in the time of the report template creating.
See also:
  VersionCount}
  property Versions[Index: Integer]: TprObjRecVersion read GetVersion;
{Use this property to iterate the object's views in the time of the report building.
See also:
  GenVersionCount}
  property GenVersions[Index: Integer]: TprObjRecVersion read GetGenVersion;
{Returns the default object's view.
This property can be used only in the time of the report template building.
See also:
  TprObjRec.DefVersion, Versions, VersionCount}
  property DefVersion: TprObjRecVersion read GetDefVersion;
{Returns the current object's view,
this view is currently selected by the report engine.
This property can be used only in the time of the report template building.
See also:
  TprObjRec.DefVersion, Versions, VersionCount}
  property GenCurVersion: TprObjRecVersion read GetGenCurVersion;
published
{Returns a TprObjRec object, which is used in the time of the report template creating.
This property can be used when the report is builded in the FirsPass method
for example.
See also:
  DefVersion, Versions, VersionCount}
  property dRec: TprObjRec read FdRec write SetdRec;

{Specifies the work mode of the links from the left edge of object.
See also:
  LeftObjs, TprHLinkMode}
  property LeftMode: TprHLinkMode read FLeftMode write FLeftMode default prlmMaxRight;
{Specifies the work mode of the links from the top edge of object.
See also:
  TopObjs, TprVLinkMode}
  property TopMode: TprVLinkMode read FTopMode write FTopMode default prlmMaxBottom;
{Specifies the work mode of the links from the right edge of object.
See also:
  WidthObjs, TprHResizeMode}
  property WidthMode: TprHResizeMode read FWidthMode write FWidthMode default prrmMaxRight;
{Specifies the work mode of the links from the bottom edge of object.
See also:
  HeightObjs, TprVResizeMode}
  property HeightMode: TprVResizeMode read FHeightMode write FHeightMode default prrmMaxBottom;
end;




{Describes the bands' types.
Items:
  bthTitle - The horizontal report title.
  bthSummary - The horizontal report summary.
  bthPageHeader - The horizontal page header.
  bthPageFooter - The horizontal page footer.
  bthDetail - The horizontal details.
  bthDetailHeader - The horizontal detail header.
  bthDetailFooter - The horizontal detail footer.
  bthGroupHeader - The horizontal group header.
  bthGroupFooter - The horizontal group footer.
  btvTitle - The vertical report title.
  btvSummary - The vertical report summary.
  btvPageHeader - The vertical page header.
  btvPageFooter - The vertical page footer.
  btvDetail - The vertical details.
  btvDetailHeader - The vertical detail header.
  btvDetailFooter - The vertical detail footer.
  btvGroupHeader - The vertical group header.
  btvGroupFooter - The vertical group footer.
}
TprBandType = (bthTitle,
               bthSummary,
               bthPageHeader,
               bthPageFooter,
               bthDetail,
               bthDetailHeader,
               bthDetailFooter,
               bthGroupHeader,
               bthGroupFooter,
               btvTitle,
               btvSummary,
               btvPageHeader,
               btvPageFooter,
               btvDetail,
               btvDetailHeader,
               btvDetailFooter,
               btvGroupHeader,
               btvGroupFooter);
{The dynamical array of bands' types.}
TprBandTypeArray = array of TprBandType;
TGenerateCellCallbackProc = procedure(Sender: TprBand; Cell: TprGenCell; Objects: TprObjs) of object;
{Describes the work mode of the band's links.
Items:
  prbrmNone -
  prbrmMaxObj -
  prbrmMaxResizeObj -
  prbrmMinResizeObj -
Syntax:
  TprBandResizeMode = (prbrmNone, prbrmMaxObj, prbrmMaxResizeObj, prbrmMinResizeObj)}
TprBandResizeMode = (prbrmNone, prbrmMaxObj, prbrmMaxResizeObj, prbrmMinResizeObj);

/////////////////////////////////////////////////
//
// TprBand
//
/////////////////////////////////////////////////
{Base class for all PReport bands.
TprBand object contains list of objects (TprObj class).
See also:
  TprObj}
TprBand = class(TprDesignComponent)
private
  FCalced : boolean;
  FBandType : TprBandType;        // type of Band
  FPage : TprCustomPage;
  FResizeObjs : TprObjs;
  FObjects : TprObjs;
  FResizeObjsNames : TStrings;
  FResizeMode : TprBandResizeMode;
  FVisible : boolean;
  FVisibleFormula : string;
  FdPageRect : TRect;
  FCanSplit: Boolean;
  FSubReportName: string;
  procedure SetPage(Value: TprCustomPage);
  procedure SetVisibleFormula(Value : string);
  function GetBandTypeStr: string;
protected
  function GetSubBandSearchDirection: Integer; virtual;
  function GetSize: Integer; virtual; abstract;
  procedure GetObjectSizes(AObject: TprObj; var AGenPos, AGenSize, ADesignMin, ADesignMax: Integer); virtual; abstract;

  procedure GroupRemoved(Group : TprGroup); virtual;
  procedure GroupAdded(Group : TprGroup); virtual;
  procedure FillGenInfo(Info : TprGenBandInfo); virtual;
  function IsFormulaVisible(var ASecondPassNeeded: Boolean; var ASecondPassFormula: string): Boolean;
  procedure GetChildren(Proc : TGetChildProc; Root : TComponent); override;
  procedure SetParentComponent(Value : TComponent); override;
  procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  function  GetChildOwner : TComponent; override;
  procedure ReadResizeObjs(Reader : TReader);
  procedure WriteResizeObjs(Writer : TWriter);
  procedure DefineProperties(Filer : TFiler); override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
  procedure DsgnNotifyDesigner;

  procedure BeginSection; virtual; abstract;
  procedure EndSection; virtual; abstract;
  
  function CalcBandSize(AObjects: TprObjs): Integer; virtual;
  function GenerateCell(CallerBand : TprBand; CallbackProc : TGenerateCellCallbackProc) : TprGenCell; virtual; abstract;
  procedure CalcdPageRect(var CurPageRect : TRect; ExData : pointer); virtual; abstract;
  procedure InitDataSet; virtual;

  property CanSplit: Boolean read FCanSplit write FCanSplit default False;
  property SubReportName: string read FSubReportName write FSubReportName;
  property BandTypeStr: string read GetBandTypeStr;
public
{Creates an instance of the TprBand class.
Parameters:
  AOwner - The owner component.}
  constructor Create(AOwner : TComponent); override;
{Frees an instance of TprBand class.}
  destructor Destroy; override;

{Returns the band's area, coordinates are specified in pixels is relative to top-left corner of the page.}
  property dPageRect: TRect read FdPageRect; // band coords, from top left of page
{Contains the list of objects with which links are defined.
See also:
  TprObjs, Resize, Objects}
  property ResizeObjs: TprObjs read FResizeObjs;
{Contains the list of the band's objects.
See also:
  TprObjs}
  property Objects: TprObjs read FObjects;
{Returns the type of band.
See also:
  TprBandType}
  property BandType: TprBandType read FBandType;
{Specifies the page of the report template which holds the band.
See also:
  TprCustomPage}
  property Page: TprCustomPage read FPage write SetPage;
{Returns the TprCustomReport component, which owns the band.}
  function Report: TprCustomReport;

{Is called in the time of the report template creating, when the
band is placed on the report template.
This method can be used for the some initializations.}
  procedure OnInsertIntoPage(p: TprCustomPage); virtual;

{Returns the true value if the object can be linked to the OtherObject component.
Parameters:
  OtherObject - The object to check.
See also:
  DsgnAllowLinkTypes, DsgnLink}
  function DsgnAllowLinkWith(OtherObject: TprDesignComponent): Boolean; override;
{Is called when the popup menu is created for band.
Parameters:
  Popup - The TPopupMenu object.
  OneObjectSelected - Contains the true value if one band is selected.}
  procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: Boolean); override;
{Is called when band's hint is showed.
Parameters:
  HintCaption - Returns caption of the band's hint.
  HintText - Return the hint text.}
  procedure DsgnGetHintText(var HintCaption: string; HintText: TStringList);

{Draws the band in the report designer.
Parameters:
  DC - Handle to the device context.
  ExData - Contains nil for TprReport or a pointer to the rTxExData structure for TprTxReport.
  DrawRext - Determines the area occupied by band.}
  procedure DrawDesign(DC: HDC; ExData: Pointer; const DrawRect: TRect); virtual; abstract;
{Returns the description of band, which is showed in designer.
This method can be overridden in the descendant classes.}
  function GetDrawDesignCaption: string; virtual;

{Is called when report template is loaded. Can be used for the some initializations.}
  procedure AfterReportLoaded; virtual;

  function HasParent : boolean; override;
  function GetParentComponent : TComponent; override;
published
{Specifies the work mode of the band's links.
These links influence on the band's size (width for vertical and height for horizontal). 
See also:
  TprBandResizeMode, ResizeObjs}
  property ResizeMode: TprBandResizeMode read FResizeMode write FResizeMode default prbrmNone;
{Visibility of the band. Non-visible bands don't show when building report.}
  property Visible: boolean read FVisible write FVisible default true;
{Specifies the formula which determines show the band or not.
If the formula returns the true value then the band is showed.}
  property VisibleFormula: string read FVisibleFormula write SetVisibleFormula;
end;

/////////////////////////////////////////////////
//
// TprCustomHBand
//
/////////////////////////////////////////////////
{Base class for all horizontal bands.}
TprCustomHBand = class(TprBand)
protected
  FHeight : integer;
  FUseVerticalBands : boolean;

  function GetSize: Integer; override;
  procedure GetObjectSizes(AObject: TprObj; var AGenPos, AGenSize, ADesignMin, ADesignMax: Integer); override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
  procedure CalcdPageRect(var CurPageRect : TRect; ExData : pointer); override;
  function GenerateCell(CallerBand: TprBand; CallbackProc: TGenerateCellCallbackProc): TprGenCell; override;
  procedure GenerateCellCallback(Sender: TprBand; Cell: TprGenCell; Objects: TprObjs);
  procedure BeginSection; override;
  procedure EndSection; override;
public
{See:
  DsgnResize}
  procedure DsgnResize(oTop,oLeft,oBottom,oRight : integer; var ResizeAccepted : boolean; ExData : pointer); override;
{See:
  DsgnLink}
  procedure DsgnLink(Linked: TprDesignComponent; LinkMode : TprLinkType; var LinkAccepted : boolean; ExData : pointer); override;
{See:
  DsgnAllowResizeTypes}
  function DsgnAllowResizeTypes: TprResizeTypeSet; override;
{See:
  DsgnAllowLinkTypes}
  function DsgnAllowLinkTypes: TprLinkTypeSet; override;
{See:
  DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected : boolean); override;
published
{Specifies the band's height in pixels.}
  property Height: integer read FHeight write FHeight;
{Specifies the boolean value which determines how band works in cross-tab reports.
This value can not have the true value for bands:
  bthTitle, bthPageHeader, bthPageFooter}
  property UseVerticalBands: Boolean read FUseVerticalBands write FUseVerticalBands default False;
end;

/////////////////////////////////////////////////
//
// TprCustomHTitleBand
//
/////////////////////////////////////////////////
{Base class for the horizontal report titles.}
TprCustomHTitleBand = class(TprCustomHBand)
end;

/////////////////////////////////////////////////
//
// TprCustomHSummaryBand
//
/////////////////////////////////////////////////
{Base class for the horizontal report summaries.}
TprCustomHSummaryBand = class(TprCustomHBand)
private
  FPrintWithBand : TprCustomHBand;
protected
  procedure FillGenInfo(Info : TprGenBandInfo); override;
  procedure Notification(AComponent : TComponent; AOperation : TOperation); override;
published
{Gets or sets the value indicating whether the band can split should it fall on a page break,
has the false value by default - band can not be splitted.}
  property CanSplit;
{Gets or sets the name of sub-report that should be inserted after this band.}
  property SubReportName;
{Specifies the horizontal band which must be printed on the one page with the
report summary band. Use this property for avoiding the situations in which
the report summary is printed without data.}
  property PrintWithBand: TprCustomHBand read FPrintWithBand write FPrintWithBand;
end;

/////////////////////////////////////////////////
//
// TprCustomHPageHeaderBand
//
/////////////////////////////////////////////////
{Base class for the horizontal page headers.}
TprCustomHPageHeaderBand = class(TprCustomHBand)
private
  FPrintOnFirstPage : boolean;
protected
  procedure OnDsgnPopupMenuClick(Sender : TObject);
public
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean); override;
published
{Gets or sets the value indicating should the page header on first page is printed or not.}
  property PrintOnFirstPage: Boolean read FPrintOnFirstPage write FPrintOnFirstPage default false;
end;

/////////////////////////////////////////////////
//
// TprCustomHPageFooterBand
//
/////////////////////////////////////////////////
{Base class for the horizontal page footers.}
TprCustomHPageFooterBand = class(TprCustomHBand)
private
  FPrintOnFirstPage: Boolean;
  FPrintAfterLastBandOnPage: Boolean;
  FPrintOnLastPage: Boolean;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
protected
  procedure CalcdPageRect(var CurPageRect : TRect; ExData : pointer); override;
public
{Creates an instance of the TprCustomHPageFooterBand class.
Parameters:
  AOwner - The component - owner.}
  constructor Create(AOwner: TComponent); override;
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: boolean); override;
published
{Gets or sets the value indicating should the page footer on first page is printed or not.}
  property PrintOnFirstPage: Boolean read FPrintOnFirstPage write FPrintOnFirstPage default false;
{Gets or sets the value indicating should the page footer on last page is printed or not.}
  property PrintOnLastPage: Boolean read FPrintOnLastPage write FPrintOnLastPage default true;
{Gets or sets the value indicatings should the page footer is printed directly
after last band on the page or at the page bottom.}
  property PrintAfterLastBandOnPage: Boolean read FPrintAfterLastBandOnPage write FPrintAfterLastBandOnPage default false;
end;

/////////////////////////////////////////////////
//
// TprCustomHDetailBand
//
/////////////////////////////////////////////////
{Base class for the horizontal details.
This band is typically used for the printing of the dataset records,
it printed once for every record/row in the connected dataset.
Use DataSetName property to specify the connected dataset.
Property Valid allows to limit amount of printed records,
it may contains formula which is calculated for all record in the dataset,
if formula returns the false value then the printing of the dataset is stopped.<br>
  For example:<br>
    DetailBand.Valid := 'DataSet.LineNo() <= 10';<br>
    With this formula only the first ten records of the dataset will be printed.
Use ColCount and ColDirection properties to create a multicolumn reports.
See also:
  DataSetName, Valid}
TprCustomHDetailBand = class(TprCustomHBand)
private
  FParentDetail: TprCustomHDetailBand;
  FDataSetName: string;
  FColCount: integer;
  FColDirection: TprColDirectionType;
  FValid: string;
  FGroupsNames : TStrings; // list of groups linked to this band
  FBandsNames : TStrings;
  FDataSet : TprDatasetLink;
  FGroups : TprGroups;
  FBands : TprBands;
  FStartNewPage: Boolean;
  FPrintWithChildDetail: TprCustomHDetailBand;

  procedure SetParentDetail(Value : TprCustomHDetailBand);
  procedure SetPrintWithChildDetail(Value: TprCustomHDetailBand);
  procedure SetValid(Value : string);
protected
  procedure FillGenInfo(Info : TprGenBandInfo); override;
  procedure GroupRemoved(Group : TprGroup); override;
  procedure GroupAdded(Group : TprGroup); override;

  procedure ReadGroups(Reader : TReader);
  procedure WriteGroups(Writer : TWriter);
  procedure ReadBands(Reader : TReader);
  procedure WriteBands(Writer : TWriter);
  procedure DefineProperties(Filer : TFiler); override;

  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);

  procedure CalcdPageRect(var CurPageRect : TRect; ExData : pointer); override;
  // generate report
  function GenerateCell(CallerBand : TprBand; CallbackProc : TGenerateCellCallbackProc) : TprGenCell; override;
  procedure InitDataSet; override;
public
{Creates an instance of the TprCustomHDetailBand.
AOwner - The component - owner.}
  constructor Create(AOwner : TComponent); override;
{Frees an instance of the TprCustomHDetailBand.}
  destructor Destroy; override;

  property DataSet: TprDatasetLink read FDataset; // initializated in prepare report
{Represents the list of groups which are linked to this band.}
  property Groups: TprGroups read FGroups;
{Represents the list of bands which are linked to this band.
This list may contains bands of the such types:<br>
  Detail header<br>
  Detail footer<br>
  Detail
See also:
  TprCustomHDetailHeaderBand, TprCustomHDetailFooterBand, ParentDetail}
  property Bands: TprBands read FBands; 

{See:
  TprBand.OnInsertIntoPage}
  procedure OnInsertIntoPage(p : TprCustomPage); override;
{See:
  TprBand.GetDrawDesignCaption}
  function GetDrawDesignCaption : string; override;
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean); override;

{See:
  TprBand.AfterReportLoaded}
  procedure AfterReportLoaded; override;
published
{Gets or sets the name of sub-report that should be inserted after this band.}
  property SubReportName;
{Gets or sets the value indicating whether the band can split should it fall on a page break,
has the false value by default - band can not be splitted.}
  property CanSplit;
{Gets or sets the name of dataset attached to this band.
If the dataset is in the other form this name must contains the full
qualified name, for example:
  DataModule.MyDataset.}
  property DataSetName: string read FDataSetName write FDataSetName;
{Gets or sets the value indicating the columns creating mode.
See also:
  ColDirection}
  property ColCount: integer read FColCount write FColCount default 0;
{Gets or sets the value indicating the creation mode of columns.
See also:
  TprColDirectionType, ColCount}
  property ColDirection: TprColDirectionType read FColDirection write FColDirection default prcdTopBottomLeftRight;
{Gets or sets the parent detail band, can be used for create the multilevel reports
(Master-detail).
If the detail band has the child bands then these bands are listed in its Bands property.}
  property ParentDetail: TprCustomHDetailBand read FParentDetail write SetParentDetail;
{Gets or sets the formula which is calculated for all dataset's records,
if this formula returns the false value then the printing of the dataset will be stopped.
  For example:
    DetailBand.Valid := 'DataSet.LineNo() <= 10';
    With this formula only the first ten records of the dataset will be printed.}
  property Valid: string read FValid write SetValid;
{Gets or sets the value indicating that all records must be printed from the new page.}
  property StartNewPage: Boolean read FStartNewPage write FStartNewPage default False;
{Gets or sets the child detail band with which this detail band must be printed
on one page to avoid situations in which the master details is printed without
the child details.}
  property PrintWithChildDetail: TprCustomHDetailBand read FPrintWithChildDetail write SetPrintWithChildDetail;
end;

/////////////////////////////////////////////////
//
// TprCustomHDetailHeaderBand
//
/////////////////////////////////////////////////
{Base class for horizontal detail headers.
The detail header band prints before the linked detail band.
Use DetailBand property for linking header with detail band.
See also:
  TprCustomHDetailBand, TprCustomHDetailFooterBand}
TprCustomHDetailHeaderBand = class(TprCustomHBand)
private
  FDetailBand : TprCustomHDetailBand;
  FColCount : integer;
  FColDirection : TprColDirectionType;
  FReprintOnEachPage : boolean;

  FLinkToDetail : boolean;

  procedure SetDetailBand(Value : TprCustomHDetailBand);
protected
  procedure FillGenInfo(Info : TprGenBandInfo); override;
  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
public
{See:
  TprBand.OnInsertIntoPage}
  procedure OnInsertIntoPage(p : TprCustomPage); override;
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: boolean); override;
{See:
  TprBand.GetDrawDesignCaption}
  function GetDrawDesignCaption: string; override;
published
{Gets or sets the name of sub-report that should be inserted after this band.}
  property SubReportName;
{Gets or sets the value indicating whether the band can split should it fall on a page break,
has the false value by default - band can not be splitted.}
  property CanSplit;
{Gets or sets the detail band for which this header must be printed.
You MUST define this property for the correct work.}
  property DetailBand: TprCustomHDetailBand read FDetailBand write SetDetailBand;

{Gets or sets the number of columns, all columns have an identical width.
Typically this property must be identical to the ColCount property of
the linked detail band.
See also:
  ColDirection}
  property ColCount: integer read FColCount write FColCount default 0;
{Gets or sets the value indicating the columns creating mode.
Typically this property must be identical to the ColCount property of
the linked detail band.
See also:
  TprColDirectionType, ColCount}
  property ColDirection: TprColDirectionType read FColDirection write FColDirection default prcdTopBottomLeftRight;

{Gets or sets the value indicating that header must be reprinted on each page where
linked detail band is located.}
  property ReprintOnEachPage: Boolean read FReprintOnEachPage write FReprintOnEachPage default False;
{Gets or sets the value indicating that this header must be printed
with its detail band on one page to avoid situations in which
the detail header is printed without details.}
  property LinkToDetail: Boolean read FLinkToDetail write FLinkToDetail default False;
end;

/////////////////////////////////////////////////
//
// TprCustomHDetailFooterBand
//
/////////////////////////////////////////////////
{Base class for horizontal detail footers.
The detail footer band prints after the linked detail band.
Use DetailBand property for linking footer with detail band.
See also:
  TprCustomHDetailBand, TprCustomHDetailHeaderBand}
TprCustomHDetailFooterBand = class(TprCustomHBand)
private
  FDetailBand : TprCustomHDetailBand;
  FColCount : integer;
  FColDirection : TprColDirectionType;

  FLinkToDetail : boolean;
  
  procedure SetDetailBand(Value : TprCustomHDetailBand);
protected
  procedure FillGenInfo(Info : TprGenBandInfo); override;
  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
public
{See:
  TprBand.OnInsertIntoPage}
  procedure OnInsertIntoPage(p : TprCustomPage); override;
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean); override;
{See:
  TprBand.GetDrawDesignCaption}
  function GetDrawDesignCaption : string; override;
published
{Gets or sets the name of sub-report that should be inserted after this band.}
  property SubReportName;
{Gets or sets the value indicating whether the band can split should it fall on a page break,
has the false value by default - band can not be splitted.}
  property CanSplit;
{Gets or sets the detail band for which this footer must be printed.
You MUST define this property for the correct work.}
  property DetailBand: TprCustomHDetailBand read FDetailBand write SetDetailBand;

{Gets or sets the number of columns, all columns have an identical width.
Typically this property must be identical to the ColCount property of
the linked detail band.
See also:
  ColDirection}
  property ColCount : integer read FColCount write FColCount;
{Gets or sets the value indicating the columns creating mode.
Typically this property must be identical to the ColCount property of
the linked detail band.
See also:
  TprColDirectionType, ColCount}
  property ColDirection : TprColDirectionType read FColDirection write FColDirection;

{Gets or sets the value indicating that this footer must be printed
with its detail band on one page to avoid situations in which
the detail footer is printed without details.}
  property LinkToDetail : boolean read FLinkToDetail write FLinkToDetail;
end;

/////////////////////////////////////////////////
//
// TprCustomHGroupHeaderBand
//
/////////////////////////////////////////////////
{Base class for horizontal group headers.
The group header band is printed each time when new group starts.
Use Group property for linking header with a group.
See also:
  TprGroup, TprCustomHGroupFooterBand}
TprCustomHGroupHeaderBand = class(TprCustomHBand)
private
  FGroup: TprGroup;
  FColCount: Integer;
  FColDirection: TprColDirectionType;
  FLinkToDetail: Boolean;
  FStartNewPage: Boolean;
  FReprintOnEachPage: Boolean;
  FMinDataRecords: Integer;
  procedure SetGroup(Value: TprGroup);
protected
  function GetSubBandSearchDirection: Integer; override;
  procedure FillGenInfo(Info: TprGenBandInfo); override;
  procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  procedure OnDsgnPopupMenuClick(Sender: TObject);
public
{Creates an instance of the TprCustomHGroupHeaderBand class}
  constructor Create(AOwner: TComponent); override;
{See:
  TprBand.OnInsertIntoPage}
  procedure OnInsertIntoPage(p : TprCustomPage); override;
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: Boolean); override;
{See:
  TprBand.GetDrawDesignCaption}
  function GetDrawDesignCaption : string; override;
published
{Gets or sets the name of sub-report that should be inserted after this band.}
  property SubReportName;
{Gets or sets the value indicating whether the band can split should it fall on a page break,
has the false value by default - band can not be splitted.}
  property CanSplit;
{Gets or sets the group for which this header must be printed.
You MUST define this property for the correct work.}
  property Group: TprGroup read FGroup write SetGroup;
{Gets or sets the number of columns, all columns have an identical width.
Typically this property must be identical to the ColCount property of
the detail band to which group is linked.
See also:
  ColDirection}
  property ColCount: Integer read FColCount write FColCount;
{Gets or sets the value indicating the columns creating mode.
Typically this property must be identical to the ColCount property of
the detail band to which group is linked.
See also:
  TprColDirectionType, ColCount}
  property ColDirection: TprColDirectionType read FColDirection write FColDirection;

{Gets or sets the value indicating that this group header must be printed
with its detail band on one page to avoid situations in which
the group header is printed without group data.}
  property LinkToDetail: Boolean read FLinkToDetail write FLinkToDetail;
{Gets or sets the value indicating that all groups must be printed from the new page.}
  property StartNewPage: Boolean read FStartNewPage write FStartNewPage;
{Gets or sets the value indicating that the group header must be reprinted on each page where
group data is located.}
  property ReprintOnEachPage: Boolean read FReprintOnEachPage write FReprintOnEachPage;
{Gets or sets the value indicating that the group header must be printed only when the
group data contains at least of the specified amount of records.
By default this value equals to 0 ie the group header will be printed always.}
  property MinDataRecords: Integer read FMinDataRecords write FMinDataRecords default 0;
end;

/////////////////////////////////////////////////
//
// TprCustomHGroupFooterBand
//
/////////////////////////////////////////////////
{Base class for horizontal group footers.
The group footer band is printed each time when new group ends.
Use Group property for linking footer with a group.
See also:
  TprGroup, TprCustomHGroupHeaderBand}
TprCustomHGroupFooterBand = class(TprCustomHBand)
private
  FGroup: TprGroup;
  FColCount: integer;
  FColDirection: TprColDirectionType;
  FLinkToDetail: boolean;
  FMinDataRecords: Integer;
  procedure SetGroup(Value : TprGroup);
protected
  function GetSubBandSearchDirection: Integer; override;
  procedure FillGenInfo(Info : TprGenBandInfo); override;
  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
public
{Creates an instance of the TprCustomHGroupFooterBand class}
  constructor Create(AOwner: TComponent); override;
{See:
  TprBand.OnInsertIntoPage}
  procedure OnInsertIntoPage(p : TprCustomPage); override;
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean); override;
{See:
  TprBand.GetDrawDesignCaption}
  function GetDrawDesignCaption : string; override;
published
{Gets or sets the name of sub-report that should be inserted after this band.}
  property SubReportName;
{Gets or sets the value indicating whether the band can split should it fall on a page break,
has the false value by default - band can not be splitted.}
  property CanSplit;
{Gets or sets the group for which this header must be printed.
You MUST define this property for the correct work.}
  property Group: TprGroup read FGroup write SetGroup;
{Gets or sets the number of columns, all columns have an identical width.
Typically this property must be identical to the ColCount property of
the detail band to which group is linked.
See also:
  ColDirection}
  property ColCount: Integer read FColCount write FColCount default 0;
{Gets or sets the value indicating the columns creating mode.
Typically this property must be identical to the ColCount property of
the detail band to which group is linked.
See also:
  TprColDirectionType, ColCount}
  property ColDirection: TprColDirectionType read FColDirection write FColDirection default prcdTopBottomLeftRight;
{Gets or sets the value indicating that this group footer must be printed
with its detail band on one page to avoid situations in which
the group footer is printed without group data.}
  property LinkToDetail: Boolean read FLinkToDetail write FLinkToDetail default False;
{Gets or sets the value indicating that the group footer must be printed only when the
group data contains at least of the specified amount of records.
By default this value equals to 0 ie the group footer will be printed always.}
  property MinDataRecords: Integer read FMinDataRecords write FMinDataRecords default 0;
end;






/////////////////////////////////////////////////
//
// TprCustomVBand
//
/////////////////////////////////////////////////
{Base class for all vertical bands.}
TprCustomVBand = class(TprBand)
private
  FUseHorizontalBands: Boolean;
  FWidth: Integer;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
protected
  function GetSize: Integer; override;
  procedure GetObjectSizes(AObject: TprObj; var AGenPos, AGenSize, ADesignMin, ADesignMax: Integer); override;
  procedure CalcdPageRect(var CurPageRect : TRect; ExData : pointer); override;
  function GenerateCell(CallerBand : TprBand; CallbackProc : TGenerateCellCallbackProc) : TprGenCell; override;
  procedure BeginSection; override;
  procedure EndSection; override;
public
{See:
  DsgnResize}
  procedure DsgnResize(oTop,oLeft,oBottom,oRight : integer; var ResizeAccepted : boolean; ExData : pointer); override;
{See:
  DsgnLink}
  procedure DsgnLink(Linked : TprDesignComponent; LinkMode : TprLinkType; var LinkAccepted : boolean; ExData : pointer); override;
{See:
  DsgnAllowResizeTypes}
  function DsgnAllowResizeTypes : TprResizeTypeSet; override;
{See:
  DsgnAllowLinkTypes}
  function DsgnAllowLinkTypes : TprLinkTypeSet; override;
{See:
  DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean); override;
published
{Specifies the band's width in pixels.}
  property Width: Integer read FWidth write FWidth;
{Specifies the boolean value which determines how band works in cross-tab reports.
This value can not have the true value for bands:
  btvTitle, btvPageHeader, btvPageFooter}
  property UseHorizontalBands: Boolean read FUseHorizontalBands write FUseHorizontalBands default False;
end;

/////////////////////////////////////////////////
//
// TprCustomVTitleBand
//
/////////////////////////////////////////////////
{Base class for the vertical report titles.}
TprCustomVTitleBand = class(TprCustomVBand)
end;

/////////////////////////////////////////////////
//
// TprCustomVSummaryBand
//
/////////////////////////////////////////////////
{Base class for the vertical report summaries.}
TprCustomVSummaryBand = class(TprCustomVBand)
private
  FPrintWithBand : TprCustomVBand;
protected
  procedure FillGenInfo(Info : TprGenBandInfo); override;
  procedure Notification(AComponent : TComponent; AOperation : TOperation); override;
published
{Gets or sets the value indicating whether the band can split should it fall on a page break,
has the false value by default - band can not be splitted.}
  property CanSplit;
{Specifies the vertical band which must be printed on the one page with the
report summary band. Use this property for avoiding the situations in which
the report summary is printed without data.}
  property PrintWithBand : TprCustomVBand read FPrintWithBand write FPrintWithBand;
end;

/////////////////////////////////////////////////
//
// TprVPageHeaderBand
//
/////////////////////////////////////////////////
{Base class for the horizontal page headers.}
TprCustomVPageHeaderBand = class(TprCustomVBand)
private
  FPrintOnFirstPage : boolean;
protected
  procedure OnDsgnPopupMenuClick(Sender : TObject);
public
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean); override;
published
{Gets or sets the value indicating should the page header on first page is printed or not.}
  property PrintOnFirstPage: Boolean read FPrintOnFirstPage write FPrintOnFirstPage default False;
end;

/////////////////////////////////////////////////
//
// TprCustomVPageFooterBand
//
/////////////////////////////////////////////////
{Base class for the vertical page footers.}
TprCustomVPageFooterBand = class(TprCustomVBand)
private
  FPrintOnFirstPage: Boolean;
  FPrintOnLastPage: Boolean;
  FPrintAfterLastBandOnPage: Boolean;
protected
  procedure OnDsgnPopupMenuClick(Sender : TObject);
  procedure CalcdPageRect(var CurPageRect : TRect; ExData : pointer); override;
public
{Creates an instance of the TprCustomHPageFooterBand class.
Parameters:
  AOwner - The component - owner.}
  constructor Create(AOwner: TComponent); override;
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean); override;
published
{Gets or sets the value indicating should the page footer on first page is printed or not.}
  property PrintOnFirstPage: Boolean read FPrintOnFirstPage write FPrintOnFirstPage default False;
{Gets or sets the value indicating should the page footer on last page is printed or not.}
  property PrintOnLastPage: Boolean read FPrintOnLastPage write FPrintOnLastPage default True;
{Gets or sets the value indicatings should the page footer is printed directly
after last band on the page or at the page right edge.}
  property PrintAfterLastBandOnPage: Boolean read FPrintAfterLastBandOnPage write FPrintAfterLastBandOnPage default False;
end;

/////////////////////////////////////////////////
//
// TprCustomVDetailBand
//
/////////////////////////////////////////////////
{Base class for the vertical details.
This band is typically used for the creating the cross-tab reports,
it generates columns of the cross-tab table and
printed once for every record/row in the connected dataset.
Use DataSetName property to specify the connected dataset.
Property Valid allows to limit amount of printed records,
it may contains formula which is calculated for all record in the dataset,
if formula returns the false value then the printing of the dataset is stopped.
  For example:
    VerticalDetailBand.Valid := 'VerticalDataSet.LineNo() <= 10';
    With this formula only the first ten records of the dataset will be printed.
See also:
  DataSetName, Valid}
TprCustomVDetailBand = class(TprCustomVBand)
private
  FParentDetail : TprCustomVDetailBand;
  FDataSetName  : string;
  FValid : string;
  FGroupsNames : TStrings;  // list of groups linked to this band
  FBandsNames : TStrings;
  FDataset : TprDatasetLink;
  FGroups : TprGroups;
  FBands : TprBands;
  FStartNewPage: Boolean;
  FPrintWithChildDetail: TprCustomVDetailBand;

  procedure SetParentDetail(Value : TprCustomVDetailBand);
  procedure SetPrintWithChildDetail(Value: TprCustomVDetailBand);
  procedure SetValid(Value : string);
protected
  procedure GroupRemoved(Group : TprGroup); override;
  procedure GroupAdded(Group : TprGroup); override;

  procedure ReadGroups(Reader : TReader);
  procedure WriteGroups(Writer : TWriter);
  procedure ReadBands(Reader : TReader);
  procedure WriteBands(Writer : TWriter);

  procedure Notification(AComponent : TComponent; Operation : TOperation); override;

  procedure DefineProperties(Filer : TFiler); override;
  procedure FillGenInfo(Info : TprGenBandInfo); override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);

  procedure CalcdPageRect(var CurPageRect : TRect; ExData : pointer); override;
  function GenerateCell(CallerBand: TprBand; CallbackProc: TGenerateCellCallbackProc) : TprGenCell; override;
  procedure InitDataSet; override;
public
{Creates an instance of the TprCustomVDetailBand.
AOwner - The component - owner.}
  constructor Create(AOwner : TComponent); override;
{Frees an instance of the TprCustomVDetailBand.}
  destructor Destroy; override;

  property Dataset: TprDatasetLink read FDataset; // initializated in report generate
{Represents the list of groups which are linked to this band.}
  property Groups: TprGroups read FGroups;
{Represents the list of bands which are linked to this band.
This list may contains bands of the such types:
  Detail header,
  Detail footer,
  Detail
See also:
  TprCustomVDetailHeaderBand, TprCustomVDetailFooterBand, ParentDetail}
  property Bands: TprBands read FBands;

{See:
  TprBand.OnInsertIntoPage}
  procedure OnInsertIntoPage(p: TprCustomPage); override;
{See:
  TprDesignComponent.GetDrawDesignCaption}
  function  GetDrawDesignCaption: string; override;
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: Boolean); override;

{See:
  TprBand.AfterReportLoaded}
  procedure AfterReportLoaded; override;
published
{Gets or sets the value indicating whether the band can split should it fall on a page break,
has the false value by default - band can not be splitted.}
  property CanSplit;
{Gets or sets the name of dataset attached to this band.
If the dataset is in the other form this name must contains the full
qualified name, for example:
  DataModule.MyDataset.}
  property DataSetName: string read FDataSetName write FDataSetName;
{Gets or sets the parent detail band, can be used for create the multilevel reports
(Master-detail).
If the detail band has the child bands then these bands are listed in its Bands property.}
  property ParentDetail: TprCustomVDetailBand read FParentDetail write SetParentDetail;
{Gets or sets the formula which is calculated for all dataset's records,
if this formula returns the false value then the printing of the dataset will be stopped.
  For example:
    VerticalDetailBand.Valid := 'VerticalDataSet.LineNo() <= 10';
    With this formula only the first ten records of the dataset will be printed.}
  property Valid: string read FValid write SetValid;
{Gets or sets the value indicating that all records must be printed from the new page.}
  property StartNewPage: Boolean read FStartNewPage write FStartNewPage default False;
{Gets or sets the child detail band with which this detail band must be printed
on one page to avoid situations in which the master details is printed without
the child details.}
  property PrintWithChildDetail: TprCustomVDetailBand read FPrintWithChildDetail write SetPrintWithChildDetail;
end;

/////////////////////////////////////////////////
//
// TprCustomVDetailHeaderBand
//
/////////////////////////////////////////////////
{Base class for vertical detail headers.
The detail header band prints before the linked detail band.
Use DetailBand property for linking header with detail band.
See also:
  TprCustomVDetailBand, TprCustomVDetailFooterBand}
TprCustomVDetailHeaderBand = class(TprCustomVBand)
private
  FDetailBand : TprCustomVDetailBand;
  FReprintOnEachPage : boolean;
  FLinkToDetail : boolean;
  procedure SetDetailBand(Value : TprCustomVDetailBand);
protected
  procedure FillGenInfo(Info : TprGenBandInfo); override;
  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
public
{See:
  TprBand.OnInsertIntoPage}
  procedure OnInsertIntoPage(p : TprCustomPage); override;
{See:
  TprBand.GetDrawDesignCaption}
  function GetDrawDesignCaption : string; override;
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean); override;
published
{Gets or sets the value indicating whether the band can split should it fall on a page break,
has the false value by default - band can not be splitted.}
  property CanSplit;
{Gets or sets the vertical detail band for which this header must be printed.
You MUST define this property for the correct work.}
  property DetailBand: TprCustomVDetailBand read FDetailBand write SetDetailBand;
{Gets or sets the value indicating that header must be reprinted on each page where
linked detail band is located.}
  property ReprintOnEachPage: boolean read FReprintOnEachPage write FReprintOnEachPage default False;
{Gets or sets the value indicating that this header must be printed
with its detail band on one page to avoid situations in which
the detail header is printed without details.}
  property LinkToDetail: boolean read FLinkToDetail write FLinkToDetail default False;
end;

/////////////////////////////////////////////////
//
// TprCustomVDetailFooterBand
//
/////////////////////////////////////////////////
{Base class for vertical detail footers.
The detail footer band prints after the linked detail band.
Use DetailBand property for linking footer with detail band.
See also:
  TprCustomVDetailBand, TprCustomVDetailHeaderBand}
TprCustomVDetailFooterBand = class(TprCustomVBand)
private
  FDetailBand: TprCustomVDetailBand;
  FLinkToDetail: boolean;
  procedure SetDetailBand(Value : TprCustomVDetailBand);
protected
  procedure FillGenInfo(Info : TprGenBandInfo); override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
public
{See:
  TprBand.OnInsertIntoPage}
  procedure OnInsertIntoPage(p : TprCustomPage); override;
{See:
  TprBand.GetDrawDesignCaption}
  function GetDrawDesignCaption : string; override;
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean); override;
published
{Gets or sets the value indicating whether the band can split should it fall on a page break,
has the false value by default - band can not be splitted.}
  property CanSplit;
{Gets or sets the vertical detail band for which this footer must be printed.
You MUST define this property for the correct work.}
  property DetailBand: TprCustomVDetailBand read FDetailBand write SetDetailBand;

{Gets or sets the value indicating that this footer must be printed
with its detail band on one page to avoid situations in which
the detail footer is printed without details.}
  property LinkToDetail: Boolean read FLinkToDetail write FLinkToDetail default False;
end;

/////////////////////////////////////////////////
//
// TprCustomVGroupHeaderBand
//
/////////////////////////////////////////////////
{Base class for vertical group headers.
The group header band is printed each time when new group starts.
Use Group property for linking header with a group.
See also:
  TprGroup, TprCustomVGroupFooterBand}
TprCustomVGroupHeaderBand = class(TprCustomVBand)
private
  FGroup: TprGroup;
  FLinkToDetail: boolean;
  FStartNewPage: boolean;
  FReprintOnEachPage: Boolean;
  FMinDataRecords: Integer;
  procedure SetGroup(Value : TprGroup);
protected
  function GetSubBandSearchDirection: Integer; override;
  procedure FillGenInfo(Info : TprGenBandInfo); override;
  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
public
{Creates an instance of the TprCustomVGroupHeaderBand class}
  constructor Create(AOwner: TComponent); override;
{See:
  TprBand.OnInsertIntoPage}
  procedure OnInsertIntoPage(p : TprCustomPage); override;
{See:
  TprBand.GetDrawDesignCaption}
  function GetDrawDesignCaption: string; override;
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: boolean); override;
published
{Gets or sets the value indicating whether the band can split should it fall on a page break,
has the false value by default - band can not be splitted.}
  property CanSplit;
{Gets or sets the group for which this header must be printed.
You MUST define this property for the correct work.}
  property Group: TprGroup read FGroup write SetGroup;
{Gets or sets the value indicating that this group header must be printed
with its detail band on one page to avoid situations in which
the group header is printed without group data.}
  property LinkToDetail: Boolean read FLinkToDetail write FLinkToDetail default False;
{Gets or sets the value indicating that all groups must be printed from the new page.}
  property StartNewPage: Boolean read FStartNewPage write FStartNewPage default False;
{Gets or sets the value indicating that the group header must be reprinted on each page where
group data is located.}
  property ReprintOnEachPage: Boolean read FReprintOnEachPage write FReprintOnEachPage default False;
{Gets or sets the value indicating that the group header must be printed only when the
group data contains at least of the specified amount of records.
By default this value equals to 0 ie the group header will be printed always.}
  property MinDataRecords: Integer read FMinDataRecords write FMinDataRecords default 0;
end;

/////////////////////////////////////////////////
//
// TprCustomVGroupFooterBand
//
/////////////////////////////////////////////////
{Base class for vertical group footers.
The group footer band is printed each time when new group ends.
Use Group property for linking footer with a group.
See also:
  TprGroup, TprCustomVGroupHeaderBand}
TprCustomVGroupFooterBand = class(TprCustomVBand)
private
  FGroup: TprGroup;
  FLinkToDetail: Boolean;
  FMinDataRecords: Integer;
  procedure SetGroup(Value : TprGroup);
protected
  function GetSubBandSearchDirection: Integer; override;
  procedure FillGenInfo(Info : TprGenBandInfo); override;
  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
  procedure OnDsgnPopupMenuClick(Sender : TObject);
public
{Creates an instance of the TprCustomVGroupFooterBand class}
  constructor Create(AOwner: TComponent); override;
{See:
  TprBand.OnInsertIntoPage}
  procedure OnInsertIntoPage(p : TprCustomPage); override;
{See:
  TprBand.GetDrawDesignCaption}
  function GetDrawDesignCaption : string; override;
{See:
  TprDesignComponent.DsgnDefinePopupMenu}
  procedure DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean); override;
published
{Gets or sets the value indicating whether the band can split should it fall on a page break,
has the false value by default - band can not be splitted.}
  property CanSplit;
{Gets or sets the group for which this footer must be printed.
You MUST define this property for the correct work.}
  property Group: TprGroup read FGroup write SetGroup;
{Gets or sets the value indicating that this group footer must be printed
with its detail band on one page to avoid situations in which
the group footer is printed without group data.}
  property LinkToDetail: boolean read FLinkToDetail write FLinkToDetail default False;
{Gets or sets the value indicating that the group footer must be printed only when the
group data contains at least of the specified amount of records.
By default this value equals to 0 ie the group footer will be printed always.}
  property MinDataRecords: Integer read FMinDataRecords write FMinDataRecords default 0;
end;

/////////////////////////////////////////////////
//
// TprBands
//
/////////////////////////////////////////////////
{Represents the list of TprBand objects.
Contains methods and properties for finding bands by name and type.}
TprBands = class(TList)
private
  function GetItm(index : integer) : TprBand;
  function GetByBandType(BandType : TprBandType) : TprBand;
  function GetByName(Name : string) : TprBand;
public
{Lists the bands references.
Use Items to obtain a pointer to a specific band in the array.
The Index parameter indicates the index of the band, where 0 is the index of the
first band, 1 is the index of the second band, and so on.
Parameters:
  Index - The index of the band in the array.}
  property Items[index: Integer]: TprBand read GetItm; default;
{Returns a band by its type, if the list contains not one band with such type
the first band is returned. If band with such type is not found the exception is raised.
Parameters:
  BandType - The type of the band.}
  property ByBandType[BandType: TprBandType]: TprBand read GetByBandType;
{Returns a band by its name.
If band with such name is not found the exception is raised.
Parameters:
  Name - The name of the band.}
  property ByName[Name: string]: TprBand read GetByName;

{Returns the position of a band by its type.
Return -1 if band is not found. 
Parameters:
  BandType - The type of the band.}
  function IndexByBandType(BandType: TprBandType): Integer;
{Returns the position of a band by its name.
Return -1 if band is not found. 
Parameters:
  Name - The name of the band.}
  function IndexByName(Name: string): Integer;
end;

{Describes an internal state of the group, for internal use.
Syntax:
  TprGroupState = (prgsHeaders, prgsFooters, prgsFootersAlways)}
TprGroupState = (prgsHeaders, prgsFooters, prgsFootersAlways);
/////////////////////////////////////////////////
//
// TprGroup
//
/////////////////////////////////////////////////
{Describes the group in the PReport.
The group is described by:
<ul>
<li>The group name, each group must have unique name in the report template.</li>
<li>The group detail band (a band of type TprCustomHDetailBand or TprCustomVDetailBand),
specifies the detail band, which prints the records which must be grouped.</li>
<li>The group expression, this expression contains the formula, the return value of
    this formula is calculated when the moving to the new dataset record occurs,
    if the formula's value is changed then a new group starts.</li>
</ul>
See also:
  TprCustomHDetailBand, TprCustomVDetailBand}
TprGroup = class(TComponent)
private
  FValid: string;
  FDetailBand: TprBand;

  FGroupState: TprGroupState;
  FPrevValue: Variant;
  FPrevGroupValue: Variant;

  FNeedHeaders: Boolean;
  FNeedFooters: Boolean;
  FLineNo: Integer;

  FReport: TprCustomReport;
  FHeaders: TprBands;
  FFooters: TprBands;

  procedure SetReport(Value: TprCustomReport);
  procedure SetDetailBand(Value: TprBand);
  function GetGroupValue: Variant;
protected
  procedure SetParentComponent(Value: TComponent); override;
  function GetChildOwner: TComponent; override;
  procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

  procedure Reset;
  procedure CalcValue;
  procedure HeadersGenerateCell(CallerBand: TprBand; CallbackProc: TGenerateCellCallbackProc);
  procedure FootersGenerateCell(CallerBand: TprBand; CallbackProc: TGenerateCellCallbackProc);
  procedure FootersAlwaysGenerateCell(CallerBand: TprBand; CallbackProc: TGenerateCellCallbackProc);

  property GroupEnded: Boolean read FNeedFooters;
public
{Creates an instance of the TprGroup component.
Parameters:
  AOwner - The component - owner.}
  constructor Create(AOwner: TComponent); override;
{Frees an instance of the TprGroup class.}
  destructor Destroy; override;

{Contains the list of the group headers.}
  property Headers: TprBands read FHeaders;
{Contains the list of the group footers.}
  property Footers: TprBands read FFooters;
{Returns the number of the current line in the group.
You can have access to this property from the parser, for example, in TprMemoObj:
  [GroupName.LineNo()]}
  property LineNo: Integer read FLineNo write FLineNo;
{Returns the TprCustomReport object which contains this group.}
  property Report: TprCustomReport read FReport write SetReport;
{Returns the index of the group in the TprCustomReport.Groups property.}
  function IndexInReport: Integer;

  function HasParent: Boolean; override;
  function GetParentComponent: TComponent; override;

{Returns the current group value. For example:<br>
We have a dataset with records:<br>
RecNo  FieldValue<br>
-----  ----------<br>
   1   aa<br>
   2   aa<br>
   3   aa<br>
   4   bb<br>
   5   bb<br>
   6   cc<br>
The GroupValue property returns:<br>
  - "aa" for records 1, 2, 3<br>
  - "bb" for records 4, 5<br>
  - "cc" for record 6.<br>
You can have access to this property from the parser, for examplem in TprMemoObj:<br>
  [GroupName.GroupValue()]}
  property GroupValue: Variant read GetGroupValue;
published
{Specifies the group expression, this expression contains the formula, the return value of
this formula is calculated when the moving to the new dataset record occurs,
if the formula's value is changed then a new group starts.
This expression usually uses the dataset field, but can have a formula, for example:
  Group.Valid := 'Copy(Dataset.Field, 1, 1)' // records are grouped by the first letter of the Dataset.Field field.}
  property Valid: string read FValid write FValid;
{Specifies the detail band to which group is linked.
The band can have a type: TprCustomHDetailBand or TprCustomVDetailBand.}
  property DetailBand: TprBand read FDetailBand write SetDetailBand;
end;

/////////////////////////////////////////////////
//
// TprGroups
//
/////////////////////////////////////////////////
{Represents the list of the TprGroup objects.
See also:
  TprGroups}
TprGroups = class(TList)
private
  function GetItm(index : integer) : TprGroup;
  function GetByName(Name : string) : TprGroup;
public
{Lists the groups references.
Use Items to obtain a pointer to a specific group in the array.
The Index parameter indicates the index of the group, where 0 is the index of the
first band, 1 is the index of the second group, and so on.
Parameters:
  Index - The index of the group in the array.}
  property Items[index : integer] : TprGroup read GetItm; default;
{Returns a group by its name.
If group with such name is not found the exception is raised.
Parameters:
  Name - The name of the group.}
  property ByName[Name : string] : TprGroup read GetByName;
{Returns the position of a group by its name.
Return -1 if group is not found. 
Parameters:
  Name - The name of the group.}
  function IndexByName(Name : string) : integer;
end;

/////////////////////////////////////////////////
//
// TprValueVersion
//
/////////////////////////////////////////////////
{Internal class.}
TprValueVersion = class(TObject)
private
  FNotUseInAccumulate: boolean;
  V1: Variant;
  V2: integer;
  Value: TprValue;
  ID: integer;   // ident
  V: Variant;
public
  property VersionValue: Variant read V;
end;

/////////////////////////////////////////////////
//
// TprSavedValue
//
/////////////////////////////////////////////////
{Internal class.}
TprSavedValue = class(TObject)
private
  V: TprVarValue;
  Cell: TprGenCell;
end;

{Describes the scope of the using of the aggregate value.
Items:
  rvtReport - The aggregate value is calculated over all report.
  rvtGroup - The aggregate value is calculated over group.
  rvtPage - The aggregate value is calculated over page.
  rvtDataSetEof - The aggregate value is calculated over dataset.
Syntax:
  TprResetValueType = (rvtReport, rvtGroup, rvtPage, rvtDataSetEof)}
TprResetValueType = (rvtReport, rvtGroup, rvtPage, rvtDataSetEof);
{Describes the moment when the aggregate value is recalculated.
Items:
  cvtDataSetNext - The aggregate value is recalculated when the moving on the dataset occurs.
  cvtEventOnReset - For internal use.
  cvtCrossTab - Use this item in cross-tab reports when aggregate value must be calculated
over horizontal.
Syntax:
  TprCalcValueType  = (cvtDataSetNext, cvtEventOnReset, cvtCrossTab)}
TprCalcValueType  = (cvtDataSetNext, cvtEventOnReset, cvtCrossTab);
{Describes the type of the aggregate function.
Items:
  prafSum - Calculates the sum of values.
  prafCount - Calculates the amount of items.
  prafAvg - Calculates the average value for the set of values.
  prafMin - Calculates the minimum value from the set of values.
  prafMax - Calculates the maximum value from the set of values.
Syntax:
  TprAggFunction = (prafSum, prafCount, prafAvg, prafMin, prafMax)}
TprAggFunction = (prafSum, prafCount, prafAvg, prafMin, prafMax);

{For internal use.}
TprOnValueCalc = procedure (Value: TprValue) of object;

{For internal use.}
TprOnGetVersionByVersionID = procedure (ValueVersion: TprValueVersion) of object;
/////////////////////////////////////////////////
//
// TprValue
//
/////////////////////////////////////////////////
{Represents the aggregate value, which can be used for calculating the report summary, the group summary and so on.
The report template contains list of the TprValue objects,
each object calculates the separate aggregate value.
Example:
  var
    AReport: TprReport;
    AValue: TprValue;
  begin
    // Creating the aggregate value that calculates the report summary
    AValue := AReport.Values.Add;
    // Name of the variable
    AValue.Name := 'SummaryOfSumField';
    // calculates a summary
    AValue.AggFunction := prafSum;
    // calculate a summary over all report.
    AValue.ResetOn := rvtReport;
    AValue.DataSetName := 'Orders';
    // calculates a summary for the Sum field of the Orders dataset.
    AValue.Formula := 'Orders.Sum'

    // Creating the aggregate value that calculates the group summary
    AValue := AReport.Values.Add;
    // Name of the variable
    AValue.Name := 'GroupSumOfSumField';
    // calculates a summary
    AValue.AggFunction := prafSum;
    // calculate a summary over group.
    AValue.ResetOn := rvtGroup;
    AValue.Group := AReport.Groups.ByName['CustomerGroup'];
    AValue.DataSetName := 'Orders';
    // calculates a summary for the Sum field of the Orders dataset.
    AValue.Formula := 'Orders.Sum'
  end;}
TprValue = class(TCollectionItem)
private
  FName : string;
  FAggFunction : TprAggFunction;
  FAccumulate : boolean;
  FFormula : string;
  FResetOn : TprResetValueType;
  FCalcOn : TprCalcValueType;
  FDataSetName : string;
  FResetDataSetName : string;
  FCrossTabHorzDataSetName : string;
  FGroup : TprGroup;
  FVersions : TList;
  FCurrentValueExists : boolean;
  FSavedValues : TList;
  FDataSet : TprDatasetLink;
  FResetDataSet : TprDatasetLink;
  FCrossTabHorzDataSet : TprDatasetLink;
  CrossTabSavedIndex : integer;   // used in CrossTab
  CrossTabColsCount : integer; // used in CrossTab
  OnCalc : TprOnValueCalc; // cvtEventOnReset
  OnGetVersionByVersionID : TprOnGetVersionByVersionID;
  function GetVersion(index: Integer) : TprValueVersion;
  procedure SetCurrentValue(Value: Variant);
  procedure SetName(Value: string);
  procedure SetFormula(Value: string);
  function AddSavedValue: TprSavedValue;
  procedure InternalCalcValue(ver: TprValueVersion; V: TprVarValue);
  function GetReport: TprCustomReport;
  function GetValue: Variant;
protected
  function VersionsCount: Integer;
  function GetCurrentVersion: TprValueVersion; overload;
  function GetCurrentVersion(var fCreated: boolean) : TprValueVersion; overload;
  function GetCurrentVersionID: string;
  function VersionByVersionID(ID: integer) : TprValueVersion;
  procedure Init;
  procedure Reset;
  procedure Clear;
  procedure Calculate(Cell : TprGenCell);

  property Versions[index: Integer] : TprValueVersion read GetVersion;
  property CurrentValue: Variant write SetCurrentValue;
public
{Creates an instance of the TprGroup class.
Do not call this contructor directly, use the TprCustomReport.Values.Add method instead.}
  constructor Create(Collection: TCollection); override;
{Frees an instance of the TprGroup class.}
  destructor Destroy; override;

{Copies the contents of another, similar object.
Parameters:
  Source - The source object.}
  procedure Assign(Source: TPersistent); override;

{Returns the TprCustomReport object containing this group.}
  property Report: TprCustomReport read GetReport;
  property DataSet: TprDatasetLink read FDataSet;
  property ResetDataSet: TprDatasetLink read FResetDataSet;
  property CrossTabHorzDataSet: TprDatasetLink read FCrossTabHorzDataSet;
published
{Returns the current value of aggregate variable, this property
can be used during the generating of the report only.}
  property Value: Variant read GetValue;
{Specifies the group over which the aggregate value is being calculated.
This property is used only when ResetOn equals to rvtGroup.
See also:
  TprGroup}
  property Group: TprGroup read FGroup write FGroup;
{Specifies the name of the aggregate value, this name must be unique in
the report template.}
  property Name: string read FName write SetName;
{Specifies the aggregate function which is used for calculating a value.
This function is applied to the each value which is being returned after calculating
the formula which is specified by the Formula property.}
  property AggFunction: TprAggFunction read FAggFunction write FAggFunction default prafSum;
{Specifies the formula which calculates the separate value which is passed for the
aggregate function.
This formula is not used when AggFunction equals to prafCount.}
  property Formula: string read FFormula write SetFormula;
{Specifies the scope of the using of the aggregate value.
See also:
  TprResetValueType}
  property ResetOn: TprResetValueType read FResetOn write FResetOn;
{Specifies the moment when the aggregate value is recalculated.
See also:
  TprCalcValueType}
  property CalcOn: TprCalcValueType read FCalcOn write FCalcOn;
{Specifies the name of the dataset moving on which causes the recalculation of the aggregate value.
This property is used when CalcOn = cvtDataSetNext.
See also:
  CalcOn}
  property DataSetName: string read FDataSetName write FDataSetName;
{Specifies the name of the dataset over which the aggregate value is being calculated.
This property is used when ResetOn = rvtDataSetEof.
See also:
  ResetOn}
  property ResetDataSetName: string read FResetDataSetName write FResetDataSetName;
{Specifies the name of the horizontal dataset when the aggregate value is calculated over vertical.
This property is used wher CalcOn = cvtCrossTab.
See also:
  CalcOn}
  property CrossTabHorzDataSetName: string read FCrossTabHorzDataSetName write FCrossTabHorzDataSetName;
{Gets or sets the value indicating when the aggregate value must be calculated with accumulating.}
  property Accumulate: boolean read FAccumulate write FAccumulate default false;
end;

/////////////////////////////////////////////////
//
// TprValues
//
/////////////////////////////////////////////////
{Represents the list of the TprValue objects.
See also:
  TprValue}
TprValues = class(TCollection)
private
  FReport: TprCustomReport;
  function GetItm(index : integer) : TprValue;
  function GetByName(name : string) : TprValue;
protected
  function VersionByVersionID(const id: string) : TprValueVersion;
public
{Creates a new TprValue instance and adds it to the items.
Return value:
  Returns the created TprValue object.}
  function Add: TprValue;
{Searches the TprValue object by its name and returs its index.
Parameters:
  Name - The name of the TprValue object.
Return value:
  Returns the index of the TprValue object.
See also:
  TprValue, TprValue.Name}
  function IndexByName(const Name: string) : integer;
{Returns the TprCustomReport object containing this object.}
  property Report: TprCustomReport read FReport;
{Lists the values in the collection.
Parameters:
  Index - The index of the object.}
  property Items[index: Integer]: TprValue read GetItm; default;
{Returns the TprValue objects by name.
Raises the exception if the object with specified name is not found.
Parameters:
  Name - The name of the TprValue object.}
  property ByName[Name: string]: TprValue read GetByName;
end;

/////////////////////////////////////////////////
//
// TprVariable
//
/////////////////////////////////////////////////
{Represents the simple user-defined variable, which can be used in the report template.
This variables is usually used for holding some constants which can be used in
many places of the report template, they can not be used for calculating the
aggregate values.
Each variable is identified by its name.
See also:
  TprVariables}
TprVariable = class(TCollectionItem)
private
  FName : string;
  FCalculated : boolean;
  FValue : TprVarValue;
  function GetAsString : string;
  function GetAsInteger : integer;
  function GetAsDouble : double;
  function GetAsDateTime : TDateTime;
  function GetAsVariant : Variant;
  function GetIsNull : boolean;
  function GetFormula : string;
  procedure SetAsString(Value : string);
  procedure SetAsInteger(Value : integer);
  procedure SetAsDouble(Value : double);
  procedure SetAsDateTime(Value : TDateTime);
  procedure SetAsVariant(Value : Variant);
  procedure SetIsNull(Value : boolean);
  procedure SetFormula(Value : string);
  function GetVarValue : PprVarValue;
protected
  procedure ReadValue(Reader : TReader);
  procedure WriteValue(Writer : TWriter);
  procedure ReadValueType(Reader : TReader);
  procedure WriteValueType(Writer : TWriter);
  procedure DefineProperties(Filer : TFiler); override;
public
{Creates an instance of the TprVariable class.
Do not call this constructor directly, use the TprVariables.Add method instead.
Parameters:
  Collection - The TprVariables object containing the created object.}
  constructor Create(Collection: TCollection); override;

{Copies the properties of another TprVariable object.
Parameters:
  Source - The TprVariable object - source.}
  procedure Assign(Source: TPersistent); override;

{Returns the true value if variable has null value.}
  property IsNull: Boolean read GetIsNull write SetIsNull;
{Represents the value of a TprVariable object as a string.}
  property AsString: String read GetAsString write SetAsString;
{Represents the value of a TprVariable object as a 32-bit integer.}
  property AsInteger: Integer read GetAsInteger write SetAsInteger;
{Represents the value of a TprVariable object as a double number.}
  property AsDouble: Double read GetAsDouble write SetAsDouble;
{Represents the value of a TprVariable object as a TDateTime value.}
  property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
{Represents the value of a TprVariable object as a Variant.}
  property AsVariant: Variant read GetAsVariant write SetAsVariant;
{Represents the value of a TprVariable object as a formula which will be calculated in the time of report generating.}
  property Formula: string read GetFormula write SetFormula;
{Returns the true value if variable contains a formula.}
  property Calculated: Boolean read FCalculated;
{Returns the value of TprVariable object as a pointer to the TprVarValue structure.
See also:
  PprVarValue, TprVarValue}
  property VarValue: PprVarValue read GetVarValue;
published
{Specifies the name of variable, with this name the variable can be used in the report template.}
  property Name: string read FName write FName;
end;

/////////////////////////////////////////////////
//
// TprVariables
//
/////////////////////////////////////////////////
{Represents the list of the used-defined variables.
This variables is usually used for holding some constants which can be used in
many places of the report template, they can not be used for calculating the
aggregate values.
See also:
  TprVariable, TprValue, TprValues}
TprVariables = class(TCollection)
private
  function GetItm(i: Integer): TprVariable;
  function GetByName(Name: string): TprVariable;
public
{Lists the variables in the collection.
Parameters:
  Index - The index of the variable.}
  property Items[i: Integer]: TprVariable read GetItm; default;
{Returns the TprVariable objects by name.
Raises the exception if the object with specified name is not found.
Parameters:
  Name - The name of the TprValue object.}
  property ByName[Name: string]: TprVariable read GetByName;
{Searches the TprVariable object with specified name, returns -1 if object is not found.
Parameters:
  Name - The name of the TprVariable object.
Return value
  Returns the index of the found object or -1.}
  function IndexByName(const Name: string): integer;
{Creates a new TprVariable instance and adds it to the items.
Return value:
  Returns the created TprValue object.}
  function AddVariable: TprVariable; overload;
{Searches the TprVariable object with the specified name (creates a new
object if it is not found) and sets its value.
Parameters:
  AVariableName - The name of the TprVariable object.
  AVariableValue - The new value of the variable.
Return value:
  Returns the created or found TprValue object.}
  function AddVariable(const AVariableName: string; const AVariableValue: Variant): TprVariable; overload;
end;

/////////////////////////////////////////////////
//
// TprCustomEndPage
//
/////////////////////////////////////////////////
{Represents the separate page of the generated report.}
TprCustomEndPage = class(TObject)
private
  FoRecs: TList;
  FReport: TprCustomReport;
  FPageNumber: Integer;
  FPagesCount: Integer;
  FResetPagesCount: Boolean;
  function GetoRec(index: Integer) : TprObjRec;
protected
  function GetWidth: integer; virtual; abstract;
  function GetHeight: integer; virtual; abstract;
  property ResetPagesCount: Boolean read FResetPagesCount write FResetPagesCount;

  procedure ThirdPass; virtual; abstract;
  procedure FreeGeneratedObjects;

  property oRec[index: Integer]: TprObjRec read GetoRec;
  function oRecsCount: Integer;
public
{Creates an instance of the TprCustomEndPage type.
Parameters:
  _Page - The TprCustomPage object producing this object.}
  constructor Create(_Page: TprCustomPage); virtual;
{Creates an instance of the TprCustomEndPage type.
Parameters:
  _Page - The TprCustomReport object containing this object.}
  constructor CreateEmpty(_Report: TprCustomReport); virtual;
{Frees an instance of the TprCustomEndPage type.}
  destructor Destroy; override;

{Returns the TprCustomReport object containg this object.
See also:
  TprCustomReport}
  property Report: TprCustomReport read FReport;
{Returns the width of the page.
The measurement units depends from the type of report, for TprReport - pixels, for TprTxReport - chars.}
  property Width: Integer read GetWidth;
{Returns the height of the page.
The measurement units depends from the type of report, for TprReport - pixels, for TprTxReport - chars.}
  property Height: Integer read GetHeight;
{Returns the page's number, the page numeration starts from 1.}
  property PageNumber: Integer read FPageNumber write FPageNumber;
{Returns the total number of pages in the report.}
  property PagesCount: Integer read FPagesCount write FPagesCount;
end;

/////////////////////////////////////////////////
//
// TprGenCell
//
/////////////////////////////////////////////////
{Internal class, used in the time of the report generating.}
TprGenCell = class(TObject)
private
  FWidth : integer;
  FHeight : integer;
  FObjRecs : TList;
  FPage : TprCustomEndPage;
  FHorzVector : TprGenHorzVector;
  FVertVector : TprGenVertVector;
  FBand : TprBand;
  function GetObjRec(i : integer) : TprObjRec;
  function GetObjRecsCount : integer;
protected
  procedure SetCellHeight(NewHeight : integer);
  procedure SetCellWidth(NewWidth : integer);
  procedure Add(ObjRec : TprObjRec);

  property ObjRecs[i : integer] : TprObjRec read GetObjRec;
  property ObjRecsCount : integer read GetObjRecsCount;
  property Page : TprCustomEndPage read FPage;
  property Band : TprBand read FBand write FBand;
  property HorzVector : TprGenHorzVector read FHorzVector write FHorzVector;
  property VertVector : TprGenVertVector read FVertVector write FVertVector;
  property Width : integer read FWidth;
  property Height : integer read FHeight;
public
{Creates an instance of the TprGenCell class.}
  constructor Create;
{Frees an instance of the TprGenCell class.}
  destructor Destroy; override;
end;

{Describes the mode of change of the current page number.
Items:
  prcpnNone - The current page number is not changing.
  prcpnOffset - The current page number is changed by specified value.
  prcpnSetTo - The current page number is set to specified value.}
TprChangePageNumberMode = (prcpnNone, prcpnOffset, prcpnSetTo);
/////////////////////////////////////////////////
//
// TprGenBandInfo
//
/////////////////////////////////////////////////
{Represents the generating-time information about band.
Instance of this class is passed into the OnBandGenerateCell event,
in which the developer can change various band properties.
This is a base class for TprGenHorzBandInfo (horizontal bands)
and TprGenVertBandInfo (vertical bands) classes.
See also:
  TprGenHorzBandInfo, TprGenVertBandInfo}
TprGenBandInfo = class(TPersistent)
private
  FVector: TprGenVector;
  FSize: integer;
  FStartNewPage: boolean;
  FBreakPage: boolean;
  FResetPagesCount: Boolean;
  FCanSplit: Boolean;
  FReprintOnEachPage: Boolean;
  FLinkedBand: TprBand;
  FLinkToBand: TprBand;
  FChangePageNumberMode: TprChangePageNumberMode;
  FChangePageNumberValue: Integer;
  FMinSubBand: TprBand;
  FMinSubBandCount: Integer;
  FSubReport: TprCustomReport;
  FSubReportName: string;
  FSubReportUserData: Integer;
  
  function GetBand: TprBand;

  procedure SetSize(Value : integer);
  procedure SetStartNewPage(Value : boolean);
  procedure SetBreakPage(Value : boolean);
  procedure SetResetPagesCount(Value: Boolean);
  procedure SetLinkedBand(Value: TprBand);
  procedure SetLinkToBand(Value: TprBand);
  procedure SetReprintOnEachPage(Value: Boolean);
  procedure SetCanSplit(Value: Boolean);
  procedure SetSubReportName(Value: string);
  procedure SetMinSubBand(Value: TprBand);
  procedure SetMinSubBandCount(Value: Integer);

  property SubReport: TprCustomReport read FSubReport;
public
{Returns the TprBand object which currently is processed.}
  property Band: TprBand read GetBand;
{Specifies the band's size.
For TprReport - in pixels, for TprTxReport - in chars.}
  property Size: integer read FSize write SetSize;
{Specifies the value indicating whether the band starts a new page.}
  property StartNewPage: Boolean read FStartNewPage write SetStartNewPage;
{Specifies the value indicating whether the new page must be started after this band.}
  property BreakPage: Boolean read FBreakPage write SetBreakPage;
{Specifies the value indicating whether the total number of pages must be reset to 1.}
  property ResetPagesCount: Boolean read FResetPagesCount write SetResetPagesCount;
{Specified the mode of change of the current page number.
See also:
  TprChangePageNumberMode, ChangePageNumberValue}
  property ChangePageNumberMode: TprChangePageNumberMode read FChangePageNumberMode write FChangePageNumberMode;
{Specifies the value which will be used for change the current page number.<br>
If ChangePageNumberMode equals to prcpnNone the current page number is not changed and
ChangePageNumberValue is not used.<br>
If ChangePageNumberMode equals to prcpnOffset the current page number is changed to
CurrentPageNumber + ChangePageNumberValue.<br>
If ChangePageNumberMode equals to prcpnSetTo the current page number is changed to ChangePageNumberValue.
See also:
  ChangePageNumberMode}
  property ChangePageNumberValue: Integer read FChangePageNumberValue write FChangePageNumberValue;
{Specifies the value indicating whether a band can be splitted.}
  property CanSplit: Boolean read FCanSplit write SetCanSplit;
{Specifies the name of sub-report (TprCustomReport component) that will be inserted after band.}
  property SubReportName: string read FSubReportName write SetSubReportName;
{Specifies the custom user-defined data which will be passed into the OnBeginSubReportGenerate event.
Example:
  procedure TForm1.CustomersReportBandGenerateCell(Sender: TObject;
    HorzBandInfo: TprGenHorzBandInfo; VertBandInfo: TprGenVertBandInfo);
  begin
    if (HorzBandInfo <> nil) and (HorzBandInfo.Band.Name = 'prHDetailBand1') then
    begin
      // save in the SubReportUserData the key of current record in the dataset of the main report.
      HorzBandInfo.SubReportUserData := customer.FieldByName('CustNo').AsInteger;
    end;
  end;

  procedure TForm1.CustomersReportBeginSubReportGenerate(Sender: TObject;
    ASubReport: TprCustomReport; ASubReportUserData: Integer);
  begin
    // open the parameterized query that is used to build the sub-report.
    orders.Close;
    orders.ParamByName('custno').AsInteger := ASubReportUserData;
    orders.Open;
  end;}
  property SubReportUserData: Integer read FSubReportUserData write FSubReportUserData;

{Specifies the band which is linked to the current band, linked bands will be printed on the one page.
Use this property to link a group header to group data for example.}
  property LinkedBand: TprBand read FLinkedBand write SetLinkedBand;

{Specifies the band to which the current band is linked, linked bands will be printed on the one page.
Use this property to link a group footer to group data for example.}
  property LinkToBand: TprBand read FLinkToBand write SetLinkToband;

{Indicates whether the header band should be reprinted on the top of each page while its data
continues. Set this property to true for group header to repeat it on each page.
Example:
  procedure THWForm.prReport1BandGenerateCell(Sender: TObject;
    HorzBandInfo: TprGenHorzBandInfo; VertBandInfo: TprGenVertBandInfo);
  begin
    if HorzBandInfo <> nil then
    begin
      // check for the group header
      if HorzBandInfo.Band.Name = 'GroupHeader' then
      begin
        HorzBandInfo.ReprintOnEachPage := True;
      end;
    end;
  end;}
  property ReprintOnEachPage: Boolean read FReprintOnEachPage write SetReprintOnEachPage;

  property MinSubBand: TprBand read FMinSubBand write SetMinSubBand;
  property MinSubBandCount: Integer read FMinSubBandCount write SetMinSubBandCount;

{Creates an instance of the TprGenBandInfo class.}
  constructor Create(Vector : TprGenVector);
{Frees an instance of the TprGenBandInfo class.}
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TprGenVector
//
/////////////////////////////////////////////////
{Internal class.}
TprGenVector = class(TObject)
private
  FVisibleFormula: string;
  FBand: TprBand;
  FCells: TList;
  FLeft: Integer;
  FTop: Integer;
  FSize: Integer;
  FDelete: Boolean;
  FSectionIndex: Integer;
  FSectionLevel: Integer;
  FLinkToVector: TprGenVector; // points to another vector, that
                               // is placed before this vector in list.
                               // Another vector should be on some page
                               // with this vector.
  PassCount: Integer;
  PageIndex: Integer;

  function GetCell(I: Integer): TprGenCell;
  function GetCellsCount: Integer;
protected
  function GetGenInfo: TprGenBandInfo; virtual; abstract;

  procedure AddCell(Cell: TprGenCell);

  function GetCellSize(ACell: TprGenCell): Integer; virtual; abstract;
  procedure UpdateObjectsSizes; virtual; abstract;
  procedure RecalculateSizes;
  
  property Band: TprBand read FBand;
  property GenInfo: TprGenBandInfo read GetGenInfo;
  property Left: Integer read FLeft write FLeft;
  property Top: Integer read FTop write FTop;
  property Size: Integer read FSize write FSize;
  property Cells[I: Integer] : TprGenCell read GetCell; default;
  property CellsCount: Integer read GetCellsCount;
  property Delete: Boolean read FDelete write FDelete;
  property SectionLevel: Integer read FSectionLevel write FSectionLevel;
  property SectionIndex: Integer read FSectionIndex write FSectionIndex;
  property LinkToVector: TprGenVector read FLinkToVector write FLinkToVector;
  property VisibleFormula: string read FVisibleFormula write FVisibleFormula; 
public
{Creates an instance of the TprGenVector class.}
  constructor Create(Band: TprBand); virtual;
{Frees an instance of the TprGenVector class.}
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TprGenHorzBandInfo
//
/////////////////////////////////////////////////
{Represents the generating-time information about horizontal band.
See also:
  TprGenBandInfo, TprGenVertBandInfo}
TprGenHorzBandInfo = class(TprGenBandInfo)
private
  FUseColumns: boolean;
  FStartNewColumn: boolean;
  FBreakColumn: boolean;
  FColDirection: TprColDirectionType;
  FColCount: Integer;
  procedure CheckUseVerticalBands;
  procedure SetUseColumns(Value : boolean);
  procedure SetStartNewColumn(Value : boolean);
  procedure SetBreakColumn(Value : boolean);
  procedure SetColDirection(Value : TprColDirectionType);
  procedure SetColCount(Value : integer);
public
{Specifies the value indicating whether the band must use a columns.
See also:
  ColCount}
  property UseColumns: Boolean read FUseColumns write SetUseColumns;
{Specifies the value indicating the creation mode of columns.
See also:
  TprColDirectionType, UseColumns, ColCount}
  property ColDirection: TprColDirectionType read FColDirection write SetColDirection;
{Specifies the amount of columns in which the band must printed.
This property is used when UseColumns = true.}
  property ColCount: Integer read FColCount write SetColCount;
{Specifies the value indicating whether the band starts a new column.}
  property StartNewColumn: Boolean read FStartNewColumn write SetStartNewColumn;
{Specifies the value indicating whether the new column must be started after this band.}
  property BreakColumn: Boolean read FBreakColumn write SetBreakColumn;
end;

/////////////////////////////////////////////////
//
// TprSubReportData
//
/////////////////////////////////////////////////
{Internal class. Represents the sub-report data that will be pasted into main report.}
TprSubReportData = class(TObject, IprReportContainer)
private
  FEndPages: TList;
  FFinishedPositionOnLastPage: Integer;
  FValues: TprValues; // contains the report values
  FSystemValues: TprValues; // contains the report system values
  FParser: TObject;
  
  function GetEndPagesCount: Integer;
  function GetEndPage(I: Integer): TprCustomEndPage;
  function GetEmpty: Boolean;
protected
  function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  function _AddRef: Integer; stdcall;
  function _Release: Integer; stdcall;

  procedure DeleteEndPage(Index: Integer);

  function FormatStrings(lSource, lDest: TStrings; DeleteEmptyLines, DeleteEmptyLinesAtEnd: Boolean): Boolean;
  function FormatTemplate(Template: string; var Res: string): Boolean;
  
  property EndPagesCount: Integer read GetEndPagesCount;
  property EndPage[I: Integer]: TprCustomEndPage read GetEndPage;
  property FinishedPositionOnLastPage: Integer read FFinishedPositionOnLastPage;
  property Empty: Boolean read GetEmpty;
  property Values: TprValues read FValues;
  property SystemValues: TprValues read FSystemValues;
public
{Creates an instance of the TprSubReportData class.
Parameters:
  AReport - The TprCustomReport object, the data of which will be stored in this object.}
  constructor Create(AReport: TprCustomReport; AParentReport: TprCustomReport);
{Frees an instance of the TprSubReportData class.}
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TprGenHorzVector
//
/////////////////////////////////////////////////
{Internal class.}
TprGenHorzVector = class(TprGenVector)
private
  FInfo: TprGenHorzBandInfo;
  FSubReportData: TprSubReportData;
protected
  function GetGenInfo: TprGenBandInfo; override;

  function GetCellSize(ACell: TprGenCell): Integer; override;
  procedure UpdateObjectsSizes; override;

  property Info: TprGenHorzBandInfo read FInfo;
  property SubReportData: TprSubReportData read FSubReportData write FSubReportData;
public
{Creates an instance of the TprGenHorzVector class.}
  constructor Create(Band: TprBand); override;
{Frees an instance of the TprGenHorzVector class.}
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TprGenVertBandInfo
//
/////////////////////////////////////////////////
{Represents the generating-time information about vertical band.
See also:
  TprGenBandInfo, TprGenHorzBandInfo}
TprGenVertBandInfo = class(TprGenBandInfo)
end;

/////////////////////////////////////////////////
//
// TprGenVertVector
//
/////////////////////////////////////////////////
{Internal class.}
TprGenVertVector = class(TprGenVector)
private
  FInfo : TprGenVertBandInfo;
protected
  function GetGenInfo : TprGenBandInfo; override;

  function GetCellSize(ACell: TprGenCell): Integer; override;
  procedure UpdateObjectsSizes; override;

  property Info: TprGenVertBandInfo read FInfo;
public
{Creates an instance of the TprGenVertVector class.}
  constructor Create(Band : TprBand); override;
{Frees an instance of the TprGenVertVector class.}
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TprStoredLink
//
/////////////////////////////////////////////////
{Internal class.}
TprStoredLink = class(TObject)
private
  FVector : TprGenVector;
  FVectorIndex : integer;
  FLinkedBand : TprBand;
protected
  property Vector: TprGenVector read FVector write FVector;
  property VectorIndex: integer read FVectorIndex write FVectorIndex;
  property LinkedBand: TprBand read FLinkedBand write FLinkedBand;
end;

/////////////////////////////////////////////////
//
// TprGenGrid
//
/////////////////////////////////////////////////
{Internal class, represents the table which is formed in the time of report generating.
Intance if this class is created for each page of the report template.}
TprGenGrid = class(TObject)
private
  FPage: TprCustomPage;
  FHorzTitle: TprGenCell;
  FVertTitle: TprGenCell;
  FHorzPageHeader: TprGenCell;
  FVertPageHeader: TprGenCell;
  FHorzPageFooter: TprGenCell;
  FVertPageFooter: TprGenCell;
  FCells: TList;
  FVertVectors: TList;
  FHorzVectors: TList;
  FHorzSectionIndex: Integer;
  FHorzSectionLevel: Integer;
  FVertSectionIndex: Integer;
  FVertSectionLevel: Integer;

  FCurHorzVector: TprGenHorzVector;
  FCurVertVectorIndex: Integer;
  FProgressUpdateCounter: Integer;

  function GetCell(I: integer) : TprGenCell;
  function GetVertVector(I: integer) : TprGenVertVector;
  function GetHorzVector(i : integer) : TprGenHorzVector;
  function GetVertVectorsCount : integer;
  function GetHorzVectorsCount : integer;
  function GetCellsCount : integer;

  function AddCell(HorzBand : TprCustomHBand; VertBand : TprCustomVBand) : TprGenCell;
  function IsEndOfLine : boolean;
  function SkipVerticalBand(VertBand : TprCustomVBand): TprGenVertVector;
  procedure EndLine;

  procedure CopyCellInfoHorz(ASource, ADest: TprGenCell; AHorzVector: TprGenHorzVector; AHorzVectorPos: Integer);
  procedure CopyCellInfoVert(ASource, ADest: TprGenCell; AVertVector: TprGenVertVector; AVertVectorPos: Integer);

  function CreateAndAddHorzVector(ABand: TprCustomHBand): TprGenHorzVector;
  function CreateAndAddVertVector(ABand: TprCustomVBand): TprGenVertVector;

  function SplitHorzVector(AVector: TprGenHorzVector; AVectorPos: Integer; ASplitPos: Integer): TprGenHorzVector;
  function SplitVertVector(AVector: TprGenVertVector; AVectorPos: Integer; ASplitPos: Integer): TprGenVertVector;

  function CopyHorzVectorToPos(Vector: TprGenHorzVector; CopyPos: Integer) : TprGenHorzVector;
  function CopyVertVectorToPos(Vector: TprGenVertVector; CopyPos: Integer) : TprGenVertVector;

  procedure BeginHorzSection;
  procedure EndHorzSection;
  procedure BeginVertSection;
  procedure EndVertSection;

  procedure Clear;
  procedure SecondPass;

  procedure DeleteAndFreeVector(AVector: TprGenVector);

  property Page: TprCustomPage read FPage;

  property HorzTitle: TprGenCell read FHorzTitle;
  property VertTitle: TprGenCell read FVertTitle;
  property HorzPageHeader: TprGenCell read FHorzPageHeader;
  property VertPageHeader: TprGenCell read FVertPageHeader;
  property HorzPageFooter: TprGenCell read FHorzPageFooter;
  property VertPageFooter: TprGenCell read FVertPageFooter;
  property Cells[I: integer] : TprGenCell read GetCell;
  property CellsCount: integer read GetCellsCount;
  property VertVectors[I: integer] : TprGenVertVector read GetVertVector;
  property HorzVectors[I: integer] : TprGenHorzVector read GetHorzVector;
  property VertVectorsCount: integer read GetVertVectorsCount;
  property HorzVectorsCount: integer read GetHorzVectorsCount;
public
{Creates an instance of the TprGenGrid class.}
  constructor Create(Page: TprCustomPage);
{Frees an instance of the TprGenGrid class.}
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TprCustomPage
//
/////////////////////////////////////////////////
{Represents the separate page of the report template.
The TprCustomPage object is a parent component for the bands which is contained in it,
Use the Bands property to access for the bands within page.
Use the Report property to determine the TprCustomReport object containing this page.
See also:
  TprCustomReport, TprBand}
TprCustomPage = class(TComponent)
private
  FReport: TprCustomReport;
  FGrid: TprGenGrid;
  FBands: TprBands; // list of bands on page
  FVisible: boolean;
  FOldEndPagesRects : array of TRect;
  procedure SetReport(Value : TprCustomReport);
  function GetIndexInReport : integer;
  procedure SetIndexInReport(Value : integer);
protected
  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
  procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  procedure SetParentComponent(Value : TComponent); override;
  function  GetChildOwner : TComponent; override;
  procedure ReportSetted; virtual;

  procedure FirstPassGenerateGrid;

  procedure MoveCellObjects(AMainPage: TprCustomPage; EndPage: TprCustomEndPage; Cell: TprGenCell; LeftOffs, TopOffs: Integer);
  procedure CopyPageFootersHeadersObjects(AMainPage: TprCustomPage; EndPage: TprCustomEndPage; Cell: TprGenCell; LeftOffs, TopOffs: integer);
  procedure CheckAllParentReportTitles(var AOffset: Integer);
  procedure GetCrossTabPageVertParams(AMainPage: TprCustomPage; IsMainLeft, IsLeft: Boolean; var Min, Max: Integer);
  procedure GetCrossTabPageHorzParams(AMainPage: TprCustomPage; IsMainTop, IsTop: Boolean; AParentReportTopOffset: Integer; var Min, Max: Integer);
  function GetClientPageRect(AMainPage: TprCustomPage; IsMainTop, IsTop: Boolean; AParentReportTopOffset: Integer): TRect;
  procedure SecondPassAddEndPage(AMainPage: TprCustomPage; AParentPages: TList; AParentReportTopOffset: Integer; IsMainTop, IsTop, IsMainLeft, IsLeft: Boolean; var LeftOffs, TopOffs: Integer);

  procedure SecondPassPlaceGridOnEndPage(AMainPage: TprCustomPage; AParentPage: TprCustomPage; AStartPageIndex, ATopOffset: Integer);
  function DsgnPageRect: TRect; virtual; abstract;
  function GenPageRect: TRect; virtual; abstract;

  property Grid: TprGenGrid read FGrid;
public
  dPageRect: PRect;
{Returns the TprBands object representing the page's bands.
See also:
  TprBands}
  property Bands: TprBands read FBands;
{Returns the TprCustomReport object containg this page.}
  property Report: TprCustomReport read FReport write SetReport;
{Specifies the page's index in the report remplate.}
  property IndexInReport: integer read GetIndexInReport write SetIndexInReport;

{Returns the true value always.}
  function HasParent: Boolean; override;
{Returns the TprCustomReport object containing this page.}
  function GetParentComponent: TComponent; override;

{Returns the true value if a page contains the cross-tab report.}
  function HasCrossTabReport: Boolean;

{Recalculates the coordinates of all objects within the page.
This method is usually used when the report template is created in runtime.}
  procedure UpdateBandsPageRect;

{Creates the band of specified type and adds it in the page.
Parameters:
  BandType - The type of the band.
Example:
procedure TForm1.Button1Click(Sender: TObject);
var
  APage: TprCustomPage;
  ATitle: TprHTitleBand;
  ADetail: TprHDetailBand;
  m: TprMemoObj;

  function CalcMemoHeight(Memo : TStrings; Font : TFont) : integer;
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

begin
  APage := prReport1.Pages[0];

  // add report title
  ATitle := TprHTitleBand(APage.InsertBand(bthTitle));
  ATitle.Height := 50; // 50 pixels
  // add object wih text
  m := TprMemoObj.Create(prReport1.prOwner);
  m.Band := ATitle; // link object to band
  with m.DefVersion do
  begin
    Memo.Add('Report title');
    Font.Size := 16;
    Font.Color := clGray;
    lBorder.Show := false;
    tBorder.Show := false;
    rBorder.Show := false;
    bBorder.Show := false;
    // coordinates of object relative to the top-left corner of the band
    m.dRec.pRect := Rect(4, 4, 100, 4 + CalcMemoHeight(Memo, Font));
  end;

  // add detail
  ADetail := TprHDetailBand(APage.InsertBand(bthDetail));
  ADetail.DatasetName := 'MyDataset';
  ADetail.Height := 30; // 30 pixels
  // add object wih text
  m := TprMemoObj.Create(prReport1.prOwner);
  m.Band := ADetail; // link object to band
  with m.DefVersion do
  begin
    Memo.Add('Data - [MyDataset.Data]');
    Font.Size := 10;
    lBorder.Show := false;
    tBorder.Show := false;
    rBorder.Show := false;
    bBorder.Show := false;
    // coordinates of object relative to the top-left corner of the band
    m.dRec.pRect := Rect(4, 4, 100, 4 + CalcMemoHeight(Memo, Font));
  end;

  // recalculate coordinates before show in designer!!!
  APage.UpdateBandsPageRect;

  prReport1.DesignReport(True);
end;}
  function InsertBand(BandType: TprBandType): TprBand;
{Removes the band from page and frees it.
Parameters:
  Band - The band to remove.}
  procedure DeleteBand(Band: TprBand);

{Creates an instance of the TprCustomPage class.
Parameters:
  AOwner - The component owner.
Example:
  // this code adds new page in the template of TprReport
  with TprPage.Create(prReport1.prOwner) do
  begin
    Name := 'MainData';
    Report := prReport1;
  end;}
  constructor Create(AOwner: TComponent); override;
{Frees an instance of the TprCustomPage class.}
  destructor Destroy; override;
published
{Specifies the value indicating whether the template page must be used in the report generating.}
  property Visible: Boolean read FVisible write FVisible default true;
end;

{TOnFoundPrintVariable is the type of event handlers that respond when
a print variable is found while report printing.
Each print variable has the following format - @@VariableName@@.
If the report engine has found string of above format while it prints a text,
the event of the TOnFoundPrintVariable type occurs.
The PReport has the following built-in print variables:<br>
  <b>COPY</b> - Prints the number of the current print copy.<br>
  <b>COPIES</b> - Prints the total number of copies.<br>
  <b>PRINTER</b> - Prints the name of the used printer.<br>  
Parameters:
  Sender - The TprCustomReport object firing the event.
  AVariableName - The name of the found print variable.
  AVariableValue - The value of the found print variable.
See also:
  TprCustomReport.OnFondPrintVariable}
TOnFoundPrintVariable = procedure (Sender: TObject; const AVariableName: string; var AVariableValue: string) of object;

{Internal structure.}
rDataSetRecNo = record
  DataSet : TprDatasetLink;
  RecNo : integer;
end;
(*$NODEFINE rDataSetRecNo*)
{Represents an array of rDataSetRecNo structures.}
TDataSetRecNoArray = array of rDataSetRecNo;
(*$NODEFINE TDataSetRecNoArray*)
(*$HPPEMIT 'struct rDataSetRecNo'*)
(*$HPPEMIT '{'*)
(*$HPPEMIT '	Pr_dataset::TprDatasetLink* DataSet;'*)
(*$HPPEMIT '	int RecNo;'*)
(*$HPPEMIT '} ;'*)
(*$HPPEMIT 'typedef DynamicArray<rDataSetRecNo >  TDataSetRecNoArray;'*)
(*$HPPEMIT '}'*)

{TOnBandGenerateCell is the type for TprCustomReport.OnBandGenerate event.
This event occurs on the stage of generating of report each time when a band is being processed by the report engine.
You can:<br>
  - change the band's size<br>
  - start the new page before or after the processed band<br>
  - and so on.
Parameters:
  Sender - The TprCustomReport object
  HorzBandInfo - Specifies the parameters for the horizontal band.
  VertBandInfo - Specifies the parameters for the vertical band (may be null, if non cross-tab report is being processed).
Example:
  // this code start new page for the horizontal group header
  procedure THWForm.prReport1BandGenerateCell(Sender: TObject;
    HorzBandInfo: TprGenHorzBandInfo; VertBandInfo: TprGenVertBandInfo);
  begin
    if HorzBandInfo <> nil then
    begin
      // check for the group header
      if HorzBandInfo.Band.Name = 'GroupHeader' then
      begin
        HorzBandInfo.StartNewPage := true;
      end;
    end;
  end;
See also:
  TprCustomReport.OnBandGenerateCell, TprGenHorzBandInfo, TprGenVertBandInfo}
TOnBandGenerateCell = procedure (Sender: TObject;
                                 HorzBandInfo: TprGenHorzBandInfo;
                                 VertBandInfo: TprGenVertBandInfo) of object;
{TOnFirstPassObject is the type for TprCustomReport.OnFirstPassObject event.
This event occurs for each object of the report template and can be used
for changing the objects' properties.
Parameters:
  Sender - The TprCustomReport object
  Obj - The report object which is being processed.
  ManuallyProcessed - Set this parameter to true to disable default object processing.
Example:
  // this code loads the image into TprImageObj object from the file, that name
  // is stored in the database field.
  procedure TForm1.prReport1FirstPassObject(Sender: TObject; Obj: TprObj;
    var ManuallyProcessed: Boolean);
  begin
    if Obj.Name = 'prImageObj1' then
    begin
      ManuallyProcessed := True;
      try
        TprImageObj(Obj).GenCurVersion.Picture.LoadFromFile(Table.FieldByName('FILENAME').AsString);
      except
      end;
    end;
  end;
See also:
  TprCustomReport.OnFirstPassObject}
TOnFirstPassObject = procedure (Sender: TObject; Obj: TprObj; var ManuallyProcessed: Boolean) of object;
{TOnUnknownVariable is the type for TprCustomReport.OnUnknownVariable event.
This event occurs when the built-in parser has found an unknown identifier
on the stage of the report's generating.
Parameters:
  Sender - The TprCustomReport object
  VarName - The name of the unknown identifier
  Value - Use this parameter to set the parameter's value.
Use the methods: _vSetAsString, _vSetAsDateTime, _vSetAsDouble
_vSetAsInteger, _vSetAsBoolean, _vSetAsVariant to write value into this parameter.
  IsProcessed - Set this parameter to true if the identifier is processed successfully.
  
Example:
  procedure TForm1.prReport1UnknownVariable(Sender: TObject;
    const VarName: String; var Value: TprVarValue; var IsProcessed: Boolean);
  var
    s : string;
  begin
    if AnsiCompareText(VarName,'FindValid')=0 then
    begin
      // prepare find valid description
      if CheckBox1.Checked then
        s := 'All records'
      else
        s := 'Company must contains "'+Edit1.Text+'"';
      _vSetAsString(Value,s);
      IsProcessed := true;
    end
    else
    if AnsiCompareText(VarName,'Order')=0 then
      begin
        // prepare order description
        _vSetAsString(Value,ComboBox1.Items[ComboBox1.ItemIndex]);
        IsProcessed := true;
      end
  end;
See also:
  TprCustomReport.OnUnknownVariable}
TOnUnknownVariable = procedure (Sender: TObject; const VarName: string;
                                                 var Value: TprVarValue;
                                                 var IsProcessed: boolean) of object;
{$IFDEF PR_CB}
TOnUnknownFunction = procedure (Sender : TObject; const FuncName   : string;
                                                  var Parameters : TprVarsArray;
                                                  ParametersCount  : integer;
                                                  var Value        : TprVarValue;
                                                  var IsProcessed  : boolean) of object;
TOnUnknownObjFunction = procedure (Sender : TObject; Component        : TComponent;
                                                     const FuncName   : string;
                                                     var Parameters : TprVarsArray;
                                                     ParametersCount  : integer;
                                                     var Value        : TprVarValue;
                                                     var IsProcessed  : boolean) of object;
{$ELSE}
{TOnUnknownFunction is the type for TprCustomReport.OnUnknownFunction event.
This event occurs when the built-in parser has found an unknown function
on the stage of the report's generating.
Parameters:
  Sender - The TprCustomReport object
  FuncName - The name of the unknown function.
  Parameters - The function's parameters.
  ParametersCount - The amount of the function's parameters.
  Value - Use this parameter to set the function's value.
Use the methods: _vSetAsString, _vSetAsDateTime, _vSetAsDouble
_vSetAsInteger, _vSetAsBoolean, _vSetAsVariant to write value into this parameter.
  IsProcessed - Set this parameter to true if the function is processed successfully.
See also:
  TprCustomReport.OnUnknownFunction}
TOnUnknownFunction = procedure (Sender : TObject; const FuncName: string;
                                                  const Parameters: TprVarsArray;
                                                  ParametersCount: integer;
                                                  var Value: TprVarValue;
                                                  var IsProcessed: boolean) of object;
{TOnUnknownObjFunction is the type for TprCustomReport.OnUnknownObjFunction event.
This event occurs when the built-in parser has found an unknown function of object
on the stage of the report's generating.
Parameters:
  Sender - The TprCustomReport object
  Component - The component for which unknown function is found.
  FuncName - The name of the unknown function.
  Parameters - The function's parameters.
  ParametersCount - The amount of the function's parameters.
  Value - Use this parameter to set the function's value.
Use the methods: _vSetAsString, _vSetAsDateTime, _vSetAsDouble
_vSetAsInteger, _vSetAsBoolean, _vSetAsVariant to write value into this parameter.
  IsProcessed - Set this parameter to true if the function is processed successfully.

Example:
  // this code calculates the length of the string fields.
  // The report template contains the TprMemoObj with string:
  //   [Query1.FieldLen("NAME")]
  procedure TForm1.prReport1UnknownObjFunction(Sender: TObject;
    Component: TComponent; const FuncName: String;
    const Parameters: TprVarsArray; ParametersCount: Integer;
    var Value: TprVarValue; var IsProcessed: Boolean);
  var
    f : TField;
  begin
    if (Component = Query1) and
       (AnsiCompareText(FuncName, 'Query1.FieldLen') = 0) and
       (ParametersCount = 1) then
    begin
      // Parameter with index 0 is the fieldname
      f := Query1.FindField(_vAsString(Parameters[0]));
      if f<>nil then
        begin
          // field is found return length of field value
          _vSetAsInteger(Value,Length(f.AsString));
          IsProcessed := true;
        end
    end
  end;

See also:
  TprCustomReport.OnUnknownObjFunction}
TOnUnknownObjFunction = procedure (Sender : TObject; Component: TComponent;
                                                     const FuncName: string;
                                                     const Parameters: TprVarsArray;
                                                     ParametersCount: integer;
                                                     var Value: TprVarValue;
                                                     var IsProcessed: Boolean) of object;
{$ENDIF}
{TOnUnknownObjProp is the type for TprCustomReport.OnUnknownObjProp event.
This event occurs when the built-in parser has found an unknown property of object
on the stage of the report's generating.
Parameters:
  Sender - The TprCustomReport object
  Component - The component for which unknown property is found.
  PropName - The name of the unknown function.
  IdentName - The full ident name, "MyDataset.MyProperty" for example.
  Value - Use this parameter to set the value of property.
Use the methods: _vSetAsString, _vSetAsDateTime, _vSetAsDouble
_vSetAsInteger, _vSetAsBoolean, _vSetAsVariant to write value into this parameter.
  IsProcessed - Set this parameter to true if the function is processed successfully.

See also:
  TprCustomReport.OnUnknownObjProp}
TOnUnknownObjProp = procedure (Sender : TObject; Component: TComponent;
                                                 const PropName: string;
                                                 const IdentName: string;
                                                 var Value: TprVarValue;
                                                 var IsProcessed: Boolean) of object;

{TprOnDesignerOpenTemplate is the type for TprCustomReport.OnDesignerOpenTemplate event.
This event occurs when user opens the report template in the designer.
Please see demo: Demo\Demos\23_CustomSaveOpenInDesigner.
Parameters:
  Sender - The TprCustomReport object.
  Stream - If you return the TStream object in this parameter then the report template will be loaded from this stream.
  FileName - If you return the file name in this parameter then the report template
will be loaded from this file (the open dialog not appears.)
  CancelOpen - Return the true value in this parameter to disable the default loading,
in this case you must load the report template manually.
  IsBinaryFormat - Indicates whether the report template is in binary mode.
See also:
  TprCustomReport.OnDesignerOpenTemplate}
TprOnDesignerOpenTemplate = procedure (Sender: TObject; var FileName: string; var Stream: TStream; var CancelOpen: boolean; var IsBinaryFormat: Boolean) of object;
{TprOnDesignerSaveTemplate is the type for TprCustomReport.OnDesignerSaveTemplate event.
This event occurs when user saves the report template in the designer.
Please see demo: Demo\Demos\23_CustomSaveOpenInDesigner.
Parameters:
  Sender - The TprCustomReport object.
  Stream - If you return the TStream object in this parameter then the report template will be saved in this stream.
  FileName - If you return the file name in this parameter then the report template
will be saved in this file (the save dialog not appears.)
  CancelSave - Return the true value in this parameter to disable the default saving,
in this case you must save the report template manually.
  IsBinaryFormat - Indicates whether the report template must be saved in binary mode.
  IsSaveAs - Indicates whether the user do the "Save as..." action.
See also:
  TprCustomReport.OnDesignerSaveTemplate}
TprOnDesignerSaveTemplate = procedure (Sender: TObject; var FileName: string; var Stream: TStream; var CancelSave: boolean; var IsBinaryFormat: boolean; IsSaveAs: boolean) of object;
{TOnGetAvailableComponents is the type for TprCustomReport.OnGetAvailableComponents event.
This event occurs when the report engine builds the list of components which
can be used in the report template.
By default this list contains:
<ul>
<li>The TprCustomReport components</li>
<li>All components of the report template:</li>
<ul>
<li>template pages</li>
<li>bands</li>
<li>objects</li>
<li>groups</li>
</ul>
<li>All child components of the report owner.</li>
<li>All application datamodules with their sub components.</li>
<li>All application forms with their sub components.</li>
</ul>

Parameters:
  Sender - The TprCustomReport object.
  L - The list of components must be returned in this parameter,
each item of the list describes one component,
use the L.AddObject method to add component, for example:<br>
L.AddObject('MyComponent', MyComponent);

See also:
  TonGetAvailableDatasets, TprCustomReport.OnGetAvailableComponents, TprCustomReport.OnGetAvailableDatasets}
TOnGetAvailableComponents = procedure (Sender: TObject; L: TStrings) of object;
{TOnGetAvailableDatasets is the type for TprCustomReport.OnGetAvailableDatasets event.
This event occurs when the report engine builds the list of datasets which
can be used in the report template. By default the report engine builds the list of available components
and then Leaves in this list only the descendants of TDataset and TprDataset .

Parameters:
  Sender - The TprCustomReport object.
  L - The list of datasets must be returned in this parameter, each item of the list describes one dataset,
use the L.AddObject method to add dataset, for example:<br>
L.AddObject('MyComponent', MyComponent);<br>
L.AddObject('MyDataModule.MyComponent', MyDataModule.MyComponent);

See also:
  TOnGetAvailableComponents, TprCustomReport.OnGetAvailableComponents, TprCustomReport.OnGetAvailableDatasets}
TOnGetAvailableDatasets = procedure (Sender: TObject; L: TStrings) of object;
{TOnCloseSetupDialogEvent is the type for TprCustomReport.OnCloseSetupDialogEvent event.
This event occurs when user closes the "Print setup" dialog.
Parameters:
  Sender - The TprCustomReport object.
  fOK - Indicates whether the user have pressed OK button in the dialog.
See also:
  TprCustomReport.OnCloseSetupDialogEvent}
TOnCloseSetupDialogEvent = procedure (Sender: TObject; fOK: boolean) of object;
{TOnCloseSetupDialogEvent is the type for TprCustomReport.OnCloseSetupDialogEvent event.
This event occurs when the report engine initializes the dataset for the detail band.
Parameters:
  Sender - The TprCustomReport object.
  DetailBand - The detail band, descendant of the TprCustomHDetailBand or TprCustomVDetailBand classes.
  Dataset - The dataset of the detail band, descendant of the TDataset or TprDataset classes.
  DatasetName - The name of the dataset.
See also:
  TprCustomReport.OnCloseSetupDialogEvent, TprDataset}
TOnInitDetailBandDataSet = procedure (Sender: TObject; DetailBand: TprBand; DataSet: TObject; const DataSetName: string) of object;
{TOnTemplatePageGenerate is the type for TprCustomReport.OnTemplatePageGenerate event.
This event occurs before starting of processing the page of the report template.
Parameters:
  Sender - The TprCustomReport object.
  Page - The TprCustomPage object representing the page of the report template.
See also:
  TprCustomReport.OnTemplatePageGenerate}
TOnTemplatePageGenerate = procedure (Sender: TObject; Page: TprCustomPage) of object;

{Describes the set of pages to printing.
Items:
  ppmAll - All pages of the report.
  ppmPagesRange - The specified pages' range.
  ppmSelection - The current selection (not used).
  ppmPagesList - The specified pages' list in the form: "1, 3, 6-9"}
TprPrintPagesMode = (ppmAll, ppmPagesRange, ppmSelection, ppmPagesList);
{Decribes the work mode of built-in forms - preview and designer.
Items:
  fmNormal - The normal mode, the form is neither an MDI parent window nor an MDI child window.
  fmMDIChild - The form is an MDI child window.}
TprFormMode = (fmNormal, fmMDIChild);

{Internal type.}
TOnTemplateChangedGlobalProc = procedure(Report : TprCustomReport);

{TOnPreviewMouseMove is the type for TprCustomReport.OnPreviewMouseMove event.
This event occurs when mouse move over client area of the preview window.
Parameters:
  Sender - The TprCustomReport object firing the event.
  PreviewUserData - User data that was transferred in OnPreviewGetUserData event,
can be null if mouse not within object.
  cur - You can return the type of the mouse cursor in this parameter.
  HighlightObject - If you return true in this parameter the object will be highlighted.
See also:
  TprCustomReport.OnPreviewMouseMove, TprPreviewUserData}
TOnPreviewMouseMove = procedure (Sender: TObject; PreviewUserData: TprPreviewUserData; var cur: TCursor; var HighlightObject: Boolean) of object;
{TOnPreviewMouseDown is the type for TprCustomReport.OnPreviewMouseDown event.
This event occurs when user click mouse button within client area of the preview window.
Parameters:
  Sender - The TprCustomReport object firing the event.
  PreviewUserData - User data that was transferred in OnPreviewGetUserData event,
can be null if mouse not within object.
  Shift - Indicates the state of the Alt, Ctrl, and Shift keys and the mouse buttons.
See also:
  TprCustomReport.OnPreviewMouseDown, TprPreviewUserData}
TOnPreviewMouseDown = procedure (Sender: TObject; PreviewUserData: TprPreviewUserData; Shift: TShiftState) of object;
{TOnPreviewDblClick is the type for TprCustomReport.OnPreviewDblClick event.
Occurs when the user double-clicks the left mouse button within client area of preview window.
Parameters:
  Sender - The TprCustomReport object firing the event.
  PreviewUserData - User data that was transferred in OnPreviewGetUserData event,
can be null if mouse not within object.
See also:
  TprCustomReport.OnPreviewDblClick, TprPreviewUserData}
TOnPreviewDblClick = procedure (Sender: TObject; PreviewUserData: TprPreviewUserData) of object;
{TOnPreviewGetUserData is the type for TprCustomReport.OnPreviewGetUserData event.
Event occurs during the report generating, after processing each object.
With use of this event the developer can assign the own data for each object of the generated report.
Parameters:
  Sender - The TprCustomReport object firing the event.
  Obj - The object of the report, which is processed.
  ObjRec - The parameters record of object.
  PreviewUserData - In this parameter you must return own data. Data is represented
by the TprPreviewUserData class, you can inherit new child class from TprPreviewUserData.
At a creating of a new class you should take into account the following:
<ul>
<li>Only instances of classes inherited from TprPreviewUserData can be used in OnPreviewGetUserData event.</li>
<li>You should not destroy these objects, they will be automatically destroyed.</li>
<li>The derived class should be registered through Classes.RegisterClass function.</li>
</ul>
See also:
  TprCustomReport.OnPreviewGetUserData, TprPreviewUserData}
TOnPreviewGetUserData = procedure (Sender: TObject; Obj: TprObj; ObjRec: TprObjRec; var PreviewUserData: TprPreviewUserData) of object;

{TprOnBeginSubReportGenerate is the type for TprCustomReport.OnBeginSubReportGenerate event.
Event occurs before starting of sub-report building and can be used for preparing the data
which are used in sub-report.
Parameters:
  Sender - The TprCustomReport object firing the event (main report).
  ASubReport - The TprCustomReport object that begins.
  ASubReportUserData - The custom, used-defined data that were returned in the OnBandGenerateCell event.
Example:
  procedure TForm1.CustomersReportBandGenerateCell(Sender: TObject;
    HorzBandInfo: TprGenHorzBandInfo; VertBandInfo: TprGenVertBandInfo);
  begin
    if (HorzBandInfo <> nil) and (HorzBandInfo.Band.Name = 'prHDetailBand1') then
    begin
      // save in the SubReportUserData the key of current record in the dataset of the main report.
      HorzBandInfo.SubReportUserData := customer.FieldByName('CustNo').AsInteger;
    end;
  end;

  procedure TForm1.CustomersReportBeginSubReportGenerate(Sender: TObject;
    ASubReport: TprCustomReport; ASubReportUserData: Integer);
  begin
    // open the parameterized query that is used to build the sub-report.
    orders.Close;
    orders.ParamByName('custno').AsInteger := ASubReportUserData;
    orders.Open;
  end;
See also:
  TprCustomReport.OnBeginSubReportGenerate, OnBandGenerateCell
}
TprOnBeginSubReportGenerate = procedure (Sender: TObject; ASubReport: TprCustomReport; ASubReportUserData: Integer) of object;
{Describes an options of export.
Items:
  preoShowParamsDlg - Indicates whether the built-in options dialog form must be shown before export.
  preoShowProgress - Indicates whether the progress window must be shown while exporting.
  preoShowAfterGenerate - Indicates whether the created document must be shown after export.
  preoShowWhileGenerate - is not used.}
TprExportOptions = (preoShowParamsDlg, preoShowProgress, preoShowAfterGenerate, preoShowWhileGenerate);
{Describes a set of export options.
See also:
  TprExportOptions}
TprExportOptionsSet = set of TprExportOptions;
/////////////////////////////////////////////////
//
// TprCustomReport
//
/////////////////////////////////////////////////
{Represents a base class for reports components - TprReport and TprTxReport.
TprCustomReport contains realization of the report engine which is identical
for TprReport (realizes the windows reports) and TprTxReport (realizes the DOS reports).
See also:
  TprReport, TprTxReport}
TprCustomReport = class(TComponent, IprReportContainer)
private
  FExportFilter: string;
  FExportFileName: string;
  FExportOptions: TprExportOptionsSet;
  FExportPagesMode: TprPrintPagesMode;
  FExportFromPage: Integer;
  FExportToPage: Integer;
  FExportPages: string;

  FTitle: string;

  FShowProgress: Boolean;
  FCanUserEdit: Boolean;

  FDesignerFormMode: TprFormMode;
  FPreviewFormMode: TprFormMode;

  FValues: TprValues;
  FVariables: TprVariables;

  FCustomActionInPreviewCaption: string;

  FOnTemplatePageGenerate: TOnTemplatePageGenerate;
  FOnBandGenerateCell: TOnBandGenerateCell;
  FOnFirstPassObject: TOnFirstPassObject;
  FOnInitDetailBandDataSet: TOnInitDetailBandDataSet;
  FOnPrintComplete: TNotifyEvent;
  FOnPrintStart: TNotifyEvent;

  FOnUnknownVariable: TOnUnknownVariable;
  FOnUnknownFunction: TOnUnknownFunction;
  FOnUnknownObjFunction: TOnUnknownObjFunction;
  FOnUnknownObjProp: TOnUnknownObjProp;
  FOnGetAvailableComponents: TOnGetAvailableComponents;
  FOnGetAvailableDatasets: TOnGetAvailableDatasets;

  FOnDesignerSaveTemplate: TprOnDesignerSaveTemplate;
  FOnDesignerOpenTemplate: TprOnDesignerOpenTemplate;
  FOnCreateDesigner: TNotifyEvent;
  FOnDestroyDesigner: TNotifyEvent;
  FOnCreatePreview: TNotifyEvent;
  FOnDestroyPreview: TNotifyEvent;
  FOnCloseSetupDialog: TOnCloseSetupDialogEvent;
  FOnCustomActionInPreview: TNotifyEvent;

  FOnPreviewGetUserData: TOnPreviewGetUserData;

  FOnPreviewMouseMove: TOnPreviewMouseMove;
  FOnPreviewMouseDown: TOnPreviewMouseDown;
  FOnPreviewDblClick: TOnPreviewDblClick;

  FOnFoundPrintVariable: TOnFoundPrintVariable;
  FOnBeginSubReportGenerate: TprOnBeginSubReportGenerate;

  FGACList : TStringList;
  FPreviewUserDataList : TList;
  FEndPages : TList;
  FPages : TList;
  FParser : TObject;
  ADSRN : TDataSetRecNoArray;

  FDesignerForm : TprDesigner;
  FPreviewForm : TprPreview;

  FReportPrepared : boolean;
  FTemplateChanged : boolean;
  FStartDateTime : TDateTime;
  FGroups : TprGroups;
  FIndexCurEndPage : integer;
  FSystemValues : TprValues;
  FPrintPagesList : TList;
  FDesignerNotifyLinks: TList;
  FPreviewNotifyLinks: TList;

  FFinishedPositionOnLastPage: Integer;
  FSubReports: TList;
  FParentPage: TprCustomPage;

  function GetPage(index : integer) : TprCustomPage;
  function GetEndPage(index : integer) : TprCustomEndPage;
  procedure SetPrintPages(Value : string);

  function GetAllValuesCount : integer;
  function GetAllValue(i : integer) : TprValue;

  procedure OnCalcPagesCount(ValueVersion: TprValueVersion);
  procedure OnCalcPagesCount2(Value: TprValue);
  procedure OnCalcPageNo(ValueVersion : TprValueVersion);
  procedure OnCalcPageNo2(Value : TprValue);

  function  GetprOwner : TComponent;

  procedure ClearPreviewUserDataList;
  procedure ClearGrids;
  function GetDesignerNotifyLinksCount: Integer;
  function GetPreviewNotifyLinksCount: Integer;

  function GetObjectCount: Integer;
  function GetReportObject(Index: Integer): TprObj;

  function GetPagesCount: Integer;
  function GetEndPagesCount: Integer;
  function GetCurEndPage: TprCustomEndPage;

  function GetSubReportsCount: Integer;
  function GetSubReport(I: Integer): TprSubReportData;

protected
  FCollate: Boolean;
  FCopies: integer;
  FFromPage: integer;
  FToPage: integer;
  FPrintPages: string;
  FPrintPagesMode: TprPrintPagesMode;
  FActionCanceled: boolean;
  FProgressForm: TprProgressForm;
  FWindowList: pointer;
  FPrintingCopy: Integer;
  FPrintingPage: Integer;

  Designer : IUnknown; // used to notify Delphi IDE


  function GetPrinterName : string; virtual; abstract;
  procedure SetPrinterName(Value : string); virtual; abstract;

  procedure Loaded; override;
  procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  procedure Notification(AComponent : TComponent; Operation : TOperation); override;
  function GetChildOwner : TComponent; override;
  procedure ReadSystemInfo(Reader : TReader);
  procedure WriteSystemInfo(Writer : TWriter);
  procedure DefineProperties(Filer : TFiler); override;

  function GetDesignerFormClass : string; virtual; abstract;
  function GetPreviewFormClass : string; virtual; abstract;
  function CheckEndPagesCountOnPreview : boolean; virtual;

  procedure SetTemplateChanged(Value : boolean); virtual;

  function GetPreparedReportEmpty : boolean; virtual; abstract;

  function CalcBooleanFormula(const AFormula: string): Boolean; overload;
  function CalcBooleanFormula(var AFormula: string; var ASecondPassNeeded: Boolean): Boolean; overload;

  procedure DoOnBandGenerateCell(HorzBandInfo : TprGenHorzBandInfo; VertBandInfo : TprGenVertBandInfo);
  procedure DoOnDataSetNext(DataSet : TprDatasetLink; HorizontalBand : TprCustomHBand; VerticalBand : TprCustomVBand; Cell : TprGenCell);
  procedure DoOnDataSetEof(Band : TprBand; DataSet : TprDatasetLink);
  procedure DoOnGroupEnd(Group : TprGroup);
  procedure DoOnFirstPassObject(Sender : TprObj; var ManuallyProcessed : boolean);
  procedure DoOnPreviewMouseMove(PreviewUserData : TprPreviewUserData; var cur : TCursor; var HighlightObject : boolean);
  procedure DoOnPreviewMouseDown(PreviewUserData : TprPreviewUserData; Shift : TShiftState);
  procedure DoOnPreviewDblClick(PreviewUserData : TprPreviewUserData);
  procedure DoOnPreviewGetUserData(Obj : TprObj; ObjRec : TprObjRec; var PreviewUserData : TprPreviewUserData);
  procedure DoOnTemplatePageGenerate(Page : TprCustomPage);
  procedure DoOnFoundPrintVariable(const AVariableName: string; var AVariableValue: string);
  procedure DoOnBeginSubReportGenerate(ASubReport: TprCustomReport; ASubReportUserData: Integer);
  procedure DoOnPrintComplete;
  procedure DoOnPrintStart;

  function ADSRN_GetIndex(DataSet : TprDatasetLink) : integer; overload;
  function ADSRN_GetIndex(DataSet : TObject) : integer; overload;
  procedure ADSRN_First(DataSet : TprDatasetLink);
  procedure ADSRN_Next(DataSet : TprDatasetLink);

  procedure DsgnNotify(Source: TObject);

  procedure AddPreviewUserData(PreviewUserData : TprPreviewUserData);
  procedure ClearValuesVersions;
  function CreatePage : TprCustomPage; virtual; abstract;

  function GetPrintVariableValue(const AVariableName: string): string;
  procedure ParsePrintingString(var S: string);

  property Parser: TObject read FParser;

  function CreateEndPage(Page: TprCustomPage) : TprCustomEndPage; virtual; abstract;
  function CreateEmptyEndPage: TprCustomEndPage; virtual; abstract;
  function AddEndPage(Page: TprCustomPage) : integer;
  procedure AddEndPageToList(EndPage: TprCustomEndPage);
  procedure ClearEndPages;

  function InternalPrepareReport(AMainPage: TprCustomPage; AParentPage: TprCustomPage; AStartPageIndex, ATopOffset: Integer): Boolean;

  procedure AddSubReportData(ASubReportData: TprSubReportData);
  procedure FreeSubReportsData;

{$IFDEF DEBUG_DUMPAGGVALUES}
  procedure DumpValues;
{$ENDIF}

  property FinishedPositionOnLastPage: Integer read FFinishedPositionOnLastPage;
  property SubReportsCount: Integer read GetSubReportsCount;
  property SubReports[I: Integer]: TprSubReportData read GetSubReport;
  property ParentPage: TprCustomPage read FParentPage write FParentPage;
public
{Returns the index of the currently generated page in the EndPages property,
this property can be used only on the stage of generating of the report.
See also:
  EndPages, CurEndPage}
  property IndexCurEndPage: Integer read FIndexCurEndPage;
{Returns the currently generated page in the EndPages property,
this property can be used only on the stage of generating of the report.
See also:
  EndPages, IndexCurEndPage}
  property CurEndPage: TprCustomEndPage read GetCurEndPage;

{Returns the component which owns all components of the report template.}
  property prOwner: TComponent read GetprOwner;

{Returns the form of the built-in designer which currently is being used for editing
the report template. This property contains the not nil value only when the designer is created
with using of the DesignReport method.
See also:
  DesignReport, PreviewForm, OnCreateDesigner}
  property DesignerForm: TprDesigner read FDesignerForm;
{Returns the form of the built-in previewer which currently is being used for previewing
the created report. This property contains the not nil value only when the preview window is created
with using of the PreviewReport method.
See also:
  PreviewReport, DesignForm}
  property PreviewForm: TprPreview read FPreviewForm;
{Returns the progress form.}
  property ProgressForm: TprProgressForm read FProgressForm;

{Indicates whether the last process is cancelled by the user.
Example:
  procedure TForm1.MakeReport(AReport: TprCustomReport);
  begin
    if AReport.PrepareReport then
      AReport.PreviewPreparedReport(True)
    else
      if AReport.ActionCancelled then
        ShowMessage('The report''s generating is cancelled by the user');
  end;}
  property ActionCanceled: boolean read FActionCanceled;
{Specifies the value indicating is the report template changed or not.}
  property TemplateChanged: boolean read FTemplateChanged write SetTemplateChanged;
{Indicates whether the report is prepared  and can be previewed or printed or not.}
  property ReportPrepared: boolean read FReportPrepared;
{Returns the true value if the report is empty and has no generated data.}
  property PreparedReportEmpty: boolean read GetPreparedReportEmpty;
{Returns the TDateTime value indicating when the generating of the report is started.}
  property StartDateTime: TDateTime read FStartDateTime;
{Returns the TList object containing the pages' numbers for print.
This list is being automatically generated each time when the new value
is assigned to the PrintPages property.
Do not change it manually.
See also:
  PrintPages}
  property PrintPagesList: TList read FPrintPagesList;

{Lists the pages of the report template.
See also:
  TprCustomPage}
  property Pages[index: integer]: TprCustomPage read GetPage;
{Lists the generated pages of the report.
See also:
  TprCustomEndPage}
  property EndPages[index: integer]: TprCustomEndPage read GetEndPage;

{Returns the amount of pages in the Pages property.
See also:
  Pages}
  property PagesCount: Integer read GetPagesCount;
{Returns the amount of pages in the EndPages property.
See also:
  EndPages}
  property EndPagesCount: Integer read GetEndPagesCount;
  
{Lists all aggrigates variables in the report template.
This property lists the system-defined and user-defined aggregate variables.
See also:
  TprValue, TprValues, AllValuesCount}
  property AllValues[I: Integer]: TprValue read GetAllValue;
{Returns the amount of items in the AllValues property.
See also:
  AllValues}
  property AllValuesCount: Integer read GetAllValuesCount;
{Returns the system-defined aggregate variables.
See also:
  TprValue, TprValues}
  property SystemValues: TprValues read FSystemValues;

{Returns the TprGroups object representing all groups in the report template.
See also:
  TprGroup, TprGroups}
  property Groups: TprGroups read FGroups;

{Lists all objects on all pages in the report template.
See also:
  ObjectCount}
  property Objects[Index: Integer]: TprObj read GetReportObject;
{Returns the amount of objects in the Objects property.
See also:
  Objects}
  property ObjectCount: Integer read GetObjectCount;

{Returns the number of the currently printed copy.
See also:
  PrintingPage, OnFoundPrintVariable}
  property PrintingCopy: Integer read FPrintingCopy;
{Returns the number of the currently printed page.
See also:
  PrintingCopy, OnFoundPrintVariable}
  property PrintingPage: Integer read FPrintingPage;

{Returns the amount of linked objects which is notified when the report template changing.}
  property DesignerNotifyLinksCount: Integer read GetDesignerNotifyLinksCount;
{Returns the amount of linked objects which is notified when the generated report changing.}
  property PreviewNotifyLinksCount: Integer read GetPreviewNotifyLinksCount;

{Inserts the TprCustomEndPage object in the report.
Parameters:
  EndPage - The TprCustomEndPage object to insert.
  I - Specified the inserting position.}
  procedure InsertEndPageIntoList(EndPage: TprCustomEndPage; I: Integer);
{Deletes and frees the generated page from the report.
Parameters:
  Index - The index of the generated page.}
  procedure DeleteEndPage(Index: Integer);
{Returns the index of the generated page in the report (index in the EndPages property).
Parameters:
  EndPage - The TprCustomEndPage object.
Return value:
  Returns the page index.}
  function EndPageIndex(EndPage: TprCustomEndPage): Integer;

{Returns the band class for specified band type.
For TprReport:
<ul>
<li>bthTitle - TprHTitleBand</li>
<li>bthDetail - TprHDetailBand</li>
<li>btvTitle - TprVTitleBand</li>
<li>and so on</li>
</ul>
For TprTxReport:
<ul>
<li>bthTitle - TprTxHTitleBand</li>
<li>bthDetail - TprTxHDetailBand</li>
<li>btvTitle - TprTxVTitleBand</li>
<li>and so on</li>
</ul>
Parameters:
  BandType - The band's type.
See also:
  TprBandType}
  function GetBandClass(BandType: TprBandType): TprBandClass; virtual;

{Starts the generating of the report.
Return value:
  Returns the true value if report has been generated successfully.
Example:
  procedure ShowReport(AReport: TprCustomReport);
  begin
    if AReport.PrepareReport then
      AReport.PreviewPreparedReport(True);
  end;}
  function PrepareReport: Boolean; virtual;
{Calls the built-in designer of the report template.
To use this method you must add in the uses section of one of application's modules the units:
  For TprReport - pr_Designer
  For TprTxReport - pr_TxDesigner
Parameters:
  Modal - Indicates whether the designer's form should be opened modal.
You can show the window as modal dialog only if the DesignerFormMode property equals to fmNormal.
Example:
  procedure LoadAndDesign(const AReportFile: string; AReport: TprCustomReport);
  begin
    AReport.LoadTemplateFromFile(AReportFile, false);
    AReport.DesignerFormMode := fmNormal;
    AReport.DesignReport(True);
  end;
See also:
  DesignerFormMode}
  procedure DesignReport(Modal: Boolean);
{Calls the built-in previewer of the generated report.
Parameters:
  Modal - Indicates whether the preview form should be opened modal.
You can show the window as modal dialog only if the PreviewFormMode property equals to fmNormal.
Example:
  procedure GenerateAndPreview(AReport: TprCustomReport);
  begin
    if AReport.PrepareReport then
      AReport.PreviewPreparedReport(True);
  end;
See also:
  PreviewFormMode}
  procedure PreviewPreparedReport(Modal: Boolean);
{Opens the built-in dialog window for setup the print params.
Return value:
  Returns the true if user click the OK button in the dialog window.}
  function SetupPrintParams: Boolean; virtual; abstract;
{Prints the generated report.
Before use of this method you must generate the report by the PrepareReport method
or load the generated report by the LoadPreparedReport method.
Return value
  Returns the true if report is printed successfully.}
  function PrintPreparedReport: Boolean; virtual; abstract;
{Opens the built-in dialog window for setup the export params.
Return value:
  Returns the true if user click the OK button in the dialog window.}
  function SetupExportParams: Boolean; virtual; abstract;
{Exports the generated report.
Before use of this method you must generate the report by the PrepareReport method
or load the generated report by the LoadPreparedReport method.}
  procedure ExportTo; virtual;
{Clears all generated data, closes the preview form if it is opened.}
  procedure ClearPreparedReport; virtual;

{Calculates the expression value, for calculation the built-in parser is used.
This function can throw an exception if expression can not be calculated.
Parameters:
  Expression - The expression, for example: "Table.SUM1 * Table.SUM2".
Return value:
  Returns the calculated value.}
  function Calc(var Expression: string): Variant;
{Internal method, used for generating content of the TprMemoObj, TprTxMemoObj, TprRichObj.
This method searches the expressions in the passed string, calculates them and replaces them
by the calculated values.
Parameters:
  Template - The string for parse.
  Res - Contains the result of formatting.
Return value:
  Returns the true if string is parsed successfully.}
  function FormatTemplate(Template: string; var Res: string): Boolean;
{Internal method, used for generating content of the TprMemoObj, TprTxMemoObj, TprRichObj.}
  function FormatStrings(lSource, lDest: TStrings; DeleteEmptyLines, DeleteEmptyLinesAtEnd: Boolean): Boolean;

{Clears the report template.}
  procedure ClearTemplate;
{Saves the report template in the stream.
Parameters:
  Stream - The stream in which the report template will be saved.
  InBinaryFormat - Specifies the format in which the report template will be saved, binary or not (text).}
  procedure SaveTemplate(Stream: TStream; InBinaryFormat: Boolean);
{Saves the report template in the file.
Parameters:
  FileName - The file name in which the report template will be saved,
creates a file if it does not exist, overwrites a file if it exist.
  InBinaryFormat - Specifies the format in which the report template will be saved, binary or not (text).}
  procedure SaveTemplateToFile(const FileName: string; InBinaryFormat : boolean);
{Saves the report template in the TStrings object, the report template can be saved
in the text mode only.
Parameters:
  Dest - The TStrings object in which the report template will be saved.}
  procedure SaveTemplateToStrings(Dest: TStrings);
{Loads the report template from the stream.
Parameters:
  Stream - The source stream.
  InBinaryFormat - Indicates the format (binary or text) of the report template in the stream.}
  procedure LoadTemplate(Stream: TStream; InBinaryFormat: Boolean);
{Loads the report template from the file.
Parameters:
  FileName - The file name.
  InBinaryFormat - Indicates the format (binary or text) of the report template in the stream.}
  procedure LoadTemplateFromFile(const FileName: string; InBinaryFormat: Boolean);
{Loads the report template from the TStrings object.
Parameters:
  FileName - The source TStrings object.}
  procedure LoadTemplateFromStrings(Source : TStrings);

{Loads the generated report from the stream.
Parameters:
  Stream - The source stream.}
  procedure LoadPreparedReport(Stream: TStream); virtual;
{Loads the generated report from the stream and appends it to the this report object.
Parameters:
  Stream - The source stream.
Example:
  // Combines two generated reports in the AReport1 object.
  procedure CombineReports(AReport1, AReport2: TprCustomReport);
  var
    ms: TMemoryStream;
  begin
    ms := TMemoryStream.Create;
    try
      AReport2.SavePreparedReport(ms);
      ms.Seek(0, soFromBeginning);
      AReport1.AppendPreparedReport(ms);
    finally
      ms.Free;
    end;
  end;}
  procedure AppendPreparedReport(Stream: TStream); virtual;
{Saves the generated report in the stream.
Parameters:
  Stream - The destination stream.}
  procedure SavePreparedReport(Stream: TStream); virtual;
{Loads the generated report from the file.
Parameters:
  FileName - The name of file containing the generated report.}
  procedure LoadPreparedReportFromFile(const FileName: string); virtual;
{Loads the generated report from the file and appends it to the this report object.
Parameters:
  FileName - The name of file containing the generated report.
See also:
  AppendPreparedReport}
  procedure AppendPreparedReportFromFile(const FileName: string); virtual;
{Saves the generated report in the file.
Parameters:
  Stream - The name of destination file.}
  procedure SavePreparedReportToFile(const FileName: string); virtual;

{Translates the identifier into the component's reference and the name of component's property or method.
Parameters:
  Ident - The identifier to translate.
  Component - The components used in the identifier.
  LastName - The name of component's property or method used in the identifier.}
  procedure TranslateObjectName(const Ident: string; var Component: TComponent; var LastName: string);

{Forms the list of components which can be used in the report template.
The components' list returned in the L parameter.
By default this list contains:
<ul>
<li>The TprCustomReport components</li>
<li>All components of the report template:</li>
<ul>
<li>template pages</li>
<li>bands</li>
<li>objects</li>
<li>groups</li>
</ul>
<li>All child components of the report owner.</li>
<li>All application datamodules with their sub components.</li>
<li>All application forms with their sub components.</li>
</ul>
Parameters:
  L - Contains the components' list, the Items property contains the components' names,
the Objects property contains the components' references.
See also:
  OnGetAvailableComponents}
  procedure GetAvailableComponents(L: TStrings);
{Forms the list of datasets which can be used in the report template.
By default the report engine builds the list of available components
(with using of the GetAvailableComponents method)
and then Leaves in this list only the descendants of TDataset or TprDataset.
See also:
  OnGetAvailableDataSets}
  procedure GetAvailableDataSets(L: TStrings);
{Forms the list of subreports (TprCustomReport objects) which can be used in the report template.
By default the report engine builds the list of available components
(with using of the GetAvailableComponents method)
and then Leaves in this list only the descendants of TprCustomReport.}
  procedure GetAvailableSubReports(L: TStrings);


{Returns the sub-report object (reference to another TprCustomReport) by its name.
This method builds the list of available sub-reports with using of the
GetAvailableSubReports method and then searches the sub-report in this list.
Parameters:
  SubReportName - The name of sub-report.
Return value:
  Returns the sub-report object or nil if sub-report is not found.}
  function GetSubReportByName(const SubReportName: string): TprCustomReport;
{Returns the dataset object by its name.
This method builds the list of available datasets with using of the
GetAvailableDataSets method and then searches the dataset in this list.
Parameters:
  DataSetName - The name of dataset.
Return value:
  Returns the dataset object or nil if dataset is not found.}
  function GetDataSetByName(const DataSetName: string): TObject;

{Returns the number of current row in the dataset.
Parameters:
  Dataset - The TprDatasetLink object describing the dataset.
Return value:
  Returns the number of current row in the dataset.}
  function GetDataSetRecNo(Dataset: TprDatasetLink): Integer; overload;
{Returns the number of current row in the dataset.
Parameters:
  Dataset - The dataset object (instance of the TDataset or TprDataset).
Return value:
  Returns the number of current row in the dataset.}
  function GetDataSetRecNo(Dataset: TObject): Integer; overload;

{Creates and opens the built-in progress form.
Parameters:
  Caption - The caption of form.
  Max - The value of the Max property of the form's TProgressBar control.}
  procedure CreateProgressForm(const Caption: string; Max: Integer);
{Updates the progress form.
Parameters:
  Text - The custom text which will shown on the form.}
  procedure UpdateProgressForm(const Text: string);
{Closes and destroies the progress form.}
  procedure CloseProgressForm;

{Links the new object to the report, this object will receive notifications when the report
template is changed.
Parameters:
  dnl - The TprNotifyLink object.}
  procedure DsgnAddDesignerNotifyLink(dnl: TprNotifyLink);
{Removes the link between report and object, that was previously added by the
DsgnAddDesignerNotifyLink method.
Parameters:
  dnl - The TprNotifyLink object that was used in the DsgnAddDesignerNotifyLink method.}
  procedure DsgnRemoveDesignerNotifyLink(dnl: TprNotifyLink);
{Call this method to notify all linked objects about changes in the report template.}
  procedure DsgnTemplateChanged(dnl: TprNotifyLink; TemplateChanged: boolean);

{Links the new object to the report, this object will receive notifications when the generated report
template is in the preview window.
Parameters:
  dnl - The TprNotifyLink object.}
  procedure PrvAddNotifyLink(dnl: TprNotifyLink);
{Removes the link between report and object, which previously was added by the
PrvAddNotifyLink method.
Parameters:
  dnl - The TprNotifyLink object which was used in the PrvAddNotifyLink method.}
  procedure PrvRemoveNotifyLink(dnl: TprNotifyLink);
{Call this method to notify all linked objects about changes in the generated report.}
  procedure PrvPreviewChanged(dnl: TprNotifyLink);

{Creates and adds a new page into the report template.
Return value:
  The created page (instance of class which is derived from the TprCustomPage class).}
  function AddPage: TprCustomPage;
{Creates and inserts a new page into the report template.
Parameters:
  InsertPos - The position of the created page.
See also:
  AddPage}
  function InsertPage(InsertPos: Integer): TprCustomPage;
{Deletes and frees the page of the report template in the specified position.
Parameters:
  PageIndex - The index of the deleted page.}
  procedure DeletePage(PageIndex: Integer);

{Searches the object in the report template.
Parameters:
  AObjectName - The name of the object.
Return value:
  Return the found object or nil if object is not found.}
  function FindObject(const AObjectName: string): TprObj;

  function ContainsObject(AReportObject: TObject): Boolean;

{Searches the band in the report template.
Parameters:
  ABandName - The name of the band.
Return value:
  Return the found band or nil if band is not found.}
  function FindBand(const ABandName: string): TprBand;

{Creates an instance of the TprCustomReport class.
Parameters:
  AOwner - The component - owner.}
  constructor Create(AOwner: TComponent); override;
{Frees an instance of the TprCustomReport class.}
  destructor Destroy; override;
published
{Indicates whether the progress form should be shown while generating the report.}
  property ShowProgress: Boolean read FShowProgress write FShowProgress default false;
{Indicates whether the generated report can be edited by the end-user in the preview window.
This option is supported only for TprReport.}
  property CanUserEdit: Boolean read FCanUserEdit write FCanUserEdit default false;

{Determines the form’s style of the built-in designer.
See also:
  TprFormMode}
  property DesignerFormMode: TprFormMode read FDesignerFormMode write FDesignerFormMode default fmNormal;
{Determines the form’s style of the built-in preview window.
See also:
  TprFormMode}
  property PreviewFormMode: TprFormMode read FPreviewFormMode write FPreviewFormMode default fmNormal;

{This property will enable or disable collation.}
  property Collate: Boolean read FCollate write FCollate default False;
{This property returns or sets the current number of copies of the report
that will be printed by the printer.}
  property Copies: Integer read FCopies write FCopies default 1;
{Indicates the page on which the print job is to begin.
This value is used only when the PrintPagesMode property equals to ppmPagesRange.
See also:
  ToPage, PrintPagesMode}
  property FromPage: Integer read FFromPage write FFromPage default -1;
{Indicates the page on which the print job is to end.
This value is used only when the PrintPagesMode property equals to ppmPagesRange.
See also:
  FromPage, PrintPagesMode}
  property ToPage: Integer read FToPage write FToPage default -1;
{Specifies the list of pages to print.
This value is used only when the PrintPagesMode property equals to ppmPagesList.
Example:
  begin
    // ...
    Report.PrintPages := '1, 2, 7-10';
    Report.PrintPagesMode := ppmPagesList;
    Report.PrinterName := prGetDefaultPrinterName; // print the report on default printer.
    Report.PrintPreparedReport;
    // ...
  end;
See also:
  PrintPagesMode}
  property PrintPages: string read FPrintPages write SetPrintPages;
{Specifies the set of pages to printing.
See also:
  TprPrintPagesMode}
  property PrintPagesMode: TprPrintPagesMode read FPrintPagesMode write FPrintPagesMode default ppmAll;
{Specifies the report's title.
This title is used as name for the print job.}
  property Title: string read FTitle write FTitle;

{Specifies the class of export filter, which will be used for the export of report.
The PReport supports exports:
<ul>
<li>TprReport:</li>
<ul>
<li>Microsoft Excel (filter class - TprXLSExportFilter)</li>
<li>HTML (filter class - TprHTMLExportFilter)</li>
</ul>
<li>TprTxReport:</li>
<ul>
<li>TXT (filter class - TprTxTXTExportFilter)</li>
</ul>
</ul>
Example:
  procedure ExportReportToXLS(AReport: TprReport; const AFileName: string);
  begin
    // generate report
    if not AReport.PrepareReport then
      exit;
    // setup parameters of export
    AReport.ExportFilter := 'TprXLSExportFilter';
    AReport.ExportFileName := AFileName;
    AReport.ExportOptions := [preoShowAfterGenerate]; // open file after export
    AReport.ExportPagesMode := ppmAll; // export all pages
    AReport.ExportTo; 
  end;}
  property ExportFilter: string read FExportFilter write FExportFilter;
{Specifies the name of file in which the export's results will be saved.}
  property ExportFileName: string read FExportFileName write FExportFileName;
{Specifies the options of export.
See also:
  TprExportOptionsSet}
  property ExportOptions: TprExportOptionsSet read FExportOptions write FExportOptions default [preoShowParamsDlg, preoShowProgress, preoShowAfterGenerate];
{Specifies the set of pages for export.
See also:
  TprPrintPagesMode}
  property ExportPagesMode: TprPrintPagesMode read FExportPagesMode write FExportPagesMode default ppmAll;
{Indicates the page on which the export is to begin.
This value is used only when the ExportPagesMode property equals to ppmPagesRange.
See also:
  ExportToPage, ExportPagesMode}
  property ExportFromPage: Integer read FExportFromPage write FExportFromPage default -1;
{Indicates the page on which the export is to end.
This value is used only when the ExportPagesMode property equals to ppmPagesRange.
See also:
  ExportFromPage, ExportPagesMode}
  property ExportToPage: Integer read FExportToPage write FExportToPage default -1;
{Specifies the list of pages for export.
This value is used only when the ExportPagesMode property equals to ppmPagesList.
Example:
  begin
    // ...
    Report.ExportPages := '1, 2, 7-10';
    Report.ExportPagesMode := ppmPagesList;
    Report.ExportTo;
    // ...
  end;
See also:
  PrintPagesMode}
  property ExportPages: string read FExportPages write FExportPages;

{Contains the list of report's aggregate variables.
See also:
  TprValues, TprValue}
  property Values: TprValues read FValues write FValues;
{Contains the list of report's variables.
See also:
  TprVariable, TprVariables}
  property Variables: TprVariables read FVariables write FVariables;

{Specifies the name of printer on which the report will be printed.}
  property PrinterName: string read GetPrinterName write SetPrinterName;
{Specifies the hint for "user button" in the built-in preview window.
The developer can define custom action for this button.
See also:
  OnCustomActionInPreview}
  property CustomActionInPreviewCaption: string read FCustomActionInPreviewCaption write FCustomActionInPreviewCaption;

{This event occurs when the built-in parser has found an unknown identifier
on the stage of the report's generating.
See also:
  TOnUnknownVariable}
  property OnUnknownVariable: TOnUnknownVariable read FOnUnknownVariable write FOnUnknownVariable;
{This event occurs when the built-in parser has found an unknown function
on the stage of the report's generating.
See also:
  TOnUnknownFunction}
  property OnUnknownFunction: TOnUnknownFunction read FOnUnknownFunction write FOnUnknownFunction;
{This event occurs when the built-in parser has found an unknown function of object
on the stage of the report's generating.
See also:
  TOnUnknownObjFunction}
  property OnUnknownObjFunction: TOnUnknownObjFunction read FOnUnknownObjFunction write FOnUnknownObjFunction;
{This event occurs when the built-in parser has found an unknown property of object
on the stage of the report's generating.
See also:
  TOnUnknownObjFunction}
  property OnUnknownObjProp: TOnUnknownObjProp read FOnUnknownObjProp write FOnUnknownObjProp;
{This event occurs when the report engine builds the list of datasets which
can be used in the report template.
See also:
  TOnGetAvailableComponents}
  property OnGetAvailableComponents: TOnGetAvailableComponents read FOnGetAvailableComponents write FOnGetAvailableComponents;
{This event occurs when the report engine builds the list of datasets which
can be used in the report template.
See also:
  TOnGetAvailableComponents}
  property OnGetAvailableDatasets: TOnGetAvailableDatasets read FOnGetAvailableDatasets write FOnGetAvailableDatasets;

{This event occurs when user saves the report template in the designer.
See also:
  TprOnDesignerSaveTemplate}
  property OnDesignerSaveTemplate: TprOnDesignerSaveTemplate read FOnDesignerSaveTemplate write FOnDesignerSaveTemplate;
{This event occurs when user opens the report template in the designer.
See also:
  TprOnDesignerOpenTemplate}
  property OnDesignerOpenTemplate: TprOnDesignerOpenTemplate read FOnDesignerOpenTemplate write FOnDesignerOpenTemplate;
{This event occurs when the built-in designer of the report template is created.
Example:

  procedure TForm1.DesignReport(ATemplate: string);
  begin
    FTemp := ATemplate;
    prReport1.LoadTemplateFromFile(ATemplate);
    prReport1.DesignReport(True);
  end;

  procedure TForm1.prReport1CreateDesigner(Sender: TObject);
  begin
    // Assign current template name to the designer form
    TprCustomDesignerForm(TprCustomReport(Sender).DesignerForm).FileName := FTemp;
  end;}
  property OnCreateDesigner: TNotifyEvent read FOnCreateDesigner write FOnCreateDesigner;
{This event occurs when the built-in designer of the report template is destroied.
See also:
  OnCreateDesigner}
  property OnDestroyDesigner: TNotifyEvent read FOnDestroyDesigner write FOnDestroyDesigner;
{This event occurs when the built-in preview window is created.}
  property OnCreatePreview: TNotifyEvent read FOnCreatePreview write FOnCreatePreview;
{This event occurs when the built-in preview window is destroied.}
  property OnDestroyPreview: TNotifyEvent read FOnDestroyPreview write FOnDestroyPreview;
{This event occurs when user closes the "Print setup" dialog.
See also:
  TOnCloseSetupDialogEvent}
  property OnCloseSetupDialog: TOnCloseSetupDialogEvent read FOnCloseSetupDialog write FOnCloseSetupDialog;

{This event occurs for each object of the report template and can be used
for changing the objects' properties.
See also:
  TOnFirstPassObject}
  property OnFirstPassObject: TOnFirstPassObject read FOnFirstPassObject write FOnFirstPassObject;
{This event occurs on the stage of generating of report each time when a band is being processed by the report engine.
See also:
  TOnBandGenerateCell}
  property OnBandGenerateCell: TOnBandGenerateCell read FOnBandGenerateCell write FOnBandGenerateCell;
{This event occurs when the report engine initializes the dataset for the detail band.
See also:
  TOnInitDetailBandDataSet}
  property OnInitDetailBandDataSet: TOnInitDetailBandDataSet read FOnInitDetailBandDataSet write FOnInitDetailBandDataSet;
{Event occurs during the report generating, after processing each object.
With use of this event the developer can assign the own data for each object of the generated report.
See also:
  TOnPreviewGetUserData}
  property OnPreviewGetUserData: TOnPreviewGetUserData read FOnPreviewGetUserData write FOnPreviewGetUserData;
{This event occurs after sending report to the printer.}
  property OnPrintComplete: TNotifyEvent read FOnPrintComplete write FOnPrintComplete;
{This event occurs immediately before sending report to the printer.}
  property OnPrintStart: TNotifyEvent read FOnPrintStart write FOnPrintStart;
{This event occurs before starting of processing the page of the report template.
See also:
  TOnTemplatePageGenerate}
  property OnTemplatePageGenerate: TOnTemplatePageGenerate read FOnTemplatePageGenerate write FOnTemplatePageGenerate;

{This event occurs when mouse move over client area of the preview window.
See also:
  TOnPreviewMouseMove}
  property OnPreviewMouseMove: TOnPreviewMouseMove read FOnPreviewMouseMove write FOnPreviewMouseMove;
{This event occurs when user click mouse button within client area of the preview window.
See also:
  TOnPreviewMouseDown}
  property OnPreviewMouseDown: TOnPreviewMouseDown read FOnPreviewMouseDown write FOnPreviewMouseDown;
{Occurs when the user double-clicks the left mouse button within client area of preview window.
See also:
  TOnPreviewDblClick}
  property OnPreviewDblClick: TOnPreviewDblClick read FOnPreviewDblClick write FOnPreviewDblClick;
{This event occurs when user clicks the "user button" on toolbar of the preview window.
See also:
  CustomActionInPreviewCaption}
  property OnCustomActionInPreview: TNotifyEvent read FOnCustomActionInPreview write FOnCustomActionInPreview;

{Occurs when a print variable is found while report printing.
See also:
  TOnFoundPrintVariable}
  property OnFoundPrintVariable: TOnFoundPrintVariable read FOnFoundPrintVariable write FOnFoundPrintVariable;

{Event occurs before starting of sub-report building and can be used for preparing the data
which are used in sub-report.
See also:
  TprOnBeginSubReportGenerate}
  property OnBeginSubReportGenerate: TprOnBeginSubReportGenerate read FOnBeginSubReportGenerate write FOnBeginSubReportGenerate;
end;

/////////////////////////////////////////////////
//
// TprCustomExportFilter
//
/////////////////////////////////////////////////
{Base class for all export filters.}
TprCustomExportFilter = class(TObject)
public
{Exports the generated report.}
  procedure SaveToFile(Report: TprCustomReport); virtual; abstract;
end;
{TprExportFilterClass is the class type of a TprCustomExportFilter descendant.}
TprExportFilterClass = class of TprCustomExportFilter;

/////////////////////////////////////////////////
//
// Functions
//
/////////////////////////////////////////////////
{Creates and adds new item to the menu.
Parameters:
  Popup - The TMenu object in which new item will be inserted.
  ParentItem - Specifies the parent item for the inserted item, can be nil, for top most item.
  Caption - The item's caption
  ImageResID - The name of resource image.
  OnClick - OnClick handler.
  ShortCut - Specifies the item's shortcut, for example: 'Ctrl+A'.
  Tag - Specifies the value of the TMenuItem.Tag property.
  Enabled - Indicates whether item is enabled.
  Checked - Indicates whether item is checked.}
function AddPopupMenuItemS(Popup: TMenu; ParentItem: TMenuItem; const Caption: string; const ImageResID: string; OnClick : TNotifyEvent; const ShortCut: string; Tag: integer; Enabled,Checked: boolean): TMenuItem;
{Creates and adds new item to the menu.
Parameters:
  Popup - The TMenu object in which new item will be inserted.
  ParentItem - Specifies the parent item for the inserted item, can be nil, for top most item.
  CaptionResID - The integer identifier of the resource string, which contains the caption of item.
  ImageResID - The name of resource image.
  OnClick - OnClick handler.
  ShortCut - Specifies the item's shortcut, for example: 'Ctrl+A'.
  Tag - Specifies the value of the TMenuItem.Tag property.
  Enabled - Indicates whether item is enabled.
  Checked - Indicates whether item is checked.}
function AddPopupMenuItem(Popup: TMenu; ParentItem: TMenuItem; CaptionResID: Integer; const ImageResID: string; OnClick: TNotifyEvent; const ShortCut: string; Tag: Integer; Enabled,Checked: Boolean): TMenuItem;

{Returns the number of screen pixels that make up a logical inch in the horizontal direction.}
function prGetScreenPixelsPerX: Integer;
{Returns the number of screen pixels that make up a logical inch in the vertical direction.}
function prGetScreenPixelsPerY: Integer;
{Returns the number of screen pixels that make up a logical
inch in the horizontal and vertical direction.
Parameters:
  PerX - pixels per inch in the horizontal direction.
  PerY - pixels per inch in the vertical direction.}
procedure prGetScreenPixelsPerInch(var PerX, PerY: Integer);
{Returns the name of default printer.}
function prGetDefaultPrinterName: string;

{Reads the string from the stream and returns it.
Parameters:
  Stream - The source stream.
Return value:
  Returns the readed string.
See also:
  WriteString}
function ReadString(Stream: TStream) : string;
{Reads the TRect structure from the stream.
Parameters:
  Stream - The source stream.
  r - Contains the readed TRect structure.}
procedure ReadRect(Stream: TStream; var r: TRect);
{Writes the string into the stream.
Parameters:
  Stream - The destination stream.
  s - The string to write.
See also:
  ReadString}
procedure WriteString(Stream: TStream; s: string);
{Writes the TRect structure into the stream.
Parameters:
  Stream - The destination stream.
  r - The TRect structure to write.
See also:
  ReadRect}
procedure WriteRect(Stream: TStream; r: TRect);

{Returns the unique name for the component.
Parameters:
  Component - The TComponent object for which the unique name must be generated.
Return value:
  Returns the emprty string if the component has no owner,
  or the unique name otherwise.}
function GetValidComponentName(Component: TComponent) : string;

procedure prLoadResImages(Form: TForm; IL: TImageList);
procedure LoadResImage(b: TBitmap; const ResID: string);
procedure LoadResImageDef(b: TBitmap; const ResID: string; const DefResID: string);

procedure prWriteCompListNames(Writer: TWriter; L: TList);
procedure prReadStringList(Reader: TReader; L: TStrings);

procedure DrawRect(DC: HDC; const R: TRect); overload;
procedure DrawRect(DC: HDC; const R: TRect; AFrameColor: TColor; AFillColor: TColor); overload;

function MulRect(const R : TRect; cx,cy : integer) : TRect; overload;
function DivRect(const R : TRect; cx,cy : integer) : TRect; overload;
function MulDivRect(const r : TRect; cx1,cx2,cy1,cy2 : integer) : TRect;
function MulRect(const r : TRect; cx,cy: Double) : TRect; overload;
function DivRect(const R : TRect; cx,cy : Double) : TRect; overload;

procedure DrawAngleRect(DC : HDC; r : TRect);

{Registers the class of band.
Parameters:
  _ClassRef - The class of band to register.
  _ReportRef - The class of the report component which can contains bands of such type.
  _PropsFormClassName - The class's name of form, which is used for editing the band's properties.}
procedure prRegisterBand(_ClassRef : TprBandClass;
                         _ReportRef : TprCustomReportClass;
                         const _PropsFormClassName : string);
{Registers the class of the report object.
Parameters:
  _ClassRef - The class of object to register.
  _RecClassRef - The class of TprObjRec object which is used by registered object.
  _ReportRef - The class of the report component which can contains objects of such type.
  _CaptionResID - The integer identifier of the resource string, which contains the object's caption which is used in the designer.
  _HintResID - The integer identifier of the resource string, which contains the object's hint which is used in the designer.
  _PropsFormClassName - The class's name of form, which is used for editing the object's properties.
  _VersionPropsFormClassName - The class's name of form, which is used for editing the properties of object's view.
}
procedure prRegisterObj(_ClassRef: TprObjClass;
                        _RecClassRef: TprObjRecClass;
                        _ReportRef: TprCustomReportClass;
                        _CaptionResID: integer;
                        _HintResID: integer;
                        const _PropsFormClassName: string;
                        const _VersionPropsFormClassName: string);
{Registers the export filter.
Parameters:
  _ExportFilterClassRef - The class of export filter.
  _ReportRef - The class of the report component which can be exported by this filter.
  _ExportFilterExtention - The file extension for this export filter.
  _ExportFilterDesc - The description of this export filter.}
procedure prRegisterExportFilter(_ExportFilterClassRef: TprExportFilterClass;
                                 _ReportRef: TprCustomReportClass;
                                 const _ExportFilterExtention: string;
                                 const _ExportFilterDesc: string);

{Creates an instance of the TprObj class.
Parameters:
  ClassRef - Specifies the class of the created object.
  Band - Specifies the band containing the created object.
Return value:
  Returns the created object.}
function CreatePrObj(ClassRef: TprObjClass; Band: TprBand): TprObj;

{Creates an instance of the TprPreviewUserData object.
Parameters:
  ClassName - Specifies the class's name of the created object.
Return value:
  Returns the created object.}
function CreatePrPreviewUserData(const ClassName: string): TprPreviewUserData;

type

{Represents the registered band.
Syntax:
  TprBandRegInfo = record
    ClassRef : TprBandClass;
    ReportRef : TprCustomReportClass;
    PropsFormClassName : string;
  end;
See also:
  prBandRegInfos}
TprBandRegInfo = record
  ClassRef : TprBandClass;
  ReportRef : TprCustomReportClass;
  PropsFormClassName : string;
end;

{Represents the registered object.
Syntax:
  TprObjRegInfo = record
    ClassRef : TprObjClass;
    RecClassRef : TprObjRecClass;
    ReportRef : TprCustomReportClass;
    CaptionResID : integer;
    HintResID : integer;
    PropsFormClassName : string;
    VersionPropsFormClassName : string;
  end;
See also:
  prBandRegInfos}
TprObjRegInfo = record
  ClassRef : TprObjClass;
  RecClassRef : TprObjRecClass;
  ReportRef : TprCustomReportClass;
  CaptionResID : integer;
  HintResID : integer;
  PropsFormClassName : string;
  VersionPropsFormClassName : string;
end;

{Represents the registered export filter.
Syntax:
  TprExportFilterRegInfo = record
    ExportFilterClassRef : TprExportFilterClass;
    ExportFilterExtention : string;
    ExportFilterDesc : string;
    ReportRef : TprCustomReportClass;
  end;
See also:
  prExportFiltersInfos}
TprExportFilterRegInfo = record
  ExportFilterClassRef : TprExportFilterClass;
  ExportFilterExtention : string;
  ExportFilterDesc : string;
  ReportRef : TprCustomReportClass;
end;
{Pointer to the PprExportFilterRegInfo structure.}
PprExportFilterRegInfo = ^TprExportFilterRegInfo;

var
{Contains the list of registered objects' types.}
  prObjRegInfos: array of TprObjRegInfo;
{Contains the list of registered bands' types.}
  prBandRegInfos: array of TprBandRegInfo;
{Contains the list of registered export filters.}
  prExportFiltersInfos: array of TprExportFilterRegInfo;
{Specifies the name of ini-file which is used for saving the settings.
By default this file has name 'pr.ini',
this file is searched in this order:
<ul>
<li>The system variable is searched with name 'PR_INIFILENAME', this
variable can be defined in the bat file as:<br>
SET PR_INIFILENAME = C:\TEMP\QQQ.INI</li>
<li>In the application directory.</li>
<li>In the windows system directory (X:\WINDOWS\SYSTEM).</li>
<li>In the windows directory (X:\WINDOWS).</li>
</ul>
If file is not found it is created in the windows directory.}
  prIniFileName: string;

{Contains the bands' captions.}
  BandTitles : array [TprBandType] of string = (
    ('Title'),
    ('Summary'),
    ('PageHeader'),
    ('PageFooter'),
    ('Detail'),
    ('DetailHeader'),
    ('DetailFooter'),
    ('GroupHeader'),
    ('GroupFooter'),
    ('VTitle'),
    ('VSummary'),
    ('VPageHeader'),
    ('VPageFooter'),
    ('VDetail'),
    ('VDetailHeader'),
    ('VDetailFooter'),
    ('VGroupHeader'),
    ('VGroupFooter'));
  
  prTemplateChangedGlobalProc: TOnTemplateChangedGlobalProc;
{$IFDEF PR_D4}
  prCreatedReports: TList;
{$ENDIF}

const
{Contains the list of the vertical bands.
Syntax:
  VerticalBands: set of TprBandType = [btvTitle,
                                        btvSummary,
                                        btvPageHeader,
                                        btvPageFooter,
                                        btvDetail,
                                        btvDetailHeader,
                                        btvDetailFooter,
                                        btvGroupHeader,
                                        btvGroupFooter];}
  VerticalBands: set of TprBandType = [btvTitle,
                                        btvSummary,
                                        btvPageHeader,
                                        btvPageFooter,
                                        btvDetail,
                                        btvDetailHeader,
                                        btvDetailFooter,
                                        btvGroupHeader,
                                        btvGroupFooter];
{Contains the list of the horizontal bands.
Syntax:
  HorizontalBands: set of TprBandType = [bthTitle,
                                         bthSummary,
                                         bthPageHeader,
                                         bthPageFooter,
                                         bthDetail,
                                         bthDetailHeader,
                                         bthDetailFooter,
                                         bthGroupHeader,
                                         bthGroupFooter];}
  HorizontalBands: set of TprBandType = [bthTitle,
                                         bthSummary,
                                         bthPageHeader,
                                         bthPageFooter,
                                         bthDetail,
                                         bthDetailHeader,
                                         bthDetailFooter,
                                         bthGroupHeader,
                                         bthGroupFooter];


implementation

uses
  pr_Strings, pr_CommonPreviewPanel, pr_Parser;//, vgr_Functions;

const
  PageBandsOrder : array [0..MAX_PAGEBANDSORDER] of TprBandType = (bthTitle,bthPageHeader,bthPageFooter,bthDetail,bthSummary,bthDetailHeader,bthDetailFooter,bthGroupHeader,bthGroupFooter);
  PageBandsOrderVert : array [0..MAX_PAGEBANDSORDERVERT] of TprBandType = (btvTitle,btvPageHeader,btvPageFooter,btvDetail,btvSummary,btvDetailHeader,btvDetailFooter,btvGroupHeader,btvGroupFooter);
  PageHeadersFootersBands : array [0..MAX_PAGEHEADERSFOOTERS] of TprBandType = (btvPageHeader,btvPageFooter,bthPageHeader,bthPageFooter);

  PrintVariablePrefix = '@@';
  LenPrintVariablePrefix = 2;
  PrintVariableSuffix = '@@';
  LenPrintVariableSuffix = 2;


  // strings with internal error messages
  sCellNotLinkedToVerticalVector = 'Cell not linked to vertical vector';
  sCellNotLinkedToHorizontalVector = 'Cell not linked to horizontal vector';
  sInvalidBandTypeForStartNewPage = 'Band [%s] (%s) can not begin new page.';
  sInvalidBandTypeForBreakPage = 'Band [%s] (%s) can not break page.';
  sInvalidLink = 'The link between [%s] (%s) and [%s] (%s) bands can not be defined.';
  sInvalidBandTypeForLink = 'Band [%s] (%s) can not be linked to another bands.';
  sInvalidBandTypeForReprintOnEachPage = 'Band [%s] (%s) can not be reprinted.';

  sInvalidBandTypeForSplit = 'Band of type [%s] can not be splitted';
  sColumnIfUseVerticalBands = 'Column can not be used for horizontal bands that use the vertical bands';
  sSubReportCanNotSpecified = 'For the [%s] band the subreport can not be specified.';
  sSubReportCanNotSpecifiedInCrossTab = 'The subreports can not be used in the cross-tab reports.';
  sSubReportNotFont = 'The subreport[%s] is not found.';

type
  /////////////////////////////////////////////////
  //
  // rprShortCutItem
  //
  /////////////////////////////////////////////////
  rprShortCutItem = record
    MenuItem: TMenuItem;
    ShortCut: TShortCut;
  end;
  pprShortCutItem = ^rprShortCutItem;

  /////////////////////////////////////////////////
  //
  // TprShortCutsList
  //
  /////////////////////////////////////////////////
  TprShortCutsList = class(TList)
  public
    procedure SaveShortCuts(Menu : TMenu);
    procedure RemoveShortCuts;
    procedure RestoreShortCuts;
    destructor Destroy; override;
  end;

  TSetLayeredWindowAttributes = function (Hwnd: THandle; crKey: COLORREF; bAlpha: Byte; dwFlags: DWORD): Boolean; stdcall;

var
  SetLayeredWindowAttributes : TSetLayeredWindowAttributes = nil;

/////////////////////////////////////////////////
//
// FUNCTIONS
//
/////////////////////////////////////////////////
function AddPopupMenuItemS(Popup : TMenu; ParentItem : TMenuItem; const Caption : string; const ImageResID : string; OnClick : TNotifyEvent; const ShortCut : string; Tag : integer; Enabled,Checked : boolean) : TMenuItem;
begin
Result := TMenuItem.Create(Popup);
if Caption='-' then
  Result.Caption := '-'
else
  begin
    if Length(Caption)>40 then
      Result.Caption := Copy(Caption,1,40)+'...'
    else
      Result.Caption := Caption;
    Result.OnClick := OnClick;
    if ImageResID<>'' then
      LoadResImage(Result.Bitmap,ImageResID);
    Result.ShortCut := TextToShortCut(ShortCut);
    Result.Enabled := Enabled;
    Result.Checked := Checked;
    Result.Tag := Tag;
  end;
if ParentItem=nil then
  Popup.Items.Add(Result)
else
  ParentItem.Add(Result);
end;

function AddPopupMenuItem(Popup : TMenu; ParentItem : TMenuItem; CaptionResID : integer; const ImageResID : string; OnClick : TNotifyEvent; const ShortCut : string; Tag : integer; Enabled,Checked : boolean) : TMenuItem;
begin
if CaptionResID=0 then
  Result := AddPopupMenuItemS(Popup,ParentItem,'-',ImageResID,OnClick,ShortCut,Tag,Enabled,Checked)
else
  Result := AddPopupMenuItemS(Popup,ParentItem,prLoadStr(CaptionResID),ImageResID,OnClick,ShortCut,Tag,Enabled,Checked)
end;

procedure prGetScreenPixelsPerInch(var PerX,PerY : integer);
var
  DC : HDC;
begin
DC := GetDC(0);
PerX := GetDeviceCaps(DC,LOGPIXELSX);
PerY := GetDeviceCaps(DC,LOGPIXELSY);
ReleaseDC(0,DC);
end;

function prGetScreenPixelsPerX : integer;
var
  DC : HDC;
begin
DC := GetDC(0);
Result := GetDeviceCaps(DC,LOGPIXELSX);
ReleaseDC(0,DC);
end;

function prGetScreenPixelsPerY : integer;
var
  DC : HDC;
begin
DC := GetDC(0);
Result := GetDeviceCaps(DC,LOGPIXELSY);
ReleaseDC(0,DC);
end;

function prGetDefaultPrinterName;
var
  n : integer;
  Buffer : pointer;
  BytesNeeded,NumInfo : cardinal;
begin
Result := '';
Buffer := nil;
BytesNeeded := 0;

case Win32Platform of
  VER_PLATFORM_WIN32_WINDOWS:
    begin
      EnumPrinters(PRINTER_ENUM_DEFAULT,nil,5,Buffer,0,BytesNeeded,NumInfo);
      if BytesNeeded<>0 then
        begin
          GetMem(Buffer,BytesNeeded);
          try
            if EnumPrinters(PRINTER_ENUM_DEFAULT,nil,5,Buffer,BytesNeeded,BytesNeeded,NumInfo) then
              if NumInfo>0 then
                Result:=StrPas(PPrinterInfo5(Buffer).pPrinterName);
          finally
            FreeMem(Buffer);
          end
        end;
    end;
  VER_PLATFORM_WIN32_NT:
    begin
    {
      EnumPrinters(PRINTER_ENUM_DEFAULT,nil,4,Buffer,0,BytesNeeded,NumInfo);
      if BytesNeeded<>0 then
        begin
          GetMem(Buffer,BytesNeeded);
          try
            if EnumPrinters(PRINTER_ENUM_DEFAULT,nil,4,Buffer,BytesNeeded,BytesNeeded,NumInfo) then
              if NumInfo>0 then
                Result:=StrPas(PPrinterInfo4(Buffer).pPrinterName);
          finally
            FreeMem(Buffer);
          end
        end
      else
        outputdebugstring(pchar(syserrormessage(getlasterror)));
    }
    end;
end;

if Result='' then
  begin
    SetLength(Result,80);
    n := GetProfileString('windows','device','',@(Result[1]),79);
    SetLength(Result,n);
    n := pos(',',Result);
    if n<>0 then
      Result := Copy(Result,1,n-1);
  end;
end;

procedure prLoadResImages(Form: TForm; IL: TImageList);
var
  i : integer;
  c : TComponent;
  l : TStringList;
  b : TBitmap;

  function LoadImage(ResName : string) : integer;
  begin
  ResName:=AnsiUpperCase(Copy(ResName,2,Length(ResName)));
  Result :=l.IndexOf(ResName);
  if Result=-1 then
    begin
      if FindResource(hInstance,PChar('PR_'+ResName),RT_BITMAP)<>0 then
        try
          b.LoadFromResourceName(hInstance,'PR_'+ResName);
          l.AddObject(ResName,TObject(IL.AddMasked(b,b.TransparentColor)));
        except
          l.AddObject(ResName,TObject(-1));
        end
      else
        l.AddObject(ResName,TObject(-1));
      Result:=integer(l.Objects[l.Count-1])
    end
  else
    begin
      Result:=integer(l.Objects[Result]);
    end;
  end;

begin
l:=TStringList.Create;
b:=TBitmap.Create;
try
  for i:=0 to Form.ComponentCount-1 do
    begin
      c := Form.Components[i];
      if (c is TToolButton) and (TToolButton(c).Action=nil) and (TToolButton(c).Style<>tbsSeparator) then
        begin
          TToolButton(c).ImageIndex := LoadImage(TToolButton(c).Name);
        end;

      if Form.Components[i] is TAction then
        begin
          TAction(c).ImageIndex := LoadImage(TAction(c).Name);
        end
    end;
finally
  l.Free;
  b.Free;
end;
end;

function MulRect(const R : TRect; cx,cy : integer): TRect;
begin
Result.Left := r.Left*cx;
Result.Right := r.Right*cx;
Result.Top := r.Top*cy;
Result.Bottom := r.Bottom*cy;
end;

function DivRect(const R : TRect; cx,cy : integer) : TRect;
begin
Result.Left := r.Left div cx;
Result.Right := r.Right div cx;
Result.Top := r.Top div cy;
Result.Bottom := r.Bottom div cy;
end;

function MulDivRect(const r : TRect; cx1,cx2,cy1,cy2 : integer) : TRect;
begin
Result.Left := MulDiv(r.Left,cx1,cx2);
Result.Right := MulDiv(r.Right,cx1,cx2);
Result.Top := MulDiv(r.Top,cy1,cy2);
Result.Bottom := MulDiv(r.Bottom,cy1,cy2);
end;

function MulRect(const r : TRect; cx,cy: Double) : TRect; 
begin
  Result.Left := Round(r.Left * cx);
  Result.Right := Round(r.Right * cx);
  Result.Top := Round(r.Top * cy);
  Result.Bottom := Round(r.Bottom * cy);
end;

function DivRect(const R : TRect; cx,cy : Double) : TRect;
begin
Result.Left := Round(r.Left / cx);
Result.Right := Round(r.Right / cx);
Result.Top := Round(r.Top / cy);
Result.Bottom := Round(r.Bottom / cy);
end;

procedure prRegisterBand(_ClassRef : TprBandClass;
                         _ReportRef : TprCustomReportClass; 
                         const _PropsFormClassName : string);
begin
SetLength(prBandRegInfos,Length(prBandRegInfos)+1);
with prBandRegInfos[High(prBandRegInfos)] do
  begin
    ClassRef := _ClassRef;
    ReportRef := _ReportRef;
    PropsFormClassName := _PropsFormClassName;
  end;
end;

procedure prRegisterObj(_ClassRef : TprObjClass;
                        _RecClassRef : TprObjRecClass;
                        _ReportRef : TprCustomReportClass;
                        _CaptionResID : integer;
                        _HintResID : integer;
                        const _PropsFormClassName : string;
                        const _VersionPropsFormClassName : string);
begin
SetLength(prObjRegInfos,Length(prObjRegInfos)+1);
with prObjRegInfos[High(prObjRegInfos)] do
  begin
    ClassRef := _ClassRef;
    RecClassRef := _RecClassRef;
    ReportRef := _ReportRef;
    CaptionResID := _CaptionResID;
    HintResID := _HintResID;
    PropsFormClassName := _PropsFormClassName;
    VersionPropsFormClassName := _VersionPropsFormClassName;
  end;
end;

procedure prRegisterExportFilter;
begin
SetLength(prExportFiltersInfos,Length(prExportFiltersInfos)+1);
with prExportFiltersInfos[High(prExportFiltersInfos)] do
  begin
    ExportFilterClassRef := _ExportFilterClassRef;
    ExportFilterExtention := _ExportFilterExtention;
    ExportFilterDesc := _ExportFilterDesc;
    ReportRef := _ReportRef; 
  end;
end;

procedure DrawAngleRect(DC : HDC; r : TRect);
var
  npn,opn : HPen;
begin
npn := CreatePen(PS_SOLID,1,clBlack);
opn := SelectObject(DC,npn);

MoveToEx(DC,r.Left+AngleSize,r.Top,nil);
LineTo(DC,r.Left,r.Top);
LineTo(DC,r.Left,r.Top+AngleSize);

MoveToEx(DC,r.Left+AngleSize,r.Bottom-1,nil);
LineTo(DC,r.Left,r.Bottom-1);
LineTo(DC,r.Left,r.Bottom-AngleSize-1);

MoveToEx(DC,r.Right-AngleSize-1,r.Top,nil);
LineTo(DC,r.Right-1,r.Top);
LineTo(DC,r.Right-1,r.Top+AngleSize);

MoveToEx(DC,r.Right-AngleSize-1,r.Bottom-1,nil);
LineTo(DC,r.Right-1,r.Bottom-1);
LineTo(DC,r.Right-1,r.Bottom-AngleSize-1);

SelectObject(DC,opn);
DeleteObject(npn);
end;

function CreatePrObj(ClassRef: TprObjClass; Band: TprBand) : TprObj;
begin
Result := ClassRef.Create(Band.Report.prOwner);
Result.Name := GetValidComponentName(Result);
Result.Band := Band;
end;

procedure DrawRect(DC: HDC; const R: TRect);
begin
MoveToEx(DC,R.Left,R.Top,nil);
LineTo(DC,R.Right-1,R.Top);
MoveToEx(DC,R.Left,R.Bottom-1,nil);
LineTo(DC,R.Right-1,R.Bottom-1);
MoveToEx(DC,R.Left,R.Top+1,nil);
LineTo(DC,R.Left,R.Bottom-1);
MoveToEx(DC,R.Right-1,R.Top,nil);
LineTo(DC,R.Right-1,R.Bottom);
end;

procedure DrawRect(DC: HDC; const R: TRect; AFrameColor: TColor; AFillColor: TColor);
var
  ABrush: HBRUSH;
begin
  ABrush := CreateSolidBrush(GetRGBColor(AFrameColor));
  with R do
  begin
    FillRect(DC, Rect(Left, Top, Right, Top + 1), ABrush);
    FillRect(DC, Rect(Right - 1, Top, Right, Bottom), ABrush);
    FillRect(DC, Rect(Left, Bottom - 1, Right, Bottom), ABrush);
    FillRect(DC, Rect(Left, Top, Left + 1, Bottom), ABrush);
  end;
  DeleteObject(ABrush);
  ABrush := CreateSolidBrush(GetRGBColor(AFillColor));
  with R do
    FillRect(DC, Rect(Left + 1, Top + 1, Right - 1, Bottom - 1), ABrush);
  DeleteObject(ABrush);
end;

procedure WriteString(Stream : TStream; s : string);
var
  b : integer;
begin
b:=Length(s);
Stream.Write(b,4);
Stream.Write(s[1],b);
end;

procedure WriteRect(Stream : TStream; r : TRect);
begin
Stream.Write(r.Left,4);
Stream.Write(r.Top,4);
Stream.Write(r.Right,4);
Stream.Write(r.Bottom,4);
end;

function ReadString(Stream : TStream) : string;
var
  b : integer;
begin
  Stream.Read(b,4);
  SetLength(Result,b);
  Stream.Read(Result[1],b);
end;

procedure ReadRect(Stream : TStream; var r : TRect);
begin
  Stream.Read(r.Left,4);
  Stream.Read(r.Top,4);
  Stream.Read(r.Right,4);
  Stream.Read(r.Bottom,4);
end;

procedure LoadResImage(b : TBitmap; const ResID : string);
begin
  try
    if FindResource(hInstance,PChar('PR_'+ResID),RT_BITMAP)<>0 then
      b.LoadFromResourceName(hInstance,'PR_'+ResID);
  except
  end;
end;

procedure LoadResImageDef(b : TBitmap; const ResID : string; const DefResID : string);
begin
if FindResource(hInstance,PChar('PR_'+ResID),RT_BITMAP)<>0 then
  LoadResImage(b,ResID)
else
  LoadResImage(b,DefResID)
end;

function GetValidComponentName;
var
  i : integer;
  s : string;
begin
if Component.Owner=nil then
  Result := ''
else
  begin
    s := Copy(Component.ClassName,2,Length(Component.ClassName));
    i := 1;
    while Component.Owner.FindComponent(s+IntToStr(i))<>nil do Inc(i);
    Result := s+IntToStr(i);
  end;
end;

procedure prWriteCompListNames(Writer : TWriter; L : TList);
var
  i : integer;
begin
Writer.WriteListBegin;
for i:=0 to L.Count-1 do
  Writer.WriteString(TComponent(L[i]).Name);
Writer.WriteListEnd;
end;

procedure prReadStringList(Reader : TReader; L : TStrings);
begin
Reader.ReadListBegin;
while not Reader.EndOfList do
  L.Add(Reader.ReadString);
Reader.ReadListEnd;
end;

/////////////////////////////////////////////////
//
// TprPersistent
//
/////////////////////////////////////////////////
{Base class that is derived from TPersistent that can generate an event when its properties are changed.}
procedure TprPersistent.DoChanged;
begin
  if Assigned(FOnChange) then
   FOnChange(Self);
end;

/////////////////////////////////////////////////
//
// TprOptions
//
/////////////////////////////////////////////////
procedure TprOptions.WriteToIni(IniFile : TIniFile; const SectionName, Prefix: string);
begin
end;

procedure TprOptions.ReadFromIni(IniFile : TIniFile; const SectionName, Prefix: string); 
begin
end;

/////////////////////////////////////////////////
//
// TprScrollBox
//
/////////////////////////////////////////////////
procedure TprScrollBox.WMVscroll;
begin
inherited;
if Assigned(OnVScroll) then
  OnVScroll(Self,Msg);
end;

procedure TprScrollBox.WMHscroll;
begin
inherited;
if Assigned(OnHScroll) then
  OnHScroll(Self,Msg);
end;

{
procedure TprScrollBox.AutoScrollInView(AControl: TControl);
begin
if AutoScrollInViewEnabled then
  inherited;
end;
}

function TprScrollBox.AutoScrollEnabled: Boolean;
begin
  Result := FAutoScrollInViewEnabled and inherited AutoScrollEnabled;
end;

function TprScrollBox.GetVertScrollPageSize : integer;
var
  si : tagSCROLLINFO;
begin
ZeroMemory(@si,sizeof(si));
si.cbSize := sizeof(si);
si.fMask := SIF_PAGE;
if GetScrollInfo(Handle,SB_VERT,si) then
  Result := si.nPage
else
  Result := -1;
end;

function TprScrollBox.GetHorzScrollPageSize : integer;
var
  si : tagSCROLLINFO;
begin
ZeroMemory(@si,sizeof(si));
si.cbSize := sizeof(si);
si.fMask := SIF_PAGE;
if GetScrollInfo(Handle,SB_HORZ,si) then
  Result := si.nPage
else
  Result := -1;
end;

/////////////////////////////
//
// TprForm
//
/////////////////////////////
procedure TprForm.AfterConstruction;
var
  Ini : TIniFile;
begin
inherited;
Ini := TIniFile.Create(prIniFileName);
try
  prRestoreProperties(Ini,ClassName);
finally
  Ini.Free;
end;
end;

procedure TprForm.BeforeDestruction;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(prIniFileName);
  try
    prSaveProperties(Ini,ClassName);
  finally
    Ini.Free;
  end;
  inherited;
end;

procedure TprForm.prRestoreProperties;
var
  ws : TWindowState;
begin
// restore form position
ws := TWindowState(Ini.ReadInteger(sn,'WindowState',integer(wsNormal)));
if ws<>wsMinimized then
  begin
    if ws=wsNormal then
      begin
        Left := Ini.ReadInteger(sn,'Left',Left);
        Top := Ini.ReadInteger(sn,'Top',Top);
        if BorderStyle in [bsSizeToolWin,bsSizeable] then
          begin
            Width := Ini.ReadInteger(sn,'Width',Width);
            Height := Ini.ReadInteger(sn,'Height',Height);
          end
      end
    else
      WindowState := ws;
  end;
end;

procedure TprForm.prSaveProperties;
begin
Ini.WriteInteger(sn,'WindowState',integer(WindowState));
Ini.WriteInteger(sn,'Left',Left);
Ini.WriteInteger(sn,'Top',Top);
if BorderStyle in [bsSizeToolWin,bsSizeable] then
  begin
    Ini.WriteInteger(sn,'Width',Width);
    Ini.WriteInteger(sn,'Height',Height);
  end
end;

/////////////////////////////////////////////////
//
// TprToolWindowForm
//
/////////////////////////////////////////////////
constructor TprToolWindowForm.Create(AOwner : TComponent);
begin
  FAlphaBlendValue := 255;
  inherited;
end;

procedure TprToolWindowForm.SetAlphaBlendValue(Value : integer);
begin
if FAlphaBlendValue=Value then exit;
FAlphaBlendValue := Value;
InitAlphaBlend;
end;

function TprToolWindowForm.GetOpacity : integer;
begin
  Result := MulDiv(FAlphaBlendValue, 100, 255);
end;

procedure TprToolWindowForm.SetOpacity(Value : integer);
begin
  AlphaBlendValue := MulDiv(Value,255,100);
end;

procedure TprToolWindowForm.CreateWindowHandle(const Params: TCreateParams);
begin
inherited;
InitAlphaBlend;
end;

procedure TprToolWindowForm.InitAlphaBlend;
const
  cUseAlpha : array [Boolean] of integer = (0,LWA_ALPHA_PR);
var
  AStyle : Integer;
begin       
if Assigned(SetLayeredWindowAttributes) and HandleAllocated then
  begin
    AStyle := GetWindowLong(Handle,GWL_EXSTYLE);
    if AlphaBlendValue<>255 then
      begin
        if (AStyle and WS_EX_LAYERED_PR)=0 then
          SetWindowLong(Handle,GWL_EXSTYLE,AStyle or WS_EX_LAYERED_PR);
        SetLayeredWindowAttributes(Handle,clBlack,FAlphaBlendValue,cUseAlpha[AlphaBlendValue<>255]);
      end
    else
      begin
        SetWindowLong(Handle,GWL_EXSTYLE,AStyle and not WS_EX_LAYERED_PR);
        RedrawWindow(Handle,nil,0,RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
      end;
  end;
end;

procedure TprToolWindowForm.DoShow;
var
  ParentForm : TCustomForm;
begin               
if GetParentControl<>nil then
  begin
    ParentForm := GetParentForm(GetParentControl);
    if fsModal in ParentForm.FormState then
      begin
        SetWindowPos(Handle,HWND_TOPMOST,0,0,0,0,SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOZORDER);
        SetWindowLong(Handle,GWL_HWNDPARENT,Longint(ParentForm.Handle));
      end;
  end;
inherited;
end;

procedure TprToolWindowForm.SetFocusOnFirstControl;
begin
SetFocus;
ActiveControl := FindNextControl(nil,true,true,false);
end;  

/////////////////////////////////////////////////
//
// TprShortCutsList
//
/////////////////////////////////////////////////
destructor TprShortCutsList.Destroy;
var
  i : integer;
begin
for i:=0 to Count-1 do
  FreeMem(Items[i]);
inherited;
end;

procedure TprShortCutsList.SaveShortCuts(Menu : TMenu);
var
  i : integer;

  procedure AddShortCut(Item : TMenuItem);
  var
    i : integer;
    p : pprShortCutItem;
  begin
  if Item.ShortCut<>0 then
    begin
      GetMem(p,sizeof(rprShortCutItem));
      p.MenuItem := Item;
      p.ShortCut := Item.ShortCut;
      Add(p);
    end;
  for i:=0 to Item.Count-1 do
    AddShortCut(Item.Items[i]);
  end;
  
begin
for i:=0 to Menu.Items.Count-1 do
  AddShortCut(Menu.Items[i]);
end;

procedure TprShortCutsList.RemoveShortCuts;
var
  i : integer;
begin
for i:=0 to Count-1 do
  pprShortCutItem(Items[i]).MenuItem.ShortCut := 0;
end;

procedure TprShortCutsList.RestoreShortCuts;
var
  i : integer;
begin
for i:=0 to Count-1 do
  pprShortCutItem(Items[i]).MenuItem.ShortCut := pprShortCutItem(Items[i]).ShortCut;
end;

/////////////////////////////////////////////////
//
// TprDesignerAndPreviewBaseForm
//
/////////////////////////////////////////////////
procedure TprDesignerAndPreviewBaseForm.AfterConstruction;
begin
  if Menu <> nil then
  begin
    FShortCuts := TprShortCutsList.Create;
    TprShortCutsList(FShortCuts).SaveShortCuts(Menu);
  end;
  inherited;
end;

procedure TprDesignerAndPreviewBaseForm.BeforeDestruction;
begin
  if FShortCuts <> nil then
  begin
    FShortCuts.Free;
    FShortCuts := nil;
  end;
  inherited;
end;

procedure TprDesignerAndPreviewBaseForm.RestoreShortCuts;
begin
  if FShortCuts<>nil then
    TprShortCutsList(FShortCuts).RestoreShortCuts;
end;

procedure TprDesignerAndPreviewBaseForm.RemoveShortCuts;
begin
  if FShortCuts<>nil then
    TprShortCutsList(FShortCuts).RemoveShortCuts;
end;

procedure TprDesignerAndPreviewBaseForm.Activate;
begin
RestoreShortCuts;
inherited;
end;

procedure TprDesignerAndPreviewBaseForm.Deactivate;
begin
RemoveShortCuts;
inherited;
end;

/////////////////////////////////////////////////
//
// TprNotifyLink
//
/////////////////////////////////////////////////
procedure TprNotifyLink.DoNotify(Source: TObject);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Source);
end;

/////////////////////////////////////////////////
//
// TprDesigner
//
/////////////////////////////////////////////////
constructor TprDesigner.CreateDesigner(AOwner : TComponent; _Report : TprCustomReport);
begin
  FReport := _Report;
  Report.FDesignerForm := Self;
  inherited Create(AOwner);
end;

procedure TprDesigner.BeforeDestruction;
begin
inherited;
Report.FDesignerForm:=nil;
if Assigned(Report.OnDestroyDesigner) then
  Report.OnDestroyDesigner(Report);
end;

procedure TprDesigner.Loaded;
begin
inherited;
if csDesigning in Report.ComponentState then
  begin
    FormStyle := fsNormal;
    Visible := false;
  end
else
  begin
    if Report.DesignerFormMode=fmNormal then
      begin
        FormStyle := fsNormal;
        Visible := false;
     end;
  end;
end;

procedure TprDesigner.CreateWnd;
begin
if csDesigning in Report.ComponentState then
  begin
    FormStyle := fsNormal;
    Visible := false;
  end
else
  begin
    if Report.DesignerFormMode=fmNormal then
      begin
        FormStyle := fsNormal;
        Visible := false;
      end;
  end;
inherited;
end;

/////////////////////////////
//
// TprPreview
//
/////////////////////////////
constructor TprPreview.CreatePreview;
begin
  FReport := _Report;
  Report.FPreviewForm := Self;
  inherited Create(AOwner);
end;

function TprPreview.GetPageIndex: Integer;
begin
  Result := TprCustomPreviewPanel(PreviewPanel).PageIndex;
end;

procedure TprPreview.SetPageIndex(Value: Integer);
begin
  TprCustomPreviewPanel(PreviewPanel).PageIndex := Value;
end;

function TprPreview.GetPageCount: Integer;
begin
  Result := TprCustomPreviewPanel(PreviewPanel).PageCount;
end;

procedure TprPreview.BeforeDestruction;
begin
  inherited;
  Report.FPreviewForm:=nil;
  if Assigned(Report.OnDestroyPreview) then
    Report.OnDestroyPreview(Report);
end;

procedure TprPreview.Loaded;
begin
  inherited;
  if csDesigning in Report.ComponentState then
  begin
    FormStyle := fsNormal;
    Visible := false;
  end
  else
  begin
    if Report.PreviewFormMode=fmNormal then
      begin
        FormStyle:=fsNormal;
        Visible  :=false;
      end;
  end;
end;

procedure TprPreview.CreateWnd;
begin
  if csDesigning in Report.ComponentState then
  begin
    FormStyle := fsNormal;
    Visible := false;
  end
  else
  begin
    if Report.PreviewFormMode=fmNormal then
      begin
        FormStyle:=fsNormal;
        Visible  :=false;
      end;
  end;
  inherited;
end;

procedure TprPreview.DoOnCustomAction;
begin
  if Assigned(Report.OnCustomActionInPreview) then
    Report.OnCustomActionInPreview(Self);
end;

procedure TprPreview.GotoPage(APageIndex: Integer);
begin
  TprCustomPreviewPanel(PreviewPanel).GotoPage(APageIndex);
end;

procedure TprPreview.GotoPriorPage;
begin
  TprCustomPreviewPanel(PreviewPanel).GotoPriorPage;
end;

procedure TprPreview.GotoNextPage;
begin
  TprCustomPreviewPanel(PreviewPanel).GotoNextPage;
end;

procedure TprPreview.GotoFirstPage;
begin
  TprCustomPreviewPanel(PreviewPanel).GotoFirstPage;
end;

procedure TprPreview.GotoLastPage;
begin
  TprCustomPreviewPanel(PreviewPanel).GotoLastPage;
end;

function CreateprPreviewUserData(const ClassName: string): TprPreviewUserData;
var
  PreviewUserDataClass : TPersistentClass;
begin
  PreviewUserDataClass := GetClass(ClassName);
  if PreviewUserDataClass=nil then
    raise Exception.CreateFmt(prLoadStr(sErrorGetPreviewUserDataClass),[ClassName]);
  Result := TprPreviewUserData(PreviewUserDataClass.Create);
end;
       
/////////////////////
//
// TprPreviewUserData
//
/////////////////////
procedure TprPreviewUserData.Assign;
begin
with TprPreviewUserData(Source) do
  begin
    Self.Tag := Tag;
  end;
end;

procedure TprPreviewUserData.SaveToStream;
begin
  Stream.Write(FTag, sizeof(FTag));
end;

procedure TprPreviewUserData.LoadFromStream;
begin
  Stream.Read(FTag, sizeof(FTag));
end;

/////////////////////
//
// TprObjRecVersion
//
/////////////////////
constructor TprObjRecVersion.Create;
begin
inherited;
FVisible:=true;
end;

procedure TprObjRecVersion.Assign;
begin
with Source as TprObjRecVersion do
  begin
    Self.Formula := Formula;
    Self.Visible := Visible;
    Self.FCompiledFormula := FCompiledFormula;
    Self.FMayBeUse := FMayBeUse;
    Self.FSecondPassCalcCurVersionNeeded := FSecondPassCalcCurVersionNeeded;
  end;
end;

///////////////////////
//
// TprObjRecVersions
//
///////////////////////
function TprObjRecVersions.GetItm;
begin
Result:=TprObjRecVersion(inherited Items[i]);
end;

/////////////////////////////////////////////////
//
// TprObjRec
//
/////////////////////////////////////////////////
constructor TprObjRec.Create(AContainer: IprReportContainer; _Obj: TprObj);
begin
  inherited Create;
{$IFDEF PG}
  FPage := _Page;
{$ENDIF}
  FContainer := AContainer;
  FObj := _Obj;
  DefVersion := 0;
  FCurVersion := -1;
  FVersions := CreateVersions;
end;

destructor TprObjRec.Destroy;
begin
  FVersions.Free;
  inherited;
end;

function TprObjRec.CreateVersions: TprObjRecVersions;
begin
  Result := TprObjRecVersions.Create(GetVersionClass);
end;

function TprObjRec.CreateCopy: TprObjRec;
begin
{$IFDEF PG}
  Result := TprObjRecClass(ClassType).Create(Self.Page, Self.Obj);
{$ELSE}
  Result := TprObjRecClass(ClassType).Create(FContainer, Self.Obj);
{$ENDIF}
  Result.Assign(Self);
end;

function TprObjRec.GetSupportSplitting: Boolean;
begin
  Result := False;
end;

function TprObjRec.GetCanSplitValue: Boolean;
begin
  Result := False;
end;

procedure TprObjRec.SetCanSplitValue(Value: Boolean);
begin
end;

function TprObjRec.GetCanSplit(AByHorizontal: Boolean; ASplitPos: Integer): Boolean;
begin
  Result := False;
end;

function TprObjRec.Split(AByHorizontal: Boolean; ASpitPos: Integer; var AAddToSplitted: Integer): TprObjRec;
begin
  Result := nil;
end;

procedure TprObjRec.Save;
var
  Writer: TWriter;
begin
  Stream.Write(FDefVersion, 4);
  WriteRect(Stream, pRect);
  Writer := TWriter.Create(Stream, 1024);
  try
    Writer.WriteCollection(FVersions);
  finally
    Writer.Free;
  end;
end;

procedure TprObjRec.Load;
var
  Reader: TReader;
begin
  Stream.Read(FDefVersion, 4);
  ReadRect(Stream,FpRect);
  Reader := TReader.Create(Stream, 1024);
  try
    Reader.ReadValue;
    Reader.ReadCollection(FVersions);
  finally
    Reader.Free;
  end;
end;

function TprObjRec.GetLeft;
begin
Result := FpRect.Left;
end;

function TprObjRec.GetTop;
begin
Result := FpRect.Top;
end;

function TprObjRec.GetRight;
begin
Result := FpRect.Right;
end;

function TprObjRec.GetBottom;
begin
Result := FpRect.Bottom;
end;

procedure TprObjRec.SetLeft;
begin
FpRect.Left := Value;
end;

procedure TprObjRec.SetTop;
begin
FpRect.Top := Value;
end;

procedure TprObjRec.SetRight;
begin
FpRect.Right := Value;
end;

procedure TprObjRec.SetBottom;
begin
FpRect.Bottom := Value;
end;

procedure TprObjRec.SetpRect(Value : TRect);
begin
  FpRect := Value;
end;

function TprObjRec.GetWidth: Integer;
begin
  Result := FpRect.Right - FpRect.Left;
end;

function TprObjRec.GetHeight: Integer;
begin
  Result := FpRect.Bottom - FpRect.Top;
end;

procedure TprObjRec.Assign;
begin
  with Source as TprObjRec do
  begin
    Self.FpRect := FpRect;
    Self.WidthAsVerticalBand := WidthAsVerticalBand;
    Self.HeightAsHorizontalBand := HeightAsHorizontalBand;  
    Self.DefVersion := DefVersion;
    Self.FCurVersion := CurVersion;
    Self.FSecondPassCalcCurVersionNeeded := FSecondPassCalcCurVersionNeeded;
    Self.FPreviewUserData := PreviewUserData;
    Self.CanSplit := CanSplit;
    Self.FVersions.Assign(Versions);
  end;
end;

procedure TprObjRec.FirstPass;
var
  i : integer;
  p : TprParser;
  Res : TprVarValue;
begin
  FCurVersion := DefVersion;
  for I := 0 to Versions.Count - 1 do
    if i <> DefVersion then
    begin
      p := TprParser(Obj.Band.Report.FParser);
      Versions[i].FCompiledFormula := Versions[i].Formula;

      Versions[i].FSecondPassCalcCurVersionNeeded := not p.Calc(Versions[i].FCompiledFormula,Res);
      if not Versions[i].FSecondPassCalcCurVersionNeeded then
        begin
          Versions[i].FMayBeUse := _vAsBoolean(Res);
          if Versions[i].FMayBeUse and (CurVersion=DefVersion) then
            FCurVersion := i;
        end;
    end;
end;

procedure TprObjRec.SecondPass;
var
  i : integer;
begin
FCurVersion := DefVersion;
for i:=0 to Versions.Count-1 do
  if i<>DefVersion then
    begin
      if Versions[i].FSecondPassCalcCurVersionNeeded then
        begin
          if Obj.Band.Report.Calc(Versions[i].FCompiledFormula) then
            begin
              FCurVersion := i;
              break;
            end;
        end
      else
        if Versions[i].FMayBeUse then
          begin
            FCurVersion := i;
            break;
          end;
    end;
end;

//////////////////
//
// TprObjs
//
//////////////////
function TprObjs.GetItm;
begin
Result:=TprObj(inherited Items[index]);
end;

/////////////////////////////////////////////////
//
// TprDesignComponent
//
/////////////////////////////////////////////////
function TprDesignComponent.DsgnAllowResizeTypes : TprResizeTypeSet;
begin
Result := [];
end;

function TprDesignComponent.DsgnAllowLinkTypes : TprLinkTypeSet;
begin
Result := [];
end;

function TprDesignComponent.DsgnAllowDrag : boolean;
begin
Result := false;
end;

function TprDesignComponent.DsgnAllowInplaceEdit : boolean;
begin
Result := false;
end;

function TprDesignComponent.DsgnAllowLinkWith(OtherObject : TprDesignComponent) : boolean; 
begin
Result := false;
end;

function TprDesignComponent.DsgnIsTransparent: Boolean;
begin
  Result := False;
end;

procedure TprDesignComponent.DsgnLink(Linked : TprDesignComponent; LinkType: TprLinkType; var LinkAccepted : boolean; ExData : pointer);
begin
end;

procedure TprDesignComponent.DsgnDrag(dx,dy : integer; var DragAccepted : boolean; ExData : pointer);
begin
end;

procedure TprDesignComponent.DsgnResize(oTop,oLeft,oBottom,oRight : integer; var ResizeAccepted : boolean; ExData : pointer);
begin
end;

procedure TprDesignComponent.DsgnDelete;
begin
  Free;
end;

procedure TprDesignComponent.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
end;

/////////////////////////////////////////////////
//
// TprObj
//
/////////////////////////////////////////////////
constructor TprObj.Create;
begin
inherited;

FTopObjs := TprObjs.Create;
FLeftObjs := TprObjs.Create;
FWidthObjs := TprObjs.Create;
FHeightObjs := TprObjs.Create;

FLeftMode := prlmMaxRight;
FTopMode := prlmMaxBottom;
FWidthMode := prrmMaxRight;
FHeightMode := prrmMaxBottom;

InitdRec;
end;

destructor TprObj.Destroy;
begin
FTopObjs.Free;
FLeftObjs.Free;
FWidthObjs.Free;
FHeightObjs.Free;
FdRec.Free;
FaRec.Free;
inherited;
end;

procedure TprObj.SetParentComponent;
begin
Band := Value as TprBand;
end;

function TprObj.GetChildOwner;
begin
Result := nil;
if Band<>nil then
  Result := Band.Report;
end;

function TprObj.HasParent;
begin
Result := true;
end;

function TprObj.GetParentComponent;
begin
Result := Band;
end;

procedure TprObj.Notification;
begin
if (Operation=opRemove) and (AComponent<>Self) and (AComponent is TprObj) then
  begin
    TopObjs.Remove(AComponent);
    LeftObjs.Remove(AComponent);
    WidthObjs.Remove(AComponent);
    HeightObjs.Remove(AComponent);
  end;
end;

procedure TprObj.DefineProperties;
begin
inherited;
Filer.DefineProperty('LeftObjs',ReadLeft,WriteLeft,LeftObjs.Count>0);
Filer.DefineProperty('TopObjs',ReadTop,WriteTop,TopObjs.Count>0);
Filer.DefineProperty('WidthObjs',ReadWidth,WriteWidth,WidthObjs.Count>0);
Filer.DefineProperty('HeightObjs',ReadHeight,WriteHeight,HeightObjs.Count>0);
Filer.DefineProperty('Visible', ReadVisible, nil, false);
end;

procedure TprObj.ReadLeft;
begin
FLeftObjsNames := TStringList.Create;
prReadStringList(Reader,FLeftObjsNames);
end;

procedure TprObj.WriteLeft;
begin
prWriteCompListNames(Writer,LeftObjs);
end;

procedure TprObj.ReadTop;
begin
FTopObjsNames := TStringList.Create;
prReadStringList(Reader,FTopObjsNames);
end;

procedure TprObj.WriteTop;
begin
prWriteCompListNames(Writer,TopObjs);
end;

procedure TprObj.ReadWidth;
begin
FWidthObjsNames := TStringList.Create;
prReadStringList(Reader,FWidthObjsNames);
end;

procedure TprObj.WriteWidth;
begin
prWriteCompListNames(Writer,WidthObjs);
end;

procedure TprObj.ReadHeight;
begin
FHeightObjsNames := TStringList.Create;
prReadStringList(Reader,FHeightObjsNames);
end;

procedure TprObj.ReadVisible(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TprObj.WriteHeight;
begin
prWriteCompListNames(Writer,HeightObjs);
end;

procedure TprObj.AfterReportLoaded;

  procedure Update(LO : TprObjs; LS : TStrings);
  var
    i : integer;
    c : TComponent;
  begin
  if LS<>nil then
    for i:=0 to LS.Count-1 do
      begin
        c := Band.Report.FindComponent(LS[i]);
        if (c<>nil) and (c is TprObj) and (Band=TprObj(c).Band) then
          LO.Add(c);
      end;
  end;

begin
try
  Update(LeftObjs,FLeftObjsNames);
  Update(TopObjs,FTopObjsNames);
  Update(WidthObjs,FWidthObjsNames);
  Update(HeightObjs,FHeightObjsNames);
finally
  FLeftObjsNames.Free;
  FTopObjsNames.Free;
  FWidthObjsNames.Free;
  FHeightObjsNames.Free;

  FLeftObjsNames := nil;
  FTopObjsNames := nil;
  FWidthObjsNames := nil;
  FHeightObjsNames := nil;
end;
end;

procedure TprObj.SetdRec(Value : TprObjRec);
begin
FdRec.Assign(Value);
end;

procedure TprObj.SetBand;
begin
if FBand=Value then exit;
if FBand<>nil then
  FBand.Objects.Remove(Self);
FBand:=Value;
if (FBand<>nil) and (FBand.Objects.IndexOf(Self)=-1) then
  FBand.Objects.Add(Self);
end;

function TprObj.GetReport;
begin
Result := nil;
if Band<>nil then
  Result := Band.Report;
end;

function TprObj.GetVersionCount: Integer;
begin
  if dRec <> nil then
    Result := dRec.FVersions.Count
  else
    Result := -1;
end;

function TprObj.GetGenVersionCount: Integer;
begin
  if aRec <> nil then
    Result := aRec.FVersions.Count
  else
    Result := -1;
end;

function TprObj.GetVersion(Index: Integer): TprObjRecVersion;
begin
  if dRec <> nil then
    Result := dRec.Versions[Index]
  else
    Result := nil;
end;

function TprObj.GetGenVersion(Index: Integer): TprObjRecVersion;
begin
  if aRec <> nil then
    Result := aRec.Versions[Index]
  else
    Result := nil;
end;

function TprObj.GetDefVersion: TprObjRecVersion;
begin
  if (dRec <> nil) and (dRec.DefVersion >= 0) and (dRec.DefVersion < dRec.Versions.Count) then
    Result := dRec.Versions[dRec.DefVersion]
  else
    Result := nil;
end;

function TprObj.GetGenCurVersion: TprObjRecVersion;
begin
  if (aRec <> nil) and (aRec.CurVersion >= 0) and (aRec.CurVersion < aRec.Versions.Count) then
    Result := aRec.Versions[aRec.CurVersion]
  else
    Result := nil;
end;

procedure TprObj.InplaceEdit;
begin
end;

procedure TprObj.SaveInplaceEdit;
begin
end;

function TprObj.GetDesc;
begin
  Result := Name;
end;

procedure TprObj.DsgnResize(oTop,oLeft,oBottom,oRight : integer; var ResizeAccepted : boolean; ExData : pointer);
begin
ResizeAccepted := (dRec.FpRect.Top+oTop<dRec.FpRect.Bottom+oBottom) and
                  (dRec.FpRect.Left+oLeft<dRec.FpRect.Right+oRight);
if not ResizeAccepted then exit;
dRec.FpRect.Top := dRec.FpRect.Top+oTop;
dRec.FpRect.Left := dRec.FpRect.Left+oLeft;
dRec.FpRect.Bottom := dRec.FpRect.Bottom+oBottom;
dRec.FpRect.Right := dRec.FpRect.Right+oRight;
end;

procedure TprObj.DsgnDrag(dx,dy : integer; var DragAccepted : boolean; ExData : pointer);
begin
dRec.FpRect.Top := dRec.FpRect.Top+dy;
dRec.FpRect.Left := dRec.FpRect.Left+dx;
dRec.FpRect.Bottom := dRec.FpRect.Bottom+dy;
dRec.FpRect.Right := dRec.FpRect.Right+dx;
DragAccepted := true;
end;

procedure TprObj.DsgnLink(Linked : TprDesignComponent; LinkType: TprLinkType; var LinkAccepted : boolean; ExData : pointer);
begin
if (Linked is TprObj) and (Band=TprObj(Linked).Band) then
  begin
    case LinkType of
      ltLeft : if LeftObjs.IndexOf(Linked)=-1 then
                 begin
                   LeftObjs.Add(Linked);
                   LinkAccepted := true;
                 end;
      ltTop : if TopObjs.IndexOf(Linked)=-1 then
                 begin
                   TopObjs.Add(Linked);
                   LinkAccepted := true;
                 end;
      ltRight : if WidthObjs.IndexOf(Linked)=-1 then
                  begin
                    WidthObjs.Add(Linked);
                    LinkAccepted := true;
                  end;
      ltBottom: if HeightObjs.IndexOf(Linked)=-1 then
                  begin
                    HeightObjs.Add(Linked);
                    LinkAccepted := true;
                  end;
    end;
  end;
end;

function TprObj.DsgnAllowDrag : boolean;
begin
  Result := true;
end;

function TprObj.DsgnAllowResizeTypes : TprResizeTypeSet;
begin
Result := [ppLeftTop,ppTop,ppRightTop,ppRight,ppRightBottom,ppBottom,ppLeftBottom,ppLeft];
end;

function TprObj.DsgnAllowLinkTypes : TprLinkTypeSet;
begin
Result := [ltLeft,ltTop,ltRight,ltBottom];
end;

function TprObj.DsgnAllowLinkWith(OtherObject : TprDesignComponent) : boolean;
begin
Result := (OtherObject<>Self) and (OtherObject is TprObj) and (TprObj(OtherObject).Band=Self.Band);
end;

procedure TprObj.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1 : DefVersion.Visible := not DefVersion.Visible;
  -2 : dRec.WidthAsVerticalBand := not dRec.WidthAsVerticalBand;
  -3 : dRec.HeightAsHorizontalBand := not dRec.HeightAsHorizontalBand;
end;
DsgnNotifyDesigner;
end;

procedure TprObj.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
AddPopupMenuItem(Popup,nil,sVisible,'',OnDsgnPopupMenuClick,'',-1,true,DefVersion.Visible);
AddPopupMenuItem(Popup,nil,sWidthAsVerticalBand,'',OnDsgnPopupMenuClick,'',-2,true,dRec.WidthAsVerticalBand);
AddPopupMenuItem(Popup,nil,sHeightAsHorizontalBand,'',OnDsgnPopupMenuClick,'',-3,true,dRec.HeightAsHorizontalBand);
end;

procedure TprObj.DsgnNotifyDesigner;
begin
  Band.Report.DsgnNotify(Self);
end;

procedure TprObj.DoOnFirstPassObject(var ManuallyProcessed : boolean);
begin
  Band.Report.DoOnFirstPassObject(Self,ManuallyProcessed);
end;

procedure TprObj.FirstPass;
var
  I, ADelta: Integer;
  AObj, MinBottomObj, MaxBottomObj, MinRightObj, MaxRightObj,
  MinHeightObj, MaxHeightObj, MinWidthObj, MaxWidthObj: TprObj;

  function IsObjLinkedToSelf(Obj: TprObj; LinkType: TprLinkType): Boolean;
  begin
    Result := false;
    case LinkType of
      ltLeft: Result := Obj.LeftObjs.IndexOf(Self) <> -1;
      ltTop: Result := Obj.TopObjs.IndexOf(Self) <> -1;
      ltRight: Result := Obj.WidthObjs.IndexOf(Self) <> -1;
      ltBottom: Result := Obj.HeightObjs.IndexOf(Self) <> -1;
    end;
  end;

  procedure CalcObjs(o: TprObjs; LinkType: TprLinkType);
  var
    I: Integer;
    fCrlLink: Boolean;
    AObj: TprObj;
  begin
    if not o[0].FirstPassProcessed then
      o[0].FirstPass;
    
    MinBottomObj := o[0];
    MaxBottomObj := o[0];
    MinRightObj := o[0];
    MaxRightObj := o[0];
    MinHeightObj := o[0];
    MaxHeightObj := o[0];
    MinWidthObj := o[0];
    MaxWidthObj := o[0];
    fCrlLink := IsObjLinkedToSelf(o[0], LinkType);
    for I := 1 to o.Count - 1 do
    begin
      AObj := o[I];
      if not AObj.FirstPassProcessed then
        AObj.FirstPass;

      fCrlLink := fCrlLink or IsObjLinkedToSelf(AObj, LinkType);

      if AObj.aRec.FpRect.Bottom > MaxBottomObj.aRec.FpRect.Bottom then
        MaxBottomObj := AObj;
      if AObj.aRec.FpRect.Bottom < MinBottomObj.aRec.FpRect.Bottom then
        MinBottomObj := AObj;
      if AObj.aRec.FpRect.Right > MaxRightObj.aRec.FpRect.Right then
        MaxRightObj := AObj;
      if AObj.aRec.FpRect.Right < MinRightObj.aRec.FpRect.Right then
        MinRightObj := AObj;
        
      if AObj.aRec.Width > MaxWidthObj.aRec.Width then
        MaxWidthObj := AObj;
      if AObj.aRec.Width < MinWidthObj.aRec.Width then
        MinWidthObj := AObj;
      if AObj.aRec.Height > MaxHeightObj.aRec.Height then
        MaxHeightObj := AObj;
      if AObj.aRec.Height < MinHeightObj.aRec.Height then
        MinHeightObj := AObj;
    end;

    if fCrlLink then
    begin
      if Self.aRec.FpRect.Bottom > MaxBottomObj.aRec.FpRect.Bottom then
        MaxBottomObj := Self;
      if Self.aRec.FpRect.Bottom < MinBottomObj.aRec.FpRect.Bottom then
        MinBottomObj := Self;
      if Self.aRec.FpRect.Right > MaxRightObj.aRec.FpRect.Right then
        MaxRightObj := Self;
      if Self.aRec.FpRect.Right < MinRightObj.aRec.FpRect.Right then
        MinRightObj := Self;

      if Self.aRec.Width > MaxWidthObj.aRec.Width then
        MaxWidthObj := Self;
      if Self.aRec.Width < MinWidthObj.aRec.Width then
        MinWidthObj := Self;
      if Self.aRec.Height > MaxHeightObj.aRec.Height then
        MaxHeightObj := Self;
      if Self.aRec.Height < MinHeightObj.aRec.Height then
        MinHeightObj := Self;
    end;
  end;

begin
  if aRec = nil then
    raise Exception.Create(prLoadStr(saRecNotInitializated));
  FirstPassProcessed := true;

  aRec.FpRect.Left := dRec.pRect.Left;
  aRec.FpRect.Top := dRec.pRect.Top;

  // TopObjs
  if TopObjs.Count > 0 then
  begin
    CalcObjs(TopObjs, ltTop);

    case TopMode of
      prlmMaxBottom:
        begin
          if MaxBottomObj <> Self then
            OffsetRect(aRec.FpRect, 0, MaxBottomObj.aRec.FpRect.Bottom - MaxBottomObj.dRec.FpRect.Bottom)
        end;
      prlmMinBottom:
        begin
          if MinBottomObj <> Self then
            OffsetRect(aRec.FpRect, 0, MinBottomObj.aRec.FpRect.Bottom - MinBottomObj.dRec.pRect.Bottom)
        end;
    end;
  end;

  // LeftObjs
  if LeftObjs.Count > 0 then
  begin
    CalcObjs(LeftObjs, ltLeft);

    case LeftMode of
      prlmMaxRight:
        begin
          if MaxRightObj <> Self then
            OffsetRect(aRec.FpRect, MaxRightObj.aRec.FpRect.Right - MaxRightObj.dRec.FpRect.Right, 0);
        end;
      prlmMinRight: 
        begin
          if MinRightObj <> Self then
            OffsetRect(aRec.FpRect, MinRightObj.aRec.FpRect.Right - MinRightObj.dRec.FpRect.Right, 0);
        end;
    end;
  end;

  // WidthObjs
  if WidthObjs.Count > 0 then
  begin
    if WidthMode in [prrmMaxWidth, prrmMinWidth, prrmMaxRight, prrmMinRight] then
    begin
      CalcObjs(WidthObjs, ltRight);

      case WidthMode of
        prrmMaxWidth:
          begin
            if MaxWidthObj <> Self then
              aRec.FpRect.Right := aRec.FpRect.Left +
                                   dRec.Width +
                                   MaxWidthObj.aRec.Width -
                                   MaxRightObj.dRec.Width;
          end;
        prrmMinWidth:
          begin
            if MinWidthObj <> Self then
              aRec.FpRect.Right := aRec.FpRect.Left +
                                   dRec.Width +
                                   MinWidthObj.aRec.Width -
                                   MinWidthObj.dRec.Width;
          end;
        prrmMaxRight:
          begin
            if MaxRightObj <> Self then
              aRec.FpRect.Right := aRec.FpRect.Left +
                                   dRec.Width +
                                   MaxRightObj.aRec.pRect.Right -
                                   MaxRightObj.dRec.pRect.Right;
          end;
        prrmMinRight:
          begin
            if MinRightObj <> Self then
              aRec.FpRect.Right := aRec.FpRect.Left +
                                   dRec.Width +
                                   MinRightObj.aRec.pRect.Right -
                                   MinRightObj.dRec.pRect.Right;
          end;
      end;
    end
    else
    begin
      if WidthMode = prrmWidthSum then
      begin
        ADelta := 0;
        for I := 0 to WidthObjs.Count - 1 do
        begin
          AObj := WidthObjs[I];
          if AObj <> Self then
          begin
            if not AObj.FirstPassProcessed then
              AObj.FirstPass;
            ADelta := ADelta + AObj.aRec.Width - AObj.dRec.Width; 
          end;
        end;
        ARec.FpRect.Right := ARec.FpRect.Left + dRec.Width + ADelta;
      end;
    end;
  end;


  // HeightObjs
  if HeightObjs.Count>0 then
  begin
    if HeightMode in [prrmMaxHeight, prrmMinHeight, prrmMaxBottom, prrmMinBottom] then
    begin
      CalcObjs(HeightObjs, ltBottom);

      case HeightMode of
        prrmMaxHeight:
          begin
            if MaxHeightObj <> Self then
              aRec.FpRect.Bottom := aRec.FpRect.Top +
                                    dRec.Height +
                                    MaxHeightObj.aRec.Height -
                                    MaxHeightObj.dRec.Height;
          end;
        prrmMinHeight:
          begin
            if MinHeightObj <> Self then
              aRec.FpRect.Bottom := aRec.FpRect.Top +
                                    dRec.Height +
                                    MinHeightObj.aRec.Height -
                                    MinHeightObj.dRec.Height;
          end;
        prrmMaxBottom:
          begin
            if MaxBottomObj <> Self then
              aRec.FpRect.Bottom := aRec.FpRect.Top +
                                    dRec.Height +
                                    MaxBottomObj.aRec.FpRect.Bottom -
                                    MaxBottomObj.dRec.pRect.Bottom;
          end;
        prrmMinBottom:
          begin
            if MinBottomObj <> Self then
              aRec.FpRect.Bottom := aRec.FpRect.Top +
                                    dRec.Height +
                                    MinBottomObj.aRec.FpRect.Bottom -
                                    MinBottomObj.dRec.pRect.Bottom;
          end;
      end;
    end
    else
    begin
      if HeightMode = prrmHeightSum then
      begin
        ADelta := 0;
        for I := 0 to HeightObjs.Count - 1 do
        begin
          AObj := HeightObjs[I];
          if AObj <> Self then
          begin
            if not AObj.FirstPassProcessed then
              AObj.FirstPass;
            ADelta := ADelta + AObj.aRec.Height - AObj.dRec.Height;
          end;
        end;
        ARec.FpRect.Bottom := ARec.FpRect.Top + dRec.Height + ADelta;
      end;
    end;
  end;
  Report.DoOnPreviewGetUserData(Self, aRec, aRec.FPreviewUserData);
end;

procedure TprObj.DrawDesign;
begin
end;

/////////////////////////////////////////////////
//
// TprValueVersion
//
/////////////////////////////////////////////////

/////////////////////////////////////////////////
//
// TprValue
//
/////////////////////////////////////////////////
constructor TprValue.Create;
var
  I: integer;
  cn: string;
begin
  inherited;
  FDataSet := TprDatasetLink.Create;
  FResetDataSet := TprDatasetLink.Create;
  FCrossTabHorzDataSet := TprDatasetLink.Create;
  FVersions := TList.Create;
  FSavedValues := TList.Create;

  if Report <> nil then
  begin
    I := 1;
    cn := ClassName;
    while Report.Values.IndexByName(Copy(cn, 2, Length(cn)) + IntToStr(I)) <> -1 do
      Inc(i);
    Name := Copy(cn, 2, Length(cn)) + IntToStr(I);
  end;
end;

destructor TprValue.Destroy;
begin
  Clear;
  FDataset.Free;
  FResetDataset.Free;
  FCrossTabHorzDataset.Free;
  FSavedValues.Free;
  FVersions.Free;
  inherited;
end;

function TprValue.GetValue: Variant;
begin
  if FVersions.Count > 0 then
    Result := Versions[FVersions.Count - 1].VersionValue
  else
    Result := Null;
end;

procedure TprValue.Assign(Source: TPersistent);
var
  I: Integer;
  AValueVersion: TprValueVersion;
  ASavedValue: TprSavedValue;
begin
  if Source is TprValue then
    with TprValue(Source) do
    begin
      Self.FGroup := Group;
      Self.FName := Name;
      Self.FAggFunction := AggFunction;
      Self.FFormula := Formula;
      Self.FResetOn := ResetOn;
      Self.FCalcOn := CalcOn;
      Self.FDataSetName := DataSetName;
      Self.FResetDataSetName := ResetDataSetName;
      Self.FCrossTabHorzDataSetName := CrossTabHorzDataSetName;
      Self.FAccumulate := Accumulate;

      // versions
      FreeListItems(Self.FVersions);
      Self.FVersions.Clear;
      for I := 0 to FVersions.Count - 1 do
      begin
        AValueVersion := TprValueVersion.Create;
        with TprValueVersion(FVersions[I]) do
        begin
          AValueVersion.FNotUseInAccumulate := FNotUseInAccumulate;
          AValueVersion.V1 := V1;
          AValueVersion.V2 := V2;
          AValueVersion.Value := Self;
          AValueVersion.ID := ID;
          AValueVersion.V := V;
        end;
        Self.FVersions.Add(AValueVersion)
      end;

      // saved values
      FreeListItems(Self.FSavedValues);
      Self.FSavedValues.Clear;
      for I := 0 to FSavedValues.Count - 1 do
      begin
        ASavedValue := TprSavedValue.Create;
        with TprSavedValue(FSavedValues[I]) do
        begin
          ASavedValue.V := V;
          ASavedValue.Cell := nil;
        end;
        Self.FSavedValues.Add(ASavedValue);
      end;
    end;
end;

function TprValue.GetReport: TprCustomReport;
begin
  Result := TprValues(Collection).Report;
end;

procedure TprValue.SetName;
begin
if CompText(Value,Name)=0 then exit;
if Report.Values.IndexByName(Value)<>-1 then
  raise Exception.CreateFmt(prLoadStr(sVarAlreadyExists),[Value,Report.Name]);
FName := Value;
end;

procedure TprValue.SetFormula;

  function CheckFunction(const FuncName : string; AggFunction : TprAggFunction) : boolean;
  var
    b : string;
  begin
  Result := false;
  if AnsiCompareText(Copy(Value,1,Length(FuncName)),FuncName)<>0 then exit;
  b := Trim(Copy(Value,Length(FuncName)+1,Length(Value)));
  FFormula := Copy(b,2,Length(b)-2);
  FAggFunction := AggFunction;
  Result := true;
  end;

begin
Value := Trim(Value);
if CheckFunction('AggSum',prafSum) then exit;
if CheckFunction('AggCount',prafCount) then exit;
if CheckFunction('AggAvg',prafAvg) then exit;
if CheckFunction('AggMin',prafMin) then exit;
if CheckFunction('AggMax',prafMax) then exit;
FFormula := Value;
end;

function TprValue.GetVersion;
begin
Result := TprValueVersion(FVersions[index]);
end;

function TprValue.VersionsCount;
begin
Result := FVersions.Count;
end;

function TprValue.GetCurrentVersionID;
begin
if CalcOn=cvtCrossTab then
  begin
    Result := Format('%s_%d',[Name,Report.GetDataSetRecNo(CrossTabHorzDataSet.Dataset)+CrossTabSavedIndex-1])
  end
else
  begin
    if FCurrentValueExists then
      Result := Format('%s_%d',[Name,FVersions.Count-1])
    else
      Result := Format('%s_%d',[Name,FVersions.Count])
  end;
end;

procedure TprValue.SetCurrentValue;
var
  ver : TprValueVersion;
begin
if CalcOn=cvtCrossTab then
  begin
    if Report.GetDataSetRecNo(CrossTabHorzDataSet)+CrossTabSavedIndex-1<VersionsCount then
      Versions[Report.GetDataSetRecNo(CrossTabHorzDataSet)+CrossTabSavedIndex-1].V := Value // ???
    else
      begin   // ???
        ver := TprValueVersion.Create;
        ver.Value := Self;
        ver.ID := FVersions.Count;
        ver.V := Value;
        FVersions.Add(ver);
      end;
  end
else
  begin
    if FCurrentValueExists then
      Versions[FVersions.Count-1].V := Value
    else
      begin   // ??? 
        ver :=TprValueVersion.Create;
        ver.Value := Self;
        ver.ID := FVersions.Count;
        ver.V := Value;
        FVersions.Add(ver);
        FCurrentValueExists := true;
      end;
  end;
end;

function TprValue.GetCurrentVersion : TprValueVersion;
var
  fCreated : boolean;
begin
Result := GetCurrentVersion(fCreated);
end;

function TprValue.GetCurrentVersion(var fCreated : boolean) : TprValueVersion;
begin
fCreated := false;
if CalcOn=cvtCrossTab then
  begin
    if Report.GetDataSetRecNo(CrossTabHorzDataSet.Dataset)+CrossTabSavedIndex-1<VersionsCount then
      Result := Versions[Report.GetDataSetRecNo(CrossTabHorzDataSet.Dataset)+CrossTabSavedIndex-1]
    else
      begin
        fCreated := true;
        Result := TprValueVersion.Create;
        if Accumulate and (CrossTabColsCount>0) then
          with TprValueVersion(FVersions[FVersions.Count-CrossTabColsCount]) do
            begin
              Result.V1 := V1;
              Result.V2 := V2;
              Result.V := V;
            end;
        FVersions.Add(Result);
      end;
  end
else
  begin
    if FCurrentValueExists then
      Result := Versions[FVersions.Count-1]
    else
      begin
        fCreated := true;
        Result := TprValueVersion.Create;
        if Accumulate and (FVersions.Count>0) and not TprValueVersion(FVersions[FVersions.Count-1]).FNotUseInAccumulate then
          with TprValueVersion(FVersions[FVersions.Count-1]) do
            begin
              Result.V1 := V1;
              Result.V2 := V2;
              Result.V := V;
            end;
        FVersions.Add(Result);
        FCurrentValueExists := true;
      end;
  end;
end;

procedure TprValue.Reset;
var
  v : TprValueVersion;
  fCreated : boolean;
begin
v := GetCurrentVersion(fCreated);
if fCreated then
  begin
    v.V := UnAssigned;
    v.V1 := UnAssigned;
  end;
case CalcOn of
  cvtEventOnReset:
    begin
      if Assigned(OnCalc) then
        OnCalc(Self)
      else
        raise Exception.CreateFmt(prLoadStr(sReportVarNotCalced),[Name]);
    end;
  cvtCrossTab:
    begin
      CrossTabSavedIndex := VersionsCount;
      if CrossTabColsCount=0 then
        CrossTabColsCount := VersionsCount;
    end
  else
    begin
      FCurrentValueExists := false;
    end
end;
end;

procedure TprValue.Init;
  procedure GetDataSet(DataSet : TprDatasetLink; DataSetName : string);
  begin
  Dataset.Dataset:=Report.GetDataSetByName(DataSetName);
  if DataSet.Dataset=nil then
    raise Exception.CreateFmt(prLoadStr(sDataSetNotFound),[DataSetName]);
  end;
begin
if CalcOn in [cvtDataSetNext,cvtCrossTab] then
  GetDataSet(DataSet,DataSetName);
if ResetOn in [rvtDataSetEof] then
  GetDataSet(ResetDataSet,ResetDataSetName);
if CalcOn  in [cvtCrossTab] then
  GetDataSet(CrossTabHorzDataSet,CrossTabHorzDataSetName);
end;

procedure TprValue.Clear;
begin
FCurrentValueExists := false;
CrossTabSavedIndex := 0;
CrossTabColsCount := 0;
FreeListItems(FVersions);
FVersions.Clear;
FreeListItems(FSavedValues);
FSavedValues.Clear;
end;

function TprValue.AddSavedValue;
begin
Result := TprSavedValue.Create;
FSavedValues.Add(Result);
end;

procedure TprValue.InternalCalcValue(ver : TprValueVersion; v : TprVarValue);
begin
case AggFunction of
  prafSum:
    if v.vType in [prvvtInteger,prvvtDouble] then
      begin
        if VarIsEmpty(ver.V) then
          ver.V := _vAsVariant(v)
        else
          ver.V := ver.V+_vAsVariant(v);
      end;
  prafCount:
    begin
      if VarIsEmpty(ver.V) then
        ver.V := 1
      else
        ver.V := ver.V+1;
    end;
  prafAvg:
    if v.vType in [prvvtInteger,prvvtDouble] then
      begin
        if VarIsEmpty(ver.V) then
          begin
            ver.V1 := _vAsVariant(v);
            ver.V2 := 1;
            ver.V := _vAsVariant(v);
          end
        else
          begin
            ver.V1 := ver.V1+_vAsVariant(v);
            ver.V2 := ver.V2+1;
            ver.V := ver.V1 / ver.V2;
          end;
      end;
  prafMin:
    if v.vType in [prvvtInteger,prvvtDouble,prvvtDateTime,prvvtString] then
      begin
        if VarIsEmpty(ver.V) then
          ver.V := _vAsVariant(v)
        else
          if ver.V>_vAsVariant(v) then
            ver.V := _vAsVariant(v);
      end;
  prafMax:
    if v.vType in [prvvtInteger,prvvtDouble,prvvtDateTime,prvvtString] then
      begin
        if VarIsEmpty(ver.V) then
          ver.V := _vAsVariant(v)
        else
          if ver.V<_vAsVariant(v) then
            ver.V := _vAsVariant(v);
      end;
end;
end;

procedure TprValue.Calculate(Cell : TprGenCell);
var
  s : string;
  v : TprVarValue;
  sv : TprSavedValue;
begin
if FFormula<>'' then
  begin
    s := FFormula;
    if not TprParser(Report.Parser).Calc(s,v) then
      raise Exception.Create(prLoadStr(sErrorCalcExpressionInOnePass));
  end
else
  _vSetNull(v);
if ResetOn=rvtPage then
  begin
    sv := AddSavedValue;
    sv.Cell := Cell;
    _vCopy(v,sv.v);
  end
else
  InternalCalcValue(GetCurrentVersion,v);
end;

function TprValue.VersionByVersionID(ID : integer) : TprValueVersion;
var
  i : integer;
  sv : TprSavedValue;
begin
if ID<VersionsCount then
  begin
    Result := Versions[ID];
    if ResetOn=rvtPage then
      begin
        // base on CurEndPage, FSavedValues calculate value
        Result.V := UnAssigned;
        Result.V1 := UnAssigned;
        if Accumulate then
          for i:=0 to FSavedValues.Count-1 do
            begin
              sv := TprSavedValue(FSavedValues[i]);
              if (sv.Cell<>nil) and (sv.Cell.Page<>nil) and (Report.EndPageIndex(sv.Cell.Page)<=Report.IndexCurEndPage) then
                InternalCalcValue(Result,sv.V);
            end
        else
          for i:=0 to FSavedValues.Count-1 do
            begin
              sv := TprSavedValue(FSavedValues[i]);
              if (sv.Cell<>nil) and (sv.Cell.Page<>nil) and (sv.Cell.Page=Report.CurEndPage) then
                InternalCalcValue(Result,sv.V);
            end;
      end;
    if Assigned(OnGetVersionByVersionID) then
      OnGetVersionByVersionID(Result);
  end
else
  raise Exception.CreateFmt(prLoadStr(sNoVarVersion),[Name,ID]);
end;

/////////////////////////////////////////////////
//
// TprValues
//
/////////////////////////////////////////////////
function TprValues.GetItm;
begin
Result := TprValue(inherited Items[index]);
end;

function TprValues.GetByName;
var
  i : integer;
begin
i := IndexByName(Name);
if i=-1 then
  raise Exception.CreateFmt(prLoadStr(sVarNotExists),[Name])
else
  Result := Items[i];
end;

function TprValues.Add : TprValue;
begin
Result := TprValue(inherited Add);
end;

function TprValues.IndexByName;
begin
  Result := 0;
  while (Result < Count) and
        (CompText(Items[Result].Name, Name) <> 0) do Inc(Result);
  if Result >= Count then
    Result := -1;
end;

function TprValues.VersionByVersionId;
var
  name : string;
  p,ind : integer;
begin
Result := nil;
p := PosLastChar(id,'_');
if p<>0 then
  begin
    ind := StrToIntDef(Copy(id,p+1,length(id)),-1);
    if Ind<>-1 then
      begin
        name := Copy(id,1,p-1);
        p := IndexByName(name);
        if p<>-1 then
          Result := Items[p].VersionByVersionID(ind)
        else
          raise Exception.CreateFmt(prLoadStr(sNoVarVersion),[Name,ind]);
      end;
  end;
end;

/////////////////////////////////////////////////
//
// TprVariable
//
/////////////////////////////////////////////////
constructor TprVariable.Create(Collection: TCollection);
begin
inherited;
IsNull := true;
end;

procedure TprVariable.Assign(Source : TPersistent);
begin
with TprVariable(Source) do
  begin
    Self.FCalculated := Calculated;
    _vCopy(VarValue^,Self.FValue);
  end;
end;

procedure TprVariable.ReadValue(Reader : TReader);
begin
if FCalculated then
  Formula := Reader.ReadString
else
  case FValue.vType of
    prvvtString: FValue.vString := Reader.ReadString;
    prvvtInteger: FValue.vInteger := Reader.ReadInteger;
    prvvtDouble: FValue.vDouble := Reader.ReadFloat;
    prvvtDateTime: FValue.vDateTime := Reader.ReadDate;
    prvvtNull: Reader.ReadString;
  end;
end;

procedure TprVariable.WriteValue(Writer : TWriter);
begin
if FCalculated then
  Writer.WriteString(FValue.vString)
else
  case FValue.vType of
    prvvtString: Writer.WriteString(FValue.vString);
    prvvtInteger: Writer.WriteInteger(FValue.vInteger);
    prvvtDouble: Writer.WriteFloat(FValue.vDouble);
    prvvtDateTime: Writer.WriteDate(FValue.vDateTime);
    prvvtNull: Writer.WriteString('');
  end;
end;

procedure TprVariable.ReadValueType(Reader : TReader);
var
  s : string;
begin
s := Reader.ReadString;
FCalculated := s='formula';
if not FCalculated then
  FValue.vType := TprVarValueType(GetEnumValue(TypeInfo(TprVarValueType),s));
end;

procedure TprVariable.WriteValueType(Writer : TWriter);
begin
if FCalculated then
  Writer.WriteString('formula')
else
  Writer.WriteString(GetEnumName(TypeInfo(TprVarValueType),integer(FValue.vType)));
end;

procedure TprVariable.DefineProperties(Filer : TFiler);
begin
inherited;
Filer.DefineProperty('ValueType',ReadValueType,WriteValueType,not IsNull);
Filer.DefineProperty('Value',ReadValue,WriteValue,not IsNull);
end;

function TprVariable.GetAsString : string;
begin
Result := _vAsString(FValue);
end;

function TprVariable.GetAsInteger : integer;
begin
Result := _vAsInteger(FValue);
end;

function TprVariable.GetAsDouble : double;
begin
Result := _vAsDouble(FValue);
end;

function TprVariable.GetAsDateTime : TDateTime;
begin
Result := _vAsDateTime(FValue);
end;

function TprVariable.GetAsVariant : Variant;
begin
Result := _vAsVariant(FValue);
end;

function TprVariable.GetIsNull : boolean;
begin
Result := _vIsNull(FValue);
end;

function TprVariable.GetFormula : string;
begin
if FCalculated then
  Result := FValue.vString
else
  Result := '';
end;

procedure TprVariable.SetAsString(Value : string);
begin
_vSetAsString(FValue,Value);
FCalculated := false;
end;

procedure TprVariable.SetAsInteger(Value : integer);
begin
_vSetAsInteger(FValue,Value);
FCalculated := false;
end;

procedure TprVariable.SetAsDouble(Value : double);
begin
_vSetAsDouble(FValue,Value);
FCalculated := false;
end;

procedure TprVariable.SetAsDateTime(Value : TDateTime);
begin
_vSetAsDateTime(FValue,Value);
FCalculated := false;
end;

procedure TprVariable.SetAsVariant(Value : Variant);
begin
_vSetAsVariant(FValue,Value);
FCalculated := false;
end;

procedure TprVariable.SetIsNull(Value : boolean);
begin
if Value then
  _vSetNull(FValue)
else
  _vSetAsString(FValue,'');
FCalculated := false;
end;

procedure TprVariable.SetFormula(Value : string);
begin
FCalculated := true;
_vSetAsString(FValue,Value);
end;

function TprVariable.GetVarValue : PprVarValue;
begin
Result := @FValue;
end;

/////////////////////////////////////////////////
//
// TprVariables
//
/////////////////////////////////////////////////
function TprVariables.GetItm(i : integer) : TprVariable;
begin
  Result := TprVariable(inherited Items[i]);
end;

function TprVariables.GetByName(Name : string) : TprVariable;
var
  i : integer;
begin
  i := IndexByName(Name);
  if i=-1 then
    raise Exception.CreateFmt(prLoadStr(sSimpleVarNotExists),[Name])
  else
    Result := Items[i];
end;

function TprVariables.IndexByName(const Name : string) : integer;
begin
  Result := 0;
  while (Result<Count) and (AnsiCompareText(Items[Result].Name,Name)<>0) do Inc(Result);
  if Result>=Count then
    Result := -1;
end;

function TprVariables.AddVariable : TprVariable;
begin
  Result := TprVariable(inherited Add);
end;

function TprVariables.AddVariable(const AVariableName: string; const AVariableValue: Variant): TprVariable;
var
  I: Integer;
begin
  I := IndexByName(AVariableName);
  if I = -1 then
  begin
    Result := AddVariable;
    Result.Name := AVariableName;
  end
  else
    Result := Items[I];
  Result.AsVariant := AVariableValue;
end;

/////////////////////////////////////////////////
//
// TprGroup
//
/////////////////////////////////////////////////
constructor TprGroup.Create;
begin
  inherited;
  FHeaders := TprBands.Create;
  FFooters := TprBands.Create;
  FPrevValue := Unassigned;
  FPrevGroupValue := Unassigned;
  FGroupState := prgsHeaders;
end;

destructor TprGroup.Destroy;
begin
  FHeaders.Free;
  FHeaders := nil;
  FFooters.Free;
  FFooters := nil;
  Report := nil;
  inherited;
end;

procedure TprGroup.Notification;
begin
  inherited;
  if AOperation = opRemove then
  begin
    if AComponent = FDetailBand then
      FDetailBand := nil;
    if Headers <> nil then
      Headers.Remove(AComponent);
    if Footers <> nil then
      Footers.Remove(AComponent);
  end;
end;

function TprGroup.GetChildOwner;
begin
  Result := Report;
end;

procedure TprGroup.SetParentComponent;
begin
  Report := Value as TprCustomReport;
end;

function TprGroup.HasParent;
begin
  Result := true;
end;

function TprGroup.GetParentComponent;
begin
  Result := Report;
end;

procedure TprGroup.SetReport;
begin
  if FReport <> Value then
  begin
    if FReport <> nil then
      FReport.Groups.Remove(Self);

    FReport := Value;

    if (FReport <> nil) and (FReport.Groups.IndexOf(Self) = -1) then
      FReport.Groups.Add(Self);
  end;
end;

procedure TprGroup.SetDetailBand;
begin
  if Value <> FDetailBand then
  begin
    if FDetailBand <> nil then
      FDetailBand.GroupRemoved(Self);

    FDetailBand := Value;
    
    if FDetailBand <> nil then
      FDetailBand.GroupAdded(Self);
  end;
end;

function TprGroup.GetGroupValue: Variant;
begin
  case FGroupState of
    prgsHeaders: Result := FPrevValue;
    prgsFooters: Result := FPrevGroupValue;
    else Result := FPrevValue;
  end;
end;

function TprGroup.IndexInReport;
begin
  Result := Report.Groups.IndexOf(Self);
end;

procedure TprGroup.Reset;
begin
  FLineNo := 0;
  FPrevValue := Unassigned;
  FPrevGroupValue := Unassigned;
  FGroupState := prgsHeaders;
end;

procedure TprGroup.CalcValue;
var
  CurValue: Variant;
begin
  FLineNo := FLineNo + 1;
  CurValue := Report.Calc(FValid);
  FNeedHeaders := VarIsEmpty(FPrevValue) or (FPrevValue <> CurValue);
  FNeedFooters := not VarIsEmpty(FPrevValue) and (FPrevValue <> CurValue);

  FPrevGroupValue := FPrevValue;
  FPrevValue := CurValue;
end;

procedure TprGroup.HeadersGenerateCell(CallerBand : TprBand; CallbackProc : TGenerateCellCallbackProc);
var
  I: Integer;
begin
  FGroupState := prgsHeaders;
  if FNeedHeaders then
  begin
    FLineNo := 1;
    DetailBand.BeginSection;
    for I := 0 to Headers.Count - 1 do
      Headers[I].GenerateCell(CallerBand, CallbackProc);
  end;
end;

procedure TprGroup.FootersGenerateCell(CallerBand: TprBand; CallbackProc: TGenerateCellCallbackProc);
var
  I: integer;
begin
  FGroupState := prgsFooters;
  if FNeedFooters then
  begin
    for I := 0 to Footers.Count - 1 do
      Footers[I].GenerateCell(CallerBand, CallbackProc);
    DetailBand.EndSection;
    
    Report.DoOnGroupEnd(Self);
  end;
end;

procedure TprGroup.FootersAlwaysGenerateCell(CallerBand: TprBand; CallbackProc: TGenerateCellCallbackProc);
var
  I: integer;
begin
  FGroupState := prgsFootersAlways;
  for I := 0 to Footers.Count - 1 do
    Footers[I].GenerateCell(CallerBand, CallbackProc);
  DetailBand.EndSection;

  Report.DoOnGroupEnd(Self);
end;

/////////////////////////////////////////////////
//
// TprGenCell
//
/////////////////////////////////////////////////
constructor TprGenCell.Create;
begin
  inherited;
  FObjRecs := TList.Create;
end;

destructor TprGenCell.Destroy;
begin
  FreeListItems(FObjRecs);
  FObjRecs.Free;
  inherited;
end;

procedure TprGenCell.Add(ObjRec : TprObjRec);
begin
FObjRecs.Add(ObjRec);
end;

function TprGenCell.GetObjRec(i : integer) : TprObjRec;
begin
Result := TprObjRec(FObjRecs[i]);
end;

function TprGenCell.GetObjRecsCount : integer;
begin
  Result := FObjRecs.Count;
end;

procedure TprGenCell.SetCellHeight(NewHeight : integer);
begin
  FHeight := NewHeight;
end;

procedure TprGenCell.SetCellWidth(NewWidth : integer);
begin
  FWidth := NewWidth;
end;

/////////////////////////////////////////////////
//
// TprGenBandInfo
//
/////////////////////////////////////////////////
constructor TprGenBandInfo.Create(Vector: TprGenVector);
begin
  inherited Create;
  FVector := Vector;
end;

destructor TprGenBandInfo.Destroy;
begin
  inherited;
end;

function TprGenBandInfo.GetBand: TprBand;
begin
  Result := FVector.Band;
end;

procedure TprGenBandInfo.SetSize(Value: integer);
begin
  FSize := Value;
end;

procedure TprGenBandInfo.SetStartNewPage(Value: boolean);
begin
  if Value and (Band.BandType in [bthTitle,
                                  btvTitle,
                                  bthSummary,
                                  btvSummary,
                                  bthPageHeader,
                                  bthPageFooter,
                                  btvPageHeader,
                                  btvPageFooter]) then
    raise Exception.CreateFmt(sInvalidBandTypeForStartNewPage, [Band.BandTypeStr, Band.Name]);
  FStartNewPage := Value;
end;

procedure TprGenBandInfo.SetBreakPage(Value: Boolean);
begin
  if Value and (Band.BandType in [bthTitle,
                                  btvTitle,
                                  bthSummary,
                                  btvSummary,
                                  bthPageHeader,
                                  bthPageFooter,
                                  btvPageHeader,
                                  btvPageFooter]) then
    raise Exception.CreateFmt(sInvalidBandTypeForBreakPage, [Band.BandTypeStr, Band.Name]);
  FBreakPage := Value;
end;

procedure TprGenBandInfo.SetResetPagesCount(Value: Boolean);
begin
  FResetPagesCount := Value;
end;

procedure TprGenBandInfo.SetLinkedBand(Value: TprBand);
begin
  if Value <> nil then
  begin
    if Band.BandType in [bthTitle,
                         btvTitle,
                         bthPageHeader,
                         bthPageFooter,
                         btvPageHeader,
                         btvPageFooter] then
      raise Exception.CreateFmt(sInvalidBandTypeForLink, [Band.BandTypeStr, Band.Name]);

    if Value.BandType in [bthTitle,
                          btvTitle,
                          bthSummary,
                          btvSummary,
                          bthPageHeader,
                          bthPageFooter,
                          btvPageHeader,
                          btvPageFooter] then
      raise Exception.CreateFmt(sInvalidLink, [Value.BandTypeStr, Value.Name, Band.BandTypeStr, Band.Name]);

    if ((Value.BandType in VerticalBands) and (Band.BandType in HorizontalBands)) or
       ((Value.BandType in HorizontalBands) and (Band.BandType in VerticalBands)) then
      raise Exception.CreateFmt(sInvalidLink, [Value.BandTypeStr, Value.Name, Band.BandTypeStr, Band.Name]);
  end;
  FLinkedBand := Value;
end;

procedure TprGenBandInfo.SetLinkToBand(Value: TprBand);
begin
  if Value <> nil then
  begin
    if Band.BandType in [bthTitle,
                         btvTitle,
                         bthPageHeader,
                         bthPageFooter,
                         btvPageHeader,
                         btvPageFooter] then
      raise Exception.CreateFmt(sInvalidBandTypeForLink, [Band.BandTypeStr, Band.Name]);

    if Value.BandType in [bthTitle,
                          btvTitle,
                          bthSummary,
                          btvSummary,
                          bthPageHeader,
                          bthPageFooter,
                          btvPageHeader,
                          btvPageFooter] then
      raise Exception.CreateFmt(sInvalidLink, [Value.BandTypeStr, Value.Name, Band.BandTypeStr, Band.Name]);

    if ((Value.BandType in VerticalBands) and (Band.BandType in HorizontalBands)) or
       ((Value.BandType in HorizontalBands) and (Band.BandType in VerticalBands)) then
      raise Exception.CreateFmt(sInvalidLink, [Value.BandTypeStr, Value.Name, Band.BandTypeStr, Band.Name]);
  end;
  FLinkToBand := Value;
end;

procedure TprGenBandInfo.SetReprintOnEachPage(Value: Boolean);
begin
  if Value and (Band.BandType in [bthTitle,
                                  btvTitle,
                                  bthSummary,
                                  btvSummary,
                                  bthPageHeader,
                                  bthPageFooter,
                                  btvPageHeader,
                                  btvPageFooter]) then
    raise Exception.CreateFmt(sInvalidBandTypeForReprintOnEachPage, [Band.BandTypeStr, Band.Name]);
  FReprintOnEachPage := Value;
end;

procedure TprGenBandInfo.SetCanSplit(Value: Boolean);
begin
  if Value and (Band.BandType in [bthTitle,
                                  btvTitle,
                                  bthPageHeader,
                                  bthPageFooter,
                                  btvPageHeader,
                                  btvPageFooter]) then
    raise Exception.CreateFmt(sInvalidBandTypeForSplit, [BandTitles[Band.BandType]]);
  FCanSplit := Value;
end;

procedure TprGenBandInfo.SetSubReportName(Value: string);
begin
  if Value <> FSubReportName then
  begin
    if Value <> '' then
    begin
      if Band.Page.HasCrossTabReport then
        raise Exception.Create(sSubReportCanNotSpecifiedInCrossTab);

      if (Band.BandType in [bthTitle, bthPageHeader, bthPageFooter]) or (Band.BandType in VerticalBands) then
        raise Exception.CreateFmt(sSubReportCanNotSpecified, [GetEnumName(TypeInfo(TprBandType), Integer(Band.BandType))]);

      // try to find the component reference
      FSubReport := Band.Report.GetSubReportByName(Value);
      if FSubReport = nil then
        raise Exception.CreateFmt(sSubReportNotFont, [value]);
    end
    else
      FSubReport := nil;
    FSubReportName := Value;
  end;
end;

procedure TprGenBandInfo.SetMinSubBand(Value: TprBand);
begin
  FMinSubBand := Value;
end;

procedure TprGenBandInfo.SetMinSubBandCount(Value: Integer);
begin
  FMinSubBandCount := Value;
end;

///////////////////////////////
//
// TprGenVector
//
///////////////////////////////
constructor TprGenVector.Create(Band : Tprband);
begin
  inherited Create;
  FSectionIndex := MaxInt;
  FSectionLevel := MaxInt; 
  FBand := Band;
  FCells := TList.Create;
end;

destructor TprGenVector.Destroy;
begin
  FCells.Free;
  inherited;
end;

procedure TprGenVector.AddCell(Cell : TprGenCell);
begin
  FCells.Add(Cell);
end;

procedure TprGenVector.RecalculateSizes;
var
  I: Integer;
  ACellSize: Integer;
begin
  case Band.ResizeMode of
    prbrmNone, prbrmMaxObj, prbrmMaxResizeObj:
      begin
        if CellsCount > 0 then
        begin
          Size := GetCellSize(Cells[0]);
          for I := 1 to CellsCount - 1 do
          begin
            ACellSize := GetCellSize(Cells[I]);
            if ACellSize > Size then
              Size := ACellSize;
          end;
        end;
      end;
    prbrmMinResizeObj:
      begin
        // serch for MIN cell
        if CellsCount > 0 then
        begin
          Size := GetCellSize(Cells[0]);
          for I := 1 to CellsCount - 1 do
          begin
            ACellSize := GetCellSize(Cells[I]);
            if ACellSize < Size then
              Size := ACellSize;
          end;
        end;
      end;
  end;

  UpdateObjectsSizes;
end;

function TprGenVector.GetCell(i : integer) : TprGenCell;
begin
  Result := TprGenCell(FCells[i]);
end;

function TprGenVector.GetCellsCount : integer;
begin
  Result := FCells.Count;
end;

/////////////////////////////////////////////////
//
// TprGenHorzBandInfo
//
/////////////////////////////////////////////////
procedure TprGenHorzBandInfo.CheckUseVerticalBands;
begin
if (Band as TprCustomHBand).UseVerticalBands then
  raise Exception.Create(sColumnIfUseVerticalBands);
end;

procedure TprGenHorzBandInfo.SetUseColumns(Value : boolean);
begin
  if Value then CheckUseVerticalBands;
  FUseColumns := Value;
end;

procedure TprGenHorzBandInfo.SetStartNewColumn(Value : boolean);
begin
  FStartNewColumn := Value;
end;

procedure TprGenHorzBandInfo.SetBreakColumn(Value : boolean);
begin
  FBreakColumn := Value;
end;

procedure TprGenHorzBandInfo.SetColDirection(Value : TprColDirectionType);
begin
  FColDirection := Value;
end;

procedure TprGenHorzBandInfo.SetColCount(Value : integer);
begin
  FColCount := Value;
end;

/////////////////////////////////////////////////
//
// TprSubReportData
//
/////////////////////////////////////////////////
constructor TprSubReportData.Create(AReport: TprCustomReport; AParentReport: TprCustomReport);
var
  AValue: TprValue;
begin
  inherited Create;

  FEndPages := AReport.FEndPages;
  AReport.FEndPages := TList.Create;

  FValues := TprValues.Create(TprValue);
  FValues.Assign(AReport.Values);

  FSystemValues := TprValues.Create(TprValue);
  FSystemValues.Assign(AReport.SystemValues);
  AValue := FSystemValues.ByName['Page'];
  AValue.OnCalc := AParentReport.OnCalcPageNo2;
  Avalue.OnGetVersionByVersionID := AParentReport.OnCalcPageNo;

  AValue := FSystemValues.ByName['PagesCount'];
  AValue.OnCalc := AParentReport.OnCalcPagesCount2;
  AValue.OnGetVersionByVersionID := AParentReport.OnCalcPagesCount;

  FParser := AReport.Parser;
  TprParser(FParser).SystemValues := FSystemValues;
  TprParser(FParser).Values := FValues;
  AReport.FParser := TprParser.Create(AReport, AReport.Values, AReport.SystemValues);

  FFinishedPositionOnLastPage := AReport.FinishedPositionOnLastPage;
end;

destructor TprSubReportData.Destroy;
begin
  FValues.Free;
  FSystemValues.Free;
  FParser.Free;
  FreeList(FEndPages);
  inherited;
end;

function TprSubReportData.GetEmpty: Boolean;
begin
  Result := (FEndPages = nil) or (FEndPages.Count <= 0);
end;

function TprSubReportData.GetEndPagesCount: Integer;
begin
  Result := FEndPages.Count;
end;

function TprSubReportData.GetEndPage(I: Integer): TprCustomEndPage;
begin
  Result := TprCustomEndPage(FEndPages[I]);
end;

procedure TprSubReportData.DeleteEndPage(Index: Integer);
begin
  FEndPages.Delete(Index);
end;

function TprSubReportData.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE
end;

function TprSubReportData._AddRef: Integer;
begin
  Result := S_OK
end;

function TprSubReportData._Release: Integer;
begin
  Result := S_OK
end;

function TprSubReportData.FormatStrings(lSource, lDest: TStrings; DeleteEmptyLines, DeleteEmptyLinesAtEnd: Boolean): Boolean;
begin
  if FParser = nil then
    Result := False
  else
    Result := TprParser(FParser).FormatStrings(lSource, lDest, DeleteEmptyLines, DeleteEmptyLinesAtEnd);
end;

function TprSubReportData.FormatTemplate(Template: string; var Res: string): Boolean;
begin
  if FParser = nil then
    Result := False
  else
    Result := TprParser(FParser).FormatTemplate(Template, Res);
end;

/////////////////////////////////////////////////
//
// TprGenHorzVector
//
/////////////////////////////////////////////////
constructor TprGenHorzVector.Create(Band : TprBand);
begin
  inherited;
  FInfo := TprGenHorzBandInfo.Create(Self);
  //FBand.FillGenInfo(Info);
end;

destructor TprGenHorzVector.Destroy;
begin
  FInfo.Free;
//  FSubReportData.Free;
  inherited;
end;

function TprGenHorzVector.GetGenInfo : TprGenBandInfo;
begin
  Result := FInfo;
end;

function TprGenHorzVector.GetCellSize(ACell: TprGenCell): Integer;
begin
  Result := ACell.Height;
end;

procedure TprGenHorzVector.UpdateObjectsSizes;
var
  I, J: Integer;
begin
  // if vector has objects for which HeightAsHorizontalBand=true then
  // correct its height
  for I := 0 to CellsCount - 1 do
    for J := 0 to Cells[I].ObjRecsCount - 1 do
      with Cells[I].ObjRecs[J] do
        if HeightAsHorizontalBand then
          FpRect.Bottom := FpRect.Top + Size - FpRect.Top;
end;

/////////////////////////////////////////////////
//
// TprGenVertVector
//
/////////////////////////////////////////////////
constructor TprGenVertVector.Create(Band: TprBand);
begin
  inherited;
  FInfo := TprGenVertBandInfo.Create(Self);
end;

destructor TprGenVertVector.Destroy;
begin
  FInfo.Free;
  inherited;
end;

function TprGenVertVector.GetGenInfo: TprGenBandInfo;
begin
  Result := FInfo;
end;

function TprGenVertVector.GetCellSize(ACell: TprGenCell): Integer;
begin
  Result := ACell.Width;
end;

procedure TprGenVertVector.UpdateObjectsSizes;
var
  I, J: Integer;
begin
  // if vector has objects for which WidthAsVerticalBand=true then
  // correct its width
  for I := 0 to CellsCount - 1 do
    for J := 0 to Cells[I].ObjRecsCount - 1 do
      with Cells[I].ObjRecs[J] do
        if WidthAsVerticalBand then
          FpRect.Right := FpRect.Left + Size - FpRect.Left;
end;

/////////////////////////////////////////////////
//
// TprGenGrid
//
/////////////////////////////////////////////////
constructor TprGenGrid.Create;
begin
  inherited Create;
  FPage := Page;
  FCells := TList.Create;
  FVertVectors := TList.Create;
  FHorzVectors := TList.Create;
end;

destructor TprGenGrid.Destroy;
begin
  Clear;
  FCells.Free;
  FVertVectors.Free;
  FHorzVectors.Free;
  inherited;
end;

procedure TprGenGrid.Clear;

  procedure FreeCell(var Cell : TprGenCell);
  begin
    if Cell <> nil then
    begin
      Cell.Free;
      Cell := nil;
    end;
  end;

begin
  FHorzSectionIndex := 0;
  FVertSectionIndex := 0;
  FHorzSectionLevel := 0;
  FVertSectionLevel := 0;
  FProgressUpdateCounter := 1;
  FCurHorzVector := nil;
  FCurVertVectorIndex := -1;
  FreeCell(FHorzTitle);
  FreeCell(FVertTitle);
  FreeCell(FHorzPageHeader);
  FreeCell(FVertPageHeader);
  FreeCell(FHorzPageFooter);
  FreeCell(FVertPageFooter);
  FreeListItems(FCells);
  FCells.Clear;
  FreeListItems(FVertVectors);
  FVertVectors.Clear;
  FreeListItems(FHorzVectors);
  FHorzVectors.Clear;
end;

procedure TprGenGrid.SecondPass;
var
  I: Integer;

  procedure UpdateLinksBetweenVectors(AVectors: TList);
  var
    I, J: Integer;
    AVector, AOtherVector: TprGenVector;
  begin
    for I := 0 to AVectors.Count - 1 do
    begin
      AVector := TprGenVector(AVectors[I]);

      if AVector.GenInfo.LinkedBand <> nil then
      begin
        for J := I + 1 to AVectors.Count - 1 do
        begin
          AOtherVector := TprGenVector(AVectors[J]);
          if AOtherVector.SectionLevel < AVector.SectionLevel then
            break;
          if (AOtherVector.SectionLevel = AVector.SectionLevel) and
             (AOtherVector.SectionIndex <> AVector.SectionIndex) then
            break;
          if AOtherVector.Band = AVector.GenInfo.LinkedBand then
          begin
//            Assert(AOtherVector.LinkToVector = nil, 'Vector already has link.');
            if (AOtherVector.LinkToVector = nil) or
               (AVectors.IndexOf(AOtherVector.LinkToVector) > AVectors.IndexOf(AVector)) then
              AOtherVector.LinkToVector := AVector;
            break;
          end;
        end; 
      end;

      if AVector.GenInfo.LinkToBand <> nil then
      begin
        for J := I - 1 downto 0 do
        begin
          AOtherVector := TprGenVector(AVectors[J]);
          if AOtherVector.SectionLevel < AVector.SectionLevel then
            break;
            
          if (AOtherVector.SectionLevel = AVector.SectionLevel) and
             (AOtherVector.SectionIndex <> AVector.SectionIndex) then
            break;

          if AOtherVector.Band = AVector.GenInfo.LinkToBand then
          begin
//            Assert(AVector.LinkToVector = nil, 'Vector already has link.');
            if (AVector.LinkToVector = nil) or
               (AVectors.IndexOf(AVector.LinkToVector) > AVectors.IndexOf(AOtherVector)) then
              AVector.LinkToVector := AOtherVector;
            break;
          end;
        end;
      end;
    end;
  end;

  procedure ProcessMinSubBandCount(AVectors: TList);
  var
    I, J, AInc, ACount: Integer;
    AVector, AVector2: TprGenVector;
  begin
    I := 0;
    while I < AVectors.Count do
    begin
      AVector := TprGenVector(AVectors[I]);

      if (AVector.GenInfo.MinSubBand <> nil) and (AVector.GenInfo.MinSubBandCount > 0) then
      begin
        AInc := AVector.Band.GetSubBandSearchDirection;
        ACount := 0;
        if AInc <> 0 then
        begin
          J := I + AInc;
          while (J >= 0) and (J < AVectors.Count) do
          begin
            AVector2 := TprGenVector(AVectors[J]);
            if AVector2.SectionLevel < AVector.SectionLevel then
              break;
            if (AVector2.SectionLevel = AVector.SectionLevel) and
               (AVector2.SectionIndex <> AVector.SectionIndex) then
              break;

            if AVector2.Band = AVector.GenInfo.MinSubBand then
              ACount := ACount + 1;

            J := J + AInc;
          end;

          if ACount < AVector.GenInfo.MinSubBandCount then
          begin
            // Delete AVector
            DeleteAndFreeVector(AVector);
            AVectors.Delete(I);
            continue;
          end;
        end;
      end;

      Inc(I);
    end;
  end;

  procedure ProcessVisibleFormula(AVectors: TList);
  var
    I: Integer;
    AVector: TprGenVector;
  begin
    I := 0;
    while I < AVectors.Count do
    begin
      AVector := TprGenVector(AVectors[I]);
      if AVector.VisibleFormula <> '' then
      begin
        if not AVector.Band.Report.CalcBooleanFormula(AVector.VisibleFormula) then
        begin
          DeleteAndFreeVector(AVector);
          AVectors.Delete(I);
          continue;
        end;
      end;
      Inc(I);
    end;
  end;

{$IFDEF DEBUG_FIRSTLINKEDVECTOR}
  procedure DumpVectors(const ACaption: string; AVectors: TList);
  var
    I: Integer;
  begin
    DbgFileStr(ACaption + #13#10);
    DbgFileStr('Index BandClassName                  SectionLevel SectionIndex LinkToVector'#13#10);
    DbgFileStr('---------------------------------------------------------------------------'#13#10);
    for I := 0 to AVectors.Count - 1 do
      with TprGenVector(AVectors[I]) do
      begin
        DbgFileStrFmt('%5d %30s %12d %12d %12d'#13#10,
                        [I,
                         Band.ClassName,
                         SectionLevel,
                         SectionIndex,
                         AVectors.IndexOf(LinkToVector)]);
      end;
  end;
{$ENDIF}

begin
  // process MinSubBand / MinSubBandCount information
  ProcessMinSubBandCount(FHorzVectors);
  ProcessMinSubBandCount(FVertVectors);

  // process visible formula
  ProcessVisibleFormula(FHorzVectors);
  ProcessVisibleFormula(FVertVectors);

  for I := 0 to HorzVectorsCount - 1 do
    HorzVectors[I].RecalculateSizes;
  for I := 0 to VertVectorsCount - 1 do
    VertVectors[I].RecalculateSizes;

  UpdateLinksBetweenVectors(FHorzVectors);
  UpdateLinksBetweenVectors(FVertVectors);

{$IFDEF DEBUG_FIRSTLINKEDVECTOR}
  DumpVectors('Horizontal vectors', FHorzVectors);
  DumpVectors('Vertical vectors', FVertVectors);
{$ENDIF}
end;

procedure TprGenGrid.BeginHorzSection;
begin
  Inc(FHorzSectionLevel);
  Inc(FHorzSectionIndex);
end;

procedure TprGenGrid.EndHorzSection;
begin
  Dec(FHorzSectionLevel);
end;

procedure TprGenGrid.BeginVertSection;
begin
  Inc(FVertSectionLevel);
  Inc(FVertSectionIndex);
end;

procedure TprGenGrid.EndVertSection;
begin
  Dec(FVertSectionLevel);
end;

procedure TprGenGrid.DeleteAndFreeVector(AVector: TprGenVector);
var
  I, J: Integer;
  ACell: TprGenCell;
  AOtherVectors: TList;
begin
  if AVector is TprGenHorzVector then
    AOtherVectors := FVertVectors
  else
    AOtherVectors := FHorzVectors;

  // delete cells of vector
  for I := AVector.FCells.Count - 1 downto 0 do
  begin
    ACell := TprGenCell(AVector.FCells[I]);
    for J := 0 to AOtherVectors.Count - 1 do
      with TprGenVector(AOtherVectors[J]) do
        FCells.Remove(ACell);
    FCells.Remove(ACell);
    ACell.Free;
  end;

  AVector.Free;
end;

function TprGenGrid.GetCell(I: Integer) : TprGenCell;
begin
  Result := TprGenCell(FCells[I]);
end;

function TprGenGrid.GetCellsCount : integer;
begin
  Result := FCells.Count;
end;

function TprGenGrid.GetVertVector(i : integer) : TprGenVertVector;
begin
Result := TprGenVertVector(FVertVectors[i]);
end;

function TprGenGrid.GetHorzVector(i : integer) : TprGenHorzVector;
begin
Result := TprGenHorzVector(FHorzVectors[i]);
end;

function TprGenGrid.GetVertVectorsCount : integer;
begin
Result := FVertVectors.Count;
end;

function TprGenGrid.GetHorzVectorsCount : integer;
begin
Result := FHorzVectors.Count;
end;

function TprGenGrid.CreateAndAddHorzVector(ABand: TprCustomHBand): TprGenHorzVector;
begin
  Result := TprGenHorzVector.Create(ABand);
  Result.FSectionIndex := FHorzSectionIndex;
  Result.FSectionLevel := FHorzSectionLevel;
  ABand.FillGenInfo(Result.Info);
  FHorzVectors.Add(Result);
end;

function TprGenGrid.CreateAndAddVertVector(ABand: TprCustomVBand): TprGenVertVector;
begin
  Result := TprGenVertVector.Create(ABand);
  Result.FSectionIndex := FVertSectionIndex;
  Result.FSectionLevel := FVertSectionLevel;
  ABand.FillGenInfo(Result.Info);
  FVertVectors.Add(Result);
end;

function TprGenGrid.SkipVerticalBand(VertBand: TprCustomVBand): TprGenVertVector;
begin
  Inc(FCurVertVectorIndex);
  if FCurVertVectorIndex >= VertVectorsCount then
    Result := CreateAndAddVertVector(VertBand)
  else
    if FCurVertVectorIndex - 1 >= 0 then
      Result := VertVectors[FCurVertVectorIndex - 1]
    else
      Result := nil;
end;

function TprGenGrid.IsEndOfLine : boolean;
begin
Result := FCurVertVectorIndex+1>=VertVectorsCount;
end;

function TprGenGrid.AddCell(HorzBand: TprCustomHBand; VertBand: TprCustomVBand) : TprGenCell;
var
  HorzInfo: TprGenHorzBandInfo;
  VertInfo: TprGenVertBandInfo;
  VertVector: TprGenVertVector;

begin
  Result := TprGenCell.Create;
  if (HorzBand <> nil) and
     (HorzBand.BandType in [bthTitle,
                            bthPageHeader,
                            bthPageFooter]) then
  begin
    Result.Band := HorzBand;
    case HorzBand.BandType of
      bthTitle: FHorzTitle := Result;
      bthPageHeader: FHorzPageHeader := Result;
      bthPageFooter: FHorzPageFooter := Result;
    end;
  end
  else
    if (VertBand <> nil) and
       (VertBand.BandType in [btvTitle,
                              btvPageHeader,
                              btvPageFooter]) then
    begin
      Result.Band := VertBand;
      case VertBand.BandType of
        btvTitle: FVertTitle := Result;
        btvPageHeader: FVertPageHeader := Result;
        btvPageFooter: FVertPageFooter := Result;
      end;
    end
    else
    begin
      if (HorzBand <> nil) and (FCurHorzVector = nil) then
        FCurHorzVector := CreateAndAddHorzVector(HorzBand);
      if VertBand <> nil then
        begin
          Inc(FCurVertVectorIndex);
          if FCurVertVectorIndex < VertVectorsCount then
            VertVector := VertVectors[FCurVertVectorIndex]
          else
            VertVector := CreateAndAddVertVector(VertBand);
        end
      else
        VertVector := nil;
        
      Result.HorzVector := FCurHorzVector;
      Result.VertVector := VertVector;
      FCells.Add(Result);
      
      if (FCurHorzVector <> nil) and (HorzBand <> nil) then
      begin
        FCurHorzVector.AddCell(Result);
        HorzInfo := FCurHorzVector.Info;
      end
      else
        HorzInfo := nil;
        
      if VertVector <> nil then
      begin
        VertVector.AddCell(Result);
        VertInfo := VertVector.Info;
      end
      else
        VertInfo := nil;
        
      Page.Report.DoOnBandGenerateCell(HorzInfo,VertInfo);

      Dec(FProgressUpdateCounter);
      if FProgressUpdateCounter=0 then
      begin
        Page.Report.UpdateProgressForm(Format(prLoadStr(sFirstPassCaption),[HorzVectorsCount,VertVectorsCount,CellsCount]));
        FProgressUpdateCounter := GenGridUpdateCounter;
      end;
    end;
end;

procedure TprGenGrid.EndLine;
begin
FCurHorzVector := nil;
FCurVertVectorIndex := -1;
end;

procedure TprGenGrid.CopyCellInfoHorz(ASource, ADest: TprGenCell; AHorzVector: TprGenHorzVector; AHorzVectorPos: Integer);
var
  I, ACellPos: Integer;
begin
  ADest.FWidth := ASource.Width;
  ADest.FHeight := ASource.Height;
  ADest.HorzVector := AHorzVector;
  ADest.VertVector := ASource.VertVector;
  if ADest.VertVector <> nil then
  begin
    ACellPos := 0;
    for I := 0 to AHorzVectorPos - 1 do
      if TprCustomHBand(HorzVectors[I].Band).UseVerticalBands then
        Inc(ACellPos);
    ADest.VertVector.FCells.Insert(ACellPos, ADest);
  end;
end;

function TprGenGrid.SplitHorzVector(AVector: TprGenHorzVector; AVectorPos: Integer; ASplitPos: Integer): TprGenHorzVector;
var
  ACellsList: TList;
  ASplittedRecs: TList;
  I, J, AAddToSplitted, ATemp: Integer;
  ANewCell: TprGenCell;
  ANewRec: TprObjRec;
  AAboveExists: Boolean;
begin
  Result := nil;
  // vector can be splitted by ASplitPos
  // find objects above ASplitPos
  ACellsList := TList.Create;
  ASplittedRecs := TList.Create;
  try
    AAboveExists := False;
    AAddToSplitted := 0; // value, that must be added to size of splited vector
    for I := 0 to AVector.CellsCount - 1 do
    begin
      ANewCell := TprGenCell.Create;
      ACellsList.Add(ANewCell);
      with AVector.Cells[I] do
      begin
        J := 0;
        while J < ObjRecsCount do
          if ObjRecs[J].FpRect.Top < ASplitPos then
          begin
            AAboveExists := True;
            if ObjRecs[J].FpRect.Bottom < ASplitPos then
            begin
              ANewCell.Add(ObjRecs[J]);
              FObjRecs.Delete(J);
            end
            else
            begin
              // ObjRec must be splitted
              ASplittedRecs.Add(ObjRecs[J]);
              ATemp := 0;
              ANewRec := ObjRecs[J].Split(False, ASplitPos - ObjRecs[J].FpRect.Top, ATemp);
              if ANewRec <> nil then
              begin
                ANewCell.Add(ANewRec);
                if ATemp > AAddToSplitted then
                  AAddToSplitted := ATemp;
              end;
              Inc(J);
            end;
          end
          else
            Inc(J);
      end;
    end;

    if AAboveExists then
    begin
      // create a vector's copy and move objects from ACellsList to this copy
      Result := TprGenHorzVector.Create(AVector.Band);
      Result.FLeft := AVector.Left;
      Result.FTop := AVector.Top;
      Result.FSize := ASplitPos;
      for I := 0 to ACellsList.Count - 1 do
      begin
        ANewCell := TprGenCell(ACellsList[I]);
        Result.FCells.Add(ANewCell);
        FCells.Add(ANewCell);

        CopyCellInfoHorz(AVector.Cells[I], ANewCell, Result, AVectorPos);
      end;

      FHorzVectors.Insert(AVectorPos, Result);
    end
    else
      FreeListItems(ACellsList);

    // decrease vector size and cells height,
    // and shifts all objects in them
    AVector.Size := AVector.Size - ASplitPos + AAddToSplitted;
    for I := 0 to AVector.CellsCount - 1 do
      with AVector.Cells[I] do
      begin
        FHeight := FHeight - ASplitPos + AAddToSplitted;
        for J := 0 to ObjRecsCount - 1 do
          if ASplittedRecs.IndexOf(ObjRecs[J]) = -1 then
            with ObjRecs[J] do
              OffsetRect(FpRect, 0, -ASplitPos);
      end;
  finally
    ACellsList.Free;
    ASplittedRecs.Free;
  end;
end;

function TprGenGrid.CopyHorzVectorToPos(Vector : TprGenHorzVector; CopyPos : integer) : TprGenHorzVector;
var
  Cell: TprGenCell;
  I, J: integer;
begin
  Result := TprGenHorzVector.Create(Vector.Band);
  Result.FSize := Vector.Size;
  for I := 0 to Vector.CellsCount - 1 do
  begin
    Cell := TprGenCell.Create;
    Result.FCells.Add(Cell);
    FCells.Add(Cell);
    for J := 0 to Vector.Cells[I].ObjRecsCount - 1 do
      Cell.Add(Vector.Cells[I].ObjRecs[J].CreateCopy);

    CopyCellInfoHorz(Vector.Cells[I], Cell, Result, CopyPos);
  end;
      
  FHorzVectors.Insert(CopyPos, Result);
end;

procedure TprGenGrid.CopyCellInfoVert(ASource, ADest: TprGenCell; AVertVector: TprGenVertVector; AVertVectorPos: Integer);
var
  I, ACellPos: Integer;
begin
  ADest.FWidth := ASource.Width;
  ADest.FHeight := ASource.Height;
  ADest.VertVector := AVertVector;
  ADest.HorzVector := ASource.HorzVector;
  if ADest.HorzVector <> nil then
  begin
    ACellPos := 0;
    for I := 0 to AVertVectorPos - 1 do
      if TprCustomVBand(VertVectors[I].Band).UseHorizontalBands then
        Inc(ACellPos);
    ADest.HorzVector.FCells.Insert(ACellPos, ADest);
  end;
end;

function TprGenGrid.SplitVertVector(AVector: TprGenVertVector; AVectorPos: Integer; ASplitPos: Integer): TprGenVertVector;
var
  ACellsList: TList;
  ASplittedRecs: TList;
  I, J, ATemp, AAddToSplitted: Integer;
  ANewRec: TprObjRec;
  ANewCell: TprGenCell;
  AAboveExists: Boolean;
begin
  Result := nil;
  // vector can be splitted by ASplitPos
  // find objects above ASplitPos
  ACellsList := TList.Create;
  ASplittedRecs := TList.Create;
  try
    AAboveExists := False;
    AAddToSplitted := 0;
    for I := 0 to AVector.CellsCount - 1 do
    begin
      ANewCell := TprGenCell.Create;
      ACellsList.Add(ANewCell);
      with AVector.Cells[I] do
      begin
        J := 0;
        while J < ObjRecsCount do
          if ObjRecs[J].FpRect.Left < ASplitPos then
          begin
            AAboveExists := True;
            if ObjRecs[J].FpRect.Right < ASplitPos then
            begin
              ANewCell.Add(ObjRecs[J]);
              FObjRecs.Delete(J);
            end
            else
            begin
              // ObjRect must be splitted
              ASplittedRecs.Add(ObjRecs[J]);
              ATemp := 0;
              ANewRec := ObjRecs[J].Split(True, ASplitPos - ObjRecs[J].FpRect.Left, ATemp);

              OffsetRect(ObjRecs[J].FpRect,
                         VertVector.Band.dPageRect.Left - HorzVector.Band.dPageRect.Left,
                         0); // !!!
              if ANewRec <> nil then
              begin
                if ATemp > AAddToSplitted then
                  AAddToSplitted := ATemp;
                ANewCell.Add(ANewRec);
              end;
              Inc(J);
            end;
          end
          else
            Inc(J);
      end;
    end;

    if AAboveExists then
    begin
      // create a vector's copy and move objects from ACellsList to this copy
      Result := TprGenVertVector.Create(AVector.Band);
      Result.FLeft := AVector.Left;
      Result.FTop := AVector.Top;
      Result.FSize := ASplitPos;
      for I := 0 to ACellsList.Count - 1 do
      begin
        ANewCell := TprGenCell(ACellsList[I]);
        Result.FCells.Add(ANewCell);
        FCells.Add(ANewCell);

        CopyCellInfoVert(AVector.Cells[I], ANewCell, Result, AVectorPos);
      end;

      FVertVectors.Insert(AVectorPos, Result);
    end
    else
      FreeListItems(ACellsList);

    // decrease vector size and cells width
    // and shifts all objects in them
    AVector.Size := AVector.Size - ASplitPos + AAddToSplitted;
    for I := 0 to AVector.CellsCount - 1 do
      with Cells[I] do
      begin
        FWidth := FWidth - ASplitPos + AAddToSplitted;
        for J := 0 to ObjRecsCount - 1 do
          if ASplittedRecs.IndexOf(ObjRecs[J]) = -1 then
            with ObjRecs[J] do
              OffsetRect(FpRect, -ASplitPos, 0);
      end;
  finally
    ACellsList.Free;
    ASplittedRecs.Free;
  end;
end;

function TprGenGrid.CopyVertVectorToPos(Vector: TprGenVertVector; CopyPos: Integer): TprGenVertVector;
var
  Cell: TprGenCell;
  I, J: integer;
begin
  Result := TprGenVertVector.Create(Vector.Band);
  Result.FSize := Vector.Size;
  for I := 0 to Vector.CellsCount - 1 do
  begin
    Cell := TprGenCell.Create;
    Result.FCells.Add(Cell);
    FCells.Add(Cell);
    for J := 0 to Vector.Cells[I].ObjRecsCount - 1 do
      Cell.Add(Vector.Cells[I].ObjRecs[J].CreateCopy);

    CopyCellInfoVert(Vector.Cells[I], Cell, Result, CopyPos);
  end;

  FVertVectors.Insert(CopyPos,Result);
end;

/////////////////////////
//
// TprGroups
//
/////////////////////////
function TprGroups.GetItm;
begin
Result:=TprGroup(inherited Items[index]);
end;

function TprGroups.GetByName;
var
  i : integer;
begin
i:=IndexByName(Name);
if i=-1 then
  raise Exception.CreateFmt(prLoadStr(sGroupNotExists),[name])
else
  result:=Items[i];
end;

function TprGroups.IndexByName;
begin
Result:=0;
while (Result<Count) and (CompText(Items[Result].Name,Name)<>0) do Inc(Result);
if Result>=Count then
  Result:=-1;
end;

///////////////////////
//
// TprBand
//
///////////////////////
constructor TprBand.Create;
var
  i : integer;
  ClassRef: TClass;
begin
inherited Create(AOwner);
FVisible := true;
FResizeMode := prbrmNone;
FResizeObjs := TprObjs.Create;
FObjects := TprObjs.Create;
ClassRef := ClassType;
while ClassRef<>nil do
  begin
    i := GetEnumValue(TypeInfo(TprBandType),'bt'+Copy(ClassRef.ClassName,10,Length(ClassRef.ClassName)-13));
    if i<>-1 then
      begin
        FBandType := TprBandType(i);
        break;
      end;
    ClassRef := ClassRef.ClassParent;
  end;
end;

destructor TprBand.Destroy;
begin
while Objects.Count>0 do
  Objects[0].Free;

Objects.Free;
ResizeObjs.Free;

inherited;
end;

function TprBand.GetSubBandSearchDirection: Integer; 
begin
  Result := 0;
end;

function TprBand.GetChildOwner;
begin
Result := Report;
end;

procedure TprBand.SetParentComponent;
begin
Page := Value as TprCustomPage;
end;

function TprBand.HasParent;
begin
Result := true;
end;

function TprBand.GetParentComponent;
begin
Result := Page;
end;

procedure TprBand.GetChildren;
var
  i : integer;
begin
for i:=0 to Objects.Count-1 do
  Proc(Objects[i]);
end;

procedure TprBand.Notification;
begin
if (Operation = opRemove) and (AComponent<>Self) and (AComponent is TprObj) then
  begin
    Objects.Remove(AComponent);
    ResizeObjs.Remove(AComponent);
  end;
end;

procedure TprBand.DefineProperties;
begin
inherited;
Filer.DefineProperty('ResizeObjs',ReadResizeObjs,WriteResizeObjs,ResizeObjs.Count>0);
end;

procedure TprBand.ReadResizeObjs;
begin
FResizeObjsNames := TStringList.Create;
prReadStringList(Reader,FResizeObjsNames);
end;

procedure TprBand.WriteResizeObjs;
begin
prWriteCompListNames(Writer,ResizeObjs);
end;

procedure TprBand.GroupRemoved;
begin
end;

procedure TprBand.GroupAdded;
begin
end;

procedure TprBand.AfterReportLoaded;
var
  i,j : integer;
begin
if FResizeObjsNames<>nil then
  begin
    for i:=0 to FResizeObjsNames.Count-1 do
      begin
        j := 0;
        while (j<Objects.Count) and (CompText(Objects[j].Name,FResizeObjsNames[i])<>0) do Inc(j);
        if j<Objects.Count then
          ResizeObjs.Add(Objects[j]);
      end;
    FResizeObjsNames.Free;
  end;
end;

procedure TprBand.SetPage;
begin
if FPage=Value then exit;
if FPage<>nil then
  Page.Bands.Remove(Self);
FPage := Value;
if (FPage<>nil) and (FPage.Bands.IndexOf(Self)=-1) then
  FPage.Bands.Add(Self);
end;

procedure TprBand.SetVisibleFormula(Value : string);
begin
FVisibleFormula := Trim(Value);
end;

function TprBand.GetBandTypeStr: string;
begin
  Result := GetEnumName(TypeInfo(TprBandType), Integer(BandType));
end;

function TprBand.Report;
begin
Result := nil;
if Page<>nil then
  Result := Page.Report;
end;

procedure TprBand.InitDataset;
begin
end;

function TprBand.GetDrawDesignCaption;
begin
  Result := BandTitles[BandType];
end;

procedure TprBand.OnInsertIntoPage;
begin
  if p.Bands.IndexByBandType(BandType)<>-1 then
    raise Exception.CreateFmt(prLoadStr(sBandAlreadyExistsInPageType),[GetDrawDesignCaption]);
end;

function TprBand.DsgnAllowLinkWith(OtherObject : TprDesignComponent) : boolean;
begin
Result := (OtherObject is TprObj) and (ResizeObjs.IndexOf(OtherObject)=-1) and (TprObj(OtherObject).Band=Self);
end;

procedure TprBand.FillGenInfo(Info : TprGenBandInfo);
begin
  Info.FCanSplit := CanSplit;
  Info.SubReportName := SubReportName;
end;

function TprBand.IsFormulaVisible(var ASecondPassNeeded: Boolean; var ASecondPassFormula: string): Boolean;
begin
  ASecondPassFormula := FVisibleFormula;
  Result := Report.CalcBooleanFormula(ASecondPassFormula, ASecondPassNeeded);
end;

procedure TprBand.DsgnNotifyDesigner;
begin
  Report.DsgnNotify(Self);
end;

procedure TprBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
  case TMenuItem(Sender).Tag of
   -1: Visible := not Visible;
   -2: ResizeMode := prbrmNone;
   -3: ResizeMode := prbrmMaxObj;
   -4: ResizeMode := prbrmMaxResizeObj;
   -5: ResizeMode := prbrmMinResizeObj;
  end;
  DsgnNotifyDesigner;
end;

procedure TprBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
var
  m: TMenuItem;
begin
  AddPopupMenuItem(Popup,nil,sVisible,'',OnDsgnPopupMenuClick,'',-1,true,Visible);
  m := AddPopupMenuItem(Popup, nil, sSize, '', nil, '', 0, true, false);
  AddPopupMenuItem(Popup,m,sBandResizeNoChange,'',OnDsgnPopupMenuClick,'',-2,true,ResizeMode=prbrmNone);
  AddPopupMenuItem(Popup,m,sBandResizeMaxBottom,'',OnDsgnPopupMenuClick,'',-3,true,ResizeMode=prbrmMaxObj);
  AddPopupMenuItem(Popup,m,sBandResizeMaxBottomInLinksList,'',OnDsgnPopupMenuClick,'',-4,true,ResizeMode=prbrmMaxResizeObj);
  AddPopupMenuItem(Popup,m,sBandResizeMinBottomInLinksList,'',OnDsgnPopupMenuClick,'',-5,true,ResizeMode=prbrmMinResizeObj);
end;

procedure TprBand.DsgnGetHintText(var HintCaption : string; HintText : TStringList);
begin
HintCaption := GetDrawDesignCaption;
end;

function TprBand.CalcBandSize(AObjects: TprObjs): Integer;
var
  I, AGenPos, AGenSize, ADesignMin, ADesignMax: Integer;
  AGenPosCur, AGenSizeCur, ADesignMinCur, ADesignMaxCur: Integer;
  AObjectExists: Boolean;
begin
  Result := GetSize;
  AObjectExists := False;
  case ResizeMode of
    prbrmMaxObj:
      begin
        if AObjects.Count > 0 then
        begin
          GetObjectSizes(AObjects[0], AGenPos, AGenSize, ADesignMin, ADesignMax);
          Result := AGenPos + AGenSize;
          for I := 1 to AObjects.Count - 1 do
          begin
            GetObjectSizes(AObjects[I], AGenPos, AGenSize, ADesignMin, ADesignMax);
            if (AGenPos + AGenSize) > Result then
              Result := AGenPos + AGenSize;
          end;
        end;
      end;

    prbrmMaxResizeObj:
      begin
        if ResizeObjs.Count > 0 then
        begin
          for I := 0 to AObjects.Count - 1 do
            if ResizeObjs.IndexOf(AObjects[I]) <> -1 then
            begin
              if AObjectExists then
              begin
                GetObjectSizes(AObjects[I], AGenPos, AGenSize, ADesignMin, ADesignMax);
                if AGenPos + AGenSize > AGenPosCur + AGenSizeCur then
                begin
                  AGenPosCur := AGenPos;
                  AGenSizeCur := AGenSize;
                  ADesignMinCur := ADesignMin;
                  ADesignMaxCur := ADesignMax;
                end;
              end
              else
                GetObjectSizes(AObjects[I], AGenPosCur, AGenSizeCur, ADesignMinCur, ADesignMaxCur);
            end;

          Result := Result + AGenPosCur + AGenSizeCur - ADesignMaxCur;
        end;
      end;

    prbrmMinResizeObj:
      begin
        if ResizeObjs.Count > 0 then
        begin
          for I := 0 to AObjects.Count - 1 do
            if ResizeObjs.IndexOf(AObjects[I]) <> -1 then
            begin
              if AObjectExists then
              begin
                GetObjectSizes(AObjects[I], AGenPos, AGenSize, ADesignMin, ADesignMax);
                if AGenPos + AGenSize > AGenPosCur + AGenSizeCur then
                begin
                  AGenPosCur := AGenPos;
                  AGenSizeCur := AGenSize;
                  ADesignMinCur := ADesignMin;
                  ADesignMaxCur := ADesignMax;
                end;
              end
              else
                GetObjectSizes(AObjects[I], AGenPosCur, AGenSizeCur, ADesignMinCur, ADesignMaxCur);
            end;

          Result := Result + AGenPosCur + AGenSizeCur - ADesignMaxCur;
        end;
      end;
  end;
end;

/////////////////////////////////////////////////
//
// TprBands
//
/////////////////////////////////////////////////
function TprBands.GetItm;
begin
Result := TprBand(inherited Items[index]);
end;

function TprBands.GetByBandType;
var
  i : integer;
begin
i:=IndexByBandType(BandType);
if i=-1 then
  raise Exception.CreateFmt(prLoadStr(sUnknownBandType),[GetEnumName(TypeInfo(TprBandType),integer(BandType))])
else
  Result:=Items[i];
end;

function TprBands.GetByName;
var
  i : integer;
begin
i:=IndexByName(Name);
if i=-1 then
  raise Exception.CreateFmt(prLoadStr(sUnknownBandName),[Name])
else
  Result:=Items[i];
end;

function TprBands.IndexByBandType;
begin
Result:=0;
while (Result<Count) and (Items[Result].BandType<>BandType) do Inc(Result);
if Result>=Count then
  Result:=-1;
end;

function TprBands.IndexByName;
begin
Result:=0;
while (Result<Count) and (CompText(Items[Result].Name,Name)<>0) do Inc(Result);
if Result>=Count then
  Result:=-1;
end;

/////////////////////////////
//
// TprCustomHBand
//
/////////////////////////////
function TprCustomHBand.GetSize: Integer;
begin
  Result := FHeight;
end;

procedure TprCustomHBand.GetObjectSizes(AObject: TprObj; var AGenPos, AGenSize, ADesignMin, ADesignMax: Integer);
begin
  with AObject do
  begin
    AGenPos := aRec.FpRect.Top;
    AGenSize := aRec.Height;

    ADesignMin := dRec.pRect.Top;
    ADesignMax := dRec.pRect.Bottom;
  end;
end;

procedure TprCustomHBand.CalcdPageRect;
begin
if FCalced then exit;
FdPageRect := Rect(CurPageRect.Left,CurPageRect.Top,CurPageRect.Right,CurPageRect.Top+Height);
CurPageRect.Top := CurPageRect.Top+Height;
FCalced := true;
end;

procedure TprCustomHBand.DsgnResize(oTop,oLeft,oBottom,oRight : integer; var ResizeAccepted : boolean; ExData : pointer);
begin
ResizeAccepted := dPageRect.Top<dPageRect.Bottom+oBottom;
if not ResizeAccepted then exit;
Height := dPageRect.Bottom+oBottom-dPageRect.Top-oTop;
Page.UpdateBandsPageRect;
end;

procedure TprCustomHBand.DsgnLink(Linked : TprDesignComponent; LinkMode : TprLinkType; var LinkAccepted : boolean; ExData : pointer);
begin
if (Linked is TprObj) and (Self=TprObj(Linked).Band) then
  begin
    case LinkMode of
      ltBottom : if ResizeObjs.IndexOf(Linked)=-1 then
                   begin
                     ResizeObjs.Add(Linked);
                     LinkAccepted := true;
                   end;
    end;
  end;
end;

function TprCustomHBand.DsgnAllowResizeTypes : TprResizeTypeSet;
begin
Result := [ppTop,ppBottom];
end;

function TprCustomHBand.DsgnAllowLinkTypes : TprLinkTypeSet; 
begin
Result := [ltBottom];
end;

procedure TprCustomHBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1: UseVerticalBands := not UseVerticalBands;
end;
DsgnNotifyDesigner;
end;

procedure TprCustomHBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
inherited;
if BandType in [btvPageHeader,btvPageFooter,bthPageHeader,bthPageFooter,bthTitle,bthSummary,btvTitle,btvSummary] then exit;
if Popup.Items.Count>0 then
  AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,nil,sUseVerticalBands,'',OnDsgnPopupMenuClick,'',-1,true,UseVerticalBands);
end;

function TprCustomHBand.GenerateCell(CallerBand: TprBand; CallbackProc: TGenerateCellCallbackProc) : TprGenCell;
var
  o: TprObjRec;
  I: integer;
  ASecondPassFormula: string;
  ASecondPassNeeded: Boolean;
begin
  Result := nil;
  if not Visible then exit;

  if not IsFormulaVisible(ASecondPassNeeded, ASecondPassFormula) then exit;

  if Page.HasCrossTabReport and UseVerticalBands then
  begin
    // cycle over all vertical bands.
    for I := 0 to Page.Bands.Count - 1 do
      if (Page.Bands[I].BandType = btvDetail) and
         (TprCustomVDetailBand(Page.Bands[I]).ParentDetail = nil) then
        Page.Bands[I].GenerateCell(Self, GenerateCellCallback);
        
    I := Page.Bands.IndexByBandType(btvSummary);
    if I <> -1 then
      Page.Bands[I].GenerateCell(nil, nil);
  end
  else
  begin
    Result := Page.Grid.AddCell(Self, nil);

    for I := 0 to Objects.Count - 1 do
      Objects[I].FirstPassProcessed := False;

    for I := 0 to Objects.Count - 1 do
      Objects[I].FirstPass;

    Result.SetCellHeight(CalcBandSize(FObjects));
    if (Result.FHorzVector <> nil) and ASecondPassNeeded then
      Result.FHorzVector.VisibleFormula := ASecondPassFormula;

    for I := 0 to Objects.Count - 1 do
    begin
      o := Objects[I].aRec.CreateCopy;
{$IFDEF PG}
      o.FPage := nil;
{$ENDIF}
      o.FObj := Objects[I];
{
      o.FpRect.Left := o.X;
      o.FpRect.Right := o.X + o.DX;
      o.FpRect.Top := o.Y;
      o.FpRect.Bottom := o.Y + o.DY;
}
      Result.Add(o);
    end;
  end;
  Page.Grid.EndLine;
end;

procedure TprCustomHBand.GenerateCellCallback(Sender: TprBand; Cell: TprGenCell; Objects: TprObjs);
begin
  Cell.SetCellHeight(CalcBandSize(Objects));
end;

procedure TprCustomHBand.BeginSection;
begin
  Page.Grid.BeginHorzSection;
end;

procedure TprCustomHBand.EndSection; 
begin
  Page.Grid.EndHorzSection;
end;

//////////////////////////
//
// TprCustomHTitleBand
//
//////////////////////////

/////////////////////////
//
// TprCustomHSummaryBand
//
/////////////////////////
procedure TprCustomHSummaryBand.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FPrintWithBand) then
    FPrintWithBand := nil;
end;

procedure TprCustomHSummaryBand.FillGenInfo(Info: TprGenBandInfo);
begin
  inherited;
  if PrintWithBand <> nil then
    Info.LinkToBand := PrintWithBand;
end;

/////////////////////////////////////////////////
//
// TprCustomHPageHeaderBand
//
/////////////////////////////////////////////////
procedure TprCustomHPageHeaderBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1: PrintOnFirstPage := not PrintOnFirstPage;
end;
DsgnNotifyDesigner;
end;

procedure TprCustomHPageHeaderBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
  inherited;
  if Popup.Items.Count>0 then
    AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
  AddPopupMenuItem(Popup,nil,sPrintOnFirstPage,'',OnDsgnPopupMenuClick,'',-1,true,PrintOnFirstPage);
end;

/////////////////////////////
//
// TprCustomHPageFooterBand
//
/////////////////////////////
constructor TprCustomHPageFooterBand.Create(AOwner: TComponent);
begin
  inherited;
  FPrintOnLastPage := True;
end;

procedure TprCustomHPageFooterBand.CalcdPageRect;
begin
if FCalced then exit;
FdPageRect := Rect(CurPageRect.Left,CurPageRect.Bottom-Height,CurPageRect.Right,CurPageRect.Bottom);
CurPageRect.Bottom := CurPageRect.Bottom-Height;
FCalced := true;
end;

procedure TprCustomHPageFooterBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1: PrintOnFirstPage := not PrintOnFirstPage;
  -2: PrintAfterLastBandOnPage := not PrintAfterLastBandOnPage;
  -3: PrintOnLastPage := not PrintOnLastPage;
end;
DsgnNotifyDesigner;
end;

procedure TprCustomHPageFooterBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
  inherited;
  if Popup.Items.Count>0 then
    AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
  AddPopupMenuItem(Popup,nil,sPrintOnFirstPage,'',OnDsgnPopupMenuClick,'',-1,true,PrintOnFirstPage);
  AddPopupMenuItem(Popup,nil,sPrintOnLastPage,'',OnDsgnPopupMenuClick,'',-3,true,PrintOnLastPage);
  AddPopupMenuItem(Popup,nil,sPrintAfterLastBandOnPage,'',OnDsgnPopupMenuClick,'',-2,true,PrintAfterLastBandOnPage);
end;

//////////////////////////
//
// TprCustomHDetailBand
//
//////////////////////////
constructor TprCustomHDetailBand.Create;
begin
  inherited;
  FDataset := TprDatasetLink.Create;
  FBands := TprBands.Create;
  FGroups := TprGroups.Create;
end;

destructor TprCustomHDetailBand.Destroy;
begin
  FBands.Free;
  FBands := nil;
  FGroups.Free;
  FGroups := nil;
  FDataset.Free;
  inherited;
end;

procedure TprCustomHDetailBand.Notification;
begin
  if (Operation = opRemove) and (AComponent <> Self) then
  begin
    if AComponent is TprGroup then
      Groups.Remove(AComponent);
    if AComponent is TprBand then
      Bands.Remove(AComponent);
    if AComponent = FParentDetail then
      FParentDetail := nil;
    if AComponent = FPrintWithChildDetail then
      FPrintWithChildDetail := nil;
  end;
  inherited;
end;

procedure TprCustomHDetailBand.DefineProperties;
begin
inherited;
Filer.DefineProperty('Groups',ReadGroups,WriteGroups,Groups.Count>0);
Filer.DefineProperty('Bands',ReadBands,WriteBands,Bands.Count>0);
end;

procedure TprCustomHDetailBand.ReadGroups;
begin
FGroupsNames := TStringList.Create;
prReadStringList(Reader,FGroupsNames);
end;

procedure TprCustomHDetailBand.WriteGroups;
begin
prWriteCompListNames(Writer,Groups);
end;

procedure TprCustomHDetailBand.ReadBands;
begin
FBandsNames := TStringList.Create;
prReadStringList(Reader,FBandsNames);
end;

procedure TprCustomHDetailBand.WriteBands;
begin
prWriteCompListNames(Writer,Bands);
end;

procedure TprCustomHDetailBand.GroupRemoved;
begin
Groups.Remove(Group);
end;

procedure TprCustomHDetailBand.GroupAdded;
begin
if Groups.IndexOf(Group)=-1 then
  Groups.Add(Group);
end;

procedure TprCustomHDetailBand.AfterReportLoaded;
var
  i,j : integer;
begin
inherited;
if FGroupsNames<>nil then
  begin
    Groups.Clear;
    for i:=0 to FGroupsNames.Count-1 do
      begin
        j:=Report.Groups.IndexByName(FGroupsNames[i]);
        if j<>-1 then
          Groups.Add(Report.Groups[j]);
      end;
    FGroupsNames.Free;
  end;
if FBandsNames<>nil then
  begin
    Bands.Clear;
    for i:=0 to FBandsNames.Count-1 do
      begin
        j:=Page.Bands.IndexByName(FBandsNames[i]);
        if j<>-1 then
          Bands.Add(Page.Bands[j]);
      end;
    FBandsNames.Free;
  end;
end;

procedure TprCustomHDetailBand.SetParentDetail;
begin
if Value=FParentDetail then exit;
if (FParentDetail<>nil) and (FParentDetail.Bands<>nil) then
  FParentDetail.Bands.Remove(Self);
FParentDetail:=Value;
if (FParentDetail<>nil) and (FParentDetail.Bands<>nil) and (FParentDetail.Bands.IndexOf(Self)=-1) then
  FParentDetail.Bands.Add(Self);
end;

procedure TprCustomHDetailBand.SetPrintWithChildDetail(Value: TprCustomHDetailBand);
begin
  FPrintWithChildDetail := Value;
end;

procedure TprCustomHDetailBand.SetValid(Value : string);
begin
FValid := Trim(Value);
end;

procedure TprCustomHDetailBand.FillGenInfo(Info : TprGenBandInfo);
begin
  inherited;
  with TprGenHorzBandInfo(Info) do
  begin
    UseColumns := Self.ColCount>1;
    ColCount := Self.ColCount;
    ColDirection := Self.ColDirection;
    StartNewPage := Self.StartNewPage;
    if Self.PrintWithChildDetail <> nil then
      LinkedBand := PrintWithChildDetail;
  end;
end;

function TprCustomHDetailBand.GenerateCell;
var
  I: integer;
  Cell: TprGenCell;
  fGroupEnded: Boolean;
begin
  Result := nil;

  InitDataSet; // !!!

  BeginSection;
  try
    for I := 0 to Bands.Count - 1 do
      if Bands[I].BandType = bthDetailHeader then
        Bands[I].GenerateCell(nil, nil);

    for I := 0 to Groups.Count - 1 do
      Groups[I].Reset;

    while not DataSet.Eof do
    begin
      if not Report.CalcBooleanFormula(FValid) then
        break;
      for I := 0 to Groups.Count - 1 do
        Groups[I].CalcValue;

      for I := Groups.Count - 1 downto 0 do
      begin
        fGroupEnded := Groups[I].GroupEnded;
        if fGroupEnded then
          DataSet.Prior;
        Groups[I].FootersGenerateCell(nil, nil);
        if fGroupEnded then
          DataSet.Next;
      end;
      for I := 0 to Groups.Count - 1 do
        Groups[I].HeadersGenerateCell(nil, nil);

      Cell := inherited GenerateCell(nil, nil);

      Report.DoOnDataSetNext(DataSet, Self, nil, Cell);

      for I := 0 to Bands.Count - 1 do
        if Bands[I].BandType in [bthDetail] then
          Bands[I].GenerateCell(nil, nil);

      DataSet.Next;
      Report.ADSRN_Next(DataSet);
    end;

    for I := Groups.Count - 1 downto 0 do
    begin
//    DataSet.Prior;
      Groups[i].FootersAlwaysGenerateCell(nil, nil);
//    DataSet.Next;
    end;

    for I := 0 to Bands.Count - 1 do
      if Bands[I].BandType = bthDetailFooter then
        Bands[I].GenerateCell(nil, nil);

    Report.DoOnDataSetEof(Self, DataSet);
    
  finally
    EndSection;
  end;
end;

procedure TprCustomHDetailBand.InitDataSet;
begin
Dataset.Dataset := Report.GetDataSetByName(DataSetName);
if Dataset.Dataset=nil then
  raise Exception.CreateFmt(prLoadStr(sDataSetNotFound),[DataSetName]);
if not Dataset.Active then
  Dataset.Open;
if Assigned(Report.OnInitDetailBandDataSet) then
  Report.OnInitDetailBandDataSet(Report,Self,DataSet.Dataset,DataSetName)
else
  DataSet.First;
Report.ADSRN_First(DataSet);
end;

procedure TprCustomHDetailBand.CalcdPageRect;
var
  i,j : integer;
begin
if FCalced then exit;

i:=Bands.IndexByBandType(bthDetailHeader);
if i<>-1 then
  Bands[i].CalcdPageRect(CurPageRect,nil);

for i:=0 to Groups.Count-1 do
  for j:=0 to Groups[i].Headers.Count-1 do
    Groups[i].Headers[j].CalcdPageRect(CurPageRect,nil);

inherited;

for i:=0 to Bands.Count-1 do
  if Bands[i].BandType in [bthDetail] then
    Bands[i].CalcdPageRect(CurPageRect,nil);

for i:=Groups.Count-1 downto 0 do
  for j:=0 to Groups[i].Footers.Count-1 do
    Groups[i].Footers[j].CalcdPageRect(CurPageRect,nil);

i:=Bands.IndexByBandType(bthDetailFooter);
if i<>-1 then
  Bands[i].CalcdPageRect(CurPageRect,nil);

FCalced:=true;
end;

procedure TprCustomHDetailBand.OnInsertIntoPage;
begin
end;

function TprCustomHDetailBand.GetDrawDesignCaption;
begin
  if ParentDetail=nil then
    Result := inherited GetDrawDesignCaption
  else
    Result := Format('%s (Parent : %s)',[inherited GetDrawDesignCaption,ParentDetail.Name]);
end;

procedure TprCustomHDetailBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
  case TMenuItem(Sender).Tag of
    -1: StartNewPage := not StartNewPage;
  end;
  DsgnNotifyDesigner;
end;

procedure TprCustomHDetailBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
  inherited;
  AddPopupMenuItem(Popup, nil, sStartNewPage, '', OnDsgnPopupMenuClick, '', -1, true, StartNewPage);
end;

////////////////////////////////
//
// TprCustomHDetailHeaderBand
//
////////////////////////////////
procedure TprCustomHDetailHeaderBand.Notification;
begin
if (Operation=opRemove) and (AComponent=FDetailBand) then
  FDetailBand := nil;
inherited;
end;

procedure TprCustomHDetailHeaderBand.SetDetailBand;
begin
if Value=FDetailBand then exit;
if (FDetailBand<>nil) and (FDetailBand.Bands<>nil) then
  FDetailBand.Bands.Remove(Self);
FDetailBand:=Value;
if (FDetailBand<>nil) and (FDetailBand.Bands<>nil) and (FDetailBand.Bands.IndexOf(Self)=-1) then
  FDetailBand.Bands.Add(Self);
end;

procedure TprCustomHDetailHeaderBand.FillGenInfo(Info: TprGenBandInfo);
begin
  inherited;
  with TprGenHorzBandInfo(Info) do
  begin
    UseColumns := Self.ColCount > 1;
    ColCount := Self.ColCount;
    ColDirection := Self.ColDirection;
    if LinkToDetail and (Self.DetailBand <> nil) then
      LinkedBand := Self.DetailBand;
    ReprintOnEachPage := Self.ReprintOnEachPage;
  end;
end;

function TprCustomHDetailHeaderBand.GetDrawDesignCaption;
begin
  if DetailBand=nil then
    Result := inherited GetDrawDesignCaption
  else
    Result := Format('%s (%s)',[inherited GetDrawDesignCaption,DetailBand.Name]);
end;

procedure TprCustomHDetailHeaderBand.OnInsertIntoPage(p: TprCustomPage);
begin
end;

procedure TprCustomHDetailHeaderBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1: ReprintOnEachPage := not ReprintOnEachPage;
  -2: LinkToDetail := not LinkToDetail;
end;
DsgnNotifyDesigner;
end;

procedure TprCustomHDetailHeaderBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
  inherited;
  AddPopupMenuItem(Popup,nil,sReprintOnEachPage,'',OnDsgnPopupMenuClick,'',-1,true,ReprintOnEachPage);
  AddPopupMenuItem(Popup,nil,sPrintWithDetail,'',OnDsgnPopupMenuClick,'',-2,true,LinkToDetail);
end;

////////////////////////////////
//
// TprCustomHDetailFooterBand
//
////////////////////////////////
procedure TprCustomHDetailFooterBand.Notification;
begin
inherited;
if (Operation=opRemove) and (AComponent=FDetailBand) then
  FDetailBand := nil;
end;

procedure TprCustomHDetailFooterBand.SetDetailBand;
begin
if Value=FDetailBand then exit;
if (FDetailBand<>nil) and (FDetailBand.Bands<>nil) then
  FDetailBand.Bands.Remove(Self);
FDetailBand := Value;
if (FDetailBand<>nil) and (FDetailBand.Bands<>nil) and (FDetailBand.Bands.IndexOf(Self)=-1) then
  FDetailBand.Bands.Add(Self);
end;

procedure TprCustomHDetailFooterBand.FillGenInfo(Info : TprGenBandInfo);
begin
  inherited;
  with TprGenHorzBandInfo(Info) do
  begin
    UseColumns := Self.ColCount>1;
    ColCount := Self.ColCount;
    ColDirection := Self.ColDirection;
    if LinkToDetail and (Self.DetailBand <> nil) then
      LinkToBand := Self.DetailBand;
  end;
end;

function TprCustomHDetailFooterBand.GetDrawDesignCaption;
begin
if DetailBand=nil then
  Result := inherited GetDrawDesignCaption
else
  Result := Format('%s (%s)',[inherited GetDrawDesignCaption,DetailBand.Name]);
end;

procedure TprCustomHDetailFooterBand.OnInsertIntoPage(p: TprCustomPage);
begin
end;

procedure TprCustomHDetailFooterBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1: LinkToDetail := not LinkToDetail;
end;
DsgnNotifyDesigner;
end;

procedure TprCustomHDetailFooterBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
  inherited;
  AddPopupMenuItem(Popup,nil,sPrintWithDetail,'',OnDsgnPopupMenuClick,'',-1,true,LinkToDetail);
end;

////////////////////////////////////
//
// TprCustomHGroupHeaderBand
//
////////////////////////////////////
constructor TprCustomHGroupHeaderBand.Create(AOwner: TComponent);
begin
  inherited;
  FMinDataRecords := 0;  
end;

procedure TprCustomHGroupHeaderBand.Notification;
begin
if (Operation=opRemove) and (AComponent=FGroup) then
  FGroup:=nil;

inherited;
end;

procedure TprCustomHGroupHeaderBand.SetGroup;
var
  i : integer;
begin
if Value=FGroup then exit;
if FGroup<>nil then
  begin
    FGroup.Headers.Remove(Self);
  end;
FGroup:=Value;
if FGroup<>nil then
  begin
    i:=FGroup.Headers.IndexOf(Self);
    if i=-1 then
      FGroup.Headers.Add(Self);
  end;
end;

function TprCustomHGroupHeaderBand.GetDrawDesignCaption;
begin
if (Group=nil) or (Group.DetailBand=nil) then
  Result:=inherited GetDrawDesignCaption
else
  Result:=Format('%s (%s,%s)',[inherited GetDrawDesignCaption,Group.Name,Group.DetailBand.Name]);
end;

procedure TprCustomHGroupHeaderBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
  case TMenuItem(Sender).Tag of
    -1: StartNewPage := not StartNewPage;
    -2: LinkToDetail := not LinkToDetail;
    -3: ReprintOnEachPage := not ReprintOnEachPage;
  end;
  DsgnNotifyDesigner;
end;

procedure TprCustomHGroupHeaderBand.OnInsertIntoPage(p: TprCustomPage);
begin
end;

procedure TprCustomHGroupHeaderBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
  inherited;
  AddPopupMenuItem(Popup, nil, sStartNewPage, '', OnDsgnPopupMenuClick, '', -1, True, StartNewPage);
  AddPopupMenuItem(Popup, nil, sPrintWithDetail, '', OnDsgnPopupMenuClick, '', -2, True, LinkToDetail);
  AddPopupMenuItem(Popup, nil, sReprintOnEachPage, '', OnDsgnPopupMenuClick, '', -3, True, ReprintOnEachPage);
end;

function TprCustomHGroupHeaderBand.GetSubBandSearchDirection: Integer;
begin
  Result := 1;
end;

procedure TprCustomHGroupHeaderBand.FillGenInfo(Info : TprGenBandInfo);
begin
  inherited;
  with TprGenHorzBandInfo(Info) do
  begin
    StartNewPage := Self.StartNewPage;
    UseColumns := Self.ColCount>1;
    ColCount := Self.ColCount;
    ColDirection := Self.ColDirection;
    if LinkToDetail and (Group <> nil) then
      LinkedBand := Group.DetailBand;
    ReprintOnEachPage := Self.ReprintOnEachPage;
    if (MinDataRecords > 0) and (Group <> nil) then
    begin
      MinSubBandCount := MinDataRecords;
      MinSubBand := Group.DetailBand;
    end; 
  end;
end;

////////////////////////////////////
//
// TprCustomHGroupFooterBand
//
////////////////////////////////////
constructor TprCustomHGroupFooterBand.Create(AOwner: TComponent);
begin
  inherited;
  FMinDataRecords := 0;  
end;

procedure TprCustomHGroupFooterBand.Notification;
begin
if (Operation=opRemove) and (AComponent=FGroup) then
  FGroup:=nil;
inherited;
end;

procedure TprCustomHGroupFooterBand.SetGroup;
var
  i : integer;
begin
if Value=FGroup then exit;
if FGroup<>nil then
  begin
    FGroup.Footers.Remove(Self);
  end;
FGroup:=Value;
if FGroup<>nil then
  begin
    i:=FGroup.Footers.IndexOf(Self);
    if i=-1 then
      FGroup.Footers.Add(Self);
  end;
end;

function TprCustomHGroupFooterBand.GetDrawDesignCaption;
begin
  if (Group = nil) or (Group.DetailBand = nil) then
    Result := inherited GetDrawDesignCaption
  else
    Result := Format('%s (%s,%s)',[inherited GetDrawDesignCaption, Group.Name, Group.DetailBand.Name]);
end;

procedure TprCustomHGroupFooterBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1: LinkToDetail := not LinkToDetail;
end;
DsgnNotifyDesigner;
end;

procedure TprCustomHGroupFooterBand.OnInsertIntoPage(p: TprCustomPage);
begin
end;

procedure TprCustomHGroupFooterBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
inherited;
AddPopupMenuItem(Popup,nil,sPrintWithDetail,'',OnDsgnPopupMenuClick,'',-1,true,LinkToDetail);
end;

function TprCustomHGroupFooterBand.GetSubBandSearchDirection: Integer;
begin
  Result := -1;
end;

procedure TprCustomHGroupFooterBand.FillGenInfo(Info : TprGenBandInfo);
begin
  inherited;
  with TprGenHorzBandInfo(Info) do
  begin
    UseColumns := Self.ColCount>1;
    ColCount := Self.ColCount;
    ColDirection := Self.ColDirection;
    if LinkToDetail and (Group <> nil) then
      LinkToBand := Group.DetailBand;
    if (MinDataRecords > 0) and (Group <> nil) then
    begin
      MinSubBand := Group.DetailBand;
      MinSubBandCount := MinDataRecords;
    end;
  end;
end;

/////////////////////////////////////////////////
//
// TprCustomVBand
//
/////////////////////////////////////////////////
function TprCustomVBand.GetSize: Integer;
begin
  Result := FWidth;
end;

procedure TprCustomVBand.GetObjectSizes(AObject: TprObj; var AGenPos, AGenSize, ADesignMin, ADesignMax: Integer);
begin
  with AObject do
  begin
    AGenPos := aRec.FpRect.Left;
    AGenSize := aRec.Width;

    ADesignMin := dRec.pRect.Left;
    ADesignMax := dRec.pRect.Right;
  end;
end;

procedure TprCustomVBand.CalcdPageRect;
begin
if FCalced then exit;
FdPageRect := Rect(CurPageRect.Left,CurPageRect.Top,CurPageRect.Left+Width,CurPageRect.Bottom);
CurPageRect.Left := CurPageRect.Left+Width;
FCalced := true;
end;

procedure TprCustomVBand.DsgnResize(oTop,oLeft,oBottom,oRight : integer; var ResizeAccepted : boolean; ExData : pointer);
begin
ResizeAccepted := dPageRect.Left-oLeft<dPageRect.Right+oRight;
if not ResizeAccepted then exit;
Width := dPageRect.Right+oRight-dPageRect.Left-oLeft;
Page.UpdateBandsPageRect;
end;

procedure TprCustomVBand.DsgnLink(Linked : TprDesignComponent; LinkMode : TprLinkType; var LinkAccepted : boolean; ExData : pointer);
begin
if Linked is TprObj then
  begin
    case LinkMode of
      ltRight : if ResizeObjs.IndexOf(Linked)=-1 then
                  ResizeObjs.Add(Linked);
    end;
    LinkAccepted := true;
  end;
end;

function TprCustomVBand.DsgnAllowResizeTypes : TprResizeTypeSet;
begin
Result := [ppRight,ppLeft];
end;

function TprCustomVBand.DsgnAllowLinkTypes : TprLinkTypeSet; 
begin
Result := [ltRight];
end;

procedure TprCustomVBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1: UseHorizontalBands := not UseHorizontalBands;
end;
DsgnNotifyDesigner;
end;

procedure TprCustomVBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
inherited;
if BandType in [btvPageHeader,btvPageFooter,bthPageHeader,bthPageFooter,bthTitle,bthSummary,btvTitle,btvSummary] then exit;
if Popup.Items.Count>0 then
  AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,nil,sUseHorizontalBands,'',OnDsgnPopupMenuClick,'',-1,true,UseHorizontalBands);
end;

function TprCustomVBand.GenerateCell(CallerBand : TprBand; CallbackProc : TGenerateCellCallbackProc) : TprGenCell;
var
  o : TprObjRec;
  HBand : TprCustomHBand;
  i, ACellWidth : integer;
  ASecondPassNeeded: Boolean;
  ASecondPassFormula: string;
begin
  Result := nil;
  if not Visible then exit;

  if not IsFormulaVisible(ASecondPassNeeded, ASecondPassFormula) then exit;

  if not UseHorizontalBands and not Page.Grid.IsEndOfLine then
  begin
    Page.Grid.SkipVerticalBand(Self);
    exit;
  end;

  if (CallerBand=nil) or not UseHorizontalBands then
    HBand := nil
  else
    HBand := CallerBand as TprCustomHBand;

Result := Page.Grid.AddCell(HBand,Self);
try
  // build the list of objects
  if (HBand <> nil) and UseHorizontalBands then
    for I := 0 to HBand.Objects.Count - 1 do
      if (HBand.Objects[I].dRec.pRect.Left + HBand.dPageRect.Left >= dPageRect.Left) and
         (HBand.Objects[I].dRec.pRect.Right + HBand.dPageRect.Left <= dPageRect.Right) then
        Objects.Add(HBand.Objects[i]);

  for I := 0 to Objects.Count - 1 do
    Objects[I].FirstPassProcessed := False;

  for I := 0 to Objects.Count - 1 do
    Objects[I].FirstPass;

  ACellWidth := CalcBandSize(FObjects);
  if (ResizeMode = prbrmMaxObj) and (HBand <> nil) and (Objects.Count > 0) then
    ACellWidth := ACellWidth - dPageRect.Left + Objects[0].Band.dPageRect.Left;
  Result.SetCellWidth(ACellWidth);

  if (Result.FVertVector <> nil) and ASecondPassNeeded then
    Result.FVertVector.VisibleFormula := ASecondPassFormula;

  if Assigned(CallBackProc) then
    CallBackProc(Self,Result,Objects);

  for i:=0 to Objects.Count-1 do
    begin
      o := Objects[i].aRec.CreateCopy;
{$IFDEF PG}
      o.FPage := nil;
{$ENDIF}
      o.FObj := Objects[i];
{
      o.FpRect.Top := o.Y;
      o.FpRect.Bottom := o.Y+o.DY;
      o.FpRect.Left := o.X;
      o.FpRect.Right := o.X+o.DX;
}
      Result.Add(o);
    end;
finally
  if (HBand<>nil) and UseHorizontalBands then
    Objects.Clear
end;
end;

procedure TprCustomVBand.BeginSection;
begin
  Page.Grid.BeginVertSection;
end;

procedure TprCustomVBand.EndSection;
begin
  Page.Grid.EndVertSection;
end;











//////////////////////////
//
// TprCustomVTitleBand
//
//////////////////////////

/////////////////////////////////////////////////
//
// TprCustomVSummaryBand
//
/////////////////////////////////////////////////
procedure TprCustomVSummaryBand.Notification(AComponent : TComponent; AOperation : TOperation);
begin
inherited;
if (AOperation=opRemove) and (AComponent=FPrintWithBand) then
  FPrintWithBand := nil;
end;

procedure TprCustomVSummaryBand.FillGenInfo(Info : TprGenBandInfo);
begin
  inherited;
  if PrintWithBand <> nil then
    Info.LinkToBand := PrintWithBand;
end;

/////////////////////////////////////////////////
//
// TprCustomVPageHeaderBand
//
/////////////////////////////////////////////////
procedure TprCustomVPageHeaderBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1: PrintOnFirstPage := not PrintOnFirstPage;
end;
DsgnNotifyDesigner;
end;

procedure TprCustomVPageHeaderBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
  inherited;
  if Popup.Items.Count>0 then
    AddPopupMenuItem(Popup, nil, 0, '', nil, '', 0, true, false);
  AddPopupMenuItem(Popup, nil, sPrintOnFirstPage, '', OnDsgnPopupMenuClick, '', -1, true, PrintOnFirstPage);
end;

/////////////////////////////
//
// TprCustomVPageFooterBand
//
/////////////////////////////
constructor TprCustomVPageFooterBand.Create(AOwner: TComponent);
begin
  inherited;
  FPrintOnLastPage := True;
end;

procedure TprCustomVPageFooterBand.CalcdPageRect;
begin
if FCalced then exit;
FdPageRect := Rect(CurPageRect.Right-Width,CurPageRect.Top,CurPageRect.Right,CurPageRect.Bottom);
CurPageRect.Right := CurPageRect.Right-Width;
FCalced := true;
end;

procedure TprCustomVPageFooterBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1: PrintOnFirstPage := not PrintOnFirstPage;
  -2: PrintAfterLastBandOnPage := not PrintAfterLastBandOnPage;
  -3: PrintOnLastPage := not PrintOnLastPage;
end;
DsgnNotifyDesigner;
end;

procedure TprCustomVPageFooterBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
  inherited;
  if Popup.Items.Count>0 then
    AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
  AddPopupMenuItem(Popup,nil,sPrintOnFirstPage,'',OnDsgnPopupMenuClick,'',-1,true,PrintOnFirstPage);
  AddPopupMenuItem(Popup,nil,sPrintOnLastPage,'',OnDsgnPopupMenuClick,'',-3,true,PrintOnLastPage);
  AddPopupMenuItem(Popup,nil,sPrintAfterLastBandOnPage,'',OnDsgnPopupMenuClick,'',-2,true,PrintAfterLastBandOnPage);
end;

//////////////////////////
//
// TprCustomVDetailBand
//
//////////////////////////
constructor TprCustomVDetailBand.Create;
begin
inherited;
FDataset := TprDatasetLink.Create;
FBands := TprBands.Create;
FGroups := TprGroups.Create;
end;

destructor TprCustomVDetailBand.Destroy;
begin
FBands.Free;
FBands := nil;
FGroups.Free;
FGroups := nil;
FDataset.Free;
inherited;
end;

procedure TprCustomVDetailBand.Notification;
begin
if (Operation=opRemove) and (AComponent<>Self) then
  begin
    if AComponent is TprGroup then
      Groups.Remove(AComponent);
    if AComponent is TprBand then
      Bands.Remove(AComponent);
    if AComponent=FParentDetail then
      FParentDetail:=nil;
  end;

inherited;
end;

procedure TprCustomVDetailBand.DefineProperties;
begin
inherited;
Filer.DefineProperty('Groups',ReadGroups,WriteGroups,Groups.Count>0);
Filer.DefineProperty('Bands',ReadBands,WriteBands,Bands.Count>0);
end;

procedure TprCustomVDetailBand.ReadGroups;
begin
FGroupsNames:=TStringList.Create;
prReadStringList(Reader,FGroupsNames);
end;

procedure TprCustomVDetailBand.WriteGroups;
begin
prWriteCompListNames(Writer,Groups);
end;

procedure TprCustomVDetailBand.ReadBands;
begin
FBandsNames:=TStringList.Create;
prReadStringList(Reader,FBandsNames);
end;

procedure TprCustomVDetailBand.WriteBands;
begin
prWriteCompListNames(Writer,Bands);
end;

procedure TprCustomVDetailBand.GroupRemoved;
begin
Groups.Remove(Group);
end;

procedure TprCustomVDetailBand.GroupAdded;
var
  i : integer;
begin
i:=Groups.IndexOf(Group);
if i=-1 then
  Groups.Add(Group);
end;

procedure TprCustomVDetailBand.AfterReportLoaded;
var
  i : integer;
begin
inherited;
if FGroupsNames<>nil then
  begin
    Groups.Clear;
    for i:=0 to FGroupsNames.Count-1 do
      Groups.Add(Report.Groups.ByName[FGroupsNames[i]]);
    FGroupsNames.Free;
  end;
if FBandsNames<>nil then
  begin
    Bands.Clear;
    for i:=0 to FBandsNames.Count-1 do
      Bands.Add(Page.Bands.ByName[FBandsNames[i]]);
    FBandsNames.Free;
  end;
end;

procedure TprCustomVDetailBand.SetParentDetail;
var
  i : integer;
begin
if Value=FParentDetail then exit;
if (FParentDetail<>nil) and (FParentDetail.Bands<>nil) then
  begin
    FParentDetail.Bands.Remove(Self);
  end;
FParentDetail:=Value;
if (FParentDetail<>nil) and (FParentDetail.Bands<>nil) then
  begin
    i:=FParentDetail.Bands.IndexOf(Self);
    if i=-1 then
      FParentDetail.Bands.Add(Self);
  end;
end;

procedure TprCustomVDetailBand.SetPrintWithChildDetail(Value: TprCustomVDetailBand);
begin
  FPrintWithChildDetail := Value;
end;

procedure TprCustomVDetailBand.SetValid(Value : string);
begin
FValid := Trim(Value);
end;

procedure TprCustomVDetailBand.InitDataSet;
begin
Dataset.Dataset:=Report.GetDataSetByName(DataSetName);
if Dataset.Dataset=nil then
  raise Exception.CreateFmt(prLoadStr(sDataSetNotFound),[DataSetName]);
if not Dataset.Active then
  Dataset.Open;
if Assigned(Report.OnInitDetailBandDataSet) then
  Report.OnInitDetailBandDataSet(Report,Self,DataSet.Dataset,DataSetName)
else
  DataSet.First;
Report.ADSRN_First(DataSet);
end;

procedure TprCustomVDetailBand.FillGenInfo(Info : TprGenBandInfo);
begin
  inherited;
  with TprGenVertBandInfo(Info) do
  begin
    StartNewPage := Self.StartNewPage;
    if Self.PrintWithChildDetail <> nil then
      LinkedBand := Self.PrintWithChildDetail;
  end;
end;

function TprCustomVDetailBand.GenerateCell(CallerBand: TprBand; CallbackProc: TGenerateCellCallbackProc) : TprGenCell;
var
  i : integer;
  Cell : TprGenCell;
  fGroupEnded : boolean;
begin
  Result := nil;

  InitDataset; // !!!

  BeginSection;
  try
    for I := 0 to Bands.Count - 1 do
      if Bands[I].BandType = btvDetailHeader then
        Bands[I].GenerateCell(CallerBand, CallbackProc);

    for I := 0 to Groups.Count - 1 do
      Groups[I].Reset;

    while not DataSet.Eof do
    begin
      if not Report.CalcBooleanFormula(FValid) then
        break;

      for I := 0 to Groups.Count - 1 do
        Groups[I].CalcValue;

      for I := Groups.Count - 1 downto 0 do
        begin
          fGroupEnded := Groups[I].GroupEnded;
          if fGroupEnded then
            DataSet.Prior;
          Groups[I].FootersGenerateCell(CallerBand, CallbackProc);
          if fGroupEnded then
            DataSet.Next;
        end;
      for I := 0 to Groups.Count - 1 do
        Groups[I].HeadersGenerateCell(CallerBand, CallbackProc);

      Cell := inherited GenerateCell(CallerBand, CallbackProc);

      if (CallerBand <> nil) and (CallerBand is TprCustomHBand) then
        Report.DoOnDataSetNext(DataSet,TprCustomHBand(CallerBand), Self, Cell)
      else
        Report.DoOnDataSetNext(DataSet, nil, Self, Cell);

      for I := 0 to Bands.Count - 1 do
        if Bands[I].BandType in [bthDetail] then
          Bands[I].GenerateCell(CallerBand, CallbackProc);

      DataSet.Next;
      Report.ADSRN_Next(DataSet);
    end;

    for I := Groups.Count - 1 downto 0 do
      Groups[i].FootersAlwaysGenerateCell(CallerBand, CallbackProc);

    for I := 0 to Bands.Count - 1 do
      if Bands[I].BandType = btvDetailFooter then
        Bands[I].GenerateCell(CallerBand, CallbackProc);

    Report.DoOnDataSetEof(Self, DataSet);
  finally
    EndSection;
  end;
end;

procedure TprCustomVDetailBand.CalcdPageRect;
var
  i,j : integer;
begin
if FCalced then exit;

i:=Bands.IndexByBandType(btvDetailHeader);
if i<>-1 then
  Bands[i].CalcdPageRect(CurPageRect,nil);

for i:=0 to Groups.Count-1 do
  for j:=0 to Groups[i].Headers.Count-1 do
    Groups[i].Headers[j].CalcdPageRect(CurPageRect,nil);

inherited;

for i:=0 to Bands.Count-1 do
  if Bands[i].BandType in [btvDetail] then
    Bands[i].CalcdPageRect(CurPageRect,nil);

for i:=0 to Groups.Count-1 do
  for j:=0 to Groups[i].Footers.Count-1 do
    Groups[i].Footers[j].CalcdPageRect(CurPageRect,nil);

i:=Bands.IndexByBandType(btvDetailFooter);
if i<>-1 then
  Bands[i].CalcdPageRect(CurPageRect,nil);

FCalced:=true;
end;

procedure TprCustomVDetailBand.OnInsertIntoPage(p: TprCustomPage);
begin
end;

function TprCustomVDetailBand.GetDrawDesignCaption;
begin
if ParentDetail=nil then
  Result := inherited GetDrawDesignCaption
else
  Result := Format('%s (Parent : %s)',[inherited GetDrawDesignCaption,ParentDetail.Name]);
end;

procedure TprCustomVDetailBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
  case TMenuItem(Sender).Tag of
    -1: StartNewPage := not StartNewPage;
  end;
  DsgnNotifyDesigner;
end;

procedure TprCustomVDetailBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
  inherited;
  AddPopupMenuItem(Popup, nil, sStartNewPage, '', OnDsgnPopupMenuClick, '', -1, true, StartNewPage);
end;

/////////////////////////////////////////////////
//
// TprCustomVDetailHeaderBand
//
/////////////////////////////////////////////////
procedure TprCustomVDetailHeaderBand.Notification;
begin
inherited;
if (Operation=opRemove) and (AComponent=FDetailBand) then
  FDetailBand := nil;
end;

procedure TprCustomVDetailHeaderBand.SetDetailBand;
begin
if Value=FDetailBand then exit;
if (FDetailBand<>nil) and (FDetailBand.Bands<>nil) then
  FDetailBand.Bands.Remove(Self);
FDetailBand:=Value;
if (FDetailBand<>nil) and (FDetailBand.Bands<>nil) and (FDetailBand.Bands.IndexOf(Self)=-1) then
  FDetailBand.Bands.Add(Self);
end;

procedure TprCustomVDetailHeaderBand.FillGenInfo(Info: TprGenBandInfo);
begin
  inherited;
  if LinkToDetail and (DetailBand <> nil) then
    Info.LinkedBand := DetailBand;
  Info.ReprintOnEachPage := Self.ReprintOnEachPage;
end;

procedure TprCustomVDetailHeaderBand.OnInsertIntoPage(p: TprCustomPage);
begin
end;

function TprCustomVDetailHeaderBand.GetDrawDesignCaption;
begin
if DetailBand=nil then
  Result := inherited GetDrawDesignCaption
else
  Result := Format('%s (%s)',[inherited GetDrawDesignCaption,DetailBand.Name]);
end;

procedure TprCustomVDetailHeaderBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1: ReprintOnEachPage := not ReprintOnEachPage;
  -2: LinkToDetail := not LinkToDetail;
end;
DsgnNotifyDesigner;
end;

procedure TprCustomVDetailHeaderBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
inherited;
AddPopupMenuItem(Popup,nil,sReprintOnEachPage,'',OnDsgnPopupMenuClick,'',-1,true,ReprintOnEachPage);
AddPopupMenuItem(Popup,nil,sPrintWithDetail,'',OnDsgnPopupMenuClick,'',-2,true,LinkToDetail);
end;

////////////////////////////////
//
// TprCustomVDetailFooterBand
//
////////////////////////////////
procedure TprCustomVDetailFooterBand.Notification;
begin
if (Operation=opRemove) and (AComponent=FDetailBand) then
  FDetailBand:=nil;

inherited;
end;

procedure TprCustomVDetailFooterBand.SetDetailBand;
begin
if Value=FDetailBand then exit;
if (FDetailBand<>nil) and (FDetailBand.Bands<>nil) then
  FDetailBand.Bands.Remove(Self);
FDetailBand := Value;
if (FDetailBand<>nil) and (FDetailBand.Bands<>nil) and (FDetailBand.Bands.IndexOf(Self)=-1) then
  FDetailBand.Bands.Add(Self);
end;

procedure TprCustomVDetailFooterBand.FillGenInfo(Info : TprGenBandInfo);
begin
  inherited;
  if LinkToDetail and (DetailBand <> nil) then
    Info.LinkToBand := DetailBand;
end;

procedure TprCustomVDetailFooterBand.OnInsertIntoPage(p: TprCustomPage);
begin
end;

function TprCustomVDetailFooterBand.GetDrawDesignCaption;
begin
if DetailBand=nil then
  Result := inherited GetDrawDesignCaption
else
  Result := Format('%s (%s)',[inherited GetDrawDesignCaption,DetailBand.Name]);
end;

procedure TprCustomVDetailFooterBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1: LinkToDetail := not LinkToDetail;
end;
DsgnNotifyDesigner;
end;

procedure TprCustomVDetailFooterBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
inherited;
AddPopupMenuItem(Popup,nil,sPrintWithDetail,'',OnDsgnPopupMenuClick,'',-1,true,LinkToDetail);
end;

////////////////////////////////////
//
// TprCustomVGroupHeaderBand
//
////////////////////////////////////
constructor TprCustomVGroupHeaderBand.Create(AOwner: TComponent);
begin
  inherited;
  FMinDataRecords := 0;  
end;

procedure TprCustomVGroupHeaderBand.Notification;
begin
inherited;
if (Operation=opRemove) and (AComponent=FGroup) then
  FGroup := nil;
end;

function TprCustomVGroupHeaderBand.GetSubBandSearchDirection: Integer;
begin
  Result := 1;
end;

procedure TprCustomVGroupHeaderBand.FillGenInfo(Info : TprGenBandInfo);
begin
  inherited;
  Info.StartNewPage := StartNewPage;
  if LinkToDetail and (Group <> nil) then
    Info.LinkedBand := Group.DetailBand;
  Info.ReprintOnEachPage := ReprintOnEachPage;
  if (MinDataRecords > 0) and (Group <> nil) then
  begin
    Info.MinSubBand := Group.DetailBand;
    Info.MinSubBandCount := MinDataRecords;
  end;
end;

procedure TprCustomVGroupHeaderBand.SetGroup;
begin
if Value=FGroup then exit;
if FGroup<>nil then
  FGroup.Headers.Remove(Self);
FGroup:=Value;
if (FGroup<>nil) and (FGroup.Headers.IndexOf(Self)=-1) then
  FGroup.Headers.Add(Self);
end;

procedure TprCustomVGroupHeaderBand.OnInsertIntoPage(p: TprCustomPage);
begin
end;

function TprCustomVGroupHeaderBand.GetDrawDesignCaption;
begin
if (Group=nil) or (Group.DetailBand=nil) then
  Result := inherited GetDrawDesignCaption
else
  Result := Format('%s (%s,%s)',[inherited GetDrawDesignCaption,Group.Name,Group.DetailBand.Name]);
end;

procedure TprCustomVGroupHeaderBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
  case TMenuItem(Sender).Tag of
    -1: StartNewPage := not StartNewPage;
    -2: LinkToDetail := not LinkToDetail;
    -3: ReprintOnEachPage := not ReprintOnEachPage;
  end;
  DsgnNotifyDesigner;
end;

procedure TprCustomVGroupHeaderBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
  inherited;
  AddPopupMenuItem(Popup, nil, sStartNewPage, '', OnDsgnPopupMenuClick, '', -1, True, StartNewPage);
  AddPopupMenuItem(Popup, nil, sPrintWithDetail, '', OnDsgnPopupMenuClick, '', -2, True, LinkToDetail);
  AddPopupMenuItem(Popup, nil, sReprintOnEachPage, '', OnDsgnPopupMenuClick, '', -3, True, ReprintOnEachPage);
end;

////////////////////////////////////
//
// TprCustomVGroupFooterBand
//
////////////////////////////////////
constructor TprCustomVGroupFooterBand.Create(AOwner: TComponent);
begin
  inherited;
  FMinDataRecords := 0;  
end;

procedure TprCustomVGroupFooterBand.Notification;
begin
if (Operation=opRemove) and (AComponent=FGroup) then
  FGroup:=nil;

inherited;
end;

procedure TprCustomVGroupFooterBand.SetGroup;
begin
if Value=FGroup then exit;
if FGroup<>nil then
  FGroup.Footers.Remove(Self);
FGroup := Value;
if (FGroup<>nil) and (FGroup.Footers.IndexOf(Self)=-1) then
  FGroup.Footers.Add(Self);
end;

function TprCustomVGroupFooterBand.GetSubBandSearchDirection: Integer;
begin
  Result := -1;
end;

procedure TprCustomVGroupFooterBand.FillGenInfo(Info : TprGenBandInfo);
begin
  inherited;
  if LinkToDetail and (Group <> nil) then
    Info.LinkToBand := Group.DetailBand;
  if (MinDataRecords > 0) and (Group <> nil) then
  begin
    Info.MinSubBand := Group.DetailBand;
    Info.MinSubBandCount := MinDataRecords;
  end;
end;

procedure TprCustomVGroupFooterBand.OnInsertIntoPage(p: TprCustomPage);
begin
end;

function TprCustomVGroupFooterBand.GetDrawDesignCaption;
begin
if (Group=nil) or (Group.DetailBand=nil) then
  Result := inherited GetDrawDesignCaption
else
  Result := Format('%s (%s,%s)',[inherited GetDrawDesignCaption,Group.Name,Group.DetailBand.Name]);
end;

procedure TprCustomVGroupFooterBand.OnDsgnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  -1: LinkToDetail := not LinkToDetail;
end;
DsgnNotifyDesigner;
end;

procedure TprCustomVGroupFooterBand.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
begin
inherited;
AddPopupMenuItem(Popup,nil,sPrintWithDetail,'',OnDsgnPopupMenuClick,'',-1,true,LinkToDetail);
end;


















//////////////////////////
//
// TprCustomEndPage
//
//////////////////////////
constructor TprCustomEndPage.CreateEmpty;
begin
  inherited Create;
  FReport := _Report;
  FoRecs := TList.Create;
end;

constructor TprCustomEndPage.Create;
begin
  CreateEmpty(_Page.Report);
end;

destructor TprCustomEndPage.Destroy;
begin
  FreeList(FoRecs);
  inherited;
end;

function TprCustomEndPage.oRecsCount;
begin
Result := FoRecs.Count;
end;

function TprCustomEndPage.GetoRec;
begin
Result:=TprObjRec(FoRecs[index]);
end;

procedure TprCustomEndPage.FreeGeneratedObjects;
begin
  FreeListItems(FoRecs);
  FoRecs.Clear;
end;

//////////////////////////
//
// TprCustomPage
//
//////////////////////////
constructor TprCustomPage.Create;
begin
inherited;
FVisible := true;
dPageRect := nil;
FBands := TprBands.Create;
FGrid := TprGenGrid.Create(Self);
Initialize(FOldEndPagesRects);
end;

destructor TprCustomPage.Destroy;
begin
FGrid.Free;
while FBands.Count>0 do
  FBands[0].Free;
FBands.Free;
Finalize(FOldEndPagesRects);
inherited;
end;

procedure TprCustomPage.SetParentComponent;
begin
Report:=Value as TprCustomReport;
end;

function TprCustomPage.HasParent;
begin
Result:=true;
end;

function TprCustomPage.GetParentComponent;
begin
Result:=Report;
end;

function TprCustomPage.GetChildOwner;
begin
Result:=Report;
end;

procedure TprCustomPage.GetChildren;
var
  i : integer;
begin
for i:=0 to Bands.Count-1 do
  Proc(Bands[i]);
end;

procedure TprCustomPage.Notification;
begin
if (Operation=opRemove) and (AComponent<>Self) then
  begin
    if AComponent is TprBand then
      Bands.Remove(AComponent);
  end;
end;

procedure TprCustomPage.SetReport;
begin
if FReport=Value then exit;
if FReport<>nil then
  FReport.FPages.Remove(Self);

FReport:=Value;

if (FReport<>nil) and (FReport.FPages.IndexOf(Self)=-1) then
  begin
    FReport.FPages.Add(Self);
    ReportSetted;
  end;
end;

function TprCustomPage.GetIndexInReport : integer;
begin
Result := Report.FPages.IndexOf(Self);
end;

procedure TprCustomPage.SetIndexInReport(Value : integer);
begin
if Value=GetIndexInReport then exit;
Report.FPages.Move(GetIndexInReport,Value);
end;

procedure TprCustomPage.ReportSetted;
begin
end;

function TprCustomPage.HasCrossTabReport;
var
  I: Integer;
begin
  Result := false;
  for I := 0 to Bands.Count - 1 do
    if (Bands[I].BandType in VerticalBands) and
       (TprCustomVBand(Bands[I]).UseHorizontalBands) then
    begin
      Result := True;
      exit;
    end;
end;

procedure TprCustomPage.FirstPassGenerateGrid;
var
  I: integer;
begin
  Grid.Clear;

  I := Bands.IndexByBandType(btvTitle);
  if I <> -1 then
    Bands[I].GenerateCell(nil, nil);

  I := Bands.IndexByBandType(bthTitle);
  if I <> -1 then
    Bands[I].GenerateCell(nil, nil);

  I := Bands.IndexByBandType(btvPageHeader);
  if I <> -1 then
    Bands[I].GenerateCell(nil, nil);

  I := Bands.IndexByBandType(bthPageFooter);
  if I <> -1 then
    Bands[I].GenerateCell(nil, nil);

  I := Bands.IndexByBandType(btvPageFooter);
  if I <> -1 then
    Bands[i].GenerateCell(nil, nil);

  I := Bands.IndexByBandType(bthPageHeader);
  if I <> -1 then
    Bands[I].GenerateCell(nil, nil);

  for I := 0 to Bands.Count - 1 do
    if (Bands[I].BandType = bthDetail) and
       (TprCustomHDetailBand(Bands[I]).ParentDetail = nil) then
      Bands[I].GenerateCell(nil, nil);

  I := Bands.IndexByBandType(bthSummary);
  if I <> -1 then
    Bands[I].GenerateCell(nil, nil);
end;

// is called for "normal" cells (report tiltes, report summaries, details, groups, etc)
procedure TprCustomPage.MoveCellObjects(AMainPage: TprCustomPage; EndPage: TprCustomEndPage; Cell: TprGenCell; LeftOffs, TopOffs: Integer);
var
  I: integer;
  o: TprObjRec;
begin
  Cell.FPage := EndPage;
  for I := 0 to Cell.ObjRecsCount - 1 do
  begin
    o := Cell.ObjRecs[I];
    if (Cell.VertVector <> nil) and (Cell.HorzVector <> nil) then
      OffsetRect(o.FpRect,
                 LeftOffs - Cell.VertVector.Band.dPageRect.Left + Cell.HorzVector.Band.dPageRect.Left{-o.pRect.Left},
                 TopOffs{+Cell.HorzVector.Band.dPageRect.Top}{-o.pRect.Top})
    else
      OffsetRect(o.FpRect, LeftOffs, TopOffs);

    o.FContainer := Self.Report; // will be used for calculate aggregate values in objects
    
    EndPage.FoRecs.Add(o);
  end;
  Cell.FObjRecs.Clear; // !!!
end;

// is called for page headers / footers
procedure TprCustomPage.CopyPageFootersHeadersObjects(AMainPage: TprCustomPage; EndPage: TprCustomEndPage; Cell: TprGenCell; LeftOffs, TopOffs: integer);
var
  I: Integer;
  o: TprObjRec;
begin
  Cell.FPage := EndPage;
  for I := 0 to Cell.ObjRecsCount - 1 do
  begin
    o := Cell.ObjRecs[I].CreateCopy;
    if (Cell.VertVector <> nil) and (Cell.HorzVector <> nil) then
      OffsetRect(o.FpRect,LeftOffs - o.pRect.Left, TopOffs - o.pRect.Top)
    else
      OffsetRect(o.FpRect, LeftOffs, TopOffs);

    o.FContainer := AMainPage.Report; // will be used for calculate aggregate values in objects

    EndPage.FoRecs.Add(o);
  end;
end;

procedure TprCustomPage.CheckAllParentReportTitles(var AOffset: Integer);
var
  APage: TprCustomPage;
begin
  APage := Self;
  repeat
    if APage.Grid.HorzTitle <> nil then
      AOffset := AOffset + APage.Grid.HorzTitle.Height;
    APage := APage.Report.ParentPage;
  until APage = nil;
end;

procedure TprCustomPage.GetCrossTabPageVertParams(AMainPage: TprCustomPage; IsMainLeft, IsLeft: Boolean; var Min, Max: Integer);
begin
  with AMainPage.GenPageRect do
  begin
    Min := Left;
    Max := Right;

    if (AMainPage.Grid.VertPageHeader <> nil) and
       (not IsLeft or TprCustomVPageHeaderBand(AMainPage.Grid.VertPageHeader.Band).PrintOnFirstPage) then
      Min := Min + AMainPage.Grid.VertPageHeader.Width;

    if (AMainPage.Grid.VertPageFooter <> nil) and
       (not IsLeft or TprCustomVPageFooterBand(AMainPage.Grid.VertPageFooter.Band).PrintOnFirstPage) then
      Max := Max - AMainPage.Grid.VertPageFooter.Width;

    if IsLeft and (AMainPage.Grid.VertTitle <> nil) then
      Min := Min + AMainPage.Grid.VertTitle.Width;
  end;
end;

procedure TprCustomPage.GetCrossTabPageHorzParams(AMainPage: TprCustomPage; IsMainTop, IsTop: Boolean; AParentReportTopOffset: Integer; var Min, Max: Integer);
begin
  with AMainPage.GenPageRect do
  begin
    Min := Top;
    Max := Bottom;

    if (AMainPage.Grid.HorzPageFooter <> nil) and
       (not IsTop or TprCustomHPageFooterBand(AMainPage.Grid.HorzPageFooter.Band).PrintOnFirstPage)then
      Max := Max - AMainPage.Grid.HorzPageFooter.Height;

    if AMainPage = Self then
    begin
      if (AMainPage.Grid.HorzPageHeader <> nil) and
         (not IsTop or TprCustomHPageHeaderBand(AMainPage.Grid.HorzPageHeader.Band).PrintOnFirstPage) then
        Min := Min + AMainPage.Grid.HorzPageHeader.Height;

      if IsTop and (AMainPage.Grid.HorzTitle <> nil) then
        Min := Min + AMainPage.Grid.HorzTitle.Height;
    end
    else
    begin
      if IsTop then
      begin
        Min := AParentReportTopOffset;
        if Self.Grid.HorzTitle <> nil then
          Min := Min + Self.Grid.HorzTitle.Height;
      end
      else
      begin
        if AMainPage.Grid.HorzPageHeader <> nil then
          Min := Min + AMainPage.Grid.HorzPageHeader.Height;
      end;
    end;
  end;
end;

//
// Used to determine the client endpage rect for non Cross-Tab reports.
// Parameters:
//   AMainPage - The main page (top parent page)
//   IsTop - true if endpage is first page of the report
//
function TprCustomPage.GetClientPageRect(AMainPage: TprCustomPage; IsMainTop, IsTop: Boolean; AParentReportTopOffset: Integer): TRect;
begin
  Result := AMainPage.GenPageRect; // the page sizes of main page is used

  if AMainPage.Grid.VertPageHeader <> nil then
    Result.Left := Result.Left + AMainPage.Grid.VertPageHeader.Width;

  if AMainPage.Grid.VertTitle <> nil then
    Result.Left := Result.Left + AMainPage.Grid.VertTitle.Width;
    
  if AMainPage.Grid.VertPageFooter <> nil then
    Result.Right := Result.Right - AMainPage.Grid.VertPageFooter.Width;

  if (AMainPage.Grid.HorzPageFooter <> nil) and
     (not IsTop or TprCustomHPageFooterBand(AMainPage.Grid.HorzPageFooter.Band).PrintOnFirstPage) then
    Result.Bottom := Result.Bottom - AMainPage.Grid.HorzPageFooter.Height;

  if AMainPage = Self then
  begin
    if (AMainPage.Grid.HorzPageHeader <> nil) and
       (not IsTop or TprCustomHPageHeaderBand(AMainPage.Grid.HorzPageHeader.Band).PrintOnFirstPage) then
      Result.Top := Result.Top + AMainPage.Grid.HorzPageHeader.Height;

    if IsTop and (AMainPage.Grid.HorzTitle <> nil) then
      Result.Top := Result.Top + AMainPage.Grid.HorzTitle.Height;
  end
  else
  begin
    if IsTop then
    begin
      Result.Top := AParentReportTopOffset;
      if Self.Grid.HorzTitle <> nil then
        Result.Top := Result.Top + Self.Grid.HorzTitle.Height;
    end
    else
    begin
      if AMainPage.Grid.HorzPageHeader <> nil then
        Result.Top := Result.Top + AMainPage.Grid.HorzPageHeader.Height;
    end;
  end;
end;

//
// The end page is added always to the top parent report (MainReport)
// Each report has protected ParentPage property that is returned the TprCustomPage object
// that calls this report
//
procedure TprCustomPage.SecondPassAddEndPage(AMainPage: TprCustomPage; AParentPages: TList; AParentReportTopOffset: Integer; IsMainTop, IsTop, IsMainLeft, IsLeft: Boolean; var LeftOffs, TopOffs: Integer);
var
  PriorPageNumber: Integer;
  AEndPage: TprCustomEndPage;
  IsFirst: Boolean;
begin
  IsFirst := Report.EndPagesCount <= 0;
  if IsFirst then
    PriorPageNumber := 0
  else
    PriorPageNumber := Report.EndPages[Report.EndPagesCount - 1].FPageNumber;
    
  Report.FIndexCurEndPage := Report.AddEndPage(AMainPage);
  AEndPage := Report.CurEndPage;
  AEndPage.FPageNumber := PriorPageNumber + 1;

  with AMainPage.GenPageRect do
  begin
    LeftOffs := Left;
    TopOffs := Top;
  end;

  // Vertical report title
  if IsLeft and (AMainPage.Grid.VertTitle <> nil) then
  begin
    if AMainPage = Self then
      MoveCellObjects(AMainPage,
                      AEndPage,
                      AMainPage.Grid.VertTitle,
                      LeftOffs,
                      AMainPage.Grid.VertTitle.Band.dPageRect.Top);
    LeftOffs := LeftOffs + AMainPage.Grid.VertTitle.Width;
  end;

  // Vertical page header
  if (AMainPage.Grid.VertPageHeader <> nil) and
     (not IsLeft or
      TprCustomVPageHeaderBand(AMainPage.Grid.VertPageHeader.Band).PrintOnFirstPage) then
  begin
    if (AMainPage = Self) or not IsFirst then
      CopyPageFootersHeadersObjects(AMainPage,
                                    AEndPage,
                                    AMainPage.Grid.VertPageHeader,
                                    LeftOffs,
                                    AMainPage.Grid.VertPageHeader.Band.dPageRect.Top);
    LeftOffs := LeftOffs + AMainPage.Grid.VertPageHeader.Width;
  end;


  if AMainPage = Self then
  begin
    if IsTop and (AMainPage.Grid.HorzTitle <> nil) then
    begin
      MoveCellObjects(AMainPage,
                      AEndPage,
                      AMainPage.Grid.HorzTitle,
                      AMainPage.Grid.HorzTitle.Band.dPageRect.Left,
                      TopOffs);
      TopOffs := TopOffs + AMainPage.Grid.HorzTitle.Height;
    end;

    if (AMainPage.Grid.HorzPageHeader <> nil) and
       (not IsMainTop or
        TprCustomHPageHeaderBand(AMainPage.Grid.HorzPageHeader.Band).PrintOnFirstPage) then
    begin
      CopyPageFootersHeadersObjects(AMainPage,
                                    AEndPage,
                                    AMainPage.Grid.HorzPageHeader,
                                    AMainPage.Grid.HorzPageHeader.Band.dPageRect.Left,
                                    TopOffs);
      TopOffs := TopOffs + AMainPage.Grid.HorzPageHeader.Height;
    end;
  end
  else
  begin
    // sub report
    if IsTop then
    begin
      TopOffs := AParentReportTopOffset;
      if Self.Grid.HorzTitle <> nil then
      begin
        MoveCellObjects(Self,
                        AEndPage,
                        Self.Grid.HorzTitle,
                        LeftOffs {Self.Grid.HorzTitle.Band.dPageRect.Left + AMainPage.GenPageRect.Left - Self.GenPageRect.Left} {!!!},
                        TopOffs);
        TopOffs := TopOffs + Self.Grid.HorzTitle.Height;
      end;
    end
    else
    begin
      if AMainPage.Grid.HorzPageHeader <> nil then
      begin
        CopyPageFootersHeadersObjects(AMainPage,
                                      AEndPage,
                                      AMainPage.Grid.HorzPageHeader,
                                      AMainPage.Grid.HorzPageHeader.Band.dPageRect.Left,
                                      TopOffs);
        TopOffs := TopOffs + AMainPage.Grid.HorzPageHeader.Height;
      end;
    end;
  end;
end;

procedure TprCustomPage.SecondPassPlaceGridOnEndPage(AMainPage: TprCustomPage;
                                                     AParentPage: TprCustomPage;
                                                     AStartPageIndex, ATopOffset: Integer);
type
  rCurrentColumnsPageInfo = record
    Index : integer;
    LeftOffs : integer;
    TopOffs : integer;
    PageRect : TRect;
    UseColumns : boolean;                //
    ColDirection : TprColDirectionType; // current column information
    ColCount : integer;                 //
    VertColStartOffs : integer;         // Top offset from which started vertical column
    VertColBottom : integer;            // maximum bottom vertical columns
    HorzColHeight : integer;            // Max height of vectors in horizontal column
  end;

  rCurrentCrossTabPageInfo = record
    Index : integer;
    Min : integer;
    Max : integer;
    Offs : integer;
  end;

var
  hv: TprGenHorzVector;
  vv: TprGenVertVector;
  cv: TprGenVector;
  ccpi: rCurrentColumnsPageInfo;
  ctpi: rCurrentCrossTabPageInfo;
  OldIndexCurEndPage, CurHorzIndex, CurVertIndex, CurTop, CurLeft, i, j, k, cvi, CellIndex: Integer;
  AEndPage: TprCustomEndPage;
  ARec: TprObjRec;

  AP: TprCustomPage;
  AParentPages: TList;

  function FindSplitPos(AVector: TprGenVector; AAvailableSize: Integer): Integer;
  var
    AHorzVector: Boolean;

    function FindNearestSplitPos(var ASplitPos: Integer): Boolean;
    var
      APos, ASize, I, J, ACurPos: Integer;
      ACell: TprGenCell;
      ARec: TprObjRec;
    begin
      Result := True;
      ACurPos := ASplitPos;
      for I := 0 to AVector.CellsCount - 1 do
      begin
        ACell := AVector[I];
        for J := 0 to ACell.ObjRecsCount - 1 do
        begin
          ARec := ACell.ObjRecs[J];
          if AHorzVector then
          begin
            APos := ARec.FpRect.Top;
            ASize := ARec.Height;
          end
          else
          begin
            APos := ARec.FpRect.Left;
            ASize := ARec.Width;
          end;
          if (APos < ASplitPos) and
             (APos + ASize > ASplitPos) and
             not (ARec.GetCanSplit(not AHorzVector, ASplitPos - APos)) then
          begin
            Result := False;
            if APos < ACurPos then
              ACurPos := APos;
          end;
        end;
      end;
      ASplitPos := ACurPos;
    end;

  begin
    AHorzVector := AVector is TprGenHorzVector;
    Result := AAvailableSize;
    while not FindNearestSplitPos(Result) do ;
  end;

  procedure InitCurrentPageInfo(Index: integer; IsMainTop, IsTop: Boolean);
  begin
    ccpi.Index := Index;
    ccpi.PageRect := GetClientPageRect(AMainPage, IsMainTop, IsTop, ATopOffset);
    ccpi.LeftOffs := ccpi.PageRect.Left;
    ccpi.TopOffs := ccpi.PageRect.Top;
    ccpi.UseColumns := false;
  end;

  function IsEqualColInfo(UseColumns: boolean; ColDirection: TprColDirectionType; ColCount: integer; hv: TprGenHorzVector): boolean;
  begin
    Result := (not UseColumns and not hv.Info.UseColumns) or
               ((hv.Info.UseColumns = UseColumns) and
               (hv.Info.ColDirection = ColDirection) and
               (hv.Info.ColCount = ColCount));
  end;

  procedure InitCurrentColInfo(hv: TprGenHorzVector);
  var
    i,j : integer;
  begin
    if ccpi.UseColumns then
    begin
      case ccpi.ColDirection of
        prcdTopBottomLeftRight:
          ccpi.TopOffs := ccpi.VertColBottom;
        prcdLeftRightTopBottom:
          ccpi.TopOffs := ccpi.TopOffs + ccpi.HorzColHeight;
      end;
      ccpi.LeftOffs := ccpi.PageRect.Left;
    end;

    ccpi.UseColumns := hv.Info.UseColumns;
    ccpi.ColDirection := hv.Info.ColDirection;
    ccpi.ColCount := Max(hv.Info.ColCount, 1);
    if ccpi.UseColumns then
    begin
      case ccpi.ColDirection of
        prcdTopBottomLeftRight:
          begin
            ccpi.VertColStartOffs := ccpi.TopOffs;
            ccpi.VertColBottom := ccpi.TopOffs;
          end;
        prcdLeftRightTopBottom:
          begin
            ccpi.HorzColHeight := 0;
            i := cvi;
            j := 1;
            while (i < Grid.HorzVectorsCount) and (j <= ccpi.ColCount) and IsEqualColInfo(hv.Info.UseColumns,hv.Info.ColDirection,hv.Info.ColCount,Grid.HorzVectors[i]) do
            begin
              if ccpi.HorzColHeight<Grid.HorzVectors[i].Size then
                ccpi.HorzColHeight := Grid.HorzVectors[i].Size;
              Inc(i);
              Inc(j);
            end;
          end;
      end;
    end;
  end;

  procedure PlaceBand(ABandVector: TprGenHorzVector);
  begin
    ABandVector.PageIndex := ccpi.Index;
    ABandVector.Left := ccpi.LeftOffs;
    ABandVector.Top := ccpi.TopOffs;
    Inc(cvi);
  end;

  // returns true if process is restarted from reprinted vector
  //  cvi - Specifies the index of the current vector
  //  ARestrartFromVector - if process is restarted then contains the vector from which it should be restarted.
  function InsertReprintedVectors(var ARestartFromVector: TprGenVector; AVectors: TList): Boolean;
  var
    I, AMinVectorIndex: Integer;
    AVector, AMinVector, ACurVector: TprGenVector;
  begin
    Result := False;
    if cvi <= 0 then
      exit;

    AMinVector := TprGenVector(AVectors[cvi]);
    ACurVector := AMinVector;
    AMinVectorIndex := cvi;

    for I := cvi - 1 downto 0 do
    begin
      AVector := TprGenVector(AVectors[I]);
      if (AVector.SectionLevel < AMinVector.SectionLevel) or
         ((AVector.SectionLevel = AMinVector.SectionLevel) and
          (AVector.SectionIndex = AMinVector.SectionIndex)) then
      begin
        AMinVector := AVector;
        AMinVectorIndex := I;
      end
      else
      begin
        if (AMinVector.SectionLevel < ACurVector.SectionLevel) and (AVector.SectionLevel >= AMinVector.SectionLevel) then
          break;
      end;
    end;

    for I := cvi - 1 downto AMinVectorIndex do
    begin
      AVector := TprGenVector(AVectors[I]);

      if AVector.GenInfo.ReprintOnEachPage then
      begin
        if (AVector.SectionLevel < ACurVector.SectionLevel) or
           ((AVector.SectionLevel = ACurVector.SectionLevel) and
            (AVector.SectionIndex = ACurVector.SectionIndex )) then
        begin
          // vector must be reprinted
          if AVector is TprGenHorzVector then
            ARestartFromVector := Grid.CopyHorzVectorToPos(TprGenHorzVector(AVector), cvi)
          else
            ARestartFromVector := Grid.CopyVertVectorToPos(TprGenVertVector(AVector), cvi);
          Result := True;
        end;
      end;
    end;
  end;

  function CheckLinkedVector(var ARestartFromVector: TprGenVector; AVectors: TList; APageIndex: Integer): Boolean;
  var
    I, AMinLinkVectorIndex, ALinkToVectorIndex: Integer;
    AMinLinkVector, AVector: TprGenVector;
  begin
    I := cvi;
    AMinLinkVectorIndex := MaxInt;
    AMinLinkVector := nil;
    while I < AVectors.Count do
    begin
      AVector := TprGenVector(AVectors[I]);

      if AVector.LinkToVector <> nil then
      begin
        ALinkToVectorIndex := AVectors.IndexOf(AVector.LinkToVector);
        if (ALinkToVectorIndex < cvi) and (AVector.LinkToVector.PageIndex < APageIndex) and (ALinkToVectorIndex < AMinLinkVectorIndex) then
        begin
          AMinLinkVector := AVector.LinkToVector;
          AMinLinkVectorIndex := ALinkToVectorIndex;
        end;
      end;

      Inc(I);
    end;

    Result := AMinLinkVector <> nil;
    if Result then
    begin
      while AMinLinkVector.LinkToVector <> nil do
        AMinLinkVector := AMinLinkVector.LinkToVector;
      ARestartFromVector := AMinLinkVector;
    end;
  end;

  // return TRUE if restart from another vector
  //  hv - current vector
  //  fNoPlace - true - There is no place on current page
  function StartNewHorzPage(var hv: TprGenHorzVector; fNoPlace: Boolean) : boolean;
  var
    ATemp: TprGenVector;
  begin
    Result := false;
    if hv.PassCount > 0 then exit; // !!!
    if not fNoPlace and
       ((cvi = 0) or
        (Grid.HorzVectors[cvi - 1].PageIndex < ccpi.Index)) {already in start of page} then exit;

    hv.PassCount := hv.PassCount + 1;
    InitCurrentPageInfo(ccpi.Index + 1, False, False);

    if CheckLinkedVector(ATemp, Grid.FHorzVectors, ccpi.Index) then
    begin
      hv := ATemp as TprGenHorzVector;
      cvi := Grid.FHorzVectors.IndexOf(hv);
      Result := True;
    end;

    if InsertReprintedVectors(ATemp, Grid.FHorzVectors) then
    begin
      hv := ATemp as TprGenHorzVector;
      Result := True;
    end;
      
    InitCurrentColInfo(hv);
  end;

  // return TRUE if StartNewPage and restart from linked vector
  // fNoPlace - true - There is no place on current page
  function StartNewColumn(var hv: TprGenHorzVector{; fNoPlace : boolean}): Boolean;
  var
    I, J: Integer;
  begin
    Result := false;
    case ccpi.ColDirection of
      prcdTopBottomLeftRight:
        begin
          ccpi.LeftOffs := ccpi.LeftOffs + (ccpi.PageRect.Right - ccpi.PageRect.Left) div ccpi.ColCount;
          if (ccpi.PageRect.Right - ccpi.LeftOffs >= (ccpi.PageRect.Right - ccpi.PageRect.Left) div ccpi.ColCount) and
             (ccpi.PageRect.Bottom - ccpi.VertColStartOffs>=hv.Size) then
            ccpi.TopOffs := ccpi.VertColStartOffs
          else
            Result := StartNewHorzPage(hv, True);
        end;
      prcdLeftRightTopBottom:
        begin
          ccpi.TopOffs := ccpi.TopOffs + ccpi.HorzColHeight;
          if ccpi.PageRect.Bottom - ccpi.TopOffs < ccpi.HorzColHeight then
            Result := StartNewHorzPage(hv, True)
          else
          begin
            ccpi.LeftOffs := ccpi.PageRect.Left;
            ccpi.HorzColHeight := 0;
            I := cvi;
            J := 1;
            while (I < Grid.HorzVectorsCount) and (j <= ccpi.ColCount) and IsEqualColInfo(hv.Info.UseColumns, hv.Info.ColDirection, hv.Info.ColCount, Grid.HorzVectors[I]) do
            begin
              if ccpi.HorzColHeight < Grid.HorzVectors[i].Size then
                ccpi.HorzColHeight := Grid.HorzVectors[i].Size;
              Inc(i);
              Inc(j);
            end;
          end;
        end;
    end;
  end;

  function StartNewColumnSplit(var AHorzVector: TprGenHorzVector; AAvailableSize: Integer): Boolean;
  var
    ASplitPos: Integer;
    ANewHorzVector: TprGenHorzVector;
  begin
    ANewHorzVector := nil;
    if AHorzVector.GenInfo.CanSplit then
    begin
      // try to split band
      ASplitPos := FindSplitPos(AHorzVector, AAvailableSize);
      if ASplitPos > 0 then
      begin
        ANewHorzVector := Grid.SplitHorzVector(AHorzVector, Grid.FHorzVectors.IndexOf(AHorzVector), ASplitPos);
        if ANewHorzVector <> nil then
        begin
          PlaceBand(ANewHorzVector);
          AHorzVector.PassCount := 0; // vector must be processed again
        end;
      end;
    end;

    Result := StartNewColumn(AHorzVector) or (ANewHorzVector <> nil);
  end;

  function StartNewHorzPageSplit(var AHorzVector: TprGenHorzVector; fNoPlace: boolean; AAvailableSize: Integer): Boolean;
  var
    ASplitPos: Integer;
    ANewHorzVector: TprGenHorzVector;
  begin
    ANewHorzVector := nil;
    if AHorzVector.GenInfo.CanSplit then
    begin
      // try to split band
      ASplitPos := FindSplitPos(AHorzVector, AAvailableSize);
      if ASplitPos > 0 then
      begin
        ANewHorzVector := Grid.SplitHorzVector(AHorzVector, Grid.FHorzVectors.IndexOf(AHorzVector), ASplitPos);
        if ANewHorzVector <> nil then
        begin
          PlaceBand(ANewHorzVector);
          AHorzVector.PassCount := 0; // vector must be processed again
        end;
      end;
    end;

    Result := StartNewHorzPage(AHorzVector, fNoPlace) or (ANewHorzVector <> nil);
  end;

  procedure ChangePageNumber(cep: TprCustomEndPage; Vector: TprGenVector);
  begin
    // ??? change page number depends from info in Vector.GenInfo
    case Vector.GenInfo.ChangePageNumberMode of
      prcpnOffset: cep.FPageNumber := cep.FPageNumber + Vector.GenInfo.ChangePageNumberValue;
      prcpnSetTo: cep.FPageNumber := Vector.GenInfo.ChangePageNumberValue;
    end;
    if Vector.GenInfo.ResetPagesCount then
      cep.ResetPagesCount := True;
  end;

  procedure GetCrossTabPageVertParams(IsMainLeft, IsLeft: Boolean; var Min, Max: Integer);
  begin
    Self.GetCrossTabPageVertParams(AMainPage, IsMainLeft, IsLeft, Min, Max);
  end;

  procedure GetCrossTabPageHorzParams(IsMainTop, IsTop: Boolean; var Min, Max: Integer);
  begin
    Self.GetCrossTabPageHorzParams(AMainPage, IsMainTop, IsTop, ATopOffset, Min, Max);
  end;

  procedure PlaceCrossTabBandVert(ABandVector: TprGenVector);
  begin
    ABandVector.PageIndex := ctpi.Index;
    ABandVector.Left := ctpi.Offs;
    Inc(cvi);
  end;

  procedure PlaceCrossTabBandHorz(ABandVector: TprGenVector);
  begin
    ABandVector.PageIndex := ctpi.Index;
    ABandVector.Top := ctpi.Offs;
    Inc(cvi);
  end;

  function StartNewCrossTabPageVert(var Vector: TprGenVector; fNoPlace: Boolean): Boolean;
  begin
    Result := false;
    if Vector.PassCount > 0 then exit;
    if not fNoPlace and
       ((cvi = 0) or
        (Grid.VertVectors[cvi - 1].PageIndex < ctpi.Index)) {already in start of page} then exit;

    Vector.PassCount := Vector.PassCount + 1;
    GetCrossTabPageVertParams(false, false, ctpi.Min, ctpi.Max);
    ctpi.Index := ctpi.Index + 1;
    ctpi.Offs := ctpi.Min;

    if CheckLinkedVector(Vector, Grid.FVertVectors, ctpi.Index) then
    begin
      cvi := Grid.FVertVectors.IndexOf(Vector);
      Result := True;
    end;

    if InsertReprintedVectors(Vector, Grid.FVertVectors) then
      Result := True;
  end;

  function StartNewCrossTabPageVertSplit(var Vector: TprGenVector; fNoPlace: boolean; AAvailableSize: Integer): Boolean;
  var
    ASplitPos: Integer;
    ANewVertVector: TprGenVertVector;
  begin
    ANewVertVector := nil;
    if Vector.GenInfo.CanSplit then
    begin
      // try to split band
      ASplitPos := FindSplitPos(Vector, AAvailableSize);
      if ASplitPos > 0 then
      begin
        ANewVertVector := Grid.SplitVertVector(Vector as TprGenVertVector, Grid.FVertVectors.IndexOf(Vector), ASplitPos);
        if ANewVertVector <> nil then
        begin
          PlaceCrossTabBandVert(ANewVertVector);
          Vector.PassCount := 0;
        end;
      end;
    end;

    Result := StartNewCrossTabPageVert(Vector, fNoPlace) or (ANewVertVector <> nil);
  end;

  function StartNewCrossTabPageHorz(var Vector: TprGenVector; fNoPlace: Boolean): Boolean;
  begin
    Result := false;
    if Vector.PassCount > 0 then exit;
    if not fNoPlace and
       ((cvi = 0) or
        (Grid.HorzVectors[cvi - 1].PageIndex < ctpi.Index)) {already in start of page} then exit;
    Vector.PassCount := Vector.PassCount + 1;
    GetCrossTabPageHorzParams(false, false, ctpi.Min, ctpi.Max);
    ctpi.Index := ctpi.Index + 1;
    ctpi.Offs := ctpi.Min;

    if CheckLinkedVector(Vector, Grid.FHorzVectors, ctpi.Index) then
    begin
      cvi := Grid.FHorzVectors.IndexOf(Vector);
      Result := True;
    end;

    if InsertReprintedVectors(Vector, Grid.FHorzVectors) then
      Result := True;
  end;

  function StartNewCrossTabPageHorzSplit(var Vector: TprGenVector; fNoPlace: boolean; AAvailableSize: Integer): Boolean;
  var
    ASplitPos: Integer;
    ANewHorzVector: TprGenHorzVector;
  begin
    ANewHorzVector := nil;
    if Vector.GenInfo.CanSplit then
    begin
      // try to split band
      ASplitPos := FindSplitPos(Vector, AAvailableSize);
      if ASplitPos > 0 then
      begin
        ANewHorzVector := Grid.SplitHorzVector(Vector as TprGenHorzVector, Grid.FHorzVectors.IndexOf(Vector), ASplitPos);
        if ANewHorzVector <> nil then
        begin
          PlaceCrossTabBandHorz(ANewHorzVector);
          Vector.PassCount := 0;
        end;
      end;
    end;

    Result := StartNewCrossTabPageHorz(Vector, fNoPlace) or (ANewHorzVector <> nil);
  end;

  procedure PlaceVertPageFooters(EndPage: TprCustomEndPage; IsLeft: boolean; LeftOffs: integer; IsRight: Boolean);
  begin
    if (AMainPage.Grid.VertPageFooter <> nil) and
       (not IsLeft or
        TprCustomVPageFooterBand(AMainPage.Grid.VertPageFooter.Band).PrintOnFirstPage) and
       (not IsRight or
        TprCustomVPageFooterBand(AMainPage.Grid.VertPageFooter.Band).PrintOnLastPage) then
    begin
      if TprCustomVPageFooterBand(AMainPage.Grid.VertPageFooter.Band).PrintAfterLastBandOnPage and
         (AMainPage.Grid.VertVectorsCount > 0) {only if CrossTab report} then
        CopyPageFootersHeadersObjects(AMainPage,
                                      EndPage,
                                      AMainPage.Grid.VertPageFooter,
                                      LeftOffs,
                                      AMainPage.Grid.VertPageFooter.Band.dPageRect.Top)
      else
        CopyPageFootersHeadersObjects(AMainPage,
                                      EndPage,
                                      AMainPage.Grid.VertPageFooter,
                                      GenPageRect.Right - AMainPage.Grid.VertPageFooter.Width,
                                      AMainPage.Grid.VertPageFooter.Band.dPageRect.Top);
    end;
  end;

  procedure PlaceHorzPageFooters(EndPage : TprCustomEndPage; IsTop : boolean; TopOffs : integer; IsBottom: Boolean);
  begin
    if (AMainPage.Grid.HorzPageFooter <> nil) and
       (not IsTop or
        TprCustomHPageFooterBand(AMainPage.Grid.HorzPageFooter.Band).PrintOnFirstPage) and
       (not IsBottom or
        TprCustomHPageFooterBand(AMainPage.Grid.HorzPageFooter.Band).PrintOnLastPage) then
    begin
      if TprCustomHPageFooterBand(AMainPage.Grid.HorzPageFooter.Band).PrintAfterLastBandOnPage then
        CopyPageFootersHeadersObjects(AMainPage,
                                      EndPage,
                                      AMainPage.Grid.HorzPageFooter,
                                      AMainPage.Grid.HorzPageFooter.Band.dPageRect.Left,
                                      TopOffs)
      else
        CopyPageFootersHeadersObjects(AMainPage,
                                      EndPage,
                                      AMainPage.Grid.HorzPageFooter,
                                      AMainPage.Grid.HorzPageFooter.Band.dPageRect.Left,
                                      GenPageRect.Bottom - AMainPage.Grid.HorzPageFooter.Height);
    end;
  end;

begin
  AParentPages := TList.Create;
  AP := Self;
  repeat
    AParentPages.Add(AP);
    AP := AP.Report.ParentPage;
  until AP = nil;

  try
    Grid.SecondPass;

    if Grid.VertVectorsCount > 0 then
    begin
      // cross tab report
      GetCrossTabPageVertParams(AMainPage = Self, true, ctpi.Min, ctpi.Max);
      ctpi.Index := 0;
      ctpi.Offs := ctpi.Min;
      cvi := 0;
      while cvi < Grid.VertVectorsCount do
      begin
        cv := Grid.VertVectors[cvi];
        if cv.GenInfo.StartNewPage then
          if StartNewCrossTabPageVert(cv, false) then continue;

        if ctpi.Max - ctpi.Offs < cv.Size then
          if StartNewCrossTabPageVertSplit(cv, true, ctpi.Max - ctpi.Offs) then continue;
  
        PlaceCrossTabBandVert(cv);
        ctpi.Offs := ctpi.Offs + cv.Size;

        if cv.GenInfo.BreakPage then
          StartNewCrossTabPageVert(cv, false);
      end;
  
      GetCrossTabPageHorzParams(AMainPage = Self, True, ctpi.Min, ctpi.Max);
      ctpi.Index := 0;
      ctpi.Offs := ctpi.Min;
      cvi := 0;
      while cvi < Grid.HorzVectorsCount do
      begin
        cv := Grid.HorzVectors[cvi];
        if cv.GenInfo.StartNewPage then
          if StartNewCrossTabPageHorz(cv, False) then continue;
  
        if ctpi.Max - ctpi.Offs < cv.Size then
          if StartNewCrossTabPageHorzSplit(cv, True, ctpi.Max - ctpi.Offs) then continue;
  
        PlaceCrossTabBandHorz(cv);
        ctpi.Offs := ctpi.Offs + cv.Size;
  
        if cv.GenInfo.BreakPage then
          StartNewCrossTabPageHorz(cv, false);
      end;
  
      // create end pages
      SecondPassAddEndPage(AMainPage,
                           AParentPages,
                           ATopOffset,
                           AMainPage = Self,
                           True,
                           AMainPage = Self,
                           True,
                           CurLeft,
                           CurTop);
      CurHorzIndex := AStartPageIndex;
      OldIndexCurEndPage := 0;
      for I := 0 to Grid.HorzVectorsCount - 1 do
        begin
          hv := Grid.HorzVectors[i];
          if CurHorzIndex <> hv.PageIndex then
            begin
              for j:=OldIndexCurEndPage to Report.EndPagesCount-1 do
                PlaceHorzPageFooters(Report.EndPages[j], CurHorzIndex = 0, CurTop, False);
              CurHorzIndex := hv.PageIndex;
              SecondPassAddEndPage(AMainPage,
                                   AParentPages,
                                   ATopOffset,
                                   false,
                                   false,
                                   AMainPage = Self,
                                   true,
                                   CurLeft,
                                   CurTop);
            end;
  
          ChangePageNumber(Report.CurEndPage,hv);
          CurTop := hv.Top+hv.Size;
  
          if not TprCustomHBand(hv.Band).UseVerticalBands then
            for k:=0 to hv.CellsCount-1 do
              MoveCellObjects(AMainPage,
                              AMainPage.Report.CurEndPage,
                              hv.Cells[k],
                              TprCustomHBand(hv.Band).dPageRect.Left,
                              hv.Top);
  
          OldIndexCurEndPage := Report.FIndexCurEndPage;
          CellIndex := 0;
          CurVertIndex := 0;
          for j:=0 to Grid.VertVectorsCount-1 do
            begin
              vv := Grid.VertVectors[j];
              if CurVertIndex<>vv.PageIndex then
                begin
                  CurVertIndex := vv.PageIndex;
                  if (i=0) or (Grid.HorzVectors[i-1].PageIndex<>hv.PageIndex) then
                    begin
                      PlaceVertPageFooters(Report.CurEndPage, CurVertIndex = 0, CurLeft, False);
                      SecondPassAddEndPage(AMainPage,
                                           AParentPages,
                                           ATopOffset,
                                           hv.PageIndex = 0,
                                           hv.PageIndex = 0,
                                           false,
                                           false,
                                           CurLeft,
                                           CurTop)
                    end
                  else
                    Report.FIndexCurEndPage := Report.FIndexCurEndPage+1;
                end;
  
              ChangePageNumber(Report.CurEndPage,vv);
              CurLeft := vv.Left+vv.Size;
  
              if TprCustomVBand(Grid.VertVectors[j].Band).UseHorizontalBands then
                begin
                  if TprCustomHBand(hv.Band).UseVerticalBands then
                    begin
                      if CellIndex < hv.CellsCount then
                        MoveCellObjects(AMainPage,
                                        AMainPage.Report.CurEndPage,
                                        hv.Cells[CellIndex],
                                        vv.Left,
                                        hv.Top);
                      Inc(CellIndex);
                    end;
                end
              else
                begin
                  for k:=0 to vv.CellsCount-1 do
                    MoveCellObjects(AMainPage,
                                    Report.CurEndPage,
                                    vv.Cells[k],
                                    vv.Left+vv.Band.dPageRect.Left-hv.Band.dPageRect.Left{!!!},
                                    TprCustomVBand(vv.Band).dPageRect.Top);
                end;
            end;
          if (i=0) or (Grid.HorzVectors[i-1].PageIndex<>hv.PageIndex) then
            PlaceVertPageFooters(Report.CurEndPage, CurVertIndex = 0, CurLeft, True);
          Report.FIndexCurEndPage := OldIndexCurEndPage;
        end;
      Report.FFinishedPositionOnLastPage := CurTop;
      
      if AMainPage = Self then
        for J := OldIndexCurEndPage to Report.EndPagesCount - 1 do
          PlaceHorzPageFooters(Report.EndPages[J], CurHorzIndex = 0, CurTop, True);
    end
  else
    begin
      // simple report, which can use columns
      if Grid.HorzVectorsCount > 0 then
      begin
        cvi := 0;
        InitCurrentPageInfo(0, AMainPage = Self, true);
        InitCurrentColInfo(Grid.HorzVectors[0]);
        while cvi < Grid.HorzVectorsCount do
        begin
          hv := Grid.HorzVectors[cvi];
          if hv.Info.StartNewPage then
            if StartNewHorzPage(hv, false) then continue;
  
          if not IsEqualColInfo(ccpi.UseColumns, ccpi.ColDirection, ccpi.ColCount, hv) then
            InitCurrentColInfo(hv);
  
          if hv.Info.UseColumns then
          begin
            if hv.Info.StartNewColumn then
              if StartNewColumn(hv) then continue;
  
            case hv.Info.ColDirection of
              prcdTopBottomLeftRight:
                begin
                  if ccpi.PageRect.Bottom - ccpi.TopOffs < hv.Size then
                    if StartNewColumnSplit(hv, ccpi.PageRect.Bottom - ccpi.TopOffs) then continue;
  
                  PlaceBand(hv);
                  ccpi.TopOffs := ccpi.TopOffs + hv.Size;
                  if ccpi.TopOffs > ccpi.VertColBottom then
                    ccpi.VertColBottom := ccpi.TopOffs;
                end;
              prcdLeftRightTopBottom:
                begin
                  if ccpi.PageRect.Right - ccpi.LeftOffs < (ccpi.PageRect.Right - ccpi.PageRect.Left) div hv.Info.ColCount then
                    if StartNewColumn(hv) then continue;
                  if ccpi.PageRect.Bottom - ccpi.TopOffs < ccpi.HorzColHeight then
                    if StartNewHorzPage(hv, True) then continue;
  
                  PlaceBand(hv);
                  ccpi.LeftOffs := ccpi.LeftOffs + ((ccpi.PageRect.Right - ccpi.PageRect.Left) div hv.Info.ColCount);
                end;
            end;
  
            if hv.Info.BreakColumn then
              StartNewColumn(hv);
          end
          else
          begin
            if ccpi.PageRect.Bottom - ccpi.TopOffs < hv.Size then
              if StartNewHorzPageSplit(hv, true, ccpi.PageRect.Bottom - ccpi.TopOffs) then continue;

            PlaceBand(hv);
  
            ccpi.TopOffs := ccpi.TopOffs + hv.Size;
  
            if hv.Info.SubReport <> nil then
            begin
              Report.DoOnBeginSubReportGenerate(hv.Info.SubReport, hv.Info.SubReportUserData);
              hv.Info.SubReport.InternalPrepareReport(AMainPage, // Self.Report or AParentGrid.Report
                                                      Self, // this grid or parent grid
                                                      ccpi.Index,
                                                      ccpi.TopOffs); // current offset from top
  
              hv.SubReportData := TprSubReportData.Create(hv.Info.SubReport, AMainPage.Report);
              AMainPage.Report.AddSubReportData(hv.SubReportData);
  
              if not hv.SubReportData.Empty then
              begin
                ccpi.Index := ccpi.Index + hv.SubReportData.EndPagesCount - 1; // index of current page.
                ccpi.TopOffs := hv.SubReportData.FinishedPositionOnLastPage; // position after last object on the last page of subreport
  
                ccpi.UseColumns := False; // reset columns
                ccpi.LeftOffs := ccpi.PageRect.Left;
              end;
            end;
          end;

          if hv.Info.BreakPage then
            StartNewHorzPage(hv, false);
        end;
      end;

      // create end pages and place objects on them
      SecondPassAddEndPage(AMainPage,
                           AParentPages,
                           ATopOffset,
                           AMainPage = Self,
                           true,
                           true,
                           true,
                           CurLeft,
                           CurTop);
                           
      CurHorzIndex := 0;
      for I := 0 to Grid.HorzVectorsCount - 1 do
      begin
        hv := Grid.HorzVectors[i];
        if CurHorzIndex<>hv.PageIndex then
        begin
          PlaceVertPageFooters(Report.CurEndPage, True, -1, False);
          PlaceHorzPageFooters(Report.CurEndPage, CurHorzIndex = 0, CurTop, False);
          CurHorzIndex := hv.PageIndex;
          SecondPassAddEndPage(AMainPage,
                               AParentPages,
                               ATopOffset,
                               false,
                               false,
                               true,
                               true,
                               CurLeft,
                               CurTop);
        end;
  
        ChangePageNumber(Report.CurEndPage,hv);
        CurTop := hv.Top + hv.Size;
  
        for j := 0 to hv.CellsCount - 1 do
          MoveCellObjects(AMainPage, Report.CurEndPage, hv.Cells[J], hv.Left, hv.Top);
  
        if hv.SubReportData <> nil then
        begin
          if not hv.SubReportData.Empty then
          begin
            with hv.SubReportData.EndPage[0] do
            begin
              // move objects from this page to the current page
              while FoRecs.Count > 0 do
              begin
                ARec := TprObjRec(FoRecs[0]);
                
                Self.Report.CurEndPage.FoRecs.Add(ARec);
                if ARec.Container = IprReportContainer(hv.Info.SubReport) then
                  ARec.FContainer := hv.SubReportData;
                FoRecs.Delete(0); // delete object from source page !!!
              end;
            end;
  
            // simple add all other pages to the Report
            K := Self.Report.CurEndPage.PageNumber + 1;
            while hv.SubReportData.EndPagesCount > 1 do
            begin
              AEndPage := hv.SubReportData.EndPage[1];
              AEndPage.PageNumber := K;
              AEndPage.FReport := Self.Report; // define the report that containing this page
              
              Self.Report.AddEndPageToList(AEndPage);
              Self.Report.FIndexCurEndPage := Self.Report.EndPagesCount - 1; // correct index

              for J := 0 to AEndPage.FoRecs.Count - 1 do
              begin
                ARec := TprObjRec(AEndPage.FoRecs[J]);
                if ARec.Container = IprReportContainer(hv.Info.SubReport) then
                  ARec.FContainer := hv.SubReportData;
              end;

              hv.SubReportData.DeleteEndPage(1); // delete end page from SubReportData
  
              Inc(K);
            end;
  
            CurHorzIndex := Report.FIndexCurEndPage;
            CurTop := hv.SubReportData.FinishedPositionOnLastPage; // Set CurTop to the position after last object on end page of subreport
          end;
  
          // frees SubReportData
        end;
      end;
      Report.FFinishedPositionOnLastPage := CurTop;

      if AMainPage = Self then
      begin
        PlaceVertPageFooters(Report.CurEndPage, true, -1, True);
        PlaceHorzPageFooters(Report.CurEndPage, CurHorzIndex = 0, CurTop, True);
      end;
    end;
  finally
    AParentPages.Free;
  end;
end;

procedure TprCustomPage.UpdateBandsPageRect;
var
  r : TRect;
  i,j : integer;
begin
for i:=0 to Bands.Count-1 do
  Bands[i].FCalced := false;

r := DsgnPageRect;
for i:=0 to MAX_PAGEBANDSORDER do
  for j:=0 to Bands.Count-1 do
    if Bands[j].BandType=PageBandsOrder[i] then
      Bands[j].CalcdPageRect(r,nil);

r := DsgnPageRect;
for i:=0 to MAX_PAGEBANDSORDERVERT do
  for j:=0 to Bands.Count-1 do
    if Bands[j].BandType=PageBandsOrderVert[i] then
      Bands[j].CalcdPageRect(r,nil);
end;

function TprCustomPage.InsertBand(BandType: TprBandType) : TprBand;
var
  BandClass : TprBandClass;
begin
  BandClass := Report.GetBandClass(BandType);
  if BandClass <> nil then
  begin
    Result := BandClass.Create(Report.prOwner);
    try
      Result.OnInsertIntoPage(Self);
      Result.Page := Self;
    except
      Result.Free;
      Result := nil;
    end;
  end
  else
    Result := nil;
end;

procedure TprCustomPage.DeleteBand(Band: TprBand);
begin
  Band.Free;
end;

///////////////////////////
//
// TprCustomReport
//
///////////////////////////
constructor TprCustomReport.Create;
begin
inherited;
FExportOptions := [preoShowParamsDlg, preoShowProgress, preoShowAfterGenerate];
Designer := nil;
FReportPrepared := false;
FCollate := false;
FCopies := 1;
FFromPage := -1;
FToPage := -1;
FExportFromPage := -1;
FExportToPage := -1;

FPreviewNotifyLinks := TList.Create;
FDesignerNotifyLinks := TList.Create; 
FPreviewUserDataList := TList.Create;
FGACList := TStringList.Create;
FEndPages := TList.Create;
FPages := TList.Create;
FPrintPagesList := TList.Create;

FGroups := TprGroups.Create;
FValues := TprValues.Create(TprValue);
FValues.FReport := Self;
FVariables := TprVariables.Create(TprVariable);

FSystemValues := TprValues.Create(TprValue);
FSystemValues.FReport := Self;

FParser := TprParser.Create(Self, FValues, FSystemValues);

  with TprValue(SystemValues.Add) do
  begin
    Name := 'PagesCount';
    ResetOn := rvtReport;
    CalcOn := cvtEventOnReset;
    OnGetVersionByVersionID := OnCalcPagesCount;
    OnCalc := OnCalcPagesCount2;
  end;
  with TprValue(SystemValues.Add) do
  begin
    Name := 'Page';
    ResetOn := rvtReport;
    CalcOn := cvtEventOnReset;
    OnGetVersionByVersionID := OnCalcPageNo;
    OnCalc := OnCalcPageNo2;
  end;

{$IFDEF PR_CB}
//if ObjectCount > 0 then
//  Objects[0].Name := Objects[0].Name;
{$ENDIF}

{$IFDEF PR_D4}
prCreatedReports.Add(Self);
{$ENDIF}
end;

destructor TprCustomReport.Destroy;
begin
if FProgressForm<>nil then
  FProgressForm.Free;
if FPreviewForm<>nil then
  FPreviewForm.Free;
if FDesignerForm<>nil then
  FDesignerForm.Free;

while PagesCount>0 do
  Pages[0].Free;

while Groups.Count>0 do
  Groups[0].Free;

ClearEndPages;  
FEndPages.Free;
FPages.Free;
FParser.Free;
FValues.Free;
SystemValues.Free;
FVariables.Free;
Groups.Free;
FGACList.Free;
FPrintPagesList.Free;
ClearPreviewUserDataList;
FPreviewUserDataList.Free;
FDesignerNotifyLinks.Free;
FPreviewNotifyLinks.Free;
{$IFDEF PR_D4}
prCreatedReports.Remove(Self);
{$ENDIF}
inherited;
end;

procedure TprCustomReport.Loaded;
var
  i,j,k : integer;
begin
inherited;

for i:=0 to PagesCount-1 do
  for j:=0 to Pages[i].Bands.Count-1 do
    for k:=0 to Pages[i].Bands[j].Objects.Count-1 do
      Pages[i].Bands[j].Objects[k].AfterReportLoaded;
for i:=0 to PagesCount-1 do
  for j:=0 to Pages[i].Bands.Count-1 do
    Pages[i].Bands[j].AfterReportLoaded;
end;

function TprCustomReport.GetChildOwner;
begin
Result:=Self;
end;

procedure TprCustomReport.GetChildren;
var
  i : integer;
begin
for i:=0 to PagesCount-1 do
  Proc(Pages[i]);
for i:=0 to Groups.Count-1 do
  Proc(Groups[i]);
end;

procedure TprCustomReport.Notification;
var
  i : integer;
begin
if (Operation=opRemove) and (AComponent<>Self) then
  begin
    if AComponent is TprGroup then
      begin
        Groups.Remove(AComponent);
        for i:=0 to AllValuesCount-1 do
          if AllValues[i].Group=AComponent then
            AllValues[i].Group:=nil;
      end;

    if AComponent is TprCustomPage then
      FPages.Remove(AComponent);
  end;

inherited;
end;

function TprCustomReport.GetprOwner;
begin
Result := Self;
end;

procedure TprCustomReport.DefineProperties;
begin
inherited;
Filer.DefineProperty('SystemInfo',ReadSystemInfo,WriteSystemInfo,true);
end;

procedure TprCustomReport.ReadSystemInfo;
begin
Reader.ReadListBegin;
while not Reader.EndOfList do
  Reader.ReadString;
Reader.ReadListEnd;
end;

procedure TprCustomReport.WriteSystemInfo;
const
  PlatformIDs : array [0..2] of string = ('WIN32s','WIN32_WINDOWS','WIN32_NT');
var
  s : string;
  si : SYSTEM_INFO;
  ovi : OSVERSIONINFO;
begin
  Writer.WriteListBegin;

  ZeroMemory(@ovi,sizeof(ovi));
  ovi.dwOSVersionInfoSize := sizeof(ovi);
  if GetVersionEx(ovi) then
  begin
    if ovi.dwPlatformId<=2 then
      s := PlatformIDs[ovi.dwPlatformId]
    else
      s := 'Unknown';
    Writer.WriteString(Format('OS: %s %d.%d.%d %s',[s,ovi.dwMajorVersion,ovi.dwMinorVersion,ovi.dwBuildNumber,StrPas(ovi.szCSDVersion)]));
    Writer.WriteString('');
  end;

  GetSystemInfo(si);
  Writer.WriteString(Format('PageSize: %d',[si.dwPageSize]));
  Writer.WriteString(Format('ActiveProcessorMask: $%4.4x',[si.dwPageSize]));
  Writer.WriteString(Format('NumberOfProcessors: %d',[si.dwNumberOfProcessors]));
  Writer.WriteString(Format('ProcessorType: %d',[si.dwProcessorType]));
  Writer.WriteString('');

  {$IFDEF PR_D4}
  Writer.WriteString('Compiler version: Delphi4');
  {$ENDIF}

  {$IFDEF PR_D5}
  {$IFDEF PR_CB5}
  Writer.WriteString('Compiler version: Builder5');
  {$ELSE}
  Writer.WriteString('Compiler version: Delphi5');
  {$ENDIF}
  {$ENDIF}

  {$IFDEF PR_D6}
  {$IFDEF PR_CB6}
  Writer.WriteString('Compiler version: Builder6');
  {$ELSE}
  Writer.WriteString('Compiler version: Delphi6');
  {$ENDIF}
  {$ENDIF}

  {$IFDEF PR_D7}
  Writer.WriteString('Compiler version: Delphi7');
  {$ENDIF}

  Writer.WriteString('PReport version: '+SPReportVersion);

  Writer.WriteListEnd;
end;

function TprCustomReport.FindObject(const AObjectName: string): TprObj;
begin
  Result := TprObj(FindComponent(AObjectName));
  if Result is TprObj then
    Result := TprObj(Result)
  else
    Result := nil;
end;

function TprCustomReport.ContainsObject(AReportObject: TObject): Boolean;
var
  I: Integer;
begin
  I := 0;
  while (I < ComponentCount) and (Components[I] <> AReportObject) do Inc(I);
  Result := I < ComponentCount;
end;

function TprCustomReport.FindBand(const ABandName: string): TprBand;
begin
  Result := TprBand(FindComponent(ABandName));
  if Result is TprBand then
    Result := TprBand(Result)
  else
    Result := nil;
end;

function TprCustomReport.CalcBooleanFormula(const AFormula: string): Boolean;
var
  S: string;
  V: TprVarValue;
begin
  if AFormula = '' then
    Result := true
  else
  begin
    S := AFormula;
    if not TprParser(FParser).Calc(S, V) then
      Exception.CreateFmt(prLoadStr(sOnePassCalcError), [AFormula]);
    Result := _vAsBoolean(V);
  end;
end;

function TprCustomReport.CalcBooleanFormula(var AFormula: string; var ASecondPassNeeded: Boolean): Boolean;
var
  V: TprVarValue;
begin
  Result := True;
  if AFormula = '' then
  begin
    ASecondPassNeeded := False;
  end
  else
  begin
    ASecondPassNeeded := not TprParser(FParser).Calc(AFormula, V);
    if not ASecondPassNeeded then
      Result := _vAsBoolean(V);
  end;
end;

procedure TprCustomReport.SetPrintPages(Value : string);
begin
if (FPrintPages=Value) or not CheckPageList(Value) then exit;
FPrintPages := Value;
TextToPageList(Value,FPrintPagesList);
end;

function TprCustomReport.GetAllValuesCount;
begin
Result := FValues.Count+SystemValues.Count;
end;

function TprCustomReport.GetAllValue;
begin
if i<SystemValues.Count then
  Result := SystemValues[i]
else
  Result := FValues[i-SystemValues.Count];
end;

function TprCustomReport.ADSRN_GetIndex(Dataset : TprDatasetLink) : integer;
begin
Result := 0;
while (Result<Length(ADSRN)) and (ADSRN[Result].DataSet<>DataSet) do Inc(Result);
if Result>=Length(ADSRN) then
  Result := -1;
end;

function TprCustomReport.ADSRN_GetIndex(Dataset : TObject) : integer;
begin
Result := 0;
while (Result<Length(ADSRN)) and (ADSRN[Result].DataSet.Dataset<>DataSet) do Inc(Result);
if Result>=Length(ADSRN) then
  Result := -1;
end;

procedure TprCustomReport.ADSRN_First;
var
  i : integer;
begin
i:=ADSRN_GetIndex(Dataset);
if i<>-1 then
  ADSRN[i].RecNo:=1
else
  begin
    SetLength(ADSRN,Length(ADSRN)+1);
    ADSRN[High(ADSRN)].DataSet:=DataSet;
    ADSRN[High(ADSRN)].RecNo  :=1;
  end;
end;

procedure TprCustomReport.ADSRN_Next;
var
  i : integer;
begin
i:=ADSRN_GetIndex(Dataset);
if (i<>-1) and (not ADSRN[i].DataSet.Eof) then
  ADSRN[i].RecNo:=ADSRN[i].RecNo+1
else
  begin
    SetLength(ADSRN,Length(ADSRN)+1);
    ADSRN[High(ADSRN)].DataSet:=DataSet;
    ADSRN[High(ADSRN)].RecNo  :=1;
  end;
end;

function  TprCustomReport.GetDataSetRecNo(Dataset : TprDatasetLink) : integer;
var
  i : integer;
begin
i:=ADSRN_GetIndex(Dataset);
if i<>-1 then
  Result:=ADSRN[i].RecNo
else
  Result:=-1;
end;

function  TprCustomReport.GetDataSetRecNo(Dataset : TObject) : integer;
var
  i : integer;
begin
i := ADSRN_GetIndex(Dataset);
if i<>-1 then
  Result := ADSRN[i].RecNo
else
  Result := -1;
end;

procedure TprCustomReport.OnCalcPagesCount(ValueVersion: TprValueVersion);
begin
  if CurEndPage = nil then
    ValueVersion.V := 0
  else
    ValueVersion.V := CurEndPage.PagesCount;
end;

procedure TprCustomReport.OnCalcPagesCount2(Value: TprValue);
begin
  Value.CurrentValue := 0;
end;

procedure TprCustomReport.OnCalcPageNo(ValueVersion: TprValueVersion);
begin
  if CurEndPage = nil then
    ValueVersion.V := 0
  else
    ValueVersion.V := CurEndPage.PageNumber;
end;

procedure TprCustomReport.OnCalcPageNo2(Value : TprValue);
begin
  Value.CurrentValue := 0;
end;

function TprCustomReport.GetPage;
begin
Result := TprCustomPage(FPages[index]);
end;

function TprCustomReport.GetEndPage;
begin
Result := TprCustomEndPage(FEndPages[index]);
end;

function TprCustomReport.GetPagesCount;
begin
Result := FPages.Count;
end;

function TprCustomReport.GetEndPagesCount;
begin
Result := FEndPages.Count;
end;

function TprCustomReport.GetCurEndPage;
begin
  if (IndexCurEndPage < 0) or (IndexCurEndPage >= FEndPages.Count) then
    Result := nil
  else
    Result := TprCustomEndPage(FEndPages[IndexCurEndPage]);
end;

function TprCustomReport.GetSubReportsCount: Integer;
begin
  if FSubReports = nil then
    Result := 0
  else
    Result := FSubReports.Count;
end;

function TprCustomReport.GetSubReport(I: Integer): TprSubReportData;
begin
  Result := TprSubReportData(FSubReports[I]);
end;

function TprCustomReport.AddEndPage(Page : TprCustomPage) : integer;
begin
Result := FEndPages.Add(CreateEndPage(Page));
FIndexCurEndPage := FEndPages.Count-1;
end;

procedure TprCustomReport.AddEndPageToList;
begin
FEndPages.Add(EndPage);
end;

procedure TprCustomReport.InsertEndPageIntoList;
begin
  FEndPages.Insert(i,EndPage);
end;

function TprCustomReport.EndPageIndex;
begin
Result := FEndPages.IndexOf(EndPage);
end;

procedure TprCustomReport.DeleteEndPage;
begin
  EndPages[index].Free;
  FEndPages.Delete(index);
end;

function TprCustomReport.Calc;
var
  v: TprVarValue;
begin
  if TprParser(FParser).Calc(Expression, v) then
    Result := _vAsVariant(v)
  else
    raise Exception.Create(prLoadStr(sErrorCalcExpressionInOnePass));
end;

function TprCustomReport.FormatTemplate;
begin
  Result:=TprParser(FParser).FormatTemplate(Template, Res);
end;

function TprCustomReport.FormatStrings(lSource, lDest: TStrings; DeleteEmptyLines, DeleteEmptyLinesAtEnd: Boolean): Boolean;
begin
  Result := TprParser(FParser).FormatStrings(lSource, lDest, DeleteEmptyLines, DeleteEmptyLinesAtEnd);
end;

procedure TprCustomReport.CreateProgressForm;
begin
FWindowList := DisableTaskWindows(GetActiveWindow);
FProgressForm := TprProgressForm.Create(Application);
FProgressForm.InitPF(Caption,Max);
end;

procedure TprCustomReport.CloseProgressForm;
begin
if FProgressForm=nil then exit;
EnableTaskWindows(FWindowList);
FProgressForm.Free;
FProgressForm := nil;
end;

procedure TprCustomReport.DoOnBandGenerateCell(HorzBandInfo : TprGenHorzBandInfo; VertBandInfo : TprGenVertBandInfo);
begin
if Assigned(OnBandGenerateCell) then
  OnBandGenerateCell(Self,HorzBandInfo,VertBandInfo);
end;

procedure TprCustomReport.DoOnDataSetNext(DataSet : TprDatasetLink; HorizontalBand : TprCustomHBand; VerticalBand : TprCustomVBand; Cell : TprGenCell);
var
  i : integer;
begin
for i:=0 to AllValuesCount-1 do
  begin
    if (AllValues[i].CalcOn=cvtDataSetNext) and (AllValues[i].Dataset.Dataset=DataSet.Dataset) then
      AllValues[i].Calculate(Cell);
    if (AllValues[i].CalcOn=cvtCrossTab) and (AllValues[i].CrossTabHorzDataSet.Dataset=DataSet.Dataset) then
      begin
        if (HorizontalBand<>nil) and (HorizontalBand.BandType in [bthDetail]) then
          AllValues[i].Calculate(Cell);
      end;
  end;
end;

procedure TprCustomReport.DoOnGroupEnd;
var
  i : integer;
begin
for i:=0 to AllValuesCount-1 do
  begin
    if (AllValues[i].ResetOn=rvtGroup) and (AllValues[i].Group=Group) then
      AllValues[i].Reset;
  end;
end;

procedure TprCustomReport.DoOnDataSetEof(Band : TprBand; DataSet : TprDatasetLink);
var
  i : integer;
  v : TprValue;
begin
for i:=0 to AllValuesCount-1 do
  begin
    v := AllValues[i];
    if (v.ResetOn=rvtDataSetEof) and (v.ResetDataSet.Dataset=DataSet.Dataset) then
      v.Reset;
    if (Band.BandType in VerticalBands) and (v.CalcOn=cvtDataSetNext) and (v.DataSet.Dataset=DataSet.DataSet) then
      v.Versions[v.VersionsCount-1].FNotUseInAccumulate := true; 

    if v.CalcOn=cvtCrossTab then // ??? çà÷åì ýòî íàäî
      begin
        if v.CrossTabHorzDataSet.Dataset=DataSet.Dataset then
          begin
            // Eof for horizontal dataset
          end;
      end;
  end;
end;

procedure TprCustomReport.DoOnPreviewMouseMove;
begin
if Assigned(FOnPreviewMouseMove) then
  FOnPreviewMouseMove(Self,PreviewUserData,cur,HighlightObject);
end;

procedure TprCustomReport.DoOnPreviewMouseDown;
begin
if Assigned(FOnPreviewMouseDown) then
  FOnPreviewMouseDown(Self,PreviewUserData,Shift);
end;

procedure TprCustomReport.DoOnPreviewDblClick;
begin
if Assigned(FOnPreviewDblClick) then
  FOnPReviewDblClick(Self,PreviewUserData);
end;

procedure TprCustomReport.DoOnPreviewGetUserData;
begin
PreviewUserData := nil;
if Assigned(FOnPreviewGetUserData) then
  begin
    FOnPreviewGetUserData(Self,Obj,ObjRec,PreviewUserData);
    if PreviewUserData<>nil then
      FPreviewUserDataList.Add(PreviewUserData);
  end;
end;

procedure TprCustomReport.DoOnTemplatePageGenerate(Page : TprCustomPage);
begin
  if Assigned(FOnTemplatePageGenerate) then
    FOnTemplatePageGenerate(Self,Page);
end;

procedure TprCustomReport.DoOnFirstPassObject;
begin
ManuallyProcessed := false;
if Assigned(FOnFirstPassObject) then
  FOnFirstPassObject(Self,Sender,ManuallyProcessed);
end;

procedure TprCustomReport.DoOnFoundPrintVariable(const AVariableName: string; var AVariableValue: string);
begin
  if Assigned(FOnFoundPrintVariable) then
    FOnFoundPrintVariable(Self, AVariableName, AVariableValue);
end;

procedure TprCustomReport.DoOnBeginSubReportGenerate(ASubReport: TprCustomReport; ASubReportUserData: Integer);
begin
  if Assigned(FOnBeginSubReportGenerate) then
    FOnBeginSubReportGenerate(Self, ASubReport, ASubReportUserData);
end;

procedure TprCustomReport.DoOnPrintComplete;
begin
  if Assigned(FOnPrintComplete) then
    FOnPrintComplete(Self);
end;

procedure TprCustomReport.DoOnPrintStart;
begin
  if Assigned(FOnPrintStart) then
    FOnPrintStart(Self);
end;

function TprCustomReport.GetPrintVariableValue(const AVariableName: string): string;
begin
  if AnsiCompareText(AVariableName, 'COPY') = 0 then
    Result := IntToStr(PrintingCopy)
  else
    if AnsiCompareText(AVariableName, 'COPIES') = 0 then
      Result := IntToStr(Copies)
    else
      if AnsiCompareText(AVariableName, 'PRINTER') = 0 then
        Result := PrinterName
      else
        Result := '';
end;

procedure TprCustomReport.ParsePrintingString(var S: string);
var
  I, J, ALen: Integer;
  AVariableValue, AVariableName: string;
begin
  I := 1;
  ALen := Length(S);
  while I <= ALen do
  begin
    if Copy(S, I, LenPrintVariablePrefix) = PrintVariablePrefix then
    begin
      J := I;
      I := I + LenPrintVariablePrefix;
      while (I <= ALen) and (Copy(S, I, LenPrintVariableSuffix) <> PrintVariableSuffix) do Inc(I);
      if I <= ALen then
      begin
        // print variable is found
        AVariableName := Copy(S, J + LenPrintVariablePrefix, I - (J + LenPrintVariablePrefix));
        AVariableValue := GetPrintVariableValue(AVariableName);
        DoOnFoundPrintVariable(AVariableName, AVariableValue);

        I := I + LenPrintVariableSuffix;
        Delete(S, J, I - J);
        Insert(AVariableValue, S, J);
        I := I - (I - J - Length(AVariableValue));
      end;
    end
    else
      Inc(I);
  end;
end;

procedure TprCustomReport.ClearPreviewUserDataList;
var
  i : integer;
begin
for i:=0 to FPreviewUserDataList.Count-1 do
  TprPreviewUserData(FPreviewUserDataList[i]).Free;
FPreviewUserDataList.Clear;
end;

procedure TprCustomReport.ClearGrids;
var
  i : integer;
begin
for i:=0 to PagesCount-1 do
  Pages[i].Grid.Clear;
end;

function TprCustomReport.GetDesignerNotifyLinksCount : integer;
begin
  Result := FDesignerNotifyLinks.Count;
end;

function TprCustomReport.GetPreviewNotifyLinksCount: Integer;
begin
  Result := FPreviewNotifyLinks.Count;
end;

function TprCustomReport.GetObjectCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TprObj then
      Inc(Result);
end;

function TprCustomReport.GetReportObject(Index: Integer): TprObj;
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TprObj then
    begin
      if Index = 0 then
      begin
        Result := TprObj(Components[I]);
        exit;
      end
      else
      begin
        Dec(Index);
        if Index < 0 then
          break;
      end;
    end;
  Result := nil;
end;

procedure TprCustomReport.AddPreviewUserData;
begin
FPreviewUserDataList.Add(PreviewUserData);
end;

procedure TprCustomReport.ClearEndPages;
begin
while EndPagesCount>0 do
  DeleteEndPage(0);
end;

procedure TprCustomReport.ClearValuesVersions;
var
  i : integer;
begin
for i:=0 to Values.Count-1 do
  Values[i].Clear;
for i:=0 to SystemValues.Count-1 do
  SystemValues[i].Clear;
end;

procedure TprCustomReport.ClearPreparedReport;
begin
  if FPreviewForm <> nil then
    FPreviewForm.Free;
  ClearEndPages;
  ClearPreviewUserDataList;
  FReportPrepared := false;
end;

procedure TprCustomReport.ExportTo;
var
  i : integer;
  ef : TprCustomExportFilter;
  ExportFilterClass : TprExportFilterClass;
begin
if preoShowParamsDlg in ExportOptions then
  if not SetupExportParams then
    exit;
i := 0;
while (i<=High(prExportFiltersInfos)) and (AnsiCompareText(prExportFiltersInfos[i].ExportFilterClassRef.ClassName,ExportFilter)<>0) do Inc(i);
if i>High(prExportFiltersInfos) then exit;
ExportFilterClass := prExportFiltersInfos[i].ExportFilterClassRef;
ef := ExportFilterClass.Create;
try
  ef.SaveToFile(Self);
finally
  ef.Free;
end;
end;

procedure TprCustomReport.AddSubReportData(ASubReportData: TprSubReportData);
begin
  if FSubReports = nil then
    FSubReports := TList.Create;
  FSubReports.Add(ASubReportData);
end;

procedure TprCustomReport.FreeSubReportsData;
begin
  if FSubReports <> nil then
    FreeList(FSubReports);
end;

function TprCustomReport.InternalPrepareReport(AMainPage: TprCustomPage;
                                               AParentPage: TprCustomPage;
                                               AStartPageIndex, ATopOffset: Integer): Boolean;
var
  I, J, K: integer;
  ep: TprCustomEndPage;
begin
  Result := false;
  FFinishedPositionOnLastPage := 0; 
  FActionCanceled := false;
  FReportPrepared := false;

  if PreviewForm <> nil then
    PreviewForm.Free;

  FStartDateTime := Now;
  FProgressForm := nil;
  FIndexCurEndPage := -1;

  ParentPage := AParentPage;

  if (AParentPage = nil) and ShowProgress then
    CreateProgressForm(prLoadStr(sCreateReportCaption), 0);

  try
    try
      SetLength(ADSRN, 0);

      ClearEndPages;
      ClearPreviewUserDataList;
      ClearValuesVersions;
      TprParser(Parser).ClearInternalStructs;

      for I := 0 to AllValuesCount - 1 do
      begin
        AllValues[I].Clear;
        AllValues[I].Init;
      end;

      I := 0;
      while I < PagesCount do
      begin
        DoOnTemplatePageGenerate(Pages[I]);
        if Pages[I].Visible then
          Pages[I].FirstPassGenerateGrid;
        Inc(I);
      end;

      I := 0;
      while I < PagesCount do
      begin
        if Pages[I].Visible then
        begin
          if AMainPage = nil then
            Pages[I].SecondPassPlaceGridOnEndPage(Pages[I], AParentPage, AStartPageIndex, ATopOffset)
          else
            Pages[I].SecondPassPlaceGridOnEndPage(AMainPage, AParentPage, AStartPageIndex, ATopOffset)
        end;
        Inc(I);
      end;

      // calculate PagesCount property of TprCustomEndPage objects,
      // based on ResetPagesCountProperty
      I := 0;
      while I < EndPagesCount do
      begin
        J := I;
        Inc(I);
        while (I < EndPagesCount) and (not EndPages[I].ResetPagesCount) do Inc(I);
        for K := J to I - 1 do
          EndPages[K].PagesCount := I - J;
      end;

      for I := 0 to AllValuesCount - 1 do
        AllValues[I].Reset;

      if AParentPage = nil then
      begin
        I := 0;
        while I < EndPagesCount do
        begin
          if AParentPage = nil then
            UpdateProgressForm(Format(prLoadStr(sThirdPassCaption),[I + 1, EndPagesCount]));

          FIndexCurEndPage := I;
          ep := EndPages[I];

          ep.ThirdPass;

          ep.FreeGeneratedObjects;

          Inc(I);
        end;
      end;

{$IFDEF DEBUG_DUMPAGGVALUES}
      DumpValues;
{$ENDIF}

      FReportPrepared := True;
      if AParentPage = nil then
        PrvPreviewChanged(nil);
      Result := true;

    except
      on E: Exception do
      begin
        ClearEndPages;
        ClearPreviewUserDataList;
        ClearGrids;
        if E is EActionCanceled then
          FActionCanceled := True
        else
          raise;
      end;
    end;

  finally
    Finalize(ADSRN);
    if AParentPage = nil then
    begin
      FreeSubReportsData;
      ClearValuesVersions; // will be used in the TprSubReportData
      TprParser(Parser).ClearInternalStructs; // will be used in the TprSubReportData
      CloseProgressForm;
    end;
  end;
end;


{$IFDEF DEBUG_DUMPAGGVALUES}
procedure TprCustomReport.DumpValues;
var
  I, J: Integer;
  AValue: TprValue;
  AVersion: TprValueVersion;
begin
  for I := 0 to Values.Count - 1 do
  begin
    AValue := Values[I];
    DbgFileStrFmt('Aggregate variable: [%s]. Versions count: [%d]'#13#10#13#10, [AValue.Name, AValue.VersionsCount]);
    for J := 0 to AValue.VersionsCount - 1 do
    begin
      AVersion := AValue.Versions[J];
      DbgFileStrFmt('%4d. Value: %s'#13#10, [J, VarToStr(AVersion.VersionValue)]);
    end;
  end;
end;
{$ENDIF}

function TprCustomReport.PrepareReport;
{
var
  I, J, K: integer;
  ep: TprCustomEndPage;
}
begin
  Result := InternalPrepareReport(nil, nil, 0, 0);
{
  FActionCanceled := false;
  Result := false;
  FReportPrepared := false;

  if PreviewForm <> nil then
    PreviewForm.Free;

  FStartDateTime := Now;
  FIndexCurEndPage := -1;
  FProgressForm := nil;

  if ShowProgress then
    CreateProgressForm(prLoadStr(sCreateReportCaption), 0);

try
  try
    SetLength(ADSRN,0);

    ClearEndPages;
    ClearPreviewUserDataList;

    for i:=0 to AllValuesCount-1 do
      begin
        AllValues[i].Clear;
        AllValues[i].Init;
      end;

    i:=0;
    while i<PagesCount do
      begin
        DoOnTemplatePageGenerate(Pages[i]);
        if Pages[i].Visible then
          Pages[i].FirstPassGenerateGrid;
        Inc(i);
      end;

    i:=0;
    while i<PagesCount do
      begin
        if Pages[i].Visible then
          Pages[i].SecondPassPlaceGridOnEndPage;
        Inc(i);
      end;

    // calculate PagesCount property of TprCustomEndPage objects,
    // based on ResetPagesCountProperty
    I := 0;
    while I < EndPagesCount do
    begin
      J := I;
      Inc(I);
      while (I < EndPagesCount) and (not EndPages[I].ResetPagesCount) do Inc(I);
      for K := J to I - 1 do
        EndPages[K].PagesCount := I - J;
    end;

    for i:=0 to AllValuesCount-1 do
      AllValues[i].Reset;

    i:=0;
    while i<EndPagesCount do
      begin
        UpdateProgressForm(Format(prLoadStr(sThirdPassCaption),[i+1,EndPagesCount]));

        FIndexCurEndPage := i;
        ep := EndPages[i];

        ep.ThirdPass;

        ep.FreeGeneratedObjects;

        Inc(i);
      end;

    FReportPrepared := true;
    PrvPreviewChanged(nil);
    Result := true;
  except
    on E : Exception do
      begin
        ClearEndPages;
        ClearPreviewUserDataList;
        ClearGrids;
        if E is EActionCanceled then
          FActionCanceled:=true
        else
          raise;
      end;
  end;

finally
  CloseProgressForm;
  Finalize(ADSRN);
  ClearValuesVersions;
  TprParser(Parser).ClearInternalStructs;
end;
}
end;

function TprCustomReport.GetSubReportByName(const SubReportName: string): TprCustomReport;
var
  I: Integer;
  L: TStringList;
begin
  L := TStringList.Create;
  try
    GetAvailableSubReports(L);
    I := L.IndexOf(SubReportName);
    if I = -1 then
      Result := nil
    else
      Result := TprCustomReport(L.Objects[I]);
  finally
    L.Free;
  end;
end;

function TprCustomReport.GetDataSetByName;
var
  i : integer;
  L : TStringList;
begin
L:=TStringList.Create;
try
  GetAvailableDataSets(L);
  i:=L.IndexOf(DataSetName);
  if i=-1 then
    Result:=nil
  else
    Result:=TDataSet(L.Objects[i]);
finally
  L.Free;
end;
end;

procedure TprCustomReport.GetAvailableDataSets;
var
  i : integer;
begin
if Assigned(OnGetAvailableDatasets) then
  begin
    L.Clear;
    FOnGetAvailableDatasets(Self,L);
  end
else
  begin
    GetAvailableComponents(L);
    i:=0;
    while i<L.Count do
      if (TComponent(L.Objects[i]) is TDataSet) or (TComponent(L.Objects[i]) is TprDataSet) then
        Inc(i)
      else
        L.Delete(i);
  end;
end;

procedure TprCustomReport.GetAvailableSubReports(L: TStrings);
var
  I: Integer;
begin
  GetAvailableComponents(L);
  I := 0;
  while I < L.Count do
    if (TComponent(L.Objects[I]) is TprCustomReport) and (L.Objects[I] <> Self) then
      Inc(I)
    else
      L.Delete(I);
end;

procedure TprCustomReport.GetAvailableComponents;
var
  i{$IFDEF PR_D6_D7},j{$ENDIF} : integer;

  procedure AddComponents(C : TComponent);
  var
    i,j : integer;
  begin
  for i:=0 to C.ComponentCount-1 do
    begin
      // component must have method or property
      j:=1;
      while (j<=prMAX_OBJFUNCTIONS) and not (C.Components[i] is ObjFuncInfo[j].ObjClassRef) do Inc(j);
      if j>prMAX_OBJFUNCTIONS then
        begin
          j:=1;
          while (j<=prMAX_OBJPROPS) and not (C.Components[i] is ObjPropInfo[j].ObjClassRef) do Inc(j);
          if j>prMAX_OBJPROPS then continue;
        end;
      // add this component
      if L.IndexOfObject(C.Components[i])=-1 then
        begin
          if (Owner=C) or (C=Self) then
            L.AddObject(C.Components[i].Name,C.Components[i])
          else
            L.AddObject(C.Name+'.'+C.Components[i].Name,C.Components[i])
        end;
    end;
  end;


begin
L.Clear;
L.AddObject(prLoadStr(sReportObjectName),Self);

if Assigned(OnGetAvailableComponents) then
  begin
    FOnGetAvailableComponents(Self,L);
  end
else
  begin
    AddComponents(Self);
    if Owner<>nil then
      AddComponents(Owner);

{$IFDEF PR_D6_D7}
    if csDesigning in ComponentState then
      begin
        for i:=0 to Screen.CustomFormCount-1 do
          if AnsiCompareText(Screen.CustomForms[i].ClassName,'TDataModuleForm')=0 then
            for j:=0 to Screen.CustomForms[i].ComponentCount-1 do
              if Screen.CustomForms[i].Components[j] is TDataModule then
                AddComponents(Screen.CustomForms[i].Components[j])
      end
    else
      begin
        for i:=0 to Screen.DataModuleCount-1 do
          AddComponents(Screen.DataModules[i]);
      end;
{$ELSE}
    for i:=0 to Screen.DataModuleCount-1 do
      AddComponents(Screen.DataModules[i]);
{$ENDIF}

    for i:=0 to Screen.FormCount-1 do
      AddComponents(Screen.Forms[i]);
  end;
end;

procedure TprCustomReport.TranslateObjectName;
var
  j,l,p : integer;
  ModuleName : string;
begin
l := Length(Ident);
Component := nil;
LastName := '';

p := l;
while (p>=1) and (Ident[p]<>'.') do Dec(p);
if p<1 then exit;

ModuleName := Copy(Ident,1,p-1);
Component := Self.FindComponent(ModuleName);
if Component<>nil then
  begin
    LastName := Copy(Ident,p+1,l);
    exit;
  end;

if Assigned(OnGetAvailableComponents) then
  begin
    FGACList.Clear;
    OnGetAvailableComponents(Self,FGACList);

    j:=0;
    while (j<FGACList.Count) and
          (CompText(FGACList[j],ModuleName)<>0) do Inc(j);
    if j<FGACList.Count then
      begin
        Component := TComponent(FGACList.Objects[j]);
        LastName := Copy(Ident,p+1,l);
      end;
  end
else
  begin
    p := 1;
    LastName := ExtractSubStr(Ident,p,['.']);
    Component := Owner.FindComponent(LastName);
    if Component=nil then
      begin
        j := 0;
        while (j<Screen.DataModuleCount) and
              (CompText(Screen.DataModules[j].Name,LastName)<>0) do Inc(j);
        if j>=Screen.DatamoduleCount then
          begin
            j := 0;
            while (j<Screen.FormCount) and
                  (CompText(Screen.Forms[j].Name,LastName)<>0) do Inc(j);
            if j<Screen.FormCount then
              Component := Screen.Forms[j];
          end
        else
          Component := Screen.DataModules[j];
      end;

    if Component<>nil then
      begin
        LastName := ExtractSubStr(Ident,p,['.']);
        while (p<=l) and (Component<>nil) do
          begin
            Component := Component.FindComponent(LastName);
            LastName := ExtractSubStr(Ident,p,['.']);
          end;
      end;
  end;
end;

procedure TprCustomReport.UpdateProgressForm;
begin
if FProgressForm<>nil then
  FProgressForm.UpdatePF(Text);
end;

procedure TprCustomReport.DsgnNotify(Source: TObject);
var
  I: integer;
begin
  for I := 0 to FDesignerNotifyLinks.Count - 1 do
    TprNotifyLink(FDesignerNotifyLinks[I]).DoNotify(Source);
  TemplateChanged := true;
end;

procedure TprCustomReport.DesignReport;
var
  FormClass : TClass;
begin
FReportPrepared := false;
if DesignerForm<>nil then
  begin
    DesignerForm.Show;
  end
else
  begin
    if not (csDesigning in ComponentState) and (DesignerFormMode=fmMDIChild) and Modal then
      raise Exception.Create(prLoadStr(sErrorNoModalForMDIChild));

    FormClass := Classes.GetClass(GetDesignerFormClass);
    if FormClass=nil then
      raise Exception.Create(prLoadStr(sErrorDesignerClassNotFound));

    TprDesignerClass(FormClass).CreateDesigner(nil,Self);
    if Assigned(OnCreateDesigner) then
      OnCreateDesigner(Self);

    if Modal then
      DesignerForm.ShowModal
    else
      DesignerForm.Show;
  end;

if DesignerForm.WindowState=wsMinimized then
  DesignerForm.WindowState:=wsNormal;
end;

procedure TprCustomReport.PreviewPreparedReport;
var
  FormClass : TClass;
begin
if CheckEndPagesCountOnPreview and (EndPagesCount<=0) then
  raise Exception.Create(prLoadStr(sReportEmptyInPreview));

if PreviewForm<>nil then
  begin
    PreviewForm.Show;
  end
else
  begin
    if not (csDesigning in ComponentState) and (PreviewFormMode=fmMDIChild) and Modal then
      raise Exception.Create(prLoadStr(sErrorNoModalForMDIChild));

    FormClass := GetClass(GetPreviewFormClass);
    if FormClass=nil then
      raise Exception.Create(prLoadStr(sErrorPreviewClassNotFound));

    TprPreviewClass(FormClass).CreatePreview(nil,Self);
    if Assigned(OnCreatePreview) then
      OnCreatePreview(Self);

    if Modal then
      PreviewForm.ShowModal
    else
      PreviewForm.Show;
  end;

if PreviewForm.WindowState=wsMinimized then
  PreviewForm.WindowState:=wsNormal;
end;

procedure TprCustomReport.ClearTemplate;
begin
while PagesCount>0 do
  Pages[0].Free;

while Groups.Count>0 do
  Groups[0].Free;

Values.Clear;
DsgnTemplateChanged(nil,false);
end;

procedure TprCustomReport.SetTemplateChanged;
begin
FTemplateChanged := Value;
if Assigned(prTemplateChangedGlobalProc) then
  prTemplateChangedGlobalProc(Self);
end;

function TprCustomReport.CheckEndPagesCountOnPreview;
begin
Result := true;
end;

procedure TprCustomReport.SaveTemplate;
var
  s : string;
  ms : TMemoryStream;
begin
s := Name;
try
  Name := '';
  if InBinaryFormat then
    begin
      Stream.WriteComponent(Self);
    end
  else
    begin
      ms := TMemoryStream.Create;
      try
        ms.WriteComponent(Self);
        ms.Seek(0,soFromBeginning);
        ObjectBinaryToText(ms,Stream);
      finally
        ms.Free;
      end;
    end;
  FTemplateChanged := false;
finally
  Name:=s
end;
end;

procedure TprCustomReport.LoadTemplate;
var
  s : string;
  ms : TMemoryStream;
begin
ClearTemplate;
s := Name;
try
  if InBinaryFormat then
    begin
      Stream.ReadComponent(Self);
    end
  else
    begin
      ms := TMemoryStream.Create;
      try
        ObjectTextToBinary(Stream,ms);
        ms.Seek(0,soFromBeginning);
        ms.ReadComponent(Self);
      finally
        ms.Free;
      end;
    end;
  FReportPrepared := false;
  DsgnTemplateChanged(nil,false);
finally
  Name := s
end;
end;

procedure TprCustomReport.SaveTemplateToFile;
var
  fs : TFileStream;
begin
fs := TFileStream.Create(FileName,fmCreate);
try
  SaveTemplate(fs,InBinaryFormat);
finally
  fs.Free;
end;
end;

procedure TprCustomReport.SaveTemplateToStrings;
var
  ms : TMemoryStream;
begin
ms := TMemoryStream.Create;
try
  SaveTemplate(ms,false);
  ms.Seek(0,soFromBeginning);
  Dest.LoadFromStream(ms);
finally
  ms.Free;
end;
end;

procedure TprCustomReport.LoadTemplateFromStrings;
var
  ms : TMemoryStream;
begin
if Source.Count=0 then exit;
ms := TMemoryStream.Create;
try
  Source.SaveToStream(ms);
  ms.Seek(0,soFromBeginning);
  LoadTemplate(ms,false);
finally
  ms.Free;
end;
end;

procedure TprCustomReport.LoadTemplateFromFile;
var
  fs : TFileStream;
begin
fs := TFileStream.Create(FileName,fmOpenRead);
try
  ClearTemplate;
  LoadTemplate(fs,InBinaryFormat);
finally
  fs.Free;
end;
end;

procedure TprCustomReport.LoadPreparedReport;
begin
  PrvPreviewChanged(nil);
end;

procedure TprCustomReport.AppendPreparedReport;
begin
  PrvPreviewChanged(nil);
end;

procedure TprCustomReport.SavePreparedReport;
begin
  PrvPreviewChanged(nil);
end;

procedure TprCustomReport.LoadPreparedReportFromFile;
var
  fs : TFileStream;
begin
fs := TFileStream.Create(FileName,fmOpenRead);
try
  LoadPreparedReport(fs);
finally
  fs.Free;
end;
end;

procedure TprCustomReport.AppendPreparedReportFromFile;
var
  fs : TFileStream;
begin
fs := TFileStream.Create(FileName,fmOpenRead);
try
  AppendPreparedReport(fs);
finally
  fs.Free;
end;
end;

procedure TprCustomReport.SavePreparedReportToFile;
var
  fs : TFileStream;
begin
fs := TFileStream.Create(FileName,fmCreate);
try
  SavePreparedReport(fs);
finally
  fs.Free;
end;
end;

function TprCustomReport.GetBandClass;
begin
  Result := nil;
end;

procedure TprCustomReport.DsgnAddDesignerNotifyLink(dnl: TprNotifyLink);
begin
  if FDesignerNotifyLinks.IndexOf(dnl)=-1 then
    FDesignerNotifyLinks.Add(dnl);
end;

procedure TprCustomReport.DsgnRemoveDesignerNotifyLink(dnl: TprNotifyLink);
begin
  FDesignerNotifyLinks.Remove(dnl);
end;

procedure TprCustomReport.DsgnTemplateChanged(dnl: TprNotifyLink; TemplateChanged: boolean);
var
  I: integer;
begin
  Self.TemplateChanged := TemplateChanged;
  for I := 0 to FDesignerNotifyLinks.Count - 1 do
    if dnl <> FDesignerNotifyLinks[i] then
      TprNotifyLink(FDesignerNotifyLinks[I]).DoNotify(nil);
end;

procedure TprCustomReport.PrvAddNotifyLink(dnl: TprNotifyLink);
begin
  if FPreviewNotifyLinks.IndexOf(dnl)=-1 then
    FPreviewNotifyLinks.Add(dnl);
end;

procedure TprCustomReport.PrvRemoveNotifyLink(dnl: TprNotifyLink);
begin
  FPreviewNotifyLinks.Remove(dnl);
end;

procedure TprCustomReport.PrvPreviewChanged(dnl: TprNotifyLink);
var
  I: integer;
begin
  for I := 0 to FPreviewNotifyLinks.Count - 1 do
    if dnl <> FPreviewNotifyLinks[i] then
      TprNotifyLink(FPreviewNotifyLinks[I]).DoNotify(nil);
end;

function TprCustomReport.AddPage : TprCustomPage;
begin
Result := CreatePage;
end;

function TprCustomReport.InsertPage(InsertPos : integer) : TprCustomPage;
begin
Result := CreatePage;
Result.IndexInReport := InsertPos;
end;

procedure TprCustomReport.DeletePage(PageIndex : integer);
begin
  Pages[PageIndex].Free;
end;

/////////////////////////////////////////////////
//
// TprCustomExportFilter
//
/////////////////////////////////////////////////

{$IFDEF PR_D4}
var
  OldFindGlobalComponent : TFindGlobalComponent;

function FindGlobalComponent(const Name: string): TComponent;
var
  i : integer;
begin
Result:=nil;
i     :=0;
while (i<prCreatedReports.Count) and
      ((TComponent(prCreatedReports[i]).Name='') or
       (CompText(TComponent(prCreatedReports[i]).Name,Name)<>0)) do Inc(i);
if i<prCreatedReports.Count then
  Result:=TComponent(prCreatedReports[i])
else
  if Assigned(OldFindGlobalComponent) then
    Result:=OldFindGlobalComponent(Name);
end;
{$ENDIF}

var
  i : integer;
  buf : array [0..255] of string;
  hMod : HMODULE;

initialization

hMod := GetModuleHandle('User32.dll');
if hMod<>0 then
  @SetLayeredWindowAttributes := GetProcAddress(hMod, 'SetLayeredWindowAttributes');

RegisterClass(TprPreviewUserData);
RegisterClass(TprGroup);

{$IFDEF PR_D4}
prCreatedReports := TList.Create;
OldFindGlobalComponent := Classes.FindGlobalComponent;
Classes.FindGlobalComponent := FindGlobalComponent;
{$ENDIF}

for i:=integer(Low(TprBandType)) to integer(High(TprBandType)) do
  BandTitles[TprBandType(i)] := prLoadStr(sBandTitlesOffset-i);

for i:=0 to 23 do
  MonthsArray[(i div 12)+1,i-(i div 12)*12+1] := prLoadStr(sMonthsNamesOffset-i);

if GetEnvironmentVariable('PR_INIFILENAME',@buf,sizeof(buf))>0 then
  begin
    prIniFileName := Trim(StrPas(@buf));
    if prIniFileName='' then
      prIniFileName := AddFlash(ExtractFilePath(Application.ExeName))+_sPrIniFileName;
  end
else
  prIniFileName := GetFindFileName(_sPrIniFileName);

{$IFDEF PR_D6_D7}
StartClassGroup(TprDesignerAndPreviewBaseForm);
ActivateClassGroup(TprDesignerAndPreviewBaseForm);
{$ENDIF}
  
finalization

{$IFDEF PR_D4}
Classes.FindGlobalComponent := OldFindGlobalComponent;
prCreatedReports.Free;
prCreatedReports := nil;
{$ENDIF}
Finalize(prObjRegInfos);

end.

