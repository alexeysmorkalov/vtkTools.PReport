{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the TprPreviewPanel class and some auxiliary classes.
See also:
  TprPreviewPanel}
unit pr_PreviewPanel;

interface

uses
  Windows, Messages, Classes, Controls, SysUtils, Graphics, Forms, comctrls,
  clipbrd, dialogs, menus, inifiles, typinfo, vgr_Functions, vgr_GUIFunctions,

  pr_Common, pr_Utils, pr_Classes, pr_CommonPreviewPanel, pr_DesignerFunctions;

{$I pr.inc}

const
  PagesStepX = 5;
  PagesStepY = 5;
  LeftOffs = 10;
  TopOffs = 10;
  RightOffs = 10;
  BottomOffs = 10;
  
type
TprPreviewPanel = class;
TprPreviewPropsFormClass = class of TprPreviewPropsForm;
/////////////////////////////////////////////////
//
// TprPreviewPropsForm
//
/////////////////////////////////////////////////
TprPreviewPropsForm = class(TForm)
private
  FPreviewPanel : TprPreviewPanel;
  FOldOnClose: TCloseEvent;
protected
  procedure CopyPropertiesFromControls(l : TList); virtual; abstract;
  procedure CopyPropertiesToControls(l : TList); virtual; abstract;
public
  property PreviewPanel : TprPreviewPanel read FPreviewPanel;

  procedure CreateParams(var Params : TCreateParams); override;
  procedure Loaded; override;
  procedure OnCloseEvent(Sender: TObject; var Action : TCloseAction); virtual;

  procedure UpdateInfo; virtual;
  procedure Apply; virtual;
  procedure Cancel; virtual;
  procedure SetFocusOnFirstControl; virtual;
  
  constructor CreatePropsForm(AOwner : TComponent; _PreviewPanel : TprPreviewPanel);
end;

/////////////////////////////////////////////////
//
// TprPreviewPanelForm
//
/////////////////////////////////////////////////
TprPreviewPanelForm = class(TprToolWindowForm)
private
  FPreviewPanel : TprPreviewPanel;
protected
  function GetParentControl : TControl; override;
public
  property PreviewPanel : TprPreviewPanel read FPreviewPanel write FPreviewPanel;
end;

/////////////////////////////////////////////////
//
// TprCustomPreviewObjectsPropsForm
//
/////////////////////////////////////////////////
TprCustomPreviewObjectsPropsForm = class(TprPreviewPanelForm)
protected
  procedure DoApplyObjectsProps;
end;

/////////////////////////////////////////////////
//
// TprPreviewBox
//
/////////////////////////////////////////////////
TprPreviewBox = class(TprCustomPreviewBox)
private
  FMouseMode : TprMouseMode;

  FDownObject : TprExObjRecVersion;
  FDownPointInfo : TprPointInfo;
  FDownResizeMode : TprResizeType;
  FFirstMoveAfterDown : boolean;
  FDblClick : boolean;

  FLastX : integer;
  FLastY : integer;
  FStartX : integer;
  FStartY : integer;
  FStartRealX : integer;
  FStartRealY : integer;
  FStuckedX : integer;
  FStuckedY : integer;
  FScrollLastX : integer;
  FScrollLastY : integer;

  FGridBitmap : HBITMAP;
  FSelectionDrawObject : TprSelectionDrawObject;

  FLastHighlightedObject : TprExObjRecVersion;
  FLastHighlightedEndPage : TprEndPage;

  function GetPanel : TprPreviewPanel;
  function GetSelObj(i : integer) : TprExObjRecVersion;
  function GetSelCount : integer;
  procedure WMGetDlgCode(var Msg : TWMGetDlgCode); message WM_GETDLGCODE;
  procedure WMEraseBkgnd(var Msg : TWmEraseBkgnd); message WM_ERASEBKGND;
  procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
  procedure WMSysKeyChar(var Msg : TWMSysChar); message WM_SYSCHAR;
  procedure WMMouseWheel(var Msg : TWMMouseWheel); message WM_MOUSEWHEEL;

  procedure DoResize(ResizeObject: TprExObjRecVersion; oTop, oLeft, oBottom, oRight: Integer; prps: pprPaintStruct);
  procedure DoDrag(DragObject: TprExObjRecVersion; dx, dy: Integer; prps: pprPaintStruct);

  function DsgnR(Obj : TprExObjRecVersion) : TRect; overload;
  function DsgnR(PagePDI : pprDrawInfo) : TRect; overload;
  function DsgnRSel(i : integer) : TRect;

  procedure OnGetCountRects(Sender : TObject; var Count : integer);
  procedure OnGetRect(Sender : TObject; index : integer; var Rect : TRect);
  procedure OnGetAllowResizeTypes(Sender : TObject; index : integer; var AllowResizeTypes : TprResizeTypeSet);

  procedure GetPointInfoAt(x,y : integer; var Obj : TprExObjRecVersion; var EndPage : TprEndPage; var EndPagePDI : pprDrawInfo; var PointInfo : TprPointInfo; var ResizeMode : TprResizeType);
  function GetStuckedOffs(Obj : TprExObjRecVersion; XOffs,YOffs : integer; AllowStuckSides : TprAllowStuckSidesSet; var StuckedX,StuckedY : integer) : boolean;

  procedure _BeginPaint(var prps : rPrPaintStruct);
  procedure _EndPaint(const prps : rPrPaintStruct);
protected
  function GetRealRect(v : TprExObjRecVersion; pdi : pprDrawInfo) : TRect;
  procedure SetRealRect(const Value : TRect; v : TprExObjRecVersion; pdi : pprDrawInfo);

  procedure DrawAnime(DC : HDC; x,y : integer; CallWhileEvents : boolean);
  procedure DrawPageRect(DC : HDC; ep : TprEndPage; Selected : boolean);
  procedure HideDrawing(DC : HDC);
  procedure ShowDrawing(DC : HDC);
  procedure InvertObject(DC : HDC; ep : TprEndPage; v : TprExObjRecVersion);
  procedure InternalPaint(DC : HDC; Rgn : HRGN);

  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure DblClick; override;

  procedure KeyDown(var Key: Word; Shift: TShiftState); override;
public
  property SelObjs[I: integer]: TprExObjRecVersion read GetSelObj;
  property SelCount: integer read GetSelCount;
  property Panel: TprPreviewPanel read GetPanel;

  constructor Create(AOwner : TComponent); override;
  destructor Destroy; override;
end;

{Describes the zooming mode for preview control.
Items:
  smPageWidth - Fit page width in the preview window.
  smOnePagePercent - Show one page, with custom zoom factor.
  smPages - Show custom number of pages (rows and columns).
Syntax:
  TprScaleMode = (smPageWidth, smOnePagePercent, smPages);}
TprScaleMode = (smPageWidth, smOnePagePercent, smPages);

{TprOnPreviewSelectionChanged is the type for TprPreviewPanel.OnSelectionChanged event.
This event occurs when list of selected objects is changed.
Parameters:
  Sender - The TprPreviewPanel object.
  SelCount - The number of selected objects.}
TprOnPreviewSelectionChanged = procedure (Sender: TObject; SelCount : integer) of object;

{TprOnPreviewObjectResized is the type for TprPreviewPanel.OnObjectResized event.
This event occurs when an object is resized.
Parameters:
  Sender - The TprPreviewPanel object.
  ResizeObject - The object that is resized.}
TprOnPreviewObjectResized = procedure (Sender : TObject; ResizeObject : TprExObjRecVersion) of object;

{TprOnPreviewObjectDrag is the type for TprPreviewPanel.OnObjectDrag event.
This event occurs when an object is dragged.
If the set of objects was dragged then event occurs for each object.
Parameters:
  Sender - The TprPreviewPanel object.
  DragObject - The object that is dragged.}
TprOnPreviewObjectDrag = procedure (Sender: TObject; DragObject: TprExObjRecVersion) of object;

{TprOnPreviewObjectInserted is the type for TprPreviewPanel.OnObjectInserted event.
This event occurs when the new object added to the report.
Parameters:
  Sender - The TprPreviewPanel object.
  InsertedObject - The added object.}
TprOnPreviewObjectInserted = procedure (Sender: TObject; InsertedObject: TprExObjRecVersion) of object;

{TprOnApplyObjectsProps is the type of TprPreviewPanel.OnApplyObjectsProps event.
This event occurs when user press "OK" or "Apply" in the window of the object's properties.
Parameters:
  Sender - The TprPreviewPanel object.}
TprOnPreviewApplyObjectsProps = procedure (Sender : TObject) of object;

{TprOnPreviewWhileObjectResize is the type of TprPreviewPanel.OnWhileObjectResize event.
This event occurs while the user is resizing the object.
Parameters:
  Sender - The TprOnPreviewWhileObjectResize object.
  ResizeRect - Specifies the currently selected rectangle of object.}
TprOnPreviewWhileObjectResize = procedure (Sender : TObject; const ResizeRect : TRect) of object;

{TprOnPreviewWhileObjectDrag is the type of TprPreviewPanel.OnWhileObjectDrag event.
This event occurs while the user is dragging the object.
Parameters:
  Sender - The TprPreviewPanel object.
  DragRect - Specifies the currently selected rectangle of object.}
TprOnPreviewWhileObjectDrag = procedure (Sender: TObject; const DragRect: TRect) of object;

{TprOnPreviewPageInserted is the type of TprPreviewPanel.OnPageInserted event.
This event occurs after inserting of new page.
Parameters:
  Sender - The TprPreviewPanel object.
  InsertedPage - The inserted page.}
TprOnPreviewPageInserted = procedure (Sender: TObject; InsertedPage: TprEndPage) of object;

{TprOnPreviewPageDeleted is the type of TprPreviewPanel.OnPageDeleted event.
This event occurs after deleting of page.
Parameters:
  Sender - The TprPreviewPanel object.
  DeletedPage - The deleted page.}
TprOnPreviewPageDeleted = procedure (Sender: TObject; DeletedPage: TprEndPage) of object;

{TprOnPreviewPageParamsChanged is the type of TprPreviewPanel.OnPageParamsChanged event.
This event occurs after changing of page's properties such as margins, paper's size etc.
Parameters:
  Sender - The TprPreviewPanel object.
  ChangedPage - The changed page.}
TprOnPreviewPageParamsChanged = procedure (Sender : TObject; ChangedPage : TprEndPage) of object;

/////////////////////////////////////////////////
//
// TprPreviewPanel
//
/////////////////////////////////////////////////
{Represents the control for previewing the generated TprReport report.
You can use this control if you want to develop non standard report designers
with some unique features. TprPreviewPanel features:
- Editing of generated report.<br>
- Contains the built-in popup menu that allows to get an access to all functions of preview panel.<br>
- The properties of each page can be changed separately.<br>
- Contains the popup forms that allow to edit objects' properties, sizes etc<br>
- Supports the search of text.<br>
- and so on<br>
See demo "12_Drag-Drop" as example of using this control.
See also:
  TprCustomPreviewPanel}
TprPreviewPanel = class(TprCustomPreviewPanel)
private
  FPagesPerHeight : integer;
  FPagesPerWidth : integer;
  FScaleMode : TprScaleMode;
  FUseGrid : boolean;
  FShowGrid : boolean;
  FGridSize : integer;
  FStuckMode : TprStuckMode;
  FStuckOptions : TprPreviewStuckOptionsSet;
  FStuckOffs : integer;
  FOpacityObjectsPropsForm : integer;
  FPopupInsertObjectMenu : boolean;
  FPopupMainMenu : boolean;
  FCachedPages : TList;

  FOnSelectionChanged : TprOnPreviewSelectionChanged;
  FOnObjectResized : TprOnPreviewObjectResized;
  FOnObjectDrag : TprOnPreviewObjectDrag;
  FOnApplyObjectsProps : TprOnPreviewApplyObjectsProps;
  FOnWhileObjectDrag : TprOnPreviewWhileObjectDrag;
  FOnWhileObjectResize  : TprOnPreviewWhileObjectResize;
  FOnMouseDown : TprOnPreviewMouseDown;
  FOnDblClick : TprOnPreviewDblClick;
  FOnMouseMove : TprOnPreviewMouseMove;
  FOnScalePercentChanged : TNotifyEvent;
  FOnPageInserted : TprOnPreviewPageInserted;
  FOnPageDeleted : TprOnPreviewPageDeleted;
  FOnPageParamsChanged : TprOnPreviewPageParamsChanged;
  FOnShowPageChanged: TNotifyEvent;

  FSelObjs : TList;
  FMaxWidth : integer;
  FMaxHeight : integer;
  FDrawWidth : integer;
  FDrawHeight : integer;
  FScaleMul : integer;
  FScaleDiv : integer;
  FFindPos : integer;

  FCurClassRef : TprObjClass;

  FCurPage : TprEndPage;
  FCurShowPage: TprEndPage;

  function GetSelObj(i : integer) : TprExObjRecVersion;
  function GetSelCount : integer;
  function GetVerScrollBoxPos : integer;
  function GetHorScrollBoxPos : integer;
  procedure ClearCachedPages;
  procedure ClearFindList;
  procedure CalcMaxSizes;
  function GetSelectedRect : TRect;
  procedure SetCurPage(Value : TprEndPage);
  function GetActivePageIndex : integer;
  procedure SetUseGrid(Value : boolean);
  procedure SetGridSize(Value : integer);
  procedure SetShowGrid(Value : boolean);
  function GetBox : TprPreviewBox;
  function GetScalePercent : integer;
  procedure SetPagesPerHeight(Value : integer);
  procedure SetPagesPerWidth(value : integer);
  procedure SetScaleMode(Value : TprScaleMode);
  procedure SetScalePercent(Value : integer);
  function GetOpacityObjectsPropsForm : integer;
  procedure SetOpacityObjectsPropsForm(Value : integer);

  procedure SelectedObjectsToClipboard;
  procedure InternalClearSelected;
  procedure InternalSelectObj(Obj : TprExObjRecVersion);
  procedure InternalDeSelectObj(Obj : TprExObjRecVersion);

  function GetVisibleObjectsPropsForm : boolean;
  procedure SetVisibleObjectsPropsForm(Value : boolean);

  function GetFindText : string;
  procedure SetFindText(Value : string);
  function GetFindCount : integer;
  function GetFindCaseSensitive : boolean;
  procedure SetFindCaseSensitive(Value : boolean);
  function GetIsFindMode : boolean;
  function GetProgressBar : TProgressBar;
  procedure SetProgressBar(Value : TProgressBar);
  function GetStatusBar : TStatusBar;
  procedure SetStatusBar(Value : TStatusBar);

  procedure OnFindDialogFind(Sender : TObject);
  procedure OnPopupMenuInsertObjectClick(Sender : TObject);

  procedure SetCurShowPage(Value: TprEndPage);
  
  procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;

  function GetReport: TprReport;
  procedure SetReport(Value: TprReport);
protected
  Frpdi : rprPreviewDrawInfo;
  FObjectsPropsForm : TprPreviewPanelForm;
  FFindDialog : TFindDialog;

  function GetPageIndex: Integer; override;
  procedure SetPageIndex(Value: Integer); override;
  function GetPageCount: Integer; override;

  function CreatePreviewBox : TprCustomPreviewBox; override;

  function GetRealRect(v : TprExObjRecVersion; pdi : pprDrawInfo) : TRect;
  procedure SetRealRect(const Value : TRect; v : TprExObjRecVersion; pdi : pprDrawInfo);
  
  procedure PagesChanged;
  function InternalFind : boolean;
  procedure UpdatePreviewBox;

  procedure Resize; override;

  procedure UpdateObjectsPropsForm;
  procedure OnPopupMenuClick(Sender : TObject);
  procedure InitPopupMenu(Popup : TPopupMenu); override;

  procedure DoSelectionChanged;
  procedure DoObjectResized(ResizeObject : TprExObjRecVersion);
  procedure DoObjectDrag(DragObject : TprExObjRecVersion);
  procedure DoApplyObjectsProps;
  procedure DoWhileObjectDrag(const DragRect : TRect);
  procedure DoWhileObjectResize(const ResizeRect : TRect);
  procedure DoPreviewMouseDown(PreviewUserData : TprPreviewUserData; X,Y : integer; Shift : TShiftState);
  procedure DoPreviewDblClick(PreviewUserData : TprPreviewUserData; X,Y : integer);
  procedure DoPreviewMouseMove(PreviewUserData : TprPreviewUserData; X,Y : integer; var cur : TCursor; var HighlightObject : boolean);
  procedure DoScalePercentChanged;
  procedure DoPageInserted(InsertedPage : TprEndPage);
  procedure DoPageDeleted(DeletedPage : TprEndPage);
  procedure DoPageParamsChanged(ChangedPage : TprEndPage);
  procedure DoShowPageChanged;

  function DsgnAllowDragObject(Obj : TprExObjRecVersion) : boolean;
  function DsgnAllowResizeObject(Obj : TprExObjRecVersion) : boolean;
  procedure DsgnObjPage(Obj : TprExObjRecVersion; var ep : TprEndPage; var pdi : pprDrawInfo);
  function DsgnObjPDI(Obj : TprExObjRecVersion) : pprDrawInfo;
  function DsgnPagePDI(Page : TprEndPage) : pprDrawInfo;

  function DsgnR(Obj : TprExObjRecVersion) : TRect; overload;
  function DsgnR(PagePDI : pprDrawInfo) : TRect; overload;
  function DsgnRSel(i : integer) : TRect;
  function DsgnWSel(i : integer) : integer;
  function DsgnHSel(i : integer) : integer;
  
  property Box: TprPreviewBox read GetBox;
public
{Lists the selected objects.
Parameters:
  I - Index of the object, from 0.}
  property SelObjs[I: integer] : TprExObjRecVersion read GetSelObj;
{Returns the number of selected objects.}
  property SelCount : integer read GetSelCount;
{Returns the list of selected objects.}
  property SelObjsList: TList read FSelObjs;
  property SelectedRect: TRect read GetSelectedRect;

{Gets or sets the first page shown by the preview.
See also:
  CurShowPageIndex}
  property CurShowPage: TprEndPage read FCurShowPage write SetCurShowPage;

{Specifies the currently active page that is highlighted with blue border.
See also:
  ActivePageIndex}
  property ActivePage: TprEndPage read FCurPage write SetCurPage;
{Specifies the index of the currently active page that is highlighted with blue border.
See also:
  ActivePage}
  property ActivePageIndex: Integer read GetActivePageIndex;

{Specifies the current position of the vertical scroll bar.}
  property VerScrollBoxPos: integer read GetVerScrollBoxPos;
{Specifies the current position of the horizontal scroll bar.}
  property HorScrollBoxPos: integer read GetHorScrollBoxPos;

{Indicates whether the "Properties" popup form is visible.
This form allows to edit the properties of selected objects.}
  property VisibleObjectsPropsForm : boolean read GetVisibleObjectsPropsForm write SetVisibleObjectsPropsForm;

{Specifies the text to find.
Example:
  // This procedure starts the search in the preview panel.
  procedure TMyForm.SearchText(const AText: string);
  begin
    with FPreviewPanel do
    begin
      FindText := AText;
      FindCaseSensitive := True;
      Find;
      ShowMessage(Format('%d fragments are found', [FindCount]));
    end;
  end;}
  property FindText : string read GetFindText write SetFindText;
{Specifies the number of texts' fragments that were found after call of Find method.}
  property FindCount: integer read GetFindCount;
{Indicates whether the text search is case sensitive.}
  property FindCaseSensitive: Boolean read GetFindCaseSensitive write SetFindCaseSensitive;
{Indicates whether the find mode is started and the searched fragments of text are highlighted.}
  property IsFindMode: boolean read GetIsFindMode;

{Returns the TStatusBar object, that is used by preview.}
  property StatusBar : TStatusBar read GetStatusBar write SetStatusBar;
{Returns the TProgressBar object, that is used by preview.}
  property ProgressBar : TProgressBar read GetProgressBar write SetProgressBar;

{Returns the true if the preview panel can work in edit mode
(if the linked report has the CanUserEdit property with true value.}
  function DsgnCanUserEdit: boolean;

{Returns the true if specified object is selected.}
  function IsObjSelected(Obj : TprExObjRecVersion) : boolean;
{Selects the specified object, the previously selected objects are lose the selection.}
  procedure SelectObject(Obj : TprExObjRecVersion);
{Adds the specified object to selection.}
  procedure AddSelectObject(Obj : TprExObjRecVersion);
{Removes the specified object from selection.}
  procedure DeSelectObject(Obj : TprExObjRecVersion);
{Clears the selection.}
  procedure ClearSelection;

{Starts the inserting of the object of specified type.
Parameters:
  ObjClassRef - Specifies the type of object to insert.}
  procedure InsertObject(ObjClassRef : TprObjClass);
{Repaints the selected objects.}
  procedure UpdateSelectedObjects;

{Returns the true if the "Find" action is allowed.}
  function AllowFind : boolean;
{Returns the true if the selected objects can be deleted.}
  function AllowDelete : boolean;
{Returns the true if the selected objects can be copied into clipboard.}
  function AllowCopy : boolean;
{Returns the true of the clipboard content can be pasted into the report template.}
  function AllowPaste : boolean;
{Returns the true if the selected objects can be cut into clipboard.}
  function AllowCut : boolean;
{Returns the true if selected objects can be aligned to grid.}
  function AllowAlignToGridAll : boolean;
{Returns the number of selected objects that support the dragging.}
  function GetNumDragSelectedRegions: integer;
{Returns the number of selected objects that support the resizing.}
  function GetNumResizeSelectedRegions: integer;
{Returns the true if the "Bring to front" action is allowed.}
  function AllowBringToFront : boolean;
{Returns the true if the "Send to back" action is allowed.}
  function AllowSendToBack : boolean;
{Returns the true if the "Insert page" action is allowed.}
  function AllowInsertPage : boolean;
{Returns the true if the "Delete page" action is allowed.}
  function AllowDeletePage : boolean;
{Returns the true if the "Edit page" action is allowed.}
  function AllowEditPage : boolean;

{Puts the selected objects in front of all other objects in its band.}
  procedure BringToFront;
{Puts the selected objects behind all other objects.}
  procedure SendToBack;
{Aligns the selected objects.
Parameters:
  ActionCode - The type of align action.
See also:
  TprAlignActionCode}
  procedure AlignAction(ActionCode : TprAlignActionCode);
{Returns the true if specified align action is allowed.
Parameters:
  ActionCode - The type of align action}
  function AlignActionAllowed(ActionCode : TprAlignActionCode) : boolean;
{Shifts the selected objects by specified value.
Parameters:
  dx - The horizontal shift.
  dy - The vertical shift.}
  procedure Nudge(dx,dy : integer);
{Changes the size of selected objects by specified value.
Parameters:
  sx - Specifies the width's change.
  sy - Specifies the height's change.}
  procedure Size(sx,sy : integer);
{Sets the value of the specified property of selected objects.
Parameters:
  Prop - Specifies the property to changing.
  Value - Specifies the new value of property.
See also:
  TprObjectPosSizeProps}
  procedure SetPosSizeProp(Prop : TprObjectPosSizeProps; Value : integer);

{Deletes the selected objects.}
  procedure DeleteSelectedObjects;
{Pastes the clipboard content into the active page.}
  procedure Paste;
{Cuts the selected objects into clipboard.
Only the objects can be cut into clipboard, pages can not be cut.}
  procedure Cut;
{Copies the selected objects into clipboard.
Only the objects can be copied into clipboard, pages can not be copied.}
  procedure Copy;

{Starts the search of text in the preview control.
Example:
  // This procedure starts the search in the preview panel.
  procedure TMyForm.SearchText(const AText: string);
  begin
    with FPreviewPanel do
    begin
      FindText := AText;
      FindCaseSensitive := True;
      Find;
      ShowMessage(Format('%d fragments are found', [FindCount]));
    end;
  end;}
  procedure Find;
{Cancels the searching, all highlighted text's fragments are become unselected.}
  procedure CancelFind;
{Goes to the next searched text fragment.}
  procedure FindNext;
{Goes to the previous searched text fragment.}
  procedure FindPrior;

{Displays the whole page (one full page in the preview window).}
  procedure ShowWholePage;
{Fit page width in the preview window.}
  procedure ShowPageWidth;
{Displays the specified number of pages per width and per height.
Parameters:
  PerHeight - The number of pages per height.
  PerWidth - The number of pages per width.}
  procedure ShowManyPages(PerHeight,PerWidth : integer);

{See:
  TprCustomPreviewPanel.LoadPreparedReport}
  procedure LoadPreparedReport(const FileName: string); override;

{Creates the new page of the report and inserts it in the report.
The created page will become active after creating.
Parameters:
  AfterPageIndex - The created page will be inserted after the page with this index.}
  function InsertPageAfter(AfterPageIndex : integer) : TprEndPage;
{Deletes the page.
Parameters:
  PageIndex - The page's index.}
  procedure DeletePage(PageIndex : integer);
{Opens the dialog window in that the page properties can be changed (width, height, paper's size and so on).
Parameters:
  PageIndex - The page's index.
Return value:
  Returns the true if user press "OK" in the dialog.}
  function EditPage(PageIndex : integer) : boolean;
{Inserts the new page after the active page.}
  procedure InsertPageAfterCurrent;
{Deletes the active page (that has the blue border).}
  procedure DeleteCurrentPage;
{Opens the dialog window in that the properties of active page
can be changed (width, height, paper's size and so on).}
  procedure EditCurrentPage;

{See:
  TprCustomPreviewPanel.WriteToIni}
  procedure WriteToIni(IniFile : TIniFile; const SectionName : string); override;
{See:
  TprCustomPreviewPanel.ReadFromIni}
  procedure ReadFromIni(IniFile : TIniFile; const SectionName : string); override;

{See:
  TprCustomPreviewPanel.EditOptions}
  function EditOptions : boolean; override;

{See:
  TprCustomPreviewPanel.FullUpdate}
  procedure FullUpdate; override;

{Creates an instance of the TprPreviewPanel class.}
  constructor Create(AOwner : TComponent); override;
{Frees an instance of the TprPreviewPanel class.}
  destructor Destroy; override;
published
  property Align;

{Specifies the TprReport object that is linked to the preview control.}
  property Report: TprReport read GetReport write SetReport;
{Indicates whether the objects must be aligned to the grid.}
  property UseGrid: boolean read FUseGrid write SetUseGrid default false;
{Indicates whether the grid is displayed.}
  property ShowGrid: boolean read FShowGrid write SetShowGrid default false;
{Specifies the grid size in pixels.}
  property GridSize : integer read FGridSize write SetGridSize default 8;
{Specifies when objects of the report template can be stuck while they been drag or been resize by mouse.
See also:
  TprStuckMode}
  property StuckMode : TprStuckMode read FStuckMode write FStuckMode default DefaultStuckMode;
{Specifies the "stuck" options.
See also:
  TprStuckOptionsSet, TprStuckOptions}
  property StuckOptions : TprPreviewStuckOptionsSet read FStuckOptions write FStuckOptions default DefaultPreviewStuckOptions;
{Specifies the minimum distance between objects from that they can stuck.}
  property StuckOffs : integer read FStuckOffs write FStuckOffs default DefaultStuckOffs;
{Specifies the current zoom mode of the preview pane.
See also:
  TprScaleMode}
  property ScaleMode : TprScaleMode read FScaleMode write SetScaleMode default smPageWidth;
{Specifies the number of displayed pages per control's width.}
  property PagesPerWidth : integer read FPagesPerWidth write SetPagesPerWidth default 2;
{Specifies the number of displayed pages per control's height.}
  property PagesPerHeight : integer read FPagesPerHeight write SetPagesPerHeight default 1;
{Specifies the zooming percent.}
  property ScalePercent : integer read GetScalePercent write SetScalePercent default 100;
{Specifies the opacity for "Object's properties" popup form.}
  property OpacityObjectsPropsForm: integer read GetOpacityObjectsPropsForm write SetOpacityObjectsPropsForm default 100;
{Specifies the value indicating whether the built-in popup menu must contain items for inserting
the new objects into the report template.}
  property PopupInsertObjectMenu: boolean read FPopupInsertObjectMenu write FPopupInsertObjectMenu;
{Specifies the value indicating whether the built-in popup menu must contain items for actions: Open, Save and so on.}
  property PopupMainMenu: boolean read FPopupMainMenu write FPopupMainMenu;

{This event occurs when list of selected objects is changed.
See also:
  TprOnPreviewSelectionChanged}
  property OnSelectionChanged : TprOnPreviewSelectionChanged read FOnSelectionChanged write FOnSelectionChanged;

{This event occurs when an object is resized.
See also:
  TprOnPreviewObjectResized}
  property OnObjectResized : TprOnPreviewObjectResized read FOnObjectResized write FOnObjectResized;

{This event occurs when an object is dragged.
If the set of objects was dragged then event occurs for each object.
See also:
  TprOnPreviewObjectDrag}
  property OnObjectDrag : TprOnPreviewObjectDrag read FOnObjectDrag write FOnObjectDrag;

{This event occurs when user press "OK" or "Apply" in the window of the object's properties.
See also:
  TprOnPreviewApplyObjectsProps}
  property OnApplyObjectsProps : TprOnPreviewApplyObjectsProps read FOnApplyObjectsProps write FOnApplyObjectsProps;

{This event occurs while the user is dragging the object.
See also:
  TprOnPreviewWhileObjectDrag}
  property OnWhileObjectDrag : TprOnPreviewWhileObjectDrag read FOnWhileObjectDrag write FOnWhileObjectDrag;

{This event occurs while the user is resizing the object.
See also:
  TprOnPreviewWhileObjectResize}
  property OnWhileObjectResize : TprOnPreviewWhileObjectResize read FOnWhileObjectResize write FOnWhileObjectResize;

{Occurs when the user presses a mouse button with the mouse pointer over a control.
See also:
  TprOnPreviewMouseDown}
  property OnMouseDown : TprOnPreviewMouseDown read FOnMouseDown write FOnMouseDown;

{Occurs when the user double-clicks the left mouse button when the mouse pointer is over the control.
See also:
  TprOnPreviewDblClick}
  property OnDblClick: TprOnPreviewDblClick read FOnDblClick write FOnDblClick;

{This event occurs when user move the mouse over control.
See also:
  TprOnPreviewMouseMove}
  property OnMouseMove: TprOnPreviewMouseMove read FOnMouseMove write FOnMouseMove;

{This event occurs when zooming percent is changed.}
  property OnScalePercentChanged: TNotifyEvent read FOnScalePercentChanged write FOnScalePercentChanged;

{This event occurs after inserting of new page.
See also:
  TprOnPreviewPageInserted}
  property OnPageInserted: TprOnPreviewPageInserted read FOnPageInserted write FOnPageInserted;

{This event occurs after deleting of page.
See also:
  TprOnPreviewPageDeleted}
  property OnPageDeleted: TprOnPreviewPageDeleted read FOnPageDeleted write FOnPageDeleted;

{This event occurs after changing of page's properties such as margins, paper's size etc.
See also:
  TprOnPreviewPageParamsChanged}
  property OnPageParamsChanged: TprOnPreviewPageParamsChanged read FOnPageParamsChanged write FOnPageParamsChanged;

{This event occurs when the first page shown by the preview is changed.}
  property OnShowPageChanged: TNotifyEvent read FOnShowPageChanged write FOnShowPageChanged; 
end;

implementation

uses
  pr_Strings, pr_MultiLang, pr_PageParams, pr_PreviewObjectsProps, pr_PreviewPanelOptions;

/////////////////////////////////////////////////
//
// TprPreviewPropsForm
//
/////////////////////////////////////////////////
constructor TprPreviewPropsForm.CreatePropsForm;
begin
FPreviewPanel := _PreviewPanel;
inherited Create(AOwner);
end;

procedure TprPreviewPropsForm.UpdateInfo;
begin
CopyPropertiesToControls(PreviewPanel.SelObjsList);
end;

procedure TprPreviewPropsForm.Apply;
begin
  CopyPropertiesFromControls(PreviewPanel.SelObjsList);
  PreviewPanel.PrvNotifyReport;
end;

procedure TprPreviewPropsForm.Cancel;
begin
end;

procedure TprPreviewPropsForm.CreateParams;
begin
inherited;
Params.Style := Params.Style and (not WS_BORDER) and (not WS_CAPTION) and (not WS_DISABLED);
Params.Style := Params.Style or WS_CHILD or WS_VISIBLE;
Params.ExStyle := Params.ExStyle or WS_EX_CONTROLPARENT;
end;

procedure TprPreviewPropsForm.Loaded;
begin
  inherited;
  FOldOnClose := Self.OnClose;
  Self.OnClose := OnCloseEvent; 
end;

procedure TprPreviewPropsForm.OnCloseEvent(Sender: TObject; var Action : TCloseAction);
begin
  Action := caFree;
  if Assigned(FOldOnClose) then
    FOldOnClose(Sender, Action);
end;

procedure TprPreviewPropsForm.SetFocusOnFirstControl;
var
  c : TWinControl;
begin
SetFocus;
c := FindNextControl(nil,true,true,false);
if c<>nil then
  c.SetFocus;
end;

/////////////////////////////////////////////////
//
// TprPreviewPanelForm
//
/////////////////////////////////////////////////
function TprPreviewPanelForm.GetParentControl : TControl;
begin
Result := PreviewPanel;
end;

/////////////////////////////////////////////////
//
// TprCustomPreviewObjectsPropsForm
//
/////////////////////////////////////////////////
procedure TprCustomPreviewObjectsPropsForm.DoApplyObjectsProps;
begin
PreviewPanel.DoApplyObjectsProps;
end;

/////////////////////////////////////////////////
//
// TprPreviewBox
//
/////////////////////////////////////////////////
constructor TprPreviewBox.Create(AOwner : TComponent);
begin
inherited;
FGridBitmap := 0;
FSelectionDrawObject := TprSelectionDrawObject.Create;
FSelectionDrawObject.OnGetCountRects := OnGetCountRects;
FSelectionDrawObject.OnGetRect := OnGetRect;
FSelectionDrawObject.OnGetAllowResizeTypes := OnGetAllowResizeTypes;

HorzScrollBar.Tracking := true;
VertScrollBar.Tracking := true;
end;

destructor TprPreviewBox.Destroy;
begin
if FGridBitmap<>0 then
  DeleteObject(FGridBitmap);
FSelectionDrawObject.Free;
inherited;
end;

function TprPreviewBox.GetPanel : TprPreviewPanel;
begin
Result := TprPreviewPanel(PreviewPanel); 
end;

function TprPreviewBox.GetSelObj(i : integer) : TprExObjRecVersion;
begin
Result := Panel.SelObjs[i];
end;

function TprPreviewBox.GetSelCount : integer;
begin
Result := Panel.SelCount;
end;

procedure TprPreviewBox.WMGetDlgCode(var Msg : TWMGetDlgCode);
begin
inherited;
Msg.Result := Msg.Result or DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;

function TprPreviewBox.GetRealRect(v : TprExObjRecVersion; pdi : pprDrawInfo) : TRect;
begin
Result := v.GetRealRect(pdi);
end;

procedure TprPreviewBox.SetRealRect(const Value : TRect; v : TprExObjRecVersion; pdi : pprDrawInfo);
begin
v.SetRealRect(Value,pdi);
end;

function TprPreviewBox.DsgnR(Obj : TprExObjRecVersion) : TRect;
begin
Result := Panel.DsgnR(Obj);
end;

function TprPreviewBox.DsgnR(PagePDI : pprDrawInfo) : TRect;
begin
Result := Panel.DsgnR(PagePDI);
end;

function TprPreviewBox.DsgnRSel(i : integer) : TRect;
begin
Result := Panel.DsgnRSel(i);
end;

procedure TprPreviewBox.OnGetCountRects(Sender : TObject; var Count : integer);
begin
Count := SelCount;
end;

procedure TprPreviewBox.OnGetRect(Sender : TObject; index : integer; var Rect : TRect);
begin
Rect := Panel.DsgnRSel(index);
end;

procedure TprPreviewBox.OnGetAllowResizeTypes(Sender : TObject; index : integer; var AllowResizeTypes : TprResizeTypeSet);
begin
AllowResizeTypes := [ppLeftTop,ppTop,ppRightTop,ppRight,ppRightBottom,ppBottom,ppLeftBottom,ppLeft];
end;

procedure TprPreviewBox.DoDrag(DragObject: TprExObjRecVersion; dx, dy: Integer; prps: pprPaintStruct);
var
  i: integer;
  pdi,pdi2 : pprDrawInfo;
  r1,r2 : TRect;
  ep,ep2 : TprEndPage;
begin
  if not Panel.DsgnAllowDragObject(DragObject) then exit;
  Panel.DsgnObjPage(DragObject, ep2, pdi2);
  r1 := GetRealRect(DragObject, pdi2);
  r2 := r1;

  OffsetRect(r1, pdi2.bRect.Left + dx, pdi2.bRect.Top + dy);
  // detect page in wich object must placed
  i := 0;
  ep := nil;
  pdi := nil;
  while i < Panel.Report.EndPagesCount do
  begin
    ep := TprEndPage(Panel.Report.EndPages[i]);
    pdi := pprDrawInfo(Panel.FCachedPages[i]);
    if RectInRect(r1,pdi.bRect) then
      break;
    Inc(i);
  end;
  if i >= Panel.Report.EndPagesCount then exit;

  if prps<>nil then
    AddRectToRegion(prps.ClipRgn,DsgnR(DragObject));

  // ep - new page
  // ep2 - current page
  if ep2=ep then
    OffsetRect(r2,dx,dy)
  else
  begin
    // move object from one page to other
    ep2.VL.Remove(DragObject);
    ep.VL.Add(DragObject);
    OffsetRect(r2,dx-pdi.bRect.Left+pdi2.bRect.Left,dy-pdi.bRect.Top+pdi2.bRect.Top);
  end;
  SetRealRect(r2, DragObject, pdi2);

  if prps<>nil then
    AddRectToRegion(prps.ClipRgn,DsgnR(DragObject));

  TprPreviewPanel(PreviewPanel).UpdateObjectsPropsForm;
  PreviewPanel.PrvNotifyReport;
  TprPreviewPanel(PreviewPanel).DoObjectDrag(DragObject);
end;

procedure TprPreviewBox.DoResize(ResizeObject: TprExObjRecVersion; oTop, oLeft, oBottom, oRight: Integer; prps: pprPaintStruct);
var
  ep : TprEndPage;
  pdi : pprDrawInfo;
  r1,r2 : TRect;
begin
  if not Panel.DsgnAllowResizeObject(ResizeObject) then exit;
  Panel.DsgnObjPage(ResizeObject,ep,pdi);
  r1 := GetRealRect(ResizeObject,pdi);
  r2 := r1;

  r1.Left := r1.Left+pdi.bRect.Left+oLeft;
  r1.Top := r1.Top+pdi.bRect.Top+oTop;
  r1.Right := r1.Right+pdi.bRect.Left+oRight;
  r1.Bottom := r1.Bottom+pdi.bRect.Top+oBottom;
  if not RectInRect(r1,pdi.bRect) then exit;

  if prps<>nil then
    AddRectToRegion(prps.ClipRgn,DsgnR(ResizeObject));

  r2.Left := r2.Left+oLeft;
  r2.Right := r2.Right+oRight;
  r2.Top := r2.Top+oTop;
  r2.Bottom := r2.Bottom+oBottom;
  SetRealRect(r2,ResizeObject,pdi);

  if prps<>nil then
    AddRectToRegion(prps.ClipRgn,DsgnR(ResizeObject));
  TprPreviewPanel(PreviewPanel).UpdateObjectsPropsForm;
  PreviewPanel.PrvNotifyReport;
  TprPreviewPanel(PreviewPanel).DoObjectResized(ResizeObject);
end;

procedure TprPreviewBox.GetPointInfoAt(x,y : integer; var Obj : TprExObjRecVersion; var EndPage : TprEndPage; var EndPagePDI : pprDrawInfo; var PointInfo : TprPointInfo; var ResizeMode : TprResizeType);
var
  i,j : integer;

  function AnalysObject(v : TprExObjRecVersion) : boolean;
  var
    r : TRect;
  begin
  Result := true;
  r := DsgnR(v);
  if Panel.DsgnAllowResizeObject(v) and GetResizeType(X,Y,r,ResizeMode) then
    begin
      PointInfo := piRegionResize;
      exit;
    end;
  if PointInRect(X,Y,r) then
    begin
      // mouse over object
      PointInfo := piRegionInside;
      exit;
    end;
  Result := false;
  end;

begin
Obj := nil;
EndPage := nil;
EndPagePDI := nil;
PointInfo := piNone;
ResizeMode := ppLeftTop;
if Panel.Report=nil then exit;

// check selected objects
for i:=0 to SelCount-1 do
  begin
    Obj := SelObjs[i];
    if AnalysObject(Obj) then
      begin
        Panel.DsgnObjPage(Obj,EndPage,EndPagePDI);
        exit;
      end;
  end;

for i:=0 to Panel.Report.EndPagesCount-1 do
  begin
    EndPage := TprEndPage(Panel.Report.EndPages[i]);
    EndPagePDI := pprDrawInfo(Panel.FCachedPages[i]);
    if PointInRect(X,Y,DsgnR(EndPagePDI)) then
      begin
        // page visible on screen and mouse over page
        // check all page objects
        for j:=EndPage.vl.Count-1 downto 0 do
          begin
            Obj := TprExObjRecVersion(EndPage.vl[j]);
            if AnalysObject(Obj) then
              exit;
          end;
        Obj := nil;
        exit;
      end;
  end;
Obj := nil;
EndPage := nil;
end;

function TprPreviewBox.GetStuckedOffs(Obj : TprExObjRecVersion; XOffs,YOffs : integer; AllowStuckSides : TprAllowStuckSidesSet; var StuckedX,StuckedY : integer) : boolean;
const
  aso : array [boolean] of integer = (-1,0);
var
  ep : TprEndPage;
  pdi : pprDrawInfo;
  i,j : integer;
  fOver : boolean;
  r,r1,r2 : TRect;

  function CheckVer : boolean;
  begin
  Result := ((r2.Top>=r.Top) and (r2.Top<=r.Bottom)) or
            ((r2.Bottom>=r.Top) and (r2.Bottom<=r.Bottom)) or
            ((r2.Top<r.Top) and (r2.Bottom>r.Bottom));
  end;
  
  function CheckHor : boolean;
  begin
  Result := ((r2.Left>=r.Left) and (r2.Left<=r.Right)) or
            ((r2.Right>=r.Left) and (r2.Right<=r.Right)) or
            ((r2.Left<r.Left) and (r2.Right>r.Right));
  end;

  procedure CheckObj(Obj : TprExObjRecVersion);
  var
    offs : integer;
  begin
  r2 := DsgnR(Obj);
  if CheckVer then
    begin
      if prassLeft in AllowStuckSides then
        begin
          offs := r2.Left-r.Left-aso[fOver];
          if (Abs(offs)<Panel.StuckOffs) and (Abs(offs)<Abs(StuckedX)) then
            StuckedX := offs;

          offs := r2.Right-r.Left-aso[fOver]-1;
          if (Abs(offs)<Panel.StuckOffs) and (Abs(offs)<Abs(StuckedX)) then
            StuckedX := offs;
        end;

      if prassRight in AllowStuckSides then
        begin
          if fOver then
            offs := r2.Left-r.Right+1
          else
            offs := r2.Left-r.Right-aso[fOver]-1;
          if (Abs(offs)<Panel.StuckOffs) and (Abs(offs)<Abs(StuckedX)) then
            StuckedX := offs;

          offs := r2.Right-r.Right+aso[fOver];
          if (Abs(offs)<Panel.StuckOffs) and (Abs(offs)<Abs(StuckedX)) then
            StuckedX := offs;
        end;
    end;
  if CheckHor then
    begin
      if prassTop in AllowStuckSides then
        begin
          offs := r2.Top-r.Top-aso[fOver];
          if (Abs(offs)<Panel.StuckOffs) and (Abs(offs)<Abs(StuckedY)) then
            StuckedY := offs;

          offs := r2.Bottom-r.Top-aso[fOver]-1;
          if (Abs(offs)<Panel.StuckOffs) and (Abs(offs)<Abs(StuckedY)) then
            StuckedY := offs;
        end;

      if prassBottom in AllowStuckSides then
        begin
          if fOver then
            offs := r2.Top-r.Bottom+1
          else
            offs := r2.Top-r.Bottom-aso[fOver]-1;
          if (Abs(offs)<Panel.StuckOffs) and (Abs(offs)<Abs(StuckedY)) then
            StuckedY := offs;
    
          offs := r2.Bottom-r.Bottom+aso[fOver];
          if (Abs(offs)<Panel.StuckOffs) and (Abs(offs)<Abs(StuckedY)) then
            StuckedY := offs;
        end;
    end;
  end;

begin
r := DsgnR(Obj);
OffsetRect(r,XOffs,YOffs);

fOver := prpsoStuckOver in Panel.StuckOptions;
StuckedX := MaxInt;
StuckedY := MaxInt;

r1 := Rect(HorzScrollBar.Position,VertScrollBar.Position,ClientWidth+HorzScrollBar.Position,ClientHeight+VertScrollBar.Position);
for i:=0 to Panel.Report.EndPagesCount-1 do
  begin
    ep := TprEndPage(Panel.Report.EndPages[i]);
    pdi := pprDrawInfo(Panel.FCachedPages[i]);
    if RectOverRect(pdi.bRect,r1) then
      begin
        // page is visible, check all objects
        for j:=0 to ep.VL.Count-1 do
          if Obj<>ep.VL[j] then
            CheckObj(ep.VL[j]);
      end;
  end;

if StuckedX=MaxInt then
  StuckedX := 0;
if StuckedY=MaxInt then
  StuckedY := 0;
Result := (StuckedY<>0) or (StuckedY<>0);
end;

procedure TprPreviewBox.DrawPageRect;
var
  r : TRect;
  br : HBRUSH;
begin
r := DsgnR(pprDrawInfo(Panel.FCachedPages[Panel.Report.EndPageIndex(ep)]));
if Selected then
  br := CreateSolidBrush(clBlue)
else
  br := CreateSolidBrush(clBlack);
FrameRect(DC,Rect(r.Left-1,r.Top-1,r.Right+1,r.Bottom+1),br);
DeleteObject(br);
end;

procedure TprPreviewBox.DrawAnime(DC : HDC; x,y : integer; CallWhileEvents : boolean);
var
  r : TRect;
  npn,opn : HPEN;
  pmm,oBottom,oLeft,oTop,oRight : integer;

  procedure DrawObjectsOffs(dx,dy : integer);
  var
    i : integer;
  begin
  for i:=0 to SelCount-1 do
    if Panel.DsgnAllowDragObject(SelObjs[i]) then
      with DsgnRSel(i) do
        DrawRect(DC,Rect(Left+dx,Top+dy,Right+dx,Bottom+dy));
  end;

begin
case FMouseMode of
  mmInsertObj:
    begin
      if (FStartX<>X) or (FStartY<>Y) then
        DrawFocusRect(DC,NormalizeRect(FStartX,FStartY,X,Y));
    end;
  mmSelect:
    begin
      if (FStartX<>X) or (FStartY<>Y) then
        DrawFocusRect(DC,NormalizeRect(FStartX,FStartY,X,Y));
    end;
  mmSelectedRegionsDrag:
    begin
      pmm := SetROP2(DC,R2_NOT);
      npn := CreatePen(PS_SOLID,1,clBlack);
      opn := SelectObject(DC,npn);

      if (FStartX<>X) or (FStartY<>Y) then
        DrawObjectsOffs(X-FStartX+FStuckedX,Y-FStartY+FStuckedY);

      if CallWhileEvents then
        begin
          r := Panel.GetSelectedRect;
          OffsetRect(r,X-FStartX+FStuckedX,Y-FStartY+FStuckedY);
          Panel.DoWhileObjectDrag(r);
        end;

      SelectObject(DC,opn);
      DeleteObject(npn);
      SetROP2(DC,pmm);
    end;
  mmRegionResize:
    begin
      if (FStartX<>X) or (FStartY<>Y) then
        begin
          CalcOffs(X-FStartX,Y-FStartY,FDownResizeMode,oTop,oLeft,oBottom,oRight,FStuckedX,FStuckedY);
          with DsgnR(FDownObject) do
            begin
              r := Rect(Left+oLeft,Top+oTop,Right+oRight,Bottom+oBottom);
              DrawFocusRect(DC,r);
              if CallWhileEvents then
                Panel.DoWhileObjectResize(r);
            end;
        end;
    end;
  mmSelectedResize:
    begin
    end;
end;
end;

procedure TprPreviewBox.HideDrawing(DC : HDC);
begin
FSelectionDrawObject.HideSelection(DC);
end;

procedure TprPreviewBox.ShowDrawing(DC : HDC);
begin
FSelectionDrawObject.ShowSelection(DC);
end;

procedure TprPreviewBox.InvertObject(DC : HDC; ep : TprEndPage; v : TprExObjRecVersion);
begin
InvertRect(DC,DsgnR(v));
end;

procedure TprPreviewBox.InternalPaint;
var
  v : TprExObjRecVersion;
  ep : TprEndPage;
  pdi : pprDrawInfo;
  nbr : HBRUSH;
  npn,opn : HPEN;
  rcPaint : TRect;
  OldOrgEx : TPoint;
  hsb,vsb,i,j : integer;
  r1,r2,r3,r4 : TRect;
  ObjsRgn,BackRgn,TempRgn : HRGN;
begin
if Panel.Report=nil then
  begin
    nbr := CreateSolidBrush(clWhite);
    FillRgn(DC,Rgn,nbr);
    DeleteObject(nbr);
  end
else
  begin
    FSelectionDrawObject.HideSelectionEx(DC,Rgn);

    hsb := HorzScrollBar.Position;
    vsb := VertScrollBar.Position;
    if GetRgnBox(rgn,rcPaint)=NULLREGION then
      rcPaint := Rect(0,0,ClientWidth,ClientHeight);
    r1 := Rect(rcPaint.Left+hsb,rcPaint.Top+vsb,rcPaint.Right+hsb,rcPaint.Bottom+vsb);
    npn := CreatePen(PS_SOLID,1,clBlack);
    opn := SelectObject(DC,npn);
    SelectClipRgn(DC,rgn);

    // erase background
    BackRgn := CreateRectRgnIndirect(rcPaint);
    for i:=0 to Panel.Report.EndPagesCount-1 do
      begin
        pdi := pprDrawInfo(Panel.FCachedPages[i]);
        if RectOverRect(pdi.bRect,r1) then
          begin
            TempRgn := CreateRectRgn(pdi.bRect.Left-hsb-1,pdi.bRect.Top-vsb-1,
                                     pdi.bRect.Right-hsb+1,pdi.bRect.Bottom-vsb+1);
            CombineRgn(BackRgn,BackRgn,TempRgn,RGN_DIFF);
            DeleteObject(TempRgn);
          end;
      end;
    nbr := CreateSolidBrush(GetRGBColor(clBtnFace));
    FillRgn(DC,BackRgn,nbr);
    DeleteObject(nbr);
    DeleteObject(BackRgn);

    for i:=0 to Panel.Report.EndPagesCount-1 do
      begin
        ep := TprEndPage(Panel.Report.EndPages[i]);
        pdi := pprDrawInfo(Panel.FCachedPages[i]);
        if RectOverRect(pdi.bRect,r1) then
          begin
            // draw page
            r2 := Rect(rcPaint.Left-pdi.bRect.Left+hsb,
                       rcPaint.Top-pdi.bRect.Top+vsb,
                       rcPaint.Right-pdi.bRect.Left+hsb,
                       rcPaint.Bottom-pdi.bRect.Top+vsb);

            // draw background
            BackRgn := CreateRectRgn(pdi.bRect.Left-hsb,pdi.bRect.Top-vsb,
                                     pdi.bRect.Right-hsb,pdi.bRect.Bottom-vsb);
            ObjsRgn := CreateRectRgn(0,0,0,0);
            CombineRgn(BackRgn,BackRgn,Rgn,RGN_AND);
            for j:=0 to ep.VL.Count-1 do
              begin
                v := TprExObjRecVersion(ep.vl[j]);
                r3 := GetRealRect(v,pdi);
                r4 := r3;
                OffsetRect(r3,pdi.bRect.Left-hsb,pdi.bRect.Top-vsb);

                TempRgn := CreateRectRgnIndirect(r3);
                CombineRgn(BackRgn,BackRgn,TempRgn,RGN_DIFF);
                if RectOverRect(r2,r4) then
                  CombineRgn(ObjsRgn,ObjsRgn,TempRgn,RGN_OR);
                DeleteObject(TempRgn);
              end;

            // Grid
            if Panel.DsgnCanUserEdit and Panel.ShowGrid and (Panel.GridSize>1) then
              DrawGrid(DC,
                       BackRgn,
                       FGridBitmap,
                       Panel.GridSize,
                       Panel.GridSize,
                       Rect(pdi.bRect.Left-hsb,
                            pdi.bRect.Top-vsb,
                            pdi.bRect.Right-hsb,
                            pdi.bRect.Bottom-vsb),
                       pdi.bRect.Left-hsb,
                       pdi.bRect.Top-vsb)
            else
              begin
                nbr := CreateSolidBrush(clWhite);
                FillRgn(DC,BackRgn,nbr);
                DeleteObject(nbr);
              end;
            DeleteObject(BackRgn);
    
            // fill ObjsRgn
            nbr := CreateSolidBrush(clWhite);
            FillRgn(DC,ObjsRgn,nbr);
            DeleteObject(nbr);
            DeleteObject(ObjsRgn);
    
            // page borders
            DrawPageRect(DC,ep, ep = Panel.ActivePage);
            SetViewportOrgEx(DC,pdi.bRect.Left-hsb,pdi.bRect.Top-vsb,@OldOrgEx);
            // now - draw objects
            for j:=0 to ep.vl.Count-1 do
              begin
                v := TprExObjRecVersion(ep.vl[j]);
                r4 := GetRealRect(v,pdi);
                if RectOverRect(r2,r4) then
                  begin
                    v.Draw(DC, pdi);
                    if v=FLastHighlightedObject then
                      InvertRect(DC,r4);
                  end;
              end;
            SetViewportOrgEx(DC,OldOrgEx.X,OldOrgEx.Y,nil);
          end;
      end;

    SelectObject(DC,opn);
    SelectClipRgn(DC,0);
    DeleteObject(npn);
    
    FSelectionDrawObject.ShowSelectionEx(DC,Rgn);
  end;
end;

procedure TprPreviewBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ep : TprEndPage;
  pdi : pprDrawInfo;
  FDownSelected : boolean;
  PreviewUserData : TprPreviewUserData;
begin
if FDblClick then
  begin
    FDblClick := false;
    exit;
  end;
SetFocus;
if Panel.Report=nil then exit;

GetPointInfoAt(X,Y,FDownObject,ep,pdi,FDownPointInfo,FDownResizeMode);
if FDownObject<>nil then
  PreviewUserData := FDownObject.PreviewUserData
else
  PreviewUserData := nil;
Panel.DoPreviewMouseDown(PreviewUserData,X,Y,Shift);

if ((ssLeft in Shift) and (not Panel.DsgnCanUserEdit)) or
   (([ssLeft,ssCtrl,ssShift]*Shift=[ssLeft,ssCtrl,ssShift]) and Panel.DsgnCanUserEdit) then
  begin
    Cursor := FScrollCursor;
    SetCursor(Screen.Cursors[FScrollCursor]);
    FScrollLastX := X;
    FScrollLastY := Y;
    exit;
  end;

//////////////////
// Now - Designer
//////////////////
if not Panel.DsgnCanUserEdit then exit;

Panel.ActivePage := ep;

if [ssLeft,ssRight]*Shift=[] then exit; // left mouse button or right mouse button not pressed

// starting mouse coord`s
if Panel.UseGrid then
  begin
    FLastX := ATG(X,Panel.GridSize);
    FLastY := ATG(Y,Panel.GridSize);
  end
else
  begin
    FLastX := X;
    FLastY := Y;
  end;
FStartX := FLastX;
FStartY := FLastY;
FStartRealX := X;
FStartRealY := Y;
FFirstMoveAfterDown := true;
FDownSelected := Panel.IsObjSelected(FDownObject);

// clear current selection
if Panel.FCurClassRef<>nil then
  begin
    // mode - inserting object
    if ssLeft in Shift then
      begin
        HideDrawing(Canvas.Handle);
        FMouseMode := mmInsertObj;
      end;
    exit;
  end;

if ssRight in Shift then
  begin
    // Right mouse button pressed
    if FDownObject<>nil then
      begin
        if ssShift in Shift then
          begin
            if not FDownSelected then
              Panel.AddSelectObject(FDownObject);
          end
        else
          Panel.SelectObject(FDownObject);
      end;
    FMouseMode := mmNone;
    exit;
  end;

// LEFT mouse button pressed
if FDownObject=nil then
  begin
    HideDrawing(Canvas.Handle);
    Panel.InternalClearSelected;
    FMouseMode := mmSelect;
    Panel.DoSelectionChanged;
    exit;
  end;

// FDownObject<>nil
if ssShift in Shift then
  begin
    if FDownSelected then
      Panel.DeselectObject(FDownObject)
    else
      Panel.AddSelectObject(FDownObject);
    FMouseMode := mmNone;
    exit;
  end;

HideDrawing(Canvas.Handle);
if FDownSelected then
  begin
    if SelCount=1 then
      begin
        case FDownPointInfo of
          piRegionInside:
            begin
              // inside object
              if Panel.DsgnAllowDragObject(FDownObject) then
                FMouseMode := mmSelectedRegionsDrag
              else
                FMouseMode := mmSelect;
            end;
          piRegionResize:
            begin
              FMouseMode := mmRegionResize;
            end;
          else
            FMouseMode := mmNone;
        end;
      end
    else
      begin
        FMouseMode := mmSelectedRegionsDrag;
      end;
  end
else
  begin
    Panel.InternalClearSelected;
    if Panel.DsgnAllowDragObject(FDownObject) then
      begin
        Panel.InternalSelectObj(FDownObject);
        ShowDrawing(Canvas.Handle);
        HideDrawing(Canvas.Handle);
        FMouseMode := mmSelectedRegionsDrag;
      end
    else
      FMouseMode := mmSelect;
    Panel.DoSelectionChanged;
  end;
end;

procedure TprPreviewBox.DblClick;
var
  p : TPoint;
  v : TprExObjRecVersion;
  ep : TprEndPage;
  pdi : pprDrawInfo;
  PointInfo : TprPointInfo;
  ResizeMode : TprResizeType;
  PreviewUserData : TprPreviewUserData;
begin
FDblClick := true;
if Panel.Report=nil then exit;
FFirstMoveAfterDown := false;
GetCursorPos(p);
p := ScreenToClient(p);
GetPointInfoAt(p.X,p.Y,v,ep,pdi,PointInfo,ResizeMode);
if v<>nil then
  PreviewUserData := v.PreviewUserData
else
  PreviewUserData := nil;
if (v<>nil) and Panel.DsgnCanUserEdit then
  begin
    if not Panel.VisibleObjectsPropsForm then
      Panel.VisibleObjectsPropsForm := true;
  end;
Panel.DoPreviewDblClick(PreviewUserData,p.x,p.y);
end;

procedure TprPreviewBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  v : TprExObjRecVersion;
  vc : TprObjVersionClass;
  ep : TprEndPage;
  pdi : pprDrawInfo;
  ClipRgn : HRGN;
  r,epr,r2 : TRect;
  dx,dy,oLeft,oTop,oRight,oBottom,i,j,RealX,RealY : integer;
begin
if Cursor=FScrollCursor then
  Cursor := FNormalCursor;

//////////////////
// Now - Designer
//////////////////
if (Panel.Report=nil) or (not Panel.DsgnCanUserEdit) then exit;
RealX := X;
RealY := Y;
if Panel.UseGrid and (FStuckedX=0) and (FStuckedY=0) then
  begin
    X := ATG(X,Panel.GridSize);
    Y := ATG(Y,Panel.GridSize);
  end;

DrawAnime(Canvas.Handle,X,Y,false); // hide animation

ClipRgn := 0;
case FMouseMode of
  mmInsertObj:
    begin
      vc := TprObjVersionClass(GetClass(Panel.FCurClassRef.ClassName+'RecVersion'));
      if vc=nil then
        begin
          ShowDrawing(Canvas.Handle);
          exit;
        end;
      // insert object
      if FFirstMoveAfterDown then
        r := Rect(x,y,x+100,y+40)
      else
        r := NormalizeRect(FStartX,FStartY,X,Y);
      // r - sizes of inserting object
      // find page or rect [R]
      i := 0;
      while (i<Panel.Report.EndPagesCount) and
            (not RectInRect(r,DsgnR(pprDrawInfo(Panel.FCachedPages[i])))) do Inc(i);
      if i>=Panel.Report.EndPagesCount then
        begin
          ShowDrawing(Canvas.Handle);
          exit;
        end;
      ep := TprEndPage(Panel.Report.EndPages[i]);
      pdi := pprDrawInfo(Panel.FCachedPages[i]);
      epr := DsgnR(pdi);
      
      if Panel.UseGrid then
        begin
          r.Top := ATG(r.Top,Panel.GridSize);
          r.Left := ATG(r.Left,Panel.GridSize);
          r.Right := ATG(r.Right,Panel.GridSize)+1;
          r.Bottom := ATG(r.Bottom,Panel.GridSize)+1;
        end;

      r2 := Rect(r.Left-epr.Left,r.Top-epr.Top,r.Right-epr.Left,r.Bottom-epr.Top);
      v := TprExObjRecVersion(vc.Create(nil));
      v.InitInDesigner;
      SetRealRect(r2,v,pdi);
      ep.VL.Add(v);
      ClipRgn := CreateRectRgn(r2.Left+epr.Left,r2.Top+epr.Top,r2.Right+epr.Left,r2.Bottom+epr.Top);
      
      Panel.InternalClearSelected;
      Panel.InternalSelectObj(v);
      
      ShowDrawing(Canvas.Handle);
      PreviewPanel.PrvNotifyReport;
      Panel.DoSelectionChanged;
    end;
  mmSelect:
    begin
      if Shift<>[ssShift] then
        Panel.InternalClearSelected;

      r := NormalizeRect(FStartRealX,FStartRealY,RealX,RealY);
      for i:=0 to Panel.Report.EndPagesCount-1 do
        begin
          ep := TprEndPage(Panel.Report.EndPages[i]);
          pdi := pprDrawInfo(Panel.FCachedPages[i]);
          if RectOverRect(r,DsgnR(pdi)) then
            begin
              // check this page
              for j:=0 to ep.vl.Count-1 do
                begin
                  v := TprExObjRecVersion(ep.vl[j]);
                  if RectOverRect(r,DsgnR(v)) then
                    Panel.InternalSelectObj(v);
                end;
            end;
        end;
      // draw selection
      ShowDrawing(Canvas.Handle);
      Panel.DoSelectionChanged;
    end;
  mmRegionResize:
    begin
      // resize object
      CalcOffs(x-FStartX,y-FStartY,FDownResizeMode,oTop,oLeft,oBottom,oRight,FStuckedX,FStuckedY);
      r := DsgnR(FDownObject);
      if (r.Left+oLeft<r.Right+oRight) and (r.Top+oTop<r.Bottom+oBottom) then
        begin
          AddRectToRegion(ClipRgn,r);
          DoResize(FDownObject,oTop,oLeft,oBottom,oRight,nil);
          AddRectToRegion(ClipRgn,DsgnR(FDownObject));
        end;
      ShowDrawing(Canvas.Handle);
    end;
  mmSelectedRegionsDrag:
    begin
      dx := X-FStartX+FStuckedX;
      dy := Y-FStartY+FStuckedY;
      if (dx<>0) or (dy<>0) then
        for i:=0 to SelCount-1 do
          begin
            v := SelObjs[i];
            AddRectToRegion(ClipRgn,DsgnR(v));
            DoDrag(v,dx,dy,nil);
            AddRectToRegion(ClipRgn,DsgnR(v));
          end;
      ShowDrawing(Canvas.Handle);
    end;
end;

if ClipRgn<>0 then
  begin
    InternalPaint(Canvas.Handle,ClipRgn);
    DeleteObject(ClipRgn);
  end;
FMouseMode := mmNone;
end;

procedure TprPreviewBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  v : TprExObjRecVersion;
  ep : TprEndPage;
  pdi : pprDrawInfo;
  cur : TCursor;
  Selected : boolean;
  PointInfo : TprPointInfo;
  ResizeMode : TprResizeType;
  PreviewUserData : TprPreviewUserData;
  HighlightObject : boolean;
begin
if Panel.Report=nil then exit;
FFirstMoveAfterDown := false;
GetPointInfoAt(x,y,v,ep,pdi,PointInfo,ResizeMode);
HighlightObject := false;
if v<>nil then
  PreviewUserData := v.PreviewUserData
else
  PreviewUserData := nil;
cur := Low(TCursor);
Panel.DoPreviewMouseMove(PreviewUserData,X,Y,cur,HighlightObject);

if v<>FLastHighlightedObject then
  begin
    if (FLastHighlightedObject<>nil) and (FLastHighlightedEndPage<>nil) then
      InvertObject(Canvas.Handle,FLastHighlightedEndPage,FLastHighlightedObject);
    if (v<>nil) and HighlightObject then
      begin
        InvertObject(Canvas.Handle,ep,v);
        FLastHighlightedObject := v;
        FLastHighlightedEndPage := ep;
      end
    else
      FLastHighlightedObject := nil;
  end;

if not Panel.DsgnCanUserEdit or ([ssShift,ssCtrl]*Shift=[ssShift,ssCtrl]) then
  begin
    if cur=Low(Cursor) then
      begin
        if ssLeft in Shift then
          begin
            if Cursor<>FScrollCursor then
              Cursor := FScrollCursor;
            if (FScrollLastX<>X) or (FScrollLastY<>Y) then
              begin
                HorzScrollBar.Position := HorzScrollBar.Position+FScrollLastX-X;
                VertScrollBar.Position := VertScrollBar.Position+FScrollLastY-Y;
                FScrollLastX := X;
                FScrollLastY := Y;
              end;
          end
        else
          if Cursor<>FNormalCursor then
            Cursor := FNormalCursor
      end
    else
      if cur<>Cursor then
        Cursor := cur;
  end
else
  begin
    DrawAnime(Canvas.Handle,FLastX,FLastY,false);
    FStuckedX := 0;
    FStuckedY := 0;

    //
    case FMouseMode of
      mmNone:
        begin
        end;
      mmSelectedRegionsDrag:
        begin
          if (Panel.StuckMode=prsmAlways) or ((Panel.StuckMode=prsmCtrlButton) and (GetKeyState(VK_CONTROL)<0)) then
            GetStuckedOffs(SelObjs[0],X-FStartX,Y-FStartY,[prassLeft,prassTop,prassRight,prassBottom],FStuckedX,FStuckedY)
          else
            if Panel.UseGrid and ((ssLeft in Shift) or (ssRight in Shift)) then
              begin
                X := ATG(X,Panel.GridSize);
                Y := ATG(Y,Panel.GridSize);
              end;
        end;
      mmRegionResize:
        begin
          if (Panel.StuckMode=prsmAlways) or ((Panel.StuckMode=prsmCtrlButton) and (GetKeyState(VK_CONTROL)<0)) then
            begin
              GetStuckedOffs(SelObjs[0],X-FStartX,Y-FStartY,aStuckSides[FDownResizeMode],FStuckedX,FStuckedY)
            end
          else
            if Panel.UseGrid and ((ssLeft in Shift) or (ssRight in Shift)) then
              begin
                X := ATG(X,Panel.GridSize);
                Y := ATG(Y,Panel.GridSize);
              end;
        end;
      else
        if Panel.UseGrid and ((ssLeft in Shift) or (ssRight in Shift)) then
          begin
            X := ATG(X,Panel.GridSize);
            Y := ATG(Y,Panel.GridSize);
          end;
    end;

    DrawAnime(Canvas.Handle,X,Y,true); // draw new animation
    FLastX := X;
    FLastY := Y;

    // Update Mouse cursor
    if FMouseMode=mmNone then
      begin
        if Panel.FCurClassRef<>nil then
          cur := crCross
        else
          begin
            Selected := Panel.IsObjSelected(v);
            if PointInfo=piSelectedResize then
              cur := prResizeCursors[ResizeMode]
            else
              if (SelCount>1) and (v<>nil) and Selected then
                cur := crMultiDrag
              else
                if (v<>nil) and Panel.DsgnAllowDragObject(v) and ((not Selected) or (Selected and (PointInfo=piRegionInside))) then
                  cur := crDrag
                else
                  if (PointInfo=piRegionResize) and Selected and Panel.DsgnAllowResizeObject(v) then
                    cur := prResizeCursors[ResizeMode];
          end;
      end;
    if Cursor<>cur then
      Cursor := cur;
  end;
end;

procedure TprPreviewBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
if Panel.Report=nil then exit;

if ((Shift=[ssCtrl]) and (Key=ord('C'))) or
   ((Shift=[ssCtrl]) and (Key=VK_INSERT)) then
  begin
    Panel.Copy;
    Key := 0;
    exit;
  end;

if ((Shift=[ssCtrl]) and (Key=ord('V'))) or
   ((Shift=[ssShift]) and (Key=VK_INSERT)) then
  begin
    Panel.Paste;
    Key := 0;
    exit;
  end;

if ((Shift=[ssCtrl]) and (Key=ord('X'))) or
   ((Shift=[ssShift]) and (Key=VK_DELETE)) then
  begin
    Panel.Cut;
    Key := 0;
    exit;
  end;

if ((Shift=[ssCtrl]) and (Key=VK_DELETE)) or
   ((Shift=[]) and (Key=VK_DELETE)) then
  begin
    Panel.DeleteSelectedObjects;
    Key := 0;
    exit;
  end;

if ((Shift=[ssCtrl]) and (Key=ord('G'))) then
  begin
    Panel.ShowGrid := not Panel.ShowGrid;
    Key := 0;
    exit;
  end;

if ((Shift=[ssCtrl]) and (Key=ord('F'))) or
   ((Shift=[ssCtrl]) and (Key=VK_F3)) then
  begin
    Panel.Find;
    Key := 0;
    exit;
  end;

if ((Shift=[]) and (Key=VK_F3)) then
  begin
    Panel.FindNext;
    Key := 0;
    exit;
  end;

if ((Shift=[ssShift]) and (Key=VK_F3)) then
  begin
    Panel.FindPrior;
    Key := 0;
    exit;
  end;

if ((Shift=[ssCtrl]) and (Key=ord('E'))) then
  begin
    Panel.EditCurrentPage;
    Key := 0;
    exit;
  end;

if ((Shift=[ssAlt]) and (Key=VK_RETURN)) then
  begin
    Panel.VisibleObjectsPropsForm := true;
    Key := 0;
    exit;
  end;

if Shift=[] then
  case Key of
    VK_DOWN:
      VertScrollBar.Position := VertScrollBar.Position+VertScrollBar.Increment;
    VK_UP:
      VertScrollBar.Position := VertScrollBar.Position-VertScrollBar.Increment;
    VK_RIGHT:
      HorzScrollBar.Position := HorzScrollBar.Position+HorzScrollBar.Increment;
    VK_LEFT:
      HorzScrollBar.Position := HorzScrollBar.Position-HorzScrollBar.Increment;
    VK_NEXT:
      VertScrollBar.Position := VertScrollBar.Position+VertScrollPageSize;
    VK_PRIOR:
      VertScrollBar.Position := VertScrollBar.Position-VertScrollPageSize;
    else
      inherited;
  end
else
  if Shift=[ssCtrl] then
    case Key of
      VK_NEXT:
        VertScrollBar.Position := VertScrollBar.Range;
      VK_PRIOR:
        VertScrollBar.Position := 0;
    end;
end;

procedure TprPreviewBox._BeginPaint(var prps : rprPaintStruct);
begin
prps.ClipRgn := 0;
prps.ClipState := [];
HideDrawing(Canvas.Handle);
end;

procedure TprPreviewBox._EndPaint(const prps : rprPaintStruct);
begin
ShowDrawing(Canvas.Handle);
if prps.ClipRgn<>0 then
  begin
    InternalPaint(Canvas.Handle,prps.ClipRgn);
    DeleteObject(prps.ClipRgn);
  end;
end;

procedure TprPreviewBox.WMEraseBkgnd(var Msg : TWmEraseBkgnd);
begin
Msg.Result := 1;
end;

procedure TprPreviewBox.WMPaint(var Msg : TWMPaint);
var
  PS : TPaintStruct;
  Rgn : HRGN;
begin
Msg.Result := 1;
BeginPaint(Handle,PS);
rgn := CreateRectRgnIndirect(ps.rcPaint);
InternalPaint(Canvas.Handle,Rgn);
DeleteObject(rgn);
EndPaint(Handle,PS);
end;

procedure TprPreviewBox.WMSysKeyChar(var Msg : TWMSysChar);
begin
if (Msg.CharCode=VK_RETURN) and ((Msg.KeyData and $20000000)<>0) then
  begin
    Msg.Result := 1; // !!! disable Beep
  end
else
  begin
    Msg.Result := 0;
    inherited;
  end;
end;

procedure TprPreviewBox.WMMouseWheel(var Msg : TWMMouseWheel);
var
  wParam : cardinal;
  MsgScroll : cardinal;
begin
if (Msg.Keys and MK_CONTROL)<>0 then
  MsgScroll := WM_HSCROLL
else
  MsgScroll := WM_VSCROLL;

if (Msg.Keys and MK_SHIFT)<>0 then
  begin
    if Msg.WheelDelta>0 then
      wParam := SB_PAGELEFT
    else
      wParam := SB_PAGERIGHT;
  end
else
  begin
    if Msg.WheelDelta>0 then
      wParam := SB_LINELEFT
    else
      wParam := SB_LINERIGHT;
  end;
PostMessage(Handle,MsgScroll,wParam,0);
inherited;
end;

/////////////////////////////////////////////////
//
// TprPreviewPanel
//
/////////////////////////////////////////////////
constructor TprPreviewPanel.Create(AOwner : TComponent);
begin
inherited;
FCachedPages := TList.Create;
Frpdi.FindList := TList.Create;
FScaleMode := smPageWidth;
FPopupInsertObjectMenu := true;
FOpacityObjectsPropsForm := 100;
FScaleMul := 100;
FScaleDiv := 100;
FPagesPerHeight := 1;
FPagesPerWidth := 1;

FGridSize := 8;
UpdateGridBitmap(Box.FGridBitmap,FGridSize,FGridSize);

FScaleMode := smPageWidth;
FPagesPerWidth := 2;
FPagesPerHeight := 1;

FSelObjs := TList.Create;

if not (csDesigning in ComponentState) then
  begin
    FFindDialog := TFindDialog.Create(Self);
    FFindDialog.Options := FFindDialog.Options + [frHideWholeWord];
    FFindDialog.OnFind := OnFindDialogFind;
    FObjectsPropsForm := TprPreviewObjectsPropsForm.Create(Self);
    FObjectsPropsForm.PreviewPanel := Self;
  end;
end;

destructor TprPreviewPanel.Destroy;
begin
ClearCachedPages;
FCachedPages.Free;
ClearFindList;
Frpdi.FindList.Free;
FSelObjs.Free;
inherited;
end;

function TprPreviewPanel.GetReport: TprReport;
begin
  Result := TprReport(inherited Report);
end;

procedure TprPreviewPanel.SetReport(Value: TprReport);
begin
  inherited Report := Value;
end;

function TprPreviewPanel.CreatePreviewBox : TprCustomPreviewBox;
begin
  Result := TprPreviewBox.Create(Self);
end;

function TprPreviewPanel.GetBox : TprPreviewBox;
begin
  Result := TprPreviewBox(FPreviewBox);
end;

procedure TprPreviewPanel.CalcMaxSizes;
var
  i : integer;
begin
if ReportEmpty then
  begin
    FMaxWidth := 0;
    FMaxHeight := 0;
  end
else
  begin
    FMaxWidth := TprEndPage(Report.EndPages[0]).PixelPageWidth;
    FMaxHeight := TprEndPage(Report.EndPages[0]).PixelPageHeight;
    for i:=1 to Report.EndPagesCount-1 do
      with TprEndPage(Report.EndPages[i]) do
        begin
          if FMaxWidth<PixelPageWidth then
            FMaxWidth := PixelPageWidth;
          if FMaxHeight<PixelPageHeight then
            FMaxHeight := PixelPageHeight;
        end;
  end;
end;

function TprPreviewPanel.GetSelectedRect : TRect;
var
  i : integer;
begin
Result := Rect(MaxInt,MaxInt,-MaxInt,-MaxInt);
for i:=0 to SelCount-1 do
  with DsgnRSel(i) do
    begin
      if Left<Result.Left then
        Result.Left := Left;
      if Right>Result.Right then
        Result.Right := Right;
      if Top<Result.Top then
        Result.Top := Top;
      if Bottom>Result.Bottom then
        Result.Bottom := Bottom;
    end;
end;

procedure TprPreviewPanel.ClearCachedPages;
var
  i : integer;
begin
for i:=0 to FCachedPages.Count-1 do
  FreeMem(FCachedPages[i]);
FCachedPages.Clear;
end;

procedure TprPreviewPanel.ClearFindList;
begin
  FreeListItems(Frpdi.FindList);
  Frpdi.FindList.Clear;
end;

function TprPreviewPanel.GetSelObj(i : integer) : TprExObjRecVersion;
begin
Result := TprExObjRecVersion(FSelObjs[i]);
end;

function TprPreviewPanel.GetSelCount : integer;
begin
Result := FSelObjs.Count;
end;

function TprPreviewPanel.DsgnAllowDragObject(Obj : TprExObjRecVersion) : boolean;
begin
Result := true;
end;

function TprPreviewPanel.DsgnAllowResizeObject(Obj : TprExObjRecVersion) : boolean;
begin
Result := true;
end;

function TprPreviewPanel.GetRealRect(v : TprExObjRecVersion; pdi : pprDrawInfo) : TRect;
begin
Result := Box.GetRealRect(v,pdi);
end;

procedure TprPreviewPanel.SetRealRect(const Value : TRect; v : TprExObjRecVersion; pdi : pprDrawInfo);
begin
Box.SetRealRect(Value,v,pdi);
end;

function TprPreviewPanel.DsgnR(Obj : TprExObjRecVersion) : TRect;
var
  ep : TprEndPage;
  pdi : pprDrawInfo;
begin
DsgnObjPage(Obj,ep,pdi);
Result := GetRealRect(Obj,pdi);
if pdi<>nil then
  with DsgnR(pdi) do
    OffsetRect(Result,Left,Top);
end;

function TprPreviewPanel.DsgnRSel(i : integer) : TRect;
begin
Result := DsgnR(SelObjs[i]);
end;

function TprPreviewPanel.DsgnWSel(i : integer) : integer;
begin
with DsgnRSel(i) do
  Result := Right-Left;
end;

function TprPreviewPanel.DsgnHSel(i : integer) : integer;
begin
with DsgnRSel(i) do
  Result := Bottom-Top;
end;

function TprPreviewPanel.DsgnR(PagePDI : pprDrawInfo) : TRect;
begin
Result := Rect(PagePDI.bRect.Left-HorScrollBoxPos,
               PagePDI.bRect.Top-VerScrollBoxPos,
               PagePDI.bRect.Right-HorScrollBoxPos,
               PagePDI.bRect.Bottom-VerScrollBoxPos);
end;

procedure TprPreviewPanel.DsgnObjPage(Obj : TprExObjRecVersion; var ep : TprEndPage; var pdi : pprDrawInfo);
var
  i : integer;
begin
for i:=0 to Report.EndPagesCount-1 do
  if TprEndPage(Report.EndPages[i]).VL.IndexOf(Obj)<>-1 then
    begin
      ep := TprEndPage(Report.EndPages[i]);
      pdi := pprDrawInfo(FCachedPages[i]);
      exit;
    end;
ep := nil;
pdi := nil;
end;

function TprPreviewPanel.DsgnObjPDI(Obj : TprExObjRecVersion) : pprDrawInfo;
var
  i : integer;
begin
for i:=0 to Report.EndPagesCount-1 do
  if TprEndPage(Report.EndPages[i]).VL.IndexOf(Obj)<>-1 then
    begin
      Result := pprDrawInfo(FCachedPages[i]);
      exit;
    end;
Result := nil;
end;

function TprPreviewPanel.DsgnPagePDI(Page : TprEndPage) : pprDrawInfo;
var
  i : integer;
begin
i := Report.EndPageIndex(Page);
if i=-1 then
  Result := nil
else
  Result := pprDrawInfo(FCachedPages[i]);
end;

function TprPreviewPanel.GetVerScrollBoxPos : integer;
begin
Result := FPreviewBox.VertScrollBar.Position;
end;

function TprPreviewPanel.GetHorScrollBoxPos : integer;
begin
Result := FPreviewBox.HorzScrollBar.Position;
end;

procedure TprPreviewPanel.InternalClearSelected;
begin
FSelObjs.Clear;
end;

function TprPreviewPanel.GetVisibleObjectsPropsForm : boolean;
begin
Result := (FObjectsPropsForm<>nil) and FObjectsPropsForm.Visible;
end;

procedure TprPreviewPanel.SetVisibleObjectsPropsForm(Value : boolean);
begin
if FObjectsPropsForm<>nil then
  begin
    FObjectsPropsForm.Visible := Value;
    if FObjectsPropsForm.Visible then
      FObjectsPropsForm.SetFocusOnFirstControl;
  end;
end;

procedure TprPreviewPanel.InternalSelectObj(Obj : TprExObjRecVersion);
begin
if FSelObjs.IndexOf(Obj)=-1 then
  FSelObjs.Add(Obj);
end;

procedure TprPreviewPanel.InternalDeSelectObj(Obj : TprExObjRecVersion);
begin
FSelObjs.Remove(Obj);
end;

function TprPreviewPanel.IsObjSelected(Obj : TprExObjRecVersion) : boolean;
begin
Result := FSelObjs.IndexOf(Obj)<>-1;
end;

procedure TprPreviewPanel.SelectObject(Obj : TprExObjRecVersion);
begin
with Box do
  begin
    HideDrawing(Canvas.Handle);
    InternalClearSelected;
    InternalSelectObj(Obj);
    ShowDrawing(Canvas.Handle);
    DoSelectionChanged;
  end;
end;

procedure TprPreviewPanel.AddSelectObject(Obj : TprExObjRecVersion);
begin
with Box do
  begin
    HideDrawing(Canvas.Handle);
    InternalSelectObj(Obj);
    ShowDrawing(Canvas.Handle);
    DoSelectionChanged;
  end;
end;

procedure TprPreviewPanel.DeSelectObject(Obj : TprExObjRecVersion);
begin
with Box do
  begin
    HideDrawing(Canvas.Handle);
    InternalDeSelectObj(Obj);
    ShowDrawing(Canvas.Handle);
    DoSelectionChanged;
  end;
end;

procedure TprPreviewPanel.ClearSelection;
begin
with Box do
  begin
    HideDrawing(Canvas.Handle);
    InternalClearSelected;
    ShowDrawing(Canvas.Handle);
    DoSelectionChanged;
  end;
end;

procedure TprPreviewPanel.InsertObject(ObjClassRef : TprObjClass);
begin
if (ObjClassRef=nil) or (GetClass(ObjClassRef.ClassName+'RecVersion')=nil) then
  FCurClassRef := nil
else
  FCurClassRef := ObjClassRef;
end;

procedure TprPreviewPanel.UpdateSelectedObjects;
var
  Rgn : HRGN;

  procedure MakeSelectedObjectsRegion;
  var
    i : integer;
  begin
  for i:=0 to SelCount-1 do
    AddRectToRegion(Rgn,DsgnRSel(i),RGN_OR);
  end;

begin
Rgn := 0;

Box.HideDrawing(Box.Canvas.Handle);
//MakeSelectedObjectsRegion;
//UpdateInternalDataSelectedObjects;
MakeSelectedObjectsRegion;
Box.ShowDrawing(Box.Canvas.Handle);

if Rgn<>0 then
  begin
    Box.InternalPaint(Box.Canvas.Handle,Rgn);
    DeleteObject(Rgn);
  end;
end;

procedure TprPreviewPanel.AlignAction(ActionCode : TprAlignActionCode);
var
  l : TList;
  r : TRect;
  prps : rprPaintStruct;
  dcLeft,dcRight,dcTop,dcBottom : TprExObjRecVersion;
  i,Width,Delta,Right,Left,Height,Bottom,Top,j,Min,iMin,Cur,Minw,Minh,Maxw,Maxh : integer;

  function GetR(dc : TObject) : TRect;
  begin
  Result := DsgnR(TprExObjRecVersion(dc));
  end;

begin
Box._BeginPaint(prps);
case ActionCode of
  aacHToLeft:
    begin
      for i:=1 to SelCount-1 do
        if DsgnAllowDragObject(SelObjs[i]) then
          Box.DoDrag(SelObjs[i],
                     DsgnRSel(0).Left-DsgnRSel(i).Left,
                     0,
                     @prps);
    end;
  aacHToRight:
    begin
      for i:=1 to SelCount-1 do
        if DsgnAllowDragObject(SelObjs[i]) then
          Box.DoDrag(SelObjs[i],
                     DsgnRSel(0).Right-DsgnRSel(i).Right,
                     0,
                     @prps);
    end;
  aacVToTop:
    begin
      for i:=1 to SelCount-1 do
        if DsgnAllowDragObject(SelObjs[i]) then
          Box.DoDrag(SelObjs[i],
                     0,
                     DsgnRSel(0).Top-DsgnRSel(i).Top,
                     @prps);
    end;
  aacVToBottom:
    begin
      for i:=1 to SelCount-1 do
        if DsgnAllowDragObject(SelObjs[i]) then
          Box.DoDrag(SelObjs[i],
                     0,
                     DsgnRSel(0).Bottom-DsgnRSel(i).Bottom,
                     @prps);
    end;
  aacVCenters:
    begin
      for i:=1 to SelCount-1 do
        if DsgnAllowDragObject(SelObjs[i]) then
          Box.DoDrag(SelObjs[i],
                     (DsgnRSel(0).Left+
                      (DsgnRSel(0).Right-DsgnRSel(0).Left) div 2-
                      DsgnRSel(i).Left-
                      (DsgnRSel(i).Right-DsgnRSel(i).Left) div 2),
                     0,
                     @prps);
    end;
  aacHCenters:
    begin
      for i:=1 to SelCount-1 do
        if DsgnAllowDragObject(SelObjs[i]) then
          Box.DoDrag(SelObjs[i],
                     0,
                     (DsgnRSel(0).Top+
                      (DsgnRSel(0).Bottom-DsgnRSel(0).Top) div 2-
                      DsgnRSel(i).Top-
                     (DsgnRSel(i).Bottom-DsgnRSel(i).Top) div 2),
                     @prps);
    end;
  aacHCenterInWindow:
    begin
      for i:=0 to SelCount-1 do
        if DsgnAllowDragObject(SelObjs[i]) then
          begin
            r := DsgnR(DsgnObjPDI(SelObjs[i]));
            Box.DoDrag(SelObjs[i],
                       (r.Right-r.Left-DsgnRSel(i).Right+DsgnRSel(i).Left) div 2-DsgnRSel(i).Left+r.Left,
                       0,
                       @prps);
          end;
    end;
  aacVCenterInWindow:
    begin
      for i:=0 to SelCount-1 do
        if DsgnAllowDragObject(SelObjs[i]) then
          begin
            r := DsgnR(DsgnObjPDI(SelObjs[i]));
            Box.DoDrag(SelObjs[i],
                       0,
                       (r.Bottom-r.Top-DsgnRSel(i).Bottom+DsgnRSel(i).Top) div 2-DsgnRSel(i).Top+r.Top,
                       @prps);
          end;
    end;
  aacWToSmall:
    begin
      Minw := DsgnWSel(0);

      for i:=1 to SelCount-1 do
        if Minw>DsgnWSel(i) then
          Minw := DsgnWSel(i);

      for i:=0 to SelCount-1 do
        Box.DoResize(SelObjs[i],
                     0,
                     0,
                     0,
                     Minw-DsgnWSel(i),
                     @prps);
    end;
  aacWToLarge:
    begin
      Maxw := DsgnWSel(0);

      for i:=1 to SelCount-1 do
        if Maxw<DsgnWSel(i) then
          Maxw := DsgnWSel(i);

      for i:=0 to SelCount-1 do
        Box.DoResize(SelObjs[i],
                     0,
                     0,
                     0,
                     Maxw-DsgnWSel(i),
                     @prps);
    end;
  aacHToSmall:
    begin
      Minh := DsgnHSel(0);

      for i:=1 to SelCount-1 do
        if Minh>DsgnHSel(i) then
          Minh := DsgnHSel(i);

      for i:=0 to SelCount-1 do
        Box.DoResize(SelObjs[i],
                     0,
                     0,
                     Minh-DsgnHSel(i),
                     0,
                     @prps);
    end;
  aacHToLarge:
    begin
      Maxh := DsgnHSel(0);

      for i:=1 to SelCount-1 do
        if Maxh<DsgnHSel(i) then
          Maxh := DsgnHSel(i);

      for i:=0 to SelCount-1 do
        Box.DoResize(SelObjs[i],
                     0,
                     0,
                     Maxh-DsgnHSel(i),
                     0,
                     @prps);
    end;
  aacAlignToGridLeftTop:
    begin
      for i:=0 to SelCount-1 do
        if DsgnAllowDragObject(SelObjs[i]) then
          begin
            Left := ATG(DsgnRSel(i).Left,GridSize);
            Top := ATG(DsgnRSel(i).Top,GridSize);
            Box.DoDrag(SelObjs[i],
                       Left-DsgnRSel(i).Left,
                       Top-DsgnRSel(i).Top,
                       @prps);
          end;
    end;
  aacAlignToGridAll:
    begin
      for i:=0 to FSelObjs.Count-1 do
        begin
          if DsgnAllowDragObject(SelObjs[i]) then
            begin
              Left := ATG(DsgnRSel(i).Left,GridSize);
              Top := ATG(DsgnRSel(i).Top,GridSize);
              Box.DoDrag(SelObjs[i],
                         Left-DsgnRSel(i).Left,
                         Top-DsgnRSel(i).Top,
                         @prps);
            end;

          if DsgnAllowResizeObject(SelObjs[i]) then
            begin
              Right := ATG(DsgnRSel(i).Right,GridSize);
              Bottom := ATG(DsgnRSel(i).Bottom,GridSize);
              Box.DoResize(SelObjs[i],
                           0,
                           0,
                           Bottom+1-DsgnRSel(i).Bottom,
                           Right+1-DsgnRSel(i).Right,
                           @prps);
            end;
        end;
    end;
  aacHSpaceEqually:
    begin
      l := TList.Create;
      try
        for i:=0 to SelCount-1 do
          if DsgnAllowDragObject(SelObjs[i]) then
            l.Add(SelObjs[i]);

        for i:=0 to l.Count-1 do
          begin
            iMin := i;
            Min := GetR(l[i]).Left;
            for j:=i+1 to l.Count-1 do
              begin
                Cur := GetR(l[j]).Left;
                if Cur<Min then
                  begin
                    iMin := j;
                    Min := Cur;
                  end;
              end;
            if iMin<>i then
              l.Exchange(i,iMin);
          end;
        dcLeft := TprExObjRecVersion(l[0]);
        Left := DsgnR(dcLeft).Left;

        // find max Right
        dcRight := TprExObjRecVersion(l[0]);
        Right := DsgnR(dcRight).Right;
        for i:=1 to l.Count-1 do
          begin
            if Right<GetR(l[i]).Right then
              begin
                dcRight := TprExObjRecVersion(l[i]);
                Right := DsgnR(dcRight).Right;
              end;
          end;

        Width := 0;
        for i:=1 to l.Count-1 do
          if GetR(l[i]).Right<Right then
            Width := Width+GetR(l[i]).Right-GetR(l[i]).Left;

        Delta := (Right-Left-(DsgnR(dcLeft).Right-DsgnR(dcLeft).Left)-(DsgnR(dcRight).Right-DsgnR(dcRight).Left)-Width) div (l.Count-1);

        // now - align
        Left := DsgnR(dcLeft).Right;
        for i:=1 to l.Count-1 do
          if l[i]<>dcRight then
            begin
              Box.DoDrag(TprExObjRecVersion(l[i]),
                         Left+Delta-GetR(l[i]).Left,
                         0,
                         @prps);
              Left := Left+Delta+GetR(l[i]).Right-GetR(l[i]).Left;
            end;
      finally
        l.Free;
      end;
    end;
  aacVSpaceEqually:
    begin
      l:=TList.Create;
      try
        for i:=0 to SelCount-1 do
          if DsgnAllowDragObject(SelObjs[i]) then
            l.Add(SelObjs[i]);

        for i:=0 to l.Count-1 do
          begin
            iMin := i;
            Min := GetR(l[i]).Top;
            for j:=i+1 to l.Count-1 do
              begin
                Cur := GetR(l[j]).Top;
                if Cur<Min then
                  begin
                    iMin := j;
                    Min := Cur;
                  end;
              end;
            if iMin<>i then
              l.Exchange(i,iMin);
          end;
        dcTop := TprExObjRecVersion(l[0]);
        Top := DsgnR(dcTop).Top;

        // find max Bottom
        dcBottom := TprExObjRecVersion(l[0]);
        Bottom := DsgnR(dcBottom).Bottom;
        for i:=1 to l.Count-1 do
          begin
            if Bottom<GetR(l[i]).Bottom then
              begin
                dcBottom := TprExObjRecVersion(l[i]);
                Bottom := DsgnR(dcBottom).Bottom;
              end;
          end;
      
        Height := 0;
        for i:=1 to l.Count-1 do
          if GetR(l[i]).Bottom<Bottom then
            Height := Height+GetR(l[i]).Bottom-GetR(l[i]).Top;
      
        Delta := (Bottom-Top-(DsgnR(dcTop).Bottom-DsgnR(dcTop).Top)-(DsgnR(dcBottom).Bottom-DsgnR(dcBottom).Top)-Height) div (l.Count-1);
      
        // now - align
        Top := DsgnR(dcTop).Bottom;
        for i:=1 to l.Count-1 do
          if l[i]<>dcBottom then
            begin
              Box.DoDrag(TprExObjRecVersion(l[i]),
                         0,
                         Top+Delta-GetR(l[i]).Top,
                         @prps);
              Top := Top+Delta+GetR(l[i]).Bottom-GetR(l[i]).Top;
            end;
      finally
        l.Free;
      end;
    end;
end;
Box._EndPaint(prps);
end;

function TprPreviewPanel.AllowFind : boolean;
begin
Result := not ReportEmpty;
end;

function TprPreviewPanel.AllowDelete : boolean;
begin
Result := DsgnCanUserEdit and (SelCount>0);
end;

function TprPreviewPanel.AllowCopy : boolean;
begin
Result := DsgnCanUserEdit and (SelCount>0);
end;

function TprPreviewPanel.AllowPaste : boolean;
begin
Result := DsgnCanUserEdit and Clipboard.HasFormat(CF_PROBJVERSIONS);
end;

function TprPreviewPanel.AllowCut : boolean;
begin
Result := DsgnCanUserEdit and (SelCount>0);
end;

function TprPreviewPanel.AllowAlignToGridAll : boolean;
var
  i : integer;
begin
i:=0;
while (i<SelCount) and (not DsgnAllowDragObject(SelObjs[i])) do Inc(i);
if i>=SelCount then
  begin
    i := 0;
    while (i<SelCount) and (not DsgnAllowResizeObject(SelObjs[i])) do Inc(i);
    Result := i<SelCount;
  end
else
  Result := true;
end;

function TprPreviewPanel.GetNumDragSelectedRegions : integer;
var
  i : integer;
begin
Result := 0;
for i:=0 to SelCount-1 do
  if DsgnAllowDragObject(SelObjs[i]) then
    Inc(Result);
end;

function TprPreviewPanel.GetNumResizeSelectedRegions : integer;
var
  i : integer;
begin
Result := 0;
for i:=0 to FSelObjs.Count-1 do
  if DsgnAllowResizeObject(SelObjs[i]) then
    Inc(Result);
end;

function TprPreviewPanel.AllowBringToFront : boolean;
begin
Result := DsgnCanUserEdit and (SelCount>0);
end;

function TprPreviewPanel.AllowSendToBack : boolean;
begin
Result := DsgnCanUserEdit and (SelCount>0);
end;

function TprPreviewPanel.AllowInsertPage : boolean;
begin
Result := DsgnCanUserEdit;
end;

function TprPreviewPanel.AllowDeletePage : boolean;
begin
Result := DsgnCanUserEdit and (ActivePage <> nil);
end;

function TprPreviewPanel.AllowEditPage : boolean;
begin
Result := DsgnCanUserEdit and (ActivePage <> nil);
end;

function TprPreviewPanel.AlignActionAllowed(ActionCode : TprAlignActionCode) : boolean;
begin
Result := false;
if not DsgnCanUserEdit then exit;
case ActionCode of
  aacHCenters,
  aacVCenters,
  aacVToBottom,
  aacVToTop,
  aacHToRight,
  aacHToLeft : Result := GetNumDragSelectedRegions>=2;
  aacVCenterInWindow,
  aacHCenterInWindow : Result := GetNumDragSelectedRegions>=1;
  aacWToSmall,
  aacWToLarge : Result := GetNumResizeSelectedRegions>=2;
  aacHToSmall,
  aacHToLarge : Result := GetNumResizeSelectedRegions>=2;
  aacAlignToGridLeftTop : Result := GetNumDragSelectedRegions>=1;
  aacAlignToGridAll : Result := AllowAlignToGridAll;
  aacHSpaceEqually,
  aacVSpaceEqually : Result := GetNumDragSelectedRegions>=3;
  else Result := false;
end;
end;

procedure TprPreviewPanel.Nudge(dx,dy : integer);
var
  i : integer;
  prps : rPrPaintStruct;
begin
Box._BeginPaint(prps);
for i:=0 to SelCount-1 do
  Box.DoDrag(SelObjs[i],dx,dy,@prps);
Box._EndPaint(prps);
end;

procedure TprPreviewPanel.Size(sx,sy : integer);
var
  i : integer;
  prps : rPrPaintStruct;
begin
Box._BeginPaint(prps);
for i:=0 to SelCount-1 do
  Box.DoResize(SelObjs[i],0,0,sy,sx,@prps);
Box._EndPaint(prps);
end;

procedure TprPreviewPanel.SetPosSizeProp(Prop : TprObjectPosSizeProps; Value : integer);
var
  v : TprExObjRecVersion;
  ep : TprEndPage;
  pdi : pprDrawInfo;
  i,pv : integer;
  prps : rprPaintStruct;
begin
Box._BeginPaint(prps);
for i:=0 to SelCount-1 do
  begin
    v := SelObjs[i];
    DsgnObjPage(v,ep,pdi);
    if Prop in [prpsaLeft,prpsaRight,prpsaWidth] then pv := Round(Value * pdi.kx)
                                                 else pv := Round(Value * pdi.ky);
    if (Prop in [prpsaLeft,prpsaRight,prpsaTop,prpsaBottom]) and DsgnAllowDragObject(v) then
      begin
        // nudge
        case Prop of
          prpsaLeft : Box.DoDrag(v,pv-GetRealRect(v,pdi).Left,0,@prps);
          prpsaTop : Box.DoDrag(v,0,pv-GetRealRect(v,pdi).Top,@prps);
          prpsaRight : Box.DoDrag(v,pv-GetRealRect(v,pdi).Right,0,@prps);
          prpsaBottom : Box.DoDrag(v,0,pv-GetRealRect(v,pdi).Bottom,@prps);
        end;
      end
    else
      begin
        if (Prop in [prpsaWidth,prpsaHeight]) then
          begin
            // resize
            case Prop of
              prpsaWidth : Box.DoResize(v,0,0,0,Value-GetRealRect(v,pdi).Right+GetRealRect(v,pdi).Left,@prps);
              prpsaHeight : Box.DoResize(v,0,0,Value-GetRealRect(v,pdi).Bottom+GetRealRect(v,pdi).Top,0,@prps);
            end;
          end;
      end;
  end;
Box._EndPaint(prps);
end;

procedure TprPreviewPanel.BringToFront;
var
  i : integer;
  v : TprExObjRecVersion;
  ep : TprEndPage;
  pdi : pprDrawInfo;
  prps : rPrPaintStruct;
begin
Box._BeginPaint(prps);
for i:=0 to SelCount-1 do
  begin
    v := SelObjs[i];
    DsgnObjPage(v,ep,pdi);
    ep.VL.Remove(v);
    ep.VL.Add(v);
    AddRectToRegion(prps.ClipRgn,DsgnR(v));
  end;
Box._EndPaint(prps);
end;

procedure TprPreviewPanel.SendToBack;
var
  i : integer;
  v : TprExObjRecVersion;
  ep : TprEndPage;
  pdi : pprDrawInfo;
  prps : rPrPaintStruct;
begin
Box._BeginPaint(prps);
for i:=0 to SelCount-1 do
  begin
    v := SelObjs[i];
    DsgnObjPage(v,ep,pdi);
    ep.VL.Remove(v);
    ep.VL.Insert(0,v);
    AddRectToRegion(prps.ClipRgn,DsgnR(v));
  end;
Box._EndPaint(prps);
end;

function TprPreviewPanel.GetFindText : string;
begin
Result := Frpdi.FindText;
end;

procedure TprPreviewPanel.SetFindText(Value : string);
begin
Frpdi.FindText := Value;
end;

function TprPreviewPanel.GetFindCount : integer;
begin
Result := Frpdi.FindList.Count;
end;

function TprPreviewPanel.GetFindCaseSensitive : boolean;
begin
Result := Frpdi.CaseSensitive;
end;

procedure TprPreviewPanel.SetFindCaseSensitive(Value : boolean);
begin
Frpdi.CaseSensitive := Value;
end;

function TprPreviewPanel.GetIsFindMode : boolean;
begin
Result := Frpdi.DrawMode=dmFind;
end;

function TprPreviewPanel.GetProgressBar : TProgressBar;
begin
Result := Frpdi.ProgressBar;
end;

procedure TprPreviewPanel.SetProgressBar(Value : TProgressBar);
begin
Frpdi.ProgressBar := Value;
end;

function TprPreviewPanel.GetStatusBar : TStatusBar;
begin
Result := Frpdi.StatusBar;
end;

procedure TprPreviewPanel.SetStatusBar(Value : TStatusBar);
begin
Frpdi.StatusBar := Value;
end;

procedure TprPreviewPanel.DeleteSelectedObjects;
var
  i : integer;
  v : TprExObjRecVersion;
  ep : TprEndPage;
  pdi : pprDrawInfo;
  ClipRgn : HRGN;
begin
  Box.HideDrawing(Box.Canvas.Handle);
  i := 0;
  ClipRgn := 0;
  while i<SelCount do
  begin
    v := SelObjs[i];
    DsgnObjPage(v,ep,pdi);
    AddRectToRegion(ClipRgn,DsgnR(v));
    ep.VL.Remove(v);
    v.Free;
    Inc(i);
  end;
  InternalClearSelected;

  Box.InternalPaint(Box.Canvas.Handle,ClipRgn);

  if ClipRgn <> 0 then
    DeleteObject(ClipRgn);

  PrvNotifyReport;
  DoSelectionChanged;
end;

procedure TprPreviewPanel.SelectedObjectsToClipboard;
var
  ms : TMemoryStream;
  hMem : THandle;
  pMem : pointer;
begin
// write objects to memory stream
ms := TMemoryStream.Create;
try
  SaveObjectsVersionsToStream(ms,SelObjsList);
  // Copy memory stream to memory
  ClipBoard.Open;
  try
    hMem := GlobalAlloc(GMEM_MOVEABLE+GMEM_SHARE+GMEM_ZEROINIT,ms.Size);
    if hMem<>0 then
      begin
        pMem := GlobalLock(hMem);
        if pMem<>nil then
          begin
            CopyMemory(pMem,ms.Memory,ms.Size);
            GlobalUnLock(hMem);
            ClipBoard.SetAsHandle(CF_PROBJVERSIONS,hMem);
          end;
      end;
  finally
    ClipBoard.Close;
  end;
finally
  ms.Free;
end;
end;

procedure TprPreviewPanel.Paste;
var
  i : integer;
  l : TList;
  v : TprExObjRecVersion;
  ms : TMemoryStream;
  hMem : THandle;
  pMem : pointer;
  prps : rPrPaintStruct;
  hSize : DWORD;
begin
  if ActivePage=nil then
  begin
    MBMessage(prLoadStr(sPreviewSelectPageForPaste),prLoadStr(sAttention));
    exit;
  end;

  ClipBoard.Open;
  try
    hMem := Clipboard.GetAsHandle(CF_PROBJVERSIONS);
    pMem := GlobalLock(hMem);
    if pMem<>nil then
    begin
      hSize := GlobalSize(hMem);
      ms := TMemoryStream.Create;
      try
        ms.Write(pMem^,hSize);
        ms.Seek(soFromBeginning,0);
        l := TList.Create;
        try
          LoadObjectsVersionsFromStream(ms,l);
          Box._BeginPaint(prps);
          InternalClearSelected;
          for i:=0 to l.Count-1 do
            begin
              v := TprExObjRecVersion(l[i]);
              OffsetRect(v.GeneratedRect,GridSize,GridSize);
              if v.GeneratedRect.Right>ActivePage.PixelPageRect.Right then
                v.GeneratedRect.Left := v.GeneratedRect.Left-v.GeneratedRect.Right+ActivePage.PixelPageRect.Right;
              if v.GeneratedRect.Bottom>ActivePage.PixelPageRect.Bottom then
                v.GeneratedRect.Top := v.GeneratedRect.Top-v.GeneratedRect.Bottom+ActivePage.PixelPageRect.Bottom;
              ActivePage.VL.Add(v);
              InternalSelectObj(v);
              AddRectToRegion(prps.ClipRgn,DsgnR(v));
            end;
          Box._EndPaint(prps);
          PrvNotifyReport;
        finally
          l.Free;
        end;
      finally
        GlobalUnlock(hMem);
        ms.Free;
      end;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure TprPreviewPanel.Cut;
begin
if not AllowCut then exit;
SelectedObjectsToClipboard;
DeleteSelectedObjects;
end;

procedure TprPreviewPanel.Copy;
begin
if not AllowCopy then exit;
SelectedObjectsToClipBoard;
end;

function TprPreviewPanel.InternalFind : boolean;
var
  ep : TprEndPage;
  pdi : pprDrawInfo;
  DC,TempDC : HDC;
  i,j,k,n,Top,TopIndex : integer;
begin
Result := false;
DC := 0;

ClearFindList;
Frpdi.DrawMode := dmFindFirst;

if Frpdi.ProgressBar<>nil then
  begin
    Frpdi.ProgressBar.Min := 0;
    Frpdi.ProgressBar.Position := 0;
    Frpdi.ProgressBar.Max := Report.EndPagesCount;
  end;
if Frpdi.StatusBar<>nil then
  begin
    Frpdi.StatusBar.Panels[1].Text:=prLoadStr(sFind);
    Frpdi.StatusBar.Repaint;
  end;
try
  // create temp DC
  TempDC := GetDC(0);
  DC := CreateCompatibleDC(TempDC);
  ReleaseDC(0,TempDC);

  for i:=0 to Report.EndPagesCount-1 do
    begin
      if Frpdi.ProgressBar<>nil then
        begin
          Frpdi.ProgressBar.Position := Frpdi.ProgressBar.Position+1;
          Application.ProcessMessages;
        end;

      // draw page
      n := Frpdi.FindList.Count;
      ep := TprEndPage(Report.EndPages[i]);
      pdi := pprDrawInfo(FCachedPages[i]);
      for j:=0 to ep.vl.Count-1 do
        TprExObjRecVersion(ep.vl[j]).Draw(DC, pdi);

      j := n;
      while j<Frpdi.FindList.Count do
        begin
          Top := TprFindText(Frpdi.FindList[j]).TextRect.Top;
          k := j+1;
          while k<Frpdi.FindList.Count do
            if Top=TprFindText(Frpdi.FindList[k]).TextRect.Top then
              begin
                TprFindText(Frpdi.FindList[k]).Free;
                Frpdi.FindList.Delete(k)
              end
            else
              Inc(k);
          with TprFindText(Frpdi.FindList[j]).TextRect do
            begin
              Left := Left+pdi.bRect.Left;
              Top := Top+pdi.bRect.Top;
              Right := Right+pdi.bRect.Left;
              Bottom := Bottom+pdi.bRect.Top;
            end;
          Inc(j);
        end;

      for j:=n to Frpdi.FindList.Count-1 do
        begin
          Top := TprFindText(Frpdi.FindList[j]).TextRect.Top;
          TopIndex := j;
          for k:=j+1 to Frpdi.FindList.Count-1 do
            if TprFindText(Frpdi.FindList[k]).TextRect.Top<Top then
              begin
                Top := TprFindText(Frpdi.FindList[k]).TextRect.Top;
                TopIndex := k;
              end;
          if j<>TopIndex then
            Frpdi.FindList.Exchange(j,TopIndex);
        end;
    end;
  Result := Frpdi.FindList.Count>0;
finally
  if Frpdi.StatusBar<>nil then
    Frpdi.StatusBar.Panels[1].Text := '';
  if Frpdi.ProgressBar<>nil then
    Frpdi.ProgressBar.Position := 0;
  if DC<>0 then
    DeleteDC(DC);
  if Result then
    Frpdi.DrawMode := dmFind
  else
    Frpdi.DrawMode := dmDraw;
end;
end;

procedure TprPreviewPanel.UpdatePreviewBox;
var
  p : pprDrawInfo;
  ep : TprEndPage;
  i,j,mmw,mmh,n,l,t,w,h : integer;
  OldScaleKoef,NewScaleKoef : double;

  procedure CachePage(ep : TprEndPage; i : integer);
  begin
  with pprDrawInfo(FCachedPages[i])^ do
    begin
      kx := w / ep.PixelPageWidth;
      ky := h / ep.PixelPageHeight;
      bRect := Rect(l,t,l+w,t+h);
    end;
  end;

begin
if ReportEmpty then j := 0
               else j := Report.EndPagesCount;
// update FCachedPages
if j<FCachedPages.Count then
  begin
    for i:=j to FCachedPages.Count-1 do
      FreeMem(FCachedPages[i]);
    FCachedPages.Count := j;
  end
else
  if j>FCachedPages.Count then
    begin
      n := FCachedPages.Count;
      FCachedPages.Count := j;
      for i:=n to j-1 do
        begin
          GetMem(p,sizeof(rprDrawInfo));
          p.ppdi := @Frpdi;
          p.IsPrinter := false;
          FCachedPages[i] := p;
        end;
    end;

if ReportEmpty then
  begin
    Box.HorzScrollBar.Range := Box.ClientWidth;
    Box.VertScrollBar.Range := Box.ClientHeight;
    Box.Repaint;
    exit;
  end;
if FScaleDiv=0 then
  OldScaleKoef := 0
else
  OldScaleKoef := FScaleMul/FScaleDiv;
case ScaleMode of
  smPages:
    begin
      mmw := FMaxWidth*PagesPerWidth+(PagesPerWidth-1)*PagesStepX;
      mmh := FMaxHeight*PagesPerHeight+(PagesPerHeight-1)*PagesStepY;
      if (Box.ClientWidth-LeftOffs-RightOffs)/mmw<(Box.ClientHeight-TopOffs-BottomOffs)/mmh then
        begin
          FScaleMul := Box.ClientWidth-LeftOffs-RightOffs;
          FScaleDiv := mmw;
        end
      else
        begin
          FScaleMul := Box.ClientHeight-TopOffs-BottomOffs;
          FScaleDiv := mmh;
        end;
      if Round(FScaleMul/FScaleDiv*100)<=0 then
        begin
          FScaleMul := 1;
          FScaleDiv := 100;
        end;

      n := Report.EndPagesCount div PagesPerWidth;
      n := n+(Report.EndPagesCount mod PagesPerWidth);

      FDrawWidth := LeftOffs+RightOffs+muldiv(FMaxWidth,FScaleMul,FScaleDiv)*PagesPerWidth+(PagesPerWidth-1)*PagesStepX;
      FDrawHeight := TopOffs+BottomOffs+muldiv(FMaxHeight,FScaleMul,FScaleDiv)*n+(n-1)*PagesStepY;
    end;

  smPageWidth:
    begin
      FScaleMul := Box.ClientWidth-LeftOffs-RightOffs;
      FScaleDiv := FMaxWidth;
      FDrawWidth := Box.ClientWidth;
      FDrawHeight := TopOffs+BottomOffs+muldiv(FMaxHeight,FScaleMul,FScaleDiv)*(Report.EndPagesCount)+PagesStepY*(Report.EndPagesCount-1);
    end;

  smOnePagePercent:
    begin
      FDrawWidth := LeftOffs+RightOffs+muldiv(FMaxWidth,FScaleMul,FScaleDiv);
      FDrawHeight := TopOffs+BottomOffs+muldiv(FMaxHeight,FScaleMul,FScaleDiv)*(Report.EndPagesCount)+PagesStepY*(Report.EndPagesCount-1);
    end;
end;

// maximum sizes of ScrollBar
Box.HorzScrollBar.Range := FDrawWidth;
Box.VertScrollBar.Range := FDrawHeight;

case ScaleMode of
  smPages:
    begin
      i := 0;
      while i*PagesPerWidth<Report.EndPagesCount do
        begin
          j := 0;
          while (j<PagesPerWidth) and (i*PagesPerWidth+j<Report.EndPagesCount) do
            begin
              ep := TprEndPage(Report.EndPages[i*PagesPerWidth+j]);
              w := muldiv(ep.PixelPageWidth,FScaleMul,FScaleDiv);
              h := muldiv(ep.PixelPageHeight,FScaleMul,FScaleDiv);
              l := LeftOffs+(muldiv(FMaxWidth,FScaleMul,FScaleDiv)+PagesStepX)*j;
              t := TopOffs+(muldiv(FMaxHeight,FScaleMul,FScaleDiv)+PagesStepY)*i;
              CachePage(ep,i*PagesPerWidth+j);
              Inc(j);
            end;
          Inc(i);
        end;
    end;
  smPageWidth,smOnePagePercent:
    begin
      for i:=0 to Report.EndPagesCount-1 do
        begin
          ep := TprEndPage(Report.EndPages[i]);
          w := muldiv(ep.PixelPageWidth,FScaleMul,FScaleDiv);
          h := muldiv(ep.PixelPageHeight,FScaleMul,FScaleDiv);
          l := LeftOffs;
          t := TopOffs+(muldiv(FMaxHeight,FScaleMul,FScaleDiv)+PagesStepY)*i;
          CachePage(ep,i);
        end;
    end;
end;

if Frpdi.DrawMode=dmFind then
  InternalFind;

Box.Repaint;

if FScaleDiv=0 then
  NewScaleKoef := 0
else
  NewScaleKoef := FScaleMul/FScaleDiv;
if Abs(OldScaleKoef-NewScaleKoef)>0.001 then
  DoScalePercentChanged;
end;

procedure TprPreviewPanel.PagesChanged;
begin
Frpdi.DrawMode := dmDraw;
ClearFindList;
CalcMaxSizes;
Box.FLastHighlightedObject := nil;
Box.FLastHighlightedEndPage := nil;
UpdatePreviewBox;
end;

function TprPreviewPanel.InsertPageAfter(AfterPageIndex : integer) : TprEndPage;
var
  cp: TprEndPage;
begin
  Result := TprEndPage.CreateEmpty(Report);
  cp := FCurPage;
  if cp = nil then
  begin
    if Report.EndPagesCount>0 then
      cp := TprEndPage(Report.EndPages[0]);
  end;
  if cp <> nil then
    Result.PageInfo.Assign(cp.PageInfo);

  Report.InsertEndPageIntoList(Result,AfterPageIndex+1);
  PagesChanged;
  PrvNotifyReport;
  DoPageInserted(Result);
end;

procedure TprPreviewPanel.DeletePage(PageIndex : integer);
var
  ep: TprEndPage;
begin
  ep := TprEndPage(Report.EndPages[PageIndex]);
  if ep = FCurPage then
    FCurPage := nil;
  Report.DeleteEndPage(PageIndex);
  PagesChanged;
  PrvNotifyReport;
  DoPageDeleted(ep);
end;

function TprPreviewPanel.EditPage(PageIndex : integer) : boolean;
var
  dummy : integer;
  dummy2 : boolean;
begin
  Result := false;
  if (PageIndex<0) or (PageIndex>=Report.EndPagesCount) then exit;
  Result := TprPageParamsForm.Create(Application).EditOptions(TprReport(Report),TprEndPage(Report.EndPages[PageIndex]).PageInfo,TprEndPage(Report.EndPages[PageIndex]).PageScaleInfo,false,dummy,dummy,false,dummy2);
  if Result then
  begin
    PagesChanged;
    PrvNotifyReport;
    DoPageParamsChanged(TprEndPage(Report.EndPages[PageIndex]));
  end;
end;

procedure TprPreviewPanel.InsertPageAfterCurrent;
var
  ep : TprEndPage;
begin
if ActivePage=nil then
  ep := InsertPageAfter(Report.EndPagesCount-1)
else
  ep := InsertPageAfter(ActivePageIndex);
with DsgnPagePDI(ep)^ do
  begin
    Box.VertScrollBar.Position := bRect.Top;
    Box.HorzScrollBar.Position := bRect.Left;
  end;
end;

procedure TprPreviewPanel.DeleteCurrentPage;
begin
if ActivePage<>nil then
  DeletePage(ActivePageIndex);
end;

procedure TprPreviewPanel.EditCurrentPage;
begin
if ActivePage<>nil then
  EditPage(ActivePageIndex);
end;

function TprPreviewPanel.GetScalePercent : integer;
begin
if FScaleDiv=0 then
  Result := 0
else
  Result := Round(FScaleMul/FScaleDiv*100);
end;

procedure TprPreviewPanel.SetPagesPerHeight(Value : integer);
begin
if (FPagesPerHeight=Value) or (Value<1) then exit;
FPagesPerHeight := Value;
FScaleMode := smPages;
UpdatePreviewBox;
end;

procedure TprPreviewPanel.SetPagesPerWidth(Value : integer);
begin
if (FPagesPerWidth=Value) or (Value<1) then exit;
FPagesPerWidth := Value;
FScaleMode := smPages;
UpdatePreviewBox;
end;

procedure TprPreviewPanel.SetScaleMode(Value : TprScaleMode);
begin
  if FScaleMode <> Value then
  begin
    FScaleMode := Value;
    UpdatePreviewBox;
  end;
end;

procedure TprPreviewPanel.SetScalePercent(Value : integer);
begin
  if (GetScalePercent <> Value) and (Value > 1) then
  begin
    FScaleMul := Value;
    FScaleDiv := 100;
    FScaleMode := smOnePagePercent;
    UpdatePreviewBox;
    DoScalePercentChanged;
  end;
end;

function TprPreviewPanel.GetOpacityObjectsPropsForm : integer;
begin
  if FObjectsPropsForm = nil then
    Result := FOpacityObjectsPropsForm
  else
    Result := FObjectsPropsForm.Opacity;
end;

procedure TprPreviewPanel.SetOpacityObjectsPropsForm(Value : integer);
begin
  FOpacityObjectsPropsForm := Value;
  if FObjectsPropsForm <> nil then
    FObjectsPropsForm.Opacity := Value;
end;

procedure TprPreviewPanel.SetShowGrid(Value : boolean);
begin
  if FShowGrid = Value then exit;
  FShowGrid := Value;
  Box.Repaint;
end;

procedure TprPreviewPanel.SetGridSize(Value : integer);
begin
  if FGridSize = Value then exit;
  FGridSize := Value;
  UpdateGridBitmap(Box.FGridBitmap, FGridSize,FGridSize);
  if ShowGrid then
    Box.Repaint;
end;

procedure TprPreviewPanel.SetUseGrid(Value : boolean);
begin
  FUseGrid := Value;
end;

procedure TprPreviewPanel.SetCurPage(Value : TprEndPage);
begin
  if FCurPage = Value then exit;
  if FCurPage <> nil then
    Box.DrawPageRect(Box.Canvas.Handle,FCurPage,false);
  FCurPage := Value;
  if FCurPage <> nil then
    Box.DrawPageRect(Box.Canvas.Handle,FCurPage,true);
end;

procedure TprPreviewPanel.SetCurShowPage(Value: TprEndPage);
var
  pdi: pprDrawInfo;
  AChanged: Boolean;
begin
  AChanged := FCurShowPage <> Value;
  FCurShowPage := Value;
  if Value = nil then
  begin
    Box.VertScrollBar.Position := 0;
    Box.HorzScrollBar.Position := 0;
  end
  else
  begin
    pdi := pprDrawInfo(FCachedPages[Report.EndPageIndex(FCurShowPage)]);
    Box.VertScrollBar.Position := pdi.bRect.Top;
    Box.HorzScrollBar.Position := pdi.bRect.Left;
  end;
  if AChanged then
    DoShowPageChanged;
end;

function TprPreviewPanel.GetPageIndex: Integer;
begin
  Result := Report.EndPageIndex(FCurShowPage);
end;

procedure TprPreviewPanel.SetPageIndex(Value: Integer);
begin
  if (Value < 0) then
    Value := 0;
  if Value >= Report.EndPagesCount then
    Value := Report.EndPagesCount - 1;
  SetCurShowPage(TprEndPage(Report.EndPages[Value]));
end;

function TprPreviewPanel.GetPageCount: Integer;
begin
  if Report = nil then
    Result := -1
  else
    Result := Report.EndPagesCount;
end;

function TprPreviewPanel.GetActivePageIndex : integer;
begin
Result := Report.EndPageIndex(ActivePage);
end;

procedure TprPreviewPanel.OnFindDialogFind(Sender : TObject);
begin
if IsFindMode then
  begin
    if frDown in FFindDialog.Options then
      FindNext
    else
      FindPrior;
  end
else
  begin
    FindCaseSensitive := frMatchCase in FFindDialog.Options;
    if FindCaseSensitive then
      FindText := FFindDialog.FindText
    else
      FindText := AnsiUpperCase(FFindDialog.FindText);
    if InternalFind then
      begin
        FFindPos := -1;
        FindNext;
        Box.Repaint;
      end
    else
      MBError(Format(prLoadStr(sTextNotFound),[Frpdi.FindText]));
  end;
end;

procedure TprPreviewPanel.Find;
begin
if ReportEmpty then exit;
FFindDialog.Execute;
end;

procedure TprPreviewPanel.CancelFind;
begin
Frpdi.DrawMode := dmDraw;
ClearFindList;
Box.Repaint;
end;

procedure TprPreviewPanel.FindNext;
begin
if not IsFindMode then exit;
if FFindPos<Frpdi.FindList.Count-1 then
  begin
    Inc(FFindPos)
  end
else
  begin
    if MBox(prLoadStr(sEndOfFind),prLoadStr(sAttention),MB_YESNO+MB_ICONQUESTION)=IDNO then
      exit;
    FFindPos := 0;
  end;
Box.HorzScrollBar.Position := TprFindText(Frpdi.FindList[FFindPos]).TextRect.Left;
Box.VertScrollBar.Position := TprFindText(Frpdi.FindList[FFindPos]).TextRect.Top;
end;

procedure TprPreviewPanel.FindPrior;
begin
if not IsFindMode then exit;
if FFindPos>0 then
  begin
    Dec(FFindPos);
  end
else
  begin
    if MBox(prLoadStr(sStartOfFind),prLoadStr(sAttention),MB_YESNO+MB_ICONQUESTION)=IDNO then
      exit;
    FFindPos := Frpdi.FindList.Count-1;
  end;
Box.HorzScrollBar.Position := TprFindText(Frpdi.FindList[FFindPos]).TextRect.Left;
Box.VertScrollBar.Position := TprFindText(Frpdi.FindList[FFindPos]).TextRect.Top;
end;

procedure TprPreviewPanel.ShowWholePage;
begin
FPagesPerHeight := 1;
FPagesPerWidth := 1;
FScaleMode :=smPages;
UpdatePreviewBox;
end;

procedure TprPreviewPanel.ShowPageWidth;
begin
FScaleMode := smPageWidth;
UpdatePreviewBox;
end;

procedure TprPreviewPanel.ShowManyPages;
begin
FPagesPerHeight := PerHeight;
FPagesPerWidth := PerWidth;
FScaleMode := smPages;
UpdatePreviewBox;
end;

procedure TprPreviewPanel.LoadPreparedReport;
begin
Report.LoadPreparedReportFromFile(FileName);
CalcMaxSizes;
Box.FLastHighlightedObject := nil;
Box.FLastHighlightedEndPage := nil;
Box.VertScrollBar.Position := 0;
Box.HorzScrollBar.Position := 0;
UpdatePreviewBox;
end;

procedure TprPreviewPanel.UpdateObjectsPropsForm;
begin
if FObjectsPropsForm<>nil then
  FObjectsPropsForm.UpdateInfo;
end;

procedure TprPreviewPanel.DoSelectionChanged;
begin
UpdateObjectsPropsForm;
if Assigned(FOnSelectionChanged) then
  FOnSelectionChanged(Self,SelCount);
end;

procedure TprPreviewPanel.DoObjectResized(ResizeObject : TprExObjRecVersion);
begin
UpdateObjectsPropsForm;
if Assigned(FOnObjectResized) then
  FOnObjectResized(Self,ResizeObject);
end;

procedure TprPreviewPanel.DoObjectDrag(DragObject : TprExObjRecVersion);
begin
UpdateObjectsPropsForm;
if Assigned(FOnObjectDrag) then
  FOnObjectDrag(Self,DragObject);
end;

procedure TprPreviewPanel.DoApplyObjectsProps;
begin
UpdateSelectedObjects;
if Assigned(FOnApplyObjectsProps) then
  FOnApplyObjectsProps(Self);
end;

procedure TprPreviewPanel.DoWhileObjectDrag(const DragRect : TRect);
begin
if Assigned(FOnWhileObjectDrag) then
  FOnWhileObjectDrag(Self,DragRect);
end;

procedure TprPreviewPanel.DoWhileObjectResize(const ResizeRect : TRect);
begin
if Assigned(FOnWhileObjectResize) then
  FOnWhileObjectResize(Self,ResizeRect);
end;

procedure TprPreviewPanel.DoPreviewMouseDown(PreviewUserData : TprPreviewUserData; X,Y : integer; Shift : TShiftState);
begin
if Assigned(FOnMouseDown) then
  FOnMouseDown(Self,PreviewUserData,X,Y,Shift);
end;

procedure TprPreviewPanel.DoPreviewDblClick(PreviewUserData : TprPreviewUserData; X,Y : integer);
begin
if Assigned(FOnDblClick) then
  FOnDblClick(Self,PreviewUserData,X,Y);
end;

procedure TprPreviewPanel.DoPreviewMouseMove(PreviewUserData : TprPreviewUserData; X,Y : integer; var cur : TCursor; var HighlightObject : boolean);
begin
if Assigned(FOnMouseMove) then
  FOnMouseMove(Self,PreviewUserData,X,Y,cur,HighlightObject);
end;

procedure TprPreviewPanel.DoScalePercentChanged;
begin
if Assigned(FOnScalePercentChanged) then
  FOnScalePercentChanged(Self);
end;

procedure TprPreviewPanel.DoPageInserted(InsertedPage : TprEndPage);
begin
if Assigned(FOnPageInserted) then
  FOnPageInserted(Self,InsertedPage);
end;

procedure TprPreviewPanel.DoPageDeleted(DeletedPage : TprEndPage);
begin
if Assigned(FOnPageDeleted) then
  FOnPageDeleted(Self,DeletedPage);
end;

procedure TprPreviewPanel.DoPageParamsChanged(ChangedPage : TprEndPage);
begin
if Assigned(FOnPageParamsChanged) then
  FOnPageParamsChanged(Self,ChangedPage);
end;

procedure TprPreviewPanel.DoShowPageChanged;
begin
  if Assigned(FOnShowPageChanged) then
    FOnShowPageChanged(Self);
end;

function TprPreviewPanel.DsgnCanUserEdit : boolean;
begin
Result := (Report<>nil) and Report.CanUserEdit;
end;

procedure TprPreviewPanel.Resize;
begin
inherited;
UpdatePreviewBox;
end;

procedure TprPreviewPanel.WMSetFocus(var Msg : TWMSetFocus);
begin
inherited;
Box.SetFocus;
end;

procedure TprPreviewPanel.OnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  1: Cut;
  2: Copy;
  3: Paste;
  4: DeleteSelectedObjects;
  5: InsertPageAfterCurrent;
  6: DeleteCurrentPage;
  7: EditCurrentPage;
  8: VisibleObjectsPropsForm := true;
  9: Print;
  10: ExportTo;
  11: ShowManyPages(1,1);
  12: ScaleMode := smPageWidth;
  13: ShowManyPages(1,2);
  14: ShowManyPages(2,2);
  15: Find;
  16: FindNext;
  17: FindPrior;
  18: CancelFind;
  20: if (GetReport<>nil) and Assigned(GetReport.OnCustomActionInPreview) then
        GetReport.OnCustomActionInPreview(Self);
  21: Load;
  22: Save;
  23: if GetReport<>nil then
        GetReport.CanUserEdit := not GetReport.CanUserEdit;
  24: Find;
  25: FindNext;
  26: FindPrior;
  27: CancelFind;
  28: SendToBack;
  29: BringToFront;
  30: EditOptions;
end;
end;

procedure TprPreviewPanel.OnPopupMenuInsertObjectClick(Sender : TObject);
begin
with TMenuItem(Sender) do
  begin
    if Tag=1 then
      InsertObject(nil)
    else
      InsertObject(prObjRegInfos[Tag-2].ClassRef);
  end;
end;

procedure TprPreviewPanel.InitPopupMenu(Popup : TPopupMenu);
var
  MainMenuItem,m : TMenuItem;
begin
ClearPopupMenu(Popup);
if DsgnCanUserEdit then
  begin
    AddPopupMenuItem(Popup,nil,sClipboardCut,'CUT',OnPopupMenuClick,'Ctrl+X',1,AllowCut,false);
    AddPopupMenuItem(Popup,nil,sClipboardCopy,'COPY',OnPopupMenuClick,'Ctrl+C',2,AllowCopy,false);
    AddPopupMenuItem(Popup,nil,sClipboardPaste,'PASTE',OnPopupMenuClick,'Ctrl+V',3,AllowPaste,false);
    AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,nil,sDeleteSelected,'DELETE',OnPopupMenuClick,'Ctrl+Del',4,AllowDelete,false);
    AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,nil,sAddPage,'NEWPAGE',OnPopupMenuClick,'',5,AllowInsertPage,false);
    AddPopupMenuItem(Popup,nil,sDeletePage,'DELPAGE',OnPopupMenuClick,'',6,AllowDeletePage,false);
    AddPopupMenuItem(Popup,nil,sChangePageParams,'PAGEPARAMS',OnPopupMenuClick,'Ctrl+E',7,AllowEditPage,false);
    AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,nil,sProperties,'PROPERTIES',OnPopupMenuClick,'Alt+Enter',8,true,VisibleObjectsPropsForm);
    if PopupInsertObjectMenu then
      begin
        AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
        m := AddPopupMenuItem(Popup,nil,sInsertObject,'',nil,'',0,true,false);
        InitprObjPopupMenu(GetReport,Popup,m,OnPopupMenuInsertObjectClick,FCurClassRef);
      end;
  end
else
  begin
    AddPopupMenuItem(Popup,nil,sPrint,'PRINT',OnPopupMenuClick,'F9',9,not ReportEmpty,false);
    AddPopupMenuItem(Popup,nil,sExport,'EXPORTTOXLS',OnPopupMenuClick,'',10,not ReportEmpty,false);
    AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,nil,sWholePage,'WHOLEPAGE',OnPopupMenuClick,'',11,not ReportEmpty,false);
    AddPopupMenuItem(Popup,nil,sPageWidth,'PAGEWIDTH',OnPopupMenuClick,'',12,not ReportEmpty,false);
    AddPopupMenuItem(Popup,nil,sTwoPages,'TWOPAGES',OnPopupMenuClick,'',13,not ReportEmpty,false);
    AddPopupMenuItem(Popup,nil,sManyPages,'PAGES',OnPopupMenuClick,'',14,not ReportEmpty,false);
    AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,nil,sFindStart,'FIND',OnPopupMenuClick,'Ctrl+F',15,not ReportEmpty and not IsFindMode,false);
    AddPopupMenuItem(Popup,nil,sFindNext,'FINDNEXT',OnPopupMenuClick,'F3',16,IsFindMode,false);
    AddPopupMenuItem(Popup,nil,sFindPrior,'FINDPREV',OnPopupMenuClick,'Shift+F3',17,IsFindMode,false);
    AddPopupMenuItem(Popup,nil,sCancelFind,'FINDCANCEL',OnPopupMenuClick,'',18,IsFindMode,false);
  end;
if PopupMainMenu then
  begin                 
    AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
    MainMenuItem := AddPopupMenuItem(Popup,nil,sPopupMainMenu,'',nil,'',0,true,false);

    // File
    m := AddPopupMenuItem(Popup,MainMenuItem,sMainMenuFile,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,m,sPrint,'PRINT',OnPopupMenuClick,'F9',9,not ReportEmpty,false);
    if (GetReport<>nil) and (GetReport.CustomActionInPreviewCaption<>'') then
      AddPopupMenuItemS(Popup,m,GetReport.CustomActionInPreviewCaption,'CUSTOMACTION',OnPopupMenuClick,'Ctrl+P',20,true,false);
    AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,m,sLoadPreparedReport,'OPEN',OnPopupMenuClick,'Ctrl+O',21,GetReport<>nil,false);
    AddPopupMenuItem(Popup,m,sSavePreparedReport,'SAVE',OnPopupMenuClick,'Ctrl+S',22,not ReportEmpty,false);
    AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,m,sExport,'EXPORTTOXLS',OnPopupMenuClick,'',10,not ReportEmpty,false);
    AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,m,sPreviewModePreview,'PREVIEWEDIT',OnPopupMenuClick,'',23,not ReportEmpty,false);

    // Edit
    m := AddPopupMenuItem(Popup,MainMenuItem,sMainMenuEdit,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,m,sClipboardCut,'CUT',OnPopupMenuClick,'Ctrl+X',1,AllowCut,false);
    AddPopupMenuItem(Popup,m,sClipboardCopy,'COPY',OnPopupMenuClick,'Ctrl+C',2,AllowCopy,false);
    AddPopupMenuItem(Popup,m,sClipboardPaste,'PASTE',OnPopupMenuClick,'Ctrl+V',3,AllowPaste,false);
    AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,m,sDeleteSelected,'DELETE',OnPopupMenuClick,'Ctrl+Del',4,AllowDelete,false);
    AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,m,sFindStart,'FIND',OnPopupMenuClick,'Ctrl+F',24,AllowFind,false);
    AddPopupMenuItem(Popup,m,sFindNext,'FINDNEXT',OnPopupMenuClick,'F3',25,IsFindMode,false);
    AddPopupMenuItem(Popup,m,sFindPrior,'FINDPREV',OnPopupMenuClick,'Shift+F3',26,IsFindMode,false);
    AddPopupMenuItem(Popup,m,sCancelFind,'FINDCANCEL',OnPopupMenuClick,'',27,IsFindMode,false);
    AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,m,sMainMenuEditSendToBack,'SENDTOBACK',OnPopupMenuClick,'',28,AllowSendToBack,false);
    AddPopupMenuItem(Popup,m,sMainMenuEditBringToFront,'BRINGTOFRONT',OnPopupMenuClick,'',29,AllowBringToFront,false);
    AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,m,sProperties,'PROPERTIES',OnPopupMenuClick,'Alt+Enter',8,true,VisibleObjectsPropsForm);

    // View
    m := AddPopupMenuItem(Popup,MainMenuItem,sMainMenuView,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,m,sWholePage,'WHOLEPAGE',OnPopupMenuClick,'',11,not ReportEmpty,false);
    AddPopupMenuItem(Popup,m,sPageWidth,'PAGEWIDTH',OnPopupMenuClick,'',12,not ReportEmpty,false);
    AddPopupMenuItem(Popup,m,sTwoPages,'TWOPAGES',OnPopupMenuClick,'',13,not ReportEmpty,false);
    AddPopupMenuItem(Popup,m,sManyPages,'PAGES',OnPopupMenuClick,'',14,not ReportEmpty,false);
    AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,m,sMainMenuViewOptions,'',OnPopupMenuClick,'',30,true,false);

    // Page
    m := AddPopupMenuItem(Popup,MainMenuItem,sMainMenuPage,'',nil,'',0,true,false);
    AddPopupMenuItem(Popup,m,sAddPage,'NEWPAGE',OnPopupMenuClick,'',5,AllowInsertPage,false);
    AddPopupMenuItem(Popup,m,sDeletePage,'DELPAGE',OnPopupMenuClick,'',6,AllowDeletePage,false);
    AddPopupMenuItem(Popup,m,sChangePageParams,'PAGEPARAMS',OnPopupMenuClick,'Ctrl+E',7,AllowEditPage,false);
  end;
end;

procedure TprPreviewPanel.WriteToIni(IniFile : TIniFile; const SectionName : string);
var
  i : TprPreviewStuckOptions;
begin
inherited;
IniFile.WriteInteger(SectionName,'OpacityObjectsPropsForm',OpacityObjectsPropsForm);
IniFile.WriteInteger(SectionName,'GridSize',GridSize);
IniFile.WriteBool(SectionName,'GridAlign',UseGrid);
IniFile.WriteBool(SectionName,'GridShow',ShowGrid);
IniFile.WriteInteger(SectionName,'StuckMode',integer(StuckMode));
IniFile.WriteInteger(SectionName,'StuckOffs',StuckOffs);
for i:=Low(TprPreviewStuckOptions) to High(TprPreviewStuckOptions) do
  IniFile.WriteBool(SectionName,'StuckOptions'+GetEnumName(TypeInfo(TprPreviewStuckOptions),integer(i)),i in StuckOptions);
end;

procedure TprPreviewPanel.ReadFromIni(IniFile : TIniFile; const SectionName : string);
var
  i : TprPreviewStuckOptions;
  so : TprPreviewStuckOptionsSet;
begin
inherited;
OpacityObjectsPropsForm := IniFile.ReadInteger(SectionName,'OpacityObjectsPropsForm',OpacityObjectsPropsForm);
GridSize := IniFile.ReadInteger(SectionName,'GridSize',GridSize);
UseGrid := IniFile.ReadBool(SectionName,'GridAlign',UseGrid);
ShowGrid := IniFile.ReadBool(SectionName,'GridShow',ShowGrid);
StuckMode := TprStuckMode(IniFile.ReadInteger(SectionName,'StuckMode',integer(StuckMode)));
StuckOffs := IniFile.ReadInteger(SectionName,'StuckOffs',StuckOffs);
so := [];
for i:=Low(TprPreviewStuckOptions) to High(TprPreviewStuckOptions) do
  if IniFile.ReadBool(SectionName,'StuckOptions'+GetEnumName(TypeInfo(TprPreviewStuckOptions),integer(i)),i in StuckOptions) then
    Include(so,i);
StuckOptions := so;
end;

function TprPreviewPanel.EditOptions : boolean;
begin
Result := TprPreviewPanelOptionsForm.Create(Application).EditOptions(Self);
if Result then
  UpdatePreviewBox;
end;

procedure TprPreviewPanel.FullUpdate;
begin
  FCurPage := nil;
  Box.FLastHighlightedObject := nil;
  Box.FLastHighlightedEndPage := nil;
  Box.VertScrollBar.Position := 0;
  Box.HorzScrollBar.Position := 0;
  InternalClearSelected;
  CalcMaxSizes;
  ClearFindList;
  UpdatePreviewBox;
  if (Report <> nil) and (Report.EndPagesCount > 0) then
  begin
    FCurShowPage := TprEndPage(Report.EndPages[0]);
    DoShowPageChanged;
  end;
end;

end.

