{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the definition of the TprCustomDesignerPanel class and some auxiliary classes.}
unit pr_CommonDesignerPanel;

interface

{$I PR.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ActnList, Menus, ExtCtrls,
  TypInfo, ClipBrd, IniFiles, ComCtrls, math,
  StdCtrls, Buttons, vgr_GUIFunctions,

  pr_Utils, pr_Common, pr_DesignerFunctions, pr_VersionsEditor;

const
{Width in pixels of the button that appears in the top-left corner of object.
Syntax:
  LinkButtonWidth = 12;}
  LinkButtonWidth = 12;
{Height in pixels of the button that appears in the top-left corner of object.
Syntax:
  LinkButtonHeight = 12;}
  LinkButtonHeight = 12;

  WM_FREEINPLACEEDIT = WM_USER+1;

type
TprCustomDesignerPanel = class;

/////////////////////////////////////////////////
//
// TprUserObjectPropsForm
//
/////////////////////////////////////////////////
TprUserObjectPropsForm = class(TprForm)
private
  function GetReport : TprCustomReport;
  function GetDesignerPanel : TprCustomDesignerPanel;
public
  property Report : TprCustomReport read GetReport;
  property DesignerPanel : TprCustomDesignerPanel read GetDesignerPanel;

  procedure UpdateInfo; virtual; abstract;
  procedure UpdateDesigner; 
end;
TprUserObjectPropsFormClass = class of TprUserObjectPropsForm;

/////////////////////////////////////////////////
//
// TprPropsForm
//
/////////////////////////////////////////////////
TprPropsForm = class(TprForm)
private
  FDesignerPanel: TprCustomDesignerPanel;
  FOldOnClose: TCloseEvent;
protected
  procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
  procedure prSaveProperties(Ini : TIniFile; sn : string); override;
public
  property DesignerPanel: TprCustomDesignerPanel read FDesignerPanel;

  procedure CreateParams(var Params : TCreateParams); override;
  procedure Loaded; override;
  procedure OnCloseEvent(Sender: TObject; var Action : TCloseAction); virtual;

  procedure UpdateInfo; virtual;
  procedure Apply; virtual;
  procedure Cancel; virtual;
  procedure SetFocusOnFirstControl; virtual;

  constructor CreatePropsForm(AOwner : TComponent; _DesignerPanel : TprCustomDesignerPanel);
end;
TprPropsFormClass = class of TprPropsForm;

/////////////////////////////////////////////////
//
// TprObjPropsForm
//
/////////////////////////////////////////////////
TprObjPropsForm = class(TprPropsForm)
protected
  FVersionsEditor : TprObjVersionsEditor;

  procedure OnVersionChanged(Sender : TObject; OldVersion,NewVersion : TprObjRecVersion);
  procedure _CopyMultiplyPropertiesToControls;
  procedure _CopyMultiplyPropertiesFromControls;

  procedure SetEnabledAfterCopyToControls; virtual;
  procedure CopySinglePropertiesFromControls(V : TprObjRecVersion); virtual;
  procedure CopyMultiplyPropertiesFromControls(L : TList); virtual;
  procedure CopySinglePropertiesToControls(V : TprObjRecVersion); virtual;
  procedure CopyMultiplyPropertiesToControls(L : TList); virtual;

  procedure DefaultCopyToControls(L : TList);
  procedure DefaultCopyFromControls(L : TList);
public
  procedure UpdateInfo; override;
  procedure Apply; override;
  procedure Cancel; override;

  procedure AfterConstruction; override;
end;

/////////////////////////////////////////////////
//
// TprDesignerPanelForm
//
/////////////////////////////////////////////////
TprDesignerPanelForm = class(TprToolWindowForm)
private
  FDesignerPanel : TprCustomDesignerPanel;
protected
  function GetParentControl : TControl; override;
public
  property DesignerPanel : TprCustomDesignerPanel read FDesignerPanel write FDesignerPanel;
end;

/////////////////////////////////////////////////
//
// TprCustomObjLinksForm
//
/////////////////////////////////////////////////
TprCustomObjLinksForm = class(TprDesignerPanelForm)
end;

/////////////////////////////////////////////////
//
// TprCustomPosSizeForm
//
/////////////////////////////////////////////////
TprCustomPosSizeForm = class(TprDesignerPanelForm)
protected
  EnabledProps : TprObjectPosSizePropsSet;
  EqProps : TprObjectPosSizePropsSet;
  SizePosArray : array [TprObjectPosSizeProps] of integer;
public
  procedure UpdateInfo; override;
end;

/////////////////////////////////////////////////
//
// TprCustomTemplateFindForm
//
/////////////////////////////////////////////////
TprCustomTemplateFindForm = class(TprDesignerPanelForm)
end;

/////////////////////////////////////////////////
//
// TprCustomObjectsPropsForm
//
/////////////////////////////////////////////////
TprCustomObjectsPropsForm = class(TprDesignerPanelForm)
protected
  function GetFormEditorClassForClass(const ClassName : string) : TprPropsFormClass;
  procedure DoApplyObjectsProps;
end;

/////////////////////////////////////////////////
//
// TprObjectLinksButton
//
/////////////////////////////////////////////////
TprObjectLinksButton = class(TButtonControl)
private
  FHighlighted : boolean;
  procedure CNMeasureItem(var Msg: TWMMeasureItem); message CN_MEASUREITEM;
  procedure CNDrawItem(var Msg: TWMDrawItem); message CN_DRAWITEM;
  procedure DrawButton(DC : HDC; Highlighted : boolean);
  procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
  procedure CMMouseEnter(var Msg : TMessage); message CM_MOUSEENTER;
protected
  procedure CreateParams(var Params: TCreateParams); override;
public
  constructor Create(AOwner : TComponent); override;
end;

TprBandsCaptionsPointInfo = (bcpiNone,bcpiInside,bcpiResizeTop,bcpiResizeBottom,bcpiResizeLeft,bcpiResizeRight,bcpiLinkButton);
/////////////////////////////////////////////////
//
// TprBandCaptions
//
/////////////////////////////////////////////////
TprBandCaptions = class(TCustomPanel)
private
  FDblClick : boolean;
  FDownBand : TprBand;
  FDownPointInfo : TprBandsCaptionsPointInfo;
  FHighlightedBand : TprBand;
  FPopupMenuBand : TprBand;
  FLastX : integer;
  FLastY : integer;
  FStartX : integer;
  FStartY : integer;
  FPopupMenu : TPopupMenu;
  FHintForm : TForm;
  procedure OnDefineLink(Sender : TObject);
  procedure OnDeleteLink(Sender : TObject);
  procedure OnShowBandProps(Sender : TObject);
  procedure CMMouseLeave(var Msg : TMessage); message CM_MOUSELEAVE;
protected
  function GetSize : integer;
  function DesignerPanel : TprCustomDesignerPanel;
  procedure SetMouseCursorState(b : TprBand; PointInfo : TprBandsCaptionsPointInfo; X,Y : integer);
  procedure GetPointInfoAt(x,y : integer; var b : TprBand; var PointInfo : TprBandsCaptionsPointInfo); virtual; abstract;
  procedure DrawAnime(x,y : integer); virtual; abstract;
  procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure DblClick; override;
  procedure ShowPopupMenu; virtual;
  function GetLinkMode : TprLinkType; virtual; abstract;
  function DsgnR(dc : TprDesignComponent) : TRect;
  procedure HighlightBand(Band : TprBand);
  procedure GetHintInfo(var BandRect : TRect; var IsVertical : boolean); virtual; abstract;
public
  property PopupMenu : TPopupMenu read FPopupMenu;
  constructor Create(AOwner : TComponent); override;
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TprHorBandCaptions
//
/////////////////////////////////////////////////
TprHorBandCaptions = class(TprBandCaptions)
protected
  procedure Paint; override;
  procedure GetPointInfoAt(x,y : integer; var b : TprBand; var PointInfo : TprBandsCaptionsPointInfo); override;
  procedure DrawAnime(x,y : integer); override;
  procedure ShowPopupMenu; override;
  function GetLinkMode : TprLinkType; override;
  procedure GetHintInfo(var BandRect : TRect; var IsVertical : boolean); override;
public
  constructor Create(AOwner : TComponent); override;
end;

/////////////////////////////////////////////////
//
// TprVerBandCaptions
//
/////////////////////////////////////////////////
TprVerBandCaptions = class(TprBandCaptions)
protected
  procedure Paint; override;
  procedure GetPointInfoAt(x,y : integer; var b : TprBand; var PointInfo : TprBandsCaptionsPointInfo); override;
  procedure DrawAnime(x,y : integer); override;
  procedure ShowPopupMenu; override;
  function GetLinkMode : TprLinkType; override;
  procedure GetHintInfo(var BandRect : TRect; var IsVertical : boolean); override;
public
  constructor Create(AOwner : TComponent); override;
end;

/////////////////////////////////////////////////
//
// TprCustomDesignerRuler
//
/////////////////////////////////////////////////
TprCustomDesignerRuler = class(TCustomPanel)
protected
  function DesignerPanel : TprCustomDesignerPanel;
end;

THackWinControl = class(TWinControl)
public
  property OnKeyDown;
end;

/////////////////////////////////////////////////
//
// TprCustomDesignerBox
//
/////////////////////////////////////////////////
TprCustomDesignerBox = class(TprScrollBox)
private
  FCanvas : TCanvas;

  FMouseMode : TprMouseMode;

  FDownObject : TprDesignComponent;
  FDownPointInfo : TprPointInfo;
  FDownResizeMode : TprResizeType;
  FDownLinkMode : TprLinkType;
  FFirstMoveAfterDown : boolean;
  FDblClick : boolean;
  FTickCount : DWORD;

  FInplaceEditor : TWinControl;
  FInplaceEditedObj : TprDesignComponent;
  FPrevObj : TprDesignComponent;

  FLastX : integer;
  FLastY : integer;
  FStartX : integer;
  FStartY : integer;
  FStartRealX : integer;
  FStartRealY : integer;
  FStuckedX : integer;
  FStuckedY : integer;

  FLinksControl : TprObjectLinksButton;
  FLinksObject : TprDesignComponent;
  FLinkPreviosObject : TprDesignComponent;
  FLinksControlPopupMenu : TPopupMenu;

  FSelectionDrawObject : TprSelectionDrawObject;

  FRightMouseButtonPressed : boolean;

  function GetDesignerPanel : TprCustomDesignerPanel;
  function GetSelCount : integer;
  function GetSelObj(i : integer) : TprDesignComponent;

  function GetStuckedOffs(Obj : TprDesignComponent; XOffs,YOffs : integer; AllowStuckSides : TprAllowStuckSidesSet; var StuckedX,StuckedY : integer) : boolean;

  procedure DrawLinks(DC : HDC);
  procedure DrawAnime(DC : HDC; x,y : integer; CallWhileEvents : boolean);
  procedure HideDrawing(DC : HDC);
  procedure ShowDrawing(DC : HDC);
  procedure InvertObject(dc : TprDesignComponent);

  procedure WMGetDlgCode(var Msg : TWMGetDlgCode); message WM_GETDLGCODE;
  procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
  procedure WMEraseBkgnd(var Msg : TWmEraseBkgnd); message WM_ERASEBKGND;
  procedure WMSysKeyChar(var Msg : TWMSysChar); message WM_SYSCHAR;
  procedure WMMouseWheel(var Msg : TWMMouseWheel); message WM_MOUSEWHEEL;

  procedure GetPointInfoAt(x,y : integer; var dc : TprDesignComponent; var PointInfo : TprPointInfo; var ResizeMode : TprResizeType; var LinkMode : TprLinkType);
  procedure SetMouseCursorState(X,Y : integer);

  procedure DoLink(SourceObject : TprDesignComponent; DestObject : TprDesignComponent; LinkMode : TprLinkType);
  procedure DoResize(ResizeObject : TprDesignComponent; oTop,oLeft,oBottom,oRight : integer; prps : pPrPaintStruct);
  procedure DoDrag(DragObject : TprDesignComponent; dx,dy : integer; prps : pPrPaintStruct);

  procedure _BeginPaint(var prps : rPrPaintStruct);
  procedure _EndPaint(const prps : rPrPaintStruct);

  procedure OnGetCountRects(Sender : TObject; var Count : integer);
  procedure OnGetRect(Sender : TObject; index : integer; var Rect : TRect);
  procedure OnGetAllowResizeTypes(Sender : TObject; index : integer; var AllowResizeTypes : TprResizeTypeSet);

  procedure SetLinksControl(dc : TprDesignComponent);
  procedure MakeLinkFromObject(dc : TprDesignComponent; LinkMode : TprLinkType);
  procedure OnLinksButtonClick(Sender : TObject);
  procedure OnLinksControlPopupMenuLink(Sender : TObject);
  procedure OnLinksControlPopupMenuDeleteLink(Sender : TObject);

  procedure OnInplaceEditorKeyDown(Sender: TObject; var Key : Word; Shift : TShiftState);
protected
  FSelectedRect : TRect;

  procedure DrawGrid(DC : HDC; BackRgn : HRGN); virtual;
  procedure DrawOther(DC : HDC); virtual;
  procedure InternalPaint(DC : HDC; Rgn : HRGN); virtual;

  function  GetExData : pointer; virtual;
  procedure InplaceEdit(EditedObject : TprObj); virtual;

  procedure KeyDown(var Key: Word; Shift: TShiftState); override;

  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure DblClick; override;

  function DsgnR(dc : TprDesignComponent) : TRect;
  function DsgnRSel(i : integer) : TRect;

  function DrawWidth : integer; virtual; abstract;
  function DrawHeight : integer; virtual; abstract;

  function GetPopupMenu : TPopupMenu; override;
public
  property SelCount : integer read GetSelCount;
  property SelObjs[i : integer] : TprDesignComponent read GetSelObj;
  property Canvas : TCanvas read FCanvas;
  property InplaceEditor : TWinControl read FInplaceEditor;
  property DesignerPanel : TprCustomDesignerPanel read GetDesignerPanel;
  property ObjectPopupMenu : TPopupMenu read FLinksControlPopupMenu;

  procedure RepaintSelectedObjects;
  procedure RepaintObject(dc : TprDesignComponent);
  procedure MakeObjectVisible(dc : TprDesignComponent);

  constructor Create(AOwner : TComponent); override;
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TprBandsCaptionsOptions
//
/////////////////////////////////////////////////
{Represents the options for area that contains the captions of bands.}
TprBandsCaptionsOptions = class(TprOptions)
private
  FVisible : boolean;
  procedure SetVisible(Value : boolean);
public
{Creates an instance of the TprBandsCaptionsOptions class.}
  constructor Create;

{Copies the properties of another object.
Parameters:
  Source - The source object.}
  procedure Assign(Source : TPersistent); override;

{See:
  TprOptions.WriteToIni}
  procedure WriteToIni(IniFile : TIniFile; const SectionName,Prefix : string); override;
{See:
  TprOptions.ReadFromIni}
  procedure ReadFromIni(IniFile : TIniFile; const SectionName,Prefix : string); override;
published
{Indicates whether the area is visible.}
  property Visible: boolean read FVisible write SetVisible default true;
end;

{TprOnSelectionChanged is the type for TprCustomDesignerPanel.OnSelectionChanged event.
This event occurs when list of selected objects is changed.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  SelCount - The number of selected objects.}
TprOnSelectionChanged = procedure (Sender: TObject; SelCount: integer) of object;

{TprOnLinkAdded is the type for TprCustomDesignerPanel.OnLinkAdded event.
This event occurs when new link between objects is defined.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  SourceObject - The object from that a link is defined.
  DestObject - The object to that a link is defined.
  LinkMode - The type of link.
See also:
  TprLinkType}
TprOnLinkAdded = procedure (Sender : TObject; SourceObject : TprDesignComponent; DestObject : TprDesignComponent; LinkMode : TprLinkType) of object;

{TprOnObjectResized is the type for TprCustomDesignerPanel.OnObjectResized event.
This event occurs when an object is resized.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  ResizeObject - The object that is resized.}
TprOnObjectResized = procedure (Sender : TObject; ResizeObject : TprDesignComponent) of object;

{TprOnObjectDrag is the type for TprCustomDesignerPanel.OnObjectDrag event.
This event occurs when an object is dragged.
If the set of objects was dragged then event occurs for each object.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  DragObject - The object that is dragged.}
TprOnObjectDrag = procedure (Sender : TObject; DragObject : TprDesignComponent) of object;

{TprOnObjectInserted is the type for TprCustomDesignerPanel.OnObjectInserted event.
This event occurs when the new object added to the report template.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  InsertedObject - The added object.}
TprOnObjectInserted = procedure (Sender : TObject; InsertedObject : TprObj) of object;

{TprOnBandInserted is the type for TprCustomDesignerPanel.OnBandInserted event.
This event occurs when the new band added to the report template.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  InsertedBand - The added band.}
TprOnBandInserted = procedure (Sender: TObject; InsertedBand: TprBand) of object;

{TprOnInplaceEditObject is the type for TprCustomDesignerPanel.OnInplaceEditObject event.
This event occurs when the user starts the inplace-edit of object.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  EditedObject - The edited TprObj object.}
TprOnInplaceEditObject = procedure (Sender : TObject; EditedObject : TprObj) of object;

{TprOnEndInplaceEditObject is the type for TprCustomDesignerPanel.OnEndInplaceEditObject event.
This event occurs when the user ends the inplace-edit of object and cancels the changes.
Parameters:
  Sender - The TprCustomDesignerPanel object.}
TprOnEndInplaceEditObject = procedure (Sender : TObject) of object;

{TprOnSaveInplaceEditObject is the type for TprCustomDesignerPanel.OnSaveInplaceEditObject event.
This event occurs when the user ends the inplace-edit of object and saves the changes.
Parameters:
  Sender - The TprCustomDesignerPanel object.}
TprOnSaveInplaceEditObject = procedure (Sender : TObject) of object;

{TprOnActivePageChanged is the type of TprCustomDesignerPanel.OnActivePageChanged event.
This event occurs when active page of the report template is changed.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  ActivePageIndex - The index of the selected page.
  ActivePage - The TprCustomPage object that corresponds to the selected page.}
TprOnActivePageChanged = procedure (Sender : TObject; ActivePageIndex : integer; ActivePage : TprCustomPage) of object;

{TprOnApplyObjectsProps is the type of TprCustomDesignerPanel.OnApplyObjectsProps event.
This event occurs when user press "OK" or "Apply" in the window of the object's properties.
Parameters:
  Sender - The TprCustomDesignerPanel object.}
TprOnApplyObjectsProps = procedure (Sender : TObject) of object;

{TprOnWhileObjectResize is the type of TprCustomDesignerPanel.OnWhileObjectResize event.
This event occurs while the user is resizing the object.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  ResizeRect - Specifies the currently selected rectangle of object.}
TprOnWhileObjectResize = procedure (Sender : TObject; const ResizeRect : TRect) of object;

{TprOnWhileObjectDrag is the type of TprCustomDesignerPanel.OnWhileObjectDrag event.
This event occurs while the user is dragging the object.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  DragRect - Specifies the currently selected rectangle of object.}
TprOnWhileObjectDrag = procedure (Sender : TObject; const DragRect : TRect) of object;

{TprOnDesignerDblClick is the type of TprCustomDesignerPanel.OnDesignerDblClick event.
Occurs when the user double-clicks the left mouse button when the mouse pointer is over the control.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  X, Y - Specify the point within control.
  Shift - Indicates the state of the Alt, Ctrl, and Shift keys and the mouse buttons.
  Processed - Return the true in this parameter if you want to disable default processing.}
TprOnDesignerDblClick = procedure (Sender : TObject; X,Y : integer; Shift : TShiftState; var Processed : boolean) of object;

{TprDesignerMouseEvent is the type of TprCustomDesignerPanel.OnDesignerMouseDown event.
Occurs when the user presses a mouse button with the mouse pointer over a control.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  X, Y - Specify the point within control.
  Shift - Indicates the state of the Alt, Ctrl, and Shift keys and the mouse buttons.
  Processed - Return the true in this parameter if you want to disable default processing.}
TprDesignerMouseEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Processed : boolean) of object;

{TprDesignerMouseUpEvent is the type of TprCustomDesignerPanel.OnDesignerMouseUp event.
Occurs when the user releases a mouse button with the mouse pointer over a control.
Parameters:                                                                    
  Sender - The TprCustomDesignerPanel object.
  X, Y - Specify the point within control.
  Shift - Indicates the state of the Alt, Ctrl, and Shift keys and the mouse buttons.}
TprDesignerMouseUpEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;

{TprOnOpenTemplate is the type for TprCustomDesignerPanel.OnOpenTemplate event.
Occurs when user want to load the new report template.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  Stream - Return the TStream in this object if this parameter is not NIL, then the
stream will be used for loading the report template.
If this parameter is NIL - the "Open dialog" will be opened to select the file for load.
  CancelOpen - Return the true in this parameter if you want to disable default processing
and load the report template manually.}
TprOnOpenTemplate = procedure (Sender: TObject; var Stream : TStream; var CancelOpen : boolean; var IsBinaryFormat : boolean) of object;

{TprOnSaveTemplate is the type for TprCustomDesignerPanel.OnSaveTemplate event.
Occurs when user want to save the report template.
Parameters:
  Sender - The TprCustomDesignerPanel object.
  Stream - Return the TStream in this object if this parameter is not NIL, then the
stream will be used for saving the report template.
If this parameter is NIL - the "Save dialog" will be opened to select the file for save.
  CancelSave - Return the true in this parameter if you want to disable default processing
and save the report template manually.}
TprOnSaveTemplate = procedure (Sender : TObject; var Stream : TStream; var CancelSave : boolean; var IsBinaryFormat : boolean; IsSaveAs : boolean) of object;

/////////////////////////////////////////////////
//
// TprCustomDesignerPanel
//
/////////////////////////////////////////////////
{TprCustomDesignerPanel represents the base class for the designer panel.
This class has two descedants:<br>
TprDesignerPanel - Designs the TprReport template.<br>
TprTxDesignerPanel - Designs the TprTxReport template.<br>
You can use this controls if you want to develop non standard report designers
with some unique features.
See also:
  TprDesignerPanel, TprTxDesignerPanel}
TprCustomDesignerPanel = class(TCustomPanel)
private
  FSelObjs : TList;
  FPastePRPS : rPrPaintStruct;
  FCurClassRef : TprObjClass;
  FDisablePageDraw : boolean;

  FActivePageIndex : integer;

  FStuckMode : TprStuckMode;
  FStuckOptions : TprStuckOptionsSet;
  FStuckOffs : integer;

  FHorBandsCaptionsOptions : TprBandsCaptionsOptions;
  FVerBandsCaptionsOptions : TprBandsCaptionsOptions;

  FOpacityObjectLinksForm : integer;
  FOpacityPosSizeForm : integer;
  FOpacityObjectsPropsForm : integer;
  FOpacityFindForm : integer;

  FPopupInsertObjectMenu : boolean;
  FPopupInsertBandMenu : boolean;
  FPopupMainMenu : boolean;

  FFileName : string;

  FOnSelectionChanged : TprOnSelectionChanged;
  FOnLinkAdded : TprOnLinkAdded;
  FOnObjectResized : TprOnObjectResized;
  FOnObjectDrag : TprOnObjectDrag;
  FOnObjectInserted : TprOnObjectInserted;
  FOnBandInserted : TprOnBandInserted;
  FOnInplaceEditObject : TprOnInplaceEditObject;
  FOnEndInplaceEditObject : TprOnEndInplaceEditObject;
  FOnSaveInplaceEditObject : TprOnSaveInplaceEditObject;
  FOnActivePageChanged : TprOnActivePageChanged;
  FOnApplyObjectsProps : TprOnApplyObjectsProps;
  FOnWhileObjectResize : TprOnWhileObjectResize;
  FOnWhileObjectDrag : TprOnWhileObjectDrag;
  FOnDesignerMouseMove : TMouseMoveEvent;
  FOnDesignerMouseDown : TprDesignerMouseEvent;
  FOnDesignerMouseUp : TprDesignerMouseUpEvent;
  FOnDesignerDblClick : TprOnDesignerDblClick;
  FOnInsertingObjectChanged : TNotifyEvent;
  FOnFileNameChanged : TNotifyEvent;
  FOnOpenTemplate : TprOnOpenTemplate;
  FOnSaveTemplate : TprOnSaveTemplate;

  function GetOnDragOver : TDragOverEvent;
  function GetOnDragDrop : TDragDropEvent;
  function GetOnStartDrag : TStartDragEvent;
  function GetOnEndDrag : TEndDragEvent;
  procedure SetOnDragOver(Value : TDragOverEvent);
  procedure SetOnDragDrop(Value : TDragDropEvent);
  procedure SetOnStartDrag(Value : TStartDragEvent);
  procedure SetOnEndDrag(Value : TEndDragEvent);

  procedure PasteCallBack(Component : TComponent);
  procedure PasteSetName(Reader: TReader; Component: TComponent; var Name: string);
  procedure SelectedObjectsToClipboard;
  procedure InternalClearSelected;
  procedure InternalSelectObj(dc : TprDesignComponent);
  procedure InternalDeSelectObj(dc : TprDesignComponent);

  function GetCurPage : TprCustomPage;
  procedure SetActivePageIndex(Value : integer);
  procedure SetHorBandsCaptionsOptions(Value : TprBandsCaptionsOptions);
  procedure SetVerBandsCaptionsOptions(Value : TprBandsCaptionsOptions);
  function GetShowPopupMenu : boolean;
  procedure SetShowPopupMenu(Value : boolean);

  function GetSelObj(i : integer) : TprDesignComponent;
  function GetSelObject : TprObj;
  function GetSelCount : integer;
  function GetInplaceEditor : TWinControl;

  function GetVisibleObjectLinksForm : boolean;
  function GetVisiblePosSizeForm : boolean;
  function GetVisibleObjectsPropsForm : boolean;
  function GetVisibleFindForm : boolean;
  procedure SetVisibleObjectLinksForm(Value : boolean);
  procedure SetVisiblePosSizeForm(Value : boolean);
  procedure SetVisibleObjectsPropsForm(Value : boolean);
  procedure SetVisibleFindForm(Value : boolean);

  function GetOpacityObjectLinksForm : integer;
  function GetOpacityPosSizeForm : integer;
  function GetOpacityObjectsPropsForm : integer;
  function GetOpacityFindForm : integer;
  procedure SetOpacityObjectLinksForm(Value : integer);
  procedure SetOpacityPosSizeForm(Value : integer);
  procedure SetOpacityObjectsPropsForm(Value : integer);
  procedure SetOpacityFindForm(Value : integer);

  function GetVerScrollBoxPos : integer;
  function GetHorScrollBoxPos : integer;
  procedure SetVerScrollBoxPos(Value : integer);
  procedure SetHorScrollBoxPos(Value : integer);
  function GetScrollBoxPos : TPoint;
  procedure SetScrollBoxPos(Pos : TPoint);

  procedure SetFileName(Value : string);

  procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
  procedure WMFreeInplaceEdit(var Msg : TMessage); message WM_FREEINPLACEEDIT;

  function GetTabPosition : TTabPosition;
  procedure SetTabPosition(Value : TTabPosition);
  function GetTabStyle : TTabStyle;
  procedure SetTabStyle(Value : TTabStyle);
protected
  FSelectedObjectsRect : TRect;

  FHorBandsCaptions : TprHorBandCaptions;
  FVerBandsCaptions : TprVerBandCaptions;
  FDesignerBox : TprCustomDesignerBox;
  FHorRuler : TprCustomDesignerRuler;
  FVerRuler : TprCustomDesignerRuler;
  FTabControl : TTabControl;

  FObjectLinksForm : TprCustomObjLinksForm;
  FPosSizeForm : TprCustomPosSizeForm;
  FObjectsPropsForm : TprCustomObjectsPropsForm;
  FFindForm : TprCustomTemplateFindForm;
  FUserObjectPropsForm : TprUserObjectPropsForm;

  FPopupMenu: TPopupMenu;
  FDesignerNotifyLink: TprNotifyLink;

  procedure UpdateTabControlCaptions;
  procedure InternalSetReport(Value : TprCustomReport); virtual;
  function CreateDesignerBox : TprCustomDesignerBox; virtual; abstract;
  procedure CreateRulers(var HorRuler,VerRuler : TprCustomDesignerRuler); virtual; abstract;
  function CreateObjectLinksForm : TprCustomObjLinksForm; virtual;
  function CreatePosSizeForm : TprCustomPosSizeForm; virtual;
  function CreateObjectsPropsForm : TprCustomObjectsPropsForm; virtual;
  function CreateFindForm : TprCustomTemplateFindForm; virtual;

  function GetPageTabCaption(Page : TprCustomPage) : string;
  function GetShowGrid : boolean; virtual;
  procedure SetShowGrid(Value : boolean); virtual;
  function GetUseGrid : boolean; virtual;
  procedure SetUseGrid(Value : boolean); virtual;

  procedure AdjustToGrid(var X,Y : integer); virtual;
  procedure AdjustMouseToGrid(var X,Y : integer); 

  function HorRulerHeight : integer;
  function VerRulerWidth : integer;
  function HorBandsCaptionsWidth : integer;
  function VerBandsCaptionsHeight : integer;

  procedure Resize; override;
  procedure AlignChildControls; virtual;
  procedure AlignControls(AControl: TControl; var Rect: TRect); override;

  procedure OnVScroll(Sender : TObject; Msg : TWMScroll);
  procedure OnHScroll(Sender : TObject; Msg : TWMScroll);
  procedure OnTabControlResize(Sender : TObject);
  procedure OnTabControlChange(Sender : TObject);
  procedure OnChangeBandsCaptionsOptions(Sender : TObject);
  procedure OnPopupMenuPopup(Sender : TObject);
  procedure OnPopupMenuClick(Sender : TObject);
  procedure OnPopupMenuInsertObjectClick(Sender : TObject);  
  procedure OnPopupMenuInsertBandClick(Sender : TObject);  

  function GetSelectedRect : TRect;
  procedure UpdateSelectedObjectsRect;

  procedure DoSelectionChanged;
  procedure DoLinkAdded(SourceObject : TprDesignComponent; DestObject : TprDesignComponent; LinkMode : TprLinkType);
  procedure DoObjectResized(ResizeObject : TprDesignComponent);
  procedure DoObjectDrag(DragObject : TprDesignComponent);
  procedure DoObjectInserted(InsertedObject : TprObj);
  procedure DoBandInserted(InsertedBand : TprBand);
  procedure DoInplaceEditObject(EditedObject : TprObj);
  procedure DoEndInplaceEditObject;
  procedure DoSaveInplaceEditObject;
  procedure DoActivePageChanged(Sender : TObject);
  procedure DoApplyObjectsProps;
  procedure DoWhileObjectResize(const ResizeRect : TRect);
  procedure DoWhileObjectDrag(const DragRect : TRect);
  procedure DoDesignerMouseMove(Shift : TShiftState; X,Y : integer);
  procedure DoDesignerMouseDown(Button: TMouseButton; Shift : TShiftState; X,Y : integer; var Processed : boolean);
  procedure DoDesignerMouseUp(Button: TMouseButton; Shift : TShiftState; X,Y : integer);
  procedure DoDesignerDblClick(var Processed : boolean);
  procedure DoInsertingObjectChanged;
  procedure DoOnFileNameChanged;
  procedure DoOnOpenTemplate(var Stream : TStream; var CancelOpen : boolean; var IsBinaryFormat : boolean);
  procedure DoOnSaveTemplate(var Stream : TStream; var CancelSave : boolean; var IsBinaryFormat : boolean; IsSaveAs : boolean);

  procedure UpdateObjectLinksForm;
  procedure UpdatePosSizeForm;
  procedure UpdateObjectsPropsForm;
  procedure UpdateFindForm;

  procedure Notification(AComponent : TComponent; AOperation : TOperation); override;

  procedure DsgnNotify(Source: TObject); virtual;

  function GetDefaultHeightForHorizontalBand : integer; virtual; abstract;
  function GetDefaultWidthForVerticalBand : integer; virtual; abstract;

  procedure InitPopupMenu(Popup : TPopupMenu); virtual;
  procedure InitPopupMainMenu(Popup : TPopupMenu; MainMenuItem : TMenuItem); virtual;

  function DsgnR(dc : TprDesignComponent) : TRect; virtual;
  function DsgnRSel(i : integer) : TRect;
  function DsgnWSel(i : integer) : integer;
  function DsgnHSel(i : integer) : integer;

  function ObjR(dc : TprDesignComponent) : TRect;
  function ObjRSel(i : integer) : TRect;

  function GridSizeX : integer; virtual; abstract;
  function GridSizeY : integer; virtual; abstract;

  property ShowGrid : boolean read GetShowGrid write SetShowGrid;
  property UseGrid : boolean read GetUseGrid write SetUseGrid;

  property StuckMode : TprStuckMode read FStuckMode write FStuckMode;
  property StuckOptions : TprStuckOptionsSet read FStuckOptions write FStuckOptions;
  property StuckOffs : integer read FStuckOffs write FStuckOffs;
public
{Lists the selected objects.
Parameters:
  I - Index of the object, from 0.}
  property SelObjs[I: integer]: TprDesignComponent read GetSelObj;
{Returns the number of selected objects.}
  property SelCount: Integer read GetSelCount;
{Returns the list of selected objects.}
  property SelObjsList: TList read FSelObjs;
{Returns the nil if selection has more than one object and returs the selected object
if selection has one object.}
  property SelObject: TprObj read GetSelObject;
{Returns the currently selected page of the report template.}
  property CurPage: TprCustomPage read GetCurPage;
{Returns the TWinControl object that is used for inplace-editing,
returns the NIL if inplace-editing is not started .}
  property InplaceEditor : TWinControl read GetInplaceEditor;

{Returns the TprCustomReport object that is being edited through this control.}
  function GetReport : TprCustomReport; virtual; abstract;

{Returns the index of active page of the report template.}
  property ActivePageIndex : integer read FActivePageIndex write SetActivePageIndex;

{Returns the options of horizontal bands area.
See also:
  TprBandsCaptionsOptions}
  property HorBandsCaptionsOptions : TprBandsCaptionsOptions read FHorBandsCaptionsOptions write SetHorBandsCaptionsOptions;
{Returns the options of vertical bands area.
See also:
  TprBandsCaptionsOptions}
  property VerBandsCaptionsOptions : TprBandsCaptionsOptions read FVerBandsCaptionsOptions write SetVerBandsCaptionsOptions;

{Indicates whether the "Links" popup form is visible.
This form allows to edit the links between objects.}
  property VisibleObjectLinksForm: Boolean read GetVisibleObjectLinksForm write SetVisibleObjectLinksForm;
{Indicates whether the "Position & sizes" popup form is visible.
This form allows to edit the sizes and positions of selected objects.}
  property VisiblePosSizeForm: Boolean read GetVisiblePosSizeForm write SetVisiblePosSizeForm;
{Indicates whether the "Properties" popup form is visible.
This form allows to edit the properties of selected objects.}
  property VisibleObjectsPropsForm: Boolean read GetVisibleObjectsPropsForm write SetVisibleObjectsPropsForm;
{Indicates whether the "Find" popup form is visible.
This form allows to find the data in the report template.}
  property VisibleFindForm: Boolean read GetVisibleFindForm write SetVisibleFindForm;

{Specifies the opacity for "Links" popup form.}
  property OpacityObjectLinksForm : integer read GetOpacityObjectLinksForm write SetOpacityObjectLinksForm;
{Specifies the opacity for "Position & sizes" popup form.}
  property OpacityPosSizeForm : integer read GetOpacityPosSizeForm write SetOpacityPosSizeForm;
{Specifies the opacity for "Properties" popup form.}
  property OpacityObjectsPropsForm : integer read GetOpacityObjectsPropsForm write SetOpacityObjectsPropsForm;
{Specifies the opacity for "Find" popup form.}
  property OpacityFindForm : integer read GetOpacityFindForm write SetOpacityFindForm;

{Specifies the current position of the vertical scroll bar.}
  property VerScrollBoxPos : integer read GetVerScrollBoxPos write SetVerScrollBoxPos;
{Specifies the current position of the horizontal scroll bar.}
  property HorScrollBoxPos : integer read GetHorScrollBoxPos write SetHorScrollBoxPos;
{Specifies the current scroll position.}
  property ScrollBoxPos : TPoint read GetScrollBoxPos write SetScrollBoxPos;

{Returns the rectangle in which are placed all selected objects.}
  property SelectedObjectsRect : TRect read FSelectedObjectsRect;

{Specifies the name of file that is opened.}
  property FileName : string read FFileName write SetFileName;

{Starts the inplace editing.
Parameters:
  EditedObject - The TprObj object for that the inplace editing is started.}
  procedure InplaceEdit(EditedObject : TprObj);
{Ends the inplace-editing and cancels changes.}
  procedure EndInplaceEdit;
{Ends the inplace-editing and saves changes.}
  procedure SaveInplaceEdit;
{Returns the true value if inplace-editing can be started.}
  function AllowInplaceEdit: boolean;
{Returns the true value if inplace-editing is started.}
  function IsInplaceEdit: boolean;

{Returns the information about a spot within the control.
Parameters:
  X, Y - Specifies the spot within control.
  dc - Returns the TprDesignComponent object (TprBand or TprObj) that is in the spot.
  PointInfo - Returns the type of place within object.
  ResizeMode - Returns the type of resizing if spot is in the place that can be used for resize an object.
  LinkMode - Returns the type of linking if spot is in the place that can be used for define a link from object.
See also:
  TprPointInfo, TprResizeType, TprLinkType}
  procedure GetPointInfoAt(x,y : integer; var dc : TprDesignComponent; var PointInfo : TprPointInfo; var ResizeMode : TprResizeType; var LinkMode : TprLinkType);

{Returns the true if selection contains TprObj objects.}
  function IsAnyTprObjSelected : boolean;
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
  function GetNumDragSelectedRegions : integer;
{Returns the number of selected objects that support the resizing.
Parameters:
  ResizeTypesSet - The type of resizing.
See also:
  TprResizeTypeSet}
  function GetNumResizeSelectedRegions(ResizeTypesSet: TprResizeTypeSet) : integer;

{Pastes the clipboard content into the report template.}
  procedure Paste;
{Cuts the selected objects into clipboard.
Only the TprObj objects can be cut into clipboard, bands can not be cut.}
  procedure Cut;
{Copies the selected objects into clipboard.
Only the TprObj objects can be copied into clipboard, bands can not be copied.}
  procedure Copy;
{Deletes the selected objects.}
  procedure DeleteSelectedObjects;
{Starts the inserting of the object of specified type into the currently selected band.
Parameters:
  ObjClassRef - Specifies the type of object to insert.}
  procedure InsertObject(ObjClassRef: TprObjClass);
{Inserts the band of specified type into the current page of the report template.
Parameters:
  BandType - Specifies the type of band to insert.
See also:
  TprBandType}
  function InsertBand(BandType : TprBandType) : TprBand;

{Aligns the selected objects.
Parameters:
  ActionCode - The type of align action.
See also:
  TprAlignActionCode}
  procedure AlignAction(ActionCode: TprAlignActionCode);
{Returns the true if specified align action is allowed.
Parameters:
  ActionCode - The type of align action}
  function AlignActionAllowed(ActionCode: TprAlignActionCode) : boolean;
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

{Converts the coordinates that are represented by rectangle from report's units to the designer's units.
Parameters:
  rSource - The rectangle in the report's coordinates.
  rDest - The rectangle in the designer's coordinates.}
  procedure ConvertToDesignerCoords(const rSource : TRect; var rDest : TRect); virtual;
{Converts the X coordinate from report's units to the designer's units.
Parameters:
  X - The coordinate in the report's coordinates.
Return value:
  Returns the value of X that was converted to designer's units.}
  function ConvertXToDesignerCoords(X : integer) : integer; virtual;
{Converts the Y coordinate from report's units to the designer's units.
Parameters:
  Y - The coordinate in the report's coordinates.
Return value:
  Returns the value of Y that was converted to designer's units.}
  function ConvertYToDesignerCoords(Y : integer) : integer; virtual;
{Converts the coordinates that are represented by rectangle from designer's units to the reports's units.
Parameters:
  rSource - The rectangle in the designer's coordinates.
  rDest - The rectangle in the report's coordinates.}
  procedure ConvertFromDesignerCoords(const rSource : TRect; var rDest : TRect); virtual;
{Converts the X coordinate from designer's units to the report's units.
Parameters:
  X - The coordinate in the designer's coordinates.
Return value:
  Returns the value of X that was converted to designer's units.}
  function ConvertXFromDesignerCoords(X : integer) : integer; virtual;
{Converts the Y coordinate from designer's units to the report's units.
Parameters:
  Y - The coordinate in the report's coordinates.
Return value:
  Returns the value of Y that was converted to report's units.}
  function ConvertYFromDesignerCoords(Y : integer) : integer; virtual;
{Translates a given point from client area coordinates (which are specified
relative to the top-left corner of the designer panel) to band coordinates.
See the 12_Drag-Drop demo as example of using this method.
Parameters:
  Band - The TprBand object.
  P - Specifies the point's coordinates.
Return value:
  Returns the coordinates relative to the top-left corner of the band.}
  function ClientToBandPoint(Band : TprBand; const p : TPoint) : TPoint; virtual;
  
{Sets the value of the specified property of selected objects.
Parameters:
  Prop - Specifies the property to changing.
  Value - Specifies the new value of property.
See also:
  TprObjectPosSizeProps}
  procedure SetPosSizeProp(Prop: TprObjectPosSizeProps; Value: integer);

{Puts the selected objects behind all other objects.}
  procedure SendToBack;
{Puts the selected objects in front of all other objects in its band.}
  procedure BringToFront;

{Deletes the link between band and object.
Parameters:
  Band - Specifies the band from that link is defined.
  LinkedObj - Specifies the object to which link is defined.}
  procedure DeleteBandLink(Band : TprBand; LinkedObj : TprObj);
{Deletes the link between two objects.
Parameters:
  Obj - Specifies the object from that link is defined.
  LinkType - Specifies the type of link.
  LinkedObj - Specifies the object to which link is defined.
See also:
  TprLinkType}
  procedure DeleteObjLink(Obj : TprObj; LinkType : TprLinkType; LinkedObj : TprObj);
{Starts the defining of link from object.
Parameters:
  dc - The object (band or TprObj) from that the link will be defined.
  LinkMode - The type of link.}
  procedure MakeLinkFromObject(dc : TprDesignComponent; LinkMode : TprLinkType);

{Creates the new page of the report template and inserts it in the report template.
The created page will become active after creating.
Parameters:
  AfterPageIndex - The created page will be inserted after the page with this index.}
  procedure InsertPageAfter(AfterPageIndex: integer);
{Deletes the page.
Parameters:
  PageIndex - The page's index.}
  procedure DeletePage(PageIndex: integer);
{Opens the dialog window in that the page properties can be changed (width, height, paper's size and so on).
Parameters:
  PageIndex - The page's index.
Return value:
  Returns the true if user press "OK" in the dialog.}
  function EditPage(PageIndex : integer) : boolean; virtual;
{Activates the next page of the report template.}
  procedure NextPage;
{Activates the previous page of the report template.}
  procedure PriorPage;

{Clears the current report template, and adds one empty page.}
  procedure New;
{Opens the report template.
By default this method opens the "Open file" dialog in that the user can select
the file name to load. Use the OnOpenTemplate method to provide the custom template source.
See also:
  OnOpenTemplate, TprOnOpenTemplate}
  procedure Open;
{Saves the report template.
By default this method opens the "Save file" dialog in that the user can select
the file name in that the report template will be saved.
Use the OnSaveTemplate method to save the report template in DB or another place.
See also:
  OnSaveTemplate, TprOnSaveTemplate}
  procedure Save;
  procedure SaveAs;
{Generates the report and prints its.}
  procedure Print;
{Generates the report and opens the preview window.}
  procedure Preview;
{Opens the dialog window in that the report properties can be changed.}
  procedure EditReportParams; virtual;
{Opens the groups designer.}
  procedure EditGroups;
{Opens the aggrigate variables designer.}
  procedure EditValues;
{Opens the simple variables designer.}
  procedure EditVariables;

{Returns the true if specified object is selected.}
  function IsObjSelected(dc : TprDesignComponent) : boolean;
{Selects the specified object, the previously selected objects are lose the selection.}
  procedure SelectObject(dc : TprDesignComponent);
{Adds the specified object to selection.}
  procedure AddSelectObject(dc : TprDesignComponent);
{Removes the specified object from selection.}
  procedure DeSelectObject(dc : TprDesignComponent);
{Clears the selection.}
  procedure ClearSelection;

{Makes an object visible.
Parameters:
  dc - The object that must be visible.}
  procedure MakeObjectVisible(dc : TprDesignComponent);

{Repaints the selected objects.}
  procedure RepaintSelectedObjects;
{Repaints the selected objects and updates the properties forms.}
  procedure UpdateSelectedObjects;
{Repaints the specified object and updates the properties forms if object is selected.}
  procedure UpdateObject(dc : TprDesignComponent);
{Updates the currently edited page.}
  procedure UpdateCurPage; virtual;
{Updates the control.}
  procedure FullUpdate;

{Clears the report template that is linked to the control.}
  procedure ClearReportTemplate;

{Saves the control settings in the INI file.
Parameters:
  IniFile - The destination TIniFile object.
  SectionName - The name of section of INI file.}
  procedure WriteToIni(IniFile : TIniFile; const SectionName : string); virtual;
{Loads the control settings from the INI file.
Parameters:
  IniFile - The source TIniFile object.
  SectionName - The name of section of INI file.}
  procedure ReadFromIni(IniFile : TIniFile; const SectionName : string); virtual;

{Opens the built-in dialog for editing the control properties.
Return value:
  Returns the true if user press "OK" in the window.}
  function EditOptions: boolean; virtual;

{Call this method to notify all other objects about the changes in the report template (to force their updating).}
  procedure DsgnNotifyReport(TemplateChanged: boolean); virtual;

{Creates an instance of the TprCustomDesignerPanel class.}
  constructor Create(AOwner : TComponent); override;
{Frees an instance of the TprCustomDesignerPanel class.}
  destructor Destroy; override;
published
  property Align;
{Specifies the value indicating whether the built-in popup menu must be displayed if user click
the right mouse button within a control's area.}
  property ShowPopupMenu: boolean read GetShowPopupMenu write SetShowPopupMenu default true;

{Specifies the value indicating whether the built-in popup menu must contain items for inserting
the new objects into the report template.}
  property PopupInsertObjectMenu : boolean read FPopupInsertObjectMenu write FPopupInsertObjectMenu default true;
{Specifies the value indicating whether the built-in popup menu must contain items for inserting
the bands into the report template.}
  property PopupInsertBandMenu : boolean read FPopupInsertBandMenu write FPopupInsertBandMenu default true;
{Specifies the value indicating whether the built-in popup menu must contain items for actions: Open, Save and so on.}
  property PopupMainMenu : boolean read FPopupMainMenu write FPopupMainMenu default false;

{Determines whether tabs appear at the top or bottom.}
  property TabPostion : TTabPosition read GetTabPosition write SetTabPosition default tpTop;
{Specifies the style of tabs.}
  property TabStyle : TTabStyle read GetTabStyle write SetTabStyle default tsTabs;

{This event occurs when list of selected objects is changed.
See also:
  TprOnSelectionChanged}
  property OnSelectionChanged: TprOnSelectionChanged read FOnSelectionChanged write FOnSelectionChanged;

{This event occurs when new link between objects is defined.
See also:
  TprOnLinkAdded}
  property OnLinkAdded: TprOnLinkAdded read FOnLinkAdded write FOnLinkAdded;

{This event occurs when an object is resized.
See also:
  TprOnObjectResized}
  property OnObjectResized: TprOnObjectResized read FOnObjectResized write FOnObjectResized;
  
{This event occurs when an object is dragged.
If the set of objects was dragged then event occurs for each object.
See also:
  TprOnObjectDrag}
  property OnObjectDrag: TprOnObjectDrag read FOnObjectDrag write FOnObjectDrag;
  
{This event occurs when the new object added to the report template.
See also:
  TprOnObjectInserted}
  property OnObjectInserted: TprOnObjectInserted read FOnObjectInserted write FOnObjectInserted;
  
{This event occurs when the new band added to the report template.
See also:
  TprOnBandInserted}
  property OnBandInserted: TprOnBandInserted read FOnBandInserted write FOnBandInserted;
  
{This event occurs when the user starts the inplace-edit of object.
See also:
  TprOnInplaceEditObject}
  property OnInplaceEditObject: TprOnInplaceEditObject read FOnInplaceEditObject write FOnInplaceEditObject;

{This event occurs when the user ends the inplace-edit of object and cancels the changes.
See also:
  TprOnEndInplaceEditObject}
  property OnEndInplaceEditObject: TprOnEndInplaceEditObject read FOnEndInplaceEditObject write FOnEndInplaceEditObject;
  
{This event occurs when the user ends the inplace-edit of object and saves the changes.
See also:
  TprOnSaveInplaceEditObject}
  property OnSaveInplaceEditObject: TprOnSaveInplaceEditObject read FOnSaveInplaceEditObject write FOnSaveInplaceEditObject;

{This event occurs when active page of the report template is changed.
See also:
  TprOnActivePageChanged}
  property OnActivePageChanged: TprOnActivePageChanged read FOnActivePageChanged write FOnActivePageChanged;

{This event occurs when user press "OK" or "Apply" in the window of the object's properties.
See also:
  TprOnApplyObjectsProps}
  property OnApplyObjectsProps: TprOnApplyObjectsProps read FOnApplyObjectsProps write FOnApplyObjectsProps;

{This event occurs while the user is resizing the object.
See also:
  TprOnWhileObjectResize}
  property OnWhileObjectResize: TprOnWhileObjectResize read FOnWhileObjectResize write FOnWhileObjectResize;

{This event occurs while the user is dragging the object.
See also:
  TprOnWhileObjectDrag}
  property OnWhileObjectDrag: TprOnWhileObjectDrag read FOnWhileObjectDrag write FOnWhileObjectDrag;

{This event occurs when user move the mouse over control.}
  property OnDesignerMouseMove: TMouseMoveEvent read FOnDesignerMouseMove write FOnDesignerMouseMove;

{Occurs when the user presses a mouse button with the mouse pointer over a control.
See also:
  TprDesignerMouseEvent}
  property OnDesignerMouseDown: TprDesignerMouseEvent read FOnDesignerMouseDown write FOnDesignerMouseDown;

{Occurs when the user releases a mouse button with the mouse pointer over a control.
See also:
  TprDesignerMouseUpEvent}
  property OnDesignerMouseUp: TprDesignerMouseUpEvent read FOnDesignerMouseUp write FOnDesignerMouseUp;

{Occurs when the user double-clicks the left mouse button when the mouse pointer is over the control.
See also:
  TprOnDesignerDblClick}
  property OnDesignerDblClick: TprOnDesignerDblClick read FOnDesignerDblClick write FOnDesignerDblClick;

{Occurs when the user select an object to insert from the popup menu.}
  property OnInsertingObjectChanged: TNotifyEvent read FOnInsertingObjectChanged write FOnInsertingObjectChanged;

{Occurs when the name of the edited file is changed, when user open another report template for example.}
  property OnFileNameChanged: TNotifyEvent read FOnFileNameChanged write FOnFileNameChanged;

{Occurs when the user drags an object over a control.}
  property OnDragOver: TDragOverEvent read GetOnDragOver write SetOnDragOver;
{Occurs when the user drops an object being dragged.}
  property OnDragDrop: TDragDropEvent read GetOnDragDrop write SetOnDragDrop;
{Occurs when the user begins to drag the control or an object it contains by left-clicking on the control and holding the mouse button down.}
  property OnStartDrag: TStartDragEvent read GetOnStartDrag write SetOnStartDrag;
{Occurs when the dragging of an object ends, either by dropping the object or by canceling the dragging.}
  property OnEndDrag: TEndDragEvent read GetOnEndDrag write SetOnEndDrag;

{Occurs when user want to load the new report template.
See also:
  OnOpenTemplate}
  property OnOpenTemplate: TprOnOpenTemplate read FOnOpenTemplate write FOnOpenTemplate;
{Occurs when user want to save the report template.
See also:
  OnSaveTemplate}
  property OnSaveTemplate: TprOnSaveTemplate read FOnSaveTemplate write FOnSaveTemplate;
end;

procedure prRegisterUserObjectPropsForm(ObjClass : TprDesignComponentClass; FormClass : TprUserObjectPropsFormClass);
procedure prUnregisterUserObjectPropsForm(ObjClass : TprDesignComponentClass);

var
  BandImages : array [TprBandType] of TBitmap;

implementation

uses
  pr_multilang, pr_strings, pr_Link, pr_ObjectsProps, pr_BandHint,
  pr_GroupsEditor, pr_ValuesEditor, pr_TemplateFindForm, pr_VariablesEditor;

const
  aEdgeBorder : array [boolean] of integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);

  crDesignerPanelLinkAccepted = 3;
  crDesignerPanelLinkNotAccepted = 4;

type
  rprUserObjectPropsFormInfo = record
    ObjClass : TprDesignComponentClass;
    FormClass : TprUserObjectPropsFormClass;
  end;

var
  FLinkAcceptedCursor,FLinkNotAcceptedCursor : TCursor;
  prUserObjectPropsForms : array of rprUserObjectPropsFormInfo;

/////////////////////////////////////////////////
//
// Functions
//
/////////////////////////////////////////////////
procedure prRegisterUserObjectPropsForm(ObjClass : TprDesignComponentClass; FormClass : TprUserObjectPropsFormClass);
var
  i : integer;
begin
i := 0;
while (i<Length(prUserObjectPropsForms)) and (prUserObjectPropsForms[i].ObjClass<>ObjClass) do Inc(i);
if i<Length(prUserObjectPropsForms) then
  prUserObjectPropsForms[i].FormClass := FormClass
else
  begin
    SetLength(prUserObjectPropsForms,Length(prUserObjectPropsForms)+1);
    prUserObjectPropsForms[High(prUserObjectPropsForms)].ObjClass := ObjClass;
    prUserObjectPropsForms[High(prUserObjectPropsForms)].FormClass := FormClass;
  end;
end;

procedure prUnregisterUserObjectPropsForm(ObjClass : TprDesignComponentClass);
var
  i : integer;
begin
i := 0;
while (i<Length(prUserObjectPropsForms)) and (prUserObjectPropsForms[i].ObjClass<>ObjClass) do Inc(i);
if i<Length(prUserObjectPropsForms) then
  begin
    if i<>High(prUserObjectPropsForms) then
      MoveMemory(@(prUserObjectPropsForms[i]),@(prUserObjectPropsForms[i+1]),(High(prUserObjectPropsForms)-i)*sizeof(rprUserObjectPropsFormInfo));
    SetLength(prUserObjectPropsForms,Length(prUserObjectPropsForms)-1);
  end;
end;

/////////////////////////////////////////////////
//
// TprUserObjectPropsForm
//
/////////////////////////////////////////////////
function TprUserObjectPropsForm.GetReport : TprCustomReport;
begin
Result := DesignerPanel.GetReport;
end;

function TprUserObjectPropsForm.GetDesignerPanel : TprCustomDesignerPanel;
begin
Result := Owner as TprCustomDesignerPanel;
end;

procedure TprUserObjectPropsForm.UpdateDesigner;
var
  i : integer;
begin
i := 0;
while (i<DesignerPanel.SelCount) and
      not (DesignerPanel.SelObjs[i] is TprBand) do Inc(i);
if i<DesignerPanel.SelCount then
  DesignerPanel.UpdateCurPage; // band selected

DesignerPanel.DoApplyObjectsProps;
end;

/////////////////////////////////////////////////
//
// TprPropsForm
//
/////////////////////////////////////////////////
constructor TprPropsForm.CreatePropsForm(AOwner : TComponent; _DesignerPanel : TprCustomDesignerPanel);
begin
FDesignerPanel := _DesignerPanel;
inherited Create(AOwner);
end;

procedure TprPropsForm.SetFocusOnFirstControl;
var
  c : TWinControl;
begin
SetFocus;
c := FindNextControl(nil,true,true,false);
if c<>nil then
  c.SetFocus;
end;

procedure TprPropsForm.CreateParams(var Params : TCreateParams);
begin
inherited;
Params.Style := Params.Style and (not WS_BORDER) and (not WS_CAPTION) and (not WS_DISABLED);
Params.Style := Params.Style or WS_CHILD or WS_VISIBLE;
Params.ExStyle := Params.ExStyle or WS_EX_CONTROLPARENT;
Params.WndParent := TWinControl(Owner).Handle;
end;

procedure TprPropsForm.Loaded;
begin
  inherited;
  FOldOnClose := Self.OnClose;
  Self.OnClose := OnCloseEvent; 
end;

procedure TprPropsForm.OnCloseEvent(Sender: TObject; var Action : TCloseAction);
begin
  Action := caFree;
  if Assigned(FOldOnClose) then
    FOldOnClose(Sender, Action);
end;

procedure TprPropsForm.UpdateInfo;
begin
end;

procedure TprPropsForm.Apply;
begin
DesignerPanel.UpdateSelectedObjects;
DesignerPanel.DsgnNotifyReport(true);
end;

procedure TprPropsForm.Cancel;
begin
end;

procedure TprPropsForm.prRestoreProperties(Ini : TIniFile; sn : string);
begin
end;

procedure TprPropsForm.prSaveProperties(Ini : TIniFile; sn : string);
begin
end;

/////////////////////////////////////////////////
//
// TprObjPropsForm
//
/////////////////////////////////////////////////
procedure TprObjPropsForm.AfterConstruction;
begin
FVersionsEditor := TprObjVersionsEditor.Create(Self);
FVersionsEditor.Parent := Self;
FVersionsEditor.Align := alTop;
FVersionsEditor.OnVersionChanged := OnVersionChanged;
inherited;
end;

procedure TprObjPropsForm.UpdateInfo;
begin
if DesignerPanel.SelCount>1 then
  begin
    FVersionsEditor.Obj := nil;
    _CopyMultiplyPropertiesToControls;
    SetEnabledAfterCopyToControls;
  end
else
  begin
    FVersionsEditor.Obj := TprObj(DesignerPanel.SelObjs[0]);
    CopySinglePropertiesToControls(FVersionsEditor.CurrentVersion);
    SetEnabledAfterCopyToControls;
  end;
end;

procedure TprObjPropsForm.OnVersionChanged;
begin
if OldVersion<>nil then
  begin
    CopySinglePropertiesFromControls(OldVersion);
  end;
if NewVersion<>nil then
  begin
    CopySinglePropertiesToControls(NewVersion);
  end;
end;

procedure TprObjPropsForm._CopyMultiplyPropertiesToControls;
var
  L : TList;
begin
L := TList.Create;
try
  MakedRecDefVersionList(DesignerPanel.SelObjsList,L);
  CopyMultiplyPropertiesToControls(L);
finally
  L.Free;
end;
end;

procedure TprObjPropsForm._CopyMultiplyPropertiesFromControls;
var
  L : TList;
begin
L := TList.Create;
try
  MakedRecDefVersionList(DesignerPanel.SelObjsList,L);
  CopyMultiplyPropertiesFromControls(L);
finally
  L.Free;
end;
end;

procedure TprObjPropsForm.SetEnabledAfterCopyToControls;
begin
end;

procedure TprObjPropsForm.CopySinglePropertiesFromControls;
var
  L : TList;
begin
L := TList.Create;
try
  L.Add(v);
  CopyMultiplyPropertiesFromControls(L);
finally
  L.Free;
end;
end;

procedure TprObjPropsForm.CopyMultiplyPropertiesFromControls;
begin
end;

procedure TprObjPropsForm.CopySinglePropertiesToControls;
var
  L : TList;
begin
L := TList.Create;
try
  L.Add(v);
  CopyMultiplyPropertiesToControls(L);
finally
  L.Free;
end;
end;

procedure TprObjPropsForm.CopyMultiplyPropertiesToControls;
begin
end;

procedure TprObjPropsForm.Apply;
begin
  if FVersionsEditor.CurrentVersion <> nil then
    CopySinglePropertiesFromControls(FVersionsEditor.CurrentVersion)
  else
    _CopyMultiplyPropertiesFromControls;
  inherited;
end;

procedure TprObjPropsForm.Cancel;
begin
  if FVersionsEditor.CurrentVersion <> nil then
    CopySinglePropertiesToControls(FVersionsEditor.CurrentVersion)
  else
    _CopyMultiplyPropertiesToControls;
end;

procedure TprObjPropsForm.DefaultCopyToControls;
begin
end;

procedure TprObjPropsForm.DefaultCopyFromControls;
begin
end;

/////////////////////////////////////////////////
//
// TprDesignerPanelForm
//
/////////////////////////////////////////////////
function TprDesignerPanelForm.GetParentControl : TControl;
begin
Result := FDesignerPanel;
end;

/////////////////////////////////////////////////
//
// TprCustomObjectsPropsForm
//
/////////////////////////////////////////////////
procedure TprCustomObjectsPropsForm.DoApplyObjectsProps;
begin
DesignerPanel.DoApplyObjectsProps;
end;

function TprCustomObjectsPropsForm.GetFormEditorClassForClass(const ClassName : string) : TprPropsFormClass;
var
  i : integer;
begin
Result := nil;
i := 0;
while (i<Length(prBandRegInfos)) and
      (not(DesignerPanel.GetReport is prBandRegInfos[i].ReportRef) or
       (AnsiCompareText(ClassName,prBandRegInfos[i].ClassRef.ClassName)<>0)) do Inc(i);

if i<Length(prBandRegInfos) then
  Result := TprPropsFormClass(GetClass(prBandRegInfos[i].PropsFormClassName))
else
  begin
    i := 0;
    while (i<Length(prObjRegInfos)) and
          (not(DesignerPanel.GetReport is prObjRegInfos[i].ReportRef) or
           (AnsiCompareText(ClassName,prObjRegInfos[i].ClassRef.ClassName)<>0)) do Inc(i);
    if i<Length(prObjRegInfos) then
      Result := TprPropsFormClass(GetClass(prObjRegInfos[i].PropsFormClassName));
  end;
end;

/////////////////////////////////////////////////
//
// TprCustomPosSizeForm
//
/////////////////////////////////////////////////
procedure TprCustomPosSizeForm.UpdateInfo;
var
  i : integer;
  j : TprObjectPosSizeProps;

  function GetPropValue(dc : TprDesignComponent; p : TprObjectPosSizeProps) : integer;
  begin
  Result := -1;
  if dc is TprObj then
    with TprObj(dc) do
      case p of
        prpsaLeft : Result := Band.dPageRect.Left+dRec.Left;
        prpsaRight : Result := Band.dPageRect.Left+dRec.Right;
        prpsaTop : Result := Band.dPageRect.Top+dRec.Top;
        prpsaBottom : Result := Band.dPageRect.Top+dRec.Bottom;
        prpsaWidth : Result := dRec.Right-dRec.Left;
        prpsaHeight : Result := dRec.Bottom-dRec.Top;
      end
  else
    if dc is TprBand then
      with TprBand(dc) do
        case p of
          prpsaLeft : Result := dPageRect.Left;
          prpsaRight : Result := dPageRect.Right;
          prpsaTop : Result := dPageRect.Top;
          prpsaBottom : Result := dPageRect.Bottom;
          prpsaWidth : Result := dPageRect.Right-dPageRect.Left;
          prpsaHeight : Result := dPageRect.Bottom-dPageRect.Top;
        end;
  end;
  
begin
if DesignerPanel.SelCount>0 then
  begin
    EnabledProps := [prpsaLeft,prpsaRight,prpsaTop,prpsaBottom,prpsaWidth,prpsaHeight];
    EqProps := [prpsaLeft,prpsaRight,prpsaTop,prpsaBottom,prpsaWidth,prpsaHeight];

    for j:=Low(TprObjectPosSizeProps) to High(TprObjectPosSizeProps) do
      SizePosArray[j] := GetPropValue(DesignerPanel.SelObjs[0],j);

    for i:=0 to DesignerPanel.SelCount-1 do
      begin
        if DesignerPanel.SelObjs[i] is TprCustomHBand then
          EnabledProps := EnabledProps-[prpsaLeft,prpsaRight,prpsaTop,prpsaBottom,prpsaWidth]
        else
          if DesignerPanel.SelObjs[i] is TprCustomVBand then
            EnabledProps := EnabledProps-[prpsaLeft,prpsaRight,prpsaTop,prpsaBottom,prpsaHeight];

        if i>0 then
          for j:=Low(TprObjectPosSizeProps) to High(TprObjectPosSizeProps) do
            if not (j in EnabledProps) or (SizePosArray[j]<>GetPropValue(DesignerPanel.SelObjs[i],j)) then
              EqProps := EqProps-[j];
      end;
  end
else
  begin
    EnabledProps := [];
    EqProps := [];
    for j:=Low(TprObjectPosSizeProps) to High(TprObjectPosSizeProps) do
      SizePosArray[j] := -1;
  end;
end;

/////////////////////////////////////////////////
//
// TprCustomDesignerRuler
//
/////////////////////////////////////////////////
function TprCustomDesignerRuler.DesignerPanel : TprCustomDesignerPanel;
begin
Result := TprCustomDesignerPanel(Parent.Parent);
end;

/////////////////////////////////////////////////
//
// TprObjectLinksButton
//
/////////////////////////////////////////////////
constructor TprObjectLinksButton.Create(AOwner : TComponent);
begin
inherited;
Width := LinkButtonWidth;
Height := LinkButtonHeight;
TabStop := false;
end;

procedure TprObjectLinksButton.CreateParams(var Params: TCreateParams);
begin
inherited;
CreateSubClass(Params, 'BUTTON');
Params.Style := Params.Style or BS_PUSHBUTTON or BS_OWNERDRAW;
end;

procedure TprObjectLinksButton.CNMeasureItem(var Msg: TWMMeasureItem);
begin
with Msg.MeasureItemStruct^ do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
Msg.Result := 1;
end;

procedure TprObjectLinksButton.CNDrawItem(var Msg: TWMDrawItem);
begin
DrawButton(Msg.DrawItemStruct^.hDC,FHighlighted);
Msg.Result := 1;
end;

procedure TprObjectLinksButton.CMMouseLeave(var Msg : TMessage);
begin
FHighlighted := false;
Repaint;
inherited;
end;

procedure TprObjectLinksButton.CMMouseEnter(var Msg : TMessage);
begin
FHighlighted := true;
Repaint;
inherited;
end;

procedure TprObjectLinksButton.DrawButton(DC : HDC; Highlighted : boolean);
const
  aFillColor : array [boolean] of TColor = (clBtnFace,clHighlight);
  aDrawColor : array [boolean] of TColor = (clBtnText,clHighlightText);
var
  r : TRect;
  nbr : HBRUSH;
  npn,opn : HPEN;
begin
nbr := CreateSolidBrush(GetRGBColor(aFillColor[Highlighted]));
npn := CreatePen(PS_SOLID,1,GetRGBColor(aDrawColor[Highlighted]));
opn := SelectObject(DC,npn);
r := Rect(0,0,ClientWidth,ClientHeight);
FrameRect(DC,r,nbr);
r.Left := r.Left+(r.Right-r.Left-5) div 2;
r.Top := r.Top+(r.Bottom-r.Top-3) div 2;
MoveToEx(DC,r.Left,r.Top,nil);
LineTo(DC,r.Left+5,r.Top);
MoveToEx(DC,r.Left+1,r.Top+1,nil);
LineTo(DC,r.Left+4,r.Top+1);
MoveToEx(DC,r.Left+2,r.Top+2,nil);
LineTo(DC,r.Left+3,r.Top+2);
SelectObject(DC,opn);
DeleteObject(nbr);
DeleteObject(npn);
end;

/////////////////////////////////////////////////
//
// TprBandCaptions
//
/////////////////////////////////////////////////
constructor TprBandCaptions.Create(AOwner : TComponent);
begin
inherited;
TabStop := false;
BevelOuter := bvNone;
FPopupMenu := TPopupMenu.Create(nil);
FHintForm := TprBandHintForm.Create(nil);
end;

destructor TprBandCaptions.Destroy;
begin
FPopupMenu.Free;
FHintForm.Free;
inherited;
end;

function TprBandCaptions.DsgnR(dc : TprDesignComponent) : TRect;
begin
Result := DesignerPanel.DsgnR(dc);
end;

procedure TprBandCaptions.OnDefineLink(Sender : TObject);
begin
DesignerPanel.MakeLinkFromObject(FPopupMenuBand,GetLinkMode);
end;

procedure TprBandCaptions.OnDeleteLink(Sender : TObject);
begin
DesignerPanel.DeleteBandLink(FPopupMenuBand,FPopupMenuBand.ResizeObjs[TMenuItem(Sender).Tag]);
end;

procedure TprBandCaptions.OnShowBandProps(Sender : TObject);
begin
DesignerPanel.SelectObject(FPopupMenuBand);
DesignerPanel.VisibleObjectsPropsForm := true;
end;

function TprBandCaptions.GetSize : integer;
begin
Result := Max(22,GetFontSize(sArialFont,8,[]).cy+6); // 22 - size of band icon +6
end;

function TprBandCaptions.DesignerPanel : TprCustomDesignerPanel;
begin
Result := TprCustomDesignerPanel(Parent.Parent);
end;

procedure TprBandCaptions.SetMouseCursorState(b : TprBand; PointInfo : TprBandsCaptionsPointInfo; X,Y : integer);
var
  nc : TCursor;
begin
nc := crArrow;
case PointInfo of
  bcpiResizeLeft,bcpiResizeRight: nc := crSizeWE;
  bcpiResizeTop,bcpiResizeBottom: nc := crSizeNS;
end;
if Cursor<>nc then
  Cursor := nc;
end;

procedure TprBandCaptions.HighlightBand(Band : TprBand);
var
  BandRect : TRect;
  IsVertical : boolean;
begin
if Band=FHighlightedBand then exit;
FHighlightedBand := Band;
Repaint;
if FHighlightedBand=nil then
  TprBandHintForm(FHintForm).HideHint
else
  begin
    GetHintInfo(BandRect,IsVertical);
    TprBandHintForm(FHintForm).ShowHint(BandRect,FHighlightedBand,IsVertical);
  end;
end;

procedure TprBandCaptions.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  b : TprBand;
  PointInfo : TprBandsCaptionsPointInfo;
begin
GetPointInfoAt(x,y,b,PointInfo);
SetMouseCursorState(b,PointInfo,X,Y);
case FDownPointInfo of
  bcpiNone:
    begin
      HighlightBand(b);
    end;
  bcpiResizeTop,bcpiResizeBottom:
    begin
      if FLastY<>Y then
        begin
          DrawAnime(FLastX,FLastY);
          FLastX := X;
          FLastY := Y;
          DrawAnime(FLastX,FLastY);
        end;
    end;
  bcpiResizeLeft,bcpiResizeRight:
    begin
      if FLastX<>X then
        begin
          DrawAnime(FLastX,FLastY);
          FLastX := X;
          FLastY := Y;
          DrawAnime(FLastX,FLastY);
        end;
    end;
end;
end;

procedure TprBandCaptions.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if DesignerPanel.CurPage=nil then exit;

FLastX := X;
FLastY := Y;
FStartX := X;
FStartY := Y;
if FDblClick then
  begin
    FDblClick := false;
    exit;
  end;

GetPointInfoAt(x,y,FDownBand,FDownPointInfo);
if FDownBand<>nil then
  case FDownPointInfo of
    bcpiLinkButton:
      begin
        // show popup menu
        HighlightBand(nil);
        FPopupMenuBand := FDownBand;
        ShowPopupMenu;
        FDownPointInfo := bcpiNone;
      end;
    bcpiInside:
      begin
        if ssShift in Shift then
          DesignerPanel.AddSelectObject(FDownBand)
        else
          DesignerPanel.SelectObject(FDownBand);
      end;
    bcpiResizeTop,bcpiResizeBottom,bcpiResizeLeft,bcpiResizeRight:
      begin
        DrawAnime(FStartX,FStartY);
      end;
  end;
end;

procedure TprBandCaptions.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  r : TRect;
  oLeft,oTop,oRight,oBottom : integer;
begin
if DesignerPanel.CurPage=nil then exit;
oLeft := 0;
oTop := 0;
oRight := 0;
oBottom := 0; 
case FDownPointInfo of
  bcpiResizeTop: oTop := Y-FStartY;
  bcpiResizeBottom: oBottom := Y-FStartY;
  bcpiResizeLeft: oLeft := X-FStartX;
  bcpiResizeRight: oRight := X-FStartX;
end;
r := DsgnR(FDownBand);
if ((oLeft<>0) or (oTop<>0) or (oRight<>0) or (oBottom<>0)) and
   (r.Left+oLeft<r.Right+oRight) and (r.Top+oTop<r.Bottom+oBottom) then
  begin
    DesignerPanel.FDesignerBox.DoResize(FDownBand,oTop,oLeft,oBottom,oRight,nil);
    DesignerPanel.FDesignerBox.Repaint;
  end
else
  DrawAnime(x,y);
FDownPointInfo := bcpiNone;
end;

procedure TprBandCaptions.DblClick;
begin
FDblClick := true;
end;

procedure TprBandCaptions.CMMouseLeave(var Msg : TMessage);
begin
inherited;
HighlightBand(nil);
end;

procedure TprBandCaptions.ShowPopupMenu;
var
  i : integer;
  m : TMenuItem;
begin
ClearPopupMenu(FPopupMenu);
FPopupMenuBand.DsgnDefinePopupMenu(FPopupMenu,true);
if FPopupMenu.Items.Count>0 then
  AddPopupMenuItem(FPopupMenu,nil,0,'',nil,'',0,true,false);
AddPopupMenuItem(FPopupMenu,nil,sBandsCaptionsDefineLink,'',OnDefineLink,'',1,true,false);
if FPopupMenuBand.ResizeObjs.Count>0 then
  begin
    m := AddPopupMenuItem(FPopupMenu,nil,sLinksControlDeleteLink,'',nil,'',0,true,false);
    for i:=0 to FPopupMenuBand.ResizeObjs.Count-1 do
      AddPopupMenuItemS(FPopupMenu,m,FPopupMenuBand.ResizeObjs[i].GetDesc,FPopupMenuBand.ResizeObjs[i].ClassName,OnDeleteLink,'',i,true,false);
  end;
AddPopupMenuItem(FPopupMenu,nil,0,'',nil,'',0,true,false);
AddPopupMenuItem(FPopupMenu,nil,sProperties,'PROPERTIES',OnShowBandProps,'',2,true,false);
end;

/////////////////////////////////////////////////
//
// TprHorBandCaptions
//
/////////////////////////////////////////////////
constructor TprHorBandCaptions.Create(AOwner : TComponent);
begin
inherited;
Width := GetSize;
end;

procedure TprHorBandCaptions.Paint;
var
  s : string;
  p : TprCustomPage;
  i,h,j : integer;
  r,r2 : TRect;
  newfont,oldfont : HFONT;
  BandSelected,BandHighlighted : boolean;
begin
Canvas.Brush.Color := clBtnFace;
Canvas.FillRect(Rect(0,0,ClientWidth,ClientHeight));
p := DesignerPanel.CurPage;
if p=nil then exit;
for i:=0 to p.Bands.Count-1 do
  if p.Bands[i].BandType in HorizontalBands then
    begin
      BandSelected := DesignerPanel.IsObjSelected(p.Bands[i]);
      BandHighlighted := FHighlightedBand=p.Bands[i];

      r.Left := 2;
      r.Top := DsgnR(p.Bands[i]).Top;
      r.Right := ClientWidth;
      r.Bottom := DsgnR(p.Bands[i]).Bottom;

      Canvas.Font.Name := sArialFont;
      Canvas.Font.Size := 8;
      if BandHighlighted then
        begin
          Canvas.Brush.Color := clHighlight;
          Canvas.Font.Color := clHighlightText;
          Canvas.FillRect(r);
        end
      else
        begin
          Canvas.Brush.Color := clBtnFace;
          Canvas.Font.Color := clBtnText;
        end;
      j := 0;
      while (j<p.Bands[i].Objects.Count) and (not DesignerPanel.IsObjSelected(p.Bands[i].Objects[j])) do Inc(j);
      if j<p.Bands[i].Objects.Count then
        Canvas.Font.Style := [fsBold]
      else
        Canvas.Font.Style := [];

      if r.Bottom-r.Top>=LinkButtonWidth then
        begin
          r2 := Rect(r.Left,r.Top,r.Right,r.Top+LinkButtonWidth);
          if BandHighlighted then
            begin
              DrawEdge(Canvas.Handle,r2,aEdgeBorder[BandSelected],BF_LEFT or BF_TOP or BF_BOTTOM);
              r.Top := r.Top+LinkButtonWidth;
              DrawEdge(Canvas.Handle,r,aEdgeBorder[BandSelected],BF_LEFT or BF_TOP or BF_BOTTOM);
            end
          else
            begin
              DrawEdge(Canvas.Handle,r,aEdgeBorder[BandSelected],BF_LEFT or BF_TOP or BF_BOTTOM);
              r.Top := r.Top+LinkButtonWidth;
            end;
          Canvas.Pen.Color := Canvas.Font.Color;
          r2.Left := r2.Left+(r2.Right-r2.Left-3) div 2;
          r2.Top := r2.Top+(r2.Bottom-r2.Top-5) div 2;
          Canvas.MoveTo(r2.Left,r2.Top);
          Canvas.LineTo(r2.Left,r2.Top+5);
          Canvas.MoveTo(r2.Left+1,r2.Top+1);
          Canvas.LineTo(r2.Left+1,r2.Top+4);
          Canvas.MoveTo(r2.Left+2,r2.Top+2);
          Canvas.LineTo(r2.Left+2,r2.Top+3);
        end
      else
        DrawEdge(Canvas.Handle,r,aEdgeBorder[BandSelected],BF_LEFT or BF_TOP or BF_RIGHT);

      // r - rectangle of band caption
      r.Left := r.Left+2;
      r.Top := r.Top+2;
      r.Bottom := r.Bottom-2;
      //
      if r.Bottom-r.Top>20 then
        begin
          // draw band icon only if it will be placed
          Canvas.Draw(r.Left+(r.Right-r.Left-16) div 2,r.Bottom-18,BandImages[p.Bands[i].BandType]);
          r.Bottom := r.Bottom-22;
        end;
      // draw text caption
      // prepare string using function DrawText
      newfont := Create90Font(Canvas.Font);
      oldfont := SelectObject(Canvas.Handle,newfont);
      r2.Left := 0;
      r2.Right := r.Bottom-r.Top;
      r2.Top := 0;
      r2.Bottom := ClientWidth-1;
      s := p.Bands[i].GetDrawDesignCaption;
      UniqueString(s);
      h := DrawText(Canvas.Handle,PChar(s),Length(s),r2,DT_END_ELLIPSIS or DT_SINGLELINE or DT_MODIFYSTRING or DT_CALCRECT);
      // draw s
      ExtTextOut(Canvas.Handle,r.Left+(r.Right-r.Left-h) div 2,r.Bottom,ETO_CLIPPED,@r,PChar(s),strlen(PChar(s)),nil);
      SelectObject(Canvas.Handle,oldfont);
      DeleteObject(newfont);
    end;
end;

procedure TprHorBandCaptions.GetPointInfoAt(x,y : integer; var b : TprBand; var PointInfo : TprBandsCaptionsPointInfo);
var
  p : TprCustomPage;
  r : TRect;
  i : integer;
begin
b := nil;
PointInfo := bcpiNone;
p := DesignerPanel.CurPage;
if p=nil then exit;
for i:=0 to p.Bands.Count-1 do
  if p.Bands[i].BandType in HorizontalBands then
    begin
      r := DsgnR(p.Bands[i]);
      if (x>=2) and (x<=Width) then
        begin
          if (y>=r.Bottom-(SelectPointSize div 2)) and (y<=r.Bottom+(SelectPointSize div 2)) then
            begin
              b := p.Bands[i];
              PointInfo := bcpiResizeBottom;
              break;
            end
          else
            if (y>=r.Top-(SelectPointSize div 2)) and (y<=r.Top+(SelectPointSize div 2)) then
              begin
                b := p.Bands[i];
                PointInfo := bcpiResizeTop;
              end
            else
              if (y>=r.Top) and (y<=r.Top+LinkButtonWidth) then
                begin
                  b := p.Bands[i];
                  PointInfo := bcpiLinkButton;
                  break;
                end
              else
                if (y>=r.Top) and (y<=r.Bottom) then
                  begin
                    b := p.Bands[i];
                    PointInfo := bcpiInside;
                    break;
                  end;
        end;
    end;
end;

procedure TprHorBandCaptions.DrawAnime(x,y : integer);
var
  r : TRect;
begin
case FDownPointInfo of
  bcpiResizeTop:
    begin
      r := DsgnR(FDownBand);
      r.Top := r.Top+y-FStartY;
      DesignerPanel.FDesignerBox.Canvas.DrawFocusRect(r);
    end;
  bcpiResizeBottom:
    begin
      r := DsgnR(FDownBand);
      r.Bottom := r.bottom+y-FStartY;
      DesignerPanel.FDesignerBox.Canvas.DrawFocusRect(r);
    end;
end;
end;

procedure TprHorBandCaptions.ShowPopupMenu;
var
  r : TRect;
  p : TPoint;
begin
inherited;
r := DsgnR(FPopupMenuBand);
p := ClientToScreen(Point(0,0));
FPopupMenu.Popup(p.x+Width,p.y+r.Top);
end;

function TprHorBandCaptions.GetLinkMode : TprLinkType;
begin
Result := ltBottom;
end;

procedure TprHorBandCaptions.GetHintInfo(var BandRect : TRect; var IsVertical : boolean);
begin
IsVertical := true;
BandRect := DsgnR(FHighlightedBand);
BandRect.Left := 0;
BandRect.Top := Max(0,BandRect.Top);
BandRect.Right := ClientWidth;
BandRect.TopLeft := ClientToScreen(BandRect.TopLeft);
BandRect.BottomRight := ClientToScreen(BandRect.BottomRight);
end;

/////////////////////////////////////////////////
//
// TprVerBandCaptions
//
/////////////////////////////////////////////////
constructor TprVerBandCaptions.Create(AOwner : TComponent);
begin
inherited;
Height := GetSize;
end;

procedure TprVerBandCaptions.Paint;
var
  p : TprCustomPage;
  i,j : integer;
  r,r2 : TRect;
  newfont,oldfont : HFONT;
  BandSelected,BandHighlighted : boolean;
begin
Canvas.Brush.Color := clBtnFace;
Canvas.FillRect(Rect(0,0,ClientWidth,ClientHeight)); 
p := DesignerPanel.CurPage;
if p=nil then exit;
for i:=0 to p.Bands.Count-1 do
  if p.Bands[i].BandType in VerticalBands then
    begin
      BandSelected := DesignerPanel.IsObjSelected(p.Bands[i]);
      BandHighlighted := FHighlightedBand=p.Bands[i];
      //
      r.Left := DsgnR(p.Bands[i]).Left;
      r.Top := 2;
      r.Right := DsgnR(p.Bands[i]).Right;
      r.Bottom := ClientHeight;
      // r - rectangle of band 

      Canvas.Font.Name := sArialFont;
      Canvas.Font.Size := 8;
      if BandHighlighted then
        begin
          Canvas.Brush.Color := clHighlight;
          Canvas.Font.Color := clHighlightText;
          Canvas.FillRect(r);
        end
      else
        begin
          Canvas.Brush.Color := clBtnFace;
          Canvas.Font.Color := clBtnText;
        end;
      j := 0;
      while (j<p.Bands[i].Objects.Count) and (not DesignerPanel.IsObjSelected(p.Bands[i].Objects[j])) do Inc(j);
      if j<p.Bands[i].Objects.Count then
        Canvas.Font.Style := [fsBold]
      else
        Canvas.Font.Style := [];

      if r.Right-r.Left>=LinkButtonWidth then
        begin
          r2 := Rect(r.Right-LinkButtonWidth,r.Top,r.Right,r.Bottom);
          if BandHighlighted then
            begin
              DrawEdge(Canvas.Handle,r2,aEdgeBorder[BandSelected],BF_LEFT or BF_TOP or BF_RIGHT);
              r.Right := r.Right-LinkButtonWidth;
              DrawEdge(Canvas.Handle,r,aEdgeBorder[BandSelected],BF_LEFT or BF_TOP or BF_RIGHT);
            end
          else
            begin
              DrawEdge(Canvas.Handle,r,aEdgeBorder[BandSelected],BF_LEFT or BF_TOP or BF_RIGHT);
              r.Right := r.Right-LinkButtonWidth;
            end;
          Canvas.Pen.Color := Canvas.Font.Color;
          r2.Left := r2.Left+(r2.Right-r2.Left-5) div 2;
          r2.Top := r2.Top+(r2.Bottom-r2.Top-3) div 2;
          Canvas.MoveTo(r2.Left,r2.Top);
          Canvas.LineTo(r2.Left+5,r2.Top);
          Canvas.MoveTo(r2.Left+1,r2.Top+1);
          Canvas.LineTo(r2.Left+4,r2.Top+1);
          Canvas.MoveTo(r2.Left+2,r2.Top+2);
          Canvas.LineTo(r2.Left+3,r2.Top+2);
        end
      else
        DrawEdge(Canvas.Handle,r,aEdgeBorder[BandSelected],BF_LEFT or BF_TOP or BF_RIGHT);

      // r - rectangle of band caption
      r.Left := r.Left+2;
      r.Top := r.Top+2;
      r.Right := r.Right-2;
      //
      if r.Right-r.Left>20 then
        begin
          // draw band icon only if it will be placed
          Canvas.Draw(r.Left+2,r.Top+(r.Bottom-r.Top-16) div 2,BandImages[p.Bands[i].BandType]);
          r.Left := r.Left+22;
        end;
      newfont := CreateApiFont(Canvas.Font);
      oldfont := SelectObject(Canvas.Handle,newfont);
      DrawText(Canvas.Handle,PChar(p.Bands[i].GetDrawDesignCaption),Length(p.Bands[i].GetDrawDesignCaption),r,DT_END_ELLIPSIS or DT_SINGLELINE or DT_VCENTER or DT_LEFT);
      SelectObject(Canvas.Handle,oldfont);
      DeleteObject(newfont);
    end;    
end;

procedure TprVerBandCaptions.GetPointInfoAt(x,y : integer; var b : TprBand; var PointInfo : TprBandsCaptionsPointInfo);
var
  p : TprCustomPage;
  r : TRect;
  i : integer;
begin
b := nil;
PointInfo := bcpiNone;
p := DesignerPanel.CurPage;
if p=nil then exit;
for i:=0 to p.Bands.Count-1 do
  if p.Bands[i].BandType in VerticalBands then
    begin
      r := DsgnR(p.Bands[i]);
      if (y>=2) and (y<=Height) then
        begin
          if (x>=r.Right-(SelectPointSize div 2)) and (x<=r.Right+(SelectPointSize div 2)) then
            begin
              b := p.Bands[i];
              PointInfo := bcpiResizeRight;
              break;
            end
          else
            if (x>=r.Left-(SelectPointSize div 2)) and (x<=r.Left+(SelectPointSize div 2)) then
              begin
                b := p.Bands[i];
                PointInfo := bcpiResizeLeft;
              end
            else
              if (x<=r.Right) and (x>=r.Right-LinkButtonWidth) then
                begin
                  b := p.Bands[i];
                  PointInfo := bcpiLinkButton;
                  break;
                end
              else
                if (x>=r.Left) and (x<=r.Right) then
                  begin
                    b := p.Bands[i];
                    PointInfo := bcpiInside;
                    break;
                  end;
        end;
    end;
end;

procedure TprVerBandCaptions.DrawAnime(x,y : integer);
var
  r : TRect;
begin
case FDownPointInfo of
  bcpiResizeLeft:
    begin
      r := DsgnR(FDownBand);
      r.Left := r.Left+x-FStartX;
      DesignerPanel.FDesignerBox.Canvas.DrawFocusRect(r);
    end;
  bcpiResizeRight:
    begin
      r := DsgnR(FDownBand);
      r.Right := r.right+x-FStartX;
      DesignerPanel.FDesignerBox.Canvas.DrawFocusRect(r);
    end;
end;
end;

procedure TprVerBandCaptions.ShowPopupMenu;
var
  r : TRect;
  p : TPoint;
begin
inherited;
r := DsgnR(FPopupMenuBand);
p := ClientToScreen(Point(0,0));
FPopupMenu.Popup(p.x+r.Right-LinkButtonWidth,p.y+Height);
end;

function TprVerBandCaptions.GetLinkMode : TprLinkType;
begin
Result := ltRight;
end;

procedure TprVerBandCaptions.GetHintInfo(var BandRect : TRect; var IsVertical : boolean);
begin
IsVertical := false;
BandRect := DsgnR(FHighlightedBand);
BandRect.Left := Max(0,BandRect.Left);
BandRect.Top := 0;
BandRect.Bottom := ClientHeight;
BandRect.TopLeft := ClientToScreen(BandRect.TopLeft);
BandRect.BottomRight := ClientToScreen(BandRect.BottomRight);
end;

/////////////////////////////////////////////////
//
// TprCustomDesignerBox
//
/////////////////////////////////////////////////
constructor TprCustomDesignerBox.Create(AOwner : TComponent);
begin
inherited;
FCanvas := TControlCanvas.Create;
TControlCanvas(FCanvas).Control := Self;
HorzScrollBar.Tracking := true;
VertScrollBar.Tracking := true;
TabStop := true;
BevelOuter := bvNone;
FLinksControlPopupMenu := TPopupMenu.Create(nil);
FSelectionDrawObject := TprSelectionDrawObject.Create;
FSelectionDrawObject.OnGetCountRects := OnGetCountRects;
FSelectionDrawObject.OnGetRect := OnGetRect;
FSelectionDrawObject.OnGetAllowResizeTypes := OnGetAllowResizeTypes;
end;

destructor TprCustomDesignerBox.Destroy;
begin
FLinksControlPopupMenu.Free;
FSelectionDrawObject.Free;
FCanvas.Free;
inherited;
end;

function TprCustomDesignerBox.DsgnR(dc : TprDesignComponent) : TRect;
begin
Result := DesignerPanel.DsgnR(dc);
end;

function TprCustomDesignerBox.DsgnRSel(i : integer) : TRect;
begin
Result := DesignerPanel.DsgnRSel(i);
end;

function TprCustomDesignerBox.GetSelCount : integer;
begin
Result := DesignerPanel.SelCount;
end;

function TprCustomDesignerBox.GetSelObj(i : integer) : TprDesignComponent;
begin
Result := DesignerPanel.SelObjs[i];
end;

procedure TprCustomDesignerBox.OnGetCountRects(Sender : TObject; var Count : integer);
begin
Count := DesignerPanel.SelCount;
end;

procedure TprCustomDesignerBox.OnGetRect(Sender : TObject; index : integer; var Rect : TRect);
begin
Rect := DsgnRSel(index);
end;

procedure TprCustomDesignerBox.OnGetAllowResizeTypes(Sender : TObject; index : integer; var AllowResizeTypes : TprResizeTypeSet);
begin
AllowResizeTypes := DesignerPanel.SelObjs[index].DsgnAllowResizeTypes;
end;

procedure TprCustomDesignerBox.WMGetDlgCode(var Msg : TWMGetDlgCode);
begin
inherited;
Msg.Result := Msg.Result or DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;

procedure TprCustomDesignerBox.InplaceEdit(EditedObject : TprObj);
begin
SetLinksControl(nil);
FInplaceEditedObj := EditedObject;
EditedObject.InplaceEdit(Self,FInplaceEditor,DsgnR(EditedObject),GetExData);
THackWinControl(FInplaceEditor).OnKeyDown := OnInplaceEditorKeyDown;
end;

procedure TprcustomDesignerBox.OnInplaceEditorKeyDown(Sender: TObject; var Key : Word; Shift : TShiftState);
begin
if (Shift=[]) and (Key=VK_ESCAPE) then
  begin
    DesignerPanel.EndInplaceEdit;
    Key := 0;
    exit;
  end;
if (Shift=[ssCtrl]) and (Key=VK_RETURN) then
  begin
    DesignerPanel.SaveInplaceEdit;
    Key := 0;
    exit;
  end;
end;

procedure TprCustomDesignerBox.WMSysKeyChar(var Msg : TWMSysChar);
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

procedure TprCustomDesignerBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  o : TprObj;
  p : TprCustomPage;
  b,b2 : TprBand;
  prps : rPrPaintStruct;
  rCur : TRect;
  dcCur,dcNext : TprDesignComponent;
  StepX,StepY,i,j : integer;
begin
p := DesignerPanel.CurPage;
if p=nil then exit;
if FInplaceEditor<>nil then exit;

if (Shift=[]) and (Key=VK_ESCAPE) then
  begin
    if FMouseMode<>mmNone then
      begin
        DrawAnime(Canvas.Handle,FLastX,FLastY,false); // remove mouse animation
        if FLinkPreviosObject<>nil then
          begin
            InvertObject(FLinkPreviosObject);
            FLinkPreviosObject := nil;
          end;
        if ssLeft in Shift then
          ShowDrawing(Canvas.Handle);
        ReleaseCapture;
        FMouseMode := mmNone;
      end
    else
      begin
        i := 0;
        while (i<DesignerPanel.SelCount) and (not(DesignerPanel.SelObjs[i] is TprObj)) do Inc(i);
        if i<DesignerPanel.SelCount then
          begin
            // select band of selected object
            o := TprObj(DesignerPanel.SelObjs[i]);
            DesignerPanel.SelectObject(o.Band);
          end;
      end;
    Key := 0;
    exit;
  end;

if FMouseMode<>mmNone then exit;

if (Shift=[ssCtrl]) and (Key=VK_RETURN) then
  begin
    if DesignerPanel.SelObject<>nil then
      DesignerPanel.InplaceEdit(DesignerPanel.SelObject);
    Key := 0;
    exit;
  end;

if (Shift=[ssCtrl]) and (Key=byte('N')) then
  begin
    DesignerPanel.New;
    Key := 0;
    exit;
  end;

if (Shift=[ssCtrl]) and (Key=byte('O')) then
  begin
    DesignerPanel.Open;
    Key := 0;
    exit;
  end;

if (Shift=[ssCtrl]) and (Key=byte('S')) then
  begin
    DesignerPanel.Save;
    Key := 0;
    exit;
  end;

if (Shift=[ssCtrl]) and (Key in [byte('C'),VK_INSERT]) then
  begin
    DesignerPanel.Copy;
    Key := 0;
    exit;
  end;

if ((Shift=[ssCtrl]) and (Key=byte('V'))) or
   ((Shift=[ssShift]) and (Key=VK_INSERT)) then
  begin
    DesignerPanel.Paste;
    Key := 0;
    exit;
  end;

if ((Shift=[ssCtrl]) and (Key=byte('X'))) or
   ((Shift=[ssShift]) and (Key=VK_DELETE)) then
  begin
    DesignerPanel.Cut;
    Key := 0;
    exit;
  end;

if ((Shift=[ssCtrl]) and (Key=VK_DELETE)) or
   ((Shift=[]) and (Key=VK_DELETE)) then
  begin
    DesignerPanel.DeleteSelectedObjects;
    Key := 0;
    exit;
  end;

if ((Shift=[ssAlt]) and (Key=VK_RETURN)) then
  begin
    DesignerPanel.VisibleObjectsPropsForm := true;
    Key := 0;
    exit;
  end;

if ((Shift=[ssCtrl]) and (Key=byte('G'))) then
  begin
    DesignerPanel.ShowGrid := not DesignerPanel.ShowGrid;
    exit;
  end;

if ((Shift=[ssCtrl,ssAlt,ssShift]) and (Key=byte('G'))) then
  begin
    DesignerPanel.AlignAction(aacAlignToGridLeftTop);
    exit;
  end;

if ((Shift=[ssCtrl,ssAlt]) and (Key=byte('G'))) then
  begin
    DesignerPanel.AlignAction(aacAlignToGridAll);
    exit;
  end;

if ((Shift=[ssCtrl]) and (Key=byte('F'))) then
  begin
    DesignerPanel.VisibleFindForm := true;
    exit; 
  end;

if (Shift=[ssCtrl]) and (Key=VK_PRIOR) then
  begin
    DesignerPanel.PriorPage;
    exit;
  end;

if (Shift=[ssCtrl]) and (Key=VK_NEXT) then
  begin
    DesignerPanel.NextPage;
    exit;
  end;

if (Shift=[ssCtrl]) and (Key=byte('O')) then
  begin
    DesignerPanel.EditOptions;
    exit;
  end;

if (Shift=[ssCtrl]) and (Key=byte('E')) then
  begin
    DesignerPanel.EditPage(DesignerPanel.ActivePageIndex);
    exit;
  end;

if Shift=[] then
  begin
    if Key in [VK_LEFT,VK_UP,VK_DOWN,VK_RIGHT] then
      begin
        dcNext := nil;
        case DesignerPanel.SelCount of
          0:
            begin
              // Nothing is selected, we select the first section
              if p.Bands.Count>0 then
                begin
                  b := p.Bands[0];
                  for i:=1 to p.Bands.Count-1 do
                    if DsgnR(b).Top>DsgnR(p.Bands[i]).Top then
                      b := p.Bands[i];
                  dcNext := b;
                end;
            end;
          1:
            begin
              dcCur := DesignerPanel.SelObjs[0];
              rCur := DsgnR(dcCur);
              if dcCur is TprObj then
                begin
                  // object selected
                  b := TprObj(dcCur).Band;
                  o := nil;
                  case Key of
                    VK_LEFT:
                      begin
                        for i:=0 to b.Objects.Count-1 do
                          if (DsgnR(b.Objects[i]).Left<rCur.Left) and
                             (b.Objects[i]<>o) and
                             ((o=nil) or
                              (DsgnR(o).Left<DsgnR(b.Objects[i]).Left)) then
                            o := b.Objects[i];
                      end;
                    VK_RIGHT:
                      begin
                        for i:=0 to b.Objects.Count-1 do
                          if (DsgnR(b.Objects[i]).Left>rCur.Left) and
                             (b.Objects[i]<>o) and
                             ((o=nil) or
                              (DsgnR(o).Left>DsgnR(b.Objects[i]).Left)) then
                            o := b.Objects[i];
                      end;
                    VK_UP:
                      begin
                        for i:=0 to b.Objects.Count-1 do
                          if (DsgnR(b.Objects[i]).Top<rCur.Top) and
                             (b.Objects[i]<>o) and
                             ((o=nil) or
                              (DsgnR(o).Top<DsgnR(b.Objects[i]).Top)) then
                            o := b.Objects[i];
                      end;
                    VK_DOWN:
                      begin
                        for i:=0 to b.Objects.Count-1 do
                          if (DsgnR(b.Objects[i]).Top>rCur.Top) and
                             (b.Objects[i]<>o) and
                             ((o=nil) or
                              (DsgnR(o).Top>DsgnR(b.Objects[i]).Top)) then
                            o := b.Objects[i];
                      end;
                  end;

                  if o<>nil then
                    begin
                      if Key in [VK_LEFT,VK_RIGHT] then
                        begin
                          for i:=0 to b.Objects.Count-1 do
                            if (b.Objects[i]<>o) and
                               (DsgnR(b.Objects[i]).Left=DsgnR(o).Left) and
                               (abs(DsgnR(b.Objects[i]).Top-DsgnR(dcCur).Top)<abs(DsgnR(b.Objects[i]).Top-DsgnR(o).Top)) then
                              o := b.Objects[i];
                        end
                      else
                        begin
                          for i:=0 to b.Objects.Count-1 do
                            if (b.Objects[i]<>o) and
                               (DsgnR(b.Objects[i]).Top=DsgnR(o).Top) and
                               (abs(DsgnR(b.Objects[i]).Left-DsgnR(dcCur).Left)<abs(DsgnR(b.Objects[i]).Left-DsgnR(o).Left)) then
                              o := b.Objects[i];
                        end;
                      dcNext := o
                    end
                  else
                    if b<>nil then
                      dcNext := b;
                end
              else
                if dcCur is TprBand then
                  begin
                    b := TprBand(dcCur);
                    if b.BandType in VerticalBands then
                      begin
                        case Key of
                          VK_LEFT  :
                            begin
                              // find band at left
                              b2 := nil;
                              for i:=0 to p.Bands.Count-1 do
                                if (p.Bands[i].BandType in VerticalBands) and
                                   (p.Bands[i]<>b) and
                                   (DsgnR(p.Bands[i]).Left<rCur.Left) and
                                   ((b2=nil) or
                                    (DsgnR(b2).Left<DsgnR(p.Bands[i]).Left)) then
                                  b2 := p.Bands[i];
                              if b2<>nil then
                                dcNext := b2;
                            end;
                          VK_RIGHT :
                            begin
                              // find band at right
                              b2 := nil;
                              for i:=0 to p.Bands.Count-1 do
                                if (p.Bands[i].BandType in VerticalBands) and
                                   (p.Bands[i]<>b) and
                                   (DsgnR(p.Bands[i]).Left>rCur.Left) and
                                   ((b2=nil) or
                                    (DsgnR(b2).Left>DsgnR(p.Bands[i]).Left)) then
                                  b2 := p.Bands[i];
                              if b2<>nil then
                                dcNext := b2;
                            end;
                          VK_UP,VK_DOWN:
                            begin
                              o := nil;
                              // find top object
                              for i:=0 to p.Bands.Count-1 do
                                for j:=0 to p.Bands[i].Objects.Count-1 do
                                  if RectInRect(DsgnR(p.Bands[i].Objects[j]),DsgnR(b)) then
                                    begin
                                      if (o=nil) or (DsgnR(o).Top>DsgnR(p.Bands[i].Objects[j]).Top) then
                                        o := p.Bands[i].Objects[j];
                                    end;
                              if o<>nil then
                                dcNext := o;
                            end;
                        end;
                      end
                    else
                      if b.BandType in HorizontalBands then
                        begin
                          case Key of
                            VK_UP  :
                              begin
                                b2 := nil;
                                for i:=0 to p.Bands.Count-1 do
                                  if (p.Bands[i].BandType in HorizontalBands) and
                                     (p.Bands[i]<>b) and
                                     (DsgnR(p.Bands[i]).Top<rCur.Top) and
                                     ((b2=nil) or
                                      (DsgnR(b2).Top<DsgnR(p.Bands[i]).Top)) then
                                    b2 := p.Bands[i];
                                if b2<>nil then
                                  dcNext := b2;
                              end;
                            VK_DOWN :
                              begin
                                b2 := nil;
                                for i:=0 to p.Bands.Count-1 do
                                  if (p.Bands[i].BandType in HorizontalBands) and
                                     (p.Bands[i]<>b) and
                                     (DsgnR(p.Bands[i]).Top>rCur.Top) and
                                     ((b2=nil) or
                                      (DsgnR(b2).Top>DsgnR(p.Bands[i]).Top)) then
                                    b2 := p.Bands[i];
                                if b2<>nil then
                                  dcNext := b2;
                              end;
                            VK_LEFT,VK_RIGHT:
                              begin
                                if b.Objects.Count>0 then
                                  begin
                                    o := b.Objects[0];
                                    for i:=1 to b.Objects.Count-1 do
                                      if DsgnR(o).Left>DsgnR(b.Objects[i]).Left then
                                        o := b.Objects[i];
                                    dcNext := o;
                                  end;
                              end;
                          end;
                        end;
                  end;
            end;
          else
            begin
              dcNext := DesignerPanel.SelObjs[0];
            end;
        end;

        //
        if dcNext<>nil then
          begin
            // It is necessary will be convinced that the given region
            // completely will be is visible
            MakeObjectVisible(dcNext);
            DesignerPanel.SelectObject(dcNext);
          end;
        Key := 0;
      end;
  end
else
  begin
    if DesignerPanel.UseGrid then
      begin
        StepY := DesignerPanel.GridSizeY;
        StepX := DesignerPanel.GridSizeX;
      end
    else
      begin
        StepY := DesignerPanel.ConvertYToDesignerCoords(1);
        StepX := DesignerPanel.ConvertXToDesignerCoords(1);
      end;

    if (Shift=[ssShift]) and (Key in [VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN]) then
      begin
        _BeginPaint(prps);
        for i:=0 to DesignerPanel.SelCount-1 do
          case Key of
            VK_LEFT:
              if ([ppLeftTop,ppLeft,ppLeftBottom]*DesignerPanel.SelObjs[i].DsgnAllowResizeTypes)<>[] then
                DoResize(DesignerPanel.SelObjs[i],0,0,0,-StepX,@prps);
            VK_RIGHT:
              if ([ppRightTop,ppRight,ppRightBottom]*DesignerPanel.SelObjs[i].DsgnAllowResizeTypes)<>[] then
                DoResize(DesignerPanel.SelObjs[i],0,0,0,StepX,@prps);
            VK_UP:
              if ([ppLeftTop,ppTop,ppRightTop]*DesignerPanel.SelObjs[i].DsgnAllowResizeTypes)<>[] then
                DoResize(DesignerPanel.SelObjs[i],0,0,-StepY,0,@prps);
            VK_DOWN:
              if ([ppLeftBottom,ppBottom,ppRightBottom]*DesignerPanel.SelObjs[i].DsgnAllowResizeTypes)<>[] then
                DoResize(DesignerPanel.SelObjs[i],0,0,StepY,0,@prps);
          end;
        _EndPaint(prps);
      end
    else
      if (Shift=[ssCtrl]) and (Key in [VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN]) then
        begin
          _BeginPaint(prps);
          for i:=0 to DesignerPanel.SelCount-1 do
            if DesignerPanel.SelObjs[i].DsgnAllowDrag then
              case Key of
                VK_LEFT:
                  DoDrag(DesignerPanel.SelObjs[i],-StepX,0,@prps);
                VK_RIGHT:
                  DoDrag(DesignerPanel.SelObjs[i],StepX,0,@prps);
                VK_UP:
                  DoDrag(DesignerPanel.SelObjs[i],0,-StepY,@prps);
                VK_DOWN:
                  DoDrag(DesignerPanel.SelObjs[i],0,StepY,@prps);
              end;
          _EndPaint(prps);
        end;
  end;
  
inherited;
end;

procedure TprCustomDesignerBox.MakeObjectVisible(dc : TprDesignComponent);
var
  rCur : TRect;
  rl,rt,rr,rb : integer;
begin
rl := HorzScrollBar.Position;
rr := HorzScrollBar.Position+ClientWidth;
rt := VertScrollBar.Position;
rb := VertScrollBar.Position+ClientHeight;
DesignerPanel.ConvertToDesignerCoords(DesignerPanel.ObjR(dc),rCur);
if (rCur.Left<rl) or
   (rCur.Left>rr) or
   (rCur.Right<rl) or
   (rCur.Right>rr) then
  begin
    if (rCur.Right-rCur.Left>rr-rl) or (rCur.Left<rl) then
      HorzScrollBar.Position := rCur.Left
    else
      HorzScrollBar.Position := rCur.Right-rr+rl;
  end;
if (rCur.Top<rt) or
   (rCur.Top>rb) or
   (rCur.Bottom<rt) or
   (rCur.Bottom>rb) then
  begin
    if (rCur.Bottom-rCur.Top>rb-rt) or (rCur.Top<rt) then
      VertScrollBar.Position := rCur.Top
    else
      VertScrollBar.Position := rCur.Bottom-rb+rt;
  end;
end;

function TprCustomDesignerBox.GetExData;
begin
Result := nil;
end;

function TprCustomDesignerBox.GetDesignerPanel : TprCustomDesignerPanel;
begin
Result := TprCustomDesignerPanel(Parent.Parent);
end;

procedure TprCustomDesignerBox.DrawLinks(DC : HDC);
var
  p : TprCustomPage;
  b : TprBand;
  o : TprObj;
  r1 : TRect;
  npn,opn : HPEN;
  i,j,k,pmm : integer;
begin
p := DesignerPanel.CurPage;
if p=nil then exit;
pmm := SetROP2(DC,R2_NOT);
npn := CreatePen(PS_SOLID,1,clBlack);
opn := SelectObject(DC,npn);

for i:=0 to p.Bands.Count-1 do
  begin
    b := p.Bands[i];
    r1 := DsgnR(b);
    for j:=0 to b.ResizeObjs.Count-1 do
      with DsgnR(b.ResizeObjs[j]) do
        begin
          if b.BandType in HorizontalBands then
            begin
              MoveToEx(DC,
                       Left+(Right-Left) div 2,
                       Bottom,
                       nil);
              LineTo(DC,
                     r1.Left+(r1.Right-r1.Left) div 2,
                     r1.Bottom);
            end
          else
            if b.BandType in VerticalBands then
              begin
                MoveToEx(DC,
                         Right,
                         Top+(Bottom-Top) div 2,
                         nil);
                LineTo(DC,
                       r1.Right,
                       r1.Top+(r1.Bottom-r1.Top) div 2);
              end;
        end;

    for j:=0 to b.Objects.Count-1 do
      begin
        o := b.Objects[j];
        r1 := DsgnR(o);
        for k:=0 to o.TopObjs.Count-1 do
          with DsgnR(o.TopObjs[k]) do
            begin
              MoveToEx(DC,r1.Left+(r1.Right-r1.Left) div 2,r1.Top,nil);
              LineTo(DC,Left+(Right-Left) div 2,Bottom);
            end;

        for k:=0 to o.LeftObjs.Count-1 do
          with DsgnR(o.LeftObjs[k]) do
            begin
              MoveToEx(DC,r1.Left,r1.Top+(r1.Bottom-r1.Top) div 2,nil);
              LineTo(DC,Right,Top+(Bottom-Top) div 2);
            end;

        for k:=0 to o.WidthObjs.Count-1 do
          with DsgnR(o.WidthObjs[k]) do
            begin
              MoveToEx(DC,r1.Right,r1.Top+(r1.Bottom-r1.Top) div 2,nil);
              LineTo(DC,Right,Top+(Bottom-Top) div 2);
            end;

        for k:=0 to o.HeightObjs.Count-1 do
          with DsgnR(o.HeightObjs[k]) do
            begin
              MoveToEx(DC,r1.Left+(r1.Right-r1.Left) div 2,r1.Bottom,nil);
              LineTo(DC,Left+(Right-Left) div 2,Bottom);
            end;
      end;
  end;

SelectObject(DC,opn);
DeleteObject(npn);
SetROP2(DC,pmm);
end;

procedure TprCustomDesignerBox.HideDrawing(DC : HDC);
begin
FSelectionDrawObject.HideSelection(DC);
DrawLinks(DC);
end;

procedure TprCustomDesignerBox.ShowDrawing(DC : HDC);
begin
DrawLinks(DC);
FSelectionDrawObject.ShowSelection(DC);
end;

function TprCustomDesignerBox.GetStuckedOffs(Obj : TprDesignComponent; XOffs,YOffs : integer; AllowStuckSides : TprAllowStuckSidesSet; var StuckedX,StuckedY : integer) : boolean;
const
  aso : array [boolean] of integer = (-1,0);
var
  p : TprCustomPage;
  i,j : integer;
  r,r2 : TRect;
  fOver : boolean;

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

  procedure CheckObj(dc : TprDesignComponent);
  var
    offs : integer;
  begin
  r2 := DsgnR(dc);
  if CheckVer then
    begin
      if prassLeft in AllowStuckSides then
        begin
          offs := r2.Left-r.Left-aso[fOver];
          if (Abs(offs)<DesignerPanel.StuckOffs) and (Abs(offs)<Abs(StuckedX)) then
            StuckedX := offs;

          offs := r2.Right-r.Left-aso[fOver]-1;
          if (Abs(offs)<DesignerPanel.StuckOffs) and (Abs(offs)<Abs(StuckedX)) then
            StuckedX := offs;
        end;

      if prassRight in AllowStuckSides then
        begin
          if fOver then
            offs := r2.Left-r.Right+1
          else
            offs := r2.Left-r.Right-aso[fOver]-1;
          if (Abs(offs)<DesignerPanel.StuckOffs) and (Abs(offs)<Abs(StuckedX)) then
            StuckedX := offs;

          offs := r2.Right-r.Right+aso[fOver];
          if (Abs(offs)<DesignerPanel.StuckOffs) and (Abs(offs)<Abs(StuckedX)) then
            StuckedX := offs;
        end;
    end;
  if CheckHor then
    begin
      if prassTop in AllowStuckSides then
        begin
          offs := r2.Top-r.Top-aso[fOver];
          if (Abs(offs)<DesignerPanel.StuckOffs) and (Abs(offs)<Abs(StuckedY)) then
            StuckedY := offs;

          offs := r2.Bottom-r.Top-aso[fOver]-1;
          if (Abs(offs)<DesignerPanel.StuckOffs) and (Abs(offs)<Abs(StuckedY)) then
            StuckedY := offs;
        end;

      if prassBottom in AllowStuckSides then
        begin
          if fOver then
            offs := r2.Top-r.Bottom+1
          else
            offs := r2.Top-r.Bottom-aso[fOver]-1;
          if (Abs(offs)<DesignerPanel.StuckOffs) and (Abs(offs)<Abs(StuckedY)) then
            StuckedY := offs;
    
          offs := r2.Bottom-r.Bottom+aso[fOver];
          if (Abs(offs)<DesignerPanel.StuckOffs) and (Abs(offs)<Abs(StuckedY)) then
            StuckedY := offs;
        end;
    end;
  end;

begin
p := DesignerPanel.CurPage;
r := DsgnR(Obj);
OffsetRect(r,XOffs,YOffs);

fOver := prsoStuckOver in DesignerPanel.StuckOptions;
StuckedX := MaxInt;
StuckedY := MaxInt;
for i:=0 to p.Bands.Count-1 do
  begin
    if (prsoStuckToBands in DesignerPanel.StuckOptions) and (Obj<>p.Bands[i]) then
      CheckObj(p.Bands[i]);
    for j:=0 to p.Bands[i].Objects.Count-1 do
      if (prsoStuckToObjects in DesignerPanel.StuckOptions) and (Obj<>p.Bands[i].Objects[j]) then
        CheckObj(p.Bands[i].Objects[j]);
  end;
if StuckedX=MaxInt then
  StuckedX := 0;
if StuckedY=MaxInt then
  StuckedY := 0;
Result := (StuckedY<>0) or (StuckedY<>0);
end;

procedure TprCustomDesignerBox.DrawAnime(DC : HDC; x,y : integer; CallWhileEvents : boolean);
var
  r : TRect;
  npn,opn : HPEN;
  pmm,oBottom,oLeft,oTop,oRight : integer;

  procedure DrawObjectsOffs(dx,dy : integer);
  var
    i : integer;
  begin
  for i:=0 to SelCount-1 do
    if SelObjs[i].DsgnAllowDrag then
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
          r := DesignerPanel.GetSelectedRect;
          OffsetRect(r,X-FStartX+FStuckedX,Y-FStartY+FStuckedY);
          OffsetRect(r,HorzScrollBar.Position,VertScrollBar.Position);
          DesignerPanel.ConvertFromDesignerCoords(r,r);
          DesignerPanel.DoWhileObjectDrag(r);
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
                begin
                  OffsetRect(r,HorzScrollBar.Position,VertScrollBar.Position);
                  DesignerPanel.ConvertFromDesignerCoords(r,r);
                  DesignerPanel.DoWhileObjectResize(r);
                end;
            end;
        end;
    end;
  mmRegionLink:
    begin
      pmm := SetROP2(DC,R2_NOT);
      npn := CreatePen(PS_SOLID,1,clBlack);
      opn := SelectObject(DC,npn);

      if (FStartRealX<>X) or (FStartRealY<>Y) then
        begin
          Canvas.MoveTo(FStartRealX,FStartRealY);
          Canvas.LineTo(X,Y);
        end;

      SelectObject(DC,opn);
      DeleteObject(npn);
      SetROP2(DC,pmm);
    end;
  mmSelectedResize:
    begin
    end;
end;
end;

procedure TprCustomDesignerBox.DrawGrid(DC : HDC; BackRgn : HRGN);
begin
end;

procedure TprCustomDesignerBox.DrawOther(DC : HDC);
begin
end;

procedure TprCustomDesignerBox.InternalPaint(DC : HDC; Rgn : HRGN);
var
  b : TprBand;
  o : TprObj;
  p : TprCustomPage;
  r : TRect;
  nbr : HBRUSH;
  i,j : integer;
  BackRgn,TempRgn : HRGN;
begin
  p := DesignerPanel.CurPage;
  if p = nil then
    begin
      nbr := CreateSolidBrush(clWhite);
      FillRgn(DC,Rgn,nbr);
      DeleteObject(nbr);
    end
  else
    begin
      // 0. remove all links and selections
      FSelectionDrawObject.HideSelectionEx(DC,Rgn);
      DrawLinks(DC);
  
      // 1. We paint over everything, that is not occupied in objects
      BackRgn := CreateRectRgn(0,0,0,0);
      if DrawWidth<ClientWidth then
        AddRectToRegion(BackRgn,Rect(DrawWidth,0,ClientWidth,ClientHeight),RGN_OR);
      if DrawHeight<ClientHeight then
        AddRectToRegion(BackRgn,Rect(0,DrawHeight,ClientWidth,ClientHeight),RGN_OR);
      CombineRgn(BackRgn,BackRgn,Rgn,RGN_AND);
      nbr := CreateSolidBrush(GetRGBColor(clBtnFace));
      FillRgn(DC,BackRgn,nbr);
      DeleteObject(nbr);
      DeleteObject(BackRgn);
  
      // form BackRgn
      BackRgn := CreateRectRgn(0,0,DrawWidth,DrawHeight);
      CombineRgn(BackRgn,BackRgn,Rgn,RGN_AND);
      for I := 0 to p.Bands.Count - 1 do
        for J := 0 to p.Bands[I].Objects.Count - 1 do
          begin
            o := p.Bands[I].Objects[J];
            if not o.DsgnIsTransparent and RectInRegion(Rgn, DsgnR(o)) then
              begin
                TempRgn := CreateRectRgnIndirect(DsgnR(o));
                CombineRgn(BackRgn,BackRgn,TempRgn,RGN_DIFF);
                DeleteObject(TempRgn);
              end;
          end;
  
      // draw grid
      if DesignerPanel.ShowGrid then
        begin
          DrawGrid(DC,BackRgn);
        end
      else
        begin
          nbr := CreateSolidBrush(clWhite);
          FillRgn(DC,BackRgn,nbr);
          DeleteObject(nbr);
        end;
      DeleteObject(BackRgn);
  
      // 2. draw only those objects, which are intersected with Rgn,
      SelectClipRgn(DC,Rgn);
      // first - bands
      for i:=0 to p.Bands.Count-1 do
        begin
          b := p.Bands[i];
          r := DsgnR(b);
          if RectInRegion(Rgn, r) then
            b.DrawDesign(DC, GetExData, r);
        end;
      // second - non transparent objects
      for I := 0 to p.Bands.Count - 1 do
        for J := 0 to p.Bands[I].Objects.Count - 1 do
        begin
          o := p.Bands[I].Objects[J];
          r := DsgnR(o);
          if not o.DsgnIsTransparent and RectInRegion(Rgn, r) then
            o.DrawDesign(DC, GetExData, r);
        end;
      // three - transparent objects
      for I := 0 to p.Bands.Count - 1 do
        for J := 0 to p.Bands[I].Objects.Count - 1 do
        begin
          o := p.Bands[I].Objects[J];
          r := DsgnR(o);
          if o.DsgnIsTransparent and RectInRegion(Rgn, r) then
            o.DrawDesign(DC, GetExData, r);
        end;
  
      // 3. Draw other
      DrawOther(DC);
  
      // 4. Remove any clip region
      SelectClipRgn(DC, 0);
  
      // 5. draw links and selection
      DrawLinks(DC);
      FSelectionDrawObject.ShowSelectionEx(DC,Rgn);
    end;
end;

procedure TprCustomDesignerBox.WMPaint(var Msg : TWMPaint);
var
  PS : TPaintStruct;
  Rgn : HRGN;
begin
Msg.Result := 1;
if DesignerPanel.FDisablePageDraw or (csDestroying in ComponentState) then exit;

BeginPaint(Handle,PS);
Rgn := CreateRectRgnIndirect(PS.rcPaint);
InternalPaint(Canvas.Handle,Rgn);
DeleteObject(Rgn);
EndPaint(Handle,PS);
end;

procedure TprCustomDesignerBox.WMEraseBkgnd(var Msg : TWmEraseBkgnd);
begin
Msg.Result := 1;
end;

procedure TprCustomDesignerBox.GetPointInfoAt(x,y : integer; var dc : TprDesignComponent; var PointInfo : TprPointInfo; var ResizeMode : TprResizeType; var LinkMode : TprLinkType);
var
  p : TprCustomPage;
  i,j,dx,dy : integer;

  function AnalysListResize(L : TList) : boolean;
  var
    i : integer;
  begin
  Result := true;
  for i:=0 to L.Count-1 do
    begin
      dc := TprDesignComponent(L[i]);
      with DsgnR(dc) do
        begin
          dx := (Right-Left) div 2;
          dy := (Bottom-Top) div 2;
        end;
      if GetResizeType(X,Y,DsgnR(dc),ResizeMode) and (ResizeMode in dc.DsgnAllowResizeTypes) then
        exit;
    end;
  Result := false;
  end;

  function AnalysListLink(L : TList) : boolean;
  var
    i : integer;
  begin
  Result := true;
  for i:=0 to L.Count-1 do
    begin
      dc := TprDesignComponent(L[i]);
      with DsgnR(dc) do
        begin
          if (ltLeft in dc.DsgnAllowLinkTypes) and PointInRect(X,Y,Rect(Left-2,Top,Left+2,Bottom)) then
            begin
              LinkMode := ltLeft;
              exit;
            end;

          if (ltTop in dc.DsgnAllowLinkTypes) and PointInRect(X,Y,Rect(Left,Top-2,Right,Top+2)) then
            begin
              LinkMode := ltTop;
              exit;
            end;

          if (ltRight in dc.DsgnAllowLinkTypes) and PointInRect(X,Y,Rect(Right-2,Top,Right+2,Bottom)) then
            begin
              LinkMode := ltRight;
              exit;
            end;

          if (ltBottom in dc.DsgnAllowLinkTypes) and PointInRect(X,Y,Rect(Left,Bottom-2,Right,Bottom+2)) then
            begin
              LinkMode := ltBottom;
              exit;
            end;
        end;
    end;
  Result := false;
  end;

begin
dc := nil;
PointInfo := piNone;
p := DesignerPanel.CurPage;
if p=nil then exit;

// selected rectangle
if (DesignerPanel.SelCount>1) and CSP(X,Y,FSelectedRect.Right,FSelectedRect.Bottom) then
  begin
    PointInfo := piSelectedResize;
    ResizeMode := ppRightBottom;
    exit;
  end;

// check Resize
PointInfo := piRegionResize;
if AnalysListResize(DesignerPanel.SelObjsList) then exit;

// check Link
PointInfo := piRegionLink;
if AnalysListLink(DesignerPanel.SelObjsList) then exit;

// getting selected object
dc := nil;
PointInfo := piNone;
for i:=0 to p.Bands.Count-1 do
  for j:=p.Bands[i].Objects.Count-1 downto 0 do
    if PointInRect(X,Y,DsgnR(p.Bands[i].Objects[j])) then
      begin
        dc := p.Bands[i].Objects[j];
        PointInfo := piRegionInside;
        exit;
      end;

// horizontal bands
for i:=0 to p.Bands.Count-1 do
  if (p.Bands[i].BandType in HorizontalBands) and
     PointInRect(X,Y,DsgnR(p.Bands[i])) then
    begin
      dc := p.Bands[i];
      PointInfo := piRegionInside;
      exit;
    end;

// vertical bands
for i:=0 to p.Bands.Count-1 do
  if (p.Bands[i].BandType in VerticalBands) and
     PointInRect(X,Y,DsgnR(p.Bands[i])) then
    begin
      dc := p.Bands[i];
      PointInfo := piRegionInside;
      exit;
    end;

// other sections
for i:=0 to p.Bands.Count-1 do
  if not(p.Bands[i].BandType in HorizontalBands) and
     not(p.Bands[i].BandType in VerticalBands) and
     PointInRect(X,Y,DsgnR(p.Bands[i])) then
    begin
      dc := p.Bands[i];
      PointInfo := piRegionInside;
      exit;
    end;
end;

procedure TprCustomDesignerBox.SetMouseCursorState(X,Y : integer);
var
  nc : TCursor;
  dc : TprDesignComponent;
  LinkMode : TprLinkType;
  Selected : boolean;
  PointInfo : TprPointInfo;
  ResizeMode : TprResizeType;
begin
nc := crArrow;

GetPointInfoAt(X,Y,dc,PointInfo,ResizeMode,LinkMode);
Selected := DesignerPanel.IsObjSelected(dc);
if FMouseMode=mmNone then
  begin
    if DesignerPanel.FCurClassRef<>nil then
      nc := crCross
    else
      begin
        if PointInfo=piSelectedResize then
          nc := prResizeCursors[ResizeMode]
        else
          if (DesignerPanel.SelCount>1) and (dc<>nil) and Selected then
            nc := crMultiDrag
          else
            if (dc<>nil) and dc.DsgnAllowDrag and ((not Selected) or (Selected and (PointInfo=piRegionInside))) then
              nc := crDrag
            else
              case PointInfo of
                piRegionResize:
                  begin
                    if Selected and (ResizeMode in dc.DsgnAllowResizeTypes) then
                      nc := prResizeCursors[ResizeMode];
                  end;
                piRegionLink:
                  begin
                    if Selected and (LinkMode in dc.DsgnAllowLinkTypes) then
                      nc := crHandPoint
                  end
              end;
      end;
  end
else
  if FMouseMode=mmRegionLink then
    begin
      if FDownObject.DsgnAllowLinkWith(dc) then
        nc := FLinkAcceptedCursor
      else
        nc := FLinkNotAcceptedCursor;
    end;

if Cursor<>nc then
  begin
    Cursor := nc;
    if FMouseMode=mmRegionLink then
      SetCursor(Screen.Cursors[nc]); // update cursor if FMouseMode = mmRegionLink
  end;
end;

procedure TprCustomDesignerBox.SetLinksControl;
var
  r : TRect;
begin
if dc is TprObj then
  FLinksObject := dc
else
  FLinksObject := nil;
  
if FLinksObject=nil then
  begin
    if FLinksControl<>nil then
      begin
        FLinksControl.Free;
        FLinksControl := nil;
        if DesignerPanel.Focused then // !!!
          DesignerPanel.SetFocus; // !!!
      end
  end
else
  begin
    r := DsgnR(FLinksObject);
    // links button must be visible
    r.Left := Max(r.Left,0);
    r.Top := Max(r.Top,0);
    if FLinksControl=nil then
      begin
        FLinksControl := TprObjectLinksButton.Create(Self);
        FLinksControl.Left := r.Left;
        FLinksControl.Top := r.Top;
        FLinksControl.Parent := Self;
        FLinksControl.OnClick := OnLinksButtonClick;
      end
    else
      begin
        FLinksControl.SetBounds(r.Left,r.Top,LinkButtonWidth,LinkButtonHeight);
      end;
  end;
end;

procedure TprCustomDesignerBox.OnLinksButtonClick(Sender : TObject);
var
  p : TPoint;
  m : TMenuItem;

  procedure AddLinks(L : TprObjs);
  var
    i : integer;
  begin
  if (L.Count>0) and (m.Count>0) and (m.Items[m.Count-1].Caption<>'-') then
    AddPopupMenuItem(FLinksControlPopupMenu,m,0,'',nil,'',0,true,false);
  for i:=0 to L.Count-1 do
    AddPopupMenuItemS(FLinksControlPopupMenu,m,L[i].GetDesc,L[i].ClassName,OnLinksControlPopupMenuDeleteLink,'',integer(L[i]),true,false);
  end;
  
begin
DesignerPanel.SetFocus;

ClearPopupMenu(FLinksControlPopupMenu);
FLinksObject.DsgnDefinePopupMenu(FLinksControlPopupMenu,true);
if FLinksControlPopupMenu.Items.Count>0 then
  AddPopupMenuItem(FLinksControlPopupMenu,nil,0,'',nil,'',0,true,false);
// add define links items
AddPopupMenuItem(FLinksControlPopupMenu,nil,sLinksControlDefineLeftLink,'',OnLinksControlPopupMenuLink,'',integer(ltLeft),true,false);
AddPopupMenuItem(FLinksControlPopupMenu,nil,sLinksControlDefineTopLink,'',OnLinksControlPopupMenuLink,'',integer(ltTop),true,false);
AddPopupMenuItem(FLinksControlPopupMenu,nil,sLinksControlDefineWidthLink,'',OnLinksControlPopupMenuLink,'',integer(ltRight),true,false);
AddPopupMenuItem(FLinksControlPopupMenu,nil,sLinksControlDefineHeightLink,'',OnLinksControlPopupMenuLink,'',integer(ltBottom),true,false);
// add menu for delete links
if FLinksObject is TprObj then
  with TprObj(FLinksObject) do
    if (LeftObjs.Count>0) or (TopObjs.Count>0) or (WidthObjs.Count>0) or (HeightObjs.Count>0) then
      begin
        m := AddPopupMenuItem(FLinksControlPopupMenu,nil,sLinksControlDeleteLink,'',nil,'',0,true,false);
        AddLinks(LeftObjs);
        AddLinks(TopObjs);
        AddLinks(WidthObjs);
        AddLinks(HeightObjs);
      end;

p := ClientToScreen(Point(0,0));
FLinksControlPopupMenu.Popup(TControl(Sender).Left+p.x,TControl(Sender).Top+TControl(Sender).Height+p.y);
end;

procedure TprCustomDesignerBox.OnLinksControlPopupMenuLink(Sender : TObject);
begin
if FLinksObject=nil then exit;
MakeLinkFromObject(FLinksObject,TprLinkType(TMenuItem(Sender).Tag));
end;

procedure TprCustomDesignerBox.OnLinksControlPopupMenuDeleteLink(Sender : TObject);
begin
if not (FLinksObject is TprObj) then exit;
with TprObj(FLinksObject) do
  if LeftObjs.IndexOf(TprObj(TMenuItem(Sender).Tag))<>-1 then
    DesignerPanel.DeleteObjLink(TprObj(FLinksObject),ltLeft,TprObj(TMenuItem(Sender).Tag))
  else
    if TopObjs.IndexOf(TprObj(TMenuItem(Sender).Tag))<>-1 then
      DesignerPanel.DeleteObjLink(TprObj(FLinksObject),ltTop,TprObj(TMenuItem(Sender).Tag))
    else
      if WidthObjs.IndexOf(TprObj(TMenuItem(Sender).Tag))<>-1 then
        DesignerPanel.DeleteObjLink(TprObj(FLinksObject),ltRight,TprObj(TMenuItem(Sender).Tag))
      else
        if HeightObjs.IndexOf(TprObj(TMenuItem(Sender).Tag))<>-1 then
          DesignerPanel.DeleteObjLink(TprObj(FLinksObject),ltBottom,TprObj(TMenuItem(Sender).Tag))
end;

procedure TprCustomDesignerBox.MakeLinkFromObject(dc : TprDesignComponent; LinkMode : TprLinkType);
begin
FDownObject := dc;
FLinkPreviosObject := nil;
FMouseMode := mmRegionLink;
FDownLinkMode := LinkMode;
with DsgnR(dc) do
  case LinkMode of
    ltLeft : begin FStartRealX := Left; FStartRealY := Top+(Bottom-Top) div 2; end;
    ltTop : begin FStartRealX := Left+(Right-Left) div 2; FStartRealY := Top; end;
    ltRight : begin FStartRealX := Right; FStartRealY := Top+(Bottom-Top) div 2; end;
    ltBottom : begin FStartRealX := Left+(Right-Left) div 2; FStartRealY := Bottom; end;
  end;
FLastX := FStartRealX;
FLastY := FStartRealY;
SetCapture(Handle);
end;

procedure TprCustomDesignerBox.DoLink(SourceObject : TprDesignComponent; DestObject : TprDesignComponent; LinkMode : TprLinkType);
var
  LinkAccepted : boolean;
begin
LinkAccepted := false;
SourceObject.DsgnLink(DestObject,LinkMode,LinkAccepted,GetExData);
if LinkAccepted then
  begin
    DesignerPanel.DsgnNotifyReport(true);
    DesignerPanel.DoLinkAdded(SourceObject,DestObject,LinkMode);
  end;
end;

procedure TprCustomDesignerBox.DoResize(ResizeObject : TprDesignComponent; oTop,oLeft,oBottom,oRight : integer; prps : pPrPaintStruct);
var
  ResizeAccepted : boolean;
  OldDsgnPageRect : TRect;
begin
oLeft := DesignerPanel.ConvertXFromDesignerCoords(oLeft);
oRight := DesignerPanel.ConvertXFromDesignerCoords(oRight);
oTop := DesignerPanel.ConvertYFromDesignerCoords(oTop);
oBottom := DesignerPanel.ConvertYFromDesignerCoords(oBottom);

OldDsgnPageRect := DsgnR(ResizeObject);
ResizeAccepted := false;
ResizeObject.DsgnResize(oTop,oLeft,oBottom,oRight,ResizeAccepted,GetExData);
if ResizeAccepted then
  begin
    if ResizeObject=FLinksObject then
      SetLinksControl(FLinksObject);
    if prps<>nil then
      begin
        AddRectToRegion(prps.ClipRgn,OldDsgnPageRect);
        AddRectToRegion(prps.ClipRgn,DsgnR(ResizeObject));
        if ResizeObject is TprObj then
          Include(prps.ClipState,prcsObj)
        else
          if ResizeObject is TprBand then
            Include(prps.ClipState,prcsBand);
      end;
    DesignerPanel.UpdatePosSizeForm;
    DesignerPanel.DsgnNotifyReport(true);
    DesignerPanel.DoObjectResized(ResizeObject);
  end;
end;

procedure TprCustomDesignerBox.DoDrag(DragObject : TprDesignComponent; dx,dy : integer; prps : pPrPaintStruct);
var
  DragAccepted : boolean;
  OldDsgnPageRect : TRect;
begin
dx := DesignerPanel.ConvertXFromDesignerCoords(dx);
dy := DesignerPanel.ConvertYFromDesignerCoords(dy);

OldDsgnPageRect := DsgnR(DragObject);
DragAccepted := false;
DragObject.DsgnDrag(dx,dy,DragAccepted,GetExData);
if DragAccepted then
  begin
    if DragObject=FLinksObject then
      SetLinksControl(FLinksObject);
    if prps<>nil then
      begin
        AddRectToRegion(prps.ClipRgn,OldDsgnPageRect);
        AddRectToRegion(prps.ClipRgn,DsgnR(DragObject));
        if DragObject is TprObj then
          Include(prps.ClipState,prcsObj);
      end;
    DesignerPanel.UpdatePosSizeForm;
    DesignerPanel.DsgnNotifyReport(true);
    DesignerPanel.DoObjectDrag(DragObject);
  end;
end;

procedure TprCustomDesignerBox.InvertObject(dc : TprDesignComponent);
begin
InvertRect(Canvas.Handle,DsgnR(dc));
end;

procedure TprCustomDesignerBox.DblClick;
var
  Processed : boolean;
begin
FDblClick := true;
FMouseMode := mmNone;
FFirstMoveAfterDown := false;
DesignerPanel.DoDesignerDblClick(Processed);
if Processed then exit;
if FDownObject<>nil then
  begin
    if not DesignerPanel.VisibleObjectsPropsForm then
      DesignerPanel.VisibleObjectsPropsForm := true;
  end;
end;

function TprCustomDesignerBox.GetPopupMenu : TPopupMenu;
begin
if FRightMouseButtonPressed then
  begin
    Result := inherited GetPopupMenu;
    FRightMouseButtonPressed := false;
  end
else
  Result := nil;
end;

procedure TprCustomDesignerBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  dc : TprDesignComponent;
  LinkMode : TprLinkType;
  PointInfo : TprPointInfo;
  ResizeMode : TprResizeType;
  FDownSelected : boolean;
  Processed : boolean;
begin
DesignerPanel.DoDesignerMouseDown(Button,Shift,x,y,Processed);
if Processed then exit;
FRightMouseButtonPressed := true;  // for GetPopupMenu
SetFocus;
if DesignerPanel.CurPage=nil then exit;

if FMouseMode=mmRegionLink then
  begin
    // mode - set link
    DrawAnime(Canvas.Handle,X,Y,false); // remove animation
    if FLinkPreviosObject<>nil then
      begin
        InvertObject(FLinkPreviosObject);
        FLinkPreviosObject := nil;
      end;
    HideDrawing(Canvas.Handle);
    GetPointInfoAt(x,y,dc,PointInfo,ResizeMode,LinkMode);
    if (dc<>FDownObject) and (dc<>nil) then
      DoLink(FDownObject,dc,FDownLinkMode);
    ShowDrawing(Canvas.Handle);
    FMouseMode := mmNone;
    exit;
  end;

if FDblClick then
  begin
    FDblClick := false;
    exit;
  end;

if [ssLeft,ssRight]*Shift=[] then exit;

// Starting mouse coords
FStartRealX := X;
FStartRealY := Y;
FLastX := X;
FLastY := Y;
if DesignerPanel.UseGrid then
  DesignerPanel.AdjustMouseToGrid(FLastX,FLastY);
FStartX := FLastX;
FStartY := FLastY;
FFirstMoveAfterDown := true;

GetPointInfoAt(X,Y,FDownObject,FDownPointInfo,FDownResizeMode,FDownLinkMode);
FDownSelected := DesignerPanel.IsObjSelected(FDownObject);

if (FInplaceEditor<>nil) and ((FDownObject=nil) or (FDownObject<>FInplaceEditedObj)) then
  begin
    // end inplace edit
    DesignerPanel.SaveInplaceEdit;
    FPrevObj := nil;
  end;

if DesignerPanel.FCurClassRef<>nil then
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
    // RIGHT mouse button pressed
    if FDownObject<>nil then
      begin
        if ssShift in Shift then
          begin
            if not FDownSelected then
              DesignerPanel.AddSelectObject(FDownObject);
          end
        else
          DesignerPanel.SelectObject(FDownObject);
      end;
    FMouseMode := mmNone;
    exit;
  end;

// LEFT mouse button pressed
if FDownPointInfo=piSelectedResize then
  begin
    // mode - resize objects
    HideDrawing(Canvas.Handle);
    FMouseMode := mmSelectedResize;
    exit;
  end;

if FDownObject=nil then
  begin
    // select objects
    HideDrawing(Canvas.Handle);
    DesignerPanel.InternalClearSelected;
    FMouseMode := mmSelect;
    DesignerPanel.DoSelectionChanged;
    exit;
  end;

// FDownObject<>nil
if ssShift in Shift then
  begin
    if FDownSelected then
      DesignerPanel.DeSelectObject(FDownObject)
    else
      DesignerPanel.AddSelectObject(FDownObject);
    FMouseMode := mmNone;
    exit;
  end;

HideDrawing(Canvas.Handle);
if FDownSelected then
  begin
    if DesignerPanel.SelCount=1 then
      begin
        case FDownPointInfo of
          piRegionInside:
            begin
              if FDownObject.DsgnAllowDrag then
                FMouseMode := mmSelectedRegionsDrag // mode - drag of object
              else
                FMouseMode := mmSelect; // mode - select objects
            end;
          piRegionResize:
            begin
              FMouseMode := mmRegionResize;
            end;
          piRegionLink:
            begin
              FLastX := X; // !!! remove bug while first call of DrawAnime in MouseMove
              FLastY := Y;
              FMouseMode := mmRegionLink;
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
    DesignerPanel.InternalClearSelected;
    if FDownObject.DsgnAllowDrag then
      begin
        DesignerPanel.InternalSelectObj(FDownObject);
        ShowDrawing(Canvas.Handle);
        HideDrawing(Canvas.Handle);
        FMouseMode := mmSelectedRegionsDrag;
      end
    else
      FMouseMode := mmSelect;
    DesignerPanel.DoSelectionChanged;
  end;
end;

procedure TprCustomDesignerBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  o : TprObj;
  b : TprBand;
  L : TList;
  p : TprCustomPage;
  f : boolean;
  dc : TprDesignComponent;
  r,r2 : TRect;
  Delta : DWORD;
  ClipRgn : HRGN;
  LinkMode : TprLinkType;
  PointInfo : TprPointInfo;
  ResizeMode : TprResizeType;
  RealX,RealY,i,j,dx,dy,oTop,oLeft,oBottom,oRight : integer;
begin
if FMouseMode=mmNone then exit;
p := DesignerPanel.CurPage;
if p=nil then exit;
RealX := X;
RealY := Y;
if (FMouseMode<>mmRegionLink) and (FStuckedX=0) and (FStuckedY=0) and DesignerPanel.UseGrid then
  DesignerPanel.AdjustMouseToGrid(X,Y);

DrawAnime(Canvas.Handle,X,Y,false); // hide animation

ClipRgn := 0;
case FMouseMode of
  mmInsertObj:
    begin
      if FFirstMoveAfterDown then
        r := Rect(x,y,x+100,y+40)
      else
        r := NormalizeRect(FStartX,FStartY,X,Y);
      OffsetRect(r,DesignerPanel.HorScrollBoxPos,DesignerPanel.VerScrollBoxPos);

      // r - sizes of inserting object
      if FDownObject<>nil then
        begin
          if FDownObject is TprObj then
            b := TprObj(FDownObject).Band
          else
            if FDownObject is TprCustomHBand then
              b := TprCustomHBand(FDownObject)
            else
              if FDownObject is TprCustomVBand then
                b := TprCustomVBand(FDownObject)
              else
                b := nil;
    
            if b<>nil then
              begin
                if DesignerPanel.UseGrid then
                  begin
                    DesignerPanel.AdjustToGrid(r.Left,r.Top);
                    DesignerPanel.AdjustToGrid(r.Right,r.Bottom);
                    r.Right := r.Right+1;
                    r.Bottom := r.Bottom+1;
                  end;
                DesignerPanel.ConvertFromDesignerCoords(r,r2);
                if (r2.Right-r2.Left>0) and (r2.Bottom-r2.Top>0) then
                  begin
                    OffsetRect(r,-DesignerPanel.HorScrollBoxPos,-DesignerPanel.VerScrollBoxPos);
                    ClipRgn := CreateRectRgnIndirect(r);
                    o := CreatePrObj(DesignerPanel.FCurClassRef,b);
                    o.dRec.Left := r2.Left-b.dPageRect.Left;
                    o.dRec.Top := r2.Top-b.dPageRect.Top;
                    o.dRec.Right := r2.Right-b.dPageRect.Left;
                    o.dRec.Bottom := r2.Bottom-b.dPageRect.Top;
                    DesignerPanel.DoObjectInserted(o);

                    DesignerPanel.InternalClearSelected;
                    DesignerPanel.InternalSelectObj(o);
                    DesignerPanel.DsgnNotifyReport(true);
                    DesignerPanel.DoSelectionChanged;
                  end;
              end;
        end;
      ShowDrawing(Canvas.Handle);
    end;
  mmSelect:
    begin
      if Shift<>[ssShift] then
        DesignerPanel.InternalClearSelected;

      r := NormalizeRect(FStartRealX,FStartRealY,RealX,RealY);
      f := true;
      for i:=0 to p.Bands.Count-1 do
        for j:=0 to p.Bands[i].Objects.Count-1 do
          if RectOverRect(DsgnR(p.Bands[i].Objects[j]),r) then
            begin
              DesignerPanel.InternalSelectObj(p.Bands[i].Objects[j]);
              f := false;
            end;
      if f then
        begin
          L := TList.Create;
          try
            for i:=0 to p.Bands.Count-1 do
              if RectOverRect(DsgnR(p.Bands[i]),r) then
                L.Add(p.Bands[i]);
            if L.Count>0 then
              if (FStartRealX=RealX) and (FStartRealY=RealY) then
                begin
                  i := 0;
                  while (i<L.Count) and not(TprDesignComponent(L[i]) is TprCustomHBand) do Inc(i);
                  if i<L.Count then
                    DesignerPanel.InternalSelectObj(TprDesignComponent(L[i]))
                  else
                    DesignerPanel.InternalSelectObj(TprDesignComponent(L[0]));
                end
              else
                begin
                  for i:=0 to L.Count-1 do
                    DesignerPanel.InternalSelectObj(TprDesignComponent(L[i]));
                end;
          finally
            L.Free;
          end;
        end;
      ShowDrawing(Canvas.Handle);
      DesignerPanel.DoSelectionChanged;
    end;
  mmRegionLink:
    begin
      if FLinkPreviosObject<>nil then
        begin
          InvertObject(FLinkPreviosObject);
          FLinkPreviosObject := nil;
        end;
      GetPointInfoAt(x,y,dc,PointInfo,ResizeMode,LinkMode);
      if (dc<>FDownObject) and (dc<>nil) then
        DoLink(FDownObject,dc,FDownLinkMode);
      ShowDrawing(Canvas.Handle);
    end;
  mmRegionResize:
    begin
      // size of object is changed
      CalcOffs(x-FStartX,y-FStartY,FDownResizeMode,oTop,oLeft,oBottom,oRight,FStuckedX,FStuckedY);
      if (DsgnR(FDownObject).Left+oLeft<DsgnR(FDownObject).Right+oRight) and
         (DsgnR(FDownObject).Top+oTop<DsgnR(FDownObject).Bottom+oBottom) then
        begin
          //
          AddRectToRegion(ClipRgn,DsgnR(FDownObject));
          DoResize(FDownObject,oTop,oLeft,oBottom,oRight,nil);
          AddRectToRegion(ClipRgn,DsgnR(FDownObject));

          if FDownObject is TprObj then
            begin
              if (FInplaceEditor<>nil) and (FInplaceEditedObj=FDownObject) then
                begin
                  FInplaceEditor.Left := FInplaceEditor.Left+oLeft;
                  FInplaceEditor.Top := FInplaceEditor.Top+oTop;
                  FInplaceEditor.Width := FInplaceEditor.Width+oRight;
                  FInplaceEditor.Height := FInplaceEditor.Height+oBottom;
                end;
            end
          else
            if FDownObject is TprBand then
              ClipRgn := CreateRectRgn(0,0,Width,Height);
        end;
      ShowDrawing(Canvas.Handle);
    end;
  mmSelectedResize:
    begin
    end;
  mmSelectedRegionsDrag:
    begin
      dx := X-FStartX+FStuckedX;
      dy := Y-FStartY+FStuckedY;
      if (dx<>0) or (dy<>0) then
        begin
          for i:=0 to SelCount-1 do
            begin
              if SelObjs[i].DsgnAllowDrag then
                begin
                  AddRectToRegion(ClipRgn,DsgnRSel(i));
                  DoDrag(SelObjs[i],dx,dy,nil);
                  AddRectToRegion(ClipRgn,DsgnRSel(i));
                end;
            end;
        end;
      ShowDrawing(Canvas.Handle);
    end;
end;

if ClipRgn<>0 then
  begin
    InternalPaint(Canvas.Handle,ClipRgn);
    DeleteObject(ClipRgn);
  end;

if (FInplaceEditor=nil) and
   (Shift=[]) and
   (DesignerPanel.SelCount=1) and
   (FMouseMode in [mmSelect,mmRegionDrag,mmSelectedRegionsDrag]) then
  begin
    // InplaceEditor
    if (FStartX=FLastX) and
       (FStartY=FLastY) and
       (FDownObject<>nil) and
       (FDownObject is TprObj) and
       (TprObj(FDownObject).DsgnAllowInplaceEdit) then
      begin
        // selected object can be edited
        if (FPrevObj<>nil) and (FPrevObj=FDownObject) then
          begin
            Delta := GetTickCount-FTickCount;
            if (Delta>700) and (Delta<1700) and (FDownPointInfo=piRegionInside) then
              begin
                DesignerPanel.InplaceEdit(TprObj(FDownObject));
              end
            else
              begin
                if Delta>1700 then
                  FTickCount := GetTickCount;
              end;
          end
        else
          begin
            FPrevObj := FDownObject;
            FTickCount := GetTickCount;
          end;
      end
    else
      begin
        FPrevObj := nil;
      end;
  end;
FMouseMode := mmNone;
DesignerPanel.DoDesignerMouseUp(Button,Shift,x,y);
end;

procedure TprCustomDesignerBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  dc : TprDesignComponent;
  XX,YY : integer;
  PointInfo : TprPointInfo;
  ResizeMode : TprResizeType;
  LinkMode : TprLinkType;
begin
FFirstMoveAfterDown := false;
DrawAnime(Canvas.Handle,FLastX,FLastY,false); // remove old animation

XX := X;
YY := Y;
FStuckedX := 0;
FStuckedY := 0;
case FMouseMode of
  mmNone:
    begin
      if FInplaceEditor=nil then
        begin
          GetPointInfoAt(x,y,dc,PointInfo,ResizeMode,LinkMode);
          if dc<>FLinksObject then
            SetLinksControl(dc);
        end;
    end;
  mmRegionLink:
    begin
      GetPointInfoAt(x,y,dc,PointInfo,ResizeMode,LinkMode);
      if not FDownObject.DsgnAllowLinkWith(dc) then
        dc := nil;
      if dc<>FLinkPreviosObject then
        begin
          if FLinkPreviosObject<>nil then
            InvertObject(FLinkPreviosObject);
          FLinkPreviosObject := dc;
          if FLinkPreviosObject<>nil then
            InvertObject(FLinkPreviosObject);
        end;
    end;
  mmSelectedRegionsDrag:
    begin
      if (DesignerPanel.StuckMode=prsmAlways) or ((DesignerPanel.StuckMode=prsmCtrlButton) and (GetKeyState(VK_CONTROL)<0)) then
        GetStuckedOffs(DesignerPanel.SelObjs[0],X-FStartX,Y-FStartY,[prassLeft,prassTop,prassRight,prassBottom],FStuckedX,FStuckedY)
      else
        if DesignerPanel.UseGrid and ((ssLeft in Shift) or (ssRight in Shift)) then
          DesignerPanel.AdjustMouseToGrid(X,Y);
    end;
  mmRegionResize:
    begin
      if (DesignerPanel.StuckMode=prsmAlways) or ((DesignerPanel.StuckMode=prsmCtrlButton) and (GetKeyState(VK_CONTROL)<0)) then
        begin
          GetStuckedOffs(DesignerPanel.SelObjs[0],X-FStartX,Y-FStartY,aStuckSides[FDownResizeMode],FStuckedX,FStuckedY)
        end
      else
        if DesignerPanel.UseGrid and ((ssLeft in Shift) or (ssRight in Shift)) then
          DesignerPanel.AdjustMouseToGrid(X,Y);
    end;
  else
    if DesignerPanel.UseGrid and ((ssLeft in Shift) or (ssRight in Shift)) then
      DesignerPanel.AdjustMouseToGrid(X,Y);
end;

DrawAnime(Canvas.Handle,X,Y,true); // draw new animation
FLastX := X;
FLastY := Y;

SetMouseCursorState(X,Y);
DesignerPanel.DoDesignerMouseMove(Shift,XX,YY);
end;

procedure TprCustomDesignerBox.WMMouseWheel(var Msg : TWMMouseWheel);
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

procedure TprCustomDesignerBox._BeginPaint(var prps : rPrPaintStruct);
begin
prps.ClipRgn := 0;
prps.ClipState := [];
HideDrawing(Canvas.Handle);
end;

procedure TprCustomDesignerBox._EndPaint(const prps : rPrPaintStruct);
begin
  ShowDrawing(Canvas.Handle);
  if prcsBand in prps.ClipState then
  begin
    // redraw all
    DesignerPanel.CurPage.UpdateBandsPageRect;
    Repaint;
  end
  else
    if prcsObj in prps.ClipState then
    begin
      if prps.ClipRgn <> 0 then
        InternalPaint(Canvas.Handle,prps.ClipRgn);
    end;
  if prps.ClipRgn <> 0 then
    DeleteObject(prps.ClipRgn);
end;

procedure TprCustomDesignerBox.RepaintObject(dc : TprDesignComponent);
var
  Rgn: HRGN;
begin
  Rgn := CreateRectRgnIndirect(DsgnR(dc));
  InternalPaint(Canvas.Handle, Rgn);
  DeleteObject(Rgn);
end;

procedure TprCustomDesignerBox.RepaintSelectedObjects;
var
  I: integer;
  Rgn: HRGN;
begin
  Rgn := 0;
  for I := 0 to SelCount - 1 do
    if SelObjs[I] is TprObj then
      AddRectToRegion(Rgn, DsgnRSel(I));

  if Rgn <> 0 then
  begin
    InternalPaint(Canvas.Handle, Rgn);
    DeleteObject(Rgn);
  end;
end;














/////////////////////////////////////////////////
//
// TprBandsCaptionsOptions
//
/////////////////////////////////////////////////
constructor TprBandsCaptionsOptions.Create;
begin
inherited;
FVisible := true; 
end;

procedure TprBandsCaptionsOptions.SetVisible(Value : boolean);
begin
if FVisible=Value then exit;
FVisible := Value;
DoChanged;
end;

procedure TprBandsCaptionsOptions.Assign(Source : TPersistent);
begin
with TprBandsCaptionsOptions(Source) do
  begin
    Self.FVisible := Visible;
  end;
DoChanged;
end;

procedure TprBandsCaptionsOptions.WriteToIni(IniFile : TIniFile; const SectionName,Prefix : string);
begin
IniFile.WriteBool(SectionName,Prefix+'Visible',Visible);
end;

procedure TprBandsCaptionsOptions.ReadFromIni(IniFile : TIniFile; const SectionName,Prefix : string);
begin
Visible := IniFile.ReadBool(SectionName,Prefix+'Visible',Visible);
end;

/////////////////////////////////////////////////
//
// TprCustomDesignerPanel
//
/////////////////////////////////////////////////
constructor TprCustomDesignerPanel.Create(AOwner : TComponent);
begin
inherited;
Width := 150;
Height := 150;
BevelOuter := bvNone;
FDesignerNotifyLink := TprNotifyLink.Create;
FDesignerNotifyLink.OnNotify := DsgnNotify;

FOpacityObjectLinksForm := 100;
FOpacityPosSizeForm := 100;
FOpacityObjectsPropsForm := 100;
FOpacityFindForm := 100;

FPopupInsertObjectMenu := true;
FPopupInsertBandMenu := true;

FSelObjs := TList.Create;

FHorBandsCaptionsOptions := TprBandsCaptionsOptions.Create;
FHorBandsCaptionsOptions.OnChange := OnChangeBandsCaptionsOptions;
FVerBandsCaptionsOptions := TprBandsCaptionsOptions.Create;
FVerBandsCaptionsOptions.OnChange := OnChangeBandsCaptionsOptions;

FTabControl := TTabControl.Create(Self);
FTabControl.TabStop := false;
FTabControl.Parent := Self;
FTabControl.OnResize := OnTabControlResize;
FTabControl.OnChange := OnTabControlChange;
FHorBandsCaptions := TprHorBandCaptions.Create(Self);
FHorBandsCaptions.Parent := FTabControl;
FVerBandsCaptions := TprVerBandCaptions.Create(Self);
FVerBandsCaptions.Parent := FTabControl;
CreateRulers(FHorRuler,FVerRuler);
if FHorRuler<>nil then
  FHorRuler.Parent := FTabControl;
if FVerRuler<>nil then
  FVerRuler.Parent := FTabControl;
FDesignerBox := CreateDesignerBox;
FDesignerBox.Parent := FTabControl;
FDesignerBox.OnVScroll := OnVScroll;
FDesignerBox.OnHScroll := OnHScroll;

FPopupMenu := TPopupMenu.Create(nil);
FPopupMenu.OnPopup := OnPopupMenuPopup;

ShowPopupMenu := true;

// create forms
if not (csDesigning in ComponentState) then
  begin
    FObjectLinksForm := CreateObjectLinksForm;
    FObjectLinksForm.DesignerPanel := Self;
    FPosSizeForm := CreatePosSizeForm;
    FPosSizeForm.DesignerPanel := Self;
    FObjectsPropsForm := CreateObjectsPropsForm;
    FObjectsPropsForm.DesignerPanel := Self;
    FFindForm := CreateFindForm;
    FFindForm.DesignerPanel := Self;
    UpdateObjectLinksForm;
    UpdatePosSizeForm;
    UpdateObjectsPropsForm;
    UpdateFindForm;
  end;
end;

destructor TprCustomDesignerPanel.Destroy;
begin
if GetReport<>nil then
  GetReport.DsgnRemoveDesignerNotifyLink(FDesignerNotifyLink);
FDesignerNotifyLink.Free;
FSelObjs.Free;
FHorBandsCaptionsOptions.Free;
FVerBandsCaptionsOptions.Free;
FPopupMenu.Free;
inherited;
end;

procedure TprCustomDesignerPanel.Notification(AComponent : TComponent; AOperation : TOperation);
begin
  inherited;
end;

procedure TprCustomDesignerPanel.DsgnNotify(Source: TObject);
begin
  FullUpdate;
end;

procedure TprCustomDesignerPanel.DsgnNotifyReport(TemplateChanged : boolean);
begin
  GetReport.DsgnTemplateChanged(FDesignerNotifyLink, TemplateChanged);
end;

procedure TprCustomDesignerPanel.InitPopupMenu(Popup : TPopupMenu);
var
  i: integer;
  m: TMenuItem;
begin
ClearPopupMenu(Popup);
if SelCount=1 then
  begin
    SelObjs[0].DsgnDefinePopupMenu(FPopupMenu,true);
    {
    dc := SelObjs[0];
    i := 1;
    while (i<SelCount) and (AnsiCompareText(SelObjs[i].ClassName,dc.ClassName)=0) do Inc(i);
    if i>=SelCount then
      // selected objects of same type
      SelObjs[0].DsgnDefinePopupMenu(FPopupMenu,SelCount=1);
    }
  end;
if Popup.Items.Count>0 then
  AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,nil,sClipboardCut,'CUT',OnPopupMenuClick,'Ctrl+X',1,AllowCut,false);
AddPopupMenuItem(Popup,nil,sClipboardCopy,'COPY',OnPopupMenuClick,'Ctrl+C',2,AllowCopy,false);
AddPopupMenuItem(Popup,nil,sClipboardPaste,'PASTE',OnPopupMenuClick,'Ctrl+V',3,AllowPaste,false);
AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,nil,sDeleteSelected,'DELETE',OnPopupMenuClick,'Ctrl+Del',4,AllowDelete,false);
AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,nil,sObjectLinks,'OBJLINKS',OnPopupMenuClick,'',5,true,VisibleObjectLinksForm);
AddPopupMenuItem(Popup,nil,sSizesAndPosition,'POSSIZE',OnPopupMenuClick,'',9,true,VisiblePosSizeForm);
AddPopupMenuItem(Popup,nil,sProperties,'PROPERTIES',OnPopupMenuClick,'Alt+Enter',7,true,VisibleObjectsPropsForm);
AddPopupMenuItem(Popup,nil,sInplaceEdit,'INPLACEEDIT',OnPopupMenuClick,'Ctrl+Enter',8,AllowInplaceEdit,false);
AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,nil,sFindInTemplate,'FIND',OnPopupMenuClick,'Ctrl+F',99,true,VisibleFindForm);

if (GetReport<>nil) and (ActivePageIndex<>-1) and
   (PopupInsertObjectMenu or PopupInsertBandMenu) then
  begin
    AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
    if PopupInsertObjectMenu then
      begin
        m := AddPopupMenuItem(Popup,nil,sInsertObject,'',nil,'',0,true,false);
        InitprObjPopupMenu(GetReport,Popup,m,OnPopupMenuInsertObjectClick,FCurClassRef);
      end;
    if PopupInsertBandMenu then
      begin
        m := AddPopupMenuItem(Popup,nil,sInsertBand,'',nil,'',0,true,false);
        for i:=integer(Low(TprBandType)) to integer(High(TprBandType)) do
          AddPopupMenuItem(Popup,m,sBandTitlesOffset-i,'',OnPopupMenuInsertBandClick,'',i,true,false);
      end;
  end;
if PopupMainMenu then
  begin
    AddPopupMenuItem(Popup,nil,0,'',nil,'',0,true,false);
    InitPopupMainMenu(Popup,AddPopupMenuItem(Popup,nil,sPopupMainMenu,'',nil,'',0,true,false));
  end;
end;

procedure TprCustomDesignerPanel.InitPopupMainMenu(Popup : TPopupMenu; MainMenuItem : TMenuItem);
var
  m : TMenuItem;
begin
m := AddPopupMenuItem(Popup,MainMenuItem,sMainMenuFile,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sMainMenuFileCreate,'NEW',OnPopupMenuClick,'Ctrl+N',10,GetReport<>nil,false);
AddPopupMenuItem(Popup,m,sMainMenuFileOpen,'OPEN',OnPopupMenuClick,'Ctrl+O',11,GetReport<>nil,false);
AddPopupMenuItem(Popup,m,sMainMenuFileSave,'SAVE',OnPopupMenuClick,'Ctrl+S',12,GetReport<>nil,false);
AddPopupMenuItem(Popup,m,sMainMenuFileSaveAs,'SAVEAS',OnPopupMenuClick,'',13,GetReport<>nil,false);
AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sMainMenuFileReportParams,'',OnPopupMenuClick,'',14,GetReport<>nil,false);
AddPopupMenuItem(Popup,m,sMainMenuFilePageParams,'PAGEPARAMS',OnPopupMenuClick,'',15,(GetReport<>nil) and (ActivePageIndex>=0) and (ActivePageIndex<GetReport.PagesCount),false);
AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sMainMenuFilePreview,'PREVIEW',OnPopupMenuClick,'',16,GetReport<>nil,false);
AddPopupMenuItem(Popup,m,sMainMenuFilePrint,'PRINT',OnPopupMenuClick,'',17,GetReport<>nil,false);

m := AddPopupMenuItem(Popup,MainMenuItem,sMainMenuPage,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sMainMenuPageAddPage,'NEWPAGE',OnPopupMenuClick,'',18,GetReport<>nil,false);
AddPopupMenuItem(Popup,m,sMainMenuPageDelPage,'DELPAGE',OnPopupMenuClick,'',19,(GetReport<>nil) and (GetReport.PagesCount>1),false);
AddPopupMenuItem(Popup,m,sMainMenuPagePageParams,'PAGEPARAMS',OnPopupMenuClick,'',15,(GetReport<>nil) and (ActivePageIndex>=0) and (ActivePageIndex<GetReport.PagesCount),false);
AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sMainMenuPageNextPage,'NEXTPAGE',OnPopupMenuClick,'',20,(GetReport<>nil) and (GetReport.PagesCount>0),false);
AddPopupMenuItem(Popup,m,sMainMenuPagePriorPage,'PRIORPAGE',OnPopupMenuClick,'',21,(GetReport<>nil) and (GetReport.PagesCount>0),false);
end;

procedure TprCustomDesignerPanel.OnPopupMenuPopup(Sender : TObject);
begin
InitPopupMenu(FPopupMenu);
end;

procedure TprCustomDesignerPanel.OnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  1: Cut;
  2: Copy;
  3: Paste;
  4: DeleteSelectedObjects;
  5: VisibleObjectLinksForm := not VisibleObjectLinksForm;
  7: VisibleObjectsPropsForm := true;
  8: InplaceEdit(TprObj(SelObjs[0]));
  9: VisiblePosSizeForm := not VisiblePosSizeForm;
  99: VisibleFindForm := not VisibleFindForm;
  10: New;
  11: Open;
  12: Save;
  13: SaveAs;
  14: EditReportParams;
  15: EditPage(ActivePageIndex);
  16: Preview;
  17: Print;
  18: InsertPageAfter(ActivePageIndex);
  19: DeletePage(ActivePageIndex);
  20: NextPage;
  21: PriorPage;
end;
end;

procedure TprCustomDesignerPanel.OnPopupMenuInsertObjectClick(Sender : TObject);
begin
with TMenuItem(Sender) do
  begin
    if Tag=1 then
      InsertObject(nil)
    else
      InsertObject(prObjRegInfos[Tag-2].ClassRef);
    DoInsertingObjectChanged;
  end;
end;  

procedure TprCustomDesignerPanel.OnPopupMenuInsertBandClick(Sender : TObject);
begin
InsertBand(TprBandType(TMenuItem(Sender).Tag));
end;  

function TprCustomDesignerPanel.DsgnR(dc : TprDesignComponent) : TRect;
begin
if dc is TprObj then
  with TprObj(dc) do
    begin
      Result := dRec.pRect;
      Result.Left := Result.Left+Band.dPageRect.Left-HorScrollBoxPos;
      Result.Top := Result.Top+Band.dPageRect.Top-VerScrollBoxPos;
      Result.Right := Result.Right+Band.dPageRect.Left-HorScrollBoxPos;
      Result.Bottom := Result.Bottom+Band.dPageRect.Top-VerScrollBoxPos;
    end
else
  if dc is TprBand then
    with TprBand(dc) do
      begin
        Result.Left := dPageRect.Left-HorScrollBoxPos;
        Result.Top := dPageRect.Top-VerScrollBoxPos;
        Result.Right := dPageRect.Right-HorScrollBoxPos;
        Result.Bottom := dPageRect.Bottom-VerScrollBoxPos;
      end
  else
    Result := Rect(-1,-1,-1,-1);
end;

function TprCustomDesignerPanel.DsgnRSel(i : integer) : TRect;
begin
Result := DsgnR(SelObjs[i]);
end;

function TprCustomDesignerPanel.DsgnWSel(i : integer) : integer;
begin
with DsgnRSel(i) do
  Result := Right-Left;
end;

function TprCustomDesignerPanel.DsgnHSel(i : integer) : integer;
begin
with DsgnRSel(i) do
  Result := Bottom-Top;
end;

function TprCustomDesignerPanel.ObjR(dc : TprDesignComponent) : TRect;
begin
if dc is TprObj then
  with TprObj(dc) do
    begin
      Result := dRec.pRect;
      Result.Left := Result.Left+Band.dPageRect.Left;
      Result.Top := Result.Top+Band.dPageRect.Top;
      Result.Right := Result.Right+Band.dPageRect.Left;
      Result.Bottom := Result.Bottom+Band.dPageRect.Top;
    end
else
  if dc is TprBand then
    Result := TprBand(dc).dPageRect
  else
    Result := Rect(-1,-1,-1,-1);
end;

function TprCustomDesignerPanel.ObjRSel(i : integer) : TRect;
begin
Result := ObjR(SelObjs[i]);
end;

function TprCustomDesignerPanel.CreateObjectLinksForm : TprCustomObjLinksForm;
begin
Result := TprObjLinksForm.Create(Self);
end;

function TprCustomDesignerPanel.CreatePosSizeForm : TprCustomPosSizeForm;
begin
Result := nil;
end;

function TprCustomDesignerPanel.CreateObjectsPropsForm : TprCustomObjectsPropsForm;
begin
Result := TprObjectsPropsForm.Create(Self); 
end;

function TprCustomDesignerPanel.CreateFindForm : TprCustomTemplateFindForm;
begin
Result := TprTemplateFindForm.Create(Self);
end;

procedure TprCustomDesignerPanel.UpdateObjectLinksForm;
begin
if FObjectLinksForm<>nil then
  FObjectLinksForm.UpdateInfo;
end;

procedure TprCustomDesignerPanel.UpdatePosSizeForm;
begin
if FPosSizeForm<>nil then
  FPosSizeForm.UpdateInfo;
end;

procedure TprCustomDesignerPanel.UpdateObjectsPropsForm;
var
  i : integer;
  f : boolean;
  dc : TprDesignComponent;
  ufc : TprUserObjectPropsFormClass;
begin
//
f := VisibleObjectsPropsForm;
ufc := nil;
if (SelCount>0) and (Length(prUserObjectPropsForms)>0) then
  begin
    i := 0;
    dc := SelObjs[0];
    while (i<SelCount) and
          (AnsiCompareText(SelObjs[i].ClassName,dc.ClassName)=0) do Inc(i);
    if i>=SelCount then
      begin
        // find user form
        i := 0;
        while (i<Length(prUserObjectPropsForms)) and not(dc is prUserObjectPropsForms[i].ObjClass) do Inc(i);
        if i<Length(prUserObjectPropsForms) then
          ufc := prUserObjectPropsForms[i].FormClass;
      end;
  end;

if (FUserObjectPropsForm<>nil) and ((ufc=nil) or not(FUserObjectPropsForm is ufc)) then
  begin
    FUserObjectPropsForm.Free;
    FUserObjectPropsForm := nil;
  end;
if (ufc<>nil) and (FUserObjectPropsForm=nil) then
  FUserObjectPropsForm := ufc.Create(Self);

if FUserObjectPropsForm<>nil then
  begin
    FUserObjectPropsForm.UpdateInfo;
    FUserObjectPropsForm.Visible := f;
    if FObjectsPropsForm<>nil then
      FObjectsPropsForm.Visible := false;
  end
else
  if FObjectsPropsForm <> nil then
    FObjectsPropsForm.Visible := f;

if FObjectsPropsForm<>nil then
  FObjectsPropsForm.UpdateInfo;
end;

procedure TprCustomDesignerPanel.UpdateFindForm;
begin
if FFindForm<>nil then
  FFindForm.UpdateInfo;
end;

procedure TprCustomDesignerPanel.OnChangeBandsCaptionsOptions(Sender : TObject);
var
  bc : TprBandCaptions;
begin
if Sender=FHorBandsCaptionsOptions then
  bc := FHorBandsCaptions
else
  bc := FVerBandsCaptions;
with TprBandsCaptionsOptions(Sender) do
  begin
    bc.Visible := Visible;
  end;
AlignChildControls;
end;

procedure TprCustomDesignerPanel.Resize;
begin
inherited;
(*
FTabControl.SetBounds(0,0,ClientWidth,ClientHeight);
FHorBandsCaptions.Repaint; // !!!
FVerBandsCaptions.Repaint; // !!!
*)
end;

procedure TprCustomDesignerPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
FTabControl.SetBounds(0,0,ClientWidth,ClientHeight);
AlignChildControls;
end;

procedure TprCustomDesignerPanel.OnTabControlResize(Sender : TObject);
begin
AlignChildControls;
end;

procedure TprCustomDesignerPanel.OnTabControlChange(Sender : TObject);
begin
ActivePageIndex := FTabControl.TabIndex;
end;

function TprCustomDesignerPanel.HorRulerHeight : integer;
begin
if (FHorRuler<>nil) and FHorRuler.Visible then
  Result := FHorRuler.Height
else
  Result := 0;
end;

function TprCustomDesignerPanel.VerRulerWidth : integer;
begin
if (FVerRuler<>nil) and FVerRuler.Visible then
  Result := FVerRuler.Width
else
  Result := 0;
end;

function TprCustomDesignerPanel.HorBandsCaptionsWidth : integer;
begin
if (FHorBandsCaptions<>nil) and FHorBandsCaptions.Visible then
  Result := FHorBandsCaptions.Width
else
  Result := 0;
end;

function TprCustomDesignerPanel.VerBandsCaptionsHeight : integer;
begin
if (FVerBandsCaptions<>nil) and FVerBandsCaptions.Visible then
  Result := FVerBandsCaptions.Height
else
  Result := 0;
end;

procedure TprCustomDesignerPanel.AlignChildControls;
begin
if HorRulerHeight>0 then
  FHorRuler.SetBounds(FTabControl.DisplayRect.Left+VerRulerWidth+HorBandsCaptionsWidth+2,
                      FTabControl.DisplayRect.Top+VerBandsCaptionsHeight,
                      FTabControl.DisplayRect.Right-FTabControl.DisplayRect.Left-VerRulerWidth-HorBandsCaptionsWidth-2,
                      HorRulerHeight)
else
  if csDesigning in ComponentState then
    FHorRuler.SetBounds(0,-FHorRuler.Height,FHorRuler.Width,FHorRuler.Height);
if VerRulerWidth>0 then
  FVerRuler.SetBounds(FTabControl.DisplayRect.Left+HorBandsCaptionsWidth,
                      FTabControl.DisplayRect.Top+HorRulerHeight+VerBandsCaptionsHeight+2,
                      VerRulerWidth,
                      FTabControl.DisplayRect.Bottom-FTabControl.DisplayRect.Top-HorRulerHeight-VerBandsCaptionsHeight-2)
else
  if csDesigning in ComponentState then
    FVerRuler.SetBounds(-FVerRuler.Width,0,FVerRuler.Width,FVerRuler.Height);
if HorBandsCaptionsWidth>0 then
  FHorBandsCaptions.SetBounds(FTabControl.DisplayRect.Left,
                              FTabControl.DisplayRect.Top+HorRulerHeight+VerBandsCaptionsHeight+2,
                              HorBandsCaptionsWidth,
                              FTabControl.DisplayRect.Bottom-FTabControl.DisplayRect.Top-HorRulerHeight-VerBandsCaptionsHeight-2)
else
  if csDesigning in ComponentState then
    FHorBandsCaptions.SetBounds(0,-FHorBandsCaptions.Height,FHorBandsCaptions.Width,FHorBandsCaptions.Height);
if VerBandsCaptionsHeight>0 then
  FVerBandsCaptions.SetBounds(FTabControl.DisplayRect.Left+VerRulerWidth+HorBandsCaptionsWidth+2,
                              FTabControl.DisplayRect.Top,
                              FTabControl.DisplayRect.Right-FTabControl.DisplayRect.Left-VerRulerWidth-HorBandsCaptionsWidth-2,
                              VerBandsCaptionsHeight)
else
  if csDesigning in ComponentState then
    FVerBandsCaptions.SetBounds(-FVerBandsCaptions.Width,0,FVerBandsCaptions.Width,FVerBandsCaptions.Height);
FDesignerBox.SetBounds(FTabControl.DisplayRect.Left+HorBandsCaptionsWidth+VerRulerWidth,
                       FTabControl.DisplayRect.Top+VerBandsCaptionsHeight+HorRulerHeight,
                       FTabControl.DisplayRect.Right-FTabControl.DisplayRect.Left-HorBandsCaptionsWidth-VerRulerWidth,
                       FTabControl.DisplayRect.Bottom-FTabControl.DisplayRect.Top-VerBandsCaptionsHeight-HorRulerHeight);
end;

procedure TprCustomDesignerPanel.InternalClearSelected;
begin
FSelObjs.Clear;
end;

function TprCustomDesignerPanel.IsObjSelected(dc : TprDesignComponent) : boolean;
begin
Result := FSelObjs.IndexOf(dc)<>-1;
end;

function TprCustomDesignerPanel.GetOnDragOver : TDragOverEvent;
begin
Result := FDesignerBox.OnDragOver;
end;

function TprCustomDesignerPanel.GetOnDragDrop : TDragDropEvent;
begin
Result := FDesignerBox.OnDragDrop;
end;

function TprCustomDesignerPanel.GetOnStartDrag : TStartDragEvent;
begin
Result := FDesignerBox.OnStartDrag;
end;

function TprCustomDesignerPanel.GetOnEndDrag : TEndDragEvent;
begin
Result := FDesignerBox.OnEndDock;
end;

procedure TprCustomDesignerPanel.SetOnDragOver(Value : TDragOverEvent);
begin
FDesignerBox.OnDragOver := Value;
end;

procedure TprCustomDesignerPanel.SetOnDragDrop(Value : TDragDropEvent);
begin
FDesignerBox.OnDragDrop := Value;
end;

procedure TprCustomDesignerPanel.SetOnStartDrag(Value : TStartDragEvent);
begin
FDesignerBox.OnStartDrag := Value;
end;

procedure TprCustomDesignerPanel.SetOnEndDrag(Value : TEndDragEvent);
begin
FDesignerBox.OnEndDrag := Value;
end;

procedure TprCustomDesignerPanel.InternalSelectObj(dc : TprDesignComponent);
begin
if not IsObjSelected(dc) then
  FSelObjs.Add(dc);
end;

procedure TprCustomDesignerPanel.InternalDeSelectObj(dc : TprDesignComponent);
begin
FSelObjs.Remove(dc);
end;

function TprCustomDesignerPanel.GetCurPage : TprCustomPage;
begin
Result := nil;
if (GetReport<>nil) and (FActivePageIndex>=0) and (FActivePageIndex<GetReport.PagesCount) then
  Result := GetReport.Pages[FActivePageIndex];
end;

function TprCustomDesignerPanel.GetShowGrid : boolean;
begin
Result := false;
end;

procedure TprCustomDesignerPanel.SetShowGrid(Value : boolean);
begin
end;

function TprCustomDesignerPanel.GetUseGrid : boolean;
begin
Result := false;
end;

procedure TprCustomDesignerPanel.SetUseGrid(Value : boolean);
begin
end;

function TprCustomDesignerPanel.GetPageTabCaption(Page : TprCustomPage) : string;
begin
if Page.Name<>'' then
  Result := Format('%d (%s)',[Page.IndexInReport+1,Page.Name])
else
  Result := IntToStr(Page.IndexInReport+1);
end;

procedure TprCustomDesignerPanel.UpdateTabControlCaptions;
var
  i : integer;
begin
FTabControl.Tabs.BeginUpdate;
FTabControl.Tabs.Clear;
if GetReport<>nil then
  for i:=0 to GetReport.PagesCount-1 do
    FTabControl.Tabs.Add(GetPageTabCaption(GetReport.Pages[i]));
FTabControl.Tabs.EndUpdate;
end;

procedure TprCustomDesignerPanel.InternalSetReport(Value : TprCustomReport);
begin
if (GetReport<>nil) and (GetReport.PagesCount>0) then
  FActivePageIndex := 0
else
  FActivePageIndex := -1;
FullUpdate;
end;

procedure TprCustomDesignerPanel.SetActivePageIndex(Value : integer);
begin
if FActivePageIndex=Value then exit;
ClearSelection;
FDesignerBox.SetLinksControl(nil);
FActivePageIndex := Value;
FTabControl.TabIndex := Value;
UpdateCurPage;
DoActivePageChanged(Self);
end;

procedure TprCustomDesignerPanel.SetHorBandsCaptionsOptions(Value : TprBandsCaptionsOptions);
begin
FHorBandsCaptionsOptions.Assign(Value);
end;

procedure TprCustomDesignerPanel.SetVerBandsCaptionsOptions(Value : TprBandsCaptionsOptions);
begin
FVerBandsCaptionsOptions.Assign(Value);
end;

function TprCustomDesignerPanel.GetShowPopupMenu : boolean;
begin
Result := FDesignerBox.PopupMenu=FPopupMenu;
end;

procedure TprCustomDesignerPanel.SetShowPopupMenu(Value : boolean);
begin
if Value then
  FDesignerBox.PopupMenu := FPopupMenu
else
  FDesignerBox.PopupMenu := nil;
end;

procedure TprCustomDesignerPanel.DoObjectInserted(InsertedObject : TprObj);
begin
if Assigned(FOnObjectInserted) then
  FOnObjectInserted(Self,InsertedObject);
end;

procedure TprCustomDesignerPanel.DoBandInserted(InsertedBand : TprBand);
begin
if Assigned(FOnBandInserted) then
  FOnBandInserted(Self,InsertedBand);
end;

procedure TprCustomDesignerPanel.ConvertToDesignerCoords(const rSource : TRect; var rDest : TRect);
begin
rDest := rSource;
end;

function TprCustomDesignerPanel.ConvertXToDesignerCoords(X : integer) : integer;
begin
Result := X;
end;

function TprCustomDesignerPanel.ConvertYToDesignerCoords(Y : integer) : integer;
begin
Result := Y;
end;

procedure TprCustomDesignerPanel.ConvertFromDesignerCoords(const rSource : TRect; var rDest : TRect);
begin
rDest := rSource;
end;

function TprCustomDesignerPanel.ConvertXFromDesignerCoords(X : integer) : integer;
begin
Result := X;
end;

function TprCustomDesignerPanel.ConvertYFromDesignerCoords(Y : integer) : integer;
begin
Result := Y;
end;

function TprCustomDesignerPanel.ClientToBandPoint(Band : TprBand; const p : TPoint) : TPoint;
begin
Result.x := ConvertXFromDesignerCoords(p.x+HorScrollBoxPos)-Band.dPageRect.Left;
Result.y := ConvertYFromDesignerCoords(p.y+VerScrollBoxPos)-Band.dPageRect.Top;
end;

procedure TprCustomDesignerPanel.AdjustMouseToGrid(var X,Y : integer);
begin
x := HorScrollBoxPos+X;
y := VerScrollBoxPos+Y;
AdjustToGrid(x,y);
x := x-HorScrollBoxPos;
y := y-VerScrollBoxPos;
end;

procedure TprCustomDesignerPanel.AdjustToGrid(var X,Y : integer);
begin
end;

function TprCustomDesignerPanel.GetVerScrollBoxPos : integer;
begin
Result := FDesignerBox.VertScrollBar.Position;
end;

function TprCustomDesignerPanel.GetHorScrollBoxPos : integer;
begin
Result := FDesignerBox.HorzScrollBar.Position;
end;

procedure TprCustomDesignerPanel.SetVerScrollBoxPos(Value : integer);
begin
FDesignerBox.VertScrollBar.Position := Value;
end;

procedure TprCustomDesignerPanel.SetHorScrollBoxPos(Value : integer);
begin
FDesignerBox.HorzScrollBar.Position := Value;
end;

function TprCustomDesignerPanel.GetScrollBoxPos : TPoint;
begin
Result := Point(FDesignerBox.HorzScrollBar.Position,FDesignerBox.VertScrollBar.Position);
end;

procedure TprCustomDesignerPanel.SetScrollBoxPos(Pos : TPoint);
begin
FDesignerBox.HorzScrollBar.Position := Pos.x;
FDesignerBox.VertScrollBar.Position := Pos.y;
end;

procedure TprCustomDesignerPanel.OnVScroll(Sender : TObject; Msg : TWMScroll);
begin
if HorBandsCaptionsWidth>0 then
  FHorBandsCaptions.Repaint;
if VerRulerWidth>0 then
  FVerRuler.Repaint;
end;

procedure TprCustomDesignerPanel.OnHScroll(Sender : TObject; Msg : TWMScroll);
begin
if VerBandsCaptionsHeight>0 then
  FVerBandsCaptions.Repaint;
if HorRulerHeight>0 then
  FHorRuler.Repaint;
end;

procedure TprCustomDesignerPanel.UpdateCurPage;
var
  I: Integer;
begin
  // check objects in FSelObjs, delete all objects which are not in report
  I := 0;
  while I < FSelObjs.Count do
    if GetReport.ContainsObject(FSelObjs[I]) then
      Inc(I)
    else
      FSelObjs.Delete(I);
  //InternalClearSelected;
  if CurPage<>nil then
    CurPage.UpdateBandsPageRect;
  FHorBandsCaptions.Repaint;
  FVerBandsCaptions.Repaint;
  FHorRuler.Repaint;
  FVerRuler.Repaint;
  FDesignerBox.Repaint;
  UpdateFindForm;
end;

procedure TprCustomDesignerPanel.FullUpdate;
begin
  UpdateTabControlCaptions;
  UpdateCurPage;
end;

function TprCustomDesignerPanel.GetInplaceEditor : TWinControl;
begin
Result := FDesignerBox.FInplaceEditor;
end;

function TprCustomDesignerPanel.GetVisibleObjectLinksForm : boolean;
begin
Result := (FObjectLinksForm<>nil) and FObjectLinksForm.Visible;
end;

function TprCustomDesignerPanel.GetVisiblePosSizeForm : boolean;
begin
Result := (FPosSizeForm<>nil) and FPosSizeForm.Visible;
end;

function TprCustomDesignerPanel.GetVisibleObjectsPropsForm : boolean;
begin
Result := ((FObjectsPropsForm<>nil) and FObjectsPropsForm.Visible) or
          ((FUserObjectPropsForm<>nil) and FUserObjectPropsForm.Visible);
end;

function TprCustomDesignerPanel.GetVisibleFindForm : boolean;
begin
Result := (FFindForm<>nil) and FFindForm.Visible;
end;

procedure TprCustomDesignerPanel.SetVisibleObjectLinksForm(Value : boolean);
begin
if FObjectLinksForm<>nil then
  begin
    FObjectLinksForm.Visible := Value;
    if FObjectLinksForm.Visible then
      FObjectLinksForm.SetFocusOnFirstControl;
  end;
end;

procedure TprCustomDesignerPanel.SetVisiblePosSizeForm(Value : boolean);
begin
if FPosSizeForm<>nil then
  begin
    FPosSizeForm.Visible := Value;
    if FPosSizeForm.Visible then
      FPosSizeForm.SetFocusOnFirstControl;
  end;
end;

procedure TprCustomDesignerPanel.SetVisibleObjectsPropsForm(Value : boolean);
begin
if FUserObjectPropsForm<>nil then
  begin
    FUserObjectPropsForm.Visible := Value
  end
else
  if FObjectsPropsForm<>nil then
    begin
      FObjectsPropsForm.Visible := Value;
      if FObjectsPropsForm.Visible then
        FObjectsPropsForm.SetFocusOnFirstControl;
    end;
end;

procedure TprCustomDesignerPanel.SetVisibleFindForm(Value : boolean);
begin
if FFindForm<>nil then
  begin
    FFindForm.Visible := Value;
    if FFindForm.Visible then
      FFindForm.SetFocusOnFirstControl;
  end;
end;

function TprCustomDesignerPanel.GetOpacityObjectLinksForm : integer;
begin
if FObjectLinksForm<>nil then
  Result := FObjectLinksForm.Opacity
else
  Result := FOpacityObjectLinksForm;
end;

function TprCustomDesignerPanel.GetOpacityPosSizeForm : integer;
begin
if FPosSizeForm<>nil then
  Result := FPosSizeForm.Opacity
else
  Result := FOpacityPosSizeForm;
end;

function TprCustomDesignerPanel.GetOpacityObjectsPropsForm : integer;
begin
if FObjectsPropsForm<>nil then
  Result := FObjectsPropsForm.Opacity
else
  Result := FOpacityObjectsPropsForm;
end;

function TprCustomDesignerPanel.GetOpacityFindForm : integer;
begin
if FFindForm<>nil then
  Result := FFindForm.Opacity
else
  Result := FOpacityFindForm;
end;

procedure TprCustomDesignerPanel.SetOpacityObjectLinksForm(Value : integer);
begin
FOpacityObjectLinksForm := Value;
if FObjectLinksForm<>nil then
  FObjectLinksForm.Opacity := Value
end;

procedure TprCustomDesignerPanel.SetOpacityPosSizeForm(Value : integer);
begin
FOpacityPosSizeForm := Value;
if FPosSizeForm<>nil then
  FPosSizeForm.Opacity := Value;
end;

procedure TprCustomDesignerPanel.SetOpacityObjectsPropsForm(Value : integer);
begin
FOpacityObjectsPropsForm := Value;
if FObjectsPropsForm<>nil then
  FObjectsPropsForm.Opacity := Value;
end;

procedure TprCustomDesignerPanel.SetOpacityFindForm(Value : integer);
begin
FOpacityFindForm := Value;
if FFindForm<>nil then
  FFindForm.Opacity := Value;
end;

function TprCustomDesignerPanel.GetTabPosition : TTabPosition;
begin
Result := FTabControl.TabPosition;
end;

procedure TprCustomDesignerPanel.SetTabPosition(Value : TTabPosition);
begin
FTabControl.TabPosition := Value;
end;

function TprCustomDesignerPanel.GetTabStyle : TTabStyle;
begin
Result := FTabControl.Style;
end;

procedure TprCustomDesignerPanel.SetTabStyle(Value : TTabStyle);
begin
FTabControl.Style := Value;
end;

function TprCustomDesignerPanel.GetSelObject : TprObj;
begin
if (FSelObjs.Count=1) and (SelObjs[0] is TprObj) then
  Result := TprObj(SelObjs[0])
else
  Result := nil;
end;

function TprCustomDesignerPanel.GetSelObj(i : integer) : TprDesignComponent;
begin
Result := TprDesignComponent(FSelObjs[i]);
end;

function TprCustomDesignerPanel.GetSelCount : integer;
begin
Result := FSelObjs.Count;
end;

procedure TprCustomDesignerPanel.InsertObject(ObjClassRef : TprObjClass);
begin
FCurClassRef := ObjClassRef;
end;

function TprCustomDesignerPanel.InsertBand(BandType : TprBandType) : TprBand;
var
  p : TprCustomPage;
  BandClass : TprBandClass;
begin
Result := nil;
BandClass := GetReport.GetBandClass(BandType);
if BandClass<>nil then
  begin
    p := CurPage;
    Result := BandClass.Create(GetReport.prOwner);
    Result.Name := GetValidComponentName(Result);
    try
      if Result.BandType in HorizontalBands then
        TprCustomHBand(Result).Height := GetDefaultHeightForHorizontalBand
      else
        if Result.BandType in VerticalBands then
          TprCustomVBand(Result).Width := GetDefaultWidthForVerticalBand;
      Result.OnInsertIntoPage(p);
      Result.Page := p;
      UpdateCurPage;
      DoBandInserted(Result);
      SelectObject(Result);
      DsgnNotifyReport(true)
    except
      on e : exception do
        begin
          Result.Free;
          Result := nil;
          MBError(e.Message);
        end;
    end;
  end;
end;

procedure TprCustomDesignerPanel.DeleteSelectedObjects;
var
  f : boolean;
  dc : TprDesignComponent;
  i,j,k : integer;
  ClipRgn : HRGN;
begin
if FSelObjs.Count<=0 then exit;
FDesignerBox.SetLinksControl(nil);
FDesignerBox.HideDrawing(FDesignerBox.Canvas.Handle);
i := 0;
f := false;
ClipRgn := 0;
while i<SelCount do
  begin
    dc := SelObjs[i];
    if dc<>nil then
      begin
        if dc is TprObj then
          begin
            AddRectToRegion(ClipRgn,DsgnRSel(i));
            dc.DsgnDelete;
          end
        else
          if dc is TprBand then
            begin
              f := true;
              for j:=0 to TprBand(dc).Objects.Count-1 do
                begin
                  k := FSelObjs.IndexOf(TprBand(dc).Objects[j]);
                  if k<>-1 then
                    FSelObjs[j] := nil; // remove reference to object
                end;
              dc.DsgnDelete;
            end;
      end;
    Inc(i);
  end;
InternalClearSelected;
FDesignerBox.ShowDrawing(FDesignerBox.Canvas.Handle);

if f then
  begin
    // band deleted
    CurPage.UpdateBandsPageRect;
    FDesignerBox.Repaint;
  end
else
  FDesignerBox.InternalPaint(FDesignerBox.Canvas.Handle,ClipRgn);

if ClipRgn<>0 then
  DeleteObject(ClipRgn);
  
DsgnNotifyReport(true);
DoSelectionChanged;
end;

procedure TprCustomDesignerPanel.InplaceEdit(EditedObject : TprObj);
begin
FDesignerBox.InplaceEdit(EditedObject);
DoInplaceEditObject(EditedObject);
end;

procedure TprCustomDesignerPanel.WMFreeInplaceEdit(var Msg : TMessage);
begin
if FDesignerBox.InplaceEditor<>nil then
  begin
    FDesignerBox.FInplaceEditor.Free;
    FDesignerBox.FInplaceEditor := nil;
    FDesignerBox.FInplaceEditedObj := nil;
  end;
SetFocus;
DoEndInplaceEditObject;
end;

procedure TprCustomDesignerPanel.EndInplaceEdit;
begin
PostMessage(Handle,WM_FREEINPLACEEDIT,0,0);
end;

procedure TprCustomDesignerPanel.SaveInplaceEdit;
begin
if FDesignerBox.FInplaceEditedObj<>nil then
  begin
    TprObj(FDesignerBox.FInplaceEditedObj).SaveInplaceEdit(FDesignerBox.FInplaceEditor);
    EndInplaceEdit;
    UpdateObjectsPropsForm;
    DsgnNotifyReport(true);
    DoSaveInplaceEditObject;
  end;
end;

function TprCustomDesignerPanel.AllowInplaceEdit : boolean;
begin
Result := not IsInplaceEdit and (FSelObjs.Count=1) and SelObjs[0].DsgnAllowInplaceEdit;
end;

function TprCustomDesignerPanel.IsInplaceEdit : boolean;
begin
Result := FDesignerBox.FInplaceEditor<>nil;
end;

procedure TprCustomDesignerPanel.GetPointInfoAt(x,y : integer; var dc : TprDesignComponent; var PointInfo : TprPointInfo; var ResizeMode : TprResizeType; var LinkMode : TprLinkType);
begin
FDesignerBox.GetPointInfoAt(x,y,dc,PointInfo,ResizeMode,LinkMode);
end;

procedure TprCustomDesignerPanel.UpdateSelectedObjects;
begin
RepaintSelectedObjects;
UpdateObjectLinksForm;
UpdatePosSizeForm;
UpdateObjectsPropsForm;
UpdateFindForm;
end;

procedure TprCustomDesignerPanel.UpdateObject(dc : TprDesignComponent);
begin
FDesignerBox.RepaintObject(dc);
if IsObjSelected(dc) then
  begin
    UpdateObjectLinksForm;
    UpdatePosSizeForm;
    UpdateObjectsPropsForm;
  end;
end;

procedure TprCustomDesignerPanel.RepaintSelectedObjects;
begin
FDesignerBox.RepaintSelectedObjects;
end;


procedure TprCustomDesignerPanel.SelectedObjectsToClipboard;
var
  i : integer;
  s : string;
  o : TprObj;
  w : TWriter;
  ms : TMemoryStream;
  hMem : THandle;
  pMem : pointer;
begin
// write objects to MemoryStream
ms := TMemoryStream.Create;
try
  w := TWriter.Create(ms,1024);
  w.Root := nil;
  try
    for i:=0 to FSelObjs.Count-1 do
      if SelObjs[i] is TprObj then
        begin
          o := TprObj(SelObjs[i]);
          s := o.Name;
          o.Name := '';
          try
            w.WriteSignature;
            w.WriteComponent(o);
          finally
            o.Name := s;
          end;
        end;
    w.WriteListEnd;
  finally
    w.Free;
  end;

  // write MemoryStream to ClipBoard memory
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
            ClipBoard.SetAsHandle(CF_PROBJ,hMem);
          end;
      end;
  finally
    ClipBoard.Close;
  end;
finally
  ms.Free;
end;
end;

procedure TprCustomDesignerPanel.PasteSetName(Reader: TReader; Component: TComponent; var Name: string);
begin
Name := GetValidComponentName(Component);
end;

procedure TprCustomDesignerPanel.PasteCallBack(Component : TComponent);
begin
with TprObj(Component) do
  begin
    dRec.Left := dRec.Left+1;
    dRec.Top := dRec.Top+1;
    dRec.Right := dRec.Right+1;
    dRec.Bottom := dRec.Bottom+1;
    AfterReportLoaded;
  end;
AddRectToRegion(FPastePRPS.ClipRgn,DsgnR(TprObj(Component)));
Include(FPastePRPS.ClipState,prcsObj);
InternalSelectObj(TprObj(Component));
end;

procedure TprCustomDesignerPanel.Paste;
var
  r : TReader;
  ms : TMemoryStream;
  Band : TprBand;
  hMem : THandle;
  pMem : pointer;
  hSize : DWORD;
begin
Band := nil;
if FSelObjs.Count>0 then
  begin
    if SelObjs[0] is TprBand then
      Band := TprBand(SelObjs[0])
    else
      if SelObjs[0] is TprObj then
        Band := TprObj(SelObjs[0]).Band
  end;
if Band=nil then
  begin
    MBError(prLoadStr(sErrorClipboardPaste));
    exit;
  end;

Clipboard.Open;
try
  hMem := Clipboard.GetAsHandle(CF_PROBJ);
  pMem := GlobalLock(hMem);
  if pMem<>nil then
    begin
      hSize := GlobalSize(hMem);
      ms := TMemoryStream.Create;
      r := TReader.Create(ms,1024);
      try
        ms.Write(pMem^,hSize);
        ms.Seek(soFromBeginning,0);

        r.OnSetName := PasteSetName;
        FDesignerBox._BeginPaint(FPastePRPS);
        InternalClearSelected;
        r.ReadComponents(GetReport,Band,PasteCallback);
        FDesignerBox._EndPaint(FPastePRPS);
        DsgnNotifyReport(true);
        DoSelectionChanged;
      finally
        GlobalUnlock(hMem);
        r.Free;
        ms.Free;
      end;
    end;
finally
  Clipboard.Close;
end;
end;

procedure TprCustomDesignerPanel.Cut;
begin
if not AllowCut then exit;
SelectedObjectsToClipboard;
DeleteSelectedObjects;
end;

procedure TprCustomDesignerPanel.Copy;
begin
if not AllowCopy then exit;
SelectedObjectsToClipboard;
end;

procedure TprCustomDesignerPanel.SendToBack;
var
  o : TprObj;
  i,j : integer;
  prps : rPrPaintStruct;
begin
FDesignerBox._BeginPaint(prps);
for i:=0 to SelCount-1 do
  if SelObjs[i] is TprObj then
    begin
      o := TprObj(SelObjs[i]);
      j := o.Band.Objects.IndexOf(o);
      if j<>-1 then
        begin
          o.Band.Objects.Insert(0,o);
          o.Band.Objects.Delete(j+1);
        end;
      AddRectToRegion(prps.ClipRgn,DsgnRSel(i));
      Include(prps.ClipState,prcsObj);
    end;
FDesignerBox._EndPaint(prps);
end;

procedure TprCustomDesignerPanel.BringToFront;
var
  o : TprObj;
  i,j : integer;
  prps : rPrPaintStruct;
begin
FDesignerBox._BeginPaint(prps);
for i:=0 to SelCount-1 do
  if SelObjs[i] is TprObj then
    begin
      o := TprObj(SelObjs[i]);
      j := o.Band.Objects.IndexOf(o);
      if j<>-1 then
        begin
          o.Band.Objects.Add(o);
          o.Band.Objects.Delete(j);
        end;
      AddRectToRegion(prps.ClipRgn,DsgnRSel(i));
      Include(prps.ClipState,prcsObj);
    end;
FDesignerBox._EndPaint(prps);
end;

procedure TprCustomDesignerPanel.Size(sx,sy : integer);
var
  i : integer;
  r : TRect;
  dc : TprDesignComponent;
  prps : rPrPaintStruct;
begin
FDesignerBox._BeginPaint(prps);
for i:=0 to FSelObjs.Count-1 do
  begin
    dc := SelObjs[i];
    r := DsgnRSel(i);
    if ((sx=0) or
        (([ppRight,ppRightTop,ppRightBottom] * dc.DsgnAllowResizeTypes)<>[])) and
       ((sy=0) or
        (([ppBottom,ppLeftBottom,ppRightBottom] * dc.DsgnAllowResizeTypes)<>[])) and
       (r.Bottom-r.Top+sy>0) and
       (r.Right-r.Left+sx>0) then
      begin
        FDesignerBox.DoResize(dc,0,0,ConvertYToDesignerCoords(sy),ConvertYToDesignerCoords(sx),@prps);
      end
  end;
FDesignerBox._EndPaint(prps);
end;

procedure TprCustomDesignerPanel.Nudge(dx,dy : integer);
var
  i : integer;
  dc : TprDesignComponent;
  prps : rPrPaintStruct;
begin
dx := ConvertXToDesignerCoords(dx);
dy := ConvertYToDesignerCoords(dy);
FDesignerBox._BeginPaint(prps);
for i:=0 to FSelObjs.Count-1 do
  begin
    dc := SelObjs[i];
    if dc.DsgnAllowDrag then
      FDesignerBox.DoDrag(dc,dx,dy,@prps);
  end;
FDesignerBox._EndPaint(prps);
end;

procedure TprCustomDesignerPanel.SetPosSizeProp(Prop : TprObjectPosSizeProps; Value : integer);
var
  i : integer;
  r : TRect;
  dc : TprDesignComponent;
  prps : rPrPaintStruct;
begin
if prop in [prpsaLeft,prpsaRight,prpsaWidth] then
  Value := ConvertXtoDesignerCoords(Value)
else
  Value := ConvertYtoDesignerCoords(Value);

FDesignerBox._BeginPaint(prps);
for i:=0 to SelCount-1 do
  begin
    dc := SelObjs[i];
    r := ObjRSel(i);
    if (Prop in [prpsaLeft,prpsaRight,prpsaTop,prpsaBottom]) and dc.DsgnAllowDrag then
      begin
        // nudge
        case Prop of
          prpsaLeft : FDesignerBox.DoDrag(dc,Value-r.Left,0,@prps);
          prpsaTop : FDesignerBox.DoDrag(dc,0,Value-r.Top,@prps);
          prpsaRight : FDesignerBox.DoDrag(dc,Value-r.Right,0,@prps);
          prpsaBottom : FDesignerBox.DoDrag(dc,0,Value-r.Bottom,@prps);
        end;
      end
    else
      begin
        if (Prop in [prpsaWidth,prpsaHeight]) then
          begin
            // resize
            case Prop of
              prpsaWidth : FDesignerBox.DoResize(dc,0,0,0,Value-r.Right+r.Left,@prps);
              prpsaHeight : FDesignerBox.DoResize(dc,0,0,Value-r.Bottom+r.Top,0,@prps);
            end;
          end;
      end;
  end;
FDesignerBox._EndPaint(prps);
end;

function TprCustomDesignerPanel.AlignActionAllowed(ActionCode : TprAlignActionCode) : boolean;
begin
Result := false;
if IsInplaceEdit then exit;
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
  aacWToLarge : Result := GetNumResizeSelectedRegions([ppRight,ppRightTop,ppRightBottom])>=2;
  aacHToSmall,
  aacHToLarge : Result := GetNumResizeSelectedRegions([ppBottom,ppLeftBottom,ppRightBottom])>=2;
  aacAlignToGridLeftTop : Result := GetNumDragSelectedRegions>=1;
  aacAlignToGridAll : Result := AllowAlignToGridAll;
  aacHSpaceEqually,
  aacVSpaceEqually : Result := GetNumDragSelectedRegions>=3;
  else Result := false;
end;
end;

procedure TprCustomDesignerPanel.AlignAction(ActionCode : TprAlignActionCode);
var
  l : TList;
  r : TRect;
  prps : rPrPaintStruct;
  dcLeft,dcRight,dcTop,dcBottom : TprDesignComponent;
  i,Width,Delta,Right,Left,Height,Bottom,Top,j,Min,iMin,Cur,Minw,Minh,Maxw,Maxh : integer;

  function GetR(dc : TObject) : TRect;
  begin
  Result := DsgnR(TprDesignComponent(dc));
  end;

begin
FDesignerBox._BeginPaint(prps);
case ActionCode of
  aacHToLeft:
    begin
      for i:=1 to SelCount-1 do
        if SelObjs[i].DsgnAllowDrag then
          FDesignerBox.DoDrag(SelObjs[i],
                              DsgnRSel(0).Left-DsgnRSel(i).Left,
                              0,
                              @prps);
    end;
  aacHToRight:
    begin
      for i:=1 to SelCount-1 do
        if SelObjs[i].DsgnAllowDrag then
          FDesignerBox.DoDrag(SelObjs[i],
                              DsgnRSel(0).Right-DsgnRSel(i).Right,
                              0,
                              @prps);
    end;
  aacVToTop:
    begin
      for i:=1 to SelCount-1 do
        if SelObjs[i].DsgnAllowDrag then
          FDesignerBox.DoDrag(SelObjs[i],
                              0,
                              DsgnRSel(0).Top-DsgnRSel(i).Top,
                              @prps);
    end;
  aacVToBottom:
    begin
      for i:=1 to SelCount-1 do
        if SelObjs[i].DsgnAllowDrag then
          FDesignerBox.DoDrag(SelObjs[i],
                              0,
                              DsgnRSel(0).Bottom-DsgnRSel(i).Bottom,
                              @prps);
    end;
  aacVCenters:
    begin
      for i:=1 to SelCount-1 do
        if SelObjs[i].DsgnAllowDrag then
          FDesignerBox.DoDrag(SelObjs[i],
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
        if SelObjs[i].DsgnAllowDrag then
          FDesignerBox.DoDrag(SelObjs[i],
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
        if (SelObjs[i] is TprObj) and SelObjs[i].DsgnAllowDrag then
          begin
            r := DsgnR(TprObj(SelObjs[i]).Band);
            FDesignerBox.DoDrag(SelObjs[i],
                                (r.Right-r.Left-DsgnRSel(i).Right+DsgnRSel(i).Left) div 2-DsgnRSel(i).Left+r.Left,
                                0,
                                @prps);
          end;
    end;
  aacVCenterInWindow:
    begin
      for i:=0 to SelCount-1 do
        if (SelObjs[i] is TprObj) and SelObjs[i].DsgnAllowDrag then
          begin
            r := DsgnR(TprObj(SelObjs[i]).Band);
            FDesignerBox.DoDrag(SelObjs[i],
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
        FDesignerBox.DoResize(SelObjs[i],
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
        FDesignerBox.DoResize(SelObjs[i],
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
        FDesignerBox.DoResize(SelObjs[i],
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
        FDesignerBox.DoResize(SelObjs[i],
                              0,
                              0,
                              Maxh-DsgnHSel(i),
                              0,
                              @prps);
    end;
  aacAlignToGridLeftTop:
    begin
      for i:=0 to SelCount-1 do
        if SelObjs[i].DsgnAllowDrag then
          begin
            Left := DsgnRSel(i).Left;
            Top := DsgnRSel(i).Top;
            AdjustMouseToGrid(Left,Top);
            FDesignerBox.DoDrag(SelObjs[i],
                                Left-DsgnRSel(i).Left,
                                Top-DsgnRSel(i).Top,
                                @prps);
          end;
    end;
  aacAlignToGridAll:
    begin
      for i:=0 to FSelObjs.Count-1 do
        begin
          if SelObjs[i].DsgnAllowDrag then
            begin
              Left := DsgnRSel(i).Left;
              Top := DsgnRSel(i).Top;
              AdjustMouseToGrid(Left,Top);
              FDesignerBox.DoDrag(SelObjs[i],
                                  Left-DsgnRSel(i).Left,
                                  Top-DsgnRSel(i).Top,
                                  @prps);
            end;

          if ((([ppRight,ppRightTop,ppRightBottom] * SelObjs[i].DsgnAllowResizeTypes)<>[]) or
              (([ppBottom,ppLeftBottom,ppRightBottom] * SelObjs[i].DsgnAllowResizeTypes)<>[])) then
            begin
              Right := DsgnRSel(i).Right;
              Bottom := DsgnRSel(i).Bottom;
              AdjustMouseToGrid(Right,Bottom);

              FDesignerBox.DoResize(SelObjs[i],
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
        for i:=0 to FSelObjs.Count-1 do
          if SelObjs[i].DsgnAllowDrag then
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
        dcLeft := TprDesignComponent(l[0]);
        Left := DsgnR(dcLeft).Left;

        // find max Right
        dcRight := TprDesignComponent(l[0]);
        Right := DsgnR(dcRight).Right;
        for i:=1 to l.Count-1 do
          begin
            if Right<GetR(l[i]).Right then
              begin
                dcRight := TprDesignComponent(l[i]);
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
              FDesignerBox.DoDrag(TprDesignComponent(l[i]),
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
        for i:=0 to FSelObjs.Count-1 do
          if SelObjs[i].DsgnAllowDrag then
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
        dcTop := TprDesignComponent(l[0]);
        Top := DsgnR(dcTop).Top;

        // find max Bottom
        dcBottom := TprDesignComponent(l[0]);
        Bottom := DsgnR(dcBottom).Bottom;
        for i:=1 to l.Count-1 do
          begin
            if Bottom<GetR(l[i]).Bottom then
              begin
                dcBottom := TprDesignComponent(l[i]);
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
              FDesignerBox.DoDrag(TprDesignComponent(l[i]),
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
FDesignerBox._EndPaint(prps);
end;

function TprCustomDesignerPanel.AllowAlignToGridAll;
var
  i : integer;
begin
i:=0;
while (i<FSelObjs.Count) and (not SelObjs[i].DsgnAllowDrag) do Inc(i);
if i>=FSelObjs.Count then
  begin
    i := 0;
    while (i<FSelObjs.Count) and
          (([ppBottom,ppLeftBottom,ppRightBottom] * SelObjs[i].DsgnAllowResizeTypes)=[]) and
          (([ppRight,ppRightTop,ppRightBottom] * SelObjs[i].DsgnAllowResizeTypes)=[]) do Inc(i);
    Result := i<FSelObjs.Count;
  end
else
  Result := true;
end;

function TprCustomDesignerPanel.AllowDelete;
begin
Result := not IsInplaceEdit and (FSelObjs.Count>0);
end;

function TprCustomDesignerPanel.GetNumDragSelectedRegions;
var
  i : integer;
begin
Result := 0;
for i:=0 to FSelObjs.Count-1 do
  if SelObjs[i].DsgnAllowDrag then
    Inc(Result);
end;

function TprCustomDesignerPanel.GetNumResizeSelectedRegions(ResizeTypesSet : TprResizeTypeSet) : integer;
var
  i : integer;
begin
Result := 0;
for i:=0 to FSelObjs.Count-1 do
  if SelObjs[i].DsgnAllowResizeTypes*ResizeTypesSet<>[] then
    Inc(Result);
end;

function TprCustomDesignerPanel.IsAnyTprObjSelected : boolean;
var
  i : integer;
begin
i:=0;
while (i<FSelObjs.Count) and not (SelObjs[i] is TprObj) do Inc(i);
Result := i<FSelObjs.Count;
end;

function TprCustomDesignerPanel.AllowCopy : boolean;
begin
Result := not IsInplaceEdit and IsAnyTprObjSelected;
end;

function TprCustomDesignerPanel.AllowCut : boolean;
begin
Result := not IsInplaceEdit and IsAnyTprObjSelected;
end;

function TprCustomDesignerPanel.AllowPaste : boolean;
begin
Result := not IsInplaceEdit and Clipboard.HasFormat(CF_PROBJ);
end;

procedure TprCustomDesignerPanel.ClearReportTemplate;
begin
if GetReport=nil then exit;
InternalClearSelected;
GetReport.ClearTemplate;
ActivePageIndex := -1;
end;

procedure TprCustomDesignerPanel.DeleteBandLink(Band : TprBand; LinkedObj : TprObj);
begin
FDesignerBox.HideDrawing(FDesignerBox.Canvas.Handle);
Band.ResizeObjs.Remove(LinkedObj);
FDesignerBox.ShowDrawing(FDesignerBox.Canvas.Handle);
UpdateObjectLinksForm;
DsgnNotifyReport(true);
end;

procedure TprCustomDesignerPanel.DeleteObjLink(Obj : TprObj; LinkType : TprLinkType; LinkedObj : TprObj);
begin
FDesignerBox.HideDrawing(FDesignerBox.Canvas.Handle);
case LinkType of
  ltLeft : Obj.LeftObjs.Remove(LinkedObj);
  ltTop : Obj.TopObjs.Remove(LinkedObj);
  ltRight : Obj.WidthObjs.Remove(LinkedObj);
  ltBottom : Obj.HeightObjs.Remove(LinkedObj);
end;
FDesignerBox.ShowDrawing(FDesignerBox.Canvas.Handle);
UpdateObjectLinksForm;
DsgnNotifyReport(true);
end;

procedure TprCustomDesignerPanel.MakeLinkFromObject(dc : TprDesignComponent; LinkMode : TprLinkType);
begin
  FDesignerBox.MakeLinkFromObject(dc,LinkMode);
end;

procedure TprCustomDesignerPanel.InsertPageAfter(AfterPageIndex : integer);
var
  Page : TprCustomPage;
begin
Page := GetReport.InsertPage(AfterPageIndex+1);
Page.Name := GetValidComponentName(Page);

UpdateTabControlCaptions;
ActivePageIndex := Page.IndexInReport;

DsgnNotifyReport(true);
end;

procedure TprCustomDesignerPanel.DeletePage(PageIndex : integer);
begin
if (GetReport.PagesCount=0) or (PageIndex<0) or (PageIndex>=GetReport.PagesCount) then exit;
if PageIndex=ActivePageIndex then
  begin
    FDisablePageDraw := true;
    InternalClearSelected;
    CurPage.Free;
    FDisablePageDraw := false;
    if FActivePageIndex=GetReport.PagesCount then
      Dec(FActivePageIndex);
    UpdateCurPage;
  end
else
  begin
    if FActivePageIndex>PageIndex then
      Dec(FActivePageIndex);
    GetReport.Pages[PageIndex].Free;
  end;
UpdateTabControlCaptions;
FTabControl.TabIndex := ActivePageIndex;
DsgnNotifyReport(true);
DoActivePageChanged(Self);
end;

function TprCustomDesignerPanel.EditPage(PageIndex : integer) : boolean;
begin
Result := false;
end;

procedure TprCustomDesignerPanel.NextPage;
begin
if FActivePageIndex=GetReport.PagesCount-1 then
  ActivePageIndex := 0
else
  ActivePageIndex := ActivePageIndex+1;
end;

procedure TprCustomDesignerPanel.PriorPage;
begin
if FActivePageIndex=0 then
  ActivePageIndex := GetReport.PagesCount-1
else
  ActivePageIndex := ActivePageIndex-1;
end;

function TprCustomDesignerPanel.GetSelectedRect : TRect;
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

procedure TprCustomDesignerPanel.UpdateSelectedObjectsRect;
begin
FSelectedObjectsRect := GetSelectedRect;
OffsetRect(FSelectedObjectsRect,HorScrollBoxPos,VerScrollBoxPos);
ConvertFromDesignerCoords(FSelectedObjectsRect,FSelectedObjectsRect);
end;

procedure TprCustomDesignerPanel.DoSelectionChanged;
begin
UpdateSelectedObjectsRect;
FHorBandsCaptions.Repaint;
FVerBandsCaptions.Repaint;
UpdateObjectLinksForm;
UpdatePosSizeForm;
UpdateObjectsPropsForm;
UpdateFindForm;
if Assigned(FOnSelectionChanged) then
  FOnSelectionChanged(Self,SelCount);
end;

procedure TprCustomDesignerPanel.DoLinkAdded(SourceObject : TprDesignComponent; DestObject : TprDesignComponent; LinkMode : TprLinkType);
begin
UpdateObjectLinksForm;
if Assigned(FOnLinkAdded) then
  FOnLinkAdded(Self,SourceObject,DestObject,LinkMode);
end;

procedure TprCustomDesignerPanel.DoObjectResized(ResizeObject : TprDesignComponent);
begin
UpdateSelectedObjectsRect;
if ResizeObject is TprCustomHBand then
  FHorBandsCaptions.Repaint
else
  if ResizeObject is TprCustomVBand then
    FVerBandsCaptions.Repaint;
if Assigned(FOnObjectResized) then
  FOnObjectResized(Self,ResizeObject);
end;

procedure TprCustomDesignerPanel.DoObjectDrag(DragObject : TprDesignComponent);
begin
UpdateSelectedObjectsRect;
if Assigned(FOnObjectDrag) then
  FOnObjectDrag(Self,DragObject);
end;

procedure TprCustomDesignerPanel.DoInplaceEditObject(EditedObject : TprObj);
begin
if Assigned(FOnInplaceEditObject) then
  FOnInplaceEditObject(Self,EditedObject);
end;

procedure TprCustomDesignerPanel.DoEndInplaceEditObject;
begin
if Assigned(FOnEndInplaceEditObject) then
  FOnEndInplaceEditObject(Self);
end;

procedure TprCustomDesignerPanel.DoSaveInplaceEditObject;
begin
if Assigned(FOnSaveInplaceEditObject) then
  FOnSaveInplaceEditObject(Self);
end;

procedure TprCustomDesignerPanel.DoActivePageChanged(Sender : TObject);
begin
if Assigned(FOnActivePageChanged) then
  FOnActivePageChanged(Self,ActivePageIndex,CurPage);
end;

procedure TprCustomDesignerPanel.DoApplyObjectsProps;
begin
FDesignerBox.RepaintSelectedObjects;
if Assigned(FOnApplyObjectsProps) then
  FOnApplyObjectsProps(Self);
end;

procedure TprCustomDesignerPanel.DoWhileObjectResize(const ResizeRect : TRect);
begin
if Assigned(FOnWhileObjectResize) then
  FOnWhileObjectResize(Self,ResizeRect);
end;

procedure TprCustomDesignerPanel.DoWhileObjectDrag(const DragRect : TRect);
begin
if Assigned(FOnWhileObjectDrag) then
  FOnWhileObjectDrag(Self,DragRect);
end;

procedure TprCustomDesignerPanel.DoDesignerMouseMove(Shift : TShiftState; X,Y : integer);
begin
if Assigned(FOnDesignerMouseMove) then
  FOnDesignerMouseMove(Self,Shift,X,Y);
end;

procedure TprCustomDesignerPanel.DoDesignerMouseDown(Button: TMouseButton; Shift : TShiftState; X,Y : integer; var Processed : boolean);
begin
Processed := false;
if Assigned(FOnDesignerMouseDown) then
  FOnDesignerMouseDown(Self,Button,Shift,X,Y,Processed);
end;

procedure TprCustomDesignerPanel.DoDesignerMouseUp(Button: TMouseButton; Shift : TShiftState; X,Y : integer);
begin
if Assigned(FOnDesignerMouseUp) then
  FOnDesignerMouseUp(Self,Button,Shift,X,Y);
end;

procedure TprCustomDesignerPanel.DoDesignerDblClick(var Processed : boolean);
var
  p : TPoint;
  Shift : TShiftState;
begin
GetCursorPos(p);
p := FDesignerBox.ScreenToClient(p);
Shift := [];
if GetKeyState(VK_SHIFT) < 0 then Include(Shift,ssShift);
if GetKeyState(VK_CONTROL) < 0 then Include(Shift,ssCtrl);
if GetKeyState(VK_MENU) < 0 then Include(Shift,ssAlt);
Processed := false;
if Assigned(FOnDesignerDblClick) then
  FOnDesignerDblClick(Self,p.x,p.y,Shift,Processed);
end;

procedure TprCustomDesignerPanel.DoInsertingObjectChanged;
begin
if Assigned(FOnInsertingObjectChanged) then
  FOnInsertingObjectChanged(Self);
end;

procedure TprCustomDesignerPanel.DoOnFileNameChanged;
begin
if Assigned(FOnFileNameChanged) then
  FOnFileNameChanged(Self); 
end;

procedure TprCustomDesignerPanel.DoOnOpenTemplate(var Stream : TStream; var CancelOpen : boolean; var IsBinaryFormat : boolean);
begin
Stream := nil;
CancelOpen := false;
IsBinaryFormat := false;
if Assigned(FOnOpenTemplate) then
  FOnOpenTemplate(Self,Stream,CancelOpen,IsBinaryFormat);
end;

procedure TprCustomDesignerPanel.DoOnSaveTemplate(var Stream : TStream; var CancelSave : boolean; var IsBinaryFormat : boolean; IsSaveAs : boolean);
begin
Stream := nil;
CancelSave := false;
IsBinaryFormat := false;
if Assigned(FOnSaveTemplate) then
  FOnSaveTemplate(Self,Stream,CancelSave,IsBinaryFormat,IsSaveAs);
end;

procedure TprCustomDesignerPanel.SelectObject(dc : TprDesignComponent);
begin
with FDesignerBox do
  begin
    HideDrawing(Canvas.Handle);
    InternalClearSelected;
    InternalSelectObj(dc);
    ShowDrawing(Canvas.Handle);
    DoSelectionChanged;
  end;
end;

procedure TprCustomDesignerPanel.AddSelectObject(dc : TprDesignComponent);
begin
with FDesignerBox do
  begin
    HideDrawing(Canvas.Handle);
    InternalSelectObj(dc);
    ShowDrawing(Canvas.Handle);
    DoSelectionChanged;
  end;
end;

procedure TprCustomDesignerPanel.DeSelectObject(dc : TprDesignComponent);
begin
with FDesignerBox do
  begin
    HideDrawing(Canvas.Handle);
    InternalDeSelectObj(dc);
    ShowDrawing(Canvas.Handle);
    DoSelectionChanged;
  end;
end;

procedure TprCustomDesignerPanel.ClearSelection;
begin
with FDesignerBox do
  begin
    HideDrawing(Canvas.Handle);
    InternalClearSelected;
    ShowDrawing(Canvas.Handle);
    DoSelectionChanged;
  end;
end;

procedure TprCustomDesignerPanel.MakeObjectVisible(dc : TprDesignComponent);
var
  Page : TprCustomPage;
begin
if dc is TprObj then
  Page := TprObj(dc).Band.Page
else
  if dc is TprBand then
    Page := TprBand(dc).Page
  else
    exit;
//
ActivePageIndex := Page.IndexInReport;
FDesignerBox.MakeObjectVisible(dc);
end;

procedure TprCustomDesignerPanel.WMSetFocus(var Msg : TWMSetFocus);
begin
inherited;
FDesignerBox.SetFocus;
end;

procedure TprCustomDesignerPanel.WriteToIni(IniFile : TIniFile; const SectionName : string);
begin
HorBandsCaptionsOptions.WriteToIni(IniFile,SectionName,'HorBandsCaptionsOptions');
VerBandsCaptionsOptions.WriteToIni(IniFile,SectionName,'VerBandsCaptionsOptions');

IniFile.WriteInteger(SectionName,'OpacityObjectsPropsForm',OpacityObjectsPropsForm);
IniFile.WriteInteger(SectionName,'OpacityObjectLinksForm',OpacityObjectLinksForm);
IniFile.WriteInteger(SectionName,'OpacityPosSizeForm',OpacityPosSizeForm);
IniFile.WriteInteger(SectionName,'OpacityFindForm',OpacityFindForm);
end;

procedure TprCustomDesignerPanel.ReadFromIni(IniFile : TIniFile; const SectionName : string);
begin
HorBandsCaptionsOptions.ReadFromIni(IniFile,SectionName,'HorBandsCaptionsOptions');
VerBandsCaptionsOptions.ReadFromIni(IniFile,SectionName,'VerBandsCaptionsOptions');

OpacityObjectsPropsForm := IniFile.ReadInteger(SectionName,'OpacityObjectsPropsForm',OpacityObjectsPropsForm);
OpacityObjectLinksForm := IniFile.ReadInteger(SectionName,'OpacityObjectLinksForm',OpacityObjectLinksForm);
OpacityPosSizeForm := IniFile.ReadInteger(SectionName,'OpacityPosSizeForm',OpacityPosSizeForm);
OpacityFindForm := IniFile.ReadInteger(SectionName,'OpacityFindForm',OpacityFindForm);
end;

function TprCustomDesignerPanel.EditOptions : boolean;
begin
Result := false;
end;

procedure TprCustomDesignerPanel.SetFileName(Value : string);
begin
if FFileName=Value then exit;
FFileName := Value;
DoOnFileNameChanged;
end;

procedure TprCustomDesignerPanel.New;
begin
ClearReportTemplate;
InsertPageAfter(-1);
ScrollBoxPos := Point(0,0);
FileName := '';
end;

procedure TprCustomDesignerPanel.Open;
var
  od : TOpenDialog;
  Stream : TStream;
  CancelOpen,IsBinaryFormat : boolean;
begin
DoOnOpenTemplate(Stream,CancelOpen,IsBinaryFormat);
if CancelOpen then exit;
if Stream=nil then
  begin
    od := TOpenDialog.Create(nil);
    try
      od.Filter := prLoadStr(prSaveReportTemplateFilter);
      od.DefaultExt := 'prt';
      if ExtractFileName(FileName)='' then
        od.InitialDir := ExtractFilePath(FileName)
      else
        od.FileName := FileName;
      od.Options := [ofHideReadOnly,ofPathMustExist,ofFileMustExist,ofEnableSizing];
      if od.Execute then
        begin
          Stream := TFileStream.Create(od.FileName,fmOpenRead or fmShareDenyWrite);
          FileName := od.FileName;
        end;
    finally
      od.Free;
    end;
  end;
if Stream=nil then exit;

try
  InternalClearSelected;
  GetReport.LoadTemplate(Stream,IsBinaryFormat);
  ScrollBoxPos := Point(0,0);
finally
  Stream.Free;
end;
end;

procedure TprCustomDesignerPanel.Save;
var
  sd : TSaveDialog;
  Stream : TStream;
  CancelSave,IsBinaryFormat : boolean;
begin
DoOnSaveTemplate(Stream,CancelSave,IsBinaryFormat,false);
if CancelSave then exit;
if Stream=nil then
  begin
    if FileName='' then
      begin
        sd := TSaveDialog.Create(Self);
        try
          sd.Filter := prLoadStr(prSaveReportTemplateFilter);
          sd.DefaultExt := 'prt';
          if ExtractFileName(FileName)='' then
            sd.InitialDir := ExtractFilePath(FileName)
          else
            sd.FileName := FileName;
          sd.Options := [ofHideReadOnly,ofPathMustExist,ofEnableSizing];
          if sd.Execute then
            begin
              Stream := TFileStream.Create(sd.FileName,fmCreate or fmShareDenyWrite);
              FileName := sd.FileName;
            end;
        finally
          sd.Free;
        end;
      end
    else
      Stream := TFileStream.Create(FileName,fmCreate or fmShareDenyWrite);
  end;
if Stream=nil then exit;
try
  GetReport.SaveTemplate(Stream,IsBinaryFormat)
finally
  Stream.Free;
end;
end;

procedure TprCustomDesignerPanel.SaveAs;
var
  sd : TSaveDialog;
  Stream : TStream;
  CancelSave,IsBinaryFormat : boolean;
begin
DoOnSaveTemplate(Stream,CancelSave,IsBinaryFormat,true);
if CancelSave then exit;
if Stream=nil then
  begin
    sd := TSaveDialog.Create(Self);
    try
      sd.Filter := prLoadStr(prSaveReportTemplateFilter);
      sd.DefaultExt := 'prt';
      if ExtractFileName(FileName)='' then
        sd.InitialDir := ExtractFilePath(FileName)
      else
        sd.FileName := FileName;
      sd.Options := [ofHideReadOnly,ofPathMustExist,ofEnableSizing];
      if sd.Execute then
        begin
          Stream := TFileStream.Create(sd.FileName,fmCreate or fmShareDenyWrite);
          FileName := sd.FileName;
        end;
    finally
      sd.Free;
    end;
  end;
if Stream=nil then exit;
try
  GetReport.SaveTemplate(Stream,IsBinaryFormat)
finally
  Stream.Free;
end;
end;

procedure TprCustomDesignerPanel.Print;
begin
with GetReport do
  if PrepareReport then
    if SetupPrintParams then
      PrintPreparedReport;
end;

procedure TprCustomDesignerPanel.Preview;
begin
with GetReport do
  if PrepareReport then
    begin
      if PreviewFormMode=fmMDIChild then
        PreviewPreparedReport(false)
      else
        PreviewPreparedReport((GetParentForm(Self)=nil) or (fsModal in GetParentForm(Self).FormState))
    end;
end;

procedure TprCustomDesignerPanel.EditReportParams;
begin
end;

procedure TprCustomDesignerPanel.EditGroups;
begin
TprGroupsEditorForm.Create(Self).EditGroups(GetReport);
end;

procedure TprCustomDesignerPanel.EditValues;
begin
TprValuesEditorForm.Create(Self).EditValues(GetReport);
end;

procedure TprCustomDesignerPanel.EditVariables;
begin
TprVariablesEditorForm.Create(Self).EditVariables(GetReport,nil);
end;

var
  i : integer;
  Cur : HCURSOR;

initialization

for i:=integer(Low(TprBandType)) to integer(High(TprBandType)) do
  begin
    BandImages[TprBandType(i)] := TBitmap.Create;
    LoadResImageDef(BandImages[TprBandType(i)],GetEnumName(TypeInfo(TprBandType),i),'TPRDEFAULTOBJRESNAME');
    BandImages[TprBandType(i)].Transparent := true;
    BandImages[TprBandType(i)].TransparentMode := tmAuto;
  end;

Cur := LoadImage(hInstance,'PR_LINKACCEPTED',IMAGE_CURSOR,0,0,LR_DEFAULTSIZE or LR_DEFAULTCOLOR);
if Cur=0 then
  FLinkAcceptedCursor := crHandPoint
else
  begin
    Screen.Cursors[crDesignerPanelLinkAccepted] := Cur;
    FLinkAcceptedCursor := crDesignerPanelLinkAccepted;
  end;

Cur := LoadImage(hInstance,'PR_LINKNOTACCEPTED',IMAGE_CURSOR,0,0,LR_DEFAULTSIZE or LR_DEFAULTCOLOR);
if Cur=0 then
  FLinkNotAcceptedCursor := crHandPoint
else
  begin
    Screen.Cursors[crDesignerPanelLinkNotAccepted] := Cur;
    FLinkNotAcceptedCursor := crDesignerPanelLinkNotAccepted;
  end;

finalization

i := integer(Low(TprBandType));
while i<=integer(High(TprBandType)) do
  begin
    BandImages[TprBandType(i)].Free;
    Inc(i);
  end;

end.
