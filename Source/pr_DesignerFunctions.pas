{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the some functions and constants that is used by designers.}
unit pr_DesignerFunctions;

{$i pr.inc}

interface

uses
  Windows, Menus, SysUtils, Classes, StdCtrls, comctrls, controls, forms,
  inifiles, typinfo, graphics, 
  {$ifdef PR_D6_D7} Variants, {$endif}

  pr_Common, pr_MultiLang, pr_Utils, vgr_ColorButton;

type
{Describes when objects can be stuck.
Items:
  prsmDisabled - Objects can not be stuck.
  prsmAlways - Objects can be stuck always.
  prsmCtrlButton - Objects can be stuck only when Ctrl button is pressed.
Syntax:
  TprStuckMode = (prsmDisabled, prsmAlways, prsmCtrlButton);}
  TprStuckMode = (prsmDisabled, prsmAlways, prsmCtrlButton);

{Describes the "stuck" options for TprReport designer.
Items:
  prsoStuckToBands - Objects can be stuck to the edges of bands.
  prsoStuckToObjects - Objects can be stuck to the edges of another objects.
  prsoStuckOver - Objects' edges are overlapped on one pixel when objects are stuck.
Syntax:
  TprStuckOptions = (prsoStuckToBands, prsoStuckToObjects, prsoStuckOver);}
  TprStuckOptions = (prsoStuckToBands, prsoStuckToObjects, prsoStuckOver);

{Describes the "stuck" options for TprReport designer.
Syntax:
  TprStuckOptionsSet = set of TprStuckOptions;
See also:
  TprStuckOptions}
  TprStuckOptionsSet = set of TprStuckOptions;

{Describes the edges of objects which can be stuck.
Items:
  prassLeft - The left edge of object can be stuck.
  prassTop - The top edge of object can be stuck.
  prassRight - The right edge of object can be stuck.
  prassBottom - The bottom edge of object can be stuck.
Syntax:
  TprAllowStuckSides = (prassLeft, prassTop, prassRight, prassBottom);}
  TprAllowStuckSides = (prassLeft, prassTop, prassRight, prassBottom);

{Describes the edges of objects which can be stuck.
Syntax:
  TprAllowStuckSidesSet = set of TprAllowStuckSides;
See also:
  TprAllowStuckSides}
  TprAllowStuckSidesSet = set of TprAllowStuckSides;

{Describes the "stuck" options for TprReport preview.
Items:
  prpsoStuckOver - Objects' edges are overlapped on one pixel when objects are stuck.
Syntax:
  TprPreviewStuckOptions = (prpsoStuckOver);}
  TprPreviewStuckOptions = (prpsoStuckOver);

{Describes the "stuck" options for TprReport preview.
Syntax:
  TprPreviewStuckOptionsSet = set of TprPreviewStuckOptions;
See also:
  TprPreviewStuckOptions}
  TprPreviewStuckOptionsSet = set of TprPreviewStuckOptions;

{Describes the object's property that specifies the size or location of object.
Items:
  prpsaLeft - The X coordinate of the top-left corner of object.
  prpsaRight - The X coordinate of the bottom-right corner of object.
  prpsaTop - The Y coordinate of the top-left corner of object.
  prpsaBottom - The Y coordinate of the bottom-right corner of object.
  prpsaWidth - The object's width.
  prpsaHeight - The object's height.
Syntax:
  TprObjectPosSizeProps = (prpsaLeft, prpsaRight, prpsaTop, prpsaBottom, prpsaWidth, prpsaHeight);}
  TprObjectPosSizeProps = (prpsaLeft, prpsaRight, prpsaTop, prpsaBottom, prpsaWidth, prpsaHeight);

{Describes the set of object's properties that specify the size or location of object.
Syntax:
  TprObjectPosSizePropsSet = set of TprObjectPosSizeProps;
See also:
  TprObjectPosSizeProps}
  TprObjectPosSizePropsSet = set of TprObjectPosSizeProps;

  TprElementMode = (premNotExists,premExists,premExistsOrNot);
  TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

  TprClip = (prcsNone,prcsObj,prcsBand);
  TprClipState = set of TprClip;
  rPrPaintStruct = record
    ClipRgn : HRGN;
    ClipState : TprClipState;
  end;
  pPrPaintStruct = ^rPrPaintStruct;

{Describes the align and size actions that can be used in the designer.
See also:
  aacHToLeft - Align left edges.
  aacHToRight - Align right edges.
  aacVToTop - Align tops.
  aacVToBottom - Align bottoms.
  aacHCenters - Align horizontal centers.
  aacVCenters - Align vertical centers.
  aacHCenterInWindow - Center horizontally in band.
  aacVCenterInWindow - Center vertically in band.
  aacWToSmall - Set the width of all objects to width of smallest object.
  aacWToLarge - Set the width of all objects to width of largest object.
  aacHToSmall - Set the height of all objects to height of smallest object.
  aacHToLarge - Set the height of all objects to height of largest object.
  aacAlignToGridLeftTop - Align to grid top-left corner of object.
  aacAlignToGridAll - Align to grid bottom-right corner of object.
  aacHSpaceEqually - Space equally, horizontally.
  aacVSpaceEqually - Space equally, vertically.}
  TprAlignActionCode = (aacHToLeft,
                        aacHToRight,
                        aacVToTop,
                        aacVToBottom,
                        aacHCenters,
                        aacVCenters,
                        aacHCenterInWindow,
                        aacVCenterInWindow,
                        aacWToSmall,
                        aacWToLarge,
                        aacHToSmall,
                        aacHToLarge,
                        aacAlignToGridLeftTop,
                        aacAlignToGridAll,
                        aacHSpaceEqually,
                        aacVSpaceEqually);

TOnGetCountRects = procedure (Sender : TObject; var Count : integer) of object;
TOnGetRect = procedure (Sender : TObject; index : integer; var Rect : TRect) of object;
TOnGetAllowResizeTypes = procedure (Sender : TObject; index : integer; var AllowResizeTypes : TprResizeTypeSet) of object;
/////////////////////////////////////////////////
//
// TprSelectionDrawObject
//
/////////////////////////////////////////////////
TprSelectionDrawObject = class(TObject)
private
  FOnGetCountRects : TOnGetCountRects;
  FOnGetRect : TOnGetRect;
  FOnGetAllowResizeTypes : TOnGetAllowResizeTypes;
  FStoredImages : TList;
  procedure ClearStoredImages;
  function GetCount : integer;
  function GetRect(i : integer) : TRect;
  function GetAllowResizeTypes(i : integer) : TprResizeTypeSet;
public
  property Count : integer read GetCount;
  property Rects[i : integer] : TRect read GetRect;
  property AllowResizeTypes[i : integer] : TprResizeTypeSet read GetAllowResizeTypes;
  property OnGetCountRects : TOnGetCountRects read FOnGetCountRects write FOnGetCountRects;
  property OnGetRect : TOnGetRect read FOnGetRect write FOnGetRect;
  property OnGetAllowResizeTypes : TOnGetAllowResizeTypes read FOnGetAllowResizeTypes write FOnGetAllowResizeTypes;
  procedure ShowSelection(DC : HDC);
  procedure ShowSelectionEx(DC : HDC; Rgn : HRGN);
  procedure HideSelection(DC : HDC);
  procedure HideSelectionEx(DC : HDC; Rgn : HRGN);
  constructor Create;
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// Functions
//
/////////////////////////////////////////////////
procedure InitprObjToolbar(Report : TprCustomReport;
                           DesignerForm : TprForm;
                           Toolbar : TToolbar;
                           _OnClick : TNotifyEvent);
procedure InitprObjPopupMenu(Report : TprCustomReport;
                             Menu : TMenu;
                             ParentItem : TMenuItem;
                             _OnClick : TNotifyEvent;
                             CurClassRef : TprObjClass);

procedure InitBandsMenu(Designer : TprDesigner;
                        PopupMenu : TPopupMenu;
                        _OnClick : TNotifyEvent);

function ATG(Coord,Step : integer) : integer;
procedure CalcOffs(dx,dy : integer; ResizeMode : TprResizeType; var oTop,oLeft,oBottom,oRight : integer; StuckedX : integer=0; StuckedY : integer=0);
function CSP(X,Y : integer; x2,y2 : integer) : boolean;
function GetResizeType(X,Y : integer; const r : TRect; var ResizeMode : TprResizeType) : boolean;
procedure DrawSelectedObject(DC : HDC; r : TRect; AllowResizeTypes : TprResizeTypeSet);

procedure MakedRecDefVersionList(LSource,LDest : TList);

function prGetListItemString(LB : TComboBox) : string;
function prGetListItemObject(LB : TComboBox) : TObject;

function prGetProp(L : TList; PropName : string; var PropValue : Variant) : boolean;
function prGetPropDef(L : TList; PropName : string; DefPropValue : Variant) : Variant;
function prGetPropDefBool(L : TList; PropName : string) : TCheckBoxState;
function prGetPropDefSet(L : TList; PropName : string; SetElement : integer) : TprElementMode;
procedure prSetComboBoxString(AComboBox: TComboBox; L: TList; const APropName: string);
procedure prSetProp(L : TList; PropName : string; const PropValue : Variant; IsNull : boolean);
procedure prSetPropSet(L : TList; PropName : string; SetElement : integer; IncludeFlag : boolean; IsNull : boolean);

procedure SetFrameLine(L : TList; EDShow : TCheckBox; EDStyle : TComboBox; UDWidth : TUpDown; BColor : TvgrColorButton; Prefix : string);
procedure GetFrameLine(L : TList; EDShow : TCheckBox; EDStyle : TComboBox; UDWidth : TUpDown; BColor : TvgrColorButton; Prefix : string);
procedure prInitColorButtons(aButtons : array of TvgrColorButton);
procedure UpdateGridBitmap(var Bitmap : HBITMAP; sx,sy : integer);
procedure DrawGrid(DC : HDC; BackRgn : HRGN; Bitmap : HBITMAP; GridSizeX,GridSizeY : integer; const GridRect : TRect; BrushOffsX,BrushOffsY : integer);

procedure EnableToolbarControls(ToolBar : TToolBar; fEnabled : boolean);

const
  DefaultStuckMode = prsmCtrlButton;
  DefaultStuckOffs = 10;
  DefaultStuckOptions = [prsoStuckToBands,prsoStuckToObjects,prsoStuckOver];
  DefaultPreviewStuckOptions = [prpsoStuckOver];
  CF_PROBJ  = CF_MAX+1;
  CF_VALUES = CF_PROBJ+1;
  CF_GROUPS = CF_VALUES+1;
  CF_PROBJVERSIONS = CF_GROUPS+1;
  CF_VARIABLES = CF_PROBJVERSIONS+1;

  SelectPointSize = 5;
  DesignerEmptyString = #12;
  prResizeCursors : array[TprResizeType] of TCursor = (crSizeNWSE,
                                                       crSizeNS,
                                                       crSizeNESW,
                                                       crSizeWE,
                                                       crSizeNWSE,
                                                       crSizeNS,
                                                       crSizeNESW,
                                                       crSizeWE);
  aStuckSides : array [TprResizeType] of TprAllowStuckSidesSet=
   ([prassLeft,prassTop],
    [prassTop],
    [prassRight,prassTop],
    [prassRight],
    [prassRight,prassBottom],
    [prassBottom],
    [prassLeft,prassBottom],
    [prassLeft]);

var
  AllowResizeMarker : TBitmap;
  NotAllowResizeMarker : TBitmap;
  FirstObjectAllowResizeMarker : TBitmap;
  FirstObjectNotAllowResizeMarker : TBitmap;

implementation

uses
  pr_Strings, pr_CommonDesignerPanel;

/////////////////////////////////////////////////
//
// TprSelectionDrawObject
//
/////////////////////////////////////////////////
constructor TprSelectionDrawObject.Create;
begin
inherited;
FStoredImages := TList.Create;
end;

destructor TprSelectionDrawObject.Destroy;
begin
ClearStoredImages;
FStoredImages.Free;
inherited;
end;

function TprSelectionDrawObject.GetCount : integer;
begin
FOnGetCountRects(Self,Result);
end;

function TprSelectionDrawObject.GetRect(i : integer) : TRect;
begin
FOnGetRect(Self,i,Result);
end;

function TprSelectionDrawObject.GetAllowResizeTypes(i : integer) : TprResizeTypeSet;
begin
FOnGetAllowResizeTypes(Self,i,Result);
end;

procedure TprSelectionDrawObject.ClearStoredImages;
var
  i : integer;
begin
for i:=0 to FStoredImages.Count-1 do
  TBitmap(FStoredImages[i]).Free;
FStoredImages.Clear;
end;

procedure TprSelectionDrawObject.ShowSelection(DC : HDC);
var
  r : TRect;
  art : TprResizeTypeSet;
  i,dx,dy : integer;

  procedure SaveImage(x,y : integer);
  var
    b : TBitmap;
  begin
  b := TBitmap.Create;
  b.Width := SelectPointSize;
  b.Height := SelectPointSize;
  BitBlt(b.Canvas.Handle,0,0,SelectPointSize,SelectPointSize,DC,x-(SelectPointSize div 2),y-(SelectPointSize div 2),SRCCOPY);
  FStoredImages.Add(b);
  end;

  procedure DrawImage(x,y : integer; AllowResize : boolean; FirstObject : boolean);
  var
    b : TBitmap;
  begin
  if AllowResize then
    begin
      if FirstObject then
        b := FirstObjectAllowResizeMarker
      else
        b := AllowResizeMarker;
    end
  else
    begin
      if FirstObject then
        b := FirstObjectNotAllowResizeMarker
      else
        b := NotAllowResizeMarker;
    end;
  BitBlt(DC,x-(SelectPointSize div 2),y-(SelectPointSize div 2),SelectPointSize,SelectPointSize,b.Canvas.Handle,0,0,SRCCOPY);
  end;

begin
ClearStoredImages;
for i:=0 to Count-1 do
  begin
    r := Rects[i];
    r.Right := r.Right-1;
    r.Bottom := r.Bottom-1;
    dx := (r.Right-r.Left) div 2;
    dy := (r.Bottom-r.Top) div 2;
    SaveImage(r.Left,r.Top);
    SaveImage(r.Left+dx,r.Top);
    SaveImage(r.Right,r.Top);
    SaveImage(r.Right,r.Top+dy);
    SaveImage(r.Right,r.Bottom);
    SaveImage(r.Left+dx,r.Bottom);
    SaveImage(r.Left,r.Bottom);
    SaveImage(r.Left,r.Top+dy);
  end;

// draw selection markers
for i:=0 to Count-1 do
  begin
    r := Rects[i];
    r.Right := r.Right-1;
    r.Bottom := r.Bottom-1;
    art := AllowResizeTypes[i];
    dx := (r.Right-r.Left) div 2;
    dy := (r.Bottom-r.Top) div 2;
    DrawImage(r.Left,r.Top,ppLeftTop in art,i=0);
    if r.Right-r.Left>SelectPointSize*2+4 then
      DrawImage(r.Left+dx,r.Top,ppTop in art,i=0);
    DrawImage(r.Right,r.Top,ppRightTop in art,i=0);
    if r.Bottom-r.Top>SelectPointSize*2+4 then
      DrawImage(r.Right,r.Top+dy,ppRight in art,i=0);
    DrawImage(r.Right,r.Bottom,ppRightBottom in art,i=0);
    if r.Right-r.Left>SelectPointSize*2+4 then
      DrawImage(r.Left+dx,r.Bottom,ppBottom in art,i=0);
    DrawImage(r.Left,r.Bottom,ppLeftBottom in art,i=0);
    if r.Bottom-r.Top>SelectPointSize*2+4 then
      DrawImage(r.Left,r.Top+dy,ppLeft in art,i=0);
  end;
end;

procedure TprSelectionDrawObject.ShowSelectionEx(DC : HDC; Rgn : HRGN);
var
  r : TRect;
  art : TprResizeTypeSet;
  i,j,dx,dy : integer;

  procedure SaveImage(x,y : integer);
  begin
  with TBitmap(FStoredImages[j]) do
    BitBlt(Canvas.Handle,0,0,SelectPointSize,SelectPointSize,DC,x-(SelectPointSize div 2),y-(SelectPointSize div 2),SRCCOPY);
  j := j+1;
  end;

  procedure DrawImage(x,y : integer; AllowResize : boolean; FirstObject : boolean);
  var
    b : TBitmap;
  begin
  if AllowResize then
    begin
      if FirstObject then
        b := FirstObjectAllowResizeMarker
      else
        b := AllowResizeMarker;
    end
  else
    begin
      if FirstObject then
        b := FirstObjectNotAllowResizeMarker
      else
        b := NotAllowResizeMarker;
    end;
  BitBlt(DC,x-(SelectPointSize div 2),y-(SelectPointSize div 2),SelectPointSize,SelectPointSize,b.Canvas.Handle,0,0,SRCCOPY);
  end;

begin
j := 0;
for i:=0 to Count-1 do
  begin
    r := Rects[i];
    if RectInRegion(Rgn,r) then
      begin
        r.Right := r.Right-1;
        r.Bottom := r.Bottom-1;
        dx := (r.Right-r.Left) div 2;
        dy := (r.Bottom-r.Top) div 2;
        SaveImage(r.Left,r.Top);
        SaveImage(r.Left+dx,r.Top);
        SaveImage(r.Right,r.Top);
        SaveImage(r.Right,r.Top+dy);
        SaveImage(r.Right,r.Bottom);
        SaveImage(r.Left+dx,r.Bottom);
        SaveImage(r.Left,r.Bottom);
        SaveImage(r.Left,r.Top+dy);
      end
    else
      j := j+8;
  end;

// draw selection markers
j := 0;
for i:=0 to Count-1 do
  begin
    r := Rects[i];
    if RectInRegion(Rgn,r) then
      begin
        r.Right := r.Right-1;
        r.Bottom := r.Bottom-1;
        art := AllowResizeTypes[i];
        dx := (r.Right-r.Left) div 2;
        dy := (r.Bottom-r.Top) div 2;
        DrawImage(r.Left,r.Top,ppLeftTop in art,i=0);
        if r.Right-r.Left>SelectPointSize*2+4 then
          DrawImage(r.Left+dx,r.Top,ppTop in art,i=0);
        DrawImage(r.Right,r.Top,ppRightTop in art,i=0);
        if r.Bottom-r.Top>SelectPointSize*2+4 then
          DrawImage(r.Right,r.Top+dy,ppRight in art,i=0);
        DrawImage(r.Right,r.Bottom,ppRightBottom in art,i=0);
        if r.Right-r.Left>SelectPointSize*2+4 then
          DrawImage(r.Left+dx,r.Bottom,ppBottom in art,i=0);
        DrawImage(r.Left,r.Bottom,ppLeftBottom in art,i=0);
        if r.Bottom-r.Top>SelectPointSize*2+4 then
          DrawImage(r.Left,r.Top+dy,ppLeft in art,i=0);
      end
    else
      j := j+8;
  end;
end;

procedure TprSelectionDrawObject.HideSelection(DC : HDC);
var
  r : TRect;
  i,j,dx,dy : integer;

  procedure DrawImage(x,y : integer);
  var
    b : TBitmap;
  begin
  b := TBitmap(FStoredImages[j]);
  BitBlt(DC,x-(SelectPointSize div 2),y-(SelectPointSize div 2),SelectPointSize,SelectPointSize,b.Canvas.Handle,0,0,SRCCOPY);
  j := j+1;
  end;

begin
j := 0;
for i:=0 to Count-1 do
  begin
    r := Rects[i];
    r.Right := r.Right-1;
    r.Bottom := r.Bottom-1;
    dx := (r.Right-r.Left) div 2;
    dy := (r.Bottom-r.Top) div 2;
    DrawImage(r.Left,r.Top);
    DrawImage(r.Left+dx,r.Top);
    DrawImage(r.Right,r.Top);
    DrawImage(r.Right,r.Top+dy);
    DrawImage(r.Right,r.Bottom);
    DrawImage(r.Left+dx,r.Bottom);
    DrawImage(r.Left,r.Bottom);
    DrawImage(r.Left,r.Top+dy);
  end;
end;

procedure TprSelectionDrawObject.HideSelectionEx(DC : HDC; Rgn : HRGN);
var
  r : TRect;
  i,j,dx,dy : integer;

  procedure DrawImage(x,y : integer);
  begin
  with TBitmap(FStoredImages[j]) do
    BitBlt(DC,x-(SelectPointSize div 2),y-(SelectPointSize div 2),SelectPointSize,SelectPointSize,Canvas.Handle,0,0,SRCCOPY);
  j := j+1;
  end;

begin
j := 0;
for i:=0 to Count-1 do
  begin
    r := Rects[i];
    if RectInRegion(Rgn,r) then
      begin
        r.Right := r.Right-1;
        r.Bottom := r.Bottom-1;
        dx := (r.Right-r.Left) div 2;
        dy := (r.Bottom-r.Top) div 2;
        DrawImage(r.Left,r.Top);
        DrawImage(r.Left+dx,r.Top);
        DrawImage(r.Right,r.Top);
        DrawImage(r.Right,r.Top+dy);
        DrawImage(r.Right,r.Bottom);
        DrawImage(r.Left+dx,r.Bottom);
        DrawImage(r.Left,r.Bottom);
        DrawImage(r.Left,r.Top+dy);
      end
    else
      j := j+8;
  end;
end;

/////////////////////////////////////////////////
//
// Functions
//
/////////////////////////////////////////////////
procedure EnableToolbarControls(ToolBar : TToolBar; fEnabled : boolean);
var
  i : integer;
begin
for i:=0 to ToolBar.ControlCount-1 do
  ToolBar.Controls[i].Enabled := fEnabled;
end;

procedure DrawSelectedObject(DC : HDC; r : TRect; AllowResizeTypes : TprResizeTypeSet);
var
  dx,dy : integer;
begin
dx := (r.Right-r.Left) div 2;
dy := (r.Bottom-r.Top) div 2;
r.Right := r.Right-1;
r.Bottom := r.Bottom-1;

if ppLeftTop in AllowResizeTypes then
  begin
    MoveToEx(DC,r.Left,r.Top,nil);
    LineTo(DC,r.Left,r.Top);
  end;
if (ppTop in AllowResizeTypes) and (r.Right-r.Left>SelectPointSize*2+4) then
  begin
    MoveToEx(DC,r.Left+dx,r.Top,nil);
    LineTo(DC,r.Left+dx,r.Top);
  end;
if ppRightTop in AllowResizeTypes then
  begin
    MoveToEx(DC,r.Right,r.Top,nil);
    LineTo(DC,r.Right,r.Top);
  end;
if (ppRight in AllowResizeTypes) and (r.Bottom-r.Top>SelectPointSize*2+4) then
  begin
    MoveToEx(DC,r.Right,r.Top+dy,nil);
    LineTo(DC,r.Right,r.Top+dy);
  end;
if ppRightBottom in AllowresizeTypes then
  begin
    MoveToEx(DC,r.Right,r.Bottom,nil);
    LineTo(DC,r.Right,r.Bottom);
  end;
if (ppBottom in AllowResizeTypes) and (r.Right-r.Left>SelectPointSize*2+4) then
  begin
    MoveToEx(DC,r.Left+dx,r.Bottom,nil);
    LineTo(DC,r.Left+dx,r.Bottom);
  end;
if ppLeftBottom in AllowResizeTypes then
  begin
    MoveToEx(DC,r.Left,r.Bottom,nil);
    LineTo(DC,r.Left,r.Bottom);
  end;
if (ppLeft in AllowResizeTypes) and (r.Bottom-r.Top>SelectPointSize*2+4) then
  begin
    MoveToEx(DC,r.Left,r.Top+dy,nil);
    LineTo(DC,r.Left,r.Top+dy);
  end;
end;

procedure UpdateGridBitmap;
var
  br : HBRUSH;
  DC,MemDC : HDC;
begin
if Bitmap<>0 then
  begin
    DeleteObject(Bitmap);
    Bitmap := 0;
  end;
if (Win32Platform=VER_PLATFORM_WIN32_NT) or ((sx=8) and (sy=8)) then
  begin
    DC := GetDC(0);
    MemDC := CreateCompatibleDC(DC);
    Bitmap := CreateCompatibleBitmap(DC,sx,sy);
    SelectObject(MemDC,Bitmap);
    br := CreateSolidBrush(clWhite);
    FillRect(MemDC,Rect(0,0,sx,sy),br);
    DeleteObject(br);
    SetPixelV(MemDC,0,0,clBlack);
    DeleteDC(MemDC);
    ReleaseDC(0,DC);
  end;
end;

procedure DrawGrid(DC : HDC; BackRgn : HRGN; Bitmap : HBITMAP; GridSizeX,GridSizeY : integer; const GridRect : TRect; BrushOffsX,BrushOffsY : integer);
var
  nbr : HBRUSH;
  i,j : integer;
  OldOrgEx : TPoint;
begin
if (Bitmap=0) or
   ((Win32Platform<>VER_PLATFORM_WIN32_NT) and ((GridSizeX<>8) or (GridSizeY<>8))) then
  begin
    // draw by points
    nbr := CreateSolidBrush(clWhite);
    FillRgn(DC,BackRgn,nbr);
    DeleteObject(nbr);

    i := GridRect.Left+BrushOffsX;
    while i<GridRect.Right do
      begin
        j := GridRect.Top+BrushOffsY;
        while j<GridRect.Bottom do
          begin
            if PtInRegion(BackRgn,i,j) then
              SetPixelV(DC,i,j,clBlack);
            Inc(j,GridSizeY);
          end;
        Inc(i,GridSizeX);
      end;
  end
else
  begin
    nbr := CreatePatternBrush(Bitmap);
    SetBrushOrgEx(DC,BrushOffsX,BrushOffsY,@OldOrgEx);
    FillRgn(DC,BackRgn,nbr);
    DeleteObject(nbr);
    SetBrushOrgEx(DC,OldOrgEx.X,OldOrgEx.Y,nil);
  end;
end;

procedure prInitColorButtons;
var
  i : integer;
begin
for i:=0 to High(aButtons) do
  with aButtons[i] do
    begin
      OtherString := prLoadStr(sColorBtnOtherColorCaption);
      TransparentString := prLoadStr(sColorBtnNoColorCaption);      
    end;
end;
 
procedure SetFrameLine;
begin
EDShow.State := prGetPropDefBool(L,Prefix+'Border.Show');
EDStyle.ItemIndex := prGetPropDef(L,Prefix+'Border.Style',-1);
UDWidth.Position := prGetPropDef(L,Prefix+'Border.Width',0);
BColor.SelectedColor := prGetPropDef(L,Prefix+'Border.Color',clDefault);
end;

procedure GetFrameLine;
begin
prSetProp(L,Prefix+'Border.Show',EDShow.State=cbChecked,EDShow.State=cbGrayed);
prSetProp(L,Prefix+'Border.Style',EDStyle.ItemIndex,EDStyle.ItemIndex=-1);
prSetProp(L,Prefix+'Border.Width',UDWidth.Position,UDWidth.Position=0);
prSetProp(L,Prefix+'Border.Color',BColor.SelectedColor,BColor.SelectedColor=clDefault);
end;

procedure MakedRecDefVersionList;
var
  i : integer;
begin
LDest.Clear;
for i:=0 to LSource.Count-1 do
  if TObject(LSource[i]) is TprObj then
    LDest.Add(TprObj(LSource[i]).dRec.Versions[TprObj(LSource[i]).dRec.DefVersion]);
end;

function prGetListItemString;
begin
if (LB.ItemIndex=-1) or (LB.ItemIndex=0) then
  Result:=''
else
  Result:=LB.Items[LB.ItemIndex];
end;

function prGetListItemObject;
begin
if LB.ItemIndex=-1 then
  Result:=nil
else
  Result:=LB.Items.Objects[LB.ItemIndex];
end;

procedure prSetComboBoxString(AComboBox: TComboBox; L: TList; const APropName: string);
var
  AValue: Variant;
begin
  if prGetProp(L, APropName, AValue) then
  begin
    if VarToStr(AValue) = '' then
      AComboBox.ItemIndex := 0
    else
      AComboBox.ItemIndex := AComboBox.Items.IndexOf(VarToStr(AValue));
  end
  else
    AComboBox.ItemIndex := -1;
end;

procedure prSetProp(L : TList; PropName : string; const PropValue : Variant; IsNull : boolean);
var
  i,p,PropValueInt : integer;
  Obj : TObject;
  lpi : PPropInfo;
  HasSubProp : boolean;
  PropValueString,ClassPropName,SubPropName : string;
  PropValueExt : extended;
begin
if IsNull then exit;

PropValueInt   :=0;
PropValueExt   :=0.0;
PropValueString:='';
case VarType(PropValue) of
  varSmallint,varInteger,varByte:
    PropValueInt := PropValue;
  varBoolean:
    if PropValue then
      PropValueInt := 1
    else
      PropValueInt := 0;
  varSingle,varDouble,varCurrency,varDate:
    PropValueExt := PropValue;
  varOleStr,varString:
    begin
      PropValueString := VarToStr(PropValue);
      if Length(PropValueString) = 1 then
        PropValueInt := Integer(PropValueString[1]);
    end;
  else
    exit;
end;

p         :=pos('.',PropName);
HasSubProp:=p<>0;
if HasSubProp then
  begin
    ClassPropName:=Copy(PropName,1,p-1);
    SubPropName  :=Copy(PropName,p+1,Length(PropName));
  end;

for i:=0 to L.Count-1 do
  begin
    DecompileProp(L[i],Obj,lpi,HasSubProp,PropName,ClassPropName,SubPropName);
    if (Obj<>nil) and (lpi<>nil) then
      begin
        case lpi^.PropType^.Kind of
          tkInteger,tkSet,tkEnumeration,tkChar,tkClass:
            SetOrdProp(Obj,lpi,PropValueInt);
          tkString,tkLString:
            SetStrProp(Obj,lpi,PropValueString);
          tkFloat:
            SetFloatProp(Obj,lpi,PropValueExt);
        end;
      end;
  end;
end;

procedure prSetPropSet;
var
//  ti : PTypeInfo;
  i,p : integer;
  Obj : TObject;
  lpi : PPropInfo;
  cs : TCardinalSet;
  HasSubProp : boolean;
  ClassPropName,SubPropName : string;
begin
if IsNull then exit;

p         :=pos('.',PropName);
HasSubProp:=p<>0;
if HasSubProp then
  begin
    ClassPropName:=Copy(PropName,1,p-1);
    SubPropName  :=Copy(PropName,p+1,Length(PropName));
  end;

for i:=0 to L.Count-1 do
  begin
    DecompileProp(L[i],Obj,lpi,HasSubProp,PropName,ClassPropName,SubPropName);
    if (Obj<>nil) and (lpi<>nil) then
      begin
        if lpi^.PropType^.Kind=tkSet then
          begin
            cs:=TCardinalSet(cardinal(GetOrdProp(Obj,lpi)));
            if IncludeFlag then
              Include(cs,SetElement)
            else
              Exclude(cs,SetElement);
            SetOrdProp(Obj,lpi,cardinal(cs));
          end;
      end;
  end;
end;

function prGetProp;
var
//  ti : PTypeInfo;
  f : boolean;
  i,p : integer;
  Obj : TObject;
  Lastlpi,lpi : PPropInfo;
  HasSubProp : boolean;
  FirstOrdProp : longint;
  FirstFloatProp : extended;
  FirstStringProp : string;
  ClassPropName,SubPropName : string;

begin
Result   :=false;
PropValue:=UnAssigned;
if L.Count<=0 then exit;

p         :=pos('.',PropName);
HasSubProp:=p<>0;
if HasSubProp then
  begin
    ClassPropName:=Copy(PropName,1,p-1);
    SubPropName  :=Copy(PropName,p+1,Length(PropName));
  end;

FirstOrdProp   :=0;
FirstStringProp:='';
FirstFloatProp :=0;
Lastlpi        :=nil;

i     :=0;
f     :=true;
Result:=true;
while i<L.Count do
  begin
    DecompileProp(L[i],Obj,lpi,HasSubProp,PropName,ClassPropName,SubPropName);
    if (Obj<>nil) and (lpi<>nil) then
      begin
        Lastlpi:=lpi;
        if f then
          begin
            case lpi^.PropType^.Kind of
              tkInteger,tkSet,tkEnumeration,tkChar,tkClass:
                FirstOrdProp:=GetOrdProp(Obj,lpi);
              tkString,tkLString:
                FirstStringProp:=GetStrProp(Obj,lpi);
              tkFloat:
                FirstFloatProp:=GetFloatProp(Obj,lpi);
            end;
            f:=false;
          end
        else
          begin
            case lpi^.PropType^.Kind of
              tkInteger,tkSet,tkEnumeration,tkChar,tkClass:
                Result:=Result and (FirstOrdProp=GetOrdProp(Obj,lpi));
              tkString,tkLString:
                Result:=Result and (FirstStringProp=GetStrProp(Obj,lpi));
              tkFloat:
                Result:=Result and (FirstFloatProp=GetFloatProp(Obj,lpi));
            end;
          end;
      end;
    Inc(i);
  end;

Result:=Result and (not f);
if Result then
  case Lastlpi^.PropType^.Kind of
    tkInteger,tkSet,tkEnumeration,tkChar,tkClass:
      PropValue:=FirstOrdProp;
    tkString,tkLString:
      PropValue:=FirstStringProp;
    tkFloat:
      PropValue:=FirstFloatProp;
  end
end;

function prGetPropDefSet;
var
//  ti : PTypeInfo;
  i,p : integer;
  Obj : TObject;
  lpi : PPropInfo;
  cs : TCardinalSet;
  HasSubProp : boolean;
  ClassPropName,SubPropName : string;
begin
Result   :=premExistsOrNot;
if L.Count<=0 then exit;

p         :=pos('.',PropName);
HasSubProp:=p<>0;
if HasSubProp then
  begin
    ClassPropName:=Copy(PropName,1,p-1);
    SubPropName  :=Copy(PropName,p+1,Length(PropName));
  end;

i     :=0;
Result:=premExistsOrNot;
while i<L.Count do
  begin
    DecompileProp(L[i],Obj,lpi,HasSubProp,PropName,ClassPropName,SubPropName);
    if (Obj<>nil) and (lpi<>nil) then
      begin
        if lpi^.PropType^.Kind=tkSet then
          begin
            cs:=TCardinalSet(cardinal(GetOrdProp(Obj,lpi)));
            if Result=premExistsOrNot then
              begin
                if SetElement in cs then
                  Result:=premExists
                else
                  Result:=premNotExists;
              end
            else
              begin
                if SetElement in cs then
                  begin
                    if Result=premNotExists then
                      Result:=premExistsOrNot;
                  end
                else
                  begin
                    if Result=premExists then
                      Result:=premExistsOrNot;
                  end;
                if Result=premExistsOrNot then
                  break;
              end;
          end;
      end;
    Inc(i);
  end;
end;

function prGetPropDef;
begin
if not prGetProp(L,PropName,Result) then
  Result:=DefPropValue;
end;

function prGetPropDefBool;
var
  v : Variant;
begin
if not prGetProp(L,PropName,v) then
  Result:=cbGrayed
else
  begin
    if v then
      Result:=cbChecked
    else
      Result:=cbUnchecked;
  end;
end;

function CSP;
var
  w : integer;
begin
w := SelectPointSize div 2;
Result := PointInRect(X,Y,Rect(x2-w,y2-w,x2+w,y2+w));
end;

function GetResizeType;
var
  dx,dy : integer;
begin
Result := true;
dx :=(r.Right-r.Left) div 2;
dy :=(r.Bottom-r.Top) div 2;
if CSP(X,Y,r.Left,r.Top) then
  begin
    ResizeMode := ppLeftTop;
    exit;
  end;

if CSP(X,Y,r.Left+dx,r.Top) then
  begin
    ResizeMode := ppTop;
    exit;
  end;

if CSP(X,Y,r.Right,r.Top) then
  begin
    ResizeMode := ppRightTop;
    exit;
  end;

if CSP(X,Y,r.Right,r.Top+dy) then
  begin
    ResizeMode := ppRight;
    exit;
  end;

if CSP(X,Y,r.Right,r.Bottom) then
  begin
    ResizeMode := ppRightBottom;
    exit;
  end;

if CSP(X,Y,r.Left+dx,r.Bottom) then
  begin
    ResizeMode := ppBottom;
    exit;
  end;

if CSP(X,Y,r.Left,r.Bottom) then
  begin
    ResizeMode := ppLeftBottom;
    exit;
  end;

if CSP(X,Y,r.Left,r.Top+dy) then
  begin
    ResizeMode := ppLeft;
    exit;
  end;
Result := false;
end;

procedure CalcOffs;
begin
oTop := 0;
oLeft := 0;
oRight := 0;
oBottom := 0;
case ResizeMode of
  ppLeftTop:
    begin oTop:=dy+StuckedY; oLeft:=dx+StuckedX; end;
  ppTop:
    begin oTop:=dy+StuckedY end;
  ppRightTop:
    begin oTop:=dy+StuckedY; oRight:=dx+StuckedX; end;
  ppRight:
    begin oRight:=dx+StuckedX; end;
  ppRightBottom:
    begin oBottom:=dy+StuckedY; oRight:=dx+StuckedX; end;
  ppBottom:
    begin oBottom:=dy+StuckedY; end;
  ppLeftBottom:
    begin oBottom:=dy+StuckedY; oLeft:=dx+StuckedX; end;
  ppLeft:
    begin oLeft:=dx+StuckedX; end;
end;
end;

function ATG;
begin
Result:=(Coord div Step) * Step
end;

procedure InitBandsMenu;
var
  m : TMenuItem;
  bt : TprBandType;
begin
for bt:=Low(TprBandType) to High(TprBandType) do
  begin
    m := TMenuItem.Create(Designer);
    m.Caption := BandTitles[bt];
    m.Tag := integer(bt);
    m.Onclick := _OnClick;
    m.Enabled := Designer.Report.GetBandClass(bt)<>nil;
    PopupMenu.Items.Add(m);
  end;
end;

procedure InitprObjToolbar;
var
  i : integer;
  b : TBitmap;
begin
b := TBitmap.Create;
try
  for i:=0 to High(prObjRegInfos) do
    if Report is prObjRegInfos[i].ReportRef then
      begin
        with TToolButton.Create(DesignerForm) do
          begin
            AutoSize := false;
            Parent := Toolbar;
            Left := MaxInt; // at end of toolbar
            Caption := prLoadStr(prObjRegInfos[i].CaptionResID);
            Hint := prLoadStr(prObjRegInfos[i].HintResID);
            AllowAllUp := true;
            Grouped := true;
            ShowHint := true;
            Tag := i;
            Style := tbsCheck;
            OnClick := _OnClick;
            LoadResImageDef(b,prObjRegInfos[i].ClassRef.ClassName,sDefaultObjResName);
            ImageIndex := Toolbar.Images.AddMasked(b,b.TransparentColor);
          end;
      end;
finally
  b.Free;
end;
end;

procedure InitprObjPopupMenu(Report : TprCustomReport;
                             Menu : TMenu;
                             ParentItem : TMenuItem;
                             _OnClick : TNotifyEvent;
                             CurClassRef : TprObjClass);
var
  i : integer;
begin
AddPopupMenuItem(Menu,ParentItem,sCancelInsertObject,'OBJARROW',_OnClick,'',1,true,CurClassRef=nil);
if Report<>nil then
  begin
    AddPopupMenuItem(Menu,ParentItem,0,'',nil,'',0,true,false);
    for i:=0 to High(prObjRegInfos) do
      if Report is prObjRegInfos[i].ReportRef then
        AddPopupMenuItem(Menu,
                         ParentItem,
                         prObjRegInfos[i].CaptionResID,
                         prObjRegInfos[i].ClassRef.ClassName,
                         _OnClick,
                         '',
                         i+2,
                         true,
                         (CurClassRef<>nil) and (CurClassRef=prObjRegInfos[i].ClassRef));
  end;
end;

initialization

AllowResizeMarker := TBitmap.Create;
LoadResImage(AllowResizeMarker,'ALLOWRESIZE');
NotAllowResizeMarker := TBitmap.Create;
LoadResImage(NotAllowResizeMarker,'NOTALLOWRESIZE');
FirstObjectAllowResizeMarker := TBitmap.Create;
LoadResImage(FirstObjectAllowResizeMarker,'FIRSTOBJECTALLOWRESIZE');
FirstObjectNotAllowResizeMarker := TBitmap.Create;
LoadResImage(FirstObjectNotAllowResizeMarker,'FIRSTOBJECTNOTALLOWRESIZE');

finalization

AllowResizeMarker.Free;
NotAllowResizeMarker.Free;
FirstObjectAllowResizeMarker.Free;
FirstObjectNotAllowResizeMarker.Free;

end.
