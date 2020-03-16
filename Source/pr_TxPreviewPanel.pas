{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the TprTxPreviewPanel class and some auxiliary classes.
See also:
  TprTxPreviewPanel}
unit pr_TxPreviewPanel;

interface

{$I PR.INC}

uses
  Windows, Messages, Classes, SysUtils, inifiles, Graphics, math, Controls,
  dialogs, forms, menus, clipbrd, vgr_GUIFunctions,

  pr_Common, pr_TxClasses, pr_CommonPreviewPanel, pr_TxUtils;

type
TprTxPreviewPanel = class;

{Describes the type of text selection.
Items:
  prtsmStandart - The standard selection.
  prtsmRect - The selection is the the rectangular area of text.
Syntax:
  TprTxPreviewSelectionMode = (prtsmStandart, prtsmRect);}
TprTxPreviewSelectionMode = (prtsmStandart, prtsmRect);
/////////////////////////////////////////////////
//
// TprTxPreviewBox
//
/////////////////////////////////////////////////
TprTxPreviewBox = class(TprCustomPreviewBox)
private
  FCaretCreated : boolean;
  FFontSizes : TSize;
  FFullLinesOnPage : integer;
  FFullColsOnPage : integer;
  FCursorPos : TPoint;
  FLeftTopPos : TPoint;
  FLastHighlightedRec : pTextDeviceRec;
  FDblClick : boolean;
  FSelectionRect : TRect;
  FSelectionMode : TprTxPreviewSelectionMode;
  FFindRect : TRect;
  FDownPos : TPoint;
  FStartSelPos : TPoint;
  function GetText : TStrings;
  function GetPanel : TprTxPreviewPanel;
  function GetPagesList : TList;
  function GetLinesCount : integer;
  procedure WMGetDlgCode(var Msg : TWMGetDlgCode); message WM_GETDLGCODE;
  procedure WMPaint(var Msg : TWMPaint); message WM_PAINT;
  procedure WMEraseBkgnd(var Msg : TWmEraseBkgnd); message WM_ERASEBKGND;
  procedure WMVScroll(var Msg : TWMVScroll); message WM_VSCROLL;
  procedure WMHScroll(var Msg : TWMHScroll); message WM_HSCROLL;
  procedure WMMouseWheel(var Msg : TWMMouseWheel); message WM_MOUSEWHEEL;
  procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
  procedure WMKillFocus(var Msg : TWMKillFocus); message WM_KILLFOCUS;
  procedure InvertRec(rec : pTextDeviceRec);
  procedure GetPointInfoAt(x,y : integer; var rec : pTextDeviceRec);
  function GetScreenRect(r : TRect) : TRect;
  procedure GetSelectionRegion(var rgn : HRGN);
  function IsEmptyFindRect : boolean;
  function IsEmptySelectionRect : boolean;
  procedure SetOptimalSelection(Shift : TShiftState; FStartPos : TPoint; FEndPos : TPoint);
protected
  procedure Resize; override;
  procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  procedure DblClick; override;
  procedure UpdateFont;
  procedure UpdateCursor;
  procedure UpdateFullLinesFullCols;
  procedure MakeCursorVisible(ShowCursor : boolean);
public
  property PagesList : TList read GetPagesList;
  property Text : TStrings read GetText;
  property Panel : TprTxPreviewPanel read GetPanel;
  property LinesCount : integer read GetLinesCount;

  procedure SetFindRect(const NewFindRect : TRect);
  procedure SetSelectionRect(NewSelectionMode : TprTxPreviewSelectionMode; const NewSelectionRect : TRect);
  procedure ResetFindRect;
  procedure ResetSelectionRect;

  constructor Create(AOwner : TComponent); override;
  destructor Destroy; override;
end;

{TprOnCursorPosChanged is the type of TprTxPreviewPanel.OnCursorPosChanged event.
This event occurs when the position of cursor in the text is changed.
Parameters:
  Sender - The TprPreviewPanel object.
  CursorPos - Specifies the new cursor position in the text.
CursorPos.Y - the line, CursorPos.X - the column.}
TprOnCursorPosChanged = procedure(Sender : TObject; CursorPos : TPoint) of object;
/////////////////////////////////////////////////
//
// TprTxPreviewPanel
//
/////////////////////////////////////////////////
{Represents the control for previewing the generated TprTxReport report.
You can use this control if you want to develop non standard report designers
with some unique features.
TprTxPreviewPanel features:
- Contains the built-in popup menu that allows to get an access to all functions of preview panel.<br>
- Supports the search of text.<br>
- and so on<br>
See demo "26_TxDrag-Drop" as example of using this control.
See also:
  TprCustomPreviewPanel}
TprTxPreviewPanel = class(TprCustomPreviewPanel)
private
  FFont : TprFixedFont;
  FTextColor : TColor;
  FFindedTextColor : TColor;
  FFindedBackColor : TColor;
  FSelectionTextColor : TColor;
  FSelectionBackColor : TColor;

  FOnMouseDown : TprOnPreviewMouseDown;
  FOnDblClick : TprOnPreviewDblClick;
  FOnMouseMove : TprOnPreviewMouseMove;
  FOnCursorPosChanged : TprOnCursorPosChanged;

  FPagesList : TList;
  FMaxLineLength : integer;

  function GetReport: TprTxReport;
  procedure SetReport(Value : TprTxReport);
  procedure SetTextColor(Value : TColor);
  procedure SetFindedBackColor(Value : TColor);
  procedure SetFindedTextColor(Value : TColor);
  procedure SetSelectionBackColor(Value : TColor);
  procedure SetSelectionTextColor(Value : TColor);
  function GetSelectedText : string;
  function GetLinesCount : integer;
  function GetCursorCol : integer;
  function GetCursorRow : integer;
  procedure OnFontChange(Sender : TObject);
  function GetBox : TprTxPreviewBox;
  function GetText : TStrings;
  function GetLineLength(i : integer) : integer;
  procedure WMSetFocus(var Msg : TWMSetFocus); message WM_SETFOCUS;
  procedure OnFindDialogFind(Sender : TObject);
  procedure SetFont(Source: TprFixedFont);
protected
  FFindDialog : TFindDialog;

  function GetPageIndex: Integer; override;
  procedure SetPageIndex(Value: Integer); override;
  function GetPageCount: Integer; override;

  function CreatePreviewBox : TprCustomPreviewBox; override;
  procedure AlignChildControls; override;

  procedure DoPreviewMouseDown(PreviewUserData : TprPreviewUserData; X,Y : integer; Shift : TShiftState);
  procedure DoPreviewDblClick(PreviewUserData : TprPreviewUserData; X,Y : integer);
  procedure DoPreviewMouseMove(PreviewUserData : TprPreviewUserData; X,Y : integer; var cur : TCursor; var HighlightObject : boolean);
  procedure DoCursorPosChanged;

  procedure OnPopupMenuClick(Sender : TObject);
  procedure InitPopupMenu(Popup : TPopupMenu); override;

  procedure Notification(AComponent : TComponent; AOperation : TOperation); override;

  property Box: TprTxPreviewBox read GetBox;
public
{Specifies the text that is currently previewed.}
  property Text: TStrings read GetText;
{Returns the length in chars of the longest string in the text.}
  property MaxLineLength: Integer read FMaxLineLength;
{Returns the length in chars of line of text.
Parameters:
  I - The index of lint.}
  property LineLength[i: integer]: integer read GetLineLength;
{Returns the currently selected text, returns the empty string if selection does not exist.}
  property SelectedText: string read GetSelectedText;
{Returns the number of lines in the text.}
  property LinesCount: Integer read GetLinesCount;

{Returns the 0-based number of column in that the cursor is located.}
  property CursorCol : integer read GetCursorCol;
{Returns the 0-based number of row in that the cursor is located.}
  property CursorRow : integer read GetCursorRow;

{Updates the control.}
  procedure UpdatePanel;

{Sets the current cursor position.
The control will be scrolled to display the specified position of the control.
Parameters:
  X - Specifies the 0-based number of column.
  Y - Specifies the 0-based number of row.
  ShowCursor - Display the cursor in this position.}
  procedure SetCursorPos(X, Y: integer; ShowCursor: Boolean = True);

{Sets the selection in the control.
The cursor position will not be changed.
Parameters:
  SelectionMode - Specifies the type of selection.
  SelectionRect - Specifies the selection rectangle.
See also:
  TprTxPreviewSelectionMode}
  procedure SetSelection(SelectionMode: TprTxPreviewSelectionMode; SelectionRect : TRect);
{Returns the true if control has selection.}
  function SelectionExists: Boolean;

{Starts the text searching.
Parameters:
  FindText - The text to search.
  IsCase - Indicates whether the searching must be case sensitive.
  IsDown - Indicates whether the searching must go to down of the text.
  FromPos - Specifies the position within the text from that the searching must be started.
FromPos.X - Specifies the column. FromPos.Y - Specifies the row.
  FindedPos - Contains the position of the found text.
Return value:
  Returns the true if text is found.}
  function FindText(const FindText: string; IsCase, IsDown: Boolean; FromPos: TPoint; var FindedPos: TPoint): Boolean;
{Opens the built-in "Find" dialog.}
  procedure Find;
{Finds the next text fragment.}
  procedure FindNext;
{Finds the previous text fragment.}
  procedure FindPrior;
{Indicates whether the find mode is started.}
  function IsFindMode : boolean;

{Copies the selected text into the clipboard.}
  procedure Copy;

{See:
  TprCustomPreviewPanel.LoadPreparedReport}
  procedure LoadPreparedReport(const FileName : string); override;

{See:
  TprCustomPreviewPanel.WriteToIni}
  procedure WriteToIni(IniFile : TIniFile; const SectionName : string); override;
{See:
  TprCustomPreviewPanel.ReadFromIni}
  procedure ReadFromIni(IniFile : TIniFile; const SectionName : string); override;

{See:
  TprCustomPreviewPanel.EditOptions}
  function EditOptions: Boolean; override;

{See:
  TprCustomPreviewPanel.FullUpdate}
  procedure FullUpdate; override;

{Creates an instance of the TprTxPreviewPanel class.}
  constructor Create(AOwner : TComponent); override;
{Frees an instance of the TprTxPreviewPanel class.}
  destructor Destroy; override;
published
  property Align;

{Specifies the TprTxReport object that is linked to the preview control.}
  property Report: TprTxReport read GetReport write SetReport;

{Specifies the fixed font that is used for drawn the text.
See also:
  TprFixedFont}
  property Font: TprFixedFont read FFont write SetFont;

{Specifies the background color.}
  property Color default clWindow;

{Specifies the color of text.}
  property TextColor : TColor read FTextColor write SetTextColor default clWindowText;

{Specifies the font's color that is used to highlight the found text.}
  property FindedTextColor : TColor read FFindedTextColor write SetFindedTextColor default clWhite;

{Specifies the background color that is used to highlight the found text.}
  property FindedBackColor : TColor read FFindedBackColor write SetFindedBackColor default clBlack;

{Specifies the background color that is used to highlight the selected text.}
  property SelectionBackColor : TColor read FSelectionBackColor write SetSelectionBackColor default clHighlight;

{Specifies the font's color that is used to highlight the selected text.}
  property SelectionTextColor : TColor read FSelectionTextColor write SetSelectionTextColor default clHighlightText;

{This event occurs when the position of cursor in the text is changed.
See also:
  TprOnCursorPosChanged}
  property OnCursorPosChanged: TprOnCursorPosChanged read FOnCursorPosChanged write FOnCursorPosChanged;

{Occurs when the user presses a mouse button with the mouse pointer over a control.
See also:
  TprOnPreviewMouseDown}
  property OnMouseDown: TprOnPreviewMouseDown read FOnMouseDown write FOnMouseDown;

{Occurs when the user double-clicks the left mouse button when the mouse pointer is over the control.
See also:
  TprOnPreviewDblClick}
  property OnDblClick: TprOnPreviewDblClick read FOnDblClick write FOnDblClick;

{This event occurs when user move the mouse over control.
See also:
  TprOnPreviewMouseMove}
  property OnMouseMove: TprOnPreviewMouseMove read FOnMouseMove write FOnMouseMove;
end;

implementation

uses
  pr_Utils, pr_TxConsts, pr_MultiLang, pr_Strings, pr_TxPreviewPanelOptions;
   
/////////////////////////////////////////////////
//
// TprTxPreviewBox
//
/////////////////////////////////////////////////
constructor TprTxPreviewBox.Create(AOwner : TComponent);
begin
inherited;
HorzScrollBar.Tracking := true;
VertScrollBar.Tracking := true;
FCaretCreated := false;
FFontSizes.cx := 0;
FFontSizes.cy := 0;
Canvas.Pen.Color := clBlue;
Canvas.Pen.Width := 2;
FCursorPos.x := 0;
FCursorPos.y := 0;
FLeftTopPos.x := 0;
FLeftTopPos.y := 0;
FFindRect.Left := -1;
FFindRect.Top := -1;
FFindRect.Right := -1;
FFindRect.Bottom := -1;
FSelectionRect.Left := -1;
FSelectionRect.Top := -1;
FSelectionRect.Right := -1;
FSelectionRect.Bottom := -1;
FDownPos.x := -1;
FDownPos.y := -1;
FFullLinesOnPage := 0;
FFullColsOnPage := 0;
end;

destructor TprTxPreviewBox.Destroy;
begin
//if FCaretCreated then
//  DestroyCaret;
inherited;
end;

function TprTxPreviewBox.IsEmptyFindRect : boolean;
begin
Result := (FFindRect.Left=-1) or
          (FFindRect.Right=-1) or
          (FFindRect.Top=-1) or
          (FFindRect.Bottom=-1) or
          (FFindRect.Left=FFindRect.Right) or
          (FFindRect.Top=FFindRect.Bottom);
end;

function TprTxPreviewBox.IsEmptySelectionRect : boolean;
begin
Result := (FSelectionRect.Left=-1) or
          (FSelectionRect.Right=-1) or
          (FSelectionRect.Top=-1) or
          (FSelectionRect.Bottom=-1);
if Result then exit;
if FSelectionMode=prtsmStandart then
  Result := (FSelectionRect.Top=FSelectionRect.Bottom) or
            ((FSelectionRect.Bottom-FSelectionRect.Top=1) and
             (FSelectionRect.Left=FSelectionRect.Right))
else
  Result := (FSelectionRect.Left=FSelectionRect.Right) or
            (FSelectionRect.Top=FSelectionRect.Bottom);
end;

function TprTxPreviewBox.GetText : TStrings;
begin
Result := Panel.Text;
end;

function TprTxPreviewBox.GetPanel : TprTxPreviewPanel;
begin
Result := TprTxPreviewPanel(PreviewPanel);
end;

function TprTxPreviewBox.GetPagesList : TList;
begin
Result := Panel.FPagesList;
end;

function TprTxPreviewBox.GetLinesCount : integer;
begin
if Text=nil then
  Result := 0
else
  Result := Text.Count;
end;

procedure TprTxPreviewBox.WMGetDlgCode(var Msg : TWMGetDlgCode);
begin
inherited;
Msg.Result := Msg.Result or DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;

procedure TprTxPreviewBox.UpdateFont;
begin
Canvas.Font.Name := Panel.Font.Name;
Canvas.Font.Size := Panel.Font.Size;
FFontSizes := GetFontSize(Canvas.Font.Name,Canvas.Font.Size,[]);
VertScrollBar.Range := FFontSizes.cy*(LinesCount+1);
HorzScrollBar.Range := FFontSizes.cx*(Panel.MaxLineLength+1);
VertScrollBar.Increment := FFontSizes.cy;
HorzScrollBar.Increment := FFontSizes.cx;
if HandleAllocated then
  begin
    VertScrollBar.Position := FFontSizes.cy*FLeftTopPos.y;
    HorzScrollBar.Position := FFontSizes.cx*FLeftTopPos.x;
    UpdateCursor;
    UpdateFullLinesFullCols;
    Repaint;
  end;
end;

procedure TprTxPreviewBox.UpdateCursor;
begin
FCaretCreated := CreateCaret(Handle,0,FFontSizes.cx,FFontSizes.cy);
if ShowCaret(Handle) then
  SetCaretPos(FCursorPos.x*FFontSizes.cx-HorzScrollBar.Position,
              FCursorPos.y*FFontSizes.cy-VertScrollBar.Position);
end;

procedure TprTxPreviewBox.UpdateFullLinesFullCols;
begin
if (FFontSizes.cy<>0) and HandleAllocated  then
  FFullLinesOnPage := ClientHeight div FFontSizes.cy
else
  FFullLinesOnPage := 0;
if (FFontSizes.cx<>0) and HandleAllocated then
  FFullColsOnPage := ClientWidth div FFontSizes.cx
else
  FFullColsOnPage := 0;
end;

procedure TprTxPreviewBox.Resize;
begin
inherited;
UpdateFullLinesFullCols;
end;

function TprTxPreviewBox.GetScreenRect(r : TRect) : TRect;
begin
Result := MulDivRect(r,FFontSizes.cx,1,FFontSizes.cy,1);
OffsetRect(Result,-HorzScrollBar.Position,-VertScrollBar.Position);
end;

procedure TprTxPreviewBox.GetSelectionRegion(var rgn : HRGN);
var
  r : TRect;
begin
rgn := 0;
if IsEmptySelectionRect then exit;
if FSelectionMode=prtsmStandart then
  begin
    r := GetScreenRect(FSelectionRect);
    if FSelectionRect.Bottom-FSelectionRect.Top<>1 then
      begin
        AddRectToRegion(rgn,Rect(r.Left,r.Top,ClientWidth,r.Top+FFontSizes.cy));
        if FSelectionRect.Bottom-FSelectionRect.Top>1 then
          AddRectToRegion(rgn,Rect(0,r.Top+FFontSizes.cy,ClientWidth,r.Bottom-FFontSizes.cy));
        AddRectToRegion(rgn,Rect(0,r.Bottom-FFontSizes.cy,r.Right,r.Bottom));
      end
    else
      rgn := CreateRectRgnIndirect(r);
  end
else
  rgn := CreateRectRgnIndirect(GetScreenRect(FSelectionRect));
end;

procedure TprTxPreviewBox.WMPaint(var Msg : TWMPaint);
var
  ps : TPaintStruct;
  br : HBRUSH;
  Buf : string;
  fInvert : boolean;
  i,p,j,fLine,tLine,CurY : integer;
  rgn,SelectionRgn,FindRgn : HRGN;
  cl : cardinal;

  procedure DrawStringPart(const Buf : string; From : integer; Count : integer=-1);
  begin
  if Count=-1 then
    Count := Length(Buf)-From+1;
  Canvas.TextOut(-HorzScrollBar.Position+(From-1)*FFontSizes.cx,CurY,Copy(Buf,From,Count));
  end;

begin
if Panel.Report=nil then
  begin
    BeginPaint(Handle,ps);
    try
      br := CreateSolidBrush(GetRGBColor(Panel.Color));
      FillRect(Canvas.Handle,ps.rcPaint,br);
      DeleteObject(br);
    finally
      EndPaint(Handle,PS);
    end;
  end
else
  begin
    fLine := Max(VertScrollBar.ScrollPos div FFontSizes.cy,0);
    tLine := Min(fLine+FFullLinesOnPage+1,LinesCount-1);
    CurY := fLine*FFontSizes.cy-VertScrollBar.ScrollPos;
    // find first page which started in interval from fLine to tLine
    p := 1;
    while (p<PagesList.Count) and ((integer(PagesList[p])<fLine) or (integer(PagesList[p])>tLine)) do Inc(p);
    if p>=PagesList.Count then
      p := -1;

    fInvert := true;
    BeginPaint(Handle,ps);
    rgn := CreateRectRgnIndirect(ps.rcPaint);
    SelectClipRgn(Canvas.Handle,rgn);
    GetSelectionRegion(SelectionRgn);
    if IsEmptyFindRect then
      FindRgn := 0
    else
      FindRgn := CreateRectRgnIndirect(GetScreenRect(FFindRect));
    if SelectionRgn<>0 then
      CombineRgn(rgn,rgn,SelectionRgn,RGN_DIFF);
    if FindRgn<>0 then
      CombineRgn(rgn,rgn,FindRgn,RGN_DIFF);
    // fill rect
    cl := GetRGBColor(Panel.Color);
    br := CreateSolidBrush(cl);
    FillRgn(Canvas.Handle,rgn,br);
    DeleteObject(br);
    DeleteObject(rgn);
    // fill findtext rect
    if FindRgn<>0 then
      begin
        br := CreateSolidBrush(GetRGBColor(Panel.FindedBackColor));
        FillRgn(Canvas.Handle,FindRgn,br);
        DeleteObject(br);
        DeleteObject(FindRgn);
      end;
    // fill selection rect
    if SelectionRgn<>0 then
      begin
        br := CreateSolidBrush(GetRGBColor(Panel.SelectionBackColor));
        FillRgn(Canvas.Handle,SelectionRgn,br);
        DeleteObject(br);
        DeleteObject(SelectionRgn);
      end;
    try
      SetBkMode(Canvas.Handle,TRANSPARENT);
      Canvas.Font.Color := Panel.TextColor;
      for i:=fLine to tLine do
        begin
          Buf := TxReportOptions.RemoveESCFromString(Text[i]);
          if not IsEmptySelectionRect and (i>=FSelectionRect.Top) and (i<FSelectionRect.Bottom) then
            begin
              case FSelectionMode of
                prtsmStandart:
                  begin
                    if i=FSelectionRect.Top then
                      begin
                        DrawStringPart(Buf,1,FSelectionRect.Left);
                        Canvas.Font.Color := Panel.SelectionTextColor;
                        if i=FSelectionRect.Bottom-1 then
                          begin
                            DrawStringPart(Buf,FSelectionRect.Left+1,FSelectionRect.Right-FSelectionRect.Left);
                            Canvas.Font.Color := Panel.TextColor;
                            DrawStringPart(Buf,FSelectionRect.Right+1);
                          end
                        else
                          begin
                            DrawStringPart(Buf,FSelectionRect.Left+1);
                            Canvas.Font.Color := Panel.TextColor;
                          end;
                      end
                    else
                      if i=FSelectionRect.Bottom-1 then
                        begin
                          Canvas.Font.Color := Panel.SelectionTextColor;
                          DrawStringPart(Buf,1,FSelectionRect.Right);
                          Canvas.Font.Color := Panel.TextColor;
                          DrawStringPart(Buf,FSelectionRect.Right+1);
                        end
                      else
                        begin
                          Canvas.Font.Color := Panel.SelectionTextColor;
                          DrawStringPart(Buf,1);
                          Canvas.Font.Color := Panel.TextColor;
                        end;
                  end;
                prtsmRect:
                  begin
                    DrawStringPart(Buf,1,FSelectionRect.Left);
                    Canvas.Font.Color := Panel.SelectionTextColor;
                    DrawStringPart(Buf,FSelectionRect.Left+1,FSelectionRect.Right-FSelectionRect.Left);
                    Canvas.Font.Color := Panel.TextColor;
                    DrawStringPart(Buf,FSelectionRect.Right+1);
                  end;
              end;
            end
          else
            if not IsEmptyFindRect and (i>=FFindRect.Top) and (i<FFindRect.Bottom) then
              begin
                DrawStringPart(Buf,1,FFindRect.Left);
                Canvas.Font.Color := Panel.FindedTextColor;
                DrawStringPart(Buf,FFindRect.Left+1,FFindRect.Right-FFindRect.Left);
                Canvas.Font.Color := Panel.TextColor;
                DrawStringPart(Buf,FFindRect.Right+1); 
              end
            else
              Canvas.TextOut(-HorzScrollBar.ScrollPos,CurY,Buf);
          if p<>-1 then
            begin
              j := p;
              while (j<PagesList.Count) and
                    (integer(PagesList[j])<=tLine) and
                    (integer(PagesList[j])<>i) do Inc(j);
              if (j<PagesList.Count) and (integer(PagesList[j])=i) then
                begin
                  // j - page number
                  Canvas.MoveTo(-2,CurY-1);
                  Canvas.LineTo(ClientWidth,CurY-1);
                end;
            end;
          if fInvert and (FLastHighlightedRec<>nil) and (i>=FLastHighlightedRec.rPlace.Top) and (i>=FLastHighlightedRec.rPlace.Bottom) then
            begin
              InvertRec(FLastHighlightedRec);
              fInvert := false;
            end;
          CurY := CurY+FFontSizes.cy;
        end;
      SetBkMode(Canvas.Handle,OPAQUE);
    finally
      SelectClipRgn(Canvas.Handle,0);
      EndPaint(Handle,PS);
    end;
  end;
end;

procedure TprTxPreviewBox.WMEraseBkgnd(var Msg : TWmEraseBkgnd);
begin
Msg.Result := 1;
end;

procedure TprTxPreviewBox.SetFindRect(const NewFindRect : TRect);
var
  rgn : HRGN;
begin
rgn := 0;
if RectOverRect(GetScreenRect(FFindRect),Rect(0,0,ClientWidth,ClientHeight)) then
  AddRectToRegion(rgn,GetScreenRect(FFindRect));
FFindRect := NewFindRect;
if RectOverRect(GetScreenRect(FFindRect),Rect(0,0,ClientWidth,ClientHeight)) then
  AddRectToRegion(rgn,GetScreenRect(FFindRect));
if rgn<>0 then
  begin
    InvalidateRgn(Handle,rgn,true);
    DeleteObject(rgn);
  end;
end;

procedure TprTxPreviewBox.ResetFindRect;
begin
SetFindRect(Rect(-1,-1,-1,-1));
end;

procedure TprTxPreviewBox.SetSelectionRect(NewSelectionMode : TprTxPreviewSelectionMode; const NewSelectionRect : TRect);
var
  rgn,TempRgn : HRGN;
begin
rgn := 0;
GetSelectionRegion(rgn);
FSelectionMode := NewSelectionMode;
FSelectionRect := NewSelectionRect;
GetSelectionRegion(TempRgn);
if (rgn<>0) and (TempRgn<>0) then
  begin
    CombineRgn(rgn,rgn,TempRgn,RGN_OR);
    DeleteObject(TempRgn);
  end
else
  if rgn=0 then
    rgn := TempRgn;
if rgn<>0 then
  begin
    InvalidateRgn(Handle,rgn,true);
    DeleteObject(rgn);
  end;
end;

procedure TprTxPreviewBox.ResetSelectionRect;
begin
SetSelectionRect(FSelectionMode,Rect(-1,-1,-1,-1));
end;

procedure TprTxPreviewBox.InvertRec(rec : pTextDeviceRec);
var
  r : TRect;
begin
r.Left := rec.rPlace.Left*FFontSizes.cx-HorzScrollBar.ScrollPos;
r.Top := rec.rPlace.Top*FFontSizes.cy-VertScrollBar.ScrollPos;
r.Right := r.Left+(rec.rPlace.Right-rec.rPlace.Left+1)*FFontSizes.cx;
r.Bottom := r.Top+(rec.rPlace.Bottom-rec.rPlace.Top+1)*FFontSizes.cy;
InvertRect(Canvas.Handle,r);
end;

procedure TprTxPreviewBox.SetOptimalSelection(Shift : TShiftState; FStartPos : TPoint; FEndPos : TPoint);
var
  r : TRect;
  SelectionMode : TprTxPreviewSelectionMode;
begin
if ssAlt in Shift then
  begin
    SelectionMode := prtsmRect;
    if FEndPos.x>=FStartPos.x then
      begin
        r.Left := FStartPos.x;
        r.Right := FEndPos.x+1;
      end
    else
      begin
        r.Left := FEndPos.x;
        r.Right := FStartPos.x+1;
      end;
      
    if FEndPos.y>=FStartPos.y then
      begin
        r.Top := FStartPos.y;
        r.Bottom := FEndPos.y+1;
      end
    else
      begin
        r.Top := FEndPos.y;
        r.Bottom := FStartPos.y+1;
      end;
  end
else
  begin
    SelectionMode := prtsmStandart;
    if FEndPos.y>=FStartPos.y then
      begin
        r.Top := FStartPos.y;
        r.Bottom := FEndPos.y+1;
        if (FEndPos.x>=FStartPos.x) or (FEndPos.y>FStartPos.y) then
          begin
            r.Left := FStartPos.x;
            r.Right := FEndPos.x;
          end
        else
          begin
            r.Left := FEndPos.x;
            r.Right := FStartPos.x+1;
          end;
      end
    else
      begin
        r.Top := FEndPos.y;
        r.Bottom := FStartPos.y+1;
        r.Left := FEndPos.x;
        r.Right := FStartPos.x;
      end;
  end;
Panel.SetSelection(SelectionMode,r);
end;

procedure TprTxPreviewBox.KeyDown(var Key: Word; Shift: TShiftState);
var
  OldCursorPos,OldLeftTopPos : TPoint;
begin
OldCursorPos := FCursorPos;
OldLeftTopPos := FLeftTopPos;
case Key of
  VK_HOME:
    begin
      FCursorPos.x := 0;
      FLeftTopPos.x := 0;
    end;
  VK_END:
    begin
      FCursorPos.x := Max(0,Panel.LineLength[FCursorPos.y]);
      if FCursorPos.x>=FFullColsOnPage then
        FLeftTopPos.x := FCursorPos.x-FFullColsOnPage+1;
    end;
  VK_DOWN:
    begin
      if FCursorPos.y<LinesCount then
        Inc(FCursorPos.y);
      FLeftTopPos.y := FLeftTopPos.y+integer(not((FCursorPos.y>=FLeftTopPos.y) and (FCursorPos.y<FLeftTopPos.y+FFullLinesOnPage)));
    end;
  VK_UP:
    begin
      if FCursorPos.y>0 then
        Dec(FCursorPos.y);
      FLeftTopPos.y := FLeftTopPos.y-integer(not ((FCursorPos.y>=FLeftTopPos.y) and (FCursorPos.y<=FLeftTopPos.y+FFullLinesOnPage)));
    end;
  VK_RIGHT:
    begin
      if FCursorPos.x<Panel.MaxLineLength then
        Inc(FCursorPos.x);
      FLeftTopPos.x := FLeftTopPos.x+integer(not ((FCursorPos.x>=FLeftTopPos.x) and (FCursorPos.x<FLeftTopPos.x+FFullColsOnPage)));
    end;
  VK_LEFT:
    begin
      if FCursorPos.x>0 then
        Dec(FCursorPos.x);
      FLeftTopPos.x := FLeftTopPos.x-integer(not ((FCursorPos.x>=FLeftTopPos.x) and (FCursorPos.x<FLeftTopPos.x+FFullColsOnPage)));
    end;
  VK_NEXT:
    begin
      if ssCtrl in Shift then
        begin
          FCursorPos.y := LinesCount;
          FLeftTopPos.y := LinesCount-FFullLinesOnPage;
        end
      else
        begin
          if FCursorPos.y+FFullLinesOnPage<=LinesCount then
            begin
              Inc(FCursorPos.y,FFullLinesOnPage);
              Inc(FLeftTopPos.y,FFullLinesOnPage);
            end
          else
            begin
              FCursorPos.y := LinesCount;
              FLeftTopPos.y := LinesCount-FFullLinesOnPage;
            end;
        end;
    end;
  VK_PRIOR:
    begin
      if ssCtrl in Shift then
        begin
          FCursorPos.y := 0;
          FLeftTopPos.y := 0;
        end
      else
        begin
          if FCursorPos.y-FFullLinesOnPage>=0 then
            begin
              Dec(FCursorPos.y,FFullLinesOnPage);
              Dec(FLeftTopPos.y,FFullLinesOnPage);
            end
          else
            begin
              FCursorPos.y := 0;
              FLeftTopPos.y := 0;
            end;
        end;
    end;
  else
    begin
      if (Key=VK_F3) and (Shift=[]) then
        Panel.FindNext;
      if (Key=VK_F3) and (Shift=[ssShift]) then
        Panel.FindPrior;
      if (Key=ord('F')) and (Shift=[ssCtrl]) then
        Panel.Find;
      if ((Key=ord('C')) and (Shift=[ssCtrl])) or
         ((Key=VK_INSERT) and (Shift=[ssCtrl])) then
        Panel.Copy;
      if ((Key=ord('O')) and (Shift=[ssCtrl])) then
        Panel.Load;
      if ((Key=ord('S')) and (Shift=[ssCtrl])) then
        Panel.Save;
      if ((Key=VK_F9) and (Shift=[])) then
        Panel.Print;
      exit;
    end;
end;

if (OldCursorPos.x=FCursorPos.x) and (OldCursorPos.y=FCursorPos.y) then exit;

ResetFindRect;
if not (((FCursorPos.y>=FLeftTopPos.y) and (FCursorPos.y<FLeftTopPos.y+FFullLinesOnPage)) and
        ((FCursorPos.x>=FLeftTopPos.x) and (FCursorPos.x<FLeftTopPos.x+FFullColsOnPage))) then
  begin
    // The cursor is not seen, we do it, seen
    FLeftTopPos.y := Max(0,FCursorPos.y-FFullLinesOnPage+1);
    FLeftTopPos.x := Max(0,FCursorPos.x-FFullColsOnPage+1);
  end;
if (OldLeftTopPos.x<>FLeftTopPos.x) or (OldLeftTopPos.y<>FLeftTopPos.y) then
  begin
    VertScrollBar.Position := FLeftTopPos.y*FFontSizes.cy;
    HorzScrollBar.Position := FLeftTopPos.x*FFontSizes.cx;
  end;
UpdateCursor;
Panel.DoCursorPosChanged;
if ssShift in Shift then
  begin
    if FStartSelPos.x=-1 then
      FStartSelPos := OldCursorPos; 
    SetOptimalSelection(Shift,FStartSelPos,FCursorPos);
  end
else
  begin
    ResetSelectionRect;
    FStartSelPos.x := -1;
    FStartSelPos.y := -1;
  end;
end;

procedure TprTxPreviewBox.WMVScroll(var Msg : TWMVScroll);
begin
inherited;
FLeftTopPos.y := VertScrollBar.Position div FFontSizes.cy;
end;

procedure TprTxPreviewBox.WMHScroll(var Msg : TWMHScroll);
begin
inherited;
FLeftTopPos.x := HorzScrollBar.Position div FFontSizes.cx;
end;

procedure TprTxPreviewBox.WMMouseWheel(var Msg : TWMMouseWheel);
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

procedure TprTxPreviewBox.GetPointInfoAt(x,y : integer; var rec : pTextDeviceRec);
var
  i,charx,chary : integer;
begin
rec := nil;
if Panel.Report=nil then exit;
charx := (HorzScrollBar.Position+x) div FFontSizes.cx;
chary := (VertScrollBar.Position+y) div FFontSizes.cy;
if (chary<LinesCount) and (charx<Panel.MaxLineLength) then
  begin
    for i:=0 to Panel.Report.TextDevice.Recs.Count-1 do
      if PointInRect(charx,chary,pTextDeviceRec(Panel.Report.TextDevice.Recs[i]).rPlace) then
        begin
          rec := pTextDeviceRec(Panel.Report.TextDevice.Recs[i]);
          break;
        end;
  end;
end;

procedure TprTxPreviewBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  rec : pTextDeviceRec;
  PreviewUserData : TprPreviewUserData;
begin
if FDblClick then
  begin
    FDblClick := false;
    exit;
  end;
SetFocus;
if Panel.Report=nil then exit;
FDownPos.x := (HorzScrollBar.Position+x) div FFontSizes.cx;
FDownPos.y := (VertScrollBar.Position+y) div FFontSizes.cy;
if (ssLeft in Shift) and (FDownPos.y<=LinesCount) and (FDownPos.x<=Panel.MaxLineLength) then
  begin
    ResetFindRect;
    ResetSelectionRect;
    FCursorPos := FDownPos;
    FStartSelPos := FDownPos;
    UpdateCursor;
    Panel.DoCursorPosChanged;

    GetPointInfoAt(x,y,rec);
    PreviewUserData := nil;
    if rec<>nil then
      PreviewUserData := rec.UserData;
    Panel.DoPreviewMouseDown(PreviewUserData,X,Y,Shift);
  end
else
  begin
    FDownPos.x := -1;
    FDownPos.y := -1;
  end;
end;

procedure TprTxPreviewBox.DblClick;
var
  p : TPoint;
  rec : pTextDeviceRec;
  PreviewUserData : TprPreviewUserData;
begin
FDblClick := true;
if Panel.Report=nil then exit;
GetCursorPos(p);
p := ScreenToClient(p);
GetPointInfoAt(p.x,p.y,rec);
PreviewUserData := nil;
if rec<>nil then
  PreviewUserData := rec.UserData;
Panel.DoPreviewDblClick(PreviewUserData,p.x,p.y);
end;

procedure TprTxPreviewBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
FDownPos.x := -1;
FDownPos.y := -1;
end;

procedure TprTxPreviewBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  cur : TCursor;
  rec : pTextDeviceRec;
  FMovePos : TPoint;
  PreviewUserData : TprPreviewUserData;
  HighlightObject : boolean;
begin
if Panel.Report=nil then exit;
if (FDownPos.x<>-1) and (FDownPos.y<>-1) then
  begin
    FMovePos.x := (HorzScrollBar.Position+x) div FFontSizes.cx;
    FMovePos.y := (VertScrollBar.Position+y) div FFontSizes.cy;
    FMovePos.x := Max(0,Min(FMovePos.x,Panel.MaxLineLength));
    FMovePos.y := Max(0,Min(FMovePos.y,LinesCount));
    SetOptimalSelection(Shift,FDownPos,FMovePos);
    FCursorPos := FMovePos;
    UpdateCursor;
    Panel.DoCursorPosChanged;
  end
else
  begin
    GetPointInfoAt(x,y,rec);
    cur := crArrow;
    HighlightObject := false;
    PreviewUserData := nil;
    if rec<>nil then
      PreviewUserData := rec.UserData;
    Panel.DoPreviewMouseMove(PreviewUserData,X,Y,cur,HighlightObject);
    if cur<>Cursor then
      Cursor := cur;
    if rec<>FLastHighlightedRec then
      begin
        if FLastHighlightedRec<>nil then
          InvertRec(FLastHighlightedRec);
        if (rec<>nil) and HighlightObject then
          begin
            InvertRec(rec);
            FLastHighlightedRec := rec;
          end
        else
          FLastHighlightedRec := nil;
      end;
  end;
end;

procedure TprTxPreviewBox.MakeCursorVisible(ShowCursor : boolean);
begin
if not (((FCursorPos.y>=FLeftTopPos.y) and (FCursorPos.y<FLeftTopPos.y+FFullLinesOnPage)) and
        ((FCursorPos.x>=FLeftTopPos.x) and (FCursorPos.x<FLeftTopPos.x+FFullColsOnPage))) then
  begin
    // The cursor is not seen, we do(make) it, seen,
    FLeftTopPos.y := Max(0,FCursorPos.y-FFullLinesOnPage+1);
    FLeftTopPos.x := Max(0,FCursorPos.x-FFullColsOnPage+1);
    VertScrollBar.Position := FLeftTopPos.y*FFontSizes.cy;
    HorzScrollBar.Position := FLeftTopPos.x*FFontSizes.cx;
  end;
if ShowCursor then
  UpdateCursor;
end;

procedure TprTxPreviewBox.WMSetFocus(var Msg : TWMSetFocus);
begin
inherited;
UpdateCursor;
end;

procedure TprTxPreviewBox.WMKillFocus(var Msg : TWMKillFocus);
begin
inherited;
//DestroyCaret;
end;

/////////////////////////////////////////////////
//
// TprTxPreviewPanel
//
/////////////////////////////////////////////////
constructor TprTxPreviewPanel.Create(AOwner : TComponent);
begin
inherited;
Color := clWindow;
FTextColor := clWindowText;
FFindedTextColor := clWhite;
FFindedBackColor := clBlack;
FSelectionBackColor := clHighlight;
FSelectionTextColor := clHighlightText;
FPagesList := TList.Create;
FFont := TprFixedFont.Create;
FFont.OnChange := OnFontChange;
FMaxLineLength := 0;
UpdatePanel;
if not (csDesigning in ComponentState) then
  begin
    FFindDialog := TFindDialog.Create(Self);
    FFindDialog.Options := FFindDialog.Options + [frHideWholeWord];
    FFindDialog.OnFind := OnFindDialogFind;
  end;
end;

destructor TprTxPreviewPanel.Destroy;
begin
FFont.Free;
FPagesList.Free;
inherited;
end;

procedure TprTxPreviewPanel.Notification(AComponent : TComponent; AOperation : TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = Report) then
    Report := nil;
end;

procedure TprTxPreviewPanel.OnFontChange(Sender : TObject);
begin
Box.UpdateFont;
end;

function TprTxPreviewPanel.GetReport: TprTxReport;
begin
  Result := TprTxReport(inherited Report);
end;

procedure TprTxPreviewPanel.SetReport(Value: TprTxReport);
begin
  inherited Report := Value;
end;

function TprTxPreviewPanel.GetLinesCount : integer;
begin
if Text=nil then
  Result := 0
else
  Result := Text.Count;
end;

function TprTxPreviewPanel.GetCursorCol : integer;
begin
Result := Box.FCursorPos.x;
end;

function TprTxPreviewPanel.GetCursorRow : integer;
begin
Result := Box.FCursorPos.y;
end;

function TprTxPreviewPanel.GetPageIndex: Integer;
var
  Y: Integer;
begin
  Result := 0;
  Y := Box.FCursorPos.y;
  while (Result < FPagesList.Count) and (y >= Integer(FPagesList[Result])) do Inc(Result);
  if Result >= PageCount then
    Result := PageCount - 1
  else
    Result := Result - 1;
end;

procedure TprTxPreviewPanel.SetPageIndex(Value: Integer);
begin
  if Value >= PageCount then exit;
  SetCursorPos(0, Integer(FPagesList[Value]));
end;

function TprTxPreviewPanel.GetPageCount : integer;
begin
Result := FPagesList.Count;
end;

procedure TprTxPreviewPanel.SetTextColor(Value : TColor);
begin
if FTextColor=Value then exit;
FTextColor := Value;
Box.Repaint;
end;

procedure TprTxPreviewPanel.SetFindedBackColor(Value : TColor);
begin
if FFindedBackColor=Value then exit;
FFindedBackColor := Value;
if not Box.IsEmptyFindRect then
  Box.SetFindRect(Box.FFindRect);
end;

procedure TprTxPreviewPanel.SetFindedTextColor(Value : TColor);
begin
if FFindedTextColor=Value then exit;
FFindedTextColor := Value;
if not Box.IsEmptyFindRect then
  Box.SetFindRect(Box.FFindRect);
end;

procedure TprTxPreviewPanel.SetSelectionBackColor(Value : TColor);
begin
if FSelectionBackColor=Value then exit;
FSelectionBackColor := Value;
if not Box.IsEmptySelectionRect then
  Box.SetSelectionRect(Box.FSelectionMode,Box.FSelectionRect);
end;

procedure TprTxPreviewPanel.SetSelectionTextColor(Value : TColor);
begin
if FSelectionTextColor=Value then exit;
FSelectionTextColor := Value;
if not Box.IsEmptySelectionRect then
  Box.SetSelectionRect(Box.FSelectionMode,Box.FSelectionRect);
end;

function TprTxPreviewPanel.GetBox : TprTxPreviewBox;
begin
Result := TprTxPreviewBox(FPreviewBox);
end;

function TprTxPreviewPanel.GetText : TStrings;
begin
if Report<>nil then
  Result := Report.TextDevice.SList
else
  Result := nil;
end;

function TprTxPreviewPanel.GetLineLength(i : integer) : integer;
begin
  if i < Text.Count then
    Result := Length(TxReportOptions.RemoveESCFromString(Text[i]))
  else
    Result := 0;
end;

function TprTxPreviewPanel.CreatePreviewBox : TprCustomPreviewBox;
begin
Result := TprTxPreviewBox.Create(Self);
end;

procedure TprTxPreviewPanel.OnPopupMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  1: Copy;
  2: Find;
  3: FindNext;
  4: FindPrior;
  5: Print;
  6: ExportTo;
  7: Load;
  8: Save;
  9: EditOptions;
end;
end;

procedure TprTxPreviewPanel.InitPopupMenu(Popup : TPopupMenu);
begin
ClearPopupMenu(Popup);
AddPopupMenuItem(Popup,nil,sClipboardCopy,'COPY',OnPopupMenuClick,'Ctrl+C',1,SelectionExists,false);
AddPopupMenuItem(Popup,nil,0,'',nil,'',0,false,false);
AddPopupMenuItem(Popup,nil,sFindStart,'FIND',OnPopupMenuClick,'Ctrl+F',2,not ReportEmpty,false);
AddPopupMenuItem(Popup,nil,sFindNext,'FINDNEXT',OnPopupMenuClick,'F3',3,IsFindMode,false);
AddPopupMenuItem(Popup,nil,sFindPrior,'FINDPREV',OnPopupMenuClick,'Shift+F3',4,IsFindMode,false);
AddPopupMenuItem(Popup,nil,0,'',nil,'',0,false,false);
AddPopupMenuItem(Popup,nil,sPrint,'PRINT',OnPopupMenuClick,'F9',5,not ReportEmpty,false);
AddPopupMenuItem(Popup,nil,sExport,'EXPORTTOTXT',OnPopupMenuClick,'',6,not ReportEmpty,false);
AddPopupMenuItem(Popup,nil,0,'',nil,'',0,false,false);
AddPopupMenuItem(Popup,nil,sLoadPreparedReport,'OPEN',OnPopupMenuClick,'Ctrl+O',7,Report<>nil,false);
AddPopupMenuItem(Popup,nil,sSavePreparedReport,'SAVE',OnPopupMenuClick,'Ctrl+S',8,not ReportEmpty,false);
AddPopupMenuItem(Popup,nil,0,'',nil,'',0,false,false);
AddPopupMenuItem(Popup,nil,sOptions,'',OnPopupMenuClick,'',9,true,false);
end;

procedure TprTxPreviewPanel.AlignChildControls;
begin
inherited;
end;

procedure TprTxPreviewPanel.WriteToIni(IniFile : TIniFile; const SectionName : string);
begin
Font.WriteToIni(IniFile,SectionName,'Font');
IniFile.WriteInteger(SectionName,'Color',Color);
IniFile.WriteInteger(SectionName,'TextColor',TextColor);
IniFile.WriteInteger(SectionName,'FindedBackColor',FindedBackColor);
IniFile.WriteInteger(SectionName,'FindedTextColor',FindedTextColor);
IniFile.WriteInteger(SectionName,'SelectionBackColor',SelectionBackColor);
IniFile.WriteInteger(SectionName,'SelectionTextColor',SelectionTextColor);
end;

procedure TprTxPreviewPanel.ReadFromIni(IniFile : TIniFile; const SectionName : string);
begin
Font.ReadFromIni(IniFile,SectionName,'Font');
Color := IniFile.ReadInteger(SectionName,'Color',Color);
TextColor := IniFile.ReadInteger(SectionName,'TextColor',TextColor);
FindedBackColor := IniFile.ReadInteger(SectionName,'FindedBackColor',FindedBackColor);
FindedTextColor := IniFile.ReadInteger(SectionName,'FindedTextColor',FindedTextColor);
SelectionBackColor := IniFile.ReadInteger(SectionName,'SelectionBackColor',SelectionBackColor);
SelectionTextColor := IniFile.ReadInteger(SectionName,'SelectionTextColor',SelectionTextColor);
end;

function TprTxPreviewPanel.EditOptions : boolean;
begin
Result := TprTxPreviewPanelOptionsForm.Create(Application).EditOptions(Self);
end;

procedure TprTxPreviewPanel.UpdatePanel;
var
  i,l : integer;
begin
FPagesList.Clear;
FMaxLineLength := 0;
if Text<>nil then
  begin
    TxReportOptions.ParsePages(Text,false,0,FPagesList);
    // The longest string
    for i:=0 to LinesCount-1 do
      begin
        l := LineLength[i];
        if l>FMaxLineLength then
          FMaxLineLength := l;
      end;
  end;
//
Box.FCursorPos.x := 0;
Box.FCursorPos.y := 0;
Box.FLeftTopPos.x := 0;
Box.FLeftTopPos.y := 0;
Box.UpdateFont;
DoCursorPosChanged;
end;

procedure TprTxPreviewPanel.DoCursorPosChanged;
begin
if Assigned(FOnCursorPosChanged) then
  FOnCursorPosChanged(Self,Box.FCursorPos);
end;

procedure TprTxPreviewPanel.DoPreviewMouseDown(PreviewUserData : TprPreviewUserData; X,Y : integer; Shift : TShiftState);
begin
if Assigned(FOnMouseDown) then
  FOnMouseDown(Self,PreviewUserData,X,Y,Shift);
end;

procedure TprTxPreviewPanel.DoPreviewDblClick(PreviewUserData : TprPreviewUserData; X,Y : integer);
begin
if Assigned(FOnDblClick) then
  FOnDblClick(Self,PreviewUserData,X,Y);
end;

procedure TprTxPreviewPanel.DoPreviewMouseMove(PreviewUserData : TprPreviewUserData; X,Y : integer; var cur : TCursor; var HighlightObject : boolean);
begin
if Assigned(FOnMouseMove) then
  FOnMouseMove(Self,PreviewUserData,X,Y,cur,HighlightObject);
end;

procedure TprTxPreviewPanel.WMSetFocus(var Msg : TWMSetFocus);
begin
inherited;
Box.SetFocus;
end;

procedure TprTxPreviewPanel.SetCursorPos(X, Y: integer; ShowCursor: Boolean = True);
begin
  if ReportEmpty then exit;
  
  Box.FCursorPos.x := Max(0,Min(x,MaxLineLength));
  Box.FCursorPos.y := Max(0,Min(y,LinesCount-1));
  Box.MakeCursorVisible(ShowCursor);
  DoCursorPosChanged;
end;

function TprTxPreviewPanel.FindText(const FindText : string; IsCase,IsDown : boolean; FromPos : TPoint; var FindedPos : TPoint) : boolean;
label
  ExitLoop;
var
  i,j,ls,lfText : integer;
  fndText,s : string;
begin
if IsCase then
  fndText := FindText
else
  fndText := AnsiLowerCase(FindText);
lfText := Length(fndText);
if IsDown then
  begin
    i := FromPos.y;
    j := FromPos.x+1;
    while i<LinesCount do
      begin
        s := TxReportOptions.RemoveESCFromString(Text[i]);
        Report.OEMToWIN(PChar(s),PChar(s));
        if not IsCase then
          s := AnsiLowerCase(s);
        ls := Length(s);
        while (j<=ls) and (System.Copy(s,j,lfText)<>fndText) do Inc(j);
        if j<=ls then break;
        j := 1;
        Inc(i);
      end;
    Result := i<LinesCount;
  end
else
  begin
    i := FromPos.y;
    j := FromPos.x+1;
    while i>=0 do
      begin
        s := TxReportOptions.RemoveESCFromString(Text[i]);
        Report.OEMToWIN(PChar(s),PChar(s));
        if not IsCase then
          s := AnsiLowerCase(s);
        if i<>FromPos.y then
          j := Length(s)-lfText+1;
        while (j>0) and (System.Copy(s,j,lfText)<>fndText) do Dec(j);
        if j>0 then break;
        Dec(i);
      end;
    Result := i>=0;
  end;
if Result then
  begin
    FindedPos.x := j-1;
    FindedPos.y := i;
  end;
end;

procedure TprTxPreviewPanel.SetFont(Source: TprFixedFont);
begin
  FFont.Assign(Source);
end;

procedure TprTxPreviewPanel.OnFindDialogFind(Sender : TObject);
var
  p,ScreenBorder : TPoint;
  r1,r2 : TRect;
  FindedPos,FromPos : TPoint;
begin
if Box.FFindRect.Left=-1 then
  begin
    // start find
    FromPos.x := Box.FCursorPos.x;
    FromPos.y := Box.FCursorPos.y;
  end
else
  begin
    // continue find
    if frDown in FFindDialog.Options then
      FromPos.x := Box.FFindRect.Left+1
    else
      FromPos.x := Box.FFindRect.Left-1;
    FromPos.y := Box.FFindRect.Top;
  end;
if FindText(FFindDialog.FindText,frMatchCase in FFindDialog.Options,frDown in FFindDialog.Options,FromPos,FindedPos) then
  begin
    Box.SetFindRect(Rect(FindedPos.x,FindedPos.y,FindedPos.x+Length(FFindDialog.FindText),FindedPos.y+1));
    SetCursorPos(Box.FFindRect.Right,Box.FFindRect.Top,false); //
    FFindDialog.Execute;
    // optimize place of FFindDialog
    if SystemParametersInfo(SPI_GETWORKAREA,0,@r1,0) then
      ScreenBorder := r1.BottomRight
    else
      ScreenBorder := Point(Screen.Width,Screen.Height);
    p := Box.ClientToScreen(Point(0,0));
    r2 := Box.GetScreenRect(Box.FFindRect);
    OffsetRect(r2,p.x,p.y);
    GetWindowRect(FFindDialog.Handle,r1);
    if ScreenBorder.y-r2.Bottom<r1.Bottom-r1.Top then
      p.y := r2.Top-(r1.Bottom-r1.Top)
    else
      p.y := r2.Bottom;
    if ScreenBorder.x-r2.Left<r1.Right-r1.Left then
      p.x := ScreenBorder.x-(r1.Right-r1.Left)
    else
      p.x := r2.Left;
    FFindDialog.Position := p;
  end
else
  begin
    Box.ResetFindRect;
    MBox(Format(prLoadStr(sTextNotFound),[FFindDialog.FindText]),prLoadStr(sAttention),MB_OK+MB_ICONEXCLAMATION);
  end;
end;

function TprTxPreviewPanel.IsFindMode : boolean;
begin
Result := not Box.IsEmptyFindRect;
end;

procedure TprTxPreviewPanel.Find;
begin
if ReportEmpty then exit;
Box.ResetFindRect;
FFindDialog.Execute;
end;

procedure TprTxPreviewPanel.FindNext;
var
  FindedPos,FromPos : TPoint;
begin
if not IsFindMode then exit;
FromPos.x := Box.FFindRect.Left+1;
FromPos.y := Box.FFindRect.Top;
if FindText(FFindDialog.FindText,frMatchCase in FFindDialog.Options,true,FromPos,FindedPos) then
  begin
    Box.SetFindRect(Rect(FindedPos.x,FindedPos.y,FindedPos.x+Length(FFindDialog.FindText),FindedPos.y+1));
    SetCursorPos(Box.FFindRect.Right,Box.FFindRect.Top);
  end
else
  begin
    MBox(Format(prLoadStr(sTextNotFound),[FFindDialog.FindText]),prLoadStr(sAttention),MB_OK+MB_ICONEXCLAMATION);
  end;
end;

procedure TprTxPreviewPanel.FindPrior;
var
  FindedPos,FromPos : TPoint;
begin
if not IsFindMode then exit;
FromPos.x := Box.FFindRect.Left-1;
FromPos.y := Box.FFindRect.Top;
if FindText(FFindDialog.FindText,frMatchCase in FFindDialog.Options,false,FromPos,FindedPos) then
  begin
    Box.SetFindRect(Rect(FindedPos.x,FindedPos.y,FindedPos.x+Length(FFindDialog.FindText),FindedPos.y+1));
    SetCursorPos(Box.FFindRect.Right,Box.FFindRect.Top,false); 
  end
else
  begin
    MBox(Format(prLoadStr(sTextNotFound),[FFindDialog.FindText]),prLoadStr(sAttention),MB_OK+MB_ICONEXCLAMATION);
  end;
end;

procedure TprTxPreviewPanel.SetSelection(SelectionMode : TprTxPreviewSelectionMode; SelectionRect : TRect);
begin
if SelectionMode=prtsmStandart then
  begin
    if SelectionRect.Top>SelectionRect.Bottom then
      Exchange(SelectionRect.Top,SelectionRect.Bottom);
    if SelectionRect.Bottom-SelectionRect.Top=1 then
      if SelectionRect.Left>SelectionRect.Right then
        Exchange(SelectionRect.Left,SelectionRect.Right);
  end
else
  begin
    SelectionRect.Left := Max(0,SelectionRect.Left);
    SelectionRect.Top := Max(0,SelectionRect.Top);
    SelectionRect.Right := Min(MaxLineLength,SelectionRect.Right);
    SelectionRect.Bottom := Min(LinesCount,SelectionRect.Bottom);
    SelectionRect := NormalizeRect(SelectionRect.Left,SelectionRect.Top,SelectionRect.Right,SelectionRect.Bottom);
  end;
Box.SetSelectionRect(SelectionMode,SelectionRect);
end;

function TprTxPreviewPanel.SelectionExists : boolean;
begin
Result := not Box.IsEmptySelectionRect;
end;

procedure TprTxPreviewPanel.Copy;
begin
Clipboard.Open;
try
  ClipBoard.AsText := SelectedText;
finally
  Clipboard.Close;                       
end;
end;

function TprTxPreviewPanel.GetSelectedText : string;
var
  i : integer;
begin
Result := '';
if SelectionExists then
  begin
    with Box do
      case FSelectionMode of
        prtsmStandart:
          begin
            if FSelectionRect.Bottom-FSelectionRect.Top=1 then
              Result := System.Copy(TxReportOptions.RemoveESCFromString(Text[FSelectionRect.Top]),FSelectionRect.Left+1,FSelectionRect.Right-FSelectionRect.Left)
            else
              begin
                Result := System.Copy(TxReportOptions.RemoveESCFromString(Text[FSelectionRect.Top]),FSelectionRect.Left+1,MaxInt)+#13#10;
                for i:=FSelectionRect.Top+1 to FSelectionRect.Bottom-2 do
                  Result := Result+TxReportOptions.RemoveESCFromString(Text[i])+#13#10;
                if FSelectionRect.Right>0 then
                  Result := Result+System.Copy(TxReportOptions.RemoveESCFromString(Text[FSelectionRect.Bottom-1]),1,FSelectionRect.Right);
              end;
          end;
        prtsmRect:
          begin
            for i:=FSelectionRect.Top to FSelectionRect.Bottom-1 do
              Result := Result+System.Copy(TxReportOptions.RemoveESCFromString(Text[i]),FSelectionRect.Left+1,FSelectionRect.Right-FSelectionRect.Left)+#13#10;
          end;
      end;
    Report.OemToWin(PChar(Result),PChar(Result));
  end
end;

procedure TprTxPreviewPanel.LoadPreparedReport(const FileName : string);
begin
  Report.LoadPreparedReportFromFile(FileName);
  UpdatePanel;
end;

procedure TprTxPreviewPanel.FullUpdate;
begin
  if Report <> nil then
    Box.Canvas.Font.Charset := Report.FontCharSet
  else
    Box.Canvas.Font.Charset := OEM_CHARSET;
  UpdatePanel;
end;
    
end.
