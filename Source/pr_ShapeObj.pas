{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the TprShapeObj class that is intended for inserting some shapes (rectangle, rounded rectangle, ellipse, triangle) in your report.}
unit pr_ShapeObj;

interface

{$i pr.inc}

uses
  Windows, Classes, SysUtils, Graphics, Menus,

  pr_Common, pr_Classes;

type

  TprShapePainterClass = class of TprShapePainter;
  /////////////////////////////////////////////////
  //
  // TprShapePainter
  //
  /////////////////////////////////////////////////
{Internal class, base class for all painters of TprShapeObj.}
  TprShapePainter = class(TObject)
  protected
    class function CreatePen(APenStyle: TPenStyle; APenColor: TColor; APenWidth: Integer): HPEN;
    class procedure DrawLine(DC: HDC; APen: HPEN; X1, Y1, X2, Y2: Integer);
    class procedure InternalDraw(DC: HDC; const ADrawRect: TRect;
                                 APenStyle: TPenStyle;
                                 APenColor: TColor;
                                 APenWidth: Integer;
                                 ABrushStyle: TBrushStyle;
                                 ABrushColor: TColor); virtual;
    class procedure Draw(DC: HDC; const ADrawRect: TRect;
                         APenStyle: TPenStyle;
                         APenColor: TColor;
                         APenWidth: Integer;
                         ABrushStyle: TBrushStyle;
                         ABrushColor: TColor);
  end;

  /////////////////////////////////////////////////
  //
  // TprLineShapePainter
  //
  /////////////////////////////////////////////////
{Internal class, base class for all line painters of TprShapeObj.}
  TprLineShapePainter = class(TprShapePainter)
  protected
    class procedure GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer); virtual;
    class procedure InternalDraw(DC: HDC; const ADrawRect: TRect;
                                 APenStyle: TPenStyle;
                                 APenColor: TColor;
                                 APenWidth: Integer;
                                 ABrushStyle: TBrushStyle;
                                 ABrushColor: TColor); override;
  end;

  /////////////////////////////////////////////////
  //
  // TprCenterHorzLinePainter
  //
  /////////////////////////////////////////////////
{Internal class, draws the centered horizontal line in TprShapeObj.}
  TprCenterHorzLinePainter = class(TprLineShapePainter)
  protected
    class procedure GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer); override;
  end;

  /////////////////////////////////////////////////
  //
  // TprBottomLinePainter
  //
  /////////////////////////////////////////////////
{Internal class, draws the line on bottom edge of TprShapeObj.}
  TprBottomLinePainter = class(TprLineShapePainter)
  protected
    class procedure GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer); override;
  end;

  /////////////////////////////////////////////////
  //
  // TprTopLinePainter
  //
  /////////////////////////////////////////////////
{Internal class, draws the line on top edge of TprShapeObj.}
  TprTopLinePainter = class(TprLineShapePainter)
  protected
    class procedure GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer); override;
  end;

  /////////////////////////////////////////////////
  //
  // TprCenterVertLinePainter
  //
  /////////////////////////////////////////////////
{Internal class, draws the vertical centered line in TprShapeObj.}
  TprCenterVertLinePainter = class(TprLineShapePainter)
  protected
    class procedure GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer); override;
  end;

  /////////////////////////////////////////////////
  //
  // TprLeftLinePainter
  //
  /////////////////////////////////////////////////
{Internal class, draws the line on left edge of TprShapeObj.}
  TprLeftLinePainter = class(TprLineShapePainter)
  protected
    class procedure GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer); override;
  end;

  /////////////////////////////////////////////////
  //
  // TprRightLinePainter
  //
  /////////////////////////////////////////////////
{Internal class, draws the line on right edge of TprShapeObj.}
  TprRightLinePainter = class(TprLineShapePainter)
  protected
    class procedure GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer); override;
  end;

  /////////////////////////////////////////////////
  //
  // TprBoxPainter
  //
  /////////////////////////////////////////////////
{Internal class, draws the box on TprShapeObj.}
  TprBoxPainter = class(TprShapePainter)
  protected
    class procedure InternalDraw(DC: HDC; const ADrawRect: TRect;
                                 APenStyle: TPenStyle;
                                 APenColor: TColor;
                                 APenWidth: Integer;
                                 ABrushStyle: TBrushStyle;
                                 ABrushColor: TColor); override;
  end;

  /////////////////////////////////////////////////
  //
  // TprEllipsePainter
  //
  /////////////////////////////////////////////////
{Internal class, draws the ellipse on TprShapeObj.}
  TprEllipsePainter = class(TprShapePainter)
  protected
    class procedure InternalDraw(DC: HDC; const ADrawRect: TRect;
                                 APenStyle: TPenStyle;
                                 APenColor: TColor;
                                 APenWidth: Integer;
                                 ABrushStyle: TBrushStyle;
                                 ABrushColor: TColor); override;
  end;

{Describes the type of shape.
Items:
  prssCenterHorzLine - Draws the centered horizontal line.
  prssBottomLine - Draws the line on the bottom edge of object.
  prssTopLine - Draws the line on the top edge of object.
  prssCenterVertLine - Draws the centered vertical line.
  prssLeftLine - Draws the line on the left edge of object.
  prssRightLine - Draws the line on the right edge of object.
  prssBox - Draws the box.
  prssEllipse - Draws the ellipse.
Syntax:
  TprShapeObjStyle = (prssCenterHorzLine, prssBottomLine, prssTopLine, prssCenterVertLine, prssLeftLine, prssRightLine, prssBox, prssEllipse);
}
  TprShapeObjStyle = (prssCenterHorzLine, prssBottomLine, prssTopLine, prssCenterVertLine, prssLeftLine, prssRightLine, prssBox, prssEllipse);
  /////////////////////////////////////////////////
  //
  // TprShapeObjRecVersion
  //
  /////////////////////////////////////////////////
{Represents the object view for TprShapeObj.
See also:
  TprShapeObj}
  TprShapeObjRecVersion = class(TprExObjRecVersion)
  private
    FStyle: TprShapeObjStyle;
    FPenStyle: TPenStyle;
    FPenColor: TColor;
    FPenWidth: Integer;
    FBrushStyle: TBrushStyle;
    FBrushColor: TColor; 
  public
{Creates an instance of the TprShapeObjRecVersion class.}
    constructor Create(Collection: TCollection); override;

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
{Specifies the type of shape.
See also:
  TprShapeObjStyle}
    property Style: TprShapeObjStyle read FStyle write FStyle default prssCenterHorzLine;
{Specifies the pen style, that is used for painting lines and border of rectangle and ellipse.}
    property PenStyle: TPenStyle read FPenStyle write FPenStyle default psSolid;
{Specifies the pen color.}
    property PenColor: TColor read FPenColor write FPenColor default clBlack;
{Specifies the pen width.}
    property PenWidth: Integer read FPenWidth write FPenWidth default 1;
{Specifies the style of brush that is used for filling the object,
if BrushStyle equals to bsClear object is transparent.}
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle default bsSolid;
{Specifies the color of brush.}
    property BrushColor: TColor read FBrushColor write FBrushColor default clNone;
  end;

  /////////////////////////////////////////////////
  //
  // TprShapeObjRec
  //
  /////////////////////////////////////////////////
{Represents a record of properties for the TprShapeObj object.
See also:
  TprShapeObj, TprShapeObjRecVersion}
  TprShapeObjRec = class(TprExObjRec)
  protected
    function GetVersionClass: TprObjVersionClass; override;
  end;

  /////////////////////////////////////////////////
  //
  // TprShapeObj
  //
  /////////////////////////////////////////////////
{The TprShapeObj class is intended for inserting some shapes (rectangle, rounded rectangle, ellipse, triangle) in your report.
As all other objects in PReport the TprShapeObj can have many views (it has one view after creating)
the necessary  object view is selected on the stage of report generating.
See also:
  TprShapeObjRecVersion}
  TprShapeObj = class(TprExObj)
  private
    function GetVersion(Index: Integer): TprShapeObjRecVersion;
    function GetGenVersion(Index: Integer): TprShapeObjRecVersion;
    function GetDefVersion: TprShapeObjRecVersion;
    function GetGenCurVersion: TprShapeObjRecVersion;
  protected
    procedure InitdRec; override;
    procedure OnDsgnPopupMenuClick(Sender : TObject);
  public
{See:
  TprObj.DrawDesign}
    procedure DrawDesign(DC: HDC; ExData: pointer; const DrawRect: TRect); override;
{See:
  TprObj.GetDesc}
    function GetDesc: string; override;
{See:
  TprObj.DsgnIsTransparent}
    function DsgnIsTransparent: Boolean; override;
{See:
  TprObj.DsgnDefinePopupMenu}
    procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: Boolean); override;
{See:
  TprObj.FirstPass}
    procedure FirstPass; override;

{See:
  TprObj.Versions}
    property Versions[Index: Integer]: TprShapeObjRecVersion read GetVersion;
{See:
  TprObj.GenVersions}
    property GenVersions[Index: Integer]: TprShapeObjRecVersion read GetGenVersion;
{See:
  TprObj.DefVersion}
    property DefVersion: TprShapeObjRecVersion read GetDefVersion;
{See:
  TprObj.GenCurVersion}
    property GenCurVersion: TprShapeObjRecVersion read GetGenCurVersion;
  end;

implementation

uses
  pr_Utils, pr_Strings, pr_MultiLang, Math;

const
  ShapePainters: array [TprShapeObjStyle] of TprShapePainterClass =
                 (TprCenterHorzLinePainter,
                  TprBottomLinePainter,
                  TprTopLinePainter,
                  TprCenterVertLinePainter,
                  TprLeftLinePainter,
                  TprRightLinePainter,
                  TprBoxPainter,
                  TprEllipsePainter);

/////////////////////////////////////////////////
//
// TprShapePainter
//
/////////////////////////////////////////////////
class function TprShapePainter.CreatePen(APenStyle: TPenStyle; APenColor: TColor; APenWidth: Integer): HPEN;
begin
  Result := prCreatePen(APenStyle, APenColor, APenWidth);
end;

class procedure TprShapePainter.DrawLine(DC: HDC; APen: HPEN; X1, Y1, X2, Y2: Integer);
var
  AOldPen: HPEN;
begin
  AOldPen := SelectObject(DC, APen);
  MoveToEx(DC, X1, Y1, nil);
  LineTo(DC, X2, Y2);
  SelectObject(DC, AOldPen);
end;

class procedure TprShapePainter.Draw(DC: HDC; const ADrawRect: TRect;
                                     APenStyle: TPenStyle;
                                     APenColor: TColor;
                                     APenWidth: Integer;
                                     ABrushStyle: TBrushStyle;
                                     ABrushColor: TColor);
var
  AOldClipRgn, AClipRgn: HRGN;
  AClipRect: TRect;
  AOrgEx: TPoint;
begin
  AClipRect := ADrawRect;
  GetViewportOrgEx(DC, AOrgEx);
  OffsetRect(AClipRect, AOrgEx.X, AOrgEx.Y);
  AClipRgn := CreateRectRgnIndirect(AClipRect);
  AOldClipRgn := CreateRectRgnIndirect(AClipRect);
  if GetClipRgn(DC, AOldClipRgn) = 0 then
  begin
    DeleteObject(AOldClipRgn);
    AOldClipRgn := 0;
    SelectClipRgn(DC, AClipRgn);
  end
  else
    ExtSelectClipRgn(DC, AClipRgn, RGN_AND);
  DeleteObject(AClipRgn);

  InternalDraw(DC, ADrawRect, APenStyle, APenColor, APenWidth, ABrushStyle, ABrushColor);

  SelectClipRgn(DC, AOldClipRgn);
  if AOldClipRgn <> 0 then
    DeleteObject(AOldClipRgn);
end;

class procedure TprShapePainter.InternalDraw(DC: HDC; const ADrawRect: TRect;
                                 APenStyle: TPenStyle;
                                 APenColor: TColor;
                                 APenWidth: Integer;
                                 ABrushStyle: TBrushStyle;
                                 ABrushColor: TColor);
begin
end;

/////////////////////////////////////////////////
//
// TprLineShapePainter
//
/////////////////////////////////////////////////
class procedure TprLineShapePainter.GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer);
begin
end;

class procedure TprLineShapePainter.InternalDraw(DC: HDC; const ADrawRect: TRect;
                                                 APenStyle: TPenStyle;
                                                 APenColor: TColor;
                                                 APenWidth: Integer;
                                                 ABrushStyle: TBrushStyle;
                                                 ABrushColor: TColor);
var
  ABrush: HBRUSH;
  APen: HPEN;
  X1, Y1, X2, Y2: Integer;
begin
  if (ABrushStyle <> bsClear) and (ABrushColor <> clNone) then
  begin
    ABrush := prCreateBrush(ABrushStyle, ABrushColor);
    FillRect(DC, ADrawRect, ABrush);
    DeleteObject(ABrush);
  end;
  if (APenStyle <> psClear) and (APenColor <> clNone) then
  begin
    GetLineCoords(ADrawRect, APenWidth, X1, Y1, X2, Y2);
    APen := prCreatePen(APenStyle, APenWidth, APenColor);
    DrawLine(DC, APen, X1, Y1, X2, Y2);
    DeleteObject(APen);
  end;
end;

/////////////////////////////////////////////////
//
// TprCenterHorzLinePainter
//
/////////////////////////////////////////////////
class procedure TprCenterHorzLinePainter.GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer);
begin
  with ADrawRect do
  begin
    X1 := Left;
    Y1 := Top + (Bottom - Top) div 2;
    X2 := Right;
    Y2 := Y1;
  end;
end;

/////////////////////////////////////////////////
//
// TprBottomLinePainter
//
/////////////////////////////////////////////////
class procedure TprBottomLinePainter.GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer);
begin
  with ADrawRect do
  begin
    X1 := Left;
    Y1 := Bottom - (ALineWidth div 2) - (ALineWidth mod 2);
    X2 := Right;
    Y2 := Y1;
  end;
end;

/////////////////////////////////////////////////
//
// TprTopLinePainter
//
/////////////////////////////////////////////////
class procedure TprTopLinePainter.GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer);
begin
  with ADrawRect do
  begin
    X1 := Left;
    Y1 := Top + (ALineWidth - 1) div 2;
    X2 := Right;
    Y2 := Y1;
  end;
end;

/////////////////////////////////////////////////
//
// TprCenterVertLinePainter
//
/////////////////////////////////////////////////
class procedure TprCenterVertLinePainter.GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer);
begin
  with ADrawRect do
  begin
    X1 := Left + (Right - Left) div 2;
    Y1 := Top;
    X2 := X1;
    Y2 := Bottom;
  end;
end;

/////////////////////////////////////////////////
//
// TprLeftLinePainter
//
/////////////////////////////////////////////////
class procedure TprLeftLinePainter.GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer);
begin
  with ADrawRect do
  begin
    X1 := Left + (ALineWidth - 1) div 2;
    Y1 := Top;
    X2 := X1;
    Y2 := Bottom;
  end;
end;

/////////////////////////////////////////////////
//
// TprRightLinePainter
//
/////////////////////////////////////////////////
class procedure TprRightLinePainter.GetLineCoords(const ADrawRect: TRect; ALineWidth: Integer; var X1, Y1, X2, Y2: Integer);
begin
  with ADrawRect do
  begin
    X1 := Right - (ALineWidth div 2) - (ALineWidth mod 2);
    Y1 := Top;
    X2 := X1;
    Y2 := Bottom;
  end;
end;

/////////////////////////////////////////////////
//
// TprBoxPainter
//
/////////////////////////////////////////////////
class procedure TprBoxPainter.InternalDraw(DC: HDC; const ADrawRect: TRect;
                                           APenStyle: TPenStyle;
                                           APenColor: TColor;
                                           APenWidth: Integer;
                                           ABrushStyle: TBrushStyle;
                                           ABrushColor: TColor);
var
  APen, AOldPen: HPEN;
  ABrush, AOldBrush: HBRUSH;
begin
  APen := prCreatePen(APenStyle, APenWidth, APenColor);
  ABrush := prCreateBrush(ABrushStyle, ABrushColor);
  AOldPen := SelectObject(DC, APen);
  AOldBrush := SelectObject(DC, ABrush);

  with ADrawRect do
    Rectangle(DC,
              Left + (APenWidth div 2),
              Top + (APenWidth div 2),
              Right - (APenWidth div 2) + ((APenWidth mod 2) xor 1),
              Bottom - (APenWidth div 2) + ((APenWidth mod 2) xor 1));
              
  SelectObject(DC, AOldPen);
  SelectObject(DC, AOldBrush);
  DeleteObject(APen);
  DeleteObject(ABrush);
end;

/////////////////////////////////////////////////
//
// TprEllipsePainter
//
/////////////////////////////////////////////////
class procedure TprEllipsePainter.InternalDraw(DC: HDC; const ADrawRect: TRect;
                                               APenStyle: TPenStyle;
                                               APenColor: TColor;
                                               APenWidth: Integer;
                                               ABrushStyle: TBrushStyle;
                                               ABrushColor: TColor);
var
  APen, AOldPen: HPEN;
  ABrush, AOldBrush: HBRUSH;
begin
  APen := prCreatePen(APenStyle, APenWidth, APenColor);
  ABrush := prCreateBrush(ABrushStyle, ABrushColor);
  AOldPen := SelectObject(DC, APen);
  AOldBrush := SelectObject(DC, ABrush);

  with ADrawRect do
    Ellipse(DC,
            Left + (APenWidth div 2),
            Top + (APenWidth div 2),
            Right - (APenWidth div 2) + ((APenWidth mod 2) xor 1),
            Bottom - (APenWidth div 2) + ((APenWidth mod 2) xor 1));
              
  SelectObject(DC, AOldPen);
  SelectObject(DC, AOldBrush);
  DeleteObject(APen);
  DeleteObject(ABrush);
end;

/////////////////////////////////////////////////
//
// TprShapeObjRecVersion
//
/////////////////////////////////////////////////
constructor TprShapeObjRecVersion.Create(Collection: TCollection);
begin
  inherited;
  FPenStyle := psSolid;
  FPenColor := clBlack;
  FPenWidth := 1;
  FBrushStyle := bsSolid;
  FBrushColor := clNone;
end;

procedure TprShapeObjRecVersion.Assign(Source: TPersistent);
begin
  if Source is TprShapeObjRecVersion then
    with TprShapeObjRecVersion(Source) do
    begin
      Self.FStyle := Style;
      Self.FPenStyle := PenStyle;
      Self.FPenColor := PenColor;
      Self.FPenWidth := PenWidth;
      Self.FBrushStyle := BrushStyle;
      Self.FBrushColor := BrushColor;
    end;
  inherited;
end;

procedure TprShapeObjRecVersion.Draw(DC: HDC; pdi: PPrDrawInfo);
var
  ARealRect: TRect;
begin
  ARealRect := GetRealRect(pdi);
  ShapePainters[Style].Draw(DC, ARealRect,
                            PenStyle,
                            PenColor,
                            Max(1, Round(PenWidth * Max(pdi.kx, pdi.ky))),
                            BrushStyle,
                            BrushColor);
end;

procedure TprShapeObjRecVersion.InitInDesigner;
begin
end;

/////////////////////////////////////////////////
//
// TprShapeObjRec
//
/////////////////////////////////////////////////
function TprShapeObjRec.GetVersionClass: TprObjVersionClass;
begin
  Result := TprShapeObjRecVersion;
end;

/////////////////////////////////////////////////
//
// TprShapeObj
//
/////////////////////////////////////////////////
function TprShapeObj.GetVersion(Index: Integer): TprShapeObjRecVersion;
begin
  Result := TprShapeObjRecVersion(inherited Versions[Index]);
end;

function TprShapeObj.GetGenVersion(Index: Integer): TprShapeObjRecVersion;
begin
  Result := TprShapeObjRecVersion(inherited GenVersions[Index]);
end;

function TprShapeObj.GetDefVersion: TprShapeObjRecVersion;
begin
  Result := TprShapeObjRecVersion(inherited DefVersion);
end;

function TprShapeObj.GetGenCurVersion: TprShapeObjRecVersion;
begin
  Result := TprShapeObjRecVersion(inherited GenCurversion);
end;

procedure TprShapeObj.InitdRec;
begin
  FdRec := TprShapeObjRec.Create(nil, Self);
  TprExObjRecVersion(dRec.Versions.Add).InitInDesigner;
end;

procedure TprShapeObj.OnDsgnPopupMenuClick(Sender : TObject);
begin
  case TMenuItem(Sender).Tag of
    -1: case TprShapeObjRecVersion(DefVersion).Style of
          prssCenterHorzLine, prssBottomLine, prssTopLine:
           dRec.Bottom := dRec.Top + TprShapeObjRecVersion(DefVersion).PenWidth;
          prssCenterVertLine, prssRightLine, prssLeftLine:
           dRec.Right := dRec.Left + TprShapeObjRecVersion(DefVersion).PenWidth;
        end;
    else
      TprShapeObjRecVersion(DefVersion).Style := TprShapeObjStyle(TMenuItem(Sender).Tag);
  end;
  DsgnNotifyDesigner;
end;

function TprShapeObj.DsgnIsTransparent: Boolean;
begin
  with TprShapeObjRecVersion(DefVersion) do
    Result := (Style = prssEllipse) or (BrushStyle = bsClear) or (BrushColor = clNone);
end;

procedure TprShapeObj.DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: Boolean);
var
  I: Integer;
  m: TMenuItem;
begin
  inherited;
  if Popup.Items.Count > 0 then
    AddPopupMenuItem(Popup, nil, 0, '', nil, '', 0, False, False);

  AddPopupMenuItem(Popup, nil, sShapeObjSetSizeAsPenWidth, '', OnDsgnPopupMenuClick, '', -1,
                   TprShapeObjRecVersion(DefVersion).Style in [prssCenterHorzLine, prssBottomLine, prssTopLine, prssCenterVertLine, prssRightLine, prssLeftLine], False);
  m := AddPopupMenuItem(Popup, nil, sShapeStyleMenuCaption, '', nil, '', 0, True, False);
  for I := Integer(Low(TprShapeObjStyle)) to Integer(High(TprShapeObjStyle)) do
    AddPopupMenuItem(Popup, m, sShapeStylesFirstIndex - I, '', OnDsgnPopupMenuClick, '', I, True, I = Integer(TprShapeObjRecVersion(DefVersion).Style));
end;

procedure TprShapeObj.DrawDesign(DC: HDC; ExData: pointer; const DrawRect: TRect);
begin
  with TprShapeObjRecVersion(DefVersion) do
    ShapePainters[Style].Draw(DC, DrawRect,
                              PenStyle,
                              PenColor,
                              PenWidth,
                              BrushStyle,
                              BrushColor);
  DrawAngleRect(DC, DrawRect);
end;

function TprShapeObj.GetDesc: string;
begin
  Result := prLoadStr(-Integer(TprShapeObjRecVersion(DefVersion).Style) + sShapeStylesFirstIndex);
end;

procedure TprShapeObj.FirstPass;
var
  ManuallyProcessed: boolean;
begin
  if FaRec = nil then
    FaRec := TprShapeObjRec.Create(nil, Self);
  aRec.Assign(dRec);

  // select version
  aRec.FirstPass;

  // OnFirstPassObject
  DoOnFirstPassObject(ManuallyProcessed);

  aRec.pRect := dRec.pRect;

  inherited;
end;

initialization

  Classes.RegisterClass(TprShapeObj);
  Classes.RegisterClass(TprShapeObjRecVersion);
  // register objects
  prRegisterObj(TprShapeObj,
                TprShapeObjRec,
                TprReport,
                sShapeObjCaption,
                sShapeObjHint,
                'TprShapeEditorForm',
                'TprPrvShapeEditorForm');

end.








