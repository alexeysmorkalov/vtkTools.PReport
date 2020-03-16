{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the TprDesignerPanel class and some auxiliary classes.
See also:
  TprDesignerPanel}
unit pr_DesignerPanel;

interface

{$I PR.INC}

uses
  Windows, Classes, Graphics, SysUtils, IniFiles, typinfo, forms, menus,

  pr_Common, pr_Classes, pr_CommonDesignerPanel, pr_DesignerFunctions;

type
/////////////////////////////////////////////////
//
// TprDesignerRuler
//
/////////////////////////////////////////////////
TprDesignerRuler = class(TprCustomDesignerRuler)
private
  FRulerUnits : TprPosSizeUnits;
  procedure SetRulerUnits(Value : TprPosSizeUnits);
protected
  procedure Paint; override;
  function GetScrollOffset : integer; virtual; abstract;
  function GetStep : integer; virtual; abstract;
  function GetFont(Font : TFont) : HFONT; virtual; abstract;
  function GetMaxPixelSize : integer; virtual; abstract;
  procedure DrawMarker(Canvas : TCanvas; Offs : integer); virtual; abstract;
  procedure DrawLabel(Canvas : TCanvas; const LabelText : string; Offs : integer); virtual; abstract;
  procedure DrawBackground(Canvas : TCanvas); virtual; abstract;
public
  property RulerUnits : TprPosSizeUnits read FRulerUnits write SetRulerUnits;

  constructor Create(AOwner : TComponent); override;
end;

/////////////////////////////////////////////////
//
// TprHorDesignerRuler
//
/////////////////////////////////////////////////
TprHorDesignerRuler = class(TprDesignerRuler)
protected
  function GetScrollOffset : integer; override;
  function GetStep : integer; override;
  function GetFont(Font : TFont) : HFONT; override;
  function GetMaxPixelSize : integer; override;
  procedure DrawMarker(Canvas : TCanvas; Offs : integer); override;
  procedure DrawLabel(Canvas : TCanvas; const LabelText : string; Offs : integer); override;
  procedure DrawBackground(Canvas : TCanvas); override;
public
  constructor Create(AOwner : TComponent); override;
end;

/////////////////////////////////////////////////
//
// TprVerDesignerRuler
//
/////////////////////////////////////////////////
TprVerDesignerRuler = class(TprDesignerRuler)
protected
  function GetScrollOffset : integer; override;
  function GetStep : integer; override;
  function GetFont(Font : TFont) : HFONT; override;
  function GetMaxPixelSize : integer; override;
  procedure DrawMarker(Canvas : TCanvas; Offs : integer); override;
  procedure DrawLabel(Canvas : TCanvas; const LabelText : string; Offs : integer); override;
  procedure DrawBackground(Canvas : TCanvas); override;
public
  constructor Create(AOwner : TComponent); override;
end;

/////////////////////////////////////////////////
//
// TprDesignerBox
//
/////////////////////////////////////////////////
TprDesignerBox = class(TprCustomDesignerBox)
private
  FGridBitmap : HBITMAP;
protected
  procedure DrawGrid(DC : HDC; BackRgn : HRGN); override;
  procedure DrawOther(DC : HDC); override;
  function DrawWidth : integer; override;
  function DrawHeight : integer; override;
public
  constructor Create(AOwner : TComponent); override;
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TprRulerOptions
//
/////////////////////////////////////////////////
{Represents the options for horizontal and vertical rulers on the designer panel.}
TprRulerOptions = class(TprOptions)
private
  FVisible : boolean;
  FRulerUnits : TprPosSizeUnits;
  procedure SetVisible(Value : boolean);
  procedure SetRulerUnits(Value : TprPosSizeUnits);
public
{Creates an instance of the TprRulerOptions class.}
  constructor Create;

{Copies the contents of another, similar object.
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
{Indicates whether the ruler is visible.}
  property Visible : boolean read FVisible write SetVisible default true;
{Specifies the units of measurement for ruler.
See also:
  TprPosSizeUnits}
  property RulerUnits : TprPosSizeUnits read FRulerUnits write SetRulerUnits default prpsuSm;
end;

/////////////////////////////////////////////////
//
//  TprDesignerPanel
//
/////////////////////////////////////////////////
{Represents the control for editing the TprReport report template.
You can use this control if you want to develop non standard report designers
with some unique features.
See demo "12_Drag-Drop" as example of using this control.
See also:
  TprCustomDesignerPanel}
TprDesignerPanel = class(TprCustomDesignerPanel)
private
  FReport : TprReport;
  FGridSize : integer;
  FShowGrid : boolean;
  FUseGrid : boolean;
  FHorRulerOptions : TprRulerOptions;
  FVerRulerOptions : TprRulerOptions;
  procedure SetGridSize(Value : integer);
  procedure SetHorRulerOptions(Value : TprRulerOptions);
  procedure SetVerRulerOptions(Value : TprRulerOptions);

  procedure OnRulerOptionsChange(Sender : TObject);
  procedure OnPopupMainMenuClick(Sender : TObject);
protected
  procedure SetReport(Value : TprReport);
  function CreateDesignerBox : TprCustomDesignerBox; override;
  procedure CreateRulers(var HorRuler,VerRuler : TprCustomDesignerRuler); override;
  function CreatePosSizeForm : TprCustomPosSizeForm; override;
  function GetShowGrid : boolean; override;
  procedure SetShowGrid(Value : boolean); override;
  function GetUseGrid : boolean; override;
  procedure SetUseGrid(Value : boolean); override;
  procedure AdjustToGrid(var X,Y : integer); override;
  function GetDefaultHeightForHorizontalBand : integer; override;
  function GetDefaultWidthForVerticalBand : integer; override;

  procedure InitPopupMainMenu(Popup : TPopupMenu; MainMenuItem : TMenuItem); override;

  procedure Notification(AComponent : TComponent; AOperation : TOperation); override;

  function GridSizeX : integer; override;
  function GridSizeY : integer; override;
public
{See:
  TprCustomDesignerPanel.GetReport}
  function GetReport: TprCustomReport; override;
{See:
  TprCustomDesignerPanel.UpdateCurPage}
  procedure UpdateCurPage; override;

{See:
  TprCustomDesignerPanel.WriteToIni}
  procedure WriteToIni(IniFile : TIniFile; const SectionName : string); override;
{See:
  TprCustomDesignerPanel.ReadFromIni}
  procedure ReadFromIni(IniFile : TIniFile; const SectionName : string); override;

{See:
  TprCustomDesignerPanel.EditOptions}
  function EditOptions: boolean; override;
{Opens the built-in dialog for editing the grid size.}
  procedure EditGridSize;
{See:
  TprCustomDesignerPanel.EditReportParams}
  procedure EditReportParams; override;
{See:
  TprCustomDesignerPanel.EditPage}
  function EditPage(PageIndex : integer) : boolean; override;

{Creates an instace the the TprCustomDesignerPanel class.}
  constructor Create(AOwner : TComponent); override;
{Frees an instace the the TprCustomDesignerPanel class.}
  destructor Destroy; override;
published
  property TabStop;
  property TabOrder;

{Specifies the TprReport object, that holds the report template.}
  property Report: TprReport read FReport write SetReport;
{See:
  TprCustomDesignerPanel.ActivePageIndex}
  property ActivePageIndex;
{Specifies when objects of the report template can be stuck while they been drag or been resize by mouse.
See also:
  TprStuckMode}
  property StuckMode default DefaultStuckMode;
{Specifies the "stuck" options.
See also:
  TprStuckOptionsSet, TprStuckOptions}
  property StuckOptions default DefaultStuckOptions;
{Specifies the minimum distance between objects from that they can stuck.}
  property StuckOffs default DefaultStuckOffs;
{Specifies the options of the horizontal ruler.
See also:
  TprRulerOptions}
  property HorRulerOptions: TprRulerOptions read FHorRulerOptions write SetHorRulerOptions;
{Specifies the options of the horizontal ruler.
See also:
  TprRulerOptions}
  property VerRulerOptions: TprRulerOptions read FVerRulerOptions write SetVerRulerOptions;
{See:
  TprCustomDesignerPanel.HorBandsCaptionsOptions}
  property HorBandsCaptionsOptions;
{See:
  TprCustomDesignerPanel.VerBandsCaptionsOptions}
  property VerBandsCaptionsOptions;
{Specifies the grid size that is used in designer.}
  property GridSize : integer read FGridSize write SetGridSize default 8;
{Indicates whether the grid is visible.}
  property ShowGrid default false;
{Indicates whether the objects must be aligned to grid.}
  property UseGrid default false;
{See:
  TprCustomDesignerPanel.OpacityObjectLinksForm}
  property OpacityObjectLinksForm default 100;
{See:
  TprCustomDesignerPanel.OpacityPosSizeForm}
  property OpacityPosSizeForm default 100;
{See:
  TprCustomDesignerPanel.OpacityObjectsPropsForm}
  property OpacityObjectsPropsForm default 100;
{See:
  TprCustomDesignerPanel.OpacityFindForm}
  property OpacityFindForm default 100;
end;

implementation

uses
  pr_Utils, pr_PageParams, pr_ObjectPosSizeForm, pr_DesignerPanelOptions,
  pr_ImageEditor, pr_MemoEditor, pr_RichEditor, pr_BandEditor, pr_Strings,
  pr_ReportParams, pr_GridSize, pr_MultiLang, pr_ShapeEditor, pr_BarCodeEditor;

const
  aDesignerRulerSteps: array [TprPosSizeUnits] of double = (30, 10, 1, 0.5, 0.01);
  aDesignerRulerFormats: array [TprPosSizeUnits] of string = ('0', '0', '0', '0.0', '0.00');
  
/////////////////////////////////////////////////
//
// TprDesignerRuler
//
/////////////////////////////////////////////////
constructor TprDesignerRuler.Create(AOwner : TComponent);
begin
inherited;
FRulerUnits := prpsuSm;
end;

procedure TprDesignerRuler.SetRulerUnits(Value : TprPosSizeUnits);
begin
if FRulerUnits=Value then exit;
FRulerUnits := Value;
Repaint;
end;

procedure TprDesignerRuler.Paint;
var
  n : extended;
  s : string;
  Step,i,mw : integer;
  newfont,oldfont : HFONT;
begin
Canvas.Brush.Color := clBtnFace;
Canvas.FillRect(Rect(0,0,ClientWidth,ClientHeight));
if DesignerPanel.CurPage=nil then exit;
DrawBackground(Canvas);
Canvas.Font.Size := 8;
Canvas.Font.Name := sArialFont;

newfont := GetFont(Canvas.Font);
oldfont := SelectObject(Canvas.Handle,newfont);
try
  SetBkMode(Canvas.Handle,TRANSPARENT);
  Step := GetStep;
  i := -GetScrollOffset;
  n := 0;
  mw := GetMaxPixelSize;
  while true do
    begin
      i := i+Step div 2;
      if i>mw then break;
      DrawMarker(Canvas,i);

      n := n+aDesignerRulerSteps[RulerUnits];
      s := FormatFloat(aDesignerRulerFormats[RulerUnits],n);
      i := i+Step-(Step div 2);
      if i>mw then break;
      DrawLabel(Canvas,s,i);
    end;
finally
  SelectObject(Canvas.Handle,oldfont);
  DeleteObject(newfont);
end;
end;

/////////////////////////////////////////////////
//
// TprHorDesignerRuler
//
/////////////////////////////////////////////////
constructor TprHorDesignerRuler.Create(AOwner : TComponent);
begin
inherited;
Height := GetFontSize(sArialFont,8,[]).cy;
end;

function TprHorDesignerRuler.GetMaxPixelSize : integer;
begin
Result := TprPage(DesignerPanel.CurPage).PixelPageWidth;
end;

function TprHorDesignerRuler.GetScrollOffset : integer;
begin
Result := DesignerPanel.HorScrollBoxPos;
end;

function TprHorDesignerRuler.GetStep : integer;
begin
Result := prConvertToPixels(aDesignerRulerSteps[RulerUnits],RulerUnits,true);
end;

function TprHorDesignerRuler.GetFont(Font : TFont) : HFONT;
begin
Result := CreateAPIFont(Font);
end;

procedure TprHorDesignerRuler.DrawMarker(Canvas : TCanvas; Offs : integer);
begin
Canvas.MoveTo(Offs,5);
Canvas.LineTo(Offs,ClientHeight-7);
end;

procedure TprHorDesignerRuler.DrawLabel(Canvas : TCanvas; const LabelText : string; Offs : integer);
begin
Canvas.TextOut(offs-Canvas.TextWidth(LabelText) div 2,0,LabelText);
end;

procedure TprHorDesignerRuler.DrawBackground(Canvas : TCanvas);
begin
with TprPage(DesignerPanel.CurPage) do
  begin
    Canvas.Brush.Color := clBtnShadow;
    Canvas.FillRect(Rect(-GetScrollOffset,0,dPageRect.Left-GetScrollOffset,ClientHeight));
    Canvas.Brush.Color := clWindow;
    Canvas.FillRect(Rect(dPageRect.Left-GetScrollOffset,0,dPageRect.Right-GetScrollOffset,ClientHeight));
    Canvas.Brush.Color := clBtnShadow;
    Canvas.FillRect(Rect(dPageRect.Right-GetScrollOffset,0,PixelPageWidth-GetScrollOffset,ClientHeight));
  end;
end;

/////////////////////////////////////////////////
//
// TprVerDesignerRuler
//
/////////////////////////////////////////////////
constructor TprVerDesignerRuler.Create(AOwner : TComponent);
begin
inherited;
Width := GetFontSize(sArialFont,8,[]).cy;
end;

function TprVerDesignerRuler.GetMaxPixelSize : integer;
begin
Result := TprPage(DesignerPanel.CurPage).PixelPageHeight;
end;

function TprVerDesignerRuler.GetScrollOffset : integer;
begin
Result := DesignerPanel.VerScrollBoxPos;
end;

function TprVerDesignerRuler.GetStep : integer;
begin
Result := prConvertToPixels(aDesignerRulerSteps[RulerUnits],RulerUnits,false);
end;

function TprVerDesignerRuler.GetFont(Font : TFont) : HFONT;
begin
Result := Create90Font(Font);
end;

procedure TprVerDesignerRuler.DrawMarker(Canvas : TCanvas; Offs : integer);
begin
Canvas.MoveTo(5,Offs);
Canvas.LineTo(ClientWidth-7,Offs);
end;

procedure TprVerDesignerRuler.DrawLabel(Canvas : TCanvas; const LabelText : string; Offs : integer);
begin
Canvas.TextOut(0,Offs+Canvas.TextWidth(LabelText) div 2,LabelText);
end;

procedure TprVerDesignerRuler.DrawBackground(Canvas : TCanvas);
begin
with TprPage(DesignerPanel.CurPage) do
  begin
    Canvas.Brush.Color := clBtnShadow;
    Canvas.FillRect(Rect(0,0-GetScrollOffset,ClientWidth,dPageRect.Top-GetScrollOffset));
    Canvas.Brush.Color := clWindow;
    Canvas.FillRect(Rect(0,dPageRect.Top-GetScrollOffset,ClientWidth,dPageRect.Bottom-GetScrollOffset));
    Canvas.Brush.Color := clBtnShadow;
    Canvas.FillRect(Rect(0,dPageRect.Bottom-GetScrollOffset,ClientWidth,PixelPageHeight-GetScrollOffset));
  end;
end;

/////////////////////////////////////////////////
//
// TprRulerOptions
//
/////////////////////////////////////////////////
constructor TprRulerOptions.Create;
begin
inherited;
FRulerUnits := prpsuSm;
FVisible := true;
end;

procedure TprRulerOptions.SetVisible(Value : boolean);
begin
if FVisible=Value then exit;
FVisible := Value;
DoChanged;
end;

procedure TprRulerOptions.SetRulerUnits(Value : TprPosSizeUnits);
begin
if FRulerUnits=Value then exit;
FRulerUnits := Value;
DoChanged;
end;

procedure TprRulerOptions.Assign(Source : TPersistent);
begin
with TprRulerOptions(Source) do
  begin
    Self.FVisible := Visible;
    Self.FRulerUnits := RulerUnits;
  end;
DoChanged;
end;

procedure TprRulerOptions.WriteToIni(IniFile : TIniFile; const SectionName,Prefix : string);
begin
IniFile.WriteBool(SectionName,Prefix+'Visible',Visible);
IniFile.WriteInteger(SectionName,Prefix+'RulerUnits',integer(RulerUnits));
end;

procedure TprRulerOptions.ReadFromIni(IniFile : TIniFile; const SectionName,Prefix : string);
begin
Visible := IniFile.ReadBool(SectionName,Prefix+'Visible',Visible);
RulerUnits := TprPosSizeUnits(IniFile.ReadInteger(SectionName,Prefix+'RulerUnits',integer(RulerUnits)));
end;

/////////////////////////////////////////////////
//
// TprDesignerBox
//
/////////////////////////////////////////////////
constructor TprDesignerBox.Create(AOwner : TComponent);
begin
inherited;
FGridBitmap := 0;
end;

destructor TprDesignerBox.Destroy;
begin
if FGridBitmap<>0 then
  DeleteObject(FGridBitmap);
inherited;
end;

procedure TprDesignerBox.DrawGrid(DC : HDC; BackRgn : HRGN);
begin
pr_DesignerFunctions.DrawGrid(DC,BackRgn,FGridBitmap,TprDesignerPanel(DesignerPanel).GridSize,TprDesignerPanel(DesignerPanel).GridSize,Rect(0,0,ClientWidth,ClientHeight),-DesignerPanel.HorScrollBoxPos,-DesignerPanel.VerScrollBoxPos);
end;

procedure TprDesignerBox.DrawOther(DC : HDC);
var
  r : TRect;
  npn1,npn2,opn : HPEN;
begin
npn1 := CreatePen(PS_SOLID,1,clBlack);
npn2 := CreatePen(PS_DOT,1,clBlack);
opn := SelectObject(DC,npn1);
try
{ ???
  r := TprPage(DesignerPanel.CurPage).pInfo.PrnSRect;
  OffsetRect(r,-DesignerPanel.HorScrollBoxPos,-DesignerPanel.VerScrollBoxPos);
  DrawRect(DC,r);
}
  SelectObject(DC,npn2);
  r := TprPage(DesignerPanel.CurPage).dPageRect^;
  OffsetRect(r,-DesignerPanel.HorScrollBoxPos,-DesignerPanel.VerScrollBoxPos);
  DrawRect(DC,r);
finally
  SelectObject(DC,opn);
  DeleteObject(npn1);
  DeleteObject(npn2);
end;
end;

function TprDesignerBox.DrawWidth : integer;
begin
if DesignerPanel.CurPage=nil then
  Result := 0
else
  Result := TprPage(DesignerPanel.CurPage).PixelPageWidth;
end;

function TprDesignerBox.DrawHeight : integer;
begin
if DesignerPanel.CurPage=nil then
  Result := 0
else
  Result := TprPage(DesignerPanel.CurPage).PixelPageHeight;
end;

/////////////////////////////////////////////////
//
//  TprDesignerPanel
//
/////////////////////////////////////////////////
constructor TprDesignerPanel.Create(AOwner : TComponent);
begin
inherited;
StuckMode := DefaultStuckMode;
StuckOptions := DefaultStuckOptions;
StuckOffs := DefaultStuckOffs;
FGridSize := 8;
UpdateGridBitmap(TprDesignerBox(FDesignerBox).FGridBitmap,FGridSize,FGridSize);
FHorRulerOptions := TprRulerOptions.Create;
FHorRulerOptions.OnChange := OnRulerOptionsChange;
FVerRulerOptions := TprRulerOptions.Create;
FVerRulerOptions.OnChange := OnRulerOptionsChange;
end;

destructor TprDesignerPanel.Destroy;
begin
FHorRulerOptions.Free;
FVerRulerOptions.Free;
inherited;
end;

function TprDesignerPanel.GetReport : TprCustomReport;
begin
Result := FReport;
end;

procedure TprDesignerPanel.SetReport(Value : TprReport);
begin
if FReport=Value then exit;
if FReport<>nil then
  FReport.DsgnRemoveDesignerNotifyLink(FDesignerNotifyLink);
FReport := Value;
if FReport<>nil then
  FReport.DsgnAddDesignerNotifyLink(FDesignerNotifyLink);
InternalSetReport(FReport);
end;

procedure TprDesignerPanel.Notification(AComponent : TComponent; AOperation : TOperation);
begin
inherited;
if (AOperation=opRemove) and (AComponent=FReport) then
  Report := nil;
end;

procedure TprDesignerPanel.OnRulerOptionsChange(Sender : TObject);
var
  r : TprDesignerRuler;
begin
if Sender=FHorRulerOptions then
  r := TprDesignerRuler(FHorRuler)
else
  r := TprDesignerRuler(FVerRuler);
with TprRulerOptions(Sender) do
  begin
    r.RulerUnits := RulerUnits;
    r.Visible := Visible;
  end;
AlignChildControls;
end;

function TprDesignerPanel.CreateDesignerBox : TprCustomDesignerBox;
begin
Result := TprDesignerBox.Create(Self);
end;

procedure TprDesignerPanel.CreateRulers(var HorRuler,VerRuler : TprCustomDesignerRuler);
begin
HorRuler := TprHorDesignerRuler.Create(Self);
VerRuler := TprVerDesignerRuler.Create(Self);
end;

function TprDesignerPanel.CreatePosSizeForm : TprCustomPosSizeForm;
begin
Result := TprObjectPosSizeForm.Create(Self);
end;

function TprDesignerPanel.GetShowGrid : boolean;
begin
Result := FShowGrid;
end;

procedure TprDesignerPanel.SetShowGrid(Value : boolean);
begin
if FShowGrid=Value then exit;
FShowGrid := Value;
FDesignerBox.Repaint;
end;

function TprDesignerPanel.GetUseGrid : boolean;
begin
Result := FUseGrid;
end;

procedure TprDesignerPanel.SetUseGrid(Value : boolean);
begin
if FUseGrid=Value then exit;
FUseGrid := Value;
end;

procedure TprDesignerPanel.UpdateCurPage;
begin
if CurPage<>nil then
  with TprPage(CurPage) do
    begin
      FDesignerBox.HorzScrollBar.Range := PixelPageWidth;
      FDesignerBox.VertScrollBar.Range := PixelPageHeight;
    end
else
  with TprPage(CurPage) do
    begin
      FDesignerBox.HorzScrollBar.Range := 0;
      FDesignerBox.VertScrollBar.Range := 0;
    end;

inherited;
end;

procedure TprDesignerPanel.SetGridSize(Value : integer);
begin
if FGridSize=Value then exit;
FGridSize := Value;
UpdateGridBitmap(TprDesignerBox(FDesignerBox).FGridBitmap,FGridSize,FGridSize);
if ShowGrid then
  FDesignerBox.Repaint;
end;

procedure TprDesignerPanel.SetHorRulerOptions(Value : TprRulerOptions);
begin
FHorRulerOptions.Assign(Value);
end;

procedure TprDesignerPanel.SetVerRulerOptions(Value : TprRulerOptions);
begin
FVerRulerOptions.Assign(Value);
end;

procedure TprDesignerPanel.AdjustToGrid(var X,Y : integer);
begin
X := (X div FGridSize)*FGridSize;
Y := (Y div FGridSize)*FGridSize;
end;

function TprDesignerPanel.GridSizeX : integer;
begin
Result := FGridSize;
end;

function TprDesignerPanel.GridSizeY : integer;
begin
Result := FGridSize;
end;

function TprDesignerPanel.GetDefaultHeightForHorizontalBand : integer;
begin
Result := 40;
end;

function TprDesignerPanel.GetDefaultWidthForVerticalBand : integer;
begin
Result := 40;
end;

procedure TprDesignerPanel.OnPopupMainMenuClick(Sender : TObject);
begin
case TMainMenu(Sender).Tag of
  1: BringToFront;
  2: SendToBack;
  3: AlignAction(aacAlignToGridLeftTop);
  4: AlignAction(aacAlignToGridAll);
  5: EditGroups;
  6: EditValues;
  7: ShowGrid := not ShowGrid;
  8: UseGrid := not UseGrid;
  9: EditGridSize;
  10: EditOptions;
end;
end;

procedure TprDesignerPanel.InitPopupMainMenu(Popup : TPopupMenu; MainMenuItem : TMenuItem);
var
  m : TMenuItem;
begin
inherited;
m := AddPopupMenuItem(Popup,MainMenuItem,sMainMenuEdit,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sMainMenuEditBringToFront,'BRINGTOFRONT',OnPopupMainMenuClick,'',1,SelCount>0,false);
AddPopupMenuItem(Popup,m,sMainMenuEditSendToBack,'SENDTOBACK',OnPopupMainMenuClick,'',2,SelCount>0,false);
AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sMainMenuEditAlignToGrid,'',OnPopupMainMenuClick,'',3,SelCount>0,false);
AddPopupMenuItem(Popup,m,sMainMenuEditAlignAndResizeToGrid,'',OnPopupMainMenuClick,'',4,AllowAlignToGridAll,false);
AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sMainMenuEditGroups,'GROUPS',OnPopupMainMenuClick,'',5,GetReport<>nil,false);
AddPopupMenuItem(Popup,m,sMainMenuEditVars,'FUNC',OnPopupMainMenuClick,'',6,GetReport<>nil,false);
AddPopupMenuItem(Popup,m,sMainMenuEditVariables,'VARIABLES',OnPopupMainMenuClick,'',11,GetReport<>nil,false);

m := AddPopupMenuItem(Popup,MainMenuItem,sMainMenuView,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sMainMenuViewShowGrid,'SHOWGRID',OnPopupMainMenuClick,'Ctrl+G',7,true,ShowGrid);
AddPopupMenuItem(Popup,m,sMainMenuViewUseGrid,'ALIGNTOGRID',OnPopupMainMenuClick,'',8,true,UseGrid);
AddPopupMenuItem(Popup,m,sMainMenuViewGridSize,'',OnPopupMainMenuClick,'',9,true,false);
AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sMainMenuViewOptions,'',OnPopupMainMenuClick,'',10,true,false);
end;

procedure TprDesignerPanel.WriteToIni(IniFile : TIniFile; const SectionName : string);
var
  i : TprStuckOptions;
begin
inherited;
IniFile.WriteInteger(SectionName,'GridSize',GridSize);
IniFile.WriteBool(SectionName,'GridAlign',UseGrid);
IniFile.WriteBool(SectionName,'GridShow',ShowGrid);
IniFile.WriteInteger(SectionName,'StuckMode',integer(StuckMode));
IniFile.WriteInteger(SectionName,'StuckOffs',StuckOffs);
for i:=Low(TprStuckOptions) to High(TprStuckOptions) do
  IniFile.WriteBool(SectionName,'StuckOptions'+GetEnumName(TypeInfo(TprStuckOptions),integer(i)),i in StuckOptions);
HorRulerOptions.WriteToIni(IniFile,SectionName,'HorRulerOptions');
VerRulerOptions.WriteToIni(IniFile,SectionName,'VerRulerOptions');
end;

procedure TprDesignerPanel.ReadFromIni(IniFile : TIniFile; const SectionName : string);
var
  i : TprStuckOptions;
  so : TprStuckOptionsSet;
begin
inherited;
GridSize := IniFile.ReadInteger(SectionName,'GridSize',GridSize);
UseGrid := IniFile.ReadBool(SectionName,'GridAlign',UseGrid);
ShowGrid := IniFile.ReadBool(SectionName,'GridShow',ShowGrid);
StuckMode := TprStuckMode(IniFile.ReadInteger(SectionName,'StuckMode',integer(StuckMode)));
StuckOffs := IniFile.ReadInteger(SectionName,'StuckOffs',StuckOffs);
so := [];
for i:=Low(TprStuckOptions) to High(TprStuckOptions) do
  if IniFile.ReadBool(SectionName,'StuckOptions'+GetEnumName(TypeInfo(TprStuckOptions),integer(i)),i in StuckOptions) then
    Include(so,i);
StuckOptions := so;
HorRulerOptions.ReadFromIni(IniFile,SectionName,'HorRulerOptions');
VerRulerOptions.ReadFromIni(IniFile,SectionName,'VerRulerOptions');
end;

function TprDesignerPanel.EditOptions : boolean;
begin
Result := TprDesignerPanelOptionsForm.Create(Application).EditOptions(Self);
if Result then
  UpdateCurPage;
end;

function TprDesignerPanel.EditPage(PageIndex : integer) : boolean;
var
  Visible : boolean;
  DsgnWidth,DsgnHeight : integer;
begin
DsgnWidth := TprPage(Report.Pages[PageIndex]).DsgnWidth;
DsgnHeight := TprPage(Report.Pages[PageIndex]).DsgnHeight;
Visible := TprPage(Report.Pages[PageIndex]).Visible;
Result := TprPageParamsForm.Create(Application).EditOptions(TprReport(Report),TprPage(Report.Pages[PageIndex]).PageInfo,TprPage(Report.Pages[PageIndex]).PageScaleInfo,true,DsgnWidth,DsgnHeight,true,Visible);
if Result then
  begin
    TprPage(Report.Pages[PageIndex]).DsgnWidth := DsgnWidth;
    TprPage(Report.Pages[PageIndex]).DsgnHeight := DsgnHeight;
    TprPage(Report.Pages[PageIndex]).Visible := Visible;
    if PageIndex=ActivePageIndex then
      UpdateCurPage;
    DsgnNotifyReport(true);
  end;
end;

procedure TprDesignerPanel.EditReportParams;
begin
if TprReportParamsForm.Create(Application).EditParams(TprReport(Report)) then
  begin
    UpdateCurPage;
    DsgnNotifyReport(true);
  end;
end;

procedure TprDesignerPanel.EditGridSize;
var
  i : integer;
begin
i := GridSize;
if TprGridSizeForm.Create(Application).EditGridSize(i) then
  GridSize := i;
end;

initialization

RegisterClass(TprRichEditorForm);
RegisterClass(TprImageEditorForm);
RegisterClass(TprMemoEditorForm);
RegisterClass(TprBandEditorForm);
RegisterClass(TprShapeEditorForm);
RegisterClass(TprBarCodeEditorForm);

end.
