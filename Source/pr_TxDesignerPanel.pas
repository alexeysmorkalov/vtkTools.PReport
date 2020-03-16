{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the TprTxDesignerPanel class and some auxiliary classes.
See also:
  TprTxDesignerPanel}
unit pr_TxDesignerPanel;

interface

{$I PR.INC}

uses
  Windows, Classes, Graphics, SysUtils, IniFiles, forms, menus,

  pr_Common, pr_TxClasses, pr_CommonDesignerPanel, pr_Utils, pr_TxUtils;

type

TprTxDesignerPanel = class;

/////////////////////////////////////////////////
//
// TprTxDesignerRuler
//
/////////////////////////////////////////////////
TprTxDesignerRuler = class(TprCustomDesignerRuler)
private
  FRulerStep : integer;
  procedure SetRulerStep(Value : integer);
protected
  procedure Paint; override;
  function GetScrollOffset : integer; virtual; abstract;
  function GetStep : integer; virtual; abstract;
  function GetFont(Font : TFont) : HFONT; virtual; abstract;
  function GetMaxPixelSize : integer; virtual; abstract;
  procedure DrawMarker(Canvas : TCanvas; Offs : integer); virtual; abstract;
  procedure DrawLabel(Canvas : TCanvas; const LabelText : string; Offs : integer); virtual; abstract;
  procedure DrawBackground(Canvas : TCanvas); virtual; abstract;
  function DesignerPanel : TprTxDesignerPanel;
public
  property RulerStep : integer read FRulerStep write SetRulerStep;
  constructor Create(AOwner : TComponent); override;
end;

/////////////////////////////////////////////////
//
// TprTxHorDesignerRuler
//
/////////////////////////////////////////////////
TprTxHorDesignerRuler = class(TprTxDesignerRuler)
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
// TprTxVerDesignerRuler
//
/////////////////////////////////////////////////
TprTxVerDesignerRuler = class(TprTxDesignerRuler)
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
// TprTxDesignerBox
//
/////////////////////////////////////////////////
TprTxDesignerBox = class(TprCustomDesignerBox)
private
  FGridBitmap : HBITMAP;
protected
  procedure DrawGrid(DC : HDC; BackRgn : HRGN); override;
  procedure DrawOther(DC : HDC); override;
  function GetExData : pointer; override;
  function DrawWidth : integer; override;
  function DrawHeight : integer; override;
public
  constructor Create(AOwner : TComponent); override;
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TprTxRulerOptions
//
/////////////////////////////////////////////////
{Represents the options for horizontal and vertical rulers on the designer panel.}
TprTxRulerOptions = class(TprOptions)
private
  FVisible: boolean;
  FRulerStep: integer;
  procedure SetVisible(Value : boolean);
  procedure SetRulerStep(Value : integer);
public
{Creates an instance of the TprTxRulerOptions class.}
  constructor Create;
{Copies the contents of another, similar object.
Parameters:
  Source - The source object.}
  procedure Assign(Source: TPersistent); override;
{See:
  TprOptions.WriteToIni}
  procedure WriteToIni(IniFile: TIniFile; const SectionName, Prefix: string); override;
{See:
  TprOptions.ReadFromIni}
  procedure ReadFromIni(IniFile: TIniFile; const SectionName, Prefix: string); override;
published
{Indicates whether the ruler is visible.}
  property Visible: Boolean read FVisible write SetVisible default true;
{Specifies the step in chars between marks.}
  property RulerStep: Integer read FRulerStep write SetRulerStep default 4;
end;

/////////////////////////////////////////////////
//
//  TprTxDesignerPanel
//
/////////////////////////////////////////////////
{Represents the control for editing the TprTxReport report template.
You can use this control if you want to develop non standard report designers
with some unique features.
See demo "26_TxDrag-Drop" as example of using this control.
See also:
  TprCustomDesignerPanel}
TprTxDesignerPanel = class(TprCustomDesignerPanel)
private
  FReport : TprTxReport;
  FFont : TprFixedFont;
  FHorRulerOptions : TprTxRulerOptions;
  FVerRulerOptions : TprTxRulerOptions;
  procedure SetHorRulerOptions(Value : TprTxRulerOptions);
  procedure SetVerRulerOptions(Value : TprTxRulerOptions);
  procedure SetFont(Value : TprFixedFont);
  procedure OnRulerOptionsChange(Sender : TObject);
  procedure OnFontChange(Sender : TObject);
  procedure FillTxExData;
  procedure OnPopupMainMenuClick(Sender : TObject);
protected
  FTxExData : rTxExData;

  procedure SetReport(Value : TprTxReport);
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

  function DsgnR(dc: TprDesignComponent): TRect; override;
public
{See:
  TprCustomDesignerPanel.GetReport}
  function GetReport : TprCustomReport; override;

{See:
  TprCustomDesignerPanel.ConvertToDesignerCoords}
  procedure ConvertToDesignerCoords(const rSource : TRect; var rDest : TRect); override;
{See:
  TprCustomDesignerPanel.ConvertXToDesignerCoords}
  function ConvertXToDesignerCoords(X : integer) : integer; override;
{See:
  TprCustomDesignerPanel.ConvertYToDesignerCoords}
  function ConvertYToDesignerCoords(Y : integer) : integer; override;
{See:
  TprCustomDesignerPanel.ConvertFromDesignerCoords}
  procedure ConvertFromDesignerCoords(const rSource : TRect; var rDest : TRect); override;
{See:
  TprCustomDesignerPanel.ConvertXFromDesignerCoords}
  function ConvertXFromDesignerCoords(X : integer) : integer; override;
{See:
  TprCustomDesignerPanel.ConvertYFromDesignerCoords}
  function ConvertYFromDesignerCoords(Y : integer) : integer; override;

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
  TprCustomDesignerPanel.EditPage}
  function EditPage(PageIndex: Integer) : boolean; override;
{See:
  TprCustomDesignerPanel.EditOptions}
  function EditOptions: Boolean; override;

{Creates an instance of the TprTxDesignerPanel class.}
  constructor Create(AOwner : TComponent); override;
{Frees an instance of the TprTxDesignerPanel class.}
  destructor Destroy; override;
published
  property TabStop;
  property TabOrder;
  
{Specifies the TprReport object, that holds the report template.}
  property Report : TprTxReport read FReport write SetReport;
{See:
  TprCustomDesignerPanel.ActivePageIndex}
  property ActivePageIndex;
{Specifies the fixed font that is used for drawn the text.
See also:
  TprFixedFont}
  property Font: TprFixedFont read FFont write SetFont;
{Specifies the options of the horizontal ruler.
See also:
  TprRulerOptions}
  property HorRulerOptions: TprTxRulerOptions read FHorRulerOptions write SetHorRulerOptions;
{Specifies the options of the horizontal ruler.
See also:
  TprRulerOptions}
  property VerRulerOptions: TprTxRulerOptions read FVerRulerOptions write SetVerRulerOptions;
{See:
  TprCustomDesignerPanel.HorBandsCaptionsOptions}
  property HorBandsCaptionsOptions;
{See:
  TprCustomDesignerPanel.VerBandsCaptionsOptions}
  property VerBandsCaptionsOptions;
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
  TprCustomDesignerPanel.OpacityObjectsPropsForm}
  property OpacityFindForm default 100;
end;

implementation

uses
  pr_TxPageParams, pr_DesignerFunctions, pr_TxObjectPosSizeForm, pr_TxDesignerPanelOptions,
  pr_TxMemoEditor, pr_TxLineEditor, pr_TxCommandEditor, pr_BandEditor,
  pr_Strings, pr_MultiLang;

/////////////////////////////////////////////////
//
// TprTxDesignerRuler
//
/////////////////////////////////////////////////
constructor TprTxDesignerRuler.Create(AOwner : TComponent);
begin
inherited;
FRulerStep := 4;
end;

function TprTxDesignerRuler.DesignerPanel: TprTxDesignerPanel;
begin
  Result := TprTxDesignerPanel(inherited DesignerPanel);
end;

procedure TprTxDesignerRuler.SetRulerStep(Value : integer);
begin
if FRulerStep=Value then exit;
FRulerStep := Value;
Repaint;
end;

procedure TprTxDesignerRuler.Paint;
var
  s : string;
  Step,i,mw,n : integer;
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

      n := n+RulerStep;
      s := IntToStr(n);
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
// TprTxHorDesignerRuler
//
/////////////////////////////////////////////////
constructor TprTxHorDesignerRuler.Create(AOwner : TComponent);
begin
inherited;
Height := GetFontSize(sArialFont,8,[]).cy;
end;

function TprTxHorDesignerRuler.GetMaxPixelSize : integer;
begin
Result := DesignerPanel.ConvertXToDesignerCoords(TprTxPage(DesignerPanel.CurPage).DsgnColNum);
end;

function TprTxHorDesignerRuler.GetScrollOffset : integer;
begin
Result := DesignerPanel.HorScrollBoxPos;
end;

function TprTxHorDesignerRuler.GetStep : integer;
begin
Result := DesignerPanel.ConvertXToDesignerCoords(RulerStep);
end;

function TprTxHorDesignerRuler.GetFont(Font : TFont) : HFONT;
begin
Result := CreateAPIFont(Font);
end;

procedure TprTxHorDesignerRuler.DrawMarker(Canvas : TCanvas; Offs : integer);
begin
Canvas.MoveTo(Offs,5);
Canvas.LineTo(Offs,ClientHeight-7);
end;

procedure TprTxHorDesignerRuler.DrawLabel(Canvas : TCanvas; const LabelText : string; Offs : integer);
begin
Canvas.TextOut(offs-Canvas.TextWidth(LabelText) div 2,0,LabelText);
end;

procedure TprTxHorDesignerRuler.DrawBackground(Canvas : TCanvas);
begin
Canvas.Brush.Color := clWindow;
Canvas.FillRect(Rect(-GetScrollOffset,0,DesignerPanel.ConvertXToDesignerCoords(TprTxPage(DesignerPanel.CurPage).DsgnColNum)-GetScrollOffset,ClientHeight));
end;

/////////////////////////////////////////////////
//
// TprTxVerDesignerRuler
//
/////////////////////////////////////////////////
constructor TprTxVerDesignerRuler.Create(AOwner : TComponent);
begin
inherited;
Width := GetFontSize(sArialFont,8,[]).cy;
end;

function TprTxVerDesignerRuler.GetMaxPixelSize : integer;
begin
Result := DesignerPanel.ConvertYToDesignerCoords(TprTxPage(DesignerPanel.CurPage).DsgnLineNum);
end;

function TprTxVerDesignerRuler.GetScrollOffset : integer;
begin
Result := DesignerPanel.VerScrollBoxPos;
end;

function TprTxVerDesignerRuler.GetStep : integer;
begin
Result := DesignerPanel.ConvertYToDesignerCoords(RulerStep);
end;

function TprTxVerDesignerRuler.GetFont(Font : TFont) : HFONT;
begin
Result := Create90Font(Font);
end;

procedure TprTxVerDesignerRuler.DrawMarker(Canvas : TCanvas; Offs : integer);
begin
Canvas.MoveTo(5,Offs);
Canvas.LineTo(ClientWidth-7,Offs);
end;

procedure TprTxVerDesignerRuler.DrawLabel(Canvas : TCanvas; const LabelText : string; Offs : integer);
begin
Canvas.TextOut(0,Offs+Canvas.TextWidth(LabelText) div 2,LabelText);
end;

procedure TprTxVerDesignerRuler.DrawBackground(Canvas : TCanvas);
begin
Canvas.Brush.Color := clWindow;
Canvas.FillRect(Rect(0,0,ClientWidth,DesignerPanel.ConvertYToDesignerCoords(TprTxPage(DesignerPanel.CurPage).DsgnLineNum)-GetScrollOffset));
end;

/////////////////////////////////////////////////
//
// TprTxRulerOptions
//
/////////////////////////////////////////////////
constructor TprTxRulerOptions.Create;
begin
inherited;
FRulerStep := 4;
FVisible := true;
end;

procedure TprTxRulerOptions.SetVisible(Value : boolean);
begin
if FVisible=Value then exit;
FVisible := Value;
DoChanged;
end;

procedure TprTxRulerOptions.SetRulerStep(Value : integer);
begin
if FRulerStep=Value then exit;
FRulerStep := Value;
DoChanged;
end;

procedure TprTxRulerOptions.Assign(Source : TPersistent);
begin
with TprTxRulerOptions(Source) do
  begin
    Self.FVisible := Visible;
    Self.FRulerStep := RulerStep;
  end;
DoChanged;
end;

procedure TprTxRulerOptions.WriteToIni(IniFile : TIniFile; const SectionName,Prefix : string);
begin
IniFile.WriteBool(SectionName,Prefix+'Visible',Visible);
IniFile.WriteInteger(SectionName,Prefix+'RulerUnits',RulerStep);
end;

procedure TprTxRulerOptions.ReadFromIni(IniFile : TIniFile; const SectionName,Prefix : string);
begin
Visible := IniFile.ReadBool(SectionName,Prefix+'Visible',Visible);
RulerStep := IniFile.ReadInteger(SectionName,Prefix+'RulerUnits',RulerStep);
end;

/////////////////////////////////////////////////
//
// TprTxDesignerBox
//
/////////////////////////////////////////////////
constructor TprTxDesignerBox.Create(AOwner : TComponent);
begin
inherited;
FGridBitmap := 0;
end;

destructor TprTxDesignerBox.Destroy;
begin
if FGridBitmap<>0 then
  DeleteObject(FGridBitmap);
inherited;
end;

procedure TprTxDesignerBox.DrawGrid(DC : HDC; BackRgn : HRGN);
begin
pr_DesignerFunctions.DrawGrid(DC, BackRgn,FGridBitmap,TprTxDesignerPanel(DesignerPanel).GridSizeX,TprTxDesignerPanel(DesignerPanel).GridSizeY,Rect(0,0,ClientWidth,ClientHeight),-DesignerPanel.HorScrollBoxPos,-DesignerPanel.VerScrollBoxPos);
end;

procedure TprTxDesignerBox.DrawOther(DC : HDC);
begin
end;

function TprTxDesignerBox.GetExData : pointer;
begin
Result := @TprTxDesignerPanel(DesignerPanel).FTxExData;
end;

function TprTxDesignerBox.DrawWidth : integer;
begin
if DesignerPanel.CurPage=nil then
  Result := 0
else
  Result := TprTxPage(DesignerPanel.CurPage).DsgnColNum*TprTxDesignerPanel(DesignerPanel).GridSizeX;
end;

function TprTxDesignerBox.DrawHeight : integer;
begin
if DesignerPanel.CurPage=nil then
  Result := 0
else
  Result := TprTxPage(DesignerPanel.CurPage).DsgnLineNum*TprTxDesignerPanel(DesignerPanel).GridSizeY;
end;

/////////////////////////////////////////////////
//
//  TprTxDesignerPanel
//
/////////////////////////////////////////////////
constructor TprTxDesignerPanel.Create(AOwner : TComponent);
begin
inherited;
FHorRulerOptions := TprTxRulerOptions.Create;
FHorRulerOptions.OnChange := OnRulerOptionsChange;
FVerRulerOptions := TprTxRulerOptions.Create;
FVerRulerOptions.OnChange := OnRulerOptionsChange;
FFont := TprFixedFont.Create;
FFont.OnChange := OnFontChange;
FillTxExData;
end;

destructor TprTxDesignerPanel.Destroy;
begin
FHorRulerOptions.Free;
FVerRulerOptions.Free;
FFont.Free;
inherited;
end;

function TprTxDesignerPanel.GetReport : TprCustomReport;
begin
Result := FReport;
end;

procedure TprTxDesignerPanel.SetReport(Value : TprTxReport);
begin
if FReport=Value then exit;
if FReport<>nil then
  FReport.DsgnRemoveDesignerNotifyLink(FDesignerNotifyLink);
FReport := Value;
if FReport<>nil then
  FReport.DsgnAddDesignerNotifyLink(FDesignerNotifyLink);
InternalSetReport(FReport);
end;

procedure TprTxDesignerPanel.Notification(AComponent : TComponent; AOperation : TOperation);
begin
inherited;
if (AOperation=opRemove) and (AComponent=FReport) then
  Report := nil;
end;

procedure TprTxDesignerPanel.FillTxExData;
begin
FTxExData.FontName := Font.Name;
FTxExData.FontSize := Font.Size;
FTxExData.SymbolSize := GetFontSize(FTxExData.FontName,FTxExData.FontSize,[]);
UpdateGridBitmap(TprTxDesignerBox(FDesignerBox).FGridBitmap,GridSizeX,GridSizeY);
end;

procedure TprTxDesignerPanel.OnFontChange(Sender : TObject);
begin
FillTxExData;
FTxExData.FontName := Font.Name;
FTxExData.FontSize := Font.Size;
// calculate symbol size
Canvas.Font.Name := Font.Name;
Canvas.Font.Size := Font.Size;
FTxExData.SymbolSize := Canvas.TextExtent('1');

UpdateCurPage;
end;

procedure TprTxDesignerPanel.OnRulerOptionsChange(Sender : TObject);
var
  r : TprTxDesignerRuler;
begin
if Sender=FHorRulerOptions then
  r := TprTxDesignerRuler(FHorRuler)
else
  r := TprTxDesignerRuler(FVerRuler);
with TprTxRulerOptions(Sender) do
  begin
    r.RulerStep := RulerStep;
    if r.Visible<>Visible then
      r.Visible := Visible;
  end;
AlignChildControls;
end;

function TprTxDesignerPanel.CreateDesignerBox : TprCustomDesignerBox;
begin
Result := TprTxDesignerBox.Create(Self);
end;

procedure TprTxDesignerPanel.CreateRulers(var HorRuler,VerRuler : TprCustomDesignerRuler);
begin
HorRuler := TprTxHorDesignerRuler.Create(Self);
VerRuler := TprTxVerDesignerRuler.Create(Self);
end;

function TprTxDesignerPanel.CreatePosSizeForm : TprCustomPosSizeForm;
begin
Result := TprTxObjectPosSizeForm.Create(Self);
end;

function TprTxDesignerPanel.GetShowGrid : boolean;
begin
Result := true;
end;

procedure TprTxDesignerPanel.SetShowGrid(Value : boolean);
begin
end;

function TprTxDesignerPanel.GetUseGrid : boolean;
begin
Result := true;
end;

procedure TprTxDesignerPanel.SetUseGrid(Value : boolean);
begin
end;

procedure TprTxDesignerPanel.UpdateCurPage;
begin
if CurPage<>nil then
  with TprTxPage(CurPage) do
    begin
      FDesignerBox.HorzScrollBar.Range := ConvertXToDesignerCoords(DsgnColNum);
      FDesignerBox.VertScrollBar.Range := ConvertYToDesignerCoords(DsgnLineNum);
      FDesignerBox.HorzScrollBar.Increment := FTxExData.SymbolSize.cx;
      FDesignerBox.VertScrollBar.Increment := FTxExData.SymbolSize.cy;
    end
else
  with TprTxPage(CurPage) do
    begin
      FDesignerBox.HorzScrollBar.Range := 0;
      FDesignerBox.VertScrollBar.Range := 0;
    end;
inherited;
end;

procedure TprTxDesignerPanel.SetHorRulerOptions(Value : TprTxRulerOptions);
begin
FHorRulerOptions.Assign(Value);
end;

procedure TprTxDesignerPanel.SetVerRulerOptions(Value : TprTxRulerOptions);
begin
FVerRulerOptions.Assign(Value);
end;

procedure TprTxDesignerPanel.SetFont(Value : TprFixedFont);
begin
FFont.Assign(Value);
end;

procedure TprTxDesignerPanel.AdjustToGrid(var X,Y : integer);
begin
X := (X div GridSizeX)*GridSizeX;
Y := (Y div GridSizeY)*GridSizeY;
end;

function TprTxDesignerPanel.GridSizeX : integer;
begin
Result := FTxExData.SymbolSize.cx;
end;

function TprTxDesignerPanel.GridSizeY : integer;
begin
Result := FTxExData.SymbolSize.cy;
end;

procedure TprTxDesignerPanel.ConvertToDesignerCoords(const rSource : TRect; var rDest : TRect);
begin
rDest := MulRect(rSource,FTxExData.SymbolSize.cx,FTxExData.SymbolSize.cy);
end;

function TprTxDesignerPanel.ConvertXToDesignerCoords(X : integer) : integer;
begin
Result := x*FTxExData.SymbolSize.cx;
end;

function TprTxDesignerPanel.ConvertYToDesignerCoords(Y : integer) : integer;
begin
Result := y*FTxExData.SymbolSize.cy;
end;

procedure TprTxDesignerPanel.ConvertFromDesignerCoords(const rSource : TRect; var rDest : TRect);
begin
rDest := DivRect(rSource,FTxExData.SymbolSize.cx,FTxExData.SymbolSize.cy);
end;

function TprTxDesignerPanel.ConvertXFromDesignerCoords(X : integer) : integer;
begin
Result := x div FTxExData.SymbolSize.cx;
end;

function TprTxDesignerPanel.ConvertYFromDesignerCoords(Y : integer) : integer; 
begin
Result := y div FTxExData.SymbolSize.cy;
end;

function TprTxDesignerPanel.DsgnR(dc : TprDesignComponent) : TRect;
begin
if dc is TprObj then
  with TprObj(dc) do
    begin
      Result := dRec.pRect;
      Result.Left := (Result.Left+Band.dPageRect.Left)*FTxExData.SymbolSize.cx-HorScrollBoxPos;
      Result.Top := (Result.Top+Band.dPageRect.Top)*FTxExData.SymbolSize.cy-VerScrollBoxPos;
      Result.Right := (Result.Right+Band.dPageRect.Left)*FTxExData.SymbolSize.cx-HorScrollBoxPos{+1};
      Result.Bottom := (Result.Bottom+Band.dPageRect.Top)*FTxExData.SymbolSize.cy-VerScrollBoxPos{+1};
    end
else
  if dc is TprBand then
    with TprBand(dc) do
      begin
        Result.Left := dPageRect.Left*FTxExData.SymbolSize.cx-HorScrollBoxPos;
        Result.Top := dPageRect.Top*FTxExData.SymbolSize.cy-VerScrollBoxPos;
        Result.Right := dPageRect.Right*FTxExData.SymbolSize.cx-HorScrollBoxPos{+1};
        Result.Bottom := dPageRect.Bottom*FTxExData.SymbolSize.cy-VerScrollBoxPos{+1};
      end
  else
    Result := Rect(-1,-1,-1,-1);
end;

function TprTxDesignerPanel.GetDefaultHeightForHorizontalBand : integer;
begin
Result := 5;
end;

function TprTxDesignerPanel.GetDefaultWidthForVerticalBand : integer;
begin
Result := 5;
end;

procedure TprTxDesignerPanel.OnPopupMainMenuClick(Sender : TObject);
begin
case TMenuItem(Sender).Tag of
  1: BringToFront;
  2: SendToBack;
  3: EditGroups;
  4: EditValues;
  5: EditOptions;
end;
end;

procedure TprTxDesignerPanel.InitPopupMainMenu(Popup : TPopupMenu; MainMenuItem : TMenuItem);
var
  m : TMenuItem;
begin
inherited;
m := AddPopupMenuItem(Popup,MainMenuItem,sMainMenuEdit,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sMainMenuEditBringToFront,'BRINGTOFRONT',OnPopupMainMenuClick,'',1,SelCount>0,false);
AddPopupMenuItem(Popup,m,sMainMenuEditSendToBack,'SENDTOBACK',OnPopupMainMenuClick,'',2,SelCount>0,false);
AddPopupMenuItem(Popup,m,0,'',nil,'',0,true,false);
AddPopupMenuItem(Popup,m,sMainMenuEditGroups,'GROUPS',OnPopupMainMenuClick,'',3,GetReport<>nil,false);
AddPopupMenuItem(Popup,m,sMainMenuEditVars,'FUNC',OnPopupMainMenuClick,'',4,GetReport<>nil,false);
AddPopupMenuItem(Popup,m,sMainMenuEditVariables,'VARIABLES',OnPopupMainMenuClick,'',11,GetReport<>nil,false);

AddPopupMenuItem(Popup,MainMenuItem,sMainMenuViewOptions,'',OnPopupMainMenuClick,'',5,true,false);
end;

procedure TprTxDesignerPanel.WriteToIni(IniFile : TIniFile; const SectionName : string);
begin
inherited;
Font.WriteToIni(IniFile,SectionName,'Font');
HorRulerOptions.WriteToIni(IniFile,SectionName,'HorRulerOptions');
VerRulerOptions.WriteToIni(IniFile,SectionName,'VerRulerOptions');
end;

procedure TprTxDesignerPanel.ReadFromIni(IniFile : TIniFile; const SectionName : string);
begin
inherited;
Font.readFromIni(IniFile,SectionName,'Font');
HorRulerOptions.ReadFromIni(IniFile,SectionName,'HorRulerOptions');
VerRulerOptions.ReadFromIni(IniFile,SectionName,'VerRulerOptions');
end;

function TprTxDesignerPanel.EditOptions : boolean;
begin
Result := TprTxDesignerPanelOptionsForm.Create(Application).EditOptions(Self);
if Result then
  UpdateCurPage;
end;

function TprTxDesignerPanel.EditPage(PageIndex : integer) : boolean;
begin
Result := TprTxPageParamsForm.Create(Application).EditOptions(TprTxPage(Report.Pages[PageIndex]));
if Result then
  begin
    if PageIndex=ActivePageIndex then
      UpdateCurPage;
    DsgnNotifyReport(true);
  end;
end;

initialization

RegisterClass(TprTxMemoEditorForm);
RegisterClass(TprTxCommandEditorForm);
RegisterClass(TprBandEditorForm);
RegisterClass(TprTxLineEditorForm);

end.

