{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_Designer;

interface            
                     
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ActnList, Menus, ExtCtrls,
  TypInfo, ClipBrd, IniFiles, ComCtrls,
  StdCtrls, ToolWin,

  vgr_ControlBar, vgr_ColorButton, vgr_FontComboBox,
  
  Pr_Utils, pr_Common, pr_CommonDesigner, pr_Classes, pr_Link,
  pr_MultiLang, pr_CommonDesignerPanel, pr_DesignerPanel;

type
  /////////////////////////////
  //
  // TprDesignerForm
  //
  /////////////////////////////
  TprDesignerForm = class(TprCustomDesignerForm)
    MainMenu: TMainMenu;
    ActionList: TActionList;
    ImageList: TImageList;
    N1: TMenuItem;
    N2: TMenuItem;
    aOpen: TAction;
    aNew: TAction;
    aSave: TAction;
    aSaveAs: TAction;
    aClose: TAction;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    PMInsertBand: TPopupMenu;
    aGroups: TAction;
    N9: TMenuItem;
    aFunc: TAction;
    N10: TMenuItem;
    aDelete: TAction;
    N11: TMenuItem;
    N12: TMenuItem;
    aShowGrid: TAction;
    aAlignToGrid: TAction;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    AlignActionList: TActionList;
    aHToLeft: TAction;
    aHCenterInWindow: TAction;
    aHCenters: TAction;
    aHSpaceEqually: TAction;
    aHToRight: TAction;
    aVToTop: TAction;
    aVCenterInWindow: TAction;
    aVCenters: TAction;
    aVSpaceEqually: TAction;
    aVToBottom: TAction;
    aOLeft: TAction;
    aORight: TAction;
    aOTop: TAction;
    aOBottom: TAction;
    aSLeft: TAction;
    aSRight: TAction;
    aSTop: TAction;
    aSBottom: TAction;
    aPreview: TAction;
    aPrint: TAction;
    aPageParams: TAction;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    aReportParams: TAction;
    N19: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    aCopy: TAction;
    aPaste: TAction;
    aCut: TAction;
    N22: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    aNewPage: TAction;
    aDelPage: TAction;
    N27: TMenuItem;
    N28: TMenuItem;
    aNextPage: TAction;
    aPriorPage: TAction;
    N29: TMenuItem;
    N30: TMenuItem;
    N31: TMenuItem;
    N32: TMenuItem;
    SB: TStatusBar;
    aGridSize: TAction;
    N33: TMenuItem;
    aObjLinks: TAction;
    N34: TMenuItem;
    N26: TMenuItem;
    N35: TMenuItem;
    aSendToBack: TAction;
    aBringToFront: TAction;
    N37: TMenuItem;
    N38: TMenuItem;
    N39: TMenuItem;
    aProperties: TAction;
    aInplaceEdit: TAction;
    N47: TMenuItem;
    N48: TMenuItem;
    aWToSmall: TAction;
    aWToLarge: TAction;
    aHToSmall: TAction;
    aHToLarge: TAction;
    aAlignToGridLeftTop: TAction;
    aAlignToGridAll: TAction;
    TopControlBar: TvgrControlBar;
    tbObjects: TToolBar;
    bInsertBand: TToolButton;
    ToolButton18: TToolButton;
    bObjArrow: TToolButton;
    tbText: TToolBar;
    tbFile: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    tbObject: TToolBar;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    CBFontSize: TComboBox;
    bBold: TToolButton;
    bItalic: TToolButton;
    bUnderline: TToolButton;
    ToolButton20: TToolButton;
    bFontColor: TToolButton;
    bFillColor: TToolButton;
    ToolButton23: TToolButton;
    bHLeft: TToolButton;
    bHCenter: TToolButton;
    bHRight: TToolButton;
    ToolButton27: TToolButton;
    bVTop: TToolButton;
    bVCenter: TToolButton;
    bVBottom: TToolButton;
    tbBorders: TToolBar;
    bbTop: TToolButton;
    bbLeft: TToolButton;
    bbBottom: TToolButton;
    bbRight: TToolButton;
    ToolButton35: TToolButton;
    bbAll: TToolButton;
    bbNone: TToolButton;
    tbAlign: TToolBar;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton19: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    tbSize: TToolBar;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    ToolButton34: TToolButton;
    ToolButton36: TToolButton;
    ToolButton37: TToolButton;
    ToolButton38: TToolButton;
    ToolButton39: TToolButton;
    ToolButton40: TToolButton;
    tbNudge: TToolBar;
    ToolButton41: TToolButton;
    ToolButton42: TToolButton;
    ToolButton43: TToolButton;
    ToolButton44: TToolButton;
    prMLRes1: TprMLRes;
    N55: TMenuItem;
    N56: TMenuItem;
    mtbFile: TMenuItem;
    mtbObject: TMenuItem;
    mtbObjects: TMenuItem;
    mtbText: TMenuItem;
    mtbBorders: TMenuItem;
    mtbAlign: TMenuItem;
    mtbSize: TMenuItem;
    mtbNudge: TMenuItem;
    ToolButton45: TToolButton;
    aPosSize: TAction;
    N58: TMenuItem;
    CBFontName: TvgrFontComboBox;
    FDesignerPanel: TprDesignerPanel;
    LeftControlBar: TvgrControlBar;
    RightControlBar: TvgrControlBar;
    aOptions: TAction;
    N40: TMenuItem;
    aOptions1: TMenuItem;
    ControlBarManager: TvgrControlBarManager;
    aFind: TAction;
    N41: TMenuItem;
    aFindInTemplate1: TMenuItem;
    aVariables: TAction;
    aVariables1: TMenuItem;
    bHJustify: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure aGroupsExecute(Sender: TObject);
    procedure aFuncExecute(Sender: TObject);
    procedure aDeleteUpdate(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aShowGridExecute(Sender: TObject);
    procedure aAlignToGridExecute(Sender: TObject);
    procedure aShowGridUpdate(Sender: TObject);
    procedure aAlignToGridUpdate(Sender: TObject);
    procedure aHToLeftUpdate(Sender: TObject);
    procedure aSLeftUpdate(Sender: TObject);
    procedure aSTopUpdate(Sender: TObject);
    procedure aOLeftExecute(Sender: TObject);
    procedure aORightExecute(Sender: TObject);
    procedure aOTopExecute(Sender: TObject);
    procedure aOBottomExecute(Sender: TObject);
    procedure aSLeftExecute(Sender: TObject);
    procedure aSRightExecute(Sender: TObject);
    procedure aSTopExecute(Sender: TObject);
    procedure aSBottomExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aPageParamsExecute(Sender: TObject);
    procedure aReportParamsExecute(Sender: TObject);
    procedure aPreviewExecute(Sender: TObject);
    procedure aCopyUpdate(Sender: TObject);
    procedure aPasteUpdate(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aPasteExecute(Sender: TObject);
    procedure aCutExecute(Sender: TObject);
    procedure aNewPageExecute(Sender: TObject);
    procedure aDelPageUpdate(Sender: TObject);
    procedure aDelPageExecute(Sender: TObject);
    procedure aNextPageExecute(Sender: TObject);
    procedure aPriorPageExecute(Sender: TObject);
    procedure aGridSizeExecute(Sender: TObject);
    procedure aObjLinksUpdate(Sender: TObject);
    procedure aObjLinksExecute(Sender: TObject);
    procedure aSendToBackUpdate(Sender: TObject);
    procedure aSendToBackExecute(Sender: TObject);
    procedure aBringToFrontExecute(Sender: TObject);
    procedure SBDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure CBFontNameClick(Sender: TObject);
    procedure CBFontSizeChange(Sender: TObject);
    procedure bBoldClick(Sender: TObject);
    procedure bHLeftClick(Sender: TObject);
    procedure bVCenterClick(Sender: TObject);
    procedure bbTopClick(Sender: TObject);
    procedure bbLeftClick(Sender: TObject);
    procedure bbBottomClick(Sender: TObject);
    procedure bbRightClick(Sender: TObject);
    procedure bbAllClick(Sender: TObject);
    procedure aPropertiesUpdate(Sender: TObject);
    procedure aInplaceEditUpdate(Sender: TObject);
    procedure aPropertiesExecute(Sender: TObject);
    procedure aInplaceEditExecute(Sender: TObject);
    procedure bFontColorClick(Sender: TObject);
    procedure aCommonAction(Sender: TObject);
    procedure mtbNudgeClick(Sender: TObject);
    procedure aPosSizeUpdate(Sender: TObject);
    procedure aPosSizeExecute(Sender: TObject);
    procedure aSaveUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure N56Click(Sender: TObject);
    procedure aHToLeftExecute(Sender: TObject);
    procedure aOLeftUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FDesignerPanelWhileObjectDrag(Sender: TObject;
      const DragRect: TRect);
    procedure aOptionsExecute(Sender: TObject);
    procedure aFindUpdate(Sender: TObject);
    procedure aFindExecute(Sender: TObject);
    procedure aVariablesExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FSelectedTopLeft : TBitmap;
    FSelectedSizes : TBitmap;
    FPaletteForm : TvgrColorPaletteForm;

    FUpdated : boolean;
    FOtherColor : TColor;
    FFontColor : TColor;
    FFillColor : TColor;

    FSettingsRestored: Boolean;
    FIniFileName: string;
    FIniFileSection: string;

    procedure SetPopupColor(IsFillColor : boolean; Color: TColor);
    procedure FontColorSetColor(Sender: TObject; Color: TColor);
    procedure FillColorSetColor(Sender: TObject; Color: TColor);
  protected
    procedure OnDesignerPanelSelectionChanged(Sender : TObject; SelCount: Integer); override;
    function GetDesignerPanel : TprCustomDesignerPanel; override;
    procedure InitDesignerPanel; override;
    procedure UpdateSizesInToolbar; override;
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    procedure UpdateTextActions; override;
  end;

implementation

uses
  pr_Preview, pr_Strings, pr_DesignerFunctions;

{$R *.DFM}

/////////////////////////////////////////////////
//
// TprDesignerForm
//
/////////////////////////////////////////////////
function TprDesignerForm.GetDesignerPanel : TprCustomDesignerPanel;
begin
Result := FDesignerPanel;
end;

procedure TprDesignerForm.InitDesignerPanel;
begin
  FDesignerPanel.Report := Report as TprReport;
  inherited;
end;

procedure TprDesignerForm.prRestoreProperties;
begin
  inherited;
  FIniFileName := Ini.FileName;
  FIniFileSection := sn;
end;

procedure TprDesignerForm.prSaveProperties;
begin
  inherited;
  ControlBarManager.WriteToIni(ini.FileName, sn + '_TB');
end;

procedure TprDesignerForm.FormCreate(Sender: TObject);

  procedure LoadBitmap(var bmp : TBitmap; const ResID : string);
  begin
  bmp := TBitmap.Create;
  LoadResImage(bmp,ResID);
  bmp.TransparentMode := tmAuto;
  bmp.Transparent := true;
  end;

begin
prLoadResImages(Self,ImageList);
LoadBitmap(FSelectedTopLeft,'SELECTEDTOPLEFT');
LoadBitmap(FSelectedSizes,'SELECTEDSIZES');
InitprObjToolbar(Report,Self,tbObjects,OnObjButtonClick);
InitBandsMenu(Self,PMInsertBand,OnInsertBandClick);

fOtherColor := clBlack;

bObjArrow.OnClick := OnObjButtonClick;
bObjArrow.Down := true;

OnDesignerPanelSelectionChanged(FDesignerPanel,0);
end;

procedure TprDesignerForm.FormDestroy(Sender: TObject);
begin
FSelectedTopLeft.Free;
FSelectedSizes.Free;
end;

procedure TprDesignerForm.aSaveExecute(Sender: TObject);
begin
Save;
end;                                       

procedure TprDesignerForm.aSaveAsExecute(Sender: TObject);
begin
SaveAs;
end;

procedure TprDesignerForm.aOpenExecute(Sender: TObject);
begin
Open;
end;

procedure TprDesignerForm.aNewExecute(Sender: TObject);
begin
New;
end;

procedure TprDesignerForm.aCloseExecute(Sender: TObject);
begin
Close;
end;

procedure TprDesignerForm.aGroupsExecute(Sender: TObject);
begin
EditGroups;
end;

procedure TprDesignerForm.aFuncExecute(Sender: TObject);
begin
EditValues;
end;

procedure TprDesignerForm.aDeleteUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := DesignerPanel.AllowDelete;
end;

procedure TprDesignerForm.aDeleteExecute(Sender: TObject);
begin
DesignerPanel.DeleteSelectedObjects;
end;

procedure TprDesignerForm.aShowGridUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit;
TAction(Sender).Checked := FDesignerPanel.ShowGrid;
end;

procedure TprDesignerForm.aShowGridExecute(Sender: TObject);
begin
FDesignerPanel.ShowGrid := not FDesignerPanel.ShowGrid;
end;

procedure TprDesignerForm.aAlignToGridUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit;
TAction(Sender).Checked := FDesignerPanel.UseGrid;
end;

procedure TprDesignerForm.aAlignToGridExecute(Sender: TObject);
begin
FDesignerPanel.UseGrid := not FDesignerPanel.UseGrid;
end;

procedure TprDesignerForm.aSLeftUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit and (DesignerPanel.GetNumResizeSelectedRegions([ppRight,ppRightTop,ppRightBottom])>=1);
end;

procedure TprDesignerForm.aSTopUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit and (DesignerPanel.GetNumResizeSelectedRegions([ppBottom,ppLeftBottom,ppRightBottom])>=1);
end;

procedure TprDesignerForm.aOLeftExecute(Sender: TObject);
begin
if FDesignerPanel.UseGrid then
  DesignerPanel.Nudge(-FDesignerPanel.GridSize,0)
else
  DesignerPanel.Nudge(-1,0);
end;

procedure TprDesignerForm.aORightExecute(Sender: TObject);
begin
if FDesignerPanel.UseGrid then
  DesignerPanel.Nudge(FDesignerPanel.GridSize,0)
else
  DesignerPanel.Nudge(1,0);
end;

procedure TprDesignerForm.aOTopExecute(Sender: TObject);
begin
if FDesignerPanel.UseGrid then
  DesignerPanel.Nudge(0,-FDesignerPanel.GridSize)
else
  DesignerPanel.Nudge(0,-1);
end;

procedure TprDesignerForm.aOBottomExecute(Sender: TObject);
begin
if FDesignerPanel.UseGrid then
  DesignerPanel.Nudge(0,FDesignerPanel.GridSize)
else
  DesignerPanel.Nudge(0,1);
end;

procedure TprDesignerForm.aSLeftExecute(Sender: TObject);
begin
if FDesignerPanel.UseGrid then
  DesignerPanel.Size(-FDesignerPanel.GridSize,0)
else
  DesignerPanel.Size(-1,0);
end;

procedure TprDesignerForm.aSRightExecute(Sender: TObject);
begin
if FDesignerPanel.UseGrid then
  DesignerPanel.Size(FDesignerPanel.GridSize,0)
else
  DesignerPanel.Size(1,0);
end;

procedure TprDesignerForm.aSTopExecute(Sender: TObject);
begin
if FDesignerPanel.UseGrid then
  DesignerPanel.Size(0,FDesignerPanel.GridSize)
else
  DesignerPanel.Size(0,1);
end;

procedure TprDesignerForm.aSBottomExecute(Sender: TObject);
begin
if FDesignerPanel.UseGrid then
  DesignerPanel.Size(0,-FDesignerPanel.GridSize)
else
  DesignerPanel.Size(0,-1);
end;

procedure TprDesignerForm.aPrintExecute(Sender: TObject);
begin
Print;
end;

procedure TprDesignerForm.aPageParamsExecute(Sender: TObject);
begin
if DesignerPanel.EditPage(DesignerPanel.ActivePageIndex) then
  UpdateCurrentPage;
end;

procedure TprDesignerForm.aReportParamsExecute(Sender: TObject);
begin
DesignerPanel.EditReportParams;
end;

procedure TprDesignerForm.aPreviewExecute(Sender: TObject);
begin
Preview;
end;

procedure TprDesignerForm.aCopyUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := DesignerPanel.AllowCopy;
end;

procedure TprDesignerForm.aPasteUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := DesignerPanel.AllowPaste;
end;

procedure TprDesignerForm.aCopyExecute(Sender: TObject);
begin
DesignerPanel.Copy;
end;

procedure TprDesignerForm.aPasteExecute(Sender: TObject);
begin
DesignerPanel.Paste;
end;

procedure TprDesignerForm.aCutExecute(Sender: TObject);
begin
DesignerPanel.Cut;
end;

procedure TprDesignerForm.aNewPageExecute(Sender: TObject);
begin
DesignerPanel.InsertPageAfter(Report.PagesCount-1);
end;

procedure TprDesignerForm.aDelPageUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit and (Report.PagesCount>1);
end;

procedure TprDesignerForm.aDelPageExecute(Sender: TObject);
begin
DesignerPanel.DeletePage(DesignerPanel.ActivePageIndex);
end;

procedure TprDesignerForm.aNextPageExecute(Sender: TObject);
begin
DesignerPanel.NextPage;
end;

procedure TprDesignerForm.aPriorPageExecute(Sender: TObject);
begin
DesignerPanel.PriorPage;
end;

procedure TprDesignerForm.aGridSizeExecute(Sender: TObject);
begin
FDesignerPanel.EditGridSize;
end;

procedure TprDesignerForm.aObjLinksUpdate(Sender: TObject);
begin
TAction(Sender).Checked := DesignerPanel.VisibleObjectLinksForm;
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit;
end;

procedure TprDesignerForm.aObjLinksExecute(Sender: TObject);
begin
DesignerPanel.VisibleObjectLinksForm := not DesignerPanel.VisibleObjectLinksForm;
end;

procedure TprDesignerForm.aSendToBackUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit and DesignerPanel.IsAnyTprObjSelected;
end;

procedure TprDesignerForm.aSendToBackExecute(Sender: TObject);
begin
DesignerPanel.SendToBack;
end;

procedure TprDesignerForm.aBringToFrontExecute(Sender: TObject);
begin
DesignerPanel.BringToFront;
end;

procedure TprDesignerForm.SBDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  b : TBitmap;
begin
if Panel.Index in [1,2] then
  begin
    b := nil;
    case Panel.Index of
      1: b := FSelectedTopLeft;
      2: b := FSelectedSizes;
    end;
    SB.Canvas.FillRect(Rect);
    StatusBar.Canvas.Draw(Rect.Left+1,Rect.Top+1,b);
    SB.Canvas.TextOut(Rect.Left+2+ImageList.Width,Rect.Top+2,Panel.Text);
  end
end;

procedure TprDesignerForm.UpdateTextActions;
var
  i : integer;
  L : TList;
  cs : TCardinalSet;
  fEnabled,fEnabledBorders : boolean;
begin
fUpdated := true;

fEnabled := not DesignerPanel.IsInplaceEdit;
fEnabledBorders := fEnabled;
if fEnabled then
  begin
    i := 0;
    fEnabled := false;
    fEnabledBorders := false;
    while i<DesignerPanel.SelCount do
      begin
        fEnabled := fEnabled or (DesignerPanel.SelObjs[i] is TprMemoObj);
        fEnabledBorders := fEnabledBorders or (DesignerPanel.SelObjs[i] is TprRichObj);
        Inc(i);
      end;
  end;

EnableToolbarControls(tbText,fEnabled);
EnableToolbarControls(tbBorders,fEnabled or fEnabledBorders);

if fEnabled or fEnabledBorders then
  begin
    L := TList.Create;
    try
      MakedRecDefVersionList(DesignerPanel.SelObjsList,L);

      FFontColor := prGetPropDef(L,'Font.Color',clBlack);
      FFillColor := prGetPropDef(L,'FillColor',clNone);
      CBFontName.ItemIndex := CBFontName.Items.IndexOf(prGetPropDef(L,'Font.Name',''));
      i := prGetPropDef(L,'Font.Size',-1);
      if i=-1 then
        CBFontSize.Text := ''
      else
        begin
          CBFontSize.ItemIndex := CBFontSize.Items.IndexOf(IntToStr(i));
          if CBFontSize.ItemIndex=-1 then
            CBFontSize.Text := IntToStr(i);
        end;

      cs := (TCardinalSet(cardinal(prGetPropDef(L,'Font.Style',0))));
      bBold.Down := cardinal(fsBold) in cs;
      bItalic.Down := cardinal(fsItalic) in cs;
      bUnderline.Down := cardinal(fsUnderLine) in cs;

      case TprHAlign(prGetPropDef(L,'hAlign',-1)) of
        prhLeft : bHLeft.Down := true;
        prhCenter : bHCenter.Down := true;
        prhRight : bHRight.Down := true;
        prhJustify: bHJustify.Down := True;
        else
          begin
            bHLeft.Down := false;
            bHCenter.Down := false;
            bHRight.Down := false;
            bHJustify.Down := False;
          end;
      end;

      case TprVAlign(prGetPropDef(L,'vAlign',-1)) of
        prvTop : bVTop.Down := true;
        prvCenter : bVCenter.Down := true;
        prvBottom : bVBottom.Down := true;
        else
          begin
            bVTop.Down := false;
            bVCenter.Down := false;
            bVBottom.Down := false;
          end;
      end;

      bbTop.Down := prGetPropDef(L,'tBorder.Show',false);
      bbLeft.Down := prGetPropDef(L,'lBorder.Show',false);
      bbRight.Down := prGetPropDef(L,'rBorder.Show',false);
      bbBottom.Down := prGetPropDef(L,'bBorder.Show',false);
    finally
      L.Free;
    end;
  end;
fUpdated := false;
end;

procedure TprDesignerForm.CBFontNameClick(Sender: TObject);
var
  i : integer;
  f : boolean;
begin
if fUpdated then exit;

f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  if DesignerPanel.SelObjs[i] is TprMemoObj then
    with TprMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion) do
      begin
        Font.Name := CBFontName.Items[CBFontName.ItemIndex];
        f := true;
      end;
if f then
  DesignerPanel.DsgnNotifyReport(true);
DesignerPanel.UpdateSelectedObjects;
ActiveControl := DesignerPanel;
end;

procedure TprDesignerForm.CBFontSizeChange(Sender: TObject);
var
  i : integer;
  f : boolean;
begin
if fUpdated then exit;

f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  if DesignerPanel.SelObjs[i] is TprMemoObj then
    with TprMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion) do
      begin
        Font.Size := StrToInt(CBFontSize.Text);
        f := true;
      end;
if f then
  DesignerPanel.DsgnNotifyReport(true);
DesignerPanel.UpdateSelectedObjects;
ActiveControl := DesignerPanel;
end;

procedure TprDesignerForm.bBoldClick(Sender: TObject);
var
  i : integer;
  f : boolean;
  fs : TFontStyles;
begin
if fUpdated then exit;
if Sender=bBold then
  fs := [fsBold]
else
  if Sender=bItalic then
    fs := [fsItalic]
  else
    if Sender=bUnderline then
      fs := [fsUnderline]
    else
      exit;
f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  if DesignerPanel.SelObjs[i] is TprMemoObj then
    with TprMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion) do
      begin
        if TToolButton(Sender).Down then
          Font.Style := Font.Style+fs
        else
          Font.Style := Font.Style-fs;
        f := true;
      end;
if f then
  DesignerPanel.DsgnNotifyReport(true);
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprDesignerForm.bHLeftClick(Sender: TObject);
var
  i : integer;
  f : boolean;
  a : TprHAlign;
begin
if fUpdated then exit;

if bHLeft.Down then
  a := prhLeft
else
  if bHCenter.Down then
    a := prhCenter
  else
    if bHRight.Down then
      a := prhRight
    else
      if bHJustify.Down then
        a := prhJustify
      else
        exit;

f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  if DesignerPanel.SelObjs[i] is TprMemoObj then
    with TprMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion) do
      begin
        hAlign := a;
        f := true;
      end;
if f then
  DesignerPanel.DsgnNotifyReport(true);
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprDesignerForm.bVCenterClick(Sender: TObject);
var
  i : integer;
  f : boolean;
  a : TprVAlign;
begin
if fUpdated then exit;

if bVTop.Down then
  a := prvTop
else
  if bVCenter.Down then
    a := prvCenter
  else
    if bVBottom.Down then
      a := prvBottom
    else
      exit;

f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  if DesignerPanel.SelObjs[i] is TprMemoObj then
    with TprMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion) do
      begin
        vAlign := a;
        f := true;
      end;
if f then
  DesignerPanel.DsgnNotifyReport(true);
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprDesignerForm.bbTopClick(Sender: TObject);
var
  i : integer;
  f : boolean;
begin
if fUpdated then exit;
f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  begin
    if DesignerPanel.SelObjs[i] is TprMemoObj then
      begin
        TprMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion).tBorder.Show := bbTop.Down;
        f := true;
      end;
    if DesignerPanel.SelObjs[i] is TprRichObj then
      begin
        TprRichObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion).tBorder.Show := bbTop.Down;
        f := true;
      end;
  end;
if f then
  DesignerPanel.DsgnNotifyReport(true);
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprDesignerForm.bbLeftClick(Sender: TObject);
var
  i : integer;
  f : boolean;
begin
if fUpdated then exit;
f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  begin
    if DesignerPanel.SelObjs[i] is TprMemoObj then
      begin
        TprMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion).lBorder.Show := bbLeft.Down;
        f := true;
      end;
    if DesignerPanel.SelObjs[i] is TprRichObj then
      begin
        TprRichObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion).lBorder.Show := bbLeft.Down;
        f := true;
      end;
  end;
if f then
  DesignerPanel.DsgnNotifyReport(true);
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprDesignerForm.bbBottomClick(Sender: TObject);
var
  i : integer;
  f : boolean;
begin
if fUpdated then exit;
f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  begin
    if DesignerPanel.SelObjs[i] is TprMemoObj then
      begin
        TprMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion).bBorder.Show := bbBottom.Down;
        f := true;
      end;
    if TObject(DesignerPanel.SelObjs[i]) is TprRichObj then
      begin
        TprRichObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion).bBorder.Show := bbBottom.Down;
        f := true;
      end;
  end;
if f then
  DesignerPanel.DsgnNotifyReport(true);
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprDesignerForm.bbRightClick(Sender: TObject);
var
  i : integer;
  f : boolean;
begin
if fUpdated then exit;
f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  begin
    if DesignerPanel.SelObjs[i] is TprMemoObj then
      begin
        TprMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion).rBorder.Show := bbRight.Down;
        f:= true;
      end;
    if DesignerPanel.SelObjs[i] is TprRichObj then
      begin
        TprRichObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion).rBorder.Show := bbRight.Down;
        f := true;
      end;
  end;
if f then
  DesignerPanel.DsgnNotifyReport(true);
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprDesignerForm.bbAllClick(Sender: TObject);
var
  i : integer;
  fAll,f : boolean;
begin
if fUpdated then exit;
if Sender=bbAll then
  fAll := true
else
  if Sender=bbNone then
    fAll := false
  else
    exit;
f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  begin
    if DesignerPanel.SelObjs[i] is TprMemoObj then
      with TprMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion) do
        begin
          tBorder.Show := fAll;
          lBorder.Show := fAll;
          bBorder.Show := fAll;
          rBorder.Show := fAll;
          f := true;
        end;
    if DesignerPanel.SelObjs[i] is TprRichObj then
      with TprRichObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion) do
        begin
          tBorder.Show := fAll;
          lBorder.Show := fAll;
          bBorder.Show := fAll;
          rBorder.Show := fAll;
          f := true;
        end;
  end;
bbTop.Down := fAll;
bbLeft.Down := fAll;
bbBottom.Down := fAll;
bbRight.Down := fAll;
if f then
  DesignerPanel.DsgnNotifyReport(true);
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprDesignerForm.aPropertiesUpdate(Sender: TObject);
begin
TAction(Sender).Checked := DesignerPanel.VisibleObjectsPropsForm;
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit;
end;

procedure TprDesignerForm.aPropertiesExecute(Sender: TObject);
begin
DesignerPanel.VisibleObjectsPropsForm := true;
end;

procedure TprDesignerForm.aInplaceEditUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := DesignerPanel.AllowInplaceEdit and Active;
end;

procedure TprDesignerForm.aInplaceEditExecute(Sender: TObject);
begin
DesignerPanel.InplaceEdit(TprObj(DesignerPanel.SelObjs[0]));
end;

procedure TprDesignerForm.SetPopupColor;
var
  i : integer;
  f : boolean;
begin
if fUpdated then exit;
FOtherColor := FPaletteForm.OtherColor;
f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  if DesignerPanel.SelObjs[i] is TprMemoObj then
    with TprMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion) do
      begin
        if IsFillColor then
          FillColor := Color
        else
          Font.Color := Color;
        f := true;
      end;
if f then
  DesignerPanel.DsgnNotifyReport(true);
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprDesignerForm.FontColorSetColor;
begin
FFontColor := Color;
SetPopupColor(false,Color);
end;

procedure TprDesignerForm.FillColorSetColor;
begin
FFillColor := Color;
SetPopupColor(true,Color);
end;

procedure TprDesignerForm.bFontColorClick(Sender: TObject);
var
  p : TPoint;
begin
if Sender=bFontColor then
  begin
    p := bFontColor.ClientToScreen(Point(0,0));
    FPaletteForm := PopupPaletteForm(Self,
                                     p.x,
                                     p.y+bFontColor.Height,
                                     FFontColor,
                                     FOtherColor,
                                     FontColorSetColor,
                                     prLoadStr(sColorBtnOtherColorCaption),
                                     prLoadStr(sColorBtnNoColorCaption))
  end
else
  if Sender=bFillColor then
    begin
      p := bFillColor.ClientToScreen(Point(0,0));
      FPaletteForm := PopupPaletteForm(Self,
                                       p.x,
                                       p.y+bFillColor.Height,
                                       FFillColor,
                                       FOtherColor,
                                       FillColorSetColor,
                                       prLoadStr(sColorBtnOtherColorCaption),
                                       prLoadStr(sColorBtnNoColorCaption))
    end;
end;

procedure TprDesignerForm.aCommonAction(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit;
end;

procedure TprDesignerForm.mtbNudgeClick(Sender: TObject);
begin
TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
ControlBarManager.ShowHideToolBar(TToolBar(FindComponent(Copy(TMenuItem(Sender).Name,2,Length(TMenuItem(Sender).Name)-1))),TMenuItem(Sender).Checked);
end;

procedure TprDesignerForm.aPosSizeUpdate(Sender: TObject);
begin
TAction(Sender).Checked := DesignerPanel.VisiblePosSizeForm;
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit;
end;

procedure TprDesignerForm.aPosSizeExecute(Sender: TObject);
begin
DesignerPanel.VisiblePosSizeForm := not DesignerPanel.VisiblePosSizeForm;
end;

procedure TprDesignerForm.aSaveUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Report.TemplateChanged and not DesignerPanel.IsInplaceEdit;
end;

procedure TprDesignerForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
CanClose := true;
if not (csDesigning in Report.ComponentState) and Report.TemplateChanged then
  begin
    case MBox(prLoadStr(sTemplaceSaveQuestion),prLoadStr(sAttention),MB_YESNOCANCEL+MB_ICONQUESTION) of
      IDYES : aSave.Execute;
      IDNO : ;
      IDCANCEL : CanClose:=false;
    end;
  end
end;

procedure TprDesignerForm.N56Click(Sender: TObject);
var
  i : integer;
  tb : TToolBar;
begin
for i:=0 to TMenuItem(Sender).Count-1 do
  begin
    tb := TToolBar(FindComponent(Copy(TMenuItem(Sender)[i].Name,2,Length(TMenuItem(Sender)[i].Name)-1)));
    TMenuItem(Sender)[i].Enabled := tb<>nil;
    if tb<>nil then
      TMenuItem(Sender)[i].Checked := tb.Visible
  end;
end;

procedure TprDesignerForm.aHToLeftExecute(Sender: TObject);
begin
DesignerPanel.AlignAction(TprAlignActionCode(GetEnumValue(TypeInfo(TprAlignActionCode),'aac'+Copy(TAction(Sender).Name,2,Length(TAction(Sender).Name)))));
end;

procedure TprDesignerForm.aHToLeftUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := DesignerPanel.AlignActionAllowed(TprAlignActionCode(GetEnumValue(TypeInfo(TprAlignActionCode),'aac'+Copy(TAction(Sender).Name,2,Length(TAction(Sender).Name)))));
end;

procedure TprDesignerForm.aOLeftUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit and (DesignerPanel.GetNumDragSelectedRegions>=1);
end;

procedure TprDesignerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

procedure TprDesignerForm.UpdateSizesInToolbar;
begin
if DesignerPanel.SelCount=0 then
  begin
    SB.Panels[1].Text := '';
    SB.Panels[2].Text := '';
  end
else
  with DesignerPanel.SelectedObjectsRect do
    begin
      SB.Panels[1].Text := Format('%d,%d (%s,%smm)',[Left,
                                                     Top,
                                                     prConvertFromPixelsString(Left,prpsuMM,true),
                                                     prConvertFromPixelsString(Top,prpsuMM,false)]);
      SB.Panels[2].Text := Format('%d,%d (%s,%smm)',[Right-Left,
                                                     Bottom-Top,
                                                     prConvertFromPixelsString(Right-Left,prpsuMM,true),
                                                     prConvertFromPixelsString(Bottom-Top,prpsuMM,false)]);
    end;
end;

procedure TprDesignerForm.FDesignerPanelWhileObjectDrag(Sender: TObject;
  const DragRect: TRect);
begin
with DragRect do
  begin
    SB.Panels[1].Text := Format('%d,%d (%s,%smm)',[Left,
                                                   Top,
                                                   prConvertFromPixelsString(Left,prpsuMM,true),
                                                   prConvertFromPixelsString(Top,prpsuMM,false)]);
    SB.Panels[2].Text := Format('%d,%d (%s,%smm)',[Right-Left,
                                                   Bottom-Top,
                                                   prConvertFromPixelsString(Right-Left,prpsuMM,true),
                                                   prConvertFromPixelsString(Bottom-Top,prpsuMM,false)]);
  end;
end;

procedure TprDesignerForm.OnDesignerPanelSelectionChanged(Sender : TObject; SelCount: Integer);
begin
SB.Panels[3].Text := GetSelectionComponentsDesc;
inherited;
end;

procedure TprDesignerForm.aOptionsExecute(Sender: TObject);
begin
DesignerPanel.EditOptions;
end;

procedure TprDesignerForm.aFindUpdate(Sender: TObject);
begin
TAction(Sender).Checked := DesignerPanel.VisibleFindForm;
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit;
end;

procedure TprDesignerForm.aFindExecute(Sender: TObject);
begin
DesignerPanel.VisibleFindForm := not DesignerPanel.VisibleFindForm;
end;

procedure TprDesignerForm.aVariablesExecute(Sender: TObject);
begin
EditVariables;
end;

procedure TprDesignerForm.FormShow(Sender: TObject);
begin
  if not FSettingsRestored then
  begin
    ControlBarManager.ReadFromIni(FIniFileName, FIniFileSection + '_TB');
    FSettingsRestored := True;
  end;
end;

initialization

RegisterClass(TprDesignerForm);

end.

