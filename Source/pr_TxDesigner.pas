{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxDesigner;

interface

{$I PR.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ActnList, Menus, ExtCtrls,
  TypInfo, ClipBrd, IniFiles, ComCtrls,
  StdCtrls, Grids, ToolWin,

  pr_Utils, pr_Common, pr_CommonDesignerPanel, pr_CommonDesigner, pr_TxClasses, pr_TxConsts, pr_MultiLang,
  pr_TxDesignerPanel, vgr_ControlBar;

type
  /////////////////////////////////////////////////
  //
  // TprTxDesignerForm
  //
  /////////////////////////////////////////////////
  TprTxDesignerForm = class(TprCustomDesignerForm)
    SB: TStatusBar;
    ActionList: TActionList;
    aNew: TAction;                  
    aOpen: TAction;
    aSave: TAction;
    aSaveAs: TAction;
    aQuit: TAction;
    aGroups: TAction;
    aFunc: TAction;
    aDelete: TAction;
    aPreview: TAction;
    aPrint: TAction;
    aPageParams: TAction;
    aReportParams: TAction;
    aCopy: TAction;
    aPaste: TAction;
    aCut: TAction;
    aNewPage: TAction;
    aDelPage: TAction;
    aNextPage: TAction;
    aPriorPage: TAction;
    aObjLinks: TAction;
    aSendToBack: TAction;
    aBringToFront: TAction;
    aProperties: TAction;
    aInplaceEdit: TAction;
    MainMenu: TMainMenu;
    N1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N19: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N2: TMenuItem;
    N22: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    N12: TMenuItem;
    N37: TMenuItem;
    N38: TMenuItem;
    N39: TMenuItem;
    N11: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    N26: TMenuItem;
    N35: TMenuItem;
    N47: TMenuItem;
    N48: TMenuItem;
    N34: TMenuItem;
    N27: TMenuItem;
    N28: TMenuItem;
    N29: TMenuItem;
    N30: TMenuItem;
    N32: TMenuItem;
    N31: TMenuItem;
    ImageList: TImageList;
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
    aWToSmall: TAction;
    aWToLarge: TAction;
    aHToSmall: TAction;
    aHToLarge: TAction;
    PMInsertBand: TPopupMenu;
    PMFontOptions: TPopupMenu;
    TopControlBar: TvgrControlBar;
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
    tbObjects: TToolBar;
    bInsertBand: TToolButton;
    ToolButton18: TToolButton;
    bObjArrow: TToolButton;
    tbText: TToolBar;
    EDFontStyle: TComboBox;
    bFontOptions: TToolButton;
    ToolButton17: TToolButton;
    bHLeft: TToolButton;
    bHCenter: TToolButton;
    bHRight: TToolButton;
    ToolButton22: TToolButton;
    bVTop: TToolButton;
    bVCenter: TToolButton;
    bVBottom: TToolButton;
    tbAlign: TToolBar;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton33: TToolButton;
    ToolButton34: TToolButton;
    ToolButton35: TToolButton;
    ToolButton36: TToolButton;
    tbSize: TToolBar;
    ToolButton37: TToolButton;
    ToolButton38: TToolButton;
    ToolButton39: TToolButton;
    ToolButton40: TToolButton;
    ToolButton41: TToolButton;
    ToolButton42: TToolButton;
    ToolButton43: TToolButton;
    ToolButton44: TToolButton;
    ToolButton45: TToolButton;
    tbNudge: TToolBar;
    ToolButton46: TToolButton;
    ToolButton47: TToolButton;
    ToolButton48: TToolButton;
    ToolButton49: TToolButton;
    prMLRes1: TprMLRes;
    N13: TMenuItem;
    N14: TMenuItem;
    mtbFile: TMenuItem;
    mtbObject: TMenuItem;
    mtbObjects: TMenuItem;
    mtbText: TMenuItem;
    mtbAlign: TMenuItem;
    mtbSize: TMenuItem;
    mtbNudge: TMenuItem;
    aPosSize: TAction;
    ToolButton16: TToolButton;
    N33: TMenuItem;
    LeftControlBar: TvgrControlBar;
    RightControlBar: TvgrControlBar;
    FDesignerPanel: TprTxDesignerPanel;
    aOptions: TAction;
    N15: TMenuItem;
    aOptions1: TMenuItem;
    ControlBarManager: TvgrControlBarManager;
    aFind: TAction;
    N40: TMenuItem;
    aFindInTemplate1: TMenuItem;
    aVariables: TAction;
    aVariables1: TMenuItem;
    bHJustify: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aQuitExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aSaveAsExecute(Sender: TObject);
    procedure aGroupsExecute(Sender: TObject);
    procedure aFuncExecute(Sender: TObject);
    procedure aDeleteUpdate(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aPreviewExecute(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aPageParamsExecute(Sender: TObject);
    procedure aReportParamsExecute(Sender: TObject);
    procedure aCopyUpdate(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aPasteUpdate(Sender: TObject);
    procedure aPasteExecute(Sender: TObject);
    procedure aCutExecute(Sender: TObject);
    procedure aNewPageExecute(Sender: TObject);
    procedure aDelPageUpdate(Sender: TObject);
    procedure aDelPageExecute(Sender: TObject);
    procedure aNextPageExecute(Sender: TObject);
    procedure aPriorPageExecute(Sender: TObject);
    procedure aObjLinksUpdate(Sender: TObject);
    procedure aObjLinksExecute(Sender: TObject);
    procedure aSendToBackUpdate(Sender: TObject);
    procedure aSendToBackExecute(Sender: TObject);
    procedure aBringToFrontExecute(Sender: TObject);
    procedure aPropertiesUpdate(Sender: TObject);
    procedure aPropertiesExecute(Sender: TObject);
    procedure aHToLeftUpdate(Sender: TObject);
    procedure aOLeftExecute(Sender: TObject);
    procedure aORightExecute(Sender: TObject);
    procedure aOTopExecute(Sender: TObject);
    procedure aOBottomExecute(Sender: TObject);
    procedure aSLeftUpdate(Sender: TObject);
    procedure aSTopUpdate(Sender: TObject);
    procedure aSLeftExecute(Sender: TObject);
    procedure aSRightExecute(Sender: TObject);
    procedure aSTopExecute(Sender: TObject);
    procedure aSBottomExecute(Sender: TObject);
    procedure bHLeftClick(Sender: TObject);
    procedure bVTopClick(Sender: TObject);
    procedure SBDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure EDFontStyleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure aInplaceEditUpdate(Sender: TObject);
    procedure aInplaceEditExecute(Sender: TObject);
    procedure aCommonAction(Sender: TObject);
    procedure mtbNudgeClick(Sender: TObject);
    procedure aPosSizeExecute(Sender: TObject);
    procedure aPosSizeUpdate(Sender: TObject);
    procedure aSaveUpdate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure aHToLeftExecute(Sender: TObject);
    procedure aOLeftUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FDesignerPanelDesignerMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure aOptionsExecute(Sender: TObject);
    procedure N14Click(Sender: TObject);
    procedure FDesignerPanelWhileObjectDrag(Sender: TObject;
      const DragRect: TRect);
    procedure aFindUpdate(Sender: TObject);
    procedure aFindExecute(Sender: TObject);
    procedure aVariablesExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FSelectedTopLeft : TBitmap;
    FSelectedSizes : TBitmap;
    FMouse : TBitmap;
    FUpdated : boolean;

    FSettingsRestored: Boolean;
    FIniFileName: string;
    FIniFileSection: string;
  protected
    procedure OnDesignerPanelSelectionChanged(Sender : TObject; SelCount: Integer); override;
    function GetDesignerPanel : TprCustomDesignerPanel; override;
    procedure InitDesignerPanel; override;
    procedure UpdateSizesInToolBar; override;
    procedure mSubScriptClick(Sender: TObject);
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    procedure UpdateTextActions; override;
  end;

function GetEDFontStyleItemIndex(L : TList) : integer;

implementation

uses pr_Strings, pr_DesignerFunctions, pr_TxMemoEditor, pr_TxLineEditor, pr_GroupsEditor,
  pr_ValuesEditor, pr_TxReportParams, pr_TxObjectPosSizeForm,
  pr_BandEditor, pr_TxCommandEditor;

{$R *.DFM}

function GetEDFontStyleItemIndex(L : TList) : integer;
var
  i : integer;
  TxFontStyle : TprTxFontStyle;
begin
TxFontStyle := TprTxMemoObjRecVersion(L[0]).TxFontStyleEx;
i := 1;
while (i<L.Count) and (TprTxMemoObjRecVersion(L[i]).TxFontStyleEx=TxFontStyle) do Inc(i);
if i>=L.Count then
  Result := TxReportOptions.IndexOfTxFontStyle(TxFontStyle)
else
  Result := -1;
end;

type
TprTxDesignerPanelAccess = class(TprTxDesignerPanel)
end;

/////////////////////////////////////////////////
//
// TprTxDesignerForm
//
/////////////////////////////////////////////////
procedure TprTxDesignerForm.prRestoreProperties;
begin
  inherited;
  FIniFileName := Ini.FileName;
  FIniFileSection := sn;
end;

procedure TprTxDesignerForm.prSaveProperties;
begin
  inherited;
  ControlBarManager.WriteToIni(ini.FileName,sn + '_TB');
end;

function TprTxDesignerForm.GetDesignerPanel : TprCustomDesignerPanel;
begin
Result := FDesignerPanel;
end;

procedure TprTxDesignerForm.InitDesignerPanel;
begin
FDesignerPanel.Report := Report as TprTxReport;
inherited;
end;

procedure TprTxDesignerForm.FormCreate(Sender: TObject);
var
  i : integer;
  m : TMenuItem;

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
LoadBitmap(FMouse,'MOUSE');
InitprObjToolbar(Report,Self,tbObjects,OnObjButtonClick);
InitBandsMenu(Self,PMInsertBand,OnInsertBandClick);

// init font styles menu
for i:=0 to TxReportOptions.TxFontOptionsCount-1 do
  begin
    m := TMenuItem.Create(Self);
    m.Caption := TxReportOptions.TxFontOptions[i].Description;
    m.OnClick := mSubScriptClick;
    m.Tag := i;
    PMFontOptions.Items.Add(m);
  end;

// init font styles combo box
for i:=0 to TxReportOptions.TxFontStylesCount-1 do
  EDFontStyle.Items.Add(TxReportOptions.TxFontStyles[i].Description);

bObjArrow.OnClick := OnObjButtonClick;
bObjArrow.Down := true;

OnDesignerPanelSelectionChanged(FDesignerPanel,0);
end;

procedure TprTxDesignerForm.FormDestroy(Sender: TObject);
begin
FSelectedTopLeft.Free;
FSelectedSizes.Free;
FMouse.Free;
end;

procedure TprTxDesignerForm.UpdateTextActions;
var
  L : TList;
  i,j : integer;
  fEnabled : boolean;
begin
fUpdated := true;
i := 0;
while (i<DesignerPanel.SelCount) and (not (DesignerPanel.SelObjs[i] is TprTxMemoObj)) do Inc(i);
fEnabled := i<DesignerPanel.SelCount;

EnableToolBarControls(tbText,fEnabled);

if fEnabled then
  begin
    L := TList.Create;
    try
      MakedRecDefVersionList(DesignerPanel.SelObjsList,L);

      // now - update buttons
      EDFontStyle.ItemIndex := GetEDFontStyleItemIndex(L);

      for i:=0 to TxReportOptions.TxFontOptionsCount-1 do
        begin
          fEnabled := true;
          for j:=0 to L.Count-1 do
            if TObject(L[j]) is TprTxMemoObjRecVersion then
              with TprTxMemoObjRecVersion(L[j]) do
                fEnabled := fEnabled and TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[i]);
          PMFontOptions.Items[i].Checked := fEnabled;
        end;

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

    finally
      L.Free;
    end;
  end;
fUpdated := false;
end;


procedure TprTxDesignerForm.aNewExecute(Sender: TObject);
begin
New;
end;

procedure TprTxDesignerForm.aQuitExecute(Sender: TObject);
begin
Close;
end;

procedure TprTxDesignerForm.aOpenExecute(Sender: TObject);
begin
Open;
end;

procedure TprTxDesignerForm.aSaveExecute(Sender: TObject);
begin
Save;
end;

procedure TprTxDesignerForm.aSaveAsExecute(Sender: TObject);
begin
SaveAs;
end;

procedure TprTxDesignerForm.aGroupsExecute(Sender: TObject);
begin
EditGroups;
end;

procedure TprTxDesignerForm.aFuncExecute(Sender: TObject);
begin
EditValues;
end;

procedure TprTxDesignerForm.aDeleteUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := DesignerPanel.AllowDelete;
end;

procedure TprTxDesignerForm.aDeleteExecute(Sender: TObject);
begin
DesignerPanel.DeleteSelectedObjects;
end;

procedure TprTxDesignerForm.aPreviewExecute(Sender: TObject);
begin
Preview;
end;

procedure TprTxDesignerForm.aPrintExecute(Sender: TObject);
begin
Print;
end;

procedure TprTxDesignerForm.aPageParamsExecute(Sender: TObject);
begin
if DesignerPanel.EditPage(DesignerPanel.ActivePageIndex) then
  UpdateCurrentPage;
end;

procedure TprTxDesignerForm.aReportParamsExecute(Sender: TObject);
begin
if TprTxReportParamsForm.Create(Application).EditParams(TprTxReport(Report)) then
  begin
    UpdateCurrentPage;
    DesignerPanel.DsgnNotifyReport(true);
  end;
end;

procedure TprTxDesignerForm.aCopyUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := DesignerPanel.AllowCopy;
end;

procedure TprTxDesignerForm.aCopyExecute(Sender: TObject);
begin
DesignerPanel.Copy;;
end;

procedure TprTxDesignerForm.aPasteUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := DesignerPanel.AllowPaste;
end;

procedure TprTxDesignerForm.aPasteExecute(Sender: TObject);
begin
DesignerPanel.Paste;
end;

procedure TprTxDesignerForm.aCutExecute(Sender: TObject);
begin
DesignerPanel.Cut;
end;

procedure TprTxDesignerForm.aNewPageExecute(Sender: TObject);
begin
DesignerPanel.InsertPageAfter(Report.PagesCount-1);
end;

procedure TprTxDesignerForm.aDelPageUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit and (Report.PagesCount>1);
end;

procedure TprTxDesignerForm.aDelPageExecute(Sender: TObject);
begin
DesignerPanel.DeletePage(DesignerPanel.ActivePageIndex);
end;

procedure TprTxDesignerForm.aNextPageExecute(Sender: TObject);
begin
DesignerPanel.NextPage;
end;

procedure TprTxDesignerForm.aPriorPageExecute(Sender: TObject);
begin
DesignerPanel.PriorPage;
end;

procedure TprTxDesignerForm.aObjLinksUpdate(Sender: TObject);
begin
TAction(Sender).Checked := DesignerPanel.VisibleObjectLinksForm;
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit;
end;

procedure TprTxDesignerForm.aObjLinksExecute(Sender: TObject);
begin
DesignerPanel.VisibleObjectLinksForm := not DesignerPanel.VisibleObjectLinksForm;
end;

procedure TprTxDesignerForm.aSendToBackUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit and DesignerPanel.IsAnyTprObjSelected;
end;

procedure TprTxDesignerForm.aSendToBackExecute(Sender: TObject);
begin
DesignerPanel.SendToBack;
end;

procedure TprTxDesignerForm.aBringToFrontExecute(Sender: TObject);
begin
DesignerPanel.BringToFront;
end;

procedure TprTxDesignerForm.aPropertiesUpdate(Sender: TObject);
begin
TAction(Sender).Checked := DesignerPanel.VisibleObjectsPropsForm;
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit;
end;

procedure TprTxDesignerForm.aPropertiesExecute(Sender: TObject);
begin
DesignerPanel.VisibleObjectsPropsForm := true;
end;

procedure TprTxDesignerForm.aHToLeftUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := DesignerPanel.AlignActionAllowed(TprAlignActionCode(GetEnumValue(TypeInfo(TprAlignActionCode),'aac'+Copy(TAction(Sender).Name,2,Length(TAction(Sender).Name)))));
end;

procedure TprTxDesignerForm.aOLeftExecute(Sender: TObject);
begin
DesignerPanel.Nudge(-1,0);
end;

procedure TprTxDesignerForm.aORightExecute(Sender: TObject);
begin
DesignerPanel.Nudge(1,0);
end;

procedure TprTxDesignerForm.aOTopExecute(Sender: TObject);
begin
DesignerPanel.Nudge(0,-1);
end;

procedure TprTxDesignerForm.aOBottomExecute(Sender: TObject);
begin
DesignerPanel.Nudge(0,1);
end;

procedure TprTxDesignerForm.aSLeftUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit and (DesignerPanel.GetNumResizeSelectedRegions([ppRight,ppRightTop,ppRightBottom])>=1);
end;

procedure TprTxDesignerForm.aSTopUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit and (DesignerPanel.GetNumResizeSelectedRegions([ppBottom,ppLeftBottom,ppRightBottom])>=1);
end;

procedure TprTxDesignerForm.aSLeftExecute(Sender: TObject);
begin
DesignerPanel.Size(-1,0);
end;

procedure TprTxDesignerForm.aSRightExecute(Sender: TObject);
begin
DesignerPanel.Size(1,0);
end;

procedure TprTxDesignerForm.aSTopExecute(Sender: TObject);
begin
DesignerPanel.Size(0,1);
end;

procedure TprTxDesignerForm.aSBottomExecute(Sender: TObject);
begin
DesignerPanel.Size(0,-1);
end;

procedure TprTxDesignerForm.bHLeftClick(Sender: TObject);
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
  if DesignerPanel.SelObjs[i] is TprTxMemoObj then
    begin
      TprTxMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion).hAlign := a;
      f := true;
    end;
if f then
  DesignerPanel.DsgnNotifyReport(true);    
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprTxDesignerForm.bVTopClick(Sender: TObject);
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
  if DesignerPanel.SelObjs[i] is TprTxMemoObj then
    begin
      TprTxMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion).vAlign := a;
      f := true;
    end;
if f then
  DesignerPanel.DsgnNotifyReport(true);    
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprTxDesignerForm.SBDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  b : TBitmap;
begin
if Panel.Index in [1,2,3] then
  begin
    b:=nil;
    case Panel.Index of
      1: b:=FSelectedTopLeft;
      2: b:=FSelectedSizes;
      3: b:=FMouse;
    end;

    SB.Canvas.FillRect(Rect);
    StatusBar.Canvas.Draw(Rect.Left+1,Rect.Top+1,b);
    SB.Canvas.TextOut(Rect.Left+2+ImageList.Width,Rect.Top+2,Panel.Text);
  end
end;

procedure TprTxDesignerForm.EDFontStyleClick(Sender: TObject);
var
  i : integer;
  f : boolean;
begin
if fUpdated then exit;

f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  if DesignerPanel.SelObjs[i] is TprTxMemoObj then
    begin
      with TprTxMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion) do
        TxFontStyleEx := TxReportOptions.TxFontStyles[EDFontStyle.ItemIndex];
      f := true;
    end;
if f then
  DesignerPanel.DsgnNotifyReport(true);    
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprTxDesignerForm.mSubScriptClick(Sender: TObject);
var
  i : integer;
  f : boolean;
begin
if fUpdated then exit;

f := false;
for i:=0 to DesignerPanel.SelCount-1 do
  if DesignerPanel.SelObjs[i] is TprTxMemoObj then
    with TprTxMemoObjRecVersion(TprObj(DesignerPanel.SelObjs[i]).DefVersion) do
      begin
        if TMenuItem(Sender).Checked then
          TxFontOptionsEx.Remove(TxReportOptions.TxFontOptions[TMenuItem(Sender).Tag])
        else
          TxFontOptionsEx.Add(TxReportOptions.TxFontOptions[TMenuItem(Sender).Tag]);
        f := true;
      end;
if f then
  DesignerPanel.DsgnNotifyReport(true);
TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
DesignerPanel.UpdateSelectedObjects;
end;

procedure TprTxDesignerForm.aInplaceEditUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := DesignerPanel.AllowInplaceEdit and Active;
end;

procedure TprTxDesignerForm.aInplaceEditExecute(Sender: TObject);
begin
DesignerPanel.InplaceEdit(TprObj(DesignerPanel.SelObjs[0]));
end;

procedure TprTxDesignerForm.aCommonAction(Sender: TObject);
begin
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit;
end;

procedure TprTxDesignerForm.mtbNudgeClick(Sender: TObject);
begin
with TMenuItem(Sender) do
  begin
    Checked := not Checked;
    TToolBar(Self.FindComponent(Copy(Name,2,Length(Name)-1))).Visible := Checked;
  end;
end;

procedure TprTxDesignerForm.aPosSizeExecute(Sender: TObject);
begin
DesignerPanel.VisiblePosSizeForm := not DesignerPanel.VisiblePosSizeForm;
end;

procedure TprTxDesignerForm.aPosSizeUpdate(Sender: TObject);
begin
TAction(Sender).Checked := DesignerPanel.VisiblePosSizeForm;
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit;
end;

procedure TprTxDesignerForm.aSaveUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Report.TemplateChanged and not DesignerPanel.IsInplaceEdit;
end;

procedure TprTxDesignerForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
CanClose := true;
if not (csDesigning in Report.ComponentState) and Report.TemplateChanged then
  begin
    case MBox(prLoadStr(sTemplaceSaveQuestion),prLoadStr(sAttention),MB_YESNOCANCEL+MB_ICONQUESTION) of
      IDYES   : aSave.Execute;
      IDNO    : ;
      IDCANCEL: CanClose:=false;
    end;
  end
end;

procedure TprTxDesignerForm.aHToLeftExecute(Sender: TObject);
begin
DesignerPanel.AlignAction(TprAlignActionCode(GetEnumValue(TypeInfo(TprAlignActionCode),'aac'+Copy(TAction(Sender).Name,2,Length(TAction(Sender).Name)))));
end;

procedure TprTxDesignerForm.aOLeftUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := DesignerPanel.GetNumDragSelectedRegions>=1;
end;

procedure TprTxDesignerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

procedure TprTxDesignerForm.OnDesignerPanelSelectionChanged(Sender : TObject; SelCount: Integer);
begin
SB.Panels[4].Text := GetSelectionComponentsDesc;
inherited;
end;

procedure TprTxDesignerForm.UpdateSizesInToolBar;
begin
if DesignerPanel.SelCount=0 then
  begin
    SB.Panels[1].Text := '';
    SB.Panels[2].Text := '';
  end
else
  with DesignerPanel.SelectedObjectsRect do
    begin
      SB.Panels[1].Text := Format('%d,%d',[Left,Top]);
      SB.Panels[2].Text := Format('%d,%d',[Right-Left,Bottom-Top]);
    end;
end;

procedure TprTxDesignerForm.FDesignerPanelDesignerMouseMove(
  Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
SB.Panels[3].Text := Format('%d,%d',
                            [TprTxDesignerPanelAccess(DesignerPanel).ConvertXFromDesignerCoords(X + DesignerPanel.HorScrollBoxPos),
                             TprTxDesignerPanelAccess(DesignerPanel).ConvertYFromDesignerCoords(Y + DesignerPanel.VerScrollBoxPos)]);
end;

procedure TprTxDesignerForm.aOptionsExecute(Sender: TObject);
begin
DesignerPanel.EditOptions;
end;

procedure TprTxDesignerForm.N14Click(Sender: TObject);
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

procedure TprTxDesignerForm.FDesignerPanelWhileObjectDrag(Sender: TObject;
  const DragRect: TRect);
begin
with DragRect do
  begin
    SB.Panels[1].Text := Format('%d,%d',[Left,
                                         Top]);
    SB.Panels[2].Text := Format('%d,%d',[Right-Left,
                                         Bottom-Top]);
  end;
end;

procedure TprTxDesignerForm.aFindUpdate(Sender: TObject);
begin
TAction(Sender).Checked := DesignerPanel.VisibleFindForm;
TAction(Sender).Enabled := not DesignerPanel.IsInplaceEdit;
end;

procedure TprTxDesignerForm.aFindExecute(Sender: TObject);
begin
DesignerPanel.VisibleFindForm := not DesignerPanel.VisibleFindForm;
end;

procedure TprTxDesignerForm.aVariablesExecute(Sender: TObject);
begin
EditVariables;
end;

procedure TprTxDesignerForm.FormShow(Sender: TObject);
begin
  if not FSettingsRestored then
  begin
    ControlBarManager.ReadFromIni(FIniFileName, FIniFileSection + '_TB');
    FSettingsRestored := True;
  end;
end;

initialization

RegisterClass(TprTxDesignerForm);

end.

