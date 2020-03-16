{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_Preview;

interface     

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ImgList, ActnList, StdCtrls, ToolWin, ComCtrls,
  flatsb, clipbrd, inifiles, menus, typinfo,

  vgr_ControlBar, vgr_FontComboBox, vgr_ColorButton,

  pr_Common, pr_Classes, Pr_Utils, pr_MultiLang, pr_DesignerFunctions,
  pr_CommonPreviewPanel, pr_PreviewPanel;

type
  ///////////////////////////////////////////////
  //
  // TprPreviewForm
  //
  ///////////////////////////////////////////////
  TprPreviewForm = class(TprPreview)
    ActionList: TActionList;
    ImageList: TImageList;
    aPrint: TAction;
    aWholePage: TAction;
    aPages: TAction;
    aClose: TAction;
    aPageWidth: TAction;
    aTwoPages: TAction;
    aFind: TAction;
    aFindNext: TAction;
    aFindPrev: TAction;
    aFindCancel: TAction;
    aSave: TAction;
    aOpen: TAction;
    SB: TStatusBar;
    aCustomAction: TAction;
    prMLRes1: TprMLRes;
    aExportToXLS: TAction;
    MainMenu: TMainMenu;
    TEST1: TMenuItem;
    SUBTEST1: TMenuItem;
    TopControlBar: TvgrControlBar;
    tbPreviewCommon: TToolBar;
    ToolButton1: TToolButton;
    ToolButton16: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    EDScale: TEdit;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    tbText: TToolBar;
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
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    Excel1: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    mToolbars: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    CBFontName: TvgrFontComboBox;
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
    aAlignToGridLeftTop: TAction;
    aAlignToGridAll: TAction;
    tbAlign: TToolBar;
    ToolButton19: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    tbSize: TToolBar;
    ToolButton33: TToolButton;
    ToolButton34: TToolButton;
    ToolButton36: TToolButton;
    ToolButton37: TToolButton;
    ToolButton38: TToolButton;
    ToolButton39: TToolButton;
    ToolButton40: TToolButton;
    ToolButton41: TToolButton;
    ToolButton42: TToolButton;
    tbNudge: TToolBar;
    ToolButton43: TToolButton;
    ToolButton44: TToolButton;
    ToolButton45: TToolButton;
    ToolButton46: TToolButton;
    aCut: TAction;
    aCopy: TAction;
    aPaste: TAction;
    aDelete: TAction;
    aBringToFront: TAction;
    aSendToBack: TAction;
    N12: TMenuItem;
    Custom1: TMenuItem;
    aCopy1: TMenuItem;
    aPaste1: TMenuItem;
    N18: TMenuItem;
    aDelete1: TMenuItem;
    N19: TMenuItem;
    aBringToFront1: TMenuItem;
    aSendToBack1: TMenuItem;
    N20: TMenuItem;
    aProperties: TAction;
    aProperties1: TMenuItem;
    N21: TMenuItem;
    aShowGrid: TAction;
    aAlignToGrid: TAction;
    aGridSize: TAction;
    N22: TMenuItem;
    aShowGrid1: TMenuItem;
    aAlignToGrid1: TMenuItem;
    aGridSize1: TMenuItem;
    Page1: TMenuItem;
    aNewPage: TAction;
    aDelPage: TAction;
    aPageParams: TAction;
    aNewPage1: TMenuItem;
    aDelPage1: TMenuItem;
    aPageParams1: TMenuItem;
    tbObject: TToolBar;
    ToolButton47: TToolButton;
    ToolButton48: TToolButton;
    ToolButton49: TToolButton;
    ToolButton50: TToolButton;
    ToolButton51: TToolButton;
    ToolButton52: TToolButton;
    ToolButton53: TToolButton;
    ToolButton54: TToolButton;
    aPreviewEdit: TAction;
    Custom2: TMenuItem;
    N23: TMenuItem;
    aPreviewEdit1: TMenuItem;
    ToolButton55: TToolButton;
    ToolButton56: TToolButton;
    tbObjects: TToolBar;
    bObjArrow: TToolButton;
    LeftControlBar: TvgrControlBar;
    RightControlBar: TvgrControlBar;
    Preview: TprPreviewPanel;
    ControlBarManager: TvgrControlBarManager;
    aOptions: TAction;
    N26: TMenuItem;
    aOptions1: TMenuItem;
    ToolButton57: TToolButton;
    ToolButton58: TToolButton;
    ToolButton59: TToolButton;
    ToolButton60: TToolButton;
    ToolButton61: TToolButton;
    EDPage: TEdit;
    aPreviewFirstPage: TAction;
    aPreviewPriorPage: TAction;
    aPreviewNextPage: TAction;
    aPreviewLastPage: TAction;
    bHJustify: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EDScaleKeyPress(Sender: TObject; var Key: Char);
    procedure aSaveExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure SBDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure aCustomActionUpdate(Sender: TObject);
    procedure aCustomActionExecute(Sender: TObject);
    procedure aExportToXLSUpdate(Sender: TObject);
    procedure aExportToXLSExecute(Sender: TObject);
    procedure CBFontNameClick(Sender: TObject);
    procedure CBFontSizeChange(Sender: TObject);
    procedure bBoldClick(Sender: TObject);
    procedure bFontColorClick(Sender: TObject);
    procedure bHLeftClick(Sender: TObject);
    procedure bVTopClick(Sender: TObject);
    procedure bbTopClick(Sender: TObject);
    procedure bbAllClick(Sender: TObject);
    procedure aHToLeftUpdate(Sender: TObject);
    procedure aHToLeftExecute(Sender: TObject);
    procedure aSLeftExecute(Sender: TObject);
    procedure aOLeftExecute(Sender: TObject);
    procedure mToolbarsClick(Sender: TObject);
    procedure aPreviewEditExecute(Sender: TObject);
    procedure aPreviewEditUpdate(Sender: TObject);
    procedure PreviewSelectionChanged(Sender: TObject; SelCount: Integer);
    procedure aOLeftUpdate(Sender: TObject);
    procedure aSLeftUpdate(Sender: TObject);
    procedure aPrintExecute(Sender: TObject);
    procedure aWholePageExecute(Sender: TObject);
    procedure aPagesExecute(Sender: TObject);
    procedure aPageWidthExecute(Sender: TObject);
    procedure aTwoPagesExecute(Sender: TObject);
    procedure aFindUpdate(Sender: TObject);
    procedure aFindExecute(Sender: TObject);
    procedure aFindActionUpdate(Sender: TObject);
    procedure aFindNextExecute(Sender: TObject);
    procedure aFindPrevExecute(Sender: TObject);
    procedure aFindCancelExecute(Sender: TObject);
    procedure aCutUpdate(Sender: TObject);
    procedure aCopyUpdate(Sender: TObject);
    procedure aPasteUpdate(Sender: TObject);
    procedure aDeleteUpdate(Sender: TObject);
    procedure aCutExecute(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aPasteExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aBringToFrontUpdate(Sender: TObject);
    procedure aBringToFrontExecute(Sender: TObject);
    procedure aSendToBackUpdate(Sender: TObject);
    procedure aSendToBackExecute(Sender: TObject);
    procedure aPropertiesUpdate(Sender: TObject);
    procedure aPropertiesExecute(Sender: TObject);
    procedure aShowGridUpdate(Sender: TObject);
    procedure aShowGridExecute(Sender: TObject);
    procedure aAlignToGridUpdate(Sender: TObject);
    procedure aAlignToGridExecute(Sender: TObject);
    procedure aGridSizeUpdate(Sender: TObject);
    procedure aGridSizeExecute(Sender: TObject);
    procedure aNewPageExecute(Sender: TObject);
    procedure aDelPageExecute(Sender: TObject);
    procedure aPageParamsExecute(Sender: TObject);
    procedure aNewPageUpdate(Sender: TObject);
    procedure aDelPageUpdate(Sender: TObject);
    procedure aPageParamsUpdate(Sender: TObject);
    procedure PreviewScalePercentChanged(Sender: TObject);
    procedure PreviewApplyObjectsProps(Sender: TObject);
    procedure PreviewPageInserted(Sender: TObject;
      InsertedPage: TprEndPage);
    procedure PreviewPageDeleted(Sender: TObject; DeletedPage: TprEndPage);
    procedure aOptionsExecute(Sender: TObject);
    procedure PreviewMouseMove(Sender: TObject;
      PreviewUserData: TprPreviewUserData; X, Y: Integer; var cur: TCursor;
      var HighlightObject: Boolean);
    procedure PreviewMouseDown(Sender: TObject;
      PreviewUserData: TprPreviewUserData; X, Y: Integer;
      Shift: TShiftState);
    procedure PreviewDblClick(Sender: TObject;
      PreviewUserData: TprPreviewUserData; X, Y: Integer);
    procedure aPreviewFirstPageUpdate(Sender: TObject);
    procedure aPreviewNextPageUpdate(Sender: TObject);
    procedure aPreviewFirstPageExecute(Sender: TObject);
    procedure aPreviewPriorPageExecute(Sender: TObject);
    procedure aPreviewNextPageExecute(Sender: TObject);
    procedure aPreviewLastPageExecute(Sender: TObject);
    procedure EDPageKeyPress(Sender: TObject; var Key: Char);
    procedure PreviewShowPageChanged(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FUpdated : boolean;
    FPreviewChanged : boolean;
    FOtherColor : TColor;
    FFontColor : TColor;
    FFillColor : TColor;
    FPaletteForm : TvgrColorPaletteForm;

    FSettingsRestored: Boolean;
    FIniFileName: string;
    FIniFileSection: string;
    
    procedure SetPreviewChanged(Value : boolean);
    procedure OnObjButtonClick(Sender : TObject);
    procedure UpdateToolbar;
    procedure SetPopupColor(IsFillColor : boolean; Color: TColor);
    procedure FontColorSetColor(Sender: TObject; Color: TColor);
    procedure FillColorSetColor(Sender: TObject; Color: TColor);
    function GetToolbar(t : TprPreviewToolbars) : TToolBar;

    procedure UpdateStatusBar;
    procedure UpdateVisibleToolBars;
    procedure InitForm;

    procedure ToolBarMenuItemClick(Sender : TObject);
  protected
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    property PreviewChanged : boolean read FPReviewChanged write SetPreviewChanged;
  end;

implementation

uses
  pr_Strings, pr_GridSize;

{$R *.DFM}

/////////////////////////////////////////////////
//
// TprPreviewForm
//
/////////////////////////////////////////////////
procedure TprPreviewForm.SetPreviewChanged;
begin
FPreviewChanged := Value;
end;

procedure TprPreviewForm.mToolbarsClick(Sender: TObject);
var
  i : integer;
  tb : TToolBar;
begin
with TMenuItem(Sender) do
  for i:=0 to Count-1 do
    begin
      tb := TToolBar(Self.FindComponent(Copy(Items[i].Name,2,Length(Items[i].Name)-1)));
      Items[i].Checked := (tb<>nil) and tb.Visible;
    end;
end;

procedure TprPreviewForm.ToolBarMenuItemClick;
var
  tb : TToolBar;
begin
TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
tb := TToolBar(FindComponent(Copy(TMenuItem(Sender).Name,2,Length(TMenuItem(Sender).Name)-1)));
if tb<>nil then
  ControlBarManager.ShowHideToolBar(tb,TMenuItem(Sender).Checked);
end;

function TprPreviewForm.GetToolbar;
var
  s : string;
begin
s := GetEnumName(TypeInfo(TprPreviewToolbars),integer(t));
s := 'tb'+Copy(s,5,Length(s)-4);
Result := TToolBar(FindComponent(s));
end;

procedure TprPreviewForm.UpdateVisibleToolBars;
var
  i : TprPreviewToolbars;
  tb : TToolBar;
begin
for i:=Low(TprPreviewToolbars) to High(TprPreviewToolbars) do
  begin
    tb := GetToolbar(i);
    if tb<>nil then
      ControlBarManager.ShowHideToolBar(tb,i in TprReport(Report).PreviewParams.ShowToolbars);
  end;
end;

procedure TprPreviewForm.prRestoreProperties;
begin
  Preview.ReadFromIni(ini,sn);
  FIniFileName := Ini.FileName;
  FIniFileSection := sn;
  inherited;
end;

procedure TprPreviewForm.prSaveProperties;
begin
  Preview.WriteToIni(ini,sn);
  ControlBarManager.WriteToIni(ini.FileName, sn + '_TB');
  inherited;
end;

procedure TprPreviewForm.InitForm;
var
  i : TprPreviewToolbars;
  m : TMenuItem;
  tb : TToolBar;
begin
if not (prpoShowMenu in TprReport(Report).PreviewParams.Options) then
  Menu := nil
else
  Menu := MainMenu;
if prpoAllowDragToolbars in TprReport(Report).PreviewParams.Options then
  begin
    tbPreviewCommon.DragKind := dkDock;
    tbText.DragKind := dkDock;
    tbBorders.DragKind := dkDock;
    tbAlign.DragKind := dkDock;
    tbSize.DragKind := dkDock;
    tbNudge.DragKind := dkDock;
    tbObjects.DragKind := dkDock;
    tbObject.DragKind := dkDock;
  end
else
  begin
    tbPreviewCommon.DragKind := dkDrag;
    tbText.DragKind := dkDrag;
    tbBorders.DragKind := dkDrag;
    tbAlign.DragKind := dkDrag;
    tbSize.DragKind := dkDrag;
    tbNudge.DragKind := dkDrag;
    tbObjects.DragKind := dkDrag;
    tbObject.DragKind := dkDrag;
  end;
if Report.CanUserEdit then
  aPreviewEdit.Caption := prLoadStr(sPreviewModePreview)
else
  aPreviewEdit.Caption := prLoadStr(sPreviewModeEdit);
// init toolbars menu
while mToolbars.Count>0 do
  MToolbars.Items[0].Free;
if prpoAllowShowHideToolbars in TprReport(Report).PreviewParams.Options then
  for i:=Low(TprPreviewToolbars) to High(TprPreviewToolbars) do
    begin
      tb := GetToolbar(i);
      if (tb<>nil) and (i in TprReport(Report).PreviewParams.ShowToolbars) then
        begin
          m := TMenuItem.Create(Self);
          m.Name := 'm'+tb.Name;
          m.Caption := tb.Caption;
          m.OnClick := ToolBarMenuItemClick;
          mToolbars.Add(m);
        end;
    end;
mToolbars.Visible := mToolbars.Count>0;
N11.Visible := mToolbars.Count>0;
end;

procedure TprPreviewForm.aPreviewEditExecute(Sender: TObject);
begin
Preview.ClearSelection;
Report.CanUserEdit := not Report.CanUserEdit;
if Report.CanUserEdit then
  begin
    TprReport(Report).PreviewParams.Options := [prpoAllowChangePreviewMode,prpoShowMenu,prpoAllowShowHideToolbars,prpoAllowDragToolbars];
    TprReport(Report).PreviewParams.ShowToolbars := [prptPreviewCommon,prptEdit,prptInsertObject,prptText,prptBorders,prptAlign,prptSize,prptNudge,prptObjects,prptObject];
  end
else
  begin
    Preview.VisibleObjectsPropsForm := false;
    TprReport(Report).PreviewParams.Options := [prpoAllowChangePreviewMode];
    TprReport(Report).PreviewParams.ShowToolbars := [prptPreviewCommon];
  end;
if Assigned(TprReport(Report).OnPreviewModeChanged) then
  TprReport(Report).OnPreviewModeChanged(Self);
UpdateVisibleToolbars;
InitForm;
end;

procedure TprPreviewForm.FormCreate(Sender: TObject);
begin
prLoadResImages(Self,ImageList);
if Report.Title='' then
  Caption := Format(prLoadStr(sPreviewCaption),[prLoadStr(sNoReportName)])
else
  Caption := Format(prLoadStr(sPreviewCaption),[Report.Title]);

InitprObjToolbar(Report,Self,tbObjects,OnObjButtonClick);
bObjArrow.OnClick := OnObjButtonClick;
bObjArrow.Down := true;
InitForm;

Preview.Report := TprReport(Report);
Preview.StatusBar := SB;
Preview.ProgressBar := TProgressBar.Create(Self);
Preview.ProgressBar.Parent := SB;

UpdateStatusBar;
PreviewSelectionChanged(Preview,0);
end;

procedure TprPreviewForm.OnObjButtonClick;
begin
(Sender as TToolButton).Down := true;
if (Sender as TToolButton).Tag=-1 then
  Preview.InsertObject(nil)
else
  Preview.InsertObject(prObjRegInfos[(Sender as TToolButton).Tag].ClassRef);
end;

procedure TprPreviewForm.aCloseExecute(Sender: TObject);
begin
Close;
end;

procedure TprPreviewForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Preview.StatusBar := nil;
Preview.ProgressBar := nil;
Action := caFree;
end;

procedure TprPreviewForm.EDScaleKeyPress(Sender: TObject; var Key: Char);
begin
if (Key=#13) and (Trim(EDScale.Text)<>'') then
  begin
    Preview.ScalePercent := StrToIntDef(Trim(EDScale.Text),100);
    ActiveControl := Preview;
  end;
end;

procedure TprPreviewForm.aSaveExecute(Sender: TObject);
begin
Preview.Save;
end;

procedure TprPreviewForm.aOpenExecute(Sender: TObject);
begin
Preview.Load;
end;

procedure TprPreviewForm.UpdateStatusBar;
begin
if Preview.ActivePageIndex=-1 then
  SB.Panels[0].Text := Format(prLoadStr(sTotalPages),[Report.EndPagesCount])
else
  SB.Panels[0].Text := Format(prLoadStr(sPageNumber),[Preview.ActivePageIndex+1,Report.EndPagesCount]);
end;

procedure TprPreviewForm.SBDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
if Panel.Index=2 then
  Preview.ProgressBar.SetBounds(Rect.Left-1,Rect.Top-1,Rect.Right-Rect.Left+2,Rect.Bottom-Rect.Top+2);
end;

procedure TprPreviewForm.aCustomActionUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Assigned(Report.OnCustomActionInPreview);
end;          

procedure TprPreviewForm.aCustomActionExecute(Sender: TObject);
begin
DoOnCustomAction;
end;

procedure TprPreviewForm.aExportToXLSUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := true;
end;
                                                      
procedure TprPreviewForm.aExportToXLSExecute(Sender: TObject);
begin
TprReport(Report).ExportTo;
end;

procedure TprPreviewForm.UpdateToolbar;
var
  i : integer;
  cs : TCardinalSet;
  fEnabled,fEnabledBorders : boolean;
begin
FUpdated:=true;
fEnabled := false;
fEnabledBorders := false;
if Report.CanUserEdit then
  begin
    i := 0;
    while i<Preview.SelCount do
      begin
        fEnabled := fEnabled or (Preview.SelObjs[i] is TprMemoObjRecVersion);
        fEnabledBorders := fEnabledBorders or (Preview.SelObjs[i] is TprRichObjRecVersion);
        Inc(i);
      end;
  end;

EnableToolbarControls(tbText,fEnabled);
EnableToolbarControls(tbBorders,fEnabledBorders or fEnabled);

if fEnabled or fEnabledBorders then
  begin
    if fEnabled then
      begin
        FFontColor := prGetPropDef(Preview.SelObjsList,'Font.Color',clBlack);
        FFillColor := prGetPropDef(Preview.SelObjsList,'FillColor',clNone);
        CBFontName.ItemIndex := CBFontName.Items.IndexOf(prGetPropDef(Preview.SelObjsList,'Font.Name',''));
        i := prGetPropDef(Preview.SelObjsList,'Font.Size',-1);
        if i=-1 then
          CBFontSize.Text := ''
        else
          begin
            CBFontSize.ItemIndex := CBFontSize.Items.IndexOf(IntToStr(i));
            if CBFontSize.ItemIndex=-1 then
              CBFontSize.Text := IntToStr(i);
          end;

        cs := (TCardinalSet(cardinal(prGetPropDef(Preview.SelObjsList,'Font.Style',0))));
        bBold.Down := cardinal(fsBold) in cs;
        bItalic.Down := cardinal(fsItalic) in cs;
        bUnderline.Down := cardinal(fsUnderLine) in cs;

        case TprHAlign(prGetPropDef(Preview.SelObjsList,'hAlign',-1)) of
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

        case TprVAlign(prGetPropDef(Preview.SelObjsList,'vAlign',-1)) of
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
    end;

    bbTop.Down := prGetPropDef(Preview.SelObjsList,'tBorder.Show',false);
    bbLeft.Down := prGetPropDef(Preview.SelObjsList,'lBorder.Show',false);
    bbRight.Down := prGetPropDef(Preview.SelObjsList,'rBorder.Show',false);
    bbBottom.Down := prGetPropDef(Preview.SelObjsList,'bBorder.Show',false);
  end;
FUpdated := false;
end;

procedure TprPreviewForm.CBFontNameClick(Sender: TObject);
var
  i : integer;
begin
if fUpdated then exit;
for i:=0 to Preview.SelCount-1 do
  if Preview.SelObjs[i] is TprMemoObjRecVersion then
    begin
      TprMemoObjRecVersion(Preview.SelObjs[i]).Font.Name := CBFontName.Items[CBFontName.ItemIndex];
      PreviewChanged := true;
    end;
Preview.UpdateSelectedObjects;
ActiveControl := Preview; 
end;

procedure TprPreviewForm.CBFontSizeChange(Sender: TObject);
var
  i : integer;
begin
if fUpdated then exit;
for i:=0 to Preview.SelCount-1 do
  if Preview.SelObjs[i] is TprMemoObjRecVersion then
    begin
      TprMemoObjRecVersion(Preview.SelObjs[i]).Font.Size := StrToInt(CBFontSize.Text);
      PreviewChanged := true;
    end;
Preview.UpdateSelectedObjects;
ActiveControl := Preview; 
end;

procedure TprPreviewForm.bBoldClick(Sender: TObject);
var
  i : integer;
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
for i:=0 to Preview.SelCount-1 do
  if Preview.SelObjs[i] is TprMemoObjRecVersion then
    begin
      if TToolButton(Sender).Down then
        TprMemoObjRecVersion(Preview.SelObjs[i]).Font.Style := TprMemoObjRecVersion(Preview.SelObjs[i]).Font.Style+fs
      else
        TprMemoObjRecVersion(Preview.SelObjs[i]).Font.Style := TprMemoObjRecVersion(Preview.SelObjs[i]).Font.Style-fs;
      PreviewChanged := true;
    end;
Preview.UpdateSelectedObjects;
end;

procedure TprPreviewForm.SetPopupColor;
var
  i : integer;
begin
if fUpdated then exit;
FOtherColor := FPaletteForm.OtherColor;
for i:=0 to Preview.SelCount-1 do
  if Preview.SelObjs[i] is TprMemoObjRecVersion then
    begin
      if IsFillColor then
        TprMemoObjRecVersion(Preview.SelObjs[i]).FillColor := Color
      else
        TprMemoObjRecVersion(Preview.SelObjs[i]).Font.Color := Color;
      PreviewChanged := true;
    end;
Preview.UpdateSelectedObjects;
end;

procedure TprPreviewForm.FontColorSetColor;
begin
FFontColor := Color;
SetPopupColor(false,Color);
end;

procedure TprPreviewForm.FillColorSetColor;
begin
FFillColor := Color;
SetPopupColor(true,Color);
end;

procedure TprPreviewForm.bFontColorClick(Sender: TObject);
var
  p : TPoint;
begin
if Sender=bFontColor then
  begin
    p := bFontColor.ClientToScreen(point(0,0));
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
      p := bFillColor.ClientToScreen(point(0,0));
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

procedure TprPreviewForm.bHLeftClick(Sender: TObject);
var
  i : integer;
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
for i:=0 to Preview.SelCount-1 do
  if Preview.SelObjs[i] is TprMemoObjRecVersion then
    begin
      TprMemoObjRecVersion(Preview.SelObjs[i]).hAlign := a;
      PreviewChanged := true;
    end;
Preview.UpdateSelectedObjects;
end;

procedure TprPreviewForm.bVTopClick(Sender: TObject);
var
  i : integer;
  a : TprVAlign;
begin
if fUpdated then exit;

if bVTop.Down then
  a:=prvTop
else
  if bVCenter.Down then
    a:=prvCenter
  else
    if bVBottom.Down then
      a:=prvBottom
    else
      exit;

for i:=0 to Preview.SelCount-1 do
  if Preview.SelObjs[i] is TprMemoObjRecVersion then
    begin
      TprMemoObjRecVersion(Preview.SelObjs[i]).vAlign := a;
      PreviewChanged := true;
    end;
Preview.UpdateSelectedObjects;
end;

procedure TprPreviewForm.bbTopClick(Sender: TObject);
var
  s : string;
begin
if fUpdated then exit;
if Sender=bbTop then
  s := 'tBorder.Show'
else
  if Sender=bbLeft then
    s := 'lBorder.Show'
  else
    if Sender=bbBottom then
      s := 'bBorder.Show'
    else
      if Sender=bbRight then
        s := 'rBorder.Show'
      else
        exit;
prSetProp(Preview.SelObjsList,s,TToolButton(Sender).Down,false);
PreviewChanged := true;
Preview.UpdateSelectedObjects;
end;

procedure TprPreviewForm.bbAllClick(Sender: TObject);
begin
if fUpdated then exit;
prSetProp(Preview.SelObjsList,'tBorder.Show',Sender=bbAll,false);
prSetProp(Preview.SelObjsList,'bBorder.Show',Sender=bbAll,false);
prSetProp(Preview.SelObjsList,'lBorder.Show',Sender=bbAll,false);
prSetProp(Preview.SelObjsList,'rBorder.Show',Sender=bbAll,false);
PreviewChanged := true;
Preview.UpdateSelectedObjects;
UpdateToolbar;
end;

procedure TprPreviewForm.aHToLeftUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.AlignActionAllowed(TprAlignActionCode(GetEnumValue(TypeInfo(TprAlignActionCode),'aac'+Copy(TAction(Sender).Name,2,Length(TAction(Sender).Name)))));
end;

procedure TprPreviewForm.aHToLeftExecute(Sender: TObject);
begin
Preview.AlignAction(TprAlignActionCode(GetEnumValue(TypeInfo(TprAlignActionCode),'aac'+Copy(TAction(Sender).Name,2,Length(TAction(Sender).Name)))));
end;

procedure TprPreviewForm.aSLeftExecute(Sender: TObject);
begin
if Sender=aSLeft then
  Preview.Size(-1,0)
else
  if Sender=aSRight then
    Preview.Size(1,0)
  else
    if Sender=aSTop then
      Preview.Size(0,-1)
    else
      if Sender=aSBottom then
        Preview.Size(0,1);
end;

procedure TprPreviewForm.aOLeftExecute(Sender: TObject);
begin
if Sender=aOLeft then
  Preview.Nudge(-1,0)
else
  if Sender=aORight then
    Preview.Nudge(1,0)
  else
    if Sender=aOTop then
      Preview.Nudge(0,-1)
    else
      if Sender=aOBottom then
        Preview.Nudge(0,1);
end;

procedure TprPreviewForm.aPreviewEditUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := prpoAllowChangePreviewMode in TprReport(Report).PreviewParams.Options;
end;

procedure TprPreviewForm.PreviewSelectionChanged(Sender: TObject;
  SelCount: Integer);
var
  i : integer;
begin
UpdateToolbar;
UpdateStatusBar;
case Preview.SelCount of
  0 : SB.Panels[3].Text := prLoadStr(sNoObjectsSelected);
  1 : begin
        i := 0;
        while (i<=High(prObjRegInfos)) and (not(prObjRegInfos[i].ReportRef=TprReport) or (prObjRegInfos[i].RecClassRef.ClassName+'Version'<>Preview.SelObjs[0].ClassName)) do Inc(i);
        if i>High(prObjRegInfos) then
          SB.Panels[3].Text := Preview.SelObjs[0].ClassName
        else
          SB.Panels[3].Text := prLoadStr(prObjRegInfos[i].CaptionResID);
      end;
  else SB.Panels[3].Text := Format(prLoadStr(sSelectedMoreThenOneObject),[Preview.SelCount]);
end;
end;

procedure TprPreviewForm.aOLeftUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.DsgnCanUserEdit and (Preview.GetNumDragSelectedRegions>=1);
end;

procedure TprPreviewForm.aSLeftUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.DsgnCanUserEdit and (Preview.GetNumResizeSelectedRegions>=1);
end;

procedure TprPreviewForm.aPrintExecute(Sender: TObject);
begin
Preview.Print;
end;

procedure TprPreviewForm.aWholePageExecute(Sender: TObject);
begin
Preview.ShowManyPages(1,1);
end;

procedure TprPreviewForm.aPagesExecute(Sender: TObject);
begin
Preview.ShowManyPages(2,2);
end;

procedure TprPreviewForm.aPageWidthExecute(Sender: TObject);
begin
Preview.ScaleMode := smPageWidth;
end;

procedure TprPreviewForm.aTwoPagesExecute(Sender: TObject);
begin
Preview.ShowManyPages(1,2);
end;

procedure TprPreviewForm.aFindUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.AllowFind;
end;

procedure TprPreviewForm.aFindExecute(Sender: TObject);
begin
Preview.Find;
end;

procedure TprPreviewForm.aFindActionUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.IsFindMode;
end;

procedure TprPreviewForm.aFindNextExecute(Sender: TObject);
begin
Preview.FindNext;
end;

procedure TprPreviewForm.aFindPrevExecute(Sender: TObject);
begin
Preview.FindPrior;
end;

procedure TprPreviewForm.aFindCancelExecute(Sender: TObject);
begin
Preview.CancelFind;
end;

procedure TprPreviewForm.aCutUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.AllowCut;
end;

procedure TprPreviewForm.aCopyUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.AllowCopy;
end;

procedure TprPreviewForm.aPasteUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.AllowPaste;
end;

procedure TprPreviewForm.aDeleteUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.AllowDelete;
end;

procedure TprPreviewForm.aCutExecute(Sender: TObject);
begin
Preview.Cut;
end;

procedure TprPreviewForm.aCopyExecute(Sender: TObject);
begin
Preview.Copy;
end;

procedure TprPreviewForm.aPasteExecute(Sender: TObject);
begin
Preview.Paste;
end;

procedure TprPreviewForm.aDeleteExecute(Sender: TObject);
begin
Preview.DeleteSelectedObjects;
end;

procedure TprPreviewForm.aBringToFrontUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.AllowBringToFront;
end;

procedure TprPreviewForm.aBringToFrontExecute(Sender: TObject);
begin
Preview.BringToFront;
end;

procedure TprPreviewForm.aSendToBackUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.AllowSendToBack;
end;

procedure TprPreviewForm.aSendToBackExecute(Sender: TObject);
begin
Preview.SendToBack;
end;

procedure TprPreviewForm.aPropertiesUpdate(Sender: TObject);
begin
TAction(Sender).Checked := Preview.VisibleObjectsPropsForm;
end;

procedure TprPreviewForm.aPropertiesExecute(Sender: TObject);
begin
Preview.VisibleObjectsPropsForm := true;
end;

procedure TprPreviewForm.aShowGridUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Report.CanUserEdit;
TAction(Sender).Checked := Report.CanUserEdit and Preview.ShowGrid;
end;

procedure TprPreviewForm.aShowGridExecute(Sender: TObject);
begin
Preview.ShowGrid := not Preview.ShowGrid;
end;

procedure TprPreviewForm.aAlignToGridUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Report.CanUserEdit;
TAction(Sender).Checked := Report.CanUserEdit and Preview.UseGrid;
end;

procedure TprPreviewForm.aAlignToGridExecute(Sender: TObject);
begin
Preview.UseGrid := not Preview.UseGrid;
end;

procedure TprPreviewForm.aGridSizeUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Report.CanUserEdit;
end;

procedure TprPreviewForm.aGridSizeExecute(Sender: TObject);
var
  GridSize : integer;
begin
GridSize := Preview.GridSize;
if TprGridSizeForm.Create(Application).EditGridSize(GridSize) then
  Preview.GridSize := GridSize;
end;

procedure TprPreviewForm.aNewPageExecute(Sender: TObject);
begin
Preview.InsertPageAfterCurrent;
end;

procedure TprPreviewForm.aDelPageExecute(Sender: TObject);
begin
Preview.DeleteCurrentPage;
end;

procedure TprPreviewForm.aPageParamsExecute(Sender: TObject);
begin
Preview.EditCurrentPage;
end;

procedure TprPreviewForm.aNewPageUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.AllowInsertPage;
end;

procedure TprPreviewForm.aDelPageUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.AllowDeletePage;
end;

procedure TprPreviewForm.aPageParamsUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Preview.AllowEditPage;
end;

procedure TprPreviewForm.PreviewScalePercentChanged(Sender: TObject);
begin
EDScale.Text := Format('%d %%',[Preview.ScalePercent]);
end;

procedure TprPreviewForm.PreviewApplyObjectsProps(Sender: TObject);
begin
UpdateToolBar;
end;

procedure TprPreviewForm.PreviewPageInserted(Sender: TObject;
  InsertedPage: TprEndPage);
begin
UpdateStatusBar;
end;

procedure TprPreviewForm.PreviewPageDeleted(Sender: TObject;
  DeletedPage: TprEndPage);
begin
UpdateStatusBar;
end;

procedure TprPreviewForm.aOptionsExecute(Sender: TObject);
begin
Preview.EditOptions;
end;

procedure TprPreviewForm.PreviewMouseMove(Sender: TObject;
  PreviewUserData: TprPreviewUserData; X, Y: Integer; var cur: TCursor;
  var HighlightObject: Boolean);
begin
if Assigned(Report.OnPreviewMouseMove) then
  Report.OnPreviewMouseMove(Report,PreviewUserData,cur,HighlightObject);
end;

procedure TprPreviewForm.PreviewMouseDown(Sender: TObject;
  PreviewUserData: TprPreviewUserData; X, Y: Integer; Shift: TShiftState);
begin
if Assigned(Report.OnPreviewMouseDown) then
  Report.OnPreviewMouseDown(Report,PreviewUserData,Shift);
end;

procedure TprPreviewForm.PreviewDblClick(Sender: TObject;
  PreviewUserData: TprPreviewUserData; X, Y: Integer);
begin
if Assigned(Report.OnPreviewDblClick) then
  Report.OnPreviewDblClick(Report,PreviewUserData);
end;

procedure TprPreviewForm.aPreviewFirstPageUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Preview.PageIndex > 0;
end;

procedure TprPreviewForm.aPreviewNextPageUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := Preview.PageIndex < Report.EndPagesCount - 1;
end;

procedure TprPreviewForm.aPreviewFirstPageExecute(Sender: TObject);
begin
  Preview.GotoFirstPage;
end;

procedure TprPreviewForm.aPreviewPriorPageExecute(Sender: TObject);
begin
  Preview.GotoPriorPage;
end;

procedure TprPreviewForm.aPreviewNextPageExecute(Sender: TObject);
begin
  Preview.GotoNextPage;
end;

procedure TprPreviewForm.aPreviewLastPageExecute(Sender: TObject);
begin
  Preview.GotoLastPage;
end;

procedure TprPreviewForm.EDPageKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and (Trim(EDPage.Text) <> '') then
  begin
    Preview.GotoPage(StrToIntDef(EDPage.Text, Preview.PageIndex + 1) - 1);
    ActiveControl := Preview;
  end;
end;

procedure TprPreviewForm.PreviewShowPageChanged(Sender: TObject);
begin
  EDPage.Text := IntToStr(Preview.PageIndex + 1);
end;

procedure TprPreviewForm.FormShow(Sender: TObject);
begin
  if not FSettingsRestored then
  begin
    ControlBarManager.ReadFromIni(FIniFileName, FIniFileSection + '_TB');
    if not TprReport(Report).CanUserEdit then
      UpdateVisibleToolBars;
    FSettingsRestored := True;
  end;
end;

initialization

RegisterClass(TprPreviewForm);

end.

