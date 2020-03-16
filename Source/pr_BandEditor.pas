{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_BandEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls,

  pr_CommonDesignerPanel, pr_Common, pr_MultiLang, pr_Strings;

{$I PR.INC}

type
  TprBandEditorForm = class(TprPropsForm)
    PC: TPageControl;
    PCommon: TTabSheet;
    Label1: TLabel;
    Label7: TLabel;
    EDName: TEdit;
    CBUseVerticalBands: TCheckBox;
    CBResizeMode: TComboBox;
    CBUseHorizontalBands: TCheckBox;
    PData: TTabSheet;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    EDDataSet: TComboBox;
    EDGroup: TComboBox;
    EDDetailBand: TComboBox;
    EDParent: TComboBox;
    PGroups: TTabSheet;
    bGroupUp: TSpeedButton;
    bGroupDown: TSpeedButton;
    LBGroups: TListBox;
    PChildBands: TTabSheet;
    bChildUp: TSpeedButton;
    bChildDown: TSpeedButton;
    LBChilds: TListBox;
    prMLRes1: TprMLRes;
    CBVisible: TCheckBox;
    PColumns: TTabSheet;
    Label6: TLabel;
    EDColCount: TEdit;
    UDColCount: TUpDown;
    Label8: TLabel;
    EDColDirection: TComboBox;
    POther: TTabSheet;
    CBPrintOnFirstPage: TCheckBox;
    CBReprintOnEachPage: TCheckBox;
    CBLinkToDetail: TCheckBox;
    CBStartNewPage: TCheckBox;
    Label9: TLabel;
    EDPrintWithBand: TComboBox;
    CBPrintAfterLastBandOnPage: TCheckBox;
    PConditions: TTabSheet;
    EDValid: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    EDVisibleFormula: TEdit;
    Label12: TLabel;
    EDPrintWithChildDetail: TComboBox;
    CBPrintOnLastPage: TCheckBox;
    CBCanSplit: TCheckBox;
    Label13: TLabel;
    EDSubReport: TComboBox;
    edMinDataRecords: TEdit;
    udMinDataRecords: TUpDown;
    Label14: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LBGroupsClick(Sender: TObject);
    procedure LBChildsClick(Sender: TObject);
    procedure bGroupUpClick(Sender: TObject);
    procedure bGroupDownClick(Sender: TObject);
    procedure bChildUpClick(Sender: TObject);
    procedure bChildDownClick(Sender: TObject);
  private
    { Private declarations }
    function Obj : TprBand;
    procedure SetEnable;
    procedure UpdateButtons;
    procedure CopyToControls;
    procedure CopyFromControls;
  public
    { Public declarations }
    procedure Apply; override;
    procedure UpdateInfo; override;
  end;

implementation

uses vgr_Functions, pr_DesignerFunctions;

{$R *.DFM}

procedure TprBandEditorForm.FormCreate(Sender: TObject);
begin
LoadResImage(bGroupUp.Glyph,'MOVEUP');
LoadResImage(bGroupDown.Glyph,'MOVEDOWN');
LoadResImage(bChildUp.Glyph,'MOVEUP');
LoadResImage(bChildDown.Glyph,'MOVEDOWN');

GetEnumNamesToStrings(TypeInfo(TprColDirectionType), EDColDirection.Items);
CBResizeMode.Items.Add(prLoadStr(sBandResizeNoChange));
CBResizeMode.Items.Add(prLoadStr(sBandResizeMaxBottom));
CBResizeMode.Items.Add(prLoadStr(sBandResizeMaxBottomInLinksList));
CBResizeMode.Items.Add(prLoadStr(sBandResizeMinBottomInLinksList));
end;

procedure TprBandEditorForm.UpdateInfo;
begin
CopyToControls;
end;

procedure TprBandEditorForm.UpdateButtons;
begin
bGroupUp.Enabled := LBGroups.ItemIndex>0;
bGroupDown.Enabled := LBGroups.ItemIndex<(LBGroups.Items.Count-1);
bChildUp.Enabled := LBChilds.ItemIndex>0;
bChildDown.Enabled := LBChilds.ItemIndex<(LBChilds.Items.Count-1);
end;

procedure TprBandEditorForm.SetEnable;
var
  bt : TprBandType;
begin
if DesignerPanel.SelCount<0 then
  bt := TprBandType(-1)
else
  bt := TprBand(DesignerPanel.SelObjs[0]).BandType;

Label1.Enabled := DesignerPanel.SelCount=1;
EDName.Enabled := DesignerPanel.SelCount=1;
Label6.Enabled := bt in [bthDetail,bthDetailHeader,bthDetailFooter,bthGroupFooter,bthGroupHeader];
Label8.Enabled := bt in [bthDetail,bthDetailHeader,bthDetailFooter,bthGroupFooter,bthGroupHeader];
EDColCount.Enabled := bt in [bthDetail,bthDetailHeader,bthDetailFooter,bthGroupFooter,bthGroupHeader];
UDColCount.Enabled := bt in [bthDetail,bthDetailHeader,bthDetailFooter,bthGroupFooter,bthGroupHeader];
EDColDirection.Enabled := bt in [bthDetail,bthDetailHeader,bthDetailFooter,bthGroupFooter,bthGroupHeader];

CBUseVerticalBands.Enabled := bt in [bthDetail..bthGroupFooter];
CBUseHorizontalBands.Enabled := bt in [btvDetail..btvGroupFooter];
CBCanSplit.Enabled := not (bt in [bthTitle, bthPageHeader, bthPageFooter, btvTitle, btvPageHeader, btvPageFooter]);

Label9.Enabled := bt in [bthSummary,btvSummary];
EDPrintWithBand.Enabled := bt in [bthSummary,btvSummary];
CBPrintOnFirstPage.Enabled := bt in [bthPageHeader,bthPageFooter,btvPageHeader,btvPageFooter];
CBPrintOnLastPage.Enabled := bt in [bthPageFooter,btvPageFooter];
CBReprintOnEachPage.Enabled := bt in [btvDetailHeader, bthDetailHeader, bthGroupHeader, btvGroupHeader];
CBLinkToDetail.Enabled := bt in [btvDetailHeader,btvDetailFooter,bthDetailHeader,bthDetailFooter,btvGroupHeader,btvGroupFooter,bthGroupHeader,bthGroupFooter];
CBStartNewPage.Enabled := bt in [bthGroupHeader,btvGroupHeader, bthDetail, btvDetail];
Label12.Enabled := bt in [bthDetail, btvDetail];
EDPrintWithChildDetail.Enabled := bt in [bthDetail, btvDetail];
CBPrintAfterLastBandOnPage.Enabled := bt in [bthPageFooter,btvPageFooter];

EDDataSet.Enabled := bt in [bthDetail,btvDetail];
Label2.Enabled := bt in [bthDetail,btvDetail];

Label3.Enabled := bt in [bthGroupHeader,bthGroupFooter,btvGroupHeader,btvGroupFooter];
EDGroup.Enabled := bt in [bthGroupHeader,bthGroupFooter,btvGroupHeader,btvGroupFooter];

Label4.Enabled := bt in [bthDetailHeader,bthDetailFooter,btvDetailHeader,btvDetailFooter];
EDDetailBand.Enabled := bt in [bthDetailHeader,bthDetailFooter,btvDetailHeader,btvDetailFooter];

Label5.Enabled := bt in [bthDetail,btvDetail];
EDParent.Enabled := bt in [bthDetail,btvDetail];
EDValid.Enabled := bt in [bthDetail,btvDetail];
Label10.Enabled := bt in [bthDetail,btvDetail];

LBGroups.Enabled := (DesignerPanel.SelCount=1) and (bt in [bthDetail,btvDetail]);
LBChilds.Enabled := (DesignerPanel.SelCount=1) and (bt in [bthDetail,btvDetail]);

Label13.Enabled := bt in [bthDetailHeader, bthDetailFooter,
                          bthGroupHeader, bthGroupFooter,
                          bthDetail, bthSummary];
EDSubReport.Enabled := Label13.Enabled;

Label14.Enabled := bt in [bthGroupHeader, bthGroupFooter, btvGroupHeader, btvGroupFooter];
edMinDataRecords.Enabled := Label14.Enabled;
udMinDataRecords.Enabled := Label14.Enabled;
end;

procedure TprBandEditorForm.CopyToControls;
var
  b : TprBand;
  i : integer;
begin
SetEnable;

b:=TprBand(DesignerPanel.SelObjs[0]);

LBGroups.Items.Clear;
LBChilds.Items.Clear;

b.Page.Report.GetAvailableDataSets(EDDataSet.Items);
EDDataSet.Items.InsertObject(0, prLoadStr(NullPointerString), nil);

EDGroup.Items.Clear;
EDGroup.Items.AddObject(prLoadStr(NullPointerString), nil);

EDDetailBand.Items.Clear;
EDDetailBand.Items.AddObject(prLoadStr(NullPointerString), nil);

EDParent.Items.Clear;
EDParent.Items.AddObject(prLoadStr(NullPointerString), nil);

EDPrintWithBand.Items.Clear;
EDPrintWithBand.Items.AddObject(prLoadStr(NullPointerString), nil);

EDPrintWithChildDetail.Items.Clear;
EDPrintWithChildDetail.Items.AddObject(prLoadStr(NullPointerString), nil);

b.Page.Report.GetAvailableSubReports(EDSubReport.Items);
EDSubReport.Items.InsertObject(0, prLoadStr(NullPointerString), nil);

  if (DesignerPanel.SelCount=1) and
     (TprBand(DesignerPanel.SelObjs[0]).BandType in [bthDetail,btvDetail]) then
  begin
    b := TprBand(DesignerPanel.SelObjs[0]);
    case b.BandType of
      btvDetail:
        begin
          with TprCustomVDetailBand(b) do
            begin
              for i:=0 to Groups.Count-1 do
                LBGroups.Items.AddObject(Groups[i].Name,Groups[i]);
              if LBGroups.Items.Count>0 then
                LBGroups.ItemIndex:=0;

              for i:=0 to Bands.Count-1 do
              begin
                if Bands[i].BandType in [btvDetail] then
                begin
                  LBChilds.Items.AddObject(Bands[i].Name,Bands[i]);
                  EDPrintWithChildDetail.Items.AddObject(Bands[i].Name,Bands[i])
                end;
              end;
              if LBChilds.Items.Count>0 then
                LBChilds.ItemIndex:=0;
            end;
        end;
      bthDetail:
        begin
          with TprCustomHDetailBand(b) do
            begin
              for i:=0 to Groups.Count-1 do
                LBGroups.Items.AddObject(Groups[i].Name,Groups[i]);
              if LBGroups.Items.Count>0 then
                LBGroups.ItemIndex:=0;

              for i:=0 to Bands.Count-1 do
              begin
                if Bands[i].BandType in [bthDetail] then
                begin
                  LBChilds.Items.AddObject(Bands[i].Name,Bands[i]);
                  EDPrintWithChildDetail.Items.AddObject(Bands[i].Name,Bands[i])
                end;
              end;
              if LBChilds.Items.Count>0 then
                LBChilds.ItemIndex:=0;
            end;
        end;
    end;
  end;

if b.BandType in [bthGroupHeader,bthGroupFooter,btvGroupHeader,btvGroupFooter] then
  begin
    for i:=0 to b.Page.Report.Groups.Count-1 do
      if (b.Page.Report.Groups[i].DetailBand<>nil) and
         ((b.BandType in [bthGroupHeader,bthGroupFooter]) and
          (b.Page.Report.Groups[i].DetailBand.BandType in [bthDetail])) or
         ((b.BandType in [btvGroupHeader,btvGroupFooter]) and
          (b.Page.Report.Groups[i].DetailBand.BandType in [btvDetail])) then
        EDGroup.Items.AddObject(b.Page.Report.Groups[i].Name,
                                b.Page.Report.Groups[i]);
  end;

if b.BandType in [bthDetailHeader,bthDetailFooter,btvDetailHeader,btvDetailFooter] then
  begin
    for i:=0 to b.Page.Bands.Count-1 do
      if ((b.BandType in [bthDetailHeader,bthDetailFooter]) and
          (b.Page.Bands[i].BandType in [bthDetail])) or
         ((b.BandType in [btvDetailHeader,btvDetailFooter]) and
          (b.Page.Bands[i].BandType in [btvDetail])) then
        EDDetailBand.Items.AddObject(b.Page.Bands[i].Name,b.Page.Bands[i]);
  end;

if b.BandType in [bthDetail,btvDetail] then
  begin
    for i:=0 to b.Page.Bands.Count-1 do
      if b<>b.Page.Bands[i] then
        begin
          if ((b.BandType in [bthDetail]) and (b.Page.Bands[i].BandType in [bthDetail])) or
             ((b.BandType in [btvDetail]) and (b.Page.Bands[i].BandType in [btvDetail])) then
            EDParent.Items.AddObject(b.Page.Bands[i].Name,b.Page.Bands[i]);
        end;
  end;

if b.BandType in [bthSummary,btvSummary] then
  begin
    EDPrintWithBand.Items.AddObject(prLoadStr(NullPointerString),nil);
    for i:=0 to b.Page.Bands.Count-1 do
      if b<>b.Page.Bands[i] then
        begin
          if ((b.BandType in [bthSummary]) and (b.Page.Bands[i].BandType in [bthTitle..bthGroupFooter])) or
             ((b.BandType in [btvSummary]) and (b.Page.Bands[i].BandType in [btvTitle..btvGroupFooter])) then
            EDPrintWithBand.Items.AddObject(b.Page.Bands[i].Name,b.Page.Bands[i]);
        end;
  end;

if DesignerPanel.SelCount=1 then
  EDName.Text := TprBand(DesignerPanel.SelObjs[0]).Name
else
  EDName.Text := '';
EDVisibleFormula.Text := TprBand(DesignerPanel.SelObjs[0]).VisibleFormula;
CBResizeMode.ItemIndex := prGetPropDef(DesignerPanel.SelObjsList,'ResizeMode',-1);
CBPrintOnFirstPage.State := prGetPropDefBool(DesignerPanel.SelObjsList,'PrintOnFirstPage');
CBPrintOnLastPage.State := prGetPropDefBool(DesignerPanel.SelObjsList,'PrintOnLastPage');
CBReprintOnEachPage.State := prGetPropDefBool(DesignerPanel.SelObjsList,'ReprintOnEachPage');
CBLinkToDetail.State := prGetPropDefBool(DesignerPanel.SelObjsList,'LinkToDetail');
CBStartNewPage.State := prGetPropDefBool(DesignerPanel.SelObjsList,'StartNewPage');
CBPrintAfterLastBandOnPage.State := prGetPropDefBool(DesignerPanel.SelObjsList,'PrintAfterLastBandOnPage');
UDColCount.Position := prGetPropDef(DesignerPanel.SelObjsList,'ColCount',-1);
EDColDirection.ItemIndex := prGetPropDef(DesignerPanel.SelObjsList,'ColDirection',-1);
CBUseVerticalBands.State := prGetPropDefBool(DesignerPanel.SelObjsList,'UseVerticalBands');
CBUseHorizontalBands.State := prGetPropDefBool(DesignerPanel.SelObjsList,'UseHorizontalBands');
CBVisible.State := prGetPropDefBool(DesignerPanel.SelObjsList,'Visible');

CBCanSplit.State := prGetPropDefBool(DesignerPanel.SelObjsList,'CanSplit');

EDPrintWithBand.ItemIndex := EDPrintWithBand.Items.IndexOfObject(pointer(integer(prGetPropDef(DesignerPanel.SelObjsList,'PrintWithBand',-1))));

prSetComboBoxString(EDDataSet, DesignerPanel.SelObjsList, 'DatasetName');
prSetComboBoxString(EDSubReport, DesignerPanel.SelObjsList, 'SubReportName');

//EDDataSet.ItemIndex := EDDataSet.Items.IndexOf(prGetPropDef(DesignerPanel.SelObjsList,'DatasetName',''));
//EDSubReport.ItemIndex := EDSubReport.Items.IndexOf(prGetPropDef(DesignerPanel.SelObjsList,'SubReportName',''));

EDGroup.ItemIndex := EDGroup.Items.IndexOfObject(pointer(integer(prGetPropDef(DesignerPanel.SelObjsList,'Group',-1))));
EDDetailBand.ItemIndex := EDDetailBand.Items.IndexOfObject(pointer(integer(prGetPropDef(DesignerPanel.SelObjsList,'DetailBand',-1))));
EDParent.ItemIndex := EDParent.Items.IndexOfObject(pointer(integer(prGetPropDef(DesignerPanel.SelObjsList,'ParentDetail',-1))));
EDValid.Text := prGetPropDef(DesignerPanel.SelObjsList,'Valid','');
EDPrintWithChildDetail.ItemIndex := EDPrintWithChildDetail.Items.IndexOfObject(pointer(integer(prGetPropDef(DesignerPanel.SelObjsList,'PrintWithChildDetail',-1))));

udMinDataRecords.Position := prGetPropDef(DesignerPanel.SelObjsList, 'MinDataRecords', 0);

UpdateButtons;
end;

procedure TprBandEditorForm.CopyFromControls;
begin
if EDName.Enabled then
  Obj.Name := EDName.Text;

prSetProp(DesignerPanel.SelObjsList,'VisibleFormula',EDVisibleFormula.Text,false);
prSetProp(DesignerPanel.SelObjsList,'ResizeMode',CBResizeMode.ItemIndex,CBResizeMode.ItemIndex=-1);
prSetProp(DesignerPanel.SelObjsList,'PrintOnFirstPage',CBPrintOnFirstPage.State=cbChecked,CBPrintOnFirstPage.State=cbGrayed);
prSetProp(DesignerPanel.SelObjsList,'PrintOnLastPage',CBPrintOnLastPage.State=cbChecked,CBPrintOnLastPage.State=cbGrayed);
prSetProp(DesignerPanel.SelObjsList,'ReprintOnEachPage',CBReprintOnEachPage.State=cbChecked,CBReprintOnEachPage.State=cbGrayed);
prSetProp(DesignerPanel.SelObjsList,'LinkToDetail',CBLinkToDetail.State=cbChecked,CBLinkToDetail.State=cbGrayed);
prSetProp(DesignerPanel.SelObjsList,'StartNewPage',CBStartNewPage.State=cbChecked,CBStartNewPage.State=cbGrayed);
prSetProp(DesignerPanel.SelObjsList,'PrintAfterLastBandOnPage',CBPrintAfterLastBandOnPage.State=cbChecked,CBPrintAfterLastBandOnPage.State=cbGrayed);

prSetProp(DesignerPanel.SelObjsList,'ColCount',UDColCount.Position,UDColCount.Position=-1);
prSetProp(DesignerPanel.SelObjsList,'ColDirection',EDColDirection.ItemIndex,EDColDirection.ItemIndex=-1);
prSetProp(DesignerPanel.SelObjsList,'UseVerticalBands',CBUseVerticalBands.State=cbChecked,CBUseVerticalBands.State=cbGrayed);
prSetProp(DesignerPanel.SelObjsList,'UseHorizontalBands',CBUseHorizontalBands.State=cbChecked,CBUseHorizontalBands.State=cbGrayed);
prSetProp(DesignerPanel.SelObjsList,'Visible',CBVisible.State=cbChecked,CBVisible.State=cbGrayed);

prSetProp(DesignerPanel.SelObjsList,'Valid',EDValid.Text,false);

prSetProp(DesignerPanel.SelObjsList, 'DatasetName', prGetListItemString(EDDataSet), (EDDataSet.ItemIndex=-1));
prSetProp(DesignerPanel.SelObjsList, 'SubReportName', prGetListItemString(EDSubReport), EDSubReport.ItemIndex=-1);

prSetProp(DesignerPanel.SelObjsList, 'Group', integer(prGetListItemObject(EDGroup)), (EDGroup.ItemIndex=-1));
prSetProp(DesignerPanel.SelObjsList, 'PrintWithBand', integer(prGetListItemObject(EDPrintWithBand)), (EDPrintWithBand.ItemIndex=-1));
prSetProp(DesignerPanel.SelObjsList, 'DetailBand', integer(prGetListItemObject(EDDetailBand)), (EDDetailBand.ItemIndex=-1));
prSetProp(DesignerPanel.SelObjsList, 'ParentDetail', integer(prGetListItemObject(EDParent)), (EDParent.ItemIndex=-1));
prSetProp(DesignerPanel.SelObjsList, 'PrintWithChildDetail', integer(prGetListItemObject(EDPrintWithChildDetail)), (EDPrintWithChildDetail.ItemIndex=-1));

prSetProp(DesignerPanel.SelObjsList, 'CanSplit', CBCanSplit.State=cbChecked,CBCanSplit.State=cbGrayed);

prSetProp(DesignerPanel.SelObjsList, 'MinDataRecords', udMinDataRecords.Position, False); 
end;

procedure TprBandEditorForm.LBGroupsClick(Sender: TObject);
begin
UpdateButtons;
end;

procedure TprBandEditorForm.LBChildsClick(Sender: TObject);
begin
UpdateButtons;
end;

function TprBandEditorForm.Obj : TprBand;
begin
if DesignerPanel.SelCount>0 then
  Result := TprBand(DesignerPanel.SelObjs[0])
else
  Result := nil;
end;

procedure TprBandEditorForm.bGroupUpClick(Sender: TObject);
var
  i : integer;
begin
if LBGroups.ItemIndex>0 then
  begin
    i := LBGroups.ItemIndex;
    LBGroups.Items.Exchange(i,i-1);
    case Obj.BandType of
      bthDetail:
        TprCustomHDetailBand(Obj).Groups.Exchange(i,i-1);
      btvDetail:
        TprCustomVDetailBand(Obj).Groups.Exchange(i,i-1);
    end;
    LBGroups.ItemIndex := i-1;
    UpdateButtons;
    DesignerPanel.UpdateCurPage;
  end;
end;

procedure TprBandEditorForm.bGroupDownClick(Sender: TObject);
var
  i : integer;
begin
if LBGroups.ItemIndex<LBGroups.Items.Count-1 then
  begin
    i := LBGroups.ItemIndex;
    LBGroups.Items.Exchange(i,i+1);
    case Obj.BandType of
      bthDetail:
        TprCustomHDetailBand(Obj).Groups.Exchange(i,i+1);
      btvDetail:
        TprCustomVDetailBand(Obj).Groups.Exchange(i,i+1);
    end;
    LBGroups.ItemIndex := i+1;
    UpdateButtons;
    DesignerPanel.UpdateCurPage;
  end;
end;

procedure TprBandEditorForm.bChildUpClick(Sender: TObject);
var
  i1,i2 : integer;
begin
if LBChilds.ItemIndex>0 then
  begin
    LBChilds.Items.Exchange(LBChilds.ItemIndex,LBChilds.ItemIndex-1);
    case Obj.BandType of
      bthDetail:
        with TprCustomHDetailBand(Obj) do
          begin
            i1:=Bands.IndexOf(LBChilds.Items.Objects[LBChilds.ItemIndex]);
            i2:=Bands.IndexOf(LBChilds.Items.Objects[LBChilds.ItemIndex+1]);
            Bands.Exchange(i1,i2);
          end;
      btvDetail:
        with TprCustomVDetailBand(Obj) do
          begin
            i1:=Bands.IndexOf(LBChilds.Items.Objects[LBChilds.ItemIndex]);
            i2:=Bands.IndexOf(LBChilds.Items.Objects[LBChilds.ItemIndex+1]);
            Bands.Exchange(i1,i2);
          end;
    end;
    UpdateButtons;
    DesignerPanel.UpdateCurPage;
  end;
end;

procedure TprBandEditorForm.bChildDownClick(Sender: TObject);
var
  i1,i2 : integer;
begin
if LBChilds.ItemIndex<LbChilds.Items.Count-1 then
  begin
    LBChilds.Items.Exchange(LBChilds.ItemIndex,LBChilds.ItemIndex+1);
    case Obj.BandType of
      bthDetail:
        with TprCustomHDetailBand(Obj) do
          begin
            i1:=Bands.IndexOf(LBChilds.Items.Objects[LBChilds.ItemIndex]);
            i2:=Bands.IndexOf(LBChilds.Items.Objects[LBChilds.ItemIndex-1]);
            Bands.Exchange(i1,i2);
          end;
      btvDetail:
        with TprCustomVDetailBand(Obj) do
          begin
            i1:=Bands.IndexOf(LBChilds.Items.Objects[LBChilds.ItemIndex]);
            i2:=Bands.IndexOf(LBChilds.Items.Objects[LBChilds.ItemIndex-1]);
            Bands.Exchange(i1,i2);
          end;
    end;
    UpdateButtons;
    DesignerPanel.UpdateCurPage;
  end;
end;

procedure TprBandEditorForm.Apply;
begin
CopyFromControls;
DesignerPanel.UpdateCurPage;
inherited;
end;

end.
 