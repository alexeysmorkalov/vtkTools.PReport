{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_Link;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls,

  pr_Utils, pr_Common, pr_MultiLang, pr_CommonDesignerPanel;

type
  TprObjLinksForm = class(TprCustomObjLinksForm)
    PC: TPageControl;
    PLeft: TTabSheet;
    PTop: TTabSheet;
    PWidth: TTabSheet;
    PHeight: TTabSheet;
    LBLeft: TListBox;
    LBTop: TListBox;
    LBWidth: TListBox;
    LBHeight: TListBox;
    Label1: TLabel;
    CBLeftMode: TComboBox;
    Label2: TLabel;
    CBTopMode: TComboBox;
    Label3: TLabel;
    CBWidthMode: TComboBox;
    Label4: TLabel;
    CBHeightMode: TComboBox;
    bDelLink: TSpeedButton;
    prMLRes1: TprMLRes;
    PSize: TTabSheet;
    LBResize: TListBox;
    Label5: TLabel;
    CBResizeMode: TComboBox;
    procedure bDelLinkClick(Sender: TObject);
    procedure CBLeftModeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function GetObj : TprObj;
    function GetBand : TprBand;
    function GetSelObj : TprDesignComponent;
  public
    { Public declarations }
    property Obj: TprObj read GetObj;
    property Band: TprBand read GetBand;
    property SelObj: TprDesignComponent read GetSelObj;
    procedure UpdateInfo; override;
  end;

implementation

uses
  pr_Strings;

{$R *.DFM}

function TprObjLinksForm.GetSelObj : TprDesignComponent;
begin
if DesignerPanel.SelCount=1 then
  Result := DesignerPanel.SelObjs[0]
else
  Result := nil;
end;

function TprObjLinksForm.GetObj : TprObj;
begin
if (DesignerPanel.SelCount=1) and (DesignerPanel.SelObjs[0] is TprObj) then
  Result := TprObj(DesignerPanel.SelObjs[0])
else
  Result := nil;
end;

function TprObjLinksForm.GetBand : TprBand;
begin
if (DesignerPanel.SelCount=1) and (DesignerPanel.SelObjs[0] is TprBand) then
  Result := TprBand(DesignerPanel.SelObjs[0])
else
  Result := nil;
end;

procedure TprObjLinksForm.UpdateInfo;

  procedure AddList(o : TprObjs; LB : TListBox);
  var
    i : integer;
  begin
  LB.Items.BeginUpdate;
  LB.Clear;
  for i:=0 to o.Count-1 do
    LB.Items.AddObject(o[i].GetDesc,o[i]);
  LB.Items.EndUpdate;
  if LB.Items.Count>0 then
    LB.ItemIndex := 0;
  end;

begin
if SelObj=nil then
  begin
    Caption := prLoadStr(sNoObjSelected);
    PC.Visible := false;
    bDelLink.Enabled := false;
  end
else
  if SelObj is TprObj then
    begin
      PLeft.TabVisible := true;
      PTop.TabVisible := true;
      PWidth.TabVisible := true;
      PHeight.TabVisible := true;
      PSize.TabVisible := false;
      Caption := Obj.GetDesc;
      PC.Visible := true;
      bDelLink.Enabled := true;

      AddList(Obj.LeftObjs,LBLeft);
      CBLeftMode.ItemIndex := integer(Obj.LeftMode);

      AddList(Obj.TopObjs,LBTop);
      CBTopMode.ItemIndex := integer(Obj.TopMode);

      AddList(Obj.WidthObjs,LBWidth);
      CBWidthMode.ItemIndex := integer(Obj.WidthMode);

      AddList(Obj.HeightObjs,LBHeight);
      CBHeightMode.ItemIndex := integer(Obj.HeightMode);

      PC.ActivePage := PLeft;
    end
  else
    if SelObj is TprBand then
      begin
        PLeft.TabVisible := false;
        PTop.TabVisible := false;
        PWidth.TabVisible := false;
        PHeight.TabVisible := false;
        PSize.TabVisible := true;
        Caption := Band.GetDrawDesignCaption;
        PC.Visible := true;
        bDelLink.Enabled := true;

        AddList(Band.ResizeObjs,LBResize);
        CBResizeMode.ItemIndex := integer(Band.ResizeMode);

        PC.ActivePage := PSize;
      end;
end;

procedure TprObjLinksForm.bDelLinkClick(Sender: TObject);
var
  LB : TListBox;
begin
if SelObj=nil then exit;
if PC.ActivePage=PLeft then
  LB := LBLeft
else
  if PC.ActivePage=PTop then
    LB := LBTop
  else
    if PC.ActivePage=PWidth then
      LB := LBWidth
    else
      if PC.ActivePage=PHeight then
        LB := LBHeight
      else
        if PC.ActivePage=PSize then
          LB := LBResize
        else exit;
if LB.ItemIndex<0 then
  MBError(prLoadStr(sLinkNotSelected))
else
  begin
    if SelObj is TprObj then
      DesignerPanel.DeleteObjLink(Obj,TprLinkType(PC.ActivePage.TabIndex),TprObj(LB.Items.Objects[LB.ItemIndex]))
    else
      if SelObj is TprBand then
        DesignerPanel.DeleteBandLink(Band,TprObj(LBResize.Items.Objects[LBResize.ItemIndex]));
  end;
end;

procedure TprObjLinksForm.CBLeftModeChange(Sender: TObject);
begin
if SelObj=nil then exit;
if Sender=CBLeftMode then
  Obj.LeftMode := TprHLinkMode(CBLeftMode.ItemIndex)
else
  if Sender=CBTopMode then
    Obj.TopMode := TprVLinkMode(CBTopMode.ItemIndex)
  else
    if Sender=CBWidthMode then
      Obj.WidthMode := TprHResizeMode(CBWidthMode.ItemIndex)
    else
      if Sender=CBHeightMode then
        Obj.HeightMode := TprVResizeMode(CBHeightMode.ItemIndex)
      else
        if Sender=CBResizeMode then
          Band.ResizeMode := TprBandResizeMode(CBResizeMode.ItemIndex)
        else
          exit;
DesignerPanel.DsgnNotifyReport(true);
end;

procedure TprObjLinksForm.FormCreate(Sender: TObject);
begin
LoadResImage(bDelLink.Glyph,'DELVERSION');

CBLeftMode.Items.Add(prLoadStr(sLinkLeftDependFromMaxRight));
CBLeftMode.Items.Add(prLoadStr(sLinkLeftDependFromMinRight));

CBTopMode.Items.Add(prLoadStr(sLinkTopDependFromMaxBottom));
CBTopMode.Items.Add(prLoadStr(sLinkTopDependFromMinBottom));

CBWidthMode.Items.Add(prLoadStr(sLinkWidthDependFromMaxRight));
CBWidthMode.Items.Add(prLoadStr(sLinkWidthDependFromMinRight));
CBWidthMode.Items.Add(prLoadStr(sLinkWidthDependFromMaxWidth));
CBWidthMode.Items.Add(prLoadStr(sLinkWidthDependFromMinWidth));
CBWidthMode.Items.Add(prLoadStr(sLinkWidthDependFromSumWidth));

CBHeightMode.Items.Add(prLoadStr(sLinkHeightDependFromMaxBottom));
CBHeightMode.Items.Add(prLoadStr(sLinkHeightDependFromMinBottom));
CBHeightMode.Items.Add(prLoadStr(sLinkWidthDependFromMaxHeight));
CBHeightMode.Items.Add(prLoadStr(sLinkWidthDependFromMinHeight));
CBHeightMode.Items.Add(prLoadStr(sLinkWidthDependFromSumHeight));

CBResizeMode.Items.Add(prLoadStr(sBandResizeNoChange));
CBResizeMode.Items.Add(prLoadStr(sBandResizeMaxBottom));
CBResizeMode.Items.Add(prLoadStr(sBandResizeMaxBottomInLinksList));
CBResizeMode.Items.Add(prLoadStr(sBandResizeMinBottomInLinksList));
end;

end.

