{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_VersionsEditor;

interface

uses
  SysUtils, Windows, Classes, Graphics, Controls, StdCtrls, extctrls, buttons,
  Math, menus,

  pr_Common, pr_MultiLang;

const
  LeftOffs  = 4;
  RightOffs = 4;
type

TOnVersionChanged = procedure (Sender : TObject; OldVersion,NewVersion : TprObjRecVersion) of object;

/////////////////////////////
//
// TprObjVersionsEditor
//
/////////////////////////////
TprObjVersionsEditor = class(TCustomPanel)
private
  FObj : TprObj;
  FCurrentVersion : TprObjRecVersion;

  FOnVersionChanged : TOnVersionChanged;

  procedure SetCurrentVersion(Value : TprObjRecVersion);
  procedure SetObj(Value : TprObj);
protected
  EDObjVersions  : TComboBox;
  bNewVersion    : TSpeedButton;
  bDelVersion    : TSpeedButton;
  bMoveUp        : TSpeedButton;
  bMoveDown      : TSpeedButton;
  bSetAsDefault  : TSpeedButton;
  bFormulaEditor : TSpeedButton;
  bOptions : TSpeedButton;
  PMOptions : TPopupMenu;
  PMIVisible : TMenuItem;
  PMIWidth : TMenuItem;
  PMIHeight : TMenuItem;
  PMCanSplit: TMenuItem;

  procedure InitControls;
  procedure UpdateButtons;

  procedure OnNewVersion(Sender : TObject);
  procedure OnDelVersion(Sender : TObject);
  procedure OnMoveUp(Sender : TObject);
  procedure OnMoveDown(Sender : TObject);
  procedure OnSetAsDefault(Sender : TObject);
  procedure OnFormulaEditor(Sender : TObject);
  procedure OnVisibleClick(Sender : TObject);
  procedure OnWidthClick(Sender : TObject);
  procedure OnHeightClick(Sender : TObject);
  procedure OnCanSplitClick(Sender: TObject);
  procedure OnOptionsClick(Sender : TObject);

  procedure EDObjVersionsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  procedure EDObjVersionsClick(Sender : TObject);

  procedure Resize; override;
public
  property CurrentVersion : TprObjRecVersion read FCurrentVersion write SetCurrentVersion;
  property Obj            : TprObj read FObj write SetObj;

  constructor Create(AOwner : TComponent); override;
published
  property Align;
  property Anchors;
  property BevelInner;
  property BevelOuter;
  property BevelWidth;
  property BiDiMode;
  property BorderWidth;
  property BorderStyle;
  property Color;
  property Constraints;
  property Ctl3D;
  property UseDockManager default True;
  property DockSite;
  property DragCursor;
  property DragKind;
  property DragMode;
  property Enabled;
  property ParentBiDiMode;
  property ParentColor;
  property ParentCtl3D;
  property ParentFont;
  property ParentShowHint;
  property PopupMenu;
  property ShowHint;
  property TabOrder;
  property TabStop;
  property Visible;

  property OnCanResize;
  property OnClick;
  property OnConstrainedResize;
  property OnDockDrop;
  property OnDockOver;
  property OnDblClick;
  property OnDragDrop;
  property OnDragOver;
  property OnEndDock;
  property OnEndDrag;
  property OnEnter;
  property OnExit;
  property OnGetSiteInfo;
  property OnMouseDown;
  property OnMouseMove;
  property OnMouseUp;
  property OnResize;
  property OnStartDock;
  property OnStartDrag;
  property OnUnDock;
  
  property OnVersionChanged : TOnVersionChanged read FOnVersionChanged write FOnVersionChanged;
end;

implementation

uses
  pr_FormatExpression, pr_Strings;

type
  TprObjRecAccess = class(TprObjRec)
  end;

/////////////////////////////
//
// TprObjVersionsEditor
//
/////////////////////////////
constructor TprObjVersionsEditor.Create;
begin
inherited;
BevelInner := bvNone;
BevelOuter := bvNone;
FObj := nil;
FCurrentVersion := nil;
Caption := '';
InitControls;
end;

procedure TprObjVersionsEditor.SetCurrentVersion;
begin
if Assigned(OnVersionChanged) then
  OnVersionChanged(Self,FCurrentVersion,Value);

FCurrentVersion:=Value;
end;

procedure TprObjVersionsEditor.SetObj;
var
  i : integer;
begin
if FObj=Value then exit;

FObj:=Value;

EDObjVersions.Items.Clear;
if FObj<>nil then
  begin
    for i:=0 to FObj.dRec.Versions.Count-1 do
      EDObjVersions.Items.Add(' ');

    EDObjVersions.ItemIndex:=0;
    FCurrentVersion        :=FObj.dRec.Versions[0];
  end
else
  FCurrentVersion:=nil;

UpdateButtons;
end;

procedure TprObjVersionsEditor.InitControls;

  procedure CreateButton(var b : TSpeedButton; ResID : string; _OnClick : TNotifyEvent; HintResID : integer);
  begin
  b := TSpeedButton.Create(Self);
  with b do
    begin
      Top := 2;
      Parent := Self;
      OnClick := _OnClick;
      Hint := prLoadStr(HintResID);
      ShowHint := true;
      LoadResImage(Glyph,ResID);
    end;
  end;

begin
EDObjVersions := TComboBox.Create(Self);
with EDObjVersions do
  begin
    Style := csOwnerDrawFixed;
    Left := LeftOffs;
    Parent := Self;
    OnDrawItem := EDObjVersionsDrawItem;
    OnClick := EDObjVersionsClick;
  end;

PMOptions := TPopupMenu.Create(Self);
PMIVisible := TMenuItem.Create(Self);
PMIVisible.Caption := prLoadStr(sVisible);
PMIVisible.OnClick := OnVisibleClick;
PMOptions.Items.Add(PMIVisible);

PMIWidth := TMenuItem.Create(Self);
PMIWidth.Caption := prLoadStr(sWidthAsVerticalBand);
PMIWidth.OnClick := OnWidthClick;
PMOptions.Items.Add(PMIWidth);

PMIHeight := TMenuItem.Create(Self);
PMIHeight.Caption := prLoadStr(sHeightAsHorizontalBand);
PMIHeight.OnClick := OnHeightClick;
PMOptions.Items.Add(PMIHeight);

PMCanSplit := TMenuItem.Create(Self);
PMCanSplit.Caption := prLoadStr(sObjCanSplit);
PMCanSplit.OnClick := OnCanSplitClick;
PMOptions.Items.Add(PMCanSplit);

bOptions := TSpeedButton.Create(Self);
with bOptions do
  begin
    Top := 2;
    Parent := Self;
    OnClick := OnOptionsClick;
    Caption := prLoadStr(sObjOptions);
    ShowHint := true;
    Width := 60;
  end;

CreateButton(bNewVersion,'NEWVERSION',OnNewVersion,sVersionsEditorNewVersion);
CreateButton(bDelVersion,'DELVERSION',OnDelVersion,sVersionsEditorDelVersion);
CreateButton(bMoveUp,'MOVEUP',OnMoveUp,sVersionsEditorMoveUp);
CreateButton(bMoveDown,'MOVEDOWN',OnMoveDown,sVersionsEditorMoveDown);
CreateButton(bSetAsDefault,'SETASDEFAULT',OnSetAsDefault,sVersionsEditorSetAsDefault);
CreateButton(bFormulaEditor,'FORMULAEDITOR',OnFormulaEditor,sVersionsEditorEditFormula);

Height := Max(EDObjVersions.Height,bNewVersion.Height)+3;

UpdateButtons;
end;

procedure TprObjVersionsEditor.UpdateButtons;
begin
  EDObjVersions.Enabled :=FObj<>nil;
  bNewVersion.Enabled   :=FObj<>nil;
  bDelVersion.Enabled   :=(FObj<>nil) and (FObj.dRec.Versions.Count>1);
  bMoveUp.Enabled       :=(FObj<>nil) and (EDObjVersions.ItemIndex>0);
  bMoveDown.Enabled     :=(FObj<>nil) and (EDObjVersions.ItemIndex<EDObjVersions.Items.Count-1);
  bSetAsDefault.Enabled :=(FObj<>nil) and (FObj.dRec.DefVersion<>EDObjVersions.ItemIndex);
  bFormulaEditor.Enabled:=FObj<>nil;
  bOptions.Enabled := FObj<>nil;
  PMIVisible.Checked := bOptions.Enabled and (CurrentVersion.Visible);
  PMIWidth.Checked := bOptions.Enabled and (FObj.dRec.WidthAsVerticalBand);
  PMIHeight.Checked := bOptions.Enabled and (FObj.dRec.HeightAsHorizontalBand);
  PMCanSplit.Enabled := bOptions.Enabled and TprObjRecAccess(FObj.dRec).GetSupportSplitting;
  PMCanSplit.Checked := bOptions.Enabled and TprObjRecAccess(FObj.dRec).CanSplit;
end;

procedure TprObjVersionsEditor.OnNewVersion;
var
  v : TprObjRecVersion;
begin
v:=TprObjRecVersion(FObj.dRec.Versions.Add);
v.Assign(FCurrentVersion);
v.Formula:='';

EDObjVersions.Items.Add(' ');
EDObjVersions.ItemIndex:=EDObjVersions.Items.Count-1;

CurrentVersion:=v;
UpdateButtons;
end;

procedure TprObjVersionsEditor.OnDelVersion;
begin
if FObj.dRec.Versions.Count>1 then
  begin
    if FObj.dRec.DefVersion=FObj.dRec.Versions.Count-1 then
      FObj.dRec.DefVersion:=FObj.dRec.Versions.Count-2;

    FObj.dRec.Versions[EDObjVersions.ItemIndex].Free;
    EDObjVersions.Items.Delete(0);

    if EDObjVersions.ItemIndex=-1 then
      EDObjVersions.ItemIndex:=0;
    FCurrentVersion:=nil;
    CurrentVersion :=FObj.dRec.Versions[EDObjVersions.ItemIndex];

    UpdateButtons;
    EDObjVersions.Repaint;
  end;
end;

procedure TprObjVersionsEditor.OnMoveUp;
begin
if EDObjVersions.ItemIndex>0 then
  begin
    FObj.dRec.Versions[EDObjVersions.ItemIndex].Index:=EDObjVersions.ItemIndex-1;
    if FObj.dRec.DefVersion=EDObjVersions.ItemIndex then
      FObj.dRec.DefVersion:=FObj.dRec.DefVersion-1;

    EDObjVersions.ItemIndex:=EDObjVersions.ItemIndex-1;
    UpdateButtons;
    EDObjVersions.Repaint;
  end;
end;

procedure TprObjVersionsEditor.OnMoveDown;
begin
if EDObjVersions.ItemIndex<FObj.dRec.Versions.Count-1 then
  begin
    FObj.dRec.Versions[EDObjVersions.ItemIndex].Index:=EDObjVersions.ItemIndex+1;
    if FObj.dRec.DefVersion=EDObjVersions.ItemIndex then
      FObj.dRec.DefVersion:=FObj.dRec.DefVersion+1;

    EDObjVersions.ItemIndex:=EDObjVersions.ItemIndex+1;
    UpdateButtons;
    EDObjVersions.Repaint;
  end;
end;

procedure TprObjVersionsEditor.OnSetAsDefault;
begin
FObj.dRec.DefVersion:=EDObjVersions.ItemIndex;
EDObjVersions.Repaint;
end;

procedure TprObjVersionsEditor.OnFormulaEditor;
var
  s : string;
begin
s:=FObj.dRec.Versions[EDObjVersions.ItemIndex].Formula;
TprFormatExpressionForm.Create(nil).SelectExpression(FObj.Band.Report,nil,s,false);
FObj.dRec.Versions[EDObjVersions.ItemIndex].Formula:=s;
EDObjVersions.Repaint;
end;

procedure TprObjVersionsEditor.OnVisibleClick;
begin
PMIVisible.Checked := not PMIVisible.Checked;
if CurrentVersion<>nil then
  CurrentVersion.Visible := PMIVisible.Checked;
end;

procedure TprObjVersionsEditor.OnWidthClick;
begin
PMIWidth.Checked := not PMIWidth.Checked;
if Obj<>nil then
  Obj.dRec.WidthAsVerticalBand := PMIWidth.Checked;
end;

procedure TprObjVersionsEditor.OnHeightClick;
begin
PMIHeight.Checked := not PMIHeight.Checked;
if Obj<>nil then
  Obj.dRec.HeightAsHorizontalBand := PMIHeight.Checked;
end;

procedure TprObjVersionsEditor.OnCanSplitClick(Sender: TObject);
begin
  PMCanSplit.Checked := not PMCanSplit.Checked;
  if Obj <> nil then
    TprObjRecAccess(Obj.dRec).CanSplit := PMCanSplit.Checked;
end;

procedure TprObjVersionsEditor.OnOptionsClick;
var
  p : TPoint;
begin
p := ClientToScreen(Point(bOptions.Left,bOptions.Top+bOptions.Height));
PMOptions.Popup(p.x,p.y);
end;

procedure TprObjVersionsEditor.Resize;
  procedure SetTop(c : TControl);
  begin
  c.Top:=(ClientHeight-c.Height) div 2;
  end;
begin
inherited;
bOptions.Top := (ClientHeight-bOptions.Height) div 2;
bOptions.Left :=ClientWidth-70;
bFormulaEditor.Left:=bOptions.Left-bFormulaEditor.Width-RightOffs;
bSetAsDefault.Left :=bFormulaEditor.Left-bSetAsDefault.Width-4;
bMoveDown.Left     :=bSetAsDefault.Left-bMoveDown.Width-4;
bMoveUp.Left       :=bMoveDown.Left-bMoveUp.Width-1;
bDelVersion.Left   :=bMoveUp.Left-bDelVersion.Width-4;
bNewVersion.Left   :=bDelVersion.Left-bNewVersion.Width-1;
EDObjVersions.Width:=bNewVersion.Left-EDObjVersions.Left-4;

SetTop(bFormulaEditor);
SetTop(bSetAsDefault);
SetTop(bMoveDown);
SetTop(bMoveUp);
SetTop(bDelVersion);
SetTop(bNewVersion);
SetTop(EDObjVersions);
end;

procedure TprObjVersionsEditor.EDObjVersionsDrawItem;
var
  s : string;
begin
if Index=FObj.dRec.DefVersion then
  EDObjVersions.Canvas.Font.Style:=[fsBold]
else
  EDObjVersions.Canvas.Font.Style:=[];

EDObjVersions.Canvas.FillRect(Rect);
if Index>=0 then
  begin
    if FObj.dRec.Versions[Index].Formula<>'' then
      s:=Format('%4d.  ',[Index+1])+FObj.dRec.Versions[Index].Formula
    else
      s:=Format('%4d.  ',[Index+1])+prLoadStr(sFormulaNotDefined);
    EDObjVersions.Canvas.TextOut(Rect.Left,Rect.Top+(Rect.Bottom-Rect.Top-EDObjVersions.Canvas.TextHeight(s)) div 2,s);
  end;
end;

procedure TprObjVersionsEditor.EDObjVersionsClick;
begin
if EDObjVersions.ItemIndex>=0 then
  begin
    CurrentVersion:=FObj.dRec.Versions[EDObjVersions.ItemIndex];
  end;
UpdateButtons;
end;

end.

