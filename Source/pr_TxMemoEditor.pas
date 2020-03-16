{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxMemoEditor;

interface

{$I PR.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls,
  StdCtrls, ExtCtrls, Buttons, CheckLst, vgr_Functions,

  pr_Common, pr_TxClasses, pr_CommonDesignerPanel, pr_TxUtils, pr_MultiLang,
  pr_Strings, pr_OemCharSelectForm;

type
  TprTxMemoEditorForm = class(TprObjPropsForm)
    PC: TPageControl;
    PText: TTabSheet;
    PView: TTabSheet;
    PAlign: TTabSheet;
    RGvAlign: TRadioGroup;
    RGhAlign: TRadioGroup;
    TabSheet1: TTabSheet;
    CBDeleteEmptyLines: TCheckBox;
    CBDeleteEmptyLinesAtEnd: TCheckBox;
    CBCanResizeX: TCheckBox;
    CBCanResizeY: TCheckBox;
    CBWordWrap: TCheckBox;
    Panel1: TPanel;
    bDBFieldName: TSpeedButton;
    Label1: TLabel;
    EDTxFontStyle: TComboBox;
    Label2: TLabel;
    LBTxFontOptions: TCheckListBox;
    bLinesEditor: TSpeedButton;
    prMLRes1: TprMLRes;
    CBDefaultFont: TCheckBox;
    cbJustifyLastLine: TCheckBox;
    cbEolIsEndOfParagraph: TCheckBox;
    PBorders: TTabSheet;
    Label3: TLabel;
    Bevel1: TBevel;
    Label4: TLabel;
    edBrdScheme: TComboBox;
    Label5: TLabel;
    procedure CBDeleteEmptyLinesClick(Sender: TObject);
    procedure SBDBFieldNameClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bLinesEditorClick(Sender: TObject);
    procedure edBrdSchemeClick(Sender: TObject);
  private
    { Private declarations }
    Memo : TprTxOEMMemo;
    FbBrdLeftTop: TprOemCharSelectButton;
    FbBrdTop: TprOemCharSelectButton;
    FbBrdRightTop: TprOemCharSelectButton;
    FbBrdLeft: TprOemCharSelectButton;
    FbBrdLeftBottom: TprOemCharSelectButton;
    FbBrdBottom: TprOemCharSelectButton;
    FbBrdRightBottom: TprOemCharSelectButton;
    FbBrdRight: TprOemCharSelectButton;
  protected
    procedure SetEnabledAfterCopyToControls; override;
    procedure CopySinglePropertiesFromControls(V : TprObjRecVersion); override;
    procedure CopyMultiplyPropertiesFromControls(L : TList); override;
    procedure CopySinglePropertiesToControls(V : TprObjRecVersion); override;
    procedure CopyMultiplyPropertiesToControls(L : TList); override;
  public
    { Public declarations }
  end;

implementation

uses pr_FormatExpression, pr_DesignerFunctions, pr_TxConsts,
  pr_TxMemoLinesEditorForm, pr_TxDesigner;

{$R *.DFM}

/////////////////////////////////////////////////
//
// TprTxMemoEditorForm
//
/////////////////////////////////////////////////
procedure TprTxMemoEditorForm.FormCreate(Sender: TObject);
var
  i : integer;
  ARecodeTable: TprTxRecodeTable;

  procedure UpdateButton(const AName: string; var AButton: TprOemCharSelectButton; X, Y: Integer);
  begin
    AButton := TprOemCharSelectButton.Create(Self);
    with AButton do
    begin
      BorderMode := True;
      Name := AName;
      SetBounds(X, Y, Width, Height);
      Parent := PBorders;
    end;
  end;

begin
  Memo := TprTxOEMMemo.Create(Self);
  with Memo do
  begin
    Report := TprTxReport(DesignerPanel.GetReport);
    Align := alClient;
    Parent := PText;
    ScrollBars := ssBoth;
  end;

  RGvAlign.Items[0] := prLoadStr(sTextAlignTop);
  RGvAlign.Items[1] := prLoadStr(sTextAlignVertCenter);
  RGvAlign.Items[2] := prLoadStr(sTextAlignBottom);
  
  RGhAlign.Items[0] := prLoadStr(sTextAlignLeft);
  RGhAlign.Items[1] := prLoadStr(sTextAlignHorzCenter);
  RGhAlign.Items[2] := prLoadStr(sTextAlignRight);
  RGhAlign.Items[3] := prLoadStr(sTextAlignJustify);
  
  LoadResImage(bDBFieldName.Glyph,'OPEN');
  LoadResImage(bLinesEditor.Glyph,'CHARMAP');
  
  for i:=0 to TxReportOptions.TxFontStylesCount-1 do
    EDTxFontStyle.Items.Add(TxReportOptions.TxFontStyles[i].Description);
  for i:=0 to TxReportOptions.TxFontOptionsCount-1 do
    LBTxFontOptions.Items.Add(TxReportOptions.TxFontOptions[i].Description);

  UpdateButton('FbBrdLeftTop', FbBrdLeftTop, 8, 16);
  UpdateButton('FbBrdTop', FbBrdTop, 64, 16);
  UpdateButton('FbBrdRightTop', FbBrdRightTop, 120, 16);
  UpdateButton('FbBrdRight', FbBrdRight, 120, 48);
  UpdateButton('FbBrdRightBottom', FbBrdRightBottom, 120, 80);
  UpdateButton('FbBrdBottom', FbBrdBottom, 64, 80);
  UpdateButton('FbBrdLeftBottom', FbBrdLeftBottom, 8, 80);
  UpdateButton('FbBrdLeft', FbBrdLeft, 8, 48);

  for I := 0 to TxReportOptions.BorderSchemeCount - 1 do
    edBrdScheme.Items.AddObject(TxReportOptions.BorderScheme[I].Caption, TxReportOptions.BorderScheme[I]);

  ARecodeTable := TprTxReport(DesignerPanel.GetReport).GetUsedRecodeTable;
  if (ARecodeTable <> nil) and (ARecodeTable.BorderSchemeCount > 0) then
  begin
    for I := 0 to ARecodeTable.BorderSchemeCount - 1 do
      edBrdScheme.Items.AddObject(ARecodeTable.BorderScheme[I].Caption, ARecodeTable.BorderScheme[I]);
  end;

  if edBrdScheme.Items.Count <= 0 then
  begin
    Label4.Enabled := False;
    edBrdScheme.Enabled := False;
    Label5.Visible := True;
  end
  else
    Label5.Visible := False;
end;

procedure TprTxMemoEditorForm.CopySinglePropertiesFromControls;
var
  i : integer;
begin
TprTxMemoObjRecVersion(v).Memo.Clear;
for i:=0 to Memo.Lines.Count-1 do
  begin
    TprTxMemoObjRecVersion(v).Memo.Add(Memo.Lines[i]);
    TprTxReport(DesignerPanel.GetReport).OemToWin(PChar(TprTxMemoObjRecVersion(v).Memo[i]),PChar(TprTxMemoObjRecVersion(v).Memo[i]));
  end;
inherited;
end;

procedure TprTxMemoEditorForm.CopyMultiplyPropertiesFromControls;
var
  I, J: Integer;

  procedure CheckBorder(const APropName: string);
  begin
    with TprOemCharSelectButton(FindComponent('Fb' + APropName)) do
    begin
      prSetProp(L, APropName, SelectedChar, not IsCharSelected);
    end;
  end;

begin
  prSetProp(L,'vAlign',RGvAlign.ItemIndex,RGvAlign.ItemIndex=-1);
  prSetProp(L,'hAlign',RGhAlign.ItemIndex,RGhAlign.ItemIndex=-1);

  prSetProp(L,'DefaultFont',CBDefaultFont.State=cbChecked,CBDefaultFont.State=cbGrayed);

  if EDTxFontStyle.ItemIndex<>-1 then
    for i:=0 to L.Count-1 do
      TprTxMemoObjRecVersion(L[i]).TxFontStyleEx := TxReportOptions.TxFontStyles[EDTxFontStyle.ItemIndex];
  for i:=0 to TxReportOptions.TxFontOptionsCount-1 do
    if LBTxFontOptions.State[i]=cbUnchecked then
      for j:=0 to L.Count-1 do
        TprTxMemoObjRecVersion(L[j]).TxFontOptionsEx.Remove(TxReportOptions.TxFontOptions[i])
    else
      if LBTxFontOptions.State[i]=cbChecked then
        for j:=0 to L.Count-1 do
          TprTxMemoObjRecVersion(L[j]).TxFontOptionsEx.Add(TxReportOptions.TxFontOptions[i]);
  
  prSetProp(L,'DeleteEmptyLines',CBDeleteEmptyLines.State=cbChecked,CBDeleteEmptyLines.State=cbGrayed);
  prSetProp(L,'DeleteEmptyLinesAtEnd',CBDeleteEmptyLinesAtEnd.State=cbChecked,CBDeleteEmptyLinesAtEnd.State=cbGrayed);
  prSetProp(L,'CanResizeX',CBCanResizeX.State=cbChecked,CBCanResizeX.State=cbGrayed);
  prSetProp(L,'CanResizeY',CBCanResizeY.State=cbChecked,CBCanResizeY.State=cbGrayed);
  prSetProp(L,'WordWrap',CBWordWrap.State=cbChecked,CBWordWrap.State=cbGrayed);
  prSetProp(L, 'JustifyLastLine', cbJustifyLastLine.State = cbChecked, cbJustifyLastLine.State = cbGrayed);
  prSetProp(L, 'EolIsEndOfParagraph', cbEolIsEndOfParagraph.State = cbChecked, cbEolIsEndOfParagraph.State = cbGrayed);

  CheckBorder('BrdLeft');
  CheckBorder('BrdLeftTop');
  CheckBorder('BrdTop');
  CheckBorder('BrdRightTop');
  CheckBorder('BrdRight');
  CheckBorder('BrdRightBottom');
  CheckBorder('BrdBottom');
  CheckBorder('BrdLeftBottom');

  inherited;
end;

procedure TprTxMemoEditorForm.SetEnabledAfterCopyToControls;
begin
bDBFieldName.Enabled := DesignerPanel.SelCount=1;
bLinesEditor.Enabled := DesignerPanel.SelCount=1;
Memo.Enabled := DesignerPanel.SelCount=1;

if DesignerPanel.SelCount<>1 then
  Memo.Clear;
end;

procedure TprTxMemoEditorForm.CopySinglePropertiesToControls(V : TprObjRecVersion);
var
  s : string;
  i,l : integer;
begin
Memo.Lines.BeginUpdate;
try
  Memo.Lines.Clear;
  for i:=0 to TprTxMemoObjRecVersion(v).Memo.Count-1 do
    begin
      l := Length(TprTxMemoObjRecVersion(v).Memo[i]);
      SetLength(s,l);
      MoveMemory(@(s[1]),@(TprTxMemoObjRecVersion(v).Memo[i][1]),l);
      TprTxReport(DesignerPanel.GetReport).WinToOem(PChar(s),PChar(s));

      Memo.Lines.Add(s);
    end;
finally
  Memo.Lines.EndUpdate;
end;

inherited;
end;

procedure TprTxMemoEditorForm.CopyMultiplyPropertiesToControls;
var
  I, J: Integer;
  flgAnd, flgOr: Boolean;
  AAllBordersEqual: Boolean;

  procedure CheckBorder(const APropName: string);
  var
    AChar: Variant;
  begin
    with TprOemCharSelectButton(FindComponent('Fb' + APropName)) do
    begin
      if prGetProp(L, APropName, AChar) then
        SelectedChar := Char(Integer(AChar))
      else
      begin
        IsCharSelected := False;
        AAllBordersEqual := False;
      end;
    end;
  end;

begin
  RGvAlign.ItemIndex := prGetPropDef(L,'vAlign',-1);
  RGhAlign.ItemIndex := prGetPropDef(L,'hAlign',-1);
  CBDefaultFont.State := prGetPropDefBool(L,'DefaultFont');
  
  EDTxFontStyle.ItemIndex := GetEDFontStyleItemIndex(L);
  for i:=0 to TxReportOptions.TxFontOptionsCount-1 do
  begin
    flgAnd := true;
    flgOr := false;
    for j:=0 to L.Count-1 do
      with TprTxMemoObjRecVersion(L[j]) do
      begin
        flgAnd := flgAnd and TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[i]);
        flgOr := flgOr or TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[i]);
      end;
    if flgAnd then
      LBTxFontOptions.State[i] := cbChecked
    else
      if flgOr then
        LBTxFontOptions.State[i] := cbGrayed
      else
        LBTxFontOptions.State[i] := cbUnchecked;
  end;

  CBDeleteEmptyLines.State := prGetPropDefBool(L,'DeleteEmptyLines');
  CBDeleteEmptyLinesAtEnd.State := prGetPropDefBool(L,'DeleteEmptyLinesAtEnd');
  CBCanResizeX.State := prGetPropDefBool(L,'CanResizeX');
  CBCanResizeY.State := prGetPropDefBool(L,'CanResizeY');
  CBWordWrap.State := prGetPropDefBool(L,'WordWrap');

  cbJustifyLastLine.State := prGetPropDefBool(L, 'JustifyLastLine');
  cbEolIsEndOfParagraph.State := prGetPropDefBool(L, 'EolIsEndOfParagraph');

  AAllBordersEqual := True;
  CheckBorder('BrdLeft');
  CheckBorder('BrdLeftTop');
  CheckBorder('BrdTop');
  CheckBorder('BrdRightTop');
  CheckBorder('BrdRight');
  CheckBorder('BrdRightBottom');
  CheckBorder('BrdBottom');
  CheckBorder('BrdLeftBottom');

  if AAllBordersEqual then
  begin
    J := -1;
    for I := 0 to edBrdScheme.Items.Count - 1 do
      with TprTxBorderScheme(edBrdScheme.Items.Objects[I]) do
        if (BrdLeft = FbBrdLeft.SelectedChar) and
           (BrdLeftTop = FbBrdLeftTop.SelectedChar) and
           (BrdTop = FbBrdTop.SelectedChar) and
           (BrdRightTop = FbBrdRightTop.SelectedChar) and
           (BrdRight = FbBrdRight.SelectedChar) and
           (BrdRightBottom = FbBrdRightBottom.SelectedChar) and
           (BrdBottom = FbBrdBottom.SelectedChar) and
           (BrdLeftBottom = FbBrdLeftBottom.SelectedChar) then
        begin
          J := I;
          break;
        end;
  end
  else
    J := -1;
  edBrdScheme.OnClick := nil;
  edBrdScheme.ItemIndex := J;
  edBrdScheme.OnClick := edBrdSchemeClick;

  CBDeleteEmptyLinesClick(nil);
end;

procedure TprTxMemoEditorForm.CBDeleteEmptyLinesClick(Sender: TObject);
begin
if CBDeleteEmptyLines.Checked then
  begin
    CBDeleteEmptyLinesAtEnd.Checked:=true;
    CBDeleteEmptyLinesAtEnd.Enabled:=false;
  end
else
  begin
    CBDeleteEmptyLinesAtEnd.Enabled:=true;
  end;
end;

procedure TprTxMemoEditorForm.SBDBFieldNameClick(Sender: TObject);
var
  s : string;
begin
TprFormatExpressionForm.Create(Application).SelectExpression(DesignerPanel.GetReport,Memo,s);
end;

procedure TprTxMemoEditorForm.bLinesEditorClick(Sender: TObject);
begin
TprTxMemoLinesEditorForm.Create(Application).EditLines(TprTxReport(DesignerPanel.GetReport),Memo.Lines);
end;

procedure TprTxMemoEditorForm.edBrdSchemeClick(Sender: TObject);
begin
  if edBrdScheme.ItemIndex < 0 then
    exit;

  with TprTxBorderScheme(edBrdScheme.Items.Objects[edBrdScheme.ItemIndex]) do
  begin
    FbBrdLeft.SelectedChar := BrdLeft;
    FbBrdLeftTop.SelectedChar := BrdLeftTop;
    FbBrdTop.SelectedChar := BrdTop;
    FbBrdRightTop.SelectedChar := BrdRightTop;
    FbBrdRight.SelectedChar := BrdRight;
    FbBrdRightBottom.SelectedChar := BrdRightBottom;
    FbBrdBottom.SelectedChar := BrdBottom;
    FbBrdLeftBottom.SelectedChar := BrdLeftBottom;
  end;
end;

end.

