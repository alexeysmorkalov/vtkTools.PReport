{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

unit pr_PrvMemoEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Buttons,

  vgr_ColorButton,

  pr_Classes, pr_PreviewPanel, pr_MultiLang, vgr_Button;

{$I pr.inc}

type
  TprPrvMemoEditorForm = class(TprPreviewPropsForm)
    PC: TPageControl;
    PText: TTabSheet;
    Memo: TMemo;
    PColors: TTabSheet;
    Label3: TLabel;
    BFont: TSpeedButton;
    Label1: TLabel;
    STFont: TStaticText;
    CBRotate90: TCheckBox;
    PAlign: TTabSheet;
    RGvAlign: TRadioGroup;
    RGhAlign: TRadioGroup;
    PBorders: TTabSheet;
    EDLeftStyle: TComboBox;
    CBLeftShow: TCheckBox;
    CBTopShow: TCheckBox;
    CBRightShow: TCheckBox;
    CBBottomShow: TCheckBox;
    EDBottomStyle: TComboBox;
    EDRightStyle: TComboBox;
    EDTopStyle: TComboBox;
    EDBottomWidth: TEdit;
    EDRightWidth: TEdit;
    EDTopWidth: TEdit;
    EDLeftWidth: TEdit;
    UDLeftWidth: TUpDown;
    UDTopWidth: TUpDown;
    UDRightWidth: TUpDown;
    UDBottomWidth: TUpDown;
    FontDialog: TFontDialog;
    prMLRes1: TprMLRes;
    bFillColor: TvgrColorButton;
    bFontColor: TvgrColorButton;
    bLeftColor: TvgrColorButton;
    bTopColor: TvgrColorButton;
    bRightColor: TvgrColorButton;
    bBottomColor: TvgrColorButton;
    TabSheet1: TTabSheet;
    CBDeleteEmptyLines: TCheckBox;
    CBDeleteEmptyLinesAtEnd: TCheckBox;
    CBWordWrap: TCheckBox;
    cbJustifyLastLine: TCheckBox;
    cbEolIsEndOfParagraph: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BFontClick(Sender: TObject);
    procedure EDLeftStyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure bFontColorColorChange(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateFont;
  protected
    procedure CopyPropertiesFromControls(l : TList); override;
    procedure CopyPropertiesToControls(l : TList); override;
  public
    { Public declarations }
  end;

implementation

uses
  pr_DesignerFunctions, pr_Strings;

{$R *.DFM}

procedure TprPrvMemoEditorForm.UpdateFont;
begin
bFontColor.SelectedColor := FontDialog.Font.Color;
STFont.Caption:=Format('%s, %d',[FontDialog.Font.Name,FontDialog.Font.Size]);
STFont.Font.Assign(FontDialog.Font);
end;

procedure TprPrvMemoEditorForm.CopyPropertiesFromControls;
begin
if l.Count=1 then
  begin
    TprMemoObjRecVersion(l[0]).Memo.Assign(Memo.Lines);
    TprMemoObjRecVersion(l[0]).Font.Assign(STFont.Font);
  end;
prSetProp(L,'FillColor',bFillColor.SelectedColor,bFillColor.SelectedColor=clDefault);
prSetProp(L,'Font.Color',bFontColor.SelectedColor,bFontColor.SelectedColor=clDefault);
prSetProp(L,'Rotate90',CBRotate90.State=cbChecked,CBRotate90.State=cbGrayed);
prSetProp(L,'vAlign',RGvAlign.ItemIndex,RGvAlign.ItemIndex=-1);
prSetProp(L,'hAlign',RGhAlign.ItemIndex,RGhAlign.ItemIndex=-1);

  prSetProp(L, 'DeleteEmptyLines', CBDeleteEmptyLines.State=cbChecked, CBDeleteEmptyLines.State=cbGrayed);
  prSetProp(L, 'DeleteEmptyLinesAtEnd', CBDeleteEmptyLinesAtEnd.State=cbChecked, CBDeleteEmptyLinesAtEnd.State=cbGrayed);
  prSetProp(L, 'WordWrap', CBWordWrap.State=cbChecked, CBWordWrap.State=cbGrayed);
  prSetProp(L, 'JustifyLastLine', cbJustifyLastLine.State = cbChecked, cbJustifyLastLine.State = cbGrayed);
  prSetProp(L, 'EolIsEndOfParagraph', cbEolIsEndOfParagraph.State = cbChecked, cbEolIsEndOfParagraph.State = cbGrayed);

GetFrameLine(L,CBLeftShow,EDLeftStyle,UDLeftWidth,bLeftColor,'l');
GetFrameLine(L,CBTopShow,EDTopStyle,UDTopWidth,bTopColor,'t');
GetFrameLine(L,CBRightShow,EDRightStyle,UDRightWidth,bRightColor,'r');
GetFrameLine(L,CBBottomShow,EDBottomStyle,UDBottomWidth,bBottomColor,'b');
end;

procedure TprPrvMemoEditorForm.CopyPropertiesToControls;
begin
if l.Count=1 then
  begin
    Memo.Lines.Assign(TprMemoObjRecVersion(l[0]).Memo);
    FontDialog.Font.Assign(TprMemoObjRecVersion(l[0]).Font);
    UpdateFont;
    Memo.Enabled := true;
  end
else
  begin
    Memo.Clear;
    Memo.Enabled := false;
  end;
bFillColor.SelectedColor := prGetPropDef(L,'FillColor',clDefault);
bFontColor.SelectedColor := prGetPropDef(L,'Font.Color',clDefault);
CBRotate90.State := prGetPropDefBool(L,'Rotate90');
RGvAlign.ItemIndex := prGetPropDef(L,'vAlign',-1);
RGhAlign.ItemIndex := prGetPropDef(L,'hAlign',-1);
SetFrameLine(L,CBLeftShow,EDLeftStyle,UDLeftWidth,bLeftColor,'l');
SetFrameLine(L,CBTopShow,EDTopStyle,UDTopWidth,bTopColor,'t');
SetFrameLine(L,CBRightShow,EDRightStyle,UDRightWidth,bRightColor,'r');
SetFrameLine(L,CBBottomShow,EDBottomStyle,UDBottomWidth,bBottomColor,'b');

  CBDeleteEmptyLines.State := prGetPropDefBool(L,'DeleteEmptyLines');
  CBDeleteEmptyLinesAtEnd.State := prGetPropDefBool(L,'DeleteEmptyLinesAtEnd');
  CBWordWrap.State := prGetPropDefBool(L,'WordWrap');
  cbJustifyLastLine.State := prGetPropDefBool(L, 'JustifyLastLine');
  cbEolIsEndOfParagraph.State := prGetPropDefBool(L, 'EolIsEndOfParagraph');
end;

procedure TprPrvMemoEditorForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
RGvAlign.Items[0]:=prLoadStr(sTextAlignTop);
RGvAlign.Items[1]:=prLoadStr(sTextAlignVertCenter);
RGvAlign.Items[2]:=prLoadStr(sTextAlignBottom);

RGhAlign.Items[0]:=prLoadStr(sTextAlignLeft);
RGhAlign.Items[1]:=prLoadStr(sTextAlignHorzCenter);
RGhAlign.Items[2]:=prLoadStr(sTextAlignRight);
RGhAlign.Items[3]:=prLoadStr(sTextAlignJustify);

prInitColorButtons([bFillColor,bFontColor,bLeftColor,bTopColor,bRightColor,bBottomColor]);

for i:=0 to integer(High(TPenStyle))-2 do
  begin
    EDLeftStyle.Items.Add(' ');
    EDTopStyle.Items.Add(' ');
    EDRightStyle.Items.Add(' ');
    EDBottomStyle.Items.Add(' ');
  end;
end;

procedure TprPrvMemoEditorForm.BFontClick(Sender: TObject);
begin
if FontDialog.Execute then
  UpdateFont;
end;

procedure TprPrvMemoEditorForm.EDLeftStyleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  LineWidth = 4;
var
  i : integer;
begin
with TComboBox(Control).Canvas do
  begin
    Pen.Color  :=clBlack;
    Pen.Style  :=TPenStyle(index);
    Brush.Color:=clWindow;
    FillRect(Rect);

    i:=Rect.Top+(Rect.Bottom-Rect.Top-LineWidth) div 2;
    for i:=i to i+LineWidth-1 do
      begin
        MoveTo(Rect.Left+3,i);
        LineTo(Rect.Right-3,i);
      end;
  end;
end;

procedure TprPrvMemoEditorForm.bFontColorColorChange(Sender: TObject);
begin
FontDialog.Font.Color := bFontColor.SelectedColor;
UpdateFont;
end;

end.
