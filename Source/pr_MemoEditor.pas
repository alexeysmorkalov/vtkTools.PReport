{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

unit pr_MemoEditor;

interface

{$I pr.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons, Math,

  vgr_ColorButton,
  
  pr_Common, pr_Classes, pr_CommonDesignerPanel, pr_MultiLang, vgr_Button;

type
  TprMemoEditorForm = class(TprObjPropsForm)
    FontDialog: TFontDialog;
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
    TabSheet1: TTabSheet;
    CBDeleteEmptyLines: TCheckBox;
    CBDeleteEmptyLinesAtEnd: TCheckBox;
    CBCanResizeX: TCheckBox;
    CBCanResizeY: TCheckBox;
    CBWordWrap: TCheckBox;
    Panel1: TPanel;
    bDBFieldName: TSpeedButton;
    prMLRes1: TprMLRes;
    bFillColor: TvgrColorButton;
    bFontColor: TvgrColorButton;
    bLeftColor: TvgrColorButton;
    bTopColor: TvgrColorButton;
    bRightColor: TvgrColorButton;
    bBottomColor: TvgrColorButton;
    cbJustifyLastLine: TCheckBox;
    cbEolIsEndOfParagraph: TCheckBox;
    procedure BFontClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EDLeftStyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure bDBFieldNameClick(Sender: TObject);
    procedure CBDeleteEmptyLinesClick(Sender: TObject);
    procedure bFontColorColorChange(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateFont;
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

uses
  pr_Strings, pr_FormatExpression, pr_DesignerFunctions;

{$R *.DFM}

procedure TprMemoEditorForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
Memo.Font.Charset := DEFAULT_CHARSET;
LoadResImage(bDBFieldName.Glyph,'OPEN');

RGvAlign.Items[0]:=prLoadStr(sTextAlignTop);
RGvAlign.Items[1]:=prLoadStr(sTextAlignVertCenter);
RGvAlign.Items[2]:=prLoadStr(sTextAlignBottom);

RGhAlign.Items[0]:=prLoadStr(sTextAlignLeft);
RGhAlign.Items[1]:=prLoadStr(sTextAlignHorzCenter);
RGhAlign.Items[2]:=prLoadStr(sTextAlignRight);
RGhAlign.Items[3] := prLoadStr(sTextAlignJustify);

prInitColorButtons([bFillColor,bFontColor,bLeftColor,bTopColor,bRightColor,bBottomColor]);

for i:=0 to integer(High(TPenStyle))-2 do
  begin
    EDLeftStyle.Items.Add(' '); // !!! not empty string to avoid Delphi7 bug
    EDTopStyle.Items.Add(' ');
    EDRightStyle.Items.Add(' ');
    EDBottomStyle.Items.Add(' ');
  end;
end;

procedure TprMemoEditorForm.CopySinglePropertiesFromControls;
begin
TprMemoObjRecVersion(v).Memo.Assign(Memo.Lines);
TprMemoObjRecVersion(v).Font.Assign(STFont.Font);
inherited;
end;

procedure TprMemoEditorForm.CopyMultiplyPropertiesFromControls;
begin
prSetProp(L,'FillColor',bFillColor.SelectedColor,bFillColor.SelectedColor=clDefault);
prSetProp(L,'Font.Color',bFontColor.SelectedColor,bFontColor.SelectedColor=clDefault);
prSetProp(L,'Rotate90',CBRotate90.State=cbChecked,CBRotate90.State=cbGrayed);
prSetProp(L,'vAlign',RGvAlign.ItemIndex,RGvAlign.ItemIndex=-1);
prSetProp(L,'hAlign',RGhAlign.ItemIndex,RGhAlign.ItemIndex=-1);

GetFrameLine(L,CBLeftShow,EDLeftStyle,UDLeftWidth,bLeftColor,'l');
GetFrameLine(L,CBTopShow,EDTopStyle,UDTopWidth,bTopColor,'t');
GetFrameLine(L,CBRightShow,EDRightStyle,UDRightWidth,bRightColor,'r');
GetFrameLine(L,CBBottomShow,EDBottomStyle,UDBottomWidth,bBottomColor,'b');

prSetProp(L,'DeleteEmptyLines',CBDeleteEmptyLines.State=cbChecked,CBDeleteEmptyLines.State=cbGrayed);
prSetProp(L,'DeleteEmptyLinesAtEnd',CBDeleteEmptyLinesAtEnd.State=cbChecked,CBDeleteEmptyLinesAtEnd.State=cbGrayed);
prSetProp(L,'CanResizeX',CBCanResizeX.State=cbChecked,CBCanResizeX.State=cbGrayed);
prSetProp(L,'CanResizeY',CBCanResizeY.State=cbChecked,CBCanResizeY.State=cbGrayed);
prSetProp(L,'WordWrap',CBWordWrap.State=cbChecked,CBWordWrap.State=cbGrayed);
  prSetProp(L, 'JustifyLastLine', cbJustifyLastLine.State = cbChecked, cbJustifyLastLine.State = cbGrayed);
  prSetProp(L, 'EolIsEndOfParagraph', cbEolIsEndOfParagraph.State = cbChecked, cbEolIsEndOfParagraph.State = cbGrayed);
inherited;
end;

procedure TprMemoEditorForm.SetEnabledAfterCopyToControls;
begin
  bDBFieldName.Enabled := DesignerPanel.SelCount = 1;
  Memo.Enabled := DesignerPanel.SelCount = 1;
  bFont.Enabled := DesignerPanel.SelCount = 1;
  if DesignerPanel.SelCount <> 1 then
  begin
    Memo.Clear;
    STFont.Caption := '';
  end;
end;

procedure TprMemoEditorForm.CopySinglePropertiesToControls;
begin
Memo.Lines.Assign(TprMemoObjRecVersion(v).Memo);
FontDialog.Font.Assign(TprMemoObjRecVersion(v).Font);
UpdateFont;
inherited;
end;

procedure TprMemoEditorForm.CopyMultiplyPropertiesToControls;
begin
bFillColor.SelectedColor := prGetPropDef(L,'FillColor',clDefault);
bFontColor.SelectedColor := prGetPropDef(L,'Font.Color',clDefault);
CBRotate90.State  := prGetPropDefBool(L,'Rotate90');
RGvAlign.ItemIndex := prGetPropDef(L,'vAlign',-1);
RGhAlign.ItemIndex := prGetPropDef(L,'hAlign',-1);

SetFrameLine(L,CBLeftShow,EDLeftStyle,UDLeftWidth,bLeftColor,'l');
SetFrameLine(L,CBTopShow,EDTopStyle,UDTopWidth,bTopColor,'t');
SetFrameLine(L,CBRightShow,EDRightStyle,UDRightWidth,bRightColor,'r');
SetFrameLine(L,CBBottomShow,EDBottomStyle,UDBottomWidth,bBottomColor,'b');

  CBDeleteEmptyLines.State := prGetPropDefBool(L,'DeleteEmptyLines');
  CBDeleteEmptyLinesAtEnd.State := prGetPropDefBool(L,'DeleteEmptyLinesAtEnd');
  CBCanResizeX.State := prGetPropDefBool(L,'CanResizeX');
  CBCanResizeY.State := prGetPropDefBool(L,'CanResizeY');
  CBWordWrap.State := prGetPropDefBool(L,'WordWrap');
  cbJustifyLastLine.State := prGetPropDefBool(L, 'JustifyLastLine');
  cbEolIsEndOfParagraph.State := prGetPropDefBool(L, 'EolIsEndOfParagraph');
end;

procedure TprMemoEditorForm.UpdateFont;
begin
  bFontColor.SelectedColor := FontDialog.Font.Color;
  STFont.Caption := Format('%s, %d',[FontDialog.Font.Name,FontDialog.Font.Size]);
  STFont.Font.Assign(FontDialog.Font);
end;

procedure TprMemoEditorForm.BFontClick(Sender: TObject);
begin
if FontDialog.Execute then
  UpdateFont;
end;

procedure TprMemoEditorForm.EDLeftStyleDrawItem(Control: TWinControl;
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

procedure TprMemoEditorForm.bDBFieldNameClick(Sender: TObject);
var
  s : string;
begin
TprFormatExpressionForm.Create(Self).SelectExpression(DesignerPanel.GetReport,Memo,s);
end;

procedure TprMemoEditorForm.CBDeleteEmptyLinesClick(Sender: TObject);
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

procedure TprMemoEditorForm.bFontColorColorChange(Sender: TObject);
begin
FontDialog.Font.Color:=bFontColor.SelectedColor;
UpdateFont;
end;

end.

