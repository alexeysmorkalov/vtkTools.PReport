{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_RichEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons, Math,

  vgr_ColorButton,

  pr_Common, pr_Classes, pr_CommonDesignerPanel, pr_MultiLang;

type
  TprRichEditorForm = class(TprObjPropsForm)
    PC: TPageControl;
    PText: TTabSheet;
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
    CBCanResizeY: TCheckBox;
    CBWordWrap: TCheckBox;
    Panel1: TPanel;
    bDBFieldName: TSpeedButton;
    prMLRes1: TprMLRes;
    Rich: TRichEdit;
    bLeftColor: TvgrColorButton;
    bTopColor: TvgrColorButton;
    bRightColor: TvgrColorButton;
    bBottomColor: TvgrColorButton;
    procedure FormCreate(Sender: TObject);
    procedure EDLeftStyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure bDBFieldNameClick(Sender: TObject);
    procedure CBDeleteEmptyLinesClick(Sender: TObject);
  private
    { Private declarations }
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

procedure TprRichEditorForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
LoadResImage(bDBFieldName.Glyph,'OPEN');
prInitColorButtons([bLeftColor,bTopColor,bRightColor,bBottomColor]);
for i:=0 to integer(High(TPenStyle))-2 do
  begin
    EDLeftStyle.Items.Add(' ');
    EDTopStyle.Items.Add(' ');
    EDRightStyle.Items.Add(' ');
    EDBottomStyle.Items.Add(' ');
  end;
end;

procedure TprRichEditorForm.CopySinglePropertiesFromControls;
begin
CopyRichText(Rich,TprRichObjRecVersion(v).hwndRich);
inherited;
end;

procedure TprRichEditorForm.CopyMultiplyPropertiesFromControls;
begin
GetFrameLine(L,CBLeftShow,EDLeftStyle,UDLeftWidth,bLeftColor,'l');
GetFrameLine(L,CBTopShow,EDTopStyle,UDTopWidth,bTopColor,'t');
GetFrameLine(L,CBRightShow,EDRightStyle,UDRightWidth,bRightColor,'r');
GetFrameLine(L,CBBottomShow,EDBottomStyle,UDBottomWidth,bBottomColor,'b');

prSetProp(L,'DeleteEmptyLines',CBDeleteEmptyLines.State=cbChecked,CBDeleteEmptyLines.State=cbGrayed);
prSetProp(L,'DeleteEmptyLinesAtEnd',CBDeleteEmptyLinesAtEnd.State=cbChecked,CBDeleteEmptyLinesAtEnd.State=cbGrayed);
prSetProp(L,'CanResizeY',CBCanResizeY.State=cbChecked,CBCanResizeY.State=cbGrayed);
prSetProp(L,'WordWrap',CBWordWrap.State=cbChecked,CBWordWrap.State=cbGrayed);

inherited;
end;

procedure TprRichEditorForm.SetEnabledAfterCopyToControls;
begin
bDBFieldName.Enabled := DesignerPanel.SelCount=1;
Rich.Enabled := DesignerPanel.SelCount=1;
if DesignerPanel.SelCount<>1 then
  Rich.Clear;
end;

procedure TprRichEditorForm.CopySinglePropertiesToControls;
begin
CopyRichText(TprRichObjRecVersion(v).hwndRich,Rich);
inherited;
end;

procedure TprRichEditorForm.CopyMultiplyPropertiesToControls;
begin
SetFrameLine(L,CBLeftShow,EDLeftStyle,UDLeftWidth,bLeftColor,'l');
SetFrameLine(L,CBTopShow,EDTopStyle,UDTopWidth,bTopColor,'t');
SetFrameLine(L,CBRightShow,EDRightStyle,UDRightWidth,bRightColor,'r');
SetFrameLine(L,CBBottomShow,EDBottomStyle,UDBottomWidth,bBottomColor,'b');

CBDeleteEmptyLines.State:=prGetPropDefBool(L,'DeleteEmptyLines');
CBDeleteEmptyLinesAtEnd.State:=prGetPropDefBool(L,'DeleteEmptyLinesAtEnd');
CBCanResizeY.State := prGetPropDefBool(L,'CanResizeY');
CBWordWrap.State := prGetPropDefBool(L,'WordWrap');
end;

procedure TprRichEditorForm.EDLeftStyleDrawItem(Control: TWinControl;
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
    Brush.Color:=clWhite;
    FillRect(Rect);

    i:=Rect.Top+(Rect.Bottom-Rect.Top-LineWidth) div 2;
    for i:=i to i+LineWidth-1 do
      begin
        MoveTo(Rect.Left+3,i);
        LineTo(Rect.Right-3,i);
      end;
  end;
end;

procedure TprRichEditorForm.bDBFieldNameClick(Sender: TObject);
var
  s : string;
begin
TprFormatExpressionForm.Create(Application).SelectExpression(DesignerPanel.GetReport,Rich,s);
end;

procedure TprRichEditorForm.CBDeleteEmptyLinesClick(Sender: TObject);
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

end.

