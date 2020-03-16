{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_PrvRichEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,

  vgr_ColorButton,

  pr_Classes, pr_PreviewPanel, pr_MultiLang;

{$I pr.inc}

type
  TprPrvRichEditorForm = class(TprPreviewPropsForm)
    PC: TPageControl;
    PText: TTabSheet;
    Rich: TRichEdit;
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
    prMLRes1: TprMLRes;
    bLeftColor: TvgrColorButton;
    bTopColor: TvgrColorButton;
    bRightColor: TvgrColorButton;
    bBottomColor: TvgrColorButton;
    procedure FormCreate(Sender: TObject);
    procedure EDLeftStyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    { Private declarations }
  protected
    procedure CopyPropertiesFromControls(l : TList); override;
    procedure CopyPropertiesToControls(l : TList); override;
  public
    { Public declarations }
  end;

implementation

uses
  pr_Strings, pr_DesignerFunctions;

{$R *.DFM}

procedure TprPrvRichEditorForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
prInitColorButtons([bLeftColor, bTopColor, bRightColor, bBottomColor]);
for i:=0 to integer(High(TPenStyle))-2 do
  begin
    EDLeftStyle.Items.Add(' ');
    EDTopStyle.Items.Add(' ');
    EDRightStyle.Items.Add(' ');
    EDBottomStyle.Items.Add(' ');
  end;
end;

procedure TprPrvRichEditorForm.CopyPropertiesFromControls;
begin
if l.Count=1 then
  CopyRichText(Rich,TprRichObjRecVersion(l[0]).hwndRich);
GetFrameLine(L,CBLeftShow,EDLeftStyle,UDLeftWidth,bLeftColor,'l');
GetFrameLine(L,CBTopShow,EDTopStyle,UDTopWidth,bTopColor,'t');
GetFrameLine(L,CBRightShow,EDRightStyle,UDRightWidth,bRightColor,'r');
GetFrameLine(L,CBBottomShow,EDBottomStyle,UDBottomWidth,bBottomColor,'b');
end;

procedure TprPrvRichEditorForm.CopyPropertiesToControls;
begin
if l.Count=1 then
  begin
    CopyRichText(TprRichObjRecVersion(l[0]).hwndRich,Rich);
    Rich.Enabled := true;
  end
else
  begin
    Rich.Clear;
    Rich.Enabled := false;
  end;
SetFrameLine(L,CBLeftShow,EDLeftStyle,UDLeftWidth,bLeftColor,'l');
SetFrameLine(L,CBTopShow,EDTopStyle,UDTopWidth,bTopColor,'t');
SetFrameLine(L,CBRightShow,EDRightStyle,UDRightWidth,bRightColor,'r');
SetFrameLine(L,CBBottomShow,EDBottomStyle,UDBottomWidth,bBottomColor,'b');
end;

procedure TprPrvRichEditorForm.EDLeftStyleDrawItem(Control: TWinControl;
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

end.
