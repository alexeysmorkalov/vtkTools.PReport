unit pr_PrvShapeEditor;

interface

{$I vtk.inc}

uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, typinfo,

  vgr_ColorButton,

  pr_Classes, pr_PreviewPanel, pr_MultiLang, pr_ShapeObj, vgr_Button;


type
  TprPrvShapeEditorForm = class(TprPreviewPropsForm)
    PC: TPageControl;
    PMain: TTabSheet;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    bPenColor: TvgrColorButton;
    edPenWidth: TEdit;
    udPenWidth: TUpDown;
    edPenStyle: TComboBox;
    edStyle: TComboBox;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    Label6: TLabel;
    bBrushColor: TvgrColorButton;
    edBrushStyle: TComboBox;
    prMLRes1: TprMLRes;
    procedure FormCreate(Sender: TObject);
    procedure edPenStyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure edBrushStyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  protected
    procedure CopyPropertiesFromControls(l : TList); override;
    procedure CopyPropertiesToControls(l : TList); override;
  end;

implementation

uses
  pr_Strings, pr_DesignerFunctions;

{$R *.dfm}

procedure TprPrvShapeEditorForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := Integer(Low(TprShapeObjStyle)) to Integer(High(TprShapeObjStyle)) do
    edStyle.Items.Add(prLoadStr(sShapeStylesFirstIndex - I));
  for I := 0 to integer(High(TPenStyle)) do
    edPenStyle.Items.Add(' ');
  for I := 0 to Integer(High(TBrushStyle)) do
    edBrushStyle.Items.Add(' ');
  prInitColorButtons([bPenColor, bBrushColor]);
end;

procedure TprPrvShapeEditorForm.CopyPropertiesFromControls;
begin
  prSetProp(L, 'Style', edStyle.ItemIndex, edStyle.ItemIndex = -1);

  prSetProp(L, 'BrushColor', bBrushColor.SelectedColor, bBrushColor.SelectedColor = clDefault);
  prSetProp(L, 'BrushStyle', edBrushStyle.ItemIndex, edBrushStyle.ItemIndex = -1);

  prSetProp(L, 'PenColor', bPenColor.SelectedColor, bPenColor.SelectedColor = clDefault);
  prSetProp(L, 'PenStyle', edPenStyle.ItemIndex, edPenStyle.ItemIndex = -1);
  prSetProp(L, 'PenWidth', udPenWidth.Position, udPenWidth.Position = 0);
  inherited;
end;

procedure TprPrvShapeEditorForm.CopyPropertiesToControls;
begin
  edStyle.ItemIndex := prGetPropDef(L, 'Style', -1);

  bBrushColor.SelectedColor := prGetPropDef(L, 'BrushColor', clDefault);
  edBrushStyle.ItemIndex := prGetPropDef(L, 'BrushStyle', -1);

  bPenColor.SelectedColor := prGetPropDef(L, 'PenColor', clDefault);
  edPenStyle.ItemIndex := prGetPropDef(L, 'PenStyle', -1);
  udPenWidth.Position := prGetPropDef(L, 'PenWidth', 0);
end;


procedure TprPrvShapeEditorForm.edPenStyleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  LineWidth = 4;
var
  I: Integer;
  S: string;
  ASize: TSize;
begin
  with TComboBox(Control).Canvas do
  begin
    Font.Color := clWindowText;
    Pen.Color := clBlack;
    Pen.Style := TPenStyle(Index);
    Brush.Color := clWhite;
    FillRect(Rect);

    S := Copy(GetEnumName(TypeInfo(TPenStyle), Index), 3, MaxInt);
    ASize := TextExtent(S);

    TextOut(Rect.Left + 3, Rect.Top + (Rect.Bottom - Rect.Top - ASize.cy) div 2, S);

    I := Rect.Top + (Rect.Bottom - Rect.Top - LineWidth) div 2;
    for I := I to I + LineWidth - 1 do
    begin
      MoveTo(Rect.Left + ASize.cx + 6, I);
      LineTo(Rect.Right - 3, I);
    end;
  end;
end;

procedure TprPrvShapeEditorForm.edBrushStyleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  S: string;
  ASize: TSize;
begin
  with TComboBox(Control).Canvas do
  begin
    Brush.Style := bsSolid;
    FillRect(Rect);

    S := Copy(GetEnumName(TypeInfo(TBrushStyle), Index), 3, MaxInt);
    ASize := TextExtent(S);
    TextOut(Rect.Left + 3, Rect.Top + (Rect.Bottom - Rect.Top - ASize.cy) div 2, S);

    Pen.Color := clBlack;
    Pen.Style := psSolid;
    Brush.Color := clBlack;
    Brush.Style := TBrushStyle(Index);

    Rect.Left := Rect.Left + ASize.cx + 6;
    Rect.Right := Rect.Right - 3;
    Rect.Top := Rect.Top + 3;
    Rect.Bottom := Rect.Bottom - 3;
    with Rect do
      Rectangle(Left, Top, Right, Bottom);
  end;
end;

end.
