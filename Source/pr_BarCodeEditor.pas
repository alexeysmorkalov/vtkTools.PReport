{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

unit pr_BarCodeEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons, vgr_Button,
  vgr_ColorButton, vgr_Barcode,

  pr_Common, pr_Classes, pr_BarCodeObj, pr_FormatExpression, pr_CommonDesignerPanel,
  pr_MultiLang;

type
  TprBarCodeEditorForm = class(TprObjPropsForm)
    PC: TPageControl;
    PMain: TTabSheet;
    PText: TTabSheet;
    PView: TTabSheet;
    Label1: TLabel;
    edText: TEdit;
    bText: TSpeedButton;
    edBarCodeType: TComboBox;
    edShowTextType: TComboBox;
    edTextPosition: TComboBox;
    STFont: TStaticText;
    bTextFont: TSpeedButton;
    Label2: TLabel;
    bBackColor: TvgrColorButton;
    bForeColor: TvgrColorButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    FontDialog: TFontDialog;
    prMLRes1: TprMLRes;
    cbShowTextOverLines: TCheckBox;
    rgRotation: TRadioGroup;
    cbAutoSize: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure bTextClick(Sender: TObject);
    procedure bTextFontClick(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateFont;
  protected
    procedure SetEnabledAfterCopyToControls; override;
    procedure CopySinglePropertiesFromControls(V: TprObjRecVersion); override;
    procedure CopySinglePropertiesToControls(V: TprObjRecVersion); override;
    procedure CopyMultiplyPropertiesFromControls(L: TList); override;
    procedure CopyMultiplyPropertiesToControls(L: TList); override;
  public
    { Public declarations }
  end;

implementation

uses
  pr_Strings, pr_DesignerFunctions;

{$R *.dfm}

procedure TprBarCodeEditorForm.UpdateFont;
begin
  bForeColor.SelectedColor := FontDialog.Font.Color;
  STFont.Caption := Format('%s, %d',[FontDialog.Font.Name,FontDialog.Font.Size]);
  STFont.Font.Assign(FontDialog.Font);
end;

procedure TprBarCodeEditorForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  LoadResImage(bText.Glyph, 'OPEN');

  edShowTextType.Items.Add(prLoadStr(sBarCodeEditorShowTextTypeNone));
  edShowTextType.Items.Add(prLoadStr(sBarCodeEditorShowTextTypeCode));
  edShowTextType.Items.Add(prLoadStr(sBarCodeEditorShowTextTypeType));
  edShowTextType.Items.Add(prLoadStr(sBarCodeEditorShowTextTypeBoth));

  edTextPosition.Items.Add(prLoadStr(sBarCodeEditorTextPositionTopLeft));
  edTextPosition.Items.Add(prLoadStr(sBarCodeEditorTextPositionTopRight));
  edTextPosition.Items.Add(prLoadStr(sBarCodeEditorTextPositionTopCenter));
  edTextPosition.Items.Add(prLoadStr(sBarCodeEditorTextPositionBottomLeft));
  edTextPosition.Items.Add(prLoadStr(sBarCodeEditorTextPositionBottomRight));
  edTextPosition.Items.Add(prLoadStr(sBarCodeEditorTextPositionBottomCenter));

  rgRotation.Items[0] := prLoadStr(sBarCodeRotationNone);

  for I := Integer(Low(TvgrBarcodeType)) to Integer(High(TvgrBarcodeType)) do
    edBarCodeType.Items.Add(BCdata[TvgrBarcodeType(I)].Name);

  prInitColorButtons([bBackColor, bForeColor]);
end;

procedure TprBarCodeEditorForm.SetEnabledAfterCopyToControls;
begin
  bText.Enabled := DesignerPanel.SelCount = 1;
  edText.Enabled := DesignerPanel.SelCount = 1;
  bTextFont.Enabled := DesignerPanel.SelCount = 1;
  if DesignerPanel.SelCount <> 1 then
  begin
    edText.Text := '';
    STFont.Caption := '';
  end;
end;

procedure TprBarCodeEditorForm.CopySinglePropertiesFromControls(V: TprObjRecVersion);
begin
  TprBarCodeObjRecVersion(V).Text := edText.Text;
  TprBarCodeObjRecVersion(V).TextFont.Assign(STFont.Font);
  inherited;
end;

procedure TprBarCodeEditorForm.CopySinglePropertiesToControls(V: TprObjRecVersion);
begin
  edText.Text := TprBarCodeObjRecVersion(v).Text;
  FontDialog.Font.Assign(TprBarCodeObjRecVersion(v).TextFont);
  UpdateFont;
  inherited;
end;

procedure TprBarCodeEditorForm.CopyMultiplyPropertiesFromControls(L: TList);
begin
  prSetProp(L, 'BarCodeType', edBarCodeType.ItemIndex, edBarCodeType.ItemIndex = -1);
  prSetProp(L, 'ShowTextOverLines', cbShowTextOverLines.State = cbChecked, cbShowTextOverLines.State = cbGrayed);
  prSetProp(L, 'ShowTextType', edShowTextType.ItemIndex, edShowTextType.ItemIndex = -1);
  prSetProp(L, 'TextPosition', edTextPosition.ItemIndex, edTextPosition.ItemIndex = -1);
  prSetProp(L, 'BackColor', bBackColor.SelectedColor, bBackColor.SelectedColor = clDefault);
  prSetProp(L, 'ForeColor', bForeColor.SelectedColor, bForeColor.SelectedColor = clDefault);
  prSetProp(L, 'Rotation', rgRotation.ItemIndex, rgRotation.ItemIndex = -1);
  prSetProp(L, 'AutoSize', cbAutoSize.State = cbChecked, cbAutoSize.State = cbGrayed);
  inherited;
end;

procedure TprBarCodeEditorForm.CopyMultiplyPropertiesToControls(L: TList);
begin
  edBarCodeType.ItemIndex := prGetPropDef(L, 'BarCodeType', -1);
  cbShowTextOverLines.State := prGetPropDefBool(L, 'ShowTextOverLines');
  edShowTextType.ItemIndex := prGetPropDef(L, 'ShowTextType', -1);
  edTextPosition.ItemIndex := prGetPropDef(L, 'TextPosition', -1);
  bBackColor.SelectedColor := prGetPropDef(L, 'BackColor', clDefault);
  bForeColor.SelectedColor := prGetPropDef(L, 'ForeColor', clDefault);
  rgRotation.ItemIndex := prGetPropDef(L, 'Rotation', -1);
  cbAutoSize.State := prGetPropDefBool(L, 'AutoSize');
  inherited;
end;

procedure TprBarCodeEditorForm.bTextClick(Sender: TObject);
var
  S: string;
begin
  TprFormatExpressionForm.Create(Application).SelectExpression(DesignerPanel.GetReport, edText, S);
end;

procedure TprBarCodeEditorForm.bTextFontClick(Sender: TObject);
begin
  if FontDialog.Execute then
    UpdateFont;
end;

end.

