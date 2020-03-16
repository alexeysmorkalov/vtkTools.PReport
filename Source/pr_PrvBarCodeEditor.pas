{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

unit pr_PrvBarCodeEditor;

interface

{$i pr.inc}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons, vgr_Button,
  vgr_ColorButton, vgr_Barcode,

  pr_Common, pr_Classes, pr_BarCodeObj, pr_PreviewPanel,
  pr_MultiLang;

type
  TprPrvBarCodeEditorForm = class(TprPreviewPropsForm)
    PC: TPageControl;
    PMain: TTabSheet;
    PText: TTabSheet;
    PView: TTabSheet;
    Label1: TLabel;
    edText: TEdit;
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
    procedure FormCreate(Sender: TObject);
    procedure bTextFontClick(Sender: TObject);
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
  pr_Strings, pr_DesignerFunctions;

{$R *.dfm}

procedure TprPrvBarCodeEditorForm.UpdateFont;
begin
  bForeColor.SelectedColor := FontDialog.Font.Color;
  STFont.Caption := Format('%s, %d',[FontDialog.Font.Name,FontDialog.Font.Size]);
  STFont.Font.Assign(FontDialog.Font);
end;

procedure TprPrvBarCodeEditorForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
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

procedure TprPrvBarCodeEditorForm.CopyPropertiesFromControls(L: TList);
var
  I: Integer;
begin
  for I := 0 to L.Count - 1 do
    with TprBarCodeObjRecVersion(L[I]) do
    begin
      Text := edText.Text;
      TextFont.Assign(STFont.Font);
    end;

  prSetProp(L, 'BarCodeType', edBarCodeType.ItemIndex, edBarCodeType.ItemIndex = -1);
  prSetProp(L, 'ShowTextOverLines', cbShowTextOverLines.State = cbChecked, cbShowTextOverLines.State = cbGrayed);
  prSetProp(L, 'ShowTextType', edShowTextType.ItemIndex, edShowTextType.ItemIndex = -1);
  prSetProp(L, 'TextPosition', edTextPosition.ItemIndex, edTextPosition.ItemIndex = -1);
  prSetProp(L, 'BackColor', bBackColor.SelectedColor, bBackColor.SelectedColor = clDefault);
  prSetProp(L, 'ForeColor', bForeColor.SelectedColor, bForeColor.SelectedColor = clDefault);
  prSetProp(L, 'Rotation', rgRotation.ItemIndex, rgRotation.ItemIndex = -1);
end;

procedure TprPrvBarCodeEditorForm.CopyPropertiesToControls(L: TList);
begin
  if L.Count > 0 then
    with TprBarCodeObjRecVersion(L[0]) do
    begin
      edText.Text := Text;
      FontDialog.Font.Assign(TextFont);
      UpdateFont;
    end;

  edBarCodeType.ItemIndex := prGetPropDef(L, 'BarCodeType', -1);
  cbShowTextOverLines.State := prGetPropDefBool(L, 'ShowTextOverLines');
  edShowTextType.ItemIndex := prGetPropDef(L, 'ShowTextType', -1);
  edTextPosition.ItemIndex := prGetPropDef(L, 'TextPosition', -1);
  bBackColor.SelectedColor := prGetPropDef(L, 'BackColor', clDefault);
  bForeColor.SelectedColor := prGetPropDef(L, 'ForeColor', clDefault);
  rgRotation.ItemIndex := prGetPropDef(L, 'Rotation', -1);
end;

procedure TprPrvBarCodeEditorForm.bTextFontClick(Sender: TObject);
begin
  if FontDialog.Execute then
    UpdateFont;
end;

end.

