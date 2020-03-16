{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_ValueEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Pr_Utils, Buttons,

  pr_Common, pr_MultiLang;

type
  TprValueEditorForm = class(TprForm)
    Label1: TLabel;
    EDName: TEdit;
    PC: TPageControl;
    PCalc: TTabSheet;
    PReset: TTabSheet;
    Label2: TLabel;
    CBCalcOn: TComboBox;
    Label3: TLabel;
    CBDataSetName: TComboBox;
    Label4: TLabel;
    CBCrossTabHorzDataSetName: TComboBox;
    Label5: TLabel;
    EDFormula: TEdit;
    Label6: TLabel;
    CBResetOn: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    CBGroup: TComboBox;
    CBResetDataSetName: TComboBox;
    bOK: TButton;
    bCancel: TButton;
    SBExpression: TSpeedButton;
    prMLRes1: TprMLRes;
    CBAggFunction: TComboBox;
    Label9: TLabel;
    CBAccumulate: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bOKClick(Sender: TObject);
    procedure CBCalcOnChange(Sender: TObject);
    procedure CBResetOnChange(Sender: TObject);
    procedure SBExpressionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    V : TprValue;
    function EditValue(_V : TprValue) : boolean;
  end;

implementation

uses pr_Strings, pr_FormatExpression, vgr_Functions;

{$R *.DFM}

procedure TprValueEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

function TprValueEditorForm.EditValue;
var
  i : integer;
begin
V := _V;

CBAccumulate.Checked := v.Accumulate;
EDName.Text := v.Name;
EDFormula.Text := v.Formula;

V.Report.GetAvailableDataSets(CBDataSetName.Items);
CBDataSetName.ItemIndex := CBDataSetName.Items.IndexOf(V.DataSetName);

V.Report.GetAvailableDataSets(CBCrossTabHorzDataSetName.Items);
CBCrossTabHorzDataSetName.ItemIndex := CBCrossTabHorzDataSetName.Items.IndexOf(V.CrossTabHorzDataSetName);

GetEnumNamesToStrings(TypeInfo(TprCalcValueType), CBCalcOn.Items);
CBCalcOn.ItemIndex := integer(V.CalcOn);

GetEnumNamesToStrings(TypeInfo(TprResetValueType), CBResetOn.Items);
CBResetOn.ItemIndex := integer(V.ResetOn);

GetEnumNamesToStrings(TypeInfo(TprAggFunction), CBAggFunction.Items);
CBAggFunction.ItemIndex := integer(V.AggFunction);

for i:=0 to V.Report.Groups.Count-1 do
  CBGroup.Items.AddObject(V.Report.Groups[i].Name,V.Report.Groups[i]);
CBGroup.ItemIndex := CBGroup.Items.IndexOfObject(V.Group);

V.Report.GetAvailableDataSets(CBResetDataSetName.Items);
CBResetDataSetName.ItemIndex := CBResetDataSetName.Items.IndexOf(V.ResetDataSetName);

CBResetOnChange(nil);
CBCalcOnChange(nil);

Result := ShowModal=mrOk;
end;

procedure TprValueEditorForm.bOKClick(Sender: TObject);
begin
if (Trim(EDFormula.Text)='') and (CBAggFunction.ItemIndex<>integer(prafCount)) then
  begin
    MBError(prLoadStr(sInvalidVarFormula));
    exit;
  end;

case CBCalcOn.ItemIndex of
  0,2:
     begin
       if CBDataSetName.ItemIndex<0 then
         begin
           MBError(prLoadStr(sCalcOnDataSetNotDefined));
           exit;
         end;
       if (CBCalcOn.ItemIndex=2) and (CBCrossTabHorzDataSetName.ItemIndex<0) then
         begin
           MBError(prLoadStr(sCalcOnCrossDataSetNotDefined));
           exit;
         end;
     end;
  1: begin
       MBError(prLoadStr(sCalcOnError));
       exit;
     end;
end;

case CBResetOn.ItemIndex of
  1: if CBGroup.ItemIndex<0 then
       begin
         MBError(prLoadStr(sResetOnGroupNotDefined));
         exit;
       end;
  3: if CBResetDataSetName.ItemIndex<0 then
       begin
         MBError(prLoadStr(sResetOnDataSetNotDefined));
         exit;
       end;
end;

V.Name := EDName.Text;
v.Accumulate := CBAccumulate.Checked;
V.AggFunction := TprAggFunction(CBAggFunction.ItemIndex);
V.Formula := EDFormula.Text;
V.CalcOn := TprCalcValueType(CBCalcOn.ItemIndex);
if CBDataSetName.ItemIndex>=0 then
  V.DataSetName := CBDataSetName.Items[CBDataSetName.ItemIndex]
else
  V.DataSetName := '';
if CBCrossTabHorzDataSetName.ItemIndex>=0 then
  V.CrossTabHorzDataSetName := CBCrossTabHorzDataSetName.Items[CBCrossTabHorzDataSetName.ItemIndex]
else
  V.CrossTabHorzDataSetName := '';

V.ResetOn := TprResetValueType(CBResetOn.ItemIndex);
if CBGroup.ItemIndex>=0 then
  V.Group := TprGroup(CBGroup.Items.Objects[CBGroup.ItemIndex])
else
  V.Group := nil;
if CBResetDataSetName.ItemIndex>=0 then
  V.ResetDataSetName := CBResetDataSetName.Items[CBResetDataSetName.ItemIndex]
else
  V.ResetDataSetName := '';
V.Report.DsgnTemplateChanged(nil,true); // notify all report designers
ModalResult := mrOk;
end;

procedure TprValueEditorForm.CBCalcOnChange(Sender: TObject);
begin
CBCrossTabHorzDataSetName.Enabled := CBCalcOn.ItemIndex in [2];
end;

procedure TprValueEditorForm.CBResetOnChange(Sender: TObject);
begin
CBGroup.Enabled := CBResetOn.ItemIndex in [1];
CBResetDataSetName.Enabled := CBResetOn.ItemIndex in [3];
end;

procedure TprValueEditorForm.SBExpressionClick(Sender: TObject);
var
  s : string;
begin
TprFormatExpressionForm.Create(Application).SelectExpression(v.Report,EDFormula,s,false);
end;

procedure TprValueEditorForm.FormCreate(Sender: TObject);
begin
LoadResImage(SBExpression.Glyph,'OPEN');
end;

end.
