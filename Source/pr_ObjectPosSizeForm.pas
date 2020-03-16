{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_ObjectPosSizeForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, inifiles, math,

  pr_Common, pr_Classes, pr_CommonDesignerPanel, pr_MultiLang, pr_DesignerFunctions;

type
  TprObjectPosSizeForm = class(TprCustomPosSizeForm)
    Label1: TLabel;
    EDUnits: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    EDLeft: TEdit;
    EDTop: TEdit;
    EDRight: TEdit;
    EDBottom: TEdit;
    EDWidth: TEdit;
    EDHeight: TEdit;
    Bevel1: TBevel;
    prMLRes1: TprMLRes;
    procedure FormCreate(Sender: TObject);
    procedure EDUnitsChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    aEdits : array [TprObjectPosSizeProps] of TEdit;
    aLabels : array [TprObjectPosSizeProps] of TLabel;
    function FindInaEdits(Edit : TEdit) : TprObjectPosSizeProps;
    procedure SetPosSize(Value : string; Prop : TprObjectPosSizeProps);
  protected
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    procedure UpdateInfo; override;
  end;

implementation

uses
  pr_Strings;

{$R *.DFM}

function TprObjectPosSizeForm.FindInaEdits;
begin
Result := Low(TprObjectPosSizeProps);
while (Result<=High(TprObjectPosSizeProps)) and (aEdits[Result]<>Edit) do Inc(Result);
end;

procedure TprObjectPosSizeForm.FormCreate(Sender: TObject);
var
  i : integer;
begin
for i:=integer(prpsuPixels) to integer(prpsuM) do
  EDUnits.Items.Add(prLoadStr(sUnitsDescsOffset-i));
aEdits[prpsaLeft] := EDLeft;
aEdits[prpsaRight] := EDRight;
aEdits[prpsaTop] := EDTop;
aEdits[prpsaBottom] := EDBottom;
aEdits[prpsaWidth] := EDWidth;
aEdits[prpsaHeight] := EDHeight;
aLabels[prpsaLeft] := Label2;
aLabels[prpsaRight] := Label4;
aLabels[prpsaTop] := Label3;
aLabels[prpsaBottom] := Label5;
aLabels[prpsaWidth] := Label6;
aLabels[prpsaHeight] := Label7;
end;

procedure TprObjectPosSizeForm.prRestoreProperties;
begin
inherited;
EDUnits.ItemIndex := Max(0,Ini.ReadInteger(sn,'CurrentUnits',0));
end;

procedure TprObjectPosSizeForm.prSaveProperties;
begin
inherited;
Ini.WriteInteger(sn,'CurrentUnits',Max(EDUnits.ItemIndex,0));
end;

procedure TprObjectPosSizeForm.UpdateInfo;
var
  i : TprObjectPosSizeProps;
begin
inherited;
for i:=Low(TprObjectPosSizeProps) to High(TprObjectPosSizeProps) do
  begin
    aEdits[i].Enabled := i in EnabledProps;
    aLabels[i].Enabled := i in EnabledProps;
    if i in EqProps then
      aEdits[i].Text := prConvertFromPixelsString(SizePosArray[i],TprPosSizeUnits(EDUnits.ItemIndex),i in [prpsaLeft,prpsaRight,prpsaWidth])
    else
      aEdits[i].Text := '';
  end;
end;

procedure TprObjectPosSizeForm.EDUnitsChange(Sender: TObject);
begin
UpdateInfo;
end;

procedure TprObjectPosSizeForm.SetPosSize;
begin
try
  DesignerPanel.SetPosSizeProp(prop,prConvertToPixels(StrToFloat(Value),TprPosSizeUnits(EDUnits.ItemIndex),prop in [prpsaLeft,prpsaRight,prpsaWidth]));
except
end;
end;

procedure TprObjectPosSizeForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TprObjectPosSizeForm.FormKeyPress(Sender: TObject;
  var Key: Char);
begin
if Key=char(13) then
  begin
    if ActiveControl is TEdit then
      begin
        SetPosSize(TEdit(ActiveControl).Text,FindInaEdits(TEdit(ActiveControl)));
        Key := #0;
      end;
  end;
end;

end.

