{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxObjectPosSizeForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,

  pr_Common, pr_TxClasses, pr_CommonDesignerPanel, pr_MultiLang, pr_DesignerFunctions;

type
  TprTxObjectPosSizeForm = class(TprCustomPosSizeForm)
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
    prMLRes1: TprMLRes;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    aEdits : array [TprObjectPosSizeProps] of TEdit;
    aLabels : array [TprObjectPosSizeProps] of TLabel;
    
    function FindInaEdits(Edit : TEdit) : TprObjectPosSizeProps;
    procedure SetPosSize(Value : string; Prop : TprObjectPosSizeProps);
  public
    { Public declarations }
    procedure UpdateInfo; override;
  end;

implementation

{$R *.DFM}

function TprTxObjectPosSizeForm.FindInaEdits;
begin
Result := Low(TprObjectPosSizeProps);
while (Result<=High(TprObjectPosSizeProps)) and (aEdits[Result]<>Edit) do Inc(Result);
end;

procedure TprTxObjectPosSizeForm.FormCreate(Sender: TObject);
begin
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

procedure TprTxObjectPosSizeForm.UpdateInfo;
var
  i : TprObjectPosSizeProps;
begin
inherited;
for i:=Low(TprObjectPosSizeProps) to High(TprObjectPosSizeProps) do
  begin
    aEdits[i].Enabled := i in EnabledProps;
    aLabels[i].Enabled := i in EnabledProps;
    if i in EqProps then
      aEdits[i].Text := IntToStr(SizePosArray[i])
    else
      aEdits[i].Text := '';
  end;
end;

procedure TprTxObjectPosSizeForm.SetPosSize;
begin
try
  DesignerPanel.SetPosSizeProp(prop,StrToInt(Value));
except
end;
end;

procedure TprTxObjectPosSizeForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    Key := 0;
    Close;
  end;
end;

procedure TprTxObjectPosSizeForm.FormKeyPress(Sender: TObject;
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

