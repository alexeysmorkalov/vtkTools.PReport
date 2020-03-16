unit Unit1;

interface

{$I pr.inc}

uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7}Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, pr_Common, pr_Classes, pr_Parser;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    edExpression: TEdit;
    Button1: TButton;
    prReport1: TprReport;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edX: TEdit;
    edY: TEdit;
    edZ: TEdit;
    Label5: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure prReport1UnknownVariable(Sender: TObject;
      const VarName: String; var Value: TprVarValue;
      var IsProcessed: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  AResult: Variant;
  s: string;
begin
  ActiveControl := edExpression;
  try
    s := edExpression.Text;
    AResult := prReport1.Calc(s);
    Application.MessageBox(PChar(Format('Result: %s', [VarToStr(AResult)] )), 'Success', MB_OK or MB_ICONINFORMATION);
  except
    on e: Exception do
      Application.MessageBox(PChar(Format('Error: %s', [e.Message] )), 'Error', MB_OK or MB_ICONEXCLAMATION);
  end;
end;

procedure TForm1.prReport1UnknownVariable(Sender: TObject;
  const VarName: String; var Value: TprVarValue; var IsProcessed: Boolean);

  procedure TextToVarValue(const S: string; var AValue: TprVarValue);
  var
    vi, valCode : integer;
    vd: Extended;
    vdt: TDateTime;
  begin
    val(s, vi, valCode);
    if valCode = 0 then
      _vSetAsInteger(AValue, vi)
    else
      if TextToFloat(PChar(S), vd, fvExtended) then
        _vSetAsDouble(AValue, vd)
      else
      begin
        try
          vdt := StrToDateTime(S);
          _vSetAsDateTime(AValue, vdt);
        except
          _vSetAsString(AValue, S);
        end;
      end;
  end;
  
begin
  if AnsiCompareText(VarName, 'X') = 0 then
  begin
    TextToVarValue(edX.Text, Value);
    IsProcessed := True;
  end
  else
  if AnsiCompareText(VarName, 'Y') = 0 then
  begin
    TextToVarValue(edY.Text, Value);
    IsProcessed := True;
  end
  else
  if AnsiCompareText(VarName, 'Z') = 0 then
  begin
    TextToVarValue(edZ.Text, Value);
    IsProcessed := True;
  end;
end;

end.
 