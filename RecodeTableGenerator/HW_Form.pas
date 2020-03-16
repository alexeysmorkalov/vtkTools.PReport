unit HW_Form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, inifiles;

type
  THWForm = class(TForm)
    bBrowse: TSpeedButton;
    Memo1: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    EDDescription: TEdit;
    Label4: TLabel;
    EDOEMCP: TEdit;
    EDANSICP: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    EDName: TEdit;
    OpenDialog: TOpenDialog;
    CBSetAsDefault: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Memo1KeyPress(Sender: TObject; var Key: Char);
    procedure bBrowseClick(Sender: TObject);
  private
    { Private declarations }
    OemCP : integer;
    AnsiCP : integer;
    OEMToWIN : array [0..127] of char;
    WINToOEM : array [0..127] of char;
  public
    { Public declarations }
  end;

var
  HWForm: THWForm;

implementation

{$R *.DFM}

procedure Decode(sSource,sDest,Table : PChar);
var
  i : integer;
begin
for i:=0 to lstrlen(sSource)-1 do
  if byte(sSource[i])>=128 then
    sDest[i] := Table[byte(sSource[i])-128];
end;

procedure THWForm.FormCreate(Sender: TObject);
var
  i,j : integer;
  aBuf1 : array [0..1] of char;
  aBuf2 : array [0..1] of char;
  aUsed : array [0..127] of byte;
begin
OemCP := GetOEMCP;
AnsiCP := GetACP;
EDOEMCP.Text := IntToStr(OemCP);
EDANSICP.Text := IntToStr(AnsiCP);
EDName.Text := Format('Auto_%d_to_%d',[OemCP,AnsiCP]);
EDDescription.Text := Format('Auto %d(OEM) to %d(ANSI)',[OemCP,AnsiCP]);

Memo1.Font.CharSet := OEM_CHARSET;
// generate table
FillMemory(@aUsed[0],128,0);
aBuf1[1] := #0;
aBuf2[1] := #0;
for i:=128 to 255 do
  begin
    aBuf1[0] := char(i);
    OemToChar(aBuf1,aBuf1);
    CharToOem(aBuf1,aBuf2);
    if aBuf2[0]=char(i) then
      begin
        OEMToWIN[i-128] := aBuf1[0];
        aUsed[byte(aBuf1[0])-128] := 1;
      end
    else
      OEMToWIN[i-128] := #0;
  end;
for i:=128 to 255 do
  if OEMToWIN[i-128]=#0 then
    begin
      j := 0;
      while (j<=127) and (aUsed[j]=1) do Inc(j);
      if j>127 then
        raise Exception.Create('Error generate recode table')
      else
        begin
          OEMToWIN[i-128] := char(j+128);
          aUsed[j] := 1;
        end;
    end;

for i:=128 to 255 do
  begin
    j := 0;
    while (j<128) and (OEMtoWIN[j]<>char(i)) do Inc(j);
    if j<128 then
      WINtoOEM[i-128] := char(j+128)
    else
      raise Exception.Create('Error generate recode table');
  end;
end;

procedure THWForm.Memo1KeyPress(Sender: TObject; var Key: Char);
var
  s : array [0..1] of char;
begin
s[0] := Key;
s[1] := #0;
Decode(s,s,WINtoOEM);
Key := s[0];
end;

procedure THWForm.bBrowseClick(Sender: TObject);
var
  i : integer;
  s : string;
  ini : TIniFile;
begin
if OpenDialog.Execute then
  begin
    ini := TIniFile.Create(OpenDialog.FileName);
    try
      s := '';
      for i:=0 to 127 do
        s := s+'#'+IntToStr(integer(OEMtoWIN[i]));
      ini.WriteString('RecodeTable_'+EDName.Text,'Description',EDDescription.Text);
      ini.WriteString('RecodeTable_'+EDName.Text,'OEMtoWINTable',s);
      if CBSetAsDefault.Checked then
        ini.WriteString('General','DefaultRecodeTable',EDName.Text);
    finally
      ini.Free;
    end;
  end;
end;

end.
