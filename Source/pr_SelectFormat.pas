{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_SelectFormat;

interface

uses                        
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, IniFiles, Buttons,

  pr_Common, pr_Parser, pr_MultiLang, pr_strings, Pr_Utils;

type
  TprSelectFormatForm = class(TprForm)
    CBUseAlign: TCheckBox;
    GroupBox1: TGroupBox;
    CBUseFormat: TCheckBox;                                             
    bOK: TButton;
    bCancel: TButton;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    LBTypes: TListBox;
    Label2: TLabel;
    LBFormats: TListBox;
    EDNewFormat: TEdit;
    RBLeft: TRadioButton;
    RBCenter: TRadioButton;
    RBRight: TRadioButton;
    Label3: TLabel;
    EDWidth: TEdit;
    UDWidth: TUpDown;
    LBResults: TListBox;
    Label4: TLabel;
    bAdd: TSpeedButton;
    bDelete: TSpeedButton;
    prMLRes1: TprMLRes;
    procedure CBUseAlignClick(Sender: TObject);
    procedure CBUseFormatClick(Sender: TObject);
    procedure LBTypesClick(Sender: TObject);
    procedure LBFormatsClick(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EDNewFormatChange(Sender: TObject);
  private
    { Private declarations }
    lNumber,lDate,lTime,lDateTime,lText : TStringList;

    procedure UpdateButtons;
    procedure UpdateResults;
    procedure AddFormat;
    procedure DelFormat;
    function  GetList : TStringList;
    function  GetSelectedFormat : string;
  protected
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    function SelectFormat(var f : string) : boolean;
  end;

implementation

{$R *.DFM}

procedure TprSelectFormatForm.prRestoreProperties;

  procedure ReadSection(sn : string; l : TStrings);
  var
    i,p : integer;
  begin
  Ini.ReadSectionValues(sn,l);
  for i:=0 to l.Count-1 do
    begin
      p:=pos('=',l[i]);
      if p>0 then
        l[i]:=Copy(l[i],p+1,Length(l[i]));
    end;
  end;

begin
inherited;
lNumber  :=TStringList.Create;
lDate    :=TStringList.Create;
lTime    :=TStringList.Create;
lDateTime:=TStringList.Create;
lText    :=TStringList.Create;

ReadSection(sn+'Number',lNumber);
ReadSection(sn+'Date',lDate);
ReadSection(sn+'Time',lTime);
ReadSection(sn+'DateTime',lDateTime);
ReadSection(sn+'Text',lText);
end;

procedure TprSelectFormatForm.prSaveProperties;

  procedure WriteSection(sn : string; l : TStrings);
  var
    i : integer;
  begin
  Ini.EraseSection(sn);
  for i:=0 to l.Count-1 do
    Ini.WriteString(sn,IntToStr(i),l[i]);
  end;

begin
inherited;
WriteSection(sn+'Number',LNumber);
WriteSection(sn+'Date',lDate);
WriteSection(sn+'Time',lTime);
WriteSection(sn+'DateTime',lDateTime);
WriteSection(sn+'Text',lText);

lNumber.Free;
lDate.Free;
lTime.Free;
lDateTime.Free;
lText.Free;
end;

procedure TprSelectFormatForm.UpdateButtons;
begin
RBLeft.Enabled  :=CBUseAlign.Checked;
RBCenter.Enabled:=CBUseAlign.Checked;
RBRight.Enabled :=CBUseAlign.Checked;
EDWidth.Enabled :=CBUseAlign.Checked;
UDWidth.Enabled :=CBUseAlign.Checked;

LBTypes.Enabled    :=CBUseFormat.Checked;
EDNewFormat.Enabled:=CBUseFormat.Checked and (LBTypes.ItemIndex>0);
LBFormats.Enabled  :=CBUseFormat.Checked and (LBTypes.ItemIndex>=0);

bDelete.Enabled    :=CBUseFormat.Checked and (LBTypes.ItemIndex>0) and (LBFormats.ItemIndex>=0);
bAdd.Enabled       :=CBUseFormat.Checked and (LBTypes.ItemIndex>0) and (Trim(EDNewFormat.Text)<>'') and (LBFormats.Items.IndexOf(EDNewFormat.Text)=-1);

bOk.Enabled        :=(CBUseAlign.Checked) or (CBUseFormat.Checked and (EDNewFormat.Text<>''));
end;

procedure TprSelectFormatForm.UpdateResults;
const
  cNumber =1123.4567891256;
  cPercent=45.12345678;
var
  f : string;
  v : TprVarValue;
begin
LBResults.Clear;
try
  case LBTypes.ItemIndex of
    0: begin
         case LBFormats.ItemIndex of
           0,1,2,5,6:
             begin
               case LBFormats.ItemIndex of
                 0: f:=SimpleCurrencyFormat;
                 1: f:=ShortCurrencyFormat;
                 2: f:=CurrencyFormat;
                 5: f:=SpacedCurrencyFormat;
                 6: f:=BankCurrencyFormat;
               end;
               _vSetAsDouble(v,cNumber);
               LBResults.Items.Add(pFormat(f,v));
               _vSetAsDouble(v,-cNumber);
               LBResults.Items.Add(pFormat(f,v));
             end;
           4:
             begin
               _vSetAsDateTime(v,Now);
               LBResults.Items.Add(pFormat(ShortDateFormat,v));
             end;
           3:
             begin
               _vSetAsDouble(v,cPercent);
               LBResults.Items.Add(pFormat(PercentFormat,v));
               _vSetAsDouble(v,-cPercent);
               LBResults.Items.Add(pFormat(PercentFormat,v));
             end;
         end;
       end;
    1: begin
         _vSetAsDouble(v,cNumber);
         LBResults.Items.Add(pFormat(EDNewFormat.Text,v));
         _vSetAsDouble(v,-cNumber);
         LBResults.Items.Add(pFormat(EDNewFormat.Text,v));
       end;
    2: begin
         _vSetAsDateTime(v,CutTime(Now));
         LBResults.Items.Add(pFormat(EDNewFormat.Text,v));
       end;
    3: begin
         _vSetAsDateTime(v,Frac(Now));
         LBResults.Items.Add(pFormat(EDNewFormat.Text,v));
       end;
    4: begin
         _vSetAsDateTime(v,Now);
         LBResults.Items.Add(pFormat(EDNewFormat.Text,v));
       end;
  end;
except
  LBResults.Clear;
end;
end;

function TprSelectFormatForm.GetList;
begin
Result:=nil;
case LBTypes.ItemIndex of
  1: Result:=lNumber;
  2: Result:=lDate;
  3: Result:=lTime;
  4: Result:=lDateTime;
  5: Result:=lText;
end;
end;

procedure TprSelectFormatForm.AddFormat;
var
  l : TStringList;
begin
if (LBTypes.ItemIndex>0) and (Trim(EDNewFormat.Text)<>'') then
  begin
    l:=GetList;
    if l.indexof(EDNewFormat.Text)=-1 then
      begin
        l.Add(EDNewFormat.Text);
        LBFormats.Items.Add(EDNewFormat.Text);
      end;
  end;
end;

procedure TprSelectFormatForm.DelFormat;
var
  s : string;
  l : TStringList;
begin
if (LBTypes.ItemIndex>0) and (LBFormats.ItemIndex>=0) then
  begin
    l:=GetList;
    s:=LBFormats.Items[LBFormats.ItemIndex];
    if l.indexof(s)<>-1 then
      begin
        l.Delete(l.indexof(s));
        LBFormats.Items.Delete(LBFormats.ItemIndex);
      end;
  end;
end;

function TprSelectFormatForm.GetSelectedFormat;
begin
if LBTypes.ItemIndex=0 then
  begin
    case LBFormats.ItemIndex of
      0: Result:='r';
      1: Result:='s';
      2: Result:='c';
      3: Result:='p';
      4: Result:='d';
      5: Result:='q';
      6: Result:='b';
    end;
  end
else
  begin
    Result:=EDNewFormat.Text;
  end;
end;

function TprSelectFormatForm.SelectFormat;
begin
Result:=ShowModal=mrOk;
if Result then
  begin
    f:=':';
    if CBUseAlign.Checked then
      begin
        f:=f+IntToStr(UDWidth.Position);
        if RBLeft.Checked then
          f:=f+'l'
        else
          if RBCenter.Checked then
            f:=f+'c'
          else
            f:=f+'r';
      end;
    if CBUseFormat.Checked then
      begin
        f:=f+'<'+GetSelectedFormat+'>';
      end;
  end;
end;

procedure TprSelectFormatForm.CBUseAlignClick(Sender: TObject);
begin
UpdateButtons;
end;

procedure TprSelectFormatForm.CBUseFormatClick(Sender: TObject);
begin
UpdateButtons;
end;

procedure TprSelectFormatForm.LBTypesClick(Sender: TObject);
begin
case LBTypes.ItemIndex of
  0  : begin
         LBFormats.Clear;
         LBFormats.Items.Add(prLoadStr(sStandartDisplayFormatSimpleCurrency));
         LBFormats.Items.Add(prLoadStr(sStandartDisplayFormatShortCurrency));
         LBFormats.Items.Add(prLoadStr(sStandartDisplayFormatCurrency));
         LBFormats.Items.Add(prLoadStr(sStandartDisplayFormatPercent));
         LBFormats.Items.Add(prLoadStr(sStandartDisplayFormatShortDate));
         LBFormats.Items.Add(prLoadStr(sStandartDisplayFormatCurrencyWithDelim));
         LBFormats.Items.Add(prLoadStr(sStandartDisplayFormatBankCurrency));
       end;
  else begin
         LBFormats.Items.Assign(GetList);
       end;
end;
if LBFormats.Items.Count>0 then
  LBFormats.ItemIndex:=0;
LBFormatsClick(nil);

UpdateButtons;
end;

procedure TprSelectFormatForm.LBFormatsClick(Sender: TObject);
begin
if (LBFormats.ItemIndex>=0) and (LBFormats.Items.Count>0) then
  EDNewFormat.Text:=LBFormats.Items[LBFormats.ItemIndex]
else
  EDNewFormat.Text:='';
UpdateResults;
end;

procedure TprSelectFormatForm.bAddClick(Sender: TObject);
begin
AddFormat;
end;

procedure TprSelectFormatForm.bDeleteClick(Sender: TObject);
begin
DelFormat;
end;

procedure TprSelectFormatForm.FormCreate(Sender: TObject);
begin
LBTypes.Items[0]:=prLoadStr(sDisplayFormatStandart);
LBTypes.Items[1]:=prLoadStr(sDisplayFormatNumber);
LBTypes.Items[2]:=prLoadStr(sDisplayFormatDate);
LBTypes.Items[3]:=prLoadStr(sDisplayFormatTime);
LBTypes.Items[4]:=prLoadStr(sDisplayFormatDateTime);
LBTypes.Items[5]:=prLoadStr(sDisplayFormatText);

LoadResImage(bDelete.Glyph,'DELVERSION');
LoadResImage(bAdd.Glyph,'NEWVERSION');

LBTypes.ItemIndex:=0;
LBTypesClick(nil);
end;

procedure TprSelectFormatForm.EDNewFormatChange(Sender: TObject);
begin
UpdateResults;
UpdateButtons;
end;

end.
