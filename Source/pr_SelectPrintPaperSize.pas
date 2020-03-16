{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_SelectPrintPaperSize;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,

  pr_Common, pr_Classes, pr_MultiLang;

{$I PR.INC}

type
  /////////////////////////////////////////////////
  //
  // rprUserSelectPaperSize
  //
  /////////////////////////////////////////////////
  TprUserSelectPaperSizeAction = (pusaScale,pusaSkip);
  rprUserSelectPaperSize = record
    GenPageWidth : integer;
    GenPageHeight : integer;
    Action : TprUserSelectPaperSizeAction;
    PaperSize : integer;
    Orientation : TprPrinterOrientation;
    PageWidth : integer;
    PageHeight : integer;
    DontAsk : boolean;
  end;
  pprUserSelectPaperSize = ^rprUserSelectPaperSize;

  /////////////////////////////////////////////////
  //
  // TprSelectPrintPaperSizeForm
  //
  /////////////////////////////////////////////////
  TprSelectPrintPaperSizeForm = class(TprForm)
    Label1: TLabel;
    Label5: TLabel;
    RBScale: TRadioButton;
    RBSkip: TRadioButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    CBDontAsk: TCheckBox;
    bOK: TButton;
    bCancel: TButton;
    prMLRes1: TprMLRes;
    GroupBox2: TGroupBox;
    iPagePortrait: TImage;
    iPageLandscape: TImage;
    RBPortrait: TRadioButton;
    RBLandscape: TRadioButton;
    GroupBox1: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    EDPaperSize: TComboBox;
    EDPageWidth: TEdit;
    EDPageHeight: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBOtherClick(Sender: TObject);
    procedure RBPortraitClick(Sender: TObject);
    procedure EDPaperSizeClick(Sender: TObject);
    procedure EDPageWidthKeyPress(Sender: TObject; var Key: Char);
    procedure EDPageWidthChange(Sender: TObject);
  private
    { Private declarations }
    fUpdated : boolean;
    Report : TprReport;
    function GetSize(Edit : TEdit; DefValue : integer) : integer;
    procedure SetSize(Edit : TEdit; Value : integer);
  public
    { Public declarations }
    function SelectPrintPaperSize(_Report : TprReport; ep : TprEndPage; var rps : rprUserSelectPaperSize) : boolean;
  end;

implementation

uses
  pr_Utils, pr_Strings;
  
{$R *.DFM}

function TprSelectPrintPaperSizeForm.GetSize(Edit : TEdit; DefValue : integer) : integer;
var
  s : string;
  v : extended;
begin
s := StringReplace(StringReplace(Edit.Text,'.',DecimalSeparator,[rfReplaceAll]),',',DecimalSeparator,[rfReplaceAll]);
if (Length(s)>0) and (s[Length(s)]=DecimalSeparator) then
  Delete(s,Length(s),1);
if TextToFloat(PChar(s),v,fvExtended) then
  Result := Round(v*10)
else
  Result := DefValue;
end;

procedure TprSelectPrintPaperSizeForm.SetSize(Edit : TEdit; Value : integer);
begin
Edit.Text := FormatFloat('0.0',Value/10);
end;

function TprSelectPrintPaperSizeForm.SelectPrintPaperSize(_Report : TprReport; ep : TprEndPage; var rps : rprUserSelectPaperSize) : boolean;
begin
Report := _Report;

Label1.Caption := Format(prLoadStr(sNotSupportedPaperSize),[Report.PrinterName,Format('%dmm x %dmm',[ep.PageInfo.PageWidth div 10,ep.PageInfo.PageHeight div 10]),Report.EndPageIndex(ep)+1]);

EDPaperSize.Items.Assign(Report.prPrinter.PaperNames);
EDPaperSize.Items.Add(prLoadStr(sOtherPageSize));
EDPaperSize.ItemIndex := 0;
if Report.prPrinter.GetPaperSizeIndex(A4_PaperSizeCode)<>-1 then
  EDPaperSize.ItemIndex := Report.prPrinter.GetPaperSizeIndex(A4_PaperSizeCode)
else
  EDPaperSize.ItemIndex := 0;
SetSize(EDPageWidth,A4_PaperWidth);
SetSize(EDPageHeight,A4_PaperHeight);

Result := ShowModal=mrOk;
if Result then
  begin
    if RBScale.Checked then
      rps.Action := pusaScale
    else
      rps.Action := pusaSkip;

    if RBLandscape.Checked then
      rps.Orientation := poLandscape
    else
      rps.Orientation := poPortrait;
    if EDPaperSize.ItemIndex=EDPaperSize.Items.Count-1 then
      rps.PaperSize := -1
    else
      rps.PaperSize := Report.prPrinter.PaperSizes[EDPaperSize.ItemIndex];
    rps.PageWidth := GetSize(EDPageWidth,rps.PageWidth);
    rps.PageHeight := GetSize(EDPageHeight,rps.PageHeight);

    rps.DontAsk := CBDontAsk.Checked;
  end;
end;

procedure TprSelectPrintPaperSizeForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

procedure TprSelectPrintPaperSizeForm.CBOtherClick(Sender: TObject);
begin
Label7.Enabled := RBScale.Checked;
Label6.Enabled := RBScale.Checked;
EDPageWidth.Enabled := RBScale.Checked;
EDPageHeight.Enabled := RBScale.Checked;
Label6.Enabled := RBScale.Checked;
EDPaperSize.Enabled := RBScale.Checked;
RBPortrait.Enabled := RBScale.Checked;
RBLandscape.Enabled := RBScale.Checked;
end;

procedure TprSelectPrintPaperSizeForm.RBPortraitClick(Sender: TObject);
var
  dummy : integer;
begin
if FUpdated then exit;
fUpdated := true;
iPagePortrait.Visible := RBPortrait.Checked;
iPageLandscape.Visible := RBLandscape.Checked;
if EDPaperSize.ItemIndex=EDPaperSize.Items.Count-1 then
  begin
    if ((RBPortrait.Checked) and (GetSize(EDPageWidth,0)>GetSize(EDPageHeight,0))) or
       ((RBLandscape.Checked) and (GetSize(EDPageHeight,0)>GetSize(EDPageWidth,0))) then
      begin
        dummy := GetSize(EDPageWidth,0);
        SetSize(EDPageWidth,GetSize(EDPageHeight,0));
        SetSize(EDPageHeight,dummy);
      end
  end
else
  begin
    if RBPortrait.Checked then
      begin
        SetSize(EDPageWidth,Report.prPrinter.PaperWidths[Report.prPrinter.PaperSizes[EDPaperSize.ItemIndex]]);
        Setsize(EDPageHeight,Report.prPrinter.PaperHeights[Report.prPrinter.PaperSizes[EDPaperSize.ItemIndex]]);
      end
    else
      if RBLandscape.Checked then
        begin
          SetSize(EDPageHeight,Report.prPrinter.PaperWidths[Report.prPrinter.PaperSizes[EDPaperSize.ItemIndex]]);
          SetSize(EDPageWidth,Report.prPrinter.PaperHeights[Report.prPrinter.PaperSizes[EDPaperSize.ItemIndex]]);
        end;
  end;
fUpdated := false;
end;

procedure TprSelectPrintPaperSizeForm.EDPaperSizeClick(Sender: TObject);
var
  i : integer;
begin
if FUpdated then exit;
fUpdated := true;
if EDPaperSize.ItemIndex=EDPaperSize.Items.Count-1 then
  begin
    i := 0;
    EDPapersize.ItemIndex := i;
  end
else
  i := EDPaperSize.ItemIndex;

if RBPortrait.Checked then
  begin
    SetSize(EDPageWidth,Report.prPrinter.PaperWidths[Report.prPrinter.PaperSizes[i]]);
    Setsize(EDPageHeight,Report.prPrinter.PaperHeights[Report.prPrinter.PaperSizes[i]]);
  end
else
  if RBLandscape.Checked then
    begin
      SetSize(EDPageHeight,Report.prPrinter.PaperWidths[Report.prPrinter.PaperSizes[i]]);
      SetSize(EDPageWidth,Report.prPrinter.PaperHeights[Report.prPrinter.PaperSizes[i]]);
    end;
fUpdated := false;
end;

procedure TprSelectPrintPaperSizeForm.EDPageWidthKeyPress(Sender: TObject;
  var Key: Char);
begin
if not (Key in [',','.','0'..'9',DecimalSeparator,#8]) then
  Key := #0;
end;

procedure TprSelectPrintPaperSizeForm.EDPageWidthChange(Sender: TObject);
var
  PaperSize : integer;
  Orientation : TprPrinterOrientation;
begin
if FUpdated then exit;
fUpdated := true;
if Report.prPrinter.FindPaperSize(GetSize(EDPageWidth,0),GetSize(EDPageHeight,0),PaperSize,Orientation,true) then
  begin
    EDPaperSize.ItemIndex := Report.prPrinter.GetPaperSizeIndex(PaperSize);
    RBPortrait.Checked := Orientation=poPortrait;
    RBLandscape.Checked := Orientation=poLandscape;
    iPagePortrait.Visible := RBPortrait.Checked;
    iPageLandscape.Visible := RBLandscape.Checked;
  end
else
  EDPaperSize.ItemIndex := EDPaperSize.Items.Count-1;
fUpdated := false;
end;

end.

