{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_PageParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Pr_Utils,

  pr_Common, pr_Classes, pr_MultiLang;

{$I pr.inc}

type
  TprPageParamsForm = class(TprForm)
    PC: TPageControl;
    TabSheet1: TTabSheet;
    bOK: TButton;
    bCancel: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    iPagePortrait: TImage;
    iPageLandscape: TImage;
    RBPortrait: TRadioButton;
    RBLandscape: TRadioButton;
    EDPaperSize: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    EDPageWidth: TEdit;
    EDPageHeight: TEdit;
    prMLRes1: TprMLRes;
    Label10: TLabel;
    EDPrinter: TComboBox;
    TabSheet3: TTabSheet;
    Label11: TLabel;
    Label12: TLabel;
    EDScaleMode: TComboBox;
    CBFitObjects: TCheckBox;
    Label16: TLabel;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    EDTop: TEdit;
    EDBottom: TEdit;
    Label7: TLabel;
    Label2: TLabel;
    EDLeft: TEdit;
    EDRight: TEdit;
    Label5: TLabel;
    EDTopMin: TEdit;
    EDBottomMin: TEdit;
    EDLeftMin: TEdit;
    EDRightMin: TEdit;
    Label1: TLabel;
    GroupBox4: TGroupBox;
    iScalePagePortrait: TImage;
    iScalePageLandscape: TImage;
    RBScalePortrait: TRadioButton;
    RBScaleLandscape: TRadioButton;
    GroupBox5: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    EDScalePaperSize: TComboBox;
    EDScalePageWidth: TEdit;
    EDScalePageHeight: TEdit;
    PDesign: TTabSheet;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    EDDsgnHeight: TEdit;
    EDDsgnWidth: TEdit;
    CBVisible: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RBPortraitClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EDPrinterClick(Sender: TObject);
    procedure EDScaleModeClick(Sender: TObject);
    procedure RBScalePortraitClick(Sender: TObject);
    procedure EDPaperSizeClick(Sender: TObject);
    procedure EDScalePaperSizeClick(Sender: TObject);
    procedure EDPageWidthChange(Sender: TObject);
    procedure EDScalePageWidthChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EDDsgnWidthChange(Sender: TObject);
    procedure EDPageWidthKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    fUpdated : boolean;
    Report : TprReport;
    PageInfo : TprPageInfo;
    PageScaleInfo : TprPageScaleInfo;
    prPrinter : TprPrinter;
    DsgnWidthDelta,DsgnHeightDelta : integer;
    procedure PrinterChanged;
    procedure UpdateDsgnSizes;
    procedure UpdateRB(RBPortrait,RBLandscape : TRadioButton; iPortrait,iLandscape : TImage; EDPaperSize : TComboBox; EDPageWidth,EDPageHeight : TEdit);
    procedure UpdateEDPaperSize(RBPortrait,RBLandscape : TRadioButton; EDPaperSize : TComboBox; EDPageWidth,EDPageHeight : TEdit);
    procedure UpdateEDSizes(RBPortrait,RBLandscape : TRadioButton; iPortrait,iLandscape : TImage; EDPaperSize : TComboBox; EDPageWidth,EDPageHeight : TEdit);
    function GetSize(Edit : TEdit; DefValue : integer) : integer;
    procedure SetSize(Edit : TEdit; Value : integer);
    function GetMargin(Edit : TEdit; DefValue : extended) : extended;
    procedure SetMargin(Edit : TEdit; Value : extended);
  public
    { Public declarations }
    function EditOptions(_Report : TprReport;
                         _PageInfo : TprPageInfo;
                         _PageScaleInfo : TprPageScaleInfo;
                         EnableDsgnSizes : boolean;
                         var DsgnWidth,DsgnHeight : integer;
                         EnableVisible : boolean;
                         var Visible : boolean) : boolean;
  end;

implementation

uses pr_Strings;

{$R *.DFM}

function TprPageParamsForm.GetSize(Edit : TEdit; DefValue : integer) : integer;
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

procedure TprPageParamsForm.SetSize(Edit : TEdit; Value : integer);
begin
Edit.Text := FormatFloat('0.0',Value/10);
end;

function TprPageParamsForm.GetMargin(Edit : TEdit; DefValue : extended) : extended;
var
  s : string;
begin
s := StringReplace(StringReplace(Edit.Text,'.',DecimalSeparator,[rfReplaceAll]),',',DecimalSeparator,[rfReplaceAll]);
if (Length(s)>0) and (s[Length(s)]=DecimalSeparator) then
  Delete(s,Length(s),1);
if not TextToFloat(PChar(s),Result,fvExtended) then
  Result := DefValue;
end;

procedure TprPageParamsForm.SetMargin(Edit : TEdit; Value : extended);
begin
Edit.Text := FormatFloat('0.0',Value);
end;

procedure TprPageParamsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

procedure TprPageParamsForm.PrinterChanged;
begin
prPrinter.PrinterIndex := EDPrinter.ItemIndex;
if not prPrinter.InitStructures or not prPrinter.InitInfo then
  begin
    MBError(Format(prLoadStr(sPrinterUnavailable),[prPrinter.PrinterName]));
    EDPrinter.ItemIndex := 0;
    prPrinter.PrinterIndex := 0;
    prPrinter.InitStructures;
    prPrinter.InitInfo;
  end;
EDScalePaperSize.Items.Assign(prPrinter.PaperNames);
EDScalePaperSize.Items.Add(prLoadStr(sOtherPageSize));
EDScalePaperSize.ItemIndex := 0;
EDScalePageWidthChange(nil);
EDPaperSize.Items.Assign(prPrinter.PaperNames);
EDPaperSize.Items.Add(prLoadStr(sOtherPageSize));
EDPaperSize.ItemIndex := 0;
EDPageWidthChange(nil);

EDLeftMin.Text := IntToStr(prPrinter.mmlMargin);
EDTopMin.Text := IntToStr(prPrinter.mmtMargin);
EDRightMin.Text := IntToStr(prPrinter.mmrMargin);
EDBottomMin.Text := IntToStr(prPrinter.mmbMargin);
end;

function TprPageParamsForm.EditOptions(_Report : TprReport;
                                       _PageInfo : TprPageInfo;
                                       _PageScaleInfo : TprPageScaleInfo;
                                       EnableDsgnSizes : boolean;
                                       var DsgnWidth,DsgnHeight : integer;
                                       EnableVisible : boolean;
                                       var Visible : boolean) : boolean;
var
  i : integer;
begin
fUpdated := true;
Report := _Report;
PageInfo := _PageInfo;
PageScaleInfo := _PageScaleInfo;

EDPrinter.Items.Assign(prPrinter.Printers);
EDPrinter.ItemIndex := Report.prPrinter.PrinterIndex;
PrinterChanged;

DsgnWidthDelta := DsgnWidth-PageInfo.PageWidth;
DsgnHeightDelta := DsgnHeight-PageInfo.PageHeight;
 
for i:=integer(Low(TprPageScaleMode)) to integer(High(TprPageScaleMode)) do
  EDScaleMode.Items.Add(prLoadStr(sPageScaleModeCaptionsOffset-i));

if (PageInfo.PaperSize=-1) or (prPrinter.GetPaperSizeIndex(PageInfo.PaperSize)=-1) then
  EDPaperSize.ItemIndex := EDPaperSize.Items.Count-1
else
  EDPaperSize.ItemIndex := prPrinter.GetPaperSizeIndex(PageInfo.PaperSize);
RBPortrait.Checked := PageInfo.Orientation=poPortrait;
RBLandscape.Checked := PageInfo.Orientation=poLandscape;
SetSize(EDPageWidth,PageInfo.PageWidth);
SetSize(EDPageHeight,PageInfo.PageHeight);

if (PageScaleInfo.PaperSize=-1) or (prPrinter.GetPaperSizeIndex(PageScaleInfo.PaperSize)=-1) then
  EDScalePaperSize.ItemIndex := EDScalePaperSize.Items.Count-1
else
  EDScalePaperSize.ItemIndex := prPrinter.GetPaperSizeIndex(PageScaleInfo.PaperSize);
RBScalePortrait.Checked := PageScaleInfo.Orientation=poPortrait;
RBScaleLandscape.Checked := PageScaleInfo.Orientation=poLandscape;
SetSize(EDScalePageWidth,PageScaleInfo.PageWidth);
SetSize(EDScalePageHeight,PageScaleInfo.PageHeight);

EDScaleMode.ItemIndex := integer(PageScaleInfo.ScaleMode);
CBFitObjects.Checked := PageScaleInfo.FitObjects;

SetMargin(EDLeft,PageInfo.lMargin);
SetMargin(EDTop,PageInfo.tMargin);
SetMargin(EDRight,PageInfo.rMargin);
SetMargin(EDBottom,PageInfo.bMargin);

if EnableDsgnSizes then
  begin
    SetSize(EDDsgnWidth,DsgnWidth);
    SetSize(EDDsgnHeight,DsgnHeight);
  end
else
  PDesign.TabVisible := false;

if EnableVisible then
  CBVisible.Checked := Visible
else
  CBVisible.Visible := false;

iPagePortrait.Visible := RBPortrait.Checked;
iPageLandscape.Visible := RBLandscape.Checked;
iScalePagePortrait.Visible := RBScalePortrait.Checked;
iScalePageLandscape.Visible := RBScaleLandscape.Checked;

if EDPaperSize.ItemIndex<>EDPaperSize.Items.Count-1 then
  begin
    if RBPortrait.Checked then
      begin
        SetSize(EDPageWidth,prPrinter.PaperWidths[prPrinter.PaperSizes[EDPaperSize.ItemIndex]]);
        SetSize(EDPageHeight,prPrinter.PaperHeights[prPrinter.PaperSizes[EDPaperSize.ItemIndex]]);
      end
    else
      if RBLandscape.Checked then
        begin
          SetSize(EDPageHeight,prPrinter.PaperWidths[prPrinter.PaperSizes[EDPaperSize.ItemIndex]]);
          SetSize(EDPageWidth,prPrinter.PaperHeights[prPrinter.PaperSizes[EDPaperSize.ItemIndex]]);
        end;
  end;
if EDScalePaperSize.ItemIndex<>EDScalePaperSize.Items.Count-1 then
  begin
    if RBScalePortrait.Checked then
      begin
        SetSize(EDScalePageWidth,prPrinter.PaperWidths[prPrinter.PaperSizes[EDScalePaperSize.ItemIndex]]);
        SetSize(EDScalePageHeight,prPrinter.PaperHeights[prPrinter.PaperSizes[EDScalePaperSize.ItemIndex]]);
      end
    else
      if RBScaleLandscape.Checked then
        begin
          SetSize(EDScalePageHeight,prPrinter.PaperWidths[prPrinter.PaperSizes[EDScalePaperSize.ItemIndex]]);
          SetSize(EDScalePageWidth,prPrinter.PaperHeights[prPrinter.PaperSizes[EDScalePaperSize.ItemIndex]]);
        end;
  end;

EDScaleModeClick(nil);

Result := ShowModal=mrOk;
if Result then
  begin
    if RBLandscape.Checked then
      PageInfo.Orientation := poLandscape
    else
      PageInfo.Orientation := poPortrait;
    if EDPaperSize.ItemIndex=EDPaperSize.Items.Count-1 then
      PageInfo.PaperSize := -1
    else
      PageInfo.PaperSize := prPrinter.PaperSizes[EDPaperSize.ItemIndex];
    PageInfo.PageWidth := GetSize(EDPageWidth,PageInfo.PageWidth);
    PageInfo.PageHeight := GetSize(EDPageHeight,PageInfo.PageHeight);

    if EnableDsgnSizes then
      begin
        DsgnWidth := GetSize(EDDsgnWidth,DsgnWidth);
        DsgnHeight := GetSize(EDDsgnHeight,DsgnHeight);
      end;

    if RBScaleLandscape.Checked then
      PageScaleInfo.Orientation := poLandscape
    else
      PageScaleInfo.Orientation := poPortrait;
    if EDScalePaperSize.ItemIndex=EDScalePaperSize.Items.Count-1 then
      PageScaleInfo.PaperSize := -1
    else
      PageScaleInfo.PaperSize := prPrinter.PaperSizes[EDScalePaperSize.ItemIndex];
    PageScaleInfo.PageWidth := GetSize(EDScalePageWidth,PageScaleInfo.PageWidth);
    PageScaleInfo.PageHeight := GetSize(EDScalePageHeight,PageScaleInfo.PageHeight);

    PageScaleInfo.ScaleMode := TprPageScaleMode(EDScaleMode.ItemIndex);
    PageScaleInfo.FitObjects := CBFitObjects.Checked;

    PageInfo.lMargin := GetMargin(EDLeft,PageInfo.lMargin);
    PageInfo.rMargin := GetMargin(EDRight,PageInfo.rMargin);
    PageInfo.tMargin := GetMargin(EDTop,PageInfo.tMargin);
    PageInfo.bMargin := GetMargin(EDBottom,PageInfo.bMargin);

    if EnableVisible then
      Visible := CBVisible.Checked; 
  end;
end;

procedure TprPageParamsForm.bOKClick(Sender: TObject);
begin
if (GetMargin(EDLeft,prPrinter.mmlMargin)<prPrinter.mmlMargin) or
   (GetMargin(EDRight,prPrinter.mmrMargin)<prPrinter.mmrMargin) or
   (GetMargin(EDTop,prPrinter.mmtMargin)<prPrinter.mmtMargin) or
   (GetMargin(EDBottom,prPrinter.mmbMargin)<prPrinter.mmbMargin) then
  begin
    if MBox(prLoadStr(sPageMarginsWarning),prLoadStr(sAttention),MB_YESNO+MB_ICONEXCLAMATION)=IDNO then
      exit;
  end;
ModalResult := mrOk;
end;

procedure TprPageParamsForm.FormCreate(Sender: TObject);
begin
prPrinter := TprPrinter.Create;
end;

procedure TprPageParamsForm.FormDestroy(Sender: TObject);
begin
prPrinter.Free;
end;

procedure TprPageParamsForm.EDPrinterClick(Sender: TObject);
begin
PrinterChanged;
end;

procedure TprPageParamsForm.EDScaleModeClick(Sender: TObject);
begin
Label16.Enabled := EDScaleMode.ItemIndex=1;
Label13.Enabled := Label16.Enabled;
EDScalePaperSize.Enabled := Label16.Enabled;
Label14.Enabled := Label16.Enabled;
Label15.Enabled := Label16.Enabled;
EDScalePageWidth.Enabled := Label16.Enabled;
EDScalePageHeight.Enabled := Label16.Enabled;
RBScalePortrait.Enabled := Label16.Enabled;
RBScaleLandscape.Enabled := Label16.Enabled;
end;

procedure TprPageParamsForm.UpdateDsgnSizes;
begin
SetSize(EDDsgnWidth,GetSize(EDPageWidth,0)+DsgnWidthDelta);
SetSize(EDDsgnHeight,GetSize(EDPageHeight,0)+DsgnHeightDelta);
end;

procedure TprPageParamsForm.UpdateRB(RBPortrait,RBLandscape : TRadioButton; iPortrait,iLandscape : TImage; EDPaperSize : TComboBox; EDPageWidth,EDPageHeight : TEdit);
var
  dummy : integer;
begin
if FUpdated then exit;
iPortrait.Visible := RBPortrait.Checked;
iLandscape.Visible := RBLandscape.Checked;
fUpdated := true;
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
        SetSize(EDPageWidth,prPrinter.PaperWidths[prPrinter.PaperSizes[EDPaperSize.ItemIndex]]);
        Setsize(EDPageHeight,prPrinter.PaperHeights[prPrinter.PaperSizes[EDPaperSize.ItemIndex]]);
      end
    else
      if RBLandscape.Checked then
        begin
          SetSize(EDPageHeight,prPrinter.PaperWidths[prPrinter.PaperSizes[EDPaperSize.ItemIndex]]);
          SetSize(EDPageWidth,prPrinter.PaperHeights[prPrinter.PaperSizes[EDPaperSize.ItemIndex]]);
        end;
  end;
fUpdated := false;
end;

procedure TprPageParamsForm.UpdateEDPaperSize(RBPortrait,RBLandscape : TRadioButton; EDPaperSize : TComboBox; EDPageWidth,EDPageHeight : TEdit);
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
    SetSize(EDPageWidth,prPrinter.PaperWidths[prPrinter.PaperSizes[i]]);
    Setsize(EDPageHeight,prPrinter.PaperHeights[prPrinter.PaperSizes[i]]);
  end
else
  if RBLandscape.Checked then
    begin
      SetSize(EDPageHeight,prPrinter.PaperWidths[prPrinter.PaperSizes[i]]);
      SetSize(EDPageWidth,prPrinter.PaperHeights[prPrinter.PaperSizes[i]]);
    end;
fUpdated := false;
end;

procedure TprPageParamsForm.UpdateEDSizes(RBPortrait,RBLandscape : TRadioButton; iPortrait,iLandscape : TImage; EDPaperSize : TComboBox; EDPageWidth,EDPageHeight : TEdit);
var
  PaperSize : integer;
  Orientation : TprPrinterOrientation;
begin
if FUpdated then exit;
fUpdated := true;
if prPrinter.FindPaperSize(GetSize(EDPageWidth,0),GetSize(EDPageHeight,0),PaperSize,Orientation,true) then
  begin
    EDPaperSize.ItemIndex := prPrinter.GetPaperSizeIndex(PaperSize);
    RBPortrait.Checked := Orientation=poPortrait;
    RBLandscape.Checked := Orientation=poLandscape;
    iPortrait.Visible := RBPortrait.Checked;
    iLandscape.Visible := RBLandscape.Checked;
  end
else
  EDPaperSize.ItemIndex := EDPaperSize.Items.Count-1;
fUpdated := false;
end;

procedure TprPageParamsForm.RBPortraitClick(Sender: TObject);
begin
UpdateRB(RBPortrait,RBLandscape,iPagePortrait,iPageLandscape,EDPaperSize,EDPageWidth,EDPageHeight);
UpdateDsgnSizes;
end;

procedure TprPageParamsForm.RBScalePortraitClick(Sender: TObject);
begin
UpdateRB(RBScalePortrait,RBScaleLandscape,iScalePagePortrait,iScalePageLandscape,EDScalePaperSize,EDScalePageWidth,EDScalePageHeight);
end;

procedure TprPageParamsForm.EDPaperSizeClick(Sender: TObject);
begin
UpdateEDPaperSize(RBPortrait,RBLandscape,EDPaperSize,EDPageWidth,EDPageHeight);
UpdateDsgnSizes;
end;

procedure TprPageParamsForm.EDScalePaperSizeClick(Sender: TObject);
begin
UpdateEDPaperSize(RBScalePortrait,RBScaleLandscape,EDScalePaperSize,EDScalePageWidth,EDScalePageHeight);
end;

procedure TprPageParamsForm.EDPageWidthChange(Sender: TObject);
begin
UpdateEDSizes(RBPortrait,RBLandscape,iPagePortrait,iPageLandscape,EDPaperSize,EDPageWidth,EDPageHeight);
UpdateDsgnSizes;
end;

procedure TprPageParamsForm.EDScalePageWidthChange(Sender: TObject);
begin
UpdateEDSizes(RBScalePortrait,RBScaleLandscape,iScalePagePortrait,iScalePageLandscape,EDScalePaperSize,EDScalePageWidth,EDScalePageHeight);
end;

procedure TprPageParamsForm.FormShow(Sender: TObject);
begin
fUpdated := false;
end;

procedure TprPageParamsForm.EDDsgnWidthChange(Sender: TObject);
begin
DsgnWidthDelta := GetSize(EDDsgnWidth,GetSize(EDPageWidth,0))-GetSize(EDPageWidth,0);
DsgnHeightDelta := GetSize(EDDsgnHeight,GetSize(EDPageHeight,0))-GetSize(EDPageHeight,0);
end;

procedure TprPageParamsForm.EDPageWidthKeyPress(Sender: TObject;
  var Key: Char);
begin
if not (Key in [',','.','0'..'9',DecimalSeparator,#8]) then
  Key := #0;
end;

end.
