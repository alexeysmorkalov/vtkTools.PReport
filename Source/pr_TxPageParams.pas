{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxPageParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  typinfo, StdCtrls, ComCtrls, ExtCtrls, CheckLst,

  pr_Common, pr_TxClasses, pr_TxConsts, pr_MultiLang;

type
  TprTxPageParamsForm = class(TprForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EDPageType: TComboBox;
    EDColNum: TEdit;
    EDLineNum: TEdit;
    UDColNum: TUpDown;
    UDLineNum: TUpDown;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    EDDefTxFontStyle: TComboBox;
    Label5: TLabel;
    EDDefTxFontOptions: TCheckListBox;
    prMLRes1: TprMLRes;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    Label6: TLabel;
    Label7: TLabel;
    EDDsgnColNum: TEdit;
    EDDsgnLineNum: TEdit;
    UDDsgnLineNum: TUpDown;
    UDDsgnColNum: TUpDown;
    Label8: TLabel;
    Label9: TLabel;
    CBVisible: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EDLineNumChange(Sender: TObject);
    procedure EDDsgnColNumChange(Sender: TObject);
  private
    { Private declarations }
    DsgnColNumDelta : integer;
    DsgnLineNumDelta : integer;
  public
    { Public declarations }
    function EditOptions(Page : TprTxPage) : boolean;
  end;

implementation

uses
  vgr_Functions;
  
{$R *.DFM}

procedure TprTxPageParamsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
ACtion := caFree;
end;

function TprTxPageParamsForm.EditOptions;
var
  i : integer;
begin
GetEnumNamesToStrings(TypeInfo(TprTxPageType), EDPageType.Items);

for i:=0 to TxReportOptions.TxFontStylesCount-1 do
  EDDefTxFontStyle.Items.AddObject(TxReportOptions.TxFontStyles[i].Description,TxReportOptions.TxFontStyles[i]);
for i:=0 to TxReportOptions.TxFontOptionsCount-1 do
  begin
    EDDefTxFontOptions.Items.AddObject(TxReportOptions.TxFontOptions[i].Description,TxReportOptions.TxFontOptions[i]);
    EDDefTxFontOptions.Checked[i] := Page.DefTxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[i]);
  end;
EDDefTxFontStyle.ItemIndex := EDDefTxFontStyle.Items.IndexOfObject(Page.DefTxFontStyleEx);

EDPageType.ItemIndex := integer(Page.PageType);
UDColNum.Position := Page.ColNum;
UDLineNum.Position := Page.LineNum;

UDDsgnColNum.Position := Page.DsgnColNum;
UDDsgnLineNum.Position := Page.DsgnLineNum;

DsgnColNumDelta := Page.DsgnColNum-Page.ColNum;
DsgnLineNumDelta := Page.DsgnLineNum-Page.LineNum;

CBVisible.Checked := Page.Visible;

Result := ShowModal=mrOk;
if Result then
  begin
    Page.PageType := TprTxPageType(EDPageType.ItemIndex);
    Page.ColNum := UDColNum.Position;
    Page.LineNum := UDLineNum.Position;
    Page.DsgnColNum := UDDsgnColNum.Position;
    Page.DsgnLineNum := UDDsgnLineNum.Position;
    if EDDefTxFontStyle.ItemIndex>=0 then
      Page.DefTxFontStyleEx := TprTxFontStyle(EDDefTxFontStyle.Items.Objects[EDDefTxFontStyle.ItemIndex]);
    Page.DefTxFontOptionsEx.Clear;
    for i:=0 to TxReportOptions.TxFontOptionsCount-1 do
      if EDDefTxFontOptions.Checked[i] then
        Page.DefTxFontOptionsEx.Add(TprTxFontOption(EDDefTxFontOptions.Items.Objects[i]));
    Page.Visible := CBVisible.Checked;
  end;
end;

procedure TprTxPageParamsForm.EDLineNumChange(Sender: TObject);
begin
UDDsgnColNum.Position := StrToIntDef(EDColNum.Text,0)+DsgnColNumDelta;
UDDsgnLineNum.Position := StrToIntDef(EDLineNum.Text,0)+DsgnLineNumDelta;
end;

procedure TprTxPageParamsForm.EDDsgnColNumChange(Sender: TObject);
begin
DsgnColNumDelta := StrToIntDef(EDDsgnColNum.Text,UDColNum.Position)-UDColNum.Position;
DsgnLineNumDelta := StrToIntDef(EDDsgnLineNum.Text,UDLineNum.Position)-UDLineNum.Position;
end;

end.
