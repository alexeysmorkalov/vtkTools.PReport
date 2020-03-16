{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxReportParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,

  pr_Common, pr_Txclasses, pr_MultiLang, pr_TxConsts, pr_Strings;

type
  TprTxReportParamsForm = class(TprForm)
    PC: TPageControl;
    TabSheet1: TTabSheet;
    Label2: TLabel;
    EDTitle: TEdit;
    bOK: TButton;
    bCancel: TButton;
    prMLRes1: TprMLRes;
    Label1: TLabel;
    edRecodeTable: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function EditParams(Report : TprTxReport) : boolean;
  end;

implementation

{$R *.DFM}

procedure TprTxReportParamsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action:=caFree;
end;

function TprTxReportParamsForm.EditParams;
var
  I: Integer;
begin
  EDTitle.Text := Report.Title;
  for I := 0 to edRecodeTable.Items.Count - 1 do
    if edRecodeTable.Items.Objects[I] <> nil then
      with (TprTxRecodeTable(edRecodeTable.Items.Objects[I])) do
        if Report.RecodeTableName = RecodeTableName then
        begin
          edRecodeTable.ItemIndex := I;
          break;
        end;
  if edRecodeTable.ItemIndex < 0 then
    edRecodeTable.ItemIndex := 0;

  Result := ShowModal=mrOk;
  if Result then
  begin
    Report.Title := EDTitle.Text;
    if edRecodeTable.ItemIndex <= 0 then
      Report.RecodeTableName := ''
    else
      Report.RecodeTableName := TprTxRecodeTable(edRecodeTable.Items.Objects[edRecodeTable.ItemIndex]).RecodeTableName;
  end;
end;

procedure TprTxReportParamsForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // fill edRecodeTable
  edRecodeTable.Items.AddObject(prLoadStr(NullPointerString), nil);
  for I := 0 to TxReportOptions.TxRecodeTablesCount - 1 do
    edRecodeTable.Items.AddObject(TxReportOptions.RecodeTables[I].Description, TxReportOptions.RecodeTables[I]);
end;

end.
