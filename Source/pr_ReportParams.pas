{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_ReportParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls,

  pr_Common, pr_classes, pr_MultiLang;

type
  TprReportParamsForm = class(TprForm)
    PC: TPageControl;
    TabSheet1: TTabSheet;
    bOK: TButton;
    bCancel: TButton;
    Label1: TLabel;
    EDPrinter: TComboBox;
    Label2: TLabel;
    EDTitle: TEdit;
    prMLRes1: TprMLRes;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bOKClick(Sender: TObject);
  private
    { Private declarations }
    Report : TprReport;
  public
    { Public declarations }
    function EditParams(_Report : TprReport) : boolean;
  end;

implementation

{$R *.DFM}

procedure TprReportParamsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

function TprReportParamsForm.EditParams;
begin
Report := _Report;

EDPrinter.Items.Assign(Report.prPrinter.Printers);
EDPrinter.ItemIndex := Report.prPrinter.PrinterIndex;
EDTitle.Text := Report.Title;
Result := ShowModal=mrOk;
end;

procedure TprReportParamsForm.bOKClick(Sender: TObject);
begin
Report.prPrinter.PrinterIndex := EDPrinter.ItemIndex;
Report.Title := EDTitle.Text;
end;

end.
