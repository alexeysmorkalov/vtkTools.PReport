unit Unit1;

interface

{$I pr.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, pr_Common, pr_Classes, DB, DBTables,

  pr_Designer, pr_Parser;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo2: TMemo;
    prReport: TprReport;
    Query: TQuery;
    Button2: TButton;
    Memo: TMemo;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure prReportUnknownVariable(Sender: TObject;
      const VarName: String; var Value: TprVarValue;
      var IsProcessed: Boolean);
    procedure prReportBandGenerateCell(Sender: TObject;
      HorzBandInfo: TprGenHorzBandInfo; VertBandInfo: TprGenVertBandInfo);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
begin
  prReport.DesignReport(True);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  prReport.PrepareReport;
  prReport.PreviewPreparedReport(True);
end;

procedure TForm1.prReportUnknownVariable(Sender: TObject;
  const VarName: String; var Value: TprVarValue; var IsProcessed: Boolean);
begin
  if VarName = 'MyReportSum' then
  begin
    _vSetAsVariant(Value, prReport.Values.ByName['ReportSum'].Value);
    IsProcessed := True;
  end
  else if VarName = 'MyGroupSum' then
  begin
    _vSetAsVariant(Value, prReport.Values.ByName['GroupSum'].Value);
    IsProcessed := True;
  end;
end;

procedure TForm1.prReportBandGenerateCell(Sender: TObject;
  HorzBandInfo: TprGenHorzBandInfo; VertBandInfo: TprGenVertBandInfo);
begin
  if HorzBandInfo.Band.Name = 'GroupFooter' then
  begin
    Memo.Lines.Add(Format('Group: [%s]; Sum: [%s]', [Query.FieldByName('Company').AsString, prReport.Values.ByName['GroupSum'].Value]));
  end
  else if HorzBandInfo.Band.Name = 'ReportFooter' then
  begin
    Memo.Lines.Add(Format('Report sum: [%s]', [prReport.Values.ByName['ReportSum'].Value]));
  end;
end;

end.
 