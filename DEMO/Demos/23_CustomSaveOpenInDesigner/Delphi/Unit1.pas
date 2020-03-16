unit Unit1;

{$I pr.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, DB, DBTables, pr_TxClasses, pr_Common,
  pr_Classes, pr_Designer, pr_TxDesigner, pr_CommonDesigner;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    DBGrid: TDBGrid;
    Label1: TLabel;
    TemplatesTable: TTable;
    bNew: TButton;
    bDesign: TButton;
    prReport1: TprReport;
    prTxReport1: TprTxReport;
    customers: TTable;
    dsTemplates: TDataSource;
    Button1: TButton;
    Button2: TButton;
    procedure prReport1DesignerSaveTemplate(Sender: TObject;
      var FileName: String; var Stream: TStream; var CancelSave,
      IsBinaryFormat: Boolean; IsSaveAs: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure bNewClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure bDesignClick(Sender: TObject);
    procedure prReport1CreateDesigner(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure prReport1DesignerOpenTemplate(Sender: TObject;
      var FileName: String; var Stream: TStream; var CancelOpen,
      IsBinaryFormat: Boolean);
  private
    { Private declarations }
    FTemplate: string;
  public
    { Public declarations }
    procedure DesignNewTprReport;
    procedure DesignNewTprTxReport;
    procedure DesignCurrentReport;
  end;

var
  Form1: TForm1;

implementation

uses Save_Form, Load_Form;

{$R *.dfm}

procedure TForm1.DesignNewTprReport;
begin
  FTemplate := '';
  prReport1.ClearTemplate;
  prReport1.DesignReport(True);
end;

procedure TForm1.DesignNewTprTxReport;
begin
  FTemplate := '';
  prTxReport1.ClearTemplate;
  prTxReport1.DesignReport(True);
end;

procedure TForm1.DesignCurrentReport;
begin
  prReport1.DesignReport(True);
end;

procedure TForm1.prReport1DesignerOpenTemplate(Sender: TObject;
  var FileName: String; var Stream: TStream; var CancelOpen,
  IsBinaryFormat: Boolean);
begin
  // show Open dialog
  if LoadForm.Load(Sender as TprCustomReport, FileName) then
    TprCustomDesignerForm(TprCustomReport(Sender).DesignerForm).FileName := FileName;
  // cancel default processing
  CancelOpen := True;
end;

procedure TForm1.prReport1DesignerSaveTemplate(Sender: TObject;
  var FileName: String; var Stream: TStream; var CancelSave,
  IsBinaryFormat: Boolean; IsSaveAs: Boolean);
begin
  // show Save dialog
  if SaveForm.Save(Sender as TprCustomReport, FileName) then
    TprCustomDesignerForm(TprCustomReport(Sender).DesignerForm).FileName := FileName;
  // cancel default processing
  CancelSave := True;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TemplatesTable.DatabaseName := ExtractFilePath(ParamStr(0));
  TemplatesTable.TableName := 'templates.dbf';
  TemplatesTable.Open;
end;

procedure TForm1.bNewClick(Sender: TObject);
begin
  ActiveControl := DBGrid;
  DesignNewTprReport;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ActiveControl := DBGrid;
  DesignNewTprTxReport;
end;

procedure TForm1.bDesignClick(Sender: TObject);
var
  AReport: TprCustomReport;
  ms: TMemoryStream;
begin
  ActiveControl := DBGrid;
  if TemplatesTable.IsEmpty then
  begin
    Application.MessageBox('The templates'' list is empty.', 'Error', MB_ICONERROR + MB_OK);
    exit;
  end;

  FTemplate := TemplatesTable.FieldByName('NAME').AsString;
  if TemplatesTable.FieldByName('REP_TYPE').AsString = 'W' then
    AReport := prReport1
  else
    AReport := prTxReport1;

  ms := TMemoryStream.Create;
  try
    TBlobField(TemplatesTable.FieldByName('DATA')).SaveToStream(ms);
    ms.Seek(0, soFromBeginning);
    AReport.LoadTemplate(ms, TemplatesTable.FieldByName('BINARY').AsBoolean);
  finally
    ms.Free;
  end;
  AReport.DesignReport(True);
end;

procedure TForm1.prReport1CreateDesigner(Sender: TObject);
begin
  // Assign current template name to the designer form
  TprCustomDesignerForm(TprCustomReport(Sender).DesignerForm).FileName := FTemplate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ActiveControl := DBGrid;
  if TemplatesTable.IsEmpty then
  begin
    Application.MessageBox('The templates'' list is empty.', 'Error', MB_ICONERROR + MB_OK);
    exit;
  end;

  TemplatesTable.Delete;
end;

end.
