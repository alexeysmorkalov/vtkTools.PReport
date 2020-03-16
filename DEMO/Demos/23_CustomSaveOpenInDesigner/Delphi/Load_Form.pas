unit Load_Form;

{$I pr.inc}
interface

uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, pr_Common, pr_TxClasses, pr_Classes,
  DB, DBTables;

type
  TLoadForm = class(TForm)
    Label1: TLabel;
    DBGrid: TDBGrid;
    Button1: TButton;
    Button2: TButton;
    DataSource1: TDataSource;
    Query1: TQuery;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function Load(AReport: TprCustomReport;
                  var ATemplateName: string): Boolean;
  end;

var
  LoadForm: TLoadForm;

implementation

uses Unit1;

{$R *.dfm}

function TLoadForm.Load(AReport: TprCustomReport;
                        var ATemplateName: string): Boolean;
var
  ms: TMemoryStream;
begin
  ActiveControl := DBGrid;
  Button1.Enabled := not Form1.TemplatesTable.IsEmpty;

  Query1.Close;
  if AReport is TprReport then
    Query1.SQL.Text := 'select * from templates where rep_type = "W"'
  else
    Query1.SQL.Text := 'select * from templates where rep_type = "D"';
  Query1.Open;

  Result := ShowModal = mrOk;
  if Result then
  begin
    with Query1 do
    begin
      ATemplateName := FieldByName('NAME').AsString;

      ms := TMemoryStream.Create;
      try
        TBlobField(FieldByName('DATA')).SaveToStream(ms);
        ms.Seek(0, soFromBeginning);
        AReport.LoadTemplate(ms, FieldByName('BINARY').AsBoolean);
      finally
        ms.Free;
      end;
    end;
  end;
  Query1.Close;
end;

procedure TLoadForm.FormCreate(Sender: TObject);
begin
  Query1.DatabaseName := ExtractFilePath(ParamStr(0));
end;

end.
