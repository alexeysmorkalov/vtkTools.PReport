unit Save_Form;

{$I pr.inc}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, pr_Common, pr_Classes, pr_TxClasses;

type
  TSaveForm = class(TForm)
    Label1: TLabel;
    edReportType: TEdit;
    edTemplateName: TEdit;
    Label2: TLabel;
    cbBinaryMode: TCheckBox;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
    function Save(AReport: TprCustomReport;
                         var ATemplateName: string): Boolean;
  end;

var
  SaveForm: TSaveForm;

implementation

uses Unit1, DB;

{$R *.dfm}

function TSaveForm.Save(AReport: TprCustomReport;
                         var ATemplateName: string): Boolean;
var
  ms: TMemoryStream;
begin
  edReportType.Text := AReport.ClassName;
  edTemplateName.Text := ATemplateName;
  Result := ShowModal = mrOk;
  if Result then
  begin
    ATemplateName := edTemplateName.Text;

    // append a new record
    with Form1.TemplatesTable do
    begin
      if Locate('NAME', ATemplateName, [loCaseInsensitive]) then
        Edit
      else
        Append;

      FieldByName('NAME').AsString := ATemplateName;
      if AReport is TprTxReport then
        FieldByName('REP_TYPE').AsString := 'D'
      else
        FieldByName('REP_TYPE').AsString := 'W';
      FieldByName('BINARY').AsBoolean := cbBinaryMode.Checked;

      ms := TMemoryStream.Create;
      try
        AReport.SaveTemplate(ms, cbBinaryMode.Checked);
        ms.Seek(0, soFromBeginning);
        TBlobField(FieldByName('DATA')).LoadFromStream(ms);
      finally
        ms.Free;
      end;

      Post;
    end;
  end;
end;

end.
