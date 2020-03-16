unit Unit1;

interface

{$I vtk.inc}
uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, pr_Common, pr_Classes;

type
  TForm1 = class(TForm)
    prReport1: TprReport;
    RichEdit1: TRichEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure prReport1FirstPassObject(Sender: TObject; Obj: TprObj;
      var ManuallyProcessed: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  RichEdit1.Lines.LoadFromFile('..\richtext.rtf');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  prReport1.PrepareReport;
  prReport1.PreviewPreparedReport(True);
end;

procedure TForm1.prReport1FirstPassObject(Sender: TObject; Obj: TprObj;
  var ManuallyProcessed: Boolean);
begin
  with TprRichObjRecVersion(Obj.GenCurVersion) do
  begin
    CopyRichText(richEdit1, hwndRich);
    ManuallyProcessed := True
  end;
end;

end.
