unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, pr_Common, pr_Classes, pr_Dataset,
  pr_ArrayDataset, pr_Designer;

type
  TForm1 = class(TForm)
    bPreview: TButton;
    Label1: TLabel;
    Button2: TButton;
    Label2: TLabel;
    edFileName: TEdit;
    SpeedButton1: TSpeedButton;
    OpenDialog: TOpenDialog;
    RtfViewerTemplate: TprReport;
    prArrayDataset1: TprArrayDataset;
    procedure SpeedButton1Click(Sender: TObject);
    procedure RtfViewerTemplateFirstPassObject(Sender: TObject;
      Obj: TprObj; var ManuallyProcessed: Boolean);
    procedure edFileNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bPreviewClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  OpenDialog.FileName := edFileName.Text;
  if OpenDialog.Execute then
  begin
    edFileName.Text := OpenDialog.FileName;
  end;
end;

procedure TForm1.RtfViewerTemplateFirstPassObject(Sender: TObject;
  Obj: TprObj; var ManuallyProcessed: Boolean);
begin
  if Obj.Name = 'prRichObj1' then
  begin
    with TprRichObj(Obj).GenCurVersion do
    begin
      LoadRichTextFromFile(hwndRich, edFileName.Text);

      SaveRichTextToFile(hwndRich, 'c:\temp\qqq.rtf');
    end;
    ManuallyProcessed := True;
  end;
end;

procedure TForm1.edFileNameChange(Sender: TObject);
begin
  bPreview.Enabled := FileExists(edFileName.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  edFileNameChange(nil);
end;

procedure TForm1.bPreviewClick(Sender: TObject);
begin
  RtfViewerTemplate.Variables.ByName['FileName'].AsString := edFileName.Text;

  RtfViewerTemplate.PrepareReport;
  RtfViewerTemplate.PreviewPreparedReport(True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  RtfViewerTemplate.DesignReport(True);
end;

end.
