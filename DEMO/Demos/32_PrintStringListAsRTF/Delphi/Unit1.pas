unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, pr_Common, pr_Classes, pr_Dataset, pr_ArrayDataset,
  pr_Parser, pr_Designer, ExtCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    edFileName: TEdit;
    Button1: TButton;
    Button2: TButton;
    prReport: TprReport;
    prArrayDataset1: TprArrayDataset;
    rgPrintWith: TRadioGroup;
    procedure prReportUnknownVariable(Sender: TObject;
      const VarName: String; var Value: TprVarValue;
      var IsProcessed: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure prReportFirstPassObject(Sender: TObject; Obj: TprObj;
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

procedure TForm1.prReportUnknownVariable(Sender: TObject;
  const VarName: String; var Value: TprVarValue; var IsProcessed: Boolean);
var
  L: TStringList;
begin
  if (VarName = 'MyRTFVar') then
  begin
    // process variable only if user selects printing with OnUnknownVariable event
    if rgPrintWith.ItemIndex = 1 then
    begin
      // Create a TStringList object.
      L := TStringList.Create;
      try
        // Load from file
        L.LoadFromFile(edFileName.Text);
        // Pass the loaded text to the report
        _vSetAsString(Value, L.Text);
      finally
        L.Free;
      end;
    end;
    IsProcessed := true;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  prReport.DesignReport(True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  prReport.PrepareReport;
  prReport.PreviewPreparedReport(True);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  rgPrintWith.ItemIndex := 0;
end;

procedure TForm1.prReportFirstPassObject(Sender: TObject; Obj: TprObj;
  var ManuallyProcessed: Boolean);
var
  L: TStringList;
begin
  if (rgPrintWith.ItemIndex = 0) and (Obj.Name = 'prRichObj1') then
  begin
    // Create a TStringList object.
    L := TStringList.Create;
    try
      // Load from file
      L.LoadFromFile(edFileName.Text);
      // Set RTF text to object
      TprRichObjRecVersion(Obj.GenCurVersion).SetRtf(L.Text);
      ManuallyProcessed := true;
    finally
      L.Free;
    end;
  end;
end;

end.
