unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, vgr_ColorButton, pr_Common, pr_Classes, Db, DBTables;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    bBackColor: TvgrColorButton;
    bTextColor: TvgrColorButton;
    Label5: TLabel;
    prReport1: TprReport;
    Table1: TTable;
    edName: TComboBox;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure prReport1FirstPassObject(Sender: TObject; Obj: TprObj;
      var ManuallyProcessed: Boolean);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FOnFirstPassObjectUsed: Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button2Click(Sender: TObject);
var
  AMemoObj: TprMemoObj;
begin
  AMemoObj := TprMemoObj(prReport1.FindObject(edName.Text));
  if not (AMemoObj is TprMemoObj) then
  begin
    ShowMessage(Format('Object [%s] not found', [edName.Text]));
    exit;
  end;

  with AMemoObj.DefVersion do
  begin
    FillColor := bBackColor.SelectedColor;
    Font.Color := bTextColor.SelectedColor;
  end;

  FOnFirstPassObjectUsed := False;
  prReport1.PrepareReport;
  prReport1.PreviewPreparedReport(True);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to prReport1.ObjectCount - 1 do
    if prReport1.Objects[I] is TprMemoObj then
      edName.Items.Add(Format('%s', [prReport1.Objects[I].Name, TprMemoObj(prReport1.Objects[I]).DefVersion.Memo.Text]));
  if edName.Items.Count > 0 then
    edName.Text := edName.Items[0];
end;

procedure TForm1.prReport1FirstPassObject(Sender: TObject; Obj: TprObj;
  var ManuallyProcessed: Boolean);
begin
  if (Obj.Name = edName.Text) and (Obj is TprMemoObj) and FOnFirstPassObjectUsed then
    with TprMemoObj(Obj).GenCurVersion do
    begin
      FillColor := bBackColor.SelectedColor;
      Font.Color := bTextColor.SelectedColor;
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FOnFirstPassObjectUsed := True;
  prReport1.PrepareReport;
  prReport1.PreviewPreparedReport(True);
end;

end.
