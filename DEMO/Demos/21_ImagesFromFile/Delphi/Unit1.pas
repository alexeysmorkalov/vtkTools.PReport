unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, DBTables, pr_Common, pr_Classes,
  pr_Designer;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Table: TTable;
    prReport1: TprReport;
    procedure FormCreate(Sender: TObject);
    procedure prReport1FirstPassObject(Sender: TObject; Obj: TprObj;
      var ManuallyProcessed: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure FillImagesTable;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FillImagesTable;
var
  buf: array [0..255] of char;
  dir: string;
  AData: TSearchRec;
begin
  Table.DatabaseName := ExtractFilePath(ParamStr(0));
  Table.TableName := 'images.dbf';
  Table.EmptyTable;
  Table.Open;

  GetWindowsDirectory(buf, 255);
  dir := ExcludeTrailingBackslash(StrPas(buf)) + '\';
  if FindFirst(dir + '*.bmp', faAnyFile, AData) = 0 then
  begin
    Table.Append;
    Table.FieldByName('FILENAME').AsString := dir + AData.Name;
    Table.Post;

    while FindNext(AData) = 0 do
    begin
      Table.Append;
      Table.FieldByName('FILENAME').AsString := dir + AData.Name;
      Table.Post;
    end;
  end;
  SysUtils.FindClose(AData);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //
end;

procedure TForm1.prReport1FirstPassObject(Sender: TObject; Obj: TprObj;
  var ManuallyProcessed: Boolean);
begin
  if Obj.Name = 'prImageObj1' then
  begin
    ManuallyProcessed := True;
    try
      TprImageObj(Obj).GenCurVersion.Picture.LoadFromFile(Table.FieldByName('FILENAME').AsString);
    except
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FillImagesTable;
  Button3.Enabled := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  prReport1.DesignReport(True);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  prReport1.PrepareReport;
  prReport1.PreviewPreparedReport(True);
end;

end.
