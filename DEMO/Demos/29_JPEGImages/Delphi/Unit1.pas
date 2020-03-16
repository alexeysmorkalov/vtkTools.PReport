unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  jpeg, pr_Common, pr_Classes, DB, DBTables, StdCtrls, Grids,
  DBGrids, ExtCtrls, pr_Designer;

type
  TForm1 = class(TForm)
    Table: TTable;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    DataSource: TDataSource;
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    Label2: TLabel;
    prReport1: TprReport;
    procedure TableAfterScroll(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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


{$R *.DFM}

procedure TForm1.TableAfterScroll(DataSet: TDataSet);
var
  ms: TMemoryStream;
  img: TJPEGImage;
begin
  ms := TMemoryStream.Create;
  TBlobField(Table.FieldByName('IMAGE')).SaveToStream(ms);
  ms.Seek(0, soFromBeginning);
  img := TJPEGImage.Create;
  img.LoadFromStream(ms);
  image1.Picture.Assign(img);
  img.Free;
  ms.Free;

  ClientHeight := image1.Top + image1.Height + 10;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Table.DatabaseName := ExtractFilePath(Application.ExeName);
  Table.TableName := 'data.db';
  Table.Open;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  prReport1.PrepareReport;
  prReport1.PreviewPreparedReport(True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  prReport1.DesignReport(True);
end;

procedure TForm1.prReport1FirstPassObject(Sender: TObject; Obj: TprObj;
  var ManuallyProcessed: Boolean);
var
  ms: TMemoryStream;
  img: TJPEGImage;
begin           
  if Obj.Name = 'prImageObj1' then
  begin
    with TprImageObjRecVersion(Obj.GenCurVersion) do
    begin
      ms := TMemoryStream.Create;
      TBlobField(Table.FieldByName('IMAGE')).SaveToStream(ms);
      ms.Seek(0, soFromBeginning);
      img := TJPEGImage.Create;
      img.LoadFromStream(ms);
      Picture.Assign(img);
      img.Free;
      ms.Free;
      ManuallyProcessed := True;
    end;
  end;
end;

end.
