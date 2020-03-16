unit Unit1;

interface

{$i pr.inc}

uses
  Windows, Messages, {$IFDEF PR_D6_D7}Variants, {$ENDIF}SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, pr_Common, pr_Classes, DB, DBTables, Math,
  vgr_BarCode, typinfo, pr_BarCodeObj,
  vgr_Functions, pr_Designer;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    edCode: TEdit;
    Label2: TLabel;
    RepDefined: TprReport;
    RepDB: TprReport;
    Label3: TLabel;
    edTypeDefined: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
    edTypeDB: TComboBox;
    Button3: TButton;
    Button4: TButton;
    Table: TTable;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
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
var
  I, J: Integer;
  N: string;
begin
  // prepare table.
  Table.DatabaseName := ExtractFilePath(Application.ExeName);
  Table.TableName := 'data.dbf';
  Table.Open;
  if Table.RecordCount = 0 then
  begin
    // fill table with random values
    for I := 1 to 100 do
    begin
      Table.Append;
      Table['NAME'] := Format('Position%d', [I]);
      Table['COST'] := RoundEx(Random * 100, 2);
      // generate random number consisting of 8 digits
      N := '';
      for J := 1 to 8 do
        N := N + chr(48 + Round(Random(10)));
      Table['BARCODE'] := N;
      Table.Post;
    end;
  end;

  // fill edTypeDefined and edTypeDB with values
  for I := Integer(Low(TvgrBarCodeType)) to Integer(High(TvgrBarCodeType)) do
  begin
    edTypeDB.Items.Add(GetEnumName(TypeInfo(TvgrBarCodeType), I));
    edTypeDefined.Items.Add(GetEnumName(TypeInfo(TvgrBarCodeType), I));
  end;
  edTypeDB.ItemIndex := Integer(bcCode_2_5_interleaved);
  edTypeDefined.ItemIndex := Integer(bcCode_2_5_interleaved);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  RepDefined.DesignReport(True);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  temp: TvgrBarcode;
  I: Integer;
begin
  // Check the entered code and type
  temp := TvgrBarcode.Create;
  try
    temp.Text := edCode.Text;
    temp.Typ := TvgrBarCodeType(edTypeDefined.ItemIndex);
    if (edCode.Text ='') or (not temp.Checkbarcode) then
    begin
      ShowMessage(Format('Barcode [%s] can not be displayed with type [%s].', [edCode.Text, GetEnumName(TypeInfo(TvgrBarCodeType), edTypeDefined.ItemIndex)]));
      exit;
    end;
  finally
    temp.Free;
  end;

  // set specified values to the all TprBarCodeObj objects in the RepDefined
  for I := 0 to RepDefined.ObjectCount do
    if RepDefined.Objects[I] is TprBarCodeObj then
      with TprBarCodeObj(RepDefined.Objects[I]).DefVersion do
      begin
        Text := edCode.Text;
        BarCodeType := TvgrBarCodeType(edTypeDefined.ItemIndex);
      end;

  RepDefined.PrepareReport;
  RepDefined.PreviewPreparedReport(True);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  RepDB.DesignReport(True);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  I: Integer;
  temp: TvgrBarcode;
begin
  // Check the entered code and type
  temp := TvgrBarcode.Create;
  try
    temp.Text := '12345678';
    temp.Typ := TvgrBarCodeType(edTypeDB.ItemIndex);
    if (edCode.Text ='') or (not temp.Checkbarcode) then
    begin
      ShowMessage(Format('Type [%s] can not be used (database containing the 8-digit codes).', [edCode.Text, GetEnumName(TypeInfo(TvgrBarCodeType), edTypeDB.ItemIndex)]));
      exit;
    end;
  finally
    temp.Free;
  end;

  // set specified values to the all TprBarCodeObj objects in the RepDefined
  for I := 0 to RepDB.ObjectCount do
    if RepDB.Objects[I] is TprBarCodeObj then
      with TprBarCodeObj(RepDB.Objects[I]).DefVersion do
      begin
        BarCodeType := TvgrBarCodeType(edTypeDB.ItemIndex);
      end;

  RepDB.PrepareReport;
  RepDB.PreviewPreparedReport(True);
end;

end.
