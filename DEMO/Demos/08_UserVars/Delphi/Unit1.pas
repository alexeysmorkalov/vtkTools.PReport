unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, DBTables, pr_Common, pr_Classes, pr_Parser;

type
  TForm1 = class(TForm)    
    Button1: TButton;
    prReport1: TprReport;
    Query1: TQuery;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure prReport1UnknownVariable(Sender: TObject;
      const VarName: String; var Value: TprVarValue;
      var IsProcessed: Boolean);
    procedure prReport1UnknownObjFunction(Sender: TObject;
      Component: TComponent; const FuncName: String;
      const Parameters: TprVarsArray; ParametersCount: Integer;
      var Value: TprVarValue; var IsProcessed: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
// generate SQL query
Query1.Close;
if CheckBox1.Checked then
  Query1.SQL.Text := 'select * from customer order by '+
                      ComboBox1.Items[ComboBox1.ItemIndex]
else
  Query1.SQL.Text := 'select * from customer where company like ''%'+
                      Edit1.Text+'%'' order by '+
                      ComboBox1.Items[ComboBox1.ItemIndex];
// start generate report
if prReport1.PrepareReport then
  prReport1.PreviewPreparedReport(true);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
ComboBox1.ItemIndex := 0;
end;

procedure TForm1.prReport1UnknownVariable(Sender: TObject;
  const VarName: String; var Value: TprVarValue; var IsProcessed: Boolean);
var
  s : string;
begin
if AnsiCompareText(VarName,'FindValid')=0 then
  begin
    // prepare find valid description
    if CheckBox1.Checked then
      s := 'All records'
    else
      s := 'Company must contains "'+Edit1.Text+'"';
    _vSetAsString(Value,s);
    IsProcessed := true;
  end
else
  if AnsiCompareText(VarName,'Order')=0 then
    begin
      // prepare order description
      _vSetAsString(Value,ComboBox1.Items[ComboBox1.ItemIndex]);
      IsProcessed := true;
    end
end;

procedure TForm1.prReport1UnknownObjFunction(Sender: TObject;
  Component: TComponent; const FuncName: String;
  const Parameters: TprVarsArray; ParametersCount: Integer;
  var Value: TprVarValue; var IsProcessed: Boolean);
var
  f : TField;
begin
if (Component=Query1) and
   (AnsiCompareText(FuncName,'Query1.FieldLen')=0) and
   (ParametersCount=1) then
  begin
    // Parameter with index 0 is the fieldname
    f := Query1.FindField(_vAsString(Parameters[0]));
    if f<>nil then
      begin
        // field is found return length of field value
        _vSetAsInteger(Value,Length(f.AsString));
        IsProcessed := true;
      end
  end
else
  if (Component = Query1) and
     (AnsiCompareText(FuncName, 'Query1.FieldSubString') = 0) and
     (ParametersCount = 3) then
  begin
    // Parameter with index 0 is the fieldname
    // 1 - index of first char
    // 2 - number of chars
    f := Query1.FindField(_vAsString(Parameters[0]));
    if f<>nil then
      begin
        // field is found return length of field value
        _vSetAsString(Value, Copy(f.AsString, _vAsInteger(Parameters[1]), _vAsInteger(PArameters[2])));
        IsProcessed := true;
      end
  end;
end;

end.
