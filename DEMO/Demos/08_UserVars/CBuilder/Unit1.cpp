//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "pr_Classes"
#pragma link "pr_Common"
#pragma link "pr_Parser"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button1Click(TObject *Sender)
{
  // generate SQL query
  Query1->Close();
  if (CheckBox1->Checked)
  {
    Query1->SQL->Text = "select * from customer order by " +
    ComboBox1->Items->Strings[ComboBox1->ItemIndex];
  }
  else
  {
    Query1->SQL->Text = "select * from customer where company like '%" +
     Edit1->Text +
     "%' order by " +
     ComboBox1->Items->Strings[ComboBox1->ItemIndex];
  }
  // start generate report
  if (prReport1->PrepareReport())
    prReport1->PreviewPreparedReport(true);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  ComboBox1->ItemIndex = 0;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::prReport1UnknownVariable(TObject *Sender,
      const AnsiString VarName, TprVarValue &Value, bool &IsProcessed)
{
  AnsiString s;
  if (AnsiCompareText(VarName, "FindValid") == 0)
  {
    // prepare find valid description
    if (CheckBox1->Checked)
      s = "All records";
    else
      s = "Company must contains """ + Edit1->Text + """";
      
    _vSetAsString(Value, s);
    IsProcessed = true;
  }
  else
    if (AnsiCompareText(VarName, "Order") == 0)
    {
      // prepare order description
      _vSetAsString(Value, ComboBox1->Items->Strings[ComboBox1->ItemIndex]);
      IsProcessed = true;
    }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::prReport1UnknownObjFunction(TObject *Sender,
      TComponent *Component, const AnsiString FuncName,
      TprVarsArray &Parameters, int ParametersCount, TprVarValue &Value,
      bool &IsProcessed)
{
if ((Component == Query1) &&
    (AnsiCompareText(FuncName, "Query1.FieldLen") == 0) &&
    (ParametersCount == 1))
  {
    // Parameter with index 0 is the fieldname
    TField* f = Query1->FindField(_vAsString(Parameters[0]));
    if (f != NULL)
    {
        // field is found return length of field value
        AnsiString s = f->AsString;
        _vSetAsInteger(Value, s.Length());
        IsProcessed = true;
    }
  }
else
  if ((Component == Query1) &&
      (AnsiCompareText(FuncName, "Query1.FieldSubString") == 0) &&
      (ParametersCount == 3))
  {
    // Parameter with index 0 is the fieldname
    // 1 - index of first char
    // 2 - number of chars
    TField* f = Query1->FindField(_vAsString(Parameters[0]));
    if (f != NULL)
      {
        // field is found return length of field value
        AnsiString s = f->AsString;
        _vSetAsString(Value, s.SubString(_vAsInteger(Parameters[1]), _vAsInteger(Parameters[2])));
        IsProcessed = true;
      }
  }
}
//---------------------------------------------------------------------------
