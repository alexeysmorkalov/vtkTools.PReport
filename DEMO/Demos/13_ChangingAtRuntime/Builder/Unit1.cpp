//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#include <pr_Classes.hpp>
#include <pr_Common.hpp>
#include <vgr_ColorButton.hpp>
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  for (int i = 0; i < prReport1->ObjectCount; i++)
    if (dynamic_cast<TprMemoObj*>(prReport1->Objects[i]) != NULL)
      edName->Items->Add(dynamic_cast<TprMemoObj*>(prReport1->Objects[i])->Name);
  if (edName->Items->Count > 0)
    edName->Text = edName->Items->Strings[0];
}
//---------------------------------------------------------------------------

void __fastcall TForm1::prReport1FirstPassObject(TObject *Sender,
      TprObj *Obj, bool &ManuallyProcessed)
{
  TprMemoObj* m;

  if ((Obj->Name == edName->Text) && (dynamic_cast<TprMemoObj*>(Obj) != NULL) && FOnFirstPassObjectUsed)
  {
    m = (TprMemoObj*)Obj;
    m->GenCurVersion->FillColor = bBackColor->SelectedColor;
    m->GenCurVersion->Font->Color = bTextColor->SelectedColor;
  }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  FOnFirstPassObjectUsed = true;
  prReport1->PrepareReport();
  prReport1->PreviewPreparedReport(true);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  TprMemoObj* m;

  m = dynamic_cast<TprMemoObj*>(prReport1->FindObject(edName->Text));
  if (m != NULL)
  {
    ShowMessage("Object [" + edName->Text + "] not found");
    return;
  }

  m->DefVersion->FillColor = bBackColor->SelectedColor;
  m->DefVersion->Font->Color = bTextColor->SelectedColor;

  FOnFirstPassObjectUsed = false;
  prReport1->PrepareReport();
  prReport1->PreviewPreparedReport(true);
}
//---------------------------------------------------------------------------
