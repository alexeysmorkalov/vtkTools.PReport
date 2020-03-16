//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "vgr_ColorButton.hpp"
#include <DB.hpp>
#include <DBTables.hpp>
#include "pr_Classes.hpp"
#include "pr_Common.hpp"
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label1;
        TLabel *Label2;
        TLabel *Label4;
        TLabel *Label5;
        TButton *Button1;
        TButton *Button2;
        TvgrColorButton *bBackColor;
        TvgrColorButton *bTextColor;
        TComboBox *edName;
        TprReport *prReport1;
        TTable *Table1;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall prReport1FirstPassObject(TObject *Sender,
          TprObj *Obj, bool &ManuallyProcessed);
        void __fastcall Button1Click(TObject *Sender);
        void __fastcall Button2Click(TObject *Sender);
private:	// User declarations
        bool FOnFirstPassObjectUsed;
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
