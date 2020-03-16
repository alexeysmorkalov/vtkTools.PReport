//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "pr_Classes.hpp"
#include "pr_Common.hpp"
#include "pr_Parser.hpp"
#include <Db.hpp>
#include <DBTables.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
        TLabel *Label1;
        TLabel *Label2;
        TButton *Button1;
        TEdit *Edit1;
        TCheckBox *CheckBox1;
        TComboBox *ComboBox1;
        TprReport *prReport1;
        TQuery *Query1;
        void __fastcall Button1Click(TObject *Sender);
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall prReport1UnknownVariable(TObject *Sender,
          const AnsiString VarName, TprVarValue &Value, bool &IsProcessed);
        void __fastcall prReport1UnknownObjFunction(TObject *Sender,
          TComponent *Component, const AnsiString FuncName,
          TprVarsArray &Parameters, int ParametersCount,
          TprVarValue &Value, bool &IsProcessed);
private:	// User declarations
public:		// User declarations
        __fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
