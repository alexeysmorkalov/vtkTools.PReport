{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxMemoLinesEditorForm;

interface
              
{$I PR.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, 

  pr_Common, pr_TxClasses, pr_TxUtils, pr_MultiLang;

type
  TprTxMemoLinesEditorForm = class(TprForm)
    Panel1: TPanel;
    prMLRes1: TprMLRes;
    Panel2: TPanel;
    bOK: TButton;
    bCancel: TButton;
    Bevel1: TBevel;
    bPasteSymbol: TSpeedButton;
    bExpression: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure bExpressionClick(Sender: TObject);
    procedure SGDblClick(Sender: TObject);
    procedure bPasteSymbolClick(Sender: TObject);
    procedure SGKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SGKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    Report: TprTxReport;
    Lines: TStrings;
    Memo: TprTxOEMMemo;
    SG: TprTxCharGrid;
    procedure PasteCurrentSymbol;
  public
    { Public declarations }
    procedure EditLines(_Report : TprTxReport; _Lines : TStrings);
  end;

implementation

uses pr_FormatExpression;

{$R *.DFM}

procedure TprTxMemoLinesEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

procedure TprTxMemoLinesEditorForm.FormCreate(Sender: TObject);
begin
  Memo := TprTxOEMMemo.Create(Self);
  with Memo do
  begin
    Align := alClient;
    Parent := Self;
    ScrollBars := ssBoth;
  end;

  SG := TprTxCharGrid.Create(Self);
  with SG do
  begin
    Align := alClient;
    Parent := Panel1;
    OnDblClick := SGDblClick;
    OnKeyDown := SGKeyDown;
    OnKeyPress := SGKeyPress;
  end;

  LoadResImage(bExpression.Glyph,'OPEN');
  LoadResImage(bPasteSymbol.Glyph,'PASTE');
end;

procedure TprTxMemoLinesEditorForm.EditLines;
begin
Lines := _Lines;
Report := _Report;
Memo.Report := _Report;
if Report <> nil then
  SG.FontCharset := Report.FontCharset;
Memo.Lines.Assign(Lines);
if ShowModal=mrOk then
  Lines.Assign(Memo.Lines);
end;

procedure TprTxMemoLinesEditorForm.bExpressionClick(Sender: TObject);
var
  s : string;
begin
TprFormatExpressionForm.Create(Application).SelectExpression(Report,Memo,s);
end;

procedure TprTxMemoLinesEditorForm.PasteCurrentSymbol;
begin
  Memo.Report := nil;
  SendMessage(Memo.Handle, WM_CHAR, SG.Row * 16 + SG.Col, 0);
  Memo.Report := Report;
end;

procedure TprTxMemoLinesEditorForm.SGDblClick(Sender: TObject);
begin
  PasteCurrentSymbol;
end;

procedure TprTxMemoLinesEditorForm.bPasteSymbolClick(Sender: TObject);
begin
  PasteCurrentSymbol;
end;

procedure TprTxMemoLinesEditorForm.SGKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
if Shift=[] then
  case Key of
    VK_SPACE:
      begin
        PasteCurrentSymbol;
        Key := 0;
      end;
  end;
end;

procedure TprTxMemoLinesEditorForm.SGKeyPress(Sender: TObject;
  var Key: Char);
begin
if Key<>' ' then
  SendMessage(Memo.Handle,WM_CHAR,byte(Key),0);
Key := #0;
end;

end.
