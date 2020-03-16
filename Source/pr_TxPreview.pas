{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxPreview;

{$i pr.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RichEdit, StdCtrls, ImgList, ActnList, ComCtrls, math, ToolWin, ExtCtrls,
  IniFiles, 
  {$ifdef PR_D6_D7} types, {$endif}

  pr_Common, pr_TxClasses, pr_MultiLang, pr_CommonPreviewPanel,
  pr_TxPreviewPanel;

type
  ///////////////////////////////
  //
  // TprTxPreviewForm
  //
  ///////////////////////////////
  TprTxPreviewForm = class(TprPreview)
    ActionList: TActionList;
    aPrint: TAction;
    aFind: TAction;
    aClose: TAction;
    aSave: TAction;
    aOpen: TAction;
    ImageList: TImageList;
    SB: TStatusBar;
    aProperties: TAction;
    aCustomAction: TAction;
    prMLRes1: TprMLRes;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton12: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    Label1: TLabel;
    EDLine: TComboBox;
    Label2: TLabel;
    EDPage: TComboBox;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton9: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    aExportToTXT: TAction;
    Preview: TprTxPreviewPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure aPrintExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure EDLineKeyPress(Sender: TObject; var Key: Char);
    procedure EDPageKeyPress(Sender: TObject; var Key: Char);
    procedure EDPageClick(Sender: TObject);
    procedure EDLineClick(Sender: TObject);
    procedure aFindExecute(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure aCustomActionExecute(Sender: TObject);
    procedure aCustomActionUpdate(Sender: TObject);
    procedure aExportToTXTExecute(Sender: TObject);
    procedure PreviewCursorPosChanged(Sender: TObject; CursorPos: TPoint);
    procedure PreviewDblClick(Sender: TObject;
      PreviewUserData: TprPreviewUserData; X, Y: Integer);
    procedure PreviewMouseDown(Sender: TObject;
      PreviewUserData: TprPreviewUserData; X, Y: Integer;
      Shift: TShiftState);
    procedure PreviewMouseMove(Sender: TObject;
      PreviewUserData: TprPreviewUserData; X, Y: Integer; var cur: TCursor;
      var HighlightObject: Boolean);
    procedure aPropertiesExecute(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateCaretPos;
  protected
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
  end;

implementation

uses pr_Strings, pr_TxConsts, pr_Utils, pr_TxUtils;

{$R *.DFM}

/////////////////////////////////////////////////
//
// TprTxPreviewForm
//
/////////////////////////////////////////////////
procedure TprTxPreviewForm.FormCreate(Sender: TObject);
begin
prLoadResImages(Self,ImageList);
if Report.Title='' then
  Caption := Format(prLoadStr(sPreviewCaption),[prLoadStr(sNoReportName)])
else
  Caption := Format(prLoadStr(sPreviewCaption),[Report.Title]);
Preview.Report := TprTxReport(Report);
aCustomAction.Hint := Report.CustomActionInPreviewCaption;
end;

procedure TprTxPreviewForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

procedure TprTxPreviewForm.prRestoreProperties;
begin
inherited;
Preview.ReadFromIni(ini,'Preview');
end;

procedure TprTxPreviewForm.prSaveProperties;
begin
inherited;
Preview.WriteToIni(ini,'Preview');
end;

procedure TprTxPreviewForm.aPrintExecute(Sender: TObject);
begin
Preview.Print;
end;

procedure TprTxPreviewForm.aOpenExecute(Sender: TObject);
begin
Preview.Load;
end;

procedure TprTxPreviewForm.aSaveExecute(Sender: TObject);
begin
Preview.Save;
end;

procedure TprTxPreviewForm.UpdateCaretPos;
var
  i : integer;
begin
SB.Panels[0].Text := Format('%d : %d',[Preview.CursorRow+1,Preview.CursorCol+1]);
EDLine.Text := IntToStr(Preview.CursorRow+1);
i := Preview.PageIndex;
if i=-1 then
  begin
    SB.Panels[1].Text := '';
    EDPage.Text := '';
  end
else
  begin
    SB.Panels[1].Text := Format(prLoadStr(sTxPreviewPages),[i+1,Preview.PageCount]);
    EDPage.Text := IntToStr(i+1);
  end;
end;

procedure TprTxPreviewForm.EDLineKeyPress(Sender: TObject; var Key: Char);
var
  l : integer;
begin
if Key=#13 then
  begin
    Key := #0;
    l := StrToIntDef(EDLine.Text,-1);
    if (l>0) and (l<=Preview.LinesCount) then
      begin
        if EDLine.Items.IndexOf(IntToStr(l))=-1 then
          EDLine.Items.Add(IntToStr(l));
        Preview.SetCursorPos(0,l-1);
        ActiveControl := Preview;
      end;
  end;
end;

procedure TprTxPreviewForm.EDPageKeyPress(Sender: TObject; var Key: Char);
var
  p: Integer;
begin
  if Key=#13 then
  begin
    Key := #0;
    p := StrToIntDef(EDPage.Text,-1);
    if p>0 then
    begin
      if p>Preview.PageCount then
        MBError(Format(prLoadStr(sErrorTxPreviewPageRange),[Preview.PageCount]))
      else
      begin
        if EDPage.Items.IndexOf(IntToStr(p))=-1 then
          EDPage.Items.Add(IntToStr(p));
        Preview.PageIndex := p-1;
        ActiveControl := Preview;
      end;
    end;
  end;
end;

procedure TprTxPreviewForm.EDPageClick(Sender: TObject);
var
  Key : char;
begin
Key := #13;
EDPageKeyPress(nil,Key);
end;

procedure TprTxPreviewForm.EDLineClick(Sender: TObject);
var
  Key : char;
begin
Key := #13;
EDLineKeyPress(nil,Key);
end;

procedure TprTxPreviewForm.aFindExecute(Sender: TObject);
begin
Preview.Find;
end;

procedure TprTxPreviewForm.aCloseExecute(Sender: TObject);
begin
Close;
end;

procedure TprTxPreviewForm.aCustomActionExecute(Sender: TObject);
begin
DoOnCustomAction;
end;

procedure TprTxPreviewForm.aCustomActionUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := Assigned(Report.OnCustomActionInPreview);
end;

procedure TprTxPreviewForm.aExportToTXTExecute(Sender: TObject);
begin
TprTxReport(Report).ExportTo;
end;

procedure TprTxPreviewForm.PreviewCursorPosChanged(Sender: TObject;
  CursorPos: TPoint);
begin
UpdateCaretPos;
end;

procedure TprTxPreviewForm.PreviewDblClick(Sender: TObject;
  PreviewUserData: TprPreviewUserData; X, Y: Integer);
begin
if Assigned(Report.OnPreviewDblClick) then
  Report.OnPreviewDblClick(Report,PreviewUserData);
end;

procedure TprTxPreviewForm.PreviewMouseDown(Sender: TObject;
  PreviewUserData: TprPreviewUserData; X, Y: Integer; Shift: TShiftState);
begin
if Assigned(Report.OnPreviewMouseDown) then
  Report.OnPreviewMouseDown(Report,PreviewUserData,Shift);
end;

procedure TprTxPreviewForm.PreviewMouseMove(Sender: TObject;
  PreviewUserData: TprPreviewUserData; X, Y: Integer; var cur: TCursor;
  var HighlightObject: Boolean);
begin
  if Assigned(Report.OnPreviewMouseMove) then
    Report.OnPreviewMouseMove(Report,PreviewUserData,cur,HighlightObject);
end;

procedure TprTxPreviewForm.aPropertiesExecute(Sender: TObject);
begin
  Preview.EditOptions;
end;

initialization

RegisterClass(TprTxPreviewForm);
  
end.

