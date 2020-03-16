{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxCommandEditor;

interface

{$I PR.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ActnList, math,

  pr_Common, pr_TxClasses, pr_CommonDesignerPanel, pr_MultiLang,
  pr_Strings, ExtCtrls;

type
  TprTxCommandEditorForm = class(TprObjPropsForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    bRight: TSpeedButton;
    bRightAll: TSpeedButton;
    bLeftAll: TSpeedButton;
    bLeft: TSpeedButton;
    bMoveUp: TSpeedButton;
    bMoveDown: TSpeedButton;
    LBAll: TListBox;
    LBSelected: TListBox;
    ActionList: TActionList;
    aRight: TAction;
    aRightAll: TAction;
    aLeft: TAction;
    aLeftAll: TAction;
    aMoveUp: TAction;
    aMoveDown: TAction;
    prMLRes1: TprMLRes;
    procedure FormCreate(Sender: TObject);
    procedure aRightUpdate(Sender: TObject);
    procedure aRightAllUpdate(Sender: TObject);
    procedure aLeftUpdate(Sender: TObject);
    procedure aLeftAllUpdate(Sender: TObject);
    procedure aMoveUpUpdate(Sender: TObject);
    procedure aMoveDownUpdate(Sender: TObject);
    procedure aRightExecute(Sender: TObject);
    procedure aRightAllExecute(Sender: TObject);
    procedure aLeftExecute(Sender: TObject);
    procedure aLeftAllExecute(Sender: TObject);
    procedure aMoveUpExecute(Sender: TObject);
    procedure aMoveDownExecute(Sender: TObject);
    procedure LBAllDblClick(Sender: TObject);
    procedure LBSelectedDblClick(Sender: TObject);
    procedure LBAllKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LBSelectedKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  protected
    procedure CopyMultiplyPropertiesFromControls(L : TList); override;
    procedure CopyMultiplyPropertiesToControls(L : TList); override;
  public
    { Public declarations }
  end;

implementation

uses pr_DesignerFunctions, pr_TxConsts, pr_TxDesigner;

{$R *.DFM}

procedure TprTxCommandEditorForm.FormCreate(Sender: TObject);
begin
LoadResImage(bLeft.Glyph,'LEFT');
LoadResImage(bLeftAll.Glyph,'LEFTALL');
LoadResImage(bRight.Glyph,'RIGHT');
LoadResImage(bRightAll.Glyph,'RIGHTALL');
LoadResImage(bMoveUp.Glyph,'MOVEUP');
LoadResImage(bMoveDown.Glyph,'MOVEDOWN');
end;

procedure TprTxCommandEditorForm.CopyMultiplyPropertiesToControls(L : TList);
var
  i,j : integer;
begin
LBSelected.Clear;
LBAll.Clear;
for i:=0 to L.Count-1 do
  with TprTxCommandObjRecVersion(L[i]) do
    for j:=0 to TxCommandsCount-1 do
      LBSelected.Items.AddObject(TxCommands[j].Description,TxCommands[j]);
for i:=0 to TxReportOptions.TxCommandsCount-1 do
  if LBSelected.Items.IndexOfObject(TxReportOptions.TxCommands[i])=-1 then
    LBAll.Items.AddObject(TxReportOptions.TxCommands[i].Description,TxReportOptions.TxCommands[i]);
end;

procedure TprTxCommandEditorForm.CopyMultiplyPropertiesFromControls(L : TList);
var
  i,j : integer;
begin
for i:=0 to L.Count-1 do
  with TprTxCommandObjRecVersion(L[i]) do
    begin
      ClearTxCommands;
      for j:=0 to LBSelected.Items.Count-1 do
        AddTxCommand(TprTxCommand(LBSelected.Items.Objects[j]));
    end;
end;

procedure TprTxCommandEditorForm.aRightUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := LBAll.ItemIndex>=0;
end;

procedure TprTxCommandEditorForm.aRightAllUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := LBAll.Items.Count>0;
end;

procedure TprTxCommandEditorForm.aLeftUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := LBSelected.ItemIndex>=0;
end;

procedure TprTxCommandEditorForm.aLeftAllUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := LBSelected.Items.Count>0;
end;

procedure TprTxCommandEditorForm.aMoveUpUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := LBSelected.ItemIndex>0;
end;

procedure TprTxCommandEditorForm.aMoveDownUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := (LBSelected.ItemIndex>=0) and (LBSelected.ItemIndex<LBSelected.Items.Count-1);
end;

procedure TprTxCommandEditorForm.aRightExecute(Sender: TObject);
var
  i : integer;
begin
LBSelected.Items.AddObject(LBAll.Items[LBAll.ItemIndex],LBAll.Items.Objects[LBAll.ItemIndex]);
i := LBAll.ItemIndex;
LBAll.Items.Delete(i);
LBAll.ItemIndex := Min(i,LBAll.Items.Count-1);
end;

procedure TprTxCommandEditorForm.aRightAllExecute(Sender: TObject);
var
  i : integer;
begin
for i:=0 to LBAll.Items.Count-1 do
  LBSelected.Items.AddObject(LBAll.Items[i],LBAll.Items.Objects[i]);
LBAll.Clear;
end;

procedure TprTxCommandEditorForm.aLeftExecute(Sender: TObject);
var
  i : integer;
begin
LBAll.Items.AddObject(LBSelected.Items[LBSelected.ItemIndex],LBSelected.Items.Objects[LBSelected.ItemIndex]);
i := LBSelected.ItemIndex;
LBSelected.Items.Delete(i);
LBSelected.ItemIndex := Min(i,LBSelected.Items.Count-1);
end;

procedure TprTxCommandEditorForm.aLeftAllExecute(Sender: TObject);
var
  i : integer;
begin
for i:=0 to LBSelected.Items.Count-1 do
  LBAll.Items.AddObject(LBSelected.Items[i],LBSelected.Items.Objects[i]);
LBSelected.Clear;
end;

procedure TprTxCommandEditorForm.aMoveUpExecute(Sender: TObject);
begin
LBSelected.Items.Exchange(LBSelected.ItemIndex,LBSelected.ItemIndex-1);
end;

procedure TprTxCommandEditorForm.aMoveDownExecute(Sender: TObject);
begin
LBSelected.Items.Exchange(LBSelected.ItemIndex,LBSelected.ItemIndex+1);
end;

procedure TprTxCommandEditorForm.LBAllDblClick(Sender: TObject);
begin
aRight.Execute;
end;

procedure TprTxCommandEditorForm.LBSelectedDblClick(Sender: TObject);
begin
aLeft.Execute;
end;

procedure TprTxCommandEditorForm.LBAllKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
if (Shift=[]) and (Key=VK_SPACE) then
  begin
    aRight.Execute;
    Key := 0;
  end;
end;

procedure TprTxCommandEditorForm.LBSelectedKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
if (Shift=[]) and (Key=VK_SPACE) then
  begin
    aLeft.Execute;
    Key := 0;
  end;
end;

end.

