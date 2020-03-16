{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_ValuesEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, Grids, typinfo, pr_utils, ActnList, IniFiles, ClipBrd,
  ComCtrls, ImgList, Math,  ToolWin, ExtCtrls,

  pr_Common, pr_MultiLang;

{$I pr.inc}

type
  TprValuesEditorForm = class(TprForm)
    ActionList: TActionList;
    aDelVersion: TAction;
    aNewVersion: TAction;
    aEditVersion: TAction;
    ImageList: TImageList;
    Grid: TDrawGrid;
    aCopy: TAction;
    aCut: TAction;
    aPaste: TAction;
    prMLRes1: TprMLRes;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure aDelVersionUpdate(Sender: TObject);
    procedure aDelVersionExecute(Sender: TObject);
    procedure aEditVersionExecute(Sender: TObject);
    procedure aNewVersionExecute(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure aPasteUpdate(Sender: TObject);
    procedure aCopyUpdate(Sender: TObject);
    procedure aPasteExecute(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aCutExecute(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateGrid;
    procedure CopyToClipBoard;
  protected
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    Report : TprCustomReport;
    
    procedure EditValues(_Report : TprCustomReport);
  end;

implementation

uses pr_Strings, pr_ValueEditor, pr_DesignerFunctions;

var
  Captions : array [0..8] of string =
             ('Var name',
              'Function',
              'Formula',
              'Calc method',
              'Calc dataset',
              'CrossTab dataset',
              'Sphere',
              'Group (sphere)',
              'Dataset (sphere)');

{$R *.DFM}

procedure TprValuesEditorForm.prRestoreProperties;
var
  i : integer;
begin
inherited;
for i:=0 to Grid.ColCount-1 do
  Grid.ColWidths[i]:=Ini.ReadInteger(sn,'Col'+IntToStr(i),Grid.ColWidths[i]);
end;

procedure TprValuesEditorForm.prSaveProperties;
var
  i : integer;
begin
inherited;
for i:=0 to Grid.ColCount-1 do
  Ini.WriteInteger(sn,'Col'+IntToStr(i),Grid.ColWidths[i]);
end;

procedure TprValuesEditorForm.UpdateGrid;
begin
Grid.RowCount:=Max(2,Report.Values.Count+1);
Grid.Repaint;
end;

procedure TprValuesEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action:=caFree;
end;

procedure TprValuesEditorForm.EditValues;
begin
Report :=_Report;
Caption:=Format(prLoadStr(sValuesEditorCaption),[Report.Name]);

UpdateGrid;
ShowModal;
end;

procedure TprValuesEditorForm.aDelVersionUpdate(Sender: TObject);
begin
TAction(Sender).Enabled:=Grid.Row<=Report.Values.Count;
end;

procedure TprValuesEditorForm.aDelVersionExecute(Sender: TObject);
begin
if MBox(prLoadStr(sDeleteVarQuestion),prLoadStr(sAttention),MB_YESNO+MB_ICONQUESTION)=IDYES then
  begin
    Report.Values[Grid.Row-1].Free;
    UpdateGrid;
    Report.DsgnTemplateChanged(nil,true);
  end;
end;

procedure TprValuesEditorForm.aEditVersionExecute(Sender: TObject);
begin
if TprValueEditorForm.Create(Application).EditValue(Report.Values[Grid.Row-1]) then
  UpdateGrid;
end;

procedure TprValuesEditorForm.aNewVersionExecute(Sender: TObject);
var
  v : TprValue;
begin
v := TprValue(Report.Values.Add);
if TprValueEditorForm.Create(Application).EditValue(v) then
  UpdateGrid
else
  v.Free;
end;

procedure TprValuesEditorForm.GridDblClick(Sender: TObject);
begin
aEditVersion.Execute;
end;

procedure TprValuesEditorForm.GridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  s : string;
  a : cardinal;
begin
if ARow=0 then
  begin
    s:=Captions[ACol];
    a:=DT_CENTER+DT_VCENTER+DT_SINGLELINE+DT_END_ELLIPSIS;
  end
else
  begin
    a:=DT_LEFT+DT_VCENTER+DT_SINGLELINE+DT_END_ELLIPSIS;
    if ARow<=Report.Values.Count then
      begin
        case ACol of
          0: s:=Report.Values[ARow-1].Name;
          1: s:=GetEnumName(TypeInfo(TprAggFunction),integer(Report.Values[ARow-1].AggFunction));
          2: s:=Report.Values[ARow-1].Formula;
          3: s:=GetEnumName(TypeInfo(TprCalcValueType),integer(Report.Values[ARow-1].CalcOn));
          4: s:=Report.Values[ARow-1].DataSetName;
          5: s:=Report.Values[ARow-1].CrossTabHorzDataSetName;

          6: s:=GetEnumName(TypeInfo(TprResetValueType),integer(Report.Values[ARow-1].ResetOn));
          7: if Report.Values[ARow-1].Group<>nil then
               s:=Report.Values[ARow-1].Group.Name
             else
               s:=prLoadStr(NullPointerString);
          8: s:=Report.Values[ARow-1].ResetDataSetName;
        end;
      end;
  end;

DrawTextEx(Grid.Canvas.Handle,PChar(s),Length(s),Rect,a,nil);
end;

procedure TprValuesEditorForm.FormCreate(Sender: TObject);
begin
prLoadResImages(Self,ImageList);
end;

procedure TprValuesEditorForm.aPasteUpdate(Sender: TObject);
begin
TAction(Sender).Enabled:=ClipBoard.HasFormat(CF_VALUES);
end;

procedure TprValuesEditorForm.aCopyUpdate(Sender: TObject);
begin
TAction(Sender).Enabled:=Grid.Row<=Report.Values.Count;
end;

procedure TprValuesEditorForm.CopyToClipboard;
var
  v : TprValue;
  Buf : string;
  hMem : THandle;
  pMem : pointer;
  lBuf : integer;
begin
Buf:='';
v  :=Report.Values[Grid.Row-1];
Buf:=v.Name+#13;
if v.Group<>nil then
  Buf:=Buf+v.Group.Name;
Buf:=Buf+#13+
     GetEnumName(TypeInfo(TprAggFunction),integer(v.AggFunction))+#13+
     v.Formula+#13+
     GetEnumName(TypeInfo(TprResetValueType),integer(v.ResetOn))+#13+
     GetEnumName(TypeInfo(TprCalcValueType),integer(v.CalcOn))+#13+
     v.DataSetName+#13+
     v.ResetDataSetName+#13+
     v.CrossTabHorzDataSetName+#13;
lBuf:=Length(Buf);

ClipBoard.Open;
try
  hMem:=GlobalAlloc(GMEM_MOVEABLE+GMEM_SHARE+GMEM_ZEROINIT,lBuf);
  if hMem<>0 then
    begin
      pMem:=GlobalLock(hMem);
      if pMem<>nil then
        begin
          CopyMemory(pMem,@(Buf[1]),lBuf);
          GlobalUnLock(hMem);
          ClipBoard.SetAsHandle(CF_VALUES,hMem);
        end;
    end;
finally
  ClipBoard.Close;
end;
end;

procedure TprValuesEditorForm.aPasteExecute(Sender: TObject);
var
  v : TprValue;
  i,p : integer;
  hMem : THandle;
  pMem : pointer;
  Buf,n : string;
begin
ClipBoard.Open;
try
  hMem:=Clipboard.GetAsHandle(CF_VALUES);
  pMem:=GlobalLock(hMem);
  if pMem<>nil then
    begin
      p:=GlobalSize(hMem);
      SetLength(Buf,p);
      MoveMemory(@(Buf[1]),pMem,p);
      try
        v :=TprValue(Report.Values.Add);
        p :=1;
        n :=ExtractSubStr(Buf,p,[#13]);
        if Report.Values.IndexByName(n)=-1 then
          v.Name:=n;
        n :=ExtractSubStr(Buf,p,[#13]);
        i :=Report.Groups.IndexByName(n);
        if i<>-1 then
          v.Group:=Report.Groups[i];
        v.AggFunction := TprAggFunction(GetEnumValue(TypeInfo(TprAggFunction),ExtractSubStr(Buf,p,[#13])));
        v.Formula := ExtractSubStr(Buf,p,[#13]);
        v.ResetOn := TprResetValueType(GetEnumValue(TypeInfo(TprResetValueType),ExtractSubStr(Buf,p,[#13])));
        v.CalcOn := TprCalcValueType(GetEnumValue(TypeInfo(TprCalcValueType),ExtractSubStr(Buf,p,[#13])));
        v.DatasetName := ExtractSubStr(Buf,p,[#13]);
        v.ResetDatasetName := ExtractSubStr(Buf,p,[#13]);
        v.CrossTabHorzDataSetName := ExtractSubStr(Buf,p,[#13]);
  
        UpdateGrid;
        Report.TemplateChanged := true;
      finally
        GlobalUnlock(hMem);
      end;
    end;
finally
  ClipBoard.Close;
end;
end;

procedure TprValuesEditorForm.aCopyExecute(Sender: TObject);
begin
CopyToClipBoard;
end;

procedure TprValuesEditorForm.aCutExecute(Sender: TObject);
begin
CopyToClipBoard;
Report.Values[Grid.Row-1].Free;
UpdateGrid;
Report.TemplateChanged := true;
end;

initialization

Captions[0]:=prLoadStr(sValuesEditorCaptionsVarName);
Captions[1]:=prLoadStr(sValuesEditorCaptionsAggFunction);
Captions[2]:=prLoadStr(sValuesEditorCaptionsFormula);
Captions[3]:=prLoadStr(sValuesEditorCaptionsCalcMethod);
Captions[4]:=prLoadStr(sValuesEditorCaptionsCalcDataset);
Captions[5]:=prLoadStr(sValuesEditorCaptionsCrossTabDataset);
Captions[6]:=prLoadStr(sValuesEditorCaptionsSphere);
Captions[7]:=prLoadStr(sValuesEditorCaptionsSphereGroup);
Captions[8]:=prLoadStr(sValuesEditorCaptionsDataSetSphere);

end.

