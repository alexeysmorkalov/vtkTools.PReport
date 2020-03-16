{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_GroupsEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Grids, Pr_Utils, ActnList,
  IniFiles, Math, ImgList, ClipBrd, ComCtrls, ToolWin,

  pr_Common, pr_MultiLang;

{$I pr.inc}

type
  TprGroupsEditorForm = class(TprForm)
    ActionList: TActionList;
    aDelVersion: TAction;
    aNewVersion: TAction;
    aEditVersion: TAction;
    ImageList: TImageList;
    Grid: TDrawGrid;
    aCut: TAction;
    aCopy: TAction;
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
    procedure aDelVersionUpdate(Sender: TObject);
    procedure aDelVersionExecute(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure aNewVersionExecute(Sender: TObject);
    procedure aEditVersionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure aPasteUpdate(Sender: TObject);
    procedure aCutUpdate(Sender: TObject);
    procedure aPasteExecute(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aCutExecute(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateGrid;
    procedure CopyToClipboard;
  protected
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    Report : TprCustomReport;

    procedure EditGroups(_Report : TprCustomReport);
  end;

implementation

uses pr_Strings, pr_GroupEditor, pr_DesignerFunctions;

var
  Captions : array [0..2] of string =
             ('Имя группы',
              'Условие',
              'Секция данных');

{$R *.DFM}

procedure TprGroupsEditorForm.prRestoreProperties;
var
  i : integer;
begin
inherited;
for i:=0 to Grid.ColCount-1 do
  Grid.ColWidths[i]:=Ini.ReadInteger(sn,'Col'+IntToStr(i),Grid.ColWidths[i]);
end;

procedure TprGroupsEditorForm.prSaveProperties;
var
  i : integer;
begin
inherited;
for i:=0 to Grid.ColCount-1 do
  Ini.WriteInteger(sn,'Col'+IntToStr(i),Grid.ColWidths[i]);
end;

procedure TprGroupsEditorForm.UpdateGrid;
begin
Grid.RowCount:=Max(2,Report.Groups.Count+1);
Grid.Repaint;
end;

procedure TprGroupsEditorForm.EditGroups;
begin
Report :=_Report;
Caption:=Format(prLoadStr(sGroupsEditorCaption),[Report.Name]);

UpdateGrid;
ShowModal;
end;

procedure TprGroupsEditorForm.aDelVersionUpdate(Sender: TObject);
begin
TAction(Sender).Enabled:=Grid.Row<=Report.Groups.Count;
end;

procedure TprGroupsEditorForm.aDelVersionExecute(Sender: TObject);
begin
if MBox(prLoadStr(sDeleteGroupQuestion),prLoadStr(sAttention),MB_YESNO+MB_ICONQUESTION)=IDYES then
  begin
    Report.Groups[Grid.Row-1].Free;
    UpdateGrid;
    Report.DsgnTemplateChanged(nil,true);
  end;
end;

procedure TprGroupsEditorForm.GridDblClick(Sender: TObject);
begin
aEditVersion.Execute;
end;

procedure TprGroupsEditorForm.GridDrawCell(Sender: TObject; ACol,
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
    if ARow<=Report.Groups.Count then
      begin
        case ACol of
          0: s:=Report.Groups[ARow-1].Name;
          1: s:=Report.Groups[ARow-1].Valid;
          2: if Report.Groups[ARow-1].DetailBand<>nil then
               s:=Report.Groups[ARow-1].DetailBand.Name
             else
               s:=prLoadStr(NullPointerString);
        end;
      end;
  end;

DrawTextEx(Grid.Canvas.Handle,PChar(s),Length(s),Rect,a,nil);
end;

procedure TprGroupsEditorForm.aNewVersionExecute(Sender: TObject);
var
  g : TprGroup;
begin
g := TprGroup.Create(Report.prOwner);
g.Report := Report;
g.Name := GetValidComponentName(g);
if TprGroupEditorForm.Create(Application).EditGroup(g) then
  UpdateGrid
else
  g.Free;
end;

procedure TprGroupsEditorForm.aEditVersionExecute(Sender: TObject);
begin
if TprGroupEditorForm.Create(Application).EditGroup(Report.Groups[Grid.Row-1]) then
  UpdateGrid;
end;

procedure TprGroupsEditorForm.FormCreate(Sender: TObject);
begin
prLoadResImages(Self,ImageList);
end;

procedure TprGroupsEditorForm.aPasteUpdate(Sender: TObject);
begin
TAction(Sender).Enabled:=ClipBoard.HasFormat(CF_GROUPS);
end;

procedure TprGroupsEditorForm.aCutUpdate(Sender: TObject);
begin
TAction(Sender).Enabled:=Grid.Row<=Report.Groups.Count;
end;

procedure TprGroupsEditorForm.CopyToClipboard;
var
  g : TprGroup;
  Buf : string;
  hMem : THandle;
  pMem : pointer;
  lBuf : integer;
begin
Buf:='';
g  :=Report.Groups[Grid.Row-1];
Buf:=g.Name+#13+g.Valid+#13;
if g.DetailBand<>nil then
  Buf:=Buf+g.DetailBand.Name;
Buf :=Buf+#13;
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
          ClipBoard.SetAsHandle(CF_GROUPS,hMem);
        end;
    end;
finally
  ClipBoard.Close;
end;
end;

procedure TprGroupsEditorForm.aPasteExecute(Sender: TObject);
label
  l1;
var
  g : TprGroup;
  i,j,p : integer;
  hMem : THandle;
  pMem : pointer;
  Buf,n : string;
begin
ClipBoard.Open;
try
  hMem:=Clipboard.GetAsHandle(CF_GROUPS);
  pMem:=GlobalLock(hMem);
  if pMem<>nil then
    begin
      p:=GlobalSize(hMem);
      SetLength(Buf,p);
      MoveMemory(@(Buf[1]),pMem,p);
      try
        g:=TprGroup.Create(Report.prOwner);
        g.Report:=Report;
  
        p :=1;
        n :=ExtractSubStr(Buf,p,[#13]);
        if Report.Groups.IndexByName(n)=-1 then
          g.Name:=n;
  
        g.Valid:=ExtractSubStr(Buf,p,[#13]);
        
        n :=ExtractSubStr(Buf,p,[#13]);
        for i:=0 to Report.PagesCount-1 do
          for j:=0 to Report.Pages[i].Bands.Count-1 do
            if (Report.Pages[i].Bands[j].BandType in [bthDetail,btvDetail]) and
               (CompText(Report.Pages[i].Bands[j].Name,n)=0) then
              begin
                g.DetailBand:=Report.Pages[i].Bands[j];
                goto l1;
              end;
l1:
  
        UpdateGrid;
        Report.TemplateChanged:=true;
      finally
        GlobalUnlock(hMem);
      end;
    end;
finally
  ClipBoard.Close;
end;
end;

procedure TprGroupsEditorForm.aCopyExecute(Sender: TObject);
begin
CopyToClipBoard;
end;

procedure TprGroupsEditorForm.aCutExecute(Sender: TObject);
begin
CopyToClipBoard;
Report.Groups[Grid.Row-1].Free;
UpdateGrid;
Report.TemplateChanged:=true;
end;

initialization

Captions[0]:=prLoadStr(sGroupsEditorCaptionsName);
Captions[1]:=prLoadStr(sGroupsEditorCaptionsValid);
Captions[2]:=prLoadStr(sGroupsEditorCaptionsDetailBand);

end.

