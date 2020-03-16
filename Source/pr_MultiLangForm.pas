{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_MultiLangForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, typinfo, math, ActnList, menus,

  pr_MultiLang, ImgList, ExtCtrls;

type
  TPropDesc = class(TObject)
  public
    Component : TComponent;
    PropName  : string;
    PropInfo  : PPropInfo;
    ResID     : integer;
    CurValue  : string;
    fOpened   : boolean;  
  end;

  TEditShowEvent = procedure (Sender: TObject; ACol, ARow: Longint; var AllowEdit: Boolean) of object;

  TprDrawGrid = class(TDrawGrid)
  private
    FOnShowEditor: TEditShowEvent;
  protected
    function CanEditShow : boolean; override;
  published
    property OnShowEditor: TEditShowEvent read FOnShowEditor write FOnShowEditor;
  end;

  TprMultiLangForm = class(TForm)
    ImageList: TImageList;
    Panel1: TPanel;
    EDProps: TComboBox;
    Label2: TLabel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EDPropsChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    { Private declarations }
    LAll  : TList;
    LUsed : TList;
    Grid  : TprDrawGrid;

    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure GridShowEditor(Sender : TObject; ACol,ARow : Longint; var AllowEdit : boolean);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure GridGetEditText(Sender: TObject; ACol, ARow: Longint; var Value: string);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Longint; const Value: string);

    procedure UpdateGrid;
  public
    { Public declarations }
    function EditRes(MLRes : TprMLRes; Form : TForm) : boolean;
  end;

implementation

{$R *.DFM}
{$R ..\res\prProp.res}

//////////////////////////////////
//
// TprDrawGrid
//
//////////////////////////////////
function TprDrawGrid.CanEditShow: Boolean;
begin
Result := inherited CanEditShow;
if Result and Assigned(FOnShowEditor) then
  begin
    FOnShowEditor(Self, Col, Row, Result);
    if not Result then
      EditorMode := False;
  end;
end;

/////////////////////////////////////////
//
// TprMultiLangForm
//
/////////////////////////////////////////
procedure TprMultiLangForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action:=caFree;
end;

procedure TprMultiLangForm.GridGetEditText;
var
  pd : TPropDesc;
begin
Value:='';
if (ARow<=LUsed.Count) then
  begin
    pd   :=TPropDesc(LUsed[ARow-1]);
    if pd.ResID<>-1 then
      Value:=IntToStr(pd.ResID);
  end;
end;

procedure TprMultiLangForm.GridSetEditText;
var
  pd : TPropDesc;
begin
if (ARow<=LUsed.Count) then
  begin
    pd      :=TPropDesc(LUsed[ARow-1]);
    pd.ResID:=StrToIntDef(Value,-1);
  end;
end;

procedure TprMultiLangForm.GridShowEditor;
var
  pd : TPropDesc;
begin
pd:=nil;
if (ARow<=LUsed.Count) then
  pd:=TPropDesc(LUsed[ARow-1]);
AllowEdit:=(pd<>nil) and (pd.PropInfo<>nil) and (ACol in [3]);
end;

procedure TprMultiLangForm.GridSelectCell;
begin
CanSelect:=ACol in [3];
end;

procedure TprMultiLangForm.GridDrawCell;
var
  s : string;
  pd : TPropDesc;
  LineRect : TRect;
begin
SetBkMode(Grid.Canvas.Handle,TRANSPARENT);
if ARow=0 then
  begin
    case ACol of
      1 : s:='Property';
      2 : s:='Current property value';
      3 : s:='String ID';
      4 : s:='String value';
    end;

    DrawFrameControl(Grid.Canvas.Handle,Rect,DFC_BUTTON,DFCS_BUTTONPUSH);
    Grid.Canvas.Font.Style:=[];
    DrawTextEx(Grid.Canvas.Handle,
               PChar(s),
               Length(s),
               Rect,
               DT_CENTER+DT_VCENTER+DT_SINGLELINE+DT_END_ELLIPSIS,nil);
  end
else
  begin
    if ARow<=LUsed.Count then
      begin
        pd:=TPropDesc(LUsed[ARow-1]);
        if pd.PropInfo=nil then
          begin
            Grid.Canvas.Brush.Color:=clBtnFace;
            Grid.Canvas.FillRect(Rect);
            case ACol of
              0 : begin
                    if pd.fOpened then
                      ImageList.Draw(Grid.Canvas,Rect.Left,Rect.Top,0)
                    else
                      ImageList.Draw(Grid.Canvas,Rect.Left,Rect.Top,1);
                  end;
              1 : begin
                    LineRect      :=Rect;
                    LineRect.Right:=Grid.ClientWidth;

                    Grid.Canvas.Font.Style:=[fsBold];
                    DrawTextEx(Grid.Canvas.Handle,
                               PChar(pd.Component.Name),
                               Length(pd.Component.Name),
                               LineRect,
                               DT_LEFT+DT_VCENTER+DT_SINGLELINE+DT_END_ELLIPSIS,nil);
                  end;
            end;
          end
        else
          begin
            Grid.Canvas.Brush.Color:=clWindow;
            Grid.Canvas.FillRect(Rect);
            Grid.Canvas.Font.Style:=[];
            case ACol of
              1: DrawTextEx(Grid.Canvas.Handle,PChar(pd.PropName),
                            Length(pd.PropName),Rect,
                            DT_LEFT+DT_VCENTER+DT_SINGLELINE+DT_END_ELLIPSIS,nil);
              2: DrawTextEx(Grid.Canvas.Handle,PChar(pd.CurValue),
                            Length(pd.CurValue),Rect,
                            DT_LEFT+DT_VCENTER+DT_SINGLELINE+DT_END_ELLIPSIS,nil);
              3: begin
                   if pd.ResID<>-1 then 
                     begin
                       s:=IntToStr(pd.ResID);
                       LineRect  :=Rect;
                       LineRect.Right:=LineRect.Right-2;
                       DrawTextEx(Grid.Canvas.Handle,PChar(s),
                                  Length(s),LineRect,
                                  DT_RIGHT+DT_VCENTER+DT_SINGLELINE+DT_END_ELLIPSIS,nil);
                     end;
                 end;
              4: begin
                   s:='';
                 end;
            end;
          end;
      end;
  end;

if gdFocused in State then
  DrawFocusRect(Grid.Canvas.Handle,Rect);
end;

procedure TprMultiLangForm.FormCreate(Sender: TObject);
begin
EDProps.ItemIndex     :=0;

LUsed:=TList.Create;
LAll :=TList.Create;
Grid :=TprDrawGrid.Create(Self);
with Grid do
  begin
    DefaultRowHeight:=18;
    ColCount        :=5;
    FixedCols       :=0;
    DefaultDrawing  :=false;
    ColWidths[0]    :=20;
    ColWidths[1]    :=100;
    ColWidths[2]    :=150;
    ColWidths[3]    :=70;
    ColWidths[4]    :=160;
    Align           :=alClient;
    Col             :=3;
    Row             :=1;

    Parent          :=Self;
    Options         :=[goVertLine,goHorzLine,goEditing];//    [,goVertLine,goHorzLine,goRangeSelect]

    OnDrawCell      :=GridDrawCell;
    OnShowEditor    :=GridShowEditor;
    OnSelectCell    :=GridSelectCell;
    OnGetEditText   :=GridGetEditText;
    OnSetEditText   :=GridSetEditText;
  end;
Grid.ColWidths[2]:=Grid.ClientWidth-
                   Grid.ColWidths[0]-
                   Grid.ColWidths[1]-
                   Grid.ColWidths[3]-
                   Grid.ColWidths[4]-
                   Grid.ColCount;

ActiveControl:=Grid;
end;

procedure TprMultiLangForm.FormDestroy(Sender: TObject);
var
  i : integer;
begin
LUsed.Free;
for i:=0 to LAll.Count-1 do
  TPropDesc(LAll[i]).Free;
LAll.Free;
end;

procedure TprMultiLangForm.UpdateGrid;
var
  i : integer;
  f : boolean;
  pd,pd2,pd3 : TPropDesc;
begin
LUsed.Clear;
i:=0;
while i<LAll.Count do
  begin
    pd:=TPropDesc(LAll[i]);
    if pd.PropInfo=nil then
      begin
        f  :=true;
        Inc(i);
        while (i<LAll.Count) and
              (TPropDesc(LAll[i]).Component=pd.Component) and
              (TPropDesc(LAll[i]).PropInfo<>nil) do
          begin
            pd2:=nil;
            pd3:=TPropDesc(LAll[i]);
            if EDProps.ItemIndex=0 then
              begin
                if (AnsiCompareText(pd3.PropName,'CAPTION')=0) or
                   (AnsiCompareText(pd3.PropName,'TEXT')=0) or
                   (AnsiCompareText(pd3.PropName,'TITLE')=0) or
                   (AnsiCompareText(pd3.PropName,'HINT')=0) then
                  pd2:=pd3;
              end
            else
              begin
                pd2:=pd3;
              end;
            if pd2<>nil then
              begin
                if f then
                  begin
                    LUsed.Add(pd);
                    f:=false;
                  end;
                LUsed.Add(pd2);
              end;
            Inc(i);
          end;
      end
    else
      Inc(i);
  end;

Grid.RowCount:=Max(2,LUsed.Count+1);
Grid.Repaint;
end;

function TprMultiLangForm.EditRes;
var
  pd : TPropDesc;
  rl : TprMLResLink;
  rp : TprMLResProp;
  i,j : integer;

  procedure AddComp(C : TComponent);
  var
    f : boolean;
    pl : PPropList;
    rl : TprMLResLink;
    pd : TPropDesc;
    i,j : integer;
    Count : integer;
  begin
  if not(c is TControl) and not(c is TAction) and not(c is TMenuItem) then exit;
  if (c is TControl) and (TControl(c).Action<>nil) then exit;
  if (c is TMenuItem) and (TMenuItem(c).Action<>nil) then exit;

  f    :=true;
  Count:=GetTypeData(C.ClassInfo)^.PropCount;
  GetMem(pl,Count*sizeof(pointer));
  try
    Count:=GetPropList(C.ClassInfo,[tkLString,tkString],pl);
    for i:=0 to Count-1 do
      if AnsiCompareText(pl^[i]^.Name,'NAME')<>0 then
        begin
          pd         :=TPropDesc.Create;
          pd.Component:=C;
          pd.PropName :=pl^[i]^.Name;
          pd.PropInfo :=pl^[i];
          pd.ResID    :=-1;
          pd.CurValue :=GetStrProp(pd.Component,pd.PropInfo);

          j:=0;
          while (j<MLRes.ResLinks.Count) and (TprMLResLink(MLRes.ResLinks.Items[j]).Component<>C) do Inc(j);
          if j<MLRes.ResLinks.Count then
            begin
              rl:=TprMLResLink(MLRes.ResLinks.Items[j]);
              j :=0;
              while (j<rl.Props.Count) and (AnsiCompareText(TprMLResProp(rl.Props.Items[j]).PropName,pd.PropName)<>0) do Inc(j);
              if j<rl.Props.Count then
                pd.ResID:=TprMLResProp(rl.Props.Items[j]).ResID;
            end;
          LAll.Add(pd);
          if f then
            begin
              pd:=TPropDesc.Create;
              pd.Component:=C;
              pd.PropInfo :=nil;
              pd.fOpened  :=true;
              LAll.Insert(LAll.Count-1,pd);
              f           :=false;
            end;
        end;
  finally
    FreeMem(pl);
  end;
  end;

begin
AddComp(Form);
for i:=0 to Form.ComponentCount-1 do
  AddComp(Form.Components[i]);
UpdateGrid;

Result:=ShowModal=mrOk;
if Result then
  begin
    MLRes.ResLinks.Clear;
    for i:=0 to LAll.Count-1 do
      begin
        pd:=TPropDesc(LAll[i]);
        if (pd.PropInfo<>nil) and (pd.ResID<>-1) then
          begin
            j:=MLRes.IndexOfComponent(pd.Component);
            if j=-1 then
              begin
                rl          :=TprMLResLink(MLRes.ResLinks.Add);
                rl.Component:=pd.Component;
              end
            else
              begin
                rl:=TprMLResLink(MLRes.ResLinks.Items[j]);
              end;
            rp         :=TprMLResProp(rl.Props.Add);
            rp.PropName:=pd.PropName;
            rp.ResID   :=pd.ResID;
          end;
      end;
  end;
end;

procedure TprMultiLangForm.EDPropsChange(Sender: TObject);
begin
UpdateGrid;
end;

procedure TprMultiLangForm.FormResize(Sender: TObject);
begin
Grid.ColWidths[2]:=Grid.ClientWidth-
                   Grid.ColWidths[0]-
                   Grid.ColWidths[1]-
                   Grid.ColWidths[3]-
                   Grid.ColWidths[4]-
                   Grid.ColCount;
end;

procedure TprMultiLangForm.Panel2Resize(Sender: TObject);
begin
Button2.Left:=Panel2.ClientWidth-Button1.Width-10;
Button1.Left:=Button2.Left-Button1.Width-10;
end;

procedure TprMultiLangForm.Panel1Resize(Sender: TObject);
begin
EDProps.Width:=Panel1.ClientWidth-EDProps.Left-10;
end;

end.
