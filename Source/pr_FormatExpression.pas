{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_FormatExpression;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, DB, ClipBrd, iniFiles, Buttons,

  pr_Common, pr_Parser, pr_SelectFormat, pr_MultiLang, pr_Strings;

{$I pr.inc}

type
  TprFormatExpressionForm = class(TprForm)
    Label2: TLabel;
    CBCategory: TComboBox;
    Notebook1: TNotebook;
    LBSystemVars: TListBox;
    CBObjectName: TComboBox;
    Label1: TLabel;
    LBObject: TListBox;
    bOK: TButton;
    bCancel: TButton;
    Label4: TLabel;
    EDExpression: TEdit;
    Label5: TLabel;
    EDDisplayFormat: TEdit;
    bPaste: TSpeedButton;
    bDisplayFormat: TSpeedButton;
    prMLRes1: TprMLRes;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CBCategoryClick(Sender: TObject);
    procedure CBObjectNameClick(Sender: TObject);
    procedure LBObjectDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBSystemVarsClick(Sender: TObject);
    procedure LBObjectClick(Sender: TObject);
    procedure LBSystemVarsDblClick(Sender: TObject);
    procedure LBObjectDblClick(Sender: TObject);
    procedure bPasteClick(Sender: TObject);
    procedure bDisplayFormatClick(Sender: TObject);
  private
    { Private declarations }
    ObjectName : string;
    bmpField,bmpFunc,bmpProp : TBitmap;
    Report : TprCustomReport;
    SelectFormatForm : TprSelectFormatForm;

    procedure UpdateButtons;
  protected
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    procedure SelectExpression(_Report : TprCustomReport; Dest : TObject; var sDest : string; FormatEnable : boolean = true);
  end;

implementation

{$R *.DFM}

uses
  pr_Dataset;

procedure TprFormatExpressionForm.prSaveProperties;
begin
inherited;
Ini.WriteInteger(sn,'Category',CBCategory.ItemIndex);
if CBObjectName.ItemIndex>=0 then
  Ini.WriteString(sn,'ObjectName',CBObjectName.Items[CBObjectName.ItemIndex]);
end;

procedure TprFormatExpressionForm.prRestoreProperties;
begin
inherited;
CBCategory.ItemIndex :=Ini.ReadInteger(sn,'Category',0);
ObjectName           :=Ini.ReadString(sn,'ObjectName','');
end;

procedure TprFormatExpressionForm.FormDestroy(Sender: TObject);
begin
bmpFunc.Free;
bmpProp.Free;
bmpField.Free;
end;

procedure TprFormatExpressionForm.FormCreate(Sender: TObject);
begin
bmpFunc :=TBitmap.Create;
bmpProp :=TBitmap.Create;
bmpField:=TBitmap.Create;

LoadResImage(bmpFunc,'FUNC');
LoadResImage(bmpProp,'PROP');
LoadResImage(bmpField,'FIELD');
LoadResImage(bPaste.Glyph,'PASTE');
LoadResImage(bDisplayFormat.Glyph,'PROPERTIES');

CBCategory.Items[0]:=prLoadStr(sFormatExpressionCategoryReportVar);
CBCategory.Items[1]:=prLoadStr(sFormatExpressionCategoryObjectPtoperty);

bmpFunc.TransparentMode:=tmAuto;
bmpProp.TransparentMode:=tmAuto;
bmpField.TransparentMode:=tmAuto;
bmpFunc.Transparent:=true;
bmpProp.Transparent:=true;
bmpField.Transparent:=true;

if bmpFunc<>nil then
  LBObject.ItemHeight:=bmpFunc.Height+1
else
  if bmpProp<>nil then
    LBObject.ItemHeight:=bmpProp.Height+1
  else
    if bmpField<>nil then
      LBObject.ItemHeight:=bmpField.Height+1
    else
      LBObject.ItemHeight:=13;

SelectFormatForm:=TprSelectFormatForm.Create(Self);
end;

procedure TprFormatExpressionForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action:=caFree;
SelectFormatForm.Free;
end;

procedure TprFormatExpressionForm.SelectExpression;
var
  i : integer;
begin
Report:=_Report;

EDExpression.Text:=sDest;

bDisplayFormat.Enabled :=FormatEnable;
EDDisplayFormat.Enabled:=FormatEnable;
Label5.Enabled         :=FormatEnable;

// varialbles' list
for i:=0 to Report.AllValuesCount-1 do
  LBSystemVars.Items.Add(Report.AllValues[i].Name);
for i:=0 to Report.Variables.Count-1 do
  LBSystemVars.Items.Add(Report.Variables[i].Name);

// available objects
Report.GetAvailableComponents(CBObjectName.Items);

CBCategoryClick(nil);
CBObjectName.ItemIndex:=CBObjectName.Items.IndexOf(ObjectName);
if CBObjectName.ItemIndex<>-1 then
  CBObjectNameClick(nil);

if ShowModal=mrOk then
  begin
    if FormatEnable then
      sDest:=EDDisplayFormat.Text+EDExpression.Text
    else
      sDest:=EDExpression.Text;

    if Dest is TEdit then
      TEdit(Dest).Text:=sDest
    else
      if ((Dest is TMemo) or (Dest is TRichEdit))then
        begin
          sDest := '['+sDest+']';
          SendMessage(TWinControl(Dest).Handle,EM_REPLACESEL,0,integer(PChar(sDest)));
        end
  end;
end;

procedure TprFormatExpressionForm.CBCategoryClick(Sender: TObject);
begin
Notebook1.PageIndex:=CBCategory.ItemIndex;
UpdateButtons;
end;

procedure TprFormatExpressionForm.CBObjectNameClick(Sender: TObject);
var
  C : TComponent;
  fActive,fErrorOpen,f : boolean;
  L : TStringList;
  i,j : integer;
begin
LBObject.Clear;
// Заполняем свойства и методы объекта
C:=TComponent(CBObjectName.Items.Objects[CBObjectName.ItemIndex]);
if C is TDataSet then
  with TDataSet(C) do
    begin
      fActive   :=Active;
      fErrorOpen:=false;
      if not Active then
        try
          Open
        except
          fErrorOpen:=true;
        end;
      if not fErrorOpen then
        begin
          f:=true;
          for i:=0 to FieldCount-1 do
            begin
              if f then
                begin
                  f:=false;
                  LBObject.Items.AddObject(prLoadStr(sFormatExpressionFields),nil);
                end;
              LBObject.Items.AddObject(Fields[i].FieldName,pointer(3));
            end;

          if not fActive then
            Close;
        end;
    end
else
  if C is TprDataset then
    begin
      L:=TStringList.Create;
      try
        TprDataset(C).GetFieldsList(L);
        if L.Count>0 then
          LBObject.Items.AddObject(prLoadStr(sFormatExpressionFields),nil);
        for i:=0 to L.Count-1 do
          LBObject.Items.AddObject(L[i],pointer(3));
      finally
        L.Free;
      end;
    end;
    
f:=true;

for i:=1 to prMAX_OBJFUNCTIONS do
  if C is ObjFuncInfo[i].ObjClassRef then
    begin
      if f then
        begin
          f:=false;
          LBObject.Items.AddObject(prLoadStr(sFormatExpressionFunctions),nil);
        end;
      LBObject.Items.AddObject(ObjFuncInfo[i].FuncName,pointer(1));
    end;

L:=TStringList.Create;
try
  f:=true;
  for i:=1 to prMAX_OBJPROPS do
    if C is ObjPropInfo[i].ObjClassRef then
      begin
        if f then
          begin
            f:=false;
            LBObject.Items.AddObject(prLoadStr(sFormatExpressionProperties),nil);
          end;
        ObjPropInfo[i].GetPropsNamesFunc(C,L);
        for j:=0 to L.Count-1 do
          LBObject.Items.AddObject(L[j],pointer(2));
      end;
finally
  L.Free;
end;
UpdateButtons;
end;

procedure TprFormatExpressionForm.LBObjectDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  o : integer;
  s : string;
  b : TBitmap;
begin
o:=integer(LBObject.Items.Objects[Index]);
if (odSelected in State) and (o<>0) then
  LBObject.Canvas.Brush.Color:=clHighlight
else
  LBObject.Canvas.Brush.Color:=clWindow;
LBObject.Canvas.FillRect(Rect);

if o=0 then
  LBObject.Canvas.Font.Style:=[fsBold]
else
  LBObject.Canvas.Font.Style:=[];

if (odSelected in State) and (o<>0) then
  LBObject.Canvas.Font.Color:=clHighlightText
else
  LBObject.Canvas.Font.Color:=clWindowText;

case o of
  1: b:=bmpFunc;
  2: b:=bmpProp;
  3: b:=bmpField
else b:=nil;
end;

s:=LBObject.Items[index];
if o=0 then
  begin
    LBObject.Canvas.TextOut(Rect.Left,Rect.Top+(Rect.Bottom-Rect.Top-LBObject.Canvas.TextHeight(s)) div 2,s);
    LBObject.Canvas.Pen.Width:=2;
    LBObject.Canvas.MoveTo(LBObject.Canvas.TextWidth(s)+4+Rect.Left,Rect.Top+2+((Rect.Bottom-Rect.Top-LBObject.Canvas.Pen.Width) div 2));
    LBObject.Canvas.LineTo(Rect.Right,Rect.Top+2+((Rect.Bottom-Rect.Top-LBObject.Canvas.Pen.Width) div 2));
  end
else
  begin
    if b<>nil then
      begin
        LBObject.Canvas.Draw(Rect.Left,Rect.Top+(Rect.Bottom-Rect.Top-b.Height) div 2,b);
        Rect.Left:=Rect.Left+b.Width+1;
      end;

    LBObject.Canvas.TextOut(Rect.Left,Rect.Top+(Rect.Bottom-Rect.Top-LBObject.Canvas.TextHeight(s)) div 2,s)
  end;
end;

procedure TprFormatExpressionForm.UpdateButtons;
begin
bPaste.Enabled:=((CBCategory.ItemIndex=0) and (LBSystemVars.ItemIndex>=0)) or
                ((CBCategory.ItemIndex=1) and (CBObjectName.ItemIndex>=0) and (LBObject.ItemIndex>=0) and (LBObject.Items.Objects[LBObject.ItemIndex]<>nil));
end;

procedure TprFormatExpressionForm.LBSystemVarsClick(Sender: TObject);
begin
UpdateButtons;
end;

procedure TprFormatExpressionForm.LBObjectClick(Sender: TObject);
begin
UpdateButtons;
end;

procedure TprFormatExpressionForm.LBSystemVarsDblClick(Sender: TObject);
begin
bPasteClick(nil);
end;

procedure TprFormatExpressionForm.LBObjectDblClick(Sender: TObject);
begin
bPasteClick(nil);
end;

procedure TprFormatExpressionForm.bPasteClick(Sender: TObject);
var
  s : string;
begin
if CBCategory.ItemIndex=0 then
  begin
    if LBSystemVars.ItemIndex<0 then exit;
    s:=LBSystemVars.Items[LBSystemVars.ItemIndex];
  end
else
  begin
    if (CBObjectName.ItemIndex<0) or
       (LBObject.ItemIndex<0) or
       (LBObject.Items.Objects[LBObject.ItemIndex]=nil) then exit;
    if CBObjectName.Items.Objects[CBObjectName.ItemIndex]=Report then
      s:=LBObject.Items[LBObject.ItemIndex]
    else
      s:=CBObjectName.Items[CBObjectName.ItemIndex]+'.'+LBObject.Items[LBObject.ItemIndex];
    if integer(LBObject.Items.Objects[LBObject.ItemIndex])=1 then
      s:=s+'()'
  end;
  
ClipBoard.Clear;
ClipBoard.AsText:=s;
EDExpression.PasteFromClipboard;
end;

procedure TprFormatExpressionForm.bDisplayFormatClick(Sender: TObject);
var
  s : string;
begin
if SelectFormatForm.SelectFormat(s) then
  begin
    EDDisplayFormat.Text:=s;
  end;
end;

end.

