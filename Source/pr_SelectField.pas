{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_SelectField;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DB, ClipBrd, IniFiles,                    
       
  pr_Common, pr_MultiLang;

type
  TprSelectFieldForm = class(TprForm)
    Label2: TLabel;
    EDDataSet: TComboBox;
    LBFields: TListBox;
    Label1: TLabel;
    bOK: TButton;
    bCancel: TButton;
    prMLRes1: TprMLRes;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EDDataSetDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure EDDataSetClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure LBFieldsDblClick(Sender: TObject);
    procedure LBFieldsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Report : TprCustomReport;
    bmpField : TBitmap;
    DataSetName,FieldName : string;
  protected
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    procedure SelectField(_Report : TprCustomReport; Dest : TObject);
  end;

implementation

uses
  pr_Dataset;

{$R *.DFM}

procedure TprSelectFieldForm.prSaveProperties;
begin
inherited;
if EDDataSet.ItemIndex>=0 then
  Ini.WriteString(sn,'DataSetName',EDDataSet.Items[EDDataSet.ItemIndex]);
if LBFields.ItemIndex>=0 then
  Ini.WriteString(sn,'FieldName',LBFields.Items[LBFields.ItemIndex]);
end;

procedure TprSelectFieldForm.prRestoreProperties;
begin
inherited;
DataSetName :=Ini.ReadString(sn,'DataSetName','');
FieldName   :=Ini.ReadString(sn,'FieldName','');
end;

procedure TprSelectFieldForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action:=caFree;
end;

procedure TprSelectFieldForm.SelectField;
var
  s : string;
begin
Report:=_Report;
Report.GetAvailableDataSets(EDDataSet.Items);
EDDataSet.ItemIndex:=EDDAtaSet.Items.IndexOf(DataSetName);
EDDataSetClick(nil);
LBFields.ItemIndex :=LBFields.Items.IndexOf(FieldName);

if ShowModal=mrOk then
  begin
    s:=EDDataSet.Items[EDDataSet.ItemIndex]+'.'+LBFields.Items[LBFields.ItemIndex];

    if Dest is TEdit then
      TEdit(Dest).Text:=s
    else
      if Dest is TMemo then
        begin
          ClipBoard.Clear;
          ClipBoard.AsText:='['+s+']';
          TMemo(Dest).PasteFromClipboard;
        end;
  end;
end;

procedure TprSelectFieldForm.EDDataSetDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  D : TDataSet;
  s : string;
begin
D:=TDataSet(EDDataSet.Items.Objects[index]);
s:=EDDataSet.Items[index];

if D.Active then
  EDDataSet.Canvas.Font.Style:=[fsBold]
else
  EDDataSet.Canvas.Font.Style:=[];

EDDataSet.Canvas.FillRect(Rect);
EDDataSet.Canvas.TextOut(Rect.Left,Rect.Top+(Rect.Bottom-Rect.Top-EDDataSet.Canvas.TextHeight(s)) div 2,s);
end;

procedure TprSelectFieldForm.EDDataSetClick(Sender: TObject);
var
  i : integer;
  D : TDataSet;
  prD : TprDataset;
  fOpened : boolean;
begin
if EDDataset.ItemIndex=-1 then exit;

LBFields.Clear;
if EDDataSet.Items.Objects[EDDataset.ItemIndex] is TDataset then
  begin
    D:=TDataset(EDDataSet.Items.Objects[EDDataset.ItemIndex]);
    if not D.Active then
      begin
        D.Open;
        fOpened:=true;
      end
    else
      fOpened:=false;
    for i:=0 to D.FieldCount-1 do
      LBFields.Items.Add(D.Fields[i].FieldName);
    if fOpened then
      D.Close;
  end
else
  if EDDataSet.Items.Objects[EDDataset.ItemIndex] is TprDataset then
    begin
      prD:=TprDataset(EDDataSet.Items.Objects[EDDataset.ItemIndex]);
      prD.GetFieldsList(LBFields.Items);
    end;

if LBFields.Items.Count>0 then
  LBFields.ItemIndex:=0;
end;

procedure TprSelectFieldForm.bOKClick(Sender: TObject);
begin
if (EDDataSet.ItemIndex>=0) and (LBFields.ItemIndex>=0) then
  ModalResult:=mrOk
end;

procedure TprSelectFieldForm.LBFieldsDblClick(Sender: TObject);
begin
bOkClick(nil);
end;

procedure TprSelectFieldForm.LBFieldsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  s : string;
begin
LBFields.Canvas.FillRect(Rect);

s:=LBFields.Items[index];
LBFields.Canvas.Draw(Rect.Left,Rect.Top+(Rect.Bottom-Rect.Top-bmpField.Height) div 2,bmpField);
Rect.Left:=Rect.Left+bmpField.Width+1;

LBFields.Canvas.TextOut(Rect.Left,Rect.Top+(Rect.Bottom-Rect.Top-LBFields.Canvas.TextHeight(s)) div 2,s)
end;

procedure TprSelectFieldForm.FormCreate(Sender: TObject);
begin
bmpField:=TBitmap.Create;
LoadResImage(bmpField,'FIELD');
bmpField.TransparentMode:=tmAuto;
bmpField.Transparent    :=true;
end;

procedure TprSelectFieldForm.FormDestroy(Sender: TObject);
begin
bmpField.Destroy;
end;

end.

