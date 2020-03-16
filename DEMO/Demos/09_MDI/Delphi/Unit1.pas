unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Db, DBTables, pr_Common, pr_Classes, pr_Designer, pr_TxClasses, pr_TxDesigner;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Opendesigner1: TMenuItem;
    Openpreview1: TMenuItem;
    N1: TMenuItem;
    Close1: TMenuItem;
    Table1: TTable;
    N2: TMenuItem;
    OpenTprReportdesigner1: TMenuItem;
    OpenTprTxReportpreview1: TMenuItem;
    Windows1: TMenuItem;
    Tile1: TMenuItem;
    Cascade1: TMenuItem;
    mAllow: TMenuItem;
    N3: TMenuItem;
    procedure Close1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Opendesigner1Click(Sender: TObject);
    procedure Openpreview1Click(Sender: TObject);
    procedure OpenTprReportdesigner1Click(Sender: TObject);
    procedure OpenTprTxReportpreview1Click(Sender: TObject);
    procedure Tile1Click(Sender: TObject);
    procedure Cascade1Click(Sender: TObject);
    procedure mAllowClick(Sender: TObject);
  private
    { Private declarations }
    L : TList;
    fDestroy : boolean;
    procedure OnDestroyDesigner(Sender : TObject);
    procedure OnDestroyPreview(Sender : TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Close1Click(Sender: TObject);
begin
Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
L := TList.Create;
fDestroy := false;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
fDestroy := true;
while L.Count>0 do
  begin
    TprCustomReport(L[0]).Free;
    L.Delete(0);
  end;
L.Free;
L := nil;
end;

procedure TForm1.Opendesigner1Click(Sender: TObject);
var
  r : TprReport;
begin
r := TprReport.Create(Self);
r.LoadTemplateFromFile(ExtractFilePath(ParamStr(0))+'template.prt',false);
r.DesignerFormMode := fmMDIChild;
r.PreviewFormMode := fmMDIChild;
r.CanUserEdit := mAllow.Checked;
if R.CanUserEdit then
  begin
    R.PreviewParams.Options := [prpoAllowChangePreviewMode,prpoShowMenu,prpoAllowShowHideToolbars,prpoAllowDragToolbars];
    R.PreviewParams.ShowToolbars := [prptPreviewCommon,prptEdit,prptInsertObject,prptText,prptBorders,prptAlign,prptSize,prptNudge,prptObjects,prptObject];
  end
else
  begin
    R.PreviewParams.Options := [];
    R.PreviewParams.ShowToolbars := [prptPreviewCommon];
  end;
r.OnDestroyDesigner := OnDestroyDesigner;
L.Add(r);
r.DesignReport(false);
end;

procedure TForm1.Openpreview1Click(Sender: TObject);
var
  r : TprReport;
begin
r := TprReport.Create(Self);
r.LoadTemplateFromFile(ExtractFilePath(ParamStr(0))+'template.prt',false);
r.DesignerFormMode := fmMDIChild;
r.PreviewFormMode := fmMDIChild;
r.OnDestroyPreview := OnDestroyPreview;
r.CanUserEdit := mAllow.Checked;
if R.CanUserEdit then
  begin
    R.PreviewParams.Options := [prpoAllowChangePreviewMode,prpoShowMenu,prpoAllowShowHideToolbars,prpoAllowDragToolbars];
    R.PreviewParams.ShowToolbars := [prptPreviewCommon,prptEdit,prptInsertObject,prptText,prptBorders,prptAlign,prptSize,prptNudge,prptObjects,prptObject];
  end
else
  begin
    R.PreviewParams.Options := [];
    R.PreviewParams.ShowToolbars := [prptPreviewCommon];
  end;
L.Add(r);
if r.PrepareReport then
  r.PreviewPreparedReport(false);
end;

procedure TForm1.OnDestroyDesigner(Sender : TObject);
begin
if fDestroy then exit;
L.Remove(Sender);
TprCustomReport(Sender).Free;
end;

procedure TForm1.OnDestroyPreview(Sender : TObject);
begin
if fDestroy then exit;
L.Remove(Sender);
TprCustomReport(Sender).Free;
end;

procedure TForm1.OpenTprReportdesigner1Click(Sender: TObject);
var
  r : TprTxReport;
begin
r := TprTxReport.Create(Self);
r.LoadTemplateFromFile(ExtractFilePath(ParamStr(0))+'txtempl.prt',false);
r.DesignerFormMode := fmMDIChild;
r.PreviewFormMode := fmMDIChild;
r.OnDestroyDesigner := OnDestroyDesigner;
L.Add(r);
r.DesignReport(false);
end;

procedure TForm1.OpenTprTxReportpreview1Click(Sender: TObject);
var
  r : TprTxReport;
begin
r := TprTxReport.Create(Self);
r.LoadTemplateFromFile(ExtractFilePath(ParamStr(0))+'txtempl.prt',false);
r.DesignerFormMode := fmMDIChild;
r.PreviewFormMode := fmMDIChild;
r.OnDestroyPreview := OnDestroyPreview;
L.Add(r);
if r.PrepareReport then
  r.PreviewPreparedReport(false);
end;

procedure TForm1.Tile1Click(Sender: TObject);
begin
Tile;
end;

procedure TForm1.Cascade1Click(Sender: TObject);
begin
Cascade;
end;

procedure TForm1.mAllowClick(Sender: TObject);
begin
mAllow.Checked := not mAllow.Checked;
end;

end.

