{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_GroupEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Pr_Utils,

  pr_Common, Buttons, pr_MultiLang;

type
  TprGroupEditorForm = class(TprForm)
    Label1: TLabel;
    EDName: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    EDValid: TEdit;
    CBDetailBand: TComboBox;
    bOK: TButton;
    bCancel: TButton;
    SBDBFieldName: TSpeedButton;
    prMLRes1: TprMLRes;
    procedure bOKClick(Sender: TObject);
    procedure SBDBFieldNameClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    G : TprGroup;

    function EditGroup(_G : TprGroup) : boolean;
  end;

implementation

uses pr_Strings, pr_SelectField;

{$R *.DFM}

function TprGroupEditorForm.EditGroup;
var
  i,j : integer;
begin
G:=_G;
EDName.Text:=G.Name;
EDValid.Text:=G.Valid;
for i:=0 to G.Report.PagesCount-1 do
  for j:=0 to G.Report.Pages[i].Bands.Count-1 do
    if G.Report.Pages[i].Bands[j].BandType in [bthDetail,btvDetail] then
      CBDetailBand.Items.AddObject(G.Report.Pages[i].Bands[j].Name,G.Report.Pages[i].Bands[j]);
CBDetailBand.ItemIndex:=CBDetailBand.Items.IndexOfObject(G.DetailBand);

Result:=ShowModal=mrOk;
end;

procedure TprGroupEditorForm.bOKClick(Sender: TObject);
begin
if Trim(EDName.Text)='' then
  begin
    MBError(prLoadStr(sNoGroupName));
    exit;
  end;
if Trim(EDValid.Text)='' then
  begin
    MBError(prLoadStr(sNoGroupValid));
    exit;
  end;
if CBDetailBand.ItemIndex<0 then
  begin
    MBError(prLoadStr(sNoGroupDetailBand));
    exit;
  end;

g.Name := EDName.Text;
g.Valid := EDValid.Text;
g.DetailBand := TprBand(CBDetailBand.Items.Objects[CBDetailBand.ItemIndex]);
g.Report.DsgnTemplateChanged(nil,true);

ModalResult := mrOk;
end;

procedure TprGroupEditorForm.SBDBFieldNameClick(Sender: TObject);
begin
TprSelectFieldForm.Create(Application).SelectField(g.Report,EDValid);
end;

procedure TprGroupEditorForm.FormCreate(Sender: TObject);
begin
LoadResImage(sbDBFieldName.Glyph,'OPEN');
end;

end.
