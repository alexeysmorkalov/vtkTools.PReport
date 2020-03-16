{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_PreviewPanelOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls,

  pr_Common, pr_Classes, pr_CommonPreviewPanel, pr_PreviewPanel, pr_MultiLang;

type
  TprPreviewPanelOptionsForm = class(TprForm)
    PC: TPageControl;
    bOK: TButton;
    bCancel: TButton;
    PGridStuck: TTabSheet;
    GroupBox1: TGroupBox;
    CBShowGrid: TCheckBox;
    CBUseGrid: TCheckBox;
    Label1: TLabel;
    EDGridSize: TEdit;
    UDGridSize: TUpDown;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    CBStuckMode: TComboBox;
    Label3: TLabel;
    EDStuckOffs: TEdit;
    UDStuckOffs: TUpDown;
    CBStuckOver: TCheckBox;
    prMLRes1: TprMLRes;
    PPopupForms: TTabSheet;
    GroupBox7: TGroupBox;
    Label6: TLabel;
    EDOpacityObjectsPropsForm: TEdit;
    UDOpacityObjectsPropsForm: TUpDown;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    function EditOptions(PreviewPanel : TprPreviewPanel) : boolean;
  end;

implementation

uses
  pr_Strings, pr_DesignerFunctions;
  
{$R *.DFM}

procedure TprPreviewPanelOptionsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

function TprPreviewPanelOptionsForm.EditOptions(PreviewPanel : TprPreviewPanel) : boolean;
var
  i : integer;
  so : TprPreviewStuckOptionsSet;
begin
for i:=integer(Low(TprStuckMode)) to integer(High(TprStuckMode)) do
  CBStuckMode.Items.Add(prLoadStr(sStuckModeCaptionsOffset-i));
with PreviewPanel do
  begin
    CBShowGrid.Checked := ShowGrid;
    CBUseGrid.Checked := UseGrid;
    UDGridSize.Position := GridSize;

    CBStuckMode.ItemIndex := integer(StuckMode);
    UDStuckOffs.Position := StuckOffs;
    CBStuckOver.Checked := prpsoStuckOver in StuckOptions;

    UDOpacityObjectsPropsForm.Position := OpacityObjectsPropsForm;
  end;
Result := ShowModal=mrOk;
if Result then
  with PreviewPanel do
    begin
      ShowGrid := CBShowGrid.Checked;
      UseGrid := CBUseGrid.Checked;
      GridSize := UDGridSize.Position;

      StuckMode := TprStuckMode(CBStuckMode.ItemIndex);
      StuckOffs := UDStuckOffs.Position;
      so := [];
      if CBStuckOver.Checked then
        Include(so,prpsoStuckOver);
      StuckOptions := so;

      OpacityObjectsPropsForm := UDOpacityObjectsPropsForm.Position;
    end;
end;

end.
