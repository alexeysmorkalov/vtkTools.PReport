{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_DesignerPanelOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls,

  pr_Common, pr_Classes, pr_CommonDesignerPanel, pr_DesignerPanel, pr_MultiLang;

type
  TprDesignerPanelOptionsForm = class(TprForm)
    PC: TPageControl;
    bOK: TButton;
    bCancel: TButton;
    PGridStuck: TTabSheet;
    POther: TTabSheet;
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
    CBStuckToBands: TCheckBox;
    CBStuckToObjects: TCheckBox;
    CBStuckOver: TCheckBox;
    GroupBox3: TGroupBox;
    CBHorizontalRulerVisible: TCheckBox;
    Label4: TLabel;
    EDHorizontalRulerUnits: TComboBox;
    GroupBox4: TGroupBox;
    Label5: TLabel;
    CBVerticalRulerVisible: TCheckBox;
    EDVerticalRulerUnits: TComboBox;
    GroupBox5: TGroupBox;
    CBHorizontalBandsCaptionsVisible: TCheckBox;
    GroupBox6: TGroupBox;
    CBVerticalBandsCaptionsVisible: TCheckBox;
    prMLRes1: TprMLRes;
    PPopupForms: TTabSheet;
    GroupBox7: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    EDOpacityObjectsPropsForm: TEdit;
    EDOpacityObjectLinksForm: TEdit;
    EDOpacityPosSizeForm: TEdit;
    UDOpacityObjectsPropsForm: TUpDown;
    UDOpacityObjectLinksForm: TUpDown;
    UDOpacityPosSizeForm: TUpDown;
    EDOpacityFindForm: TEdit;
    UDOpacityFindForm: TUpDown;
    Label10: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    function EditOptions(DesignerPanel : TprDesignerPanel) : boolean;
  end;

implementation

uses
  pr_Strings, pr_DesignerFunctions;
  
{$R *.DFM}

procedure TprDesignerPanelOptionsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

function TprDesignerPanelOptionsForm.EditOptions(DesignerPanel : TprDesignerPanel) : boolean;
var
  i : integer;
  so : TprStuckOptionsSet;
begin
for i:=integer(Low(TprStuckMode)) to integer(High(TprStuckMode)) do
  CBStuckMode.Items.Add(prLoadStr(sStuckModeCaptionsOffset-i));
for i:=integer(Low(TprPosSizeUnits)) to integer(High(TprPosSizeUnits)) do
  EDHorizontalRulerUnits.Items.Add(prLoadStr(sUnitsDescsOffset-i));
for i:=integer(Low(TprPosSizeUnits)) to integer(High(TprPosSizeUnits)) do
  EDVerticalRulerUnits.Items.Add(prLoadStr(sUnitsDescsOffset-i));
with DesignerPanel do
  begin
    CBShowGrid.Checked := ShowGrid;
    CBUseGrid.Checked := UseGrid;
    UDGridSize.Position := GridSize;

    CBStuckMode.ItemIndex := integer(StuckMode);
    UDStuckOffs.Position := StuckOffs;
    CBStuckToBands.Checked := prsoStuckToBands in StuckOptions;
    CBStuckToObjects.Checked := prsoStuckToObjects in StuckOptions;
    CBStuckOver.Checked := prsoStuckOver in StuckOptions;

    UDOpacityObjectsPropsForm.Position := OpacityObjectsPropsForm;
    UDOpacityObjectLinksForm.Position := OpacityObjectLinksForm;
    UDOpacityPosSizeForm.Position := OpacityPosSizeForm;
    UDOpacityFindForm.Position := OpacityFindForm;

    CBHorizontalRulerVisible.Checked := HorRulerOptions.Visible;
    EDHorizontalRulerUnits.ItemIndex := integer(HorRulerOptions.RulerUnits);
    CBVerticalRulerVisible.Checked := VerRulerOptions.Visible;
    EDVerticalRulerUnits.ItemIndex := integer(VerRulerOptions.RulerUnits);

    CBHorizontalBandsCaptionsVisible.Checked := HorBandsCaptionsOptions.Visible;
    CBVerticalBandsCaptionsVisible.Checked := VerBandsCaptionsOptions.Visible;
  end;
Result := ShowModal=mrOk;
if Result then
  with DesignerPanel do
    begin
      ShowGrid := CBShowGrid.Checked;
      UseGrid := CBUseGrid.Checked;
      GridSize := UDGridSize.Position;

      StuckMode := TprStuckMode(CBStuckMode.ItemIndex);
      StuckOffs := UDStuckOffs.Position;
      so := [];
      if CBStuckToBands.Checked then
        Include(so,prsoStuckToBands);
      if CBStuckToObjects.Checked then
        Include(so,prsoStuckToObjects);
      if CBStuckOver.Checked then
        Include(so,prsoStuckOver);
      StuckOptions := so;

      OpacityObjectsPropsForm := UDOpacityObjectsPropsForm.Position;
      OpacityObjectLinksForm := UDOpacityObjectLinksForm.Position;
      OpacityPosSizeForm := UDOpacityPosSizeForm.Position;
      OpacityFindForm := UDOpacityFindForm.Position;

      HorRulerOptions.Visible := CBHorizontalRulerVisible.Checked;
      HorRulerOptions.RulerUnits := TprPosSizeUnits(EDHorizontalRulerUnits.ItemIndex);
      VerRulerOptions.Visible := CBVerticalRulerVisible.Checked;
      VerRulerOptions.RulerUnits := TprPosSizeUnits(EDVerticalRulerUnits.ItemIndex);

      HorBandsCaptionsOptions.Visible := CBHorizontalBandsCaptionsVisible.Checked;
      VerBandsCaptionsOptions.Visible := CBVerticalBandsCaptionsVisible.Checked;
    end;
end;

end.
