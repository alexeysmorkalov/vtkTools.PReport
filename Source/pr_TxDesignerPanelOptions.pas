{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxDesignerPanelOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls,

  vgr_FontComboBox,

  pr_Common, pr_TxClasses, pr_CommonDesignerPanel, pr_TxDesignerPanel, pr_MultiLang;

type
  TprTxDesignerPanelOptionsForm = class(TprForm)
    PC: TPageControl;
    bOK: TButton;
    bCancel: TButton;
    POther: TTabSheet;
    GroupBox3: TGroupBox;
    CBHorizontalRulerVisible: TCheckBox;
    Label4: TLabel;
    GroupBox4: TGroupBox;
    Label5: TLabel;
    CBVerticalRulerVisible: TCheckBox;
    GroupBox5: TGroupBox;
    CBHorizontalBandsCaptionsVisible: TCheckBox;
    GroupBox6: TGroupBox;
    CBVerticalBandsCaptionsVisible: TCheckBox;
    prMLRes1: TprMLRes;
    PMain: TTabSheet;
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
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    EDFontSize: TEdit;
    UDFontSize: TUpDown;
    EDHorizontalRulerStep: TEdit;
    EDVerticalRulerStep: TEdit;
    UDHorizontalRulerStep: TUpDown;
    UDVerticalRulerStep: TUpDown;
    EDFontName: TvgrFontComboBox;
    Label10: TLabel;
    EDOpacityFindForm: TEdit;
    UDOpacityFindForm: TUpDown;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    function EditOptions(DesignerPanel : TprTxDesignerPanel) : boolean;
  end;

implementation

uses
  pr_Strings;
  
{$R *.DFM}

procedure TprTxDesignerPanelOptionsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

function TprTxDesignerPanelOptionsForm.EditOptions(DesignerPanel : TprTxDesignerPanel) : boolean;
begin
with DesignerPanel do
  begin
    EDFontName.ItemIndex := EDFontName.Items.IndexOf(Font.Name);
    UDFontSize.Position := Font.Size;

    UDOpacityObjectsPropsForm.Position := OpacityObjectsPropsForm;
    UDOpacityObjectLinksForm.Position := OpacityObjectLinksForm;
    UDOpacityPosSizeForm.Position := OpacityPosSizeForm;
    UDOpacityFindForm.Position := OpacityFindForm;

    CBHorizontalRulerVisible.Checked := HorRulerOptions.Visible;
    UDHorizontalRulerStep.Position := HorRulerOptions.RulerStep;
    CBVerticalRulerVisible.Checked := VerRulerOptions.Visible;
    UDVerticalRulerStep.Position := VerRulerOptions.RulerStep;

    CBHorizontalBandsCaptionsVisible.Checked := HorBandsCaptionsOptions.Visible;
    CBVerticalBandsCaptionsVisible.Checked := VerBandsCaptionsOptions.Visible;
  end;
Result := ShowModal=mrOk;
if Result then
  with DesignerPanel do
    begin
      Font.Name := EDFontName.Items[EDFontName.ItemIndex];
      Font.Size := UDFontSize.Position;

      OpacityObjectsPropsForm := UDOpacityObjectsPropsForm.Position;
      OpacityObjectLinksForm := UDOpacityObjectLinksForm.Position;
      OpacityFindForm := UDOpacityFindForm.Position;
      OpacityPosSizeForm := UDOpacityPosSizeForm.Position;
      
      HorRulerOptions.Visible := CBHorizontalRulerVisible.Checked;
      HorRulerOptions.RulerStep := UDHorizontalRulerStep.Position;
      VerRulerOptions.Visible := CBVerticalRulerVisible.Checked;
      VerRulerOptions.RulerStep := UDVerticalRulerStep.Position;

      HorBandsCaptionsOptions.Visible := CBHorizontalBandsCaptionsVisible.Checked;
      VerBandsCaptionsOptions.Visible := CBVerticalBandsCaptionsVisible.Checked;
    end;
end;

end.
