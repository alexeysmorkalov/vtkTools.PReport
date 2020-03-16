{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxPreviewPanelOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, pr_MultiLang, ComCtrls,

  pr_Common, pr_TxPreviewPanel, vgr_ColorButton, vgr_FontComboBox;

type
  TprTxPreviewPanelOptionsForm = class(TprForm)
    PC: TPageControl;
    PMain: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    EDFontSize: TEdit;
    UDFontSize: TUpDown;
    EDFontName: TvgrFontComboBox;
    prMLRes1: TprMLRes;
    bOK: TButton;
    bCancel: TButton;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    bBackColor: TvgrColorButton;
    bTextColor: TvgrColorButton;
    Label4: TLabel;
    Label5: TLabel;
    bFindedBackColor: TvgrColorButton;
    bFindedTextColor: TvgrColorButton;
    Label6: TLabel;
    Label7: TLabel;
    bSelectionBackColor: TvgrColorButton;
    bSelectionTextColor: TvgrColorButton;
    Label8: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    function EditOptions(PreviewPanel : TprTxPreviewPanel) : boolean;
  end;

implementation

{$R *.DFM}

procedure TprTxPreviewPanelOptionsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

function TprTxPreviewPanelOptionsForm.EditOptions(PreviewPanel : TprTxPreviewPanel) : boolean;
begin
EDFontName.ItemIndex := EDFontName.Items.IndexOf(PreviewPanel.Font.Name);
UDFontSize.Position := PreviewPanel.Font.Size;

bBackColor.SelectedColor := PreviewPanel.Color;
bTextColor.SelectedColor := PreviewPanel.TextColor;

bFindedBackColor.SelectedColor := PreviewPanel.FindedBackColor;
bFindedTextColor.SelectedColor := PreviewPanel.FindedTextColor;

bSelectionBackColor.SelectedColor := PreviewPanel.SelectionBackColor;
bSelectionTextColor.SelectedColor := PreviewPanel.SelectionTextColor;

Result := ShowModal=mrOk;
if Result then
  with PreviewPanel do
    begin
      Font.Name := EDFontName.Items[EDFontName.ItemIndex];
      Font.Size := UDFontSize.Position;
      
      Color := bBackColor.SelectedColor;
      TextColor := bTextColor.SelectedColor;
      
      FindedBackColor := bFindedBackColor.SelectedColor;
      FindedTextColor := bFindedTextColor.SelectedColor;
      
      SelectionBackColor := bSelectionBackColor.SelectedColor;
      SelectionTextColor := bSelectionTextColor.SelectedColor;
    end;
end;

end.
