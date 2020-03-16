{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

unit pr_ImageEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Math, StdCtrls, ComCtrls, ExtCtrls, Buttons, ExtDlgs,

  pr_Common, pr_ImagePreview, pr_Classes, pr_CommonDesignerPanel,
  pr_MultiLang, vgr_ColorButton, vgr_Button;

type
  TprImageEditorForm = class(TprObjPropsForm)
    OpenPictureDialog: TOpenPictureDialog;
    Panel1: TPanel;
    Label4: TLabel;
    EDDrawMode: TComboBox;
    Label5: TLabel;
    GroupBox1: TGroupBox;
    SBPicture: TSpeedButton;
    SBPictureShow: TSpeedButton;
    SBFileName: TSpeedButton;
    SBDBFieldName: TSpeedButton;
    RBImage: TRadioButton;
    RBFileName: TRadioButton;
    RBDBFieldName: TRadioButton;
    EDFileName: TEdit;
    EDDBFieldName: TEdit;
    prMLRes1: TprMLRes;
    bFillColor: TvgrColorButton;
    procedure SBPictureClick(Sender: TObject);
    procedure SBFileNameClick(Sender: TObject);
    procedure SBDBFieldNameClick(Sender: TObject);
    procedure RBImageClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SBPictureShowClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Private declarations }
    ip : TprImagePreviewForm;
    procedure UpdateImageSource;
  protected
    procedure SetEnabledAfterCopyToControls; override;
    procedure CopySinglePropertiesFromControls(V : TprObjRecVersion); override;
    procedure CopyMultiplyPropertiesFromControls(L : TList); override;
    procedure CopySinglePropertiesToControls(V : TprObjRecVersion); override;
    procedure CopyMultiplyPropertiesToControls(L : TList); override;
  public
    { Public declarations }
  end;

implementation

uses
  pr_Strings, pr_SelectField, pr_FormatExpression, pr_DesignerFunctions;

{$R *.DFM}

procedure TprImageEditorForm.FormCreate(Sender: TObject);
begin
ip := TprImagePreviewForm.Create(Self);
LoadResImage(SBFileName.Glyph,'OPEN');
LoadResImage(SBDBFieldName.Glyph,'OPEN');
LoadResImage(SBPicture.Glyph,'OPEN');
prInitColorButtons([bFillColor]);
EDDrawMode.Items.Add(prLoadStr(sImageEditorDrawModeCenter));
EDDrawMode.Items.Add(prLoadStr(sImageEditorDrawModeStretch));
EDDrawMode.Items.Add(prLoadStr(sImageEditorDrawModeStretchProp));
EDDrawMode.Items.Add(prLoadStr(sImageEditorDrawModeAsImageSize));
end;

procedure TprImageEditorForm.CopySinglePropertiesFromControls;
begin
TprImageObjRecVersion(v).Picture.Assign(ip.Image.Picture);
inherited;
end;

procedure TprImageEditorForm.CopyMultiplyPropertiesFromControls;
begin
prSetProp(L,'DrawMode',EDDrawMode.ItemIndex,EDDrawMode.ItemIndex=-1);
prSetProp(L,'FillColor',bFillColor.SelectedColor,bFillColor.SelectedColor=clDefault);

if RBImage.Checked then
  prSetProp(L,'ImageSource',isPicture,false)
else
  if RBFileName.Checked then
    prSetProp(L,'ImageSource',isFileName,false)
  else
    if RBDBFieldName.Checked then
      prSetProp(L,'ImageSource',isDBFieldName,false);

prSetProp(L,'FileName',EDFileName.Text,EDFileName.Text=DesignerEmptyString);
prSetProp(L,'DBFieldName',EDDBFieldName.Text,EDDBFieldName.Text=DesignerEmptyString);

inherited;
end;

procedure TprImageEditorForm.SetEnabledAfterCopyToControls;
begin
sbPictureShow.Enabled := DesignerPanel.SelCount=1;
sbPicture.Enabled := DesignerPanel.SelCount=1;
rbImage.Enabled := DesignerPanel.SelCount=1;
end;

procedure TprImageEditorForm.CopySinglePropertiesToControls;
begin
ip.Image.Picture.Assign(TprImageObjRecVersion(v).Picture);
inherited;
end;

procedure TprImageEditorForm.CopyMultiplyPropertiesToControls;
var
  ims : TprImageSource;
begin
EDDrawMode.ItemIndex   :=prGetPropDef(L,'DrawMode',-1);
bFillColor.SelectedColor :=prGetPropDef(L,'FillColor',clDefault);

ims                    :=prGetPropDef(L,'ImageSource',-1);
RBImage.Checked        :=ims=isPicture;
RBFileName.Checked     :=ims=isFileName;
RBDBFieldName.Checked  :=ims=isDBFieldName;

EDFileName.Text        :=prGetPropDef(L,'FileName',DesignerEmptyString);
EDDBFieldName.Text     :=prGetPropDef(L,'DBFieldName',DesignerEmptyString);

UpdateImageSource;
inherited;
end;

procedure TprImageEditorForm.SBPictureClick(Sender: TObject);
begin
if OpenPictureDialog.Execute then
  begin
    ip.Image.Picture.LoadFromFile(OpenPictureDialog.FileName);
  end;
end;

procedure TprImageEditorForm.SBFileNameClick(Sender: TObject);
begin
OpenPictureDialog.FileName:=EDFileName.Text;
if OpenPictureDialog.Execute then
  begin
    EDFileName.Text:=OpenPictureDialog.FileName;
  end;
end;

procedure TprImageEditorForm.SBDBFieldNameClick(Sender: TObject);
begin
TprSelectFieldForm.Create(Application).SelectField(DesignerPanel.GetReport,EDDBFieldName);
end;

procedure TprImageEditorForm.UpdateImageSource;
begin
SBPictureShow.Enabled := RBImage.Checked;
SBPictureShow.Down := RBImage.Checked;
SBPicture.Enabled := RBImage.Checked;
ip.Visible := RBImage.Checked and Showing;

EDFileName.Enabled := RBFileName.Checked;
SBFileName.Enabled := RBFileName.Checked;

EDDBFieldName.Enabled := RBDBFieldName.Checked;
SBDBFieldName.Enabled := RBDBFieldName.Checked;
end;

procedure TprImageEditorForm.RBImageClick(Sender: TObject);
begin
UpdateImageSource;
end;

procedure TprImageEditorForm.SBPictureShowClick(Sender: TObject);
begin
ip.Visible := SBPictureShow.Down;
end;

procedure TprImageEditorForm.FormShow(Sender: TObject);
begin
ip.Visible := RBImage.Checked and Showing;
end;

procedure TprImageEditorForm.FormHide(Sender: TObject);
begin
ip.Visible := false;
end;

end.

