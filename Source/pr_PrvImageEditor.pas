{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_PrvImageEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons, ExtDlgs, clipbrd,

  vgr_ColorButton,
  
  pr_Classes, pr_PreviewPanel, pr_MultiLang;

{$I pr.inc}

type
  TprPrvImageEditorForm = class(TprPreviewPropsForm)
    PC: TPageControl;
    PCommon: TTabSheet;
    Label4: TLabel;
    EDDrawMode: TComboBox;
    Label5: TLabel;
    bCopy: TSpeedButton;
    bPaste: TSpeedButton;
    bOpen: TSpeedButton;
    Image: TImage;
    bSave: TSpeedButton;
    OpenPictureDialog: TOpenPictureDialog;
    prMLRes1: TprMLRes;
    SavePictureDialog: TSavePictureDialog;
    bFillColor: TvgrColorButton;
    procedure PCommonResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bCopyClick(Sender: TObject);
    procedure bPasteClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
  private
    { Private declarations }
    FImageChanged : boolean;
  protected
    procedure CopyPropertiesFromControls(l : TList); override;
    procedure CopyPropertiesToControls(l : TList); override;
  public
    { Public declarations }
  end;

implementation

uses
  pr_Strings, pr_DesignerFunctions, pr_Common;

{$R *.DFM}

procedure TprPrvImageEditorForm.PCommonResize(Sender: TObject);
begin
bCopy.Left := PCommon.ClientWidth-bCopy.Width-4;
bPaste.Left := bCopy.Left;
bOpen.Left := bCopy.Left;
bSave.Left := bCopy.Left;
Image.Width := bCopy.Left-EDDrawMode.Left-EDDrawMode.Width-12;
Image.Height := PCommon.ClientHeight-12;
end;

procedure TprPrvImageEditorForm.FormCreate(Sender: TObject);
begin
prInitColorButtons([bFillColor]);
LoadResImage(bCopy.Glyph,'COPY');
LoadResImage(bPaste.Glyph,'PASTE');
LoadResImage(bOpen.Glyph,'OPEN');
LoadResImage(bSave.Glyph,'SAVE');
EDDrawMode.Items.Add(prLoadStr(sImageEditorDrawModeCenter));
EDDrawMode.Items.Add(prLoadStr(sImageEditorDrawModeStretch));
EDDrawMode.Items.Add(prLoadStr(sImageEditorDrawModeStretchProp));
EDDrawMode.Items.Add(prLoadStr(sImageEditorDrawModeAsImageSize));
end;

procedure TprPrvImageEditorForm.CopyPropertiesFromControls;
var
  i : integer;
begin
prSetProp(L,'DrawMode',EDDrawMode.ItemIndex,EDDrawMode.ItemIndex=-1);
prSetProp(L,'FillColor',bFillColor.SelectedColor,bFillColor.SelectedColor=clDefault);
if FImageChanged then
  for i:=0 to l.Count-1 do
    TprImageObjRecVersion(l[i]).Picture.Assign(Image.Picture);
FImageChanged := false;
end;

procedure TprPrvImageEditorForm.CopyPropertiesToControls;
begin
Image.Visible := l.Count=1;
if Image.Visible then
  Image.Picture.Assign(TprImageObjRecVersion(l[0]).Picture);
EDDrawMode.ItemIndex := prGetPropDef(L,'DrawMode',-1);
bFillColor.SelectedColor := prGetPropDef(L,'FillColor',clDefault);
end;

procedure TprPrvImageEditorForm.bCopyClick(Sender: TObject);
var
  AFormat : WORD;
  AData : cardinal;
  APalette : HPALETTE;
begin
if not Image.Visible then exit;
Clipboard.Open;
try
  Image.Picture.SaveToClipboardFormat(AFormat,AData,APalette);
  SetClipboardData(AFormat,AData);
  if APalette<>0 then
    SetClipboardData(CF_PALETTE,APalette);
finally
  Clipboard.Close;
end;
end;

procedure TprPrvImageEditorForm.bPasteClick(Sender: TObject);
var
  i : integer;
begin
Clipboard.Open;
try
  for i:=0 to Clipboard.FormatCount-1 do
    if Image.Picture.SupportsClipboardFormat(Clipboard.Formats[i]) then
      begin
        Image.Picture.LoadFromClipboardFormat(Clipboard.Formats[i],Clipboard.GetAsHandle(Clipboard.Formats[i]),0);
        Image.Visible := true;
        FImageChanged := true;
        exit;
      end;
finally
  Clipboard.Close;
end;
end;

procedure TprPrvImageEditorForm.bOpenClick(Sender: TObject);
begin
if OpenPictureDialog.Execute then
  begin
    Image.Picture.LoadFromFile(OpenPictureDialog.FileName);
    FImageChanged := true;
    Image.Visible := true;
  end;
end;

procedure TprPrvImageEditorForm.bSaveClick(Sender: TObject);
begin
if not Image.Visible then exit;
if SavePictureDialog.Execute then
  Image.Picture.LoadFromFile(SavePictureDialog.FileName);
end;

end.
