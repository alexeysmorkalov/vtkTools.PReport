unit pr_BandHint;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, math, ExtCtrls,

  pr_Common;

const
  BorderOffs = 4;
  
type
  TprBandHintForm = class(TForm)
    LabelCaption: TLabel;
    Image: TImage;
    LabelHint: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FHintCaption : string;
    FHintText : TStringList;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    procedure ShowHint(const CaptionRect : TRect; Band : TprBand; IsVertical : boolean);
    procedure HideHint;
  end;

implementation

{$R *.DFM}

uses
  pr_CommonDesignerPanel;

procedure TprBandHintForm.CreateParams(var Params: TCreateParams);
begin
inherited CreateParams(Params);
with Params do
  begin
    Style := WS_POPUP or WS_BORDER;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    if NewStyleControls then ExStyle := WS_EX_TOOLWINDOW;
    AddBiDiModeExStyle(ExStyle);
  end;
end;

procedure TprBandHintForm.HideHint;
begin
SetWindowPos(Handle,0,0,0,0,0,SWP_HIDEWINDOW or
                              SWP_NOMOVE or
                              SWP_NOOWNERZORDER or
                              SWP_NOACTIVATE or
                              SWP_NOREPOSITION or
                              SWP_NOSIZE or
                              SWP_NOZORDER) ;
end;

procedure TprBandHintForm.ShowHint(const CaptionRect : TRect; Band : TprBand; IsVertical : boolean);
var
  r : TRect;
  Left,Top,Width,Height : integer;
begin                  
Band.DsgnGetHintText(FHintCaption,FHintText);
Image.Picture.Assign(BandImages[Band.BandType]);
LabelCaption.Left := Image.Left+Image.Width+4;
LabelHint.Left := Image.Left+Image.Width+4;
LabelCaption.Caption := FHintCaption;
LabelHint.Caption := FHintText.Text;

r := GetClientRect;
Width := Max(LabelCaption.Left+LabelCaption.Width,LabelHint.Left+LabelHint.Width)+BorderOffs+Self.Width-(r.Right-r.Left);
if FHintText.Count=0 then
  Height := Max(Image.Top+Image.Height,LabelCaption.Top+LabelCaption.Height)
else
  Height := Max(Image.Top+Image.Height,LabelHint.Top+LabelHint.Height);
Height := Height+BorderOffs+Self.Height-(r.Bottom-r.Top);

if IsVertical then
  begin
    if Width<Screen.Width-CaptionRect.Right then
      Left := CaptionRect.Right
    else
      if Width<CaptionRect.Left then
        Left := CaptionRect.Left-Width
      else
        Left := CaptionRect.Right;
    Top := Max(0,CaptionRect.Top);
    if Top+Height>Screen.Height then
      begin
        if Height>Screen.Height then
          Top := 0
        else
          Top := Screen.Height-Height;
      end;
  end
else
  begin
    if Height<Screen.Height-CaptionRect.Bottom then
      Top := CaptionRect.Bottom
    else
      if Height<CaptionRect.Top then
        Top := CaptionRect.Top-Height
      else
        Top := CaptionRect.Bottom;
    Left := Max(0,CaptionRect.Left);
    if Left+Width>Screen.Width then
      begin
        if Width>Screen.Width then
          Left := 0
        else
          Left := Screen.Width-Width;
      end;
  end;
SetWindowPos(Handle,HWND_TOP,Left,Top,Width,Height,SWP_SHOWWINDOW or SWP_NOACTIVATE);
end;

procedure TprBandHintForm.FormCreate(Sender: TObject);
begin
FHintText := TStringList.Create;
end;

procedure TprBandHintForm.FormDestroy(Sender: TObject);
begin
FHintText.Free;
end;

end.
