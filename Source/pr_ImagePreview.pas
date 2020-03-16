{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_ImagePreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls,

  pr_Common, pr_MultiLang;

type
  TprImagePreviewForm = class(TprForm)
    Image: TImage;
    prMLRes1: TprMLRes;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CreateParams (var Params: TCreateParams); override;
  end;

implementation

{$R *.DFM}

procedure TprImagePreviewForm.FormShow(Sender: TObject);
begin
SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE +
  SWP_NOSIZE + SWP_NOACTIVATE + SWP_NOZORDER);

SetWindowLong (Handle, GWL_HWNDPARENT, Longint(TForm(Owner).Handle));
end;

procedure TprImagePreviewForm.CreateParams (var Params: TCreateParams);
begin
  inherited;
    with Params do begin
      { Note: WS_THICKFRAME and WS_BORDER styles are included to ensure that
        sizing grips are displayed on child controls with scrollbars. The
        thick frame or border is not drawn by Windows; TCustomToolWindow97
        handles all border drawing by itself. }
      Style := WS_POPUP or WS_BORDER or WS_THICKFRAME or WS_CAPTION;
      { The WS_EX_TOOLWINDOW style is needed so there isn't a taskbar button
        for the toolbar when FloatingMode = fmOnTopOfAllForms. }
      ExStyle := WS_EX_TOOLWINDOW;
    end;
end;

end.
