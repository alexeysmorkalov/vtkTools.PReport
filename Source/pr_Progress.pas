{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_Progress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls,

  pr_MultiLang;


type
  ////////////////////////////
  //
  // Exception
  //
  ////////////////////////////
  EActionCanceled = class(Exception);

  TprProgressForm = class(TForm)
    ProgressBar1: TProgressBar;
    STDesc: TStaticText;
    bCancel: TButton;
    prMLRes1: TprMLRes;
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    fCancelPrepare : boolean;
    procedure InitPF(_Caption : string = ''; _Max : integer = 0);
    procedure UpdatePF(_Desc : string = ''; _Step : integer = 0);
  end;

implementation

uses pr_Strings;

{$R *.DFM}

procedure TprProgressForm.InitPF;
begin
if _Caption='' then
  Caption := prLoadStr(sDefaultProgressFormCaption)
else
  Caption := _Caption;

if _Max>0 then
  with ProgressBar1 do
    begin
      Min := 1;
      Max := _Max;
      Position := 1;
    end
else
  ProgressBar1.Visible := false;

STDesc.Caption := '';
Show;
end;

procedure TprProgressForm.UpdatePF;
begin
STDesc.Caption := _Desc;
if ProgressBar1.Visible then
  begin
    if _Step=0 then
      ProgressBar1.Position := ProgressBar1.Position+1
    else
      ProgressBar1.Position := ProgressBar1.Position+_Step;
  end;
Application.ProcessMessages;
if fCancelPrepare then
  raise EActionCanceled.Create(prLoadStr(sActionCanceled));
end;

procedure TprProgressForm.bCancelClick(Sender: TObject);
begin
fCancelPrepare := true;
end;

end.

