{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_GridSize;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls,

  pr_Common, pr_MultiLang;

type
  TprGridSizeForm = class(TprForm)
    Label1: TLabel;
    EDGridSize: TEdit;
    UDGridSize: TUpDown;
    bOK: TButton;
    bCancel: TButton;
    prMLRes1: TprMLRes;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    function EditGridSize(var GridSize : integer) : boolean;
  end;

implementation

{$R *.DFM}

procedure TprGridSizeForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action := caFree;
end;

function TprGridSizeForm.EditGridSize;
begin
UDGridSize.Position := GridSize;
Result := ShowModal=mrOk;
if Result then
  GridSize := UDGridSize.Position;
end;

end.
