{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_ObjectsProps;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,

  pr_Common, pr_Utils, pr_MultiLang, pr_CommonDesignerPanel;

type
  TprObjectsPropsForm = class(TprCustomObjectsPropsForm)
    Label1: TLabel;
    Panel1: TPanel;
    bOK: TButton;
    bCancel: TButton;
    bApply: TButton;
    prMLRes1: TprMLRes;
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  protected
    PropsForm : TprPropsForm;
  public
    { Public declarations }
    procedure UpdateInfo; override;
    procedure SetFocusOnFirstControl; override; 
  end;

implementation

uses
  pr_Strings;

{$R *.DFM}

procedure TprObjectsPropsForm.SetFocusOnFirstControl;
begin
if PropsForm<>nil then
  PropsForm.SetFocusOnFirstControl
else
  inherited;
end;

procedure TprObjectsPropsForm.UpdateInfo;
var
  i : integer;
  fc : TprPropsFormClass;
  dc : TprDesignComponent;
begin
if DesignerPanel.SelCount<=0 then
  begin
    if PropsForm<>nil then
      begin
        PropsForm.Free{Release};
        PropsForm := nil;
      end;
    Caption := '';
    exit;
  end;

i := 1;
dc := DesignerPanel.SelObjs[0];
while (i<DesignerPanel.SelCount) and
      (AnsiCompareText(DesignerPanel.SelObjs[i].ClassName,dc.ClassName)=0) do Inc(i);
if i>=DesignerPanel.SelCount then
  fc := GetFormEditorClassForClass(dc.ClassName)
else
  fc := nil;

if ((PropsForm<>nil) and (fc=nil)) or
   ((PropsForm<>nil) and not (PropsForm is fc)) then
  begin
    PropsForm.Free{Release};
    PropsForm := nil;
  end;

if (fc<>nil) and (PropsForm=nil) then
  begin
    PropsForm := fc.CreatePropsForm(Self,DesignerPanel);
    PropsForm.Align := alClient;
    PropsForm.Parent := Self;
  end;

Caption := '';
if PropsForm<>nil then
  begin
    if DesignerPanel.SelCount=1 then
      begin
        dc := DesignerPanel.SelObjs[0];
        if dc is TprObj then
          Caption := TprObj(dc).GetDesc
        else
          if dc is TprBand then
            Caption := TprBand(dc).GetDrawDesignCaption
          else
            Caption := dc.ClassName;
      end
    else
      Caption := Format(prLoadStr(sPropsFormManyObjectsSelectedCaption),[DesignerPanel.SelCount])
  end;

if PropsForm<>nil then
  PropsForm.UpdateInfo;
end;

procedure TprObjectsPropsForm.bOKClick(Sender: TObject);
begin
if PropsForm<>nil then
  begin
    PropsForm.Apply;
    DoApplyObjectsProps;
  end;
Close;
end;

procedure TprObjectsPropsForm.bCancelClick(Sender: TObject);
begin
if PropsForm<>nil then
  PropsForm.Cancel;
Close;
end;

procedure TprObjectsPropsForm.bApplyClick(Sender: TObject);
begin
if PropsForm<>nil then
  begin
    PropsForm.Apply;
    DoApplyObjectsProps;
  end;
end;

procedure TprObjectsPropsForm.FormCreate(Sender: TObject);
begin
Caption := '';
end;

procedure TprObjectsPropsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    Key := 0;
    Close;
  end
else
if ((Key=VK_RETURN) and (Shift=[ssCtrl])) or
   ((Key=VK_RETURN) and (Shift=[ssAlt])) then
  begin
    Key := 0;
    if PropsForm<>nil then
      begin
        PropsForm.Apply;
        DoApplyObjectsProps;
      end;
    DesignerPanel.SetFocus;
  end;
end;

procedure TprObjectsPropsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
if Key=#10 then
  Key := #0; // Ctrl+Enter pressed
end;

end.
