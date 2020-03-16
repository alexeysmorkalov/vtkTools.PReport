{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_PreviewObjectsProps;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, comctrls, inifiles, math,

  pr_Common, pr_PreviewPanel, pr_Utils, pr_MultiLang, pr_Classes,
  pr_DesignerFunctions;

{$I pr.inc}

type
  ///////////////////////////////
  //
  // TprPreviewObjectsPropsForm
  //
  ///////////////////////////////
  TprPreviewObjectsPropsForm = class(TprCustomPreviewObjectsPropsForm)
    prMLRes1: TprMLRes;
    Label1: TLabel;
    Panel1: TPanel;
    bOK: TButton;
    bCancel: TButton;
    bApply: TButton;
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    PropsForm : TprPreviewPropsForm;
    SizesPage : TTabSheet;
    EDUnits : TComboBox;
    aEdits : array [TprObjectPosSizeProps] of TEdit;
    aLabels : array [TprObjectPosSizeProps] of TLabel;
    fChanged : array [TprObjectPosSizeProps] of boolean;
    FUnits : TprPosSizeUnits;
    FUpdated : boolean;
    procedure EDUnitsClick(Sender : TObject);
    procedure OnPropChange(Sender : TObject);
    procedure ApplySizes;
    procedure Apply;
  protected
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    procedure UpdateInfo; override;
    procedure SetFocusOnFirstControl; override;
  end;

implementation

uses
  pr_Strings, pr_PrvMemoEditor, pr_PrvImageEditor, pr_PrvRichEditor, pr_PrvShapeEditor,
  pr_PrvBarCodeEditor;

{$R *.DFM}

/////////////////////////////////
//
// TprPreviewObjectsPropsForm
//
/////////////////////////////////
procedure TprPreviewObjectsPropsForm.prRestoreProperties;
begin
inherited;
FUnits := TprPosSizeUnits(Max(0,Ini.ReadInteger(sn,'CurrentUnits',0)));
end;

procedure TprPreviewObjectsPropsForm.prSaveProperties;
begin
inherited;
Ini.WriteInteger(sn,'CurrentUnits',Max(integer(FUnits),0));
end;

procedure TprPreviewObjectsPropsForm.SetFocusOnFirstControl;
begin
if PropsForm<>nil then
  PropsForm.SetFocusOnFirstControl
else
  inherited;
end;

procedure TprPreviewObjectsPropsForm.UpdateInfo;
var
  i : integer;
  v : TprExObjRecVersion;
  fc : TprPreviewPropsFormClass;
  PC : TPageControl;

  procedure FreePropsForm;
  begin
  if PropsForm<>nil then
    begin
      PropsForm.Free;
      PropsForm := nil;
    end
  else
    if SizesPage<>nil then
      SizesPage.PageControl.Free;
  SizesPage := nil;
  end;

  procedure CreateEdit(Prop : TprObjectPosSizeProps; _Left,_Top,CaptionResID : integer);
  begin
  aLabels[Prop] := TLabel.Create(SizesPage);
  with aLabels[Prop] do
    begin
      Parent := SizesPage;
      Caption := prLoadStr(CaptionResID);
      Left := _Left;
      Top := _Top;
      Width := 48;
      Height := 21;
      Layout := tlCenter;
      Alignment := taRightJustify;
    end;
  aEdits[Prop] := TEdit.Create(SizesPage);
  with aEdits[Prop] do
    begin
      Parent := SizesPage;
      Left := _Left+56;
      Top := _Top;
      Width := 65;
      Height := 21;
      Tag := integer(Prop);
      OnChange := OnPropChange;
    end;
  end;

  procedure UpdateEdit(Prop : TprObjectPosSizeProps);
  var
    i,v : integer;
    function GetPropValue(v : TprExObjRecVersion) : integer;
    begin
    Result := High(Integer);
    case Prop of
      prpsaLeft : Result := v.GeneratedRect.Left;
      prpsaRight : Result := v.GeneratedRect.Right;
      prpsaTop : Result := v.GeneratedRect.Top;
      prpsaBottom : Result := v.GeneratedRect.Bottom;
      prpsaWidth : Result := v.GeneratedRect.Right-v.GeneratedRect.Left;
      prpsaHeight : Result := v.GeneratedRect.Bottom-v.GeneratedRect.Top;
    end;
    end;
  begin
  v := GetPropValue(PreviewPanel.SelObjs[0]);
  i := 1;
  while (i<PreviewPanel.SelCount) and (v=GetPropValue(PreviewPanel.SelObjs[i])) do Inc(i);
  if i<PreviewPanel.SelCount then
    aEdits[Prop].Text := ''
  else
    aEdits[Prop].Text := prConvertFromPixelsString(v,TprPosSizeUnits(EDUnits.ItemIndex),Prop in [prpsaLeft,prpsaRight,prpsaWidth]);
  fChanged[Prop] := false;
  end;

begin
if PreviewPanel.SelCount<=0 then
  begin
    FreePropsForm;
    Caption := '';
    exit;
  end;

fc := nil;
i := 0;
v := PreviewPanel.SelObjs[0];
while (i<PreviewPanel.SelCount) and
      (CompText(PreviewPanel.SelObjs[i].ClassName,v.ClassName)=0) do Inc(i);

if i>=PreviewPanel.SelCount then
  begin
    // all objects has eq. class
    i := 0;
    while (i<Length(prObjRegInfos)) and
          (CompText(v.ClassName,prObjRegInfos[i].ClassRef.ClassName+'RecVersion')<>0) do Inc(i);
    if i<Length(prObjRegInfos) then
      fc := TprPreviewPropsFormClass(GetClass(prObjRegInfos[i].VersionPropsFormClassName));
  end
else
  begin
    // objects classes different
  end;

if ((PropsForm<>nil) and (fc=nil)) or
   ((PropsForm<>nil) and not (PropsForm is fc)) then
  FreePropsForm;

if (fc<>nil) and (PropsForm=nil) then
  begin
    PropsForm := fc.CreatePropsForm(Self,PreviewPanel);
    PropsForm.Align := alClient;
    PropsForm.Parent := Self;
  end;

if SizesPage=nil then
  begin
    if PropsForm=nil then
      begin
        PC := TPageControl.Create(Self);
        PC.Align := alClient;
        PC.Parent := Self;
      end
    else
      PC := TPageControl(PropsForm.FindComponent('PC'));

    // add sizes TabSheet
    if PC<>nil then
      begin
        SizesPage := TTabSheet.Create(PC);
        SizesPage.PageControl := PC;
        SizesPage.Caption := prLoadStr(sPreviewPropsFormSizesPageCaption);

        with TLabel.Create(SizesPage) do
          begin
            Parent := SizesPage;
            Caption := prLoadStr(sPreviewPropsFormUnitsCaption);
            Left := 4;
            Top := 4;
            Width := 48;
            Height := 21;
            Layout := tlCenter;
            Alignment := taRightJustify;
          end;
        EDUnits := TComboBox.Create(SizesPage);
        with EDUnits do
          begin
            Parent := SizesPage;
            Style := csDropDownList;
            Left := 4+56;
            Top := 4;
            Width := 185;
            Height := 21;
            OnClick := EDUnitsClick;
          end;
        for i:=integer(prpsuPixels) to integer(prpsuM) do
          EDUnits.Items.Add(prLoadStr(sUnitsDescsOffset-i));
        EDUnits.ItemIndex := integer(FUnits);

        CreateEdit(prpsaLeft,4,32,sPreviewPropsFormLeftSizeEditCaption);
        CreateEdit(prpsaTop,4,56,sPreviewPropsFormTopSizeEditCaption);
        CreateEdit(prpsaRight,4,80,sPreviewPropsFormRightSizeEditCaption);
        CreateEdit(prpsaBottom,4,104,sPreviewPropsFormBottomSizeEditCaption);
        CreateEdit(prpsaWidth,132,80,sPreviewPropsFormWidthSizeEditCaption);
        CreateEdit(prpsaHeight,132,104,sPreviewPropsFormHeightSizeEditCaption);
      end;
  end;

Caption := '';
if PropsForm<>nil then
  begin
    if PreviewPanel.SelCount=1 then
      Caption :=prLoadStr(sPreviewPropsFormOneObjectSelectedCaption)
    else
      Caption := Format(prLoadStr(sPropsFormManyObjectsSelectedCaption),[PreviewPanel.SelCount])
  end;

if PropsForm<>nil then
  PropsForm.UpdateInfo;
if SizesPage<>nil then
  begin
    fUpdated := true;
    for i:=integer(Low(TprObjectPosSizeProps)) to integer(High(TprObjectPosSizeProps)) do
      UpdateEdit(TprObjectPosSizeProps(i));
    fUpdated := false;
  end;
end;

procedure TprPreviewObjectsPropsForm.ApplySizes;
var
  i : TprObjectPosSizeProps;
  j : integer;
  sz : extended;
begin
for i:=Low(TprObjectPosSizeProps) to High(TprObjectPosSizeProps) do
  if fChanged[i] and TextToFloat(PChar(aEdits[i].Text),sz,fvExtended) then
    begin
      j := prConvertToPixels(sz,TprPosSizeUnits(EDUnits.ItemIndex),i in [prpsaLeft,prpsaRight,prpsaWidth]);
      PreviewPanel.SetPosSizeProp(i,j);
    end;
end;

procedure TprPreviewObjectsPropsForm.Apply;
begin
if SizesPage<>nil then
  ApplySizes;
if PropsForm<>nil then
  PropsForm.Apply;
if (SizesPage<>nil) or (PropsForm<>nil) then
  DoApplyObjectsProps;
end;

procedure TprPreviewObjectsPropsForm.bOKClick(Sender: TObject);
begin
Apply;
Close;
end;

procedure TprPreviewObjectsPropsForm.bCancelClick(Sender: TObject);
begin
if PropsForm<>nil then
  PropsForm.Cancel;
Close;
end;

procedure TprPreviewObjectsPropsForm.bApplyClick(Sender: TObject);
begin
Apply;
end;

procedure TprPreviewObjectsPropsForm.FormCreate(Sender: TObject);
begin
Caption := '';
end;

procedure TprPreviewObjectsPropsForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
if (Key=VK_ESCAPE) and (Shift=[]) then
  begin
    Key:=0;
    Close;
  end
else
if ((Key=VK_RETURN) and (Shift=[ssCtrl])) or
   ((Key=VK_RETURN) and (Shift=[ssAlt])) then
  begin
    Key := 0;
    Apply;
    PreviewPanel.SetFocus;
  end;
end;

procedure TprPreviewObjectsPropsForm.EDUnitsClick;
var
  i : TprObjectPosSizeProps;
  v : extended;
begin
for i:=Low(TprObjectPosSizeProps) to High(TprObjectPosSizeProps) do
  if TextToFloat(PChar(aEdits[i].Text),v,fvExtended) then
    aEdits[i].Text := prConvertFromPixelsString(prConvertToPixels(v,FUnits,i in [prpsaLeft,prpsaRight,prpsaWidth]),TprPosSizeUnits(EDUnits.ItemIndex),i in [prpsaLeft,prpsaRight,prpsaWidth]);
FUnits := TprPosSizeUnits(EDUnits.ItemIndex);
end;

procedure TprPreviewObjectsPropsForm.OnPropChange;
begin
if FUpdated then exit;
fChanged[TprObjectPosSizeProps(TEdit(Sender).Tag)] := true;
end;

initialization

RegisterClass(TprPrvMemoEditorForm);
RegisterClass(TprPrvImageEditorForm);
RegisterClass(TprPrvRichEditorForm);
RegisterClass(TprPrvShapeEditorForm);
RegisterClass(TprPrvBarCodeEditorForm);

end.

