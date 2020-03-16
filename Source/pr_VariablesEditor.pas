{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_VariablesEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, ToolWin, StdCtrls, ImgList, clipbrd, ActnList,
  typinfo, inifiles,

  pr_Common, pr_MultiLang;

{$I pr.inc}

type
  TprVariablesEditorForm = class(TprForm)
    ToolBar1: TToolBar;
    bNewVersion: TToolButton;
    bEditVersion: TToolButton;
    bDelVersion: TToolButton;
    ToolButton4: TToolButton;
    bCut: TToolButton;
    bCopy: TToolButton;
    bPaste: TToolButton;
    Panel1: TPanel;
    LV: TListView;
    Label1: TLabel;
    EDName: TEdit;
    EDType: TComboBox;
    Label2: TLabel;
    EDValue: TEdit;
    Label3: TLabel;
    prMLRes1: TprMLRes;
    ImageList1: TImageList;
    ActionList1: TActionList;
    aNewVersion: TAction;
    aEditVersion: TAction;
    aDelVersion: TAction;
    aCopy: TAction;
    aCut: TAction;
    aPaste: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LVSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure EDNameChange(Sender: TObject);
    procedure EDNameExit(Sender: TObject);
    procedure EDNameKeyPress(Sender: TObject; var Key: Char);
    procedure EDTypeClick(Sender: TObject);
    procedure aNewVersionExecute(Sender: TObject);
    procedure LVEdited(Sender: TObject; Item: TListItem; var S: String);
    procedure LVKeyPress(Sender: TObject; var Key: Char);
    procedure aEditVersionUpdate(Sender: TObject);
    procedure aEditVersionExecute(Sender: TObject);
    procedure aDelVersionUpdate(Sender: TObject);
    procedure aDelVersionExecute(Sender: TObject);
    procedure aPasteUpdate(Sender: TObject);
    procedure aPasteExecute(Sender: TObject);
    procedure aCopyUpdate(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aCutUpdate(Sender: TObject);
    procedure aCutExecute(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    { Private declarations }
    function GetVariableTypeIndex(Variable : TprVariable) : integer;
    function GetVariableTypeDesc(Variable : TprVariable) : string;
    function GetVariableName : string;
    procedure UpdateLVItem(Variable : TprVariable; Item : TListItem);
    procedure CopyFromNameAndValue;
    function CheckName(Variable : TprVariable; const NewName : string) : boolean;
  protected
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    Report : TprCustomReport;
    procedure EditVariables(_Report : TprCustomReport; _Variable : TprVariable);
  end;

implementation

uses
  pr_Strings, pr_Utils, pr_DesignerFunctions;
  
{$R *.DFM}

procedure TprVariablesEditorForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
Action := caFree;
end;

procedure TprVariablesEditorForm.FormCreate(Sender: TObject);
begin
prLoadResImages(Self,ImageList1);
LV.Columns[0].Caption := prLoadStr(sVariablesEditorFormName);
LV.Columns[1].Caption := prLoadStr(sVariablesEditorFormType);
LV.Columns[2].Caption := prLoadStr(sVariablesEditorFormValue);
EDType.Items.Add(prLoadStr(sVariablesEditorFormValueFormula));
EDType.Items.Add(prLoadStr(sVariablesEditorFormValueInteger));
EDType.Items.Add(prLoadStr(sVariablesEditorFormValueDateTime));
EDType.Items.Add(prLoadStr(sVariablesEditorFormValueString));
EDType.Items.Add(prLoadStr(sVariablesEditorFormValueDouble));
EDType.Items.Add(prLoadStr(sVariablesEditorFormValueNull));
end;

procedure TprVariablesEditorForm.prRestoreProperties;
var
  i : integer;
begin
inherited;
for i:=0 to LV.Columns.Count-1 do
  LV.Columns[i].Width := Ini.ReadInteger(sn,'Col'+IntToStr(i),LV.Columns[i].Width);
end;

procedure TprVariablesEditorForm.prSaveProperties;
var
  i : integer;
begin
inherited;
for i:=0 to LV.Columns.Count-1 do
  Ini.WriteInteger(sn,'Col'+IntToStr(i),LV.Columns[i].Width);
end;

function TprVariablesEditorForm.GetVariableTypeDesc(Variable : TprVariable) : string;
const
  aCaptions : array [TprVarValueType] of integer = (0,
                                                    sVariablesEditorFormValueInteger,
                                                    sVariablesEditorFormValueDateTime,
                                                    sVariablesEditorFormValueString,
                                                    sVariablesEditorFormValueDouble,
                                                    0,
                                                    sVariablesEditorFormValueNull,
                                                    0);
begin
if Variable.Calculated then
  Result := prLoadStr(sVariablesEditorFormValueFormula)
else
  Result := prLoadStr(aCaptions[Variable.VarValue.vType]);
end;

function TprVariablesEditorForm.GetVariableTypeIndex(Variable : TprVariable) : integer;
const
  aIndexes : array [TprVarValueType] of integer = (-1,
                                                   1,
                                                   2,
                                                   3,
                                                   4,
                                                   -1,
                                                   5,
                                                   -1);
begin
if Variable.Calculated then
  Result := 0
else
  Result := aIndexes[Variable.VarValue.vType];
end;

function TprVariablesEditorForm.GetVariableName : string;
var
  i : integer;
begin
i := 1;
Result := 'Variable';
while Report.Variables.IndexByName(Result+IntToStr(i))<>-1 do Inc(i);
Result := Result+IntToStr(i);
end;

procedure TprVariablesEditorForm.UpdateLVItem(Variable : TprVariable; Item : TListItem);
begin
with Item do
  begin
    Caption := Variable.Name;
    SubItems.Clear;
    SubItems.Add(GetVariableTypeDesc(Variable));
    SubItems.Add(Variable.AsString);
    Data := Variable;
  end;
end;

procedure TprVariablesEditorForm.EditVariables(_Report : TprCustomReport; _Variable : TprVariable);
var
  i : integer;
  it : TListItem;
begin
Report := _Report;
Caption := Format(prLoadStr(sVariablesEditorFormCaption),[Report.Name]);
LV.Items.BeginUpdate;
it := nil;
for i:=0 to Report.Variables.Count-1 do
  begin
    UpdateLVItem(Report.Variables[i],LV.Items.Add);
    if Report.Variables[i]=_Variable then
      it := LV.Items[i];
  end;
LV.Items.EndUpdate;
if LV.Items.Count>0 then
  begin
    if it=nil then
      LV.ItemFocused := LV.Items[0]
    else
      LV.ItemFocused := it;
    LV.ItemFocused.Selected := true;
  end;
LVSelectItem(nil,nil,false);
ShowModal;
end;

procedure TprVariablesEditorForm.LVSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  i,cIndex : integer;
  cValue : string;
begin
//
Label1.Enabled := LV.SelCount=1;
EDName.Enabled := LV.SelCount=1;
Label2.Enabled := LV.SelCount>0;
EDType.Enabled := LV.SelCount>0;
Label3.Enabled := LV.SelCount>0;
EDValue.Enabled := LV.SelCount>0;

if LV.SelCount=0 then
  begin
    EDName.Text := '';
    EDType.ItemIndex := -1;
    EDValue.Text := '';
  end
else
  begin
    if LV.SelCount=1 then
      EDName.Text := TprVariable(LV.Selected.Data).Name
    else
      EDName.Text := '';
    i := 0;
    cIndex := -1;
    cValue := '';
    while i<LV.Items.Count do
      begin
        if LV.Items[i].Selected then
          begin
            cIndex := GetVariableTypeIndex(TprVariable(LV.Items[i].Data));
            cValue := TprVariable(LV.Items[i].Data).AsString;
            break;
          end;
        Inc(i);
      end;
    for i:=i+1 to LV.Items.Count-1 do
      if LV.Items[i].Selected then
        begin
          if (GetVariableTypeIndex(TprVariable(LV.Items[i].Data))<>cIndex) then
            cIndex := -1;
          if TprVariable(LV.Items[i].Data).AsString<>cValue then
            cValue := '';
        end;
    EDType.ItemIndex := cIndex;
    EDValue.Text := cValue;
  end;
EDName.Tag := 0;
EDValue.Tag := 0;
end;

function TprVariablesEditorForm.CheckName(Variable : TprVariable; const NewName : string) : boolean;
var
  i : integer;
begin
i := 0;
while (i<Report.Variables.Count) and
      ((Report.Variables[i]=Variable) or
       (AnsiCompareText(Report.Variables[i].Name,NewName)<>0)) do Inc(i);
Result := i>=Report.Variables.Count;
end;

procedure TprVariablesEditorForm.CopyFromNameAndValue;
var
  f : boolean;
  v : TprVariable;
  vExtended : extended;
  i,vInt,eCode : integer;
begin
f := false;
for i:=0 to LV.Items.Count-1 do
  if LV.Items[i].Selected then
    begin
      v := TprVariable(LV.Items[i].Data);
      if EDName.Tag=1 then
        begin
          if not CheckName(v,EDName.Name) then
            begin
              MBError(prLoadStr(sVariablesEditorFormDuplicateVariableName));
              exit;
            end;
          v.Name := EDName.Text;
          f := true;
        end;
      if EDValue.Tag=1 then
        begin
          // detect entered value type
          if EDValue.Text='' then
            v.IsNull := true
          else
            if EDValue.Text[1]='=' then
              v.Formula := Copy(EDValue.Text,2,Length(EDValue.Text))
            else
              begin
                val(EDValue.Text,vInt,eCode);
                if eCode=0 then
                  v.AsInteger := vInt
                else
                  if TextToFloat(PChar(EDValue.Text),vExtended,fvExtended) then
                    v.AsDouble := vExtended
                  else
                    begin
                      try
                        v.AsDateTime := StrToDateTime(EDValue.Text);
                      except
                        v.AsString := EDValue.Text;
                      end;
                    end;
              end;
          f := true;    
        end;
      UpdateLVItem(v,LV.Items[i]);
    end;
LVSelectItem(nil,nil,false);
if f then
  Report.DsgnTemplateChanged(nil,true);
end;

procedure TprVariablesEditorForm.EDNameChange(Sender: TObject);
begin
TComponent(Sender).Tag := 1;
end;

procedure TprVariablesEditorForm.EDNameExit(Sender: TObject);
begin
CopyFromNameAndValue;
end;

procedure TprVariablesEditorForm.EDNameKeyPress(Sender: TObject;
  var Key: Char);
begin
if Key=#13 then
  begin
    CopyFromNameAndValue;
    ActiveControl := LV;
    Key := #0;
  end;
end;

procedure TprVariablesEditorForm.EDTypeClick(Sender: TObject);
var
  i : integer;
  v : TprVariable;
begin
for i:=0 to LV.Items.Count-1 do
  if LV.Items[i].Selected then
    begin
      v := TprVariable(LV.Items[i].Data);
      try
        case EDType.ItemIndex of
          0: v.Formula := v.AsString;
          1: v.AsInteger := v.AsVariant;
          2: v.AsDateTime := v.AsVariant;
          3: v.AsString := v.AsVariant;
          4: v.AsDouble := v.AsVariant;
          5: v.IsNull := true;
        end;
        UpdateLVItem(v,LV.Items[i]);
      except
      end;
    end;
LVSelectItem(nil,nil,false);
Report.DsgnTemplateChanged(nil,true);
end;

procedure TprVariablesEditorForm.aNewVersionExecute(Sender: TObject);
var
  i : integer;
  v : TprVariable;
begin
v := Report.Variables.AddVariable;
v.Name := GetVariableName;
v.AsString := '';

LV.Items.BeginUpdate;
try
  for i:=0 to LV.Items.Count-1 do
    LV.Items[i].Selected := false;
  LV.ItemFocused := LV.Items.Add;
  UpdateLVItem(v,LV.ItemFocused);
  Report.DsgnTemplateChanged(nil,true);
  LV.ItemFocused.Selected := true;
  LV.ItemFocused.EditCaption;
finally
  LV.Items.EndUpdate;
end;
end;

procedure TprVariablesEditorForm.LVEdited(Sender: TObject; Item: TListItem;
  var S: String);
begin
s := Trim(s);
if s='' then
  s := TprVariable(Item.Data).Name
else
  if not CheckName(TprVariable(Item.Data),s) then
    begin
      s := TprVariable(Item.Data).Name;
      MBError(prLoadStr(sVariablesEditorFormDuplicateVariableName));
    end
  else
    begin
      TprVariable(Item.Data).Name := s;
      LVSelectItem(nil,nil,false);
      Report.DsgnTemplateChanged(nil,true);
    end;
end;

procedure TprVariablesEditorForm.LVKeyPress(Sender: TObject;
  var Key: Char);
begin
if (Key=#13) and (LV.SelCount>0) then
  begin
    ActiveControl := EDValue;
    Key := #0;
  end;
end;

procedure TprVariablesEditorForm.aEditVersionUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := LV.SelCount>0;
end;

procedure TprVariablesEditorForm.aEditVersionExecute(Sender: TObject);
begin
ActiveControl := EDValue;
end;

procedure TprVariablesEditorForm.aDelVersionUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := LV.SelCount>0;
end;

procedure TprVariablesEditorForm.aDelVersionExecute(Sender: TObject);
var
  i : integer;
begin
LV.Items.BeginUpdate;
try
  i := 0;
  while i<LV.Items.Count do
    begin
      if LV.Items[i].Selected then
        begin
          TprVariable(LV.Items[i].Data).Free;
          LV.Items.Delete(i)
        end
      else
        Inc(i);
    end;
  if LV.ItemFocused<>nil then
    LV.ItemFocused.Selected := true;
  Report.DsgnTemplateChanged(nil,true);
finally
  LV.Items.EndUpdate;
end;
end;

procedure TprVariablesEditorForm.aPasteUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := ClipBoard.HasFormat(CF_VARIABLES);
end;

procedure TprVariablesEditorForm.aPasteExecute(Sender: TObject);
var
  p : integer;
  v : TprVariable;
  it : TListItem;
  hMem : THandle;
  pMem : pointer;
  Buf,s : string;
  vType : TprVarValueType;
begin
ClipBoard.Open;
try
  hMem := Clipboard.GetAsHandle(CF_VARIABLES);
  pMem := GlobalLock(hMem);
  if pMem<>nil then
    begin
      p := GlobalSize(hMem);
      SetLength(Buf,p);
      MoveMemory(@(Buf[1]),pMem,p);
      LV.Items.BeginUpdate;
      try
        for p:=0 to LV.Items.Count-1 do
          LV.Items[p].Selected := false;
        p := 1;
        it := nil;
        while p<=Length(Buf)  do
          begin
            v := Report.Variables.AddVariable;
            v.Name := ExtractSubStr(Buf,p,[#0]);
            if Report.Variables.IndexByName(v.Name)<>-1 then
              v.Name := GetVariableName;
            s := ExtractSubStr(Buf,p,[#0]);
            if s='formula' then
              v.Formula := ExtractSubStr(Buf,p,[#0])
            else
              begin
                vType := TprVarValueType(GetEnumValue(TypeInfo(TprVarValueType),s));
                s := ExtractSubStr(Buf,p,[#0]);
                case vType of
                  prvvtString: v.AsString := s;
                  prvvtInteger: v.AsInteger := StrToInt(s);
                  prvvtDouble: v.AsDouble := StrToFloat(s);
                  prvvtDateTime: v.AsDateTime := StrToDateTime(s);
                  prvvtNull: v.IsNull := true;
                end;
              end;

            it := LV.Items.Add;
            UpdateLVItem(v,it);
            it.Selected := true;
          end;
        if it<>nil then
          LV.ItemFocused := it;
        Report.DsgnTemplateChanged(nil,true);
      finally
        GlobalUnlock(hMem);
        LV.Items.EndUpdate;
      end;
    end;
finally
  ClipBoard.Close;
end;
end;

procedure TprVariablesEditorForm.aCopyUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := LV.SelCount>0;
end;

procedure TprVariablesEditorForm.aCopyExecute(Sender: TObject);
var
  i : integer;
  Buf : string;
  hMem : THandle;
  pMem : pointer;
begin
Buf := '';
for i:=0 to LV.Items.Count-1 do
  if LV.Items[i].Selected then
    with TprVariable(LV.Items[i].Data) do
      begin
        Buf := Buf+Name+#0;
        if Calculated then
          Buf := Buf+'formula'#0
        else
          Buf := Buf+GetEnumName(TypeInfo(TprVarValueType),integer(VarValue.vType))+#0;
        Buf := Buf+AsString+#0;
      end;
i := Length(Buf);

ClipBoard.Open;
try
  hMem := GlobalAlloc(GMEM_MOVEABLE+GMEM_SHARE+GMEM_ZEROINIT,i);
  if hMem<>0 then
    begin
      pMem := GlobalLock(hMem);
      if pMem<>nil then
        begin
          CopyMemory(pMem,@(Buf[1]),i);
          GlobalUnLock(hMem);
          ClipBoard.SetAsHandle(CF_VARIABLES,hMem);
        end;
    end;
finally
  ClipBoard.Close;
end;
end;

procedure TprVariablesEditorForm.aCutUpdate(Sender: TObject);
begin
TAction(Sender).Enabled := LV.SelCount>0;
end;

procedure TprVariablesEditorForm.aCutExecute(Sender: TObject);
begin
aCopy.Execute;
aDelVersion.Execute;
end;

procedure TprVariablesEditorForm.Panel1Resize(Sender: TObject);
begin
EDValue.Width := Panel1.ClientWidth-EDValue.Left-10;
EDType.Left := Panel1.ClientWidth-EDType.Width-10;
Label2.Left := EDType.Left-4-Label2.Width;
EDName.Width := Label2.Left-EDName.Left-10;
end;

end.

