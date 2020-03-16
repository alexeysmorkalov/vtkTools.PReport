{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TemplateFindForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Grids, math, inifiles, typinfo,

  pr_Utils, pr_Common, pr_CommonDesignerPanel, pr_MultiLang, pr_DesignerFunctions;

{$I pr.inc}

type
  TprTemplateFindForm = class(TprCustomTemplateFindForm)
    PC: TPageControl;
    PFindParameters: TTabSheet;
    PFoundList: TTabSheet;
    Label1: TLabel;
    EDFindText: TComboBox;
    EDReplaceText: TComboBox;
    bFind: TButton;
    bReplaceAll: TButton;
    CBWholeWordsOnly: TCheckBox;
    CBCaseSensitive: TCheckBox;
    Label2: TLabel;
    prMLRes1: TprMLRes;
    Grid: TDrawGrid;
    CBPromptOnReplace: TCheckBox;
    procedure EDFindTextChange(Sender: TObject);
    procedure bFindClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure GridClick(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
    procedure PFindParametersResize(Sender: TObject);
    procedure bReplaceAllClick(Sender: TObject);
  private
    { Private declarations }
    FoundList : TprTemplateFoundList;
    procedure Find(FoundList : TprTemplateFoundList; const FindText : string; WholeWords,CaseSensitive : boolean);
    procedure UpdateGrid;
    function GetObjectDesc(fo : TprTemplateFoundObject) : string;
    procedure GetObjectImage(fo : TprTemplateFoundObject; Bitmap : TBitmap);
  protected
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    procedure UpdateInfo; override;
  end;

implementation

uses
  pr_Strings, pr_ValueEditor, pr_GroupEditor, pr_VariablesEditor;

var
  aCaptions : array [2..4] of string =
              ('Object',
               'Property',
               'Text');

{$R *.DFM}

procedure TprTemplateFindForm.prRestoreProperties;
var
  i,n : integer;
begin
inherited;
for i:=0 to Grid.ColCount-1 do
  Grid.ColWidths[i] := Ini.ReadInteger(sn,'Col'+IntToStr(i),Grid.ColWidths[i]);
CBWholeWordsOnly.Checked := Ini.ReadBool(sn,'WholeWordsOnly',CBWholeWordsOnly.Checked);
CBCaseSensitive.Checked := Ini.ReadBool(sn,'CaseSensitive',CBCaseSensitive.Checked);
CBPromptOnReplace.Checked := Ini.ReadBool(sn,'PromptOnReplace',CBPromptOnReplace.Checked);
n := Ini.ReadInteger(sn,'FindTextItemsCount',0);
for i:=1 to n do
  EDFindText.Items.Add(Ini.ReadString(sn,'FindTextItem'+IntToStr(i),''));
n := Ini.ReadInteger(sn,'ReplaceTextItemsCount',0);
for i:=1 to n do
  EDReplaceText.Items.Add(Ini.ReadString(sn,'ReplaceTextItem'+IntToStr(i),''));
end;

procedure TprTemplateFindForm.prSaveProperties;
var
  i : integer;
begin
inherited;
for i:=0 to Grid.ColCount-1 do
  Ini.WriteInteger(sn,'Col'+IntToStr(i),Grid.ColWidths[i]);
Ini.WriteBool(sn,'WholeWordsOnly',CBWholeWordsOnly.Checked);
Ini.WriteBool(sn,'CaseSensitive',CBCaseSensitive.Checked);
Ini.WriteBool(sn,'PromptOnReplace',CBPromptOnReplace.Checked);
Ini.WriteInteger(sn,'FindTextItemsCount',EDFindText.Items.Count);
for i:=0 to EDFindText.Items.Count-1 do
  Ini.WriteString(sn,'FindTextItem'+IntToStr(i+1),EDFindText.Items[i]);
Ini.WriteInteger(sn,'ReplaceTextItemsCount',EDReplaceText.Items.Count);
for i:=0 to EDReplaceText.Items.Count-1 do
  Ini.WriteString(sn,'ReplaceTextItem'+IntToStr(i+1),EDReplaceText.Items[i]);
end;

procedure TprTemplateFindForm.EDFindTextChange(Sender: TObject);
begin
bFind.Enabled := EDFindText.Text<>'';
bReplaceAll.Enabled := EDFindText.Text<>'';
end;

procedure TprTemplateFindForm.bFindClick(Sender: TObject);
begin
if (EDFindText.Text<>'') and (EDFindText.Items.IndexOf(EDFindText.Text)=-1) then
  EDFindText.Items.Add(EDFindText.Text);
Grid.Row := 1;
Find(FoundList,EDFindText.Text,CBWholeWordsOnly.Checked,CBCaseSensitive.Checked);
UpdateGrid;
if FoundList.Count<=0 then
  MBError(prLoadStr(sNotFoundInTemplate))
else
  PC.ActivePage := PFoundList;
end;

procedure TprTemplateFindForm.bReplaceAllClick(Sender: TObject);
var
  i : integer;

  procedure MakeReplace(fo : TprTemplateFoundObject);
  begin
  if fo.SubObjectRef is TprObjRecVersion then
    prReplaceTextInProp(TPersistent(fo.SubObjectRef),
                        fo.PropName,
                        EDFindText.Text,
                        EDReplaceText.Text,
                        CBWholeWordsOnly.Checked,
                        CBCaseSensitive.Checked)
  else
    prReplaceTextInProp(TPersistent(fo.ObjectRef),
                        fo.PropName,
                        EDFindText.Text,
                        EDReplaceText.Text,
                        CBWholeWordsOnly.Checked,
                        CBCaseSensitive.Checked)
  end;

begin
if (EDFindText.Text<>'') and (EDFindText.Items.IndexOf(EDFindText.Text)=-1) then
  EDFindText.Items.Add(EDFindText.Text);
if (EDReplaceText.Text<>'') and (EDReplaceText.Items.IndexOf(EDReplaceText.Text)=-1) then
  EDReplaceText.Items.Add(EDReplaceText.Text);
Grid.Row := 1;
Find(FoundList,EDFindText.Text,CBWholeWordsOnly.Checked,CBCaseSensitive.Checked);
UpdateGrid;
if FoundList.Count<=0 then
  MBError(prLoadStr(sNotFoundInTemplate))
else
  begin
    PC.ActivePage := PFoundList;
    for i:=0 to FoundList.Count-1 do
      begin
        if CBPromptOnReplace.Checked then
          begin
            Grid.Row := i+1;
            case MBox(Format(prLoadStr(sTemplateFindReplaceQuestion),[EDFindText.Text,EDReplaceText.Text,GetObjectDesc(FoundList[i]),FoundList[i].PropName]),
                      prLoadStr(sAttention),
                      MB_YESNOCANCEL or MB_ICONQUESTION) of
              IDYES: MakeReplace(FoundList[i]);
              IDCANCEL: exit;
            end
          end
        else
          MakeReplace(FoundList[i]);
      end;
    DesignerPanel.UpdateCurPage;
    DesignerPanel.DsgnNotifyReport(true);
  end;
end;

procedure TprTemplateFindForm.FormCreate(Sender: TObject);
begin
FoundList := TprTemplateFoundList.Create;
EDFindTextChange(nil);
UpdateGrid;
end;

procedure TprTemplateFindForm.FormDestroy(Sender: TObject);
begin
FoundList.Free;
end;

procedure TprTemplateFindForm.GridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  s : string;
  a : cardinal;
  bmp : TBitmap;
begin
InflateRect(Rect,-1,-1);
if ARow=0 then
  begin
    case ACol of
      0 : s := '¹';
      1 : s := '';
      else s := aCaptions[ACol];
    end;
    a := DT_CENTER+DT_VCENTER+DT_SINGLELINE+DT_END_ELLIPSIS;
  end
else
  begin
    a := DT_LEFT+DT_VCENTER+DT_SINGLELINE+DT_END_ELLIPSIS;
    if ARow<=FoundList.Count then
      begin
        case ACol of
          0: begin
               s := IntToStr(ARow);
               a := DT_RIGHT+DT_VCENTER+DT_SINGLELINE+DT_END_ELLIPSIS;
               InflateRect(Rect,-1,-1);
             end;
          1: begin
               bmp := TBitmap.Create;
               try
                 GetObjectImage(FoundList[ARow-1],bmp);
                 Grid.Canvas.Draw(Rect.Left+(Rect.Right-Rect.Left-bmp.Width) div 2,Rect.Top+(Rect.Bottom-Rect.Top-bmp.Height) div 2,bmp);
               finally
                 bmp.Free;
               end;
             end;
          2: s := GetObjectDesc(FoundList[ARow-1]);
          3: s := FoundList[ARow-1].PropName;
          4: s := FoundList[ARow-1].PropValue;
        end;
      end;
  end;
DrawTextEx(Grid.Canvas.Handle,PChar(s),Length(s),Rect,a,nil);
end;

procedure TprTemplateFindForm.UpdateGrid;
begin
Grid.RowCount := Max(2,FoundList.Count+1);
Grid.Repaint;
end;

procedure TprTemplateFindForm.UpdateInfo;
var
  fo : TprTemplateFoundObject;
  i,j : integer;
begin
// check all objects in list
i := 0;
while i<FoundList.Count do
  begin
    fo := FoundList[i];
    j := 0;
    while (j<DesignerPanel.GetReport.prOwner.ComponentCount) and
          (DesignerPanel.GetReport.prOwner.Components[j]<>fo.ObjectRef) do Inc(j);
    if j>=DesignerPanel.GetReport.prOwner.ComponentCount then
      begin
        j := 0;
        while (j<DesignerPanel.GetReport.Values.Count) and
              (DesignerPanel.GetReport.Values[j]<>fo.ObjectRef) do Inc(j);
        if j>=DesignerPanel.GetReport.Values.Count then
          begin
            j := 0;
            while (j<DesignerPanel.GetReport.Variables.Count) and
                  (DesignerPanel.GetReport.Variables[j]<>fo.ObjectRef) do Inc(j);
            if j>=DesignerPanel.GetReport.Variables.Count then
              j := -1;
          end;
      end;
    if j=-1 then
      FoundList.DeleteObject(i)
    else
      Inc(i);
  end;
UpdateGrid;
end;

function TprTemplateFindForm.GetObjectDesc(fo : TprTemplateFoundObject) : string;
begin
if fo.ObjectRef is TprCustomReport then
  Result := prLoadStr(sTemplateFindReportDesc)
else
  if fo.ObjectRef is TprGroup then
    Result := Format(prLoadStr(sTemplateFindGroupDesc),[TprGroup(fo.ObjectRef).Name])
  else
    if fo.ObjectRef is TprValue then
      Result := Format(prLoadStr(sTemplateFindValueDesc),[TprValue(fo.ObjectRef).Name])
    else
      if fo.ObjectRef is TprVariable then
        Result := Format(prLoadStr(sTemplateFindVariableDesc),[TprValue(fo.ObjectRef).Name])
      else
        if fo.ObjectRef is TprCustomPage then
          Result := Format(prLoadStr(sTemplateFindPageDesc),[TprCustomPage(fo.ObjectRef).IndexInReport+1,TprCustomPage(fo.ObjectRef).Name])
        else
          if fo.ObjectRef is TprBand then
            Result := Format(prLoadStr(sTemplateFindBandDesc),[TprBand(fo.ObjectRef).GetDrawDesignCaption])
          else
            if fo.ObjectRef is TprObj then
              begin
                if fo.SubObjectRef is TprObjRecVersion then
                  Result := Format(prLoadStr(sTemplateFindObjectVersionDesc),[TprObjRecVersion(fo.SubObjectRef).Index+1,TprObj(fo.ObjectRef).GetDesc])
                else
                  Result := Format(prLoadStr(sTemplateFindObjectDesc),[TprObj(fo.ObjectRef).GetDesc])
              end
            else
              Result := fo.ObjectRef.ClassName
end;

procedure TprTemplateFindForm.GetObjectImage(fo : TprTemplateFoundObject; Bitmap : TBitmap);
begin
if fo.ObjectRef is TprObj then
  LoadResImage(Bitmap,fo.ObjectRef.ClassName)
else
  if fo.ObjectRef is TprBand then
    LoadResImage(Bitmap,GetEnumName(TypeInfo(TprBandType),integer(TprBand(fo.ObjectRef).BandType)))
  else
    if fo.ObjectRef is TprGroup then
      LoadResImage(Bitmap,'GROUPS')
    else
      if fo.ObjectRef is TprValue then
        LoadResImage(Bitmap,'FUNC')
      else
        if fo.ObjectRef is TprVariable then
          LoadResImage(Bitmap,'VARIABLES')
        else
          exit;
Bitmap.Transparent := true;
Bitmap.TransparentMode := tmAuto;
end;

procedure TprTemplateFindForm.Find(FoundList : TprTemplateFoundList; const FindText : string; WholeWords,CaseSensitive : boolean);
var
  Report : TprCustomReport;
  i,j,k,m : integer;
begin
FoundList.Clear;
Report := DesignerPanel.GetReport;
prFindTextInProps(Report,
                  Report,
                  FindText,
                  WholeWords,
                  CaseSensitive,
                  FoundList);
for i:=0 to Report.Groups.Count-1 do
  prFindTextInProps(Report.Groups[i],
                    Report.Groups[i],
                    FindText,
                    WholeWords,
                    CaseSensitive,
                    FoundList);
for i:=0 to Report.Values.Count-1 do
  prFindTextInProps(Report.Values[i],
                    Report.Values[i],
                    FindText,
                    WholeWords,
                    CaseSensitive,
                    FoundList);
for i:=0 to Report.Variables.Count-1 do
  prFindTextInProps(Report.Variables[i],
                    Report.Variables[i],
                    FindText,
                    WholeWords,
                    CaseSensitive,
                    FoundList);
for i:=0 to Report.PagesCount-1 do
  begin
    prFindTextInProps(Report.Pages[i],
                      Report.Pages[i],
                      FindText,
                      WholeWords,
                      CaseSensitive,
                      FoundList);
    for j:=0 to Report.Pages[i].Bands.Count-1 do
      begin
        prFindTextInProps(Report.Pages[i].Bands[j],
                          Report.Pages[i].Bands[j],
                          FindText,
                          WholeWords,
                          CaseSensitive,
                          FoundList);
        for k:=0 to Report.Pages[i].Bands[j].Objects.Count-1 do
          begin
            prFindTextInProps(Report.Pages[i].Bands[j].Objects[k],
                              Report.Pages[i].Bands[j].Objects[k],
                              FindText,
                              WholeWords,
                              CaseSensitive,
                              FoundList);
            for m:=0 to Report.Pages[i].Bands[j].Objects[k].dRec.Versions.Count-1 do
              prFindTextInProps(Report.Pages[i].Bands[j].Objects[k].dRec.Versions[m],
                                Report.Pages[i].Bands[j].Objects[k],
                                FindText,
                                WholeWords,
                                CaseSensitive,
                                FoundList);
          end;
      end;
  end;
end;

procedure TprTemplateFindForm.GridClick(Sender: TObject);
var
  fo : TprTemplateFoundObject;
begin
if Grid.Row>FoundList.Count then exit;
fo := FoundList[Grid.Row-1];
if (fo.ObjectRef is TprObj) or (fo.ObjectRef is TprBand) then
  begin
    DesignerPanel.MakeObjectVisible(TprDesignComponent(fo.ObjectRef));
    DesignerPanel.SelectObject(TprDesignComponent(fo.ObjectRef));
  end
else
  if fo.ObjectRef is TprCustomPage then
    DesignerPanel.ActivePageIndex := TprCustomPage(fo.ObjectRef).IndexInReport;
end;

procedure TprTemplateFindForm.GridDblClick(Sender: TObject);
var
  fo : TprTemplateFoundObject;
begin
if Grid.Row>FoundList.Count then exit;
fo := FoundList[Grid.Row-1];
if (fo.ObjectRef is TprObj) or (fo.ObjectRef is TprBand) then
  DesignerPanel.VisibleObjectsPropsForm := true
else
  if fo.ObjectRef is TprCustomPage then
    DesignerPanel.EditPage(DesignerPanel.ActivePageIndex)
  else
    if fo.ObjectRef is TprGroup then
      TprGroupEditorForm.Create(Application).EditGroup(TprGroup(fo.ObjectRef))
    else
      if fo.ObjectRef is TprValue then
        TprValueEditorForm.Create(Application).EditValue(TprValue(fo.ObjectRef))
      else
        if fo.ObjectRef is TprVariable then
          TprVariablesEditorForm.Create(Application).EditVariables(DesignerPanel.GetReport,TprVariable(fo.ObjectRef));
end;

procedure TprTemplateFindForm.PFindParametersResize(Sender: TObject);
begin
bFind.Left := PFindParameters.ClientWidth-bFind.Width-10;
bReplaceAll.Left := PFindParameters.ClientWidth-bReplaceAll.Width-10;
EDFindText.Width := bFind.Left-EDFindText.Left-10;
EDReplaceText.Width := bFind.Left-EDReplaceText.Left-10;
CBWholeWordsOnly.Width := bFind.Left-CBWholeWordsOnly.Left-10;
CBCaseSensitive.Width := bFind.Left-CBCaseSensitive.Left-10;
CBPromptOnReplace.Width := bFind.Left-CBPromptOnReplace.Left-10;
end;

initialization

aCaptions[2] := prLoadStr(sTemplateFindFormCaptionsObject);
aCaptions[3] := prLoadStr(sTemplateFindFormCaptionsProperty);
aCaptions[4] := prLoadStr(sTemplateFindFormCaptionsText);

end.
