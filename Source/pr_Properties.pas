{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_Properties;

{$i pr.inc}

interface

uses
  Classes, forms, dialogs,
  {$ifdef PR_D6_D7} DesignIntf, DesignEditors, {$else} dsgnintf, {$endif}
  
  pr_Common, pr_Classes, pr_Designer, pr_TxClasses,
  pr_MultiLang, pr_MultiLangForm, pr_TxConsts, pr_TxUtils;

type

/////////////////////////////////////////////////
//
// TprMLResEditor
//
/////////////////////////////////////////////////
TprMLResEditor = class(TComponentEditor)
public
  procedure Edit; override;
  procedure ExecuteVerb(Index: Integer); override;
  function GetVerb(Index: Integer): String; override;
  function GetVerbCount: Integer; override;
end;

/////////////////////////////////////////////////
//
// TprResLinksProperty
//
/////////////////////////////////////////////////
TprResLinksProperty = class(TPropertyEditor)
public
  function GetAttributes: TPropertyAttributes; override;
  function GetValue : string; override;
  procedure Edit; override;
end;

/////////////////////////////////////////////////
//
// TprCustomReportEditor
//
/////////////////////////////////////////////////
TprCustomReportEditor = class(TComponentEditor)
public
  procedure Edit; override;
  procedure ExecuteVerb(Index: Integer); override;
  function GetVerb(Index: Integer): String; override;
  function GetVerbCount: Integer; override;
end;

/////////////////////////////////////////////////
//
// TprValuesProperty
//
/////////////////////////////////////////////////
TprValuesProperty = class(TPropertyEditor)
public
  function GetAttributes: TPropertyAttributes; override;
  function GetValue : string; override;
  procedure Edit; override;
end;

/////////////////////////////////////////////////
//
// TprVariablesProperty
//
/////////////////////////////////////////////////
TprVariablesProperty = class(TPropertyEditor)
public
  function GetAttributes: TPropertyAttributes; override;
  function GetValue : string; override;
  procedure Edit; override;
end;

/////////////////////////////////////////////////
//
// TprPrinterNameProperty
//
/////////////////////////////////////////////////
TprPrinterNameProperty = class(TPropertyEditor)
public
  function  GetAttributes: TPropertyAttributes; override;
  procedure GetValues(Proc : TGetStrProc); override;
  function  GetValue : string; override;
  procedure SetValue(const Value : string); override;
end;

/////////////////////////////////////////////////
//
// TprTxPrinterNameProperty
//
/////////////////////////////////////////////////
TprTxPrinterNameProperty = class(TPropertyEditor)
public
  function  GetAttributes: TPropertyAttributes; override;
  procedure GetValues(Proc : TGetStrProc); override;
  function  GetValue : string; override;
  procedure SetValue(const Value : string); override;
end;

/////////////////////////////////////////////////
//
// TprESCModelProperty
//
/////////////////////////////////////////////////
TprESCModelProperty = class(TPropertyEditor)
public
  function GetAttributes: TPropertyAttributes; override;
  procedure GetValues(Proc : TGetStrProc); override;
  function GetValue : string; override;
  procedure SetValue(const Value : string); override;
end;

/////////////////////////////////////////////////
//
// TprTxRecodeTableProperty
//
/////////////////////////////////////////////////
TprTxRecodeTableProperty = class(TPropertyEditor)
public
  function GetAttributes: TPropertyAttributes; override;
  procedure GetValues(Proc : TGetStrProc); override;
  function GetValue : string; override;
  procedure SetValue(const Value : string); override;
end;

/////////////////////////////////////////////////
//
// TprExportFilterProperty
//
/////////////////////////////////////////////////
TprExportFilterProperty = class(TPropertyEditor)
public
  function GetAttributes: TPropertyAttributes; override;
  procedure GetValues(Proc : TGetStrProc); override;
  function GetValue : string; override;
  procedure SetValue(const Value : string); override;
end;

/////////////////////////////////////////////////
//
// TprFixedFontProperty
//
/////////////////////////////////////////////////
TprFixedFontProperty = class(TPropertyEditor)
public
  function GetAttributes: TPropertyAttributes; override;
  function  GetValue : string; override;
  procedure Edit; override;
end;

implementation

uses
  pr_ValuesEditor, pr_GroupsEditor, pr_Utils, pr_Strings, pr_VariablesEditor;

type
  TprCustomReportAccess = class(TprCustomReport)
  end;
  
/////////////////////////////////////////////////
//
// TprResLinksProperty
//
/////////////////////////////////////////////////
function TprResLinksProperty.GetAttributes;
begin
Result := [paDialog];
end;

function TprResLinksProperty.GetValue;
begin
Result := '(ResLinks)';
end;

procedure TprResLinksProperty.Edit;
begin
TprMultiLangForm.Create(nil).EditRes(GetComponent(0) as TprMLRes,(GetComponent(0) as TprMLRes).Owner as TForm);
end;

///////////////////////
//
// TprValuesProperty
//
///////////////////////
function TprValuesProperty.GetAttributes;
begin
Result:=[paDialog];
end;

function TprValuesProperty.GetValue;
begin
Result:='(TprValues)';
end;

procedure TprValuesProperty.Edit;
var
  r: TprCustomReportAccess;
begin
  r := TprCustomReportAccess(GetComponent(0));
  r.Designer := Designer;
  TprValuesEditorForm.Create(nil).EditValues(r);
end;

///////////////////////
//
// TprVariablesProperty
//
///////////////////////
function TprVariablesProperty.GetAttributes;
begin
Result:=[paDialog];
end;

function TprVariablesProperty.GetValue;
begin
Result:='(TprVariablesProperty)';
end;

procedure TprVariablesProperty.Edit;
var
  r : TprCustomReportAccess;
begin
  r := TprCustomReportAccess(GetComponent(0));
  r.Designer := Designer;
  TprVariablesEditorForm.Create(nil).EditVariables(r, nil);
end;

//////////////////////////////////////
//
// TprPrinterNameProperty
//
//////////////////////////////////////
function TprPrinterNameProperty.GetAttributes;
begin
Result:=[paValueList,paRevertable];
end;

procedure TprPrinterNameProperty.GetValues;
var
  i : integer;
  Report : TprReport;
begin
Report:=TprReport(GetComponent(0));
for i:=0 to Report.prPrinter.Printers.Count-1 do
  Proc(Report.prPrinter.Printers[i]);
end;

function TprPrinterNameProperty.GetValue;
begin
Result:=GetStrValue;
end;

procedure TprPrinterNameProperty.SetValue(const Value : string);
begin
SetStrValue(Value)
end;

//////////////////////////////////////
//
// TprTxPrinterNameProperty
//
//////////////////////////////////////
function TprTxPrinterNameProperty.GetAttributes;
begin
Result:=[paValueList,paRevertable];
end;

procedure TprTxPrinterNameProperty.GetValues;
var
  i : integer;
  L : TStringList;
begin
L := TStringList.Create;
try
  UpdatePrintersList(L,'');
  for i:=0 to L.Count-1 do
    Proc(L[i]);
finally
  L.Free;
end;
end;

function TprTxPrinterNameProperty.GetValue;
begin
Result:=GetStrValue;
end;

procedure TprTxPrinterNameProperty.SetValue(const Value : string);
begin
SetStrValue(Value)
end;

//////////////////////
//
// TprCustomReportEditor
//
//////////////////////
procedure TprCustomReportEditor.Edit;
begin
  TprCustomReportAccess(Component).Designer := Designer;
  TprCustomReportAccess(Component).DesignReport({$IFDEF PR_D6_D7}false{$ELSE}true{$ENDIF});
end;

procedure TprCustomReportEditor.ExecuteVerb(Index: Integer);
var
  r: TprCustomReportAccess;
begin
  r := TprCustomReportAccess(Component);
  r.Designer := Designer;
  case Index of
    0: Edit;
    1: TprValuesEditorForm.Create(nil).EditValues(r);
    2: TprGroupsEditorForm.Create(nil).EditGroups(r);
  end;
end;

function TprCustomReportEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

function TprCustomReportEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0  : Result := prLoadStr(sPropertiesEditTemplate);
    1  : Result := prLoadStr(sPropertiesEditVars);
    2  : Result := prLoadStr(sPropertiesEditGroups);
    else Result := '';
  end;
end;

/////////////////////////////////////////////////
//
// TprMLResEditor
//
/////////////////////////////////////////////////
procedure TprMLResEditor.Edit;
begin
TprMultiLangForm.Create(nil).EditRes(Component as TprMLRes,Component.Owner as TForm);
end;

procedure TprMLResEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TprMLResEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TprMLResEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0  : Result := 'Edit StringID table';
    else Result := '';
  end;
end;

procedure TemplateChangedGlobalProc(Report : TprCustomReport);
begin
if Assigned(TprCustomReportAccess(Report).Designer) then
  (TprCustomReportAccess(Report).Designer as {$ifdef PR_D6_D7} IDesigner {$else} IFormDesigner {$endif}).Modified;
end;

/////////////////////////////////////////////////
//
// TprESCModelProperty
//
/////////////////////////////////////////////////
function TprESCModelProperty.GetAttributes;
begin
Result := [paValueList];
end;

procedure TprESCModelProperty.GetValues;
var
  i : integer;
begin
for i:=0 to TxReportOptions.ESCModelsCount-1 do
  Proc(TxReportOptions.ESCModels[i].ModelName);
end;

function TprESCModelProperty.GetValue;
begin
Result := GetStrValue;
end;

procedure TprESCModelProperty.SetValue(const Value : string);
begin
SetStrValue(Value)
end;

/////////////////////////////////////////////////
//
// TprTxRecodeTableProperty
//
/////////////////////////////////////////////////
function TprTxRecodeTableProperty.GetAttributes;
begin
Result := [paValueList];
end;

procedure TprTxRecodeTableProperty.GetValues;
var
  i : integer;
begin
for i:=0 to TxReportOptions.TxRecodeTablesCount-1 do
  Proc(TxReportOptions.RecodeTables[i].RecodeTableName);
end;

function TprTxRecodeTableProperty.GetValue;
begin
Result := GetStrValue;
end;

procedure TprTxRecodeTableProperty.SetValue(const Value : string);
begin
SetStrValue(Value)
end;

/////////////////////////////////////////////////
//
// TprExportFilterProperty
//
/////////////////////////////////////////////////
function TprExportFilterProperty.GetAttributes;
begin
Result := [paValueList];
end;

procedure TprExportFilterProperty.GetValues;
var
  i : integer;
begin
for i:=0 to High(prExportFiltersInfos) do
  if GetComponent(0) is prExportFiltersInfos[i].ReportRef then
    Proc(prExportFiltersInfos[i].ExportFilterClassRef.ClassName);
end;

function TprExportFilterProperty.GetValue;
begin
Result := GetStrValue;
end;

procedure TprExportFilterProperty.SetValue(const Value : string);
begin
SetStrValue(Value)
end;

/////////////////////////////////////////////////
//
// TprFixedFontProperty
//
/////////////////////////////////////////////////
function TprFixedFontProperty.GetAttributes : TPropertyAttributes;
begin
Result := [paDialog];
end;

function TprFixedFontProperty.GetValue : string;
begin
Result := '(TprFixedFont)';
end;

procedure TprFixedFontProperty.Edit;
var
  fd : TFontDialog;
begin
fd := TFontDialog.Create(nil);
try
  fd.Options := [fdFixedPitchOnly,fdForceFontExist];
  with TObject(GetOrdValue) as TprFixedFont do
    begin
      fd.Font.Name := Name;
      fd.Font.Size := Size;
    end;
  if fd.Execute then
    with TObject(GetOrdValue) as TprFixedFont do
      begin
        Name := fd.Font.Name;
        Size := fd.Font.Size;
      end
finally
  fd.Free;
end;
end;

initialization

prTemplateChangedGlobalProc := TemplateChangedGlobalProc;

finalization

prTemplateChangedGlobalProc := nil;

end.

