{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_Reg;

{$i pr.inc}

interface

uses
  {$ifdef PR_D6_D7} DesignIntf, DesignEditors, {$else} dsgnintf, {$endif}

  Classes, SysUtils, pr_Common, pr_Classes, pr_TxClasses, pr_Properties,
  pr_VersionsEditor, pr_MultiLang, pr_Dataset, 
  pr_DesignerPanel, pr_TxDesignerPanel, pr_PreviewPanel,
  pr_TxPreviewPanel, pr_TxUtils, pr_ArrayDataset;

procedure Register;

implementation

{$R *.RES}

procedure Register;
begin
  RegisterComponents('vtkTools PReport', [TprReport]);
  RegisterComponents('vtkTools PReport', [TprTxReport]);
  RegisterComponents('vtkTools PReport', [TprMLRes]);
  RegisterComponents('vtkTools PReport', [TprStringsDataset]);
  RegisterComponents('vtkTools PReport', [TprEventsDataset]);
  RegisterComponents('vtkTools PReport', [TprArrayDataset]);
  RegisterComponents('vtkTools PReport', [TprPreviewPanel]);
  RegisterComponents('vtkTools PReport', [TprDesignerPanel]);
  RegisterComponents('vtkTools PReport', [TprTxPreviewPanel]);
  RegisterComponents('vtkTools PReport', [TprTxDesignerPanel]);

  RegisterNoIcon([TprObjVersionsEditor]);
     
  RegisterNoIcon([TprHTitleBand]);
  RegisterNoIcon([TprHSummaryBand]);
  RegisterNoIcon([TprHPageHeaderBand]);
  RegisterNoIcon([TprHPageFooterBand]);
  RegisterNoIcon([TprHDetailBand]);
  RegisterNoIcon([TprHDetailHeaderBand]);
  RegisterNoIcon([TprHDetailFooterBand]);
  RegisterNoIcon([TprHGroupHeaderBand]);
  RegisterNoIcon([TprHGroupFooterBand]);
  RegisterNoIcon([TprVTitleBand]);
  RegisterNoIcon([TprVSummaryBand]);
  RegisterNoIcon([TprVPageHeaderBand]);
  RegisterNoIcon([TprVPageFooterBand]);
  RegisterNoIcon([TprVDetailBand]);
  RegisterNoIcon([TprVDetailHeaderBand]);
  RegisterNoIcon([TprVDetailFooterBand]);
  RegisterNoIcon([TprVGroupHeaderBand]);
  RegisterNoIcon([TprVGroupFooterBand]);

  RegisterNoIcon([TprTxHTitleBand]);
  RegisterNoIcon([TprTxHSummaryBand]);
  RegisterNoIcon([TprTxHPageHeaderBand]);
  RegisterNoIcon([TprTxHPageFooterBand]);
  RegisterNoIcon([TprTxHDetailBand]);
  RegisterNoIcon([TprTxHDetailHeaderBand]);
  RegisterNoIcon([TprTxHDetailFooterBand]);
  RegisterNoIcon([TprTxHGroupHeaderBand]);
  RegisterNoIcon([TprTxHGroupFooterBand]);
  RegisterNoIcon([TprTxVTitleBand]);
  RegisterNoIcon([TprTxVSummaryBand]);
  RegisterNoIcon([TprTxVPageHeaderBand]);
  RegisterNoIcon([TprTxVPageFooterBand]);
  RegisterNoIcon([TprTxVDetailBand]);
  RegisterNoIcon([TprTxVDetailHeaderBand]);
  RegisterNoIcon([TprTxVDetailFooterBand]);
  RegisterNoIcon([TprTxVGroupHeaderBand]);
  RegisterNoIcon([TprTxVGroupFooterBand]);

  RegisterNoIcon([TprPage]);
  RegisterNoIcon([TprMemoObj]);
  RegisterNoIcon([TprImageObj]);
  RegisterNoIcon([TprGroup]);

  RegisterNoIcon([TprTxPage]);
  RegisterNoIcon([TprTxMemoObj]);

  RegisterPropertyEditor(TypeInfo(string),TprCustomReport,'ExportFilter',TprExportFilterProperty);
  RegisterComponentEditor(TprReport,TprCustomReportEditor);
  RegisterPropertyEditor(TypeInfo(string),TprReport,'PrinterName',TprPrinterNameProperty);
  RegisterPropertyEditor(TypeInfo(TprValues),TprReport,'Values',TprValuesProperty);
  RegisterPropertyEditor(TypeInfo(TprVariables),TprReport,'Variables',TprVariablesProperty);

  RegisterComponentEditor(TprTxReport,TprCustomReportEditor);
  RegisterPropertyEditor(TypeInfo(string),TprTxReport,'PrinterName',TprTxPrinterNameProperty);
  RegisterPropertyEditor(TypeInfo(TprValues),TprTxReport,'Values',TprValuesProperty);
  RegisterPropertyEditor(TypeInfo(TprVariables),TprTxReport,'Variables',TprVariablesProperty);

  RegisterPropertyEditor(TypeInfo(string),TprTxReport,'ESCModelName',TprESCModelProperty);
  RegisterPropertyEditor(TypeInfo(string),TprTxReport,'ExportESCModelName',TprESCModelProperty);
  RegisterPropertyEditor(TypeInfo(string),TprTxReport,'RecodeTableName',TprTxRecodeTableProperty);
  RegisterPropertyEditor(TypeInfo(TprFixedFont),TprTxDesignerPanel,'Font',TprFixedFontProperty);
  RegisterPropertyEditor(TypeInfo(TprFixedFont),TprTxPreviewPanel,'Font',TprFixedFontProperty);

  RegisterComponentEditor(TprMLRes,TprMLResEditor);
  RegisterPropertyEditor(TypeInfo(TCollection),TprMLRes,'ResLinks',TprResLinksProperty);
end;

end.
