//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("prPackageCB5.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("dsnide50.bpi");
USEUNIT("pr_XLSConts.pas");
USEUNIT("pr_Classes.pas");
USEUNIT("pr_Common.pas");
USEUNIT("pr_CommonDesigner.pas");
USEUNIT("pr_Dataset.pas");
USEFORMNS("pr_designer.pas", Pr_designer, prDesignerForm);
USEUNIT("pr_DesignerFunctions.pas");
USEFORMNS("pr_FormatExpression.pas", Pr_formatexpression, prFormatExpressionForm);
USEFORMNS("pr_GridSize.pas", Pr_gridsize, prGridSizeForm);
USEFORMNS("pr_GroupEditor.pas", Pr_groupeditor, prGroupEditorForm);
USEFORMNS("pr_GroupsEditor.pas", Pr_groupseditor, prGroupsEditorForm);
USEFORMNS("pr_ImageEditor.pas", Pr_imageeditor, prImageEditorForm);
USEFORMNS("pr_ImagePreview.pas", Pr_imagepreview, prImagePreviewForm);
USEFORMNS("pr_Link.pas", Pr_link, prObjLinksForm);
USEFORMNS("pr_MemoEditor.pas", Pr_memoeditor, prMemoEditorForm);
USEUNIT("pr_MultiLang.pas");
USEFORMNS("pr_MultiLangForm.pas", Pr_multilangform, prMultiLangForm);
USEFORMNS("pr_ObjectPosSizeForm.pas", Pr_objectpossizeform, prObjectPosSizeForm);
USEFORMNS("pr_ObjectsProps.pas", Pr_objectsprops, prObjectsPropsForm);
USEFORMNS("pr_PageParams.pas", Pr_pageparams, prPageParamsForm);
USEUNIT("pr_Parser.pas");
USEFORMNS("pr_Preview.pas", Pr_preview, prPreviewForm);
USEFORMNS("pr_Progress.pas", Pr_progress, prProgressForm);
USEUNIT("pr_Properties.pas");
USEUNIT("pr_Reg.pas");
USEFORMNS("pr_ReportParams.pas", Pr_reportparams, prReportParamsForm);
USEFORMNS("pr_RichEditor.pas", Pr_richeditor, prRichEditorForm);
USEFORMNS("pr_SelectField.pas", Pr_selectfield, prSelectFieldForm);
USEFORMNS("pr_SelectFormat.pas", Pr_selectformat, prSelectFormatForm);
USEUNIT("pr_Strings.pas");
USEUNIT("pr_TxClasses.pas");
USEUNIT("pr_TxConsts.pas");
USEFORMNS("pr_TxDesigner.pas", Pr_txdesigner, prTxDesignerForm);
USEFORMNS("pr_TxMemoEditor.pas", Pr_txmemoeditor, prTxMemoEditorForm);
USEFORMNS("pr_TxMemoLinesEditorForm.pas", Pr_txmemolineseditorform, prTxMemoLinesEditorForm);
USEFORMNS("pr_TxObjectPosSizeForm.pas", Pr_txobjectpossizeform, prTxObjectPosSizeForm);
USEFORMNS("pr_TxPageParams.pas", Pr_txpageparams, prTxPageParamsForm);
USEFORMNS("pr_TxPreview.pas", Pr_txpreview, prTxPreviewForm);
USEFORMNS("pr_TxReportParams.pas", Pr_txreportparams, prTxReportParamsForm);
USEUNIT("pr_TxUtils.pas");
USEUNIT("pr_Utils.pas");
USEFORMNS("pr_ValueEditor.pas", Pr_valueeditor, prValueEditorForm);
USEFORMNS("pr_ValuesEditor.pas", Pr_valueseditor, prValuesEditorForm);
USEUNIT("pr_VersionsEditor.pas");
USEFORMNS("pr_BandEditor.pas", Pr_bandeditor, prBandEditorForm);
USEPACKAGE("VCLX50.bpi");
USEPACKAGE("VCLDB50.bpi");
USEFORMNS("pr_ExportParams.pas", Pr_exportparams, prExportParamsForm);
USEFORMNS("pr_TxExportParams.pas", Pr_txexportparams, prTxExportParamsForm);
USEUNIT("pr_PrvRichEditor.pas");
USEUNIT("pr_PreviewObjectsProps.pas");
USEUNIT("pr_PrvImageEditor.pas");
USEUNIT("pr_PrvMemoEditor.pas");
USEUNIT("vteWriters.pas");
USEUNIT("vteConsts.pas");
USEUNIT("vteExcel.pas");
USEUNIT("vteExcelTypes.pas");
USEUNIT("BIFF8_Types.pas");
USEUNIT("pr_TxExportFilters.pas");
USEUNIT("pr_ExportFilters.pas");
USEFORMNS("pr_TxCommandEditor.pas", Pr_txcommandeditor, prTxCommandEditorForm);
USEFORMNS("pr_TxDesignerPanelOptions.pas", Pr_txdesignerpaneloptions, prTxDesignerPanelOptionsForm);
USEUNIT("pr_CommonDesignerPanel.pas");
USEUNIT("pr_CommonPreviewPanel.pas");
USEUNIT("pr_DesignerPanel.pas");
USEFORMNS("pr_DesignerPanelOptions.pas", Pr_designerpaneloptions, prDesignerPanelOptionsForm);
USEUNIT("pr_PreviewPanel.pas");
USEFORMNS("pr_PreviewPanelOptions.pas", Pr_previewpaneloptions, prPreviewPanelOptionsForm);
USEFORMNS("pr_SelectPrintPaperSize.pas", Pr_selectprintpapersize, prSelectPrintPaperSizeForm);
USEUNIT("pr_TxDesignerPanel.pas");
USEFORMNS("pr_BandHint.pas", Pr_bandhint, prBandHintForm);
USEUNIT("pr_TxPreviewPanel.pas");
USEFORMNS("pr_TxPreviewPanelOptions.pas", Pr_txpreviewpaneloptions, prTxPreviewPanelOptionsForm);
USEUNIT("vteExcelFormula.pas");
USEFORMNS("pr_TemplateFindForm.pas", Pr_templatefindform, prTemplateFindForm);
USEUNIT("vteExcelFormula_iftab.pas");
USEFORMNS("pr_VariablesEditor.pas", Pr_variableseditor, prVariablesEditorForm);
USEPACKAGE("vgr_CommonControlsBCB5.bpi");
USEFORMNS("pr_TxLineEditor.pas", Pr_txlineeditor, prTxLineEditorForm);
USEUNIT("pr_ArrayDataset.pas");
USEFORMNS("pr_PrvShapeEditor.pas", Pr_prvshapeeditor, prPrvShapeEditorForm);
USEFORMNS("pr_ShapeEditor.pas", Pr_shapeeditor, prShapeEditorForm);
USEUNIT("pr_ShapeObj.pas");
USEUNIT("pr_TxAddon.pas");
USEUNIT("pr_CommonClasses.pas");
USEPACKAGE("vcljpg50.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
