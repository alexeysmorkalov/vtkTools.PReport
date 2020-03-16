//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("pr_BandEditor.pas", Pr_bandeditor, prBandEditorForm);
USEFORMNS("pr_Designer.pas", Pr_designer, prDesignerForm);
USEFORMNS("pr_ExportParams.pas", Pr_exportparams, prExportParamsForm);
USEFORMNS("pr_FormatExpression.pas", Pr_formatexpression, prFormatExpressionForm);
USEFORMNS("pr_GridSize.pas", Pr_gridsize, prGridSizeForm);
USEFORMNS("pr_GroupEditor.pas", Pr_groupeditor, prGroupEditorForm);
USEFORMNS("pr_GroupsEditor.pas", Pr_groupseditor, prGroupsEditorForm);
USEFORMNS("pr_ImageEditor.pas", Pr_imageeditor, prImageEditorForm);
USEFORMNS("pr_ImagePreview.pas", Pr_imagepreview, prImagePreviewForm);
USEFORMNS("pr_Link.pas", Pr_link, prObjLinksForm);
USEFORMNS("pr_MemoEditor.pas", Pr_memoeditor, prMemoEditorForm);
USEFORMNS("pr_MultiLangForm.pas", Pr_multilangform, prMultiLangForm);
USEFORMNS("pr_ObjectPosSizeForm.pas", Pr_objectpossizeform, prObjectPosSizeForm);
USEFORMNS("pr_ObjectsProps.pas", Pr_objectsprops, prObjectsPropsForm);
USEFORMNS("pr_PageParams.pas", Pr_pageparams, prPageParamsForm);
USEFORMNS("pr_Preview.pas", Pr_preview, prPreviewForm);
USEFORMNS("pr_PreviewObjectsProps.pas", Pr_previewobjectsprops, prPreviewObjectsPropsForm);
USEFORMNS("pr_Progress.pas", Pr_progress, prProgressForm);
USEFORMNS("pr_PrvImageEditor.pas", Pr_prvimageeditor, prPrvImageEditorForm);
USEFORMNS("pr_PrvMemoEditor.pas", Pr_prvmemoeditor, prPrvMemoEditorForm);
USEFORMNS("pr_PrvRichEditor.pas", Pr_prvricheditor, prPrvRichEditorForm);
USEFORMNS("pr_ReportParams.pas", Pr_reportparams, prReportParamsForm);
USEFORMNS("pr_RichEditor.pas", Pr_richeditor, prRichEditorForm);
USEFORMNS("pr_SelectField.pas", Pr_selectfield, prSelectFieldForm);
USEFORMNS("pr_SelectFormat.pas", Pr_selectformat, prSelectFormatForm);
USEFORMNS("pr_TxCommandEditor.pas", Pr_txcommandeditor, prTxCommandEditorForm);
USEFORMNS("pr_Txdesigner.pas", Pr_txdesigner, prTxDesignerForm);
USEFORMNS("pr_TxExportParams.pas", Pr_txexportparams, prTxExportParamsForm);
USEFORMNS("pr_TxMemoEditor.pas", Pr_txmemoeditor, prTxMemoEditorForm);
USEFORMNS("pr_TxMemoLinesEditorForm.pas", Pr_txmemolineseditorform, prTxMemoLinesEditorForm);
USEFORMNS("pr_TxObjectPosSizeForm.pas", Pr_txobjectpossizeform, prTxObjectPosSizeForm);
USEFORMNS("pr_TxPageParams.pas", Pr_txpageparams, prTxPageParamsForm);
USEFORMNS("pr_TxPreview.pas", Pr_txpreview, prTxPreviewForm);
USEFORMNS("pr_TxReportParams.pas", Pr_txreportparams, prTxReportParamsForm);
USEFORMNS("pr_ValueEditor.pas", Pr_valueeditor, prValueEditorForm);
USEFORMNS("pr_ValuesEditor.pas", Pr_valueseditor, prValuesEditorForm);
USEFORMNS("pr_TxPreviewPanelOptions.pas", Pr_txpreviewpaneloptions, prTxPreviewPanelOptionsForm);
USEFORMNS("pr_DesignerPanelOptions.pas", Pr_designerpaneloptions, prDesignerPanelOptionsForm);
USEFORMNS("pr_PreviewPanelOptions.pas", Pr_previewpaneloptions, prPreviewPanelOptionsForm);
USEFORMNS("pr_SelectPrintPaperSize.pas", Pr_selectprintpapersize, prSelectPrintPaperSizeForm);
USEFORMNS("pr_TxDesignerPanelOptions.pas", Pr_txdesignerpaneloptions, prTxDesignerPanelOptionsForm);
USEFORMNS("pr_BandHint.pas", Pr_bandhint, prBandHintForm);
USEFORMNS("pr_TemplateFindForm.pas", Pr_templatefindform, prTemplateFindForm);
USEFORMNS("pr_VariablesEditor.pas", Pr_variableseditor, prVariablesEditorForm);
USEFORMNS("pr_PrvShapeEditor.pas", Pr_prvshapeeditor, prPrvShapeEditorForm);
USEFORMNS("pr_ShapeEditor.pas", Pr_shapeeditor, prShapeEditorForm);
USEFORMNS("pr_TxLineEditor.pas", Pr_txlineeditor, prTxLineEditorForm);
USEFORMNS("pr_PrvBarCodeEditor.pas", Pr_prvbarcodeeditor, prPrvBarCodeEditorForm);
USEFORMNS("pr_BarCodeEditor.pas", Pr_barcodeeditor, prBarCodeEditorForm);
USEFORMNS("pr_OemCharSelectForm.pas", Pr_oemcharselectform, prOemCharSelectForm);
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
