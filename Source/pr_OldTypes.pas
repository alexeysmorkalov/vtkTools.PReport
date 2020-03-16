{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2005 by vtkTools      }
{                                          }
{******************************************}

{Contains the old deprecated types.}
unit pr_OldTypes;

interface

type
{DEPRECIATED, replaced with TprReportRangeType and TprTxReportRangeType. Describes the set of pages to printing.
Items:
  ppmAll - All pages of the report.
  ppmPagesRange - The specified pages' range.
  ppmSelection - The current selection (not used).
  ppmPagesList - The specified pages' list in the form: "1, 3, 6-9"}
  TprPrintPagesMode = (ppmAll, ppmPagesRange, ppmSelection, ppmPagesList);

{DEPRECIATED, replaced with TprFormStyle. Decribes the work mode of built-in forms - preview and designer.
Items:
  fmNormal - The normal mode, the form is neither an MDI parent window nor an MDI child window.
  fmMDIChild - The form is an MDI child window.}
  TprFormMode = (fmNormal, fmMDIChild);

{DEPRECIATED replaced with TprCustomOptionsExport. Describes an options of export.
Items:
  preoShowParamsDlg - Indicates whether the built-in options dialog form must be shown before export.
  preoShowProgress - Indicates whether the progress window must be shown while exporting.
  preoShowAfterGenerate - Indicates whether the created document must be shown after export.
  preoShowWhileGenerate - is not used.}
  TprExportOptions = (preoShowParamsDlg, preoShowProgress, preoShowAfterGenerate, preoShowWhileGenerate);
{DEPRECIATED replaced with TprCustomOptionsExport. Describes a set of export options.
See also:
  TprExportOptions}
  TprExportOptionsSet = set of TprExportOptions;
  
implementation

end.
