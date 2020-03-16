{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_Strings;

interface

const
  sPRResBase = 40000;


  sBandResizeNoChange                      = -1;
  sBandResizeMaxBottom                     = -2;
  sBandResizeMaxBottomInLinksList          = -3;
  sBandResizeMinBottomInLinksList          = -4;

  sFormatExpressionCategoryReportVar       = -5;
  sFormatExpressionCategoryObjectPtoperty  = -6;

  sImageEditorDrawModeCenter               = -7;
  sImageEditorDrawModeStretch              = -8;
  sImageEditorDrawModeStretchProp          = -9;
  sImageEditorDrawModeAsImageSize          = -10;

  sLinkLeftDependFromMaxRight              = -11;
  sLinkLeftDependFromMinRight              = -12;
  sLinkTopDependFromMaxBottom              = -13;
  sLinkTopDependFromMinBottom              = -14;

  sLinkWidthDependFromMaxRight             = -15;
  sLinkWidthDependFromMinRight             = -16;
  sLinkHeightDependFromMaxBottom           = -17;
  sLinkHeightDependFromMinBottom           = -18;

  sTextAlignTop                            = 33;
  sTextAlignVertCenter                     = 34;
  sTextAlignBottom                         = 35;
  sTextAlignJustify                        = -541;

  sTextAlignLeft                           = 30;
  sTextAlignHorzCenter                     = 31;
  sTextAlignRight                          = 32;

  sDisplayFormatStandart                   = -19;
  sDisplayFormatNumber                     = -20;
  sDisplayFormatDate                       = -21;
  sDisplayFormatTime                       = -22;
  sDisplayFormatDateTime                   = -23;
  sDisplayFormatText                       = -24;

  NullPointerString                        = -25;

  sPageNumber                              = -26;
  sBandAlreadyExistsInPageName             = -27;
  sBandAlreadyExistsInPageType             = -28;
  sErrorBandLoadUnknownObject              = -29;
  sUnknownBandType                         = -30;
  sUnknownBandName                         = -31;
  sBandHeightToBig                         = -32;
  sBandWidthToBig                          = -33;
  sDataSetNotFound                         = -34;
  sNoReportName                            = -35;
  sPreviewCaption                          = -36;
  sTextNotFound                            = -37;
  sAttention                               = -38;
  sEndOfFind                               = -39;
  sStartOfFind                             = -40;
  sErrorClipboardPaste                     = -41;
  sUnknownTypeObject                       = -42;
  sDeletePageQuestion                      = -43;
  sLinkNotSelected                         = -44;
  sVarAlreadyExists                        = -45;
  sGroupAlreadyExists                      = -46;

  sReportVarNotCalced                      = -47;
  sVarNotExists                            = -48;
  sGroupNotExists                          = -49;

  sPageMarginsWarning                      = -50;

  sNoObjSelected                           = -52;

  sDefaultProgressFormCaption              = -53;
  sActionCanceled                          = -54;

  sFirstPassCaption                        = -55;
  sSecondPassCaption                       = -56;
  sThirdPassCaption                        = -140;

  sReportEmptyInPreview                    = -58;

  sNoVarVersion                            = -59;

  sDeleteGroupQuestion                     = -60;
  sDeleteVarQuestion                       = -61;

  sValuesEditorCaption                     = -62;
  sGroupsEditorCaption                     = -63;

  sInvalidVarFormula                       = -64;
  sCalcOnDataSetNotDefined                 = -65;

  sNoGroupName                             = -66;
  sNoGroupValid                            = -67;
  sNoGroupDetailBand                       = -68;

  sCalcOnCrossDataSetNotDefined            = -69;
  sCalcOnError                             = -70;

  sResetOnGroupNotDefined                  = -71;
  sResetOnDataSetNotDefined                = -72;

  saRecNotInitializated                    = -73;

  sPrintReportCaption                      = -74;
  sCreateReportCaption                     = -75;
  sPrintReport                             = -76;

  sSetupPrintError1                        = -77;
  sSetupPrintError2                        = -78;
  sSetupPrintError3                        = -79;
  sSetupPrintError4                        = -80;
  sSetupPrintError5                        = -81;
  sSetupPrintError6                        = -82;

  prSaveReportTemplateFilter               = -83;

  sPropsFormManyObjectsSelectedCaption     = -84;
  sFormulaNotDefined                       = -85;

  sErrorPropName                           = -86;

  sTotalPages                              = -87;

  sErrorTxPreviewPageRange                 = -88;
  sErrorTxOpenPrinter                      = -89;
  sErrorTxStartDoc                         = -90;
  sErrorTxStartPAge                        = -91;
  sErrorTxESCModelNotFound                 = -92;

  sErrorNoModalForMDIChild                 = -93;

  sFind                                    = -94;

  sErrorSelectPrinter                      = -95;
  sErrorSelectPrinter2                     = -96;

  sDefaultPrinterName                      = -97;
  sInvalidPrinterName                      = -98;
  sPrinterErrorNotInitInfo                 = -99;
  sPrinterErrorUnaibleInitializeInfo       = -100;
  sPrinterErrorInvalidPaperSize            = -101;
  sPrinterErrorUnaibleInitializeStructures = -102;
  sPrinterErrorUnaibleCreateCanvas         = -103;
  sPrinterErrorUnknown                     = -104;
  sPrinterErrorInvalidPaperSizeIndex       = -432;
  sPrinterErrorUnsupportedPaperSize        = -433;
  sPrinterErrorOpenPrinter                 = -434;

  sStandartDisplayFormatSimpleCurrency     = -105;
  sStandartDisplayFormatShortCurrency      = -106;
  sStandartDisplayFormatCurrency           = -107;
  sStandartDisplayFormatPercent            = -108;
  sStandartDisplayFormatShortDate          = -109;
  sStandartDisplayFormatCurrencyWithDelim  = -110;
  sStandartDisplayFormatBankCurrency       = -111;

  sFormatExpressionFields                  = -112;
  sFormatExpressionFunctions               = -113;
  sFormatExpressionProperties              = -114;

  sGroupsEditorCaptionsName                = -115;
  sGroupsEditorCaptionsValid               = -116;
  sGroupsEditorCaptionsDetailBand          = -117;

  sValuesEditorCaptionsVarName             = -118;
  sValuesEditorCaptionsFormula             = -119;
  sValuesEditorCaptionsCalcMethod          = -120;
  sValuesEditorCaptionsCalcDataset         = -121;
  sValuesEditorCaptionsCrossTabDataset     = -122;
  sValuesEditorCaptionsSphere              = -123;
  sValuesEditorCaptionsSphereGroup         = -124;
  sValuesEditorCaptionsDataSetSphere       = -125;

  sPropertiesEditTemplate                  = -126;
  sPropertiesEditVars                      = -127;
  sPropertiesEditGroups                    = -128;

  sVisible	                           = -129;

  sGetComponentsReportDesc                 = -130;

  sVersionsEditorNewVersion                = -131;
  sVersionsEditorDelVersion                = -132;
  sVersionsEditorMoveUp                    = -133;
  sVersionsEditorMoveDown                  = -134;
  sVersionsEditorSetAsDefault              = -135;
  sVersionsEditorEditFormula               = -136;
  sErrorDesignerClassNotFound              = -137;
  sErrorPreviewClassNotFound               = -138;

  sErrorInvalidDatasetForprDataset         = -141;
  sErrorCalcExpressionInOnePass            = -142;
  sErrorInvalidStringsSource               = -143;
  sErrorStringsSourceNotDefined            = -144;

  sParserErrorUnknownFieldType             = -145;
  sParserErrorErrorValueType               = -146;
  sParserErrorCalcExpression               = -147;
  sParserErrorInvalidFormatString          = -148;
  sParserErrorInFormatString               = -149;
  sParserErrorParamsCountForLocate         = -150;
  sParserErrorGetReferenceToClass          = -151;

  sParserErrorUnknownVariable              = -152;
  sParserErrorGetVarValue                  = -153;
  sParserErrorString                       = -154;
  sParserErrorUnknownOperandType           = -155;
  sParserErrorUnknownOperator              = -156;
  sParserErrorUnknownFunction              = -157;
  sParserErrorCommonError                  = -158;
  sParserErrorInvalidBrackets              = -159;
  sParserErrorCompile                      = -160;
  sParserErrorInvalidFuncParametersCount   = -161;
  sParserErrorStack                        = -162;
  sParserErrorIncompatibleTypes            = -163;
  sParserErrorInvalidTypes                 = -164;
  sParserErrorUnknownProp                  = -165;
  sParserErrorObjectNotFound               = -166;
  sParserErrorUnknownObjectFunction        = -167;
  sParserErrorUnknownReportVariableVersion = -169;
  sParserErrorParameterWithoutFunction     = -483;
  sParserErrorUnknownOperand               = -484;
  sParserErrorInvalidSymbol                = -485;

  sTemplaceSaveQuestion                    = -170;
  sDesignerCaption                         = -171;

  sOnePassCalcError                        = -172;

  sImageObjDescFileMask                    = -173;
  sImageObjDescDBFieldMask                 = -174;

  sOtherPageSize                           = -175;

  sValuesEditorCaptionsAggFunction         = -176;
  sReportEmptyInExport                     = -177;

  sExportErrorInitOLEServer                = -178;

  sPreviewFileMask                         = -179;

  sExportReportCaption                     = -181;
  sExportReportProgress                    = -182;

  sSelectFormatAddFormat                   = -183;
  sSelectFormatDelFormat                   = -184;

  sExportPrecisionLow                      = -185;
  sExportPrecisionNormal                   = -186;
  sExportPrecisionHigh                     = -187;

  sExportFileNotDefined                    = -188;
  sExportPagesRangeNotValid                = -189;
  sExportPagesListNotValid                 = -190;

  sExportNotUseESCModels                   = -192;
  sExportLinesRangeNotValid                = -193;

  sExportTXTErrorCreateFile                = -194;

  sExportReportProgressLines               = -195;
  sExportTXTErrorWriteFile                 = -196;

  sTxPreviewPages                          = -197;

  sReportObjectName                        = -198;

  sError                                   = -199;

  sErrorGetPreviewUserDataClass            = -400;
  sNoObjectsSelected                       = -401;
  sSelectedMoreThenOneObject               = -402;
  sColorBtnOtherColorCaption               = -403;
  sColorBtnNoColorCaption                  = -404;
  sPreviewPropsFormOneObjectSelectedCaption= -405;
  sPreviewPropsFormSizesPageCaption        = 277;
  sPreviewPropsFormLeftSizeEditCaption     = 271;
  sPreviewPropsFormTopSizeEditCaption      = 272;
  sPreviewPropsFormRightSizeEditCaption    = 273;
  sPreviewPropsFormBottomSizeEditCaption   = 274;
  sPreviewPropsFormWidthSizeEditCaption    = 275;
  sPreviewPropsFormHeightSizeEditCaption   = 276;
  sPreviewPropsFormUnitsCaption            = 270;
  sPreviewSelectPageForPaste               = -406;
  sPreviewDeletePageQuestion               = -407;
  sPreviewModeEdit                         = -408;
  sPreviewModePreview                      = -409;
  sCurrencyFormat                          = -410;
  sShortCurrencyFormat                     = -411;
  sSimpleCurrencyFormat                    = -412;
  sPercentFormat                           = -413;
  sSpacedCurrencyFormat                    = -414;
  sBankCurrencyFormat                      = -415;

  sWidthAsVerticalBand                     = -416;
  sHeightAsHorizontalBand                  = -417;
  sObjOptions                              = -418;
  sExportFormExportFormat                  = -419;
  sXLSExportFilterDesc                     = -420;
  sHTMExportFilterDesc                     = -421;
  sTXTExportFilterDesc                     = -422;
  sExportFilterNotDefined                  = -423;

  sBandsCaptionsDefineLink                 = -424;
  sLinksControlDefineLeftLink              = -425;
  sLinksControlDefineTopLink               = -426;
  sLinksControlDefineWidthLink             = -427;
  sLinksControlDefineHeightLink            = -428;
  sVerticalAlignment                       = -429;
  sHorizontalAlignment                     = -430;
  sPrinterUnavailable                      = -431;
  sNotSupportedPaperSize                   = -435;
  sPopupMainMenu = -436;
  sCancelInsertObject = -437;

  sTemplateFindFormCaptionsObject = -438;
  sTemplateFindFormCaptionsProperty = -439;
  sTemplateFindFormCaptionsText = -440;

  sNotFoundInTemplate = -441;
  sFindInTemplate = -442;

  sTemplateFindReportDesc = -443;
  sTemplateFindGroupDesc = -444;
  sTemplateFindValueDesc = -445;
  sTemplateFindPageDesc = -446;
  sTemplateFindBandDesc = -447;
  sTemplateFindObjectDesc = -448;
  sTemplateFindObjectVersionDesc = -449;
  sTemplateFindReplaceQuestion = -470;
  sTemplateFindVariableDesc = -471;
  sSimpleVarNotExists = -472;
  sLinksControlDeleteLink = 17;
  sTxMemoObjFontStyle = -473;
  sTxMemoObjFontOptions = -474;

  sVariablesEditorFormName = 467;
  sVariablesEditorFormType = 468;
  sVariablesEditorFormValue = 469;
  sVariablesEditorFormValueFormula = -475;
  sVariablesEditorFormValueString = -476;
  sVariablesEditorFormValueInteger = -477;
  sVariablesEditorFormValueDouble = -478;
  sVariablesEditorFormValueDateTime = -479;
  sVariablesEditorFormValueNull = -480;
  sVariablesEditorFormCaption = -481;
  sVariablesEditorFormDuplicateVariableName = -482;

  sPreviewFirstPage = -486;
  sPreviewPriorPage = -487;
  sPreviewNextPage = -488;
  sPreviewLastPage = -489;

  sHLineObjDesc = -490;
  sVLineObjDesc = -491;

  sMainCharTabSheet = -492;
  sStartingCharTabSheet = -493;
  sEndingCharTabSheet = -494;
  sHEXCodeLabel = -495;
  sAllCharsAsMainChar = -496;

  sSaveCombination = -497;
  sLoadCombination = -498;
  sCurrentCombination = -499;
  sCombinationsList = -510;
  sLineTemplates = -511;

  sShapeStyleMenuCaption = -512; // version 1.9
  sShapeStylesFirstIndex = -513; // version 1.9

  sShapeCenterHorzLine = -513; // version 1.9
  sShapeBottomLine = -514; // version 1.9
  sShapeTopLine = -515; // version 1.9
  sShapeCenterVertLine = -516; // version 1.9
  sShapeLeftLine = -517; // version 1.9
  sShapeRightLine = -518; // version 1.9
  sShapeBox = -519; // version 1.9
  sShapeEllipse = -520; // version 1.9

  sShapeObjCaption = -521; // version 1.9
  sShapeObjHint = -522; // version 1.9

  sShapeObjEditorMain = -523; // version 1.9
  sShapeObjEditorStyle = -524; // version 1.9
  sShapeObjEditorPen = -525; // version 1.9
  sShapeObjEditorBrush = -526; // version 1.9
  sShapeObjEditorColor = -527; // version 1.9
  sShapeObjEditorPenWidth = -528; // version 1.9
  sShapeObjSetSizeAsPenWidth = -529; // version 1.9
  sLinkWidthDependFromMaxWidth = -530; // version 1.9
  sLinkWidthDependFromMinWidth = -531; // version 1.9
  sLinkWidthDependFromMaxHeight = -532; // version 1.9
  sLinkWidthDependFromMinHeight = -533; // version 1.9
  sLinkWidthDependFromSumWidth = -534; // version 1.9.2
  sLinkWidthDependFromSumHeight = -535; // version 1.9.2
  sBandPrintOnLastPage = -536; // version 1.9.2
  sBandCanSplit = -537; // version 1.9.5
  sObjCanSplit = -538; // version 1.9.5
  sSubReport = -539; // version 1.9.7
  sMinDataRecords = -540; // version 1.9.7

  sJustifyLastLine = -542; // version 1.9.7
  sEolIsEndOfParagraph = -543; // version 1.9.7


  sClipboardCopy = 68;
  sClipboardPaste = 69;
  sClipboardCut = 70;
  sDeleteSelected = 16;
  sObjectLinks = 76;
  sBandLinks = 77;
  sProperties = 80;
  sSizesAndPosition = 278;
  sInplaceEdit = 81;
  sHorizontalAlignLeft = 30;
  sHorizontalAlignCenter = 31;
  sHorizontalAlignRight = 32;
  sVerticalAlignTop = 33;
  sVerticalAlignCenter = 34;
  sVerticalAlignBottom = 35;
  sAddititional = 158;
  sDeleteEmptyLines = 159;
  sDeleteEmptyLinesAtEnd = 160;
  sResizeHorizontally = 161;
  sResizeVertically = 162;
  sWordWrap = 163;
  sSize = 18;

  sAddPage = 71;
  sDeletePage = 72;
  sChangePageParams = 66;
  sPrint = 65;
  sExport = 283;
  sLoadPreparedReport = 199;
  sSavePreparedReport = 198;
  sWholePage = 185;
  sPageWidth = 189;
  sTwoPages = 191;
  sManyPages = 187;
  sFindStart = 193;
  sFindNext = 194;
  sFindPrior = 195;
  sCancelFind = 196;

  sPrintOnFirstPage = 4;
  sPrintOnLastPage = 474;
  sPrintWithChildDetail = 475;
  sUseVerticalBands = 5;
  sUseHorizontalBands = 6;
  sPrintAfterLastBandOnPage = 281;
  sReprintOnEachPage = 266;
  sPrintWithDetail = 267;
  sStartNewPage = 268;

  sPrinter = 201;
  sOptions = 327;

  sTxMemoObjDefaultFont = 301;

  sInsertObject = 257;
  sInsertBand = 21;
  sMainMenuFile = 248;
  sMainMenuEdit = 249;
  sMainMenuObject = 250;
  sMainMenuView = 252;
  sMainMenuPage = 251;

  sMainMenuFileCreate = 48;
  sMainMenuFileOpen = 50;
  sMainMenuFileSave = 52;
  sMainMenuFileSaveAs = 54;
  sMainMenuFileReportParams = 67;
  sMainMenuFilePageParams = 66;
  sMainMenuFilePreview = 64;
  sMainMenuFilePrint = 65;

  sMainMenuPageAddPage = 71;
  sMainMenuPageDelPage = 72;
  sMainMenuPageNextPage = 73;
  sMainMenuPagePriorPage = 74;
  sMainMenuPagePageParams = 66;

  sMainMenuEditSendToBack = 78;
  sMainMenuEditBringToFront = 79;
  sMainMenuEditAlignToGrid = 104;
  sMainMenuEditAlignAndResizeToGrid = 105;
  sMainMenuEditGroups = 57;
  sMainMenuEditVars = 59;
  sMainMenuEditVariables = 470;

  sMainMenuViewShowGrid = 62;
  sMainMenuViewUseGrid = 63;
  sMainMenuViewGridSize = 75;
  sMainMenuViewOptions = 327;

  sBandTitlesOffset                        = -200;
  sbthTitle                                = sBandTitlesOffset-0;
  sbthSummary                              = sBandTitlesOffset-1;
  sbthPageHeader                           = sBandTitlesOffset-2;
  sbthPageFooter                           = sBandTitlesOffset-3;
  sbthDetail                               = sBandTitlesOffset-4;
  sbthDetailHeader                         = sBandTitlesOffset-5;
  sbthDetailFooter                         = sBandTitlesOffset-6;
  sbthGroupHeader                          = sBandTitlesOffset-7;
  sbthGroupFooter                          = sBandTitlesOffset-8;
  sbtvTitle                                = sBandTitlesOffset-9;
  sbtvSummary                              = sBandTitlesOffset-10;
  sbtvPageHeader                           = sBandTitlesOffset-11;
  sbtvPageFooter                           = sBandTitlesOffset-12;
  sbtvDetail                               = sBandTitlesOffset-13;
  sbtvDetailHeader                         = sBandTitlesOffset-14;
  sbtvDetailFooter                         = sBandTitlesOffset-15;
  sbtvGroupHeader                          = sBandTitlesOffset-16;
  sbtvGroupFooter                          = sBandTitlesOffset-17;

  sUnitsDescsOffset                        = -250;
  sUnitDescPixel                           = sUnitsDescsOffset-0;
  sUnitDescMM                              = sUnitsDescsOffset-1;
  sUnitDescSM                              = sUnitsDescsOffset-2;
  sUnitDescInch                            = sUnitsDescsOffset-3;
  sUnitDescM                               = sUnitsDescsOffset-4;

  sMonthsNamesOffset                       = -260;

  sObjectsOffset                           = -300;
  sMemoObjCaption                          = sObjectsOffset-0;
  sMemoObjHint                             = sObjectsOffset-1;
  sImageObjCaption                         = sObjectsOffset-2;
  sImageObjHint                            = sObjectsOffset-3;

  sTxMemoObjCaption                        = sObjectsOffset-4;
  sTxMemoObjHint                           = sObjectsOffset-5;

  sRichObjCaption                          = sObjectsOffset-6;
  sRichObjHint                             = sObjectsOffset-7;

  sTxCommandObjCaption                     = sObjectsOffset-8; // version 1.7
  sTxCommandObjHint                        = sObjectsOffset-9; // version 1.7

  sTxHLineObjCaption                       = sObjectsOffset - 10; // version 1.9
  sTxHLineObjHint                          = sObjectsOffset - 11; // version 1.9

  sTxVLineObjCaption                       = sObjectsOffset - 12; // version 1.9
  sTxVLineObjHint                          = sObjectsOffset - 13; // version 1.9

  sTxCodePageTitlesOffset                  = -350;
  sTxCodePageTitleDOS866                   = sTxCodePageTitlesOffset-0;
  sTxCodePageTitleWIN1251                  = sTxCodePageTitlesOffset-1;

  sStuckModeCaptionsOffset                 = -450;  // version 1.8
  sStuckModeCaptionsOffsetDisabled         = sStuckModeCaptionsOffset-0; // version 1.8
  sStuckModeCaptionsOffsetAlways           = sStuckModeCaptionsOffset-1; // version 1.8
  sStuckModeCaptionsOffsetCtrlButton       = sStuckModeCaptionsOffset-2; // version 1.8

  sPageScaleModeCaptionsOffset = -500; // version 1.8
  sPageScaleModeCaptionsToNearest = sPageScaleModeCaptionsOffset-0; // version 1.8
  sPageScaleModeCaptionsToDefined = sPageScaleModeCaptionsOffset-1; // version 1.8
  sPageScaleModeCaptionsUserSelected = sPageScaleModeCaptionsOffset-2; // version 1.8

  sBarCodeObjCaption = -544; // version 1.9.7
  sBarCodeObjHint = -545; // version 1.9.7

  sBarCodeEditorText = -546; // version 1.9.7
  sBarCodeEditorBarcodeType = -547; // version 1.9.7
  sBarCodeEditorShowTextOverLines = -548; // version 1.9.7
  sBarCodeEditorCaptionType = -550; // version 1.9.7
  sBarCodeEditorPosition = -551; // version 1.9.7
  sBarCodeEditorTextFont = -552; // version 1.9.7
  sBarCodeEditorBackground = -553; // version 1.9.7
  sBarCodeEditorForeground = -554; // version 1.9.7
  sBarCodeEditorRotation = -568; // version 1.9.7

  sBarCodeEditorShowTextTypeNone = -555; // version 1.9.7
  sBarCodeEditorShowTextTypeCode = -556; // version 1.9.7
  sBarCodeEditorShowTextTypeType = -557; // version 1.9.7
  sBarCodeEditorShowTextTypeBoth = -558; // version 1.9.7

  sBarCodeEditorTextPositionTopLeft = -559; // version 1.9.7
  sBarCodeEditorTextPositionTopRight = -560; // version 1.9.7
  sBarCodeEditorTextPositionTopCenter = -561; // version 1.9.7
  sBarCodeEditorTextPositionBottomLeft = -562; // version 1.9.7
  sBarCodeEditorTextPositionBottomRight = -563; // version 1.9.7
  sBarCodeEditorTextPositionBottomCenter = -564; // version 1.9.7

  sBarCodeEditorSheetMain = -565; // version 1.9.7
  sBarCodeEditorSheetText = -566; // version 1.9.7
  sBarCodeEditorSheetView = -567; // version 1.9.7
  sBarCodeRotationNone = -569; // version 1.9.7

  sBarCodeAutoSize = -570; // version 1.9.8
  
  sTxMemoCustomBorderScheme = -571; // version 1.9.8
  sTxMemoPredefinedBorderSchemas = -572; // version 1.9.8
  sTxMemoNoBorderSchemas = -573; // version 1.9.8
  sRecodeTable = -574; // version 1.9.8

implementation

end.
