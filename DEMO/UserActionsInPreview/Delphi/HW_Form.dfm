�
 THWFORM 0.�  TPF0THWFormHWFormLeft� Top� BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionUser actions in previewClientHeight� ClientWidth*Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderOnCreate
FormCreatePixelsPerInch`
TextHeight TButtonButton1LeftTopWidthHeightCaptionTprReport - WIN DemoTabOrder OnClickButton1Click  TButtonButton2LeftTop8WidthHeightCaptionTprTxReport - DOS DemoTabOrderOnClickButton2Click  	TDatabaseDatabase	Connected	DatabaseNameDatabase
DriverNameSTANDARDParams.StringsPath=..\..\DBF SessionNameDefaultLeftTopX  TQueryQuery1Active	DatabaseNameDatabaseSQL.Strings)select * from orders where CustNo=:CustNo Left0TopX	ParamDataDataType	ftIntegerNameCustNo	ParamTypeptInputValueG    TTable	CustomersActive	DatabaseNameDatabase	TableNameCUSTOMER.DBLeftpTopX  	TprReport
rCustomersTitleCustomers listExportFromPage ExportToPage Values 	Variables PrinterNameVirtual printerOnPreviewGetUserDatarCustomersPreviewGetUserDataOnPreviewMouseMoverCustomersPreviewMouseMoveOnPreviewMouseDownrCustomersPreviewMouseDownPreviewParams.Options PreviewParams.ShowToolbarsprptPreviewCommon Left� TopX
SystemInfoOS: WIN32_NT 5.1.2600  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.3 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1Height(UseVerticalBands 
TprMemoObj
prMemoObj1dRec.DefVersion dRec.VersionsVisible	Memo.StringsSimple Customers list lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top
dRec.Right�dRec.Bottom#Visible   TprHDetailBandprHDetailBand1
ResizeModeprbrmMaxObjHeight(UseVerticalBandsDataSetName	CustomersColCountColDirectionprcdTopBottomLeftRight 
TprMemoObj
prMemoObj2dRec.DefVersion dRec.VersionsVisible	Memo.Strings&[Customers.CustNo] [Customers.Company] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftdRec.Top
dRec.RightrdRec.BottomVisible     	TprReportrOrdersTitleOrders listExportFromPage ExportToPage ValuesNameit_ItemsTotalAggFunctionprafSumFormulaQuery1.ItemsTotalResetOn	rvtReportCalcOncvtDataSetNextDataSetNameQuery1 Name
it_FreightAggFunctionprafSumFormulaQuery1.FreightResetOn	rvtReportCalcOncvtDataSetNextDataSetNameQuery1 Nameit_AmountPaidAggFunctionprafSumFormulaQuery1.AmountPaidResetOn	rvtReportCalcOncvtDataSetNextDataSetNameQuery1  	Variables PrinterNameVirtual printerOnPreviewGetUserDatarOrdersPreviewGetUserDataOnPreviewMouseMoverOrdersPreviewMouseMoveOnPreviewDblClickrOrdersPreviewDblClickPreviewParams.Options PreviewParams.ShowToolbarsprptPreviewCommon Left� TopX
SystemInfoOS: WIN32_NT 5.1.2600  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.3 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1HeightPUseVerticalBands 
TprMemoObj
prMemoObj1dRec.DefVersion dRec.VersionsVisible	Memo.StringsOrders list for customer: lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top
dRec.Right�dRec.BottomVisible  
TprMemoObj
prMemoObj2dRec.DefVersion dRec.VersionsVisible	Memo.StringsCustNo : [Customers.CustNo]Company : [Customers.Company] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left1dRec.Top"
dRec.Right�dRec.BottomGVisible   TprHPageHeaderBandprHPageHeaderBand1Height,UseVerticalBandsPrintOnFirstPage	 
TprMemoObj
prMemoObj3dRec.DefVersion dRec.VersionsVisible	Memo.StringsOrderNo lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top
dRec.Right6dRec.Bottom'Visible  
TprMemoObj
prMemoObj4dRec.DefVersion dRec.VersionsVisible	Memo.Strings	Sale date lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left=dRec.Top
dRec.Right� dRec.Bottom'Visible  
TprMemoObj
prMemoObj5dRec.DefVersion dRec.VersionsVisible	Memo.Strings	Ship date lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left� dRec.Top
dRec.Right� dRec.Bottom'Visible  
TprMemoObj
prMemoObj7dRec.DefVersion dRec.VersionsVisible	Memo.StringsPaymentmethod lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left� dRec.Top
dRec.RightFdRec.Bottom'Visible  
TprMemoObj
prMemoObj8dRec.DefVersion dRec.VersionsVisible	Memo.StringsItems total lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftIdRec.Top
dRec.Right�dRec.Bottom'Visible  
TprMemoObjprMemoObj10dRec.DefVersion dRec.VersionsVisible	Memo.Strings
AmountPaid lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left�dRec.Top
dRec.Right�dRec.Bottom'Visible  
TprMemoObjprMemoObj11dRec.DefVersion dRec.VersionsVisible	Memo.StringsFreight lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top
dRec.Right~dRec.Bottom'Visible  
TprMemoObjprMemoObj12dRec.DefVersion dRec.VersionsVisible	Memo.StringsTaxRate lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left�dRec.Top
dRec.RightdRec.Bottom'Visible   TprHDetailBandprHDetailBand1
ResizeModeprbrmMaxObjHeight UseVerticalBandsDataSetNameQuery1ColCount ColDirectionprcdTopBottomLeftRight 
TprMemoObj
prMemoObj9dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<$>Query1.AmountPaid] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left�dRec.Top
dRec.Right�dRec.BottomVisible  
TprMemoObjprMemoObj13dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<$>Query1.Freight] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top
dRec.Right~dRec.BottomVisible  
TprMemoObjprMemoObj14dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<p>Query1.TaxRate] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left�dRec.Top
dRec.RightdRec.BottomVisible  
TprMemoObjprMemoObj15dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<$>Query1.ItemsTotal] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftIdRec.Top
dRec.Right�dRec.BottomVisible  
TprMemoObjprMemoObj16dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query1.PaymentMethod] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left� dRec.Top
dRec.RightFdRec.BottomVisible  
TprMemoObjprMemoObj17dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<d>Query1.ShipDate] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left� dRec.Top
dRec.Right� dRec.BottomVisible  
TprMemoObjprMemoObj18dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<d>Query1.SaleDate] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left=dRec.Top
dRec.Right� dRec.BottomVisible  
TprMemoObjprMemoObj19dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query1.OrderNo] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top
dRec.Right6dRec.BottomVisible   TprHSummaryBandprHSummaryBand1Height(UseVerticalBands 
TprMemoObjprMemoObj20dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<$>it_Freight] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftdRec.Top

dRec.Right~dRec.BottomVisible  
TprMemoObjprMemoObj21dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<$>it_ItemsTotal] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftIdRec.Top

dRec.Right�dRec.BottomVisible  
TprMemoObjprMemoObj22dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<$>it_AmountPaid] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left�dRec.Top

dRec.Right�dRec.BottomVisible  
TprMemoObj
prMemoObj6dRec.DefVersion dRec.VersionsVisible	Memo.StringsTOTAL:   lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlignprhRightvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftdRec.Top

dRec.Right6dRec.BottomVisible     	TprReportrOrderCanUserEdit	TitleOrderExportFromPage ExportToPage Values 	Variables PrinterNameVirtual printerPreviewParams.OptionsprpoShowMenuprpoAllowShowHideToolbarsprpoAllowDragToolbars PreviewParams.ShowToolbarsprptPreviewCommonprptEditprptInsertObjectprptTextprptBorders	prptAlignprptSize	prptNudgeprptObjects Left TopX
SystemInfoOS: WIN32_NT 5.1.2600  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.3 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1Height� UseVerticalBands 
TprMemoObj
prMemoObj1dRec.DefVersion dRec.VersionsVisible	Memo.StringsOrder No : [Query1.OrderNo] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlignprhLeftvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTimes New Roman
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left	dRec.Top
dRec.Right�dRec.Bottom.Visible  
TprMemoObj
prMemoObj2dRec.DefVersion dRec.VersionsVisible	Memo.Strings	Customer: lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left	dRec.Top5
dRec.Right� dRec.BottomFVisible  
TprMemoObj
prMemoObj4dRec.DefVersion dRec.VersionsVisible	Memo.Strings
Sale date: lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left	dRec.TopI
dRec.Right� dRec.BottomZVisible  
TprMemoObj
prMemoObj5dRec.DefVersion dRec.VersionsVisible	Memo.Strings
Ship date: lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left�dRec.TopI
dRec.RightdRec.BottomZVisible  
TprMemoObj
prMemoObj6dRec.DefVersion dRec.VersionsVisible	Memo.Strings	Ship VIA: lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left	dRec.Top]
dRec.Right� dRec.BottomnVisible  
TprMemoObj
prMemoObj7dRec.DefVersion dRec.VersionsVisible	Memo.StringsTerms: lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left�dRec.Top]
dRec.RightdRec.BottomnVisible  
TprMemoObj
prMemoObj8dRec.DefVersion dRec.VersionsVisible	Memo.StringsPayment method: lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left	dRec.Topq
dRec.Right� dRec.Bottom� Visible  
TprMemoObj
prMemoObj3dRec.DefVersion dRec.VersionsVisible	Memo.Strings&[Customers.CustNo] [Customers.Company] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize	  	dRec.Left� dRec.Top5
dRec.Right�dRec.BottomFVisible  
TprMemoObjprMemoObj10dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<d>Query1.SaleDate] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize	  	dRec.Left� dRec.TopI
dRec.RightrdRec.BottomZVisible  
TprMemoObjprMemoObj11dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<d>Query1.ShipDate] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize	  	dRec.Left	dRec.TopI
dRec.Right�dRec.BottomZVisible  
TprMemoObjprMemoObj12dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query1.ShipVIA] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize	  	dRec.Left� dRec.Top]
dRec.RightrdRec.BottomnVisible  
TprMemoObjprMemoObj13dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query1.Terms] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize	  	dRec.Left	dRec.Top]
dRec.Right�dRec.BottomnVisible  
TprMemoObjprMemoObj14dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query1.PaymentMethod] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize	  	dRec.Left� dRec.Topq
dRec.RightrdRec.Bottom� Visible   TprHPageHeaderBandprHPageHeaderBand1HeightUseVerticalBandsPrintOnFirstPage	 
TprMemoObj
prMemoObj9dRec.DefVersion dRec.VersionsVisible	Memo.StringsItem No lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top
dRec.RightfdRec.BottomVisible  
TprMemoObjprMemoObj15dRec.DefVersion dRec.VersionsVisible	Memo.StringsPart No lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftidRec.Top
dRec.Right�dRec.BottomVisible  
TprMemoObjprMemoObj16dRec.DefVersion dRec.VersionsVisible	Memo.StringsQuantity lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left�dRec.Top
dRec.RightbdRec.BottomVisible  
TprMemoObjprMemoObj17dRec.DefVersion dRec.VersionsVisible	Memo.StringsDiscount lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftedRec.Top
dRec.Right�dRec.BottomVisible   TprHDetailBandprHDetailBand1
ResizeModeprbrmMaxObjHeightUseVerticalBandsDataSetNameQuery2ColCount ColDirectionprcdTopBottomLeftRight 
TprMemoObjprMemoObj19dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query2.ItemNo] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top
dRec.RightfdRec.BottomVisible  
TprMemoObjprMemoObj20dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query2.PartNo] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftidRec.Top
dRec.Right�dRec.BottomVisible  
TprMemoObjprMemoObj21dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query2.Qty] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left�dRec.Top
dRec.RightbdRec.BottomVisible  
TprMemoObjprMemoObj22dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query2.Discount] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftedRec.Top
dRec.Right�dRec.BottomVisible   TprHSummaryBandprHSummaryBand1Height|UseVerticalBands 
TprMemoObjprMemoObj23dRec.DefVersion dRec.VersionsVisible	Memo.StringsItems total lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left	dRec.Top	
dRec.RightndRec.Bottom"Visible  
TprMemoObjprMemoObj24dRec.DefVersion dRec.VersionsVisible	Memo.StringsTaxRate lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left	dRec.Top%
dRec.RightndRec.Bottom>Visible  
TprMemoObjprMemoObj25dRec.DefVersion dRec.VersionsVisible	Memo.StringsFreight lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left	dRec.TopA
dRec.RightndRec.BottomZVisible  
TprMemoObjprMemoObj26dRec.DefVersion dRec.VersionsVisible	Memo.Strings
AmountPaid lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left	dRec.Top]
dRec.RightndRec.BottomvVisible  
TprMemoObjprMemoObj27dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<$>Query1.ItemsTotal] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTimes New Roman
Font.StylefsBoldfsItalic Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftudRec.Top	
dRec.Right&dRec.Bottom"Visible  
TprMemoObjprMemoObj28dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<p>Query1.TaxRate] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTimes New Roman
Font.StylefsBoldfsItalic Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftudRec.Top%
dRec.Right&dRec.Bottom>Visible  
TprMemoObjprMemoObj29dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<$>Query1.Freight] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTimes New Roman
Font.StylefsBoldfsItalic Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftudRec.TopA
dRec.Right&dRec.BottomZVisible  
TprMemoObjprMemoObj30dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<$>Query1.AmountPaid] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTimes New Roman
Font.StylefsBoldfsItalic Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftudRec.Top]
dRec.Right&dRec.BottomvVisible     TQueryQuery2Active	DatabaseNameDatabaseSQL.Strings*select * from items where OrderNo=:OrderNo LeftPTopX	ParamDataDataType	ftIntegerNameOrderNo	ParamTypeptInputValue�    TprTxReportrTxCustomersTitleCustomers listExportFromPage ExportToPage Values 	Variables OnPreviewGetUserDatarTxCustomersPreviewGetUserDataOnPreviewMouseMoverCustomersPreviewMouseMoveOnPreviewMouseDownrTxCustomersPreviewMouseDownWrapAfterColumn EjectPageAfterPrintLinesOnPage FromLine ToLine ExportFromLine ExportToLine Left� Topx
SystemInfoOS: WIN32_NT 5.1.2600  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.3  	TprTxPage	prTxPage1PageTypetptPageLineNum<ColNumP TprTxHTitleBandprTxHTitleBand1HeightUseVerticalBands TprTxMemoObjprTxMemoObj1dRec.DefVersion dRec.VersionsVisible	Memo.StringsSimple customers list DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlign	prvCenterDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.RightOdRec.BottomVisible   TprTxHDetailBandprTxHDetailBand1HeightUseVerticalBandsDataSetName	CustomersColCountColDirectionprcdTopBottomLeftRight TprTxMemoObjprTxMemoObj2dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Customers.Company] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top 
dRec.Right'dRec.BottomVisible  TprTxMemoObjprTxMemoObj3dRec.DefVersion dRec.VersionsVisible	Memo.Strings(---------------------------------------- DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.Left dRec.Top
dRec.Right(dRec.BottomVisible  TprTxMemoObjprTxMemoObj4dRec.DefVersion dRec.VersionsVisible	Memo.Strings| DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.Left'dRec.Top 
dRec.Right(dRec.BottomVisible     TprTxReport	rTxOrdersTitleOrders listExportFromPage ExportToPage ValuesNameit_ItemsTotalAggFunctionprafSumFormulaQuery1.ItemsTotalResetOn	rvtReportCalcOncvtDataSetNextDataSetNameQuery1 Name
it_FreightAggFunctionprafSumFormulaQuery1.FreightResetOn	rvtReportCalcOncvtDataSetNextDataSetNameQuery1 Nameit_AmountPaidAggFunctionprafSumFormulaQuery1.AmountPaidResetOn	rvtReportCalcOncvtDataSetNextDataSetNameQuery1  	Variables OnPreviewGetUserDatarTxOrdersPreviewGetUserDataOnPreviewMouseMoverOrdersPreviewMouseMoveOnPreviewDblClickrTxOrdersPreviewDblClickWrapAfterColumn EjectPageAfterPrintLinesOnPage FromLine ToLine ExportFromLine ExportToLine Left� Topx
SystemInfoOS: WIN32_NT 5.1.2600  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.3  	TprTxPage	prTxPage1PageTypetptRollLineNum<ColNumq TprTxHTitleBandprTxHTitleBand1HeightUseVerticalBands TprTxMemoObjprTxMemoObj1dRec.DefVersion dRec.VersionsVisible	Memo.StringsOrders list for customer DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.Right@dRec.BottomVisible  TprTxMemoObjprTxMemoObj2dRec.DefVersion dRec.VersionsVisible	Memo.StringsCustNo  : [Customers.CustNo]Company : [Customers.Company] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.RightDdRec.BottomVisible  TprTxMemoObjprTxMemoObj3dRec.DefVersion dRec.VersionsVisible	Memo.StringsOrder No DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlign	prvCenterDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.Right	dRec.Bottom
Visible  TprTxMemoObjprTxMemoObj4dRec.DefVersion dRec.VersionsVisible	Memo.Strings	Sale date DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlign	prvCenterDefaultFontWordWrap  	dRec.Left
dRec.Top
dRec.RightdRec.Bottom
Visible  TprTxMemoObjprTxMemoObj6dRec.DefVersion dRec.VersionsVisible	Memo.Strings	Ship date DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlign	prvCenterDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.Right#dRec.Bottom
Visible  TprTxMemoObjprTxMemoObj7dRec.DefVersion dRec.VersionsVisible	Memo.StringsPaymentmethod DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlign	prvCenterDefaultFontWordWrap  	dRec.Left$dRec.Top
dRec.Right0dRec.Bottom
Visible  TprTxMemoObjprTxMemoObj5dRec.DefVersion dRec.VersionsVisible	Memo.StringsItems total DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlign	prvCenterDefaultFontWordWrap  	dRec.Left1dRec.Top
dRec.Right@dRec.Bottom
Visible  TprTxMemoObjprTxMemoObj9dRec.DefVersion dRec.VersionsVisible	Memo.StringsTax rate DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlign	prvCenterDefaultFontWordWrap  	dRec.LeftAdRec.Top
dRec.RightPdRec.Bottom
Visible  TprTxMemoObjprTxMemoObj10dRec.DefVersion dRec.VersionsVisible	Memo.StringsFreight DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlign	prvCenterDefaultFontWordWrap  	dRec.LeftQdRec.Top
dRec.Right`dRec.Bottom
Visible  TprTxMemoObjprTxMemoObj11dRec.DefVersion dRec.VersionsVisible	Memo.Strings
AmountPaid DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlign	prvCenterDefaultFontWordWrap  	dRec.LeftadRec.Top
dRec.RightpdRec.Bottom
Visible  TprTxMemoObjprTxMemoObj8dRec.DefVersion dRec.VersionsVisible	Memo.Stringsq----------------------------------------------------------------------------------------------------------------- DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.Left dRec.Top

dRec.RightqdRec.BottomVisible   TprTxHDetailBandprTxHDetailBand1HeightUseVerticalBandsDataSetNameQuery1ColCount ColDirectionprcdTopBottomLeftRight TprTxMemoObjprTxMemoObj12dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query1.OrderNo] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top 
dRec.Right	dRec.BottomVisible  TprTxMemoObjprTxMemoObj13dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<d>Query1.SaleDate] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlignprvTopDefaultFontWordWrap  	dRec.Left
dRec.Top 
dRec.RightdRec.BottomVisible  TprTxMemoObjprTxMemoObj14dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<d>Query1.ShipDate] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top 
dRec.Right#dRec.BottomVisible  TprTxMemoObjprTxMemoObj15dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query1.PaymentMethod] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlignprvTopDefaultFontWordWrap  	dRec.Left$dRec.Top 
dRec.Right0dRec.BottomVisible  TprTxMemoObjprTxMemoObj18dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<\ts $>Query1.Freight] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.LeftQdRec.Top 
dRec.Right`dRec.BottomVisible  TprTxMemoObjprTxMemoObj19dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<\ts $>Query1.AmountPaid] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.LeftadRec.Top 
dRec.RightpdRec.BottomVisible  TprTxMemoObjprTxMemoObj17dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<\ts 0.00%>Query1.TaxRate] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.LeftAdRec.Top 
dRec.RightPdRec.BottomVisible  TprTxMemoObjprTxMemoObj20dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<\ts $>Query1.ItemsTotal] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.Left1dRec.Top 
dRec.Right@dRec.BottomVisible   TprTxHSummaryBandprTxHSummaryBand1HeightUseVerticalBands TprTxMemoObjprTxMemoObj21dRec.DefVersion dRec.VersionsVisible	Memo.Stringsq----------------------------------------------------------------------------------------------------------------- DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.Left dRec.Top
dRec.RightqdRec.BottomVisible  TprTxMemoObjprTxMemoObj22dRec.DefVersion dRec.VersionsVisible	Memo.Stringsq----------------------------------------------------------------------------------------------------------------- DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.Left dRec.Top
dRec.RightqdRec.BottomVisible  TprTxMemoObjprTxMemoObj23dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<\ts $>it_ItemsTotal] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.Left1dRec.Top
dRec.Right@dRec.BottomVisible  TprTxMemoObjprTxMemoObj24dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<\ts $>it_Freight] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.LeftQdRec.Top
dRec.Right`dRec.BottomVisible  TprTxMemoObjprTxMemoObj25dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<\ts $>it_AmountPaid] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.LeftadRec.Top
dRec.RightpdRec.BottomVisible  TprTxMemoObjprTxMemoObj16dRec.DefVersion dRec.VersionsVisible	Memo.StringsTOTAL: DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.Left*dRec.Top
dRec.Right0dRec.BottomVisible     TprTxReportrTxOrderTitleOrderExportFromPage ExportToPage Values 	Variables WrapAfterColumn EjectPageAfterPrintLinesOnPage FromLine ToLine ExportFromLine ExportToLine Left Topx
SystemInfoOS: WIN32_NT 5.1.2600  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.3  	TprTxPage	prTxPage1PageTypetptPageLineNum<ColNumP TprTxHTitleBandprTxHTitleBand1HeightUseVerticalBands TprTxMemoObjprTxMemoObj1dRec.DefVersion dRec.VersionsVisible	Memo.StringsN------------------------------------------------------------------------------ DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top 
dRec.RightOdRec.BottomVisible  TprTxMemoObjprTxMemoObj2dRec.DefVersion dRec.VersionsVisible	Memo.Strings| DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.RightdRec.BottomVisible  TprTxMemoObjprTxMemoObj4dRec.DefVersion dRec.VersionsVisible	Memo.Strings| DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftNdRec.Top
dRec.RightOdRec.BottomVisible  TprTxMemoObjprTxMemoObj3dRec.DefVersion dRec.VersionsVisible	Memo.StringsN------------------------------------------------------------------------------ DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.RightOdRec.BottomVisible  TprTxMemoObjprTxMemoObj5dRec.DefVersion dRec.VersionsVisible	Memo.StringsOrder No : [Query1.OrderNo] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.RightMdRec.BottomVisible  TprTxMemoObjprTxMemoObj6dRec.DefVersion dRec.VersionsVisible	Memo.Strings	Customer: DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.RightdRec.BottomVisible  TprTxMemoObjprTxMemoObj7dRec.DefVersion dRec.VersionsVisible	Memo.Strings
Sale date: DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.RightdRec.BottomVisible  TprTxMemoObjprTxMemoObj8dRec.DefVersion dRec.VersionsVisible	Memo.Strings
Ship date: DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.Left(dRec.Top
dRec.Right5dRec.BottomVisible  TprTxMemoObjprTxMemoObj9dRec.DefVersion dRec.VersionsVisible	Memo.StringsPaymentmethod: DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top

dRec.RightdRec.BottomVisible  TprTxMemoObjprTxMemoObj10dRec.DefVersion dRec.VersionsVisible	Memo.Strings	Ship VIA: DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.RightdRec.Bottom	Visible  TprTxMemoObjprTxMemoObj11dRec.DefVersion dRec.VersionsVisible	Memo.StringsTerms: DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.Left(dRec.Top
dRec.Right5dRec.Bottom	Visible  TprTxMemoObjprTxMemoObj12dRec.DefVersion dRec.VersionsVisible	Memo.Strings'[Customers.CustNo]  [Customers.Company] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.RightNdRec.BottomVisible  TprTxMemoObjprTxMemoObj13dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<d>Query1.SaleDate] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.Right&dRec.BottomVisible  TprTxMemoObjprTxMemoObj14dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query1.ShipVIA] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.Right&dRec.Bottom	Visible  TprTxMemoObjprTxMemoObj15dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<d>Query1.ShipDate] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.Left7dRec.Top
dRec.RightNdRec.BottomVisible  TprTxMemoObjprTxMemoObj16dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query1.Terms] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.Left7dRec.Top
dRec.RightNdRec.Bottom	Visible  TprTxMemoObjprTxMemoObj17dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query1.PaymentMethod] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top

dRec.Right&dRec.BottomVisible  TprTxMemoObjprTxMemoObj18dRec.DefVersion dRec.VersionsVisible	Memo.StringsItem no DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.Right#dRec.BottomVisible  TprTxMemoObjprTxMemoObj19dRec.DefVersion dRec.VersionsVisible	Memo.StringsPart no DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlignprvTopDefaultFontWordWrap  	dRec.Left$dRec.Top
dRec.Right.dRec.BottomVisible  TprTxMemoObjprTxMemoObj20dRec.DefVersion dRec.VersionsVisible	Memo.StringsQuantity DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlignprvTopDefaultFontWordWrap  	dRec.Left/dRec.Top
dRec.Right>dRec.BottomVisible  TprTxMemoObjprTxMemoObj22dRec.DefVersion dRec.VersionsVisible	Memo.StringsDiscount DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlign	prhCentervAlignprvTopDefaultFontWordWrap  	dRec.Left?dRec.Top
dRec.RightNdRec.BottomVisible  TprTxMemoObjprTxMemoObj23dRec.DefVersion dRec.VersionsVisible	Memo.Strings
---------- DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.Right#dRec.BottomVisible  TprTxMemoObjprTxMemoObj24dRec.DefVersion dRec.VersionsVisible	Memo.Strings
---------- DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.Left$dRec.Top
dRec.Right.dRec.BottomVisible  TprTxMemoObjprTxMemoObj25dRec.DefVersion dRec.VersionsVisible	Memo.Strings--------------- DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.Left/dRec.Top
dRec.Right>dRec.BottomVisible  TprTxMemoObjprTxMemoObj26dRec.DefVersion dRec.VersionsVisible	Memo.Strings--------------- DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.Left?dRec.Top
dRec.RightNdRec.BottomVisible  TprTxMemoObjprTxMemoObj31dRec.DefVersion dRec.VersionsVisible	Memo.StringsN------------------------------------------------------------------------------ DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.RightOdRec.BottomVisible   TprTxHDetailBandprTxHDetailBand1HeightUseVerticalBandsDataSetNameQuery2ColCount ColDirectionprcdTopBottomLeftRight TprTxMemoObjprTxMemoObj27dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query2.ItemNo] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top 
dRec.Right#dRec.BottomVisible  TprTxMemoObjprTxMemoObj28dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query2.PartNo] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.Left$dRec.Top 
dRec.Right.dRec.BottomVisible  TprTxMemoObjprTxMemoObj29dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query2.Qty] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.Left/dRec.Top 
dRec.Right>dRec.BottomVisible  TprTxMemoObjprTxMemoObj30dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Query2.Discount] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhRightvAlignprvTopDefaultFontWordWrap  	dRec.Left?dRec.Top 
dRec.RightNdRec.BottomVisible   TprTxHSummaryBandprTxHSummaryBand1HeightUseVerticalBands TprTxMemoObjprTxMemoObj21dRec.DefVersion dRec.VersionsVisible	Memo.Strings(Items total: [:<\ts $>Query1.ItemsTotal]%Tax rate   : [:<\ts p>Query1.TaxRate]%Freight    : [:<\ts $>Query1.Freight](Amount paid: [:<\ts $>Query1.AmountPaid] DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.Right2dRec.BottomVisible  TprTxMemoObjprTxMemoObj33dRec.DefVersion dRec.VersionsVisible	Memo.StringsN------------------------------------------------------------------------------ DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYhAlignprhLeftvAlignprvTopDefaultFontWordWrap  	dRec.LeftdRec.Top
dRec.RightOdRec.BottomVisible      