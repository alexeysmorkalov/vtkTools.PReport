�
 TFORM1 0OU  TPF0TForm1Form1Left� Top� BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionChange page numberingClientHeightZClientWidthcColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPixelsPerInch`
TextHeight TButtonButton1Left� Top0WidthKHeightCaptionPrintTabOrder OnClickButton1Click  TButtonButton2Left� TopWidthKHeightCaptionDesignerTabOrderOnClickButton2Click  TQueryRepQueryDatabaseNameDBDEMOSSQL.Strings4select * from customer a, orders b, items c, parts dwhere a.custno = b.custno  and b.orderno = c.orderno  and c.partno = d.partnoorder by a.company, orderno LeftTop  	TprReport	prReport1ShowProgress	ExportFromPage ExportToPage ValuesGroupprReport1.OrderGroupName
OrderTotalAggFunctionprafSumFormulaRepQuery.Qty*RepQuery.ListPriceResetOnrvtGroupCalcOncvtDataSetNextDataSetNameRepQuery GroupprReport1.CustomerGroupNameCustomerTotalAggFunctionprafSumFormulaRepQuery.Qty*RepQuery.ListPriceResetOnrvtGroupCalcOncvtDataSetNextDataSetNameRepQuery GroupprReport1.CustomerGroupNameCustomerAvgAggFunctionprafAvgFormulaRepQuery.Qty*RepQuery.ListPriceResetOnrvtGroupCalcOncvtDataSetNextDataSetNameRepQuery GroupprReport1.OrderGroupNameOrderPartCountAggFunction	prafCountResetOnrvtGroupCalcOncvtDataSetNextDataSetNameRepQuery  	Variables PrinterNameVirtual printerOnBandGenerateCellprReport1BandGenerateCellPreviewParams.Options PreviewParams.ShowToolbarsprptPreviewCommon Left(Top
SystemInfoOS: WIN32_NT 5.1.2600  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.2 
LOGPIXELSX`
LOGPIXELSY` TprPage Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1Height$UseVerticalBands 
TprMemoObj
prMemoObj1dRec.DefVersion dRec.VersionsVisible	Memo.StringsList of clients lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBoldfsItalic Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left� dRec.Top
dRec.Right�dRec.Bottom Visible   TprHDetailBandprHDetailBand1HeightUseVerticalBandsDataSetNameRepQueryColCountColDirectionprcdTopBottomLeftRightGroupsCustomerGroup
OrderGroup  
TprMemoObj
prMemoObj2dRec.DefVersion dRec.VersionsVisible	Memo.Strings[RepQuery.PartNo] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left4dRec.Top
dRec.RightydRec.BottomVisible  
TprMemoObj
prMemoObj3dRec.DefVersion dRec.VersionsVisible	Memo.Strings[RepQuery.Description] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left|dRec.Top
dRec.Right�dRec.BottomVisible  
TprMemoObj
prMemoObj4dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<c>RepQuery.ListPrice] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left�dRec.Top
dRec.RightdRec.BottomVisible  
TprMemoObj
prMemoObj5dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<#,0.000>RepQuery.Qty] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top
dRec.RightqdRec.BottomVisible  
TprMemoObj
prMemoObj6dRec.DefVersion dRec.VersionsVisible	Memo.Strings%[:<c>RepQuery.ListPrice*RepQuery.Qty] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize Formula-CustomerAvg<(RepQuery.ListPrice*RepQuery.Qty)Visible	Memo.Strings%[:<c>RepQuery.ListPrice*RepQuery.Qty] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetRUSSIAN_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize FormulaOrderPartCount=1Visible	Memo.Strings%[:<c>RepQuery.ListPrice*RepQuery.Qty] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.Color Font.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LefttdRec.Top
dRec.Right�dRec.BottomVisible  
TprMemoObjprMemoObj12dRec.DefVersion dRec.VersionsVisible	Memo.Strings[OrderGroup.LineNo()] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top
dRec.Right2dRec.BottomVisible   TprHGroupHeaderBandprHGroupHeaderBand1Height8UseVerticalBandsGroupprReport1.OrderGroupColCount ColDirectionprcdTopBottomLeftRightLinkToDetailStartNewPageReprintOnEachPage 
TprMemoObj
prMemoObj7dRec.DefVersion dRec.VersionsVisible	Memo.Strings2Order N [RepQuery.OrderNo] [:<d>RepQuery.SaleDate] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlignprhLeftvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftdRec.Top
dRec.Right�dRec.BottomVisible  
TprMemoObj dRec.DefVersion dRec.VersionsVisible	Memo.StringsN lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left4dRec.Top
dRec.RightydRec.Bottom0Visible  
TprMemoObj dRec.DefVersion dRec.VersionsVisible	Memo.StringsDescription lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left|dRec.Top
dRec.Right�dRec.Bottom0Visible  
TprMemoObj dRec.DefVersion dRec.VersionsVisible	Memo.StringsCost lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left�dRec.Top
dRec.RightdRec.Bottom0Visible  
TprMemoObj dRec.DefVersion dRec.VersionsVisible	Memo.StringsQuantity lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top
dRec.RightqdRec.Bottom0Visible  
TprMemoObj dRec.DefVersion dRec.VersionsVisible	Memo.StringsSum lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LefttdRec.Top
dRec.Right�dRec.Bottom0Visible   TprHGroupFooterBandprHGroupFooterBand1HeightUseVerticalBandsGroupprReport1.OrderGroupColCount ColDirectionprcdTopBottomLeftRightLinkToDetail 
TprMemoObj dRec.DefVersion dRec.VersionsVisible	Memo.Strings7Summary by order N [RepQuery.OrderNo]: [:<c>OrderTotal] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftdRec.Top
dRec.Right�dRec.BottomVisible   TprHGroupHeaderBandprHGroupHeaderBand2HeightdUseVerticalBandsGroupprReport1.CustomerGroupColCount ColDirectionprcdTopBottomLeftRightLinkToDetailStartNewPage	ReprintOnEachPage 
TprMemoObj
prMemoObj8dRec.DefVersion dRec.VersionsVisible	lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftdRec.Top
dRec.Right�dRec.Bottom`Visible  
TprMemoObj
prMemoObj9dRec.DefVersion dRec.VersionsVisible	Memo.StringsPhone lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclNonehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left�dRec.Top
dRec.RightZdRec.BottomVisible  
TprMemoObj dRec.DefVersion dRec.VersionsVisible	Memo.StringsFax lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclNonehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftedRec.Top
dRec.Right�dRec.BottomVisible  
TprMemoObj dRec.DefVersion dRec.VersionsVisible	Memo.StringsCompany lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclNonehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top
dRec.Right�dRec.BottomVisible  
TprMemoObj dRec.DefVersion dRec.VersionsVisible	Memo.Strings[RepQuery.Company] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclNonehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftdRec.Top%
dRec.Right�dRec.BottomBVisible  
TprMemoObj dRec.DefVersion dRec.VersionsVisible	Memo.Strings[RepQuery.Phone] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclNonehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left�dRec.Top%
dRec.RightZdRec.BottomBVisible  
TprMemoObj dRec.DefVersion dRec.VersionsVisible	Memo.Strings[RepQuery.FAX] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclNonehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.LeftedRec.Top%
dRec.Right�dRec.BottomBVisible  
TprMemoObjprMemoObj10dRec.DefVersion dRec.VersionsVisible	Memo.Strings+Average cost of position: [:<c>CustomerAvg] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclNonehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.Color Font.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left�dRec.TopG
dRec.Right�dRec.Bottom\Visible   TprHGroupFooterBandprHGroupFooterBand2Height,UseVerticalBandsGroupprReport1.CustomerGroupColCount ColDirectionprcdTopBottomLeftRightLinkToDetail 
TprMemoObjprMemoObj11dRec.DefVersion dRec.VersionsVisible	Memo.Strings'Summary by company: [:<c>CustomerTotal]*Average sum by position: [:<c>CustomerAvg] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclAquahAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftdRec.Top
dRec.Right�dRec.Bottom(Visible   TprHPageFooterBandprHPageFooterBand1Height UseVerticalBandsPrintOnFirstPage	PrintAfterLastBandOnPage 
TprMemoObjprMemoObj13dRec.DefVersion dRec.VersionsVisible	Memo.StringsPage [Page] / [PagesCount] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left1dRec.Top
dRec.Right�dRec.BottomVisible    TprGroup
OrderGroupValidRepQuery.OrderNo
DetailBandprReport1.prHDetailBand1  TprGroupCustomerGroupValidRepQuery.CustNo
DetailBandprReport1.prHDetailBand1    