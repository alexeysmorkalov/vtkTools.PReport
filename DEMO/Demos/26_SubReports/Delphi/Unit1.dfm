�
 TFORM1 0"W  TPF0TForm1Form1LeftTop� BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionSubReports demoClientHeightRClientWidth�Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderOnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel1LeftTop� Width8HeightCaptionMain report:  TLabelLabel2Left TopWidth#HeightCaptionDetails:  TMemoMemo1LeftTopWidth�Height� BorderStylebsNoneColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold Lines.StringsNPReport supports sub reports from version 2.0. The subreport can be specified Cfor the band and will be inserted into main report after this band. 	Features:*  - The enclosed subreports are supported.3  - Cross-tab reports can be defined as subreports. Restrictions:5  - Cross-tab reports can not contain the subreports.;  - Subreports can be defined only for horizontal sections:    - Detail    - Detail header    - Detail footer    - Group header    - Group footer    - Summary 
ParentFontTabOrder   	TComboBoxedMainReportLeftHTop� Width� HeightStylecsDropDownList
ItemHeightTabOrderOnClickedMainReportClick  	TComboBoxedDetailsReportLeftHTopWidth� HeightStylecsDropDownList
ItemHeightTabOrder  TButtonButton1Left(Top� WidthKHeightCaption	Design...TabOrderOnClickButton1Click  TButtonButton2Left(TopWidthKHeightCaption	Design...TabOrderOnClickButton2Click  TButtonButton3Left�TopWidthKHeightCaption
Preview...TabOrderOnClickButton3Click  TButtonButton4Left�Top� WidthKHeightCaption
Preview...TabOrderOnClickButton4Click  TTablecustomerActive	DatabaseNameDBDEMOS	TableNamecustomer.dbLeftTop0  TQueryordersActive	DatabaseNameDBDEMOSSQL.Strings6select orders.*, employee.firstname, employee.lastname   from orders, employeeJwhere orders.empno = employee.empno and custno = :custno order by saledate Left0Top0	ParamDataDataType	ftIntegerNamecustno	ParamType	ptUnknownValue�    	TprReportCustomersReportShowProgress	Title	CustomersValuesNameCustomersTotalAggFunction	prafCountResetOn	rvtReportCalcOncvtDataSetNextDataSetNamecustomer  	Variables PrinterNameVirtual printerOnBandGenerateCellCustomersReportBandGenerateCellOnBeginSubReportGenerate%CustomersReportBeginSubReportGenerateLeft`Top0
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi5PReport version: 1.9.6 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1Height3 
TprMemoObj
prMemoObj1dRec.VersionsVisible	Memo.Strings	Customers lBorder.WidthrBorder.Show	rBorder.WidthtBorder.WidthbBorder.Show	bBorder.Width	FillColor��� vAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.Style FontSize  	dRec.Left
dRec.Top
dRec.RightdRec.Bottom+  
TprMemoObj
prMemoObj2dRec.VersionsVisible	Memo.Strings(Date: [:<dd.mm.yyyy hh:nn>StartDateTime] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitevAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclGrayFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left
dRec.Top
dRec.Right�dRec.Bottom  
TprMemoObj
prMemoObj3dRec.VersionsVisible	Memo.StringsTotal: [CustomersTotal] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitevAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBoldfsUnderline FontSize  	dRec.Left
dRec.Top
dRec.Right�dRec.Bottom+   TprHDetailBand
DetailBand
ResizeModeprbrmMaxObjHeightHDataSetNamecustomer 
TprMemoObj
prMemoObj4dRec.VersionsVisible	Memo.StringsCustNo: [customer.CustNo] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitevAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.LeftzdRec.Top
dRec.Right�dRec.Bottom  
TprMemoObj
prMemoObj5dRec.VersionsVisible	Memo.Strings[customer.Company] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� vAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.StylefsBold FontSize  	dRec.Left
dRec.Top
dRec.RightsdRec.Bottom   
TprMemoObj
prMemoObj6dRec.VersionsVisible	Memo.Strings[customer.Addr1] lBorder.WidthrBorder.Show	rBorder.WidthtBorder.WidthbBorder.Show	bBorder.Width	FillColor��� vAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftBdRec.Top'
dRec.Right#dRec.Bottom8  
TprMemoObj
prMemoObj7dRec.VersionsVisible	Memo.StringsAddress: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlignprhRightvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.Top'
dRec.Right;dRec.Bottom8  
TprMemoObj
prMemoObj8dRec.VersionsVisible	Memo.StringsPhone / FAX: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlignprhRightvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left*dRec.Top'
dRec.RightsdRec.Bottom8  
TprMemoObj
prMemoObj9dRec.VersionsVisible	Memo.Strings![customer.Phone] / [customer.FAX] lBorder.WidthrBorder.Show	rBorder.WidthtBorder.WidthbBorder.Show	bBorder.Width	FillColor��� vAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftzdRec.Top'
dRec.RightSdRec.Bottom8  
TprMemoObjprMemoObj10dRec.VersionsVisible	Memo.Strings[customer.Contact] lBorder.WidthrBorder.Show	rBorder.WidthtBorder.WidthbBorder.Show	bBorder.Width	FillColor��� vAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftZdRec.Top'
dRec.Right�dRec.Bottom8   TprHPageFooterBandprHPageFooterBand1HeightPrintOnFirstPage	 
TprMemoObjprMemoObj11dRec.VersionsVisible	Memo.StringsPage [Page] of [PagesCount] lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Width	FillColor��� hAlignprhRightvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftjdRec.Top
dRec.Right�dRec.Bottom  
TprMemoObjprMemoObj12dRec.VersionsVisible	Memo.Strings"[:<dd.mm.yyyy hh:nn>StartDateTime] lBorder.Show	lBorder.WidthrBorder.WidthtBorder.Show	tBorder.WidthbBorder.Width	FillColor��� vAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left
dRec.Top
dRec.RightcdRec.Bottom     	TprReportShortOrdersListSubTitleOrders list (short form)ValuesNameordersTotalFormulaorders.ItemsTotalResetOn	rvtReportCalcOncvtDataSetNextDataSetNameorders  	Variables PrinterNameVirtual printerLeft� Top0
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi5PReport version: 1.9.6 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHDetailBandprHDetailBand1
ResizeModeprbrmMaxObjHeight+DataSetNameordersColCountColDirectionprcdLeftRightTopBottomBandsprHDetailFooterBand1  
TprMemoObj
prMemoObj1dRec.VersionsVisible	Memo.Strings
Sale date: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.LeftdRec.Top
dRec.Right;dRec.Bottom  
TprMemoObj
prMemoObj2dRec.VersionsVisible	Memo.StringsTotal: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.LeftdRec.Top
dRec.Right;dRec.Bottom#  
TprMemoObj
prMemoObj3dRec.VersionsVisible	Memo.Strings[orders.ShipDate] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� vAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftBdRec.Top
dRec.Right� dRec.Bottom  
TprMemoObj
prMemoObj4dRec.VersionsVisible	Memo.Strings[orders.ItemsTotal] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� vAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftBdRec.Top
dRec.Right� dRec.Bottom#   TprHDetailFooterBandprHDetailFooterBand1
ResizeModeprbrmMaxObjHeight(
DetailBand!ShortOrdersListSub.prHDetailBand1ColCountColDirectionprcdLeftRightTopBottomLinkToDetail 
TprMemoObj
prMemoObj6dRec.VersionsVisible	Memo.StringsTotal: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.LeftdRec.Top
dRec.Right<dRec.Bottom  
TprMemoObj
prMemoObj8dRec.VersionsVisible	Memo.Strings[ordersTotal] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� vAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBoldfsUnderline FontSize  	dRec.LeftCdRec.Top
dRec.Right� dRec.Bottom     	TprReportOrdersListSubTitleOrders list (full)ValuesNameordersItemsTotalFormulaorders.ItemsTotalResetOn	rvtReportCalcOncvtDataSetNextDataSetNameorders  	Variables PrinterNameVirtual printerLeft� Top0
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi5PReport version: 1.9.6 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHDetailHeaderBandprHDetailHeaderBand1Height
DetailBandOrdersListSub.prHDetailBand1ReprintOnEachPage	 
TprMemoObj
prMemoObj1dRec.VersionsVisible	Memo.StringsOrderNo lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftdRec.Top
dRec.Right3dRec.Bottom  
TprMemoObj
prMemoObj2dRec.VersionsVisible	Memo.Strings	Sale date lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left:dRec.Top
dRec.Right� dRec.Bottom  
TprMemoObj
prMemoObj3dRec.VersionsVisible	Memo.Strings	Ship date lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left� dRec.Top
dRec.Right� dRec.Bottom  
TprMemoObj
prMemoObj4dRec.VersionsVisible	Memo.StringsEmployee lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left� dRec.Top
dRec.Right�dRec.Bottom  
TprMemoObj
prMemoObj6dRec.VersionsVisible	Memo.StringsShip VIA lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left�dRec.Top
dRec.RightdRec.Bottom  
TprMemoObj
prMemoObj7dRec.VersionsVisible	Memo.StringsTotal lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftrdRec.Top
dRec.Right�dRec.Bottom  
TprMemoObj
prMemoObj5dRec.VersionsVisible	Memo.StringsPayment method lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftdRec.Top
dRec.RightkdRec.Bottom   TprHDetailBandprHDetailBand1
ResizeModeprbrmMaxObjHeight(DataSetNameordersBandsprHDetailHeaderBand1prHDetailFooterBand1  
TprMemoObj
prMemoObj8dRec.VersionsVisible	Memo.Strings[orders.OrderNo] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� vAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.LeftdRec.Top
dRec.Right3dRec.Bottom  
TprMemoObj
prMemoObj9dRec.VersionsVisible	Memo.Strings[orders.SaleDate] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left:dRec.Top
dRec.Right� dRec.Bottom  
TprMemoObjprMemoObj10dRec.VersionsVisible	Memo.Strings[orders.ShipDate] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left� dRec.Top
dRec.Right� dRec.Bottom  
TprMemoObjprMemoObj11dRec.VersionsVisible	Memo.Strings$[orders.firstname] [orders.lastname] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� vAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left� dRec.Top
dRec.Right�dRec.Bottom  
TprMemoObjprMemoObj12dRec.VersionsVisible	Memo.Strings[orders.ShipVIA] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left�dRec.Top
dRec.RightdRec.Bottom  
TprMemoObjprMemoObj13dRec.VersionsVisible	Memo.Strings[orders.ItemsTotal] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.LeftrdRec.Top
dRec.Right�dRec.Bottom  
TprMemoObjprMemoObj14dRec.VersionsVisible	Memo.Strings[orders.PaymentMethod] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� hAlign	prhCentervAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left*dRec.Top
dRec.RightkdRec.Bottom  TprImageObjprImageObj1dRec.VersionsVisible		FillColorclNone  	dRec.LeftdRec.Top
dRec.Right'dRec.Bottom   TprHDetailFooterBandprHDetailFooterBand1Height
DetailBandOrdersListSub.prHDetailBand1ColCount ColDirectionprcdTopBottomLeftRightLinkToDetail 
TprMemoObjprMemoObj15dRec.VersionsVisible	Memo.Strings[ordersItemsTotal] lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftrdRec.Top
dRec.Right�dRec.Bottom     TprTxReportTxCustomersReportTitleCustomers (text report)ValuesNameCustomersTotalAggFunction	prafCountResetOn	rvtReportCalcOncvtDataSetNextDataSetNamecustomer  	Variables Left� Top0
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi5PReport version: 1.9.6  	TprTxPage	prTxPage1 TprTxHTitleBandprTxHTitleBand1Height TprTxMemoObjprTxMemoObj1dRec.VersionsVisible	Memo.Strings)Date : [:<dd.mm.yyyy hh:nn>StartDateTime]   	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom  TprTxMemoObjprTxMemoObj2dRec.VersionsVisible	Memo.StringsTotal: [CustomersTotal]   	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom  TprTxMemoObjprTxMemoObj3dRec.VersionsVisible	Memo.StringsC U S T O M E R S hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom   TprTxHDetailBand
DetailBandHeightDataSetNamecustomer TprTxMemoObjprTxMemoObj4dRec.VersionsVisible	Memo.Strings[customer.Company]   	dRec.LeftdRec.Top
dRec.RightBdRec.Bottom  TprTxMemoObjprTxMemoObj5dRec.VersionsVisible	Memo.Strings([customer.CustNo]) hAlignprhRight  	dRec.LeftCdRec.Top
dRec.RightOdRec.Bottom  TprTxMemoObjprTxMemoObj6dRec.VersionsVisible	Memo.StringsAddress:   	dRec.LeftdRec.Top
dRec.RightdRec.Bottom  TprTxMemoObjprTxMemoObj7dRec.VersionsVisible	Memo.Strings[customer.Addr1]   	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom  TprTxHLineObjprTxHLineObj1dRec.VersionsVisible	MainChar=StartingChar=
EndingChar=  	dRec.LeftdRec.Top 
dRec.RightOdRec.Bottom  TprTxHLineObjprTxHLineObj2dRec.VersionsVisible	MainChar=StartingChar=
EndingChar=  	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom     TprTxReportTxOrdersListSubTitleOrders list (full)ValuesNameordersItemsTotalFormulaorders.ItemsTotalResetOn	rvtReportCalcOncvtDataSetNextDataSetNameorders  	Variables Left� Top0
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi5PReport version: 1.9.6  	TprTxPage	prTxPage1ColNumd TprTxHDetailHeaderBandprTxHDetailHeaderBand1Height
DetailBand TxOrdersListSub.prTxHDetailBand1 TprTxMemoObjprTxMemoObj1dRec.VersionsVisible	Memo.StringsOrderNo hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.Right
dRec.Bottom  TprTxMemoObjprTxMemoObj2dRec.VersionsVisible	Memo.Strings	Sale date hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.RightdRec.Bottom  TprTxMemoObjprTxMemoObj3dRec.VersionsVisible	Memo.Strings	Ship date hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.Right*dRec.Bottom  TprTxMemoObjprTxMemoObj4dRec.VersionsVisible	Memo.StringsEmployee hAlign	prhCenter  	dRec.Left+dRec.Top
dRec.RightDdRec.Bottom  TprTxMemoObjprTxMemoObj5dRec.VersionsVisible	Memo.StringsShip VIA hAlign	prhCenter  	dRec.LeftEdRec.Top
dRec.RightQdRec.Bottom  TprTxMemoObjprTxMemoObj6dRec.VersionsVisible	Memo.StringsTotal hAlign	prhCenter  	dRec.LeftRdRec.Top
dRec.RightcdRec.Bottom   TprTxHDetailBandprTxHDetailBand1HeightDataSetNameordersBandsprTxHDetailHeaderBand1prTxHDetailFooterBand1  TprTxMemoObjprTxMemoObj7dRec.VersionsVisible	Memo.Strings[orders.OrderNo]   	dRec.LeftdRec.Top
dRec.Right
dRec.Bottom  TprTxMemoObjprTxMemoObj8dRec.VersionsVisible	Memo.Strings[orders.SaleDate] hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.RightdRec.Bottom  TprTxMemoObjprTxMemoObj9dRec.VersionsVisible	Memo.Strings[orders.ShipDate] hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.Right*dRec.Bottom  TprTxMemoObjprTxMemoObj10dRec.VersionsVisible	Memo.Strings$[orders.firstname] [orders.lastname]   	dRec.Left+dRec.Top
dRec.RightDdRec.Bottom  TprTxMemoObjprTxMemoObj11dRec.VersionsVisible	Memo.Strings[orders.ShipVIA] hAlign	prhCenter  	dRec.LeftEdRec.Top
dRec.RightQdRec.Bottom  TprTxMemoObjprTxMemoObj12dRec.VersionsVisible	Memo.Strings[orders.ItemsTotal] hAlignprhRight  	dRec.LeftRdRec.Top
dRec.RightcdRec.Bottom   TprTxHDetailFooterBandprTxHDetailFooterBand1Height
DetailBand TxOrdersListSub.prTxHDetailBand1ColCount ColDirectionprcdTopBottomLeftRightLinkToDetail TprTxMemoObjprTxMemoObj13dRec.VersionsVisible	Memo.Strings[ordersItemsTotal] hAlignprhRight  	dRec.LeftRdRec.Top
dRec.RightcdRec.Bottom      