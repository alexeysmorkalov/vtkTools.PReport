�
 TFORM1 0f6  TPF0TForm1Form1LeftwTop� BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionAppendPreparedReportClientHeight� ClientWidth�Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPixelsPerInch`
TextHeight TMemoMemo1LeftTopWidth�HeightIBorderStylebsNoneColor	clBtnFaceLines.StringsOThis demo demostrates the using of TprCustomReport.AppendPreparedReport method. 4This application generates a report (prRecordReport)&for each record of CUSTOMER table and appends it to the prMainReport. TabOrder   	TGroupBox	GroupBox1LeftTop`Width� HeightACaption TprReport: TabOrder TButtonButton1LeftTopWidthKHeightCaption	Design...TabOrder OnClickButton1Click  TButtonButton2LeftXTopWidthyHeightCaptionPreview full report...TabOrderOnClickButton2Click   	TGroupBox	GroupBox2Left� Top`Width� HeightACaption TprTxReport: TabOrder TButtonButton3LeftTopWidthKHeightCaption	Design...TabOrder OnClickButton3Click  TButtonButton4LeftXTopWidthyHeightCaptionPreview full report...TabOrderOnClickButton4Click   TProgressBarPBLeftTop� Width�HeightTabOrderVisible  TQuery	QCUSTOMERDatabaseNameDBDEMOSSQL.Strings-select * from customer where custno = :custno LeftTop`	ParamDataDataType	ftUnknownNamecustno	ParamType	ptUnknown    TQueryQORDERDatabaseNameDBDEMOSSQL.Strings+select * from orders where custno = :custno Left(Top`	ParamDataDataType	ftUnknownNamecustno	ParamType	ptUnknown    TTableCUSTOMERActive	DatabaseNameDBDEMOS	TableNamecustomer.dbLeftHTop`  	TprReportprMainReportValues 	Variables PrinterNameHP LaserJet 1100 (MS)LeftpTop`
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.6 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@   	TprReportprRecordReportValues 	Variables PrinterNameHP LaserJet 1100 (MS)Left�Top`
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.6 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1HeightK 
TprMemoObj
prMemoObj1dRec.VersionsVisible	Memo.StringsCustNo: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize
  	dRec.Left
dRec.Top

dRec.RightsdRec.Bottom#  
TprMemoObj
prMemoObj2dRec.VersionsVisible	Memo.StringsCompany: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize
  	dRec.Left
dRec.Top*
dRec.RightsdRec.BottomC  
TprMemoObj
prMemoObj3dRec.VersionsVisible	Memo.Strings[qcustomer.CustNo] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� vAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize
  	dRec.Left� dRec.Top

dRec.Right#dRec.Bottom#  
TprMemoObj
prMemoObj4dRec.VersionsVisible	Memo.Strings[qcustomer.Company] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� vAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize
  	dRec.Left� dRec.Top*
dRec.Right#dRec.BottomC   TprHPageFooterBandprHPageFooterBand1HeightPrintOnFirstPage	 
TprMemoObj
prMemoObj5dRec.VersionsVisible	Memo.StringsPage [Page] of [PagesCount] lBorder.WidthrBorder.WidthtBorder.Show	tBorder.WidthbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.Top
dRec.Right�dRec.Bottom   TprHDetailHeaderBandprHDetailHeaderBand1Height 
DetailBandprRecordReport.prHDetailBand1 
TprMemoObj
prMemoObj6dRec.VersionsVisible	Memo.StringsOrderNo lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left
dRec.Top
dRec.RightSdRec.Bottom  
TprMemoObj
prMemoObj7dRec.VersionsVisible	Memo.Strings	Sale date lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftZdRec.Top
dRec.Right� dRec.Bottom  
TprMemoObj
prMemoObj8dRec.VersionsVisible	Memo.Strings	Ship date lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left� dRec.Top
dRec.Right3dRec.Bottom  
TprMemoObj
prMemoObj9dRec.VersionsVisible	Memo.StringsShip VIA lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left:dRec.Top
dRec.Right�dRec.Bottom  
TprMemoObjprMemoObj10dRec.VersionsVisible	Memo.StringsTerms lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left�dRec.Top
dRec.Right�dRec.Bottom  
TprMemoObjprMemoObj11dRec.VersionsVisible	Memo.StringsPayment method lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftdRec.Top
dRec.RightkdRec.Bottom  
TprMemoObjprMemoObj12dRec.VersionsVisible	Memo.StringsItems total lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftrdRec.Top
dRec.Right�dRec.Bottom   TprHDetailBandprHDetailBand1Height DataSetNameQORDERBandsprHDetailHeaderBand1  
TprMemoObjprMemoObj13dRec.VersionsVisible	Memo.Strings[qorder.OrderNo] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.Top
dRec.RightSdRec.Bottom  
TprMemoObjprMemoObj14dRec.VersionsVisible	Memo.Strings[qorder.SaleDate] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.LeftZdRec.Top
dRec.Right� dRec.Bottom  
TprMemoObjprMemoObj15dRec.VersionsVisible	Memo.Strings[qorder.ShipDate] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left� dRec.Top
dRec.Right3dRec.Bottom  
TprMemoObjprMemoObj16dRec.VersionsVisible	Memo.Strings[qorder.ShipVIA] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left:dRec.Top
dRec.Right�dRec.Bottom  
TprMemoObjprMemoObj17dRec.VersionsVisible	Memo.Strings[qorder.Terms] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left�dRec.Top
dRec.Right�dRec.Bottom  
TprMemoObjprMemoObj18dRec.VersionsVisible	Memo.Strings[qorder.PaymentMethod] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.LeftdRec.Top
dRec.RightkdRec.Bottom  
TprMemoObjprMemoObj19dRec.VersionsVisible	Memo.Strings[qorder.ItemsTotal] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.LeftrdRec.Top
dRec.Right�dRec.Bottom     TprTxReportprTxMainReportValues 	Variables LeftpTop� 
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.6   TprTxReportprTxRecordReportValues 	Variables Left�Top� 
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.6  	TprTxPage	prTxPage1 TprTxHTitleBandprTxHTitleBand1Height TprTxMemoObjprTxMemoObj1dRec.VersionsVisible	Memo.StringsCompany:   	dRec.LeftdRec.Top
dRec.Right
dRec.Bottom  TprTxMemoObjprTxMemoObj2dRec.VersionsVisible	Memo.StringsCustNo:   	dRec.LeftdRec.Top
dRec.Right
dRec.Bottom  TprTxMemoObjprTxMemoObj3dRec.VersionsVisible	Memo.Strings[qcustomer.company]   	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom  TprTxMemoObjprTxMemoObj4dRec.VersionsVisible	Memo.Strings[qcustomer.custno]   	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom   TprTxHDetailBandprTxHDetailBand1HeightDataSetNameQORDERBandsprTxHDetailHeaderBand1  TprTxMemoObjprTxMemoObj5dRec.VersionsVisible	Memo.Strings[qorder.orderno] hAlignprhRight  	dRec.LeftdRec.Top
dRec.RightdRec.Bottom  TprTxMemoObjprTxMemoObj6dRec.VersionsVisible	Memo.Strings[qorder.saledate] hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.RightdRec.Bottom  TprTxMemoObjprTxMemoObj7dRec.VersionsVisible	Memo.Strings[qorder.shipdate] hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.Right+dRec.Bottom  TprTxMemoObjprTxMemoObj8dRec.VersionsVisible	Memo.Strings[qorder.PaymentMethod] hAlign	prhCenter  	dRec.Left,dRec.Top
dRec.Right=dRec.Bottom  TprTxMemoObjprTxMemoObj9dRec.VersionsVisible	Memo.Strings[qorder.ItemsTotal] hAlignprhRight  	dRec.Left>dRec.Top
dRec.RightOdRec.Bottom   TprTxHDetailHeaderBandprTxHDetailHeaderBand1Height
DetailBand!prTxRecordReport.prTxHDetailBand1 TprTxMemoObjprTxMemoObj10dRec.VersionsVisible	Memo.StringsOrder No hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.RightdRec.Bottom  TprTxMemoObjprTxMemoObj11dRec.VersionsVisible	Memo.Strings	Sale Date hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.RightdRec.Bottom  TprTxMemoObjprTxMemoObj12dRec.VersionsVisible	Memo.Strings	Ship Date hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.Right+dRec.Bottom  TprTxMemoObjprTxMemoObj13dRec.VersionsVisible	Memo.StringsPayment method hAlign	prhCenter  	dRec.Left,dRec.Top
dRec.Right=dRec.Bottom  TprTxMemoObjprTxMemoObj14dRec.VersionsVisible	Memo.StringsItems total hAlign	prhCenter  	dRec.Left>dRec.Top
dRec.RightOdRec.Bottom  TprTxHLineObjprTxHLineObj1dRec.VersionsVisible	  	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom  TprTxHLineObjprTxHLineObj2dRec.VersionsVisible	  	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom   TprTxHPageFooterBandprTxHPageFooterBand1HeightPrintOnFirstPage	PrintAfterLastBandOnPage	 TprTxHLineObjprTxHLineObj3dRec.VersionsVisible	MainChar=StartingChar=
EndingChar=  	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom  TprTxMemoObjprTxMemoObj15dRec.VersionsVisible	Memo.StringsPage [Page] of [PagesCount] hAlignprhRight  	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom  TprTxCommandObjprTxCommandObj1dRec.VersionsVisible	
TxCommandstxcFormFeed   	dRec.LeftdRec.Top
dRec.RightdRec.Bottom      