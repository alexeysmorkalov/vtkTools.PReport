�
 TFORM1 0�?  TPF0TForm1Form1Left� Top� BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaption!Access to the aggregate variablesClientHeightHClientWidthyColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPixelsPerInch`
TextHeight TButtonButton1LeftTop(WidthKHeightCaption
Preview...TabOrder OnClickButton1Click  TMemoMemo2LeftTopWidthiHeight)BorderStylebsNoneColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style Lines.StringsHThis example demostrates how to access to the aggregate variables of the+report during the generating of the report. 
ParentFontReadOnly	TabOrder  TButtonButton2LeftXTop(WidthKHeightCaption	Design...TabOrderOnClickButton2Click  TMemoMemoLeftTop8WidthiHeight� Font.CharsetRUSSIAN_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style 
ParentFont
ScrollBarsssBothTabOrder  	TprReportprReportValuesGroupprReport.GroupNameGroupSumFormulaQuery.ItemsTotalResetOnrvtGroupCalcOncvtDataSetNextDataSetNameQuery Name	ReportSumFormulaQuery.ItemsTotalResetOn	rvtReportCalcOncvtDataSetNextDataSetNameQuery  	Variables PrinterNameFaxOnUnknownVariableprReportUnknownVariableOnBandGenerateCellprReportBandGenerateCellLeft� Top(
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.7 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1Heightc 
TprMemoObj
prMemoObj1dRec.VersionsVisible	Memo.Strings"Access to the aggregate variables. lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclLimehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.Top

dRec.Right�dRec.Bottom#  
TprMemoObj
prMemoObj2dRec.VersionsVisible	Memo.StringsSum by report: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclLimehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.Top*
dRec.Right� dRec.Bottom;  
TprMemoObj
prMemoObj3dRec.VersionsVisible	Memo.Strings[:<c>ReportSum] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclGreenhAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWhiteFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left� dRec.Top*
dRec.Right+dRec.Bottom;  
TprMemoObj
prMemoObj4dRec.VersionsVisible	Memo.StringsSum by report (from event): lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclLimehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.TopB
dRec.Right� dRec.BottomS  
TprMemoObj
prMemoObj5dRec.VersionsVisible	Memo.Strings[:<c>MyReportSum] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclRedhAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWhiteFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left� dRec.TopB
dRec.Right+dRec.BottomS  
TprMemoObjprMemoObj18dRec.VersionsVisible	Memo.Strings�The value of ReportSum variable is available only after first pass, so you can use MyReportSum variable in the report footer only.Use OnUnknownVariable event. lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitevAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style WordWrap	FontSize  	dRec.Left2dRec.Top*
dRec.Right�dRec.Bottom[   TprHDetailBandDetail
ResizeModeprbrmMaxObjHeight DataSetNameQueryGroupsGroup  
TprMemoObjprMemoObj11dRec.VersionsVisible	Memo.Strings[Query.OrderNo] lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left� dRec.Top
dRec.Right� dRec.Bottom  
TprMemoObjprMemoObj12dRec.VersionsVisible	Memo.Strings[:<d>Query.SaleDate] lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left� dRec.Top
dRec.Right;dRec.Bottom  
TprMemoObjprMemoObj13dRec.VersionsVisible	Memo.Strings[:<d>Query.ShipDate] lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftBdRec.Top
dRec.Right�dRec.Bottom  
TprMemoObjprMemoObj14dRec.VersionsVisible	Memo.Strings[Query.PaymentMethod] lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left�dRec.Top
dRec.Right3dRec.Bottom  
TprMemoObjprMemoObj15dRec.VersionsVisible	Memo.Strings[:<c>Query.ItemsTotal] lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left:dRec.Top
dRec.Right�dRec.Bottom   TprHGroupHeaderBandprHGroupHeaderBand1HeightxGroupprReport.GroupColCount ColDirectionprcdTopBottomLeftRightLinkToDetail	StartNewPageReprintOnEachPage 
TprMemoObjprMemoObj16dRec.VersionsVisible	Memo.StringsCompany: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.Style WordWrap	FontSize
  	dRec.Left
dRec.Top
dRec.Right� dRec.Bottom   
TprMemoObjprMemoObj17dRec.VersionsVisible	Memo.Strings[Query.Company] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitevAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.StylefsBold WordWrap	FontSize
  	dRec.Left� dRec.Top
dRec.Right{dRec.Bottom   
TprMemoObjprMemoObj19dRec.VersionsVisible	Memo.StringsSum by group: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclAquahAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.Top'
dRec.Right� dRec.Bottom8  
TprMemoObjprMemoObj20dRec.VersionsVisible	Memo.StringsSum by group (from event): lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclAquahAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.Top?
dRec.Right� dRec.BottomP  
TprMemoObjprMemoObj21dRec.VersionsVisible	Memo.Strings[:<c>MyGroupSum] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclRedhAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWhiteFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left� dRec.Top?
dRec.Right+dRec.BottomP  
TprMemoObjprMemoObj22dRec.VersionsVisible	Memo.Strings[:<c>GroupSum] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclTealhAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWhiteFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left� dRec.Top'
dRec.Right+dRec.Bottom8  
TprMemoObjprMemoObj23dRec.VersionsVisible	Memo.StringsThe value of GroupSum variable is available only after first pass, so you can use MyGroupSum variable in the group footer only.Use OnUnknownVariable event. lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitevAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style WordWrap	FontSize  	dRec.Left2dRec.Top'
dRec.Right�dRec.BottomX  
TprMemoObj
prMemoObj6dRec.VersionsVisible	Memo.StringsOrderNo lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left� dRec.Top_
dRec.Right� dRec.Bottomp  
TprMemoObj
prMemoObj7dRec.VersionsVisible	Memo.Strings	Sale date lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left� dRec.Top_
dRec.Right;dRec.Bottomp  
TprMemoObj
prMemoObj8dRec.VersionsVisible	Memo.Strings	Ship date lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftBdRec.Top_
dRec.Right�dRec.Bottomp  
TprMemoObj
prMemoObj9dRec.VersionsVisible	Memo.StringsPayment method lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left�dRec.Top_
dRec.Right3dRec.Bottomp  
TprMemoObjprMemoObj10dRec.VersionsVisible	Memo.StringsTotal lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left:dRec.Top_
dRec.Right�dRec.Bottomp  
TprMemoObjprMemoObj35dRec.VersionsVisible	Memo.Strings[Query.CustNo] lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize
  	dRec.Left�dRec.Top
dRec.Right�dRec.Bottom    TprHGroupFooterBandGroupFooterHeight8GroupprReport.GroupLinkToDetail	 
TprMemoObjprMemoObj24dRec.VersionsVisible	Memo.StringsSum by group: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclAquahAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left2dRec.Top
dRec.Right�dRec.Bottom  
TprMemoObjprMemoObj25dRec.VersionsVisible	Memo.StringsSum by group (from event): lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclAquahAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left2dRec.Top
dRec.Right�dRec.Bottom0  
TprMemoObjprMemoObj26dRec.VersionsVisible	Memo.Strings[:<c>MyGroupSum] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclTealhAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWhiteFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left�dRec.Top
dRec.RightRdRec.Bottom0  
TprMemoObjprMemoObj27dRec.VersionsVisible	Memo.Strings[:<c>GroupSum] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclTealhAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWhiteFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left�dRec.Top
dRec.RightSdRec.Bottom  
TprMemoObjprMemoObj32dRec.VersionsVisible	Memo.StringsGroup footer: lBorder.WidthrBorder.Show	rBorder.WidthtBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.Style FontSize  	dRec.Left
dRec.Top
dRec.Right+dRec.Bottom0   TprHSummaryBandReportFooterHeight8 
TprMemoObjprMemoObj28dRec.VersionsVisible	Memo.StringsSum by report: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclAquahAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left2dRec.Top
dRec.Right�dRec.Bottom  
TprMemoObjprMemoObj29dRec.VersionsVisible	Memo.StringsSum by report (from event): lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclAquahAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left2dRec.Top
dRec.Right�dRec.Bottom0  
TprMemoObjprMemoObj30dRec.VersionsVisible	Memo.Strings[:<c>MyReportSum] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclTealhAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWhiteFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left�dRec.Top
dRec.RightSdRec.Bottom0  
TprMemoObjprMemoObj31dRec.VersionsVisible	Memo.Strings[:<c>ReportSum] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclTealhAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWhiteFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left�dRec.Top
dRec.RightSdRec.Bottom  
TprMemoObjprMemoObj33dRec.VersionsVisible	Memo.StringsReport footer: lBorder.WidthrBorder.Show	rBorder.WidthtBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.Style FontSize  	dRec.Left
dRec.Top
dRec.Right+dRec.Bottom0   TprHPageFooterBandprHPageFooterBand1
ResizeModeprbrmMaxObjHeight$ 
TprMemoObjprMemoObj34dRec.VersionsVisible	Memo.Strings[Page] / [PagesCount] lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvBottomFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.Top
dRec.Right�dRec.Bottom    TprGroupGroupValidQuery.CustNo
DetailBandprReport.Detail   TQueryQueryActive	DatabaseNameDBDEMOSSQL.StringsUselect c.*, o.* from orders o, customer c where o.CustNo = c.CustNo order by c.CustNo Left� Top(   