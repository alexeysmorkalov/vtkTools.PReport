�
 TFORM1 0n  TPF0TForm1Form1LeftpTop� BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionPrint variablesClientHeight� ClientWidth�Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPixelsPerInch`
TextHeight 	TGroupBox	GroupBox1LeftTop`Width� HeightACaption TprReport: TabOrder  TButtonButton1LeftTopWidthKHeightCaption	Design...TabOrder OnClickButton1Click  TButtonButton2Left`TopWidthKHeightCaption
Preview...TabOrderOnClickButton2Click   	TGroupBox	GroupBox2Left� Top`Width� HeightACaption TprTxReport: TabOrder TButtonButton3LeftTopWidthKHeightCaption	Design...TabOrder OnClickButton3Click  TButtonButton4Left`TopWidthKHeightCaption
Preview...TabOrderOnClickButton4Click   TMemoMemo1LeftTopWidth�HeightIBorderStylebsNoneColor	clBtnFaceLines.StringsDThis demo demonstrates how to use the printing variables in PReport.QThese variables are calculated in the time of report printing and can be used for+output of the various printing information:'  - the number of current printing copy  - the name of printer  - and so on.  TabOrder  TTable	customersActive	DatabaseNameDBDEMOS	TableNamecustomer.dbLeft� Top8  	TprReport	prReport1Values 	Variables PrinterNameFinePrint 2000OnFoundPrintVariableprReport1FoundPrintVariableLeftTop8
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.9 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1Heightc 
TprMemoObj
prMemoObj1dRec.VersionsVisible	Memo.StringsCopy: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.Top

dRec.RightCdRec.Bottom  
TprMemoObj
prMemoObj2dRec.VersionsVisible	Memo.StringsPrinter: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.Top"
dRec.RightCdRec.Bottom3  
TprMemoObj
prMemoObj3dRec.VersionsVisible	Memo.Strings@@COPY@@ / @@COPIES@@ lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitevAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftJdRec.Top

dRec.Right� dRec.Bottom  
TprMemoObj
prMemoObj4dRec.VersionsVisible	Memo.Strings@@PRINTER@@ lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitevAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftJdRec.Top"
dRec.Right� dRec.Bottom3  
TprMemoObj
prMemoObj5dRec.VersionsVisible	Memo.StringsCustomers' list lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColor��� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTimes New Roman
Font.StylefsBold FontSize  	dRec.Left
dRec.Top:
dRec.Right�dRec.Bottom[  
TprMemoObj
prMemoObj9dRec.VersionsVisible	Memo.StringsPrintTime: @@PT@@GenerateTime: [StartDateTime] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlignprhRightvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTimes New Roman
Font.Style FontSize  	dRec.Left�dRec.Top

dRec.Right�dRec.Bottom+   TprHDetailBandprHDetailBand1Height(DataSetName	customers 
TprMemoObj
prMemoObj7dRec.VersionsVisible	Memo.Strings[customers.CustNo] lBorder.WidthrBorder.Show	rBorder.WidthtBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize
  	dRec.Left
dRec.Top
dRec.RightkdRec.Bottom   
TprMemoObj
prMemoObj8dRec.VersionsVisible	Memo.Strings[customers.Company] lBorder.WidthrBorder.Show	rBorder.WidthtBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitevAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize
  	dRec.LeftrdRec.Top
dRec.Right�dRec.Bottom    TprHPageHeaderBandprHPageHeaderBand1Height PrintOnFirstPage	 
TprMemoObj
prMemoObj6dRec.VersionsVisible	Memo.StringsPage [Page] of [PagesCount] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitehAlignprhRightvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftdRec.Top
dRec.Right�dRec.Bottom     TprTxReportprTxReport1CopiesValues 	Variables OnFoundPrintVariableprReport1FoundPrintVariableLeft(Top8
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.9  	TprTxPage	prTxPage1 TprTxHTitleBandprTxHTitleBand1Height TprTxMemoObjprTxMemoObj1dRec.VersionsVisible	Memo.StringsCustomers' list hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.Right:dRec.Bottom  TprTxMemoObjprTxMemoObj2dRec.VersionsVisible	Memo.StringsCopy   : @@COPY@@Printer: @@PRINTER@@   	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom  TprTxMemoObjprTxMemoObj8dRec.VersionsVisible	Memo.Strings/PrintTime: @@PT@@ GenerateTime: [StartDateTime]   	dRec.LeftdRec.Top 
dRec.RightOdRec.Bottom   TprTxHDetailBandprTxHDetailBand2HeightDataSetName	customers TprTxMemoObjprTxMemoObj5dRec.VersionsVisible	Memo.Strings[customers.CustNo] hAlignprhRight  	dRec.LeftdRec.Top
dRec.RightdRec.Bottom  TprTxMemoObjprTxMemoObj6dRec.VersionsVisible	Memo.Strings[customers.Company]   	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom  TprTxHLineObjprTxHLineObj1dRec.VersionsVisible	  	dRec.Left dRec.Top
dRec.RightPdRec.Bottom   TprTxHPageHeaderBandprTxHPageHeaderBand1HeightPrintOnFirstPage	 TprTxHLineObjprTxHLineObj2dRec.VersionsVisible	MainChar=StartingChar=
EndingChar=  	dRec.Left dRec.Top
dRec.RightPdRec.Bottom  TprTxMemoObjprTxMemoObj7dRec.VersionsVisible	Memo.StringsPage [Page] of [PagesCount] hAlignprhRight  	dRec.Left dRec.Top
dRec.RightPdRec.Bottom  TprTxMemoObjprTxMemoObj3dRec.VersionsVisible	Memo.StringsCustNo hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.RightdRec.Bottom  TprTxMemoObjprTxMemoObj4dRec.VersionsVisible	Memo.StringsCompany hAlign	prhCenter  	dRec.LeftdRec.Top
dRec.RightOdRec.Bottom  TprTxHLineObjprTxHLineObj3dRec.VersionsVisible	MainChar=StartingChar=
EndingChar=  	dRec.Left dRec.Top
dRec.RightPdRec.Bottom      