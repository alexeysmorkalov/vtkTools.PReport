�
 TFORM1 0�"  TPF0TForm1Form1LeftQTop� Width�Height� CaptionBarCodesColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderOnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel1LeftTopWidth_HeightCaption;This demo demonstrates the using of barcodes in the PReportFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFont  	TGroupBox	GroupBox1LeftTop(Width� Height� Caption Predefined barcodes: TabOrder  TLabelLabel2LeftTopWidthHeightCaptionCode:  TLabelLabel3LeftTop4WidthHeightCaptionType:  TEditedCodeLeft8TopWidth� HeightTabOrder Text12345678  	TComboBoxedTypeDefinedLeft8Top0Width� HeightStylecsDropDownList
ItemHeightTabOrder  TButtonButton1Left� TopPWidthKHeightCaption	Design...TabOrderOnClickButton1Click  TButtonButton2Left� ToppWidthKHeightCaption
Preview...TabOrderOnClickButton2Click   	TGroupBox	GroupBox2Left Top(Width� Height� Caption Barcodes from database: TabOrder TLabelLabel4LeftTopWidthHeightCaptionType:  	TComboBoxedTypeDBLeft8TopWidth� HeightStylecsDropDownList
ItemHeightTabOrder   TButtonButton3Left� TopPWidthKHeightCaption	Design...TabOrderOnClickButton3Click  TButtonButton4Left� ToppWidthKHeightCaption
Preview...TabOrderOnClickButton4Click   	TprReport
RepDefinedCanUserEdit	Values 	Variables PrinterNameVirtual printerPreviewParams.OptionsprpoAllowChangePreviewMode LeftTop� 
SystemInfo#OS: WIN32_WINDOWS 4.10.67766446  A  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi4PReport version: 1.9.7 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1Height� 
TprMemoObj
prMemoObj1dRec.VersionsVisible	Memo.Strings7This report demonstrates the various types of barcodes. lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.Top

dRec.Right�dRec.Bottom3  TprBarCodeObjprBarCodeObj1dRec.VersionsVisible	Text	123456789BarCodeType	bcCodeMSI
ZoomFactor       � @  	dRec.Left
dRec.TopZ
dRec.Right� dRec.Bottom�   
TprMemoObj
prMemoObj2dRec.VersionsVisible	Memo.StringsNon rotated: lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclSilvervAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.Style FontSize  	dRec.Left
dRec.TopB
dRec.Right�dRec.BottomS  TprBarCodeObjprBarCodeObj2dRec.VersionsVisible	Text	123456789BarCodeType	bcCodeMSI
ZoomFactor       � @ShowTextTypebcoCode  	dRec.Left� dRec.TopZ
dRec.Right�dRec.Bottom�   TprBarCodeObjprBarCodeObj3dRec.VersionsVisible	Text	123456789BarCodeType	bcCodeMSI
ZoomFactor       � @ShowTextTypebcoBoth  	dRec.Left�dRec.TopZ
dRec.RightcdRec.Bottom�   TprBarCodeObjprBarCodeObj4dRec.VersionsVisible	Text	123456789BarCodeType	bcCodeMSI
ZoomFactor       � @ShowTextTypebcoTypShowTextOverLines	  	dRec.Left
dRec.Top� 
dRec.Right� dRec.Bottom3  TprBarCodeObjprBarCodeObj5dRec.VersionsVisible	Text	123456789BarCodeType	bcCodeMSI
ZoomFactor       � @ShowTextTypebcoCodeTextFont.CharsetDEFAULT_CHARSETTextFont.ColorclWindowTextTextFont.Height�TextFont.NameCourier NewTextFont.Style 	ForeColorclBlue  	dRec.Left� dRec.Top� 
dRec.Right�dRec.Bottom3  TprBarCodeObjprBarCodeObj6dRec.VersionsVisible	Text	123456789BarCodeType	bcCodeMSI
ZoomFactor       � @ShowTextTypebcoCodeTextPositionstpBottomLeft	BackColorclSilver  	dRec.Left�dRec.Top� 
dRec.RightcdRec.Bottom3  TprBarCodeObjprBarCodeObj7dRec.VersionsVisible	Text	123456789BarCodeType	bcCodeMSI
ZoomFactor       � @Rotationprbr90  	dRec.Left
dRec.Topj
dRec.Right� dRec.Bottom  TprBarCodeObjprBarCodeObj8dRec.VersionsVisible	Text	123456789BarCodeType	bcCodeMSI
ZoomFactor       � @ShowTextTypebcoCodeRotationprbr180  	dRec.Left� dRec.Topj
dRec.Right�dRec.Bottom  TprBarCodeObjprBarCodeObj9dRec.VersionsVisible	Text	123456789BarCodeType	bcCodeMSI
ZoomFactor       � @ShowTextTypebcoBothRotationprbr270  	dRec.Left�dRec.Topj
dRec.RightcdRec.Bottom  TprBarCodeObjprBarCodeObj10dRec.VersionsVisible	Text	123456789BarCodeType	bcCodeMSI
ZoomFactor       � @ShowTextTypebcoTypRotationprbr90ShowTextOverLines	  	dRec.Left
dRec.Top*
dRec.Right� dRec.Bottom�  TprBarCodeObjprBarCodeObj11dRec.VersionsVisible	Text	123456789BarCodeType	bcCodeMSI
ZoomFactor       � @ShowTextTypebcoCodeTextFont.CharsetDEFAULT_CHARSETTextFont.ColorclWindowTextTextFont.Height�TextFont.NameCourier NewTextFont.Style Rotationprbr180	ForeColorclBlue  	dRec.Left� dRec.Top*
dRec.Right�dRec.Bottom�  TprBarCodeObjprBarCodeObj12dRec.VersionsVisible	Text	123456789BarCodeType	bcCodeMSI
ZoomFactor       � @ShowTextTypebcoCodeTextPositionstpBottomLeftRotationprbr270	BackColorclSilver  	dRec.Left�dRec.Top*
dRec.RightcdRec.Bottom�  
TprMemoObj
prMemoObj3dRec.VersionsVisible	Memo.StringsRotated: lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclSilvervAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.Style FontSize  	dRec.Left
dRec.TopJ
dRec.Right�dRec.Bottom[     	TprReportRepDBValues 	Variables PrinterNameVirtual printerLeft8Top� 
SystemInfo#OS: WIN32_WINDOWS 4.10.67766446  A  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi4PReport version: 1.9.7 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1HeightC 
TprMemoObj
prMemoObj1dRec.VersionsVisible	Memo.Strings.This report prints the barcodes from database. lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize  	dRec.Left
dRec.Top

dRec.Right�dRec.Bottom3   TprHDetailBandprHDetailBand1Height� DataSetNameTableColCount 
TprMemoObj
prMemoObj2dRec.VersionsVisible	Memo.StringsName: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclSilvervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.StylefsBold FontSize  	dRec.Left
dRec.Top
dRec.RightkdRec.Bottom   
TprMemoObj
prMemoObj3dRec.VersionsVisible	Memo.Strings[Table.Name] lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitevAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.Style WordWrap	FontSize
  	dRec.Left
dRec.Top'
dRec.RightkdRec.Bottom@  
TprMemoObj
prMemoObj4dRec.VersionsVisible	Memo.StringsCost: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclSilvervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.StylefsBold FontSize  	dRec.Left
dRec.TopG
dRec.RightkdRec.Bottom`  
TprMemoObj
prMemoObj5dRec.VersionsVisible	Memo.Strings[:<c>Table.Cost] lBorder.Show	lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Show	bBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclRedFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.Left
dRec.Topg
dRec.RightkdRec.Bottom�   TprBarCodeObjprBarCodeObj1dRec.VersionsVisible	Text[Table.BarCode]
ZoomFactor       � @ShowTextTypebcoCode  	dRec.LeftzdRec.Top
dRec.Right[dRec.Bottom�      TTableTableLeftXTop�    