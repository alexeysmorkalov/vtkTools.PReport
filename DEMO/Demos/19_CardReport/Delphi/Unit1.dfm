�
 TFORM1 0  TPF0TForm1Form1Left� Top� WidthCHeight� CaptionCard reportColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderOnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel1LeftTop<WidthlHeightCaptionRecords per width:Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFont  TLabelLabel2LeftTopTWidthqHeightCaptionRecords per height:Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFont  TMemoMemo1LeftTopWidth!Height!BorderStylebsNoneColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold Lines.Strings-In this demo records of the CUSTOMER.DB tablewill be printed as cards. 
ParentFontReadOnly	TabOrder   TButtonButton1LeftToppWidthYHeightCaptionDesign reportTabOrderOnClickButton1Click  TButtonButton2LeftTop� WidthYHeightCaptionPreview reportTabOrderOnClickButton2Click  TEditEdit1Left� Top8WidthyHeightTabOrderText3OnChangeEdit1Change  TEditEdit2Left� TopPWidthyHeightColor	clBtnFaceReadOnly	TabOrder  TTableTableActive	DatabaseNameDBDEMOS	TableNamecustomer.dbLeftpTop�   	TprReport	prReport1Values 	Variables PrinterNameHP LaserJet 6LOnBandGenerateCellprReport1BandGenerateCellOnInitDetailBandDataSetprReport1InitDetailBandDataSetPreviewParams.Options PreviewParams.ShowToolbarsprptPreviewCommon Left� Top� 
SystemInfoOS: WIN32_NT 5.1.2600  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi5PReport version: 1.9.3 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1Height;UseVerticalBands 
TprMemoObj
prMemoObj1dRec.DefVersion dRec.VersionsVisible	Memo.Strings	Customers lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor�� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left� dRec.Top

dRec.Right�dRec.Bottom3Visible   TprHDetailBandprHDetailBand1HeightHUseVerticalBands	DataSetNameVertDatasetColCount ColDirectionprcdTopBottomLeftRight 
TprMemoObj
prMemoObj2dRec.DefVersion dRec.VersionsVisible	Memo.StringsCustNo:  lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftdRec.Top
dRec.RightKdRec.Bottom Visible  
TprMemoObj
prMemoObj3dRec.DefVersion dRec.VersionsVisible	Memo.Strings	Company:  lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftdRec.Top'
dRec.RightKdRec.Bottom@Visible  
TprMemoObj
prMemoObj4dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Table.CustNo] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftZdRec.Top
dRec.Right� dRec.Bottom Visible  
TprMemoObj
prMemoObj5dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Table.Company] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftZdRec.Top'
dRec.Right� dRec.Bottom@Visible   TprVDetailBandprVDetailBand1
ResizeModeprbrmMaxObjWidth� UseHorizontalBands	DataSetNameHorzDatasetBandsprVDetailHeaderBand1   TprVDetailHeaderBandprVDetailHeaderBand1WidthSUseHorizontalBands	
DetailBandprReport1.prVDetailBand1ReprintOnEachPageLinkToDetail    TprArrayDatasetHorzDatasetLeft� Top�   TprArrayDatasetVertDatasetLeft� Top�    