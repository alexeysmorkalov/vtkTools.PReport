�
 TFORM1 0e  TPF0TForm1Form1Left� Top� WidthHeight� CaptionRotated reportColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPixelsPerInch`
TextHeight TMemoMemo1LeftTopWidthHeightyBorderStylebsNoneColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold Lines.Strings.You can form reports on the basis of vertical +bands, however, for such report you should +to add on horizontal detail band, which is $linked to a dataset with one record. .Report in this demo prints list of records in #the Customer table by horizontally."Horizontal dataset represented by TprStringsDataset component. 
ParentFontReadOnly	TabOrder   TButtonButton1LeftTop� WidthYHeightCaptionDesign reportTabOrderOnClickButton1Click  TButtonButton2Left� Top� WidthYHeightCaptionPreview reportTabOrderOnClickButton2Click  TMemotempMemoLeftxTop� Width� HeightLines.StringstempMemo TabOrderVisible  TTableTable1DatabaseNameDBDEMOS	TableNamecustomer.dbLeftTop�   TprStringsDatasetprStringsDataset1StringsSourcetempMemoLeft(Top�   	TprReport	prReport1Values 	Variables PrinterNameHP LaserJet 6LPreviewParams.Options PreviewParams.ShowToolbarsprptPreviewCommon LeftHTop� 
SystemInfoOS: WIN32_NT 5.1.2600  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.3 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width�Height4	PaperSize	OrientationpoLandscapelMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHDetailBandprHDetailBand1Height�UseVerticalBands	DataSetNameprStringsDataset1ColCount ColDirectionprcdTopBottomLeftRight 
TprMemoObj
prMemoObj1dRec.DefVersion dRec.VersionsVisible	Memo.StringsCustNo lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor�� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold Rotate90	DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left*dRec.TopB
dRec.RightCdRec.Bottom�Visible  
TprMemoObj
prMemoObj2dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Table1.CustNo] lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlign	prvBottomFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style Rotate90	DeleteEmptyLinesAtEnd	DeleteEmptyLines
CanResizeX	
CanResizeYWordWrap	FontSize
  	dRec.LeftRdRec.TopB
dRec.RightkdRec.Bottom�Visible  
TprMemoObj
prMemoObj3dRec.DefVersion dRec.VersionsVisible	Memo.StringsCompany lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor�� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold Rotate90	DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left*dRec.Topz
dRec.RightCdRec.Bottom;Visible  
TprMemoObj
prMemoObj4dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Table1.Company] lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlign	prvBottomFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style Rotate90	DeleteEmptyLinesAtEnd	DeleteEmptyLines
CanResizeX	
CanResizeYWordWrap	FontSize
  	dRec.LeftRdRec.Topz
dRec.RightkdRec.Bottom;Visible  
TprMemoObj
prMemoObj6dRec.DefVersion dRec.VersionsVisible	Memo.StringsAddress lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor�� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold Rotate90	DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left*dRec.Top

dRec.RightCdRec.BottomsVisible  
TprMemoObj
prMemoObj7dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Table1.Addr1][Table1.Addr2] lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlign	prvBottomFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style Rotate90	DeleteEmptyLinesAtEnd	DeleteEmptyLines
CanResizeX	
CanResizeYWordWrap	FontSize
  	dRec.LeftRdRec.Top

dRec.RightkdRec.BottomsVisible   TprVDetailHeaderBandprVDetailHeaderBand1Width(UseHorizontalBands	
DetailBandprReport1.prVDetailBand1ReprintOnEachPageLinkToDetail  TprVDetailBandprVDetailBand1
ResizeModeprbrmMaxObjWidth(UseHorizontalBands	DataSetNameTable1BandsprVDetailHeaderBand1   TprVTitleBandprVTitleBand1Width(UseHorizontalBands 
TprMemoObj
prMemoObj5dRec.DefVersion dRec.VersionsVisible	Memo.StringsCustomers list lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor�� hAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameTahoma
Font.StylefsBold Rotate90	DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left
dRec.Top� 
dRec.Right#dRec.Bottom�Visible      