�
 TFORM1 0  TPF0TForm1Form1Left� Top� BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaption.Change properties of report objects at runtimeClientHeightClientWidth�Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPositionpoScreenCenterOnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel1LeftTop� Width�Height)AutoSizeCaption�This demo demonstarates how to change properties of PReport objects at runtime. Also this demo demonstrates how to use new properties of objects added in  1.9 version.Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.StylefsBold 
ParentFontWordWrap	  TLabelLabel2LeftTopWidth~HeightCaptionName of object to change:  TLabelLabel4Left/Top-WidthWHeightCaptionBackground color:  TLabelLabel5LeftTTopMWidth2HeightCaptionText color:  TButtonButton1Left� Top� Width� HeightCaptionUse OnFirstPassObject eventTabOrderOnClickButton1Click  TButtonButton2Left� Top� Width� HeightCaption'Change report template before preparingTabOrderOnClickButton2Click  TvgrColorButton
bBackColorLeft� Top(Width-HeightTabOrderSelectedColorclWhite
OtherColorclBlack  TvgrColorButton
bTextColorLeft� TopHWidth-HeightTabOrderSelectedColorclBlack
OtherColorclBlack  	TComboBoxedNameLeft� TopWidthHeight
ItemHeightTabOrder   	TprReport	prReport1Values 	Variables PrinterName%   Виртуальный принтерOnFirstPassObjectprReport1FirstPassObjectPreviewParams.Options PreviewParams.ShowToolbarsprptPreviewCommon LeftTop� 
SystemInfoOS: WIN32_NT 5.1.2600  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi7PReport version: 1.9.3 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1HeighteUseVerticalBands 
TprMemoObj
prMemoObj1dRec.DefVersion dRec.VersionsVisible	Memo.Strings&This demo demonstarates how to change )properties of PReport objects at runtime.+Also this demo demonstrates how to use new ,properties of objects added in  1.9 version. lBorder.Show	lBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.Show	rBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclAquahAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrap	FontSize
  	dRec.Left� dRec.Top
dRec.RightAdRec.BottomYVisible  TprShapeObjprShapeObj1dRec.DefVersion dRec.VersionsVisible	StyleprssEllipsePenWidth
BrushColor	clFuchsia  	dRec.Left
dRec.Top

dRec.RightkdRec.Bottom[Visible  TprShapeObjprShapeObj2dRec.DefVersion dRec.VersionsVisible	StyleprssBoxPenWidth
BrushColor	clFuchsia  	dRec.LeftbdRec.Top

dRec.Right�dRec.Bottom[Visible   TprHDetailBandprHDetailBand1
ResizeModeprbrmMaxObjHeight(UseVerticalBandsDataSetNameTable1ColCount ColDirectionprcdTopBottomLeftRight 
TprMemoObj
prMemoObj2dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Table1.Company] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeY	WordWrap	FontSize
  	dRec.Left
dRec.Top
dRec.Right� dRec.BottomVisible  
TprMemoObj
prMemoObj3dRec.DefVersion dRec.VersionsVisible	Memo.Strings[Table1.Addr1+Table1.Addr2] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhLeftvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeY	WordWrap	FontSize
  	dRec.Left� dRec.Top
dRec.Right�dRec.BottomVisible   TprHSummaryBandprHSummaryBand1Height(UseVerticalBands    TTableTable1Active	DatabaseNameDBDEMOS	TableNamecustomer.dbLeft(Top�    