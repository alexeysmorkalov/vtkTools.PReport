�
 TFORM1 0�0  TPF0TForm1Form1Left� TopgWidth� HeightrCaptionAdvanced CrossTabColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderOnCreate
FormCreatePixelsPerInch`
TextHeight TButtonButton1Left(Top0Width� HeightCaptionGenerate reportTabOrder OnClickButton1Click  TQueryDebSQL.Strings+select distinct dks from spost order by dks LeftTop  TQueryKrdSQL.Strings+select distinct kks from spost order by kks Left(Top  TTableTable1	TableName	SPOST.DBFLeftHTop  	TprReport	prReport1ShowProgress	CanUserEditDesignerFormModefmNormalPreviewFormModefmNormalCollateCopiesFromPage�ToPage�PrintPagesModeppmAllExportOptionspreoShowParamsDlgpreoShowProgresspreoShowAfterGenerate ExportPagesModeppmAllExportFromPage ExportToPage ValuesGroupprReport1.KrdGroupNameitogoKrdGroupAggFunctionprafSumFormula:iif(Table1.Locate("DKS",Deb.dks,"KKS",Krd.kks),Table1.S,0)ResetOnrvtGroupCalcOncvtDataSetNextDataSetNameKrd GroupprReport1.DebGroupNameitogoDebGroupAggFunctionprafSumFormula:iif(Table1.Locate("DKS",Deb.dks,"KKS",Krd.kks),Table1.S,0)ResetOnrvtGroupCalcOncvtCrossTabDataSetNameDebCrossTabHorzDataSetNameKrd NameitogoDebAccountAggFunctionprafSumFormula:iif(Table1.Locate("DKS",Deb.dks,"KKS",Krd.kks),Table1.S,0)ResetOnrvtDataSetEofCalcOncvtDataSetNextDataSetNameKrdResetDataSetNameKrd NameitogoKrdAccountAggFunctionprafSumFormula:iif(Table1.Locate("DKS",Deb.dks,"KKS",Krd.kks),Table1.S,0)ResetOnrvtDataSetEofCalcOncvtCrossTabDataSetNameDebResetDataSetNameDebCrossTabHorzDataSetNameKrd  	Variables PrinterNameVirtual printerExportPrecisionExportPrecisionLow
ExportPrecisionNormalExportPrecisionHighPreviewParams.Options PreviewParams.ShowToolbarsprptPreviewCommon LefthTop
SystemInfoOS: WIN32_NT 5.1.2600  PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi5PReport version: 1.80 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMarginrMargintMarginbMargin TprHTitleBandprHTitleBand1Height(UseVerticalBands 
TprMemoObj
prMemoObj1dRec.DefVersion dRec.VersionsVisible	Memo.StringsAdvanced CrossTab demo lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.Show	tBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.Show	bBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlign	prhCentervAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize  	dRec.Left� dRec.Top
dRec.RightdRec.Bottom#Visible   TprHDetailBandprHDetailBand1HeightUseVerticalBands	DataSetNameDebColCount ColDirectionprcdTopBottomLeftRightGroupsDebGroup BandsprHDetailHeaderBand1prHDetailFooterBand1  
TprMemoObj
prMemoObj3dRec.DefVersion dRec.VersionsVisible	Memo.Strings	[Deb.dks] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclLimehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftdRec.Top
dRec.RightbdRec.BottomVisible  
TprMemoObj
prMemoObj5dRec.DefVersion dRec.VersionsVisible	Memo.Strings@[:<c>iif(Table1.Locate("DKS",Deb.dks,"KKS",Krd.kks),Table1.S,0)] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclWhitehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftfdRec.Top
dRec.Right� dRec.BottomVisible  
TprMemoObjprMemoObj10dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<c>itogoKrdGroup] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left� dRec.Top
dRec.Right)dRec.BottomVisible  
TprMemoObjprMemoObj12dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<c>itogoDebAccount] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorU�� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left.dRec.Top
dRec.Right�dRec.BottomVisible   TprVDetailBandprVDetailBand1WidthdUseHorizontalBands	DataSetNameKrdGroupsKrdGroup BandsprVDetailHeaderBand1prVDetailFooterBand1   TprHDetailHeaderBandprHDetailHeaderBand1HeightUseVerticalBands	
DetailBandprReport1.prHDetailBand1ColCount ColDirectionprcdTopBottomLeftRightReprintOnEachPageLinkToDetail 
TprMemoObj
prMemoObj2dRec.DefVersion dRec.VersionsVisible	Memo.Strings	[Krd.kks] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclRedhAlignprhLeftvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftfdRec.Top
dRec.Right� dRec.BottomVisible  
TprMemoObj
prMemoObj6dRec.DefVersion dRec.VersionsVisible	Memo.StringsSub deb. itogo lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclRedhAlignprhLeftvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left� dRec.Top
dRec.Right)dRec.BottomVisible  
TprMemoObj
prMemoObj7dRec.DefVersion dRec.VersionsVisible	Memo.Strings	Deb itogo lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclRedhAlignprhLeftvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.Left.dRec.Top
dRec.Right�dRec.BottomVisible   TprVDetailHeaderBandprVDetailHeaderBand1WidthdUseHorizontalBands	
DetailBandprReport1.prVDetailBand1ReprintOnEachPageLinkToDetail  TprHGroupFooterBandprHGroupFooterBand1HeightUseVerticalBands	GroupprReport1.DebGroupColCount ColDirectionprcdTopBottomLeftRightLinkToDetail 
TprMemoObj
prMemoObj8dRec.DefVersion dRec.VersionsVisible	Memo.StringsSub krd. itogo lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclLimehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftdRec.Top
dRec.RightbdRec.BottomVisible  
TprMemoObjprMemoObj11dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<c>itogoDebGroup] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColor��� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.Style Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftfdRec.Top
dRec.Right� dRec.BottomVisible   TprVGroupFooterBandprVGroupFooterBand1WidthdUseHorizontalBands	GroupprReport1.KrdGroupLinkToDetail  TprHDetailFooterBandprHDetailFooterBand1HeightUseVerticalBands	
DetailBandprReport1.prHDetailBand1ColCount ColDirectionprcdTopBottomLeftRightLinkToDetail 
TprMemoObj
prMemoObj9dRec.DefVersion dRec.VersionsVisible	Memo.Strings
Krd. itogo lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorclLimehAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftdRec.Top
dRec.RightbdRec.BottomVisible  
TprMemoObjprMemoObj13dRec.DefVersion dRec.VersionsVisible	Memo.Strings[:<c>itogoKrdAccount] lBorder.ShowlBorder.StylepsSolidlBorder.ColorclBlacklBorder.WidthrBorder.ShowrBorder.StylepsSolidrBorder.ColorclBlackrBorder.WidthtBorder.ShowtBorder.StylepsSolidtBorder.ColorclBlacktBorder.WidthbBorder.ShowbBorder.StylepsSolidbBorder.ColorclBlackbBorder.Width	FillColorU�� hAlignprhRightvAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.Name	Arial Cyr
Font.StylefsBold Rotate90DeleteEmptyLinesAtEndDeleteEmptyLines
CanResizeX
CanResizeYWordWrapFontSize
  	dRec.LeftfdRec.Top
dRec.Right� dRec.BottomVisible   TprVDetailFooterBandprVDetailFooterBand1WidthdUseHorizontalBands	
DetailBandprReport1.prVDetailBand1LinkToDetail   TprGroupDebGroupValidCopy(Deb.dks,1,1)
DetailBandprReport1.prHDetailBand1  TprGroupKrdGroupValidCopy(Krd.kks,1,1)
DetailBandprReport1.prVDetailBand1    