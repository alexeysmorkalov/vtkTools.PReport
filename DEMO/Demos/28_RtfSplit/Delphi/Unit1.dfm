�
 TFORM1 0:  TPF0TForm1Form1LeftTop� BorderIconsbiSystemMenu
biMinimize BorderStylebsSingleCaptionSplitting of RTF textClientHeight� ClientWidth�Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderOnCreate
FormCreatePixelsPerInch`
TextHeight TLabelLabel1LeftTopWidthyHeightYAutoSizeCaption+  This sample demonstrates spllitting of the RTF text.

The report template has one TprRichObj object, in the OnFirstPassObject
event the selected RTF file will be loaded into this object.
If RTF file can not be fitted in one page then the object will be split
and will be moved on the next page.  TLabelLabel2LeftToptWidth0HeightCaption
File name:  TSpeedButtonSpeedButton1LeftHToppWidth9HeightCaption	Select...OnClickSpeedButton1Click  TButtonbPreviewLeft� Top� WidthKHeightCaption
Preview...TabOrderOnClickbPreviewClick  TButtonButton2Left8Top� WidthKHeightCaption	Design...TabOrderOnClickButton2Click  TEdit
edFileNameLeft@ToppWidth	HeightTabOrder OnChangeedFileNameChange  TOpenDialog
OpenDialog
DefaultExtrtfFilterRTF files (*.rtf)|*.rtfOptionsofHideReadOnlyofPathMustExistofFileMustExistofEnableSizing LeftTop�   	TprReportRtfViewerTemplateCanUserEdit	Values 	VariablesNameFileName	ValueTypeprvvtStringValue   PrinterNameEpson LX-1050+OnFirstPassObject RtfViewerTemplateFirstPassObjectLeft(Top� 
SystemInfo$OS: WIN32_NT 5.1.2600 Service Pack 1 PageSize: 4096ActiveProcessorMask: $1000NumberOfProcessors: 1ProcessorType: 586 Compiler version: Delphi5PReport version: 1.9.7 
LOGPIXELSX`
LOGPIXELSY` TprPageprPage1Width4Height�	PaperSize	Orientation
poPortraitlMargin       �@rMargin       �@tMargin       �@bMargin       �@ TprHTitleBandprHTitleBand1Height+ 
TprMemoObj
prMemoObj1dRec.VersionsVisible	Memo.Strings
File name: lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitevAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.Style FontSize
  	dRec.Left
dRec.Top

dRec.RightcdRec.Bottom#  
TprMemoObj
prMemoObj2dRec.VersionsVisible	Memo.Strings
[FileName] lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width	FillColorclWhitevAlign	prvCenterFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize
  	dRec.LeftjdRec.Top

dRec.Right�dRec.Bottom#   TprHDetailBandprHDetailBand1
ResizeModeprbrmMaxObjHeight`CanSplit	DataSetNameprArrayDataset1 
TprRichObj
prRichObj1dRec.VersionsVisible	lBorder.WidthrBorder.WidthtBorder.WidthbBorder.Width
CanResizeY	RichText
�   �   {\rtf1\ansi\ansicpg1251\deff0\deflang1049{\fonttbl{\f0\fswiss\fprq2\fcharset204 System;}}
{\*\generator Msftedit 5.41.15.1503;}\viewkind4\uc1\pard\b\f0\fs20\par
}
   	dRec.Left
dRec.Top
dRec.Right�dRec.BottomHdRec.CanSplit	   TprHPageFooterBandprHPageFooterBand1HeightPrintOnFirstPage	 
TprMemoObj
prMemoObj3dRec.VersionsVisible	Memo.StringsPage [Page] of [PagesCount] lBorder.WidthrBorder.Show	rBorder.WidthtBorder.Show	tBorder.WidthbBorder.Width	FillColorclWhitehAlignprhRightvAlignprvTopFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameArial
Font.StylefsBold FontSize  	dRec.LeftdRec.Top
dRec.Right�dRec.Bottom     TprArrayDatasetprArrayDataset1LeftHTop�    