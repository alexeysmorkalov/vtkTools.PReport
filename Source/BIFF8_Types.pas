{******************************************}
{                                          }
{           vtk Export library             }
{                                          }
{      Copyright (c) 2002 by vtkTools      }
{                                          }
{******************************************}

unit BIFF8_Types;

{$include vtk.inc}

interface

uses
  Windows;

const
  XLSMaxRowsInSheet = 65536;
  XLSMaxRowsInBlock = 32;
  XLSMaxCellsInRow = 256;
  XLSMaxBlocks = XLSMaxRowsInSheet div XLSMaxRowsInBlock;
  XLSMaxColorsInPalette = 56;

  b8_IMDATA = $007F;
  b8_OBJ = $005D;
  b8_EOF = $000A;
  b8_BOF = $0809;
  b8_COLINFO = $007D;
  b8_XF = $00E0;
  b8_LABEL = $0204;
  b8_BLANK = $0201;
  b8_DIMENSIONS = $0200;
  b8_ROW = $0208;
  b8_INTERFACHDR = $00E1;
  b8_INTERFACEND = $00E2;
  b8_MMS = $00C1;
  b8_CODEPAGE = $0042;
  b8_TABID = $013D;
  b8_FNGROUPCOUNT = $009C;
  b8_WINDOWPROTECT = $0019;
  b8_PROTECT = $0012;
  b8_PASSWORD = $0013;
  b8_WINDOW1 = $003D;
  b8_BACKUP = $0040;
  b8_HIDEOBJ = $008D;
  b8_1904 = $0022;
  b8_PRECISION = $000E;
  b8_BOOKBOOL = $00DA;
  b8_FONT = $0031; // $0231 - in MSDN
  b8_FORMAT = $041E;
  b8_COUNTRY = $008C;
  b8_INDEX = $020B;
  b8_CALCMODE = $000D;
  b8_CALCCOUNT = $000C;
  b8_REFMODE = $000F;
  b8_ITERATION = $0011;
  b8_SAVERECALC = $005F;
  b8_DELTA = $0010;
  b8_PRINTHEADERS = $002A;
  b8_PRINTGRIDLINES = $002B;
  b8_GRIDSET = $0082;
  b8_GUTS = $0080;
  b8_DEFAULTROWHEIGHT = $0225;
  b8_WSBOOL = $0081;
  b8_HEADER = $0014;
  b8_FOOTER = $0015;
  b8_HCENTER = $0083;
  b8_VCENTER = $0084;
  b8_DEFCOLWIDTH = $0055;
  b8_WRITEACCESS = $005C;
  b8_DOUBLESTREAMFILE = $0161;
  b8_PROT4REV = $01AF;
  b8_PROT4REVPASS = $01BC;
  b8_REFRESHALL = $01B7;
  b8_USESELFS = $0160;
  b8_BOUNDSHEET = $0085;
  b8_WINDOW2 = $023E;
  b8_SELECTION = $001D;
  b8_DBCELL = $00D7;
  b8_MULBLANK = $00BE;
  b8_FORMULA = $0006;
  b8_MERGE = $00E5; // see in biffview
  b8_PALETTE = $0092;
  b8_CONTINUE = $003C;
  b8_SETUP = $00A1;
  b8_SST = $00FC;
  b8_EXTSST = $00FF;
  b8_LABELSST = $00FD;
  b8_NUMBER = $0203;
  b8_MSODRAWING = $00EC;
  b8_MSODRAWINGGROUP = $00EB;
  b8_SUPBOOK = $01AE;
  b8_EXTERNSHET = $0017;

  b8_OBJ_OT_PictureObject = $0008;
  b8_OBJ_grbit_fSel = $0001;
  b8_OBJ_grbit_fAutoSize = $0002;
  b8_OBJ_grbit_fMove = $0004;
  b8_OBJ_grbit_reserved1 = $0008;
  b8_OBJ_grbit_fLocked = $0010;
  b8_OBJ_grbit_reserved2 = $0020;
  b8_OBJ_grbit_reserved3 = $0040;
  b8_OBJ_grbit_fGrouped = $0080;
  b8_OBJ_grbit_fHidden = $0100;
  b8_OBJ_grbit_fVisible = $0200;
  b8_OBJ_grbit_fPrint = $0400;

  b8_OBJPICTURE_grbit_fAutoPict = $0001;
  b8_OBJPICTURE_grbit_fDde = $0002;
  b8_OBJPICTURE_grbit_fIcon = $0004;

  b8_HORIZONTALPAGEBREAKS = $001B;






  b8_BOF_vers = $0600;

  b8_BOF_dt_WorkbookGlobals = $0005;
  b8_BOF_dt_VisualBasicModule = $0006;
  b8_BOF_dt_Worksheet = $0010;
  b8_BOF_dt_Chart = $0020;
  b8_BOF_dt_MacroSheet = $0040;
  b8_BOF_dt_WorkspaceFile = $0100;

  b8_BOF_rupBuild_Excel97 = $0DBB;

  b8_BOF_rupYear_Excel07 = $07CC;

  b8_XF_Opt1_fLocked = $0001;
  b8_XF_Opt1_fHidden = $0002;
  b8_XF_Opt1_fStyleXF = $0004;
  b8_XF_Opt1_f123Prefix = $0008;
  b8_XF_Opt1_ixfParent = $FFF0;

  b8_XF_Opt2_alcGeneral = $0000;
  b8_XF_Opt2_alcLeft = $0001;
  b8_XF_Opt2_alcCenter = $0002;
  b8_XF_Opt2_alcRight = $0003;
  b8_XF_Opt2_alcFill = $0004;
  b8_XF_Opt2_alcJustify = $0005;
  b8_XF_Opt2_alcCenterAcrossSelection = $0006;

  b8_XF_Opt2_fWrap = $0008;
  
  b8_XF_Opt2_alcVTop = $0000;
  b8_XF_Opt2_alcVCenter = $0010;
  b8_XF_Opt2_alcVBottom = $0020;
  b8_XF_Opt2_alcVJustify = $0030;

  b8_XF_Opt3_fMergeCell = $0020;
  b8_XF_Opt3_fAtrNum = $0400;
  b8_XF_Opt3_fAtrFnt = $0800;
  b8_XF_Opt3_fAtrAlc = $1000;
  b8_XF_Opt3_fAtrBdr = $2000;
  b8_XF_Opt3_fAtrPat = $4000;
  b8_XF_Opt3_fAtrProt = $8000;

  b8_XF_Border_None = $0000;
  b8_XF_Border_Thin = $0001;
  b8_XF_Border_Medium = $0002;
  b8_XF_Border_Dashed = $0003;
  b8_XF_Border_Dotted = $0004;
  b8_XF_Border_Thick = $0005;
  b8_XF_Border_Double = $0006;
  b8_XF_Border_Hair = $0007;
  b8_XF_Border_MediumDashed = $0008;
  b8_XF_Border_DashDot = $0009;
  b8_XF_Border_MediumDashDot = $000A;
  b8_XF_Border_DashDotDot = $000B;
  b8_XF_Border_MediumDashDotDot = $000C;
  b8_XF_Border_SlantedDashDot = $000D;

  b8_INTERFACHDR_cv_IBMPC = $01B5;
  b8_INTERFACHDR_cv_Macintosh = $8000;
  b8_INTERFACHDR_cv_ANSI = $04E4;

  b8_CODEPAGE_cv_IBMPC = $01B5;
  b8_CODEPAGE_cv_Macintosh = $8000;
  b8_CODEPAGE_cv_ANSI = $04E4;

  b8_WINDOW1_grbit_fHidden = $0001;
  b8_WINDOW1_grbit_fIconic = $0002;
  b8_WINDOW1_grbit_fDspHScroll = $0008;
  b8_WINDOW1_grbit_fDspVScroll = $0010;
  b8_WINDOW1_grbit_fBotAdornment = $0020;

  b8_FONT_grbit_fItalic = $0002;
  b8_FONT_grbit_fStrikeout = $0008;
  b8_FONT_grbit_fOutline = $0010;
  b8_FONT_grbit_fShadow = $0020;

  b8_DEFAULTROWHEIGHT_fUnsynced = $0001;
  b8_DEFAULTROWHEIGHT_fDyZero = $0002;
  b8_DEFAULTROWHEIGHT_fExAsc = $0004;
  b8_DEFAULTROWHEIGHT_fExDsc = $0008;

  b8_WSBOOL_fShowAutoBreaks = $0001;
  b8_WSBOOL_fDialog = $0010;
  b8_WSBOOL_fApplyStyles = $0020;
  b8_WSBOOL_fRwSumsBelow = $0040;
  b8_WSBOOL_fColSumsRight = $0080;
  b8_WSBOOL_fFitToPage = $0100;
  b8_WSBOOL_fDspGuts = $0200;
  b8_WSBOOL_fAee = $0400;
  b8_WSBOOL_fAfe = $8000;

  b8_WINDOW1_fHidden = $0001;
  b8_WINDOW1_fIconic = $0002;
  b8_WINDOW1_fDspHScroll = $0008;
  b8_WINDOW1_fDspVScroll = $0010;
  b8_WINDOW1_fBotAdornment = $0020;


  b8_WINDOW2_grbit_fDspFmla = $0001;
  b8_WINDOW2_grbit_fDspGrid = $0002;
  b8_WINDOW2_grbit_fDspRwCol = $0004;
  b8_WINDOW2_grbit_fFrozen = $0008;
  b8_WINDOW2_grbit_fDspZeros = $0010;
  b8_WINDOW2_grbit_fDefaultHdr = $0020;
  b8_WINDOW2_grbit_fArabic = $0040;
  b8_WINDOW2_grbit_fDspGuts = $0080;
  b8_WINDOW2_grbit_fFrozenNoSplit = $0100;
  b8_WINDOW2_grbit_fSelected = $0200;
  b8_WINDOW2_grbit_fPaged = $0400;
  b8_WINDOW2_grbit_fSLV = $0800;

  b8_ROW_grbit_fCollapsed = $0010;
  b8_ROW_grbit_fDyZero = $0020;
  b8_ROW_grbit_fUnsynced = $0040;
  b8_ROW_grbit_fGhostDirty = $0080;
  b8_ROW_grbit_mask_iOutLevel = $0007;

  b8_COLINFO_fHidden = $0001;
  b8_COLINFO_fCollapsed = $1000;

  b8_SETUP_fLeftToRight = $0001;
  b8_SETUP_fLandscape = $0002;
  b8_SETUP_fNoPls = $0004;
  b8_SETUP_fNoColor = $0008;
  b8_SETUP_fDraft = $0010;
  b8_SETUP_fNotes = $0020;
  b8_SETUP_fNoOrient = $0040;
  b8_SETUP_fUsePage = $0080;

  b8_LEFTMARGIN = $0026;
  b8_RIGHTMARGIN = $0027;
  b8_TOPMARGIN = $0028;
  b8_BOTTOMMARGIN = $0029;

  /////////////////////////////////////////////////
  //
  // structs for MSODRAWING
  //
  /////////////////////////////////////////////////
  b8_MSOFBH_inst_mask = $FFF0;
  b8_MSOFBH_ver_mask = $000F;
  
  b8_msoContainerVer = $000F;
  b8_msofbtBSEVer = $0002;
  
  b8_msofbtDggContainer = $F000;
  b8_msofbtDgg = $F006;
  b8_msofbtCLSID = $F016;
  b8_msofbtOPT = $F00B;
  b8_msofbtColorMRU = $F11A;
  b8_msofbtSplitMenuColors = $F11E;
  b8_msofbtBstoreContainer = $F001;
  b8_msofbtBSE = $F007;
  b8_msofbtBlip = $F018;
  b8_msofbtDgContainer = $F002;
  b8_msofbtDg = $F008;
  b8_msofbtRegroupItems = $F118;
  b8_msofbtColorScheme = $F120;
  b8_msofbtSpgrContainer = $F003;
  b8_msofbtSpContainer = $F004;
  b8_msofbtSpgr = $F009;
  b8_msofbtSp = $F00A;
  b8_msofbtTextbox = $F00C;
  b8_msofbtClientTextbox = $F00D;
  b8_msofbtAnchor = $F00E;
  b8_msofbtChildAnchor = $F00F;
  b8_msofbtClientAnchor = $F010;
  b8_msofbtClientData = $F011;
  b8_msofbtOleObject = $F11F;
  b8_msofbtDeletedPspl = $F11D;
  b8_msofbtSolverContainer = $F005;
  b8_msofbtConnectorRule = $F012;
  b8_msomsofbtAlignRule = $F013;
  b8_msofbtArcRule = $F014;
  b8_msofbtClientRule = $F015;
  b8_msofbtCalloutRule = $F017;
  b8_msofbtSelection = $F119;
  
  b8_msoblipERROR = 0;          // An error occured during loading
  b8_msoblipUNKNOWN = 1;        // An unknown blip type
  b8_msoblipEMF = 2;            // Windows Enhanced Metafile
  b8_msoblipWMF = 3;            // Windows Metafile
  b8_msoblipPICT = 4;           // Macintosh PICT
  b8_msoblipJPEG = 5;           // JFIF
  b8_msoblipPNG = 6;            // PNG
  b8_msoblipDIB = 7;            // Windows DIB
  b8_msoblipFirstClient = 32;   // First client defined blip type
  b8_msoblipLastClient =33;
  
  b8_fsp_fGroup = $00000001;
  b8_fsp_fChild = $00000002;
  b8_fsp_fPatriarch = $00000004;
  b8_fsp_fDeleted = $00000008;
  b8_fsp_fOleShape = $00000010;
  b8_fsp_fHaveMaster = $00000020;
  b8_fsp_fFlipH = $00000040;
  b8_fsp_fFlipV = $00000080;
  b8_fsp_fConnector = $00000100;
  b8_fsp_fHaveAnchor = $00000200;
  b8_fsp_fBackground = $00000400;
  b8_fsp_fHaveSpt = $00000800;
  b8_fsp_reserved = $FFFFF000;

  b8_FORMULA_fAlwaysCalc = $0001;
  b8_FORMULA_fCalcOnLoad = $0002;
  b8_FORMULA_fShrFmla = $0008;

type

rb8BOF = packed record
  vers : word;
  dt : word;
  rupBuild : word;
  rupYear : word;
  bfh : cardinal;
  sfo : cardinal;
end;

rb8COLINFO = packed record
  colFirst : word;
  colLast : word;
  coldx : word;
  ixfe : word;
  grbit : word;
  res1 : byte;
end;

rb8XF = packed record
  ifnt : word;
  ifmt : word;
  Opt1 : word;
  Opt2 : byte;
  trot : byte;
  Opt3 : word;
  Borders1 : word;
{ Line style for borders
    dgLeft : 3..0
    dgRight : 7..4
    dgTop : 11..8
    dgBottom : 15..12 }
  Borders2 : word;
{
    icvLeft : 6..0
    icvRight : 13..7
    grbitDiag : 15..14}
  Borders3 : cardinal;
{
    icvTop : 6..0
    icvBottom : 13..7
    icvDiag : 20..14
    dgDiag : 24..21
    Reserved : 25
    fls : 31..26}
  Colors : word;
{
    icvFore : 6..0
    icvBack : 13..7
    fSxButton : 14
    Reserved : 15}
end;
pb8XF = ^rb8XF;

rb8DIMENSIONS = packed record
  rwMic : cardinal;
  rwMac : cardinal;
  colMic : word;
  colMac : word;
  Res1 : word;
end;

rb8ROW = packed record
  rw : word;
  colMic : word;
  colMac : word;
  miyRw : word;
  irwMac : word;
  Res1 : word;
  grbit : word;
  ixfe : word;
end;

rb8INTERFACHDR = packed record
  cv : word;
end;

rb8MMS = packed record
  caitm : byte;
  cditm : byte;
end;

rb8CODEPAGE = packed record
  cv : word;
end;

rb8FNGROUPCOUNT = packed record
  cFnGroup : word;
end;

rb8WINDOWPROTECT = packed record
  fLockWn : word;
end;

rb8PROTECT = packed record
  fLock : word;
end;

rb8PASSWORD = packed record
  wPassword : word;
end;

rb8BACKUP = packed record
  fBackupFile : word;
end;

rb8HIDEOBJ = packed record
  fHideObj : word;
end;

rb81904 = packed record
  f1904 : word;
end;

rb8PRECISION = packed record
  fFullPrec : word;
end;

rb8BOOKBOOL = packed record
  fNoSaveSupp : word;
end;

rb8FONT = packed record
  dyHeight : word;
  grbit : word;
  icv : word;
  bls : word;
  sss : word;
  uls : byte;
  bFamily : byte;
  bCharSet : byte;
  Res1 : byte;
  cch : byte; // or word ???
  cchgrbit : byte;
end;
pb8FONT = ^rb8FONT;

rb8FORMAT = packed record
  ifmt : word;
  cch : word;
  cchgrbit : byte;
end;
pb8FORMAT = ^rb8FORMAT;

rb8COUNTRY = packed record
  iCountryDef : word;
  iCountryWinIni : word;
end;

rb8INDEX = packed record
  Res1 : cardinal;
  rwMic : cardinal;
  rwMac : cardinal;
  Res2 : cardinal;
end;
pb8INDEX = ^rb8INDEX;

rb8CALCMODE = packed record
  fAutoRecalc : word;
end;

rb8CALCCOUNT = packed record
  cIter : word;
end;

rb8REFMODE = packed record
  fRefA1 : word;
end;

rb8ITERATION = packed record
  fIter : word;
end;

rb8DELTA = packed record
  numDelta : int64; // ??? must be IEEE floating-point 
end;

rb8SAVERECALC = packed record
  fSaveRecalc : word;
end;

rb8PRINTHEADERS = packed record
  fPrintRwCol : word;
end;

rb8PRINTGRIDLINES = packed record
  fPrintGrid : word;
end;

rb8GRIDSET = packed record
  fGridSet : word;
end;

rb8GUTS = packed record
  dxRwGut : word;
  dyColGut : word;
  iLevelRwMac : word;
  iLevelColMac : word;
end;

rb8DEFAULTROWHEIGHT = packed record
  grbit : word;
  miyRw : word;
end;

rb8WSBOOL = packed record
  grbit : word;
end;

rb8HEADER = packed record
  cch : word;
  cchgrbit : byte;
end;
pb8HEADER = ^rb8HEADER;

rb8FOOTER = packed record
  cch : word;
  cchgrbit : byte;
end;
pb8FOOTER = ^rb8FOOTER;

rb8HCENTER = packed record
  fHCenter : word;
end;

rb8VCENTER = packed record
  fVCenter : word;
end;

rb8DEFCOLWIDTH = packed record
  cchdefColWidth : word;
end;

rb8WRITEACCESS = packed record
  stName : array [0..111] of byte; // User name as an unformatted unicode string
end;

rb8DOUBLESTREAMFILE = packed record
  fDSF : word;
end;

rb8PROT4REV = packed record
  fRevLock : word;
end;

rb8PROT4REVPASS = packed record
  wRevPass : word;
end;

rb8WINDOW1 = packed record
  xWn : word;
  yWn : word;
  dxWn : word;
  dyWn : word;
  grbit : word;
  itabCur : word;
  itabFirst : word;
  ctabSel : word;
  wTabRatio : word;
end;

rb8REFRESHALL = packed record
  fRefreshAll : word;
end;

rb8USESELFS = packed record
  fUsesElfs : word;
end;

rb8PALETTE = packed record
  ccv : word;
  colors : array [0..XLSMaxColorsInPalette-1] of cardinal;
end;
pb8PALETTE = ^rb8PALETTE;

rb8BOUNDSHEET = packed record
  lbPlyPos : cardinal;
  grbit : word;
  cch : byte;
  cchgrbit : byte;
end;
pb8BOUNDSHEET = ^rb8BOUNDSHEET;

rb8WINDOW2 = packed record
  grbit : word;
  rwTop : word;
  colLeft : word;
  icvHdr : cardinal;
  wScaleSLV : word;
  wScaleNormal : word;
  Res1 : cardinal;
end;

rb8SELECTION = packed record
  pnn : byte;
  rwAct : word;
  colAct : word;
  irefAct :word;
  cref : word;
end;
pb8SELECTION = ^rb8SELECTION;

rb8DBCELL = packed record
  dbRtrw : cardinal;
end;

Tb8DBCELLCellsOffsArray = array [0..XLSMaxCellsInRow-1] of word;
rb8DBCELLfull = packed record
  dbRtrw : cardinal;
  cellsOffs : Tb8DBCELLCellsOffsArray;
end;

rb8MERGErec = packed record
  top : word;
  bottom : word;
  left : word;
  right : word;
end;
pb8MERGErec = ^rb8MERGErec;

rb8MERGE = packed record
  cnt : word;  // next - array of merge structs top,bottom,left,right
end;
pb8MERGE = ^rb8MERGE;

rb8LABEL = packed record
  rw : word;
  col : word;
  ixfe : word;
  cch : word;
  cchgrbit : byte;
end;
pb8LABEL = ^rb8LABEL;

rb8BLANK = packed record
  rw : word;
  col : word;
  ixfe : word;
end;

rb8MULBLANK = packed record
  rw : word;
  colFirst : word;
//  colLast : word;
end;
pb8MULBLANK = ^rb8MULBLANK;

rb8SETUP = packed record
  iPaperSize : word;
  iScale : word;
  iPageStart : word;
  iFitWidth : word;
  iFitHeight : word;
  grbit : word;
  iRes : word;
  iVRes : word;
  numHdr : double;
  numFtr : double;
  iCopies : word;
end;

rb8SST = packed record
  cstTotal : cardinal;
  cstUnique : cardinal;
end;
pb8SST = ^rb8SST;

rb8EXTSST = packed record
  Dsst : word;
end;
pb8EXTSST = ^rb8EXTSST;

rb8ISSTINF = packed record
  ib : cardinal;
  cb : word;
  res1 : word;
end;
pb8ISSTINF = ^rb8ISSTINF;

rb8LABELSST = packed record
  rw : word;
  col : word;
  ixfe : word;
  isst : cardinal;
end;

rb8FORMULA = packed record
  rw : word;
  col : word;
  ixfe : word;
  value : double;
  grbit : word;
  chn : cardinal;
  cce : word;
end;
pb8FORMULA = ^rb8FORMULA;

rb8LEFTMARGIN = packed record
  num : double;
end;

rb8RIGHTMARGIN = packed record
  num : double;
end;

rb8TOPMARGIN = packed record
  num : double;
end;

rb8BOTTOMMARGIN = packed record
  num : double;
end;

rb8NUMBER = packed record
  rw : word;
  col : word;
  ixfe : word;
  num : double;
end;
pb8NUMBER = ^rb8NUMBER;

rb8IMDATA = packed record
  cf : word;
  env : word;
  lcb : cardinal;
end;
pb8IMDATA = ^rb8IMDATA;

rb8OBJ = packed record
  cObj : cardinal;
  OT : word;
  id : word;
  grbit : word;
  colL : word;
  dxL : word;
  rwT : word;
  dyT : word;
  colR : word;
  dxR : word;
  rwB : word;
  dyB : word;
  cbMacro : word;
  Reserved : array [0..5] of byte;
end;
pb8OBJ = ^rb8OBJ;

rb8OBJPICTURE = packed record
  icvBack : byte;
  icvFore : byte;
  fls : byte;
  fAutoFill : byte;
  icv : byte;
  lns : byte;
  lnw : byte;
  fAutoBorder : byte;
  frs : word;
  cf : word;
  Reserved1 : cardinal;
  cbPictFmla : word;
  Reserved2 : word;
  grbit : word;
  Reserved3 : cardinal;
end;
pb8OBJPICTURE = ^rb8OBJPICTURE;

rb8HORIZONTALPAGEBREAKS = packed record
  cbrk : word;
end;
pb8HORIZONTALPAGEBREAKS = ^rb8HORIZONTALPAGEBREAKS;

rb8HORIZONTALPAGEBREAK = packed record
  row : word;
  startcol : word;
  endcol : word;
end;
pb8HORIZONTALPAGEBREAK = ^rb8HORIZONTALPAGEBREAK;

rb8SUPBOOK = packed record
  Ctab : word;
  cch : word;
  grbit : byte;
  code : word;
end;
pb8SUPBOOK = ^rb8SUPBOOK;

rb8EXTERNSHEET = packed record
  cXTI : word;
end;
pb8EXTERNSHEET = ^rb8EXTERNSHEET;
rb8XTI = packed record
  iSUPBOOK : word;
  itabFirst : word;
  itabLast : word;
end;
pb8XTI = ^rb8XTI;

/////////////////////////////////////////////////
//
// structs and types for support MSODRAWINGxxx
// records
//
/////////////////////////////////////////////////
MSODGID = cardinal;
MSOSPID = cardinal;

rb8FSP = packed record
  spid : MSOSPID;
  grfPersistent : cardinal;
end;
pb8FSP = ^rb8FSP;

rb8FOPTE = packed record
  pid_fBid_fComplex : word;
  op : cardinal;
end;
pb8FOPTE = ^rb8FOPTE;

rb8MSOFBH = packed record
  inst_ver : word;
  fbt : word;
  cbLength : cardinal;
end;
pb8MSOFBH = ^rb8MSOFBH;

rb8FBSE = packed record
  btWin32 : byte;
  btMacOs : byte;
  rgbUid : array [0..15] of byte;
  tag : word;
  size : cardinal;
  cRef : cardinal;
  foDelay : cardinal;
  usage : byte;
  cbName : byte;
  unused2 : byte;
  unused3 : byte;
end;
pb8FBSE = ^rb8FBSE;

rb8FBSEDIB = packed record
  Unknown : array [0..7] of byte;
  rgbUid : array [0..15] of byte;
  Tag : byte;
end;
pb8FBSEDIB = ^rb8FBSEDIB;

rb8FDGG = packed record
  spidMax : MSOSPID;
  cidcl : cardinal;
  cspSaved : cardinal;
  cdgSaved : cardinal;
end;
pb8FDGG = ^rb8FDGG;

rb8FIDCL = packed record
  dgid : MSODGID;
  cspidCur : cardinal;
end;
pb8FIDCL = ^rb8FIDCL;

rb8FDG = packed record
  csp : cardinal;
  spidCur : MSOSPID;
end;
pb8FDG = ^rb8FDG;

rb8FSPGR = packed record
  rcgBounds : TRect;
end;
pb8FSPGR = ^rb8FSPGR;

rb8FDGGFull = packed record
  Header : rb8MSOFBH;
  FDGG : rb8FDGG;
end;

rb8FDGFull = packed record
  Header : rb8MSOFBH;
  FDG : rb8FDG;
end;
pb8FDGFull = ^rb8FDGFull;

rb8FBSEFull = packed record
  Header : rb8MSOFBH;
  FBSE : rb8FBSE;
  FBSEDIB : rb8FBSEDIB;
end;

rb8FSPFull = packed record
  Header : rb8MSOFBH;
  FSP : rb8FSP;
end;
pb8FSPFull = ^rb8FSPFull;

rb8FSPGRFull = packed record
  Header : rb8MSOFBH;
  FSPGR : rb8FSPGR;
end;
pb8FSPGRFull = ^rb8FSPGRFull;

/////////////////////////////////////////////////
//
// Microsoft Excel ptgs
//
/////////////////////////////////////////////////
const
ptgInt = $1E;
ptgNum = $1F;
ptgStr = $17;
ptgGT = $0D;
ptgGE = $0C;
ptgEQ = $0B;
ptgNE = $0E;
ptgLE = $0A;
ptgLT = $09;
ptgAdd = $03;
ptgSub = $04;
ptgMul = $05;
ptgDiv = $06;
ptgPower = $07;
ptgPercent = $14;
ptgConcat = $08;
ptgUplus = $12;
ptgUminus = $13;
ptgParen = $15;
ptgMissArg = $16;
ptgRef = $44; // $24
ptgArea = $25;
ptgFuncVar = $42;
ptgRef3D = $5A;
ptgArea3D = $3B;

type

rptgInt = packed record
  w : word;
end;

rptgNum = packed record
  num : double;
end;

rptgStr = packed record
  cch : byte;
  grbit : byte;
end;
pptgStr = ^rptgStr;

rptgRef = packed record
  rw : word;
  grbitCol : word;
end;
pptgRef = ^rptgRef;

rptgArea = packed record
  rwFirst : word;
  rwLast : word;
  grbitColFirst : word;
  grbitColLast : word;
end;
pptgArea = ^rptgArea;

rptgFuncVar = packed record
  cargs : byte;
  iftab : word;
end;

rptgRef3D = packed record
  ixti : word;
  rw : word;
  grbitCol : word;
end;
pptgRef3D = ^rptgRef3D;

rptgArea3D = packed record
  ixti : word;
  rwFirst : word;
  rwLast : word;
  grbitColFirst : word;
  grbitColLast : word;
end;
pptgArea3D = ^rptgArea3D;

/////////////////////////////////////////////////
//
// common functions
//
/////////////////////////////////////////////////
procedure StringToWideChar(const Source : string; Dest : PWideChar; DestSize : Integer);

implementation

procedure StringToWideChar(const Source: string; Dest: PWideChar;
  DestSize: Integer);
begin
MultiByteToWideChar(0,0,PChar(Source),Length(Source),Dest,DestSize);
end;


end.



