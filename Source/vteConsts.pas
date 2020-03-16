
{******************************************}
{                                          }
{           vtk Export library             }
{                                          }
{      Copyright (c) 2002 by vtkTools      }
{                                          }
{******************************************}

unit vteConsts;

interface

{$include vtk.inc}

const

// HTMLSaver constants
  vteHTML_VERSION = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0//EN">';
  vteOPENTAGPREFIX = '<';
  vteCLOSETAGPREFIX = '</';
  vteTAGPOSTFIX = '>';
  vteTABLETAG = 'TABLE';
  vteROWTAG = 'TR';
  vteCELLTAG = 'TD';
  vteHEADTAG = 'HEAD';
  vteTITLETAG = 'TITLE';
  vteFORMTAG = 'FORM';
  vteHTMLTAG = 'HTML';
  vteBODYTAG = 'BODY';
  vteSTYLETAG = 'STYLE';
  vteANCHORTAG = 'A';
  vtePRETAG = 'PRE';

  vteCOLSPANATTRIBUTE = 'colspan';
  vteROWSPANATTRIBUTE = 'rowspan';
  vteWIDTHATTRIBUTE = 'width';
  vteHEIGHTATTRIBUTE = 'height';
  vteBLOCKSEPARATOR : string = #13#10;
  vteMAXOBJECTTYPENAME = 32;
  vteMAXLENGTH = 255;
  vteMAXMARGINSTRING : array [0..31] of char = '                                ';
  vteMAXFSWD = 512;

  vteFONT_BOLD = 'bold';
  vteFONT_NORMAL = 'normal';
  vteFONT_UNDERLINE = 'underline';
  vteFONT_STRIKE = 'line-through ';
  vteFONT_NONE = 'none';
  vteFONT_ITALIC = 'italic';
  vteTABLESTYLE = 'border-collapse:collapse';
  vteSTYLEFORMAT = 'c%d';
  vteVALIGN = 'vectical-align';
  vteTEXTTOP = 'text-top';
  vteMiddle = 'middle';
  vteTextBottom = 'text-bottom';
  vteTEXTALIGN = 'text-align';
  vteLEFT = 'left';
  vteRIGHT = 'right';
  vteCENTER = 'center';
  vteJustify = 'justify';
  vteBackgroundColor = 'background-color';

// Special characters for HTML
  vteHtml_quot = '&quot;'; //quotation mark
  vteHtml_amp = '&amp;';   // ampersand
  vteHtml_lt = '&lt;';     // less-than sign
  vteHtml_gt = '&gt;';     // greater-than sign
  vteHtml_space = '&nbsp;'; // space
  vteHtml_crlf = '<BR>';    // CRLF

// DefaultFont for Export
  vteDefFontFace = 'Arial';
  vteDefFontSize = 10;
  vteDefFontSymbol = 'x';

  vteSmRepInch = 2.5412;
  vtePointPerInch = 72;

implementation

end.
