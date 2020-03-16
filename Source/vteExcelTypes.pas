
{******************************************}
{                                          }
{           vtk Export library             }
{                                          }
{      Copyright (c) 2002 by vtkTools      }
{                                          }
{******************************************}

unit vteExcelTypes;

interface

{$include vtk.inc}

type

TvteCellDataType = (vtecdtNumber,vtecdtString,vtecdtFormula);
TvteXLSLineStyleType = (vtelsNone,
                       vtelsThin,
                       vtelsMedium,
                       vtelsDashed,
                       vtelsDotted,
                       vtelsThick,
                       vtelsDouble,
                       vtelsHair,
                       vtelsMediumDashed,
                       vtelsDashDot,
                       vtelsMediumDashDot,
                       vtelsDashDotDot,
                       vtelsMediumDashDotDot,
                       vtelsSlantedDashDot);
TvteXLSWeightType = (vtexlHairline,
                    vtexlThin,
                    vtexlMedium,
                    vtexlThick);
TvteXLSBorderType = (vtexlDiagonalDown,
                    vtexlDiagonalUp,
                    vtexlEdgeBottom,
                    vtexlEdgeLeft,
                    vtexlEdgeRight,
                    vtexlEdgeTop{,
                    vtexlInsideHorizontal,
                    vtexlInsideVertical});

TvteXLSBorderTypes = set of TvteXLSBorderType;

TvteXLSHorizontalAlignmentType = (vtexlHAlignGeneral,
                                 vtexlHAlignLeft,
                                 vtexlHAlignCenter,
                                 vtexlHAlignRight,
                                 vtexlHAlignFill,
                                 vtexlHAlignJustify,
                                 vtexlHAlignCenterAcrossSelection);
TvteXLSVerticalAlignmentType = (vtexlVAlignTop,
                               vtexlVAlignCenter,
                               vtexlVAlignBottom,
                               vtexlVAlignJustify);

TvteXLSOrderType = (vtexlDownThenOver,vtexlOverThenDown);
TvteXLSOrientationType = (vtexlPortrait,vtexlLandscape);
TvteXLSPaperSizeType = (vtexlPaperOther,
                        vtexlPaperLetter, {8 1/2 x 11"}
                        vtexlPaperLetterSmall, {8 1/2 x 11"}
                        vtexlPaperTabloid, {11 x 17"}
                        vtexlPaperLedger, {17 x 11"}
                        vtexlPaperLegal, {8 1/2 x 14"}
                        vtexlPaperStatement, {5 1/2 x 8 1/2"}
                        vtexlPaperExecutive, {7 1/4 x 10 1/2"}
                        vtexlPaperA3, {297 x 420 μμ}
                        vtexlPaperA4, {210 x 297 μμ}
                        vtexlPaperA4SmallSheet, {210 x 297 μμ}
                        vtexlPaperA5, {148 x 210 μμ}
                        vtexlPaperB4, {250 x 354 μμ}
                        vtexlPaperB5, {182 x 257 μμ}
                        vtexlPaperFolio, {8 1/2 x 13"}
                        vtexlPaperQuartoSheet, {215 x 275 μμ}
                        vtexlPaper10x14, {10 x 14"}
                        vtexlPaper11x17, {11 x 17"}
                        vtexlPaperNote, {8 1/2 x 11"}
                        vtexlPaper9Envelope, {3 7/8 x 8 7/8"}
                        vtexlPaper10Envelope, {4 1/8  x 9 1/2"}
                        vtexlPaper11Envelope, {4 1/2 x 10 3/8"}
                        vtexlPaper12Envelope, {4 3/4 x 11"}
                        vtexlPaper14Envelope, {5 x 11 1/2"}
                        vtexlPaperCSheet, {17 x 22"}
                        vtexlPaperDSheet, {22 x 34"}
                        vtexlPaperESheet, {34 x 44"}
                        vtexlPaperDLEnvelope, {110 x 220 μμ}
                        vtexlPaperC5Envelope, {162 x 229 μμ}
                        vtexlPaperC3Envelope, {324 x 458 μμ}
                        vtexlPaperC4Envelope, {229 x 324 μμ}
                        vtexlPaperC6Envelope, {114 x 162 μμ}
                        vtexlPaperC65Envelope, {114 x 229 μμ}
                        vtexlPaperB4Envelope, {250 x 353 μμ}
                        vtexlPaperB5Envelope, {176 x 250 μμ}
                        vtexlPaperB6Envelope, {176 x 125 μμ}
                        vtexlPaperItalyEnvelope, {110 x 230 μμ}
                        vtexlPaperMonarchEnvelope, {3 7/8 x 7 1/2"}
                        vtexlPaper63_4Envelope, {3 5/8 x 6 1/2"}
                        vtexlPaperUSStdFanfold, {14 7/8 x 11"}
                        vtexlPaperGermanStdFanfold, {8 1/2 x 12"}
                        vtexlPaperGermanLegalFanfold, {8 1/2 x 13"}
                        vtexlPaperB4_ISO, {250 x 353 μμ}
                        vtexlPaperJapanesePostcard, {100 x 148 μμ}
                        vtexlPaper9x11, {9 x 11"}
                        vtexlPaper10x11, {10 x 11"}
                        vtexlPaper15x11, {15 x 11"}
                        vtexlPaperEnvelopeInvite, {220 x 220 μμ}
                        vtexlPaperLetterExtra, {9 \ 275 x 12"}
                        vtexlPaperLegalExtra, {9 \275 x 15"}
                        vtexlPaperTabloidExtra, {11.69 x 18"}
                        vtexlPaperA4Extra, {9.27 x 12.69"}
                        vtexlPaperLetterTransverse, {8 \275 x 11"}
                        vtexlPaperA4Transverse, {210 x 297 μμ} 
                        vtexlPaperLetterExtraTransverse, {9\275 x 12"}
                        vtexlPaperSuperASuperAA4, {227 x 356 μμ}
                        vtexlPaperSuperBSuperBA3, {305 x 487 μμ}
                        vtexlPaperLetterPlus, {8.5 x 12.69"}
                        vtexlPaperA4Plus, {210 x 330 μμ}
                        vtexlPaperA5Transverse, {148 x 210 μμ}
                        vtexlPaperB5_JIS_Transverse, {182 x 257 μμ}
                        vtexlPaperA3Extra, {322 x 445 μμ}
                        vtexlPaperA5Extra, {174 x 235 μμ}
                        vtexlPaperB5_ISO_Extra, {201 x 276 μμ}
                        vtexlPaperA2, {420 x 594 μμ}
                        vtexlPaperA3Transverse, {297 x 420 μμ}
                        vtexlPaperA3ExtraTransverse {322 x 445 μμ});
(*
vtexlPaper11x17,{ 11 in. x 17 in.}
                       vtexlPaperA4,{ A4 (210 mm x 297 mm)}
                       vtexlPaperA5,{ A5 (148 mm x 210 mm)}
                       vtexlPaperB5,{ A5 (148 mm x 210 mm)}
                       vtexlPaperDsheet,{ D size sheet}
                       vtexlPaperEnvelope11,{ Envelope #11 (4-1/2 in. x 10-3/8 in.)}
                       vtexlPaperEnvelope14,{ Envelope #14 (5 in. x 11-1/2 in.)}
                       vtexlPaperEnvelopeB4,{ Envelope B4 (250 mm x 353 mm)}
                       vtexlPaperEnvelopeB6,{ Envelope B6 (176 mm x 125 mm)}
                       vtexlPaperEnvelopeC4,{ Envelope C4 (229 mm x 324 mm)}
                       vtexlPaperEnvelopeC6,{ Envelope C6 (114 mm x 162 mm)}
                       vtexlPaperEnvelopeDL,{ Envelope DL (110 mm x 220 mm)}
                       vtexlPaperEnvelopeMonarch,{ Envelope Monarch (3-7/8 in. x 7-1/2 in.)}
                       vtexlPaperEsheet,{ E size sheet}
                       vtexlPaperFanfoldLegalGerman,{ German Legal Fanfold (8-1/2 in. x 13 in.)}
                       vtexlPaperFanfoldUS,{ U.S. Standard Fanfold (14-7/8 in. x 11 in.)}
                       vtexlPaperLedger,{ Ledger (17 in. x 11 in.)}
                       vtexlPaperLetter,{ Letter (8-1/2 in. x 11 in.)}
                       vtexlPaperNote,{ Note (8-1/2 in. x 11 in.)}
                       vtexlPaperStatement,{ Statement (5-1/2 in. x 8-1/2 in.)}
                       vtexlPaperUser,{ User-defined}
                       vtexlPaper10x14,{ 10 in. x 14 in.}
                       vtexlPaperA3,{ A3 (297 mm x 420 mm)}
                       vtexlPaperA4Small,{ A4 Small (210 mm x 297 mm)}
                       vtexlPaperB4,{ B4 (250 mm x 354 mm)}
                       vtexlPaperCsheet,{ C size sheet}
                       vtexlPaperEnvelope10,{ Envelope #10 (4-1/8 in. x 9-1/2 in.)}
                       vtexlPaperEnvelope12,{ Envelope #12 (4-1/2 in. x 11 in.)}
                       vtexlPaperEnvelope9,{ Envelope #9 (3-7/8 in. x 8-7/8 in.)}
                       vtexlPaperEnvelopeB5,{ Envelope B5 (176 mm x 250 mm)}
                       vtexlPaperEnvelopeC3,{ Envelope C3 (324 mm x 458 mm)}
                       vtexlPaperEnvelopeC5,{ Envelope C5 (162 mm x 229 mm)}
                       vtexlPaperEnvelopeC65,{ Envelope C65 (114 mm x 229 mm)}
                       vtexlPaperEnvelopeItaly,{ Envelope (110 mm x 230 mm)}
                       vtexlPaperEnvelopePersonal,{ Envelope (3-5/8 in. x 6-1/2 in.)}
                       vtexlPaperExecutive,{ Executive (7-1/2 in. x 10-1/2 in.)}
                       vtexlPaperFanfoldStdGerman,{ German Legal Fanfold (8-1/2 in. x 13 in.)}
                       vtexlPaperFolio,{ Folio (8-1/2 in. x 13 in.)}
                       vtexlPaperLegal,{ Legal (8-1/2 in. x 14 in.)}
                       vtexlPaperLetterSmall,{ Letter Small (8-1/2 in. x 11 in.)}
                       vtexlPaperQuarto,{ Quarto (215 mm x 275 mm)}
                       vtexlPaperTabloid{ Tabloid (11 in. x 17 in.)}); *)
TvteXLSPrintErrorsType = (vtexlPrintErrorsBlank,vtexlPrintErrorsDash,vtexlPrintErrorsDisplayed,vtexlPrintErrorsNA);
TvteXLSFillPattern = (vtefpNone,
                     vtefpAutomatic,
                     vtefpChecker,
                     vtefpCrissCross,
                     vtefpDown,
                     vtefpGray8,
                     vtefpGray16,
                     vtefpGray25,
                     vtefpGray50,
                     vtefpGray75,
                     vtefpGrid,
                     vtefpHorizontal,
                     vtefpLightDown,
                     vtefpLightHorizontal,
                     vtefpLightUp,
                     vtefpLightVertical,
                     vtefpSemiGray75,
                     vtefpSolid,
                     vtefpUp,
                     vtefpVertical);

TvteXLSImageBorderLineStyle = (vteblsSolid,
                               vteblsDash,
                               vteblsDot,
                               vteblsDashDot,
                               vteblsDashDotDot,
                               vteblsNull,
                               vteblsDarkGray,
                               vteblsMediumGray,
                               vteblsLightGray);
TvteXLSImageBorderLineWeight = (vteblwHairline,
                                vteblwSingle,
                                vteblwDouble,
                                vteblwThick);                               

implementation

end.
