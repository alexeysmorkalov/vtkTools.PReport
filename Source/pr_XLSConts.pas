{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_XLSConts;

interface

uses
  graphics, vteExcelTypes,

  pr_Common;
  
const
  MAX_EXCEL_ROW_HEIGHT = 409;
  MAX_EXCEL_COLUMN_WIDTH = 255;

  XLSHorAlignment : array [TprHAlign] of TvteXLSHorizontalAlignmentType = (vtexlHAlignLeft, vtexlHAlignCenter, vtexlHAlignRight, vtexlHAlignLeft);
  XLSVertAlignment : array [TprVAlign] of TvteXLSVerticalAlignmentType = (vtexlVAlignTop, vtexlVAlignCenter, vtexlVAlignBottom);
  XLSLineStyle : array [TPenStyle] of TvteXLSLineStyleType = (vtelsThin,vtelsDashed,vtelsDotted,vtelsDashDot,vtelsDashDotDot,vtelsNone,vtelsNone);

implementation

end.
