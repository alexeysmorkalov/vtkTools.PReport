{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the definitions of some base classes and interfaces of TprReport.
See also:
  pr_Common}
unit pr_CommonClasses;

{$i pr.inc}

interface

uses
  Classes;

type

  /////////////////////////////////////////////////
  //
  // IprReportContainer
  //
  /////////////////////////////////////////////////
{Internal interface, is used for implementation of subreports.}
  IprReportContainer = interface
    function FormatStrings(lSource, lDest: TStrings; DeleteEmptyLines, DeleteEmptyLinesAtEnd: Boolean): Boolean;
    function FormatTemplate(Template: string; var Res: string): Boolean;
  end;

implementation

end.
