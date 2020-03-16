{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

unit pr_Parser;

{$i pr.inc}
interface

uses
  Windows, math,
  Classes, SysUtils, Pr_Utils, Db, Mask, Forms,
  {$ifdef PR_D6_D7} variants, maskutils, {$endif}

  pr_Common, pr_Dataset;

const
  prMAX_CALCOPERATORS = 20;
  prMAX_FUNCTIONS  = 36;
  prMAX_OBJFUNCTIONS = 11;
  prMAX_OBJPROPS = 3;
  prMAX_QUERYES = 4;
  prDefaultStackSize = 10;
  REPORT_PROPS_INFO_INDEX = 2;

var
  CurrencyFormat : string = '#,0.00р''.'';-#,0.00р''.''';
  ShortCurrencyFormat : string = '0.00р''.'';-0.00р''.''';
  SimpleCurrencyFormat : string = '0.00;-0.00';
  PercentFormat : string = '0.00%;-0.00%';
  SpacedCurrencyFormat : string = '#,0.00;-#,0.00';
  BankCurrencyFormat : string = '\ds-0.00;-0.00';

type

  TprParser = class;
  TprExprItemType = (preiOperator,preiValue,preiVar,preiFunction,preiObjectProperty,preiObjectFunction,preiReportVariable,preiReportSystemVariable,preiMissedFunctionParameter,preiReportSimpleVariable);
  TprTypeBracket = (prtbFunction,prtbExpression);
  TprIdentType = (pritVar,pritObject,pritObjectProperty,pritObjectFunction,pritReportVariable);

  TprCalcOperatorProc = procedure(ML : TprParser; var Res : TprVarValue);
  TprCalcOperatorInfo = record
    Name : string[2];
    Priority : integer;
    Proc : TprCalcOperatorProc;
  end;
  pprCalcOperatorInfo = ^TprCalcOperatorInfo;

  TprFuncProc = procedure(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  TprFuncInfo = record
    Name : string;
    Func : TprFuncProc;
    min : integer;
    max : integer;
  end;
  pprFuncInfo = ^TprFuncInfo;

  TprObjFuncProc = procedure(ML : TprParser; C : TComponent; Parameters : TprVarsArray; var Res : TprVarValue);
  TprObjFuncInfo = record
    ObjClassRef : TPersistentClass;
    FuncName : string;
    Func : TprObjFuncProc;
    min : integer;
    max : integer;
  end;
  PprObjFuncInfo = ^TprObjFuncInfo;

  TprObjPropFunc = procedure(C: TObject; const PropName: string; var Res: TprVarValue);
  TprGetPropsNamesFunc = procedure(C: TObject; L: TStringList);
  TprObjPropInfo = record
    ObjClassRef : TPersistentClass;
    Func : TprObjPropFunc;
    GetPropsNamesFunc : TprGetPropsNamesFunc;
    min: integer;
    max: integer;
  end;
  PprObjPropInfo = ^TprObjPropInfo;

  /////////////////////////////////////////////////
  //
  // TprExprItem
  //
  /////////////////////////////////////////////////
  TprExprItem = record
    ObjType : TprExprItemType; // type - operation, function, var, etc
    ObjOpInfo : pprCalcOperatorInfo;
    ObjInfo : pointer;
    ObjName : string;
    ObjObject : TComponent;
    ObjLastName : string;
    ObjValue : TprVarValue;
    ObjParCount : integer;
    ObjOperandExists : boolean;
  end;
  pprExprItem = ^TprExprItem;

  /////////////////////////////////////////////////
  //
  // TprExpr
  //
  /////////////////////////////////////////////////
  TprExpr = class(TObject)
  private
    FList : TList;
  protected
    function GetItm(i : integer) : pprExprItem;
    function GetCount : integer; virtual;
    function GetLast : pprExprItem; virtual;
  public
    property Items[i : integer] : pprExprItem read GetItm; default;
    property Count : integer read GetCount;
    property Last : pprExprItem read GetLast;
    function Push : pprExprItem; virtual;
    function Pop : pprExprItem; virtual;
    procedure Clear; virtual;
    procedure Reset; virtual;
    constructor Create;
    destructor Destroy; override;
  end;

  /////////////////////////////////////////////////
  //
  // TprCachedExpr
  //
  /////////////////////////////////////////////////
  TprCachedExpr = class(TprExpr)
  private
    FCurPos : integer;
  protected
    function GetCount : integer; override;
    function GetLast : pprExprItem; override;
  public
    function Push : pprExprItem; override;
    function Pop : pprExprItem; override;
    procedure Clear; override;
    procedure Reset; override;
    constructor Create;
  end;

  /////////////////////////////////////////////////
  //
  // TprExprs
  //
  /////////////////////////////////////////////////
  TprExprs = class(TList)
  private
    function GetItm(i : integer) : TprExpr;
  public
    property Items[i : integer] : TprExpr read GetItm; default;
    procedure Clear; override;
  end;

  TprFormatReplaceCallBackOptions = (prfrcRTF);
  TprFormatReplaceCallBackOptionsSet = set of TprFormatReplaceCallBackOptions;
  TprFormatReplaceCallBackFunc = procedure (FromPos, Count: Integer; const Buf: PChar; BufSize: Integer; Flags: TprFormatReplaceCallBackOptionsSet; CallBackData: Pointer);
  /////////////////////////////////////////////////
  //
  // TprParser
  //
  /////////////////////////////////////////////////
  TprParser = class(TObject)
  private
    Stack: TprVarsArray;  // stack
    pStack: Integer; // position in stack
    sStack: Integer; // max size of stack
    LReportProps: TStringList;

    FTempExpr : TprExpr;
    FExprs : TprExprs;
    FCompileOpStack : TprCachedExpr;

    FReport: TprCustomReport;
    FValues: TprValues;
    FSystemValues: TprValues;

    procedure ResetStack;
    function rStack : PprVarValue; // read from stack
    procedure wStack(const Value : TprVarValue); overload; // push in stack Value
    function wStack : integer; overload;
    procedure CopyExprItem(Source,Dest : pprExprItem);
    procedure SetError(const ErrorMessage : string);
    procedure CalcExpression(Expr : TprExpr; var Res : TprVarValue);
    procedure SaveExpression(Expr : TprExpr);
    function FindObjPropInfo(Component : TComponent) : PprObjPropInfo;
    function FindObjFuncInfo(Component : TComponent; const FuncName : string) : PprObjFuncInfo;
    procedure InternalReadParams(n : integer; var Vars : TprVarsArray);
    procedure InternalCheckParams(min,max,v : integer; const FuncName : string);
    procedure InternalCalcFunction(e : pprExprItem);
    procedure InternalCalcVar(e : pprExprItem);
    procedure InternalCalcObjectProp(e : pprExprItem);
    procedure InternalCalcObjectFunction(e : pprExprItem);
  public
    constructor Create(AReport: TprCustomReport; AValues: TprValues; ASystemValues: TprValues);
    destructor Destroy; override;

    function CompileExpression(const s : string; cExpr: TprExpr): Boolean;
    function Calc(var Expr: string; var Res: TprVarValue): Boolean;
    procedure CalcOnePass(const Expr: string; var Res: TprVarValue);
    function FormatTemplate(Mask: string; var Res: string) : boolean;
    function FormatTemplateEx(const Mask: string; ReplaceCallBack: TprFormatReplaceCallBackFunc; CallBackData: pointer; var Res: string): boolean;
    function FormatStrings(lSource, lDest: TStrings; DeleteEmptyLines, DeleteEmptyLinesAtEnd: Boolean): Boolean;
    procedure ClearInternalStructs;

    property Report: TprCustomReport read FReport;
    property Values: TprValues read FValues write FValues;
    property SystemValues: TprValues read FSystemValues write FSystemValues;
  end;

  ////////////////////////////
  //
  // functions
  //
  ////////////////////////////
  procedure _vCopy(const vSource : TprVarValue; var vDest : TprVarValue);
  function _vAsString(const v : TprVarValue) : string;
  function _vAsBoolean(const v : TprVarValue) : boolean;
  function _vAsInteger(const v : TprVarValue) : integer;
  function _vAsDouble(const v : TprVarValue) : double;
  function _vAsObject(const v : TprVarValue) : TObject;
  function _vAsDateTime(const v : TprVarValue) : TDateTime;
  function _vAsVariant(const v : TprVarValue) : Variant;
  function _vIsNull(const v : TprVarValue) : boolean;
  
  procedure _vSetNull(var v : TprVarValue);
  procedure _vSetAsObject(var v : TprVarValue; Value : TObject);
  procedure _vSetAsString(var v : TprVarValue; Value : string);
  procedure _vSetAsDateTime(var v : TprVarValue; Value : TDateTime);
  procedure _vSetAsDouble(var v : TprVarValue; Value : double);
  procedure _vSetAsInteger(var v : TprVarValue; Value : integer);
  procedure _vSetAsBoolean(var v : TprVarValue; Value : boolean);
  procedure _vSetAsVariant(var v : TprVarValue; Value : Variant);
  procedure _vSetAsType(var v : TprVarValue; Value : Variant; VarType : TprVarValueType);

  procedure VarFromField(Field : TField; var Res : TprVarValue);

  function pFormat(const aDisplayFormat: String; const aValue: TprVarValue): String;

  ////////////////////////////////////////
  //
  // OPERATORS
  //
  ////////////////////////////////////////
  procedure _OpDiv(ML : TprParser; var Res : TprVarValue);
  procedure _OpMul(ML : TprParser; var Res : TprVarValue);
  procedure _OpPlus(ML : TprParser; var Res : TprVarValue);
  procedure _OpMinus(ML : TprParser; var Res : TprVarValue);
  procedure _OpNot(ML : TprParser; var Res : TprVarValue);
  procedure _OpNEQ(ML : TprParser; var Res : TprVarValue);
  procedure _OpGEQ(ML : TprParser; var Res : TprVarValue);
  procedure _OpLEQ(ML : TprParser; var Res : TprVarValue);
  procedure _OpG(ML : TprParser; var Res : TprVarValue);
  procedure _OpL(ML : TprParser; var Res : TprVarValue);
  procedure _OpEQ(ML : TprParser; var Res : TprVarValue);
  procedure _OpAnd(ML : TprParser; var Res : TprVarValue);
  procedure _OpOr(ML : TprParser; var Res : TprVarValue);
  procedure _OpPower(ML : TprParser; var Res : TprVarValue);
  procedure _OpUPlus(ML : TprParser; var Res : TprVarValue);
  procedure _OpUMinus(ML : TprParser; var Res : TprVarValue);

  /////////////////////////////////////////////////
  //
  // FUNCTIONS
  //
  /////////////////////////////////////////////////
  procedure _DateTime(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _Time(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _GetMonth(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _GetYear(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _GetDay(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _GetLastDayMonth(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _GetFirstDayMonth(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _GetMonthName(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _HourBetween(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _MonthsBetween(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _DaysBetween(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _IncMonth(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _IncDay(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _GetYM(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _IncYM(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _GetDateFromYM(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);

  procedure _Round(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _Trunc(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _Min(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _Max(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _In(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);

  procedure _GSN(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _IsZero(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _IsNotZero(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _IIF(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);

  procedure _Copy(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _MakeStr(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);

  procedure _AnsiUpperCase(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _AnsiLowerCase(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _Trim(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _AddLeft(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _AddRight(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _Length(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);

  procedure _UID(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);

  procedure _Null(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _Abs(ML : TprParser; Parameters : TprVarsArray; var Res : TprVarValue);

  /////////////////////////////////////////////////
  //
  // Classes methods
  //
  /////////////////////////////////////////////////
  // TDataSet
  procedure _TDataSet_Eof(ML : TprParser; C : TComponent; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _TDataSet_Locate(ML : TprParser; C : TComponent; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _TDataSet_IsNullField(ML : TprParser; C : TComponent; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _TDataSet_IsZeroField(ML : TprParser; C : TComponent; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _TDataSet_LineNo(ML : TprParser; C : TComponent; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _TDataSet_GetFieldValue(ML : TprParser; C : TComponent; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _TprDataSet_GetFieldValue(ML : TprParser; C : TComponent; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _TDataSet_FieldDisplayText(ML : TprParser; C : TComponent; Parameters : TprVarsArray; var Res : TprVarValue);

  procedure _TprDataSet_LineNo(ML : TprParser; C : TComponent; Parameters : TprVarsArray; var Res : TprVarValue);

  procedure _TprGroup_LineNo(ML : TprParser; C : TComponent; Parameters : TprVarsArray; var Res : TprVarValue);
  procedure _TprGroup_GroupValue(ML : TprParser; C : TComponent; Parameters : TprVarsArray; var Res : TprVarValue);

  /////////////////////////////////////////////////
  //
  // Classes properties
  //
  /////////////////////////////////////////////////
  procedure _TDataSet_rProp(C: TObject; const PropName: string; var Res: TprVarValue);
  procedure _TprCustomReport_rProp(C: TObject; const PropName: string; var Res : TprVarValue);
  procedure _TDataSet_PropsList(C: TObject; L: TStringList);
  procedure _TprCustomReport_PropsList(C: TObject; L: TStringList);
  procedure _TprDataSet_rProp(C: TObject; const PropName: string; var Res: TprVarValue);
  procedure _TprDataSet_PropsList(C: TObject; L: TStringList);

const
  prParserStartBracketOperatorIndex = 1;
  prParserEndBracketOperatorIndex = 2;
  prParserUnaryPlusOperatorIndex = 17;
  prParserUnaryMinusOperatorIndex = 18;
  prParserFunctionOperatorIndex = prMAX_CALCOPERATORS;

  prParserFunctionPriority = 8; // !!!
  prParserStartBracketPriority = 0; // !!!
  prParserEndBracketPriority = 1; // !!!

  CO : array [1..prMAX_CALCOPERATORS] of TprCalcOperatorInfo =
  (
   (Name : '('; Priority : 0; Proc : nil),
   (Name : ')'; Priority : 1; Proc : nil),
   (Name : '>='; Priority : 2; Proc : _OpGEQ),
   (Name : '=<'; Priority : 2; Proc : _OpLEQ),
   (Name : '<='; Priority : 2; Proc : _OpLEQ),
   (Name : '<>'; Priority : 2; Proc : _OpNEQ),
   (Name : '>'; Priority : 2; Proc : _OpG),
   (Name : '='; Priority : 2; Proc : _OpEQ),
   (Name : '<'; Priority : 2; Proc : _OpL),
   (Name : '&'; Priority : 3; Proc : _OpAnd),
   (Name : '|'; Priority : 3; Proc : _OpOr),
   (Name : '+'; Priority : 4; Proc : _OpPlus),
   (Name : '-'; Priority : 4; Proc : _OpMinus),
   (Name : '*'; Priority : 5; Proc : _OpMul),
   (Name : '/'; Priority : 5; Proc : _OpDiv),
   (Name : '^'; Priority : 6; Proc : _OpPower),
   (Name : '+'; Priority : 7; Proc : _OpUPlus),
   (Name : '-'; Priority : 7; Proc : _OpUMinus),
   (Name : '!'; Priority : 7; Proc : _OpNot),
   (Name : ''; Priority : 8; Proc : nil) // !!! this line must be at end of array
  );

  FuncInfo : array [1..prMAX_FUNCTIONS] of TprFuncInfo=
  (
   (Name : 'DateTime';            Func : _DateTime; min : 0; max : 0),
   (Name : 'Time';                Func : _Time; min : 0; max : 0),
   (Name : 'GetFirstDayMonth';    Func : _GetFirstDayMonth; min : 1; max : 2),
   (Name : 'GetLastDayMonth';     Func : _GetLastDayMonth; min : 1; max : 2),
   (Name : 'GetYear';             Func : _GetYear; min : 1; max : 1),
   (Name : 'GetMonth';            Func : _GetMonth; min : 1; max : 1),
   (Name : 'HourBetween';         Func : _HourBetween; min : 2; max : 4),
   (Name : 'MonthsBetween';       Func : _MonthsBetween; min : 2; max : 2),
   (Name : 'DaysBetween';         Func : _DaysBetween; min : 2; max : 2),
   (Name : 'IncMonth';            Func : _IncMonth; min : 2; max : 2),
   (Name : 'IncDay';              Func : _IncDay; min : 2; max : 2),
   (Name : 'GetYM';               Func : _GetYM; min : 1; max : 2),
   (Name : 'IncYM';               Func : _IncYM; min : 2; max : 2),
   (Name : 'GetMonthName';        Func : _GetMonthName; min : 1; max : 1),
   (Name : 'GetDateFromYM';       Func : _GetDateFromYM; min : 2; max : 2),

   (Name : 'Round';               Func : _Round; min : 1; max : 2),
   (Name : 'Trunc';               Func : _Trunc; min : 1; max : 1),
   (Name : 'Min';                 Func : _Min; min : 1; max : -1),
   (Name : 'Max';                 Func : _Max; min : 1; max : -1),

   (Name : 'Copy';                Func : _Copy; min : 2; max : 3),
   (Name : 'MakeStr';             Func : _MakeStr; min : 1; max : 2),

   (Name : 'GSN';                 Func : _GSN; min : 1; max : 14),
   (Name : 'IsZero';              Func : _IsZero; min : 1; max : 1),
   (Name : 'IIF';                 Func : _IIF; min : 3; max : 3),
   (Name : 'IsNotZero';           Func : _IsNotZero; min : 1; max : 1),

   (Name : 'AnsiUpperCase';       Func : _AnsiUpperCase; min : 1; max : 1),
   (Name : 'AnsiLowerCase';       Func : _AnsiLowerCase; min : 1; max : 1),
   (Name : 'Trim';                Func : _Trim; min : 1; max : 1),

   (Name : 'UID';                 Func : _UID; min : 1; max : -1),

   (Name : 'In';                  Func : _In; min : 2; max : -1),
   (Name : 'AddLeft';             Func : _AddLeft; min : 3; max : 3),
   (Name : 'AddRight';            Func : _AddRight; min : 3; max : 3),
   (Name : 'Length';              Func : _Length; min : 1; max : 1),
   (Name : 'Null';                Func : _Null; min : 0; max : 0),

   (Name : 'Abs';                 Func : _Abs; min : 1; max :1),
   (Name : 'GetDay';              Func : _GetDay; min : 1; max :1)
  );

  ObjFuncInfo : array [1..prMAX_OBJFUNCTIONS] of TprObjFuncInfo=
  (
   (ObjClassRef : TDataSet; FuncName : 'IsNullField'; Func : _TDataSet_IsNullField; min : 1; max : 1),
   (ObjClassRef : TDataSet; FuncName : 'Locate';      Func : _TDataSet_Locate; min : 2; max : -1),
   (ObjClassRef : TDataSet; FuncName : 'Eof';         Func : _TDataSet_Eof; min : 0; max : 0),
   (ObjClassRef : TDataSet; FuncName : 'IsZeroField'; Func : _TDataSet_IsZeroField; min : 1; max : 1),
   (ObjClassRef : TDataSet; FuncName : 'LineNo';      Func : _TDataSet_LineNo; min : 0; max : 0),
   (ObjClassRef : TprGroup; FuncName : 'LineNo';      Func : _TprGroup_LineNo; min : 0; max : 0),
   (ObjClassRef : TprDataSet; FuncName : 'LineNo';      Func : _TprDataSet_LineNo; min : 0; max : 0),
   (ObjClassRef : TDataSet; FuncName : 'GetFieldValue'; Func : _TDataSet_GetFieldValue; min : 1; max : 1),
   (ObjClassRef : TprDataSet; FuncName : 'GetFieldValue';      Func : _TprDataSet_GetFieldValue; min : 1; max : 1),
   (ObjClassRef : TDataSet; FuncName : 'FieldDisplayText';      Func : _TDataSet_FieldDisplayText; min : 1; max : 1),
   (ObjClassRef : TprGroup; FuncName : 'GroupValue';      Func : _TprGroup_GroupValue; min : 0; max : 0)
   );

  ObjPropInfo : array [1..prMAX_OBJPROPS] of TprObjPropInfo=
  (
   (ObjClassRef : TDataSet; Func : _TDataSet_rProp; GetPropsNamesFunc : _TDataSet_PropsList),
   (ObjClassRef : TprCustomReport; Func : _TprCustomReport_rProp; GetPropsNamesFunc : _TprCustomReport_PropsList),
   (ObjClassRef : TprDataSet; Func : _TprDataSet_rProp; GetPropsNamesFunc : _TprDataSet_PropsList)
  );
  
implementation

uses
  pr_MultiLang, pr_Strings, vgr_Functions;

const
  sInternalError1 = 'Internal error at calculation of value of expression, expression - [%s].';

var
  i: integer;

type
  TprValueAccess = class(TprValue)
  end;

  TprValuesAccess = class(TprValues)
  end;

const
  prParserStringChar : char = '"';
  prParserStartBracketChar : char = '(';
  prParserEndBracketChar : char = ')';
  prParserFuncParamsDelim : char = ',';
  prParserUnaryPlusOperator : char = '+';
//  prParserUnaryMinusOperator : char = '-';
  prParserStartIdentChars : set of char = ['a'..'z','а'..'я','A'..'Z','А'..'Я'];
  prParserIdentChars : set of char = ['a'..'z','а'..'я','A'..'Z','А'..'Я','_','0'..'9','.','$','@'];
  prParserOperatorChars : set of char = ['!','>','<','=','+','-','*','/','&','|','^'];
  prParserNumberDateChars : set of char = ['0'..'9','.'];
  prParserUnaryOperators : set of char = ['+','-'];
  
  procedure _CenterStr(var b : string; c : integer);
  begin
  if Length(b)>c then
    b := Copy(b,1,c)
  else
    b := CenterStr(b,c);
  end;

  procedure _LeftStr(var b : string; c : integer);
  begin
  if Length(b)>c then
    b := Copy(b,1,c)
  else
    b := AddCharR(' ',b,c);
  end;

  procedure _RightStr(var b : string; c : integer);
  begin
  if Length(b)>c then
    b := Copy(b,Length(b)-c+1,Length(b))
  else
    b := AddChar(' ',b,c);
  end;

  function pFormat;
  var
    vDouble : double;
    lfFormat : TFloatFormat;
    lsFormatStr,_aDisplayFormat,lsString : String;
    i,p,pLMN,liDigits,liPrecision,liPos : integer;

    OldThSep : char;
    OldDecSep : char;
    OldLongMonthNames : array[1..12] of string;
  begin
  _aDisplayFormat := aDisplayFormat;

  OldDecSep := DecimalSeparator;
  p := pos('\ds',_aDisplayFormat);
  if (p<>0) and (p+3<=Length(_aDisplayFormat)) then
    begin
      DecimalSeparator := _aDisplayFormat[p+3];
      Delete(_aDisplayFormat,p,4);
    end;

  OldThSep := ThousandSeparator;
  p := pos('\ts',_aDisplayFormat);
  if (p<>0) and (p+3<=Length(_aDisplayFormat)) then
    begin
      ThousandSeparator := _aDisplayFormat[p+3];
      Delete(_aDisplayFormat,p,4);
    end;

  pLMN := pos('\lmn',_aDisplayFormat);
  if pLMN<>0 then
    begin
      for i:=1 to 12 do
        OldLongMonthNames[i] := LongMonthNames[i];
      for i:=1 to 12 do
        LongMonthNames[i] := MonthsArray[2,i];
      Delete(_aDisplayFormat,pLMN,4);
    end;

  case aValue.vType of
    prvvtInteger,prvvtDouble:
      begin
        lsFormatStr := _aDisplayFormat;
        vDouble := _vAsDouble(aValue);

        if Pos('$', _aDisplayFormat) <> 0 then
          begin
            if Pos('.', _aDisplayFormat) = 0 then
              liDigits := 0
            else
              begin
                liPos :=  Pos(';', _aDisplayFormat);
                if liPos > 0 then
                  lsString := Copy(_aDisplayFormat, 1, liPos-1)
                else
                  lsString := _aDisplayFormat;
                liDigits := Length(lsString) - Pos('.', lsString);
              end;
            lfFormat := ffCurrency;
            liPrecision := 15;
            lsString := FloatToStrF(vDouble, lfFormat, liPrecision, liDigits);
          end
        else
          if (_aDisplayFormat <> '') then
            lsString := FormatFloat(lsFormatStr, vDouble)
          else
            begin
              lfFormat := ffGeneral;
              liDigits := 0;
              liPrecision := 15;
              lsString := FloatToStrF(vDouble,lfFormat,liPrecision,liDigits);
            end;
        Result := lsString;
      end;

    prvvtDateTime:
      if Length(_aDisplayFormat) > 0 then
        Result := FormatDateTime(_aDisplayFormat, _vAsDateTime(aValue))
      else
        Result := FormatDateTime(ShortDateFormat, _vAsDateTime(aValue));

    prvvtString:
      if Length(_aDisplayFormat) > 0 then
         Result := FormatMaskText(_aDisplayFormat, _vAsString(aValue))
      else
         Result := _vAsString(aValue);

    prvvtBoolean:
      Result := _vAsString(aValue);

    else
      Result := '';
  end;

  if DecimalSeparator<>OldDecSep then
    DecimalSeparator := OldDecSep;
  if ThousandSeparator<>OldThSep then
    ThousandSeparator := OldThSep;
  if pLMN<>0 then
    for i:=1 to 12 do
      LongMonthNames[i] := OldLongMonthNames[i];
  end;

  function prParserTextToFloat(const S: string; var AValue: Extended): Boolean;
  var
    I: Integer;
    ABuf: string;
  begin
    ABuf := S;
    for I := 1 to Length(ABuf) do
      if (ABuf[I] = '.') or (ABuf[I] = ',') then
        ABuf[I] := DecimalSeparator;
    Result := TextToFloat(PChar(ABuf), AValue, fvExtended);
  end;

//////////////////////////////////////
//
// functions
//
//////////////////////////////////////
procedure VarFromField;
begin
if Field.IsNull then
  Res.vType := prvvtNull
else
  case Field.DataType of
    ftString,ftFixedChar,ftWideString :
      begin
        Res.vType := prvvtString;
        Res.vString := Field.AsString;
      end;
    ftSmallInt,ftInteger,ftWord,ftLargeInt,ftAutoinc:
      begin
        Res.vType := prvvtInteger;
        Res.vInteger := Field.AsInteger;
      end;
    ftFloat,ftBCD,ftCurrency{$IFDEF PR_D6_D7},ftFMTBcd{$ENDIF} :
      begin
        Res.vType := prvvtDouble;
        Res.vDouble := Field.AsFloat;
      end;
    ftBoolean:
      begin
        Res.vType := prvvtBoolean;
        Res.vBoolean := Field.AsBoolean;
      end;
    ftDate,ftDateTime,ftTime:
      begin
        Res.vType := prvvtDateTime;
        Res.vDateTime := Field.AsDateTime;
      end;
    ftMemo,ftfmtMemo,ftBlob:
      begin
        Res.vType := prvvtString;
        Res.vString := Field.AsString;
      end;
    else
      raise Exception.Create(prLoadStr(sParserErrorUnknownFieldType));
  end;
end;

procedure _vCopy;
begin
vDest.vType := vSource.vType;
vDest.vString := vSource.vString;
vDest.vDouble := vSource.vDouble;
end;

function _vIsNull;
begin
Result := v.vType=prvvtNull;
end;

function _vAsString;
begin
try
  case v.vType of
    prvvtString : Result := v.vString;
    prvvtInteger : Result := IntToStr(v.vInteger);
    prvvtDateTime : Result := DateTimeToStr(v.vDateTime);
    prvvtDouble : Result := FloatToStr(v.vDouble);
    prvvtBoolean : Result := BoolToStr(v.vBoolean);
    prvvtNull : Result := '';
    else raise Exception.Create(prLoadStr(sParserErrorErrorValueType));
  end;
except
  on E : Exception do
    raise Exception.CreateFmt(prLoadStr(sParserErrorCalcExpression),[E.Message]);
end
end;

function _vAsBoolean;
begin
try
  case v.vType of
    prvvtString : Result := StrToBool(v.vString);
    prvvtInteger : Result := v.vInteger<>0;
    prvvtDouble: Result := not IsZero(v.vDouble);
    prvvtBoolean : Result := v.vBoolean;
    prvvtNull : Result := false;
    else raise Exception.Create(prLoadStr(sParserErrorErrorValueType));
  end;
except
  on E : Exception do
    raise Exception.CreateFmt(prLoadStr(sParserErrorCalcExpression),[E.Message]);
end
end;

function _vAsInteger;
begin
try
  case v.vType of
    prvvtString : Result := StrToInt(v.vString);
    prvvtInteger : Result := v.vInteger;
    prvvtDouble : Result := ExtRound(v.vDouble);
    prvvtBoolean : Result := integer(v.vBoolean);
    prvvtNull : Result := 0;
    else raise Exception.Create(prLoadStr(sParserErrorErrorValueType));
  end;
except
  on E : Exception do
    raise Exception.CreateFmt(prLoadStr(sParserErrorCalcExpression),[E.Message]);
end
end;

function _vAsDouble;
begin
try
  case v.vType of
    prvvtString : Result := StrToFloat(v.vString);
    prvvtInteger : Result := v.vInteger;
    prvvtDouble : Result := v.vDouble;
    prvvtNull : Result := 0;
    else raise Exception.Create(prLoadStr(sParserErrorErrorValueType));
  end;
except
  on E : Exception do
    raise Exception.CreateFmt(prLoadStr(sParserErrorCalcExpression),[E.Message]);
end
end;

function _vAsDateTime;
begin
try
  case v.vType of
    prvvtString : Result := StrToDateTime(v.vString);
    prvvtDateTime : Result := v.vDateTime;
    prvvtNull : Result := 0;
    else raise Exception.Create(prLoadStr(sParserErrorErrorValueType));
  end;
except
  on E : Exception do
    raise Exception.CreateFmt(prLoadStr(sParserErrorCalcExpression),[E.Message]);
end
end;

function _vAsObject;
begin
try
  case v.vType of
    prvvtObject : Result := v.vObject;
    prvvtNull : Result := nil;
    else raise Exception.Create(prLoadStr(sParserErrorErrorValueType));
  end;
except
  on E : Exception do
    raise Exception.CreateFmt(prLoadStr(sParserErrorCalcExpression),[E.Message]);
end
end;

function _vAsVariant;
begin
try
  case v.vType of
    prvvtString : Result := v.vString;
    prvvtInteger : Result := v.vInteger;
    prvvtDateTime : Result := v.vDateTime;
    prvvtDouble : Result := v.vDouble;
    prvvtBoolean : Result := v.vBoolean;
    prvvtNull : Result := Null;
    else raise Exception.Create(prLoadStr(sParserErrorErrorValueType));
  end;
except
  on E : Exception do
    raise Exception.CreateFmt(prLoadStr(sParserErrorCalcExpression),[E.Message]);
end
end;

procedure _vSetNull;
begin
  v.vType := prvvtNull;
end;

procedure _vSetAsObject;
begin
  v.vType := prvvtObject;
  v.vObject := Value;
end;

procedure _vSetAsString;
begin
  v.vType := prvvtString;
  v.vString := Value;
end;

procedure _vSetAsDateTime;
begin
  v.vType := prvvtDateTime;
  v.vDateTime := Value;
end;

procedure _vSetAsDouble;
begin
  v.vType := prvvtDouble;
  v.vDouble := Value;
end;

procedure _vSetAsInteger;
begin
v.vType := prvvtInteger;
v.vInteger := Value;
end;

procedure _vSetAsBoolean;
begin
v.vType := prvvtBoolean;
v.vBoolean := Value;
end;

procedure _vSetAsType;
begin
case VarType of
  prvvtString  : v.vString := Value;
  prvvtDateTime : v.vDateTime := Value;
  prvvtInteger : v.vInteger := Value;
  prvvtBoolean : v.vBoolean := Value;
  prvvtDouble  : v.vDouble := Value
  else raise Exception.Create(prLoadStr(sParserErrorErrorValueType));
end;
v.vType:=VarType;
end;

procedure _vSetAsVariant;
begin
case VarType(Value) of
  varNull,
  varEmpty : _vSetNull(v);

  varSmallInt,
  varInteger,
  varByte: _vSetAsInteger(v,Value);

  varSingle,
  varDouble,
  varCurrency : _vSetAsDouble(v,Value);

  varDate : _vSetAsDateTime(v,Value);

  varString,
  varOleStr : _vSetAsString(v,Value);

  varBoolean : _vSetAsBoolean(v,Value);
  else raise Exception.Create(prLoadStr(sParserErrorErrorValueType));
end
end;

/////////////////////////////////////////////////
//
// TprExpr
//
/////////////////////////////////////////////////
constructor TprExpr.Create;
begin
inherited;
FList := TList.Create;
end;

destructor TprExpr.Destroy;
begin
Clear;
FList.Free;
inherited;
end;

function TprExpr.GetItm(i : integer) : pprExprItem;
begin
Result := pprExprItem(FList[i]);
end;

function TprExpr.GetCount : integer;
begin
Result := FList.Count;
end;

function TprExpr.GetLast : pprExprItem;
begin
if FList.Count=0 then
  Result := nil
else
  Result := Items[FList.Count-1];
end;

function TprExpr.Push : pprExprItem;
begin
GetMem(Result,sizeof(TprExprItem));
Initialize(Result.ObjName);
Initialize(Result.ObjLastName);
Initialize(Result.ObjValue.vString);
FList.Add(Result);
end;

function TprExpr.Pop : pprExprItem;
begin
with pprExprItem(FList[FList.Count-1])^ do
  begin
    Finalize(ObjName);
    Finalize(ObjLastName);
    Finalize(ObjValue.vString);
  end;
FreeMem(FList[i]);
FList.Delete(FList.Count-1);
Result := Last;
end;

procedure TprExpr.Reset;
begin
Clear;
end;

procedure TprExpr.Clear;
var
  i : integer;
begin
for i:=0 to FList.Count-1 do
  begin
    with pprExprItem(FList[i])^ do
      begin
        Finalize(ObjName);
        Finalize(ObjLastName);
        Finalize(ObjValue.vString);
      end;
    FreeMem(FList[i]);
  end;
FList.Clear;
end;

/////////////////////////////////////////////////
//
// TprCachedExpr
//
/////////////////////////////////////////////////
constructor TprCachedExpr.Create;
begin
inherited;
FCurPos := -1;
end;

function TprCachedExpr.GetCount : integer;
begin
Result := FCurPos+1;
end;

function TprCachedExpr.GetLast : pprExprItem;
begin
if FCurPos>=0 then
  Result := Items[FCurPos]
else
  Result := nil;
end;

function TprCachedExpr.Push : pprExprItem;
begin
Inc(FCurPos);
if FCurPos=FList.Count then
  begin
    GetMem(Result,sizeof(TprExprItem));
    Initialize(Result.ObjName);
    Initialize(Result.ObjLastName);
    Initialize(Result.ObjValue.vString);
    FList.Add(Result);
  end
else
  Result := Items[FCurPos];
end;

function TprCachedExpr.Pop : pprExprItem;
begin
Dec(FCurPos);
Result := Last;
end;

procedure TprCachedExpr.Reset;
begin
FCurPos := -1;
end;

procedure TprCachedExpr.Clear;
begin
inherited;
FCurPos := -1;
end;

/////////////////////////////////////////////////
//
// TprExprs
//
/////////////////////////////////////////////////
function TprExprs.GetItm(i : integer) : TprExpr;
begin
Result := TprExpr(inherited Items[i]);
end;

procedure TprExprs.Clear;
var
  i : integer;
begin
for i:=0 to Count-1 do
  Items[i].Free;
inherited;
end;

/////////////////////////////////////////////////
//
// TprParser
//
/////////////////////////////////////////////////
constructor TprParser.Create(AReport: TprCustomReport; AValues: TprValues; ASystemValues: TprValues);
begin
  inherited Create;
  FReport := AReport;
  FValues := AValues;
  FSystemValues := ASystemValues;
  
  LReportProps := TStringList.Create;
  ObjPropInfo[REPORT_PROPS_INFO_INDEX].GetPropsNamesFunc(FReport, LReportProps);
  SetLength(Stack, prDefaultStackSize);
  sStack := prDefaultStackSize;
  pStack := -1;

  FTempExpr := TprExpr.Create;
  FExprs := TprExprs.Create;
  FCompileOpStack := TprCachedExpr.Create;
end;

destructor TprParser.Destroy;
begin
  Finalize(Stack);
  LReportProps.Free;
  FExprs.Free;
  FCompileOpStack.Free;
  FTempExpr.Free;
  inherited;
end;

procedure TprParser.ClearInternalStructs;
begin
  FCompileOpStack.Clear;
  FExprs.Clear;
  FTempExpr.Clear;
end;

procedure TprParser.ResetStack;
begin
  pStack := -1;
end;

function TprParser.rStack;
begin
  if pStack<0 then
    raise Exception.Create(prLoadStr(sParserErrorStack));
  Result := @Stack[pStack];
  Dec(pStack);
end;

procedure TprParser.wStack(const Value : TprVarValue);
begin
  inc(pStack);
  if pStack >= sStack then
  begin
    sStack := sStack + prDefaultStackSize;
    SetLength(Stack, sStack);
  end;
  _vCopy(Value, Stack[pStack]);
end;

function TprParser.wStack : integer;
begin
  Inc(pStack);
  if pStack >= sStack then
  begin
    sStack := sStack + prDefaultStackSize;
    SetLength(Stack, sStack);
  end;
  Result := pStack;
end;

function TprParser.FindObjPropInfo;
var
  i : integer;
begin
i := 1;
while (i<=prMAX_OBJPROPS) and not (Component is ObjPropInfo[i].ObjClassRef) do Inc(i);
if i<=prMAX_OBJPROPS then
  Result := @ObjPropInfo[i]
else
  Result := nil;
end;

function TprParser.FindObjFuncInfo;
var
  i : integer;
begin
i := 1;
while (i<=prMAX_OBJFUNCTIONS) and
      not((Component is ObjFuncInfo[i].ObjClassRef) and
          (CompText(ObjFuncInfo[i].FuncName,FuncName)=0)) do Inc(i);
if i<=prMAX_OBJFUNCTIONS then
  Result := @ObjFuncInfo[i]
else
  Result := nil;
end;

procedure TprParser.SetError(const ErrorMessage : string);
begin
raise Exception.Create(ErrorMessage);
end;

procedure TprParser.CopyExprItem(Source,Dest : pprExprItem);
begin
Dest.ObjType := Source.ObjType;
Dest.ObjOpInfo := Source.ObjOpInfo;
Dest.ObjInfo := Source.ObjInfo;
Dest.ObjName := Source.ObjName;
Dest.ObjObject := Source.ObjObject;
Dest.ObjLastName := Source.ObjLastName;
Dest.ObjParCount := Source.ObjParCount;
Dest.ObjOperandExists := Source.ObjOperandExists;
_vCopy(Source.ObjValue,Dest.ObjValue);
end;

function TprParser.CompileExpression(const s: string; cExpr: TprExpr): Boolean;
var
  b1 : string;
  vd : extended;
  vdt : TDateTime;
  Last,LastFunction : pprExprItem;
  i, j, l, vi, valCode : integer;

  procedure AddOperator(o : pprExprItem);
  begin
  CopyExprItem(o,cExpr.Push);
  end;

  function GetOperatorIndex(const s : string) : integer;
  begin
  Result := 1;
  while (Result<=prMAX_CALCOPERATORS) and (AnsiCompareText(CO[Result].Name,s)<>0) do Inc(Result);
  if Result>prMAX_CALCOPERATORS then
    SetError(Format(prLoadStr(sParserErrorUnknownOperator),[s]));
  end;

  procedure FindLastFunction;
  var
    i : integer;
  begin
  i := FCompileOpStack.Count-1;
  while (i>=0) and not (FCompileOpStack[i].ObjType in [preiFunction,preiObjectFunction]) do Dec(i);
  if i>=0 then
    LastFunction := FCompileOpStack[i]
  else
    LastFunction := nil;
  end;

  function ProcessOperator(OperatorInfoIndex : integer) : pprExprItem;
  var
    oi : pprCalcOperatorInfo;
    Last : pprExprItem;
  begin
  Result := nil;
  oi := @CO[OperatorInfoIndex];

  Last := FCompileOpStack.Last;
  if (Last<>nil) and (oi.Priority<>0) and (oi.Priority<=Last.ObjOpInfo.Priority) then
    begin
      while (Last<>nil) and (Last.ObjOpInfo.Priority>=oi.Priority) do
        begin
          AddOperator(Last);
          Last := FCompileOpStack.Pop;
        end;
      FindLastFunction;
    end;

  if oi.Priority<>prParserEndBracketPriority then
    begin
      Result := FCompileOpStack.Push;
      with Result^ do
        begin
          ObjType := preiOperator;
          ObjOpInfo := oi;
          ObjParCount := 0;
          ObjOperandExists := false;
        end
    end
  else
    begin
      if Last=nil then
        SetError(prLoadStr(sParserErrorInvalidBrackets))
      else
        begin
          Last := FCompileOpStack.Pop;
          if Last<>nil then
            begin
              if Last.ObjOpInfo.Priority=prParserFunctionPriority then
                begin
                  // Last - function
                  LastFunction := Last;
                  if Last.ObjOperandExists then
                    Inc(Last.ObjParCount)
                  else
                    if Last.ObjParCount>0 then
                      begin
                        cExpr.Push^.ObjType := preiMissedFunctionParameter;
                        Inc(Last.ObjParCount)
                      end;
                end
              else
                FindLastFunction;
            end;
        end;
    end;
  end;

  procedure TranslateObjectName(const s : string; var Component : TComponent; var LastName : string);
  begin
  Report.TranslateObjectName(s,Component,LastName);
  if Component=nil then
    raise Exception.CreateFmt(prLoadStr(sParserErrorObjectNotFound),[s]);
  end;

begin
Result := true;
LastFunction := nil;
FCompileOpStack.Reset;
cExpr.Reset;
l := Length(s);
i := 1;
try
  while i<=l do
    begin
      if s[i] in prParserStartIdentChars then
        begin
          // identificator
          if LastFunction<>nil then
            LastFunction.ObjOperandExists := true;
  
          j := i;
          while (i<=l) and (s[i] in prParserIdentChars) do Inc(i);
          while (i<=l) and (s[i]<=#32) do Inc(i);
          b1 := Trim(Copy(s,j,i-j));
          if (i<=l) and (s[i]=prParserStartBracketChar) then
            begin
              // this is function
              LastFunction := ProcessOperator(prParserFunctionOperatorIndex);
              // getting function info
              if pos('.',b1)=0 then
                begin
                  LastFunction.ObjType := preiFunction;
                  LastFunction.ObjName := b1;
                  j := 1;
                  while (j<=prMAX_FUNCTIONS) and (AnsiCompareText(FuncInfo[j].Name,b1)<>0) do Inc(j);
                  if j>prMAX_FUNCTIONS then
                    // user defined function
                    LastFunction.ObjInfo := nil
                  else
                    LastFunction.ObjInfo := @FuncInfo[j];
                end
              else
                begin
                  LastFunction.ObjType := preiObjectFunction;
                  LastFunction.ObjName := b1;
                  TranslateObjectName(b1,LastFunction.ObjObject,LastFunction.ObjLastName);
                  LastFunction.ObjInfo := FindObjFuncInfo(LastFunction.ObjObject,LastFunction.ObjLastName);
                end;
            end
          else
            begin
              // determine a type of object
              with cExpr.Push^ do
                begin
                  j := SystemValues.IndexByName(b1);
                  if j<>-1 then
                    begin
                      ObjType := preiReportSystemVariable;
                      ObjName := TprValueAccess(SystemValues[j]).GetCurrentVersionID;
                      Result := false;
                    end
                  else
                    begin
                      j := Values.IndexByName(b1);
                      if j<>-1 then
                        begin
                          ObjType := preiReportVariable;
                          ObjName := TprValueAccess(Values[j]).GetCurrentVersionID;
                          Result := false;
                        end
                      else
                        begin
                          j := Report.Variables.IndexByName(b1);
                          if j<>-1 then
                            begin
                              ObjType := preiReportSimpleVariable;
                              ObjInfo := pointer(j);
                            end
                          else
                           begin
                             ObjName := b1;
                             if pos('.',b1)<>0 then
                               begin
                                 // object property
                                 ObjType := preiObjectProperty;
                                 TranslateObjectName(b1,ObjObject,ObjLastName);
                                 ObjInfo := FindObjPropInfo(ObjObject);
                               end
                             else
                               // var
                               ObjType := preiVar;
                           end;
                        end;
                    end;
                end;
            end;
        end
      else
      if (s[i]=prParserFuncParamsDelim) then
        begin
          // this is a function parameters delimeter
          if LastFunction=nil then
            SetError(prLoadStr(sParserErrorParameterWithoutFunction));
          if not LastFunction.ObjOperandExists then
            cExpr.Push^.ObjType := preiMissedFunctionParameter;
  
          // We should process all operators up to the first opening bracket
          Last := FCompileOpStack.Last;
          while Last.ObjOpInfo.Priority<>prParserStartBracketPriority do
            begin
              AddOperator(Last);
              Last := FCompileOpStack.Pop;
            end;
          FindLastFunction;
          with LastFunction^ do
            begin
              ObjOperandExists := false;
              Inc(ObjParCount);
            end;
          Inc(i);
        end
      else
      if s[i] in prParserOperatorChars then
        begin
          // operator
          j := i;
          while (i<=l) and (s[i] in prParserOperatorChars) do Inc(i);
          b1 := Copy(s,j,i-j);
          vi := Length(b1);
          if (vi>1) and (b1[vi] in prParserUnaryOperators) then
            begin
              ProcessOperator(GetOperatorIndex(Copy(b1,1,vi-1)));
              if b1[vi]=prParserUnaryPlusOperator then
                ProcessOperator(prParserUnaryPlusOperatorIndex).ObjParCount := 1
              else
                ProcessOperator(prParserUnaryMinusOperatorIndex).ObjParCount := 1;
            end
          else
            if (vi=1) and (b1[1] in prParserUnaryOperators) then
              begin
                // Probably it is a unary operator
                Dec(j);
                while (j>1) and (s[j]<=#32) do Dec(j);
                if (j<1) or (s[j] in [prParserStartBracketChar,prParserFuncParamsDelim]) then
                  begin
                    if b1[vi]=prParserUnaryPlusOperator then
                      ProcessOperator(prParserUnaryPlusOperatorIndex).ObjParCount := 1
                    else
                      ProcessOperator(prParserUnaryMinusOperatorIndex).ObjParCount := 1;
                  end
                else
                  ProcessOperator(GetOperatorIndex(b1)).ObjParCount := 2;
              end
            else
              ProcessOperator(GetOperatorIndex(b1)).ObjParCount := 2;
        end
      else
      if s[i]=prParserStartBracketChar then
        begin
          ProcessOperator(prParserStartBracketOperatorIndex);
          Inc(i);
        end
      else
      if s[i]=prParserEndBracketChar then
        begin
          ProcessOperator(prParserEndBracketOperatorIndex);
          Inc(i);
        end
      else
      if s[i] = prParserStringChar then
        begin
          // text string
          if LastFunction<>nil then
            LastFunction.ObjOperandExists := true;
  
          Inc(i);
          j := i;
          while (i<=l) and (s[i]<>prParserStringChar) do Inc(i);
          if i>l then
            SetError(prLoadStr(sParserErrorString));
          with cExpr.Push^ do
            begin
              ObjType := preiValue;
              _vSetAsString(ObjValue,Copy(s,j,i-j));
            end;
          Inc(i);
        end
      else
      if (s[i] in prParserNumberDateChars) then
        begin
          // number - integer or double
          if LastFunction<>nil then
            LastFunction.ObjOperandExists := true;

          j := i;
          while (i<=l) and (s[i] in prParserNumberDateChars) do Inc(i);
          b1 := Copy(s,j,i-j);

          val(b1, vi, valCode);
          if valCode = 0 then
            with cExpr.Push^ do
            begin
              ObjType := preiValue;
              _vSetAsInteger(ObjValue,vi);
            end
          else
            if prParserTextToFloat(b1, vd) then
              with cExpr.Push^ do
              begin
                ObjType := preiValue;
                _vSetAsDouble(ObjValue,vd);
              end
            else
              if TryStrToDateTime(S, vdt) then
                with cExpr.Push^ do
                begin
                   ObjType := preiValue;
                  _vSetAsDateTime(ObjValue,vdt);
                end
              else
                SetError(Format(prLoadStr(sParserErrorUnknownOperand),[b1]));
        end
      else
      if s[i]<=#32 then
        Inc(i)
      else
        raise Exception.Create(Format(prLoadStr(sParserErrorInvalidSymbol),[s[i]]));
    end;
  
  Last := FCompileOpStack.Last;
  while Last<>nil do
    begin
      if Last.ObjOpInfo.Priority=prParserStartBracketPriority then
        SetError(prLoadStr(sParserErrorInvalidBrackets));
      AddOperator(Last);
      Last := FCompileOpStack.Pop;
    end;
except
  on E : Exception do
    raise Exception.CreateFmt(prLoadStr(sParserErrorCompile),[s,E.Message])
end
end;

procedure TprParser.InternalReadParams(n : integer; var Vars : TprVarsArray);
var
  i : integer;
begin
SetLength(Vars,n);
for i:=n-1 downto 0 do
  _vCopy(rStack^,Vars[i]);
end;

procedure TprParser.InternalCheckParams(min,max,v : integer; const FuncName : string);
begin
if not (((min=-1) or (v>=min)) and ((max=-1) or (v<=max))) then
  raise Exception.CreateFmt(prLoadStr(sParserErrorInvalidFuncParametersCount),[FuncName,v,min,max]);
end;

procedure TprParser.InternalCalcFunction(e : pprExprItem);
var
  IsProcessed: Boolean;
  ATempVars: TprVarsArray;
  ATempValue: TprVarValue;
begin
  InternalReadParams(e.ObjParCount, ATempVars);
  if e.ObjInfo <> nil then
  begin
    InternalCheckParams(pprFuncInfo(e.ObjInfo).min,
                        pprFuncInfo(e.ObjInfo).max,
                        e.ObjParCount,
                        e.ObjName);
    pprFuncInfo(e.ObjInfo).Func(Self, ATempVars, Stack[wStack])
  end
else
  begin
    IsProcessed := false;
    if Assigned(Report.OnUnknownFunction) then
      begin
        Report.OnUnknownFunction(Report, e.ObjName, ATempVars, e.ObjParCount, ATempValue, IsProcessed);
        if IsProcessed then
          wStack(ATempValue)
      end;
    if not IsProcessed then
      raise Exception.CreateFmt(prLoadStr(sParserErrorUnknownFunction), [e.ObjName]);
  end;
end;

procedure TprParser.InternalCalcVar(e : pprExprItem);
var
  I: integer;
  IsProcessed: boolean;
  ATempValue: TprVarValue;
begin
i := 0;
while (i<LReportProps.Count) and (CompText(LReportProps[i], e.ObjName)<>0) do Inc(i);
if i>=LReportProps.Count then
  begin
    // this is unknown variable
    IsProcessed := false;
    if Assigned(Report.OnUnknownVariable) then
      begin
        Report.OnUnknownVariable(Report, e.ObjName, ATempValue, IsProcessed);
        if IsProcessed then
          wStack(ATempValue);
      end;
    if not IsProcessed then
      raise Exception.CreateFmt(prLoadStr(sParserErrorUnknownVariable),[e.ObjName]);
  end
else
  begin
    ObjPropInfo[REPORT_PROPS_INFO_INDEX].Func(Report,e.ObjName,ATempValue);
    wStack(ATempValue);
  end;
end;

procedure TprParser.InternalCalcObjectFunction(e : pprExprItem);
var
  IsProcessed: Boolean;
  ATempVars: TprVarsArray;
  ATempValue: TprVarValue;
begin
  InternalReadParams(e.ObjParCount, ATempVars);
  if e.ObjInfo <> nil then
  begin
    InternalCheckParams(pprObjFuncInfo(e.ObjInfo).min,
                        pprObjFuncInfo(e.ObjInfo).max,
                        e.ObjParCount,
                        e.ObjLastName);
    pprObjFuncInfo(e.ObjInfo).Func(Self, e.ObjObject, ATempVars, ATempValue)
  end
  else
  begin
    IsProcessed := false;
    if Assigned(Report.OnUnknownObjFunction) then
      begin
        Report.OnUnknownObjFunction(Report,e.ObjObject,e.ObjName, ATempVars,e.ObjParCount,ATempValue,IsProcessed);
        if IsProcessed then
          wStack(ATempValue)
      end;
    if not IsProcessed then
      raise Exception.CreateFmt(prLoadStr(sParserErrorUnknownObjectFunction),[e.ObjName]);
  end;
  _vCopy(ATempValue,Stack[wStack]);
end;

procedure TprParser.InternalCalcObjectProp(e : pprExprItem);
var
  IsProcessed: Boolean;
  ATempValue: TprVarValue;
begin
  if e.ObjInfo=nil then
  begin
    IsProcessed := false;
    if Assigned(Report.OnUnknownObjProp) then
      begin
        Report.OnUnknownObjProp(Report, e.ObjObject, e.ObjLastName, e.ObjName, ATempValue, IsProcessed);
        if IsProcessed then
          wStack(ATempValue)
      end;
    if not IsProcessed then
      raise Exception.CreateFmt(prLoadStr(sParserErrorUnknownProp),[e.ObjLastName, e.ObjObject.ClassName]);
  end
  else
    pprObjPropInfo(e.ObjInfo).Func(e.ObjObject, e.ObjLastName, ATempValue);

  _vCopy(ATempValue,Stack[wStack]);
end;

procedure TprParser.CalcExpression(Expr : TprExpr; var Res : TprVarValue);
var
  e : pprExprItem;
  v : TprVarValue;
  i : integer;
  ver : TprValueVersion;
begin
ResetStack;
try
  if Expr.Count=0 then
    begin
      _vSetAsBoolean(Res,false);
      exit;
    end;
  for i:=0 to Expr.Count-1 do
    begin
      e := Expr[i];
      case e.ObjType of
        preiValue:
          begin
            wStack(e.ObjValue);
          end;
        preiOperator:
          begin
            e.ObjOpInfo.Proc(Self,v);
            _vCopy(v,Stack[wStack]);
          end;
        preiFunction:
          begin
            InternalCalcFunction(e);
          end;
        preiReportVariable:
          begin
            // e.ObjName - identifier or var version
            ver := TprValuesAccess(Values).VersionByVersionID(e.ObjName);
            if ver <> nil then
            begin
              _vSetAsVariant(v, ver.VersionValue);
              wStack(v);
            end
            else
              raise Exception.CreateFmt(prLoadStr(sParserErrorUnknownReportVariableVersion),[e.ObjName]);
          end;
        preiReportSystemVariable:
          begin
            // e.ObjName - identifier or var version
            ver := TprValuesAccess(SystemValues).VersionByVersionID(e.ObjName);
            if ver <> nil then
            begin
              _vSetAsVariant(v, ver.VersionValue);
              wStack(v);
            end
            else
              raise Exception.CreateFmt(prLoadStr(sParserErrorUnknownReportVariableVersion),[e.ObjName]);
          end;
        preiReportSimpleVariable:
          begin
            with Report.Variables[integer(e.ObjInfo)] do
              begin
                if Calculated then
                  begin
                    CalcOnePass(Formula,v);
                    wStack(v);
                  end
                else
                  _vCopy(VarValue^,Stack[wStack]);
              end;
          end;
        preiVar:
          begin
            // var or report property
            InternalCalcVar(e);
          end;
        preiObjectFunction:
          begin
            InternalCalcObjectFunction(e);
          end;
        preiObjectProperty:
          begin
            InternalCalcObjectProp(e);
          end;
      end;
    end;
  _vCopy(rStack^,Res);
except
  on E : Exception do
    begin
      ResetStack;
      _vSetNull(Res);
      raise Exception.CreateFmt(prLoadStr(sParserErrorCalcExpression),[E.Message]);
    end;
end
end;

procedure TprParser.SaveExpression(Expr : TprExpr);
var
  e : pprExprItem;
  v : TprVarValue;
  i : integer;
  saveexpr : TprExpr;

  function ValidateParametersForOperation(n : integer) : boolean;
  var
    i : integer;
    v : pprVarValue;
  begin
  while (n>0) and (Stack[pStack-n+1].vType<>prvvtNotCalced) do Dec(n);
  Result := n=0;
  if not Result then
    begin
      // expression cannot be calced, moved current stack to saveexpr
      for i:= 0 to pStack do
        begin
          v := @Stack[i];
          if v.vType=prvvtNotCalced then
            begin
              if v^.vObject<>nil then
                CopyExprItem(pprExprItem(v^.vObject),saveexpr.Push);
            end
          else
            with saveexpr.Push^ do
              begin
                ObjType := preiValue;
                _vCopy(v^,ObjValue);
              end;
        end;
      CopyExprItem(e,saveexpr.Push);
      //
      ResetStack;
      with Stack[wStack] do
        begin
          vType := prvvtNotCalced;
          vObject := nil;
        end;
    end;
  end;

begin
ResetStack;
saveexpr := TprExpr.Create;
try
  for i:=0 to Expr.Count-1 do
    begin
      e := Expr[i];
      case e.ObjType of
        preiValue:
          begin
            wStack(e.ObjValue);
          end;
        preiOperator:
          begin
            if ValidateParametersForOperation(e.ObjParCount) then
              begin
                e.ObjOpInfo.Proc(Self,v);
                _vCopy(v,Stack[wStack]);
              end;
          end;
        preiFunction:
          begin
            if ValidateParametersForOperation(e.ObjParCount) then
              InternalCalcFunction(e);
          end;
        preiReportVariable,preiReportSystemVariable:
          begin
            v.vType := prvvtNotCalced;
            v.vObject := pointer(e);
            wStack(v);
          end;
        preiVar:
          begin
            // var or report property
            InternalCalcVar(e);
          end;
        preiObjectFunction:
          begin
            if ValidateParametersForOperation(e.ObjParCount) then
              InternalCalcObjectFunction(e);
          end;
        preiObjectProperty:
          begin
            InternalCalcObjectProp(e);
          end;
        preiReportSimpleVariable:
          begin
            with Report.Variables[integer(e.ObjInfo)] do
              begin
                if Calculated then
                  begin
                    CalcOnePass(Formula,v);
                    wStack(v);
                  end
                else
                  _vCopy(VarValue^,Stack[wStack]);
              end;
          end;
      end;
    end;

  if pStack=0 then
    with rStack^ do
      if vObject<>nil then
        CopyExprItem(pprExprItem(vObject),saveexpr.Push);
except
  on E : Exception do
    begin
      saveexpr.Free;
      ResetStack;
      raise Exception.CreateFmt(prLoadStr(sParserErrorCalcExpression),[E.Message]);
    end;
end;
// add saveexpr to FExprs
FExprs.Add(saveexpr);
end;

function TprParser.Calc(var Expr: string; var Res: TprVarValue): Boolean;
var
  I: Integer;
begin
  if (Length(Expr) > 1) and (Expr[1] = '$') then
  begin
    I := StrToInt(Copy(Expr, 2, Length(Expr)));
    if I >= FExprs.Count then
      raise Exception.CreateFmt(sInternalError1, [Expr]);

    CalcExpression(FExprs[I], Res);
    Result := True;
  end
  else
  begin
    Result := CompileExpression(Expr, FTempExpr);
    if Result then
      CalcExpression(FTempExpr, Res)
    else
    begin
      SaveExpression(FTempExpr);
      Expr := '$' + IntToStr(FExprs.Count - 1);
    end;
  end;
end;

procedure TprParser.CalcOnePass(const Expr : string; var Res : TprVarValue);
begin
if not CompileExpression(Expr,FTempExpr) then
  raise Exception.Create(prLoadStr(sErrorCalcExpressionInOnePass));
CalcExpression(FTempExpr,Res);
end;

function TprParser.FormatTemplateEx;
var
  v : TprVarValue;
  nv : Variant;
  Align : char;
  fRTF,fFormatNull,f : boolean;
  FullMask,FormatMask,b2 : string;
  iStartBrackets,i,j,k,Width,LenMask,Lenb2 : integer;

  procedure CheckChar;
  begin
  if i>LenMask then
    raise Exception.Create(prLoadStr(sParserErrorInvalidFormatString));
  end;

  procedure NextChar;
  begin
  Inc(i);
  CheckChar;
  end;

  procedure CheckCharb2;
  begin
  if j>Lenb2 then
    raise Exception.Create(prLoadStr(sParserErrorInvalidFormatString));
  end;

  procedure NextCharb2;
  begin
  Inc(j);
  CheckCharb2;
  end;

begin
Result := true;
try
  Res := '';
  i := 1;
  LenMask := Length(Mask);
  while i<=LenMask do
    begin
      if (Mask[i] = '[') and (i+1<=LenMask) then
        begin
          // get expression in brackets
          iStartBrackets := i;
          f := false;
          NextChar;
          while (i<=LenMask) and (f or (Mask[i]<>']')) do
            begin
              f := f xor (Mask[i]=prParserStringChar);
              Inc(i);
            end;
          if i>LenMask then
            break;
          b2 := Copy(Mask,iStartBrackets+1,i-iStartBrackets-1);
          Inc(i);

          // b2 - expression in brackets
          fRTF := false;
          Lenb2 := Length(b2);
          if Lenb2>0 then
            begin
              j := 1;
              FormatMask := '';
              FullMask := '';
              Width := 0;
              Align := 'r';
              fFormatNull := false;
              nv := 0;
              if b2[j]=':' then
                begin
                  // get format parameters
                  FullMask := ':';
                  NextCharb2;
                  // Null values
                  fFormatNull := b2[j] in ['n','N'];
                  if fFormatNull then
                    begin
                      FullMask := FullMask+'N';
                      NextCharb2;
                      case b2[j] of
                        'd','D': begin FullMask := FullMask+'D'; NextCharb2; nv := VarAsType(0,varDate); end;
                        's','S': begin FullMask := FullMask+'S'; NextCharb2; nv := ''; end;
                        'n','N': begin FullMask := FullMask+'N'; NextCharb2; nv := 0; end;
                        else begin FullMask := FullMask+'N'; nv := 0; end;
                      end;
                    end;
                  // width
                  k := j;
                  while (j<=Lenb2) and (b2[j] in ['0'..'9']) do Inc(j);
                  if k<>j then
                    begin
                      val(Copy(b2,k,j-k),Width,k);
                      FullMask := FullMask+IntToStr(Width);
                      CheckCharb2;
                      Align := b2[j];
                      FullMask := FullMask+Align;
                      NextCharb2;
                    end;
                  // FormatMask
                  if b2[j]='<' then
                    begin
                      FullMask := FullMask+'<';
                      NextCharb2;
                      k := j;
                      while (j<=Lenb2) and (b2[j]<>'>') do Inc(j);
                      FormatMask := Copy(b2,k,j-k);
                      FullMask := FullMask+FormatMask+'>';
                      NextCharb2;
                      if Length(FormatMask)=1 then
                        case FormatMask[1] of
                          'r','R' : FormatMask := SimpleCurrencyFormat;
                          's','S' : FormatMask := ShortCurrencyFormat;
                          'c','C' : FormatMask := CurrencyFormat;
                          'p','P' : FormatMask := PercentFormat;
                          'd','D' : FormatMask := ShortDateFormat;
                          'q','Q' : FormatMask := SpacedCurrencyFormat;
                          'b','B' : FormatMask := BankCurrencyFormat;
                        end
                      else
                        fRTF := AnsiCompareText(FormatMask,'rtf')=0;
                    end;
                end;

              b2 := Copy(b2,j,Lenb2);
              if Calc(b2,v) then
                begin
                  if fRTF then
                    b2 := _vAsString(v)
                  else
                    begin
                      if (fFormatNull or (v.vType<>prvvtNull)) then
                        begin
                          if v.vType=prvvtNull then
                            _vSetAsVariant(v,nv);
                          if FormatMask='' then b2 := _vAsString(v)
                                           else b2 := pFormat(FormatMask,v);

                          if Width<>0 then
                            begin
                              case Align of
                                'r','R': _RightStr(b2,Width);
                                'c','C': _CenterStr(b2,Width);
                                else _LeftStr(b2,Width);
                              end;
                            end;
                        end
                      else
                        b2 := '';
                    end;
                end
              else
                begin
                  b2 := '['+FullMask+b2+']';
                  Result := false;
                end;
            end;
          if Assigned(ReplaceCallBack) then
            begin
              if fRTF then
                ReplaceCallBack(iStartBrackets,i-iStartBrackets,PChar(b2),Length(b2),[prfrcRTF],CallBackData)
              else
                ReplaceCallBack(iStartBrackets,i-iStartBrackets,PChar(b2),Length(b2),[],CallBackData)
            end
          else
            Res := Res+b2
        end
      else
        if Mask[i] in ['\'] then
          begin
            iStartBrackets := i;
            NextChar;
            if Mask[i] in ['0'..'9'] then
              begin
                j := i;
                while (i<=Length(Mask)) and (Mask[i] in ['0'..'9']) do Inc(i);
                b2 := chr(StrToInt(Copy(Mask,j,i-j)));
              end
            else
              begin
                b2 := Mask[i];
                Inc(i);
              end;
            if Assigned(ReplaceCallBack) then
              ReplaceCallBack(iStartBrackets,i-iStartBrackets,PChar(b2),Length(b2),[],CallBackData)
            else
              Res := Res+b2
          end
        else
          begin
            if not Assigned(ReplaceCallBack) then
              Res := Res+Mask[i];
            Inc(i);
          end;
    end;
except
  on E : Exception do
    raise Exception.CreateFmt(prLoadStr(sParserErrorInFormatString),[i,E.Message]);
end;
end;

function TprParser.FormatTemplate;
begin
Result := FormatTemplateEx(Mask,nil,nil,Res);
end;

function TprParser.FormatStrings(lSource, lDest: TStrings; DeleteEmptyLines, DeleteEmptyLinesAtEnd: Boolean): Boolean;
var
  I: Integer;
  s, Buf: string;
begin
  Buf := '';
  Result := True;
  for I := 0 to lSource.Count - 1 do
  begin
    Result := FormatTemplate(lSource[I],s) and Result;
    // parse #13,#10
    if (Trim(s) <> '') or (not DeleteEmptyLines) then
    begin
      if Buf = '' then
        Buf := s
      else
        Buf := Buf + #13#10 + s;
    end;
  end;

  lDest.Text := Buf;

  if not DeleteEmptyLines and DeleteEmptyLinesAtEnd then
  begin
    while lDest.Count > 0 do
    begin
      if Trim(lDest[lDest.Count - 1]) <> '' then
        break;
      lDest.Delete(lDest.Count - 1);
    end;
  end;
end;

/////////////////////////////////////////////////
//
// OPERATORS
//
/////////////////////////////////////////////////
function IsEq(v1,v2 : double) : boolean;
begin
Result := Abs(v1-v2)<prDoublePrec;
end;

function NotIsEQ(v1,v2 : double) : boolean;
begin
Result:= Abs(v1-v2)>prDoublePrec;
end;

procedure _OpPlus;
var
  v1,v2 : PprVarValue;
begin
v2 := ML.rStack;
v1 := ML.rStack;
if (v1^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) and
   (v2^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) then
  begin
    if (v1^.vType=prvvtString) or (v2^.vType=prvvtString) then
      _vSetAsString(Res,_vAsString(v1^)+_vAsString(v2^))
    else
      if (v1^.vType=prvvtDouble) or (v2^.vType=prvvtDouble) then
        _vSetAsDouble(Res,_vAsDouble(v1^)+_vAsDouble(v2^))
      else
        if (v1^.vType=prvvtDateTime) and (v2^.vType=prvvtInteger) then
          _vSetAsDateTime(Res,IncDay(_vAsDateTime(v1^),_vAsInteger(v2^)))
        else
          if (v1^.vType=prvvtInteger) and (v2^.vType=prvvtDateTime) then
            _vSetAsDateTime(Res,IncDay(_vAsDateTime(v2^),_vAsInteger(v1^)))
          else
            if (v1^.vType=prvvtInteger) or (v2^.vType=prvvtInteger) then
              _vSetAsInteger(Res,_vAsInteger(v1^)+_vAsInteger(v2^))
            else
              if (v1^.vType=prvvtNull) and (v2^.vType=prvvtNull) then
                _vSetNull(Res)
              else
                raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpMinus;
var
  v1,v2 : PprVarValue;
begin
v2:=ML.rStack;
v1:=ML.rStack;
if (v1^.vType in [prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) and
   (v2^.vType in [prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) then
  begin
    if (v1^.vType=prvvtDateTime) and (v2^.vType=prvvtInteger) then
      _vSetAsDateTime(Res,IncDay(_vAsDateTime(v1^),_vAsInteger(v2^)))
    else
      if (v1^.vType=prvvtDateTime) and (v2^.vType=prvvtDateTime) then
        _vSetAsInteger(Res,DaysBetween(_vAsDateTime(v2^),_vAsDateTime(v1^)))
      else
        if (v1^.vType=prvvtDouble) or (v2^.vType=prvvtDouble) then
          _vSetAsDouble(Res,_vAsDouble(v1^)-_vAsDouble(v2^))
        else
          if (v1^.vType=prvvtInteger) or (v2^.vType=prvvtInteger) then
            _vSetAsInteger(Res,_vAsInteger(v1^)-_vAsInteger(v2^))
          else
            if (v1^.vType=prvvtNull) and (v2^.vType=prvvtNull) then
              _vSetNull(Res)
            else
              raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpMul;
var
  v1,v2 : PprVarValue;
begin
v2:=ML.rStack;
v1:=ML.rStack;
if (v1^.vType in [prvvtDouble,prvvtInteger,prvvtNull]) and
   (v2^.vType in [prvvtDouble,prvvtInteger,prvvtNull]) then
  begin
    if (v1^.vType=prvvtDouble) or (v2^.vType=prvvtDouble) then
      _vSetAsDouble(Res,_vAsDouble(v1^)*_vAsDouble(v2^))
    else
      if (v1^.vType=prvvtInteger) or (v2^.vType=prvvtInteger) then
        _vSetAsInteger(Res,_vAsInteger(v1^)*_vAsInteger(v2^))
      else
        if (v1^.vType=prvvtNull) and (v2^.vType=prvvtNull) then
          _vSetNull(Res)
        else
          raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpDiv;
var
  v1,v2 : PprVarValue;
begin
v2:=ML.rStack;
v1:=ML.rStack;
if (v1^.vType in [prvvtDouble,prvvtInteger,prvvtNull]) and
   (v2^.vType in [prvvtDouble,prvvtInteger,prvvtNull]) then
  begin
    if v2^.vType=prvvtNull then
      _vSetNull(Res)
    else
      if (v1^.vType=prvvtDouble) or (v2^.vType=prvvtDouble) then
        begin
          if _vAsDouble(v2^)=0 then
            _vSetNull(Res)
          else
            _vSetAsDouble(Res,_vAsDouble(v1^)/_vAsDouble(v2^))
        end
      else
        if (v1^.vType=prvvtInteger) or (v2^.vType=prvvtInteger) then
          begin
            if _vAsInteger(v2^)=0 then
              _vSetNull(Res)
            else
              _vSetAsDouble(Res,_vAsInteger(v1^)/_vAsInteger(v2^))
          end
        else
          raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

function __OpL(ML : TprParser; v1,v2 : PprVarValue) : boolean;
var
  vd1,vd2 : double;
begin
if (v1^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) and
   (v2^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) then
  begin
    if (v1^.vType=prvvtString) or (v2^.vType=prvvtString) then
      Result := _vAsString(v1^)<_vAsString(v2^)
    else
      if (v1^.vType=prvvtDouble) or (v2^.vType=prvvtDouble) then
        begin
          vd1 := _vAsDouble(v1^);
          vd2 := _vAsDouble(v2^);
          Result := NotIsEQ(vd1,vd2) and (vd1<vd2)
        end
      else
        if (v1^.vType=prvvtInteger) or (v2^.vType=prvvtInteger) then
          Result := _vAsInteger(v1^)<_vAsInteger(v2^)
        else
          if (v1^.vType=prvvtDateTime) or (v2^.vType=prvvtDateTime) then
            Result := _vAsDateTime(v1^)<_vAsDateTime(v2^)
          else
            if (v1^.vType=prvvtNull) and (v2^.vType=prvvtNull) then
              Result := false
            else
              raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpL;
var
  v1,v2 : PprVarValue;
begin
v2 := ML.rStack;
v1 := ML.rStack;
_vSetAsBoolean(Res,__OpL(ML,v1,v2));
end;

function __OpG(ML : TprParser; v1,v2 : PprVarValue) : boolean;
var
  vd1,vd2 : double;
begin
if (v1^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) and
   (v2^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) then
  begin
    if (v1^.vType=prvvtString) or (v2^.vType=prvvtString) then
      Result := _vAsString(v1^)>_vAsString(v2^)
    else
      if (v1^.vType=prvvtDouble) or (v2^.vType=prvvtDouble) then
        begin
          vd1 := _vAsDouble(v1^);
          vd2 := _vAsDouble(v2^);
          Result := NotIsEQ(vd1,vd2) and (vd1>vd2)
        end
      else
        if (v1^.vType=prvvtInteger) or (v2^.vType=prvvtInteger) then
          Result := _vAsInteger(v1^)>_vAsInteger(v2^)
        else
          if (v1^.vType=prvvtDateTime) or (v2^.vType=prvvtDateTime) then
            Result := _vAsDateTime(v1^)>_vAsDateTime(v2^)
          else
            if (v1^.vType=prvvtNull) and (v2^.vType=prvvtNull) then
              Result := false
            else
              raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpG;
var
  v1,v2 : PprVarValue;
begin
v2 := ML.rStack;
v1 := ML.rStack;
_vSetAsBoolean(Res,__OpG(ML,v1,v2));
end;

function __OpEQ(ML : TprParser; v1,v2 : PprVarValue) : boolean;
var
  vd1,vd2 : double;
begin
if (v1^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) and
   (v2^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) then
  begin
    if (v1^.vType=prvvtString) or (v2^.vType=prvvtString) then
      Result := _vAsString(v1^)=_vAsString(v2^)
    else
      if (v1^.vType=prvvtDouble) or (v2^.vType=prvvtDouble) then
        begin
          vd1 := _vAsDouble(v1^);
          vd2 := _vAsDouble(v2^);
          Result := IsEQ(vd1,vd2)
        end
      else
        if (v1^.vType=prvvtInteger) or (v2^.vType=prvvtInteger) then
          Result := _vAsInteger(v1^)=_vAsInteger(v2^)
        else
          if (v1^.vType=prvvtDateTime) or (v2^.vType=prvvtDateTime) then
            Result := _vAsDateTime(v1^)=_vAsDateTime(v2^)
          else
            if (v1^.vType=prvvtNull) and (v2^.vType=prvvtNull) then
              Result := true
            else
              raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpEQ;
var
  v1,v2 : PprVarValue;
begin
v2 := ML.rStack;
v1 := ML.rStack;
_vSetAsBoolean(Res,__OpEQ(ML,v1,v2));
end;

function __OpNEQ(ML : TprParser; v1,v2 : PprVarValue) : boolean;
var
  vd1,vd2 : double;
begin
if (v1^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) and
   (v2^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) then
  begin
    if (v1^.vType=prvvtString) or (v2^.vType=prvvtString) then
      Result := _vAsString(v1^)<>_vAsString(v2^)
    else
      if (v1^.vType=prvvtDouble) or (v2^.vType=prvvtDouble) then
        begin
          vd1 := _vAsDouble(v1^);
          vd2 := _vAsDouble(v2^);
          Result := NotIsEQ(vd1,vd2)
        end
      else
        if (v1^.vType=prvvtInteger) or (v2^.vType=prvvtInteger) then
          Result := _vAsInteger(v1^)<>_vAsInteger(v2^)
        else
          if (v1^.vType=prvvtDateTime) or (v2^.vType=prvvtDateTime) then
            Result := _vAsDateTime(v1^)<>_vAsDateTime(v2^)
          else
            if (v1^.vType=prvvtNull) and (v2^.vType=prvvtNull) then
              Result := false
            else
              raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpNEQ;
var
  v1,v2 : PprVarValue;
begin
v2 := ML.rStack;
v1 := ML.rStack;
_vSetAsBoolean(Res,__OpNEQ(ML,v1,v2));
end;

function __OpLEQ(ML : TprParser; v1,v2 : PprVarValue) : boolean;
var
  vd1,vd2 : double;
begin
if (v1^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) and
   (v2^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) then
  begin
    if (v1^.vType=prvvtString) or (v2^.vType=prvvtString) then
      Result := _vAsString(v1^)<=_vAsString(v2^)
    else
      if (v1^.vType=prvvtDouble) or (v2^.vType=prvvtDouble) then
        begin
          vd1 := _vAsDouble(v1^);
          vd2 := _vAsDouble(v2^);
          Result := IsEQ(vd1,vd2) or (vd1<vd2)
        end
      else
        if (v1^.vType=prvvtInteger) or (v2^.vType=prvvtInteger) then
          Result := _vAsInteger(v1^)<=_vAsInteger(v2^)
        else
          if (v1^.vType=prvvtDateTime) or (v2^.vType=prvvtDateTime) then
            Result := _vAsDateTime(v1^)<=_vAsDateTime(v2^)
          else
            if (v1^.vType=prvvtNull) and (v2^.vType=prvvtNull) then
              Result := true
            else
              raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpLEQ;
var
  v1,v2 : PprVarValue;
begin
v2 := ML.rStack;
v1 := ML.rStack;
_vSetAsBoolean(Res,__OpLEQ(ML,v1,v2));
end;

function __OpGEQ(ML : TprParser; v1,v2 : PprVarValue) : boolean;
var
  vd1,vd2 : double;
begin
if (v1^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) and
   (v2^.vType in [prvvtString,prvvtDouble,prvvtDateTime,prvvtInteger,prvvtNull]) then
  begin
    if (v1^.vType=prvvtString) or (v2^.vType=prvvtString) then
      Result := _vAsString(v1^)>=_vAsString(v2^)
    else
      if (v1^.vType=prvvtDouble) or (v2^.vType=prvvtDouble) then
        begin
          vd1 := _vAsDouble(v1^);
          vd2 := _vAsDouble(v2^);
          Result := IsEQ(vd1,vd2) or (vd1>vd2)
        end
      else
        if (v1^.vType=prvvtInteger) or (v2^.vType=prvvtInteger) then
          Result := _vAsInteger(v1^)>=_vAsInteger(v2^)
        else
          if (v1^.vType=prvvtDateTime) or (v2^.vType=prvvtDateTime) then
            Result := _vAsDateTime(v1^)>=_vAsDateTime(v2^)
          else
            if (v1^.vType=prvvtNull) and (v2^.vType=prvvtNull) then
              Result := true
            else
              raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpGEQ;
var
  v1,v2 : PprVarValue;
begin
v2 := ML.rStack;
v1 := ML.rStack;
_vSetAsBoolean(Res,__OpGEQ(ML,v1,v2));
end;

procedure _OpAnd;
var
  v1,v2 : PprVarValue;
begin
v2:=ML.rStack;
v1:=ML.rStack;
if (v1^.vType in [prvvtBoolean,prvvtInteger]) and
   (v2^.vType in [prvvtBoolean,prvvtInteger]) then
  begin
    if (v1^.vType=prvvtBoolean) and (v2^.vType=prvvtBoolean) then
      _vSetAsBoolean(Res,v1^.vBoolean and v2^.vBoolean)
    else
      if (v1^.vType=prvvtInteger) and (v2^.vType=prvvtInteger) then
        _vSetAsInteger(Res,v1^.vInteger and v2^.vInteger)
      else
        raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpOr;
var
  v1,v2 : PprVarValue;
begin
v2:=ML.rStack;
v1:=ML.rStack;
if (v1^.vType in [prvvtBoolean,prvvtInteger]) and
   (v2^.vType in [prvvtBoolean,prvvtInteger]) then
  begin
    if (v1^.vType=prvvtBoolean) and (v2^.vType=prvvtBoolean) then
      _vSetAsBoolean(Res,v1^.vBoolean or v2^.vBoolean)
    else
      if (v1^.vType=prvvtInteger) and (v2^.vType=prvvtInteger) then
        _vSetAsInteger(Res,v1^.vInteger or v2^.vInteger)
      else
        raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpNot;
var
  v1 : PprVarValue;
begin
v1:=ML.rStack;
if v1^.vType in [prvvtBoolean, prvvtInteger, prvvtNull] then
  begin
    if v1^.vType = prvvtBoolean then
      _vSetAsBoolean(Res,not v1^.vBoolean)
    else
      if v1^.vType = prvvtInteger then
        _vSetAsInteger(Res,not v1^.vInteger)
      else
        if v1^.vType = prvvtNull then
          _vSetNull(Res);
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpPower(ML : TprParser; var Res : TprVarValue);
var
  v1,v2 : PprVarValue;
begin
v2 := ML.rStack;
v1 := ML.rStack;
if (v1^.vType in [prvvtDouble,prvvtInteger,prvvtNull]) and
   (v2^.vType in [prvvtDouble,prvvtInteger,prvvtNull]) then
  begin
    if (v1^.vType=prvvtDouble) or (v2^.vType=prvvtDouble) then
      _vSetAsDouble(Res,Power(_vAsDouble(v1^),_vAsDouble(v2^)))
    else
      if (v1^.vType=prvvtInteger) or (v2^.vType=prvvtInteger) then
        _vSetAsInteger(Res,Round(Power(_vAsInteger(v1^),_vAsInteger(v2^))))
      else
        if (v1^.vType=prvvtNull) and (v2^.vType=prvvtNull) then
          _vSetNull(Res)
        else
          raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

procedure _OpUPlus(ML : TprParser; var Res : TprVarValue);
var
  v : PprVarValue;
begin
v := ML.rStack;
_vCopy(v^,Res);
end;

procedure _OpUMinus(ML : TprParser; var Res : TprVarValue);
var
  v : PprVarValue;
begin
v := ML.rStack;
if v^.vType in [prvvtDouble,prvvtInteger,prvvtNull] then
  begin
    if v^.vType=prvvtDouble then
      _vSetAsDouble(Res,-_vAsDouble(v^))
    else
      if v^.vType=prvvtInteger then
        _vSetAsDouble(Res,-_vAsInteger(v^))
      else
        if v^.vType=prvvtNull then
          _vSetNull(Res)
        else
          raise Exception.Create(prLoadStr(sParserErrorIncompatibleTypes));
  end
else
  raise Exception.Create(prLoadStr(sParserErrorInvalidTypes));
end;

/////////////////////////////////////////////////
//
// Functions
//
/////////////////////////////////////////////////
procedure _DateTime;
begin
_vSetAsDateTime(Res,Now);
end;

procedure _Time;
begin
_vSetAsDateTime(Res,Time);
end;

procedure _AnsiUpperCase;
begin
_vSetAsString(Res,AnsiUpperCase(_vAsString(Parameters[0])));
end;

procedure _AnsiLowerCase;
begin
_vSetAsString(Res,AnsiLowerCase(_vAsString(Parameters[0])));
end;

procedure _Trim;
begin
_vSetAsString(Res,Trim(_vAsString(Parameters[0])));
end;

//
// Adds given character at the left to string up to given length
// 0 - string
// 1 - character
// 2 - length
//
procedure _AddLeft;
begin
_vSetAsString(Res,AddChar(_vAsString(Parameters[1])[1],_vAsString(Parameters[0]),_vAsInteger(Parameters[2])));
end;

//
// Adds given character at the right to string up to given length
// 0 - string
// 1 - character
// 2 - length
//
procedure _AddRight;
begin
_vSetAsString(Res,AddCharR(_vAsString(Parameters[1])[1],_vAsString(Parameters[0]),_vAsInteger(Parameters[2])));
end;

procedure _Length;
begin
_vSetAsInteger(Res,Length(_vAsString(Parameters[0])));
end;

procedure _UID;
var
  s : string;
  i : integer;
begin
s := _vAsString(Parameters[0]);
for i:=1 to High(Parameters) do
  s := s+#01+_vAsString(Parameters[i]);
_vSetAsString(Res,s);
end;

procedure _In;
var
  i,hi : integer;
  sBuf : string;
  bBuf : boolean;
  iBuf : integer;
  dtBuf : TDateTime;
  dBuf : double;
  oBuf : TObject;
begin
hi := High(Parameters);
i := 1;
case Parameters[0].vType of
  prvvtString   :
    begin
      sBuf:=_vAsString(Parameters[0]);
      while (i<=hi) and (sBuf<>_vAsString(Parameters[i])) do Inc(i);
    end;
  prvvtBoolean  :
    begin
      bBuf:=_vAsBoolean(Parameters[0]);
      while (i<=hi) and (bBuf=_vAsBoolean(Parameters[i])) do Inc(i);
    end;
  prvvtInteger  :
    begin
      iBuf:=_vAsInteger(Parameters[0]);
      while (i<=hi) and (iBuf<>_vAsInteger(Parameters[i])) do Inc(i);
    end;
  prvvtDateTime :
    begin
      dtBuf:=_vAsDateTime(Parameters[0]);
      while (i<=hi) and (dtBuf<>_vAsDateTime(Parameters[i])) do Inc(i);
    end;
  prvvtDouble   :
    begin
      dBuf:=_vAsDouble(Parameters[0]);
      while (i<=hi) and (not IsEQ(dBuf,_vAsDouble(Parameters[i]))) do Inc(i);
    end;
  prvvtObject   :
    begin
      oBuf:=_vAsObject(Parameters[0]);
      while (i<=hi) and (oBuf<>_vAsObject(Parameters[i])) do Inc(i);
    end;
end;
_vSetAsBoolean(Res,i<=hi);
end;

procedure _Null;
begin
_vSetNull(Res);
end;

procedure _Abs;
begin
if Parameters[0].vType=prvvtInteger then
  _vSetAsInteger(Res,Abs(_vAsInteger(Parameters[0])))
else
  _vSetAsDouble(Res,Abs(_vAsDouble(Parameters[0])));
end;

procedure _IncYM;
begin
_vSetAsInteger(Res,IncYM(_vAsInteger(Parameters[0]),_vAsInteger(Parameters[1])));
end;

procedure _GetYM;
var
  y,m,d : word;
begin
if Parameters[0].vType=prvvtDateTime then
  begin
    DecodeDate(_vAsDateTime(Parameters[0]),y,m,d);
    _vSetAsInteger(Res,m+y*100);
  end
else
  _vSetAsInteger(Res,_vAsInteger(Parameters[0])+_vAsInteger(Parameters[1])*100);
end;

procedure _GetDateFromYM;
begin
_vSetAsDateTime(Res,StrToDate(Format('%d.%d.%d',[_vAsInteger(Parameters[1]),
                                                 _vAsInteger(Parameters[0]) mod 100,
                                                 _vAsInteger(Parameters[0]) div 100])));
end;

procedure _HourBetween;
var
  dd1,dd2 : TDateTime;
  tt1,tt2 : string;
  d1,d2   : TDateTime;
begin
dd1 := _vAsDateTime(Parameters[0]);
dd2 := _vAsDateTime(Parameters[1]);
if High(Parameters)>=2 then tt1 := _vAsString(Parameters[2])
                       else tt1 := '00:00';
if High(Parameters)>=3 then tt2 := _vAsString(Parameters[3])
                       else tt2 := '00:00';

d1 := StrToDateTime(Format('%s %s:00',[DateToStr(CutTime(dd1)),tt1]));
d2 := StrToDateTime(Format('%s %s:00',[DateToStr(CutTime(dd2)),tt2]));
_vSetAsInteger(Res,integer(ExtRound((d2-d1)*24)));
end;

procedure _MonthsBetween;
begin
_vSetAsInteger(Res,integer(Trunc(MonthsBetween(_vAsDateTime(Parameters[0]),_vAsDateTime(Parameters[1])))));
end;

//
// 0 - первая дата меньшая
// 1 - вторая дата большая
//
procedure _DaysBetween;
begin
_vSetAsInteger(Res,DaysBetween(_vAsDateTime(Parameters[0]),_vAsDateTime(Parameters[1])));
end;

procedure _Round;
begin
if High(Parameters)>=1 then
  _vSetAsDouble(Res,RoundEx(_vAsDouble(Parameters[0]),_vAsInteger(Parameters[1])))
else
  _vSetAsDouble(Res,RoundEx(_vAsDouble(Parameters[0]), 2));
end;

procedure _Trunc;
begin
_vSetAsInteger(Res,Trunc(_vAsDouble(Parameters[0])))
end;

procedure _GetMonth;
var
  d,m,y : Word;
begin
if Parameters[0].vType in [prvvtInteger] then
  begin
    _vSetAsInteger(Res,_vAsInteger(Parameters[0]) mod 100);
  end
else
  begin
    DecodeDate(_vAsDateTime(Parameters[0]),y,m,d);
    _vSetAsInteger(Res,m);
  end;
end;

procedure _GetDay;
var
  d,m,y : Word;
begin
DecodeDate(_vAsDateTime(Parameters[0]),y,m,d);
_vSetAsInteger(Res,d);
end;

procedure _GetYear;
var
  d,m,y : Word;
begin
if Parameters[0].vType in [prvvtInteger] then
  begin
    _vSetAsInteger(Res,_vAsInteger(Parameters[0]) div 100);
  end
else
  begin
    DecodeDate(_vAsDateTime(Parameters[0]),y,m,d);
    _vSetAsInteger(Res,y);
  end;
end;

procedure _GetFirstDayMonth;
var
  y,m,d : word;
begin
if High(Parameters)=0 then
  begin
    if Parameters[0].vType in [prvvtInteger] then
      begin
        y:=_vAsInteger(Parameters[0]) div 100;
        m:=_vAsInteger(Parameters[0]) mod 100;
      end
    else
      begin
        DecodeDate(_vAsDateTime(Parameters[0]),y,m,d);
      end;
  end
else
  if High(Parameters)=1 then
    begin
      m:=_vAsInteger(Parameters[0]);
      y:=_vAsInteger(Parameters[1]);
    end;

_vSetAsDateTime(Res,GetFirstDayMonth(m,y));
end;

procedure _GetLastDayMonth;
var
  y,m,d : word;
begin
if High(Parameters)=0 then
  begin
    if Parameters[0].vType in [prvvtInteger] then
      begin
        y:=_vAsInteger(Parameters[0]) div 100;
        m:=_vAsInteger(Parameters[0]) mod 100;
      end
    else
      begin
        DecodeDate(_vAsDateTime(Parameters[0]),y,m,d);
      end;
  end
else
  if High(Parameters)=1 then
    begin
      m:=_vAsInteger(Parameters[0]);
      y:=_vAsInteger(Parameters[1]);
    end;

_vSetAsDateTime(Res,GetLastDayMonth(m,y));
end;

procedure _GetMonthName;
begin
_vSetAsString(Res,GetMonthName(_vAsInteger(Parameters[0])));
end;

procedure _IncMonth;
begin
_vSetAsDateTime(Res,IncMonth(_vAsDateTime(Parameters[0]),_vAsInteger(Parameters[1])))
end;

procedure _IncDay;
begin
_vSetAsDateTime(Res,IncDay(_vAsDateTime(Parameters[0]),_vAsInteger(Parameters[1])))
end;

procedure _Min;
var
  i : integer;
begin
_vCopy(Parameters[0],Res);
for i:=1 to High(Parameters) do
  if __OpL(ML,@(Parameters[i]),@Res) then
    _vCopy(Parameters[i],Res);
end;

procedure _Max;
var
  i : integer;
begin
_vCopy(Parameters[0],Res);
for i:=1 to High(Parameters) do
  if __OpG(ML,@(Parameters[i]),@Res) then
    _vCopy(Parameters[i],Res);
end;

procedure _IIF;
begin
if _vAsBoolean(Parameters[0]) then
  _vCopy(Parameters[1],Res)
else
  _vCopy(Parameters[2],Res)
end;

procedure _IsZero;
begin
_vSetAsBoolean(Res,(Parameters[0].vType=prvvtNull) or (_vAsInteger(Parameters[0])=0));
end;

procedure _IsNotZero;
begin
_vSetAsBoolean(Res,(Parameters[0].vType<>prvvtNull) and (_vAsInteger(Parameters[0])<>0));
end;

procedure _GSN;
var
  Number : extended;
  NumberType : TNumberType;
  CentsFormat : TCentsFormat;
  CentsText : string;
  Gender : TGender;
  sOne : string;
  sTwoToFour : string;
  sOverFour : string;
  sOneRUB,sTwoToFourRUB,sOverFourRUB,sOneKOP,sTwoToFourKOP,sOverFourKOP : string;
begin
Number:=_vAsDouble(Parameters[0]);
if High(Parameters)>=1 then NumberType:=TNumberType(_vAsInteger(Parameters[1]))
                       else NumberType:=ntCurrency;
if High(Parameters)>=2 then CentsFormat:=TCentsFormat(_vAsInteger(Parameters[2]))
                       else CentsFormat:=cfTwoDigit;
if High(Parameters)>=3 then CentsText:=_vAsString(Parameters[3])
                       else CentsText:='коп.';
if High(Parameters)>=4 then Gender:=TGender(_vAsInteger(Parameters[4]))
                       else Gender:=gMale;
if High(Parameters)>=5 then sOne:=_vAsString(Parameters[5])
                       else sOne:='';
if High(Parameters)>=6 then sTwoToFour:=_vAsString(Parameters[6])
                       else sTwoToFour:='';
if High(Parameters)>=7 then sOverFour:=_vAsString(Parameters[7])
                       else sOverFour:='';
if High(Parameters)>=8 then sOneRUB:=_vAsString(Parameters[8])
                       else sOneRUB:='рубль';
if High(Parameters)>=9 then sTwoToFourRUB:=_vAsString(Parameters[9])
                       else sTwoToFourRUB:='рубля';
if High(Parameters)>=10 then sOverFourRUB:=_vAsString(Parameters[10])
                        else sOverFourRUB:='рублей';
if High(Parameters)>=11 then sOneKOP:=_vAsString(Parameters[11])
                        else sOneKOP:='копейка';
if High(Parameters)>=12 then sTwoToFourKOP:=_vAsString(Parameters[12])
                        else sTwoToFourKOP:='копейки';
if High(Parameters)>=13 then sOverFourKOP:=_vAsString(Parameters[13])
                        else sOverFourKOP:='копеек';
_vSetAsString(Res,GSN(Number,NumberType,CentsFormat,CentsText,Gender,sOne,sTwoToFour,sOverFour,sOneRUB,sTwoToFourRUB,sOverFourRUB,sOneKOP,sTwoToFourKOP,sOverFourKOP));
end;

procedure _Copy;
begin
if High(Parameters)>=2 then
  _vSetAsString(Res,Copy(_vAsString(Parameters[0]),_vAsInteger(Parameters[1]),_vAsInteger(Parameters[2])))
else
  _vSetAsString(Res,Copy(_vAsString(Parameters[0]),_vAsInteger(Parameters[1]),Length(_vAsString(Parameters[0]))))
end;

procedure _MakeStr;
var
  c : char;
begin
if (High(Parameters)>=1) and (Length(_vAsString(Parameters[1]))>=1) then
  c:=_vAsString(Parameters[1])[1]
else
  c:=' ';
_vSetAsString(Res,MakeStr(c,_vAsInteger(Parameters[0])));
end;

/////////////////////////////////////////////
//
// properties of classes
//
/////////////////////////////////////////////
procedure _TDataSet_PropsList(C : TObject; L : TStringList);
begin
  L.Clear;
  L.Add('RecordCount');
  L.Add('RecNo');
end;

procedure _TprCustomReport_PropsList(C : TObject; L : TStringList);
begin
  L.Clear;
  L.Add('StartDateTime');
end;

procedure _TDataSet_rProp(C: TObject; const PropName: string; var Res: TprVarValue);
var
  f : TField;
begin
with TDataSet(c) do
  begin
    if CompText(PropName,'RecordCount')=0 then
      begin
        _vSetAsInteger(Res,RecordCount);
      end
    else
      if CompText(PropName,'RecNo')=0 then
        begin
          _vSetAsInteger(Res,RecNo);
        end
      else
        begin
          f := FindField(PropName);
          if f<>nil then
            VarFromField(f,Res)
          else
            raise Exception.CreateFmt(prLoadStr(sParserErrorUnknownProp),[PropName,c.ClassName]);
        end
  end;
end;

procedure _TprCustomReport_rProp;
begin
with TprCustomReport(c) do
  begin
    if CompText(PropName,'StartDateTime')=0 then
      _vSetAsDateTime(res,StartDateTime)
    else
      raise Exception.CreateFmt(prLoadStr(sParserErrorUnknownProp),[PropName,c.ClassName]);
  end;
end;

procedure _TprDataSet_rProp;
var
  v : Variant;
begin
with TprDataSet(c) do
  begin
    if CompText(PropName,'RecordCount')=0 then
      begin
        _vSetAsInteger(Res,RecordCount);
      end
    else
      begin
        v := GetFieldValue(PropName);
        if VarIsEmpty(v) then
          raise Exception.CreateFmt(prLoadStr(sParserErrorUnknownProp),[PropName,c.ClassName])
        else
          _vSetAsVariant(Res,v);
      end
  end;
end;

procedure _TprDataSet_PropsList(C : TObject; L : TStringList);
begin
  L.Clear;
  L.Add('RecordCount');
end;

//////////////////////////////////////
//
// classes methods 
//
//////////////////////////////////////
procedure _TDataSet_Locate;
var
  i : integer;
  s : string;
  va : array of Variant;
begin
if Length(Parameters) mod 2 <>0 then
  raise Exception.Create(prLoadStr(sParserErrorParamsCountForLocate));

if Length(Parameters)=2 then
  _vSetAsBoolean(Res,TDataSet(c).Locate(_vAsString(Parameters[0]),_vAsVariant(Parameters[1]),[]))
else
  begin
    SetLength(va,Length(Parameters) div 2);
    s := _vAsString(Parameters[0]);
    va[0] := _vAsVariant(Parameters[1]);
    
    i := 2;
    while i<Length(Parameters) do
      begin
        s := s+';'+_vAsString(Parameters[i]);
        Inc(i);
        va[i div 2]:=_vAsVariant(Parameters[i]);
        Inc(i);
      end;
    _vSetAsBoolean(Res,TDataSet(c).Locate(s,VarArrayOf(va),[]));
  end;
end;

procedure _TDataSet_Eof;
begin
_vSetAsBoolean(Res,TDataSet(C).Eof);
end;

procedure _TDataSet_IsNullField;
begin
_vSetAsBoolean(Res,TDataSet(C).FieldByName(_vAsString(Parameters[0])).IsNull);
end;

procedure _TDataSet_IsZeroField;
var
  f : TField;
begin
f := TDataSet(C).FieldByName(_vAsString(Parameters[0]));
if f.IsNull then
  _vSetAsBoolean(Res,true)
else
  begin
    if f.DataType in [ftFloat,ftCurrency,ftBCD] then
      _vSetAsBoolean(Res,Abs(f.AsFloat)<prDoublePrec)
    else
      _vSetAsBoolean(Res,f.AsVariant=0);
  end;
end;

procedure _TDataSet_LineNo;
begin
_vSetAsInteger(Res,ML.Report.GetDataSetRecNo(C));
end;

procedure _TprDataset_LineNo;
begin
_vSetAsInteger(Res,ML.Report.GetDataSetRecNo(C));
end;

procedure _TDataSet_GetFieldValue;
begin
VarFromField(TDataSet(C).FieldByName(_vAsString(Parameters[0])),Res)
end;

procedure _TDataSet_FieldDisplayText;
begin
_vSetAsString(Res,TDataSet(C).FieldByName(_vAsString(Parameters[0])).DisplayText);
end;

procedure _TprGroup_LineNo;
begin
_vSetAsInteger(Res,TprGroup(C).LineNo);
end;

procedure _TprGroup_GroupValue(ML: TprParser; C: TComponent; Parameters: TprVarsArray; var Res: TprVarValue);
begin
  _vSetAsVariant(Res, TprGroup(C).GroupValue);
end;

procedure _TprDataSet_GetFieldValue;
var
  v : Variant;
begin
v := TprDataSet(c).GetFieldValue(_vAsString(Parameters[0]));
if VarIsEmpty(v) then
  raise Exception.CreateFmt(prLoadStr(sParserErrorUnknownProp),[_vAsString(Parameters[0]),c.ClassName])
else
  _vSetAsVariant(Res,v);
end;

initialization

CurrencyFormat := prLoadStr(sCurrencyFormat);
ShortCurrencyFormat := prLoadStr(sShortCurrencyFormat);
SimpleCurrencyFormat := prLoadStr(sSimpleCurrencyFormat);
PercentFormat := prLoadStr(sPercentFormat);
SpacedCurrencyFormat := prLoadStr(sSpacedCurrencyFormat);
BankCurrencyFormat := prLoadStr(sBankCurrencyFormat); 

end.

