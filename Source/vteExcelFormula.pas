
{******************************************}
{                                          }
{           vtk Export library             }
{                                          }
{      Copyright (c) 2002 by vtkTools      }
{                                          }
{******************************************}

unit vteExcelFormula;

interface

uses
  Windows, Classes, SysUtils,

  BIFF8_Types, vteExcelFormula_iftab;

{$i vtk.inc}

type

{$DEFINE DEBUG}

/////////////////////////////////////////////////
//
// rvteOperatorInfo
//
/////////////////////////////////////////////////
rvteOperatorInfo = record
  Name : string[2];
  Priority : integer;
  ptg : byte;
end;
pvteOperatorInfo = ^rvteOperatorInfo;

/////////////////////////////////////////////////
//
// rvteOperator
//
/////////////////////////////////////////////////
rvteOperator = record
  OperatorInfo : pvteOperatorInfo;
  iftab : word;
  ParCount : integer;
  OperandExists : boolean;
end;
pvteOperator = ^rvteOperator;

/////////////////////////////////////////////////
//
// TvteCompileOpStack
//
/////////////////////////////////////////////////
TvteCompileOpStack = class(TObject)
private
  FList : TList;
  FCurPos : integer;
  FLastFunction : pvteOperator;
  function GetItem(i : integer) : pvteOperator;
  function GetCount : integer;
public
  property Items[i : integer] : pvteOperator read GetItem; default;
  property Count : integer read GetCount;
  property LastFunction : pvteOperator read FLastFunction write FLastFunction;
  function Push : pvteOperator;
  function Pop : pvteOperator;
  function Last : pvteOperator;
  procedure Reset;
  procedure Clear;
  constructor Create;
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TvteExtSheet
//
/////////////////////////////////////////////////
TvteExtSheet = class(TObject)
private
  FName : string;
  FiSUPBOOK : integer;
  Fitab : integer;
public
  property Name : string read FName;
  property iSUPBOOK : integer read FiSUPBOOK;
  property itab : integer read Fitab;
  constructor Create(_Name : string; _iSUPBOOK : integer; _itab : integer);
end;

/////////////////////////////////////////////////
//
// TvteExtWorkbook
//
/////////////////////////////////////////////////
TvteExtWorkbook = class(TObject)
private
  FName : string;
  FSheets : TList;
  function GetSheet(i : integer) : TvteExtSheet;
  function GetSheetsCount : integer;
public
  property Sheets[i : integer] : TvteExtSheet read GetSheet; default;
  property SheetsCount : integer read GetSheetsCount;
  property Name : string read FName;
  constructor Create(const _Name : string);
  destructor Destroy; override; 
end;

/////////////////////////////////////////////////
//
// TvteExtRefs
//
/////////////////////////////////////////////////
TvteExtRefs = class(TObject)
private
  FBooks : TList;
  FSheets : TList;
  function GetBook(i : integer) : TvteExtWorkbook;
  function GetSheet(i : integer) : TvteExtSheet;
  function GetBooksCount : integer;
  function GetSheetsCount : integer;
public
  property Books[i : integer] : TvteExtWorkbook read GetBook; default;
  property BooksCount : integer read GetBooksCount;
  property Sheets[i : integer] : TvteExtSheet read GetSheet;
  property SheetsCount : integer read GetSheetsCount;
  function GetSheetIndex(const BookName : string; const SheetName : string) : integer;
  procedure Clear;
  constructor Create;
  destructor Destroy; override;
end;

/////////////////////////////////////////////////
//
// TvteExcelFormulaCompiler
//
/////////////////////////////////////////////////
TvteExcelFormulaCompiler = class(TObject)
private
  FCompileOpStack : TvteCompileOpStack;
  FExtRefs : TvteExtRefs;
  procedure SetError(const ErrorMessage : string);
public
  property ExtRefs : TvteExtRefs read FExtRefs;
  procedure CompileFormula(const s : string; var Ptgs : PChar; var PtgsSize : integer);
  procedure Clear;
  constructor Create;
  destructor Destroy; override;
end;

const
  vteFormulaEndBracketChar = ')';
  vteFormulaStartBracketChar = '(';
  vteFormulaStringChar = '"';
  vteFormulaFuncParamsDelim = ';';
  vteFormulaPercentOperator = '%';
  vteFormulaUnaryPlusOperator = '+';
  vteFormulaUnaryMinusOperator = '-';

  vteFormulaUnaryOperators : set of char = ['+','-'];
  vteFormulaOperatorChars : set of char = ['>','<','=','+','-','*','/','&','%','^'];
  vteFormulaStartIdentChars : set of char = ['a'..'z','A'..'Z','''','$','['];
  vteFormulaIdentChars : set of char = ['a'..'z','A'..'Z','_','0'..'9','.','$','@','''','!',':','[',']'];

  vteOperatorsCount = 18;
  vteOperatorsInfos : array [1..vteOperatorsCount] of rvteOperatorInfo =
                        ((Name : '('; Priority : 0; ptg : 0),
                         (Name : ')'; Priority : 1; ptg : 0),
                         (Name : '>='; Priority : 2; ptg : ptgGE),
                         (Name : '<='; Priority : 2; ptg : ptgLE),
                         (Name : '<>'; Priority : 2; ptg : ptgNE),
                         (Name : '='; Priority : 2; ptg : ptgEQ),
                         (Name : '>'; Priority : 2; ptg : ptgGT),
                         (Name : '<'; Priority : 2; ptg : ptgLT),
                         (Name : '&'; Priority : 3; ptg : ptgConcat),
                         (Name : '+'; Priority : 4; ptg : ptgAdd),
                         (Name : '-'; Priority : 4; ptg : ptgSub),
                         (Name : '*'; Priority : 5; ptg : ptgMul),
                         (Name : '/'; Priority : 5; ptg : ptgDiv),
                         (Name : '^'; Priority : 6; ptg : ptgPower),
                         (Name : '%'; Priority : 7; ptg : ptgPercent),
                         (Name : '+'; Priority : 8; ptg : ptgUPlus),
                         (Name : '-'; Priority : 8; ptg : ptgUMinus),
                         (Name : ''; Priority : 9; ptg : $FF));
  vteFormulaStartBracketOperatorIndex = 1;
  vteFormulaEndBracketOperatorIndex = 2;                       
  vteFormulaPercentOperatorIndex = 15;
  vteFormulaUnaryPlusOperatorIndex = 16;
  vteFormulaUnaryMinusOperatorIndex = 17;
  vteFormulaFunctionOperatorIndex = 18;

  vteFormulaFunctionPriority = 9; // !!!
  vteFormulaStartBracketPriority = 0; // !!!
  vteFormulaEndBracketPriority = 1; // !!!
  vteFormulaPercentOperatorPriority = 7; // !!!


  svteFormulaCompileErrorInvalidBrackets = 'Invalid brackets';
  svteFormulaCompileErrorParameterWithoutFunction = 'Parameter without function';
  svteFormulaCompileErrorInvalidString = 'Invalid string';
  svteFormulaCompileErrorInvalidNumber = 'Invalid number [%s]';
  svteFormulaCompileErrorInvalidSymbol = 'Invalid symbol [%s]';
  svteFormulaCompileErrorUnknownOperator = 'Unknown operator [%s]';
  svteFormulaCompileErrorUnknownFunction = 'Unknown function [%s]';
  svteFormulaCompileErrorInvalidCellReference = 'Invalid cell reference [%s]';
  svteFormulaCompileErrorInvalidRangeReference = 'Invalid range reference [%s]';

var
  vteFormulaNumberChars : set of char = ['0'..'9','.'];

implementation

/////////////////////////////////////////////////
//
// TvteCompileOpStack
//
/////////////////////////////////////////////////
constructor TvteCompileOpStack.Create;
begin
FList := TList.Create;
FCurPos := -1;
end;

destructor TvteCompileOpStack.Destroy;
begin
Clear;
FList.Free;
inherited;
end;

function TvteCompileOpStack.GetItem(i : integer) : pvteOperator;
begin
Result := pvteOperator(FList[i]);
end;

function TvteCompileOpStack.GetCount : integer;
begin
Result := FCurPos+1;
end;

procedure TvteCompileOpStack.Clear;
var
  i : integer;
begin
for i:=0 to FList.Count-1 do
  FreeMem(Items[i]);
FList.Clear;
FCurPos := -1;
FLastFunction := nil;
end;

procedure TvteCompileOpStack.Reset;
begin
FCurPos := -1;
FLastFunction := nil;
end;

function TvteCompileOpStack.Push : pvteOperator;
begin
Inc(FCurPos);
if FCurPos=FList.Count then
  begin
    GetMem(Result,sizeof(rvteOperator));
    FList.Add(Result);
  end
else
  Result := Items[FCurPos];
end;

function TvteCompileOpStack.Pop : pvteOperator;
var
  i : integer;
begin
Dec(FCurPos);
Result := Last;
i := Count-1;
while (i>=0) and (Items[i].OperatorInfo.Priority<>vteFormulaFunctionPriority) do Dec(i);
if i<0 then
  FLastFunction := nil
else
  FLastFunction := Items[i];
end;

function TvteCompileOpStack.Last : pvteOperator;
begin
if FCurPos>=0 then
  Result := Items[FCurPos]
else
  Result := nil;
end;

/////////////////////////////////////////////////
//
// TvteExtSheet
//
/////////////////////////////////////////////////
constructor TvteExtSheet.Create(_Name : string; _iSUPBOOK : integer; _itab : integer);
begin
inherited Create;
FName := _Name;
FiSUPBOOK := _iSUPBOOK;
Fitab := _itab;
end;

/////////////////////////////////////////////////
//
// TvteExtWorkbook
//
/////////////////////////////////////////////////
constructor TvteExtWorkbook.Create(const _Name : string);
begin
inherited Create;
FName := _Name;
FSheets := TList.Create;
end;

destructor TvteExtWorkbook.Destroy;
begin
FSheets.Free;
inherited;
end;

function TvteExtWorkbook.GetSheet(i : integer) : TvteExtSheet;
begin
Result := TvteExtSheet(FSheets[i]);
end;

function TvteExtWorkbook.GetSheetsCount : integer;
begin
Result := FSheets.Count;
end;

/////////////////////////////////////////////////
//
// TvteExtRefs
//
/////////////////////////////////////////////////
constructor TvteExtRefs.Create;
begin
inherited;
FBooks := TList.Create;
FSheets := TList.Create;
end;

destructor TvteExtRefs.Destroy;
begin
Clear;
FSheets.Free;
FBooks.Free;
inherited;
end;

function TvteExtRefs.GetBook(i : integer) : TvteExtWorkbook;
begin
Result := TvteExtWorkbook(FBooks[i]);
end;

function TvteExtRefs.GetSheet(i : integer) : TvteExtSheet;
begin
Result := TvteExtSheet(FSheets[i]);
end;

function TvteExtRefs.GetBooksCount : integer;
begin
Result := FBooks.Count;
end;

function TvteExtRefs.GetSheetsCount : integer;
begin
Result := FSheets.Count;
end;

procedure TvteExtRefs.Clear;
var
  i : integer;
begin
for i:=0 to FBooks.Count-1 do
  TvteExtWorkbook(FBooks[i]).Free;
for i:=0 to FSheets.Count-1 do
  TvteExtSheet(FSheets[i]).Free;
FBooks.Clear;
FSheets.Clear;
end;

function TvteExtRefs.GetSheetIndex(const BookName : string; const SheetName : string) : integer;
var
  i,iBook : integer;
  Book : TvteExtWorkbook;
  Sheet : TvteExtSheet;
begin
iBook := 0;
while (iBook<FBooks.Count) and (Books[iBook].Name<>BookName) do Inc(iBook);
if iBook>=FBooks.Count then
  begin
    Book := TvteExtWorkbook.Create(BookName);
    iBook := FBooks.Add(Book);
    Sheet := TvteExtSheet.Create(SheetName,iBook,0);
    Book.FSheets.Add(Sheet);
    Result := FSheets.Add(Sheet);
  end
else
  begin
    Book := Books[iBook];
    i := 0;
    while (i<Book.SheetsCount) and (Book.Sheets[i].Name<>SheetName) do Inc(i);
    if i<Book.SheetsCount then
      Result := FSheets.IndexOf(Book.Sheets[i])
    else
      begin
        Sheet := TvteExtSheet.Create(SheetName,iBook,Book.SheetsCount);
        Book.FSheets.Add(Sheet);
        Result := FSheets.Add(Sheet);
      end;
  end;
end;

/////////////////////////////////////////////////
//
// TvteExcelFormulaCompiler
//
/////////////////////////////////////////////////
constructor TvteExcelFormulaCompiler.Create;
begin
inherited;
FExtRefs := TvteExtRefs.Create;
FCompileOpStack := TvteCompileOpStack.Create;
end;

destructor TvteExcelFormulaCompiler.Destroy;
begin
FCompileOpStack.Free;
FExtRefs.Free;
inherited;
end;

procedure TvteExcelFormulaCompiler.Clear;
begin
FExtRefs.Clear;
FCompileOpStack.Clear;
end;

procedure TvteExcelFormulaCompiler.SetError(const ErrorMessage : string);
begin
raise Exception.Create(ErrorMessage);
end;

procedure TvteExcelFormulaCompiler.CompileFormula(const s : string; var Ptgs : PChar; var PtgsSize : integer);
var
  vd : extended;
  Str : pptgStr;
  Last : pvteOperator;
  b1,ExtRef,CellRef,ExtBook,ExtSheet : string;
  i,j,l,vi,valCode,NewStrSize,CurStrSize : integer;

  procedure Addptg(_Ptg : byte; _PtgData : pointer; _PtgDataSize : integer);
  begin
  ReallocMem(Ptgs,PtgsSize+_PtgDataSize+1);
  PChar(Ptgs)[PtgsSize] := char(_Ptg);
  if _PtgData<>nil then
    MoveMemory(PChar(Ptgs)+1+PtgsSize,_PtgData,_PtgDataSize);
  PtgsSize := PtgsSize+_PtgDataSize+1;
  end;

  procedure AddptgOperator(o : pvteOperator);
  var
    FuncVar : rptgFuncVar;
  begin
  if o.OperatorInfo.ptg=$FF then
    begin
      FuncVar.cargs := o.ParCount;
      FuncVar.iftab := o.iftab;
      Addptg(ptgFuncVar,@FuncVar,sizeof(rptgFuncVar));
    end
  else
    Addptg(o.OperatorInfo.ptg,nil,0);
  end;

  procedure AddptgIdentificator(const Ident : string);
  var
    p : integer;
    Ref : rptgRef;
    Area : rptgArea;
    Ref3D : rptgRef3D;
    Area3D : rptgArea3D;

    function CompileCellRef(s : string; var rw,grbitCol : word) : boolean;
    const
      SymbolA = 65;
      SymbolZ = 90;
      SymbolsAZ = SymbolZ-SymbolA+1;
    var
      i,l : integer;
    begin
    Result := false;
    s := UpperCase(s);
    rw := 0;
    grbitCol := 0;
    l := Length(s);
    i := l;
    while (i>0) and (s[i] in ['0'..'9']) do Dec(i);
    if (i=0) or (i=l) then exit;
    rw := StrToInt(Copy(s,i+1,l))-1;
    if s[i]<>'$' then
      grbitCol := grbitCol or $8000
    else
      if i=1 then exit
      else
        Dec(i);
    j := i;
    while (i>0) and (s[i] in ['A'..'Z']) do Dec(i);
    if (i > 0) and (s[i]='$') then
      begin
        if i<>1 then exit;
      end
    else
      if i<>0 then exit
      else
        grbitCol := grbitCol or $4000;
    if j-i>2 then exit;
    if j-i=1 then
      grbitCol := grbitCol or (byte(s[j])-SymbolA)
    else
      grbitCol := grbitCol or (byte(s[i+1])-SymbolA+1)*SymbolsAZ+(byte(s[j])-SymbolA);
    Result := true;
    end;

  begin
  // In the current version it can be only reference to a cell or range of cells of the same sheet
  p := pos('!',Ident);
  if p<>0 then
    begin
      if Ident[1]='''' then
        begin
          p := 2;
          while (p<Length(Ident)) and (Ident[p]<>'''') do Inc(p);
          if (p>=Length(Ident)) or (Ident[p+1]<>'!') then
            SetError(Format(svteFormulaCompileErrorInvalidRangeReference,[Ident]));
          ExtRef := Copy(Ident,2,p-2);
          CellRef := Trim(Copy(Ident,p+2,Length(Ident)));
        end
      else
        begin
          ExtRef := Copy(Ident,1,p-1);
          CellRef := Trim(Copy(Ident,p+1,Length(Ident)));
        end;
      ExtRef := Trim(ExtRef);
      if ExtRef='' then
        SetError(Format(svteFormulaCompileErrorInvalidRangeReference,[Ident]));
      if ExtRef[1]='[' then
        begin
          p := 2;
          while (p<=Length(ExtRef)) and (ExtRef[p]<>']') do Inc(p);
          if p>Length(ExtRef) then
            SetError(Format(svteFormulaCompileErrorInvalidRangeReference,[Ident]));
          ExtBook := Copy(ExtRef,2,p-2);
          ExtSheet := Copy(ExtRef,p+1,Length(ExtRef));
        end
      else
        begin
          ExtBook := '';
          ExtSheet := ExtRef;
        end;
      // analyze cellref
      p := pos(':',CellRef);
      if p=0 then
        begin
          // cell reference
          if not CompileCellRef(CellRef,Ref3D.rw,Ref3D.grbitCol) then
            SetError(Format(svteFormulaCompileErrorInvalidCellReference,[Ident]));
          Ref3D.ixti := FExtRefs.GetSheetIndex(ExtBook,ExtSheet);
          Addptg(ptgRef3D,@Ref3D,sizeof(rptgRef3D));
        end
      else
        begin
          // area reference
          if not CompileCellRef(Copy(CellRef,1,p-1),Area3D.rwFirst,Area3D.grbitColFirst) then
            SetError(Format(svteFormulaCompileErrorInvalidRangeReference,[Ident]))
          else
            if not CompileCellRef(Copy(CellRef,p+1,Length(Ident)),Area3D.rwLast,Area3D.grbitColLast) then
              SetError(Format(svteFormulaCompileErrorInvalidRangeReference,[Ident]));
          Area3D.ixti := FExtRefs.GetSheetIndex(ExtBook,ExtSheet);
          Addptg(ptgArea3D,@Area3D,sizeof(rptgArea3D));
        end;
    end
  else
    begin
      p := pos(':',Ident);
      if p=0 then
        begin
          // cell reference
          if not CompileCellRef(Ident,Ref.rw,Ref.grbitCol) then
            SetError(Format(svteFormulaCompileErrorInvalidCellReference,[Ident]));
          Addptg(ptgRef,@Ref,sizeof(rptgRef));
        end
      else
        begin
          // area reference
          if not CompileCellRef(Copy(Ident,1,p-1),Area.rwFirst,Area.grbitColFirst) then
            SetError(Format(svteFormulaCompileErrorInvalidRangeReference,[Ident]))
          else
            if not CompileCellRef(Copy(Ident,p+1,Length(Ident)),Area.rwLast,Area.grbitColLast) then
              SetError(Format(svteFormulaCompileErrorInvalidRangeReference,[Ident]));
          Addptg(ptgArea,@Area,sizeof(rptgArea));
        end;
    end;
  end;

  procedure AddptgStr(const s : string);
  begin
  NewStrSize := sizeof(rptgStr)+Length(s)*sizeof(WideChar);
  if NewStrSize>CurStrSize then
    begin
      ReallocMem(Str,NewStrSize);
      CurStrSize := NewStrSize;
    end;
  Str.cch := Length(s);
  Str.grbit := 1;
  StringToWideChar(s,PWideChar(PChar(Str)+sizeof(rptgStr)),Length(s)*sizeof(WideChar));
  Addptg(ptgStr,PChar(Str),NewStrSize);
  end;

  procedure AddptgInt(n : word);
  var
    int : rptgInt;
  begin
  int.w := n;
  Addptg(ptgInt,@int,sizeof(rptgInt));
  end;

  procedure AddptgNum(n : double);
  var
    num : rptgNum;
  begin
  num.num := n;
  Addptg(ptgNum,@num,sizeof(rptgNum));
  end;

  function GetOperatorIndex(const s : string) : integer;
  begin
  Result := 1;
  while (Result<=vteOperatorsCount) and (AnsiCompareText(vteOperatorsInfos[Result].Name,s)<>0) do Inc(Result);
  if Result>vteOperatorsCount then
    SetError(Format(svteFormulaCompileErrorUnknownOperator,[s]));
  end;

  function GetFunction_iftab(const s : string) : integer;
  begin
  Result := 1;
  while (Result<=vteExcelFunctionsCount) and (AnsiCompareText(vteExcelFunctions[Result].FuncName,s)<>0) do Inc(Result);
  if Result>vteExcelFunctionsCount then
    SetError(Format(svteFormulaCompileErrorUnknownFunction,[s]))
  else
    Result :=  vteExcelFunctions[Result].iftab;
  end;

  function ProcessOperator(OperatorInfoIndex : integer) : pvteOperator;
  var
    Last : pvteOperator;
    oi : pvteOperatorInfo;
  begin
  Result := nil;
  oi := @vteOperatorsInfos[OperatorInfoIndex];

  Last := FCompileOpStack.Last;
  if (Last<>nil) and (oi.Priority<>0) and (oi.Priority<=Last.OperatorInfo.Priority) then
    begin
      while (Last<>nil) and (Last.OperatorInfo.Priority>=oi.Priority) do
        begin
          AddptgOperator(Last);
          Last := FCompileOpStack.Pop;
        end;
    end;

  if oi.Priority<>vteFormulaEndBracketPriority then
    begin
      Result := FCompileOpStack.Push;
      with Result^ do
        begin
          OperatorInfo := oi;
          ParCount := 0;
          OperandExists := false;
        end
    end
  else
    begin
      if Last=nil then
        SetError(svteFormulaCompileErrorInvalidBrackets)
      else
        begin
          Last := FCompileOpStack.Pop;
          if Last<>nil then
            begin
              if Last.OperatorInfo.Priority<>vteFormulaFunctionPriority then
                Addptg(ptgParen,nil,0)
              else
                begin
                  if Last.OperandExists then
                    Inc(Last.ParCount)
                  else
                    if Last.ParCount>0 then
                      begin
                        Addptg(ptgMissArg,nil,0);
                        Inc(Last.ParCount)
                      end;
                end;
            end
          else
            Addptg(ptgParen,nil,0)
        end;
    end;
  end;

begin
FCompileOpStack.Reset;
Str := nil;
CurStrSize := 0;
l := Length(s);
i := 1;
try
  while i<=l do
    begin
      if s[i] in vteFormulaStartIdentChars then
        begin
          // identificator
          if FCompileOpStack.LastFunction<>nil then
            FCompileOpStack.LastFunction.OperandExists := true;
  
          j := i;
          while (i<=l) and (s[i] in vteFormulaIdentChars) do Inc(i);
          while (i<=l) and (s[i]<=#32) do Inc(i);
          b1 := Trim(Copy(s,j,i-j));
          if (i<=l) and (s[i]=vteFormulaStartBracketChar) then
            begin
              // this is function, find function iftab
              FCompileOpStack.LastFunction := ProcessOperator(vteFormulaFunctionOperatorIndex);
              FCompileOpStack.LastFunction.iftab := GetFunction_iftab(b1)
            end
          else
            AddptgIdentificator(b1);
        end
      else
      if (s[i]=vteFormulaFuncParamsDelim) then
        begin
          // this is a function parameters delimeter
          if FCompileOpStack.LastFunction=nil then
            SetError(svteFormulaCompileErrorParameterWithoutFunction);
          if not FCompileOpStack.LastFunction.OperandExists then
            Addptg(ptgMissArg,nil,0);
  
          // We should process all operators up to the first opening bracket
          Last := FCompileOpStack.Last;
          while Last.OperatorInfo.Priority<>vteFormulaStartBracketPriority do
            begin
              AddptgOperator(Last);
              Last := FCompileOpStack.Pop;
            end;
          with FCompileOpStack.LastFunction^ do
            begin
              OperandExists := false;
              Inc(ParCount);
            end;
          Inc(i);
        end
      else
      if (s[i]=vteFormulaPercentOperator) then
        begin
          ProcessOperator(vteFormulaPercentOperatorIndex);
          Inc(i);
        end
      else
      if s[i] in vteFormulaOperatorChars then
        begin
          // operator
          j := i;
          while (i<=l) and (s[i] in vteFormulaOperatorChars) do Inc(i);
          b1 := Copy(s,j,i-j);
          vi := Length(b1);
          if (vi>1) and (b1[vi] in vteFormulaUnaryOperators) then
            begin
              ProcessOperator(GetOperatorIndex(Copy(b1,1,vi-1)));
              if b1[vi]=vteFormulaUnaryPlusOperator then
                ProcessOperator(vteFormulaUnaryPlusOperatorIndex)
              else
                ProcessOperator(vteFormulaUnaryMinusOperatorIndex);
            end
          else
            if (vi=1) and (b1[1] in vteFormulaUnaryOperators) then
              begin
                // Probably it is a unary operator
                Dec(j);
                while (j>1) and (s[j]<=#32) do Dec(j);
                if (j<1) or (s[j] in [vteFormulaStartBracketChar,vteFormulaFuncParamsDelim]) then
                  begin
                    if b1[vi]=vteFormulaUnaryPlusOperator then
                      ProcessOperator(vteFormulaUnaryPlusOperatorIndex)
                    else
                      ProcessOperator(vteFormulaUnaryMinusOperatorIndex);
                  end
                else
                  ProcessOperator(GetOperatorIndex(b1));
              end
            else
              ProcessOperator(GetOperatorIndex(b1));
        end
      else
      if s[i]=vteFormulaStartBracketChar then
        begin
          ProcessOperator(vteFormulaStartBracketOperatorIndex);
          Inc(i);
        end
      else
      if s[i]=vteFormulaEndBracketChar then
        begin
          ProcessOperator(vteFormulaEndBracketOperatorIndex);
          Inc(i);
        end
      else
      if s[i] = vteFormulaStringChar then
        begin
          // text string
          if FCompileOpStack.LastFunction<>nil then
            FCompileOpStack.LastFunction.OperandExists := true;
  
          Inc(i);
          j := i;
          while (i<=l) and (s[i]<>vteFormulaStringChar) do Inc(i);
          if i>l then
            SetError(svteFormulaCompileErrorInvalidString);
          // build ptgStr
          AddptgStr(Copy(s,j,i-j));
          Inc(i);
        end
      else
      if (s[i] in vteFormulaNumberChars) then
        begin
          // number - integer or double
          if FCompileOpStack.LastFunction<>nil then
            FCompileOpStack.LastFunction.OperandExists := true;
  
          j := i;
          while (i<=l) and (s[i] in vteFormulaNumberChars) do Inc(i);
          b1 := Copy(s,j,i-j);

          val(b1,vi,valCode);
          if (valCode=0) and (vi<=$FFFF) then
            AddptgInt(vi)
          else
            if TextToFloat(PChar(b1),vd,fvExtended) then
              AddptgNum(vd)
            else
              SetError(Format(svteFormulaCompileErrorInvalidNumber,[b1]));
        end
      else
      if s[i]<=#32 then
        Inc(i)
      else
        SetError(Format(svteFormulaCompileErrorInvalidSymbol,[s[i]]));
    end;
  
  Last := FCompileOpStack.Last;
  while FCompileOpStack.Last<>nil do
    begin
      if Last.OperatorInfo.Priority=vteFormulaStartBracketPriority then
        SetError(svteFormulaCompileErrorInvalidBrackets);
      AddptgOperator(Last);
      Last := FCompileOpStack.Pop;
    end;
finally
  if Str<>nil then
    FreeMem(Str);
end;
end;

initialization

Include(vteFormulaNumberChars,DecimalSeparator);

end.
