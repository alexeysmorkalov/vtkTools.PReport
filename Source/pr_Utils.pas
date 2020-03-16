{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_Utils;

interface

{$I PR.INC}

uses
  menus, messages, WinSpool, Windows, Forms, SysUtils, Math, Classes, Graphics,
  {$IFDEF PR_D6_D7} Variants, {$ENDIF}
  typinfo, vgr_Functions;

const
  ZeroAsciiCode = 48;
  NullYearMonth = 0;
  sWordsDelimiters = ['.',',','>','<','`','~','!','@','#','$','%','^','&','*','(',')','+','-','=','\','|','[',']','{','}',':',';','"','''','?',#13,#10];
{$IFDEF PR_DEBUG}
  sLogFileDelim = '----------------';
  LogFileName   = 'c:\prLogFile.log';
{$ENDIF}

type
  TRoundProc = function (Value : Extended) : Int64;
  TGender = (gFemale,gMale);
  TNumberType = (ntCurrency,ntSimple);
  TCentsFormat = (cfNone,cfTwoDigit,cfTwoDigitWithCentsText);
  TArrayString = array of string;

/////////////////////////////////////////////////
//
// TprTemplateFoundObject
//
/////////////////////////////////////////////////
TprTemplateFoundObject = class(TObject)
private
  FObjectRef : TObject;
  FSubObjectRef : TObject;
  FPropName : string;
  FPropValue : string;
public
  property ObjectRef : TObject read FObjectRef write FObjectRef;
  property SubObjectRef : TObject read FSubObjectRef write FSubObjectRef;
  property PropName : string read FPropName write FPropName;
  property PropValue : string read FPropValue write FPropValue;
end;

/////////////////////////////////////////////////
//
// TprTemplateFoundList
//
/////////////////////////////////////////////////
TprTemplateFoundList = class(TList)
private
  function GetItm(i : integer) : TprTemplateFoundObject;
public
  property Items[i : integer] : TprTemplateFoundObject read GetItm; default;
  procedure AddObject(_ObjectRef : TObject; _SubObjectRef : TObject; const _PropName,_PropValue : string);
  procedure DeleteObject(i : integer);
  procedure Clear; override;
  destructor Destroy; override;
end;

{$IFDEF PR_DEBUG}
procedure WriteToLog(Msg : string);
{$ENDIF}

procedure SaveComponent(FileName : string; C : TComponent);
function  LoadComponent(FileName : String) : TComponent;

function AddFlash(const PathName : string) : string;
function GetFindFileName(FileName : string) : string;
function ExtractFileNameOnly(const fn : string) : string;

// message boxes
function MBox(Text,Caption : string; Flags : integer) : integer;
procedure MBError(Text : string);
procedure MBMessage(Text,Caption : string);

// date
function IncDay(ADate: TDateTime; Delta: Integer): TDateTime;
function DaysBetween(Date1, Date2: TDateTime): Longint;
function CutTime(ADate: TDateTime): TDateTime; { Set time to 00:00:00:00 }
function MonthsBetween(Date1, Date2: TDateTime): Double;
procedure DateDiff(Date1, Date2: TDateTime; var Days, Months, Years: Word);
function DaysPerMonth(AYear, AMonth: Integer): Integer;
function GetFirstDayMonth(m,y : integer) : TDateTime;
function GetLastDayMonth(m,y : integer) : TDateTime;
function GetMonthName(m : integer) : string;

// other
procedure WBeep(Tone : word; MSecs : integer);
function TextToPageList(const s: string; PagesList: TList) : boolean;
function CheckPageList(const s : string) : boolean;
procedure UpdatePrintersList(FPrinters : TStrings; const VirtualPrinterName : string);
function GetPrintersCount : integer;
function ExtRound(Value : extended) : Int64;
function RoundEx(Value : extended; N : integer = 2) : double;
procedure Exchange(var v1,v2 : integer);
procedure ClearPopupMenu(PopupMenu : TPopupMenu);

// forms
procedure ShowForm(Form : TForm);

// string functions
function FindTextInString(const s : string; const SearchString : string; WholeWords,CaseSensitive : boolean) : integer;
function ReplaceTextInString(const s : string; const SearchString,ReplaceString : string; WholeWords,CaseSensitive : boolean) : string;
function PosLastChar(const s : string; c : char) : integer;
function IsThisCharsOnly(const s : string; chars : TSysCharSet) : boolean;
function PExtractSubStr(S : string; var p : integer) : string;
procedure CutString(s: string; lenline: integer; var ress: TArrayString);
function Copy2SymbFromEnd(s : string; c : char) : string;
function Copy3SymbFromEnd(s : string; c : char) : string;
function GetTempFile(Prefix : string) : string; overload;
function GetTempFile(Dir,Prefix : string) : string; overload;
function CompText(const S1, S2: string): Integer;
function MakeStr(C: Char; N: Integer): string;
function ExtractSubstr(const S: string;
                       var Pos: Integer;
                       const Delims: TSysCharSet): string;
function AddChar(C: Char; const S: string; N: Integer): string;
function AddCharR(C: Char; const S: string; N: Integer): string;
function CenterStr(const S: string; Len: Integer): string;

// Variant
function GetVariantTypeDesc(V : Variant) : string;
function IsValidVariantType(V : Variant) : boolean;

// type convertions
function StrToYearMonth(V : string) : integer;
function StrToYearMonthDef(V : string; Def : integer) : integer;
function GetCurrentYearMonth : integer;
function ValidYearMonth(V : integer) : boolean;
function IncYM(V,Delta : integer) : integer;
function YearMonthToStr(V : integer) : string;
function YearMonthToStrFull(V : integer) : string;
function BoolToStr(V : boolean) : string;
function StrToBool(V : string) : boolean;
function StrToDateDef(s : string; def : TDateTime) : TDateTime;
function StrToFloatDef(s : string; def : double) : double;
function StrToBoolDef(V : string; def : boolean) : boolean;
function DoEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;

function PointInRect(X,Y : integer; const R : TRect) : boolean;
function RectInRect(const r1 : TRect; const r2 : TRect) : boolean;
function RectOverRect(const r1 : TRect; const r2 : TRect) : boolean;

// WinAPI
procedure Check(wnd : HWND; CtrlIDs : array of integer);
procedure UnCheck(wnd : HWND; CtrlIDs : array of integer);
function IsChecked(wnd : HWND; CtrlID : integer) : boolean;
function GetText(wnd : HWND; CtrlID : integer) : string;
function GetSelText(wnd : HWND; CtrlID : integer) : string;
procedure CenterWindow(wnd: HWND);
function Create90Font(Font : TFont) : HFont;
function CreateAPIFont(Font : TFont) : HFont;
function CreateDefFont(DC : HDC; Size : integer; Color : TColor) : HFont;
function Create90DefFont(DC : HDC; Size : integer; Color : TColor) : HFont;
function GetFontSize(const FontName : string; FontSize : integer; FontStyle : TFontStyles) : TSize;
procedure AddRectToRegion(var Rgn : HRGN; const r : TRect; Op : integer = RGN_OR);
function NormalizeRect(X1,Y1,X2,Y2 : integer) : TRect;

// RTTI
procedure DecompileProp(o : TObject; var obj : TObject; var lpi : PPropInfo; HasSubProp : boolean; const PropName,ClassPropName,SubPropName : string);
procedure prFindTextInProps(Obj : TPersistent; ObjectRef : TPersistent; const Text : string; WholeWords,CaseSensitive : boolean; FoundList : TprTemplateFoundList);
procedure prReplaceTextInProp(Obj : TPersistent; const PropName,FindText,ReplaceText : string; WholeWords,CaseSensitive : boolean);

// from number - string, only for RUSSIAN LANGUAGE
function  GSN(Number : extended;
              NumberType : TNumberType = ntCurrency;
              CentsFormat : TCentsFormat = cfTwoDigit;
              CentsText : string = 'коп.';
              Gender : TGender = gMale;
              sOne : string = '';
              sTwoToFour : string = '';
              sOverFour : string = '';
              sOneRUB : string = 'рубль';
              sTwoToFourRUB : string = 'рубл€';
              sOverFourRUB : string = 'рублей';
              sOneKOP : string = 'копейка';
              sTwoToFourKOP : string = 'копейки';
              sOverFourKOP : string = 'копеек') : string;


var
  ScreenDC: HDC;
  
  prDoublePrec : double = 0.0001;
  prRoundProc : TRoundProc = nil;
  RoundArray : array [0..5] of integer =(1,10,100,1000,10000,100000);

  
  // this array loaded at run program from resources
  MonthsArray : array [1..2,1..12] of string =
                (('January','February','March','April','May','June','July','August','September','Oktober','November','December'),
                 ('January','February','March','April','May','June','July','August','September','Oktober','November','December'));

  DaysInMonths : array [1..12] of integer = (31,28,31,30,31,30,31,31,30,31,30,31);

  // this arrays used by function GSN, only for RUSSIAN LANGUAGE
  GSNArray1 : array [0..9] of string =
              ('дес€ть',
               'одиннадцать',
               'двенадцать',
               'тринадцать',
               'четырнадцать',
               'п€тнадцать',
               'шестнадцать',
               'семнадцать',
               'восемнадцать',
               'дев€тнадцать');
  GSNArray2 : array [1..9] of string =
              ('сто',
               'двести',
               'триста',
               'четыреста',
               'п€тьсот',
               'шестьсот',
               'семьсот',
               'восемьсот',
               'дев€тьсот');
  GSNArray3 : array [2..9] of string =
              ('двадцать',
               'тридцать',
               'сорок',
               'п€тьдес€т',
               'шестьдес€т',
               'семьдес€т',
               'восемьдес€т',
               'дев€носто');
  GSNArray4 : array [1..9] of string =
              ('один',
               'два',
               'три',
               'четыре',
               'п€ть',
               'шесть',
               'семь',
               'восемь',
               'дев€ть');
  GSNArray5 : array [1..9] of string =
              ('одна',
               'две',
               'три',
               'четыре',
               'п€ть',
               'шесть',
               'семь',
               'восемь',
               'дев€ть');
  GSNArray6 : array [1..3,0..2] of string =
               (('тыс€ча','тыс€чи','тыс€ч'),
                ('миллион','миллиона','миллионов'),
                ('миллиард','миллиарда','миллиардов'));
  GSNArray7 : array [1..4,1..2] of string =
               (('дес€та€','дес€тых'),
                ('сота€','сотых'),
                ('тыс€чна€','тыс€чных'),
                ('дес€титыс€чна€','дес€титыс€чных'));

implementation

uses
  pr_MultiLang, pr_Strings;

/////////////////////////////////////////////////
//
// TprTemplateFoundList
//
/////////////////////////////////////////////////
function TprTemplateFoundList.GetItm(i : integer) : TprTemplateFoundObject;
begin
Result := TprTemplateFoundObject(inherited Items[i]);
end;

procedure TprTemplateFoundList.AddObject(_ObjectRef : TObject; _SubObjectRef : TObject; const _PropName,_PropValue : string);
begin
with Items[Add(TprTemplateFoundObject.Create)] do
  begin
    ObjectRef := _ObjectRef;
    SubObjectRef := _SubObjectRef;
    PropName := _PropName;
    PropValue := _PropValue;
  end;
end;

procedure TprTemplateFoundList.DeleteObject(i : integer);
begin
Items[i].Free;
Delete(i);
end;

procedure TprTemplateFoundList.Clear;
begin
FreeListItems(Self);
inherited;
end;

destructor TprTemplateFoundList.Destroy;
begin
Clear;
inherited;
end;

{$IFDEF PR_DEBUG}
procedure WriteToLog;
var
  hFile : THandle;

  procedure WriteString(s : string);
  var
    BytesWritten : cardinal;
  begin
  s:=s+#13#10;
  WriteFile(hFile,(@s[1])^,Length(s),BytesWritten,nil);
  end;

begin
hFile:=CreateFile(PChar(LogFileName),
                  GENERIC_WRITE,
                  FILE_SHARE_WRITE or FILE_SHARE_READ,
                  nil,
                  OPEN_ALWAYS,
                  FILE_ATTRIBUTE_NORMAL or FILE_FLAG_WRITE_THROUGH,
                  0);
if (hFile=INVALID_HANDLE_VALUE) then
  exit;

SetFilePointer(hFile,0,nil,FILE_END);
WriteString(sLogFileDelim);
WriteString(Format('DateTime : %s',[DateTimetoStr(Now)]));
WriteString('Message  :');
WriteString(Msg);
WriteString(sLogFileDelim);

FlushFileBuffers(hFile);
CloseHandle(hFile);
end;
{$ENDIF}

/////////////////////////////////////////////////
//
// Functions
//
/////////////////////////////////////////////////
procedure CenterWindow(wnd: HWnd);
var
  Rect: TRect;
  Monitor: TMonitor;
begin
GetWindowRect(wnd,Rect);
if Application.MainForm <> nil then
  Monitor := Application.MainForm.Monitor
else
  Monitor := Screen.Monitors[0];
  
SetWindowPos(Wnd,
             0,
             Monitor.Left+((Monitor.Width-Rect.Right+Rect.Left) div 2),
             Monitor.Top+((Monitor.Height-Rect.Bottom+Rect.Top) div 3),
             0,0,SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
end;

procedure Check(wnd: HWnd; CtrlIDs : array of integer);
var
  i : integer;
begin
for i:=0 to High(CtrlIDs) do
  SendDlgItemMessage(Wnd,CtrlIDs[i],BM_SETCHECK,BST_CHECKED,0);
end;

procedure UnCheck(wnd: HWnd; CtrlIDs : array of integer);
var
  i : integer;
begin
for i:=0 to High(CtrlIDs) do
  SendDlgItemMessage(Wnd,CtrlIDs[i],BM_SETCHECK,BST_UNCHECKED,0);
end;

function IsChecked(wnd: HWnd; CtrlID : integer) : boolean;
begin
Result := SendDlgItemMessage(Wnd,CtrlID,BM_GETCHECK,0,0)=BST_CHECKED;
end;

function GetText(wnd: HWnd; CtrlID : integer) : string;
var
  TextLength : integer;
begin
TextLength := GetWindowTextLength(GetDlgItem(Wnd,CtrlID));
if TextLength>0 then
  begin
    SetLength(Result,TextLength);
    if GetDlgItemText(Wnd,CtrlID,@(Result[1]),TextLength+1)<=0 then
      Result := '';
  end
else
  Result := '';
end;

function GetSelText(wnd: HWnd; CtrlID : integer) : string;
var
  i,TextLength : integer;
begin
Result := '';
i := SendDlgItemMessage(Wnd,CtrlID,CB_GETCURSEL,0,0);
if i<>CB_ERR then
  begin
    TextLength:=SendDlgItemMessage(Wnd,CtrlID,CB_GETLBTEXTLEN,i,0);
    if TextLength>0 then
      begin
        SetLength(Result,TextLength);
        if SendDlgItemMessage(Wnd,CtrlID,CB_GETLBTEXT,i,integer(@(Result[1])))=CB_ERR then
          Result := ''
      end
    else
      Result := '';
  end;
end;

function PointInRect;
begin
Result:=((X>=R.Left) and (X<=R.Right)) and ((Y>=R.Top) and (Y<=R.Bottom))
end;

function RectInRect;
begin
Result:=PointInRect(r1.Left,r1.Top,r2) and
        PointInRect(r1.Right,r1.Top,r2) and
        PointInRect(r1.Right,r1.Bottom,r2) and
        PointInRect(r1.Left,r1.Bottom,r2);
end;

function RectOverRect;
begin
Result:=PointInRect(r1.Left,r1.Top,r2) or
        PointInRect(r1.Right,r1.Top,r2) or
        PointInRect(r1.Right,r1.Bottom,r2) or
        PointInRect(r1.Left,r1.Bottom,r2) or

        PointInRect(r2.Left,r2.Top,r1) or
        PointInRect(r2.Right,r2.Top,r1) or
        PointInRect(r2.Right,r2.Bottom,r1) or
        PointInRect(r2.Left,r2.Bottom,r1) or

        ((r1.Left>r2.Left) and (r1.Right<r2.Right) and (r1.Top<r2.Top) and (r1.Bottom>r2.Bottom)) or
        ((r1.Left<r2.Left) and (r1.Right>r2.Right) and (r1.Top>r2.Top) and (r1.Bottom<r2.Bottom)) or

        ((r2.Left>r1.Left) and (r2.Right<r1.Right) and (r2.Top<r1.Top) and (r2.Bottom>r1.Bottom)) or
        ((r2.Left<r1.Left) and (r2.Right>r1.Right) and (r2.Top>r1.Top) and (r2.Bottom<r1.Bottom));
end;

function AddFlash(const PathName : string) : string;
begin
Result := PathName;
if Result[Length(Result)]<>'\' then
  Result := Result+'\';
end;

function GetFindFileName;
var
  Buf : array [0..255] of char;
begin
Result := AddFlash(ExtractFilePath(Application.ExeName))+FileName;
if not FileExists(Result) then
  begin
    {find in System dir}
    GetSystemDirectory(Buf, 255);
    Result := AddFlash(StrPas(Buf))+FileName;
    if not FileExists(Result) then
      begin
        {find in Windows dir}
        GetWindowsDirectory(Buf, 255);
        Result := AddFlash(StrPas(Buf))+FileName;
      end;
  end;
end;

function ExtractFileNameOnly(const fn : string) : string;
var
  i : integer;
begin
Result := ExtractFileName(fn);
i := Length(Result);
while (i>=1) and (Result[i]<>'.') do Dec(i);
if i>=1 then
  SetLength(Result,i-1);
end;

function sp(Item1, Item2: Pointer): Integer;
begin
Result := integer(Item1)-integer(Item2);
end;

function CheckPageList(const s : string) : boolean;
var
  l : TList;
begin
l := TList.Create;
try
  Result := TextToPageList(s,l);
finally
  l.Free;
end;
end;

function TextToPageList(const s: String; PagesList: TList) : boolean;
var
  i,j,l,n1,n2 : integer;
begin
PagesList.Clear;
Result := Trim(s)='';
if Result then exit;
Result := false;
n2 := -1;
n1 := -1;
l := Length(s);
i := 1;
while i<=l do
  begin
    case s[i] of
      '-':
        begin
          if (n2=-1) or (n1<>-1) then exit;
          n1 := n2;
        end;
      ',':
        begin
          if (n1<>-1) and (n2<>-1) then
            for j:=n1 to n2 do
              PagesList.Add(pointer(j))
          else
            if n2<>-1 then
              PagesList.Add(pointer(n2))
            else
              exit;
          n1 := -1;
          n2 := -1;
        end;
      '0'..'9':
        begin
          j := i;
          while (i<=l) and (s[i] in ['0'..'9']) do Inc(i);
          n2 := StrToInt(Copy(s,j,i-j));
          Dec(i);
        end;
      else
        exit;
    end;
    Inc(i);
  end;
if (n1<>-1) and (n2<>-1) then
  for j:=n1 to n2 do
    PagesList.Add(pointer(j))
else
  if n2<>-1 then
    PagesList.Add(pointer(n2))
  else
    exit;
PagesList.Sort(sp);
Result := true;
end;

function GetPrintersCount;
var
  Buffer : pointer;
  Level,Flags,BytesNeeded,NumInfo : cardinal;
begin
Result := 0;
if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Flags := PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
    Level := 4;
  end
else
  begin
    Flags := PRINTER_ENUM_LOCAL;
    Level := 5;
  end;

Buffer := nil;
BytesNeeded := 0;
EnumPrinters(Flags,nil,Level,Buffer,0,BytesNeeded,NumInfo);
if BytesNeeded = 0 then exit;
GetMem(Buffer,BytesNeeded);
try
  if EnumPrinters(Flags,nil,Level,Buffer,BytesNeeded,BytesNeeded,NumInfo) then
    Result := NumInfo;
finally
  FreeMem(Buffer);
end;
end;

procedure UpdatePrintersList;
var
  i : integer;
  tmpBuffer,Buffer : PChar;
  Level,Flags,BytesNeeded,NumInfo : cardinal;
begin
FPrinters.Clear;
if VirtualPrinterName<>'' then
  FPrinters.Add(VirtualPrinterName);

if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Flags := PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
    Level := 4;
  end
else
  begin
    Flags := PRINTER_ENUM_LOCAL;
    Level := 5;
  end;

Buffer := nil;
BytesNeeded := 0;
EnumPrinters(Flags,nil,Level,Buffer,0,BytesNeeded,NumInfo);
if BytesNeeded=0 then exit;

GetMem(Buffer,BytesNeeded);
tmpBuffer := Buffer;
try
  if not EnumPrinters(Flags,nil,Level,Buffer,BytesNeeded,BytesNeeded,NumInfo) then exit;

  for i:=0 to NumInfo-1 do
    begin
      if Level = 4 then
        begin
          FPrinters.Add(PPrinterInfo4(Buffer)^.pPrinterName);
          Inc(Buffer,sizeof(PRINTER_INFO_4));
        end
      else
        begin
          FPrinters.Add(PPrinterInfo5(Buffer)^.pPrinterName);
          Inc(Buffer,sizeof(PRINTER_INFO_5));
        end;
    end;
finally
  FreeMem(tmpBuffer);
end;
end;

function GetMonthName;
begin
if (m<1) or (m>12) then Result:=''
                   else Result:=MonthsArray[1,m];
end;

function GetFirstDayMonth;
begin
Result := EncodeDate(y,m,1);
end;

function GetLastDayMonth;
var
  d : integer;
begin
if (m>=1) and (m<=12) then d:=DaysInMonths[m]
                      else d:=1;
if (m=2) and ((y mod 4)=0) then Inc(d);
Result := EncodeDate(y,m,d);
end;

procedure SaveComponent;
var
  fs : TFileStream;
  ms : TMemoryStream;
begin
fs:=nil;
ms:=nil;
try
  fs:=TFileStream.Create(FileName,fmCreate);
  ms:=TMemoryStream.Create;
  ms.WriteComponent(C);
  ms.Seek(0,soFromBeginning);
  ObjectBinaryToText(ms,fs);
finally
  fs.Free;
  ms.Free;
end;
end;

function LoadComponent;
var
  fs : TFileStream;
  ms : TMemoryStream;
begin
fs:=nil;
ms:=nil;
try
  fs:=TFileStream.Create(FileName,fmOpenRead);
  ms:=TMemoryStream.Create;
  ObjectTextToBinary(fs,ms);
  ms.Seek(0,soFromBeginning);
  Result:=ms.ReadComponent(nil);
finally
  fs.Free;
  ms.Free;
end;
end;

procedure Exchange(var v1,v2 : integer);
var
  buf : integer;
begin
buf := v1;
v1 := v2;
v2 := buf;
end;

procedure ClearPopupMenu(PopupMenu : TPopupMenu);
begin
  while PopupMenu.Items.Count>0 do
    PopupMenu.Items[0].Free;
end;

function _GetPort(address : word) : word;
var
  bValue : byte;
begin
  asm
    mov dx, address
    in al, dx
    mov bValue, al
  end;
  Result := bValue;
end;

procedure _SetPort(address,Value : Word);
var
  bValue : byte;
begin
  bValue := Trunc(Value and 255);
  asm
    mov dx, address
    mov al, bValue
    out dx, al
  end;
end;

procedure WBeep;
const
 LOW_FREQ   = 40;
 HIGH_FREQ  = 5000;
 REST       = 1;
var
  StartTime : DWORD;

  procedure StartBeep(Freq : Word);
  var
    B: Byte;
  begin
    if (Freq >= LOW_FREQ) and (Freq <= HIGH_FREQ)
    then
      begin
        Freq := Word(1193181 div LongInt(Freq));
        B := Byte(_GetPort($61));
        if (B and 3) = 0
        then
          begin
            _SetPort($61, Word(B or 3));
            _SetPort($43, $B6);
          end;
        _SetPort($42, Freq);
        _SetPort($42, Freq shr 8);
      end;
  end; { StartBeep }

  procedure StopBeep;
  var
   Value: Word;
  begin
    Value := _GetPort($61) and $FC;
    _SetPort($61, Value);
  end;  { StopBeep }

begin
if Tone = REST
  then
    begin
      StartTime:=GetTickCount;
      while ( (GetTickCount - StartTime) < DWORD(MSecs) ) do
        Application.ProcessMessages;
      Exit;
    end;

case Win32Platform of
  VER_PLATFORM_WIN32_NT :
    Windows.Beep (Tone, MSecs);
  VER_PLATFORM_WIN32_WINDOWS,VER_PLATFORM_WIN32s :
    begin
      StartBeep(Tone);
      StartTime:=GetTickCount;
      while ( (GetTickCount - StartTime) < DWORD(MSecs) ) do
        Application.ProcessMessages;
      StopBeep;
    end;
end;
end;

function IncDay;
begin
  Result := ADate + Delta;
end;

function DaysBetween;
begin
  Result := Trunc(Date2) - Trunc(Date1) + 1;
  if Result < 0 then Result := 0;
end;

function CutTime;
begin
  Result := Trunc(ADate);
end;

procedure DateDiff;
var
  DtSwap: TDateTime;
  Day1, Day2, Month1, Month2, Year1, Year2: Word;
begin
  if Date1 > Date2 then begin
    DtSwap := Date1;
    Date1 := Date2;
    Date2 := DtSwap;
  end;
  DecodeDate(Date1, Year1, Month1, Day1);
  DecodeDate(Date2, Year2, Month2, Day2);
  Years := Year2 - Year1;
  Months := 0;
  Days := 0;
  if Month2 < Month1 then begin
    Inc(Months, 12);
    Dec(Years);
  end;
  Inc(Months, Month2 - Month1);
  if Day2 < Day1 then begin
    Inc(Days, DaysPerMonth(Year1, Month1));
    if Months = 0 then begin
      Dec(Years);
      Months := 11;
    end
    else Dec(Months);
  end;
  Inc(Days, Day2 - Day1);
end;

function DaysPerMonth;
const
  DaysInMonth: array[1..12] of Integer =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result := DaysInMonth[AMonth];
  if (AMonth = 2) and IsLeapYear(AYear) then Inc(Result); { leap-year Feb is special }
end;

function MonthsBetween;
var
  D, M, Y: Word;
begin
  DateDiff(Date1, Date2, D, M, Y);
  Result := 12 * Y + M;
  if (D > 1) and (D < 7) then Result := Result + 0.25
  else if (D >= 7) and (D < 15) then Result := Result + 0.5
  else if (D >= 15) and (D < 21) then Result := Result + 0.75
  else if (D >= 21) then Result := Result + 1;
end;

function PExtractSubStr;
var
  i : Integer;
  f : boolean;
  b : Integer;
begin
  i := p;
  f := false;
  b := 0;
  while (I <= Length(S)) and (F or (B<>0) or (not (S[I] = ','))) do
    begin
      F:=F xor (S[i]='"');
      if S[i]='(' then Inc(B)
      else
        if S[i]=')' then Dec(B);
      Inc(I);
    end;
  Result := Copy(S, p, I - p);
  if (I <= Length(S)) and (S[I] = ',') then Inc(I);
  p := I;
end;

function  IsThisCharsOnly;
var
  i,l : integer;
begin
i:=1;
l:=Length(s);
while (i<=l) and (s[i] in chars) do Inc(i);
Result:=i>l;
end;

function PosLastChar;
begin
Result:=Length(s);
while (Result>=1) and (s[Result]<>c) do Dec(Result);
end;

function  Copy2SymbFromEnd;
var
  i : integer;
begin
i:=Length(s);
while (i>0) and (s[i]<>c) do Dec(i);
if i>0 then
  Result:=Copy(s,1,i-1)
else
  Result:=s;
end;

function  Copy3SymbFromEnd;
var
  i : integer;
begin
i:=Length(s);
while (i>0) and (s[i]<>c) do Dec(i);
if i>0 then
  Result:=Copy(s,i+1,Length(s))
else
  Result:=s;
end;

function YearMonthToStr;
begin
Result := Format('%2.2d%s%4.4d',[v mod 100,DateSeparator,v div 100]);
end;

function YearMonthToStrFull;
begin
Result := Format('%s %4.4d',[MonthsArray[1,v mod 100],v div 100]);
end;

function GetCurrentYearMonth;
var
  y,m,d : word;
begin
DecodeDate(Now,y,m,d);
Result := y*100+m;
end;

function ValidYearMonth;
begin
Result := ((v mod 100)>=1) and ((v mod 100)<=12) and ((v div 100)>0)
end;

function IncYM;
var
  m,y : integer;
begin
m :=V mod 100;
y :=V div 100;
y :=y+(Delta div 12);
m :=m+(Delta mod 12);
if m>12 then
  begin
    Inc(y);
    m:=m-12;
  end
else
  if m<1 then
    begin
      Dec(y);
      m:=12+m;
    end;
Result:=y*100+m;
end;

function StrToYearMonth;
var
  p,m,y : integer;
  y1,m1,d1 : word;
begin
p:=pos(DateSeparator,v);
if p=0 then
  raise Exception.CreateFmt('"%s" - invalid YearMonth',[v]);
m:=StrToIntDef(Trim(Copy(v,1,p-1)),0);
y:=StrToIntDef(Trim(Copy(v,p+1,Length(v))),-1);
if (m<1) or (m>12) then
  raise Exception.CreateFmt('"%s" - invalid YearMonth',[v]);
if y=-1 then
  begin
    DecodeDate(Now,y1,m1,d1);
    y:=y1;
  end
else
  if y<100 then
    begin
      if y<=30 then y:=y+2000
               else y:=y+1900;
    end;
Result:=y*100+m;
end;

function StrToYearMonthDef;
begin
try
  Result := StrToYearMonth(v);
except
  Result := Def;
end;
end;

function StrToFloatDef;
begin
try
  Result:=StrToFloat(s);
except
  Result:=def;
end;
end;

function StrToDateDef;
begin
try
  Result := StrToDate(s);
except
  Result := def;
end;
end;

function DoEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Result := True;
  end;
end;

function StrToBool;
begin
if AnsiCompareText(V,'true')=0 then Result:=True
else
  if AnsiCompareText(V,'false')=0 then Result:=False
  else
    raise Exception.CreateFmt('StrToBool: invalid string [%s]',[V]);
end;

function StrToBoolDef;
begin
try
  Result:=StrToBool(V);
except
  Result:=def;
end;
end;

function BoolToStr;
begin
if V then Result:='true'
     else Result:='false';
end;

function IsValidVariantType;
var
  T : integer;
begin
T     :=VarType(v);
Result:=(T = varEmpty) or
        (T = varNull) or
        (T = varSmallint) or
        (T = varInteger) or
        (T = varSingle) or
        (T = varDouble) or
        (T = varCurrency) or
        (T = varDate) or
        (T = varOleStr) or
        (T = varDispatch) or
        (T = varError) or
        (T = varBoolean) or
        (T = varVariant) or
        (T = varUnknown) or
        (T = varByte) or
        (T = varString) or
        (T = varTypeMask) or
        (T = varArray) or
        (T = varByRef);
end;

function GetVariantTypeDesc;
begin
case VarType(V) of
  varEmpty    : Result:='Empty';
  varNull     : Result:='NULL';
  varSmallint : Result:='SmallInt';
  varInteger  : Result:='Integer';
  varSingle   : Result:='Single';
  varDouble   : Result:='Double';
  varCurrency : Result:='Currency';
  varDate     : Result:='Date';
  varOleStr   : Result:='OleStr';
  varDispatch : Result:='Dispatch';
  varError    : Result:='Error';
  varBoolean  : Result:='Boolean';
  varVariant  : Result:='Variant';
  varUnknown  : Result:='Unknown';
  varByte     : Result:='Byte';
  varString   : Result:='String';
  varTypeMask : Result:='TypeMask';
  varArray    : Result:='Array';
  varByRef    : Result:='ByRef'
  else          Result:='';
end;
end;

function ExtRound;
begin
if Assigned(prRoundProc) then
  Result := prRoundProc(Value)
else
  Result := Round(Value);
end;

function RoundEx;
begin
Result := Round(Value*RoundArray[n])/RoundArray[n];
end;

procedure ShowForm;
begin
Form.Show;
if Form.WindowState=wsMinimized then
  Form.WindowState:=wsNormal;
end;

function MBox;
begin
Result:=Application.MessageBox(Pchar(Text),PChar(Caption),Flags);
end;

procedure MBError(Text : string);
begin
MBox(Text,prLoadStr(sError),MB_OK or MB_ICONERROR);
end;

procedure MBMessage;
begin
MBox(Text,Caption,MB_OK or MB_ICONINFORMATION);
end;

function GetTempFile(Prefix : string) : string;
var
  TempFile : array [0..255] of char;
  TempDir : array [0..255] of char;
begin
GetTempPath(sizeof(TempDir),TempDir);
GetTempFileName(TempDir,PChar(Prefix),0,TempFile);
Result := StrPas(TempFile);
end;

function GetTempFile(Dir,Prefix : string) : string;
var
  TempFile : array [0..255] of char;
begin
GetTempFileName(PChar(Dir),PChar(Prefix),0,TempFile);
Result:=StrPas(TempFile);
end;

function CompText;
begin
Result := CompareString(GetThreadLocale,
                        SORT_STRINGSORT or NORM_IGNORECASE,
                        PChar(S1),
                        Length(S1),
                        PChar(S2),
                        Length(S2)) - 2;
end;

function MakeStr(C: Char; N: Integer): string;
begin
if N < 1 then
  Result := ''
else
  begin
    SetLength(Result, N);
    FillChar(Result[1], Length(Result), C);
  end;
end;

function AddChar;
begin
  if Length(S) < N then
    Result := MakeStr(C, N - Length(S)) + S
  else Result := S;
end;

function AddCharR;
begin
  if Length(S) < N then
    Result := S + MakeStr(C, N - Length(S))
  else Result := S;
end;

function ExtractSubstr;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and not (S[I] in Delims) do Inc(I);
  Result := Copy(S, Pos, I - Pos);
  if (I <= Length(S)) and (S[I] in Delims) then Inc(I);
  Pos := I;
end;

function CenterStr(const S: string; Len: Integer): string;
begin
  if Length(S) < Len then begin
    Result := MakeStr(' ', (Len div 2) - (Length(S) div 2)) + S;
    Result := Result + MakeStr(' ', Len - Length(Result));
  end
  else Result := S;
end;

procedure CutString;
var
  i : integer;
begin
s:=trim(s);
while length(s)>LenLine do
  begin
    i:=lenline;
    while (i>=1) and (s[i]<>' ') do Dec(i);
    SetLength(Ress,Length(Ress)+1);
    if i=0 then
      begin
        Ress[High(Ress)]:=Copy(s,1,LenLine);
        s:=Copy(s,LenLine+1,Length(s));
      end
    else
      begin
        Ress[High(Ress)]:=Copy(s,1,i-1);
        s:=Copy(s,i+1,Length(s));
      end;
  end;
SetLength(Ress,Length(Ress)+1);
Ress[High(Ress)]:=Trim(s);
end;


////////////////////////////////////////
//
// from number - string, only for RUSSIAN LANGUAGE
//
////////////////////////////////////////
function GSN;
var
  b   : string;
  p   : integer;
  WPart : string;  // цела€ часть
  FPart : string;  // дробна€ часть
  WValue: integer; // значение целой части
  FValue: integer; // значение дробной части

  //
  // ѕолучает миллионы, триллионы и так далее
  //
  function GetCounted(Digit : integer; Number : integer) : string;

    function gi(i : integer) : integer;
    begin
    case Digit of
      1    : Result:=0;
      2..4 : Result:=1;
      else   Result:=2;
    end;
    end;

  begin
  if Number=0 then
    Result:=''
  else
    Result:=GSNArray6[Number,gi(Digit)]
  end;

  //
  // Number - позици€ тройки 1 - дл€ тыс€ч, 0 - сама€ перва€ тройка
  // c1     - старша€ цифра в тройке
  // c2     - средн€€ цифра
  // c3     - младша€
  // Gender - род считаемого (один / одна)
  //
  function ProcessThree(c1,c2,c3 : integer;
                        Number : integer;
                        Gender : TGender) : string;
  begin
  Result:='';
  if c1<>0 then
    Result:=GSNArray2[c1];
  if (c2=0) and (c3=0) then
    Result:=Result+' '+GetCounted(9,Number)
  else
    begin
      if c2=1 then
        Result:=Result+' '+GSNArray1[c3]+' '+GetCounted(9,Number)
      else
        begin
          if c2 in [2..9] then
            Result:=Result+' '+GSNArray3[c2];
          if c3=0 then
            Result:=Result+' '+GetCounted(9,Number)
          else
            case Number of
              0 :
                case Gender of
                  gMale   : Result:=Result+' '+GSNArray4[c3];
                  gFeMale : Result:=Result+' '+GSNArray5[c3];
                end;
              1 :  // тыс€чи
                Result:=Result+' '+GSNArray5[c3]+' '+GetCounted(c3,Number);
              else // миллионы, трилионы и так далее
                Result:=Result+' '+GSNArray4[c3]+' '+GetCounted(c3,Number);
            end;
        end
    end;
  Result:=Trim(Result)+' ';
  end;

  function ProcessPart(s : string; Gender : TGender) : string;
  var
    i,p,index : integer;
  begin
  case (Length(s) mod 3) of
    1: s:='00'+s;
    2: s:='0'+s;
  end;
  // “еперь идем по тройкам
  p := Length(s) div 3;
  Result := '';
  for i:=1 to p do
    begin
      index :=(i-1)*3+1;
      if (i=1) or (Copy(s,index,3)<>'000') then
        Result := Result+ProcessThree(ord(s[index])-ZeroAsciiCode,
                                      ord(s[index+1])-ZeroAsciiCode,
                                      ord(s[index+2])-ZeroAsciiCode,
                                      p-i,
                                      Gender)
    end;
  Result := Trim(Result);
  end;

  function GetCounted2(s : string;
                       sOne, sTwoToFour,sOverFour : string) : string;
  begin
  if (Length(s)>1) and (s[Length(s)-1]='1') then
    Result:=sOverFour
  else
    case s[Length(s)] of
      '1':          Result:=sOne;
      '2'..'4':     Result:=sTwoToFour;
      '5'..'9','0': Result:=sOverFour;
    end;
  end;

begin
Result := '';
Number := Abs(Number);

// собственно обрабатываем в зависимости от типа
case NumberType of
  ntCurrency:
    begin
      // ѕолучаем WPart и FPart
      Number := RoundEx(Number);
      WValue := Trunc(Number);
      FValue := Trunc(Frac(Number)*100+0.5);
      WPart := IntToStr(WValue);
      FPart := IntToStr(FValue);
      // рубли
      if WValue=0 then
        Result := 'ноль '+sOverFourRUB
      else
        Result := ProcessPart(WPart,gMale)+' '+GetCounted2(WPart,sOneRUB,sTwoToFourRUB,sOverFourRUB);
      // копейки
      case CentsFormat of
        cfTwoDigit:
          begin
            if Length(FPart)=1 then
              begin
                case FPart[1] of
                  '1'        : b := sOneKOP;
                  '2','3','4': b := sTwoToFourKOP
                  else         b := sOverFourKOP
                end;
              end
            else
              begin
                if FPart[1]='1' then
                  b := sOverFourKOP
                else
                  case FPart[2] of
                    '1'        : b := sOneKOP;
                    '2','3','4': b := sTwoToFourKOP
                    else         b := sOverFourKOP
                  end;
              end;
            Result := Result+' '+AddChar('0',FPart,2)+' '+b;
          end;
        cfTwoDigitWithCentsText :
          Result := Result+' '+AddChar('0',FPart,2)+' '+CentsText;
      end;
    end;
  ntSimple:
    begin
      // ѕолучаем WPart и FPart
      b := FloatToStr(Number);
      p := pos(DecimalSeparator,b);
      if p=0 then
        begin
          WPart := b;
          WValue := StrToInt(WPart);
          FPart := '';
          FValue := 0;
        end
      else
        begin
          WPart := Copy(b,1,p-1);
          WValue := StrToInt(WPart);
          FPart := Copy(b,p+1,Length(b));
          FValue := StrToInt(FPart);
        end;

      if FValue=0 then
        begin
          if WValue=0 then
            Result := 'ноль '+sOverFour
          else
            Result := ProcessPart(WPart,Gender)+' '+GetCounted2(WPart,sOne,sTwoToFour,sOverFour);
        end
      else
        begin
          if WValue=0 then
            Result := 'ноль целых'
          else
            Result := ProcessPart(WPart,gFemale)+' '+GetCounted2(WPart,'цела€','целых','целых');
          Result := Result+' '+ProcessPart(FPart,gFemale)+' ';
          if FPart[Length(FPart)]='1' then
            Result := Result+GSNArray7[Length(FPart),1]
          else
            Result := Result+GSNArray7[Length(FPart),2];
          Result := Result+' '+sTwoToFour
        end;
    end
  else
    Result := '';
end;
Result := Trim(Result);
Result := AnsiUpperCase(Copy(Result,1,1))+Copy(Result,2,Length(Result));
end;

function Create90Font(Font : TFont) : HFont;
var
  F : TLogFont;
begin
  GetObject(Font.Handle, SizeOf(TLogFont), @F);
  F.lfEscapement := 900;
  F.lfOrientation := 900;
  Result := CreateFontIndirect(F);
end;

function CreateAPIFont(Font : TFont) : HFont;
var
  F : TLogFont;
begin
  GetObject(Font.Handle,SizeOf(TLogFont),@F);
  Result := CreateFontIndirect(F);
end;

function  CreateDefFont(DC : HDC; Size : integer; Color : TColor) : HFont;
var
  F : TLogFont;
begin
  SetTextColor(DC,Color);
  ZeroMemory(@F,sizeof(F));
  F.lfHeight := -MulDiv(Size, GetDeviceCaps(DC, LOGPIXELSY), 72);
  F.lfFaceName := 'Arial';
  F.lfCharSet := DEFAULT_CHARSET;
  Result := CreateFontIndirect(F);
end;

function  Create90DefFont(DC : HDC; Size : integer; Color : TColor) : HFont;
var
  F : TLogFont;
begin
  SetTextColor(DC,Color);
  ZeroMemory(@F,sizeof(F));
  F.lfHeight := -MulDiv(Size, GetDeviceCaps(DC, LOGPIXELSY), 72);
  F.lfFaceName := 'Arial';
  F.lfCharSet := DEFAULT_CHARSET;
  F.lfEscapement :=  900;
  F.lfOrientation :=  900;
  Result := CreateFontIndirect(F);
end;

function GetFontSize(const FontName : string; FontSize : integer; FontStyle : TFontStyles) : TSize;
var
  DC : HDC;
  Font : TFont;
  newfont,oldfont : HFONT;
begin
Font := TFont.Create;
try
  Font.Name := FontName;
  Font.Size := FontSize;
  Font.Style := Font.Style;
  DC := GetDC(0);
  newfont := CreateAPIFont(Font);
  oldfont := SelectObject(DC,newfont);
  GetTextExtentPoint32(DC,'W',1,Result);
  SelectObject(DC,oldfont);
  DeleteObject(newfont);
  ReleaseDC(0,DC);
finally
  Font.Free;
end;
end;

procedure AddRectToRegion(var Rgn : HRGN; const r : TRect; Op : integer = RGN_OR);
var
  Rgn2 : HRGN;
begin
if Rgn=0 then
  Rgn:=CreateRectRgnIndirect(r)
else
  begin
    Rgn2:=CreateRectRgnIndirect(r);
    CombineRgn(Rgn,Rgn,Rgn2,Op);
    DeleteObject(Rgn2);
  end;
end;

function NormalizeRect(X1,Y1,X2,Y2 : integer) : TRect;
begin
if X1>X2 then
  begin
    Result.Left := X2;
    Result.Right := X1;
  end
else
  begin
    Result.Left := X1;
    Result.Right := X2;
  end;
if Y1>Y2 then
  begin
    Result.Top := Y2;
    Result.Bottom := Y1;
  end
else
  begin
    Result.Top := Y1;
    Result.Bottom := Y2;
  end;
end;

function FindTextInString(const s : string; const SearchString : string; WholeWords,CaseSensitive : boolean) : integer;
var
  Buf,ls,lSearchString : string;
begin
if not CaseSensitive then
  begin
    ls := AnsiUpperCase(s);
    lSearchString := AnsiUpperCase(SearchString);
  end;
if not WholeWords then
  Result := pos(lSearchString,ls)
else
  begin
    Result := 1;
    while Result<=Length(ls) do
      begin
        Buf := ExtractSubStr(ls,Result,sWordsDelimiters);
        if Buf=lSearchString then exit;
      end;
    Result := 0;
  end;
end;

function ReplaceTextInString(const s : string; const SearchString,ReplaceString : string; WholeWords,CaseSensitive : boolean) : string;
var
  p,pp : integer;
  Buf,ls,lSearchString : string;
begin
if not WholeWords then
  begin
    if CaseSensitive then
      Result := StringReplace(s,SearchString,ReplaceString,[rfReplaceAll])
    else
      Result := StringReplace(s,SearchString,ReplaceString,[rfReplaceAll,rfIgnoreCase]);
  end
else
  begin
    if not CaseSensitive then
      begin
        ls := AnsiUpperCase(s);
        lSearchString := AnsiUpperCase(SearchString);
      end;
    Result := s;
    p := 1;
    while p<=Length(ls) do
      begin
        pp := p;
        Buf := ExtractSubStr(ls,p,sWordsDelimiters);
        if Buf=lSearchString then
          begin
            Delete(Result,pp,Length(SearchString));
            Insert(ReplaceString,Result,pp);
            Delete(ls,pp,Length(SearchString));
            Insert(ReplaceString,ls,pp);
            p := p+Length(ReplaceString)-Length(SearchString);
          end;
      end;
  end;
end;

procedure DecompileProp(o : TObject; var obj : TObject; var lpi : PPropInfo; HasSubProp : boolean; const PropName,ClassPropName,SubPropName : string);
var
  td : PTypeData;
begin
if HasSubProp then
  begin
    lpi := GetPropInfo(o.ClassInfo,ClassPropName);
    if (lpi<>nil) and (lpi^.PropType^.Kind=tkClass) then
      begin
        Obj := TObject(GetOrdProp(o,lpi));
        td := GetTypeData(lpi^.PropType^);
        lpi := GetPropInfo(td.ClassType.ClassInfo,SubPropName);
      end;
  end
else
  begin
    Obj := o;
    lpi := GetPropInfo(o.ClassInfo,PropName);
  end;
end;

procedure prFindTextInProps(Obj : TPersistent; ObjectRef : TPersistent; const Text : string; WholeWords,CaseSensitive : boolean; FoundList : TprTemplateFoundList);
const
  aPropsTypes = [tkString,tkWChar, tkLString, tkWString,tkClass];

  procedure CheckObj(o : TPersistent; const PropPrefix : string);
  var
    SubObj : TPersistent;
    PropValue : string;
    PropsList : PPropList;
    i,PropsCount : integer;
  begin
  PropsCount := GetPropList(o.ClassInfo,aPropsTypes,nil);
  if PropsCount=0 then exit;
  GetMem(PropsList,PropsCount*sizeof(PPropInfo));
  try
    GetPropList(o.ClassInfo,aPropsTypes,PropsList);
    for i:=0 to PropsCount-1 do
      if PropsList[i]^.PropType^.Kind=tkClass then
        begin
          SubObj := TPersistent(GetOrdProp(o,PropsList[i]));
          if (SubObj<>nil) and not (SubObj is TComponent) then
            begin
              if SubObj is TStrings then
                begin
                  PropValue := TStrings(SubObj).Text;
                  if FindTextInString(PropValue,Text,WholeWords,CaseSensitive)<>0 then
                    FoundList.AddObject(ObjectRef,o,PropPrefix+PropsList[i]^.Name,PropValue);
                end
              else
                begin
                  CheckObj(SubObj,PropsList[i]^.Name+'.');
                end;
            end
        end
      else
        begin
          PropValue := GetStrProp(o,PropsList[i]);
          if FindTextInString(PropValue,Text,WholeWords,CaseSensitive)<>0 then
            FoundList.AddObject(ObjectRef,o,PropPrefix+PropsList[i]^.Name,PropValue);
        end;
  finally
    FreeMem(PropsList);
  end;
  end;

begin
CheckObj(Obj,'');
end;

procedure prReplaceTextInProp(Obj : TPersistent; const PropName,FindText,ReplaceText : string; WholeWords,CaseSensitive : boolean);
var
  p : integer;
  lpi : PPropInfo;
  o,SubObj : TObject;
  PropValue,ClassPropName,SubPropName : string;
begin
p := pos('.',PropName);
if p<>0 then
  begin
    ClassPropName := Copy(PropName,1,p-1);
    SubPropName := Copy(PropName,p+1,Length(PropName));
  end;

DecompileProp(Obj,o,lpi,p<>0,PropName,ClassPropName,SubPropName);
if (o<>nil) and (lpi<>nil) then
  begin
    case lpi^.PropType^.Kind of
      tkClass:
        begin
          SubObj := TObject(GetOrdProp(o,lpi));
          if (SubObj<>nil) and (SubObj is TStrings) then
            begin
              PropValue := TStrings(SubObj).Text;
              TStrings(SubObj).Text := ReplaceTextInString(PropValue,FindText,ReplaceText,WholeWords,CaseSensitive);
            end;
        end;
      tkString,tkLString:
        begin
          PropValue := GetStrProp(o,lpi);
          SetStrProp(o,lpi,ReplaceTextInString(PropValue,FindText,ReplaceText,WholeWords,CaseSensitive));
        end;
    end;
  end;
end;

initialization

  ScreenDC := GetDC(0);

finalization

  ReleaseDC(0, ScreenDC);
  
end.

