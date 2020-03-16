{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxUtils;

interface

{$I PR.INC}

uses
  SysUtils, Windows, Classes, WinSpool, stdctrls, messages, clipbrd,
  CommDlg, Forms, inifiles, Grids, Graphics,

  pr_TxConsts, pr_Common, pr_MultiLang, pr_TxClasses;

type

/////////////////////////////////////////////////
//
// TprFixedFont
//
/////////////////////////////////////////////////
{Represents the fixed font.}
TprFixedFont = class(TprOptions)
private
  FName : string;
  FSize : integer;
  procedure SetName(Value : string);
  procedure SetSize(Value : integer);
  function StoredName : boolean;
public
{Creates an instance of the TprFixedFont class.}
  constructor Create;
{Copies the contents of another, similar object.
Parameters:
  Source - The source object.}
  procedure Assign(Source: TPersistent); override;
{See;
  TprOptions.WriteToIni}
  procedure WriteToIni(IniFile: TIniFile; const SectionName, Prefix: string); override;
{See;
  TprOptions.ReadFromIni}
  procedure ReadFromIni(IniFile: TIniFile; const SectionName, Prefix: string); override;
published
{Specifies the name of font.}
  property Name: string read FName write SetName stored StoredName;
{Specifies the height of the font in points.}
  property Size: integer read FSize write SetSize default 14;
end;

  /////////////////////////////////////////////////
  //
  // TprTxOEMMemo
  //
  /////////////////////////////////////////////////
  TprTxOEMMemo = class(TMemo)
  private
    FReport: TprTxReport;
    procedure SetReport(Value: TprTxReport);
    procedure WMCopy(var Msg: TWMCopy); message WM_COPY;
    procedure WMPaste(var Msg: TWMPaste); message WM_PASTE;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
  public
    constructor Create(AOwner: TComponent); override;
    property Report: TprTxReport read FReport write SetReport;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxCharGrid
  //
  /////////////////////////////////////////////////
  TprTxCharGrid = class(TDrawGrid)
  private
    function GetFontCharset: TFontCharset;
    procedure SetFontCharset(Value: TFontCharset);
    function GetSelectedChar: Char;
    procedure SetSelectedChar(Value: Char);
  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TComponent); override;
    property FontCharset: TFontCharset read GetFontCharset write SetFontCharset;
    property SelectedChar: Char read GetSelectedChar write SetSelectedChar;
  end;

implementation

uses
  pr_Strings, pr_Utils;

/////////////////////////////////////////////////
//
// TprFixedFont
//
/////////////////////////////////////////////////
constructor TprFixedFont.Create;
begin
inherited Create;
FName := sFixedFont;
FSize := 14;
end;

function TprFixedFont.StoredName : boolean;
begin
Result := (FName<>'') and (FName<>sFixedFont);
end;

procedure TprFixedFont.SetName(Value : string);
begin
if FName=Value then exit;
FName := Value;
DoChanged;
end;

procedure TprFixedFont.SetSize(Value : integer);
begin
if FSize=Value then exit;
FSize := Value;
DoChanged;
end;

procedure TprFixedFont.Assign(Source : TPersistent);
begin
with TprFixedFont(Source) do
  begin
    Self.FName := Name;
    Self.FSize := Size;
  end;
DoChanged;
end;

procedure TprFixedFont.WriteToIni(IniFile : TIniFile; const SectionName,Prefix : string);
begin
IniFile.WriteString(SectionName,Prefix+'Name',Name);
IniFile.WriteInteger(SectionName,Prefix+'Size',Size);
end;

procedure TprFixedFont.ReadFromIni(IniFile : TIniFile; const SectionName,Prefix : string);
begin
Name := IniFile.ReadString(SectionName,Prefix+'Name',Name);
Size := IniFile.ReadInteger(SectionName,Prefix+'Size',Size);
end;

/////////////////////////////////////////////////
//
// TprTxOEMMemo
//
/////////////////////////////////////////////////
constructor TprTxOEMMemo.Create;
begin
  inherited;
  Font.Name := sOemFontName;
  Font.Size := 12;
  Font.Charset := OEM_CHARSET;
  FReport := nil;
end;

procedure TprTxOEMMemo.SetReport(Value : TprTxReport);
begin
  if FReport <> Value then
  begin
    FReport := Value;
    if FReport <> nil then
      Font.CharSet := FReport.FontCharSet
    else
      Font.CharSet := OEM_CHARSET;
  end;
end;

procedure TprTxOEMMemo.WMPaste;
var
  I: integer;
  S: string;
begin
  S := ClipBoard.AsText;
  for I := 1 to Length(S) do
    if s[I]<>#10 then
      SendMessage(Handle,WM_CHAR,integer(s[i]),0);
end;

procedure TprTxOEMMemo.WMCopy;
var
  s : string;
begin
inherited;
if FReport<>nil then
  begin
    s := ClipBoard.AsText;
    FReport.OEMtoWIN(PChar(s),PChar(s));
    ClipBoard.AsText := s;
  end;
end;

procedure TprTxOEMMemo.WMChar;
var
  s : array [0..1] of char;
begin    
if FReport<>nil then
  begin
    s[0] := char(Msg.CharCode);
    s[1] := #0;
    FReport.WINtoOEM(s,s);
    Msg.CharCode := word(s[0]);
  end;
inherited;
end;

/////////////////////////////////////////////////
//
// TprTxCharGrid
//
/////////////////////////////////////////////////
constructor TprTxCharGrid.Create(AOwner: TComponent);
begin
  inherited;
  ColCount := 16;
  RowCount := 16;
  FixedCols := 0;
  FixedRows := 0;
  DefaultColWidth := 21;
  DefaultRowHeight := 21;
  ScrollBars := ssVertical;
  Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected];
end;

function TprTxCharGrid.GetFontCharset: TFontCharset;
begin
  Result := Font.Charset;
end;

procedure TprTxCharGrid.SetFontCharset(Value: TFontCharset);
begin
  Font.Charset := Value;
end;

function TprTxCharGrid.GetSelectedChar: Char;
begin
  Result := Char(Row * 16 + Col);
end;

procedure TprTxCharGrid.SetSelectedChar(Value: Char);
begin
{$IFDEF PR_D6_D7}
  FocusCell(Byte(Value) mod 16, Byte(Value) div 16, True);
{$ELSE}
  Row := Byte(Value) div 16;
  Col := Byte(Value) mod 16;
{$ENDIF}
end;

procedure TprTxCharGrid.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  S: Char;
  ANewFont, AOldFont: HFONT;
begin
  S := chr(ARow * 16 + ACol);
  ANewFont := CreateTxDraftFont(sOemFontName, 14);
  AOldFont := SelectObject(Canvas.Handle, ANewFont);
  DrawTextEx(Canvas.Handle, @S, 1, ARect, DT_CENTER + DT_VCENTER + DT_SINGLELINE, nil);
  SelectObject(Canvas.Handle, AOldFont);
  DeleteObject(ANewFont);
end;

end.

