{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the TprTxHLineObj and TprTxVLineObj classes, that can be used for inserting the vertical and horizontal lines in your report.}
unit pr_TxAddon;

interface

{$i pr.inc}

uses
  SysUtils, Classes, Windows, menus, Graphics,

  {pr_Classes, } pr_Common, pr_TxConsts, pr_MultiLang, pr_TxClasses;

type

  /////////////////////////////////////////////////
  //
  // TprTxLineObjRecVersion
  //
  /////////////////////////////////////////////////
{Represents the object view for TprTxLineObj.
This is a base class for TprTxVLineObjRecVersion and TprTxHLineObjRecVersion classes.
See also:
  TprTxLineObj, TprTxVLineObjRecVersion, TprTxHLineObjRecVersion}
  TprTxLineObjRecVersion = class(TprObjRecVersion)
  private
    FMainChar: char;
    FStartingChar: char;
    FEndingChar: char;
    FDefaultFont: Boolean;
    FTxFontStyleEx: TprTxFontStyle;
    FTxFontOptionsEx: TprTxFontOptions;
    procedure SetTxFontOptionsEx(Value: TprTxFontOptions);
    function StoredTxFontOptionsEx: Boolean;
    function StoredTxFontStyleEx: Boolean;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadTxFontStyleEx(Reader: TReader);
    procedure WriteTxFontStyleEx(Writer: TWriter);
    procedure ReadTxFontOptionsEx(Reader: TReader);
    procedure WriteTxFontOptionsEx(Writer: TWriter);
    function BuildMemoObjRecVersion(AWidth, AHeight: Integer; AStarting, AMain, AEnding: Char): TprTxMemoObjRecVersion; virtual;
  public
{Creates an instance of the TprTxLineObjRecVersion class.}
    constructor Create(Collection: TCollection); override;
{Frees an instance of the TprTxLineObjRecVersion class.}
    destructor Destroy; override;

{See:
  TprObjRecVersion.Assign}
    procedure Assign(Source: TPersistent); override;

{Specifies the font style for object.
See also:
  TprTxFontStyle}
    property TxFontStyleEx: TprTxFontStyle read FTxFontStyleEx write FTxFontStyleEx;
{Specifies the font options for object (bold, italic).
See also:
  TprTxFontStyle}
    property TxFontOptionsEx: TprTxFontOptions read FTxFontOptionsEx write SetTxFontOptionsEx;
{Specifies the symbol, that forms a line except for the starting and ending symbols.
Char must be specified in OEM codepage.}
    property MainChar: char read FMainChar write FMainChar;
{Specifies the starting symbol of line.
Char must be specified in OEM codepage.}
    property StartingChar: char read FStartingChar write FStartingChar;
{Specifies the ending symbol of line.
Char must be specified in OEM codepage.}
    property EndingChar: char read FEndingChar write FEndingChar;
  published
{Specifies the value indicating whether the default font (specified for page) must be used for
printing the memo. The TxFontStyleEx and TxFontOptionsEx properties
are ignored if this property equals to true.}
    property DefaultFont: Boolean read FDefaultFont write FDefaultFont default False;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxLineObjRec
  //
  /////////////////////////////////////////////////
{Represents a record of properties for the TprTxLineObj object.
See also:
  TprTxLineObj, TprTxLineObjRecVersion}
  TprTxLineObjRec = class(TprObjRec)
  private
    FCanSplit: Boolean;
  protected
    function GetSupportSplitting: Boolean; override;
    function GetCanSplitValue: Boolean; override;
    procedure SetCanSplitValue(Value: Boolean); override;
  public
{See:
  TprObjRec.ThirdPass}
    procedure ThirdPass(AEndPage: TprCustomEndPage; Device : TObject; const r : TRect); override;
  published
{Specifies the value indicating whether the object can split should it fall on a page break.}
    property CanSplit;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxLineObj
  //
  /////////////////////////////////////////////////
  TprTxLineObj = class(TprObj)
  private
    function GetVersion(Index: Integer): TprTxLineObjRecVersion;
    function GetGenVersion(Index: Integer): TprTxLineObjRecVersion;
    function GetDefVersion: TprTxLineObjRecVersion;
    function GetGenCurVersion: TprTxLineObjRecVersion;
  protected
    procedure OnDsgnPopupMenuClick(Sender: TObject);
    procedure OnDsgnPopupMenuFontStyleClick(Sender: TObject);
    procedure OnDsgnPopupMenuFontOptionClick(Sender: TObject);
    function FirstPassCreateRec: TprTxLineObjRec; virtual; abstract;
  public
{See:
  TprObj.DsgnDefinePopupMenu}
    procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: boolean); override;
{See:
  TprObj.FirstPass}
    procedure FirstPass; override;

{See:
  TprObj.Versions}
    property Versions[Index: Integer]: TprTxLineObjRecVersion read GetVersion;
{See:
  TprObj.GenVersions}
    property GenVersions[Index: Integer]: TprTxLineObjRecVersion read GetGenVersion;
{See:
  TprObj.DefVersion}
    property DefVersion: TprTxLineObjRecVersion read GetDefVersion;
{See:
  TprObj.GenCurVersion}
    property GenCurVersion: TprTxLineObjRecVersion read GetGenCurVersion;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxHLineObjRecVersion
  //
  /////////////////////////////////////////////////
{Represents the object view for TprTxHLineObj.
See also:
  TprTxHLineObj}
  TprTxHLineObjRecVersion = class(TprTxLineObjRecVersion)
  protected
    function BuildMemoObjRecVersion(AWidth, AHeight: Integer; AStarting, AMain, AEnding: Char): TprTxMemoObjRecVersion; override;
  public
{Creates an instance of the TprTxHLineObjRecVersion class.}
    constructor Create(Collection: TCollection); override;
  published
{See:
  TprTxLineObjRecVersion.MainChar}
    property MainChar default '-';
{See:
  TprTxLineObjRecVersion.StartingChar}
    property StartingChar default '-';
{See:
  TprTxLineObjRecVersion.EndingChar}
    property EndingChar default '-';
  end;

  /////////////////////////////////////////////////
  //
  // TprTxHLineObjRec
  //
  /////////////////////////////////////////////////
{Represents a record of properties for the TprTxHLineObj object.
See also:
  TprTxHLineObj, TprTxHLineObjRecVersion}
  TprTxHLineObjRec = class(TprTxLineObjRec)
  protected
    function GetCanSplit(AByHorizontal: Boolean; ASplitPos: Integer): Boolean; override;
    function Split(AByHorizontal: Boolean; ASplitPos: Integer; var AAddToSplitted: Integer): TprObjRec; override;

    function GetVersionClass: TprObjVersionClass; override;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxHLineObj
  //
  /////////////////////////////////////////////////
{TprTxHLineObj is intended for inserting the horizontal lines in your report.
See also:
  TprTxHLineObj}
  TprTxHLineObj = class(TprTxLineObj)
  private
    function GetVersion(Index: Integer): TprTxHLineObjRecVersion;
    function GetGenVersion(Index: Integer): TprTxHLineObjRecVersion;
    function GetDefVersion: TprTxHLineObjRecVersion;
    function GetGenCurVersion: TprTxHLineObjRecVersion;
  protected
    procedure InitdRec; override;
    function FirstPassCreateRec: TprTxLineObjRec; override;
  public
{See:
  TprObj.DrawDesign}
    procedure DrawDesign(DC: HDC; ExData: pointer; const DrawRect: TRect); override;
{See:
  TprObj.GetDesc}
    function GetDesc : string; override;

{See:
  TprObj.Versions}
    property Versions[Index: Integer]: TprTxHLineObjRecVersion read GetVersion;
{See:
  TprObj.GenVersions}
    property GenVersions[Index: Integer]: TprTxHLineObjRecVersion read GetGenVersion;
{See:
  TprObj.DefVersion}
    property DefVersion: TprTxHLineObjRecVersion read GetDefVersion;
{See:
  TprObj.GenCurVersion}
    property GenCurVersion: TprTxHLineObjRecVersion read GetGenCurVersion;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxVLineObjRecVersion
  //
  /////////////////////////////////////////////////
{Represents the object view for TprTxVLineObj.
See also:
  TprTxVLineObj}
  TprTxVLineObjRecVersion = class(TprTxLineObjRecVersion)
  protected
    function BuildMemoObjRecVersion(AWidth, AHeight: Integer; AStarting, AMain, AEnding: Char): TprTxMemoObjRecVersion; override;
  public
{Creates an instance of the TprTxVLineObjRecVersion class.}
    constructor Create(Collection: TCollection); override;
  published
{See:
  TprTxLineObjRecVersion.MainChar}
    property MainChar default '|';
{See:
  TprTxLineObjRecVersion.StartingChar}
    property StartingChar default '|';
{See:
  TprTxLineObjRecVersion.EndingChar}
    property EndingChar default '|';
  end;

  /////////////////////////////////////////////////
  //
  // TprTxVLineObjRec
  //
  /////////////////////////////////////////////////
{Represents a record of properties for the TprTxVLineObj object.
See also:
  TprTxVLineObj, TprTxVLineObjRecVersion}
  TprTxVLineObjRec = class(TprTxLineObjRec)
  protected
    function GetCanSplit(AByHorizontal: Boolean; ASplitPos: Integer): Boolean; override;
    function Split(AByHorizontal: Boolean; ASplitPos: Integer; var AAddToSplitted: Integer): TprObjRec; override;

    function GetVersionClass: TprObjVersionClass; override;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxVLineObj
  //
  /////////////////////////////////////////////////
{TprTxVLineObj is intended for inserting the vertical lines in your report.
See also:
  TprTxVLineObj}
  TprTxVLineObj = class(TprTxLineObj)
  private
    function GetVersion(Index: Integer): TprTxVLineObjRecVersion;
    function GetGenVersion(Index: Integer): TprTxVLineObjRecVersion;
    function GetDefVersion: TprTxVLineObjRecVersion;
    function GetGenCurVersion: TprTxVLineObjRecVersion;
  protected
    procedure InitdRec; override;
    function FirstPassCreateRec: TprTxLineObjRec; override;
  public
{See:
  TprObj.DrawDesign}
    procedure DrawDesign(DC: HDC; ExData: pointer; const DrawRect: TRect); override;
{See:
  TprObj.GetDesc}
    function GetDesc : string; override;

{See:
  TprObj.Versions}
    property Versions[Index: Integer]: TprTxVLineObjRecVersion read GetVersion;
{See:
  TprObj.GenVersions}
    property GenVersions[Index: Integer]: TprTxVLineObjRecVersion read GetGenVersion;
{See:
  TprObj.DefVersion}
    property DefVersion: TprTxVLineObjRecVersion read GetDefVersion;
{See:
  TprObj.GenCurVersion}
    property GenCurVersion: TprTxVLineObjRecVersion read GetGenCurVersion;
  end;

implementation

uses
  pr_Utils, pr_Strings;

type
  TprObjRecAccess = class(TprObjRec)
  end;

/////////////////////////////////////////////////
//
// TprTxLineObjRecVersion
//
/////////////////////////////////////////////////
constructor TprTxLineObjRecVersion.Create(Collection: TCollection);
begin
  inherited;
  FTxFontOptionsEx := TprTxFontOptions.Create;
end;

destructor TprTxLineObjRecVersion.Destroy;
begin
  FTxFontOptionsEx.Free;
  inherited;
end;

procedure TprTxLineObjRecVersion.Assign(Source: TPersistent);
begin
  if Source is TprTxLineObjRecVersion then
    with TprTxLineObjRecVersion(Source) do
    begin
      Self.FMainChar := MainChar;
      Self.FStartingChar := StartingChar;
      Self.FEndingChar := EndingChar;
      Self.FDefaultFont := DefaultFont;
      Self.FTxFontStyleEx := FTxFontStyleEx;
      Self.FTxFontOptionsEx.Assign(FTxFontOptionsEx);
    end;
  inherited;
end;

procedure TprTxLineObjRecVersion.SetTxFontOptionsEx(Value: TprTxFontOptions);
begin
  FTxFontOptionsEx.Assign(Value);
end;

function TprTxLineObjRecVersion.StoredTxFontOptionsEx : boolean;
begin
  Result := FTxFontOptionsEx.Count > 0;
end;

function TprTxLineObjRecVersion.StoredTxFontStyleEx : boolean;
begin
Result := FTxFontStyleEx <> nil;
end;

procedure TprTxLineObjRecVersion.DefineProperties(Filer : TFiler);
begin
  inherited;
  Filer.DefineProperty('TxFontStyleEx', ReadTxFontStyleEx, WriteTxFontStyleEx, StoredTxFontStyleEx);
  Filer.DefineProperty('TxFontOptionsEx', ReadTxFontOptionsEx, WriteTxFontOptionsEx, StoredTxFontOptionsEx);
end;

procedure TprTxLineObjRecVersion.ReadTxFontStyleEx(Reader: TReader);
begin
  FTxFontStyleEx := TxReportOptions.FindTxFontStyle(Reader.ReadIdent);
end;

procedure TprTxLineObjRecVersion.WriteTxFontStyleEx(Writer: TWriter);
begin
  if FTxFontStyleEx <> nil then
    Writer.WriteIdent(FTxFontStyleEx.Name);
end;

procedure TprTxLineObjRecVersion.ReadTxFontOptionsEx(Reader : TReader);
var
  TxFontOption: TprTxFontOption;
begin
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    TxFontOption := TxReportOptions.FindTxFontOption(Reader.ReadIdent);
    if TxFontOption <> nil then
      FTxFontOptionsEx.Add(TxFontOption);
  end;
  Reader.ReadListEnd;
end;

procedure TprTxLineObjRecVersion.WriteTxFontOptionsEx(Writer : TWriter);
var
  i: integer;
begin
  Writer.WriteListBegin;
  for i := 0 to FTxFontOptionsEx.Count - 1 do
    Writer.WriteIdent(FTxFontOptionsEx[i].Name);
  Writer.WriteListEnd;
end;

function TprTxLineObjRecVersion.BuildMemoObjRecVersion(AWidth, AHeight: Integer; AStarting, AMain, AEnding: Char): TprTxMemoObjRecVersion;
begin
  Result := TprTxMemoObjRecVersion.Create(nil);
  with Result do
  begin
    DefaultFont := Self.DefaultFont;
    TxFontStyleEx := Self.TxFontStyleEx;
    TxFontOptionsEx := Self.TxFontOptionsEx;
  end;
end;

/////////////////////////////////////////////////
//
// TprTxLineObjRec
//
/////////////////////////////////////////////////
function TprTxLineObjRec.GetSupportSplitting: Boolean;
begin
  Result := True;
end;

function TprTxLineObjRec.GetCanSplitValue: Boolean;
begin
  Result := FCanSplit;
end;

procedure TprTxLineObjRec.SetCanSplitValue(Value: Boolean);
begin
  FCanSplit := Value;
end;

procedure TprTxLineObjRec.ThirdPass(AEndPage: TprCustomEndPage; Device : TObject; const r : TRect);
var
  AMemoVersion: TprTxMemoObjRecVersion;
  AStarting, AMain, AEnding: Char;
begin
  if not Versions[CurVersion].Visible then exit;

  with TprTxLineObjRecVersion(Versions[CurVersion]) do
  begin
    with TprTxReport(Obj.Report) do
    begin
      AStarting := OemToWin(StartingChar);
      AMain := OemToWin(MainChar);
      AEnding := OemToWin(EndingChar);
    end;
    AMemoVersion := BuildMemoObjRecVersion(Right - Left, Bottom - Top, AStarting, AMain, AEnding);
    //for I := 0 to AMemoVersion.Memo.Count - 1 do
    //  TprTxReport(Obj.Report).OemToWin(PChar(AMemoVersion.Memo[I]), PChar(AMemoVersion.Memo[I]));
    try
      TprTextDevice(Device).PlaceTxMemo(AMemoVersion, r, PreviewUserData);
    finally
      AMemoVersion.Free;
    end;
  end;
  FPreviewUserData := nil;
end;

/////////////////////////////////////////////////
//
// TprTxLineObj
//
/////////////////////////////////////////////////
function TprTxLineObj.GetVersion(Index: Integer): TprTxLineObjRecVersion;
begin
  Result := TprTxLineObjRecVersion(inherited Versions[Index]);
end;

function TprTxLineObj.GetGenVersion(Index: Integer): TprTxLineObjRecVersion;
begin
  Result := TprTxLineObjRecVersion(inherited GenVersions[Index]);
end;

function TprTxLineObj.GetDefVersion: TprTxLineObjRecVersion;
begin
  Result := TprTxLineObjRecVersion(inherited DefVersion);
end;

function TprTxLineObj.GetGenCurVersion: TprTxLineObjRecVersion;
begin
  Result := TprTxLineObjRecVersion(inherited GenCurversion);
end;

procedure TprTxLineObj.OnDsgnPopupMenuClick(Sender: TObject);
begin
  with TprTxMemoObjRecVersion(DefVersion) do
    case TMenuItem(Sender).Tag of
      -1: DefaultFont := not DefaultFont;
    end;
  DsgnNotifyDesigner;
end;

procedure TprTxLineObj.DsgnDefinePopupMenu(Popup : TPopupMenu; OneObjectSelected : boolean);
var
  I: Integer;
  m: TMenuItem;
begin
  inherited;
  if Popup.Items.Count>0 then
    AddPopupMenuItem(Popup, nil, 0, '', nil, '', 0, False, False);
  
  AddPopupMenuItem(Popup, nil, sTxMemoObjDefaultFont, '', OnDsgnPopupMenuClick, '', -1, True, TprTxLineObjRecVersion(DefVersion).DefaultFont);
  m := AddPopupMenuItem(Popup, nil, sTxMemoObjFontStyle, '', nil, '', 0, True, False);
  for I := 0 to TxReportOptions.TxFontStylesCount - 1 do
    AddPopupMenuItemS(Popup,
                      m,
                      TxReportOptions.TxFontStyles[I].Description,
                      '',
                      OnDsgnPopupMenuFontStyleClick,
                      '',
                      I,
                      True,
                      (TprTxLineObjRecVersion(DefVersion).TxFontStyleEx <> nil) and
                      (TprTxLineObjRecVersion(DefVersion).TxFontStyleEx.Name = TxReportOptions.TxFontStyles[I].Name));
  
  m := AddPopupMenuItem(Popup, nil, sTxMemoObjFontOptions, '', nil, '', 0, True, False);
  for I := 0 to TxReportOptions.TxFontOptionsCount - 1 do
    AddPopupMenuItemS(Popup,
                      m,
                      TxReportOptions.TxFontOptions[I].Description,
                      '',
                      OnDsgnPopupMenuFontOptionClick,
                      '',
                      I,
                      true,
                      TprTxLineObjRecVersion(DefVersion).TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[I]));
end;

procedure TprTxLineObj.OnDsgnPopupMenuFontStyleClick(Sender: TObject);
begin
  with TprTxLineObjRecVersion(DefVersion) do
    TxFontStyleEx := TxReportOptions.TxFontStyles[TMenuItem(Sender).Tag];
  DsgnNotifyDesigner;
end;

procedure TprTxLineObj.OnDsgnPopupMenuFontOptionClick(Sender: TObject);
begin
  with TprTxLineObjRecVersion(DefVersion) do
    if TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[TMenuItem(Sender).Tag]) then
      TxFontOptionsEx.Remove(TxReportOptions.TxFontOptions[TMenuItem(Sender).Tag])
    else
      TxFontOptionsEx.Add(TxReportOptions.TxFontOptions[TMenuItem(Sender).Tag]);
  DsgnNotifyDesigner;
end;

procedure TprTxLineObj.FirstPass;
var
  ManuallyProcessed: boolean;
begin
  if FaRec = nil then
    FaRec := FirstPassCreateRec;
  aRec.Assign(dRec);

  // select version
  aRec.FirstPass;

  // OnFirstPassObject
  DoOnFirstPassObject(ManuallyProcessed);

  aRec.pRect := dRec.pRect;

  inherited;
end;

/////////////////////////////////////////////////
//
// TprTxHLineObjRecVersion
//
/////////////////////////////////////////////////
constructor TprTxHLineObjRecVersion.Create(Collection: TCollection);
begin
  inherited;
  FMainChar := '-';
  FStartingChar := '-';
  FEndingChar := '-';
end;

function TprTxHLineObjRecVersion.BuildMemoObjRecVersion(AWidth, AHeight: Integer; AStarting, AMain, AEnding: Char): TprTxMemoObjRecVersion;
begin
  Result := inherited BuildMemoObjRecVersion(AWidth, AHeight, AStarting, AMain, AEnding);
  if AWidth = 1 then
    Result.Memo.Add(AStarting)
  else
    Result.Memo.Add(AStarting + MakeStr(AMain, AWidth - 2) + AEnding)
end;

/////////////////////////////////////////////////
//
// TprTxHLineObjRec
//
/////////////////////////////////////////////////
function TprTxHLineObjRec.GetCanSplit(AByHorizontal: Boolean; ASplitPos: Integer): Boolean;
begin
  Result := AByHorizontal;
end;

function TprTxHLineObjRec.Split(AByHorizontal: Boolean; ASplitPos: Integer; var AAddToSplitted: Integer): TprObjRec;
var
  I: Integer;
begin
  Result := CreateCopy;
  AAddToSplitted := 0;

  with TprObjRecAccess(Result).FpRect do
  begin
    Right := ASplitPos;
    for I := 0 to Result.Versions.Count - 1 do
      with TprTxHLineObjRecVersion(Result.Versions[I]) do
        EndingChar := MainChar;
  end;

  with FpRect do
  begin
    for I := 0 to Versions.Count - 1 do
      with TprTxHLineObjRecVersion(Versions[I]) do
        StartingChar := MainChar;

    I := Right - Left - ASplitPos;
    Left := 0;
    Right := Left + I;
  end;
end;

function TprTxHLineObjRec.GetVersionClass: TprObjVersionClass;
begin
  Result := TprTxHLineObjRecVersion;
end;

/////////////////////////////////////////////////
//
// TprTxHLineObj
//
/////////////////////////////////////////////////
function TprTxHLineObj.GetVersion(Index: Integer): TprTxHLineObjRecVersion;
begin
  Result := TprTxHLineObjRecVersion(inherited Versions[Index]);
end;

function TprTxHLineObj.GetGenVersion(Index: Integer): TprTxHLineObjRecVersion;
begin
  Result := TprTxHLineObjRecVersion(inherited GenVersions[Index]);
end;

function TprTxHLineObj.GetDefVersion: TprTxHLineObjRecVersion;
begin
  Result := TprTxHLineObjRecVersion(inherited DefVersion);
end;

function TprTxHLineObj.GetGenCurVersion: TprTxHLineObjRecVersion;
begin
  Result := TprTxHLineObjRecVersion(inherited GenCurversion);
end;

procedure TprTxHLineObj.InitdRec;
begin
  FdRec := TprTxHLineObjRec.Create(nil,Self);
  dRec.Versions.Add;
end;

procedure TprTxHLineObj.DrawDesign(DC: HDC; ExData: pointer; const DrawRect: TRect);
var
  s: string;
  v: TprTxLineObjRecVersion;
  ARec: TprTxLineObjRec;
  nfn,ofn: HFONT;
begin
  v := TprTxHLineObjRecVersion(DefVersion);
  ARec := TprTxHLineObjRec(dRec);

  pr_Common.DrawRect(DC, DrawRect, clGray, clWhite);

  if ARec.Right - ARec.Left > 0 then
  begin
    // select font
    nfn := CreateTxFont(GetDeviceCaps(DC, LOGPIXELSY), PTxExData(ExData));
    ofn := SelectObject(DC, nfn);
    try
      SetBkMode(DC, TRANSPARENT);
      if ARec.Right - ARec.Left = 1 then
        S := v.StartingChar
      else
        if ARec.Right - ARec.Left > 1 then
          S := v.StartingChar + MakeStr(v.MainChar, ARec.Right - ARec.Left - 2) + v.EndingChar
        else
          S := '';

      ExtTextOut(DC,
                 DrawRect.Left,
                 DrawRect.Top,
                 ETO_CLIPPED,
                 @DrawRect,
                 PChar(s),
                 Length(s),
                 nil);

      SetBkMode(DC, OPAQUE);
    finally
      SelectObject(DC, ofn);
      DeleteObject(nfn);
    end;
  end;
  inherited;
end;

function TprTxHLineObj.GetDesc : string;
begin
  with TprTxHLineObjRecVersion(DefVersion) do
    Result := Format(prLoadStr(sHLineObjDesc), [StartingChar, MainChar, EndingChar, dRec.Right - dRec.Left]);
end;

function TprTxHLineObj.FirstPassCreateRec: TprTxLineObjRec;
begin
  Result := TprTxHLineObjRec.Create(nil, Self);
end;

/////////////////////////////////////////////////
//
// TprTxVLineObjRecVersion
//
/////////////////////////////////////////////////
constructor TprTxVLineObjRecVersion.Create(Collection: TCollection);
begin
  inherited;
  FMainChar := '|';
  FStartingChar := '|';
  FEndingChar := '|';
end;

function TprTxVLineObjRecVersion.BuildMemoObjRecVersion(AWidth, AHeight: Integer; AStarting, AMain, AEnding: Char): TprTxMemoObjRecVersion;
var
  I: Integer;
begin
  Result := inherited BuildMemoObjRecVersion(AWidth, AHeight, AStarting, AMain, AEnding);
  for I := 0 to AHeight - 1 do
  begin
    if I = 0 then
      Result.Memo.Add(AStarting)
    else
      if I = AHeight - 1 then
        Result.Memo.Add(AEnding)
      else
        Result.Memo.Add(AMain);
  end;
end;

/////////////////////////////////////////////////
//
// TprTxVLineObjRec
//
/////////////////////////////////////////////////
function TprTxVLineObjRec.GetCanSplit(AByHorizontal: Boolean; ASplitPos: Integer): Boolean;
begin
  Result := not AByHorizontal;
end;

function TprTxVLineObjRec.Split(AByHorizontal: Boolean; ASplitPos: Integer; var AAddToSplitted: Integer): TprObjRec;
var
  I: Integer;
begin
  Result := CreateCopy;
  AAddToSplitted := 0;

  with TprObjRecAccess(Result).FpRect do
  begin
    Bottom := ASplitPos;
    for I := 0 to Result.Versions.Count - 1 do
      with TprTxVLineObjRecVersion(Result.Versions[I]) do
        EndingChar := MainChar;
  end;

  with FpRect do
  begin
    for I := 0 to Versions.Count - 1 do
      with TprTxVLineObjRecVersion(Versions[I]) do
        StartingChar := MainChar;

    I := Bottom - Top - ASplitPos;
    Top := 0;
    Bottom := Top + I;
  end;
end;

function TprTxVLineObjRec.GetVersionClass: TprObjVersionClass;
begin
  Result := TprTxVLineObjRecVersion;
end;

/////////////////////////////////////////////////
//
// TprTxVLineObj
//
/////////////////////////////////////////////////
function TprTxVLineObj.GetVersion(Index: Integer): TprTxVLineObjRecVersion;
begin
  Result := TprTxVLineObjRecVersion(inherited Versions[Index]);
end;

function TprTxVLineObj.GetGenVersion(Index: Integer): TprTxVLineObjRecVersion;
begin
  Result := TprTxVLineObjRecVersion(inherited GenVersions[Index]);
end;

function TprTxVLineObj.GetDefVersion: TprTxVLineObjRecVersion;
begin
  Result := TprTxVLineObjRecVersion(inherited DefVersion);
end;

function TprTxVLineObj.GetGenCurVersion: TprTxVLineObjRecVersion;
begin
  Result := TprTxVLineObjRecVersion(inherited GenCurversion);
end;

procedure TprTxVLineObj.InitdRec;
begin
  FdRec := TprTxVLineObjRec.Create(nil,Self);
  dRec.Versions.Add;
end;

procedure TprTxVLineObj.DrawDesign(DC: HDC; ExData: pointer; const DrawRect: TRect);
var
  I: Integer;
  S: string;
  V: TprTxLineObjRecVersion;
  ARec: TprTxLineObjRec;
  nfn,ofn: HFONT;
begin
  v := TprTxVLineObjRecVersion(DefVersion);
  ARec := TprTxVLineObjRec(dRec);

  pr_Common.DrawRect(DC, DrawRect, clGray, clWhite);

  if ARec.Right - ARec.Left > 0 then
  begin
    // select font
    nfn := CreateTxFont(GetDeviceCaps(DC, LOGPIXELSY), PTxExData(ExData));
    ofn := SelectObject(DC, nfn);
    try
      SetBkMode(DC, TRANSPARENT);
      for I := 0 to ARec.Bottom - ARec.Top - 1 do
      begin
        if I = 0 then
          S := v.StartingChar
        else
          if I = ARec.Bottom - ARec.Top - 1 then
            S := v.EndingChar
          else
            S := v.MainChar;
        ExtTextOut(DC,
                   DrawRect.Left,
                   DrawRect.Top + PTxExData(ExData).SymbolSize.cy * I,
                   ETO_CLIPPED,
                   @DrawRect,
                   PChar(S),
                   Length(S),
                   nil);
      end;

      SetBkMode(DC, OPAQUE);
    finally
      SelectObject(DC, ofn);
      DeleteObject(nfn);
    end;
  end;
  inherited;
end;

function TprTxVLineObj.GetDesc : string;
begin
  with TprTxVLineObjRecVersion(DefVersion) do
    Result := Format(prLoadStr(sVLineObjDesc), [StartingChar, MainChar, EndingChar, dRec.Right - dRec.Left]);
end;

function TprTxVLineObj.FirstPassCreateRec: TprTxLineObjRec;
begin
  Result := TprTxVLineObjRec.Create(nil, Self);
end;

initialization

  Classes.RegisterClass(TprTxHLineObj);
  Classes.RegisterClass(TprTxVLineObj);
  // register objects                 
  prRegisterObj(TprTxHLineObj,
                TprTxHLineObjRec,
                TprTxReport,
                sTxHLineObjCaption,
                sTxHLineObjHint,
                'TprTxLineEditorForm',
                '');

  prRegisterObj(TprTxVLineObj,
                TprTxVLineObjRec,
                TprTxReport,
                sTxVLineObjCaption,
                sTxVLineObjHint,
                'TprTxLineEditorForm',
                '');

end.
