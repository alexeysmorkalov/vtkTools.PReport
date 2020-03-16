{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the TprBarCodeObj class that is intended for inserting barcodes in your report.}
unit pr_BarCodeObj;

interface

{$I pr.inc}

uses
  Windows, Classes, SysUtils, Graphics, Menus, Math, typinfo,

  pr_Common, pr_Classes, vgr_Functions, vgr_GUIFunctions, vgr_Barcode;

const
  cDefaultBarCodeFontName = 'Courier New';
  cDefaultBarCodeFontSize = 8;
  cDefaultBarCodeFontStyle = [];
  cDefaultBarCodeFontCharset = DEFAULT_CHARSET;

type

{Describes the supported rotations of barcode.
Items:
  prbrNone - No rotation (0 degrees).
  prbr90 - 90 degrees.
  prbr180 - 180 degrees.
  prbr270 - 270 degrees.}
  TprBarcodeRotation = (prbrNone, prbr90, prbr180, prbr270);
  /////////////////////////////////////////////////
  //
  // TprBarCodeObjRecVersion
  //
  /////////////////////////////////////////////////
{Represents the object view for TprBarCodeObj.
See also:
  TprBarCodeObj}
  TprBarCodeObjRecVersion = class(TprExObjRecVersion)
  private
    FBarCode: TvgrBarcode;
    FAutoSize: Boolean;
    SecondPassNeeded: Boolean;

    function GetText: string;
    procedure SetText(Value: string);
    function GetBarCodeType: TvgrBarCodeType;
    procedure SetBarCodeType(Value: TvgrBarCodeType);
    function GetZoomFactor: Double;
    procedure SetZoomFactor(Value: Double);
    function IsZoomFactorStored: Boolean;
    function GetTextType: TvgrBarCodeOption;
    procedure SetTextType(Value: TvgrBarCodeOption);
    function GetTextFont: TFont;
    procedure SetTextFont(Value: TFont);
    function IsTextFontStored: Boolean;
    function GetTextPosition: TvgrShowTextPosition;
    procedure SetTextPosition(Value: TvgrShowTextPosition);
    function GetRotation: TprBarcodeRotation;
    procedure SetRotation(Value: TprBarcodeRotation);
    function GetBackColor: TColor;
    procedure SetBackColor(Value: TColor);
    function GetForeColor: TColor;
    procedure SetForeColor(Value: TColor);
    function GetShowTextOverLines: Boolean;
    procedure SetShowTextOverLines(Value: Boolean);
  protected
    property Barcode: TvgrBarcode read FBarcode;

    procedure InternalDraw(DC: HDC; const ARect: TRect; pdi: PPrDrawInfo);
  public
{Creates an instance of the TprBarCodeObjRecVersion class.}
    constructor Create(Collection: TCollection); override;
{Frees an instance of the TprBarCodeObjRecVersion class.}
    destructor Destroy; override;

{See:
  TprObjRecVersion.Assign}
    procedure Assign(Source: TPersistent); override;

{See:
  TprExObjRecVersion.Draw}
    procedure Draw(DC: HDC; pdi: PPrDrawInfo); override;
{See:
  TprExObjRecVersion.InitInDesigner}
    procedure InitInDesigner; override;
  published
{Text which will be displayed as barcode.}
    property Text: string read GetText write SetText;
{Specifies the type of barcode.
See also:
  TvgrBarcodeType}
    property BarCodeType: TvgrBarCodeType read GetBarCodeType write SetBarCodeType default bcCode_2_5_interleaved;
{Specifies the zoom factor of barcode, default is 1.0 - no zooming.}
    property ZoomFactor: Double read GetZoomFactor write SetZoomFactor stored IsZoomFactorStored;
{Indicates whether the text should be displayed with barcode.
See also:
  TvgrBarcodeOption, TextFont, TextPosition}
    property ShowTextType: TvgrBarcodeOption read GetTextType write SetTextType default bcoNone;
{Specifies the font of displayed text, used only if TextType does not equal to bcoNone.}
    property TextFont: TFont read GetTextFont write SetTextFont stored IsTextFontStored;
{Specifies the position of displayed text.}
    property TextPosition: TvgrShowTextPosition read GetTextPosition write SetTextPosition default stpBottomCenter;
{Specifies the rotation angle of barcode.}
    property Rotation: TprBarcodeRotation read GetRotation write SetRotation default prbrNone;
{Specifies the background color.}
    property BackColor: TColor read GetBackColor write SetBackColor default clWhite;
{Specifies the foreground color, this color will be used for lines and text.}
    property ForeColor: TColor read GetForeColor write SetForeColor default clBlack;
{Indicates whether the text should be displaed over lines of barcode, used
only if ShowTextType does not equal bcoNone.}
    property ShowTextOverLines: Boolean read GetShowTextOverLines write SetShowTextOverLines default False;
{Indicates whether the size of object must be adjusted.}
    property AutoSize: Boolean read FAutoSize write FAutoSize default True;
  end;

  /////////////////////////////////////////////////
  //
  // TprBarCodeObjRec
  //
  /////////////////////////////////////////////////
{Represents a record of properties for the TprBarCodeObj object.
See also:
  TprBarCodeObj, TprBarCodeObjRecVersion}
  TprBarCodeObjRec = class(TprExObjRec)
  protected
    function GetVersionClass: TprObjVersionClass; override;
  public
{See:
  TprObjRec.SecondPass}
    procedure SecondPass; override;
  end;

  /////////////////////////////////////////////////
  //
  // TprBarCodeObj
  //
  /////////////////////////////////////////////////
{The TprShapeObj class is intended for inserting barcodes in your report.
As all other objects in PReport the TprBarCodeObj can have many views (it has one view after creating)
the necessary  object view is selected on the stage of report generating.
See also:
  TprBarCodeObjRecVersion}
  TprBarCodeObj = class(TprExObj)
  private
    function GetVersion(Index: Integer): TprBarCodeObjRecVersion;
    function GetGenVersion(Index: Integer): TprBarCodeObjRecVersion;
    function GetDefVersion: TprBarCodeObjRecVersion;
    function GetGenCurVersion: TprBarCodeObjRecVersion;
  protected
    procedure InitdRec; override;
    procedure OnDsgnPopupMenuClick(Sender : TObject);
  public
{See:
  TprObj.DrawDesign}
    procedure DrawDesign(DC: HDC; ExData: pointer; const DrawRect: TRect); override;
{See:
  TprObj.GetDesc}
    function GetDesc: string; override;
{See:
  TprObj.DsgnIsTransparent}
    function DsgnIsTransparent: Boolean; override;
{See:
  TprObj.DsgnDefinePopupMenu}
    procedure DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: Boolean); override;
{See:
  TprObj.FirstPass}
    procedure FirstPass; override;

{See:
  TprObj.Versions}
    property Versions[Index: Integer]: TprBarCodeObjRecVersion read GetVersion;
{See:
  TprObj.GenVersions}
    property GenVersions[Index: Integer]: TprBarCodeObjRecVersion read GetGenVersion;
{See:
  TprObj.DefVersion}
    property DefVersion: TprBarCodeObjRecVersion read GetDefVersion;
{See:
  TprObj.GenCurVersion}
    property GenCurVersion: TprBarCodeObjRecVersion read GetGenCurVersion;
  end;

implementation

uses
  pr_Strings {$IFDEF PR_D6_D7}, Types{$ENDIF};

/////////////////////////////////////////////////
//
// TprBarCodeObjRecVersion
//
/////////////////////////////////////////////////
constructor TprBarCodeObjRecVersion.Create(Collection: TCollection);
begin
  inherited;
  FAutoSize := True;
  FBarCode := TvgrBarcode.Create;

  BarCodeType := bcCode_2_5_interleaved;
  ZoomFactor := 1;
  ShowTextType := bcoNone;
  TextPosition := stpBottomCenter;
  Rotation := prbrNone;
  BackColor := clWhite;
  ForeColor := clBlack;
  with GetTextFont do
  begin
    Name := cDefaultBarCodeFontName;
    Size := cDefaultBarCodeFontSize;
    Style := cDefaultBarCodeFontStyle;
    Charset := cDefaultBarCodeFontCharset;
  end;
  ShowTextOverLines := False;
end;
 
destructor TprBarCodeObjRecVersion.Destroy;
begin
  FreeAndNil(FBarCode);
  inherited;
end;
 
function TprBarCodeObjRecVersion.GetText: string;
begin
  Result := Barcode.Text;
end;
 
procedure TprBarCodeObjRecVersion.SetText(Value: string);
begin
  Barcode.Text := Value;
end;
 
function TprBarCodeObjRecVersion.GetBarCodeType: TvgrBarCodeType;
begin
  Result := Barcode.Typ;
end;
 
procedure TprBarCodeObjRecVersion.SetBarCodeType(Value: TvgrBarCodeType);
begin
  Barcode.Typ := Value;
end;
 
function TprBarCodeObjRecVersion.GetZoomFactor: Double;
begin
  Result := Barcode.Ratio;
end;
 
procedure TprBarCodeObjRecVersion.SetZoomFactor(Value: Double);
begin
  Barcode.Ratio := Value;
end;
 
function TprBarCodeObjRecVersion.IsZoomFactorStored: Boolean;
begin
  Result := not IsZero(Barcode.Ratio - 1);
end;
 
function TprBarCodeObjRecVersion.GetTextType: TvgrBarCodeOption;
begin
  Result := Barcode.ShowText;
end;

procedure TprBarCodeObjRecVersion.SetTextType(Value: TvgrBarCodeOption);
begin
  Barcode.ShowText := Value;
end;
 
function TprBarCodeObjRecVersion.GetTextFont: TFont;
begin
  Result := Barcode.ShowTextFont;
end;
 
procedure TprBarCodeObjRecVersion.SetTextFont(Value: TFont);
begin
  Barcode.ShowTextFont.Assign(Value);
end;
 
function TprBarCodeObjRecVersion.IsTextFontStored: Boolean;
begin
  with Barcode.ShowTextFont do
    Result := (Name <> cDefaultBarCodeFontName) or
              (Size <> cDefaultBarCodeFontSize) or
              (Style <> cDefaultBarCodeFontStyle) or
              (Charset <> cDefaultBarCodeFontCharset);
end;
 
function TprBarCodeObjRecVersion.GetTextPosition: TvgrShowTextPosition;
begin
  Result := Barcode.ShowTextPosition;
end;
 
procedure TprBarCodeObjRecVersion.SetTextPosition(Value: TvgrShowTextPosition);
begin
  Barcode.ShowTextPosition := Value;
end;
 
function TprBarCodeObjRecVersion.GetRotation: TprBarcodeRotation;
begin
  if IsZero(Barcode.Angle) then
    Result := prbrNone
  else
    if IsZero(Barcode.Angle - 90) then
      Result := prbr90
    else
      if IsZero(Barcode.Angle - 180) then
        Result := prbr180
      else
        if IsZero(Barcode.Angle - 270) then
          Result := prbr270
        else
          Result := prbrNone;
end;

procedure TprBarCodeObjRecVersion.SetRotation(Value: TprBarcodeRotation);
const
  ARotation: array [TprBarcodeRotation] of double = (0, 90, 180, 270);
begin
  Barcode.Angle := ARotation[Value];
end;

function TprBarCodeObjRecVersion.GetBackColor: TColor;
begin
  Result := Barcode.Color;
end;
 
procedure TprBarCodeObjRecVersion.SetBackColor(Value: TColor);
begin
  Barcode.Color := Value;
end;
 
function TprBarCodeObjRecVersion.GetForeColor: TColor;
begin
  Result := Barcode.ColorBar;
end;
 
procedure TprBarCodeObjRecVersion.SetForeColor(Value: TColor);
begin
  Barcode.ColorBar := Value;
end;

function TprBarCodeObjRecVersion.GetShowTextOverLines: Boolean;
begin
  Result := Barcode.ShowTextOverLines;
end;

procedure TprBarCodeObjRecVersion.SetShowTextOverLines(Value: Boolean);
begin
  Barcode.ShowTextOverLines := Value;
end;

procedure TprBarCodeObjRecVersion.Assign(Source: TPersistent);
begin
  if Source is TprBarCodeObjRecVersion then
    with TprBarCodeObjRecVersion(Source) do
    begin
      Self.FAutoSize := FAutoSize;
      Self.Barcode.Assign(Barcode);
      Self.SecondPassNeeded := SecondPassNeeded;
    end;
  inherited;
end;

procedure TprBarCodeObjRecVersion.InternalDraw(DC: HDC; const ARect: TRect; pdi: PPrDrawInfo);
const
  ADrawMode: array [TprImageDrawMode] of TvgrImageDrawMode = (vgridmCenter, vgridmStretch, vgridmStretchProp, vgridmStretch);
var
  ACanvas: TCanvas;
  ABrush: HBRUSH;
  AMetafile: TMetafile;
  AMetafileCanvas: TMetafileCanvas;
  S: string;
begin
  ACanvas := TCanvas.Create;
  ACanvas.Handle := DC;

  try
    S := Barcode.Text;
    if (Barcode.Text = '') or not Barcode.CheckBarcode then
      Barcode.Text := '12345678'; // this code will be displayed always without errors

    if IsZero(pdi.kx - 1) and IsZero(pdi.ky - 1) then
      // do not use a metafile
      Barcode.DrawBarcode(ACanvas, ARect)
    else
    begin
      if (BackColor <> clNone) and ((BackColor <> clWhite) or (not pdi.IsPrinter)) then
      begin
        ABrush := CreateSolidBrush(GetRGBColor(BackColor));
        FillRect(ACanvas.Handle, ARect, ABrush);
        DeleteObject(ABrush);
      end;

      // we must use a metafile
      // build metafile
      AMetafile := TMetaFile.Create;
      try
        AMetafile.Width := GeneratedRect.Right - GeneratedRect.Left;
        AMetafile.Height := GeneratedRect.Bottom - GeneratedRect.Top;
        AMetafileCanvas := TMetafileCanvas.Create(AMetafile, 0);

        with GeneratedRect do
          BarCode.DrawBarcode(AMetafileCanvas, Rect(0, 0, Right - Left, Bottom - Top));

        AMetafileCanvas.Free;

        DrawImage(ACanvas, AMetafile, ARect, 1, 1, vgridmStretchProp);
      finally
        AMetafile.Free;
      end;
    end;

    Barcode.Text := S;
  finally
    ACanvas.Free;
  end;
end;

procedure TprBarCodeObjRecVersion.Draw(DC: HDC; pdi: PPrDrawInfo);
const
  ADrawMode: array [TprImageDrawMode] of TvgrImageDrawMode = (vgridmCenter, vgridmStretch, vgridmStretchProp, vgridmStretch);
begin
  InternalDraw(DC, GetRealRect(pdi), pdi);
end;

procedure TprBarCodeObjRecVersion.InitInDesigner;
begin
end;

/////////////////////////////////////////////////
//
// TprBarCodeObjRec
//
/////////////////////////////////////////////////
function TprBarCodeObjRec.GetVersionClass: TprObjVersionClass;
begin
  Result := TprBarCodeObjRecVersion;
end;

procedure TprBarCodeObjRec.SecondPass;
var
  V: TprBarCodeObjRecVersion;
  I: Integer;
  S: string;
begin
  inherited; // calculate version of object

  for I := 0 to Versions.Count - 1 do
    if TprBarCodeObjRecVersion(Versions[I]).SecondPassNeeded then
    begin
      V := TprBarCodeObjRecVersion(Versions[I]);
      Container.FormatTemplate(V.Text, S);
      V.Text := S;
    end;
end;

/////////////////////////////////////////////////
//
// TprBarCodeObj
//
/////////////////////////////////////////////////
function TprBarCodeObj.GetVersion(Index: Integer): TprBarCodeObjRecVersion;
begin
  Result := TprBarCodeObjRecVersion(inherited Versions[Index]);
end;

function TprBarCodeObj.GetGenVersion(Index: Integer): TprBarCodeObjRecVersion;
begin
  Result := TprBarCodeObjRecVersion(inherited GenVersions[Index]);
end;

function TprBarCodeObj.GetDefVersion: TprBarCodeObjRecVersion;
begin
  Result := TprBarCodeObjRecVersion(inherited DefVersion);
end;

function TprBarCodeObj.GetGenCurVersion: TprBarCodeObjRecVersion;
begin
  Result := TprBarCodeObjRecVersion(inherited GenCurversion);
end;

procedure TprBarCodeObj.InitdRec;
begin
  FdRec := TprBarCodeObjRec.Create(nil, Self);
  TprExObjRecVersion(dRec.Versions.Add).InitInDesigner;
end;

procedure TprBarCodeObj.OnDsgnPopupMenuClick(Sender : TObject);
begin
end;

procedure TprBarCodeObj.DrawDesign(DC: HDC; ExData: pointer; const DrawRect: TRect);
var
  rdi: rprDrawInfo;
begin
  with rdi do
  begin
    kx := 1;
    ky := 1;
    prnkx := 1;
    prnky := 1;
    IsPrinter := False;
    Report := nil;
    ppdi := nil;
  end;

  DefVersion.InternalDraw(DC, DrawRect, @rdi);
end;

function TprBarCodeObj.GetDesc: string;
begin
  with DefVersion do
    Result := Format('%s (%s)', [Text, GetEnumName(TypeInfo(TvgrBarcodeType), Integer(BarCodeType))]);
end;

function TprBarCodeObj.DsgnIsTransparent: Boolean;
begin
  Result := DefVersion.BackColor = clNone;
end;

procedure TprBarCodeObj.DsgnDefinePopupMenu(Popup: TPopupMenu; OneObjectSelected: Boolean);
begin
  inherited;
end;

procedure TprBarCodeObj.FirstPass;
var
  ManuallyProcessed: boolean;
  S: string;
  I: Integer;
begin
  if FaRec = nil then
    FaRec := TprBarCodeObjRec.Create(nil, Self);
  aRec.Assign(dRec);

  // select version
  aRec.FirstPass;

  // OnFirstPassObject
  DoOnFirstPassObject(ManuallyProcessed);

  if not ManuallyProcessed then
    for I := 0 to aRec.Versions.Count - 1 do
      with TprBarCodeObjRecVersion(aRec.Versions[I]) do
      begin
        SecondPassNeeded := not Band.Report.FormatTemplate(Text, S);
        Text := S;
      end;

  aRec.pRect := dRec.pRect;

  with GenCurVersion do
    if AutoSize then
    begin
      if Rotation in [prbrNone, prbr180] then
      begin
        TprBarCodeObjRec(aRec).FpRect.Bottom := TprBarCodeObjRec(aRec).FpRect.Top + dRec.Height;
        TprBarCodeObjRec(aRec).FpRect.Right := TprBarCodeObjRec(aRec).FpRect.Left + Barcode.GetLinesWidth;
      end
      else
      begin
        TprBarCodeObjRec(aRec).FpRect.Right := TprBarCodeObjRec(aRec).FpRect.Left + dRec.Width;
        TprBarCodeObjRec(aRec).FpRect.Bottom := TprBarCodeObjRec(aRec).FpRect.Top + Barcode.GetLinesWidth;
      end;
    end;

  inherited;
end;

initialization

  Classes.RegisterClass(TprBarCodeObj);
  Classes.RegisterClass(TprBarCodeObjRecVersion);
  // register objects
  prRegisterObj(TprBarCodeObj,
                TprBarCodeObjRec,
                TprReport,
                sBarCodeObjCaption,
                sBarCodeObjHint,
                'TprBarCodeEditorForm',
                'TprPrvBarCodeEditorForm');

end.

