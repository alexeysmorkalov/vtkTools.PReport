{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

unit pr_OemCharSelectForm;

interface

{$I vtk.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons,

  vgr_GUIFunctions, vgr_Button,

  pr_Common, pr_TxClasses, pr_TxUtils, pr_MultiLang, pr_TxConsts;

type
  TprOemCharSelectForm = class(TvgrDropDownForm)
    bOk: TButton;
    bCancel: TButton;
    Label1: TLabel;
    edCodeHex: TEdit;
    Label2: TLabel;
    edCodeDec: TEdit;
    Panel1: TPanel;
    prMLRes1: TprMLRes;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure edCodeDecChange(Sender: TObject);
    procedure edCodeHexChange(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
    FSG: TprTxCharGrid;
    FOnSelect: TNotifyEvent;
    procedure SGDblClick(Sender: TObject);
    procedure SGClick(Sender: TObject);
    function GetSelectedChar: Char;
    procedure SetSelectedChar(Value: Char);
  public
    { Public declarations }
    property SelectedChar: Char read GetSelectedChar write SetSelectedChar;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  /////////////////////////////////////////////////
  //
  // TprOemCharSelectButton
  //
  /////////////////////////////////////////////////
  TprOemCharSelectButton = class(TvgrBaseButton)
  private
    FSelectedChar: Char;
    FIsCharSelected: Boolean;
    FBorderMode: Boolean;
    FOnSelect: TNotifyEvent;
    procedure SetSelectedChar(Value: Char);
    procedure SetBorderMode(Value: Boolean);
    procedure SetIsCharSelected(Value: Boolean);
    function GetDropDownForm: TprOemCharSelectForm;
  protected
    function GetDropDownFormClass: TvgrDropDownFormClass; override;
    procedure ShowDropDownForm; override;
    procedure DrawButtonInternal(ADC: HDC; ADisabled, APushed, AFocused, AUnderMouse: Boolean; const AInternalRect: TRect); override;
    procedure OnPopupFormSelect(Sender: TObject);

    property DropDownForm: TprOemCharSelectForm read GetDropDownForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Click; override;

    property BorderMode: Boolean read FBorderMode write SetBorderMode;
    property IsCharSelected: Boolean read FIsCharSelected write SetIsCharSelected;
    property SelectedChar: Char read FSelectedChar write SetSelectedChar;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

implementation

{$R *.dfm}

function PopupOemCharForm(const AButtonRect: TRect; AChar: Char; AOnSelect: TNotifyEvent; AOwner: TComponent; ATag: Integer): TprOemCharSelectForm;
var
  APoint: TPoint;
begin
  Result := TprOemCharSelectForm.Create(AOwner);
  Result.BorderStyle := bsSingle;
  Result.SelectedChar := AChar;
  Result.OnSelect := AOnSelect;
  Result.Tag := ATag;

  APoint := FindPopupPoint(AButtonRect, Result.Width, Result.Height);
  Result.SetBounds(APoint.X, APoint.Y, Result.Width, Result.Height);
  Result.Show;
end;

/////////////////////////////////////////////////
//
// TprOemCharSelectForm
//
/////////////////////////////////////////////////
procedure TprOemCharSelectForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TprOemCharSelectForm.FormCreate(Sender: TObject);
begin
  FSG := TprTxCharGrid.Create(Self);
  with FSG do
  begin
    Align := alClient;
    Parent := Panel1;
    OnDblClick := SGDblClick;
    OnClick := SGClick;
  end;
  ActiveControl := FSG;
end;

procedure TprOemCharSelectForm.SGDblClick(Sender: TObject);
begin
  bOkClick(nil);
end;

procedure TprOemCharSelectForm.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(ClientRect);
end;

function TprOemCharSelectForm.GetSelectedChar: Char;
begin
  Result := FSG.SelectedChar;
end;

procedure TprOemCharSelectForm.SetSelectedChar(Value: Char);
begin
  FSG.SelectedChar := Value;
end;

procedure TprOemCharSelectForm.SGClick(Sender: TObject);
begin
  with edCodeHex do
  begin
    OnChange := nil;
    Text := IntToHex(Byte(FSG.SelectedChar), 2);
    OnChange := edCodeHexChange;
  end;

  with edCodeDec do
  begin
    OnChange := nil;
    Text := IntToStr(Byte(FSG.SelectedChar));
    OnChange := edCodeDecChange;
  end;
end;

procedure TprOemCharSelectForm.bOkClick(Sender: TObject);
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
  Close;
end;

procedure TprOemCharSelectForm.bCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TprOemCharSelectForm.edCodeDecChange(Sender: TObject);
var
  ACode: Integer;
begin
  FSG.OnClick := nil;
  edCodeHex.OnChange := nil;
  ACode := StrToIntDef(edCodeDec.Text, -1);
  if (ACode < 0) or (ACode > 255) then
    FSG.SelectedChar := #0;

  FSG.SelectedChar := Char(ACode);
  edCodeHex.Text := IntToHex(ACode, 2);
  FSG.OnClick := SGClick;
  edCodeHex.OnChange := edCodeDecChange;
end;

procedure TprOemCharSelectForm.edCodeHexChange(Sender: TObject);
var
  ACode, ADig1, ADig2: Byte;

  function Convert(AChar: Char; var AValue: Byte): Boolean;
  begin
    Result := True;
    if AChar in ['0'..'9'] then
      AValue := Byte(AChar) - Byte('0')
    else
      if AChar in  ['A'..'F'] then
        AValue := Byte(AChar) - Byte('A') + 10
      else
        Result := False;
  end;
  
begin
  FSG.OnClick := nil;
  edCodeDec.OnChange := nil;

  ACode := 0;
  case Length(edCodeHex.Text) of
    1: Convert(edCodeHex.Text[1], ACode);
    2: begin
         if Convert(edCodeHex.Text[1], ADig1) then
           if Convert(edCodeHex.Text[2], ADig2) then
             ACode := (ADig1 shl 4) or ADig2;
       end;
  end;

  FSG.SelectedChar := Char(ACode);
  edCodeDec.Text := IntToStr(ACode);
  FSG.OnClick := SGClick;
  edCodeDec.OnChange := edCodeDecChange;
end;

/////////////////////////////////////////////////
//
// TprOemCharSelectForm
//
/////////////////////////////////////////////////
constructor TprOemCharSelectButton.Create(AOwner: TComponent);
begin
  inherited;
  DropDownArrow := True;
  FSelectedChar := #0;
  Width := 40;
  Height := 28;
end;

destructor TprOemCharSelectButton.Destroy;
begin
  inherited;
end;

procedure TprOemCharSelectButton.Click;
begin
  ShowDropDownForm;
  inherited;
end;

function TprOemCharSelectButton.GetDropDownFormClass: TvgrDropDownFormClass;
begin
  Result := TprOemCharSelectForm;
end;

procedure TprOemCharSelectButton.ShowDropDownForm;
begin
  inherited;

  DropDownForm.SelectedChar := SelectedChar;
  DropDownForm.OnSelect := OnPopupFormSelect;
end;

procedure TprOemCharSelectButton.DrawButtonInternal(ADC: HDC; ADisabled, APushed, AFocused, AUnderMouse: Boolean; const AInternalRect: TRect);
var
  r: TRect;
  ASize: TSize;
  ANewFont, AOldFont: HFONT;
begin
  r := AInternalRect;
  if APushed then
  begin
    r.Top := r.Top + 1;
    r.Left := r.Left + 1;
  end;

  if IsCharSelected then
  begin
    if FBorderMode and (FSelectedChar = #0) then
    begin
    end
    else
    begin
      ANewFont := CreateTxDraftFont(sOemFontName, 12);
      AOldFont := SelectObject(ADC, ANewFont);

      GetTextExtentPoint(ADC, @FSelectedChar, 1, ASize);
      TextOut(ADC, r.Left + 1, r.Top + (r.Bottom - r.Top - ASize.cy) div 2, @FSelectedChar, 1);

      SelectObject(ADC, AOldFont);
      DeleteObject(ANewFont);
    end;
  end
  else
  begin
  end;
end;

procedure TprOemCharSelectButton.SetSelectedChar(Value: Char);
begin
  if (FSelectedChar <> Value) or not FIsCharSelected then
  begin
    FSelectedChar := Value;
    FIsCharSelected := True;
    Invalidate;
  end;
end;

procedure TprOemCharSelectButton.SetBorderMode(Value: Boolean);
begin
  if FBorderMode <> Value then
  begin
    FBorderMode := Value;
    Invalidate;
  end;
end;

procedure TprOemCharSelectButton.SetIsCharSelected(Value: Boolean);
begin
  if FIsCharSelected <> Value then
  begin
    FIsCharSelected := Value;
    Invalidate;
  end;
end;

function TprOemCharSelectButton.GetDropDownForm: TprOemCharSelectForm;
begin
  Result := TprOemCharSelectForm(inherited DropDownForm);
end;

procedure TprOemCharSelectButton.OnPopupFormSelect(Sender: TObject);
begin
  SelectedChar := DropDownForm.SelectedChar;
end;

end.
