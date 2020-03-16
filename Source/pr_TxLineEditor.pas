{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_TxLineEditor;

interface

{$I vtk.inc}

uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls, ComCtrls, Buttons, IniFiles,

  vgr_Functions,
  
  pr_Common, pr_TxClasses, pr_CommonDesignerPanel, pr_TxUtils, pr_MultiLang,
  pr_Strings, pr_TxAddon;

const
  sCharCombinationCount = 'CharCombinationCount';
  sCharCombination = 'CharCombination';

type
  /////////////////////////////////////////////////
  //
  // TprLineCombination
  //
  /////////////////////////////////////////////////
  TprLineCombination = class(TObject)
  private
    FStartingChar: Char;
    FMainChar: Char;
    FEndingChar: Char;
  public
    constructor Create(AStartingChar, AMainChar, AEndingChar: Char);
    property StartingChar: Char read FStartingChar write FStartingChar;
    property MainChar: Char read FMainChar write FMainChar;
    property EndingChar: Char read FEndingChar write FEndingChar;
  end;

  /////////////////////////////////////////////////
  //
  // TprLineCombinations
  //
  /////////////////////////////////////////////////
  TprLineCombinations = class(TList)
  private
    function GetItem(Index: Integer): TprLineCombination;
  public
    destructor Destroy; override;

    procedure Clear; override;

    procedure LoadFromIni(AIniFile: TIniFile; const ASection: string);
    procedure SaveToIni(AIniFile: TIniFile; const ASection: string);

    function IndexByChars(AStarting, AMain, AEnding: Char): Integer;

    property Items[Index: Integer]: TprLineCombination read GetItem; default;
  end;

  /////////////////////////////////////////////////
  //
  // TprTxLineEditorForm
  //
  /////////////////////////////////////////////////
  TprTxLineEditorForm = class(TprObjPropsForm)
    PC: TPageControl;
    PMainChar: TTabSheet;
    PView: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    EDTxFontStyle: TComboBox;
    LBTxFontOptions: TCheckListBox;
    CBDefaultFont: TCheckBox;
    prMLRes1: TprMLRes;
    Panel2: TPanel;
    Label3: TLabel;
    edMainCharHexCode: TEdit;
    pPreviewMainChar: TPanel;
    PStartingChar: TTabSheet;
    PEndingChar: TTabSheet;
    Panel1: TPanel;
    Label4: TLabel;
    edStartingCharHexCode: TEdit;
    pPreviewStartingChar: TPanel;
    Panel4: TPanel;
    Label5: TLabel;
    edEndingCharHexCode: TEdit;
    pPreviewEndingChar: TPanel;
    bAllAsMainChar: TSpeedButton;
    PTemplates: TTabSheet;
    Label6: TLabel;
    edCurrent: TEdit;
    lbSaved: TListBox;
    Label7: TLabel;
    bSave: TButton;
    bLoad: TButton;
    procedure FormCreate(Sender: TObject);
    procedure edMainCharHexCodeKeyPress(Sender: TObject; var Key: Char);
    procedure edMainCharHexCodeChange(Sender: TObject);
    procedure bAllAsMainCharClick(Sender: TObject);
    procedure edStartingCharHexCodeChange(Sender: TObject);
    procedure edEndingCharHexCodeChange(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure bLoadClick(Sender: TObject);
    procedure lbSavedClick(Sender: TObject);
  private
    { Private declarations }
    FCombinations: TprLineCombinations;
    SGMain: TprTxCharGrid;
    SGStarting: TprTxCharGrid;
    SGEnding: TprTxCharGrid;
    procedure UpdateLineChar(c: char; APanel: TPanel; AEdit: TEdit; AGrid: TprTxCharGrid);
    procedure UpdateCurrent;
    procedure UpdateEnabled;
    procedure UpdateSaved;
    procedure OnGridClick(AEdit: TEdit; APanel: TPanel; AGrid: TprTxCharGrid);
    procedure OnEditChange(AEdit: TEdit; APanel: TPanel; AGrid: TprTxCharGrid);
    procedure SGMainOnClick(Sender: TObject);
    procedure SGStartingOnClick(Sender: TObject);
    procedure SGEndingOnClick(Sender: TObject);
    function GetCombinationString(AStarting, AMain, AEnding: Integer): string;
    function GetStarting: Char;
    function GetMain: Char;
    function GetEnding: Char;
  protected
    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;
  public
    { Public declarations }
    procedure SetEnabledAfterCopyToControls; override;
    procedure CopySinglePropertiesFromControls(V: TprObjRecVersion); override;
    procedure CopyMultiplyPropertiesFromControls(L: TList); override;
    procedure CopySinglePropertiesToControls(V: TprObjRecVersion); override;
    procedure CopyMultiplyPropertiesToControls(L: TList); override;

    property Starting: Char read GetStarting;
    property Main: Char read GetMain;
    property Ending: Char read GetEnding;
  end;

implementation

uses pr_DesignerFunctions, pr_TxConsts,
  pr_TxMemoLinesEditorForm, pr_TxDesigner;
  
{$R *.dfm}

/////////////////////////////////////////////////
//
// TprLineCombination
//
/////////////////////////////////////////////////
constructor TprLineCombination.Create(AStartingChar, AMainChar, AEndingChar: Char);
begin
  inherited Create;
  FStartingChar := AStartingChar;
  FMainChar := AMainChar;
  FEndingChar := AEndingChar;
end;

/////////////////////////////////////////////////
//
// TprLineCombinations
//
/////////////////////////////////////////////////
destructor TprLineCombinations.Destroy;
begin
  Clear;
  inherited;
end;

procedure TprLineCombinations.Clear;
begin
  FreeListItems(Self);
  inherited;
end;

function TprLineCombinations.GetItem(Index: Integer): TprLineCombination;
begin
  Result := TprLineCombination(inherited Items[Index]);
end;

procedure TprLineCombinations.LoadFromIni(AIniFile: TIniFile; const ASection: string);
var
  I, ACount: Integer;
  S: string;
  AStarting, AMain, AEnding: Integer;
begin
  Clear;
  ACount := AIniFile.ReadInteger(ASection, sCharCombinationCount, 0);
  for I := 0 to ACount - 1 do
  begin
    S := AIniFile.ReadString(ASection, sCharCombination + IntToStr(I), '');
    if Length(S) = 6 then
    begin
      S := AnsiLowerCase(S);
      AStarting := HexToIntDef(Copy(S, 1, 2));
      AMain := HexToIntDef(Copy(S, 3, 2));
      AEnding := HexToIntDef(Copy(S, 5, 2));
      Add(TprLineCombination.Create(Char(AStarting), Char(AMain), Char(AEnding)));
    end;
  end;
end;

procedure TprLineCombinations.SaveToIni(AIniFile: TIniFile; const ASection: string);
var
  I: Integer;
begin
  AIniFile.WriteInteger(ASection, sCharCombinationCount, Count);
  for I := 0 to Count - 1 do
    with Items[I] do
      AIniFile.WriteString(ASection, sCharCombination + IntToStr(I),
                           Format('%x%x%x', [Integer(StartingChar),
                                             Integer(MainChar),
                                             Integer(EndingChar)]));
end;

function TprLineCombinations.IndexByChars(AStarting, AMain, AEnding: Char): Integer;
begin
  Result := 0;
  while (Result < Count) and
        not ((Items[Result].StartingChar = AStarting) and
             (Items[Result].MainChar = AMain) and
             (Items[Result].EndingChar = AEnding)) do Inc(Result);
  if Result >= Count then
    Result := -1;
end;

/////////////////////////////////////////////////
//
// TprTxLineEditorForm
//
/////////////////////////////////////////////////
procedure TprTxLineEditorForm.FormCreate(Sender: TObject);
var
  I: Integer;

  function CreateSG(AParent: TTabSheet; AOnClick: TNotifyEvent): TprTxCharGrid;
  begin
    Result := TprTxCharGrid.Create(Self);
    with Result do
    begin
      Align := alClient;
      Parent := AParent;
      FontCharset := TprTxReport(DesignerPanel.GetReport).FontCharset;
      OnClick := AOnClick;
    end;
  end;

  procedure SetOemFont(AFont: TFont);
  begin
    AFont.Name := sOemFontName;
    AFont.Charset := TprTxReport(DesignerPanel.GetReport).FontCharSet;
  end;
  
begin
  FCombinations := TprLineCombinations.Create;
  for I := 0 to TxReportOptions.TxFontStylesCount - 1 do
    EDTxFontStyle.Items.Add(TxReportOptions.TxFontStyles[I].Description);
  for I := 0 to TxReportOptions.TxFontOptionsCount - 1 do
    LBTxFontOptions.Items.Add(TxReportOptions.TxFontOptions[I].Description);

  SGMain := CreateSG(PMainChar, SGMainOnClick);
  SGStarting := CreateSG(PStartingChar, SGStartingOnClick);
  SGEnding := CreateSG(PEndingChar, SGEndingOnClick);

  SetOemFont(pPreviewMainChar.Font);
  SetOemFont(pPreviewStartingChar.Font);
  SetOemFont(pPreviewEndingChar.Font);

  SetOemFont(edCurrent.Font);
  SetOemFont(lbSaved.Font);
end;

procedure TprTxLineEditorForm.prRestoreProperties(Ini : TIniFile; sn : string);
begin
  FCombinations.LoadFromIni(Ini, sn);
  UpdateSaved;
end;

procedure TprTxLineEditorForm.prSaveProperties(Ini : TIniFile; sn : string);
begin
  FCombinations.SaveToIni(Ini, sn);
end;

procedure TprTxLineEditorForm.UpdateLineChar(c: char; APanel: TPanel; AEdit: TEdit; AGrid: TprTxCharGrid);
begin
  if c = #0 then
  begin
    APanel.Caption := '';
    AEdit.Text := '';
    AGrid.Col := 0;
    AGrid.Row := 0;
  end
  else
  begin
    APanel.Caption := c;
    AEdit.Text := IntToHex(byte(c), 2);
    AGrid.Col := Byte(c) mod 16;
    AGrid.Row := Byte(c) div 16;
  end;
end;

procedure TprTxLineEditorForm.UpdateCurrent;
begin
  if (Starting <> #0) and (Main <> #0) and (Ending <> #0) then
    edCurrent.Text := GetCombinationString(Integer(Starting), Integer(Main), Integer(Ending))
  else
    edCurrent.Text := '';
  UpdateEnabled;
end;

procedure TprTxLineEditorForm.UpdateEnabled;
begin
  bSave.Enabled := FCombinations.IndexByChars(Starting, Main, Ending) = -1;
  bLoad.Enabled := (lbSaved.ItemIndex >= 0) and (lbSaved.ItemIndex < lbSaved.Items.Count); 
end;

procedure TprTxLineEditorForm.UpdateSaved;
var
  I: Integer;
begin
  lbSaved.Items.Clear;
  for I := 0 to FCombinations.Count - 1 do
    lbSaved.Items.AddObject(GetCombinationString(Integer(FCombinations[I].StartingChar),
                                                 Integer(FCombinations[I].MainChar),
                                                 Integer(FCombinations[I].EndingChar)),
                            FCombinations[I]);
  if lbSaved.Items.Count > 0 then
    lbSaved.ItemIndex := 0;
  UpdateEnabled;
end;

procedure TprTxLineEditorForm.CopySinglePropertiesFromControls;
begin
  inherited;
end;

procedure TprTxLineEditorForm.CopyMultiplyPropertiesFromControls;
var
  i,j: integer;
begin
  prSetProp(L, 'DefaultFont', CBDefaultFont.State = cbChecked, CBDefaultFont.State = cbGrayed);

  if EDTxFontStyle.ItemIndex <> -1 then
    for I := 0 to L.Count - 1 do
      TprTxLineObjRecVersion(L[I]).TxFontStyleEx := TxReportOptions.TxFontStyles[EDTxFontStyle.ItemIndex];
      
  for I := 0 to TxReportOptions.TxFontOptionsCount - 1 do
    if LBTxFontOptions.State[I] = cbUnchecked then
      for J := 0 to L.Count - 1 do
        TprTxLineObjRecVersion(L[J]).TxFontOptionsEx.Remove(TxReportOptions.TxFontOptions[I])
    else
      if LBTxFontOptions.State[I]=cbChecked then
        for J := 0 to L.Count - 1 do
          TprTxLineObjRecVersion(L[J]).TxFontOptionsEx.Add(TxReportOptions.TxFontOptions[I]);

  if Main <> #0 then
    prSetProp(L, 'MainChar', Byte(Main), False);
  if Starting <> #0 then
    prSetProp(L, 'StartingChar', Byte(Starting), False);
  if Ending <> #0 then
    prSetProp(L, 'EndingChar', Byte(Ending), False);

  inherited;
end;

procedure TprTxLineEditorForm.SetEnabledAfterCopyToControls;
begin
end;

procedure TprTxLineEditorForm.CopySinglePropertiesToControls;
begin
  inherited;
end;

procedure TprTxLineEditorForm.CopyMultiplyPropertiesToControls(L: TList);
var
  i,j: Integer;
  flgAnd, flgOr : boolean;

  procedure CheckProp(const APropName: string; APanel: TPanel; AEdit: TEdit; AGrid: TprTxCharGrid);
  var
    AChar: Variant;
  begin
    if prGetProp(L, APropName, AChar) then
      // convert AChar: win -> oem
      UpdateLineChar(Char(Integer(AChar)),
                     APanel,
                     AEdit,
                     AGrid)
    else
      UpdateLineChar(#0, APanel, AEdit, AGrid);
  end;
  
begin
  CBDefaultFont.State := prGetPropDefBool(L, 'DefaultFont');

  EDTxFontStyle.ItemIndex := GetEDFontStyleItemIndex(L);
  for I := 0 to TxReportOptions.TxFontOptionsCount - 1 do
  begin
    flgAnd := true;
    flgOr := false;
    for J := 0 to L.Count - 1 do
      with TprTxLineObjRecVersion(L[J]) do
      begin
        flgAnd := flgAnd and TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[i]);
        flgOr := flgOr or TxFontOptionsEx.Contains(TxReportOptions.TxFontOptions[i]);
      end;
    if flgAnd then
      LBTxFontOptions.State[I] := cbChecked
    else
      if flgOr then
        LBTxFontOptions.State[I] := cbGrayed
      else
        LBTxFontOptions.State[I] := cbUnchecked;
  end;

  CheckProp('MainChar', pPreviewMainChar, edMainCharHexCode, SGMain);
  CheckProp('StartingChar', pPreviewStartingChar, edStartingCharHexCode, SGStarting);
  CheckProp('EndingChar', pPreviewEndingChar, edEndingCharHexCode, SGEnding);

  UpdateCurrent;
end;

function TprTxLineEditorForm.GetCombinationString(AStarting, AMain, AEnding: Integer): string;
begin
  Result := Format('%s%s%s ($%x$%x$%x)', [Char(AStarting), Char(AMain), Char(AEnding), AStarting, AMain, AEnding]);
end;

function TprTxLineEditorForm.GetStarting: Char;
begin
  if Length(pPreviewStartingChar.Caption) = 1 then
    Result := pPreviewStartingChar.Caption[1]
  else
    Result := #0;
end;

function TprTxLineEditorForm.GetMain: Char;
begin
  if Length(pPreviewMainChar.Caption) = 1 then
    Result := pPreviewMainChar.Caption[1]
  else
    Result := #0;
end;

function TprTxLineEditorForm.GetEnding: Char;
begin
  if Length(pPreviewEndingChar.Caption) = 1 then
    Result := pPreviewEndingChar.Caption[1]
  else
    Result := #0;
end;

procedure TprTxLineEditorForm.edMainCharHexCodeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not (Key in [#8, '0'..'9', 'A'..'F', 'a'..'f']) then
    Key := #0;
end;

procedure TprTxLineEditorForm.OnGridClick(AEdit: TEdit; APanel: TPanel; AGrid: TprTxCharGrid);
var
  AOldOnChange: TNotifyEvent;
begin
  APanel.Caption := Char(AGrid.Row * 16 + AGrid.Col);
  AOldOnChange := AEdit.OnChange;
  AEdit.OnChange := nil;
  AEdit.Text := IntToHex(AGrid.Row * 16 + AGrid.Col, 2);
  AEdit.OnChange := AOldOnChange;
  UpdateCurrent;
end;

procedure TprTxLineEditorForm.OnEditChange(AEdit: TEdit; APanel: TPanel; AGrid: TprTxCharGrid);
var
  AOldOnClick: TNotifyEvent;
  ACode: Integer;
begin
  ACode := HexToIntDef(AEdit.Text);
  APanel.Caption := Char(ACode);
  AOldOnClick := AGrid.OnClick;
  AGrid.OnClick := nil;
  AGrid.Col := ACode mod 16;
  AGrid.Row := ACode div 16;
  AGrid.OnClick := AOldOnClick;
  UpdateCurrent;
end;

procedure TprTxLineEditorForm.SGMainOnClick(Sender: TObject);
begin
  OnGridClick(edMainCharHexCode, pPreviewMainChar, SGMain);
end;

procedure TprTxLineEditorForm.SGStartingOnClick(Sender: TObject);
begin
  OnGridClick(edStartingCharHexCode, pPreviewStartingChar, SGStarting);
end;

procedure TprTxLineEditorForm.SGEndingOnClick(Sender: TObject);
begin
  OnGridClick(edEndingCharHexCode, pPreviewEndingChar, SGEnding);
end;

procedure TprTxLineEditorForm.edMainCharHexCodeChange(Sender: TObject);
begin
  OnEditChange(edMainCharHexCode, pPreviewMainChar, SGMain);
end;

procedure TprTxLineEditorForm.bAllAsMainCharClick(Sender: TObject);
begin
  UpdateLineChar(pPreviewMainChar.Caption[1], pPreviewStartingChar, edStartingCharHexCode, SGStarting);
  UpdateLineChar(pPreviewMainChar.Caption[1], pPreviewEndingChar, edEndingCharHexCode, SGEnding);
end;

procedure TprTxLineEditorForm.edStartingCharHexCodeChange(Sender: TObject);
begin
  OnEditChange(edStartingCharHexCode, pPreviewStartingChar, SGStarting);
end;

procedure TprTxLineEditorForm.edEndingCharHexCodeChange(Sender: TObject);
begin
  OnEditChange(edEndingCharHexCode, pPreviewEndingChar, SGEnding);
end;

procedure TprTxLineEditorForm.bSaveClick(Sender: TObject);
begin
  if (Starting <> #0) and (Main <> #0) and (Ending <> #0) then
  begin
    FCombinations.Add(TprLineCombination.Create(Starting, Main, Ending));
    UpdateSaved;
  end;
end;

procedure TprTxLineEditorForm.bLoadClick(Sender: TObject);
begin
  if (lbSaved.ItemIndex >= 0) and (lbSaved.ItemIndex < FCombinations.Count) then
  begin
    with FCombinations[lbSaved.ItemIndex] do
    begin
      UpdateLineChar(StartingChar, pPreviewStartingChar, edStartingCharHexCode, SGStarting);
      UpdateLineChar(MainChar, pPreviewMainChar, edMainCharHexCode, SGMain);
      UpdateLineChar(EndingChar, pPreviewEndingChar, edEndingCharHexCode, SGEnding);
    end;
  end;
end;

procedure TprTxLineEditorForm.lbSavedClick(Sender: TObject);
begin
  UpdateEnabled;
end;

end.

