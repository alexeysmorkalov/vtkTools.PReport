unit FileDataset;

interface

{$I pr.inc}

uses
  Classes, SysUtils, Windows, {$IFDEF PR_D6_D7}Variants, {$ENDIF}Math, pr_Dataset;

type
  TprFileDatasetOrder = (doFileName, doFileExt, doCreated);

  /////////////////////////////////////////////////
  //
  // TprFileDatasetEntry
  //
  /////////////////////////////////////////////////
  TprFileDatasetEntry = class(TObject)
  private
    FFileName: string;
    FFileExt: string;
    FCreated: TDateTime;
  public
    constructor Create(const AFileName: string; const AFileExt: string; ACreated: TDateTime);
    property FileName: string read FFileName write FFileName;
    property FileExt: string read FFileExt write FFileExt;
    property Created: TDateTime read FCreated write FCreated; 
  end;

  /////////////////////////////////////////////////
  //
  // TprFileDataset
  //
  /////////////////////////////////////////////////
  TprFileDataset = class(TprDataset)
  private
    FDirectory: string;
    FOrder: TprFileDatasetOrder;
    FItems: TList;
    FCurrentIndex: Integer;

    procedure SetDirectory(const Value: string);
    procedure SetOrder(Value: TprFileDatasetOrder);
    function GetRecord(Index: Integer): TprFileDatasetEntry;
  protected
    procedure Update;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Active : boolean; override;
    function Eof : boolean; override;
    function RecordCount : integer; override;
    function GetFieldValue(FieldName : string) : Variant; override;

    procedure Open; override;
    procedure First; override;
    procedure Next; override;
    procedure Prior; override;

    procedure GetFieldsList(L : TStrings); override;

    property Records[Index: Integer]: TprFileDatasetEntry read GetRecord; default;
    property Directory: string read FDirectory write SetDirectory;
    property Order: TprFileDatasetOrder read FOrder write SetOrder;
  end;

implementation

uses
  pr_Utils, vgr_Functions;

/////////////////////////////////////////////////
//
// TprFileDatasetEntry
//
/////////////////////////////////////////////////
constructor TprFileDatasetEntry.Create(const AFileName: string; const AFileExt: string; ACreated: TDateTime);
begin
  inherited Create;
  FFileName := AFileName;
  FFileExt := AFileExt;
  FCreated := ACreated;
end;

/////////////////////////////////////////////////
//
// TprFileDataset
//
/////////////////////////////////////////////////
constructor TprFileDataset.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TList.Create;
  FCurrentIndex := -1;
end;

destructor TprFileDataset.Destroy;
begin
  FreeList(FItems);
  inherited;
end;

procedure TprFileDataset.SetDirectory(const Value: string);
begin
  if FDirectory <> Value then
  begin
    if not DirectoryExists(Value) then
      raise Exception.Create('Directory not found');
    FDirectory := Value;
    Update;
  end;
end;

procedure TprFileDataset.SetOrder(Value: TprFileDatasetOrder);
begin
  if FOrder <> Value then
  begin
    FOrder := Value;
    Update;
  end;
end;

function TprFileDataset.GetRecord(Index: Integer): TprFileDatasetEntry;
begin
  Result := TprFileDatasetEntry(FItems[Index]);
end;

function SortProcFileName(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareText(TprFileDatasetEntry(Item1).FileName,
                            TprFileDatasetEntry(Item2).FileName);
end;

function SortProcFileExt(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareText(TprFileDatasetEntry(Item1).FileExt,
                            TprFileDatasetEntry(Item2).FileExt);
end;

function SortProcCreated(Item1, Item2: Pointer): Integer;
begin
  if TprFileDatasetEntry(Item1).Created < TprFileDatasetEntry(Item2).Created then
    Result := -1
  else
    if TprFileDatasetEntry(Item1).Created > TprFileDatasetEntry(Item2).Created then
      Result := 1
    else
      Result := 0;
end;

procedure TprFileDataset.Update;
var
  AData: TSearchRec;

  procedure AddFile;
  var
    AEntry: TprFileDatasetEntry;
    FT: TFileTime;
    I: Integer;
  begin
    if AData.Attr and faDirectory = 0 then
    begin
      FileTimeToLocalFileTime(AData.FindData.ftCreationTime, FT);
      FileTimeToDosDateTime(FT, LongRec(I).Hi, LongRec(I).Lo);

      AEntry := TprFileDatasetEntry.Create(AData.Name,
                                           ExtractFileExt(AData.Name),
                                           FileDateToDateTime(I));
      FItems.Add(AEntry);
    end;
  end;
  
begin
  // ???
  FreeListItems(FItems);
  FItems.Clear;

  if FindFirst(AddFlash(FDirectory) + '*.*', faAnyFile, AData) = 0 then
  begin
    AddFile;
    while FindNext(AData) = 0 do
      AddFile;
  end;
  SysUtils.FindClose(AData);

  case FOrder of
    doFileExt: FItems.Sort(SortProcFileExt);
    doCreated: FItems.Sort(SortProcCreated);
    else FItems.Sort(SortProcFileName);
  end;

  FCurrentIndex := 0;
end;

function TprFileDataset.Active: Boolean;
begin
  Result := (FCurrentIndex >= 0) and (FCurrentIndex < FItems.Count);
end;

function TprFileDataset.Eof: Boolean;
begin
  Result := FCurrentIndex >= FItems.Count;
end;

function TprFileDataset.RecordCount: Integer;
begin
  Result := FItems.Count;
end;

function TprFileDataset.GetFieldValue(FieldName: string): Variant;
begin
  with Records[Min(FItems.Count - 1, FCurrentIndex)] do
    if AnsiCompareText(FieldName, 'FileName') = 0 then
      Result := FileName
    else
      if AnsiCompareText(FieldName, 'FileExt') = 0 then
        Result := FileExt
      else
        if AnsiCompareText(FieldName, 'Created') = 0 then
          Result := Created
        else
          Result := Null;
end;

procedure TprFileDataset.Open;
begin
  Update;
end;

procedure TprFileDataset.First;
begin
  FCurrentIndex := 0;
end;

procedure TprFileDataset.Next;
begin
  FCurrentIndex := FCurrentIndex + 1;
end;

procedure TprFileDataset.Prior; 
begin
  FCurrentIndex := FCurrentIndex - 1;
end;

procedure TprFileDataset.GetFieldsList(L : TStrings);
begin
  L.Add('FileName');
  L.Add('FileExt');
  L.Add('Created');
end;

end.
