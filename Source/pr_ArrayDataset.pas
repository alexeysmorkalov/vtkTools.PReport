{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the TprArrayDataset class.}
unit pr_ArrayDataset;

{$I pr.inc}

interface

uses
   SysUtils, Classes, Math,
   {$ifdef PR_D6_D7}Variants,{$endif}
   DB, stdctrls,

   pr_Dataset;

type

  TprArrayDataset = class;

{TprArrayDatasetGetFieldValue is the type for TprArrayDataset.OnGetFieldValue event.
This event occurs when the value of dataset's field is needed.
Parameters:
  Sender - The TprArrayDataset object.
  ARecNo - The number of current record.
  AFieldName - The field's name.
  AFieldValue - Return the field's value in this parameter.}
  TprArrayDatasetGetFieldValue = procedure (Sender: TprArrayDataset; ARecNo: Integer; const AFieldName: string; var AFieldValue: Variant) of object;
  /////////////////////////////////////////////////
  //
  // TprArrayDataset
  //
  /////////////////////////////////////////////////
{Represents the virtual dataset, that provides access to the array as to a dataset.
See also:
  TprDataset, TprStringsDataset, TprEventsDataset}
  TprArrayDataset = class(TprDataset)
  private
    FFields: TStrings;
    FRecCount: Integer;
    FOnGetFieldValue: TprArrayDatasetGetFieldValue;

    FRecNo: Integer;
    FEof: Boolean;
    procedure SetFields(Value: TStrings);
    procedure SetRecCount(Value: Integer);
  protected
    procedure DoGetFieldValue(const AFieldName: string; var AFieldValue: Variant);
  public
{Creates an instance of the TprArrayDataset class.}
    constructor Create(AOwner: TComponent); override;
{Frees an instance of the TprArrayDataset class.}
    destructor Destroy; override;

{See:
  TprDataset.Active}
    function Active: boolean; override;
{See:
  TprDataset.Eof}
    function Eof : boolean; override;
{See:
  TprDataset.RecordCount}
    function RecordCount : integer; override;
{See:
  TprDataset.GetFieldValue}
    function GetFieldValue(const FieldName : string) : Variant; override;
{See:
  TprDataset.Open}
    procedure Open; override;
{See:
  TprDataset.First}
    procedure First; override;
{See:
  TprDataset.Next}
    procedure Next; override;
{See:
  TprDataset.Prior}
    procedure Prior; override;

{See:
  TprDataset.Open}
    procedure GetFieldsList(L : TStrings); override;

{Returns the number of current record, corresponds to an index of array element.
Can change from 1 to RecCount}
    property RecNo: Integer read FRecNo;
  published
{Specifies the fields' names in the dataset.}
    property Fields: TStrings read FFields write SetFields;
{Specifies the number of records in the dataset.}
    property RecCount: Integer read FRecCount write SetRecCount default 1;
{Occurs when the value of dataset's field is needed.
See also:
  TprArrayDatasetGetFieldValue}
    property OnGetFieldValue: TprArrayDatasetGetFieldValue read FOnGetFieldValue write FOnGetFieldValue;
  end;

implementation

/////////////////////////////////////////////////
//
// TprArrayDataset
//
/////////////////////////////////////////////////
constructor TprArrayDataset.Create(AOwner: TComponent);
begin
  inherited;
  FFields := TStringList.Create;
  FRecCount := 1;
  FRecNo := 1;
end;

destructor TprArrayDataset.Destroy;
begin
  FFields.Free;
  inherited;
end;

procedure TprArrayDataset.SetFields(Value: TStrings);
begin
  FFields.Assign(Value);
end;

procedure TprArrayDataset.SetRecCount(Value: Integer);
begin
  FRecCount := Value;
  First;
end;

procedure TprArrayDataset.DoGetFieldValue(const AFieldName: string; var AFieldValue: Variant);
begin
  AFieldValue := Null;
  if Assigned(FOnGetFieldValue) then
    FOnGetFieldValue(Self, RecNo, AFieldName, AFieldValue);
end;

function TprArrayDataset.Active: Boolean;
begin
  Result := True;
end;

function TprArrayDataset.Eof: Boolean;
begin
  Result := FEof;
end;

function TprArrayDataset.RecordCount: Integer;
begin
  Result := FRecCount;
end;

function TprArrayDataset.GetFieldValue(const FieldName: string): Variant;
begin
  DoGetFieldValue(FieldName, Result);
end;

procedure TprArrayDataset.Open;
begin
  First;
end;

procedure TprArrayDataset.First;
begin
  FRecNo := Min(FRecCount, 1);
  FEof := FRecNo < 1;
end;

procedure TprArrayDataset.Next;
begin
  if FRecNo < FRecCount then
    Inc(FRecNo)
  else
    FEof := True;
end;

procedure TprArrayDataset.Prior;
begin
  if FRecNo > 1 then
    Dec(FRecNo);
end;

procedure TprArrayDataset.GetFieldsList(L: TStrings);
begin
  L.Assign(FFields);
end;

end.
