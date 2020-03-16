{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the TprDataset, TprStringsDataset, TprEventsDataset that can be used as "virtual" datasets in PReport.}
unit pr_Dataset;

{$I pr.inc}

interface

uses
  SysUtils, Classes,
  {$ifdef PR_D6_D7}Variants, {$endif}
  DB, stdctrls;
   
type

{Describes the type of dataset, that can be used in the PReport.}
TprDatasetType = (prdtNone, prdtNativeDataset, prdtPrDataset);
/////////////////////////////////////////////////
//
// TprDataset
//
/////////////////////////////////////////////////
{Base abstract class for virtual datasets in the PReport.
Virtual data sets can be used in a report template
as usual datasets (classes derived from TDataset), their basic purpose -
providing of access to the data of the program which are outside of a database.
For example, TprStringsDataset - provides an access to the strings' list as to a dataset.
See also:
  TprStringsDataset, TprEventsDataset, TprArrayDataset}
TprDataset = class(TComponent)
public
{Returns the true if dataset is active.}
  function Active : boolean; virtual; abstract;
{Indicates whether a dataset is positioned at the last record.}
  function Eof : boolean; virtual; abstract;
{Indicates the total number of records associated with the dataset.}
  function RecordCount : integer; virtual; abstract;
{Returns the value of dataset's field.
Parameters:
  FieldName - The field's name.
Return value:
  Returns the field's value.}
  function GetFieldValue(const FieldName : string) : Variant; virtual; abstract;

{Opens the dataset.}
  procedure Open; virtual; abstract;
{Moves to the first record in the dataset.}
  procedure First; virtual; abstract;
{Moves to the next record in the dataset.}
  procedure Next; virtual; abstract;
{Moves to the previous record in the dataset.}
  procedure Prior; virtual; abstract;

{Fills the TStrings object with fields' names.}
  procedure GetFieldsList(L : TStrings); virtual; abstract;
end;

/////////////////////////////////////////////////
//
// TprStringsDataset
//
/////////////////////////////////////////////////
{Represents the virtual dataset, that provides access to the strings' list as to a dataset.
As a strings' source can be used:<br>
- TListBox object, its Items property.<br>
- TComboBox object, its Items property.<br>
- TMemo object, its Lines property.<br>
This object has two fields:<br>
- NAME - corresponds to the TStrings.Items property.<br>
- ID - corresponds to the TStrings.Items.Objects property (object is converted to Integer).<br>
See also:
  TprDataset}
TprStringsDataset = class(TprDataset)
private
  FCurIndex          : integer;
  FStrings           : TStrings;
  FStringsSource     : TComponent;

  procedure SetStringsSource(Value : TComponent);
  function GetStrings : TStrings;
protected
  procedure Notification(AComponent : TComponent; AOperation : TOperation); override;
public
{Returns the TStrings object that is linked to object.}
  property Strings: TStrings read GetStrings write FStrings;

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

{Creates an instance of the TprStringsDataset class.}
  constructor Create(AOwner : TComponent); override;
published
{Specifies the strings' source, as a strings' source can be used:<br>
- TListBox object, its Items property.<br>
- TComboBox object, its Items property.<br>
- TMemo object, its Lines property.<br>
Exception will be raised if you will assign object of another type to this property.}
  property StringsSource: TComponent read FStringsSource write SetStringsSource;
end;

{TprEventsDatasetOnProcedure is the type for events of TprEventsDataset object.
Parameters:
  prDataset - The TprEventsDataset object that fires the event.}
TprEventsDatasetOnProcedure = procedure (prDataset: TprDataset) of object;
{TprEventsDatasetOnActive is the type for TprEventsDataset.OnActive event.
This event occurs when the dataset is tested - is it opened or not.
Parameters:
  prDataset - The TprEventsDataset object.
  IsActive - Return the true in this parameter if dataset is active.}
TprEventsDatasetOnActive = procedure (prDataset: TprDataset; var IsActive : boolean) of object;
{TprEventsDatasetOnEof is the type for TprEventsDataset.OnEof event.
This event occurs when the dataset is tested - is it positioned at the last record or not.
Parameters:
  prDataset - The TprEventsDataset object.
  IsEof - Return the true in this parameter if dataset is positioned at the last record.}
TprEventsDatasetOnEof = procedure (prDataset: TprDataset; var IsEof : boolean) of object;
{TprEventsDatasetOnRecordCount is the type for TprEventsDataset.OnRecordCount event.
This event occurs when the number of records in the dataset is needed.
Parameters:
  prDataset - The TprEventsDataset object.
  RecordCount - Return the number of records in the dataset in this parameter.}
TprEventsDatasetOnRecordCount = procedure (prDataset: TprDataset; var RecordCount : integer) of object;
{TprEventsDatasetOnGetFieldValue is the type for TprEventsDataset.OnGetFieldValue event.
This event occurs when the value of dataset's field is needed.
Parameters:
  prDataset - The TprEventsDataset object.
  FieldName - The name of field.
  FieldValue - Return the field's value in this parameter.}
TprEventsDatasetOnGetFieldValue = procedure (prDataset: TprDataset; const FieldName : string; var FieldValue : Variant) of object;
{TprEventsDatasetOnGetFieldValue is the type for TprEventsDataset.OnGetFieldsList event.
This event occurs when the fields' list of dataset is needed.
Parameters:
  prDataset - The TprEventsDataset object.
  L - Return the names of fields in this parameter.}
TprEventsDatasetOnGetFieldsList = procedure (prDataset: TprDataset; L : TStrings) of object;

/////////////////////////////////////////////////
//
// TprEventsDataset
//
/////////////////////////////////////////////////
{Represents the virtual dataset, that allows writing dataset logic with using of Delphi events.}
TprEventsDataset = class(TprDataset)
private
  FOnActive        : TprEventsDatasetOnActive;
  FOnEof           : TprEventsDatasetOnEof;
  FOnRecordCount   : TprEventsDatasetOnRecordCount;
  FOnGetFieldValue : TprEventsDatasetOnGetFieldValue;

  FOnOpen          : TprEventsDatasetOnProcedure;
  FOnFirst         : TprEventsDatasetOnProcedure;
  FOnNext          : TprEventsDatasetOnProcedure;
  FOnPrior         : TprEventsDatasetOnProcedure;

  FOnGetFieldsList : TprEventsDatasetOnGetFieldsList;
public
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
published
{Occurs when the dataset is tested - is it opened or not.
See also:
  TprEventsDatasetOnActive}
  property OnActive: TprEventsDatasetOnActive read FOnActive write FOnActive;
{Occurs when the dataset is tested - is it positioned at the last record or not.
See also:
  TprEventsDatasetOnEof}
  property OnEof: TprEventsDatasetOnEof read FOnEof write FOnEof;
{Occurs when the number of records in the dataset is needed.
See also:
  TprEventsDatasetOnRecordCount}
  property OnRecordCount: TprEventsDatasetOnRecordCount read FOnRecordCount write FOnRecordCount;
{Occurs when the value of dataset's field is needed.
See also:
  TprEventsDatasetOnGetFieldValue}
  property OnGetFieldValue: TprEventsDatasetOnGetFieldValue read FOnGetFieldValue write FOnGetFieldValue;

{Occurs when the dataset must be opened.
See also:
  TprEventsDatasetOnProcedure}
  property OnOpen: TprEventsDatasetOnProcedure read FOnOpen write FOnOpen;
{Occurs when the dataset must be moved to the first record.
See also:
  TprEventsDatasetOnProcedure}
  property OnFirst: TprEventsDatasetOnProcedure read FOnFirst write FOnFirst;
{Occurs when the dataset must be moved to the next record.
See also:
  TprEventsDatasetOnProcedure}
  property OnNext: TprEventsDatasetOnProcedure read FOnNext write FOnNext;
{Occurs when the dataset must be moved to the previous record.
See also:
  TprEventsDatasetOnProcedure}
  property OnPrior: TprEventsDatasetOnProcedure read FOnPrior write FOnPrior;

{Occurs when the fields' list of dataset is needed.
See also:
  TprEventsDatasetOnGetFieldsList}
  property OnGetFieldsList: TprEventsDatasetOnGetFieldsList read FOnGetFieldsList write FOnGetFieldsList;
end;

/////////////////////////////////////////////////
//
// TprDatasetLink
//
/////////////////////////////////////////////////
{Internal class, provides the similar access to the TprDataset and TDataset objects.}
TprDatasetLink = class(TObject)
private
  FDataset     : TObject;
  FDatasetType : TprDatasetType;

  procedure SetDataset(Value: TObject);
public
  property Dataset: TObject read FDataset write SetDataset;

  function Active: boolean;
  function Eof: boolean;

  procedure Open;
  procedure First;
  procedure Next;
  procedure Prior;

  constructor Create;
end;

implementation

uses
  pr_Strings, pr_MultiLang, pr_Utils;

/////////////////////////////////
//
// TprStringsDataset
//
/////////////////////////////////
constructor TprStringsDataset.Create;
begin
inherited;
//FStringsSourceType:=prssNone;
end;

procedure TprStringsDataset.Notification;
begin
inherited;
if (AComponent=FStringsSource) and (AOperation=opRemove) then
  FStringsSource:=nil;
end;

procedure TprStringsDataset.SetStringsSource;
begin
  if (Value = nil) or (Value is TComboBox) or (Value is TListBox) or (Value is TMemo) then
    FStringsSource := Value
  else
    raise Exception.Create(prLoadStr(sErrorInvalidStringsSource));
end;

function TprStringsDataset.GetStrings;
begin
  if FStringsSource is TComboBox then
    Result := TComboBox(FStringsSource).Items
  else
    if FStringsSource is TListBox then
      Result := TListBox(FStringsSource).Items
    else
      if FStringsSource is TComboBox then
        Result := TMemo(FStringsSource).Lines
      else
        if FStrings <> nil then
          Result := FStrings
        else
          raise Exception.Create(prLoadStr(sErrorStringsSourceNotDefined));
end;

function TprStringsDataset.Active;
begin
  Result:=true;
end;

function TprStringsDataset.Eof;
begin
  Result:=FCurIndex>=Strings.Count;
end;

function TprStringsDataset.RecordCount;
begin
  Result:=Strings.Count;
end;

function TprStringsDataset.GetFieldValue;
begin
  if CompText('NAME',FieldName)=0 then
    Result:=Strings[FCurIndex]
  else
    if CompText('ID',FieldName)=0 then
      Result:=integer(Strings.Objects[FCurIndex]);
end;

procedure TprStringsDataset.Open;
begin
  FCurIndex:=0;
end;

procedure TprStringsDataset.First;
begin
  FCurIndex:=0;
end;

procedure TprStringsDataset.Next;
begin
  if FCurIndex<Strings.Count then
    Inc(FCurIndex);
end;

procedure TprStringsDataset.Prior;
begin
  if FCurIndex>0 then
    Dec(FCurIndex);
end;

procedure TprStringsDataset.GetFieldsList;
begin
  L.Add('NAME');
  L.Add('ID');
end;

/////////////////////////////////
//
// TprDataset
//
/////////////////////////////////

/////////////////////////////////
//
// TprDatasetLink
//
/////////////////////////////////
constructor TprDatasetLink.Create;
begin
inherited;
FDataset    :=nil;
FDatasetType:=prdtNone;
end;

procedure TprDatasetLink.SetDataset;
begin
if (Value is TDataset) or (Value is TprDataset) or (Value=nil) then
  begin
    FDataset:=Value;
    if FDataset=nil then
      FDatasetType:=prdtNone
    else
      if FDataset is TDataset then
        FDatasetType:=prdtNativeDataset
      else
        FDatasetType:=prdtPrDataset;
  end
else
  raise Exception.Create(prLoadStr(sErrorInvalidDatasetForprDataset));
end;

function TprDatasetLink.Active;
begin
Result:=false;
case FDatasetType of
  prdtNativeDataset: Result:=TDataset(FDataset).Active;
  prdtPrDataset    : Result:=TprDataset(FDataset).Active;
end;
end;

function TprDatasetLink.Eof;
begin
Result:=false;
case FDatasetType of
  prdtNativeDataset: Result:=TDataset(FDataset).Eof;
  prdtPrDataset    : Result:=TprDataset(FDataset).Eof;
end;
end;

procedure TprDatasetLink.Open;
begin
case FDatasetType of
  prdtNativeDataset: TDataset(FDataset).Open;
  prdtPrDataset    : TprDataset(FDataset).Open;
end;
end;

procedure TprDatasetLink.First;
begin
case FDatasetType of
  prdtNativeDataset: TDataset(FDataset).First;
  prdtPrDataset    : TprDataset(FDataset).First;
end;
end;

procedure TprDatasetLink.Next;
begin
case FDatasetType of
  prdtNativeDataset: TDataset(FDataset).Next;
  prdtPrDataset    : TprDataset(FDataset).Next;
end;
end;

procedure TprDatasetLink.Prior;
begin
case FDatasetType of
  prdtNativeDataset: TDataset(FDataset).Prior;
  prdtPrDataset    : TprDataset(FDataset).Prior;
end;
end;

////////////////////////////
//
// TprEventsDataset
//
////////////////////////////
function TprEventsDataset.Active;
begin
Result:=false;
if Assigned(FOnActive) then
  FOnActive(Self,Result);
end;

function TprEventsDataset.Eof;
begin
Result:=false;
if Assigned(FOnEof) then
  FOnEof(Self,Result);
end;

function TprEventsDataset.RecordCount;
begin
Result:=-1;
if Assigned(FOnRecordCount) then
  FOnRecordCount(Self,Result);
end;

function TprEventsDataset.GetFieldValue;
begin
Result:=UnAssigned;
if Assigned(FOnGetFieldValue) then
  FOnGetFieldValue(Self,FieldName,Result);
end;

procedure TprEventsDataset.Open;
begin
if Assigned(FOnOpen) then
  FOnOpen(Self);
end;

procedure TprEventsDataset.First;
begin
if Assigned(FOnFirst) then
  FOnFirst(Self);
end;

procedure TprEventsDataset.Next;
begin
if Assigned(FOnNext) then
  FOnNext(Self);
end;

procedure TprEventsDataset.Prior;
begin
if Assigned(FOnPrior) then
  FOnPrior(Self);
end;

procedure TprEventsDataset.GetFieldsList;
begin
if Assigned(FOnGetFieldsList) then
  FOnGetFieldsList(Self,L);
end;

initialization

RegisterClass(TprDataset);

end.

