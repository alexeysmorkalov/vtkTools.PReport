{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

unit pr_MultiLang;

interface

uses
  Classes, typinfo, SysUtils, forms;

type

TprMLResLink = class;
////////////////////////////////
//
// TprMLResProp
//
////////////////////////////////
TprMLResProp = class(TCollectionItem)
private
  FPropName : string;
  FResID    : integer;
public
  ResLink : TprMLResLink;

  procedure Assign(Source : TPersistent); override;
published
  property PropName : string read FPropName write FPropName;
  property ResID    : integer read FResID write FResID;
end;

////////////////////////////////
//
// TprMLResLink
//
////////////////////////////////
TprMLResLink = class(TCollectionItem)
private
  FComponent : TComponent;
  FProps     : TCollection;

  procedure SetProps(Value : TCollection);
public
  procedure Assign(Source : TPersistent); override;
  
  constructor Create(Collection : TCollection); override;
  destructor Destroy; override;
published
  property Component : TComponent read FComponent write FComponent;
  property Props     : TCollection read FProps write SetProps;
end;

//////////////////////////////////
//
// TprMLRes
//
//////////////////////////////////
TprMLRes = class(TComponent)
private
  FResLinks : TCollection;
  FOldOnCreate : TNotifyEvent;

  procedure SetResLinks(Value : TCollection);
  procedure OwnerFormOnCreate(Sender : TObject);
protected
  procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  procedure Loaded; override;
public
  function IndexOfComponent(Component : TComponent) : integer;
  procedure LoadLanguageFromResource;
  
  constructor Create(AOwner : TComponent); override;
  destructor Destroy; override;
published
  property ResLinks : TCollection read FResLinks write SetResLinks;
end;

function prLoadStr(StringID : integer) : string;

implementation

uses
  pr_Strings;

function prLoadStr;
begin
Result := LoadStr(sPRResBase+StringID);
Result := StringReplace(Result,'#13',#13#10,[rfReplaceAll]);
end;

///////////////////////////////
//
// TprMLResProp
//
///////////////////////////////
procedure TprMLResProp.Assign;
begin
with TprMLResProp(Source) do
  begin
    Self.PropName:=PropName;
    Self.ResID   :=ResID;
  end;
end;

////////////////////////////////
//
// TprMLResLink
//
////////////////////////////////
constructor TprMLResLink.Create;
begin
inherited;
FProps:=TCollection.Create(TprMLResProp);
end;

destructor TprMLResLink.Destroy;
begin
inherited;
FProps.Free;
end;

procedure TprMLResLink.SetProps;
begin
FProps.Assign(Value);
end;

procedure TprMLResLink.Assign;
begin
with TprMLResLink(Source) do
  begin
    Self.Component:=Component;
    Self.Props.Assign(Props);
  end;
end;

///////////////////////////////////
//
// TprMLRes
//
///////////////////////////////////
constructor TprMLRes.Create;
begin
inherited;
FResLinks:=TCollection.Create(TprMLResLink);
end;

destructor TprMLRes.Destroy;
begin
FResLinks.Free;
inherited;
end;

procedure TprMLRes.Notification;
var
  i : integer;
begin
inherited;
if Operation=opRemove then
  begin
    i:=IndexOfComponent(AComponent);
    if i<>-1 then
      ResLinks.Items[i].Free;
  end;
end;

procedure TprMLRes.Loaded;
begin
inherited;
if (Owner is TForm) and (not(csDesigning in Owner.ComponentState)) then
  begin
    with TForm(Owner) do
      begin
       FOldOnCreate:=OnCreate;
       OnCreate    :=OwnerFormOnCreate;
      end;
  end;
end;

procedure TprMLRes.OwnerFormOnCreate;
begin
LoadLanguageFromResource;
if Assigned(FOldOnCreate) then
  FOldOnCreate(Sender);
end;

procedure TprMLRes.SetResLinks;
begin
FResLinks.Assign(Value);
end;

function TprMLRes.IndexOfComponent;
begin
Result:=0;
while (Result<ResLinks.Count) and
      (TprMLResLink(ResLinks.Items[Result]).Component<>Component) do Inc(Result);
if Result>=ResLinks.Count then
  Result:=-1;
end;

procedure TprMLRes.LoadLanguageFromResource;
var
  i,j : integer;
  s : string;
  rl : TprMLResLink;
  rp : TprMLResProp;
  pi : PPropInfo;
begin
for i:=0 to FResLinks.Count-1 do
  begin
    rl:=TprMLResLink(FResLinks.Items[i]);
    for j:=0 to rl.FProps.Count-1 do
      begin
        rp:=TprMLResProp(rl.Props.Items[j]);
        pi:=GetPropInfo(rl.Component.ClassInfo,rp.PropName);
        if (pi<>nil) and (pi.PropType^.Kind in [tkString,tkLString]) then
          begin
            // можно попробывать загрузить из ресурсов
            s:=LoadStr(rp.ResID+sPRResBase);
            if s<>'' then
              begin
                SetStrProp(rl.Component,pi,s);
              end;
          end;
      end;
  end;
end;

end.
