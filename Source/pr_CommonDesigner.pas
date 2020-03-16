{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2002 by vtkTools      }
{                                          }
{******************************************}

{Contains the TprCustomDesignerForm class - base class for built-in report designers.
See also:
  TprCustomDesignerForm}
unit pr_CommonDesigner;

interface

{$I pr.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ActnList, Menus, ExtCtrls,
  TypInfo, ClipBrd, IniFiles, ComCtrls,
  StdCtrls,

  Pr_Utils, pr_Common, pr_Link, pr_VersionsEditor,
  pr_MultiLang, pr_DesignerFunctions, pr_CommonDesignerPanel;

type
  /////////////////////////////////////////////////
  //
  // TprCustomDesignerForm
  //
  /////////////////////////////////////////////////
{Represents the base class for built-in report designers.
See also:
  TprDesignerForm, TprTxDesignerForm}
  TprCustomDesignerForm = class(TprDesigner)
  private
    procedure SetFileName(Value : string);
    function GetFileName : string;
  protected
    function GetSelectionComponentsDesc : string;
    procedure OnDesignerPanelInplaceEditObject(Sender: TObject; EditedObject: TprObj); virtual;
    procedure OnDesignerPanelEndInplaceEditObject(Sender: TObject); virtual;
    procedure OnDesignerPanelSaveInplaceEditObject(Sender: TObject); virtual;
    procedure OnDesignerPanelObjectResized(Sender: TObject; ResizeObject: TprDesignComponent); virtual;
    procedure OnDesignerPanelObjectDrag(Sender: TObject; DragObject: TprDesignComponent); virtual;
    procedure OnDesignerPanelSelectionChanged(Sender : TObject; SelCount: Integer); virtual;
    procedure OnDesignerPanelFileNameChanged(Sender : TObject); virtual;
    procedure OnDesignerPanelSaveTemplate(Sender : TObject; var Stream : TStream; var CancelSave : boolean; var IsBinaryFormat : boolean; IsSaveAs : boolean);
    procedure OnDesignerPanelOpenTemplate(Sender : TObject; var Stream : TStream; var CancelOpen : boolean; var IsBinaryFormat : boolean);
    function GetDesignerPanel : TprCustomDesignerPanel; virtual; abstract;
    procedure InitDesignerPanel; virtual;

    procedure prRestoreProperties(Ini : TIniFile; sn : string); override;
    procedure prSaveProperties(Ini : TIniFile; sn : string); override;

    procedure OnInsertBandClick(Sender : TObject);
    procedure OnObjButtonClick(Sender : TObject);

    procedure UpdateSizesInToolBar; virtual; abstract;
  public
{Specifies the name of the currently opened file.}
    property FileName: string read GetFileName write SetFileName;
{Returns the TprCustomDesignerPanel control that is used for editing the report template.}
    property DesignerPanel : TprCustomDesignerPanel read GetDesignerPanel;

{Updates the form.}
    procedure UpdateCurrentPage; override;
{Updates the buttons that are linked with text actions (font style, name of font).}
    procedure UpdateTextActions; virtual; abstract;

{Initializes the "Save" action.}
    procedure Save; virtual;
{Initializes the "Save as" action.}
    procedure SaveAs; virtual;
{Initializes the "Open" action.}
    procedure Open; virtual;
{Initializes the "New" action.}
    procedure New;
{Initializes the "Print" action.}
    procedure Print;
{Initializes the "Preview" action.}
    procedure Preview;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

{Opens the built-in groups' designer.}
    procedure EditGroups;
{Opens the built-in designer of the aggregate variables.}
    procedure EditValues;
{Opens the built-in designer of the variables.}
    procedure EditVariables;
  end;

implementation

uses
  pr_Strings, pr_ObjectsProps;

/////////////////////////////////////////////////
//
// TprCustomDesignerForm
//
/////////////////////////////////////////////////
procedure TprCustomDesignerForm.BeforeDestruction;
begin
inherited;
end;

procedure TprCustomDesignerForm.AfterConstruction;
begin
OnDesignerPanelFileNameChanged(DesignerPanel);
InitDesignerPanel;
if Report.PagesCount=0 then
  DesignerPanel.InsertPageAfter(-1);
inherited;
end;

function TprCustomDesignerForm.GetFileName : string;
begin
Result := DesignerPanel.FileName;
end;

procedure TprCustomDesignerForm.SetFileName;
begin
DesignerPanel.FileName := Value;
end;

procedure TprCustomDesignerForm.UpdateCurrentPage;
begin
DesignerPanel.UpdateCurPage;
UpdateTextActions;
end;

procedure TprCustomDesignerForm.OnInsertBandClick(Sender : TObject);
begin
DesignerPanel.InsertBand(TprBandType(TMenuItem(Sender).Tag));
end;

procedure TprCustomDesignerForm.OnObjButtonClick(Sender: TObject);
var
  Tag : integer;
begin
(Sender as TToolButton).Down := true;
Tag := (Sender as TToolButton).Tag;
if Tag=-1 then
  DesignerPanel.InsertObject(nil)
else
  DesignerPanel.InsertObject(prObjRegInfos[Tag].ClassRef);
end;

procedure TprCustomDesignerForm.Save;
begin
DesignerPanel.Save;
end;

procedure TprCustomDesignerForm.SaveAs;
begin
DesignerPanel.SaveAs;
end;

procedure TprCustomDesignerForm.Open;
begin
DesignerPanel.Open;
end;

procedure TprCustomDesignerForm.New;
begin
DesignerPanel.New;
end;

procedure TprCustomDesignerForm.Print;
begin
DesignerPanel.Print;
end;

procedure TprCustomDesignerForm.Preview;
begin
DesignerPanel.Preview;
end;

procedure TprCustomDesignerForm.EditGroups;
begin
DesignerPanel.EditGroups;
end;

procedure TprCustomDesignerForm.EditValues;
begin
DesignerPanel.EditValues;
end;

procedure TprCustomDesignerForm.EditVariables;
begin
DesignerPanel.EditVariables;
end;

procedure TprCustomDesignerForm.prRestoreProperties(Ini : TIniFile; sn : string);
begin
DesignerPanel.ReadFromIni(Ini,sn);
inherited;
end;

procedure TprCustomDesignerForm.prSaveProperties(Ini : TIniFile; sn : string);
begin
DesignerPanel.WriteToIni(Ini,sn);
inherited;
end;

procedure TprCustomDesignerForm.InitDesignerPanel;
begin
DesignerPanel.OnInplaceEditObject := OnDesignerPanelInplaceEditObject;
DesignerPanel.OnSaveInplaceEditObject := OnDesignerPanelSaveInplaceEditObject;
DesignerPanel.OnEndInplaceEditObject := OnDesignerPanelEndInplaceEditObject;
DesignerPanel.OnObjectResized := OnDesignerPanelObjectResized;
DesignerPanel.OnObjectDrag := OnDesignerPanelObjectDrag;
DesignerPanel.OnSelectionChanged := OnDesignerPanelSelectionChanged;
DesignerPanel.OnFileNameChanged := OnDesignerPanelFileNameChanged;
DesignerPanel.OnSaveTemplate := OnDesignerPanelSaveTemplate;
DesignerPanel.OnOpenTemplate := OnDesignerPanelOpenTemplate;
end;

procedure TprCustomDesignerForm.OnDesignerPanelInplaceEditObject(Sender: TObject; EditedObject: TprObj);
begin
RemoveShortCuts;
UpdateTextActions;
end;

procedure TprCustomDesignerForm.OnDesignerPanelEndInplaceEditObject(Sender: TObject);
begin
RestoreShortCuts;
UpdateTextActions;
end;

procedure TprCustomDesignerForm.OnDesignerPanelSaveInplaceEditObject(Sender: TObject); 
begin
RestoreShortCuts;
UpdateTextActions;
end;

procedure TprCustomDesignerForm.OnDesignerPanelObjectResized(Sender: TObject; ResizeObject: TprDesignComponent);
begin
UpdateSizesInToolbar;
end;

procedure TprCustomDesignerForm.OnDesignerPanelObjectDrag(Sender: TObject; DragObject: TprDesignComponent);
begin
UpdateSizesInToolbar;
end; 

function TprCustomDesignerForm.GetSelectionComponentsDesc : string;
begin
case DesignerPanel.SelCount of
  0 : Result := prLoadStr(sNoObjectsSelected);
  1 : Result := Format('%s : %s',[DesignerPanel.SelObjs[0].Name,DesignerPanel.SelObjs[0].ClassName]);
  else Result := Format(prLoadStr(sSelectedMoreThenOneObject),[DesignerPanel.SelCount]);
end;
end;

procedure TprCustomDesignerForm.OnDesignerPanelSelectionChanged(Sender : TObject; SelCount: Integer);
begin
UpdateTextActions;
UpdateSizesInToolbar;
end;

procedure TprCustomDesignerForm.OnDesignerPanelFileNameChanged;
begin
Caption := Format(prLoadStr(sDesignerCaption),[FileName]);
end;

procedure TprCustomDesignerForm.OnDesignerPanelSaveTemplate(Sender : TObject; var Stream : TStream; var CancelSave : boolean; var IsBinaryFormat : boolean; IsSaveAs : boolean);
var
  FileName : string;
begin
if Assigned(Report.OnDesignerSaveTemplate) then
  begin
    FileName := Self.FileName;
    Report.OnDesignerSaveTemplate(Report,FileName,Stream,CancelSave,IsBinaryFormat,IsSaveAs);
    Self.FileName := FileName;
  end;
end;

procedure TprCustomDesignerForm.OnDesignerPanelOpenTemplate(Sender : TObject; var Stream : TStream; var CancelOpen : boolean; var IsBinaryFormat : boolean);
var
  FileName : string;
begin
if Assigned(Report.OnDesignerOpenTemplate) then
  begin
    FileName := Self.FileName;
    Report.OnDesignerOpenTemplate(Report,FileName,Stream,CancelOpen,IsBinaryFormat);
    Self.FileName := FileName;
  end;
end;

end.

