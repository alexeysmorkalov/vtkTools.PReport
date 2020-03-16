{******************************************}
{                                          }
{                 PReport                  }
{                                          }
{ Copyright (c) 1999-2004 by vtkTools      }
{                                          }
{******************************************}

{Contains the definition of the TprCustomPreviewPanel class and some auxiliary classes.}
unit pr_CommonPreviewPanel;

interface

{$I PR.INC}

uses
  Windows, Messages, Classes, SysUtils, extctrls, controls, Graphics,
  forms, menus, inifiles, dialogs,

  pr_Common;

type
TprCustomPreviewPanel = class;

/////////////////////////////////////////////////
//
// TprCustomPreviewBox
//
/////////////////////////////////////////////////
TprCustomPreviewBox = class(TprScrollBox)
private
  FCanvas : TCanvas;
  function GetPreviewPanel : TprCustomPreviewPanel;
public
  property PreviewPanel : TprCustomPreviewPanel read GetPreviewPanel;
  property Canvas : TCanvas read FCanvas;

  constructor Create(AOwner : TComponent); override;
  destructor Destroy; override;
end;

{TprOnPreviewMouseDown is the type for TprCustomPreviewPanel.OnPreviewMouseDown event.
Occurs when the user presses a mouse button with the mouse pointer over a control.
Parameters:
  Sender - The TprCustomPreviewPanel object.
  X, Y - Specifies the mouse position.
  PreviewUserData - The instance of the TprPreviewUserData class that is located in the point specified by the X and Y parameters, can be nil.
  Shift - Indicates the state of the Alt, Ctrl, and Shift keys and the mouse buttons.}
TprOnPreviewMouseDown = procedure (Sender: TObject; PreviewUserData: TprPreviewUserData; X,Y: Integer; Shift: TShiftState) of object;
{TprOnPreviewDblClick is the type for TprCustomPreviewPanel.OnPreviewDblClick event.
Occurs when the user double-clicks the left mouse button when the mouse pointer is over the control.
Parameters:
  Sender - The TprCustomPreviewPanel object.
  X, Y - Specifies the mouse position.
  PreviewUserData - The instance of the TprPreviewUserData class that is located in the point specified by the X and Y parameters, can be nil.}
TprOnPreviewDblClick = procedure (Sender: TObject; PreviewUserData: TprPreviewUserData; X,Y: Integer) of object;
{TprOnPreviewMouseMove is the type for TprCustomPreviewPanel.OnPreviewMouseMove event.
This event occurs when user move the mouse over control.
Parameters:
  Sender - The TprCustomPreviewPanel object.
  X, Y - Specifies the mouse position.
  PreviewUserData - The instance of the TprPreviewUserData class that is located in the point specified by the X and Y parameters, can be nil.
  cur - You can specify the type of mouse cursor in this parameter.
  HighlightObject - Return the true in this parameter to highlight object.}
TprOnPreviewMouseMove = procedure (Sender: TObject; PreviewUserData: TprPreviewUserData; X,Y: Integer; var cur: TCursor; var HighlightObject : boolean) of object;

/////////////////////////////////////////////////
//
// TprCustomPreviewPanel
//
/////////////////////////////////////////////////
{TprCustomDesignerPanel represents the base class for the preview panel.
This class has two descedants:<br>
TprPreviewPanel - Previews the TprReport template.<br>
TprTxPreviewPanel - Previews the TprTxReport template.<br>
You can use this controls if you want to develop non standard report previewers
with some unique features.
See also:
  TprPreviewPanel, TprTxPreviewPanel}
TprCustomPreviewPanel = class(TCustomPanel)
private
  FReport: TprCustomReport;

  procedure OnPopupMenuPopup(Sender : TObject);
  function GetShowPopupMenu : boolean;
  procedure SetShowPopupMenu(Value : boolean);
  function GetReportEmpty : boolean;
  procedure SetReport(Value: TprCustomReport);
protected
  FPreviewBox : TprCustomPreviewBox;
  FPopupMenu : TPopupMenu;
  FOpenDialog : TOpenDialog;
  FSaveDialog : TSaveDialog;
  FPreviewNotifyLink: TprNotifyLink;

  function GetPageIndex: Integer; virtual; abstract;
  procedure SetPageIndex(Value: Integer); virtual; abstract;
  function GetPageCount: Integer; virtual; abstract;

  function CreatePreviewBox : TprCustomPreviewBox; virtual; abstract;
  procedure AlignChildControls; virtual;
  procedure AlignControls(AControl: TControl; var Rect: TRect); override;

  procedure InitPopupMenu(Popup : TPopupMenu); virtual;

  procedure PreviewNotify(Source: TObject); virtual;

  procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
public
{Creates an instance of the TprCustomPreviewPanel class.}
  constructor Create(AOwner : TComponent); override;
{Frees an instance of the TprCustomPreviewPanel class.}
  destructor Destroy; override;

{Saves the control settings in the INI file.
Parameters:
  IniFile - The destination TIniFile object.
  SectionName - The name of section of INI file.}
  procedure WriteToIni(IniFile: TIniFile; const SectionName: string); virtual;
{Loads the control settings from the INI file.
Parameters:
  IniFile - The source TIniFile object.
  SectionName - The name of section of INI file.}
  procedure ReadFromIni(IniFile: TIniFile; const SectionName: string); virtual;

{Opens the built-in dialog for editing the control properties.
Return value:
  Returns the true if user press "OK" in the window.}
  function EditOptions : boolean; virtual;

{Prints the attached report.}
  procedure Print;
{Exports the attached report.}
  procedure ExportTo;
{Opens the "Open file" dialog in that the user can select a file to preview.}
  procedure Load;
{Opens the "Save file" dialog in that the user can select a file to save the generated report.}
  procedure Save;
{Loads the prepared report from the file.
Parameters:
  FileName - The name of file to load.}
  procedure LoadPreparedReport(const FileName : string); virtual; abstract;
{Saves the prepared report to the file.
Parameters:
  FileName - The name of file to save.}
  procedure SavePreparedReport(const FileName : string); virtual;

{Forces the updating of control.}
  procedure FullUpdate; virtual; abstract;

{Call this method to notify all other objects about the changes in the generated report (to force their updating).}
  procedure PrvNotifyReport; virtual;

{Displays the page with specified index.}
  procedure GotoPage(APageIndex: Integer);
{Displays the previous page.}
  procedure GotoPriorPage;
{Displays the next page.}
  procedure GotoNextPage;
{Displays the first page.}
  procedure GotoFirstPage;
{Displays the last page.}
  procedure GotoLastPage;

{Returns the true if attached report is empty.}
  property ReportEmpty: Boolean read GetReportEmpty;
{Specifies the TprCustomReport object that is attached to the control.}
  property Report: TprCustomReport read FReport write SetReport;
{Specifies the zero-base index of the displayed page.}
  property PageIndex: Integer read GetPageIndex write SetPageIndex;
{Returns the number of pages in the connected report, or -1 if report is not connected.}
  property PageCount: Integer read GetPageCount;
published
{Specifies the value indicating whether the built-in popup menu must be displayed if user click
the right mouse button within a control's area.}
  property ShowPopupMenu: Boolean read GetShowPopupMenu write SetShowPopupMenu default True; 
end;

var
  FNormalCursor, FScrollCursor : TCursor;

implementation

uses
  pr_Strings, pr_Multilang;

const
  crPreviewNormalCursor = 1;
  crPreviewScrollCursor = 2;

/////////////////////////////////////////////////
//
// TprCustomPreviewBox
//
/////////////////////////////////////////////////
constructor TprCustomPreviewBox.Create(AOwner : TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
end;

destructor TprCustomPreviewBox.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

function TprCustomPreviewBox.GetPreviewPanel : TprCustomPreviewPanel;
begin
  Result := TprCustomPreviewPanel(Parent);
end;

/////////////////////////////////////////////////
//
// TprCustomPreviewPanel
//
/////////////////////////////////////////////////
constructor TprCustomPreviewPanel.Create(AOwner: TComponent);
begin
  inherited;
  Width := 150;
  Height := 150;
  BevelOuter := bvNone;
  TabStop := True;

  FPreviewNotifyLink := TprNotifyLink.Create;
  FPreviewNotifyLink.OnNotify := PreviewNotify;

  FPreviewBox := CreatePreviewBox;
  FPreviewBox.Parent := Self;
  
  FPopupMenu := TPopupMenu.Create(nil);
  FPopupMenu.OnPopup := OnPopupMenuPopup;
  
  ShowPopupMenu := true;
  if not(csDesigning in ComponentState) then
  begin
    FOpenDialog := TOpenDialog.Create(Self);
    FOpenDialog.Filter := prLoadStr(sPreviewFileMask);
    FOpenDialog.DefaultExt := 'pr';
    FOpenDialog.Options := [ofHideReadOnly,ofPathMustExist,ofFileMustExist,ofEnableSizing];
    FSaveDialog := TSaveDialog.Create(Self);
    FSaveDialog.Filter := prLoadStr(sPreviewFileMask);
    FSaveDialog.DefaultExt := 'pr';
    FSaveDialog.Options := [ofHideReadOnly,ofPathMustExist,ofEnableSizing];
  end;
end;

destructor TprCustomPreviewPanel.Destroy;
begin
  if Report<>nil then
    Report.PrvRemoveNotifyLink(FPreviewNotifyLink);
  FPreviewNotifyLink.Free;
  FPopupMenu.Free;
  inherited;
end;

procedure TprCustomPreviewPanel.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = Report) then
    Report := nil;
end;

procedure TprCustomPreviewPanel.SetReport(Value: TprCustomReport);
begin
  if FReport <> Value then
  begin
    if FReport <> nil then
      FReport.PrvRemoveNotifyLink(FPreviewNotifyLink);
    FReport := Value;
    if FReport <> nil then
      FReport.PrvAddNotifyLink(FPreviewNotifyLink);
    FullUpdate;
  end;
end;

function TprCustomPreviewPanel.GetReportEmpty : boolean;
begin
Result := (Report = nil) or Report.PreparedReportEmpty;
end;

procedure TprCustomPreviewPanel.AlignChildControls;
begin
FPreviewBox.SetBounds(0,0,ClientWidth,ClientHeight);
end;

procedure TprCustomPreviewPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
AlignChildControls;
end;

function TprCustomPreviewPanel.GetShowPopupMenu : boolean;
begin
Result := FPreviewBox.PopupMenu=FPopupMenu;
end;

procedure TprCustomPreviewPanel.SetShowPopupMenu(Value : boolean);
begin
if Value then
  FPreviewBox.PopupMenu := FPopupMenu
else
  FPreviewBox.PopupMenu := nil;
end;

procedure TprCustomPreviewPanel.OnPopupMenuPopup(Sender : TObject);
begin
InitPopupMenu(FPopupMenu);
end;

procedure TprCustomPreviewPanel.InitPopupMenu(Popup : TPopupMenu);
begin
end;

procedure TprCustomPreviewPanel.PreviewNotify(Source: TObject);
begin
  FullUpdate;
end;

procedure TprCustomPreviewPanel.WriteToIni(IniFile : TIniFile; const SectionName : string);
begin
end;

procedure TprCustomPreviewPanel.ReadFromIni(IniFile : TIniFile; const SectionName : string);
begin
end;

function TprCustomPreviewPanel.EditOptions : boolean;
begin
  Result := False;
end;

procedure TprCustomPreviewPanel.Print;
begin
  if ReportEmpty then exit;
  if Report.SetupPrintParams then
    Report.PrintPreparedReport;
end;

procedure TprCustomPreviewPanel.ExportTo;
begin
  if ReportEmpty then exit;
  Report.ExportTo;
end;

procedure TprCustomPreviewPanel.Load;
begin
if FOpenDialog.Execute then
  LoadPreparedReport(FOpenDialog.FileName);
end;

procedure TprCustomPreviewPanel.Save;
begin
if ReportEmpty then exit;
if FSaveDialog.Execute then
  SavePreparedReport(FSaveDialog.FileName);
end;

procedure TprCustomPreviewPanel.SavePreparedReport(const FileName : string);
begin
  Report.SavePreparedReportToFile(FileName);
end;

procedure TprCustomPreviewPanel.PrvNotifyReport;
begin
  Report.PrvPreviewChanged(FPreviewNotifyLink);
end;

procedure TprCustomPreviewPanel.GotoPage(APageIndex: Integer);
begin
  PageIndex := APageIndex;
end;

procedure TprCustomPreviewPanel.GotoPriorPage;
begin
  if PageIndex > 0 then
    PageIndex := PageIndex - 1;
end;

procedure TprCustomPreviewPanel.GotoNextPage;
begin
  if PageIndex < PageCount then
    PageIndex := PageIndex + 1;
end;

procedure TprCustomPreviewPanel.GotoFirstPage;
begin
  PageIndex := 0;
end;

procedure TprCustomPreviewPanel.GotoLastPage;
begin
  PageIndex := PageCount - 1;
end;

var
  Cur : HCURSOR;

initialization

Cur := LoadImage(hInstance,'PR_SCROLL',IMAGE_CURSOR,0,0,LR_DEFAULTSIZE or LR_DEFAULTCOLOR);
if Cur=0 then
  FNormalCursor := crArrow
else
  begin
    Screen.Cursors[crPreviewNormalCursor] := Cur;
    FNormalCursor := crPreviewNormalCursor;
  end;

Cur := LoadImage(hInstance,'PR_SCROLLDRAG',IMAGE_CURSOR,0,0,LR_DEFAULTSIZE or LR_DEFAULTCOLOR);
if Cur=0 then
  FScrollCursor := crArrow
else
  begin
    Screen.Cursors[crPreviewScrollCursor] := Cur;
    FScrollCursor := crPreviewScrollCursor;
  end;

end.
