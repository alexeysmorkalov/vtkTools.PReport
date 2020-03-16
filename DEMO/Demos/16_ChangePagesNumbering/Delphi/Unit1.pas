unit Unit1;

interface

{$I pr.inc}

uses
  Windows, Messages, SysUtils, {$IFDEF PR_D6_D7} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, pr_Common, pr_Classes, DB, DBTables,
  pr_Designer;

type
  TForm1 = class(TForm)
    Button1: TButton;
    RepQuery: TQuery;
    prReport1: TprReport;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure prReport1BandGenerateCell(Sender: TObject;
      HorzBandInfo: TprGenHorzBandInfo; VertBandInfo: TprGenVertBandInfo);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
begin
  prReport1.DesignReport(True);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  prReport1.PrepareReport;
  prReport1.PreviewPreparedReport(True);
end;

procedure TForm1.prReport1BandGenerateCell(Sender: TObject;
  HorzBandInfo: TprGenHorzBandInfo; VertBandInfo: TprGenVertBandInfo);
begin
  if HorzBandInfo.Band.Name = 'prHGroupHeaderBand2' then
  begin
    HorzBandInfo.ChangePageNumberMode := prcpnSetTo;
    HorzBandInfo.ChangePageNumberValue := 1;
    HorzBandInfo.ResetPagesCount := True;
  end; 
end;

end.
