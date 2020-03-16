object Form1: TForm1
  Left = 216
  Top = 110
  Width = 325
  Height = 375
  Caption = 'Show/Hide objects'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 48
    Top = 8
    Width = 232
    Height = 26
    Caption = 
      'In this demo for all customers with CustNo<=1500'#13#10'showed its pho' +
      'ne and fax'
  end
  object Button1: TButton
    Left = 48
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Show report'
    TabOrder = 0
    OnClick = Button1Click
  end
  object customer: TTable
    Active = True
    DatabaseName = 'DBDEMOS'
    TableName = 'customer.db'
    Left = 8
    Top = 8
  end
  object prTxReport1: TprTxReport
    ShowProgress = False
    CanUserEdit = False
    DesignerFormMode = fmNormal
    PreviewFormMode = fmNormal
    Collate = False
    Copies = 1
    FromPage = -1
    ToPage = -1
    PrintPagesMode = ppmAll
    ExportOptions = [preoShowParamsDlg, preoShowProgress, preoShowAfterGenerate]
    ExportPagesMode = ppmAll
    ExportFromPage = 0
    ExportToPage = 0
    Values = <>
    LeftSpaces = 0
    WrapAfterColumn = 0
    StartNewLineOnWrap = False
    EjectPageAfterPrint = False
    PaperType = ptPage
    UseLinesOnPage = False
    LinesOnPage = 0
    MakeFormFeedOnRulon = False
    PrintRulonMode = prmAllLines
    FromLine = 0
    ToLine = 0
    ExportTxOptions = []
    ExportFromLine = 0
    ExportToLine = 0
    ExportCodePage = prtxcpDOS866
    Left = 8
    Top = 40
    SystemInfo = (
      'OS: WIN32_NT 5.1.2600 '
      ''
      'PageSize: 4096'
      'ActiveProcessorMask: $1000'
      'NumberOfProcessors: 1'
      'ProcessorType: 586'
      ''
      'Compiler version: Delphi5'
      'PReport version: 1.80')
    object prTxPage1: TprTxPage
      Width = 0
      Height = 0
      LineNum = 60
      ColNum = 70
      PageType = tptRoll
      object prTxHDetailBand1: TprTxHDetailBand
        Height = 6
        UseVerticalBands = False
        DataSetName = 'customer'
        ColCount = 0
        ColDirection = prcdTopBottomLeftRight
        object prTxMemoObj2: TprTxMemoObj
          dRec.DefVersion = 0
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                '[customer.CustNo]')
              DeleteEmptyLinesAtEnd = False
              DeleteEmptyLines = False
              CanResizeX = False
              CanResizeY = False
              hAlign = prhRight
              vAlign = prvTop
              DefaultFont = False
              WordWrap = False
            end>
          dRec.Left = 1
          dRec.Top = 1
          dRec.Right = 11
          dRec.Bottom = 2
          Visible = False
        end
        object prTxMemoObj3: TprTxMemoObj
          dRec.DefVersion = 0
          dRec.Versions = <
            item
              Visible = True
              DeleteEmptyLinesAtEnd = False
              DeleteEmptyLines = False
              CanResizeX = False
              CanResizeY = False
              hAlign = prhLeft
              vAlign = prvTop
              DefaultFont = False
              WordWrap = False
            end>
          dRec.Left = 14
          dRec.Top = -59
          dRec.Right = 42
          dRec.Bottom = -58
          Visible = False
        end
        object prTxMemoObj4: TprTxMemoObj
          dRec.DefVersion = 0
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                '[customer.Company]')
              DeleteEmptyLinesAtEnd = False
              DeleteEmptyLines = False
              CanResizeX = False
              CanResizeY = False
              hAlign = prhLeft
              vAlign = prvTop
              DefaultFont = False
              WordWrap = True
            end>
          dRec.Left = 12
          dRec.Top = 1
          dRec.Right = 43
          dRec.Bottom = 3
          Visible = False
        end
        object prTxMemoObj5: TprTxMemoObj
          dRec.DefVersion = 0
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                'Phone: [customer.Phone]'
                'FAX  : [customer.FAX]')
              DeleteEmptyLinesAtEnd = False
              DeleteEmptyLines = False
              CanResizeX = False
              CanResizeY = False
              hAlign = prhLeft
              vAlign = prvTop
              DefaultFont = False
              WordWrap = False
            end
            item
              Formula = 'customer.custno>1500'
              Visible = False
              Memo.Strings = (
                'Phone: [customer.Phone]'
                'FAX  : [customer.FAX]')
              DeleteEmptyLinesAtEnd = False
              DeleteEmptyLines = False
              CanResizeX = False
              CanResizeY = False
              hAlign = prhLeft
              vAlign = prvTop
              DefaultFont = False
              WordWrap = False
            end>
          dRec.Left = 44
          dRec.Top = 1
          dRec.Right = 67
          dRec.Bottom = 3
          Visible = False
        end
        object prTxMemoObj6: TprTxMemoObj
          dRec.DefVersion = 0
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                'Substract:'
                'customer.custno-1000 = [customer.custno-1000]'
                
                  'customer.custno-customer.custno = [customer.custno-customer.cust' +
                  'no]')
              DeleteEmptyLinesAtEnd = False
              DeleteEmptyLines = False
              CanResizeX = False
              CanResizeY = False
              hAlign = prhLeft
              vAlign = prvTop
              DefaultFont = False
              WordWrap = False
            end>
          dRec.Left = 12
          dRec.Top = 3
          dRec.Right = 67
          dRec.Bottom = 6
          Visible = False
        end
      end
      object prTxHTitleBand1: TprTxHTitleBand
        Height = 3
        UseVerticalBands = False
        object prTxMemoObj1: TprTxMemoObj
          dRec.DefVersion = 0
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                'In this demo for all customers with CustNo<=1500'
                'showed its phone and fax')
              DeleteEmptyLinesAtEnd = False
              DeleteEmptyLines = False
              CanResizeX = False
              CanResizeY = False
              hAlign = prhLeft
              vAlign = prvTop
              DefaultFont = False
              WordWrap = False
            end>
          dRec.Left = 2
          dRec.Top = 0
          dRec.Right = 68
          dRec.Bottom = 2
          Visible = False
        end
      end
    end
  end
end
