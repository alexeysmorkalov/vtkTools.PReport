object Form1: TForm1
  Left = 204
  Top = 158
  Width = 364
  Height = 202
  Caption = 'Images'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 345
    Height = 41
    TabStop = False
    BevelInner = bvNone
    BorderStyle = bsNone
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    Lines.Strings = (
      'This demo prints images from files which names stored in the '
      'database field.')
    ParentFont = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 64
    Width = 337
    Height = 25
    Caption = '1. Fill the DBF table with the filenames for WINDOWS directory'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 144
    Width = 73
    Height = 25
    Caption = 'Design...'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 96
    Width = 337
    Height = 25
    Caption = '2. Preview the report'
    Enabled = False
    TabOrder = 3
    OnClick = Button3Click
  end
  object Table: TTable
    Exclusive = True
    Left = 320
    Top = 144
  end
  object prReport1: TprReport
    Values = <>
    Variables = <>
    PrinterName = 'Epson LX-1050+'
    OnFirstPassObject = prReport1FirstPassObject
    PreviewParams.Options = []
    PreviewParams.ShowToolbars = [prptPreviewCommon]
    Left = 288
    Top = 144
    SystemInfo = (
      'OS: WIN32_NT 5.1.2600 '
      ''
      'PageSize: 4096'
      'ActiveProcessorMask: $1000'
      'NumberOfProcessors: 1'
      'ProcessorType: 586'
      ''
      'Compiler version: Delphi7'
      'PReport version: 1.9.5')
    LOGPIXELSX = 96
    LOGPIXELSY = 96
    object prPage1: TprPage
      Width = 2100
      Height = 2970
      PaperSize = 9
      Orientation = poPortrait
      lMargin = 10.000000000000000000
      rMargin = 10.000000000000000000
      tMargin = 10.000000000000000000
      bMargin = 10.000000000000000000
      object prHTitleBand1: TprHTitleBand
        Height = 43
        UseVerticalBands = False
        object prMemoObj1: TprMemoObj
          dRec.DefVersion = 0
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                'List of images')
              lBorder.Show = True
              lBorder.Style = psSolid
              lBorder.Color = clBlack
              lBorder.Width = 1
              rBorder.Show = True
              rBorder.Style = psSolid
              rBorder.Color = clBlack
              rBorder.Width = 1
              tBorder.Show = True
              tBorder.Style = psSolid
              tBorder.Color = clBlack
              tBorder.Width = 1
              bBorder.Show = True
              bBorder.Style = psSolid
              bBorder.Color = clBlack
              bBorder.Width = 1
              FillColor = 12516601
              hAlign = prhCenter
              vAlign = prvCenter
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -19
              Font.Name = 'Times New Roman'
              Font.Style = [fsBold]
              DeleteEmptyLinesAtEnd = False
              DeleteEmptyLines = False
              CanResizeX = False
              CanResizeY = False
              WordWrap = False
              FontSize = 14
            end>
          dRec.Left = 130
          dRec.Top = 2
          dRec.Right = 587
          dRec.Bottom = 35
          Visible = False
        end
      end
      object prHDetailBand1: TprHDetailBand
        Height = 168
        UseVerticalBands = False
        DataSetName = 'Table'
        ColCount = 0
        ColDirection = prcdTopBottomLeftRight
        object prImageObj1: TprImageObj
          dRec.DefVersion = 0
          dRec.Versions = <
            item
              Visible = True
              ImageSource = isPicture
              DrawMode = prdmStretchProp
              FillColor = clNone
            end>
          dRec.Left = 10
          dRec.Top = 7
          dRec.Right = 179
          dRec.Bottom = 144
          Visible = False
        end
        object prMemoObj2: TprMemoObj
          dRec.DefVersion = 0
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                'File name:')
              lBorder.Show = True
              lBorder.Style = psSolid
              lBorder.Color = clBlack
              lBorder.Width = 1
              rBorder.Show = True
              rBorder.Style = psSolid
              rBorder.Color = clBlack
              rBorder.Width = 1
              tBorder.Show = True
              tBorder.Style = psSolid
              tBorder.Color = clBlack
              tBorder.Width = 1
              bBorder.Show = True
              bBorder.Style = psSolid
              bBorder.Color = clBlack
              bBorder.Width = 1
              FillColor = clWhite
              hAlign = prhLeft
              vAlign = prvTop
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              DeleteEmptyLinesAtEnd = False
              DeleteEmptyLines = False
              CanResizeX = False
              CanResizeY = False
              WordWrap = False
              FontSize = 12
            end>
          dRec.Left = 194
          dRec.Top = 7
          dRec.Right = 715
          dRec.Bottom = 32
          Visible = False
        end
        object prMemoObj3: TprMemoObj
          dRec.DefVersion = 0
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                '[Table.FileName]')
              lBorder.Show = True
              lBorder.Style = psSolid
              lBorder.Color = clBlack
              lBorder.Width = 1
              rBorder.Show = True
              rBorder.Style = psSolid
              rBorder.Color = clBlack
              rBorder.Width = 1
              tBorder.Show = True
              tBorder.Style = psSolid
              tBorder.Color = clBlack
              tBorder.Width = 1
              bBorder.Show = True
              bBorder.Style = psSolid
              bBorder.Color = clBlack
              bBorder.Width = 1
              FillColor = clWhite
              hAlign = prhLeft
              vAlign = prvTop
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial'
              Font.Style = []
              DeleteEmptyLinesAtEnd = False
              DeleteEmptyLines = False
              CanResizeX = False
              CanResizeY = True
              WordWrap = True
              FontSize = 10
            end>
          dRec.Left = 194
          dRec.Top = 39
          dRec.Right = 715
          dRec.Bottom = 144
          Visible = False
        end
        object prShapeObj1: TprShapeObj
          dRec.DefVersion = 0
          dRec.Versions = <
            item
              Visible = True
              PenWidth = 2
            end>
          dRec.Left = 2
          dRec.Top = 151
          dRec.Right = 715
          dRec.Bottom = 160
          Visible = False
        end
      end
    end
  end
end
