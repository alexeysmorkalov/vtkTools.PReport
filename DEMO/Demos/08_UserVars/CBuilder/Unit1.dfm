object Form1: TForm1
  Left = 192
  Top = 107
  Width = 376
  Height = 126
  Caption = '8 - User vars'
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
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 90
    Height = 13
    Caption = 'Company contains:'
  end
  object Label2: TLabel
    Left = 55
    Top = 36
    Width = 43
    Height = 13
    Caption = 'Order by:'
  end
  object Button1: TButton
    Left = 16
    Top = 64
    Width = 257
    Height = 25
    Caption = 'Generate report'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 104
    Top = 8
    Width = 153
    Height = 21
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 264
    Top = 10
    Width = 89
    Height = 17
    Caption = 'All customers'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object ComboBox1: TComboBox
    Left = 104
    Top = 32
    Width = 153
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    Items.Strings = (
      'Company'
      'Country')
  end
  object prReport1: TprReport
    Title = '8 - User vars'
    ExportFromPage = 0
    ExportToPage = 0
    Values = <>
    Variables = <>
    PrinterName = 'Virtual printer'
    OnUnknownVariable = prReport1UnknownVariable
    OnUnknownObjFunction = prReport1UnknownObjFunction
    Left = 312
    Top = 56
    SystemInfo = (
      'OS: WIN32_NT 5.1.2600 Service Pack 1'
      ''
      'PageSize: 4096'
      'ActiveProcessorMask: $1000'
      'NumberOfProcessors: 1'
      'ProcessorType: 586'
      ''
      'Compiler version: Builder5'
      'PReport version: 1.9.7')
    LOGPIXELSX = 96
    LOGPIXELSY = 96
    object prPage1: TprPage
      Width = 2101
      Height = 2969
      PaperSize = 9
      Orientation = poPortrait
      lMargin = 7
      rMargin = 7
      tMargin = 5
      bMargin = 5
      object prHTitleBand1: TprHTitleBand
        Height = 81
        object prMemoObj1: TprMemoObj
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                'List of customers')
              lBorder.Show = True
              lBorder.Width = 1
              rBorder.Show = True
              rBorder.Width = 1
              tBorder.Show = True
              tBorder.Width = 1
              bBorder.Show = True
              bBorder.Width = 1
              FillColor = clWhite
              vAlign = prvTop
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial Cyr'
              Font.Style = []
              FontSize = 10
            end>
          dRec.Left = 220
          dRec.Top = 7
          dRec.Right = 428
          dRec.Bottom = 28
        end
        object prMemoObj4: TprMemoObj
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                '[FindValid]')
              lBorder.Width = 1
              rBorder.Width = 1
              tBorder.Width = 1
              bBorder.Width = 1
              FillColor = 14933198
              vAlign = prvTop
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial Cyr'
              Font.Style = []
              CanResizeX = True
              FontSize = 10
            end>
          dRec.Left = 7
          dRec.Top = 33
          dRec.Right = 726
          dRec.Bottom = 52
        end
        object prMemoObj5: TprMemoObj
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                'Order by [Order]')
              lBorder.Width = 1
              rBorder.Width = 1
              tBorder.Width = 1
              bBorder.Width = 1
              FillColor = 14933198
              vAlign = prvTop
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial Cyr'
              Font.Style = []
              CanResizeX = True
              FontSize = 10
            end>
          dRec.Left = 7
          dRec.Top = 58
          dRec.Right = 726
          dRec.Bottom = 77
        end
      end
      object prHDetailBand1: TprHDetailBand
        Height = 68
        DataSetName = 'Query1'
        ColCount = 1
        object prMemoObj2: TprMemoObj
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                '[Query1.CustNo]')
              lBorder.Show = True
              lBorder.Width = 1
              rBorder.Show = True
              rBorder.Width = 1
              tBorder.Show = True
              tBorder.Width = 1
              bBorder.Show = True
              bBorder.Width = 1
              FillColor = clWhite
              vAlign = prvTop
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial Cyr'
              Font.Style = []
              FontSize = 10
            end>
          dRec.Left = 8
          dRec.Top = 4
          dRec.Right = 78
          dRec.Bottom = 24
        end
        object prMemoObj3: TprMemoObj
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                '[Query1.Company] (Country - [Query1.Country])')
              lBorder.Show = True
              lBorder.Width = 1
              rBorder.Show = True
              rBorder.Width = 1
              tBorder.Show = True
              tBorder.Width = 1
              bBorder.Show = True
              bBorder.Width = 1
              FillColor = clWhite
              vAlign = prvTop
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial Cyr'
              Font.Style = []
              FontSize = 10
            end>
          dRec.Left = 98
          dRec.Top = 4
          dRec.Right = 450
          dRec.Bottom = 24
        end
        object prMemoObj7: TprMemoObj
          dRec.Versions = <
            item
              Visible = True
              Memo.Strings = (
                'Length of Company : [Query1.FieldLen("Company")]'
                'First char of Company: [Query1.FieldSubString("Company", 1, 1)]')
              lBorder.Show = True
              lBorder.Width = 1
              rBorder.Show = True
              rBorder.Width = 1
              tBorder.Show = True
              tBorder.Width = 1
              bBorder.Show = True
              bBorder.Width = 1
              FillColor = 14933198
              vAlign = prvTop
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'Arial Cyr'
              Font.Style = []
              CanResizeY = True
              FontSize = 10
            end>
          dRec.Left = 457
          dRec.Top = 4
          dRec.Right = 736
          dRec.Bottom = 48
        end
      end
    end
  end
  object Query1: TQuery
    DatabaseName = 'DBDEMOS'
    Left = 280
    Top = 56
  end
end
