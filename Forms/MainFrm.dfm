object MainForm: TMainForm
  Left = 238
  Top = 81
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'X'#178'Software XML Data Binding Wizard for Delphi'
  ClientHeight = 244
  ClientWidth = 438
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    438
    244)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSchema: TLabel
    Left = 8
    Top = 11
    Width = 58
    Height = 13
    Caption = 'Schema file:'
  end
  object feSchema: TJvFilenameEdit
    Left = 99
    Top = 8
    Width = 331
    Height = 21
    AddQuotes = False
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'F:\XTxXSD\TelefoonGegevens.xsd'
  end
  object gbOutput: TGroupBox
    Left = 8
    Top = 43
    Width = 422
    Height = 162
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Output '
    TabOrder = 1
    DesignSize = (
      422
      162)
    object rbFile: TRadioButton
      Left = 7
      Top = 21
      Width = 126
      Height = 17
      Caption = 'Output to &single file'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbFolder: TRadioButton
      Left = 8
      Top = 44
      Width = 149
      Height = 17
      Caption = 'Output to separate &files'
      Enabled = False
      TabOrder = 1
    end
    object plOutput: TJvPageList
      Left = 3
      Top = 72
      Width = 416
      Height = 87
      ActivePage = spFile
      PropagateEnable = False
      ShowDesignCaption = sdcBottomRight
      Anchors = [akLeft, akTop, akRight, akBottom]
      object spFile: TJvStandardPage
        Left = 0
        Top = 0
        Width = 416
        Height = 87
        object lblFile: TLabel
          Left = 8
          Top = 7
          Width = 55
          Height = 13
          Caption = 'Output file:'
        end
        object feFile: TJvFilenameEdit
          Left = 88
          Top = 3
          Width = 321
          Height = 21
          AddQuotes = False
          TabOrder = 0
          Text = 'F:\XTxXSD\Output\xml_Offerte.pas'
        end
      end
      object spFolder: TJvStandardPage
        Left = 0
        Top = 0
        Width = 416
        Height = 87
        DesignSize = (
          416
          87)
        object lblFolder: TLabel
          Left = 8
          Top = 7
          Width = 69
          Height = 13
          Caption = 'Output folder:'
        end
        object lblFolderPrefix: TLabel
          Left = 8
          Top = 34
          Width = 51
          Height = 13
          Caption = 'File prefix:'
        end
        object lblFolderPostfix: TLabel
          Left = 8
          Top = 60
          Width = 56
          Height = 13
          Caption = 'File postfix:'
        end
        object deFolder: TJvDirectoryEdit
          Left = 88
          Top = 3
          Width = 321
          Height = 21
          DialogKind = dkWin32
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'F:\XTxXSD\Output\'
        end
        object edtFolderPrefix: TEdit
          Left = 88
          Top = 30
          Width = 121
          Height = 21
          TabOrder = 1
          Text = 'xml_'
        end
        object edtFolderPostfix: TEdit
          Left = 88
          Top = 57
          Width = 121
          Height = 21
          TabOrder = 2
        end
      end
    end
  end
  object btnGenerate: TButton
    Left = 274
    Top = 211
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Generate'
    Default = True
    TabOrder = 2
    OnClick = btnGenerateClick
  end
  object btnClose: TButton
    Left = 355
    Top = 211
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Close'
    TabOrder = 3
    OnClick = btnCloseClick
  end
end
