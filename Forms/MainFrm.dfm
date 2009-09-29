object MainForm: TMainForm
  Left = 245
  Top = 81
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsDialog
  Caption = 'X'#178'Software XML Data Binding for Delphi'
  ClientHeight = 254
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
    254)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSchema: TLabel
    Left = 8
    Top = 11
    Width = 58
    Height = 13
    Caption = 'Schema file:'
  end
  object feSchema: TcxButtonEdit
    Left = 99
    Top = 8
    Anchors = [akLeft, akTop, akRight]
    Properties.Buttons = <
      item
        Kind = bkEllipsis
      end>
    Properties.OnButtonClick = feSchemaPropertiesButtonClick
    Properties.OnChange = feSchemaPropertiesChange
    TabOrder = 0
    Width = 331
  end
  object gbOutput: TGroupBox
    Left = 8
    Top = 43
    Width = 422
    Height = 167
    Anchors = [akLeft, akTop, akBottom]
    Caption = ' Output '
    TabOrder = 1
    DesignSize = (
      422
      167)
    object rbFile: TRadioButton
      Left = 7
      Top = 21
      Width = 126
      Height = 17
      Caption = 'Output to &single file'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = OutputTypeClick
    end
    object rbFolder: TRadioButton
      Left = 8
      Top = 44
      Width = 149
      Height = 17
      Caption = 'Output to separate &files'
      TabOrder = 1
      OnClick = OutputTypeClick
    end
    object plOutput: TPageControl
      Left = 3
      Top = 68
      Width = 416
      Height = 95
      ActivePage = spFile
      Anchors = [akLeft, akTop, akRight, akBottom]
      Style = tsButtons
      TabOrder = 2
      object spFile: TTabSheet
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblFile: TLabel
          Left = 8
          Top = 7
          Width = 55
          Height = 13
          Caption = 'Output file:'
        end
        object feFile: TcxButtonEdit
          Left = 88
          Top = 3
          Properties.Buttons = <
            item
              Kind = bkEllipsis
            end>
          Properties.OnButtonClick = feFilePropertiesButtonClick
          TabOrder = 0
          Width = 317
        end
      end
      object spFolder: TTabSheet
        TabVisible = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          408
          85)
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
        object deFolder: TcxButtonEdit
          Left = 88
          Top = 3
          Anchors = [akLeft, akTop, akRight]
          Properties.Buttons = <
            item
              Kind = bkEllipsis
            end>
          Properties.OnButtonClick = deFolderPropertiesButtonClick
          TabOrder = 0
          Width = 317
        end
        object edtFolderPrefix: TcxTextEdit
          Left = 88
          Top = 30
          TabOrder = 1
          Text = 'xml_'
          Width = 121
        end
        object edtFolderPostfix: TcxTextEdit
          Left = 88
          Top = 57
          TabOrder = 2
          Width = 121
        end
      end
    end
  end
  object btnGenerate: TButton
    Left = 274
    Top = 221
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Generate'
    Default = True
    TabOrder = 3
    OnClick = btnGenerateClick
  end
  object btnClose: TButton
    Left = 355
    Top = 221
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Close'
    TabOrder = 4
    OnClick = btnCloseClick
  end
  object btnHints: TButton
    Left = 7
    Top = 221
    Width = 142
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Generate blank &Hints file'
    TabOrder = 2
    OnClick = btnHintsClick
  end
  object DefaultEditStyle: TcxDefaultEditStyleController
    Style.HotTrack = False
    Left = 264
    Top = 60
  end
  object LookAndFeel: TcxLookAndFeelController
    Kind = lfFlat
    NativeStyle = True
    Left = 368
    Top = 60
  end
  object dlgSchema: TOpenDialog
    Filter = 'W3C XML Schema files (*.xsd)|*.xsd|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 260
    Top = 156
  end
  object dlgOutputFile: TSaveDialog
    Filter = 'Delphi source files (*.pas)|*.pas|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 343
    Top = 157
  end
end
