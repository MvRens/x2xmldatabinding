unit MainFrm;

{$WARN UNIT_PLATFORM OFF}

interface
uses
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  Forms,
  Mask,
  StdCtrls,

  cxButtonEdit,
  cxContainer,
  cxControls,
  cxEdit,
  cxLookAndFeels,
  cxMaskEdit,
  cxTextEdit;


type
  TMainForm = class(TForm)
    btnClose:                                   TButton;
    btnGenerate:                                TButton;
    DefaultEditStyle:                           TcxDefaultEditStyleController;
    deFolder:                                   TcxButtonEdit;
    dlgOutputFile:                              TSaveDialog;
    dlgSchema:                                  TOpenDialog;
    edtFolderPostfix:                           TcxTextEdit;
    edtFolderPrefix:                            TcxTextEdit;
    feFile:                                     TcxButtonEdit;
    feSchema:                                   TcxButtonEdit;
    gbOutput:                                   TGroupBox;
    lblFile:                                    TLabel;
    lblFolder:                                  TLabel;
    lblFolderPostfix:                           TLabel;
    lblFolderPrefix:                            TLabel;
    lblSchema:                                  TLabel;
    LookAndFeel:                                TcxLookAndFeelController;
    plOutput:                                   TPageControl;
    rbFile:                                     TRadioButton;
    rbFolder:                                   TRadioButton;
    spFile:                                     TTabSheet;
    spFolder:                                   TTabSheet;

    procedure btnCloseClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OutputTypeClick(Sender: TObject);
    procedure feFilePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure deFolderPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure feSchemaPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
  private
    procedure GetFileName(Sender: TObject; const SchemaName: String; var Path, FileName: String);
  end;


implementation
uses
  FileCtrl,
  SysUtils,
  Windows,

  X2UtTempFile,

  DelphiXMLDataBindingGenerator,
  XMLDataBindingGenerator;


{$R *.dfm}


{ TMainForm }
procedure TMainForm.FormCreate(Sender: TObject);
var
  schemaFile:   String;

begin
  plOutput.ActivePageIndex := 0;

  if ParamCount() > 0 then
  begin
    schemaFile  := ParamStr(1);

    if FileExists(schemaFile) then
    begin
      feSchema.Text   := schemaFile;
      feFile.Text     := ChangeFileExt(schemaFile, '.pas');
      deFolder.Text   := ExtractFilePath(schemaFile);
    end;
  end;
end;


procedure TMainForm.OutputTypeClick(Sender: TObject);
begin
  if Sender = rbFile then
    plOutput.ActivePage := spFile
  else if Sender = rbFolder then
    plOutput.ActivePage := spFolder;
end;


procedure TMainForm.btnGenerateClick(Sender: TObject);
begin
  if not FileExists(feSchema.Text) then
  begin
    MessageBox(Self.Handle, 'Please specify a valid schema file.',
               'Schema file does not exist', MB_OK or MB_ICONERROR);

    ActiveControl := feFile;
    Exit;
  end;

  with TDelphiXMLDataBindingGenerator.Create() do
  try
    if rbFile.Checked then
    begin
      OutputType  := otSingle;
      OutputPath  := feFile.Text;
    end else if rbFolder.Checked then
    begin
      OutputType  := otMultiple;
      OutputPath  := deFolder.Text;
    end;

    OnGetFileName := GetFileName;
    Execute(feSchema.Text);

    ShowMessage('The data binding has been generated.');
  finally
    Free();
  end;
end;


procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close();
end;


procedure TMainForm.GetFileName(Sender: TObject; const SchemaName: String; var Path, FileName: String);
begin
  FileName  := ChangeFileExt(edtFolderPrefix.Text + FileName,
                             edtFolderPostfix.Text + ExtractFileExt(FileName));
  CheckValidFileName(FileName);
end;


procedure TMainForm.feFilePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if dlgOutputFile.Execute() then
    feFile.Text := dlgOutputFile.FileName;
end;


procedure TMainForm.deFolderPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  directory:  String;

begin
  if SelectDirectory('Select output folder', '', directory) then
    deFolder.Text := directory;
end;


procedure TMainForm.feSchemaPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if dlgSchema.Execute() then
    feSchema.Text := dlgSchema.FileName;
end;

end.
