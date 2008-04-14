unit MainFrm;

interface
uses
  Classes,
  Controls,
  Forms,
  Mask,
  StdCtrls,

  JvComponent,
  JvExControls,
  JvExMask,
  JvPageList,
  JvToolEdit;


type
  TMainForm = class(TForm)
    btnClose:                                   TButton;
    btnGenerate:                                TButton;
    deFolder:                                   TJvDirectoryEdit;
    edtFolderPostfix:                           TEdit;
    edtFolderPrefix:                            TEdit;
    feFile:                                     TJvFilenameEdit;
    feSchema:                                   TJvFilenameEdit;
    gbOutput:                                   TGroupBox;
    lblFile:                                    TLabel;
    lblFolder:                                  TLabel;
    lblFolderPostfix:                           TLabel;
    lblFolderPrefix:                            TLabel;
    lblSchema:                                  TLabel;
    plOutput:                                   TJvPageList;
    rbFile:                                     TRadioButton;
    rbFolder:                                   TRadioButton;
    spFile:                                     TJvStandardPage;
    spFolder:                                   TJvStandardPage;

    procedure btnCloseClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OutputTypeClick(Sender: TObject);
  private
    procedure GetFileName(Sender: TObject; const SchemaName: String; var Path, FileName: String);
  end;


implementation
uses
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
  if not FileExists(feSchema.FileName) then
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
      OutputPath  := feFile.FileName;
    end else if rbFolder.Checked then
    begin
      OutputType  := otMultiple;
      OutputPath  := deFolder.Text;
    end;

    OnGetFileName := GetFileName;
    Execute(feSchema.FileName);
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
  FileName  := edtFolderPrefix.Text + FileName + edtFolderPostfix.Text;
  CheckValidFileName(FileName);
end;

end.
