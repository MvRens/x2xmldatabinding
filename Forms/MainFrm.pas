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
  end;


implementation
uses
  SysUtils,
  Windows,

  DelphiXMLDataBindingGenerator,
  XMLDataBindingGenerator;


{$R *.dfm}


{ TMainForm }
procedure TMainForm.FormCreate(Sender: TObject);
begin
  plOutput.ActivePageIndex := 0;
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
    OutputType  := otSingle;
    OutputPath  := feFile.FileName;

    Execute(feSchema.FileName);
  finally
    Free();
  end;
end;


procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close();
end;

end.
