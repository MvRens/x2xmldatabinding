program X2XMLDataBinding;

{$WARN SYMBOL_PLATFORM OFF}

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  MainFrm in 'Forms\MainFrm.pas' {MainForm},
  XMLDataBindingGenerator in 'Units\XMLDataBindingGenerator.pas',
  DelphiXMLDataBindingGenerator in 'Units\DelphiXMLDataBindingGenerator.pas',
  XMLDataBindingHelpers in 'Units\XMLDataBindingHelpers.pas';

{$R *.res}

var
  MainForm: TMainForm;

begin
  {$IFDEF VER180}
  ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
  {$ENDIF}

  Application.Initialize;
  Application.Title := 'X²Software XML Data Binding Wizard for Delphi';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
