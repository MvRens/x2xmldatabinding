program X2XMLDataBinding;

{$WARN SYMBOL_PLATFORM OFF}

uses
  Forms,
  MainFrm in 'Forms\MainFrm.pas' {MainForm},
  XMLDataBindingGenerator in 'Units\XMLDataBindingGenerator.pas',
  DelphiXMLDataBindingGenerator in 'Units\DelphiXMLDataBindingGenerator.pas',
  XMLDataBindingHelpers in 'Units\XMLDataBindingHelpers.pas',
  DelphiXMLDataBindingResources in 'Units\DelphiXMLDataBindingResources.pas',
  DataBindingSettingsXML in 'Units\DataBindingSettingsXML.pas',
  DataBindingHintsXML in 'Units\DataBindingHintsXML.pas',
  MSXML2_TLB in 'Units\MSXML2_TLB.pas',
  XMLDataBindingUtils in 'Units\XMLDataBindingUtils.pas',
  X2UtNamedFormat in 'Units\X2UtNamedFormat.pas',
  X2UtTempFile in 'Units\X2UtTempFile.pas';

{$R *.res}

var
  MainForm: TMainForm;

begin
  {$IFDEF VER180}
  ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
  {$ENDIF}

  Application.Initialize;
  Application.Title := 'X²Software XML Data Binding for Delphi';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
