program X2XMLDataBindingCmdLine;

uses
  ActiveX,
  SysUtils,
  DelphiXMLDataBindingGenerator in 'Units\DelphiXMLDataBindingGenerator.pas',
  XMLDataBindingGenerator in 'Units\XMLDataBindingGenerator.pas',
  XMLDataBindingHelpers in 'Units\XMLDataBindingHelpers.pas',
  DelphiXMLDataBindingResources in 'Units\DelphiXMLDataBindingResources.pas';

begin
  CoInitialize(nil);

  with TDelphiXMLDataBindingGenerator.Create() do
  try
    OutputType  := otSingle;
    OutputPath  := ParamStr(2);

    Execute(ParamStr(1));
  finally
    Free();
  end;
end.

