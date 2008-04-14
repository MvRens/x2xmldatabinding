program X2XMLDataBindingCmdLine;

uses
  ActiveX,
  SysUtils,
  DelphiXMLDataBindingGenerator in 'Units\DelphiXMLDataBindingGenerator.pas',
  XMLDataBindingGenerator in 'Units\XMLDataBindingGenerator.pas',
  XMLDataBindingHelpers in 'Units\XMLDataBindingHelpers.pas',
  DelphiXMLDataBindingResources in 'Units\DelphiXMLDataBindingResources.pas',
  xml_ExternalLeadFeed in '..\xml_ExternalLeadFeed.pas',
  xml_Offerte in '..\..\xtx\xtx\xsd\xml_Offerte.pas';


begin
  CoInitialize(nil);

  with TDelphiXMLDataBindingGenerator.Create() do
  try
    OutputType  := otSingle;
    OutputPath  := ParamStr(2);

    if DirectoryExists(OutputPath) then
      OutputType  := otMultiple
    else
      OutputType  := otSingle;

    Execute(ParamStr(1));
  finally
    Free();
  end;
end.

