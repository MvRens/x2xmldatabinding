program X2XMLDataBindingTests;

uses
  ActiveX,
  GUITestRunner,
  ObjectMappingTests in 'Source\ObjectMappingTests.pas',
  DataBindingResultXML in 'Source\DataBindingResultXML.pas',
  XMLDataBindingGenerator in '..\Units\XMLDataBindingGenerator.pas',
  XMLDataBindingHelpers in '..\Units\XMLDataBindingHelpers.pas';

begin
  CoInitialize(nil);
  RunRegisteredTests;
end.

