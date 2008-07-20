program X2XMLDataBindingTests;

{$APPTYPE CONSOLE}
uses
  ActiveX,
  GUITestRunner,
  ObjectMappingTests in 'Source\ObjectMappingTests.pas',
  DataBindingResultXML in 'Source\DataBindingResultXML.pas',
  XMLDataBindingGenerator in '..\Units\XMLDataBindingGenerator.pas',
  XMLDataBindingHelpers in '..\Units\XMLDataBindingHelpers.pas',
  XMLDataBindingUtilsTest in 'Source\XMLDataBindingUtilsTest.pas',
  XMLDataBindingUtils in '..\Shared\XMLDataBindingUtils.pas';

begin
  CoInitialize(nil);
  RunRegisteredTests;
end.

