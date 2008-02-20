unit DelphiXMLDataBindingGenerator;

interface
uses
  Classes,
  XMLSchema,

  XMLDataBindingGenerator,
  XMLDataBindingHelpers;

type
  TGetFileNameEvent = procedure(Sender: TObject; const SchemaName: String; var Result: String) of object;

  TDelphiXMLDataBindingGenerator = class(TXMLDataBindingGenerator)
  private
    FOnGetFileName: TGetFileNameEvent;
  protected
    procedure GenerateDataBinding(); override;
    procedure GenerateSingleDataBinding();
    procedure GenerateMultipleDataBinding();

    function DoGetFileName(const ASchemaName: String): String;

    procedure WriteUnitHeader(AStream: TStreamHelper; const AFileName: String);
    procedure WriteSchemaInterfaces(AStream: TStreamHelper; ASchemaDef: IXMLSchemaDef; AForward: Boolean);
    procedure WriteElementInterface(AStream: TStreamHelper; AElement: IXMLElementDef; AForward: Boolean);
    procedure WriteElements(AStream: TStreamHelper; AType: IXMLTypeDef);
    procedure WriteImplementation(AStream: TStreamHelper);
    procedure WriteUnitFooter(AStream: TStreamHelper);
  public
    property OnGetFileName:   TGetFileNameEvent read FOnGetFileName write FOnGetFileName;
  end;

implementation
uses
  SysUtils;


{ TDelphiXMLDataBindingGenerator }
procedure TDelphiXMLDataBindingGenerator.GenerateDataBinding();
begin
  case OutputType of
    otSingle:     GenerateSingleDataBinding();
    otMultiple:   GenerateMultipleDataBinding();
  end;
end;


procedure TDelphiXMLDataBindingGenerator.GenerateSingleDataBinding();
var
  unitName:     String;
  unitStream:   TStreamHelper;
  schemaIndex:  Integer;

begin
  unitName    := DoGetFileName(Schema[0].SchemaName);
  unitStream  := TStreamHelper.Create(TFileStream.Create(unitName, fmCreate), soOwned);
  try
    WriteUnitHeader(unitStream, unitName);
    unitStream.WriteLn('type');

    for schemaIndex := 0 to Pred(SchemaCount) do
    begin
      unitStream.WriteLn('  { Forward declarations for ' + Schema[schemaIndex].SchemaName + ' }');
      WriteSchemaInterfaces(unitStream, Schema[schemaIndex].SchemaDef, True);
    end;

    for schemaIndex := 0 to Pred(SchemaCount) do
    begin
      unitStream.WriteLn('  { Interfaces for ' + Schema[schemaIndex].SchemaName + ' }');
      WriteSchemaInterfaces(unitStream, Schema[schemaIndex].SchemaDef, False);
    end;

    WriteImplementation(unitStream);
    WriteUnitFooter(unitStream);
  finally
    FreeAndNil(unitStream);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.GenerateMultipleDataBinding();
begin
end;


procedure TDelphiXMLDataBindingGenerator.WriteUnitHeader(AStream: TStreamHelper; const AFileName: String);
begin
  // #ToDo1 (MvR) 14-4-2007: if outputtype = multiple, use include files

  AStream.WriteLn('{');
  AStream.WriteLn('  X2Software XML Data Binding Wizard');
  AStream.WriteLn('    Generated from: ' + SourceFileName);
  AStream.WriteLn('}');
  AStream.WriteLn('unit ' + ChangeFileExt(ExtractFileName(AFileName), '') + ';');
  AStream.WriteLn();
  AStream.WriteLn('interface');
  AStream.WriteLn('uses');
  AStream.WriteLn('  XMLDoc,');
  AStream.WriteLn('  XMLIntf;');
  AStream.WriteLn();
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaInterfaces(AStream: TStreamHelper; ASchemaDef: IXMLSchemaDef; AForward: Boolean);

  procedure ProcessElementDefs(AElements: IXMLElementDefList);
  var
    elementIndex:         Integer;
    element:              IXMLElementDef;

  begin
    for elementIndex := 0 to Pred(AElements.Count) do
    begin
      element := AElements[elementIndex];

      if element.DataType.IsComplex then
      begin
        WriteElementInterface(AStream, element, AForward);
        ProcessElementDefs(element.ChildElements);
      end;
    end;
  end;

var
  elementIndex:         Integer;
  element:              IXMLElementDef;
  complexTypeIndex:     Integer;
  complexType:          IXMLComplexTypeDef;

begin
  for elementIndex := 0 to Pred(ASchemaDef.ElementDefs.Count) do
  begin
    element := ASchemaDef.ElementDefs[elementIndex];

    WriteElementInterface(AStream, element, AForward);
    if element.DataType.IsComplex then
    begin
      ProcessElementDefs(element.ChildElements);
    end;
  end;

  for complexTypeIndex := 0 to Pred(ASchemaDef.ComplexTypes.Count) do
  begin
    complexType := ASchemaDef.ComplexTypes[complexTypeIndex];

    if AForward then
    begin
      AStream.WriteLn('  IXML' + complexType.Name + ' = interface; { ComplexType }');
    end else
    begin
      AStream.WriteLn('  IXML' + complexType.Name + ' = interface');
      AStream.WriteLn('    {TODO:GUID}');

      WriteElements(complexType);

      AStream.WriteLn('  end;');
      AStream.WriteLn();
    end;

    ProcessElementDefs(complexType.ElementDefList);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteElementInterface(AStream: TStreamHelper; AElement: IXMLElementDef; AForward: Boolean);
begin
  if AForward then
  begin
    AStream.WriteLn('  IXML' + AElement.Name + ' = interface; { ElementDef }');
  end else
  begin
    // #ToDo1 (MvR) 14-4-2007: output element interface
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteImplementation(AStream: TStreamHelper);
begin
end;


procedure TDelphiXMLDataBindingGenerator.WriteUnitFooter(AStream: TStreamHelper);
begin
  AStream.WriteLn();
  AStream.WriteLn('end.');
end;


function TDelphiXMLDataBindingGenerator.DoGetFileName(const ASchemaName: String): String;
begin
  Result  := OutputPath;

  if OutputType = otMultiple then
  begin
    Result := IncludeTrailingPathDelimiter(Result) + ASchemaName + '.pas';
    if Assigned(FOnGetFileName) then
      FOnGetFileName(Self, ASchemaName, Result);
  end;
end;

end.
