unit DelphiXMLDataBindingGenerator;

interface
uses
  Classes,
  XMLSchema,

  XMLDataBindingGenerator,
  XMLDataBindingHelpers;

type
  TGetFileNameEvent = procedure(Sender: TObject; const SchemaName: String; var Result: String) of object;

  TDelphiXMLSection = (dxsForward, dxsInterface, dxsClass, dxsImplementation);
  TDelphiXMLMember = (dxmPropertyGet, dxmPropertySet, dxmPropertyDeclaration);

  TDelphiXMLDataBindingGenerator = class(TXMLDataBindingGenerator)
  private
    FOnGetFileName: TGetFileNameEvent;
  protected
    procedure GenerateDataBinding(); override;
    procedure GenerateSingleDataBinding();
    procedure GenerateMultipleDataBinding();

    function DoGetFileName(const ASchemaName: String): String;

    function TranslateDataType(ADataType: IXMLTypeDef): String;
    function CreateNewGUID(): String;

    procedure WriteUnitHeader(AStream: TStreamHelper; const AFileName: String);
    procedure WriteInterface(AStream: TStreamHelper; ASchemaDef: IXMLSchemaDef; ASection: TDelphiXMLSection);

    procedure WriteComplexElementInterface(AStream: TStreamHelper; AElement: IXMLElementDef; ASection: TDelphiXMLSection);
    function WriteSimpleElementInterface(AStream: TStreamHelper; AElement: IXMLElementDef; AMember: TDelphiXMLMember): Boolean;
    procedure WriteEnumeration(AStream: TStreamHelper; AElement: IXMLElementDef);

    procedure WriteElements(AStream: TStreamHelper; AElements: IXMLElementDefs); overload;
    procedure WriteElements(AStream: TStreamHelper; AElements: IXMLElementDefList); overload;

    procedure WriteImplementation(AStream: TStreamHelper);
    procedure WriteUnitFooter(AStream: TStreamHelper);
  public
    property OnGetFileName:   TGetFileNameEvent read FOnGetFileName write FOnGetFileName;
  end;

implementation
uses
  SysUtils;


const
  SectionComments:  array[TDelphiXMLSection] of String =
                    (
                      '  { Forward declarations for %s }',
                      '  { Interfaces for %s }',
                      '  { Classes for %s }',
                      '{ Implementation for %s }'
                    );

  MemberPropertyGet       = '    function Get%0:s: %1:s;';
  MemberPropertySet       = '    procedure Set%0:s(const Value: %1:s);';
  MemberProperty          = '    property %0:s: %1:s read Get%0:s write Set%0:s;';
  MemberPropertyReadOnly  = '    property %0:s: %1:s read Get%0:s;';


  ReservedWords:  array[0..111] of String =
                  (
                    'absolute', 'abstract', 'and', 'array', 'as', 'asm',
                    'assembler', 'automated', 'begin', 'case', 'cdecl', 'class',
                    'const', 'constructor', 'contains', 'default', 'deprecated',
                    'destructor', 'dispid', 'dispinterface', 'div', 'do',
                    'downto', 'dynamic', 'else', 'end', 'except', 'export',
                    'exports', 'external', 'far', 'file', 'final', 'finalization',
                    'finally', 'for', 'forward', 'function', 'goto', 'if',
                    'implementation', 'implements', 'in', 'index', 'inherited',
                    'initialization', 'inline', 'interface', 'is', 'label',
                    'library', 'local', 'message', 'mod', 'name', 'near',
                    'nil', 'nodefault', 'not', 'object', 'of', 'or', 'out',
                    'overload', 'override', 'package', 'packed', 'pascal',
                    'platform', 'private', 'procedure', 'program', 'property',
                    'protected', 'public', 'published', 'raise', 'read',
                    'readonly', 'record', 'register', 'reintroduce', 'repeat',
                    'requires', 'resident', 'resourcestring', 'safecall',
                    'sealed', 'set', 'shl', 'shr', 'static', 'stdcall',
                    'stored', 'string', 'then', 'threadvar', 'to', 'try', 'type',
                    'unit', 'unsafe', 'until', 'uses', 'var', 'varargs',
                    'virtual', 'while', 'with', 'write', 'writeonly', 'xor'
                  );


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
  section:      TDelphiXMLSection;

begin
  unitName    := DoGetFileName(Schema[0].SchemaName);
  unitStream  := TStreamHelper.Create(TFileStream.Create(unitName, fmCreate), soOwned);
  try
    WriteUnitHeader(unitStream, unitName);
    unitStream.WriteLn('type');
    unitStream.WriteLn('  TXMLCollection = Variant;');
    unitStream.WriteLn();

    for section := dxsForward to dxsClass do
    begin
      for schemaIndex := 0 to Pred(SchemaCount) do
      begin
        unitStream.WriteLn(Format(SectionComments[section], [Schema[schemaIndex].SchemaName]));
        WriteInterface(unitStream, Schema[schemaIndex].SchemaDef, section);
        unitStream.WriteLn();
      end;
    end;

    unitStream.WriteLn();
    WriteImplementation(unitStream);

    for schemaIndex := 0 to Pred(SchemaCount) do
    begin
      unitStream.WriteLn(Format(SectionComments[dxsImplementation], [Schema[schemaIndex].SchemaName]));
      WriteInterface(unitStream, Schema[schemaIndex].SchemaDef, dxsImplementation);
      unitStream.WriteLn();
    end;

    WriteUnitFooter(unitStream);
  finally
    FreeAndNil(unitStream);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.GenerateMultipleDataBinding();
begin
end;


function TDelphiXMLDataBindingGenerator.TranslateDataType(ADataType: IXMLTypeDef): String;
begin
  Result  := 'Variant';

  // #ToDo1 (MvR) 26-2-2008: check type mapping
  if ADataType.IsComplex then
  begin
    Result  := 'IXML' + ADataType.Name;
  end else if ADataType.Enumerations.Count > 0 then
  begin
    Result  := 'TXML' + ADataType.Name;
  end else
  begin
    if ADataType.NamespaceURI = SXMLSchemaURI_2001 then
    begin
      if ADataType.Name = 'int' then
        Result  := 'Integer'
      else if ADataType.Name = 'float' then
        Result  := 'Double'
      else if ADataType.Name = 'boolean' then
        Result  := 'Boolean'
      else if ADataType.Name = 'string' then
        Result  := 'String';
    end;
  end;
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
  AStream.WriteLn('  XMLIntf,');
  AStream.WriteLn('  Variants;');
  AStream.WriteLn();
end;


procedure TDelphiXMLDataBindingGenerator.WriteInterface(AStream: TStreamHelper; ASchemaDef: IXMLSchemaDef; ASection: TDelphiXMLSection);

  procedure ProcessElementDefs(AElements: IXMLElementDefList);
  var
    elementIndex:         Integer;
    element:              IXMLElementDef;

  begin
    for elementIndex := 0 to Pred(AElements.Count) do
    begin
      element := AElements[elementIndex];

      if not Assigned(element.Ref) then
      begin
        if element.DataType.IsComplex and
           element.DataType.IsAnonymous then
        begin
          WriteComplexElementInterface(AStream, element, ASection);
          ProcessElementDefs(element.ChildElements);
        end;
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

    if element.DataType.Enumerations.Count > 0 then
    begin
      { Enumerated type }
      if ASection = dxsForward then
        WriteEnumeration(AStream, element);
    end else
    begin
      { Element }
      WriteComplexElementInterface(AStream, element, ASection);
      if element.DataType.IsComplex then
      begin
        ProcessElementDefs(element.ChildElements);
      end;
    end;
  end;

  for complexTypeIndex := 0 to Pred(ASchemaDef.ComplexTypes.Count) do
  begin
    complexType := ASchemaDef.ComplexTypes[complexTypeIndex];

    case ASection of
      dxsForward:
        begin
          AStream.WriteLn('  IXML' + complexType.Name + ' = interface;');
        end;
      dxsInterface:
        begin
          AStream.WriteLn('  IXML' + complexType.Name + ' = interface(IXMLNode)');
          AStream.WriteLn('    ' + CreateNewGUID());
          WriteElements(AStream, complexType.ElementDefs);
          AStream.WriteLn('  end;');
          AStream.WriteLn();
        end;
      dxsClass:
        begin
          AStream.WriteLn(Format('  TXML%0:s = class(TXMLNode, IXML%0:s)', [complexType.Name]));
          WriteElements(AStream, complexType.ElementDefs);
          AStream.WriteLn('  end;');
          AStream.WriteLn();
        end;
      dxsImplementation:
        begin

        end;
    end;

    ProcessElementDefs(complexType.ElementDefList);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteComplexElementInterface(AStream: TStreamHelper; AElement: IXMLElementDef; ASection: TDelphiXMLSection);
begin
  case ASection of
    dxsForward:
      begin
        AStream.WriteLn('  IXML' + AElement.Name + ' = interface;');
      end;
    dxsInterface:
      begin
        AStream.WriteLn('  IXML' + AElement.Name + ' = interface(IXMLNode)');
        AStream.WriteLn('    ' + CreateNewGUID());
        WriteElements(AStream, AElement.ChildElements);
        AStream.WriteLn('  end;');
        AStream.WriteLn();
      end;
    dxsClass:
      begin
        AStream.WriteLn(Format('  TXML%0:s = class(TXMLNode, IXML%0:s)', [AElement.Name]));
        WriteElements(AStream, AElement.ChildElements);
        AStream.WriteLn('  end;');
        AStream.WriteLn();
      end;
    dxsImplementation:
      begin
      end;
  end;
end;


function TDelphiXMLDataBindingGenerator.WriteSimpleElementInterface(AStream: TStreamHelper; AElement: IXMLElementDef; AMember: TDelphiXMLMember): Boolean;
var
  isReadOnly: Boolean;
  memberName: String;
  dataType: String;
  memberFormat: String;

begin
  Result  := False;
  isReadOnly := AElement.DataType.IsComplex;
  if isReadOnly and (AMember = dxmPropertySet) then
    exit;

  dataType := '';
    // #ToDo1 (MvR) 22-2-2008: escape reserved words
  memberName := AElement.Name;

  if (AElement.MaxOccurs = 'unbounded') or
     (AElement.MaxOccurs > 1) then
  begin
    { Collection }
    dataType := Format('IXML%sCollection', [AElement.Name]);
  end else
  begin
    dataType := TranslateDataType(AElement.DataType);
  end;


  case AMember of
    dxmPropertyGet:
      memberFormat  := MemberPropertyGet;
    dxmPropertySet:
      memberFormat  := MemberPropertySet;
    dxmPropertyDeclaration:
      if isReadOnly then
        memberFormat  := MemberPropertyReadOnly
      else
        memberFormat  := MemberProperty;
  end;

  AStream.Write(Format(memberFormat, [memberName, dataType]));

  if AElement.MinOccurs = 0 then
    { Optional }
    AStream.WriteLn(' { Optional }')
  else
    AStream.WriteLn();

  Result  := True;
end;


procedure TDelphiXMLDataBindingGenerator.WriteEnumeration(AStream: TStreamHelper; AElement: IXMLElementDef);
var
  enumerations: IXMLEnumerationCollection;
  enumIndex: Integer;
  enumStart: String;
  lineIndent: String;

begin
  enumerations := AElement.DataType.Enumerations;
  if enumerations.Count = 0 then
    exit;

  // #ToDo1 (MvR) 26-2-2008: unique prefix?
  enumStart := Format('  TXML%s = (', [AElement.Name]);
  AStream.Write(enumStart);
  lineIndent := StringOfChar(' ', Length(enumStart));

  for enumIndex := 0 to Pred(enumerations.Count) do
  begin
    if enumIndex > 0 then
      AStream.Write(lineIndent);

    AStream.Write(Format('%s_%s', [AElement.Name, enumerations[enumIndex].Value]));

    if enumIndex < Pred(enumerations.Count) then
      AStream.WriteLn(',')
    else
      AStream.WriteLn(');');
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteElements(AStream: TStreamHelper; AElements: IXMLElementDefList);
var
  elementIndex: Integer;
  member: TDelphiXMLMember;
  hasMembers: Boolean;

begin
  for member := Low(TDelphiXMLMember) to High(TDelphiXMLMember) do
  begin
    hasMembers := False;

    for elementIndex := 0 to Pred(AElements.Count) do
      if WriteSimpleElementInterface(AStream, AElements[elementIndex], member) then
        hasMembers := True;

    if hasMembers and (member < High(TDelphiXMLMember)) then
      AStream.WriteLn();
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteElements(AStream: TStreamHelper; AElements: IXMLElementDefs);
var
  elementIndex: Integer;
  member: TDelphiXMLMember;
  hasMembers: Boolean;

begin
  for member := Low(TDelphiXMLMember) to High(TDelphiXMLMember) do
  begin
    hasMembers := False;

    for elementIndex := 0 to Pred(AElements.Count) do
      if WriteSimpleElementInterface(AStream, AElements[elementIndex], member) then
        hasMembers := True;

    if hasMembers and (member < High(TDelphiXMLMember)) then
      AStream.WriteLn();
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteImplementation(AStream: TStreamHelper);
begin
  AStream.WriteLn('implementation');
  AStream.WriteLn();
end;


procedure TDelphiXMLDataBindingGenerator.WriteUnitFooter(AStream: TStreamHelper);
begin
  AStream.WriteLn();
  AStream.WriteLn('end.');
end;


function TDelphiXMLDataBindingGenerator.CreateNewGUID(): String;
var
  guid: TGUID;

begin
  Result  := '{ GUID generation failed }';
  if CreateGUID(guid) = S_OK then
    Result  := '[''' + GUIDToString(guid) + ''']';
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
