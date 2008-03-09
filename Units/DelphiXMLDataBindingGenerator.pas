unit DelphiXMLDataBindingGenerator;

interface
uses
  Classes,
  XMLSchema,

  X2UtHashes,

  XMLDataBindingGenerator,
  XMLDataBindingHelpers;

type
  TGetFileNameEvent = procedure(Sender: TObject; const SchemaName: String; var Result: String) of object;

  TDelphiXMLSection = (dxsForward, dxsInterface, dxsClass, dxsImplementation);
  TDelphiXMLMember = (dxmPropertyGet, dxmPropertySet, dxmPropertyDeclaration);

  TDelphiXMLDataBindingGenerator = class(TXMLDataBindingGenerator)
  private
    FOnGetFileName: TGetFileNameEvent;
    FProcessedItems: TX2OIHash;
  protected
    procedure GenerateDataBinding(); override;
    procedure GenerateSingleDataBinding();
    procedure GenerateMultipleDataBinding();

    function DelphiSafeName(const AName: String): String;
    function TranslateItemName(AItem: TXMLDataBindingItem): String; override;

    function DoGetFileName(const ASchemaName: String): String;


    function TranslateDataType(ADataType: IXMLTypeDef): String;
    function CreateNewGUID(): String;

    procedure WriteUnitHeader(AStream: TStreamHelper; const AFileName: String);
    procedure WriteInterface(AStream: TStreamHelper);
    procedure WriteImplementation(AStream: TStreamHelper);
    procedure WriteUnitFooter(AStream: TStreamHelper);
    procedure WriteSection(AStream: TStreamHelper; ASection: TDelphiXMLSection);
    procedure WriteDocumentFunctions(AStream: TStreamHelper; ASection: TDelphiXMLSection);
    procedure WriteEnumerationConstants(AStream: TStreamHelper);
    procedure WriteDocumentation(AStream: TStreamHelper; AItem: TXMLDataBindingItem); 

    procedure WriteSchemaItem(AStream: TStreamHelper; AItem: TXMLDataBindingItem; ASection: TDelphiXMLSection);
    procedure WriteSchemaInterface(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
    procedure WriteSchemaInterfaceProperties(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
    procedure WriteSchemaCollection(AStream: TStreamHelper; AItem: TXMLDataBindingCollection; ASection: TDelphiXMLSection);
    procedure WriteSchemaCollectionProperties(AStream: TStreamHelper; AItem: TXMLDataBindingCollection; ASection: TDelphiXMLSection);
    procedure WriteSchemaEnumeration(AStream: TStreamHelper; AItem: TXMLDataBindingEnumeration; ASection: TDelphiXMLSection);
    procedure WriteSchemaEnumerationArray(AStream: TStreamHelper; AItem: TXMLDataBindingEnumeration);

    property ProcessedItems:  TX2OIHash read FProcessedItems;
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


  PrefixInterface         = 'IXML';
  PrefixClass             = 'TXML';
  PrefixOptional          = 'Has';
  PostfixText             = 'Text';


  InterfaceItemForward    = '  ' + PrefixInterface + '%0:s = interface;';
  InterfaceItemInterface  = '  ' + PrefixInterface + '%0:s = interface(%1:s)';
  InterfaceItemClass      = '  ' + PrefixClass + '%0:s = class(%1:s, ' + PrefixInterface + '%0:s)';


  CollectionInterface     = 'IXMLNodeCollection';
  CollectionClass         = 'TXMLNodeCollection';

  ItemInterface           = 'IXMLNode';
  ItemClass               = 'TXMLNode';
  

  MemberPropertyGet       = '    function Get%0:s: %1:s;';
  MemberPropertySet       = '    procedure Set%0:s(const Value: %1:s);';
  MemberProperty          = '    property %0:s: %1:s read Get%0:s write Set%0:s;';
  MemberPropertyReadOnly  = '    property %0:s: %1:s read Get%0:s;';


  // #ToDo1 (MvR) 9-3-2008: document / node / etc
  // #ToDo1 (MvR) 9-3-2008: WideString etc ?
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


  SimpleTypeMapping:  array[0..3, 0..1] of String =
                      (
                        ('int', 'Integer'),
                        ('float', 'Double'),
                        ('boolean', 'Boolean'),
                        ('string', 'WideString')
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

begin
  unitName    := DoGetFileName(Schemas[0].SchemaName);
  unitStream  := TStreamHelper.Create(TFileStream.Create(unitName, fmCreate), soOwned);
  try
    WriteUnitHeader(unitStream, unitName);
    
    WriteInterface(unitStream);
    WriteSection(unitStream, dxsForward);

    FProcessedItems := TX2OIHash.Create();
    try
      FProcessedItems.Clear();
      WriteSection(unitStream, dxsInterface);
      
      FProcessedItems.Clear();
      WriteSection(unitStream, dxsClass);
    finally
      FreeAndNil(FProcessedItems);
    end;

    WriteDocumentFunctions(unitStream, dxsInterface);
    WriteEnumerationConstants(unitStream);

    WriteImplementation(unitStream);
    WriteDocumentFunctions(unitStream, dxsImplementation);
    WriteSection(unitStream, dxsImplementation);

    WriteUnitFooter(unitStream);
  finally
    FreeAndNil(unitStream);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.GenerateMultipleDataBinding();
begin
end;


function TDelphiXMLDataBindingGenerator.TranslateDataType(ADataType: IXMLTypeDef): String;
var
  mappingIndex: Integer;
  dataTypeName: string;

begin
  Assert(not ADataType.IsComplex, 'Complex DataTypes not supported');
  Assert(ADataType.Enumerations.Count = 0, 'Enumerations not supported');
  Result  := 'Variant';

  if (ADataType.NamespaceURI = SXMLSchemaURI_1999) or
     (ADataType.NamespaceURI = SXMLSchemaURI_2000_10) or
     (ADataType.NamespaceURI = SXMLSchemaURI_2001) then
  begin
    dataTypeName  := ADataType.Name;

    for mappingIndex := Low(SimpleTypeMapping) to High(SimpleTypeMapping) do
      if SimpleTypeMapping[mappingIndex][0] = dataTypeName then
      begin
        Result := SimpleTypeMapping[mappingIndex][1];
        Break;
      end;
  end;
end;


function TDelphiXMLDataBindingGenerator.DelphiSafeName(const AName: String): String;
var
  wordIndex:  Integer;

begin
  Result := AName;

  for wordIndex := Low(ReservedWords) to High(ReservedWords) do
  begin
    if Result = ReservedWords[wordIndex] then
    begin
      Result := '_' + Result;
      Break;
    end;
  end;
end;


function TDelphiXMLDataBindingGenerator.TranslateItemName(AItem: TXMLDataBindingItem): String;
begin
  Result := DelphiSafeName(inherited TranslateItemName(AItem));

  case AItem.ItemType of
    itCollection:
      Result := Result + 'List';
      
    itEnumerationMember:
      Result := TXMLDataBindingEnumerationMember(AItem).Enumeration.TranslatedName + '_' + Result;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteUnitHeader(AStream: TStreamHelper; const AFileName: String);
begin
  // #ToDo3 (MvR) 14-4-2007: if outputtype = multiple, use include files

  AStream.WriteLn('{');
  AStream.WriteLn('  X2Software XML Data Binding Wizard');
  AStream.WriteLn('    Generated from: ' + SourceFileName);
  AStream.WriteLn('}');
  AStream.WriteLn('unit ' + ChangeFileExt(ExtractFileName(AFileName), '') + ';');
  AStream.WriteLn();
end;


procedure TDelphiXMLDataBindingGenerator.WriteInterface(AStream: TStreamHelper);
begin
  AStream.WriteLn('interface');
  AStream.WriteLn('uses');
  AStream.WriteLn('  Classes,');
  AStream.WriteLn('  XMLDoc,');
  AStream.WriteLn('  XMLIntf;');
  AStream.WriteLn();
  AStream.WriteLn('type');
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


procedure TDelphiXMLDataBindingGenerator.WriteSection(AStream: TStreamHelper; ASection: TDelphiXMLSection);
var
  schemaIndex:  Integer;
  schema:       TXMLDataBindingSchema;
  itemIndex:    Integer;

begin
  for schemaIndex := 0 to Pred(SchemaCount) do
  begin
    schema := Schemas[schemaIndex];
    AStream.WriteLnFmt(SectionComments[ASection], [schema.SchemaName]);

    for itemIndex := 0 to Pred(schema.ItemCount) do
      WriteSchemaItem(AStream, schema.Items[itemIndex], ASection);

    AStream.WriteLn;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteDocumentFunctions(AStream: TStreamHelper; ASection: TDelphiXMLSection);

  procedure WriteFunction(const AItemName, AFunction, AImplementation: String; const AVariables: String = '');
  begin
    if ASection = dxsInterface then
      AStream.Write('  ');

    AStream.WriteLnFmt('function ' + AFunction + ': %1:s%0:s;',
                       [AItemName, PrefixInterface]);

    if ASection = dxsImplementation then
    begin
      if Length(AVariables) > 0 then
      begin
        AStream.WriteLn('var');
        AStream.WriteLn(AVariables);
        AStream.WriteLn();
      end;

      AStream.WriteLn('begin');
      AStream.WriteLn(AImplementation);
      AStream.WriteLn('end;');
      AStream.WriteLn();
    end;
  end;


var
  schemaIndex:      Integer;
  schema:           TXMLDataBindingSchema;
  itemIndex:        Integer;
  item:             TXMLDataBindingItem;
  interfaceItem:    TXMLDataBindingInterface;
  hasItem:          Boolean;
  docBinding:       String;

begin
  hasItem := False;

  for schemaIndex := 0 to Pred(SchemaCount) do
  begin
    schema := Schemas[schemaIndex];

    for itemIndex := 0 to Pred(schema.ItemCount) do
    begin
      item  := schema.Items[itemIndex];

      if item.ItemType = itInterface then
      begin
        interfaceItem := TXMLDataBindingInterface(item);

        if item.DocumentElement then
        begin
          if not hasItem then
          begin
            if ASection = dxsInterface then
              AStream.Write('  ');
              
            AStream.WriteLn('{ Document functions }');
            hasItem := True;
          end;

          docBinding  := Format('GetDocBinding(''%0:s'', %1:s%0:s, TargetNamespace) as %2:s%0:s',
                                [interfaceItem.TranslatedName,
                                 PrefixClass,
                                 PrefixInterface]);

          WriteFunction(interfaceItem.TranslatedName,
                        'Get%0:s(ADocument: IXMLDocument)',
                        '  Result := ADocument.' + docBinding);

          WriteFunction(interfaceItem.TranslatedName,
                        'Load%0:s(const AFileName: String)',
                        '  Result := LoadXMLDocument(AFileName).' + docBinding);

          WriteFunction(interfaceItem.TranslatedName,
                        'Load%0:sFromStream(AStream: TStream)',
                        '  doc := NewXMLDocument;'#13#10 +
                        '  doc.LoadFromStream(AStream);'#13#10 +
                        '  Result  := GetOfferte(doc);',
                        '  doc: IXMLDocument');

          WriteFunction(interfaceItem.TranslatedName,
                        'New%0:s',
                        '  Result := NewXMLDocument.' + docBinding);

          AStream.WriteLn();
        end;
      end;     
    end;
  end;

  if hasItem and (ASection = dxsInterface) then
  begin
    // #ToDo3 (MvR) 9-3-2008: namespace support?
    AStream.WriteLn('const');
    AStream.WriteLn('  TargetNamespace = '''';');
    AStream.WriteLn();
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteEnumerationConstants(AStream: TStreamHelper);
var
  item:           TXMLDataBindingItem;
  itemIndex:      Integer;
  schema:         TXMLDataBindingSchema;
  schemaIndex:    Integer;
  hasItem:        Boolean;

begin
  { Write array constants for enumerations }
  hasItem := False;

  for schemaIndex := 0 to Pred(SchemaCount) do
  begin
    schema := Schemas[schemaIndex];

    for itemIndex := 0 to Pred(schema.ItemCount) do
    begin
      item := schema.Items[itemIndex];

      if item.ItemType = itEnumeration then
      begin
        if not hasItem then
          AStream.WriteLn('const');

        WriteSchemaEnumerationArray(AStream, TXMLDataBindingEnumeration(item));
        hasItem := True;
      end;
    end;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteDocumentation(AStream: TStreamHelper; AItem: TXMLDataBindingItem);
var
  lines:      TStringList;
  lineIndex:  Integer;

begin
  // #ToDo2 (MvR) 9-3-2008: check for Delphi comment-ending sequences
  if not AItem.HasDocumentation then
    exit;

  lines := TStringList.Create();
  try
    lines.Text  := WrapText(AItem.Documentation, 76);

    AStream.WriteLn('  {');
    for lineIndex := 0 to Pred(lines.Count) do
      AStream.WriteLn('    ' + lines[lineIndex]);

    AStream.WriteLn('  }');
  finally
    FreeAndNil(lines);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaItem(AStream: TStreamHelper; AItem: TXMLDataBindingItem; ASection: TDelphiXMLSection);
begin
  case AItem.ItemType of
    itInterface:    WriteSchemaInterface(AStream, TXMLDataBindingInterface(AItem), ASection);
    itCollection:   WriteSchemaCollection(AStream, TXMLDataBindingCollection(AItem), ASection);
    itEnumeration:  WriteSchemaEnumeration(AStream, TXMLDataBindingEnumeration(AItem), ASection);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaInterface(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
var
  parent:     String;

begin
  if ASection in [dxsInterface, dxsClass] then
  begin
    { Ensure the base item is completely defined first, Delphi doesn't allow
      inheritance with just a forward declaration. }
    if ProcessedItems.Exists(AItem) then
      exit;

    if Assigned(AItem.BaseItem) then
      WriteSchemaInterface(AStream, AItem.BaseItem, ASection);

    ProcessedItems[AItem] := 1;
  end;


  case ASection of
    dxsForward:
      AStream.WriteLnFmt(InterfaceItemForward, [AItem.TranslatedName]);
    dxsInterface:
      begin
        if Assigned(AItem.BaseItem) then
          parent  := PrefixInterface + AItem.BaseItem.TranslatedName
        else
          parent  := ItemInterface;

        WriteDocumentation(AStream, AItem);
        AStream.WriteLnFmt(InterfaceItemInterface, [AItem.TranslatedName,
                                                    parent]);
        AStream.WriteLnFmt('    %s', [CreateNewGUID()]);

        WriteSchemaInterfaceProperties(AStream, AItem, ASection);

        AStream.WriteLn('  end;');
        AStream.WriteLn();
      end;
    dxsClass:
      begin
        if Assigned(AItem.BaseItem) then
          parent  := PrefixClass + AItem.BaseItem.TranslatedName
        else
          parent  := ItemClass;

        AStream.WriteLnFmt(InterfaceItemClass, [AItem.TranslatedName,
                                                parent]);

        WriteSchemaInterfaceProperties(AStream, AItem, ASection);

        AStream.WriteLn('  end;');
        AStream.WriteLn();
      end;
    dxsImplementation:
      begin
        WriteSchemaInterfaceProperties(AStream, AItem, ASection);
      end;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaInterfaceProperties(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);

  procedure WriteAfterConstruction;
  var
    propertyIndex:  Integer;
    propertyItem:   TXMLDataBindingProperty;
    itemProperty:   TXMLDataBindingItemProperty;
    hasInterface:   Boolean;

  begin
    hasInterface  := False;

    for propertyIndex := 0 to Pred(AItem.PropertyCount) do
    begin
      propertyItem  := AItem.Properties[propertyIndex];

      if propertyItem.PropertyType = ptItem then
      begin
        itemProperty  := TXMLDataBindingItemProperty(propertyItem);

        if Assigned(itemProperty.Item) and
           (itemProperty.Item.ItemType <> itEnumeration) then
        begin
          case ASection of
            dxsClass:
              begin
                AStream.WriteLn('  public');
                AStream.WriteLn('    procedure AfterConstruction; override;');
                break;
              end;
            dxsImplementation:
              begin
                if not hasInterface then
                begin
                  AStream.WriteLnFmt('procedure %1:s%0:s.AfterConstruction;',
                                     [AItem.TranslatedName,
                                      PrefixClass]);
                  AStream.WriteLn('begin');
                  hasInterface  := True;
                end;

                AStream.WriteLnFmt('  RegisterChildNode(''%0:s'', %2:s%1:s);',
                                   [itemProperty.Item.Name,
                                    itemProperty.Item.TranslatedName,
                                    PrefixClass]);
              end;
          end;
        end;
      end;
    end;

    if (ASection = dxsImplementation) and hasInterface then
    begin
      AStream.WriteLn('end;');
      AStream.WriteLn();
    end;
  end;


var
  propertyIndex:    Integer;
  itemProperty:     TXMLDataBindingProperty;
  propertyItem:     TXMLDataBindingItem;
  dataTypeName:     String;
  propertyFormat:   String;
  optionalFormat:   String;
  writeOptional:    Boolean;
  writeTextProp:    Boolean;
  hasMembers:       Boolean;
  localHasMembers:  Boolean;
  member:           TDelphiXMLMember;
  value:            String;

begin
  // #ToDo1 (MvR) 9-3-2008: refactor WriteSchemaInterfaceProperties
  if ASection = dxsForward then
    Exit;

  if ASection = dxsImplementation then
    WriteAfterConstruction();

  hasMembers  := False;

  for member := Low(TDelphiXMLMember) to High(TDelphiXMLMember) do
  begin
    localHasMembers := False;

    for propertyIndex := 0 to Pred(AItem.PropertyCount) do
    begin
      itemProperty  := AItem.Properties[propertyIndex];
      dataTypeName  := '';
      writeTextProp := False;

      { Get data type }
      case itemProperty.PropertyType of
        ptSimple:
          dataTypeName  := TranslateDataType(TXMLDataBindingSimpleProperty(itemProperty).DataType);
        ptItem:
          begin
            propertyItem  := TXMLDataBindingItemProperty(itemProperty).Item;
            if Assigned(propertyItem) then
            begin
              if propertyItem.ItemType = itEnumeration then
              begin
                dataTypeName  := PrefixClass;
                writeTextProp := True;
              end else
                dataTypeName  := PrefixInterface;

              dataTypeName  := dataTypeName + propertyItem.TranslatedName;
            end;
          end;
      end;


      if Length(dataTypeName) > 0 then
      begin
        writeOptional := itemProperty.IsOptional and
                         (member in [dxmPropertyGet, dxmPropertyDeclaration]);

        case ASection of
          dxsInterface,
          dxsClass:
            begin
              { Interface declaration }
              propertyFormat  := '';
              optionalFormat  := '';

              case member of
                dxmPropertyGet:
                  begin
                    propertyFormat  := MemberPropertyGet;
                    optionalFormat  := propertyFormat;
                  end;

                dxmPropertySet:
                  if not itemProperty.IsReadOnly then
                  begin
                    propertyFormat  := MemberPropertySet;
                    optionalFormat  := '';
                  end;

                dxmPropertyDeclaration:
                  begin
                    if itemProperty.IsReadOnly then
                      propertyFormat := MemberPropertyReadOnly
                    else
                      propertyFormat := MemberProperty;

                    optionalFormat  := MemberPropertyReadOnly;
                  end;
              end;


              if Length(propertyFormat) > 0 then
              begin
                if not hasMembers then
                begin
                  if ASection = dxsClass then
                    AStream.WriteLn('  protected');
                end else if not localHasMembers then
                  AStream.WriteLn();

                if writeOptional then
                  AStream.WriteLnFmt(optionalFormat, [PrefixOptional + itemProperty.TranslatedName,
                                                      'Boolean']);

                if writeTextProp then
                  AStream.WriteLnFmt(propertyFormat, [itemProperty.TranslatedName + PostfixText,
                                                      'WideString']);

                AStream.WriteLnFmt(propertyFormat, [itemProperty.TranslatedName,
                                                    dataTypeName]);
                hasMembers      := True;
                localHasMembers := True;
              end;
            end;
          dxsImplementation:
            begin
              { Implementation }
              case member of
                dxmPropertyGet:
                  begin
                    // #ToDo3 (MvR) 7-3-2008: extract strings
                    if writeOptional then
                    begin
                      AStream.WriteLnFmt('function %0:s%1s.Get%2:s%3:s: Boolean;',
                                             [PrefixClass,
                                              AItem.TranslatedName,
                                              PrefixOptional,
                                              itemProperty.TranslatedName]);
                      AStream.WriteLn('begin');
                      AStream.WriteLnFmt('  Result := Assigned(ChildNodes.FindNode(''%0:s''));', [itemProperty.Name]);
                      AStream.WriteLn('end;');
                      AStream.WriteLn();
                    end;


                    if writeTextProp then
                    begin
                      AStream.WriteLnFmt('function %0:s%1s.Get%3:s%2:s: WideString;',
                                             [PrefixClass,
                                              AItem.TranslatedName,
                                              PostfixText,
                                              itemProperty.TranslatedName]);
                      AStream.WriteLn('begin');
                      AStream.WriteLnFmt('  Result := ChildNodes[''%0:s''].NodeValue;', [itemProperty.Name]);
                      AStream.WriteLn('end;');
                      AStream.WriteLn();
                    end;


                    AStream.WriteLnFmt('function %0:s%1:s.Get%2:s: %3:s;',
                                           [PrefixClass,
                                            AItem.TranslatedName,
                                            itemProperty.TranslatedName,
                                            dataTypeName]);

                    case itemProperty.PropertyType of
                      ptSimple:
                        begin
                          AStream.WriteLn('begin');
                          AStream.WriteLnFmt('  Result := ChildNodes[''%0:s''].NodeValue;',
                                                 [itemProperty.Name]);
                        end;

                      ptItem:
                        begin
                          propertyItem  := TXMLDataBindingItemProperty(itemProperty).Item;

                          case propertyItem.ItemType of
                            itInterface,
                            itCollection:
                              begin
                                AStream.WriteLn('begin');
                                AStream.WriteLnFmt('  Result := (ChildNodes[''%0:s''] as %1:s%2:s);',
                                                       [itemProperty.Name,
                                                        PrefixInterface,
                                                        propertyItem.TranslatedName]);
                              end;

                            itEnumeration:
                              begin
                                AStream.WriteLn(   'var');
                                AStream.WriteLn(   '  nodeValue: WideString;');
                                AStream.WriteLnFmt('  enumValue: %0:s;', [dataTypeName]);
                                AStream.WriteLn();
                                AStream.WriteLn(   'begin');
                                AStream.WriteLnFmt('  Result := %0:s(-1);', [dataTypeName]);
                                AStream.WriteLnFmt('  nodeValue := Get%0:sText;', [itemProperty.TranslatedName]);
                                AStream.WriteLnFmt('  for enumValue := Low(%0:s) to High(%0:s) do', [dataTypeName]);
                                AStream.WriteLnFmt('    if %0:sValues[enumValue] = nodeValue then', [propertyItem.TranslatedName]);
                                AStream.WriteLn(   '    begin');
                                AStream.WriteLn(   '      Result := enumValue;');
                                AStream.WriteLn(   '      break;');
                                AStream.WriteLn(   '    end;');
                              end;
                          end;
                        end;
                    end;

                    AStream.WriteLn('end;');
                    AStream.WriteLn();
                  end;
                dxmPropertySet:
                  if not itemProperty.IsReadOnly then
                  begin
                    if writeTextProp then
                    begin
                      AStream.WriteLnFmt('procedure %0:s%1:s.Set%2:s%3:s(const Value: WideString);',
                                             [PrefixClass,
                                              AItem.TranslatedName,
                                              itemProperty.TranslatedName,
                                              PostfixText]);
                      AStream.WriteLn('begin');
                      AStream.WriteLnFmt('  ChildNodes[''%s''].NodeValue := Value;', [itemProperty.Name]);
                      AStream.WriteLn('end;');
                      AStream.WriteLn();
                    end;

                    if (itemProperty.PropertyType = ptItem) and
                       (TXMLDataBindingItemProperty(itemProperty).Item.ItemType = itEnumeration) then
                      value := Format('%0:sValues[Value]',
                                      [TXMLDataBindingItemProperty(itemProperty).Item.TranslatedName])
                    else
                      value := 'Value';

                    AStream.WriteLnFmt('procedure %0:s%1:s.Set%2:s(const Value: %3:s);',
                                           [PrefixClass,
                                            AItem.TranslatedName,
                                            itemProperty.TranslatedName,
                                            dataTypeName]);
                    AStream.WriteLn('begin');
                    AStream.WriteLnFmt('  ChildNodes[''%0s''].NodeValue := %1:s;', [itemProperty.Name, value]);
                    AStream.WriteLn('end;');
                    AStream.WriteLn();
                  end;
              end;
          end;
        end;
      end;
    end;
  end;

  if ASection = dxsClass then
    WriteAfterConstruction();
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaCollection(AStream: TStreamHelper; AItem: TXMLDataBindingCollection; ASection: TDelphiXMLSection);
begin
  case ASection of
    dxsForward:
      AStream.WriteLnFmt(InterfaceItemForward, [AItem.TranslatedName]);
    dxsInterface:
      begin
        AStream.WriteLnFmt(InterfaceItemInterface, [AItem.TranslatedName,
                                                        CollectionInterface]);
        AStream.WriteLnFmt('    %s', [CreateNewGUID()]);

        WriteSchemaCollectionProperties(AStream, AItem, ASection);

        AStream.WriteLn('  end;');
        AStream.WriteLn();
      end;
    dxsClass:
      begin
        AStream.WriteLnFmt(InterfaceItemClass, [AItem.TranslatedName,
                                                    CollectionClass]);

        WriteSchemaCollectionProperties(AStream, AItem, ASection);

        AStream.WriteLn('  end;');
        AStream.WriteLn();
      end;
    dxsImplementation:
      begin
        WriteSchemaCollectionProperties(AStream, AItem, ASection);
      end;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaCollectionProperties(AStream: TStreamHelper; AItem: TXMLDataBindingCollection; ASection: TDelphiXMLSection);

  procedure WriteMethodInterface(const AFunction: String);
  begin
    AStream.WriteLnFmt('    function ' + AFunction + ': %1:s%0:s;',
                       [AItem.CollectionItem.TranslatedName,
                        PrefixInterface]);
  end;


  procedure WriteMethodImplementation(const AFunction, AImplementation: String);
  begin
    AStream.WriteLnFmt('function %3:s%0:s.' + AFunction + ': %2:s%1:s;',
                       [AItem.TranslatedName,
                        AItem.CollectionItem.TranslatedName,
                        PrefixInterface,
                        PrefixClass]);
    AStream.WriteLn('begin');

    AStream.WriteLnFmt(AImplementation,
                       [AItem.CollectionItem.TranslatedName,
                        PrefixInterface]);

    AStream.WriteLn('end;');
    AStream.WriteLn();
  end;
    

begin
  if ASection = dxsClass then
    AStream.WriteLn('  protected');


  case ASection of
    dxsInterface,
    dxsClass:
      begin
        WriteMethodInterface('Get_%0:s(Index: Integer)');
        WriteMethodInterface('Add');
        WriteMethodInterface('Insert(Index: Integer)');
      end;
    dxsImplementation:
      begin
        AStream.WriteLnFmt('procedure %1:s%0:s.AfterConstruction;',
                           [AItem.TranslatedName,
                            PrefixClass]);
        AStream.WriteLn('begin');

        AStream.WriteLnFmt('  RegisterChildNode(''%0:s'', %2:s%1:s);',
                           [AItem.CollectionItem.Name,
                            AItem.CollectionItem.TranslatedName,
                            PrefixClass]);

        AStream.WriteLn();
        AStream.WriteLnFmt('  ItemTag := ''%0:s'';',
                           [AItem.CollectionItem.Name]);
                           
        AStream.WriteLnFmt('  ItemInterface := %1:s%0:s;',
                           [AItem.CollectionItem.TranslatedName,
                            PrefixInterface]);

        AStream.WriteLn();
        AStream.WriteLn('  inherited;');
        AStream.WriteLn('end;');
        AStream.WriteLn();

        WriteMethodImplementation('Get_%1:s(Index: Integer)',
                                  '  Result := (List[Index] as %1:s%0:s);');

        WriteMethodImplementation('Add',
                                  '  Result := (AddItem(-1) as %1:s%0:s);');

        WriteMethodImplementation('Insert(Index: Integer)',
                                  '  Result := (AddItem(Index) as %1:s%0:s);');
      end;
  end;

  case ASection of
    dxsInterface:
      begin
        AStream.WriteLn;
        AStream.WriteLnFmt('    property %0:s[Index: Integer]: %1:s%0:s read Get_%0:s; default;',
                               [AItem.CollectionItem.TranslatedName,
                                PrefixInterface]);
      end;

    dxsClass:
      begin
        AStream.WriteLn('  public');
        AStream.WriteLn('    procedure AfterConstruction; override;');
      end;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaEnumeration(AStream: TStreamHelper; AItem: TXMLDataBindingEnumeration; ASection: TDelphiXMLSection);
var
  memberIndex: Integer;
  enumStart: String;
  lineIndent: String;

begin
  if (ASection <> dxsForward) or (AItem.MemberCount = 0) then
    exit;

  enumStart := Format('  %0:s%1:s = (', [PrefixClass, AItem.TranslatedName]);
  AStream.Write(enumStart);
  lineIndent := StringOfChar(' ', Length(enumStart));

  for memberIndex := 0 to Pred(AItem.MemberCount) do
  begin
    if memberIndex > 0 then
      AStream.Write(lineIndent);

    AStream.Write(AItem.Members[memberIndex].TranslatedName);

    if memberIndex < Pred(AItem.MemberCount) then
      AStream.WriteLn(',')
    else
      AStream.WriteLn(');');
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaEnumerationArray(AStream: TStreamHelper; AItem: TXMLDataBindingEnumeration);
var
  memberIndex: Integer;
  enumStart: String;
  lineIndent: String;

begin
  if (AItem.MemberCount = 0) then
    exit;

  enumStart := Format('  %0:sValues: ', [AItem.TranslatedName]);
  AStream.WriteLn(enumStart + Format('array[%0:s%1:s] of WideString =',
                                     [PrefixClass, AItem.TranslatedName]));
  lineIndent := StringOfChar(' ', Length(enumStart));

  AStream.WriteLn(lineIndent + '(');

  for memberIndex := 0 to Pred(AItem.MemberCount) do
  begin
    AStream.Write(Format('%s  ''%s''', [lineIndent, AItem.Members[memberIndex].Name]));

    if memberIndex < Pred(AItem.MemberCount) then
      AStream.WriteLn(',')
    else
      AStream.WriteLn();
  end;

  AStream.WriteLn(lineIndent + ');');
  AStream.WriteLn();
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

