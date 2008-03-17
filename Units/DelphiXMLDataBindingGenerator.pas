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
  SysUtils,

  X2UtNamedFormat;


const
  SectionComments:  array[TDelphiXMLSection] of String =
                    (
                      '  { Forward declarations for %SchemaName:s }',
                      '  { Interfaces for %SchemaName:s }',
                      '  { Classes for %SchemaName:s }',
                      '{ Implementation for %SchemaName:s }'
                    );


  PrefixInterface         = 'IXML';
  PrefixClass             = 'TXML';


  InterfaceItemForward    = '  IXML%Name:s = interface;';
  InterfaceItemInterface  = '  IXML%Name:s = interface(%ParentName:s)';
  InterfaceItemClass      = '  TXML%Name:s = class(%ParentName:s, IXML%Name:s)';


  CollectionInterface     = 'IXMLNodeCollection';
  CollectionClass         = 'TXMLNodeCollection';

  ItemInterface           = 'IXMLNode';
  ItemClass               = 'TXMLNode';
  


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


type
  // #ToDo1 (MvR) 10-3-2008: check handling for floats and booleans maybe?
  TTypeHandling = (thNone, thDateTime);

  TTypeMapping  = record
    SchemaName:   String;
    DelphiName:   String;
    Handling:     TTypeHandling;
  end;


const
  SimpleTypeMapping:  array[0..9] of TTypeMapping =
                      (
                        (SchemaName:  'int';        DelphiName:  'Integer';     Handling:    thNone),
                        (SchemaName:  'integer';    DelphiName:  'Integer';     Handling:    thNone),
                        (SchemaName:  'short';      DelphiName:  'Smallint';    Handling:    thNone),
                        (SchemaName:  'date';       DelphiName:  'TDateTime';   Handling:    thDateTime),
                        (SchemaName:  'time';       DelphiName:  'TDateTime';   Handling:    thDateTime),
                        (SchemaName:  'dateTime';   DelphiName:  'TDateTime';   Handling:    thDateTime),
                        (SchemaName:  'float';      DelphiName:  'Double';      Handling:    thNone),
                        (SchemaName:  'double';     DelphiName:  'Extended';    Handling:    thNone),
                        (SchemaName:  'boolean';    DelphiName:  'Boolean';     Handling:    thNone),
                        (SchemaName:  'string';     DelphiName:  'WideString';  Handling:    thNone)
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
      if SimpleTypeMapping[mappingIndex].SchemaName = dataTypeName then
      begin
        Result := SimpleTypeMapping[mappingIndex].DelphiName;
        Break;
      end;
  end;

//  if Result = 'Variant' then
//    ShowMessage('Unknown type: ' + ADataType.Name);
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
    AStream.WriteLnNamedFmt(SectionComments[ASection],
                            ['SchemaName',  schema.SchemaName]);

    for itemIndex := 0 to Pred(schema.ItemCount) do
      WriteSchemaItem(AStream, schema.Items[itemIndex], ASection);

    AStream.WriteLn;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteDocumentFunctions(AStream: TStreamHelper; ASection: TDelphiXMLSection);
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

          docBinding  := NamedFormat('GetDocBinding(''%SourceName:s'', TXML%Name:s, TargetNamespace) as IXML%Name:s',
                                     ['SourceName', interfaceItem.Name,
                                      'Name',       interfaceItem.TranslatedName]);


          with TNamedFormatStringList.Create() do
          try
            case ASection of
              dxsInterface:
                begin
                  Add('  function Get%Name:s(ADocument: IXMLDocument): IXML%Name:s;');
                  Add('  function Load%Name:s(const AFileName: String): IXML%Name:s;');
                  Add('  function Load%Name:sFromStream(AStream: TStream): IXML%Name:s;');
                  Add('  function New%Name:s: IXML%Name:s;');
                end;
              dxsImplementation:
                begin
                  Add('function Get%Name:s(ADocument: IXMLDocument): IXML%Name:s;');
                  Add('begin');
                  Add('  Result := ADocument.' + docBinding);
                  Add('end;');
                  AddLn;

                  Add('function Load%Name:s(const AFileName: String): IXML%Name:s;');
                  Add('begin');
                  Add('  Result := LoadXMLDocument(AFileName).' + docBinding);
                  Add('end;');
                  AddLn;

                  Add('function Load%Name:sFromStream(AStream: TStream): IXML%Name:s;');
                  Add('var');
                  Add('  doc: IXMLDocument;');
                  AddLn;
                  Add('begin');
                  Add('  doc := NewXMLDocument;');
                  Add('  doc.LoadFromStream(AStream);');
                  Add('  Result  := Get%Name:s(doc);');
                  Add('end;');
                  AddLn;

                  Add('function New%Name:s: IXML%Name:s;');
                  Add('begin');
                  Add('  Result := NewXMLDocument.' + docBinding);
                  Add('end;');
                  AddLn;
                end;
            end;

            AStream.Write(Format(['Name', interfaceItem.TranslatedName]));
          finally
            Free();
          end;

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
      AStream.WriteLnNamedFmt(InterfaceItemForward,
                              ['Name',  AItem.TranslatedName]);
    dxsInterface:
      begin
        if Assigned(AItem.BaseItem) then
          parent  := PrefixInterface + AItem.BaseItem.TranslatedName
        else
          parent  := ItemInterface;

        WriteDocumentation(AStream, AItem);
        AStream.WriteLnNamedFmt(InterfaceItemInterface,
                                ['Name',        AItem.TranslatedName,
                                 'ParentName',  parent]);                                 
        AStream.WriteLn('    ' + CreateNewGUID());

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

        AStream.WriteLnNamedFmt(InterfaceItemClass,
                                ['Name',        AItem.TranslatedName,
                                 'ParentName',  parent]);

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
                  AStream.WriteLnFmt('procedure TXML%s.AfterConstruction;', [AItem.TranslatedName]);
                  AStream.WriteLn('begin');
                  hasInterface  := True;
                end;

                AStream.WriteLnNamedFmt('  RegisterChildNode(''%SourceName:s'', TXML%Name:s);',
                                        ['SourceName', itemProperty.Item.Name,
                                         'Name',       itemProperty.Item.TranslatedName]);
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
  writeOptional:    Boolean;
  writeTextProp:    Boolean;
  hasMembers:       Boolean;
  localHasMembers:  Boolean;
  member:           TDelphiXMLMember;
  value:            String;
  sourceCode:       TNamedFormatStringList;
  propertyItemName: String;

begin
  // #ToDo1 (MvR) 9-3-2008: refactor WriteSchemaInterfaceProperties
  // #ToDo1 (MvR) 17-3-2008: support conversions!
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
      propertyItem  := nil;
      dataTypeName  := '';
      writeTextProp := False;
      writeOptional := True;

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

              { Collections have a Count property, no need to write a
                HasX property as well. }
              writeOptional := (propertyItem.ItemType <> itCollection);

              dataTypeName  := dataTypeName + propertyItem.TranslatedName;
            end;
          end;
      end;


      if Length(dataTypeName) > 0 then
      begin
        writeOptional := writeOptional and
                         itemProperty.IsOptional and
                         (member in [dxmPropertyGet, dxmPropertyDeclaration]);


        sourceCode  := TNamedFormatStringList.Create();
        try
          case ASection of
            dxsInterface,
            dxsClass:
              begin
                { Interface declaration }
                if not hasMembers then
                begin
                  if ASection = dxsClass then
                    AStream.WriteLn('  protected');
                end else if not localHasMembers then
                  AStream.WriteLn();
                  

                case member of
                  dxmPropertyGet:
                    begin
                      if writeOptional then
                        sourceCode.Add('    function GetHas%Name:s: Boolean;');

                      if writeTextProp then
                        sourceCode.Add('    function Get%Name:sText: WideString;');

                      sourceCode.Add('    function Get%Name:s: %DataType:s;');
                    end;

                  dxmPropertySet:
                    if not itemProperty.IsReadOnly then
                    begin
                      if writeTextProp then
                        sourceCode.Add('    procedure Set%Name:sText(const Value: WideString);');

                      sourceCode.Add('    procedure Set%Name:s(const Value: %DataType:s);');
                    end;

                  dxmPropertyDeclaration:
                    begin
                      if writeOptional then
                        sourceCode.Add('    property Has%Name:s: Boolean read GetHas%Name:s;');

                      if writeTextProp then
                        sourceCode.Add('    property %Name:sText: WideString read Get%Name:sText;');

                      if itemProperty.IsReadOnly then
                        sourceCode.Add('    property %Name:s: %DataType:s read Get%Name:s;')
                      else
                        sourceCode.Add('    property %Name:s: %DataType:s read Get%Name:s write Set%Name:s;');
                    end;
                end;


                hasMembers      := True;
                localHasMembers := True;
              end;
            dxsImplementation:
              begin
                { Implementation }
                case member of
                  dxmPropertyGet:
                    begin
                      if writeOptional then
                      begin
                        sourceCode.Add('function TXML%Name:s.GetHas%PropertyName:s: Boolean;');
                        sourceCode.Add('begin');
                        sourceCode.Add('  Result := Assigned(ChildNodes.FindNode(''%PropertySourceName:s''));');
                        sourceCode.Add('end;');
                        sourceCode.AddLn;
                      end;


                      if writeTextProp then
                      begin
                        sourceCode.Add('function TXML%Name:s.Get%PropertyName:sText: WideString;');
                        sourceCode.Add('begin');
                        sourceCode.Add('  Result := ChildNodes[''%PropertySourceName:s''].NodeValue;');
                        sourceCode.Add('end;');
                        sourceCode.AddLn;
                      end;


                      sourceCode.Add('function TXML%Name:s.Get%PropertyName:s: %DataType:s;');

                      case itemProperty.PropertyType of
                        ptSimple:
                          begin
                            sourceCode.Add('begin');
                            sourceCode.Add('  Result := ChildNodes[''%PropertySourceName:s''].NodeValue;');
                            sourceCode.Add('end;');
                          end;

                        ptItem:
                          begin
                            if Assigned(propertyItem) then
                            begin
                              case propertyItem.ItemType of
                                itInterface,
                                itCollection:
                                  begin
                                    sourceCode.Add('begin');
                                    sourceCode.Add('  Result := (ChildNodes[''%Name:s''] as IXML%PropertyItemName:s);');
                                    sourceCode.Add('end;');
                                  end;

                                itEnumeration:
                                  begin
                                    sourceCode.Add('var');
                                    sourceCode.Add('  nodeValue: WideString;');
                                    sourceCode.Add('  enumValue: %DataType:s;');
                                    sourceCode.AddLn;
                                    sourceCode.Add('begin');
                                    sourceCode.Add('  Result := %DataType:s(-1);');
                                    sourceCode.Add('  nodeValue := Get%PropertyName:sText;');
                                    sourceCode.Add('  for enumValue := Low(%DataType:s) to High(%DataType:s) do');
                                    sourceCode.Add('    if %PropertyName:sValues[enumValue] = nodeValue then');
                                    sourceCode.Add('    begin');
                                    sourceCode.Add('      Result := enumValue;');
                                    sourceCode.Add('      break;');
                                    sourceCode.Add('    end;');
                                  end;
                              end;
                            end;
                          end;
                      end;

                      sourceCode.AddLn;
                    end;
                  dxmPropertySet:
                    if not itemProperty.IsReadOnly then
                    begin
                      if writeTextProp then
                      begin
                        sourceCode.Add('procedure TXML%Name:s.Set%PropertyName:sText(const Value: WideString);');
                        sourceCode.Add('begin');
                        sourceCode.Add('  ChildNodes[''%PropertySourceName:s''].NodeValue := Value;');
                        sourceCode.Add('end;');
                        sourceCode.AddLn;
                      end;

                      if Assigned(propertyItem) and (propertyItem.ItemType = itEnumeration) then
                        value := '%PropertyItemName:sValues[Value]'
                      else
                        value := 'Value';

                      sourceCode.Add('procedure TXML%Name:s.Set%PropertyName:s(const Value: %DataType:s);');
                      sourceCode.Add('begin');
                      sourceCode.Add('  ChildNodes[''%PropertySourceName:s''].NodeValue := ' + value + ';');
                      sourceCode.Add('end;');
                      sourceCode.AddLn;
                    end;
                end;
              end;
          end;

          propertyItemName  := '';
          if Assigned(propertyItem) then
            propertyItemName  := propertyItem.TranslatedName;

          AStream.Write(sourceCode.Format(['Name',                AItem.TranslatedName,
                                           'PropertySourceName',  itemProperty.Name,
                                           'PropertyName',        itemProperty.TranslatedName,
                                           'PropertyItemName',    propertyItemName,
                                           'DataType',            dataTypeName]));
        finally
          FreeAndNil(sourceCode);
        end;
      end;
    end;
  end;

  if ASection = dxsClass then
    WriteAfterConstruction();
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaCollection(AStream: TStreamHelper; AItem: TXMLDataBindingCollection; ASection: TDelphiXMLSection);
begin
  if not Assigned(AItem.CollectionItem) then
    Exit;

  case ASection of
    dxsForward:
      AStream.WriteLnNamedFmt(InterfaceItemForward,
                              ['Name',
                               AItem.TranslatedName]);
    dxsInterface:
      begin
        AStream.WriteLnNamedFmt(InterfaceItemInterface,
                                ['Name',        AItem.TranslatedName,
                                 'ParentName',  CollectionInterface]);
        AStream.WriteLn('    ' + CreateNewGUID());

        WriteSchemaCollectionProperties(AStream, AItem, ASection);

        AStream.WriteLn('  end;');
        AStream.WriteLn();
      end;
    dxsClass:
      begin
        AStream.WriteLnNamedFmt(InterfaceItemClass,
                                ['Name',        AItem.TranslatedName,
                                 'ParentName',  CollectionClass]);

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
var
  dataTypeName: string;
  dataIntfName: string;
  sourceCode: TNamedFormatStringList;

begin
  if ASection = dxsClass then
    AStream.WriteLn('  protected');

  // #ToDo1 (MvR) 17-3-2008: DataType for enumerations etc.
  dataTypeName  := PrefixInterface + AItem.CollectionItem.TranslatedName;
  dataIntfName  := dataTypeName;

  sourceCode    := TNamedFormatStringList.Create();
  try
    case ASection of
      dxsInterface,
      dxsClass:
        begin
          sourceCode.Add('    function Get_%ItemName:s(Index: Integer): %DataType:s;');
          sourceCode.Add('    function Add: %DataType:s;');
          sourceCode.Add('    function Insert(Index: Integer): %DataType:s;');
        end;
      dxsImplementation:
        begin
          sourceCode.Add('procedure TXML%Name:s.AfterConstruction;');
          sourceCode.Add('begin');

          // #ToDo1 (MvR) 17-3-2008: DataType class / interface!!
          sourceCode.Add('  RegisterChildNode(''%ItemSourceName:s'', %DataType:s);');

          sourceCode.AddLn;
          sourceCode.Add('  ItemTag := ''%ItemSourceName:s'';');
          sourceCode.Add('  ItemInterface := %DataInterface:s;');
          sourceCode.AddLn;
          sourceCode.Add('  inherited;');
          sourceCode.Add('end;');
          sourceCode.AddLn;

          sourceCode.Add('function TXML%Name:s.Get_%ItemName:s(Index: Integer): %DataType:s;');
          sourceCode.Add('begin');
          sourceCode.Add('  Result := (List[Index] as %DataType:s;');
          sourceCode.Add('end;');
          sourceCode.AddLn;

          sourceCode.Add('function TXML%Name:s.Add(Index: Integer): %DataType:s;');
          sourceCode.Add('begin');
          sourceCode.Add('  Result := (AddItem(-1) as %DataType:s;');
          sourceCode.Add('end;');
          sourceCode.AddLn;

          sourceCode.Add('function TXML%Name:s.Insert(Index: Integer): %DataType:s;');
          sourceCode.Add('begin');
          sourceCode.Add('  Result := (AddItem(Index) as %DataType:s;');
          sourceCode.Add('end;');
          sourceCode.AddLn;
        end;
    end;

    case ASection of
      dxsInterface:
        begin
          sourceCode.AddLn;
          sourceCode.Add('    property %ItemName:s[Index: Integer]: %DataType:s read Get_%ItemName:s; default;');
        end;

      dxsClass:
        begin
          sourceCode.Add('  public');
          sourceCode.Add('    procedure AfterConstruction; override;');
        end;
    end;

    AStream.Write(sourceCode.Format(['Name',            AItem.TranslatedName,
                                     'ItemName',        AItem.CollectionItem.TranslatedName,
                                     'ItemSourceName',  AItem.CollectionItem.Name,
                                     'DataType',        dataTypeName,
                                     'DataInterface',   dataIntfName]));
  finally
    FreeAndNil(sourceCode);
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

  enumStart := NamedFormat('  TXML%Name:s = (',
                           ['Name', AItem.TranslatedName]);
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

  enumStart := NamedFormat('  %Name:sValues: ', ['Name', AItem.TranslatedName]);
  AStream.WriteLn(enumStart + NamedFormat('array[TXML%Name:s] of WideString =',
                                          ['Name',  AItem.TranslatedName]));

  lineIndent := StringOfChar(' ', Length(enumStart));

  AStream.WriteLn(lineIndent + '(');

  for memberIndex := 0 to Pred(AItem.MemberCount) do
  begin
    AStream.Write(NamedFormat('%Indent:s  ''%Name:s''',
                             ['Indent', lineIndent,
                              'Name',   AItem.Members[memberIndex].Name]));

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

