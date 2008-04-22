unit DelphiXMLDataBindingGenerator;

interface
uses
  Classes,
  Contnrs,
  XMLSchema,

  X2UtHashes,

  DelphiXMLDataBindingResources,
  XMLDataBindingGenerator,
  XMLDataBindingHelpers;


type
  TGetFileNameEvent = procedure(Sender: TObject; const SchemaName: String; var Path, FileName: String) of object;


  TXMLSchemaList  = class(TObjectList)
  private
    function GetItem(Index: Integer): TXMLDataBindingSchema;
    procedure SetItem(Index: Integer; const Value: TXMLDataBindingSchema);
  public
    constructor Create();

    property Items[Index: Integer]: TXMLDataBindingSchema read GetItem  write SetItem; default;
  end;


  TDelphiXMLDataBindingGenerator = class(TXMLDataBindingGenerator)
  private
    FProcessedItems:  TX2OIHash;
    FUnitNames:       TX2OSHash;

    FOnGetFileName:   TGetFileNameEvent;
  protected
    procedure GenerateDataBinding(); override;
    procedure GenerateOutputFile(ASchemaList: TXMLSchemaList; const ASourceFileName, AUnitName: String);
    function GenerateUsesClause(ASchemaList: TXMLSchemaList): String;

    function DelphiSafeName(const AName: String): String;
    function TranslateItemName(AItem: TXMLDataBindingItem): String; override;

    function DoGetFileName(const ASchemaName: String): String;


    function GetDataTypeMapping(ADataType: IXMLTypeDef; out ATypeMapping: TTypeMapping): Boolean;
    function GetDataTypeName(AProperty: TXMLDataBindingProperty; AInterfaceName: Boolean): String;
    function TranslateDataType(ADataType: IXMLTypeDef): String;
    function CreateNewGUID(): String;

    procedure WriteUnitHeader(AStream: TStreamHelper; const ASourceFileName, AFileName: String);
    procedure WriteSection(AStream: TStreamHelper; ASection: TDelphiXMLSection; ASchemaList: TXMLSchemaList);
    procedure WriteDocumentFunctions(AStream: TStreamHelper; ASection: TDelphiXMLSection; ASchemaList: TXMLSchemaList);
    procedure WriteEnumerationConversions(AStream: TStreamHelper; ASection: TDelphiXMLSection; ASchemaList: TXMLSchemaList);
    procedure WriteConversionHelpers(AStream: TStreamHelper; ASchemaList: TXMLSchemaList);
    procedure WriteDocumentation(AStream: TStreamHelper; AItem: TXMLDataBindingItem);
    procedure WriteAfterConstruction(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
    function WriteInlineCollectionFields(AStream: TStreamHelper; AItem: TXMLDataBindingInterface): Boolean;

    procedure WriteSchemaItem(AStream: TStreamHelper; AItem: TXMLDataBindingItem; ASection: TDelphiXMLSection);
    procedure WriteSchemaInterface(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
    procedure WriteSchemaInterfaceProperties(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
    function WriteSchemaInterfaceCollectionProperties(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection): Boolean;
    function WriteSchemaInterfaceProperty(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; AProperty: TXMLDataBindingProperty; ASection: TDelphiXMLSection; AMember: TDelphiXMLMember; ANewLine: Boolean): Boolean;
    procedure WriteSchemaEnumeration(AStream: TStreamHelper; AItem: TXMLDataBindingEnumeration; ASection: TDelphiXMLSection);
    procedure WriteSchemaEnumerationArray(AStream: TStreamHelper; AItem: TXMLDataBindingEnumeration);

    function GetDelphiNodeType(AProperty: TXMLDataBindingProperty): TDelphiNodeType;
    function DataTypeConversion(const ADestination, ASource: String; ADataType: IXMLTypeDef; AAccessor: TDelphiAccessor; ANodeType: TDelphiNodeType; const ALinesBefore: String = ''): String;
    function XMLToNativeDataType(const ADestination, ASource: String; ADataType: IXMLTypeDef; ANodeType: TDelphiNodeType; const ALinesBefore: String = ''): String;
    function NativeDataTypeToXML(const ADestination, ASource: String; ADataType: IXMLTypeDef; ANodeType: TDelphiNodeType; const ALinesBefore: String = ''): String;

    property ProcessedItems:  TX2OIHash read FProcessedItems;
    property UnitNames:       TX2OSHash read FUnitNames;
  public
    property OnGetFileName:   TGetFileNameEvent read FOnGetFileName write FOnGetFileName;
  end;


implementation
uses
  SysUtils,

  X2UtNamedFormat;



{ TDelphiXMLDataBindingGenerator }
procedure TDelphiXMLDataBindingGenerator.GenerateDataBinding();
var
  schemaList:   TXMLSchemaList;
  schemaIndex:  Integer;
  schema:       TXMLDataBindingSchema;
  unitName:     String;
  
begin
  schemaList  := TXMLSchemaList.Create();
  try
    case OutputType of
      otSingle:
        begin
          for schemaIndex := 0 to Pred(SchemaCount) do
            schemaList.Add(Schemas[schemaIndex]);

          unitName  := DoGetFileName(Schemas[0].SchemaName);
          GenerateOutputFile(schemaList, SourceFileName, unitName);
        end;

      otMultiple:
        begin
          FUnitNames  := TX2OSHash.Create();
          try
            for schemaIndex := 0 to Pred(SchemaCount) do
            begin
              schema              := Schemas[schemaIndex];
              FUnitNames[schema]  := DoGetFileName(schema.SchemaName);
            end;

            for schemaIndex := 0 to Pred(SchemaCount) do
            begin
              schema  := Schemas[schemaIndex];

              schemaList.Clear();
              schemaList.Add(schema);

              unitName  := FUnitNames[schema];
              GenerateOutputFile(schemaList, schema.SourceFileName, unitName);
            end;
          finally
            FreeAndNil(FUnitNames);
          end;
        end;
    end;
  finally
    FreeAndNil(schemaList);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.GenerateOutputFile(ASchemaList: TXMLSchemaList; const ASourceFileName, AUnitName: String);
var
  unitStream:         TStreamHelper;
  usesClause:         String;

begin
  usesClause  := '';

  if OutputType = otMultiple then
    usesClause  := GenerateUsesClause(ASchemaList);

  unitStream  := TStreamHelper.Create(TFileStream.Create(AUnitName, fmCreate), soOwned);
  try
    WriteUnitHeader(unitStream, ASourceFileName, AUnitName);

    unitStream.WriteNamedFmt(UnitInterface,
                             ['UsesClause', usesClause]);
    WriteSection(unitStream, dxsForward, ASchemaList);

    FProcessedItems := TX2OIHash.Create();
    try
      FProcessedItems.Clear();
      WriteSection(unitStream, dxsInterface, ASchemaList);

      FProcessedItems.Clear();
      WriteSection(unitStream, dxsClass, ASchemaList);
    finally
      FreeAndNil(FProcessedItems);
    end;

    WriteDocumentFunctions(unitStream, dxsInterface, ASchemaList);
    WriteEnumerationConversions(unitStream, dxsInterface, ASchemaList);

    unitStream.Write(UnitImplementation);
    WriteDocumentFunctions(unitStream, dxsImplementation, ASchemaList);
    WriteEnumerationConversions(unitStream, dxsImplementation, ASchemaList);
    WriteConversionHelpers(unitStream, ASchemaList);

    WriteSection(unitStream, dxsImplementation, ASchemaList);

    unitStream.Write(unitFooter);
  finally
    FreeAndNil(unitStream);
  end;
end;


function TDelphiXMLDataBindingGenerator.GenerateUsesClause(ASchemaList: TXMLSchemaList): String;
var
  includedSchemas:    TObjectList;

  procedure AddSchema(ASchema: TXMLDataBindingSchema);
  begin
    if Assigned(ASchema) and
       (includedSchemas.IndexOf(ASchema) = -1) and
       (ASchemaList.IndexOf(ASchema) = -1) then
      includedSchemas.Add(ASchema);
  end;
  

var
  schemaIndex:        Integer;
  schema:             TXMLDataBindingSchema;
  itemIndex:          Integer;
  interfaceItem:      TXMLDataBindingInterface;
  propertyIndex:      Integer;
  propertyItem:       TXMLDataBindingProperty;
  includeIndex:       Integer;

begin
  Result := '';

  includedSchemas := TObjectList.Create(False);
  try
    { Determine which items are used }
    for schemaIndex := 0 to Pred(ASchemaList.Count) do
    begin
      schema  := ASchemaList[schemaIndex];

      for itemIndex := 0 to Pred(schema.ItemCount) do
      begin
        if schema.Items[itemIndex].ItemType = itInterface then
        begin
          interfaceItem := TXMLDataBindingInterface(schema.Items[itemIndex]);

          if Assigned(interfaceItem.CollectionItem) then
            AddSchema(interfaceItem.CollectionItem.Schema);

          for propertyIndex := 0 to Pred(interfaceItem.PropertyCount) do
          begin
            propertyItem  := interfaceItem.Properties[propertyIndex];

            if propertyItem.PropertyType = ptItem then
              AddSchema(TXMLDataBindingItemProperty(propertyItem).Item.Schema);
          end;
        end;
      end;
    end;

    { Build uses clause }
    if includedSchemas.Count > 0 then
    begin
      for includeIndex := 0 to Pred(includedSchemas.Count) do
      begin
        schema  := TXMLDataBindingSchema(includedSchemas[includeIndex]);
        Result  := Result + '  ' + ChangeFileExt(ExtractFileName(FUnitNames[schema]), '') + ',' + CrLf;
      end;

      Result  := Result + CrLf;
    end;
  finally
    FreeAndNil(includedSchemas);
  end;
end;


function TDelphiXMLDataBindingGenerator.GetDataTypeMapping(ADataType: IXMLTypeDef; out ATypeMapping: TTypeMapping): Boolean;
var
  mappingIndex: Integer;
  dataTypeName: String;

begin
  Assert(not ADataType.IsComplex, 'Complex DataTypes not supported');
  Assert(ADataType.Enumerations.Count = 0, 'Enumerations not supported');
  Result  := False;

  if (ADataType.NamespaceURI = SXMLSchemaURI_1999) or
     (ADataType.NamespaceURI = SXMLSchemaURI_2000_10) or
     (ADataType.NamespaceURI = SXMLSchemaURI_2001) then
  begin
    dataTypeName  := ADataType.Name;

    for mappingIndex := Low(SimpleTypeMapping) to High(SimpleTypeMapping) do
      if SimpleTypeMapping[mappingIndex].SchemaName = dataTypeName then
      begin
        ATypeMapping  := SimpleTypeMapping[mappingIndex];
        Result        := True;
        Break;
      end;
  end;
end;


function TDelphiXMLDataBindingGenerator.GetDataTypeName(AProperty: TXMLDataBindingProperty; AInterfaceName: Boolean): String;
var
  item:   TXMLDataBindingItem;

begin
  case AProperty.PropertyType of
    ptSimple:
      Result := TranslateDataType(TXMLDataBindingSimpleProperty(AProperty).DataType);
    ptItem:
      begin
        item  := TXMLDataBindingItemProperty(AProperty).Item;

        if (item.ItemType = itEnumeration) or (not AInterfaceName) then
          Result := PrefixClass
        else
          Result := PrefixInterface;

        Result := Result + item.TranslatedName;
      end;
  end;
end;


function TDelphiXMLDataBindingGenerator.TranslateDataType(ADataType: IXMLTypeDef): String;
var
  typeMapping:  TTypeMapping;

begin
  Result  := 'Variant';
  if GetDataTypeMapping(ADataType, typeMapping) then
    Result := typeMapping.DelphiName;
end;


function TDelphiXMLDataBindingGenerator.DelphiSafeName(const AName: String): String;
var
  charIndex:  Integer;
  wordIndex:  Integer;

begin
  Result := AName;


  { Remove unsafe characters }
  for charIndex := Length(Result) downto 1 do
  begin
    if not (Result[charIndex] in SafeChars) then
      Delete(Result, charIndex, 1);
  end;


  if Length(Result) > 0 then
  begin
    { Number as the first character is not allowed }
    if Result[1] in ['0'..'9'] then
      Result := '_' + Result;


    { Check for reserved words }
    for wordIndex := Low(ReservedWords) to High(ReservedWords) do
    begin
      if SameText(Result, ReservedWords[wordIndex]) then
      begin
        Result := '_' + Result;
        Break;
      end;
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


procedure TDelphiXMLDataBindingGenerator.WriteUnitHeader(AStream: TStreamHelper; const ASourceFileName, AFileName: String);
begin
  AStream.WriteNamedFmt(UnitHeader,
                        ['SourceFileName',  ASourceFileName,
                         'UnitName',        ChangeFileExt(ExtractFileName(AFileName), ''),
                         'DateTime',        DateTimeToStr(Now)]);
end;


procedure TDelphiXMLDataBindingGenerator.WriteSection(AStream: TStreamHelper; ASection: TDelphiXMLSection; ASchemaList: TXMLSchemaList);
var
  schemaIndex:  Integer;
  schema:       TXMLDataBindingSchema;
  itemIndex:    Integer;

begin
  for schemaIndex := 0 to Pred(ASchemaList.Count) do
  begin
    schema := ASchemaList[schemaIndex];
    AStream.WriteLnNamedFmt(SectionComments[ASection],
                            ['SchemaName',  schema.SchemaName]);

    for itemIndex := 0 to Pred(schema.ItemCount) do
      WriteSchemaItem(AStream, schema.Items[itemIndex], ASection);

    AStream.WriteLn;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteDocumentFunctions(AStream: TStreamHelper; ASection: TDelphiXMLSection; ASchemaList: TXMLSchemaList);
var
  schemaIndex:      Integer;
  schema:           TXMLDataBindingSchema;
  itemIndex:        Integer;
  item:             TXMLDataBindingItem;
  interfaceItem:    TXMLDataBindingInterface;
  hasItem:          Boolean;

begin
  hasItem := False;

  for schemaIndex := 0 to Pred(ASchemaList.Count) do
  begin
    schema := ASchemaList[schemaIndex];

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

          with TNamedFormatStringList.Create() do
          try
            case ASection of
              dxsInterface:         Add(DocumentFunctionsInterface);
              dxsImplementation:    Add(DocumentFunctionsImplementation);
            end;

            AStream.Write(Format(['SourceName', interfaceItem.Name,
                                  'Name',       interfaceItem.TranslatedName]));
          finally
            Free();
          end;

          AStream.WriteLn();
        end;
      end;     
    end;
  end;

  if ASection = dxsInterface then
  begin
    AStream.WriteLn('const');
    AStream.WriteLn('  XMLSchemaInstanceURI = ''http://www.w3.org/2001/XMLSchema-instance'';');

    if hasItem then
      // #ToDo3 (MvR) 9-3-2008: namespace support?
      AStream.WriteLn('  TargetNamespace = '''';');

    AStream.WriteLn();
    AStream.WriteLn();
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteEnumerationConversions(AStream: TStreamHelper; ASection: TDelphiXMLSection; ASchemaList: TXMLSchemaList);
var
  enumerations:     TObjectList;
  schemaIndex:      Integer;
  schema:           TXMLDataBindingSchema;
  itemIndex:        Integer;
  item:             TXMLDataBindingItem;
  enumerationItem:  TXMLDataBindingEnumeration;
  sourceCode:       TNamedFormatStringList;
  indent:           String;

begin
  if not (ASection in [dxsInterface, dxsImplementation]) then
    Exit;


  enumerations  := TObjectList.Create(False);
  try
    for schemaIndex := 0 to Pred(ASchemaList.Count) do
    begin
      schema := ASchemaList[schemaIndex];

      for itemIndex := 0 to Pred(schema.ItemCount) do
      begin
        item := schema.Items[itemIndex];

        if item.ItemType = itEnumeration then
          enumerations.Add(item);
      end;
    end;


    if enumerations.Count > 0 then
    begin
      if ASection = dxsInterface then
      begin
        { Enumeration value arrays }
        AStream.WriteLn('const');

        for itemIndex := 0 to Pred(enumerations.Count) do
          WriteSchemaEnumerationArray(AStream, TXMLDataBindingEnumeration(enumerations[itemIndex]));
      end;


      { Conversion helpers }
      if ASection = dxsInterface then
        AStream.Write('  ');
        
      AStream.WriteLn('{ Enumeration conversion helpers }');
      

      for itemIndex := Pred(enumerations.Count) downto 0 do
      begin
        enumerationItem := TXMLDataBindingEnumeration(enumerations[itemIndex]);

        indent  := '';
        if ASection = dxsInterface then
          indent  := '  ';

        sourceCode  := TNamedFormatStringList.Create();
        try
          sourceCode.Add(indent + 'function StringTo%<ItemName>:s(const AValue: WideString): %<DataType>:s;');

          if ASection = dxsImplementation then
          begin
            sourceCode.Add('var');
            sourceCode.Add('  enumValue: %<DataType>:s;');
            sourceCode.AddLn;
            sourceCode.Add('begin');
            sourceCode.Add('  Result := %<DataType>:s(-1);');
            sourceCode.Add('  for enumValue := Low(%<DataType>:s) to High(%<DataType>:s) do');
            sourceCode.Add('    if %<ItemName>:sValues[enumValue] = AValue then');
            sourceCode.Add('    begin');
            sourceCode.Add('      Result := enumValue;');
            sourceCode.Add('      break;');
            sourceCode.Add('    end;');
            sourceCode.Add('end;');
            sourceCode.AddLn;
          end;

          AStream.Write(sourceCode.Format(['ItemName',  enumerationItem.TranslatedName,
                                           'DataType',  PrefixClass + enumerationItem.TranslatedName]));
        finally
          FreeAndNil(sourceCode);
        end;
      end;

      AStream.WriteLn;
    end;
  finally
    FreeAndNil(enumerations);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteConversionHelpers(AStream: TStreamHelper; ASchemaList: TXMLSchemaList);
var
  usedConversions:  TTypeConversions;
  schemaIndex:      Integer;
  schema:           TXMLDataBindingSchema;
  itemIndex:        Integer;
  interfaceItem:    TXMLDataBindingInterface;
  propertyIndex:    Integer;
  propertyItem:     TXMLDataBindingSimpleProperty;
  typeMapping:      TTypeMapping;
  conversion:       TTypeConversion;
  hasHelpers:       Boolean;
  hasNillable:      Boolean;

begin
  usedConversions := [];
  hasNillable     := False;

  { Determine which conversions are used }
  for schemaIndex := Pred(ASchemaList.Count) downto 0 do
  begin
    schema  := ASchemaList[schemaIndex];

    for itemIndex := Pred(schema.ItemCount) downto 0 do
    begin
      if schema.Items[itemIndex].ItemType = itInterface then
      begin
        interfaceItem := TXMLDataBindingInterface(schema.Items[itemIndex]);

        for propertyIndex := Pred(interfaceItem.PropertyCount) downto 0 do
        begin
          if interfaceItem.Properties[propertyIndex].PropertyType = ptSimple then
          begin
            propertyItem  := TXMLDataBindingSimpleProperty(interfaceItem.Properties[propertyIndex]);
            if GetDataTypeMapping(propertyItem.DataType, typeMapping) then
              Include(usedConversions, typeMapping.Conversion);

            if propertyItem.IsNillable then
              hasNillable := True;
          end;
        end;
      end;
    end;
  end;


  hasHelpers  := False;
  for conversion := Low(TTypeConversion) to High(TTypeConversion) do
    if conversion in usedConversions then
    begin
      if Length(TypeConversionHelpers[conversion]) > 0 then
      begin
        if not hasHelpers then
          AStream.WriteLn('{ Data type conversion helpers }');

        AStream.Write(TypeConversionHelpers[conversion]);
        hasHelpers  := True;
      end;
    end;

  if hasHelpers then
    AStream.WriteLn();

  if hasNillable then
    AStream.Write(NilElementHelpers);
end;


procedure TDelphiXMLDataBindingGenerator.WriteDocumentation(AStream: TStreamHelper; AItem: TXMLDataBindingItem);
var
  documentation:    String;
  lineIndex:        Integer;
  lines:            TStringList;

begin
  if not AItem.HasDocumentation then
    exit;

  lines := TStringList.Create();
  try
    documentation := AItem.Documentation;

    { Replace dangerous characters }
    documentation := StringReplace(documentation, '{', '(', [rfReplaceAll]);
    documentation := StringReplace(documentation, '}', ')', [rfReplaceAll]);

    lines.Text    := WrapText(documentation, 76);

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
        else if AItem.IsCollection then
          parent  := CollectionInterface
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
        else if AItem.IsCollection then
          parent  := CollectionClass
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


procedure TDelphiXMLDataBindingGenerator.WriteAfterConstruction(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
var
  hasPrototype:       Boolean;


  procedure WritePrototype();
  begin
    if not hasPrototype then
    begin
      case ASection of
        dxsClass:
          begin
            AStream.WriteLn('  public');
            AStream.WriteLn('    procedure AfterConstruction; override;');
          end;

        dxsImplementation:
          begin
            AStream.WriteLnFmt('procedure TXML%s.AfterConstruction;', [AItem.TranslatedName]);
            AStream.WriteLn('begin');
          end;
      end;

      hasPrototype  := True;
    end;
  end;


var
  itemProperty:         TXMLDataBindingItemProperty;
  propertyIndex:        Integer;
  propertyItem:         TXMLDataBindingProperty;

begin
  if not (ASection in [dxsClass, dxsImplementation]) then
    Exit;

  if (ASection = dxsClass) and
     (not AItem.IsCollection) then
    WriteInlineCollectionFields(AStream, AItem);


  hasPrototype  := False;
    
  for propertyIndex := 0 to Pred(AItem.PropertyCount) do
  begin
    propertyItem  := AItem.Properties[propertyIndex];

    if (not AItem.IsCollection) and Assigned(propertyItem.Collection) then
    begin
      WritePrototype;

      { Inline collection }
      if ASection = dxsImplementation then
      begin
        AStream.WriteLnNamedFmt('  RegisterChildNode(''%<ItemSourceName>:s'', %<ItemClass>:s);',
                                ['ItemSourceName',      propertyItem.Name,
                                 'ItemClass',           PrefixClass + propertyItem.TranslatedName]);

        AStream.WriteLnNamedFmt('  %<FieldName>:s := CreateCollection(%<CollectionClass>:s, %<ItemInterface>:s, ''%<ItemSourceName>:s'') as %<CollectionInterface>:s;',
                                ['FieldName',           PrefixField + propertyItem.TranslatedName,
                                 'CollectionClass',     PrefixClass + propertyItem.Collection.TranslatedName,
                                 'CollectionInterface', PrefixInterface + propertyItem.Collection.TranslatedName,
                                 'ItemInterface',       PrefixInterface + propertyItem.TranslatedName,
                                 'ItemSourceName',      propertyItem.Name]);
      end;
    end else if (propertyItem.PropertyType = ptItem) and
                ((not AItem.IsCollection) or
                 (propertyItem <> AItem.CollectionItem)) then
    begin
      { Item property }
      itemProperty  := TXMLDataBindingItemProperty(propertyItem);

      if Assigned(itemProperty.Item) and
         (itemProperty.Item.ItemType = itInterface) then
      begin
        case ASection of
          dxsClass:
            WritePrototype;
              
          dxsImplementation:
            begin
              WritePrototype;
              AStream.WriteLnNamedFmt('  RegisterChildNode(''%<SourceName>:s'', TXML%<Name>:s);',
                                      ['SourceName', propertyItem.Name,
                                       'Name',       itemProperty.Item.TranslatedName]);
            end;
        end;
      end;
    end;
  end;

  if AItem.IsCollection then
  begin
    WritePrototype;

    if ASection = dxsImplementation then
    begin
      WritePrototype;
      AStream.WriteLnNamedFmt('  RegisterChildNode(''%<SourceName>:s'', %<DataClass>:s);',
                              ['SourceName',  AItem.CollectionItem.Name,
                               'DataClass',  GetDataTypeName(AItem.CollectionItem, False)]);
      AStream.WriteLn;
      AStream.WriteLnFmt('  ItemTag := ''%s'';', [AItem.CollectionItem.Name]);
      AStream.WriteLnFmt('  ItemInterface := %s;', [GetDataTypeName(AItem.CollectionItem, True)]);
      AStream.WriteLn;
    end;
  end;

  if hasPrototype and (ASection = dxsImplementation) then
  begin
    AStream.WriteLn('  inherited;');
    AStream.WriteLn('end;');
    AStream.WriteLn;
  end;
end;


function TDelphiXMLDataBindingGenerator.WriteInlineCollectionFields(AStream: TStreamHelper; AItem: TXMLDataBindingInterface): Boolean;
var
  propertyIndex:      Integer;
  collectionProperty: TXMLDataBindingProperty;

begin
  Result  := False;

  for propertyIndex := 0 to Pred(AItem.PropertyCount) do
    if AItem.Properties[propertyIndex].IsRepeating then
    begin
      collectionProperty  := AItem.Properties[propertyIndex];

      if Assigned(collectionProperty.Collection) then
      begin
        if not Result then
        begin
          AStream.WriteLn('  private');
          Result  := True;
        end;

        AStream.WriteLnNamedFmt('    %<PropertyName>:s: %<DataInterface>:s;',
                                ['PropertyName',  PrefixField + collectionProperty.TranslatedName,
                                 'DataInterface', PrefixInterface + collectionProperty.Collection.TranslatedName]);
      end;
    end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaInterfaceProperties(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
var
  propertyIndex:    Integer;
  itemProperty:     TXMLDataBindingProperty;
  hasMembers:       Boolean;
  firstMember:      Boolean;
  member:           TDelphiXMLMember;

begin
  if ASection = dxsForward then
    Exit;

  if ASection in [dxsClass, dxsImplementation] then
    WriteAfterConstruction(AStream, AItem, ASection);

  if ASection = dxsClass then
    AStream.WriteLn('  protected');

  hasMembers  := WriteSchemaInterfaceCollectionProperties(AStream, AItem, ASection);

  for member := Low(TDelphiXMLMember) to High(TDelphiXMLMember) do
  begin
    firstMember   := True;

    for propertyIndex := 0 to Pred(AItem.PropertyCount) do
    begin
      itemProperty  := AItem.Properties[propertyIndex];

      if WriteSchemaInterfaceProperty(AStream, AItem, itemProperty, ASection, member,
                                      hasMembers and firstMember and (ASection in [dxsInterface, dxsClass])) then
      begin
        firstMember := False;
        hasMembers  := True;
      end;
    end;
  end;
end;


function TDelphiXMLDataBindingGenerator.WriteSchemaInterfaceCollectionProperties(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection): Boolean;
var
  dataIntfName: String;
  dataTypeName: String;
  dataClassName: String;
  collectionItem: TXMLDataBindingItem;
  sourceCode: TNamedFormatStringList;
  typeDef: IXMLTypeDef;
  typeMapping: TTypeMapping;

begin
  Result := False;

  if not AItem.IsCollection then
    Exit;

  case AItem.CollectionItem.PropertyType of
    ptSimple:
      begin
        dataTypeName  := TranslateDataType(TXMLDataBindingSimpleProperty(AItem.CollectionItem).DataType);
        dataClassName := 'TXMLNode';
        dataIntfName  := 'IXMLNode';
      end;
    ptItem:
      begin
        collectionItem  := TXMLDataBindingItemProperty(AItem.CollectionItem).Item;

        if collectionItem.ItemType = itEnumeration then
        begin
          // #ToDo1 (MvR) 17-3-2008: DataType and conversions for enumerations
          dataTypeName  := PrefixInterface + collectionItem.TranslatedName;
          dataClassName := PrefixClass + collectionItem.TranslatedName;
          dataIntfName  := dataTypeName;
        end else
        begin
          dataTypeName  := PrefixInterface + collectionItem.TranslatedName;
          dataClassName := PrefixClass + collectionItem.TranslatedName;
          dataIntfName  := dataTypeName;
        end;
      end;
  end;

  sourceCode    := TNamedFormatStringList.Create();
  try
    case ASection of
      dxsInterface,
      dxsClass:
        begin
          sourceCode.Add('    function Get_%<ItemName>:s(Index: Integer): %<DataType>:s;');

          case AItem.CollectionItem.PropertyType of
            ptSimple:
              begin
                sourceCode.Add('    function Add(%<ItemName>:s: %<DataType>:s): %<DataInterface>:s;');
                sourceCode.Add('    function Insert(Index: Integer; %<ItemName>:s: %<DataType>:s): %<DataInterface>:s;');
              end;

            ptItem:
              begin
                sourceCode.Add('    function Add: %<DataType>:s;');
                sourceCode.Add('    function Insert(Index: Integer): %<DataType>:s;');
              end;
          end;
        end;

      dxsImplementation:
        begin
          case AItem.CollectionItem.PropertyType of
            ptSimple:
              begin
                typeDef := TXMLDataBindingSimpleProperty(AItem.CollectionItem).DataType;

                sourceCode.Add('function TXML%<Name>:s.Get_%<ItemName>:s(Index: Integer): %<DataType>:s;');

                if GetDataTypeMapping(typeDef, typeMapping) and (typeMapping.Conversion = tcString) then
                  sourceCode.Add(XMLToNativeDataType('Result', 'List[Index].Text', typeDef, dntCustom))
                else
                  sourceCode.Add(XMLToNativeDataType('Result', 'List[Index].NodeValue', typeDef, dntCustom));

                sourceCode.AddLn;

                sourceCode.Add('function TXML%<Name>:s.Add(%<ItemName>:s: %<DataType>:s): %<DataInterface>:s;');
                sourceCode.Add(NativeDataTypeToXML('Result.NodeValue', '%<ItemName>:s', typeDef, dntCustom,
                                                   '  Result := AddItem(-1);'));
                sourceCode.AddLn;

                sourceCode.Add('function TXML%<Name>:s.Insert(Index: Integer; %<ItemName>:s: %<DataType>:s): %<DataInterface>:s;');
                sourceCode.Add(NativeDataTypeToXML('Result.NodeValue', '%<ItemName>:s', typeDef, dntCustom,
                                                   '  Result := AddItem(Index);'));
                sourceCode.AddLn;
              end;


            ptItem:
              begin
                sourceCode.Add('function TXML%<Name>:s.Get_%<ItemName>:s(Index: Integer): %<DataType>:s;');
                sourceCode.Add('begin');
                sourceCode.Add('  Result := (List[Index] as %<DataType>:s);');
                sourceCode.Add('end;');
                sourceCode.AddLn;

                sourceCode.Add('function TXML%<Name>:s.Add: %<DataType>:s;');
                sourceCode.Add('begin');
                sourceCode.Add('  Result := (AddItem(-1) as %<DataType>:s);');
                sourceCode.Add('end;');
                sourceCode.AddLn;

                sourceCode.Add('function TXML%<Name>:s.Insert(Index: Integer): %<DataType>:s;');
                sourceCode.Add('begin');
                sourceCode.Add('  Result := (AddItem(Index) as %<DataType>:s);');
                sourceCode.Add('end;');
                sourceCode.AddLn;
              end;
          end;
        end;
    end;

    if ASection = dxsInterface then
    begin
      sourceCode.AddLn;
      sourceCode.Add('    property %<ItemName>:s[Index: Integer]: %<DataType>:s read Get_%<ItemName>:s; default;');
    end;

    Result := (sourceCode.Count > 0);

    if Result then
      AStream.Write(sourceCode.Format(['Name',            AItem.TranslatedName,
                                       'ItemName',        AItem.CollectionItem.TranslatedName,
                                       'ItemSourceName',  AItem.CollectionItem.Name,
                                       'DataType',        dataTypeName,
                                       'DataClass',       dataClassName,
                                       'DataInterface',   dataIntfName]));
  finally
    FreeAndNil(sourceCode);
  end;
end;


function TDelphiXMLDataBindingGenerator.WriteSchemaInterfaceProperty(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; AProperty: TXMLDataBindingProperty; ASection: TDelphiXMLSection; AMember: TDelphiXMLMember; ANewLine: Boolean): Boolean;

  procedure WriteNewLine;
  begin
    if ANewLine then
      AStream.WriteLn;
  end;


var
  sourceCode:       TNamedFormatStringList;
  writeOptional:    Boolean;
  writeNil:         Boolean;
  writeTextProp:    Boolean;
  propertyItem:     TXMLDataBindingItem;
  dataTypeName:     String;
  value:            String;
  propertyItemName: String;
  fieldName:        String;

begin
  Result  := False;

  if AProperty = AItem.CollectionItem then
    Exit;

  { If the property has a collection, it's Count property will be enough
    to check if an item is present, no need to write a HasX method. }
  // #ToDo3 (MvR) 14-4-2008: move first check to XMLDataBindingGenerator ?
  writeOptional := False;
  writeNil      := AProperty.IsNillable;

  if AMember in [dxmPropertyGet, dxmPropertyDeclaration] then
    writeOptional := not Assigned(AProperty.Collection) and
                     AProperty.IsOptional;


  dataTypeName  := '';
  propertyItem  := nil;
  fieldName     := '';

  { Get data type }
  writeTextProp := False;

  if Assigned(AProperty.Collection) then
  begin
    dataTypeName  := PrefixInterface + AProperty.Collection.TranslatedName;
    fieldName     := PrefixField + AProperty.TranslatedName;
  end else
  begin
    case AProperty.PropertyType of
      ptSimple:
        dataTypeName  := TranslateDataType(TXMLDataBindingSimpleProperty(AProperty).DataType);

      ptItem:
        begin
          propertyItem  := TXMLDataBindingItemProperty(AProperty).Item;
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
  end;


  if Length(dataTypeName) = 0 then
    Exit;


  sourceCode  := TNamedFormatStringList.Create();
  try
    case ASection of
      dxsInterface,
      dxsClass:
        begin
          { Interface declaration }
          case AMember of
            dxmPropertyGet:
              begin
                WriteNewLine;

                if writeOptional then
                  sourceCode.Add(PropertyIntfMethodGetOptional);

                if writeNil then
                  sourceCode.Add(PropertyIntfMethodGetNil);

                if writeTextProp then
                  sourceCode.Add(PropertyIntfMethodGetText);

                sourceCode.Add(PropertyIntfMethodGet);
              end;

            dxmPropertySet:
              if not AProperty.IsReadOnly then
              begin
                WriteNewLine;

                if writeNil then
                  sourceCode.Add(PropertyIntfMethodSetNil);

                if writeTextProp then
                  sourceCode.Add(PropertyIntfMethodSetText);

                sourceCode.Add(PropertyIntfMethodSet);
              end;

            dxmPropertyDeclaration:
              if ASection = dxsInterface then
              begin
                WriteNewLine;

                if writeOptional then
                  sourceCode.Add(PropertyInterfaceOptional);

                if AProperty.IsReadOnly then
                begin
                  if writeNil then
                    sourceCode.Add(PropertyInterfaceNilReadOnly);

                  if writeTextProp then
                    sourceCode.Add(PropertyInterfaceTextReadOnly);

                  sourceCode.Add(PropertyInterfaceReadOnly);
                end else
                begin
                  if writeNil then
                    sourceCode.Add(PropertyInterfaceNil);

                  if writeTextProp then
                    sourceCode.Add(PropertyInterfaceText);

                  sourceCode.Add(PropertyInterface);
                end;
              end;
          end;
        end;
      dxsImplementation:
        begin
          { Implementation }
          case AMember of
            dxmPropertyGet:
              begin
                WriteNewLine;

                // #ToDo1 (MvR) 21-4-2008: optional attributes!
                if writeOptional then
                  sourceCode.Add(PropertyImplMethodGetOptional);

                if writeNil then
                  sourceCode.Add(PropertyImplMethodGetNil);

                if writeTextProp then
                  sourceCode.Add(PropertyImplMethodGetText);

                sourceCode.Add('function TXML%<Name>:s.Get%<PropertyName>:s: %<DataType>:s;');

                case AProperty.PropertyType of
                  ptSimple:
                    sourceCode.Add(XMLToNativeDataType('Result',
                                                       '%<PropertySourceName>:s',
                                                       TXMLDataBindingSimpleProperty(AProperty).DataType,
                                                       GetDelphiNodeType(AProperty)));

                  ptItem:
                    begin
                      if Assigned(AProperty.Collection) then
                      begin
                        sourceCode.Add('begin');
                        sourceCode.Add('  Result := %<FieldName>:s;');
                        sourceCode.Add('end;');
                      end else
                      begin
                        if Assigned(propertyItem) then
                        begin
                          case propertyItem.ItemType of
                            itInterface:
                              begin
                                sourceCode.Add('begin');
                                sourceCode.Add('  Result := (ChildNodes[''%<PropertySourceName>:s''] as IXML%<PropertyItemName>:s);');
                                sourceCode.Add('end;');
                              end;

                            itEnumeration:
                              begin
                                sourceCode.Add('begin');
                                sourceCode.Add('  Result := StringTo%<PropertyItemName>:s(Get%<PropertyName>:sText);');
                                sourceCode.Add('end;');
                              end;
                          end;
                        end;
                      end;
                    end;
                end;

                sourceCode.AddLn;
              end;
            dxmPropertySet:
              if not AProperty.IsReadOnly then
              begin
                WriteNewLine;

                if writeNil then
                  sourceCode.Add(PropertyImplMethodSetNil);

                if writeTextProp then
                  sourceCode.Add(PropertyImplMethodSetText);

                sourceCode.Add('procedure TXML%<Name>:s.Set%<PropertyName>:s(const Value: %<DataType>:s);');
                value := '%<PropertySourceName>:s';

                if Assigned(propertyItem) and (propertyItem.ItemType = itEnumeration) then
                begin
                  sourceCode.Add(NativeDataTypeToXML(value, '%<PropertyItemName>:sValues[Value]', nil,
                                                     GetDelphiNodeType(AProperty))); 
                end else
                begin
                  if AProperty.PropertyType <> ptSimple then
                    raise Exception.Create('Setter must be a simple type');

                  sourceCode.Add(NativeDataTypeToXML(value, 'Value',
                                                     TXMLDataBindingSimpleProperty(AProperty).DataType,
                                                     GetDelphiNodeType(AProperty)));
                end;

                sourceCode.AddLn;
              end;
          end;
        end;
    end;

    propertyItemName  := '';
    if Assigned(propertyItem) then
      propertyItemName  := propertyItem.TranslatedName;

    Result := (sourceCode.Count > 0);
    if Result then
      AStream.Write(sourceCode.Format(['Name',                AItem.TranslatedName,
                                       'PropertySourceName',  AProperty.Name,
                                       'PropertyName',        AProperty.TranslatedName,
                                       'PropertyItemName',    propertyItemName,
                                       'DataType',            dataTypeName,
                                       'FieldName',           fieldName]));
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

  enumStart := NamedFormat('  TXML%<Name>:s = (',
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

  enumStart := NamedFormat('  %<Name>:sValues: ', ['Name', AItem.TranslatedName]);
  AStream.WriteLn(enumStart + NamedFormat('array[TXML%<Name>:s] of WideString =',
                                          ['Name',  AItem.TranslatedName]));

  lineIndent := StringOfChar(' ', Length(enumStart));

  AStream.WriteLn(lineIndent + '(');

  for memberIndex := 0 to Pred(AItem.MemberCount) do
  begin
    AStream.Write(NamedFormat('%<Indent>:s  ''%<Name>:s''',
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


function TDelphiXMLDataBindingGenerator.GetDelphiNodeType(AProperty: TXMLDataBindingProperty): TDelphiNodeType;
begin
  if AProperty.IsAttribute then
    Result := dntAttribute
  else
    Result := dntElement;
end;


function TDelphiXMLDataBindingGenerator.DataTypeConversion(const ADestination, ASource: String; ADataType: IXMLTypeDef; AAccessor: TDelphiAccessor; ANodeType: TDelphiNodeType; const ALinesBefore: String = ''): String;
var
  typeMapping:  TTypeMapping;
  conversion:   String;

begin
  with TNamedFormatStringList.Create() do
  try
    if not (Assigned(ADataType) and GetDataTypeMapping(ADataType, typeMapping)) then
      typeMapping.Conversion  := tcNone;


    Add('begin');

    if Length(ALinesBefore) > 0 then
      Add(ALinesBefore);


    conversion  := TypeConversion[AAccessor, ANodeType, typeMapping.Conversion];
    if Length(conversion) = 0 then
      conversion  := TypeConversionNone[AAccessor, ANodeType];


    Add(conversion);
    Add('end;');

    Result := Trim(Format(['Destination', ADestination,
                           'Source',      ASource]));
  finally
    Free();
  end;
end;


function TDelphiXMLDataBindingGenerator.XMLToNativeDataType(const ADestination, ASource: String; ADataType: IXMLTypeDef; ANodeType: TDelphiNodeType; const ALinesBefore: String): String;
begin
  Result := DataTypeConversion(ADestination, ASource, ADataType, daGet, ANodeType, ALinesBefore);
end;


function TDelphiXMLDataBindingGenerator.NativeDataTypeToXML(const ADestination, ASource: String; ADataType: IXMLTypeDef; ANodeType: TDelphiNodeType; const ALinesBefore: String): String;
begin
  Result := DataTypeConversion(ADestination, ASource, ADataType, daSet, ANodeType, ALinesBefore);
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
var
  path:       String;
  fileName:   String;
begin
  Result  := OutputPath;

  if OutputType = otMultiple then
  begin
    path      := IncludeTrailingPathDelimiter(Result);
    fileName  := ASchemaName + '.pas';

    if Assigned(FOnGetFileName) then
      FOnGetFileName(Self, ASchemaName, path, fileName);

    Result  := IncludeTrailingPathDelimiter(path) + fileName;
  end;
end;


{ TXMLSchemaList }
constructor TXMLSchemaList.Create();
begin
  inherited Create(False);
end;


function TXMLSchemaList.GetItem(Index: Integer): TXMLDataBindingSchema;
begin
  Result := TXMLDataBindingSchema(inherited GetItem(Index));
end;


procedure TXMLSchemaList.SetItem(Index: Integer; const Value: TXMLDataBindingSchema);
begin
  inherited SetItem(Index, Value);
end;

end.



