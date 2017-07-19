unit DelphiXMLDataBindingGenerator;

interface
uses
  System.Classes,
  System.Generics.Collections,
  Xml.XMLSchema,

  DelphiXMLDataBindingResources,
  XMLDataBindingGenerator,
  XMLDataBindingHelpers;


type
  TGetFileNameEvent = procedure(Sender: TObject; const SchemaName: String; var Path, FileName: String) of object;

  TXMLSchemaList = TList<TXMLDataBindingSchema>;

  TDelphiXMLDataBindingGenerator = class(TXMLDataBindingGenerator)
  private
    FProcessedItems:  TList<TXMLDataBindingInterface>;
    FUnitNames:       TDictionary<TXMLDataBindingSchema, String>;

    FOnGetFileName:   TGetFileNameEvent;
  protected
    procedure GenerateDataBinding; override;
    procedure GenerateOutputFile(ASchemaList: TXMLSchemaList; const ASourceFileName, AUnitName: String);
    function GenerateUsesClause(ASchemaList: TXMLSchemaList): String;

    function DelphiSafeName(const AName: String): String;
    function TranslateItemName(AItem: TXMLDataBindingItem): String; override;
    procedure PostProcessItem(ASchema: TXMLDataBindingSchema; AItem: TXMLDataBindingItem); override;
    procedure ResolvePropertyNameConflicts(AItem: TXMLDataBindingInterface);

    function DoGetFileName(const ASchemaName: String): String;

    function GetDataTypeMapping(ADataType: IXMLTypeDef; out ATypeMapping: TTypeMapping): Boolean;
    function GetDataTypeName(AProperty: TXMLDataBindingProperty; AInterfaceName: Boolean): String;
    function TranslateDataType(ADataType: IXMLTypeDef): String;
    function CreateNewGUID: String;

    procedure WriteUnitHeader(AWriter: TNamedFormatWriter; const ASourceFileName, AFileName: String);
    procedure WriteSection(AWriter: TNamedFormatWriter; ASection: TDelphiXMLSection; ASchemaList: TXMLSchemaList);
    procedure WriteDocumentFunctions(AWriter: TNamedFormatWriter; ASection: TDelphiXMLSection; ASchemaList: TXMLSchemaList);
    procedure WriteEnumerationConversions(AWriter: TNamedFormatWriter; ASection: TDelphiXMLSection; ASchemaList: TXMLSchemaList);
    procedure WriteImplementationUses(AWriter: TNamedFormatWriter; ASchemaList: TXMLSchemaList);
    procedure WriteDocumentation(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingItem);
    procedure WriteAfterConstruction(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
    function WriteInlineCollectionFields(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface): Boolean;

    procedure WriteSchemaItem(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingItem; ASection: TDelphiXMLSection);
    procedure WriteSchemaInterface(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
    procedure WriteSchemaInterfaceProperties(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
    function WriteSchemaInterfaceCollectionProperties(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection): Boolean;
    function WriteSchemaInterfaceProperty(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; AProperty: TXMLDataBindingProperty; ASection: TDelphiXMLSection; AMember: TDelphiXMLMember; ANewLine: Boolean): Boolean;
    procedure WriteSchemaEnumeration(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingEnumeration; ASection: TDelphiXMLSection);
    procedure WriteSchemaEnumerationArray(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingEnumeration);

    procedure WriteValidate(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
    procedure WriteValidateImplementation(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; AStrict: Boolean);
    procedure WriteEnumeratorMethod(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
    procedure WriteEnumerator(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);

    function GetDelphiNodeType(AProperty: TXMLDataBindingProperty): TDelphiNodeType;
    function GetDelphiElementType(ANodeType: TDelphiNodeType): TDelphiElementType;
    function DataTypeConversion(const ADestination, ASource: String; ADataType: IXMLTypeDef; AAccessor: TDelphiAccessor; ANodeType: TDelphiNodeType; const ATargetNamespace: string; const ALinesBefore: String = ''): String;
    function XMLToNativeDataType(const ADestination, ASource: String; ADataType: IXMLTypeDef; ANodeType: TDelphiNodeType; const ATargetNamespace: string; const ALinesBefore: String = ''): String;
    function NativeDataTypeToXML(const ADestination, ASource: String; ADataType: IXMLTypeDef; ANodeType: TDelphiNodeType; const ATargetNamespace: string; const ALinesBefore: String = ''): String;

    property ProcessedItems:  TList<TXMLDataBindingInterface> read FProcessedItems;
    property UnitNames:       TDictionary<TXMLDataBindingSchema, String> read FUnitNames;
  public
    property OnGetFileName:   TGetFileNameEvent read FOnGetFileName write FOnGetFileName;
  end;


implementation
uses
  StrUtils,
  SysUtils,

  X2UtNamedFormat,

  X2Log.Global;



{ TDelphiXMLDataBindingGenerator }
procedure TDelphiXMLDataBindingGenerator.GenerateDataBinding;
var
  schemaList:   TXMLSchemaList;
  schemaIndex:  Integer;
  schema:       TXMLDataBindingSchema;
  unitName:     String;
  
begin
  schemaList  := TXMLSchemaList.Create;
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
          FUnitNames  := TDictionary<TXMLDataBindingSchema, String>.Create;
          try
            for schemaIndex := 0 to Pred(SchemaCount) do
            begin
              schema  := Schemas[schemaIndex];
              FUnitNames.Add(schema, DoGetFileName(schema.SchemaName));
            end;

            for schemaIndex := 0 to Pred(SchemaCount) do
            begin
              schema  := Schemas[schemaIndex];

              schemaList.Clear;
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
  unitWriter: TNamedFormatWriter;
  usesClause: String;

begin
  usesClause  := '';

  if OutputType = otMultiple then
    usesClause  := GenerateUsesClause(ASchemaList);

  unitWriter := TNamedFormatWriter.Create(AUnitName, False, TEncoding.ANSI);
  try
    WriteUnitHeader(unitWriter, ASourceFileName, AUnitName);

    unitWriter.WriteNamedFmt(UnitInterface,
                             ['UsesClause', usesClause]);
    WriteSection(unitWriter, dxsForward, ASchemaList);

    FProcessedItems := TList<TXMLDataBindingInterface>.Create;
    try
      FProcessedItems.Clear;
      WriteSection(unitWriter, dxsInterface, ASchemaList);

      FProcessedItems.Clear;
      WriteSection(unitWriter, dxsClass, ASchemaList);
    finally
      FreeAndNil(FProcessedItems);
    end;

    WriteDocumentFunctions(unitWriter, dxsInterface, ASchemaList);
    WriteEnumerationConversions(unitWriter, dxsInterface, ASchemaList);

    unitWriter.Write(UnitImplementation);
    WriteImplementationUses(unitWriter, ASchemaList);
    WriteDocumentFunctions(unitWriter, dxsImplementation, ASchemaList);
    WriteEnumerationConversions(unitWriter, dxsImplementation, ASchemaList);

    WriteSection(unitWriter, dxsImplementation, ASchemaList);

    unitWriter.Write(unitFooter);
  finally
    FreeAndNil(unitWriter);
  end;
end;


function TDelphiXMLDataBindingGenerator.GenerateUsesClause(ASchemaList: TXMLSchemaList): String;
var
  includedSchemas:    TList<TXMLDataBindingSchema>;

  procedure AddSchema(ASchema: TXMLDataBindingSchema);
  begin
    if Assigned(ASchema) and
       (not includedSchemas.Contains(ASchema)) and
       (not ASchemaList.Contains(ASchema)) then
      includedSchemas.Add(ASchema);
  end;
  

var
  schemaIndex:        Integer;
  schema:             TXMLDataBindingSchema;
  itemIndex:          Integer;
  interfaceItem:      TXMLDataBindingInterface;
  propertyIndex:      Integer;
  propertyItem:       TXMLDataBindingProperty;

begin
  Result := '';

  includedSchemas := TList<TXMLDataBindingSchema>.Create;
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
      for schema in includedSchemas do
        Result  := Result + '  ' + ChangeFileExt(ExtractFileName(FUnitNames[schema]), '') + ',' + CrLf;

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
      if AProperty.IsRepeating then
      begin
        if AInterfaceName then
          Result := ItemInterface
        else
          Result := ItemClass;
      end else
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
    if not CharInSet(Result[charIndex], SafeChars) then
      Delete(Result, charIndex, 1);
  end;


  if Length(Result) > 0 then
  begin
    { Number as the first character is not allowed }
    if CharInSet(Result[1], ['0'..'9']) then
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
      Result := DelphiSafeName(TXMLDataBindingEnumerationMember(AItem).Enumeration.TranslatedName) + '_' + Result;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.PostProcessItem(ASchema: TXMLDataBindingSchema; AItem: TXMLDataBindingItem);
begin
  inherited PostProcessItem(ASchema, AItem);

  if AItem.ItemType = itInterface then
  begin
    { Resolve conflicts in case only for properties }
    ResolvePropertyNameConflicts(TXMLDataBindingInterface(AItem));
  end;
end;


procedure TDelphiXMLDataBindingGenerator.ResolvePropertyNameConflicts(AItem: TXMLDataBindingInterface);
var
  propertyNames:  TStringList;
  propertyItem:   TXMLDataBindingProperty;
  propertyIndex:  Integer;
  baseName:       String;
  counter:        Integer;

begin
  propertyNames := TStringList.Create;
  try
    propertyNames.CaseSensitive := False;

    for propertyIndex := 0 to Pred(AItem.PropertyCount) do
    begin
      propertyItem := AItem.Properties[propertyIndex];

      baseName := propertyItem.TranslatedName;
      counter := 1;

      while propertyNames.IndexOf(propertyItem.TranslatedName) > -1 do
      begin
        { Unfortunately, the context is exactly the same, this is the best we can do }
        Inc(counter);
        propertyItem.TranslatedName := baseName + IntToStr(counter);
      end;

      propertyNames.Add(propertyItem.TranslatedName);
    end;
  finally
    FreeAndNil(propertyNames);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteUnitHeader(AWriter: TNamedFormatWriter; const ASourceFileName, AFileName: String);
begin
  AWriter.WriteNamedFmt(UnitHeader,
                        ['SourceFileName',  ASourceFileName,
                         'UnitName',        ChangeFileExt(ExtractFileName(AFileName), ''),
                         'DateTime',        DateTimeToStr(Now)]);
end;


procedure TDelphiXMLDataBindingGenerator.WriteSection(AWriter: TNamedFormatWriter; ASection: TDelphiXMLSection; ASchemaList: TXMLSchemaList);
var
  schemaIndex:  Integer;
  schema:       TXMLDataBindingSchema;
  itemIndex:    Integer;

begin
  for schemaIndex := 0 to Pred(ASchemaList.Count) do
  begin
    schema := ASchemaList[schemaIndex];
    AWriter.WriteLineNamedFmt(SectionComments[ASection],
                              ['SchemaName',  schema.SchemaName]);

    for itemIndex := 0 to Pred(schema.ItemCount) do
      WriteSchemaItem(AWriter, schema.Items[itemIndex], ASection);

    AWriter.WriteLine;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteDocumentFunctions(AWriter: TNamedFormatWriter; ASection: TDelphiXMLSection; ASchemaList: TXMLSchemaList);
var
  schemaIndex:      Integer;
  schema:           TXMLDataBindingSchema;
  itemIndex:        Integer;
  item:             TXMLDataBindingItem;
  interfaceItem:    TXMLDataBindingInterface;
  hasItem:          Boolean;
  nameSpace:        String;

begin
  hasItem   := False;
  nameSpace := '';

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
              AWriter.Write('  ');

            AWriter.WriteLine('{ Document functions }');
            hasItem := True;
          end;

          if Length(schema.TargetNamespace) > 0 then
            nameSpace := schema.TargetNamespace;

          with TNamedFormatStringList.Create do
          try
            case ASection of
              dxsInterface:         Add(DocumentFunctionsInterface);
              dxsImplementation:    Add(DocumentFunctionsImplementation);
            end;

            AWriter.Write(Format(['SourceName', interfaceItem.Name,
                                  'Name',       interfaceItem.TranslatedName]));
          finally
            Free;
          end;

          AWriter.WriteLine;
        end;
      end;
    end;
  end;

  if (ASection = dxsInterface) and hasItem then
  begin
    AWriter.WriteLine('const');
    AWriter.WriteLine('  TargetNamespace = ''%s'';', [nameSpace]);
    AWriter.WriteLine;
    AWriter.WriteLine;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteEnumerationConversions(AWriter: TNamedFormatWriter; ASection: TDelphiXMLSection; ASchemaList: TXMLSchemaList);
var
  enumerations:     TList<TXMLDataBindingItem>;
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


  enumerations  := TList<TXMLDataBindingItem>.Create;
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
        AWriter.WriteLine('const');

        for itemIndex := 0 to Pred(enumerations.Count) do
          WriteSchemaEnumerationArray(AWriter, TXMLDataBindingEnumeration(enumerations[itemIndex]));
      end;


      { Conversion helpers }
      if ASection = dxsInterface then
        AWriter.Write('  ');
        
      AWriter.WriteLine('{ Enumeration conversion helpers }');
      

      for itemIndex := Pred(enumerations.Count) downto 0 do
      begin
        enumerationItem := TXMLDataBindingEnumeration(enumerations[itemIndex]);

        indent  := '';
        if ASection = dxsInterface then
          indent  := '  ';

        sourceCode  := TNamedFormatStringList.Create;
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

          AWriter.Write(sourceCode.Format(['ItemName',  enumerationItem.TranslatedName,
                                           'DataType',  PrefixClass + enumerationItem.TranslatedName]));
        finally
          FreeAndNil(sourceCode);
        end;
      end;

      AWriter.WriteLine;
    end;
  finally
    FreeAndNil(enumerations);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteImplementationUses(AWriter: TNamedFormatWriter; ASchemaList: TXMLSchemaList);
begin
  { In ye olde days this is where we checked if XMLDataBindingUtils was required. With the
    introduction of the IXSDValidate, this is practically always the case. }
  AWriter.WriteLine('uses');
  AWriter.WriteLine('  Variants;');
  AWriter.WriteLine;
end;


procedure TDelphiXMLDataBindingGenerator.WriteDocumentation(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingItem);
var
  documentation:    String;
  lineIndex:        Integer;
  lines:            TStringList;

begin
  if not AItem.HasDocumentation then
    exit;

  lines := TStringList.Create;
  try
    documentation := AItem.Documentation;

    { Replace dangerous characters }
    documentation := StringReplace(documentation, '{', '(', [rfReplaceAll]);
    documentation := StringReplace(documentation, '}', ')', [rfReplaceAll]);

    lines.Text    := WrapText(documentation, 76);

    AWriter.WriteLine('  {');
    for lineIndex := 0 to Pred(lines.Count) do
      AWriter.WriteLine('    ' + lines[lineIndex]);

    AWriter.WriteLine('  }');
  finally
    FreeAndNil(lines);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaItem(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingItem; ASection: TDelphiXMLSection);
begin
  case AItem.ItemType of
    itInterface:    WriteSchemaInterface(AWriter, TXMLDataBindingInterface(AItem), ASection);
    itEnumeration:  WriteSchemaEnumeration(AWriter, TXMLDataBindingEnumeration(AItem), ASection);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaInterface(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
var
  parent:     String;

begin
  if ASection in [dxsInterface, dxsClass] then
  begin
    { Ensure the base item is completely defined first, Delphi doesn't allow
      inheritance with just a forward declaration. }
    if ProcessedItems.Contains(AItem) then
      exit;

    if Assigned(AItem.BaseItem) then
      WriteSchemaInterface(AWriter, AItem.BaseItem, ASection);

    ProcessedItems.Add(AItem);
  end;


  case ASection of
    dxsForward:
      AWriter.WriteLineNamedFmt(InterfaceItemForward,
                                ['Name',  AItem.TranslatedName]);

    dxsInterface:
      begin
        if Assigned(AItem.BaseItem) then
          parent  := PrefixInterface + AItem.BaseItem.TranslatedName
        else if AItem.IsCollection then
        begin
          parent  := CollectionInterface;
          WriteEnumerator(AWriter, AItem, ASection);
        end else
          parent  := ItemInterface;


        WriteDocumentation(AWriter, AItem);
        AWriter.WriteLineNamedFmt(InterfaceItemInterface,
                                  ['Name',        AItem.TranslatedName,
                                   'ParentName',  parent]);
        AWriter.WriteLine('    ' + CreateNewGUID);

        WriteSchemaInterfaceProperties(AWriter, AItem, ASection);

        AWriter.WriteLine('  end;');
        AWriter.WriteLine;
      end;

    dxsClass:
      begin
        if Assigned(AItem.BaseItem) then
          parent  := PrefixClass + AItem.BaseItem.TranslatedName
        else if AItem.IsCollection then
        begin
          parent  := CollectionClass;
          WriteEnumerator(AWriter, AItem, ASection);
        end else
          parent  := ItemClass;


        if AItem.CanValidate then
          parent := parent + ', ' + XSDValidateInterface;
          

        AWriter.WriteLineNamedFmt(InterfaceItemClass,
                                  ['Name',        AItem.TranslatedName,
                                   'ParentName',  parent]);

        WriteSchemaInterfaceProperties(AWriter, AItem, ASection);

        AWriter.WriteLine('  end;');
        AWriter.WriteLine;
      end;

    dxsImplementation:
      begin
        WriteEnumerator(AWriter, AItem, ASection);
        WriteSchemaInterfaceProperties(AWriter, AItem, ASection);
      end;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteAfterConstruction(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
var
  hasPrototype:       Boolean;


  procedure WritePrototype;
  begin
    if not hasPrototype then
    begin
      case ASection of
        dxsClass:
          begin
            AWriter.WriteLine('  public');
            AWriter.WriteLine('    procedure AfterConstruction; override;');
          end;

        dxsImplementation:
          begin
            AWriter.WriteLine('procedure TXML%s.AfterConstruction;', [AItem.TranslatedName]);
            AWriter.WriteLine('begin');
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
    WriteInlineCollectionFields(AWriter, AItem);


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
        if propertyItem.PropertyType = ptItem then
        begin
          if propertyItem.HasTargetNamespace then
            AWriter.WriteLineNamedFmt('  RegisterChildNode(''%<ItemSourceName>:s'', %<ItemClass>:s, ''%<Namespace>:s'');',
                                      ['ItemSourceName',      propertyItem.Name,
                                       'ItemClass',           GetDataTypeName(propertyItem, False),
                                       'Namespace',           propertyItem.TargetNamespace])
          else
            AWriter.WriteLineNamedFmt('  RegisterChildNode(''%<ItemSourceName>:s'', %<ItemClass>:s);',
                                      ['ItemSourceName',      propertyItem.Name,
                                       'ItemClass',           GetDataTypeName(propertyItem, False)]);
        end;

        AWriter.WriteLineNamedFmt('  %<FieldName>:s := CreateCollection(%<CollectionClass>:s, %<ItemInterface>:s, ''%<ItemSourceName>:s'') as %<CollectionInterface>:s;',
                                  ['FieldName',           PrefixField + propertyItem.TranslatedName,
                                   'CollectionClass',     PrefixClass + propertyItem.Collection.TranslatedName,
                                   'CollectionInterface', PrefixInterface + propertyItem.Collection.TranslatedName,
                                   'ItemInterface',       GetDataTypeName(propertyItem, True),
                                   'ItemSourceName',      propertyItem.Name]);
      end;
    end;

    if propertyItem.PropertyType = ptItem then
    begin
      itemProperty  := TXMLDataBindingItemProperty(propertyItem);

      if (not AItem.IsCollection) or
         (propertyItem <> AItem.CollectionItem) then
      begin
        { Item property }
        if Assigned(itemProperty.Item) and
           (itemProperty.Item.ItemType = itInterface) then
        begin
          case ASection of
            dxsClass:
              WritePrototype;

            dxsImplementation:
              begin
                WritePrototype;

                if propertyItem.HasTargetNamespace then
                  AWriter.WriteLineNamedFmt('  RegisterChildNode(''%<SourceName>:s'', TXML%<Name>:s, ''%<Namespace>:s'');',
                                            ['SourceName', propertyItem.Name,
                                             'Name',       itemProperty.Item.TranslatedName,
                                             'Namespace',  propertyItem.TargetNamespace])
                else
                  AWriter.WriteLineNamedFmt('  RegisterChildNode(''%<SourceName>:s'', TXML%<Name>:s);',
                                            ['SourceName', propertyItem.Name,
                                             'Name',       itemProperty.Item.TranslatedName]);
              end;
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
      if AItem.CollectionItem.HasTargetNamespace then
        AWriter.WriteLineNamedFmt('  RegisterChildNode(''%<SourceName>:s'', %<DataClass>:s, ''%<Namespace>:s'');',
                                  ['SourceName',  AItem.CollectionItem.Name,
                                   'DataClass',   GetDataTypeName(AItem.CollectionItem, False),
                                   'Namespace',   AItem.CollectionItem.TargetNamespace])
      else
        AWriter.WriteLineNamedFmt('  RegisterChildNode(''%<SourceName>:s'', %<DataClass>:s);',
                                  ['SourceName',  AItem.CollectionItem.Name,
                                   'DataClass',   GetDataTypeName(AItem.CollectionItem, False)]);

      AWriter.WriteLine;
      AWriter.WriteLine('  ItemTag := ''%s'';', [AItem.CollectionItem.Name]);
      AWriter.WriteLine('  ItemInterface := %s;', [GetDataTypeName(AItem.CollectionItem, True)]);
      AWriter.WriteLine;
    end;
  end;

  if hasPrototype and (ASection = dxsImplementation) then
  begin
    AWriter.WriteLine('  inherited;');

    if AItem.IsCollection and (AItem.TargetNamespace <> AItem.CollectionItem.TargetNamespace) then
    begin
      AWriter.WriteLine;
      AWriter.WriteLine('  ItemNS := ''%s'';', [AItem.CollectionItem.TargetNamespace]);
    end;

    AWriter.WriteLine('end;');
    AWriter.WriteLine;
  end;
end;


function TDelphiXMLDataBindingGenerator.WriteInlineCollectionFields(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface): Boolean;
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
          AWriter.WriteLine('  private');
          Result  := True;
        end;

        AWriter.WriteLineNamedFmt('    %<PropertyName>:s: %<DataInterface>:s;',
                                  ['PropertyName',  PrefixField + collectionProperty.TranslatedName,
                                   'DataInterface', PrefixInterface + collectionProperty.Collection.TranslatedName]);
      end;
    end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaInterfaceProperties(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
var
  propertyIndex:    Integer;
  itemProperty:     TXMLDataBindingProperty;
  hasMembers:       Boolean;
  firstMember:      Boolean;
  member:           TDelphiXMLMember;

begin
  TX2GlobalLog.Verbose('WriteSchemaInterfaceProperties: ' + AItem.Name);

  if ASection = dxsForward then
    Exit;

  if ASection in [dxsClass, dxsImplementation] then
    WriteAfterConstruction(AWriter, AItem, ASection);

  if ASection = dxsClass then
    AWriter.WriteLine('  protected');

  WriteValidate(AWriter, AItem, ASection);
  WriteEnumeratorMethod(AWriter, AItem, ASection);
  hasMembers  := WriteSchemaInterfaceCollectionProperties(AWriter, AItem, ASection);

  for member := Low(TDelphiXMLMember) to High(TDelphiXMLMember) do
  begin
    firstMember   := True;

    for propertyIndex := 0 to Pred(AItem.PropertyCount) do
    begin
      itemProperty  := AItem.Properties[propertyIndex];

      if WriteSchemaInterfaceProperty(AWriter, AItem, itemProperty, ASection, member,
                                      hasMembers and firstMember and (ASection in [dxsInterface, dxsClass])) then
      begin
        firstMember := False;
        hasMembers  := True;
      end;
    end;
  end;
end;


function TDelphiXMLDataBindingGenerator.WriteSchemaInterfaceCollectionProperties(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection): Boolean;
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
        dataClassName := ItemClass;
        dataIntfName  := ItemInterface;
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

  sourceCode    := TNamedFormatStringList.Create;
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
                  sourceCode.Add(XMLToNativeDataType('Result', 'List[Index].Text', typeDef, dntCustom, AItem.CollectionItem.TargetNamespace))
                else
                  sourceCode.Add(XMLToNativeDataType('Result', 'List[Index].NodeValue', typeDef, dntCustom, AItem.CollectionItem.TargetNamespace));

                sourceCode.AddLn;

                sourceCode.Add('function TXML%<Name>:s.Add(%<ItemName>:s: %<DataType>:s): %<DataInterface>:s;');
                sourceCode.Add(NativeDataTypeToXML('Result.NodeValue', '%<ItemName>:s', typeDef, dntCustom, '',
                                                   '  Result := AddItem(-1);'));
                sourceCode.AddLn;

                sourceCode.Add('function TXML%<Name>:s.Insert(Index: Integer; %<ItemName>:s: %<DataType>:s): %<DataInterface>:s;');
                sourceCode.Add(NativeDataTypeToXML('Result.NodeValue', '%<ItemName>:s', typeDef, dntCustom, '',
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
      AWriter.Write(sourceCode.Format(['Name',            AItem.TranslatedName,
                                       'ItemName',        AItem.CollectionItem.TranslatedName,
                                       'ItemSourceName',  AItem.CollectionItem.Name,
                                       'DataType',        dataTypeName,
                                       'DataClass',       dataClassName,
                                       'DataInterface',   dataIntfName]));
  finally
    FreeAndNil(sourceCode);
  end;
end;


function TDelphiXMLDataBindingGenerator.WriteSchemaInterfaceProperty(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; AProperty: TXMLDataBindingProperty; ASection: TDelphiXMLSection; AMember: TDelphiXMLMember; ANewLine: Boolean): Boolean;

  procedure WriteNewLine;
  begin
    if ANewLine then
      AWriter.WriteLine;
  end;


  function IsReadOnly(AProperty: TXMLDataBindingProperty): Boolean;
  var
    typeMapping: TTypeMapping;

  begin
    if Assigned(AProperty.Collection) then
      exit(True);

    Result := AProperty.IsReadOnly;

    if (not Result) and (AProperty.PropertyType = ptSimple) then
    begin
      if GetDataTypeMapping(TXMLDataBindingSimpleProperty(AProperty).DataType, typeMapping) then
        Result := (typeMapping.Conversion = tcNode);
    end;
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
  writeStream:      Boolean;
  typeMapping:      TTypeMapping;
  nodeType:         TDelphiNodeType;

begin
  Result  := False;

  if AProperty = AItem.CollectionItem then
    Exit;

  { If the property has a collection, it's Count property will be enough
    to check if an item is present, no need to write a HasX method. }
  writeOptional := False;
  writeNil      := AProperty.IsNillable;

  if AMember in [dxmPropertyGet, dxmPropertyDeclaration] then
    writeOptional := not Assigned(AProperty.Collection) and
                     AProperty.IsOptional;

  writeStream   := False;
  if (AMember = dxmPropertyMethods) and (AProperty.PropertyType = ptSimple) then
  begin
    if GetDataTypeMapping(TXMLDataBindingSimpleProperty(AProperty).DataType, typeMapping) then
      writeStream := (typeMapping.Conversion = tcBase64);
  end;




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


  sourceCode  := TNamedFormatStringList.Create;
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
              if not IsReadOnly(AProperty) then
              begin
                WriteNewLine;

                if writeNil then
                  sourceCode.Add(PropertyIntfMethodSetNil);

                if writeTextProp then
                  sourceCode.Add(PropertyIntfMethodSetText);

                sourceCode.Add(PropertyIntfMethodSet);
              end;

            dxmPropertyMethods:
              if writeStream then
              begin
                sourceCode.Add(PropertyIntfMethodLoadFromStream);
                sourceCode.Add(PropertyIntfMethodLoadFromFile);
                sourceCode.Add(PropertyIntfMethodSaveToStream);
                sourceCode.Add(PropertyIntfMethodSaveToFile);
              end;

            dxmPropertyDeclaration:
              if ASection = dxsInterface then
              begin
                WriteNewLine;

                if writeOptional then
                  sourceCode.Add(PropertyInterfaceOptional);

                if IsReadOnly(AProperty) then
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
                nodeType := GetDelphiNodeType(AProperty);
                WriteNewLine;

                if writeOptional then
                  if AProperty.IsAttribute then
                    sourceCode.Add(PropertyImplMethodGetOptionalAttr)
                  else
                    sourceCode.Add(PropertyImplMethodGetOptional[GetDelphiElementType(nodeType)]);

                if writeNil then
                  sourceCode.Add(PropertyImplMethodGetNil[GetDelphiElementType(nodeType)]);

                if writeTextProp then
                  if AProperty.IsAttribute then
                    sourceCode.Add(PropertyImplMethodGetTextAttr)
                  else
                    sourceCode.Add(PropertyImplMethodGetText[GetDelphiElementType(nodeType)]);

                sourceCode.Add('function TXML%<Name>:s.Get%<PropertyName>:s: %<DataType>:s;');

                case AProperty.PropertyType of
                  ptSimple:
                    if Assigned(AProperty.Collection) then
                    begin
                      sourceCode.Add('begin');
                      sourceCode.Add('  Result := %<FieldName>:s;');
                      sourceCode.Add('end;');
                    end else
                      sourceCode.Add(XMLToNativeDataType('Result',
                                                         '%<PropertySourceName>:s',
                                                         TXMLDataBindingSimpleProperty(AProperty).DataType,
                                                         nodeType,
                                                         AProperty.TargetNamespace));

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

                                if AProperty.HasTargetNamespace then
                                  sourceCode.Add('  Result := (ChildNodesNS[''%<PropertySourceName>:s'', ''%<Namespace>:s''] as IXML%<PropertyItemName>:s);')
                                else
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
              if not IsReadOnly(AProperty) then
              begin
                nodeType := GetDelphiNodeType(AProperty);
                WriteNewLine;

                if writeNil then
                  sourceCode.Add(PropertyImplMethodSetNil[GetDelphiElementType(nodeType)]);

                if writeTextProp then
                  if AProperty.IsAttribute then
                    sourceCode.Add(PropertyImplMethodSetTextAttr)
                  else
                    sourceCode.Add(PropertyImplMethodSetText[GetDelphiElementType(nodeType)]);

                sourceCode.Add('procedure TXML%<Name>:s.Set%<PropertyName>:s(const Value: %<DataType>:s);');
                value := '%<PropertySourceName>:s';

                if Assigned(propertyItem) and (propertyItem.ItemType = itEnumeration) then
                begin
                  sourceCode.Add(NativeDataTypeToXML(value, '%<PropertyItemName>:sValues[Value]', nil,
                                                     nodeType,
                                                     AProperty.TargetNamespace)); 
                end else
                begin
                  if AProperty.PropertyType <> ptSimple then
                    raise Exception.Create('Setter must be a simple type');

                  sourceCode.Add(NativeDataTypeToXML(value, 'Value',
                                                     TXMLDataBindingSimpleProperty(AProperty).DataType,
                                                     nodeType,
                                                     AProperty.TargetNamespace));
                end;

                sourceCode.AddLn;
              end;

            dxmPropertyMethods:
              if writeStream then
              begin
                nodeType := GetDelphiElementType(GetDelphiNodeType(AProperty));
                sourceCode.Add(PropertyImplMethodLoadFromStream[nodeType]);
                sourceCode.Add(PropertyImplMethodLoadFromFile[nodeType]);
                sourceCode.Add(PropertyImplMethodSaveToStream[nodeType]);
                sourceCode.Add(PropertyImplMethodSaveToFile[nodeType]);
              end;
          end;
        end;
    end;

    propertyItemName  := '';
    if Assigned(propertyItem) then
      propertyItemName  := propertyItem.TranslatedName;

    Result := (sourceCode.Count > 0);
    if Result then
      AWriter.Write(sourceCode.Format(['Name',                AItem.TranslatedName,
                                       'PropertySourceName',  AProperty.Name,
                                       'PropertyName',        AProperty.TranslatedName,
                                       'PropertyItemName',    propertyItemName,
                                       'DataType',            dataTypeName,
                                       'FieldName',           fieldName,
                                       'Namespace',           AProperty.TargetNamespace]));
  finally
    FreeAndNil(sourceCode);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaEnumeration(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingEnumeration; ASection: TDelphiXMLSection);
var
  memberIndex: Integer;
  enumStart: String;
  lineIndent: String;

begin
  if (ASection <> dxsForward) or (AItem.MemberCount = 0) then
    exit;

  enumStart := NamedFormat('  TXML%<Name>:s = (',
                           ['Name', AItem.TranslatedName]);
  AWriter.Write(enumStart);
  lineIndent := StringOfChar(' ', Length(enumStart));

  for memberIndex := 0 to Pred(AItem.MemberCount) do
  begin
    if memberIndex > 0 then
      AWriter.Write(lineIndent);

    AWriter.Write(AItem.Members[memberIndex].TranslatedName);

    if memberIndex < Pred(AItem.MemberCount) then
      AWriter.WriteLine(',')
    else
      AWriter.WriteLine(');');
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteSchemaEnumerationArray(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingEnumeration);
var
  memberIndex: Integer;
  enumStart: String;
  lineIndent: String;

begin
  if (AItem.MemberCount = 0) then
    exit;

  enumStart := NamedFormat('  %<Name>:sValues: ', ['Name', AItem.TranslatedName]);
  AWriter.WriteLine(enumStart + NamedFormat('array[TXML%<Name>:s] of WideString =',
                                          ['Name',  AItem.TranslatedName]));

  lineIndent := StringOfChar(' ', Length(enumStart));

  AWriter.WriteLine(lineIndent + '(');

  for memberIndex := 0 to Pred(AItem.MemberCount) do
  begin
    AWriter.Write(NamedFormat('%<Indent>:s  ''%<Name>:s''',
                             ['Indent', lineIndent,
                              'Name',   AItem.Members[memberIndex].Name]));

    if memberIndex < Pred(AItem.MemberCount) then
      AWriter.WriteLine(',')
    else
      AWriter.WriteLine;
  end;

  AWriter.WriteLine(lineIndent + ');');
  AWriter.WriteLine;
end;


procedure TDelphiXMLDataBindingGenerator.WriteValidate(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
begin
  if AItem.DocumentElement then
  begin
    case ASection of
      dxsInterface,
      dxsClass:
        AWriter.WriteLine(XSDValidateDocumentMethodInterface);

      dxsImplementation:
        AWriter.WriteLineNamedFmt(XSDValidateDocumentMethodImplementation,
                                  ['Name', AItem.TranslatedName]);
    end;
  end;

  if AItem.CanValidate then
  begin
    case ASection of
      dxsInterface,
      dxsClass:
        begin
          AWriter.WriteLine(XSDValidateMethodInterface);
          AWriter.WriteLine('');
        end;

      dxsImplementation:
        begin
          WriteValidateImplementation(AWriter, AItem, False);
          WriteValidateImplementation(AWriter, AItem, True);
        end;
    end;
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteValidateImplementation(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; AStrict: Boolean);

  procedure AddArrayElement(var AOutput: string; var ACount: Integer; const AValue: string);
  begin
    AOutput := AOutput + ', ';

    { Prevent "Line too long" on large elements }
    if (ACount > 0) and (ACount mod 5 = 0) then
      AOutput := AOutput + XSDValidateMethodImplementationArrayBreak;

    AOutput := AOutput + AValue;
    Inc(ACount);
  end;


var
  propertyIndex: Integer;
  propertyItem: TXMLDataBindingProperty;
  elementSortCount: Integer;
  elementSortOrder: string;
  elementRequired: string;
  elementRequiredCount: Integer;
  attributeRequired: string;
  attributeRequiredCount: Integer;

begin
  AWriter.WriteLineNamedFmt(IfThen(AStrict, XSDValidateStrictMethodImplementationBegin, XSDValidateMethodImplementationBegin),
                            ['Name', AItem.TranslatedName]);

  elementSortCount := 0;
  elementSortOrder := '';
  elementRequiredCount := 0;
  elementRequired := '';
  attributeRequiredCount := 0;
  attributeRequired := '';

  for propertyIndex := 0 to Pred(AItem.PropertyCount) do
  begin
    propertyItem := AItem.Properties[propertyIndex];

    if propertyItem.IsAttribute then
    begin
      if not propertyItem.IsOptional then
        AddArrayElement(attributeRequired, attributeRequiredCount, QuotedStr(propertyItem.Name));
    end else if not propertyItem.IsNodeValue then
    begin
      AddArrayElement(elementSortOrder, elementSortCount, QuotedStr(propertyItem.Name));

      if (not propertyItem.IsOptional) and (not propertyItem.IsRepeating) then
      begin
        case propertyItem.PropertyType of
          ptSimple:
            AddArrayElement(elementRequired, elementRequiredCount, QuotedStr(propertyItem.Name));

          ptItem:
            { For Item properties, we call our getter property. This ensures the child element exists,
              but also that it is created using our binding implementation. Otherwise there will be no
              IXSDValidate interface to call on the newly created node. }
            AWriter.WriteLineNamedFmt(XSDValidateMethodImplementationComplex,
                                      ['Name', propertyItem.TranslatedName]);
        end;
      end;
    end;
  end;


  if elementRequiredCount > 0 then
  begin
    Delete(elementRequired, 1, 2);
    AWriter.WriteLineNamedFmt(IfThen(AStrict, XSDValidateStrictMethodImplementationRequired, XSDValidateMethodImplementationRequired),
                              ['RequiredElements', elementRequired]);
  end;


  if attributeRequiredCount > 0 then
  begin
    Delete(attributeRequired, 1, 2);
    AWriter.WriteLineNamedFmt(IfThen(AStrict, XSDValidateStrictMethodImplementationAttrib, XSDValidateMethodImplementationAttrib),
                              ['RequiredAttributes', attributeRequired]);
  end;

  if elementSortCount > 1 then
  begin
    Delete(elementSortOrder, 1, 2);
    AWriter.WriteLineNamedFmt(XSDValidateMethodImplementationSort,
                              ['SortOrder', elementSortOrder]);
  end;

  AWriter.WriteLine(IfThen(AStrict, XSDValidateStrictMethodImplementationEnd, XSDValidateMethodImplementationEnd));
end;


procedure TDelphiXMLDataBindingGenerator.WriteEnumeratorMethod(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
begin
  if not AItem.IsCollection then
    Exit;

  case ASection of
    dxsInterface,
    dxsClass:
      begin
        AWriter.WriteLineNamedFmt(EnumeratorMethodInterface,
                                  ['Name', AItem.TranslatedName]);
        AWriter.WriteLine('');
      end;

    dxsImplementation:
      begin
        AWriter.WriteLineNamedFmt(EnumeratorMethodImplementation,
                                  ['Name', AItem.TranslatedName]);
      end;
  end;
end;



procedure TDelphiXMLDataBindingGenerator.WriteEnumerator(AWriter: TNamedFormatWriter; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
begin
  if not AItem.IsCollection then
    Exit;



  case ASection of
    dxsInterface:
      begin
        AWriter.WriteLineNamedFmt(EnumeratorInterface,
                                  ['Name', AItem.TranslatedName,
                                   'DataType', GetDataTypeName(AItem.CollectionItem, True),
                                   'GUID', CreateNewGUID]);
        AWriter.WriteLine('');
      end;

    dxsClass:
      begin
        AWriter.WriteLineNamedFmt(EnumeratorClass,
                                  ['Name', AItem.TranslatedName,
                                   'DataType', GetDataTypeName(AItem.CollectionItem, True)]);
        AWriter.WriteLine('');
      end;

    dxsImplementation:
      begin
        AWriter.WriteLineNamedFmt(EnumeratorImplementation,
                                  ['Name', AItem.TranslatedName,
                                   'DataType', GetDataTypeName(AItem.CollectionItem, True)]);
      end;
  end;
end;


function TDelphiXMLDataBindingGenerator.GetDelphiNodeType(AProperty: TXMLDataBindingProperty): TDelphiNodeType;
begin
  if AProperty.IsAttribute then
    Result := dntAttribute
  else if AProperty.IsNodeValue then
    Result := dntNodeValue
  else if AProperty.HasTargetNamespace then
    Result := dntElementNS
  else
    Result := dntElement;
end;


function TDelphiXMLDataBindingGenerator.GetDelphiElementType(ANodeType: TDelphiNodeType): TDelphiElementType;
begin
  if ANodeType = dntElementNS then
    Result := dntElementNS
  else
    Result := dntElement;
end;


function TDelphiXMLDataBindingGenerator.DataTypeConversion(const ADestination, ASource: String; ADataType: IXMLTypeDef;
                                                           AAccessor: TDelphiAccessor; ANodeType: TDelphiNodeType;
                                                           const ATargetNamespace: string; const ALinesBefore: String = ''): String;
var
  typeMapping:  TTypeMapping;
  conversion:   String;

begin
  with TNamedFormatStringList.Create do
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

    // #ToDo1 -oMvR: 6-4-2012: Namespace
    Result := Trim(Format(['Destination', ADestination,
                           'Source',      ASource,
                           'Namespace',   ATargetNamespace]));
  finally
    Free;
  end;
end;


function TDelphiXMLDataBindingGenerator.XMLToNativeDataType(const ADestination, ASource: String; ADataType: IXMLTypeDef;
                                                            ANodeType: TDelphiNodeType; const ATargetNamespace: string;
                                                            const ALinesBefore: String): String;
begin
  Result := DataTypeConversion(ADestination, ASource, ADataType, daGet, ANodeType, ATargetNamespace, ALinesBefore);
end;


function TDelphiXMLDataBindingGenerator.NativeDataTypeToXML(const ADestination, ASource: String; ADataType: IXMLTypeDef;
                                                            ANodeType: TDelphiNodeType; const ATargetNamespace: string;
                                                            const ALinesBefore: String): String;
begin
  Result := DataTypeConversion(ADestination, ASource, ADataType, daSet, ANodeType, ATargetNamespace, ALinesBefore);
end;


function TDelphiXMLDataBindingGenerator.CreateNewGUID: String;
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

end.


