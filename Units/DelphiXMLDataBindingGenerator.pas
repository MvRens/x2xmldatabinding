unit DelphiXMLDataBindingGenerator;

interface
uses
  Classes,
  XMLSchema,

  X2UtHashes,

  DelphiXMLDataBindingResources,
  XMLDataBindingGenerator,
  XMLDataBindingHelpers;

  
type
  TGetFileNameEvent = procedure(Sender: TObject; const SchemaName: String; var Path, FileName: String) of object;

  
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


    function GetDataTypeMapping(ADataType: IXMLTypeDef; out ATypeMapping: TTypeMapping): Boolean;
    function TranslateDataType(ADataType: IXMLTypeDef): String;
    function CreateNewGUID(): String;

    procedure WriteUnitHeader(AStream: TStreamHelper; const AFileName: String);
    procedure WriteSection(AStream: TStreamHelper; ASection: TDelphiXMLSection);
    procedure WriteDocumentFunctions(AStream: TStreamHelper; ASection: TDelphiXMLSection);
    procedure WriteEnumerationConstants(AStream: TStreamHelper);
    procedure WriteEnumerationConversions(AStream: TStreamHelper);
    procedure WriteDocumentation(AStream: TStreamHelper; AItem: TXMLDataBindingItem);

    procedure WriteSchemaItem(AStream: TStreamHelper; AItem: TXMLDataBindingItem; ASection: TDelphiXMLSection);
    procedure WriteSchemaInterface(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
    procedure WriteSchemaInterfaceProperties(AStream: TStreamHelper; AItem: TXMLDataBindingInterface; ASection: TDelphiXMLSection);
    procedure WriteSchemaCollection(AStream: TStreamHelper; AItem: TXMLDataBindingCollection; ASection: TDelphiXMLSection);
    procedure WriteSchemaCollectionProperties(AStream: TStreamHelper; AItem: TXMLDataBindingCollection; ASection: TDelphiXMLSection);
    procedure WriteSchemaEnumeration(AStream: TStreamHelper; AItem: TXMLDataBindingEnumeration; ASection: TDelphiXMLSection);
    procedure WriteSchemaEnumerationArray(AStream: TStreamHelper; AItem: TXMLDataBindingEnumeration);

    function DataTypeConversion(const ADestination, ASource: string; ADataType: IXMLTypeDef; AToNative: Boolean; const ALinesBefore: string = ''): string;
    function XMLToNativeDataType(const ADestination, ASource: string; ADataType: IXMLTypeDef; const ALinesBefore: string = ''): string;
    function NativeDataTypeToXML(const ADestination, ASource: string; ADataType: IXMLTypeDef; const ALinesBefore: string = ''): string;

    property ProcessedItems:  TX2OIHash read FProcessedItems;
  public
    property OnGetFileName:   TGetFileNameEvent read FOnGetFileName write FOnGetFileName;
  end;

  
implementation
uses
  Contnrs,
  SysUtils,

  X2UtNamedFormat;



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

    unitStream.Write(UnitInterface);
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

    unitStream.Write(UnitImplementation);
    WriteDocumentFunctions(unitStream, dxsImplementation);
    WriteEnumerationConversions(unitStream);

    // #ToDo1 (MvR) 20-3-2008: write conversion methods

    WriteSection(unitStream, dxsImplementation);

    unitStream.Write(unitFooter);
  finally
    FreeAndNil(unitStream);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.GenerateMultipleDataBinding();
begin
end;


function TDelphiXMLDataBindingGenerator.GetDataTypeMapping(ADataType: IXMLTypeDef; out ATypeMapping: TTypeMapping): Boolean;
var
  mappingIndex: Integer;
  dataTypeName: string;

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
  AStream.WriteNamedFmt(UnitHeader,
                        ['SourceFileName',  SourceFileName,
                         'UnitName',        ChangeFileExt(ExtractFileName(AFileName), '')]);
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
  enumerations:   TObjectList;

begin
  { Write array constants for enumerations }
  enumerations  := TObjectList.Create(False);
  try
    for schemaIndex := 0 to Pred(SchemaCount) do
    begin
      schema := Schemas[schemaIndex];

      for itemIndex := 0 to Pred(schema.ItemCount) do
      begin
        item := schema.Items[itemIndex];

        if item.ItemType = itEnumeration then
          enumerations.Add(item);
      end;
    end;

    if enumerations.Count > 0 then
    begin
      AStream.WriteLn('const');

      for itemIndex := 0 to Pred(enumerations.Count) do
        WriteSchemaEnumerationArray(AStream, TXMLDataBindingEnumeration(enumerations[itemIndex]));
    end;
  finally
    FreeAndNil(enumerations);
  end;
end;


procedure TDelphiXMLDataBindingGenerator.WriteEnumerationConversions(AStream: TStreamHelper);
begin
  //
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

                AStream.WriteLnNamedFmt('  RegisterChildNode(''%<SourceName>:s'', TXML%<Name>:s);',
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
  member:           TDelphiXMLMember;
  value:            String;
  sourceCode:       TNamedFormatStringList;
  propertyItemName: String;

begin
  // #ToDo1 (MvR) 9-3-2008: refactor WriteSchemaInterfaceProperties
  if ASection = dxsForward then
    Exit;

  if ASection = dxsImplementation then
    WriteAfterConstruction();

  if ASection = dxsClass then
    AStream.WriteLn('  protected');

  hasMembers  := False;
  for member := Low(TDelphiXMLMember) to High(TDelphiXMLMember) do
  begin
    if hasMembers then
      AStream.WriteLn;

    hasMembers  := False;

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
                case member of
                  dxmPropertyGet:
                    begin
                      if writeOptional then
                        sourceCode.Add(PropertyIntfMethodGetOptional);

                      if writeTextProp then
                        sourceCode.Add(PropertyIntfMethodGetText);

                      sourceCode.Add(PropertyIntfMethodGet);
                      hasMembers  := True;
                    end;

                  dxmPropertySet:
                    if not itemProperty.IsReadOnly then
                    begin
                      if writeTextProp then
                        sourceCode.Add(PropertyIntfMethodSetText);

                      sourceCode.Add(PropertyIntfMethodSet);
                      hasMembers  := True;
                    end;

                  dxmPropertyDeclaration:
                    begin
                      if writeOptional then
                        sourceCode.Add(PropertyInterfaceOptional);

                      if itemProperty.IsReadOnly then
                      begin
                        if writeTextProp then
                          sourceCode.Add(PropertyInterfaceTextReadOnly);

                        sourceCode.Add(PropertyInterfaceReadOnly);
                      end else
                      begin
                        if writeTextProp then
                          sourceCode.Add(PropertyInterfaceText);

                        sourceCode.Add(PropertyInterface);
                      end;

                      hasMembers  := True;
                    end;
                end;
              end;
            dxsImplementation:
              begin
                { Implementation }
                case member of
                  dxmPropertyGet:
                    begin
                      if writeOptional then
                        sourceCode.Add(PropertyImplMethodGetOptional);

                      if writeTextProp then
                        sourceCode.Add(PropertyImplMethodGetText);

                      sourceCode.Add('function TXML%<Name>:s.Get%<PropertyName>:s: %<DataType>:s;');

                      case itemProperty.PropertyType of
                        ptSimple:
                          sourceCode.Add(XMLToNativeDataType('Result',
                                                             'ChildNodes[''%<PropertySourceName>:s''].NodeValue',
                                                             TXMLDataBindingSimpleProperty(itemProperty).DataType));

                        ptItem:
                          begin
                            if Assigned(propertyItem) then
                            begin
                              case propertyItem.ItemType of
                                itInterface,
                                itCollection:
                                  begin
                                    sourceCode.Add('begin');
                                    sourceCode.Add('  Result := (ChildNodes[''%<Name>:s''] as IXML%<PropertyItemName>:s);');
                                    sourceCode.Add('end;');
                                  end;

                                itEnumeration:
                                  begin
                                    sourceCode.Add('var');
                                    sourceCode.Add('  nodeValue: WideString;');
                                    sourceCode.Add('  enumValue: %<DataType>:s;');
                                    sourceCode.AddLn;
                                    sourceCode.Add('begin');
                                    sourceCode.Add('  Result := %<DataType>:s(-1);');
                                    sourceCode.Add('  nodeValue := Get%<PropertyName>:sText;');
                                    sourceCode.Add('  for enumValue := Low(%<DataType>:s) to High(%<DataType>:s) do');
                                    sourceCode.Add('    if %<PropertyName>:sValues[enumValue] = nodeValue then');
                                    sourceCode.Add('    begin');
                                    sourceCode.Add('      Result := enumValue;');
                                    sourceCode.Add('      break;');
                                    sourceCode.Add('    end;');
                                    sourceCode.Add('end;');
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
                        sourceCode.Add(PropertyImplMethodSetText);

                      sourceCode.Add('procedure TXML%<Name>:s.Set%<PropertyName>:s(const Value: %<DataType>:s);');
                      value := 'ChildNodes[''%<PropertySourceName>:s''].NodeValue';
                      
                      if Assigned(propertyItem) and (propertyItem.ItemType = itEnumeration) then
                      begin
                        sourceCode.Add('begin');
                        sourceCode.Add('  ' + value + ' := %<PropertyItemName>:sValues[Value]');
                        sourceCode.Add('end;');
                        sourceCode.AddLn;
                      end else
                      begin
                        if itemProperty.PropertyType <> ptSimple then
                          raise Exception.Create('Setter must be a simple type');

                        sourceCode.Add(NativeDataTypeToXML(value, 'Value',
                                                           TXMLDataBindingSimpleProperty(itemProperty).DataType));
                      end;
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
  dataIntfName: string;
  dataTypeName: string;
  dataClassName: string;
  sourceCode: TNamedFormatStringList;
  typeDef: IXMLTypeDef;

begin
  if ASection = dxsClass then
    AStream.WriteLn('  protected');

  // #ToDo1 (MvR) 17-3-2008: DataType for enumerations
  case AItem.CollectionItem.PropertyType of
    ptSimple:
      begin
        dataTypeName  := TranslateDataType(TXMLDataBindingSimpleProperty(AItem.CollectionItem).DataType);
        dataClassName := 'TXMLNode';
        dataIntfName  := 'IXMLNode';
      end;
    ptItem:
      begin
        dataTypeName  := PrefixInterface + AItem.CollectionItemTranslatedName;
        dataClassName := PrefixClass + AItem.CollectionItemTranslatedName;
        dataIntfName  := dataTypeName;
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
          sourceCode.Add('procedure TXML%<Name>:s.AfterConstruction;');
          sourceCode.Add('begin');
          sourceCode.Add('  RegisterChildNode(''%<ItemSourceName>:s'', %<DataClass>:s);');
          sourceCode.AddLn;
          sourceCode.Add('  ItemTag := ''%<ItemSourceName>:s'';');
          sourceCode.Add('  ItemInterface := %<DataInterface>:s;');
          sourceCode.AddLn;
          sourceCode.Add('  inherited;');
          sourceCode.Add('end;');
          sourceCode.AddLn;


          case AItem.CollectionItem.PropertyType of
            ptSimple:
              begin
                typeDef := TXMLDataBindingSimpleProperty(AItem.CollectionItem).DataType;

                // #ToDo1 (MvR) 19-3-2008: .Text for strings ?
                sourceCode.Add('function TXML%<Name>:s.Get_%<ItemName>:s(Index: Integer): %<DataType>:s;');
                sourceCode.Add(XMLToNativeDataType('Result', 'List[Index].NodeValue', typeDef));
                sourceCode.AddLn;

                sourceCode.Add('function TXML%<Name>:s.Add(%<ItemName>:s: %<DataType>:s): %<DataInterface>:s;');
                sourceCode.Add(NativeDataTypeToXML('Result.NodeValue', '%<ItemName>:s', typeDef,
                                                   '  Result := AddItem(-1);'));
                sourceCode.AddLn;

                sourceCode.Add('function TXML%<Name>:s.Insert(Index: Integer; %<ItemName>:s: %<DataType>:s): %<DataInterface>:s;');
                sourceCode.Add(NativeDataTypeToXML('Result.NodeValue', '%<ItemName>:s', typeDef,
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

    case ASection of
      dxsInterface:
        begin
          sourceCode.AddLn;
          sourceCode.Add('    property %<ItemName>:s[Index: Integer]: %<DataType>:s read Get_%<ItemName>:s; default;');
        end;

      dxsClass:
        begin
          sourceCode.Add('  public');
          sourceCode.Add('    procedure AfterConstruction; override;');
        end;
    end;

    AStream.Write(sourceCode.Format(['Name',            AItem.TranslatedName,
                                     'ItemName',        AItem.CollectionItemTranslatedName,
                                     'ItemSourceName',  AItem.CollectionItem.Name,
                                     'DataType',        dataTypeName,
                                     'DataClass',       dataClassName,
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


function TDelphiXMLDataBindingGenerator.DataTypeConversion(const ADestination, ASource: string; ADataType: IXMLTypeDef; AToNative: Boolean; const ALinesBefore: string): string;
var
  typeMapping:  TTypeMapping;

begin
  with TNamedFormatStringList.Create() do
  try
    if not GetDataTypeMapping(ADataType, typeMapping) then
      typeMapping.Conversion  := tcNone;


    if Length(TypeConversionVariables[typeMapping.Conversion]) > 0 then
    begin
      Add('var');
      Add(TypeConversionVariables[typeMapping.Conversion]);
    end;

    Add('begin');

    if Length(ALinesBefore) > 0 then
      Add(ALinesBefore);

    if AToNative then
      Add(TypeConversionToNative[typeMapping.Conversion])
    else
      Add(TypeConversionToXML[typeMapping.Conversion]);

    Add('end;');
    
    Result := Format(['Destination', ADestination,
                      'Source',      ASource]);
  finally
    Free();
  end;
end;


function TDelphiXMLDataBindingGenerator.XMLToNativeDataType(const ADestination, ASource: string; ADataType: IXMLTypeDef; const ALinesBefore: string): string;
begin
  Result := DataTypeConversion(ADestination, ASource, ADataType, True, ALinesBefore);
end;


function TDelphiXMLDataBindingGenerator.NativeDataTypeToXML(const ADestination, ASource: string; ADataType: IXMLTypeDef; const ALinesBefore: string): string;
begin
  Result := DataTypeConversion(ADestination, ASource, ADataType, False, ALinesBefore);
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

end.


