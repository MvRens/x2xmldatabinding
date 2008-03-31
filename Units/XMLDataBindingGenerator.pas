unit XMLDataBindingGenerator;

// #ToDo1 (MvR) 7-3-2008: check if List items can be collapsed if an item is
//                        already a list parent
// #ToDo1 (MvR) 19-3-2008: attributes
interface
uses
  Classes,
  Contnrs,
  XMLSchema;

type
  TXMLDataBindingSchema = class;
  TXMLDataBindingItem = class;
  TXMLDataBindingInterface = class;
  TXMLDataBindingCollection = class;
  TXMLDataBindingEnumerationMember = class;
  TXMLDataBindingEnumeration = class;
  TXMLDataBindingProperty = class;


  TXMLDataBindingOutputType = (otSingle, otMultiple);
  TXMLDataBindingItemType = (itInterface, itCollection, itEnumeration,
                             itEnumerationMember, itProperty, itForward,
                             itComplexTypeElement);
  TXMLDataBindingInterfaceType = (ifElement, ifComplexType);
  TXMLDataBindingPropertyType = (ptSimple, ptItem);


  TXMLDataBindingIterateItemsProc = procedure(AItem: TXMLDataBindingItem; AData: Pointer; var AAbort: Boolean) of object;


  TXMLDataBindingGenerator = class(TObject)
  private
    FIncludePaths:      TStrings;
    FOutputPath:        String;
    FOutputType:        TXMLDataBindingOutputType;
    FSourceFileName:    String;

    FSchemas:           TObjectList;
    FMustResolve: Boolean;

    function GetSchemaCount(): Integer;
    function GetSchemas(Index: Integer): TXMLDataBindingSchema;
  protected
    function LoadSchema(const AStream: TStream; const ASchemaName: String): TXMLDataBindingSchema;
    function GetSchemaData(const ALocation: String): TStream;
    function FindSchema(const ALocation: String): TXMLDataBindingSchema;

    procedure GenerateSchemaObjects(ASchema: TXMLDataBindingSchema; ARootDocument: Boolean);
    procedure GenerateElementObjects(ASchema: TXMLDataBindingSchema; ARootDocument: Boolean);
    procedure GenerateComplexTypeObjects(ASchema: TXMLDataBindingSchema);

    function ProcessElement(ASchema: TXMLDataBindingSchema; AElement: IXMLElementDef): TXMLDataBindingItem;
    procedure ProcessChildElement(ASchema: TXMLDataBindingSchema; AElement: IXMLElementDef; AInterface: TXMLDataBindingInterface);

    function IterateSchemaItems(ASchema: TXMLDataBindingSchema; AIterateProc: TXMLDataBindingIterateItemsProc; AData: Pointer): TXMLDataBindingItem;

    procedure FindInterfaceProc(AItem: TXMLDataBindingItem; AData: Pointer; var AAbort: Boolean);
    function FindInterface(ASchema: TXMLDataBindingSchema; const AName: String; AType: TXMLDataBindingInterfaceType): TXMLDataBindingInterface;

    procedure FindCollectionProc(AItem: TXMLDataBindingItem; AData: Pointer; var AAbort: Boolean);
    function FindCollection(ASchema: TXMLDataBindingSchema; const AName: String): TXMLDataBindingCollection;

    procedure FindEnumerationProc(AItem: TXMLDataBindingItem; AData: Pointer; var AAbort: Boolean);
    function FindEnumeration(ASchema: TXMLDataBindingSchema; const AName: String): TXMLDataBindingEnumeration;

    procedure ResolveSchema(ASchema: TXMLDataBindingSchema);
    procedure ResolveItem(ASchema: TXMLDataBindingSchema; AItem: TXMLDataBindingItem);
    procedure ResolveNameConflicts();

    procedure TranslateSchema(ASchema: TXMLDataBindingSchema);
    procedure TranslateItem(AItem: TXMLDataBindingItem);
    function TranslateItemName(AItem: TXMLDataBindingItem): String; virtual;

    procedure GenerateDataBinding(); virtual; abstract;

    property SourceFileName:            String                read FSourceFileName  write FSourceFileName;
    property SchemaCount:               Integer               read GetSchemaCount;
    property Schemas[Index: Integer]:   TXMLDataBindingSchema read GetSchemas;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Execute(const AStream: TStream; const ASchemaName: String); overload;
    procedure Execute(const AFileName: String); overload;

    property IncludePaths:    TStrings                  read FIncludePaths;
    property OutputType:      TXMLDataBindingOutputType read FOutputType      write FOutputType;
    property OutputPath:      String                    read FOutputPath      write FOutputPath;
  end;


  TXMLDataBindingSchema = class(TObject)
  private
    FIncludes:            TObjectList;
    FItems:               TObjectList;
    FItemsGenerated:      Boolean;
    FSchemaDef:           IXMLSchemaDef;
    FSchemaName:          String;

    function GetItemCount(): Integer;
    function GetItems(Index: Integer): TXMLDataBindingItem;
    function GetIncludeCount(): Integer;
    function GetIncludes(Index: Integer): TXMLDataBindingSchema;
  protected
    procedure AddInclude(ASchema: TXMLDataBindingSchema);
    procedure AddItem(AItem: TXMLDataBindingItem);

    property ItemsGenerated:  Boolean read FItemsGenerated  write FItemsGenerated;
  public
    constructor Create();
    destructor Destroy(); override;

    property IncludeCount:              Integer               read GetIncludeCount;
    property Includes[Index: Integer]:  TXMLDataBindingSchema read GetIncludes;

    property SchemaDef:                 IXMLSchemaDef         read FSchemaDef   write FSchemaDef;
    property SchemaName:                String                read FSchemaName  write FSchemaName;

    property ItemCount:                 Integer               read GetItemCount;
    property Items[Index: Integer]:     TXMLDataBindingItem   read GetItems;
  end;


  TXMLDataBindingItem = class(TObject)
  private
    FDocumentElement: Boolean;
    FName:            String;
    FSchemaItem:      IXMLSchemaItem;
    FTranslatedName:  String;

    function GetDocumentation(): String;
    function GetHasDocumentation(): Boolean;
  protected
    function GetItemType(): TXMLDataBindingItemType; virtual; abstract;
    procedure SetName(const Value: String);
    procedure SetTranslatedName(const Value: string);

    property SchemaItem:      IXMLSchemaItem            read FSchemaItem;
  public
    constructor Create(ASchemaItem: IXMLSchemaItem; const AName: String);

    property DocumentElement:     Boolean                   read FDocumentElement write FDocumentElement;
    property Documentation:       String                    read GetDocumentation;
    property HasDocumentation:    Boolean                   read GetHasDocumentation;
    property ItemType:            TXMLDataBindingItemType   read GetItemType;
    property Name:                String                    read FName;
    property TranslatedName:      String                    read FTranslatedName;
  end;


  TXMLDataBindingInterface = class(TXMLDataBindingItem)
  private
    FInterfaceType:   TXMLDataBindingInterfaceType;
    FProperties:      TObjectList;
    FBaseName:        String;
    FBaseItem:        TXMLDataBindingInterface;

    function GetProperties(Index: Integer): TXMLDataBindingProperty;
    function GetPropertyCount: Integer;
  protected
    function GetItemType(): TXMLDataBindingItemType; override;

    procedure AddProperty(AProperty: TXMLDataBindingProperty);
  public
    constructor Create(ASchemaItem: IXMLSchemaItem; const AName: String);
    destructor Destroy; override;

    property BaseName:        String                        read FBaseName    write FBaseName;
    property BaseItem:        TXMLDataBindingInterface      read FBaseItem    write FBaseItem;

    property InterfaceType:   TXMLDataBindingInterfaceType  read FInterfaceType;

    property PropertyCount:               Integer                 read GetPropertyCount;
    property Properties[Index: Integer]:  TXMLDataBindingProperty read GetProperties;
  end;


  TXMLDataBindingCollection = class(TXMLDataBindingItem)
  private
    FCollectionItem:  TXMLDataBindingProperty;

    function GetActualCollectionItem(): TXMLDataBindingItem;
    function GetCollectionItemName(): String;
    function GetCollectionItemTranslatedName(): String;
    procedure SetCollectionItem(const Value: TXMLDataBindingProperty);
  protected
    function GetItemType(): TXMLDataBindingItemType; override;
  public
    property CollectionItem:                TXMLDataBindingProperty read FCollectionItem;
    property CollectionItemName:            String                  read GetCollectionItemName;
    property CollectionItemTranslatedName:  String                  read GetCollectionItemTranslatedName;
  end;


  TXMLDataBindingEnumerationMember = class(TXMLDataBindingItem)
  private
    FEnumeration: TXMLDataBindingEnumeration;
  protected
    function GetItemType(): TXMLDataBindingItemType; override;
  public
    constructor Create(AEnumeration: TXMLDataBindingEnumeration; const AName: String);

    property Enumeration: TXMLDataBindingEnumeration  read FEnumeration;
  end;


  TXMLDataBindingEnumeration = class(TXMLDataBindingItem)
  private
    FDataType:  IXMLTypeDef;
    FMembers:   TObjectList;

    function GetMemberCount(): Integer;
    function GetMembers(Index: Integer): TXMLDataBindingEnumerationMember;
  protected
    function GetItemType(): TXMLDataBindingItemType; override;
  public
    constructor Create(ASchemaItem: IXMLSchemaItem; ADataType: IXMLTypeDef; const AName: String);
    destructor Destroy(); override;

    property DataType:                IXMLTypeDef                       read FDataType;
    property MemberCount:             Integer                           read GetMemberCount;
    property Members[Index: Integer]: TXMLDataBindingEnumerationMember  read GetMembers;
  end;


  TXMLDataBindingProperty = class(TXMLDataBindingItem)
  private
    FIsOptional: Boolean;
  protected
    function GetIsReadOnly(): Boolean; virtual; abstract;

    function GetItemType(): TXMLDataBindingItemType; override;
    function GetPropertyType(): TXMLDataBindingPropertyType; virtual; abstract;
  public
    property IsOptional:    Boolean                     read FIsOptional    write FIsOptional;
    property IsReadOnly:    Boolean                     read GetIsReadOnly;
    property PropertyType:  TXMLDataBindingPropertyType read GetPropertyType;
  end;


  TXMLDataBindingSimpleProperty = class(TXMLDataBindingProperty)
  private
    FDataType:  IXMLTypeDef;
  protected
    function GetIsReadOnly(): Boolean; override;
    function GetPropertyType(): TXMLDataBindingPropertyType; override;
  public
    constructor Create(ASchemaItem: IXMLSchemaItem; const AName: String; ADataType: IXMLTypeDef);

    property DataType:  IXMLTypeDef read FDataType;
  end;


  TXMLDataBindingItemProperty = class(TXMLDataBindingProperty)
  private
    FItem:  TXMLDataBindingItem;

    function GetItem(): TXMLDataBindingItem;
  protected
    function GetIsReadOnly(): Boolean; override;
    function GetPropertyType(): TXMLDataBindingPropertyType; override;
  public
    constructor Create(ASchemaItem: IXMLSchemaItem; const AName: String; AItem: TXMLDataBindingItem);

    property Item:  TXMLDataBindingItem read GetItem;
  end;


  TXMLDataBindingForwardItem = class(TXMLDataBindingItem)
  private
    FItem:  TXMLDataBindingItem;
    FInterfaceType: TXMLDataBindingInterfaceType;
  protected
    function GetItemType(): TXMLDataBindingItemType; override;
  public
    constructor Create(ASchemaItem: IXMLSchemaItem; const AName: String; AInterfaceType: TXMLDataBindingInterfaceType);

    property InterfaceType:   TXMLDataBindingInterfaceType  read FInterfaceType;
    property Item:            TXMLDataBindingItem           read FItem          write FItem;
  end;


  TXMLDataBindingComplexTypeElementItem = class(TXMLDataBindingItem)
  private
    FItem: TXMLDataBindingItem;
  protected
    function GetItemType(): TXMLDataBindingItemType; override;
  public
    property Item:  TXMLDataBindingItem read FItem  write FItem;
  end;


implementation
uses
  SysUtils,
  Windows,
  XMLDoc,
  XMLIntf,
  XMLSchemaTags,

  X2UtHashes;


const
  MaxOccursUnbounded  = 'unbounded';
  CollectionPostfix   = 'List';



function GetInterfaceType(ASchemaItem: IXMLSchemaItem): TXMLDataBindingInterfaceType;
begin
  if Supports(ASchemaItem, IXMLComplexTypeDef) then
    Result  := ifComplexType
  else
    Result  := ifElement;
end;


function GetActualItem(AItem: TXMLDataBindingItem): TXMLDataBindingItem;
begin
  Result  := AItem;

  while Assigned(Result) do
  begin
    case Result.ItemType of
      itForward:
        Result  := TXMLDataBindingForwardItem(Result).Item;

      itComplexTypeElement:
        Result  := TXMLDataBindingComplexTypeElementItem(Result).Item;
    else
      break;
    end;
  end;
end;


{ TXMLDataBindingGenerator }
constructor TXMLDataBindingGenerator.Create();
begin
  inherited;

  FIncludePaths := TStringList.Create();
  FSchemas      := TObjectList.Create(True);

  with TStringList(FIncludePaths) do
  begin
    CaseSensitive := False;
    Duplicates    := dupIgnore;
  end;
end;


destructor TXMLDataBindingGenerator.Destroy();
begin
  FreeAndNil(FSchemas);
  FreeAndNil(FIncludePaths);

  inherited;
end;


procedure TXMLDataBindingGenerator.Execute(const AStream: TStream; const ASchemaName: String);
var
  schemaIndex:    Integer;

begin
  FSchemas.Clear();
  LoadSchema(AStream, ASchemaName);

  if SchemaCount > 0 then
  begin
    { Map schema elements to objects }
    for schemaIndex := 0 to Pred(SchemaCount) do
      GenerateSchemaObjects(Schemas[schemaIndex], (schemaIndex = 0));


    { Process unresolved references
      - some references can't be resolved the first time (especially
        ComplexTypeElement references). Fix this workaround some time. }
    for schemaIndex := 0 to Pred(SchemaCount) do
      ResolveSchema(Schemas[schemaIndex]);

    for schemaIndex := 0 to Pred(SchemaCount) do
      ResolveSchema(Schemas[schemaIndex]);

      
    { Collapse collections }


    { Resolve naming conflicts }
    ResolveNameConflicts();


    { Perform output-specific translations }
    for schemaIndex := 0 to Pred(SchemaCount) do
      TranslateSchema(Schemas[schemaIndex]);


    { Output }
    GenerateDataBinding();
  end;
end;


procedure TXMLDataBindingGenerator.Execute(const AFileName: String);
var
  currentDir:   String;
  fileStream:   TFileStream;

begin
  currentDir  := GetCurrentDir();
  try
    ChDir(ExtractFilePath(AFileName));

    fileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      FSourceFileName := AFileName;
      IncludePaths.Add(ExtractFilePath(AFileName));

      Execute(fileStream, ChangeFileExt(ExtractFileName(AFileName), ''));
    finally
      FreeAndNil(fileStream);
    end;
  finally
    ChDir(currentDir);
  end;
end;



function TXMLDataBindingGenerator.LoadSchema(const AStream: TStream; const ASchemaName: String): TXMLDataBindingSchema;

  procedure HandleDocRefs(const ADocRefs: IXMLSchemaDocRefs; ASchema: TXMLDataBindingSchema);
  var
    location:       String;
    schemaName:     String;
    refSchema:      TXMLDataBindingSchema;
    refIndex:       Integer;
    refStream:      TStream;

  begin
    for refIndex := 0 to Pred(ADocRefs.Count) do
    begin
      location      := ADocRefs[refIndex].SchemaLocation;
      schemaName    := ChangeFileExt(ExtractFileName(location), '');
      refSchema     := FindSchema(schemaName);

      if not Assigned(refSchema) then
      begin
        refStream     := GetSchemaData(location);

        if Assigned(refStream) then
        try
          refSchema   := LoadSchema(refStream, schemaName);
        finally
          FreeAndNil(refStream);
        end;
      end;

      if Assigned(refSchema) then
        ASchema.AddInclude(refSchema);
    end;
  end;


var
  schemaDoc:      IXMLSchemaDoc;
  schemaDef:      IXMLSchemaDef;

begin
  schemaDoc := TXMLSchemaDoc.Create(nil);
  schemaDoc.LoadFromStream(AStream);
  schemaDef := schemaDoc.SchemaDef;

  Result            := TXMLDataBindingSchema.Create();
  Result.SchemaDef  := schemaDef;
  Result.SchemaName := ASchemaName;
  FSchemas.Add(Result);

  { Handle imports / includes }
  HandleDocRefs(schemaDef.SchemaImports, Result);
  HandleDocRefs(schemaDef.SchemaIncludes, Result);
end;


function TXMLDataBindingGenerator.GetSchemaData(const ALocation: String): TStream;
var
  includeIndex:   Integer;
  includePath:    String;

begin
  Result := nil;

  // #ToDo3 (MvR) 31-1-2007: support more locations than just a filename ?

  for includeIndex := 0 to Pred(IncludePaths.Count) do
  begin
    includePath := IncludeTrailingPathDelimiter(IncludePaths[includeIndex]);

    if FileExists(includePath + ALocation) then
    begin
      Result := TFileStream.Create(includePath + ALocation, fmOpenRead or fmShareDenyNone);
      break;
    end;
  end;
end;


function TXMLDataBindingGenerator.FindSchema(const ALocation: String): TXMLDataBindingSchema;
var
  schemaIndex: Integer;

begin
  Result  := nil;

  for schemaIndex := 0 to Pred(SchemaCount) do
    if Schemas[schemaIndex].SchemaName = ALocation then
    begin
      Result  := Schemas[schemaIndex];
      break;
    end;
end;


procedure TXMLDataBindingGenerator.GenerateSchemaObjects(ASchema: TXMLDataBindingSchema; ARootDocument: Boolean);
var
  includeIndex:         Integer;

begin
  if ASchema.ItemsGenerated then
    exit;

  ASchema.ItemsGenerated  := True;

  { First generate the objects for all includes and imports, so we can get
    proper references. }
  for includeIndex := 0 to Pred(ASchema.IncludeCount) do
    GenerateSchemaObjects(ASchema.Includes[includeIndex], False);


  GenerateElementObjects(ASchema, ARootDocument);
  GenerateComplexTypeObjects(ASchema);
end;


procedure TXMLDataBindingGenerator.GenerateElementObjects(ASchema: TXMLDataBindingSchema; ARootDocument: Boolean);
var
  schemaDef:            IXMLSchemaDef;
  elementIndex:         Integer;
  item:                 TXMLDataBindingItem;

begin
  schemaDef := ASchema.SchemaDef;

  for elementIndex := 0 to Pred(schemaDef.ElementDefs.Count) do
  begin
    item  := ProcessElement(ASchema, schemaDef.ElementDefs[elementIndex]);

    if Assigned(item) and ARootDocument then
      item.DocumentElement  := True;
  end;
end;


procedure TXMLDataBindingGenerator.GenerateComplexTypeObjects(ASchema: TXMLDataBindingSchema);
var
  schemaDef:            IXMLSchemaDef;
  complexTypeIndex:     Integer;
  complexType:          IXMLComplexTypeDef;
  interfaceItem:        TXMLDataBindingInterface;
  elementIndex:         Integer;

begin
  schemaDef := ASchema.SchemaDef;

  for complexTypeIndex := 0 to Pred(schemaDef.ComplexTypes.Count) do
  begin
    complexType   := schemaDef.ComplexTypes[complexTypeIndex];
    interfaceItem := TXMLDataBindingInterface.Create(complexType, complexType.Name);
    ASchema.AddItem(interfaceItem);

    for elementIndex := 0 to Pred(complexType.ElementDefs.Count) do
      ProcessChildElement(ASchema, complexType.ElementDefs[elementIndex], interfaceItem);
  end;
end;


function TXMLDataBindingGenerator.ProcessElement(ASchema: TXMLDataBindingSchema; AElement: IXMLElementDef): TXMLDataBindingItem;
var
  elementIndex:         Integer;
  enumerationObject:    TXMLDataBindingEnumeration;
  interfaceObject:      TXMLDataBindingInterface;
  complexTypeElement:   TXMLDataBindingComplexTypeElementItem;

begin
  Result := nil;
  interfaceObject := nil;

  if Assigned(AElement.Ref) then
  begin
    { Find reference. If not found, mark as "resolve later". }
    Result := FindInterface(ASchema, AElement.Ref.Name, ifElement);

    if not Assigned(Result) then
    begin
      Result  := TXMLDataBindingForwardItem.Create(AElement, AElement.Ref.Name, ifElement);
      ASchema.AddItem(Result);
    end;
  end else
  begin
    if (not AElement.DataType.IsAnonymous) and
       AElement.DataType.IsComplex then
    begin
      { Find data type. If not found, mark as "resolve later". }
      Result        := FindInterface(ASchema, AElement.DataTypeName, ifComplexType);

      if not Assigned(Result) then
      begin
        Result  := TXMLDataBindingForwardItem.Create(AElement, AElement.DataTypeName, ifComplexType);
        ASchema.AddItem(Result);
      end;

      if AElement.IsGlobal then
      begin
        { The element is global, but only references a complex type. Keep track
          to properly resolve references to the element. }
        complexTypeElement      := TXMLDataBindingComplexTypeElementItem.Create(AElement, AElement.Name);
        complexTypeElement.Item := Result;
        ASchema.AddItem(complexTypeElement);
      end;
    end;

    if not Assigned(Result) then
    begin
      if AElement.DataType.Enumerations.Count > 0 then
      begin
        { Enumeration }
        enumerationObject := TXMLDataBindingEnumeration.Create(AElement, AElement.DataType, AElement.Name);
        ASchema.AddItem(enumerationObject);
        Result := enumerationObject;
      end else if AElement.DataType.IsComplex then
      begin
        { Interface }
        interfaceObject := TXMLDataBindingInterface.Create(AElement, AElement.Name);
        if Assigned(AElement.DataType.BaseType) then
          interfaceObject.BaseName := AElement.DataType.BaseTypeName;

        ASchema.AddItem(interfaceObject);
        Result := interfaceObject;
      end;


      if Assigned(interfaceObject) then
        for elementIndex := 0 to Pred(AElement.ChildElements.Count) do
          ProcessChildElement(ASchema, AElement.ChildElements[elementIndex], interfaceObject);
    end;
  end;
end;


procedure TXMLDataBindingGenerator.ProcessChildElement(ASchema: TXMLDataBindingSchema; AElement: IXMLElementDef; AInterface: TXMLDataBindingInterface);
var
  collectionObject:     TXMLDataBindingCollection;
  propertyType:         TXMLDataBindingItem;
  propertyItem:         TXMLDataBindingProperty;

begin
  collectionObject := nil;


  if Assigned(AInterface) then
  begin
    if (AElement.MaxOccurs = MaxOccursUnbounded) or
       (AElement.MaxOccurs > 1) then
    begin
      { Collection }
      collectionObject  := FindCollection(ASchema, AElement.Name);

      if not Assigned(collectionObject) then
      begin
        collectionObject  := TXMLDataBindingCollection.Create(AElement, AElement.Name + CollectionPostfix);
        ASchema.AddItem(collectionObject);
      end;
    end;
  end;


  propertyType := ProcessElement(ASchema, AElement);

  if Assigned(collectionObject) then
  begin
    { Create intermediate object for collections }
    if Assigned(propertyType) then
      propertyItem  := TXMLDataBindingItemProperty.Create(AElement,
                                                          propertyType.Name,
                                                          propertyType)
    else
      propertyItem  := TXMLDataBindingSimpleProperty.Create(AElement,
                                                            AElement.Name,
                                                            AElement.DataType);


    collectionObject.SetCollectionItem(propertyItem);
    propertyType := collectionObject;
  end;


  if Assigned(AInterface) then
  begin
    if Assigned(propertyType) then
      propertyItem  := TXMLDataBindingItemProperty.Create(AElement,
                                                          AElement.Name,
                                                          propertyType)
    else
      propertyItem  := TXMLDataBindingSimpleProperty.Create(AElement,
                                                            AElement.Name,
                                                            AElement.DataType);

    propertyItem.IsOptional := (AElement.MinOccurs = 0);
    AInterface.AddProperty(propertyItem);
  end;
end;


function TXMLDataBindingGenerator.IterateSchemaItems(ASchema: TXMLDataBindingSchema; AIterateProc: TXMLDataBindingIterateItemsProc; AData: Pointer): TXMLDataBindingItem;
var
  abort:          Boolean;
  itemIndex:      Integer;
  schemaItem:     TXMLDataBindingItem;
  includeIndex:   Integer;

begin
  Result  := nil;
  abort   := False;

  for itemIndex := 0 to Pred(ASchema.ItemCount) do
  begin
    schemaItem  := ASchema.Items[itemIndex];

    AIterateProc(schemaItem, AData, abort);
    if abort then
    begin
      Result := schemaItem;
      Break;
    end;
  end;

  if not Assigned(Result) then
  begin
    for includeIndex := 0 to Pred(ASchema.IncludeCount) do
    begin
      Result  := IterateSchemaItems(ASchema.Includes[includeIndex], AIterateProc, AData);
      if Assigned(Result) then
        break;
    end;
  end;
end;



type
  PFindInterfaceInfo  = ^TFindInterfaceInfo;
  TFindInterfaceInfo  = record
    InterfaceType:    TXMLDataBindingInterfaceType;
    Name:             String;
  end;


procedure TXMLDataBindingGenerator.FindInterfaceProc(AItem: TXMLDataBindingItem; AData: Pointer; var AAbort: Boolean);
var
  findInfo:       PFindInterfaceInfo;

begin
  AAbort    := False;
  findInfo  := PFindInterfaceInfo(AData);


  if AItem.Name = findInfo^.Name then
  begin
    case AItem.ItemType of
      itInterface:
        AAbort  := (TXMLDataBindingInterface(AItem).InterfaceType = findInfo^.InterfaceType);

      itComplexTypeElement:
        AAbort  := (findInfo^.InterfaceType = ifElement);
    end;
  end;
end;


function TXMLDataBindingGenerator.FindInterface(ASchema: TXMLDataBindingSchema; const AName: String; AType: TXMLDataBindingInterfaceType): TXMLDataBindingInterface;
var
  findInfo:   TFindInterfaceInfo;

begin
  findInfo.InterfaceType  := AType;
  findInfo.Name           := AName;
  Result                  := TXMLDataBindingInterface(GetActualItem(IterateSchemaItems(ASchema, FindInterfaceProc, @findInfo)));
end;


procedure TXMLDataBindingGenerator.FindEnumerationProc(AItem: TXMLDataBindingItem; AData: Pointer; var AAbort: Boolean);
begin
  AAbort  := (AItem.ItemType = itEnumeration) and
             (AItem.Name = PChar(AData));
end;


function TXMLDataBindingGenerator.FindEnumeration(ASchema: TXMLDataBindingSchema; const AName: String): TXMLDataBindingEnumeration;
begin
  Result  := TXMLDataBindingEnumeration(IterateSchemaItems(ASchema, FindEnumerationProc, PChar(AName)));
end;


procedure TXMLDataBindingGenerator.FindCollectionProc(AItem: TXMLDataBindingItem; AData: Pointer; var AAbort: Boolean);
var
  collection: TXMLDataBindingCollection;

begin
  if AItem.ItemType = itCollection then
  begin
    collection  := TXMLDataBindingCollection(AItem);
    AAbort      := Assigned(collection.CollectionItem) and
                   (collection.CollectionItem.Name = PChar(AData));
  end;
end;


function TXMLDataBindingGenerator.FindCollection(ASchema: TXMLDataBindingSchema; const AName: String): TXMLDataBindingCollection;
begin
  Result  := TXMLDataBindingCollection(IterateSchemaItems(ASchema, FindCollectionProc, PChar(AName)));
end;



procedure TXMLDataBindingGenerator.ResolveSchema(ASchema: TXMLDataBindingSchema);
var
  itemIndex:      Integer;
  item:           TXMLDataBindingItem;
  interfaceItem:  TXMLDataBindingInterface;

begin
  for itemIndex := 0 to Pred(ASchema.ItemCount) do
  begin
    item  := ASchema.Items[itemIndex];

    case item.ItemType of
      itInterface:
        begin
          { Resolve base interface }
          interfaceItem := TXMLDataBindingInterface(item);

          if (not Assigned(interfaceItem.BaseItem)) and
             (Length(interfaceItem.BaseName) > 0) then
            interfaceItem.BaseItem  := FindInterface(ASchema, interfaceItem.BaseName, ifComplexType);
        end;

      itForward:
        ResolveItem(ASchema, item);
    end;
  end;
end;


procedure TXMLDataBindingGenerator.ResolveItem(ASchema: TXMLDataBindingSchema; AItem: TXMLDataBindingItem);
var
  forwardItem:    TXMLDataBindingForwardItem;
  referenceItem:  TXMLDataBindingItem;

begin
  if (not Assigned(AItem)) or (AItem.ItemType <> itForward) then
    Exit;

  { Resolve forwarded item }
  forwardItem   := TXMLDataBindingForwardItem(AItem);
  if not Assigned(forwardItem.Item) then
  begin
    referenceItem := FindInterface(ASchema, AItem.Name, forwardItem.InterfaceType);

    if (not Assigned(referenceItem)) and
       (forwardItem.InterfaceType = ifElement) then
      referenceItem := FindEnumeration(ASchema, AItem.Name);

    if Assigned(referenceItem) then
      forwardItem.Item  := referenceItem;
  end;
end;


procedure TXMLDataBindingGenerator.ResolveNameConflicts();
var
  itemNames:      TX2SOHash;


  procedure AddItem(AItem: TXMLDataBindingItem);
  var
    hashName:       String;
    items:          TObjectList;

  begin
    { Collections use the same Name as their items, differentiate
      between them while determining conflicts. }
    hashName  := AItem.Name;
    if AItem.ItemType = itCollection then
      hashName  := hashName + #1;

    if not itemNames.Exists(hashName) then
    begin
      items               := TObjectList.Create(False);
      itemNames[hashName] := items;
    end else
      items               := TObjectList(itemNames[hashName]);

    items.Add(AItem);
  end;


  function ResolveItemNameConflict(AItem: TXMLDataBindingItem; ADepth: Integer; out ANewName: String): Boolean;
  var
    currentDepth:   Integer;
    parentNode:     IXMLNode;
    schemaItem:     IXMLSchemaItem;

  begin
    Result        := False;
    currentDepth  := 0;
    parentNode    := AItem.SchemaItem;
    ANewName      := AItem.Name;

    while Assigned(parentNode) do
    begin
      parentNode  := parentNode.ParentNode;

      if Assigned(parentNode) and
         Supports(parentNode, IXMLSchemaItem, schemaItem) and
         (Length(schemaItem.Name) > 0) then
      begin
        ANewName  := schemaItem.Name + ANewName;

        Inc(currentDepth);
        if currentDepth = ADepth then
        begin
          Result  := True;
          break;
        end;
      end;
    end;
  end;


var
  schemaIndex:    Integer;
  schema:         TXMLDataBindingSchema;
  itemIndex:      Integer;
  items:          TObjectList;
  item:           TXMLDataBindingItem;
  depth:          Integer;
  newName:        String;
  resolved:       Boolean;

begin
  itemNames := TX2SOHash.Create(True);
  try
    { Gather names }
    for schemaIndex := 0 to Pred(SchemaCount) do
    begin
      schema  := Schemas[schemaIndex];

      for itemIndex := 0 to Pred(schema.ItemCount) do
      begin
        item      := schema.Items[itemIndex];

        if item.ItemType in [itInterface, itCollection, itEnumeration] then
          AddItem(item);
      end;
    end;


    { Find conflicts }
    itemNames.First();

    while itemNames.Next() do
    begin
      items := TObjectList(itemNames.CurrentValue);

      if items.Count > 1 then
      begin
        { Attempt to rename items }
        for itemIndex := Pred(items.Count) downto 0 do
        begin
          item      := TXMLDataBindingItem(items[itemIndex]);
          newName   := item.Name;
          resolved  := False;
          depth     := 1;

          while ResolveItemNameConflict(item, depth, newName) do
          begin
            if not itemNames.Exists(newName) then
            begin
              resolved  := True;
              break;
            end else
              Inc(depth);
          end;

          if resolved then
          begin
            items.Delete(itemIndex);

            item.SetName(newName);
            AddItem(item);
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(itemNames);
  end;
end;


procedure TXMLDataBindingGenerator.TranslateSchema(ASchema: TXMLDataBindingSchema);
var
  itemIndex:  Integer;

begin
  for itemIndex := 0 to Pred(ASchema.ItemCount) do
    TranslateItem(ASchema.Items[itemIndex]);
end;


procedure TXMLDataBindingGenerator.TranslateItem(AItem: TXMLDataBindingItem);
var
  interfaceItem: TXMLDataBindingInterface;
  propertyIndex: Integer;
  enumerationItem: TXMLDataBindingEnumeration;
  memberIndex: Integer;

begin
  AItem.SetTranslatedName(TranslateItemName(AItem));

  case AItem.ItemType of
    itInterface:
      begin
        interfaceItem := TXMLDataBindingInterface(AItem);

        for propertyIndex := 0 to Pred(interfaceItem.PropertyCount) do
          TranslateItem(interfaceItem.Properties[propertyIndex]);
      end;
    itEnumeration:
      begin
        enumerationItem := TXMLDataBindingEnumeration(AItem);

        for memberIndex := 0 to Pred(enumerationItem.MemberCount) do
          TranslateItem(enumerationItem.Members[memberIndex]);
      end;
  end;
end;


function TXMLDataBindingGenerator.TranslateItemName(AItem: TXMLDataBindingItem): String;
begin
  Result := AItem.Name;
end;


function TXMLDataBindingGenerator.GetSchemaCount(): Integer;
begin
  Result  := FSchemas.Count;
end;


function TXMLDataBindingGenerator.GetSchemas(Index: Integer): TXMLDataBindingSchema;
begin
  Result  := TXMLDataBindingSchema(FSchemas[Index]);
end;


{ TXMLDataBindingSchema }
constructor TXMLDataBindingSchema.Create();
begin
  inherited;

  FIncludes := TObjectList.Create(False);
  FItems := TObjectList.Create(True);
end;


destructor TXMLDataBindingSchema.Destroy();
begin
  FreeAndNil(FItems);
  FreeAndNil(FIncludes);

  inherited;
end;


procedure TXMLDataBindingSchema.AddInclude(ASchema: TXMLDataBindingSchema);
begin
  if FIncludes.IndexOf(ASchema) = -1 then
    FIncludes.Add(ASchema);
end;


procedure TXMLDataBindingSchema.AddItem(AItem: TXMLDataBindingItem);
begin
  if FItems.IndexOf(AItem) = -1 then
    FItems.Add(AItem);
end;


function TXMLDataBindingSchema.GetIncludeCount(): Integer;
begin
  Result  := FIncludes.Count;
end;


function TXMLDataBindingSchema.GetIncludes(Index: Integer): TXMLDataBindingSchema;
begin
  Result  := TXMLDataBindingSchema(FIncludes[Index]);
end;


function TXMLDataBindingSchema.GetItemCount(): Integer;
begin
  Result  := FItems.Count;
end;


function TXMLDataBindingSchema.GetItems(Index: Integer): TXMLDataBindingItem;
begin
  Result  := TXMLDataBindingItem(FItems[Index]);
end;


{ TXMLDataBindingItem }
constructor TXMLDataBindingItem.Create(ASchemaItem: IXMLSchemaItem; const AName: String);
begin
  inherited Create();

  FName := AName;
  FSchemaItem := ASchemaItem;
  FTranslatedName := AName;
end;


function TXMLDataBindingItem.GetDocumentation(): String;
var
  documentationIndex: Integer;

begin
  Result  := '';
  if HasDocumentation then
  begin
    for documentationIndex := 0 to Pred(SchemaItem.Documentation.Count) do
      Result  := Result + SchemaItem.Documentation[documentationIndex].Text + #13#10;

    Result  := Trim(Result);
  end;
end;


function TXMLDataBindingItem.GetHasDocumentation: Boolean;
begin
  Result  := Assigned(SchemaItem) and
             (SchemaItem.Documentation.Count > 0);
end;

procedure TXMLDataBindingItem.SetName(const Value: String);
begin
  FName := Value;
end;


procedure TXMLDataBindingItem.SetTranslatedName(const Value: string);
begin
  FTranslatedName := Value;
end;


{ TXMLDataBindingInterface }
constructor TXMLDataBindingInterface.Create(ASchemaItem: IXMLSchemaItem; const AName: String);
begin
  inherited Create(ASchemaItem, AName);

  FProperties := TObjectList.Create(True);
  FInterfaceType := GetInterfaceType(SchemaItem);
end;


destructor TXMLDataBindingInterface.Destroy;
begin
  FreeAndNil(FProperties);

  inherited;
end;


procedure TXMLDataBindingInterface.AddProperty(AProperty: TXMLDataBindingProperty);
begin
  FProperties.Add(AProperty);
end;


function TXMLDataBindingInterface.GetItemType(): TXMLDataBindingItemType;
begin
  Result  := itInterface;
end;


function TXMLDataBindingInterface.GetPropertyCount(): Integer;
begin
  Result := FProperties.Count;
end;


function TXMLDataBindingInterface.GetProperties(Index: Integer): TXMLDataBindingProperty;
begin
  Result := TXMLDataBindingProperty(FProperties[Index]);
end;


{ TXMLDataBindingCollection }
function TXMLDataBindingCollection.GetItemType(): TXMLDataBindingItemType;
begin
  Result  := itCollection;
end;


function TXMLDataBindingCollection.GetActualCollectionItem(): TXMLDataBindingItem;
begin
  Result  := nil;

  if Assigned(CollectionItem) then
  begin
    case CollectionItem.PropertyType of
      ptSimple: Result  := CollectionItem;
      ptItem:   Result  := TXMLDataBindingItemProperty(CollectionItem).Item;
    end;
  end;
end;

function TXMLDataBindingCollection.GetCollectionItemName(): String;
var
  item:   TXMLDataBindingItem;

begin
  Result  := '';
  item    := GetActualCollectionItem();
  if Assigned(item) then
    Result  := item.Name;
end;


function TXMLDataBindingCollection.GetCollectionItemTranslatedName(): String;
var
  item:   TXMLDataBindingItem;

begin
  Result  := '';
  item    := GetActualCollectionItem();
  if Assigned(item) then
    Result  := item.Name;
end;


procedure TXMLDataBindingCollection.SetCollectionItem(const Value: TXMLDataBindingProperty);
begin
  FCollectionItem := Value;
end;


{ TXMLDataBindingEnumerationMember }
constructor TXMLDataBindingEnumerationMember.Create(AEnumeration: TXMLDataBindingEnumeration; const AName: String);
begin
  inherited Create(nil, AName);

  FEnumeration  := AEnumeration;
end;


function TXMLDataBindingEnumerationMember.GetItemType(): TXMLDataBindingItemType;
begin
  Result := itEnumerationMember;
end;


{ TXMLDataBindingEnumeration }
constructor TXMLDataBindingEnumeration.Create(ASchemaItem: IXMLSchemaItem; ADataType: IXMLTypeDef; const AName: String);
var
  memberIndex:  Integer;

begin
  inherited Create(ASchemaItem, AName);

  FDataType := ADataType;
  FMembers  := TObjectList.Create();

  for memberIndex := 0 to Pred(ADataType.Enumerations.Count) do
    FMembers.Add(TXMLDataBindingEnumerationMember.Create(Self, ADataType.Enumerations.Items[memberIndex].Value));
end;


destructor TXMLDataBindingEnumeration.Destroy();
begin
  FreeAndNil(FMembers);

  inherited;
end;


function TXMLDataBindingEnumeration.GetItemType(): TXMLDataBindingItemType;
begin
  Result  := itEnumeration;
end;


function TXMLDataBindingEnumeration.GetMemberCount(): Integer;
begin
  Result  := FMembers.Count;
end;


function TXMLDataBindingEnumeration.GetMembers(Index: Integer): TXMLDataBindingEnumerationMember;
begin
  Result  := TXMLDataBindingEnumerationMember(FMembers[Index]);
end;


{ TXMLDataBindingProperty }
function TXMLDataBindingProperty.GetItemType(): TXMLDataBindingItemType;
begin
  Result := itProperty;
end;


{ TXMLDataBindingSimpleProperty }
constructor TXMLDataBindingSimpleProperty.Create(ASchemaItem: IXMLSchemaItem; const AName: String; ADataType: IXMLTypeDef);
begin
  inherited Create(ASchemaItem, AName);

  FDataType := ADataType;
end;


function TXMLDataBindingSimpleProperty.GetIsReadOnly(): Boolean;
begin
  Result := False;
end;


function TXMLDataBindingSimpleProperty.GetPropertyType(): TXMLDataBindingPropertyType;
begin
  Result := ptSimple;
end;


{ TXMLDataBindingItemProperty }
constructor TXMLDataBindingItemProperty.Create(ASchemaItem: IXMLSchemaItem; const AName: String; AItem: TXMLDataBindingItem);
begin
  inherited Create(ASchemaItem, AName);

  FItem := AItem;
end;


function TXMLDataBindingItemProperty.GetIsReadOnly(): Boolean;
begin
  Result := Assigned(Item) and (Item.ItemType <> itEnumeration);
end;


function TXMLDataBindingItemProperty.GetPropertyType(): TXMLDataBindingPropertyType;
begin
  Result := ptItem;
end;


function TXMLDataBindingItemProperty.GetItem(): TXMLDataBindingItem;
begin
  Result := GetActualItem(FItem);
end;


{ TXMLDataBindingForwardItem }
constructor TXMLDataBindingForwardItem.Create(ASchemaItem: IXMLSchemaItem; const AName: String; AInterfaceType: TXMLDataBindingInterfaceType);
begin
  inherited Create(ASchemaItem, AName);

  FInterfaceType := AInterfaceType;
end;


function TXMLDataBindingForwardItem.GetItemType(): TXMLDataBindingItemType;
begin
  Result  := itForward;
end;


{ TXMLDataBindingComplexTypeElementItem }
function TXMLDataBindingComplexTypeElementItem.GetItemType(): TXMLDataBindingItemType;
begin
  Result  := itComplexTypeElement;
end;

end.




