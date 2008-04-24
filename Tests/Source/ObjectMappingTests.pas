unit ObjectMappingTests;

interface
uses
  TestFramework,

  DataBindingResultXML,
  XMLDataBindingGenerator;


type
  TTestXMLDataBindingGenerator  = class(TXMLDataBindingGenerator)
  protected
    procedure GenerateDataBinding(); override;
  end;


  TObjectMappingTests = class(TAbstractTest, ITest)
  private
    FFileName:  String;
  protected
    procedure RunTest(testResult: TTestResult); override;

    procedure CompareSchemas(ATestResult: TTestResult; AGenerator: TTestXMLDataBindingGenerator; AResult: IXMLDataBindingResult);
    procedure CompareItems(ATestResult: TTestResult; AGeneratorSchema: TXMLDataBindingSchema; AResultSchema: IXMLSchema);
//    procedure CompareCollection(ATestResult: TTestResult; AGeneratorSchema: TXMLDataBindingSchema; AGeneratorItem: TXMLDataBindingCollection; AResultItem: IXMLItem);

    property FileName:  String  read FFileName;
  public
    constructor Create(const AFileName: String);

    class function Suite(): ITestSuite;
  end;


implementation
uses
  Contnrs,
  SysUtils,

  X2UtApp;


const
  ExpectedExtension = '_expected.xml';


{ TObjectMappingTests }
class function TObjectMappingTests.Suite(): ITestSuite;
var
  basePath: String;
  fileInfo: TSearchRec;

begin
  Result := TTestSuite.Create(Self.ClassName);

  { Add tests for all .xsd files which have a corresponding .expected file }
  basePath  := App.Path + 'Tests\Data\';
  
  if FindFirst(basePath + '*.xsd', faAnyFile, fileInfo) = 0 then
  begin
    repeat
      if FileExists(basePath + ChangeFileExt(fileInfo.Name, ExpectedExtension)) then
      begin
        Result.AddTest(Self.Create(basePath + fileInfo.Name));
      end;
    until FindNext(fileInfo) <> 0;

    SysUtils.FindClose(fileInfo);
  end;
end;


constructor TObjectMappingTests.Create(const AFileName: String);
begin
  inherited Create(ChangeFileExt(ExtractFileName(AFileName), ''));

  FFileName := AFileName;
end;


procedure TObjectMappingTests.RunTest(testResult: TTestResult);
var
  generator:        TTestXMLDataBindingGenerator;
  expectedResult:   IXMLDataBindingResult;

begin
  generator := TTestXMLDataBindingGenerator.Create();
  try
    generator.Execute(FileName);

    expectedResult := LoadDataBindingResult(ChangeFileExt(FileName, ExpectedExtension));
    CompareSchemas(testResult, generator, expectedResult);
  finally
    FreeAndNil(generator);
  end;
end;


procedure TObjectMappingTests.CompareSchemas(ATestResult: TTestResult; AGenerator: TTestXMLDataBindingGenerator; AResult: IXMLDataBindingResult);
var
  handled:        TObjectList;
  schemaIndex:    Integer;
  resultSchema:   IXMLSchema;
  bindingSchema:  TXMLDataBindingSchema;

begin
  handled := TObjectList.Create(False);
  try
    { Iterate expected schemas }
    for schemaIndex := 0 to Pred(AResult.Schemas.Count) do
    begin
      resultSchema  := AResult.Schemas[schemaIndex];
      bindingSchema := AGenerator.FindSchema(resultSchema.Name);

      if Assigned(bindingSchema) then
      begin
        handled.Add(bindingSchema);
        CompareItems(ATestResult, bindingSchema, resultSchema);
      end else
        ATestResult.AddFailure(Self, nil, Format('Schema "%s" expected', [resultSchema.Name]));
    end;

    { Find unexpected schemas }
    for schemaIndex := 0 to Pred(AGenerator.SchemaCount) do
      if handled.IndexOf(AGenerator.Schemas[schemaIndex]) = -1 then
      begin
        ATestResult.AddFailure(Self, nil, Format('Schema "%s" not expected', [AGenerator.Schemas[schemaIndex].SchemaName]));
      end;
  finally
    FreeAndNil(handled);
  end;
end;


procedure TObjectMappingTests.CompareItems(ATestResult: TTestResult; AGeneratorSchema: TXMLDataBindingSchema; AResultSchema: IXMLSchema);

  function FindItem(const AResultItem: IXMLItem): TXMLDataBindingItem;
  var
    itemType:   TXMLDataBindingItemType;
    itemIndex:  Integer;
    item:       TXMLDataBindingItem;

  begin
    Result    := nil;
    itemType  := itInterface;

    {if AResultItem.ItemType = 'Collection' then
      itemType := itInterface
    else }if AResultItem.ItemType = 'Enumeration' then
      itemType := itEnumeration;

    for itemIndex := 0 to Pred(AGeneratorSchema.ItemCount) do
    begin
      item  := AGeneratorSchema.Items[itemIndex];

      if (item.ItemType = itemType) and
         (item.Name = AResultItem.Name) then
      begin
        Result  := item;
        break;
      end;
    end;
  end;


var
  handled:        TObjectList;
  itemIndex:      Integer;
  resultItem:     IXMLItem;
  bindingItem:    TXMLDataBindingItem;

begin
  handled := TObjectList.Create(False);
  try
    { Iterate expected items }
    for itemIndex := 0 to Pred(AResultSchema.Items.Count) do
    begin
      resultItem  := AResultSchema.Items[itemIndex];
      bindingItem := FindItem(resultItem);

      if Assigned(bindingItem) then
      begin
        handled.Add(bindingItem);

//        case bindingItem.ItemType of
//          itInterface:  CompareProperties;
//          itCollection: CompareCollection(ATestResult, AGeneratorSchema, TXMLDataBindingCollection(bindingItem), resultItem);
//        end;
      end else
        ATestResult.AddFailure(Self, nil, Format('Item "%s.%s" expected',
                               [AGeneratorSchema.SchemaName, resultItem.Name]));
    end;

    { Find unexpected items }
    for itemIndex := 0 to Pred(AGeneratorSchema.ItemCount) do
    begin
//      bindingItem := AGeneratorSchema.Items[itemIndex];

//      if bindingItem.ItemType <> itForward then
//      begin
//        if handled.IndexOf(bindingItem) = -1 then
//        begin
//          ATestResult.AddFailure(Self, nil, Format('Item "%s.%s" not expected',
//                                                   [AGeneratorSchema.SchemaName,
//                                                    AGeneratorSchema.Items[itemIndex].Name]));
//        end;
//      end;
    end;
  finally
    FreeAndNil(handled);
  end;
end;


{
procedure TObjectMappingTests.CompareCollection(ATestResult: TTestResult; AGeneratorSchema: TXMLDataBindingSchema; AGeneratorItem: TXMLDataBindingCollection; AResultItem: IXMLItem);
begin
  if Assigned(AGeneratorItem.CollectionItem) then
  begin
    if AGeneratorItem.CollectionItem.Name <> AResultItem.Collection.ItemName then
      ATestResult.AddFailure(Self, nil, Format('Item "%s.%s": collection item "%s" expected but "%s" found',
                                               [AGeneratorSchema.SchemaName,
                                                AGeneratorItem.Name,
                                                AResultItem.Collection.ItemName,
                                                AGeneratorItem.CollectionItem.Name]));
  end else
    ATestResult.AddFailure(Self, nil, Format('Item "%s.%s": collection item not Assigned',
                                             [AGeneratorSchema.SchemaName,
                                              AGeneratorItem.Name]));
end;
}


{ TTestXMLDataBindingGenerator }
procedure TTestXMLDataBindingGenerator.GenerateDataBinding();
begin
end;


initialization
//  RegisterTest(TObjectMappingTests.Suite);

end.

