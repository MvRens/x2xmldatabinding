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

  function FindItem(const AName: String): TXMLDataBindingItem;
  var
    itemIndex: Integer;

  begin
    Result  := nil;

    for itemIndex := 0 to Pred(AGeneratorSchema.ItemCount) do
      if (AGeneratorSchema.Items[itemIndex].Name = AName) and
         (AGeneratorSchema.Items[itemIndex].ItemType <> itForward) then
      begin
        Result  := AGeneratorSchema.Items[itemIndex];
        break;
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
    { Iterate expected schemas }
    for itemIndex := 0 to Pred(AResultSchema.Items.Count) do
    begin
      resultItem  := AResultSchema.Items[itemIndex];
      bindingItem := FindItem(resultItem.Name);

      if Assigned(bindingItem) then
      begin
        handled.Add(bindingItem);
//        CompareItems(ATestResult, bindingSchema, resultSchema);
      end else
        ATestResult.AddFailure(Self, nil, Format('Schema "%s": item "%s" expected',
                               [AGeneratorSchema.SchemaName, resultItem.Name]));
    end;

    { Find unexpected schemas }
    for itemIndex := 0 to Pred(AGeneratorSchema.ItemCount) do
    begin
      bindingItem := AGeneratorSchema.Items[itemIndex];

      if bindingItem.ItemType <> itForward then
      begin
        if handled.IndexOf(bindingItem) = -1 then
        begin
          ATestResult.AddFailure(Self, nil, Format('Schema "%s": item "%s" not expected',
                                                   [AGeneratorSchema.SchemaName,
                                                    AGeneratorSchema.Items[itemIndex].Name]));
        end;
      end;
    end;
  finally
    FreeAndNil(handled);
  end;
end;


{ TTestXMLDataBindingGenerator }
procedure TTestXMLDataBindingGenerator.GenerateDataBinding();
begin
end;


initialization
  RegisterTest(TObjectMappingTests.Suite);

end.

