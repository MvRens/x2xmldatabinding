unit ObjectMappingTests;

interface
uses
  TestFramework;


type
  TObjectMappingTests = class(TAbstractTest, ITest)
  protected
    procedure RunTest(testResult: TTestResult); override;
  public
    class function Suite: ITestSuite;
  end;


implementation

{ TObjectMappingTests }
class function TObjectMappingTests.Suite: ITestSuite;
begin
  Result := TTestSuite.Create(Self.ClassName);

  { Add tests for all .xsd files which have a corresponding .expected file } 
  Result.AddTest(Self.Create('Test'));
end;


procedure TObjectMappingTests.RunTest(testResult: TTestResult);
begin
  testResult.AddFailure(Self, nil, 'Oeps!');
end;



initialization
  RegisterTest(TObjectMappingTests.Suite);

end.

