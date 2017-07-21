{
  X2Software XML Data Binding

    Generated on:   21-7-2017 9:19:46
    Generated from: P:\x2xmldatabinding\XSD\DataBindingSettings.xsd
}
unit DataBindingSettingsXML;

interface
uses
  Classes,
  SysUtils,
  XMLDoc,
  XMLIntf,
  XMLDataBindingUtils;

type
  { Forward declarations for DataBindingSettings }
  IXMLDataBindingSettings = interface;
  IXMLDataBindingOutput = interface;
  IXMLOutputSingle = interface;
  IXMLOutputMultiple = interface;
  TXMLDataBindingOutputType = (DataBindingOutputType_Single,
                               DataBindingOutputType_Multiple);

  { Interfaces for DataBindingSettings }
  {
    Contains the settings and hints for the Delphi XML Data Binding.
  }
  IXMLDataBindingSettings = interface(IXMLNode)
    ['{0407CBCB-B49B-4ED3-A2F6-CCDDFE46F334}']
    procedure XSDValidateDocument(AStrict: Boolean = False);
    function GetHasOutput: Boolean;
    function GetOutput: IXMLDataBindingOutput;

    property HasOutput: Boolean read GetHasOutput;
    property Output: IXMLDataBindingOutput read GetOutput;
  end;

  {
    Contains the user-defined output settings last used
  }
  IXMLDataBindingOutput = interface(IXMLNode)
    ['{21821EFA-C7D8-4299-BF44-8122FBF2BC2E}']
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetOutputTypeText: WideString;
    function GetOutputType: TXMLDataBindingOutputType;
    function GetHasOutputSingle: Boolean;
    function GetOutputSingle: IXMLOutputSingle;
    function GetHasOutputMultiple: Boolean;
    function GetOutputMultiple: IXMLOutputMultiple;
    function GetHasHasChecksEmpty: Boolean;
    function GetHasChecksEmpty: Boolean;

    procedure SetOutputTypeText(const Value: WideString);
    procedure SetOutputType(const Value: TXMLDataBindingOutputType);
    procedure SetHasChecksEmpty(const Value: Boolean);

    property OutputTypeText: WideString read GetOutputTypeText write SetOutputTypeText;
    property OutputType: TXMLDataBindingOutputType read GetOutputType write SetOutputType;
    property HasOutputSingle: Boolean read GetHasOutputSingle;
    property OutputSingle: IXMLOutputSingle read GetOutputSingle;
    property HasOutputMultiple: Boolean read GetHasOutputMultiple;
    property OutputMultiple: IXMLOutputMultiple read GetOutputMultiple;
    property HasHasChecksEmpty: Boolean read GetHasHasChecksEmpty;
    property HasChecksEmpty: Boolean read GetHasChecksEmpty write SetHasChecksEmpty;
  end;

  IXMLOutputSingle = interface(IXMLNode)
    ['{ABB2D62A-0B4D-4E4B-8835-ED8AFE7564EA}']
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetFileName: WideString;

    procedure SetFileName(const Value: WideString);

    property FileName: WideString read GetFileName write SetFileName;
  end;

  IXMLOutputMultiple = interface(IXMLNode)
    ['{2F4918D6-EE9C-4986-9052-285A8B4D58C5}']
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetPath: WideString;
    function GetPrefix: WideString;
    function GetPostfix: WideString;

    procedure SetPath(const Value: WideString);
    procedure SetPrefix(const Value: WideString);
    procedure SetPostfix(const Value: WideString);

    property Path: WideString read GetPath write SetPath;
    property Prefix: WideString read GetPrefix write SetPrefix;
    property Postfix: WideString read GetPostfix write SetPostfix;
  end;


  { Classes for DataBindingSettings }
  TXMLDataBindingSettings = class(TX2XMLNode, IXMLDataBindingSettings)
  public
    procedure AfterConstruction; override;
  protected
    procedure XSDValidateDocument(AStrict: Boolean = False);
    function GetHasOutput: Boolean;
    function GetOutput: IXMLDataBindingOutput;
  end;

  TXMLDataBindingOutput = class(TX2XMLNode, IXSDValidate, IXSDValidateStrict, IXMLDataBindingOutput)
  public
    procedure AfterConstruction; override;
  protected
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetOutputTypeText: WideString;
    function GetOutputType: TXMLDataBindingOutputType;
    function GetHasOutputSingle: Boolean;
    function GetOutputSingle: IXMLOutputSingle;
    function GetHasOutputMultiple: Boolean;
    function GetOutputMultiple: IXMLOutputMultiple;
    function GetHasHasChecksEmpty: Boolean;
    function GetHasChecksEmpty: Boolean;

    procedure SetOutputTypeText(const Value: WideString);
    procedure SetOutputType(const Value: TXMLDataBindingOutputType);
    procedure SetHasChecksEmpty(const Value: Boolean);
  end;

  TXMLOutputSingle = class(TX2XMLNode, IXSDValidate, IXSDValidateStrict, IXMLOutputSingle)
  protected
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetFileName: WideString;

    procedure SetFileName(const Value: WideString);
  end;

  TXMLOutputMultiple = class(TX2XMLNode, IXSDValidate, IXSDValidateStrict, IXMLOutputMultiple)
  protected
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetPath: WideString;
    function GetPrefix: WideString;
    function GetPostfix: WideString;

    procedure SetPath(const Value: WideString);
    procedure SetPrefix(const Value: WideString);
    procedure SetPostfix(const Value: WideString);
  end;


  { Document functions }
  function GetDataBindingSettings(ADocument: XMLIntf.IXMLDocument): IXMLDataBindingSettings;
  function LoadDataBindingSettings(const AFileName: String): IXMLDataBindingSettings;
  function LoadDataBindingSettingsFromStream(AStream: TStream): IXMLDataBindingSettings;
  function LoadDataBindingSettingsFromString(const AString: String{$IF CompilerVersion >= 20}; AEncoding: TEncoding = nil; AOwnsEncoding: Boolean = True{$IFEND}): IXMLDataBindingSettings;
  function NewDataBindingSettings: IXMLDataBindingSettings;


const
  TargetNamespace = 'http://www.x2software.net/xsd/databinding/DataBindingSettings.xsd';


const
  DataBindingOutputTypeValues: array[TXMLDataBindingOutputType] of WideString =
                               (
                                 'Single',
                                 'Multiple'
                               );

  { Enumeration conversion helpers }
  function StringToDataBindingOutputType(const AValue: WideString): TXMLDataBindingOutputType;

implementation
uses
  Variants;

{ Document functions }
function GetDataBindingSettings(ADocument: XMLIntf.IXMLDocument): IXMLDataBindingSettings;
begin
  Result := ADocument.GetDocBinding('DataBindingSettings', TXMLDataBindingSettings, TargetNamespace) as IXMLDataBindingSettings
end;

function LoadDataBindingSettings(const AFileName: String): IXMLDataBindingSettings;
begin
  Result := LoadXMLDocument(AFileName).GetDocBinding('DataBindingSettings', TXMLDataBindingSettings, TargetNamespace) as IXMLDataBindingSettings
end;

function LoadDataBindingSettingsFromStream(AStream: TStream): IXMLDataBindingSettings;
var
  doc: XMLIntf.IXMLDocument;

begin
  doc := NewXMLDocument;
  doc.LoadFromStream(AStream);
  Result  := GetDataBindingSettings(doc);
end;

function LoadDataBindingSettingsFromString(const AString: String{$IF CompilerVersion >= 20}; AEncoding: TEncoding; AOwnsEncoding: Boolean{$IFEND}): IXMLDataBindingSettings;
var
  stream: TStringStream;

begin
  {$IF CompilerVersion >= 20}
  if Assigned(AEncoding) then
    stream := TStringStream.Create(AString, AEncoding, AOwnsEncoding)
  else
  {$IFEND}
    stream := TStringStream.Create(AString);
  try
    Result  := LoadDataBindingSettingsFromStream(stream);
  finally
    FreeAndNil(stream);
  end;
end;

function NewDataBindingSettings: IXMLDataBindingSettings;
begin
  Result := NewXMLDocument.GetDocBinding('DataBindingSettings', TXMLDataBindingSettings, TargetNamespace) as IXMLDataBindingSettings
end;



{ Enumeration conversion helpers }
function StringToDataBindingOutputType(const AValue: WideString): TXMLDataBindingOutputType;
var
  enumValue: TXMLDataBindingOutputType;

begin
  Result := TXMLDataBindingOutputType(-1);
  for enumValue := Low(TXMLDataBindingOutputType) to High(TXMLDataBindingOutputType) do
    if DataBindingOutputTypeValues[enumValue] = AValue then
    begin
      Result := enumValue;
      break;
    end;
end;


{ Implementation for DataBindingSettings }
procedure TXMLDataBindingSettings.AfterConstruction;
begin
  RegisterChildNode('Output', TXMLDataBindingOutput);
  inherited;
end;

procedure TXMLDataBindingSettings.XSDValidateDocument(AStrict: Boolean);
begin
  if AStrict then
    XMLDataBindingUtils.XSDValidateStrict(Self)
  else
    XMLDataBindingUtils.XSDValidate(Self);
end;

function TXMLDataBindingSettings.GetHasOutput: Boolean;
begin
  Result := Assigned(ChildNodes.FindNode('Output'));
end;


function TXMLDataBindingSettings.GetOutput: IXMLDataBindingOutput;
begin
  Result := (ChildNodes['Output'] as IXMLDataBindingOutput);
end;

procedure TXMLDataBindingOutput.AfterConstruction;
begin
  RegisterChildNode('OutputSingle', TXMLOutputSingle);
  RegisterChildNode('OutputMultiple', TXMLOutputMultiple);
  inherited;
end;

procedure TXMLDataBindingOutput.XSDValidate;
begin
  GetOutputType;
  SortChildNodes(Self, ['OutputType', 'OutputSingle', 'OutputMultiple', 'HasChecksEmpty']);
end;

procedure TXMLDataBindingOutput.XSDValidateStrict(AResult: IXSDValidateStrictResult);
begin
  GetOutputType;
  SortChildNodes(Self, ['OutputType', 'OutputSingle', 'OutputMultiple', 'HasChecksEmpty']);
end;

function TXMLDataBindingOutput.GetOutputTypeText: WideString;
begin
  Result := ChildNodes['OutputType'].Text;
end;


function TXMLDataBindingOutput.GetOutputType: TXMLDataBindingOutputType;
begin
  Result := StringToDataBindingOutputType(GetOutputTypeText);
end;

function TXMLDataBindingOutput.GetHasOutputSingle: Boolean;
begin
  Result := Assigned(ChildNodes.FindNode('OutputSingle'));
end;


function TXMLDataBindingOutput.GetOutputSingle: IXMLOutputSingle;
begin
  Result := (ChildNodes['OutputSingle'] as IXMLOutputSingle);
end;

function TXMLDataBindingOutput.GetHasOutputMultiple: Boolean;
begin
  Result := Assigned(ChildNodes.FindNode('OutputMultiple'));
end;


function TXMLDataBindingOutput.GetOutputMultiple: IXMLOutputMultiple;
begin
  Result := (ChildNodes['OutputMultiple'] as IXMLOutputMultiple);
end;

function TXMLDataBindingOutput.GetHasHasChecksEmpty: Boolean;
begin
  Result := Assigned(ChildNodes.FindNode('HasChecksEmpty'));
end;


function TXMLDataBindingOutput.GetHasChecksEmpty: Boolean;
begin
  Result := ChildNodes['HasChecksEmpty'].NodeValue;
end;

procedure TXMLDataBindingOutput.SetOutputTypeText(const Value: WideString);
begin
  ChildNodes['OutputType'].NodeValue := Value;
end;


procedure TXMLDataBindingOutput.SetOutputType(const Value: TXMLDataBindingOutputType);
begin
  ChildNodes['OutputType'].NodeValue := DataBindingOutputTypeValues[Value];
end;

procedure TXMLDataBindingOutput.SetHasChecksEmpty(const Value: Boolean);
begin
  ChildNodes['HasChecksEmpty'].NodeValue := BoolToXML(Value);
end;

procedure TXMLOutputSingle.XSDValidate;
begin
  CreateRequiredElements(Self, ['FileName']);
end;

procedure TXMLOutputSingle.XSDValidateStrict(AResult: IXSDValidateStrictResult);
begin
  ValidateRequiredElements(AResult, Self, ['FileName']);
end;

function TXMLOutputSingle.GetFileName: WideString;
begin
  Result := ChildNodes['FileName'].Text;
end;

procedure TXMLOutputSingle.SetFileName(const Value: WideString);
begin
  ChildNodes['FileName'].NodeValue := GetValidXMLText(Value);
end;

procedure TXMLOutputMultiple.XSDValidate;
begin
  CreateRequiredElements(Self, ['Path', 'Prefix', 'Postfix']);
  SortChildNodes(Self, ['Path', 'Prefix', 'Postfix']);
end;

procedure TXMLOutputMultiple.XSDValidateStrict(AResult: IXSDValidateStrictResult);
begin
  ValidateRequiredElements(AResult, Self, ['Path', 'Prefix', 'Postfix']);
  SortChildNodes(Self, ['Path', 'Prefix', 'Postfix']);
end;

function TXMLOutputMultiple.GetPath: WideString;
begin
  Result := ChildNodes['Path'].Text;
end;

function TXMLOutputMultiple.GetPrefix: WideString;
begin
  Result := ChildNodes['Prefix'].Text;
end;

function TXMLOutputMultiple.GetPostfix: WideString;
begin
  Result := ChildNodes['Postfix'].Text;
end;

procedure TXMLOutputMultiple.SetPath(const Value: WideString);
begin
  ChildNodes['Path'].NodeValue := GetValidXMLText(Value);
end;

procedure TXMLOutputMultiple.SetPrefix(const Value: WideString);
begin
  ChildNodes['Prefix'].NodeValue := GetValidXMLText(Value);
end;

procedure TXMLOutputMultiple.SetPostfix(const Value: WideString);
begin
  ChildNodes['Postfix'].NodeValue := GetValidXMLText(Value);
end;



end.
