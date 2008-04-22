{
  X2Software XML Data Binding Wizard
    Generated from: P:\test\XMLDataBinding\XSD\DataBindingSettings.xsd
}
unit DataBindingSettingsXML;

interface
uses
  Classes,
  XMLDoc,
  XMLIntf;

type
  { Forward declarations for DataBindingSettings }
  IXMLDataBindingSettings = interface;
  IXMLDataBindingOutput = interface;
  TXMLOutputType = (OutputType_Single,
                    OutputType_Multiple);
  IXMLOutputSingle = interface;
  IXMLOutputMultiple = interface;

  { Interfaces for DataBindingSettings }
  {
    Contains the settings and hints for the Delphi XML Data Binding.
  }
  IXMLDataBindingSettings = interface(IXMLNode)
    ['{2F402DC3-E73C-487E-A921-357A99CF717F}']
    function GetHasOutput: Boolean;
    function GetOutput: IXMLDataBindingOutput;

    property HasOutput: Boolean read GetHasOutput;
    property Output: IXMLDataBindingOutput read GetOutput;
  end;

  {
    Contains the user-defined output settings last used
  }
  IXMLDataBindingOutput = interface(IXMLNode)
    ['{812D7883-4F30-4B28-AA38-B107A99C90EC}']
    function GetOutputTypeText: WideString;
    function GetOutputType: TXMLOutputType;
    function GetHasOutputSingle: Boolean;
    function GetOutputSingle: IXMLOutputSingle;
    function GetHasOutputMultiple: Boolean;
    function GetOutputMultiple: IXMLOutputMultiple;

    procedure SetOutputTypeText(const Value: WideString);
    procedure SetOutputType(const Value: TXMLOutputType);

    property OutputTypeText: WideString read GetOutputTypeText write SetOutputTypeText;
    property OutputType: TXMLOutputType read GetOutputType write SetOutputType;
    property HasOutputSingle: Boolean read GetHasOutputSingle;
    property OutputSingle: IXMLOutputSingle read GetOutputSingle;
    property HasOutputMultiple: Boolean read GetHasOutputMultiple;
    property OutputMultiple: IXMLOutputMultiple read GetOutputMultiple;
  end;

  IXMLOutputSingle = interface(IXMLNode)
    ['{025F89C0-0036-44DD-B0FC-833D572B668E}']
    function GetFileName: WideString;

    procedure SetFileName(const Value: WideString);

    property FileName: WideString read GetFileName write SetFileName;
  end;

  IXMLOutputMultiple = interface(IXMLNode)
    ['{ABF68B77-E356-42DC-9166-72AA956EDA8E}']
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
  TXMLDataBindingSettings = class(TXMLNode, IXMLDataBindingSettings)
  public
    procedure AfterConstruction; override;
  protected
    function GetHasOutput: Boolean;
    function GetOutput: IXMLDataBindingOutput;
  end;

  TXMLDataBindingOutput = class(TXMLNode, IXMLDataBindingOutput)
  public
    procedure AfterConstruction; override;
  protected
    function GetOutputTypeText: WideString;
    function GetOutputType: TXMLOutputType;
    function GetHasOutputSingle: Boolean;
    function GetOutputSingle: IXMLOutputSingle;
    function GetHasOutputMultiple: Boolean;
    function GetOutputMultiple: IXMLOutputMultiple;

    procedure SetOutputTypeText(const Value: WideString);
    procedure SetOutputType(const Value: TXMLOutputType);
  end;

  TXMLOutputSingle = class(TXMLNode, IXMLOutputSingle)
  protected
    function GetFileName: WideString;

    procedure SetFileName(const Value: WideString);
  end;

  TXMLOutputMultiple = class(TXMLNode, IXMLOutputMultiple)
  protected
    function GetPath: WideString;
    function GetPrefix: WideString;
    function GetPostfix: WideString;

    procedure SetPath(const Value: WideString);
    procedure SetPrefix(const Value: WideString);
    procedure SetPostfix(const Value: WideString);
  end;


  { Document functions }
  function GetDataBindingSettings(ADocument: IXMLDocument): IXMLDataBindingSettings;
  function LoadDataBindingSettings(const AFileName: String): IXMLDataBindingSettings;
  function LoadDataBindingSettingsFromStream(AStream: TStream): IXMLDataBindingSettings;
  function NewDataBindingSettings: IXMLDataBindingSettings;


const
  XMLSchemaInstanceURI = 'http://www.w3.org/2001/XMLSchema-instance';
  TargetNamespace = '';


const
  OutputTypeValues: array[TXMLOutputType] of WideString =
                    (
                      'Single',
                      'Multiple'
                    );

  { Enumeration conversion helpers }
  function StringToOutputType(const AValue: WideString): TXMLOutputType;

implementation
uses
  SysUtils;


{ Document functions }
function GetDataBindingSettings(ADocument: IXMLDocument): IXMLDataBindingSettings;
begin
  Result := ADocument.GetDocBinding('DataBindingSettings', TXMLDataBindingSettings, TargetNamespace) as IXMLDataBindingSettings
end;

function LoadDataBindingSettings(const AFileName: String): IXMLDataBindingSettings;
begin
  Result := LoadXMLDocument(AFileName).GetDocBinding('DataBindingSettings', TXMLDataBindingSettings, TargetNamespace) as IXMLDataBindingSettings
end;

function LoadDataBindingSettingsFromStream(AStream: TStream): IXMLDataBindingSettings;
var
  doc: IXMLDocument;

begin
  doc := NewXMLDocument;
  doc.LoadFromStream(AStream);
  Result  := GetDataBindingSettings(doc);
end;

function NewDataBindingSettings: IXMLDataBindingSettings;
begin
  Result := NewXMLDocument.GetDocBinding('DataBindingSettings', TXMLDataBindingSettings, TargetNamespace) as IXMLDataBindingSettings
end;



{ Enumeration conversion helpers }
function StringToOutputType(const AValue: WideString): TXMLOutputType;
var
  enumValue: TXMLOutputType;

begin
  Result := TXMLOutputType(-1);
  for enumValue := Low(TXMLOutputType) to High(TXMLOutputType) do
    if OutputTypeValues[enumValue] = AValue then
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

function TXMLDataBindingOutput.GetOutputTypeText: WideString;
begin
  Result := ChildNodes['OutputType'].NodeValue;
end;


function TXMLDataBindingOutput.GetOutputType: TXMLOutputType;
begin
  Result := StringToOutputType(GetOutputTypeText);
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

procedure TXMLDataBindingOutput.SetOutputTypeText(const Value: WideString);
begin
  ChildNodes['OutputType'].NodeValue := Value;
end;


procedure TXMLDataBindingOutput.SetOutputType(const Value: TXMLOutputType);
begin
  ChildNodes['OutputType'].NodeValue := OutputTypeValues[Value];
end;

function TXMLOutputSingle.GetFileName: WideString;
begin
  Result := ChildNodes['FileName'].Text;
end;

procedure TXMLOutputSingle.SetFileName(const Value: WideString);
begin
  ChildNodes['FileName'].NodeValue := Value;
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
  ChildNodes['Path'].NodeValue := Value;
end;

procedure TXMLOutputMultiple.SetPrefix(const Value: WideString);
begin
  ChildNodes['Prefix'].NodeValue := Value;
end;

procedure TXMLOutputMultiple.SetPostfix(const Value: WideString);
begin
  ChildNodes['Postfix'].NodeValue := Value;
end;



end.
