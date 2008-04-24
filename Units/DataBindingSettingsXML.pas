{
  X2Software XML Data Binding

    Generated on:   24-4-2008 11:37:27
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
    ['{C78D63A5-77C2-4547-AC37-5311160D543B}']
    function GetHasOutput: Boolean;
    function GetOutput: IXMLDataBindingOutput;

    property HasOutput: Boolean read GetHasOutput;
    property Output: IXMLDataBindingOutput read GetOutput;
  end;

  {
    Contains the user-defined output settings last used
  }
  IXMLDataBindingOutput = interface(IXMLNode)
    ['{81374819-83EF-42A8-A7B8-2F59A470D77B}']
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
    ['{9BB52722-C7C0-45F8-81A1-59BE074BF62E}']
    function GetFileName: WideString;

    procedure SetFileName(const Value: WideString);

    property FileName: WideString read GetFileName write SetFileName;
  end;

  IXMLOutputMultiple = interface(IXMLNode)
    ['{4B5AC82E-572A-4C21-B779-4626BF79E0E6}']
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
  TargetNamespace = 'http://www.x2software.net/xsd/databinding/DataBindingSettings.xsd';


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
  Result := ChildNodes['OutputType'].Text;
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
