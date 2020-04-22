{
  X2Software XML Data Binding

    Generated on:   22/04/2020 11:59:03
    Generated from: P:\x2xmldatabinding\XSD\DataBindingHints.xsd
}
unit DataBindingHintsXML;

interface
uses
  Classes,
  SysUtils,
  XMLDoc,
  XMLIntf,
  XMLDataBindingUtils;

type
  { Forward declarations for DataBindingHints }
  IXMLDataBindingHints = interface;
  IXMLEnumerations = interface;
  IXMLEnumeration = interface;
  IXMLMember = interface;
  IXMLDocumentElements = interface;
  IXMLDocumentElement = interface;
  IXMLInterfaces = interface;
  IXMLInterfaceName = interface;
  IXMLProperties = interface;
  IXMLPropertyName = interface;

  { Interfaces for DataBindingHints }
  {
    Contains hints and mappings for the data binding output
  }
  IXMLDataBindingHints = interface(IXMLNode)
    ['{8122E348-4BE1-4436-AD2A-DAFED0CFA0C4}']
    procedure XSDValidateDocument(AStrict: Boolean = False);
    function GetHasEnumerations: Boolean;
    function GetEnumerations: IXMLEnumerations;
    function GetHasDocumentElements: Boolean;
    function GetDocumentElements: IXMLDocumentElements;
    function GetHasInterfaces: Boolean;
    function GetInterfaces: IXMLInterfaces;
    function GetHasProperties: Boolean;
    function GetProperties: IXMLProperties;

    property HasEnumerations: Boolean read GetHasEnumerations;
    property Enumerations: IXMLEnumerations read GetEnumerations;
    property HasDocumentElements: Boolean read GetHasDocumentElements;
    property DocumentElements: IXMLDocumentElements read GetDocumentElements;
    property HasInterfaces: Boolean read GetHasInterfaces;
    property Interfaces: IXMLInterfaces read GetInterfaces;
    property HasProperties: Boolean read GetHasProperties;
    property Properties: IXMLProperties read GetProperties;
  end;

  IXMLEnumerationsEnumerator = interface
    ['{725760E4-70A5-47A6-B91B-C240F1FADA60}']
    function GetCurrent: IXMLEnumeration;
    function MoveNext: Boolean;
    property Current: IXMLEnumeration read GetCurrent;
  end;


  IXMLEnumerations = interface(IXMLNodeCollection)
    ['{509EAF84-A4BF-4F15-B77C-98B58792A9C3}']
    function GetEnumerator: IXMLEnumerationsEnumerator;

    function Get_Enumeration(Index: Integer): IXMLEnumeration;
    function Add: IXMLEnumeration;
    function Insert(Index: Integer): IXMLEnumeration;

    property Enumeration[Index: Integer]: IXMLEnumeration read Get_Enumeration; default;
  end;

  IXMLEnumerationEnumerator = interface
    ['{8A1AF158-2AF3-49FB-9CFC-46897D8AF28C}']
    function GetCurrent: IXMLMember;
    function MoveNext: Boolean;
    property Current: IXMLMember read GetCurrent;
  end;


  IXMLEnumeration = interface(IXMLNodeCollection)
    ['{01A5E078-6EEB-40A0-BF72-972B467AD983}']
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetEnumerator: IXMLEnumerationEnumerator;

    function Get_Member(Index: Integer): IXMLMember;
    function Add: IXMLMember;
    function Insert(Index: Integer): IXMLMember;

    property Member[Index: Integer]: IXMLMember read Get_Member; default;

    function GetSchema: WideString;
    function GetXPath: WideString;
    function GetHasReplaceMembers: Boolean;
    function GetReplaceMembers: Boolean;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);
    procedure SetReplaceMembers(const Value: Boolean);

    property Schema: WideString read GetSchema write SetSchema;
    property XPath: WideString read GetXPath write SetXPath;
    property HasReplaceMembers: Boolean read GetHasReplaceMembers;
    property ReplaceMembers: Boolean read GetReplaceMembers write SetReplaceMembers;
  end;

  IXMLMember = interface(IXMLNode)
    ['{C58EF7F8-E182-47A3-B591-550A51AA0751}']
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetName: WideString;
    function GetValue: WideString;

    procedure SetName(const Value: WideString);
    procedure SetValue(const Value: WideString);

    property Name: WideString read GetName write SetName;
    property Value: WideString read GetValue write SetValue;
  end;

  IXMLDocumentElementsEnumerator = interface
    ['{D58F3363-3E98-4EF9-9A9E-F57B7C4639D7}']
    function GetCurrent: IXMLDocumentElement;
    function MoveNext: Boolean;
    property Current: IXMLDocumentElement read GetCurrent;
  end;


  {
    If present, only elements which are included in this list will be marked as 
    a Document Element.
  }
  IXMLDocumentElements = interface(IXMLNodeCollection)
    ['{D991E86F-3D42-4B05-BB90-10AF42324FE1}']
    function GetEnumerator: IXMLDocumentElementsEnumerator;

    function Get_DocumentElement(Index: Integer): IXMLDocumentElement;
    function Add: IXMLDocumentElement;
    function Insert(Index: Integer): IXMLDocumentElement;

    property DocumentElement[Index: Integer]: IXMLDocumentElement read Get_DocumentElement; default;
  end;

  IXMLDocumentElement = interface(IXMLNode)
    ['{0FD90406-67B2-4076-870C-47DA8E8582ED}']
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetSchema: WideString;
    function GetXPath: WideString;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);

    property Schema: WideString read GetSchema write SetSchema;
    property XPath: WideString read GetXPath write SetXPath;
  end;

  IXMLInterfacesEnumerator = interface
    ['{A1433E41-A316-4DBD-ADC2-7EA490CEFCB3}']
    function GetCurrent: IXMLInterfaceName;
    function MoveNext: Boolean;
    property Current: IXMLInterfaceName read GetCurrent;
  end;


  IXMLInterfaces = interface(IXMLNodeCollection)
    ['{6A2EDBB5-36FE-4CA6-B3B9-1AC7A016E372}']
    function GetEnumerator: IXMLInterfacesEnumerator;

    function Get_InterfaceName(Index: Integer): IXMLInterfaceName;
    function Add: IXMLInterfaceName;
    function Insert(Index: Integer): IXMLInterfaceName;

    property InterfaceName[Index: Integer]: IXMLInterfaceName read Get_InterfaceName; default;
  end;

  IXMLInterfaceName = interface(IXMLNode)
    ['{F0057FA9-92D8-47C5-AC29-6A045595B7F0}']
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetSchema: WideString;
    function GetXPath: WideString;
    function GetValue: WideString;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);
    procedure SetValue(const Value: WideString);

    property Schema: WideString read GetSchema write SetSchema;
    property XPath: WideString read GetXPath write SetXPath;
    property Value: WideString read GetValue write SetValue;
  end;

  IXMLPropertiesEnumerator = interface
    ['{4FDE9618-2177-4D53-8B14-5E81E397E084}']
    function GetCurrent: IXMLPropertyName;
    function MoveNext: Boolean;
    property Current: IXMLPropertyName read GetCurrent;
  end;


  IXMLProperties = interface(IXMLNodeCollection)
    ['{38320F29-9D8C-4158-B0F6-8E1D1FD31EB9}']
    function GetEnumerator: IXMLPropertiesEnumerator;

    function Get_PropertyName(Index: Integer): IXMLPropertyName;
    function Add: IXMLPropertyName;
    function Insert(Index: Integer): IXMLPropertyName;

    property PropertyName[Index: Integer]: IXMLPropertyName read Get_PropertyName; default;
  end;

  IXMLPropertyName = interface(IXMLNode)
    ['{F99D3125-B2DE-4A4A-88D0-DBCB50C75C59}']
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetSchema: WideString;
    function GetXPath: WideString;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);

    property Schema: WideString read GetSchema write SetSchema;
    property XPath: WideString read GetXPath write SetXPath;
  end;


  { Classes for DataBindingHints }
  TXMLDataBindingHints = class(TX2XMLNode, IXMLDataBindingHints)
  public
    procedure AfterConstruction; override;
  protected
    procedure XSDValidateDocument(AStrict: Boolean = False);
    function GetHasEnumerations: Boolean;
    function GetEnumerations: IXMLEnumerations;
    function GetHasDocumentElements: Boolean;
    function GetDocumentElements: IXMLDocumentElements;
    function GetHasInterfaces: Boolean;
    function GetInterfaces: IXMLInterfaces;
    function GetHasProperties: Boolean;
    function GetProperties: IXMLProperties;
  end;

  TXMLEnumerationsEnumerator = class(TXMLNodeCollectionEnumerator, IXMLEnumerationsEnumerator)
  protected
    function GetCurrent: IXMLEnumeration;
  end;


  TXMLEnumerations = class(TX2XMLNodeCollection, IXMLEnumerations)
  public
    procedure AfterConstruction; override;
  protected
    function GetEnumerator: IXMLEnumerationsEnumerator;

    function Get_Enumeration(Index: Integer): IXMLEnumeration;
    function Add: IXMLEnumeration;
    function Insert(Index: Integer): IXMLEnumeration;
  end;

  TXMLEnumerationEnumerator = class(TXMLNodeCollectionEnumerator, IXMLEnumerationEnumerator)
  protected
    function GetCurrent: IXMLMember;
  end;


  TXMLEnumeration = class(TX2XMLNodeCollection, IXSDValidate, IXSDValidateStrict, IXMLEnumeration)
  public
    procedure AfterConstruction; override;
  protected
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetEnumerator: IXMLEnumerationEnumerator;

    function Get_Member(Index: Integer): IXMLMember;
    function Add: IXMLMember;
    function Insert(Index: Integer): IXMLMember;

    function GetSchema: WideString;
    function GetXPath: WideString;
    function GetHasReplaceMembers: Boolean;
    function GetReplaceMembers: Boolean;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);
    procedure SetReplaceMembers(const Value: Boolean);
  end;

  TXMLMember = class(TX2XMLNode, IXSDValidate, IXSDValidateStrict, IXMLMember)
  protected
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetName: WideString;
    function GetValue: WideString;

    procedure SetName(const Value: WideString);
    procedure SetValue(const Value: WideString);
  end;

  TXMLDocumentElementsEnumerator = class(TXMLNodeCollectionEnumerator, IXMLDocumentElementsEnumerator)
  protected
    function GetCurrent: IXMLDocumentElement;
  end;


  TXMLDocumentElements = class(TX2XMLNodeCollection, IXMLDocumentElements)
  public
    procedure AfterConstruction; override;
  protected
    function GetEnumerator: IXMLDocumentElementsEnumerator;

    function Get_DocumentElement(Index: Integer): IXMLDocumentElement;
    function Add: IXMLDocumentElement;
    function Insert(Index: Integer): IXMLDocumentElement;
  end;

  TXMLDocumentElement = class(TX2XMLNode, IXSDValidate, IXSDValidateStrict, IXMLDocumentElement)
  protected
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetSchema: WideString;
    function GetXPath: WideString;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);
  end;

  TXMLInterfacesEnumerator = class(TXMLNodeCollectionEnumerator, IXMLInterfacesEnumerator)
  protected
    function GetCurrent: IXMLInterfaceName;
  end;


  TXMLInterfaces = class(TX2XMLNodeCollection, IXMLInterfaces)
  public
    procedure AfterConstruction; override;
  protected
    function GetEnumerator: IXMLInterfacesEnumerator;

    function Get_InterfaceName(Index: Integer): IXMLInterfaceName;
    function Add: IXMLInterfaceName;
    function Insert(Index: Integer): IXMLInterfaceName;
  end;

  TXMLInterfaceName = class(TX2XMLNode, IXSDValidate, IXSDValidateStrict, IXMLInterfaceName)
  protected
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetSchema: WideString;
    function GetXPath: WideString;
    function GetValue: WideString;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);
    procedure SetValue(const Value: WideString);
  end;

  TXMLPropertiesEnumerator = class(TXMLNodeCollectionEnumerator, IXMLPropertiesEnumerator)
  protected
    function GetCurrent: IXMLPropertyName;
  end;


  TXMLProperties = class(TX2XMLNodeCollection, IXMLProperties)
  public
    procedure AfterConstruction; override;
  protected
    function GetEnumerator: IXMLPropertiesEnumerator;

    function Get_PropertyName(Index: Integer): IXMLPropertyName;
    function Add: IXMLPropertyName;
    function Insert(Index: Integer): IXMLPropertyName;
  end;

  TXMLPropertyName = class(TX2XMLNode, IXSDValidate, IXSDValidateStrict, IXMLPropertyName)
  protected
    procedure XSDValidate;
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);

    function GetSchema: WideString;
    function GetXPath: WideString;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);
  end;


  { Document functions }
  function GetDataBindingHints(ADocument: XMLIntf.IXMLDocument): IXMLDataBindingHints;
  function LoadDataBindingHints(const AFileName: String): IXMLDataBindingHints;
  function LoadDataBindingHintsFromStream(AStream: TStream): IXMLDataBindingHints;
  function LoadDataBindingHintsFromString(const AString: String{$IF CompilerVersion >= 20}; AEncoding: TEncoding = nil; AOwnsEncoding: Boolean = True{$IFEND}): IXMLDataBindingHints;
  function NewDataBindingHints: IXMLDataBindingHints;


const
  TargetNamespace = 'http://www.x2software.net/xsd/databinding/DataBindingHints.xsd';


implementation
uses
  Variants;

{ Document functions }
function GetDataBindingHints(ADocument: XMLIntf.IXMLDocument): IXMLDataBindingHints;
begin
  Result := ADocument.GetDocBinding('DataBindingHints', TXMLDataBindingHints, TargetNamespace) as IXMLDataBindingHints
end;

function LoadDataBindingHints(const AFileName: String): IXMLDataBindingHints;
begin
  Result := LoadXMLDocument(AFileName).GetDocBinding('DataBindingHints', TXMLDataBindingHints, TargetNamespace) as IXMLDataBindingHints
end;

function LoadDataBindingHintsFromStream(AStream: TStream): IXMLDataBindingHints;
var
  doc: XMLIntf.IXMLDocument;

begin
  doc := NewXMLDocument;
  doc.LoadFromStream(AStream);
  Result  := GetDataBindingHints(doc);
end;

function LoadDataBindingHintsFromString(const AString: String{$IF CompilerVersion >= 20}; AEncoding: TEncoding; AOwnsEncoding: Boolean{$IFEND}): IXMLDataBindingHints;
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
    Result  := LoadDataBindingHintsFromStream(stream);
  finally
    FreeAndNil(stream);
  end;
end;

function NewDataBindingHints: IXMLDataBindingHints;
begin
  Result := NewXMLDocument.GetDocBinding('DataBindingHints', TXMLDataBindingHints, TargetNamespace) as IXMLDataBindingHints
end;



{ Implementation for DataBindingHints }
procedure TXMLDataBindingHints.AfterConstruction;
begin
  RegisterChildNode('Enumerations', TXMLEnumerations);
  RegisterChildNode('DocumentElements', TXMLDocumentElements);
  RegisterChildNode('Interfaces', TXMLInterfaces);
  RegisterChildNode('Properties', TXMLProperties);
  inherited;
end;

procedure TXMLDataBindingHints.XSDValidateDocument(AStrict: Boolean);
begin
  if AStrict then
    XMLDataBindingUtils.XSDValidateStrict(Self)
  else
    XMLDataBindingUtils.XSDValidate(Self);
end;

function TXMLDataBindingHints.GetHasEnumerations: Boolean;
begin
  Result := Assigned(ChildNodes.FindNode('Enumerations'));
end;


function TXMLDataBindingHints.GetEnumerations: IXMLEnumerations;
begin
  Result := (ChildNodes['Enumerations'] as IXMLEnumerations);
end;

function TXMLDataBindingHints.GetHasDocumentElements: Boolean;
begin
  Result := Assigned(ChildNodes.FindNode('DocumentElements'));
end;


function TXMLDataBindingHints.GetDocumentElements: IXMLDocumentElements;
begin
  Result := (ChildNodes['DocumentElements'] as IXMLDocumentElements);
end;

function TXMLDataBindingHints.GetHasInterfaces: Boolean;
begin
  Result := Assigned(ChildNodes.FindNode('Interfaces'));
end;


function TXMLDataBindingHints.GetInterfaces: IXMLInterfaces;
begin
  Result := (ChildNodes['Interfaces'] as IXMLInterfaces);
end;

function TXMLDataBindingHints.GetHasProperties: Boolean;
begin
  Result := Assigned(ChildNodes.FindNode('Properties'));
end;


function TXMLDataBindingHints.GetProperties: IXMLProperties;
begin
  Result := (ChildNodes['Properties'] as IXMLProperties);
end;

function TXMLEnumerationsEnumerator.GetCurrent: IXMLEnumeration;
begin
  Result := (inherited GetCurrent as IXMLEnumeration);
end;

procedure TXMLEnumerations.AfterConstruction;
begin
  RegisterChildNode('Enumeration', TXMLEnumeration);

  ItemTag := 'Enumeration';
  ItemInterface := IXMLEnumeration;

  inherited;
end;

function TXMLEnumerations.GetEnumerator: IXMLEnumerationsEnumerator;
begin
  Result := TXMLEnumerationsEnumerator.Create(Self);
end;

function TXMLEnumerations.Get_Enumeration(Index: Integer): IXMLEnumeration;
begin
  Result := (List[Index] as IXMLEnumeration);
end;

function TXMLEnumerations.Add: IXMLEnumeration;
begin
  Result := (AddItem(-1) as IXMLEnumeration);
end;

function TXMLEnumerations.Insert(Index: Integer): IXMLEnumeration;
begin
  Result := (AddItem(Index) as IXMLEnumeration);
end;

function TXMLEnumerationEnumerator.GetCurrent: IXMLMember;
begin
  Result := (inherited GetCurrent as IXMLMember);
end;

procedure TXMLEnumeration.AfterConstruction;
begin
  RegisterChildNode('Member', TXMLMember);

  ItemTag := 'Member';
  ItemInterface := IXMLMember;

  inherited;
end;

procedure TXMLEnumeration.XSDValidate;
begin
  CreateRequiredAttributes(Self, ['Schema', 'XPath']);
end;

procedure TXMLEnumeration.XSDValidateStrict(AResult: IXSDValidateStrictResult);
begin
  ValidateRequiredAttributes(AResult, Self, ['Schema', 'XPath']);
end;

function TXMLEnumeration.GetEnumerator: IXMLEnumerationEnumerator;
begin
  Result := TXMLEnumerationEnumerator.Create(Self);
end;

function TXMLEnumeration.Get_Member(Index: Integer): IXMLMember;
begin
  Result := (List[Index] as IXMLMember);
end;

function TXMLEnumeration.Add: IXMLMember;
begin
  Result := (AddItem(-1) as IXMLMember);
end;

function TXMLEnumeration.Insert(Index: Integer): IXMLMember;
begin
  Result := (AddItem(Index) as IXMLMember);
end;

function TXMLEnumeration.GetSchema: WideString;
begin
  Result := AttributeNodes['Schema'].Text;
end;

function TXMLEnumeration.GetXPath: WideString;
begin
  Result := AttributeNodes['XPath'].Text;
end;

function TXMLEnumeration.GetHasReplaceMembers: Boolean;
begin
  Result := Assigned(AttributeNodes.FindNode('ReplaceMembers'));
end;


function TXMLEnumeration.GetReplaceMembers: Boolean;
begin
  Result := AttributeNodes['ReplaceMembers'].NodeValue;
end;

procedure TXMLEnumeration.SetSchema(const Value: WideString);
begin
  SetAttribute('Schema', GetValidXMLText(Value));
end;

procedure TXMLEnumeration.SetXPath(const Value: WideString);
begin
  SetAttribute('XPath', GetValidXMLText(Value));
end;

procedure TXMLEnumeration.SetReplaceMembers(const Value: Boolean);
begin
  SetAttribute('ReplaceMembers', BoolToXML(Value));
end;

procedure TXMLMember.XSDValidate;
begin
  CreateRequiredAttributes(Self, ['Name']);
end;

procedure TXMLMember.XSDValidateStrict(AResult: IXSDValidateStrictResult);
begin
  ValidateRequiredAttributes(AResult, Self, ['Name']);
end;

function TXMLMember.GetName: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

function TXMLMember.GetValue: WideString;
begin
  Result := VarToStr(GetNodeValue);
end;

procedure TXMLMember.SetName(const Value: WideString);
begin
  SetAttribute('Name', GetValidXMLText(Value));
end;

procedure TXMLMember.SetValue(const Value: WideString);
begin
  SetNodeValue(GetValidXMLText(Value));
end;

function TXMLDocumentElementsEnumerator.GetCurrent: IXMLDocumentElement;
begin
  Result := (inherited GetCurrent as IXMLDocumentElement);
end;

procedure TXMLDocumentElements.AfterConstruction;
begin
  RegisterChildNode('DocumentElement', TXMLDocumentElement);

  ItemTag := 'DocumentElement';
  ItemInterface := IXMLDocumentElement;

  inherited;
end;

function TXMLDocumentElements.GetEnumerator: IXMLDocumentElementsEnumerator;
begin
  Result := TXMLDocumentElementsEnumerator.Create(Self);
end;

function TXMLDocumentElements.Get_DocumentElement(Index: Integer): IXMLDocumentElement;
begin
  Result := (List[Index] as IXMLDocumentElement);
end;

function TXMLDocumentElements.Add: IXMLDocumentElement;
begin
  Result := (AddItem(-1) as IXMLDocumentElement);
end;

function TXMLDocumentElements.Insert(Index: Integer): IXMLDocumentElement;
begin
  Result := (AddItem(Index) as IXMLDocumentElement);
end;

procedure TXMLDocumentElement.XSDValidate;
begin
  CreateRequiredAttributes(Self, ['Schema', 'XPath']);
end;

procedure TXMLDocumentElement.XSDValidateStrict(AResult: IXSDValidateStrictResult);
begin
  ValidateRequiredAttributes(AResult, Self, ['Schema', 'XPath']);
end;

function TXMLDocumentElement.GetSchema: WideString;
begin
  Result := AttributeNodes['Schema'].Text;
end;

function TXMLDocumentElement.GetXPath: WideString;
begin
  Result := AttributeNodes['XPath'].Text;
end;

procedure TXMLDocumentElement.SetSchema(const Value: WideString);
begin
  SetAttribute('Schema', GetValidXMLText(Value));
end;

procedure TXMLDocumentElement.SetXPath(const Value: WideString);
begin
  SetAttribute('XPath', GetValidXMLText(Value));
end;

function TXMLInterfacesEnumerator.GetCurrent: IXMLInterfaceName;
begin
  Result := (inherited GetCurrent as IXMLInterfaceName);
end;

procedure TXMLInterfaces.AfterConstruction;
begin
  RegisterChildNode('InterfaceName', TXMLInterfaceName);

  ItemTag := 'InterfaceName';
  ItemInterface := IXMLInterfaceName;

  inherited;
end;

function TXMLInterfaces.GetEnumerator: IXMLInterfacesEnumerator;
begin
  Result := TXMLInterfacesEnumerator.Create(Self);
end;

function TXMLInterfaces.Get_InterfaceName(Index: Integer): IXMLInterfaceName;
begin
  Result := (List[Index] as IXMLInterfaceName);
end;

function TXMLInterfaces.Add: IXMLInterfaceName;
begin
  Result := (AddItem(-1) as IXMLInterfaceName);
end;

function TXMLInterfaces.Insert(Index: Integer): IXMLInterfaceName;
begin
  Result := (AddItem(Index) as IXMLInterfaceName);
end;

procedure TXMLInterfaceName.XSDValidate;
begin
  CreateRequiredAttributes(Self, ['Schema', 'XPath']);
end;

procedure TXMLInterfaceName.XSDValidateStrict(AResult: IXSDValidateStrictResult);
begin
  ValidateRequiredAttributes(AResult, Self, ['Schema', 'XPath']);
end;

function TXMLInterfaceName.GetSchema: WideString;
begin
  Result := AttributeNodes['Schema'].Text;
end;

function TXMLInterfaceName.GetXPath: WideString;
begin
  Result := AttributeNodes['XPath'].Text;
end;

function TXMLInterfaceName.GetValue: WideString;
begin
  Result := VarToStr(GetNodeValue);
end;

procedure TXMLInterfaceName.SetSchema(const Value: WideString);
begin
  SetAttribute('Schema', GetValidXMLText(Value));
end;

procedure TXMLInterfaceName.SetXPath(const Value: WideString);
begin
  SetAttribute('XPath', GetValidXMLText(Value));
end;

procedure TXMLInterfaceName.SetValue(const Value: WideString);
begin
  SetNodeValue(GetValidXMLText(Value));
end;

function TXMLPropertiesEnumerator.GetCurrent: IXMLPropertyName;
begin
  Result := (inherited GetCurrent as IXMLPropertyName);
end;

procedure TXMLProperties.AfterConstruction;
begin
  RegisterChildNode('PropertyName', TXMLPropertyName);

  ItemTag := 'PropertyName';
  ItemInterface := IXMLPropertyName;

  inherited;
end;

function TXMLProperties.GetEnumerator: IXMLPropertiesEnumerator;
begin
  Result := TXMLPropertiesEnumerator.Create(Self);
end;

function TXMLProperties.Get_PropertyName(Index: Integer): IXMLPropertyName;
begin
  Result := (List[Index] as IXMLPropertyName);
end;

function TXMLProperties.Add: IXMLPropertyName;
begin
  Result := (AddItem(-1) as IXMLPropertyName);
end;

function TXMLProperties.Insert(Index: Integer): IXMLPropertyName;
begin
  Result := (AddItem(Index) as IXMLPropertyName);
end;

procedure TXMLPropertyName.XSDValidate;
begin
  CreateRequiredAttributes(Self, ['Schema', 'XPath']);
end;

procedure TXMLPropertyName.XSDValidateStrict(AResult: IXSDValidateStrictResult);
begin
  ValidateRequiredAttributes(AResult, Self, ['Schema', 'XPath']);
end;

function TXMLPropertyName.GetSchema: WideString;
begin
  Result := AttributeNodes['Schema'].Text;
end;

function TXMLPropertyName.GetXPath: WideString;
begin
  Result := AttributeNodes['XPath'].Text;
end;

procedure TXMLPropertyName.SetSchema(const Value: WideString);
begin
  SetAttribute('Schema', GetValidXMLText(Value));
end;

procedure TXMLPropertyName.SetXPath(const Value: WideString);
begin
  SetAttribute('XPath', GetValidXMLText(Value));
end;



end.
