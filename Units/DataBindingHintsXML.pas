{
  X2Software XML Data Binding

    Generated on:   14-5-2008 11:21:00
    Generated from: P:\test\XMLDataBinding\XSD\DataBindingHints.xsd
}
unit DataBindingHintsXML;

interface
uses
  Classes,
  XMLDoc,
  XMLIntf;

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

  { Interfaces for DataBindingHints }
  {
    Contains hints and mappings for the data binding output
  }
  IXMLDataBindingHints = interface(IXMLNode)
    ['{D2B7C152-7F8F-4B0F-9270-7330351B8D4E}']
    function GetHasEnumerations: Boolean;
    function GetEnumerations: IXMLEnumerations;
    function GetHasDocumentElements: Boolean;
    function GetDocumentElements: IXMLDocumentElements;
    function GetHasInterfaces: Boolean;
    function GetInterfaces: IXMLInterfaces;

    property HasEnumerations: Boolean read GetHasEnumerations;
    property Enumerations: IXMLEnumerations read GetEnumerations;
    property HasDocumentElements: Boolean read GetHasDocumentElements;
    property DocumentElements: IXMLDocumentElements read GetDocumentElements;
    property HasInterfaces: Boolean read GetHasInterfaces;
    property Interfaces: IXMLInterfaces read GetInterfaces;
  end;

  IXMLEnumerations = interface(IXMLNodeCollection)
    ['{1D5E90E0-06DD-4476-BA73-0753D35B6193}']
    function Get_Enumeration(Index: Integer): IXMLEnumeration;
    function Add: IXMLEnumeration;
    function Insert(Index: Integer): IXMLEnumeration;

    property Enumeration[Index: Integer]: IXMLEnumeration read Get_Enumeration; default;
  end;

  IXMLEnumeration = interface(IXMLNodeCollection)
    ['{07097378-D346-4809-B0A2-86C4BA09C124}']
    function Get_Member(Index: Integer): IXMLMember;
    function Add: IXMLMember;
    function Insert(Index: Integer): IXMLMember;

    property Member[Index: Integer]: IXMLMember read Get_Member; default;

    function GetSchema: WideString;
    function GetXPath: WideString;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);

    property Schema: WideString read GetSchema write SetSchema;
    property XPath: WideString read GetXPath write SetXPath;
  end;

  IXMLMember = interface(IXMLNode)
    ['{A5C711D5-FEC5-4490-A36B-A2687AB39748}']
    function GetName: WideString;

    procedure SetName(const Value: WideString);

    property Name: WideString read GetName write SetName;
  end;

  {
    If present, only elements which are included in this list will be marked as 
    a Document Element.
  }
  IXMLDocumentElements = interface(IXMLNodeCollection)
    ['{E6C9CBB2-7457-4597-939D-AAE9B1C5F42B}']
    function Get_DocumentElement(Index: Integer): IXMLDocumentElement;
    function Add: IXMLDocumentElement;
    function Insert(Index: Integer): IXMLDocumentElement;

    property DocumentElement[Index: Integer]: IXMLDocumentElement read Get_DocumentElement; default;
  end;

  IXMLDocumentElement = interface(IXMLNode)
    ['{93F6182F-4E03-420A-8E84-49F33DC29FA3}']
    function GetSchema: WideString;
    function GetXPath: WideString;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);

    property Schema: WideString read GetSchema write SetSchema;
    property XPath: WideString read GetXPath write SetXPath;
  end;

  IXMLInterfaces = interface(IXMLNodeCollection)
    ['{A18D3AFF-24FC-45FD-9583-15D9292249D2}']
    function Get_InterfaceName(Index: Integer): IXMLInterfaceName;
    function Add: IXMLInterfaceName;
    function Insert(Index: Integer): IXMLInterfaceName;

    property InterfaceName[Index: Integer]: IXMLInterfaceName read Get_InterfaceName; default;
  end;

  IXMLInterfaceName = interface(IXMLNode)
    ['{EB24ED8F-0D81-48D5-A420-438CAE003A23}']
    function GetHasSchema: Boolean;
    function GetSchema: WideString;
    function GetXPath: WideString;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);

    property HasSchema: Boolean read GetHasSchema;
    property Schema: WideString read GetSchema write SetSchema;
    property XPath: WideString read GetXPath write SetXPath;
  end;


  { Classes for DataBindingHints }
  TXMLDataBindingHints = class(TXMLNode, IXMLDataBindingHints)
  public
    procedure AfterConstruction; override;
  protected
    function GetHasEnumerations: Boolean;
    function GetEnumerations: IXMLEnumerations;
    function GetHasDocumentElements: Boolean;
    function GetDocumentElements: IXMLDocumentElements;
    function GetHasInterfaces: Boolean;
    function GetInterfaces: IXMLInterfaces;
  end;

  TXMLEnumerations = class(TXMLNodeCollection, IXMLEnumerations)
  public
    procedure AfterConstruction; override;
  protected
    function Get_Enumeration(Index: Integer): IXMLEnumeration;
    function Add: IXMLEnumeration;
    function Insert(Index: Integer): IXMLEnumeration;
  end;

  TXMLEnumeration = class(TXMLNodeCollection, IXMLEnumeration)
  public
    procedure AfterConstruction; override;
  protected
    function Get_Member(Index: Integer): IXMLMember;
    function Add: IXMLMember;
    function Insert(Index: Integer): IXMLMember;

    function GetSchema: WideString;
    function GetXPath: WideString;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);
  end;

  TXMLMember = class(TXMLNode, IXMLMember)
  protected
    function GetName: WideString;

    procedure SetName(const Value: WideString);
  end;

  TXMLDocumentElements = class(TXMLNodeCollection, IXMLDocumentElements)
  public
    procedure AfterConstruction; override;
  protected
    function Get_DocumentElement(Index: Integer): IXMLDocumentElement;
    function Add: IXMLDocumentElement;
    function Insert(Index: Integer): IXMLDocumentElement;
  end;

  TXMLDocumentElement = class(TXMLNode, IXMLDocumentElement)
  protected
    function GetSchema: WideString;
    function GetXPath: WideString;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);
  end;

  TXMLInterfaces = class(TXMLNodeCollection, IXMLInterfaces)
  public
    procedure AfterConstruction; override;
  protected
    function Get_InterfaceName(Index: Integer): IXMLInterfaceName;
    function Add: IXMLInterfaceName;
    function Insert(Index: Integer): IXMLInterfaceName;
  end;

  TXMLInterfaceName = class(TXMLNode, IXMLInterfaceName)
  protected
    function GetHasSchema: Boolean;
    function GetSchema: WideString;
    function GetXPath: WideString;

    procedure SetSchema(const Value: WideString);
    procedure SetXPath(const Value: WideString);
  end;


  { Document functions }
  function GetDataBindingHints(ADocument: IXMLDocument): IXMLDataBindingHints;
  function LoadDataBindingHints(const AFileName: String): IXMLDataBindingHints;
  function LoadDataBindingHintsFromStream(AStream: TStream): IXMLDataBindingHints;
  function NewDataBindingHints: IXMLDataBindingHints;


const
  TargetNamespace = 'http://www.x2software.net/xsd/databinding/DataBindingHints.xsd';


implementation
uses
  SysUtils;

{ Document functions }
function GetDataBindingHints(ADocument: IXMLDocument): IXMLDataBindingHints;
begin
  Result := ADocument.GetDocBinding('DataBindingHints', TXMLDataBindingHints, TargetNamespace) as IXMLDataBindingHints
end;

function LoadDataBindingHints(const AFileName: String): IXMLDataBindingHints;
begin
  Result := LoadXMLDocument(AFileName).GetDocBinding('DataBindingHints', TXMLDataBindingHints, TargetNamespace) as IXMLDataBindingHints
end;

function LoadDataBindingHintsFromStream(AStream: TStream): IXMLDataBindingHints;
var
  doc: IXMLDocument;

begin
  doc := NewXMLDocument;
  doc.LoadFromStream(AStream);
  Result  := GetDataBindingHints(doc);
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
  inherited;
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

procedure TXMLEnumerations.AfterConstruction;
begin
  RegisterChildNode('Enumeration', TXMLEnumeration);

  ItemTag := 'Enumeration';
  ItemInterface := IXMLEnumeration;

  inherited;
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

procedure TXMLEnumeration.AfterConstruction;
begin
  RegisterChildNode('Member', TXMLMember);

  ItemTag := 'Member';
  ItemInterface := IXMLMember;

  inherited;
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

procedure TXMLEnumeration.SetSchema(const Value: WideString);
begin
  SetAttribute('Schema', Value);
end;

procedure TXMLEnumeration.SetXPath(const Value: WideString);
begin
  SetAttribute('XPath', Value);
end;

function TXMLMember.GetName: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLMember.SetName(const Value: WideString);
begin
  SetAttribute('Name', Value);
end;

procedure TXMLDocumentElements.AfterConstruction;
begin
  RegisterChildNode('DocumentElement', TXMLDocumentElement);

  ItemTag := 'DocumentElement';
  ItemInterface := IXMLDocumentElement;

  inherited;
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
  SetAttribute('Schema', Value);
end;

procedure TXMLDocumentElement.SetXPath(const Value: WideString);
begin
  SetAttribute('XPath', Value);
end;

procedure TXMLInterfaces.AfterConstruction;
begin
  RegisterChildNode('InterfaceName', TXMLInterfaceName);

  ItemTag := 'InterfaceName';
  ItemInterface := IXMLInterfaceName;

  inherited;
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

function TXMLInterfaceName.GetHasSchema: Boolean;
begin
  Result := Assigned(ChildNodes.FindNode('Schema'));
end;


function TXMLInterfaceName.GetSchema: WideString;
begin
  Result := AttributeNodes['Schema'].Text;
end;

function TXMLInterfaceName.GetXPath: WideString;
begin
  Result := AttributeNodes['XPath'].Text;
end;

procedure TXMLInterfaceName.SetSchema(const Value: WideString);
begin
  SetAttribute('Schema', Value);
end;

procedure TXMLInterfaceName.SetXPath(const Value: WideString);
begin
  SetAttribute('XPath', Value);
end;



end.
