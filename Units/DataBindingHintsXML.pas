{
  X2Software XML Data Binding

    Generated on:   24-4-2008 11:37:14
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

  { Interfaces for DataBindingHints }
  {
    Contains hints and mappings for the data binding output
  }
  IXMLDataBindingHints = interface(IXMLNode)
    ['{BF3AC439-748A-4051-B05D-31067CDF0781}']
    function GetHasEnumerations: Boolean;
    function GetEnumerations: IXMLEnumerations;

    property HasEnumerations: Boolean read GetHasEnumerations;
    property Enumerations: IXMLEnumerations read GetEnumerations;
  end;

  IXMLEnumerations = interface(IXMLNodeCollection)
    ['{12A3082B-138D-4F00-8D53-AEE76E4A9AD9}']
    function Get_Enumeration(Index: Integer): IXMLEnumeration;
    function Add: IXMLEnumeration;
    function Insert(Index: Integer): IXMLEnumeration;

    property Enumeration[Index: Integer]: IXMLEnumeration read Get_Enumeration; default;
  end;

  IXMLEnumeration = interface(IXMLNodeCollection)
    ['{BAF25450-A88E-42A7-A466-652E5EA90D1F}']
    function Get_Member(Index: Integer): IXMLMember;
    function Add: IXMLMember;
    function Insert(Index: Integer): IXMLMember;

    property Member[Index: Integer]: IXMLMember read Get_Member; default;

    function GetName: WideString;

    procedure SetName(const Value: WideString);

    property Name: WideString read GetName write SetName;
  end;

  IXMLMember = interface(IXMLNode)
    ['{202F3AB6-9908-4B87-9271-16B737BFC7CB}']
    function GetName: WideString;

    procedure SetName(const Value: WideString);

    property Name: WideString read GetName write SetName;
  end;


  { Classes for DataBindingHints }
  TXMLDataBindingHints = class(TXMLNode, IXMLDataBindingHints)
  public
    procedure AfterConstruction; override;
  protected
    function GetHasEnumerations: Boolean;
    function GetEnumerations: IXMLEnumerations;
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

    function GetName: WideString;

    procedure SetName(const Value: WideString);
  end;

  TXMLMember = class(TXMLNode, IXMLMember)
  protected
    function GetName: WideString;

    procedure SetName(const Value: WideString);
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

function TXMLEnumeration.GetName: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLEnumeration.SetName(const Value: WideString);
begin
  SetAttribute('Name', Value);
end;

function TXMLMember.GetName: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLMember.SetName(const Value: WideString);
begin
  SetAttribute('Name', Value);
end;



end.
