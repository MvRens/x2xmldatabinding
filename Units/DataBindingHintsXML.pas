{
  X2Software XML Data Binding Wizard
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
    ['{DA83EE96-932F-45FB-A7B4-9BF68E10A082}']
    function GetHasEnumerations: Boolean;
    function GetEnumerations: IXMLEnumerations;

    property HasEnumerations: Boolean read GetHasEnumerations;
    property Enumerations: IXMLEnumerations read GetEnumerations;
  end;

  IXMLEnumerations = interface(IXMLNodeCollection)
    ['{5DD6B71B-6E29-46C0-B900-59445CF98597}']
    function Get_Enumeration(Index: Integer): IXMLEnumeration;
    function Add: IXMLEnumeration;
    function Insert(Index: Integer): IXMLEnumeration;

    property Enumeration[Index: Integer]: IXMLEnumeration read Get_Enumeration; default;
  end;

  IXMLEnumeration = interface(IXMLNodeCollection)
    ['{DA297C8A-C7A8-4BC6-8969-0939B67A584F}']
    function Get_Member(Index: Integer): IXMLMember;
    function Add: IXMLMember;
    function Insert(Index: Integer): IXMLMember;

    property Member[Index: Integer]: IXMLMember read Get_Member; default;

    function GetName: WideString;

    procedure SetName(const Value: WideString);

    property Name: WideString read GetName write SetName;
  end;

  IXMLMember = interface(IXMLNode)
    ['{BE7BEDE3-0609-437C-A699-3FB67263E88D}']
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
  XMLSchemaInstanceURI = 'http://www.w3.org/2001/XMLSchema-instance';
  TargetNamespace = '';


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
