{
  X2Software XML Data Binding

    Generated on:   25-4-2008 10:37:37
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

  { Interfaces for DataBindingHints }
  {
    Contains hints and mappings for the data binding output
  }
  IXMLDataBindingHints = interface(IXMLNode)
    ['{33A3ED30-3F1C-4607-A848-D3F17297687F}']
    function GetHasEnumerations: Boolean;
    function GetEnumerations: IXMLEnumerations;
    function GetHasDocumentElements: Boolean;
    function GetDocumentElements: IXMLDocumentElements;

    property HasEnumerations: Boolean read GetHasEnumerations;
    property Enumerations: IXMLEnumerations read GetEnumerations;
    property HasDocumentElements: Boolean read GetHasDocumentElements;
    property DocumentElements: IXMLDocumentElements read GetDocumentElements;
  end;

  IXMLEnumerations = interface(IXMLNodeCollection)
    ['{BD382537-6E8E-4821-A6FB-598234A7B646}']
    function Get_Enumeration(Index: Integer): IXMLEnumeration;
    function Add: IXMLEnumeration;
    function Insert(Index: Integer): IXMLEnumeration;

    property Enumeration[Index: Integer]: IXMLEnumeration read Get_Enumeration; default;
  end;

  IXMLEnumeration = interface(IXMLNodeCollection)
    ['{DC00C775-25B9-4612-A712-9D2DAC346415}']
    function Get_Member(Index: Integer): IXMLMember;
    function Add: IXMLMember;
    function Insert(Index: Integer): IXMLMember;

    property Member[Index: Integer]: IXMLMember read Get_Member; default;

    function GetName: WideString;

    procedure SetName(const Value: WideString);

    property Name: WideString read GetName write SetName;
  end;

  IXMLMember = interface(IXMLNode)
    ['{C242311F-B6B6-44B6-BAF2-40EBE6501963}']
    function GetName: WideString;

    procedure SetName(const Value: WideString);

    property Name: WideString read GetName write SetName;
  end;

  {
    If present, only elements which are included in this list will be marked as 
    a Document Element.
  }
  IXMLDocumentElements = interface(IXMLNodeCollection)
    ['{A2036427-9FCE-41DF-B254-4BFBA42258AA}']
    function Get_DocumentElement(Index: Integer): IXMLDocumentElement;
    function Add: IXMLDocumentElement;
    function Insert(Index: Integer): IXMLDocumentElement;

    property DocumentElement[Index: Integer]: IXMLDocumentElement read Get_DocumentElement; default;
  end;

  IXMLDocumentElement = interface(IXMLNode)
    ['{DBC9940F-A0A3-42A4-83CF-AD90BD0892E5}']
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
    function GetHasDocumentElements: Boolean;
    function GetDocumentElements: IXMLDocumentElements;
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
  RegisterChildNode('DocumentElements', TXMLDocumentElements);
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

function TXMLDocumentElement.GetName: WideString;
begin
  Result := AttributeNodes['Name'].Text;
end;

procedure TXMLDocumentElement.SetName(const Value: WideString);
begin
  SetAttribute('Name', Value);
end;



end.
