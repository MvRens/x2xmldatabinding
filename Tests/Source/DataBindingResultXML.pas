
{****************************************************************************************}
{                                                                                        }
{                                    XML Data Binding                                    }
{                                                                                        }
{         Generated on: 10-3-2008 20:12:45                                               }
{       Generated from: F:\Archive\2007\XMLDataBinding\Tests\XSD\DataBindingResult.xsd   }
{                                                                                        }
{****************************************************************************************}

unit DataBindingResultXML;

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLDataBindingResult = interface;
  IXMLSchemas = interface;
  IXMLSchema = interface;
  IXMLItems = interface;
  IXMLItem = interface;
  IXMLInterface_ = interface;
  IXMLCollection = interface;
  IXMLEnumeration = interface;

{ IXMLDataBindingResult }

  IXMLDataBindingResult = interface(IXMLNode)
    ['{B62DB507-8C4B-4966-BE94-F862B6546389}']
    { Property Accessors }
    function Get_Schemas: IXMLSchemas;
    { Methods & Properties }
    property Schemas: IXMLSchemas read Get_Schemas;
  end;

{ IXMLSchemas }

  IXMLSchemas = interface(IXMLNodeCollection)
    ['{EBDC76EA-3887-4479-8359-2D8038878707}']
    { Property Accessors }
    function Get_Schema(Index: Integer): IXMLSchema;
    { Methods & Properties }
    function Add: IXMLSchema;
    function Insert(const Index: Integer): IXMLSchema;
    property Schema[Index: Integer]: IXMLSchema read Get_Schema; default;
  end;

{ IXMLSchema }

  IXMLSchema = interface(IXMLNode)
    ['{6C82BA3F-537E-4112-BE1E-85B50482D4C1}']
    { Property Accessors }
    function Get_Name: WideString;
    function Get_Items: IXMLItems;
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property Name: WideString read Get_Name write Set_Name;
    property Items: IXMLItems read Get_Items;
  end;

{ IXMLItems }

  IXMLItems = interface(IXMLNodeCollection)
    ['{4A1633BF-D402-4A8B-8DA2-0C7E06EE899F}']
    { Property Accessors }
    function Get_Item(Index: Integer): IXMLItem;
    { Methods & Properties }
    function Add: IXMLItem;
    function Insert(const Index: Integer): IXMLItem;
    property Item[Index: Integer]: IXMLItem read Get_Item; default;
  end;

{ IXMLItem }

  IXMLItem = interface(IXMLNode)
    ['{934648C4-4E29-4F45-B2AF-2BDC0B82A80A}']
    { Property Accessors }
    function Get_ItemType: WideString;
    function Get_Name: WideString;
    function Get_Interface_: IXMLInterface_;
    function Get_Collection: IXMLCollection;
    function Get_Enumeration: IXMLEnumeration;
    procedure Set_ItemType(Value: WideString);
    procedure Set_Name(Value: WideString);
    { Methods & Properties }
    property ItemType: WideString read Get_ItemType write Set_ItemType;
    property Name: WideString read Get_Name write Set_Name;
    property Interface_: IXMLInterface_ read Get_Interface_;
    property Collection: IXMLCollection read Get_Collection;
    property Enumeration: IXMLEnumeration read Get_Enumeration;
  end;

{ IXMLInterface_ }

  IXMLInterface_ = interface(IXMLNode)
    ['{F480A9C8-0B74-4CB7-A26C-C66A9ACA533B}']
  end;

{ IXMLCollection }

  IXMLCollection = interface(IXMLNode)
    ['{0D31D0E8-EE5F-4804-86F3-C3B3CA271F79}']
    { Property Accessors }
    function Get_ItemName: WideString;
    procedure Set_ItemName(Value: WideString);
    { Methods & Properties }
    property ItemName: WideString read Get_ItemName write Set_ItemName;
  end;

{ IXMLEnumeration }

  IXMLEnumeration = interface(IXMLNode)
    ['{3E1E8C1C-073B-4860-AC03-23EF7954C13D}']
  end;

{ Forward Decls }

  TXMLDataBindingResult = class;
  TXMLSchemas = class;
  TXMLSchema = class;
  TXMLItems = class;
  TXMLItem = class;
  TXMLInterface_ = class;
  TXMLCollection = class;
  TXMLEnumeration = class;

{ TXMLDataBindingResult }

  TXMLDataBindingResult = class(TXMLNode, IXMLDataBindingResult)
  protected
    { IXMLDataBindingResult }
    function Get_Schemas: IXMLSchemas;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSchemas }

  TXMLSchemas = class(TXMLNodeCollection, IXMLSchemas)
  protected
    { IXMLSchemas }
    function Get_Schema(Index: Integer): IXMLSchema;
    function Add: IXMLSchema;
    function Insert(const Index: Integer): IXMLSchema;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSchema }

  TXMLSchema = class(TXMLNode, IXMLSchema)
  protected
    { IXMLSchema }
    function Get_Name: WideString;
    function Get_Items: IXMLItems;
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLItems }

  TXMLItems = class(TXMLNodeCollection, IXMLItems)
  protected
    { IXMLItems }
    function Get_Item(Index: Integer): IXMLItem;
    function Add: IXMLItem;
    function Insert(const Index: Integer): IXMLItem;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLItem }

  TXMLItem = class(TXMLNode, IXMLItem)
  protected
    { IXMLItem }
    function Get_ItemType: WideString;
    function Get_Name: WideString;
    function Get_Interface_: IXMLInterface_;
    function Get_Collection: IXMLCollection;
    function Get_Enumeration: IXMLEnumeration;
    procedure Set_ItemType(Value: WideString);
    procedure Set_Name(Value: WideString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLInterface_ }

  TXMLInterface_ = class(TXMLNode, IXMLInterface_)
  protected
    { IXMLInterface_ }
  end;

{ TXMLCollection }

  TXMLCollection = class(TXMLNode, IXMLCollection)
  protected
    { IXMLCollection }
    function Get_ItemName: WideString;
    procedure Set_ItemName(Value: WideString);
  end;

{ TXMLEnumeration }

  TXMLEnumeration = class(TXMLNode, IXMLEnumeration)
  protected
    { IXMLEnumeration }
  end;

{ Global Functions }

function GetDataBindingResult(Doc: IXMLDocument): IXMLDataBindingResult;
function LoadDataBindingResult(const FileName: WideString): IXMLDataBindingResult;
function NewDataBindingResult: IXMLDataBindingResult;

const
  TargetNamespace = '';

implementation

{ Global Functions }

function GetDataBindingResult(Doc: IXMLDocument): IXMLDataBindingResult;
begin
  Result := Doc.GetDocBinding('DataBindingResult', TXMLDataBindingResult, TargetNamespace) as IXMLDataBindingResult;
end;

function LoadDataBindingResult(const FileName: WideString): IXMLDataBindingResult;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('DataBindingResult', TXMLDataBindingResult, TargetNamespace) as IXMLDataBindingResult;
end;

function NewDataBindingResult: IXMLDataBindingResult;
begin
  Result := NewXMLDocument.GetDocBinding('DataBindingResult', TXMLDataBindingResult, TargetNamespace) as IXMLDataBindingResult;
end;

{ TXMLDataBindingResult }

procedure TXMLDataBindingResult.AfterConstruction;
begin
  RegisterChildNode('Schemas', TXMLSchemas);
  inherited;
end;

function TXMLDataBindingResult.Get_Schemas: IXMLSchemas;
begin
  Result := ChildNodes['Schemas'] as IXMLSchemas;
end;

{ TXMLSchemas }

procedure TXMLSchemas.AfterConstruction;
begin
  RegisterChildNode('Schema', TXMLSchema);
  ItemTag := 'Schema';
  ItemInterface := IXMLSchema;
  inherited;
end;

function TXMLSchemas.Get_Schema(Index: Integer): IXMLSchema;
begin
  Result := List[Index] as IXMLSchema;
end;

function TXMLSchemas.Add: IXMLSchema;
begin
  Result := AddItem(-1) as IXMLSchema;
end;

function TXMLSchemas.Insert(const Index: Integer): IXMLSchema;
begin
  Result := AddItem(Index) as IXMLSchema;
end;

{ TXMLSchema }

procedure TXMLSchema.AfterConstruction;
begin
  RegisterChildNode('Items', TXMLItems);
  inherited;
end;

function TXMLSchema.Get_Name: WideString;
begin
  Result := ChildNodes['Name'].Text;
end;

procedure TXMLSchema.Set_Name(Value: WideString);
begin
  ChildNodes['Name'].NodeValue := Value;
end;

function TXMLSchema.Get_Items: IXMLItems;
begin
  Result := ChildNodes['Items'] as IXMLItems;
end;

{ TXMLItems }

procedure TXMLItems.AfterConstruction;
begin
  RegisterChildNode('Item', TXMLItem);
  ItemTag := 'Item';
  ItemInterface := IXMLItem;
  inherited;
end;

function TXMLItems.Get_Item(Index: Integer): IXMLItem;
begin
  Result := List[Index] as IXMLItem;
end;

function TXMLItems.Add: IXMLItem;
begin
  Result := AddItem(-1) as IXMLItem;
end;

function TXMLItems.Insert(const Index: Integer): IXMLItem;
begin
  Result := AddItem(Index) as IXMLItem;
end;

{ TXMLItem }

procedure TXMLItem.AfterConstruction;
begin
  RegisterChildNode('Interface', TXMLInterface_);
  RegisterChildNode('Collection', TXMLCollection);
  RegisterChildNode('Enumeration', TXMLEnumeration);
  inherited;
end;

function TXMLItem.Get_ItemType: WideString;
begin
  Result := ChildNodes['ItemType'].Text;
end;

procedure TXMLItem.Set_ItemType(Value: WideString);
begin
  ChildNodes['ItemType'].NodeValue := Value;
end;

function TXMLItem.Get_Name: WideString;
begin
  Result := ChildNodes['Name'].Text;
end;

procedure TXMLItem.Set_Name(Value: WideString);
begin
  ChildNodes['Name'].NodeValue := Value;
end;

function TXMLItem.Get_Interface_: IXMLInterface_;
begin
  Result := ChildNodes['Interface'] as IXMLInterface_;
end;

function TXMLItem.Get_Collection: IXMLCollection;
begin
  Result := ChildNodes['Collection'] as IXMLCollection;
end;

function TXMLItem.Get_Enumeration: IXMLEnumeration;
begin
  Result := ChildNodes['Enumeration'] as IXMLEnumeration;
end;

{ TXMLInterface_ }

{ TXMLCollection }

function TXMLCollection.Get_ItemName: WideString;
begin
  Result := ChildNodes['ItemName'].Text;
end;

procedure TXMLCollection.Set_ItemName(Value: WideString);
begin
  ChildNodes['ItemName'].NodeValue := Value;
end;

{ TXMLEnumeration }

end.
