unit DelphiXMLDataBindingResources;

interface
type
  TDelphiXMLSection = (dxsForward, dxsInterface, dxsClass, dxsImplementation);
  TDelphiXMLMember = (dxmPropertyGet, dxmPropertySet, dxmPropertyDeclaration);


const
  CrLf  = #13#10;
  
  UnitHeader          = '{'                                                     + CrLf +
                        '  X2Software XML Data Binding Wizard'                  + CrLf +
                        '    Generated from: %<SourceFileName>:s'               + CrLf +
                        '}'                                                     + CrLf +
                        'unit %<UnitName>:s;'                                   + CrLf +
                        ''                                                      + CrLf;

  UnitInterface       = 'interface'                                             + CrLf +
                        'uses'                                                  + CrLf +
                        '  Classes,'                                            + CrLf +
                        '  XMLDoc,'                                             + CrLf +
                        '  XMLIntf;'                                            + CrLf +
                        ''                                                      + CrLf +
                        'type'                                                  + CrLf;

  UnitImplementation  = 'implementation'                                        + CrLf +
                        ''                                                      + CrLf;
                        
  UnitFooter          = ''                                                      + CrLf +
                        'end.'                                                  + CrLf;



  DocumentBinding                 = 'GetDocBinding(''%<SourceName>:s'', TXML%<Name>:s, TargetNamespace) as IXML%<Name>:s';

  DocumentFunctionsInterface      = '  function Get%<Name>:s(ADocument: IXMLDocument): IXML%<Name>:s;'      + CrLf +
                                    '  function Load%<Name>:s(const AFileName: String): IXML%<Name>:s;'     + CrLf +
                                    '  function Load%<Name>:sFromStream(AStream: TStream): IXML%<Name>:s;'  + CrLf +
                                    '  function New%<Name>:s: IXML%<Name>:s;'                               + CrLf;

  DocumentFunctionsImplementation = 'function Get%<Name>:s(ADocument: IXMLDocument): IXML%<Name>:s;'        + CrLf +
                                    'begin'                                                                 + CrLf +
                                    '  Result := ADocument.' + DocumentBinding                              + CrLf +
                                    'end;'                                                                  + CrLf +
                                    ''                                                                      + CrLf +
                                    'function Load%<Name>:s(const AFileName: String): IXML%<Name>:s;'       + CrLf +
                                    'begin'                                                                 + CrLf +
                                    '  Result := LoadXMLDocument(AFileName).' + DocumentBinding             + CrLf +
                                    'end;'                                                                  + CrLf +
                                    ''                                                                      + CrLf +
                                    'function Load%<Name>:sFromStream(AStream: TStream): IXML%<Name>:s;'    + CrLf +
                                    'var'                                                                   + CrLf +
                                    '  doc: IXMLDocument;'                                                  + CrLf +
                                    ''                                                                      + CrLf +
                                    'begin'                                                                 + CrLf +
                                    '  doc := NewXMLDocument;'                                              + CrLf +
                                    '  doc.LoadFromStream(AStream);'                                        + CrLf +
                                    '  Result  := Get%<Name>:s(doc);'                                       + CrLf +
                                    'end;'                                                                  + CrLf +
                                    ''                                                                      + CrLf +
                                    'function New%<Name>:s: IXML%<Name>:s;'                                 + CrLf +
                                    'begin'                                                                 + CrLf +
                                    '  Result := NewXMLDocument.' + DocumentBinding                         + CrLf +
                                    'end;'                                                                  + CrLf +
                                    ''                                                                      + CrLf;


  PropertyIntfMethodGetOptional = '    function GetHas%<PropertyName>:s: Boolean;';
  PropertyIntfMethodGetText     = '    function Get%<PropertyName>:sText: WideString;';
  PropertyIntfMethodGet         = '    function Get%<PropertyName>:s: %<DataType>:s;';
  PropertyIntfMethodSetText     = '    procedure Set%<PropertyName>:sText(const Value: WideString);';
  PropertyIntfMethodSet         = '    procedure Set%<PropertyName>:s(const Value: %<DataType>:s);';

  PropertyInterfaceOptional     = '    property Has%<PropertyName>:s: Boolean read GetHas%<PropertyName>:s;';
  PropertyInterfaceTextReadOnly = '    property %<PropertyName>:sText: WideString read Get%<PropertyName>:sText;';
  PropertyInterfaceReadOnly     = '    property %<PropertyName>:s: %<DataType>:s read Get%<PropertyName>:s;';
  PropertyInterfaceText         = '    property %<PropertyName>:sText: WideString read Get%<PropertyName>:sText write Set%<PropertyName>:sText;';
  PropertyInterface             = '    property %<PropertyName>:s: %<DataType>:s read Get%<PropertyName>:s write Set%<PropertyName>:s;';

  PropertyImplMethodGetOptional = 'function TXML%<Name>:s.GetHas%<PropertyName>:s: Boolean;'                + CrLf +
                                  'begin'                                                                   + CrLf +
                                  '  Result := Assigned(ChildNodes.FindNode(''%<PropertySourceName>:s''));' + CrLf +
                                  'end;'                                                                    + CrLf +
                                  ''                                                                        + CrLf;

  PropertyImplMethodGetText     = 'function TXML%<Name>:s.Get%<PropertyName>:sText: WideString;'            + CrLf +
                                  'begin'                                                                   + CrLf +
                                  '  Result := ChildNodes[''%<PropertySourceName>:s''].NodeValue;'          + CrLf +
                                  'end;'                                                                    + CrLf +
                                  ''                                                                        + CrLf;

  PropertyImplMethodSetText     = 'procedure TXML%<Name>:s.Set%<PropertyName>:sText(const Value: WideString);'  + CrLf +
                                  'begin'                                                                       + CrLf +
                                  '  ChildNodes[''%<PropertySourceName>:s''].NodeValue := Value;'               + CrLf +
                                  'end;'                                                                        + CrLf +
                                  ''                                                                            + CrLf;


  SectionComments:  array[TDelphiXMLSection] of String =
                    (
                      '  { Forward declarations for %<SchemaName>:s }',
                      '  { Interfaces for %<SchemaName>:s }',
                      '  { Classes for %<SchemaName>:s }',
                      '{ Implementation for %<SchemaName>:s }'
                    );




  PrefixInterface         = 'IXML';
  PrefixClass             = 'TXML';


  InterfaceItemForward    = '  IXML%<Name>:s = interface;';
  InterfaceItemInterface  = '  IXML%<Name>:s = interface(%<ParentName>:s)';
  InterfaceItemClass      = '  TXML%<Name>:s = class(%<ParentName>:s, IXML%<Name>:s)';


  CollectionInterface     = 'IXMLNodeCollection';
  CollectionClass         = 'TXMLNodeCollection';

  ItemInterface           = 'IXMLNode';
  ItemClass               = 'TXMLNode';



  // #ToDo1 (MvR) 9-3-2008: document / node / etc
  // #ToDo1 (MvR) 9-3-2008: WideString etc ?
  ReservedWords:  array[0..111] of String =
                  (
                    'absolute', 'abstract', 'and', 'array', 'as', 'asm',
                    'assembler', 'automated', 'begin', 'case', 'cdecl', 'class',
                    'const', 'constructor', 'contains', 'default', 'deprecated',
                    'destructor', 'dispid', 'dispinterface', 'div', 'do',
                    'downto', 'dynamic', 'else', 'end', 'except', 'export',
                    'exports', 'external', 'far', 'file', 'final', 'finalization',
                    'finally', 'for', 'forward', 'function', 'goto', 'if',
                    'implementation', 'implements', 'in', 'index', 'inherited',
                    'initialization', 'inline', 'interface', 'is', 'label',
                    'library', 'local', 'message', 'mod', 'name', 'near',
                    'nil', 'nodefault', 'not', 'object', 'of', 'or', 'out',
                    'overload', 'override', 'package', 'packed', 'pascal',
                    'platform', 'private', 'procedure', 'program', 'property',
                    'protected', 'public', 'published', 'raise', 'read',
                    'readonly', 'record', 'register', 'reintroduce', 'repeat',
                    'requires', 'resident', 'resourcestring', 'safecall',
                    'sealed', 'set', 'shl', 'shr', 'static', 'stdcall',
                    'stored', 'string', 'then', 'threadvar', 'to', 'try', 'type',
                    'unit', 'unsafe', 'until', 'uses', 'var', 'varargs',
                    'virtual', 'while', 'with', 'write', 'writeonly', 'xor'
                  );


type
  TTypeConversion   = (tcNone, tcBoolean, tcFloat, tcDateTime);
  TTypeConversions  = set of TTypeConversion;

  TTypeMapping  = record
    SchemaName:   String;
    DelphiName:   String;
    Conversion:   TTypeConversion;
  end;


const
  SimpleTypeMapping:  array[0..9] of TTypeMapping =
                      (
                        (SchemaName:  'int';        DelphiName:  'Integer';     Conversion:    tcNone),
                        (SchemaName:  'integer';    DelphiName:  'Integer';     Conversion:    tcNone),
                        (SchemaName:  'short';      DelphiName:  'Smallint';    Conversion:    tcNone),
                        (SchemaName:  'date';       DelphiName:  'TDateTime';   Conversion:    tcDateTime),
                        (SchemaName:  'time';       DelphiName:  'TDateTime';   Conversion:    tcDateTime),
                        (SchemaName:  'dateTime';   DelphiName:  'TDateTime';   Conversion:    tcDateTime),
                        (SchemaName:  'float';      DelphiName:  'Double';      Conversion:    tcFloat),
                        (SchemaName:  'double';     DelphiName:  'Double';      Conversion:    tcFloat),
                        (SchemaName:  'boolean';    DelphiName:  'Boolean';     Conversion:    tcBoolean),
                        (SchemaName:  'string';     DelphiName:  'WideString';  Conversion:    tcNone)
                      );



  TypeConversionNone  = '  %<Destination>:s := %<Source>:s;';


  TypeConversionVariables:  array[TTypeConversion] of String =
                            (
                              { tcNone }      '',
                              { tcBoolean }   '',
                              { tcFloat }     '',
                              { tcDateTime }  ''
                            );

  TypeConversionToNative:   array[TTypeConversion] of String =
                            (
                              { tcNone }      TypeConversionNone,
                              { tcBoolean }   TypeConversionNone,
                              { tcFloat }     TypeConversionNone,
                              { tcDateTime }  TypeConversionNone
                            );

  TypeConversionToXML:      array[TTypeConversion] of String =
                            (
                              { tcNone }      TypeConversionNone,
                              { tcBoolean }   '  %<Destination>:s := LowerCase(BoolToStr(%<Source>:s, True));',
                              { tcFloat }     TypeConversionNone,
                              { tcDateTime }  TypeConversionNone
                            );


implementation
end.

