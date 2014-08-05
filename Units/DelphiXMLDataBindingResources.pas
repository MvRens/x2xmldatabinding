unit DelphiXMLDataBindingResources;

// #ToDo1 -oMvR: 6-4-2012: namespace support voor attributes

interface
type
  TDelphiXMLSection = (dxsForward, dxsInterface, dxsClass, dxsImplementation);
  TDelphiXMLMember = (dxmPropertyGet, dxmPropertySet, dxmPropertyDeclaration);
  TDelphiAccessor = (daGet, daSet);
  TDelphiNodeType = (dntElement, dntElementNS, dntAttribute, dntNodeValue, dntCustom);
  TDelphiElementType = dntElement..dntElementNS;


const
  CrLf  = #13#10;
  
  UnitHeader          = '{'                                                     + CrLf +
                        '  X2Software XML Data Binding'                         + CrLf +
                        ''                                                      + CrLf +
                        '    Generated on:   %<DateTime>:s'                     + CrLf +
                        '    Generated from: %<SourceFileName>:s'               + CrLf +
                        '}'                                                     + CrLf +
                        'unit %<UnitName>:s;'                                   + CrLf +
                        ''                                                      + CrLf;

  UnitInterface       = 'interface'                                             + CrLf +
                        'uses'                                                  + CrLf +
                        '%<UsesClause>:s'                                       +
                        '  Classes,'                                            + CrLf +
                        '  XMLDoc,'                                             + CrLf +
                        '  XMLIntf,'                                            + CrLf +
                        '  XMLDataBindingUtils;'                                + CrLf +
                        ''                                                      + CrLf +
                        'type'                                                  + CrLf;

  UnitImplementation  = 'implementation'                                        + CrLf;

  UnitFooter          = ''                                                      + CrLf +
                        'end.'                                                  + CrLf;



  DocumentBinding                 = 'GetDocBinding(''%<SourceName>:s'', TXML%<Name>:s, TargetNamespace) as IXML%<Name>:s';

  DocumentFunctionsInterface      = '  function Get%<Name>:s(ADocument: XMLIntf.IXMLDocument): IXML%<Name>:s;'  + CrLf +
                                    '  function Load%<Name>:s(const AFileName: String): IXML%<Name>:s;'         + CrLf +
                                    '  function Load%<Name>:sFromStream(AStream: TStream): IXML%<Name>:s;'      + CrLf +
                                    '  function Load%<Name>:sFromString(const AString: String): IXML%<Name>:s;' + CrLf +
                                    '  function New%<Name>:s: IXML%<Name>:s;'                                   + CrLf;

  DocumentFunctionsImplementation = 'function Get%<Name>:s(ADocument: XMLIntf.IXMLDocument): IXML%<Name>:s;'  + CrLf +
                                    'begin'                                                                   + CrLf +
                                    '  Result := ADocument.' + DocumentBinding                                + CrLf +
                                    'end;'                                                                    + CrLf +
                                    ''                                                                        + CrLf +
                                    'function Load%<Name>:s(const AFileName: String): IXML%<Name>:s;'         + CrLf +
                                    'begin'                                                                   + CrLf +
                                    '  Result := LoadXMLDocument(AFileName).' + DocumentBinding               + CrLf +
                                    'end;'                                                                    + CrLf +
                                    ''                                                                        + CrLf +
                                    'function Load%<Name>:sFromStream(AStream: TStream): IXML%<Name>:s;'      + CrLf +
                                    'var'                                                                     + CrLf +
                                    '  doc: XMLIntf.IXMLDocument;'                                            + CrLf +
                                    ''                                                                        + CrLf +
                                    'begin'                                                                   + CrLf +
                                    '  doc := NewXMLDocument;'                                                + CrLf +
                                    '  doc.LoadFromStream(AStream);'                                          + CrLf +
                                    '  Result  := Get%<Name>:s(doc);'                                         + CrLf +
                                    'end;'                                                                    + CrLf +
                                    ''                                                                        + CrLf +
                                    'function Load%<Name>:sFromString(const AString: String): IXML%<Name>:s;' + CrLf +
                                    'var'                                                                     + CrLf +
                                    '  stream: TStringStream;'                                                + CrLf +
                                    ''                                                                        + CrLf +
                                    'begin'                                                                   + CrLf +
                                    '  stream := TStringStream.Create(AString);'                              + CrLf +
                                    '  try'                                                                   + CrLf +
                                    '    Result  := Load%<Name>:sFromStream(stream);'                         + CrLf +
                                    '  finally'                                                               + CrLf +
                                    '    FreeAndNil(stream);'                                                 + CrLf +
                                    '  end;'                                                                  + CrLf +
                                    'end;'                                                                    + CrLf +
                                    ''                                                                        + CrLf +
                                    'function New%<Name>:s: IXML%<Name>:s;'                                   + CrLf +
                                    'begin'                                                                   + CrLf +
                                    '  Result := NewXMLDocument.' + DocumentBinding                           + CrLf +
                                    'end;'                                                                    + CrLf +
                                    ''                                                                        + CrLf;



  XSDValidateInterface                    = 'IXSDValidate';

  XSDValidateDocumentMethodInterface      = '    procedure XSDValidateDocument;';

  XSDValidateDocumentMethodImplementation = 'procedure TXML%<Name>:s.XSDValidateDocument;'                  + CrLf +
                                            'begin'                                                         + CrLf +
                                            '  XMLDataBindingUtils.XSDValidate(Self);'                      + CrLf +
                                            'end;'                                                          + CrLf;


  XSDValidateMethodInterface              = '    procedure XSDValidate;';

  XSDValidateMethodImplementationBegin    = 'procedure TXML%<Name>:s.XSDValidate;'                          + CrLf +
                                            'begin';

  XSDValidateMethodImplementationRequired     = '  CreateRequiredElements(Self, [%<RequiredElements>:s]);';
  XSDValidateMethodImplementationComplex      = '  Get%<Name>:s;';
  XSDValidateMethodImplementationAttrib       = '  CreateRequiredAttributes(Self, [%<RequiredAttributes>:s]);';
  XSDValidateMethodImplementationSort         = '  SortChildNodes(Self, [%<SortOrder>:s]);';
  XSDValidateMethodImplementationSortNewLine  = #13#10 + '    ';

  XSDValidateMethodImplementationEnd      = 'end;' + CrLf;


  EnumeratorMethodInterface       = '    function GetEnumerator: IXML%<Name>:sEnumerator;';
  EnumeratorMethodImplementation  = 'function TXML%<Name>:s.GetEnumerator: IXML%<Name>:sEnumerator;' + CrLf +
                                    'begin' + CrLf +
                                    '  Result := TXML%<Name>:sEnumerator.Create(Self);' + CrLf +
                                    'end;' + CrLf;


  EnumeratorInterface = '  IXML%<Name>:sEnumerator = interface' + CrLf +
                        '    %<GUID>:s' + CrLf +
                        '    function GetCurrent: %<DataType>:s;' + CrLf +
                        '    function MoveNext: Boolean;' + CrLf +
                        '    property Current: %<DataType>:s read GetCurrent;' + CrLf +
                        '  end;' + CrLf;


  EnumeratorClass     = '  TXML%<Name>:sEnumerator = class(TXMLNodeCollectionEnumerator, IXML%<Name>:sEnumerator)' + CrLf +
                        '  protected' + CrLf +
                        '    function GetCurrent: %<DataType>:s;' + CrLf +
                        '  end;' + CrLf;

  EnumeratorImplementation = 'function TXML%<Name>:sEnumerator.GetCurrent: %<DataType>:s;' + CrLf +
                             'begin' + CrLf +
                             '  Result := (inherited GetCurrent as %<DataType>:s);' + CrLf +
                             'end;' + CrLf;

  PropertyIntfMethodGetOptional = '    function GetHas%<PropertyName>:s: Boolean;';
  PropertyIntfMethodGetNil      = '    function Get%<PropertyName>:sIsNil: Boolean;';
  PropertyIntfMethodGetText     = '    function Get%<PropertyName>:sText: WideString;';
  PropertyIntfMethodGet         = '    function Get%<PropertyName>:s: %<DataType>:s;';
  PropertyIntfMethodSetNil      = '    procedure Set%<PropertyName>:sIsNil(const Value: Boolean);';
  PropertyIntfMethodSetText     = '    procedure Set%<PropertyName>:sText(const Value: WideString);';
  PropertyIntfMethodSet         = '    procedure Set%<PropertyName>:s(const Value: %<DataType>:s);';
  PropertyIntfMethodStream      = '    procedure Save%<PropertyName>:sToStream(AStream: TStream);';
  PropertyIntfMethodFile        = '    procedure Save%<PropertyName>:sToFile(const AFileName: string);';

  PropertyInterfaceOptional     = '    property Has%<PropertyName>:s: Boolean read GetHas%<PropertyName>:s;';
  PropertyInterfaceNilReadOnly  = '    property %<PropertyName>:sIsNil: Boolean read Get%<PropertyName>:sIsNil;';
  PropertyInterfaceNil          = '    property %<PropertyName>:sIsNil: Boolean read Get%<PropertyName>:sIsNil write Set%<PropertyName>:sIsNil;';
  PropertyInterfaceTextReadOnly = '    property %<PropertyName>:sText: WideString read Get%<PropertyName>:sText;';
  PropertyInterfaceReadOnly     = '    property %<PropertyName>:s: %<DataType>:s read Get%<PropertyName>:s;';
  PropertyInterfaceText         = '    property %<PropertyName>:sText: WideString read Get%<PropertyName>:sText write Set%<PropertyName>:sText;';
  PropertyInterface             = '    property %<PropertyName>:s: %<DataType>:s read Get%<PropertyName>:s write Set%<PropertyName>:s;';

  PropertyImplMethodGetOptional: array[TDelphiElementType] of string =
                                 (
                                   { dntElement }
                                   'function TXML%<Name>:s.GetHas%<PropertyName>:s: Boolean;'                + CrLf +
                                   'begin'                                                                   + CrLf +
                                   '  Result := Assigned(ChildNodes.FindNode(''%<PropertySourceName>:s''));' + CrLf +
                                   'end;'                                                                    + CrLf +
                                   ''                                                                        + CrLf,

                                   { dntElementNS }
                                   'function TXML%<Name>:s.GetHas%<PropertyName>:s: Boolean;'                                     + CrLf +
                                   'begin'                                                                                        + CrLf +
                                   '  Result := Assigned(ChildNodes.FindNode(''%<PropertySourceName>:s'', ''%<Namespace>:s''));'  + CrLf +
                                   'end;'                                                                                         + CrLf +
                                   ''                                                                                             + CrLf
                                 );

  PropertyImplMethodGetOptionalAttr = 'function TXML%<Name>:s.GetHas%<PropertyName>:s: Boolean;'                + CrLf +
                                      'begin'                                                                   + CrLf +
                                      '  Result := Assigned(AttributeNodes.FindNode(''%<PropertySourceName>:s''));' + CrLf +
                                      'end;'                                                                    + CrLf +
                                      ''                                                                        + CrLf;

  PropertyImplMethodGetNil: array[TDelphiElementType] of string =
                            (
                              { dntElement }
                              'function TXML%<Name>:s.Get%<PropertyName>:sIsNil: Boolean;'                                + CrLf +
                              'begin'                                                                                     + CrLf +
                              '  Result := GetNodeIsNil(ChildNodes[''%<PropertySourceName>:s'']);'                        + CrLf +
                              'end;'                                                                                      + CrLf +
                              ''                                                                                          + CrLf,

                              { dntElementNS }
                              'function TXML%<Name>:s.Get%<PropertyName>:sIsNil: Boolean;'                                      + CrLf +
                              'begin'                                                                                           + CrLf +
                              '  Result := GetNodeIsNil(ChildNodesNS[''%<PropertySourceName>:s'', ''%<Namespace>:s'']);'        + CrLf +
                              'end;'                                                                                            + CrLf +
                              ''                                                                                                + CrLf
                            );

  PropertyImplMethodSetNil: array[TDelphiElementType] of string =
                            (
                              { dntElement }
                              'procedure TXML%<Name>:s.Set%<PropertyName>:sIsNil(const Value: Boolean);'    + CrLf +
                              'begin'                                                                       + CrLf +
                              '  SetNodeIsNil(ChildNodes[''%<PropertySourceName>:s''], Value);'             + CrLf +
                              'end;'                                                                        + CrLf +
                              ''                                                                            + CrLf,

                              { dntElementNS }
                              'procedure TXML%<Name>:s.Set%<PropertyName>:sIsNil(const Value: Boolean);'                      + CrLf +
                              'begin'                                                                                         + CrLf +
                              '  SetNodeIsNil(ChildNodesNS[''%<PropertySourceName>:s'', ''%<Namespace>:s''], Value);'         + CrLf +
                              'end;'                                                                                          + CrLf +
                              ''                                                                                              + CrLf
                            );

  PropertyImplMethodGetText: array[TDelphiElementType] of string =
                             (
                               { dntElement }
                               'function TXML%<Name>:s.Get%<PropertyName>:sText: WideString;'            + CrLf +
                               'begin'                                                                   + CrLf +
                               '  Result := ChildNodes[''%<PropertySourceName>:s''].Text;'               + CrLf +
                               'end;'                                                                    + CrLf +
                               ''                                                                        + CrLf,

                               { dntElement }
                               'function TXML%<Name>:s.Get%<PropertyName>:sText: WideString;'                           + CrLf +
                               'begin'                                                                                  + CrLf +
                               '  Result := ChildNodesNS[''%<PropertySourceName>:s'', ''%<Namespace>:s''].Text;'        + CrLf +
                               'end;'                                                                                   + CrLf +
                               ''                                                                                       + CrLf
                             );

  PropertyImplMethodGetTextAttr = 'function TXML%<Name>:s.Get%<PropertyName>:sText: WideString;'            + CrLf +
                                  'begin'                                                                   + CrLf +
                                  '  Result := AttributeNodes[''%<PropertySourceName>:s''].Text;'           + CrLf +
                                  'end;'                                                                    + CrLf +
                                  ''                                                                        + CrLf;

  PropertyImplMethodSetText: array[TDelphiElementType] of string =
                             (
                               { dntElement }
                               'procedure TXML%<Name>:s.Set%<PropertyName>:sText(const Value: WideString);'  + CrLf +
                               'begin'                                                                       + CrLf +
                               '  ChildNodes[''%<PropertySourceName>:s''].NodeValue := Value;'               + CrLf +
                               'end;'                                                                        + CrLf +
                               ''                                                                            + CrLf,

                               { dntElementNS }
                               'procedure TXML%<Name>:s.Set%<PropertyName>:sText(const Value: WideString);'                 + CrLf +
                               'begin'                                                                                      + CrLf +
                               '  ChildNodesNS[''%<PropertySourceName>:s'', ''%<Namespace>:s''].NodeValue := Value;'        + CrLf +
                               'end;'                                                                                       + CrLf +
                               ''                                                                                           + CrLf
                             );

  PropertyImplMethodSetTextAttr = 'procedure TXML%<Name>:s.Set%<PropertyName>:sText(const Value: WideString);'  + CrLf +
                                  'begin'                                                                       + CrLf +
                                  '  AttributeNodes[''%<PropertySourceName>:s''].NodeValue := Value;'           + CrLf +
                                  'end;'                                                                        + CrLf +
                                  ''                                                                            + CrLf;

  PropertyImplMethodStream: array[TDelphiElementType] of string =
                            (
                              { dntElement }
                              'procedure TXML%<Name>:s.Save%<PropertyName>:sToStream(AStream: TStream);'             + CrLf +
                              'begin'                                                                                + CrLf +
                              '  Base64DecodeToStream(Trim(ChildNodes[''%<PropertySourceName>:s''].Text), AStream);' + CrLf +
                              'end;'                                                                                 + CrLf +
                              ''                                                                                     + CrLf,

                              { dntElementNS }
                              'procedure TXML%<Name>:s.Save%<PropertyName>:sToStream(AStream: TStream);'                                          + CrLf +
                              'begin'                                                                                                             + CrLf +
                              '  Base64DecodeToStream(Trim(ChildNodesNS[''%<PropertySourceName>:s'', ''%<Namespace>:s''].Text), AStream);' + CrLf +
                              'end;'                                                                                                              + CrLf +
                              ''                                                                                                                  + CrLf
                            );

  PropertyImplMethodFile: array[TDelphiElementType] of string =
                          (
                            { dntElement }
                            'procedure TXML%<Name>:s.Save%<PropertyName>:sToFile(const AFileName: string);'        + CrLf +
                            'begin'                                                                                + CrLf +
                            '  Base64DecodeToFile(Trim(ChildNodes[''%<PropertySourceName>:s''].Text), AFileName);' + CrLf +
                            'end;'                                                                                 + CrLf +
                            ''                                                                                     + CrLf,

                            { dntElementNS }
                            'procedure TXML%<Name>:s.Save%<PropertyName>:sToFile(const AFileName: string);'                                     + CrLf +
                            'begin'                                                                                                             + CrLf +
                            '  Base64DecodeToFile(Trim(ChildNodesNS[''%<PropertySourceName>:s'', ''%<Namespace>:s''].Text), AFileName);' + CrLf +
                            'end;'                                                                                                              + CrLf +
                            ''                                                                                                                  + CrLf
                          );

  SectionComments:  array[TDelphiXMLSection] of String =
                    (
                      '  { Forward declarations for %<SchemaName>:s }',
                      '  { Interfaces for %<SchemaName>:s }',
                      '  { Classes for %<SchemaName>:s }',
                      '{ Implementation for %<SchemaName>:s }'
                    );




  PrefixInterface         = 'IXML';
  PrefixClass             = 'TXML';
  PrefixField             = 'F';


  InterfaceItemForward    = '  IXML%<Name>:s = interface;';
  InterfaceItemInterface  = '  IXML%<Name>:s = interface(%<ParentName>:s)';
  InterfaceItemClass      = '  TXML%<Name>:s = class(%<ParentName>:s, IXML%<Name>:s)';


  CollectionInterface     = 'IXMLNodeCollection';
  CollectionClass         = 'TX2XMLNodeCollection';

  ItemInterface           = 'IXMLNode';
  ItemClass               = 'TX2XMLNode';



  // #ToDo1 (MvR) 9-3-2008: document / node / etc
  // #ToDo1 (MvR) 9-3-2008: WideString etc ?
  ReservedWords:  array[0..107] of String =
                  (
                    'absolute', 'abstract', 'and', 'array', 'as', 'asm',
                    'assembler', {'automated', }'begin', 'case', 'cdecl', 'class',
                    'const', 'constructor', {'contains', }'default', 'deprecated',
                    'destructor', 'dispid', 'dispinterface', 'div', 'do',
                    'downto', 'dynamic', 'else', 'end', 'except', 'export',
                    'exports', 'external', 'far', 'file', {'final', }'finalization',
                    'finally', 'for', 'forward', 'function', 'goto', 'if',
                    'implementation', 'implements', 'in', 'index', 'inherited',
                    'initialization', 'inline', 'interface', 'is', 'label',
                    'library', 'local', 'message', 'mod', {'name', }'near',
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

  SafeChars = ['A'..'Z', 'a'..'z', '0'..'9', '_'];


type
  TTypeConversion   = (tcNone,
                       tcBoolean,
                       tcFloat,
                       tcDateTime,
                       tcDate,
                       tcTime,
                       tcString,
                       tcBase64,
                       tcNode);
                       
  TTypeConversions  = set of TTypeConversion;

  TTypeMapping  = record
    SchemaName:   String;
    DelphiName:   String;
    Conversion:   TTypeConversion;
  end;


const
  SimpleTypeMapping:  array[0..13] of TTypeMapping =
                      (
                        (SchemaName:  'int';          DelphiName:  'Integer';     Conversion:    tcNone),
                        (SchemaName:  'integer';      DelphiName:  'Integer';     Conversion:    tcNone),
                        (SchemaName:  'short';        DelphiName:  'Smallint';    Conversion:    tcNone),
                        (SchemaName:  'date';         DelphiName:  'TDateTime';   Conversion:    tcDate),
                        (SchemaName:  'time';         DelphiName:  'TDateTime';   Conversion:    tcTime),
                        (SchemaName:  'dateTime';     DelphiName:  'TDateTime';   Conversion:    tcDateTime),
                        (SchemaName:  'decimal';      DelphiName:  'Double';      Conversion:    tcFloat),
                        (SchemaName:  'float';        DelphiName:  'Double';      Conversion:    tcFloat),
                        (SchemaName:  'double';       DelphiName:  'Double';      Conversion:    tcFloat),
                        (SchemaName:  'boolean';      DelphiName:  'Boolean';     Conversion:    tcBoolean),
                        (SchemaName:  'string';       DelphiName:  'WideString';  Conversion:    tcString),
                        (SchemaName:  'anyURI';       DelphiName:  'WideString';  Conversion:    tcString),
                        (SchemaName:  'base64Binary'; DelphiName:  'WideString';  Conversion:    tcBase64),
                        (SchemaName:  'anyType';      DelphiName:  'IXMLNode';    Conversion:    tcNode)
                      );


  TypeConversionReqUtils:   array[TTypeConversion] of Boolean =
                            (
                              { tcNone }      False,
                              { tcBoolean }   True,
                              { tcFloat }     True,
                              { tcDateTime }  True,
                              { tcDate }      True,
                              { tcTime }      True,
                              { tcString }    True,
                              { tcBase64 }    True,
                              { tcNone }      False
                            );

  TypeConversionNone:       array[TDelphiAccessor, TDelphiNodeType] of String =
                            (
                              { daGet }
                              (
                                { dntElement }    '  %<Destination>:s := ChildNodes[''%<Source>:s''].NodeValue;',
                                { dntElementNS }  '  %<Destination>:s := ChildNodesNS[''%<Source>:s'', ''%<Namespace>:s''].NodeValue;',
                                { dntAttribute }  '  %<Destination>:s := AttributeNodes[''%<Source>:s''].NodeValue;',
                                { dntNodeValue }  '  %<Destination>:s := GetNodeValue;',
                                { dntCustom }     '  %<Destination>:s := %<Source>:s;'
                              ),
                              { daSet }
                              (
                                { dntElement }    '  ChildNodes[''%<Destination>:s''].NodeValue := %<Source>:s;',
                                { dntElementNS }  '  ChildNodesNS[''%<Destination>:s'', ''%<Namespace>:s''].NodeValue := %<Source>:s;',
                                { dntAttribute }  '  SetAttribute(''%<Destination>:s'', %<Source>:s);',
                                { dntNodeValue }  '  SetNodeValue(%<Source>:s);',
                                { dntCustom }     '  %<Destination>:s := %<Source>:s;'
                              )
                            );


  TypeConversion:           array[TDelphiAccessor, TDelphiNodeType, TTypeConversion] of String =
                            (
                              { daGet }
                              (
                                { dntElement }
                                (
                                  { tcNone }      '',
                                  { tcBoolean }   '',
                                  { tcFloat }     '  %<Destination>:s := XMLToFloat(ChildNodes[''%<Source>:s''].NodeValue);',
                                  { tcDateTime }  '  %<Destination>:s := XMLToDateTime(ChildNodes[''%<Source>:s''].NodeValue, xdtDateTime);',
                                  { tcDate }      '  %<Destination>:s := XMLToDateTime(ChildNodes[''%<Source>:s''].NodeValue, xdtDate);',
                                  { tcTime }      '  %<Destination>:s := XMLToDateTime(ChildNodes[''%<Source>:s''].NodeValue, xdtTime);',
                                  { tcString }    '  %<Destination>:s := ChildNodes[''%<Source>:s''].Text;',
                                  { tcBase64 }    '  %<Destination>:s := Base64Decode(Trim(ChildNodes[''%<Source>:s''].Text));',
                                  { tcNode }      '  %<Destination>:s := ChildNodes[''%<Source>:s''];'
                                ),
                                { dntElementNS }
                                (
                                  { tcNone }      '',
                                  { tcBoolean }   '',
                                  { tcFloat }     '  %<Destination>:s := XMLToFloat(ChildNodesNS[''%<Source>:s'', ''%<Namespace>:s''].NodeValue);',
                                  { tcDateTime }  '  %<Destination>:s := XMLToDateTime(ChildNodesNS[''%<Source>:s'', ''%<Namespace>:s''].NodeValue, xdtDateTime);',
                                  { tcDate }      '  %<Destination>:s := XMLToDateTime(ChildNodesNS[''%<Source>:s'', ''%<Namespace>:s''].NodeValue, xdtDate);',
                                  { tcTime }      '  %<Destination>:s := XMLToDateTime(ChildNodesNS[''%<Source>:s'', ''%<Namespace>:s''].NodeValue, xdtTime);',
                                  { tcString }    '  %<Destination>:s := ChildNodesNS[''%<Source>:s'', ''%<Namespace>:s''].Text;',
                                  { tcBase64 }    '  %<Destination>:s := Base64Decode(Trim(ChildNodesNS[''%<Source>:s'', ''%<Namespace>:s''].Text));',
                                  { tcNode }      '  %<Destination>:s := ChildNodesNS[''%<Source>:s'', ''%<Namespace>:s''];'
                                ),
                                { dntAttribute }
                                (
                                  { tcNone }      '',
                                  { tcBoolean }   '',
                                  { tcFloat }     '  %<Destination>:s := XMLToFloat(AttributeNodes[''%<Source>:s''].NodeValue);',
                                  { tcDateTime }  '  %<Destination>:s := XMLToDateTime(AttributeNodes[''%<Source>:s''].NodeValue, xdtDateTime);',
                                  { tcDate }      '  %<Destination>:s := XMLToDateTime(AttributeNodes[''%<Source>:s''].NodeValue, xdtDate);',
                                  { tcTime }      '  %<Destination>:s := XMLToDateTime(AttributeNodes[''%<Source>:s''].NodeValue, xdtTime);',
                                  { tcString }    '  %<Destination>:s := AttributeNodes[''%<Source>:s''].Text;',
                                  { tcBase64 }    '  %<Destination>:s := Base64Decode(Trim(AttributeNodes[''%<Source>:s''].Text));',
                                  { tcNode }      '  %<Destination>:s := AttributeNodes[''%<Source>:s''];'
                                ),
                                { dntNodeValue }
                                (
                                  { tcNone }      '',
                                  { tcBoolean }   '',
                                  { tcFloat }     '  %<Destination>:s := XMLToFloat(GetNodeValue);',
                                  { tcDateTime }  '  %<Destination>:s := XMLToDateTime(GetNodeValue, xdtDateTime);',
                                  { tcDate }      '  %<Destination>:s := XMLToDateTime(GetNodeValue, xdtDate);',
                                  { tcTime }      '  %<Destination>:s := XMLToDateTime(GetNodeValue, xdtTime);',
                                  { tcString }    '  %<Destination>:s := GetNodeValue;',
                                  { tcBase64 }    '  %<Destination>:s := Base64Decode(Trim(GetNodeValue));',
                                  { tcNode }      ''
                                ),
                                { dntCustom}
                                (
                                  { tcNone }      '',
                                  { tcBoolean }   '',
                                  { tcFloat }     '  %<Destination>:s := XMLToFloat(%<Source>:s);',
                                  { tcDateTime }  '  %<Destination>:s := XMLToDateTime(%<Source>:s, xdtDateTime);',
                                  { tcDate }      '  %<Destination>:s := XMLToDateTime(%<Source>:s, xdtDate);',
                                  { tcTime }      '  %<Destination>:s := XMLToDateTime(%<Source>:s, xdtTime);',
                                  { tcString }    '',
                                  { tcBase64 }    '  %<Destination>:s := Base64Decode(Trim(%<Source>:s));',
                                  { tcNode }      ''
                                )
                              ),
                              { daSet }
                              (
                                { dntElement }
                                (
                                  { tcNone }      '',
                                  { tcBoolean }   '  ChildNodes[''%<Destination>:s''].NodeValue := BoolToXML(%<Source>:s);',
                                  { tcFloat }     '  ChildNodes[''%<Destination>:s''].NodeValue := FloatToXML(%<Source>:s);',
                                  { tcDateTime }  '  ChildNodes[''%<Destination>:s''].NodeValue := DateTimeToXML(%<Source>:s, xdtDateTime);',
                                  { tcDate }      '  ChildNodes[''%<Destination>:s''].NodeValue := DateTimeToXML(%<Source>:s, xdtDate);',
                                  { tcTime }      '  ChildNodes[''%<Destination>:s''].NodeValue := DateTimeToXML(%<Source>:s, xdtTime);',
                                  { tcString }    '  ChildNodes[''%<Destination>:s''].NodeValue := GetValidXMLText(%<Source>:s);',
                                  { tcBase64 }    '  ChildNodes[''%<Destination>:s''].NodeValue := Base64Encode(%<Source>:s);',
                                  { tcNode }      '  ChildNodes[''%<Destination>:s''] := %<Source>:s;'
                                ),
                                { dntElementNS }
                                (
                                  { tcNone }      '',
                                  { tcBoolean }   '  ChildNodesNS[''%<Destination>:s'', ''%<Namespace>:s''].NodeValue := BoolToXML(%<Source>:s);',
                                  { tcFloat }     '  ChildNodesNS[''%<Destination>:s'', ''%<Namespace>:s''].NodeValue := FloatToXML(%<Source>:s);',
                                  { tcDateTime }  '  ChildNodesNS[''%<Destination>:s'', ''%<Namespace>:s''].NodeValue := DateTimeToXML(%<Source>:s, xdtDateTime);',
                                  { tcDate }      '  ChildNodesNS[''%<Destination>:s'', ''%<Namespace>:s''].NodeValue := DateTimeToXML(%<Source>:s, xdtDate);',
                                  { tcTime }      '  ChildNodesNS[''%<Destination>:s'', ''%<Namespace>:s''].NodeValue := DateTimeToXML(%<Source>:s, xdtTime);',
                                  { tcString }    '  ChildNodesNS[''%<Destination>:s'', ''%<Namespace>:s''].NodeValue := GetValidXMLText(%<Source>:s);',
                                  { tcBase64 }    '  ChildNodesNS[''%<Destination>:s'', ''%<Namespace>:s''].NodeValue := Base64Encode(%<Source>:s);',
                                  { tcNode }      '  ChildNodesNS[''%<Destination>:s'', ''%<Namespace>:s''] := %<Source>:s;'
                                ),
                                { dntAttribute }
                                (
                                  { tcNone }      '',
                                  { tcBoolean }   '  SetAttribute(''%<Destination>:s'', BoolToXML(%<Source>:s));',
                                  { tcFloat }     '  SetAttribute(''%<Destination>:s'', FloatToXML(%<Source>:s));',
                                  { tcDateTime }  '  SetAttribute(''%<Destination>:s'', DateTimeToXML(%<Source>:s, xdtDateTime));',
                                  { tcDate }      '  SetAttribute(''%<Destination>:s'', DateTimeToXML(%<Source>:s, xdtDate));',
                                  { tcTime }      '  SetAttribute(''%<Destination>:s'', DateTimeToXML(%<Source>:s, xdtTime));',
                                  { tcString }    '  SetAttribute(''%<Destination>:s'', GetValidXMLText(%<Source>:s));',
                                  { tcBase64 }    '  SetAttribute(''%<Destination>:s'', Base64Encode(%<Source>:s));',
                                  { tcNode }      ''
                                ),
                                { dntNodeValue }
                                (
                                  { tcNone }      '',
                                  { tcBoolean }   '  SetNodeValue(BoolToXML(%<Source>:s));',
                                  { tcFloat }     '  SetNodeValue(FloatToXML(%<Source>:s));',
                                  { tcDateTime }  '  SetNodeValue(DateTimeToXML(%<Source>:s, xdtDateTime));',
                                  { tcDate }      '  SetNodeValue(DateTimeToXML(%<Source>:s, xdtDate));',
                                  { tcTime }      '  SetNodeValue(DateTimeToXML(%<Source>:s, xdtTime));',
                                  { tcString }    '  SetNodeValue(GetValidXMLText(%<Source>:s));',
                                  { tcBase64 }    '  SetNodeValue(Base64Encode(%<Source>:s));',
                                  { tcNode }      ''
                                ),
                                { dntCustom}
                                (
                                  { tcNone }      '',
                                  { tcBoolean }   '  %<Destination>:s := BoolToXML(%<Source>:s);',
                                  { tcFloat }     '  %<Destination>:s := FloatToXML(%<Source>:s);',
                                  { tcDateTime }  '  %<Destination>:s := DateTimeToXML(%<Source>:s, xdtDateTime);',
                                  { tcDate }      '  %<Destination>:s := DateTimeToXML(%<Source>:s, xdtDate);',
                                  { tcTime }      '  %<Destination>:s := DateTimeToXML(%<Source>:s, xdtTime);',
                                  { tcString }    '  %<Destination>:s := GetValidXMLText(%<Source>:s);',
                                  { tcBase64 }    '  %<Destination>:s := Base64Encode(%<Source>:s);',
                                  { tcNode }      ''
                                )
                              )
                            );


implementation
end.

