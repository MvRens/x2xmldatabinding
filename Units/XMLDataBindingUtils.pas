{
  Helpers functions for the X2Software XML Data Binding
}
unit XMLDataBindingUtils;

interface
uses
  Classes,
  SysUtils,
  XMLDoc,
  xmldom,
  XMLIntf;


type
  EBase64Error        = class(Exception);
  EXSDValidationError = class(Exception);

  TXMLDateTimeFormat  = (xdtDateTime, xdtDate, xdtTime);
  TXMLTimeFragment    = (xtfMilliseconds, xtfTimezone);
  TXMLTimeFragments   = set of TXMLTimeFragment;
  TDateConvert        = (dcToUtc, dcToLocal);

  IXSDValidate  = interface
    ['{3BFDC851-7459-403B-87B3-A52E9E85BC8C}']
    procedure XSDValidate;
  end;

  IXSDValidateStrictResult = interface
    ['{F10E1CB2-ECDF-4215-AF2C-28B5C6C51A90}']
    procedure MissingElement(AParent: IXMLNode; const AName: string);
    procedure MissingAttribute(AParent: IXMLNode; const AName: string);
  end;

  IXSDValidateStrict = interface
    ['{82C3B08E-F327-4D38-9FE2-F99925E7E401}']
    procedure XSDValidateStrict(AResult: IXSDValidateStrictResult);
  end;


  TX2XMLNode = class(TXMLNode)
  private
    function GetChildNodesNS(const ANodeName, ANamespaceURI: DOMString): IXMLNode;
  protected
    property ChildNodesNS[const ANodeName, ANamespaceURI: DOMString]: IXMLNode read GetChildNodesNS;
  end;


  TX2XMLNodeCollection = class(TXMLNodeCollection)
  private
    function GetChildNodesNS(const ANodeName, ANamespaceURI: DOMString): IXMLNode;
  protected
    property ChildNodesNS[const ANodeName, ANamespaceURI: DOMString]: IXMLNode read GetChildNodesNS;
  end;


  TXMLNodeCollectionEnumerator = class(TInterfacedObject)
  private
    FNodeCollection: IXMLNodeCollection;
    FIndex: Integer;
  public
    constructor Create(ANodeCollection: IXMLNodeCollection);

    function GetCurrent: IXMLNode;
    function MoveNext: Boolean; virtual;

    property Current: IXMLNode read GetCurrent;
  end;



const
  AllTimeFragments    = [Low(TXMLTimeFragment)..High(TXMLTimeFragment)];



  function DateTimeToXML(ADate: TDateTime; AFormat: TXMLDateTimeFormat; ATimeFragments: TXMLTimeFragments = AllTimeFragments): string;
  function XMLToDateTime(const ADate: string; AFormat: TXMLDateTimeFormat): TDateTime;
  
  function BoolToXML(AValue: Boolean): WideString;
  function XMLToBool(const AValue: WideString): Boolean;

  function FloatToXML(AValue: Extended): WideString;
  function XMLToFloat(const AValue: WideString): Extended;

  function GetNodeIsNil(ANode: IXMLNode): Boolean;
  procedure SetNodeIsNil(ANode: IXMLNode; ASetNil: Boolean);

  procedure XSDValidate(AParent: IXMLNode; ARecurse: Boolean = True; AValidateParent: Boolean = True);
  procedure XSDValidateStrict(AParent: IXMLNode; ARecurse: Boolean = True; AValidateParent: Boolean = True); overload;
  procedure XSDValidateStrict(AResult: IXSDValidateStrictResult; AParent: IXMLNode; ARecurse: Boolean = True; AValidateParent: Boolean = True); overload;
  procedure ValidateRequiredElements(AResult: IXSDValidateStrictResult; AParent: IXMLNode; ANodes: array of string);
  procedure ValidateRequiredAttributes(AResult: IXSDValidateStrictResult; AParent: IXMLNode; ANodes: array of string);
  procedure CreateRequiredElements(AParent: IXMLNode; ANodes: array of string); overload;
  procedure CreateRequiredElements(AParent: IXMLNode; ANodes: array of string; Namespaces: array of string); overload;
  procedure CreateRequiredAttributes(AParent: IXMLNode; ANodes: array of string);
  procedure SortChildNodes(AParent: IXMLNode; ASortOrder: array of string);

  function IsValidXMLChar(AChar: WideChar): Boolean;
  function GetValidXMLText(AText: WideString): WideString;

  { Now wraps the JclMime implementation:
      Lightening fast Mime (Base64) Encoding and Decoding routines.
      Coded by Ralf Junker (ralfjunker@gmx.de).}
  function Base64Encode(AValue: String): string;
  function Base64Decode(AValue: String): string;
  function Base64EncodeFromStream(AStream: TStream): string;
  function Base64EncodeFromFile(const AFileName: string): string;
  procedure Base64DecodeToStream(AValue: string; AStream: TStream);
  procedure Base64DecodeToFile(AValue: string; const AFileName: string);

const
  XMLSchemaInstanceURI = 'http://www.w3.org/2001/XMLSchema-instance';

  XMLDateFormat     = 'yyyy"-"mm"-"dd';
  XMLTimeFormat     = 'hh":"nn":"ss';
  XMLMsecsFormat    = '"."zzz';
  XMLTimezoneZulu   = 'Z';
  XMLTimezoneFormat = '%s%.2d:%.2d';

  XMLDateTimeFormats: array[TXMLDateTimeFormat] of String =
                      (
                        XMLDateFormat + '"T"' + XMLTimeFormat,
                        XMLDateFormat,
                        XMLTimeFormat
                      );

  XMLTimezoneSigns:   array[Boolean] of Char = ('-', '+');
  XMLBoolValues:      array[Boolean] of String =
                      (
                        'false',
                        'true'
                      );

  XMLIsNilAttribute = 'nil';
  XMLIsNilAttributeNS = 'xsi:nil';

  Base64ValidChars  = ['A'..'Z', 'a'..'z', '0'..'9', '+', '/'];
  Base64LookupTable = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
                      'abcdefghijklmnopqrstuvwxyz' +
                      '0123456789+/';
  Base64Padding     = '=';


implementation
uses
  DateUtils,
  Math,
  Types,
  Windows;


type
  PSortNodeInfo = ^TSortNodeInfo;
  TSortNodeInfo = record
    Node: IXMLNode;
    SortIndex: Integer;
    OriginalIndex: Integer;
  end;


  TXSDValidateStrictResult = class(TInterfacedPersistent, IXSDValidateStrictResult)
  private
    FMissingElements: TStrings;
    FMissingAttributes: TStrings;

    function GetMissingAttributes: TStrings;
    function GetMissingElements: TStrings;
  protected
    function GetNodeTree(AParent: IXMLNode; const AName: string): string;

    property MissingElements: TStrings read GetMissingElements;
    property MissingAttributes: TStrings read GetMissingAttributes;
  public
    destructor Destroy; override;

    procedure RaiseResult;

    { IXSDValidateStrictResult }
    procedure MissingElement(AParent: IXMLNode; const AName: string);
    procedure MissingAttribute(AParent: IXMLNode; const AName: string);
  end;


  function MimeEncodeString(const S: AnsiString): AnsiString; forward;
  function MimeDecodeString(const S: AnsiString): AnsiString; forward;
  procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream); forward;
  procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream); forward;



{ TX2XMLNode }
function TX2XMLNode.GetChildNodesNS(const ANodeName, ANamespaceURI: DOMString): IXMLNode;
begin
  Result := ChildNodes.FindNode(ANodeName, ANamespaceURI);
  if (not Assigned(Result)) and (doNodeAutoCreate in OwnerDocument.Options) then
    Result := AddChild(ANodeName, ANamespaceURI);  
end;



{ TX2XMLNodeCollection }
function TX2XMLNodeCollection.GetChildNodesNS(const ANodeName, ANamespaceURI: DOMString): IXMLNode;
begin
  Result := ChildNodes.FindNode(ANodeName, ANamespaceURI);
  if (not Assigned(Result)) and (doNodeAutoCreate in OwnerDocument.Options) then
    Result := AddChild(ANodeName, ANamespaceURI);  
end;



{ TXMLNodeCollectionEnumerator }
constructor TXMLNodeCollectionEnumerator.Create(ANodeCollection: IXMLNodeCollection);
begin
  inherited Create;

  FNodeCollection := ANodeCollection;
  FIndex := -1;
end;


function TXMLNodeCollectionEnumerator.GetCurrent: IXMLNode;
begin
  if (FIndex >= 0) and (FIndex < FNodeCollection.Count) then
    Result := FNodeCollection.Nodes[FIndex]
  else
    Result := nil;
end;


function TXMLNodeCollectionEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := (FIndex < FNodeCollection.Count);
end;


function InDSTSpan(ADate: TDateTime; ATimeZoneInfo: TTimeZoneInformation): boolean;
var
  lowerDayLight: TDateTime;
  upperDayLight: TDateTime;
  day: TDateTime;
  days: Integer;

  function GetDay(AYear, AMonth, ADay, ADayOfWeek: Integer): TDateTime;
  var
    I, Counter : Integer;
  begin
    Result := 0;
    Counter := 0;

    days := DaysInAMonth(AYear, AMonth);
    for I := 1 to days do
    begin
      Result := EncodeDate(AYear, AMonth, I);
      // Delphi DayOfWeek 1 = Sunday
      // TimeZoneInfo.wDayOfWeek 0 = Sunday
      if DayOfWeek(Result) -1 = ADayOfWeek then
      begin
        inc(Counter);
        if (counter = ADay) or ((Counter < Aday) and (I >= days - 6)) then
          break;
      end;
    end;
  end;
  
begin
  with ATimeZoneInfo.DaylightDate do
  begin
    day := GetDay(wYear + YearOf(ADate), wMonth, wDay, wDayOfWeek);
    lowerDayLight := day + EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
  end;

  with ATimeZoneInfo.StandardDate do
  begin
    day := GetDay(wYear + YearOf(ADate), wMonth, wDay, wDayOfWeek);
    upperDayLight := day + EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
  end;

  Result := (ADate >= lowerDayLight) and (ADate <= upperDayLight);
end;


function ConvertDate(ADate: TDateTime; ADateconvert: TDateConvert): TDateTime;
var
  timeZone: TTimeZoneInformation;
  timeZoneID: Cardinal;
  localOffset: Integer;

begin
  FillChar(timeZone, SizeOf(TTimeZoneInformation), #0);
  timeZoneID := GetTimeZoneInformation(timeZone);

  if timeZoneID in [TIME_ZONE_ID_STANDARD, TIME_ZONE_ID_DAYLIGHT] then
    localOffset := -timeZone.Bias - IfThen(InDSTSpan(ADate, timeZone), timeZone.DaylightBias, timeZone.StandardBias)
  else
    localOffset := 0;

  if ADateconvert = dcToUtc then
    localOffset := localOffset * -1;

  Result      := IncMinute(ADate, localOffset);
end;


function DateTimeToXML(ADate: TDateTime; AFormat: TXMLDateTimeFormat; ATimeFragments: TXMLTimeFragments): string;
var
  formatSettings: TFormatSettings;
  utcDate: TDateTime;
  offsetMinutes: Integer;

begin
  formatSettings := TFormatSettings.Create;;
  Result  := FormatDateTime(XMLDateTimeFormats[AFormat], ADate, formatSettings);

  if AFormat in [xdtDateTime, xdtTime] then
  begin
    if xtfMilliseconds in ATimeFragments then
      Result  := Result + FormatDateTime(XMLMsecsFormat, ADate);

    if (xtfTimezone in ATimeFragments) then
    begin
      utcDate := ConvertDate(ADate, dcToUtc);
      offsetMinutes := MinutesBetween(ADate, utcDate);

      if offsetMinutes = 0 then
        Result  := Result + XMLTimezoneZulu
      else
        Result := Result + Format(XMLTimezoneFormat,
          [XMLTimezoneSigns[offsetMinutes > 0], offsetMinutes div 60, offsetMinutes mod 60]);
    end;
  end;
end;


function XMLToDateTime(const ADate: string; AFormat: TXMLDateTimeFormat): TDateTime;
const
  { yyyy-mm-ddThh:nn:ss.zzz+xx:xx }
  XMLTimeSeparatorPos = 11;
  XMLTimeSeparator    = 'T';
  XMLMinTimeLength    = 8;

var
  date: string;
  time: string;
  year: Integer;
  month: Integer;
  day: Integer;
  hour: Integer;
  minute: Integer;
  second: Integer;
  msec: Integer;
  hasTimezone: Boolean;
  xmlOffset: Integer;
  endPos: Integer;

begin
  Result  := 0;
  date    := '';
  time    := '';

  case AFormat of
    xdtDateTime:
      begin
        if (Length(ADate) < XMLTimeSeparatorPos) or
           (ADate[XMLTimeSeparatorPos] <> XMLTimeSeparator) then
          Exit;

        date  := ADate;
        time  := ADate;
        SetLength(date, Pred(XMLTimeSeparatorPos));
        Delete(time, 1, XMLTimeSeparatorPos);
      end;

    xdtDate:
      begin
        if Length(ADate) < Pred(XMLTimeSeparatorPos) then
          Exit;
          
        date  := ADate;
      end;

    xdtTime:
      begin
        if Length(ADate) < XMLMinTimeLength then
          Exit;

        time  := ADate;
      end;
  end;

  if AFormat in [xdtDateTime, xdtDate] then
  begin
    { Parse date (yyyy-mm-hh) }
    if TryStrToInt(Copy(date, 1, 4), year) and
       TryStrToInt(Copy(date, 6, 2), month) and
       TryStrToInt(Copy(date, 9, 2), day) then
      Result  := EncodeDate(year, month, day);
  end;

  if AFormat in [xdtDateTime, xdtTime] then
  begin
    { Parse time (hh:nn:ss) }
    if TryStrToInt(Copy(time, 1, 2), hour) and
       TryStrToInt(Copy(time, 4, 2), minute) and
       TryStrToInt(Copy(time, 7, 2), second) then
    begin
      msec  := 0;
      Delete(time, 1, 8);

      if Length(time) > 0 then
      begin
        if time[1] = '.' then
        begin
          { Parse milliseconds (.zzz+) }
          Delete(time, 1, 1);
          endPos := 1;

          while (endPos <= Length(time)) and (CharInSet(time[endPos], ['0'..'9'])) do
            Inc(endPos);

          Dec(endPos);

          if (endPos = 0) or (not TryStrToInt(Copy(time, 1, Min(endPos, 3)), msec)) then
            msec  := 0;

          if endPos > 0 then
            Delete(time, 1, endPos);
        end;
      end;

      Result  := Result + EncodeTime(hour, minute, second, msec);

      if Length(time) > 0 then
      begin
        hasTimezone := False;

        if time[1] = XMLTimezoneZulu then
        begin
          { Zulu time }
          hasTimezone := True;
        end else if CharInSet(time[1], [XMLTimezoneSigns[False], XMLTimezoneSigns[True]]) then
        begin
          { Parse timezone ([+|-]xx:xx) }
          if TryStrToInt(Copy(time, 2, 2), hour) and
             TryStrToInt(Copy(time, 5, 2), minute) then
          begin
            xmlOffset   := (hour * MinsPerHour) + minute;
            hasTimezone := True;

            if time[1] = XMLTimezoneSigns[False] then
              xmlOffset := -xmlOffset;

            Result := IncMinute(Result, - xmlOffset);
          end;
        end;

        if hasTimezone then
          Result := ConvertDate(Result, dcToLocal);
      end;
    end;
  end;
end;


function BoolToXML(AValue: Boolean): WideString;
begin
  Result := XMLBoolValues[AValue];
end;


function XMLToBool(const AValue: WideString): Boolean;
begin
  Result := StrToBoolDef(AValue, False);
end;


function GetXMLFloatFormatSettings(): TFormatSettings;
begin
  Result.DecimalSeparator := '.';
end;


function FloatToXML(AValue: Extended): WideString;
begin
  Result := FloatToStr(AValue, GetXMLFloatFormatSettings());
end;


function XMLToFloat(const AValue: WideString): Extended;
begin
  Result := StrToFloat(AValue, GetXMLFloatFormatSettings());
end;


function Base64Encode(AValue: String): String;
begin
  Result := string(MimeEncodeString(AnsiString(AValue)));
end;


function Base64Decode(AValue: String): String;
begin
  Result := string(MimeDecodeString(AnsiString(AValue)));
end;


function Base64EncodeFromStream(AStream: TStream): string;
var
  output: TStringStream;

begin
  output := TStringStream.Create('');
  try
    MimeEncodeStream(AStream, output);
    Result := output.DataString;
  finally
    FreeAndNil(output);
  end;
end;


function Base64EncodeFromFile(const AFileName: string): string;
var
  input: TFileStream;

begin
  input := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := Base64EncodeFromStream(input);
  finally
    FreeAndNil(input);
  end;
end;


procedure Base64DecodeToStream(AValue: String; AStream: TStream);
var
  input: TStringStream;

begin
  input := TStringStream.Create(AValue);
  try
    MimeDecodeStream(input, AStream);
  finally
    FreeAndNil(input);
  end;
end;


procedure Base64DecodeToFile(AValue: String; const AFileName: String);
var
  output: TFileStream;

begin
  output := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
  try
    Base64DecodeToStream(AValue, output);
  finally
    FreeAndNil(output);
  end;
end;


function GetNodeIsNil(ANode: IXMLNode): Boolean;
begin
  Result := ANode.HasAttribute(XMLIsNilAttribute, XMLSchemaInstanceURI) and
            XMLToBool(ANode.GetAttributeNS(XMLIsNilAttribute, XMLSchemaInstanceURI));
end;


procedure SetNodeIsNil(ANode: IXMLNode; ASetNil: Boolean);
var
  documentElement: IXMLNode;

begin
  if ASetNil then
  begin
    ANode.ChildNodes.Clear;

    documentElement := ANode.OwnerDocument.DocumentElement;
    if not documentElement.HasAttribute('xmlns:xsi') then
      documentElement.SetAttributeNS('xmlns:xsi', '', XMLSchemaInstanceURI);

    ANode.SetAttributeNS(XMLIsNilAttributeNS, XMLSchemaInstanceURI, BoolToXML(True));
  end else
    ANode.AttributeNodes.Delete(XMLIsNilAttribute, XMLSchemaInstanceURI);
end;


function DoSortNodes(Item1, Item2: Pointer): Integer;
var
  nodeInfo1: PSortNodeInfo;
  nodeInfo2: PSortNodeInfo;

begin
  nodeInfo1 := Item1;
  nodeInfo2 := Item2;

  if (nodeInfo1^.SortIndex > -1) and (nodeInfo2^.SortIndex = -1) then
    Result := GreaterThanValue

  else if (nodeInfo1^.SortIndex = -1) and (nodeInfo2^.SortIndex > -1) then
    Result := LessThanValue

  else if (nodeInfo1^.SortIndex = nodeInfo2^.SortIndex) then
    Result := CompareValue(nodeInfo1^.OriginalIndex, nodeInfo2^.OriginalIndex)

  else
    Result := CompareValue(nodeInfo1^.SortIndex, nodeInfo2^.SortIndex);
end;


procedure XSDValidate(AParent: IXMLNode; ARecurse, AValidateParent: Boolean);
var
  validate: IXSDValidate;
  childIndex: Integer;

begin
  if AValidateParent and Supports(AParent, IXSDValidate, validate) then
    validate.XSDValidate;

  if ARecurse then
  begin
    for childIndex := 0 to Pred(AParent.ChildNodes.Count) do
      XSDValidate(AParent.ChildNodes[childIndex], ARecurse, True);
  end;
end;


procedure XSDValidateStrict(AParent: IXMLNode; ARecurse: Boolean; AValidateParent: Boolean);
var
  result: TXSDValidateStrictResult;

begin
  result := TXSDValidateStrictResult.Create;
  try
    XSDValidateStrict(result, AParent, ARecurse, AValidateParent);

    result.RaiseResult;
  finally
    FreeAndNil(result);
  end;
end;


procedure XSDValidateStrict(AResult: IXSDValidateStrictResult; AParent: IXMLNode; ARecurse: Boolean; AValidateParent: Boolean);
var
  validate: IXSDValidateStrict;
  childIndex: Integer;

begin
  if AValidateParent and Supports(AParent, IXSDValidateStrict, validate) then
    validate.XSDValidateStrict(AResult);

  if ARecurse then
  begin
    for childIndex := 0 to Pred(AParent.ChildNodes.Count) do
      XSDValidateStrict(AResult, AParent.ChildNodes[childIndex], ARecurse, True);
  end;
end;


procedure ValidateRequiredElements(AResult: IXSDValidateStrictResult; AParent: IXMLNode; ANodes: array of string);
var
  nodeIndex: Integer;

begin
  for nodeIndex := Low(ANodes) to High(ANodes) do
  begin
    if not Assigned(AParent.ChildNodes.FindNode(ANodes[nodeIndex])) then
      AResult.MissingElement(AParent, ANodes[nodeIndex]);
  end;
end;


procedure ValidateRequiredAttributes(AResult: IXSDValidateStrictResult; AParent: IXMLNode; ANodes: array of string);
var
  nodeIndex: Integer;

begin
  for nodeIndex := Low(ANodes) to High(ANodes) do
  begin
    if not Assigned(AParent.AttributeNodes.FindNode(ANodes[nodeIndex])) then
      AResult.MissingAttribute(AParent, ANodes[nodeIndex]);
  end;
end;


procedure CreateRequiredElements(AParent: IXMLNode; ANodes: array of string); overload;
var
  nodeIndex: Integer;
  node: IXMLNode;

begin
  for nodeIndex := Low(ANodes) to High(ANodes) do
  begin
    if not Assigned(AParent.ChildNodes.FindNode(ANodes[nodeIndex])) then
    begin
      node := AParent.OwnerDocument.CreateElement(ANodes[nodeIndex], AParent.NamespaceURI);
      AParent.ChildNodes.Add(node);
    end;
  end;
end;


procedure CreateRequiredElements(AParent: IXMLNode; ANodes: array of string; Namespaces: array of string);
var
  nodeIndex: Integer;
  node: IXMLNode;

begin
  for nodeIndex := Low(ANodes) to High(ANodes) do
  begin
    if not Assigned(AParent.ChildNodes.FindNode(ANodes[nodeIndex], Namespaces[nodeIndex])) then
    begin
      node := AParent.OwnerDocument.CreateElement(ANodes[nodeIndex], Namespaces[nodeIndex]);
      AParent.ChildNodes.Add(node);
    end;
  end;
end;


procedure CreateRequiredAttributes(AParent: IXMLNode; ANodes: array of string);
var
  nodeIndex: Integer;

begin
  for nodeIndex := Low(ANodes) to High(ANodes) do
  begin
    if not Assigned(AParent.AttributeNodes.FindNode(ANodes[nodeIndex])) then
      AParent.Attributes[ANodes[nodeIndex]] := '';
  end;
end;


procedure SortChildNodes(AParent: IXMLNode; ASortOrder: array of string);
var
  sortList: TList;
  nodeInfo: PSortNodeInfo;
  childIndex: Integer;
  sortIndex: Integer;
  node: IXMLNode;

begin
  sortList := TList.Create;
  try
    { Build a list of the child nodes, with their original index and the
      index in the ASortOrder array. }
    for childIndex := 0 to Pred(AParent.ChildNodes.Count) do
    begin
      New(nodeInfo);
      nodeInfo^.Node := AParent.ChildNodes[childIndex];
      nodeInfo^.OriginalIndex := childIndex;

      for sortIndex := Low(ASortOrder) to High(ASortOrder) do
      begin
        if ASortOrder[sortIndex] = nodeInfo^.Node.NodeName then
        begin
          nodeInfo^.SortIndex := sortIndex;
          Break;
        end;
      end;

      sortList.Add(nodeInfo);
    end;

    sortList.Sort(DoSortNodes);

    { Rebuild the ChildNodes list }
    for childIndex := 0 to Pred(sortList.Count) do
    begin
      node := PSortNodeInfo(sortList[childIndex])^.Node;

      AParent.ChildNodes.Remove(node);
      AParent.ChildNodes.Insert(childIndex, node);
    end;
  finally
    for sortIndex := 0 to Pred(sortList.Count) do
      Dispose(PSortNodeInfo(sortList[sortIndex]));

    FreeAndNil(sortList);
  end;
end;


function IsValidXMLChar(AChar: WideChar): Boolean;
begin
  Result := (Ord(AChar) in [9, 10, 13]) or
            (Ord(AChar) >= 32); 
end;


function GetValidXMLText(AText: WideString): WideString;
var
  validText: WideString;
  sourcePos: Integer;
  destPos: Integer;

begin
  SetLength(validText, Length(AText));
  destPos := 0;

  for sourcePos := 1 to Length(AText) do
  begin
    if IsValidXMLChar(AText[sourcePos]) then
    begin
      Inc(destPos);
      validText[destPos] := AText[sourcePos];
    end;
  end;

  SetLength(validText, destPos);
  Result := validText;
end;


{ --- JclMime implementation from here. }
type
  {$IFDEF WIN64}
  SizeInt = NativeInt;
  TJclAddr = UInt64;
  {$ELSE}
  SizeInt = Integer;
  TJclAddr = Cardinal;
  {$ENDIF}

  PByte4 = ^TByte4;
  TByte4 = packed record
    B1: Byte;
    B2: Byte;
    B3: Byte;
    B4: Byte;
  end;

  PByte3 = ^TByte3;
  TByte3 = packed record
    B1: Byte;
    B2: Byte;
    B3: Byte;
  end;


const
  MIME_ENCODED_LINE_BREAK = 76;
  MIME_DECODED_LINE_BREAK = MIME_ENCODED_LINE_BREAK div 4 * 3;
  MIME_BUFFER_SIZE = MIME_DECODED_LINE_BREAK * 3 * 4 * 4;

  MIME_ENCODE_TABLE: array [0..63] of Byte = (
    065, 066, 067, 068, 069, 070, 071, 072, //  00 - 07
    073, 074, 075, 076, 077, 078, 079, 080, //  08 - 15
    081, 082, 083, 084, 085, 086, 087, 088, //  16 - 23
    089, 090, 097, 098, 099, 100, 101, 102, //  24 - 31
    103, 104, 105, 106, 107, 108, 109, 110, //  32 - 39
    111, 112, 113, 114, 115, 116, 117, 118, //  40 - 47
    119, 120, 121, 122, 048, 049, 050, 051, //  48 - 55
    052, 053, 054, 055, 056, 057, 043, 047); // 56 - 63

  MIME_PAD_CHAR = Byte('=');

  MIME_DECODE_TABLE: array [Byte] of Byte = (
    255, 255, 255, 255, 255, 255, 255, 255, //   0 -   7
    255, 255, 255, 255, 255, 255, 255, 255, //   8 -  15
    255, 255, 255, 255, 255, 255, 255, 255, //  16 -  23
    255, 255, 255, 255, 255, 255, 255, 255, //  24 -  31
    255, 255, 255, 255, 255, 255, 255, 255, //  32 -  39
    255, 255, 255, 062, 255, 255, 255, 063, //  40 -  47
    052, 053, 054, 055, 056, 057, 058, 059, //  48 -  55
    060, 061, 255, 255, 255, 255, 255, 255, //  56 -  63
    255, 000, 001, 002, 003, 004, 005, 006, //  64 -  71
    007, 008, 009, 010, 011, 012, 013, 014, //  72 -  79
    015, 016, 017, 018, 019, 020, 021, 022, //  80 -  87
    023, 024, 025, 255, 255, 255, 255, 255, //  88 -  95
    255, 026, 027, 028, 029, 030, 031, 032, //  96 - 103
    033, 034, 035, 036, 037, 038, 039, 040, // 104 - 111
    041, 042, 043, 044, 045, 046, 047, 048, // 112 - 119
    049, 050, 051, 255, 255, 255, 255, 255, // 120 - 127
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255);


function MimeEncodedSize(const InputSize: SizeInt): SizeInt;
begin
  if InputSize > 0 then
    Result := (InputSize + 2) div 3 * 4 + (InputSize - 1) div MIME_DECODED_LINE_BREAK * 2
  else
    Result := InputSize;
end;


procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer);
var
  B: Cardinal;
  InnerLimit, OuterLimit: TJclAddr;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  { Do we have enough input to encode a full line? }
  if InputByteCount < MIME_DECODED_LINE_BREAK then
    Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  InnerLimit := TJclAddr(InPtr);
  Inc(InnerLimit, MIME_DECODED_LINE_BREAK);

  OuterLimit := TJclAddr(InPtr);
  Inc(OuterLimit, InputByteCount);

  { Multiple line loop. }
  repeat
    { Single line loop. }
    repeat
      { Read 3 bytes from InputBuffer. }
      B := InPtr^.B1;
      B := B shl 8;
      B := B or InPtr^.B2;
      B := B shl 8;
      B := B or InPtr^.B3;
      Inc(InPtr);
      { Write 4 bytes to OutputBuffer (in reverse order). }
      OutPtr^.B4 := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B3 := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B2 := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B1 := MIME_ENCODE_TABLE[B];
      Inc(OutPtr);
    until TJclAddr(InPtr) >= InnerLimit;

    { Write line break (CRLF). }
    OutPtr^.B1 := 13;
    OutPtr^.B2 := 10;
    Inc(TJclAddr(OutPtr), 2);

    Inc(InnerLimit, MIME_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;


procedure MimeEncodeNoCRLF(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer);
var
  B: Cardinal;
  InnerLimit, OuterLimit: SizeInt;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  if InputByteCount = 0 then
    Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := TJclAddr(InPtr);
  Inc(InnerLimit, OuterLimit);

  { Last line loop. }
  while TJclAddr(InPtr) < TJclAddr(InnerLimit) do
  begin
    { Read 3 bytes from InputBuffer. }
    B := InPtr^.B1;
    B := B shl 8;
    B := B or InPtr^.B2;
    B := B shl 8;
    B := B or InPtr^.B3;
    Inc(InPtr);
    { Write 4 bytes to OutputBuffer (in reverse order). }
    OutPtr^.B4 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B3 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B2 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B1 := MIME_ENCODE_TABLE[B];
    Inc(OutPtr);
  end;

  { End of data & padding. }
  case InputByteCount - OuterLimit of
    1:
      begin
        B := InPtr^.B1;
        B := B shl 4;
        OutPtr.B2 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := MIME_ENCODE_TABLE[B];
        OutPtr.B3 := MIME_PAD_CHAR; { Pad remaining 2 bytes. }
        OutPtr.B4 := MIME_PAD_CHAR;
      end;
    2:
      begin
        B := InPtr^.B1;
        B := B shl 8;
        B := B or InPtr^.B2;
        B := B shl 2;
        OutPtr.B3 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B2 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := MIME_ENCODE_TABLE[B];
        OutPtr.B4 := MIME_PAD_CHAR; { Pad remaining byte. }
      end;
  end;
end;


procedure MimeEncode(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer);
var
  IDelta, ODelta: SizeInt;
  I, O: PByte;
begin
  MimeEncodeFullLines(InputBuffer, InputByteCount, OutputBuffer);
  IDelta := InputByteCount div MIME_DECODED_LINE_BREAK; // Number of lines processed so far.
  ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * MIME_DECODED_LINE_BREAK;
  I := @InputBuffer;
  Inc(I, IDelta);
  O := @OutputBuffer;
  Inc(O, ODelta);
  MimeEncodeNoCRLF(I^, InputByteCount - IDelta, O^);
end;


function MimeDecodePartial(const InputBuffer; const InputByteCount: SizeInt; out OutputBuffer;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): SizeInt;
var
  LByteBuffer, LByteBufferSpace, C: Cardinal;
  InPtr, OuterLimit: PByte;
  OutPtr: PByte3;
begin
  if InputByteCount > 0 then
  begin
    InPtr := @InputBuffer;
    OuterLimit := Pointer(TJclAddr(InPtr) + TJclAddr(InputByteCount));
    OutPtr := @OutputBuffer;
    LByteBuffer := ByteBuffer;
    LByteBufferSpace := ByteBufferSpace;
    while InPtr <> OuterLimit do
    begin
      { Read from InputBuffer. }
      C := MIME_DECODE_TABLE[InPtr^];
      Inc(InPtr);
      if C = $FF then
        Continue;
      LByteBuffer := LByteBuffer shl 6;
      LByteBuffer := LByteBuffer or C;
      Dec(LByteBufferSpace);
      { Have we read 4 bytes from InputBuffer? }
      if LByteBufferSpace <> 0 then
        Continue;

      { Write 3 bytes to OutputBuffer (in reverse order). }
      OutPtr^.B3 := Byte(LByteBuffer);
      LByteBuffer := LByteBuffer shr 8;
      OutPtr^.B2 := Byte(LByteBuffer);
      LByteBuffer := LByteBuffer shr 8;
      OutPtr^.B1 := Byte(LByteBuffer);
      LByteBuffer := 0;
      Inc(OutPtr);
      LByteBufferSpace := 4;
    end;
    ByteBuffer := LByteBuffer;
    ByteBufferSpace := LByteBufferSpace;
    Result := SizeInt(TJclAddr(OutPtr) - TJclAddr(@OutputBuffer));
  end
  else
    Result := 0;
end;


function MimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): SizeInt;
var
  LByteBuffer: Cardinal;
begin
  case ByteBufferSpace of
    1:
      begin
        LByteBuffer := ByteBuffer shr 2;
        PByte3(@OutputBuffer)^.B2 := Byte(LByteBuffer);
        LByteBuffer := LByteBuffer shr 8;
        PByte3(@OutputBuffer)^.B1 := Byte(LByteBuffer);
        Result := 2;
      end;
    2:
      begin
        LByteBuffer := ByteBuffer shr 4;
        PByte3(@OutputBuffer)^.B1 := Byte(LByteBuffer);
        Result := 1;
      end;
  else
    Result := 0;
  end;
end;


function MimeEncodeString(const S: AnsiString): AnsiString;
var
  L: SizeInt;
begin
  if S <> '' then
  begin
    L := Length(S);
    SetLength(Result, MimeEncodedSize(L));
    MimeEncode(PAnsiChar(S)^, L, PAnsiChar(Result)^);
  end
  else
    Result := '';
end;


function MimeDecodedSize(const InputSize: SizeInt): SizeInt;
begin
  Result := (InputSize + 3) div 4 * 3;
end;


function MimeDecodeString(const S: AnsiString): AnsiString;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  L: SizeInt;
  P, R: PAnsiChar;
begin
  if S <> '' then
  begin
    L := Length(S);
    SetLength(Result, MimeDecodedSize(L));
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    P := PAnsiChar(S);
    R := PAnsiChar(Result);
    L := MimeDecodePartial(P^, L, R^, ByteBuffer, ByteBufferSpace);
    Inc(R, L);
    Inc(L, MimeDecodePartialEnd(R^, ByteBuffer, ByteBufferSpace));
    SetLength(Result, L);
  end
  else
    Result := '';
end;


procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array [0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..(MIME_BUFFER_SIZE + 2) div 3 * 4 + MIME_BUFFER_SIZE div MIME_DECODED_LINE_BREAK * 2 - 1] of Byte;
  BytesRead: SizeInt;
  IDelta, ODelta: SizeInt;
  I, O: PByte;
begin
  InputBuffer[0] := 0;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead = Length(InputBuffer) do
  begin
    MimeEncodeFullLines(InputBuffer, Length(InputBuffer), OutputBuffer);
    OutputStream.Write(OutputBuffer, Length(OutputBuffer));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;

  MimeEncodeFullLines(InputBuffer, BytesRead, OutputBuffer);

  IDelta := BytesRead div MIME_DECODED_LINE_BREAK; // Number of lines processed.
  ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * MIME_DECODED_LINE_BREAK;

  I := @InputBuffer;
  Inc(I, IDelta);
  O := @OutputBuffer;
  Inc(O, ODelta);

  MimeEncodeNoCRLF(I^, BytesRead - IDelta, O^);

  OutputStream.Write(OutputBuffer, MimeEncodedSize(BytesRead));
end;


procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  InputBuffer: array [0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..(MIME_BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
  BytesRead: SizeInt;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  InputBuffer[0] := 0;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead > 0 do
  begin
    OutputStream.Write(OutputBuffer, MimeDecodePartial(InputBuffer, BytesRead, OutputBuffer, ByteBuffer, ByteBufferSpace));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;
  OutputStream.Write(OutputBuffer, MimeDecodePartialEnd(OutputBuffer, ByteBuffer, ByteBufferSpace));
end;


{ TXSDValidateStrictResult }
destructor TXSDValidateStrictResult.Destroy;
begin
  FreeAndNil(FMissingAttributes);
  FreeAndNil(FMissingElements);

  inherited Destroy;
end;


procedure TXSDValidateStrictResult.MissingElement(AParent: IXMLNode; const AName: string);
begin
  MissingElements.Add(GetNodeTree(AParent, AName));
end;


procedure TXSDValidateStrictResult.MissingAttribute(AParent: IXMLNode; const AName: string);
begin
  MissingAttributes.Add(GetNodeTree(AParent, AName));
end;


procedure TXSDValidateStrictResult.RaiseResult;
var
  msg: string;

  procedure AddList(AList: TStrings; const ATitle: string);
  var
    itemIndex: Integer;

  begin
    if not Assigned(AList) then
      exit;

    msg := msg + ATitle + #13#10;
    for itemIndex := 0 to Pred(AList.Count) do
      msg := msg + '- ' + AList[itemIndex] + #13#10;

    msg := msg + #13#10;
  end;

begin
  msg := '';
  AddList(FMissingElements, 'Missing elements:');
  AddList(FMissingAttributes, 'Missing attributes:');

  if Length(msg) > 0 then
    raise EXSDValidationError.Create('XSD validation failed.'#13#10 + Trim(msg));
end;


function TXSDValidateStrictResult.GetMissingElements: TStrings;
begin
  if not Assigned(FMissingElements) then
    FMissingElements := TStringList.Create;

  Result := FMissingElements;
end;


function TXSDValidateStrictResult.GetNodeTree(AParent: IXMLNode; const AName: string): string;


  function GetNodeIndex(ANodeCollection: IXMLNodeCollection; ANode: IXMLNode): string;
  var
    nodeIndex: Integer;
  begin
    Result := '?';

    for nodeIndex := 0 to Pred(ANodeCollection.Count) do
      if ANodeCollection[nodeIndex] = ANode then
      begin
        Result := IntToStr(nodeIndex);
        break;
      end;
  end;


var
  node: IXMLNode;
  nodeCollection: IXMLNodeCollection;

begin
  Result := '';

  node := AParent;
  while Assigned(node) and Assigned(node.ParentNode) do
  begin
    if Length(Result) > 0 then
      Result := '.' + Result;

    if Supports(node.ParentNode, IXMLNodeCollection, nodeCollection) then
      Result := Result + '[' + GetNodeIndex(nodeCollection, node) + ']';


    Result := node.NodeName + Result;
    node := node.ParentNode;
  end;

  if Length(Result) > 0 then
    Result := Result + '.';

  Result := Result + AName;
end;


function TXSDValidateStrictResult.GetMissingAttributes: TStrings;
begin
  if not Assigned(FMissingAttributes) then
    FMissingAttributes := TStringList.Create;

  Result := FMissingAttributes;
end;

end.

