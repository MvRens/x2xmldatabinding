{
  Helpers functions for the X2Software XML Data Binding

  Last changed:   $Date$
  Revision:       $Rev$
  URL:            $URL$
}
unit XMLDataBindingUtils;

interface
uses
  Classes,
  SysUtils,
  XMLIntf;


type
  EBase64Error        = class(Exception);

  TXMLDateTimeFormat  = (xdtDateTime, xdtDate, xdtTime);
  TXMLTimeFragment    = (xtfMilliseconds, xtfTimezone);
  TXMLTimeFragments   = set of TXMLTimeFragment;


  IXSDValidate  = interface
    ['{3BFDC851-7459-403B-87B3-A52E9E85BC8C}']
    procedure XSDValidate;
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
  procedure CreateRequiredElements(AParent: IXMLNode; ANodes: array of string);
  procedure CreateRequiredAttributes(AParent: IXMLNode; ANodes: array of string);
  procedure SortChildNodes(AParent: IXMLNode; ASortOrder: array of string);


  { Now wraps the JclMime implementation:
      Lightening fast Mime (Base64) Encoding and Decoding routines.
      Coded by Ralf Junker (ralfjunker@gmx.de).}
  function Base64Encode(AValue: String): String;
  function Base64Decode(AValue: String): String;
  procedure Base64DecodeToStream(AValue: String; AStream: TStream);
  procedure Base64DecodeToFile(AValue: String; const AFileName: String);

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


  function MimeEncodeString(const S: AnsiString): AnsiString; forward;
  function MimeDecodeString(const S: AnsiString): AnsiString; forward;
  procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream); forward;
  procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream); forward;
  function MimeEncodedSize(const I: Cardinal): Cardinal; forward;
  function MimeDecodedSize(const I: Cardinal): Cardinal; forward;
  procedure MimeEncode(var InputBuffer; const InputByteCount: Cardinal; var OutputBuffer); forward;
  function MimeDecode(var InputBuffer; const InputBytesCount: Cardinal; var OutputBuffer): Cardinal; forward;
  function MimeDecodePartial(var InputBuffer; const InputBytesCount: Cardinal; var OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal; forward;
  function MimeDecodePartialEnd(var OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal; forward;



function DateTimeToXML(ADate: TDateTime; AFormat: TXMLDateTimeFormat; ATimeFragments: TXMLTimeFragments): string;
var
  formatSettings: TFormatSettings;
  timeZone: TTimeZoneInformation;
  timeOffset: Integer;

begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, formatSettings);
  Result  := FormatDateTime(XMLDateTimeFormats[AFormat], ADate, formatSettings);

  if AFormat in [xdtDateTime, xdtTime] then
  begin
    if xtfMilliseconds in ATimeFragments then
      Result  := Result + FormatDateTime(XMLMsecsFormat, ADate);

    if xtfTimezone in ATimeFragments then
    begin
      FillChar(timeZone, SizeOf(TTimeZoneInformation), #0);
      if GetTimeZoneInformation(timeZone) <> TIME_ZONE_ID_INVALID then
      begin
        timeOffset  := -timeZone.Bias;

        if timeOffset = 0 then
          Result  := Result + XMLTimezoneZulu
        else
          Result  := Result + Format(XMLTimezoneFormat,
                                     [XMLTimezoneSigns[timeOffset > 0],
                                      Abs(timeZone.Bias div 60),
                                      Abs(timeZone.Bias mod 60)]);
      end;
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
  timeZone: TTimeZoneInformation;
  localOffset: Integer;

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
          { Parse milliseconds (.zzz) }
          if not TryStrToInt(Copy(time, 2, 3), msec) then
            msec  := 0;

          Delete(time, 1, 4);
        end;
      end;

      Result  := Result + EncodeTime(hour, minute, second, msec);

      if Length(time) > 0 then
      begin
        hasTimezone := False;
        xmlOffset   := 0;

        if time[1] = XMLTimezoneZulu then
        begin
          { Zulu time }
          hasTimezone := True;
        end else if time[1] in [XMLTimezoneSigns[False], XMLTimezoneSigns[True]] then
        begin
          { Parse timezone ([+|-]xx:xx) }
          if TryStrToInt(Copy(time, 2, 2), hour) and
             TryStrToInt(Copy(time, 5, 2), minute) then
          begin
            xmlOffset   := (hour * MinsPerHour) + minute;
            hasTimezone := True;

            if time[1] = XMLTimezoneSigns[False] then
              xmlOffset := -xmlOffset;
          end;
        end;

        if hasTimezone then
        begin
          FillChar(timeZone, SizeOf(TTimeZoneInformation), #0);
          if GetTimeZoneInformation(timeZone) <> TIME_ZONE_ID_INVALID then
          begin
            localOffset := -timeZone.Bias;
            Result      := IncMinute(Result, localOffset - xmlOffset);
          end;
        end;
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
  Result := MimeEncodeString(AValue);
end;


function Base64Decode(AValue: String): String;
begin
  Result := MimeDecodeString(AValue);
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
  input: TStringStream;
  output: TFileStream;

begin
  input := TStringStream.Create(AValue);
  try
    output := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
    try
      MimeDecodeStream(input, output);
    finally
      FreeAndNil(output);
    end;
  finally
    FreeAndNil(input);
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


procedure CreateRequiredElements(AParent: IXMLNode; ANodes: array of string);
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


{ --- JclMime implementation from here. }
// Caution: For MimeEncodeStream and all other kinds of multi-buffered
// Mime encodings (i.e. Files etc.), BufferSize must be set to a multiple of 3.
// Even though the implementation of the Mime decoding routines below
// do not require a particular buffer size, they work fastest with sizes of
// multiples of four. The chosen size is a multiple of 3 and of 4 as well.
// The following numbers are, in addition, also divisible by 1024:
// $2400, $3000, $3C00, $4800, $5400, $6000, $6C00.

const
  BUFFER_SIZE = $3000;
  EqualSign = Byte('=');

  MIME_ENCODE_TABLE: array [0..63] of Byte = (
     65,  66,  67,  68,  69,  70,  71,  72,  // 00 - 07
     73,  74,  75,  76,  77,  78,  79,  80,  // 08 - 15
     81,  82,  83,  84,  85,  86,  87,  88,  // 16 - 23
     89,  90,  97,  98,  99, 100, 101, 102,  // 24 - 31
    103, 104, 105, 106, 107, 108, 109, 110,  // 32 - 39
    111, 112, 113, 114, 115, 116, 117, 118,  // 40 - 47
    119, 120, 121, 122,  48,  49,  50,  51,  // 48 - 55
     52,  53,  54,  55,  56,  57,  43,  47); // 56 - 63

  MIME_DECODE_TABLE: array [Byte] of Cardinal = (
    255, 255, 255, 255, 255, 255, 255, 255, //  00 -  07
    255, 255, 255, 255, 255, 255, 255, 255, //  08 -  15
    255, 255, 255, 255, 255, 255, 255, 255, //  16 -  23
    255, 255, 255, 255, 255, 255, 255, 255, //  24 -  31
    255, 255, 255, 255, 255, 255, 255, 255, //  32 -  39
    255, 255, 255,  62, 255, 255, 255,  63, //  40 -  47
     52,  53,  54,  55,  56,  57,  58,  59, //  48 -  55
     60,  61, 255, 255, 255, 255, 255, 255, //  56 -  63
    255,   0,   1,   2,   3,   4,   5,   6, //  64 -  71
      7,   8,   9,  10,  11,  12,  13,  14, //  72 -  79
     15,  16,  17,  18,  19,  20,  21,  22, //  80 -  87
     23,  24,  25, 255, 255, 255, 255, 255, //  88 -  95
    255,  26,  27,  28,  29,  30,  31,  32, //  96 - 103
     33,  34,  35,  36,  37,  38,  39,  40, // 104 - 111
     41,  42,  43,  44,  45,  46,  47,  48, // 112 - 119
     49,  50,  51, 255, 255, 255, 255, 255, // 120 - 127
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

type
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


//------------------------------------------------------------------------------
// Wrapper functions & procedures
//------------------------------------------------------------------------------

function MimeEncodeString(const S: AnsiString): AnsiString;
var
  L: Cardinal;
begin
  L := Length(S);
  if L > 0 then
  begin
    SetLength(Result, MimeEncodedSize(L));
    MimeEncode(PChar(S)^, L, PChar(Result)^);
  end
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function MimeDecodeString(const S: AnsiString): AnsiString;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  L: Cardinal;
begin
  L := Length(S);
  if L > 0 then
  begin
    SetLength(Result, MimeDecodedSize(L));
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    L := MimeDecodePartial(PChar(S)^, L, PChar(Result)^, ByteBuffer, ByteBufferSpace);
    Inc(L, MimeDecodePartialEnd(PChar(Cardinal(Result) + L)^, ByteBuffer, ByteBufferSpace));
    SetLength(Result, L);
  end;
end;

//------------------------------------------------------------------------------

procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array [0..BUFFER_SIZE - 1] of Byte;
  OutputBuffer: array [0..((BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
  BytesRead: Integer;
begin
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead > 0 do
  begin
    MimeEncode(InputBuffer, BytesRead, OutputBuffer);
    OutputStream.Write(OutputBuffer, MimeEncodedSize(BytesRead));
    BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  end;
end;

//------------------------------------------------------------------------------

procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  InputBuffer: array [0..(BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
  OutputBuffer: array [0..BUFFER_SIZE - 1] of Byte;
  BytesRead: Integer;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead > 0 do
  begin
    OutputStream.Write(OutputBuffer, MimeDecodePartial(InputBuffer, BytesRead, OutputBuffer, ByteBuffer, ByteBufferSpace));
    BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  end;
  OutputStream.Write(OutputBuffer, MimeDecodePartialEnd(OutputBuffer, ByteBuffer, ByteBufferSpace));
end;

//------------------------------------------------------------------------------
// Helper functions
//------------------------------------------------------------------------------

function MimeEncodedSize(const I: Cardinal): Cardinal;
begin
  Result := (I + 2) div 3 * 4;
end;

//------------------------------------------------------------------------------

function MimeDecodedSize(const I: Cardinal): Cardinal;
begin
  Result := (I + 3) div 4 * 3;
end;

//------------------------------------------------------------------------------
// Primary functions & procedures
//------------------------------------------------------------------------------

procedure MimeEncode(var InputBuffer; const InputByteCount: Cardinal; var OutputBuffer);
var
  B: Cardinal;
  InMax3: Cardinal;
  InPtr, InLimitPtr: ^Byte;
  OutPtr: PByte4;
begin
  if InputByteCount <= 0 then
    Exit;

  InPtr := @InputBuffer;
  InMax3 := InputByteCount div 3 * 3;
  OutPTr := @OutputBuffer;
  Cardinal(InLimitPtr) := Cardinal(InPtr) + InMax3;

  while InPtr <> InLimitPtr do
  begin
    B := InPtr^;
    B := B shl 8;
    Inc(InPtr);
    B := B or InPtr^;
    B := B shl 8;
    Inc(InPtr);
    B := B or InPtr^;
    Inc(InPtr);
    // Write 4 bytes to OutputBuffer (in reverse order).
    OutPtr.B4 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr.B3 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr.B2 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr.B1 := MIME_ENCODE_TABLE[B];
    Inc(OutPtr);
  end;

  case InputByteCount - InMax3 of
    1:
      begin
        B := InPtr^;
        B := B shl 4;
        OutPtr.B2 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := MIME_ENCODE_TABLE[B];
        OutPtr.B3 := EqualSign; // Fill remaining 2 bytes.
        OutPtr.B4 := EqualSign;
      end;
    2:
      begin
        B := InPtr^;
        Inc(InPtr);
        B := B shl 8;
        B := B or InPtr^;
        B := B shl 2;
        OutPtr.B3 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPTr.b2 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := MIME_ENCODE_TABLE[B];
        OutPtr.B4 := EqualSign; // Fill remaining byte.
      end;
  end;
end;

//------------------------------------------------------------------------------

function MimeDecode(var InputBuffer; const InputBytesCount: Cardinal; var OutputBuffer): Cardinal;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  Result := MimeDecodePartial(InputBuffer, InputBytesCount, OutputBuffer, ByteBuffer, ByteBufferSpace);
  Inc(Result, MimeDecodePartialEnd(PChar(Cardinal(OutputBuffer) + Result)^, ByteBuffer, ByteBufferSpace));
end;

//------------------------------------------------------------------------------

function MimeDecodePartial(var InputBuffer; const InputBytesCount: Cardinal;
  var OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
var
  lByteBuffer, lByteBufferSpace, C: Cardinal;
  InPtr, InLimitPtr: ^Byte;
  OutPtr: PByte3;
begin
  if InputBytesCount > 0 then
  begin
    InPtr := @InputBuffer;
    Cardinal(InLimitPtr) := Cardinal(InPtr) + InputBytesCount;
    OutPtr := @OutputBuffer;
    lByteBuffer := ByteBuffer;
    lByteBufferSpace := ByteBufferSpace;
    while InPtr <> InLimitPtr do
    begin
      C := MIME_DECODE_TABLE[InPtr^]; // Read from InputBuffer.
      Inc(InPtr);
      if C = $FF then
        Continue;

      lByteBuffer := lByteBuffer shl 6;
      lByteBuffer := lByteBuffer or C;
      Dec(lByteBufferSpace);
      if lByteBufferSpace <> 0 then
        Continue; // Read 4 bytes from InputBuffer?

      OutPtr.B3 := Byte(lByteBuffer); // Write 3 bytes to OutputBuffer (in reverse order).
      lByteBuffer := lByteBuffer shr 8;
      OutPtr.B2 := Byte(lByteBuffer);
      lByteBuffer := lByteBuffer shr 8;
      OutPtr.B1 := Byte(lByteBuffer);
      lByteBuffer := 0;
      Inc(OutPtr);
      lByteBufferSpace := 4;
    end;
    ByteBuffer := lByteBuffer;
    ByteBufferSpace := lByteBufferSpace;
    Result := Cardinal(OutPtr) - Cardinal(@OutputBuffer);
  end
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function MimeDecodePartialEnd(var OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): Cardinal;
var
  lByteBuffer: Cardinal;
begin
  case ByteBufferSpace of
    1:
      begin
        lByteBuffer := ByteBuffer shr 2;
        PByte3(@OutputBuffer).B2 := Byte(lByteBuffer);
        lByteBuffer := lByteBuffer shr 8;
        PByte3(@OutputBuffer).B1 := Byte(lByteBuffer);
        Result := 2;
      end;
    2:
      begin
        lByteBuffer := ByteBuffer shr 4;
        PByte3(@OutputBuffer).B1 := Byte(lByteBuffer);
        Result := 1;
      end;
  else
    Result := 0;
  end;
end;

end.

