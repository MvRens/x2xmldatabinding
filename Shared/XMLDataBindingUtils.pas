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

  function Base64Encode(AValue: String): String;
  function Base64Decode(AValue: String): String;

  function GetNodeIsNil(ANode: IXMLNode): Boolean;
  procedure SetNodeIsNil(ANode: IXMLNode; ASetNil: Boolean);

  procedure XSDValidate(AParent: IXMLNode; ARecurse: Boolean = True; AValidateParent: Boolean = True);
  procedure CreateRequiredElements(AParent: IXMLNode; ANodes: array of string);
  procedure CreateRequiredAttributes(AParent: IXMLNode; ANodes: array of string);
  procedure SortChildNodes(AParent: IXMLNode; ASortOrder: array of string);
  

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
var
  pos: Integer;
  lookupIndex: array[0..3] of Byte;
  padCount: Integer;

begin
  Result    := '';
  if Length(AValue) = 0 then
    exit;

  padCount  := 0;

  { At least 3 input bytes are required, and the input must be a multiple of 3 }
  if Length(AValue) < 3 then
    padCount  := 3 - Length(AValue)
  else if Length(AValue) mod 3 <> 0 then
    padCount  := 3 - (Length(AValue) mod 3);

  if padCount > 0 then
    AValue    := AValue + StringOfChar(#0, padCount);

  pos := 1;

  { Process in 3-byte blocks }
  while pos <= Length(AValue) - 2 do
  begin
    { Each 3 input bytes are converted into 4 index values
      in the range of 0..63, by taking 6 bits each step.

      6 high bytes of first char }
    lookupIndex[0]  := (Ord(AValue[pos]) shr 2) and $3F;

    { 2 low bytes of first char + 4 high bytes of second char }
    lookupIndex[1]  := ((Ord(AValue[pos]) shl 4) and $3F) or
                       (Ord(AValue[pos + 1]) shr 4);

    { 4 low bytes of second char + 2 high bytes of third char }
    lookupIndex[2]  :=((Ord(AValue[pos + 1]) shl 2) and $3F) or
                      (Ord(AValue[pos + 2]) shr 6);

    { 6 low bytes of third char }
    lookupIndex[3]  := Ord(AValue[pos + 2]) and $3F;

    Result := Result + Base64LookupTable[lookupIndex[0] + 1] +
                       Base64LookupTable[lookupIndex[1] + 1] +
                       Base64LookupTable[lookupIndex[2] + 1] +
                       Base64LookupTable[lookupIndex[3] + 1];
    Inc(pos, 3);
  end;

  { Replace padding }
  if padCount > 0 then
  begin
    for pos := Length(Result) downto Length(Result) - Pred(padCount) do
      Result[pos] := Base64Padding;
  end;
end;


function Base64LookupIndex(AChar: Char): Byte;
var
  lookupIndex:  Integer;

begin
  Result  := Ord(Base64Padding);

  for lookupIndex := 1 to Length(Base64LookupTable) do
    if Base64LookupTable[lookupIndex] = AChar then
    begin
      Result  := Pred(lookupIndex);
      break;
    end;
end;


function Base64Decode(AValue: String): String;
var
  pos: Integer;
  padCount: Integer;
  value: Byte;

begin
  Result  := '';
  if Length(AValue) = 0 then
    exit;

  if Length(AValue) mod 4 <> 0 then
    raise EBase64Error.Create('Value length must be a multiple of 4');

  padCount := 0;
  pos := Length(AValue);

  { Count padding chars }
  while (pos > 0) and (AValue[pos] = Base64Padding) do
  begin
    Inc(padCount);
    Dec(pos);
  end;
  
  Result := '';
  pos := 1;

  while pos <= Length(AValue) - 3 do
  begin
    value   := (Base64LookupIndex(AValue[pos]) shl 2) or
               (Base64LookupIndex(AValue[pos + 1]) shr 4);
    Result  := Result + Chr(value);

    value   := (Base64LookupIndex(AValue[pos + 1]) shl 4) or
               (Base64LookupIndex(AValue[pos + 2]) shr 2);
    Result  := Result + Chr(value);

    value   := (Base64LookupIndex(AValue[pos + 2]) shl 6) or
               (Base64LookupIndex(AValue[pos + 3]));
    Result  := Result + Chr(value);

    Inc(pos, 4);
  end;

  { Delete padding }
  if padCount > 0 then
    SetLength(Result, Length(Result) - padCount);
end;


function GetNodeIsNil(ANode: IXMLNode): Boolean;
begin
  Result := ANode.HasAttribute(XMLIsNilAttribute, XMLSchemaInstanceURI) and
            XMLToBool(ANode.GetAttributeNS(XMLIsNilAttribute, XMLSchemaInstanceURI));
end;


procedure SetNodeIsNil(ANode: IXMLNode; ASetNil: Boolean);
begin
  if ASetNil then
  begin
    ANode.ChildNodes.Clear;
    ANode.SetAttributeNS(XMLIsNilAttribute, XMLSchemaInstanceURI, BoolToXML(True));
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

  else if (nodeInfo1^.SortIndex = -1) and (nodeInfo2^.SortIndex = -1) then
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

end.

