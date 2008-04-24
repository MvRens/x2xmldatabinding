{
  Helpers functions for the X2Software XML Data Binding

  Last changed:   $Date$
  Revision:       $Rev$
  URL:            $URL$
}
unit XMLDataBindingUtils;

interface
uses
  XMLIntf;

  
type
  TXMLDateTimeFormat  = (xdtDateTime, xdtDate, xdtTime);
  TXMLTimeFragment    = (xtfMilliseconds, xtfTimezone);
  TXMLTimeFragments   = set of TXMLTimeFragment;


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


implementation
uses
  DateUtils,
  SysUtils,
  Windows;


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

end.

