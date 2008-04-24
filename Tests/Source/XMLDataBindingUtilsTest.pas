unit XMLDataBindingUtilsTest;

interface
uses
  TestFramework;


type
  TXMLDataBindingUtilsTest = class(TTestCase)
  protected
    procedure CheckEqualsDateTime(AExpected, AActual: TDateTime; const AMsg: string = '');
  published
    procedure ToXMLDate;
    procedure ToXMLTime;
    procedure ToXMLDateTime;

    procedure ToDate;
    procedure ToTime;
    procedure ToDateTime;
  end;


implementation
uses
  DateUtils,
  SysUtils,

  XMLDataBindingUtils;

const
  DateDelta = 0.00000001;


{ TXMLDateUtilsTest }
procedure TXMLDataBindingUtilsTest.ToXMLDate;
begin
  CheckEquals('2008-05-23', DateTimeToXML(EncodeDate(2008, 5, 23), xdtDate));
end;


procedure TXMLDataBindingUtilsTest.ToXMLTime;
var
  date: TDateTime;

begin
  date  := EncodeTime(14, 38, 02, 507);

  CheckEquals('14:38:02', DateTimeToXML(date, xdtTime, []), 'No time fragments');
  CheckEquals('14:38:02.507', DateTimeToXML(date, xdtTime, [xtfMilliseconds]), 'Milliseconds');
  // (MvR) 23-4-2008: dit werkt alleen met GMT+1 locale...
  CheckEquals('14:38:02.507+01:00', DateTimeToXML(date, xdtTime), 'All time fragments');
end;


procedure TXMLDataBindingUtilsTest.ToXMLDateTime;
var
  date: TDateTime;

begin
  date  := EncodeDate(2008, 5, 23) + EncodeTime(14, 38, 02, 507);

  CheckEquals('2008-05-23T14:38:02', DateTimeToXML(date, xdtDateTime, []), 'No time fragments');
  CheckEquals('2008-05-23T14:38:02.507', DateTimeToXML(date, xdtDateTime, [xtfMilliseconds]), 'Milliseconds');
  // (MvR) 23-4-2008: dit werkt alleen met GMT+1 locale...
  CheckEquals('2008-05-23T14:38:02.507+01:00', DateTimeToXML(date, xdtDateTime), 'All time fragments');
end;


procedure TXMLDataBindingUtilsTest.ToDate;
begin
  CheckEqualsDateTime(EncodeDate(2008, 5, 23), XMLToDateTime('2008-05-23', xdtDate));
end;


procedure TXMLDataBindingUtilsTest.ToTime;
var
  date: TDateTime;

begin
  date  := EncodeTime(14, 38, 02, 0);
  CheckEqualsDateTime(date, XMLToDateTime('14:38:02', xdtTime), 'No time fragments');

  date  := EncodeTime(14, 38, 02, 507);
  CheckEqualsDateTime(date, XMLToDateTime('14:38:02.507', xdtTime), 'Milliseconds');
  // (MvR) 23-4-2008: dit werkt alleen met GMT+1 locale...
  CheckEqualsDateTime(IncHour(date, -1), XMLToDateTime('14:38:02.507+02:00', xdtTime), 'All time fragments');
  CheckEqualsDateTime(IncHour(date), XMLToDateTime('14:38:02.507Z', xdtTime), 'All time fragments');
end;


procedure TXMLDataBindingUtilsTest.ToDateTime;
var
  date: TDateTime;

begin
  date  := EncodeDate(2008, 5, 23) + EncodeTime(14, 38, 02, 0);
  CheckEqualsDateTime(date, XMLToDateTime('2008-05-23T14:38:02', xdtDateTime), 'No time fragments');

  date  := EncodeDate(2008, 5, 23) + EncodeTime(14, 38, 02, 507);
  CheckEqualsDateTime(date, XMLToDateTime('2008-05-23T14:38:02.507', xdtDateTime), 'Milliseconds');
  // (MvR) 23-4-2008: dit werkt alleen met GMT+1 locale...
  CheckEqualsDateTime(date, XMLToDateTime('2008-05-23T14:38:02.507+01:00', xdtDateTime), 'All time fragments');
end;


procedure TXMLDataBindingUtilsTest.CheckEqualsDateTime(AExpected, AActual: TDateTime; const AMsg: string);
begin
  if Abs(AExpected - AActual) > DateDelta then
    FailNotEquals(DateTimeToStr(AExpected), DateTimeToStr(AActual), AMsg);
end;


initialization
  RegisterTest('XMLDataBindingUtils', TXMLDataBindingUtilsTest.Suite);
  
end.

