{
  :: X2UtNamedFormat implements Format-style functionality using named
  :: instead of indexed parameters.
  ::
  :: Last changed:    $Date$
  :: Revision:        $Rev$
  :: Author:          $Author$
}
unit X2UtNamedFormat;

interface
uses
  Classes, 
  SysUtils;


type
  TNamedFormatStringList  = class(TStringList)
  public
    procedure AddLn();

    function Format(AParams: array of const): String;
  end;


  {
    AFormat uses the same format strings as SysUtils.Format, where each
    format specifier may use a named instead of a numeric index, surrounded by
    <>, eg:

      %<Value1>:s %<Value2>:.2d


    AParams contains alternating the parameter name and it's value.

    Note: NamedFormat works by mapping names to indices and passing the result
    to SysUtils.Format. Unnamed or existing indexed specifiers will therefore
    be affected by named specifiers! It is strongly recommended to name all
    specifiers.
  }
  function NamedFormat(const AFormat: String; AParams: array of const; AFormatSettings: TFormatSettings): String; overload;
  function NamedFormat(const AFormat: String; AParams: array of const): String; overload;


implementation
uses
  Windows;

type
  TProtectedMemoryStream  = class(TMemoryStream);


const
  SpecifierChar       = '%';
  SpecifierNameStart  = '<';
  SpecifierNameEnd    = '>';
  ValidNameChars      = ['A'..'Z', 'a'..'z', '0'..'9', '_'];


procedure StreamWriteChar(const AStream: TStream; const AValue: Char);
begin
  AStream.WriteBuffer(AValue, SizeOf(Char));
end;


procedure StreamWriteString(const AStream: TStream; const AValue: String);
begin
  AStream.WriteBuffer(PChar(AValue)^, Length(AValue) * SizeOf(Char));
end;


function FindNameEnd(const APosition: PChar; const AEnd: PChar): PChar;
var
  position:   PChar;

begin
  Result    := nil;
  position  := APosition;

  while position < AEnd do
  begin
    if position^ = SpecifierNameEnd then
    begin
      Result  := position;
      break;
    end;

    if not CharInSet(position^, ValidNameChars) then
      break;

    Inc(position);
  end;
end;


function NamedFormat(const AFormat: String; AParams: array of const): String; overload;
var
  formatSettings: TFormatSettings;

begin
  formatSettings := TFormatSettings.Create;
  Result := NamedFormat(AFormat, AParams, formatSettings);
end;


function NamedFormat(const AFormat: string; AParams: array of const; AFormatSettings: TFormatSettings): String;
var
  currentPos:     PChar;
  formatEnd:      PChar;
  formatStream:   TMemoryStream;
  formatString:   String;
  name:           String;
  nameEnd:        PChar;
  nameStart:      PChar;
  param:          TVarRec;
  paramIndex:     Integer;
  paramNames:     TStringList;
  paramValues:    array of TVarRec;
  specifierIndex: Integer;
  errorMsg:       String;

begin
  if Length(AParams) mod 2 = 1 then
    raise Exception.Create('AParams must contains a multiple of 2 number of items');

  currentPos    := PChar(AFormat);
  SetLength(paramValues, 0);

  formatEnd   := currentPos;
  Inc(formatEnd, Length(AFormat));

  paramNames    := TStringList.Create();
  try
    paramNames.CaseSensitive  := False;

    formatStream  := TMemoryStream.Create();
    try
      { Most likely scenario; the names are longer than the replacement
        indexes. }
      TProtectedMemoryStream(formatStream).Capacity := Length(AFormat) * SizeOf(Char);

      while currentPos < formatEnd do
      begin
        { Search for % }
        if currentPos^ = SpecifierChar then
        begin
          StreamWriteChar(formatStream, currentPos^);
          Inc(currentPos);

          { Check if this is indeed a named specifier }
          if (currentPos < formatEnd) and (currentPos^ = SpecifierNameStart) then
          begin
            Inc(currentPos);

            nameStart := currentPos;
            nameEnd   := FindNameEnd(currentPos, formatEnd);

            if Assigned(nameEnd) then
            begin
              SetString(name, nameStart, nameEnd - nameStart);

              specifierIndex  := paramNames.IndexOf(name);
              if specifierIndex = -1 then
                specifierIndex  := paramNames.Add(name);

              StreamWriteString(formatStream, IntToStr(specifierIndex));

              currentPos  := nameEnd;
            end;
          end else
            StreamWriteChar(formatStream, currentPos^);
        end else
          StreamWriteChar(formatStream, currentPos^);

        Inc(currentPos);
      end;

      SetString(formatString, PChar(formatStream.Memory), formatStream.Size div SizeOf(Char));
    finally
      FreeAndNil(formatStream);
    end;

    SetLength(paramValues, paramNames.Count);
    paramIndex  := 0;

    while paramIndex < High(AParams) do
    begin
      param := AParams[paramIndex];

      case param.VType of
        vtChar:           name  := string(param.VChar);
        vtString:         name  := string(param.VString^);
        vtPChar:          name  := string(param.VPChar);
        vtAnsiString:     name  := string(PChar(param.VAnsiString));
        vtWideChar:       name  := string(param.VWideChar);
        vtWideString:     name  := string(WideString(param.VWideString));
        vtUnicodeString:  name  := string(UnicodeString(param.VUnicodeString));
      else
        raise Exception.CreateFmt('Parameter name at index %d is not a string value',
                                  [paramIndex div 2]);
      end;

      Inc(paramIndex);

      specifierIndex  := paramNames.IndexOf(name);
      if specifierIndex > -1 then
        paramValues[specifierIndex] := AParams[paramIndex];

      Inc(paramIndex);
    end;

    try

      Result := Format(formatString, paramValues, AFormatSettings);
    except
      on E:EConvertError do
      begin
        errorMsg  := E.Message;

        { Translate specifiers in error messages back to names }
        for paramIndex := 0 to Pred(paramNames.Count) do
          errorMsg  := StringReplace(errorMsg, SpecifierChar + IntToStr(paramIndex) + ':',
                                     SpecifierChar + SpecifierNameStart +
                                     paramNames[paramIndex] + SpecifierNameEnd + ':',
                                     [rfReplaceAll]);

        raise EConvertError.Create(errorMsg);
      end;
    end;
  finally
    FreeAndNil(paramNames);
  end;
end;


{ TNamedFormatStringList }
procedure TNamedFormatStringList.AddLn;
begin
  Add('');
end;


function TNamedFormatStringList.Format(AParams: array of const): String;
begin
  Result := NamedFormat(Text, AParams);
end;

end.
