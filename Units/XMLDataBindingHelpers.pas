unit XMLDataBindingHelpers;

interface
uses
  System.Classes;


type
  TNamedFormatWriter = class(TStreamWriter)
  public
    procedure WriteNamedFmt(const ASource: String; const AParams: array of const);
    procedure WriteLineNamedFmt(const ASource: String; const AParams: array of const);
  end;


implementation
uses
  SysUtils,

  X2UtNamedFormat;


{ TNamedFormatWriter }
procedure TNamedFormatWriter.WriteNamedFmt(const ASource: String; const AParams: array of const);
begin
  Write(NamedFormat(ASource, AParams));
end;


procedure TNamedFormatWriter.WriteLineNamedFmt(const ASource: String; const AParams: array of const);
begin
  WriteLine(NamedFormat(ASource, AParams));
end;

end.

