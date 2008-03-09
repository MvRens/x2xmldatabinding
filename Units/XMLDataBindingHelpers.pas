unit XMLDataBindingHelpers;

interface
uses
  Classes;

type
  TStreamHelper = class(TObject)
  private
    FOwnership:   TStreamOwnership;
    FStream:      TStream;
  public
    constructor Create(AStream: TStream; AOwnership: TStreamOwnership = soReference);
    destructor Destroy(); override;

    function ReadString(ASize: Integer = -1): String;
    function ReadInteger(): Integer;
    function ReadDateTime(): TDateTime;
    function ReadBoolean(): Boolean;

    procedure Write(const ASource: String);
    procedure WriteLn(const ASource: String = '');

    procedure WriteFmt(const ASource: String; const AParams: array of const);
    procedure WriteLnFmt(const ASource: String; const AParams: array of const);

    procedure WriteString(const ASource: String);
    procedure WriteInteger(const ASource: Integer);
    procedure WriteDateTime(const ASource: TDateTime);
    procedure WriteBoolean(const ASource: Boolean);
  end;


implementation
uses
  SysUtils;


{ TStreamHelper }
constructor TStreamHelper.Create(AStream: TStream; AOwnership: TStreamOwnership);
begin
  FOwnership  := AOwnership;
  FStream     := AStream;

  inherited Create();
end;


destructor TStreamHelper.Destroy();
begin
  if FOwnership = soOwned then
    FreeAndNil(FStream);

  inherited;
end;


function TStreamHelper.ReadString(ASize: Integer): String;
var
  iSize:      Integer;

begin
  if ASize = -1 then
    iSize := ReadInteger()
  else
    iSize := ASize;

  SetLength(Result, iSize);
  FStream.Read(PChar(Result)^, iSize);
end;


function TStreamHelper.ReadInteger(): Integer;
begin
  FStream.Read(Result, SizeOf(Integer));
end;


function TStreamHelper.ReadDateTime(): TDateTime;
begin
  FStream.Read(Result, SizeOf(TDateTime));
end;


function TStreamHelper.ReadBoolean(): Boolean;
begin
  FStream.Read(Result, SizeOf(Boolean));
end;


procedure TStreamHelper.Write(const ASource: String);
begin
  FStream.Write(PChar(ASource)^, Length(ASource));
end;


procedure TStreamHelper.WriteLn(const ASource: String);
begin
  Write(ASource + #13#10);
end;


procedure TStreamHelper.WriteFmt(const ASource: String; const AParams: array of const);
begin
  Write(Format(ASource, AParams));
end;


procedure TStreamHelper.WriteLnFmt(const ASource: String; const AParams: array of const);
begin
  WriteLn(Format(ASource, AParams));
end;


procedure TStreamHelper.WriteString(const ASource: String);
var
  iSize:      Integer;

begin
  iSize := Length(ASource);
  WriteInteger(iSize);
  FStream.Write(PChar(ASource)^, iSize);
end;


procedure TStreamHelper.WriteInteger(const ASource: Integer);
begin
  FStream.Write(ASource, SizeOf(Integer));
end;


procedure TStreamHelper.WriteDateTime(const ASource: TDateTime);
begin
  FStream.Write(ASource, SizeOf(TDateTime));
end;


procedure TStreamHelper.WriteBoolean(const ASource: Boolean);
begin
  FStream.Write(ASource, SizeOf(Boolean));
end;

end.


