unit XMLDataBindingGenerator;

interface
uses
  Classes,
  Contnrs,
  XMLSchema;

type
  TXMLDataBindingSchema = class(TObject)
  private
    FSchemaDef:     IXMLSchemaDef;
    FSchemaName:    String;
  public
    property SchemaDef:     IXMLSchemaDef read FSchemaDef   write FSchemaDef;
    property SchemaName:    String        read FSchemaName  write FSchemaName;
  end;

  TXMLDataBindingOutputType = (otSingle, otMultiple);

  TXMLDataBindingGenerator = class(TObject)
  private
    FIncludePaths:      TStrings;
    FOutputPath:        string;
    FOutputType:        TXMLDataBindingOutputType;
    FSourceFileName:    String;

    FSchemas:           TObjectList;

    function GetSchemaCount(): Integer;
    function GetSchema(Index: Integer): TXMLDataBindingSchema;
  protected
    function LoadSchema(const AStream: TStream; const ASchemaName: String): IXMLSchemaDef;
    function FindSchema(const ALocation: String): TStream;
    function SchemaLoaded(const ALocation: String): Boolean;

    procedure GenerateDataBinding(); virtual; abstract;

    property SourceFileName:          String                read FSourceFileName  write FSourceFileName;
    property SchemaCount:             Integer               read GetSchemaCount;
    property Schema[Index: Integer]:  TXMLDataBindingSchema read GetSchema;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure Execute(const AStream: TStream; const ASchemaName: String); overload;
    procedure Execute(const AFileName: String); overload;

    property IncludePaths:    TStrings                  read FIncludePaths;
    property OutputType:      TXMLDataBindingOutputType read FOutputType      write FOutputType;
    property OutputPath:      string                    read FOutputPath      write FOutputPath;
  end;


implementation
uses
  SysUtils,

  XMLDoc,
  XMLIntf;


{ TXMLDataBindingGenerator }
constructor TXMLDataBindingGenerator.Create();
begin
  inherited;

  FIncludePaths := TStringList.Create();
  FSchemas      := TObjectList.Create(True);

  with TStringList(FIncludePaths) do
  begin
    CaseSensitive := False;
    Duplicates    := dupIgnore;
  end;
end;


destructor TXMLDataBindingGenerator.Destroy();
begin
  FreeAndNil(FSchemas);
  FreeAndNil(FIncludePaths);

  inherited;
end;


procedure TXMLDataBindingGenerator.Execute(const AStream: TStream; const ASchemaName: String);
//var
//  schemaIndex:    Integer;
//
begin
  FSchemas.Clear();
  LoadSchema(AStream, ASchemaName);

  if SchemaCount > 0 then
    GenerateDataBinding();
end;


procedure TXMLDataBindingGenerator.Execute(const AFileName: String);
var
  currentDir:   String;
  fileStream:   TFileStream;

begin
  currentDir  := GetCurrentDir();
  try
    ChDir(ExtractFilePath(AFileName));

    fileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      FSourceFileName := AFileName;
      IncludePaths.Add(ExtractFilePath(AFileName));

      Execute(fileStream, ChangeFileExt(ExtractFileName(AFileName), ''));
    finally
      FreeAndNil(fileStream);
    end;
  finally
    ChDir(currentDir);
  end;
end;



function TXMLDataBindingGenerator.LoadSchema(const AStream: TStream; const ASchemaName: String): IXMLSchemaDef;

  procedure HandleDocRefs(const ADocRefs: IXMLSchemaDocRefs);
  var
    location:       String;
    refIndex:       Integer;
    refStream:      TStream;

  begin
    for refIndex := 0 to Pred(ADocRefs.Count) do
    begin
      location      := ADocRefs[refIndex].SchemaLocation;

      if not SchemaLoaded(ChangeFileExt(location, '')) then
      begin
        refStream     := FindSchema(location);

        if Assigned(refStream) then
        try
          location    := ChangeFileExt(ExtractFileName(location), '');
          LoadSchema(refStream, location);
        finally
          FreeAndNil(refStream);
        end;
      end;
    end;
  end;


var
  schema:         TXMLDataBindingSchema;
  schemaDoc:      IXMLSchemaDoc;

begin
  schemaDoc := TXMLSchemaDoc.Create(nil);
  schemaDoc.LoadFromStream(AStream);
  Result    := schemaDoc.SchemaDef;

  schema            := TXMLDataBindingSchema.Create();
  schema.SchemaDef  := Result;
  schema.SchemaName := ASchemaName;
  FSchemas.Add(schema);

  { Handle imports / includes }
  HandleDocRefs(Result.SchemaImports);
  HandleDocRefs(Result.SchemaIncludes);
end;


function TXMLDataBindingGenerator.FindSchema(const ALocation: String): TStream;
var
  includeIndex:   Integer;
  includePath:    String;

begin
  Result := nil;

  // #ToDo3 (MvR) 31-1-2007: support more locations than just a filename ?

  for includeIndex := 0 to Pred(IncludePaths.Count) do
  begin
    includePath := IncludeTrailingPathDelimiter(IncludePaths[includeIndex]);

    if FileExists(includePath + ALocation) then
    begin
      Result := TFileStream.Create(includePath + ALocation, fmOpenRead or fmShareDenyNone);
      break;
    end;
  end;
end;


function TXMLDataBindingGenerator.SchemaLoaded(const ALocation: String): Boolean;
var
  schemaIndex: Integer;

begin
  Result  := False;
  
  for schemaIndex := 0 to Pred(SchemaCount) do
    if Schema[schemaIndex].SchemaName = ALocation then
    begin
      Result  := True;
      break;
    end;
end;


function TXMLDataBindingGenerator.GetSchemaCount(): Integer;
begin
  Result  := FSchemas.Count;
end;


function TXMLDataBindingGenerator.GetSchema(Index: Integer): TXMLDataBindingSchema;
begin
  Result  := TXMLDataBindingSchema(FSchemas[Index]);
end;

end.

