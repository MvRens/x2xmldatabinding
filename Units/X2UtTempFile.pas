{: Provides temporary file functions.

   Last changed:    $Date$
   Revision:        $Rev$
   Author:          $Author$
}
unit X2UtTempFile;

interface
  function GetAppDataPath(): String;

  function GetTempFile(const APrefix: String): String; overload;
  function GetTempFile(const APath, AFileName, AExtension: String): String; overload;
  function GetTempFile(const APath, AFileName: String): String; overload;
  function GetTempAppDataFile(const ASubPath, AFileName, AExtension: String): String; overload;
  function GetTempAppDataFile(const ASubPath, AFileName: String): String; overload;

  function IsValidFileChar(const AChar: Char): Boolean;
  function CheckValidFileName(var AFileName: String; const AReplacement: Char = #0): Boolean;

implementation
uses
  ShlObj,
  SysUtils,
  Windows;

function GetAppDataPath(): String;
var
  path:     array[0..MAX_PATH] of Char;

begin
  FillChar(path, SizeOf(path), #0);
  if not SHGetSpecialFolderPath(0, @path, CSIDL_APPDATA, True) then
  begin
    FillChar(path, SizeOf(path), #0);
    GetTempPath(SizeOf(path), @path);
  end;

  Result  := path;
  if Length(Result) > 0 then
    Result := IncludeTrailingPathDelimiter(Result);
end;


function GetTempFile(const APrefix: String): String; overload;
var
  tempPath: array[0..MAX_PATH] of Char;
  tempFile: array[0..MAX_PATH] of Char;

begin
  FillChar(tempPath, SizeOf(tempPath), #0);
  FillChar(tempFile, SizeOf(tempFile), #0);

  Windows.GetTempPath(SizeOf(tempPath), @tempPath);
  Windows.GetTempFileName(@tempPath, PChar(APrefix), 0, @tempFile);

  Result := String(tempFile);
end;


function GetTempFile(const APath, AFileName, AExtension: String): String; overload;
var
  iCounter:         Integer;
  sBase:            String;
  sExtension:       String;

begin
  iCounter    := 0;
  sBase       := IncludeTrailingPathDelimiter(APath);

  if not ForceDirectories(sBase) then
  begin
    Result    := '';
    exit;
  end;

  sExtension  := AExtension;
  if (Length(sExtension) > 0) and (AnsiPos('.', sExtension) = 0) then
    sExtension  := '.' + sExtension;

  sBase       := sBase + AFileName;
  Result      := sBase + sExtension;

  while FileExists(Result) do
  begin
    Inc(iCounter);
    Result  := Format('%s(%d)%s', [sBase, iCounter, sExtension]);
  end;
end;


function GetTempFile(const APath, AFileName: String): String; overload;
var
  sExt:     String;

begin
  sExt    := ExtractFileExt(AFileName);
  Result  := GetTempFile(APath, Copy(AFileName, 1, Length(AFileName) - Length(sExt)),
                      sExt);
end;


function GetTempAppDataFile(const ASubPath, AFileName, AExtension: String): String; overload;
begin
  Result  := GetTempFile(GetAppDataPath + ASubPath, AFileName, AExtension);
end;


function GetTempAppDataFile(const ASubPath, AFileName: String): String; overload;
var
  sExt:     String;

begin
  sExt    := ExtractFileExt(AFileName);
  Result  := GetTempAppDataFile(ASubPath, Copy(AFileName, 1,
                                               Length(AFileName) - Length(sExt)),
                                               sExt);
end;


function IsValidFileChar(const AChar: Char): Boolean;
begin
  Result  := not CharInSet(AChar, ['\', '/', ':', '*', '?', '"', '<', '>', '|']);
end;


function CheckValidFileName(var AFileName: String; const AReplacement: Char): Boolean;
var
  iPos:     Integer;

begin
  Result  := True;
  
  for iPos := Length(AFileName) downto 1 do
    if not IsValidFileChar(AFileName[iPos]) then
    begin
      Result  := False;
      if AReplacement = #0 then
        Delete(AFileName, iPos, 1)
      else
        AFileName[iPos] := AReplacement;
    end;
end;

end.
