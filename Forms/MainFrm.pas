unit MainFrm;

{$WARN UNIT_PLATFORM OFF}

interface
uses
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  Forms,
  Mask,
  StdCtrls,
  XMLDOM,
  XMLIntf,

  cxButtonEdit,
  cxContainer,
  cxControls,
  cxEdit,
  cxLookAndFeels,
  cxMaskEdit,
  cxTextEdit,

  DataBindingHintsXML,
  XMLDataBindingGenerator, cxGraphics, cxLookAndFeelPainters, cxClasses;


type
  TMainForm = class(TForm)
    btnClose:                                   TButton;
    btnGenerate:                                TButton;
    btnHints:                                   TButton;
    DefaultEditStyle:                           TcxDefaultEditStyleController;
    deFolder:                                   TcxButtonEdit;
    dlgOutputFile:                              TSaveDialog;
    dlgSchema:                                  TOpenDialog;
    edtFolderPostfix:                           TcxTextEdit;
    edtFolderPrefix:                            TcxTextEdit;
    feFile:                                     TcxButtonEdit;
    feSchema:                                   TcxButtonEdit;
    gbOutput:                                   TGroupBox;
    lblFile:                                    TLabel;
    lblFolder:                                  TLabel;
    lblFolderPostfix:                           TLabel;
    lblFolderPrefix:                            TLabel;
    lblSchema:                                  TLabel;
    LookAndFeel:                                TcxLookAndFeelController;
    plOutput:                                   TPageControl;
    rbFile:                                     TRadioButton;
    rbFolder:                                   TRadioButton;
    spFile:                                     TTabSheet;
    spFolder:                                   TTabSheet;
    cbHasChecksEmpty: TCheckBox;
    cbGenerateGetOptionalOrDefault: TCheckBox;

    procedure btnCloseClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OutputTypeClick(Sender: TObject);
    procedure feFilePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure deFolderPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure feSchemaPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure feSchemaPropertiesChange(Sender: TObject);
    procedure btnHintsClick(Sender: TObject);
  private
    function CheckValidSchemaFile: Boolean;
    function CheckReadOnly(const AFileName: String): Boolean;

    procedure GetFileName(Sender: TObject; const SchemaName: String; var Path, FileName: String);

    function GetSettingsFileName(const AFileName: String): String;
    procedure LoadSettings(const AFileName: String);
    procedure SaveSettings(const AFileName: String);
  end;


implementation
uses
  FileCtrl,
  SysUtils,
  Windows,
  Generics.Collections,

  MSXMLDOM,
  MSXML2_TLB,
  X2UtNamedFormat,
  X2UtTempFile,

  DataBindingSettingsXML,
  DelphiXMLDataBindingGenerator;


type
  TProtectedXMLDataBindingItem      = class(TXMLDataBindingItem);
  TProtectedXMLDataBindingProperty  = class(TXMLDataBindingProperty);

  THintsDelphiXMLDataBindingGenerator = class(TDelphiXMLDataBindingGenerator)
  private
    FHints:       IXMLDataBindingHints;
  protected
    procedure GenerateDataBinding; override;

    procedure ProcessHints;

    procedure ProcessEnumerations;
    procedure ProcessDocumentElements;
    procedure ProcessInterfaces;
    procedure ProcessProperties;

    function FindSchema(const ASchemaName: String; out ASchema: TXMLDataBindingSchema): Boolean;
    function FindNode(const ASchemaName, AXPath: String; out AItem: TXMLDataBindingItem): Boolean;
  public
    property Hints: IXMLDataBindingHints  read FHints write FHints;
  end;


{$R *.dfm}


{ TMainForm }
procedure TMainForm.FormCreate(Sender: TObject);
var
  schemaFile:   String;

begin
  plOutput.ActivePageIndex := 0;

  if ParamCount > 0 then
  begin
    schemaFile  := ParamStr(1);

    if FileExists(schemaFile) then
    begin
      feFile.Text     := ChangeFileExt(schemaFile, '.pas');
      deFolder.Text   := ExtractFilePath(schemaFile);

      { Set schema last, the Change event will attempt to load the
        settings file and overwrite the file / folder. }
      feSchema.Text   := schemaFile;
    end;
  end;
end;


procedure TMainForm.OutputTypeClick(Sender: TObject);
begin
  if Sender = rbFile then
    plOutput.ActivePage := spFile
  else if Sender = rbFolder then
    plOutput.ActivePage := spFolder;
end;


procedure TMainForm.btnGenerateClick(Sender: TObject);
var
  hintsFile:      String;
  hints:          IXMLDataBindingHints;
  generator:      THintsDelphiXMLDataBindingGenerator;

begin
  if not CheckValidSchemaFile then
    Exit;

  hintsFile := ChangeFileExt(feSchema.Text, '.hints.xml');
  if FileExists(hintsFile) then
    hints := LoadDataBindingHints(hintsFile);

  try
    generator := THintsDelphiXMLDataBindingGenerator.Create;
    try
      generator.Hints := hints;

      if rbFile.Checked then
      begin
        if not CheckReadOnly(feFile.Text) then
          Exit;

        generator.OutputType  := otSingle;
        generator.OutputPath  := feFile.Text;
      end else if rbFolder.Checked then
      begin
        generator.OutputType  := otMultiple;
        generator.OutputPath  := deFolder.Text;
      end;

      generator.HasChecksEmpty := cbHasChecksEmpty.Checked;
      generator.HasGenerateGetOptionalOrDefault := cbGenerateGetOptionalOrDefault.Checked;
      generator.OnGetFileName := GetFileName;
      generator.Execute(feSchema.Text);

      SaveSettings(feSchema.Text);

      ShowMessage('The data binding has been generated.');
    finally
      FreeAndNil(generator);
    end;
  finally
    hints := nil;
  end;
end;


procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TMainForm.GetFileName(Sender: TObject; const SchemaName: String; var Path, FileName: String);
begin
  FileName  := ChangeFileExt(edtFolderPrefix.Text + FileName,
                             edtFolderPostfix.Text + ExtractFileExt(FileName));
  CheckValidFileName(FileName);
end;


procedure TMainForm.feFilePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if dlgOutputFile.Execute then
    feFile.Text := dlgOutputFile.FileName;
end;


procedure TMainForm.deFolderPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  directory:  String;

begin
  if SelectDirectory('Select output folder', '', directory) then
    deFolder.Text := directory;
end;


procedure TMainForm.feSchemaPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
begin
  if dlgSchema.Execute then
    feSchema.Text := dlgSchema.FileName;
end;


procedure TMainForm.feSchemaPropertiesChange(Sender: TObject);
begin
  if FileExists(feSchema.Text) then
    LoadSettings(feSchema.Text);
end;


function TMainForm.GetSettingsFileName(const AFileName: String): String;
begin
  Result := ChangeFileExt(AFileName, '.settings.xml');
end;


procedure TMainForm.LoadSettings(const AFileName: String);
var
  fileName:       String;
  settings:       IXMLDataBindingSettings;
  outputSingle:   IXMLOutputSingle;
  outputMultiple: IXMLOutputMultiple;

begin
  fileName  := GetSettingsFileName(AFileName);
  if FileExists(fileName) then
  begin
    settings  := LoadDataBindingSettings(fileName);

    if settings.HasOutput then
    begin
      case settings.Output.OutputType of
        DataBindingOutputType_Single:
          begin
            outputSingle    := settings.Output.OutputSingle;
            rbFile.Checked  := True;
            feFile.Text     := outputSingle.FileName;
          end;

        DataBindingOutputType_Multiple:
          begin
            outputMultiple        := settings.Output.OutputMultiple;
            rbFolder.Checked      := True;
            deFolder.Text         := outputMultiple.Path;
            edtFolderPrefix.Text  := outputMultiple.Prefix;
            edtFolderPostfix.Text := outputMultiple.Postfix;
          end;
      end;

      cbHasChecksEmpty.Checked := settings.Output.HasHasChecksEmpty and settings.Output.HasChecksEmpty;
      cbGenerateGetOptionalOrDefault.Checked := settings.Output.HasGenerateGetOptionalOrDefault and settings.Output.GenerateGetOptionalOrDefault;
    end;
  end;
end;


procedure TMainForm.SaveSettings(const AFileName: String);
var
  fileName:       String;
  settings:       IXMLDataBindingSettings;
  outputSingle:   IXMLOutputSingle;
  outputMultiple: IXMLOutputMultiple;

begin
  fileName  := GetSettingsFileName(AFileName);
  if FileExists(fileName) then
    settings  := LoadDataBindingSettings(fileName)
  else
    settings  := NewDataBindingSettings;

  settings.Output.ChildNodes.Clear;

  if rbFile.Checked then
  begin
    settings.Output.OutputType  := DataBindingOutputType_Single;
    outputSingle                := settings.Output.OutputSingle;
    outputSingle.FileName       := feFile.Text;
  end else
  begin
    settings.Output.OutputType  := DataBindingOutputType_Multiple;
    outputMultiple              := settings.Output.OutputMultiple;
    outputMultiple.Path         := deFolder.Text;
    outputMultiple.Prefix       := edtFolderPrefix.Text;
    outputMultiple.Postfix      := edtFolderPostfix.Text;
  end;

  settings.Output.HasChecksEmpty := cbHasChecksEmpty.Checked;
  settings.Output.GenerateGetOptionalOrDefault := cbGenerateGetOptionalOrDefault.Checked;
  settings.OwnerDocument.SaveToFile(fileName);
end;


function TMainForm.CheckValidSchemaFile: Boolean;
begin
  Result := FileExists(feSchema.Text);

  if not Result then
  begin
    MessageBox(Self.Handle, 'Please specify a valid schema file.',
               'Schema file does not exist', MB_OK or MB_ICONERROR);

    ActiveControl := feFile;
    Exit;
  end;
end;


function TMainForm.CheckReadOnly(const AFileName: String): Boolean;
begin
  Result := True;

  if FileExists(AFileName) and FileIsReadOnly(AFileName) then
  begin
    if MessageBox(Self.Handle, 'The output file is read-only. Do you want to ' +
                               'remove the read-only attribute?',
                               'Read-only', MB_YESNO or MB_ICONQUESTION) = ID_YES then
    begin
      Result := FileSetReadOnly(AFileName, False);
    end else
      Result := False;
  end;
end;


procedure TMainForm.btnHintsClick(Sender: TObject);
var
  hintsFile:  String;
  hints:      IXMLDataBindingHints;

begin
  if CheckValidSchemaFile then
  begin
    hintsFile := ChangeFileExt(feSchema.Text, '.hints.xml');
    if FileExists(hintsFile) then
    begin
      if MessageBox(Self.Handle, 'Do you want to overwrite the existing hints file?',
                    'Overwrite', MB_YESNO or MB_ICONQUESTION) <> ID_YES then
        Exit;
    end;

    hints := NewDataBindingHints;
    hints.OwnerDocument.SaveToFile(hintsFile);
    ShowMessage('The hints file has been generated.');
  end;
end;


{ THintsDelphiXMLDataBindingGenerator }
procedure THintsDelphiXMLDataBindingGenerator.GenerateDataBinding;
begin
  if Assigned(Hints) then
    ProcessHints;

  inherited GenerateDataBinding;
end;


procedure THintsDelphiXMLDataBindingGenerator.ProcessHints;
begin
  if Hints.HasEnumerations then
    ProcessEnumerations;

  if Hints.HasDocumentElements then
    ProcessDocumentElements;

  if Hints.HasInterfaces then
    ProcessInterfaces;

  if Hints.HasProperties then
    ProcessProperties;
end;


procedure THintsDelphiXMLDataBindingGenerator.ProcessEnumerations;

  procedure ProcessEnumeration(ABindingEnumeration: TXMLDataBindingEnumeration; AHintEnumeration: IXMLEnumeration);
  var
    hintMemberIndex:  Integer;
    memberName:       String;
    memberIndex:      Integer;

  begin
    for hintMemberIndex := 0 to Pred(AHintEnumeration.Count) do
    begin
      memberName  := AHintEnumeration.Member[hintMemberIndex].Name;

      for memberIndex := 0 to Pred(ABindingEnumeration.MemberCount) do
      begin
        if ABindingEnumeration.Members[memberIndex].Name = memberName then
        begin
          ABindingEnumeration.Members[memberIndex].TranslatedName := AHintEnumeration[hintMemberIndex].Text;
          Break;
        end;
      end;
    end;
  end;


  function GetNewMembers(ABindingEnumeration: TXMLDataBindingEnumeration; AHintEnumeration: IXMLEnumeration): TList<TXMLDataBindingEnumerationMember>;
  var
    hintMemberIndex:  Integer;
    member:           TXMLDataBindingEnumerationMember;

  begin
    Result := TList<TXMLDataBindingEnumerationMember>.Create;

    for hintMemberIndex := 0 to Pred(AHintEnumeration.Count) do
    begin
      member := TXMLDataBindingEnumerationMember.Create(Self, ABindingEnumeration, AHintEnumeration[hintMemberIndex].Name);
      member.TranslatedName := AHintEnumeration[hintMemberIndex].Text;
      Result.Add(member);
    end;
  end;


var
  itemIndex:        Integer;
  enumeration:      IXMLEnumeration;
  schemaItem:       TXMLDataBindingItem;
  enumerationItem:  TXMLDataBindingEnumeration;
  propertyItem:     TXMLDataBindingSimpleProperty;
  newMembers:       TList<TXMLDataBindingEnumerationMember>;
  newPropertyItem:  TXMLDataBindingItemProperty;

begin
  for itemIndex := 0 to Pred(Hints.Enumerations.Count) do
  begin
    enumeration := Hints.Enumerations[itemIndex];

    if FindNode(enumeration.Schema, enumeration.XPath, schemaItem) then
    begin
      case schemaItem.ItemType of
        itEnumeration:
          begin
            enumerationItem := TXMLDataBindingEnumeration(schemaItem);

            if enumeration.HasReplaceMembers and enumeration.ReplaceMembers then
            begin
              newMembers := GetNewMembers(enumerationItem, enumeration);
              try
                enumerationItem.ReplaceMembers(newMembers);
              finally
                FreeAndNil(newMembers);
              end;
            end else
              ProcessEnumeration(TXMLDataBindingEnumeration(schemaItem), enumeration);
          end;

        itProperty:
          if TXMLDataBindingProperty(schemaItem).PropertyType = ptSimple then
          begin
            propertyItem := TXMLDataBindingSimpleProperty(schemaItem);
            if propertyItem.DataType.Name = 'string' then
            begin
              enumerationItem := TXMLDataBindingEnumeration.Create(Self, schemaItem.SchemaItem, nil, schemaItem.Name);
              newPropertyItem := TXMLDataBindingItemProperty.Create(Self, propertyItem.SchemaItem, propertyItem.Name, enumerationItem);

              newMembers := GetNewMembers(enumerationItem, enumeration);
              try
                enumerationItem.ReplaceMembers(newMembers);
                ReplaceItem(schemaItem, newPropertyItem, False);
              finally
                FreeAndNil(newMembers);
              end;
            end;
          end;
      end;
    end;
  end;
end;


procedure THintsDelphiXMLDataBindingGenerator.ProcessDocumentElements;
var
  schemaIndex:      Integer;
  schema:           TXMLDataBindingSchema;
  itemIndex:        Integer;
  documentElement:  IXMLDocumentElement;
  schemaItem:       TXMLDataBindingItem;

begin
  for schemaIndex := 0 to Pred(SchemaCount) do
  begin
    schema  := Schemas[schemaIndex];

    for itemIndex := 0 to Pred(schema.ItemCount) do
      schema.Items[itemIndex].DocumentElement := False;
  end;

  for itemIndex := 0 to Pred(Hints.DocumentElements.Count) do
  begin
    documentElement := Hints.DocumentElements[itemIndex];

    if FindNode(documentElement.Schema, documentElement.XPath, schemaItem) then
    begin
      if schemaItem.ItemType = itInterface then
        schemaItem.DocumentElement  := True;
    end;
  end;
end;


procedure THintsDelphiXMLDataBindingGenerator.ProcessInterfaces;
var
  itemIndex:        Integer;
  interfaceName:    IXMLInterfaceName;
  schemaItem:       TXMLDataBindingItem;
  propertyItem: TXMLDataBindingProperty;

begin
  for itemIndex := 0 to Pred(Hints.Interfaces.Count) do
  begin
    interfaceName := Hints.Interfaces[itemIndex];

    if FindNode(interfaceName.Schema, interfaceName.XPath, schemaItem) then
    begin
      case schemaItem.ItemType of
        itInterface,
        itEnumeration:
          schemaItem.TranslatedName := interfaceName.Text;

        itProperty:
          begin
            propertyItem := TXMLDataBindingProperty(schemaItem);
            if propertyItem.PropertyType = ptItem then
              TXMLDataBindingItemProperty(propertyItem).Item.TranslatedName := interfaceName.Text;
          end;
      end;
    end;
  end;
end;


procedure THintsDelphiXMLDataBindingGenerator.ProcessProperties;
var
  itemIndex:    Integer;
  propertyName: IXMLPropertyName;
  schemaItem:   TXMLDataBindingItem;

begin
  for itemIndex := 0 to Pred(Hints.Properties.Count) do
  begin
    propertyName := Hints.Properties[itemIndex];

    if FindNode(propertyName.Schema, propertyName.XPath, schemaItem) then
    begin
      if schemaItem.ItemType = itProperty then
        schemaItem.TranslatedName := propertyName.Text;
    end;
  end;
end;


function THintsDelphiXMLDataBindingGenerator.FindSchema(const ASchemaName: String; out ASchema: TXMLDataBindingSchema): Boolean;
var
  schemaIndex:  Integer;

begin
  Result := False;

  if SchemaCount > 0 then
  begin
    if Length(ASchemaName) = 0 then
    begin
      ASchema := Schemas[0];
      Result  := True;
    end else
    begin
      for schemaIndex := 0 to Pred(SchemaCount) do
        if SameText(Schemas[schemaIndex].SchemaName, ASchemaName) then
        begin
          ASchema := Schemas[schemaIndex];
          Result  := True;
          Break;
        end;
    end;
  end;
end;


function THintsDelphiXMLDataBindingGenerator.FindNode(const ASchemaName, AXPath: String; out AItem: TXMLDataBindingItem): Boolean;

  function SameNode(ANode1, ANode2: IDOMNode): Boolean;
  var
    attributeIndex: Integer;
    attribute1:     IDOMNode;
    attribute2:     IDOMNode;
    hasParent1:     Boolean;
    hasParent2:     Boolean;

  begin
    { Compare name and number of attributes }
    Result := (ANode1.nodeName = ANode2.nodeName) and
              (ANode1.attributes.length = ANode2.attributes.length);

    if Result then
    begin
      { Compare attribute content }
      for attributeIndex := 0 to Pred(ANode1.attributes.length) do
      begin
        attribute1  := ANode1.attributes[attributeIndex];
        attribute2  := ANode2.attributes[attributeIndex];

        Result  := (attribute1.nodeName = attribute2.nodeName) and
                   (attribute1.nodeValue = attribute2.nodeValue);

        if not Result then
          Break;
      end;

      if Result then
      begin
        { Compare parent nodes }
        hasParent1  := Assigned(ANode1.parentNode) and (ANode1.parentNode.nodeType <> NODE_DOCUMENT);
        hasParent2  := Assigned(ANode2.parentNode) and (ANode2.parentNode.nodeType <> NODE_DOCUMENT);

        if hasParent1 = hasParent2 then
        begin
          if hasParent1 then
            Result := SameNode(ANode1.parentNode, ANode2.parentNode);
        end else
          Result := False;
      end;
    end;
  end;


var
  schema:         TXMLDataBindingSchema;
  schemaItem:     IDOMNode;
  item:           TProtectedXMLDataBindingItem;
  itemIndex:      Integer;
  domDocument:    IXMLDOMDocument2;
  interfaceItem:  TXMLDataBindingInterface;
  propertyIndex:  Integer;
  propertyItem:   TProtectedXMLDataBindingProperty;

begin
  Result  := False;

  if (Length(AXPath) > 0) and FindSchema(ASchemaName, schema) then
  begin
    domDocument := ((schema.SchemaDef.OwnerDocument.DOMDocument as IXMLDOMNodeRef).GetXMLDOMNode as IXMLDOMDocument2);
    domDocument.setProperty('SelectionLanguage', 'XPath');
    domDocument.setProperty('SelectionNamespaces', 'xmlns:xs="http://www.w3.org/2001/XMLSchema"');

    schemaItem  := (schema.SchemaDef.DOMNode as IDOMNodeSelect).selectNode(AXPath);

    if Assigned(schemaItem) then
    begin
      for itemIndex := 0 to Pred(schema.ItemCount) do
      begin
        item  := TProtectedXMLDataBindingItem(schema.Items[itemIndex]);

        if Assigned(item.SchemaItem) and SameNode(item.SchemaItem.DOMNode, schemaItem) then
        begin
          AItem   := schema.Items[itemIndex];
          Result  := True;
        end else if item.ItemType = itInterface then
        begin
          interfaceItem := TXMLDataBindingInterface(item);

          for propertyIndex := 0 to Pred(interfaceItem.PropertyCount) do
          begin
            propertyItem  := TProtectedXMLDataBindingProperty(interfaceItem.Properties[propertyIndex]);

            if Assigned(propertyItem.SchemaItem) and SameNode(propertyItem.SchemaItem.DOMNode, schemaItem) then
            begin
              AItem   := propertyItem;
              Result  := True;
              Break;
            end;
          end;
        end;

        if Result then
          Break;
      end;
    end;
  end;
end;

end.

