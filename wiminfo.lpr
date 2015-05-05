(*
 * Chhom Seng
 * 2015/05/03
 *
 * WIMInfo
 *
 * Command-line tool to modify custom XML information in a WIM file. This tool
 * supports embed, extract, and remove.
 *
 * Embed: The user can specify an XML that is to be embedded into the WIM. An
 *        optional image index parameter can be given to indicate which image
 *        should be modified.
 *
 * Extract: The user can use an XPath expression to indicate information
 *          to be extracted from the WIM file. When extracting, the output is
 *          written to standard out.
 *
 * Remove: The user can use an XPath expression to indicate nodes to remove
 *         from the WIM file.
 *
 * The XPath expression must be in absolute addressing.
 *
 * TODO: Implement WIMContainer class to handle common WIM tasks.
 *)

program wiminfo;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  wimgapi,
  Windows,
  Dialogs,
  DOM,
  XMLRead,
  XMLWrite,
  XPath,
  XMLUtils;

const
  { The WIMGet/WIMSet information functions work with a string buffer that is
    preceded by $FEFF. Without this marker, both functions fail.               }
  WIM_INFO_MARKER = widechar($FEFF);

type
  { Handles WIM files. Currently unimplemented }
  WIMContainer = class(TObject)
    FHandle: THandle;
    FFilePath: string;
    FDesiredAccess, FDisposition, FFlagsAndAttributes, FCompressionType: DWORD;
  public
    { Return value of WIMCreateFile. 0 is an invalid value }
    property Handle: THandle read FHandle;

    {property FilePath: string read FFilePath write SetFilePath; }
  end;

  { TWIMInfo }

  TWIMInfo = class(TCustomApplication)
  protected
    WIM: WIMContainer;
    WIMFile, XMLFile: string;
    Index: integer;

    procedure DoRun; override;

    { Copies an XML document into the specified image }
    procedure Embed(XMLPath: string; ImageIndex: integer = 0);

    { Extracts the specified node (using XPath) }
    procedure Extract(XPathExp: string = '');

    { Removes the specified node (using XPath) }
    procedure Remove(XPathExp: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TWIMInfo }

  procedure TWIMInfo.DoRun;
  var
    ErrorMsg, XPathExp: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('hiexr', ['help', 'index', 'embed', 'extract', 'remove']);
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    { The first parameter must always be a WIM file }
    WIMFile := GetParams(1);

    if not FileExists(WIMFile) then
    begin
      ShowException(Exception.Create('The given WIM file was not found.'));
      Terminate;
      Exit;
    end
    else
    begin
      { Remember to initialize the WIMGAPI unit }
      InitWIMGAPI();

      //WIM := WIMContainer.Create(WIMFile);

      if ParamCount = 1 then
      begin
        Extract('//WIM');
      end;
      if HasOption('e', 'embed') then
      begin
        XMLFile := GetOptionValue('e', 'embed');
        if HasOption('i', 'index') then
          Index := StrToInt(GetOptionValue('i', 'index'))
        else
          Index := 0;
        Embed(XMLFile, Index);
      end
      else if HasOption('x', 'extract') then
      begin
        XPathExp := GetOptionValue('x', 'extract');
        Extract(XPathExp);
      end
      else if HasOption('r', 'remove') then
      begin
        XPathExp := GetOptionValue('r', 'remove');
        Remove(XPathExp);
      end
      else
      begin
        { Default handling }
      end;
    end;

    // stop program loop
    Terminate;
  end;

  { XMLPath is the path to an XML file }
  { ImageIndex is the specified image. Indices start at 1 }
  procedure TWIMInfo.Embed(XMLPath: string = ''; ImageIndex: integer = 0);
  var
    { WIMHandle is the handle to the WIM file
      InfoHandle is the handle to use for retrieving information               }
    WIMHandle, InfoHandle: THandle;
    OperationResult: boolean;
    OldInfoBuffer, NewInfoBuffer: PWideChar;
    BufferSize, dwResult: DWord;

    // Temporary directory needed for WIM modification
    TempDir: string;

    XMLDataString: TStringStream;
    XMLDoc, ImageXML: TXMLDocument;
    NewNode, ExistingNode: TDOMNode;
  begin
    if not FileExists(XMLPath) then
    begin
      raise Exception.Create('The given XML was not found');
    end;

    WIMHandle := 0;
    InfoHandle := 0;
    OldInfoBuffer := nil;
    NewInfoBuffer := nil;
    BufferSize := 0;
    dwResult := 0;
    XMLDataString := nil;
    XMLDoc := nil;
    ImageXML := nil;
    NewNode := nil;
    ExistingNode := nil;

    ReadXMLFile(XMLDoc, XMLPath);

    if XMLDoc = nil then
      raise Exception.Create('Failed to initialize XML');

    WIMHandle := WIMCreateFile(StringToOleStr(WIMFile), WIM_GENERIC_WRITE,
      WIM_OPEN_EXISTING, WIM_FLAG_VERIFY, 0, @dwResult);

    if WIMHandle = 0 then
      raise Exception.Create('Failed to open the WIM');

    { Must always set temporary path or modifications will fail }
    TempDir := WIMFile + '_temp';
    if not DirectoryExists(TempDir) then
      CreateDir(Tempdir);

    OperationResult := WIMSetTemporaryPath(WIMHandle, StringToOleStr(TempDir));

    if not OperationResult = True then
      raise Exception.Create('Failed to set temporary path with error: ' +
        IntToStr(GetLastError()));

    if (ImageIndex > 0) and (ImageIndex <= WIMGetImageCount(WIMHandle)) then
    begin
      InfoHandle := WIMLoadImage(WIMHandle, ImageIndex);
    end
    else
    begin
      InfoHandle := WIMHandle;
    end;

    OperationResult := WIMGetImageInformation(InfoHandle, @OldInfoBuffer, @BufferSize);

    if not OperationResult = True then
      raise Exception.Create('Failed to get image information');

    { Skip the first character or XML creation fails }
    XMLDataString := TStringStream.Create(WideCharToString(@OldInfoBuffer[1]));
    ReadXMLFile(ImageXML, XMLDataString);
    XMLDataString.Free;

    NewNode := ImageXML.ImportNode(TDOMNode(XMLDoc.DocumentElement), True);

    { Determine whether the node already exists. If it does, replace it }
    ExistingNode := ImageXML.DocumentElement.FindNode(NewNode.NodeName);

    if Assigned(ExistingNode) then
    begin
      ImageXML.DocumentElement.ReplaceChild(NewNode, ExistingNode);
    end
    else
    begin
      ImageXML.DocumentElement.AppendChild(NewNode);
    end;

    XMLDataString := TStringStream.Create('');
    WriteXML(ImageXML.DocumentElement, XMLDataString);

    NewInfoBuffer := AllocMem(Length(XMLDataString.DataString) * SizeOf(PWideChar));

    { Skip over the first element (the Header); don't overwrite it or WIMSetImageInformation will fail }
    NewInfoBuffer^ := WIM_INFO_MARKER;
    StrCopy(@NewInfoBuffer[1], StringToOleStr(XMLDataString.DataString));

    OperationResult := WIMSetImageInformation(InfoHandle, NewInfoBuffer,
      StrLen(NewInfoBuffer) * SizeOf(NewInfoBuffer));

    if not OperationResult = True then
      raise Exception.Create('Failed to set new information');

    Freememory(NewInfoBuffer);

    if not (OldInfoBuffer = nil) then
      LocalFree(HLOCAL(OldInfoBuffer));

    ExistingNode.Free;
    NewNode.Free;

    XMLDataString.Free;
    ImageXML.Free;
    XMLDoc.Free;

    { Will need to check whether WIMHandle are the same because of the current
      implementation }
    if WIMHandle = InfoHandle then
    begin
      OperationResult := WIMCloseHandle(WIMHandle);
    end
    else
    begin
      if InfoHandle <> 0 then
        WIMCloseHandle(InfoHandle);

      if WIMHandle <> 0 then
        OperationResult := WIMCloseHandle(WIMHandle);
    end;

    if DirectoryExists(TempDir) then
      RemoveDir(TempDir);

    if not OperationResult = True then
      raise Exception.Create('Failed to close WIM with error: ' +
        IntToStr(GetLastError()));
  end;

  procedure TWIMInfo.Extract(XPathExp: string);
  var
    Handle: THandle;
    InfoBuffer: PWideChar;
    OperationResult: boolean;
    dwResult, BufferSize: DWord;

    XML, OutputXML: TXMLDocument;
    TempNode: TDOMNode;
    NodePtr: Pointer;
    XMLString: TStringStream;
    TempDir: string;
    FoundNodes: TXPathVariable;

  begin
    if not FileExists(WIMFile) then
    begin
      raise Exception.Create('The specified WIM file was not found');
    end;

    Handle := 0;
    InfoBuffer := nil;
    OperationResult := False;
    dwResult := 0;
    BufferSize := 0;
    XML := nil;

    Handle := WIMCreateFile(StringToOleStr(WIMFile), WIM_GENERIC_READ,
      WIM_OPEN_EXISTING, 0, 0, @dwResult);

    { dwResult indicates whether it opened new or existing }
    if Handle = 0 then
    begin
      raise Exception.Create('Unable to open the specified WIM with error: ' +
        IntToStr(GetLastError()));
    end;

    { Must always set temporary path or modifications will fail }
    TempDir := WIMFile + '_temp';
    if not DirectoryExists(TempDir) then
      CreateDir(Tempdir);

    OperationResult := WIMSetTemporaryPath(Handle, StringToOleStr(TempDir));

    if not OperationResult = True then
      raise Exception.Create('Failed to set temporary path with error: ' +
        IntToStr(GetLastError()));

    OperationResult := WIMGetImageInformation(Handle, @InfoBuffer, @BufferSize);

    if not OperationResult = True then
    begin
      raise Exception.Create('Failed to get the image information');
    end;

    { Need to skip the first character in the buffer or XML won't initialize }
    XMLString := TStringStream.Create(WideCharToString(@InfoBuffer[1]));
    ReadXMLFile(XML, XMLString);
    XMLString.Free;

    { The XPath expression should strip out the root node since it is interpreted incorrectly }
    if XPathExp = '' then
      XPathExp := '/';
    FoundNodes := EvaluateXPathExpression(XPathExp, XML.DocumentElement);

    if FoundNodes = nil then
    begin
      raise Exception.Create('Failed to evaluate the XPath expression');
    end;

    OutputXML := TXMLDocument.Create();

    for NodePtr in FoundNodes.AsNodeSet() do
    begin
      TempNode := TDOMNode(NodePtr);
      XMLString := TStringStream.Create('');
      WriteXML(TempNode, XMLString);
      Write(XMLString.DataString);
      XMLString.Free;
    end;

    FoundNodes.Free;

    XML.Free;
    OutputXML.Free;

    if DirectoryExists(TempDir) then
      RemoveDir(TempDir);

    if not (InfoBuffer = nil) then
    begin
      LocalFree(HLOCAL(InfoBuffer));
    end;

    OperationResult := WIMCloseHandle(Handle);

    if not OperationResult = True then
    begin
      raise Exception.Create('Failed to close the handle with error: ' +
        IntToStr(GetLastError()));
    end;
  end;

  procedure TWIMInfo.Remove(XPathExp: string);
  var
    Handle: THandle;
    OldInfoBuffer, NewInfoBuffer: PWideChar;
    BufferSize, dwResult: DWord;
    OperationResult: boolean;
    XMLDoc: TXMLDocument;
    XMLDataString: TStringStream;

    FoundNodes: TXPathVariable;
    TempNode: TDOMNode;
    TempPtr: Pointer;
    TempDir: string;
  begin
    Handle := 0;
    OldInfoBuffer := nil;
    BufferSize := 0;
    dwResult := 0;
    OperationResult := False;
    XMLDoc := nil;
    XMLDataString := nil;
    TempNode := nil;

    { Modify the WIM, so open write }
    Handle := WIMCreateFile(StringToOleStr(WIMFile), WIM_GENERIC_WRITE,
      WIM_OPEN_EXISTING, 0, 0, @dwResult);

    if Handle = 0 then
    begin
      raise Exception.Create('Failed to open the specified WIM with error: ' +
        IntToStr(GetLastError()));
    end;

    { Must always set temporary path or modifications will fail }
    TempDir := WIMFile + '_temp';
    if not DirectoryExists(TempDir) then
      CreateDir(Tempdir);

    OperationResult := WIMSetTemporaryPath(Handle, StringToOleStr(TempDir));

    if not OperationResult = True then
      raise Exception.Create('Failed to set temporary path with error: ' +
        IntToStr(GetLastError()));

    OperationResult := WIMGetImageInformation(Handle, @OldInfoBuffer, @dwResult);

    if OperationResult = False then
    begin
      raise Exception.Create('Failed to get image information with error: ' +
        IntToStr(GetLastError()));
    end;

    { $FEFF precedes OldInfoBuffer. Must skip it for successful XML initialization }
    XMLDataString := TStringStream.Create(WideCharToString(@OldInfoBuffer[1]));
    ReadXMLFile(XMLDoc, XMLDataString);
    XMLDataString.Free;

    if not Assigned(XMLDoc) then
    begin
      raise Exception.Create('Failed to create XML document');
    end;

    { Should ensure that XPath like //WIM/IMAGE are not accepted }
    FoundNodes := EvaluateXPathExpression(XPathExp, XMLDoc.DocumentElement);

    for TempPtr in FoundNodes.AsNodeSet do
    begin
      TempNode := TDOMNode(TempPtr);
      TempNode.ParentNode.RemoveChild(TempNode);
    end;

    XMLDataString := TStringStream.Create('');
    WriteXML(XMLDoc.DocumentElement, XMLDataString);

    NewInfoBuffer := AllocMem(Length(XMLDataString.DataString) * SizeOf(PWideChar));

    { Skip over the first element (the Header); don't overwrite it or WIMSetImageInformation will fail }
    NewInfoBuffer^ := WIM_INFO_MARKER;
    StrCopy(@NewInfoBuffer[1], StringToOleStr(XMLDataString.DataString));
    XMLDataString.Free;

    OperationResult := WIMSetImageInformation(Handle, NewInfoBuffer,
      StrLen(NewInfoBuffer) * SizeOf(NewInfoBuffer));

    if not OperationResult = True then
      raise Exception.Create('Failed to set new information with error: ' +
        IntToStr(GetLastError()));

    FoundNodes.Free;
    XMLDoc.Free;

    Freememory(NewInfoBuffer);

    if not (OldInfoBuffer = nil) then
      LocalFree(HLOCAL(OldInfoBuffer));

    if Handle <> 0 then
      WIMCloseHandle(Handle);

    if DirectoryExists(TempDir) then
      RemoveDir(TempDir);
  end;

  constructor TWIMInfo.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TWIMInfo.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TWIMInfo.WriteHelp;
  begin
    { add your help code here }
    WriteLn('Usage: ', 'wiminfo', ' -h');
    WriteLn('wiminfo', ' <path_to_wim_file>    Display the XML information in the WIM.');
    WriteLn;
    WriteLn('wiminfo',
      ' <path_to_wim_file> [-i index] [-e <path_to_xml_file>]    Embed the given XML file into the specified image.');
    WriteLn;
    WriteLn('wiminfo',
      ' <path_to_wim_file> [-i index] [-x <xpath>]    Display information with the specified XPath expression.');
    WriteLn;
    WriteLn('wiminfo',
      ' <path_to_wim_file> [-i index] [-r <xpath>]    Remove the information from the WIM with the XPath expression.');
    WriteLn;
    WriteLn('Press any key to continue...');
    ReadLn;
  end;

var
  Application: TWIMInfo;

{$R *.res}

begin
  Application := TWIMInfo.Create(nil);
  Application.Title := 'WIMInfo';
  Application.Run;
  Application.Free;
end.
