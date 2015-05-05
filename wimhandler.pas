unit WIMHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, WIMGAPI, Windows, DOM, XMLRead, XMLWrite,
  xmlutils, Dialogs;

const
  WIMAGERFIELDS = 'WIMAGERFIELDS';

type
  WIMContainer = class;
  WIMImage = class;
  WIMImageList = class;
  WIMImageField = class;

  { WIMContainer }
  WIMContainer = class(TObject)
    FFilePath, FTempPath: string;
    FOpened: boolean;
    FDesiredAccess, FDisposition, FFlagsAndAttributes, FCompressionType: DWORD;
    FXMLInfo: TXMLDocument;
    FImageCount: cardinal;

  private
    FHandle: THandle;
    FImages: WIMImageList;
    WIMImageNames: TStringList;

    procedure SetDesiredAccess(NewDesiredAccess: DWORD);
    procedure SetCompressionType(NewCompressionType: DWORD);
    procedure SetTempPath();
    procedure SetTempPath(NewTempPath: string);
    procedure SetInformation(NewInformation: TXMLDocument);

    function GetXMLInformation: string;
  public
    property Access: DWORD read FDesiredAccess write SetDesiredAccess;
    property Compression: DWORD read FCompressionType write SetCompressionType;
    property TemporaryPath: string read FTempPath write SetTempPath;
    property Handle: THandle read FHandle;
    property Info: string read GetXMLInformation;
    property Images: WIMImageList read FImages;
    property ImageNames: TStringList read WIMImageNames;
    property XMLInfo: TXMLDocument read FXMLInfo write SetInformation;

    property FilePath: string read FFilePath;
    property Opened: boolean read FOpened;

    function Open(): boolean;
    procedure Close();

    constructor Create();
    constructor Create(SourceFilePath: string);
    constructor Create(SourceFilePath: string; Overwrite: boolean);
    destructor Free();
  end;

  { WIMImage }
  WIMImage = class(TObject)
    FName, FDescription: string;
    FIndex, FSize: cardinal;
    FCreationDate, FModificationDate: SYSTEMTIME;
    FHandle: THandle;
    FXMLInfo, FXMLCustomInfo: TXMLDocument;
    FXMLCustomNode: TDOMNode;
    FFieldList: TStringList;
    FContainer: WIMContainer;
  private
    procedure SetImageName(NewImageName: string);
    procedure SetImageDescription(NewImageDescription: string);
    function GetCustomFields(): TStringList;
    function GetXMLCustomFields(): TDOMNodeList;
    procedure UpdateXMLInfo();

  public
    property Name: string read FName write SetImageName;
    property Description: string read FDescription write SetImageDescription;
    property Parent: WIMContainer read FContainer write FContainer;
    property CreationDate: SYSTEMTIME read FCreationDate;
    property ModificationDate: SYSTEMTIME read FModificationDate;
    property Size: cardinal read FSize;
    property CustomFields: TStringList read GetCustomFields;
    property XMLCustomFields: TDOMNodeList read GetXMLCustomFields;

    function GetField(FieldName: string): string;
    procedure SetField(FieldName, FieldValue: string);

    function GetCustomField(FieldName: string): string;
    procedure SetCustomField(FieldName, FieldValue: string; Index: integer = 0);
    procedure AddCustomField(FieldName, FieldValue: string);
    procedure RenameCustomField(OldFieldName, NewFieldName: string);
    procedure ClearCustomFields();

    procedure Apply(TargetDirectory: string);

    constructor Create();
    constructor Create(CapturedHandle: THandle);
    constructor Create(CapturedHandle: THandle; NewName, NewDescription: string);
    destructor Free();
  end;

  WIMImageList = class(TList)
  private
    function Get(Index: integer): WIMImage;
    procedure Put(Index: integer; NewImage: WIMImage);

  public
    property Items[Index: integer]: WIMImage read Get write Put; default;

    function Add(NewImage: WIMImage): integer;

    destructor Destroy(); override;
  end;

  { Used by WIMImage to implement custom fields }
  WIMImageField = class(THashTable)

  end;

implementation

constructor WIMContainer.Create();
begin
  FFilePath := '';
  FTempPath := '';
  FOpened := False;

  Self.FDesiredAccess := WIM_GENERIC_WRITE;
  Self.FDisposition := WIM_OPEN_EXISTING;
  Self.FFlagsAndAttributes := WIM_FLAG_VERIFY;
  Self.Compression := WIM_COMPRESS_NONE;
end;

constructor WIMContainer.Create(SourceFilePath: string);
begin
  FFilePath := SourceFilePath;
  FTempPath := FFilePath + '_temp';
  FOpened := False;
  FHandle := 0;

  Self.FDesiredAccess := WIM_GENERIC_WRITE;
  Self.FDisposition := WIM_OPEN_ALWAYS;
  Self.FFlagsAndAttributes := WIM_FLAG_VERIFY;
  Self.FCompressionType := WIM_COMPRESS_NONE;

  { Check whether the file exists }
  if not FileExists(FFilePath) then
  begin
    Self.FDisposition := WIM_CREATE_ALWAYS;
  end;
end;

{ This constructor allows overwriting of any existing WIM file specified
  by SourceFilePath }
constructor WIMContainer.Create(SourceFilePath: string; Overwrite: boolean);
begin
  FFilePath := SourceFilePath;
  FTempPath := FFilePath + '_temp';
  FOpened := False;

  Self.FDesiredAccess := WIM_GENERIC_WRITE;

  if Overwrite = True then
    Self.FDisposition := WIM_CREATE_ALWAYS
  else
    Self.FDisposition := WIM_OPEN_ALWAYS;

  Self.FCompressionType := WIM_COMPRESS_NONE;
  Self.FFlagsAndAttributes := WIM_FLAG_VERIFY;
end;

destructor WIMContainer.Free();
begin
  if FOpened = True then
    Self.Close;

  RemoveDir(TemporaryPath);
end;

function WIMContainer.Open(): boolean;
var
  OperationResult: boolean;
  LastError: DWORD;

  ImageInfoBuffer: PWideChar;
  dwResult, StructSize: DWORD;

  XMLDataString: TStringStream;
  ImageNodes: TDOMNodeList;
  ImageNode, NameNode: TDOMNode;
  i, ListLength: integer;
  n: string;

  ImageHandle: THandle;
  Image: WIMImage;
begin
  if FOpened = True then
  begin
    Result := True;
    Exit;
  end;

  if not FileExists(FFilePath) then
  begin
    FOpened := False;
  end;

  { Flags must be one of WIM_FLAG_VERIFY or WIM_FLAG_SHAREWRITE }
  FFlagsAndAttributes := WIM_FLAG_VERIFY;
  FHandle := WIMCreateFile(StringToOleStr(FFilePath), FDesiredAccess,
    FDisposition, FFlagsAndAttributes, FCompressionType, @dwResult);

  if FHandle = 0 then
  begin
    raise Exception.Create('In Open method, failed to create file with error: ' +
      IntToStr(GetLastError()));
    Result := False;
    Exit;
  end;

  SetTempPath();

  FImageCount := WIMGetImageCount(FHandle);

  StructSize := 0;

  OperationResult := WIMGetImageInformation(FHandle, @ImageInfoBuffer, @StructSize);

  if OperationResult = False then
  begin
    // throw error message
    LastError := GetLastError();  { Log the error message }
    FOpened := False;
    raise Exception.Create('Failed to get image information with error: ' +
      IntToStr(LastError));
    Exit;
  end;

  { The information returned from WIMGetImageInformation is UTF16. PWideChar
    can be used.
    Also, Length(ImageInfo) reports as half of StructSize. This is probably
    normal behavior, because of zero-padding.
    The $FEFF must precede the XML to be copied via WIMSetImageInformation. }

  { $FEFF precedes ImageInfoBuffer. Must skip it for successful XML initialization }
  XMLDataString := TStringStream.Create(WideCharToString(@ImageInfoBuffer[1]));
  ReadXMLFile(FXMLInfo, XMLDataString);

  { Initialize the WIMImageNames list. Though it seems straightforward to look for
    the 'NAME' tag, that tag may not always be added into the WIM info. In order
    to handle this scenario, look for the 'IMAGE' tag and for each one, check
    whether the node contains a 'NAME' tag. If it doesn't, add an empty string
    to the WIMImageNames list. }
  WIMImageNames := TStringList.Create();
  ImageNodes := FXMLInfo.GetElementsByTagName('IMAGE');

  ListLength := FImageCount;

  if not Assigned(FImages) then
  begin
    FImages := WIMImageList.Create();
    FImages.Capacity := FImageCount;
  end;

  if FImageCount > 0 then
  begin
    for i := 0 to FImageCount - 1 do
    begin
      ImageNode := ImageNodes.Item[i];
      NameNode := ImageNode.FindNode('NAME');
      if NameNode = nil then
        n := ''
      else
        n := NameNode.TextContent;

      WIMImageNames.Add(n);
      SetTempPath();

      try
        if OperationResult = False then
          LastError := GetLastError();
        ImageHandle := WIMLoadImage(FHandle, i + 1);
        if ImageHandle = 0 then
          LastError := GetLastError();
        Image := WIMImage.Create(ImageHandle);
        Image.FContainer := Self;
        FImages.Add(Image);
      finally
      end;
    end;
  end;

  if XMLDataString <> nil then
    XMLDataString.Free;

  if ImageInfoBuffer <> nil then
    LocalFree(HLOCAL(ImageInfoBuffer));

  FOpened := True;
  Result := FOpened;
end;

procedure WIMContainer.Close();
var
  OperationResult: boolean;
  LastError: DWORD;

  TempDataString: TStringStream;

begin
  if FOpened = True then
  begin

    if Assigned(WIMImageNames) then
      WIMImageNames.Free;

    if FImages <> nil then
    begin
      FImages.Destroy;
      FImages := nil;
    end;
    TempDataString := TStringStream.Create('');
    WriteXML(FXMLInfo, TempDataString);
    if FXMLInfo <> nil then
      FXMLInfo.Free;

    if FHandle <> 0 then
    begin
      OperationResult := WIMCloseHandle(FHandle);

      if OperationResult = False then
      begin
        raise Exception.Create('In Close, failed to close the WIM Container with error: '
          + IntToStr(GetLastError()));
      end;
    end;

    if OperationResult = False then
    begin
      LastError := GetLastError();
      raise Exception.Create('Failed to close WIMContainer with error ' +
        IntToStr(LastError));
    end;

    if DirectoryExists(TemporaryPath) then
      RemoveDir(TemporaryPath);
  end;

  FOpened := False;
end;

procedure WIMContainer.SetDesiredAccess(NewDesiredAccess: DWORD);
begin
  { Of the four types of access offered by WIMGAPI, WIM_OPEN_EXISTING is the least destructive so default to it }
  if (NewDesiredAccess <> WIM_GENERIC_READ) and (NewDesiredAccess <>
    WIM_GENERIC_WRITE) and (NewDesiredAccess <> WIM_GENERIC_MOUNT) then
    FDesiredAccess := WIM_GENERIC_READ
  else
    FDesiredAccess := NewDesiredAccess;
end;

function WIMContainer.GetXMLInformation: string;
var
  XMLText: TStringStream;
begin
  XMLText := TStringStream.Create('');
  WriteXML(FXMLInfo, XMLText);
  Result := XMLText.DataString;
  XMLText.Free;
end;

procedure WIMContainer.SetCompressionType(NewCompressionType: DWORD);
begin
  if (NewCompressionType <> WIM_COMPRESS_NONE) and
    (NewCompressionType <> WIM_COMPRESS_XPRESS) and
    (NewCompressionType <> WIM_COMPRESS_LZX) then
    FCompressionType := WIM_COMPRESS_NONE
  else
    FCompressionType := NewCompressionType;
end;

procedure WIMContainer.SetTempPath();
var
  OperationResult: boolean;
  LastError: DWORD;

  NewTempPath: string;
begin
  { For temporary directory creation, might actually want to create in the
    defined temp directories }
  NewTempPath := FFilePath + '_temp';
  if not DirectoryExists(NewTempPath) then
  begin
    OperationResult := CreateDir(NewTempPath);

    if OperationResult = False then
    begin
      raise Exception.Create('Failed to set a temporary directory for file: ' +
        FFilePath);
      Exit;
    end;
  end;

  OperationResult := WIMSetTemporaryPath(FHandle, StringToOleStr(NewTempPath));

  if OperationResult = False then
  begin
    LastError := GetLastError();
    raise Exception.Create('Failed to set the temporary path: ' + IntToStr(LastError));
  end;
end;

procedure WIMContainer.SetTempPath(NewTempPath: string);
begin
  FTempPath := NewTempPath;
  SetTempPath();
end;

procedure WIMContainer.SetInformation(NewInformation: TXMLDocument);
var
  OldInfoBuffer, NewInfoBuffer: PWideChar;
  BufferSize: DWORD;
  OperationResult: boolean;

  XMLDataString: TStringStream;

begin
  if not Assigned(NewInformation) then
  begin
    raise Exception.Create('NewInformation is not assigned');
  end;

  XMLDataString := TStringStream.Create('');
  WriteXML(NewInformation.DocumentElement, XMLDataString);

  { Clear out the old information }
  //XMLInfo.Free;
  ;

  ReadXMLFile(FXMLInfo, XMLDataString);

  NewInfoBuffer := AllocMem(Length(XMLDataString.DataString) * SizeOf(PWideChar));
  NewInfoBuffer^ := WIM_INFO_MARKER;
  StrCopy(@NewInfoBuffer[1], StringToOleStr(XMLDataString.DataString));
  OperationResult := WIMSetImageInformation(Handle, @NewInfoBuffer,
    StrLen(NewInfoBuffer) * SizeOf(PWideChar));

  if not OperationResult = True then
  begin
    raise Exception.Create('Failed to get information for the WIM file');
  end;

  Freememory(NewInfoBuffer);
  XMLDataString.Free;
end;

{ WIMImage class. This class represents an image within the WIM file. }
constructor WIMImage.Create();
begin
  FContainer := nil;

  FXMLInfo := nil;
  FXMLCustomInfo := nil;
  FXMLCustomNode := nil;
  FFieldList := nil;

  FName := '';
  FDescription := '';
  FHandle := 0;
  FIndex := 0;
  FSize := 0;
  ZeroMemory(@FCreationDate, SizeOf(FCreationDate));
  ZeroMemory(@FModificationDate, SizeOf(FModificationDate));

end;

constructor WIMImage.Create(CapturedHandle: THandle);
begin
  Self.Create(CapturedHandle, '', '');
end;

{ By default, WIMGAPI doesn't set Name and Description. These are useful
  information and ImageX also saves them. Therefore, it's best for our program
  to add them as well.

  Name and Description are image-specific; they are found only within the
  <IMAGE> tags.
  }
constructor WIMImage.Create(CapturedHandle: THandle; NewName, NewDescription: string);
var
  OperationResult: boolean;
  LastError: DWORD;

  ImageInfoBuffer: PWideChar;
  StructSize: DWORD;
  XMLText: string;

  TempFileTime: FILETIME;
  TempSystemTime: SYSTEMTIME;
  TempStr: string;

  TempCustomFields: TDOMNodeList;
  XMLDataString: TStringStream;
  Node, CustomNode, aNode: TDOMNode;

  rf: TReplaceFlags;
  i: integer;
begin
  FContainer := nil;

  FXMLInfo := nil;
  FXMLCustomInfo := nil;
  FXMLCustomNode := nil;
  FFieldList := nil;

  FName := '';
  FDescription := '';
  FHandle := 0;
  FIndex := 0;
  FSize := 0;
  ZeroMemory(@FCreationDate, SizeOf(FCreationDate));
  ZeroMemory(@FModificationDate, SizeOf(FModificationDate));

  FName := '';
  FDescription := '';

  FHandle := CapturedHandle;

  OperationResult := WIMGetImageInformation(FHandle, @ImageInfoBuffer, @StructSize);

  if OperationResult = False then
  begin
    LastError := GetLastError();
    raise Exception.Create('Failed to get image information with error ' +
      IntToStr(LastError));
  end;
  XMLDataString := TStringStream.Create(WideCharToString(@ImageInfoBuffer[1]));
  ReadXMLFile(FXMLInfo, XMLDataString);

  { DocumentElement is the root element. In this case, IMAGE }
  FIndex := StrToInt(FXMLInfo.DocumentElement.GetAttribute('INDEX'));

  ZeroMemory(@TempFileTime, SizeOf(TempFileTime));
  ZeroMemory(@TempSystemTime, SizeOf(TempSystemTime));

  rf := [rfIgnoreCase, rfReplaceAll];

  XMLText := FXMLInfo.DocumentElement.FindNode('CREATIONTIME').FindNode(
    'HIGHPART').TextContent;

  TempStr := StringReplace(XMLText, '0x', '$', rf);
  TempFileTime.dwHighDateTime := StrToInt64(TempStr);

  XMLText := FXMLInfo.DocumentElement.FindNode('CREATIONTIME').FindNode(
    'LOWPART').TextContent;

  TempStr := StringReplace(XMLText, '0x', '$', rf);
  TempFileTime.dwLowDateTime := StrToInt64(TempStr);

  FileTimeToSystemTime(TempFileTime, TempSystemTime);

  FCreationDate := TempSystemTime;

  ZeroMemory(@TempFileTime, SizeOf(TempFileTime));
  ZeroMemory(@TempSystemTime, SizeOf(TempSystemTime));

  XMLText := FXMLInfo.DocumentElement.FindNode('LASTMODIFICATIONTIME').FindNode(
    'HIGHPART').TextContent;

  TempStr := StringReplace(XMLText, '0x', '$', rf);
  TempFileTime.dwHighDateTime := StrToInt64(TempStr);

  XMLText := FXMLInfo.DocumentElement.FindNode('LASTMODIFICATIONTIME').FindNode(
    'LOWPART').TextContent;

  TempStr := StringReplace(XMLText, '0x', '$', rf);
  TempFileTime.dwLowDateTime := StrToInt64(TempStr);

  FileTimeToSystemTime(TempFileTime, TempSystemTime);

  FModificationDate := TempSystemTime;

  FSize := StrToInt(FXMLInfo.DocumentElement.FindNode('TOTALBYTES').TextContent);
  if Length(NewName) > 0 then
    Name := NewName
  else
  begin
    Node := FXMLInfo.DocumentElement.FindNode('NAME');
    if Node <> nil then
      FName := Node.TextContent;
  end;
  if Length(NewDescription) > 0 then
    Description := NewDescription
  else
  begin
    Node := FXMLInfo.DocumentElement.FindNode('DESCRIPTION');
    if Node <> nil then
      FDescription := Node.TextContent;
  end;

  { TODO: Generate the custom fields list }
  { NOTE: Moving forward, there should be one WIMAGERFIELDS node }
  { NOTE: All custom fields are subnodes of the WIMAGERFIELDS node }
  { Ensure there's only one WIMAGERFIELDS node. Merge together mulitple nodes }

  CustomNode := FXMLInfo.DocumentElement.FindNode(WIMAGERFIELDS);

  if Assigned(CustomNode) then
  begin
    if Assigned(CustomNode.ChildNodes) then
      TempCustomFields := CustomNode.ChildNodes;

    FFieldList := TStringList.Create();
    FFieldList.Capacity := TempCustomFields.Length;
    for i := 0 to TempCustomFields.Length do
    begin
      aNode := TempCustomFields.Item[i];
      if Assigned(aNode) then
      begin
        //FFieldList.Add(aNode.TextContent);
      end;
    end;
    FXMLCustomNode := CustomNode;
  end;

  if XMLDataString <> nil then
    XMLDataString.Free;
end;

procedure WIMImage.UpdateXMLInfo();
var
  OperationResult: boolean;
  LastError: DWORD;

  ImageInfoBuffer: PWideChar;
  StructSize: DWORD;

  XMLDataString: TStringStream;
begin
  OperationResult := False;
  LastError := 0;

  ImageInfoBuffer := nil;
  StructSize := 0;

  XMLDataString := nil;

  if Assigned(FXMLInfo) = True then
  begin
    FXMLInfo.Free;
  end;

  OperationResult := WIMGetImageInformation(FHandle, @ImageInfoBuffer, @StructSize);

  if OperationResult = False then
  begin
    LastError := GetLastError();
    raise Exception.Create('Failed to get image information with error ' +
      IntToStr(LastError));
  end;

  XMLDataString := TStringStream.Create(WideCharToString(@ImageInfoBuffer[1]));

  if Assigned(XMLDataString) = True then
  begin
    ReadXMLFile(FXMLInfo, XMLDataString);
    XMLDataString.Free;
  end
  else
    raise Exception.Create('Failed to update XML information');
end;

procedure WIMImage.Apply(TargetDirectory: string);
var
  TempBuffer: PWideChar;
  OperationResult: boolean;
  RegisterValue: DWORD;
begin
  TempBuffer := StringToOleStr(TargetDirectory);
  OperationResult := False;
  RegisterValue := 0;

  if RegisterValue = INVALID_CALLBACK_VALUE then
  begin
    raise Exception.Create('Failed to register callback with error ' +
      IntToStr(GetLastError()));
  end;

  OperationResult := WIMApplyImage(FHandle, TempBuffer, WIM_FLAG_VERIFY);

  if OperationResult = False then
  begin
    raise Exception.Create('Failed to apply the image with error: ' +
      IntToStr(GetLastError()));
  end;

  OperationResult := WIMUnregisterMessageCallback(Parent.Handle,
    Pointer(RegisterValue));

  if OperationResult = False then
  begin
    raise Exception.Create('Failed to unregister callback with error: ' +
      IntToStr(GetLastError()));
  end;
end;

destructor WIMImage.Free();
begin
  if Assigned(FFieldList) = True then
    FFieldList.Free;

  if Assigned(FXMLCustomInfo) = True then
    FXMLCustomInfo.Free;

  //if Assigned(FXMLCustomNode) = True then
  //  FXMLCustomNode.Free;

  if Assigned(FXMLInfo) = True then
    FXMLInfo.Free;

  if FHandle <> 0 then
    WIMCloseHandle(FHandle);
end;

procedure WIMImage.SetImageName(NewImageName: string);
begin
  SetField('NAME', NewImageName);
  FName := NewImageName;
end;

procedure WIMImage.SetImageDescription(NewImageDescription: string);
begin
  SetField('DESCRIPTION', NewImageDescription);
  FDescription := NewImageDescription;
end;

{ WIMager custom fields use the WIMAGERFIELDS node. This node contains other }
{ nodes, such as those created by the user. }
procedure WIMImage.SetField(FieldName, FieldValue: string);
var
  OperationResult: boolean;
  LastError: DWORD;
  ImageInfoBuffer, FinalInfoBuffer: PWideChar;
  StructSize: DWORD;

  XML: TXMLDocument;
  XMLDataString: TStringStream;
  ImageNode, FieldNode: TDOMNode;

  ReplaceFlag: TReplaceFlags;
begin
  OperationResult := False;
  LastError := 0;
  ImageInfoBuffer := nil;
  FinalInfoBuffer := nil;
  StructSize := 0;
  XML := nil;
  XMLDataString := nil;
  ImageNode := nil;
  FieldNode := nil;

  { The information returned from WIMGetImageInformation is UTF16.
    Some important things to remember: Pascal doesn't natively support UTF16.
    Therefore, it is necessary to do conversions.
    Also, Length(ImageInfo) reports as half of StructSize. This is probably
    normal behavior, because of zero-padding.
    The $FEFF must precede the XML to be copied via WIMSetImageInformation. }

  OperationResult := WIMGetImageInformation(FHandle, @ImageInfoBuffer, @StructSize);

  if OperationResult = False then
  begin
    LastError := GetLastError();
    raise Exception.Create('Failed to set temp path with error: ' +
      IntToStr(lastError));
  end;

  XMLDataString := TStringStream.Create(WideCharToString(@ImageInfoBuffer[1]));
  ReadXMLFile(XML, XMLDataString);

  ImageNode := XML.DocumentElement;

  { FieldName cannot have spaces }
  if Pos(' ', FieldName) > 0 then
  begin
    ReplaceFlag := [rfIgnoreCase, rfReplaceAll];
    FieldName := StringReplace(FieldName, ' ', '', ReplaceFlag);
  end;

  FieldNode := ImageNode.FindNode(FieldName);

  if FieldNode = nil then
    FieldNode := XML.CreateElement(FieldName);

  FieldNode.TextContent := FieldValue;

  ImageNode.AppendChild(FieldNode);

  XMLDataString.Free;
  XMLDataString := TStringStream.Create('');
  WriteXML(ImageNode, XMLDataString);

  FinalInfoBuffer := AllocMem(Length(XMLDataString.DataString) * SizeOf(PWideChar));

  { WIM requires the header, which is $FEFF }
  FinalInfoBuffer^ := WIM_INFO_MARKER;

  { Skip over the first element (the Header); don't overwrite it }
  StrCopy(@FinalInfoBuffer[1], StringToOleStr(XMLDataString.DataString));

  OperationResult := WIMSetImageInformation(FHandle, FinalInfoBuffer,
    StrLen(FinalInfoBuffer) * SizeOf(FinalInfoBuffer));

  if OperationResult = False then
  begin
    LastError := GetLastError();
    raise Exception.Create('Failed to set image information with error ' +
      IntToStr(LastError));
  end;

  if FieldNode <> nil then
    FieldNode.Free;

  if ImageNode <> nil then
    ImageNode.Free;

  if XML <> nil then
    XML.Free;

  if XMLDataString <> nil then
    XMLDataString.Free;

  Freememory(FinalInfoBuffer);

  if ImageInfoBuffer <> nil then
    LocalFree(HLOCAL(imageInfobuffer));

end;

function WIMImage.GetField(FieldName: string): string;
var
  FieldNode: TDOMNode;
  FieldValue: string;

  ReplaceFlag: TReplaceFlags;
begin
  FieldNode := nil;
  FieldValue := '';

  { FieldName cannot have spaces }
  if Pos(' ', FieldName) > 0 then
  begin
    ReplaceFlag := [rfIgnoreCase, rfReplaceAll];
    FieldName := StringReplace(FieldName, ' ', '', ReplaceFlag);
  end;

  FieldNode := FXMLInfo.DocumentElement.FindNode(FieldName);

  if FieldNode = nil then
    FieldValue := ''
  else
    FieldValue := FieldNode.TextContent;

  Result := FieldValue;
end;

{ When creating a custom field, WIMager's behavior is specified as creating
  a node named WIMAGERFIELD which encloses the custom fields. }
procedure WIMImage.AddCustomField(FieldName, FieldValue: string);
var
  OperationResult: boolean;
  LastError: DWORD;
  ImageInfoBuffer, FinalInfoBuffer: PWideChar;
  StructSize: DWORD;

  XML: TXMLDocument;
  XMLDataString: TStringStream;
  ImageNode, CustomNode, FieldNode: TDOMNode;

  ReplaceFlag: TReplaceFlags;
begin
  OperationResult := False;
  LastError := 0;
  ImageInfoBuffer := nil;
  FinalInfoBuffer := nil;
  StructSize := 0;
  XML := nil;
  XMLDataString := nil;
  ImageNode := nil;
  CustomNode := nil;
  FieldNode := nil;

  { The information returned from WIMGetImageInformation is UTF16.
    Some important things to remember: Pascal doesn't natively support UTF16.
    Therefore, it is necessary to do conversions.
    Also, Length(ImageInfo) reports as half of StructSize. This is probably
    normal behavior, because of zero-padding.
    The $FEFF must precede the XML to be copied via WIMSetImageInformation. }

  OperationResult := WIMGetImageInformation(FHandle, @ImageInfoBuffer, @StructSize);

  if OperationResult = False then
  begin
    LastError := GetLastError();
    raise Exception.Create('Failed to set temp path with error: ' + IntToStr(lastError));
  end;

  XMLDataString := TStringStream.Create(WideCharToString(@ImageInfoBuffer[1]));
  ReadXMLFile(XML, XMLDataString);

  ImageNode := XML.DocumentElement;

  CustomNode := ImageNode.FindNode(WIMAGERFIELDS);

  if Assigned(CustomNode) = False then
  begin
    CustomNode := XML.CreateElement(WIMAGERFIELDS);
    ImageNode.AppendChild(CustomNode);
  end;

  { FieldName cannot have spaces }
  if Pos(' ', FieldName) > 0 then
  begin
    ReplaceFlag := [rfIgnoreCase, rfReplaceAll];
    FieldName := UpperCase(StringReplace(FieldName, ' ', '', ReplaceFlag));
  end;

  FieldNode := ImageNode.FindNode(FieldName);

  if FieldNode = nil then
    FieldNode := XML.CreateElement(UpperCase(FieldName));

  FieldNode.TextContent := FieldValue;

  CustomNode.AppendChild(FieldNode);

  XMLDataString.Free;
  XMLDataString := TStringStream.Create('');
  WriteXML(ImageNode, XMLDataString);

  FinalInfoBuffer := AllocMem(Length(XMLDataString.DataString) * SizeOf(PWideChar));

  { WIM requires the header, which is $FEFF }
  FinalInfoBuffer^ := WIM_INFO_MARKER;

  { Skip over the first element (the Header); don't overwrite it }
  StrCopy(@FinalInfoBuffer[1], StringToOleStr(XMLDataString.DataString));

  OperationResult := WIMSetImageInformation(FHandle, FinalInfoBuffer,
    StrLen(FinalInfoBuffer) * SizeOf(FinalInfoBuffer));

  if OperationResult = False then
  begin
    LastError := GetLastError();
    raise Exception.Create('Failed to set custom field information with error ' +
      IntToStr(LastError));
  end;

  UpdateXMLInfo();

  if FieldNode <> nil then
    FieldNode.Free;

  if CustomNode <> nil then
    CustomNode.Free;

  if ImageNode <> nil then
    ImageNode.Free;

  if XML <> nil then
    XML.Free;

  if XMLDataString <> nil then
    XMLDataString.Free;

  Freememory(FinalInfoBuffer);

  if ImageInfoBuffer <> nil then
    LocalFree(HLOCAL(imageInfobuffer));
end;

{ Renaming a custom field requires the following: validating the new field name }
procedure WIMImage.RenameCustomField(OldFieldName, NewFieldName: string);
var
  OldNode, NewNode: TDOMNode;
begin
  OldNode := FXMLCustomInfo.FindNode(OldFieldName);

  if Assigned(OldNode) then
  begin
    NewNode := FXMLCustomInfo.CreateElement(NewFieldName);
    NewNode.TextContent := OldNode.TextContent;
    FXMLCustomInfo.ReplaceChild(NewNode, OldNode);
  end;
  raise Exception.Create('Implemented but not tested');

  { Apparently, this won't work because the XML info is not being modified }
  OldNode := FXMLCustomNode.FindNode(OldFieldName);

  if Assigned(OldNode) then
  begin
    NewNode := FXMLCustomNode.OwnerDocument.CreateElement(NewFieldName);
    NewNode.TextContent := OldNode.TextContent;
    FXMLCustomNode.ReplaceChild(NewNode, OldNode);
    //raise Exception.Create('Implemented but not tested');
  end;
end;

procedure WIMImage.ClearCustomFields;
var
  CustomFieldNode: TDOMNode;
  XML: TStringStream;
begin
  CustomFieldNode := FXMLInfo.DocumentElement.FindNode(WIMAGERFIELDS);

  if Assigned(CustomFieldNode) then
  begin
    FXMLInfo.DocumentElement.RemoveChild(CustomFieldNode);
  end;
  XML := TStringStream.Create('');
  WriteXML(FXMLInfo, XML);
  raise Exception.Create('Unimplemented');
end;

function WIMImage.GetCustomField(FieldName: string): string;
var
  FieldNode: TDOMNode;
  FieldValue: string;

  ReplaceFlag: TReplaceFlags;
begin
  FieldNode := nil;
  FieldValue := '';

  { FieldName cannot have spaces }
  if Pos(' ', FieldName) > 0 then
  begin
    ReplaceFlag := [rfIgnoreCase, rfReplaceAll];
    FieldName := StringReplace(FieldName, ' ', '', ReplaceFlag);
  end;

  if Assigned(FXMLInfo) = True then
  begin
    FXMLCustomNode := FXMLInfo.DocumentElement.FindNode(WIMAGERFIELDS);
  end;

  if Assigned(FXMLCustomNode) = True then
  begin
    FieldNode := FXMLCustomNode.FindNode(UpperCase(FieldName));
  end;

  if Assigned(FieldNode) = False then
    FieldValue := ''
  else
    FieldValue := FieldNode.TextContent;

  Result := FieldValue;
end;

procedure WIMImage.SetCustomField(FieldName, FieldValue: string; Index: integer = 0);
var
  OperationResult: boolean;
  LastError: DWORD;
  ImageInfoBuffer, FinalInfoBuffer: PWideChar;
  StructSize: DWORD;

  XML: TXMLDocument;
  XMLDataString: TStringStream;
  ImageNode, CustomFieldNode, FieldNode: TDOMNode;

  i: integer;
  ReplaceFlag: TReplaceFlags;
begin
  OperationResult := False;
  LastError := 0;
  ImageInfoBuffer := nil;
  FinalInfoBuffer := nil;
  StructSize := 0;
  XML := nil;
  XMLDataString := nil;
  ImageNode := nil;
  FieldNode := nil;

  { The information returned from WIMGetImageInformation is UTF16.
    Some important things to remember: Pascal doesn't natively support UTF16.
    Therefore, it is necessary to do conversions.
    Also, Length(ImageInfo) reports as half of StructSize. This is probably
    normal behavior, because of zero-padding.
    The $FEFF must precede the XML to be copied via WIMSetImageInformation. }

  OperationResult := WIMGetImageInformation(FHandle, @ImageInfoBuffer, @StructSize);

  if OperationResult = False then
  begin
    LastError := GetLastError();
    raise Exception.Create('Failed to set temp path with error: ' +
      IntToStr(lastError));
  end;

  XMLDataString := TStringStream.Create(WideCharToString(@ImageInfoBuffer[1]));
  ReadXMLFile(XML, XMLDataString);

  ImageNode := XML.DocumentElement;

  { FieldName cannot have spaces }
  if Pos(' ', FieldName) > 0 then
  begin
    ReplaceFlag := [rfIgnoreCase, rfReplaceAll];
    FieldName := StringReplace(FieldName, ' ', '', ReplaceFlag);
  end;

  { Must start with getting the custom field node }
  CustomFieldNode := ImageNode.FindNode(WIMAGERFIELDS);

  if not Assigned(CustomFieldNode) then
    raise Exception.Create('Failed to retrieve the custom field node');

  FieldNode := CustomFieldNode.FindNode(FieldName);

  if FieldNode = nil then
    FieldNode := XML.CreateElement(FieldName);

  FieldNode.TextContent := FieldValue;

  ImageNode.AppendChild(CustomFieldNode);
  CustomFieldNode.AppendChild(FieldNode);

  { Will want to add an index attribute to each of the custom fields }
  { Doing so will help to identify duplicate fields }
  { TODO: Find a way to optimize this bit }
  for i := 0 to CustomFieldNode.ChildNodes.Count - 1 do
  begin
    TDOMElement(CustomFieldNode.ChildNodes[i]).SetAttribute('Index', IntToStr(i));
  end;

  XMLDataString.Free;
  XMLDataString := TStringStream.Create('');
  WriteXML(ImageNode, XMLDataString);

  FinalInfoBuffer := AllocMem(Length(XMLDataString.DataString) * SizeOf(PWideChar));

  { WIM requires the header, which is $FEFF }
  FinalInfoBuffer^ := WIM_INFO_MARKER;

  { Skip over the first element (the Header); don't overwrite it }
  StrCopy(@FinalInfoBuffer[1], StringToOleStr(XMLDataString.DataString));

  OperationResult := WIMSetImageInformation(FHandle, FinalInfoBuffer,
    StrLen(FinalInfoBuffer) * SizeOf(FinalInfoBuffer));

  if OperationResult = False then
  begin
    LastError := GetLastError();
    raise Exception.Create('Failed to set image information with error ' +
      IntToStr(LastError));
  end;

  if FieldNode <> nil then
    FieldNode.Free;

  if ImageNode <> nil then
    ImageNode.Free;

  if XML <> nil then
    XML.Free;

  if XMLDataString <> nil then
    XMLDataString.Free;

  Freememory(FinalInfoBuffer);

  if ImageInfoBuffer <> nil then
    LocalFree(HLOCAL(imageInfobuffer));
end;

{ Returns the custom fields' names as a TStringList class }
{ NOTE: The returned item should always be fresh }
function WIMImage.GetCustomFields(): TStringList;
var
  i: integer;
  s: string;
begin
  if Assigned(FXMLCustomNode) = False then
  begin
    FXMLCustomNode := FXMLInfo.FindNode(WIMAGERFIELDS);
  end;

  if Assigned(FXMLCustomNode) = True then
  begin
    if Assigned(FFieldList) then
      FFieldList.Clear;

    for i := 0 to FXMLCustomNode.ChildNodes.Length - 1 do
    begin
      FFieldList.Add(FXMLCustomNode.ChildNodes.Item[i].NodeName);
    end;
  end;

  if Assigned(FFieldList) = True then
    for i := 0 to FFieldList.Count - 1 do
      s := FFieldList[i];

  Result := FFieldList;
end;

function WIMImage.GetXMLCustomFields(): TDOMNodeList;
var
  FieldName, FieldValue, Index: string;
begin
  if (Assigned(FXMLCustomNode) and Assigned(FXMLCustomNode.ChildNodes)) then
  begin
    Result := FXMLCustomNode.ChildNodes;
  end
  else
    Result := nil;
end;

function WIMImageList.Get(Index: integer): WIMImage;
var
  Image: WIMImage;
begin
  Result := WIMImage(inherited Items[Index]);
end;

procedure WIMImageList.Put(Index: integer; NewImage: WIMImage);
begin
  inherited Items[Index];
end;

function WIMImageList.Add(NewImage: WIMImage): integer;
begin
  Result := inherited Add(NewImage);
end;

destructor WIMImageList.Destroy();
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].Free();
  end;

  inherited;
end;

end.
