unit WIMGAPI;

interface

uses Windows;

const
   WIM_INFO_MARKER = WideChar($FEFF);

   GENERIC_EXECUTE = $20000000;

   { dwDesiredAccess }
   WIM_GENERIC_READ = GENERIC_READ;
   WIM_GENERIC_WRITE = GENERIC_WRITE;
   WIM_GENERIC_MOUNT = GENERIC_EXECUTE;

   { dwDisposition }
   WIM_CREATE_NEW = CREATE_NEW;
   WIM_CREATE_ALWAYS = CREATE_ALWAYS;
   WIM_OPEN_EXISTING = OPEN_EXISTING;
   WIM_OPEN_ALWAYS = OPEN_ALWAYS;

   WIM_COMPRESS_NONE = 0;
   WIM_COMPRESS_XPRESS = 1;
   WIM_COMPRESS_LZX = 2;

   WIM_CREATED_NEW = 0;
   WIM_OPENED_EXISTING = 1;


// WIMCreateFile, WIMCaptureImage, WIMApplyImage, WIMMountImageHandle flags:
   WIM_FLAG_RESERVED = $00000001;
   WIM_FLAG_VERIFY = $00000002;
   WIM_FLAG_INDEX = $00000004;
   WIM_FLAG_NO_APPLY = $00000008;
   WIM_FLAG_NO_DIRACL = $00000010;
   WIM_FLAG_NO_FILEACL = $00000020;
   WIM_FLAG_SHARE_WRITE = $00000040;
   WIM_FLAG_FILEINFO = $00000080;
   WIM_FLAG_NO_RP_FIX = $00000100;
   WIM_FLAG_MOUNT_READONLY = $00000200;

// WIMGetMountedImageList flags
   WIM_MOUNT_FLAG_MOUNTED = $00000001;
   WIM_MOUNT_FLAG_MOUNTING = $00000002;
   WIM_MOUNT_FLAG_REMOUNTABLE = $00000004;
   WIM_MOUNT_FLAG_INVALID = $00000008;
   WIM_MOUNT_FLAG_NO_WIM = $00000010;
   WIM_MOUNT_FLAG_NO_MOUNTDIR = $00000020;
   WIM_MOUNT_FLAG_MOUNTDIR_REPLACED = $00000040;
   WIM_MOUNT_FLAG_READWRITE = $00000100;

// WIMCommitImageHandle flags
   WIM_COMMIT_FLAG_APPEND = $00000001;

// WIMSetReferenceFile
   WIM_REFERENCE_APPEND = $00010000;
   WIM_REFERENCE_REPLACE = $00020000;

// WIMExportImage
   WIM_EXPORT_ALLOW_DUPLICATES = $00000001;
   WIM_EXPORT_ONLY_RESOURCES = $00000002;
   WIM_EXPORT_ONLY_METADATA = $00000004;

// WIMRegisterMessageCallback:
   INVALID_CALLBACK_VALUE = $FFFFFFFF;

// WIMCopyFile
   WIM_COPY_FILE_RETRY = $01000000;

// WIMDeleteImageMounts
   WIM_DELETE_MOUNTS_ALL = $00000001;

// WIMMessageCallback Notifications:
   WM_APP = $8000; //uses Messages
   WIM_MSG = WM_APP + $1476;
   WIM_MSG_TEXT = WIM_MSG + 1;
   WIM_MSG_PROGRESS = WIM_MSG + 2;
   WIM_MSG_PROCESS = WIM_MSG + 3;
   WIM_MSG_SCANNING = WIM_MSG + 4;
   WIM_MSG_SETRANGE = WIM_MSG + 5;
   WIM_MSG_SETPOS = WIM_MSG + 6;
   WIM_MSG_STEPIT = WIM_MSG + 7;
   WIM_MSG_COMPRESS = WIM_MSG + 8;
   WIM_MSG_ERROR = WIM_MSG + 9;
   WIM_MSG_ALIGNMENT = WIM_MSG + 10;
   WIM_MSG_RETRY = WIM_MSG + 11;
   WIM_MSG_SPLIT = WIM_MSG + 12;
   WIM_MSG_FILEINFO = WIM_MSG + 13;
   WIM_MSG_INFO = WIM_MSG + 14;
   WIM_MSG_WARNING = WIM_MSG + 15;
   WIM_MSG_CHK_PROCESS = WIM_MSG + 16;
   WIM_MSG_WARNING_OBJECTID = WIM_MSG + 17;
   WIM_MSG_STALE_MOUNT_DIR = WIM_MSG + 18;
   WIM_MSG_STALE_MOUNT_FILE = WIM_MSG + 19;
   WIM_MSG_MOUNT_CLEANUP_PROGRESS = WIM_MSG + 20;
   WIM_MSG_CLEANUP_SCANNING_DRIVE = WIM_MSG + 21;
   WIM_MSG_IMAGE_ALREADY_MOUNTED = WIM_MSG + 22;
   WIM_MSG_CLEANUP_UNMOUNTING_IMAGE = WIM_MSG + 23;
   WIM_MSG_QUERY_ABORT = WIM_MSG + 24;

// WIMMessageCallback Return codes:
   WIM_MSG_SUCCESS = ERROR_SUCCESS;
   WIM_MSG_DONE = $FFFFFFF0;
   WIM_MSG_SKIP_ERROR = $FFFFFFFE;
   WIM_MSG_ABORT_IMAGE = $FFFFFFFF;


// WIM_INFO dwFlags values:
   WIM_ATTRIBUTE_NORMAL = $00000000;
   WIM_ATTRIBUTE_RESOURCE_ONLY = $00000001;
   WIM_ATTRIBUTE_METADATA_ONLY = $00000002;
   WIM_ATTRIBUTE_VERIFY_DATA = $00000004;
   WIM_ATTRIBUTE_RP_FIX = $00000008;
   WIM_ATTRIBUTE_SPANNED = $00000010;
   WIM_ATTRIBUTE_READONLY = $00000020;

//MOUNTED_IMAGE_INFO_LEVELS
   MountedImageInfoLevel0 = 1;
   MountedImageInfoLevel1 = 2;
   MountedImageInfoLevelInvalid = 3;

type
   PLARGE_INTEGER = ^LARGE_INTEGER;

   _WIM_INFO = packed record
     WimPath : array[0..MAX_PATH - 1] of WideChar;
     Guid : TGUID;
     ImageCount : DWORD;
     CompressionType: DWORD;
     PartNumber : WORD;
     TotalParts : WORD;
     BootIndex : DWORD;
     WimAttributes : DWORD;
     WimFlagsAndAttr: DWORD;
   end;
   WIM_INFO = _WIM_INFO;
   LPWIM_INFO = ^WIM_INFO;
   PWIM_INFO = ^WIM_INFO;

   _WIM_MOUNT_LIST = packed record
     WimPath : array[0..MAX_PATH - 1] of WideChar;
     MountPath : array[0..MAX_PATH - 1] of WideChar;
     ImageIndex : DWORD;
     MountedForRW: BOOL;
   end;
   WIM_MOUNT_LIST = _WIM_MOUNT_LIST;
   LPWIM_MOUNT_LIST = ^WIM_MOUNT_LIST;
   PWIM_MOUNT_LIST = ^WIM_MOUNT_LIST;
   WIM_MOUNT_INFO_LEVEL0 = _WIM_MOUNT_LIST;
   PWIM_MOUNT_INFO_LEVEL0 = ^WIM_MOUNT_LIST;
   LPWIM_MOUNT_INFO_LEVEL0 = ^WIM_MOUNT_LIST;

   _WIM_MOUNT_INFO_LEVEL1 = packed record
     WimPath : array[0..MAX_PATH - 1] of WideChar;
     MountPath : array[0..MAX_PATH - 1] of WideChar;
     ImageIndex: DWORD;
     MountFlags: DWORD;
   end;
   WIM_MOUNT_INFO_LEVEL1 = _WIM_MOUNT_INFO_LEVEL1;
   PWIM_MOUNT_INFO_LEVEL1 = ^WIM_MOUNT_INFO_LEVEL1;
   LPWIM_MOUNT_INFO_LEVEL1 = ^WIM_MOUNT_INFO_LEVEL1;
   _MOUNTED_IMAGE_INFO_LEVELS = DWORD;
   MOUNTED_IMAGE_INFO_LEVELS = _MOUNTED_IMAGE_INFO_LEVELS;

type
 TWIMCreateFile = function(
   pzWimPath: PWideChar;
   dwDesiredAccess, dwCreationDisposition, dwFlagsAndAttributes, dwCompressionType: DWORD;
   pdwCreationResult: PDWORD
   ): THandle; stdcall;

 TWIMCloseHandle = function(
   hObject: THandle
   ): BOOL; stdcall;

 TWIMSetTemporaryPath = function(
   hWim: THandle; pszPath: PWideChar
   ): BOOL; stdcall;

 TWIMSetReferenceFile = function(
   hWim: THandle;
   pszPath: PWideChar;
   dwFlags: DWORD
   ): BOOL; stdcall;

 TWIMSplitFile = function(
   hWim: THandle;
   pszPartPath: PWideChar;
   pliPartSize: PLARGE_INTEGER;
   dwFlags: DWORD
   ): BOOL; stdcall;

 TWIMExportImage = function(
   hImage, hWim: THandle;
   dwFlags: DWORD
   ): BOOL; stdcall;

 TWIMDeleteImage = function(
   hWim: THandle;
   dwImageIndex: DWORD
   ): BOOL; stdcall;

 TWIMGetImageCount = function(
   hWim: THandle
   ): DWORD; stdcall;

 TWIMGetAttributes = function(
   hWim: THandle;
   pWimInfo: PWIM_INFO;
   cbWimInfo: DWORD
   ): BOOL; stdcall;

 TWIMSetBootImage = function(
   hWim: THandle;
   dwImageIndex: DWORD
   ): BOOL; stdcall;

 TWIMCaptureImage = function(
   hWim: THandle;
   pszPath: PWideChar;
   dwCaptureFlags: DWORD
   ): THandle; stdcall;

 TWIMLoadImage = function(
   hWim: THandle;
   dwImageIndex: DWORD
   ): THandle; stdcall;

 TWIMApplyImage = function(
   hImage: THandle;
   pszPath: PWideChar;
   dwApplyFlags: DWORD
   ): BOOL; stdcall;

 TWIMGetImageInformation = function(
   hImage: THandle;
   ppvImageInfo: Pointer;
   pcbImageInfo: PDWORD
   ): BOOL; stdcall;

 TWIMSetImageInformation = function(
   hImage: THandle;
   pvImageInfo: Pointer;
   cbImageInfo: DWORD
   ): BOOL; stdcall;

 TWIMGetMessageCallbackCount = function(
   hWim: THandle
   ): DWORD; stdcall;

 TWIMRegisterMessageCallback = function(
   hWim: THandle;
   fpMessageProc, pvUserData: Pointer
   ): DWORD; stdcall;

 TWIMUnregisterMessageCallback = function(
   hWim: THandle;
   fpMessageProc: Pointer
   ): BOOL; stdcall;

 TWIMMessageCallback = function(
   dwMessageId: DWORD;
   wParam: WPARAM;
   lParam: LPARAM;
   pvUserData: Pointer
   ): DWORD; stdcall;

 TWIMCopyFile = function(
   pszExistingFileName, pszNewFileName: PWideChar;
   pProgressRoutine, pvData: Pointer;
   pbCancel: PBOOL;
   dwCopyFlags: DWORD
   ): BOOL; stdcall;

 TWIMMountImage = function(
   pszMountPath, pszWimFileName: PWideChar;
   dwImageIndex: DWORD;
   pszTempPath: PWideChar
   ): BOOL; stdcall;


//Neu in Windows 7 WIMGAPI
 TWIMMountImageHandle = function(
   hImage: THandle;
   pszMountPath: PWideChar;
   dwMountFlags: DWORD
   ): BOOL; stdcall;

//Neu in Windows 7 WIMGAPI
 TWIMRemountImage = function(
   pszMountPath: PWideChar;
   dwMountFlags: DWORD
   ): BOOL; stdcall;

//Neu in Windows 7 WIMGAPI
 TWIMCommitImageHandle = function(
   hImage: THandle;
   dwCommitFlags: DWORD;
   phNewImageHandle: PHandle
   ): BOOL; stdcall;

 TWIMUnmountImage = function(
   pszMountPath, pszWimFileName: PWideChar;
   dwImageIndex: DWORD;
   bCommitChanges: BOOL
   ): BOOL; stdcall;

//Neu in Windows 7 WIMGAPI
 TWIMUnmountImageHandle = function(
   hImage: THandle;
   dwUnmountFlags: DWORD
   ): BOOL; stdcall;

//VERALTET seit Windows 7 - Ersetzt durch WIMGetMountedImageInfo
 TWIMGetMountedImages = function(
   pMountList: PWIM_MOUNT_LIST;
   pcbMountListLength: PDWORD
   ): BOOL; stdcall;

//Neu in Windows 7 WIMGAPI
 TWIMGetMountedImageInfo = function(
   fInfoLevelId: MOUNTED_IMAGE_INFO_LEVELS;
   pdwImageCount: PDWORD;
   pMountInfo: Pointer;
   cbMountInfoLength: DWORD;
   pcbReturnLength: PDWORD
   ): BOOL; stdcall;

//Neu in Windows 7 WIMGAPI
 TWIMGetMountedImageInfoFromHandle = function(
   hImage: THandle;
   fInfoLevelId: MOUNTED_IMAGE_INFO_LEVELS;
   pMountInfo: Pointer;
   cbMountInfoLength: DWORD;
   pcbReturnLength: PDWORD
   ): BOOL; stdcall;

//Neu in Windows 7 WIMGAPI
 TWIMGetMountedImageHandle = function(
   pszMountPath: PWideChar;
   dwFlags: DWORD;
   phWimHandle, phImageHandle: PHandle
   ): BOOL; stdcall;

//Neu in Windows 7 WIMGAPI
 TWIMDeleteImageMounts = function(
   dwDeleteFlags: DWORD
   ): BOOL; stdcall;

//Neu in Windows 7 WIMGAPI
 TWIMRegisterLogFile = function(
   pszLogFile: PWideChar;
   dwFlags: DWORD
   ): BOOL; stdcall;

//Neu in Windows 7 WIMGAPI
 TWIMUnregisterLogFile = function(
   pszLogFile: PWideChar
   ): BOOL; stdcall;

//Neu in Windows 7 WIMGAPI
 TWIMExtractImagePath = function(
   hImage: THandle;
   pszImagePath, pszDestinationPath: PWideChar;
   dwExtractFlags: DWORD
   ): BOOL; stdcall;

 TWIMInitFileIOCallbacks = function(
   pCallbacks: Pointer
   ): BOOL; stdcall;

 TWIMSetFileIOCallbackTemporaryPath = function(
   pszPath: PWideChar
   ): BOOL; stdcall;

//
// File I/O callback prototypes
//
type
   PFILEIOCALLBACK_SESSION = Pointer;

type
   FileIOCallbackOpenFile = function(
     pszFileName: PWideChar
     ): PFILEIOCALLBACK_SESSION;


   FileIOCallbackCloseFile = function(
     hFile: PFILEIOCALLBACK_SESSION
     ): BOOL;

   FileIOCallbackReadFile = function(
     hFile: PFILEIOCALLBACK_SESSION;
     pBuffer: Pointer;
     nNumberOfBytesToRead: DWORD;
     pNumberOfBytesRead: LPDWORD;
     pOverlapped: POVERLAPPED
     ): BOOL;

   FileIOCallbackSetFilePointer = function(
     hFile: PFILEIOCALLBACK_SESSION;
     liDistanceToMove: LARGE_INTEGER;
     pNewFilePointer: PLARGE_INTEGER;
     dwMoveMethod: DWORD
     ): BOOL;

   FileIOCallbackGetFileSize = function(
     hFile : THANDLE;
     pFileSize: PLARGE_INTEGER
     ): BOOL;

type
   _SFileIOCallbackInfo = packed record
     pfnOpenFile : FileIOCallbackOpenFile;
     pfnCloseFile : FileIOCallbackCloseFile;
     pfnReadFile : FileIOCallbackReadFile;
     pfnSetFilePointer: FileIOCallbackSetFilePointer;
     pfnGetFileSize : FileIOCallbackGetFileSize;
     end;
   SFileIOCallbackInfo = _SFileIOCallbackInfo;

const
   WIMGAPI_LIBNAME = 'wimgapi.dll';

var
   WIMCreateFile: TWIMCreateFile;
   WIMCloseHandle: TWIMCloseHandle;
   WIMSetTemporaryPath: TWIMSetTemporaryPath;
   WIMSetReferenceFile: TWIMSetReferenceFile;
   WIMSplitFile: TWIMSplitFile;
   WIMExportImage: TWIMExportImage;
   WIMDeleteImage: TWIMDeleteImage;
   WIMGetImageCount: TWIMGetImageCount;
   WIMGetAttributes: TWIMGetAttributes;
   WIMSetBootImage: TWIMSetBootImage;
   WIMCaptureImage: TWIMCaptureImage;
   WIMLoadImage: TWIMLoadImage;
   WIMApplyImage: TWIMApplyImage;
   WIMGetImageInformation: TWIMGetImageInformation;
   WIMSetImageInformation: TWIMSetImageInformation;
   WIMGetMessageCallbackCount: TWIMGetMessageCallbackCount;
   WIMRegisterMessageCallback: TWIMRegisterMessageCallback;
   WIMUnregisterMessageCallback: TWIMUnregisterMessageCallback;
   WIMMessageCallback: TWIMMessageCallback;
   WIMCopyFile: TWIMCopyFile;
   WIMMountImage: TWIMMountImage;
   WIMMountImageHandle: TWIMMountImageHandle;
   WIMRemountImage: TWIMRemountImage;
   WIMCommitImageHandle: TWIMCommitImageHandle;
   WIMUnmountImage: TWIMUnmountImage;
   WIMUnmountImageHandle: TWIMUnmountImageHandle;
   WIMGetMountedImages: TWIMGetMountedImages;
   WIMGetMountedImageInfo: TWIMGetMountedImageInfo;
   WIMGetMountedImageInfoFromHandle: TWIMGetMountedImageInfoFromHandle;
   WIMGetMountedImageHandle: TWIMGetMountedImageHandle;
   WIMDeleteImageMounts: TWIMDeleteImageMounts;
   WIMRegisterLogFile: TWIMRegisterLogFile;
   WIMUnregisterLogFile: TWIMUnregisterLogFile;
   WIMExtractImagePath: TWIMExtractImagePath;
   WIMInitFileIOCallbacks: TWIMInitFileIOCallbacks;
   WIMSetFileIOCallbackTemporaryPath: TWIMSetFileIOCallbackTemporaryPath;

procedure InitWIMGAPI(const APath: String = WIMGAPI_LIBNAME);
function WIMGAPILoaded: Boolean;

implementation
var
   hWIMGAPI: THandle;

procedure InitWIMGAPI(const APath: String = WIMGAPI_LIBNAME);
begin
   if (hWIMGAPI = 0) then
   begin
     hWIMGAPI := LoadLibrary(PChar(APath));

     if (hWIMGAPI <> 0) then
     begin
       Pointer(WIMCreateFile) := GetProcAddress(hWIMGAPI, 'WIMCreateFile');
       Pointer(WIMCloseHandle) := GetProcAddress(hWIMGAPI, 'WIMCloseHandle');
       Pointer(WIMSetTemporaryPath) := GetProcAddress(hWIMGAPI, 'WIMSetTemporaryPath');
       Pointer(WIMSetReferenceFile) := GetProcAddress(hWIMGAPI, 'WIMSetReferenceFile');
       Pointer(WIMSplitFile) := GetProcAddress(hWIMGAPI, 'WIMSplitFile');
       Pointer(WIMExportImage) := GetProcAddress(hWIMGAPI, 'WIMExportImage');
       Pointer(WIMDeleteImage) := GetProcAddress(hWIMGAPI, 'WIMDeleteImage');
       Pointer(WIMGetImageCount) := GetProcAddress(hWIMGAPI, 'WIMGetImageCount');
       Pointer(WIMGetAttributes) := GetProcAddress(hWIMGAPI, 'WIMGetAttributes');
       Pointer(WIMSetBootImage) := GetProcAddress(hWIMGAPI, 'WIMSetBootImage');
       Pointer(WIMCaptureImage) := GetProcAddress(hWIMGAPI, 'WIMCaptureImage');
       Pointer(WIMLoadImage) := GetProcAddress(hWIMGAPI, 'WIMLoadImage');
       Pointer(WIMApplyImage) := GetProcAddress(hWIMGAPI, 'WIMApplyImage');
       Pointer(WIMGetImageInformation) := GetProcAddress(hWIMGAPI, 'WIMGetImageInformation');
       Pointer(WIMSetImageInformation) := GetProcAddress(hWIMGAPI, 'WIMSetImageInformation');
       Pointer(WIMGetMessageCallbackCount) := GetProcAddress(hWIMGAPI, 'WIMGetMessageCallbackCount');
       Pointer(WIMRegisterMessageCallback) := GetProcAddress(hWIMGAPI, 'WIMRegisterMessageCallback');
       Pointer(WIMUnregisterMessageCallback) := GetProcAddress(hWIMGAPI, 'WIMUnregisterMessageCallback');
       Pointer(WIMMessageCallback) := GetProcAddress(hWIMGAPI, 'WIMMessageCallback');
       Pointer(WIMCopyFile) := GetProcAddress(hWIMGAPI, 'WIMCopyFile');
       Pointer(WIMMountImage) := GetProcAddress(hWIMGAPI, 'WIMMountImage');
       Pointer(WIMMountImageHandle) := GetProcAddress(hWIMGAPI, 'WIMMountImageHandle');
       Pointer(WIMRemountImage) := GetProcAddress(hWIMGAPI, 'WIMRemountImage');
       Pointer(WIMCommitImageHandle) := GetProcAddress(hWIMGAPI, 'WIMCommitImageHandle');
       Pointer(WIMUnmountImage) := GetProcAddress(hWIMGAPI, 'WIMUnmountImage');
       Pointer(WIMUnmountImageHandle) := GetProcAddress(hWIMGAPI, 'WIMUnmountImageHandle');
       Pointer(WIMGetMountedImages) := GetProcAddress(hWIMGAPI, 'WIMGetMountedImages');
       Pointer(WIMGetMountedImageInfo) := GetProcAddress(hWIMGAPI, 'WIMGetMountedImageInfo');
       Pointer(WIMGetMountedImageInfoFromHandle) := GetProcAddress(hWIMGAPI, 'WIMGetMountedImageInfoFromHandle');
       Pointer(WIMGetMountedImageHandle) := GetProcAddress(hWIMGAPI, 'WIMGetMountedImageHandle');
       Pointer(WIMDeleteImageMounts) := GetProcAddress(hWIMGAPI, 'WIMDeleteImageMounts');
       Pointer(WIMRegisterLogFile) := GetProcAddress(hWIMGAPI, 'WIMRegisterLogFile');
       Pointer(WIMUnregisterLogFile) := GetProcAddress(hWIMGAPI, 'WIMUnregisterLogFile');
       Pointer(WIMExtractImagePath) := GetProcAddress(hWIMGAPI, 'WIMExtractImagePath');
       Pointer(WIMInitFileIOCallbacks) := GetProcAddress(hWIMGAPI, 'WIMInitFileIOCallbacks');
       Pointer(WIMSetFileIOCallbackTemporaryPath) := GetProcAddress(hWIMGAPI, 'WIMSetFileIOCallbackTemporaryPath');
     end;
   end;
end;

procedure CloseWIMGAPI;
begin
   if (hWIMGAPI <> 0) then
   begin
     FreeLibrary(hWIMGAPI);
     hWIMGAPI := 0;
   end;
end;

function WIMGAPILoaded: Boolean;
begin
   Result := (hWIMGAPI <> 0);
end;

initialization
   hWIMGAPI := 0;

   WIMCreateFile := nil;
   WIMCloseHandle := nil;
   WIMSetTemporaryPath := nil;
   WIMSetReferenceFile := nil;
   WIMSplitFile := nil;
   WIMExportImage := nil;
   WIMDeleteImage := nil;
   WIMGetImageCount := nil;
   WIMGetAttributes := nil;
   WIMSetBootImage := nil;
   WIMCaptureImage := nil;
   WIMLoadImage := nil;
   WIMApplyImage := nil;
   WIMGetImageInformation := nil;
   WIMSetImageInformation := nil;
   WIMGetMessageCallbackCount := nil;
   WIMRegisterMessageCallback := nil;
   WIMUnregisterMessageCallback := nil;
   WIMMessageCallback := nil;
   WIMCopyFile := nil;
   WIMMountImage := nil;
   WIMMountImageHandle := nil;
   WIMRemountImage := nil;
   WIMCommitImageHandle := nil;
   WIMUnmountImage := nil;
   WIMUnmountImageHandle := nil;
   WIMGetMountedImages := nil;
   WIMGetMountedImageInfo := nil;
   WIMGetMountedImageInfoFromHandle := nil;
   WIMGetMountedImageHandle := nil;
   WIMDeleteImageMounts := nil;
   WIMRegisterLogFile := nil;
   WIMUnregisterLogFile := nil;
   WIMExtractImagePath := nil;
   WIMInitFileIOCallbacks := nil;
   WIMSetFileIOCallbackTemporaryPath := nil;

finalization
   CloseWIMGAPI;

end.
