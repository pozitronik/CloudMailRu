unit CloudMailRu;

interface

uses
	DebugHelper,
	CMRDirItemList,
	CMRDirItem,
	CMRInviteList,
	CMRIncomingInviteList,
	CMROAuth,
	CMRSpace,
	CMRFileIdentity,
	CMROperationResult,
	CMRTwostep,
	JSONHelper,
	ParsingHelper,
	CMRConstants,
	CloudMailRuHTTP,
	LANGUAGE_STRINGS,
	HashInfo,
	System.Hash,
	System.Classes,
	System.Generics.Collections,
	System.SysUtils,
	SETTINGS_CONSTANTS,
	PLUGIN_TYPES,
	Winapi.Windows,
	PluginHelper,
	PathHelper,
	FileHelper,
	StringHelper,
	SystemHelper,
	WindowsHelper,
	ILoggerInterface,
	IProgressInterface,
	IRequestInterface,
	CipherInterface,
	RealPath,
	CloudSettings,
	FileCipher,
	FileSplitInfo,
	ChunkedFileStream,
	IHTTPManagerInterface,
	IdCookieManager,
	DCPbase64,
	AskPassword,
	IAuthStrategyInterface;

type
	TCloudMailRu = class
	private
		FSettings: TCloudSettings; {Current options set for the cloud instance}

		FHTTPManager: IHTTPManager; {Internal connections manager (can be set externally or omitted)}
		FHTTPConnection: TCloudMailRuHTTP; {Normally managed by HTTPConnectionsManager. If HTTPConnectionsManager is omitted, Cloud will create its own atomic connection}

		FCookieManager: TIdCookieManager; {The auth cookie, should be stored separately, because it associated with a cloud instance, not a connection}

		FLogger: ILogger;
		FProgress: IProgress;
		FRequest: IRequest;

		FCipher: ICipher; {The encryption instance}
		FAuthStrategy: IAuthStrategy; {Authentication strategy}

		FPublicLink: WideString; {Holder for GetPublicLink() value, should not be accessed directly}
		FPublicShard: WideString; {Public shard url, used for public downloads}
		FDownloadShard: WideString; {Holder of the current instance download shard adress, retrieved on the first download attempt}
		FUploadShard: WideString; {Holder of the current instance upload shard adress, retrieved on the first upload attempt}

		FAuthToken: WideString; {The current (constantly refreshing) connection token}
		FOAuthToken: TCMROAuth; {Unused at this moment}

		FUnitedParams: WideString; {The set of required authentification attributes united to the string — just for a handy usage}

		{HTTP REQUESTS WRAPPERS}
		function InitSharedConnectionParameters(): Boolean;
		function GetShard(var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean;
		function GetUserSpace(var SpaceInfo: TCMRSpace): Boolean;
		function PutFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCMRFileIdentity): Integer; overload; //отправка на сервер данных из потока
		{PRIVATE UPLOAD METHODS CHAIN (CALLED FROM putFile())}
		function PutFileWhole(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): Integer; //Загрузка файла целиком
		function PutFileSplit(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer; //Загрузка файла по частям
		function PutFileStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString = CLOUD_CONFLICT_STRICT): Integer;

		{OTHER ROUTINES}
		function GetHTTPConnection: TCloudMailRuHTTP;
		function RefreshCSRFToken: Boolean;
	protected
		FUser: WideString;
		FDomain: WideString;
		FDoCryptFiles: Boolean;
		FDoCryptFilenames: Boolean;
		{HASHING - exposed for testing via subclass}
		function CloudHash(Path: WideString): WideString; overload; //get cloud hash for specified file
		function CloudHash(Stream: TStream; Path: WideString = CALCULATING_HASH): WideString; overload; //get cloud hash for data in stream
		{Those properties are simple shortcuts to settings fields}
		property Password: WideString read FSettings.AccountSettings.Password;
		property Email: WideString read FSettings.AccountSettings.Email;
		property DownloadShardOverride: WideString read FSettings.AccountSettings.ShardOverride;
		property UploadShardOverride: WideString read FSettings.AccountSettings.UploadUrlOverride;
		property UnlimitedFileSize: Boolean read FSettings.AccountSettings.UnlimitedFileSize;
		property SplitLargeFiles: Boolean read FSettings.AccountSettings.SplitLargeFiles;
		property CloudMaxFileSize: Int64 read FSettings.CloudMaxFileSize;
		property PrecalculateHash: Boolean read FSettings.PrecalculateHash;
		property ForcePrecalculateSize: Int64 read FSettings.ForcePrecalculateSize;
		property CheckCRC: Boolean read FSettings.CheckCRC;
		property OperationErrorMode: Integer read FSettings.OperationErrorMode;
		property RetryAttempts: Integer read FSettings.RetryAttempts;
		property AttemptWait: Integer read FSettings.AttemptWait;
		{Also shortcut properties}
		property HTTP: TCloudMailRuHTTP read GetHTTPConnection;

		{REGULAR CLOUD}
		function LoginRegular(Method: Integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function GetFileRegular(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = true): Integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		{SHARED WEBFOLDERS}
		function LoginShared(Method: Integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function GetFileShared(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = true): Integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		function GetPublicLink(): WideString;
	public
		property IsPublicAccount: Boolean read FSettings.AccountSettings.PublicAccount;
		{ERROR RESULT MAPPING - exposed for testing and external use}
		function CloudResultToFsResult(CloudResult: TCMROperationResult; ErrorPrefix: WideString = ''): Integer; overload;
		function CloudResultToFsResult(JSON: WideString; ErrorPrefix: WideString = ''): Integer; overload;
		function CloudResultToBoolean(CloudResult: TCMROperationResult; ErrorPrefix: WideString = ''): Boolean; overload;
		function CloudResultToBoolean(JSON: WideString; ErrorPrefix: WideString = ''): Boolean; overload;
		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(CloudSettings: TCloudSettings; ConnectionManager: IHTTPManager; AuthStrategy: IAuthStrategy; Logger: ILogger; Progress: IProgress; Request: IRequest);
		destructor Destroy; override;
		{CLOUD INTERFACE METHODS}
		function Login(Method: Integer = CLOUD_AUTH_METHOD_WEB): Boolean;
		function GetDirListing(Path: WideString; var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function GetSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
		function GetSharedLinksListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function GetIncomingLinksListing(var IncomingListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean; overload;
		function GetIncomingLinksListing(var IncomingListing: TCMRDirItemList; var InvitesListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean; overload;
		function GetTrashbinListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
		function CreateDir(Path: WideString): Boolean;
		function RemoveDir(Path: WideString): Boolean;
		function StatusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
		function GetFile(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean = true): Integer; //LogErrors=false => не логируем результат копирования, нужно для запроса descript.ion (которого может не быть)
		function PutFile(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer;
		function RenameFile(OldName, NewName: WideString): Integer; //смена имени без перемещения
		function MoveFile(OldName, ToPath: WideString): Integer; //перемещение по дереву каталогов
		function CopyFile(OldName, ToPath: WideString): Integer; //Копирование файла внутри одного аккаунта
		function FileMove(OldName, NewName: WideString): Integer; //объединяющая функция, определяет делать rename или move
		function FileCopy(OldName, NewName: WideString): Integer; //Копирует файл, и переименует, если нужно
		function DeleteFile(Path: WideString): Boolean;
		function PublishFile(Path: WideString; var PublicLink: WideString; Publish: Boolean = CLOUD_PUBLISH): Boolean;
		function CloneWeblink(Path, Link: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Integer; //клонировать публичную ссылку в текущий каталог

		function AddFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = False): Integer; overload;
		function AddFileByIdentity(FileIdentity: TCMRDirItem; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = False): Integer; overload;

		function GetShareInfo(Path: WideString; var InviteListing: TCMRInviteList): Boolean;
		function ShareFolder(Path, Email: WideString; Access: Integer): Boolean;
		function TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function TrashbinEmpty(): Boolean;
		function MountFolder(Home, InviteToken: WideString; ConflictMode: WideString = CLOUD_CONFLICT_RENAME): Boolean;
		function UnmountFolder(Home: WideString; Clone_copy: Boolean): Boolean;
		function RejectInvite(InviteToken: WideString): Boolean;
		function GetPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; Publish: Boolean = CLOUD_PUBLISH): Boolean;
		{OTHER ROUTINES}
		function GetDescriptionFile(RemotePath, LocalCopy: WideString): Boolean; //Если в каталоге remotePath есть descript.ion - скопировать его в файл localcopy
		function PutDescriptionFile(RemotePath, LocalCopy: WideString): Boolean; //Скопировать descript.ion из временного файла на сервер
		procedure LogUserSpaceInfo();
		function FileIdentity(LocalPath: WideString): TCMRFileIdentity;
		{STATIC ROUTINES}
		class function CloudAccessToString(Access: WideString; Invert: Boolean = False): WideString; static;
		class function StringToCloudAccess(AccessString: WideString; Invert: Boolean = False): Integer; static;
		class function ErrorCodeText(ErrorCode: Integer): WideString; static;
		class function TempPublicCloudInit(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean; static;
	end;

implementation

{TCloudMailRu}
function TCloudMailRu.AddFileByIdentity(FileIdentity: TCMRFileIdentity; RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; LogErrors: Boolean = true; LogSuccess: Boolean = False): Integer;
var
	FileName: WideString;
	JSON: WideString;
	OperationResult: TCMROperationResult;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);

	if FDoCryptFilenames then
	begin
		FileName := ExtractUniversalFileName(RemotePath);
		FileName := FCipher.CryptFileName(FileName);
		RemotePath := ChangePathFileName(RemotePath, FileName);
	end;
	{Экспериментально выяснено, что параметры api, build, email, x-email, x-page-id в запросе не обязательны}
	if HTTP.PostForm(API_FILE_ADD + '?' + FUnitedParams, Format('api=2&conflict=%s&home=/%s&hash=%s&size=%d', [ConflictMode, PathToUrl(RemotePath), FileIdentity.Hash, FileIdentity.size]), JSON, 'application/x-www-form-urlencoded', LogErrors, False) then {Do not allow to cancel operation here}
	begin
		OperationResult.FromJSON(JSON);
		Result := CloudResultToFsResult(OperationResult, PREFIX_ERR_FILE_UPLOADING);
		if (CLOUD_OPERATION_OK = OperationResult.OperationResult) and LogSuccess then
			FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, FILE_FOUND_BY_HASH, [RemotePath]);
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := AddFileByIdentity(FileIdentity, RemotePath, ConflictMode, LogErrors, LogSuccess);
	end;
end;

function TCloudMailRu.AddFileByIdentity(FileIdentity: TCMRDirItem; RemotePath, ConflictMode: WideString; LogErrors, LogSuccess: Boolean): Integer;
var
	CloudFileIdentity: TCMRFileIdentity;
begin
	CloudFileIdentity.Hash := FileIdentity.Hash;
	CloudFileIdentity.size := FileIdentity.size;
	Result := AddFileByIdentity(CloudFileIdentity, RemotePath, ConflictMode, LogErrors, LogSuccess)
end;

function TCloudMailRu.CloneWeblink(Path, Link, ConflictMode: WideString): Integer;
var
	JSON: WideString;
	Progress: Boolean;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);
	Progress := true;
	if HTTP.GetPage(Format('%s?folder=/%s&weblink=%s&conflict=%s&%s', [API_CLONE, PathToUrl(Path), Link, ConflictMode, FUnitedParams]), JSON, Progress) then
	begin //Парсим ответ
		Result := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_PUBLISH);
		if (Result <> FS_FILE_OK) and not(Progress) then
			Result := FS_FILE_USERABORT; //user cancelled
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := CloneWeblink(Path, Link, ConflictMode);
	end;
end;

function TCloudMailRu.CloudResultToBoolean(JSON, ErrorPrefix: WideString): Boolean;
begin
	Result := CloudResultToBoolean(TCMROperationResult.GetOperationResult(JSON), ErrorPrefix)
end;

function TCloudMailRu.CloudResultToBoolean(CloudResult: TCMROperationResult; ErrorPrefix: WideString): Boolean;
begin
	Result := CloudResult.ToBoolean;
	if not(Result) and (ErrorPrefix <> EmptyWideStr) then
		FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s%s%s', [ErrorPrefix, ErrorCodeText(CloudResult.OperationResult), PREFIX_STATUS, CloudResult.OperationStatus]);
end;

function TCloudMailRu.CloudResultToFsResult(JSON, ErrorPrefix: WideString): Integer;
begin
	Result := CloudResultToFsResult(TCMROperationResult.GetOperationResult(JSON), ErrorPrefix);
end;

function TCloudMailRu.CloudResultToFsResult(CloudResult: TCMROperationResult; ErrorPrefix: WideString): Integer;
begin
	case CloudResult.OperationResult of
		CLOUD_OPERATION_OK:
			Exit(FS_FILE_OK);
		CLOUD_ERROR_EXISTS:
			Exit(FS_FILE_EXISTS);
		CLOUD_ERROR_REQUIRED, CLOUD_ERROR_INVALID, CLOUD_ERROR_READONLY, CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
			Exit(FS_FILE_WRITEERROR);
		CLOUD_ERROR_UNKNOWN:
			Exit(FS_FILE_NOTSUPPORTED);
		CLOUD_ERROR_OVERQUOTA:
			begin
				FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_INSUFFICIENT_STORAGE);
				Exit(FS_FILE_WRITEERROR);
			end;
		CLOUD_ERROR_NAME_TOO_LONG:
			begin
				FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_NAME_TOO_LONG);
				Exit(FS_FILE_WRITEERROR);
			end;
		else
			begin //что-то неизвестное
				if (ErrorPrefix <> EmptyWideStr) then
					FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s%s%d', [ErrorPrefix, ErrorCodeText(CloudResult.OperationResult), PREFIX_STATUS, CloudResult.OperationStatus]);
				Exit(FS_FILE_WRITEERROR);
			end;
	end;
end;

function TCloudMailRu.CopyFile(OldName, ToPath: WideString): Integer;
var
	JSON: WideString;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);
	HTTP.SetProgressNames(OldName, Format('%s%s', [IncludeTrailingPathDelimiter(ToPath), ExtractFileName(OldName)]));
	if HTTP.PostForm(API_FILE_COPY + '?' + FUnitedParams, Format('home=/%s&folder=/%s&conflict', [PathToUrl(OldName), PathToUrl(ToPath)]), JSON) then
	begin //Парсим ответ
		Result := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_COPY);
	end;
	if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
		Result := CopyFile(OldName, ToPath);
end;

function TCloudMailRu.FileCopy(OldName, NewName: WideString): Integer;
var
	NewPath: WideString;
	SameDir, SameName: Boolean;
begin //Облако умеет скопировать файл, но не сможет его переименовать, поэтому хитрим
	NewPath := ExtractFilePath(NewName);
	SameDir := ExtractFilePath(OldName) = ExtractFilePath(NewName);
	SameName := ExtractFileName(OldName) = ExtractFileName(NewName);
	if (SameDir) then //копирование в тот же каталог не поддерживается напрямую, а мудрить со временными каталогами я не хочу
	begin
		FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, ERR_COPY_SAME_DIR_NOT_SUPPORTED);
		Exit(FS_FILE_NOTSUPPORTED);
	end else begin
		{TODO: issue #219}
		//if (statusFile(NewName,FileInfo)) then //file already exists
		//begin
		//
		//end;
		Result := CopyFile(OldName, NewPath);
		if Result <> CLOUD_OPERATION_OK then
			Exit;
	end;
	if not(SameName) then
	begin //скопированный файл лежит в новом каталоге со старым именем
		Result := RenameFile(Format('%s%s', [NewPath, ExtractFileName(OldName)]), ExtractFileName(NewName));
	end;
end;

constructor TCloudMailRu.Create(CloudSettings: TCloudSettings; ConnectionManager: IHTTPManager; AuthStrategy: IAuthStrategy; Logger: ILogger; Progress: IProgress; Request: IRequest);
var
	FileCipherInstance: TFileCipher;
begin
	try
		FSettings := CloudSettings;
		ExtractEmailParts(Email, FUser, FDomain);

		FHTTPManager := ConnectionManager;
		FAuthStrategy := AuthStrategy;

		FProgress := Progress;
		FLogger := Logger;
		FRequest := Request;

		FCookieManager := TIdCookieManager.Create();

		if FSettings.AccountSettings.EncryptFilesMode <> EncryptModeNone then
		begin
			FileCipherInstance := TFileCipher.Create(FSettings.CryptFilesPassword, FSettings.AccountSettings.CryptedGUIDFiles, FSettings.AccountSettings.EncryptFilenames);
			if FileCipherInstance.IsWrongPassword then
				FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_WRONG_ENCRYPT_PASSWORD);

			FDoCryptFiles := not(FileCipherInstance.IsWrongPassword);
			FDoCryptFilenames := FDoCryptFiles and FSettings.AccountSettings.EncryptFilenames;
			FCipher := FileCipherInstance;
		end;

	except
		on E: Exception do
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_ERR_FILE_UPLOADING, E.Message]);
		end;
	end;
end;

function TCloudMailRu.CreateDir(Path: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit;
	HTTP.SetProgressNames(CREATE_DIRECTORY, Path);
	Result := HTTP.PostForm(API_FOLDER_ADD + '?' + FUnitedParams, Format('home=/%s&conflict', [PathToUrl(Path)]), JSON) and CloudResultToBoolean(JSON);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := CreateDir(Path);
end;

function TCloudMailRu.DeleteFile(Path: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit;
	HTTP.SetProgressNames(DELETE_FILE, Path);
	Result := HTTP.PostForm(API_FILE_REMOVE + '?' + FUnitedParams, Format('home=/%s&conflict', [PathToUrl(Path)]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_DELETE_FILE);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := DeleteFile(Path);
end;

destructor TCloudMailRu.Destroy;
begin
	//HTTP.Destroy;

	FCookieManager.Destroy;

	if Assigned(FHTTPConnection) then
		FHTTPConnection.Destroy;

	FCipher := nil; {Release interface reference}

	inherited;
end;

class function TCloudMailRu.ErrorCodeText(ErrorCode: Integer): WideString;
begin
	case ErrorCode of
		CLOUD_ERROR_EXISTS:
			Exit(ERR_CLOUD_ERROR_EXISTS);
		CLOUD_ERROR_REQUIRED:
			Exit(ERR_CLOUD_ERROR_REQUIRED);
		CLOUD_ERROR_INVALID:
			Exit(ERR_CLOUD_ERROR_INVALID);
		CLOUD_ERROR_READONLY:
			Exit(ERR_CLOUD_ERROR_READONLY);
		CLOUD_ERROR_NAME_LENGTH_EXCEEDED:
			Exit(ERR_CLOUD_ERROR_NAME_LENGTH_EXCEEDED);
		CLOUD_ERROR_OVERQUOTA:
			Exit(ERR_CLOUD_ERROR_OVERQUOTA);
		CLOUD_ERROR_NOT_EXISTS:
			Exit(ERR_CLOUD_ERROR_NOT_EXISTS);
		CLOUD_ERROR_OWN:
			Exit(ERR_CLOUD_ERROR_OWN);
		CLOUD_ERROR_NAME_TOO_LONG:
			Exit(ERR_CLOUD_ERROR_NAME_TOO_LONG);
		CLOUD_ERROR_VIRUS_SCAN_FAIL:
			Exit(ERR_CLOUD_ERROR_VIRUS_SCAN_FAIL);
		CLOUD_ERROR_OWNER:
			Exit(ERR_CLOUD_ERROR_OWNER);
		CLOUD_ERROR_FAHRENHEIT:
			Exit(ERR_CLOUD_ERROR_FAHRENHEIT);
		CLOUD_ERROR_BAD_REQUEST:
			Exit(ERR_CLOUD_ERROR_BAD_REQUEST);
		CLOUD_ERROR_TREES_CONFLICT:
			Exit(ERR_CLOUD_ERROR_TREES_CONFLICT);
		CLOUD_ERROR_UNPROCESSABLE_ENTRY:
			Exit(ERR_CLOUD_ERROR_UNPROCESSABLE_ENTRY);
		CLOUD_ERROR_USER_LIMIT_EXCEEDED:
			Exit(ERR_CLOUD_ERROR_USER_LIMIT_EXCEEDED);
		CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED:
			Exit(ERR_CLOUD_ERROR_EXPORT_LIMIT_EXCEEDED);
		CLOUD_ERROR_NOT_ACCEPTABLE:
			Exit(ERR_CLOUD_ERROR_NOT_ACCEPTABLE);
		else
			Exit(Format(ERR_CLOUD_ERROR_UNKNOWN, [ErrorCode]));
	end;
end;

function TCloudMailRu.FileIdentity(LocalPath: WideString): TCMRFileIdentity;
begin
	Result.Hash := CloudHash(LocalPath);
	Result.size := SizeOfFile(LocalPath);
end;

function TCloudMailRu.GetDescriptionFile(RemotePath, LocalCopy: WideString): Boolean;
var
	ResultHash: WideString;
begin
	Result := GetFile(RemotePath, LocalCopy, ResultHash, False) = FS_FILE_OK;
end;

function TCloudMailRu.PutDescriptionFile(RemotePath, LocalCopy: WideString): Boolean;
begin
	if FileExists(LocalCopy) then
		Result := PutFile(LocalCopy, RemotePath) = FS_FILE_OK
	else
		Result := DeleteFile(RemotePath);
end;

function TCloudMailRu.GetSharedLinksListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if IsPublicAccount then
		Exit;
	if (ShowProgress) then
		HTTP.SetProgressNames(SHARED_LINKS_LISTING, UNKNOWN_ITEM);

	Result := HTTP.GetPage(Format('%s?%s', [API_FOLDER_SHARED_LINKS, FUnitedParams]), JSON, ShowProgress);
	if Result then
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_SHARED_LINKS_LISTING) and GetDirListing(JSON, DirListing)
	else
	begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := GetSharedLinksListing(DirListing, ShowProgress);
	end;

end;

function TCloudMailRu.GetIncomingLinksListing(var IncomingListing: TCMRIncomingInviteList; ShowProgress: Boolean): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	SetLength(IncomingListing, 0);
	if IsPublicAccount then
		Exit;
	if (ShowProgress) then
		HTTP.SetProgressNames(INCOMING_LINKS_LISTING, UNKNOWN_ITEM);
	Result := HTTP.GetPage(Format('%s?%s', [API_FOLDER_SHARED_INCOMING, FUnitedParams]), JSON, ShowProgress);

	if Result then
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_INCOMING_REQUESTS_LISTING) and IncomingListing.FromJSON(JSON)
	else
	begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := GetIncomingLinksListing(IncomingListing, ShowProgress);
	end;
end;

function TCloudMailRu.GetIncomingLinksListing(var IncomingListing: TCMRDirItemList; var InvitesListing: TCMRIncomingInviteList; ShowProgress: Boolean = False): Boolean;
var
	i: Integer;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	SetLength(IncomingListing, 0);
	Result := GetIncomingLinksListing(InvitesListing, ShowProgress);
	if Result then
	begin
		SetLength(IncomingListing, length(InvitesListing));
		for i := 0 to length(InvitesListing) - 1 do
		begin

			IncomingListing[i].name := InvitesListing[i].name;
			IncomingListing[i].size := InvitesListing[i].size;
			IncomingListing[i].tree := InvitesListing[i].tree;
			//IncomingListing[length(IncomingListing)].
		end;

	end;
end;

function TCloudMailRu.GetTrashbinListing(var DirListing: TCMRDirItemList; ShowProgress: Boolean): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if IsPublicAccount then
		Exit;
	if (ShowProgress) then
		HTTP.SetProgressNames(TRASH_LISTING, UNKNOWN_ITEM);
	Result := HTTP.GetPage(Format('%s?%s', [API_TRASHBIN, FUnitedParams]), JSON, ShowProgress);

	if Result then
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_TRASH_LISTING) and GetDirListing(JSON, DirListing)
	else
	begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := GetTrashbinListing(DirListing, ShowProgress);
	end;

end;

function TCloudMailRu.GetDirListing(Path: WideString; var DirListing: TCMRDirItemList; ShowProgress: Boolean = False): Boolean;
var
	JSON: WideString;
	OperationResult: TCMROperationResult;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	SetLength(DirListing, 0);
	if IsPublicAccount then
		Result := HTTP.GetPage(Format('%s&weblink=%s%s&%s', [API_FOLDER, IncludeSlash(GetPublicLink), PathToUrl(Path, False), FUnitedParams]), JSON, ShowProgress)
	else
	begin
		HTTP.SetProgressNames(DIR_LISTING, Path);
		Result := HTTP.GetPage(Format('%s&home=%s&%s', [API_FOLDER, PathToUrl(Path), FUnitedParams]), JSON, ShowProgress);
	end;
	if Result then
	begin
		OperationResult.FromJSON(JSON);
		Result := CloudResultToBoolean(OperationResult, PREFIX_ERR_DIR_LISTING);
		if Result then
		begin
			Result := DirListing.FromJSON(JSON);
			if Result and FDoCryptFilenames then
				FCipher.DecryptDirListing(DirListing);
		end else if OperationResult.OperationResult = CLOUD_ERROR_NOT_EXISTS then
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_ERR_PATH_NOT_EXISTS, Path]);
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := GetDirListing(Path, DirListing, ShowProgress);
	end;

end;

function TCloudMailRu.GetFile(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации

	HTTP.SetProgressNames(RemotePath, LocalPath);
	if IsPublicAccount then
		Result := GetFileShared(RemotePath, LocalPath, ResultHash, LogErrors)
	else
		Result := GetFileRegular(RemotePath, LocalPath, ResultHash, LogErrors);

end;

function TCloudMailRu.GetFileRegular(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
var
	FileStream: TBufferedFileStream;
	URL, FileName: WideString;
	MemoryStream: TMemoryStream;
	DispatcherResponse: WideString;
	Progress: Boolean;
	SavedUserAgent: WideString;
begin
	Result := FS_FILE_NOTSUPPORTED;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if FDownloadShard = EmptyWideStr then
	begin
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, UNDEFINED_DOWNLOAD_SHARD);
		if DownloadShardOverride <> EmptyWideStr then
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_DETAILS, SHARD_OVERRIDDEN);
			FDownloadShard := DownloadShardOverride;
		end else begin
			{OAuth uses different dispatcher endpoint that returns plain text URL}
			Progress := False;
			if HTTP.GetPage(Format('%s/d?token=%s', [OAUTH_DISPATCHER_URL, FOAuthToken.access_token]), DispatcherResponse, Progress) then
			begin
				{Response format: "URL IP COUNT", extract the URL (first word)}
				FDownloadShard := Trim(Copy(DispatcherResponse, 1, Pos(' ', DispatcherResponse) - 1));
				FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PREFIX_SHARD_RECEIVED, [FDownloadShard, SHARD_TYPE_GET]);
			end else
				Exit;
		end;
	end;
	if FDoCryptFilenames then
	begin
		FileName := ExtractUniversalFileName(RemotePath);
		FileName := FCipher.DecryptFileName(FileName);
		LocalPath := ChangePathFileName(LocalPath, FileName);
	end;

	try
		FileStream := TBufferedFileStream.Create(GetUNCFilePath(LocalPath), fmCreate);
	except
		on E: Exception do
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, E.Message);
			Exit(FS_FILE_WRITEERROR);
		end;
	end;

	{OAuth download endpoint blocks browser-like User-Agents (Mozilla/*)}
	SavedUserAgent := HTTP.HTTP.Request.UserAgent;
	HTTP.HTTP.Request.UserAgent := OAUTH_USERAGENT;
	try
		if FDoCryptFiles then //Загрузка файла в память, дешифрация в файл
		begin
			MemoryStream := TMemoryStream.Create;
			try
				{OAuth requires client_id and token parameters for download authentication}
				URL := Format('%s%s?client_id=%s&token=%s', [FDownloadShard, PathToUrl(RemotePath, False), OAUTH_CLIENT_ID, FOAuthToken.access_token]);
				Result := HTTP.GetFile(URL, MemoryStream, LogErrors);

				if (CLOUD_ERROR_TOKEN_OUTDATED = Result) and RefreshCSRFToken() then
					Exit(GetFileRegular(RemotePath, LocalPath, ResultHash, LogErrors));

				if Result in [FS_FILE_NOTSUPPORTED] then //this code returned on shard connection error
				begin
					FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, '%s%s', [PREFIX_REDIRECTION_LIMIT, URL]);
					if (FRequest.Request(RT_MsgYesNo, REDIRECTION_LIMIT, TRY_ANOTHER_SHARD, EmptyWideStr, 0)) and (GetShard(FDownloadShard)) then
						Exit(GetFileRegular(RemotePath, LocalPath, ResultHash, LogErrors));
				end;

				if Result in [FS_FILE_OK] then
				begin
					ResultHash := CloudHash(MemoryStream);
					MemoryStream.Position := 0;
					FCipher.DecryptStream(MemoryStream, FileStream);
				end;
			finally
				MemoryStream.Free;
			end;
		end else begin
			{OAuth requires client_id and token parameters for download authentication}
			Result := HTTP.GetFile(Format('%s%s?client_id=%s&token=%s', [FDownloadShard, PathToUrl(RemotePath, False), OAUTH_CLIENT_ID, FOAuthToken.access_token]), FileStream, LogErrors);
			if (CLOUD_ERROR_TOKEN_OUTDATED = Result) and RefreshCSRFToken() then
				Exit(GetFileRegular(RemotePath, LocalPath, ResultHash, LogErrors));
			if ((Result in [FS_FILE_OK]) and (EmptyWideStr = ResultHash)) then
				ResultHash := CloudHash(FileStream);
		end;

		FlushFileBuffers(FileStream.Handle);
	finally
		HTTP.HTTP.Request.UserAgent := SavedUserAgent;
		FileStream.Free;
	end;

	if not(Result in [FS_FILE_OK]) then
		System.SysUtils.DeleteFile(GetUNCFilePath(LocalPath));
end;

{since 29.07.2022: изменена логика получения ссылок, см. issue #285. URL теперь всегда должны быть кодированы, иначе в некоторых случаях приходит 400}
function TCloudMailRu.GetSharedFileUrl(RemotePath: WideString; ShardType: WideString = SHARD_TYPE_DEFAULT): WideString;
var
	usedShard: WideString;
	ProgressEnabled: Boolean;
begin
	if ShardType = SHARD_TYPE_DEFAULT then
		usedShard := FPublicShard
	else
		GetShard(usedShard, ShardType);
	if (IsPublicAccount) then
		Exit(Format('%s%s%s', [IncludeSlash(usedShard), IncludeSlash(GetPublicLink), PathToUrl(RemotePath, true, true)]));

	if (TRealPath.GetRealPath(RemotePath).isDir = ID_True) then {для ссылок внутри каталогов перебираются файлы внутри «публичной ссылки» на каталог}
	begin
		Result := Format('%s%s%s', [IncludeSlash(usedShard), GetPublicLink, PathToUrl(RemotePath, true, true)]);
	end else begin {для прямых ссылок берутся публичные ссылки файлов}
		Result := Format('%s%s%s', [IncludeSlash(usedShard), GetPublicLink])
	end;

	ProgressEnabled := False;
	FHTTPConnection.GetRedirection(Result, Result, ProgressEnabled);

end;

function TCloudMailRu.GetFileShared(RemotePath, LocalPath: WideString; var ResultHash: WideString; LogErrors: Boolean): Integer;
var
	FileStream: TBufferedFileStream;
begin
	Result := FS_FILE_NOTFOUND;
	if (FPublicShard = EmptyWideStr) then
		Exit;
	try
		FileStream := TBufferedFileStream.Create(GetUNCFilePath(LocalPath), fmCreate);
	except
		on E: Exception do
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, E.Message);
			Exit(FS_FILE_WRITEERROR);
		end;
	end;
	if (Assigned(FileStream)) then
	begin
		try
			Result := HTTP.GetFile(GetSharedFileUrl(RemotePath), FileStream, LogErrors);
			if ((Result in [FS_FILE_OK]) and (EmptyWideStr = ResultHash)) then
				ResultHash := CloudHash(FileStream);
			FlushFileBuffers(FileStream.Handle);
		finally
			FileStream.Free;
		end;
	end;
	if Result <> FS_FILE_OK then
		System.SysUtils.DeleteFile(GetUNCFilePath(LocalPath));
end;

function TCloudMailRu.GetHTTPConnection: TCloudMailRuHTTP;
begin
	if not(Assigned(self)) then
		Exit(nil); //Проверка на вызов без инициализации
	if (nil = FHTTPManager) then
	begin
		if not Assigned(FHTTPConnection) then
			FHTTPConnection := TCloudMailRuHTTP.Create(FSettings.ConnectionSettings, FLogger, FProgress);

		Result := FHTTPConnection;
	end
	else
		Result := FHTTPManager.get(GetCurrentThreadID());
	Result.AuthCookie := FCookieManager;
	if EmptyWideStr <> FAuthToken then
		Result.HTTP.Request.CustomHeaders.Values['X-CSRF-Token'] := FAuthToken;
end;

function TCloudMailRu.GetPublicLink: WideString;
begin
	if FPublicLink <> '' then
		Exit(FPublicLink); {Already have a public link}

	if IsPublicAccount and (FSettings.AccountSettings.PublicUrl <> EmptyWideStr) then
	begin
		FPublicLink := FSettings.AccountSettings.PublicUrl;
		FSettings.AccountSettings.PublicUrl := IncludeSlash(FSettings.AccountSettings.PublicUrl);
		Delete(FPublicLink, 1, length(PUBLIC_ACCESS_URL));
		if (FPublicLink <> EmptyWideStr) and (FPublicLink[length(FPublicLink)] = '/') then
			Delete(FPublicLink, length(FPublicLink), 1);
	end;
	Exit(FPublicLink)
end;

function TCloudMailRu.GetPublishedFileStreamUrl(FileIdentity: TCMRDirItem; var StreamUrl: WideString; ShardType: WideString = SHARD_TYPE_WEBLINK_VIDEO; Publish: Boolean = CLOUD_PUBLISH): Boolean;
var
	shard_url: WideString;
begin
	Result := False;
	if (EmptyWideStr = FileIdentity.weblink) then //publish and fill weblink, if required
	begin
		if (not Publish) or (not PublishFile(FileIdentity.Home, FileIdentity.weblink)) then
			Exit;
	end;

	if not GetShard(shard_url, ShardType) then
		Exit;
	StreamUrl := Format('%s0p/%s.m3u8?double_encode=1', [shard_url, DCPbase64.Base64EncodeStr(String(RawByteString(FileIdentity.weblink)))]); //UTF2Ansi is required
	Result := true;
end;

function TCloudMailRu.GetShard(var Shard: WideString; ShardType: WideString = SHARD_TYPE_GET): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	Result := HTTP.PostForm(API_DISPATCHER + '?' + FUnitedParams, '', JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_SHARD_RECEIVE);
	if Result then
	begin
		Result := JSONHelper.GetShard(JSON, Shard, ShardType) and (Shard <> EmptyWideStr);
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PREFIX_SHARD_RECEIVED, [Shard, ShardType]);
	end;
end;

function TCloudMailRu.RefreshCSRFToken: Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	HTTP.GetPage(API_CSRF, JSON, Progress);
	Result := getBodyToken(JSON, FAuthToken);
	if Result then
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, TOKEN_UPDATED)
	else
		FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_TOKEN_UPDATE)
end;

function TCloudMailRu.InitSharedConnectionParameters(): Boolean;
var
	PageContent: WideString;
	Progress: Boolean;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	Progress := False;
	Result := HTTP.GetPage(FSettings.AccountSettings.PublicUrl, PageContent, Progress);
	if Result then
	begin
		if not extractPublicShard(PageContent, FPublicShard) then
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_GET_PUBLIC_SHARE);
			Exit(False);
		end;
	end;
end;

function TCloudMailRu.GetUserSpace(var SpaceInfo: TCMRSpace): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	Progress := False;
	Result := HTTP.GetPage(Format('%s?%s', [API_USER_SPACE, FUnitedParams]), JSON, Progress);
	if Result then
	begin
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_GET_USER_SPACE) and SpaceInfo.FromJSON(JSON);
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := GetUserSpace(SpaceInfo)
	end;
end;

function TCloudMailRu.Login(Method: Integer): Boolean;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	HTTP.SetProgressNames(LOGIN_IN_PROGRESS, EmptyWideStr);
	if IsPublicAccount then
		Exit(LoginShared());
	Exit(LoginRegular(Method)); {If not a public account}
end;

{Delegates authentication to the injected IAuthStrategy.
 The strategy is responsible for obtaining auth tokens and setting up connection parameters.
 Method parameter is ignored - the injected strategy determines the auth method.}
function TCloudMailRu.LoginRegular(Method: Integer): Boolean;
var
	Credentials: TAuthCredentials;
	AuthResult: TAuthResult;
begin
	Result := False;

	FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, Format(LOGIN_TO, [Email]));

	{Build credentials from account settings}
	Credentials := TAuthCredentials.Create(Email, Password, FUser, FDomain);

	{Delegate to auth strategy}
	AuthResult := FAuthStrategy.Authenticate(Credentials, HTTP, FLogger);

	if AuthResult.Success then
	begin
		{Apply auth result to connection state}
		FAuthToken := AuthResult.AuthToken;
		FOAuthToken := AuthResult.OAuthToken;
		FUnitedParams := AuthResult.UnitedParams;
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, CONNECTED_TO, [Email]);
		LogUserSpaceInfo;
		Result := True;
	end;
end;

function TCloudMailRu.LoginShared(Method: Integer): Boolean;
begin
	FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, URL_OPEN, [FSettings.AccountSettings.PublicUrl]);
	Exit(InitSharedConnectionParameters());
end;

procedure TCloudMailRu.LogUserSpaceInfo;
var
	US: TCMRSpace;
	QuotaInfo: WideString;
begin
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit;
	if GetUserSpace(US) then
	begin
		if (US.overquota) then
			QuotaInfo := WARN_QUOTA_EXHAUSTED
		else
			QuotaInfo := EmptyWideStr;
		FLogger.Log(LOG_LEVEL_FILE_OPERATION, MSGTYPE_DETAILS, USER_SPACE_INFO, [FormatSize(US.total), FormatSize(US.used), FormatSize(US.total - US.used), QuotaInfo]);
	end else begin
		FLogger.Log(LOG_LEVEL_DEBUG, MSGTYPE_IMPORTANTERROR, ERR_GET_USER_SPACE, [Email]);
	end;
end;

function TCloudMailRu.MoveFile(OldName, ToPath: WideString): Integer;
var
	JSON: WideString;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);
	if HTTP.PostForm(API_FILE_MOVE + '?' + FUnitedParams, Format('home=%s&folder=%s&conflict', [PathToUrl(OldName), PathToUrl(ToPath)]), JSON) then
		Result := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_MOVE);
	if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
		Result := MoveFile(OldName, ToPath);
end;

function TCloudMailRu.FileMove(OldName, NewName: WideString): Integer;
var
	NewPath: WideString;
	SameDir, SameName: Boolean;
begin //К сожалению, переименование и перемещение в облаке - разные действия
	NewPath := ExtractFilePath(NewName);
	SameDir := ExtractFilePath(OldName) = ExtractFilePath(NewName);
	SameName := ExtractFileName(OldName) = ExtractFileName(NewName);
	if SameDir then
	begin //один каталог
		Result := RenameFile(OldName, ExtractFileName(NewName));
	end else begin
		Result := MoveFile(OldName, ExtractFilePath(NewName)); //Если файл со старым именем лежит в новом каталоге, вернётся ошибка. Так реализовано в облаке, а мудрить со временными каталогами я не хочу
		if Result <> CLOUD_OPERATION_OK then
			Exit;
		if not(SameName) then
		begin //скопированный файл лежит в новом каталоге со старым именем
			Result := RenameFile(Format('%s%s', [NewPath, ExtractFileName(OldName)]), ExtractFileName(NewName));
		end;
	end;
end;

function TCloudMailRu.PublishFile(Path: WideString; var PublicLink: WideString; Publish: Boolean): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit;
	if Publish then
	begin
		Result := HTTP.PostForm(API_FILE_PUBLISH + '?' + FUnitedParams, Format('home=/%s&conflict', [PathToUrl(Path)]), JSON, 'application/x-www-form-urlencoded', true, False);
	end else begin
		Result := HTTP.PostForm(API_FILE_UNPUBLISH + '?' + FUnitedParams, Format('weblink=%s&conflict', [PublicLink]), JSON, 'application/x-www-form-urlencoded', true, False);
	end;

	if Result then
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_FILE_PUBLISH);

	if Result and Publish then
		Result := JSONHelper.GetPublicLink(JSON, PublicLink);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := PublishFile(Path, PublicLink, Publish);
end;

function TCloudMailRu.GetShareInfo(Path: WideString; var InviteListing: TCMRInviteList): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	Progress := False;
	if HTTP.GetPage(Format('%s?home=%s&%s', [API_FOLDER_SHARED_INFO, PathToUrl(Path), FUnitedParams]), JSON, Progress) then
	begin
		Result := InviteListing.FromJSON(JSON);
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := GetShareInfo(Path, InviteListing);
	end;
end;

function TCloudMailRu.ShareFolder(Path, Email: WideString; Access: Integer): Boolean;
var
	JSON: WideString;
	access_string: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if Access in [CLOUD_SHARE_RW, CLOUD_SHARE_RO] then
	begin
		if Access = CLOUD_SHARE_RW then
			access_string := CLOUD_SHARE_ACCESS_READ_WRITE
		else
			access_string := CLOUD_SHARE_ACCESS_READ_ONLY;

		Result := HTTP.PostForm(API_FOLDER_SHARE + '?' + FUnitedParams, Format('home=/%s&invite={"email":"%s","access":"%s"}', [PathToUrl(Path), Email, access_string]), JSON)
	end else begin
		Result := HTTP.PostForm(API_FOLDER_UNSHARE + '?' + FUnitedParams, Format('home=/%s&invite={"email":"%s"}', [PathToUrl(Path), Email]), JSON);
	end;
	if Result then
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_INVITE_MEMBER);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := ShareFolder(Path, Email, Access);
end;

function TCloudMailRu.TrashbinRestore(Path: WideString; RestoreRevision: Integer; ConflictMode: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit;
	Result := HTTP.PostForm(API_TRASHBIN_RESTORE + '?' + FUnitedParams, Format('path=%s&restore_revision=%d&conflict=%s', [PathToUrl(Path), RestoreRevision, ConflictMode]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_FILE_RESTORE);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := TrashbinRestore(Path, RestoreRevision, ConflictMode);
end;

function TCloudMailRu.TrashbinEmpty(): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit;

	Result := HTTP.PostForm(API_TRASHBIN_EMPTY + '?' + FUnitedParams, '', JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_TRASH_CLEAN);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := TrashbinEmpty();
end;

function TCloudMailRu.MountFolder(Home, InviteToken, ConflictMode: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit;
	Result := HTTP.PostForm(API_FOLDER_MOUNT + '?' + FUnitedParams, Format('home=%s&invite_token=%s&conflict=%s', [UrlEncode(Home), InviteToken, ConflictMode]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_FOLDER_MOUNT);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := MountFolder(Home, InviteToken, ConflictMode);
end;

function TCloudMailRu.UnmountFolder(Home: WideString; Clone_copy: Boolean): Boolean;
var
	JSON: WideString;
	CopyStr: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit;
	if Clone_copy then
		CopyStr := 'true'
	else
		CopyStr := 'false';
	Result := HTTP.PostForm(API_FOLDER_UNMOUNT + '?' + FUnitedParams, Format('home=%s&clone_copy=%s', [UrlEncode(Home), CopyStr]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_FOLDER_UNMOUNT);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := UnmountFolder(Home, Clone_copy);
end;

function TCloudMailRu.RejectInvite(InviteToken: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit;
	Result := HTTP.PostForm(API_INVITE_REJECT + '?' + FUnitedParams, Format('invite_token=%s', [InviteToken]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_INVITE_REJECT);
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := RejectInvite(InviteToken);
end;

function TCloudMailRu.PutFileStream(FileName, RemotePath: WideString; FileStream: TStream; ConflictMode: WideString): Integer;
var
	LocalFileIdentity, RemoteFileIdentity: TCMRFileIdentity;
	OperationResult: Integer;
	MemoryStream: TMemoryStream;
	UseHash: Boolean;
begin

	Result := FS_FILE_WRITEERROR;
	OperationResult := CLOUD_OPERATION_FAILED;

	UseHash := PrecalculateHash or (ForcePrecalculateSize >= FileStream.size); //issue #231

	if UseHash or CheckCRC then
	begin
		LocalFileIdentity.Hash := CloudHash(FileStream);
		LocalFileIdentity.size := FileStream.size;
	end;
	if UseHash and (LocalFileIdentity.Hash <> EmptyWideStr) and (not FDoCryptFiles) and (FS_FILE_OK = AddFileByIdentity(LocalFileIdentity, RemotePath, CLOUD_CONFLICT_STRICT, False, true)) then {issue #135}
		Exit(CLOUD_OPERATION_OK);

	try
		if FDoCryptFiles then {Will encrypt any type of data passed here}
		begin
			MemoryStream := TMemoryStream.Create;
			try
				FCipher.CryptStream(FileStream, MemoryStream);
				MemoryStream.Position := 0;
				OperationResult := PutFileToCloud(FileName, MemoryStream, RemoteFileIdentity);
			finally
				MemoryStream.Free;
			end;
		end else begin
			OperationResult := PutFileToCloud(FileName, FileStream, RemoteFileIdentity)
		end;
	except
		on E: Exception do
		begin
			if E.ClassName = 'EAbort' then
			begin
				Result := FS_FILE_USERABORT;
			end else begin
				FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_UPLOAD_INFO, [E.ClassName, E.Message]);
				Result := FS_FILE_WRITEERROR;
			end;
		end;
	end;
	if OperationResult = CLOUD_OPERATION_OK then
	begin
		if CheckCRC then
		begin
			if not LocalFileIdentity.IsEqualTo(RemoteFileIdentity) then {При включённой проверке CRC сравниваем хеши и размеры}
				Result := CLOUD_OPERATION_FAILED;

		end;
	end else if OperationResult = CLOUD_OPERATION_CANCELLED then
	begin
		Result := FS_FILE_USERABORT;
	end;

	if OperationResult = CLOUD_OPERATION_OK then
		Result := AddFileByIdentity(RemoteFileIdentity, RemotePath, ConflictMode, False); //Не логируем HTTP-ошибку, она распарсится и обработается уровнем выше
end;

function TCloudMailRu.PutFileWhole(LocalPath, RemotePath, ConflictMode: WideString): Integer;
var
	FileStream: TBufferedFileStream;
begin
	FileStream := TBufferedFileStream.Create(GetUNCFilePath(LocalPath), fmOpenRead or fmShareDenyWrite);
	try
		Result := PutFileStream(ExtractFileName(RemotePath), RemotePath, FileStream, ConflictMode); {putFileStream может обойтись без параметра имени - оно всегда берётся из remotePath}
	finally
		FileStream.Free;
	end;
end;

{$WARN NO_RETVAL OFF}
(*
 The W1035 compiler warning could be a false positive in this case
 BUT this code needs to be covered with tests and perhaps refactored due its length and complexity.
*)

function TCloudMailRu.PutFileSplit(LocalPath, RemotePath, ConflictMode: WideString; ChunkOverwriteMode: Integer): Integer;
var
	LocalFileIdentity: TCMRFileIdentity;
	SplitFileInfo: TFileSplitInfo;
	SplittedPartIndex: Integer;
	ChunkRemotePath, CRCRemotePath: WideString;
	ChunkStream: TChunkedFileStream;
	CRCStream: TStringStream;
	RetryAttemptsCount: Integer;
	UseHash: Boolean;
begin
	UseHash := PrecalculateHash or (ForcePrecalculateSize >= SizeOfFile(LocalPath)); //issue #231
	if UseHash then //try to add whole file by hash at first.
		LocalFileIdentity := FileIdentity(GetUNCFilePath(LocalPath));
	{Отмена расчёта хеша приведёт к отмене всей операции: TC запоминает нажатие отмены и ExternalProgressProc будет возвращать 1 до следующего вызова копирования}
	if UseHash and (LocalFileIdentity.Hash <> EmptyWideStr) and (not FDoCryptFiles) and (FS_FILE_OK = AddFileByIdentity(LocalFileIdentity, RemotePath, CLOUD_CONFLICT_STRICT, False, true)) then {issue #135}
		Exit(CLOUD_OPERATION_OK);

	SplitFileInfo := TFileSplitInfo.Create(GetUNCFilePath(LocalPath), CloudMaxFileSize); //quickly get information about file parts
	try
		RetryAttemptsCount := 0;
		SplittedPartIndex := 0;

		while SplittedPartIndex < SplitFileInfo.ChunksCount do {use while instead for..loop, need to modify loop counter sometimes}
		begin
			ChunkRemotePath := Format('%s%s', [ExtractFilePath(RemotePath), SplitFileInfo.GetChunks[SplittedPartIndex].name]);
			HTTP.SetProgressNames(LocalPath, ChunkRemotePath);
			FLogger.Log(LOG_LEVEL_DEBUG, MSGTYPE_DETAILS, PARTIAL_UPLOAD_INFO, [LocalPath, (SplittedPartIndex + 1), SplitFileInfo.ChunksCount, ChunkRemotePath]);
			ChunkStream := TChunkedFileStream.Create(GetUNCFilePath(LocalPath), fmOpenRead or fmShareDenyWrite, SplitFileInfo.GetChunks[SplittedPartIndex].start, SplitFileInfo.GetChunks[SplittedPartIndex].size);
			try
				Result := PutFileStream(ExtractFileName(ChunkRemotePath), ChunkRemotePath, ChunkStream, ConflictMode);
			finally
				ChunkStream.Free;
			end;

			case Result of
			FS_FILE_OK:
				begin
					RetryAttemptsCount := 0;
				end;
			FS_FILE_USERABORT:
				begin
					FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PARTIAL_UPLOAD_ABORTED);
					Break;
				end;
			FS_FILE_EXISTS:
				begin
					case ChunkOverwriteMode of
						ChunkOverwrite: //silently overwrite chunk
							begin
								FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_DETAILS, CHUNK_OVERWRITE, [ChunkRemotePath]);
								if not(DeleteFile(ChunkRemotePath)) then
								begin
									Result := FS_FILE_WRITEERROR;
									Break;
								end else begin
									Dec(SplittedPartIndex); //retry with this chunk
								end;
							end;
						ChunkOverwriteIgnore: //ignore this chunk
							begin
								FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_DETAILS, CHUNK_SKIP, [ChunkRemotePath]); //ignore and continue
							end;
						ChunkOverwriteAbort: //abort operation
							begin
								FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_DETAILS, CHUNK_ABORT, [ChunkRemotePath]);
								Result := FS_FILE_NOTSUPPORTED;
								Break;
							end;
					end;
				end;
			else {any other error}
				begin
					case OperationErrorMode of
						OperationErrorModeAsk:
							begin
								case (MsgBox(ERR_PARTIAL_UPLOAD_ASK, [Result, ChunkRemotePath], ERR_UPLOAD, MB_ABORTRETRYIGNORE + MB_ICONERROR)) of
									ID_ABORT:
										begin
											Result := FS_FILE_USERABORT;
											Break;
										end;
									ID_RETRY:
										Dec(SplittedPartIndex); //retry with this chunk
									ID_IGNORE:
										begin {do nothing && continue}
										end;
								end;
							end;
						OperationErrorModeIgnore:
							begin
								FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_IGNORE, [Result]);
							end;
						OperationErrorModeAbort:
							begin
								FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_ABORT, [Result]);
								Result := FS_FILE_USERABORT;
								Break;
							end;
						OperationErrorModeRetry:
							begin
								Inc(RetryAttemptsCount);
								if RetryAttemptsCount <> RetryAttempts + 1 then
								begin
									FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_RETRY, [Result, RetryAttemptsCount, RetryAttempts]);
									Dec(SplittedPartIndex); //retry with this chunk
									ProcessMessages;
									Sleep(AttemptWait);
								end else begin
									FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_PARTIAL_UPLOAD_RETRY_EXCEED, [Result]);
									Result := CLOUD_OPERATION_FAILED;
									Break;
								end;
							end
						else {unknown option value}
							begin
								Result := CLOUD_OPERATION_FAILED;
								Break;
							end;
					end
				end;
		end;
			Inc(SplittedPartIndex); //all ok, continue with next chunk
		end; {end while}

		if Result = FS_FILE_OK then {Only after successful upload}
		begin
			CRCRemotePath := ExtractFilePath(RemotePath) + SplitFileInfo.CRCFileName;
			HTTP.TargetName := CRCRemotePath;
			CRCStream := TStringStream.Create;
			try
				SplitFileInfo.GetCRCData(CRCStream);
				PutFileStream(SplitFileInfo.CRCFileName, CRCRemotePath, CRCStream, ConflictMode);
			finally
				CRCStream.Free;
			end;
		end;
	finally
		SplitFileInfo.Free;
	end;
	Exit(FS_FILE_OK); //Файлик залит по частям, выходим
end;
{$WARN NO_RETVAL ON}

{Wrapper for putFileWhole/putFileSplit}
function TCloudMailRu.PutFile(LocalPath, RemotePath: WideString; ConflictMode: WideString = CLOUD_CONFLICT_STRICT; ChunkOverwriteMode: Integer = 0): Integer;
begin
	if not(Assigned(self)) then
		Exit(FS_FILE_WRITEERROR); //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit(FS_FILE_NOTSUPPORTED);
	HTTP.SetProgressNames(LocalPath, RemotePath);
	if (not(UnlimitedFileSize)) and (SizeOfFile(GetUNCFilePath(LocalPath)) > CloudMaxFileSize) then
	begin
		if SplitLargeFiles then
		begin
			FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, SPLIT_LARGE_FILE, [CloudMaxFileSize]);
			Exit(PutFileSplit(LocalPath, RemotePath, ConflictMode, ChunkOverwriteMode));
		end else begin
			FLogger.Log(LOG_LEVEL_WARNING, MSGTYPE_IMPORTANTERROR, SPLIT_LARGE_FILE_IGNORE, [CloudMaxFileSize]);
			Exit(FS_FILE_NOTSUPPORTED);
		end;
	end;

	Result := PutFileWhole(LocalPath, RemotePath, ConflictMode);
end;

function TCloudMailRu.PutFileToCloud(FileName: WideString; FileStream: TStream; var FileIdentity: TCMRFileIdentity): Integer;
var
	PostAnswer: WideString;
	UploadUrl: WideString;
	DispatcherResponse: WideString;
	Progress: Boolean;
begin
	FileIdentity.Hash := EmptyWideStr;
	FileIdentity.size := -1;
	Result := CLOUD_OPERATION_FAILED;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit;
	if (EmptyWideStr = FUploadShard) then
	begin
		FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, UNDEFINED_UPLOAD_SHARD);
		if (EmptyWideStr <> UploadShardOverride) then
		begin
			FLogger.Log(LOG_LEVEL_ERROR, MSGTYPE_DETAILS, UPLOAD_URL_OVERRIDDEN);
			FUploadShard := UploadShardOverride;
		end else begin
			{OAuth uses different dispatcher endpoint that returns plain text URL}
			Progress := False;
			if HTTP.GetPage(Format('%s/u?token=%s', [OAUTH_DISPATCHER_URL, FOAuthToken.access_token]), DispatcherResponse, Progress) then
			begin
				{Response format: "URL IP COUNT", extract the URL (first word)}
				FUploadShard := Trim(Copy(DispatcherResponse, 1, Pos(' ', DispatcherResponse) - 1));
				FLogger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, PREFIX_SHARD_RECEIVED, [FUploadShard, SHARD_TYPE_UPLOAD]);
			end;
		end
	end;

	{OAuth requires only client_id and token parameters for upload authentication}
	UploadUrl := Format('%s?client_id=%s&token=%s', [FUploadShard, OAUTH_CLIENT_ID, FOAuthToken.access_token]);
	Result := HTTP.PutFile(UploadUrl, FileName, FileStream, PostAnswer);
	if (CLOUD_ERROR_TOKEN_OUTDATED = Result) and RefreshCSRFToken() then
		Result := PutFileToCloud(FileName, FileStream, FileIdentity);
	if (Result = CLOUD_OPERATION_OK) then
	begin
		if length(PostAnswer) <> SHA1_HEX_LENGTH then
		begin
			Result := CLOUD_OPERATION_FAILED;
		end else begin
			FileIdentity.Hash := PostAnswer;
			FileIdentity.size := FileStream.size;
		end;
	end;
end;

function TCloudMailRu.RemoveDir(Path: WideString): Boolean;
var
	JSON: WideString;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit;
	HTTP.SetProgressNames(DELETE_DIR, Path);
	Result := HTTP.PostForm(API_FILE_REMOVE + '?' + FUnitedParams, Format('home=/%s&conflict', [IncludeSlash(PathToUrl(Path))]), JSON) and CloudResultToBoolean(JSON, PREFIX_ERR_DELETE_DIR); //API всегда отвечает true, даже если путь не существует
	if (not Result and (NAME_TOKEN = getBodyError(JSON))) and RefreshCSRFToken() then
		Result := RemoveDir(Path);
end;

function TCloudMailRu.RenameFile(OldName, NewName: WideString): Integer;
var
	JSON: WideString;
begin
	Result := FS_FILE_WRITEERROR;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	if IsPublicAccount then
		Exit;
	if HTTP.PostForm(API_FILE_RENAME + '?' + FUnitedParams, Format('home=%s&name=%s', [PathToUrl(OldName), PathToUrl(NewName)]), JSON) then
		Result := CloudResultToFsResult(JSON, PREFIX_ERR_FILE_RENAME);
	if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
		Result := RenameFile(OldName, NewName);
end;

function TCloudMailRu.StatusFile(Path: WideString; var FileInfo: TCMRDirItem): Boolean;
var
	JSON: WideString;
	Progress: Boolean;
begin
	Result := False;
	if not(Assigned(self)) then
		Exit; //Проверка на вызов без инициализации
	Progress := False;
	if IsPublicAccount then
		Result := HTTP.GetPage(Format('%s?weblink=%s%s&%s', [API_FILE, IncludeSlash(GetPublicLink), PathToUrl(Path), FUnitedParams]), JSON, Progress)
	else
		Result := HTTP.GetPage(Format('%s?home=%s&%s', [API_FILE, PathToUrl(Path), FUnitedParams]), JSON, Progress);
	if Result then
	begin
		Result := CloudResultToBoolean(JSON, PREFIX_ERR_FILE_STATUS) and FileInfo.FromJSON(JSON);
	end else begin
		if (NAME_TOKEN = getBodyError(JSON)) and RefreshCSRFToken() then
			Result := StatusFile(Path, FileInfo);
	end;

end;

class function TCloudMailRu.CloudAccessToString(Access: WideString; Invert: Boolean): WideString;
begin
	if Access = 'read only' then
		Access := CLOUD_SHARE_ACCESS_READ_ONLY;
	if Access = 'read and write' then
		Access := CLOUD_SHARE_ACCESS_READ_WRITE;
	if Invert then
	begin
		if (Access = CLOUD_SHARE_ACCESS_READ_ONLY) then
			Access := CLOUD_SHARE_ACCESS_READ_WRITE
		else
			Access := CLOUD_SHARE_ACCESS_READ_ONLY;
	end;
	if Access = CLOUD_SHARE_ACCESS_READ_ONLY then
		Result := 'read only'
	else
		Result := 'read and write';
end;

class function TCloudMailRu.StringToCloudAccess(AccessString: WideString; Invert: Boolean): Integer;
begin
	if AccessString = 'read only' then
		AccessString := CLOUD_SHARE_ACCESS_READ_ONLY;
	if AccessString = 'read and write' then
		AccessString := CLOUD_SHARE_ACCESS_READ_WRITE;
	if Invert then
	begin
		if (AccessString = CLOUD_SHARE_ACCESS_READ_ONLY) then
			AccessString := CLOUD_SHARE_ACCESS_READ_WRITE
		else
			AccessString := CLOUD_SHARE_ACCESS_READ_ONLY;
	end;
	if AccessString = CLOUD_SHARE_ACCESS_READ_ONLY then
		Result := CLOUD_SHARE_RO
	else
		Result := CLOUD_SHARE_RW;
end;

class function TCloudMailRu.TempPublicCloudInit(var TempCloud: TCloudMailRu; PublicUrl: WideString): Boolean;
var
	TempCloudSettings: TCloudSettings;
begin
	TempCloudSettings := default (TCloudSettings);
	TempCloudSettings.AccountSettings.PublicAccount := true;
	TempCloudSettings.AccountSettings.PublicUrl := PublicUrl;
	TempCloud := TCloudMailRu.Create(TempCloudSettings, nil, TNullAuthStrategy.Create, TNullLogger.Create, TNullProgress.Create, TNullRequest.Create);
	Result := TempCloud.Login;
end;

function TCloudMailRu.CloudHash(Path: WideString): WideString;
var
	Stream: TStream;
begin
	Result := EmptyWideStr;
	if not FileExists(Path) then
		Exit;

	try
		Stream := TBufferedFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
	except
		Exit;
	end;
	try
		Result := CloudHash(Stream, GetLFCFilePath(Path));
	finally
		Stream.Free;
	end;
end;

function TCloudMailRu.CloudHash(Stream: TStream; Path: WideString = CALCULATING_HASH): WideString;
const
	{Cloud Mail.ru hash algorithm constants}
	HASH_SEED = 'mrCloud';
	SMALL_FILE_THRESHOLD = 21;
	SMALL_FILE_BUFFER = 20;
	BUFFER_SIZE = 8192;
var
	sha1: THashSHA1;
	buffer: array [0 .. BUFFER_SIZE - 1] of byte;
	read, iteration, processedBytes: Int64;
	initBuffer, finalBuffer: TBytes;
	Percent: Integer;
	Aborted: Boolean;
begin
	Stream.Position := 0;
	Result := EmptyWideStr;
	if Stream.size < SMALL_FILE_THRESHOLD then
	begin
		SetLength(initBuffer, SMALL_FILE_BUFFER);
		Stream.read(initBuffer, Stream.size);
		Result := UpperCase(THash.DigestAsString(initBuffer));
		Exit;
	end;

	FillChar(buffer, sizeof(buffer), 0);
	initBuffer := TEncoding.UTF8.GetBytes(HASH_SEED);

	sha1 := THashSHA1.Create;
	sha1.Update(initBuffer, length(initBuffer));
	iteration := 0;
	repeat
		iteration := iteration + 1;
		processedBytes := BUFFER_SIZE * iteration;
		Percent := Round((processedBytes / Stream.size) * 100);
		if Percent > 100 then
			Percent := 100;

		read := Stream.read(buffer, BUFFER_SIZE);
		sha1.Update(buffer, read);
		Aborted := FProgress.Progress(Path, CALCULATING_HASH, Percent);
	until (read < sizeof(buffer)) or Aborted;

	finalBuffer := TEncoding.UTF8.GetBytes(Stream.size.ToString);
	sha1.Update(finalBuffer, length(finalBuffer));
	if (not Aborted) then
		Result := UpperCase(sha1.HashAsString);
	sha1.Reset;
end;

end.
