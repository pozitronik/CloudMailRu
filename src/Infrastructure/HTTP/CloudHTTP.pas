unit CloudHTTP;

{Interface and implementation for HTTP operations.
	ICloudHTTP enables testability without real HTTP connections.
	TCloudMailRuHTTP implements this interface for production use.
	TNullCloudHTTP provides a null implementation for testing.}

interface

uses
	System.SysUtils,
	System.Classes,
	System.Generics.Collections,
	ChunkedFileStream,
	FileSplitInfo,
	TCLogger,
	TCProgress,
	SETTINGS_CONSTANTS,
	PLUGIN_TYPES,
	CMRConstants,
	LANGUAGE_STRINGS,
	ParsingHelper,
	JSONHelper,
	IdStack,
	IdCookieManager,
	IdIOHandler,
	IdIOHandlerSocket,
	IdIOHandlerStack,
	IdSSL,
	IdSSLOpenSSL,
	IdBaseComponent,
	IdComponent,
	IdTCPConnection,
	IdTCPClient,
	IdSocks,
	IdHTTP,
	IdAuthentication,
	IdIOHandlerStream,
	IdInterceptThrottler,
	IdCookie,
	IdMultipartFormData,
	ConnectionSettings;

type
	{Interface for cloud HTTP operations}
	ICloudHTTP = interface
		['{B4F9BB0D-F019-480A-A6DE-C6DA169EDBBF}']

		{GET request returning page content}
		function GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;

		{GET request to download file to stream}
		function GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean = True): Integer;

		{GET request following redirects}
		function GetRedirection(URL: WideString; var RedirectionURL: WideString; var ProgressEnabled: Boolean): Boolean;

		{POST form data}
		function PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = True; ProgressEnabled: Boolean = True): Boolean;

		{POST multipart form data}
		function PostMultipart(URL: WideString; Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean;

		{POST file stream}
		function PostFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;

		{PUT file stream}
		function PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;

		{HEAD request}
		procedure Head(URL: WideString);

		{Set progress display names}
		procedure SetProgressNames(SourceName, TargetName: WideString);

		{Cookie manager access}
		procedure SetAuthCookie(Value: TIdCookieManager);

		{Access to underlying HTTP for header manipulation}
		function GetHTTP: TIdHTTP;

		property AuthCookie: TIdCookieManager write SetAuthCookie;
		property HTTP: TIdHTTP read GetHTTP;
	end;

	{Null implementation for testing - all operations return failure/empty}
	TNullCloudHTTP = class(TInterfacedObject, ICloudHTTP)
	public
		function GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
		function GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean = True): Integer;
		function GetRedirection(URL: WideString; var RedirectionURL: WideString; var ProgressEnabled: Boolean): Boolean;
		function PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = True; ProgressEnabled: Boolean = True): Boolean;
		function PostMultipart(URL: WideString; Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean;
		function PostFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
		function PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
		procedure Head(URL: WideString);
		procedure SetProgressNames(SourceName, TargetName: WideString);
		procedure SetAuthCookie(Value: TIdCookieManager);
		function GetHTTP: TIdHTTP;
	end;

	{Production HTTP implementation using Indy}
	TCloudMailRuHTTP = class(TInterfacedObject, ICloudHTTP)
	private
		{VARIABLES}
		ExternalSourceName: WideString;
		ExternalTargetName: WideString;

		SSL: TIdSSLIOHandlerSocketOpenSSL;
		Socks: TIdSocksInfo;
		Throttle: TIdInterceptThrottler;
		Settings: TConnectionSettings;

		Logger: ILogger;
		Progress: IProgress;

		{PROCEDURES}
		procedure SetCookie(const Value: TIdCookieManager);
		procedure SetExternalSourceName(const Value: WideString);
		procedure SetExternalTargetName(const Value: WideString);

		{ICloudHTTP interface methods}
		procedure SetAuthCookie(Value: TIdCookieManager);
		function GetHTTP: TIdHTTP;

	public
		{PROPERTIES}
		HTTP: TIdHTTP;
		Property Options: TConnectionSettings read Settings;
		Property AuthCookie: TIdCookieManager write SetCookie; {Managed externally for auth sharing between connections}
		property SourceName: WideString write SetExternalSourceName;
		property TargetName: WideString write SetExternalTargetName;
		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(Settings: TConnectionSettings; Logger: ILogger; Progress: IProgress);
		destructor Destroy; override;
		{MAIN ROUTINES}
		procedure Head(URL: WideString);

		function GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean; //если ProgressEnabled - включаем обработчик onWork, возвращаем ProgressEnabled=false при отмене
		function GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean = True): Integer;
		function GetRedirection(URL: WideString; var RedirectionURL: WideString; var ProgressEnabled: Boolean): Boolean;

		function PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = True; ProgressEnabled: Boolean = True): Boolean; //Постинг данных с возможным получением ответа.
		function PostMultipart(URL: WideString; Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean;
		function PostFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer; overload; //Постинг потока данных как файла

		function Post(URL: WideString; PostData, ResultData: TStringStream; UnderstandResponseCode: Boolean = false; ContentType: WideString = ''; LogErrors: Boolean = True; ProgressEnabled: Boolean = True): Integer; overload; //Постинг подготовленных данных, отлов ошибок
		function Post(URL: WideString; var PostData: TIdMultiPartFormDataStream; ResultData: TStringStream): Integer; overload; //TIdMultiPartFormDataStream should be passed via var

		function OptionsMethod(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
		function PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
		function Put(URL: WideString; var PostData: TStream; ResultData: TStringStream): Integer;

		function ExceptionHandler(E: Exception; URL: WideString; HTTPMethod: Integer = HTTP_METHOD_POST; LogErrors: Boolean = True): Integer;

		procedure SetProgressNames(SourceName, TargetName: WideString);
		procedure HTTPProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
	end;

implementation

{TNullCloudHTTP}

function TNullCloudHTTP.GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
begin
	Answer := '';
	Result := false;
end;

function TNullCloudHTTP.GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean): Integer;
begin
	Result := FS_FILE_READERROR;
end;

function TNullCloudHTTP.GetRedirection(URL: WideString; var RedirectionURL: WideString; var ProgressEnabled: Boolean): Boolean;
begin
	RedirectionURL := '';
	Result := false;
end;

function TNullCloudHTTP.PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString; LogErrors: Boolean; ProgressEnabled: Boolean): Boolean;
begin
	Answer := '';
	Result := false;
end;

function TNullCloudHTTP.PostMultipart(URL: WideString; Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean;
begin
	Answer := '';
	Result := false;
end;

function TNullCloudHTTP.PostFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
begin
	Answer := '';
	Result := FS_FILE_WRITEERROR;
end;

function TNullCloudHTTP.PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
begin
	Answer := '';
	Result := FS_FILE_WRITEERROR;
end;

procedure TNullCloudHTTP.Head(URL: WideString);
begin
	{No-op}
end;

procedure TNullCloudHTTP.SetProgressNames(SourceName, TargetName: WideString);
begin
	{No-op}
end;

procedure TNullCloudHTTP.SetAuthCookie(Value: TIdCookieManager);
begin
	{No-op}
end;

function TNullCloudHTTP.GetHTTP: TIdHTTP;
begin
	Result := nil;
end;

{TCloudMailRuHTTP}

constructor TCloudMailRuHTTP.Create(Settings: TConnectionSettings; Logger: ILogger; Progress: IProgress);
begin
	self.Progress := Progress;
	self.Logger := Logger;
	self.Throttle := TIdInterceptThrottler.Create();
	self.Socks := TIdSocksInfo.Create();
	SSL := TIdSSLIOHandlerSocketOpenSSL.Create();
	SSL.SSLOptions.SSLVersions := [sslvSSLv23];
	HTTP := TIdHTTP.Create();

	if Settings.ProxySettings.ProxyType in SocksProxyTypes then
	begin
		self.Socks.Host := Settings.ProxySettings.Server;
		self.Socks.Port := Settings.ProxySettings.Port;
		if Settings.ProxySettings.User <> EmptyWideStr then
		begin
			self.Socks.Authentication := saUsernamePassword;
			self.Socks.Username := Settings.ProxySettings.User;
			self.Socks.password := Settings.ProxySettings.password;
		end
		else
			self.Socks.Authentication := saNoAuthentication;

		case Settings.ProxySettings.ProxyType of
			ProxySocks5:
				Socks.Version := svSocks5;
			ProxySocks4:
				Socks.Version := svSocks4;
		end;
		self.Socks.Enabled := True;
		SSL.TransparentProxy := self.Socks;
	end;

	if Settings.ProxySettings.ProxyType = ProxyHTTP then
	begin
		HTTP.ProxyParams.ProxyServer := Settings.ProxySettings.Server;
		HTTP.ProxyParams.ProxyPort := Settings.ProxySettings.Port;
		if Settings.ProxySettings.User <> EmptyWideStr then
		begin
			HTTP.ProxyParams.BasicAuthentication := True;
			HTTP.ProxyParams.ProxyUsername := Settings.ProxySettings.User;
			HTTP.ProxyParams.ProxyPassword := Settings.ProxySettings.password;
		end
	end;

	HTTP.IOHandler := SSL;
	HTTP.AllowCookies := True;
	HTTP.HTTPOptions := [hoForceEncodeParams, hoNoParseMetaHTTPEquiv, hoKeepOrigProtocol, hoTreat302Like303];
	HTTP.HandleRedirects := True;
	if (Settings.SocketTimeout < 0) then
	begin
		HTTP.ConnectTimeout := Settings.SocketTimeout;
		HTTP.ReadTimeout := Settings.SocketTimeout;
	end;
	if (Settings.UploadBPS > 0) or (Settings.DownloadBPS > 0) then
	begin
		self.Throttle.RecvBitsPerSec := Settings.DownloadBPS;
		self.Throttle.SendBitsPerSec := Settings.UploadBPS;
	end;

	HTTP.Request.UserAgent := Settings.UserAgent;
	HTTP.Request.Connection := EmptyWideStr;
end;

destructor TCloudMailRuHTTP.Destroy;
begin
	HTTP.free;
	SSL.free;
	self.Throttle.free;
	self.Socks.free;
	inherited;
end;

function TCloudMailRuHTTP.GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean): Integer;
begin
	Result := FS_FILE_OK;
	try
		HTTP.Intercept := Throttle;
		HTTP.Request.ContentType := 'application/octet-stream';
		HTTP.Response.KeepAlive := True;
		HTTP.OnWork := self.HTTPProgress;
		HTTP.Get(URL, FileStream);
		if (HTTP.RedirectCount = HTTP.RedirectMaximum) and (FileStream.size = 0) then
		begin
			Result := FS_FILE_NOTSUPPORTED;
		end;
	except
		on E: Exception do
		begin
			Result := self.ExceptionHandler(E, URL, HTTP_METHOD_GET, LogErrors);
		end;
	end;
end;

function TCloudMailRuHTTP.GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
begin
	Result := false;
	try
		if ProgressEnabled then
			HTTP.OnWork := self.HTTPProgress //Вызов прогресса ведёт к возможности отменить получение списка каталогов и других операций, поэтому он нужен не всегда
		else
			HTTP.OnWork := nil;
		Answer := HTTP.Get(URL);
		Result := Answer <> EmptyWideStr;
	Except
		on E: Exception do
		begin
			case self.ExceptionHandler(E, URL) of
				CLOUD_ERROR_TOKEN_OUTDATED:
					begin
						Answer := (E as EIdHTTPProtocolException).ErrorMessage; //на протухание токена возвращаем JSON ответа для дальнейшего парсинга в базовом классе
					end;
				CLOUD_OPERATION_CANCELLED:
					begin
						ProgressEnabled := false; //сообщаем об отмене
					end;
				CLOUD_OPERATION_FAILED:
					begin
						case HTTP.ResponseCode of
							HTTP_ERROR_BAD_REQUEST, HTTP_ERROR_OVERQUOTA:
								{Expected API responses - see Post method comments}
								begin
									//Answer := (E as EIdHTTPProtocolException).ErrorMessage; //TODO: нужно протестировать, наверняка тут не json
								end;
						end;
					end;
			end;
		end;
	end;
end;

{Проверяет редирект с указанного адреса}
function TCloudMailRuHTTP.GetRedirection(URL: WideString; var RedirectionURL: WideString; var ProgressEnabled: Boolean): Boolean;
var
	Answer: WideString;
begin
	Result := false;
	HTTP.HandleRedirects := false;
	try
		try
			Answer := HTTP.Get(URL);
		except
			on E: Exception do
			begin
				if (HTTP_FOUND_REDIRECT = HTTP.ResponseCode) then
				begin
					RedirectionURL := HTTP.Response.Location;
					Result := True;
				end else begin
					self.ExceptionHandler(E, URL, HTTP_METHOD_GET);
				end;
			end;
		end;
	finally
		HTTP.HandleRedirects := True;
	end;
end;

procedure TCloudMailRuHTTP.Head(URL: WideString);
begin
	HTTP.Head(URL);
	HTTP.Request.Referer := URL;
end;

function TCloudMailRuHTTP.Post(URL: WideString; PostData, ResultData: TStringStream; UnderstandResponseCode: Boolean; ContentType: WideString; LogErrors, ProgressEnabled: Boolean): Integer;
begin
	Result := CLOUD_OPERATION_OK;
	ResultData.Position := 0;
	try
		if ContentType <> EmptyWideStr then
			HTTP.Request.ContentType := ContentType;
		if ProgressEnabled then
			HTTP.OnWork := self.HTTPProgress
		else
			HTTP.OnWork := nil;
		HTTP.Post(URL, PostData, ResultData);

	except
		on E: Exception do
		begin
			Result := self.ExceptionHandler(E, URL, HTTP_METHOD_POST, LogErrors);
			if UnderstandResponseCode and (E is EIdHTTPProtocolException) then
			begin
				case HTTP.ResponseCode of
					HTTP_ERROR_BAD_REQUEST, HTTP_ERROR_OVERQUOTA:
						{These are "expected" API responses, not transport errors:
							- 400: Often means "hash not found" for deduplication checks (see AddFileByIdentity)
							- 507: Storage quota exceeded
						Write response body so caller can parse the actual API error.}
						begin
							ResultData.WriteString((E as EIdHTTPProtocolException).ErrorMessage);
							Result := CLOUD_OPERATION_OK;
						end;
				end;
			end;

		end;
	end;
end;

function TCloudMailRuHTTP.Post(URL: WideString; var PostData: TIdMultiPartFormDataStream; ResultData: TStringStream): Integer;
begin
	Result := CLOUD_OPERATION_OK;
	ResultData.Position := 0;
	try
		HTTP.Intercept := Throttle;
		HTTP.OnWork := self.HTTPProgress;
		HTTP.Post(URL, PostData, ResultData);
	except
		On E: Exception do
		begin
			Result := self.ExceptionHandler(E, URL);
		end;
	end;
end;

function TCloudMailRuHTTP.PostFile(URL, FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
var
	PostData: TIdMultiPartFormDataStream;
	ResultStream: TStringStream;
begin
	ResultStream := TStringStream.Create;
	try
		PostData := TIdMultiPartFormDataStream.Create;
		try
			PostData.AddFormField('file', 'application/octet-stream', EmptyWideStr, FileStream, FileName);
			Result := self.Post(URL, PostData, ResultStream);
			Answer := ResultStream.DataString;
		finally
			PostData.free;
		end;
	finally
		ResultStream.free;
	end;
end;

function TCloudMailRuHTTP.PostForm(URL, PostDataString: WideString; var Answer: WideString; ContentType: WideString; LogErrors, ProgressEnabled: Boolean): Boolean;
var
	ResultStream, PostData: TStringStream;
	PostResult: Integer;
begin
	ResultStream := TStringStream.Create;
	try
		PostData := TStringStream.Create(PostDataString, TEncoding.UTF8);
		try
			PostResult := self.Post(URL, PostData, ResultStream, True, ContentType, LogErrors, ProgressEnabled);
			Result := PostResult = CLOUD_OPERATION_OK;
			Answer := ResultStream.DataString;
		finally
			PostData.free;
		end;
	finally
		ResultStream.free;
	end;
end;

function TCloudMailRuHTTP.PostMultipart(URL: WideString; Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean;
var
	ResultStream: TStringStream;
	PostData: TIdMultiPartFormDataStream;
	ParamItem: TPair<WideString, WideString>;
	PostResult: Integer;
begin
	ResultStream := TStringStream.Create;
	try
		PostData := TIdMultiPartFormDataStream.Create;
		try
			for ParamItem in Params do
				PostData.AddFormField(ParamItem.Key, ParamItem.Value);

			PostResult := self.Post(URL, PostData, ResultStream);
			Result := PostResult = CLOUD_OPERATION_OK;
			Answer := ResultStream.DataString;
		finally
			PostData.free;
		end;
	finally
		ResultStream.free;
	end;
end;

function TCloudMailRuHTTP.OptionsMethod(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
var
	ResultStream: TStringStream;
begin
	Result := True;
	ResultStream := TStringStream.Create;
	try
		try
			HTTP.Intercept := Throttle;
			HTTP.OnWork := self.HTTPProgress;
			HTTP.Options(URL, ResultStream);
			Answer := ResultStream.DataString;
		except
			On E: Exception do
			begin
				self.ExceptionHandler(E, URL, HTTP_METHOD_OPTIONS);
				Result := false;
			end;
		end;
	finally
		ResultStream.free;
	end;
end;

procedure TCloudMailRuHTTP.HTTPProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
var
	ContentLength: int64;
	Percent: Integer;
begin
	HTTP := TIdHTTP(ASender);
	if AWorkMode = wmRead then
		ContentLength := HTTP.Response.ContentLength
	else
		ContentLength := HTTP.Request.ContentLength; //Считаем размер обработанных данных в зависимости от того, скачивание это или загрузка
	if (Pos('chunked', LowerCase(HTTP.Response.TransferEncoding)) = 0) and (ContentLength > 0) then
	begin
		Percent := 100 * AWorkCount div ContentLength;
		if self.Progress.Progress(self.ExternalSourceName, self.ExternalTargetName, Percent) then {При передаче nil прогресс оставляет предыдущие значения}
			abort;
	end;
end;

function TCloudMailRuHTTP.Put(URL: WideString; var PostData: TStream; ResultData: TStringStream): Integer;
var
	PutAnswer: WideString;
begin
	Result := CLOUD_OPERATION_OK;
	ResultData.Position := 0;
	try
		HTTP.Intercept := Throttle;
		HTTP.OnWork := self.HTTPProgress;
		PutAnswer := HTTP.Put(URL, PostData);
		ResultData.WriteString(PutAnswer);

	except
		On E: Exception do
		begin
			Result := self.ExceptionHandler(E, URL, HTTP_METHOD_PUT);
		end;
	end;
end;

function TCloudMailRuHTTP.PutFile(URL, FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
var
	ResultStream: TStringStream;
begin
	ResultStream := TStringStream.Create;
	try
		Result := self.Put(URL, FileStream, ResultStream);
		Answer := ResultStream.DataString;
	finally
		ResultStream.free;
	end;
end;

procedure TCloudMailRuHTTP.SetCookie(const Value: TIdCookieManager);
begin
	self.HTTP.CookieManager := Value;
end;

procedure TCloudMailRuHTTP.SetExternalSourceName(const Value: WideString);
begin
	self.ExternalSourceName := Value;
end;

procedure TCloudMailRuHTTP.SetExternalTargetName(const Value: WideString);
begin
	self.ExternalTargetName := Value;
end;

procedure TCloudMailRuHTTP.SetProgressNames(SourceName, TargetName: WideString);
begin
	self.ExternalSourceName := SourceName;
	self.ExternalTargetName := TargetName;
end;

procedure TCloudMailRuHTTP.SetAuthCookie(Value: TIdCookieManager);
begin
	self.HTTP.CookieManager := Value;
end;

function TCloudMailRuHTTP.GetHTTP: TIdHTTP;
begin
	Result := self.HTTP;
end;

function TCloudMailRuHTTP.ExceptionHandler(E: Exception; URL: WideString; HTTPMethod: Integer; LogErrors: Boolean): Integer; //todo: handle OPTIONS method
var
	method_string: WideString; //в зависимости от метода исходного запроса меняется текст сообщения
begin
	Result := FS_FILE_OK; //just to avoid compiler warning
	case HTTPMethod of
		HTTP_METHOD_GET:
			begin
				method_string := METHOD_STR_RECEIVE;
				Result := FS_FILE_READERROR; //для GetFile, GetForm не интересует код ошибки
			end;
		HTTP_METHOD_POST, HTTP_METHOD_PUT:
			begin
				method_string := METHOD_STR_POST;
				Result := CLOUD_OPERATION_FAILED; //Для всех Post-запросов
			end;
		HTTP_METHOD_OPTIONS:
			begin
				method_string := METHOD_STR_OPTIONS;
				Result := CLOUD_OPERATION_FAILED; //Для всех Post-запросов
			end;
	end;
	if (E is EIdHTTPProtocolException and (NAME_TOKEN = JSONHelper.getBodyError((E as EIdHTTPProtocolException).ErrorMessage))) then
	begin
		Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, CSRF_UPDATE_REQUIRED, [method_string, URL]);
		exit(CLOUD_ERROR_TOKEN_OUTDATED);
	end;

	if E is EAbort then
	begin
		Result := CLOUD_OPERATION_CANCELLED;
	end else if LogErrors then //разбирать ошибку дальше имеет смысл только для логирования - что вернуть уже понятно
	begin
		if E is EIdHTTPProtocolException then
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_HTTP_GENERAL, [E.ClassName, E.Message, method_string, URL, (E as EIdHTTPProtocolException).ErrorMessage])
		else if E is EIdSocketerror then
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_SOCKET_GENERAL, [E.ClassName, E.Message, method_string, URL])
		else
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_OTHER_GENERAL, [E.ClassName, E.Message, method_string, URL]);;
	end;
end;

end.
