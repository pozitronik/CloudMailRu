unit CloudHTTP;

{Interface and implementation for HTTP operations.
	ICloudHTTP enables testability without real HTTP connections.
	TCloudMailRuHTTP implements this interface for production use.
	TNullCloudHTTP provides a null implementation for testing.}

interface

uses
	System.SysUtils,
	System.Classes,
	ChunkedFileStream,
	FileSplitInfo,
	Logger,
	Progress,
	SettingsConstants,
	WFXTypes,
	CloudConstants,
	LanguageStrings,
	JSONHelper,
	SSLHandlerFactory,
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

		{POST form data}
		function PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = True; ProgressEnabled: Boolean = True): Boolean;

		{PUT file stream}
		function PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;

		{Set progress display names}
		procedure SetProgressNames(SourceName, TargetName: WideString);

		{Replace progress reporter - allows injection of decorators like TScaledProgress}
		procedure SetProgress(Progress: IProgress);

		{Cookie manager access}
		procedure SetAuthCookie(Value: TIdCookieManager);
		function GetAuthCookie: TIdCookieManager;

		{Set CSRF token for authenticated requests}
		procedure SetCSRFToken(const Token: WideString);

		{Access to underlying HTTP for header manipulation}
		function GetHTTP: TIdHTTP;

		property AuthCookie: TIdCookieManager read GetAuthCookie write SetAuthCookie;
		property HTTP: TIdHTTP read GetHTTP;
	end;

	{Null implementation for testing - all operations return failure/empty}
	TNullCloudHTTP = class(TInterfacedObject, ICloudHTTP)
	public
		function GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
		function GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean = True): Integer;
		function PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = True; ProgressEnabled: Boolean = True): Boolean;
		function PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
		procedure SetProgressNames(SourceName, TargetName: WideString);
		procedure SetProgress(Progress: IProgress);
		procedure SetAuthCookie(Value: TIdCookieManager);
		function GetAuthCookie: TIdCookieManager;
		procedure SetCSRFToken(const Token: WideString);
		function GetHTTP: TIdHTTP;
	end;

	{Production HTTP implementation using Indy}
	TCloudMailRuHTTP = class(TInterfacedObject, ICloudHTTP)
	private
		{VARIABLES}
		ExternalSourceName: WideString;
		ExternalTargetName: WideString;

		SSL: TIdSSLIOHandlerSocketBase;
		Throttle: TIdInterceptThrottler;
		Settings: TConnectionSettings;

		Logger: ILogger;
		Progress: IProgress;

		{PROCEDURES}
		function TruncateForLog(const S: WideString): WideString;
		procedure SetCookie(const Value: TIdCookieManager);
		procedure SetExternalSourceName(const Value: WideString);
		procedure SetExternalTargetName(const Value: WideString);

		{ICloudHTTP interface methods}
		procedure SetAuthCookie(Value: TIdCookieManager);
		function GetAuthCookie: TIdCookieManager;
		procedure SetCSRFToken(const Token: WideString);
		function GetHTTP: TIdHTTP;

	public
		{PROPERTIES}
		HTTP: TIdHTTP;
		Property Options: TConnectionSettings read Settings;
		Property AuthCookie: TIdCookieManager read GetAuthCookie write SetCookie; {Managed externally for auth sharing between connections}
		property SourceName: WideString write SetExternalSourceName;
		property TargetName: WideString write SetExternalTargetName;
		{CONSTRUCTOR/DESTRUCTOR}
		constructor Create(Settings: TConnectionSettings; SSLFactory: ISSLHandlerFactory; Logger: ILogger; Progress: IProgress);
		destructor Destroy; override;
		{MAIN ROUTINES}
		function GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean; {If ProgressEnabled, enable onWork handler; returns ProgressEnabled=false on cancel}
		function GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean = True): Integer;

		function PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = True; ProgressEnabled: Boolean = True): Boolean; {Post data with optional response}

		function Post(URL: WideString; PostData, ResultData: TStringStream; UnderstandResponseCode: Boolean = false; ContentType: WideString = ''; LogErrors: Boolean = True; ProgressEnabled: Boolean = True): Integer; overload; {Post prepared data with error handling}
		function Post(URL: WideString; var PostData: TIdMultiPartFormDataStream; ResultData: TStringStream): Integer; overload; {TIdMultiPartFormDataStream should be passed via var}

		function PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
		function Put(URL: WideString; var PostData: TStream; ResultData: TStringStream): Integer;

		function ExceptionHandler(E: Exception; URL: WideString; HTTPMethod: Integer = HTTP_METHOD_POST; LogErrors: Boolean = True): Integer;

		procedure SetProgressNames(SourceName, TargetName: WideString);
		procedure SetProgress(Progress: IProgress);
		procedure HTTPProgress(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
	end;

implementation

const
	MAX_LOG_BODY_LENGTH = 4096; {Truncate response bodies beyond this length in HTTP trace logs}

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

function TNullCloudHTTP.PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString; ContentType: WideString; LogErrors: Boolean; ProgressEnabled: Boolean): Boolean;
begin
	Answer := '';
	Result := false;
end;

function TNullCloudHTTP.PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
begin
	Answer := '';
	Result := FS_FILE_WRITEERROR;
end;

procedure TNullCloudHTTP.SetProgressNames(SourceName, TargetName: WideString);
begin
	{No-op}
end;

procedure TNullCloudHTTP.SetProgress(Progress: IProgress);
begin
	{No-op}
end;

procedure TNullCloudHTTP.SetAuthCookie(Value: TIdCookieManager);
begin
	{No-op}
end;

function TNullCloudHTTP.GetAuthCookie: TIdCookieManager;
begin
	Result := nil;
end;

procedure TNullCloudHTTP.SetCSRFToken(const Token: WideString);
begin
	{No-op}
end;

function TNullCloudHTTP.GetHTTP: TIdHTTP;
begin
	Result := nil;
end;

{TCloudMailRuHTTP}

constructor TCloudMailRuHTTP.Create(Settings: TConnectionSettings; SSLFactory: ISSLHandlerFactory; Logger: ILogger; Progress: IProgress);
begin
	self.Progress := Progress;
	self.Logger := Logger;
	self.Throttle := TIdInterceptThrottler.Create();
	SSL := SSLFactory.CreateHandler;
	SSLFactory.ConfigureSocksProxy(SSL, Settings.ProxySettings);
	HTTP := TIdHTTP.Create();

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
	HTTP.CookieManager := TIdCookieManager.Create(HTTP);
	HTTP.HTTPOptions := [hoForceEncodeParams, hoNoParseMetaHTTPEquiv, hoKeepOrigProtocol, hoTreat302Like303];
	HTTP.HandleRedirects := True;
	if (Settings.SocketTimeout <> 0) then
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
	HTTP.Free;
	SSL.Free;
	Throttle.Free;
	inherited;
end;

function TCloudMailRuHTTP.TruncateForLog(const S: WideString): WideString;
begin
	if Length(S) > MAX_LOG_BODY_LENGTH then
		Result := Copy(S, 1, MAX_LOG_BODY_LENGTH) + '... [truncated]'
	else
		Result := S;
end;

function TCloudMailRuHTTP.GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean): Integer;
begin
	Result := FS_FILE_OK;
	Logger.Log(LOG_LEVEL_HTTP, MSGTYPE_DETAILS, HTTP_REQUEST, ['GET', URL]);
	try
		HTTP.Intercept := Throttle;
		HTTP.Request.ContentType := 'application/octet-stream';
		HTTP.Response.KeepAlive := True;
		HTTP.OnWork := self.HTTPProgress;
		HTTP.Get(URL, FileStream);
		Logger.Log(LOG_LEVEL_HTTP, MSGTYPE_DETAILS, HTTP_RESPONSE, [URL, HTTP.ResponseCode, FileStream.Size]);
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
	Logger.Log(LOG_LEVEL_HTTP, MSGTYPE_DETAILS, HTTP_REQUEST, ['GET', URL]);
	try
		if ProgressEnabled then
			HTTP.OnWork := self.HTTPProgress {Progress callback enables cancellation of listing and other operations, so not always needed}
		else
			HTTP.OnWork := nil;
		Answer := HTTP.Get(URL);
		Logger.Log(LOG_LEVEL_HTTP, MSGTYPE_DETAILS, HTTP_RESPONSE, [URL, HTTP.ResponseCode, Length(Answer)]);
		Logger.Log(LOG_LEVEL_HTTP, MSGTYPE_DETAILS, HTTP_RESPONSE_BODY, [TruncateForLog(Answer)]);
		Result := Answer <> EmptyWideStr;
	Except
		on E: Exception do
		begin
			case self.ExceptionHandler(E, URL) of
				CLOUD_ERROR_TOKEN_OUTDATED:
					begin
						Answer := (E as EIdHTTPProtocolException).ErrorMessage; {Return response JSON for token expiry parsing in the base class}
					end;
				CLOUD_OPERATION_CANCELLED:
					begin
						ProgressEnabled := false; {Signal cancellation}
					end;
			end;
		end;
	end;
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
			{Pass error body on token expiry so retry layer can detect it via IsTokenExpiredInJSON}
			if (Result = CLOUD_ERROR_TOKEN_OUTDATED) and (E is EIdHTTPProtocolException) then
			begin
				ResultData.Size := 0;
				ResultData.WriteString((E as EIdHTTPProtocolException).ErrorMessage);
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

function TCloudMailRuHTTP.PostForm(URL, PostDataString: WideString; var Answer: WideString; ContentType: WideString; LogErrors, ProgressEnabled: Boolean): Boolean;
var
	ResultStream, PostData: TStringStream;
	PostResult: Integer;
begin
	Logger.Log(LOG_LEVEL_HTTP, MSGTYPE_DETAILS, HTTP_REQUEST, ['POST', URL]);
	ResultStream := TStringStream.Create;
	try
		PostData := TStringStream.Create(PostDataString, TEncoding.UTF8);
		try
			PostResult := self.Post(URL, PostData, ResultStream, True, ContentType, LogErrors, ProgressEnabled);
			Result := PostResult = CLOUD_OPERATION_OK;
			Answer := ResultStream.DataString;
			Logger.Log(LOG_LEVEL_HTTP, MSGTYPE_DETAILS, HTTP_RESPONSE, [URL, HTTP.ResponseCode, Length(Answer)]);
			Logger.Log(LOG_LEVEL_HTTP, MSGTYPE_DETAILS, HTTP_RESPONSE_BODY, [TruncateForLog(Answer)]);
		finally
			PostData.free;
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
		ContentLength := HTTP.Request.ContentLength; {Calculate processed data size depending on download or upload}
	if (Pos('chunked', LowerCase(HTTP.Response.TransferEncoding)) = 0) and (ContentLength > 0) then
	begin
		Percent := 100 * AWorkCount div ContentLength;
		if self.Progress.Progress(self.ExternalSourceName, self.ExternalTargetName, Percent) then {When nil is passed, progress keeps previous values}
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
	Logger.Log(LOG_LEVEL_HTTP, MSGTYPE_DETAILS, HTTP_REQUEST_BODY, ['PUT', URL, FileStream.Size]);
	ResultStream := TStringStream.Create;
	try
		Result := self.Put(URL, FileStream, ResultStream);
		Answer := ResultStream.DataString;
		Logger.Log(LOG_LEVEL_HTTP, MSGTYPE_DETAILS, HTTP_RESPONSE, [URL, HTTP.ResponseCode, Length(Answer)]);
		Logger.Log(LOG_LEVEL_HTTP, MSGTYPE_DETAILS, HTTP_RESPONSE_BODY, [TruncateForLog(Answer)]);
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

procedure TCloudMailRuHTTP.SetProgress(Progress: IProgress);
begin
	self.Progress := Progress;
end;

procedure TCloudMailRuHTTP.SetAuthCookie(Value: TIdCookieManager);
begin
	self.HTTP.CookieManager := Value;
end;

function TCloudMailRuHTTP.GetAuthCookie: TIdCookieManager;
begin
	Result := self.HTTP.CookieManager;
end;

function TCloudMailRuHTTP.GetHTTP: TIdHTTP;
begin
	Result := self.HTTP;
end;

procedure TCloudMailRuHTTP.SetCSRFToken(const Token: WideString);
begin
	HTTP.Request.CustomHeaders.Values['X-CSRF-Token'] := Token;
end;

function TCloudMailRuHTTP.ExceptionHandler(E: Exception; URL: WideString; HTTPMethod: Integer; LogErrors: Boolean): Integer;
var
	method_string: WideString; {Message text depends on the original request method}
begin
	Result := FS_FILE_OK; {just to avoid compiler warning}
	case HTTPMethod of
		HTTP_METHOD_GET:
			begin
				method_string := METHOD_STR_RECEIVE;
				Result := FS_FILE_READERROR; {For GetFile/GetForm, error code doesn't matter}
			end;
		HTTP_METHOD_POST, HTTP_METHOD_PUT:
			begin
				method_string := METHOD_STR_POST;
				Result := CLOUD_OPERATION_FAILED; {For all POST requests}
			end;
	end;
	if E is EIdHTTPProtocolException then
	begin
		{HTTP 403 from cloud API always means authentication failure (expired token or invalid session).
			Also check JSON body patterns for non-403 cases that still indicate token issues.}
		if ((E as EIdHTTPProtocolException).ErrorCode = HTTP_ERROR_FORBIDDEN)
			or (NAME_TOKEN = JSONHelper.getBodyError((E as EIdHTTPProtocolException).ErrorMessage))
			or JSONHelper.isNotAuthorizedError((E as EIdHTTPProtocolException).ErrorMessage) then
		begin
			Logger.Log(LOG_LEVEL_DETAIL, MSGTYPE_DETAILS, CSRF_UPDATE_REQUIRED, [method_string, URL]);
			exit(CLOUD_ERROR_TOKEN_OUTDATED);
		end;
	end;

	if E is EAbort then
	begin
		Result := CLOUD_OPERATION_CANCELLED;
	end else if LogErrors then {Further error analysis only makes sense for logging -- return value is already determined}
	begin
		if E is EIdHTTPProtocolException then
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_HTTP_GENERAL, [E.ClassName, E.Message, method_string, URL, (E as EIdHTTPProtocolException).ErrorMessage])
		else if E is EIdSocketerror then
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_SOCKET_GENERAL, [E.ClassName, E.Message, method_string, URL])
		else
			Logger.Log(LOG_LEVEL_ERROR, MSGTYPE_IMPORTANTERROR, ERR_OTHER_GENERAL, [E.ClassName, E.Message, method_string, URL]);
	end;
end;

end.
