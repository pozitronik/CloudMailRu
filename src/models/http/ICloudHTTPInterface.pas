unit ICloudHTTPInterface;

{Interface for HTTP operations, enabling testability without real HTTP connections.
 TCloudMailRuHTTP implements this interface for production use.
 Mock implementations can be used for testing.}

interface

uses
	System.Classes,
	System.Generics.Collections,
	IdCookieManager,
	IdHTTP;

type
	{Interface for cloud HTTP operations}
	ICloudHTTP = interface
		['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']

		{GET request returning page content}
		function GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;

		{GET request to download file to stream}
		function GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean = True): Integer;

		{GET request following redirects}
		function GetRedirection(URL: WideString; var RedirectionURL: WideString; var ProgressEnabled: Boolean): Boolean;

		{POST form data}
		function PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString;
			ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = True;
			ProgressEnabled: Boolean = True): Boolean;

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
		function PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString;
			ContentType: WideString = 'application/x-www-form-urlencoded'; LogErrors: Boolean = True;
			ProgressEnabled: Boolean = True): Boolean;
		function PostMultipart(URL: WideString; Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean;
		function PostFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
		function PutFile(URL: WideString; FileName: WideString; FileStream: TStream; var Answer: WideString): Integer;
		procedure Head(URL: WideString);
		procedure SetProgressNames(SourceName, TargetName: WideString);
		procedure SetAuthCookie(Value: TIdCookieManager);
		function GetHTTP: TIdHTTP;
	end;

implementation

uses
	PLUGIN_TYPES;

{TNullCloudHTTP}

function TNullCloudHTTP.GetPage(URL: WideString; var Answer: WideString; var ProgressEnabled: Boolean): Boolean;
begin
	Answer := '';
	Result := False;
end;

function TNullCloudHTTP.GetFile(URL: WideString; FileStream: TStream; LogErrors: Boolean): Integer;
begin
	Result := FS_FILE_READERROR;
end;

function TNullCloudHTTP.GetRedirection(URL: WideString; var RedirectionURL: WideString; var ProgressEnabled: Boolean): Boolean;
begin
	RedirectionURL := '';
	Result := False;
end;

function TNullCloudHTTP.PostForm(URL: WideString; PostDataString: WideString; var Answer: WideString;
	ContentType: WideString; LogErrors: Boolean; ProgressEnabled: Boolean): Boolean;
begin
	Answer := '';
	Result := False;
end;

function TNullCloudHTTP.PostMultipart(URL: WideString; Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean;
begin
	Answer := '';
	Result := False;
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

end.
