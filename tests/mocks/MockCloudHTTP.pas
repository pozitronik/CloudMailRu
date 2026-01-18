unit MockCloudHTTP;

{Configurable mock HTTP implementation for testing TCloudMailRu.
 Allows setting canned responses for specific URL patterns.}

interface

uses
	System.Classes,
	System.SysUtils,
	System.Generics.Collections,
	ICloudHTTPInterface,
	PLUGIN_TYPES,
	IdCookieManager,
	IdHTTP;

type
	{Response configuration for a single URL pattern}
	TMockResponse = record
		Success: Boolean;
		Answer: WideString;
		ResultCode: Integer;
		{For file operations - data to write to stream}
		FileData: TBytes;
	end;

	{Configurable mock HTTP - allows setting responses per URL pattern}
	TMockCloudHTTP = class(TInterfacedObject, ICloudHTTP)
	private
		FResponses: TDictionary<WideString, TMockResponse>;
		FDefaultResponse: TMockResponse;
		FCalls: TList<WideString>;
		FPostData: TList<WideString>;

		function FindResponse(URL: WideString): TMockResponse;
	public
		constructor Create;
		destructor Destroy; override;

		{Configure responses}
		procedure SetResponse(URLPattern: WideString; Success: Boolean; Answer: WideString;
			ResultCode: Integer = FS_FILE_OK);
		procedure SetFileResponse(URLPattern: WideString; Data: TBytes; ResultCode: Integer = FS_FILE_OK);
		procedure SetDefaultResponse(Success: Boolean; Answer: WideString;
			ResultCode: Integer = FS_FILE_READERROR);
		procedure ClearResponses;

		{Inspection - what was called}
		function GetCallCount: Integer;
		function GetCall(Index: Integer): WideString;
		function GetLastCall: WideString;
		function WasURLCalled(URLPattern: WideString): Boolean;
		function GetPostedData(Index: Integer): WideString;
		function GetLastPostedData: WideString;

		{ICloudHTTP implementation}
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

{TMockCloudHTTP}

constructor TMockCloudHTTP.Create;
begin
	inherited Create;
	FResponses := TDictionary<WideString, TMockResponse>.Create;
	FCalls := TList<WideString>.Create;
	FPostData := TList<WideString>.Create;
	{Default: fail with empty response}
	FDefaultResponse.Success := False;
	FDefaultResponse.Answer := '';
	FDefaultResponse.ResultCode := FS_FILE_READERROR;
end;

destructor TMockCloudHTTP.Destroy;
begin
	FreeAndNil(FResponses);
	FreeAndNil(FCalls);
	FreeAndNil(FPostData);
	inherited;
end;

function TMockCloudHTTP.FindResponse(URL: WideString): TMockResponse;
var
	Pattern: WideString;
begin
	{Exact match first}
	if FResponses.TryGetValue(URL, Result) then
		Exit;

	{Partial match - check if URL contains any registered pattern}
	for Pattern in FResponses.Keys do
	begin
		if Pos(Pattern, URL) > 0 then
		begin
			Result := FResponses[Pattern];
			Exit;
		end;
	end;

	Result := FDefaultResponse;
end;

procedure TMockCloudHTTP.SetResponse(URLPattern: WideString; Success: Boolean;
	Answer: WideString; ResultCode: Integer);
var
	Response: TMockResponse;
begin
	Response.Success := Success;
	Response.Answer := Answer;
	Response.ResultCode := ResultCode;
	SetLength(Response.FileData, 0);
	FResponses.AddOrSetValue(URLPattern, Response);
end;

procedure TMockCloudHTTP.SetFileResponse(URLPattern: WideString; Data: TBytes;
	ResultCode: Integer);
var
	Response: TMockResponse;
begin
	Response.Success := ResultCode = FS_FILE_OK;
	Response.Answer := '';
	Response.ResultCode := ResultCode;
	Response.FileData := Data;
	FResponses.AddOrSetValue(URLPattern, Response);
end;

procedure TMockCloudHTTP.SetDefaultResponse(Success: Boolean; Answer: WideString;
	ResultCode: Integer);
begin
	FDefaultResponse.Success := Success;
	FDefaultResponse.Answer := Answer;
	FDefaultResponse.ResultCode := ResultCode;
end;

procedure TMockCloudHTTP.ClearResponses;
begin
	FResponses.Clear;
	FCalls.Clear;
	FPostData.Clear;
end;

function TMockCloudHTTP.GetCallCount: Integer;
begin
	Result := FCalls.Count;
end;

function TMockCloudHTTP.GetCall(Index: Integer): WideString;
begin
	if (Index >= 0) and (Index < FCalls.Count) then
		Result := FCalls[Index]
	else
		Result := '';
end;

function TMockCloudHTTP.GetLastCall: WideString;
begin
	if FCalls.Count > 0 then
		Result := FCalls[FCalls.Count - 1]
	else
		Result := '';
end;

function TMockCloudHTTP.WasURLCalled(URLPattern: WideString): Boolean;
var
	Call: WideString;
begin
	for Call in FCalls do
		if Pos(URLPattern, Call) > 0 then
			Exit(True);
	Result := False;
end;

function TMockCloudHTTP.GetPostedData(Index: Integer): WideString;
begin
	if (Index >= 0) and (Index < FPostData.Count) then
		Result := FPostData[Index]
	else
		Result := '';
end;

function TMockCloudHTTP.GetLastPostedData: WideString;
begin
	if FPostData.Count > 0 then
		Result := FPostData[FPostData.Count - 1]
	else
		Result := '';
end;

{ICloudHTTP implementation}

function TMockCloudHTTP.GetPage(URL: WideString; var Answer: WideString;
	var ProgressEnabled: Boolean): Boolean;
var
	Response: TMockResponse;
begin
	FCalls.Add('GET:' + URL);
	Response := FindResponse(URL);
	Answer := Response.Answer;
	Result := Response.Success;
end;

function TMockCloudHTTP.GetFile(URL: WideString; FileStream: TStream;
	LogErrors: Boolean): Integer;
var
	Response: TMockResponse;
begin
	FCalls.Add('GETFILE:' + URL);
	Response := FindResponse(URL);
	if Length(Response.FileData) > 0 then
		FileStream.Write(Response.FileData[0], Length(Response.FileData));
	Result := Response.ResultCode;
end;

function TMockCloudHTTP.GetRedirection(URL: WideString; var RedirectionURL: WideString;
	var ProgressEnabled: Boolean): Boolean;
var
	Response: TMockResponse;
begin
	FCalls.Add('REDIRECT:' + URL);
	Response := FindResponse(URL);
	RedirectionURL := Response.Answer;
	Result := Response.Success;
end;

function TMockCloudHTTP.PostForm(URL: WideString; PostDataString: WideString;
	var Answer: WideString; ContentType: WideString; LogErrors: Boolean;
	ProgressEnabled: Boolean): Boolean;
var
	Response: TMockResponse;
begin
	FCalls.Add('POST:' + URL);
	FPostData.Add(PostDataString);
	Response := FindResponse(URL);
	Answer := Response.Answer;
	Result := Response.Success;
end;

function TMockCloudHTTP.PostMultipart(URL: WideString;
	Params: TDictionary<WideString, WideString>; var Answer: WideString): Boolean;
var
	Response: TMockResponse;
	ParamStr: WideString;
	Key: WideString;
begin
	FCalls.Add('POSTMULTI:' + URL);
	{Record params as string for inspection}
	ParamStr := '';
	for Key in Params.Keys do
		ParamStr := ParamStr + Key + '=' + Params[Key] + '&';
	FPostData.Add(ParamStr);
	Response := FindResponse(URL);
	Answer := Response.Answer;
	Result := Response.Success;
end;

function TMockCloudHTTP.PostFile(URL: WideString; FileName: WideString;
	FileStream: TStream; var Answer: WideString): Integer;
var
	Response: TMockResponse;
begin
	FCalls.Add('POSTFILE:' + URL + ':' + FileName);
	Response := FindResponse(URL);
	Answer := Response.Answer;
	Result := Response.ResultCode;
end;

function TMockCloudHTTP.PutFile(URL: WideString; FileName: WideString;
	FileStream: TStream; var Answer: WideString): Integer;
var
	Response: TMockResponse;
begin
	FCalls.Add('PUT:' + URL + ':' + FileName);
	Response := FindResponse(URL);
	Answer := Response.Answer;
	Result := Response.ResultCode;
end;

procedure TMockCloudHTTP.Head(URL: WideString);
begin
	FCalls.Add('HEAD:' + URL);
end;

procedure TMockCloudHTTP.SetProgressNames(SourceName, TargetName: WideString);
begin
	{No-op for mock}
end;

procedure TMockCloudHTTP.SetAuthCookie(Value: TIdCookieManager);
begin
	{No-op for mock}
end;

function TMockCloudHTTP.GetHTTP: TIdHTTP;
begin
	{Mock doesn't have real HTTP - return nil}
	Result := nil;
end;

end.
