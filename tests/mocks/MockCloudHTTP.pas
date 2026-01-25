unit MockCloudHTTP;

{Configurable mock HTTP implementation for testing TCloudMailRu.
 Allows setting canned responses for specific URL patterns.
 Supports sequential responses via response queue for multi-step operation testing.}

interface

uses
	System.Classes,
	System.SysUtils,
	System.Generics.Collections,
	CloudHTTP,
	TCProgress,
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

	{Stream response configuration for GetFile/PutFile testing}
	TMockStreamResponse = record
		Content: TBytes;           {Binary content for GetFile response}
		ExpectedHash: WideString;  {Hash to return for PutFile (40-char SHA1 hex)}
		ResultCode: Integer;       {HTTP result code}
		SimulateProgress: Boolean; {Whether to invoke progress callback}
	end;

	{Captured upload data for verification}
	TMockUploadCapture = record
		FileName: WideString;
		Content: TBytes;
		URL: WideString;
	end;

	{Configurable mock HTTP - allows setting responses per URL pattern}
	TMockCloudHTTP = class(TInterfacedObject, ICloudHTTP)
	private
		FResponses: TDictionary<WideString, TMockResponse>;
		FDefaultResponse: TMockResponse;
		FCalls: TList<WideString>;
		FPostData: TList<WideString>;

		{Response queue for sequential responses (multi-step flows)}
		FResponseQueue: TDictionary<WideString, TList<TMockResponse>>;

		{Stream response configuration}
		FStreamResponses: TDictionary<WideString, TMockStreamResponse>;
		FStreamResponseQueue: TDictionary<WideString, TList<TMockStreamResponse>>;

		{Upload captures for verification}
		FUploadCaptures: TList<TMockUploadCapture>;

		{TIdHTTP instance for User-Agent manipulation}
		FHTTP: TIdHTTP;

		function FindResponse(URL: WideString): TMockResponse;
		function FindStreamResponse(URL: WideString; var Response: TMockStreamResponse): Boolean;
		function DequeueResponse(URL: WideString; var Response: TMockResponse): Boolean;
		function DequeueStreamResponse(URL: WideString; var Response: TMockStreamResponse): Boolean;
		function MatchURLPattern(const URL, Pattern: WideString): Boolean;
	public
		constructor Create;
		destructor Destroy; override;

		{Configure single responses}
		procedure SetResponse(URLPattern: WideString; Success: Boolean; Answer: WideString;
			ResultCode: Integer = FS_FILE_OK);
		procedure SetFileResponse(URLPattern: WideString; Data: TBytes; ResultCode: Integer = FS_FILE_OK);
		procedure SetDefaultResponse(Success: Boolean; Answer: WideString;
			ResultCode: Integer = FS_FILE_READERROR);
		procedure ClearResponses;

		{Configure stream responses for GetFile/PutFile}
		procedure SetStreamResponse(URLPattern: WideString; Content: TBytes;
			ResultCode: Integer = FS_FILE_OK);
		procedure SetPutFileResponse(URLPattern: WideString; ExpectedHash: WideString;
			ResultCode: Integer = FS_FILE_OK);

		{Queue responses for sequential access (multi-step operations)}
		procedure QueueResponse(URLPattern: WideString; Success: Boolean; Answer: WideString;
			ResultCode: Integer = FS_FILE_OK);
		procedure QueueStreamResponse(URLPattern: WideString; Content: TBytes;
			ResultCode: Integer = FS_FILE_OK);
		procedure QueuePutFileResponse(URLPattern: WideString; ExpectedHash: WideString;
			ResultCode: Integer = FS_FILE_OK);
		function HasPendingResponses(URLPattern: WideString): Boolean;
		procedure ClearQueues;

		{Shard/dispatcher response helpers}
		procedure SetShardResponse(ShardType: WideString; ShardURL: WideString);
		procedure SetFullDispatcherResponse(DownloadURL, UploadURL: WideString);
		procedure SetOAuthDispatcherResponse(DispatcherType: WideString; URL: WideString);

		{Inspection - what was called}
		function GetCallCount: Integer;
		function GetCall(Index: Integer): WideString;
		function GetLastCall: WideString;
		function WasURLCalled(URLPattern: WideString): Boolean;
		function GetPostedData(Index: Integer): WideString;
		function GetLastPostedData: WideString;

		{Upload capture inspection}
		function GetUploadCount: Integer;
		function GetUploadCapture(Index: Integer): TMockUploadCapture;
		function GetLastUploadCapture: TMockUploadCapture;
		function GetCapturedUploadContent(URLPattern: WideString): TBytes;

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
		procedure SetProgress(Progress: IProgress);
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

	{Initialize queue structures}
	FResponseQueue := TDictionary<WideString, TList<TMockResponse>>.Create;
	FStreamResponses := TDictionary<WideString, TMockStreamResponse>.Create;
	FStreamResponseQueue := TDictionary<WideString, TList<TMockStreamResponse>>.Create;
	FUploadCaptures := TList<TMockUploadCapture>.Create;

	{Create TIdHTTP for User-Agent manipulation}
	FHTTP := TIdHTTP.Create(nil);

	{Default: fail with empty response}
	FDefaultResponse.Success := False;
	FDefaultResponse.Answer := '';
	FDefaultResponse.ResultCode := FS_FILE_READERROR;
end;

destructor TMockCloudHTTP.Destroy;
var
	Queue: TList<TMockResponse>;
	StreamQueue: TList<TMockStreamResponse>;
begin
	{Free queue lists}
	if Assigned(FResponseQueue) then
	begin
		for Queue in FResponseQueue.Values do
			Queue.Free;
		FreeAndNil(FResponseQueue);
	end;

	if Assigned(FStreamResponseQueue) then
	begin
		for StreamQueue in FStreamResponseQueue.Values do
			StreamQueue.Free;
		FreeAndNil(FStreamResponseQueue);
	end;

	FreeAndNil(FStreamResponses);
	FreeAndNil(FUploadCaptures);
	FreeAndNil(FResponses);
	FreeAndNil(FCalls);
	FreeAndNil(FPostData);
	FreeAndNil(FHTTP);
	inherited;
end;

function TMockCloudHTTP.MatchURLPattern(const URL, Pattern: WideString): Boolean;
begin
	{Empty pattern matches everything - useful for catch-all responses}
	if Pattern = '' then
		Exit(True);
	{Exact match or pattern contained in URL}
	Result := (URL = Pattern) or (Pos(Pattern, URL) > 0);
end;

function TMockCloudHTTP.DequeueResponse(URL: WideString; var Response: TMockResponse): Boolean;
var
	Pattern: WideString;
	Queue: TList<TMockResponse>;
begin
	Result := False;
	for Pattern in FResponseQueue.Keys do
	begin
		if MatchURLPattern(URL, Pattern) then
		begin
			Queue := FResponseQueue[Pattern];
			if Queue.Count > 0 then
			begin
				Response := Queue[0];
				Queue.Delete(0);
				Result := True;
				Exit;
			end;
		end;
	end;
end;

function TMockCloudHTTP.DequeueStreamResponse(URL: WideString; var Response: TMockStreamResponse): Boolean;
var
	Pattern: WideString;
	Queue: TList<TMockStreamResponse>;
begin
	Result := False;
	for Pattern in FStreamResponseQueue.Keys do
	begin
		if MatchURLPattern(URL, Pattern) then
		begin
			Queue := FStreamResponseQueue[Pattern];
			if Queue.Count > 0 then
			begin
				Response := Queue[0];
				Queue.Delete(0);
				Result := True;
				Exit;
			end;
		end;
	end;
end;

function TMockCloudHTTP.FindResponse(URL: WideString): TMockResponse;
var
	Pattern: WideString;
begin
	{Check queue first for sequential response support}
	if DequeueResponse(URL, Result) then
		Exit;

	{Exact match}
	if FResponses.TryGetValue(URL, Result) then
		Exit;

	{Partial match - check if URL contains any registered pattern}
	for Pattern in FResponses.Keys do
	begin
		if MatchURLPattern(URL, Pattern) then
		begin
			Result := FResponses[Pattern];
			Exit;
		end;
	end;

	Result := FDefaultResponse;
end;

function TMockCloudHTTP.FindStreamResponse(URL: WideString; var Response: TMockStreamResponse): Boolean;
var
	Pattern: WideString;
begin
	{Check queue first for sequential response support}
	if DequeueStreamResponse(URL, Response) then
		Exit(True);

	{Exact match}
	if FStreamResponses.TryGetValue(URL, Response) then
		Exit(True);

	{Partial match}
	for Pattern in FStreamResponses.Keys do
	begin
		if MatchURLPattern(URL, Pattern) then
		begin
			Response := FStreamResponses[Pattern];
			Exit(True);
		end;
	end;

	Result := False;
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
	FStreamResponses.Clear;
	FUploadCaptures.Clear;
	ClearQueues;
end;

{Stream response configuration}

procedure TMockCloudHTTP.SetStreamResponse(URLPattern: WideString; Content: TBytes;
	ResultCode: Integer);
var
	Response: TMockStreamResponse;
begin
	Response.Content := Content;
	Response.ExpectedHash := '';
	Response.ResultCode := ResultCode;
	Response.SimulateProgress := False;
	FStreamResponses.AddOrSetValue(URLPattern, Response);
end;

procedure TMockCloudHTTP.SetPutFileResponse(URLPattern: WideString; ExpectedHash: WideString;
	ResultCode: Integer);
var
	Response: TMockStreamResponse;
begin
	Response.Content := nil;
	Response.ExpectedHash := ExpectedHash;
	Response.ResultCode := ResultCode;
	Response.SimulateProgress := False;
	FStreamResponses.AddOrSetValue(URLPattern, Response);
end;

{Queue methods for sequential responses}

procedure TMockCloudHTTP.QueueResponse(URLPattern: WideString; Success: Boolean;
	Answer: WideString; ResultCode: Integer);
var
	Queue: TList<TMockResponse>;
	Response: TMockResponse;
begin
	if not FResponseQueue.TryGetValue(URLPattern, Queue) then
	begin
		Queue := TList<TMockResponse>.Create;
		FResponseQueue.Add(URLPattern, Queue);
	end;

	Response.Success := Success;
	Response.Answer := Answer;
	Response.ResultCode := ResultCode;
	SetLength(Response.FileData, 0);
	Queue.Add(Response);
end;

procedure TMockCloudHTTP.QueueStreamResponse(URLPattern: WideString; Content: TBytes;
	ResultCode: Integer);
var
	Queue: TList<TMockStreamResponse>;
	Response: TMockStreamResponse;
begin
	if not FStreamResponseQueue.TryGetValue(URLPattern, Queue) then
	begin
		Queue := TList<TMockStreamResponse>.Create;
		FStreamResponseQueue.Add(URLPattern, Queue);
	end;

	Response.Content := Content;
	Response.ExpectedHash := '';
	Response.ResultCode := ResultCode;
	Response.SimulateProgress := False;
	Queue.Add(Response);
end;

procedure TMockCloudHTTP.QueuePutFileResponse(URLPattern: WideString; ExpectedHash: WideString;
	ResultCode: Integer);
var
	Queue: TList<TMockStreamResponse>;
	Response: TMockStreamResponse;
begin
	if not FStreamResponseQueue.TryGetValue(URLPattern, Queue) then
	begin
		Queue := TList<TMockStreamResponse>.Create;
		FStreamResponseQueue.Add(URLPattern, Queue);
	end;

	Response.Content := nil;
	Response.ExpectedHash := ExpectedHash;
	Response.ResultCode := ResultCode;
	Response.SimulateProgress := False;
	Queue.Add(Response);
end;

function TMockCloudHTTP.HasPendingResponses(URLPattern: WideString): Boolean;
var
	Queue: TList<TMockResponse>;
	StreamQueue: TList<TMockStreamResponse>;
begin
	Result := False;
	if FResponseQueue.TryGetValue(URLPattern, Queue) then
		if Queue.Count > 0 then
			Exit(True);
	if FStreamResponseQueue.TryGetValue(URLPattern, StreamQueue) then
		if StreamQueue.Count > 0 then
			Exit(True);
end;

procedure TMockCloudHTTP.ClearQueues;
var
	Queue: TList<TMockResponse>;
	StreamQueue: TList<TMockStreamResponse>;
begin
	for Queue in FResponseQueue.Values do
		Queue.Clear;
	for StreamQueue in FStreamResponseQueue.Values do
		StreamQueue.Clear;
end;

{Shard/dispatcher response helpers}

procedure TMockCloudHTTP.SetShardResponse(ShardType: WideString; ShardURL: WideString);
var
	JSON: WideString;
begin
	{Create dispatcher-style response with single shard}
	JSON := Format('{"email":"test@mail.ru","body":{"%s":[{"url":"%s"}]},"status":200}',
		[ShardType, ShardURL]);
	SetResponse('/dispatcher/', True, JSON);
end;

procedure TMockCloudHTTP.SetFullDispatcherResponse(DownloadURL, UploadURL: WideString);
var
	JSON: WideString;
begin
	{Create dispatcher response with download and upload shards}
	JSON := Format(
		'{"email":"test@mail.ru","body":{"get":[{"url":"%s"}],"upload":[{"url":"%s"}]},"status":200}',
		[DownloadURL, UploadURL]);
	SetResponse('/dispatcher/', True, JSON);
end;

procedure TMockCloudHTTP.SetOAuthDispatcherResponse(DispatcherType: WideString; URL: WideString);
begin
	{OAuth dispatcher returns plain text: "URL IP COUNT"}
	SetResponse('/' + DispatcherType, True, Format('%s 127.0.0.1 1', [URL]));
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

{Upload capture inspection}

function TMockCloudHTTP.GetUploadCount: Integer;
begin
	Result := FUploadCaptures.Count;
end;

function TMockCloudHTTP.GetUploadCapture(Index: Integer): TMockUploadCapture;
begin
	if (Index >= 0) and (Index < FUploadCaptures.Count) then
		Result := FUploadCaptures[Index]
	else
		Result := Default(TMockUploadCapture);
end;

function TMockCloudHTTP.GetLastUploadCapture: TMockUploadCapture;
begin
	if FUploadCaptures.Count > 0 then
		Result := FUploadCaptures[FUploadCaptures.Count - 1]
	else
		Result := Default(TMockUploadCapture);
end;

function TMockCloudHTTP.GetCapturedUploadContent(URLPattern: WideString): TBytes;
var
	i: Integer;
begin
	SetLength(Result, 0);
	for i := FUploadCaptures.Count - 1 downto 0 do
	begin
		if MatchURLPattern(FUploadCaptures[i].URL, URLPattern) then
		begin
			Result := FUploadCaptures[i].Content;
			Exit;
		end;
	end;
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
	StreamResponse: TMockStreamResponse;
begin
	FCalls.Add('GETFILE:' + URL);

	{Check for stream response first (preferred for GetFile)}
	if FindStreamResponse(URL, StreamResponse) then
	begin
		if Length(StreamResponse.Content) > 0 then
			FileStream.Write(StreamResponse.Content[0], Length(StreamResponse.Content));
		Result := StreamResponse.ResultCode;
		Exit;
	end;

	{Fall back to regular response (backwards compatibility)}
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
	StreamResponse: TMockStreamResponse;
	Capture: TMockUploadCapture;
begin
	FCalls.Add('POSTFILE:' + URL + ':' + FileName);

	{Capture the uploaded content for verification}
	Capture.URL := URL;
	Capture.FileName := FileName;
	if Assigned(FileStream) then
	begin
		SetLength(Capture.Content, FileStream.Size);
		if FileStream.Size > 0 then
		begin
			FileStream.Position := 0;
			FileStream.Read(Capture.Content[0], FileStream.Size);
			FileStream.Position := 0;
		end;
	end
	else
		SetLength(Capture.Content, 0);
	FUploadCaptures.Add(Capture);

	{Check for stream response first}
	if FindStreamResponse(URL, StreamResponse) then
	begin
		Answer := StreamResponse.ExpectedHash;
		Result := StreamResponse.ResultCode;
		Exit;
	end;

	{Fall back to regular response}
	Response := FindResponse(URL);
	Answer := Response.Answer;
	Result := Response.ResultCode;
end;

function TMockCloudHTTP.PutFile(URL: WideString; FileName: WideString;
	FileStream: TStream; var Answer: WideString): Integer;
var
	Response: TMockResponse;
	StreamResponse: TMockStreamResponse;
	Capture: TMockUploadCapture;
begin
	FCalls.Add('PUT:' + URL + ':' + FileName);

	{Capture the uploaded content for verification}
	Capture.URL := URL;
	Capture.FileName := FileName;
	if Assigned(FileStream) then
	begin
		SetLength(Capture.Content, FileStream.Size);
		if FileStream.Size > 0 then
		begin
			FileStream.Position := 0;
			FileStream.Read(Capture.Content[0], FileStream.Size);
			FileStream.Position := 0; {Reset position for potential re-use}
		end;
	end
	else
		SetLength(Capture.Content, 0);
	FUploadCaptures.Add(Capture);

	{Check for stream response first (returns hash for successful uploads)}
	if FindStreamResponse(URL, StreamResponse) then
	begin
		Answer := StreamResponse.ExpectedHash;
		Result := StreamResponse.ResultCode;
		Exit;
	end;

	{Fall back to regular response}
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

procedure TMockCloudHTTP.SetProgress(Progress: IProgress);
begin
	{No-op for mock}
end;

procedure TMockCloudHTTP.SetAuthCookie(Value: TIdCookieManager);
begin
	{No-op for mock}
end;

function TMockCloudHTTP.GetHTTP: TIdHTTP;
begin
	Result := FHTTP;
end;

end.
