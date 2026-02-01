unit MockCloudContext;

{Mock implementation of ICloudContext for testing services without TCloudMailRu dependency.
	Provides configurable responses for all context methods, simplifying test setup.}

interface

uses
	CloudContext,
	CloudOAuth,
	CloudOperationResult,
	CloudSpace,
	CloudHTTP,
	MockCloudHTTP,
	WFXTypes,
	System.Generics.Collections;

type
	{Test mock for ICloudContext, IShardContext, and IRetryContext.
		All values are configurable via setters for easy test setup.}
	TMockCloudContext = class(TInterfacedObject, ICloudContext, IShardContext, IRetryContext)
	private
		FIsPublicAccount: Boolean;
		FUnitedParams: WideString;
		FPublicLink: WideString;
		FHTTP: ICloudHTTP;
		FOAuthToken: TCloudOAuth;
		FRefreshCSRFTokenResult: Boolean;
		FRefreshCSRFTokenCalled: Boolean;
		FCloudResultToBooleanResult: Boolean;
		FCloudResultToFsResultResult: Integer;
		FCloudResultToFsResultQueue: TQueue<Integer>;
		FCloudResultToFsResultCallCount: Integer;
		FGetUserSpaceResult: Boolean;
		FUserSpaceInfo: TCloudSpace;
		FDeleteFileResult: Boolean;
		FDeleteFileCalled: Boolean;
		FDeleteFilePath: WideString;
		FPostFormResult: Boolean;
		FPostFormResponse: WideString;
		FGetPageResult: Boolean;
		FGetPageResponse: WideString;
	public
		constructor Create;
		destructor Destroy; override;

		{Configuration setters for test setup}
		procedure SetIsPublicAccount(Value: Boolean);
		procedure SetUnitedParams(const Value: WideString);
		procedure SetPublicLink(const Value: WideString);
		procedure SetHTTP(Value: ICloudHTTP);
		procedure SetOAuthToken(const Value: TCloudOAuth);
		procedure SetRefreshCSRFTokenResult(Value: Boolean);
		procedure SetCloudResultToBooleanResult(Value: Boolean);
		procedure SetCloudResultToFsResultResult(Value: Integer);
		{Queue a result for CloudResultToFsResult - queue is consumed before default value}
		procedure QueueCloudResultToFsResult(Value: Integer);
		procedure SetGetUserSpaceResult(Value: Boolean; SpaceInfo: TCloudSpace);
		procedure SetDeleteFileResult(Value: Boolean);
		procedure SetPostFormResult(Value: Boolean; const Response: WideString = '');
		procedure SetGetPageResult(Value: Boolean; const Response: WideString = '');

		{Test inspection methods}
		function WasRefreshCSRFTokenCalled: Boolean;
		procedure ResetRefreshCSRFTokenCalled;
		function GetCloudResultToFsResultCallCount: Integer;
		function WasDeleteFileCalled: Boolean;
		function GetDeleteFilePath: WideString;

		{ICloudContext implementation}
		function IsPublicAccount: Boolean;
		function GetUnitedParams: WideString;
		function GetPublicLink: WideString;
		function GetHTTP: ICloudHTTP;
		function GetOAuthToken: TCloudOAuth;
		function RefreshCSRFToken: Boolean;
		function CloudResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean; overload;
		function CloudResultToBoolean(const OperationResult: TCloudOperationResult; const ErrorPrefix: WideString): Boolean; overload;
		function CloudResultToFsResult(const JSON, ErrorPrefix: WideString): Integer; overload;
		function CloudResultToFsResult(const OperationResult: TCloudOperationResult; const ErrorPrefix: WideString): Integer; overload;
		function GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
		function DeleteFile(const Path: WideString): Boolean;

		{IShardContext implementation - reuses ICloudContext methods where applicable}
		function PostForm(const URL, Data: WideString; var Answer: WideString): Boolean;
		function GetOAuthAccessToken: WideString;

		{IRetryContext implementation}
		function RefreshToken: Boolean;
		function GetPage(const URL: WideString; var JSON: WideString; var ShowProgress: Boolean): Boolean;
		function ResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean;
		function ResultToInteger(const JSON, ErrorPrefix: WideString): Integer;
	end;

implementation

{TMockCloudContext}

constructor TMockCloudContext.Create;
begin
	inherited Create;
	FIsPublicAccount := False;
	FUnitedParams := 'token=test&x-email=test@mail.ru';
	FPublicLink := '';
	FHTTP := TMockCloudHTTP.Create;
	FOAuthToken := Default(TCloudOAuth);
	FRefreshCSRFTokenResult := True;
	FRefreshCSRFTokenCalled := False;
	FCloudResultToBooleanResult := True;
	FCloudResultToFsResultResult := FS_FILE_OK;
	FCloudResultToFsResultQueue := TQueue<Integer>.Create;
	FCloudResultToFsResultCallCount := 0;
	FGetUserSpaceResult := True;
	FUserSpaceInfo := Default(TCloudSpace);
	FDeleteFileResult := True;
	FDeleteFileCalled := False;
	FDeleteFilePath := '';
	FPostFormResult := True;
	FPostFormResponse := '';
	FGetPageResult := True;
	FGetPageResponse := '';
end;

destructor TMockCloudContext.Destroy;
begin
	FCloudResultToFsResultQueue.Free;
	inherited;
end;

procedure TMockCloudContext.SetIsPublicAccount(Value: Boolean);
begin
	FIsPublicAccount := Value;
end;

procedure TMockCloudContext.SetUnitedParams(const Value: WideString);
begin
	FUnitedParams := Value;
end;

procedure TMockCloudContext.SetPublicLink(const Value: WideString);
begin
	FPublicLink := Value;
end;

procedure TMockCloudContext.SetHTTP(Value: ICloudHTTP);
begin
	FHTTP := Value;
end;

procedure TMockCloudContext.SetOAuthToken(const Value: TCloudOAuth);
begin
	FOAuthToken := Value;
end;

procedure TMockCloudContext.SetRefreshCSRFTokenResult(Value: Boolean);
begin
	FRefreshCSRFTokenResult := Value;
end;

procedure TMockCloudContext.SetCloudResultToBooleanResult(Value: Boolean);
begin
	FCloudResultToBooleanResult := Value;
end;

procedure TMockCloudContext.SetCloudResultToFsResultResult(Value: Integer);
begin
	FCloudResultToFsResultResult := Value;
end;

procedure TMockCloudContext.QueueCloudResultToFsResult(Value: Integer);
begin
	FCloudResultToFsResultQueue.Enqueue(Value);
end;

function TMockCloudContext.GetCloudResultToFsResultCallCount: Integer;
begin
	Result := FCloudResultToFsResultCallCount;
end;

procedure TMockCloudContext.SetGetUserSpaceResult(Value: Boolean; SpaceInfo: TCloudSpace);
begin
	FGetUserSpaceResult := Value;
	FUserSpaceInfo := SpaceInfo;
end;

procedure TMockCloudContext.SetDeleteFileResult(Value: Boolean);
begin
	FDeleteFileResult := Value;
end;

function TMockCloudContext.IsPublicAccount: Boolean;
begin
	Result := FIsPublicAccount;
end;

function TMockCloudContext.GetUnitedParams: WideString;
begin
	Result := FUnitedParams;
end;

function TMockCloudContext.GetPublicLink: WideString;
begin
	Result := FPublicLink;
end;

function TMockCloudContext.GetHTTP: ICloudHTTP;
begin
	Result := FHTTP;
end;

function TMockCloudContext.GetOAuthToken: TCloudOAuth;
begin
	Result := FOAuthToken;
end;

function TMockCloudContext.RefreshCSRFToken: Boolean;
begin
	FRefreshCSRFTokenCalled := True;
	Result := FRefreshCSRFTokenResult;
end;

function TMockCloudContext.WasRefreshCSRFTokenCalled: Boolean;
begin
	Result := FRefreshCSRFTokenCalled;
end;

procedure TMockCloudContext.ResetRefreshCSRFTokenCalled;
begin
	FRefreshCSRFTokenCalled := False;
end;

function TMockCloudContext.CloudResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean;
begin
	Result := FCloudResultToBooleanResult;
end;

function TMockCloudContext.CloudResultToBoolean(const OperationResult: TCloudOperationResult; const ErrorPrefix: WideString): Boolean;
begin
	Result := FCloudResultToBooleanResult;
end;

function TMockCloudContext.CloudResultToFsResult(const JSON, ErrorPrefix: WideString): Integer;
begin
	Inc(FCloudResultToFsResultCallCount);
	if FCloudResultToFsResultQueue.Count > 0 then
		Result := FCloudResultToFsResultQueue.Dequeue
	else
		Result := FCloudResultToFsResultResult;
end;

function TMockCloudContext.CloudResultToFsResult(const OperationResult: TCloudOperationResult; const ErrorPrefix: WideString): Integer;
begin
	Inc(FCloudResultToFsResultCallCount);
	if FCloudResultToFsResultQueue.Count > 0 then
		Result := FCloudResultToFsResultQueue.Dequeue
	else
		Result := FCloudResultToFsResultResult;
end;

function TMockCloudContext.GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
begin
	SpaceInfo := FUserSpaceInfo;
	Result := FGetUserSpaceResult;
end;

function TMockCloudContext.DeleteFile(const Path: WideString): Boolean;
begin
	FDeleteFileCalled := True;
	FDeleteFilePath := Path;
	Result := FDeleteFileResult;
end;

function TMockCloudContext.WasDeleteFileCalled: Boolean;
begin
	Result := FDeleteFileCalled;
end;

function TMockCloudContext.GetDeleteFilePath: WideString;
begin
	Result := FDeleteFilePath;
end;

procedure TMockCloudContext.SetPostFormResult(Value: Boolean; const Response: WideString);
begin
	FPostFormResult := Value;
	FPostFormResponse := Response;
end;

procedure TMockCloudContext.SetGetPageResult(Value: Boolean; const Response: WideString);
begin
	FGetPageResult := Value;
	FGetPageResponse := Response;
end;

{IShardContext implementation}

function TMockCloudContext.PostForm(const URL, Data: WideString; var Answer: WideString): Boolean;
begin
	{Delegate to HTTP mock if available, otherwise use configured result}
	if (FHTTP <> nil) then
		Result := FHTTP.PostForm(URL, Data, Answer)
	else
	begin
		Answer := FPostFormResponse;
		Result := FPostFormResult;
	end;
end;

function TMockCloudContext.GetOAuthAccessToken: WideString;
begin
	Result := FOAuthToken.access_token;
end;

{IRetryContext implementation}

function TMockCloudContext.RefreshToken: Boolean;
begin
	FRefreshCSRFTokenCalled := True;
	Result := FRefreshCSRFTokenResult;
end;

function TMockCloudContext.GetPage(const URL: WideString; var JSON: WideString; var ShowProgress: Boolean): Boolean;
begin
	{Delegate to HTTP mock if available, otherwise use configured result}
	if (FHTTP <> nil) then
		Result := FHTTP.GetPage(URL, JSON, ShowProgress)
	else
	begin
		JSON := FGetPageResponse;
		Result := FGetPageResult;
	end;
end;

function TMockCloudContext.ResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean;
begin
	Result := FCloudResultToBooleanResult;
end;

function TMockCloudContext.ResultToInteger(const JSON, ErrorPrefix: WideString): Integer;
begin
	Inc(FCloudResultToFsResultCallCount);
	if FCloudResultToFsResultQueue.Count > 0 then
		Result := FCloudResultToFsResultQueue.Dequeue
	else
		Result := FCloudResultToFsResultResult;
end;

end.
