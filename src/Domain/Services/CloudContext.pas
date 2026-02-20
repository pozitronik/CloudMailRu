unit CloudContext;

{Defines context interfaces for cloud services.
	These interfaces provide access to TCloudMailRu's dynamic state and operations
	without creating direct dependencies on the TCloudMailRu class itself.
	Services receive context interfaces instead of multiple individual callbacks.}

interface

uses
	CloudHTTP,
	CloudOAuth,
	CloudOperationResult,
	CloudSpace,
	CloudEndpoints;

type
	{Context for shard resolution operations.
		Used by TCloudShardManager to resolve and cache shard URLs.}
	IShardContext = interface
		['{12D84BC0-5D2F-467C-93B7-A81DBBF5256C}']
		{POST form data to URL and return response.
			Authentication parameters are appended automatically by the implementation.}
		function PostForm(const URL, Data: WideString; var Answer: WideString): Boolean;
		{Convert API JSON response to boolean success}
		function CloudResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean;
		{Fetch a page by URL, used for OAuth dispatcher resolution}
		function GetPage(const URL: WideString; var Response: WideString; var ShowProgress: Boolean): Boolean;
		{Get the current OAuth access token string for dispatcher authentication}
		function GetOAuthAccessToken: WideString;
		{True when using VK ID cookie-based auth (affects shard resolution strategy)}
		function IsCookieBasedAuth: Boolean;
	end;

	{Context for retry operations with token refresh.
		Used by TRetryOperation to execute API calls with automatic retry on token expiration.}
	IRetryContext = interface
		['{CE9C36B4-DB4D-4882-AFB1-F0A3D62D11FF}']
		{Refresh authentication token}
		function RefreshToken: Boolean;
		{POST form data to URL and return JSON response.
			Authentication parameters are appended automatically by the implementation.}
		function PostForm(const URL, Params: WideString; var JSON: WideString): Boolean;
		{GET page content and return JSON response}
		function GetPage(const URL: WideString; var JSON: WideString; var ShowProgress: Boolean): Boolean;
		{Convert API JSON response to boolean success}
		function ResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean;
		{Convert API JSON response to file system result code}
		function ResultToInteger(const JSON, ErrorPrefix: WideString): Integer;
	end;

	{Main context for cloud services.
		Provides access to TCloudMailRu's dynamic state and high-level operations.}
	ICloudContext = interface
		['{95965B86-6C27-4458-8176-6E461D39B0B7}']
		{HTTP connection access - returns thread-specific connection with auth state}
		function GetHTTP: ICloudHTTP;

		{Authentication state accessors}
		function GetOAuthToken: TCloudOAuth;
		function GetUnitedParams: WideString;
		function RefreshCSRFToken: Boolean;

		{Account type detection}
		function IsPublicAccount: Boolean;
		function GetPublicLink: WideString;

		{Cloud operation result mapping - converts API responses to standardized results}
		function CloudResultToBoolean(const JSON, ErrorPrefix: WideString): Boolean; overload;
		function CloudResultToBoolean(const OperationResult: TCloudOperationResult; const ErrorPrefix: WideString): Boolean; overload;
		function CloudResultToFsResult(const JSON, ErrorPrefix: WideString): Integer; overload;
		function CloudResultToFsResult(const OperationResult: TCloudOperationResult; const ErrorPrefix: WideString): Integer; overload;

		{Endpoint configuration for this cloud instance}
		function GetEndpoints: TCloudEndpoints;

		{Cloud operations - high-level operations that services may need}
		function GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
		function DeleteFile(const Path: WideString): Boolean;
	end;

implementation

end.
