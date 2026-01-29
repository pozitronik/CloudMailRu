unit CloudContext;

{Defines the context interface for cloud services.
	This interface provides access to TCloudMailRu's dynamic state and operations
	without creating direct dependencies on the TCloudMailRu class itself.
	Services receive ICloudContext instead of multiple individual callbacks.}

interface

uses
	CloudHTTP,
	CloudOAuth,
	CloudOperationResult,
	CloudSpace;

type
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
		function CloudResultToFsResult(const JSON, ErrorPrefix: WideString): Integer;

		{Cloud operations - high-level operations that services may need}
		function GetUserSpace(var SpaceInfo: TCloudSpace): Boolean;
		function DeleteFile(const Path: WideString): Boolean;
	end;

implementation

end.
