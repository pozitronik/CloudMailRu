unit CloudCallbackTypes;

{Shared callback type definitions for Application layer services.
	These types provide access to CloudMailRu state and operations
	without creating direct dependencies on the TCloudMailRu class.}

interface

uses
	System.Classes,
	CloudOAuth,
	CloudOperationResult,
	CloudSpace,
	CloudHTTP,
	TokenRetryHelper;

type
	{Basic getter callbacks - used by multiple services}
	TGetHTTPFunc = reference to function: ICloudHTTP;
	TGetOAuthTokenFunc = reference to function: TCloudOAuth;
	TGetBoolFunc = reference to function: Boolean;
	TGetIntFunc = reference to function: Integer;
	TGetInt64Func = reference to function: Int64;
	TGetStringFunc = reference to function: WideString;
	TGetThreadIDFunc = reference to function: TThreadID;

	{Semantic aliases - same signature as TGetBoolFunc but with specific meaning}
	TAbortCheckFunc = TGetBoolFunc; {For checking if operation was aborted/cancelled by user}

	{Specialized callbacks - used by specific services}
	THashStreamFunc = reference to function(Stream: TStream; Path: WideString): WideString;
	THashFileFunc = reference to function(Path: WideString): WideString;
	TDeleteFileFunc = reference to function(Path: WideString): Boolean;
	TGetRetryOperationFunc = reference to function: IRetryOperation;
	TCloudResultToFsResultFunc = reference to function(JSON: WideString; ErrorPrefix: WideString): Integer;
	TCloudResultToBooleanFunc = reference to function(JSON: WideString; ErrorPrefix: WideString): Boolean;
	TCloudResultToBooleanFromResultFunc = reference to function(OperationResult: TCloudOperationResult; ErrorPrefix: WideString): Boolean;
	TGetUserSpaceFunc = reference to function(var SpaceInfo: TCloudSpace): Boolean;

implementation

end.
