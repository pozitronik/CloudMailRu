unit CloudCallbackTypes;

{Shared callback type definitions for Application layer services.
	These types provide access to CloudMailRu state and operations
	without creating direct dependencies on the TCloudMailRu class.}

interface

uses
	System.Classes,
	CMROAuth,
	CMRSpace,
	CloudHTTP,
	TokenRetryHelper;

type
	{Basic getter callbacks - used by multiple services}
	TGetHTTPFunc = reference to function: ICloudHTTP;
	TGetOAuthTokenFunc = reference to function: TCMROAuth;
	TGetBoolFunc = reference to function: Boolean;
	TGetIntFunc = reference to function: Integer;
	TGetInt64Func = reference to function: Int64;
	TGetStringFunc = reference to function: WideString;
	TRefreshTokenFunc = reference to function: Boolean;

	{Specialized callbacks - used by specific services}
	TGetUnitedParamsFunc = reference to function: WideString;
	THashStreamFunc = reference to function(Stream: TStream; Path: WideString): WideString;
	THashFileFunc = reference to function(Path: WideString): WideString;
	TDeleteFileFunc = reference to function(Path: WideString): Boolean;
	TGetRetryOperationFunc = reference to function: TRetryOperation;
	TCloudResultToFsResultFunc = reference to function(JSON: WideString; ErrorPrefix: WideString): Integer;
	TGetUserSpaceFunc = reference to function(var SpaceInfo: TCMRSpace): Boolean;

implementation

end.
