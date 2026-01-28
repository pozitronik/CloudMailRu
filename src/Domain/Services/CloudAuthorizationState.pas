unit CloudAuthorizationState;

{Authorization state types for cloud connections. Enables state machine pattern for separating construction from authorization.}

interface

type
	{Authorization state machine states.
		asPending - Initial state, authorization not attempted
		asAuthorizing - Authorization in progress
		asAuthorized - Successfully authenticated
		asFailed - Authorization failed (wrong password, network error, etc.)}
	TAuthorizationState = (asPending, asAuthorizing, asAuthorized, asFailed);

	{Authorization error codes for categorizing failure reasons}
	TAuthorizationErrorCode = (
		aecNone,        // No error
		aecUnknown,     // Generic/unknown error
		aecInitFailed,  // Cloud initialization failed
		aecAuthFailed   // Authentication failed
	);

	{Authorization error details.
		Provides context when authorization fails.}
	TAuthorizationError = record
		ErrorCode: TAuthorizationErrorCode;
		ErrorMessage: WideString;
		class function Empty: TAuthorizationError; static;
		class function Create(Code: TAuthorizationErrorCode; const Message: WideString): TAuthorizationError; overload; static;
		class function Create(const Message: WideString): TAuthorizationError; overload; static;
	end;

implementation

{TAuthorizationError}

class function TAuthorizationError.Empty: TAuthorizationError;
begin
	Result.ErrorCode := aecNone;
	Result.ErrorMessage := '';
end;

class function TAuthorizationError.Create(Code: TAuthorizationErrorCode; const Message: WideString): TAuthorizationError;
begin
	Result.ErrorCode := Code;
	Result.ErrorMessage := Message;
end;

class function TAuthorizationError.Create(const Message: WideString): TAuthorizationError;
begin
	Result.ErrorCode := aecUnknown;
	Result.ErrorMessage := Message;
end;

end.
