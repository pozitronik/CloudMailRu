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

	{Authorization error details.
		Provides context when authorization fails.}
	TAuthorizationError = record
		ErrorCode: Integer;
		ErrorMessage: WideString;
		class function Empty: TAuthorizationError; static;
		class function Create(Code: Integer; const Message: WideString): TAuthorizationError; static;
	end;

implementation

{TAuthorizationError}

class function TAuthorizationError.Empty: TAuthorizationError;
begin
	Result.ErrorCode := 0;
	Result.ErrorMessage := '';
end;

class function TAuthorizationError.Create(Code: Integer; const Message: WideString): TAuthorizationError;
begin
	Result.ErrorCode := Code;
	Result.ErrorMessage := Message;
end;

end.
