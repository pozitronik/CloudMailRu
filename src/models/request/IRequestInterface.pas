unit IRequestInterface;

{Interface for user request dialogs, abstracting the concrete implementation from consumers.
 This enables dependency injection and testability by allowing mock implementations.}

interface

type
	{User request interface for interactive dialogs.
	 Returns True if request was executed (user responded), False otherwise.}
	IRequest = interface
		['{E0604605-0537-4F5F-9296-BD266AEFF24B}']
		function Request(RequestType: Integer; CustomTitle, CustomText: WideString; var ReturnedText: WideString; maxlen: Integer): Boolean;
	end;

	{Null object implementation of IRequest. Request() returns False (not executed).
	 Use when user dialogs are not needed, e.g., in tests or standalone operations.}
	TNullRequest = class(TInterfacedObject, IRequest)
	public
		function Request(RequestType: Integer; CustomTitle, CustomText: WideString; var ReturnedText: WideString; maxlen: Integer): Boolean;
	end;

implementation

{TNullRequest}

function TNullRequest.Request(RequestType: Integer; CustomTitle, CustomText: WideString; var ReturnedText: WideString; maxlen: Integer): Boolean;
begin
	Result := False; {Request not executed}
end;

end.
