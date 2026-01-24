unit TCRequest;

{Interface for user request dialogs, abstracting the concrete implementation from consumers.
	This enables dependency injection and testability by allowing mock implementations.}

interface

uses
	SysUtils,
	PLUGIN_TYPES;

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

	TTCRequest = class(TInterfacedObject, IRequest)
	private
		PluginNum: Integer;
		RequestProc: TRequestProcW;
	public
		constructor Create(RequestProc: TRequestProcW; PluginNum: Integer);
		function Request(RequestType: Integer; CustomTitle, CustomText: WideString; var ReturnedText: WideString; maxlen: Integer): Boolean;
	end;

implementation

{TNullRequest}

function TNullRequest.Request(RequestType: Integer; CustomTitle, CustomText: WideString; var ReturnedText: WideString; maxlen: Integer): Boolean;
begin
	Result := False; {Request not executed}
end;

{TTCRequest}

constructor TTCRequest.Create(RequestProc: TRequestProcW; PluginNum: Integer);
begin
	self.RequestProc := RequestProc;
	self.PluginNum := PluginNum;
end;

function TTCRequest.Request(RequestType: Integer; CustomTitle, CustomText: WideString; var ReturnedText: WideString; maxlen: Integer): Boolean;
var
	pReturnedText: PWideChar;
begin
	SetLength(ReturnedText, maxlen);
	pReturnedText := PWideChar(ReturnedText);
	Result := RequestProc(PluginNum, RequestType, PWideChar(CustomTitle), PWideChar(CustomText), pReturnedText, maxlen);
	ReturnedText := pReturnedText; // not tested!
end;

end.
