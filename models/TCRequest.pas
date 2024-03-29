﻿unit TCRequest;

interface

uses
	SysUtils,
	PLUGIN_TYPES;

type
	TTCRequest = class
	private
		PluginNum: Integer;
		RequestProc: TRequestProcW;
	public
		constructor Create(); overload; //creates a dummy progress
		constructor Create(RequestProc: TRequestProcW; PluginNum: Integer); overload;
		function Request(RequestType: Integer; CustomTitle, CustomText: WideString; var ReturnedText: WideString; maxlen: Integer): Boolean;
	end;

implementation

{TTCRequest}

constructor TTCRequest.Create;
begin
	self.RequestProc := nil;
	self.PluginNum := -1;
end;

constructor TTCRequest.Create(RequestProc: TRequestProcW; PluginNum: Integer);
begin
	self.RequestProc := RequestProc;
	self.PluginNum := PluginNum;
end;

function TTCRequest.Request(RequestType: Integer; CustomTitle, CustomText: WideString; var ReturnedText: WideString; maxlen: Integer): Boolean;
var
	pReturnedText: PWideChar;
begin
	Result := false;
	if Assigned(RequestProc) then
	begin
		SetLength(ReturnedText, maxlen);
		pReturnedText := PWideChar(ReturnedText);
		Result := RequestProc(PluginNum, RequestType, PWideChar(CustomTitle), PWideChar(CustomText), pReturnedText, maxlen);
		ReturnedText := pReturnedText; // not tested!
	end;
end;

end.
