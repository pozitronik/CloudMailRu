unit SystemHelper;

{System utilities helper methods}
interface

uses
	SysUtils,
	Windows;

procedure ProcessMessages;
function DateTimeToUnix(ConvDate: TDateTime): integer;
function CheckFlag(Check: byte; Flags: LongInt): boolean; //Определяет, установлен ли указанный бит
function DateTimeToFileTime(FileTime: TDateTime): TFileTime;

implementation

procedure ProcessMessages;
var
	Msg: TMsg;
begin
	while true do
	begin
		if not PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
			Break;
		if Msg.Message <> $0012 then
		begin
			TranslateMessage(Msg);
			DispatchMessage(Msg);
		end;
	end;
end;

function DateTimeToUnix(ConvDate: TDateTime): integer;
const
	UnixStartDate: TDateTime = 25569.0;
begin
	Result := Round((ConvDate - UnixStartDate) * 86400);
end;

function CheckFlag(Check: byte; Flags: LongInt): boolean; //Определяет, установлен ли указанный бит
begin
	Result := (Flags and Check) <> 0;
end;

function DateTimeToFileTime(FileTime: TDateTime): TFileTime;
var
	LocalFileTime, Ft: TFileTime;
	SystemTime: TSystemTime;
begin
	Result.dwLowDateTime := 0;
	Result.dwHighDateTime := 0;
	DateTimeToSystemTime(FileTime, SystemTime);
	SystemTimeToFileTime(SystemTime, LocalFileTime);
	LocalFileTimeToFileTime(LocalFileTime, Ft);
	Result := Ft;
end;

end.
