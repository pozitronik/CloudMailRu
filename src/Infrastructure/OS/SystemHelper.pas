unit SystemHelper;

{System utilities helper methods}
interface

uses
	SysUtils,
	Windows;

procedure ProcessMessages;
function CheckFlag(Check: byte; Flags: LongInt): boolean; //Определяет, установлен ли указанный бит

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

function CheckFlag(Check: byte; Flags: LongInt): boolean; //Определяет, установлен ли указанный бит
begin
	Result := (Flags and Check) <> 0;
end;

end.
